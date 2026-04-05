#include "gc.h"
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <sys/mman.h>
#include <ucontext.h>
#include "lisp.h"
#include "lisp_tracepoint.h"

#ifdef __SANITIZE_ADDRESS__
#include <sanitizer/asan_interface.h>
#else
#define ASAN_POISON_MEMORY_REGION(addr, size) ((void) (addr), (void) (size))
#define ASAN_UNPOISON_MEMORY_REGION(addr, size) ((void) (addr), (void) (size))
#endif

#ifdef __AVX2__
#include <immintrin.h>
#endif

#ifndef GC_HEAP_SIZE
#define GC_HEAP_SIZE 0x800000 ///< GC heap allocation size in bytes.
#endif
#define NUM_BLOCKS (GC_HEAP_SIZE / sizeof(struct GcBlock) - 1)
#define MIN_FREE (NUM_BLOCKS * 3 / 100)
#define OBJECT_MAP_SIZE (sizeof(struct GcHeap) / (GC_ALIGNMENT * CHAR_BIT))
#define NULL_BUMP_PTR(block) (struct BumpPointer) { (block)->data, (block)->data }

struct BumpPointer { char *cursor, *limit; };

[[gnu::alloc_align (2), gnu::alloc_size (3)]]
static void *bump_alloc(struct BumpPointer *ptr, size_t align, size_t size) {
	if (ptr->cursor - size < ptr->limit) return NULL;
	// Bump allocate downward to align with a single AND instruction
	char *p = (char *) (((uintptr_t) ASSUME_ALIGNED(ptr->cursor, GC_ALIGNMENT)
			- size) & ~(align - 1));
	if (p) return ptr->cursor = p; else unreachable();
}

static_assert(GC_LINE_SIZE % alignof(max_align_t) == 0);
/** Locates the next gap of unmarked lines. */
static struct BumpPointer next_gap(struct GcBlock *block, char *top) {
	unsigned count = 0, end = (top - block->data) / GC_LINE_SIZE;
	char *limit = block->data;
	for (unsigned i = end; i--;)
		if (block->line_marks[i]) {
			// If at least 2 preceeding lines were unmarked then
			// consider the previous line as conservatively marked.
			if (count > 1) { limit += GC_LINE_SIZE * (i + 2); break; }
			count = 0;
			end = i;
		} else ++count;
	return count
		? (struct BumpPointer) { block->data + GC_LINE_SIZE * end, limit }
		: (struct BumpPointer) {};
}

struct GcHeap {
	// Store at same offset to use a single pointer for both
	struct LispCtx lisp_ctx;

	struct BumpPointer ptr, overflow_ptr; ///< Bump pointer for medium objects.
	struct GcBlock **free, **recycled;
	size_t free_len, recycled_len;

	bool mark_color, inhibit_gc, is_major_gc, is_defrag;
	unsigned *object_map; ///< Bitset of object start positions.
	struct MarkStack { void **beg, **p, **end; } mark_stack;
	struct ModSet { unsigned len, capacity; struct BumpPointer *xs; } modset;
	struct GcBlock blocks[NUM_BLOCKS];
};

struct GcHeap *gc_new() {
	struct GcHeap *heap;
	size_t alignment = alignof(struct GcHeap);
#if USE_COMPRESSED_PTRS
	alignment = /* 4 GiB */ 1ull << 32;
#endif
	char *p;
	if ((p = mmap(NULL, sizeof *heap + alignment - 1, PROT_READ | PROT_WRITE,
				MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0))
		== MAP_FAILED) return NULL;
	heap = (struct GcHeap *) ALIGN_UP(p, alignment);
	munmap(p, (char *) heap - p);
	munmap(heap + 1, p + sizeof *heap + alignment - 1 - (char *) (heap + 1));

#ifndef __linux__
	heap->mark_color = heap->inhibit_gc = heap->is_major_gc = heap->is_defrag = false;
	heap->mark_stack = (struct MarkStack) {};
	heap->modset = (struct ModSet) {};
	heap->free = NULL;
	heap->recycled_len = 0;
#endif
	if (!((heap->object_map = calloc(1, OBJECT_MAP_SIZE))
			&& (heap->free = malloc(2 * NUM_BLOCKS * sizeof *heap->free)))) {
		gc_free(heap);
		return NULL;
	}
	heap->free_len = NUM_BLOCKS - 1;
	heap->recycled = heap->free + NUM_BLOCKS;

	struct GcBlock *blocks = heap->blocks;
	heap->ptr = heap->overflow_ptr = NULL_BUMP_PTR(blocks);
	for (struct GcBlock *block = blocks; block < blocks + NUM_BLOCKS; ++block) {
		ASAN_POISON_MEMORY_REGION(block->data, sizeof block->data);
#ifndef __linux__
		memset(block->line_marks, 0, sizeof block->line_marks);
		blocks->flag = 0;
#endif
	}
	for (unsigned i = 0; i < NUM_BLOCKS; ++i) heap->free[i] = blocks + i;
	return heap;
}

void gc_free(struct GcHeap *heap) {
	free(heap->modset.xs);
	free(heap->mark_stack.beg);
	free(heap->free);
	free(heap->object_map);
	munmap(heap, sizeof *heap);
}

/** Remembers @a p as a live allocated object location. */
static void object_map_add(struct GcHeap *heap, char *p) {
	size_t i = (p - (char *)heap) / GC_ALIGNMENT, n = CHAR_BIT * sizeof *heap->object_map;
	heap->object_map[i / n] |= 1u << i % n;
}

/** Removes @a x from the object map, returning whether it was present. */
static bool object_map_remove(struct GcHeap *heap, uintptr_t x) {
	if (x % GC_ALIGNMENT || x - (uintptr_t)heap->blocks >= sizeof heap->blocks)
		return false;
	size_t i = (x - (uintptr_t)heap) / GC_ALIGNMENT, n = CHAR_BIT * sizeof *heap->object_map;
	unsigned *v = heap->object_map + i / n, mask = 1u << i % n, ret = *v & mask;
#if __x86_64__ && __GNUC__
	__asm__ ("btr %0, %k2" : "+m" (*heap->object_map), "=@ccc" (ret) : "Ir" (i) : "cc");
#else
	*v &= ~mask;
#endif
	return ret;
}

[[gnu::cold]] static void modset_grow(struct ModSet *set) {
	size_t new_capacity = set->capacity ? 2 * set->capacity : 8;
	struct BumpPointer *xs;
	if (!(xs = realloc(set->xs, new_capacity * sizeof *xs))) die("malloc failed");
	set->xs = xs;
	set->capacity = new_capacity;
}

[[gnu::noinline]]
static void *alloc_slow_path(struct GcHeap *heap, size_t alignment, size_t size) {
	struct BumpPointer *ptr = &heap->ptr;
	if (size <= GC_LINE_SIZE) {
		struct BumpPointer gap = next_gap(GC_BLOCK(ptr->limit), ptr->limit);
		if (gap.cursor) { *ptr = gap; goto out_bump; }
		if (heap->recycled_len) {
			struct GcBlock *block = heap->recycled[--heap->recycled_len];
			// Recycled blocks have gaps of >=1 line; enough for small objects
			*ptr = next_gap(block, (&block->data)[1]);
			goto out_bump;
		}
	} else { // Demand-driven overflow allocation
		char *p;
		if ((p = bump_alloc(ptr = &heap->overflow_ptr, alignment, size))) return p;
	}
	// Acquire a free block
	if (heap->free_len <= MIN_FREE && !heap->inhibit_gc) garbage_collect(heap);
	if (!heap->free_len) return NULL;
	struct GcBlock *block = heap->free[--heap->free_len];
	*ptr = (struct BumpPointer) { (&block->data)[1], block->data };
out_bump:
	ASAN_POISON_MEMORY_REGION(ptr->limit, ptr->cursor - ptr->limit);
	if (heap->modset.len >= heap->modset.capacity) modset_grow(&heap->modset);
	heap->modset.xs[heap->modset.len++] = *ptr;
	return bump_alloc(ptr, alignment, size);
}

void *gc_alloc(struct GcHeap *heap, size_t alignment, size_t size) {
	char *p;
	if (UNLIKELY(size > sizeof (struct GcBlock) {}.data)
		|| !(LIKELY(p = bump_alloc(&heap->ptr, alignment, size))
			|| (p = alloc_slow_path(heap, alignment, size)))) return NULL;
	ASAN_UNPOISON_MEMORY_REGION(p, size);
	*(struct GcObjectHeader *) p = (struct GcObjectHeader) { heap->mark_color };
	object_map_add(heap, p);
	return p;
}

[[gnu::cold]] static void mark_stack_grow(struct GcHeap *heap) {
	struct MarkStack *stack = &heap->mark_stack;
	size_t len = stack->p - stack->beg, new_capacity = len ? 2 * len : 8;
	void **xs;
	if (!(xs = realloc(stack->beg, new_capacity * sizeof *xs)))
		die("malloc failed");
	*stack = (struct MarkStack) { xs, xs + len, xs + new_capacity };
}

static void mark_stack_push(struct GcHeap *heap, void *x) {
	if (heap->mark_stack.p >= heap->mark_stack.end) mark_stack_grow(heap);
	*heap->mark_stack.p++ = x;
}

void gc_log_object(struct GcHeap *heap, struct GcObjectHeader *src) {
	src->flags &= ~GC_UNLOGGED;
	mark_stack_push(heap, src); // Add to remembered set
}

enum { GC_FORWARDED = 4 };

void gc_pin(struct GcHeap *heap, bool mark_color, void *p) {
	struct GcObjectHeader *hdr = p;
	assert(!(hdr->flags & GC_FORWARDED) && "already forwarded");
	hdr->flags = mark_color | GC_UNLOGGED;
	mark_stack_push(heap, p);
}

void *gc_evacuate(struct GcHeap *heap, void *p) {
	struct GcObjectHeader *hdr = p;
	struct GcRef *fwd = (struct GcRef *) ALIGN_UP(hdr + 1, alignof(struct GcRef));
	if ((hdr->flags & GC_MARK) == heap->mark_color) // Already traced
		return hdr->flags & GC_FORWARDED ? (void *) GC_DECOMPRESS(heap, *fwd) : p;
	hdr->flags = heap->mark_color | GC_UNLOGGED;

	size_t alignment, size = gc_object_size(p, &alignment);
	void *q;
	if ((q = gc_alloc(heap, alignment, size))) {
		memcpy(q, p, size);
		*fwd = GC_COMPRESS(p = q); // Leave forwarding pointer
		hdr->flags |= GC_FORWARDED;
	}

	mark_stack_push(heap, p);
	return p;
}

extern void *__libc_stack_end; ///< Highest used stack address.
static void scan_stack(struct GcHeap *heap) {
	void *base = __libc_stack_end, *sp = __builtin_frame_address(0);
	sp = (void *) ((uintptr_t) sp & ~(alignof(struct GcRef) - 1));
	for (struct GcRef *p = sp; (void *) p <= base; ++p) {
		uintptr_t x = GC_DECOMPRESS(heap, *p) & ~1ull;
		// Pin to not "forward" a false positive root
		if (object_map_remove(heap, x)) gc_pin(heap, heap->mark_color, (void *) x);
	}
}

static struct BlockStats {
	unsigned num_marks, num_holes;
} block_stats(struct GcBlock *block) {
	struct BlockStats result = {};
	for (unsigned i = 0, prev_was_marked = false; i < GC_LINE_COUNT;
			prev_was_marked = block->line_marks[i], ++i)
		if (block->line_marks[i]) ++result.num_marks;
		else if (prev_was_marked) ++result.num_holes;
	return result;
}

[[gnu::no_sanitize_address]] static enum BlockStatus {
	FREE, ///< Unallocated.
	RECYCLABLE, ///< Partly used with at least F=1 free lines.
	UNAVAILABLE, ///< No unmarked lines.
} sweep(struct GcBlock *block) {
	block->flag = 0;
	unsigned unavailable_lines = 0;
#ifdef __AVX2__
	static_assert(GC_LINE_COUNT == 127);
	__m256i_u *ys = (__m256i_u *) (block->line_marks - 1);
	__m256i *xs = (__m256i *) block->line_marks, as[] = {
		_mm256_or_si256(_mm256_load_si256(xs),
			_mm256_andnot_si256( // Mask block->line_marks[-1]
				_mm256_set_epi64x(0, 0, 0, 0xff), _mm256_loadu_si256(ys))),
		_mm256_or_si256(_mm256_load_si256(xs + 1), _mm256_loadu_si256(ys + 1)),
		_mm256_or_si256(_mm256_load_si256(xs + 2), _mm256_loadu_si256(ys + 2)),
		_mm256_or_si256(_mm256_load_si256(xs + 3), _mm256_loadu_si256(ys + 3)),
	}, sums = _mm256_sad_epu8(
		_mm256_add_epi8(_mm256_add_epi8(as[0], as[1]), _mm256_add_epi8(as[2], as[3])),
		_mm256_setzero_si256());
	unavailable_lines = _mm256_extract_epi64(sums, 0) + _mm256_extract_epi64(sums, 1)
		+ _mm256_extract_epi64(sums, 2) + _mm256_extract_epi64(sums, 3);
#else
	for (unsigned i = 0, prev_was_marked = false; i < GC_LINE_COUNT;
			prev_was_marked = block->line_marks[i], ++i)
		if (block->line_marks[i] || prev_was_marked) ++unavailable_lines;
#endif
	return !unavailable_lines ? FREE
		: unavailable_lines < GC_LINE_COUNT ? RECYCLABLE
		: UNAVAILABLE;
}

[[gnu::noinline]] static void select_defrag_candidates(struct GcHeap *heap) {
#define MAX_HOLES ((GC_LINE_COUNT + 2) / 3)
	unsigned mark_histogram[MAX_HOLES] = {};
	for (size_t i = 0; i < heap->recycled_len; ++i) {
		struct GcBlock *block = heap->recycled[i];
		struct BlockStats stats = block_stats(block);
		mark_histogram[block->flag = stats.num_holes] += stats.num_marks;
	}

	ssize_t available_space = GC_LINE_SIZE * GC_LINE_COUNT * heap->free_len;
	unsigned bin = MAX_HOLES;
	do available_space -= GC_LINE_SIZE * mark_histogram[--bin];
	while (available_space > 0 && bin);

	for (size_t i = 0; i < heap->recycled_len;) {
		struct GcBlock *block = heap->recycled[i];
		bool is_defrag_candidate = block->flag > bin;
		block->flag = is_defrag_candidate ? 1 : 2;
		if (is_defrag_candidate)
			// Remove from recycled list to not evacuate into itself
			heap->recycled[i] = heap->recycled[--heap->recycled_len];
		else ++i;
	}
	GC_BLOCK(heap->ptr.cursor)->flag = 2;
}

#ifndef __GNUC__
volatile void *gc_nop_sink;
#endif

void garbage_collect(struct GcHeap *heap) {
	heap->inhibit_gc = true;
	lttng_ust_tracepoint(lisp, garbage_collection, heap->is_major_gc + heap->is_defrag);
	// Unlog remembered set
	if (heap->is_major_gc) heap->mark_stack.p = heap->mark_stack.beg; // Ignore it
	for (void **p = heap->mark_stack.beg; p < heap->mark_stack.p; ++p)
		((struct GcObjectHeader *) *p)->flags |= GC_UNLOGGED;
	// Alternate liveness color to skip zeroing object marks
	register bool mark_color = heap->mark_color ^= 1;

	// Push callee-saved register onto the stack
	ucontext_t ctx;
	if (getcontext(&ctx)) __builtin_unwind_init();
	scan_stack(heap); // Collect conservative roots
	// Prevent prematurely popping register contents
#ifdef __GNUC__
	__asm__ volatile ("" : : "X" (&ctx) : "memory");
#else
	gc_nop_sink = &ctx;
#endif

	if (heap->is_major_gc) {
		memset(heap->object_map, 0, OBJECT_MAP_SIZE);
		if (heap->is_defrag) select_defrag_candidates(heap);
		// TODO Cyclical line marks (see MMTk) need not be reset, but
		// would complicate gc_mark().
		for (struct GcBlock *block = heap->blocks; block < heap->blocks + NUM_BLOCKS; ++block)
			if (block->flag == 2) block->flag = 0;
			else memset(block->line_marks, 0, sizeof block->line_marks);
	} else for (size_t i = 0; i < heap->modset.len; ++i) {
			struct BumpPointer p = heap->modset.xs[i];
			// Clear portions of object map that were allocated into
			char *x = (char *)heap->object_map + (p.limit - (char *)heap) / (GC_ALIGNMENT * CHAR_BIT);
			memset(x, 0, (p.cursor - p.limit) / (GC_ALIGNMENT * CHAR_BIT));
	}
	heap->modset.len = 0;
	gc_trace_roots(heap, mark_color);
	while (heap->mark_stack.p > heap->mark_stack.beg) { // Trace live objects
		void *p = *--heap->mark_stack.p;
		object_map_add(heap, p);
		gc_object_visit(heap, mark_color, p);
	}

	heap->ptr = heap->overflow_ptr = NULL_BUMP_PTR(heap->blocks);
	heap->free_len = heap->recycled_len = 0;
	for (struct GcBlock *block = heap->blocks; block < heap->blocks + NUM_BLOCKS; ++block)
		switch (sweep(block)) {
		case UNAVAILABLE: break;
		case RECYCLABLE: heap->recycled[heap->recycled_len++] = block; break;
		case FREE: heap->free[heap->free_len++] = block; break;
		}

	heap->is_major_gc = heap->free_len <= NUM_BLOCKS / 4;
	heap->is_defrag = heap->free_len <= 2 * MIN_FREE;
	heap->mark_color ^= !heap->is_major_gc;
	heap->inhibit_gc = false;
}
