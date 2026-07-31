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
#define GC_HEAP_SIZE /* 64 MiB */ 0x4000000 ///< GC heap allocation size in bytes.
#endif
#define LOS_SIZE (offsetof(struct GcHeap, blocks) / GC_ALIGNMENT * GC_BLOCK_SIZE)
#define NUM_BLOCKS (GC_HEAP_SIZE / GC_BLOCK_SIZE - 1)
#define MIN_FREE (0.3 * NUM_BLOCKS)
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

struct LargeObject {
	alignas(GC_BLOCK_SIZE) unsigned char flag; ///< @see GcBlock::flag.
	bool is_live;
	struct LargeObject *next;
	alignas(max_align_t) struct GcObjectHeader hdr;
};

struct GcHeap {
	// Store at same offset to use a single pointer for both
	struct LispCtx lisp_ctx;

	struct BumpPointer ptr, overflow_ptr; ///< Bump pointer for medium objects.
	struct GcBlock **free, **recycled;
	unsigned free_len, recycled_len;

	bool mark_color, inhibit_gc, is_major_gc, is_defrag;
	unsigned *object_map; ///< Bitset of object start positions.
	struct MarkStack { void **beg, **p, **end; } mark_stack;
	struct ModSet { unsigned len, capacity; struct BumpPointer *xs; } modset;
	struct LargeObject *los; ///< Large Object Space (LOS) implicit free list.
	struct GcBlock blocks[NUM_BLOCKS];
};

struct GcHeap *gc_new() {
	struct GcHeap *heap;
	size_t alignment = alignof(struct GcHeap);
#if USE_COMPRESSED_PTRS
	alignment = /* 4 GiB */ 1ull << 32;
#endif
	char *p;
	if ((p = mmap(NULL, sizeof *heap + LOS_SIZE + alignment - 1, PROT_READ | PROT_WRITE,
				MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0))
		== MAP_FAILED) return NULL;
	heap = (struct GcHeap *)ALIGN_UP(p, alignment);
	munmap(p, (char *)heap - p);
	munmap((char *)(heap + 1) + LOS_SIZE, p + alignment - 1 - (char *)heap);

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

	struct LargeObject *l0 = heap->los = (struct LargeObject *)(heap + 1),
		*l1 = (struct LargeObject *)((char *)l0 + LOS_SIZE) - 1;
	l0->next = l1;
	l1->next = NULL;
	ASAN_POISON_MEMORY_REGION(&l0->hdr, (char *)l1 - (char *)&l0->hdr);

	struct GcBlock *blocks = heap->blocks;
	heap->ptr = heap->overflow_ptr = NULL_BUMP_PTR(blocks);
	for (struct GcBlock *block = blocks; block < blocks + NUM_BLOCKS; ++block) {
		ASAN_POISON_MEMORY_REGION(block->data, sizeof block->data);
#ifndef __linux__
		blocks->flag = 0;
		memset(block->line_marks, 0, sizeof block->line_marks);
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
	munmap(heap, sizeof *heap + LOS_SIZE);
}

/** Remembers @a p as a live allocated object location. */
static void object_map_add(struct GcHeap *heap, char *p) {
	unsigned i = (p - (char *)heap), n = CHAR_BIT * sizeof *heap->object_map;
	if (p < (char *)(heap + 1)) i /= GC_ALIGNMENT; else i /= alignof(struct LargeObject);
	heap->object_map[i / n] |= 1u << i % n;
}

/** Removes @a x from the object map, returning whether it was present. */
static bool object_map_remove(struct GcHeap *heap, uintptr_t x) {
	if (x - (uintptr_t)heap->blocks >= sizeof heap->blocks + LOS_SIZE) return false;
	unsigned i = x - (uintptr_t)heap, n = CHAR_BIT * sizeof *heap->object_map;
	if (x >= (uintptr_t)(heap + 1)) {
		if ((i -= offsetof(struct LargeObject, hdr)) % alignof(struct LargeObject))
			return false;
		i /= alignof(struct LargeObject);
	} else { if (i % GC_ALIGNMENT) return false; i /= GC_ALIGNMENT; }
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
	if (!heap->free_len) die("gc_alloc failed");
	struct GcBlock *block = heap->free[--heap->free_len];
	*ptr = (struct BumpPointer) { (&block->data)[1], block->data };
out_bump:
	ASAN_POISON_MEMORY_REGION(ptr->limit, ptr->cursor - ptr->limit);
	if (heap->modset.len >= heap->modset.capacity) modset_grow(&heap->modset);
	heap->modset.xs[heap->modset.len++] = *ptr;
	return bump_alloc(ptr, alignment, size);
}

[[gnu::cold]] static void *alloc_large(struct GcHeap *heap, [[maybe_unused]] size_t alignment, size_t size) {
	assert(alignment <= alignof(max_align_t));
	assert(!heap->inhibit_gc && "evacuated large object");
do_retry: struct LargeObject *obj = heap->los;
	do if (!obj->is_live && (size_t)((char *)obj->next - (char *)&obj->hdr) >= size)
		   goto found; // First-fit allocation
	while ((obj = obj->next)->next);
	garbage_collect(heap);
	goto do_retry;
found:
	struct LargeObject *new = (struct LargeObject *)
		ALIGN_UP((char *)&obj->hdr + size, alignof(struct LargeObject));
	if (new < obj->next) {
		ASAN_UNPOISON_MEMORY_REGION(new, sizeof *new);
		*new = (struct LargeObject){ .next = obj->next };
		obj->next = new;
	}
	ASAN_UNPOISON_MEMORY_REGION(&obj->hdr, size);
	obj->is_live = true;
	obj->hdr = (struct GcObjectHeader){ heap->mark_color };
	object_map_add(heap, (char *)&obj->hdr);
	return &obj->hdr;
}

void *gc_alloc(struct GcHeap *heap, size_t alignment, size_t size) {
	if (size > sizeof (struct GcBlock){}.data) return alloc_large(heap, alignment, size);
	char *p = LIKELY(p = bump_alloc(&heap->ptr, alignment, size)) ? p
		: alloc_slow_path(heap, alignment, size);
	ASAN_UNPOISON_MEMORY_REGION(p, size);
	*(struct GcObjectHeader *) p = (struct GcObjectHeader) { heap->mark_color };
	if (p < (char *)(heap + 1)) object_map_add(heap, p); else unreachable();
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
	__m256i_u *ys = (__m256i_u *)block->line_marks;
	__m256i *xs = (__m256i *)(block->line_marks - 1), as[] = {
		_mm256_or_si256(_mm256_load_si256(xs), _mm256_loadu_si256(ys)),
		_mm256_or_si256(_mm256_load_si256(xs + 1), _mm256_loadu_si256(ys + 1)),
		_mm256_or_si256(_mm256_load_si256(xs + 2), _mm256_loadu_si256(ys + 2)),
		_mm256_or_si256(_mm256_load_si256(xs + 3),
			_mm256_andnot_si256( // Mask block->line_marks[127]
				_mm256_set_epi64x(-1ull << 56, 0, 0, 0), _mm256_loadu_si256(ys + 3))),
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

	for (size_t i = 0; i < heap->recycled_len; ++i) {
		struct GcBlock *block = heap->recycled[i];
		bool is_defrag_candidate = block->flag > bin;
		block->flag = is_defrag_candidate ? 1 : 2;
		if (is_defrag_candidate)
			// Remove from recycled list to not evacuate into itself
			heap->recycled[i--] = heap->recycled[--heap->recycled_len];
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
	for (struct LargeObject *obj = heap->los, *prev = NULL; obj->next; obj = obj->next)
		if (obj->is_live && (obj->hdr.flags & GC_MARK) == mark_color) prev = NULL;
		else {
			bool was_live = obj->is_live;
			obj->is_live = false;
			object_map_remove(heap, (uintptr_t)&obj->hdr);
			if (prev) { prev->next = obj->next; obj = prev; } else prev = obj;
			if (was_live) ASAN_POISON_MEMORY_REGION(&obj->hdr, (char *)obj->next - (char *)&obj->hdr);
		}

	heap->is_major_gc = heap->free_len <= NUM_BLOCKS / 4;
	heap->is_defrag = heap->free_len <= 2 * MIN_FREE;
	heap->mark_color ^= !heap->is_major_gc;
	heap->inhibit_gc = false;
}
