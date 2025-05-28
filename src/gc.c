#include "gc.h"
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <sys/mman.h>
#include <ucontext.h>
#include "lisp.h"

#ifdef __SANITIZE_ADDRESS__
#include <sanitizer/asan_interface.h>
#else
#define ASAN_POISON_MEMORY_REGION(addr, size) ((void) (addr), (void) (size))
#define ASAN_UNPOISON_MEMORY_REGION(addr, size) ((void) (addr), (void) (size))
#endif

#ifdef __AVX2__
#include <immintrin.h>
#endif

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

static struct BumpPointer block_bump_ptr(struct GcBlock *block) {
	return (struct BumpPointer) { (&block->data)[1], block->data };
}

#ifndef GC_HEAP_SIZE
#define GC_HEAP_SIZE 0x800000 ///< GC heap allocation size in bytes.
#endif
#define NUM_BLOCKS (GC_HEAP_SIZE / sizeof(struct GcBlock) - 1)
#define MIN_FREE (NUM_BLOCKS * 3 / 100)
#define OBJECT_MAP_SIZE (sizeof(struct GcHeap) / (GC_ALIGNMENT * CHAR_BIT))

struct GcHeap {
	// Store at same offset to use a single pointer for both
	struct LispCtx lisp_ctx;

	struct BumpPointer ptr, overflow_ptr; ///< Bump pointer for medium objects.
	struct GcBlock **free, **recycled;
	size_t free_len, recycled_len;

	bool mark_color, inhibit_gc, is_defrag, is_major_gc;
	char *object_map; ///< Bitset of object start positions.
	struct TraceStack { size_t length, capacity; void **items; } trace_stack;
	struct GcBlock blocks[NUM_BLOCKS];
};

struct GcHeap *gc_new() {
	struct GcHeap *heap;
	size_t alignment =
#if USE_COMPRESSED_PTRS
		/* 4 GiB */ 1ull << 32;
#else
		alignof(struct GcHeap);
#endif
	char *p;
	if ((p = mmap(NULL, sizeof *heap + alignment - 1, PROT_READ | PROT_WRITE,
				MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0))
		== MAP_FAILED) return NULL;
	heap = (struct GcHeap *) ALIGN_UP(p, alignment);
	munmap(p, (char *) heap - p);
	munmap(heap + 1, p + sizeof *heap + alignment - 1 - (char *) (heap + 1));

	heap->mark_color = heap->inhibit_gc = heap->is_defrag = heap->is_major_gc = false;
	heap->trace_stack = (struct TraceStack) {};
	heap->free = NULL;
	if (!((heap->object_map = calloc(1, OBJECT_MAP_SIZE))
			&& (heap->free = malloc(2 * NUM_BLOCKS * sizeof *heap->free)))) {
		gc_free(heap);
		return NULL;
	}
	heap->free_len = NUM_BLOCKS - 1;
	heap->recycled_len = 0;
	heap->recycled = heap->free + NUM_BLOCKS;

	struct GcBlock *blocks = heap->blocks;
	heap->ptr = block_bump_ptr(blocks);
	heap->overflow_ptr = NULL_BUMP_PTR(blocks);
	for (struct GcBlock *block = blocks; block < blocks + NUM_BLOCKS; ++block) {
		ASAN_POISON_MEMORY_REGION(block->data, sizeof block->data);
#ifndef __linux__
		memset(block->line_marks, 0, sizeof block->line_marks);
		blocks->flag = 0;
#endif
	}
	for (unsigned i = 1; i < NUM_BLOCKS; ++i) heap->free[i - 1] = blocks + i;
	return heap;
}

void gc_free(struct GcHeap *heap) {
	free(heap->trace_stack.items);
	free(heap->free);
	free(heap->object_map);
	munmap(heap, sizeof *heap);
}

/** Remembers @a x as a live allocated object location. */
static void object_map_add(struct GcHeap *heap, char *x) {
	size_t i = (x - (char *) heap) / GC_ALIGNMENT;
	heap->object_map[i / CHAR_BIT] |= 1 << i % CHAR_BIT;
}

/** Removes @a x from the object map, returning whether it was present. */
static bool object_map_remove(struct GcHeap *heap, uintptr_t x) {
	size_t i = (x - (uintptr_t) heap) / GC_ALIGNMENT;
	if (x % GC_ALIGNMENT || x - (uintptr_t) heap->blocks >= sizeof heap->blocks)
		return false;
	char *v = heap->object_map + i / CHAR_BIT, mask = 1 << i % CHAR_BIT;
	if (*v & mask) { *v &= ~mask; return true; }
	return false;
}

enum {
	GC_MARK = 1, ///< Object mark bit: Ensures transitive closure terminates.
	GC_FORWARDED = 2,
};

[[gnu::noinline]]
static void *alloc_slow_path(struct GcHeap *heap, size_t alignment, size_t size) {
	struct BumpPointer *ptr = &heap->ptr;
	if (size <= GC_LINE_SIZE) {
		struct GcBlock *block = GC_BLOCK(ptr->limit);
		if ((*ptr = next_gap(block, ptr->limit)).cursor) goto out_bump;
		if (heap->recycled_len) {
			block = heap->recycled[--heap->recycled_len];
			// Recycled blocks have gaps of >=1 line; enough for small objects
			*ptr = next_gap(block, (&block->data)[1]);
			goto out_bump;
		}
	} else { // Demand-driven overflow allocation
		char *p;
		if ((p = bump_alloc(ptr = &heap->overflow_ptr, alignment, size))) return p;
	}
	// Acquire a free block
	if (heap->free_len <= MIN_FREE) garbage_collect(heap);
	if (!heap->free_len) return NULL;
	*ptr = block_bump_ptr(heap->free[--heap->free_len]);
out_bump:
	ASAN_POISON_MEMORY_REGION(ptr->limit, ptr->cursor - ptr->limit);
	memset(heap->object_map + (ptr->limit - (char *) heap) / (GC_ALIGNMENT * CHAR_BIT),
		0, (ptr->cursor - ptr->limit) / (GC_ALIGNMENT * CHAR_BIT));
	return bump_alloc(ptr, alignment, size);
}

void *gc_alloc(struct GcHeap *heap, size_t alignment, size_t size) {
	char *p;
	if (UNLIKELY(size > sizeof (struct GcBlock) {}.data)
		|| !(LIKELY(p = bump_alloc(&heap->ptr, alignment, size))
			|| (p = alloc_slow_path(heap, alignment, size)))) return NULL;
	ASAN_UNPOISON_MEMORY_REGION(p, size);
	*(struct GcObjectHeader *) p = (struct GcObjectHeader) { .flags = heap->mark_color };
	object_map_add(heap, p);
	return p;
}

[[gnu::noinline]] static void trace_stack_grow(struct GcHeap *heap) {
	struct TraceStack *stack = &heap->trace_stack;
	size_t new_capacity = stack->capacity ? 2 * stack->capacity : 8;
	void **items;
	if (!(items = realloc(stack->items, new_capacity * sizeof *items)))
		die("malloc failed");
	stack->items = items;
	stack->capacity = new_capacity;
}

static void trace_stack_push(struct GcHeap *heap, void *x) {
	if (UNLIKELY(heap->trace_stack.length >= heap->trace_stack.capacity))
		trace_stack_grow(heap);
	heap->trace_stack.items[heap->trace_stack.length++] = x;
}

void gc_log_object(struct GcHeap *heap, struct GcObjectHeader *src) {
	src->flags &= ~GC_UNLOGGED;
	trace_stack_push(heap, src); // Add to remembered set
}

void *gc_trace(struct GcHeap *heap, void *p) {
	struct GcObjectHeader *hdr = p;
	struct GcRef *fwd = (struct GcRef *) ALIGN_UP(hdr + 1, alignof(struct GcRef));
	if ((hdr->flags & GC_MARK) == heap->mark_color) // Already traced
		return hdr->flags & GC_FORWARDED ? (void *) GC_DECOMPRESS(heap, *fwd) : p;
	hdr->flags = heap->mark_color | GC_UNLOGGED;

	// Opportunistic evacuation if block is a defragmentation candidate
	struct GcBlock *block = GC_BLOCK(p);
	size_t alignment, size;
	void *q;
	if (block->flag && (size = gc_object_size(p, &alignment),
			q = gc_alloc(heap, alignment, size))) {
		memcpy(q, p, size);
		*fwd = GC_COMPRESS(p = q); // Leave forwarding pointer
		hdr->flags |= GC_FORWARDED;
	} else object_map_add(heap, p);

	trace_stack_push(heap, p);
	return p;
}

void gc_pin(struct GcHeap *heap, void *p) {
	struct GcObjectHeader *hdr = p;
	assert(!(hdr->flags & GC_FORWARDED) && "Already forwarded");
	hdr->flags = heap->mark_color | GC_UNLOGGED;
	object_map_add(heap, p);
	trace_stack_push(heap, p);
}

extern void *__libc_stack_end; ///< Highest used stack address.
static void scan_stack(struct GcHeap *heap) {
	void *base = __libc_stack_end, *sp = __builtin_frame_address(0);
	sp = (void *) ((uintptr_t) sp & ~(alignof(struct GcRef) - 1));
	for (struct GcRef *p = sp; (void *) p <= base; ++p) {
		uintptr_t x = GC_DECOMPRESS(heap, *p) & ~1ull;
		// Pin to not "forward" a false positive root
		if (object_map_remove(heap, x)) gc_pin(heap, (void *) x);
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
	if (heap->inhibit_gc) return; else heap->inhibit_gc = true;
	if (heap->is_major_gc) {
		if (heap->is_defrag) select_defrag_candidates(heap);
		// TODO Cyclical line marks (see MMTk) need not be reset, but
		// would complicate gc_mark().
		for (struct GcBlock *block = heap->blocks; block < heap->blocks + NUM_BLOCKS; ++block)
			if (block->flag == 2) block->flag = 0;
			else memset(block->line_marks, 0, sizeof block->line_marks);
		heap->trace_stack.length = 0; // Ignore remembered set
	}
	// Unlog remembered set
	for (size_t i = 0; i < heap->trace_stack.length; ++i)
		((struct GcObjectHeader *) heap->trace_stack.items[i])->flags |= GC_UNLOGGED;
	// Alternate liveness color to skip zeroing object marks
	heap->mark_color = !heap->mark_color;

	// Push callee-saved register onto the stack
	ucontext_t ctx;
	if (UNLIKELY(getcontext(&ctx))) {
		fputs("getcontext() failed\n", stderr);
		__builtin_unwind_init();
	}
	scan_stack(heap); // Collect conservative roots
	// Prevent prematurely popping register contents
#ifdef __GNUC__
	__asm__ volatile ("" : : "X" (&ctx) : "memory");
#else
	gc_nop_sink = &ctx;
#endif

	if (heap->is_major_gc) memset(heap->object_map, 0, OBJECT_MAP_SIZE);
	gc_trace_roots(heap);
	while (heap->trace_stack.length) // Trace live objects
		gc_object_visit(heap, heap->trace_stack.items[--heap->trace_stack.length]);

	heap->ptr = heap->overflow_ptr = NULL_BUMP_PTR(heap->blocks);
	heap->free_len = heap->recycled_len = 0;
	for (struct GcBlock *block = heap->blocks; block < heap->blocks + NUM_BLOCKS; ++block)
		switch (sweep(block)) {
		case UNAVAILABLE: break;
		case RECYCLABLE: heap->recycled[heap->recycled_len++] = block; break;
		case FREE: heap->free[heap->free_len++] = block; break;
		}

	if (!(heap->is_major_gc = heap->free_len <= NUM_BLOCKS / 4))
		heap->mark_color = !heap->mark_color;
	heap->is_defrag = heap->free_len <= 2 * MIN_FREE;
	heap->inhibit_gc = false;
}
