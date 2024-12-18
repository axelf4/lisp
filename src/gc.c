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

struct BumpPointer { char *cursor, *limit; };

[[gnu::alloc_align (2), gnu::alloc_size (3)]]
static void *bump_alloc(struct BumpPointer *ptr, size_t align, size_t size) {
	// Bump allocate downward to align with a single AND instruction
	if ((uintptr_t) ptr->cursor - size < (uintptr_t) ptr->limit) return NULL;
	char *p = (char *) ((uintptr_t) (ptr->cursor - size) & ~(align - 1));
	if (p) return ptr->cursor = p; else unreachable();
}

static_assert(GC_LINE_SIZE % alignof(max_align_t) == 0);
/** Locates the next gap of unmarked lines of sufficient size. */
static struct BumpPointer next_gap(struct GcBlock *block, char *top, size_t size) {
	unsigned required_lines = (size + GC_LINE_SIZE - 1) / GC_LINE_SIZE, count = 0,
		end = (top - (char *) block) / GC_LINE_SIZE;
	for (unsigned i = end; i-- > 0;)
		if (block->line_marks[i]) {
			if (count > required_lines)
				// At least 2 preceeding lines were unmarked. Consider
				// the previous line as conservatively marked.
				return (struct BumpPointer) {
					block->data + GC_LINE_SIZE * end,
					block->data + GC_LINE_SIZE * (i + 2),
				};
			count = 0;
			end = i;
		} else ++count;
	return count >= required_lines
		? (struct BumpPointer) { block->data + GC_LINE_SIZE * end, block->data }
		: (struct BumpPointer) {};
}

static struct BumpPointer empty_block_ptr(struct GcBlock *block) {
	return (struct BumpPointer) { block->data + sizeof block->data, block->data };
}

#ifndef GC_HEAP_SIZE
#define GC_HEAP_SIZE 0x800000 ///< GC heap allocation size in bytes.
#endif
#define NUM_BLOCKS (GC_HEAP_SIZE / sizeof(struct GcBlock) - 1)
#define MIN_FREE (NUM_BLOCKS / (100 / 3))
#define OBJECT_MAP_SIZE (sizeof(struct GcHeap) / (GC_MIN_ALIGNMENT * CHAR_BIT))

struct GcHeap {
	// Store at same offset to use a single pointer for both
	struct LispCtx lisp_ctx;

	struct BumpPointer ptr, overflow_ptr;
	struct GcBlock *head, ///< The current block being allocated into.
		*overflow, ///< Block kept for writing medium objects.
		**free, **recycled;
	size_t free_len, recycled_len;

	bool mark_color, inhibit_gc, defrag, is_major_gc;
	char *object_map; ///< Bitset of object start positions.
	struct TraceStack { size_t length, capacity; void **items; } trace_stack;
	struct GcBlock blocks[NUM_BLOCKS];
};

struct GcHeap *gc_new() {
	struct GcHeap *heap;
	size_t alignment = 1ull << 32;
	void *p;
	if ((p = mmap(NULL, sizeof *heap + alignment - 1, PROT_READ | PROT_WRITE,
				MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0))
		== MAP_FAILED) return NULL;
	heap = (struct GcHeap *) ALIGN_UP(p, alignment); // Align to multiple of 4 GiB
	if (heap != p) munmap(p, (char *) heap - (char *) p);
	char *end = (char *) p + sizeof *heap + alignment - 1;
	if (end != (char *) (heap + 1)) munmap(heap + 1, end - (char *) (heap + 1));

	heap->mark_color = heap->inhibit_gc = heap->defrag = heap->is_major_gc = false;
	heap->trace_stack = (struct TraceStack) {};
	heap->free = NULL;
	if (!((heap->object_map = calloc(OBJECT_MAP_SIZE, 1))
			&& (heap->free = malloc(2 * NUM_BLOCKS * sizeof *heap->free)))) {
		gc_free(heap);
		return NULL;
	}
	heap->free_len = NUM_BLOCKS - 1;
	heap->recycled_len = 0;
	heap->recycled = heap->free + NUM_BLOCKS;

	struct GcBlock *blocks = heap->blocks;
	heap->ptr = empty_block_ptr(heap->head = blocks);
	heap->overflow_ptr = (struct BumpPointer) { blocks->data, blocks->data }; // Lazily init
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
	size_t i = (x - (char *) heap) / GC_MIN_ALIGNMENT;
	heap->object_map[i / CHAR_BIT] |= 1 << i % CHAR_BIT;
}

/** Removes @a x from the object map, returning whether it was present. */
static bool object_map_remove(struct GcHeap *heap, uintptr_t x) {
	size_t i = (x - (uintptr_t) heap) / GC_MIN_ALIGNMENT;
	if (x % GC_MIN_ALIGNMENT || x < (uintptr_t) heap
		|| i / CHAR_BIT > OBJECT_MAP_SIZE) return false;
	char *v = heap->object_map + i / CHAR_BIT, mask = 1 << i % CHAR_BIT;
	if (*v & mask) { *v &= ~mask; return true; }
	return false;
}

enum {
	GC_MARK = 1, ///< Object mark bit: Ensures transitive closure terminates.
	GC_FORWARDED = 2,
};

void *gc_alloc(struct GcHeap *heap, size_t alignment, size_t size) {
	char *p;
	struct BumpPointer *ptr = &heap->ptr;
	if (LIKELY(p = bump_alloc(ptr, alignment, size))) goto out;
	struct GcBlock **block = &heap->head;
	if (size <= GC_LINE_SIZE) {
		if ((*ptr = next_gap(*block, ptr->limit, size)).cursor) goto out_bump;
		if (heap->recycled_len) {
			*block = heap->recycled[--heap->recycled_len];
			// Recycled blocks have gaps of >=1 line; enough for small objects
			*ptr = next_gap(*block, (*block)->data + sizeof (*block)->data, size);
			goto out_bump;
		}
	} else if (LIKELY(size <= GC_BLOCK_SIZE)) { // Demand-driven overflow allocation
		if ((p = bump_alloc(ptr = &heap->overflow_ptr, alignment, size))) goto out;
		block = &heap->overflow;
	} else return NULL;
	// Acquire a free block
	if (heap->free_len <= MIN_FREE) garbage_collect(heap);
	if (!heap->free_len) return NULL;
	*ptr = empty_block_ptr(*block = heap->free[--heap->free_len]);
out_bump:
	p = bump_alloc(ptr, alignment, size);
out:
	ASAN_UNPOISON_MEMORY_REGION(p, size);
	*(struct GcObjectHeader *) p = (struct GcObjectHeader) { .flags = heap->mark_color };
	object_map_add(heap, p);
	return p;
}

static void trace_stack_push(struct GcHeap *heap, void *x) {
	struct TraceStack *vec = &heap->trace_stack;
	if (UNLIKELY(vec->length >= vec->capacity)) {
		size_t new_capacity = vec->capacity ? 2 * vec->capacity : 4;
		void **items;
		if (!(items = realloc(vec->items, new_capacity * sizeof *items)))
			die("malloc failed");
		vec->items = items;
		vec->capacity = new_capacity;
	}
	vec->items[vec->length++] = x;
}

void gc_log_object(struct GcHeap *heap, struct GcObjectHeader *src) {
	src->flags = (src->flags & ~GC_UNLOGGED) ^ heap->mark_color;
	trace_stack_push(heap, src); // Add to remembered set
}

void *gc_trace(struct GcHeap *heap, void *p) {
	struct GcObjectHeader *hdr = p;
	// Forwarding pointer
	struct GcRef *fwd = (struct GcRef *) ALIGN_UP(hdr + 1, alignof(struct GcRef));
	if ((hdr->flags & GC_MARK) == heap->mark_color) // Already traced
		return hdr->flags & GC_FORWARDED ? (void *) GC_DECOMPRESS(heap, *fwd) : p;
	hdr->flags = heap->mark_color | GC_UNLOGGED;

	// Opportunistic evacuation if block is a defragmentation candidate
	struct GcBlock *block = (struct GcBlock *) ((uintptr_t) hdr & ~(GC_BLOCK_SIZE - 1));
	size_t alignment, size;
	void *q;
	if (block->flag && (size = gc_object_size(p, &alignment),
			q = gc_alloc(heap, alignment, size))) {
		memcpy(q, p, size);
		*fwd = GC_COMPRESS(p = q);
		hdr->flags |= GC_FORWARDED;
	} else object_map_add(heap, p);

	trace_stack_push(heap, p);
	return p;
}

void gc_pin(struct GcHeap *heap, void *p) {
	struct GcObjectHeader *hdr = p;
	assert((hdr->flags & 1) ^ heap->mark_color && "Already traced");
	hdr->flags = heap->mark_color | GC_UNLOGGED;
	object_map_add(heap, p);
	trace_stack_push(heap, p);
}

/** Call @a fn with callee-saved registers pushed to the stack. */
#ifndef __GNUC__
volatile void *gc_nop_sink;
#endif
static void with_callee_saves_pushed(void (*fn)(void *), void *arg) {
	ucontext_t ctx;
	if (UNLIKELY(getcontext(&ctx))) {
		fputs("getcontext() failed\n", stderr);
		__builtin_unwind_init();
	}
	fn(arg);
	// Inhibit tail-call which would pop register contents prematurely
#ifdef __GNUC__
	__asm__ volatile ("" : : "X" (&ctx) : "memory");
#else
	gc_nop_sink = &ctx;
#endif
}

extern void *__libc_stack_end;
[[gnu::no_sanitize_address]] static void collect_roots(void *x) {
	struct GcHeap *heap = x;
	void *base = __libc_stack_end, *sp = __builtin_frame_address(0);
	sp = (void *) ((uintptr_t) sp & ~(alignof(void *) - 1));
	for (uintptr_t *p = sp; (void *) p < base; ++p) {
		uintptr_t x = *p & ~(uintptr_t) 1;
		if (object_map_remove(heap, x)) trace_stack_push(heap, (void *) x);
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

static enum BlockStatus {
	FREE, ///< Unallocated.
	RECYCLABLE, ///< Partly used with at least F=1 free lines.
	UNAVAILABLE, ///< No unmarked lines.
} sweep(struct GcBlock *block) {
	block->flag = 0;
	unsigned unavailable_lines = 0;
#if defined __SANITIZE_ADDRESS__ || !defined __AVX2__
	for (unsigned i = 0; i < GC_LINE_COUNT; ++i)
		if (block->line_marks[i]) ++unavailable_lines;
		else ASAN_POISON_MEMORY_REGION(block->data + GC_LINE_SIZE * i, GC_LINE_SIZE);
#else
	static_assert(GC_LINE_COUNT == 127);
	__m256i sums = _mm256_add_epi8(
		_mm256_add_epi8(_mm256_load_si256((const __m256i *) block->line_marks),
			_mm256_load_si256((const __m256i *) block->line_marks + 1)),
		_mm256_add_epi8(_mm256_load_si256((const __m256i *) block->line_marks + 2),
			_mm256_load_si256((const __m256i *) block->line_marks + 3))
	),
		total = _mm256_sad_epu8(sums, _mm256_setzero_si256());
	unavailable_lines = _mm256_extract_epi64(total, 0) + _mm256_extract_epi64(total, 1)
		+ _mm256_extract_epi64(total, 2) + _mm256_extract_epi64(total, 3);
#endif
	return !unavailable_lines ? FREE
		: unavailable_lines < GC_LINE_COUNT ? RECYCLABLE
		: UNAVAILABLE;
}

void garbage_collect(struct GcHeap *heap) {
	if (heap->inhibit_gc) return; else heap->inhibit_gc = true;
	if (heap->is_major_gc) heap->trace_stack.length = 0; // Ignore remembered set
	size_t remembered_len = heap->trace_stack.length;
	with_callee_saves_pushed(collect_roots, heap); // Collect conservative roots

	if (heap->defrag) {
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
			bool is_defrag_candidate = (block->flag = block->flag > bin);
			if (is_defrag_candidate)
				// Remove from recycled list to not evacuate into itself
				heap->recycled[i] = heap->recycled[--heap->recycled_len];
			else ++i;
		}
	}
	if (heap->is_major_gc) {
		memset(heap->object_map, 0, OBJECT_MAP_SIZE); // Clear object map
		// Unmark blocks
		for (struct GcBlock *block = heap->blocks; block < heap->blocks + NUM_BLOCKS; ++block)
			memset(block->line_marks, 0, sizeof block->line_marks);
	}
	// Alternate liveness color to skip zeroing object marks
	heap->mark_color = !heap->mark_color;
	for (size_t i = remembered_len; i < heap->trace_stack.length; ++i) {
		// Pin to not "evacuate" a false positive root
		char *p = heap->trace_stack.items[i];
		((struct GcObjectHeader *) p)->flags = heap->mark_color | GC_UNLOGGED;
		object_map_add(heap, p);
	}
	gc_trace_roots(heap);
	while (heap->trace_stack.length) // Trace live objects
		gc_object_visit(heap, heap->trace_stack.items[--heap->trace_stack.length]);

	heap->free_len = heap->recycled_len = 0;
	for (struct GcBlock *block = heap->blocks; block < heap->blocks + NUM_BLOCKS; ++block)
		switch (sweep(block)) {
		case UNAVAILABLE: break;
		case RECYCLABLE: heap->recycled[heap->recycled_len++] = block; break;
		case FREE: heap->free[heap->free_len++] = block; break;
		}

	if (!(heap->is_major_gc = heap->free_len <= NUM_BLOCKS / 4))
		heap->mark_color = !heap->mark_color;
	heap->defrag = heap->free_len <= 2 * MIN_FREE;
	heap->inhibit_gc = false;
}
