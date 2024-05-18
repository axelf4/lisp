#include "gc.h"
#include <stdlib.h>
#include <stdio.h>
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

struct BumpPointer { char *cursor, *limit; };

[[gnu::alloc_align (2), gnu::alloc_size (3)]]
static void *bump_alloc(struct BumpPointer *ptr, size_t align, size_t size) {
	// Bump allocate downward to align with a single AND instruction
	return (size_t) (ptr->cursor - ptr->limit) >= size
		? ptr->cursor = (char *) ((uintptr_t) (ptr->cursor - size) & ~(align - 1))
		: NULL;
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

struct Vec { size_t length, capacity; void **items; };

static bool vec_reserve(struct Vec *vec, size_t additional) {
	size_t n = vec->length + additional;
	if (LIKELY(n <= vec->capacity)) return true;
	size_t new_capacity = MAX(vec->capacity ? 2 * vec->capacity : 4, n);
	void **items;
	if (!(items = realloc(vec->items, new_capacity * sizeof *items))) return false;
	vec->items = items;
	vec->capacity = new_capacity;
	return true;
}

static void vec_push(struct Vec *vec, void *x) { vec->items[vec->length++] = x; }

static void *vec_pop(struct Vec *vec) {
	return vec->length ? vec->items[--vec->length] : NULL;
}

struct GcHeap {
	// Stuff into same allocation as an optimization
	struct LispCtx lisp_ctx;

	struct GcBlock *head, ///< The current block being allocated into.
		*overflow; ///< Block kept for writing medium objects.
	struct BumpPointer ptr, overflow_ptr;
	struct Vec free, recycled, trace_stack;

	bool mark_color, inhibit_gc, defrag;
	char *object_map; ///< Bitset of object start positions.
	size_t object_map_size;
	struct GcBlock blocks[];
};

#define NUM_BLOCKS 255

struct GcHeap *gc_new() {
	size_t alignment = (size_t) 1 << 32,
		size = sizeof(struct GcHeap) + NUM_BLOCKS * sizeof(struct GcBlock);
	void *p;
	if ((p = mmap(NULL, size + alignment - 1, PROT_READ | PROT_WRITE,
				MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0))
		== MAP_FAILED) return NULL;
	// Align to multiple of 4 GiB
	struct GcHeap *heap = (struct GcHeap *) (((uintptr_t) p + alignment - 1) & ~(alignment - 1));
	if (heap != p) munmap(p, (char *) heap - (char *) p);
	char *aligned_end = (char *) p + size + alignment - 1;
	if (aligned_end != (char *) heap + size)
		munmap((char *) heap + size, aligned_end - ((char *) heap + size));
	struct GcBlock *blocks = heap->blocks;

	bool success = true;
	size_t object_map_size = size / (GC_MIN_ALIGNMENT * CHAR_BIT);
	char *object_map;
	struct Vec free = {}, recycled = {};
	if (!((object_map = calloc(object_map_size, 1))
			&& vec_reserve(&free, NUM_BLOCKS)
			&& vec_reserve(&recycled, NUM_BLOCKS))) {
		success = false;
		goto out;
	}

	for (struct GcBlock *block = blocks; block < blocks + NUM_BLOCKS; ++block) {
		ASAN_POISON_MEMORY_REGION(block->data, sizeof block->data);
#ifndef __linux__
		memset(block->line_marks, 0, sizeof block->line_marks);
		blocks->flag = 0;
#endif
	}
	for (struct GcBlock *block = blocks + 1; block < blocks + NUM_BLOCKS; ++block)
		vec_push(&free, block);

out:
	*heap = (struct GcHeap) {
		.head = blocks, .ptr = empty_block_ptr(blocks),
		.overflow_ptr = { blocks->data, blocks->data }, // Lazily init
		.free = free, .recycled = recycled,
		.object_map = object_map, .object_map_size = object_map_size,
	};
	if (!success) { gc_free(heap); return NULL; }
	return heap;
}

void gc_free(struct GcHeap *heap) {
	free(heap->free.items);
	free(heap->recycled.items);
	free(heap->trace_stack.items);
	free(heap->object_map);
	munmap(heap, sizeof *heap + NUM_BLOCKS * sizeof *heap->blocks);
}

/** Remembers @a x as a live allocated object location. */
static void object_map_add(struct GcHeap *heap, char *x) {
	unsigned i = (x - (char *) heap) / GC_MIN_ALIGNMENT;
	heap->object_map[i / CHAR_BIT] |= 1 << i % CHAR_BIT;
}

/** Removes @a x from the object map, returning whether it was present. */
static bool object_map_remove(struct GcHeap *heap, uintptr_t x) {
	unsigned i = (x - (uintptr_t) heap) / GC_MIN_ALIGNMENT;
	if (x % GC_MIN_ALIGNMENT || x < (uintptr_t) heap
		|| i / CHAR_BIT > heap->object_map_size) return false;
	char *v = heap->object_map + i / CHAR_BIT, mask = 1 << i % CHAR_BIT;
	if (*v & mask) { *v &= ~mask; return true; }
	return false;
}

enum {
	GC_MARK = 1, ///< Object mark bit: Ensures transitive closure terminates.
	GC_FORWARDED = 2,
};

#define MIN_FREE (NUM_BLOCKS / (100 / 3))

void *gc_alloc(struct GcHeap *heap, size_t alignment, size_t size) {
	char *p;
	struct BumpPointer *ptr = &heap->ptr;
	if ((p = bump_alloc(ptr, alignment, size))) goto out;
	struct GcBlock **block = &heap->head, *new_block;
	if (LIKELY(size <= GC_LINE_SIZE)) {
		if ((*ptr = next_gap(*block, ptr->limit, size)).cursor) goto out_bump;
		if ((new_block = vec_pop(&heap->recycled))) {
			// Recycled blocks have gaps of >=1 line; enough for small objects
			*ptr = next_gap(*block = new_block, new_block->data + sizeof new_block->data, size);
			goto out_bump;
		}
	} else if (size <= sizeof (*block)->data) { // Demand-driven overflow allocation
		block = &heap->overflow;
		ptr = &heap->overflow_ptr;
		if ((p = bump_alloc(ptr, alignment, size))) goto out;
	} else return NULL;
	// Acquire a free block
	if (heap->free.length <= MIN_FREE && !heap->inhibit_gc) garbage_collect(heap);
	if (!(new_block = vec_pop(&heap->free))) return NULL;
	*ptr = empty_block_ptr(*block = new_block);
out_bump:
	p = bump_alloc(ptr, alignment, size);
out:
	ASAN_UNPOISON_MEMORY_REGION(p, size);
	*(struct GcObjectHeader *) p = (struct GcObjectHeader) { .flags = heap->mark_color };
	object_map_add(heap, p);
	return p;
}

void *gc_trace(struct GcHeap *heap, void *p) {
	struct GcObjectHeader *hdr = p;
	struct GcRef *fwd = (struct GcRef *) (((uintptr_t) (hdr + 1) // Forwarding pointer
			+ alignof(struct GcRef) - 1) & ~(alignof(struct GcRef) - 1));
	if ((hdr->flags & GC_MARK) == heap->mark_color) // Already traced
		return hdr->flags & GC_FORWARDED ? (void *) GC_DECOMPRESS(heap, *fwd) : p;
	hdr->flags = heap->mark_color;

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

	if (!vec_reserve(&heap->trace_stack, 1)) die("malloc failed");
	vec_push(&heap->trace_stack, p);
	return p;
}

static void pin_and_trace(struct GcHeap *heap, void *p) {
	((struct GcObjectHeader *) p)->flags = heap->mark_color;
	object_map_add(heap, p);
	gc_object_visit(heap, p);
}

/** Call @a fn with callee-saved registers pushed to the stack. */
#ifndef __GNUC__
volatile void *gc_nop_sink;
#endif
static void with_callee_saves_pushed(void (*fn)(void *), void *arg) {
	ucontext_t ctx;
	if (getcontext(&ctx) < 0) {
		puts("getcontext() failed");
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
		if (object_map_remove(heap, x)) {
			if (!vec_reserve(&heap->trace_stack, 1)) die("malloc failed");
			vec_push(&heap->trace_stack, (void *) x);
		}
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
} sweep_block(struct GcBlock *block) {
	unsigned unavailable_lines = 0;
	for (unsigned i = 0; i < GC_LINE_COUNT; ++i)
		if (block->line_marks[i]) ++unavailable_lines;
		else ASAN_POISON_MEMORY_REGION(block->data + GC_LINE_SIZE * i, GC_LINE_SIZE);
	return !unavailable_lines ? FREE
		: unavailable_lines < GC_LINE_COUNT ? RECYCLABLE
		: UNAVAILABLE;
}

void garbage_collect(struct GcHeap *heap) {
	heap->inhibit_gc = true;
	with_callee_saves_pushed(collect_roots, heap); // Collect conservative roots

	if (heap->defrag) {
#define MAX_HOLES ((GC_LINE_COUNT + 2) / 3)
		unsigned mark_histogram[MAX_HOLES] = {};
		for (size_t i = 0; i < heap->recycled.length; ++i) {
			struct GcBlock *block = heap->recycled.items[i];
			struct BlockStats stats = block_stats(block);
			mark_histogram[block->flag = stats.num_holes] += stats.num_marks;
		}

		ssize_t available_space = GC_LINE_SIZE * GC_LINE_COUNT * heap->free.length;
		unsigned bin = MAX_HOLES;
		do available_space -= GC_LINE_SIZE * mark_histogram[--bin];
		while (available_space > 0 && bin);

		for (size_t i = 0; i < heap->recycled.length;) {
			struct GcBlock *block = heap->recycled.items[i];
			bool is_defrag_candidate = (block->flag = block->flag > bin);
			if (is_defrag_candidate)
				// Remove from recycled list to not evacuate into itself
				heap->recycled.items[i] = vec_pop(&heap->recycled);
			else ++i;
		}
	}

	memset(heap->object_map, 0, heap->object_map_size); // Clear object map
	// Unmark blocks
	for (struct GcBlock *block = heap->blocks; block < heap->blocks + NUM_BLOCKS; ++block)
		memset(block->line_marks, 0, sizeof block->line_marks);

	// Alternate the liveness color to skip zeroing object marks
	heap->mark_color = !heap->mark_color;
	size_t num_roots = heap->trace_stack.length;
	for (size_t i = 0; i < num_roots; ++i)
		// Pin to not "evacuate" a false positive root
		pin_and_trace(heap, heap->trace_stack.items[i]);
	if (heap->trace_stack.length -= num_roots) {
		size_t n = MIN(num_roots, heap->trace_stack.length);
		memcpy(heap->trace_stack.items,
			heap->trace_stack.items + num_roots + heap->trace_stack.length - n,
			n * sizeof *heap->trace_stack.items);
	}
	gc_trace_roots(heap);
	size_t prev_num_free = heap->free.length;
	while (heap->trace_stack.length) // Trace live objects
		gc_object_visit(heap, heap->trace_stack.items[--heap->trace_stack.length]);

	heap->free.length = heap->recycled.length = 0;
	for (struct GcBlock *block = heap->blocks; block < heap->blocks + NUM_BLOCKS; ++block) {
		block->flag = 0;
		switch (sweep_block(block)) {
		case UNAVAILABLE: break;
		case RECYCLABLE: vec_push(&heap->recycled, block); break;
		case FREE: vec_push(&heap->free, block); break;
		}
	}

	heap->defrag = heap->free.length <= MAX(MIN_FREE, prev_num_free);
	heap->inhibit_gc = false;
}
