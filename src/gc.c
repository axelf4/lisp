#include "gc.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <ucontext.h>
#include "util.h"

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
				// the previous block as conservatively marked.
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

struct Vec { size_t length, capacity; void **items; };

static bool vec_reserve(struct Vec *vec, size_t additional) {
	size_t n = vec->length + additional;
	if (__builtin_expect(n <= vec->capacity, true)) return true;
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

#define BLOCKS_PER_CHUNK (128 - 1)

/** The largest internal unit of memory allocation. */
struct Chunk {
	alignas((BLOCKS_PER_CHUNK + 1) * alignof(struct GcBlock))
		struct GcBlock blocks[BLOCKS_PER_CHUNK];
	/// Bitset of the start positions of live objects.
	char object_map[BLOCKS_PER_CHUNK * sizeof(struct GcBlock)
		/ (alignof(max_align_t) * CHAR_BIT)];
	struct Chunk *next;
};

struct GcHeap {
	struct GcBlock *head, ///< The current block being allocated into.
		*overflow; ///< Block kept for writing medium objects.
	struct BumpPointer ptr, overflow_ptr;
	struct Vec free, recycled;
	struct Chunk *chunks;

	bool mark_color, inhibit_gc, defrag;
	struct Vec trace_stack;
};

static struct GcBlock *acquire_block(struct GcHeap *heap) {
	void *p;
	if (!(vec_reserve(&heap->free, BLOCKS_PER_CHUNK)
			&& vec_reserve(&heap->recycled, BLOCKS_PER_CHUNK))
		|| (p = mmap(NULL, sizeof(struct Chunk) + alignof(struct Chunk) - 1,
				PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0))
		== MAP_FAILED) return NULL;
	// Align to multiple of chunk size
	struct Chunk *chunk = (struct Chunk *)
		(((uintptr_t) p + alignof(struct Chunk) - 1) & ~(alignof(struct Chunk) - 1));
	munmap(p, (char *) chunk - (char *) p);
	struct GcBlock *blocks = chunk->blocks;
	for (struct GcBlock *block = blocks; block < blocks + BLOCKS_PER_CHUNK; ++block) {
		ASAN_POISON_MEMORY_REGION(block->data, sizeof block->data);
#ifndef __linux__
		memset(block->line_marks, 0, sizeof block->line_marks);
		blocks->flag = 0;
#endif
	}
	for (struct GcBlock *block = blocks + 1; block < blocks + BLOCKS_PER_CHUNK; ++block)
		vec_push(&heap->free, block);
#ifndef __linux__
	memset(chunk->object_map, 0, sizeof chunk->object_map);
#endif
	chunk->next = heap->chunks;
	heap->chunks = chunk;
	return blocks;
}

static struct BumpPointer empty_block_ptr(struct GcBlock *block) {
	return (struct BumpPointer) { block->data + sizeof block->data, block->data };
}

struct GcHeap *gc_new() {
	struct GcHeap *heap;
	if (!((heap = calloc(1, sizeof *heap)) && (heap->head = acquire_block(heap)))) {
		free(heap);
		return NULL;
	}
	heap->ptr = empty_block_ptr(heap->head);
	return heap;
}

/** Remember @a x as a live allocated object location. */
static void object_map_add(char *x) {
	struct Chunk *chunk = (struct Chunk *) ((uintptr_t) x & ~(alignof(struct Chunk) - 1));
	unsigned i = (x - (char *) chunk) / alignof(max_align_t);
	chunk->object_map[i / CHAR_BIT] |= 1 << i % CHAR_BIT;
}

/** Removes @a x from the object map, returning whether it was present. */
static bool object_map_remove(struct GcHeap *heap, uintptr_t x) {
	if (x % alignof(max_align_t)) return false;
	uintptr_t p = x & ~(alignof(struct Chunk) - 1);
	struct Chunk *chunk;
	for (chunk = heap->chunks; chunk; chunk = chunk->next)
		if ((uintptr_t) chunk == p) goto found;
	return false;
found:
	unsigned i = (x - p) / alignof(max_align_t);
	char *v = chunk->object_map + i / CHAR_BIT, mask = 1 << i % CHAR_BIT;
	if (*v & mask) { *v &= ~mask; return true; }
	return false;
}

enum {
	GC_MARK = 1, ///< Object mark bit: Ensures transitive closure terminates.
	GC_PINNED = 2,
	GC_FORWARDED = 4,
};

#define MIN_FREE (BLOCKS_PER_CHUNK / (100 / 3) + 3)

void *gc_alloc(struct GcHeap *heap, size_t size, struct GcTypeInfo *tib) {
	if ((size += sizeof(struct GcObjectHeader)) > sizeof heap->head->data) return NULL;
	char *p;
	struct GcBlock **block = &heap->head, *new_block;
	struct BumpPointer *ptr = &heap->ptr;
	if ((p = bump_alloc(ptr, alignof(max_align_t), size))) goto success;
	if (size <= GC_LINE_SIZE) {
		if ((*ptr = next_gap(*block, ptr->limit, size)).cursor) {
			p = bump_alloc(ptr, alignof(max_align_t), size);
			goto success;
		}
		if ((new_block = vec_pop(&heap->recycled))) {
			// Recycled blocks have gaps of >=1 lines; enough for a small obj
			*ptr = next_gap(*block = new_block, new_block->data + sizeof new_block->data, size);
			p = bump_alloc(ptr, alignof(max_align_t), size);
			goto success;
		}
	} else { // Demand-driven overflow allocation
		block = &heap->overflow;
		ptr = &heap->overflow_ptr;
		if (*block && (p = bump_alloc(ptr, alignof(max_align_t), size))) goto success;
	}
	// Acquire a free block
	if (!((new_block = vec_pop(&heap->free)) || (new_block = acquire_block(heap))))
		return NULL;
	*ptr = empty_block_ptr(*block = new_block);
	p = bump_alloc(ptr, alignof(max_align_t), size);
success:
	ASAN_UNPOISON_MEMORY_REGION(p, size);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-align"
#pragma GCC diagnostic ignored "-Wnull-dereference"
#pragma GCC diagnostic ignored "-Wanalyzer-null-dereference"
	*(struct GcObjectHeader *) p
		= (struct GcObjectHeader) { .flags = heap->mark_color, .tib = tib };
#pragma GCC diagnostic pop
	p += sizeof(struct GcObjectHeader);
	object_map_add(p);
	if (heap->free.length <= MIN_FREE && !heap->inhibit_gc) garbage_collect(heap);
	return p;
}

void gc_trace(struct GcHeap *heap, void **p) {
	struct GcObjectHeader *header = (struct GcObjectHeader *) *p - 1;
	if ((header->flags & GC_MARK) == heap->mark_color) { // Already traced
		if (header->flags & GC_FORWARDED) *p = header->fwd;
		return;
	}
	header->flags = heap->mark_color;

	// Opportunistic evacuation if block is marked as defrag candidate
	struct GcBlock *block = (struct GcBlock *) ((uintptr_t) header & ~(GC_BLOCK_SIZE - 1));
	size_t size;
	void *q;
	if (block->flag && !(header->flags & GC_PINNED)
		&& (q = gc_alloc(heap, size = header->tib->size(*p), header->tib))) {
		memcpy(q, *p, size);
		header->fwd = *p = q;
		header->flags |= GC_FORWARDED;
	} else object_map_add(*p);

	if (!vec_reserve(&heap->trace_stack, 1)) die("malloc failed");
	vec_push(&heap->trace_stack, *p);
}

static void pin_and_trace(struct GcHeap *heap, void *p) {
	struct GcObjectHeader *header = (struct GcObjectHeader *) p - 1;
	header->flags = heap->mark_color | GC_PINNED;
	object_map_add(p);
	header->tib->trace(heap, p);
}

/** Call @a fn with callee-saved registers pushed to the stack. */
#ifdef __clang__
[[clang::disable_tail_calls]]
#elifndef __GNUC__
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
	asm volatile ("" : : "X" (&ctx) : "memory");
#else
	gc_nop_sink = &ctx;
#endif
}

extern void *__libc_stack_end;
[[gnu::no_sanitize_address]] static void collect_roots(void *x) {
	struct GcHeap *heap = x;
	void *base = __libc_stack_end, *sp = __builtin_frame_address(0);
	sp = (void *) (((uintptr_t) sp + alignof(void *) - 1)
		& ~(alignof(void *) - 1)); // Round up to alignment
	for (uintptr_t *p = sp; p < (uintptr_t *) base; ++p)
		if (object_map_remove(heap, *p)) {
			if (!vec_reserve(&heap->trace_stack, 1)) die("malloc failed");
			vec_push(&heap->trace_stack, (void *) *p);
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

	for (struct Chunk *chunk = heap->chunks; chunk; chunk = chunk->next) {
		memset(chunk->object_map, 0, sizeof chunk->object_map); // Clear object map
		// Unmark blocks
		for (struct GcBlock *block = chunk->blocks; block < chunk->blocks + BLOCKS_PER_CHUNK; ++block)
			memset(block->line_marks, 0, sizeof block->line_marks);
	}

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
	size_t prev_num_free = heap->free.length;
	while (heap->trace_stack.length) { // Trace live objects
		void *p = heap->trace_stack.items[--heap->trace_stack.length];
		((struct GcObjectHeader *) p - 1)->tib->trace(heap, p);
	}

	heap->free.length = heap->recycled.length = 0;
	for (struct Chunk *chunk = heap->chunks; chunk; chunk = chunk->next)
		for (struct GcBlock *block = chunk->blocks; block < chunk->blocks + BLOCKS_PER_CHUNK; ++block) {
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
