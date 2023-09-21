#include "gc.h"
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdalign.h>
#include <assert.h>
#include <roaring/roaring.h>

#include <sys/mman.h>
#include <ucontext.h>

#include <stdio.h>

#include "vec.h"

#define LINE_SIZE_BITS 7
#define LINE_SIZE (1 << LINE_SIZE_BITS) // 0x80
#define BLOCK_SIZE_BITS 15
#define BLOCK_SIZE (1 << BLOCK_SIZE_BITS) // 0x8000

#define LINE_COUNT (BLOCK_SIZE / LINE_SIZE - 1)
// One byte per line is used for flags
#define BLOCK_CAPACITY (BLOCK_SIZE - LINE_COUNT - 1)

#define BLOCKS_PER_CHUNK 8

struct BumpPointer {
	char *cursor, *limit;
};

static void *bump_alloc(struct BumpPointer *ptr, size_t size, size_t align) {
	return (size_t) (ptr->cursor - ptr->limit) >= size
		? ptr->cursor = (char *) ((uintptr_t) (ptr->cursor - size) & ~(align - 1))
		: NULL;
}

enum BlockStatus {
	FREE,
	/// Partly used with at least F=1 free lines.
	RECYCLABLE,
	UNAVAILABLE,
};

/// Aligned to block boundary.
struct Block {
	char data[BLOCK_CAPACITY];
	struct BlockMeta {
		char lines[LINE_COUNT];
		enum BlockStatus status : 8;
	} meta;
};

_Static_assert(sizeof(struct Block) == BLOCK_SIZE);

static void *block_alloc(struct Block *block, struct BumpPointer *ptr, size_t size) {
	_Static_assert(!(LINE_SIZE & (alignof(max_align_t) - 1)));
	size_t align = alignof(max_align_t);

	void *result;
	if ((result = bump_alloc(ptr, size, align))) return result;

	// Locate next gap of unmarked lines of sufficient size
	unsigned required_lines = (size + LINE_SIZE - 1) / LINE_SIZE, count = 0,
		end = ((char *) block - (char *) ptr->limit) / LINE_SIZE;
	for (unsigned i = end; i-- > 0;) {
		bool marked = block->meta.lines[i];
		if (marked) {
			if (count > required_lines) {
				// At least 2 preceeding lines were unmarked. Consider
				// the previous block as conservatively marked.
				ptr->cursor = block->data + LINE_SIZE * end;
				ptr->limit = block->data + LINE_SIZE * (i + 2);
				return block_alloc(block, ptr, size);
			}

			count = 0;
			end = i;
		} else ++count;
	}

	if (count >= required_lines) {
		*ptr = (struct BumpPointer) { block->data + LINE_SIZE * end, block->data };
		return bump_alloc(ptr, size, align);
	}
	return NULL;
}

struct BlockList {
	struct Block *block;
	struct BlockList *next;
};

struct Chunk {
	struct Block *data;
	roaring_bitmap_t object_map;
	struct Chunk *next;
};

static struct Heap {
	struct BumpPointer ptr, overflow_ptr;

	struct Block *head, ///< The current block being allocated into.
		*overflow; ///< Block kept for writing medium objects.
	struct BlockList *free, *recycled, *rest;
	struct Chunk *chunks;

	bool mark_color;
} heap;

void gc_init() {}

static struct Block *acquire_block(struct Heap *heap) {
	printf("Acquiring a new block...\n");

	if (heap->free) {
		struct Block *block = heap->free->block;
		heap->free = heap->free->next;
		return block;
	}

	struct Block *blocks;
	if ((blocks = mmap(NULL, (BLOCKS_PER_CHUNK + 1) * sizeof *blocks - 1,
				PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0))
		== MAP_FAILED)
		return NULL;
	// Align to block boundary
	blocks = (void *) ((uintptr_t) (blocks + 1) & ~(sizeof *blocks - 1));
	for (unsigned i = 1; i < BLOCKS_PER_CHUNK; ++i) {
		struct BlockList *cell;
		if (!(cell = malloc(sizeof *cell))) break;
		cell->block = blocks + i;
		cell->next = heap->free;
		heap->free = cell;
	}

	struct Chunk *chunk;
	if (!(chunk = malloc(sizeof *chunk))) return NULL;
	chunk->data = blocks;
	roaring_bitmap_init_cleared(&chunk->object_map);
	chunk->next = heap->chunks;
	heap->chunks = chunk;

	return blocks;
}

static struct BumpPointer empty_block_ptr(struct Block *block) {
	return (struct BumpPointer) { block->data + sizeof block->data, block->data };
}

/** Remember @arg x as a live allocated object location. */
static void object_map_add(char *x) {
	struct Chunk *chunk = heap.chunks;
	while (!((char *) chunk->data <= x && x < (char *) (chunk->data + BLOCKS_PER_CHUNK)))
		chunk = chunk->next;
	roaring_bitmap_add(&chunk->object_map,
		(x - (char *) chunk->data) / alignof(void *));
}

struct ObjectHeader {
	bool mark; //< GC flags.
	union {
		struct GcTypeInfo *tib;
		void *forwarding_ptr;
	};
};

enum SizeClass { SMALL, MEDIUM, LARGE };

static enum SizeClass size_class(size_t size) {
	return size <= LINE_SIZE ? SMALL
		: size <= BLOCK_CAPACITY ? MEDIUM
		: LARGE;
}

void *gc_alloc(size_t size, struct GcTypeInfo *tib) {
	char *p;
	enum SizeClass class = size_class(size += sizeof(struct ObjectHeader));
	switch (class) {
	case SMALL: case MEDIUM:
		if (!heap.head)
			heap.ptr = empty_block_ptr(heap.head = acquire_block(&heap));
		struct Block **block = &heap.head;
		struct BumpPointer *ptr = &heap.ptr;
		if (class == SMALL) {
			if ((p = block_alloc(*block, ptr, size))) break;
			if (heap.recycled) {
				*ptr = empty_block_ptr(*block = heap.recycled->block);
				heap.recycled = heap.recycled->next;
				// Recycled blocks have gaps of >=1 lines; enough for a small obj
				p = block_alloc(*block, ptr, size);
				break;
			}
		} else {
			if ((p = bump_alloc(ptr, size, alignof(max_align_t)))) break;
			// Demand-driven overflow allocation
			block = &heap.overflow;
			ptr = &heap.overflow_ptr;
			if (block && (p = bump_alloc(ptr, size, alignof(max_align_t)))) break;
		}

		// Acquire a free block
		struct BlockList *new_rest;
		if (!(new_rest = malloc(sizeof *new_rest))) return NULL;
		*new_rest = (struct BlockList) { *block, heap.rest };
		heap.rest = new_rest;
		if (!(*block = acquire_block(&heap))) return NULL;
		*ptr = empty_block_ptr(*block);

		p = bump_alloc(ptr, size, alignof(max_align_t));
		break;
	case LARGE: printf("TODO: Large Object Space\n"); return NULL;
	}

	*(struct ObjectHeader *) p = (struct ObjectHeader) {
		.mark = heap.mark_color, .tib = tib,
	};
	p += sizeof(struct ObjectHeader);
	object_map_add(p);
	return p;
}

void gc_trace(void *p) {
	struct ObjectHeader *header = (struct ObjectHeader *) p - 1;
	if (header->mark == heap.mark_color) return; // Already traced
	header->mark = !header->mark;

	printf("Tracing object at '%p'\n", p);
	object_map_add(p);
	header->tib->trace(p);
}

/** Mark the line that contains the given pointee. */
void gc_mark(char *p) {
	p -= sizeof(struct ObjectHeader);
	struct Block *block = (struct Block *) ((uintptr_t) p & ~(BLOCK_SIZE - 1));
	unsigned line = (p - (char *) block) / LINE_SIZE;
	block->meta.lines[line] = 1;
}

volatile void *gc_noop_sink;
void gc_noop1(void *x) { gc_noop_sink = x; }

/** Call @arg fn with callee-saved registers pushed to the stack. */
static void with_callee_saves_pushed(void (*fn)(void *), void *arg) {
	ucontext_t ctx;
	if (getcontext(&ctx) < 0) {
		printf("getcontext() failed\n");
		__builtin_unwind_init();
	}
	fn(arg);
	// Inhibit tail-call which would pop register contents prematurely
	gc_noop1(&ctx);
}

extern void *__libc_stack_end;
static void collect_roots(void *roots) {
	// register void *sp asm ("sp");
	void *base = __libc_stack_end,
		*sp = __builtin_frame_address(0);
	sp = (void *) (((uintptr_t) sp + alignof(void *) - 1)
		& ~(alignof(void *) - 1)); // Round up to alignment
	printf("SP: %p, base: %p\n", sp, base);
	for (uintptr_t *p = sp; p < (uintptr_t *) base; ++p) {
		uintptr_t x = *p;
		if (x % alignof(void *)) continue;
		for (struct Chunk *chunk = heap.chunks; chunk; chunk = chunk->next)
			if (x >= (uintptr_t) chunk->data && x < (uintptr_t) (chunk->data + BLOCKS_PER_CHUNK)
				&& roaring_bitmap_remove_checked(&chunk->object_map,
					(x - (uintptr_t) chunk->data) / alignof(void *))) {
				vec_push(roots, (void *) x);
				break;
			}
	}
}

static void unmark_block(struct Block *block) {
	memset(block->meta.lines, heap.mark_color, sizeof block->meta.lines);
}

static enum BlockStatus sweep_block(struct Block *block) {
	unsigned unavailable_lines = 0;
	for (unsigned i = 0; i < LINE_COUNT; ++i)
		if (block->meta.lines[i] == heap.mark_color) ++unavailable_lines;
	return block->meta.status = (!unavailable_lines ? FREE
		: unavailable_lines < LINE_COUNT ? RECYCLABLE
		: UNAVAILABLE);
}

__attribute__ ((noinline)) void garbage_collect() {
	// Collect conservative roots
	struct Vec roots = vec_new();
	with_callee_saves_pushed(collect_roots, &roots);
	// Empty object map
	for (struct Chunk *chunk = heap.chunks; chunk; chunk = chunk->next)
		roaring_bitmap_clear(&chunk->object_map);
	// Unmark blocks
	for (struct Chunk *x = heap.chunks; x; x = x->next)
		for (unsigned i = 0; i < BLOCKS_PER_CHUNK; ++i)
			unmark_block(x->data + i);

	heap.mark_color = !heap.mark_color;
	// Trace live objects
	for (size_t i = 0; i < roots.length; ++i)
		gc_trace(roots.items[i]); // Mark conservative and pinned
	vec_free(&roots);

	struct BlockList *prev = NULL;
	for (struct BlockList *x = heap.rest; x;) {
		enum BlockStatus status = sweep_block(x->block);
		if (status == UNAVAILABLE) {
			prev = x;
			x = x->next;
		} else {
			*(prev ? &prev->next : &heap.rest) = x->next;
			struct BlockList *y = x;
			x = x->next;
			if (status == RECYCLABLE) {
				printf("Recycling a block...\n");
				y->next = heap.recycled;
				heap.recycled = y;
			} else {
				printf("Freeing a block...\n");
				y->next = heap.free;
				heap.free = y;
			}
		}
	}
}
