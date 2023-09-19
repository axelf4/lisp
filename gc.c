#include "gc.h"
#include <stdint.h>
#include <stdbool.h>
#include <stdalign.h>
#include <assert.h>
#include <roaring/roaring.h>

#include <sys/mman.h>
#include <ucontext.h>

#include <stdlib.h>
#include <stdio.h>

#define LINE_SIZE_BITS 8
#define LINE_SIZE (1 << LINE_SIZE_BITS) // 2 * 0x80
#define BLOCK_SIZE_BITS 15
#define BLOCK_SIZE (1 << BLOCK_SIZE_BITS) // 0x8000

#define LINE_COUNT (BLOCK_SIZE / LINE_SIZE)
// One byte per line is used for flags
#define BLOCK_CAPACITY (BLOCK_SIZE - LINE_COUNT)

enum BlockStatus {
	FREE,
	/// Partly used with at least F=1 free lines.
	RECYCLABLE,
	UNAVAILABLE,
};

struct BumpPointer {
	void *cursor, *limit;
};

struct Block {
	void *cursor, *limit; // Bump pointer cursor and limit.
	union {
		void *ptr;
		/// Aligned to block boundary.
		struct BlockData {
			char data[BLOCK_CAPACITY];
			struct BlockMeta {
				char lines[LINE_COUNT];
			} meta;
		} *block;
	};
};

/**
 * Locate a gap of unmarked lines of sufficient size.
 */
static struct BumpPointer next_hole(struct Block block, size_t size) {
	unsigned required_lines = (size + (LINE_SIZE - 1)) / LINE_SIZE, count = 0,
		end = ((uintptr_t) block.limit) / LINE_SIZE;
	for (unsigned i = end; i-- > 0;) {
		bool marked = block.block->meta.lines[i];

		if (marked) {
			if (count > required_lines) {
				// At least 2 preceeding lines were unmarked. Consider
				// the preceeding block as conservatively marked.
				return (struct BumpPointer) {
					block.ptr + LINE_SIZE * end,
					block.ptr + LINE_SIZE * (i + 2) 
				};
			}

			count = 0;
			end = i;
		} else
			++count;
	}

	return count >= required_lines
		? (struct BumpPointer) { block.ptr + LINE_SIZE * end, block.ptr }
		: (struct BumpPointer) { NULL, NULL };
}

static void *block_alloc(struct Block *block, size_t size) {
	_Static_assert(!(LINE_SIZE & (alignof(max_align_t) - 1)));
	size_t align = alignof(max_align_t);

	if (block->cursor - block->limit >= size)
		return (block->cursor = (void *)
			((uintptr_t) block->cursor - size & ~(align - 1)));

	struct BumpPointer gap = next_hole(*block, size);
	if (!gap.cursor) return NULL;
	block->cursor = gap.cursor;
	block->limit = gap.limit;
	return block_alloc(block, size);
}

struct BlockList {
	struct Block block;
	struct BlockList *next;
};

struct Heap {
	struct Block head, ///< The current block being allocated into.
		overflow; ///< Block kept for writing medium objects.
	struct BlockList *free, *rest;

	roaring_bitmap_t *object_map;
};

static struct Block block_init(char data[static BLOCK_SIZE]) {
	return (struct Block) {
		.cursor = data + BLOCK_CAPACITY, .limit = data, .ptr = data,
	};
}

static struct Block get_block(struct Heap *heap) {
	if (heap->free) {
		struct Block block = heap->free->block;
		heap->free = heap->free->next;
		return block;
	}

	const int chunk_size = 8;
	void *data;
	if ((data = mmap(NULL, chunk_size * BLOCK_SIZE + BLOCK_SIZE - 1,
				PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0))
		== MAP_FAILED)
		return (struct Block) {};
	// Align to block boundary
	data = (void *) ((uintptr_t) data + BLOCK_SIZE & ~(BLOCK_SIZE - 1));
	for (int i = 1; i < chunk_size; ++i) {
		struct BlockList *cell;
		if (!(cell = malloc(sizeof *cell))) break;
		cell->block = block_init(data + BLOCK_SIZE * i);
		cell->next = heap->free;
		heap->free = cell;
	}
	return block_init(data);
}

enum SizeClass { SMALL, MEDIUM, LARGE };

static enum SizeClass size_class(size_t size) {
	return size <= LINE_SIZE ? SMALL
		: size <= BLOCK_CAPACITY ? MEDIUM
		: LARGE;
}

static void *find_block(struct Heap *heap, size_t size) {
	switch (size_class(size)) {
	case SMALL:
		struct Block *block = &heap->head;
		printf("find_blocK: %p\n", block->ptr);
		if (!block->ptr) heap->head = get_block(heap);

		void *result = block_alloc(block, size);
		if (result) return result;

		// Store head into block list
		struct BlockList *new_rest;
		if (!(new_rest = malloc(sizeof *new_rest))) return NULL;
		*new_rest = (struct BlockList) { *block, heap->rest };
		heap->rest = new_rest;

		heap->head = get_block(heap);
		return block_alloc(block, size);
	default:
		printf("Error: Bad size class\n");
		return NULL;
	}
}

static struct Heap heap;

void gc_init() {
	heap = (struct Heap) {
		.object_map = roaring_bitmap_create(),
	};
}

struct ObjectHeader {
	bool mark;
	struct GcTypeInfo *tib;
};

void *myalloc(size_t size) {
	void *ptr = find_block(&heap, sizeof(struct ObjectHeader) + size);
	if (!ptr) return NULL;
	struct ObjectHeader *header = ptr;
	ptr += sizeof(struct ObjectHeader);

	header->mark = 0;

	size_t min_align = 8;
	roaring_bitmap_add(heap.object_map, (uintptr_t) ptr / min_align);

	return ptr;
}

volatile void *gc_noop_sink;
void gc_noop1(void *x) { gc_noop_sink = x; }

/** Call the function with callee-saved registers pushed to the stack. */
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
static void push_regs(void *_) {
	/* register uintptr_t sp asm ("sp"); */
	/* printf("SP: %p\n", (void *) sp); */

	void *base = __libc_stack_end,
		*sp = __builtin_frame_address(0);
	sp = (void *) ((uintptr_t) sp + (alignof(void *) - 1)
		& ~(alignof(void *) - 1)); // Round up to alignment
	printf("SP: %p, base: %p\n", sp, base);
	for (uintptr_t *p = sp, x; p < (uintptr_t *) base; ++p, x = *p)
		if (roaring_bitmap_contains(heap.object_map, x / 8)) {
			printf("Found object %p at: %p\n", (void *) x, p);
		}
}

void garbage_collect() {
	with_callee_saves_pushed(push_regs, NULL);
}
