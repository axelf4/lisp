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

#include "vec.h"

#define LINE_SIZE_BITS 7
#define LINE_SIZE (1 << LINE_SIZE_BITS) // 0x80
#define BLOCK_SIZE_BITS 15
#define BLOCK_SIZE (1 << BLOCK_SIZE_BITS) // 0x8000

#define LINE_COUNT (BLOCK_SIZE / LINE_SIZE - 1)
// One byte per line is used for flags
#define BLOCK_CAPACITY (BLOCK_SIZE - LINE_COUNT - 1)

#define BLOCKS_PER_CHUNK 8

enum BlockStatus {
	FREE,
	/// Partly used with at least F=1 free lines.
	RECYCLABLE,
	UNAVAILABLE,
};

struct BumpPointer {
	void *cursor, *limit;
};

/// Aligned to block boundary.
struct Block {
	char data[BLOCK_CAPACITY];
	struct BlockMeta {
		char lines[LINE_COUNT];
		enum BlockStatus status : 8;
	} meta;
};

static void *block_alloc(struct Block *block, struct BumpPointer *ptr, size_t size) {
	_Static_assert(!(LINE_SIZE & (alignof(max_align_t) - 1)));
	size_t align = alignof(max_align_t);

	if ((size_t) ((char *) ptr->cursor - (char *) ptr->limit) >= size)
		return (ptr->cursor = (void *) (((uintptr_t) ptr->cursor - size) & ~(align - 1)));

	// Locate next gap of unmarked lines of sufficient size
	unsigned required_lines = (size + LINE_SIZE - 1) / LINE_SIZE, count = 0,
		end = ((char *) block - (char *) ptr->limit) / LINE_SIZE;
	for (unsigned i = end; i-- > 0;) {
		bool marked = block->meta.lines[i];
		if (marked) {
			if (count > required_lines) {
				// At least 2 preceeding lines were unmarked. Consider
				// the previous block as conservatively marked.
				ptr->cursor = (char *) block + LINE_SIZE * end;
				ptr->limit = (char *) block + LINE_SIZE * (i + 2);
				return block_alloc(block, ptr, size);
			}

			count = 0;
			end = i;
		} else ++count;
	}

	if (count >= required_lines) {
		ptr->cursor = (char *) block + LINE_SIZE * end;
		ptr->limit = (char *) block;
		return block_alloc(block, ptr, size);
	}
	return NULL;
}

struct BlockList {
	struct Block *block;
	struct BlockList *next;
};

struct Chunk {
	struct Block *data;
	roaring_bitmap_t *object_map;
	struct Chunk *next;
};

struct Heap {
	struct BumpPointer ptr;

	struct Block *head, ///< The current block being allocated into.
		*overflow; ///< Block kept for writing medium objects.
	struct BlockList *free, *rest;
	struct Chunk *chunks;

	bool mark_color;
};

static struct Block *acquire_block(struct Heap *heap) {
	printf("Acquiring new block...\n");

	if (heap->free) {
		struct Block *block = heap->free->block;
		heap->free = heap->free->next;
		return block;
	}

	struct Block *blocks;
	if ((blocks = mmap(NULL, BLOCKS_PER_CHUNK * sizeof *blocks + sizeof *blocks - 1,
				PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0))
		== MAP_FAILED)
		return NULL;
	// Align to block boundary
	blocks = (void *) (((uintptr_t) blocks + sizeof *blocks) & ~(sizeof *blocks - 1));
	for (int i = 1; i < BLOCKS_PER_CHUNK; ++i) {
		struct BlockList *cell;
		if (!(cell = malloc(sizeof *cell))) break;
		cell->block = blocks + i;
		cell->next = heap->free;
		heap->free = cell;
	}

	struct Chunk *chunk;
	if (!(chunk = malloc(sizeof *chunk))) return NULL;
	chunk->data = blocks;
	chunk->object_map = roaring_bitmap_create();
	chunk->next = heap->chunks;
	heap->chunks = chunk;

	return blocks;
}

enum SizeClass { SMALL, MEDIUM, LARGE };

static enum SizeClass size_class(size_t size) {
	return size <= LINE_SIZE ? SMALL
		: size <= BLOCK_CAPACITY ? MEDIUM
		: LARGE;
}

static struct BumpPointer empty_block_ptr(struct Block *block) {
	return (struct BumpPointer) { (char *) block + BLOCK_CAPACITY, (void *) block };
}

static void *do_alloc(struct Heap *heap, size_t size) {
	switch (size_class(size)) {
	case SMALL:
		struct Block *block = heap->head;
		if (!block) {
			block = heap->head = acquire_block(heap);
			heap->ptr = empty_block_ptr(block);
		}

		void *result = block_alloc(block, &heap->ptr, size);
		if (result) return result;

		if (!(block = heap->head = acquire_block(heap))) return NULL;
		// Store head into block list
		struct BlockList *new_rest;
		if (!(new_rest = malloc(sizeof *new_rest))) return NULL;
		*new_rest = (struct BlockList) { block, heap->rest };
		heap->rest = new_rest;

		heap->ptr = empty_block_ptr(block);
		return block_alloc(block, &heap->ptr, size);
	default:
		printf("Error: Bad size class\n");
		return NULL;
	}
}

static void unmark_block(struct Block *block) {
	memset(block->meta.lines, 0, sizeof block->meta.lines);
}

static struct Heap heap;

void gc_init() {
	heap = (struct Heap) { 0 };
}

struct ObjectHeader {
	bool mark;
	struct GcTypeInfo *tib;
};

void *gc_alloc(size_t size) {
	char *ptr = do_alloc(&heap, sizeof(struct ObjectHeader) + size);
	if (!ptr) return NULL;
	struct ObjectHeader *header = (struct ObjectHeader *) ptr;
	ptr += sizeof(struct ObjectHeader);

	header->mark = !heap.mark_color;

	// Add to object map
	const size_t min_align = 8;
	struct Chunk *chunk = heap.chunks;
	while (chunk && !((char *) chunk->data <= ptr
			&& ptr < (char *) (chunk->data + BLOCKS_PER_CHUNK))) chunk = chunk->next;
	roaring_bitmap_add(chunk->object_map, (uintptr_t) (ptr - (char *) chunk->data) / min_align);

	return ptr;
}

void gc_trace(void *p) {
	printf("foo Tracing object at '%p'\n", p);
	struct ObjectHeader *header = (void *) ((uintptr_t) p - sizeof(struct ObjectHeader));
	if (header->mark == heap.mark_color) return;
	header->mark = !header->mark;

	printf("Tracing object at '%p'\n", p);
	if (header->tib) header->tib->trace(p); else gc_mark(p);
}

/** Mark the line that contains the given pointee. */
void gc_mark(void *p) {
	struct Block *block = (void *) ((uintptr_t) p & ~(BLOCK_SIZE - 1));
	unsigned line = ((char *) p - (char *) block) / LINE_SIZE;
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
static void push_regs(void *roots) {
	/* register uintptr_t sp asm ("sp"); */
	/* printf("SP: %p\n", (void *) sp); */

	void *base = __libc_stack_end,
		*sp = __builtin_frame_address(0);
	sp = (void *) (((uintptr_t) sp + alignof(void *) - 1)
		& ~(alignof(void *) - 1)); // Round up to alignment
	printf("SP: %p, base: %p\n", sp, base);
	for (uintptr_t *p = sp; p < (uintptr_t *) base; ++p) {
		uintptr_t x = *p;
		if (x % 8) continue;
		for (struct Chunk *chunk = heap.chunks; chunk; chunk = chunk->next) {
			if (x >= (uintptr_t) chunk->data
				&& roaring_bitmap_contains(chunk->object_map, (x - (uintptr_t) chunk->data) / 8)) {
				printf("adding object at '%p'\n", (void *) x);
				vec_push(roots, (void *) x);
				break;
			}
		}
	}
}

static enum BlockStatus sweep_block(struct Block *block) {
	unsigned unavailable_lines = 0;
	for (unsigned i = 0; i < LINE_COUNT; ++i)
		if (block->meta.lines[i]) ++unavailable_lines;
	return (block->meta.status = !unavailable_lines ? FREE
		: unavailable_lines < LINE_COUNT ? RECYCLABLE
		: UNAVAILABLE);
}

void garbage_collect() {
	if (heap.head) unmark_block(heap.head);
	for (struct Chunk *x = heap.chunks; x; x = x->next)
		for (unsigned i = 0; i < BLOCKS_PER_CHUNK; ++i)
			unmark_block(x->data + i);

	// Find conservative roots
	struct Vec roots = vec_new();
	with_callee_saves_pushed(push_regs, &roots);
	for (size_t i = 0; i < roots.length; ++i) {
		void *p = roots.items[i];
		gc_trace(p); // Mark conservative and pinned
	}
	vec_free(&roots);

	struct BlockList *prev = NULL;
	for (struct BlockList *x = heap.rest; x;) {
		enum BlockStatus status = sweep_block(x->block);

		if (status == FREE) {
			printf("Freeing a block...\n");
			*(prev ? &prev->next : &heap.rest) = x->next;
			struct BlockList *y = x;
			x = x->next;
			y->next = heap.free;
			heap.free = y;
		} else {
			prev = x;
			x = x->next;
		}
	}
}
