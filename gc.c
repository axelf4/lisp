#include "gc.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdalign.h>
#include <roaring/roaring.h>

#include <sys/mman.h>
#include <ucontext.h>

#include "vec.h"

#define LINE_SIZE_BITS 7
#define LINE_SIZE (1 << LINE_SIZE_BITS) // 0x80
#define BLOCK_SIZE_BITS 15
#define BLOCK_SIZE (1 << BLOCK_SIZE_BITS) // 0x8000

#define LINE_COUNT (BLOCK_SIZE / LINE_SIZE - 1)
// One byte per line is used for flags
#define BLOCK_CAPACITY (BLOCK_SIZE - LINE_COUNT - 1)

#define BLOCKS_PER_CHUNK 8 // 128

struct BumpPointer {
	char *cursor, *limit;
};

static void *bump_alloc(struct BumpPointer *ptr, size_t size, size_t align) {
	return (size_t) (ptr->cursor - ptr->limit) >= size
		? ptr->cursor = (char *) ((uintptr_t) (ptr->cursor - size) & ~(align - 1))
		: NULL;
}

_Static_assert(sizeof(struct GcBlock) == BLOCK_SIZE);

_Static_assert(!(LINE_SIZE % alignof(max_align_t)));
/** Locate next gap of unmarked lines of sufficient size. */
static struct BumpPointer next_gap(struct GcBlock *block, char *top, size_t size) {
	unsigned required_lines = (size + LINE_SIZE - 1) / LINE_SIZE, count = 0,
		end = ((char *) block - top) / LINE_SIZE;
	for (unsigned i = end; i-- > 0;)
		if (block->line_marks[i]) {
			if (count > required_lines) {
				// At least 2 preceeding lines were unmarked. Consider
				// the previous block as conservatively marked.
				return (struct BumpPointer) {
					block->data + LINE_SIZE * end,
					block->data + LINE_SIZE * (i + 2),
				};
			}
			count = 0;
			end = i;
		} else ++count;
	return count >= required_lines
		? (struct BumpPointer) { block->data + LINE_SIZE * end, block->data }
		: (struct BumpPointer) { NULL, NULL };
}

struct GcBlockList {
	struct GcBlock *block;
	struct GcBlockList *next;
};

struct Chunk {
	struct GcBlock *data;
	roaring_bitmap_t object_map;
	struct Chunk *next;
};

#define MIN_FREE (BLOCKS_PER_CHUNK / 35 + 2)

static struct Heap {
	struct BumpPointer ptr, overflow_ptr;

	struct GcBlock *head, ///< The current block being allocated into.
		*overflow; ///< Block kept for writing medium objects.
	struct GcBlockList *free, *recycled, *rest;
	struct Chunk *chunks;
	size_t num_free;

	bool mark_color, is_gc, defrag;
} heap;

static struct GcBlock *acquire_block(struct Heap *heap) {
	printf("Acquiring a new block...\n");
	if (heap->free) {
		struct GcBlockList *x = heap->free;
		struct GcBlock *block = x->block;
		heap->free = x->next;
		free(x);
		return block;
	}

	struct GcBlock *blocks;
	if ((blocks = mmap(NULL, (BLOCKS_PER_CHUNK + 1) * sizeof *blocks - 1,
				PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0))
		== MAP_FAILED)
		return NULL;
	// Align to block boundary
	blocks = (void *) ((uintptr_t) (blocks + 1) & ~(sizeof *blocks - 1));
	for (unsigned i = 1; i < BLOCKS_PER_CHUNK; ++i) {
		struct GcBlockList *cell;
		if (!(cell = malloc(sizeof *cell))) break;
		cell->block = blocks + i;
		cell->next = heap->free;
		heap->free = cell;
		++heap->num_free;
#ifndef __linux__
		memset(blocks[i].line_marks, 0, sizeof blocks[i].line_marks);
#endif
	}

	struct Chunk *chunk;
	if (!(chunk = malloc(sizeof *chunk))) return NULL;
	chunk->data = blocks;
	roaring_bitmap_init_cleared(&chunk->object_map);
	chunk->next = heap->chunks;
	heap->chunks = chunk;
#ifndef __linux__
	memset(blocks->line_marks, 0, sizeof blocks->line_marks);
#endif

	return blocks;
}

static struct BumpPointer empty_block_ptr(struct GcBlock *block) {
	return (struct BumpPointer) { block->data + sizeof block->data, block->data };
}

bool gc_init() {
	if (!(heap.head = acquire_block(&heap))) return false;
	heap.ptr = empty_block_ptr(heap.head);
	return true;
}

/** Remember @arg x as a live allocated object location. */
static void gc_object_map_add(char *x) {
	struct Chunk *chunk = heap.chunks;
	while (!((char *) chunk->data <= x && x < (char *) (chunk->data + BLOCKS_PER_CHUNK)))
		chunk = chunk->next;
	roaring_bitmap_add(&chunk->object_map,
		(x - (char *) chunk->data) / alignof(void *));
}

void *gc_alloc(size_t size, struct GcTypeInfo *tib) {
	if ((size += sizeof(struct GcObjectHeader)) > BLOCK_CAPACITY) return NULL;
	char *p;
	struct GcBlock **block = &heap.head;
	struct BumpPointer *ptr = &heap.ptr;
	if ((p = bump_alloc(ptr, size, alignof(max_align_t)))) goto success;
	if (size <= LINE_SIZE) {
		if ((*ptr = next_gap(*block, ptr->limit, size)).cursor) {
			p = bump_alloc(ptr, size, alignof(max_align_t));
			goto success;
		}
		if (heap.recycled) {
			*block = heap.recycled->block;
			heap.recycled = heap.recycled->next;
			// Recycled blocks have gaps of >=1 lines; enough for a small obj
			*ptr = next_gap(*block, (*block)->data + BLOCK_CAPACITY, size);
			p = bump_alloc(ptr, size, alignof(max_align_t));
			goto success;
		}
	} else {
		// Demand-driven overflow allocation
		block = &heap.overflow;
		ptr = &heap.overflow_ptr;
		if (block && (p = bump_alloc(ptr, size, alignof(max_align_t)))) goto success;
	}

	// Acquire a free block
	struct GcBlockList *new_rest;
	if (!(new_rest = malloc(sizeof *new_rest))) return NULL;
	*new_rest = (struct GcBlockList) { *block, heap.rest };
	heap.rest = new_rest;
	if (!(*block = acquire_block(&heap))) return NULL;
	*ptr = empty_block_ptr(*block);

	p = bump_alloc(ptr, size, alignof(max_align_t));
success:
	*(struct GcObjectHeader *) p = (struct GcObjectHeader) {
		.mark = heap.mark_color, .tib = tib,
	};
	p += sizeof(struct GcObjectHeader);
	gc_object_map_add(p);
	if (heap.num_free <= MIN_FREE && !heap.is_gc) garbage_collect();
	return p;
}

void gc_trace(void **p) {
	struct GcObjectHeader *header = (struct GcObjectHeader *) *p - 1;
	if (header->mark == heap.mark_color) { // Already traced
		if (header->flags & GC_FORWARDED) *p = header->forwarding;
		return;
	}
	printf("Tracing object at '%p'\n", *p);
	header->mark = heap.mark_color;

	// Opportunistic evacuation if block is marked as defrag candidate
	struct GcBlock *block = (struct GcBlock *)
		((uintptr_t) ((struct GcObjectHeader *) *p - 1) & ~(GC_BLOCK_SIZE - 1));
	bool should_evacuate = !block->flag;
	size_t size;
	void *q;
	if (heap.defrag && !(header->flags & GC_PINNED) && should_evacuate
		&& (q = gc_alloc(size = header->tib->size(*p), header->tib))) {
		printf("Evacuating %p...\n", *p);
		((struct GcObjectHeader *) q - 1)->flags |= GC_FORWARDED;
		memcpy(q, *p, size);
		*p = q;
	}

	gc_object_map_add(*p);
	header->tib->trace(*p);
}

static void gc_pin_and_trace(void *p) {
	struct GcObjectHeader *header = (struct GcObjectHeader *) p - 1;
	header->flags = GC_PINNED;
	gc_trace(&p);
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

enum BlockStatus {
	FREE,
	/// Partly used with at least F=1 free lines.
	RECYCLABLE,
	UNAVAILABLE,
};

static enum BlockStatus sweep_block(struct GcBlock *block) {
	unsigned unavailable_lines = 0;
	for (unsigned i = 0; i < LINE_COUNT; ++i)
		if (block->line_marks[i]) ++unavailable_lines;
	return !unavailable_lines ? FREE
		: unavailable_lines < LINE_COUNT ? RECYCLABLE
		: UNAVAILABLE;
}

static struct BlockStats {
	unsigned num_marks, num_holes;
} block_stats(struct GcBlock *block) {
	struct BlockStats result = { 0 };
	for (unsigned i = 0; i < LINE_COUNT; ++i) {
		while (i < LINE_COUNT && block->line_marks[i]) ++i, ++result.num_marks;
		if (i < LINE_COUNT) ++result.num_holes;
		while (i < LINE_COUNT && !block->line_marks[i]) ++i;
	}
	return result;
}

__attribute__ ((noinline)) void garbage_collect() {
	heap.is_gc = true;
	size_t prev_num_free = heap.num_free;

	// Collect conservative roots
	struct Vec roots = vec_new();
	with_callee_saves_pushed(collect_roots, &roots);
	// Empty object map
	for (struct Chunk *chunk = heap.chunks; chunk; chunk = chunk->next)
		roaring_bitmap_clear(&chunk->object_map);

#define MAX_HOLES ((LINE_COUNT + 1) / 2)
	unsigned mark_histogram[MAX_HOLES] = { 0 };
	// Unmark blocks
	for (struct Chunk *x = heap.chunks; x; x = x->next)
		for (unsigned i = 0; i < BLOCKS_PER_CHUNK; ++i) {
			struct GcBlock *block = x->data + i;
			if (heap.defrag) {
				struct BlockStats stats = block_stats(block);
				mark_histogram[stats.num_holes] += stats.num_marks;
				block->flag = stats.num_holes + 1;
			}
			memset(block->line_marks, 0, sizeof block->line_marks);
		}

	if (heap.defrag) {
		printf("Defragmenting...");
		ssize_t available_space = BLOCK_CAPACITY * heap.num_free;
		unsigned bin = MAX_HOLES;
		do available_space -= LINE_SIZE * mark_histogram[--bin];
		while (available_space > 0 && bin);

		for (struct GcBlockList *x = heap.rest; x; x = x->next) {
			struct GcBlock *block = x->block;
			bool is_defrag_candidate = (unsigned) block->flag - 1 > bin;
			if (is_defrag_candidate) block->flag = 0;
		}
		struct GcBlockList *prev = NULL;
		for (struct GcBlockList *x = heap.recycled; x;) {
			struct GcBlock *block = x->block;
			bool is_defrag_candidate = (unsigned) block->flag - 1 > bin;
			if (is_defrag_candidate) {
				block->flag = 0;
				struct GcBlockList *y = x;
				x = x->next;
				// Remove from recycled list so it will not be used for allocation
				*(prev ? &prev->next : &heap.recycled) = y->next;
				y->next = heap.rest;
				heap.rest = y;
			} else {
				prev = x;
				x = x->next;
			}
		}
	}

	// Zeroing object marks is impossible since their locations are unknown
	heap.mark_color = !heap.mark_color;
	// Trace live objects
	for (size_t i = 0; i < roots.length; ++i)
		// Pin to not "evacuate" a false positive root
		gc_pin_and_trace(roots.items[i]); 
	vec_free(&roots);

	struct GcBlockList *prev = NULL;
	for (struct GcBlockList *x = heap.rest; x;) {
		enum BlockStatus status = sweep_block(x->block);
		if (status == UNAVAILABLE) {
			prev = x;
			x = x->next;
		} else {
			*(prev ? &prev->next : &heap.rest) = x->next;
			struct GcBlockList *y = x;
			x = x->next;
			if (status == RECYCLABLE) {
				printf("Recycling a block...\n");
				y->next = heap.recycled;
				heap.recycled = y;
			} else {
				printf("Freeing a block...\n");
				y->next = heap.free;
				heap.free = y;
				++heap.num_free;
			}
		}
	}

	heap.defrag = heap.num_free <= prev_num_free;
	heap.is_gc = false;
}
