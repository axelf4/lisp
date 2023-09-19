#include "gc.h"
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdalign.h>
#include <assert.h>

#define LINE_SIZE_BITS 7
#define LINE_SIZE (1 << LINE_SIZE_BITS) // 0x80
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

struct Block {
	void *cursor, *limit; // Bump pointer cursor and limit.
	union {
		void *ptr;
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
static struct Gap {
	void *cursor, *limit;
} next_hole(struct Block *block, size_t size) {
	unsigned required_lines = (size + (LINE_SIZE - 1)) / LINE_SIZE, count = 0,
		end = ((uintptr_t) block->limit) / LINE_SIZE;
	for (unsigned i = end; i-- > 0;) {
		bool marked = block->meta.lines[i];

		if (marked) {
			if (count > required_lines) {
				// At least 2 preceeding lines were unmarked. Consider
				// the preceeding block as conservatively marked.
				return (struct Gap) {
					&block->block + LINE_SIZE * end,
					&block->block + LINE_SIZE * (i + 2) 
				};
			}

			count = 0;
			end = i;
		} else
			++count;
	}

	return count >= required_lines
		? (struct Gap) { &block->block + LINE_SIZE * end, &block->block }
		: (struct Gap) { NULL, NULL };
}

static void *block_alloc(struct Block *block, size_t size) {
	_Static_assert(!(LINE_SIZE & (alignof(max_align_t) - 1)));
	size_t align = alignof(max_align_t);

	if (block->cursor - block->limit >= size)
		return (block->cursor = (void *)
			((uintptr_t) block->cursor - size & ~(align - 1)));

	struct Gap gap = next_hole(block, size);
	if (!gap.cursor) return NULL;

	block->cursor = gap.cursor;
	block->limit = gap.limit;
	return block_alloc(block, size);
}

struct BlockList {
	head
};
