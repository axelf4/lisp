#ifndef GC_H
#define GC_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

#define GC_LINE_SIZE_BITS 7
#define GC_LINE_SIZE (1 << GC_LINE_SIZE_BITS) // 0x80
#define GC_BLOCK_SIZE_BITS 15
#define GC_BLOCK_SIZE (1 << GC_BLOCK_SIZE_BITS) // 0x8000

#define GC_LINE_COUNT (GC_BLOCK_SIZE / GC_LINE_SIZE - 1)
// One byte per line is used for flags
#define GC_BLOCK_CAPACITY (GC_BLOCK_SIZE - GC_LINE_COUNT - 1)

/** Type information block (TIB). */
struct GcTypeInfo {
	size_t (*size)(void *);
	void (*trace)(void *);
};

enum {
	GC_PINNED = 2,
	GC_FORWARDED = 4,
};

struct GcObjectHeader {
	char mark; ///< Object mark bit: Ensures transitive closure terminates.
	char flags; //< GC flags.
	union {
		struct GcTypeInfo *tib;
		void *forwarding; ///< Forwarding pointer.
	};
};

/// Aligned to block boundary.
struct GcBlock {
	char data[GC_BLOCK_CAPACITY];
	char line_marks[GC_LINE_COUNT];
	unsigned char flag;
};

bool gc_init();

void *gc_alloc(size_t size, struct GcTypeInfo *tib);

void gc_trace(void **p);

/** Mark the lines containing the given pointee. */
static inline void gc_mark(char *p, size_t size) {
	char *end = p + size;
	p -= sizeof(struct GcObjectHeader);
	do {
		struct GcBlock *block = (struct GcBlock *) ((uintptr_t) p & ~(GC_BLOCK_SIZE - 1));
		unsigned line = (p - (char *) block) / GC_LINE_SIZE;
		block->line_marks[line] = 1;
	} while ((p += GC_LINE_SIZE) < end);
}

void garbage_collect();

#endif
