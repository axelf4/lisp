#ifndef GC_H
#define GC_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

#define GC_LINE_SIZE 0x80
#define GC_BLOCK_SIZE 0x8000
#define GC_LINE_COUNT (GC_BLOCK_SIZE / GC_LINE_SIZE - 1)
// One byte per line is used for flags
#define GC_BLOCK_CAPACITY (GC_BLOCK_SIZE - GC_LINE_COUNT - 1)

struct Heap;

/** Type information block (TIB). */
struct GcTypeInfo {
	size_t (*size)(void *);
	void (*trace)(struct Heap *, void *);
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

__attribute__ ((malloc, warn_unused_result)) struct Heap *gc_new();

__attribute__ ((alloc_size (2), hot, warn_unused_result))
void *gc_alloc(struct Heap *heap, size_t size, struct GcTypeInfo *tib);

__attribute__ ((hot)) void gc_trace(struct Heap *heap, void **p);

/** Mark the lines containing the given pointee. */
static inline void gc_mark(size_t len, const char p[static len]) {
	const char *end = p + len;
	p -= sizeof(struct GcObjectHeader);
	do {
		struct GcBlock *block = (struct GcBlock *) ((uintptr_t) p & ~(GC_BLOCK_SIZE - 1));
		unsigned line = (p - (char *) block) / GC_LINE_SIZE;
		block->line_marks[line] = 1;
	} while ((p += GC_LINE_SIZE) < end);
}

__attribute__ ((noinline)) void garbage_collect(struct Heap *heap);

#endif
