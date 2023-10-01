/**
 * Conservative single-threaded immix garbage collector.
 *
 * See: BLACKBURN, Stephen M.; MCKINLEY, Kathryn S. Immix: a
 *      mark-region garbage collector with space efficiency, fast
 *      collection, and mutator performance. ACM SIGPLAN Notices,
 *      2008, 43.6: 22-32.
 */

#ifndef GC_H
#define GC_H

#include <stddef.h>
#include <stdint.h>

#define GC_LINE_SIZE 0x100
#define GC_BLOCK_SIZE 0x8000
#define GC_LINE_COUNT (GC_BLOCK_SIZE / GC_LINE_SIZE - 1)

struct Heap;

/** Type information block (TIB). */
struct GcTypeInfo {
	void (*trace)(struct Heap *, void *);
	size_t (*size)(void *);
};

struct GcObjectHeader {
	alignas(max_align_t)
		char mark, ///< Object mark bit: Ensures transitive closure terminates.
		flags; ///< GC flags.
	union {
		struct GcTypeInfo *tib;
		void *forwarding; ///< Forwarding pointer.
	};
};

struct GcBlock {
	alignas(GC_BLOCK_SIZE) char data[GC_LINE_SIZE * GC_LINE_COUNT];
	char line_marks[GC_LINE_COUNT];
	unsigned char flag;
};

[[gnu::malloc, nodiscard]] struct Heap *gc_new();

/** Allocate @arg size bytes. */
[[gnu::alloc_size (2), gnu::hot, nodiscard]]
void *gc_alloc(struct Heap *heap, size_t size, struct GcTypeInfo *tib);

[[gnu::hot]] void gc_trace(struct Heap *heap, void **p);

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

[[gnu::noinline]] void garbage_collect(struct Heap *heap);

#endif