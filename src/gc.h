/** @file
 * Conservative single-threaded immix garbage collector.
 *
 * @see BLACKBURN, Stephen M.; MCKINLEY, Kathryn S. Immix: a
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

struct GcHeap;

/** Type information block (TIB). */
struct GcTypeInfo {
	void (*trace)(struct GcHeap *, void *);
	size_t (*size)(void *);
};

struct GcObjectHeader {
	alignas(max_align_t)
		char mark, ///< Object mark bit: Ensures transitive closure terminates.
		flags; ///< GC flags.
	union {
		struct GcTypeInfo *tib;
		void *fwd; ///< Forwarding pointer.
	};
};

struct GcBlock {
	alignas(GC_BLOCK_SIZE) char data[GC_LINE_SIZE * GC_LINE_COUNT];
	char line_marks[GC_LINE_COUNT];
	unsigned char flag;
};

[[gnu::malloc, nodiscard]] struct GcHeap *gc_new();

/** Allocates @a size bytes. */
[[gnu::alloc_size (2), gnu::assume_aligned (alignof(max_align_t)), gnu::hot, gnu::malloc, nodiscard]]
void *gc_alloc(struct GcHeap *heap, size_t size, struct GcTypeInfo *tib);

void gc_trace(struct GcHeap *heap, void **p);

/** Marks the lines containing the given pointee. */
static inline void gc_mark(size_t len, const char p[static len]) {
	const char *end = p + len;
	p -= sizeof(struct GcObjectHeader);
	struct GcBlock *block = (struct GcBlock *) ((uintptr_t) p & ~(GC_BLOCK_SIZE - 1));
	unsigned line = (p - (char *) block) / GC_LINE_SIZE;
	// The end of the object may extend into another line implicitly
	// due to conservative marking.
	do block->line_marks[line++] = 1; while ((p += GC_LINE_SIZE) < end);
}

[[gnu::noinline]] void garbage_collect(struct GcHeap *heap);

#endif
