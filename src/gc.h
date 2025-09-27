/** @file
 * Generational conservative single-threaded immix garbage collector.
 *
 * During minor GCs, sticky mark bits curb the retracing of mature
 * objects. Hence, @ref gc_write_barrier() must be called upon
 * potential old-to-young edges.
 *
 * For disambiguating tagged pointers from SMIs the LSB is reserved,
 * and on 64-bit platforms pointer compression (inspired by V8, see
 * https://v8.dev/blog/pointer-compression) is used:
 *
 *                             32-bits      32-bits
 *                 Pointer: |____base____|___offset__1|
 *     SMall Integer (SMI): |************|___int31___0|
 *
 * @see BLACKBURN, Stephen M.; MCKINLEY, Kathryn S. Immix: a
 *      mark-region garbage collector with space efficiency, fast
 *      collection, and mutator performance. ACM SIGPLAN Notices,
 *      2008, 43.6: 22-32.
 */

#ifndef GC_H
#define GC_H

#include <stdint.h>
#include "util.h"

#define GC_LINE_SIZE 0x100
#define GC_BLOCK_SIZE 0x8000
#define GC_LINE_COUNT (GC_BLOCK_SIZE / GC_LINE_SIZE - 1)

#ifndef USE_COMPRESSED_PTRS
#define USE_COMPRESSED_PTRS __LP64__
#endif

struct GcRef {
#if USE_COMPRESSED_PTRS
	uint32_t
#else
	uintptr_t
#endif
	p;
};

#define GC_COMPRESS(p) (struct GcRef) { (uintptr_t) (p) }
#if USE_COMPRESSED_PTRS
#define GC_DECOMPRESS(base, ref) ((uintptr_t) ASSUME_ALIGNED(base, 1ull << 32) + (ref).p)
#else
#define GC_DECOMPRESS(base, ref) ((void) (base), (ref).p)
#endif

#define GC_ALIGNMENT (sizeof(struct GcRef))
#define GC_BLOCK(p) ((struct GcBlock *) ((uintptr_t) (p) & ~(GC_BLOCK_SIZE - 1)))

struct GcObjectHeader { unsigned char flags; };

struct GcBlock {
	alignas(GC_BLOCK_SIZE) char data[GC_LINE_SIZE * GC_LINE_COUNT];
	bool line_marks[GC_LINE_COUNT];
	unsigned char flag;
};

struct GcHeap;

[[nodiscard, gnu::malloc]] struct GcHeap *gc_new();

void gc_free(struct GcHeap *heap);

/** Allocates @a size bytes. */
[[nodiscard, gnu::alloc_align (2), gnu::alloc_size (3), gnu::malloc]]
void *gc_alloc(struct GcHeap *heap, size_t alignment, size_t size);

enum {
	GC_UNLOGGED = 4, ///< Object is not already remembered (nor in the nursery).
};
/** Remembers that a reference field of the object @a src was mutated.
 *
 * @see YANG, Xi, et al. Barriers reconsidered, friendlier still!. ACM
 *      SIGPLAN Notices, 2012, 47.11: 37-48.
 */
static inline void gc_write_barrier(struct GcHeap *heap, struct GcObjectHeader *src) {
	void gc_log_object(struct GcHeap *heap, struct GcObjectHeader *src);
	if (UNLIKELY(src->flags & GC_UNLOGGED)) gc_log_object(heap, src);
}

/** Traces the GC object @a p.
 *
 * @return The new address of @a p in case it moved.
 */
[[nodiscard]] void *gc_trace(struct GcHeap *heap, void *p);
void gc_pin(struct GcHeap *heap, void *p);

/** Marks the lines containing the given pointee. */
static inline void gc_mark(size_t len, const char p[static len]) {
	const char *end = p + len;
	struct GcBlock *block = GC_BLOCK(p);
	unsigned line = (p - block->data) / GC_LINE_SIZE;
	// The end of the object may extend into another line implicitly
	// due to conservative marking.
	do block->line_marks[line++] = 1; while ((p += GC_LINE_SIZE) < end);
}

void garbage_collect(struct GcHeap *heap);

/** @name Embedder API */ ///@{

void gc_object_visit(struct GcHeap *heap, void *p);

/** Gets the size and alignment of the GC object.
 *
 * @param p The GC object.
 * @param[out] alignment The minimum byte alignment.
 * @return The byte size.
 */
size_t gc_object_size(void *p, size_t *alignment);

/** Traces all explicit GC roots. */
void gc_trace_roots(struct GcHeap *heap);

///@}

#endif
