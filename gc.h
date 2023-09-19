#ifndef GC_H
#define GC_H

#include <stddef.h>

/** Type information block (TIB). */
struct GcTypeInfo {
	/* size_t (*size)(void *); */
	void (*trace)(void *);
};

void gc_init();

void *myalloc(size_t size);

void garbage_collect();

void gc_noop1(void *);

#endif
