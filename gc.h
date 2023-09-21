#ifndef GC_H
#define GC_H

#include <stddef.h>

/** Type information block (TIB). */
struct GcTypeInfo {
	/* size_t (*size)(void *); */
	void (*trace)(void *);
};

void gc_init();

void *gc_alloc(size_t size, struct GcTypeInfo *tib);

void gc_trace(void *p);

void gc_mark(char *);

void garbage_collect();

#endif
