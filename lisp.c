#include "lisp.h"

struct Heap *heap;

static size_t consSize(__attribute__ ((unused)) void *x) { return sizeof(struct Cons); }

static void consTrace(struct Heap *heap, void *x) {
	struct Cons *cons = x;
	gc_mark(x, sizeof *cons);
	if (cons->car) gc_trace(heap, (void **) &cons->car);
	if (cons->cdr) gc_trace(heap, (void **) &cons->cdr);
}

static struct GcTypeInfo consTib = { consSize, consTrace };

struct LispObject *cons(struct LispObject *car, struct LispObject *cdr) {
	struct Cons *cell = gc_alloc(heap, sizeof(struct Cons), &consTib);
	cell->object.tag = LISP_CONS;
	cell->car = car;
	cell->cdr = cdr;
	return &cell->object;
}
