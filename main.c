#include <stdio.h>
#include "gc.h"

struct Cons {
	void *car, *cdr;
};

static size_t consSize(__attribute__ ((unused)) void *x) { return sizeof(struct Cons); }

static void consTrace(struct Heap *heap, void *x) {
	struct Cons *cons = x;
	gc_mark(x, sizeof *cons);
	if (cons->car) gc_trace(heap, &cons->car);
	if (cons->cdr) gc_trace(heap, &cons->cdr);
}

static struct GcTypeInfo consTib = { consSize, consTrace };

static struct Heap *heap;

static void *cons(void *car, void *cdr) {
	struct Cons *cell = gc_alloc(heap, sizeof(struct Cons), &consTib);
	cell->car = car;
	cell->cdr = cdr;
	return cell;
}

static void foo() {
	for (int i = 0; i < 2000; ++i) {
		__attribute__ ((unused)) void *p = cons(NULL, NULL);
		/* printf("foo alloc:ed: %p\n", p); */
	}
}

int main(void) {
	if (!(heap = gc_new())) return 1;

	void *p = cons(NULL, cons(NULL, NULL));
	printf("Allocated: %p\n", p);

	foo();
	printf("Collecting garbage...\n");
	garbage_collect(heap);

	printf("Again: %p\n", p);
}
