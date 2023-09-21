#include <stdio.h>
#include "gc.h"

struct Cons {
	void *car, *cdr;
};

static void consTrace(void *x) {
	gc_mark(x);
	struct Cons *cons = x;
	if (cons->car) gc_trace(cons->car);
	if (cons->cdr) gc_trace(cons->cdr);
}

static struct GcTypeInfo consTib = {
	.trace = consTrace,
};

static void *cons(void *car, void *cdr) {
	struct Cons *cell = gc_alloc(sizeof(struct Cons), &consTib);
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
	gc_init();

	void *p = cons(NULL, cons(NULL, NULL));
	printf("Allocated: %p\n", p);

	foo();
	printf("Collecting garbage...\n");
	garbage_collect();

	printf("Again: %p\n", p);
}
