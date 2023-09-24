#include <stdio.h>
#include "gc.h"
#include "lisp.h"

static void foo() {
	for (int i = 0; i < 2000; ++i) {
		__attribute__ ((unused)) void *p = cons(NULL, NULL);
		/* printf("foo alloc:ed: %p\n", p); */
	}
}

int main(void) {
	if (!(heap = gc_new())) return 1;

	LispObject *object;
	enum LispReadError error;
	if ((error = lisp_read_whole("(nil . ((42) . nil))", &object)))
		fprintf(stderr, "Error: %d\n", error);
	else {
		printf("Read object: ");
		lisp_print(object);
		printf("\n");
	}

	void *p = cons(NULL, cons(NULL, NULL));
	printf("Allocated: %p\n", p);

	foo();
	garbage_collect(heap);

	printf("Again: %p\n", p);
}
