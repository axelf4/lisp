#include <stdio.h>
#include "gc.h"
#include "lisp.h"

static void foo() {
	for (int i = 0; i < 2000; ++i) {
		__attribute__ ((unused)) void *p = cons(NULL, NULL);
	}
}

int main(void) {
	if (!(heap = gc_new())) return 1;
	struct LispContext *ctx = lisp_init();

	LispObject *object;
	enum LispReadError error;
	if ((error = lisp_read_whole(ctx, "(x x y nil . ((42) . nil))", &object)))
		fprintf(stderr, "Error: %d\n", error);
	else {
		printf("Read object: ");
		lisp_print(object);
		printf("\n");
	}

	foo();
	garbage_collect(heap);

	char line[256];
	while (fgets(line, sizeof line, stdin)) {
		LispObject *object;
		enum LispReadError error;
		if ((error = lisp_read_whole(ctx, line, &object)))
			fprintf(stderr, "Error: %d\n", error);
		else {
			printf("Read expression: ");
			lisp_print(object);
			printf("\n");

			printf("Result: ");
			lisp_print(lisp_eval(ctx, object));
			printf("\n");
		}
	}
}
