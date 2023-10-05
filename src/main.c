#include <stdio.h>
#include "gc.h"
#include "lisp.h"

void go(struct LispContext *ctx);

void start_tui();

int main(void) {
	if (!(heap = gc_new())) return 1;
	struct LispContext *ctx = lisp_init();

	LispObject *object;
	enum LispReadError error;
	if ((error = lisp_read_whole(ctx,
				"\
(let ((loop (lambda (x) (if (< x 5) (progn (print x) (loop (+ x 1))) 10))))	\
  (print (loop 0)))",
				&object))) { printf("Error: %d\n", error); __builtin_exit(1); }
	printf("Read object: ");
	lisp_print(object);
	printf("\n");
	lisp_print(lisp_eval(ctx, object));

	garbage_collect(heap);

	start_tui();

	/*
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
	*/

	lisp_free(ctx);
}
