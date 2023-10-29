#include <stdio.h>
#include "gc.h"
#include "lisp.h"

int main(void) {
	if (!(heap = gc_new())) return 1;
	struct LispContext *ctx = lisp_init();

	char line[256];
	while (fgets(line, sizeof line, stdin)) {
		LispObject *form;
		enum LispReadError error;
		if ((error = lisp_read_whole(ctx, line, &form)))
			fprintf(stderr, "Error: %d\n", error);
		else {
			fputs("Read expression: ", stdout);
			lisp_print(form);
			fputs("\nResult: ", stdout);
			lisp_print(lisp_eval(ctx, form));
			putchar('\n');
		}
	}

	lisp_free(ctx);
}
