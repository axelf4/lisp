#include <stdio.h>
#include <signal.h>
#include "lisp.h"

static struct LispCtx *ctx;

[[gnu::cold]] static void signal_handler(int sig, siginfo_t *info, void *ucontext) {
	if (lisp_signal_handler(sig, info, ucontext, ctx)) return;

	signal(sig, SIG_DFL);
	// Returning is not defined by POSIX, but in practice allows the
	// signal to be re-delivered.
}

static LispObject lisp_load(struct LispCtx *ctx, const char *filename) {
	FILE *f;
	if (!(f = fopen(filename, "r"))) die("Failed to open file");
	fseek(f, 0, SEEK_END);
	size_t length = ftell(f);
	fseek(f, 0, SEEK_SET);
	char *buf;
	if (!(buf = malloc(length + 1))) die("malloc failed");
	if (fread(buf, 1, length, f) != length) die("fread failed");
	fclose(f);
	buf[length] = '\0';

	const char *s = buf;
	LispObject form, result = NIL(ctx);
	for (;;) {
		enum LispReadError err = lisp_read(ctx, &s, &form);
		switch (err) {
		case LISP_READ_OK: result = lisp_eval(ctx, form); break;
		case LISP_READ_EMPTY:
			if (*s != '\0') { err = LISP_READ_EOF; goto err; }
			free(buf);
			return result;
		default: err: die("Error: %d", err);
		}
	}
}

int main() {
	if (!(ctx = lisp_new())) return EXIT_FAILURE;

	struct sigaction action;
	action.sa_sigaction = signal_handler;
	sigemptyset(&action.sa_mask);
	action.sa_flags = SA_SIGINFO | SA_NODEFER | SA_RESTART;
	if (sigaction(SIGSEGV, &action, NULL)) return EXIT_FAILURE;

	lisp_load(ctx, "stdlib.lisp");

	char line[256];
	while (fgets(line, sizeof line, stdin)) {
		LispObject form;
		enum LispReadError error;
		if ((error = lisp_read_whole(ctx, line, &form)))
			fprintf(stderr, "Error: %d\n", error);
		else {
			fputs("Read expression: ", stdout);
			lisp_print(ctx, form, stdout);
			fputs("\nResult: ", stdout);
			lisp_print(ctx, lisp_eval(ctx, form), stdout);
			putchar('\n');
		}
	}

#ifndef NDEBUG
	lisp_free(ctx);
#endif
}
