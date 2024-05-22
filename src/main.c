#include <stdio.h>
#include <signal.h>
#include "gc.h"
#include "lisp.h"

static struct LispCtx ctx;

static void signal_handler(int sig, siginfo_t *info, void *ucontext) {
	if (lisp_signal_handler(sig, info, ucontext, &ctx)) return;

	signal(sig, SIG_DFL);
	// Returning is not defined by POSIX, but in practice allows the
	// signal to be re-delivered.
}

int main() {
	if (!((heap = gc_new(&ctx)) && lisp_init(&ctx))) return EXIT_FAILURE;

	struct sigaction action;
	action.sa_sigaction = signal_handler;
	sigemptyset(&action.sa_mask);
	action.sa_flags = SA_SIGINFO | SA_NODEFER | SA_RESTART;
	if (sigaction(SIGSEGV, &action, NULL)) return EXIT_FAILURE;

	char line[256];
	while (fgets(line, sizeof line, stdin)) {
		LispObject form;
		enum LispReadError error;
		if ((error = lisp_read_whole(&ctx, line, &form)))
			fprintf(stderr, "Error: %d\n", error);
		else {
			fputs("Read expression: ", stdout);
			lisp_print(form);
			fputs("\nResult: ", stdout);
			lisp_print(lisp_eval(&ctx, form));
			putchar('\n');
		}
	}

#ifndef NDEBUG
	lisp_free(&ctx);
#endif
}
