#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <getopt.h>
#include <signal.h>
#include <unistd.h>
#include "lisp.h"

#define STRING(s) (struct Str){ sizeof(s) - 1, s }
#define STDLIB_LANDMARK "lisp/core.lisp"

struct Str { size_t len; const char *p; };

struct Config {
	const char *script; ///< File name of script to run, or NULL.
};

static struct LispCtx *ctx;

static bool parse_args(int argc, char *argv[], struct Config *config) {
	const char *shortopts = "hs:";
	struct option longopts[] = {
		{"help", no_argument, NULL, 'h' },
		{"script",     required_argument, NULL,  's' },
		{}
	};
	int c;

	while ((c = getopt_long(argc, argv, shortopts, longopts, NULL)) >= 0)
		switch (c) {
		case 's': config->script = optarg; break;
		case 'h': case '?': goto err;
		default: unreachable();
		}

	if (optind >= argc) return true;
err: fprintf(stderr, "usage: %s [--script=<file>]\n", *argv);
	return false;
}

[[gnu::cold]] static void signal_handler(int sig, siginfo_t *info, void *ucontext) {
	if (lisp_signal_handler(sig, info, ucontext, ctx)) return;

	signal(sig, SIG_DFL);
	// Returning is not defined by POSIX, but in practice allows the
	// signal to be re-delivered.
}

static size_t search_up(size_t len, char s[static len], struct Str *landmarks) {
	for (char *sep; (sep = strrchr(s, '/')); *sep = '\0')
		for (struct Str *landmark = landmarks; landmark->p; ++landmark) {
			size_t n = sep + 1 - s + landmark->len;
			if (n >= len) continue;
			memcpy(sep + 1, landmark->p, landmark->len);
			s[n] = '\0';
			if (!access(s, F_OK)) return n;
		}
	return 0;
}

static void init_lisp_path(const struct Config *config, const char *argv0) {
	LispObject list = NIL(ctx);

	if (!argv0) argv0 = "";
	char s[PATH_MAX];
	ssize_t n = readlink( "/proc/self/exe", s, sizeof s);
	if (n < 0 || sizeof s <= (size_t)n) strncpy(s, argv0, n = sizeof s - 1);
	s[n] = '\0';
	// Push standard library directory if landmark is found
	struct Str landmarks[]
		= { STRING(DATADIR "/" STDLIB_LANDMARK), STRING(STDLIB_LANDMARK), {} };
	if ((n = search_up(sizeof s, s, landmarks))) {
		LispObject stdlib_dir = lisp_str(ctx, n - (sizeof "/core.lisp" - 1), s);
		list = cons(ctx, stdlib_dir, list);
	}

	// TODO Consult LISPPATH environment variable

	list = cons(ctx, lisp_str(ctx, 0, ""), list); // Push the current directory

	// Push directory of input script
	const char *script = config->script, *script_base;
	if (script && (script_base = strrchr(script, '/')))
		list = cons(ctx, lisp_str(ctx, script_base - script, script), list);

	struct LispSymbol *sload_path = UNTAG_OBJ(LISP_INTERN(ctx, "load-path"));
	sload_path->value = list;
}

/** Executes a file of Lisp code named @p filename. */
static LispObject lisp_load(struct LispCtx *ctx, const char *filename) {
	// TODO Support reading from - or /dev/stdin
	size_t filename_len = strlen(filename);
	char *buf = NULL;
	FILE *f;
	bool ok = false;
	struct LispSymbol *sload_path = UNTAG_OBJ(LISP_INTERN(ctx, "load-path"));
	for (LispObject xs = sload_path->value, x; !NILP(ctx, x = pop(ctx, &xs));) {
		if (lisp_type(x) != LISP_STRING) goto out_free_buf;
		struct LispString *dir = UNTAG_OBJ(x);
		if (!(buf = realloc(buf, dir->len + sizeof "/" + filename_len)))
			die("malloc failed");
		memcpy(buf, dir->s, dir->len);
		buf[dir->len] = '/';
		memcpy(buf + dir->len + 1, filename, filename_len + 1);

		if ((f = fopen(dir->len ? buf : filename, "rb"))) goto found;
		if (errno != ENOENT) { perror("fopen failed"); goto out_free_buf; }
	}
	fprintf(stderr, "file not found: %s\n", filename);
	goto out_free_buf;
found:
	fseek(f, 0, SEEK_END);
	long len;
	if ((len = ftell(f)) < 0) { fprintf(stderr, "ftell failed\n"); goto out_close; }
	fseek(f, 0, SEEK_SET);
	if (!(buf = realloc(buf, (size_t)len + 1))) die("malloc failed");
	if (fread(buf, 1, len, f) != (size_t)len) { perror("fread failed"); goto out_close; }
	buf[len] = '\0';

	LispObject form, result = NIL(ctx);
	const char *s = buf;
	enum LispReadError err;
	while (!(err = lisp_read(ctx, &s, &form))) result = lisp_eval(ctx, form);
	switch (err) {
	case LISP_READ_OK: unreachable();
	case LISP_READ_EMPTY: assert(!*s); ok = true; break;
	default: fprintf(stderr, "lisp_read failed: %d\n", err);
	}
out_close: fclose(f);
out_free_buf: free(buf);
	if (ok) return result; else throw(1);
}

void do_repl() {
	puts("Type forms to execute:");
	char line[256];
	while (fputs("> ", stdout), fgets(line, sizeof line, stdin)) {
		LispObject form;
		enum LispReadError error;
		if ((error = lisp_read_whole(ctx, line, &form)))
			fprintf(stderr, "lisp_read failed: %d\n", error);
		else {
			lisp_print(ctx, lisp_eval(ctx, form), stdout);
			putchar('\n');
		}
	}
}

int main(int argc, char *argv[]) {
	struct Config config;
	if (!parse_args(argc, argv, &config)) return EXIT_FAILURE;

	if (!(ctx = lisp_new())) return EXIT_FAILURE;

	struct sigaction action;
	action.sa_sigaction = signal_handler;
	sigemptyset(&action.sa_mask);
	action.sa_flags = SA_SIGINFO | SA_NODEFER | SA_RESTART;
	if (sigaction(SIGSEGV, &action, NULL)) return EXIT_FAILURE;

	init_lisp_path(&config, *argv);
	lisp_load(ctx, "core.lisp");

	if (config.script) lisp_load(ctx, config.script); else do_repl();

#ifndef NDEBUG
	lisp_free(ctx);
#endif
}
