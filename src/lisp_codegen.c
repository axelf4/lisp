#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include "gc.c"
#include "lisp.c"
#include "util.c"
#include "phf.c"

LispObject lisp_eval(struct LispCtx *, LispObject) { unreachable(); }

#define KW_FIRST fquote
#define KW_LAST ffn

static bool write_keyword_phf(struct LispCtx *ctx, FILE *f) {
#define X(_, var) LISP_CONST(ctx, var) - LISP_CONST(ctx, KW_FIRST),
	uint32_t kw_symbols[LISP_NUM_KEYWORDS] = { FOR_KEYWORDS(X) };
#undef X

	struct Phf result;
	struct PhfParameters params = { .c = 0, .alpha = 1 };
	uint64_t seed, max_tries = 32;
	for (seed = 0; seed < max_tries; ++seed) {
		uint64_t keys[LISP_NUM_KEYWORDS];
		for (size_t i = 0; i < LENGTH(keys); ++i)
			keys[i] = fxhash(seed, kw_symbols[i] );

		switch (phf_build(&params, LENGTH(keys), keys, &result)) {
		case PHF_OK: goto out;
		case PHF_RESEED: case PHF_NO_MEMORY: break;
		}
	}
	return false;

out:
	fprintf(f, "static inline enum LispKeyword lisp_symbol_to_keyword(struct LispCtx *ctx, LispObject sym) {\n"
		"\tstruct Phf keyword_phf = {\n"
		"\t\t.n = %zu, .n_prime = %zu, .m = %zu,\n"
		"\t\t.pilots = (unsigned char []) {",
		result.n, result.n_prime, result.m);
	for (size_t i = 0; i < result.m; ++i) fprintf(f, "%hhu, ", result.pilots[i]);
	if (result.n_prime > result.n) {
		fputs("},\n\t\t.remap = (size_t []) {", f);
		for (size_t i = 0; i < result.n_prime - result.n; ++i)
			fprintf(f, "%zu,", result.remap[i]);
	}
	fprintf(f, "}\n\t};\n"
		"\tif (sym - LISP_CONST(ctx, " STR(KW_FIRST) ")"
		" > LISP_CONST(ctx, " STR(KW_LAST) ") - LISP_CONST(ctx, " STR(KW_FIRST) ")\n"
		"\t\t|| lisp_type(sym) != LISP_SYMBOL) return LISP_NO_KEYWORD;\n"
		"\tuint32_t key = sym - LISP_CONST(ctx, " STR(KW_FIRST) ");\n"
		"\tswitch (phf(&keyword_phf, fxhash(%" PRIu64 ", key))) {\n",
		seed);
	for (enum LispKeyword i = 0; i < LISP_NUM_KEYWORDS; ++i) {
		size_t j = phf(&result, fxhash(seed, kw_symbols[i]));
		fprintf(f, "\t\tcase %zu: return %d;\n", j, i);
	}
	fprintf(f, "\t\tdefault: unreachable();\n"
		"\t}\n"
		"}\n");

	phf_free(&result);
	return true;
}

int main(int argc, char *argv[]) {
	const char *filename = argv[argc - 1];
	int ret = 0;
	FILE *f;
	if (!(f = fopen(filename, "w"))) return EXIT_FAILURE;
	struct LispCtx *ctx;
	if (!(ctx = lisp_new())) { ret = EXIT_FAILURE; goto out_close; }

	fprintf(f,
		"#include <stddef.h>\n\n"

#define X(var, _) "Lobj " #var ";"
		"struct LispConstants {" FOR_SYMBOL_CONSTS(X) "};\n"

#define Y(...) "{%" PRIu32 "},"
		"static constexpr struct LispConstants lisp_consts = {" FOR_SYMBOL_CONSTS(Y) "};\n"

		"#define LISP_CONST(ctx, name) "
#if USE_COMPRESSED_PTRS
		"GC_DECOMPRESS(ctx, lisp_consts.name)\n"
#else
		"((uintptr_t)ctx + lisp_consts.name.p)\n"
#endif

#define Z(var, _) , (uint32_t) (LISP_CONST(ctx, var) - (uintptr_t) ctx)
		FOR_SYMBOL_CONSTS(Z));

	if (!write_keyword_phf(ctx, f)) ret = EXIT_FAILURE;

	if (ferror(f)) ret = EXIT_FAILURE;
	lisp_free(ctx);
out_close:
	fclose(f);
	return ret;
}
