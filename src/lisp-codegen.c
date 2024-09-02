#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include "gc.c"
#include "lisp.c"
#include "util.c"

LispObject lisp_eval(struct LispCtx *, LispObject) { unreachable(); }

int main(int argc, char *argv[]) {
	const char *filename = argv[argc - 1];
	struct GcHeap *heap;
	struct LispCtx *ctx;
	FILE *f;
	if (!((ctx = (struct LispCtx *) (heap = gc_new())) && lisp_init(ctx)
			&& (f = fopen(filename, "w"))))
		return EXIT_FAILURE;

	fprintf(f,
#define X(var, _) "Lobj " #var ";"
		"struct LispConstants {" FOR_SYMBOL_CONSTS(X) "};\n"

#define Y(...) "{%" PRIu32 "},"
		"static constexpr struct LispConstants lisp_consts = {" FOR_SYMBOL_CONSTS(Y) "};\n"

		"#define LISP_CONST_COMPRESSED(ctx, name) "
#if USE_COMPRESSED_PTRS
		"lisp_consts.name"
#define SUB_BASE
#else
		"(struct GcRef) { lisp_consts.name.p + (uintptr_t) (ctx) }"
#define SUB_BASE - (uintptr_t) ctx
#endif
		"\n#define LISP_CONST(ctx, name) GC_DECOMPRESS((ctx), LISP_CONST_COMPRESSED((ctx), name))\n"

#define Z(var, _) , (uint32_t) (GC_COMPRESS(ctx->var).p SUB_BASE)
		FOR_SYMBOL_CONSTS(Z));
	fclose(f);

	lisp_free(ctx);
	gc_free(heap);
}
