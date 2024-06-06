#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include "gc.c"
#include "lisp.c"
#include "util.c"

LispObject lisp_eval([[maybe_unused]] struct LispCtx *ctx, [[maybe_unused]] LispObject form) {
	return NIL;
}

int main(int argc, char *argv[]) {
	struct GcHeap *heap;
	struct LispCtx *ctx;
	if (!((ctx = (struct LispCtx *) (heap = gc_new())) && lisp_init(ctx)))
		return EXIT_FAILURE;

	const char *filename = argv[argc - 1];
	FILE *f;
	if (!(f = fopen(filename, "w")))
		return EXIT_FAILURE;
	fprintf(f,
		"struct LispConstants { Lobj ffn, fif, flet, fset, fprogn, fquote, t; };\n"
		"static constexpr struct LispConstants lisp_constants = {\n"
		"\t{ %" PRIu32 " }, { %" PRIu32 " }, { %" PRIu32 " }, { %" PRIu32 " }, { %" PRIu32 " }, { %" PRIu32 " }, { %" PRIu32 " }\n"
		"};",
		GC_COMPRESS(ctx->ffn).p,
		GC_COMPRESS(ctx->fif).p,
		GC_COMPRESS(ctx->flet).p,
		GC_COMPRESS(ctx->fset).p,
		GC_COMPRESS(ctx->fprogn).p,
		GC_COMPRESS(ctx->fquote).p,
		GC_COMPRESS(ctx->t).p);
	fclose(f);

	lisp_free(ctx);
	gc_free(heap);
}
