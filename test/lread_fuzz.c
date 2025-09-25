/** Lisp reader LLVM fuzzer harness. */

#include "lisp.h"

static struct LispCtx *ctx;

static void free_ctx() { lisp_free(ctx); }

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
	if (!ctx && (!(ctx = lisp_new()) || atexit(free_ctx))) abort();

	char *buf;
	if (!(buf = malloc(size + 1))) abort();
	memcpy(buf, data, size);
	buf[size] = '\0';

	const char *s = buf;
	LispObject result;
	lisp_read(ctx, &s, &result);

	free(buf);
	return 0;
}
