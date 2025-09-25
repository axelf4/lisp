/** Lisp reader LLVM fuzzer harness. */

#include "lisp.h"

static struct GcHeap *heap;

static void free_heap() { gc_free(heap); }

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
	if (!heap && (!(heap = gc_new()) || atexit(free_heap))) abort();

	struct LispCtx *ctx = (struct LispCtx *) heap;
	if (!lisp_init(ctx)) abort();

	char *buf;
	if (!(buf = malloc(size + 1))) abort();
	memcpy(buf, data, size);
	buf[size] = '\0';

	const char *s = buf;
	LispObject result;
	lisp_read(ctx, &s, &result);

	free(buf);
	lisp_free(ctx);
	return 0;
}
