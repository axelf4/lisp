#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <setjmp.h>
#include <cmocka.h>

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size);

static void run(void **state) {
	FILE *f;
	if (!(f = fopen(*state, "rb"))) goto err;
	fseek(f, 0, SEEK_END);
	size_t size = ftell(f);
	fseek(f, 0, SEEK_SET);
	uint8_t *data;
	if (!(data = malloc(size))
		|| fread(data, sizeof *data, size, f) != size) goto err;
	fclose(f);

	LLVMFuzzerTestOneInput(data, size);
	free(data);
	return;
err: fail();
}

int main(int argc, char *argv[]) {
	struct CMUnitTest *tests;
	if (!(tests = malloc((argc - 1) * sizeof *tests))) return EXIT_FAILURE;
	for (int i = 1; i < argc; ++i)
		tests[i - 1] = (struct CMUnitTest) { argv[i], run, .initial_state = argv[i] };
	int result = _cmocka_run_group_tests("corpus", tests, argc - 1, NULL, NULL);
	free(tests);
	return result;
}
