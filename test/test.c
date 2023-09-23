#include <stddef.h>
#include <stdarg.h>
#include <setjmp.h>
#include <cmocka.h>
#include <gc.h>
#include <lisp.h>

void test1() {
	assert_non_null(cons(NULL, NULL));
}

int main(void) {
	if (!(heap = gc_new())) return 1;

	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test1),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
