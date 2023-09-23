#include <stddef.h>
#include <stdarg.h>
#include <setjmp.h>
#include <cmocka.h>
#include <gc.h>
#include <lisp.h>
#include <tbl.h>

void test1() {
	assert_non_null(cons(NULL, NULL));
}

void test_hash_table() {
	struct HashTable table = tbl_new();
	tbl_insert(&table, (struct Key) { 42, &table });

	assert_ptr_equal(tbl_find(table, (struct Key) { .key = 42 })->value, &table);
	assert_null(tbl_find(table, (struct Key) { .key = 1337 }));

	tbl_free(table);
}

int main(void) {
	if (!(heap = gc_new())) return 1;

	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test1),
		cmocka_unit_test(test_hash_table),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
