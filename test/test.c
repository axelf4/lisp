#include <stddef.h>
#include <stdarg.h>
#include <setjmp.h>
#include <cmocka.h>
#include "gc.h"
#include "lisp.h"
#include "util.h"

void test1(void **) {
	assert_non_null(cons(NULL, NULL));
}

void test_next_power_of_2(void **) {
	assert_int_equal(next_power_of_2(3), 4);
	assert_int_equal(next_power_of_2(8), 8);
}

static uint64_t my_hash(int key) { return key; }
static bool my_equal(int a, int b) { return a == b; }

#define NAME my
#define KEY int
#define TYPE HashTable
#include "tbl.h"

void my_tbl_insert(struct HashTable *table, int key) {
	int *entry;
	my_tbl_entry(table, key, &entry);
	*entry = key;
}

void test_hash_table(void **) {
	struct HashTable table = my_tbl_new();
	my_tbl_insert(&table, 1);
	int *entry = my_tbl_find(&table, 1);
	assert_int_equal(entry ? *entry : 0, 1);
	assert_null(my_tbl_find(&table, 2));
	my_tbl_free(&table);
}

void test_reader(void **state) {
	LispObject *result;
	assert_int_equal(lisp_read_whole(*state, "(0 .", &result), LISP_READ_EOF);
}

static int setup_lisp(void **state) { *state = lisp_init(); return 0; }
static int teardown_lisp(void **state) { lisp_free(*state); return 0; }

int main(void) {
	if (!(heap = gc_new())) return 1;

	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test1),
		cmocka_unit_test(test_next_power_of_2),
		cmocka_unit_test(test_hash_table),
	};
	int result;
	if ((result = cmocka_run_group_tests(tests, NULL, NULL))) return result;

	const struct CMUnitTest lisp_tests[] = {
		cmocka_unit_test(test_reader),
	};
	return cmocka_run_group_tests(lisp_tests, setup_lisp, teardown_lisp);
}
