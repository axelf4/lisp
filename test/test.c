#include <stddef.h>
#include <stdarg.h>
#include <setjmp.h>
#include <cmocka.h>
#include <stdio.h>
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

static int setup_lisp(void **state) { *state = lisp_init(); return 0; }
static int teardown_lisp(void **state) { lisp_free(*state); return 0; }

void test_reader(void **state) {
	LispObject *obj;
	assert_int_equal(lisp_read_whole(*state, "(0 .", &obj), LISP_READ_EOF);
	assert_int_equal(lisp_read_whole(*state, "(0 . 0 .", &obj), LISP_READ_EXPECTED_RPAREN);
}

static LispObject *eval(struct LispContext *ctx, const char *s) {
	LispObject *form;
	assert_int_equal(lisp_read_whole(ctx, s, &form), LISP_READ_OK);
	return lisp_eval(ctx, form);
}

static void assert_lisp_equal(LispObject *a, LispObject *b) {
	enum LispObjectType type = lisp_type(b);
	assert_int_equal(lisp_type(a), type);
	switch (type) {
	case LISP_NIL: break;
	case LISP_CONS:
		struct Cons *x = a, *y = b;
		assert_lisp_equal(x->car, y->car);
		assert_lisp_equal(x->cdr, y->cdr);
		break;
	case LISP_INTEGER: assert_int_equal(*(int *) a, *(int *) b); break;
	default: UNREACHABLE("TODO");
	}
}

void test_eval(void **state) {
	struct LispContext *ctx = *state;
	assert_lisp_equal(eval(ctx, "\
(let ((mult (lambda (x y acc) (if (< y 1) acc (mult x (+ y -1) (+ acc x)))))) \
  (mult 4 3 0))"),
		lisp_integer(12));
	// Test that closures capture the environment
	assert_lisp_equal(
		eval(ctx, "((let ((x 1)) (lambda () x)))"),
		lisp_integer(1));
}

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
		cmocka_unit_test(test_eval),
	};
	return cmocka_run_group_tests(lisp_tests, setup_lisp, teardown_lisp);
}
