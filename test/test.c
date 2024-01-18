#include <stddef.h>
#include <stdarg.h>
#include <setjmp.h>
#include <cmocka.h>
#include <stdio.h>
#include "gc.h"
#include "lisp.h"
#include "rope.h"
#include "util.h"

static void test_next_power_of_2(void **) {
	assert_int_equal(next_power_of_2(3), 4);
	assert_int_equal(next_power_of_2(8), 8);
}

static void do_nothing(void *) {}

static void do_throw_error(void *) {
	throw(42);
	fail();
}

static void test_exception(void **state) {
	assert_int_equal(pcall(state, do_nothing), 0);
	assert_int_equal(pcall(state, do_throw_error), 42);
}

static uint64_t my_hash(int key) { return key; }
static bool my_equal(int a, int b) { return a == b; }

#define NAME my
#define KEY int
#include "tbl.h"

static void test_hash_table(void **) {
	struct Table table = tbl_new();
	int *entry;
	my_tbl_entry(&table, 1, &entry);
	assert_int_equal(*entry, 1);
	assert_ptr_equal(my_tbl_find(&table, 1), entry);
	assert_null(my_tbl_find(&table, 2));
	my_tbl_free(&table);
}

static void test_rope(void **) {
	struct Rope rope;
	if (!rope_init(&rope)) fail();
	rope_replace(&rope, 0, 0, 26, "abcdefghijklmnopqrstuvwxyz");
	assert_int_equal(rope_size(&rope), 26);
	rope_replace(&rope, 1, 17, 0, "");
	assert_int_equal(rope_size(&rope), 10);
	rope_free(&rope);
}

static int setup_lisp(void **state) { *state = lisp_new(); return 0; }
static int teardown_lisp(void **state) { lisp_free(*state); return 0; }

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
	case LISP_SYMBOL: assert_ptr_equal(a, b); break;
	case LISP_INTEGER: assert_int_equal(*(int *) a, *(int *) b); break;
	default: die("TODO");
	}
}

static void assert_read_whole_equal(void *state, const char *s, LispObject *expected) {
	LispObject *x;
	assert_int_equal(lisp_read_whole(state, s, &x), LISP_READ_OK);
	assert_lisp_equal(x, expected);
}

static void test_reader(void **state) {
	LispObject *obj;
	assert_int_equal(lisp_read_whole(*state, "(0 .", &obj), LISP_READ_EOF);
	assert_int_equal(lisp_read_whole(*state, "(0 . 0 .", &obj), LISP_READ_EXPECTED_RPAREN);
	// Test lexing a symbol with a numeric prefix
	assert_read_whole_equal(*state, "1x", intern(*state, sizeof "1x" - 1, "1x"));
}

static LispObject *eval(struct LispContext *ctx, const char *s) {
	LispObject *form;
	assert_int_equal(lisp_read_whole(ctx, s, &form), LISP_READ_OK);
	return lisp_eval(ctx, form);
}

static void test_eval(void **state) {
	struct LispContext *ctx = *state;
	assert_lisp_equal(eval(ctx, "\
(let ((mult (fn (x y acc) (if (< y 1) acc (mult x (+ y -1) (+ acc x)))))) \
  (mult 4 3 0))"),
		lisp_integer(12));

	// Test that closures capture the environment
	assert_lisp_equal(eval(ctx, "((let ((x 1)) (fn () x)))"), lisp_integer(1));

	// Test that macros work
	eval(ctx, "(set mymacro (cons 'macro (fn () '(+ 1 2))))");
	assert_lisp_equal(eval(ctx, "(mymacro)"), lisp_integer(3));
}

int main(void) {
	if (!(heap = gc_new())) return 1;

	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_next_power_of_2),
		cmocka_unit_test(test_exception),
		cmocka_unit_test(test_hash_table),
		cmocka_unit_test(test_rope),
	};
	int result;
	if ((result = cmocka_run_group_tests(tests, NULL, NULL))) return result;

	const struct CMUnitTest lisp_tests[] = {
		cmocka_unit_test(test_reader),
		cmocka_unit_test(test_eval),
	};
	return cmocka_run_group_tests(lisp_tests, setup_lisp, teardown_lisp);
}
