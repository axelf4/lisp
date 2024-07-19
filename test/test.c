#include <stddef.h>
#include <stdarg.h>
#include <setjmp.h>
#include <cmocka.h>
#include "gc.h"
#include "lisp.h"
#include "rope.h"
#include "phf.h"
#include "util.h"

static_assert(SAR(-1, 1) == -1);
static_assert(IS_POWER_OF_TWO(16));
static_assert(!IS_POWER_OF_TWO(10));

static void test_rotate_left(void **) {
	assert_int_equal(rotate_left(UINT64_C(1) << 63 | 2, 65), 0b101);
}

static void do_nothing(void *) {}
static void do_throw_error(void *) { throw(42); fail(); }

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
	assert_int_equal(table.len, 1);
	assert_int_equal(*entry, 1);
	assert_ptr_equal(my_tbl_find(&table, 1), entry);
	assert_null(my_tbl_find(&table, 2));
	my_tbl_free(&table);
}

static void test_gc_traces_live_object(void **state) {
	LispObject obj = cons(*state, TAG_SMI(1), NIL);
	garbage_collect(*state);
	assert_int_equal(UNTAG_SMI(car(*state, obj)), 1);
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

/** Tests constructing a perfect hash function. */
static void test_phf_is_bijective(void **) {
	uint64_t keys[256];
	for (size_t i = 0; i < LENGTH(keys); ++i) keys[i] = fxhash64(0, i);
	struct Phf f;
	struct PhfParameters params = { .c = 3, .alpha = 0.99 };
	assert_int_equal(phf_build(&params, LENGTH(keys), keys, &f), PHF_OK);

	bool seen[LENGTH(keys)] = {};
	for (size_t i = 0; i < LENGTH(keys); ++i) { // Assert injectiveness
		size_t pos = phf(&f, keys[i]);
		assert_false(seen[pos]);
		seen[pos] = true;
	}
	phf_free(&f);
}

static void assert_lisp_equal(struct LispCtx *ctx, LispObject a, LispObject b) {
	assert_true(lisp_eq(ctx, a, b));
}

static void assert_read_whole_equal(struct LispCtx *ctx, const char *s, LispObject expected) {
	LispObject x;
	assert_int_equal(lisp_read_whole(ctx, s, &x), LISP_READ_OK);
	assert_lisp_equal(ctx, x, expected);
}

static void test_reader(void **state) {
	struct LispCtx *ctx = *state;
	LispObject obj;
	assert_int_equal(lisp_read_whole(ctx, "(0 .", &obj), LISP_READ_EOF);
	assert_int_equal(lisp_read_whole(ctx, "(0 . 0 .", &obj), LISP_READ_EXPECTED_RPAREN);
	// Test lexing a symbol with a numeric prefix
	assert_read_whole_equal(ctx, "1x", intern(ctx, sizeof "1x" - 1, "1x"));
}

static void test_reader_ignores_whitespace(void **state) {
	struct LispCtx *ctx = *state;
	assert_read_whole_equal(ctx, " ( x 0\n . ' y ) ",
		cons(ctx, intern(ctx, 1, "x"), cons(ctx, 0,
				cons(ctx, LISP_CONST(ctx, fquote), cons(ctx, intern(ctx, 1, "y"), NIL)))));
}

static LispObject eval(struct LispCtx *ctx, const char *s) {
	LispObject form;
	assert_int_equal(lisp_read_whole(ctx, s, &form), LISP_READ_OK);
	return lisp_eval(ctx, form);
}

static void test_eval(void **state) {
	struct LispCtx *ctx = *state;
	// Test that closures capture the environment
	assert_lisp_equal(ctx, eval(ctx, "((let ((x t)) (fn () x)))"), LISP_CONST(ctx, t));

	// Test that macros work
	eval(ctx, "(set mymacro (cons (fn () '(+ 1 2)) nil))");
	assert_lisp_equal(ctx, eval(ctx, "(mymacro)"), TAG_SMI(3));

	assert_lisp_equal(ctx, eval(ctx, "\
(let ((mult (fn (x y acc) (if (< y 1) acc (mult x (+ y -1) (+ acc x)))))) \
  (mult 4 3 0))"), TAG_SMI(12));
}

static int setup(void **state) {
	return !((*state = gc_new()) && lisp_init(*state));
}
static int teardown(void **state) {
	lisp_free(*state);
	gc_free(*state);
	return 0;
}

int main() {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_rotate_left),
		cmocka_unit_test(test_exception),
		cmocka_unit_test(test_hash_table),
		cmocka_unit_test(test_gc_traces_live_object),
		cmocka_unit_test(test_rope),
		cmocka_unit_test(test_phf_is_bijective),
		cmocka_unit_test(test_reader),
		cmocka_unit_test(test_reader_ignores_whitespace),
		cmocka_unit_test(test_eval),
	};
	return cmocka_run_group_tests(tests, setup, teardown);
}
