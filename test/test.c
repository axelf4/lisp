#include <stddef.h>
#include <stdarg.h>
#include <setjmp.h>
#include <cmocka.h>
#include "gc.h"
#include "lisp.h"
#include "rope.h"
#include "fxhash.h"
#include "phf.h"
#include "asm.h"
#include "util.h"

static_assert(SAR(-1, 1) == -1);
static_assert(IS_POWER_OF_TWO(16));
static_assert(!IS_POWER_OF_TWO(10));

static void test_rotate_left(void **) {
	assert_int_equal(rol64(UINT64_C(1) << 63 | 0b10, 65), 0b101);
}

static void do_nothing(void *) {}
static void do_throw_error(void *) { throw(42); fail(); }

static void test_exception(void **) {
	assert_int_equal(pcall(NULL, do_nothing), 0);
	assert_int_equal(pcall(NULL, do_throw_error), 42);
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
	struct LispCtx *ctx = *state;
	LispObject obj = cons(ctx, TAG_SMI(1), NIL(ctx));
	garbage_collect(*state);
	assert_int_equal(UNTAG_SMI(car(ctx, obj)), 1);
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
	for (size_t i = 0; i < LENGTH(keys); ++i) keys[i] = fxhash(0, i);
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

static_assert(MODRM(MOD_REG, 5, rsp) == 0xec);

static void test_asm(void **) {
#ifdef __x86_64__
	struct Assembler ctx;
	if (!asm_init(&ctx)) fail();
	*--ctx.p = XI_RET;
	asm_loadu64(&ctx, rax, 42);
	int (*f)() = (int (*)()) asm_assemble(&ctx, ctx.buf + MCODE_CAPACITY);
	assert_int_equal(f(), 42);
	asm_free(&ctx);
#else
	skip();
#endif
}

static void test_insn_len_disasm(void **) {
#ifdef __x86_64__
	assert_int_equal(asm_insn_len((uint8_t[]) { XI_XORr, MODRM(MOD_REG, rax, rax) }), 2);
	assert_int_equal(asm_insn_len((uint8_t[]) { XI_PUSHib, 0 }), 2);
	assert_int_equal(asm_insn_len((uint8_t[]) { XI_JMP, 0, 0, 0, 0 }), 5);
	assert_int_equal(asm_insn_len((uint8_t[]) { 0x0f, XI_Jcc | CC_E, 0, 0, 0, 0 }), 6);

	assert_int_equal(asm_insn_len((uint8_t[])
			{ REX(1, 0, 0, 0), XI_MOVmi, MODRM(MOD_DISP32, 0, rsp), SIB(0, rsp, rsp),
			  0, 0, 0, 0, 0, 0, 0, 0 }), 12);
	assert_int_equal(asm_insn_len((uint8_t[])
			{ REX(1, 0, 0, 0), IMM_GRP1_MR(XG_ADD), MODRM(MOD_REG, rax, rax) }), 3);
#else
	skip();
#endif
}

static void _assert_lisp_equal(struct LispCtx *ctx, LispObject a, LispObject b,
	const char *file, int line) {
	if (lisp_eq(ctx, a, b)) return;

	char *buf;
	size_t size;
	FILE *f;
	if (!(f = open_memstream(&buf, &size))) fail();

	lisp_print(ctx, a, f);
	fputs(" != ", f);
	lisp_print(ctx, b, f);

	fclose(f);
	cm_print_error("%s\n", buf);
	free(buf);
	_fail(file, line);
}
#define assert_lisp_equal(ctx, a, b) _assert_lisp_equal(ctx, a, b, __FILE__, __LINE__)

static void assert_read_whole_equal(struct LispCtx *ctx, const char *s, LispObject expected) {
	LispObject x;
	assert_int_equal(lisp_read_whole(ctx, s, &x), LISP_READ_OK);
	assert_lisp_equal(ctx, x, expected);
}

static void test_intern_reuses_sym(void **state) {
	struct LispCtx *ctx = *state;
	assert_lisp_equal(ctx, intern(ctx, 1, "x"), intern(ctx, 1, "x"));
}

static void test_intern_recognizes_nil(void **state) {
	struct LispCtx *ctx = *state;
	assert_true(NILP(ctx, intern(ctx, sizeof "nil" - 1, "nil")));
}

static void test_reader(void **state) {
	struct LispCtx *ctx = *state;
	LispObject obj;
	assert_int_equal(lisp_read_whole(ctx, "(0 .", &obj), LISP_READ_EOF);
	assert_int_equal(lisp_read_whole(ctx, "(0 . 0 .", &obj), LISP_READ_EXPECTED_RPAREN);
	assert_read_whole_equal(ctx, "42", TAG_SMI(42));
	assert_read_whole_equal(ctx, "1x", intern(ctx, sizeof "1x" - 1, "1x"));
}

static void test_reader_ignores_whitespace(void **state) {
	struct LispCtx *ctx = *state;
	assert_read_whole_equal(ctx, " ( x 0\n . ' y ) ",
		cons(ctx, intern(ctx, 1, "x"), cons(ctx, 0,
				cons(ctx, LISP_CONST(ctx, fquote), cons(ctx, intern(ctx, 1, "y"), NIL(ctx))))));
}

static LispObject eval(struct LispCtx *ctx, const char *s) {
	LispObject form;
	assert_int_equal(lisp_read_whole(ctx, s, &form), LISP_READ_OK);
	return lisp_eval(ctx, form);
}

static void test_eval(void **state) {
	struct LispCtx *ctx = *state;
	assert_lisp_equal(ctx, eval(ctx, "\
(let (mult (fn (x y acc) (if (< y 1) acc (mult x (+ y -1) (+ acc x))))) \
  (mult 4 3 0))"), TAG_SMI(12));
}

static void test_closure_captures_env(void **state) {
	struct LispCtx *ctx = *state;
	assert_lisp_equal(ctx, eval(ctx, "((let (x 't) (fn () x)))"), LISP_CONST(ctx, t));
}

static void test_macros_work(void **state) {
	struct LispCtx *ctx = *state;
	eval(ctx, "(set mymacro (cons (fn () '(+ 1 2)) nil))");
	assert_lisp_equal(ctx, eval(ctx, "(mymacro)"), TAG_SMI(3));
}

static void test_man_or_boy(void **state) {
	struct LispCtx *ctx = *state;
	const char *s =
		"(let (a (fn (k x1 x2 x3 x4 x5)\n"
		"          (let (b (fn () (set k (+ k -1)) (a k b x1 x2 x3 x4)))\n"
		"            (if (< k 1) (+ (x4) (x5)) (b)))))\n"
		"  (a 10 (fn () 1) (fn () -1) (fn () -1) (fn () 1) (fn () 0)))";
	assert_lisp_equal(ctx, eval(ctx, s), TAG_SMI(-67));
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
		cmocka_unit_test(test_asm),
		cmocka_unit_test(test_insn_len_disasm),
		cmocka_unit_test(test_intern_reuses_sym),
		cmocka_unit_test(test_intern_recognizes_nil),
		cmocka_unit_test(test_reader),
		cmocka_unit_test(test_reader_ignores_whitespace),
		cmocka_unit_test(test_eval),
		cmocka_unit_test(test_closure_captures_env),
		cmocka_unit_test(test_macros_work),
		cmocka_unit_test(test_man_or_boy),
	};
	return cmocka_run_group_tests(tests, setup, teardown);
}
