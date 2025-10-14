/** Lisp reader. */

#include <stddef.h>
#include "lisp.h"

/** Maximum number of digits for reading int31_t safely (no overflow). */
#define INT31_SAFE_DIG 9 // int31_t max is 1073741823, 10 digits

/** Character type lookup table. */
static enum CharType : unsigned char {
	CHAR_SPACE = 1,
	CHAR_SPECIAL = 1 << 1,
	CHAR_COMMENT = 1 << 2,
	CHAR_DIGIT = 1 << 3,
} char_table[256] = {
	['\0'] = 0x2, ['\t'] = 0x1, ['\n'] = 0x1, ['\r'] = 0x1, [' '] = 0x1,
	['\''] = 0x2, ['('] = 0x2, [')'] = 0x2, [','] = 0x2, ['.'] = 0x2,
	['0'] = 0x8, 0x8, 0x8, 0x8, 0x8, 0x8, 0x8, 0x8, 0x8, 0x8,
	[';'] = 0x4, ['`'] = 0x2,
};

static bool is_space(char c) { return char_table[(unsigned char) c] & CHAR_SPACE; }
static bool is_digit(char c) { return char_table[(unsigned char) c] & CHAR_DIGIT; }
static bool is_ident(char c) { return !(char_table[(unsigned char) c]
		& (CHAR_SPACE | CHAR_COMMENT | CHAR_SPECIAL)); }

/** Skips whitespace and comments. */
static void skip_whitespace(const char **s) {
	for (const char *x = *s; ;) {
		if (*x == ';') { do ++x; while (*x != '\n' && *x); continue; }
		if (!is_space(*x)) { *s = x; break; }
		do ++x; while (is_space(*x));
	}
}

static enum LispReadError read_int(const char **s, LispObject *result) {
	bool is_neg = false;
	switch (**s) {
	case '-': is_neg = true; [[fallthrough]];
	case '+': ++*s;
	}
	if (!is_digit(**s)) return LISP_READ_TRAILING;
	const char *beg = *s;
	uint32_t i = **s - '0', d;
	while ((d = *++*s - '0') <= 9) i = 10 * i + d;
	if (is_ident(**s)) return LISP_READ_TRAILING;
	// TODO Precise overflow check
	if (*s - beg > INT31_SAFE_DIG) return LISP_READ_INT_TOO_LARGE;
	*result = TAG_SMI(is_neg ? -i : i);
	return LISP_READ_OK;
}

static bool read_prefix(struct LispCtx *ctx, const char **s, LispObject *result) {
	switch (**s) {
	case '\'': ++*s; *result = LISP_CONST(ctx, fquote); return true;
	case '`': ++*s; *result = LISP_CONST(ctx, fquasiquote); return true;
	case ',':
		*result = *++*s == '@'
			? (++*s, LISP_CONST(ctx, funquoteSplicing))
			: LISP_CONST(ctx, funquote);
		return true;
	default: return false;
	}
}

#define TYPE_BITS 3
// Type is ORed with parent length shifted by TYPE_BITS
enum ContainerType {
	CTN_LIST = 0 << 1,
	CTN_DOTTED = 1 << 1,
	CTN_PREFIX = 2 << 1,
};

enum LispReadError lisp_read(struct LispCtx *ctx, const char **s, LispObject *result) {
	uintptr_t *p = ctx->bp, *p0 = p;
	size_t len = 0;

val_beg:
	skip_whitespace(s);
val_beg_no_ws:
	LispObject value;
	if (**s == '(') {
		++*s;
		skip_whitespace(s);
		if (UNLIKELY(**s == ')')) { ++*s; value = NIL(ctx); goto val_end; }
		*p++ = len << TYPE_BITS | CTN_LIST;
		len = 0;
		goto val_beg_no_ws;
	} else if (read_prefix(ctx, s, p)) {
		++p;
		*p++ = len << TYPE_BITS | CTN_PREFIX;
		len = 0;
		goto val_beg;
	}
	const char *beg = *s;
	enum LispReadError err;
	if ((err = read_int(s, &value))) {
		if (UNLIKELY(err != LISP_READ_TRAILING)) return err;
		while (is_ident(**s)) ++*s;
		if (UNLIKELY(*s == beg)) return p > p0 ? LISP_READ_EOF : LISP_READ_EMPTY;
		value = intern(ctx, *s - beg, beg);
	}
val_end:
	if (p == p0) { *result = value; return LISP_READ_OK; } // No remaining nesting
	uintptr_t *ctn = p - ++len;
	enum ContainerType ctn_ty = *ctn % (1 << TYPE_BITS);
	if (ctn_ty == CTN_PREFIX) {
		len = *--p >> TYPE_BITS;
		value = cons(ctx, *--p, cons(ctx, value, NIL(ctx)));
		goto val_end;
	}
	*p++ = value;

	skip_whitespace(s);
	if (**s == ')') {
		++*s;
		value = ctn_ty == CTN_DOTTED ? --len, --p, value : NIL(ctx);
		do value = cons(ctx, *--p, value); while (--len);
		len = *--p >> TYPE_BITS; // Pop container from stack
		goto val_end;
	} else if (UNLIKELY(ctn_ty == CTN_DOTTED)) return LISP_READ_EXPECTED_RPAREN;
	else if (**s == '.') { ++*s; *ctn |= CTN_DOTTED; goto val_beg; }
	goto val_beg_no_ws;
}

enum LispReadError lisp_read_whole(struct LispCtx *ctx, const char *s, LispObject *result) {
	enum LispReadError err;
	return (err = lisp_read(ctx, &s, result)) ? err
		: lisp_read(ctx, &s, result) == LISP_READ_EMPTY ? LISP_READ_OK
		: LISP_READ_TRAILING;
}
