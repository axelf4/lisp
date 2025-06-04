/** Lisp reader. */

#include <stddef.h>
#include "lisp.h"

/// Maximum number of digits for reading int31_t safely (no overflow).
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
		if (*x == ';') { ++x; while (*x != '\n' && *x) ++x; continue; }
		if (!is_space(*x)) { *s = x; break; }
		++x;
		while (is_space(*x)) ++x;
	}
}

static bool read_integer(const char **s, LispObject *result) {
	bool is_neg = false;
	switch (**s) {
	case '-': is_neg = true; [[fallthrough]];
	case '+': ++*s;
	}
	if (!is_digit(**s)) return false;
	const char *beg = *s;
	uint32_t i = **s - '0', d;
	while ((d = *++*s - '0') <= 9) i = 10 * i + d;
	if (is_ident(**s)) return false;
	if (*s - beg > INT31_SAFE_DIG) throw(1); // TODO Precise overflow check
	*result = TAG_SMI(is_neg ? -i : i);
	return true;
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

#define TYPE_BITS 2

union StackElement {
	enum ContainerType : size_t {
		CTN_LIST,
		CTN_DOTTED,
		CTN_PREFIX,
	} tag; ///< The container type ORed with parent length shifted by TYPE_BITS.
	LispObject value;
};

enum LispReadError lisp_read(struct LispCtx *ctx, const char **s, LispObject *result) {
	union StackElement stack[256], *p = stack;
	size_t len = 0;

val_beg:
	skip_whitespace(s);
val_beg_no_ws:
	LispObject value;
	const char *start = *s;
	if (**s == '(') {
		++*s;
		skip_whitespace(s);
		if (UNLIKELY(**s == ')')) { ++*s; value = NIL(ctx); goto val_end; }
		*p++ = (union StackElement) { .tag = len << TYPE_BITS | CTN_LIST };
		len = 0;
		goto val_beg_no_ws;
	} else if (read_prefix(ctx, s, &p->value)) {
		++p;
		*p++ = (union StackElement) { .tag = len << TYPE_BITS | CTN_PREFIX };
		len = 0;
		goto val_beg;
	} else if (!read_integer(s, &value)) {
		while (is_ident(**s)) ++*s;
		if (UNLIKELY(*s == start))
			return p > stack ? LISP_READ_EOF : LISP_READ_EMPTY;
		value = intern(ctx, *s - start, start); // Read a symbol
	}
val_end:
	if (p == stack) { *result = value; return LISP_READ_OK; } // No remaining nesting
	union StackElement *ctn = p - ++len;
	enum ContainerType ctn_ty = ctn->tag % (1 << TYPE_BITS);
	if (ctn_ty == CTN_PREFIX) {
		len = (--p)->tag >> TYPE_BITS;
		value = cons(ctx, (--p)->value, cons(ctx, value, NIL(ctx)));
		goto val_end;
	}
	p++->value = value;

	skip_whitespace(s);
	if (**s == ')') {
		++*s;
		value = ctn_ty == CTN_DOTTED ? --len, --p, value : NIL(ctx);
		do value = cons(ctx, (--p)->value, value); while (--len);
		len = (--p)->tag >> TYPE_BITS; // Pop container from stack
		goto val_end;
	} else if (UNLIKELY(ctn_ty == CTN_DOTTED)) return LISP_READ_EXPECTED_RPAREN;
	else if (**s == '.') { ++*s; ctn->tag |= CTN_DOTTED; goto val_beg; }
	goto val_beg_no_ws;
}

enum LispReadError lisp_read_whole(struct LispCtx *ctx, const char *s, LispObject *result) {
	enum LispReadError err;
	return (err = lisp_read(ctx, &s, result)) ? err
		: lisp_read(ctx, &s, &(LispObject) {}) == LISP_READ_EMPTY ? LISP_READ_OK
		: LISP_READ_TRAILING;
}
