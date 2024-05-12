/** Lisp reader. */

#include <stddef.h>
#include "lisp.h"

/** Character type lookup table. */
static enum CharType : unsigned char {
	CHAR_SPACE = 1,
	CHAR_SPECIAL = 1 << 1,
	CHAR_COMMENT = 1 << 2,
	CHAR_DIGIT = 1 << 3,
} char_table[256] = {
	0x02, 0, 0, 0, 0, 0, 0, 0,
	0, /* \t */ 0x1, /* \n */ 0x1, 0, 0, /* \r */ 0x1, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	/* \s */ 0x1, 0, 0, 0, 0, 0, 0, /* ' */ 0x2,
	/* ( */ 0x2,  /* ) */ 0x2, 0, 0, /* , */ 0x2, 0, /* . */ 0x2, 0,
	/* 0 */ 0x8, 0x8, 0x8, 0x8, 0x8, 0x8, 0x8, 0x8,
	0x8, 0x8, 0, /* ; */ 0x4, 0, 0, 0, 0,
	/* @ */ 0x2, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0
};

static bool is_digit(char c) { return char_table[(unsigned char) c] & CHAR_DIGIT; }
static bool is_ident(char c) { return !(char_table[(unsigned char) c]
		& (CHAR_SPACE | CHAR_COMMENT | CHAR_SPECIAL)); }

/** Skips whitespace and comments. */
static void skip_whitespace(const char **s) {
	for (const char *x = *s; ;) {
		if (*x == ';') { ++x; while (*x != '\n' && *x) ++x; continue; }
		if (!(char_table[(unsigned char) *x] & CHAR_SPACE)) { *s = x; break; }
		++x;
		while (char_table[(unsigned char) *x] & CHAR_SPACE) ++x;
	}
}

static LispObject read_integer(const char **s) {
	int sign = 1;
	switch (**s) {
	case '-': sign = -1; [[fallthrough]];
	case '+': ++*s;
	}
	if (!is_digit(**s)) return NULL;
	int result = 0;
	do result = 10 * result + (**s - '0'); while (is_digit(*++*s));
	return is_ident(**s) ? NULL : lisp_integer(sign * result);
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
		if (UNLIKELY(**s == ')')) { ++*s; value = NULL; goto val_end; }
		*p++ = (union StackElement) { .tag = len << TYPE_BITS | CTN_LIST };
		len = 0;
		goto val_beg_no_ws;
	} else if (**s == '\'') {
		++*s;
		*p++ = (union StackElement) { .value = ctx->fquote };
		*p++ = (union StackElement) { .tag = len << TYPE_BITS | CTN_PREFIX };
		len = 0;
		goto val_beg;
	} else if ((value = read_integer(s))) ; else {
		while (is_ident(**s)) ++*s;
		if (UNLIKELY(*s == start)) return LISP_READ_EOF;
		value = intern(ctx, *s - start, start); // Read a symbol
	}
val_end:
	if (p == stack) { *result = value; return LISP_READ_OK; } // No remaining nesting
	union StackElement *ctn = p - ++len;
	enum ContainerType ctn_ty = ctn->tag & ((1 << TYPE_BITS) - 1);
	if (ctn_ty == CTN_PREFIX) {
		len = (--p)->tag >> TYPE_BITS;
		value = cons((--p)->value, cons(value, NULL));
		goto val_end;
	}
	p++->value = value;

	skip_whitespace(s);
	if (**s == ')') {
		++*s;
		value = ctn_ty == CTN_DOTTED ? --len, --p, value : NULL;
		do value = cons((--p)->value, value); while (--len);
		len = (--p)->tag >> TYPE_BITS; // Pop container from stack
		goto val_end;
	} else if (UNLIKELY(ctn_ty == CTN_DOTTED)) return LISP_READ_EXPECTED_RPAREN;
	else if (**s == '.') { ++*s; ctn->tag |= CTN_DOTTED; goto val_beg; }
	goto val_beg_no_ws;
}

enum LispReadError lisp_read_whole(struct LispCtx *ctx, const char *s, LispObject *result) {
	enum LispReadError error;
	if ((error = lisp_read(ctx, &s, result))) return error;
	skip_whitespace(&s);
	return *s == '\0' ? LISP_READ_OK : LISP_READ_TRAILING;
}
