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
	for (const char *x = *s;;) {
		if (*x == ';') { ++x; while (*x++ != '\n') ; continue; }
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

struct StackElement {
	/** For containers: The type ORed with the parent length shifted by TYPE_BITS. */
	enum ContainerType : size_t {
		CTN_LIST,
		CTN_DOTTED,
		CTN_PREFIX,
	} tag;
	union {
		LispObject object;
		struct Symbol *prefix_sym;
	};
};

enum LispReadError lisp_read(struct LispContext *ctx, const char **s, LispObject *result) {
	struct StackElement stack[256], *x = stack;
	size_t len = 0;

val_beg:
	skip_whitespace(s);
val_beg_no_ws:
	LispObject value;
	const char *start = *s;
	if (**s == '(') {
		++*s;
		skip_whitespace(s);
		if (__builtin_expect(**s == ')', false)) { ++*s; value = NULL; goto val_end; }
		*x++ = (struct StackElement) { .tag = len << TYPE_BITS | CTN_LIST };
		len = 0;
		goto val_beg_no_ws;
	} else if (**s == '\'') {
		++*s;
		*x++ = (struct StackElement)
			{ .tag = len << TYPE_BITS | CTN_PREFIX, .prefix_sym = ctx->fquote };
		len = 0;
		goto val_beg;
	} else if ((value = read_integer(s))) ; else {
		while (is_ident(**s)) ++*s;
		if (__builtin_expect(*s == start, false)) return LISP_READ_EOF;
		value = intern(ctx, *s - start, start); // Read a symbol
	}
val_end:
	if (x == stack) { *result = value; return LISP_READ_OK; } // No remaining nesting
	struct StackElement *ctn = x - ++len;
	enum ContainerType ctn_ty = ctn->tag & ((1 << TYPE_BITS) - 1);
	if (ctn_ty == CTN_PREFIX) {
		value = cons(ctn->prefix_sym, cons(value, NULL));
		len = (--x)->tag >> TYPE_BITS;
		goto val_end;
	}
	x++->object = value;

	skip_whitespace(s);
	if (**s == ')') {
		++*s;
		value = ctn_ty == CTN_DOTTED ? --len, --x, value : NULL;
		do value = cons((--x)->object, value); while (--len);
		len = (--x)->tag >> TYPE_BITS; // Pop container from stack
		goto val_end;
	} else if (__builtin_expect(ctn_ty == CTN_DOTTED, false))
		return LISP_READ_EXPECTED_RPAREN;
	else if (**s == '.') { ++*s; ctn->tag |= CTN_DOTTED; goto val_beg; }
	goto val_beg_no_ws;
}

enum LispReadError lisp_read_whole(struct LispContext *ctx, const char *s, LispObject *result) {
	enum LispReadError error;
	if ((error = lisp_read(ctx, &s, result))) return error;
	skip_whitespace(&s);
	return *s == '\0' ? LISP_READ_OK : LISP_READ_TRAILING;
}
