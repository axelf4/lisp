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
	for (;;) {
		if (**s == ';') while (*(*s)++ != '\n') ;
		if (!(char_table[(unsigned char) **s] & CHAR_SPACE)) break;
		++*s;
		while (char_table[(unsigned char) **s] & CHAR_SPACE) ++*s;
	}
}

static LispObject *read_integer(const char **s) {
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

union StackElement {
	LispObject *object;
	// Container entry
	struct {
		union StackElement *prev_container;
		enum ContainerType {
			CTN_LIST,
			CTN_DOTTED,
			CTN_PREFIX,
		} type;
		union {
			unsigned len;
			struct Symbol *prefix_sym;
		};
	};
};

enum LispReadError lisp_read(struct LispContext *ctx, const char **s, LispObject **result) {
	union StackElement stack[256], *cur = stack, *ctn = NULL;

	skip_whitespace(s);

	struct LispObject *value;
val_beg:
	const char *start = *s;
	if (**s == '(') { ++*s; goto list_beg; }
	if (**s == '\'') {
		++*s;
		skip_whitespace(s);
		union StackElement *prev_ctn = ctn;
		*(ctn = cur++) = (union StackElement)
			{ .prev_container = prev_ctn, .type = CTN_PREFIX, .prefix_sym = ctx->fquote };
		goto val_beg;
	}
	if ((value = read_integer(s))) ;
	else { // Read a symbol
		while (is_ident(**s)) ++*s;
		if (__builtin_expect(*s == start, false)) return LISP_READ_EOF;
		value = intern(ctx, *s - start, start);
	}
val_end:
	if (!ctn) { *result = value; return LISP_READ_OK; } // Not in a container context

	if (ctn->type == CTN_PREFIX) {
		value = cons(ctn->prefix_sym, cons(value, NULL));
		ctn = ctn->prev_container;
		--cur;
		goto val_end;
	}

	(cur++)->object = value;
	++ctn->len;

	skip_whitespace(s);
	if (**s == ')') { ++*s; goto list_end; }
	if (ctn->type == CTN_DOTTED) return LISP_READ_EXPECTED_RPAREN;
	if (**s == '.') { ++*s; skip_whitespace(s); ctn->type = CTN_DOTTED; }
	goto val_beg;

list_beg:
	skip_whitespace(s);
	if (**s == ')') { ++*s; value = NULL; goto val_end; }

	union StackElement *prev_ctn = ctn;
	*(ctn = cur++) = (union StackElement) { .prev_container = prev_ctn };
	goto val_beg;

list_end:
	value = ctn->type == CTN_DOTTED ? --ctn->len, (--cur)->object : NULL;
	do value = cons((--cur)->object, value); while (--ctn->len);
	ctn = ctn->prev_container;
	--cur; // Pop container stack element
	goto val_end;
}

enum LispReadError lisp_read_whole(struct LispContext *ctx, const char *s, LispObject **result) {
	enum LispReadError error;
	if ((error = lisp_read(ctx, &s, result))) return error;
	skip_whitespace(&s);
	return *s == '\0' ? LISP_READ_OK : LISP_READ_TRAILING;
}
