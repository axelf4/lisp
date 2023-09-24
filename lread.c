#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "lisp.h"

static bool is_whitespace(char c) {
	return c == ' ' || c == '\t' || c == '\t' || c == '\r';
}

static bool is_integer(char c) { return '0' <= c && c <= '9'; }

/** Skip whitespace and comments. */
static void skip_whitespace(char **s) {
	for (;;) {
		if (**s == ';') while (*++*s != '\n') ;
		if (!is_whitespace(**s)) break;
		while (is_whitespace(*++*s)) ;
	}
}

static int read_integer(char **s) {
	int sign = 1;
	switch (**s) {
	case '-': sign = -1; // Intentional fall through
	case '+': ++*s;
	}
	int result = 0;
	while ('0' <= **s && **s <= '9')
		result = 10 * result + (*(*s)++ - '0');
	return sign * result;
}

static struct LispObject *read_symbol(char **s) {
	char *start = *s;
	for (;; ++*s)
		switch (**s) {
		case '\0': case '(': case ')': case '.': goto done;
		default:
			if (is_whitespace(**s)) goto done;
			break;
		}
done: return intern(*s - start, start);
}

union StackElement {
	LispObject *object;
	// Container entry
	struct {
		union StackElement *prev_container;
		unsigned len;
		bool is_dotted_pair;
	};
};

enum LispReadError lisp_read(char **s, LispObject **result) {
	union StackElement stack[256], *cur = stack, *ctn = NULL;

	skip_whitespace(s);

	struct LispObject *value;
val_beg:
	if (**s == '(') { ++*s; goto list_beg; }
	if (is_integer(**s) || **s == '-') value = lisp_integer(read_integer(s));
	else if (__builtin_expect(!**s, false)) return LISP_READ_EOF;
	else value = read_symbol(s);
val_end:
	if (!ctn) { *result = value; return LISP_READ_OK; } // Not in a container context

	(cur++)->object = value;
	++ctn->len;

	skip_whitespace(s);
	if (**s == ')') { ++*s; goto list_end; }
	if (**s == '.') { ++*s; skip_whitespace(s); ctn->is_dotted_pair = true; }
	else if (ctn->is_dotted_pair) return LISP_READ_EXPECTED_RPAREN;
	goto val_beg;

list_beg:
	skip_whitespace(s);
	if (**s == ')') { ++*s; value = NULL; goto val_end; }

	union StackElement *prev_ctn = ctn;
	*(ctn = cur++) = (union StackElement) {
		.prev_container = prev_ctn,
	};
	goto val_beg;

list_end:
	value = ctn->is_dotted_pair ? --ctn->len, (--cur)->object : NULL;
	do value = cons((--cur)->object, value); while (--ctn->len);
	ctn = ctn->prev_container;
	--cur; // Pop container stack element
	goto val_end;
}

enum LispReadError lisp_read_whole(char *s, LispObject **result) {
	enum LispReadError error;
	if ((error = lisp_read(&s, result))) return error;
	if (*s != '\0') return LISP_READ_TRAILING;
	return LISP_READ_OK;
}
