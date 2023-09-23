#ifndef LISP_H
#define LISP_H

#include "gc.h"

extern struct Heap *heap;

enum LispObjectType {
	LISP_CONS,
	LISP_INTEGER,
};

struct LispObject {
	enum LispObjectType tag;
};

struct Cons {
	struct LispObject object, *car, *cdr;
};

struct LispObject *cons(struct LispObject *car, struct LispObject *cdr);

struct LispInteger {
	struct LispObject object;
	int i;
};

struct LispObject *lisp_integer(int i);

static inline struct LispObject *intern(size_t len, __attribute__ ((unused)) char s[static len]) {
	// TODO
	return NULL;
}

enum LispReadError {
	LISP_READ_OK,
	/// End of file during parsing.
	LISP_READ_EOF,
	LISP_READ_EXPECTED_RPAREN,
	LISP_READ_TRAILING,
};

enum LispReadError lisp_read(char **s, struct LispObject **result);

enum LispReadError lisp_read_whole(char *s, struct LispObject **result);

void lisp_print(struct LispObject *object);

#endif
