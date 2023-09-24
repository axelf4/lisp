#ifndef LISP_H
#define LISP_H

#include "gc.h"

extern struct Heap *heap;

enum LispObjectType {
	LISP_NULL,
	LISP_CONS,
	LISP_INTEGER,
};

struct LispTypeInfo {
	struct GcTypeInfo gcTib;
	enum LispObjectType tag;
};

typedef void LispObject;

static inline enum LispObjectType lisp_tag(LispObject *p) {
	if (!p) return LISP_NULL;
	struct LispTypeInfo *tib
		= (struct LispTypeInfo *) ((struct GcObjectHeader *) p - 1)->tib;
	return tib->tag;
}

struct Cons {
	LispObject *car, *cdr;
};

LispObject *cons(LispObject *car, LispObject *cdr);

LispObject *lisp_integer(int i);

static inline LispObject *intern(size_t len, __attribute__ ((unused)) char s[static len]) {
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

enum LispReadError lisp_read(char **s, LispObject **result);

enum LispReadError lisp_read_whole(char *s, LispObject **result);

void lisp_print(LispObject *object);

#endif
