#ifndef LISP_H
#define LISP_H

#include "gc.h"

extern struct Heap *heap;

enum LispObjectType {
	LISP_CONS,
};

struct LispObject {
	enum LispObjectType tag;
};

struct Cons {
	struct LispObject object, *car, *cdr;
};

struct LispObject *cons(struct LispObject *car, struct LispObject *cdr);

#endif
