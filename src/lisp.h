#ifndef LISP_H
#define LISP_H

#include "gc.h"

extern struct Heap *heap;

struct LispContext;

enum LispObjectType {
	LISP_NIL,
	LISP_CONS,
	LISP_SYMBOL,
	LISP_FUNCTION,
	LISP_INTEGER,
};

struct LispTypeInfo {
	struct GcTypeInfo gc_tib;
	enum LispObjectType tag;
};

typedef uintptr_t LispSymbol;
typedef void LispObject;

static inline enum LispObjectType lisp_type(LispObject *p) {
	if (!p) return LISP_NIL;
	struct LispTypeInfo *tib
		= (struct LispTypeInfo *) ((struct GcObjectHeader *) p - 1)->tib;
	return tib->tag;
}

struct Cons {
	LispObject *car, *cdr;
};

LispObject *cons(LispObject *car, LispObject *cdr);

LispObject *lisp_integer(int i);

LispObject *intern(struct LispContext *ctx, size_t len, const char s[static len]);

enum LispReadError {
	LISP_READ_OK,
	/// End of file during parsing.
	LISP_READ_EOF,
	LISP_READ_EXPECTED_RPAREN,
	LISP_READ_TRAILING,
};

enum LispReadError lisp_read(struct LispContext *ctx, const char **s, LispObject **result);

enum LispReadError lisp_read_whole(struct LispContext *ctx, const char *s, LispObject **result);

void lisp_print(LispObject *object);

struct LispContext *lisp_init();

void lisp_free(struct LispContext *);

LispObject *lisp_eval(struct LispContext *ctx, LispObject *x);

#endif
