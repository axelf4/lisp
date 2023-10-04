#ifndef LISP_H
#define LISP_H

#include "gc.h"

extern struct GcHeap *heap;

struct LispContext;

enum LispObjectType {
	LISP_NIL,
	LISP_CONS,
	LISP_SYMBOL,
	LISP_FUNCTION,
	LISP_CLOSURE,
	LISP_INTEGER,
};

struct LispTypeInfo {
	struct GcTypeInfo gc_tib;
	enum LispObjectType tag;
};

typedef void LispObject;

static inline enum LispObjectType lisp_type(LispObject *p) {
	if (!p) return LISP_NIL;
	struct LispTypeInfo *tib
		= (struct LispTypeInfo *) ((struct GcObjectHeader *) p - 1)->tib;
	return tib->tag;
}

struct Symbol {
	size_t len;
	const char *name;
	LispObject *value;
};

struct Subr {
	union {
		LispObject *(*a0)();
		LispObject *(*a1)(LispObject *);
		LispObject *(*a2)(LispObject *, LispObject *);
		LispObject *(*a3)(LispObject *, LispObject *, LispObject *);
	};
	const char *name;
	unsigned char min_args;
	struct Subr *next;
};

struct Function { struct Subr *subr; };

struct Cons {
	LispObject *car, *cdr;
};

LispObject *cons(LispObject *car, LispObject *cdr);

LispObject *lisp_integer(int i);

LispObject *intern(struct LispContext *ctx, size_t len, const char s[static len]);

/** Interns a NULL-terminated string. */
LispObject *intern_c_string(struct LispContext *ctx, const char *s);

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

LispObject *lisp_eval(struct LispContext *ctx, LispObject *form);

static inline bool consp(LispObject *x) { return lisp_type(x) == LISP_CONS; }

static inline bool listp(LispObject *x) { return !x || consp(x); }

static inline LispObject *pop(LispObject **x) {
	if (lisp_type(*x) != LISP_CONS) return NULL;
	struct Cons *cell = *x, *result = cell->car;
	*x = cell->cdr;
	return result;
}

#endif
