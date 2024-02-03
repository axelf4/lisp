/** @file
 * Lisp interpreter.
 *
 * The virtual machine maintains a stack of frames, each of the form:
 *
 *     v-- BP
 *     +----+----+----     ----
 *     | A  | B  | x₀  ...  xₙ ...
 *     +----+----+----     ----
 *
 * where
 * - If A is a correctly aligned pointer, then A is the closure that
 *   was called with arguments in slots x₀,...,xₙ, and B is the return
 *   address, or NULL for the first frame.
 * - (Exception handlers, etc. will eventually also use stack frames
 *   disambiguated via pointer tagging of A.)
 *
 * Stack overflows are detected by setting up (memory protected) guard
 * pages after the allocated stack. The base pointer (BP), which
 * points to the topmost frame, is kept in a register while in the VM,
 * and only synchronized with @ref LispContext.bp due to:
 * - Pushing or popping an exception handler.
 * - Entering a native function, which may recursively call @ref
 *   ::lisp_eval.
 */

#ifndef LISP_H
#define LISP_H

#include <signal.h>
#include "gc.h"
#include "tbl.h"

extern struct GcHeap *heap;

enum LispObjectType {
	LISP_NIL,
	LISP_PAIR,
	LISP_SYMBOL,
	LISP_FUNCTION,
	LISP_CLOSURE,
	LISP_INTEGER,
};

struct LispTypeInfo {
	struct GcTypeInfo gc_tib;
	enum LispObjectType tag;
};

typedef void *LispObject;

static inline enum LispObjectType lisp_type(LispObject p) {
	if (!p) return LISP_NIL;
	struct LispTypeInfo *tib
		= (struct LispTypeInfo *) ((struct GcObjectHeader *) p - 1)->tib;
	return tib->tag;
}

struct Symbol {
	size_t len; ///< Name length (excluding NULL terminator).
	const char *name; ///< NULL-terminated name string.
	LispObject value;
};

struct LispCtx {
	struct Table symbol_tbl;
	// Common interned symbols
	struct Symbol *ffn, *fif, *flet, *fset, *fprogn, *fquote, *smacro;
	LispObject *bp; ///< Base pointer.
	uintptr_t guard_end;
};

struct Subr {
	union {
		LispObject (*a0)();
		LispObject (*a1)(LispObject);
		LispObject (*a2)(LispObject, LispObject);
		LispObject (*a3)(LispObject, LispObject, LispObject);
	};
	const char *name;
	unsigned char min_args;
	struct Subr *next;
};

struct Function { struct Subr *subr; };

/** Cons cell. */
struct LispPair {
	LispObject car, cdr;
};

LispObject cons(LispObject car, LispObject cdr);

LispObject intern(struct LispCtx *ctx, size_t len, const char s[static len]);

LispObject lisp_integer(int i);

enum LispReadError {
	LISP_READ_OK,
	/// End of file during parsing.
	LISP_READ_EOF,
	LISP_READ_EXPECTED_RPAREN,
	LISP_READ_TRAILING,
};

enum LispReadError lisp_read(struct LispCtx *ctx, const char **s, LispObject *result);

enum LispReadError lisp_read_whole(struct LispCtx *ctx, const char *s, LispObject *result);

/** Evaluates @a form. */
LispObject lisp_eval(struct LispCtx *ctx, LispObject form);

void lisp_print(LispObject object);

/** Returns whether @a a and @a b are structurally equal. */
bool lisp_eq(LispObject a, LispObject b);

struct LispCtx *lisp_new();

void lisp_free(struct LispCtx *);

/** Lisp VM signal handler to consult before user application signal handling.
 *
 * This routine recognizes SIGSEGV signals.
 *
 * @return Whether the signal was handled.
 */
[[gnu::cold]] bool lisp_signal_handler(int sig, siginfo_t *info, void *ucontext,
	struct LispCtx *ctx);

static inline bool consp(LispObject x) { return lisp_type(x) == LISP_PAIR; }

static inline bool listp(LispObject x) { return !x || consp(x); }

static inline LispObject car(LispObject x) {
	return consp(x) ? ((struct LispPair *) x)->car : NULL;
}

static inline LispObject pop(LispObject *x) {
	if (!consp(*x)) return NULL;
	struct LispPair *cell = *x, *result = cell->car;
	*x = cell->cdr;
	return result;
}

#endif
