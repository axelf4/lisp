/** @file
 * Lisp interpreter.
 *
 * The virtual machine maintains a stack of frames, each of the form:
 *
 *     v-- BP
 *     +---+---+---     ---
 *     | A | B | x₀ ...  xₙ ...
 *     +---+---+---     ---
 *
 * where
 * - If A is a tagged object pointer, then A is the closure that was
 *   called with arguments in slots x₀,...,xₙ, and B is the return
 *   address, or NULL for the first frame.
 * - (Exception handlers, etc. will eventually also use stack frames
 *   disambiguated via pointer tagging of A.)
 *
 * Stack overflows are detected by setting up (memory protected) guard
 * pages after the allocated stack. The base pointer (BP), which
 * points to the topmost frame, is kept in a register while in the VM,
 * and only synchronized with @ref LispCtx.bp due to:
 * - Pushing or popping an exception handler.
 * - Entering a native function, which may recursively call @ref
 *   ::lisp_eval.
 */

#ifndef LISP_H
#define LISP_H

#include <signal.h>
#include "gc.h"
#include "tbl.h"

#define IS_SMI(x) (!((x) & 1))
#define TAG_SMI(i) ((uintptr_t) (i) << 1)
#define UNTAG_SMI(x) SAR((union { uint32_t u; int32_t i; }) { (x) }.i, 1)
#define TAG_OBJ(p) ((uintptr_t) (p) + 1)
#define UNTAG_OBJ(x) ((void *) ((x) - 1))

#define NIL TAG_OBJ(NULL)
#define NILP(x) ((uint32_t) (x) == 1)

enum LispObjectType : unsigned char {
	LISP_PAIR,
	LISP_SYMBOL,
	LISP_STRING,
	LISP_CFUNCTION,
	LISP_CLOSURE,
	LISP_UPVALUE,
	LISP_BYTECODE_CHUNK,
	LISP_NIL,
	LISP_INTEGER,
};

struct LispObjectHeader {
	struct GcObjectHeader hdr;
	enum LispObjectType tag;
};

typedef uintptr_t LispObject;
typedef struct GcRef Lobj;

static inline enum LispObjectType lisp_type(LispObject p) {
	if (NILP(p)) return LISP_NIL;
	if (IS_SMI(p)) return LISP_INTEGER;
	return ((struct LispObjectHeader *) UNTAG_OBJ(p))->tag;
}

struct Symbol {
	alignas(GC_MIN_ALIGNMENT) struct LispObjectHeader hdr;
	size_t len; ///< Name length (excluding NULL terminator).
	const char *name; ///< NULL-terminated name string.
	LispObject value;
};

/** X-macro for the interned symbol constants. */
#define FOR_SYMBOL_CONSTS(X) \
	X(ffn, fn) \
	X(fif, if) \
	X(flet, let) \
	X(fset, set) \
	X(fquote, quote) \
	X(t, t)

struct LispCtx {
	struct Table symbol_tbl;
	uintptr_t *bp, ///< Base pointer.
		guard_end;

#ifndef LISP_GENERATED_FILE
#define X(var, _) LispObject var;
	FOR_SYMBOL_CONSTS(X)
#undef X
#endif
};

#ifdef LISP_GENERATED_FILE
#include LISP_GENERATED_FILE
#else
#define LISP_CONST(ctx, name) (ctx)->name
#define LISP_CONST_COMPRESSED(ctx, name) GC_COMPRESS(LISP_CONST((ctx), name))
#endif

struct LispCFunction {
	alignas(GC_MIN_ALIGNMENT) struct LispObjectHeader hdr;
	unsigned char nargs;
	LispObject (*f)(struct LispCtx *, uint8_t n, const LispObject args[static n]);
	const char *name;
};

/** Cons cell. */
struct LispPair {
	alignas(GC_MIN_ALIGNMENT) struct LispObjectHeader hdr;
	Lobj car, cdr;
};

LispObject cons(struct LispCtx *ctx, LispObject car, LispObject cdr);

LispObject intern(struct LispCtx *ctx, size_t len, const char s[static len]);

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

void lisp_print(struct LispCtx *ctx, LispObject object);

/** Returns whether @a a and @a b are structurally equal. */
bool lisp_eq(struct LispCtx *ctx, LispObject a, LispObject b);

/** Lisp VM signal handler to consult before user application signal handling.
 *
 * This routine recognizes SIGSEGV signals.
 *
 * @return Whether the signal was handled.
 */
[[gnu::cold]] bool lisp_signal_handler(int sig, siginfo_t *info, void *ucontext,
	struct LispCtx *ctx);

bool lisp_init(struct LispCtx *);

void lisp_free(struct LispCtx *);

static inline bool consp(LispObject x) { return lisp_type(x) == LISP_PAIR; }

static inline bool listp(LispObject x) { return NILP(x) || consp(x); }

static inline LispObject car(struct LispCtx *ctx, LispObject x) {
	return consp(x) ? GC_DECOMPRESS(ctx, ((struct LispPair *) UNTAG_OBJ(x))->car) : NIL;
}

static inline LispObject cdr(struct LispCtx *ctx, LispObject x) {
	return consp(x) ? GC_DECOMPRESS(ctx, ((struct LispPair *) UNTAG_OBJ(x))->cdr) : NIL;
}

static inline LispObject pop(struct LispCtx *ctx, LispObject *x) {
	if (!consp(*x)) return NIL;
	struct LispPair *cell = UNTAG_OBJ(*x);
	*x = GC_DECOMPRESS(ctx, cell->cdr);
	return GC_DECOMPRESS(ctx, cell->car);
}

#endif
