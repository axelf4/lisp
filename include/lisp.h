/** Lisp interpreter. @file */

#ifndef LISP_H
#define LISP_H

#include <stdint.h>
#include <stdio.h>
#include <signal.h>

#define _LISP_CAT(a, b) a ## b
#define LISP_CAT(a, b) _LISP_CAT(a, b)
#define _VA_COUNT(_7, _6, _5, _4, _3, _2, _1, n, ...) n
/** Expands to the number of arguments in the macro invocation. */
#define VA_COUNT(...) _VA_COUNT(__VA_ARGS__ __VA_OPT__(,) 7, 6, 5, 4, 3, 2, 1, 0)

#define LISP_INTERN(ctx, s) lisp_intern(ctx, sizeof(s) - 1, s)

#define _LISP_ARGS_1 ctx
#define _LISP_ARGS_2 _LISP_ARGS_1, *argv
#define _LISP_ARGS_3 _LISP_ARGS_2, argv[1]
#define _LISP_ARGS_4 _LISP_ARGS_3, argv[2]
#define _LISP_ARGS_5 _LISP_ARGS_4, argv[3]
/** Defines a built-in variadic function. @see #DEFUN() */
#define DEFUN_VA(lname, cname)											\
	static LispObject F ## cname(struct LispCtx *ctx, size_t argc, const LispObject *argv); \
	static struct LispSubr S ## cname = {								\
		.hdr.tag = LISP_SUBROUTINE, .f = F ## cname, .name = lname };	\
	static LispObject F ## cname([[maybe_unused]] struct LispCtx *ctx,	\
		[[maybe_unused]] size_t argc, [[maybe_unused]] const LispObject *argv)
/** Defines a built-in function for calling from Lisp.
 *
 * Follow by the function body. Register with #lisp_defsubr(), passing
 * @p cname prefixed by "S" as @p fn.
 *
 * @par Examples
 * ```
 * DEFUN("identity", id, (struct LispCtx *, LispObject x)) { return x; }
 * lisp_defsubr(ctx, &Sid);
 * ```
 *
 * @param lname Name to give the function in Lisp.
 * @param cname Name of the function in C.
 * @param args Parameter list consisting of a LispCtx pointer and zero
 *     or more LispObject arguments.
 */
#define DEFUN(lname, cname, args)									\
	static LispObject _ ## cname args;								\
	DEFUN_VA(lname, cname) {										\
		if (argc != VA_COUNT args - 1) throw(1);					\
		return _ ## cname(LISP_CAT(_LISP_ARGS_, VA_COUNT args));	\
	}																\
	static LispObject _ ## cname args

typedef uintptr_t LispObject; ///< Tagged Lisp value pointer.
typedef struct GcRef Lobj;

/** Lisp object variant. */
enum LispType : unsigned char {
	LISP_PAIR,
	LISP_SYMBOL,
	LISP_STRING,
	LISP_SUBROUTINE,
	LISP_CLOSURE,
	LISP_UPVALUE,
	LISP_BYTECODE,
	LISP_NIL,
	LISP_INTEGER,
};

struct LispObjectHeader {
	struct GcObjectHeader { unsigned char flags; } hdr;
	enum LispType tag;
};

struct LispCtx;

enum LispReadError {
	LISP_READ_OK, ///< Success.
	LISP_READ_EOF, ///< End of file during parsing.
	LISP_READ_EMPTY, ///< Input was empty save for whitespace or comments.
	LISP_READ_EXPECTED_RPAREN, ///< Expected closing parenthesis.
	LISP_READ_INT_TOO_LARGE, ///< Integer literal is too large to be represented.
	LISP_READ_TRAILING, ///< Initial datum followed by stray token(s).
};

enum LispReadError lisp_read(struct LispCtx *ctx, const char **s, LispObject *result);

enum LispReadError lisp_read_whole(struct LispCtx *ctx, const char *s, LispObject *result);

/** Evaluates @a form. */
LispObject lisp_eval(struct LispCtx *ctx, LispObject form);

void lisp_print(struct LispCtx *ctx, LispObject object, FILE *stream);

/** Returns whether @a a and @a b are structurally equal. */
bool lisp_eq(struct LispCtx *ctx, LispObject a, LispObject b);

/** Lisp VM signal handler to consult before user application signal handling.
 *
 * This routine recognizes @c SIGSEGV signals.
 *
 * @return Whether the signal was handled.
 */
bool lisp_signal_handler(int sig, siginfo_t *info, void *ucontext, struct LispCtx *ctx);

[[gnu::cold]] void lisp_interrupt(struct LispCtx *ctx);

[[nodiscard, gnu::malloc]] struct LispCtx *lisp_new();

void lisp_free(struct LispCtx *ctx);

LispObject lisp_str(struct LispCtx *ctx, size_t len, const char s[static len]);

LispObject lisp_intern(struct LispCtx *ctx, size_t len, const char s[static len]);

struct LispSubr {
	struct LispObjectHeader hdr;
	unsigned char jit_id;
	LispObject (*f)(struct LispCtx *, size_t n, const LispObject args[static n]);
	const char *name;
};

/** Defines the symbol for @a fn at start-up time. */
void lisp_defsubr(struct LispCtx *ctx, const struct LispSubr *subr);

#endif
