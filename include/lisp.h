/** Lisp interpreter. @file
 *
 * For disambiguating tagged pointers from SMIs the LSB is reserved,
 * and on 64-bit platforms pointer compression (inspired by V8, see
 * https://v8.dev/blog/pointer-compression) is used:
 *
 *                             32-bits      32-bits
 *                 Pointer: |____base____|___offset__1|
 *     SMall Integer (SMI): |************|___int31___0|
 */

#ifndef LISP_H
#define LISP_H

#include <stdint.h>
#include <stdio.h>
#include <signal.h>

#ifdef __cplusplus
extern "C" {
#endif

#if defined(__has_builtin) && __has_builtin(__builtin_assume_aligned)
#define ASSUME_ALIGNED(x, align) __builtin_assume_aligned(x, align)
#else
#define ASSUME_ALIGNED(x, align) ((void *) (x))
#endif

#ifndef USE_COMPRESSED_PTRS
#define USE_COMPRESSED_PTRS __LP64__
#endif

#define _LISP_CAT(a, b) a ## b
#define LISP_CAT(a, b) _LISP_CAT(a, b)
#define _VA_COUNT(_7, _6, _5, _4, _3, _2, _1, n, ...) n
/** Expands to the number of arguments in the macro invocation. */
#define VA_COUNT(...) _VA_COUNT(__VA_ARGS__ __VA_OPT__(,) 7, 6, 5, 4, 3, 2, 1, 0)

#define LISP_IS_SMI(x) (!((x) & 1))
#define LISP_TAG_SMI(i) ((uint32_t)(i) << 1)
#define LISP_UNTAG_SMI(x) ((union { uint32_t u; int32_t i; }){ x }.i / 2)
#define LISP_TAG_OBJ(p) ((LispObject)(p) + 1)
#define LISP_UNTAG_OBJ(x) ((void *)((x) - 1))

#define GC_COMPRESS(p) (struct LispRef){ (uintptr_t)(p) }
#if USE_COMPRESSED_PTRS
#define GC_DECOMPRESS(base, ref) ((uintptr_t)ASSUME_ALIGNED(base, 1ull << 32) + (ref).p)
#else
#define GC_DECOMPRESS(base, ref) ((void)(base), (ref).p)
#endif
#define LISP_ALIGNMENT sizeof(struct LispRef)

/** Returns the nil value. */
#define LISP_NIL(ctx) LISP_TAG_OBJ(ctx)
/** Returns whether @p x is the nil value. */
#if USE_COMPRESSED_PTRS
#define LISP_NILP(ctx, x) ((void)(ctx), GC_COMPRESS(x).p == LISP_TAG_OBJ(0))
#else
#define LISP_NILP(ctx, x) ((x) == NIL(ctx))
#endif

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

/** Compressed #LispObject. */
struct LispRef {
#if USE_COMPRESSED_PTRS
	uint32_t
#else
	LispObject
#endif
	p;
};

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

static inline enum LispType lisp_type(LispObject p) {
	return LISP_IS_SMI(p) ? LISP_INTEGER
		: ((struct LispObjectHeader *)LISP_UNTAG_OBJ(p))->tag;
}

struct LispCtx;

enum LispReadError {
	LISP_READ_OK, ///< Success.
	LISP_READ_EOF, ///< End of file during parsing.
	LISP_READ_EMPTY, ///< Input was empty save for whitespace or comments.
	LISP_READ_EXPECTED_RPAREN, ///< Expected closing parenthesis.
	LISP_READ_INT_TOO_LARGE, ///< Integer literal is too large to be represented.
	LISP_READ_TRAILING, ///< Initial datum followed by stray token(s).
};

/** Reads one Lisp expression as text from @p s. */
enum LispReadError lisp_read(struct LispCtx *ctx, const char **s, LispObject *result);

enum LispReadError lisp_read_whole(struct LispCtx *ctx, const char *s, LispObject *result);

/** Evaluates @a form. */
LispObject lisp_eval(struct LispCtx *ctx, LispObject form);

/** Outputs the printed representation of @p object to @p stream. */
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

struct LispString {
	alignas(LISP_ALIGNMENT) struct LispObjectHeader hdr;
	unsigned len;
	char s[];
};

LispObject lisp_str(struct LispCtx *ctx, size_t len, const char s[static len]);

/** Interned string with a value slot. */
struct LispSymbol {
	alignas(LISP_ALIGNMENT) struct LispObjectHeader hdr;
	unsigned len; ///< Byte length of #name.
	const char *name; ///< Name string (not NULL-terminated).
	LispObject value;
};

/** Returns the canonical symbol whose name is @p s. */
LispObject lisp_intern(struct LispCtx *ctx, size_t len, const char s[static len]);

/** Cons cell. */
struct LispPair {
	alignas(LISP_ALIGNMENT) struct LispObjectHeader hdr;
	struct LispRef car, cdr;
};

/** Constructs a pair with @p car and @p cdr as components. */
LispObject lisp_cons(struct LispCtx *ctx, LispObject car, LispObject cdr);

static inline bool lisp_consp(LispObject x) { return lisp_type(x) == LISP_PAIR; }

/** Projects the first component of the pair @p x. */
static inline LispObject lisp_car(struct LispCtx *ctx, LispObject x) {
	return lisp_consp(x) ? GC_DECOMPRESS(ctx, ((struct LispPair *)LISP_UNTAG_OBJ(x))->car)
		: LISP_NIL(ctx);
}
/** Projects the second component of the pair @p x. */
static inline LispObject lisp_cdr(struct LispCtx *ctx, LispObject x) {
	return lisp_consp(x) ? GC_DECOMPRESS(ctx, ((struct LispPair *)LISP_UNTAG_OBJ(x))->cdr)
		: LISP_NIL(ctx);
}

struct LispSubr {
	struct LispObjectHeader hdr;
	unsigned char jit_id;
	LispObject (*f)(struct LispCtx *, size_t n, const LispObject args[static n]);
	const char *name;
};

/** Defines the symbol for @a fn at start-up time. */
void lisp_defsubr(struct LispCtx *ctx, const struct LispSubr *subr);

#ifdef __cplusplus
}
#endif

#endif
