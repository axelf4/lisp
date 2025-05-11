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
 * and only synchronized with @ref LispCtx.bp when:
 * - Pushing or popping an exception handler.
 * - Entering a native function, which may recursively call
 *   lisp_eval().
 *
 * Closures are implemented using *upvalues*, obviating complex
 * control-flow analysis:
 *
 *             | pending vars.   |
 *     *-*     v         *--v    |
 *     |_|  *-*-*  close | *-*-* |
 *     |_|<-| | |   ==>  *-|x| | |
 *     | |  *-*-*          *-*-* |
 *     stack   v                 v
 */

#ifndef LISP_H
#define LISP_H

#include <signal.h>
#include "gc.h"
#include "tbl.h"

#define IS_SMI(x) (!((x) & 1))
#define TAG_SMI(i) ((uint32_t) (i) << 1)
#define UNTAG_SMI(x) SAR((union { uint32_t u; int32_t i; }) { x }.i, 1)
#define TAG_OBJ(p) ((LispObject) (p) + 1)
#define UNTAG_OBJ(x) ((void *) ((x) - 1))

#define NIL TAG_OBJ(NULL)
#define NILP(x) (GC_COMPRESS(x).p == NIL)

typedef uintptr_t LispObject;
typedef struct GcRef Lobj;

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

static inline enum LispObjectType lisp_type(LispObject p) {
	return NILP(p) ? LISP_NIL
		: IS_SMI(p) ? LISP_INTEGER
		: ((struct LispObjectHeader *) UNTAG_OBJ(p))->tag;
}

/** Interned string with a value slot. */
struct LispSymbol {
	alignas(GC_ALIGNMENT) struct LispObjectHeader hdr;
	unsigned int len; ///< Length of #name (excluding NULL terminator).
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
	X(fquasiquote, quasiquote) \
	X(funquote, unquote) \
	X(funquoteSplicing, unquote-splicing) \
	X(t, t)
/** X-macro for Lisp "keywords". */
#define FOR_KEYWORDS(X) \
	X(QUOTE, fquote) \
	X(FN, ffn) \
	X(IF, fif) \
	X(LET, flet) \
	X(SET, fset)

/** Lisp special form or common function. */
enum LispKeyword {
#define X(name, _) LISP_KW_ ## name,
	FOR_KEYWORDS(X)
#undef X
	LISP_NUM_KEYWORDS,
	LISP_NO_KEYWORD = LISP_NUM_KEYWORDS
};

struct LispTrace;
struct JitState;

struct LispCtx {
	uintptr_t *bp, ///< Base pointer.
		guard_end;
	struct Table symbol_tbl;
	struct Upvalue *upvalues;

#if ENABLE_JIT
	unsigned char hotcounts[64];
	struct JitState *jit_state;
	struct LispTrace *(*traces)[UINT16_MAX], *current_trace;
#endif

#ifndef LISP_GENERATED_FILE
#define X(var, _) LispObject var;
	FOR_SYMBOL_CONSTS(X)
#undef X
#endif
};

#ifdef LISP_GENERATED_FILE
#include "phf.h"
#include LISP_GENERATED_FILE
#else
#define LISP_CONST(ctx, name) (ctx)->name
#define LISP_CONST_COMPRESSED(ctx, name) GC_COMPRESS(LISP_CONST(ctx, name))
#endif

struct LispCFunction {
	alignas(GC_ALIGNMENT) struct LispObjectHeader hdr;
	unsigned char nargs;
	LispObject (*f)(struct LispCtx *, size_t n, const LispObject args[static n]);
	const char *name;
};

/** Cons cell. */
struct LispPair {
	alignas(GC_ALIGNMENT) struct LispObjectHeader hdr;
	Lobj car, cdr;
};

LispObject cons(struct LispCtx *ctx, LispObject car, LispObject cdr);

LispObject intern(struct LispCtx *ctx, size_t len, const char s[static len]);

enum LispReadError {
	LISP_READ_OK,
	/// End of file during parsing.
	LISP_READ_EOF,
	LISP_READ_EMPTY,
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
[[gnu::cold]]
bool lisp_signal_handler(int sig, siginfo_t *info, void *ucontext, struct LispCtx *ctx);

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
	LispObject cell = *x;
	*x = cdr(ctx, cell);
	return car(ctx, cell);
}

#define FOR_OPS(X) \
	X(RET) /* Return R(A) */ \
	X(LOAD_NIL) /* R(A) <- NIL */ \
	X(LOAD_OBJ) /* R(A) <- K(B) */ \
	X(LOAD_SHORT) /* R(A) <- sB */ \
	X(GETGLOBAL) /* R(A) <- G[K[PC, B]] */ \
	X(SETGLOBAL) /* G[K[PC, B]] <- R(A) */ \
	X(GETUPVALUE) /* R(A) <- U[C] */ \
	X(SETUPVALUE) /* U[C] <- R(A) */ \
	X(CALL) /* R(A) <- R(A)(R(A+2), ..., R(A+2+C-1)) */ \
	X(TAIL_CALL) \
	X(TAIL_JIT_CALL) \
	X(CALL_INTERPR) /* Like CALL but blacklisted from being JIT:ed. */ \
	X(TAIL_CALL_INTERPR) \
	X(MOV) /* R(A) <- R(C) */ \
	X(JMP) /* PC += sB */ \
	X(JNIL) /* If NILP(R(A)) then PC += sB */ \
	X(CLOS) \
	X(CLOSE_UPVALS) /* Close stack variables up to R(A). */

/** Bytecode operation code. */
#define X(op) op,
enum Op : uint8_t { FOR_OPS(X) BC_NUM_OPS };
#undef X

/** Bytecode instruction. */
struct Instruction {
	enum Op op;
	uint8_t a; ///< Operand 1.
	union {
		uint16_t b; ///< Operand 2.
		struct { uint8_t c, d; }; // Smaller operands 2 and 3
	};
};

/** Sequence of bytecode instructions. */
struct Chunk {
	alignas(GC_ALIGNMENT) struct LispObjectHeader hdr;
	uint16_t num_consts;
	size_t count;
	/// Array of #num_consts constants, followed by #count instructions.
	alignas(LispObject) alignas(struct Instruction) char data[];
};

#define PROTO_VARIADIC 0x80

/** Lisp closure prototype. */
struct Prototype {
	uint8_t arity, num_upvalues;
	unsigned offset; ///< Byte offset within chunk.
	struct Instruction body[];
};

struct Upvalue {
	alignas(GC_ALIGNMENT) struct LispObjectHeader hdr;
	bool is_closed;
	union {
		struct Upvalue *next; ///< Next unclosed in list sorted by stack locations.
		LispObject value; ///< The closed over object.
	};
	LispObject *location;
};

struct Closure {
	alignas(GC_ALIGNMENT) struct LispObjectHeader hdr;
	struct Prototype *prototype;
	struct Upvalue *upvalues[];
	// TODO Add plist as an alternative to https://zenodo.org/records/6228797
};

static inline LispObject *chunk_constants(struct Chunk *chunk) {
	return (LispObject *) chunk->data;
}
static inline struct Instruction *chunk_instructions(struct Chunk *chunk) {
	return (struct Instruction *) (chunk_constants(chunk) + chunk->num_consts);
}

#define REG_LISP_CTX r15
#define REG_PC rsi

[[gnu::malloc]] struct JitState *jit_new(struct LispCtx *ctx);

void jit_free(struct JitState *state);

bool jit_init(struct JitState *state, struct Closure *f, struct Instruction *pc);

/** Records instruction preceding @a pc prior to it being executed. */
bool jit_record(struct JitState *state, struct Instruction *pc);

uint8_t trace_arity(struct LispTrace *trace);

#endif
