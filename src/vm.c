/** Register-based bytecode virtual machine and single-pass compiler. */

#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <string.h>
#include "lisp.h"
#include "util.h"

/** Bytecode operation code. */
enum Op : uint8_t {
	RET,
	LOAD_NIL, ///< R(A) <- NULL
	LOAD_OBJ, ///< R(A) <- K(B)
	LOAD_SHORT, ///< R(A) <- (int16_t) B
	GETGLOBAL,
	SETGLOBAL,
	GETUPVALUE,
	SETUPVALUE,
	/// R(A) <- R(A)(R(A+1), ..., R(A+C))
	CALL,
	TAIL_CALL,
	MOV, ///< R(A) <- R(C)
	JMP,
	JNIL, ///< Conditional jump.
	CLOS,
	CLOSE_UPVALS, ///< Close stack variables up to R(A).
};

/** Byte-code instruction. */
struct Instruction {
	enum Op op;
	uint8_t a; ///< Operand 1.
	union {
		uint16_t b; ///< Operand 2.
		struct { uint8_t c, d; }; // Smaller operands 2 and 3.
	};
};

#define PROTO_VARIADIC 0x80
/// Flag signifying that the upvalue captures a local instead of an upvalue.
#define UPVALUE_LOCAL 0x80

/** Lisp closure prototype. */
struct Prototype {
	uint8_t arity, num_upvalues;
	union {
		LispObject *consts;
		size_t next;
	};
	struct Instruction body[];
};

/** Block of bytecode instructions. */
struct Chunk {
	size_t count;
	uint16_t num_consts;
	/// Array of #num_consts constants, followed by #count instructions.
	alignas(LispObject) alignas(struct Instruction) char data[];
};

static_assert(sizeof(LispObject) % alignof(struct Instruction) == 0);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-align"
static LispObject *chunk_constants(struct Chunk *chunk) {
	return (LispObject *) chunk->data;
}
static struct Instruction *chunk_instructions(struct Chunk *chunk) {
	return (struct Instruction *) (chunk_constants(chunk) + chunk->num_consts);
}
#pragma GCC diagnostic pop

static size_t chunk_size(void *x) {
	struct Chunk *chunk = x;
	return sizeof *chunk + chunk->num_consts * sizeof(LispObject)
		+ chunk->count * sizeof(struct Instruction);
}
static void forward_prototype_consts(LispObject *consts, struct Instruction *p, struct Instruction *end) {
	for  (; p < end; ++p) if (p->op == CLOS) {
			struct Prototype *proto = (struct Prototype *) (p + 1);
			proto->consts = consts;
			size_t metadata_size = proto->num_upvalues * sizeof(uint8_t) + sizeof *p - 1;
			p += p->b;
			forward_prototype_consts(consts, proto->body, p - metadata_size / sizeof *p);
	}
}
static void chunk_trace(struct GcHeap *heap, void *x) {
	struct Chunk *chunk = x;
	gc_mark(chunk_size(x), x);
	for (LispObject *p = chunk_constants(chunk), *end = p + chunk->num_consts;
			p < end; ++p) gc_trace(heap, p);
	struct Instruction *xs = chunk_instructions(chunk);
	forward_prototype_consts(chunk_constants(chunk), xs, xs + chunk->count);
}
static struct GcTypeInfo chunk_tib = { chunk_trace, chunk_size };

static void disassemble_range(struct Chunk *chunk, size_t n, struct Instruction xs[static n], int indent) {
	LispObject *consts = chunk_constants(chunk);
	for (size_t i = 0; i < n; ++i) {
		struct Instruction x = xs[i];
		printf("%*s%.4zu ", indent, "", i);
		switch (x.op) {
		case RET: printf("RET %" PRIu8 "\n", x.a); break;
		case LOAD_NIL: printf("LOAD_NIL %" PRIu8 " <- NIL\n", x.a); break;
		case LOAD_OBJ: printf("LOAD_OBJ %" PRIu8 " <- %p\n", x.a, consts[x.b]); break;
		case LOAD_SHORT: printf("LOAD_SHORT %" PRIu8 " <- %" PRIi16 "\n", x.a, (int16_t) x.b); break;
		case GETGLOBAL: printf("GETGLOBAL %" PRIu8 " <- [%s]\n", x.a,
			((struct Symbol *) consts[x.b])->name); break;
		case SETGLOBAL: printf("SETGLOBAL %" PRIu8 " -> [%s]\n", x.a,
			((struct Symbol *) consts[x.b])->name); break;
		case GETUPVALUE: printf("GETUPVALUE %" PRIu8 " <- %" PRIu8 "\n", x.a, x.c); break;
		case SETUPVALUE: printf("SETUPVALUE %" PRIu8 " -> %" PRIu8 "\n", x.a, x.c); break;
		case CALL: case TAIL_CALL:
			printf("%sCALL %" PRIu8 " <- (%" PRIu8,
				x.op == TAIL_CALL ? "TAIL_" : "", x.a, x.a);
			for (unsigned i = 0; i < x.c; ++i) printf(" %" PRIu8, x.a + 2 + i);
			puts(")");
			break;
		case MOV: printf("MOV %" PRIu8 " <- %" PRIu8 "\n", x.a, x.c); break;
		case JMP: printf("JMP => %.4zu\n", i + x.b); break;
		case JNIL: printf("JMP if %" PRIu8 " == NIL => %.4zu\n", x.a, i + x.b); break;
		case CLOS:
			struct Prototype *proto = (struct Prototype *) (xs + i + 1);
			printf("CLOS %" PRIu8 " <- (arity: %" PRIu8 ") (num_upvals: %" PRIu8 "):\n",
				x.a, proto->arity, proto->num_upvalues);
			size_t metadata_size = sizeof *proto + proto->num_upvalues * sizeof(uint8_t) + sizeof x - 1;
			disassemble_range(chunk, x.b - metadata_size / sizeof x, proto->body, indent + 2);
			i += x.b;
			break;
		case CLOSE_UPVALS: printf("CLOSE_UPVALS >= %" PRIu8 "\n", x.a); break;
		default: __builtin_unreachable();
		}
	}
}
static void disassemble(struct Chunk *chunk) {
	puts("Disassembling chunk:");
	disassemble_range(chunk, chunk->count, chunk_instructions(chunk), 0);
}

struct Upvalue {
	LispObject *location;
	// Either the closed over LispObject or the next element in the
	// list of unclosed upvalues sorted by stack locations.
	void *ptr;
};

static size_t upvalue_size(void *) { return sizeof(struct Upvalue); }
static void upvalue_trace(struct GcHeap *heap, void *x) {
	struct Upvalue *upvalue = x;
	gc_mark(sizeof *upvalue, x);
	gc_trace(heap, &upvalue->ptr);
}
static struct GcTypeInfo upvalue_tib = { upvalue_trace, upvalue_size };

struct Closure {
	struct Prototype *prototype;
	struct Upvalue *upvalues[];
};

static size_t closure_size(void *x) {
	struct Closure *closure = x;
	return sizeof *closure
		+ closure->prototype->num_upvalues * sizeof *closure->upvalues;
}
static void closure_trace(struct GcHeap *heap, void *x) {
	struct Closure *closure = x;
	gc_mark(closure_size(x), x);

	char *chunk = (char *) closure->prototype->consts - offsetof(struct Chunk, data);
	size_t prototype_offset = (char *) closure->prototype - chunk;
	gc_trace(heap, (void **) &chunk);
	// Update prototype as chunk may have moved
	closure->prototype = (struct Prototype *) (chunk + prototype_offset);

	for (unsigned i = 0; i < closure->prototype->num_upvalues; ++i)
		gc_trace(heap, (void **) (closure->upvalues + i));
}
static struct LispTypeInfo closure_tib = {
	.gc_tib = { closure_trace, closure_size }, .tag = LISP_CLOSURE,
};

static struct Upvalue *capture_upvalue(struct Upvalue **upvalues, LispObject *local) {
	struct Upvalue *prev = NULL, *x = *upvalues;
	while (x && x->location > local) { prev = x; x = x->ptr; }
	if (x && x->location == local) return x;

	struct Upvalue *new = gc_alloc(heap, sizeof *new, &upvalue_tib);
	*new = (struct Upvalue) { .location = local, .ptr = x };
	return *(prev ? (struct Upvalue **) &prev->ptr : upvalues) = new;
}

union SsaInstruction {
	struct {
		uint8_t op;
	};
};

#define MAX_TRACE_LEN 512

struct TraceRecording {
	struct Closure *start;
	union SsaInstruction trace[MAX_TRACE_LEN];
	unsigned count;
};

[[gnu::optimize ("-fnon-call-exceptions")]]
static LispObject run(struct LispCtx *ctx, LispObject *consts, struct Instruction *pc) {
	LispObject *bp = ctx->bp;
	struct Upvalue *upvalues = NULL;
	uint8_t hotcounts[64] = {};
	bool is_recording = false;
	struct TraceRecording recording = {};

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
	static void *normal_dispatch_table[] = {
		[RET] = &&op_ret,
		[LOAD_NIL] = &&op_load_nil,
		[LOAD_OBJ] = &&op_load_obj,
		[LOAD_SHORT] = &&op_load_short,
		[GETGLOBAL] = &&op_getglobal, [SETGLOBAL] = &&op_setglobal,
		[GETUPVALUE] = &&op_getupvalue, [SETUPVALUE] = &&op_setupvalue,
		[CALL] = &&op_call, [TAIL_CALL] = &&op_tail_call,
		[MOV] = &&op_mov,
		[JMP] = &&op_jmp,
		[JNIL] = &&op_jnil,
		[CLOS] = &&op_clos,
		[CLOSE_UPVALS] = &&op_close_upvals,
	}, *recording_dispatch_table[] = {
		[RET] = &&op_ret,
		[LOAD_NIL] = &&op_load_nil,
		[LOAD_OBJ] = &&op_load_obj,
		[LOAD_SHORT] = &&op_load_short,
		[GETGLOBAL] = &&op_getglobal, [SETGLOBAL] = &&op_setglobal,
		[GETUPVALUE] = &&op_getupvalue, [SETUPVALUE] = &&op_setupvalue,
		[CALL] = &&op_call, [TAIL_CALL] = &&op_tail_call,
		[MOV] = &&op_mov,
		[JMP] = &&op_jmp,
		[JNIL] = &&op_jnil,
		[CLOS] = &&op_clos,
		[CLOSE_UPVALS] = &&op_close_upvals,
	};

	void **dispatch_table = normal_dispatch_table;
	struct Instruction ins;
	// Use token-threading to be able to swap dispatch table when recording
#define CONTINUE goto *dispatch_table[(ins = *pc++).op]
	CONTINUE;

op_ret:
	if (!(pc = bp[1])) return ins.a[ctx->bp = bp];
	*bp = bp[ins.a]; // Copy return value to R(A) of CALL instruction
	bp -= pc[-1].a; // Operand A of the CALL was the base pointer offset
	consts = ((struct Closure *) *bp)->prototype->consts;
	CONTINUE;
op_load_nil: bp[ins.a] = NULL; CONTINUE;
op_load_obj: bp[ins.a] = consts[ins.b]; CONTINUE;
op_load_short: bp[ins.a] = lisp_integer((int16_t) ins.b); CONTINUE;
op_getglobal: bp[ins.a] = ((struct Symbol *) consts[ins.b])->value; CONTINUE;
op_setglobal: ((struct Symbol *) consts[ins.b])->value = bp[ins.a]; CONTINUE;
op_getupvalue:
	bp[ins.a] = *((struct Closure *) *bp)->upvalues[ins.c]->location;
	CONTINUE;
op_setupvalue:
	*((struct Closure *) *bp)->upvalues[ins.c]->location = bp[ins.a];
	CONTINUE;
op_call: op_tail_call:
	LispObject *vals = bp + ins.a;
	switch (lisp_type(*vals)) {
	case LISP_FUNCTION:
		struct Subr *subr = ((struct Function *) *vals)->subr;
		if (ins.c != subr->min_args) die("Too few arguments");
		1[ctx->bp = vals] = pc; // Synchronize bp and link call-frames
		LispObject *args = vals + 2;
		switch (subr->min_args) {
		case 0: *vals = subr->a0(); break;
		case 1: *vals = subr->a1(*args); break;
		case 2: *vals = subr->a2(*args, args[1]); break;
		case 3: *vals = subr->a3(*args, args[1], args[2]); break;
		default: __builtin_unreachable();
		}
		if (ins.op == TAIL_CALL) { *bp = *vals; goto op_ret; }
		break;
	case LISP_CLOSURE:
		struct Closure *closure = *vals;
		struct Prototype *proto = closure->prototype;
		uint8_t nargs = proto->arity & ~PROTO_VARIADIC;
		if (proto->arity & PROTO_VARIADIC) {
			LispObject rest = NULL;
			while (ins.c > nargs) rest = cons(vals[2 + --ins.c], rest);
			vals[2 + ins.c++] = rest;
		}
		if (ins.c != nargs + !!(proto->arity & PROTO_VARIADIC))
			die("Wrong number of arguments");

		struct Instruction *to = proto->body;
		// Increment hotcount
		if (is_recording) {
			if (closure == recording.start) {
				printf("Found loop start!\n");
				is_recording = false;
			}
		} else {
			unsigned hash = ((uintptr_t) pc ^ (uintptr_t) to) / alignof *pc;
			uint8_t *hotcount = hotcounts + hash % LENGTH(hotcounts);
			printf("Incrementing hotcount to %u\n", 1 + *hotcount);
#define JIT_THRESHOLD 3
			if (++*hotcount >= JIT_THRESHOLD) {
				*hotcount = 0;
				// Start recording trace
				dispatch_table = recording_dispatch_table;
				is_recording = true;
				recording.start = closure;
			}
		}

		if (ins.op == TAIL_CALL) {
			*bp = *vals;
			memmove(bp + 2, vals + 2, ins.c * sizeof *vals);
		} else 1[bp = vals] = pc;
		pc = to;
		consts = proto->consts;
		break;
	default: die("Bad function");
	}
	CONTINUE;
op_mov: bp[ins.a] = bp[ins.c]; CONTINUE;
op_jmp: pc += ins.b; CONTINUE;
op_jnil: if (!bp[ins.a]) pc += ins.b; CONTINUE;
op_clos:
	struct Prototype *proto = (struct Prototype *) pc;
	struct Closure *closure = gc_alloc(heap,
		sizeof *closure + proto->num_upvalues * sizeof *closure->upvalues,
		&closure_tib.gc_tib);
	*closure = (struct Closure) { .prototype = proto };
	// Read upvalues
	uint8_t *indices = (uint8_t *) (pc += ins.b) - proto->num_upvalues;
	for (unsigned i = 0; i < proto->num_upvalues; ++i) {
		uint8_t index = indices[i];
		closure->upvalues[i] = index & UPVALUE_LOCAL
			? capture_upvalue(&upvalues, bp + (index & ~UPVALUE_LOCAL))
			: ((struct Closure *) *bp)->upvalues[index];
	}
	bp[ins.a] = closure;
	CONTINUE;
op_close_upvals:
	while (upvalues && upvalues->location >= bp + ins.a) {
		struct Upvalue *x = upvalues;
		upvalues = x->ptr;
		x->ptr = *x->location;
		x->location = &x->ptr;
	}
	CONTINUE;
#pragma GCC diagnostic pop
}

[[gnu::optimize ("-fnon-call-exceptions")]]
static LispObject apply(struct LispCtx *ctx, LispObject function, uint8_t n, LispObject args[static n + 1]) {
	enum LispObjectType ty = lisp_type(function);
	if (ty != LISP_CLOSURE && ty != LISP_FUNCTION) throw(1);

	if (ty != LISP_CLOSURE) die("TODO Implement apply for native functions");
	struct Closure *closure = function;
	struct Prototype *proto = closure->prototype;

	*ctx->bp = function;
	bool variadic = proto->arity & PROTO_VARIADIC;
	uint8_t m = proto->arity & ~PROTO_VARIADIC;
	memcpy(ctx->bp + 2, args, MIN(n, m) * sizeof *args);

	LispObject xs = args[n];
	if (n < m) while (xs && n < m) ctx->bp[2 + n++] = pop(&xs);
	else while (n > m) xs = cons(args[--n], xs);
	if (n < m || (xs && !variadic)) die("Wrong number of arguments");
	if (variadic) ctx->bp[2 + m] = xs;
	return run(ctx, proto->consts, proto->body);
}

#define MAX_LOCAL_VARS 192
#define MAX_UPVALUES 64

struct ConstantEntry {
	LispObject obj;
	uint16_t slot;
};

static uint64_t constant_hash(struct ConstantEntry x) { return moremur((uintptr_t) x.obj); }

static bool constant_equal(struct ConstantEntry a, struct ConstantEntry b) {
	return a.obj == b.obj;
}

#define NAME constant
#define KEY struct ConstantEntry
#include "tbl.h"

typedef uint8_t Register;

struct Local {
	struct Symbol *symbol;
	Register slot; ///< The register.
	char is_captured;
};

struct FuncState {
	struct FuncState *prev;
	uint8_t prev_num_regs, vars_start, num_upvalues;
	uint8_t upvalues[MAX_UPVALUES];
};

struct ByteCompCtx {
	struct LispCtx *lisp_ctx;
	struct FuncState *fun;
	uint8_t num_regs,
		num_vars;
	struct Local vars[MAX_LOCAL_VARS];
	struct Table constants;
	size_t prototypes;

	size_t count, capacity;
	struct Instruction *ins;
};

enum CompileError {
	COMP_INVALID_FORM = 1,
	COMP_EXPECTED_LIST,
	COMP_INVALID_VARIABLE,
	COMP_EXPECTED_CONSEQUENT,
	COMP_TOO_MANY_CONSTS,
};

static uint8_t resolve_upvalue(struct ByteCompCtx *ctx, struct FuncState *fun, unsigned var) {
	uint8_t upvalue = !fun->prev || var >= fun->prev->vars_start
		? ctx->vars[var].is_captured = true, ctx->vars[var].slot | UPVALUE_LOCAL
		: resolve_upvalue(ctx, fun->prev, var);
	// Check if this closure already has the upvalue
	for (unsigned i = 0; i < fun->num_upvalues; ++i)
		if (fun->upvalues[i] == upvalue) return i;
	// Otherwise, create a new nonlocal upvalue
	fun->upvalues[fun->num_upvalues] = upvalue;
	return fun->num_upvalues++;
}

static struct VarRef {
	enum VarRefType { VAR_LOCAL, VAR_UPVALUE, VAR_GLOBAL } type;
	unsigned slot;
} lookup_var(struct ByteCompCtx *ctx, struct Symbol *sym) {
	for (size_t i = ctx->num_vars; i-- > 0;)
		if (ctx->vars[i].symbol == sym)
			return ctx->fun && i < ctx->fun->vars_start
				// If not a local value, then resolve upvalue
				? (struct VarRef) { VAR_UPVALUE, .slot = resolve_upvalue(ctx, ctx->fun, i) }
				: (struct VarRef) { VAR_LOCAL, .slot = ctx->vars[i].slot };
	return (struct VarRef) { .type = VAR_GLOBAL };
}

static void chunk_reserve(struct ByteCompCtx *ctx, size_t additional) {
	size_t n = ctx->count + additional;
	if (LIKELY(n <= ctx->capacity)) return;
	size_t new_capacity = MAX(ctx->capacity ? 2 * ctx->capacity : 32, n);
	struct Instruction *ins;
	if (!(ins = realloc(ctx->ins, new_capacity * sizeof *ins))) die("malloc failed");
	ctx->ins = ins;
	ctx->capacity = new_capacity;
}

static void emit(struct ByteCompCtx *ctx, struct Instruction ins) {
	chunk_reserve(ctx, 1);
	ctx->ins[ctx->count++] = ins;
}

/** Emit instructions to move variables greater than or equal to @a var_limit to the heap. */
static void emit_close_upvalues(struct ByteCompCtx *ctx, uint16_t vars_start, uint16_t regs_start) {
	for (size_t i = vars_start; i < ctx->num_vars; ++i)
		if (ctx->vars[i].is_captured) {
			emit(ctx, (struct Instruction) { .op = CLOSE_UPVALS, .a = regs_start });
			break;
		}
}

static uint16_t constant_slot(struct ByteCompCtx *ctx, LispObject x) {
	struct ConstantEntry *entry;
	if (!(constant_tbl_entry(&ctx->constants, (struct ConstantEntry) { .obj = x }, &entry))) {
		if (!entry) die("malloc failed");
		if (ctx->constants.len > UINT16_MAX) throw(COMP_TOO_MANY_CONSTS);
		entry->slot = ctx->constants.len - 1;
	}
	return entry->slot;
}

// Provides a limited form of register coalescing.
struct Destination {
	Register reg;
	bool discarded : 1, ///< Whether anything but side-effects will be ignored.
		is_return : 1; ///< Whether the form is in return position.
};

static void emit_load_obj(struct ByteCompCtx *ctx, LispObject x, struct Destination dst) {
	if (dst.discarded) return;
	struct Instruction ins;
	int i;
	if (!x) ins = (struct Instruction) { .op = LOAD_NIL, .a = dst.reg };
	else if (lisp_type(x) == LISP_INTEGER
		&& INT16_MIN <= (i = *(int *) x) && i <= INT16_MAX)
		ins = (struct Instruction) { .op = LOAD_SHORT, .a = dst.reg, .b = i };
	else {
		uint16_t slot = constant_slot(ctx, x);
		ins = (struct Instruction) { .op = LOAD_OBJ, .a = dst.reg, .b = slot };
	}
	emit(ctx, ins);
}

enum CompileResult { COMP_OK, COMP_NORETURN };

static enum CompileResult compile_form(struct ByteCompCtx *ctx, LispObject x, struct Destination dst);

static enum CompileResult compile_progn(struct ByteCompCtx *ctx, LispObject x, struct Destination dst) {
	enum CompileResult res;
	do {
		if (x && !consp(x)) throw(COMP_EXPECTED_LIST);
		LispObject form = pop(&x);
		struct Destination d = dst;
		d.is_return &= !x;
		res = compile_form(ctx, form, d);
	} while (x);
	return res;
}

/** Byte-compiles the form @a x. */
static enum CompileResult compile_form(struct ByteCompCtx *ctx, LispObject x, struct Destination dst) {
	struct LispCtx *lisp_ctx = ctx->lisp_ctx;
	switch (lisp_type(x)) {
	case LISP_NIL: case LISP_INTEGER: emit_load_obj(ctx, x, dst); break;
	case LISP_SYMBOL:
		if (dst.discarded) break;
		struct VarRef var = lookup_var(ctx, x);
		switch (var.type) {
		case VAR_LOCAL:
			if (var.slot != dst.reg)
				emit(ctx, (struct Instruction) { .op = MOV, .a = dst.reg, .c = var.slot });
			break;
		case VAR_UPVALUE:
			emit(ctx, (struct Instruction) { .op = GETUPVALUE, .a = dst.reg, .c = var.slot });
			break;
		case VAR_GLOBAL:
			uint16_t slot = constant_slot(ctx, x);
			emit(ctx, (struct Instruction) { .op = GETGLOBAL, .a = dst.reg, .b = slot });
			break;
		default: __builtin_unreachable();
		}
		break;
	case LISP_FUNCTION: case LISP_CLOSURE: throw(COMP_INVALID_FORM);
	case LISP_PAIR:
		LispObject head = pop(&x);
		if (!listp(x)) throw(COMP_INVALID_FORM);

		if (head == lisp_ctx->fprogn) return compile_progn(ctx, x, dst);
		else if (head == lisp_ctx->fquote) emit_load_obj(ctx, pop(&x), dst);
		else if (head == lisp_ctx->ffn) {
			if (dst.discarded) break;
			LispObject args = pop(&x);
			struct FuncState fun = {
				.prev = ctx->fun,
				.prev_num_regs = ctx->num_regs, .vars_start = ctx->num_vars,
			};
			ctx->fun = &fun;
			ctx->num_regs = 2; // Reserve closure and PC registers

			chunk_reserve(ctx, 1 + (alignof(struct Prototype) - 1 + sizeof(struct Prototype))
				/ sizeof *ctx->ins);
			static_assert(alignof(struct Prototype) % sizeof *ctx->ins == 0);
			while ((ctx->count + 1) * sizeof *ctx->ins % alignof(struct Prototype))
				ctx->ins[ctx->count++] = (struct Instruction) { .op = JMP }; // Align with NOPs
			ctx->ins[ctx->count++] = (struct Instruction) { .op = CLOS, .a = dst.reg };
			size_t proto_beg = ctx->count;
			ctx->count += sizeof(struct Prototype) / sizeof *ctx->ins;

			uint8_t num_args = 0;
			while (args) {
				LispObject sym;
				if (consp(args)) { sym = pop(&args); ++num_args; }
				else { sym = args; args = NULL; num_args |= PROTO_VARIADIC; }
				if (lisp_type(sym) != LISP_SYMBOL) throw(COMP_INVALID_VARIABLE);
				ctx->vars[ctx->num_vars++]
					= (struct Local) { .symbol = sym, .slot = ctx->num_regs++ };
			}

			Register reg = ctx->num_regs++; // Return register
			if (!compile_progn(ctx, x, (struct Destination) { .reg = reg, .is_return = true })) {
				emit_close_upvalues(ctx, fun.vars_start, 0);
				emit(ctx, (struct Instruction) { .op = RET, .a = reg });
			}

			*(struct Prototype *) (ctx->ins + proto_beg) = (struct Prototype) {
				.arity = num_args, .num_upvalues = fun.num_upvalues,
				.next = ctx->prototypes,
			};
			ctx->prototypes = proto_beg;

			size_t upvalues_size = fun.num_upvalues * sizeof *fun.upvalues,
				data_size = (upvalues_size + sizeof *ctx->ins - 1) / sizeof *ctx->ins;
			chunk_reserve(ctx, data_size);
			memcpy((char *) (ctx->ins + (ctx->count += data_size)) - upvalues_size,
				fun.upvalues, fun.num_upvalues * sizeof *fun.upvalues); // Write upvalues
			ctx->ins[proto_beg - 1].b = ctx->count - proto_beg; // Write prototype size

			ctx->fun = fun.prev;
			ctx->num_regs = fun.prev_num_regs;
			ctx->num_vars = fun.vars_start;
		} else if (head == lisp_ctx->flet) {
			uint8_t prev_num_regs = ctx->num_regs, prev_num_vars = ctx->num_vars;
			LispObject vars = pop(&x);
			while (vars) {
				LispObject def = pop(&vars), sym, init;
				if (consp(def)) { sym = pop(&def); init = pop(&def); }
				else { sym = def; init = NULL; }
				Register reg = ctx->num_regs++;
				ctx->vars[ctx->num_vars++] = (struct Local) { .symbol = sym, .slot = reg };
				compile_form(ctx, init, (struct Destination) { .reg = reg });
			}
			if (compile_progn(ctx, x, dst)) return COMP_NORETURN;
			emit_close_upvalues(ctx, prev_num_vars, prev_num_regs);
			ctx->num_regs = prev_num_regs;
			ctx->num_vars = prev_num_vars;
		} else if (head == lisp_ctx->fset) {
			LispObject var = pop(&x), value = pop(&x);
			if (lisp_type(var) != LISP_SYMBOL) throw(COMP_INVALID_VARIABLE);
			struct Symbol *sym = (struct Symbol *) var;
			struct VarRef v = lookup_var(ctx, sym);
			if (dst.discarded && v.type == VAR_LOCAL)
				return compile_form(ctx, value, (struct Destination) { .reg = v.slot });
			compile_form(ctx, value, (struct Destination) { .reg = dst.reg });
			switch (v.type) {
			case VAR_LOCAL:
				if (v.slot != dst.reg)
					emit(ctx, (struct Instruction) { .op = MOV, .a = v.slot, .c = dst.reg });
				break;
			case VAR_UPVALUE:
				emit(ctx, (struct Instruction) { .op = SETUPVALUE, .a = dst.reg, .c = v.slot });
				break;
			case VAR_GLOBAL:
				uint16_t slot = constant_slot(ctx, var);
				emit(ctx, (struct Instruction) { .op = SETGLOBAL, .a = dst.reg, .b = slot });
				break;
			default: __builtin_unreachable();
			}
		} else if (head == lisp_ctx->fif) {
			// Emit condition
			compile_form(ctx, pop(&x), (struct Destination) { .reg = dst.reg });
			size_t jmp = ctx->count;
			emit(ctx, (struct Instruction) { .op = JNIL, .a = dst.reg });
			if (!x) throw(COMP_EXPECTED_CONSEQUENT);
			bool noreturn = compile_form(ctx, pop(&x), dst); // Emit consequent
			if (x) { // Emit alternative
				emit(ctx, (struct Instruction) { .op = JMP });
				ctx->ins[jmp].b = ctx->count - (jmp + 1);
				jmp = ctx->count - 1;
				noreturn &= compile_progn(ctx, x, dst);
			} else noreturn = false;
			ctx->ins[jmp].b = ctx->count - (jmp + 1);
			if (noreturn) return COMP_NORETURN;
		} else if (lisp_type(head) == LISP_SYMBOL
			&& car(((struct Symbol *) head)->value) == lisp_ctx->smacro) {
			LispObject macro = ((struct LispPair *) ((struct Symbol *) head)->value)->cdr;
			return compile_form(ctx, apply(ctx->lisp_ctx, macro, 0, &x), dst);
		} else { // Function call
			uint8_t prev_num_regs = ctx->num_regs, num_args = 0;
			Register reg = dst.reg == ctx->num_regs - 1 ? dst.reg : ctx->num_regs++;
			++ctx->num_regs; // Reserve register for PC
			while (x) {
				if (!consp(x)) throw(COMP_EXPECTED_LIST);
				++num_args;
				compile_form(ctx, pop(&x), (struct Destination) { .reg = ctx->num_regs++ });
			}
			compile_form(ctx, head, (struct Destination) { .reg = reg });
			ctx->num_regs = prev_num_regs;

			if (dst.is_return) {
				// Close upvalues before tail-call, since there is no opportunity later
				emit_close_upvalues(ctx, ctx->fun ? ctx->fun->vars_start : 0, 0);
				emit(ctx, (struct Instruction) { .op = TAIL_CALL, .a = reg, .c = num_args });
				return COMP_NORETURN;
			} else {
				emit(ctx, (struct Instruction) { .op = CALL, .a = reg, .c = num_args, });
				if (reg != dst.reg && !dst.discarded)
					emit(ctx, (struct Instruction) { .op = MOV, .a = dst.reg, .c = reg });
			}
		}
		break;
	default: __builtin_unreachable();
	}
	return COMP_OK;
}

static struct Chunk *compile(struct LispCtx *lisp_ctx, LispObject form) {
	struct ByteCompCtx ctx = {
		.lisp_ctx = lisp_ctx,
		.num_regs = 3, // Reserve return, closure and PC registers
	};
	ctx.constants = tbl_new();
	if (!compile_form(&ctx, form, (struct Destination) { .reg = 2, .is_return = true }))
		emit(&ctx, (struct Instruction) { .op = RET, .a = 2 });

	struct Chunk *chunk = gc_alloc(heap, sizeof *chunk
		+ ctx.constants.len * sizeof(LispObject) + ctx.count * sizeof *ctx.ins,
		&chunk_tib);
	chunk->count = ctx.count;
	chunk->num_consts = ctx.constants.len;
	LispObject *consts = chunk_constants(chunk);
	struct ConstantEntry *constant;
	for (size_t i = 0; constant_tbl_iter_next(&ctx.constants, &i, &constant);)
		consts[constant->slot] = constant->obj;
	memcpy(chunk_instructions(chunk), ctx.ins, ctx.count * sizeof *ctx.ins);
	// Patch prototype constants pointers
	for (size_t i = ctx.prototypes, next; i; i = next) {
		struct Prototype *proto = (struct Prototype *) (chunk_instructions(chunk) + i);
		next = proto->next;
		proto->consts = consts;
	}

	constant_tbl_free(&ctx.constants);
	free(ctx.ins);
	return chunk;
}

LispObject lisp_eval(struct LispCtx *ctx, LispObject form) {
	struct Chunk *chunk = compile(ctx, form);
	disassemble(chunk);
	LispObject *consts = chunk_constants(chunk);
	// Store dummy closure in first call frame to handle returns uniformly
	*ctx->bp = &(struct Closure) { .prototype = &(struct Prototype) { .consts = consts } };
	return run(ctx, consts, chunk_instructions(chunk));
}
