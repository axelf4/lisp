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

/** Block of bytecode instructions. */
struct Chunk {
	size_t count;
	uint16_t num_consts;
	/**
	 * Array of #num_consts constants, followed by #count bytecode
	 * instructions.
	 */
	alignas(LispObject **) char data[];
};
static_assert(alignof(LispObject **) >= alignof(struct Instruction));

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-align"
static LispObject **chunk_constants(struct Chunk *chunk) {
	return (LispObject **) chunk->data;
}
static struct Instruction *chunk_instructions(struct Chunk *chunk) {
	return (struct Instruction *) (chunk_constants(chunk) + chunk->num_consts);
}
#pragma GCC diagnostic pop

static size_t chunk_size(void *x) {
	struct Chunk *chunk = x;
	return sizeof *chunk + chunk->num_consts * sizeof(LispObject *)
		+ chunk->count * sizeof(struct Instruction);
}
static void chunk_trace(struct GcHeap *heap, void *x) {
	struct Chunk *chunk = x;
	gc_mark(chunk_size(x), x);
	for (LispObject **consts = chunk_constants(chunk), **x = consts;
			x < consts + chunk->num_consts; ++x) gc_trace(heap, x);
}
static struct GcTypeInfo chunk_tib = { chunk_trace, chunk_size };

/// Flag signifying that the upvalue captures a local instead of an upvalue.
#define UPVALUE_LOCAL 0x80

/** Lisp closure prototype. */
struct Prototype {
	struct Chunk *chunk;
	size_t offset, // Offset into the chunk.
		len;
	uint8_t arity,
		num_upvalues;
	uint8_t upvalues[];
};

static size_t prototype_size(void *x) {
	struct Prototype *prototype = x;
	return sizeof *prototype + prototype->num_upvalues * sizeof *prototype->upvalues;
}
static void prototype_trace(struct GcHeap *, void *x) { gc_mark(prototype_size(x), x); }
static struct GcTypeInfo prototype_tib = { prototype_trace, prototype_size };

static void disassemble(struct Chunk *chunk, const char *name) {
	printf("Disassembling chunk '%s':\n", name);
	LispObject **consts = chunk_constants(chunk);
	struct Instruction *xs = chunk_instructions(chunk);
	for (size_t i = 0; i < chunk->count;) {
		printf("%.4lu ", i);
		struct Instruction ins = xs[i++];
		switch (ins.op) {
		case RET: printf("RET %" PRIu8 "\n", ins.a); break;
		case LOAD_NIL: printf("LOAD_NIL %" PRIu8 " <- NIL\n", ins.a); break;
		case LOAD_OBJ: printf("LOAD_OBJ %" PRIu8 " <- %p\n", ins.a, consts[ins.b]); break;
		case LOAD_SHORT: printf("LOAD_SHORT %" PRIu8 " <- %d\n", ins.a, (int16_t) ins.b); break;
		case GETGLOBAL: printf("GETGLOBAL %" PRIu8 " <- [%s]\n", ins.a,
			((struct Symbol *) consts[ins.b])->name); break;
		case SETGLOBAL: printf("SETGLOBAL %" PRIu8 " -> [%s]\n", ins.a,
			((struct Symbol *) consts[ins.b])->name); break;
		case GETUPVALUE: printf("GETUPVALUE %" PRIu8 " <- %u\n", ins.a, ins.c); break;
		case SETUPVALUE: printf("SETUPVALUE %" PRIu8 " -> %u\n", ins.a, ins.c); break;
		case CALL: case TAIL_CALL:
			printf("%sCALL %" PRIu8 " <- (%d", ins.op == TAIL_CALL ? "TAIL_" : "", ins.a, ins.a);
			for (unsigned i = 0; i < ins.c; ++i) printf(" %" PRIu8, ins.a + 2 + i);
			puts(")");
			break;
		case MOV: printf("MOV %" PRIu8 " <- %" PRIu8 "\n", ins.a, ins.c); break;
		case JMP: printf("JMP => %.4lu\n", i + ins.b); break;
		case JNIL: printf("JMP if %" PRIu8 " == NIL => %.4lu\n", ins.a, i + ins.b); break;
		case CLOS:
			struct Prototype *proto = consts[ins.b];
			printf("CLOS %" PRIu8 " <- (arity: %d) (num_upvals: %d) (len: %lu):\n",
				ins.a, proto->arity, proto->num_upvalues, proto->len);
			break;
		case CLOSE_UPVALS: printf("CLOSE_UPVALS >= %" PRIu8 "\n", ins.a); break;
		default: printf("Unknown opcode: %" PRIu8 "\n", ins.op); break;
		}
	}
}

struct Upvalue {
	LispObject **location;
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
	for (unsigned i = 0; i < closure->prototype->num_upvalues; ++i)
		gc_trace(heap, (void **) (closure->upvalues + i));
}
static struct LispTypeInfo closure_tib = {
	.gc_tib = { closure_trace, closure_size }, .tag = LISP_CLOSURE,
};

static struct Upvalue *capture_upvalue(struct Upvalue **upvalues, LispObject **local) {
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

static LispObject *run(struct Chunk *chunk) {
	LispObject *stack[512], **bp = stack, **consts = chunk_constants(chunk);
	// Store dummy closure in first call frame to handle returns uniformly
	*stack = (LispObject *) &(struct Closure) {
		.prototype = &(struct Prototype) { .chunk = chunk }
	};
	stack[1] = NULL; // No return address for first call frame
	struct Upvalue *upvalues = NULL;

	disassemble(chunk, "my chunk");

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
	struct Instruction *pc = chunk_instructions(chunk), ins;
	// Use token-threading to be able to swap dispatch table when recording
#define CONTINUE do { ins = *pc++; goto *dispatch_table[ins.op]; } while (0)
	CONTINUE;

op_ret:
	if (!(pc = bp[1])) return bp[ins.a];
	*bp = bp[ins.a]; // Copy return value to R(A) of CALL instruction
	bp -= pc[-1].a; // Operand A of the CALL was the base pointer offset
	consts = chunk_constants(((struct Closure *) *bp)->prototype->chunk);
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
	LispObject **vals = bp + ins.a;
	switch (lisp_type(*vals)) {
	case LISP_FUNCTION:
		struct Subr *subr = ((struct Function *) *vals)->subr;
		if (ins.c != subr->min_args) die("Too few arguments");
		LispObject **args = vals + 2;
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
		if (ins.c != proto->arity) die("Wrong number of arguments");

		struct Instruction *to = chunk_instructions(proto->chunk) + proto->offset;
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
		} else (bp = vals)[1] = pc;
		pc = to;
		consts = chunk_constants(proto->chunk);
		break;
	default: die("Bad function");
	}
	CONTINUE;
op_mov: bp[ins.a] = bp[ins.c]; CONTINUE;
op_jmp: pc += ins.b; CONTINUE;
op_jnil: if (!bp[ins.a]) pc += ins.b; CONTINUE;
op_clos:
	struct Prototype *proto = consts[ins.b];
	struct Closure *closure
		= gc_alloc(heap, sizeof *closure + proto->num_upvalues * sizeof *closure->upvalues,
			&closure_tib.gc_tib);
	*closure = (struct Closure) { .prototype = proto };
	// Read upvalues
	for (unsigned i = 0; i < proto->num_upvalues; ++i) {
		uint8_t index = proto->upvalues[i];
		closure->upvalues[i] = (index & UPVALUE_LOCAL)
			? capture_upvalue(&upvalues, bp + (index & ~UPVALUE_LOCAL))
			: ((struct Closure *) *bp)->upvalues[index];
	}
	bp[ins.a] = closure;
	pc += proto->len;
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

#define MAX_LOCAL_VARS 192
#define MAX_UPVALUES 64

struct ConstantEntry {
	LispObject *obj;
	uint16_t slot,
		is_prototype;
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
	struct LispContext *lisp_ctx;
	struct FuncState *fun;
	uint8_t num_regs,
		num_vars;
	struct Local vars[MAX_LOCAL_VARS];
	uint16_t num_constants;
	struct Table constants;

	size_t count, capacity;
	struct Instruction *ins;
};

enum CompileError {
	COMP_OK,
	COMP_NORETURN,
	COMP_INVALID_FORM,
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

static void emit(struct ByteCompCtx *ctx, struct Instruction ins) {
	if (__builtin_expect(ctx->count >= ctx->capacity, false)) {
		size_t new_capacity = ctx->capacity ? 2 * ctx->capacity : 32;
		struct Instruction *ins;
		if (!(ins = realloc(ctx->ins, new_capacity * sizeof *ins))) die("malloc failed");
		ctx->ins = ins;
		ctx->capacity = new_capacity;
	}

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

static size_t length(LispObject *x) {
	size_t result = 0;
	while (lisp_type(x) == LISP_CONS) { ++result; pop(&x); }
	return result;
}

static bool maybe_eval_macro(struct ByteCompCtx *ctx, struct Symbol *sym, LispObject *args, LispObject **out) {
	if (!(lisp_type(sym->value) == LISP_CONS
			&& ((struct Cons *) sym->value)->car == ctx->lisp_ctx->smacro)) return false;
	LispObject *macro = ((struct Cons *) sym->value)->cdr;

	size_t argc = length(args), num_consts = 1 + argc, num_ins = 2 + argc;
	struct Chunk *chunk;
	char data[sizeof *chunk
		+ num_consts * sizeof(LispObject *) + num_ins * sizeof(struct Instruction)];
	chunk = (void *) data;
	chunk->num_consts = num_consts;
	chunk->count = num_ins;

	LispObject **consts = chunk_constants(chunk);
	struct Instruction *ins = chunk_instructions(chunk);
	unsigned i = 0;
	*ins++ = (struct Instruction) { .op = LOAD_OBJ, .a = 2 + i, .b = i };
	consts[i++] = macro;
	while (args) {
		*ins++ = (struct Instruction) { .op = LOAD_OBJ, .a = 2 + i, .b = i };
		consts[i++] = pop(&args);
	}
	*ins++ = (struct Instruction) { .op = TAIL_CALL, .a = 2, .c = argc };

	*out = run(chunk);
	return true;
}

static enum CompileError constant_slot(struct ByteCompCtx *ctx, LispObject *x, uint16_t *slot) {
	struct ConstantEntry *entry;
	if (!(constant_tbl_entry(&ctx->constants, (struct ConstantEntry) { .obj = x }, &entry))) {
		if (!entry) die("malloc failed");
		if (ctx->constants.len > UINT16_MAX) return COMP_TOO_MANY_CONSTS;
		entry->slot = ctx->constants.len - 1;
	}
	*slot = entry->slot;
	return COMP_OK;
}

// Provides a limited form of register coalescing.
struct Destination {
	Register reg;
	bool discarded : 1, ///< Whether anything but side-effects will be ignored.
		is_return : 1; ///< Whether the form is in return position.
};

static enum CompileError emit_load_obj(struct ByteCompCtx *ctx, LispObject *x, struct Destination dst) {
	if (dst.discarded) return COMP_OK;
	int i;
	struct Instruction ins;
	if (!x) ins = (struct Instruction) { .op = LOAD_NIL, .a = dst.reg };
	else if (lisp_type(x) == LISP_INTEGER
		&& INT16_MIN <= (i = *(int *) x) && i <= INT16_MAX)
		ins = (struct Instruction) { .op = LOAD_SHORT, .a = dst.reg, .b = i };
	else {
		uint16_t slot;
		enum CompileError err;
		if ((err = constant_slot(ctx, x, &slot))) return err;
		ins = (struct Instruction) { .op = LOAD_OBJ, .a = dst.reg, .b = slot };
	}
	emit(ctx, ins);
	return COMP_OK;
}

static enum CompileError compile_form(struct ByteCompCtx *ctx, LispObject *x, struct Destination dst);

static enum CompileError compile_progn(struct ByteCompCtx *ctx, LispObject *x, struct Destination dst) {
	if (!x) return emit_load_obj(ctx, NULL, dst);
	while (x) {
		if (lisp_type(x) != LISP_CONS) return COMP_EXPECTED_LIST;
		LispObject *form = pop(&x);
		struct Destination d = dst;
		d.is_return &= !x;
		enum CompileError err;
		if ((err = compile_form(ctx, form, d))) return err;
	}
	return COMP_OK;
}

/** Byte-compiles the form @a x. */
static enum CompileError compile_form(struct ByteCompCtx *ctx, LispObject *x, struct Destination dst) {
	struct LispContext *lisp_ctx = ctx->lisp_ctx;
	enum CompileError err;
	switch (lisp_type(x)) {
	case LISP_NIL: return emit_load_obj(ctx, NULL, dst);
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
			uint16_t slot;
			if ((err = constant_slot(ctx, x, &slot))) return err;
			emit(ctx, (struct Instruction) { .op = GETGLOBAL, .a = dst.reg, .b = slot });
			break;
		default: __builtin_unreachable();
		}
		break;
	case LISP_INTEGER: return emit_load_obj(ctx, x, dst);
	case LISP_FUNCTION: return COMP_INVALID_FORM;
	case LISP_CONS:
		LispObject *head = pop(&x);
		if (!listp(x)) return COMP_INVALID_FORM;

		if (head == lisp_ctx->fprogn) return compile_progn(ctx, x, dst);
		else if (head == lisp_ctx->fquote) return emit_load_obj(ctx, pop(&x), dst);
		else if (head == lisp_ctx->flambda) {
			if (dst.discarded) break;
			LispObject *args = pop(&x);
			struct FuncState fun = {
				.prev = ctx->fun,
				.prev_num_regs = ctx->num_regs, .vars_start = ctx->num_vars,
			};
			ctx->fun = &fun;
			ctx->num_regs = 2; // Reserve closure and PC registers

			size_t num_args = 0;
			while (args) {
				LispObject *sym = pop(&args);
				if (lisp_type(sym) != LISP_SYMBOL) return COMP_INVALID_VARIABLE;
				++num_args;
				ctx->vars[ctx->num_vars++]
					= (struct Local) { .symbol = sym, .slot = ctx->num_regs++ };
			}

			size_t closure_pos = ctx->count;
			emit(ctx, (struct Instruction) { .op = CLOS, .a = dst.reg });
			Register reg = ctx->num_regs++; // Return register
			err = compile_progn(ctx, x, (struct Destination) { .reg = reg, .is_return = true });
			if (!err) {
				emit_close_upvalues(ctx, fun.vars_start, 0);
				emit(ctx, (struct Instruction) { .op = RET, .a = reg });
			} else if (err != COMP_NORETURN) return err;

			struct Prototype *prototype = gc_alloc(heap,
				sizeof *prototype + fun.num_upvalues * sizeof *prototype->upvalues,
				&prototype_tib);
			uint16_t prototype_idx = ctx->constants.len;
			if (ctx->constants.len >= UINT16_MAX) return COMP_TOO_MANY_CONSTS;
			struct ConstantEntry *entry;
			constant_tbl_entry(&ctx->constants, (struct ConstantEntry) {
					.obj = (LispObject *) prototype,
					.slot = prototype_idx,
					.is_prototype = true,
				}, &entry);
			ctx->ins[closure_pos].b = prototype_idx;
			*prototype = (struct Prototype) {
				.offset = closure_pos + 1, .len = ctx->count - (closure_pos + 1),
				.arity = num_args,
				.num_upvalues = fun.num_upvalues,
			};
			memcpy(prototype->upvalues, fun.upvalues,
				fun.num_upvalues * sizeof *fun.upvalues); // Write upvalues

			ctx->fun = fun.prev;
			ctx->num_regs = fun.prev_num_regs;
			ctx->num_vars = fun.vars_start;
		} else if (head == lisp_ctx->flet) {
			size_t prev_num_regs = ctx->num_regs, prev_num_vars = ctx->num_vars;
			LispObject *vars = pop(&x);
			while (vars) {
				LispObject *def = pop(&vars), *sym, *init;
				if (consp(def)) { sym = pop(&def); init = pop(&def); }
				else { sym = def; init = NULL; }
				Register reg = ctx->num_regs++;
				ctx->vars[ctx->num_vars++] = (struct Local) { .symbol = sym, .slot = reg };
				compile_form(ctx, init, (struct Destination) { .reg = reg });
			}
			if ((err = compile_progn(ctx, x, dst))) return err;
			emit_close_upvalues(ctx, prev_num_vars, prev_num_regs);
			ctx->num_regs = prev_num_regs;
			ctx->num_vars = prev_num_vars;
		} else if (head == lisp_ctx->fset) {
			LispObject *var = pop(&x), *value = pop(&x);
			if (lisp_type(var) != LISP_SYMBOL) return COMP_INVALID_VARIABLE;
			struct Symbol *sym = (struct Symbol *) var;
			struct VarRef v = lookup_var(ctx, sym);
			if (dst.discarded && v.type == VAR_LOCAL)
				return compile_form(ctx, value, (struct Destination) { .reg = v.slot });
			if ((err = compile_form(ctx, value, (struct Destination) { .reg = dst.reg })))
				return err;
			switch (v.type) {
			case VAR_LOCAL:
				if (v.slot != dst.reg)
					emit(ctx, (struct Instruction) { .op = MOV, .a = v.slot, .c = dst.reg });
				break;
			case VAR_UPVALUE:
				emit(ctx, (struct Instruction) { .op = SETUPVALUE, .a = dst.reg, .c = v.slot });
				break;
			case VAR_GLOBAL:
				uint16_t slot;
				if ((err = constant_slot(ctx, var, &slot))) return err;
				emit(ctx, (struct Instruction) { .op = SETGLOBAL, .a = dst.reg, .b = slot });
				break;
			default: __builtin_unreachable();
			}
		} else if (head == lisp_ctx->fif) {
			// Emit condition evaluation
			if ((err = compile_form(ctx, pop(&x), (struct Destination) { .reg = dst.reg })))
				return err;
			size_t jmp = ctx->count;
			emit(ctx, (struct Instruction) { .op = JNIL, .a = dst.reg });
			if (!x) return COMP_EXPECTED_CONSEQUENT;
			err = compile_form(ctx, pop(&x), dst); // Emit consequent
			if (!(err == COMP_OK || err == COMP_NORETURN)) return err;
			bool noreturn = err == COMP_NORETURN;
			if (x) { // Emit alternative
				emit(ctx, (struct Instruction) { .op = JMP });
				ctx->ins[jmp].b = ctx->count - (jmp + 1);
				jmp = ctx->count - 1;
				err = compile_progn(ctx, x, dst);
				if (!(err == COMP_OK || err == COMP_NORETURN)) return err;
				noreturn &= err == COMP_NORETURN;
			} else noreturn = false;
			ctx->ins[jmp].b = ctx->count - (jmp + 1);
			if (noreturn) return COMP_NORETURN;
		} else if (maybe_eval_macro(ctx, head, x, &x)) return compile_form(ctx, x, dst);
		else { // Function call
			size_t prev_num_regs = ctx->num_regs, num_args = 0;
			Register reg = dst.reg == ctx->num_regs - 1 ? dst.reg : ctx->num_regs++;
			++ctx->num_regs; // Reserve register for PC
			while (x) {
				if (lisp_type(x) != LISP_CONS) return COMP_EXPECTED_LIST;
				++num_args;
				if ((err = compile_form(ctx, pop(&x),
							(struct Destination) { .reg = ctx->num_regs++ }))) return err;
			}
			if ((err = compile_form(ctx, head, (struct Destination) { .reg = reg })))
				return err;
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

static struct Chunk *compile(struct LispContext *lisp_ctx, LispObject *form) {
	struct ByteCompCtx ctx = {
		.lisp_ctx = lisp_ctx,
		.num_regs = 3, // Reserve return, closure and PC registers
	};
	ctx.constants = tbl_new();
	enum CompileError err
		= compile_form(&ctx, form, (struct Destination) { .reg = 2, .is_return = true });
	if (!err) emit(&ctx, (struct Instruction) { .op = RET, .a = 2 });
	else if (err != COMP_NORETURN) die("Compilation error: %d", err);

	struct Chunk *chunk = gc_alloc(heap, sizeof *chunk
		+ ctx.constants.len * sizeof(LispObject *) + ctx.count * sizeof *ctx.ins,
		&chunk_tib);
	chunk->count = ctx.count;
	chunk->num_consts = ctx.constants.len;
	struct ConstantEntry *constant;
	for (size_t i = 0; constant_tbl_iter_next(&ctx.constants, &i, &constant);) {
		chunk_constants(chunk)[constant->slot] = constant->obj;
		// Patch prototype chunk pointer
		if (constant->is_prototype) ((struct Prototype *) constant->obj)->chunk = chunk;
	}
	memcpy(chunk_instructions(chunk), ctx.ins, ctx.count * sizeof *ctx.ins);

	constant_tbl_free(&ctx.constants);
	free(ctx.ins); // TODO Reuse for next top-level form
	return chunk;
}

LispObject *lisp_eval(struct LispContext *ctx, LispObject *form) {
	struct Chunk *chunk = compile(ctx, form);
	return run(chunk);
}
