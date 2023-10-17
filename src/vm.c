/** Register-based bytecode virtual machine and single-pass compiler. */

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <string.h>
#include "lisp.h"
#include "util.h"

/** Operation code. */
enum Op : uint8_t {
	RET,
		/// R(A) <- NULL
		LOAD_NIL,
		LOAD_OBJ,
		LOAD_SHORT,
		GETGLOBAL,
		SETGLOBAL,
		GETUPVALUE,
		SETUPVALUE,
		/// R(A) <- R(A)(R(A+1), ..., R(A+C))
		CALL,
		TAIL_CALL,
		MOV,
		JMP,
		JNIL, ///< Conditional jump.
		CLOS,
		CLOSE_UPVALS, ///< Close stack variables up to R(A).
		};

/** Byte-code instruction. */
union Instruction {
	struct {
		enum Op op;
		uint8_t a; /// Operand 1.
		union {
			uint16_t b; /// Operand 2.
			struct { uint8_t c, d; };
		};
	};
	int32_t i; ///< Integer literal.
	LispObject *v;
};

/** A block of instructions. */
struct Chunk {
	size_t count;
	union Instruction ins[];
};

static size_t chunk_size(void *x) {
	struct Chunk *chunk = x;
	return sizeof *chunk + chunk->count + sizeof *chunk->ins;
}
static void chunk_trace(struct GcHeap *, void *x) { gc_mark(chunk_size(x), x); }
static struct GcTypeInfo chunk_tib = { chunk_trace, chunk_size };

void disassemble(struct Chunk *chunk, const char *name) {
	printf("Disassembling chunk '%s':\n", name);
	for (size_t i = 0; i < chunk->count;) {
		printf("%.4lu ", i);
		union Instruction ins = chunk->ins[i++];

		switch (ins.op) {
		case RET: printf("RET 0\n"); break;
		case LOAD_NIL: printf("LOAD_NIL %d <- NIL\n", ins.a); break;
		case LOAD_OBJ: printf("LOAD_OBJ %d <- %p\n", ins.a, chunk->ins[i++].v); break;
		case LOAD_SHORT: printf("LOAD_SHORT %d <- %d\n", ins.a, (int16_t) ins.b); break;
		case GETGLOBAL: printf("GETGLOBAL %d <- [%s]\n", ins.a,
			((struct Symbol *) chunk->ins[i++].v)->name); break;
		case SETGLOBAL: printf("SETGLOBAL %d -> [%s]\n", ins.a,
			((struct Symbol *) chunk->ins[i++].v)->name); break;
		case GETUPVALUE: printf("GETUPVALUE %d <- %u\n", ins.a, ins.b); break;
		case SETUPVALUE: printf("SETUPVALUE %d -> %u\n", ins.a, ins.b); break;
		case CALL: case TAIL_CALL:
			printf("%sCALL %d <- (%d", ins.op == TAIL_CALL ? "TAIL_" : "", ins.a, ins.a);
			for (unsigned i = 0; i < ins.c; ++i) printf(" %" PRIu8, ins.a + 1 + i);
			puts(")");
			break;
		case MOV: printf("MOV %u <- %d\n", ins.a, ins.b); break;
		case JMP: printf("JMP => %.4lu\n", i + ins.b); break;
		case JNIL: printf("JMP if %u == NIL => %.4lu\n", ins.a, i + ins.b); break;
		case CLOS:
			size_t len = chunk->ins[i++].i;
			printf("CLOS %d <- (arity: %d) (num_upvals: %d) (len: %lu):\n", ins.a, ins.c, ins.d, len);
			break;
		case CLOSE_UPVALS: printf("CLOSE_UPVALS >= %u\n", ins.a); break;
		default: printf("Unknown opcode: %d\n", ins.op); break;
		}
	}
}

struct ObjUpvalue {
	LispObject **location, *closed;
	struct ObjUpvalue *next;
};

static size_t obj_upvalue_size(void *) { return sizeof(struct ObjUpvalue); }
static void obj_upvalue_trace(struct GcHeap *heap, void *x) {
	struct ObjUpvalue *upvalue = x;
	gc_mark(sizeof *upvalue, x);
	if (upvalue->next) gc_trace(heap, (void **) &upvalue->next);
}
static struct GcTypeInfo obj_upvalue_tib = { obj_upvalue_trace, obj_upvalue_size };

struct Closure {
	struct Chunk *chunk;
	size_t offset; // Offset in the chunk.
	uint8_t arity,
		num_upvalues;
	struct ObjUpvalue *upvalues[];
};

static size_t closure_size(void *x) {
	struct Closure *closure = x;
	return sizeof *closure + closure->num_upvalues * sizeof *closure->upvalues;
}
static void closure_trace(struct GcHeap *heap, void *x) {
	struct Closure *closure = x;
	gc_mark(closure_size(x), x);
	for (size_t i = 0; i < closure->num_upvalues; ++i)
		gc_trace(heap, (void **) (closure->upvalues + i));
}
static struct LispTypeInfo closure_tib = {
	.gc_tib = { closure_trace, closure_size }, .tag = LISP_CLOSURE,
};

static struct ObjUpvalue *capture_upvalue(struct ObjUpvalue **upvalues, LispObject **local) {
	struct ObjUpvalue *prev = NULL, *x = *upvalues;
	for (; x && x->location > local; x = x->next) ;
	if (x && x->location == local) return x;

	struct ObjUpvalue *new = gc_alloc(heap, sizeof *new, &obj_upvalue_tib);
	*new = (struct ObjUpvalue) { .location = local, .next = *upvalues };
	return *(prev ? &prev->next : upvalues) = new;
}

struct CallFrame {
	size_t bp;
	union Instruction *pc;
	struct Closure *closure;
};

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
	LispObject *stack_top[256], **stack = stack_top;
	struct CallFrame frames[128] = {};
	unsigned num_frames = 0;
	struct ObjUpvalue *upvalues = NULL;

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
	union Instruction *pc = chunk->ins, ins;
	// Use token-threading to be able to swap dispatch table when recording
#define CONTINUE do { ins = *pc++; goto *dispatch_table[ins.op]; } while (0)
	CONTINUE;

op_ret:
	if (num_frames) {
		pc = frames[--num_frames].pc;
		stack = stack_top + (num_frames ? frames[num_frames - 1].bp : 0);
		CONTINUE;
	} else return *stack;
op_load_nil: stack[ins.a] = NULL; CONTINUE;
op_load_obj: stack[ins.a] = pc++->v; CONTINUE;
op_load_short: stack[ins.a] = lisp_integer((int16_t) ins.b); CONTINUE;
op_getglobal: stack[ins.a] = ((struct Symbol *) pc++->v)->value; CONTINUE;
op_setglobal: ((struct Symbol *) pc++->v)->value = stack[ins.a]; CONTINUE;
op_getupvalue:
	stack[ins.a] = *frames[num_frames - 1].closure->upvalues[ins.b]->location;
	CONTINUE;
op_setupvalue:
	*frames[num_frames - 1].closure->upvalues[ins.b]->location = stack[ins.a];
	CONTINUE;
op_call: op_tail_call:
	LispObject **vals = stack + ins.a;
	switch (lisp_type(*vals)) {
	case LISP_FUNCTION:
		struct Subr *subr = ((struct Function *) *vals)->subr;
		if (ins.c != subr->min_args) die("Too few arguments");
		LispObject **args = vals + 1;
		switch (subr->min_args) {
		case 0: *vals = subr->a0(); break;
		case 1: *vals = subr->a1(*args); break;
		case 2: *vals = subr->a2(*args, args[1]); break;
		case 3: *vals = subr->a3(*args, args[1], args[2]); break;
		default: die("Bad min_args");
		}
		if (ins.op == TAIL_CALL) { *stack = *vals; goto op_ret; }
		break;
	case LISP_CLOSURE:
		struct Closure *closure = *vals;
		if (ins.c != closure->arity) die("Wrong number of arguments");

		union Instruction *to = closure->chunk->ins + closure->offset;
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
			if (vals != stack)
				memcpy(stack + 1, vals + 1, ins.c * sizeof *vals);
			frames[num_frames - 1].closure = closure;
		} else {
			frames[num_frames++] = (struct CallFrame) {
				.pc = pc, .bp = vals - stack_top, .closure = closure,
			};
			stack = vals;
		}
		pc = to;
		break;
	default: die("Bad function");
	}
	CONTINUE;
op_mov: stack[ins.a] = stack[ins.b]; CONTINUE;
op_jmp: pc += ins.b; CONTINUE;
op_jnil: if (!stack[ins.a]) pc += ins.b; CONTINUE;
op_clos:
	size_t len = pc++->i, num_upvalues = ins.d;
	struct Closure *closure
		= gc_alloc(heap, sizeof *closure + num_upvalues * sizeof *closure->upvalues,
			&closure_tib.gc_tib);
	*closure = (struct Closure) {
		.chunk = chunk, .offset = pc - chunk->ins,
		.arity = ins.c, .num_upvalues = num_upvalues,
	};
	pc += len;
	// Read upvalues
	for (unsigned i = 0; i < num_upvalues; ++i) {
		union Instruction ins = *pc++;
		uint8_t is_local = ins.a, index = ins.b;
		struct CallFrame *frame = num_frames ? frames + num_frames - 1 : NULL;
		closure->upvalues[i] = is_local
			? capture_upvalue(&upvalues, stack_top + (frame ? frame->bp : 0) + index)
			: frame->closure->upvalues[index];
	}
	stack[ins.a] = closure;
	CONTINUE;
op_close_upvals:
	while (upvalues && upvalues->location >= stack + ins.a) {
		struct ObjUpvalue *x = upvalues;
		x->closed = *x->location;
		x->location = &x->closed;
		upvalues = x->next;
		x->next = NULL;
	}
	CONTINUE;
#pragma GCC diagnostic pop
}

#define MAX_LOCAL_VARS 192
#define MAX_UPVALUES 64

typedef uint8_t Register;

struct Local {
	struct Symbol *symbol;
	Register slot; ///< The register.
	char is_captured;
};

struct Upvalue {
	bool is_local : 1; ///< Whether this upvalue captures a local or an upvalue.
	uint8_t index;
};

struct FuncState {
	struct FuncState *prev;
	uint8_t prev_num_regs, vars_start, num_upvalues;
	struct Upvalue upvalues[MAX_UPVALUES];
};

struct ByteCompCtx {
	struct LispContext *lisp_ctx;
	struct FuncState *fun;
	uint8_t num_regs,
		num_vars;
	struct Local vars[MAX_LOCAL_VARS];

	struct Symbol *flambda, *fif, *flet, *fset, *fprogn, *fquote, *smacro;

	size_t count, capacity;
	union Instruction *ins;
};

static size_t resolve_upvalue(struct ByteCompCtx *ctx, struct FuncState *fun, size_t var) {
	struct Upvalue upvalue = !fun->prev || var >= fun->prev->vars_start 
		? ctx->vars[var].is_captured = true,
		(struct Upvalue) { .is_local = true, .index = ctx->vars[var].slot }
		: (struct Upvalue) { .is_local = false, .index = resolve_upvalue(ctx, fun->prev, var) };
	// Check if this closure already has the upvalue
	for (size_t i = 0; i < fun->num_upvalues; ++i) {
		struct Upvalue x = fun->upvalues[i];
		if (x.is_local == upvalue.is_local && x.index == upvalue.index) return i;
	}
	// Otherwise, create a new nonlocal upvalue
	fun->upvalues[fun->num_upvalues] = upvalue;
	return fun->num_upvalues++;
}

static struct VarRef {
	enum VarRefType { VAR_LOCAL, VAR_GLOBAL, VAR_UPVALUE } type;
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

static void emit(struct ByteCompCtx *ctx, union Instruction ins) {
	if (__builtin_expect(ctx->count >= ctx->capacity, false)) {
		size_t new_capacity = ctx->capacity ? 2 * ctx->capacity : 32;
		union Instruction *ins;
		if (!(ins = realloc(ctx->ins, new_capacity * sizeof *ins))) die("malloc failed");
		ctx->ins = ins;
		ctx->capacity = new_capacity;
	}

	ctx->ins[ctx->count++] = ins;
}

/** Emit instructions to move variables greater than or equal to @arg var_limit to the heap. */
static void emit_close_upvalues(struct ByteCompCtx *ctx, uint16_t vars_start, uint16_t regs_start) {
	for (size_t i = vars_start; i < ctx->num_vars; ++i)
		if (ctx->vars[i].is_captured) {
			emit(ctx, (union Instruction) { .op = CLOSE_UPVALS, .a = regs_start });
			break;
		}
}

enum CompileError {
	COMP_OK,
	COMP_NORETURN,
	COMP_INVALID_FORM,
	COMP_EXPECTED_LIST,
	COMP_INVALID_VARIABLE,
	COMP_EXPECTED_CONSEQUENT,
};

struct Destination {
	Register reg;
	bool discarded : 1, ///< Whether anything but side-effects will be ignored.
		is_return : 1; ///< Whether the form is in return position.
};

static bool maybe_eval_macro(struct ByteCompCtx *ctx, struct Symbol *sym, LispObject *args, LispObject **out) {
	if (!(lisp_type(sym->value) == LISP_CONS
			&& ((struct Cons *) sym->value)->car == ctx->smacro)) return false;
	LispObject *macro = ((struct Cons *) sym->value)->cdr;

	struct Chunk *chunk;
	char data[sizeof *chunk + (4 + 2 * UINT8_MAX) * sizeof *chunk->ins];
	chunk = (void *) data;
	size_t i = 0, argc = 0;
	chunk->ins[i++] = (union Instruction) { .op = LOAD_OBJ, .a = 0 };
	chunk->ins[i++] = (union Instruction) { .v = macro };
	while (args) {
		chunk->ins[i++] = (union Instruction) { .op = LOAD_OBJ, .a = 1 + argc++ };
		chunk->ins[i++] = (union Instruction) { .v = pop(&args) };
	}
	chunk->ins[i++] = (union Instruction) { .op = CALL, .a = 0, .c = argc };
	chunk->ins[i++] = (union Instruction) { .op = RET };
	chunk->count = i;
	*out = run(chunk);
	return true;
}

static enum CompileError emit_load_obj(struct ByteCompCtx *ctx, LispObject *x, struct Destination dst) {
	int i;
	if (dst.discarded) ;
	else if (!x) emit(ctx, (union Instruction) { .op = LOAD_NIL, .a = dst.reg });
	else if (lisp_type(x) == LISP_INTEGER
		&& INT16_MIN <= (i = *(int *) x) && i <= INT16_MAX)
		emit(ctx, (union Instruction) { .op = LOAD_SHORT, .a = dst.reg, .b = i });
	else {
		emit(ctx, (union Instruction) { .op = LOAD_OBJ, .a = dst.reg });
		emit(ctx, (union Instruction) { .v = x });
	}
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

/** Byte-compile the form @arg x. */
static enum CompileError compile_form(struct ByteCompCtx *ctx, LispObject *x, struct Destination dst) {
	enum CompileError err;
	switch (lisp_type(x)) {
	case LISP_NIL: return emit_load_obj(ctx, NULL, dst);
	case LISP_SYMBOL:
		if (dst.discarded) break;
		struct VarRef var = lookup_var(ctx, x);
		switch (var.type) {
		case VAR_LOCAL:
			if (var.slot != dst.reg)
				emit(ctx, (union Instruction) { .op = MOV, .a = dst.reg, .b = var.slot });
			break;
		case VAR_GLOBAL:
			emit(ctx, (union Instruction) { .op = GETGLOBAL, .a = dst.reg });
			emit(ctx, (union Instruction) { .v = x });
			break;
		case VAR_UPVALUE:
			emit(ctx, (union Instruction) { .op = GETUPVALUE, .a = dst.reg, .b = var.slot });
			break;
		default: __builtin_unreachable();
		}
		break;
	case LISP_INTEGER: return emit_load_obj(ctx, x, dst);
	case LISP_FUNCTION: return COMP_INVALID_FORM;
	case LISP_CONS:
		LispObject *head = pop(&x);
		if (!listp(x)) return COMP_INVALID_FORM;

		if (head == ctx->fprogn) return compile_progn(ctx, x, dst);
		else if (head == ctx->fquote) return emit_load_obj(ctx, pop(&x), dst);
		else if (head == ctx->flambda) {
			if (dst.discarded) break;
			LispObject *args = pop(&x);
			struct FuncState fun = {
				.prev = ctx->fun,
				.prev_num_regs = ctx->num_regs, .vars_start = ctx->num_vars,
			};
			ctx->fun = &fun;
			ctx->num_regs = 1; // Reserve 0 for return register

			size_t num_args = 0;
			while (args) {
				LispObject *sym = pop(&args);
				if (lisp_type(sym) != LISP_SYMBOL) return COMP_INVALID_VARIABLE;
				++num_args;
				ctx->vars[ctx->num_vars++]
					= (struct Local) { .symbol = sym, .slot = ctx->num_regs++ };
			}

			size_t closure_pos = ctx->count;
			emit(ctx, (union Instruction) { .op = CLOS, .a = dst.reg, .c = num_args });
			emit(ctx, (union Instruction) {}); // Placeholder for #instructions
			err = compile_progn(ctx, x, (struct Destination) { .reg = 0, .is_return = true });
			if (!err) {
				emit_close_upvalues(ctx, fun.vars_start, 0);
				emit(ctx, (union Instruction) { .op = RET });
			} else if (err != COMP_NORETURN) return err;

			ctx->ins[closure_pos + 1].i = ctx->count - (closure_pos + 2);
			// Write upvalues
			ctx->ins[closure_pos].d = fun.num_upvalues;
			for (unsigned i = 0; i < fun.num_upvalues; ++i) {
				struct Upvalue x = fun.upvalues[i];
				emit(ctx, (union Instruction) { .a = x.is_local, .b = x.index });
			}
			ctx->fun = fun.prev;
			ctx->num_regs = fun.prev_num_regs;
			ctx->num_vars = fun.vars_start;
		} else if (head == ctx->flet) {
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
			err = compile_progn(ctx, x, dst);
			if (!err) emit_close_upvalues(ctx, prev_num_vars, prev_num_regs);
			else if (err != COMP_NORETURN) return err;
			ctx->num_regs = prev_num_regs;
			ctx->num_vars = prev_num_vars;
		} else if (head == ctx->fset) {
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
					emit(ctx, (union Instruction) { .op = MOV, .a = v.slot, .b = dst.reg });
				break;
			case VAR_GLOBAL:
				emit(ctx, (union Instruction) { .op = SETGLOBAL, .a = dst.reg });
				emit(ctx, (union Instruction) { .v = sym });
				break;
			case VAR_UPVALUE:
				emit(ctx, (union Instruction) { .op = SETUPVALUE, .a = dst.reg, .b = v.slot });
				break;
			default: __builtin_unreachable();
			}
		} else if (head == ctx->fif) {
			// Emit condition evaluation
			if ((err = compile_form(ctx, pop(&x), (struct Destination) { .reg = dst.reg })))
				return err;
			size_t jmp = ctx->count;
			emit(ctx, (union Instruction) { .op = JNIL, .a = dst.reg });
			if (!x) return COMP_EXPECTED_CONSEQUENT;
			err = compile_form(ctx, pop(&x), dst); // Emit consequent
			if (!(err == COMP_OK || err == COMP_NORETURN)) return err;
			bool noreturn = err == COMP_NORETURN;
			if (x) { // Emit alternative
				emit(ctx, (union Instruction) { .op = JMP });
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
			while (x) {
				if (lisp_type(x) != LISP_CONS) return COMP_EXPECTED_LIST;
				++num_args;
				if ((err = compile_form(ctx, pop(&x),
							(struct Destination) { .reg = ctx->num_regs++ }))) return err;
			}
			if ((err = compile_form(ctx, head, (struct Destination) { .reg = reg })))
				return err;
			ctx->num_regs = prev_num_regs;

			if (dst.is_return && ctx->fun) {
				// Close upvalues before tail-call, since there is no opportunity later
				emit_close_upvalues(ctx, ctx->fun->vars_start, 0);
				emit(ctx, (union Instruction) { .op = TAIL_CALL, .a = reg, .c = num_args });
				return COMP_NORETURN;
			} else {
				emit(ctx, (union Instruction) { .op = CALL, .a = reg, .c = num_args, });
				if (reg != dst.reg && !dst.discarded)
					emit(ctx, (union Instruction) { .op = MOV, .a = dst.reg, .b = reg });
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
		.flambda = intern_c_string(lisp_ctx, "lambda"),
		.fif = intern_c_string(lisp_ctx, "if"),
		.flet = intern_c_string(lisp_ctx, "let"),
		.fset = intern_c_string(lisp_ctx, "set"),
		.fprogn = intern_c_string(lisp_ctx, "progn"),
		.fquote = intern_c_string(lisp_ctx, "quote"),
		.smacro = intern_c_string(lisp_ctx, "macro"),
		.num_regs = 1, // Reserve 0 for return register
	};
	enum CompileError err = compile_form(&ctx, form, (struct Destination) { .reg = 0, .is_return = true });
	if (!err) emit(&ctx, (union Instruction) { .op = RET });
	else if (err != COMP_NORETURN) die("Compilation error: %d", err);

	struct Chunk *chunk = gc_alloc(heap, sizeof *chunk + ctx.count * sizeof *ctx.ins, &chunk_tib);
	chunk->count = ctx.count;
	memcpy(chunk->ins, ctx.ins, ctx.count * sizeof *ctx.ins);

	free(ctx.ins); // TODO Reuse for next top-level form
	return chunk;
}

LispObject *lisp_eval(struct LispContext *ctx, LispObject *form) {
	struct Chunk *chunk = compile(ctx, form);
	return run(chunk);
}
