/** Register-based bytecode virtual machine and single-pass compiler. */

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "lisp.h"
#include "util.h"

/** Operation code. */
enum Op : uint8_t {
	RET,
		LOAD_NIL,
		LOAD_OBJ,
		LOAD_SHORT,
		GETGLOBAL,
		GETUPVALUE,
		CALL,
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
static void chunk_trace(struct Heap *, void *x) { gc_mark(chunk_size(x), x); }
static struct GcTypeInfo chunk_tib = { chunk_trace, chunk_size };

void disassemble(struct Chunk *chunk, const char *name) {
	printf("Disassembling chunk '%s':\n", name);
	for (size_t i = 0; i < chunk->count;) {
		printf("%.4lu ", i);
		union Instruction ins = chunk->ins[i++];

		switch (ins.op) {
		case RET: printf("RET %d\n", ins.a); break;
		case LOAD_NIL: printf("LOAD_NIL %d <- NIL\n", ins.a); break;
		case LOAD_OBJ: printf("LOAD_OBJ %d <- %p\n", ins.a, chunk->ins[i++].v); break;
		case LOAD_SHORT: printf("LOAD_SHORT %d <- %d\n", ins.a, (int16_t) ins.b); break;
		case GETGLOBAL: printf("GETGLOBAL %d <- [%s]\n", ins.a,
			((struct Symbol *) chunk->ins[i++].v)->name); break;
		case GETUPVALUE: printf("GETUPVALUE %d <- %u\n", ins.a, ins.b); break;
		case CALL: printf("CALL %d <- (%d", ins.a, ins.a);
			for (size_t i = 0; i < ins.b; ++i) printf(" %lu", ins.a + 1 + i);
			puts(")");
			break;
		case MOV: printf("MOV %u <- %d\n", ins.a, ins.b); break;
		case JMP: printf("JMP => %.4lu\n", i + (int32_t) ins.b); break;
		case JNIL: printf("JMP if %u == NIL => %.4lu\n", ins.a, i + (int32_t) ins.b); break;
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
static void obj_upvalue_trace(struct Heap *heap, void *x) {
	struct ObjUpvalue *upvalue = x;
	gc_mark(sizeof *upvalue, x);
	if (upvalue->next) gc_trace(heap, (void **) &upvalue->next);
}
static struct GcTypeInfo obj_upvalue_tib = { obj_upvalue_trace, obj_upvalue_size };

struct Closure {
	struct Chunk *chunk;
	size_t offset; // Offset in the chunk.
	unsigned char arity,
		num_upvalues;
	struct ObjUpvalue *upvalues[];
};

static size_t closure_size(void *x) {
	struct Closure *closure = x;
	return sizeof *closure + closure->num_upvalues * sizeof *closure->upvalues;
}
static void closure_trace(struct Heap *heap, void *x) {
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
	size_t ip, bp;
	struct Closure *closure;
};

static LispObject *run(struct Chunk *chunk) {
	union Instruction *xs = chunk->ins;
	LispObject *stack_top[256], **stack = stack_top;
	struct CallFrame frames[128] = {};
	size_t num_frames = 0;
	struct ObjUpvalue *upvalues;

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
	static void *dispatch_table[] = {
		[RET] = &&op_ret,
		[LOAD_NIL] = &&op_load_nil,
		[LOAD_OBJ] = &&op_load_obj,
		[LOAD_SHORT] = &&op_load_short,
		[GETGLOBAL] = &&op_getglobal,
		[GETUPVALUE] = &&op_getupvalue,
		[CALL] = &&op_call,
		[MOV] = &&op_mov,
		[JMP] = &&op_jmp,
		[JNIL] = &&op_jnil,
		[CLOS] = &&op_clos,
		[CLOSE_UPVALS] = &&op_close_upvals,
	};

	for (size_t ip = 0;;) {
		/* printf("Executing instruction at %lu\n", ip); */
		union Instruction ins = xs[ip++];
		goto *dispatch_table[ins.op];
#pragma GCC diagnostic pop

	op_ret:
		if (num_frames) {
			*stack = stack[ins.a];
			ip = frames[--num_frames].ip;
			stack = stack_top + (num_frames ? frames[num_frames - 1].bp : 0);
			continue;
		} else return stack[ins.a];
	op_load_nil: stack[ins.a] = NULL; continue;
	op_load_obj: stack[ins.a] = xs[ip++].v; continue;
	op_load_short: stack[ins.a] = lisp_integer((int16_t) ins.b); continue;
	op_getglobal: stack[ins.a] = ((struct Symbol *) xs[ip++].v)->value; continue;
	op_getupvalue:
		stack[ins.a] = *frames[num_frames - 1].closure->upvalues[ins.b]->location;
		continue;
	op_call:
		LispObject **vals = stack + ins.a;
		switch (lisp_type(*vals)) {
		case LISP_FUNCTION:
			struct Subr *subr = ((struct Function *) *vals)->subr;
			if (ins.b != subr->min_args) UNREACHABLE("Too few arguments\n");
			LispObject **args = vals + 1;
			switch (subr->min_args) {
			case 0: *vals = subr->a0(); break;
			case 1: *vals = subr->a1(*args); break;
			case 2: *vals = subr->a2(*args, args[1]); break;
			case 3: *vals = subr->a3(*args, args[1], args[2]); break;
			default: UNREACHABLE("Bad min_args\n");
			}
			break;
		case LISP_CLOSURE:
			struct Closure *closure = *vals;
			if (ins.b != closure->arity) UNREACHABLE("Wrong number of arguments\n");
			frames[num_frames++] = (struct CallFrame) {
				.ip = ip, .bp = vals - stack_top, .closure = closure,
			};
			ip = closure->offset;
			stack = vals;
			break;
		default: UNREACHABLE("Bad function\n");
		}
		continue;
	op_mov: stack[ins.a] = stack[ins.b]; continue;
	op_jmp: ip += (int32_t) ins.b; continue;
	op_jnil: if (!stack[ins.a]) ip += (int32_t) ins.b; continue;
	op_clos:
		size_t len = xs[ip++].i, num_upvalues = ins.d;
		struct Closure *closure
			= gc_alloc(heap, sizeof *closure + num_upvalues * sizeof *closure->upvalues,
				&closure_tib.gc_tib);
		*closure = (struct Closure) {
			.chunk = chunk, .offset = ip, .arity = ins.c, .num_upvalues = num_upvalues,
		};
		ip += len;
		// Read upvalues
		for (unsigned i = 0; i < num_upvalues; ++i) {
			union Instruction ins = xs[ip++];
			uint8_t is_local = ins.a, index = ins.b;
			struct CallFrame *frame = num_frames ? frames + num_frames - 1 : NULL;
			closure->upvalues[i] = is_local
				? capture_upvalue(&upvalues, stack_top + (frame ? frame->bp : 0) + index)
				: frame->closure->upvalues[index];
		}
		stack[ins.a] = closure;
		continue;
	op_close_upvals:
		while (upvalues && upvalues->location >= stack + ins.a) {
			struct ObjUpvalue *x = upvalues;
			x->closed = *x->location;
			x->location = &x->closed;
			upvalues = x->next;
			x->next = NULL;
		}
		continue;
	}
}

#define MAX_LOCAL_VARS 192
#define MAX_UPVALUES 64

typedef uint16_t Register;

struct Local {
	struct Symbol *symbol;
	Register slot; ///< The register.
	bool is_captured;
};

struct Upvalue {
	char is_local; ///< Whether this upvalue captures a local or an upvalue.
	uint8_t index;
};

struct FuncState {
	struct FuncState *prev;
	size_t prev_num_regs, vars_start, num_upvalues;
	struct Upvalue upvalues[MAX_UPVALUES];
};

struct ByteCompCtx {
	struct LispContext *lisp_ctx;
	struct FuncState *fun;
	size_t num_regs;
	size_t num_vars;
	struct Local vars[MAX_LOCAL_VARS];

	struct Symbol *flambda, *fif, *flet, *fprogn, *fquote;

	size_t count, capacity;
	union Instruction *ins;
};

static size_t resolve_upvalue(struct ByteCompCtx *ctx, struct FuncState *fun, size_t var) {
	struct Upvalue upvalue = !fun->prev || var >= fun->prev->vars_start 
		? ctx->vars[var].is_captured = true,
		(struct Upvalue) { .is_local = true, .index = ctx->vars[var].slot  }
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
	size_t slot;
} lookup_var(struct ByteCompCtx *ctx, struct Symbol *sym) {
	for (size_t i = ctx->num_vars; i-- > 0;)
		if (ctx->vars[i].symbol == sym)
			return ctx->fun && i < ctx->fun->vars_start
				// If not a local value, then resolve upvalue
				? (struct VarRef) { VAR_UPVALUE, .slot = resolve_upvalue(ctx, ctx->fun, i) }
				: (struct VarRef) { VAR_LOCAL, .slot = ctx->vars[i].slot };
	return (struct VarRef) { .type = VAR_GLOBAL };
}

enum FormValueKind {
	KIND_NIL,
	KIND_OBJ,
	KIND_GLOBAL, ///< Global symbol value. sym = the symbol
	KIND_LOCAL, ///< Local register. reg = the register
	KIND_UPVALUE, ///< Upvalue. reg = the upvalue index
};

struct FormValue {
	enum FormValueKind kind;
	union {
		Register reg;
		LispObject *obj;
		struct Symbol *sym;
	};
};

static void emit(struct ByteCompCtx *ctx, union Instruction ins) {
	if (__builtin_expect(ctx->count >= ctx->capacity, false)) {
		size_t new_capacity = ctx->capacity ? 2 * ctx->capacity : 32;
		union Instruction *ins;
		if (!(ins = realloc(ctx->ins, new_capacity * sizeof *ins))) exit(1);
		ctx->ins = ins;
		ctx->capacity = new_capacity;
	}

	ctx->ins[ctx->count++] = ins;
}

static void store(struct ByteCompCtx *ctx, Register reg, struct FormValue x) {
	switch (x.kind) {
	case KIND_NIL: 
		emit(ctx, (union Instruction) { .op = LOAD_NIL, .a = reg });
		break;
	case KIND_OBJ:
		int i;
		if (lisp_type(x.obj) == LISP_INTEGER &&
			INT16_MIN <= (i = *(int *) x.obj) && i <= INT16_MAX)
			emit(ctx, (union Instruction) { .op = LOAD_SHORT, .a = reg, .b = i });
		else {
			emit(ctx, (union Instruction) { .op = LOAD_OBJ, .a = reg });
			emit(ctx, (union Instruction) { .v = x.obj });
		}
		break;
	case KIND_LOCAL:
		if (x.reg != reg)
			emit(ctx, (union Instruction) { .op = MOV, .a = reg, .b = x.reg });
		break;
	case KIND_GLOBAL:
		emit(ctx, (union Instruction) { .op = GETGLOBAL, .a = reg });
		emit(ctx, (union Instruction) { .v = x.sym });
		break;
	case KIND_UPVALUE:
		emit(ctx, (union Instruction) { .op = GETUPVALUE, .a = reg, .b = x.reg });
		break;
	default: UNREACHABLE("Invalid form value kind\n");
	}
}

static struct FormValue compile_form(struct ByteCompCtx *ctx, LispObject *x, Register reg_hint);

static struct FormValue compile_progn(struct ByteCompCtx *ctx, LispObject *x, Register reg_hint) {
	struct FormValue value = { .kind = KIND_NIL, };
	while (x) {
		if (lisp_type(x) != LISP_CONS) UNREACHABLE("Bad list");
		value = compile_form(ctx, pop(&x), reg_hint);
	} 
	return value;
}

/**
 * Byte-compile the form @arg x.
 *
 * This emits instructions to perform the side-effects of the form and
 * returns directions on how to obtain the resulting value.
 */
static struct FormValue compile_form(struct ByteCompCtx *ctx, LispObject *x, Register reg_hint) {
	switch (lisp_type(x)) {
	case LISP_NIL: return (struct FormValue) { .kind = KIND_NIL, };
	case LISP_SYMBOL:
		struct VarRef var = lookup_var(ctx, x);
		switch (var.type) {
		case VAR_LOCAL: return (struct FormValue) { .kind = KIND_LOCAL, .reg = var.slot };
		case VAR_GLOBAL: return (struct FormValue) { .kind = KIND_GLOBAL, .sym = x };
		case VAR_UPVALUE: return (struct FormValue) { .kind = KIND_UPVALUE, .reg = var.slot };
		default: __builtin_unreachable();
		}
	case LISP_INTEGER: return (struct FormValue) { .kind = KIND_OBJ, .obj = x };
	case LISP_FUNCTION: UNREACHABLE("Cannot evaluate function\n");
	case LISP_CONS:
		struct Cons *cell = x;
		LispObject *head = pop(&x);
		if (!listp(cell->cdr)) UNREACHABLE("Bad list"); 

		if (head == ctx->fprogn) return compile_progn(ctx, x, reg_hint);
		else if (head == ctx->fquote) return (struct FormValue) { KIND_OBJ, .obj = pop(&x) };
		else if (head == ctx->flambda) {
			LispObject *args = pop(&x);
			struct FuncState fun = {
				.prev = ctx->fun,
				.prev_num_regs = ctx->num_regs, .vars_start = ctx->num_vars,
			};
			ctx->fun = &fun;
			ctx->num_regs = 0;

			size_t num_args = 0;
			Register retval_reg = ctx->num_regs++;
			while (args) {
				LispObject *sym = pop(&args);
				if (lisp_type(sym) != LISP_SYMBOL) UNREACHABLE("Bad argument\n");
				++num_args;
				ctx->vars[ctx->num_vars++]
					= (struct Local) { .symbol = sym, .slot = ctx->num_regs++ };
			}

			Register result_reg = reg_hint != (Register) -1 ? reg_hint : ctx->num_regs++;
			size_t closurePos = ctx->count;
			emit(ctx, (union Instruction) { .op = CLOS, .a = result_reg, .c = num_args });
			emit(ctx, (union Instruction) {}); // Placeholder for #instructions
			store(ctx, retval_reg, compile_progn(ctx, x, retval_reg));

			for (size_t i = fun.vars_start; i < ctx->num_vars; ++i)
				if (ctx->vars[i].is_captured) {
					emit(ctx, (union Instruction) { .op = CLOSE_UPVALS, .a = fun.prev_num_regs });
					break;
				}

			emit(ctx, (union Instruction) { .op = RET, .a = retval_reg });

			ctx->ins[closurePos + 1].i = ctx->count - (closurePos + 2);
			// Write upvalues
			ctx->ins[closurePos].d = fun.num_upvalues;
			for (unsigned i = 0; i < fun.num_upvalues; ++i) {
				struct Upvalue x = fun.upvalues[i];
				emit(ctx, (union Instruction) { .a = x.is_local, .b = x.index });
			}

			ctx->fun = fun.prev;
			ctx->num_regs = fun.prev_num_regs;
			ctx->num_vars = fun.vars_start;
			return (struct FormValue) { .kind = KIND_LOCAL, .reg = result_reg };
		} else if (head == ctx->flet) {
			size_t prev_num_regs = ctx->num_regs, prev_num_vars = ctx->num_vars;
			LispObject *vars = pop(&x);
			while (vars) {
				LispObject *def = pop(&vars), *sym = pop(&def), *initial = pop(&def);
				Register reg = ctx->num_regs++;
				ctx->vars[ctx->num_vars++]
					= (struct Local) { .symbol = sym, .slot = reg };
				store(ctx, reg, compile_form(ctx, initial, reg));
			}
			struct FormValue result = compile_progn(ctx, x, reg_hint);
			if (result.kind == KIND_LOCAL && result.reg >= prev_num_regs) {
				Register reg = reg_hint != (Register) -1 ? reg_hint : ctx->num_regs++;
				store(ctx, reg, result);
				result = (struct FormValue) { .kind = KIND_LOCAL, .reg = reg };
			}

			for (size_t i = prev_num_vars; i < ctx->num_vars; ++i)
				if (ctx->vars[i].is_captured) {
					emit(ctx, (union Instruction) { .op = CLOSE_UPVALS, .a = prev_num_regs });
					break;
				}

			ctx->num_regs = prev_num_regs;
			ctx->num_vars = prev_num_vars;
			return result;
		} else if (head == ctx->fif) {
			Register reg = reg_hint != (Register) -1 ? reg_hint : ctx->num_regs++;
			store(ctx, reg, compile_form(ctx, pop(&x), reg));
			struct FormValue result = x
				? (struct FormValue) { .kind = KIND_LOCAL, .reg = reg }
				: (struct FormValue) { .kind = KIND_NIL };

			size_t jmp = ctx->count;
			emit(ctx, (union Instruction) { .op = JNIL, .a = reg });
			if (x) store(ctx, reg, compile_form(ctx, pop(&x), reg));

			size_t jmp2 = ctx->count;
			emit(ctx, (union Instruction) { .op = JMP });

			ctx->ins[jmp].b = ctx->count - (jmp + 1);

			if (x) store(ctx, reg, compile_progn(ctx, x, reg));
			else emit(ctx, (union Instruction) { .op = LOAD_NIL, .a = reg });

			ctx->ins[jmp2].b = ctx->count - (jmp2 + 1);

			return result;
		}

		size_t prev_num_regs = ctx->num_regs, num_args = 0;
		Register reg = reg_hint == ctx->num_regs - 1 ? reg_hint : ctx->num_regs++;
		while (x) {
			if (lisp_type(x) != LISP_CONS) UNREACHABLE("Bad argument list\n");
			++num_args;
			Register arg_reg = ctx->num_regs++;
			store(ctx, arg_reg, compile_form(ctx, pop(&x), arg_reg));
		}
		store(ctx, reg, compile_form(ctx, head, reg));
		emit(ctx, (union Instruction) { .op = CALL, .a = reg, .b = num_args, });
		ctx->num_regs = prev_num_regs;
		// CALL places result in function value register
		// TODO If given reg_hint always move into that register and return correspondingly
		return (struct FormValue) { .kind = KIND_LOCAL, .reg = reg, };
	default: __builtin_unreachable();
	}
}

static struct Chunk *compile(struct LispContext *lisp_ctx, LispObject *form) {
	struct ByteCompCtx ctx = {
		.lisp_ctx = lisp_ctx,
		.flambda = intern_c_string(lisp_ctx, "lambda"),
		.fif = intern_c_string(lisp_ctx, "if"),
		.flet = intern_c_string(lisp_ctx, "let"),
		.fprogn = intern_c_string(lisp_ctx, "progn"),
		.fquote = intern_c_string(lisp_ctx, "quote"),
	};
	Register reg = ctx.num_regs++;
	store(&ctx, reg, compile_form(&ctx, form, reg)); // TODO Ensure any register (does not have to be first)
	emit(&ctx, (union Instruction) { .op = RET, .a = reg });

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
