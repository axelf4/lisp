/** Register-based bytecode virtual machine and single-pass compiler. */

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <string.h>
#include "lisp.h"
#include "fxhash.h"

#define UPVALUE_LOCAL 0x80 ///< The upvalue captures a local instead of an upvalue.

static_assert(sizeof(LispObject) % alignof(struct Instruction) == 0);

static void disassemble_range(size_t n, struct Instruction xs[static n], int indent) {
	for (size_t i = 0; i < n;) {
		printf("%*s%.4zu ", indent, "", i);
		struct Instruction x = xs[i++];
#define GET_CONST (*(LispObject *) (xs + i - x.b))
		switch (x.op) {
		case RET: printf("RET %" PRIu8 "\n", x.a); break;
		case LOAD_NIL: printf("LOAD_NIL %" PRIu8 " <- NIL\n", x.a); break;
		case LOAD_OBJ: printf("LOAD_OBJ %" PRIu8 " <- %" PRIuPTR "\n", x.a, GET_CONST); break;
		case LOAD_SHORT: printf("LOAD_SHORT %" PRIu8 " <- %" PRIi16 "\n", x.a, (int16_t) x.b); break;
		case GETGLOBAL: printf("GETGLOBAL %" PRIu8 " <- [%s]\n", x.a,
			((struct Symbol *) UNTAG_OBJ(GET_CONST))->name); break;
		case SETGLOBAL: printf("SETGLOBAL %" PRIu8 " -> [%s]\n", x.a,
			((struct Symbol *) UNTAG_OBJ(GET_CONST))->name); break;
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
			struct Prototype *proto = (struct Prototype *) (xs + i);
			printf("CLOS %" PRIu8 " <- (arity: %" PRIu8 ") (num_upvals: %" PRIu8 "):\n",
				x.a, proto->arity, proto->num_upvalues);
			size_t metadata_size = sizeof *proto + proto->num_upvalues * sizeof(uint8_t) + sizeof x - 1;
			disassemble_range(x.b - metadata_size / sizeof x, proto->body, indent + 2);
			i += x.b;
			break;
		case CLOSE_UPVALS: printf("CLOSE_UPVALS >= %" PRIu8 "\n", x.a); break;
		default: unreachable();
		}
	}
}
static void disassemble(struct Chunk *chunk) {
	puts("Disassembling chunk:");
	disassemble_range(chunk->count, chunk_instructions(chunk), 0);
}

static struct Upvalue *capture_upvalue(struct GcHeap *heap, struct Upvalue **p, LispObject *local) {
	while (*p && (*p)->location > local) p = &(*p)->next;
	if (*p && (*p)->location == local) return *p;

	struct Upvalue *new = gc_alloc(heap, alignof(struct Upvalue), sizeof *new);
	*new = (struct Upvalue) { { new->hdr.hdr, LISP_UPVALUE },
		.is_closed = false, .next = *p, .location = local };
	return *p = new;
}

[[gnu::optimize ("-fnon-call-exceptions"), gnu::hot]]
static LispObject run(struct LispCtx *ctx, struct Instruction *pc) {
	struct GcHeap *heap = (struct GcHeap *) ctx;
	uintptr_t *bp = ctx->bp;
	bp[1] = (uintptr_t) NULL;
	struct Upvalue *upvalues = NULL;
#if ENABLE_JIT
	unsigned char hotcounts[64] = {};
#define JIT_THRESHOLD 4
	memset(hotcounts, JIT_THRESHOLD, sizeof hotcounts);
#endif

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
	void *main_dispatch_table[] = {
		[RET] = &&op_ret,
		[LOAD_NIL] = &&op_load_nil,
		[LOAD_OBJ] = &&op_load_obj,
		[LOAD_SHORT] = &&op_load_short,
		[GETGLOBAL] = &&op_getglobal, [SETGLOBAL] = &&op_setglobal,
		[GETUPVALUE] = &&op_getupvalue, [SETUPVALUE] = &&op_setupvalue,
		[CALL] = &&op_call, [TAIL_CALL] = &&op_tail_call,
		[TAIL_JIT_CALL] = &&op_tail_jit_call,
		[MOV] = &&op_mov,
		[JMP] = &&op_jmp, [JNIL] = &&op_jnil,
		[CLOS] = &&op_clos, [CLOSE_UPVALS] = &&op_close_upvals,
	}, **dispatch_table = main_dispatch_table;
	void *recording_dispatch_table[BC_NUM_OPS];
	for (unsigned i = 0; i < LENGTH(recording_dispatch_table); ++i)
		recording_dispatch_table[i] = &&do_record;
	struct Instruction ins;
	// Use token-threading to be able to swap dispatch table when recording
#define NEXT goto *dispatch_table[(ins = *pc++).op]
	NEXT;

#define LOAD_CONST (*(LispObject *) (pc - ins.b))
op_ret:
	if (!LIKELY(pc = (struct Instruction *) bp[1])) return ins.a[ctx->bp = bp];
	*bp = bp[ins.a]; // Copy return value to R(A) of CALL instruction
	bp -= pc[-1].a; // Operand A of the CALL was the base pointer offset
	NEXT;
op_load_nil: bp[ins.a] = NIL; NEXT;
op_load_obj: bp[ins.a] = LOAD_CONST; NEXT;
op_load_short: bp[ins.a] = TAG_SMI((int16_t) ins.b); NEXT;
op_getglobal: bp[ins.a] = ((struct Symbol *) UNTAG_OBJ(LOAD_CONST))->value; NEXT;
op_setglobal:
	struct Symbol *sym = UNTAG_OBJ(LOAD_CONST);
	sym->value = bp[ins.a];
	gc_write_barrier(heap, &sym->hdr.hdr);
	NEXT;
op_getupvalue:
	bp[ins.a] = *((struct Closure *) UNTAG_OBJ(*bp))->upvalues[ins.c]->location;
	NEXT;
op_setupvalue:
	struct Upvalue *upvalue = ((struct Closure *) UNTAG_OBJ(*bp))->upvalues[ins.c];
	*upvalue->location = bp[ins.a];
	gc_write_barrier(heap, &upvalue->hdr.hdr);
	NEXT;
op_call: op_tail_call:
	LispObject *vals = bp + ins.a;
	switch (lisp_type(*vals)) {
	case LISP_CFUNCTION:
		struct LispCFunction *fun = UNTAG_OBJ(*vals);
		1[ctx->bp = vals] = (uintptr_t) pc; // Synchronize bp and link call-frames
		*vals = fun->f(ctx, ins.c, vals + 2);
		if (ins.op == TAIL_CALL) goto op_ret;
		break;
	case LISP_CLOSURE:
		struct Closure *closure = UNTAG_OBJ(*vals);
		struct Prototype *proto = closure->prototype;
		uint8_t nargs = proto->arity & ~PROTO_VARIADIC;
		if (proto->arity & PROTO_VARIADIC) {
			LispObject rest = NIL;
			while (ins.c > nargs) rest = cons(ctx, vals[2 + --ins.c], rest);
			vals[2 + ins.c++] = rest;
			++nargs;
		}
		if (ins.c != nargs) die("Wrong number of arguments");

		struct Instruction *to = proto->body;
#if ENABLE_JIT
		// Increment hotcount
		unsigned char hash = ((uintptr_t) pc ^ (uintptr_t) to) / sizeof *to,
			*hotcount = hotcounts + hash % LENGTH(hotcounts);
		if ((*hotcount -= 1 + (ins.op == TAIL_CALL)) <= 0) {
			*hotcount = JIT_THRESHOLD;
			if (dispatch_table != recording_dispatch_table
				&& jit_init(ctx->jit_state, closure))
				dispatch_table = recording_dispatch_table; // Start recording trace
		}
#endif

		if (ins.op == TAIL_CALL) {
			*bp = *vals;
			memmove(bp + 2, vals + 2, ins.c * sizeof *vals);
		} else 1[bp = vals] = (uintptr_t) pc;
		pc = to;
		break;
	default: die("Bad function");
	}
	NEXT;
op_mov: bp[ins.a] = bp[ins.c]; NEXT;
op_jnil: if (NILP(bp[ins.a])) op_jmp: pc += ins.b; NEXT;
op_clos:
	struct Prototype *proto = (struct Prototype *) pc;
	struct Closure *closure = gc_alloc(heap, alignof(struct Closure),
		sizeof *closure + proto->num_upvalues * sizeof *closure->upvalues);
	*closure = (struct Closure) { { closure->hdr.hdr, LISP_CLOSURE }, proto };
	// Read upvalues
	uint8_t *indices = (uint8_t *) (pc += ins.b) - proto->num_upvalues;
	for (unsigned i = 0; i < proto->num_upvalues; ++i) {
		uint8_t index = indices[i];
		closure->upvalues[i] = index & UPVALUE_LOCAL
			? capture_upvalue(heap, &upvalues, bp + (index & ~UPVALUE_LOCAL))
			: ((struct Closure *) UNTAG_OBJ(*bp))->upvalues[index];
	}
	bp[ins.a] = TAG_OBJ(closure);
	NEXT;
op_close_upvals:
	while (upvalues && upvalues->location >= bp + ins.a) {
		struct Upvalue *x = upvalues;
		upvalues = x->next;
		*x = (struct Upvalue)
			{ x->hdr, .is_closed = true, .value = *x->location, .location = &x->value };
	}
	NEXT;
#if ENABLE_JIT
op_tail_jit_call:
	struct LispTrace *trace = ctx->current_trace = (*ctx->traces)[ins.b];
	vals = bp + ins.a;
	*bp = *vals;
	memmove(bp + 2, vals + 2, trace_arity(trace) * sizeof *vals);
	ctx->bp = bp; // Synchronize bp
	register struct LispCtx *ctx2 __asm__ (STR(REG_LISP_CTX)) = ctx;
	register struct Instruction *pc2 __asm__ (STR(REG_PC));
	uint8_t frame_offset;
	__asm__ volatile ("call %[f]"
		: "=r" (pc2), "=d" (frame_offset)
		: "r" (ctx2), [f] "rm" (*(void (**)()) trace)
		: "rax", "rcx", "rdi", "r8", "r9", "r10", "r11", "cc", "memory");
	pc = pc2;
	bp += frame_offset;
	NEXT;
do_record:
	ctx->bp = bp;
	if (!jit_record(ctx->jit_state, pc)) {
		dispatch_table = main_dispatch_table;
		ins = pc[-1];
	}
	goto *main_dispatch_table[ins.op];
#else
op_tail_jit_call: do_record: unreachable();
#endif
#pragma GCC diagnostic pop
}

/** Applies @a function to @a args.
 *
 * @param ctx The Lisp context.
 * @param function The Lisp function object to call.
 * @param n The number of fixed arguments.
 * @param args @a n + 1 arguments, the last element being a list to flatten.
 * @return The function return value.
 */
[[gnu::optimize ("-fnon-call-exceptions")]]
static LispObject apply(struct LispCtx *ctx, LispObject function, uint8_t n, LispObject args[static n + 1]) {
	switch (lisp_type(function)) {
	case LISP_CFUNCTION:
		struct LispCFunction *fun = UNTAG_OBJ(function);
		if (!(n == fun->nargs && NILP(args[n]))) die("TODO");
		return fun->f(ctx, n, args);
	case LISP_CLOSURE: break;
	default: throw(1);
	}
	struct Prototype *proto = ((struct Closure *) UNTAG_OBJ(function))->prototype;
	bool variadic = proto->arity & PROTO_VARIADIC;
	uint8_t m = proto->arity & ~PROTO_VARIADIC;

	*ctx->bp = function;
	memcpy(ctx->bp + 2, args, MIN(n, m) * sizeof *args);

	LispObject xs = args[n];
	if (n < m) while (!NILP(xs) && n < m) ctx->bp[2 + n++] = pop(ctx, &xs);
	else while (n > m) xs = cons(ctx, args[--n], xs);
	if (n < m || (!NILP(xs) && !variadic)) die("Wrong number of arguments");
	if (variadic) ctx->bp[2 + m] = xs;

	return run(ctx, proto->body);
}

#define MAX_LOCAL_VARS 192
#define MAX_UPVALUES 64

#ifndef LISP_GENERATED_FILE
static enum LispKeyword lisp_symbol_to_keyword(struct LispCtx *ctx, LispObject sym) {
	Lobj x = GC_COMPRESS(sym);
#define X(kw, var) else if (x.p == LISP_CONST_COMPRESSED(ctx, var).p) return LISP_KW_ ## kw;
	if (false) ; FOR_KEYWORDS(X) else return LISP_NO_KEYWORD;
#undef X
}
#endif

struct ConstantEntry {
	LispObject obj;
	uint16_t slot;
};

static uint64_t constant_hash(struct ConstantEntry x) {
	return fxhash_finish(fxhash(0, x.obj));
}

static bool constant_equal(struct ConstantEntry a, struct ConstantEntry b) {
	return a.obj == b.obj;
}

#define NAME constant
#define KEY struct ConstantEntry
#include "tbl.h"

typedef uint8_t Register;

struct Local {
	Lobj symbol;
	Register slot; ///< The register.
	bool is_captured;
};

struct FnState {
	uint8_t vars_start, prev_num_regs, num_upvalues;
	struct FnState *prev;
	uint8_t upvalues[MAX_UPVALUES];
};

struct ByteCompCtx {
	struct LispCtx *lisp_ctx;
	struct FnState *fn;
	uint8_t num_regs,
		num_vars;
	struct Local vars[MAX_LOCAL_VARS];
	unsigned prototypes; ///< Linked list of function prototype offsets.

	size_t count, capacity;
	struct Instruction *ins;
	struct Table constants;
};

enum CompileError {
	COMP_INVALID_FORM = 1,
	COMP_EXPECTED_LIST,
	COMP_INVALID_VARIABLE,
	COMP_EXPECTED_CONSEQUENT,
	COMP_TOO_MANY_CONSTS,
};

static uint8_t resolve_upvalue(struct ByteCompCtx *ctx, struct FnState *fun, unsigned var) {
	uint8_t upvalue = !fun->prev || var >= fun->prev->vars_start
		? ctx->vars[var].is_captured = true, ctx->vars[var].slot | UPVALUE_LOCAL
		: resolve_upvalue(ctx, fun->prev, var);
	// Check if this closure already has the upvalue
	for (unsigned i = 0; i < fun->num_upvalues; ++i)
		if (fun->upvalues[i] == upvalue) return i;
	// Otherwise, create a new one
	fun->upvalues[fun->num_upvalues] = upvalue;
	return fun->num_upvalues++;
}

static struct VarRef {
	enum VarRefType { VAR_LOCAL, VAR_UPVALUE, VAR_GLOBAL } type;
	unsigned slot;
} lookup_var(struct ByteCompCtx *ctx, LispObject sym) {
	for (size_t i = ctx->num_vars; i-- > 0;)
		if (ctx->vars[i].symbol.p == GC_COMPRESS(sym).p)
			return i < ctx->fn->vars_start
				? (struct VarRef) { VAR_UPVALUE, resolve_upvalue(ctx, ctx->fn, i) }
				: (struct VarRef) { VAR_LOCAL, ctx->vars[i].slot };
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

/** Emits instructions to move variables greater than or equal to @a var_limit to the heap. */
static void emit_close_upvalues(struct ByteCompCtx *ctx, uint16_t vars_start, uint16_t regs_start) {
	for (unsigned i = vars_start; i < ctx->num_vars; ++i)
		if (ctx->vars[i].is_captured) {
			emit(ctx, (struct Instruction) { .op = CLOSE_UPVALS, .a = regs_start });
			break;
		}
}

static uint16_t constant_slot(struct ByteCompCtx *ctx, LispObject x) {
	struct ConstantEntry *entry, key = { .obj = x};
	if (!(constant_tbl_entry(&ctx->constants, key, &entry))) {
		if (!entry) die("malloc failed");
		entry->slot = ctx->constants.len;
	}
	size_t offset = (sizeof x / sizeof *ctx->ins) * entry->slot + ctx->count + 1;
	if (offset > UINT16_MAX) throw(COMP_TOO_MANY_CONSTS);
	return offset;
}

// Provides a limited form of register coalescing.
struct Destination {
	Register reg;
	bool discarded : 1, ///< Whether anything but side-effects will be ignored.
		is_return : 1; ///< Whether the form is in tail position.
};

static void emit_load_obj(struct ByteCompCtx *ctx, LispObject x, struct Destination dst) {
	if (dst.discarded) return;
	struct Instruction ins;
	int i;
	if (NILP(x)) ins = (struct Instruction) { .op = LOAD_NIL, .a = dst.reg };
	else if (lisp_type(x) == LISP_INTEGER
		&& INT16_MIN <= (i = UNTAG_SMI(x)) && i <= INT16_MAX)
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
		if (!listp(x)) throw(COMP_EXPECTED_LIST);
		LispObject form = pop(ctx->lisp_ctx, &x);
		struct Destination d = dst;
		if (!NILP(x)) { d.discarded = true; d.is_return = false; }
		res = compile_form(ctx, form, d);
	} while (!NILP(x));
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
		default: unreachable();
		}
		break;
	case LISP_CFUNCTION: case LISP_CLOSURE: throw(COMP_INVALID_FORM);
	case LISP_PAIR:
		LispObject head = pop(lisp_ctx, &x);
		if (!listp(x)) throw(COMP_INVALID_FORM);
		switch (lisp_symbol_to_keyword(lisp_ctx, head)) {
		case LISP_KW_QUOTE: emit_load_obj(ctx, pop(lisp_ctx, &x), dst); break;
		case LISP_KW_FN: {
			if (dst.discarded) break;
			LispObject args = pop(lisp_ctx, &x);
			struct FnState fun = {
				.prev = ctx->fn,
				.prev_num_regs = ctx->num_regs, .vars_start = ctx->num_vars,
			};
			ctx->fn = &fun;
			ctx->num_regs = 2; // Reserve closure and PC registers

			chunk_reserve(ctx, 1 + (alignof(struct Prototype) - 1 + sizeof(struct Prototype))
				/ sizeof *ctx->ins);
			static_assert(IS_POWER_OF_TWO(sizeof *ctx->ins));
			while ((ctx->count + 1) * sizeof *ctx->ins % alignof(struct Prototype))
				ctx->ins[ctx->count++] = (struct Instruction) { .op = JMP }; // Align with NOPs
			ctx->ins[ctx->count++] = (struct Instruction) { .op = CLOS, .a = dst.reg };
			size_t proto_beg = ctx->count;
			ctx->count += sizeof(struct Prototype) / sizeof *ctx->ins;

			uint8_t num_args = 0;
			while (!NILP(args)) {
				LispObject sym;
				if (consp(args)) { sym = pop(lisp_ctx, &args); ++num_args; }
				else { sym = args; args = NIL; num_args |= PROTO_VARIADIC; }
				if (lisp_type(sym) != LISP_SYMBOL) throw(COMP_INVALID_VARIABLE);
				ctx->vars[ctx->num_vars++]
					= (struct Local) { .symbol = GC_COMPRESS(sym), .slot = ctx->num_regs++ };
			}

			Register reg = ctx->num_regs++; // Return register
			if (!compile_progn(ctx, x, (struct Destination) { .reg = reg, .is_return = true })) {
				emit_close_upvalues(ctx, fun.vars_start, 0);
				emit(ctx, (struct Instruction) { .op = RET, .a = reg });
			}

			*(struct Prototype *) (ctx->ins + proto_beg) = (struct Prototype) {
				.arity = num_args, .num_upvalues = fun.num_upvalues,
				.offset = ctx->prototypes,
			};
			ctx->prototypes = proto_beg;

			size_t upvalues_size = fun.num_upvalues * sizeof *fun.upvalues,
				data_size = (upvalues_size + sizeof *ctx->ins - 1) / sizeof *ctx->ins;
			chunk_reserve(ctx, data_size);
			memcpy((char *) (ctx->ins + (ctx->count += data_size)) - upvalues_size,
				fun.upvalues, fun.num_upvalues * sizeof *fun.upvalues); // Write upvalues
			ctx->ins[proto_beg - 1].b = ctx->count - proto_beg; // Write prototype size

			ctx->fn = fun.prev;
			ctx->num_regs = fun.prev_num_regs;
			ctx->num_vars = fun.vars_start;
			break;
		}
		case LISP_KW_LET: {
			uint8_t prev_num_regs = ctx->num_regs, prev_num_vars = ctx->num_vars;
			LispObject defs = pop(lisp_ctx, &x);
			while (!NILP(defs)) {
				LispObject var = pop(lisp_ctx, &defs), init = pop(lisp_ctx, &defs);
				Register reg = ctx->num_regs++;
				ctx->vars[ctx->num_vars++] = (struct Local) { .symbol = GC_COMPRESS(var), .slot = reg };
				compile_form(ctx, init, (struct Destination) { .reg = reg });
			}
			if (compile_progn(ctx, x, dst)) return COMP_NORETURN;
			emit_close_upvalues(ctx, prev_num_vars, prev_num_regs);
			ctx->num_regs = prev_num_regs;
			ctx->num_vars = prev_num_vars;
			break;
		}
		case LISP_KW_SET: {
			LispObject var = pop(lisp_ctx, &x), value = pop(lisp_ctx, &x);
			if (lisp_type(var) != LISP_SYMBOL) throw(COMP_INVALID_VARIABLE);
			struct VarRef v = lookup_var(ctx, var);
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
			default: unreachable();
			}
			break;
		}
		case LISP_KW_IF: {
			// Emit condition
			compile_form(ctx, pop(lisp_ctx, &x), (struct Destination) { .reg = dst.reg });
			size_t jmp = ctx->count;
			emit(ctx, (struct Instruction) { .op = JNIL, .a = dst.reg });
			if (NILP(x)) throw(COMP_EXPECTED_CONSEQUENT);
			bool noreturn = compile_form(ctx, pop(lisp_ctx, &x), dst); // Emit consequent
			if (!NILP(x)) { // Emit alternative
				emit(ctx, (struct Instruction) { .op = JMP });
				ctx->ins[jmp].b = ctx->count - (jmp + 1);
				jmp = ctx->count - 1;
				noreturn &= compile_progn(ctx, x, dst);
			} else noreturn = false;
			ctx->ins[jmp].b = ctx->count - (jmp + 1);
			if (noreturn) return COMP_NORETURN;
			break;
		}
		default:
			if (lisp_type(head) == LISP_SYMBOL
				&& consp(((struct Symbol *) UNTAG_OBJ(head))->value)) {
				LispObject macro = car(lisp_ctx, ((struct Symbol *) UNTAG_OBJ(head))->value);
				return compile_form(ctx, apply(ctx->lisp_ctx, macro, 0, &x), dst);
			}

			// Function call
			uint8_t prev_num_regs = ctx->num_regs, num_args = 0;
			Register reg = dst.reg == ctx->num_regs - 1 ? dst.reg : ctx->num_regs++;
			++ctx->num_regs; // Reserve register for PC
			while (!NILP(x)) {
				if (!consp(x)) throw(COMP_EXPECTED_LIST);
				++num_args;
				compile_form(ctx, pop(lisp_ctx, &x), (struct Destination) { .reg = ctx->num_regs++ });
			}
			compile_form(ctx, head, (struct Destination) { .reg = reg });
			ctx->num_regs = prev_num_regs;

			if (dst.is_return) {
				// Close upvalues before tail-call, since there is no opportunity later
				emit_close_upvalues(ctx, ctx->fn->vars_start, 0);
				emit(ctx, (struct Instruction) { .op = TAIL_CALL, .a = reg, .c = num_args });
				return COMP_NORETURN;
			} else {
				emit(ctx, (struct Instruction) { .op = CALL, .a = reg, .c = num_args, });
				if (reg != dst.reg && !dst.discarded)
					emit(ctx, (struct Instruction) { .op = MOV, .a = dst.reg, .c = reg });
			}
			break;
		}
		break;
	default: unreachable();
	}
	return COMP_OK;
}

static struct Chunk *compile(struct LispCtx *lisp_ctx, LispObject form) {
	struct ByteCompCtx ctx = {
		.lisp_ctx = lisp_ctx,
		.num_regs = 3, // Reserve return, closure and PC registers
		// Dummy top-level function context
		.fn = (struct FnState *) &(alignas(struct FnState) uint8_t) { /* vars_start */ },
	};
	ctx.constants = tbl_new();
	if (!compile_form(&ctx, form, (struct Destination) { .reg = 2, .is_return = true }))
		emit(&ctx, (struct Instruction) { .op = RET, .a = 2 });

	struct GcHeap *heap = (struct GcHeap *) lisp_ctx;
	struct Chunk *chunk = gc_alloc(heap, alignof(struct Chunk), sizeof *chunk
		+ ctx.constants.len * sizeof(LispObject) + ctx.count * sizeof *ctx.ins);
	*chunk = (struct Chunk) { { chunk->hdr.hdr, LISP_BYTECODE_CHUNK },
		.num_consts = ctx.constants.len, .count = ctx.count, };
	LispObject *consts = chunk_constants(chunk);
	struct ConstantEntry *constant;
	for (size_t i = 0; constant_tbl_iter_next(&ctx.constants, &i, &constant);)
		consts[chunk->num_consts - constant->slot] = constant->obj;
	memcpy(chunk_instructions(chunk), ctx.ins, ctx.count * sizeof *ctx.ins);
	for (size_t i = ctx.prototypes; i;) { // Patch prototype chunk offsets
		struct Prototype *proto = (struct Prototype *) (chunk_instructions(chunk) + i);
		i = proto->offset;
		proto->offset = (char *) proto - (char *) chunk;
	}

	constant_tbl_free(&ctx.constants);
	free(ctx.ins);
	return chunk;
}

LispObject lisp_eval(struct LispCtx *ctx, LispObject form) {
	struct Chunk *chunk = compile(ctx, form);
	disassemble(chunk);
	return run(ctx, chunk_instructions(chunk));
}
