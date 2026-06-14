/** Register-based bytecode virtual machine. */

#include <stdckdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "lisp.h"

#define UV_INSTACK 0x80 ///< The upvalue captures a local instead of an upvalue.
#define UV_MUTABLE 0x40

static_assert(sizeof(LispObject) % sizeof(struct Instruction) == 0);

static struct Upvalue *find_uv(struct LispCtx *ctx, LispObject *slot, bool is_mut) {
	struct Upvalue **p = &ctx->upvalues;
	while (*p && (*p)->location > slot) p = &(*p)->next;
	if (*p && (*p)->location == slot) return *p;

	struct GcHeap *heap = (struct GcHeap *)ctx;
	struct Upvalue *new = gc_alloc(heap, alignof(struct Upvalue), sizeof *new);
	*new = (struct Upvalue) { { new->hdr.hdr, LISP_UPVALUE },
		.is_mut = is_mut, .next = *p, .location = slot };
	return *p = new;
}

/** Collects arguments of a CALL instruction. */
static uint8_t bind_rest(struct LispCtx *ctx, struct Prototype *prototype,
	LispObject *bp, uint8_t n) {
	uint8_t arity = prototype->arity, nargs = arity & ~PROTO_VARIADIC;

	if (LIKELY(n == arity)) return n;
	if (!(arity & PROTO_VARIADIC) || n < nargs) die("Wrong number of arguments");
	LispObject rest = NIL(ctx);
	while (n > nargs) rest = cons(ctx, bp[2 + --n], rest);
	bp[2 + n++] = rest;
	return n;
}

#if __has_c_attribute(clang::musttail)
#ifdef __clang__
#define PRESERVE_NONE [[clang::preserve_none]]
#elifdef __GNUC__
#define PRESERVE_NONE [[gnu::no_callee_saved_registers]]
#else
#define PRESERVE_NONE
#endif

struct Handler;
#define HANDLER_PARAMS struct LispCtx *ctx, struct Instruction *restrict pc, \
		LispObject *bp, struct Instruction insn, struct Handler *dispatch_table
#define HANDLER_ARGS ctx, pc, bp, insn, dispatch_table
PRESERVE_NONE typedef LispObject LispTailCallFunc(HANDLER_PARAMS);

#define HANDLER(op) _tail_call_ ## op
#define DECLARE_HANDLER(op) HANDLER(op),
#define HANDLER_PTR(op) { HANDLER(op) },
#define RECORD_PTR(_) HANDLER_PTR(RECORD)
static LispTailCallFunc FOR_OPS(DECLARE_HANDLER) HANDLER(RECORD);
static struct Handler { LispTailCallFunc *hnd; }
	main_dispatch_table[] = { FOR_OPS(HANDLER_PTR) },
	recording_dispatch_table[] = { FOR_OPS(RECORD_PTR) };

#define DEFINE_OP(op) } PRESERVE_NONE static LispObject HANDLER(op)(HANDLER_PARAMS) {
#define NEXT do { insn = *pc++;											\
		[[clang::musttail]] return dispatch_table[insn.op].hnd(HANDLER_ARGS); } \
	while (0)
#define JMP_TO_LABEL(name) \
	do [[clang::musttail]] return HANDLER(name)(HANDLER_ARGS); while (0)
#define DISPATCH_MAIN(op) do [[clang::musttail]] return \
			main_dispatch_table[op].hnd(HANDLER_ARGS); while (0)

#define VM_BEGIN														\
	struct Handler *dispatch_table = main_dispatch_table;				\
	struct Instruction insn = *pc++;									\
	return dispatch_table[insn.op].hnd(HANDLER_ARGS);					\
	_Pragma("GCC diagnostic push")										\
	_Pragma("GCC diagnostic ignored \"-Wmaybe-musttail-local-addr\"")
#define VM_END _Pragma("GCC diagnostic pop")
#elif HAVE_COMPUTED_GOTO
#define DEFINE_OP(op) op_ ## op:
// Use token-threading to be able to swap dispatch table when recording
#define NEXT do goto *dispatch_table[(insn = *pc++).op]; while (0)
#define JMP_TO_LABEL(name) goto op_ ## name
#define DISPATCH_MAIN(op) do goto *main_dispatch_table[op]; while (0)

#define HANDLER_PTR(op) &&op_ ## op,
#define RECORD_PTR(_) HANDLER_PTR(RECORD)
#define VM_BEGIN _Pragma("GCC diagnostic push")							\
	_Pragma("GCC diagnostic ignored \"-Wpedantic\"")					\
	void *main_dispatch_table[] = { FOR_OPS(HANDLER_PTR) },				\
		*recording_dispatch_table [[maybe_unused]][] = { FOR_OPS(RECORD_PTR) },	\
		**dispatch_table = main_dispatch_table;							\
	struct Instruction insn; NEXT;
#define VM_END _Pragma("GCC diagnostic pop")
#else
#define DEFINE_OP(op) [[maybe_unused]] op_ ## op: case op:
#define NEXT goto vm_start
#define JMP_TO_LABEL(name) goto op_ ## name
#define DISPATCH_MAIN(op) do { _op = (op); goto vm_dispatch; } while (0)
enum { main_dispatch_table, RECORD = 0x1f, recording_dispatch_table = RECORD };

#define VM_BEGIN uint8_t dispatch_table = main_dispatch_table, _op;		\
	vm_start: struct Instruction insn = *pc++; _op = insn.op | dispatch_table; \
	[[maybe_unused]] vm_dispatch: switch (_op) {
#define VM_END }
#endif

[[gnu::optimize ("-fnon-call-exceptions"), gnu::hot]]
static LispObject run(struct LispCtx *ctx, struct Instruction *pc) {
	uintptr_t *bp = ctx->bp;
	bp[1] = (uintptr_t) NULL; // TODO Reentrancy
#if ENABLE_JIT
	assert(!ctx->current_trace && "TODO");
#define JIT_THRESHOLD 4
	memset(ctx->hotcounts, JIT_THRESHOLD, sizeof ctx->hotcounts);
#endif

#define LOAD_CONST (*(LispObject *) (pc - insn.b))

	VM_BEGIN
	DEFINE_OP(MOV) { bp[insn.a] = bp[insn.c]; NEXT; }
	DEFINE_OP(LOADNIL) { bp[insn.a] = NIL(ctx); NEXT; }
	DEFINE_OP(LOADOBJ) { bp[insn.a] = LOAD_CONST; NEXT; }
	DEFINE_OP(LOADSHORT) { bp[insn.a] = TAG_SMI((int16_t) insn.b); NEXT; }
	DEFINE_OP(GETGLOBAL) {
		bp[insn.a] = ((struct LispSymbol *) UNTAG_OBJ(LOAD_CONST))->value;
		NEXT;
	}
	DEFINE_OP(SETGLOBAL) {
		struct LispSymbol *sym = UNTAG_OBJ(LOAD_CONST);
		sym->value = bp[insn.a];
		gc_write_barrier((struct GcHeap *) ctx, &sym->hdr.hdr);
		NEXT;
	}
	DEFINE_OP(GETUPVALUE) {
		bp[insn.a] = *((struct Closure *) UNTAG_OBJ(*bp))->upvalues[insn.c]->location;
		NEXT;
	}
	DEFINE_OP(SETUPVALUE) {
		struct Upvalue *upvalue = ((struct Closure *) UNTAG_OBJ(*bp))->upvalues[insn.c];
		*upvalue->location = bp[insn.a];
		gc_write_barrier((struct GcHeap *) ctx, &upvalue->hdr.hdr);
		NEXT;
	}
	DEFINE_OP(JMP) { pc += insn.b; NEXT; }
	DEFINE_OP(JNIL) { if (NILP(ctx, bp[insn.a])) JMP_TO_LABEL(JMP); NEXT; }

	DEFINE_OP(CALL) {
		LispObject *frame = bp + insn.a;
		frame[1] = (uintptr_t) pc; // Link call-frames
		switch (lisp_type(*frame)) {
		case LISP_CFUNCTION:
			struct LispCFunction *fn = UNTAG_OBJ(*frame);
			ctx->bp = frame; // Synchronize bp
			*frame = fn->f(ctx, insn.c, frame + 2);
			break;
		case LISP_CLOSURE:
			struct Closure *closure = UNTAG_OBJ(*frame);
			bind_rest(ctx, closure->prototype, bp = frame, insn.c);
			pc = closure->prototype->body;
			break;
		default: die("Bad function");
		}
		NEXT;
	}
	DEFINE_OP(TAILCALL) {
		LispObject *frame = bp + insn.a;
		switch (lisp_type(*frame)) {
		case LISP_CFUNCTION:
			struct LispCFunction *fn = UNTAG_OBJ(*frame);
			1[ctx->bp = frame] = (uintptr_t) pc; // Synchronize bp and link call-frames
			*frame = fn->f(ctx, insn.c, frame + 2);
			JMP_TO_LABEL(RET);
		case LISP_CLOSURE:
			struct Closure *closure = UNTAG_OBJ(*bp = *frame);
			uint8_t n = bind_rest(ctx, closure->prototype, frame, insn.c);
			for (unsigned i = 0; i < n; ++i) bp[2 + i] = frame[2 + i];
			pc = closure->prototype->body;
			NEXT;
		default: die("Bad function");
		}
	}
	DEFINE_OP(RET) {
		if (!LIKELY(pc = (struct Instruction *) bp[1])) return insn.a[ctx->bp = bp];
		*bp = bp[insn.a]; // Copy return value to R(A) of CALL instruction
		bp -= pc[-1].a; // Operand A of the CALL was the base pointer offset
		NEXT;
	}

	DEFINE_OP(FNEW) {
		struct Prototype *proto = (struct Prototype *) pc;
		uint8_t *upvalues = (uint8_t *)(pc += insn.b) - proto->num_upvalues;
		struct Closure *closure = gc_alloc((struct GcHeap *)ctx, alignof(struct Closure),
			sizeof *closure + proto->num_upvalues * sizeof *closure->upvalues);
		*closure = (struct Closure) { { closure->hdr.hdr, LISP_CLOSURE }, proto };
		// Read upvalues
		for (unsigned i = 0; i < proto->num_upvalues; ++i) {
			uint8_t uv = upvalues[i], idx = uv % UV_MUTABLE;
			closure->upvalues[i] = uv & UV_INSTACK
				? find_uv(ctx, bp + idx, uv & UV_MUTABLE)
				: ((struct Closure *) UNTAG_OBJ(*bp))->upvalues[idx];
		}
		bp[insn.a] = TAG_OBJ(closure);
		NEXT;
	}
	DEFINE_OP(CLO) {
		while (ctx->upvalues && ctx->upvalues->location >= bp) {
			struct Upvalue *x = ctx->upvalues;
			ctx->upvalues = x->next;
			x->value = *x->location;
			x->location = &x->value;
		}
		NEXT;
	}
#if ENABLE_JIT
	/** Decrements hotcount, triggering trace recorder at zero. */
#define DECR_HOTCOUNT() do {											\
		unsigned char _hash = (uintptr_t) pc / sizeof *pc,				\
			*hotcount = ctx->hotcounts + _hash % LENGTH(ctx->hotcounts); \
		if (!ckd_sub(hotcount, *hotcount, 1)) break;					\
		*hotcount = JIT_THRESHOLD;										\
		if (dispatch_table == recording_dispatch_table) break;			\
		jit_init(ctx->jit_state, pc);									\
		dispatch_table = recording_dispatch_table;						\
} while(0)

	DEFINE_OP(FHDR) { DECR_HOTCOUNT(); NEXT; }
	DEFINE_OP(FHDR_INTERPR) { NEXT; }
	DEFINE_OP(FHDR_JIT) {
		struct LispTrace *trace = (*ctx->traces)[insn.b];
		ctx->bp = bp;
		struct SideExitResult x = trace_exec(ctx, trace);
		pc = (struct Instruction *) GC_DECOMPRESS(ctx, x.pc);
		bp = ctx->bp + x.base_offset;
		dispatch_table = x.should_record ? recording_dispatch_table : main_dispatch_table;
		NEXT;
	}
	DEFINE_OP(RECORD) {
		if (!jit_record(ctx, pc, bp)) {
			dispatch_table = main_dispatch_table;
			insn = pc[-1];
		}
		DISPATCH_MAIN(insn.op);
	}
#else
	DEFINE_OP(RECORD) { unreachable(); }
#endif
	VM_END;
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
	LispObject xs = args[n];
	switch (lisp_type(function)) {
	case LISP_CFUNCTION:
		struct LispCFunction *fn = UNTAG_OBJ(function);
		if (!(n == fn->nargs && NILP(ctx, xs))) die("TODO");
		return fn->f(ctx, n, args);
	case LISP_CLOSURE: break;
	default: throw(1);
	}
	struct Prototype *proto = ((struct Closure *) UNTAG_OBJ(function))->prototype;
	bool is_variadic = proto->arity & PROTO_VARIADIC;
	uint8_t m = proto->arity & ~PROTO_VARIADIC;

	while (n > m) xs = cons(ctx, args[--n], xs);

	*ctx->bp = function;
	memcpy(ctx->bp + 2, args, n * sizeof *args);
	while (n < m && !NILP(ctx, xs)) ctx->bp[2 + n++] = pop(ctx, &xs);

	if (n < m || !(is_variadic || NILP(ctx, xs))) die("Wrong number of arguments");
	ctx->bp[2 + m] = xs;
	return run(ctx, proto->body);
}

#define MAX_LOCAL_VARS 128
#define MAX_UPVALUES 64

#ifndef LISP_GENERATED_FILE
static enum LispKeyword lisp_symbol_to_keyword(struct LispCtx *ctx, LispObject sym) {
#define X(kw, var) LISP_EQ(sym, LISP_CONST(ctx, var)) ? LISP_KW_ ## kw :
	return FOR_KEYWORDS(X) LISP_NO_KEYWORD;
#undef X
}
#endif

typedef uint8_t Register;

struct Local {
	Lobj symbol;
	Register slot; ///< The register.
	bool is_mutable;
};

struct FnState {
	uint8_t prev_num_regs, vars_beg, num_upvalues;
	bool has_uvs;
	unsigned children;
	struct FnState *prev;
	uint8_t upvalues[MAX_UPVALUES];
};

struct ByteCompCtx {
	uint8_t num_regs,
		num_vars;
	unsigned len, capacity,
		prototypes; ///< Linked list of function prototype offsets.
	struct Instruction *insns;
	struct Table consts;
	struct FnState *fn;
	struct LispCtx *lisp_ctx;
	struct Local vars[MAX_LOCAL_VARS];
};

enum CompileError {
	COMP_INVALID_FORM = 1,
	COMP_EXPECTED_LIST,
	COMP_INVALID_VARIABLE,
	COMP_EXPECTED_CONSEQUENT,
	COMP_TOO_MANY_CONSTS,
};

static uint8_t resolve_upvalue(struct FnState *fn, uint8_t i) {
	uint8_t upvalue = i >= fn->prev->vars_beg
		? fn->prev->has_uvs = true, i | UV_INSTACK
		: resolve_upvalue(fn->prev, i);
	// Check if this prototype already has the upvalue
#pragma GCC novector
	for (unsigned i = 0; i < fn->num_upvalues; ++i)
		if (fn->upvalues[i] == upvalue) return i;
	// Otherwise, create a new one
	fn->upvalues[fn->num_upvalues] = upvalue;
	return fn->num_upvalues++;
}

static struct VarRef {
	uint8_t slot;
	enum VarRefType : unsigned char { VAR_LOCAL, VAR_UPVALUE, VAR_GLOBAL } type;
	struct Local *var;
} lookup(struct ByteCompCtx *ctx, LispObject sym) {
	for (unsigned i = ctx->num_vars; i--;) {
		struct Local *var = ctx->vars + i;
		if (var->symbol.p == GC_COMPRESS(sym).p)
			return i < ctx->fn->vars_beg
				? (struct VarRef) { resolve_upvalue(ctx->fn, i), VAR_UPVALUE, var }
				: (struct VarRef) { var->slot, VAR_LOCAL, var };
	}
	return (struct VarRef) { .type = VAR_GLOBAL };
}

static void fixup_uvs(struct ByteCompCtx *ctx) {
	for (unsigned p = ctx->fn->children; p;) {
		struct Prototype *proto = (struct Prototype *)(ctx->insns + p);
		struct Instruction *insns = (struct Instruction *)proto;
		uint8_t *upvalues = (uint8_t *)(insns + insns[-1].b) - proto->num_upvalues;
		for (unsigned i = 0; i < proto->num_upvalues; ++i) {
			struct Local var = ctx->vars[upvalues[i] % UV_INSTACK];
			if (upvalues[i] & UV_INSTACK)
				upvalues[i] = var.slot | UV_INSTACK | (var.is_mutable ? UV_MUTABLE : 0);
		}
		p = proto->next_sibling;
	}
}

[[gnu::cold]] static void chunk_grow(struct ByteCompCtx *ctx) {
	size_t new_capacity = ctx->capacity ? 2 * ctx->capacity : 32;
	struct Instruction *xs;
	if (!(xs = realloc(ctx->insns, new_capacity * sizeof *xs))) die("malloc failed");
	ctx->insns = xs;
	ctx->capacity = new_capacity;
}

static void emit(struct ByteCompCtx *ctx, struct Instruction insn) {
	if (ctx->len >= ctx->capacity) chunk_grow(ctx);
	ctx->insns[ctx->len++] = insn;
}

static uint16_t constant_slot(struct ByteCompCtx *ctx, LispObject x) {
	struct LispEntry *entry, key = { .obj = x };
	if (!(lisp_tbl_entry(&ctx->consts, key, &entry))) {
		if (!entry) die("malloc failed");
		entry->slot = ctx->consts.len;
	}
	size_t offset = (sizeof x / sizeof *ctx->insns) * entry->slot + ctx->len + 1;
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
	struct Instruction insn;
	switch (lisp_type(x)) {
	case LISP_NIL: insn = (struct Instruction) { .op = LOADNIL, .a = dst.reg }; break;
	case LISP_INTEGER:
		int32_t i = UNTAG_SMI(x);
		if (INT16_MIN <= i && i <= INT16_MAX) {
			insn = (struct Instruction) { .op = LOADSHORT, .a = dst.reg, .b = i };
			break;
		}
		[[fallthrough]];
	default:
		uint16_t slot = constant_slot(ctx, x);
		insn = (struct Instruction) { .op = LOADOBJ, .a = dst.reg, .b = slot };
		break;
	}
	emit(ctx, insn);
}

enum CompileResult { COMP_OK, COMP_NORETURN };

static enum CompileResult compile_form(struct ByteCompCtx *ctx, LispObject x, struct Destination dst);

static enum CompileResult compile_progn(struct ByteCompCtx *ctx, LispObject x, struct Destination dst) {
	enum CompileResult ret;
	do {
		if (!listp(x)) throw(COMP_EXPECTED_LIST);
		LispObject form = pop(ctx->lisp_ctx, &x);
		struct Destination d = dst;
		if (!NILP(ctx->lisp_ctx, x)) { d.discarded = true; d.is_return = false; }
		ret = compile_form(ctx, form, d);
	} while (!NILP(ctx->lisp_ctx, x));
	return ret;
}

static void compile_fn(struct ByteCompCtx *ctx, LispObject x, struct Destination dst) {
	if (UNLIKELY(dst.discarded)) return;
	struct FnState fn
		= { .prev = ctx->fn, .prev_num_regs = ctx->num_regs, .vars_beg = ctx->num_vars };
	ctx->fn = &fn;
	ctx->num_regs = 2; // Reserve closure and PC registers

	static_assert(IS_POWER_OF_TWO(sizeof *ctx->insns));
	while ((ctx->len + 1) * sizeof *ctx->insns % alignof(struct Prototype))
		emit(ctx, (struct Instruction) { .op = JMP }); // Align with NOPs
	emit(ctx, (struct Instruction) { .op = FNEW, .a = dst.reg });
	unsigned proto_beg = ctx->len;
	ctx->len += sizeof(struct Prototype) / sizeof *ctx->insns;

	uint8_t num_args = 0;
	for (LispObject args = pop(ctx->lisp_ctx, &x); !NILP(lisp_ctx, args);) {
		LispObject sym;
		if (consp(args)) { sym = pop(ctx->lisp_ctx, &args); ++num_args; }
		else { sym = args; args = NIL(ctx->lisp_ctx); num_args |= PROTO_VARIADIC; }
		if (lisp_type(sym) != LISP_SYMBOL) throw(COMP_INVALID_VARIABLE);
		ctx->vars[ctx->num_vars++]
			= (struct Local) { .symbol = GC_COMPRESS(sym), .slot = ctx->num_regs++ };
	}
#if ENABLE_JIT
	emit(ctx, (struct Instruction) { .op = FHDR });
#endif

	Register reg = ctx->num_regs++; // Return register
	if (!compile_progn(ctx, x, (struct Destination) { .reg = reg, .is_return = true })) {
		if (fn.has_uvs) emit(ctx, (struct Instruction) { .op = CLO });
		emit(ctx, (struct Instruction) { .op = RET, .a = reg });
	}
	fixup_uvs(ctx);

	struct Prototype *prototype = (struct Prototype *)(ctx->insns + proto_beg);
	*prototype = (struct Prototype) {
		.arity = num_args, .num_upvalues = fn.num_upvalues,
		.is_toplevel = !fn.prev->vars_beg,
		.offset = ctx->prototypes,
		.next_sibling = fn.prev->children,
	};
	ctx->prototypes = proto_beg;
	fn.prev->children = proto_beg;

	size_t uv_len = DIV_ROUND_UP(fn.num_upvalues * sizeof *fn.upvalues, sizeof *ctx->insns);
	if (ctx->capacity < ctx->len + uv_len) chunk_grow(ctx);
	uint8_t *upvalues = (uint8_t *)(ctx->insns + (ctx->len += uv_len)) - fn.num_upvalues;
	memcpy(upvalues, fn.upvalues, fn.num_upvalues * sizeof *upvalues);
	ctx->insns[proto_beg - 1].b = ctx->len - proto_beg; // Write prototype size

	ctx->fn = fn.prev;
	ctx->num_regs = fn.prev_num_regs;
	ctx->num_vars = fn.vars_beg;
}

/** Byte-compiles the form @a x. */
static enum CompileResult compile_form(struct ByteCompCtx *ctx, LispObject x, struct Destination dst) {
	struct LispCtx *lisp_ctx = ctx->lisp_ctx;
	switch (lisp_type(x)) {
	case LISP_NIL: case LISP_INTEGER: emit_load_obj(ctx, x, dst); break;
	case LISP_SYMBOL:
		if (dst.discarded) break;
		struct VarRef var = lookup(ctx, x);
		switch (var.type) {
		case VAR_LOCAL:
			if (var.slot == dst.reg) break;
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
		case LISP_KW_FN: compile_fn(ctx, x, dst); break;
		case LISP_KW_LET: {
			uint8_t prev_num_vars = ctx->num_vars;
			for (LispObject defs = pop(lisp_ctx, &x); !NILP(lisp_ctx, defs);) {
				LispObject var = pop(lisp_ctx, &defs), init = pop(lisp_ctx, &defs);
				Register reg = ctx->num_regs;
				ctx->vars[ctx->num_vars++]
					= (struct Local) { .symbol = GC_COMPRESS(var), .slot = reg };
				compile_form(ctx, init, (struct Destination) { .reg = reg });
				++ctx->num_regs;
			}
			enum CompileResult ret = compile_progn(ctx, x, dst);
			ctx->num_vars = prev_num_vars;
			return ret;
		}
		case LISP_KW_SET: {
			LispObject var = pop(lisp_ctx, &x), value = pop(lisp_ctx, &x);
			struct VarRef v = lookup(ctx, var);
			if (dst.discarded && v.type == VAR_LOCAL) dst.reg = v.slot;
			compile_form(ctx, value, (struct Destination) { .reg = dst.reg });
			switch (v.type) {
			case VAR_LOCAL:
				v.var->is_mutable = true;
				if (v.slot == dst.reg) break;
				emit(ctx, (struct Instruction) { .op = MOV, .a = v.slot, .c = dst.reg });
				break;
			case VAR_UPVALUE:
				v.var->is_mutable = true;
				emit(ctx, (struct Instruction) { .op = SETUPVALUE, .a = dst.reg, .c = v.slot });
				break;
			case VAR_GLOBAL:
				if (lisp_type(var) != LISP_SYMBOL) throw(COMP_INVALID_VARIABLE);
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
			size_t jmp = ctx->len;
			emit(ctx, (struct Instruction) { .op = JNIL, .a = dst.reg });
			if (NILP(lisp_ctx, x)) throw(COMP_EXPECTED_CONSEQUENT);
			bool noreturn = compile_form(ctx, pop(lisp_ctx, &x), dst); // Emit consequent
			if (!NILP(lisp_ctx, x)) { // Emit alternative
				emit(ctx, (struct Instruction) { .op = JMP });
				ctx->insns[jmp].b = ctx->len - (jmp + 1);
				jmp = ctx->len - 1;
				noreturn &= compile_progn(ctx, x, dst);
			} else noreturn = false;
			ctx->insns[jmp].b = ctx->len - (jmp + 1);
			return noreturn ? COMP_NORETURN : COMP_OK;
		}
		case LISP_NO_KEYWORD: // Macro or function call
			if (lisp_type(head) == LISP_SYMBOL
				&& consp(((struct LispSymbol *) UNTAG_OBJ(head))->value)) {
				LispObject macro = car(lisp_ctx, ((struct LispSymbol *) UNTAG_OBJ(head))->value);
				return compile_form(ctx, apply(lisp_ctx, macro, 0, &x), dst);
			}

			uint8_t prev_num_regs = ctx->num_regs, num_args = 0;
			Register reg = dst.reg == ctx->num_regs - 1 ? dst.reg : ctx->num_regs++;
			++ctx->num_regs; // Reserve register for PC
			while (!NILP(lisp_ctx, x)) {
				if (!consp(x)) throw(COMP_EXPECTED_LIST);
				++num_args;
				compile_form(ctx, pop(lisp_ctx, &x), (struct Destination) { .reg = ctx->num_regs++ });
			}
			compile_form(ctx, head, (struct Destination) { .reg = reg });
			ctx->num_regs = prev_num_regs;

			if (dst.is_return) {
				// Close upvalues before tail-call, since there is no opportunity later
				if (ctx->fn->has_uvs) emit(ctx, (struct Instruction) { .op = CLO });
				emit(ctx, (struct Instruction) { .op = TAILCALL, .a = reg, .c = num_args });
				return COMP_NORETURN;
			} else {
				emit(ctx, (struct Instruction) { .op = CALL, .a = reg, .c = num_args, });
				if (reg != dst.reg && !dst.discarded)
					emit(ctx, (struct Instruction) { .op = MOV, .a = dst.reg, .c = reg });
			}
			break;
		default: unreachable();
		}
		break;
	default: unreachable();
	}
	return COMP_OK;
}

static struct Chunk *compile(struct LispCtx *lisp_ctx, LispObject form) {
	struct FnState fn;
	struct ByteCompCtx ctx = {
		.lisp_ctx = lisp_ctx,
		.num_regs = 2, // Reserve closure and PC registers
		.fn = &fn, // Dummy top-level function context
		.consts = tbl_new(),
	};
	fn.children = fn.vars_beg = fn.has_uvs = 0;
	if (!compile_form(&ctx, form, (struct Destination) { .reg = 2, .is_return = true })) {
		if (fn.has_uvs) emit(&ctx, (struct Instruction) { .op = CLO });
		emit(&ctx, (struct Instruction) { .op = RET, .a = 2 });
	}
	fixup_uvs(&ctx);

	struct Chunk *chunk = gc_alloc((struct GcHeap *)lisp_ctx, alignof(struct Chunk),
		sizeof *chunk + ctx.consts.len * sizeof(LispObject) + ctx.len * sizeof *ctx.insns);
	*chunk = (struct Chunk) { { chunk->hdr.hdr, LISP_BYTECODE_CHUNK },
		.num_consts = ctx.consts.len, .count = ctx.len };
	struct LispEntry *constant;
	for (size_t i = 0; lisp_tbl_iter_next(&ctx.consts, &i, &constant);)
		chunk_constants(chunk)[chunk->num_consts - constant->slot] = constant->obj;
	memcpy(chunk_instructions(chunk), ctx.insns, ctx.len * sizeof *ctx.insns);
	for (size_t p = ctx.prototypes; p;) { // Patch prototype chunk offsets
		struct Prototype *proto = (struct Prototype *)(chunk_instructions(chunk) + p);
		p = proto->offset;
		proto->offset = (char *)proto - (char *)chunk;
	}

	lisp_tbl_free(&ctx.consts);
	free(ctx.insns);
	return chunk;
}

LispObject lisp_eval(struct LispCtx *ctx, LispObject form) {
	struct Chunk *chunk = compile(ctx, form);
	return run(ctx, chunk_instructions(chunk));
}
