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
	BC_NUM_OPS,
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
static void chunk_trace(struct GcHeap *heap, void *x) {
	struct Chunk *chunk = x;
	gc_mark(chunk_size(x), x);
	for (LispObject *p = chunk_constants(chunk), *end = p + chunk->num_consts;
			p < end; ++p) gc_trace(heap, p);
}
static struct GcTypeInfo chunk_tib = { chunk_trace, chunk_size };

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
	for (unsigned i = 0; i < closure->prototype->num_upvalues; ++i)
		gc_trace(heap, (void **) (closure->upvalues + i));

	struct Chunk *chunk
		= (struct Chunk *) ((char *) closure->prototype->consts - offsetof(struct Chunk, data));
	size_t prototype_offset = (char *) closure->prototype - (char *) chunk;
	gc_trace(heap, (void **) &chunk);
	// Update prototype as chunk may have moved
	(closure->prototype = (struct Prototype *) ((char *) chunk + prototype_offset))
		->consts = chunk_constants(chunk);
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

/** Superset of @ref LispObjectType. */
typedef uint8_t IrType;

enum {
	TY_UNBOXED_INT = LISP_INTEGER + 1,
	TY_ANY,
};

enum SsaOp : uint8_t {
	// Comparison operations are ordered such that flipping the zeroth
	// bit inverts them.
	IR_EQ, IR_NEQ,
	IR_SLOAD, ///< Load stack slot.
	IR_GLOAD, ///< Load global.
	IR_ULOAD, ///< Load upvalue.
	IR_CALL, ///< Call C function.
	IR_CALLARG, ///< Argument for C function call.
	IR_LOOP,
	IR_PHI,
	IR_NOP,
	IR_NUM_OPS
};

union SsaInstruction {
	struct {
		enum SsaOp op;
		IrType ty; ///< The type of the instruction result.
		uint16_t a, ///< Operand 1.
			b; ///< Operand 2.
		union {
			uint16_t prev;
			uint8_t reg; ///< The allocated register.
		};
	};
	LispObject v; ///< Object constant.
	int32_t i; ///< Integer constant.
};

typedef uint32_t IrRef;

#define IR_REF_TYPE_SHIFT 16
#define IR_REF_NONE 0

struct SnapshotEntry {
	struct { // Stack entry.
		uint32_t slot : 8, ///< The stack slot to write.
			ref : 24; ///< IR reference to the contents to write.
	};
};

/*
 * When taking an exit, the computations performed hitherto need to be
 * transferred to the stack so that the interpreter can continue from
 * where the compiled trace left off.
 *
 * A snapshot is therefore a list of slots and respective values to
 * restore. (TODO Need to also reconstruct call frames and the PC.)
 */
struct Snapshot {
	/// The number of instructions that have been executed before this snapshot is taken.
	unsigned instruction_count,
		offset, ///< Offset into @ref Recording::snap_entries for first entry.
		stack_entry_count;
};

#define MAX_TRACE_LEN 512
#define MAX_SNAPSHOTS 64
#define MAX_SNAPSHOT_ENTRIES 128
#define IR_BIAS 256

/** Trace recording. */
struct Recording {
	struct Closure *start;
	union SsaInstruction trace[IR_BIAS + MAX_TRACE_LEN];
	unsigned count, num_consts, frame_depth, base_offset, num_slots;
	uint16_t chain[IR_NUM_OPS];

	IrRef slots[256], ///< Array of IR references for each bytecode register.
		*base; ///< Pointer into slots at the current frame offset.

	bool need_snapshot;
	struct SnapshotEntry snap_entries[MAX_SNAPSHOT_ENTRIES];
	struct Snapshot snapshots[MAX_SNAPSHOTS];
	unsigned num_snapshots, num_snap_entries;
};

#define IR_GET(state, ref) ((state)->trace[(ref) & 0xffff])

/** Type of trace link. */
enum TraceLink {
	TRACE_LINK_NONE, ///< No link yet, the trace is incomplete.
	/**
	 * Abort the trace recording, e.g. due to NYI (not yet
	 * implemented) or return from the initial function.
	 */
	TRACE_LINK_ABORT,
	TRACE_LINK_LOOP, ///< Loop to itself.
};

static void record_init(struct Recording *state, struct Closure *f) {
	state->start = f;
	state->count = state->num_consts = 0;
	memset(state->chain, 0, sizeof state->chain);

	state->num_slots = state->base_offset = 0;
	memset(state->slots, 0, sizeof state->slots);
	state->base = state->slots;

	state->need_snapshot = false;
	state->num_snapshots = state->num_snap_entries = 0;
}

static void take_snapshot(struct Recording *state) {
	if (!state->need_snapshot) return;
	if (state->num_snapshots >= MAX_SNAPSHOTS
		|| state->num_snap_entries + state->num_slots >= MAX_SNAPSHOT_ENTRIES)
		die("Too many snapshots");
	struct Snapshot *snap = state->snapshots + state->num_snapshots++;
	*snap = (struct Snapshot) {
		.instruction_count = state->count, .offset = state->num_snap_entries,
	};
	for (unsigned i = 0; i < state->num_slots; ++i) {
		IrRef ref = state->slots[i];
		if (ref) {
			union SsaInstruction ins = IR_GET(state, ref);
			// Skip unmodified SLOADs (TODO Cannot do this in nested frames)
			if ((ref & 0xffff) >= IR_BIAS && ins.op == IR_SLOAD && ins.a == i) continue;
			struct SnapshotEntry entry = { .slot = i, .ref = ref };
			state->snap_entries[state->num_snap_entries++] = entry;
			++snap->stack_entry_count;
		}
	}
	state->need_snapshot = false;
}

static IrRef emit_ir(struct Recording *state, union SsaInstruction x) {
	x.prev = state->chain[x.op];
	unsigned i;
	state->trace[state->chain[x.op] = i = IR_BIAS + state->count++] = x;
	return x.ty << IR_REF_TYPE_SHIFT | i;
}

static IrRef emit_folded(struct Recording *state, union SsaInstruction x) {
	switch (x.op) {
	case IR_SLOAD: return state->slots[x.a];
	case IR_EQ: case IR_NEQ: case IR_ULOAD: break;
	case IR_GLOAD: break; // TODO Check that was not set since last load (same for upvalues)
	default: goto no_cse;
	}
	// Common Subexpression Elimination (CSE)
	for (uint16_t ref = state->chain[x.op]; ref;) {
		union SsaInstruction o = IR_GET(state, ref);
		if (o.a == x.a && o.b == x.b) return o.ty << IR_REF_TYPE_SHIFT | ref;
		ref = o.prev;
	}
no_cse: return emit_ir(state, x);
}

static IrRef emit_const(struct Recording *state, IrType ty, union SsaInstruction x) {
	unsigned i;
	// TODO Search if already emitted and return that instead
	state->trace[i = IR_BIAS - ++state->num_consts] = x;
	return ty << IR_REF_TYPE_SHIFT | i;
}

/** Emits a stack load instruction unless the stack slot content is already loaded. */
static IrRef sload(struct Recording *state, LispObject *bp, unsigned slot) {
	IrRef ref = state->base[slot];
	if (ref) return ref; // Already loaded

	take_snapshot(state);
	// TODO Specialize to the case where supsequent code is predicated
	// on the value being of some specific type. That way we can avoid
	// an extraneous guard.
	IrType ty = lisp_type(bp[slot]);
	unsigned min_num_slots = state->base_offset + slot + 1;
	if (state->num_slots < min_num_slots) state->num_slots = min_num_slots;
	return state->base[slot] = emit_ir(state, (union SsaInstruction)
		{ .op = IR_SLOAD, .ty = ty, .a = state->base_offset + slot });
}

/** Copy-substitutes the snapshot. */
static void subst_loop_snapshot(struct Recording *state, struct Snapshot *old, struct Snapshot *loop_snap, uint16_t *substs) {
	// TODO Make a new snapshot where the old entries take precedence and use substs
	(void) state, (void) old, (void) loop_snap, (void) substs;
}

/** Peels off a preamble from the loop.
 *
 * @see ARDÖ, Håkan; BOLZ, Carl Friedrich; FIJABKOWSKI, Maciej.
 *      Loop-aware optimizations in PyPy's tracing JIT. In:
 *      Proceedings of the 8th symposium on Dynamic languages. 2012.
 *      p. 63-72.
 */
static void peel_loop(struct Recording *state) {
	unsigned preamble_len = state->count;
	take_snapshot(state);
	struct Snapshot *loop_snap = state->snapshots + state->num_snapshots - 1;
	// Map of variables in the preamble onto variables of the peeled loop
	uint16_t *substs;
	if (!(substs = calloc(preamble_len, sizeof *substs))) die("malloc failed");
	// Separate preamble from loop body by LOOP instruction
	emit_ir(state, (union SsaInstruction) { .op = IR_LOOP });

	unsigned num_phis = 0;
	uint16_t phis[16];
	struct Snapshot *snap = state->snapshots;
	for (unsigned i = 0; i < preamble_len; ++i) {
		// TODO May need to increment current snapshot and copy-substitute it?
		if (i >= snap->instruction_count)
			subst_loop_snapshot(state, snap++, loop_snap, substs);

		union SsaInstruction ins = state->trace[IR_BIAS + i];
		if (ins.a >= IR_BIAS) ins.a = substs[ins.a - IR_BIAS];
		if (ins.b >= IR_BIAS) ins.b = substs[ins.b - IR_BIAS];

		uint16_t ref = substs[i] = emit_folded(state, ins);
		if (ref != IR_BIAS + i) { // Loop-carried dependency
			if (IR_BIAS <= ref && ref < IR_BIAS + preamble_len) {
				printf("i: %u, ref: %" PRIu16 "\n", i, ref - IR_BIAS);
				phis[num_phis++] = ref;
			}
			// TODO Check ref for type-instability
		}
	}

	// Emit PHI moves
	for (unsigned i = 0; i < num_phis; ++i) {
		uint16_t lref = phis[i], rref = substs[lref - IR_BIAS];
		if (lref == rref) die("Invariant phi");
		IrType ty = IR_GET(state, lref).ty;
		printf("lref: %" PRIu16 ", rref: %" PRIu16 "\n", lref - IR_BIAS, rref - IR_BIAS);
		emit_ir(state, (union SsaInstruction) { .op = IR_PHI, .ty = ty, .a = lref, .b = rref });
	}
	free(substs);
}

static bool has_side_effects(enum SsaOp x) {
	return x == IR_EQ || x == IR_NEQ || x == IR_CALL || x == IR_CALLARG;
}

#define IR_MARK 0x80

/** Dead-code elimination. */
static void dce(struct Recording *state) {
	// Mark all instructions referenced in snapshots
	for (struct Snapshot *snap = state->snapshots;
			snap < state->snapshots + state->num_snapshots; ++snap)
		for (struct SnapshotEntry *entry = state->snap_entries + snap->offset,
					*end = entry + snap->stack_entry_count; entry < end; ++entry)
			if ((entry->ref & 0xffff) >= IR_BIAS) IR_GET(state, entry->ref).ty |= IR_MARK;

	// Propagate marks
	uint16_t *pchain[IR_NUM_OPS];
	for (unsigned i = 0; i < IR_NUM_OPS; ++i) pchain[i] = state->chain + i;
	for (union SsaInstruction *x = state->trace + IR_BIAS + state->count;
			x-- > state->trace + IR_BIAS;) {
		if (!(x->ty & IR_MARK || has_side_effects(x->op))) {
			*x = (union SsaInstruction) { .op = IR_NOP };
			*pchain[x->op] = x->prev;
			continue;
		}
		x->ty &= ~IR_MARK;
		pchain[x->op] = &x->prev;
		if (x->a >= IR_BIAS) IR_GET(state, x->a).ty |= IR_MARK;
		if (x->b >= IR_BIAS) IR_GET(state, x->b).ty |= IR_MARK;
	}
}

/** Records instruction @a x prior to it being executed. */
static enum TraceLink record_instruction(struct Recording *state, LispObject *bp, struct Instruction x) {
	LispObject *consts = ((struct Closure *) *bp)->prototype->consts;
	IrRef result = IR_REF_NONE;
	switch (x.op) {
	case LOAD_SHORT:
		result = emit_const(state,
			TY_UNBOXED_INT, (union SsaInstruction) { .i = (int16_t) x.b });
		break;
	case GETGLOBAL:
		struct Symbol *sym = consts[x.b];
		IrRef sym_ref = emit_const(state,
			LISP_SYMBOL, (union SsaInstruction) { .v = sym });
		take_snapshot(state);
		result = emit_ir(state, (union SsaInstruction)
			{ .op = IR_GLOAD, .ty = lisp_type(sym->value), .a = sym_ref });
		break;
	case GETUPVALUE:
		// TODO If in nested function then upvalue might be in our stack already
		// TODO Keep track of whether upvalue is immutable and can be inlined
		struct Upvalue *upvalue = state->start->upvalues[x.c];
		take_snapshot(state);
		result = emit_folded(state, (union SsaInstruction)
			{ .op = IR_ULOAD, .ty = lisp_type(*upvalue->location), .a = x.c });
		break;
	case CALL:
		LispObject fun_value = bp[x.a];
		switch (lisp_type(fun_value)) {
		case LISP_CFUNCTION:
			IrRef ref = sload(state, bp, x.a);
			// TODO If type of ref is TY_ANY then need to emit type check

			// TODO There will not be a need to specialize C
			// subroutines once the calling convention is changed so
			// that all such functions take #args and argument array.
			result = emit_ir(state, (union SsaInstruction)
				{ .op = IR_CALL, .ty = TY_ANY, .a = ref, .b = x.c });
			for (unsigned i = 0; i < x.c; ++i) {
				IrRef arg_ref = sload(state, bp, x.a + 2 + i);
				emit_ir(state, (union SsaInstruction)
					{ .op = IR_CALLARG, .ty = arg_ref >> IR_REF_TYPE_SHIFT, .a = arg_ref });
			}
			break;
		case LISP_CLOSURE:
			puts("Cannot descend into Lisp closure yet.");
			return TRACE_LINK_ABORT;
		default: return TRACE_LINK_ABORT;
		}
		state->num_slots = state->base_offset + x.a + 1; // CALL always uses highest register
		break;
	case TAIL_CALL:
		// Move args down to current frame
		memmove(state->base, state->base + x.a, (2 + x.c) * sizeof *state->base);
		state->need_snapshot = true;

		fun_value = bp[x.a];
		// Specialize to the function value in question
		take_snapshot(state);
		IrRef ref = sload(state, bp, x.a),
			fn_ref = emit_const(state, LISP_CFUNCTION, (union SsaInstruction) { .v = fun_value });
		emit_folded(state, (union SsaInstruction) { .op = IR_EQ, .ty = LISP_CLOSURE, .a = ref, .b = fn_ref });

		if (fun_value == state->start) {
			state->num_slots = state->base_offset + 1 + x.c;
			dce(state);
			peel_loop(state);
			return TRACE_LINK_LOOP;
		}
		puts("Tail calls not yet implemented, aborting trace...");
		return TRACE_LINK_ABORT;
	case MOV: result = sload(state, bp, x.c); break;
	case JMP: break;
	case JNIL:
		ref = sload(state, bp, x.a);
		IrType ty = ref >> IR_REF_TYPE_SHIFT;
		if (ty != TY_ANY) break; // If the NIL comparison is constant there is nothing to do
		take_snapshot(state);
		IrRef nil = emit_const(state, LISP_NIL, (union SsaInstruction) { .v = NULL });
		emit_folded(state, (union SsaInstruction)
			{ .op = IR_EQ ^ !!bp[x.a], .ty = ty, .a = ref, .b = nil });
		break;
	case RET:
		if (state->frame_depth > 0)
			puts("TODO record RET if having inlined a function (when framedepth > 0)");
		[[fallthrough]];
	default:
		printf("Recording instruction %" PRIu8 " is NYI, aborting trace...\n", x.op);
		return TRACE_LINK_ABORT;
	}

	if (result) {
		state->need_snapshot |= state->base[x.a] != result;
		state->base[x.a] = result;
		if (state->base_offset + x.a >= state->num_slots)
			state->num_slots = state->base_offset + x.a + 1U;
	}
	return TRACE_LINK_NONE;
}

static void print_ir_ref(struct Recording *state, IrType ty, uint16_t ref) {
	if (ref >= IR_BIAS) { printf("%.4u", ref - IR_BIAS); return; }
	union SsaInstruction x = IR_GET(state, ref);
	switch (ty) {
	case TY_ANY: case LISP_CFUNCTION: case LISP_CLOSURE: printf("%p", x.v); break;
	case LISP_SYMBOL:
		struct Symbol *sym = x.v;
		printf("[%.*s]", (int) sym->len, sym->name);
		break;
	case TY_UNBOXED_INT: printf("%+d", x.i); break;
	default: printf("const(ty: %" PRIu8 ")", ty); break;
	}
}

static void print_trace(struct Recording *state, enum TraceLink link) {
	const char *type_names[]
		= { "nil", "cns", "sym", "cfn", "clo", "int", "INT", "___" };

	puts("---- TRACE IR");
	for (unsigned i = 0, snap_idx = 0; i < state->count; ++i) {
		struct Snapshot *snap = state->snapshots + snap_idx;
		if (snap_idx < state->num_snapshots && i >= snap->instruction_count) {
			printf("....      SNAP  #%-3u [ ", snap_idx++);
			unsigned j = 0;
			for (struct SnapshotEntry *entry = state->snap_entries + snap->offset,
						*end = entry + snap->stack_entry_count; entry < end; ++entry) {
				for (; j < entry->slot; ++j) printf("---- ");
				print_ir_ref(state, entry->ref >> IR_REF_TYPE_SHIFT, entry->ref);
				putchar(' ');
			}
			puts("]");
		}

		union SsaInstruction x = state->trace[IR_BIAS + i];
		printf("%.4u  %s ", i, type_names[x.ty]);
		switch (x.op) {
		case IR_EQ: printf("EQ    ");
			print_ir_ref(state, x.ty, x.a);
			printf("  ");
			print_ir_ref(state, x.ty, x.b);
			putchar('\n');
			break;
		case IR_NEQ:
			printf("NEQ   ");
			print_ir_ref(state, x.ty, x.a);
			printf("  ");
			print_ir_ref(state, x.ty, x.b);
			putchar('\n');
			break;
		case IR_SLOAD: printf("SLOAD #%" PRIu16 "\n", x.a); break;
		case IR_GLOAD:
			printf("GLOAD "); print_ir_ref(state, LISP_SYMBOL, x.a); putchar('\n'); break;
		case IR_ULOAD: printf("ULOAD #%" PRIu16 "\n", x.a); break;
		case IR_CALL: printf("CALL  ");
			print_ir_ref(state, x.ty, x.a);
			printf("  (");
			for (unsigned j = 0; j < x.b; ++j) {
				union SsaInstruction arg = state->trace[IR_BIAS + ++i];
				if (j > 0) printf("  ");
				print_ir_ref(state, arg.ty, arg.a);
			}
			puts(")");
			break;
		case IR_LOOP: puts("LOOP ------------"); break;
		case IR_PHI:
			printf("PHI   ");
			print_ir_ref(state, x.ty, x.a);
			printf("  ");
			print_ir_ref(state, x.ty, x.b);
			putchar('\n');
			break;
		default: puts("OTHER");
		}
	}
	switch (link) {
	case TRACE_LINK_NONE: puts("---- INCOMPLETE -------"); break;
	case TRACE_LINK_LOOP: puts("---- TRACE stop -> loop"); break;
	default: __builtin_unreachable();
	}
}

[[gnu::optimize ("-fnon-call-exceptions")]]
static LispObject run(struct LispContext *ctx, LispObject *consts, struct Instruction *pc) {
	LispObject *bp = ctx->bp;
	struct Upvalue *upvalues = NULL;
	unsigned char hotcounts[64] = {};
	struct Recording recording;

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
		[MOV] = &&op_mov,
		[JMP] = &&op_jmp,
		[JNIL] = &&op_jnil,
		[CLOS] = &&op_clos,
		[CLOSE_UPVALS] = &&op_close_upvals,
	}, **dispatch_table = main_dispatch_table;
	void *recording_dispatch_table[BC_NUM_OPS];
	for (unsigned i = 0; i < LENGTH(recording_dispatch_table); ++i)
		recording_dispatch_table[i] = &&do_record;
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
	case LISP_CFUNCTION:
		struct LispCFunction *cfun = (struct LispCFunction *) *vals;
		if (ins.c != cfun->nargs) die("Wrong number of arguments");
		1[ctx->bp = vals] = pc; // Synchronize bp and link call-frames
		*vals = cfun->f(ctx, vals + 2);
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
		unsigned char hash = ((uintptr_t) pc ^ (uintptr_t) to) / alignof(struct Instruction),
			*hotcount = hotcounts + hash % LENGTH(hotcounts);
#define JIT_THRESHOLD 4
		if ((*hotcount += 1 + (ins.op == TAIL_CALL)) >= JIT_THRESHOLD) {
			*hotcount = 0;
			if (dispatch_table != recording_dispatch_table) {
				puts("Starting trace recording");
				// Start recording trace
				record_init(&recording, closure);
				dispatch_table = recording_dispatch_table;
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
do_record:
	switch (record_instruction(&recording, bp, ins)) {
	case TRACE_LINK_LOOP:
		puts("Found loop start! Trace:\n");
		print_trace(&recording, TRACE_LINK_LOOP);
		[[fallthrough]];
	case TRACE_LINK_ABORT: dispatch_table = main_dispatch_table; break;
	default: break;
	}
	goto *main_dispatch_table[ins.op];
#pragma GCC diagnostic pop
}

[[gnu::optimize ("-fnon-call-exceptions")]]
static LispObject apply(struct LispContext *ctx, LispObject function, uint8_t n, LispObject args[static n + 1]) {
	enum LispObjectType ty = lisp_type(function);
	if (ty != LISP_CLOSURE && ty != LISP_CFUNCTION) throw(1);

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
	struct LispContext *lisp_ctx;
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
	if (__builtin_expect(n <= ctx->capacity, true)) return;
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
	struct LispContext *lisp_ctx = ctx->lisp_ctx;
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
	case LISP_CFUNCTION: case LISP_CLOSURE: throw(COMP_INVALID_FORM);
	case LISP_CONS:
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
			LispObject macro = ((struct Cons *) ((struct Symbol *) head)->value)->cdr;
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

static struct Chunk *compile(struct LispContext *lisp_ctx, LispObject form) {
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

LispObject lisp_eval(struct LispContext *ctx, LispObject form) {
	struct Chunk *chunk = compile(ctx, form);
	disassemble(chunk);
	LispObject *consts = chunk_constants(chunk);
	// Store dummy closure in first call frame to handle returns uniformly
	*ctx->bp = &(struct Closure) { .prototype = &(struct Prototype) { .consts = consts } };
	return run(ctx, consts, chunk_instructions(chunk));
}
