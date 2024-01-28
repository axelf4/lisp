#include "jit.h"
#include <stdbit.h>
#include <stdio.h>
#include <inttypes.h>
#include <assert.h>
#include "asm.h"

#define IR_BIAS 0x8000u
#define IR_REF_TYPE_SHIFT 16
#define IS_VAR_REF(ref) (((ref) & 0xffff) >= IR_BIAS)
#define IR_GET(state, ref) (state)->trace[((ref) & 0xffff) - IR_BIAS + MAX_CONSTS]

#define MAX_CONSTS 256
#define MAX_TRACE_LEN 512
#define MAX_SNAPSHOTS 64
#define MAX_SNAPSHOT_ENTRIES 128

/** Trace recording. */
struct Recording {
	uint16_t count, num_consts, frame_depth, base_offset, num_slots;

	IrRef slots[0xff], ///< Array of IR references for each bytecode register.
		*base; ///< Pointer into slots at the current frame offset.
	uint16_t chain[IR_NUM_OPS];
	struct Closure *start; ///< The closure that triggered the recording.

	bool need_snapshot;
	unsigned num_snapshots, num_snap_entries;
	struct Snapshot snapshots[MAX_SNAPSHOTS];
	/// Backing storage for snapshot data.
	struct SnapshotEntry snap_entries[MAX_SNAPSHOT_ENTRIES];

	union SsaInstruction trace[MAX_CONSTS + MAX_TRACE_LEN];
};

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
		if (!ref) continue;
		union SsaInstruction ins = IR_GET(state, ref);
		// Skip unmodified SLOADs (TODO Cannot do this in nested frames as could skip closure/return addr)
		if (IS_VAR_REF(ref) && ins.op == IR_SLOAD && ins.a == i) continue;
		state->snap_entries[state->num_snap_entries++]
			= (struct SnapshotEntry) { .slot = i, .ref = ref };
		++snap->num_stack_entries;
	}
	state->need_snapshot = false;
}

static IrRef emit(struct Recording *state, union SsaInstruction x) {
	x.prev = state->chain[x.op];
	IrRef i;
	IR_GET(state, i = state->chain[x.op] = IR_BIAS + state->count++) = x;
	return x.ty << IR_REF_TYPE_SHIFT | i;
}

static IrRef emit_folded(struct Recording *state, union SsaInstruction x) {
	switch (x.op) {
	case IR_NOP: return 0;
	case IR_SLOAD: return state->slots[x.a];
	case IR_EQ: case IR_NEQ: break;
	case IR_ULOAD: case IR_GLOAD: break; // TODO Check that was not set since last load
	default: goto no_cse;
	}
	// Common Subexpression Elimination (CSE)
	for (uint16_t ref = state->chain[x.op]; ref;) {
		union SsaInstruction o = IR_GET(state, ref);
		if (o.a == x.a && o.b == x.b) return o.ty << IR_REF_TYPE_SHIFT | ref;
		ref = o.prev;
	}
no_cse: return emit(state, x);
}

static IrRef emit_const(struct Recording *state, IrType ty, union SsaInstruction x) {
	IrRef i;
	for (i = IR_BIAS; i-- > IR_BIAS - state->num_consts;)
		if (IR_GET(state, i).v == x.v) goto out;
	IR_GET(state, i = IR_BIAS - ++state->num_consts) = x;
out: return ty << IR_REF_TYPE_SHIFT | i;
}

/** Emits a stack load instruction unless the stack slot content is already loaded. */
static IrRef sload(struct Recording *state, unsigned slot) {
	IrRef ref = state->base[slot];
	if (ref) return ref; // Already loaded

	take_snapshot(state); // Type guard may be added
	unsigned min_num_slots = state->base_offset + slot + 1;
	if (state->num_slots < min_num_slots) state->num_slots = min_num_slots;
	return state->base[slot] = emit(state, (union SsaInstruction)
		{ .op = IR_SLOAD, .ty = TY_ANY, .a = state->base_offset + slot });
}

static void assert_type(struct Recording *state, IrRef *ref, IrType ty) {
	if (*ref >> IR_REF_TYPE_SHIFT == ty) return;
	assert(IS_VAR_REF(*ref));
	union SsaInstruction *x = &IR_GET(state, *ref);
	if (x->ty == TY_ANY) x->ty = ty;
	else if (x->ty != ty) return; // Error upon execution is imminent
	*ref = ty << IR_REF_TYPE_SHIFT | (*ref & 0xffff);
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
	// Map of variables in the preamble onto variables of the peeled loop
	uint16_t subst[MAX_TRACE_LEN];
	// Separate preamble from loop body by LOOP instruction
	emit(state, (union SsaInstruction) { .op = IR_LOOP, .ty = TY_ANY });

	struct Snapshot *snap = state->snapshots,
		*loopsnap = (take_snapshot(state), snap + state->num_snapshots - 1);

	unsigned num_phis = 0;
	uint16_t phis[16];
	for (unsigned i = 0; i < preamble_len; ++i) {
		if (i >= snap->instruction_count) { // Copy-substitute the current snapshot
			struct Snapshot *s = state->snapshots + state->num_snapshots;
			if (s[-1].instruction_count < state->count) {
				++state->num_snapshots;
				*s = (struct Snapshot)
					{ .instruction_count = state->count, .offset = state->num_snap_entries, };
			} else --s; // Overwrite previous snapshot
			struct SnapshotEntry
				*o = state->snap_entries + snap->offset, *oend = o + snap++->num_stack_entries,
				*l = state->snap_entries + loopsnap->offset, *lend = l + loopsnap->num_stack_entries,
				*n = state->snap_entries + s->offset;
			while (o < oend) {
				if (l < lend) {
					if (l->slot < o->slot) { *n++ = *l++; continue; }
					if (l->slot == o->slot) ++l; // Shadowed slot
				}
				struct SnapshotEntry x = *o++;
				if (IS_VAR_REF(x.ref)) x.ref = (x.ref & ~0xffff) | subst[x.ref % IR_BIAS];
				*n++ = x;
			}
			s->num_stack_entries = n - (state->snap_entries + s->offset);
			state->num_snap_entries = s->offset + s->num_stack_entries;
		}

		union SsaInstruction ins = state->trace[MAX_CONSTS + i];
		if (IS_VAR_REF(ins.a)) ins.a = subst[ins.a - IR_BIAS];
		if (IS_VAR_REF(ins.b)) ins.b = subst[ins.b - IR_BIAS];

		IrRef ref = emit_folded(state, ins);
		uint16_t j = (subst[i] = ref) - IR_BIAS;
		if (j != i) { // Loop-carried dependency
			if (j < preamble_len) {
				printf("i: %u, ref: %" PRIu16 "\n", i, j);
				phis[num_phis++] = ref;
			}
			// In case of type-instability, need to emit conversion
			// since later instructions expect the previous type.
			assert(ref >> IR_REF_TYPE_SHIFT == ins.ty && "TODO: type-instability");
		}
	}

	// Emit PHI moves
	for (unsigned i = 0; i < num_phis; ++i) {
		uint16_t lref = phis[i], rref = subst[lref - IR_BIAS];
		if (lref == rref) die("Invariant phi");
		IrType ty = IR_GET(state, lref).ty;
		printf("lref: %" PRIu16 ", rref: %" PRIu16 "\n", lref - IR_BIAS, rref - IR_BIAS);
		emit(state, (union SsaInstruction) { .op = IR_PHI, .ty = ty, .a = lref, .b = rref });
	}
}

static bool has_side_effects(enum SsaOp x) {
	return x == IR_EQ || x == IR_NEQ || x == IR_CALL || x == IR_CALLARG;
}

#define IR_MARK 0x80

/** Dead-code elimination. */
static void dce(struct Recording *state) {
	// Mark instructions referenced in snapshots
	for (struct Snapshot *snap = state->snapshots;
			snap < state->snapshots + state->num_snapshots; ++snap)
		for (struct SnapshotEntry *entry = state->snap_entries + snap->offset,
					*end = entry + snap->num_stack_entries; entry < end; ++entry)
			if (IS_VAR_REF(entry->ref)) IR_GET(state, entry->ref).ty |= IR_MARK;

	// Propagate marks
	uint16_t *pchain[IR_NUM_OPS];
	for (unsigned i = 0; i < IR_NUM_OPS; ++i) pchain[i] = state->chain + i;

	for (union SsaInstruction *x = state->trace + MAX_CONSTS + state->count;
			x-- > state->trace + MAX_CONSTS;) { // Sweep
		if (!(x->ty & IR_MARK || has_side_effects(x->op))) {
			*pchain[x->op] = x->prev;
			x->op = IR_NOP;
			continue;
		}
		x->ty &= ~IR_MARK;
		pchain[x->op] = &x->prev;
		if (IS_VAR_REF(x->a)) IR_GET(state, x->a).ty |= IR_MARK;
		if (IS_VAR_REF(x->b)) IR_GET(state, x->b).ty |= IR_MARK;
	}
}

struct Recording *record_new(struct Closure *f) {
	puts("Starting trace recording");
	struct Recording *state;
	if (!(state = calloc(1, sizeof *state))) return NULL;
	state->start = f;
	state->base = state->slots;
	return state;
}

#ifdef DEBUG
static void print_ir_ref(struct Recording *state, IrType ty, uint16_t ref) {
	if (IS_VAR_REF(ref)) { printf("%.4u", ref - IR_BIAS); return; }
	union SsaInstruction x = IR_GET(state, ref);
	switch (ty) {
	case TY_ANY:
		if (NILP(x.v)) { case LISP_NIL: printf("nil "); break; }
		[[fallthrough]];
	case LISP_CFUNCTION: case LISP_CLOSURE: printf("%" PRIuPTR, x.v); break;
	case LISP_SYMBOL:
		struct Symbol *sym = UNTAG_OBJ(x.v);
		printf("[%.*s]", (int) sym->len, sym->name);
		break;
	case LISP_INTEGER: printf("%+" PRIi32, UNTAG_SMI(x.v)); break;
	default: printf("const(ty: %" PRIu8 ")", ty); break;
	}
}

static void print_trace(struct Recording *state, enum TraceLink link) {
	const char *type_names[]
		= { "AxB", "sym", "str", "cfn", "clo", NULL, NULL, "nil", "int", "___" };

	puts("---- TRACE IR");
	for (unsigned i = 0, snap_idx = 0; i < state->count; ++i) {
		struct Snapshot *snap = state->snapshots + snap_idx;
		if (snap_idx < state->num_snapshots && i >= snap->instruction_count) {
			printf("....      SNAP  #%-3u [ ", snap_idx++);
			unsigned j = 0;
			for (struct SnapshotEntry *entry = state->snap_entries + snap->offset,
						*end = entry + snap->num_stack_entries; entry < end; ++entry) {
				while (j++ < entry->slot) printf("---- ");
				print_ir_ref(state, entry->ref >> IR_REF_TYPE_SHIFT, entry->ref);
				putchar(' ');
			}
			puts("]");
		}

		union SsaInstruction x = state->trace[MAX_CONSTS + i];
		printf("%.4u  %s ", i, type_names[x.ty]);
		switch (x.op) {
		case IR_EQ: printf("EQ    ");
			if (false) case IR_NEQ: printf("NEQ   ");
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
			for (uint16_t j = 0; j < x.b; ++j) {
				union SsaInstruction arg = state->trace[MAX_CONSTS + ++i];
				if (j > 0) printf("  ");
				print_ir_ref(state, arg.ty, arg.a);
			}
			puts(")");
			break;
		case IR_CALLARG: case IR_NUM_OPS: unreachable();
		case IR_LOOP: puts("LOOP ------------"); break;
		case IR_PHI:
			printf("PHI   ");
			print_ir_ref(state, x.ty, x.a);
			printf("  ");
			print_ir_ref(state, x.ty, x.b);
			putchar('\n');
			break;
		case IR_NOP: break;
		}
	}
	switch (link) {
	case TRACE_LINK_LOOP: puts("---- TRACE stop -> loop"); break;
	case TRACE_LINK_ABORT: unreachable();
	}
}
#endif

bool record_instruction(struct Recording *state, uintptr_t *bp, struct Instruction *pc) {
	struct Instruction x = pc[-1];
	IrRef result = 0;
	switch (x.op) {
	case LOAD_SHORT:
		result = emit_const(state, LISP_INTEGER, (union SsaInstruction)
			{ .v = TAG_SMI((int16_t) x.b) });
		break;
	case GETGLOBAL:
		IrRef sym = emit_const(state, LISP_SYMBOL, (union SsaInstruction)
			{ .v = *(LispObject *) (pc - x.b) });
		take_snapshot(state);
		result = emit_folded(state, (union SsaInstruction)
			{ .op = IR_GLOAD, .ty = TY_ANY, .a = sym });
		break;
	case GETUPVALUE:
		// TODO If in nested function then upvalue might be in our stack already
		// TODO Keep track of whether upvalue is immutable and can be inlined
		take_snapshot(state);
		result = emit_folded(state, (union SsaInstruction)
			{ .op = IR_ULOAD, .ty = TY_ANY, .a = x.c });
		break;
	case CALL:
		LispObject fun_value = bp[x.a];
		switch (lisp_type(fun_value)) {
		case LISP_CFUNCTION:
			IrRef ref = sload(state, x.a);
			assert_type(state, &ref, LISP_CFUNCTION);
			take_snapshot(state); // Type guard may get added to return value
			result = emit(state, (union SsaInstruction) { .op = IR_CALL, .ty = TY_ANY, .a = ref, .b = x.c });
			for (unsigned i = 0; i < x.c; ++i) {
				IrRef arg = sload(state, x.a + 2 + i);
				emit(state, (union SsaInstruction)
					{ .op = IR_CALLARG, .ty = arg >> IR_REF_TYPE_SHIFT, .a = arg });
			}
			state->num_slots = state->base_offset + x.a + 1; // CALL always uses highest register
			state->need_snapshot = true; // Call may have side-effects
			break;
		case LISP_CLOSURE:
			puts("Cannot descend into Lisp closure yet.");
			return false;
		default: return false;
		}
		break;
	case TAIL_CALL:
		fun_value = bp[x.a];
		// Specialize to the function value in question
		IrRef ref = sload(state, x.a), fn_ref = emit_const(state, LISP_CFUNCTION,
			(union SsaInstruction) { .v = fun_value });
		take_snapshot(state);
		emit_folded(state, (union SsaInstruction)
			{ .op = IR_EQ, .ty = LISP_CLOSURE, .a = ref, .b = fn_ref });
		// Move args down to current frame
		memmove(state->base, state->base + x.a, (2 + x.c) * sizeof *state->base);
		state->num_slots = state->base_offset + 2 + x.c;

		if (fun_value == TAG_OBJ(state->start)) {
			dce(state);
			peel_loop(state);
#ifdef DEBUG
			puts("Found loop start! Trace:\n");
			print_trace(state, TRACE_LINK_LOOP);
#endif
			return false;
		}
		state->need_snapshot = true;
		puts("Tail calls not yet implemented, aborting trace...");
		return false;
	case MOV: result = sload(state, x.c); break;
	case JMP: break;
	case JNIL:
		ref = sload(state, x.a);
		IrType ty = ref >> IR_REF_TYPE_SHIFT;
		if (ty != TY_ANY) break; // If the NIL comparison is constant there is nothing to do
		IrRef nil = emit_const(state, LISP_NIL, (union SsaInstruction) { .v = NIL });
		take_snapshot(state);
		emit_folded(state, (union SsaInstruction)
			{ .op = IR_EQ ^ !!bp[x.a], .ty = ty, .a = ref, .b = nil });
		break;
	case RET:
		if (state->frame_depth > 0)
			puts("TODO record RET if having inlined a function (when framedepth > 0)");
		[[fallthrough]];
	default:
		printf("Recording instruction %" PRIu8 " is NYI, aborting trace...\n", x.op);
		return false;
	}

	if (result) {
		state->num_slots = MAX(state->num_slots, state->base_offset + x.a + 1);
		state->base[x.a] = result;
	}
	return true;
}

// x86 codegen:
//
// * What registers to reserve?
//   - The lisp context in rdi (1st integer argument register)
//   - stack pointer in rbx (callee saved)

struct RegAlloc {
	// Register bit sets:
	uint32_t occupied, clobbers;
};

void record_assemble(struct Recording *state) {
	struct Assembler ass;
	if (!asm_init(&ass)) die("asm_init failed");

	for (unsigned i = state->count; i-- > 0;) {
		union SsaInstruction x = state->trace[MAX_CONSTS + i];

		switch (x.op) {
		case IR_NOP: break;

		case IR_SLOAD:
		case IR_GLOAD:
		case IR_ULOAD:
		case IR_EQ:
		case IR_NEQ:
		case IR_CALL:
		case IR_CALLARG:
		case IR_LOOP:
		case IR_PHI:
		case IR_NUM_OPS:
			break;
                }
        }
}
