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

/** In-progress trace recording. */
struct JitState {
	uint16_t count, num_consts, frame_depth, base_offset, num_slots;

	IrRef slots[0xff], ///< Array of IR references for each VM register.
		*base; ///< Pointer into #slots at the current frame offset.
	uint16_t chain[IR_NUM_OPS];
	struct Closure *start; ///< The closure that triggered the recording.

	bool need_snapshot;
	unsigned num_snapshots, num_snap_entries;
	struct Snapshot snapshots[MAX_SNAPSHOTS];
	/// Backing storage for snapshot data.
	struct SnapshotEntry snap_entries[MAX_SNAPSHOT_ENTRIES];

	union SsaInstruction trace[MAX_CONSTS + MAX_TRACE_LEN];
};

static void take_snapshot(struct JitState *state) {
	if (!state->need_snapshot) return;
	if (state->num_snapshots >= MAX_SNAPSHOTS
		|| state->num_snap_entries + state->num_slots >= MAX_SNAPSHOT_ENTRIES)
		die("Too many snapshots");
	struct Snapshot *snap = state->snapshots + state->num_snapshots++;
	*snap = (struct Snapshot) {
		.ir_start = state->count, .offset = state->num_snap_entries,
	};
	for (unsigned i = 0; i < state->num_slots; ++i) {
		IrRef ref = state->slots[i];
		if (!ref) continue;
		union SsaInstruction ins = IR_GET(state, ref);
		// Skip unmodified SLOADs
		// TODO Make sure this does not skip closure/ret-addr in nested frames
		if (IS_VAR_REF(ref) && ins.op == IR_SLOAD && ins.a == i) continue;
		state->snap_entries[state->num_snap_entries++]
			= (struct SnapshotEntry) { .slot = i, .ref = ref };
		++snap->num_stack_entries;
	}
	state->need_snapshot = false;
}

static IrRef emit(struct JitState *state, union SsaInstruction x) {
	x.prev = state->chain[x.op];
	IrRef i;
	IR_GET(state, i = state->chain[x.op] = IR_BIAS + state->count++) = x;
	return x.ty << IR_REF_TYPE_SHIFT | i;
}

static IrRef emit_folded(struct JitState *state, union SsaInstruction x) {
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

static IrRef emit_const(struct JitState *state, IrType ty, union SsaInstruction x) {
	IrRef i;
	for (i = IR_BIAS; i-- > IR_BIAS - state->num_consts;)
		if (IR_GET(state, i).v == x.v) goto out;
	IR_GET(state, i = IR_BIAS - ++state->num_consts) = x;
out: return ty << IR_REF_TYPE_SHIFT | i;
}

/** Emits a stack load instruction unless the stack slot content is already loaded. */
static IrRef sload(struct JitState *state, int slot) {
	IrRef ref = state->base[slot];
	if (ref) return ref; // Already loaded
	take_snapshot(state); // Type guard may be added
	unsigned min_num_slots = state->base_offset + slot + 1;
	if (state->num_slots < min_num_slots) state->num_slots = min_num_slots;
	return state->base[slot] = emit(state, (union SsaInstruction)
		{ .op = IR_SLOAD, .ty = TY_ANY, .a = state->base_offset + slot });
}

/** Emits an instruction to load the given upvalue. */
static IrRef uref(struct JitState *state, uintptr_t *bp, uint8_t idx) {
	struct Upvalue *upvalue = ((struct Closure *) UNTAG_OBJ(*bp))->upvalues[idx];
	// TODO Track whether the upvalue is immutable and inlinable
	if (!upvalue->is_closed) {
		// In a nested frame the upvalue may be available on the stack
		ptrdiff_t slot = upvalue->location - (bp - state->base_offset);
		if (slot >= 0) return sload(state, slot - state->base_offset);
	}
	take_snapshot(state);
	return emit_folded(state, (union SsaInstruction) { .op = IR_ULOAD, .ty = TY_ANY,
			.a = sload(state, /* this closure */ 0), .b = idx });
}

static void assert_type(struct JitState *state, IrRef *ref, IrType ty) {
	if (*ref >> IR_REF_TYPE_SHIFT == ty) return;
	assert(IS_VAR_REF(*ref));
	union SsaInstruction *x = &IR_GET(state, *ref);
	if (x->ty == TY_ANY) x->ty = ty;
	// If x->ty != ty, then abort upon execution is imminent
	*ref = ty << IR_REF_TYPE_SHIFT | (*ref & 0xffff);
}

static bool has_side_effects(enum SsaOp x) {
	return x == IR_EQ || x == IR_NEQ || x == IR_CALL || x == IR_CALLARG;
}

#define IR_MARK 0x80

/** Dead-code elimination. */
static void dce(struct JitState *state) {
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

/** Peels off a preamble from the loop.
 *
 * @see ARDÖ, Håkan; BOLZ, Carl Friedrich; FIJABKOWSKI, Maciej.
 *      Loop-aware optimizations in PyPy's tracing JIT. In:
 *      Proceedings of the 8th symposium on Dynamic languages. 2012.
 *      p. 63-72.
 */
static void peel_loop(struct JitState *state) {
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
		if (i >= snap->ir_start) { // Copy-substitute the current snapshot
			struct Snapshot *s = state->snapshots + state->num_snapshots;
			if (s[-1].ir_start < state->count) {
				++state->num_snapshots;
				*s = (struct Snapshot)
					{ .ir_start = state->count, .offset = state->num_snap_entries, };
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
		if (j != i && j < preamble_len) { // Loop-carried dependency
			if (IS_VAR_REF(ref)) {
				// SLOAD:s of arguments varied in tail call give rise to φ:s
				phis[num_phis++] = ref;
			}

			// In case of type-instability, need to emit conversion
			// since later instructions expect the previous type.
			assert(ref >> IR_REF_TYPE_SHIFT == ins.ty && "TODO Type-instability");
		}
	}

	// Emit PHI moves
	for (unsigned i = 0; i < num_phis; ++i) {
		uint16_t lref = phis[i], rref = subst[lref - IR_BIAS];
		assert(lref != rref && "Invariant φ");
		IrType ty = IR_GET(state, lref).ty;
		printf("lref: %" PRIu16 ", rref: %" PRIu16 "\n", lref - IR_BIAS, rref - IR_BIAS);
		emit(state, (union SsaInstruction) { .op = IR_PHI, .ty = ty, .a = lref, .b = rref });
	}
}

struct JitState *record_new(struct Closure *f) {
	puts("Starting trace recording");
	struct JitState *state;
	if (!(state = calloc(1, sizeof *state))) return NULL;
	state->start = f;
	state->base = state->slots;
	return state;
}

/* Reverse linear-scan register allocation.
 */

// x86 codegen:
//
// * What registers to reserve?
//   - The lisp context in rdi (1st integer argument register)
//   - stack pointer in rbx (callee saved)

#define REG_NONE 0x80 ///< Register flag for spilled or unallocated.
#define SPILL_SLOT_NONE 0xff

typedef uint32_t RegSet;
static_assert(NUM_REGS <= CHAR_BIT * sizeof(RegSet));

/** Initial set of allocatable registers. */
#define REG_ALL (((1 << NUM_REGS) - 1) & ~(1 << rsp))
#define REG_LISP_CTX r15

struct RegAlloc {
	struct Assembler assembler;
	struct JitState *trace;
	// Register bit sets
	RegSet available, clobbers;
	/** The LuaJIT register cost model for each register. */
	alignas(32) uint16_t reg_costs[NUM_REGS];
	uint8_t num_spill_slots;
};

static bool has_reg(union SsaInstruction x) { return !(x.reg & REG_NONE); }

/** Spills one of the registers in @a mask. */
static enum Register reg_evict(struct RegAlloc *ctx, RegSet mask) {
	assert(!(ctx->available & mask) && "Redundant eviction.");
	mask &= ~ctx->available & REG_ALL;
	// TODO AVX512F _mm512_mask_reduce_min_epu32
	uint16_t min_cost = UINT16_MAX;
	for (unsigned i = 0; i < NUM_REGS; ++i)
		if (LIKELY(1 << i & mask)) min_cost = MIN(ctx->reg_costs[i], min_cost);
	assert(min_cost < UINT16_MAX && "No available registers.");
	uint16_t ref = min_cost;
	if (IS_VAR_REF(ref)) {
		union SsaInstruction *x = &IR_GET(ctx->trace, ref);
		assert(has_reg(*x) && "Evicting unallocated virtual?");
		if (x->spill_slot == SPILL_SLOT_NONE
			&& (x->spill_slot = ctx->num_spill_slots++) == UINT8_MAX) // Allocate spill slot
			die("TODO Out of spill slots");
		enum Register reg = x->reg;
		x->reg |= REG_NONE;
		// Reload from stack
		asm_rmro(&ctx->assembler, XI_MOVrm, reg, rsp, x->spill_slot * sizeof x->v);
		ctx->available |= 1 << reg;
		return reg;
	} else {
		// (Re-)materialize constant
		unreachable();
	}
}

enum RegMode { /* definition */ REG_DEF, /* usage */ REG_USE };

/** Allocates a register for @a ref.
 *
 * @param mode Whether a definition or use.
 * @param mask Allowed registers, for fixed allocations.
 */
static enum Register reg_alloc(struct RegAlloc *ctx, enum RegMode mode,
	uint16_t ref, RegSet mask) {
	union SsaInstruction *x = &IR_GET(ctx->trace, ref);
	enum Register reg = x->reg;
	RegSet available = ctx->available & mask;
	if (IS_VAR_REF(ref) && ((has_reg(*x) && 1 << reg & mask) // Already allocated
			|| (reg != 0xff && 1 << (reg &= ~REG_NONE) & available))) goto out;
	reg = available
		// Prefer callee-saved registers
		? stdc_trailing_zeros(available & CALLEE_SAVED_REGS
			? available & CALLEE_SAVED_REGS : available)
		: reg_evict(ctx, mask);
	ctx->clobbers |= 1 << reg;
	if (!IS_VAR_REF(ref)) {
		asm_loadu64(&ctx->assembler, reg, x->v);
		return reg;
	}
out:
	switch (mode) {
	case REG_DEF:
		if (has_reg(*x)) {
			if (reg != x->reg) asm_mov_reg_reg(&ctx->assembler, x->reg, reg);
			ctx->available |= 1 << x->reg; // Free register
		} else if (x->reg == 0xff) x->reg = reg;

		// Save ASAP for all exits to see the same spilled value.
		// (Snapshots always prefer spill slot over register for split virtuals.)
		if (x->spill_slot != SPILL_SLOT_NONE)
			asm_rmro(&ctx->assembler, XI_MOVrr, reg, rsp, x->spill_slot * sizeof x->v);
		break;
	case REG_USE:
		if (has_reg(*x)) // Existing allocation was masked
			asm_mov_reg_reg(&ctx->assembler, reg, x->reg);
		else if (x->reg != 0xff && (x->reg & ~REG_NONE) != reg)
			// Spilled but later assigned an unavailable register
			assert(false && "TODO Mov to reg from spill slot");
		else {
			ctx->available &= ~(1 << reg);
			ctx->reg_costs[x->reg = reg] = ref;
		}
		break;
	}
	return reg;
}

static void assemble_trace(struct JitState *trace) {
	struct RegAlloc ctx = { .trace = trace, .available = REG_ALL, };
	if (!asm_init(&ctx.assembler)) die("asm_init failed");
	// Initialize registers as unallocated
	for (unsigned i = 0; i < trace->count; ++i) {
		union SsaInstruction *x = trace->trace + MAX_CONSTS + i;
		x->spill_slot = x->reg = -1;
	}

	for (unsigned i = trace->count; i-- > 0;) {
		union SsaInstruction *x = trace->trace + MAX_CONSTS + i;
		uint16_t ref = IR_BIAS + i;
		switch (x->op) {
		case IR_SLOAD:
			enum Register reg = reg_alloc(&ctx, REG_DEF, ref, -1);
			asm_rmro(&ctx.assembler, XI_MOVrm, reg, rbp, x->a * sizeof x->v);
			break;
		case IR_GLOAD:
			reg = reg_alloc(&ctx, REG_DEF, ref, -1);
			break;
		case IR_ULOAD:
			reg = reg_alloc(&ctx, REG_DEF, ref, -1);
			break;

		case IR_EQ:
		case IR_NEQ:
			if (x->ty == LISP_CLOSURE) break; // TODO
			break;

		case IR_CALL:
			// Evict all caller-saved registers
			for (RegSet xs = REG_ALL & ~ctx.available & ~CALLEE_SAVED_REGS; xs; xs &= xs - 1)
				reg_evict(&ctx, 1 << stdc_trailing_zeros(xs));

			reg_alloc(&ctx, REG_DEF, ref, 1 << rax);
			// TODO Emit CALL

			uint32_t arg_regs
				= rdi | rsi << 4 | rdx << 8 | rcx << 12 | r8 << 16 | r9 << 20;
			for (unsigned j = x->b; j-- > 0;) {
				enum Register arg_reg = arg_regs & 0xf;
				arg_regs >>= 4;
				assert(arg_reg && "TODO Pass additional arguments on stack");

				uint16_t arg_ref = trace->trace[MAX_CONSTS + i + 1 + j].a;
				reg_alloc(&ctx, REG_USE, arg_ref, 1 << arg_reg);
			}

			break;
		case IR_CALLARG: break;

		case IR_LOOP:
			// TODO
			break;

		case IR_PHI:
			assert(IS_VAR_REF(x->a) && IS_VAR_REF(x->b) && "Variant ref cannot be constant.");
			union SsaInstruction *out = &IR_GET(trace, x->b);
			// Allocate register for in
			enum Register dst = reg_alloc(&ctx, REG_USE, x->a, -1);
			// φ-resolution
			if (has_reg(*out))
				// Outgoing variable already has a location: Move from out to in
				asm_mov_reg_reg(&ctx.assembler, dst, out->reg);
			else ctx.reg_costs[out->reg = dst] = x->b;
			break;

		case IR_NOP: break;
		default: unreachable();
		}
	}
}

#ifdef DEBUG
static void print_ir_ref(struct JitState *state, IrType ty, uint16_t ref) {
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

static void print_trace(struct JitState *state, enum TraceLink link) {
	const char *type_names[]
		= { "AxB", "sym", "str", "cfn", "clo", NULL, NULL, "nil", "int", "___" };

	puts("---- TRACE IR");
	for (unsigned i = 0, snap_idx = 0; i < state->count; ++i) {
		struct Snapshot *snap = state->snapshots + snap_idx;
		if (snap_idx < state->num_snapshots && i >= snap->ir_start) {
			printf("....         SNAP  #%-3u [ ", snap_idx++);
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
		printf("%.4u %s %3u ", i, type_names[x.ty], x.reg);
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
		case IR_ULOAD:
			printf("ULOAD #%" PRIu16 " from ", x.b);
			print_ir_ref(state, LISP_CLOSURE, x.a);
			putchar('\n');
			break;
		case IR_CALL: printf("CALL  ");
			print_ir_ref(state, x.ty, x.a);
			printf("  (");
			for (uint16_t j = 0; j < x.b; ++j) {
				union SsaInstruction arg = state->trace[MAX_CONSTS + ++i];
				if (j > 0) printf(" ");
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

bool record_instruction(struct JitState *state, uintptr_t *bp, struct Instruction *pc) {
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
	case GETUPVALUE: result = uref(state, bp, x.c); break;
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
		IrRef ref = sload(state, x.a), cur = *state->base,
			fn_ref = emit_const(state, LISP_CLOSURE, (union SsaInstruction) { .v = fun_value });
		take_snapshot(state);
		// TODO Guard on prototype only
		emit_folded(state, (union SsaInstruction)
			{ .op = IR_EQ, .ty = LISP_CLOSURE, .a = ref, .b = fn_ref });
		// Move args down to current frame
		memmove(state->base, state->base + x.a, (2 + x.c) * sizeof *state->base);
		state->num_slots = state->base_offset + 2 + x.c;
		if (fun_value == TAG_OBJ(state->start)) {
			*state->base = cur; // Avoid reloading as it is the same
			dce(state);
			peel_loop(state);
			assemble_trace(state);
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
