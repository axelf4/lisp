/** Tracing just-in-time compiler. */

#include <stdio.h>
#include <inttypes.h>
#include <assert.h>
#include "lisp.h"
#include "asm.h"

#define IR_BIAS 0x8000u
#define IR_REF_TYPE_SHIFT 16
#define IS_VAR_REF(ref) ((Ref) (ref) >= IR_BIAS)
#define IR_GET(state, ref) (state)->trace[(Ref) (ref) - IR_BIAS + MAX_CONSTS]

#define MAX_CONSTS 256
#define MAX_TRACE_LEN 512
#define MAX_SNAPSHOTS 64
#define MAX_SNAPSHOT_ENTRIES UINT8_MAX

#define REG_NONE 0x80 ///< Register flag for spilled or unallocated.
#define SPILL_SLOT_NONE 0xff

/** Superset of @ref LispObjectType. */
typedef uint8_t IrType;

enum {
	TY_ANY = LISP_INTEGER + 1,
	TY_RET_ADDR,
};

enum SsaOp : uint8_t {
	IR_NOP,
	IR_SLOAD, ///< Load stack slot.
	IR_GLOAD, ///< Load global.
	IR_ULOAD, ///< Load upvalue.
	// Comparisons are ordered such that flipping the LSB inverts them
	IR_EQ, IR_NEQ,
	IR_CALL, ///< Call C function.
	IR_CALLARG, ///< Argument for C function call.
	IR_LOOP,
	IR_PHI,
	IR_NUM_OPS
};

typedef uint16_t Ref; ///< Reference to a virtual or constant of a trace.
typedef uint32_t IrRef;

union SsaInstruction {
	struct {
		enum SsaOp op;
		IrType ty; ///< The type of the instruction result.
		Ref a, ///< Operand 1.
			b; ///< Operand 2.
		union {
			Ref prev;
			struct {
				uint8_t reg, ///< The allocated register.
					spill_slot;
			};
		};
	};
	LispObject v; ///< Object constant.
};

/** Snapshot stack slot entry. */
struct SnapshotEntry {
	Ref ref; ///< The content to write.
	uint8_t ty,
		slot; ///< The stack slot to restore.
};

/** PC and list of stack slots to restore on exit.
 *
 * When taking an exit, the computations performed hitherto need to be
 * transferred to the stack for the interpreter to continue from where
 * the compiled trace left off.
 */
struct Snapshot {
	/// Number of instructions executed before this snapshot was taken.
	uint16_t ir_start,
		offset; ///< Offset into @ref JitState::snap_entries.
	uint8_t num_stack_entries, frame_offset;
	struct GcRef pc; ///< Instruction pointer after this point.
};

enum TraceLink {
	/* TRACE_LINK_NONE, ///< No link yet, the trace is incomplete. */
	/**
	 * Abort the trace recording, e.g. due to NYI (not yet
	 * implemented) or return from the initial function.
	 */
	TRACE_LINK_ABORT,
	TRACE_LINK_LOOP, ///< Loop to itself.
};

struct LispTrace {
	void (*f)();
	uint16_t len, num_consts;
	uint8_t num_snapshots, num_spill_slots, arity;
	uint32_t clobbers;
	alignas(union SsaInstruction) char data[];
};

uint8_t trace_arity(struct LispTrace *trace) { return trace->arity; }

/** In-progress trace recording. */
struct JitState {
	uint16_t len, num_consts;
	uint8_t num_snapshots, num_stack_entries,
		base_offset, num_slots, frame_depth;
	bool need_snapshot;
	IrRef slots[0xff], ///< Array of IR references for each VM register.
		*bp; ///< Pointer into #slots at the current frame offset.
	Ref chain[IR_NUM_OPS];

	struct Instruction *pc;
	struct Closure *origin; ///< The closure that triggered the recording.

	struct LispCtx *lisp_ctx;
	uint16_t num_traces;

	struct Snapshot snapshots[MAX_SNAPSHOTS];
	/// Backing storage for snapshot data.
	struct SnapshotEntry stack_entries[MAX_SNAPSHOT_ENTRIES];
	union SsaInstruction trace[MAX_CONSTS + MAX_TRACE_LEN];
};

static void take_snapshot(struct JitState *state) {
	if (!state->need_snapshot) return;
	if (state->num_snapshots >= MAX_SNAPSHOTS / 2
		|| state->num_stack_entries + state->num_slots >= MAX_SNAPSHOT_ENTRIES / 2)
		throw(1); // Too many snapshots
	struct Snapshot *snap = state->snapshots + state->num_snapshots++;
	*snap = (struct Snapshot) {
		.ir_start = state->len, .offset = state->num_stack_entries,
		.frame_offset = state->base_offset, .pc = GC_COMPRESS(state->pc - 1),
	};
	for (unsigned i = 0; i < state->num_slots; ++i) {
		IrRef ref = state->slots[i];
		if (!ref) continue;
		union SsaInstruction x = IR_GET(state, ref);
		// Skip unmodified SLOADs
		if (IS_VAR_REF(ref) && x.op == IR_SLOAD && x.a == i
			// Do not skip closure/ret-addr in nested frames
			&& state->slots[i + 1] >> IR_REF_TYPE_SHIFT != TY_RET_ADDR) continue;
		state->stack_entries[state->num_stack_entries++] = (struct SnapshotEntry)
			{ .slot = i, .ref = ref, .ty = ref >> IR_REF_TYPE_SHIFT };
		++snap->num_stack_entries;
	}
	state->need_snapshot = false;
}

static IrRef emit(struct JitState *state, union SsaInstruction x) {
	if (state->len >= MAX_TRACE_LEN) throw(1);
	x.prev = state->chain[x.op];
	Ref ref = state->chain[x.op] = IR_BIAS + state->len++;
	IR_GET(state, ref) = x;
	return x.ty << IR_REF_TYPE_SHIFT | ref;
}

/** Emits @a x after peephole optimizations and CSE. */
static IrRef emit_folded(struct JitState *state, union SsaInstruction x) {
	switch (x.op) {
	case IR_NOP: return 0;
	case IR_SLOAD: return state->slots[x.a];
	case IR_EQ: case IR_NEQ:
		if (!IS_VAR_REF(x.a) && !IS_VAR_REF(x.b)) {
			if ((x.a == x.b) ^ (x.op == IR_NEQ)) return 0;
			else throw(1);
		}
		break;
	case IR_ULOAD: case IR_GLOAD: break; // TODO Check if set since last load
	default: goto no_cse;
	}
	// Common Subexpression Elimination (CSE)
	for (Ref ref = state->chain[x.op]; ref;) {
		union SsaInstruction o = IR_GET(state, ref);
		if (o.a == x.a && o.b == x.b) return o.ty << IR_REF_TYPE_SHIFT | ref;
		ref = o.prev;
	}
no_cse: return emit(state, x);
}

static IrRef emit_const(struct JitState *state, IrType ty, uintptr_t x) {
	Ref i;
	for (i = IR_BIAS; i-- > IR_BIAS - state->num_consts;)
		if (GC_COMPRESS(IR_GET(state, i).v).p == GC_COMPRESS(x).p) goto out;
	if (state->num_consts >= MAX_CONSTS) throw(1);
	IR_GET(state, i = IR_BIAS - ++state->num_consts) = (union SsaInstruction) { .v = x };
out: return ty << IR_REF_TYPE_SHIFT | i;
}

/** Emits a stack load instruction unless the stack slot content is already loaded. */
static IrRef sload(struct JitState *state, int slot) {
	IrRef ref = state->bp[slot];
	if (LIKELY(ref)) return ref; // Already loaded
	take_snapshot(state); // Type guard may be added
	unsigned min_num_slots = state->base_offset + slot + 1;
	if (state->num_slots < min_num_slots) state->num_slots = min_num_slots;
	return state->bp[slot] = emit(state, (union SsaInstruction)
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
	return emit_folded(state, (union SsaInstruction) { .op = IR_ULOAD,
			.ty = TY_ANY, .a = sload(state, /* this closure */ 0), .b = idx });
}

static void assert_type(struct JitState *state, IrRef *ref, IrType ty) {
	if (*ref >> IR_REF_TYPE_SHIFT == ty || !LIKELY(IS_VAR_REF(*ref))) return;
	// If ty </: ref->ty, then type error is imminent
	*ref = (IR_GET(state, *ref).ty = ty) << IR_REF_TYPE_SHIFT | (Ref) *ref;
}

static void assert_value(struct JitState *state, IrRef *ref, LispObject value) {
	if (!IS_VAR_REF(*ref)) return;
	enum LispObjectType ty = lisp_type(value);
	take_snapshot(state);
	emit_folded(state, (union SsaInstruction)
		{ .op = IR_EQ, .ty = ty, .a = *ref, .b = emit_const(state, ty, value) });
	assert_type(state, ref, ty);
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
		for (struct SnapshotEntry *entry = state->stack_entries + snap->offset,
					*end = entry + snap->num_stack_entries; entry < end; ++entry)
			if (IS_VAR_REF(entry->ref)) IR_GET(state, entry->ref).ty |= IR_MARK;
	Ref *pchain[IR_NUM_OPS];
	for (unsigned i = 0; i < IR_NUM_OPS; ++i) pchain[i] = state->chain + i;
	for (union SsaInstruction *xs = state->trace + MAX_CONSTS,
				*x = xs + state->len; x-- > xs;) { // Sweep
		if (!(x->ty & IR_MARK || has_side_effects(x->op))) {
			*pchain[x->op] = x->prev;
			x->op = IR_NOP;
			continue;
		}
		x->ty &= ~IR_MARK;
		pchain[x->op] = &x->prev;
		// Propagate marks
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
	unsigned preamble_len = state->len;
	// Separate preamble from loop body by LOOP instruction
	emit(state, (union SsaInstruction) { .op = IR_LOOP, .ty = TY_ANY });
	state->need_snapshot = true;
	struct Snapshot *snap = state->snapshots,
		*loopsnap = (take_snapshot(state), snap + state->num_snapshots - 1);

	// Map of variables in the preamble onto variables of the peeled loop
	Ref subst[MAX_TRACE_LEN], phis[12];
	unsigned num_phis = 0;
	for (unsigned i = 0; i < preamble_len; ++i) {
		if (i >= snap->ir_start) { // Copy-substitute the next snapshot
			struct Snapshot *s = state->snapshots + state->num_snapshots;
			unsigned offset = state->num_stack_entries;
			if (s[-1].ir_start < state->len) ++state->num_snapshots;
			else offset = (--s)->offset; // Overwrite previous snapshot
			*s = (struct Snapshot)
				{ .ir_start = state->len, .offset = offset,
				  .frame_offset = snap->frame_offset, .pc = snap->pc };
			struct SnapshotEntry
				*o = state->stack_entries + snap->offset, *oend = o + snap++->num_stack_entries,
				*l = state->stack_entries + loopsnap->offset, *lend = l + loopsnap->num_stack_entries,
				*n = state->stack_entries + offset;
			while (o < oend) {
				if (l < lend) {
					if (l->slot < o->slot) { *n++ = *l++; continue; }
					if (l->slot == o->slot) ++l; // Shadowed
				}
				struct SnapshotEntry x = *o++;
				if (IS_VAR_REF(x.ref)) x.ref = subst[x.ref % IR_BIAS];
				*n++ = x;
			}
			while (l < lend) *n++ = *l++;
			s->num_stack_entries = n - (state->stack_entries + offset);
			state->num_stack_entries = offset + s->num_stack_entries;
		}

		union SsaInstruction ins = state->trace[MAX_CONSTS + i];
		if (IS_VAR_REF(ins.a)) ins.a = subst[ins.a - IR_BIAS];
		if (IS_VAR_REF(ins.b)) ins.b = subst[ins.b - IR_BIAS];

		IrRef ref = emit_folded(state, ins);
		uint16_t j = (subst[i] = ref) - IR_BIAS;
		if (j != i && j < preamble_len) { // Loop-carried dependency
			if (IS_VAR_REF(ref)) {
				// SLOAD:s of arguments varied in tail call give rise to φ:s
				if (num_phis >= LENGTH(phis)) throw(1);
				phis[num_phis++] = ref;
			}
			// In case of type-instability, need to emit conversion
			// since later instructions expect the previous type.
			assert(ref >> IR_REF_TYPE_SHIFT == ins.ty && "TODO Type-instability");
		}
	}

	for (unsigned i = 0; i < num_phis; ++i) { // Emit φ-functions
		Ref lref = phis[i], rref = subst[lref - IR_BIAS];
		if (lref == rref) continue; // Invariant φ
		emit(state, (union SsaInstruction)
			{ .op = IR_PHI, .ty = IR_GET(state, lref).ty, .a = lref, .b = rref });
	}
}

[[gnu::used]] static struct SideExitResult {
	struct Instruction *pc;
	uint8_t base_offset, spill_slots;
	uint32_t clobbers;
} side_exit_handler_inner(struct LispCtx *ctx, uintptr_t *regs) {
	struct LispTrace *trace = ctx->current_trace;
	uint8_t exit_num = *regs;
	union SsaInstruction *instructions = (union SsaInstruction *) trace->data;
	struct Snapshot *snapshots
		= (struct Snapshot *) (instructions + trace->num_consts + trace->len),
		*snapshot = snapshots + exit_num;
	struct SnapshotEntry *snapshot_entries
		= (struct SnapshotEntry *) (snapshots + trace->num_snapshots);
	printf("Side exit %" PRIu8 "\n", exit_num);
	for (struct SnapshotEntry *e = snapshot_entries + snapshot->offset,
				*end = e + snapshot->num_stack_entries; e < end; ++e) {
		union SsaInstruction ins = instructions[e->ref - IR_BIAS + trace->num_consts];
		ctx->bp[e->slot] = IS_VAR_REF(e->ref)
			? regs[ins.spill_slot == SPILL_SLOT_NONE ? -ins.reg - 1 : 1 + ins.spill_slot]
			: ins.v;
		printf("Restoring stack slot %" PRIu8 " to value: ", e->slot);
		lisp_print(ctx, ctx->bp[e->slot]);
		puts(".");
	}
	struct Instruction *pc = (struct Instruction *) GC_DECOMPRESS(ctx, snapshot->pc);
	return (struct SideExitResult) { pc, snapshot->frame_offset, trace->num_spill_slots, trace->clobbers };
}

__asm__ (
	"side_exit_handler:\n\t"
	// Push all GP registers
	"push rax; push rcx; push rdx; push rbx; push rsp; push rbp; push rsi; push rdi\n\t"
	"mov rdi, " STR(REG_LISP_CTX) "\n\t" // Pass Lisp context
	"lea rsi, [rsp+8*8]\n\t"
	"push r8; push r9; push r10; push r11; push r12; push r13; push r14; push r15\n\t"
	"call side_exit_handler_inner\n\t"
	"mov " STR(REG_PC) ", rax\n\t"
	"xor eax, eax\n\t"
	"mov al, dh\n\t"
	"lea rsp, [rsp+8*(16+1+rax)]\n\t" // Pop stack
	// Restore callee-saved registers
	"bt rdx, 3+32; jnc 0f; pop rbx; 0:\n\t"
	"bt rdx, 5+32; jnc 0f; pop rbp; 0:\n\t"
	"bt rdx, 12+32; jnc 0f; pop r12; 0:\n\t"
	"bt rdx, 13+32; jnc 0f; pop r13; 0:\n\t"
	"bt rdx, 14+32; jnc 0f; pop r14; 0:\n\t"
	"ret");

/* Reverse linear-scan register allocation. */

#define REF_BP 0
/** Initial set of allocatable registers. */
#define REG_ALL (((1 << NUM_REGS) - 1) & ~(1 << rsp | 1 << REG_LISP_CTX))

typedef uint32_t RegSet;
static_assert(NUM_REGS <= CHAR_BIT * sizeof(RegSet));

struct RegAlloc {
	struct Assembler assembler;
	struct JitState *trace;
	RegSet available, clobbers, phi_regs;
	union SsaInstruction bp_ins; ///< Dummy virtual representing the BP.
	uint8_t num_spill_slots, snapshot_idx;
	/** The LuaJIT register cost model. */
	alignas(32) uint16_t reg_costs[NUM_REGS];
	Ref phis_refs[NUM_REGS]; ///< For each #phi_regs bit, its corresponding "in".
};

static bool has_reg(union SsaInstruction x) { return !(x.reg & REG_NONE); }

/** Spills @a ref, emitting a reload. */
static enum Register reload(struct RegAlloc *ctx, Ref ref) {
	enum Register reg;
	if (IS_VAR_REF(ref)) {
		union SsaInstruction *x = &IR_GET(ctx->trace, ref);
		assert(has_reg(*x) && "Evicting unallocated virtual?");
		printf("Evicting %" PRIu16 " from register %d\n", ref - IR_BIAS, x->reg);
		if (x->spill_slot == SPILL_SLOT_NONE
			&& (x->spill_slot = ctx->num_spill_slots++) >= UINT8_MAX) // Allocate spill slot
			throw(1); // Out of spill slots
		reg = x->reg;
		x->reg |= REG_NONE;
		// Reload from stack
		asm_rmrd(&ctx->assembler, 1, XI_MOVrm, reg, rsp, x->spill_slot * sizeof x->v);
	} else { // (Re-)materialize constant
		assert(ref == REF_BP);
		reg = ctx->bp_ins.reg;
		ctx->bp_ins.reg = -1;
		asm_rmrd(&ctx->assembler, 1, XI_MOVrm, reg, REG_LISP_CTX, offsetof(struct LispCtx, bp));
	}
	ctx->available |= 1 << reg;
	return reg;
}

/** Spills one of the registers in @a mask. */
static enum Register evict(struct RegAlloc *ctx, RegSet mask) {
	assert(!(ctx->available & mask) && "Redundant eviction");
	mask &= ~ctx->available & REG_ALL;
	uint16_t min_cost = UINT16_MAX;
	for (unsigned i = 0; i < NUM_REGS; ++i)
		if (LIKELY(1 << i & mask)) min_cost = MIN(ctx->reg_costs[i], min_cost);
	assert(min_cost < UINT16_MAX && "No available registers");
	return reload(ctx, min_cost);
}

/** Allocates a register for the definition of @a ref.
 *
 * @param mask Allowed registers, for fixed allocations.
 */
static enum Register reg_alloc(struct RegAlloc *ctx, Ref ref, RegSet mask) {
	assert(IS_VAR_REF(ref));
	union SsaInstruction *x = &IR_GET(ctx->trace, ref);
	RegSet available = ctx->available & mask;
	enum Register reg
		= has_reg(*x) && 1 << x->reg & mask ? x->reg // Already allocated
		: available ? stdc_trailing_zeros(available) : evict(ctx, mask);
	ctx->clobbers |= 1 << reg;
	if (has_reg(*x)) {
		if (reg != x->reg) // Existing allocation was masked
			asm_mov_reg_reg(&ctx->assembler, x->reg, reg);
		ctx->available |= 1 << x->reg; // Free register
	} else x->reg = reg;
	// Save ASAP for all exits to see the same spilled value.
	// (Snapshots always prefer spill slot over register for split virtuals.)
	if (x->spill_slot != SPILL_SLOT_NONE)
		asm_rmrd(&ctx->assembler, 1, XI_MOVrr, reg, rsp, x->spill_slot * sizeof x->v);
	return reg;
}

/** Allocates a register for a use of @a ref.
 *
 * If @a ref is a constant, a load must be emitted, otherwise, if @a
 * ref has a prior masked register, then a move is needed.
 */
static enum Register reg_use(struct RegAlloc *ctx, Ref ref, RegSet mask) {
	union SsaInstruction *x = ref == REF_BP ? &ctx->bp_ins
		: IS_VAR_REF(ref) ? &IR_GET(ctx->trace, ref) : NULL;
	if (x && has_reg(*x) && 1 << x->reg & mask) return x->reg; // Already allocated
	RegSet available = ctx->available & mask;
	enum Register reg;
	if (!(x && 1 << (reg = x->reg % stdc_bit_ceil_uc(NUM_REGS)) & available)) // Use hint
		// Prefer callee-saved registers
		reg = available ? stdc_trailing_zeros(available & CALLEE_SAVED_REGS
			? available & CALLEE_SAVED_REGS : available)
			: evict(ctx, mask);
	ctx->clobbers |= 1 << reg;
	assert(!(x && has_reg(*x)) && "TODO Need to move");
	if (x && !has_reg(*x)) {
		ctx->available &= ~(1 << reg);
		ctx->reg_costs[x->reg = reg] = ref;
	}
	return reg;
}

static void asm_guard(struct RegAlloc *ctx, enum Cc cc) {
	// Emit conditional jump to side exit trampoline
	uint8_t *target = ctx->assembler.buf + MCODE_CAPACITY - 5 - 2 - 4 * ctx->snapshot_idx;
	asm_write32(&ctx->assembler, rel32((uintptr_t) ctx->assembler.p, (uintptr_t) target));
	*--ctx->assembler.p = /* Jcc */ 0x80 | cc;
	*--ctx->assembler.p = 0x0f;
}

/** Assembles the @ref IR_LOOP instruction. */
static void asm_loop(struct RegAlloc *ctx, uint8_t *asm_end) {
	// Reload invariants whose registers get clobbered
	FOR_ONES(reg, ctx->clobbers & ~(ctx->available | ctx->phi_regs))
		reload(ctx, ctx->reg_costs[reg]);

	// φ-resolution
	// FIXME: For now shuffle φ-registers by spilling in-between
	FOR_ONES(reg, ctx->phi_regs) {
		Ref lref = ctx->phis_refs[reg];
		union SsaInstruction *in = &IR_GET(ctx->trace, lref);
		if (LIKELY(in->reg == reg)) continue;
		if (has_reg(*in)) reload(ctx, lref);
		if (!(1 << reg & ctx->available)) reload(ctx, ctx->reg_costs[reg]);
	}
	FOR_ONES(reg, ctx->phi_regs) {
		Ref lref = ctx->phis_refs[reg];
		union SsaInstruction *in = &IR_GET(ctx->trace, lref);
		if (LIKELY(in->spill_slot == SPILL_SLOT_NONE)) continue;
		// Resave spill slot of "in" on each iteration
		asm_rmrd(&ctx->assembler, 1, XI_MOVrr, reg, rsp, in->spill_slot * sizeof in->v);
		in->ty &= ~IR_MARK; // Remember that spill slot is handled by loop
		// Allocate (now free) register for correct save on 1st iteration too
		reg_use(ctx, lref, 1 << reg);
	}

	int32_t jmp_disp = rel32((uintptr_t) asm_end, (uintptr_t) ctx->assembler.p);
	memcpy((int32_t *) asm_end - 1, &jmp_disp, sizeof jmp_disp);
}

static struct LispTrace *assemble_trace(struct JitState *trace) {
	struct RegAlloc ctx = { .trace = trace, .available = REG_ALL, .bp_ins.reg = -1,
		.snapshot_idx = trace->num_snapshots - 1, };
	if (!asm_init(&ctx.assembler)) die("asm_init failed");
	// Emit side-exit trampolines
	void side_exit_handler();
	asm_write32(&ctx.assembler,
		rel32((uintptr_t) ctx.assembler.p, (uintptr_t) side_exit_handler));
	*--ctx.assembler.p = /* JMP */ 0xe9;
	for (unsigned i = 0; i < MAX_SNAPSHOTS; ++i) {
		if (i) {
			*--ctx.assembler.p = (4 * i - 2) & ~0x80; // Chain jumps if i>=32
			*--ctx.assembler.p = /* JMP */ 0xeb;
		}
		*--ctx.assembler.p = i;
		*--ctx.assembler.p = /* PUSH */ 0x6a;
	}

	// Initialize registers as unallocated
	for (unsigned i = 0; i < trace->len; ++i) {
		union SsaInstruction *x = trace->trace + MAX_CONSTS + i;
		x->spill_slot = x->reg = -1;
	}
	// Emit loop backedge placeholder
	uint8_t *asm_end = ctx.assembler.p;
	asm_write32(&ctx.assembler, 0);
	*--ctx.assembler.p = /* JMP */ 0xe9;

	for (unsigned i = trace->len; i-- > 0;) {
		if (ctx.snapshot_idx && i < trace->snapshots[ctx.snapshot_idx].ir_start)
			--ctx.snapshot_idx;

		Ref ref = IR_BIAS + i;
		union SsaInstruction x = IR_GET(trace, ref);
		switch (x.op) {
		case IR_SLOAD:
			enum Register reg = reg_alloc(&ctx, ref, -1), bp = reg_use(&ctx, REF_BP, -1);
			asm_rmrd(&ctx.assembler, 1, XI_MOVrm, reg, bp, x.a * sizeof x.v);
			break;
		case IR_GLOAD:
			reg = reg_alloc(&ctx, ref, -1);
			int32_t p = GC_COMPRESS(UNTAG_OBJ(IR_GET(trace, x.a).v)).p + offsetof(struct Symbol, value);
			asm_rmrd(&ctx.assembler, 1, XI_MOVrm, reg, REG_LISP_CTX, p);
			break;
		case IR_ULOAD: {
			enum Register reg = reg_alloc(&ctx, ref, -1), fn = reg_use(&ctx, x.a, -1);
			asm_rmrd(&ctx.assembler, 1, XI_MOVrm, reg, reg, 0);
			asm_rmrd(&ctx.assembler, 1, XI_MOVrm, reg, reg,
				offsetof(struct Upvalue, location));
			asm_rmrd(&ctx.assembler, 1, XI_MOVrm, reg, fn,
				offsetof(struct Closure, upvalues[x.b]) - 1);
			break;
		}

		case IR_EQ:
		case IR_NEQ:
			assert(IS_VAR_REF(x.a));
			asm_guard(&ctx, x.op == IR_EQ ? CC_NE : CC_E);
			enum Register reg1 = reg_use(&ctx, x.a, -1);
			if (IS_VAR_REF(x.b)) {
				enum Register reg2 = reg_use(&ctx, x.b, -1);
				*--ctx.assembler.p = MODRM(MOD_REG, reg2, reg1);
				*--ctx.assembler.p = XI_CMP;
				EMIT_REX(&ctx.assembler, 0, reg2, 0, reg1);
			} else asm_grp1_imm(&ctx.assembler, 0, /* CMP */ 7,
				reg1, (uint32_t) IR_GET(trace, x.b).v);
			break;

		case IR_CALL:
			// Evict caller-saved registers
			FOR_ONES(reg, REG_ALL & ~ctx.available & ~CALLEE_SAVED_REGS)
				reload(&ctx, ctx.reg_costs[reg]);
			asm_grp1_imm(&ctx.assembler, 1, /* ADD */ 0, rsp, x.b * sizeof x.v);

			reg_alloc(&ctx, ref, 1 << rax);
			enum Register f_reg = reg_use(&ctx, x.a, CALLEE_SAVED_REGS);
			asm_rmrd(&ctx.assembler, 0, XI_GRP5, /* CALL */ 2, f_reg, offsetof(struct LispCFunction, f) - 1);

			uint32_t arg_regs = rdi | rsi << 4 | rdx << 8 | rcx << 12 | r8 << 16 | r9 << 20;
			asm_mov_reg_reg(&ctx.assembler, arg_regs & 0xf, REG_LISP_CTX);
			asm_loadu64(&ctx.assembler, arg_regs >> 4 & 0xf, x.b);
			asm_mov_reg_reg(&ctx.assembler, arg_regs >> 2 * 4 & 0xf, rsp);
			for (unsigned j = 0; j <  x.b; ++j) {
				Ref arg_ref = trace->trace[MAX_CONSTS + i + 1 + j].a;
				union SsaInstruction arg = IR_GET(trace, arg_ref);
				if (!IS_VAR_REF(arg_ref) && (int8_t) arg.v == (intptr_t) arg.v) {
					*--ctx.assembler.p = arg.v;
					*--ctx.assembler.p = /* PUSH */ 0x6a;
					continue;
				}
				// TODO Fuse memory loads into PUSH
				enum Register arg_reg
					= IS_VAR_REF(arg_ref) ? reg_use(&ctx, arg_ref, -1) : rax;
				*--ctx.assembler.p = /* PUSH */ 0x50 + (arg_reg & 7);
				EMIT_REX(&ctx.assembler, 0, 0, 0, arg_reg);
				if (!IS_VAR_REF(arg_ref)) asm_loadu64(&ctx.assembler, arg_reg, arg.v);
			}
			break;
		case IR_CALLARG: break;

		case IR_LOOP: asm_loop(&ctx, asm_end); break;
		case IR_PHI:
			assert(IS_VAR_REF(x.a) && IS_VAR_REF(x.b) && "Variant ref cannot be constant");
			union SsaInstruction *in = &IR_GET(trace, x.a), *out = &IR_GET(trace, x.b);
			assert(stdc_count_ones(ctx.available) > 1 && "Out of φ registers");
			// Pick φ registers in opposite direction to reduce collisions
			enum Register dst = stdc_bit_width(ctx.available) - 1;
			if (LIKELY(!has_reg(*out))) reg_use(&ctx, x.b, 1 << dst);
			else { // Outgoing variable already has a location: Move from out to in
				ctx.clobbers |= 1 << dst;
				asm_mov_reg_reg(&ctx.assembler, dst, out->reg);
			}
			in->reg = dst | REG_NONE; // Add hint
			in->ty |= IR_MARK;
			ctx.phi_regs |= 1 << dst;
			ctx.phis_refs[dst] = x.a;
			break;
		case IR_NOP: break;
		default: unreachable();
		}
	}

	FOR_ONES(reg, ctx.phi_regs) {
		union SsaInstruction in = IR_GET(ctx.trace, ctx.phis_refs[reg]);
		if (in.spill_slot != SPILL_SLOT_NONE && in.ty & IR_MARK)
			throw(1); // Spilled outside loop
	}
	reload(&ctx, REF_BP);
	assert(ctx.available == REG_ALL);
	// TODO Align stack to 16-byte boundary
	asm_grp1_imm(&ctx.assembler, 1, /* SUB */ 5, rsp, ctx.num_spill_slots * sizeof(void *));
	// Save callee-saved registers
	FOR_ONES(reg, ctx.clobbers &= CALLEE_SAVED_REGS) {
		*--ctx.assembler.p = /* PUSH */ 0x50 + (reg & 7);
		EMIT_REX(&ctx.assembler, 0, 0, 0, reg);
	}

	struct LispTrace *result;
	if (!(result = malloc(sizeof *result
				+ (trace->num_consts + trace->len) * sizeof *trace->trace
				+ trace->num_snapshots * sizeof *trace->snapshots
				+ trace->num_stack_entries * sizeof *trace->stack_entries)))
		die("malloc failed");
	*result = (struct LispTrace) {
		.f = asm_assemble(&ctx.assembler),
		.len = trace->len, .num_consts = trace->num_consts,
		.num_snapshots = trace->num_snapshots,
		.arity = trace->origin->prototype->arity,
		.num_spill_slots = ctx.num_spill_slots, .clobbers = ctx.clobbers,
	};
	char *p = result->data;
	size_t size;
	memcpy(p, trace->trace + MAX_CONSTS - trace->num_consts,
		size = (trace->num_consts + trace->len) * sizeof *trace->trace);
	p += size;
	memcpy(p, trace->snapshots, size = trace->num_snapshots * sizeof *trace->snapshots);
	p += size;
	memcpy(p, trace->stack_entries, trace->num_stack_entries * sizeof *trace->stack_entries);
	return result;
}

#ifdef DEBUG
static void print_ir_ref(struct JitState *state, IrType ty, Ref ref) {
	if (IS_VAR_REF(ref)) { printf("%.4u", ref - IR_BIAS); return; }
	union SsaInstruction x = IR_GET(state, ref);
	switch (ty & ~IR_MARK) {
	case TY_ANY:
		if (NILP(x.v)) { case LISP_NIL: printf("nil "); break; }
		[[fallthrough]];
	case LISP_CFUNCTION: case LISP_CLOSURE: printf("%#" PRIxPTR, x.v); break;
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
	for (unsigned i = 0, snap_idx = 0; i < state->len; ++i) {
		struct Snapshot *snap = state->snapshots + snap_idx;
		if (snap_idx < state->num_snapshots && i >= snap->ir_start) {
			printf("....         SNAP  #%-3u [ ", snap_idx++);
			unsigned j = 0;
			for (struct SnapshotEntry *entry = state->stack_entries + snap->offset,
						*end = entry + snap->num_stack_entries; entry < end; ++entry) {
				while (j++ < entry->slot) printf("---- ");
				print_ir_ref(state, entry->ty, entry->ref);
				putchar(' ');
			}
			puts("]");
		}

		union SsaInstruction x = state->trace[MAX_CONSTS + i];
		printf("%.4u %s %3u ", i, type_names[x.ty & ~IR_MARK], x.reg);
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
			for (Ref j = 0; j < x.b; ++j) {
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

struct JitState *jit_new(struct LispCtx *ctx) {
	struct JitState *state;
	if (!(state = malloc(sizeof *state))) return NULL;
	state->lisp_ctx = ctx;
	state->num_traces = 0;
	return state;
}

void jit_free(struct JitState *state) {
	free(state);
}

bool jit_init(struct JitState *state, struct Closure *f) {
	puts("Starting trace recording");
	if (UNLIKELY(state->num_traces >= LENGTH(*state->lisp_ctx->traces))) return false;
	memset(state, 0, offsetof(struct JitState, pc));
	state->origin = f;
	state->bp = state->slots;
	state->need_snapshot = true;
	return true;
}

static void do_record(void *userdata) {
	struct JitState *state = userdata;
	uintptr_t *bp = state->lisp_ctx->bp;
	struct Instruction *pc = state->pc, x = pc[-1];
	IrRef result = 0;
	switch (x.op) {
	case LOAD_NIL: result = emit_const(state, LISP_NIL, NIL); break;
	case LOAD_OBJ:
		LispObject obj = *(LispObject *) (pc - x.b);
		result = emit_const(state, lisp_type(obj), obj);
		break;
	case LOAD_SHORT:
		result = emit_const(state, LISP_INTEGER, TAG_SMI((int16_t) x.b));
		break;
	case GETGLOBAL:
		IrRef sym = emit_const(state, LISP_SYMBOL, *(LispObject *) (pc - x.b));
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
			// Specialize to the function value in question
			assert_value(state, (IrRef[]) { sload(state, x.a) }, fun_value);
			struct Prototype *proto = ((struct Closure *) UNTAG_OBJ(fun_value))->prototype;
			if (proto->arity & PROTO_VARIADIC
				|| fun_value == TAG_OBJ(state->origin)) /* NYI */ throw(1);
			(state->bp += x.a)[1] = emit_const(state, TY_RET_ADDR, (uintptr_t) pc);
			state->base_offset += x.a;
			++state->num_slots;
			++state->frame_depth;
			break;
		default: throw(1);
		}
		break;
	case TAIL_CALL:
		fun_value = bp[x.a];
		// Specialize to the function value in question
		// TODO Guard on prototype only
		IrRef ref = sload(state, x.a), cur = *state->bp;
		assert_value(state, &ref, fun_value);
		// Move args down to current frame
		memmove(state->bp, state->bp + x.a, (2 + x.c) * sizeof *state->bp);
		state->num_slots = state->base_offset + 2 + x.c;
		if (fun_value == TAG_OBJ(state->origin)) {
			*state->bp = cur; // Avoid reloading closure
			dce(state);
			peel_loop(state);
			struct LispTrace *trace = assemble_trace(state);
#ifdef DEBUG
			puts("Reached loop start! Trace:\n");
			print_trace(state, TRACE_LINK_LOOP);
#endif
			uint16_t trace_num = state->num_traces++;
			(*state->lisp_ctx->traces)[trace_num] = trace;
			pc[-1] = (struct Instruction) { .op = TAIL_JIT_CALL, .a = pc[-1].a, .b = trace_num };
			throw(1);
		}
		puts("Tail calls not yet implemented, aborting trace...");
		throw(1);
	case MOV: result = sload(state, x.c); break;
	case JMP: break;
	case JNIL:
		ref = sload(state, x.a);
		IrType ty = ref >> IR_REF_TYPE_SHIFT;
		if (ty != TY_ANY) break; // Constant NIL comparison is a no-op
		IrRef nil = emit_const(state, LISP_NIL, NIL);
		take_snapshot(state);
		emit_folded(state, (union SsaInstruction)
			{ .op = IR_EQ ^ !NILP(bp[x.a]), .ty = ty, .a = ref, .b = nil });
		break;
	case RET:
		if (state->frame_depth--) {
			result = state->bp[x.a];
			uint8_t offset = x.a = ((struct Instruction *) bp[1])[-1].a;
			state->bp -= offset;
			state->base_offset -= offset;
			break;
		}
		[[fallthrough]];
	case TAIL_JIT_CALL: // Await side trace instead
	default:
		printf("Instruction %" PRIu8 " is NYI, aborting trace...\n", x.op);
		throw(1);
	}

	if (result) {
		state->num_slots = MAX(state->num_slots, state->base_offset + x.a + 1);
		state->bp[x.a] = result;
	}
}

bool jit_record(struct JitState *state, struct Instruction *pc) {
	state->pc = pc;
	return !pcall(state, do_record);
}
