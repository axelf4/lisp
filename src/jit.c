/** Tracing just-in-time compiler. */

#include <stdio.h>
#include <inttypes.h>
#include <assert.h>
#include "lisp.h"
#include "asm.h"

#define IR_BIAS 0x8000u
#define IR_REF_TYPE_SHIFT 16
#define IS_VAR(ref) ((Ref) (ref) >= IR_BIAS)
#define IR_GET(state, ref) (state)->trace[(Ref) (ref) - IR_BIAS + MAX_CONSTS]

#define MAX_CONSTS 256
#define MAX_TRACE_LEN 512
#define MAX_SNAPSHOTS 64
#define MAX_SNAPSHOT_ENTRIES UINT8_MAX

#define IR_MARK 0x80 ///< Multi-purpose flag.
#define REG_NONE 0x80 ///< Spilled or unallocated register flag.
#define SPILL_SLOT_NONE 0xff

enum {
	TY_ANY = LISP_INTEGER + 1,
	TY_RET_ADDR,
};

enum SsaOp : uint8_t {
	// Comparisons are ordered such that flipping the LSB inverts them
	IR_EQ, IR_NEQ,
	IR_SLOAD, ///< Load stack slot.
	IR_GLOAD, ///< Load global.
	IR_ULOAD, ///< Load upvalue.
	IR_CALL, ///< Call C function.
	IR_CALLARG, ///< Argument for C function call.
	IR_LOOP,
	IR_PHI,
	IR_NOP,

	IR_ADD,

	IR_NUM_OPS
};

typedef uint16_t Ref; ///< Reference to a virtual or constant of a trace.
typedef uint32_t IrRef;

union SsaInstruction {
	struct {
		enum SsaOp op;
		uint8_t ty; ///< Type of the result (superset of @ref LispObjectType).
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

/** Patchpoint header for on-stack replacement.
 *
 * When taking an exit, computations performed hitherto need to be
 * transferred to the stack for the interpreter to continue from where
 * the compiled trace left off. The snapshot contains the PC and list
 * of stack slots to restore.
 */
struct Snapshot {
	/// Number of instructions recorded when this snapshot was taken.
	uint16_t ir_start,
		offset; ///< Offset into @ref JitState::stack_entries.
	uint8_t num_entries, base_offset;
	struct GcRef pc; ///< Instruction pointer after this point.
};

enum TraceLink {
	/* TRACE_LINK_NONE, ///< No link yet, the trace is incomplete. */
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

enum RecordStatus : unsigned char {
	REC_OK, ///< Incomplete but OK trace recording.
	/// Abort the trace recording, e.g., due to NYI (not yet implemented).
	REC_NYI,
	REC_DONE,
};

/** Closure JIT penalty FIFO-cache slot. */
struct Penalty {
	struct Closure *closure;
	unsigned char value;
};

/** In-progress trace recording. */
struct JitState {
	uint16_t len, num_consts;
	uint8_t num_snapshots, num_stack_entries,
		base_offset, max_slot;
	bool need_snapshot;
	enum RecordStatus rec_status;
	IrRef slots[0x100], ///< Array of IR references for each VM register.
		*bp; ///< Pointer into @ref #slots at the current frame offset.
	Ref chain[IR_NUM_OPS];

	struct Instruction *pc;
	struct Closure *origin; ///< The closure that triggered the recording.
	struct Instruction *origin_pc;

	struct LispCtx *lisp_ctx;
	uint16_t num_traces;
	unsigned char next_penalty_slot;

	struct Snapshot snapshots[MAX_SNAPSHOTS];
	/// Backing storage for snapshot data.
	struct SnapshotEntry stack_entries[MAX_SNAPSHOT_ENTRIES];
	union SsaInstruction trace[MAX_CONSTS + MAX_TRACE_LEN];

	struct Penalty penalties[32];
};

[[gnu::cold]]
static void rec_err(struct JitState *state) { state->rec_status = REC_NYI; }

static void take_snapshot(struct JitState *state) {
	if (!state->need_snapshot) return;
	state->need_snapshot = false;
	uint8_t max_slot = state->base_offset + state->max_slot;
	if (state->num_snapshots >= MAX_SNAPSHOTS / 2
		|| state->num_stack_entries + max_slot >= MAX_SNAPSHOT_ENTRIES / 2) // TODO
		rec_err(state); // Too many snapshots
	struct Snapshot *snapshot = state->snapshots + state->num_snapshots++;
	*snapshot = (struct Snapshot) {
		.ir_start = state->len, .offset = state->num_stack_entries,
		.base_offset = state->base_offset, .pc = GC_COMPRESS(state->pc - 1),
	};
	for (uint8_t i = 0; i <= max_slot; ++i) {
		IrRef ref = state->slots[i];
		if (!ref) continue;
		union SsaInstruction x = IR_GET(state, ref);
		// Skip unmodified SLOADs
		if (IS_VAR(ref) && x.op == IR_SLOAD && x.a == i
			// Do not skip closure in nested frames
			&& state->slots[i + 1] >> IR_REF_TYPE_SHIFT != TY_RET_ADDR) continue;
		state->stack_entries[state->num_stack_entries++] = (struct SnapshotEntry)
			{ .slot = i, .ref = ref, .ty = ref >> IR_REF_TYPE_SHIFT };
		++snapshot->num_entries;
	}
}

static IrRef emit_const(struct JitState *state, uint8_t ty, uintptr_t x) {
	Ref i = IR_BIAS;
	while (i-- > IR_BIAS - state->num_consts)
		if (LISP_EQ(IR_GET(state, i).v, x)) goto out;
	if (state->num_consts >= MAX_CONSTS) { rec_err(state); return 0; }
	IR_GET(state, i = IR_BIAS - ++state->num_consts).v = x;
out: return ty << IR_REF_TYPE_SHIFT | i;
}

static IrRef emit(struct JitState *state, union SsaInstruction x) {
	if (state->len >= MAX_TRACE_LEN) { rec_err(state); return 0; }
	x.prev = state->chain[x.op];
	Ref ref = state->chain[x.op] = IR_BIAS + state->len++;
	IR_GET(state, ref) = x;
	return x.ty << IR_REF_TYPE_SHIFT | ref;
}

/** Normalizes a commutative instruction. */
static union SsaInstruction comm_swap(union SsaInstruction x) {
	// Swap lower refs (constants in particular) to the right
	if (x.a < x.b) { Ref tmp = x.a; x.a = x.b; x.b = tmp; }
	return x;
}

/** Emits @a x after peephole optimizations and CSE. */
static IrRef emit_opt(struct JitState *state, union SsaInstruction x) {
	union SsaInstruction o;
	switch (x.op) {
	case IR_NOP: return 0;
	case IR_SLOAD: return state->slots[x.a];
	case IR_EQ: case IR_NEQ:
		if (x.a == x.b) { if (x.op == IR_NEQ) rec_err(state); return 0; }
		x = comm_swap(x);
		if (!IS_VAR(x.a)) rec_err(state);
		break;
	case IR_GLOAD: case IR_ULOAD: break; // Forward loads of globals/upvalues
	default: goto out_no_cse;

	case IR_ADD:
		x = comm_swap(x);
		if (IR_GET(state, x.b).v == TAG_SMI(0)) return x.a;
		if (!IS_VAR(x.a)) return emit_const(state, LISP_INTEGER,
			IR_GET(state, x.a).v + IR_GET(state, x.b).v);
		break;
	}
	// Common Subexpression Elimination (CSE)
	for (Ref ref = state->chain[x.op]; ref; ref = o.prev) {
		o = IR_GET(state, ref);
		if (o.a == x.a && o.b == x.b) return o.ty << IR_REF_TYPE_SHIFT | ref;
	}
out_no_cse: return emit(state, x);
}

/** Emits a stack load instruction unless the stack slot content is already loaded. */
static IrRef sload(struct JitState *state, int slot) {
	IrRef ref = state->bp[slot];
	if (LIKELY(ref)) return ref; // Already loaded
	take_snapshot(state); // Type guard may be added
	state->max_slot = MAX(state->max_slot, slot);
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
	return emit_opt(state, (union SsaInstruction) { .op = IR_ULOAD,
			.ty = TY_ANY, .a = sload(state, /* this closure */ 0), .b = idx });
}

/** Coerces @a ref to @a ty.
 *
 * @return Whether @a ty was a subtype of the type of @a ref.
 */
static bool assert_type(struct JitState *state, IrRef *ref, uint8_t ty) {
	uint8_t ty2 = *ref >> IR_REF_TYPE_SHIFT;
	if (ty2 == ty) return true;
	// If ty </: ref->ty, then type error is imminent
	if (IS_VAR(*ref))
		*ref = (IR_GET(state, *ref).ty = ty) << IR_REF_TYPE_SHIFT | (Ref) *ref;
	return ty2 == TY_ANY;
}

static void assert_value(struct JitState *state, IrRef *ref, LispObject value) {
	if (!IS_VAR(*ref)) return;
	enum LispObjectType ty = lisp_type(value);
	take_snapshot(state);
	emit_opt(state, (union SsaInstruction)
		{ .op = IR_EQ, .ty = ty, .a = *ref, .b = emit_const(state, ty, value) });
	assert_type(state, ref, ty);
}

static bool has_side_effects(enum SsaOp x) {
	return x == IR_EQ || x == IR_NEQ || x == IR_CALL || x == IR_CALLARG;
}

/** Dead-code elimination (DCE). */
static void dce(struct JitState *state) {
	// Mark virtuals escaping into snapshots
	for (struct Snapshot *snap = state->snapshots;
			snap < state->snapshots + state->num_snapshots; ++snap)
		for (struct SnapshotEntry *entry = state->stack_entries + snap->offset,
					*end = entry + snap->num_entries; entry < end; ++entry)
			if (IS_VAR(entry->ref)) IR_GET(state, entry->ref).ty |= IR_MARK;
	Ref *pchain[IR_NUM_OPS];
	for (unsigned i = 0; i < IR_NUM_OPS; ++i) pchain[i] = state->chain + i;
	// Sweep in reverse while propagating marks
	for (union SsaInstruction *xs = state->trace + MAX_CONSTS,
				*x = xs + state->len - /* Skip IR_LOOP */ 1; x-- > xs;) {
		if (!(x->ty & IR_MARK || has_side_effects(x->op))) {
			*pchain[x->op] = x->prev;
			x->op = IR_NOP;
			continue;
		}
		x->ty &= ~IR_MARK;
		pchain[x->op] = &x->prev;
		if (IS_VAR(x->a)) IR_GET(state, x->a).ty |= IR_MARK;
		if (IS_VAR(x->b)) IR_GET(state, x->b).ty |= IR_MARK;
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
	unsigned preamble_len = state->len, num_phis = 0;
	// Separate preamble from loop body by LOOP instruction
	emit(state, (union SsaInstruction) { .op = IR_LOOP, .ty = TY_ANY });
	state->need_snapshot = true;
	struct Snapshot *snap = state->snapshots,
		*loopsnap = (take_snapshot(state), snap + state->num_snapshots - 1);

	dce(state);

	// Map of variables in the preamble onto variables of the peeled loop
	Ref subst[MAX_TRACE_LEN], phis[12];
	for (unsigned i = 0; i < preamble_len; ++i) {
		if (i >= snap->ir_start) { // Copy-substitute the next snapshot
			struct Snapshot *s = state->snapshots + state->num_snapshots;
			unsigned offset = s[-1].ir_start < state->len
				? ++state->num_snapshots, state->num_stack_entries
				: (--s)->offset; // Overwrite previous snapshot
			struct SnapshotEntry
				*o = state->stack_entries + snap->offset, *oend = o + snap->num_entries,
				*l = state->stack_entries + loopsnap->offset, *lend = l + loopsnap->num_entries,
				*n = state->stack_entries + offset;
			while (o < oend) {
				if (l < lend) {
					if (l->slot < o->slot) { *n++ = *l++; continue; }
					if (l->slot == o->slot) ++l; // Shadowed
				}
				struct SnapshotEntry x = *o++;
				if (IS_VAR(x.ref)) x.ref = subst[x.ref % IR_BIAS];
				*n++ = x;
			}
			while (l < lend) *n++ = *l++;
			*s = (struct Snapshot)
				{ .ir_start = state->len, .offset = offset,
				  .num_entries = n - (state->stack_entries + offset),
				  .base_offset = snap->base_offset, .pc = snap->pc };
			state->num_stack_entries = offset + s->num_entries;
			++snap;
		}

		union SsaInstruction insn = state->trace[MAX_CONSTS + i];
		if (IS_VAR(insn.a)) insn.a = subst[insn.a - IR_BIAS];
		if (IS_VAR(insn.b)) insn.b = subst[insn.b - IR_BIAS];

		IrRef ref = emit_opt(state, insn);
		unsigned j = (subst[i] = ref) % IR_BIAS;
		if (j != i && j < preamble_len) { // Loop-carried dependency
			// SLOAD:s of arguments varied in tail call give rise to φ:s
			if (IS_VAR(ref)) {
				if (num_phis >= LENGTH(phis)) rec_err(state);
				else phis[num_phis++] = ref;
			}
			// In case of type-instability, need to emit conversion
			// since later instructions expect the previous type.
			if (insn.ty != TY_ANY && !assert_type(state, &ref, insn.ty)) rec_err(state);
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
	uint8_t base_offset, num_spill_slots;
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
				*end = e + snapshot->num_entries; e < end; ++e) {
		union SsaInstruction insn = instructions[trace->num_consts + e->ref - IR_BIAS];
		ctx->bp[e->slot] = IS_VAR(e->ref)
			? regs[insn.spill_slot == SPILL_SLOT_NONE ? -insn.reg - 1 : 1 + insn.spill_slot]
			: insn.v;
		printf("Restoring stack slot %" PRIu8 " to value: ", e->slot);
		lisp_print(ctx, ctx->bp[e->slot]);
		puts(".");
	}
	struct Instruction *pc = (struct Instruction *) GC_DECOMPRESS(ctx, snapshot->pc);
	return (struct SideExitResult) { pc, snapshot->base_offset,
		trace->num_spill_slots, trace->clobbers };
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
	"movzx eax, dh\n\t"
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
	uint8_t num_spill_slots, snapshot_idx;
	union SsaInstruction bp_insn; ///< Dummy virtual representing the BP.
	/** The LuaJIT register cost model. */
	alignas(32) Ref reg_costs[NUM_REGS];
	Ref phis_refs[NUM_REGS]; ///< For each #phi_regs bit, its corresponding "in".
};

static bool has_reg(union SsaInstruction x) { return !(x.reg & REG_NONE); }

/** Spills @a ref, emitting a reload. */
static enum Register reload(struct RegAlloc *ctx, Ref ref) {
	enum Register reg;
	if (IS_VAR(ref)) {
		union SsaInstruction *x = &IR_GET(ctx->trace, ref);
		assert(has_reg(*x) && "Evicting unallocated virtual?");
		printf("Evicting %" PRIu16 " from register %d\n", ref - IR_BIAS, x->reg);
		if (x->spill_slot == SPILL_SLOT_NONE
			&& (x->spill_slot = ctx->num_spill_slots++) >= UINT8_MAX) // Allocate spill slot
			rec_err(ctx->trace); // Out of spill slots
		reg = x->reg;
		x->reg |= REG_NONE;
		// Reload from stack
		asm_rmrd(&ctx->assembler, 1, XI_MOVrm, reg, rsp, x->spill_slot * sizeof x->v);
	} else { // (Re-)materialize constant
		assert(ref == REF_BP);
		reg = ctx->bp_insn.reg;
		ctx->bp_insn.reg = -1;
		asm_rmrd(&ctx->assembler, 1, XI_MOVrm, reg,
			REG_LISP_CTX, offsetof(struct LispCtx, bp));
	}
	ctx->available |= 1 << reg;
	return reg;
}

/** Spills one of the registers in @a mask. */
[[gnu::cold]] static enum Register evict(struct RegAlloc *ctx, RegSet mask) {
	assert(!(ctx->available & mask) && "Redundant eviction");
	mask &= ~ctx->available & REG_ALL;
	Ref min_cost = UINT16_MAX;
	for (unsigned i = 0; i < NUM_REGS; ++i)
		if (LIKELY(1 << i & mask)) min_cost = MIN(ctx->reg_costs[i], min_cost);
	assert(min_cost < UINT16_MAX && "No available registers");
	return reload(ctx, min_cost);
}

static enum Register reg_alloc(struct RegAlloc *ctx, RegSet mask) {
	if (!(ctx->available & mask)) return evict(ctx, mask);
	enum Register r = stdc_trailing_zeros(ctx->available & mask);
	ctx->clobbers |= 1 << r;
	return r;
}

/** Allocates a register for the definition of @a ref.
 *
 * @param mask Allowed registers, for fixed allocations.
 */
static enum Register reg_def(struct RegAlloc *ctx, Ref ref, RegSet mask) {
	assert(IS_VAR(ref));
	union SsaInstruction *x = &IR_GET(ctx->trace, ref);
	enum Register reg
		= has_reg(*x) && 1 << x->reg & mask ? x->reg // Already allocated
		: reg_alloc(ctx, mask);
	if (has_reg(*x)) {
		if (reg != x->reg) // Existing allocation was masked
			asm_mov(&ctx->assembler, x->reg, reg);
		ctx->available |= 1 << x->reg; // Free register
	} else x->reg = reg;
	// Save ASAP for all exits to see the same spilled value.
	// (Snapshots always prefer spill slot over register for split virtuals.)
	if (x->spill_slot != SPILL_SLOT_NONE)
		asm_rmrd(&ctx->assembler, 1, XI_MOVmr, reg, rsp, x->spill_slot * sizeof x->v);
	return reg;
}

/** Allocates a register for a use of @a ref.
 *
 * If @a ref is a constant, a load must be emitted, otherwise, if @a
 * ref has a prior masked register, then a move is needed.
 */
static enum Register reg_use(struct RegAlloc *ctx, Ref ref, RegSet mask) {
	union SsaInstruction *x = IS_VAR(ref) ? &IR_GET(ctx->trace, ref)
		: ref == REF_BP ? &ctx->bp_insn : NULL;
	if (!x) return reg_alloc(ctx, mask);
	if (has_reg(*x) && 1 << x->reg & mask) return x->reg; // Already allocated
	RegSet available = ctx->available & mask;
	enum Register reg = x->reg % stdc_bit_ceil_uc(NUM_REGS); // Use hint
	if (!(1 << reg & available))
		// Prefer callee-saved registers
		reg = reg_alloc(ctx, available & CALLEE_SAVED_REGS
			? available & CALLEE_SAVED_REGS : mask);
	assert(!has_reg(*x) && "TODO Need to move");
	if (!has_reg(*x)) {
		ctx->available &= ~(1 << reg);
		ctx->reg_costs[x->reg = reg] = ref;
	}
	return reg;
}

static void asm_guard(struct RegAlloc *ctx, enum Cc cc) {
	// Emit conditional jump to side exit trampoline
	uint8_t *target = ctx->assembler.buf + MCODE_CAPACITY - 5 - 2 - 4 * ctx->snapshot_idx;
	asm_write32(&ctx->assembler, REL32(ctx->assembler.p, target));
	*--ctx->assembler.p = XI_Jcc | cc;
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
		asm_rmrd(&ctx->assembler, 1, XI_MOVmr, reg, rsp, in->spill_slot * sizeof in->v);
		in->ty |= IR_MARK; // Remember that spilling is handled by loop
		// Allocate (now free) register for correct save on 1st iteration too
		reg_use(ctx, lref, 1 << reg);
	}

	int32_t jmp_disp = REL32(asm_end, ctx->assembler.p);
	memcpy(asm_end - sizeof jmp_disp, &jmp_disp, sizeof jmp_disp);
}

static void asm_call(struct RegAlloc *ctx, Ref ref) {
	// Evict caller-saved registers
	FOR_ONES(reg, REG_ALL & ~ctx->available & ~CALLEE_SAVED_REGS)
		reload(ctx, ctx->reg_costs[reg]);

#define ARG_REGS (1 << rdi | 1 << rsi | 1 << rdx | 1 << rcx | 1 << r8 | 1 << r9)
	union SsaInstruction x = IR_GET(ctx->trace, ref);
	reg_def(ctx, ref, 1 << rax);
	enum Register f_reg = reg_use(ctx, x.a, ~ARG_REGS);
	asm_rmrd(&ctx->assembler, 0, XI_GRP5, /* CALL */ 2,
		f_reg, offsetof(struct LispCFunction, f) - 1);

	uint32_t arg_regs = rdi | rsi << 4 | rdx << 8 | rcx << 12 | r8 << 16 | r9 << 20;
	asm_mov(&ctx->assembler, arg_regs & 0xf, REG_LISP_CTX);
	asm_loadu64(&ctx->assembler, (arg_regs >>= 4) & 0xf, x.b);
	enum Register args_reg = (arg_regs >>= 4) & 0xf;
	for (unsigned i = x.b; i--;) {
		Ref arg_ref = IR_GET(ctx->trace, ref + 1 + i).a;
		union SsaInstruction arg = IR_GET(ctx->trace, arg_ref);
		if (!IS_VAR(arg_ref) && (uint32_t) arg.v == arg.v) {
			asm_write32(&ctx->assembler, arg.v);
			asm_rmrd(&ctx->assembler, 0, XI_MOVmi, 0, args_reg, i * sizeof arg.v);
			continue;
		}
		enum Register arg_reg = reg_use(ctx, arg_ref, ~(1 << args_reg));
		asm_rmrd(&ctx->assembler, 1, XI_MOVmr, arg_reg, args_reg, i * sizeof arg.v);
		if (!IS_VAR(arg_ref)) asm_loadu64(&ctx->assembler, arg_reg, arg.v);
	}
	enum Register bp = reg_use(ctx, REF_BP, -1);
	asm_rmrd(&ctx->assembler, 1, XI_LEA, args_reg, bp, 0x100 * sizeof x.v);
}

static void asm_arith(struct RegAlloc *ctx, enum ImmGrp1 op, Ref ref) {
	union SsaInstruction x = IR_GET(ctx->trace, ref),
		a = IR_GET(ctx->trace, x.a), b = IR_GET(ctx->trace, x.b);
	assert(IS_VAR(x.a) && "non-folded constant arithmetic op");

	asm_guard(ctx, CC_O);

	RegSet mask = ~(IS_VAR(x.b) && has_reg(b)
		? /* Avoid aliasing live operand */ 1 << b.reg : 0);
	enum Register dst = reg_def(ctx, ref, mask);
	if (IS_VAR(x.b)) {
		enum Register src = reg_use(ctx, x.b, -1);
		asm_rr(&ctx->assembler, 0, IMM_GRP1_TO_MR(op), src, dst);
	} else asm_grp1_imm(&ctx->assembler, 0, op, dst, (uint32_t) b.v);

	// Fix up 2-operand instruction by moving left operand to destination
	if (has_reg(a)) asm_mov(&ctx->assembler, dst, a.reg);
	else reg_use(ctx, x.a, 1 << dst);
}

static struct LispTrace *assemble_trace(struct JitState *trace) {
	struct RegAlloc ctx = { .trace = trace, .available = REG_ALL, .bp_insn.reg = -1,
		.snapshot_idx = trace->num_snapshots - 1, };
	if (!asm_init(&ctx.assembler)) die("asm_init failed");
	// Emit side-exit trampolines
	void side_exit_handler();
	asm_write32(&ctx.assembler, REL32(ctx.assembler.p, side_exit_handler));
	*--ctx.assembler.p = XI_JMP;
	for (unsigned i = 0; i < MAX_SNAPSHOTS; ++i) {
		if (i) {
			*--ctx.assembler.p = (4 * i - 2) & 0x7f; // Chain jumps if i>=32
			*--ctx.assembler.p = XI_JMPs;
		}
		*--ctx.assembler.p = i;
		*--ctx.assembler.p = XI_PUSHib;
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

	for (unsigned i = trace->len; i--;) {
		if (i < trace->snapshots[ctx.snapshot_idx].ir_start && ctx.snapshot_idx)
			--ctx.snapshot_idx;

		Ref ref = IR_BIAS + i;
		union SsaInstruction x = IR_GET(trace, ref);
		switch (x.op) {
		case IR_SLOAD:
			enum Register reg = reg_def(&ctx, ref, -1), bp = reg_use(&ctx, REF_BP, -1);
			asm_rmrd(&ctx.assembler, 1, XI_MOVrm, reg, bp, x.a * sizeof x.v);
			break;
		case IR_GLOAD:
			struct LispSymbol *sym = UNTAG_OBJ(IR_GET(trace, x.a).v);
			int32_t p = (char *) &sym->value - (char *) trace->lisp_ctx;
			reg = reg_def(&ctx, ref, -1);
			asm_rmrd(&ctx.assembler, 1, XI_MOVrm, reg, REG_LISP_CTX, p);
			break;
		case IR_ULOAD: {
			enum Register reg = reg_def(&ctx, ref, -1), fn = reg_use(&ctx, x.a, -1);
			asm_rmrd(&ctx.assembler, 1, XI_MOVrm, reg, reg, 0);
			asm_rmrd(&ctx.assembler, 1, XI_MOVrm, reg, reg,
				offsetof(struct Upvalue, location));
			asm_rmrd(&ctx.assembler, 1, XI_MOVrm, reg, fn,
				offsetof(struct Closure, upvalues[x.b]) - 1);
			break;
		}

		case IR_EQ:
		case IR_NEQ:
			assert(IS_VAR(x.a));
			asm_guard(&ctx, x.op == IR_EQ ? CC_NE : CC_E);
			enum Register reg1 = reg_use(&ctx, x.a, -1);
			if (IS_VAR(x.b)) {
				enum Register reg2 = reg_use(&ctx, x.b, -1);
				asm_rr(&ctx.assembler, 0, IMM_GRP1_TO_MR(XG_CMP), reg2, reg1);
			} else asm_grp1_imm(&ctx.assembler, 0, XG_CMP,
				reg1, (uint32_t) IR_GET(trace, x.b).v);
			break;

		case IR_CALL: asm_call(&ctx, ref); break;
		case IR_CALLARG: break;

		case IR_LOOP: asm_loop(&ctx, asm_end); break;
		case IR_PHI:
			assert(IS_VAR(x.a) && IS_VAR(x.b) && "Variant ref cannot be constant");
			union SsaInstruction *in = &IR_GET(trace, x.a), *out = &IR_GET(trace, x.b);
			assert(stdc_count_ones(ctx.available) > 1 && "Out of φ registers");
			// Pick φ registers in opposite direction to reduce collisions
			enum Register dst = stdc_bit_width(ctx.available) - 1;
			if (LIKELY(!has_reg(*out))) reg_use(&ctx, x.b, 1 << dst);
			else { // Outgoing variable already has a location: Move from out to in
				ctx.clobbers |= 1 << dst;
				asm_mov(&ctx.assembler, dst, out->reg);
			}
			in->reg = dst | REG_NONE; // Add hint
			ctx.phi_regs |= 1 << dst;
			ctx.phis_refs[dst] = x.a;
			break;
		case IR_NOP: break;

		case IR_ADD: asm_arith(&ctx, XG_ADD, ref); break;

		default: unreachable();
		}
	}

	FOR_ONES(reg, ctx.phi_regs) {
		union SsaInstruction in = IR_GET(ctx.trace, ctx.phis_refs[reg]);
		if (in.spill_slot != SPILL_SLOT_NONE && !(in.ty & IR_MARK))
			rec_err(trace); // Spilled outside loop
	}
	reload(&ctx, REF_BP);
	assert(ctx.available == REG_ALL);
	ctx.clobbers &= CALLEE_SAVED_REGS;
	if ((stdc_count_ones(ctx.clobbers) + ctx.num_spill_slots) % 2 == 0)
		++ctx.num_spill_slots; // Align stack to 16-byte boundary
	asm_grp1_imm(&ctx.assembler, 1, XG_SUB, rsp, ctx.num_spill_slots * sizeof(void *));
	// Save callee-saved registers
	FOR_ONES(reg, ctx.clobbers) {
		*--ctx.assembler.p = /* PUSH */ 0x50 + (reg & 7);
		EMIT_REX(&ctx.assembler, 0, 0, 0, reg);
	}

	if (trace->rec_status != REC_OK) return NULL;
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

static void penalize(struct JitState *state) {
	for (struct Penalty *x = state->penalties,
				*end = x + LENGTH(state->penalties); x < end; ++x) {
		if (x->closure != state->origin) continue;

		if (!--x->value) {
			x->closure = NULL;
			state->origin_pc->op += CALL_INTERPR - CALL; // Blacklist
		}
		return;
	}

	state->penalties[state->next_penalty_slot++ % LENGTH(state->penalties)]
		= (struct Penalty) { .closure = state->origin, .value = 16 };
}

#ifdef DEBUG
static void print_ref(struct JitState *state, Ref ref) {
	if (IS_VAR(ref)) { printf("%.4u", ref - IR_BIAS); return; }
	LispObject v = IR_GET(state, ref).v;
	switch (lisp_type(v)) {
	case LISP_NIL: printf("nil "); break;
	case LISP_INTEGER: printf("%+" PRIi32, UNTAG_SMI(v)); break;
	case LISP_SYMBOL:
		struct LispSymbol *sym = UNTAG_OBJ(v);
		printf("[%.*s]", (int) sym->len, sym->name);
		break;
	case LISP_CFUNCTION:
		printf("<%s>", ((struct LispCFunction *) UNTAG_OBJ(v))->name);
		break;
	case LISP_CLOSURE: default: printf("%#" PRIxPTR, v); break;
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
						*end = entry + snap->num_entries; entry < end; ++entry) {
				while (j++ < entry->slot) printf("---- ");
				print_ref(state, entry->ref);
				putchar(' ');
			}
			puts("]");
		}

		union SsaInstruction x = state->trace[MAX_CONSTS + i];
		printf("%.4u %s %3u ", i, type_names[x.ty & ~IR_MARK], x.reg);
		switch (x.op) {
		case IR_EQ: printf("EQ    ");
			if (false) case IR_NEQ: printf("NEQ   ");
			print_ref(state, x.a);
			printf("  ");
			print_ref(state, x.b);
			putchar('\n');
			break;
		case IR_SLOAD: printf("SLOAD #%" PRIu16 "\n", x.a); break;
		case IR_GLOAD:
			printf("GLOAD "); print_ref(state, x.a); putchar('\n'); break;
		case IR_ULOAD:
			printf("ULOAD #%" PRIu16 " from ", x.b);
			print_ref(state, x.a);
			putchar('\n');
			break;
		case IR_CALL: printf("CALL  ");
			print_ref(state, x.a);
			printf("  (");
			for (Ref j = 0; j < x.b; ++j) {
				union SsaInstruction arg = state->trace[MAX_CONSTS + ++i];
				if (j) printf(" ");
				print_ref(state, arg.a);
			}
			puts(")");
			break;
		case IR_CALLARG: case IR_NUM_OPS: unreachable();
		case IR_LOOP: puts("LOOP ------------"); break;
		case IR_PHI:
			printf("PHI   ");
			print_ref(state, x.a);
			printf("  ");
			print_ref(state, x.b);
			putchar('\n');
			break;
		case IR_NOP: puts("NOP"); break;

		case IR_ADD:
			printf("ADD   ");
			print_ref(state, x.a);
			printf("  ");
			print_ref(state, x.b);
			putchar('\n');
			break;
		}
	}
	switch (link) {
	case TRACE_LINK_LOOP: puts("---- TRACE stop -> loop"); break;
	}
}
#endif

struct JitState *jit_new(struct LispCtx *ctx) {
	struct JitState *state;
	if (!(state = malloc(sizeof *state))) return NULL;
	state->lisp_ctx = ctx;
	state->num_traces = 0;
	state->next_penalty_slot = 0;
	memset(state->penalties, 0, sizeof state->penalties);
	return state;
}

void jit_free(struct JitState *state) {
	free(state);
}

bool jit_init(struct JitState *state, struct Closure *f, struct Instruction *pc) {
	puts("Starting trace recording");
	if (UNLIKELY(state->num_traces >= LENGTH(*state->lisp_ctx->traces))) return false;
	memset(state, 0, offsetof(struct JitState, pc));
	state->origin = f;
	state->origin_pc = pc - 1;
	state->bp = state->slots;
	state->need_snapshot = true;
	return true;
}

static IrRef record_c_call(struct JitState *state, struct Instruction x) {
	IrRef ref = sload(state, x.a);

	LispObject f_value = state->lisp_ctx->bp[x.a];
	struct LispCFunction *f = UNTAG_OBJ(f_value);
	if (!strcmp(f->name, "+")) {
		assert_value(state, &ref, f_value);
		IrRef a = sload(state, x.a + 2), b = sload(state, x.a + 2 + 1);
		assert_type(state, &a, LISP_INTEGER);
		assert_type(state, &b, LISP_INTEGER);
		return emit_opt(state, (union SsaInstruction)
			{ .op = IR_ADD, .ty = LISP_INTEGER, .a = a, .b = b });
	}

	assert_type(state, &ref, LISP_CFUNCTION);
	take_snapshot(state); // Type guard may get added to return value
	IrRef result = emit(state, (union SsaInstruction)
		{ .op = IR_CALL, .ty = TY_ANY, .a = ref, .b = x.c });
	for (unsigned i = 0; i < x.c; ++i) {
		IrRef arg = sload(state, x.a + 2 + i);
		emit(state, (union SsaInstruction)
			{ .op = IR_CALLARG, .ty = arg >> IR_REF_TYPE_SHIFT, .a = arg });
	}
	state->max_slot = x.a; // CALL always uses highest register
	state->need_snapshot = true; // Call may have side-effects
	return result;
}

bool jit_record(struct JitState *state, struct Instruction *pc) {
	uintptr_t *bp = state->lisp_ctx->bp;
	struct Instruction x = (state->pc = pc)[-1];
	IrRef result = 0;
	switch (x.op) {
	case LOAD_NIL: result = emit_const(state, LISP_NIL, NIL(state->lisp_ctx)); break;
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
		result = emit_opt(state, (union SsaInstruction)
			{ .op = IR_GLOAD, .ty = TY_ANY, .a = sym });
		break;
	case GETUPVALUE: result = uref(state, bp, x.c); break;
	case CALL: case CALL_INTERPR:
		LispObject fn_value = bp[x.a];
		switch (lisp_type(fn_value)) {
		case LISP_CFUNCTION: result = record_c_call(state, x); break;
		case LISP_CLOSURE:
			// Specialize to the function value in question
			assert_value(state, (IrRef[]) { sload(state, x.a) }, fn_value);
			struct Prototype *proto = ((struct Closure *) UNTAG_OBJ(fn_value))->prototype;
			if (proto->arity & PROTO_VARIADIC
				|| fn_value == TAG_OBJ(state->origin)) /* NYI */ rec_err(state);
			(state->bp += x.a)[1] = emit_const(state, TY_RET_ADDR, (uintptr_t) pc);
			state->base_offset += x.a;
			state->max_slot = 2 + x.c - 1;
			break;
		default: break;
		}
		break;
	case TAIL_CALL: case TAIL_CALL_INTERPR:
		fn_value = bp[x.a];
		bool is_cfn = lisp_type(fn_value) == LISP_CFUNCTION;
		if (is_cfn) { state->bp[x.a] = record_c_call(state, x); goto do_ret; }
		// Specialize to the function value in question
		// TODO Guard on prototype only
		IrRef cur = *state->bp, ref = *state->bp = sload(state, x.a);
		assert_value(state, &ref, fn_value);
		// Move arguments down to current frame
		memmove(state->bp + 2, state->bp + x.a + 2, x.c * sizeof *state->bp);
		state->max_slot = 2 + x.c - 1;
		if (fn_value == TAG_OBJ(state->origin)) {
			*state->bp = cur; // Avoid reloading closure
			peel_loop(state);
			struct LispTrace *trace;
			if (!(trace = assemble_trace(state))) break;
#ifdef DEBUG
			puts("Reached loop start! Trace:\n");
			print_trace(state, TRACE_LINK_LOOP);
#endif
			uint16_t trace_num = state->num_traces++;
			(*state->lisp_ctx->traces)[trace_num] = trace;
			pc[-1] = (struct Instruction) { .op = TAIL_JIT_CALL, .a = x.a, .b = trace_num };
			state->rec_status = REC_DONE;
		}
		break;
	case MOV: result = sload(state, x.c); break;
	case JMP: break;
	case JNIL:
		ref = sload(state, x.a);
		uint8_t ty = ref >> IR_REF_TYPE_SHIFT;
		if (ty != TY_ANY) break; // Constant NIL comparison is a no-op
		IrRef nil = emit_const(state, LISP_NIL, NIL(state->lisp_ctx));
		take_snapshot(state);
		emit_opt(state, (union SsaInstruction)
			{ .op = IR_EQ ^ !NILP(state->lisp_ctx, bp[x.a]), .ty = ty, .a = ref, .b = nil });
		break;
	case RET: do_ret:
		if (state->base_offset) {
			result = state->bp[x.a];
			uint8_t offset = state->max_slot = x.a = ((struct Instruction *) bp[1])[-1].a;
			state->bp -= offset;
			state->base_offset -= offset;
			break;
		}
		[[fallthrough]];
	case TAIL_JIT_CALL: // Await side trace instead
	default:
		printf("Instruction %" PRIu8 " is NYI, aborting trace...\n", x.op);
		rec_err(state);
		break;
	}

	if (result) {
		state->max_slot = MAX(state->max_slot, x.a);
		state->bp[x.a] = result;
	}
	if (state->rec_status == REC_NYI) penalize(state);
	return state->rec_status == REC_OK;
}
