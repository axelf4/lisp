/** Tracing just-in-time compiler.
 *
 * @see GAL, Andreas, et al. Trace-based just-in-time type
 *      specialization for dynamic languages. ACM Sigplan Notices,
 *      2009, 44.6: 465-478.
 */

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
#define SIDE_TRACE_THRESHOLD 1

#define IR_MARK 0x80 ///< Multi-purpose flag.
#define REG_NONE 0x80 ///< Spilled or unallocated register flag.

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
	IR_PLOAD, ///< Load from parent trace.
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
					spill_slot; ///< 1-based spill slot index.
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
	uint8_t num_entries, ///< Number of stack entries.
		base_offset,
		hotcount;
	struct GcRef pc; ///< Instruction pointer after this point.
	struct LispTrace *trace; ///< Side trace.
};
#define FOR_SNAPSHOT_ENTRIES(snapshot, entries, entry) \
	for (struct SnapshotEntry *entry = (entries) + (snapshot)->offset, \
				*_end = entry + (snapshot)->num_entries; entry < _end; ++entry)

enum TraceLink {
	TRACE_LINK_LOOP, ///< Loop to itself.
	TRACE_LINK_ROOT, ///< Jump to root trace.
};

struct LispTrace {
	void (*f)();
	uint16_t len, num_consts;
	uint8_t num_snapshots, num_spill_slots, arity;
	uint32_t mcode_size,
		mcode_tail; ///< Offset from #f to end of machine code block.
	alignas(union SsaInstruction) char data[];
};

uint8_t trace_arity(struct LispTrace *trace) { return trace->arity; }

enum RecordStatus : unsigned char {
	REC_OK, ///< Incomplete trace recording.
	/// Abort the trace recording, e.g., due to NYI (not yet implemented).
	REC_NYI,
};

/** Closure JIT penalty FIFO-cache slot. */
struct Penalty {
	struct Closure *closure;
	unsigned char value;
};

/** In-progress trace recording. */
struct JitState {
	uint16_t len, num_consts;
	uint8_t num_snapshots, num_stack_entries, max_slot, base_offset;
	bool need_snapshot;
	enum RecordStatus status;
	IrRef *bp, ///< Pointer into @ref #slots at the current frame offset.
		slots[0x100]; ///< Array of IR references for each VM register.
	Ref chain[IR_NUM_OPS];

	struct Closure *origin; ///< The closure that triggered the recording.
	struct LispTrace *parent, *link;
	struct Instruction *pc;
	struct Instruction *origin_pc;
	uint8_t side_exit_num;

	uint16_t num_traces; ///< Length of #LispCtx::traces.
	unsigned char next_penalty_slot;

	struct Snapshot snapshots[MAX_SNAPSHOTS];
	/// Backing storage for snapshot data.
	struct SnapshotEntry stack_entries[MAX_SNAPSHOT_ENTRIES];
	union SsaInstruction trace[MAX_CONSTS + MAX_TRACE_LEN];

	struct Penalty penalties[32];
};

struct JitState *jit_new() {
	struct JitState *state;
	if (!(state = malloc(sizeof *state))) return NULL;
	state->num_traces = 0;
	state->next_penalty_slot = 0;
	memset(state->penalties, 0, sizeof state->penalties);
	return state;
}

void jit_free(struct JitState *state) { free(state); }

static void jit_init(struct JitState *state) {
	memset(state, 0, offsetof(struct JitState, pc));
	state->bp = state->slots;
	state->need_snapshot = true;
}

void jit_init_root(struct JitState *state, struct Closure *f, struct Instruction *pc) {
	jit_init(state);
	state->origin = f;
	state->origin_pc = pc - 1;
}

[[gnu::cold]]
static void rec_err(struct JitState *state) { state->status = REC_NYI; }

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
		.hotcount = SIDE_TRACE_THRESHOLD
	};
	for (unsigned i = 0; i <= max_slot; ++i) {
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

[[gnu::noinline]] static IrRef sload(struct JitState *state, int slot) {
	take_snapshot(state); // Type guard may be added
	state->max_slot = MAX(state->max_slot, slot);
	return state->bp[slot] = emit(state, (union SsaInstruction)
		{ .op = IR_SLOAD, .ty = TY_ANY, .a = state->base_offset + slot });
}
/** Emits a load of stack slot @a i unless it is already loaded. */
#define SLOT(state, i) ((state)->bp[i] ? (state)->bp[i] : sload(state, i))

/** Emits an instruction to load the given upvalue. */
static IrRef uref(struct JitState *state, uintptr_t *bp, uint8_t idx) {
	struct Upvalue *upvalue = ((struct Closure *) UNTAG_OBJ(*bp))->upvalues[idx];
	// TODO Track whether the upvalue is immutable and inlinable
	if (!upvalue->is_closed) {
		// In a nested frame the upvalue may be available on the stack
		ptrdiff_t slot = upvalue->location - (bp - state->base_offset);
		if (slot >= 0) return SLOT(state, slot - state->base_offset);
	}
	take_snapshot(state);
	return emit_opt(state, (union SsaInstruction) { .op = IR_ULOAD,
			.ty = TY_ANY, .a = SLOT(state, /* this closure */ 0), .b = idx });
}

/** Coerces @a ref to @a ty.
 *
 * @return Whether @a ty was a subtype of the type of @a ref.
 */
static bool guard_type(struct JitState *state, IrRef *ref, uint8_t ty) {
	uint8_t ty2 = *ref >> IR_REF_TYPE_SHIFT;
	if (ty2 == ty) return true;
	// If ty </: ref->ty, then type error is imminent
	if (IS_VAR(*ref))
		*ref = (IR_GET(state, *ref).ty = ty) << IR_REF_TYPE_SHIFT | (Ref) *ref;
	return ty2 == TY_ANY;
}

static void guard_value(struct JitState *state, IrRef *ref, LispObject value) {
	if (!IS_VAR(*ref)) return;
	enum LispObjectType ty = lisp_type(value);
	take_snapshot(state);
	emit_opt(state, (union SsaInstruction)
		{ .op = IR_EQ, .ty = ty, .a = *ref, .b = emit_const(state, ty, value) });
	guard_type(state, ref, ty);
}

static bool has_side_effects(enum SsaOp x) {
	return x == IR_EQ || x == IR_NEQ || x == IR_CALL || x == IR_CALLARG;
}

/** Dead-code elimination (DCE). */
static void dce(struct JitState *state) {
	// Mark virtuals escaping into snapshots
	for (struct Snapshot *snap = state->snapshots;
			snap < state->snapshots + state->num_snapshots; ++snap)
		FOR_SNAPSHOT_ENTRIES(snap, state->stack_entries, entry)
			if (IS_VAR(entry->ref)) IR_GET(state, entry->ref).ty |= IR_MARK;
	Ref *pchain[IR_NUM_OPS];
	for (unsigned i = 0; i < IR_NUM_OPS; ++i) pchain[i] = state->chain + i;
	// Sweep in reverse while propagating marks
	for (union SsaInstruction *xs = state->trace + MAX_CONSTS,
				*x = xs + state->len; x-- > xs;) {
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
	struct Snapshot *snap = state->snapshots,
		*loopsnap = snap + state->num_snapshots - 1;
	// Separate preamble from loop body by LOOP instruction
	emit(state, (union SsaInstruction) { .op = IR_LOOP, .ty = TY_ANY });
	++loopsnap->ir_start;

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
				  .base_offset = snap->base_offset, .pc = snap->pc,
				  .hotcount = SIDE_TRACE_THRESHOLD };
			state->num_stack_entries = offset + s->num_entries;
			++snap;
		}

		union SsaInstruction insn = IR_GET(state, IR_BIAS + i);
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
			if (insn.ty != TY_ANY && !guard_type(state, &ref, insn.ty))
				rec_err(state);
		}
	}

	for (unsigned i = 0; i < num_phis; ++i) { // Emit φ-functions
		Ref lref = phis[i], rref = subst[lref - IR_BIAS];
		// TODO φ:s whose arguments are the same or other redundant
		// φ:s are in turn redundant and eliminable.
		if (lref == rref) continue; // Redundant φ
		emit(state, (union SsaInstruction)
			{ .op = IR_PHI, .ty = IR_GET(state, lref).ty, .a = lref, .b = rref });
	}
}

[[gnu::used]] static struct SideExitResult {
	struct Instruction *pc;
	uint8_t base_offset, num_spill_slots, should_record;
} side_exit_handler_inner(struct LispCtx *ctx, uintptr_t *regs) {
	struct JitState *state = ctx->jit_state;
	struct LispTrace *trace = ctx->current_trace;
	uint8_t exit_num = *regs;
	union SsaInstruction *insns = (union SsaInstruction *) trace->data;
	struct Snapshot
		*snapshots = (struct Snapshot *) (insns + trace->num_consts + trace->len),
		*snapshot = snapshots + exit_num;
	struct SnapshotEntry *entries
		= (struct SnapshotEntry *) (snapshots + trace->num_snapshots);
	printf("Side exit %" PRIu8 "\n", exit_num);
	assert(!snapshot->trace);
	ctx->current_trace = NULL;
	FOR_SNAPSHOT_ENTRIES(snapshot, entries, e) {
		union SsaInstruction insn = insns[trace->num_consts + e->ref - IR_BIAS];
		ctx->bp[e->slot] = IS_VAR(e->ref)
			? regs[insn.spill_slot ? insn.spill_slot : -insn.reg - 1]
			: insn.v;

		printf("Restoring stack slot %" PRIu8 " to value: ", e->slot);
		lisp_print(ctx, ctx->bp[e->slot]);
		puts(".");
	}

	bool should_record = !--snapshot->hotcount;
	if (should_record) {
		snapshot->hotcount = SIDE_TRACE_THRESHOLD;
		puts("Starting side trace recording");
		jit_init(state);
		state->parent = trace;
		state->side_exit_num = exit_num;
		FOR_SNAPSHOT_ENTRIES(snapshot, entries, e) {
			union SsaInstruction insn = insns[trace->num_consts + e->ref - IR_BIAS];
			state->slots[state->max_slot = e->slot] = IS_VAR(e->ref)
				? emit(ctx->jit_state, (union SsaInstruction)
					{ .op = IR_PLOAD, .ty = e->ty, .a = insn.reg, .b = insn.spill_slot })
				: emit_const(state, e->ty, insn.v);
			if (e->ty == TY_RET_ADDR) state->base_offset = e->slot - 1;
		}
		state->bp = state->slots + state->base_offset;
	}

	struct Instruction *pc = (struct Instruction *) GC_DECOMPRESS(ctx, snapshot->pc);
	return (struct SideExitResult) { pc, snapshot->base_offset,
		trace->num_spill_slots, should_record };
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
	"ret");

/* Reverse linear-scan register allocation. */

#define REF_BP 0
/** Initial set of allocatable registers. */
#if PRESERVE_FRAME_POINTER
#define REG_ALL (((1 << NUM_REGS) - 1) & ~(1 << rsp | 1 << rbp | 1 << REG_LISP_CTX))
#else
#define REG_ALL (((1 << NUM_REGS) - 1) & ~(1 << rsp | 1 << REG_LISP_CTX))
#endif

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
	uint8_t *end;
};

static bool has_reg(union SsaInstruction x) { return !(x.reg & REG_NONE); }

/** Spills @a ref, emitting a reload. */
static enum Register reload(struct RegAlloc *ctx, Ref ref) {
	enum Register reg;
	if (IS_VAR(ref)) {
		union SsaInstruction *x = &IR_GET(ctx->trace, ref);
		assert(has_reg(*x) && "Evicting unallocated virtual?");
		reg = x->reg;
		printf("Evicting %" PRIu16 " from register %d\n", ref - IR_BIAS, reg);
		x->reg |= REG_NONE;
		if (!x->spill_slot && !(x->spill_slot = ++ctx->num_spill_slots))
			rec_err(ctx->trace); // Out of spill slots
		// Reload from stack
		asm_rmrd(&ctx->assembler, 1, XI_MOVrm, reg, rsp, (x->spill_slot - 1) * sizeof x->v);
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
	if (x->spill_slot)
		asm_rmrd(&ctx->assembler, 1, XI_MOVmr, reg, rsp, (x->spill_slot - 1) * sizeof x->v);
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

#define EXIT_TRAMPOLINE(end, exit_num) ((end) - 5 - 2 - 4 * (exit_num))

static void asm_guard(struct RegAlloc *ctx, enum Cc cc) {
	// Emit conditional jump to side exit trampoline
	uint8_t *target = EXIT_TRAMPOLINE(ctx->assembler.buf + MCODE_CAPACITY, ctx->snapshot_idx);
	asm_write32(&ctx->assembler, REL32(ctx->assembler.p, target));
	*--ctx->assembler.p = XI_Jcc | cc;
	*--ctx->assembler.p = 0x0f;
}

/** Assembles the @ref IR_LOOP instruction. */
static void asm_loop(struct RegAlloc *ctx) {
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
		if (LIKELY(!in->spill_slot)) continue;
		// Resave spill slot of "in" on each iteration
		asm_rmrd(&ctx->assembler, 1, XI_MOVmr, reg, rsp, (in->spill_slot - 1) * sizeof in->v);
		in->ty |= IR_MARK; // Remember that spilling is handled by loop
		// Allocate (now free) register for correct save on 1st iteration too
		reg_use(ctx, lref, 1 << reg);
	}

	int32_t jmp_dest = REL32(ctx->end, ctx->assembler.p);
	memcpy(ctx->end - sizeof jmp_dest, &jmp_dest, sizeof jmp_dest);
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

static struct LispTrace *assemble_trace(struct LispCtx *lisp_ctx, struct JitState *trace,
	enum TraceLink link_type) {
	trace->need_snapshot = true;
	take_snapshot(trace);
	dce(trace);
	if (link_type == TRACE_LINK_LOOP) peel_loop(trace);

	struct RegAlloc ctx = { .trace = trace, .available = REG_ALL, .bp_insn.reg = -1,
		.snapshot_idx = trace->num_snapshots - 1,
		.num_spill_slots = trace->parent ? trace->parent->num_spill_slots : 0 };
	if (!LIKELY(asm_init(&ctx.assembler))) return NULL;
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
		union SsaInstruction *x = &IR_GET(trace, IR_BIAS + i);
		x->reg = -1;
		x->spill_slot = 0;
	}
	for (unsigned i = 0; ; ++i) {
		union SsaInstruction *x = &IR_GET(trace, IR_BIAS + i);
		if (x->op != IR_PLOAD) break;
		x->reg = x->a | REG_NONE; // Add hint
		x->spill_slot = x->b;
	}
	ctx.end = ctx.assembler.p;
	// Emit loop backedge placeholder
	switch (link_type) {
	case TRACE_LINK_LOOP: *(ctx.assembler.p -= 1 + sizeof(int32_t)) = XI_JMP; break;
	case TRACE_LINK_ROOT:
		ctx.assembler.p -= /* ADD + JMP */ 12;
		// Restore stack
		enum Register bp = reg_use(&ctx, REF_BP, -1);
		struct Snapshot *snapshot = trace->snapshots + trace->num_snapshots - 1;
		FOR_SNAPSHOT_ENTRIES(snapshot, trace->stack_entries, entry) {
			union SsaInstruction x = IR_GET(trace, entry->ref);
			enum Register reg = reg_use(&ctx, entry->ref, ~(1 << bp));
			asm_rmrd(&ctx.assembler, 1, XI_MOVmr, reg, bp, entry->slot * sizeof x.v);
			if (!IS_VAR(entry->ref)) asm_loadu64(&ctx.assembler, reg, x.v);
		}
		break;
	default: unreachable();
	}

	for (unsigned i = trace->len; i--;) {
		if (i < trace->snapshots[ctx.snapshot_idx].ir_start && ctx.snapshot_idx) {
			struct Snapshot *s = trace->snapshots + --ctx.snapshot_idx;
			// Allocate registers for virtuals escaping into snapshot
			FOR_SNAPSHOT_ENTRIES(s, trace->stack_entries, e) {
				union SsaInstruction *x = &IR_GET(trace, e->ref);
				if (IS_VAR(e->ref) && !x->spill_slot) reg_use(&ctx, e->ref, -1);
			}
		}

		Ref ref = IR_BIAS + i;
		union SsaInstruction x = IR_GET(trace, ref);
		switch (x.op) {
		case IR_SLOAD:
			enum Register reg = reg_def(&ctx, ref, -1), bp = reg_use(&ctx, REF_BP, -1);
			asm_rmrd(&ctx.assembler, 1, XI_MOVrm, reg, bp, x.a * sizeof x.v);
			break;
		case IR_GLOAD:
			struct LispSymbol *sym = UNTAG_OBJ(IR_GET(trace, x.a).v);
			int32_t p = (char *) &sym->value - (char *) lisp_ctx;
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
		case IR_PLOAD:
			if (/* inherited spilled */ x.b) { if (has_reg(x)) reload(&ctx, ref); }
			else reg_def(&ctx, ref, 1 << x.a);
			break;

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

		case IR_LOOP: asm_loop(&ctx); break;
		case IR_PHI:
			assert(IS_VAR(x.a) && IS_VAR(x.b) && "Variant ref cannot be constant");
			union SsaInstruction *in = &IR_GET(trace, x.a), *out = &IR_GET(trace, x.b);
			assert(!has_reg(*in));
			assert(stdc_count_ones(ctx.available) > 1 && "Out of φ registers");
			// Pick φ registers from opposite end to reduce collisions
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
		if (in.spill_slot && !(in.ty & IR_MARK)) rec_err(trace); // Spilled outside loop
	}
	if (has_reg(ctx.bp_insn)) reload(&ctx, REF_BP);
	assert(ctx.available == REG_ALL);
	ctx.num_spill_slots += ctx.num_spill_slots % 2 == 0; // 16-byte align stack
	unsigned dsp = ctx.num_spill_slots - (trace->parent ? trace->parent->num_spill_slots : 0);
	if (dsp) asm_grp1_imm(&ctx.assembler, 1, XG_SUB, rsp, dsp * sizeof(void *));

	struct LispTrace *result;
	if (UNLIKELY(trace->status != REC_OK)
		|| !(result = malloc(sizeof *result
				+ (trace->num_consts + trace->len) * sizeof *trace->trace
				+ trace->num_snapshots * sizeof *trace->snapshots
				+ trace->num_stack_entries * sizeof *trace->stack_entries)))
		return NULL;

	if (link_type == TRACE_LINK_ROOT) {
		asm_mov_mi64(&ctx.assembler, REG_LISP_CTX,
			offsetof(struct LispCtx, current_trace), (uintptr_t) result);
		// Patch tail
		unsigned stack_size = ctx.num_spill_slots * sizeof(void *);
		struct Assembler _asm = { ctx.end - (stack_size > INT8_MAX ? 0 : 3), NULL };
		int32_t jmp_dest = REL32(_asm.p, trace->link->f);
		asm_write32(&_asm, jmp_dest);
		*--_asm.p = XI_JMP;
		asm_grp1_imm(&_asm, 1, XG_ADD, rsp, stack_size);
	}

	*result = (struct LispTrace) {
		.f = asm_assemble(&ctx.assembler),
		.len = trace->len, .num_consts = trace->num_consts,
		.num_snapshots = trace->num_snapshots,
		.num_spill_slots = ctx.num_spill_slots,
		.arity = trace->origin ? trace->origin->prototype->arity : 0,
		.mcode_size = ctx.end - ctx.assembler.p,
		.mcode_tail = ctx.assembler.buf + MCODE_CAPACITY - ctx.assembler.p
	};
	char *p = result->data;
	size_t size = (trace->num_consts + trace->len) * sizeof *trace->trace;
	memcpy(p, trace->trace + MAX_CONSTS - trace->num_consts, size);
	p += size;
	memcpy(p, trace->snapshots, size = trace->num_snapshots * sizeof *trace->snapshots);
	p += size;
	memcpy(p, trace->stack_entries, trace->num_stack_entries * sizeof *trace->stack_entries);
	return result;
}

static void patch_exit(struct LispTrace *parent, uint8_t exit_num, struct LispTrace *trace) {
	printf("Overwriting side exit %u jump of %p!\n", exit_num, (void *) parent);
	union SsaInstruction *insns = (union SsaInstruction *) parent->data;
	struct Snapshot
		*snapshots = (struct Snapshot *) (insns + parent->num_consts + parent->len),
		*snapshot = snapshots + exit_num;
	long page_size = sysconf(_SC_PAGESIZE);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
	char *beg = (char *) parent->f, *end = beg + parent->mcode_size,
		*page = (char *) ((uintptr_t) beg & ~(page_size - 1)),
		*trampoline = EXIT_TRAMPOLINE(beg + parent->mcode_tail, exit_num);
#pragma GCC diagnostic pop

	snapshot->trace = trace;
	if (mprotect(page, end - page, PROT_READ | PROT_WRITE))
	err: die("mprotect failed");
	for (uint8_t *p = (uint8_t *) beg; p < (uint8_t *) end; p += asm_insn_len(p)) {
		int32_t dest;
		memcpy(&dest, p + 2, sizeof dest);
		if (!(*p == 0x0f && (p[1] & 0xf0) == XI_Jcc
				&& dest == REL32(p + 6, trampoline))) continue;
		dest = REL32(p + 6, trace->f);
		memcpy(p + 2, &dest, sizeof dest);
	}
	if (mprotect(page, end - page, PROT_READ | PROT_EXEC)) goto err;
	__builtin___clear_cache(beg, end);
}

static void penalize(struct JitState *state) {
	if (state->parent) return; // TODO

	struct Penalty *slot = state->penalties, *end = slot + LENGTH(state->penalties);
	do if (slot->closure == state->origin) goto out_found; while (++slot < end);

	state->penalties[state->next_penalty_slot++ % LENGTH(state->penalties)]
		= (struct Penalty) { .closure = state->origin, .value = 16 };
	return;

out_found:
	if (!--slot->value) {
		slot->closure = NULL;
		state->origin_pc->op += CALL_INTERPR - CALL; // Blacklist
	}
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
	for (unsigned i = 0, snap_idx = 0; ; ++i) {
		struct Snapshot *snap = state->snapshots + snap_idx;
		if (snap_idx < state->num_snapshots && i >= snap->ir_start) {
			printf("....         SNAP  #%-3u [ ", snap_idx++);
			unsigned j = 0;
			FOR_SNAPSHOT_ENTRIES(snap, state->stack_entries, entry) {
				while (j++ < entry->slot) printf("---- ");
				print_ref(state, entry->ref);
				putchar(' ');
			}
			puts("]");
		}
		if (i >= state->len) break;

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
		case IR_PLOAD: printf("PLOAD %u %u\n", x.a, x.b); break;
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
	case TRACE_LINK_ROOT: puts("---- TRACE stop -> root"); break;
	}
}
#endif

static IrRef record_c_call(struct JitState *state, uintptr_t *bp, struct Instruction x) {
	IrRef ref = SLOT(state, x.a);
	LispObject f_value = bp[x.a];
	struct LispCFunction *f = UNTAG_OBJ(f_value);
	if (!strcmp(f->name, "+")) {
		guard_value(state, &ref, f_value);
		IrRef a = SLOT(state, x.a + 2), b = SLOT(state, x.a + 2 + 1);
		guard_type(state, &a, LISP_INTEGER);
		guard_type(state, &b, LISP_INTEGER);
		return emit_opt(state, (union SsaInstruction)
			{ .op = IR_ADD, .ty = LISP_INTEGER, .a = a, .b = b });
	}

	guard_type(state, &ref, LISP_CFUNCTION);
	take_snapshot(state); // Type guard may get added to return value
	IrRef result = emit(state, (union SsaInstruction)
		{ .op = IR_CALL, .ty = TY_ANY, .a = ref, .b = x.c });
	for (unsigned i = 0; i < x.c; ++i) {
		IrRef arg = SLOT(state, x.a + 2 + i);
		emit(state, (union SsaInstruction)
			{ .op = IR_CALLARG, .ty = arg >> IR_REF_TYPE_SHIFT, .a = arg });
	}
	state->max_slot = x.a; // CALL always uses highest register
	state->need_snapshot = true; // Call may have side-effects
	return result;
}

bool jit_record(struct LispCtx *ctx, struct Instruction *pc, LispObject *bp) {
	struct JitState *state = ctx->jit_state;
	struct Instruction x = (state->pc = pc)[-1];
	IrRef result = 0;
	switch (x.op) {
	case LOAD_NIL: result = emit_const(state, LISP_NIL, NIL(ctx)); break;
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
		case LISP_CFUNCTION: result = record_c_call(state, bp, x); break;
		case LISP_CLOSURE:
			// Specialize to the function value in question
			// TODO Guard on prototype only
			guard_value(state, (IrRef[]) { SLOT(state, x.a) }, fn_value);
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
		if (is_cfn) { state->bp[x.a] = record_c_call(state, bp, x); goto do_ret; }
		if (/* Need reload? */ fn_value != *bp) *state->bp = state->bp[x.a];
		// Move arguments down to current frame
		memmove(state->bp + 2, state->bp + x.a + 2, x.c * sizeof *state->bp);
		state->max_slot = 2 + x.c - 1;
		// Specialize to the function value in question
		guard_value(state, state->bp, fn_value);
		if (fn_value == TAG_OBJ(state->origin)) {
			struct LispTrace *trace;
			if (!(trace = assemble_trace(ctx, state, TRACE_LINK_LOOP))) break;
#ifdef DEBUG
			puts("Reached loop start! Trace:\n");
			print_trace(state, TRACE_LINK_LOOP);
#endif
			if (UNLIKELY(state->num_traces >= LENGTH(*ctx->traces)))
				return false;
			uint16_t trace_num = state->num_traces++;
			(*ctx->traces)[trace_num] = trace;
			pc[-1] = (struct Instruction) { .op = TAIL_JIT_CALL, .a = x.a, .b = trace_num };
			return false;
		}
		break;
	case TAIL_JIT_CALL:
		if (!state->parent) { rec_err(state); break; } // Await side trace

		state->link = (*ctx->traces)[x.b];
		uint8_t arity = state->link->arity;
		// Move arguments down to current frame
		memmove(state->bp + 2, state->bp + x.a + 2, arity * sizeof *state->bp);
		state->max_slot = 2 + arity - 1;

		struct LispTrace *trace;
		if (!(trace = assemble_trace(ctx, state, TRACE_LINK_ROOT))) break;
#ifdef DEBUG
		puts("Reached root for side exit! Trace:\n");
		print_trace(state, TRACE_LINK_ROOT);
#endif
		patch_exit(state->parent, state->side_exit_num, trace);
		return false;
	case MOV: result = SLOT(state, x.c); break;
	case JMP: break;
	case JNIL:
		IrRef ref = SLOT(state, x.a);
		uint8_t ty = ref >> IR_REF_TYPE_SHIFT;
		if (ty != TY_ANY) break; // Constant NIL comparison is a no-op
		IrRef nil = emit_const(state, LISP_NIL, NIL(ctx));
		take_snapshot(state);
		emit_opt(state, (union SsaInstruction)
			{ .op = IR_EQ ^ !NILP(ctx, bp[x.a]), .ty = ty, .a = ref, .b = nil });
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
	default:
		printf("Instruction %" PRIu8 " is NYI, aborting trace...\n", x.op);
		rec_err(state);
		break;
	}

	if (result) {
		state->max_slot = MAX(state->max_slot, x.a);
		state->bp[x.a] = result;
	}
	if (state->status == REC_NYI) penalize(state);
	return state->status == REC_OK;
}
