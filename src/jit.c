#include "jit.h"
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
	uint16_t len, num_consts, frame_depth, base_offset, num_slots;

	IrRef slots[0xff], ///< Array of IR references for each VM register.
		*base; ///< Pointer into #slots at the current frame offset.
	uint16_t chain[IR_NUM_OPS];
	struct Closure *start; ///< The closure that triggered the recording.
	struct Instruction *pc;

	bool need_snapshot;
	unsigned char num_snapshots;
	unsigned num_snap_entries;
	struct Snapshot snapshots[MAX_SNAPSHOTS];
	/// Backing storage for snapshot data.
	union SnapshotEntry snap_entries[MAX_SNAPSHOT_ENTRIES];

	union SsaInstruction trace[MAX_CONSTS + MAX_TRACE_LEN];
};

static void take_snapshot(struct JitState *state) {
	if (!state->need_snapshot) return;
	if (state->num_snapshots >= MAX_SNAPSHOTS
		|| state->num_snap_entries + state->num_slots >= MAX_SNAPSHOT_ENTRIES)
		die("Too many snapshots");
	struct Snapshot *snap = state->snapshots + state->num_snapshots++;
	*snap = (struct Snapshot) {
		.ir_start = state->len, .offset = state->num_snap_entries,
	};
	for (unsigned i = 0; i < state->num_slots; ++i) {
		IrRef ref = state->slots[i];
		if (!ref) continue;
		union SsaInstruction ins = IR_GET(state, ref);
		// Skip unmodified SLOADs
		// TODO Make sure this does not skip closure/ret-addr in nested frames
		if (IS_VAR_REF(ref) && ins.op == IR_SLOAD && ins.a == i) continue;
		state->snap_entries[state->num_snap_entries++]
			= (union SnapshotEntry) { .slot = i, .ref = ref };
		++snap->num_stack_entries;
	}
	state->snap_entries[state->num_snap_entries++].pc = GC_COMPRESS(state->pc - 1);
	state->need_snapshot = false;
}

static IrRef emit(struct JitState *state, union SsaInstruction x) {
	x.prev = state->chain[x.op];
	IrRef ref = state->chain[x.op] = IR_BIAS + state->len++;
	IR_GET(state, ref) = x;
	return x.ty << IR_REF_TYPE_SHIFT | ref;
}

/** Emits @a x after peephole optimizations and CSE. */
static IrRef emit_folded(struct JitState *state, union SsaInstruction x) {
	switch (x.op) {
	case IR_NOP: return 0;
	case IR_SLOAD: return state->slots[x.a];
	case IR_EQ: case IR_NEQ: break;
	case IR_ULOAD: case IR_GLOAD: break; // TODO Check if set since last load
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
		if (GC_COMPRESS(IR_GET(state, i).v).p == GC_COMPRESS(x.v).p) goto out;
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
		for (union SnapshotEntry *entry = state->snap_entries + snap->offset,
					*end = entry + snap->num_stack_entries; entry < end; ++entry)
			if (IS_VAR_REF(entry->ref)) IR_GET(state, entry->ref).ty |= IR_MARK;

	// Propagate marks
	uint16_t *pchain[IR_NUM_OPS];
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
			if (s[-1].ir_start < state->len) {
				++state->num_snapshots;
				*s = (struct Snapshot)
					{ .ir_start = state->len, .offset = state->num_snap_entries, };
			} else --s; // Overwrite previous snapshot
			union SnapshotEntry
				*o = state->snap_entries + snap->offset, *oend = o + snap++->num_stack_entries,
				*l = state->snap_entries + loopsnap->offset, *lend = l + loopsnap->num_stack_entries,
				*n = state->snap_entries + s->offset;
			while (o < oend) {
				if (l < lend) {
					if (l->slot < o->slot) { *n++ = *l++; continue; }
					if (l->slot == o->slot) ++l; // Shadowed slot
				}
				union SnapshotEntry x = *o++;
				if (IS_VAR_REF(x.ref)) x.ref = subst[x.ref % IR_BIAS] | (x.ref & ~0xffff);
				*n++ = x;
			}
			s->num_stack_entries = n - (state->snap_entries + s->offset);
			state->num_snap_entries = s->offset + s->num_stack_entries;
			state->snap_entries[state->num_snap_entries++].pc = oend->pc;
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

	// Emit φ-functions
	for (unsigned i = 0; i < num_phis; ++i) {
		uint16_t lref = phis[i], rref = subst[lref - IR_BIAS];
		assert(lref != rref && "Invariant φ");
		emit(state, (union SsaInstruction)
			{ .op = IR_PHI, .ty = IR_GET(state, lref).ty, .a = lref, .b = rref });
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

/* Reverse linear-scan register allocation. */

#define REG_NONE 0x80 ///< Register flag for spilled or unallocated.
#define SPILL_SLOT_NONE 0xff

typedef uint32_t RegSet;
static_assert(NUM_REGS <= CHAR_BIT * sizeof(RegSet));

#define REG_LISP_CTX r15
#define REG_PC r14
/** Initial set of allocatable registers. */
#define REG_ALL (((1 << NUM_REGS) - 1) & ~(1 << rsp | 1 << REG_LISP_CTX))
#define REF_BP 0

struct RegAlloc {
	struct Assembler assembler;
	struct JitState *trace;
	// Register bit sets
	RegSet available, clobbers, phi_regs;
	union SsaInstruction bp_ins;
	uint8_t num_spill_slots, snapshot_idx;
	/** The LuaJIT register cost model. */
	alignas(32) uint16_t reg_costs[NUM_REGS];
};

static bool has_reg(union SsaInstruction x) { return !(x.reg & REG_NONE); }

/** Spills @a ref, emitting a reload. */
static enum Register reload(struct RegAlloc *ctx, uint16_t ref) {
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
		asm_rmro(&ctx->assembler, 1, XI_MOVrm, reg, rsp, x->spill_slot * sizeof x->v);
	} else { // (Re-)materialize constant
		assert(ref == REF_BP);
		reg = ctx->bp_ins.reg;
		ctx->bp_ins.reg = -1;
		asm_rmro(&ctx->assembler, 1, XI_MOVrm, reg, REG_LISP_CTX, offsetof(struct LispCtx, bp));
	}
	ctx->available |= 1 << reg;
	return reg;
}

/** Spills one of the registers in @a mask. */
static enum Register evict(struct RegAlloc *ctx, RegSet mask) {
	assert(!(ctx->available & mask) && "Redundant eviction.");
	mask &= ~ctx->available & REG_ALL;
	// TODO AVX512F _mm512_mask_reduce_min_epu32
	uint16_t min_cost = UINT16_MAX;
	for (unsigned i = 0; i < NUM_REGS; ++i)
		if (LIKELY(1 << i & mask)) min_cost = MIN(ctx->reg_costs[i], min_cost);
	assert(min_cost < UINT16_MAX && "No available registers.");
	return reload(ctx, min_cost);
}

/** Allocates a register for the definition of @a ref.
 *
 * @param mask Allowed registers, for fixed allocations.
 */
static enum Register reg_alloc(struct RegAlloc *ctx, uint16_t ref, RegSet mask) {
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
		asm_rmro(&ctx->assembler, 1, XI_MOVrr, reg, rsp, x->spill_slot * sizeof x->v);
	return reg;
}

static enum Register reg_reserve(struct RegAlloc *ctx, RegSet mask) {
	RegSet available = ctx->available & mask;
	enum Register reg = available
		// Prefer callee-saved registers
		? stdc_trailing_zeros(available & CALLEE_SAVED_REGS
			? available & CALLEE_SAVED_REGS : available)
		: evict(ctx, mask);
	ctx->clobbers |= 1 << reg;
	return reg;
}

/** Allocates a register for a use of @a ref.
 *
 * If @a ref is a constant, a load must be emitted, otherwise, if @a
 * ref has a prior masked register, then a move is needed.
 */
static enum Register reg_use(struct RegAlloc *ctx, uint16_t ref, RegSet mask) {
	union SsaInstruction *x = ref == REF_BP ? &ctx->bp_ins
		: IS_VAR_REF(ref) ? &IR_GET(ctx->trace, ref) : NULL;
	if (x && has_reg(*x) && 1 << x->reg & mask) return x->reg; // Already allocated
	RegSet available = ctx->available & mask;
	enum Register reg = x && x->reg != 0xff && 1 << (x->reg & ~REG_NONE) & available
		? x->reg & ~REG_NONE // Use hint
		: reg_reserve(ctx, mask);
	assert(!(x && has_reg(*x)) && "TODO Need to move");
	if (x && !has_reg(*x)) {
		ctx->available &= ~(1 << reg);
		ctx->reg_costs[x->reg = reg] = ref;
	}
	return reg;
}

/** Emits an Immediate Group 1 instruction with a register as 2nd operand. */
static inline void asm_grp1_imm(struct Assembler *ctx, bool w, unsigned char reg, enum Register rm, int32_t i) {
	uint8_t op;
	if ((int8_t) i == i) { *--ctx->p = i; op = 0x83; }
	else { asm_write32(ctx, i); op = 0x81; }
	*--ctx->p = MODRM(MOD_REG, reg, rm);
	*--ctx->p = op;
	EMIT_REX(ctx, w, 0, 0, rm);
}

static void asm_guard(struct RegAlloc *ctx, enum Cc cc) {
	// Emit conditional jump to side exit trampoline
	uint8_t *target = ctx->assembler.buf + MCODE_CAPACITY - 5 - 2 - 4 * ctx->snapshot_idx;
	asm_write32(&ctx->assembler, rel32(ctx->assembler.p, target));
	*--ctx->assembler.p = /* Jcc */ 0x80 | cc;
	*--ctx->assembler.p = 0x0f;
}

struct LispTrace {
	void (*f)();
	unsigned num_instructions, num_consts, num_spill_slots;
	RegSet clobbers;
	struct Snapshot snapshots[MAX_SNAPSHOTS];
	union SnapshotEntry snapshot_entries[MAX_SNAPSHOT_ENTRIES];
	union SsaInstruction instructions[128];
};

[[gnu::used]] static struct Instruction *side_exit_handler_inner(struct LispCtx *ctx, uintptr_t *regs) {
	uint8_t exit_num = *regs;
	struct LispTrace *trace = ctx->current_trace;
	struct Snapshot *snapshot = trace->snapshots + exit_num;
	printf("Side exit %" PRIu8 "\n", exit_num);

	union SnapshotEntry *e = trace->snapshot_entries + snapshot->offset,
		*eend = e + snapshot->num_stack_entries;
	for (; e < eend; ++e) {
		union SsaInstruction x = trace->instructions[(e->ref & 0xffff) - IR_BIAS + trace->num_consts];
		ctx->bp[e->slot] = IS_VAR_REF(e->ref)
			? regs[x.spill_slot != SPILL_SLOT_NONE ? 1 + x.spill_slot : -(1 + x.reg)]
			: x.v;
		printf("Restoring stack slot %" PRIu8 " to value: ", e->slot);
		lisp_print(ctx, ctx->bp[e->slot]);
		puts(".");
	}

	struct Instruction *pc = (struct Instruction *) GC_DECOMPRESS(ctx, e->pc);
	printf("Setting pc to %p\n", (void *) pc);
	*regs = trace->clobbers;
	regs[-1] = trace->num_spill_slots * sizeof *regs;
	return pc;
}

[[gnu::naked]] static void side_exit_handler() {
	__asm__ (
		// Push all GP registers
		"push rax; push rcx; push rdx; push rbx; push rsp; push rbp; push rsi; push rdi\n\t"
		"push r8; push r9; push r10; push r11; push r12; push r13; push r14; push r15\n\t"

		"mov rdi, r15\n\t" // Pass Lisp context
		"lea rsi, [rsp+16*8]\n\t"
		"call side_exit_handler_inner\n\t"
		"mov r14, rax\n\t" // Set new PC
		// TODO Set bp

		"mov rcx, [rsp+16*8]\n\t" // Load clobbers
		"mov rax, [rsp+(16-1)*8]\n\t" // Load stack offset
		"lea rsp, [rsp+rax+(16+1)*8]\n\t"
		// Restore callee-saved registers
		"bt rcx, 3; jnc 0f; pop rbx; 0:\n\t"
		"bt rcx, 5; jnc 0f; pop rbp; 0:\n\t"
		"bt rcx, 12; jnc 0f; pop r12; 0:\n\t"
		"bt rcx, 13; jnc 0f; pop r13; 0:\n\t"

		"ret"
	);
}

static struct LispTrace *assemble_trace(struct JitState *trace) {
	struct RegAlloc ctx = { .trace = trace, .available = REG_ALL, .bp_ins.reg = -1,
		.snapshot_idx = trace->num_snapshots - 1, };
	if (!asm_init(&ctx.assembler)) die("asm_init failed");
	// Emit exit trampolines
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
	asm_write32(&ctx.assembler, rel32(ctx.assembler.p, (uint8_t *) side_exit_handler));
#pragma GCC diagnostic pop
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
	void *asm_end = ctx.assembler.p;
	asm_write32(&ctx.assembler, 0);
	*--ctx.assembler.p = /* JMP */ 0xe9;

	for (unsigned i = trace->len; i-- > 0;) {
		if (ctx.snapshot_idx && i < trace->snapshots[ctx.snapshot_idx].ir_start)
			--ctx.snapshot_idx;

		uint16_t ref = IR_BIAS + i;
		union SsaInstruction x = IR_GET(trace, ref);
		switch (x.op) {
		case IR_SLOAD:
			/* if (x.reg == 0xff) break; // Unused */
			enum Register reg = reg_alloc(&ctx, ref, -1), bp = reg_use(&ctx, REF_BP, -1);
			asm_rmro(&ctx.assembler, 1, XI_MOVrm, reg, bp, x.a * sizeof x.v);
			break;
		case IR_GLOAD:
			reg = reg_alloc(&ctx, ref, -1);
			int32_t p = GC_COMPRESS(UNTAG_OBJ(IR_GET(trace, x.a).v)).p + offsetof(struct Symbol, value);
			asm_rmro(&ctx.assembler, 1, XI_MOVrm, reg, REG_LISP_CTX, p);
			break;
		case IR_ULOAD: // TODO
			reg = reg_alloc(&ctx, ref, -1);
			break;

		case IR_EQ:
		case IR_NEQ:
			assert(IS_VAR_REF(x.a));
			if (x.ty == LISP_CLOSURE) break; // FIXME: Ignore ULOAD for now
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
			asm_rmro(&ctx.assembler, 0, XI_GRP5, /* CALL */ 2, f_reg, offsetof(struct LispCFunction, f) - 1);

			uint32_t arg_regs = rdi | rsi << 4 | rdx << 8 | rcx << 12 | r8 << 16 | r9 << 20;
			asm_mov_reg_reg(&ctx.assembler, arg_regs & 0xf, REG_LISP_CTX);
			asm_loadu64(&ctx.assembler, arg_regs >> 4 & 0xf, x.b);
			asm_mov_reg_reg(&ctx.assembler, arg_regs >> 2 * 4 & 0xf, rsp);
			for (unsigned j = 0; j <  x.b; ++j) {
				uint16_t arg_ref = trace->trace[MAX_CONSTS + i + 1 + j].a;
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

		case IR_LOOP:
			// Reload invariants whose registers get clobbered
			FOR_ONES(reg, ctx.clobbers & ~(ctx.available | ctx.phi_regs))
				reload(&ctx, ctx.reg_costs[reg]);

			int32_t jmp_disp = rel32(asm_end, ctx.assembler.p);
			memcpy((int32_t *) asm_end - 1, &jmp_disp, sizeof jmp_disp);
			break;
		case IR_PHI:
			assert(IS_VAR_REF(x.a) && IS_VAR_REF(x.b) && "Variant ref cannot be constant.");
			union SsaInstruction *in = &IR_GET(trace, x.a), *out = &IR_GET(trace, x.b);
			enum Register dst;
			// φ-resolution
			if (LIKELY(!has_reg(*out))) {
				// Pick φ registers in opposite direction to reduce risk of collisions
				RegSet mask = ctx.available ? 1 << (stdc_bit_width(ctx.available) - 1) : -1;
				dst = reg_use(&ctx, x.b, mask);
			} else {
				// Outgoing variable already has a location: Move from out to in
				dst = reg_reserve(&ctx, -1);
				// TODO out may be spilled or constant
				asm_mov_reg_reg(&ctx.assembler, dst, out->reg);
			}
			in->reg = dst | REG_NONE; // Add hint
			ctx.phi_regs |= 1 << dst;
			break;
		case IR_NOP: break;
		default: unreachable();
		}
	}

	reload(&ctx, REF_BP);
	assert(ctx.available == REG_ALL);
	asm_grp1_imm(&ctx.assembler, 1, /* SUB */ 5, rsp, ctx.num_spill_slots * sizeof(void *));
	// Save callee-saved registers
	FOR_ONES(reg, ctx.clobbers = ctx.clobbers & CALLEE_SAVED_REGS & ~(1 << REG_PC)) {
		*--ctx.assembler.p = /* PUSH */ 0x50 + (reg & 7);
		EMIT_REX(&ctx.assembler, 0, 0, 0, reg);
	}

	struct LispTrace *mytrace;
	if (!(mytrace = malloc(sizeof *mytrace))) die("malloc failed");
	mytrace->num_instructions = trace->len;
	mytrace->num_consts = trace->num_consts;
	mytrace->num_spill_slots = ctx.num_spill_slots;
	mytrace->clobbers = ctx.clobbers;
	memcpy(mytrace->snapshots, trace->snapshots,
		trace->num_snapshots * sizeof *trace->snapshots);
	memcpy(mytrace->snapshot_entries, trace->snap_entries,
		trace->num_snap_entries * sizeof *trace->snap_entries);
	memcpy(mytrace->instructions, trace->trace + MAX_CONSTS - trace->num_consts,
		(trace->num_consts + trace->len) * sizeof *trace->trace);
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
	for (unsigned i = 0, snap_idx = 0; i < state->len; ++i) {
		struct Snapshot *snap = state->snapshots + snap_idx;
		if (snap_idx < state->num_snapshots && i >= snap->ir_start) {
			printf("....         SNAP  #%-3u [ ", snap_idx++);
			unsigned j = 0;
			for (union SnapshotEntry *entry = state->snap_entries + snap->offset,
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
	state->pc = pc;
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
		state->need_snapshot = true;
		if (fun_value == TAG_OBJ(state->start)) {
			*state->base = cur; // Avoid reloading the same closure
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
		if (ty != TY_ANY) break; // Constant NIL comparison is a no-op
		IrRef nil = emit_const(state, LISP_NIL, (union SsaInstruction) { .v = NIL });
		take_snapshot(state);
		emit_folded(state, (union SsaInstruction)
			{ .op = IR_EQ ^ !bp[x.a], .ty = ty, .a = ref, .b = nil });
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
