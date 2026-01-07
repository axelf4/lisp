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
#include "lisp_tracepoint.h"

#define IR_BIAS 0x8000
#define REF_BP (IR_BIAS - 1) ///< Virtual representing #LispCtx.bp.
#define IS_VAR(ref) ((Ref) (ref) >= IR_BIAS)
#define IR_GET(state, ref) (state)->trace[(Ref) (ref) - IR_BIAS + MAX_CONSTS]
#define HAS_REG(x) (!((x).reg & REG_NONE))

#define MAX_CONSTS UINT8_MAX
#define MAX_TRACE_LEN 512
#define MAX_SNAPSHOTS 64
#define MAX_SNAPSHOT_ENTRIES UINT8_MAX
#define TRACE_ATTEMPTS 4 ///< Number of times to try to compile a trace.
#define SIDE_TRACE_THRESHOLD 5

#define REF_TYPE_SHIFT 16
#define IR_MARK 0x80 ///< Multi-purpose flag.
#define REG_NONE 0x80 ///< Spilled or unallocated register flag.

#define REG_LISP_CTX r15
/** Initial set of allocatable registers. */
#if PRESERVE_FRAME_POINTER
#define REG_ALL (((1 << NUM_REGS) - 1) & ~(1 << rsp | 1 << rbp | 1 << REG_LISP_CTX))
#else
#define REG_ALL (((1 << NUM_REGS) - 1) & ~(1 << rsp | 1 << REG_LISP_CTX))
#endif

typedef uint16_t Ref; ///< Reference to a virtual or constant of a trace.
typedef uint32_t IrRef;
typedef uint32_t RegSet;
static_assert(NUM_REGS <= CHAR_BIT * sizeof(RegSet));

enum {
	TY_ANY = LISP_INTEGER + 1,
	TY_RET_ADDR,
};

enum SsaOp : uint8_t {
	// Comparisons are ordered such that flipping the LSB inverts them
	IR_LT, IR_GE, IR_LE, IR_GT,
	IR_EQ, IR_NEQ,
	IR_CALL, ///< Call C function.
	IR_CALLARG, ///< Argument for #IR_CALL.
	IR_RET,
	IR_SLOAD, ///< Load stack slot.
	IR_GLOAD, ///< Load global.
	IR_ULOAD, ///< Load upvalue.
	IR_PLOAD, ///< Load from parent trace.
	IR_LOOP,
	IR_PHI,
	IR_NOP,

	IR_ADD,

	IR_NUM_OPS
};

union Node {
	struct {
		enum SsaOp op;
		uint8_t ty; ///< Type of the result (superset of @ref LispObjectType).
		Ref /** Operand 1*/ a, /** Operand 2 */ b;
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
	Ref beg; /// One past the last virtual recorded at time of capture.
	uint8_t offset, ///< Offset into @ref JitState::stack_entries.
		num_entries, ///< Number of stack entries.
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
	TRACE_LINK_INTERPR, ///< Fallback to interpreter.
	TRACE_LINK_UPREC, ///< Up-recursion.
};

struct LispTrace {
	uint8_t arity, num_snapshots, num_spill_slots;
	uint16_t len, num_consts;
	void (*f)(); ///< Machine code entry.
	uint32_t mcode_size,
		mcode_tail; ///< Offset from #f to end of machine code block.
	alignas(union Node) char data[];
};

enum RecordStatus : unsigned char {
	REC_OK, ///< Incomplete trace recording.
	REC_NYI, ///< Abort recording, e.g., due to NYI (not yet implemented).
};

/** Closure JIT penalty FIFO-cache slot. */
struct Penalty {
	struct Instruction *pc;
	unsigned char value;
};

/** In-progress trace recording. */
struct JitState {
	Ref end;
	uint8_t num_consts, num_snapshots, num_stack_entries,
		max_slot, base_offset, side_exit_num;
	bool need_snapshot;
	enum RecordStatus status;
	IrRef *bp, ///< Pointer into @ref #slots at the current frame offset.
		slots[0x100]; ///< Array of IR references for each VM register.
	Ref chain[IR_NUM_OPS];

	struct Closure *origin; ///< The closure that triggered the recording.
	struct LispTrace *parent, *link;
	struct Instruction *pc;
	struct Instruction *origin_pc;

	uint16_t num_traces; ///< Length of #LispCtx::traces.
	unsigned char next_penalty_slot;
	struct Penalty penalties[32];

	struct Snapshot snapshots[MAX_SNAPSHOTS];
	/// Backing storage for snapshot data.
	struct SnapshotEntry stack_entries[MAX_SNAPSHOT_ENTRIES];
	union Node trace[MAX_CONSTS + MAX_TRACE_LEN];
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
	state->end = IR_BIAS;
	state->bp = state->slots;
	state->need_snapshot = true;
}

void jit_init_root(struct JitState *state, struct Closure *f, struct Instruction *pc) {
	jit_init(state);
	state->origin = f;
	state->origin_pc = pc - 1;
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
		printf("[%.*s]", sym->len, sym->name);
		break;
	case LISP_CFUNCTION:
		printf("<%s>", ((struct LispCFunction *) UNTAG_OBJ(v))->name);
		break;
	case LISP_CLOSURE: default: printf("%#" PRIxPTR, v); break;
	}
}

static void print_trace(struct JitState *state, enum TraceLink link) {
	const char *ops[] = {
		"LT", "GE", "LE", "GT", "EQ", "NEQ", [IR_PHI] = "PHI", NULL,
		"ADD",
	}, *type_names[]
		= { "AxB", "sym", "str", "cfn", "clo", NULL, NULL, "nil", "int", "___" },
		*link_names[] = { "loop", "root", "interpreter", "up-recursion" };

	puts("\n---- TRACE IR");
	for (unsigned i = IR_BIAS, snap_idx = 0; ; ++i) {
		struct Snapshot *snap = state->snapshots + snap_idx;
		if (snap_idx < state->num_snapshots && i >= snap->beg) {
			printf("....         SNAP  #%-3u [ ", snap_idx++);
			unsigned j = 0;
			FOR_SNAPSHOT_ENTRIES(snap, state->stack_entries, entry) {
				while (j++ < entry->slot) printf("---- ");
				print_ref(state, entry->ref);
				putchar(' ');
			}
			puts("]");
		}
		if (i >= state->end) break;

		union Node x = IR_GET(state, i);
		printf("%.4u %s %3u ", i - IR_BIAS, type_names[x.ty & ~IR_MARK], x.reg);
		switch (x.op) {
		case IR_SLOAD: printf("SLOAD #%" PRIu16 "\n", x.a); break;
		case IR_GLOAD:
			printf("GLOAD "); print_ref(state, x.a); putchar('\n'); break;
		case IR_ULOAD:
			printf("ULOAD #%" PRIu16 " from ", x.b);
			print_ref(state, x.a);
			putchar('\n');
			break;
		case IR_PLOAD: printf("PLOAD %3u %u\n", x.a, x.b); break;
		case IR_CALL: printf("CALL  ");
			print_ref(state, x.a);
			printf("  (");
			for (unsigned j = 0; j < x.b; ++j) {
				union Node arg = IR_GET(state, ++i);
				if (j) putchar(' ');
				print_ref(state, arg.a);
			}
			puts(")");
			break;
		case IR_CALLARG: unreachable();
		case IR_RET: puts("RET"); break;
		case IR_LOOP: puts("LOOP ------------"); break;
		case IR_NOP: puts("NOP"); break;
		default:
			printf("%-5s ", ops[x.op]);
			if (x.a) {
				print_ref(state, x.a);
				if (x.b) { printf("  "); print_ref(state, x.b); }
			}
			putchar('\n');
			break;
		}
	}
	printf("---- TRACE stop -> %s\n", link_names[link]);
}
#endif

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
	*snapshot = (struct Snapshot)
		{ .beg = state->end, .offset = state->num_stack_entries,
		  .base_offset = state->base_offset, .pc = GC_COMPRESS(state->pc - 1) };
	for (unsigned i = 0; i <= max_slot; ++i) {
		IrRef ref = state->slots[i];
		if (!ref) continue;
		union Node x = IR_GET(state, ref);
		// Skip unmodified SLOADs
		if (IS_VAR(ref) && x.op == IR_SLOAD && x.a == i) continue;
		state->stack_entries[state->num_stack_entries++] = (struct SnapshotEntry)
			{ .slot = i, .ref = ref, .ty = ref >> REF_TYPE_SHIFT };
		++snapshot->num_entries;
	}
}

static IrRef emit_const(struct JitState *state, uint8_t ty, uintptr_t x) {
	Ref i = REF_BP;
	while (i-- > REF_BP - state->num_consts)
		if (LISP_EQ(IR_GET(state, i).v, x)) goto out;
	if (++state->num_consts >= MAX_CONSTS) { rec_err(state); return 0; }
	IR_GET(state, i).v = x;
out: return ty << REF_TYPE_SHIFT | i;
}

static IrRef emit(struct JitState *state, union Node x) {
	if (state->end >= IR_BIAS + MAX_TRACE_LEN) { rec_err(state); return 0; }
	x.prev = state->chain[x.op];
	Ref ref = state->chain[x.op] = state->end++;
	IR_GET(state, ref) = x;
	return x.ty << REF_TYPE_SHIFT | ref;
}

static bool cmp(LispObject a, enum SsaOp op, LispObject b) {
	switch (op) {
	case IR_LT: return a < b;
	case IR_GE: return a >= b;
	case IR_LE: return a <= b;
	case IR_GT: return a > b;
	default: unreachable();
	}
}

/** Normalizes a commutative instruction. */
static union Node comm_swap(union Node x) {
	// Swap lower refs (constants in particular) to the right
	if (x.a < x.b) { Ref tmp = x.a; x.a = x.b; x.b = tmp; }
	return x;
}

/** Emits @a x after peephole optimizations and CSE. */
static IrRef emit_opt(struct JitState *state, union Node x) {
	/** Drop if true, otherwise fail. */
#define CONDFOLD(cond) do { if (!(cond)) rec_err(state); return 0; } while (0)

	switch (x.op) {
	case IR_NOP: return 0;
	case IR_SLOAD: return state->slots[x.a];
	case IR_EQ: case IR_NEQ:
		if (x.a == x.b) CONDFOLD(x.op == IR_EQ);
		x = comm_swap(x);
		if (!IS_VAR(x.a)) CONDFOLD(x.op != IR_EQ);
		break;
	case IR_LT: case IR_GE: case IR_LE: case IR_GT:
		if (x.a == x.b) CONDFOLD(/* inclusive? */ (x.op >> 1 & x.op) & 1);
		if (x.a < x.b) x = (union Node) {{ x.op ^ 0b11, x.ty, x.b, x.a, {} }};
		if (!IS_VAR(x.a))
			CONDFOLD(cmp(IR_GET(state, x.a).v, x.op, IR_GET(state, x.b).v));
		break;
	case IR_GLOAD: case IR_ULOAD: break; // Forward loads of globals/upvalues
	default: goto out_no_cse;

	case IR_ADD:
		x = comm_swap(x);
		if (LISP_EQ(IR_GET(state, x.b).v, TAG_SMI(0))) return x.a;
		if (!IS_VAR(x.a)) return emit_const(state, LISP_INTEGER,
			IR_GET(state, x.a).v + IR_GET(state, x.b).v);
		break;
	}
	// Common Subexpression Elimination (CSE)
	union Node o;
	for (Ref ref = state->chain[x.op], lim = MAX(x.a, x.b); ref > lim; ref = o.prev) {
		o = IR_GET(state, ref);
		if (o.a == x.a && o.b == x.b) return o.ty << REF_TYPE_SHIFT | ref;
	}
out_no_cse: return emit(state, x);
}

[[gnu::noinline]] static IrRef sload(struct JitState *state, int slot) {
	take_snapshot(state); // Type guard may be added
	return state->bp[slot] = emit(state, (union Node)
		{ .op = IR_SLOAD, .ty = TY_ANY, .a = state->base_offset + slot });
}
/** Emits a load of stack slot @a i unless it is already loaded. */
#define SLOT(state, i) ((state)->bp[i] ? (state)->bp[i] : sload(state, i))

/** Emits an instruction to load the given upvalue. */
static IrRef uref(struct JitState *state, uintptr_t *bp, uint8_t idx) {
	struct Upvalue *upvalue = ((struct Closure *) UNTAG_OBJ(*bp))->upvalues[idx];
	// TODO Track whether the upvalue is immutable and inlinable
	if (IS_UV_OPEN(*upvalue)) {
		// In a nested frame the upvalue may be available on the stack
		ptrdiff_t slot = upvalue->location - (bp - state->base_offset);
		if (slot >= 0) return SLOT(state, slot - state->base_offset);
	}
	take_snapshot(state);
	return emit_opt(state, (union Node) { .op = IR_ULOAD,
			.ty = TY_ANY, .a = SLOT(state, /* this closure */ 0), .b = idx });
}

/** Coerces @a ref to @a ty.
 *
 * @return Whether the type of @a ref was compatible with @a ty.
 */
static bool guard_type(struct JitState *state, IrRef *ref, uint8_t ty) {
	uint8_t ty2 = *ref >> REF_TYPE_SHIFT;
	if (ty == TY_ANY || ty2 == ty) return true;
	// If ty </: ref->ty, then type error is imminent
	if (IS_VAR(*ref))
		*ref = (IR_GET(state, *ref).ty = ty) << REF_TYPE_SHIFT | (Ref) *ref;
	return ty2 == TY_ANY;
}

static void guard_value(struct JitState *state, IrRef *ref, LispObject value) {
	if (!IS_VAR(*ref)) return;
	enum LispObjectType ty = lisp_type(value);
	take_snapshot(state);
	emit_opt(state, (union Node)
		{ .op = IR_EQ, .ty = ty, .a = *ref, .b = emit_const(state, ty, value) });
	guard_type(state, ref, ty);
}

static bool is_effectful(enum SsaOp x) { return x <= IR_RET; }

/** Dead-code elimination (DCE). */
static void dce(struct JitState *state) {
	// Mark virtuals escaping into snapshots
	for (struct Snapshot *s = state->snapshots, *end = s + state->num_snapshots;
			s < end; ++s)
		FOR_SNAPSHOT_ENTRIES(s, state->stack_entries, entry)
			if (IS_VAR(entry->ref)) IR_GET(state, entry->ref).ty |= IR_MARK;
	Ref *pchain[IR_NUM_OPS];
	for (unsigned i = 0; i < IR_NUM_OPS; ++i) pchain[i] = state->chain + i;
	// Sweep in reverse while propagating marks
	for (Ref i = state->end; i-- > IR_BIAS;) {
		union Node *x = &IR_GET(state, i);
		if (!(x->ty & IR_MARK || is_effectful(x->op))) {
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

#define SUBST_GET(subst, ref) (*(Ref *) ((subst) + (ref) * sizeof(Ref)))
static void subst_snapshot(struct JitState *state, uintptr_t subst,
	struct Snapshot *os, struct Snapshot *loopsnap) {
	struct Snapshot *ns = state->snapshots + state->num_snapshots;
	unsigned offset = ns[-1].beg < state->end
		? ++state->num_snapshots, state->num_stack_entries
		: (--ns)->offset; // Overwrite previous snapshot
	struct SnapshotEntry
		*o = state->stack_entries + os->offset, *oend = o + os->num_entries,
		*l = state->stack_entries + loopsnap->offset, *lend = l + loopsnap->num_entries,
		*n = state->stack_entries + offset, *nbeg = n;
	while (o < oend) {
		if (l < lend) {
			if (l->slot < o->slot) { *n++ = *l++; continue; }
			if (l->slot == o->slot) ++l; // Shadowed
		}
		struct SnapshotEntry x = *o++;
		if (IS_VAR(x.ref)) x.ref = SUBST_GET(subst, x.ref);
		*n++ = x;
	}
	while (l < lend) *n++ = *l++;
	*ns = (struct Snapshot)
		{ .beg = state->end, .pc = os->pc, .offset = offset,
		  .num_entries = n - nbeg, .base_offset = os->base_offset };
	state->num_stack_entries = offset + ns->num_entries;
}

/** Peels off a preamble from the loop.
 *
 * @see ARDÖ, Håkan; BOLZ, Carl Friedrich; FIJABKOWSKI, Maciej.
 *      Loop-aware optimizations in PyPy's tracing JIT. In:
 *      Proceedings of the 8th symposium on Dynamic languages. 2012.
 *      p. 63-72.
 */
static void peel_loop(struct JitState *state) {
	unsigned preamble_end = state->end, num_phis = 0;
	struct Snapshot *s = state->snapshots, *loopsnap = s + state->num_snapshots - 1;
	// Separate preamble from loop body by LOOP instruction
	emit(state, (union Node) { .op = IR_LOOP, .ty = TY_ANY });
	++loopsnap->beg;

	Ref subst_buf[MAX_TRACE_LEN], phis[12];
	// Map of virtuals in the preamble onto virtuals of the peeled loop
	uintptr_t subst = (uintptr_t) subst_buf - IR_BIAS * sizeof *subst_buf;
	for (Ref i = IR_BIAS; i < preamble_end; ++i) {
		if (i >= s->beg) subst_snapshot(state, subst, s++, loopsnap);

		union Node insn = IR_GET(state, i);
		if (IS_VAR(insn.a)) insn.a = SUBST_GET(subst, insn.a);
		if (IS_VAR(insn.b)) insn.b = SUBST_GET(subst, insn.b);

		IrRef ref = emit_opt(state, insn), j = (Ref) ref;
		if (j != i && j < preamble_end) { // Loop-carried dependency
			// SLOAD:s of arguments varied in tail call give rise to φ:s
			if (IS_VAR(ref)) {
				if (num_phis >= LENGTH(phis)) rec_err(state);
				else phis[num_phis++] = ref;
			}
			// In case of type-instability, need to emit conversion
			// since later instructions expect the previous type.
			if (!guard_type(state, &ref, insn.ty)) rec_err(state);
		}
		SUBST_GET(subst, i) = ref;
	}

	for (unsigned i = 0; i < num_phis; ++i) { // Emit φ-functions
		Ref lref = phis[i], rref = SUBST_GET(subst, lref);
		// TODO φ:s whose arguments are the same or other redundant
		// φ:s are in turn redundant and eliminable.
		if (lref == rref) continue; // Redundant φ
		emit(state, (union Node) {{ IR_PHI, TY_ANY, lref, rref, {} }});
	}
}

/* Reverse linear-scan register allocation */

struct RegAlloc {
	struct Assembler as;
	struct JitState *state;
	RegSet available, phi_regs, clobbers;
	uint8_t num_spill_slots, snapshot_idx;
	Ref next_snapshot_beg;
	uint8_t *end;
	/** The LuaJIT register cost model. */
	alignas(32) Ref reg_costs[NUM_REGS];
	Ref phi_refs[NUM_REGS]; ///< For each #phi_regs bit, its corresponding "in".
};

/** Spills @a ref, emitting a reload. */
static enum Register reload(struct RegAlloc *ctx, Ref ref) {
	union Node *x = &IR_GET(ctx->state, ref);
	assert(HAS_REG(*x) && "evicting unallocated virtual");
	enum Register reg = x->reg;
	if (IS_VAR(ref)) {
		if (!x->spill_slot && !(x->spill_slot = ++ctx->num_spill_slots))
			rec_err(ctx->state); // Out of spill slots
		// Reload from stack
		asm_rmrd(&ctx->as, 1, XI_MOVrm, reg, rsp, (x->spill_slot - 1) * sizeof x->v);
	} else { // (Re-)materialize constant
		assert(ref == REF_BP);
		asm_rmrd(&ctx->as, 1, XI_MOVrm, reg,
			REG_LISP_CTX, offsetof(struct LispCtx, bp));
	}
	x->reg |= REG_NONE;
	ctx->available |= 1 << reg;
	ctx->clobbers |= 1 << reg;
	return reg;
}

/** Spills one of the registers in @a mask. */
[[gnu::cold]] static enum Register evict(struct RegAlloc *ctx, RegSet mask) {
	assert(!(ctx->available & mask) && "redundant eviction");
	mask &= ~ctx->available & REG_ALL;
	Ref min_cost = UINT16_MAX;
	for (unsigned i = 0; i < NUM_REGS; ++i)
		if (LIKELY(1 << i & mask)) min_cost = MIN(ctx->reg_costs[i], min_cost);
	assert(min_cost < UINT16_MAX && "no available registers");
	return reload(ctx, min_cost);
}

static enum Register reg_alloc(struct RegAlloc *ctx, RegSet mask) {
	return ctx->available & mask
		? stdc_trailing_zeros(ctx->available & mask) : evict(ctx, mask);
}

/** Allocates a register for the definition of @a ref. */
static enum Register reg_def(struct RegAlloc *ctx, Ref ref, RegSet mask) {
	assert(IS_VAR(ref));
	union Node x = IR_GET(ctx->state, ref);
	enum Register reg
		= HAS_REG(x) && 1 << x.reg & mask ? x.reg // Already allocated
		: reg_alloc(ctx, mask);
	if (HAS_REG(x)) {
		if (reg != x.reg) // Existing allocation was masked
			asm_mov(&ctx->as, x.reg, reg);
		ctx->available |= 1 << x.reg; // Free register
		ctx->clobbers |= 1 << x.reg;
	}
	ctx->clobbers |= 1 << reg;
	// Save ASAP as exits use spilled value of split virtual over its register
	if (x.spill_slot)
		asm_rmrd(&ctx->as, 1, XI_MOVmr, reg, rsp, (x.spill_slot - 1) * sizeof x.v);
	return reg;
}

/** Allocates a register for usage of @a ref.
 *
 * @note If @a ref already has a register, it must be in @a mask.
 */
static enum Register reg_use(struct RegAlloc *ctx, Ref ref, RegSet mask) {
	union Node *x = &IR_GET(ctx->state, ref);
	assert(IS_VAR(ref) || ref == REF_BP);
	if (HAS_REG(*x)) { assert(1 << x->reg & mask); return x->reg; }
	RegSet available = ctx->available & mask;
	enum Register hint = x->reg % stdc_bit_ceil_uc(NUM_REGS),
		reg = 1 << hint & available ? hint : reg_alloc(ctx, mask);
	ctx->available &= ~(1 << reg);
	ctx->reg_costs[reg] = ref;
	return x->reg = reg;
}

#define EXIT_TRAMPOLINE(end, exit_num) ((end) - 7 - 4 * (exit_num))
/** Emits a conditional jump to the side exit trampoline. */
static void asm_guard(struct RegAlloc *ctx, enum Cc cc) {
	uint8_t *target = EXIT_TRAMPOLINE(ctx->as.buf + MCODE_CAPACITY, ctx->snapshot_idx);
	asm_write32(&ctx->as, REL32(ctx->as.p, target));
	*--ctx->as.p = XI_Jcc | cc;
	*--ctx->as.p = 0x0f;
}

/** Assembles synchronization of interpreter stack with on-trace state. */
static void asm_stack_restore(struct RegAlloc *ctx) {
	enum Register bp = reg_use(ctx, REF_BP, -1);
	int32_t dbp = ctx->state->base_offset * sizeof(LispObject);
	if (dbp) // Shift base pointer to next frame
		asm_rmrd(&ctx->as, 1, asm_imm_grp1_op(&ctx->as, dbp),
			(uint8_t) XG_ADD, REG_LISP_CTX, offsetof(struct LispCtx, bp));

	struct Snapshot *snapshot = ctx->state->snapshots + ctx->state->num_snapshots - 1;
	FOR_SNAPSHOT_ENTRIES(snapshot, ctx->state->stack_entries, entry) {
		union Node x = IR_GET(ctx->state, entry->ref);
		if (IS_VAR(entry->ref)) {
			enum Register reg = reg_use(ctx, entry->ref, ~(1 << bp));
			asm_rmrd(&ctx->as, 1, XI_MOVmr, reg, bp, entry->slot * sizeof x.v);
		} else asm_mov_mi64(&ctx->as, bp, entry->slot * sizeof x.v, x.v);
	}
}

/** Assembles the @ref IR_LOOP instruction. */
static void asm_loop(struct RegAlloc *ctx) {
	// Reload invariants whose registers get clobbered
	FOR_ONES(reg, ctx->clobbers & ~(ctx->available | ctx->phi_regs))
		reload(ctx, ctx->reg_costs[reg]);

	// φ-resolution
	// TODO Shuffle φ-registers without always spilling in between
	FOR_ONES(reg, ctx->phi_regs) {
		Ref lref = ctx->phi_refs[reg];
		union Node *in = &IR_GET(ctx->state, lref);
		if (LIKELY(in->reg == reg)) continue;
		if (HAS_REG(*in)) reload(ctx, lref);
		if (!(1 << reg & ctx->available)) reload(ctx, ctx->reg_costs[reg]);
	}
	FOR_ONES(reg, ctx->phi_regs) {
		Ref lref = ctx->phi_refs[reg];
		union Node *in = &IR_GET(ctx->state, lref);
		if (LIKELY(!in->spill_slot)) continue;
		reg_def(ctx, lref, 1 << reg); // Resave "in" on each iteration
		in->ty |= IR_MARK; // Remember that spilling is handled by loop
		reg_use(ctx, lref, 1 << reg); // Allocate (now free) register
	}

	int32_t jmp_dest = REL32(ctx->end, ctx->as.p);
	memcpy(ctx->end - sizeof jmp_dest, &jmp_dest, sizeof jmp_dest);
}

static void asm_phi(struct RegAlloc *ctx, union Node x) {
	union Node *in = &IR_GET(ctx->state, x.a),
		out [[maybe_unused]] = IR_GET(ctx->state, x.b);
	RegSet available = ctx->available & ~ctx->phi_regs;
	assert(IS_VAR(x.a) && IS_VAR(x.b) && "constant variant ref");
	assert(stdc_count_ones(available) && "out of φ registers");
	assert(!HAS_REG(*in) && !HAS_REG(out));
	// Pick φ registers from opposite end to reduce collisions
	enum Register dst = stdc_bit_width(available) - 1;
	reg_use(ctx, x.b, 1 << dst);
	in->reg = dst | REG_NONE; // Add hint
	ctx->phi_regs |= 1 << dst;
	ctx->phi_refs[dst] = x.a;
}

static void asm_call(struct RegAlloc *ctx, Ref ref) {
	reg_def(ctx, ref, 1 << rax);
	// Evict caller-saved registers
	FOR_ONES(reg, REG_ALL & ~CALLEE_SAVED_REGS & ~ctx->available)
		reload(ctx, ctx->reg_costs[reg]);
	ctx->clobbers |= REG_ALL & ~CALLEE_SAVED_REGS;

#define ARG_REGS (1 << rdi | 1 << rsi | 1 << rdx | 1 << rcx | 1 << r8 | 1 << r9)
	union Node x = IR_GET(ctx->state, ref);
	enum Register f_reg = reg_use(ctx, x.a, ~ARG_REGS);
	asm_rmrd(&ctx->as, 0, XI_GRP5, /* CALL */ 2,
		f_reg, offsetof(struct LispCFunction, f) - 1);

	uint32_t arg_regs = rdi | rsi << 4 | rdx << 8 | rcx << 12 | r8 << 16 | r9 << 20;
	asm_mov(&ctx->as, arg_regs & 0xf, REG_LISP_CTX);
	asm_loadu64(&ctx->as, (arg_regs >>= 4) & 0xf, x.b);
	enum Register args_reg = (arg_regs >>= 4) & 0xf;
	for (unsigned i = x.b; i--;) {
		Ref arg_ref = IR_GET(ctx->state, ref + 1 + i).a;
		union Node arg = IR_GET(ctx->state, arg_ref);
		if (!IS_VAR(arg_ref)) {
			if (IS_SMI(arg.v)) {
				asm_write32(&ctx->as, arg.v);
				asm_rmrd(&ctx->as, 0, XI_MOVmi, 0, args_reg, i * sizeof arg.v);
			} else asm_mov_mi64(&ctx->as, args_reg, i * sizeof arg.v, arg.v);
			continue;
		}
		enum Register arg_reg = reg_use(ctx, arg_ref, ~(1 << args_reg));
		asm_rmrd(&ctx->as, 1, XI_MOVmr, arg_reg, args_reg, i * sizeof arg.v);
	}
	asm_rmrd(&ctx->as, 1, XI_LEA, args_reg, rsp, ctx->num_spill_slots * sizeof x.v);
	ctx->num_spill_slots += x.b;
}

static void asm_ret(struct RegAlloc *ctx, union Node x) {
	enum Register bp = reg_use(ctx, REF_BP, -1);
	asm_rmrd(&ctx->as, 1, XI_MOVmr, bp, REG_LISP_CTX, offsetof(struct LispCtx, bp));
	asm_grp1_imm(&ctx->as, 1, XG_SUB, bp, x.b * sizeof(LispObject));

	uintptr_t pc = IR_GET(ctx->state, x.a).v;
	asm_guard(ctx, CC_NE); // Guard the return address
	asm_write32(&ctx->as, pc);
	asm_rmrd(&ctx->as, 0, 0x81, (uint8_t) XG_CMP, bp, sizeof(LispObject));
}

static void asm_arith(struct RegAlloc *ctx, enum ImmGrp1 op, Ref ref) {
	union Node x = IR_GET(ctx->state, ref),
		a = IR_GET(ctx->state, x.a), b = IR_GET(ctx->state, x.b);
	assert(IS_VAR(x.a) && "non-folded constant arithmetic op");

	asm_guard(ctx, CC_O);

	RegSet mask = ~(IS_VAR(x.b) && HAS_REG(b) ? 1 << b.reg : 0);
	enum Register dst = reg_def(ctx, ref, mask);
	if (IS_VAR(x.b)) {
		enum Register src = reg_use(ctx, x.b, ~(1 << dst));
		asm_rr(&ctx->as, 0, IMM_GRP1_MR(op), src, dst);
	} else asm_grp1_imm(&ctx->as, 0, op, dst, (uint32_t) b.v);

	// Fix up 2-operand instruction by moving left operand to destination
	if (HAS_REG(a)) asm_mov(&ctx->as, dst, a.reg);
	else reg_use(ctx, x.a, 1 << dst);
}

static void patch_exit(struct LispTrace *parent, uint8_t exit_num, struct LispTrace *trace) {
	union Node *insns = (union Node *) parent->data;
	struct Snapshot *snapshot = (struct Snapshot *) (insns + parent->len) + exit_num;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
	uint8_t *beg = (uint8_t *) parent->f, *end = beg + parent->mcode_size,
		*page = (uint8_t *) ((uintptr_t) beg & ~(page_size() - 1)),
		*trampoline = EXIT_TRAMPOLINE(beg + parent->mcode_tail, exit_num);
#pragma GCC diagnostic pop

	snapshot->trace = trace;
	if (mprotect(page, end - page, PROT_READ | PROT_WRITE))
		err: die("mprotect failed");
	for (uint8_t *p = beg; p < end; p += asm_insn_len(p)) {
		int32_t dest;
		memcpy(&dest, p + 2, sizeof dest);
		if (!(*p == 0x0f && (p[1] & 0xf0) == XI_Jcc
				&& dest == REL32(p + 6, trampoline))) continue;
		dest = REL32(p + 6, trace->f);
		memcpy(p + 2, &dest, sizeof dest);
	}
	if (mprotect(page, end - page, PROT_EXEC)) goto err;
	__builtin___clear_cache(beg, end);
}

static struct LispTrace *assemble_trace(struct JitState *state, enum TraceLink link_type) {
	state->need_snapshot = true, take_snapshot(state);
	dce(state);
	if (link_type == TRACE_LINK_LOOP) peel_loop(state);
	state->snapshots[0].beg = IR_BIAS;

	bool is_pload_ok = false;
do_retry:
	struct RegAlloc ctx =
		{ .state = state, .available = REG_ALL,
		  .snapshot_idx = state->num_snapshots, .next_snapshot_beg = state->end };
	if (!LIKELY(asm_init(&ctx.as))) return NULL;
	// Emit side-exit trampolines
	void side_exit_handler();
	asm_write32(&ctx.as, REL32(ctx.as.p, side_exit_handler));
	*--ctx.as.p = XI_JMP;
	for (unsigned i = 0; i < MAX_SNAPSHOTS; ++i) {
		if (i) {
			*--ctx.as.p = (4 * i - 2) & 0x7f; // Chain jumps if i>=32
			*--ctx.as.p = XI_JMPs;
		}
		*--ctx.as.p = i;
		*--ctx.as.p = XI_PUSHib;
	}

	// Initialize registers as unallocated and add hints
	for (Ref ref = REF_BP; ref < state->end; ++ref) {
		union Node *x = &IR_GET(state, ref);
		x->spill_slot = 0;
		switch (x->op) {
		case IR_PLOAD: x->reg = x->a | REG_NONE; x->spill_slot = x->b; break;
		case IR_CALL:
			ctx.clobbers |= REG_ALL & ~CALLEE_SAVED_REGS;
			x->reg = rax | REG_NONE;
			break;
		case IR_PHI: --ctx.next_snapshot_beg; [[fallthrough]];
		default: x->reg = -1; break;
		}
	}
	ctx.end = ctx.as.p;
	*(ctx.as.p -= 1 + sizeof(int32_t)) = XI_JMP; // Loop backedge placeholder
	switch (link_type) {
	case TRACE_LINK_LOOP: break;
	case TRACE_LINK_INTERPR:
		void side_exit_interpr();
		int32_t jmp_dest = REL32(ctx.end, side_exit_interpr);
		memcpy(ctx.end - sizeof jmp_dest, &jmp_dest, sizeof jmp_dest);
		asm_grp1_imm(&ctx.as, 1, XG_ADD,
			rsp, state->parent->num_spill_slots * sizeof(void *));
		struct Snapshot *snapshot = state->snapshots + state->num_snapshots - 1;
		asm_loadu64(&ctx.as, rdx, state->base_offset);
		asm_loadu64(&ctx.as, rax, snapshot->pc.p);
		state->base_offset = 0;
		if (false) case TRACE_LINK_ROOT: case TRACE_LINK_UPREC:
			ctx.as.p -= /* ADD rsp, imm32 */ 7;
		asm_stack_restore(&ctx);
		break;
	default: unreachable();
	}

	for (Ref ref = state->end; ref-- > IR_BIAS;) {
		if (ref < ctx.next_snapshot_beg) {
			struct Snapshot *snapshot = state->snapshots + --ctx.snapshot_idx;
			ctx.next_snapshot_beg = snapshot->beg;
			// Start live ranges of virtuals escaping into snapshot
			FOR_SNAPSHOT_ENTRIES(snapshot, state->stack_entries, e)
				if (IS_VAR(e->ref) && !IR_GET(state, e->ref).spill_slot)
					reg_use(&ctx, e->ref, -1);
		}

		union Node x = IR_GET(state, ref);
		switch (x.op) {
		case IR_SLOAD:
			enum Register reg = reg_def(&ctx, ref, -1), bp = reg_use(&ctx, REF_BP, -1);
			asm_rmrd(&ctx.as, 1, XI_MOVrm, reg, bp, x.a * sizeof x.v);
			break;
		case IR_GLOAD:
			struct LispSymbol *sym = UNTAG_OBJ(IR_GET(state, x.a).v);
			int32_t p = GC_COMPRESS(&sym->value).p;
			reg = reg_def(&ctx, ref, -1);
			asm_rmrd(&ctx.as, 1, XI_MOVrm, reg, REG_LISP_CTX, p);
			break;
		case IR_ULOAD: {
			enum Register reg = reg_def(&ctx, ref, -1), fn = reg_use(&ctx, x.a, -1);
			asm_rmrd(&ctx.as, 1, XI_MOVrm, reg, reg, 0);
			asm_rmrd(&ctx.as, 1, XI_MOVrm, reg, reg,
				offsetof(struct Upvalue, location));
			asm_rmrd(&ctx.as, 1, XI_MOVrm, reg, fn,
				offsetof(struct Closure, upvalues[x.b]) - 1);
			break;
		}
		case IR_PLOAD:
			if (/* inherited spilled? */ x.b) { if (HAS_REG(x)) reload(&ctx, ref); }
			else reg_def(&ctx, ref, 1 << x.a);
			// Known side trace stack usage: Rectify parent spill slots and redo
			if (ref == IR_BIAS && !is_pload_ok && ctx.num_spill_slots) {
				ctx.num_spill_slots += ctx.num_spill_slots % 2; // 16-byte align stack
				union Node *y = &IR_GET(state, ref);
				do if (y->b) y->b += ctx.num_spill_slots; while ((++y)->op == IR_PLOAD);
				is_pload_ok = true;
				goto do_retry;
			}
			break;
		case IR_EQ: case IR_NEQ: case IR_LT: case IR_GE: case IR_LE: case IR_GT:
			assert(IS_VAR(x.a));
			enum Cc cc = x.op < IR_EQ ? CC_L + x.op - IR_LT : CC_E + x.op - IR_EQ;
			asm_guard(&ctx, cc_negate(cc));
			enum Register reg1 = reg_use(&ctx, x.a, -1);
			if (IS_VAR(x.b)) {
				enum Register reg2 = reg_use(&ctx, x.b, ~(1 << reg1));
				asm_rr(&ctx.as, 0, IMM_GRP1_MR(XG_CMP), reg2, reg1);
			} else asm_grp1_imm(&ctx.as, 0, XG_CMP,
				reg1, (uint32_t) IR_GET(state, x.b).v);
			break;
		case IR_CALL: asm_call(&ctx, ref); break;
		case IR_CALLARG: break;
		case IR_RET: asm_ret(&ctx, x); break;
		case IR_LOOP: asm_loop(&ctx); break;
		case IR_PHI: asm_phi(&ctx, x); break;
		case IR_NOP: break;
		default: unreachable();

		case IR_ADD: asm_arith(&ctx, XG_ADD, ref); break;
		}
	}

	FOR_ONES(reg, ctx.phi_regs) {
		union Node in = IR_GET(state, ctx.phi_refs[reg]);
		if (in.spill_slot && !(in.ty & IR_MARK)) rec_err(state); // Spilled outside loop
	}
	if (HAS_REG(IR_GET(state, REF_BP))) reload(&ctx, REF_BP);
	assert(ctx.available == REG_ALL);
	ctx.num_spill_slots += ctx.num_spill_slots % 2; // 16-byte align stack
	unsigned dsp = ctx.num_spill_slots * sizeof(LispObject);
	if (dsp) asm_grp1_imm(&ctx.as, 1, XG_SUB, rsp, dsp);

	struct LispTrace *result;
	unsigned len = state->num_consts + state->end - IR_BIAS;
	if (UNLIKELY(state->status != REC_OK)
		|| !(result = malloc(sizeof *result + len * sizeof *state->trace
				+ state->num_snapshots * sizeof *state->snapshots
				+ state->num_stack_entries * sizeof *state->stack_entries)))
		return NULL;
#ifdef DEBUG
	print_trace(state, link_type);
#endif

	uintptr_t target = (uintptr_t) ctx.as.p;
	switch (link_type) {
	case TRACE_LINK_INTERPR: break;
	case TRACE_LINK_ROOT:
		target = (uintptr_t) state->link->f;
		dsp = (ctx.num_spill_slots += state->parent->num_spill_slots) * sizeof(LispObject);
		[[fallthrough]];
	case TRACE_LINK_UPREC: // Patch tail
		struct Assembler as = { ctx.end -= dsp > INT8_MAX ? 0 : dsp ? 3 : 7, NULL };
		asm_write32(&as, REL32(as.p, target));
		*--as.p = XI_JMP;
		if (dsp) asm_grp1_imm(&as, 1, XG_ADD, rsp, dsp);
		[[fallthrough]];
	case TRACE_LINK_LOOP:
		asm_mov_mi64(&ctx.as, REG_LISP_CTX,
			offsetof(struct LispCtx, current_trace), (uintptr_t) result);
		break;
	}

	*result = (struct LispTrace) {
		.f = asm_assemble(&ctx.as),
		.len = len, .num_consts = state->num_consts,
		.num_snapshots = state->num_snapshots,
		.num_spill_slots = ctx.num_spill_slots,
		.arity = state->origin ? state->origin->prototype->arity : 0,
		.mcode_size = ctx.end - ctx.as.p,
		.mcode_tail = ctx.as.buf + MCODE_CAPACITY - ctx.as.p
	};
	char *p = result->data;
	size_t size = len * sizeof *state->trace;
	memcpy(p, state->trace + MAX_CONSTS - state->num_consts, size);
	p += size;
	memcpy(p, state->snapshots, size = state->num_snapshots * sizeof *state->snapshots);
	p += size;
	memcpy(p, state->stack_entries, state->num_stack_entries * sizeof *state->stack_entries);
	// TODO Register FDE for mcode with __register_frame()

	if (state->parent) patch_exit(state->parent, state->side_exit_num, result);
	return result;
}

static void penalize(struct JitState *state) {
	if (state->parent) return; // Handled in side_exit_handler_inner()

	struct Penalty *slot = state->penalties, *end = slot + LENGTH(state->penalties);
	do if (slot->pc == state->origin_pc) goto found; while (++slot < end);

	slot = &state->penalties[state->next_penalty_slot++ % LENGTH(state->penalties)];
	*slot = (struct Penalty) { .pc = state->origin_pc, .value = TRACE_ATTEMPTS };
found:
	if (!--slot->value)
		state->origin_pc->op += CALL_INTERPR - CALL; // Blacklist
}

static IrRef record_c_call(struct LispCtx *ctx, struct JitState *state, uintptr_t *bp, struct Instruction x) {
	IrRef ref = SLOT(state, x.a),
		a = state->bp[x.a + 2], b = state->bp[x.a + 2 + 1];
	struct LispCFunction *f = UNTAG_OBJ(bp[x.a]);
	enum SsaOp cmp_op;

	if (!strcmp(f->name, "+")) {
		guard_type(state, &a, LISP_INTEGER);
		guard_type(state, &b, LISP_INTEGER);
		return emit_opt(state, (union Node)
			{ .op = IR_ADD, .ty = LISP_INTEGER, .a = a, .b = b });
	} else if (!strcmp(f->name, "=")) {
		cmp_op = IR_EQ;
	do_record_cmp:
		LispObject value = f->f(ctx, x.c, bp + x.a + 2);
		emit_opt(state, (union Node)
			{ .op = cmp_op ^ NILP(ctx, value), .ty = TY_ANY, .a = a, .b = b });
		return emit_const(state, lisp_type(value), value);
	} else if (!strcmp(f->name, "<")) { cmp_op = IR_LT; goto do_record_cmp; }

	guard_type(state, &ref, LISP_CFUNCTION);
	take_snapshot(state); // Type guard may get added to return value
	IrRef result = emit(state, (union Node)
		{ .op = IR_CALL, .ty = TY_ANY, .a = ref, .b = x.c });
	for (unsigned i = 0; i < x.c; ++i) {
		IrRef arg = state->bp[x.a + 2 + i];
		uint8_t ty = arg >> REF_TYPE_SHIFT;
		emit(state, (union Node) { .op = IR_CALLARG, .ty = ty, .a = arg });
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
		result = emit_opt(state,
			(union Node) { .op = IR_GLOAD, .ty = TY_ANY, .a = sym });
		break;
	case GETUPVALUE: result = uref(state, bp, x.c); break;
	case MOV: result = SLOT(state, x.c); break;
	case JMP: break;
	case JNIL:
		IrRef ref = SLOT(state, x.a);
		uint8_t ty = ref >> REF_TYPE_SHIFT;
		if (ty != TY_ANY) break; // Constant NIL comparison is a no-op
		IrRef nil = emit_const(state, LISP_NIL, NIL(ctx));
		take_snapshot(state);
		emit_opt(state, (union Node)
			{ .op = IR_EQ ^ !NILP(ctx, bp[x.a]), .ty = ty, .a = ref, .b = nil });
		break;
	case CALL: case CALL_INTERPR:
		LispObject fn_value = bp[x.a];
		enum TraceLink link_type = TRACE_LINK_UPREC;
		enum Op jit_op = JIT_CALL;
		switch (lisp_type(fn_value)) {
		case LISP_CFUNCTION: result = record_c_call(ctx, state, bp, x); break;
		case LISP_CLOSURE:
			(state->bp += x.a)[1] = emit_const(state, TY_RET_ADDR, (uintptr_t) pc);
			state->base_offset += x.a;
		do_rec_call:
			struct Prototype *proto = ((struct Closure *) UNTAG_OBJ(fn_value))->prototype;
			// TODO Guard on prototype only
			guard_value(state, state->bp, fn_value); // Specialize to the function
			state->max_slot = 2 + x.c - 1;
			if (LISP_EQ(fn_value, TAG_OBJ(state->origin))) {
				struct LispTrace *trace;
				if (UNLIKELY(state->num_traces >= LENGTH(*ctx->traces))
					|| !(trace = assemble_trace(state, link_type))) break;
				uint16_t trace_num = state->num_traces++;
				(*ctx->traces)[trace_num] = trace;
				pc[-1] = (struct Instruction) { .op = jit_op, .a = x.a, .b = trace_num };
				return false;
			} else if (proto->arity & PROTO_VARIADIC) rec_err(state);
			break;
		default: break;
		}
		break;
	case TAIL_CALL: case TAIL_CALL_INTERPR:
		fn_value = bp[x.a];
		switch (lisp_type(fn_value)) {
		case LISP_CFUNCTION: state->bp[x.a] = record_c_call(ctx, state, bp, x); goto do_ret;
		case LISP_CLOSURE:
			if (/* need reload? */ fn_value != *bp) *state->bp = state->bp[x.a];
			// Move arguments down to current frame
			memmove(state->bp + 2, state->bp + x.a + 2, x.c * sizeof *state->bp);
			link_type = TRACE_LINK_LOOP;
			jit_op = TAIL_JIT_CALL;
			goto do_rec_call;
		default: break;
		}
		break;
	case JIT_CALL:
		(state->bp += x.a)[1] = emit_const(state, TY_RET_ADDR, (uintptr_t) pc);
		state->base_offset += x.a;
		x.a = 0;
		[[fallthrough]];
	case TAIL_JIT_CALL:
		if (!state->parent) { rec_err(state); break; } // Await side trace

		state->link = (*ctx->traces)[x.b];
		unsigned n = state->link->arity;
		// Move arguments down to current frame
		memmove(state->bp + 2, state->bp + x.a + 2, n * sizeof *state->bp);
		state->max_slot = 2 + n - 1;
		if (assemble_trace(state, TRACE_LINK_ROOT)) return false;
		break;
	case RET: do_ret:
		struct Instruction *npc = (struct Instruction *) bp[1];
		if (!npc) { rec_err(state); break; }
		result = state->bp[x.a];
		uint8_t offset = state->max_slot = x.a = npc[-1].a;
		if (state->base_offset) {
			assert(state->base_offset >= offset);
			state->bp -= offset;
			state->base_offset -= offset;
			break;
		}

		take_snapshot(state);
		Ref pc_ref = emit_const(state, TY_RET_ADDR, (uintptr_t) npc);
		emit(state, (union Node) { .op = IR_RET, .ty = TY_ANY, .a = pc_ref, .b = offset });
		memset(state->bp, 0, offset * sizeof *state->bp); // Clear frame below
		state->need_snapshot = true;

		for (IrRef ref = state->chain[IR_RET]; ref; ref = IR_GET(state, ref).prev) {
			union Node o = IR_GET(state, ref);
			struct Instruction *onpc = (struct Instruction *) IR_GET(state, o.a).v;
			if (npc == onpc) { rec_err(state); break; } // Down-recursion
		}
		break;
	default:
		lttng_ust_tracepoint(lisp, record_nyi, x.op);
		rec_err(state);
		break;
	}

	if (result) {
		state->max_slot = MAX(state->max_slot, x.a);
		state->bp[x.a] = result;
	}
	if (state->status != REC_OK) penalize(state);
	return state->status == REC_OK;
}

static struct SideExitResult side_exit_handler_inner(struct LispCtx *ctx, uintptr_t *regs) {
	struct LispTrace *trace = ctx->current_trace;
	uint8_t exit_num = *regs;
	union Node *insns = (union Node *) trace->data;
	struct Snapshot *snapshots = (struct Snapshot *) (insns + trace->len),
		*snapshot = snapshots + exit_num;
	struct SnapshotEntry *entries
		= (struct SnapshotEntry *) (snapshots + trace->num_snapshots);
	lttng_ust_tracepoint(lisp, side_exit, trace, exit_num);
	assert(!snapshot->trace);
	FOR_SNAPSHOT_ENTRIES(snapshot, entries, e) {
		union Node insn = insns[trace->num_consts + e->ref - IR_BIAS];
		ctx->bp[e->slot] = IS_VAR(e->ref)
			? regs[insn.spill_slot ? insn.spill_slot : -insn.reg - 1]
			: insn.v;
	}

	bool should_record = ++snapshot->hotcount >= SIDE_TRACE_THRESHOLD;
	if (should_record) {
		struct JitState *state = ctx->jit_state;
		jit_init(state);
		state->parent = trace;
		state->side_exit_num = exit_num;
		state->pc = (struct Instruction *) GC_DECOMPRESS(ctx, snapshot->pc) + 1;
		FOR_SNAPSHOT_ENTRIES(snapshot, entries, e) {
			union Node insn = insns[trace->num_consts + e->ref - IR_BIAS];
			state->slots[state->max_slot = e->slot] = IS_VAR(e->ref)
				? emit(ctx->jit_state, (union Node)
					{ .op = IR_PLOAD, .ty = e->ty, .a = insn.reg, .b = insn.spill_slot })
				: emit_const(state, e->ty, insn.v);
		}
		state->bp += (state->base_offset = snapshot->base_offset);

		if (snapshot->hotcount >= SIDE_TRACE_THRESHOLD + TRACE_ATTEMPTS) {
			should_record = false;
			assemble_trace(state, TRACE_LINK_INTERPR);
		}
	}

	return (struct SideExitResult) { snapshot->pc, {{ snapshot->base_offset,
				trace->num_spill_slots, should_record }} };
}

struct SideExitResult trace_exec(struct LispCtx *ctx, struct LispTrace *trace) {
	register struct LispCtx *ctx2 __asm__ (STR(REG_LISP_CTX)) = ctx;
	struct SideExitResult result;
	__asm__ volatile ("jmp %[f]\n\t"
		".p2align 4\n\t"
		".globl side_exit_handler\n\t"
		"side_exit_handler:\n\t"
		// Push all GP registers
		"push rax; push rcx; push rdx; push rbx; push rsp; push rbp; push rsi; push rdi\n\t"
		"mov rdi, " STR(REG_LISP_CTX) "\n\t"
		"lea rsi, [rsp+8*8]\n\t"
		"push r8; push r9; push r10; push r11; push r12; push r13; push r14; push r15\n\t"
		"call %P[inner]\n\t"
		"movzx ecx, dh\n\t"
		"lea rsp, [rsp+8*(16+1+rcx)]\n\t"
		".globl side_exit_interpr\n\t"
		"side_exit_interpr:"
		: "=a" (result.pc), "=d" (result.rdx)
		: "r" (ctx2), [f] "rm" (trace->f), [inner] "i" (side_exit_handler_inner)
		: "rcx", "rbx", "rsi", "rdi",
#if !PRESERVE_FRAME_POINTER
		"rbp",
#endif
		"r8", "r9", "r10", "r11", "r12", "r13", "r14", "cc", "memory", "redzone");
	ctx->current_trace = NULL;
	return result;
}

void trace_free(struct LispTrace *trace) {
	if (!trace) return;
	union Node *insns = (union Node *) trace->data;
	struct Snapshot *snapshots = (struct Snapshot *) (insns + trace->len);
	for (struct Snapshot *s = snapshots, *end = s + trace->num_snapshots; s < end; ++s)
		trace_free(s->trace);
	free(trace);
}
