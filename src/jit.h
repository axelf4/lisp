/** Tracing just-in-time compiler. */

#ifndef JIT_H
#define JIT_H

#include "lisp.h"

/** Superset of @ref LispObjectType. */
typedef uint8_t IrType;

enum {
	TY_ANY = LISP_INTEGER + 1,
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

union SsaInstruction {
	struct {
		enum SsaOp op;
		IrType ty; ///< The type of the instruction result.
		uint16_t a, ///< Operand 1.
			b; ///< Operand 2.
		union {
			uint16_t prev;
			struct {
				uint8_t reg, ///< The allocated register.
					spill_slot;
			};
		};
	};
	LispObject v; ///< Object constant.
};

typedef uint32_t IrRef;

struct SnapshotEntry {
	struct { // Stack entry
		IrRef ref : 24; ///< The content to write.
		uint8_t slot; ///< The stack slot to restore.
	};
};

/** List of stack slots, frame widths and PC to restore on exit.
 *
 * When taking an exit, the computations performed hitherto need to be
 * transferred to the stack for the interpreter to continue from where
 * the compiled trace left off.
 */
struct Snapshot {
	/// Number of instructions executed before this snapshot was taken.
	uint16_t ir_start,
		offset; ///< Offset into @ref JitState::snap_entries.
	uint8_t num_stack_entries;
};

/** Type of trace link. */
enum TraceLink {
	/* TRACE_LINK_NONE, ///< No link yet, the trace is incomplete. */
	/**
	 * Abort the trace recording, e.g. due to NYI (not yet
	 * implemented) or return from the initial function.
	 */
	TRACE_LINK_ABORT,
	TRACE_LINK_LOOP, ///< Loop to itself.
};

struct JitState;

[[gnu::malloc]] struct JitState *record_new(struct Closure *f);

/** Records instruction preceding @a pc prior to it being executed. */
bool record_instruction(struct JitState *state, uintptr_t *bp, struct Instruction *pc);

#endif
