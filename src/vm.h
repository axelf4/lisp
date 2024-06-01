#ifndef VM_H
#define VM_H

#include <stdint.h>
#include "lisp.h"

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
	alignas(GC_MIN_ALIGNMENT) struct LispObjectHeader hdr;
	uint16_t num_consts;
	size_t count;
	/// Array of #num_consts constants, followed by #count instructions.
	alignas(LispObject) alignas(struct Instruction) char data[];
};

/** Lisp closure prototype. */
struct Prototype {
	uint8_t arity, num_upvalues;
	unsigned offset; ///< Byte offset within chunk.
	struct Instruction body[];
};

struct Upvalue {
	alignas(GC_MIN_ALIGNMENT) struct LispObjectHeader hdr;
	bool is_closed;
	union {
		struct Upvalue *next; ///< Next unclosed in list sorted by stack locations.
		LispObject value; ///< The closed over object.
	};
	LispObject *location;
};

struct Closure {
	alignas(GC_MIN_ALIGNMENT) struct LispObjectHeader hdr;
	struct Prototype *prototype;
	struct Upvalue *upvalues[];
	// TODO Add plist as an alternative to https://zenodo.org/records/6228797
};

static inline LispObject *chunk_constants(struct Chunk *chunk) {
	return (LispObject *) chunk->data;
}
static inline struct Instruction *chunk_instructions(struct Chunk *chunk) {
	return (struct Instruction *) (chunk_constants(chunk) + chunk->num_consts);
}

#endif
