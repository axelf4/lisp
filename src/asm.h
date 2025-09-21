/** Machine code assembler. @file
 *
 * @see Intel 64 and IA-32 Architectures Software Developer's Manual
 */

#ifndef ASM_H
#define ASM_H

#include <stddef.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>
#include <sys/mman.h>
#include "fxhash.h"
#include "util.h"

#define MCODE_CAPACITY 0x10000

struct Assembler {
	uint8_t *p, *buf;
};

static void asm_write(struct Assembler *ctx, size_t n, unsigned char xs[static n]) {
	memcpy(ctx->p -= n, xs, n);
}

static inline void asm_write32(struct Assembler *ctx, uint32_t x) {
	asm_write(ctx, sizeof x, (unsigned char *) &x);
}

static inline void asm_write64(struct Assembler *ctx, uint64_t x) {
	asm_write(ctx, sizeof x, (unsigned char *) &x);
}

static inline int32_t rel32(uintptr_t p, uintptr_t target) {
	ptrdiff_t dt = target - p;
	assert((int32_t) dt == dt && "Jump target is out of range");
	return dt;
}
#define REL32(p, target) rel32((uintptr_t) (p), (uintptr_t) (target))

#ifdef __x86_64__
#define JMP_RANGE 31 // +-2^31

#define MODRM(mod, reg, rm) ((mod) << 6 | ((reg) & 7) << 3 | (rm & 7))
#define SIB MODRM
/** Formats a REX prefix.
 *
 * @param w The W bit, indicating a 64-bit operand size.
 * @param rr The MODRM.reg field.
 * @param rx The SIB.index field.
 * @param rb The MODRM.rm field.
 */
#define REX(w, rr, rx, rb) \
	(0x40 | (w) << 3 | ((rr) >> 1 & 0b0100) | ((rx) >> 2 & 0b0010) | (rb) >> 3)
#define EMIT_REX(ctx, w, rr, rx, rb) do { uint8_t _n = REX(w, rr, rx, rb); \
		if (_n != 0x40) *--(ctx)->p = _n; } while (0)

/** Register addressing mode. */
enum Mod {
    MOD_DISP0, ///< [%reg]
    MOD_DISP8, ///< [%reg+disp8]
	MOD_DISP32, ///< [%reg+disp32] (or [%rip+disp32])
	MOD_REG, ///< Direct addressing (%reg).
};

enum Register : uint8_t {
  rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi,
  r8, r9, r10, r11, r12, r13, r14, r15,
  NUM_REGS
};

#define CALLEE_SAVED_REGS (1 << rbx | 1 << rsp | 1 << rbp \
		| 1 << r12 | 1 << r13 | 1 << r14 | 1 << r15)

enum {
	XI_XORr = 0x31,
	XI_PUSHib = 0x6a,
	XI_MOVmr = 0x89,
	XI_MOVrm = 0x8b,
	XI_MOVri = 0xb8,
	XI_MOVmi = 0xc7,
	XI_LEA = 0x8d,
	XI_RET = 0xc3,
	XI_JMP = 0xe9,
	XI_JMPs = 0xeb,
	XI_Jcc = 0x80,
	XI_GRP5 = 0xff,
};

/** Emits @a op with operands @a reg and @a rm. */
static inline void asm_rr(struct Assembler *ctx, bool w, uint8_t op,
	enum Register reg, enum Register rm) {
	*--ctx->p = MODRM(MOD_REG, reg, rm);
	*--ctx->p = op;
	EMIT_REX(ctx, w, reg, 0, rm);
}
#define asm_mov(ctx, dst, src) asm_rr(ctx, 1, XI_MOVmr, src, dst)

/** Emits @a op with operands @a reg and `[%base+disp]`.
 *
 * @a op determines which is Operand1 and Operand2 respectively.
 */
static inline void asm_rmrd(struct Assembler *ctx, bool w, uint8_t op,
	enum Register reg, enum Register base, int32_t disp) {
	enum Mod mod;
    if (!disp && (base & 0b111) != rbp) mod = MOD_DISP0;
    else if ((int8_t) disp == disp) { mod = MOD_DISP8; *--ctx->p = disp; }
	else { mod = MOD_DISP32; asm_write32(ctx, disp); }
	if ((base & 0b111) == rsp) *--ctx->p = SIB(/* scale 1 */ 0, rsp, rsp);
	*--ctx->p = MODRM(mod, reg, base);
	*--ctx->p = op;
	EMIT_REX(ctx, w, reg, 0, base);
}

static inline void asm_loadu64(struct Assembler *ctx, enum Register r, uint64_t x) {
	if ((uint32_t) x != x) {
		asm_write64(ctx, x);
		*--ctx->p = XI_MOVri + (r & 7);
		EMIT_REX(ctx, 1, 0, 0, r);
	} else if (!x) asm_rr(ctx, 0, XI_XORr + (r & 7), r, r);
	else {
		asm_write32(ctx, x);
		*--ctx->p = XI_MOVri + (r & 7);
		EMIT_REX(ctx, 0, 0, 0, r);
	}
}

static inline void asm_mov_mi64(struct Assembler *ctx,
	enum Register base, int32_t disp, uint64_t i) {
	asm_write32(ctx, i >> 32);
	asm_rmrd(ctx, 0, XI_MOVmi, 0, base, disp + sizeof(int32_t));
	asm_write32(ctx, i);
	asm_rmrd(ctx, 0, XI_MOVmi, 0, base, disp);
}

enum ImmGrp1 { XG_ADD, XG_SUB = 5, XG_CMP = 7 };

#define IMM_GRP1_MR(op) (8 * (op) + 1)

/** Emits an Immediate Group 1 instruction with a register as 2nd operand. */
static inline void asm_grp1_imm(struct Assembler *ctx, bool w, enum ImmGrp1 reg,
	enum Register rm, int32_t i) {
	uint8_t op;
	if ((int8_t) i == i) { *--ctx->p = i; op = 0x83; }
	else { asm_write32(ctx, i); op = 0x81; }
	asm_rr(ctx, w, op, (uint8_t) reg, rm);
}

/** Condition code for Conditional Test fields. */
enum Cc {
	CC_O, ///< Overflow.
	CC_E = 0x4, ///< Equal, Zero.
	CC_NE, ///< Not equal, Not zero.
};

static inline enum Cc cc_negate(enum Cc x) { return x ^ 1; }

static inline void asm_ret(struct Assembler *ctx) { *--ctx->p = XI_RET; }

/** x86(-64) instruction length disassembler. */
static inline unsigned asm_insn_len(const uint8_t p[static 1]) {
	const unsigned char lut1[256] = {
		[0x0f] = 0x20,
		[IMM_GRP1_MR(XG_ADD)] = 0x02, [IMM_GRP1_MR(XG_SUB)] = 0x02, [IMM_GRP1_MR(XG_CMP)] = 0x02,
		[XI_XORr] = 0x52, [XI_PUSHib] = 0x02,
		// REX
		[0x40] = 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40,
		[/* REX.W */ 0x48] = 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44,
		[0x66] = 0x42, [0x67] = 0x41,
		[0x81] = 0x06, [0x83] = 0x03,

		[XI_MOVmr] = 0x52, [XI_MOVrm] = 0x52, [XI_LEA] = 0x52,
		[XI_MOVri] = 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05, 0x05,
		[XI_MOVmi] = 0x56,

		[XI_RET] = 0x01, [XI_JMP] = 0x05, [XI_JMPs] = 0x02,
		[XI_GRP5] = 0x52,
	}, lut2[256] = {
		[0x38] = 0x30, [0x3a] = 0x30,
		[XI_Jcc] = 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06,
		0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06,
	};

	unsigned char prefixes = 0, acc = 0;
do_prefix: unsigned char x = lut1[*p];
do_loop:
	switch (x >> 4) {
	case 0: return acc + x + (prefixes & 4);
	case 2: x = lut2[*++p]; goto do_loop; // >=2-byte opcode
	case 3: ++p; goto do_modrm; // 3-byte opcode
	case 4: prefixes |= x; ++acc; ++p; goto do_prefix; // Legacy/REX prefix
	case 5: do_modrm: // ModR/M
		acc += x & 0xf;
		uint8_t modrm = *++p, mod = modrm >> 6, rm = modrm & 7;
		switch (mod) {
		case MOD_DISP0: break;
		case MOD_DISP8: ++acc; break;
		case MOD_DISP32: acc += 4; break;
		case MOD_REG: return acc;
		}
		if (rm == rsp) { rm = /* SIB */ p[1] & 7; ++acc; }
		if (mod == MOD_DISP0 && rm == 5) acc += 4;
		return acc;
	default: unreachable();
	}
}
#else
#error Unknown architecture
#endif

/** Reserves space for machine code. */
static inline bool asm_init(struct Assembler *ctx) {
	uint8_t *p;
	size_t length = MCODE_CAPACITY;
	// Force within range of relative jumps/CALLs to our code
	uintptr_t target = (uintptr_t) asm_init & ~0xffff,
		range = (1u << (JMP_RANGE - 1)) - (1u << 21),
		hint = 0, state = FXHASH_K;
	for (unsigned i = 0; i < JMP_RANGE; ++i) {
		if ((p = mmap((void *) hint, length, PROT_READ | PROT_WRITE,
					MAP_PRIVATE | MAP_ANONYMOUS, -1, 0)) == MAP_FAILED) break;
		if ((uintptr_t) p - target < range || target - (uintptr_t) p < range + length) {
			*ctx = (struct Assembler) { .buf = p, .p = p + length };
			return true;
		}
		munmap(p, length);
		// Probe random 64K-aligned addresses
		hint = ((state = fxhash(state, hint)) & (2 * range - 0x10000)) + target - range;
	}
	return false;
}

static inline void asm_free(struct Assembler *ctx) {
	munmap(ctx->buf, MCODE_CAPACITY);
}

static inline void (*asm_assemble(struct Assembler *ctx))() {
	uint8_t *beg = (uint8_t *) ((uintptr_t) ctx->p & ~(page_size() - 1)),
		*end = ctx->buf + MCODE_CAPACITY;
	if (mprotect(beg, end - beg, PROT_EXEC)) die("mprotect failed");
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
	return (void (*)()) ctx->p;
#pragma GCC diagnostic pop
}

#endif
