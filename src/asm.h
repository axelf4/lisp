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
#include <unistd.h>
#include <sys/mman.h>
#include "fxhash.h"

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

static int32_t rel32(uintptr_t p, uintptr_t target) {
	ptrdiff_t dt = target - p;
	assert((int32_t) dt == dt && "Out of range jump target");
	return dt;
}

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
#define EMIT_REX(ctx, w, rr, rx, rb) do { uint8_t _n = REX((w), (rr), (rx), (rb)); \
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
  NUM_REGS,
};

#define CALLEE_SAVED_REGS (1 << rbx | 1 << rsp | 1 << rbp \
		| 1 << r12 | 1 << r13 | 1 << r14 | 1 << r15)

enum {
	XI_XORr = 0x31,
	XI_CMP = 0x39,
	XI_MOVrr = 0x89,
	XI_MOVrm = 0x8b,
	XI_MOVri = 0xb8,
	XI_RET = 0xc3,
	XI_CALL = 0xe8,
	XI_GRP5 = 0xff,
};

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

static inline void asm_mov_reg_reg(struct Assembler *ctx, enum Register dst, enum Register src) {
	*--ctx->p = MODRM(MOD_REG, src, dst);
	*--ctx->p = XI_MOVrr;
	EMIT_REX(ctx, 1, src, 0, dst);
}

static inline void asm_loadu64(struct Assembler *ctx, enum Register r, uint64_t x) {
	if ((uint32_t) x != x) {
		asm_write64(ctx, x);
		*--ctx->p = XI_MOVri + (r & 7);
		EMIT_REX(ctx, 1, 0, 0, r);
	} else if (!x) {
		*--ctx->p = MODRM(MOD_REG, r, r);
		*--ctx->p = XI_XORr + (r & 7);
		EMIT_REX(ctx, 0, r, 0, r);
	} else {
		asm_write32(ctx, x);
		*--ctx->p = XI_MOVri + (r & 7);
		EMIT_REX(ctx, 0, 0, 0, r);
	}
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

/** Condition code for Conditional Test fields. */
enum Cc : unsigned char {
	CC_E = 0x4, ///< Equal, Zero.
	CC_NE, ///< Not equal, Not zero.
};

static inline enum Cc cc_negate(enum Cc x) { return x ^ 1; }

static inline void asm_ret(struct Assembler *ctx) { *--ctx->p = XI_RET; }

static inline void asm_call(struct Assembler *ctx, uintptr_t target) {
	asm_write32(ctx, rel32((uintptr_t) ctx->p, target));
	*--ctx->p = XI_CALL;
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
					MAP_PRIVATE | MAP_ANONYMOUS, -1, 0)) == MAP_FAILED) return false;
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
	long page_size = sysconf(_SC_PAGESIZE);
	uint8_t *beg = (uint8_t *) ((uintptr_t) ctx->p & ~(page_size - 1)),
		*end = ctx->buf + MCODE_CAPACITY;
	if (mprotect(beg, end - beg, PROT_EXEC)) die("mprotect failed");
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
	return (void (*)()) ctx->p;
#pragma GCC diagnostic pop
}

#endif
