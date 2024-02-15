/** x86/-64 assembler.
 *
 * @see Intel 64 and IA-32 Architectures Software Developer's Manual
 */

#include "asm.h"
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include <stdio.h>

#define JMP_RANGE 31 // +-2^31

#define MODRM(mod, reg, rm) ((mod) << 6 | ((reg) & 7) << 3 | (rm & 7))
#define MOD_REG 3 ///< Register addressing mode

/** Formats a REX prefix.
 *
 * @param w The W bit, indicating a 64-bit operand size.
 * @param rr The MODRM.reg field.
 * @param rx The SIB.index field.
 * @param rb The MODRM.rm field.
 */
#define REX(w, rr, rx, rb) \
	(0x40 | w << 3 | ((rr) >> 1 & 0b0100) | ((rx) >> 2 & 0b0010) | (rb) >> 3)
#define REX_W 0x48
#define EMIT_REX(ctx, w, rr, rx, rb) do { uint8_t _n = REX(w, rr, rx, rb); \
		if (_n != 0x40) *(ctx)->p++ = _n; } while (0)

static void asm_write(struct Assembler *ctx, size_t n, uint8_t xs[static n]) {
	memcpy(ctx->p, xs, n);
	ctx->p += n;
}

static inline void asm_write32(struct Assembler *ctx, uint32_t x) {
	asm_write(ctx, sizeof x, (uint8_t *) &x);
}

static inline void asm_write64(struct Assembler *ctx, uint64_t x) {
	asm_write(ctx, sizeof x, (uint8_t *) &x);
}

enum Register {
  rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi,
  r8, r9, r10, r11, r12, r13, r14, r15
};

enum {
	XI_XORr = 0x31,
	XI_ARITHib = 0x80,
	XI_ARITHi8 = 0x83,
	XI_RET = 0xc3,
	XI_CALL = 0xe8,
	XI_MOVrr = 0x89,
	XI_MOVri = 0xb8,
};

static int32_t rel32(uint8_t *p, uint8_t *target) {
	ptrdiff_t dt = target - p;
	assert((int32_t) dt == dt && "out of range jump target");
	return dt;
}

static inline void asm_mov_reg_reg(struct Assembler *ctx, enum Register dst, enum Register src) {
	EMIT_REX(ctx, 1, src, 0, dst);
	*ctx->p++ = XI_MOVrr;
	*ctx->p++ = MODRM(MOD_REG, src, dst);
}

static inline void asm_loadu64(struct Assembler *ctx, enum Register r, uint64_t x) {
	if ((uint32_t) x == x) {
		if (!x) {
			EMIT_REX(ctx, 0, r, 0, r);
			*ctx->p++ = XI_XORr + (r & 7);
			*ctx->p++ = MODRM(MOD_REG, r, r);
			return;
		}

		EMIT_REX(ctx, 0, 0, 0, r);
		*ctx->p++ = XI_MOVri + (r & 7);
		asm_write32(ctx, x);
	} else {
		EMIT_REX(ctx, 1, 0, 0, r);
		*ctx->p++ = XI_MOVri + (r & 7);
		asm_write64(ctx, x);
	}
}

static inline void asm_ret(struct Assembler *ctx) { *ctx->p++ = XI_RET; }

static inline void asm_call(struct Assembler *ctx, uint8_t *target) {
	*ctx->p++ = XI_CALL;
	asm_write32(ctx, rel32(ctx->p + 4, target));
}
