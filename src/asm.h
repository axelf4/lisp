/** Machine code assembler. @file */

#ifndef ASM_H
#define ASM_H

#include <stddef.h>
#include <stdint.h>
#include <sys/mman.h>
#include "util.h"

#define MCODE_CAPACITY 0x10000

struct Assembler {
	uint8_t *buf, *p, *end;
};

#ifdef __x86_64__
#include "asm_x86.h"
#else
#error Unknown architecture
#endif

[[gnu::used]] static void asm_foo() {
	puts("foo");
}

/** Reserves space for machine code in the range of relative jumps of our code. */
static inline bool asm_init(struct Assembler *ctx) {
	uint8_t *p;
	size_t length = MCODE_CAPACITY;
	uintptr_t target = (uintptr_t) asm_foo & ~0xffff,
		range = (1u << (JMP_RANGE - 1)) - (1u << 21),
		hint = 0;
	for (unsigned i = 0; i < JMP_RANGE; ++i) {
		if ((p = mmap((void *) hint, length, PROT_READ | PROT_WRITE,
					MAP_PRIVATE | MAP_ANONYMOUS, -1, 0)) == MAP_FAILED) return false;
		if ((uintptr_t) p - target < range || target - (uintptr_t) p < range + length) {
			*ctx = (struct Assembler) { .buf = p, .p = p, .end = p + length };
			return true;
		}
		munmap(p, length);
		// Probe random 64K-aligned addresses
		hint = (fxhash64(hint, (uintptr_t) p) & ((1u << JMP_RANGE) - 0x10000))
			+ target - range;
	}
	return false;
}

static inline void asm_free(struct Assembler *ctx) {
	munmap(ctx->buf, MCODE_CAPACITY);
}

static inline int (*asm_assemble(struct Assembler *ctx))() {
	if (mprotect(ctx->buf, ctx->p - ctx->buf, PROT_EXEC)) die("mprotect failed");

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
	return (int (*)()) ctx->buf;
#pragma GCC diagnostic pop
}

#endif
