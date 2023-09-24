#ifndef UTIL_H
#define UTIL_H

#include <stdint.h>

#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))

/// Number of elements in an array.
#define LENGTH(x) (sizeof(x) / sizeof *(x))

static inline unsigned int next_power_of_2(unsigned int x) {
	return x == 1 ? 2 : 1 << (8 * sizeof x - __builtin_clz(x - 1));
}

static inline uint64_t rotate_left(uint64_t x, uint64_t n) {
	return (x << n) | ((x >> (sizeof x - n)) & ~(-1 << n));
}

#define FX_SEED64 0x517cc1b727220a95

static inline uint64_t fxhash64(uint64_t a, uint64_t b) {
	return (rotate_left(a, 5) ^ b) * FX_SEED64;
}

static inline uint64_t moremur(uint64_t x) {
	x ^= x >> 27;
	x *= 0x3C79AC492BA7B653;
	x ^= x >> 33;
	x *= 0x1C69B3F74AC4AE35;
	x ^= x >> 27;
	return x;
}

#endif
