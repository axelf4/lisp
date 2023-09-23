#ifndef UTIL_H
#define UTIL_H

#include <stdint.h>

#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))

#define FOR_SET_BITS(var, x) for (unsigned __i = x, var;			\
		__i && (var = __builtin_ctz(__i), 1); __i &= (__i - 1))

static inline unsigned int next_power_of_2(unsigned int x) {
	return x == 1 ? 2 : 1 << (8 * sizeof x - __builtin_clz(x - 1));
}

static inline uint64_t rotate_left(uint64_t x, uint64_t n) {
	return (x << n) | ((x >> (sizeof x - n)) & ~(-1 << n));
}

#endif
