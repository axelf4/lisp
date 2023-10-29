/** Miscellaneous utilities. */

#ifndef UTIL_H
#define UTIL_H

#include <stdint.h>
#include <limits.h>

#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))

/// Number of elements in the array.
#define LENGTH(x) (sizeof (x) / sizeof *(x))

#define _CAT(a, b) a ## b
#define CAT(a, b) _CAT(a, b)

#define SWAP(x, y) do { auto _tmp = (x); (x) = (y); (y) = _tmp; } while(0)

/** Returns the smallest power of two greater than or equal to @arg x. */
static inline unsigned int next_power_of_2(unsigned int x) {
	return x & (x - 1) ? 1U << (CHAR_BIT * sizeof x - __builtin_clz(x)) : x;
}

/** Converts @arg x to little endian from the target's endianness. */
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define HTOL(x) x
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#define HTOL(x) _Generic((x),					\
		uint32_t: __builtin_bswap32,			\
		uint64_t: __builtin_bswap64)(x)
#else
#error Unknown byte-order
#endif

/** Terminates the program with the specified error message. */
[[noreturn, gnu::cold, gnu::format (printf, 1, 2)]] void die(const char *format, ...);

static inline uint64_t rotate_left(uint64_t x, unsigned n) {
	unsigned mask = CHAR_BIT * sizeof n - 1;
	n &= mask;
	return (x << n) | (x >> (-n & mask));
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

/** Throws an exception with @arg errcode. */
[[noreturn, gnu::cold]] void throw(unsigned errcode);

/**
 * Protected call.
 *
 * Applies @arg f to @arg x, preventing stack unwinding past this
 * function in case an exception is thrown.
 *
 * @return The error code, or zero in the absence of errors.
 */
unsigned pcall(void *x, void (*f)(void *));

#endif
