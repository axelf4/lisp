/** Miscellaneous utilities. @file */

#ifndef UTIL_H
#define UTIL_H

#include <stdbit.h>
#include <stdint.h>
#include <limits.h>

#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))

/** Number of elements in the array. */
#define LENGTH(x) (sizeof (x) / sizeof *(x))

#define _CAT(a, b) a ## b
#define CAT(a, b) _CAT(a, b)

#if defined __has_builtin && __has_builtin(__builtin_expect)
#define LIKELY(x) __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
#define LIKELY(x) (x)
#define UNLIKELY(x) (x)
#endif

/** Converts @a x to little endian from the target's endianness. */
#if __STDC_ENDIAN_NATIVE__ == __STDC_ENDIAN_LITTLE__
#define HTOL(x) (x)
#elif __STDC_ENDIAN_NATIVE__ == __STDC_ENDIAN_BIG__
#define HTOL(x) _Generic((x),					\
		uint16_t: __builtin_bswap16,			\
		uint32_t: __builtin_bswap32,			\
		uint64_t: __builtin_bswap64)(x)
#else
#error Unknown byte-order
#endif

/** Arithmetic right shift of @a x by @a y bits. */
#define SAR(x, y) ((x) < 0 ? ~(~(x) >> (y)) : (x) >> (y))

/** Returns true iff @a x == 2^k for some k. */
#define IS_POWER_OF_TWO(x) ((x) && !((x) & ((x) - 1)))

/** Iterates over the indices of set bits in @a x. */
#define FOR_ONES(var, x) for (typeof(x) _i = (x), var; \
		_i && (var = stdc_trailing_zeros(_i), true); _i &= _i - 1)

/** Terminates the program with the specified error message. */
[[noreturn, gnu::cold, gnu::format (printf, 1, 2)]] void die(const char *format, ...);

static inline uint64_t rotate_left(uint64_t x, unsigned n) {
	unsigned mask = CHAR_BIT * sizeof x - 1;
	return x << (n & mask) | x >> (-n & mask);
}

#define FX_SEED64 0x517cc1b727220a95 ///< FxHash mixing constant.

static inline uint64_t fxhash64(uint64_t a, uint64_t b) {
	return (rotate_left(a, 5) ^ b) * FX_SEED64;
}

static inline uint64_t moremur(uint64_t x) {
	x ^= x >> 27;
	x *= 0x3c79ac492ba7b653;
	x ^= x >> 33;
	x *= 0x1c69b3f74ac4ae35;
	x ^= x >> 27;
	return x;
}

/** Throws an exception with @a errcode. */
[[noreturn, gnu::cold]] void throw(unsigned errcode);

/** Protected call.
 *
 * Applies @a f to @a x, preventing stack unwinding past this function
 * in case an exception is thrown.
 *
 * @return The error code, or zero in the absence of exceptions.
 */
unsigned pcall(void *x, void (*f)(void *));

#endif
