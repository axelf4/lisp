/** Miscellaneous utilities. @file */

#ifndef UTIL_H
#define UTIL_H

#include <stdbit.h>
#ifndef LG_PAGE
#include <unistd.h>
#endif

#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))

/** Number of elements in the array. */
#define LENGTH(x) (sizeof (x) / sizeof *(x))

#define _STR(x) #x
#define STR(x) _STR(x)
#define _CAT(a, b) a ## b
#define CAT(a, b) _CAT(a, b)

#if defined __has_builtin && __has_builtin(__builtin_expect)
#define LIKELY(x) __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
#define LIKELY(x) (x)
#define UNLIKELY(x) (x)
#endif

#if defined __has_builtin && __has_builtin(__builtin_assume_aligned)
#define ASSUME_ALIGNED(x, align) __builtin_assume_aligned(x, align)
#else
#define ASSUME_ALIGNED(x, align) ((void *) (x))
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
/** The smallest multiple of the power of 2 @a a greater than or equal to @a x. */
#define ALIGN_UP(x, a) (((uintptr_t) (x) + (a) - 1) & ~((a) - 1))

/** Iterates over the indices of set bits in @a x. */
#define FOR_ONES(var, x) for (typeof(x) _i = (x), var; \
		_i && (var = stdc_trailing_zeros(_i), true); _i &= _i - 1)

/** Terminates the program with the specified error message. */
[[noreturn, gnu::cold, gnu::format (printf, 1, 2)]] void die(const char *format, ...);

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

void checkpoint();

[[noreturn]] void restore();

/** Gets the page size in bytes. */
static inline unsigned long page_size() {
#ifdef LG_PAGE
	return 1 << LG_PAGE;
#else
	return sysconf(_SC_PAGESIZE);
#endif
}

#endif
