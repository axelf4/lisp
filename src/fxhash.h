/** Fast non-cryptographic hashing algorithm used by rustc. @file
 *
 * The algorithm was designed by Orson Peters.
 *
 * @see https://github.com/rust-lang/rustc-hash
 */

#ifndef FXHASH_H
#define FXHASH_H

#include <stdint.h>
#include <limits.h>
#include <string.h>

/** Rotates 64 bits @a x left @a n times. */
static inline uint64_t rol64(uint64_t x, unsigned n) {
	unsigned mask = CHAR_BIT * sizeof x - 1;
	return x << (n & mask) | x >> (-n & mask);
}

/** FxHash multiplier.
 *
 * The polynomial hash
 *
 *     m[0]*k    + m[1]*k^2  + m[2]*k^3  + ...
 *
 * can be seen as a multilinear hash with keystream k[..]
 *
 *     m[0]*k[0] + m[1]*k[1] + m[2]*k[2] + ...
 *
 * where k was generated using a multiplicative congruential random
 * number generator (MCG). Hence, a known-good MCG constant is used.
 *
 * @see STEELE JR, Guy L.; VIGNA, Sebastiano. Computationally easy,
 *      spectrally good multipliers for congruential pseudorandom
 *      number generators. Software: Practice and Experience, 2022,
 *      52.2: 443-458.
 */
#define FXHASH_K 0xf1357aea2e62a9c5

/** Appends @a x to the FxHash @a s. */
static inline uint64_t fxhash(uint64_t s, uint64_t x) {
	return (s + x) * FXHASH_K;
}

static inline uint64_t fxhash_finish(uint64_t s) {
	// Due to multiplication the top bits have the most entropy, but
	// hash table implementation computes bucket index from the bottom
	// bits: Move bits from top to bottom.
	return rol64(s, 20);
}

static inline uint64_t fxhash_mul_mix(uint64_t x, uint64_t y) {
	auto full = (unsigned _BitInt(128)) x * y;
	// The middle bits (top bits of low, bottom bits of high half)
	// fluctuate the most on small input changes: XOR the halves.
	return full ^ full >> 64;
}

static inline uint32_t fxhash_load32(const char p[static 4]) {
	uint32_t x;
	memcpy(&x, p, sizeof x);
	return x;
}
static inline uint64_t fxhash_load64(const char p[static 8]) {
	uint64_t x;
	memcpy(&x, p, sizeof x);
	return x;
}

/** wyhash-inspired non-collision-resistant hash for strings. */
static inline uint64_t fxhash_str(size_t n, const char p[static n]) {
	uint64_t s0 = 0x243f6a8885a308d3, s1 = 0x13198a2e03707344, // Digits of pi
		prevent_trivial_zero_collapse = 0xa4093822299f31d0;
	if (n > 16) { // Handle bulk
		for (size_t off = 0; off < n - 16; off += 16) {
			// Replace s1 with mix of s0, x and y, and s0 with s1,
			// ensuring loop is unrollable to 2 independent streams.
			uint64_t x = fxhash_load64(p + off), y = fxhash_load64(p + off + 8),
				t = fxhash_mul_mix(s0 ^ x, prevent_trivial_zero_collapse ^ y);
			s0 = s1;
			s1 = t;
		}
		p += n - 16;
		goto do_16;
	} else if (n >= 8) { do_16: s0 ^= fxhash_load64(p); s1 ^= fxhash_load64(p + n - 8); }
	else if (n >= 4) { s0 ^= fxhash_load32(p); s1 ^= fxhash_load32(p + n - 4); }
	else if (n) {
		uint64_t lo = p[0], mid = p[n / 2], hi = p[n - 1];
		s0 ^= lo;
		s1 ^= hi << 8 | mid;
	}
	// Subsequent multiplication and taking high bits will avalanche for us
	return fxhash_mul_mix(s0, s1) ^ n;
}

#endif
