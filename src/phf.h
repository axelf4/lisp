/** @file
 * Perfect hash function (PHF) builder.
 *
 * A (minimal) PHF maps some set of *n* keys to *1,...,n* without
 * collisions. Improving upon FCH, PTHash is an algorithm for finding
 * such functions.
 *
 * FCH partitions keys into non-uniform buckets. For each bucket in
 * order of non-increasing size, a displacement *d* and extra bit *b*
 * are determined such that no key in the bucket collides with
 * previously occupied positions when mapped to:
 *
 *     position(x, d, b) = (h(x, s + b) + d) mod n
 *
 * PTHash aims to make the bucket parameter compressible, to allow
 * more buckets for the same space budget. Keys are instead mapped to:
 *
 *     position(x, p) = (h(x, s) ^ h(p, s)) mod n
 *
 * where *p* is the bucket *pilot*. As pilots are hashed, they may be
 * tried starting from zero instead of at random (every increment has
 * a 50% chance of flipping each key hash bit anyway). Thus they tend
 * to be small.
 *
 * Similar to [PTRHash](https://curiouscoding.nl/posts/ptrhash-paper/)
 * cuckoo hashing is used if no 8-bit pilot is found.
 *
 * @see PIBIRI, Giulio Ermanno; TRANI, Roberto. PTHash: Revisiting FCH
 *      minimal perfect hashing. In: Proceedings of the 44th
 *      international ACM SIGIR conference on research and development
 *      in information retrieval. 2021. p. 1339-1348.
 */

#ifndef PHF_H
#define PHF_H

#include "fxhash.h"
#include "util.h"

/** Maps @a x to the range [0,@a p) fairly. */
static inline uint64_t fastrange64(uint64_t x, uint64_t p) {
	return (unsigned _BitInt(128)) x * p >> CHAR_BIT * sizeof x;
}

/** Maps @a hash to a PTHash bucket in the range [0..@a m) monotonically.
 *
 * @param hash The key hash to reduce.
 * @param m Total number of buckets.
 * @return The bucket index.
 */
static inline uint64_t pthash_bucket(uint64_t hash, uint64_t m) {
	uint64_t t = 2. / 3 /* =: a */ * (double) UINT64_MAX;
	// p1 keys are mapped to the first p2 buckets, where p1 := an and p2 := m/3
	bool is_dense = /* Approx. hash mod n < an */ hash < t;
	// a is chosen such that a/2 + 2(1 - a) = 1
	return fastrange64(is_dense ? hash / 2 : t / 2 + 2 * (hash - t), m);
}

/** Returns the PTHash position for @a hash.
 *
 * @param hash A key hash.
 * @param k The bucket pilot.
 * @param n The codomain size (preferably not a power-of-2).
 */
static inline size_t pthash_position(uint64_t hash, unsigned char k, size_t n) {
	uint64_t pilot_hash = fxhash(0, k);
	return (hash ^ pilot_hash) % n;
}

struct Phf {
	size_t n, n_prime, m;
	unsigned char *pilots; ///< The bucket pilot table.
	size_t *remap; ///< #n_prime-#n indices of free positions.
};

/** Looks up the index for @a hash given the perfect hash function @a f. */
static inline size_t phf(struct Phf *f, uint64_t hash) {
	uint64_t bucket = pthash_bucket(hash, f->m),
		pos = pthash_position(hash, f->pilots[bucket], f->n_prime);
	return LIKELY(pos < f->n) ? pos : f->remap[pos - f->n];
}

enum PhfError {
	PHF_OK, ///< PHF was constructed successfully.
	PHF_RESEED, ///< Reseed the hash function and try again.
	PHF_NO_MEMORY, ///< @c malloc failed.
};

struct PhfParameters {
	float c, ///< Bucket count multiplier.
		alpha; ///< Load factor.
};

/** Constructs a perfect hash function for @a keys.
 *
 * If successful, @a result will need to be freed.
 *
 * @param params PTHash parameters.
 * @param n The number of keys.
 * @param keys Key hashes.
 * @param[out] result Is set to the built PHF upon success.
 * @return Whether successful.
 */
enum PhfError phf_build(const struct PhfParameters *params,
	size_t n, uint64_t keys[static n], struct Phf *result);

void phf_free(struct Phf *);

#endif
