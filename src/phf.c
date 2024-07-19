#include "phf.h"
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#define BITSET_GET(x, i) ((x)[(i) / (CHAR_BIT * sizeof *(x))] \
		& (typeof(*(x))) 1 << (i) % (CHAR_BIT * sizeof *(x)))
#define MAX_TRIALS 4096

static int key_cmp(const void *x, const void *y) {
	uint64_t a = *(uint64_t *) x, b = *(uint64_t *) y;
	return (a > b) - (a < b);
}
static int bucket_cmp(const void *x, const void *y) {
	size_t a = *(size_t *) x, b = *(size_t *) y;
	return (a < b) - (a > b);
}

/** Packs the @a width bit wide integers given in @a xs. */
static unsigned long *compress_vec(size_t m, uint16_t xs[static m], unsigned char width) {
	unsigned long *result, *p, acc = 0;
	if (!(p = result = calloc((width * m + CHAR_BIT * sizeof *result - 1)
				/ (CHAR_BIT * sizeof *result), sizeof *result))) return NULL;
	unsigned j = 0;
	for (size_t i = 0; i < m; ++i) {
		unsigned n = MIN(CHAR_BIT * sizeof *p - j, width);
		acc |= (unsigned long) xs[i] << j;
		if ((j += n) == CHAR_BIT * sizeof *p) {
			*p++ = acc;
			acc = xs[i] >> n;
			j = width - n;
		}
	}
	if (j) *p = acc;
	return result;
}

enum PhfError phf_build(const struct PhfParameters *params, size_t n, uint64_t keys[static n], struct Phf *result) {
	assert(n);
	// Sort by hashes as fastrange64 makes pthash_bucket() monotonic
	qsort(keys, n, sizeof *keys, key_cmp);
	// Check need to reseed due to hash collision
	for (size_t i = 1; i < n; ++i) if (keys[i - 1] == keys[i]) return PHF_RESEED;

	size_t n_prime = n / params->alpha;
	if (IS_POWER_OF_TWO(n_prime)) ++n_prime; // Use all bits of entropy
	// Map keys into m buckets
	uint64_t m = params->c * n / stdc_bit_width(n) + 0.5; // Total number of buckets
	struct Bucket { size_t size, start, i; } *buckets;
	unsigned long *taken;
	size_t taken_size = ((n_prime + CHAR_BIT - 1) / CHAR_BIT + sizeof *taken - 1) & ~(sizeof *taken - 1);
	uint16_t *pilots;
	if (!(buckets = malloc(m * (sizeof *buckets + sizeof *pilots) + taken_size)))
		return PHF_NO_MEMORY;

	// Find bucket start indices and sizes
	size_t bucket_start = 0;
	for (size_t i = 0; i < m - 1; ++i) { // Binary search for ends
		size_t lo = bucket_start, hi = n, mid = MIN(lo + n / m, hi) - 1;
		for (; lo < hi; mid = lo + (hi - lo) / 2)
			if (pthash_bucket(keys[mid], m) > i) hi = mid; else lo = mid + 1;
		buckets[i] = (struct Bucket) { .start = bucket_start, .size = lo - bucket_start, .i = i };
		bucket_start = lo;
	}
	buckets[m - 1] = (struct Bucket) { .start = bucket_start, .size = n - bucket_start, .i = m - 1 };
	// Sort buckets in order of non-increasing size
	qsort(buckets, m, sizeof *buckets, bucket_cmp);

	// Find bucket pilots
	enum PhfError status = PHF_OK;
	unsigned char pilot_width = 0;
	memset(taken = (unsigned long *) (buckets + m), 0, taken_size);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-align"
	pilots = (uint16_t *) ((char *) taken + taken_size);
#pragma GCC diagnostic pop
	size_t *positions;
	if (!(positions = malloc(buckets[0].size * sizeof *positions))) die("malloc failed");
	for (struct Bucket *bucket = buckets; bucket < buckets + m; ++bucket) {
		uint16_t pilot = 0;
		if (false) do_retry: if (++pilot >= MAX_TRIALS) { status = PHF_RESEED; goto err; }
		for (size_t i = 0; i < bucket->size; ++i) {
			size_t pos = pthash_position(keys[bucket->start + i], pilot, n_prime);
			if (BITSET_GET(taken, pos)) goto do_retry;
			for (size_t j = 0; j < i; ++j)
				if (pos == positions[j]) goto do_retry; // Intra-bucket collision
			positions[i] = pos;
		}
		pilots[bucket->i] = pilot;
		pilot_width = MAX(pilot_width, stdc_bit_width(pilot));
		for (size_t i = 0; i < bucket->size; ++i)
			taken[positions[i] / (CHAR_BIT * sizeof *taken)] |= 1ul << positions[i] % (CHAR_BIT * sizeof *taken);
	}

	size_t *remap;
	if (!(remap = malloc((n_prime - n) * sizeof *remap))) { status = PHF_NO_MEMORY; goto err; }
	for (size_t i = 0, p = 0, offset = 0; offset < n; ++i, offset += CHAR_BIT * sizeof *taken)
		FOR_ONES(x, HTOL(~taken[i])) {
			while (!BITSET_GET(taken, n + p)) ++p;
			remap[p++] = offset + x;
		}

	unsigned long *compressed_pilots;
	if (!(compressed_pilots = compress_vec(m, pilots, pilot_width))) {
		free(remap);
		status = PHF_NO_MEMORY;
		goto err;
	}

	*result = (struct Phf) {
		.n = n, .n_prime = n_prime, .m = m, .remap = remap,
		.pilots = (struct CompactVec) { .width = pilot_width, .data = compressed_pilots },
	};
err:
	free(positions);
	free(buckets);
	return status;
}

void phf_free(struct Phf *x) {
	free(x->pilots.data);
	free(x->remap);
}
