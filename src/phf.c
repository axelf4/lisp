#include "phf.h"
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#define BITSET_GET(x, i) ((x)[(i) / (CHAR_BIT * sizeof *(x))] \
		& (typeof(*(x))) 1 << (i) % (CHAR_BIT * sizeof *(x)))

static int key_cmp(const void *x, const void *y) {
	uint64_t a = *(uint64_t *) x, b = *(uint64_t *) y;
	return (a > b) - (a < b);
}
static int bucket_cmp(const void *x, const void *y) {
	size_t a = *(size_t *) x, b = *(size_t *) y;
	return (a < b) - (a > b);
}

enum PhfError phf_build(const struct PhfParameters *params, size_t n, uint64_t keys[static n], struct Phf *result) {
	assert(n);
	// Sort by hashes as fastrange64 makes pthash_bucket() monotonic
	qsort(keys, n, sizeof *keys, key_cmp);
	// Check for hash collision
	for (size_t i = 1; i < n; ++i) if (keys[i - 1] == keys[i]) return PHF_RESEED;

	size_t n_prime = n / params->alpha;
	if (IS_POWER_OF_TWO(n_prime)) ++n_prime; // Use all bits of entropy
	// Map keys into m buckets
	uint64_t m = params->c * n / stdc_bit_width(n) + 0.5; // Total number of buckets
	struct Bucket { size_t size, start, i; } *buckets;
	size_t *slots;
	unsigned long *taken;
	size_t taken_size = ((n_prime + CHAR_BIT - 1) / CHAR_BIT + sizeof *taken - 1) & ~(sizeof *taken - 1);
	if (!(buckets = malloc(m * sizeof *buckets + n_prime * sizeof *slots + taken_size)))
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
	slots = (size_t *) (buckets + m);
	memset(taken = (unsigned long *) (slots + n_prime), 0, taken_size);
	size_t *positions;
	unsigned char *pilots = NULL;
	if (!(positions = malloc(buckets[0].size * sizeof *positions))
		|| !(pilots = malloc(m * sizeof *pilots))) { status = PHF_NO_MEMORY; goto err; }
	for (size_t b = 0, stack[64], stack_size = 0, recent[16] = {}, recent_idx = 0,
				num_displacements = 0; b < m || stack_size;) {
		struct Bucket *bucket = buckets + (recent[recent_idx++ % LENGTH(recent)]
			= stack_size ? stack[--stack_size] : b++);
		unsigned char pilot = 0;
		if (false) do_retry: if (pilot++ >= UCHAR_MAX) {
				// Search for pilot minimizing the number of collisions
				unsigned char p0 = rand(), p = p0;
				unsigned best_cost = UINT_MAX;
				do {
					unsigned cost = 0;
					for (size_t i = 0; i < bucket->size; ++i) {
						size_t pos = positions[i]
							= pthash_position(keys[bucket->start + i], p, n_prime);
						for (size_t j = 0; j < i; ++j)
							if (UNLIKELY(pos == positions[j])) goto try_next_pilot; // Intra-bucket collision
						if (!BITSET_GET(taken, pos)) continue; // No collision!
						for (unsigned i = 0; i < LENGTH(recent); ++i)
							if (slots[pos] == recent[i]) goto try_next_pilot;
						size_t size = buckets[slots[pos]].size;
						if ((cost += size * size) >= best_cost) goto try_next_pilot;
					}
					pilot = p;
					if ((best_cost = cost) <= bucket->size * bucket->size) break;
				try_next_pilot:
				} while (++p != p0);
				if (UNLIKELY(best_cost == UINT_MAX)) { status = PHF_RESEED; goto err; }

				// Displace colliding buckets by pushing them onto the stack
				for (size_t i = 0; i < bucket->size; ++i) {
					size_t pos = positions[i]
						= pthash_position(keys[bucket->start + i], pilot, n_prime);
					if (!BITSET_GET(taken, pos)) continue;
					if (UNLIKELY(stack_size >= LENGTH(stack) || ++num_displacements > 8 * n)) {
						status = PHF_RESEED; goto err;
					}
					struct Bucket *b2 = buckets + (stack[stack_size++] = slots[pos]);
					for (uint64_t *key = keys + b2->start, *end = key + b2->size; key < end; ++key) {
						size_t pos2 = pthash_position(*key, pilots[b2->i], n_prime);
						taken[pos2 / (CHAR_BIT * sizeof *taken)] ^= 1ul << pos2 % (CHAR_BIT * sizeof *taken);
					}
				}
				goto found_pilot;
		}
		// Fast path in case of no collisions
		for (size_t i = 0; i < bucket->size; ++i) {
			size_t pos = pthash_position(keys[bucket->start + i], pilot, n_prime);
			if (BITSET_GET(taken, pos)) goto do_retry;
			for (size_t j = 0; j < i; ++j)
				if (UNLIKELY(pos == positions[j])) goto do_retry; // Intra-bucket collision
			positions[i] = pos;
		}
	found_pilot:
		pilots[bucket->i] = pilot;
		for (size_t i = 0; i < bucket->size; ++i) {
			taken[positions[i] / (CHAR_BIT * sizeof *taken)] |= 1ul << positions[i] % (CHAR_BIT * sizeof *taken);
			slots[positions[i]] = bucket - buckets;
		}
	}

	size_t *remap;
	if (!(remap = malloc((n_prime - n) * sizeof *remap))) { status = PHF_NO_MEMORY; goto err; }
	for (size_t i = 0, p = 0, offset = 0; offset < n; ++i, offset += CHAR_BIT * sizeof *taken)
		FOR_ONES(x, ~taken[i]) {
			while (!BITSET_GET(taken, n + p)) ++p;
			remap[p++] = offset + x;
		}

	*result = (struct Phf) {
		.n = n, .n_prime = n_prime, .m = m, .pilots = pilots, .remap = remap,
	};
	if (false) err: free(pilots);
	free(positions);
	free(buckets);
	return status;
}

void phf_free(struct Phf *x) {
	free(x->pilots);
	free(x->remap);
}
