#include "phf.h"
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#define BITSET_OP(x, op, i) ((x)[(i) / (CHAR_BIT * sizeof *(x))] \
		op (typeof(*(x)))1 << (i) % (CHAR_BIT * sizeof *(x)))

static int key_cmp(const void *x, const void *y) {
	uint64_t a = *(uint64_t *) x, b = *(uint64_t *) y;
	return (a > b) - (a < b);
}
static int bucket_cmp(const void *x, const void *y) {
	size_t a = *(size_t *) x, b = *(size_t *) y;
	return (a < b) - (a > b);
}

/** Returns whether @a pilot yields no collisions. */
static bool try_pilot(unsigned char pilot, size_t len, uint64_t keys[static len],
	size_t n_prime, size_t *taken, size_t positions[static len]) {
	for (size_t i = 0; i < len; ++i) {
		size_t pos = pthash_position(keys[i], pilot, n_prime);
		if (BITSET_OP(taken, &, pos)) return false;
		for (size_t j = 0; j < i; ++j)
			if (UNLIKELY(pos == positions[j])) return false; // Intra-bucket collision
		positions[i] = pos;
	}
	return true;
}

enum PhfError phf_build(const struct PhfParameters *params,
	size_t n, uint64_t keys[static n], struct Phf *result) {
	assert(n);
	// Sort by hashes as fastrange64 makes pthash_bucket() monotonic
	qsort(keys, n, sizeof *keys, key_cmp);
	// Check for hash collision
	for (size_t i = 1; i < n; ++i) if (keys[i - 1] == keys[i]) return PHF_RESEED;

	size_t n_prime = n / params->alpha;
	if (IS_POWER_OF_TWO(n_prime)) ++n_prime; // Use all bits of entropy
	// Map keys into m buckets
	uint64_t m = MAX(params->c * n / stdc_bit_width(n), 1); // Total number of buckets
	struct Bucket { size_t size, start, i; } *buckets;
	size_t *slots;
	unsigned long *taken;
	size_t taken_size = ALIGN_UP((n_prime + CHAR_BIT - 1) / CHAR_BIT, sizeof *taken);
	if (!(buckets = malloc(m * sizeof *buckets + n_prime * sizeof *slots + taken_size)))
		return PHF_NO_MEMORY;

	// Find bucket ranges
	size_t bucket_start = 0;
	for (size_t i = 0; i < m - 1; ++i) { // Binary search for ends
		size_t lo = bucket_start, hi = n, mid = MIN(lo + n / m, hi) - 1;
		for (; lo < hi; mid = lo + (hi - lo) / 2)
			if (pthash_bucket(keys[mid], m) > i) hi = mid; else lo = mid + 1;
		buckets[i] = (struct Bucket)
			{ .start = bucket_start, .size = lo - bucket_start, .i = i };
		bucket_start = lo;
	}
	buckets[m - 1] = (struct Bucket)
		{ .start = bucket_start, .size = n - bucket_start, .i = m - 1 };
	// Sort buckets in order of non-increasing size
	qsort(buckets, m, sizeof *buckets, bucket_cmp);

	// Find bucket pilots
	enum PhfError status = PHF_OK;
	slots = (size_t *) (buckets + m);
	memset(taken = (unsigned long *) (slots + n_prime), 0, taken_size);
	size_t *positions, *remap = NULL;
	unsigned char *pilots;
	if (!(positions = malloc(buckets[0].size * sizeof *positions))
		|| !(remap = malloc((n_prime - n) * sizeof *remap + m * sizeof *pilots))) {
		status = PHF_NO_MEMORY;
		goto err;
	}
	pilots = (unsigned char *) (remap + (n_prime - n));
	for (size_t b = 0, stack[64], stack_size = 0, recent[16] = {}, recent_idx = 0,
				num_displacements = 0; b < m || stack_size;) {
		size_t b1 = stack_size ? stack[--stack_size] : b++;
		struct Bucket *bucket = buckets + (recent[recent_idx++ % LENGTH(recent)] = b1);
		unsigned char pilot = 0;
		// Fast path in case of no collisions
		do if (try_pilot(pilot, bucket->size, keys + bucket->start,
				n_prime, taken, positions)) goto out_found_pilot;
		while (pilot++ < UCHAR_MAX);

		// Search for pilot minimizing the number of collisions
		unsigned char p0 = pilot = rand();
		unsigned best_cost = UINT_MAX;
		do {
			unsigned cost = 0;
			for (size_t i = 0; i < bucket->size; ++i) {
				size_t pos = positions[i]
					= pthash_position(keys[bucket->start + i], pilot, n_prime);
				for (size_t j = 0; j < i; ++j)
					if (UNLIKELY(pos == positions[j])) goto do_retry;
				if (!BITSET_OP(taken, &, pos)) continue; // No collision!
				for (unsigned i = 0; i < LENGTH(recent); ++i)
					if (slots[pos] == recent[i]) goto do_retry;
				size_t size = buckets[slots[pos]].size;
				if ((cost += size * size) >= best_cost) goto do_retry;
			}
			if ((best_cost = cost) <= bucket->size * bucket->size) break;
		do_retry:
		} while (++pilot != p0);
		if (UNLIKELY(best_cost == UINT_MAX)) { status = PHF_RESEED; goto err; }
		// Displace colliding buckets by pushing them onto the stack
		for (size_t i = 0; i < bucket->size; ++i) {
			size_t pos = positions[i]
				= pthash_position(keys[bucket->start + i], pilot, n_prime);
			if (!BITSET_OP(taken, &, pos)) continue;
			if (UNLIKELY(stack_size >= LENGTH(stack) || ++num_displacements > 8 * n)) {
				status = PHF_RESEED;
				goto err;
			}
			struct Bucket *b2 = buckets + (stack[stack_size++] = slots[pos]);
			for (uint64_t *key = keys + b2->start, *end = key + b2->size; key < end; ++key) {
				size_t pos2 = pthash_position(*key, pilots[b2->i], n_prime);
				BITSET_OP(taken, ^=, pos2);
			}
		}

	out_found_pilot:
		pilots[bucket->i] = pilot;
		for (size_t i = 0; i < bucket->size; ++i) {
			BITSET_OP(taken, |=, positions[i]);
			slots[positions[i]] = b1;
		}
	}

	BITSET_OP(taken, -=, n_prime);
	for (size_t i = 0, p = 0, offset = 0; offset < n; ++i, offset += CHAR_BIT * sizeof *taken)
		FOR_ONES(x, ~taken[i]) {
			while (!BITSET_OP(taken, &, n + p)) ++p;
			remap[p++] = offset + x;
		}

	*result = (struct Phf) { n, n_prime, m, pilots, remap };
	if (false) err: free(remap);
	free(positions);
	free(buckets);
	return status;
}

void phf_free(struct Phf *x) { free(x->remap); }
