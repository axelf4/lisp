/** SWAR implementation of Swiss tables. */

#if !(defined(NAME) && defined(KEY) && defined(TYPE) && defined(KEY_HASH) && defined(KEY_EQUAL))
#error
#else

#ifndef TBL_IMPL
#define TBL_IMPL

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdalign.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"

#define EMPTY 0b1111'1111
#define DELETED 0b1000'0000
#define IS_FULL(ctrl) !(ctrl & 0x80)

#define GROUP_WIDTH sizeof(size_t)

/// Primary hash function, used for probing.
static size_t h1(uint64_t hash) { return hash; }

/// Secondary hash function, saved in the control byte.
static char h2(uint64_t hash) { return hash >> (sizeof hash - 7); }

static size_t bucket_mask_to_capacity(size_t mask) {
	return mask < 8 ? mask : ((mask + 1) / 8) * 7;
}

static size_t capacity_to_buckets(size_t capacity) {
	return capacity < 8 ? (capacity < 4 ? 4 : 8)
		: next_power_of_2(capacity * 8 / 7);
}

/** Return the integer with all bytes equal to @arg x. */
#define REPEAT(x) (x * (~0ULL / 0xff))

#define FOR_SET_BITS(var, x) for (unsigned __i = x, var;			\
		__i && (var = __builtin_ctz(__i), 1); __i &= (__i - 1))

static size_t match_byte(unsigned char x, size_t group) {
	size_t cmp = group ^ REPEAT(x);
	return (cmp - REPEAT(0x01)) & ~cmp & REPEAT(0x80);
}
static size_t match_empty_or_deleted(size_t group) { return group & REPEAT(0x80); }

#define PROBE(table, hash, bucket, group)								\
	for (size_t bucket = h1(hash) & (table).bucket_mask, __probe_distance = 0, group; \
			memcpy(&group, (table).ctrl + bucket, sizeof group), true;	\
			/* Triangular probing */									\
			bucket = (bucket + GROUP_WIDTH * ++__probe_distance) & (table).bucket_mask)

#define SET_CTRL(table, i, x) \
	((table).ctrl[(((i) - GROUP_WIDTH) & (table).bucket_mask) + GROUP_WIDTH] \
		= (table).ctrl[i] = (x))

static unsigned char empty_ctrl[] __attribute__ ((aligned (GROUP_WIDTH)))
	= { EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, };
_Static_assert(LENGTH(empty_ctrl) >= GROUP_WIDTH);

#define JOIN(x, y) JOIN2(x, y)
#define JOIN2(x, y) x ## y
#endif

struct TYPE {
	size_t bucket_mask, ///< = n - 1
		growth_left,
		len;
	/// Array of n + GROUP_WIDTH "control" bytes.
	///
	/// Each byte is one of:
	/// * 0b1111_1111 (EMPTY)
	/// * 0b1000_0000 (DELETED (tombstone))
	/// * 0b0xxx_xxxx (FULL (x is a hash fragment))
	unsigned char *ctrl;
	KEY *buckets; ///< Array of n keys.
};

struct TYPE JOIN(NAME, _tbl_new)() { return (struct TYPE) { .ctrl = empty_ctrl, }; }
void JOIN(NAME, _tbl_free)(struct TYPE table) { if (table.buckets) free(table.ctrl); }

KEY *JOIN(NAME, _tbl_find)(struct TYPE table, KEY key) {
	uint64_t h = KEY_HASH(key);
	PROBE(table, h, bucket, group) {
		// Search the group for h2 of the key
		FOR_SET_BITS(i, match_byte(h2(h), group)) {
			KEY *entry = table.buckets + ((bucket + i / 8) & table.bucket_mask);
			// Check if keys are equal
			if (__builtin_expect(KEY_EQUAL(*entry, key), true)) return entry;
		}
		// Check if there were any empty matches
		if (__builtin_expect(match_byte(EMPTY, group), true)) return NULL;
	}
}

static size_t JOIN(NAME, _tbl__find_insert_slot)(struct TYPE table, uint64_t h) {
	PROBE(table, h, bucket, group) {
		unsigned x = match_empty_or_deleted(group);
		if (__builtin_expect(x, true)) {
			size_t match = (bucket + __builtin_ctz(x) / 8) & table.bucket_mask;
			// If n < GROUP_WIDTH, there may be fake EMPTY bytes before the mirror bytes
			if (IS_FULL(table.ctrl[match])) {
				group = *(size_t *) table.ctrl;
				match = (bucket + __builtin_ctz(match_empty_or_deleted(group)) / 8)
					& table.bucket_mask;
			}
			return match;
		}
	}
}

static void JOIN(NAME, _tbl_reserve)(struct TYPE *table, size_t additional) {
	if (__builtin_expect(additional <= table->growth_left, true)) return;

	size_t new_len = table->len + additional,
		full_capacity = bucket_mask_to_capacity(table->bucket_mask),
		new_capacity = MAX(new_len, full_capacity + 1),
		n = capacity_to_buckets(new_capacity);

	struct TYPE new_table;
	new_table.bucket_mask = n - 1;
	size_t buckets_offset
		= (n + GROUP_WIDTH - 1 + 1 + alignof(KEY) - 1) & (alignof(KEY) - 1);
	if (!(new_table.ctrl = malloc(buckets_offset + n * sizeof(KEY))))
		exit(1);
	memset(new_table.ctrl, EMPTY, n + GROUP_WIDTH);
	new_table.buckets = (KEY *) (new_table.ctrl + buckets_offset);

	for (size_t i = 0; i < table->bucket_mask + 1; ++i) {
		if (!IS_FULL(table->ctrl[i])) continue;

		uint64_t h = KEY_HASH(table->buckets[i]);
		size_t new_i = JOIN(NAME, _tbl__find_insert_slot)(new_table, h);
		SET_CTRL(new_table, new_i, h2(h));
		new_table.buckets[new_i] = table->buckets[i];
	}
	new_table.growth_left = bucket_mask_to_capacity(new_table.bucket_mask)
		- (new_table.len = table->len);

	if (table->ctrl != empty_ctrl) free(table->ctrl);
	*table = new_table;
}

bool JOIN(NAME, _tbl_entry)(struct TYPE *table, KEY key, KEY **entry) {
	if ((*entry = JOIN(NAME, _tbl_find)(*table, key))) { return true; }

	uint64_t h = KEY_HASH(key);
	if (__builtin_expect(table->growth_left == 0, false)) JOIN(NAME, _tbl_reserve)(table, 1);

	// Key is not present: Search for EMPTY/DELETED instead
	size_t i = JOIN(NAME, _tbl__find_insert_slot)(*table, h);
	unsigned char old_ctrl = table->ctrl[i];
	table->growth_left -= (old_ctrl & 1) != 0; // Avoid decrementing if was tombstone
	++table->len;
	SET_CTRL(*table, i, h2(h));
	*(*entry = table->buckets + i) = key;
	return false;
}

void JOIN(NAME, _tbl_insert)(struct TYPE *table, KEY key) {
	KEY *entry;
	JOIN(NAME, _tbl_entry)(table, key, &entry);
	*entry = key;
}

#undef NAME
#undef KEY
#undef TYPE
#undef KEY_HASH
#undef KEY_EQUAL
#endif
