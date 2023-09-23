/** SWAR implementation of Swiss tables. */

#include "tbl.h"
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

#define SEED64 0x517cc1b727220a95

/** FxHash function. */
static uint64_t hash(uint64_t a, uint64_t b) {
	return (rotate_left(a, 5) ^ b) * SEED64;
}

/// Primary hash function, used for probing.
static size_t h1(uint64_t hash) { return hash; }

/// Secondary hash function, saved in the control byte.
static char h2(uint64_t hash) { return hash >> (sizeof hash - 7); }

static unsigned char empty_ctrl[] __attribute__ ((aligned (GROUP_WIDTH)))
	= { EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, };
_Static_assert(sizeof empty_ctrl / sizeof *empty_ctrl >= GROUP_WIDTH);
struct HashTable tbl_new() { return (struct HashTable) { .ctrl = empty_ctrl, }; }
void tbl_free(struct HashTable table) { if (table.buckets) free(table.ctrl); }

/** Return the integer with all bytes equal to @arg x. */
#define REPEAT(x) (x * (~0ULL / 0xff))

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

struct Key *tbl_find(struct HashTable table, struct Key key) {
	uint64_t h = hash(0, key.key);
	PROBE(table, h, bucket, group) {
		// Search the group for h2 of the key
		FOR_SET_BITS(i, match_byte(h2(h), group)) {
			struct Key *entry = table.buckets + ((bucket + i / 8) & table.bucket_mask);
			// Check if keys are equal
			if (__builtin_expect(entry->key == key.key, true)) return entry;
		}
		// Check if there were any empty matches
		if (__builtin_expect(match_byte(EMPTY, group), true)) return NULL;
	}
}

static size_t find_insert_slot(struct HashTable table, uint64_t h) {
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

static size_t bucket_mask_to_capacity(size_t mask) {
	return mask < 8 ? mask : ((mask + 1) / 8) * 7;
}

static size_t capacity_to_buckets(size_t capacity) {
	return capacity < 8 ? (capacity < 4 ? 4 : 8)
		: next_power_of_2(capacity * 8 / 7);
}

static void set_ctrl(struct HashTable table, size_t i, unsigned char ctrl) {
	size_t i2 = ((i - GROUP_WIDTH) & table.bucket_mask) + GROUP_WIDTH;
	table.ctrl[i2] = table.ctrl[i] = ctrl;
}

static void tbl_reserve(struct HashTable *table, size_t additional) {
	if (__builtin_expect(additional <= table->growth_left, true)) return;

	size_t new_len = table->len + additional,
		full_capacity = bucket_mask_to_capacity(table->bucket_mask),
		new_capacity = MAX(new_len, full_capacity + 1),
		n = capacity_to_buckets(new_capacity);

	struct HashTable new_table;
	new_table.bucket_mask = n - 1;
	size_t buckets_offset
		= (n + GROUP_WIDTH - 1 + 1 + alignof(struct Key) - 1) & (alignof(struct Key) - 1);
	if (!(new_table.ctrl = malloc(buckets_offset + n * sizeof(struct Key))))
		exit(1);
	memset(new_table.ctrl, EMPTY, n + GROUP_WIDTH);
	new_table.buckets = (struct Key *) (new_table.ctrl + buckets_offset);

	for (size_t i = 0; i < table->bucket_mask + 1; ++i) {
		if (!IS_FULL(table->ctrl[i])) continue;

		uint64_t h = hash(0, table->buckets[i].key);
		size_t new_i = find_insert_slot(new_table, h);
		set_ctrl(new_table, new_i, h2(h));
		new_table.buckets[new_i] = table->buckets[i];
	}
	new_table.growth_left = bucket_mask_to_capacity(new_table.bucket_mask)
		- (new_table.len = table->len);

	if (table->ctrl != empty_ctrl) free(table->ctrl);
	*table = new_table;
}

void tbl_insert(struct HashTable *table, struct Key key) {
	struct Key *entry;
	if ((entry = tbl_find(*table, key))) { *entry = key; return; }

	uint64_t h = hash(0, key.key);
	if (__builtin_expect(table->growth_left == 0, false)) tbl_reserve(table, 1);

	// Key is not present: Search for EMPTY/DELETED instead
	size_t i = find_insert_slot(*table, h);
	unsigned char old_ctrl = table->ctrl[i];
	table->growth_left -= (old_ctrl & 1) != 0; // Avoid decrementing if was tombstone
	++table->len;
	set_ctrl(*table, i, h2(h));
	table->buckets[i] = key;
}
