/**
 * Swiss tables SWAR implementation.
 *
 * See: https://abseil.io/about/design/swisstables
 */

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-align"
#ifndef TBL_H
#define TBL_H

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "util.h"

#define EMPTY 0b1111'1111
#define DELETED 0b1000'0000
#define IS_FULL(ctrl) (!(ctrl & 0x80))

/// Primary hash function, used for probing.
static inline size_t h1(uint64_t hash) { return hash; }
/// Secondary hash function, saved in the control byte.
static inline char h2(uint64_t hash) { return hash >> (CHAR_BIT * (sizeof hash - 1) + 1); }

static inline size_t bucket_mask_to_capacity(size_t mask) {
	return mask < 8 ? mask : ((mask + 1) / 8) * 7;
}
static inline size_t capacity_to_buckets(size_t capacity) {
	return capacity < 8 ? (capacity < 4 ? 4 : 8) : next_power_of_2(capacity * 8 / 7);
}

typedef size_t Group;

/** Return the integer with all bytes equal to @arg x. */
#define REPEAT(x) (x * (~0ULL / 0xff))

#define FOR_SET_BITS(var, x) for (typeof(x) _i = x, var;			\
		_i && (var = __builtin_ctzll(_i), true); _i &= _i - 1)

static inline size_t match_byte(unsigned char x, size_t group) {
	size_t cmp = group ^ REPEAT(x);
	return (cmp - REPEAT(0x01)) & ~cmp & REPEAT(0x80);
}
static inline size_t match_empty_or_deleted(size_t group) { return group & REPEAT(0x80); }

#define PROBE(table, hash, bucket, group)								\
	for (size_t bucket = h1(hash) & (table)->bucket_mask, _probe_distance = 0, group; \
			memcpy(&group, (table)->ctrl + bucket, sizeof group), group = HTOL(group), true; \
			/* Triangular probing */									\
			bucket = (bucket + ++_probe_distance * sizeof group) & (table)->bucket_mask)

#define CTRL_OFFSET(n) (((n) * sizeof(KEY) + MAX(alignof(KEY), alignof(Group)) - 1) \
		& ~(MAX(alignof(KEY), alignof(Group)) - 1))

#define SET_CTRL(table, i, x) \
	((table).ctrl[(((i) - sizeof(Group)) & (table).bucket_mask) + sizeof(Group)] \
		= (table).ctrl[i] = (x))

/** Hash table. */
struct Table {
	size_t bucket_mask, ///< = n - 1
		growth_left,
		len;
	/** Array of n + GROUP_WIDTH "control" bytes, preceded by n buckets of keys.
	 *
	 * Each byte is one of:
	 * - 0b1111'1111: EMPTY
	 * - 0b1000'0000: DELETED (tombstone)
	 * - 0b0xxx'xxxx: FULL (x is a hash fragment) */
	unsigned char *ctrl;
};

static inline struct Table tbl_new() {
	static const Group empty_ctrl = REPEAT(EMPTY);
	return (struct Table) { .ctrl = (unsigned char *) &empty_ctrl };
}

static inline size_t find_insert_slot(struct Table *table, uint64_t h) {
	PROBE(table, h, bucket, group) {
		size_t x = match_empty_or_deleted(group);
		if (!__builtin_expect(x, true)) continue;
		size_t match = (bucket + __builtin_ctzll(x) / CHAR_BIT) & table->bucket_mask;
		// If n < GROUP_WIDTH, there may be fake EMPTY bytes before the mirror bytes
		if (IS_FULL(table->ctrl[match])) {
			group = HTOL(*(Group *) table->ctrl);
			match = __builtin_ctzll(match_empty_or_deleted(group)) / CHAR_BIT
				& table->bucket_mask;
		}
		return match;
	}
}
#endif

#if defined(NAME) && defined(KEY)

void CAT(NAME, _tbl_free)(struct Table *table) {
	if (table->bucket_mask) free(table->ctrl - CTRL_OFFSET(table->bucket_mask + 1));
}

KEY *CAT(NAME, _tbl_find)(struct Table *table, KEY key) {
	KEY *buckets = (KEY *) table->ctrl;
	uint64_t h = CAT(NAME, _hash)(key);
	PROBE(table, h, bucket, group) {
		// Search the group for h2 of the key
		FOR_SET_BITS(i, match_byte(h2(h), group)) {
			KEY *entry = buckets - ((bucket + i / CHAR_BIT) & table->bucket_mask) - 1;
			if (__builtin_expect(CAT(NAME, _equal)(*entry, key), true)) return entry;
		}
		// If there were any empty matches then probing is done
		if (__builtin_expect(match_byte(EMPTY, group), true)) return NULL;
	}
}

/**
 * Iterates over @arg table entries.
 *
 * @param i Auxilliary index that should be zero-initialized when starting iteration.
 * @param[out] entry Pointer to write the table entries to.
 * @return Whether @arg entry holds the current entry and iteration is not yet done.
 */
static bool CAT(NAME, _tbl_iter_next)(struct Table *table, size_t *i, KEY **entry) {
	while (*i <= table->bucket_mask) if (IS_FULL(table->ctrl[(*i)++])) {
			*entry = (KEY *) table->ctrl - *i;
			return true;
	}
	return false;
}

static bool CAT(NAME, _tbl_reserve)(struct Table *table, size_t additional) {
	if (__builtin_expect(additional <= table->growth_left, true)) return true;
	size_t new_capacity
		= MAX(table->len + additional, bucket_mask_to_capacity(table->bucket_mask) + 1),
		n = capacity_to_buckets(new_capacity);

	unsigned char *ctrl;
	size_t ctrl_offset = CTRL_OFFSET(n);
	if (!(ctrl = malloc(ctrl_offset + n + sizeof(Group)))) return false;
	memset(ctrl += ctrl_offset, EMPTY, n + sizeof(Group));
	struct Table new_table
		= { n - 1, bucket_mask_to_capacity(n - 1) - table->len, table->len, ctrl };
	KEY *x;
	for (size_t i = 0; CAT(NAME, _tbl_iter_next)(table, &i, &x);) {
		uint64_t h = CAT(NAME, _hash)(*x);
		size_t new_i = find_insert_slot(&new_table, h);
		SET_CTRL(new_table, new_i, h2(h));
		((KEY *) new_table.ctrl)[-new_i - 1] = *x;
	}
	CAT(NAME, _tbl_free)(table);
	*table = new_table;
	return true;
}

/**
 * Inserts @arg key if it does not yet exist, and outputs the entry into @arg entry.
 *
 * @return Whether @arg key was already present.
 */
bool CAT(NAME, _tbl_entry)(struct Table *table, KEY key, KEY **entry) {
	if ((*entry = CAT(NAME, _tbl_find)(table, key))) return true;

	if (!(__builtin_expect(table->growth_left, true) || CAT(NAME, _tbl_reserve)(table, 1)))
		return (*entry = NULL);
	// Key is not present: Search for EMPTY/DELETED instead
	uint64_t h = CAT(NAME, _hash)(key);
	size_t i = find_insert_slot(table, h);
	table->growth_left -= table->ctrl[i] & 1; // Decrement unless replaced tombstone
	++table->len;
	SET_CTRL(*table, i, h2(h));
	KEY *buckets = (KEY *) table->ctrl;
	*(*entry = buckets - i - 1) = key;
	return false;
}
#pragma GCC diagnostic pop

#undef NAME
#undef KEY
#endif
