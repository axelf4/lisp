/** Swiss tables SWAR implementation. */

#if !(defined(NAME) && defined(KEY) && defined(TYPE))
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
#include <limits.h>
#include "util.h"

#define EMPTY 0b1111'1111
#define DELETED 0b1000'0000
#define IS_FULL(ctrl) !(ctrl & 0x80)

/// Primary hash function, used for probing.
static size_t h1(uint64_t hash) { return hash; }
/// Secondary hash function, saved in the control byte.
static char h2(uint64_t hash) { return hash >> (CHAR_BIT * (sizeof hash - 1) + 1); }

static size_t bucket_mask_to_capacity(size_t mask) {
	return mask < 8 ? mask : ((mask + 1) / 8) * 7;
}
static size_t capacity_to_buckets(size_t capacity) {
	return capacity < 8 ? (capacity < 4 ? 4 : 8) : next_power_of_2(capacity * 8 / 7);
}

typedef size_t Group;

/** Return the integer with all bytes equal to @arg x. */
#define REPEAT(x) (x * (~0ULL / 0xff))

#define FOR_SET_BITS(var, x) for (typeof(x) _i = x, var;			\
		_i && (var = __builtin_ctzll(_i), true); _i &= (_i - 1))

static size_t match_byte(unsigned char x, size_t group) {
	size_t cmp = group ^ REPEAT(x);
	return (cmp - REPEAT(0x01)) & ~cmp & REPEAT(0x80);
}
static size_t match_empty_or_deleted(size_t group) { return group & REPEAT(0x80); }

#define PROBE(table, hash, bucket, group)								\
	for (size_t bucket = h1(hash) & (table)->bucket_mask, _probe_distance = 0, group; \
			memcpy(&group, (table)->ctrl + bucket, sizeof group), true;	\
			/* Triangular probing */									\
			bucket = (bucket + sizeof(Group) * ++_probe_distance) & (table)->bucket_mask)

#define BUCKETS(table) ((typeof((table).buckets)) (table).ctrl - ((table).bucket_mask + 1))

#define TBL_FOR_EACH(table, entry) for (typeof((table).buckets) _start = BUCKETS(table), \
			entry = _start;	entry < _start + (table).bucket_mask + 1; ++entry) \
		if (!IS_FULL((table).ctrl[entry - _start])) ; else

#define CTRL_OFFSET(n) (((n) * sizeof(KEY) + MAX(alignof(KEY), alignof(Group)) - 1) \
		& ~(MAX(alignof(KEY), alignof(Group)) - 1))

#define SET_CTRL(table, i, x) \
	((table).ctrl[(((i) - sizeof(Group)) & (table).bucket_mask) + sizeof(Group)] \
		= (table).ctrl[i] = (x))

static unsigned char empty_ctrl[] [[gnu::aligned (alignof(Group))]]
	= { EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, };
_Static_assert(LENGTH(empty_ctrl) >= sizeof(Group));
#endif

struct TYPE {
	size_t bucket_mask, ///< = n - 1
		growth_left,
		len;
	/// Array of n + GROUP_WIDTH "control" bytes, preceded by n buckets of keys.
	///
	/// Each byte is one of:
	/// * 0b1111_1111 (EMPTY)
	/// * 0b1000_0000 (DELETED (tombstone))
	/// * 0b0xxx_xxxx (FULL (x is a hash fragment))
	union { KEY *buckets; unsigned char *ctrl; };
};

struct TYPE CAT(NAME, _tbl_new)() { return (struct TYPE) { .ctrl = empty_ctrl, }; }
void CAT(NAME, _tbl_free)(struct TYPE *table) {
	if (table->bucket_mask) free(table->ctrl - CTRL_OFFSET(table->bucket_mask + 1));
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-align"
KEY *CAT(NAME, _tbl_find)(struct TYPE *table, KEY key) {
	uint64_t h = CAT(NAME, _hash)(key);
	PROBE(table, h, bucket, group) {
		// Search the group for h2 of the key
		FOR_SET_BITS(i, match_byte(h2(h), group)) {
			KEY *entry = BUCKETS(*table) + ((bucket + i / CHAR_BIT) & table->bucket_mask);
			if (__builtin_expect(CAT(NAME, _equal)(*entry, key), true)) return entry;
		}
		// Check if there were any empty matches
		if (__builtin_expect(match_byte(EMPTY, group), true)) return NULL;
	}
}

static size_t CAT(NAME, _tbl__find_insert_slot)(struct TYPE *table, uint64_t h) {
	PROBE(table, h, bucket, group) {
		size_t x = match_empty_or_deleted(group);
		if (!__builtin_expect(x, true)) continue;
		size_t match = (bucket + __builtin_ctzll(x) / CHAR_BIT) & table->bucket_mask;
		// If n < GROUP_WIDTH, there may be fake EMPTY bytes before the mirror bytes
		if (IS_FULL(table->ctrl[match])) {
			group = *(size_t *) table->ctrl;
			match = (bucket + __builtin_ctzll(match_empty_or_deleted(group)) / CHAR_BIT)
				& table->bucket_mask;
		}
		return match;
	}
}

static void CAT(NAME, _tbl_reserve)(struct TYPE *table, size_t additional) {
	if (__builtin_expect(additional <= table->growth_left, true)) return;
	size_t new_capacity
		= MAX(table->len + additional, bucket_mask_to_capacity(table->bucket_mask) + 1),
		n = capacity_to_buckets(new_capacity);

	unsigned char *ctrl;
	size_t ctrl_offset = CTRL_OFFSET(n);
	if (!(ctrl = malloc(ctrl_offset + n + sizeof(Group)))) exit(1);
	memset(ctrl += ctrl_offset, EMPTY, n + sizeof(Group));
	struct TYPE new_table = {
		n - 1, bucket_mask_to_capacity(n - 1) - table->len, table->len, .ctrl = ctrl,
	};
	TBL_FOR_EACH(*table, x) {
		uint64_t h = CAT(NAME, _hash)(*x);
		size_t new_i = CAT(NAME, _tbl__find_insert_slot)(&new_table, h);
		SET_CTRL(new_table, new_i, h2(h));
		BUCKETS(new_table)[new_i] = *x;
	}
	CAT(NAME, _tbl_free)(table);
	*table = new_table;
}

bool CAT(NAME, _tbl_entry)(struct TYPE *table, KEY key, KEY **entry) {
	if ((*entry = CAT(NAME, _tbl_find)(table, key))) return true;

	uint64_t h = CAT(NAME, _hash)(key);
	if (__builtin_expect(!table->growth_left, false)) CAT(NAME, _tbl_reserve)(table, 1);

	// Key is not present: Search for EMPTY/DELETED instead
	size_t i = CAT(NAME, _tbl__find_insert_slot)(table, h);
	table->growth_left -= table->ctrl[i] & 1; // Avoid decrementing if replaced tombstone
	++table->len;
	SET_CTRL(*table, i, h2(h));
	*(*entry = BUCKETS(*table) + i) = key;
	return false;
}
#pragma GCC diagnostic pop

void CAT(NAME, _tbl_insert)(struct TYPE *table, KEY key) {
	KEY *entry;
	CAT(NAME, _tbl_entry)(table, key, &entry);
	*entry = key;
}

#undef NAME
#undef KEY
#undef TYPE
#endif
