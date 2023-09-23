#ifndef TBL_H
#define TBL_H

#include <stddef.h>

struct Key {
	size_t key;
	void *value;
};

struct HashTable {
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
	struct Key *buckets; ///< Array of n keys.
};

struct HashTable tbl_new();

void tbl_free(struct HashTable table);

struct Key *tbl_find(struct HashTable table, struct Key key);

void tbl_insert(struct HashTable *table, struct Key key);

#endif
