/** Rope data structure backed by B-tree. */

#ifndef ROPE_H
#define ROPE_H

#include <stddef.h>

struct RopeNode;

struct Rope {
	struct RopeNode *root;
};

bool rope_init(struct Rope *rope);

void rope_free(struct Rope *rope);

/** Replaces the bytes from @arg beg (inclusive) to @arg end (exclusive) with @arg s. */
void rope_replace(struct Rope *rope, size_t beg, size_t end, const char *text);

/** Returns the byte size of @arg rope. */
size_t rope_size(struct Rope *rope);

#endif
