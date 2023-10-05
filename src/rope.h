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

struct Range { size_t start, end; };

/** Replaces the bytes in the inclusive-exclusive @arg range with @arg s. */
void rope_replace(struct Rope *rope, struct Range range, const char *s);

/** Returns the byte size of @arg rope. */
size_t rope_size(struct Rope *rope);

#endif
