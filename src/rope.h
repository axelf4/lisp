/** @file
 * Rope data structure backed by a B-tree.
 *
 * @see BOEHM, Hans‐J.; ATKINSON, Russ; PLASS, Michael. Ropes: An
 *      alternative to strings. Software: Practice and Experience,
 *      1995, 25.12: 1315-1330.
 */

#ifndef ROPE_H
#define ROPE_H

#include <stddef.h>

struct RopeNode;

struct Rope {
	struct RopeNode *root;
};

bool rope_init(struct Rope *rope);

void rope_free(struct Rope *rope);

/** Replaces the bytes from @a beg (inclusive) to @a end (exclusive) with @a s. */
void rope_replace(struct Rope *rope, size_t beg, size_t end, size_t len, const char s[static len]);

/** Returns the byte size of @a rope. */
size_t rope_size(struct Rope *rope);

#endif
