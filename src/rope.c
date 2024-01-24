#include "rope.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "util.h"

#ifdef DEBUG
#define MAX_BYTES 16
#else
#define MAX_BYTES 2048
#endif
#define MIN_BYTES (MAX_BYTES / 4)

struct GapBuffer {
	uint16_t len_left, ///< Number of bytes left of the gap.
		len_right; ///< Number of bytes right of the gap.
	char data[MAX_BYTES];
};

/** Recenter the gap such that @a i bytes are to the left of it. */
static void move_gap(struct GapBuffer *x, uint16_t i) {
	if (i <= x->len_left) { // New gap is in left section: x|x...yy => x|...xyy
		uint16_t len_moved = x->len_left - i;
		memmove(x->data + MAX_BYTES - x->len_right - len_moved, x->data + i, len_moved);
		x->len_right += len_moved;
	} else { // Otherwise, in right section: xx...y|y => xxy|...y
		uint16_t len_moved = i - x->len_left;
		memmove(x->data + x->len_left, x->data + MAX_BYTES - x->len_right, len_moved);
		x->len_right -= len_moved;
	}
	x->len_left = i;
}

static size_t gap_buffer_len(struct GapBuffer *x) { return x->len_left + x->len_right; }

#define MAX_CHILDREN 4
#define MIN_CHILDREN (MAX_CHILDREN / 2)

/** Rope tree node.
 *
 * @invariant
 * 1. Internal nodes have between MIN_CHILDREN and MAX_CHILDREN
 *    children, except the root which may have as few as two.
 * 2. All leaves appear at the same level.
 * 3. Leaf nodes contain at most MAX_BYTES and generally at least
 *    MIN_BYTES bytes.
 */
typedef struct RopeNode {
	unsigned char depth; ///< Height of subtree, i.e. zero signifies leaf node.
} Node;

struct Internal {
	Node node;
	unsigned char num_children;
	Node *children[MAX_CHILDREN];
	size_t byte_size; ///< Total byte size of the leaves in this subtree.
};

struct Leaf {
	Node node;
	struct GapBuffer value;
};

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-align"
static bool is_underfilled(Node *x) {
	return x->depth ? ((struct Internal *) x)->num_children < MIN_CHILDREN
		: gap_buffer_len(&((struct Leaf *) x)->value) < MIN_BYTES;
}

static size_t node_byte_size(Node *x) {
	return x->depth ? ((struct Internal *) x)->byte_size
		: gap_buffer_len(&((struct Leaf *) x)->value);
}

static void node_free(Node *node) {
	if (node->depth) {
		struct Internal *x = (struct Internal *) node;
		for (unsigned i = 0; i < x->num_children; ++i) node_free(x->children[i]);
	}
	free(node);
}

static void replace_with_child(Node **node) {
	if (((struct Internal *) *node)->num_children == 1) {
		Node *child = *((struct Internal *) *node)->children;
		free(*node);
		*node = child;
	}
}

static void count_child(struct Internal *parent, int sign, Node *child) {
	parent->byte_size += sign * node_byte_size(child);
}

static void insert_children(struct Internal *node, unsigned i,
	unsigned len, Node *xs[static len]) {
	memmove(node->children + i + len, node->children + i,
		(node->num_children - i) * sizeof *node->children);
	memcpy(node->children + i, xs, len * sizeof *xs);
	node->num_children += len;
	for (Node **x = xs; x < xs + len; ++x) count_child(node, 1, *x);
}

static void remove_children(struct Internal *parent, unsigned i, unsigned len) {
	for (Node **x = parent->children + i; x < parent->children + i + len; ++x)
		count_child(parent, -1, *x);
	memmove(parent->children + i, parent->children + i + len,
		((parent->num_children -= len) - i) * sizeof *parent->children);
}

static Node *remove_child(struct Internal *parent, unsigned i) {
	Node *child = parent->children[i];
	remove_children(parent, i, 1);
	return child;
}

struct NodeSlice { size_t len; Node **xs; };

static void make_parents(size_t count, struct NodeSlice xs[static count], size_t total, struct NodeSlice *out) {
	size_t len = 0;
	struct NodeSlice acc = { 0, (Node *[MAX_CHILDREN]) {} };
	for (unsigned i = 0; i < count; ++i) {
		struct NodeSlice x = xs[i];
		while (x.len) {
			// If cannot fit the rest then leave enough for a non-underfilled node
			unsigned max = (total > MAX_CHILDREN
				? MIN(MAX_CHILDREN, total - MIN_CHILDREN) : total) - acc.len,
				n = MIN(x.len, max);
			memcpy(acc.xs + acc.len, x.xs, n * sizeof *acc.xs);
			acc.len += n;
			x.len -= n;
			x.xs += n;

			if (n < max) continue;
			struct Internal *node;
			if (!(node = malloc(sizeof *node))) die("malloc failed");
			*node = (struct Internal) { .node = { .depth = (*acc.xs)->depth + 1 } };
			insert_children(node, 0, acc.len, acc.xs);
			out->xs[len++] = &node->node;
			total -= acc.len;
			acc.len = 0;
		}
	}
	out->len = len;
}

/** Inserts the children in @a xs at index @a i of @a node.
 *
 * @param[in,out] xs New children, and afterward, extra siblings to insert after @a node.
 */
static void insert_children_overflowing(struct Internal *node, unsigned i, struct NodeSlice *xs) {
	if (node->num_children + xs->len <= MAX_CHILDREN) {
		insert_children(node, i, xs->len, xs->xs);
		xs->len = 0;
		return;
	}
	struct NodeSlice new = *xs;
#define SATURATING_USUB(a, b) ((a) < (b) ? 0 : (a) - (b))
	unsigned num_rchildren = node->num_children - i,
		num_lchildren = SATURATING_USUB(MIN_CHILDREN, new.len + num_rchildren);
	Node *lchildren[MAX_CHILDREN], **rchildren = lchildren + num_lchildren;
	memcpy(rchildren, node->children + i, num_rchildren * sizeof *rchildren);
	remove_children(node, i, num_rchildren);
	if (num_lchildren) {
		memcpy(lchildren, node->children + i - num_lchildren, num_lchildren * sizeof *lchildren);
		remove_children(node, i - num_lchildren, num_lchildren);
	} else while (node->num_children < MIN_CHILDREN)
			   insert_children(node, node->num_children, 1,
				   new.len ? (--new.len, new.xs++) : (--num_rchildren, rchildren++));
	struct NodeSlice slices[]
		= { { num_lchildren, lchildren }, new, { num_rchildren, rchildren } };
	make_parents(LENGTH(slices), slices, num_lchildren + new.len + num_rchildren, xs);
}

/** Balances @a a with its right sibling @a b.
 *
 * @return Whether @a b became empty.
 */
static bool balance(Node *a, Node *b) {
	if (a->depth != b->depth) __builtin_unreachable();
	if (!(is_underfilled(b) || is_underfilled(a))) ;
	else if (b->depth) {
		struct Internal *x = (struct Internal *) a, *y = (struct Internal *) b;
		if (x->num_children + y->num_children <= MAX_CHILDREN) {
			insert_children(x, x->num_children, y->num_children, y->children);
			y->num_children = y->byte_size = 0;
			return true;
		} else if (x->num_children < y->num_children) { // Move child from b to a
			insert_children(x, x->num_children, 1, y->children);
			remove_children(y, 0, 1);
		} else { // Move child from a to b
			insert_children(y, 0, 1, x->children + x->num_children - 1);
			remove_children(x, x->num_children - 1, 1);
		}
	} else {
		struct GapBuffer *x = &((struct Leaf *) a)->value, *y = &((struct Leaf *) b)->value;
		unsigned x_size = gap_buffer_len(x), y_size = gap_buffer_len(y);
		if (x_size + y_size <= MAX_BYTES) {
			move_gap(x, x_size);
			memcpy(x->data + x_size, y->data, y->len_left);
			x->len_left += y->len_left;
			memcpy(x->data + MAX_BYTES - (x->len_right = y->len_right),
				y->data + MAX_BYTES - y->len_right, y->len_right);
			y->len_left = y->len_right = 0;
			return true;
		} else if (x_size > y_size) { // Move bytes from x to y
			move_gap(x, x_size);
			unsigned n = MIN_BYTES - y_size;
			memmove(y->data + n, y->data, y->len_left); // Make room
			memcpy(y->data, x->data + (x->len_left -= n), n);
			y->len_left += n;
		} else { // Move bytes from y to x
			unsigned n = MIN_BYTES - x_size;
			move_gap(y, n);
			memmove(x->data + MAX_BYTES - x->len_right - n,
				x->data + MAX_BYTES - x->len_right, x->len_right); // Make room
			memcpy(x->data + MAX_BYTES - n, y->data, n);
			x->len_right += n;
			y->len_left = 0;
		}
	}
	return false;
}

static Node *append_child(struct Internal *parent, Node *x) {
	Node *last = parent->children[parent->num_children - 1];
	if (x->depth + 1 < parent->node.depth) {
		count_child(parent, -1, last);
		x = append_child((struct Internal *) last, x);
		count_child(parent, 1, last);
		if (!x) return NULL;
	} else {
		count_child(parent, -1, last);
		bool b = balance(last, x);
		count_child(parent, 1, last);
		if (b) { free(x); return NULL; }
	}
	if (parent->num_children < MAX_CHILDREN) {
		insert_children(parent, parent->num_children, 1, &x);
		return NULL;
	}
	struct Internal *new;
	if (!(new = malloc(sizeof *new))) die("malloc failed");
	last = remove_child(parent, MAX_CHILDREN - 1);
	*new = (struct Internal) { .node = { .depth = parent->node.depth } };
	insert_children(new, 0, 2, (Node *[]) { last, x });
	return &new->node;
}

static Node *prepend_child(struct Internal *parent, Node *x) {
	Node *first = *parent->children;
	if (x->depth + 1 < parent->node.depth) {
		count_child(parent, -1, first);
		x = prepend_child((struct Internal *) first, x);
		count_child(parent, 1, first);
		if (!x) return NULL;
	} else {
		count_child(parent, -1, first);
		if (balance(x, first)) {
			free(first);
			count_child(parent, 1, *parent->children = x);
			return NULL;
		} else count_child(parent, 1, first);
	}
	if (parent->num_children < MAX_CHILDREN) {
		insert_children(parent, 0, 1, &x);
		return NULL;
	}
	struct Internal *new;
	if (!(new = malloc(sizeof *new))) die("malloc failed");
	first = remove_child(parent, 0);
	*new = *parent;
	*parent = (struct Internal) { .node = { .depth = new->node.depth } };
	insert_children(parent, 0, 2, (Node *[]) { x, first });
	return &new->node;
}

static void insert_at_depth(struct Internal *parent, unsigned i, Node *x) {
	Node *new;
	if (i > 0) {
		Node *last = parent->children[i - 1];
		count_child(parent, -1, last);
		new = append_child((struct Internal *) last, x);
		count_child(parent, 1, last);
	} else {
		Node *first = *parent->children;
		count_child(parent, -1, first);
		new = prepend_child((struct Internal *) first, x);
		count_child(parent, 1, first);
	}
	if (new) insert_children(parent, i, 1, &new);
}

/** String slice. */
struct Str { size_t len; const char *p; };

static struct SplitStr { struct Str left, right; } str_split_at(struct Str s, size_t i) {
	// TODO Check char boundary
	return (struct SplitStr) { { i, s.p }, { s.len - i, s.p + i } };
}

static struct NodeSlice segment_chunks(size_t size, size_t count, struct Str segments[static count]) {
	size_t capacity = (size + MAX_BYTES - 1) / MAX_BYTES;
	Node **nodes, **p;
	if (!(p = nodes = malloc(capacity * sizeof *nodes))) return (struct NodeSlice) {};
	for (Node **node = nodes; node < nodes + capacity; ++node) {
		struct Leaf *leaf;
		if (!(leaf = malloc(sizeof *leaf))) {
			for (Node **x = nodes; x < node; ++x) free(*x);
			free(nodes);
			return (struct NodeSlice) {};
		}
		leaf->node.depth = leaf->value.len_left = leaf->value.len_right = 0;
		*node = &leaf->node;
	}
	for (struct Str *s = segments; s < segments + count; ++s) while (s->len) {
			struct GapBuffer *buf = &((struct Leaf *) *p)->value;
			unsigned max = (size > MAX_BYTES ? MIN(MAX_BYTES, size - MIN_BYTES) : size)
				- buf->len_left,
				n = MIN(s->len, max);
			memcpy(buf->data + buf->len_left, s->p, n);
			buf->len_left += n;
			s->len -= n;
			s->p += n;
			if (n == max) { size -= buf->len_left; ++p; }
	}
	return (struct NodeSlice) { capacity, nodes };
}

static void replace_child_range_with_leaves(struct Internal *node, unsigned child_start, unsigned child_end,
	struct NodeSlice extra_leaves, size_t *extras_offset) {
	for (unsigned i = child_start; i < child_end; ++i) {
		if (*extras_offset == extra_leaves.len) {
			for (Node **x = node->children + i; x < node->children + child_end; ++x) {
				count_child(node, -1, *x);
				node_free(*x);
			}
			memmove(node->children + i, node->children + child_end,
				((node->num_children -= child_end - i) - i) * sizeof *node->children);
			return;
		}
		Node **child = node->children + i;
		count_child(node, -1, *child);
		if ((*child)->depth)
			replace_child_range_with_leaves(((struct Internal *) *child),
				0, ((struct Internal *) *child)->num_children,
				extra_leaves, extras_offset);
		else {
			free(*child);
			*child = extra_leaves.xs[(*extras_offset)++];
		}
		count_child(node, 1, *child);
	}
}

static void replace_nodes_in_end_subtree(Node **node, size_t end,
	struct NodeSlice *extras, size_t *extras_offset) {
	if ((*node)->depth) {
		struct Internal *x = (struct Internal *) *node;
		Node **child = x->children;
		for (size_t n; (n = node_byte_size(*child)) < end; end -= n) ++child;
		unsigned prev_num_children = x->num_children;
		replace_child_range_with_leaves(x, 0, child - x->children, *extras, extras_offset);
		child -= prev_num_children - x->num_children;

		count_child(x, -1, *child);
		replace_nodes_in_end_subtree(child, end, extras, extras_offset);
		count_child(x, 1, *child);
		if (extras->len) insert_children_overflowing(x, child - x->children, extras);
		return;
	}

	// This is the rightmost leaf affected by the replacement
	struct GapBuffer *buf = &((struct Leaf *) *node)->value;
	move_gap(buf, end);
	buf->len_left = 0;
	if (extras->len - *extras_offset && balance(extras->xs[extras->len - 1], *node)) {
		free(*node);
		*node = extras->xs[--extras->len];
	}
	if (extras->len) // Compact the extra nodes list to discharge extras_offset
		memmove(extras->xs, extras->xs + *extras_offset,
			(extras->len -= *extras_offset) * sizeof *extras->xs);
}

/** Patches deletion seam between @a i:th child of @a x and @a j:th child of @a y.
 *
 * The pointers @a x and @a y may alias.
 *
 * @post No child of @a x or @a y is underfilled or a different height.
 * @return Whether @a x needs to be replaced with its sole unbalanced
 *         child, and @a y now has @a j children.
 */
static bool fix_seam(struct Internal *x, unsigned i, struct Internal *y, unsigned j) {
	bool result = false;
	Node *l = x->children[i], *r = y->children[j];
	count_child(x, -1, l);
	count_child(y, -1, r);
	if (r->depth && fix_seam(
			(struct Internal *) l, ((struct Internal *) l)->num_children - 1,
			(struct Internal *) r, 0)) {
		count_child(x, 1, l);
		free(remove_child(y, j)); // Remove r as it became empty
		replace_with_child(x->children + i);
		// l was replaced by its child: Insert it at the right depth
		struct Internal *parent;
		unsigned insert_idx;
		if (x->num_children > 1) { parent = x; insert_idx = i; }
		else if (y != x && y->num_children) { parent = y; insert_idx = 0; }
		else return true;
		insert_at_depth(parent, insert_idx, remove_child(x, i));
		return false;
	} else if (balance(l, r)) {
		free(remove_child(y, j));
		if (!is_underfilled(l)) ;
		else if (i) {
			count_child(x, 1, l);
			if (balance(x->children[i - 1], l)) free(remove_child(x, i));
			return false;
		} else if (j < y->num_children) {
			count_child(y, -1, y->children[j]);
			if (balance(l, y->children[j])) free(remove_child(y, j));
			else count_child(y, 1, y->children[j]);
		} else result = true;
	} else count_child(y, 1, r);
	count_child(x, 1, l);
	return result;
}

static struct NodeSlice node_replace(Node **node, size_t beg, size_t end, struct Str s, size_t *extras_offset) {
	if (!(*node)->depth) {
		struct GapBuffer *buf = &((struct Leaf *) *node)->value;
		end = MIN(end, gap_buffer_len(buf));
		size_t len = gap_buffer_len(buf) - (end - beg) + s.len;
		if (len <= MAX_BYTES) {
			move_gap(buf, end);
			memcpy(buf->data + (buf->len_left -= end - beg), s.p, s.len);
			buf->len_left += s.len;
			return (struct NodeSlice) {};
		}

		struct Str ll, lr, // Text left of replacement, left/right of gap respectively,
			rl, rr; // ... and right of replacement.
		if (beg <= buf->len_left) {
			ll = (struct Str) { beg, buf->data };
			lr = (struct Str) { 0, buf->data };
		} else {
			ll = (struct Str) { buf->len_left, buf->data };
			lr = (struct Str) { beg - buf->len_left, buf->data + MAX_BYTES - buf->len_right };
		}
		if (end <= buf->len_left) {
			rl = (struct Str) { buf->len_left - end, buf->data + end };
			rr = (struct Str) { buf->len_right, buf->data + MAX_BYTES - buf->len_right };
		} else {
			rl = (struct Str) { 0, buf->data };
			uint16_t i = buf->len_right - (end - buf->len_left);
			rr = (struct Str) { i, buf->data + MAX_BYTES - i };
		}

		size_t new_len = 0; // The new length of this gap buffer
		struct Str segments[] = { ll, lr, s, rl, rr }, *x = segments;
		while (new_len + x->len <= MAX_BYTES && len - new_len - x->len >= MIN_BYTES)
			new_len += x++->len;
		size_t split_idx = MIN(x->len, MAX_BYTES - new_len);
		if (len - (new_len + split_idx) < MIN_BYTES)
			split_idx = len - new_len - MIN_BYTES;
		struct SplitStr split = str_split_at(*x, split_idx);
		new_len += split.left.len;
		*x = split.right;

		// Now x holds the new segments. Allocate new gap buffers.
		struct NodeSlice extras
			= segment_chunks(len - new_len, segments + LENGTH(segments) - x, x);
		if (!extras.xs) die("malloc failed");
		buf->len_left = x > segments ? ll.len : 0;
		buf->len_right = 0;
		for (struct Str *y = segments + 1; y < x; buf->len_left += y++->len)
			memmove(buf->data + buf->len_left, y->p, y->len);
		memmove(buf->data + buf->len_left, split.left.p, split.left.len);
		buf->len_left += split.left.len;
		return extras;
	}

	struct Internal *x = (struct Internal *) *node;
	Node **child = x->children;
	size_t n;
	while ((n = node_byte_size(*child)) < beg) ++child, beg -= n, end -= n;
	unsigned child_idx = child - x->children;
	count_child(x, -1, *child);
	struct NodeSlice extras = node_replace(child, beg, end, s, extras_offset);
	count_child(x, 1, *child);

	if (n < end) {
        /* No one child envelops the whole range. Proceed by:
         *
         *                   x
         *       (1) ___-   /|\   -___
         *          /______/ | \______\ (3)
         *         //        |   (4)  \\
         *        |x   (2)   x    ^   |x
         *       _/\\_   \   |\_/ | \_/\\_
         *      /   \ \   >  | /\   /\  \ \
         *     x     > x     x   . .     > x
         *             |<---edit-range---->|
         *
         * 1. Finding leftmost affected leaf and applying replacement
         *    for a list of extra leaves to smear out over edit range.
		 * 2. Substituting leaves left-to-right, removing remaining
		 *    subtrees upon exhausting the extra leaves.
		 * 3. Finding rightmost affected leaf and balancing it with
		 *    the last extra leaf if such remains, bubbling up
		 *    remaining extra leaves for insertion before found leaf.
		 * 4. Patching the seam created by a deletion, by balancing
		 *    nodes on the same levels of the rightmost and
		 *    penultimate rightmost subtrees bottom-up.
		 */
		end -= n;
		while (++child - x->children < x->num_children
			&& (n = node_byte_size(*child)) < end) end -= n;
		unsigned prev_num_children = x->num_children;
		replace_child_range_with_leaves(x, child_idx + 1, child - x->children,
			extras, extras_offset);
		child -= prev_num_children - x->num_children;

		if (child - x->children >= x->num_children) return extras;
		count_child(x, -1, *child);
		replace_nodes_in_end_subtree(child, end, &extras, extras_offset);
		count_child(x, 1, *child);

		unsigned end_idx = child - x->children;
		if (extras.len) // Insert new child nodes before the end node
			insert_children_overflowing(x, end_idx, &extras);
		else {
			fix_seam(x, end_idx - 1, x, end_idx);
			replace_with_child(node);
		}
		return extras;
	}

	if (extras.len) // There are new child nodes to insert after the child
		insert_children_overflowing(x, child_idx + 1, &extras);
	else if ((*child)->depth < (*node)->depth - 1) {
		// Child is at lower depth than its siblings and needs to be subsumed
		insert_at_depth(x, child_idx, remove_child(x, child_idx));
		replace_with_child(node);
	} else { // Child stayed at same depth but may be underfilled
		if (child_idx == 0) ++child, ++child_idx;
		if (balance(child[-1], *child)) {
			free(remove_child(x, child_idx));
			replace_with_child(node);
		}
	}
	return extras;
}

bool rope_init(struct Rope *rope) {
	struct Leaf *root;
	if (!(root = malloc(sizeof *root))) return false;
	root->node.depth = root->value.len_left = root->value.len_right = 0;
	rope->root = &root->node;
	return true;
}
#pragma GCC diagnostic pop

void rope_free(struct Rope *rope) { node_free(rope->root); }

static Node *node_from_nodes(Node *first, struct NodeSlice nodes) {
	if (!!first + nodes.len <= MAX_CHILDREN) {
		struct Internal *node;
		if (!(node = malloc(sizeof *node))) return NULL;
		*node = (struct Internal) { .node = { .depth = nodes.xs[0]->depth + 1 } };
		if (first) insert_children(node, 0, 1, &first);
		insert_children(node, !!first, nodes.len, nodes.xs);
		assert(node->num_children >= MIN_CHILDREN);
		return &node->node;
	}
	struct NodeSlice slices[] = { { !!first, &first }, nodes };
	make_parents(LENGTH(slices), slices, !!first + nodes.len, &nodes);
	return node_from_nodes(NULL, nodes);
}

size_t rope_size(struct Rope *rope) { return node_byte_size(rope->root); }

void rope_replace(struct Rope *rope, size_t beg, size_t end, size_t len, const char s[static len]) {
	assert(end <= rope_size(rope));
	size_t extras_offset = 0;
	struct NodeSlice extras = node_replace(&rope->root, beg, end,
		(struct Str) { len, s }, &extras_offset);
	if (extras.len && !(rope->root = node_from_nodes(rope->root, extras)))
		die("malloc failed");
	free(extras.xs);
}
