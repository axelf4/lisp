#include "rope.h"
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
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

/** Recenter the gap such that @arg i bytes are to the left of it. */
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
 * The following invariants are maintained:
 *
 * 1. Internal nodes have between MIN_CHILDREN and MAX_CHILDREN
 *    children, except the root which may have as few as two.
 * 2. All leaves appear at the same level.
 * 3. Leaf nodes contain at most MAX_BYTES and generally at least
 *    MIN_BYTES bytes. */
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

static void replace_with_child(Node **node) {
	if (((struct Internal *) *node)->num_children == 1) {
		Node *child = *((struct Internal *) *node)->children;
		free(*node);
		*node = child;
	}
}

static inline void count_child(struct Internal *parent, int sign, Node *child) {
	parent->byte_size += sign * node_byte_size(child);
}

static inline void insert_children(struct Internal *node, unsigned i,
	unsigned len, Node *xs[static len]) {
	memmove(node->children + i + len, node->children + i,
		(node->num_children - i) * sizeof *node->children);
	memcpy(node->children + i, xs, len * sizeof *xs);
	node->num_children += len;
	for (Node **x = xs; x < xs + len; ++x) count_child(node, 1, *x);
}

static inline void remove_children(struct Internal *parent, unsigned i, unsigned len) {
	for (Node **x = parent->children + i; x < parent->children + i + len; ++x)
		count_child(parent, -1, *x);
	memmove(parent->children + i, parent->children + i + len,
		((parent->num_children -= len) - i) * sizeof *parent->children);
}

static inline Node *remove_child(struct Internal *parent, unsigned i) {
	Node *child = parent->children[i];
	remove_children(parent, i, 1);
	return child;
}

struct NodeSlice { size_t len; Node **xs; };

static size_t make_parents(size_t count, struct NodeSlice xs[static count], size_t total, Node **out) {
	size_t out_len = 0;
	unsigned acc_len = 0;
	Node *acc[MAX_CHILDREN];
	for (struct NodeSlice *x = xs; x < xs + count; ++x) do {
			unsigned n = MIN(x->len, MAX_CHILDREN - acc_len);
			// If cannot fit the rest, then leave enough for a non-underfilled node
			if (total > MAX_CHILDREN - acc_len) n = MIN(n, total - MIN_CHILDREN);

			memcpy(acc + acc_len, x->xs, n * sizeof *acc);
			acc_len += n;
			total -= n;
			x->len -= n;
			x->xs += n;

			if (acc_len >= MAX_CHILDREN || (total <= MIN_CHILDREN && MAX_CHILDREN - acc_len < total)) {
				struct Internal *node;
				if (!(node = malloc(sizeof *node))) die("malloc failed");
				*node = (struct Internal) { .node = { .depth = (*acc)->depth + 1 } };
				insert_children(node, 0, acc_len, acc);
				out[out_len++] = &node->node;
				acc_len = 0;
			}
	} while (x->len);
	return out_len;
}

/** Inserts the children in @arg new at index @arg i of @arg node.
 *
 * @return Number of extra siblings outputted into @arg new to insert after @arg node. */
static size_t insert_children_overflowing(struct Internal *node, unsigned i, struct NodeSlice new) {
	if (node->num_children + new.len <= MAX_CHILDREN) {
		insert_children(node, i, new.len, new.xs);
		return 0;
	}
	unsigned rchildren_len = node->num_children - i,
		lchildren_len = MAX(MIN_CHILDREN - (new.len + rchildren_len), 0);
	Node *lchildren[lchildren_len], **out = new.xs,
		*rchildren_array[rchildren_len], **rchildren = rchildren_array;
	memcpy(rchildren, node->children + i, sizeof rchildren_array);
	remove_children(node, i, rchildren_len);
	if (lchildren_len) {
		memcpy(lchildren, node->children + i - lchildren_len, sizeof lchildren);
		remove_children(node, i - lchildren_len, lchildren_len);
	} else while (node->num_children < MIN_CHILDREN)
			   insert_children(node, node->num_children, 1,
				   new.len ? (--new.len, new.xs++) : (--rchildren_len, rchildren++));
	struct NodeSlice slices[]
		= { { lchildren_len, lchildren }, new, { rchildren_len, rchildren } };
	return make_parents(LENGTH(slices), slices, lchildren_len + new.len + rchildren_len, out);
}

/** Balances @arg a with its right sibling @arg b.
 *
 * @return Whether @arg b became empty. */
static bool balance(Node *a, Node *b) {
	if (a->depth != b->depth) __builtin_unreachable();
	if (!(is_underfilled(b) || is_underfilled(a))) ;
	else if (!b->depth) {
		struct GapBuffer *x = &((struct Leaf *) a)->value, *y = &((struct Leaf *) b)->value;
		unsigned x_size = gap_buffer_len(x), y_size = gap_buffer_len(y);
		if (x_size + y_size <= MAX_BYTES) {
			move_gap(x, x_size);
			memcpy(x->data + x_size, y->data, y->len_left);
			memcpy(x->data + (x->len_left += y->len_left),
				y->data + MAX_BYTES - y->len_right, y->len_right);
			x->len_left += y->len_right;
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
			memmove(x->data + MAX_BYTES - x->len_right - n,
				x->data + MAX_BYTES - x->len_right, n); // Make room
			x->len_right += n;

			unsigned m = MIN(n, y->len_left);
			memcpy(x->data + MAX_BYTES - n, y->data, m);
			memmove(y->data, y->data + m, y->len_left -= m);

			n -= m;
			memcpy(x->data + MAX_BYTES - n, y->data + MAX_BYTES - y->len_right, n);
			y->len_right -= n;
		}
	} else {
		struct Internal *x = (struct Internal *) a, *y = (struct Internal *) b;
		if (x->num_children + y->num_children <= MAX_CHILDREN) {
			insert_children(x, x->num_children, y->num_children, y->children);
			remove_children(y, 0, y->num_children);
			return true;
		} else if (x->num_children > y->num_children) { // Move children from a to b
			unsigned n = MIN_CHILDREN - y->num_children;
			insert_children(y, 0, n, x->children + x->num_children - n);
			remove_children(x, x->num_children - n, n);
		} else { // Move children from b to a
			unsigned n = MIN_CHILDREN - x->num_children;
			if (n != 1) __builtin_unreachable();
			insert_children(x, x->num_children, n, y->children);
			remove_children(y, 0, n);
		}
	}
	return false;
}

static Node *append_child(struct Internal *parent, Node *x) {
	if (x->depth + 1 < parent->node.depth) {
		Node *last = parent->children[parent->num_children - 1];
		count_child(parent, -1, last);
		x = append_child((struct Internal *) last, x);
		count_child(parent, 1, last);
		if (!x) return NULL;
	} else if (balance(parent->children[parent->num_children - 1], x)) {
		free(x);
		return NULL;
	}
	if (parent->num_children < MAX_CHILDREN) {
		insert_children(parent, parent->num_children, 1, &x);
		return NULL;
	}
	struct Internal *new;
	if (!(new = malloc(sizeof *new))) die("malloc failed");
	Node *last = remove_child(parent, MAX_CHILDREN - 1);
	*new = (struct Internal) { .node = { .depth = parent->node.depth } };
	insert_children(new, 0, 2, (Node *[]) { last, x });
	return &new->node;
}

static Node *prepend_child(struct Internal *parent, Node *x) {
	if (x->depth + 1 < parent->node.depth) {
		Node *first = *parent->children;
		count_child(parent, -1, first);
		x = prepend_child((struct Internal *) first, x);
		count_child(parent, 1, first);
		if (!x) return NULL;
	} else if (balance(x, *parent->children)) {
		free(*parent->children);
		*parent->children = x;
		return NULL; 
	}
	if (parent->num_children < MAX_CHILDREN) {
		insert_children(parent, 0, 1, &x);
		return NULL;
	}
	struct Internal *new;
	if (!(new = malloc(sizeof *new))) die("malloc failed");
	Node *first = remove_child(parent, 0);
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
	for (struct Str *s = segments; s < segments + count; ++s) for (; s->len; ++p) {
			struct GapBuffer *acc = &((struct Leaf *) *p)->value;
			unsigned n = MIN(s->len, MAX_BYTES);
			if (size > (size_t) MAX_BYTES - acc->len_left) n = MIN(n, size - MIN_BYTES);
			memcpy(acc->data + acc->len_left, s->p, n);
			acc->len_left += n;
			size -= n;
			s->len -= n;
			s->p += n;
	}
	return (struct NodeSlice) { capacity, nodes };
}

/** Returns the number of children removed before @arg child_end. */
static unsigned replace_child_range_with_leaves(struct Internal *node, unsigned child_start, unsigned child_end,
	struct NodeSlice extra_leaves, size_t *extra_leaves_offset) {
	for (unsigned i = child_start; i < child_end; ++i) {
		if (!(extra_leaves.len - *extra_leaves_offset)) {
			unsigned n = node->num_children - i;
			remove_children(node, i, n);
			return n;
		}
		Node **child = node->children + i;
		count_child(node, -1, *child);
		if ((*child)->depth)
			replace_child_range_with_leaves(((struct Internal *) *child),
				0, ((struct Internal *) *child)->num_children,
				extra_leaves, extra_leaves_offset);
		else {
			free(*child);
			*child = extra_leaves.xs[(*extra_leaves_offset)++];
		}
		count_child(node, 1, *child);
	}
	return 0;
}

static struct NodeSlice node_replace(Node **node, size_t beg, size_t end, struct Str s);

static struct NodeSlice replace_nodes_in_start_subtree(Node *node,
	size_t beg, struct Str s, size_t *extra_leaves_offset) {
	if (!node->depth) // Leaf node
		return node_replace(&node, beg, node_byte_size(node), s);

	struct Internal *x = (struct Internal *) node;
	Node **child = x->children;
	for (size_t n; (n = node_byte_size(*child)) < beg; beg -= n) ++child;
	unsigned child_idx = child - x->children;

	struct NodeSlice extra_leaves = replace_nodes_in_start_subtree(*child,
		beg, s, extra_leaves_offset);
	replace_child_range_with_leaves(x, child_idx + 1, x->num_children,
		extra_leaves, extra_leaves_offset);
	return extra_leaves;
}

static void replace_nodes_in_end_subtree(Node *node, size_t end,
	struct NodeSlice *extras, size_t *extra_leaves_offset) {
	if (node->depth) { // Internal node
		struct Internal *x = (struct Internal *) node;
		Node **child = x->children;
		for (size_t n; (n = node_byte_size(*child)) < end; end -= n) ++child;
		child -= replace_child_range_with_leaves(x, 0, child - x->children,
			*extras, extra_leaves_offset);

		replace_nodes_in_end_subtree(*child, end, extras, extra_leaves_offset);
		extras->len = insert_children_overflowing(x, x->children - child, *extras);
		return;
	}

	// This is the rightmost leaf affected by the replacement
	struct GapBuffer *buf = &((struct Leaf *) node)->value;
	move_gap(buf, end);
	memmove(buf->data, buf->data + end, buf->len_left - end);
	buf->len_left -= end;
	if (extras->len - *extra_leaves_offset && balance(extras->xs[extras->len - 1], node))
		*node = *extras->xs[--extras->len];

	if (extras->len) // Compact the extra nodes list to discharge extra_leaves_offset
		memmove(extras->xs, extras->xs + *extra_leaves_offset,
			extras->len -= *extra_leaves_offset);
}

/**
 * Patches deletion seam between @arg i:th child of @arg x and @arg j:th child of @arg y.
 *
 * The pointers @arg x and @arg y may alias.
 *
 * @post No child of @arg x or @arg y is underfilled or a different height.
 * @return Whether @arg x needs to be replaced with its sole
 *         unbalanced child, and @arg y now has @arg j children.
 */
static bool fix_seam(struct Internal *x, unsigned i, struct Internal *y, unsigned j) {
	Node *left = x->children[i], *right = y->children[j];
	if (right->depth
		&& fix_seam((struct Internal *) left, ((struct Internal *) left)->num_children - 1,
				(struct Internal *) right, 0)) {
		remove_child(y, j); // Remove right as it became empty
		replace_with_child(x->children + i);
		// left was replaced by its child: Insert it at the right depth
		struct Internal *parent;
		unsigned insert_idx;
		if (x->num_children > 1) { parent = x; insert_idx = i; }
		else if (y != x && y->num_children) { parent = y; insert_idx = 0; }
		else return true;
		insert_at_depth(parent, insert_idx, remove_child(x, i));
	} else if (balance(left, right)) {
		free(remove_child(y, j));
		if (!is_underfilled(left)) ;
		else if (i) {
			if (balance(x->children[i - 1], left)) free(remove_child(x, i));
		} else if (j < y->num_children) {
			if (balance(left, y->children[j])) free(remove_child(y, j));
		} else return true;
	}
	return false;
}

static struct NodeSlice node_replace(Node **node, size_t beg, size_t end, struct Str s) {
	assert(end <= node_byte_size(*node));
	if (!(*node)->depth) { // Leaf node
		struct GapBuffer *buf = &((struct Leaf *) *node)->value;
		if (s.len <= MAX_BYTES - gap_buffer_len(buf) - (end - beg)) {
			move_gap(buf, end);
			memcpy(buf->data + (buf->len_left -= end - beg), s.p, s.len);
			buf->len_left += s.len;
			return (struct NodeSlice) {};
		}

		struct Str ll, lr, // Text left of replacement, left/right of gap respectively,
			rl, rr; // ... and right of replacement.
		if (beg <= buf->len_left) {
			ll = (struct Str) { beg, buf->data };
			lr = (struct Str) { 0, "" };
		} else {
			ll = (struct Str) { buf->len_left, buf->data };
			uint16_t i = beg - buf->len_left;
			lr = (struct Str) { i - buf->len_right, buf->data + MAX_BYTES - i };
		}
		if (end <= buf->len_left) {
			rl = (struct Str) { buf->len_left - end, buf->data + end };
			rr = (struct Str) { buf->len_right, buf->data + MAX_BYTES - buf->len_right };
		} else {
			rl = (struct Str) { 0, "" };
			uint16_t i = end - buf->len_left;
			rr = (struct Str) { i - buf->len_right, buf->data + MAX_BYTES - i };
		}

		size_t len = gap_buffer_len(buf) - (end - beg) + s.len,
			new_len = 0; // The new length of this gap buffer
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
		buf->len_left = (x > segments ? ll.len : 0) + (buf->len_right = 0);
		for (struct Str *y = segments + 1; y < x; buf->len_left += y++->len)
			memcpy(buf->data + buf->len_left, y->p, y->len);
		memcpy(buf->data + buf->len_left, split.left.p, split.left.len);
		buf->len_left += split.left.len;
		return extras;
	}

	struct Internal *x = (struct Internal *) *node;
	// Find a child that envelops the whole range
	Node **child = x->children;
	for (size_t n; (n = node_byte_size(*child)) < beg; beg -= n, end -= n) ++child;
	unsigned child_idx = child - x->children;
	if (node_byte_size(*child) < end) {
        /* Here node is the deepest node that contains the whole
		 * range. Proceed by:
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
		 *    penultimate rightmost subtrees bottom-up. */
		unsigned start_idx = child_idx;
		end -= node_byte_size(*child);
		size_t extra_leaves_offset = 0;
		count_child(x, -1, *child);
		struct NodeSlice extras
			= replace_nodes_in_start_subtree(*child, beg, s, &extra_leaves_offset);
		count_child(x, 1, *child);

		for (size_t n; (n = node_byte_size(*++child)) < end; end -= n) ;
		child -= replace_child_range_with_leaves(x, start_idx + 1, child - x->children,
			extras, &extra_leaves_offset);

		unsigned end_idx = child - x->children;
		count_child(x, -1, *child);
		replace_nodes_in_end_subtree(*child, end, &extras, &extra_leaves_offset);
		count_child(x, 1, *child);

		if (extras.len) // Insert new child nodes before the end node
			extras.len = insert_children_overflowing(x, end_idx, extras);
		else {
			fix_seam(x, end_idx - 1, x, end_idx);
			replace_with_child(node);
		}
		return extras;
	}

	count_child(x, -1, *child);
	struct NodeSlice extras = node_replace(child, beg, end, s);
	count_child(x, 1, *child);
	if (extras.len) // There are new child nodes to insert after the child
		extras.len = insert_children_overflowing(x, child_idx + 1, extras);
	else if ((*child)->depth < (*node)->depth - 1) {
		// Child is at lower depth than its siblings and needs to be subsumed
		insert_at_depth(x, child_idx, remove_child(x, child_idx));
		replace_with_child(node);
	} else { // Child stayed at same depth but may be underfilled and in need of rebalancing
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
	root->node.depth = 0;
	root->value.len_left = root->value.len_right = 0;
	rope->root = &root->node;
	return true;
}

static void node_free(Node *node) {
	if (node->depth)
		for (unsigned i = 0; i < ((struct Internal *) node)->num_children; ++i)
			node_free(((struct Internal *) node)->children[i]);
	free(node);
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
	nodes.len = make_parents(LENGTH(slices), slices, !!first + nodes.len, nodes.xs);
	return node_from_nodes(NULL, nodes);
}

void rope_replace(struct Rope *rope, size_t beg, size_t end, const char *text) {
	struct Str s = { strlen(text), text };
	struct NodeSlice extras = node_replace(&rope->root, beg, end, s);
	if (extras.len && !(rope->root = node_from_nodes(rope->root, extras)))
		die("malloc failed");
	free(extras.xs);
}

size_t rope_size(struct Rope *rope) { return node_byte_size(rope->root); }
