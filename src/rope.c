#include "rope.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "util.h"

#pragma GCC diagnostic ignored "-Wcast-align"

#ifdef ROPE_CHUNK_CAPACITY
#define MAX_BYTES ROPE_CHUNK_CAPACITY
#else
#define MAX_BYTES 1018
#endif
#define MIN_BYTES (MAX_BYTES / 4)

struct GapBuffer {
	uint16_t len_left, ///< Number of bytes left of the gap.
		len_right; ///< Number of bytes right of the gap.
	char data[MAX_BYTES];
};

/** Recenters the gap such that @a i bytes are to the left of it. */
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

static uint16_t gap_buffer_len(struct GapBuffer *x) { return x->len_left + x->len_right; }

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
	if (((struct Internal *) *node)->num_children != 1) return;
	Node *child = *((struct Internal *) *node)->children;
	free(*node);
	*node = child;
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
}

static void insert_children_count(struct Internal *node, unsigned i,
	unsigned len, Node *xs[static len]) {
	insert_children(node, i, len, xs);
	for (size_t i = 0; i < len; ++i) count_child(node, 1, xs[i]);
}

static Node *remove_child(struct Internal *parent, unsigned i) {
	Node *child = parent->children[i];
	memmove(parent->children + i, parent->children + i + 1,
		(--parent->num_children - i) * sizeof *parent->children);
	return child;
}

struct Str { size_t len; const char *p; };
struct NodeSlice { size_t len; Node **xs; };

static struct NodeSlice segment_chunks(size_t size, size_t count, struct Str segments[static count]) {
	size_t capacity = (size + MAX_BYTES - 1) / MAX_BYTES;
	Node **nodes, **p;
	if (!(p = nodes = malloc(capacity * sizeof *nodes))) return (struct NodeSlice) {};
	for (Node **node = nodes; node < nodes + capacity; ++node) {
		struct Leaf *leaf;
		if (!(leaf = malloc(sizeof *leaf))) {
			while (p < node) free(*p++);
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

static void make_parents(size_t count, struct NodeSlice xs[static count], size_t total, struct NodeSlice *out) {
	size_t len = 0;
	struct NodeSlice acc = { 0, (Node *[MAX_CHILDREN]) {} };
	for (unsigned i = 0; i < count; ++i) {
		struct NodeSlice x = xs[i];
		while (x.len) {
			// If the rest cannot fit then leave enough for a non-underfilled node
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
			*node = (struct Internal) { .node.depth = (*acc.xs)->depth + 1 };
			insert_children_count(node, 0, acc.len, acc.xs);
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
		insert_children_count(node, i, xs->len, xs->xs);
		xs->len = 0;
		return;
	}
	struct NodeSlice new = *xs;
#define SUB_SAT(a, b) ((a) < (b) ? 0 : (a) - (b))
	unsigned num_rchildren = node->num_children - i,
		num_lchildren = SUB_SAT(MIN_CHILDREN, new.len + num_rchildren);
	Node **rchildren = node->children + i, **lchildren = rchildren - num_lchildren;
	node->num_children -= num_lchildren + num_rchildren;
	for (unsigned j = 0; j < num_lchildren + num_rchildren; ++j)
		count_child(node, -1, node->children[node->num_children + j]);

	struct NodeSlice ys = { 0, (Node *[MIN_CHILDREN]) {} };
	while (node->num_children + ys.len < MIN_CHILDREN)
		ys.xs[ys.len++] = *(new.len ? (--new.len, new.xs++) : (--num_rchildren, rchildren++));

	struct NodeSlice slices[]
		= { { num_lchildren, lchildren }, new, { num_rchildren, rchildren } };
	make_parents(LENGTH(slices), slices, num_lchildren + new.len + num_rchildren, xs);
	insert_children_count(node, node->num_children, ys.len, ys.xs);
}

/** Balances @a a with its right sibling @a b.
 *
 * @return Whether @a b became empty.
 */
static bool balance(Node *restrict a, Node *restrict b) {
	assert(a->depth == b->depth);
	if (b->depth) {
		struct Internal *x = (struct Internal *) a, *y = (struct Internal *) b;
		if (y->num_children >= MIN_CHILDREN && x->num_children >= MIN_CHILDREN) ;
		else if (x->num_children + y->num_children <= MAX_CHILDREN) {
			insert_children_count(x, x->num_children, y->num_children, y->children);
			free(b);
			return true;
		} else if (x->num_children < y->num_children) { // Move child from b to a
			insert_children_count(x, x->num_children, 1, y->children);
			count_child(y, -1, remove_child(y, 0));
		} else { // Move child from a to b
			insert_children_count(y, 0, 1, x->children + x->num_children - 1);
			count_child(x, -1, remove_child(x, x->num_children - 1));
		}
	} else {
		struct GapBuffer *x = &((struct Leaf *) a)->value, *y = &((struct Leaf *) b)->value;
		size_t x_size = gap_buffer_len(x), y_size = gap_buffer_len(y);
		if (y_size >= MIN_BYTES && x_size >= MIN_BYTES) ;
		else if (x_size + y_size <= MAX_BYTES) {
			move_gap(x, x_size);
			memcpy(x->data + x_size, y->data, y->len_left);
			x->len_left += y->len_left;
			memcpy(x->data + MAX_BYTES - (x->len_right = y->len_right),
				y->data + MAX_BYTES - y->len_right, y->len_right);
			free(b);
			return true;
		} else if (x_size > y_size) { // Move bytes from x to y
			move_gap(x, x_size);
			size_t n = MIN_BYTES - y_size;
			memmove(y->data + n, y->data, y->len_left); // Make room
			memcpy(y->data, x->data + (x->len_left -= n), n);
			y->len_left += n;
		} else { // Move bytes from y to x
			size_t n = MIN_BYTES - x_size;
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
	count_child(parent, 1, x);
	Node *last = parent->children[parent->num_children - 1];
	if (x->depth + 1 < parent->node.depth) {
		if (!(x = append_child((struct Internal *) last, x))) return NULL;
	} else if (balance(last, x)) return NULL;
	if (parent->num_children < MAX_CHILDREN) {
		insert_children(parent, parent->num_children, 1, &x);
		return NULL;
	}
	if (!parent->node.depth) unreachable();
	struct Internal *new;
	if (!(new = malloc(sizeof *new))) die("malloc failed");
	remove_child(parent, MAX_CHILDREN - 1);
	*new = (struct Internal) { .node.depth = parent->node.depth };
	insert_children_count(new, 0, 2, (Node *[]) { last, x });
	count_child(parent, -1, &new->node);
	return &new->node;
}

static Node *prepend_child(struct Internal *parent, Node *x) {
	count_child(parent, 1, x);
	Node *first = *parent->children;
	if (x->depth + 1 < parent->node.depth) {
		if (!(x = prepend_child((struct Internal *) first, x))) return NULL;
	} else if (balance(x, first)) { *parent->children = x; return NULL; }
	if (parent->num_children < MAX_CHILDREN) {
		insert_children(parent, 0, 1, &x);
		return NULL;
	}
	if (!parent->node.depth) unreachable();
	struct Internal *new;
	if (!(new = malloc(sizeof *new))) die("malloc failed");
	remove_child(parent, 0);
	*new = *parent;
	*parent = (struct Internal) { .node.depth = parent->node.depth };
	insert_children_count(parent, 0, 2, (Node *[]) { x, first });
	count_child(new, -1, &parent->node);
	return &new->node;
}

static void insert_at_depth(struct Internal *parent, unsigned i) {
	Node *x = remove_child(parent, i),
		*new = i ? append_child((struct Internal *) parent->children[i - 1], x)
		: prepend_child((struct Internal *) *parent->children, x);
	if (new) insert_children(parent, i, 1, &new);
}

/** Patches deletion seam between child @a l of @a x and @a j:th child of @a y.
 *
 * The pointers @a x and @a y may alias.
 * @post No child of @a x or @a y is underfilled.
 */
static void fix_seam(struct Internal *x, Node *l, struct Internal *y, unsigned j, size_t end) {
	Node **child = y->children + j, **child_end = child;
	for (size_t n; (n = node_byte_size(*child_end)) < end; ++child_end, end -= n) {
		count_child(y, -1, *child_end);
		node_free(*child_end);
	}
	memmove(child, child_end, (char *) (y->children + y->num_children) - (char *) child_end);
	y->num_children -= child_end - child;

	Node *r = *child;
	count_child(y, -1, r);
	if (l->depth < r->depth) {
		fix_seam(x, l, (struct Internal *) r, 0, end);
		if (!((struct Internal *) r)->byte_size) remove_child(y, j);
		else { count_child(y, 1, r); replace_with_child(child); }
		return;
	}
	count_child(x, -1, l);
	if (r->depth) {
		struct Internal *l2 = (struct Internal *) l;
		fix_seam(l2, l2->children[l2->num_children - 1], (struct Internal *) r, 0, end);
	} else { // r is the rightmost leaf affected by the replacement
		struct GapBuffer *buf = &((struct Leaf *) r)->value;
		move_gap(buf, end);
		buf->len_left = 0;
	}
	if (balance(l, r)) remove_child(y, j); // Remove r as it became empty
	else count_child(y, 1, r);
	count_child(x, 1, l);
}

static struct NodeSlice node_replace(Node **node, size_t beg, size_t end, struct Str s) {
	assert(end <= node_byte_size(*node));
	if (!(*node)->depth) {
		struct GapBuffer *buf = &((struct Leaf *) *node)->value;
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
		size_t split_idx, n = 0; // The new length of this gap buffer
		struct Str segments[] = { ll, lr, s, rl, rr }, *p = segments;
		while (p->len <= (split_idx = MIN(MAX_BYTES - n, len - n - MIN_BYTES)))
			n += p++->len;
		n += split_idx;
		struct Str last = { split_idx, p->p };
		*p = (struct Str) { p->len - split_idx, p->p + split_idx };

		// Now p holds the new segments: Allocate new leaves
		struct NodeSlice extras
			= segment_chunks(len - n, segments + LENGTH(segments) - p, p);
		if (!extras.xs) die("malloc failed");
		buf->len_left = p > segments ? ll.len : 0;
		for (struct Str *q = segments + 1; q < p; buf->len_left += q++->len)
			memmove(buf->data + buf->len_left, q->p, q->len);
		memmove(buf->data + MAX_BYTES - (buf->len_right = last.len), last.p, last.len);
		return extras;
	}

	struct Internal *x = (struct Internal *) *node;
	Node **child = x->children;
	size_t n;
	while ((n = node_byte_size(*child)) < beg) { ++child; beg -= n; end -= n; }
	count_child(x, -1, *child);
	struct NodeSlice extras = node_replace(child, beg, MIN(end, n), s);
	count_child(x, 1, *child);
	unsigned child_idx = child - x->children;

	if (n < end) {
		/* No one child envelops the whole range. Proceed by:
		 *
		 *                   x
		 *       (1) ____/ / | \ \____
		 *          /_____/  |  \_____\ (3)
		 *         v/        |   (4)  \\
		 *         x   (2)---x->  ^   |x
		 *       _/ \_     _/|\_/ | \_/\\_
		 *      /     \   /  | /\ | /\  \ \
		 *     x       x x   x   . .     > x
		 *             |<---edit-range---->|
		 *
		 * 1. Finding leftmost affected child and applying replacement
		 *    for a list of extra nodes to smear out over edit range.
		 * 2. Substituting nodes left-to-right, removing remaining
		 *    subtrees upon exhausting the extra nodes.
		 * 3. Finding the rightmost affected leaf, deleting everything
		 *    left of the end of the edit.
		 * 4. Patching the seam created by a deletion, by balancing
		 *    nodes on the same levels of the rightmost and
		 *    penultimate rightmost subtrees bottom-up.
		 */
		end -= n;
		size_t extras_idx = 0;
		for (; extras_idx < extras.len && (n = node_byte_size(child[1])) < end; end -= n) {
			++child_idx;
			count_child(x, -1, *++child);
			node_free(*child);
			count_child(x, 1, *child = extras.xs[extras_idx++]);
		}
		if (extras_idx) // Compact extras to discharge extras_idx
			memmove(extras.xs, extras.xs + extras_idx,
				(extras.len -= extras_idx) * sizeof *extras.xs);

		// With to-be-inserted extras, balance their right-edge instead
		Node *l = extras.len ? extras.xs[extras.len - 1] : *child;
		fix_seam(extras.len ? &(struct Internal) {} : x, l, x, child_idx + 1, end);
		// r may be at lower depth if l was too (due to deletion)
		if (child_idx + 1 < x->num_children && child[1]->depth + 1 < x->node.depth) {
			if (child[0]->depth == child[1]->depth) {
				struct NodeSlice xs = { 2, child };
				make_parents(1, &xs, 2, &xs);
				remove_child(x, child_idx + 1);
			} else
				insert_at_depth(x, child_idx + (child[0]->depth > child[1]->depth));
		}
		if (x->num_children == 1 && !extras.len) { replace_with_child(node); return /* empty */ extras; }
	}

	if (extras.len) // There are new child nodes to insert after the child
		insert_children_overflowing(x, child_idx + 1, &extras);
	else if ((*child)->depth < (*node)->depth - 1) {
		// Child is at lower depth than its siblings and needs to be subsumed
		insert_at_depth(x, child_idx);
		replace_with_child(node);
	} else { // Child stayed at same depth but may be underfilled
		if (child_idx == 0) ++child, ++child_idx;
		if (balance(child[-1], *child)) {
			remove_child(x, child_idx);
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

void rope_free(struct Rope *rope) { node_free(rope->root); }

size_t rope_size(struct Rope *rope) { return node_byte_size(rope->root); }

void rope_replace(struct Rope *rope, size_t beg, size_t end, size_t len, const char s[static len]) {
	struct NodeSlice extras = node_replace(&rope->root, beg, end, (struct Str) { len, s });
	if (extras.len) {
		struct NodeSlice xs[] = { { 1, &rope->root }, extras };
		do make_parents(LENGTH(xs), xs, xs->len + xs[1].len, xs + 1);
		while (xs->len = 0, xs[1].len > 1);
		rope->root = *xs[1].xs;
	}
	free(extras.xs);
}
