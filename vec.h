#include <stdlib.h>
#include <stdio.h>

struct Vec {
	size_t length, capacity;
	void **items;
};

static inline struct Vec vec_new() {
	return (struct Vec) { 0, 0, NULL, };
}

static void vec_push(struct Vec *vec, void *x) {
	if (vec->length >= vec->capacity) {
		void **items;
		size_t new_capacity = vec->capacity ? 2 * vec->capacity : 2;
		if (!(items = realloc(vec->items, new_capacity * sizeof *items))) {
			printf("realloc failed\n");
			return;
		}
		vec->items = items;
		vec->capacity = new_capacity;
	}
	vec->items[vec->length++] = x;
}

static inline void vec_free(struct Vec *vec) {
	free(vec->items);
}
