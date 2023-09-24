#include <stddef.h>
#include <stdarg.h>
#include <setjmp.h>
#include <cmocka.h>
#include "gc.h"
#include "lisp.h"
#include "util.h"

void test1(void **) {
	assert_non_null(cons(NULL, NULL));
}

struct Key {
	unsigned key;
	void *value;
};

static uint64_t myhash(struct Key key) { return fxhash64(0, key.key); }
static bool myequal(struct Key a, struct Key b) { return a.key == b.key; }

#define NAME my
#define KEY struct Key
#define TYPE HashTable
#define KEY_HASH myhash
#define KEY_EQUAL myequal
#include "tbl.h"

void test_hash_table(void **) {
	struct HashTable table = my_tbl_new();
	my_tbl_insert(&table, (struct Key) { 42, &table });

	assert_ptr_equal(my_tbl_find(table, (struct Key) { .key = 42 })->value, &table);
	assert_null(my_tbl_find(table, (struct Key) { .key = 1337 }));

	my_tbl_free(table);
}

int main(void) {
	if (!(heap = gc_new())) return 1;

	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test1),
		cmocka_unit_test(test_hash_table),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
