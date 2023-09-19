#include <stdio.h>
#include "gc.h"

int main(void) {
	gc_init();

	void *test = myalloc(64);
	garbage_collect();

	printf("Allocated: %p\n", test);
}
