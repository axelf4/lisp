#include <stdio.h>
#include "gc.h"

void foo() {
	for (int i = 0; i < 2000; ++i) {
		void *p = gc_alloc(64);
		printf("foo alloc:ed: %p\n", p);
	}
}

int main(void) {
	gc_init();

	void *p = gc_alloc(64);
	printf("Allocated: %p\n", p);

	printf("Collecting garbage...\n");
	foo();
	garbage_collect();

	printf("Again: %p\n", p);
}
