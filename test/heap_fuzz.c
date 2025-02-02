/** Expert-guided heap fuzzing.
 *
 * @see POLITO, Guillermo, et al. Heap fuzzing: Automatic garbage
 *      collection testing with expert-guided random events. In: 2023
 *      IEEE Conference on Software Testing, Verification and
 *      Validation (ICST). IEEE, 2023. p. 107-116.
 */

#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <time.h>
#include <sys/time.h>
#include "gc.c"
#include "util.c"

#include "gc.h"

#define MAX_EVENTS 1024 ///< Number of events limit per scenario.

#ifdef __SANITIZE_ADDRESS__
#include <sanitizer/asan_interface.h>

static void sanitizer_death_callback() {
	fprintf(stderr, "In custom handler!\n");
}
#endif

static uint64_t rng(unsigned _BitInt(128) *state) {
	// 64-bit Lehmer's generator
	*state *= 0xda942042e4dd58b5;
	return *state >> 64;
}

// TODO Need a good test oracle:
//
// Iterates all heap entities (objects, free memory chunks) and
// validates invariants such as ensuring that their classes are
// correct, that their slots point to valid objects, and that old
// objects refer to young objects are remembered

// For mutation events: Need to track two previous objects to create
// an edge between.

// Want to hook into tracing: Add each traced object to list of live objects.

struct Ctx {
	size_t num_objects;
#define MAX_LIVE_OBJECTS /* each event allocates at most 1 object */ MAX_EVENTS
	struct GcRef (*objects)[MAX_LIVE_OBJECTS];
};

struct Obj {
	alignas(GC_MIN_ALIGNMENT) struct GcObjectHeader hdr;
	size_t size;
	char data[];
};

void gc_object_visit(struct GcHeap *heap, void *p) {
	struct Obj *obj = p;
	gc_mark(sizeof *obj, p);
}

size_t gc_object_size(void *p, size_t *alignment) {
	struct Obj *obj = p;
	*alignment = alignof(struct Obj);
	return sizeof *obj + obj->size;
}

void gc_trace_roots(struct GcHeap *heap) {}

enum Event {
	EVENT_ALLOC,
	EVENT_GC,
	NUM_EVENTS
};

static volatile sig_atomic_t should_stop;

static void signal_handler(int sig) {
	switch (sig) {
	case SIGALRM: should_stop = true; break;
	default: unreachable();
	}
}

int main(void) {
#ifdef __SANITIZE_ADDRESS__
	__sanitizer_set_death_callback(sanitizer_death_callback);
#endif

	if (signal(SIGALRM, signal_handler) == SIG_ERR) die("signal failed");

	setitimer(ITIMER_REAL, &(const struct itimerval) {
			.it_value.tv_usec = /* 0.75s */ 750'000
		}, NULL);

	struct GcRef (*objects)[MAX_LIVE_OBJECTS];
	if (!(objects = malloc(sizeof *objects))) goto err; // malloc to avoid rooting

	unsigned _BitInt(128) seed = time(NULL), state = seed;

	struct GcHeap *heap;
	unsigned int scenario_count;
	for (scenario_count = 0; !should_stop; ++scenario_count) {
		if (!(heap = gc_new())) goto err_free_objects;
		struct Ctx *ctx = (struct Ctx *) heap;
		*ctx = (struct Ctx) {
			.objects = objects,
		};

		for (unsigned i = MAX_EVENTS; i--;) {
			switch (rng(&state) % NUM_EVENTS) {
			case EVENT_ALLOC:
				struct Obj *obj;
				if (!(obj = gc_alloc(heap, alignof(struct Obj), sizeof *obj)))
					goto err_free_heap;
				(*objects)[ctx->num_objects] = GC_COMPRESS(obj);
				break;

			case EVENT_GC:
				garbage_collect(heap);
				break;
			}
		}

		gc_free(heap);
	}

	printf("ran %u scenarios\n", scenario_count);
		
	free(objects);
	return 0;
err_free_heap:
	gc_free(heap);
err_free_objects:
	free(objects);
err: return EXIT_FAILURE;
}
