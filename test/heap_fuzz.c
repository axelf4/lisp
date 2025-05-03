/** Expert-guided heap fuzzing.
 *
 * As test oracle, AddressSanitizer (ASan) is used, together with a
 * memory consistency checker: After each GC event, invariants, such
 * as all object slots pointing to valid objects, are validated.
 *
 * The event generator uses a list of live objects updated after each
 * GC. Predicting the exact object collection would require
 * reimplementing GC semantics.
 *
 * @see POLITO, Guillermo, et al. Heap fuzzing: Automatic garbage
 *      collection testing with expert-guided random events. In: 2023
 *      IEEE Conference on Software Testing, Verification and
 *      Validation (ICST). IEEE, 2023. p. 107-116.
 */

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <inttypes.h>
#include <signal.h>
#include <time.h>
#include <sys/time.h>
#include "fxhash.h"
#include "gc.c"
#include "util.c"

#ifdef __SANITIZE_ADDRESS__
#include <sanitizer/asan_interface.h>
#endif

#define MAX_EVENTS 1024 ///< Number of events limit per scenario.

/** Approximates Gaussian sampling using POPCNT.
 *
 * @see http://marc-b-reynolds.github.io/distribution/2021/03/18/CheapGaussianApprox.html
 */
static double rnorm(uint64_t u) {
	// The sum of the bits of @a u is binomially distributed (n=64, p=1/2)
	int64_t x = (((uint64_t) stdc_count_ones(u) - 32) << 32)
		+ (uint32_t) u - (u >> 32);
	return /* σ⁻¹ = 1/sqrt(64*0.5*(1-0.5) + 2/12) = */ 0x1.fb760cp-35 * x;
}

struct Obj {
	alignas(GC_ALIGNMENT) struct {
		struct GcObjectHeader hdr;
		uint8_t used_slot_count;
		uint16_t num_slots;
	};
	struct Obj *slots[];
};

static size_t object_size(struct Obj *obj) {
	return sizeof *obj + obj->num_slots * sizeof *obj->slots;
}

struct Ctx {
	unsigned num_objects, num_roots;
#define MAX_LIVE_OBJECTS /* each event allocates at most 1 object */ MAX_EVENTS
	struct GcRef (*objects)[MAX_LIVE_OBJECTS];
};

void gc_object_visit(struct GcHeap *heap, void *p) {
	struct Ctx *ctx = (struct Ctx *) heap;
	(*ctx->objects)[ctx->num_objects++] = GC_COMPRESS(p); // Record live object

	struct Obj *obj = p;
	gc_mark(object_size(obj), p);
	for (unsigned i = 0; i < obj->used_slot_count; ++i)
		obj->slots[i] = gc_trace(heap, obj->slots[i]);
}

size_t gc_object_size(void *p, size_t *alignment) {
	*alignment = alignof(struct Obj);
	return object_size(p);
}

void gc_trace_roots(struct GcHeap *heap) {
	struct Ctx *ctx = (struct Ctx *) heap;
	ctx->num_objects = ctx->num_roots; // Start of GC: Reset live objects

	for (unsigned i = 0; i < ctx->num_roots; ++i)
		(*ctx->objects)[i] = GC_COMPRESS(gc_trace(heap,
				(void *) GC_DECOMPRESS(heap, (*ctx->objects)[i])));
}

static bool is_obj_valid(struct Obj *x, bool mark_color) {
	return (x->hdr.flags & GC_MARK) == mark_color;
}

static bool validate_heap(struct GcHeap *heap) {
	struct Ctx *ctx = (struct Ctx *) heap;
	bool mark_color = heap->mark_color ^ !heap->is_major_gc;
	for (unsigned i = 0; i < ctx->num_objects; ++i) {
		struct Obj *obj = (struct Obj *) GC_DECOMPRESS(heap, (*ctx->objects)[i]);

		if (!is_obj_valid(obj, mark_color)) return false;
		for (unsigned j = 0; j < obj->used_slot_count; ++j)
			if (!is_obj_valid(obj->slots[j], mark_color)) return false;
	}
	return true;
}

enum Event {
	EVENT_ALLOC,
	EVENT_MUTATE, ///< Creates an edge between to objects.
	EVENT_GC
};

static enum Event gen_event_type(uint8_t u) {
	return u < 0.4 * UINT8_MAX ? EVENT_ALLOC
		: u < 0.8 * UINT8_MAX ? EVENT_MUTATE
		: EVENT_GC;
}

static size_t gen_obj_data_size(uint64_t u) {
	size_t max = sizeof (struct GcBlock) {}.data - sizeof(struct Obj),
		x = GC_LINE_SIZE / 2 * rnorm(u) + GC_LINE_SIZE / 2;
	return (x + (max << CHAR_BIT * sizeof max / 2)) % max;
}

static volatile uint64_t this_seed;

static void death_callback() {
	fprintf(stderr, "Failure! seed: %" PRIx64 "\n", this_seed);
}

static volatile sig_atomic_t should_stop;
static void sigalrm_handler([[maybe_unused]] int sig) { should_stop = true; }

static void sig_handler(int sig) {
	death_callback();
	signal(sig, SIG_DFL); // TODO Reduce test case using ddmin
}

int main() {
	if (signal(SIGSEGV, sig_handler) == SIG_ERR
		|| signal(SIGALRM, sigalrm_handler) == SIG_ERR)
		die("signal failed");
	// Set timeout
	setitimer(ITIMER_REAL, &(const struct itimerval) {
			.it_value.tv_usec = /* 0.75s */ 750'000
		}, NULL);
#ifdef __SANITIZE_ADDRESS__
	__sanitizer_set_death_callback(death_callback);
#endif

	struct GcRef (*objects)[MAX_LIVE_OBJECTS];
	if (!(objects = malloc(sizeof *objects))) goto err; // malloc to avoid rooting

	struct GcHeap *heap;
	uint64_t state = time(NULL);
	unsigned int scenario_count = 0;
	do {
		this_seed = state;
		if (!(heap = gc_new())) goto err_free_objects;
		struct Ctx *ctx = (struct Ctx *) heap;
		*ctx = (struct Ctx) { .objects = objects };

		for (unsigned i = MAX_EVENTS; i--;) {
			uint64_t b = state = fxhash(state, 0);
			switch (gen_event_type(b)) {
			case EVENT_ALLOC: {
				size_t data_size = gen_obj_data_size(b);
				struct Obj *obj;
				if (!(obj = gc_alloc(heap, alignof(struct Obj), sizeof *obj + data_size))) {
					fputs("gc_alloc failed\n", stderr);
					goto err_free_heap;
				}
				*obj = (struct Obj)
					{ .hdr = obj->hdr, .num_slots = data_size / sizeof *obj->slots };

				unsigned i = ctx->num_objects++;
				bool is_root = !(b & 0x700);
				if (is_root) {
					unsigned j = ctx->num_roots++;
					(*objects)[i] = (*objects)[j];
					i = j;
				}
				(*objects)[i] = GC_COMPRESS(obj);
				break;
			}

			case EVENT_MUTATE: {
				if (!ctx->num_objects) break;
				unsigned i = (b >> 8) % ctx->num_objects,
					j = (b >> 40) % ctx->num_objects;
				struct Obj *src = (struct Obj *) GC_DECOMPRESS(heap, (*objects)[i]),
					*dst = (struct Obj *) GC_DECOMPRESS(heap, (*objects)[j]);
				if (!src->num_slots) break;
				unsigned k = b % MIN(src->num_slots, UINT8_MAX - 1);
				if (k >= src->used_slot_count) k = src->used_slot_count++;

				src->slots[k] = dst;
				gc_write_barrier(heap, &src->hdr);
				break;
			}

			case EVENT_GC:
				garbage_collect(heap);
				if (b < 0.6 * (double) UINT64_MAX) {
					heap->mark_color ^= !heap->is_major_gc;
					heap->is_major_gc = true;
					heap->defrag |= b < 0.3 * (double) UINT64_MAX;
				}
				if (!validate_heap(heap)) goto err_free_heap;
				break;
			default: unreachable();
			}
		}

		gc_free(heap);
	} while (++scenario_count, !should_stop);
		
	printf("Ran %u scenarios successfully\n", scenario_count);
	free(objects);
	return 0;
err_free_heap:
	gc_free(heap);
err_free_objects:
	free(objects);

	death_callback();
err: return EXIT_FAILURE;
}
