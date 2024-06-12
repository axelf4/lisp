#include "lisp.h"
#include <stdckdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
#include <xxh3.h>
#include "vm.h"
#include "util.h"

#define COMMA ,
#define NUM_ARGS_IMPL(_8, _7, _6, _5, _4, _3, _2, _1, n, ...) n
#define NUM_ARGS(...) NUM_ARGS_IMPL(__VA_ARGS__ __VA_OPT__(,) 7, 6, 5, 4, 3, 2, 1, 0)
#define MAP_ARGS_IMPL(_8, _7, _6, _5, _4, _3, _2, _1, n, ...) n
#define MAP_ARGS(...) MAP_ARGS_IMPL(__VA_ARGS__ __VA_OPT__(,) 7, 6, 5, 4, 3, *__args COMMA __args[1], *__args, )

#define DEFUN(lname, cname, args, ...)									\
	static LispObject __ ## cname args;									\
	static LispObject F ## cname(struct LispCtx *ctx, uint8_t n, const LispObject __args[static n]) { \
		if (n != NUM_ARGS args) throw(2);								\
		return __ ## cname(ctx, MAP_ARGS args);							\
	}																	\
	static struct LispCFunction S ## cname = {							\
		.hdr.tag = LISP_CFUNCTION, .nargs = NUM_ARGS args,				\
		.f = F ## cname,												\
		.name = lname,													\
	};																	\
	static LispObject __ ## cname args

static uint64_t symbol_hash(struct Symbol *x) { return XXH3_64bits(x->name, x->len); }

static bool symbol_equal(struct Symbol *a, struct Symbol *b) {
	return a->len == b->len && memcmp(a->name, b->name, a->len) == 0;
}

#define NAME symbol
#define KEY struct Symbol *
#include "tbl.h"

LispObject cons(struct LispCtx *ctx, LispObject car, LispObject cdr) {
	struct GcHeap *heap = (struct GcHeap *) ctx;
	struct LispPair *cell = gc_alloc(heap, alignof(struct LispPair), sizeof *cell);
	*cell = (struct LispPair) { { cell->hdr.hdr, LISP_PAIR }, GC_COMPRESS(car), GC_COMPRESS(cdr) };
	return TAG_OBJ(cell);
}

struct LispString {
	alignas(GC_MIN_ALIGNMENT) struct LispObjectHeader hdr;
	size_t len;
	char s[];
};

LispObject intern(struct LispCtx *ctx, size_t len, const char s[static len]) {
	if (len == 3 && memcmp(s, "nil", 3) == 0) return NIL;

	struct GcHeap *heap = (struct GcHeap *) ctx;
	struct Symbol key = { .len = len, .name = s }, **entry;
	if (!symbol_tbl_entry(&ctx->symbol_tbl, &key, &entry)) {
		if (!entry) die("malloc failed");
		struct LispString *name = gc_alloc(heap, alignof(struct LispString), sizeof *name + len + 1);
		name->hdr.tag = LISP_STRING;
		memcpy(name->s, s, name->len = len);
		name->s[len] = '\0';
		*entry = gc_alloc(heap, alignof(struct Symbol), sizeof **entry);
		**entry = (struct Symbol) { { (*entry)->hdr.hdr, LISP_SYMBOL },
			.name = name->s, .len = len, };
	}
	return TAG_OBJ(*entry);
}

void lisp_print(struct LispCtx *ctx, LispObject x) {
	switch (lisp_type(x)) {
	case LISP_NIL: printf("nil"); break;
	case LISP_INTEGER: printf("%i", UNTAG_SMI(x)); break;
	case LISP_PAIR:
		struct LispPair *cell = UNTAG_OBJ(x);
		putchar('(');
	print_next_cell:
		LispObject car = GC_DECOMPRESS(ctx, cell->car),
			cdr = GC_DECOMPRESS(ctx, cell->cdr);
		lisp_print(ctx, car);
		if (consp(cdr)) {
			putchar(' ');
			cell = UNTAG_OBJ(cdr);
			goto print_next_cell;
		} else if (!NILP(cdr)) {
			printf(" . ");
			lisp_print(ctx, cdr);
		}
		putchar(')');
		break;
	case LISP_SYMBOL:
		struct Symbol *sym = UNTAG_OBJ(x);
		fwrite(sym->name, sizeof *sym->name, sym->len, stdout);
		break;
	case LISP_CFUNCTION:
		printf("#<subr %s>", ((struct LispCFunction *) UNTAG_OBJ(x))->name);
		break;
	case LISP_CLOSURE: printf("#<closure>"); break;
	default: unreachable();
	}
}

bool lisp_eq(struct LispCtx *ctx, LispObject a, LispObject b) {
	if (a == b) return true;
	enum LispObjectType ty = lisp_type(a);
	if (lisp_type(b) != ty) return false;
	switch (ty) {
	case LISP_INTEGER: case LISP_SYMBOL: case LISP_CFUNCTION: case LISP_CLOSURE:
		return false;
	case LISP_PAIR:
		struct LispPair *x = UNTAG_OBJ(a), *y = UNTAG_OBJ(b);
		return lisp_eq(ctx, GC_DECOMPRESS(ctx, x->car), GC_DECOMPRESS(ctx, y->car))
			&& lisp_eq(ctx, GC_DECOMPRESS(ctx, x->cdr), GC_DECOMPRESS(ctx, y->cdr));
	default: unreachable();
	}
}

bool lisp_signal_handler(int sig, siginfo_t *info, [[maybe_unused]] void *ucontext, struct LispCtx *ctx) {
	if (sig == SIGSEGV) {
		// Check if fault was within the stack guard pages
		if ((uintptr_t) ctx->bp <= (uintptr_t) info->si_addr
			&& (uintptr_t) info->si_addr < ctx->guard_end)
			// Safety: run is compiled with -fnon-call-exceptions and
			// SIGSEGV is a synchronous signal.
			throw(SIGSEGV); // Throw stack overflow exception
	}
	return false;
}

static size_t lisp_string_size(struct LispString *x) { return sizeof *x + x->len + 1; }

static size_t closure_size(struct Closure *x) {
	return sizeof *x + x->prototype->num_upvalues * sizeof *x->upvalues;
}

static size_t chunk_size(struct Chunk *x) {
	return sizeof *x + x->num_consts * sizeof(LispObject)
		+ x->count * sizeof(struct Instruction);
}

static void lisp_trace(struct GcHeap *heap, LispObject *p) {
	if (!(IS_SMI(*p) || NILP(*p))) *p = TAG_OBJ(gc_trace(heap, UNTAG_OBJ(*p)));
}
static void lisp_trace_compressed(struct GcHeap *heap, Lobj *p) {
	if (IS_SMI(p->p) || NILP(p->p)) return;
	*p = GC_COMPRESS(TAG_OBJ(gc_trace(heap, UNTAG_OBJ(GC_DECOMPRESS(heap, *p)))));
}

void gc_object_visit(struct GcHeap *heap, void *p) {
	struct LispObjectHeader *hdr = p;
	switch (hdr->tag) {
	case LISP_PAIR:
		struct LispPair *pair = p;
		gc_mark(sizeof *pair, p);
		lisp_trace_compressed(heap, &pair->cdr);
		lisp_trace_compressed(heap, &pair->car);
		break;
	case LISP_SYMBOL:
		struct Symbol *sym = p;
		gc_mark(sizeof *sym, p);
		struct LispString *str
			= gc_trace(heap, (char *) sym->name - offsetof(struct LispString, s));
		sym->name = str->s;
		lisp_trace(heap, &sym->value);
		break;
	case LISP_STRING: gc_mark(lisp_string_size(p), p); break;
	case LISP_CFUNCTION: gc_mark(sizeof(struct LispCFunction), p); break;
	case LISP_CLOSURE: {
		struct Closure *x = p;
		gc_mark(closure_size(x), p);
		for (struct Upvalue **it = x->upvalues, **end = it + x->prototype->num_upvalues;
				it < end; ++it) *it = gc_trace(heap, *it);

		char *chunk = gc_trace(heap, (char *) x->prototype - x->prototype->offset);
		// Update prototype as chunk may have moved
		x->prototype = (struct Prototype *) (chunk + x->prototype->offset);
		break;
	}
	case LISP_UPVALUE:
		struct Upvalue *x = p;
		gc_mark(sizeof *x, p);
		if (x->is_closed) lisp_trace(heap, x->location = &x->value);
		else x->next = gc_trace(heap, x->next);
		break;
	case LISP_BYTECODE_CHUNK:
		struct Chunk *chunk = p;
		gc_mark(chunk_size(p), p);
		for (LispObject *x = chunk_constants(chunk), *end = x + chunk->num_consts;
				x < end; ++x) lisp_trace(heap, x);
		break;
	case LISP_NIL: case LISP_INTEGER: unreachable();
	}
}

size_t gc_object_size(void *p, size_t *alignment) {
	struct LispObjectHeader *hdr = p;
	switch (hdr->tag) {
	case LISP_PAIR:
		*alignment = alignof(struct LispPair);
		return sizeof(struct LispPair);
	case LISP_SYMBOL:
		*alignment = alignof(struct Symbol);
		return sizeof(struct Symbol);
	case LISP_STRING:
		*alignment = alignof(struct LispString);
		return lisp_string_size(p);
	case LISP_CFUNCTION:
		*alignment = alignof(struct LispCFunction);
		return sizeof(struct LispCFunction);
	case LISP_CLOSURE:
		*alignment = alignof(struct Closure);
		return closure_size(p);
	case LISP_UPVALUE:
		*alignment = alignof(struct Upvalue);
		return sizeof(struct Upvalue);
	case LISP_BYTECODE_CHUNK:
		*alignment = alignof(struct Chunk);
		return chunk_size(p);
	case LISP_NIL: case LISP_INTEGER:
	}
	unreachable();
}

void gc_trace_roots(struct GcHeap *heap) {
	struct LispCtx *ctx = (struct LispCtx *) heap;
	struct Symbol **sym;
	for (size_t i = 0; symbol_tbl_iter_next(&ctx->symbol_tbl, &i, &sym);)
		*sym = gc_trace(heap, *sym);

	LispObject *objs[] = { &ctx->ffn, &ctx->fif, &ctx->flet, &ctx->fset,
		&ctx->fprogn, &ctx->fquote, &ctx->t };
	for (size_t i = 0; i < LENGTH(objs); ++i) lisp_trace(heap, objs[i]);

	// TODO Trace the stack
}

DEFUN("eval", eval, (struct LispCtx *ctx, LispObject form)) { return lisp_eval(ctx, form); }

DEFUN("print", print, (struct LispCtx *ctx, LispObject x)) {
	lisp_print(ctx, x);
	putchar('\n');
	return NIL;
}

DEFUN("cons", cons, (struct LispCtx *ctx, LispObject car, LispObject cdr)) {
	return cons(ctx, car, cdr);
}

DEFUN("car", car, (struct LispCtx *ctx, LispObject x)) { return car(ctx, x); }

DEFUN("cdr", cdr, (struct LispCtx *ctx, LispObject x)) { return cdr(ctx, x); }

DEFUN("+", add, (struct LispCtx *, LispObject a, LispObject b)) {
	if (!(IS_SMI(a) && IS_SMI(b))) throw(1);
	int32_t result;
	if (ckd_add(&result, (int32_t) (uint32_t) a, (int32_t) (uint32_t) b))
		throw(1);
	return result;
}

DEFUN("<", lt, (struct LispCtx *ctx, LispObject a, LispObject b)) {
	if (!(IS_SMI(a) && IS_SMI(b))) throw(1);
	return (int32_t) (uint32_t) a < (int32_t) (uint32_t) b ? ctx->t : NIL;
}

bool lisp_init(struct LispCtx *ctx) {
	long page_size = sysconf(_SC_PAGESIZE);
	// TODO Divide guard pages into yellow and red zones (in HotSpot
	// terminology) where the yellow zone is temporarily disabled for
	// exception handlers not to immediately trigger another overflow.
	uintptr_t *stack;
#define STACK_LEN 0x1000
	size_t size = STACK_LEN * sizeof *stack,
		guard_size = (0xff * sizeof *stack + page_size - 1) & (page_size - 1);
	if ((ctx->bp = stack = mmap(NULL, size + guard_size,
				PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0))
		== MAP_FAILED) return NULL;
	if (mprotect(stack, size, PROT_READ | PROT_WRITE)) {
		munmap(stack, size + guard_size);
		return false;
	}
	ctx->guard_end = (uintptr_t) stack + size + guard_size;
	*stack = NIL;
	stack[1] = (uintptr_t) NULL; // No return address for first call frame

	ctx->symbol_tbl = tbl_new();

	ctx->ffn = intern(ctx, sizeof "fn" - 1, "fn");
	ctx->fif = intern(ctx, sizeof "if" - 1, "if");
	ctx->flet = intern(ctx, sizeof "let" - 1, "let");
	ctx->fset = intern(ctx, sizeof "set" - 1, "set");
	ctx->fprogn = intern(ctx, sizeof "progn" - 1, "progn");
	ctx->fquote = intern(ctx, sizeof "quote" - 1, "quote");
	ctx->t = intern(ctx, 1, "t");
	((struct Symbol *) UNTAG_OBJ(ctx->t))->value = ctx->t;

	struct GcHeap *heap = (struct GcHeap *) ctx;
	struct LispCFunction *cfuns[]
		= { &Seval, &Sprint, &Scons, &Scar, &Scdr, &Sadd, &Slt, };
	for (size_t i = 0; i < LENGTH(cfuns); ++i) {
		struct LispCFunction *x = gc_alloc(heap, alignof(struct LispCFunction), sizeof *x);
		struct GcObjectHeader hdr = x->hdr.hdr;
		*x = *cfuns[i];
		x->hdr.hdr = hdr;
		struct Symbol *sym = UNTAG_OBJ(intern(ctx, strlen(x->name), x->name));
		sym->value = TAG_OBJ(x);
	}

	return true;
}

void lisp_free(struct LispCtx *ctx) {
	munmap(ctx->bp, ctx->guard_end - (uintptr_t) ctx->bp);
	symbol_tbl_free(&ctx->symbol_tbl);
}
