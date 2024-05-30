#include "lisp.h"
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
#include <xxh3.h>
#include "util.h"

struct GcHeap *heap;

static uint64_t symbol_hash(struct Symbol *x) { return XXH3_64bits(x->name, x->len); }

static bool symbol_equal(struct Symbol *a, struct Symbol *b) {
	return a->len == b->len && memcmp(a->name, b->name, a->len) == 0;
}

#define NAME symbol
#define KEY struct Symbol *
#include "tbl.h"

#define NUM_ARGS_IMPL(_8, _7, _6, _5, _4, _3, _2, _1, n, ...) n
#define NUM_ARGS(...) NUM_ARGS_IMPL(__VA_ARGS__ __VA_OPT__(,) 8, 7, 6, 5, 4, 3, 2, 1, 0)

struct Subr *subr_head;

#define DEFUN(lname, cname, args, ...)									\
	static LispObject F ## cname args;									\
	[[gnu::constructor]] static void lisp_constructor_ ## cname(void) { \
		static struct Subr subr = {										\
			.CAT(a, NUM_ARGS args) = F ## cname,						\
			.name = lname,												\
			.min_args = NUM_ARGS args,									\
		};																\
		subr.next = subr_head;											\
		subr_head = &subr;												\
	}																	\
	static LispObject F ## cname args

static size_t string_size(void *x) { return strlen(x) + 1; }

static void string_trace(struct GcHeap *, void *x) { gc_mark(string_size(x), x); }

static struct GcTypeInfo string_tib = { string_trace, string_size };

static size_t cons_size(void *) { return sizeof(struct LispPair); }

static void cons_trace(struct GcHeap *heap, void *x) {
	struct LispPair *cons = x;
	gc_mark(sizeof *cons, x);
	if (cons->car) gc_trace(heap, &cons->car);
	if (cons->cdr) gc_trace(heap, &cons->cdr);
}

static size_t symbol_size(void *) { return sizeof(struct Symbol); }

static void symbol_trace(struct GcHeap *heap, void *x) {
	struct Symbol *sym = x;
	gc_mark(sizeof *sym, x);
	gc_mark(sym->len + 1, sym->name);
	if (sym->value) gc_trace(heap, &sym->value);
}

static size_t function_size(void *) { return sizeof(struct Function); }

static size_t integer_size(void *) { return sizeof(int); }

// Generic trace function for objects that fit in a line.
static void trace_small(struct GcHeap *, void *x) { gc_mark(1, x); }

static struct LispTypeInfo cons_tib = {
	.gc_tib = { cons_trace, cons_size },
	.tag = LISP_PAIR,
}, symbol_tib = {
	.gc_tib = { symbol_trace, symbol_size },
	.tag = LISP_SYMBOL,
}, function_tib = {
	.gc_tib = { trace_small, function_size },
	.tag = LISP_FUNCTION,
}, integer_tib = {
	.gc_tib = { trace_small, integer_size },
	.tag = LISP_INTEGER,
};

LispObject cons(LispObject car, LispObject cdr) {
	struct LispPair *cell = gc_alloc(heap, sizeof *cell, &cons_tib.gc_tib);
	*cell = (struct LispPair) { car, cdr };
	return cell;
}

LispObject intern(struct LispCtx *ctx, size_t len, const char s[static len]) {
	if (len == 3 && memcmp(s, "nil", 3) == 0) return NULL;

	struct Symbol key = { .len = len, .name = s }, **entry;
	if (!symbol_tbl_entry(&ctx->symbol_tbl, &key, &entry)) {
		if (!entry) die("malloc failed");
		char *name = gc_alloc(heap, len + 1, &string_tib);
		memcpy(name, s, len);
		name[len] = '\0';
		*entry = gc_alloc(heap, sizeof **entry, &symbol_tib.gc_tib);
		**entry = (struct Symbol) { .name = name, .len = len, };
	}
	return *entry;
}

LispObject lisp_integer(int i) {
	int *p = gc_alloc(heap, sizeof *p, &integer_tib.gc_tib);
	*p = i;
	return p;
}

void lisp_print(LispObject object) {
	switch (lisp_type(object)) {
	case LISP_NIL: printf("nil"); break;
	case LISP_PAIR:
		struct LispPair *cell = object;
		putchar('(');
	print_next_cell:
		lisp_print(cell->car);
		if (!cell->cdr) ;
		else if (consp(cell->cdr)) {
			putchar(' ');
			cell = cell->cdr;
			goto print_next_cell;
		} else {
			printf(" . ");
			lisp_print(cell->cdr);
		}
		putchar(')');
		break;
	case LISP_SYMBOL:
		struct Symbol *sym = object;
		fwrite(sym->name, sizeof *sym->name, sym->len, stdout);
		break;
	case LISP_FUNCTION:
		printf("#<subr %s>", ((struct Function *) object)->subr->name);
		break;
	case LISP_CLOSURE: printf("#<closure>"); break;
	case LISP_INTEGER: printf("%i", *(int *) object); break;
	default: unreachable();
	}
}

bool lisp_eq(LispObject a, LispObject b) {
	if (a == b) return true;
	enum LispObjectType ty = lisp_type(a);
	if (lisp_type(b) != ty) return false;
	switch (ty) {
	case LISP_SYMBOL: case LISP_FUNCTION: case LISP_CLOSURE:
		return false;
	case LISP_PAIR:
		struct LispPair *x = a, *y = b;
		return lisp_eq(x->car, y->car) && lisp_eq(x->cdr, y->cdr);
	case LISP_INTEGER: return *((int *) a) == *((int *) b);
	case LISP_NIL:
	}
	unreachable();
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

void gc_trace_roots(struct GcHeap *heap, void *userdata) {
	struct LispCtx *ctx = userdata;
	struct Symbol **sym;
	for (size_t i = 0; symbol_tbl_iter_next(&ctx->symbol_tbl, &i, &sym);)
		gc_trace(heap, (void **) sym);

	LispObject *objs[] = { &ctx->ffn, &ctx->fif, &ctx->flet, &ctx->fset,
		&ctx->fprogn, &ctx->fquote, &ctx->t };
	for (size_t i = 0; i < LENGTH(objs); ++i) gc_trace(heap, objs[i]);

	// TODO Trace the stack
}

bool lisp_init(struct LispCtx *ctx) {
	long page_size = sysconf(_SC_PAGESIZE);
	// TODO Divide guard pages into yellow and red zones (in HotSpot
	// terminology) where the yellow zone is temporarily disabled for
	// exception handlers not to immediately trigger another overflow.
	LispObject *stack;
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
	*stack = stack[1] = NULL; // No return address for first call frame

	ctx->symbol_tbl = tbl_new();

	ctx->ffn = intern(ctx, sizeof "fn" - 1, "fn");
	ctx->fif = intern(ctx, sizeof "if" - 1, "if");
	ctx->flet = intern(ctx, sizeof "let" - 1, "let");
	ctx->fset = intern(ctx, sizeof "set" - 1, "set");
	ctx->fprogn = intern(ctx, sizeof "progn" - 1, "progn");
	ctx->fquote = intern(ctx, sizeof "quote" - 1, "quote");
	struct Symbol *t = ctx->t = intern(ctx, 1, "t");
	t->value = t;

	for (struct Subr *subr = subr_head; subr; subr = subr->next) {
		struct Function *f = gc_alloc(heap, sizeof *f, &function_tib.gc_tib);
		*f = (struct Function) { subr };
		struct Symbol *sym = intern(ctx, strlen(subr->name), subr->name);
		sym->value = f;
	}

	return true;
}

void lisp_free(struct LispCtx *ctx) {
	munmap(ctx->bp, ctx->guard_end - (uintptr_t) ctx->bp);
	symbol_tbl_free(&ctx->symbol_tbl);
}

DEFUN("print", print, (LispObject x)) { lisp_print(x); puts(""); return NULL; }

DEFUN("cons", cons, (LispObject car, LispObject cdr)) {
	return cons(car, cdr);
}

DEFUN("car", car, (LispObject x)) { return car(x); }

DEFUN("cdr", cdr, (LispObject x)) {
	return lisp_type(x) == LISP_PAIR ? ((struct LispPair *) x)->cdr : NULL;
}

DEFUN("+", add, (LispObject a, LispObject b)) {
	if (!(lisp_type(a) == LISP_INTEGER && lisp_type(b) == LISP_INTEGER))
		throw(1);
	return lisp_integer(*(int *) a + *(int *) b);
}

DEFUN("<", lt, (LispObject a, LispObject b)) {
	if (!(lisp_type(a) == LISP_INTEGER && lisp_type(b) == LISP_INTEGER))
		throw(1);
	return *(int *) a < *(int *) b ? lisp_integer(1) : NULL;
}
