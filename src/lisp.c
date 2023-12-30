#include "lisp.h"
#include <stdio.h>
#include <string.h>
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

#define COMMA ,
#define NUM_ARGS_IMPL(_8, _7, _6, _5, _4, _3, _2, _1, n, ...) n
#define NUM_ARGS(...) NUM_ARGS_IMPL(__VA_ARGS__ __VA_OPT__(,) 7, 6, 5, 4, 3, 2, 1, 0)
#define MAP_ARGS_IMPL(_8, _7, _6, _5, _4, _3, _2, _1, n, ...) n
#define MAP_ARGS(...) MAP_ARGS_IMPL(__VA_ARGS__ __VA_OPT__(,) 7, 6, 5, 4, 3, *__args COMMA __args[1], *__args, )

#define DEFUN(lname, cname, args, ...)									\
	static LispObject *__ ## cname args;								\
	static LispObject *F ## cname(struct LispContext *ctx, LispObject *const *__args) { \
		return __ ## cname(ctx, MAP_ARGS args);							\
	}																	\
	static struct LispCFunction S ## cname = {							\
		.f = F ## cname,												\
		.nargs = NUM_ARGS args,											\
		.name = lname,													\
	};																	\
	static LispObject *__ ## cname args

static size_t string_size(void *x) { return strlen(x) + 1; }

static void string_trace(struct GcHeap *, void *x) { gc_mark(string_size(x), x); }

static struct GcTypeInfo string_tib = { string_trace, string_size };

static size_t cons_size(void *) { return sizeof(struct Cons); }

static void cons_trace(struct GcHeap *heap, void *x) {
	struct Cons *cons = x;
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

static size_t cfunction_size(void *) { return sizeof(struct LispCFunction); }

static size_t integer_size(void *) { return sizeof(int); }

// Generic trace function for objects that fit in a line.
static void trace_small(struct GcHeap *, void *x) { gc_mark(1, x); }

static struct LispTypeInfo cons_tib = {
	.gc_tib = { cons_trace, cons_size },
	.tag = LISP_CONS,
}, symbol_tib = {
	.gc_tib = { symbol_trace, symbol_size },
	.tag = LISP_SYMBOL,
}, cfunction_tib = {
	.gc_tib = { trace_small, cfunction_size },
	.tag = LISP_CFUNCTION,
}, integer_tib = {
	.gc_tib = { trace_small, integer_size },
	.tag = LISP_INTEGER,
};

LispObject *cons(LispObject *car, LispObject *cdr) {
	struct Cons *cell = gc_alloc(heap, sizeof *cell, &cons_tib.gc_tib);
	*cell = (struct Cons) { car, cdr };
	return cell;
}

LispObject *intern(struct LispContext *ctx, size_t len, const char s[static len]) {
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

LispObject *lisp_integer(int i) {
	int *p = gc_alloc(heap, sizeof *p, &integer_tib.gc_tib);
	*p = i;
	return p;
}

void lisp_print(LispObject *object) {
	switch (lisp_type(object)) {
	case LISP_NIL: printf("nil"); break;
	case LISP_CONS:
		struct Cons *cell = object;
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
	case LISP_CFUNCTION:
		printf("#<subr %s>", ((struct LispCFunction *) object)->name);
		break;
	case LISP_CLOSURE: printf("#<closure>"); break;
	case LISP_INTEGER: printf("%i", *(int *) object); break;
	default: __builtin_unreachable();
	}
}

size_t lisp_ctx_size(void *) { return sizeof(struct LispContext); }

static void lisp_ctx_trace(struct GcHeap *, void *x) {
	struct LispContext *ctx = x;
	gc_mark(sizeof *ctx, x);

	struct Symbol **sym;
	for (size_t i = 0; symbol_tbl_iter_next(&ctx->symbol_tbl, &i, &sym);)
		gc_trace(heap, (void **) sym);
}

struct GcTypeInfo lisp_ctx_tib = { lisp_ctx_trace, lisp_ctx_size };

void lisp_free(struct LispContext *ctx) {
	symbol_tbl_free(&ctx->symbol_tbl);
}

DEFUN("print", print, (struct LispContext *, LispObject *x)) {
	lisp_print(x);
	putchar('\n');
	return NULL;
}

DEFUN("cons", cons, (struct LispContext *, LispObject *car, LispObject *cdr)) {
	return cons(car, cdr);
}

DEFUN("car", car, (struct LispContext *, LispObject *x)) {
	return consp(x) ? ((struct Cons *) x)->car : NULL;
}

DEFUN("cdr", cdr, (struct LispContext *, LispObject *x)) {
	return consp(x) ? ((struct Cons *) x)->cdr : NULL;
}

DEFUN("+", add, (struct LispContext *, LispObject *a, LispObject *b)) {
	if (!(lisp_type(a) == LISP_INTEGER && lisp_type(b) == LISP_INTEGER))
		throw(2);
	return lisp_integer(*(int *) a + *(int *) b);
}

DEFUN("<", lt, (struct LispContext *ctx, LispObject *a, LispObject *b)) {
	if (!(lisp_type(a) == LISP_INTEGER && lisp_type(b) == LISP_INTEGER))
		throw(2);
	return *(int *) a < *(int *) b ? ctx->t : NULL;
}

struct LispContext *lisp_new() {
	struct LispContext *ctx = gc_alloc(heap, sizeof *ctx, &lisp_ctx_tib);
	ctx->symbol_tbl = tbl_new();

	ctx->ffn = intern(ctx, sizeof "fn" - 1, "fn");
	ctx->fif = intern(ctx, sizeof "if" - 1, "if");
	ctx->flet = intern(ctx, sizeof "let" - 1, "let");
	ctx->fset = intern(ctx, sizeof "set" - 1, "set");
	ctx->fprogn = intern(ctx, sizeof "progn" - 1, "progn");
	ctx->fquote = intern(ctx, sizeof "quote" - 1, "quote");
	ctx->smacro = intern(ctx, sizeof "macro" - 1, "macro");
	ctx->t = intern(ctx, sizeof "t" - 1, "t");

	struct LispCFunction *cfuns[] = { &Sprint, &Scons, &Scar, &Scdr, &Sadd, &Slt, };
	for (size_t i = 0; i < LENGTH(cfuns); ++i) {
		struct LispCFunction *x = gc_alloc(heap, sizeof *x, &cfunction_tib.gc_tib);
		*x = *(cfuns[i]);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wnonnull"
		struct Symbol *sym = intern(ctx, strlen(x->name), x->name);
#pragma GCC diagnostic pop
		sym->value = x;
	}

	return ctx;
}
