#include "lisp.h"
#include <stdio.h>
#include <string.h>
#include <xxh3.h>

struct Heap *heap;

struct Symbol {
	size_t len;
	char *name;
};

static uint64_t symbol_hash(struct Symbol *x) { return XXH3_64bits(x->name, x->len); }

static bool symbol_equal(struct Symbol *a, struct Symbol *b) {
	return a->len == b->len && memcmp(a->name, b->name, a->len) == 0;
}

#define NAME symbol
#define KEY struct Symbol *
#define TYPE SymbolTable
#define KEY_HASH symbol_hash
#define KEY_EQUAL symbol_equal
#include "tbl.h"

struct LispContext {
	struct SymbolTable symbol_tbl;
};

static size_t string_size(void *x) { return strlen(x) + 1; }

static void string_trace(struct Heap *, void *x) { gc_mark(string_size(x), x); }

static struct GcTypeInfo string_tib = { string_size, string_trace };

static size_t cons_size(void *) { return sizeof(struct Cons); }

static void cons_trace(struct Heap *heap, void *x) {
	struct Cons *cons = x;
	gc_mark(sizeof *cons, x);
	if (cons->car) gc_trace(heap, &cons->car);
	if (cons->car) gc_trace(heap, &cons->cdr);
}

static size_t symbol_size(void *) { return sizeof(struct Symbol); }

static void symbol_trace(struct Heap *heap, void *x) {
	struct Symbol *sym = x;
	gc_mark(sizeof *sym, x);
	gc_trace(heap, (void **) &sym->name);
}

static size_t integer_size(void *) { return sizeof(int); }

// Generic trace function for objects that fit in a line.
static void trace_small(struct Heap *, void *x) { gc_mark(1, x); }

static struct LispTypeInfo cons_tib = {
	.gc_tib = { cons_size, cons_trace },
	.tag = LISP_CONS,
}, symbol_tib = {
	.gc_tib = { symbol_size, symbol_trace },
	.tag = LISP_SYMBOL,
}, integer_tib = {
	.gc_tib = { integer_size, trace_small },
	.tag = LISP_INTEGER,
};

LispObject *cons(LispObject *car, LispObject *cdr) {
	struct Cons *cell = gc_alloc(heap, sizeof(struct Cons), &cons_tib.gc_tib);
	cell->car = car;
	cell->cdr = cdr;
	return (LispObject *) cell;
}

LispObject *intern(struct LispContext *ctx, size_t len, char s[static len]) {
	char nil[3] = "nil";
	if (len == 3 && memcmp(s, nil, LENGTH(nil)) == 0) return NULL;

	struct Symbol key = { .len = len, .name = s }, **entry;
	if (!symbol_tbl_entry(&ctx->symbol_tbl, &key, &entry)) {
		struct Symbol *sym = *entry = gc_alloc(heap, sizeof **entry, &symbol_tib.gc_tib);
		memcpy(sym->name = gc_alloc(heap, len + 1, &string_tib), s, sym->len = len);
		sym->name[len] = '\0';
	}

	return *entry;
}

LispObject *lisp_integer(int i) {
	int *p = gc_alloc(heap, sizeof(int), &integer_tib.gc_tib);
	*p = i;
	return (LispObject *) p;
}

void lisp_print(LispObject *object) {
	switch (lisp_tag(object)) {
	case LISP_NULL: printf("nil"); break;
	case LISP_CONS:
		struct Cons *cell = object;
		putchar('(');
	print_next_cell:
		lisp_print(cell->car);
		if (!cell->cdr) printf(")");
		else if (lisp_tag(cell->cdr) == LISP_CONS) {
			putchar(' ');
			cell = cell->cdr;
			goto print_next_cell;
		} else {
			printf(" . ");
			lisp_print(cell->cdr);
			putchar(')');
		}
		break;
	case LISP_SYMBOL:
		struct Symbol *sym = object;
		fwrite(sym->name, sizeof(char), sym->len, stdout);
		break;
	case LISP_INTEGER: printf("%i", *(int *) object); break;
	default: puts("Bad tag"); exit(1); break;
	}
}

size_t lisp_ctx_size(void *) { return sizeof(struct LispContext); }

static void lisp_ctx_trace(struct Heap *, void *x) {
	struct LispContext *ctx = x;
	gc_mark(sizeof *ctx, x);

	for (size_t i = 0; i < ctx->symbol_tbl.bucket_mask + 1; ++i)
		if (IS_FULL(ctx->symbol_tbl.ctrl[i]))
			gc_trace(heap, (void **) &ctx->symbol_tbl.buckets[i]);
}

struct GcTypeInfo lisp_ctx_tib = { lisp_ctx_size, lisp_ctx_trace };

struct LispContext *lisp_init() {
	struct LispContext *ctx = gc_alloc(heap, sizeof(struct LispContext), &lisp_ctx_tib);
	*ctx = (struct LispContext) {
		.symbol_tbl = symbol_tbl_new(),
	};

	return ctx;
}
