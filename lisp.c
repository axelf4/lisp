#include "lisp.h"
#include <stdio.h>

struct Heap *heap;

static size_t consSize(void *) { return sizeof(struct Cons); }

static void consTrace(struct Heap *heap, void *x) {
	struct Cons *cons = x;
	gc_mark(sizeof *cons, x);
	if (cons->car) gc_trace(heap, (void **) &cons->car);
	if (cons->cdr) gc_trace(heap, (void **) &cons->cdr);
}

static size_t integerSize(void *) { return sizeof(int); }

static void integerTrace(struct Heap *, void *x) { gc_mark(sizeof(int), x); }

static struct LispTypeInfo consTib = {
	.gcTib = { consSize, consTrace },
	.tag = LISP_CONS,
}, integerTib = {
	.gcTib = { integerSize, integerTrace },
	.tag = LISP_INTEGER,
};

LispObject *cons(LispObject *car, LispObject *cdr) {
	struct Cons *cell = gc_alloc(heap, sizeof(struct Cons), &consTib.gcTib);
	cell->car = car;
	cell->cdr = cdr;
	return (LispObject *) cell;
}

LispObject *lisp_integer(int i) {
	int *p = gc_alloc(heap, sizeof(int), &integerTib.gcTib);
	*p = i;
	return (LispObject *) p;
}

void lisp_print(LispObject *object) {
	switch (lisp_tag(object)) {
	case LISP_NULL: printf("nil"); break;
	case LISP_CONS:
		struct Cons *cell = (struct Cons *) object;
		putchar('(');
	print_next_cell:
		lisp_print(cell->car);
		if (!cell->cdr) printf(")");
		else if (lisp_tag(cell->cdr) == LISP_CONS) {
			putchar(' ');
			cell = (struct Cons *) cell->cdr;
			goto print_next_cell;
		} else {
			printf(" . ");
			lisp_print(cell->cdr);
			putchar(')');
		}
		break;
	case LISP_INTEGER: printf("%d", *(int *) object); break;
	default: printf("INVALID_TAG"); break;
	}
}
