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

static struct GcTypeInfo consTib = { consSize, consTrace };

struct LispObject *cons(struct LispObject *car, struct LispObject *cdr) {
	struct Cons *cell = gc_alloc(heap, sizeof(struct Cons), &consTib);
	cell->object.tag = LISP_CONS;
	cell->car = car;
	cell->cdr = cdr;
	return &cell->object;
}

static size_t integerSize(void *) { return sizeof(struct LispInteger); }

static void integerTrace(struct Heap *, void *x) {
	gc_mark(sizeof(struct LispObject), x);
}

static struct GcTypeInfo integerTib = { integerSize, integerTrace };

struct LispObject *lisp_integer(int i) {
	struct LispInteger *obj = gc_alloc(heap, sizeof(struct LispInteger), &integerTib);
	obj->object.tag = LISP_INTEGER;
	obj->i = i;
	return &obj->object;
}

void lisp_print(struct LispObject *object) {
	if (!object) {
		printf("nil");
		return;
	}

	switch (object->tag) {
	case LISP_CONS:
		struct Cons *cell = (struct Cons *) object;
		printf("(");
	print_next_cell:
		lisp_print(cell->car);
		if (!cell->cdr) printf(")");
		else if (cell->cdr->tag == LISP_CONS) {
			printf(" ");
			cell = (struct Cons *) cell->cdr;
			goto print_next_cell;
		} else {
			printf(" . ");
			lisp_print(cell->cdr);
			printf(")");
		}
		break;
	case LISP_INTEGER: printf("%d", ((struct LispInteger *) object)->i); break;
	}
}
