#include "lisp.h"
#include <stdckdint.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include "fxhash.h"
#include "util.h"

#define STACK_LEN 0x1000
#define MAX_FRAME 0x100

static uint64_t symbol_hash(struct LispSymbol *x) {
	return fxhash_finish(fxhash(0, fxhash_str(x->len, x->name)));
}

static bool symbol_equal(struct LispSymbol *a, struct LispSymbol *b) {
	return a->len == b->len && memcmp(a->name, b->name, a->len) == 0;
}

#define NAME symbol
#define KEY struct LispSymbol *
#include "tbl.h"

LispObject cons(struct LispCtx *ctx, LispObject car, LispObject cdr) {
	struct LispPair *cell
		= gc_alloc((struct GcHeap *) ctx, alignof(struct LispPair), sizeof *cell);
	*cell = (struct LispPair)
		{ { cell->hdr.hdr, LISP_PAIR }, GC_COMPRESS(car), GC_COMPRESS(cdr) };
	return TAG_OBJ(cell);
}

struct LispString {
	alignas(GC_ALIGNMENT) struct LispObjectHeader hdr;
	unsigned len;
	char s[];
};

LispObject intern(struct LispCtx *ctx, size_t len, const char s[static len]) {
	if (len == 3 && memcmp(s, "nil", 3) == 0) return NIL(ctx);

	struct GcHeap *heap = (struct GcHeap *) ctx;
	struct LispSymbol key = { .len = len, .name = s }, **entry;
	if (!symbol_tbl_entry(&ctx->symbol_tbl, &key, &entry)) {
		if (!entry) die("malloc failed");
		*entry = (struct LispSymbol *) &ctx->nil;

		struct LispString *name
			= gc_alloc(heap, alignof(struct LispString), sizeof *name + len);
		name->hdr.tag = LISP_STRING;
		memcpy(name->s, s, name->len = len);

		*entry = gc_alloc(heap, alignof(struct LispSymbol), sizeof **entry);
		**entry = (struct LispSymbol) { { (*entry)->hdr.hdr, LISP_SYMBOL },
			.name = name->s, .len = len, .value = NIL(ctx) };
	}
	return TAG_OBJ(*entry);
}

void lisp_print(struct LispCtx *ctx, LispObject x, FILE *stream) {
	switch (lisp_type(x)) {
	case LISP_INTEGER: fprintf(stream, "%i", UNTAG_SMI(x)); break;
	case LISP_NIL: fputs("nil", stream); break;
	case LISP_PAIR:
		putc('(', stream);
		do lisp_print(ctx, car(ctx, x), stream);
		while (consp(x = cdr(ctx, x)) && (putc(' ', stream), true));
		if (!NILP(ctx, x)) { fputs(" . ", stream); lisp_print(ctx, x, stream); }
		putc(')', stream);
		break;
	case LISP_SYMBOL:
		struct LispSymbol *sym = UNTAG_OBJ(x);
		fwrite(sym->name, sizeof *sym->name, sym->len, stream);
		break;
	case LISP_CFUNCTION:
		fprintf(stream, "#<subr %s>", ((struct LispCFunction *) UNTAG_OBJ(x))->name);
		break;
	case LISP_CLOSURE: fputs("#<closure>", stream); break;
	default: unreachable();
	}
}

static uint64_t lisp_hash(struct LispEntry x) { return fxhash_finish(fxhash(0, x.obj)); }
static bool lisp_equal(struct LispEntry a, struct LispEntry b) { return a.obj == b.obj; }
#define NAME lisp
#define KEY struct LispEntry
#include "tbl.h"

/** Locates the representative of the equivalence class of @a b. */
static struct LispEntry *find(struct Table *table, struct LispEntry *x) {
	for (struct LispEntry *n; !IS_SMI(x->next); x = n) {
		n = lisp_tbl_find(table, (struct LispEntry) { .obj = x->next });
		if (!IS_SMI(n->next)) x->next = n->next; // Path compression by splitting
	}
	return x;
}

static bool union_find(struct Table *table, LispObject x, LispObject y) {
	struct LispEntry *ax, *ay;
	bool has_x = lisp_tbl_entry(table, (struct LispEntry) { .obj = x, .next = TAG_SMI(1) }, &ax),
		has_y = lisp_tbl_entry(table, (struct LispEntry) { .obj = y, .next = x }, &ay);
	if (has_x && has_y) {
		struct LispEntry *rx = find(table, ax), *ry = find(table, ay);
		if (rx == ry) return true;
		if (rx->next < ry->next) { struct LispEntry *tmp = rx; rx = ry; ry = tmp; }
		rx->next += ry->next;
		ry->next = rx->obj;
	} else if (has_x) ay->next = find(table, ax)->obj;
	else if (has_y) ax->next = find(table, ay)->obj;
	return false;
}

#define EQ_PARAMS struct LispCtx *ctx, LispObject a, LispObject b, int *k, struct Table *table
static bool eq_precheck(EQ_PARAMS), eq(EQ_PARAMS);

enum EqualityMode { EQ_PRECHECK, EQ_FAST, EQ_SLOW };
[[gnu::always_inline]] static inline bool eq_check(EQ_PARAMS, enum EqualityMode mode) {
	if (LISP_EQ(a, b)) return true;
	enum LispType ty = lisp_type(a);
	if (lisp_type(b) != ty) return false;
	auto f = mode == EQ_PRECHECK ? eq_precheck : eq;
	switch (ty) {
	case LISP_INTEGER: case LISP_SYMBOL: case LISP_CFUNCTION: case LISP_CLOSURE:
		return false;
	case LISP_PAIR:
		if (mode == EQ_SLOW && union_find(table, a, b)) {
			*k = 0; // If one equivalence was found more are likely to follow
			return true;
		}
		struct LispPair *x = UNTAG_OBJ(a), *y = UNTAG_OBJ(b);
		return (--*k || mode != EQ_PRECHECK)
			&& f(ctx, GC_DECOMPRESS(ctx, x->car), GC_DECOMPRESS(ctx, y->car), k, table)
			&& f(ctx, GC_DECOMPRESS(ctx, x->cdr), GC_DECOMPRESS(ctx, y->cdr), k, table);
	default: unreachable();
	}
}
static bool eq_precheck(EQ_PARAMS) {  return eq_check(ctx, a, b, k, table, EQ_PRECHECK); }
static bool eq_fast(EQ_PARAMS) { return eq_check(ctx, a, b, k, table, EQ_FAST); }
[[gnu::cold]] static bool eq_slow(EQ_PARAMS) { return eq_check(ctx, a, b, k, table, EQ_SLOW); }

#define K0 256
#define KB (-20)

static bool eq(EQ_PARAMS) {
	// Interleave tree-equality checking with union-find.
	//
	// See: ADAMS, Michael D.; DYBVIG, R. Kent. Efficient
	//      nondestructive equality checking for trees and graphs. In:
	//      Proceedings of the 13th ACM SIGPLAN international
	//      conference on Functional programming. 2008. p. 179-188.
	return LIKELY(*k > 0) ? eq_fast(ctx, a, b, k, table)
		: *k <= KB ? *k = table->len % K0, eq_fast(ctx, a, b, k, table)
		: eq_slow(ctx, a, b, k, table);
}

bool lisp_eq(struct LispCtx *ctx, LispObject a, LispObject b) {
	int k = K0;
	bool result = eq_precheck(ctx, a, b, &k, NULL);
	if (k) return result;

	struct Table table = tbl_new();
	result = eq(ctx, a, b, &k, &table);
	lisp_tbl_free(&table);
	return result;
}

bool lisp_signal_handler(int sig, siginfo_t *info, [[maybe_unused]] void *ucontext, struct LispCtx *ctx) {
	if (sig == SIGSEGV) {
		// Check if fault was within the stack guard pages
		if ((uintptr_t) ctx->bp <= (uintptr_t) info->si_addr
			&& (uintptr_t) info->si_addr < ctx->guard_end)
			// Safety: SIGSEGV is a synchronous signal and run() is
			// compiled with -fnon-call-exceptions.
			throw(SIGSEGV); // Throw stack overflow exception
	}
	return false;
}

void lisp_interrupt(struct LispCtx *ctx) {
	// TODO Handle SIGINT by PROT_NONE mprotect:ing the stack and
	// catching the resulting SIGSEGV.
	uintptr_t *stack = (uintptr_t *)ctx->guard_end - (STACK_LEN + MAX_FRAME);
	mprotect(stack, (uintptr_t *)ctx->guard_end - stack, PROT_NONE);
}

static size_t string_size(struct LispString *x) { return sizeof *x + x->len; }

static size_t closure_size(struct Closure *x) {
	return sizeof *x + x->prototype->num_upvalues * sizeof *x->upvalues;
}

static size_t chunk_size(struct Chunk *x) {
	return sizeof *x + x->num_consts * sizeof(LispObject)
		+ x->count * sizeof(struct Instruction);
}

static void lisp_trace(struct GcHeap *heap, bool mark_color, LispObject *v) {
	void *p = UNTAG_OBJ(*v);
	if (!IS_SMI(*v) && GC_TRACE(heap, mark_color, p)) *v = TAG_OBJ(p);
}
static void lisp_trace_compressed(struct GcHeap *heap, bool mark_color, Lobj *v) {
	void *p = UNTAG_OBJ(GC_DECOMPRESS(heap, *v));
	if (!IS_SMI(v->p) && GC_TRACE(heap, mark_color, p)) *v = GC_COMPRESS(TAG_OBJ(p));
}

void gc_object_visit(struct GcHeap *heap, bool mark_color, void *p) {
	switch (((struct LispObjectHeader *) p)->tag) {
	case LISP_INTEGER: default: unreachable();
	case LISP_NIL: break;
	case LISP_PAIR:
		struct LispPair *cell = p;
		gc_mark(sizeof *cell, p);
		lisp_trace_compressed(heap, mark_color, &cell->cdr);
		lisp_trace_compressed(heap, mark_color, &cell->car);
		break;
	case LISP_SYMBOL:
		struct LispSymbol *sym = p;
		gc_mark(sizeof *sym, p);
		void *str = (char *) sym->name - offsetof(struct LispString, s);
		if (GC_TRACE(heap, mark_color, str)) sym->name = ((struct LispString *) str)->s;
		lisp_trace(heap, mark_color, &sym->value);
		break;
	case LISP_STRING: gc_mark(string_size(p), p); break;
	case LISP_CFUNCTION: gc_mark(sizeof(struct LispCFunction), p); break;
	case LISP_CLOSURE: {
		struct Closure *f = p;
		gc_mark(closure_size(f), p);
		for (struct Upvalue **x = f->upvalues,
					**end = x + f->prototype->num_upvalues; x < end; ++x)
			GC_TRACE(heap, mark_color, *x);

		struct Chunk *chunk = prototype_chunk(f->prototype);
		if (GC_TRACE(heap, mark_color, chunk))
			f->prototype = (struct Prototype *)((char *)chunk + f->prototype->offset);
		break;
	}
	case LISP_UPVALUE:
		struct Upvalue *uv = p;
		gc_mark(sizeof *uv, p);
		lisp_trace(heap, mark_color, uv->location);
		break;
	case LISP_BYTECODE_CHUNK:
		struct Chunk *chunk = p;
		gc_mark(chunk_size(p), p);
		for (LispObject *x = chunk_consts(chunk), *end = x + chunk->num_consts;
				x < end; ++x) lisp_trace(heap, mark_color, x);
		break;
	}
}

size_t gc_object_size(void *p, size_t *alignment) {
	switch (((struct LispObjectHeader *) p)->tag) {
	case LISP_PAIR:
		*alignment = alignof(struct LispPair);
		return sizeof(struct LispPair);
	case LISP_SYMBOL:
		*alignment = alignof(struct LispSymbol);
		return sizeof(struct LispSymbol);
	case LISP_STRING:
		*alignment = alignof(struct LispString);
		return string_size(p);
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
	case LISP_INTEGER: case LISP_NIL:
	}
	unreachable();
}

void gc_trace_roots(struct GcHeap *heap, bool mark_color) {
	struct LispCtx *ctx = (struct LispCtx *)heap;

	gc_pin(heap, mark_color, &ctx->nil);
#define X(var, _) gc_pin(heap, mark_color, UNTAG_OBJ(LISP_CONST(ctx, var)));
	FOR_SYMBOL_CONSTS(X)
#undef X

#if ENABLE_JIT
	for (unsigned i = 0; i < LENGTH(*ctx->traces); ++i)
		trace_trace(heap, mark_color, (*ctx->traces)[i]);
#endif

	// Trace stack
	uintptr_t *end = (uintptr_t *)ctx->guard_end - MAX_FRAME,
		*stack = end - STACK_LEN, *top = MIN(ctx->bp + MAX_FRAME, end);
	// Non-LispObjects, i.a. return addresses, masquerade as SMIs via
	// >1-byte alignment.
	for (uintptr_t *x = stack; x < top; ++x) switch (lisp_type(*x)) {
		case LISP_CLOSURE:
			struct Closure *closure = UNTAG_OBJ(*x);
			gc_pin(heap, mark_color, prototype_chunk(closure->prototype));
			[[fallthrough]];
		default: lisp_trace(heap, mark_color, x); break;
		case LISP_BYTECODE_CHUNK: gc_pin(heap, mark_color, UNTAG_OBJ(*x)); break;
	}
	memset(top, 0, end - top); // Zero unused stack to not resurrect GCd object

	struct LispSymbol **sym;
	for (size_t i = 0; symbol_tbl_iter_next(&ctx->symbol_tbl, &i, &sym);)
		GC_TRACE(heap, mark_color, *sym);

	for (struct Upvalue **uv = &ctx->upvalues; *uv; uv = &(*uv)->next)
		GC_TRACE(heap, mark_color, *uv);

	struct LispEntry *constant;
	for (size_t i = 0; lisp_tbl_iter_next(&ctx->consts, &i, &constant);)
		lisp_trace(heap, mark_color, &constant->obj);
}

void lisp_defsubr(struct LispCtx *ctx, const struct LispCFunction *fn) {
	struct LispCFunction *x
		= gc_alloc((struct GcHeap *)ctx, alignof(struct LispCFunction), sizeof *x);
	struct GcObjectHeader hdr = x->hdr.hdr;
	*x = *fn;
	x->hdr.hdr = hdr;
	struct LispSymbol *sym = UNTAG_OBJ(intern(ctx, strlen(x->name), x->name));
	sym->value = TAG_OBJ(x);
}

DEFUN("eval", eval, (struct LispCtx *ctx, LispObject form)) { return lisp_eval(ctx, form); }

DEFUN("print", print, (struct LispCtx *ctx, LispObject x)) {
	lisp_print(ctx, x, stdout);
	putchar('\n');
	return NIL(ctx);
}

DEFUN("=", equal, (struct LispCtx *ctx, LispObject a, LispObject b)) {
	return lisp_eq(ctx, a, b) ? LISP_CONST(ctx, t) : NIL(ctx);
}

DEFUN("cons", cons, (struct LispCtx *ctx, LispObject car, LispObject cdr)) {
	return cons(ctx, car, cdr);
}

DEFUN("consp", consp, (struct LispCtx *ctx, LispObject x)) {
	return consp(x) ? LISP_CONST(ctx, t) : NIL(ctx);
}

DEFUN("car", car, (struct LispCtx *ctx, LispObject x)) { return car(ctx, x); }

DEFUN("cdr", cdr, (struct LispCtx *ctx, LispObject x)) { return cdr(ctx, x); }

DEFUN("+", add, (struct LispCtx *, LispObject a, LispObject b)) {
	if (!(IS_SMI(a) && IS_SMI(b))) throw(1);
	int32_t result;
	if (ckd_add(&result, (int32_t) a, (int32_t) b))
		throw(1);
	return result;
}

DEFUN("<", lt, (struct LispCtx *ctx, LispObject a, LispObject b)) {
	if (!(IS_SMI(a) && IS_SMI(b))) throw(1);
	return (int32_t) a < (int32_t) b ? LISP_CONST(ctx, t) : NIL(ctx);
}

struct LispCtx *lisp_new() {
	struct GcHeap *heap;
	struct LispCtx *ctx;
	if (!(ctx = (struct LispCtx *)(heap = gc_new()))) goto err;
	ctx->nil = (struct LispObjectHeader) { .tag = LISP_NIL };
	ctx->upvalues = NULL;
	ctx->consts = tbl_new();

	// TODO Divide guard pages into yellow and red zones (in HotSpot
	// terminology) where the yellow zone is temporarily disabled for
	// exception handlers not to immediately trigger another overflow.
	size_t size = STACK_LEN * sizeof *ctx->bp, guard_size = MAX_FRAME * sizeof *ctx->bp;
	if ((ctx->bp = mmap(NULL, size + guard_size, PROT_NONE,
				MAP_PRIVATE | MAP_ANONYMOUS, -1, 0)) == MAP_FAILED) goto err_free_heap;
	if (mprotect(ctx->bp, size, PROT_READ | PROT_WRITE)) goto err_free_stack;
	ctx->guard_end = (uintptr_t)ctx->bp + size + guard_size;
	*ctx->bp = 0;

#if ENABLE_JIT
	if (!(ctx->traces = calloc(1, sizeof *ctx->traces))) goto err_free_stack;
	if (!(ctx->jit_state = jit_new())) { free(ctx->traces); goto err_free_stack; }
	ctx->current_trace = NULL;
#endif

	ctx->symbol_tbl = tbl_new();
#ifdef LISP_GENERATED_FILE
#define X(var, sym) intern(ctx, sizeof #sym - 1, #sym);
#else
#define X(var, sym) ctx->var = intern(ctx, sizeof #sym - 1, #sym);
#endif
	FOR_SYMBOL_CONSTS(X)
#undef X

	struct LispCFunction *cfuns[]
		= { &Seval, &Sprint, &Sequal, &Scons, &Sconsp, &Scar, &Scdr, &Sadd, &Slt, };
	for (size_t i = 0; i < LENGTH(cfuns); ++i) lisp_defsubr(ctx, cfuns[i]);

	return ctx;
err_free_stack:
	munmap(ctx->bp, size + guard_size);
err_free_heap:
	gc_free(heap);
err:
	return NULL;
}

void lisp_free(struct LispCtx *ctx) {
#if ENABLE_JIT
	jit_free(ctx->jit_state);
	for (unsigned i = 0; i < LENGTH(*ctx->traces); ++i) trace_free((*ctx->traces)[i]);
	free(ctx->traces);
#endif
	lisp_tbl_free(&ctx->consts);
	symbol_tbl_free(&ctx->symbol_tbl);
	munmap(ctx->bp, ctx->guard_end - (uintptr_t)ctx->bp);
	gc_free((struct GcHeap *)ctx);
}
