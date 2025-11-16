#include "lisp.h"
#include <stdckdint.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include "fxhash.h"
#include "util.h"

#define STACK_LEN 0x1000

#define COMMA ,
#define NUM_ARGS_IMPL(_8, _7, _6, _5, _4, _3, _2, _1, n, ...) n
#define NUM_ARGS(...) NUM_ARGS_IMPL(__VA_ARGS__ __VA_OPT__(,) 7, 6, 5, 4, 3, 2, 1, 0)
#define MAP_ARGS_IMPL(_8, _7, _6, _5, _4, _3, _2, _1, n, ...) n
#define MAP_ARGS(...) MAP_ARGS_IMPL(__VA_ARGS__ __VA_OPT__(,) 7, 6, 5, 4, 3, *_args COMMA _args[1], *_args, )

#define DEFUN(lname, cname, args, ...)									\
	static LispObject _ ## cname args;									\
	static LispObject F ## cname(struct LispCtx *ctx, size_t n, const LispObject _args[static n]) { \
		if (n != NUM_ARGS args) throw(1);								\
		return _ ## cname(ctx, MAP_ARGS args);							\
	}																	\
	static struct LispCFunction S ## cname = {							\
		.hdr.tag = LISP_CFUNCTION, .nargs = NUM_ARGS args,				\
		.f = F ## cname,												\
		.name = lname,													\
	};																	\
	static LispObject _ ## cname args

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
	unsigned int len;
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

bool lisp_eq(struct LispCtx *ctx, LispObject a, LispObject b) {
	if (LISP_EQ(a, b)) return true;
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
			// Safety: SIGSEGV is a synchronous signal and run is
			// compiled with -fnon-call-exceptions.
			throw(SIGSEGV); // Throw stack overflow exception
	}
	return false;
}

void lisp_interrupt(struct LispCtx *ctx) {
	// TODO Handle SIGINT by PROT_NONE mprotect:ing the stack and
	// catching the resulting SIGSEGV.
	uintptr_t *stack = (uintptr_t *) ctx->guard_end - (STACK_LEN + 0xff);
	mprotect(stack, (uintptr_t *) ctx->guard_end - stack, PROT_NONE);
}

static size_t string_size(struct LispString *x) { return sizeof *x + x->len; }

static size_t closure_size(struct Closure *x) {
	return sizeof *x + x->prototype->num_upvalues * sizeof *x->upvalues;
}

static size_t chunk_size(struct Chunk *x) {
	return sizeof *x + x->num_consts * sizeof(LispObject)
		+ x->count * sizeof(struct Instruction);
}

static void lisp_trace(struct GcHeap *heap, LispObject *p) {
	if (!IS_SMI(*p)) *p = TAG_OBJ(gc_trace(heap, UNTAG_OBJ(*p)));
}
static void lisp_trace_compressed(struct GcHeap *heap, Lobj *p) {
	if (IS_SMI(p->p)) return;
	*p = GC_COMPRESS(TAG_OBJ(gc_trace(heap, UNTAG_OBJ(GC_DECOMPRESS(heap, *p)))));
}

void gc_object_visit(struct GcHeap *heap, void *p) {
	switch (((struct LispObjectHeader *) p)->tag) {
	case LISP_INTEGER: default: unreachable();
	case LISP_NIL: break;
	case LISP_PAIR:
		struct LispPair *pair = p;
		gc_mark(sizeof *pair, p);
		lisp_trace_compressed(heap, &pair->cdr);
		lisp_trace_compressed(heap, &pair->car);
		break;
	case LISP_SYMBOL:
		struct LispSymbol *sym = p;
		gc_mark(sizeof *sym, p);
		struct LispString *str
			= gc_trace(heap, (char *) sym->name - offsetof(struct LispString, s));
		sym->name = str->s;
		lisp_trace(heap, &sym->value);
		break;
	case LISP_STRING: gc_mark(string_size(p), p); break;
	case LISP_CFUNCTION: gc_mark(sizeof(struct LispCFunction), p); break;
	case LISP_CLOSURE: {
		struct Closure *x = p;
		gc_mark(closure_size(x), p);
		for (struct Upvalue **it = x->upvalues,
					**end = it + x->prototype->num_upvalues; it < end; ++it)
			*it = gc_trace(heap, *it);

		char *chunk = gc_trace(heap, (char *) x->prototype - x->prototype->offset);
		// Update prototype as chunk may have moved
		x->prototype = (struct Prototype *) (chunk + x->prototype->offset);
		break;
	}
	case LISP_UPVALUE:
		struct Upvalue *x = p;
		gc_mark(sizeof *x, p);
		if (x->is_closed) lisp_trace(heap, x->location);
		else if (x->next) x->next = gc_trace(heap, x->next);
		break;
	case LISP_BYTECODE_CHUNK:
		struct Chunk *chunk = p;
		gc_mark(chunk_size(p), p);
		for (LispObject *x = chunk_constants(chunk), *end = x + chunk->num_consts;
				x < end; ++x) lisp_trace(heap, x);
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

void gc_trace_roots(struct GcHeap *heap) {
	struct LispCtx *ctx = (struct LispCtx *) heap;

	gc_pin(heap, &ctx->nil);
#define X(var, _) gc_pin(heap, UNTAG_OBJ(LISP_CONST(ctx, var)));
	FOR_SYMBOL_CONSTS(X)
#undef X

	struct LispSymbol **sym;
	for (size_t i = 0; symbol_tbl_iter_next(&ctx->symbol_tbl, &i, &sym);)
		*sym = gc_trace(heap, *sym);

	for (struct Upvalue **uv = &ctx->upvalues; *uv; uv = &(*uv)->next)
		*uv = gc_trace(heap, *uv);

	// Trace stack
	uintptr_t *end = (uintptr_t *) ctx->guard_end - 0xff,
		*stack = end - STACK_LEN, *top = MIN(ctx->bp + 0x100, end);
	// Non-LispObjects, i.a. return addresses, masquerade as SMIs by
	// >1-byte alignment.
	for (uintptr_t *x = stack; x < top; ++x) lisp_trace(heap, x);
	// Zero unused stack to not resurrect GCd object
	memset(top, 0, end - top);
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

bool lisp_init(struct LispCtx *ctx) {
	// TODO Divide guard pages into yellow and red zones (in HotSpot
	// terminology) where the yellow zone is temporarily disabled for
	// exception handlers not to immediately trigger another overflow.
	uintptr_t *stack;
	size_t size = STACK_LEN * sizeof *stack, guard_size = 0xff * sizeof *stack;
	if ((ctx->bp = stack = mmap(NULL, size + guard_size, PROT_NONE,
				MAP_PRIVATE | MAP_ANONYMOUS, -1, 0)) == MAP_FAILED) goto err;
	if (mprotect(stack, size, PROT_READ | PROT_WRITE)) goto err_free_stack;
	ctx->guard_end = (uintptr_t) stack + size + guard_size;
	*stack = 0;

	ctx->nil = (struct LispObjectHeader) { .tag = LISP_NIL };
	ctx->upvalues = NULL;

	ctx->symbol_tbl = tbl_new();
#ifdef LISP_GENERATED_FILE
#define X(var, sym) intern(ctx, sizeof #sym - 1, #sym);
#else
#define X(var, sym) ctx->var = intern(ctx, sizeof #sym - 1, #sym);
#endif
	FOR_SYMBOL_CONSTS(X)
#undef X

	struct GcHeap *heap = (struct GcHeap *) ctx;
	struct LispCFunction *cfuns[]
		= { &Seval, &Sprint, &Sequal, &Scons, &Sconsp, &Scar, &Scdr, &Sadd, &Slt, };
	for (size_t i = 0; i < LENGTH(cfuns); ++i) {
		struct LispCFunction *x = gc_alloc(heap, alignof(struct LispCFunction), sizeof *x);
		struct GcObjectHeader hdr = x->hdr.hdr;
		*x = *cfuns[i];
		x->hdr.hdr = hdr;
		struct LispSymbol *sym = UNTAG_OBJ(intern(ctx, strlen(x->name), x->name));
		sym->value = TAG_OBJ(x);
	}

#if ENABLE_JIT
	if (!(ctx->traces = calloc(1, sizeof *ctx->traces))) goto err_free_stack;
	if (!(ctx->jit_state = jit_new())) goto err_free_traces;
	ctx->current_trace = NULL;
	return true;
err_free_traces:
	free(ctx->traces);
#else
	return true;
#endif
err_free_stack:
	munmap(stack, size + guard_size);
err:
	return false;
}

void lisp_free(struct LispCtx *ctx) {
#if ENABLE_JIT
	jit_free(ctx->jit_state);
	for (unsigned i = 0; i < LENGTH(*ctx->traces); ++i) free((*ctx->traces)[i]);
	free(ctx->traces);
#endif
	symbol_tbl_free(&ctx->symbol_tbl);
	munmap(ctx->bp, ctx->guard_end - (uintptr_t) ctx->bp);
}
