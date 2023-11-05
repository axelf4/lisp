#include "util.h"
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>

void die(const char *format, ...) {
	va_list vargs;
	va_start(vargs, format);
	vfprintf(stderr, format, vargs);
	va_end(vargs);
	fputc('\n', stderr);
	exit(EXIT_FAILURE);
}

#if USE_SJLJ
#include <setjmp.h>

static thread_local union { jmp_buf *env; unsigned errcode; } exn_handler;

void throw(unsigned code) {
	jmp_buf *env = exn_handler.env;
	exn_handler.errcode = code;
	if (env) longjmp(*env, 1);
	fputs("Uncaught exception\n", stderr);
	abort();
}

unsigned pcall(void *x, void (*f)(void *)) {
	jmp_buf env, *prev_env = exn_handler.env;
	exn_handler.env = &env;
	unsigned result = 0;
	if (setjmp(env)) result = exn_handler.errcode; else f(x);
	exn_handler.env = prev_env;
	return result;
}
#else
/*
 * Stack unwinding for exceptions.
 *
 * See: LU, H. J., et al. System V application binary interface. AMD64
 *      Architecture Processor Supplement, 2018, 588-601.
 */

#include <unwind.h>

/// Our exception class.
#define CLASS 0x4c69737000000000ULL // Lisp\0\0\0\0

static thread_local struct {
	struct _Unwind_Exception exception;
	// Language-specific information used to process the exception:
} uex;

void throw(unsigned errcode) {
	uex.exception.exception_class = CLASS | errcode;
	uex.exception.exception_cleanup = NULL;
	switch (_Unwind_RaiseException(&uex.exception)) {
	case _URC_END_OF_STACK: fputs("Uncaught exception\n", stderr); [[fallthrough]];
	case _URC_FATAL_PHASE1_ERROR: default: abort();
	}
}

/** Personality routine that identifies the frame handling the exception. */
[[gnu::no_split_stack, gnu::flatten, gnu::used]]
static _Unwind_Reason_Code eh_personality(int version, _Unwind_Action actions,
	_Unwind_Exception_Class exception_class, struct _Unwind_Exception *exception_object,
	struct _Unwind_Context *context) {
	if (__builtin_expect(version != 1, false)) return _URC_FATAL_PHASE1_ERROR;

	if (actions & _UA_SEARCH_PHASE) return _URC_HANDLER_FOUND;
	else if (actions & _UA_CLEANUP_PHASE) {
		// Without unwind-protect or equivalent there is no cleanup to do
		if (!(actions & _UA_HANDLER_FRAME)) return _URC_CONTINUE_UNWIND;

		unsigned errcode;
		if ((exception_class & CLASS) == CLASS)
			errcode = exception_class & 0xffffffff;
		else { // Foreign exception
			_Unwind_DeleteException(exception_object);
			errcode = -1;
		}

		extern void pcall_landing_pad();
		_Unwind_SetIP(context, (_Unwind_Ptr) pcall_landing_pad);
		_Unwind_SetGR(context, 0, errcode); // Put error code into %rax
		return _URC_INSTALL_CONTEXT;
	}
	return _URC_FATAL_PHASE1_ERROR;
}

[[gnu::naked]] unsigned pcall(void *, void (*)(void *)) {
	asm ("DW_EH_PE_absptr = 0x00\n\t"
		".cfi_personality DW_EH_PE_absptr, eh_personality\n\t"
		"sub rsp, 8\n\t"
		".cfi_adjust_cfa_offset 8\n\t"
		"call rsi\n\t"
		"xor eax, eax\n\t"
		"pcall_landing_pad:\n\t"
		"add rsp, 8\n\t"
		".cfi_adjust_cfa_offset -8\n\t"
		"ret");
}
#endif
