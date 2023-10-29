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
