/** LTTng tracepoint provider. */

#if !defined LISP_TRACEPOINT_H || defined LTTNG_UST_TRACEPOINT_HEADER_MULTI_READ
#define LISP_TRACEPOINT_H

#if ENABLE_TRACING
#define LTTNG_UST_TRACEPOINT_PROVIDER lisp
#define LTTNG_UST_TRACEPOINT_INCLUDE "lisp_tracepoint.h"

#include <lttng/tracepoint.h>
#include "lisp.h"

LTTNG_UST_TRACEPOINT_ENUM(lisp, gc_kind, LTTNG_UST_TP_ENUM_VALUES(
		lttng_ust_field_enum_auto("MINOR") lttng_ust_field_enum_auto("MAJOR")
		lttng_ust_field_enum_auto("DEFRAG")))

LTTNG_UST_TRACEPOINT_EVENT(lisp, garbage_collection,
	LTTNG_UST_TP_ARGS(int, kind),
	LTTNG_UST_TP_FIELDS(
		lttng_ust_field_enum(lisp, gc_kind, int, kind, kind)))

LTTNG_UST_TRACEPOINT_EVENT(lisp, record_nyi,
	LTTNG_UST_TP_ARGS(uint8_t, op),
	LTTNG_UST_TP_FIELDS(
		lttng_ust_field_integer(uint8_t, op, op)))

LTTNG_UST_TRACEPOINT_EVENT(lisp, side_exit,
	LTTNG_UST_TP_ARGS(struct LispTrace *, trace, uint8_t, exit),
	LTTNG_UST_TP_FIELDS(
		lttng_ust_field_integer_hex(uintptr_t, trace, (uintptr_t) trace)
		lttng_ust_field_integer(uint8_t, exit, exit)))

#include <lttng/tracepoint-event.h>
#else
#define lttng_ust_tracepoint(provider, name, ...) do {} while (0)
#endif

#endif
