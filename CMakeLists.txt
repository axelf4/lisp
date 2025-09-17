cmake_minimum_required(VERSION 3.31)
project(lisp C)

set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED ON)

include(CheckCSourceRuns)
include(GNUInstallDirs)
include(CTest)
find_package(LTTngUST 2.14.0)
find_package(cmocka 1.1.0)

check_c_source_runs("#include <stdlib.h>
int main(void) { void *ptr = &&foo; goto *ptr; return EXIT_FAILURE; foo: }"
  HAVE_COMPUTED_GOTO)
check_c_source_runs([[int main() { register int x __asm__ ("rbp");
  __asm__ volatile ("xor %%ebp, %%ebp" : "=r" (x)); }]] HAVE_GENERAL_PURPOSE_RBP)
if(NOT HAVE_GENERAL_PURPOSE_RBP)
  set(preserve_frame_pointer TRUE)
endif()

set(LG_PAGE "Detect" CACHE STRING "Explicit log2 page size; \"Detect\" or \"Dynamic\".")
option(WITH_JIT "Enable just-in-time compilation." ON)
option(WITH_TAIL_CALL_INTERP "Enable tail-calling interpreter.")
option(PRESERVE_FRAME_POINTER "Disable RBP usage as a general-purpose register."
  ${preserve_frame_pointer})
option(WITH_CHECKPOINT_RESTORE "Enable checkpoint/restore.")
option(WITH_TRACING "Enable tracing instrumentation." ${LTTngUST_FOUND})

add_compile_definitions(_POSIX_C_SOURCE=200809L)
if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
  add_compile_options(-masm=intel -Wall -Wextra -Wpedantic
	-Wnull-dereference -Wcast-align=strict -Wwrite-strings -Wformat=2)
endif()

if(LG_PAGE STREQUAL "Detect")
  set(_source "#include <stdbit.h>
#include <stdio.h>
#include <unistd.h>
int main() { printf(\"%u\", stdc_trailing_zeros((size_t) sysconf(_SC_PAGESIZE))); }")
  try_run(GETPAGESIZE_EXITCODE GETPAGESIZE_COMPILED
	SOURCE_FROM_VAR "src.c" _source RUN_OUTPUT_VARIABLE LG_PAGE)
  if(NOT (GETPAGESIZE_COMPILED AND GETPAGESIZE_EXITCODE EQUAL 0))
	message(FATAL_ERROR "Failed to detect page size.")
  endif()
endif()

if(WITH_CHECKPOINT_RESTORE)
  add_library(restore MODULE src/criu.c)
  target_compile_definitions(restore PRIVATE IS_RESTORER_DSO)
  target_compile_options(restore PRIVATE
	-ffreestanding -fno-stack-protector -nostdlib -fno-sanitize=all)

  install(TARGETS restore LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} NAMELINK_SKIP)
endif()

add_executable(lisp_codegen EXCLUDE_FROM_ALL src/lisp_codegen.c)
add_custom_command(
  OUTPUT ${PROJECT_BINARY_DIR}/lisp_generated.h
  COMMAND lisp_codegen ${PROJECT_BINARY_DIR}/lisp_generated.h
  DEPENDS lisp_codegen VERBATIM CODEGEN)

add_library(lisplib OBJECT src/gc.c src/lisp.c src/lread.c src/vm.c src/rope.c src/util.c
  $<$<CONFIG:Release>:${PROJECT_BINARY_DIR}/lisp_generated.h>
  $<$<BOOL:${WITH_JIT}>:src/jit.c>
  $<$<BOOL:${WITH_CHECKPOINT_RESTORE}>:src/criu.c>
  $<$<BOOL:${WITH_TRACING}>:src/lisp_tracepoint.c>)
target_compile_definitions(lisplib PUBLIC
  $<IF:$<STREQUAL:${LG_PAGE},Dynamic>,,LG_PAGE=${LG_PAGE}>
  $<$<CONFIG:Release>:LISP_GENERATED_FILE="${PROJECT_BINARY_DIR}/lisp_generated.h">
  ENABLE_JIT=$<BOOL:${WITH_JIT}>)
target_compile_definitions(lisplib PRIVATE
  HAVE_COMPUTED_GOTO=$<BOOL:${HAVE_COMPUTED_GOTO}>
  ENABLE_TAIL_CALL_INTERP=$<BOOL:${WITH_TAIL_CALL_INTERP}>
  PRESERVE_FRAME_POINTER=$<BOOL:${PRESERVE_FRAME_POINTER}>
  ENABLE_TRACING=$<BOOL:${WITH_TRACING}>
  LIBRESTORE_SO="${CMAKE_INSTALL_FULL_LIBDIR}/${CMAKE_SHARED_LIBRARY_PREFIX}restore${CMAKE_SHARED_LIBRARY_SUFFIX}")
target_include_directories(lisplib PRIVATE src)
target_link_libraries(lisplib PRIVATE $<$<BOOL:${WITH_TRACING}>:LTTng::UST>)

add_executable(lisp src/main.c)
target_link_libraries(lisp lisplib)

if(BUILD_TESTING)
  add_executable(unittest test/test.c src/phf.c)
  target_include_directories(unittest PRIVATE src)
  target_link_libraries(unittest cmocka::cmocka lisplib)
  add_test(unittest unittest)

  add_executable(heap_fuzz test/heap_fuzz.c)
  target_include_directories(heap_fuzz PRIVATE src)
  add_test(heap_fuzz heap_fuzz)
endif()

install(TARGETS lisp)
install(FILES lisp-gdb.py TYPE BIN RENAME $<TARGET_FILE_NAME:lisp>-gdb.py)
