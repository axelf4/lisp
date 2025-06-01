cmake_minimum_required(VERSION 3.19)
project(lisp C)

set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED ON)

include(CheckCSourceRuns)
include(GNUInstallDirs)
include(CTest)
find_package(cmocka 1.1.0 REQUIRED)

check_c_source_runs("#include <stdlib.h>
int main(void) { void *ptr = &&foo; goto *ptr; return EXIT_FAILURE; foo: }"
  HAVE_COMPUTED_GOTO)

option(WITH_JIT "Enable just-in-time compilation." ON)
option(WITH_TAIL_CALL_INTERP "Enable tail-calling interpreter.")
option(WITH_CHECKPOINT_RESTORE "Enable checkpoint/restore.")

add_compile_definitions(_POSIX_C_SOURCE=200809L)
if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
  add_compile_options(-masm=intel -Wall -Wextra -Wpedantic
	-Wnull-dereference -Wcast-align=strict -Wwrite-strings -Wformat=2)
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
  DEPENDS lisp_codegen
  COMMAND lisp_codegen ${PROJECT_BINARY_DIR}/lisp_generated.h)

add_library(lisplib OBJECT src/gc.c src/lisp.c src/lread.c src/vm.c src/rope.c src/util.c
  $<$<CONFIG:Release>:${PROJECT_BINARY_DIR}/lisp_generated.h>
  $<$<BOOL:${WITH_JIT}>:src/jit.c>
  $<$<BOOL:${WITH_CHECKPOINT_RESTORE}>:src/criu.c>)
target_compile_definitions(lisplib PUBLIC
  $<$<CONFIG:Release>:LISP_GENERATED_FILE="${PROJECT_BINARY_DIR}/lisp_generated.h">
  ENABLE_JIT=$<BOOL:${WITH_JIT}>)
target_compile_definitions(lisplib PRIVATE
  HAVE_COMPUTED_GOTO=$<BOOL:${HAVE_COMPUTED_GOTO}>
  ENABLE_TAIL_CALL_INTERP=$<BOOL:${WITH_TAIL_CALL_INTERP}>
  LIBRESTORE_SO="${CMAKE_INSTALL_FULL_LIBDIR}/${CMAKE_SHARED_LIBRARY_PREFIX}restore${CMAKE_SHARED_LIBRARY_SUFFIX}")

add_executable(lisp src/main.c)
target_link_libraries(lisp lisplib)

if(BUILD_TESTING)
  add_executable(unittest test/test.c src/phf.c)
  target_include_directories(unittest PRIVATE cmocka "${CMAKE_SOURCE_DIR}/src")
  target_link_libraries(unittest cmocka lisplib)
  add_test(unittest unittest)

  add_executable(heap_fuzz test/heap_fuzz.c)
  target_include_directories(heap_fuzz PRIVATE "${CMAKE_SOURCE_DIR}/src")
  add_test(heap_fuzz heap_fuzz)
endif()

install(TARGETS lisp)
