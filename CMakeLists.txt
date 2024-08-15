cmake_minimum_required(VERSION 3.19)
project(lisp C)

set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED ON)

include(CheckCSourceRuns)
include(CTest)
find_package(cmocka 1.1.0 REQUIRED)
find_package(xxHash REQUIRED)

check_c_source_runs("#include <stdlib.h>
int main(void) { void *ptr = &&foo; goto *ptr; return EXIT_FAILURE; foo: }"
  HAVE_COMPUTED_GOTO)
if(NOT HAVE_COMPUTED_GOTO)
  message(FATAL_ERROR "Computed goto support is required")
endif()

add_compile_definitions(_POSIX_C_SOURCE=200809L)
if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
  add_compile_options(-masm=intel -Wall -Wextra -Wpedantic
	-Wnull-dereference -Wcast-align=strict -Wwrite-strings -Wformat=2)
endif()

add_executable(lisp-codegen EXCLUDE_FROM_ALL src/lisp-codegen.c)
target_include_directories(lisp-codegen PRIVATE xxHash::xxhash)
target_link_libraries(lisp-codegen xxHash::xxhash)
add_custom_command(
  OUTPUT ${PROJECT_BINARY_DIR}/lisp-generated.h
  DEPENDS lisp-codegen
  COMMAND lisp-codegen ${PROJECT_BINARY_DIR}/lisp-generated.h)

add_library(lisplib OBJECT src/gc.c src/lisp.c src/lread.c src/vm.c src/rope.c src/util.c
  $<$<CONFIG:Release>:${PROJECT_BINARY_DIR}/lisp-generated.h>)
target_include_directories(lisplib PRIVATE xxHash::xxhash)
target_link_libraries(lisplib xxHash::xxhash)
target_compile_definitions(lisplib
  PUBLIC $<$<CONFIG:Release>:LISP_GENERATED_FILE="${PROJECT_BINARY_DIR}/lisp-generated.h">)

add_executable(lisp src/main.c)
target_link_libraries(lisp lisplib)

if(BUILD_TESTING)
  add_executable(unittest test/test.c src/phf.c)
  target_include_directories(unittest PRIVATE cmocka "${CMAKE_SOURCE_DIR}/src")
  target_link_libraries(unittest cmocka lisplib)
  add_test(unittest unittest)
endif()

install(TARGETS lisp)
