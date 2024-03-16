cmake_minimum_required(VERSION 3.19)
project(lisp C)

set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED ON)

include(CTest)
find_package(cmocka 1.1.0 REQUIRED)
find_package(xxHash REQUIRED)

add_compile_definitions(_POSIX_C_SOURCE=200809L)
if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
  add_compile_options(-masm=intel -Wall -Wextra -Wpedantic
	-Wnull-dereference -Wcast-align=strict -Wwrite-strings -Wformat=2)
endif()

add_library(lisplib OBJECT src/gc.c src/lisp.c src/lread.c src/vm.c src/rope.c src/util.c)
target_include_directories(lisplib PRIVATE xxHash::xxhash)
target_link_libraries(lisplib xxHash::xxhash)

add_executable(lisp src/main.c)
target_link_libraries(lisp lisplib)

if(BUILD_TESTING)
  add_executable(unittest test/test.c)
  target_include_directories(unittest PRIVATE cmocka "${CMAKE_SOURCE_DIR}/src")
  target_link_libraries(unittest cmocka lisplib)
  add_test(unittest unittest)
endif()

install(TARGETS lisp)
