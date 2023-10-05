cmake_minimum_required(VERSION 3.19)
project(lisp C)

set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED ON)

enable_testing()

find_package(cmocka 1.1.0 REQUIRED)
find_package(roaring REQUIRED)
find_package(xxHash REQUIRED)

add_compile_options(-Wall -Wextra -pedantic -Wnull-dereference -Wcast-align=strict -Wwrite-strings -Wformat=2 -fsanitize=address,undefined)
add_link_options(-fsanitize=address,undefined)

add_library(lisplib OBJECT src/gc.c src/lisp.c src/lread.c src/vm.c src/rope.c src/util.c src/tui.c)
target_include_directories(lisplib PRIVATE roaring::roaring xxHash::xxhash)
target_link_libraries(lisplib roaring::roaring xxHash::xxhash)

add_executable(lisp src/main.c)
target_link_libraries(lisp lisplib)

add_executable(unittest test/test.c)
target_include_directories(unittest PRIVATE cmocka "${CMAKE_SOURCE_DIR}/src")
target_link_libraries(unittest cmocka lisplib)
add_test(unittest unittest)

install(TARGETS lisp)
