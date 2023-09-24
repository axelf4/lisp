cmake_minimum_required(VERSION 3.19)
project(gc C)

enable_testing()

find_package(cmocka 1.1.0 REQUIRED)
find_package(roaring REQUIRED)
find_package(xxHash REQUIRED)

add_compile_options(-Wall -Wextra -pedantic)

add_library(lisp gc.c lisp.c lread.c)
set_target_properties(lisp PROPERTIES C_STANDARD 23)
target_include_directories(lisp PRIVATE roaring::roaring xxHash::xxhash)
target_link_libraries(lisp PRIVATE roaring::roaring xxHash::xxhash)

add_executable(gc-test main.c)
set_target_properties(gc-test PROPERTIES C_STANDARD 23)
target_link_libraries(gc-test PRIVATE lisp)

add_executable(unittest test/test.c)
set_target_properties(unittest PROPERTIES C_STANDARD 23)
target_include_directories(unittest PRIVATE cmocka "${CMAKE_SOURCE_DIR}")
target_link_libraries(unittest PRIVATE cmocka lisp)
add_test(unittest unittest)
