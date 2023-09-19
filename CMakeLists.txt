cmake_minimum_required(VERSION 3.19)
project(gc C)

find_package(roaring REQUIRED)

add_executable(gc-test main.c gc.c)
set_target_properties(gc-test PROPERTIES C_STANDARD 23)
target_link_libraries(gc-test PRIVATE roaring::roaring)
target_include_directories(gc-test PRIVATE roaring::roaring)
