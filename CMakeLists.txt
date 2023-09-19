cmake_minimum_required(VERSION 3.19)
project(gc C)

add_executable(gc-test main.c gc.c)
set_target_properties(gc-test PROPERTIES
  C_STANDARD 23)
