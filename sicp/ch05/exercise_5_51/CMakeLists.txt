cmake_minimum_required (VERSION 2.6)
project (evaluator)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -Wall -Wextra -pedantic -std=c99")

file(GLOB evaluator_SRC "*.h" "*.c")
add_executable(evaluator ${evaluator_SRC})