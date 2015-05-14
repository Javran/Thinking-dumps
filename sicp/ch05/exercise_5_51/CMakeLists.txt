cmake_minimum_required (VERSION 2.6)
project (evaluator)
file(GLOB evaluator_SRC "*.h" "*.c")
add_executable(evaluator ${evaluator_SRC})