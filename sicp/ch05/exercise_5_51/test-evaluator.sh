#!/bin/bash

# Test evaluator by running it using a test program

make evaluator || exit -1

./evaluator test.scm

# TODO:
# * standard output
# * output comparison
# * valgrind
