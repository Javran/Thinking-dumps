#!/bin/bash

# let's just keep it simple:
# *  run "test.scm" on the evaluator
# * compare the output for correctness
# * use valgrind and run "test.scm" again for testing resource management

VALGRIND_ARGS="--trace-children=yes --leak-check=full --error-exitcode=1"

# Test evaluator by running it using a test program

make evaluator || exit 1

if ./evaluator test.scm | diff - test-expected.out;
then
    echo "Test passed"
else
    echo "Test failed"
    exit 1
fi

if valgrind $VALGRIND_ARGS -- ./evaluator test.scm;
then
    echo "Memcheck passed"
else
    echo "Memcheck failed"
    exit 1
fi
