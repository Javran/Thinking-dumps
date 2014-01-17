#!/bin/bash

# change all files in a directory from "*.scm" to "*.rkt"

function print_usage {
    echo "argument: <the dir>"
}

if [ -z $1 ]; then
    print_usage
    exit 1
fi

cd $1

rename -v .scm .rkt *.scm

for i in *.rkt
do
    echo "modifying $i"
    sed -i 's/\.scm/\.rkt/g' $i
done
