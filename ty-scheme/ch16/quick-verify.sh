#!/bin/bash

dd if=/dev/urandom of=src-file count=1234 bs=1

./guile-file-split.scm src-file tgt-file 121

SPLIT_COUNT=`dc -e "1234 121/ 1+p"`

echo "Listing file size for each splitted file ..."

for ((i=1; i<=$SPLIT_COUNT; ++i))
do
	du -b "tgt-file.$i" | cut -f 1
	cat "tgt-file.$i" >>tgt-file_all
done

diff tgt-file_all src-file

if (($? == 0)); then
	echo "Test passed";
else
	echo "Test failed";
fi

rm -f src-file tgt-file*
