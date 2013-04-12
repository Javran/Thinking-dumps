#!/bin/bash

echo Generating HTML files from markdown files ...

for i in *.md
do
	TARGET=${i%.md}.html
	markdown_py "$i" -f "${TARGET}"
	echo "$i -> ${TARGET}"
done
