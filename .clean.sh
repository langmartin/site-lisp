#!/bin/sh
cat source-file-list.txt | \
while read file; do
	rm "${file}c"
done
