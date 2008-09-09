#!/bin/sh
< source-file-list.txt | \
while read file; do
	rm "${file%el}".elc
done
