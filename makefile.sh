#!/bin/sh
libs="-l tramp -l buffer-time-stamp"

cat source-file-list.txt | while read file; do
	emacs -q --no-site-file $libs -batch -f batch-byte-compile "$file" || exit
done
