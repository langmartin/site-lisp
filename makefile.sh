#!/bin/sh
emacs="$1"; shift
[ -z "$emacs" ] && emacs=`which emacs`
libs="-l buffer-time-stamp.el -L ."

cat source-file-list.txt | while read file; do
	[ "$file" -nt "${file}c" ] || continue
	echo "$file: "
	"$emacs" -q --no-site-file \
		$libs \
		-batch -f batch-byte-compile "$file" || exit
done
