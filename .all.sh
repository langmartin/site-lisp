#!/bin/sh
[ -z "$emacs" ] && emacs=`which emacs`
libs="-l buffer-time-stamp.el -L ."

for file in "$@"; do
	[ "$file" -nt "${file}c" ] || continue
	echo "$file: "
	"$emacs" -q --no-site-file \
		$libs \
		-batch -f batch-byte-compile "$file" || exit
done
