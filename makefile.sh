#!/bin/sh
emacs="$1"; shift
[ -z "$emacs" ] && emacs=`which emacs`
libs="-l buffer-time-stamp.el"

cat source-file-list.txt | while read file; do
	"$emacs" -q --no-site-file \
		$libs \
		-batch -f batch-byte-compile "$file" || exit
done
