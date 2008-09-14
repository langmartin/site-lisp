#!/bin/sh
for file in "$@"; do
	rm -f "${file}c"
done
