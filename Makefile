files = $(shell cat source-file-list.txt)

all: .all.sh $(files)
	./.all.sh

clean:
	./.clean.sh
