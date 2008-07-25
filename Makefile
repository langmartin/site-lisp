files = $(shell cat source-file-list.txt)
all: makefile.sh $(files)
	./makefile.sh
