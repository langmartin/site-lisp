files = $(shell cat source-file-list.txt)

js2-mode.elc: js2-mode.el
	./.clean.sh $(files)
	./.all.sh js2-mode.el

all: .all.sh $(files)
	./.all.sh $(files)

clean:
	./.clean.sh $(files)
