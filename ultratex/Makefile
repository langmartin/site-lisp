# 'make most' byte-compiles the main el files: aucify, ultex,
#             ultex-setup
# 'make extras' byte-compiles font-latex
# 'make all' byte-compiles all of these
EMACS = emacs
RM = rm -f
FLAGS = -batch -l docomp.el -f batch-byte-compile 
EXTRA_FLAGS = -batch -l docomp.el -f ultex-byte-compile-quietly

SOURCES = aucify.el light.el ultex.el ultex-setup.el ultex-cus.el
# You might run into problems when byte-compiling tex-buf.el if
# Emacs can't find the custom library.  This shouldn't be a problem
# with GNU Emacs 20.1, XEmacs 19.15 (?) or any version of Emacs in
# which the custom package is in the default load-path.
EXTRAS = tex-buf.el font-latex.el
# Contrariwise, "font-lock support for TeX is built into GNU Emacs 20.1,
# so font-latex.el is no longer needed."
TEX-BUF = tex-buf.el

all:    most extras

most: 
	$(EMACS) $(FLAGS) $(SOURCES)

extras:
	$(EMACS) $(EXTRA_FLAGS) $(EXTRAS)

clean:
	$(RM) *.elc *~

distclean: clean
