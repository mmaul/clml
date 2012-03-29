.PHONY: clean html pdf

clean:
	rm -rf include *.aux *.cp *.fn *.fns *.ky *.log *.pg *.toc *.tp *.tps *.vr *.pdf *.html

# Hook into the super sekrit texinfo generator in the SBCL tree -- this is just a quick way
# to bootrap documentation for now.
include:
	sbcl --eval '(progn (require :asdf) (require :alexandria) (rename-package :alexandria :alexandria))' \
	--eval '(load (merge-pathnames "doc/manual/docstrings" (posix-getenv "SBCL_SOURCE_ROOT")))' \
	--eval '(sb-texinfo:generate-includes "include/" :alexandria)' \
	--eval '(quit)'
	mv include/fun-alexandria-type=.texinfo include/fun-alexandria-type-equal.texinfo

pdf: include
	texi2pdf alexandria.texinfo

html: include
	texi2html alexandria.texinfo
