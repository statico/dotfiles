# Interface between Emacs Lisp and Python - Makefile.
# Copyright © 2001, 2002, 2003 Progiciels Bourbeau-Pinard inc.
# François Pinard <pinard@iro.umontreal.ca>, 2001.

PYSETUP = python setup.py

emacs = $(PYMACS_EMACS)
python = $(PYMACS_PYTHON)

all:
	$(PYSETUP) build

check:
	$(PYSETUP) clean
	touch .stamp
	cd tests && \
	  PYMACS_EMACS=$(emacs) PYMACS_PYTHON=$(python) ./pytest $(TEST)

install:
	$(PYSETUP) install

clean:
	rm -rf build* Pymacs/*.pyc tests/*.pyc
	rm -f pymacs.el pymacs.rst pymacs.pdf Pymacs/__init__.py

pymacs.pdf: pymacs.rst.in
	$(PYSETUP) clean
	touch .stamp
	rm -rf tmp-pdf
	mkdir tmp-pdf
	rst2latex.py --use-latex-toc --input-encoding=UTF-8 \
	  pymacs.rst tmp-pdf/pymacs.tex
	cd tmp-pdf && pdflatex pymacs.tex
	cd tmp-pdf && pdflatex pymacs.tex
	mv -f tmp-pdf/pymacs.pdf $@
	rm -rf tmp-pdf

# (Note: python setup.py clean is the most no-op thing I could find.)
pymacs.el pymacs.rst Pymacs/__init__.py: .stamp
.stamp: pymacs.el.in pymacs.rst.in __init__.py.in
	$(PYSETUP) clean
	touch .stamp

# The following goals for the maintainer of the Pymacs Web site.

VERSION = `grep '^version' setup.py | sed -e "s/'$$//" -e "s/.*'//"`

local: pymacs.pdf pymacs.rst
	ajuster-web web

publish:
	version=$(VERSION) && \
	  git archive --format=tar --prefix=Pymacs-$$version/ HEAD . \
	    | gzip > web/archives/Pymacs-$$version.tar.gz

official: publish
	rm -f web/archives/Pymacs.tar.gz
	version=$(VERSION) && \
	  ln -s Pymacs-$$version.tar.gz web/archives/Pymacs.tar.gz

synchro: local
	du -s .git
	git gc --prune
	du -s .git
	find -name '*~' | xargs rm -fv
	synchro -PD alcyon entretien
