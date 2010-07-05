# -*- Makefile -*- $Id: Makefile,v 1.8 2002/10/12 09:54:09 torsten Exp $

EMACS=emacs

VERSION=1.8
PACKAGE=dictionary
TYPE=comm
XEMACS-PACKAGE=$(PACKAGE)-$(VERSION)-pkg.tar.gz

SOURCES=dictionary.el connection.el link.el
COMPILED=dictionary.elc connection.elc link.elc

# For make dist
LISP_FILES=connection.el dictionary.el link.el lpath.el dictionary-init.el \
           install-package.el
DOC_FILES=README GPL Makefile
DEB_FILES=README.debian control copyright install.debian postinst \
          prerm remove.debian rules changelog dirs
DEB_DIR=deb

.SUFFIXES: .elc .el

.el.elc:
	$(EMACS) -q -no-site-file -no-init-file -batch -l lpath.el \
	-f batch-byte-compile $<

.PHONY: all
all: $(COMPILED)

.PHONY: debian
debian:
	[ -x debian ] || ln -s deb debian
	@if [ -x /usr/bin/fakeroot ]; then \
	  dpkg-buildpackage -us -uc -rfakeroot; \
	elif [ `id -u` -ne 0 ]; then \
	  echo "You are not root and fakeroot is not installed, aborting"; \
	  exit 1; \
	else \
	  dpkg-buildpackage -us -uc; \
	fi
	@echo "You can now install the debian package, the previous output tells"
	@echo "you its location (probably stored in ..)"
	@echo
	@echo "Please note, this debian package is unofficial, report bugs"
	@echo "to me only, not to the Debian Bugtracking System."

.PHONY: package
package: $(XEMACS-PACKAGE) 

$(XEMACS-PACKAGE): $(COMPILED)
	@case $(EMACS) in emacs*) printf "\aNote, packages work with XEmacs 21 only, hope you know what you are doing\n\n";; esac
	@mkdir -p lisp/$(PACKAGE)
	@mkdir -p pkginfo
	@printf ";;;###autoload\n(package-provide '$(PACKAGE)\n:version $(VERSION)\n:type '$(TYPE))\n" > lisp/$(PACKAGE)/_pkg.el
	@rm -f lisp/$(PACKAGE)/auto-autoloads.el lisp/$(PACKAGE)/custom-load.el
	@cp $(SOURCES) $(COMPILED) lisp/$(PACKAGE)
	@cd lisp &&  \
	$(EMACS) -vanilla -batch -l autoload -f batch-update-directory $(PACKAGE) && \
	$(EMACS) -vanilla -batch -l cus-dep -f Custom-make-dependencies $(PACKAGE) && \
	$(EMACS) -vanilla -batch -f batch-byte-compile $(PACKAGE)/auto-autoloads.el $(PACKAGE)/custom-load.el
	@touch pkginfo/MANIFEST.$(PACKAGE)
	@find lisp pkginfo -type f > pkginfo/MANIFEST.$(PACKAGE)
	@tar cf - pkginfo lisp | gzip -c > $(XEMACS-PACKAGE)

.PHONY: package-install
package-install: package
	@if [ `id -u` -ne 0 ]; then printf "\aWarning, you are not root; the installation might fail\n\n"; fi
	@$(EMACS) -vanilla -batch -l install-package.el -f install-package `pwd`/$(XEMACS-PACKAGE)

.PHONY: view-info
view-info: info
	info doc/dictionary.info

.PHONY: doc
doc: info html

.PHONY: info
info: doc/dictionary.info

doc/dictionary.info: doc/dictionary.texi
	cd doc && makeinfo --output dictionary.info dictionary

.PHONY: html
html: doc/dictionary

doc/dictionary: doc/dictionary.texi
	cd doc && makeinfo --html dictionary

.PHONY: dist
dist:
	@[ -x debian ] || ln -s deb debian; \
	VERSION=$$(dpkg-parsechangelog | perl -n -e '/^Version: (.*)-/ && print "$$1\n"'); \
	DIR=$$(mktemp -d); \
	DESTDIR="$$DIR/dictionary-$$VERSION"; \
	install -d $$DESTDIR; \
	install $(LISP_FILES) $(DOC_FILES) $$DESTDIR; \
	mkdir $$DESTDIR/$(DEB_DIR); \
	cd $(DEB_DIR) && install $(DEB_FILES) $$DESTDIR/$(DEB_DIR); \
	tar czf $(CURDIR)/dictionary-$$VERSION.tar.gz -C $$DIR .; \
	rm -r $$DIR; \
	echo "dictionary-$$VERSION.tar.gz has been created"

.PHONY: clean
clean:
	rm -f $(XEMACS-PACKAGE) $(COMPILED) build
	rm -rf debian/tmp lisp pkginfo
