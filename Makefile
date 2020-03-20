.PHONY : clean distclean test version

EMACS ?= emacs
CASK ?= cask

LOADPATH = -L .

ELPA_DIR = $(shell EMACS=$(EMACS) $(CASK) package-directory)

version: elpa
	$(CASK) exec $(EMACS) --version

test: elpa
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) \
		-l test/test.el \
		-f ert-run-tests-batch-and-exit

elpa: $(ELPA_DIR)
$(ELPA_DIR): Cask
	$(CASK) install
	touch $@

clean:
	rm -rf $(ELPA_DIR)

distclean:
	rm -rf .cask
