EMACS ?= emacs
LOAD_PATH ?=

.PHONY: test
test:
	$(EMACS) --batch -Q --eval '(setq load-prefer-newer t)' -L . $(LOAD_PATH) \
	  -l tests/org-quiver-test.el -l tests/org-quiver-export-test.el \
	  -f ert-run-tests-batch-and-exit
