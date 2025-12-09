EMACS=emacs

.PHONY: byte-compile

byte-compile:
	@$(EMACS) -Q -L lisp/ --batch -f batch-byte-compile lisp/*.el
