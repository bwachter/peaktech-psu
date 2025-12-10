EMACS=emacs

.PHONY: byte-compile clean

clean:
	@rm -f lisp/*.elc

byte-compile: clean
	@$(EMACS) -Q -L ~/.emacs.d/modules -L ../eplot -L lisp/ --batch -f batch-byte-compile lisp/*.el
