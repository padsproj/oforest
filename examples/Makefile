SHELL = /bin/bash

EXAMPLES = oopsla simple paperEx accounts universal dircomp

.SUFFIXES: .ml .native .byte


.PHONY : clean all oopsla simple paperEx accounts universal dircomp

all: oopsla simple paperEx accounts universal


%.native: 
	ocamlbuild -use-ocamlfind -I $(subst .native,,$@) $@

%.byte: 
	ocamlbuild -use-ocamlfind -I $(subst .byte,,$@) $@



oopsla: oopsla.native
simple: simple.native
paperEx: paperEx.native
accounts: accounts.native
universal: universal.native

dircomp: dircomp.native

clean:
	ocamlbuild -clean
