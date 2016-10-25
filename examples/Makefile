SHELL = /bin/bash

# Running make [example] should create an executable called [example].native
# The SWAT example builds in its own directory.

EXAMPLES = oopsla simple paperEx universal simpleShell accounts

.SUFFIXES: .ml .native .byte

.PHONY : clean all swat $(EXAMPLES)

all: $(EXAMPLES)

swat:
	cd swat; make

$(EXAMPLES) : % : %.native

.SECONDEXPANSION:
%.native: $$*/$$*.ml
	ocamlbuild -use-ocamlfind -I $* $@
	touch $@

%.byte: $$*/$$*.ml
	ocamlbuild -use-ocamlfind -I $* $@
	touch $@

clean:
	ocamlbuild -clean
