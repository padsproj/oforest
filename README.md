# oforest

OCaml Forest requires OCaml 4.03.0 or greater,
plus a few packages.

You should be able to get everything you need by running:

> opam update

> opam switch 4.03.0

> eval $(opam config env)

> opam install ocamlfind oasis utop menhir core

To build:

> make && make install

To rebuild:

> make && make reinstall

If you get errors, try

> ocamlfind remove forest

You can desugar a Forest source file into vanilla OCaml using

> desugar.sh [file.ml]

Examples:

Each example is in a separate folder in the examples directory.

To compile, desugar, or run example [x], run (in the examples directory)
- Compile: 

> make [x]C

- Desugar: 

> make [x]D

- Run:

> make [x]R

Ex:
For example simple:

> make simpleC
> make simpleD
> make simpleR

See the [wiki](https://github.com/padsproj/oforest/wiki) for the iForest syntax:
