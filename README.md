# Introduction to OCaml Forest

## What is OCaml Forest?

OCaml Forest is a data-description language embedded in OCaml using PPX
extension points. It can be used to easily generate safe load and store
functions for specified data.  OCaml Forest is an OCaml implementation of
[Forest][forestproj].

OCaml Forest is extended to be [incremental][iForest] allowing programmers to
delay subspecifications that should not be loaded with the rest of
the specification and instead need to be forced explicitly. This makes
it much easier to write streaming computations and deal with large
amounts of data.

<a name="install"/>
## Installation

OCaml Forest requires OCaml 4.05.0 or greater, [OCaml Pads][opads],
and a few OPAM packages.

You can get all the prerequisites by following these steps:

1. [Install OPAM][opamInstall]
2. Run the following commands in your shell:

   > opam update

   > opam upgrade

   > opam switch 4.05.0

   > eval $(opam config env)

   > opam install ocamlfind oasis utop menhir core

3. [Install OCaml Pads][opadsInstall]
4. Clone this repository (not into the OCaml Pads clone)
5. In the root of the clone, run:

   To build:

   > make && make install

   To rebuild:

   > make && make reinstall

   If you get errors, try

   > ocamlfind remove forest

<a name="usage"/>
## Usage

See the [wiki][wikiEx] for information on how to compile and run the
examples and how they work.

More generally, in the examples directory, there is a script for
desugaring a Forest source file into vanilla OCaml used by running:

> desugar.sh [file.ml]

There are two recommended ways to build a Forest source file using
either ocamlfind or ocamlbuild (which uses ocamlfind):

> ocamlfind ocamlc -thread -w -30 -package pads,pads.pads_ppx,forest,forest.forest_ppx -linkpkg [file.ml]

> ocamlbuild -use-ocamlfind -pkgs pads.pads_ppx,forest.forest_ppx,pads,forest
  -tags thread,'warn(-30)' -I [project folder containing fileName.ml]
  [fileName].native
  
Where X.X_ppx imports the extension interpreter for X, '-w -30' or
'warn(-30)' turns off warning 30 (which is for label collisions in
records, an inevitable consequence of Incremental Forest), and the
thread tag is required due to some other libraries.
  
See the [wiki][wikiGrammar] for the iForest grammar.

[forestproj]: http://forestproj.org/ "Forest Project"
[opads]: https://github.com/padsproj/opads "OCaml Pads Repository"
[opadsInstall]: https://github.com/padsproj/opads#install "Installing OCaml Pads"
[iForest]:
http://www.cs.cornell.edu/~dilorenzo/docs/incremental-forest.pdf
"Incremental Forest Paper"
[opamInstall]: https://opam.ocaml.org/doc/Install.html
"How to install OPAM"
[wikiEx]:
https://github.com/padsproj/oforest/wiki/Getting-Started-with-Incremental-Forest#examples
"Incremental Forest Examples"
[wikiGrammar]:
https://github.com/padsproj/oforest/wiki/Getting-Started-with-Incremental-Forest#grammar
"Incremental Forest Grammar"