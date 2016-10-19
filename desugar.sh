#OCAMLFIND="ocamlfind query -predicates syntax,preprocessor -r"
#INCLUDE=`$OCAMLFIND -i-format forest.syntax`
#ARCHIVES=`$OCAMLFIND -a-format forest.syntax`
#camlp4o -printer o $INCLUDE str.cma $ARCHIVES $1
#ocamlfind ocamlc -dsource -package forest $1
ppxdir=$(dirname $0)
ocamlfind ppx_tools/rewriter $ppxdir/ppx_forest.native $1