#!/bin/bash

# $0 = Scriptname
# $1 = filename
# $2 = mode

if (( $# < 1 )) || (( $# > 2));
then echo "Usage: ./compileEx.sh filename [mode]";
    echo "Modes: 1 - compiles normally (equal to having no mode)";
    echo "       2 - desugars into examples/tmp.ml then compiles that";
    echo "       3 - just compiles examples/tmp.ml and ignores argument 1";
    echo "       4 - just desugars argument 1";
  exit 1;
fi

fullname=$1;
fullnamenoext=${fullname%.*}
namenoext=$(basename $fullname | cut -d '.' -f 1)

dir=$(dirname $0);
tmp=$dir/examples/tmp.ml;
tmpfullnamenoext=${tmp%.*};

if (( $# == 2 )) && [ "$2" -eq "2" ];
then
    $dir/desugar.sh $fullname > $tmp
    ocamlfind ocamlopt -o $namenoext.out -package core,pads,pads.ppx,forest,forest.ppx,re,unix,str,re.glob -linkpkg $tmp -thread -g
    rm $tmpfullnamenoext.cmx $tmpfullnamenoext.o $tmpfullnamenoext.cmi
    rm -f $fullnamenoext.cmx $fullnamenoext.o $fullnamenoext.cmi
else if (( $# == 2 )) && [ "$2" -eq "3" ];
then
    ocamlfind ocamlopt -o $namenoext.out -package forest.ppx,pads,pads.ppx,re,core,unix,str,re.glob,forest -linkpkg $tmp -thread -g
    rm $tmpfullnamenoext.cmx $tmpfullnamenoext.o $tmpfullnamenoext.cmi
else if (( $# == 2 )) && [ "$2" -eq "4" ];
then
    $dir/desugar.sh $fullname
else
    ocamlfind ocamlopt -o $namenoext.out -package core,pads,pads.ppx,forest,forest.ppx,re,unix,str,re.glob -linkpkg $fullname -thread -w -30
    rm $fullnamenoext.cmx $fullnamenoext.o $fullnamenoext.cmi
fi;
fi;
fi

