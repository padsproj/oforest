#!/bin/bash

./clean.sh
make;
if [ $? -eq 0 ];
then 
make reinstall;
if [ $? -ne 0 ];
then ocamlfind remove forest;
make install
fi
clear;
#./desugar.sh examples/simple/simple.ml;
else exit 1;
fi;
exit 0;
