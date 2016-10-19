#!/bin/bash

scriptDir=$(dirname $0);

cd $scriptDir/..

name=swatTimeParse
rdir=${name}Results;
sdir=${name}Store;
numLines=42744

make $name.out
mkdir -p $rdir;
mkdir -p $sdir;

outputRch=$(pwd)/../../swatData/TxtInOut_currentnew/output.rch

for num in {1..100};
do 
l=$((numLines/100*num))
head $outputRch -n $l > $sdir/crch.$num
done

for i in {1..100};
do for j in {1..100};
do ./$name.out $sdir/crch.$i > $rdir/$i.$j
done
done
