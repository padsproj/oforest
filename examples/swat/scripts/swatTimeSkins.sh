#!/bin/bash

cd $(dirname $0)/..

name=swatTimeSkins
dir=${name}Results

make $name.out
mkdir -p $dir

for i in {1..100};
do ./$name.out > $dir/$i
done
