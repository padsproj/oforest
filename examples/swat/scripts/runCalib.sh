#!/bin/bash

# $1 = Skinned(1) or unskinned (0)?

if (( $# != 1 ))
then echo "Usage: ./runCalib.sh [skinned]";
     echo "Where skinned is 1 if skinned and 0 o.w."
     exit 1;
fi
if [ $1 != 0 ] && [ $1 != 1 ];
then echo "First parameter to runCalib.sh must be 1 or 0"
     exit 1;
fi

#Setup

name=swatCalibration
rdir=calibResults;
swatDir=$(dirname $0)/..

cd $swatDir;

make $name.out
mkdir -p $rdir;

skinned=$1

for i in {0..5}
do for j in {0..5}
do for k in {0..5}
do
  ./$name.out $skinned $i $j $k 
done;
done;
done
