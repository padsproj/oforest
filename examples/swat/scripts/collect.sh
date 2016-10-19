#!/bin/bash


#$0 = script
#$1 = dirname
#$2 = basename
#$3 = num to collect
#$4 = output name

if (( $# < 4 ))
then echo "Usage: ./collect.sh [dirname] [basename] [numFiles] [output]";
     exit 1;
fi

dir=$(pwd)/$1;
base=$2;
num=$3;
out=$dir$4;

force=1

if [ $force -eq 1 ];
then 
  rm -f $out;

  for i in $(seq 1 $num);
  do cat $dir$base$i >> $out; echo "" >> $out;
  done
else 
echo "This will collect $num files named $base{1-$num} in directory"
echo "$dir into file"
echo "$out"
echo "(which will be removed). Continue (Y/N)?"

read response

if [ "$response" = "Y" -o "$response" = "y" ]
then 
  echo "Calculating"
  rm -f $out;

  for i in $(seq 1 $num);
  do cat $dir$base$i >> $out; echo "" >> $out;
  done
else echo "Exiting"
     exit 1;
fi;
fi
