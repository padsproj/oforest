#!/bin/bash

get_mult () {
  if (( $# < 1 ));
  then echo "Get_mult takes one argument"
      exit 1
  fi
  arg=$1;

  if [[ "$arg" == *"ms" ]];
  then mult=1
  else if [[ "$arg" == *"us" ]];
  then mult=0.001
  else if [[ "$arg" == *"s" ]];
  then mult=1000
  else if [[ "$arg" == *"m" ]];
  then mult=60000
  else echo "That took longer than expected."
      echo $arg;
      exit 1;
  fi;
  fi;
  fi;
  fi;
  return 0;
}

get_newArg () {
  if (( $# < 1 ));
  then echo "Get_newArg takes one argument"
      exit 1
  fi
  arg=$1;

  if [[ "$arg" == *"ms" ]];
  then narg=$(echo $arg | cut -d 'm' --fields=1)
  else if [[ "$arg" == *"us" ]];
  then narg=$(echo $arg | cut -d 'u' --fields=1)
  else if [[ "$arg" == *"s" ]];
  then narg=$(echo $arg | cut -d 's' --fields=1)
  else if [[ "$arg" == *"m" ]];
  then narg=$(echo $arg | cut -d 'm' --fields=1)
  else echo "That took longer than expected."
      echo $arg;
      exit 1;
  fi;
  fi;
  fi;
  fi;
  return 0;

}

sdir=$(dirname $0)/..

cFile=swatTimeParseSheet.csv
dir=swatTimeParseResults/
num=100
out=output

cd $sdir;

for i in {1..100};
do ./scripts/collect.sh $dir $i. $num $out$i;
done;

echo "Full,KVPair,File,avg,stddev,Full,KVPair,File,,Full,KVPair,File,High,Low,Full,KVPair,File" > $cFile

for i in {1..100};
do

first=0;
second=0;
third=0;
check=0;

for arg in $(cat $dir$out$i);
do 
    get_mult $arg;
    get_newArg $arg;
    if [ $check -eq 0 ]
    then
        first=$(echo "scale=10; $first + $narg * $mult" | bc);
        check=1;
    else if [ $check -eq 1 ]
    then
        second=$(echo "scale=10; $second + $narg * $mult" | bc);
        check=2;
    else 
        third=$(echo "scale=10; $third + $narg * $mult" | bc);
        check=0;
    fi;
    fi;
done

avg1=$(echo "scale=10 ; $first / $num" | bc)
avg2=$(echo "scale=10 ; $second / $num" | bc)
avg3=$(echo "scale=10 ; $third / $num" | bc)
stddev1=0
stddev2=0
stddev3=0
check=0

for arg in $(cat $dir$out$i);
do 
    get_mult $arg;
    get_newArg $arg;
    if [ $check -eq 0 ]
    then
        stddev1=$(echo "scale=10; $stddev1 + ($narg * $mult - $avg1)^2" | bc);
        check=1;
    else if [ $check -eq 1 ]
    then
        stddev2=$(echo "scale=10; $stddev2 + ($narg * $mult - $avg2)^2" | bc);
        check=2;
    else 
        stddev3=$(echo "scale=10; $stddev3 + ($narg * $mult - $avg3)^2" | bc);
        check=0;
    fi;
    fi;
done

stddev1=$(echo "scale=10; sqrt($stddev1/$num)" | bc)
stddev2=$(echo "scale=10; sqrt($stddev2/$num)" | bc)
stddev3=$(echo "scale=10; sqrt($stddev3/$num)" | bc)

high1=$(echo "scale=10;$avg1+$stddev1" | bc)
high2=$(echo "scale=10;$avg2+$stddev2" | bc)
high3=$(echo "scale=10;$avg3+$stddev3" | bc)

low1=$(echo "scale=10;$avg1-$stddev1" | bc)
low2=$(echo "scale=10;$avg2-$stddev2" | bc)
low3=$(echo "scale=10;$avg3-$stddev3" | bc)

echo "$avg1,$avg2,$avg3,,,$stddev1,$stddev2,$stddev3,,$high1,$high2,$high3,,,$low1,$low2,$low3" >> $cFile

done

