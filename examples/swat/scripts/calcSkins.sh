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

cFile=swatTimeSkinsSheet.csv
dir=swatTimeSkinsResults/
num=100
out=output

cd $sdir;

./scripts/collect.sh $dir "" $num $out;

first=0;
second=0;
third=0;
fourth=0;
fifth=0;
check=0;

for arg in $(cat $dir$out);
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
    else if [ $check -eq 2 ]
    then 
        third=$(echo "scale=10; $third + $narg * $mult" | bc);
        check=3;
    else if [ $check -eq 3 ]
    then 
        fourth=$(echo "scale=10; $fourth + $narg * $mult" | bc);
        check=4;
    else 
        fifth=$(echo "scale=10; $fifth + $narg * $mult" | bc);
        check=0;
    fi;
    fi;
    fi;
    fi;
done

avg1=$(echo "scale=10 ; $first / $num" | bc)
avg2=$(echo "scale=10 ; $second / $num" | bc)
avg3=$(echo "scale=10 ; $third / $num" | bc)
avg4=$(echo "scale=10 ; $fourth / $num" | bc)
avg5=$(echo "scale=10 ; $fifth / $num" | bc)
stddev1=0
stddev2=0
stddev3=0
stddev4=0
stddev5=0
check=0

for arg in $(cat $dir$out);
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
    else if [ $check -eq 2 ]
    then 
        stddev3=$(echo "scale=10; $stddev3 + ($narg * $mult - $avg3)^2" | bc);
        check=3;
    else if [ $check -eq 3 ]
    then 
        stddev4=$(echo "scale=10; $stddev4 + ($narg * $mult - $avg4)^2" | bc);
        check=4;
    else 
        stddev5=$(echo "scale=10; $stddev5 + ($narg * $mult - $avg5)^2" | bc);
        check=0;
    fi;
    fi;
    fi;
    fi;
done

stddev1=$(echo "scale=10; sqrt($stddev1/$num)" | bc)
stddev2=$(echo "scale=10; sqrt($stddev2/$num)" | bc)
stddev3=$(echo "scale=10; sqrt($stddev3/$num)" | bc)
stddev4=$(echo "scale=10; sqrt($stddev4/$num)" | bc)
stddev5=$(echo "scale=10; sqrt($stddev5/$num)" | bc)


high1=$(echo "scale=10;$avg1+$stddev1" | bc)
high2=$(echo "scale=10;$avg2+$stddev2" | bc)
high3=$(echo "scale=10;$avg3+$stddev3" | bc)
high4=$(echo "scale=10;$avg4+$stddev4" | bc)
high5=$(echo "scale=10;$avg5+$stddev5" | bc)

low1=$(echo "scale=10;$avg1-$stddev1" | bc)
low2=$(echo "scale=10;$avg2-$stddev2" | bc)
low3=$(echo "scale=10;$avg3-$stddev3" | bc)
low4=$(echo "scale=10;$avg4-$stddev4" | bc)
low5=$(echo "scale=10;$avg5-$stddev5" | bc)


echo "Field,FullDelay,Both,Input,Output,Neither" > $cFile
echo "Total,$first,$second,$third,$fourth,$fifth" >> $cFile
echo "Avg,$avg1,$avg2,$avg3,$avg4,$avg5" >> $cFile
echo "High,$high1,$high2,$high3,$high4,$high5" >> $cFile
echo "Low,$low1,$low2,$low3,$low4,$low5" >> $cFile
echo "Stddev,$stddev1,$stddev2,$stddev3,$stddev4,$stddev5" >> $cFile
