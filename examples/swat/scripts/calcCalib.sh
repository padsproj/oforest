#!/bin/bash

# $1 = Skinned(1) or unskinned (0)?

if (( $# != 1 ))
then echo "Usage: ./calcCalib.sh [skinned]";
     echo "Where skinned is 1 if skinned and 0 o.w."
     exit 1;
fi
if [ $1 != 0 ] && [ $1 != 1 ];
then echo "First parameter to runCalib.sh must be 1 or 0"
     exit 1;
fi

get_mult () {
  if (( $# < 1 ));
  then echo "Get_mult takes one argument"
      exit 1
  fi
  arg=$1;

  if [[ "$arg" == *"ms" ]];
  then mult=0.001
  else if [[ "$arg" == *"us" ]];
  then mult=0.000001
  else if [[ "$arg" == *"s" ]];
  then mult=1
  else if [[ "$arg" == *"m" ]];
  then mult=60
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

cd $(dirname $0)/..

if [ $1 == 1 ];
then z=skinned
else z=unskinned
fi

fnameT=timeResultsCalib$z.csv
fnameN=nseResults$z.csv

echo ",Total,Non-SWAT,SWAT" > $fnameT
echo -n "," > $fnameN
#Setup
for i in {0..5};
do for j in {0..5};
do   echo -n "($i;$j)," >> $fnameN
done
done

for i in {0..5};
do for j in {0..5};
do echo "" >> $fnameN
   echo -n "($i;$j)," >> $fnameN
for k in {0..5};
do  echo -n "($i;$j:$k)," >> $fnameT
  time=calibResults/time.$z.$i.$j.$k;
  for f in {1..3};
  do 
      get_mult $(cat $time | cut -d ' ' --fields=$f);
      get_newArg $(cat $time | cut -d ' ' --fields=$f);
      var=$(echo "scale=10; $narg * $mult" | bc);
      echo -n $var >> $fnameT;
      echo -n "," >> $fnameT;
  done
  echo "" >> $fnameT

for l in {0..5};
do 
  nse=calibResults/nse.$z.$i.$j.$k.$l;
  cat $nse >> $fnameN
  echo -n "," >> $fnameN
done
done
done
done
