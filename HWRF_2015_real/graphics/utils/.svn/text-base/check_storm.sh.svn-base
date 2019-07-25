#!/bin/bash
cycle="0600"
tcvital="/lfs1/projects/hwrfv3/hwrf-input/syndat_tcvitals.2011"
rm -f fort.*
grep NHC ${tcvital} | awk '{print $4}' > fort.13
touch ./fort.12
touch ./fort.14
for i in $( cat ./fort.13)
do
 flag=1
 for k in $( awk '{print $3}' ./fort.12 )
 do
  if [ "$k" == "$i" ]; then
   flag=0
  fi
 done
 if [ "$flag" == "1" ]; then
  grep $i $tcvital | grep NHC | awk '{if($5=='$cycle') print $2}' > fort.10
  rm -f ./fort.11
  touch ./fort.11
  for j in $( cat fort.10 )
  do
   var=`grep $j fort.10 | head -n 1`
   check=`grep $var ./fort.11` 
   if [ "$check" == "" ]; then 
    echo $var >> ./fort.11
   fi
  done 
  ns=`cat ./fort.11 | awk '{if(tname=="")(tname=$1); if(tname!=$1)(nc+=1); if(tname!=$1)(tname=$1)} END {print nc+1}'`
  echo "checking date $i and return $ns storms" >> ./fort.12
  if [ $ns -gt 4 ]; then
   echo "checking date $i and return $ns storms" >> ./fort.14
   cat ./fort.11 >> fort.14
  fi
 else
  echo "$i has been checked"
 fi
done
mv ./fort.14 ./cycle_${cycle}.txt
