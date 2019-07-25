#!/bin/bash
set -x
ATCFID=H130
for ifile in $( ls *.txt )
do
 echo "rename file $ifile"
 yyyymmddhh=`echo $ifile | cut -d '.' -f 2`
 yyyy=`echo $yyyymmddhh | cut -c1-4`
 storm=`echo $ifile | cut -d '.' -f 1`
 length=${#storm}
 y1=$(($length-2))
 y2=$length
 stormid=`echo $storm | cut  -c${y1}-${y2}`
 stormnum=`echo $stormid | cut  -c1-2`
 basin=`echo $stormid | grep -i l`
 if [ "$basin" != ""  ]; then
  shipname="sal${stormnum}${yyyy}_${ATCFID}_${yyyymmddhh}_diag.dat"
  mv $ifile $shipname
 else
  shipname="sep${stormnum}${yyyy}_${ATCFID}_${yyyymmddhh}_diag.dat"
  mv $ifile $shipname
 fi
done
