#!/bin/ksh

GNUDATE=date

yyyymmddhh1=$1
yyyymmddhh2=$2
yyyymmdd_hh1=`echo $yyyymmddhh1 | sed 's/\([0-9][0-9]\)$/ \1/'`
yyyymmdd_hh2=`echo $yyyymmddhh2 | sed 's/\([0-9][0-9]\)$/ \1/'`
secs1=`${GNUDATE} +"%s" -d "${yyyymmdd_hh1}"`
secs2=`${GNUDATE} +"%s" -d "${yyyymmdd_hh2}"`
(( hour=(secs1-secs2) / 3600 ))
if [ ${hour} -lt 0 ]; then
  (( hour=hour*-1 ))
  printf "-%02d\n" ${hour}
else
  printf "%02d\n" ${hour}
fi
