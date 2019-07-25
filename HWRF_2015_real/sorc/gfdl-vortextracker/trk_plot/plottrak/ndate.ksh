#!/bin/ksh

GNUDATE=date

offset=$1
yyyymmddhh=$2
yyyymmdd_hh=`echo $yyyymmddhh | sed 's/\([0-9][0-9]\)$/ \1/'`
if [ ${offset} -lt 0 ]; then
  (( offset=offset*-1 ))
  offset_str="ago"
else
  offset_str=""
fi
datelen=`echo -n "${yyyymmddhh}" | wc -c`
if [ ${datelen} -eq 10 ]; then
  ${GNUDATE} -u +"%Y%m%d%H" -d "${yyyymmdd_hh} ${offset} hours ${offset_str}"
elif [ ${datelen} -eq 8 ]; then
  ${GNUDATE} -u +"%y%m%d%H" -d "${yyyymmdd_hh} ${offset} hours ${offset_str}"
else
  echo "ndate: Invalid date argument, datelen=${datelen}"
  return -1
fi


