#!/bin/ksh

# This script was written by Tim Marchok and is part of the
# GrADS ATCF track & intensity plotting script.  If there are
# any problems, contact timothy.marchok@noaa.gov
#

if [ $# -lt 2 ]; then
  echo " "
  echo " !!! ERROR: You must enter at least 2 arguments:"
  echo " !!!"
  echo " !!! USAGE: `basename $0` basin yyyy"
  echo " "
  echo "     basin  = al, ep, wp, si"
  echo "     yyyy   = year"
  echo " "
  exit 8
fi

basin=$1
yyyy=$2

rundir=/misc/whome/tmarchok/plottrak

for stormfile in `ls -1 a${basin}*${yyyy}.dat`
do
  
  stormname=` grep "CARQ,   0" ${stormfile} | tail -1 | cut -c150-159`
  stormname=` echo $stormname`

  sn_first=`     echo ${stormname} | cut -c1-1`
  sn_remainder=` echo ${stormname} | cut -c2- | tr '[A-Z]' '[a-z]'`

  stormname=${sn_first}${sn_remainder}

  echo "${stormfile} ${stormname}"

done
