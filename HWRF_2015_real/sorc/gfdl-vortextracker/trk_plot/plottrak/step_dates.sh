#!/bin/ksh

# This script was written by Tim Marchok and is part of the
# GrADS ATCF track & intensity plotting script.  If there are
# any problems, contact timothy.marchok@noaa.gov
#

if [ $# -ne 4 ]; then
  echo " "
  echo " !!! ERROR: You must enter 4 arguments:"
  echo " !!!"
  echo " !!! USAGE: `basename $0` storm_list_num startymdh timestep userid"
  echo " "
  echo "     storm_list_num = integer number of the vlist file in your tracks"
  echo "                      directory.  This is *not* the ATCF number."
  echo " "
  echo "     startymdh = Our reference yyyymmddh from which we are starting"
  echo "                 our date calculation. yyyymmddhh format."
  echo " "
  echo "     timestep = number of hours either forwards or backwards in time"
  echo "                that we wish to go in order to get a new date"
  echo "                (Must be either -24, -12, -6, 6, 12, 24)"
  echo " "
  echo "     userid = user logon (tpm, wd20tm, etc....)"
  echo " "
  exit 8
fi

# This script calculates a new dtg either forward or backwards 
# from an input date and dumps out the value of that new date.
# In the track-plotting script, there is a dropdown menu for 
# the list of storms.  When a user selects a storm, the item 
# in that dropdown list is given a number, sequentially from
# the top of the list.

# set -x

stlistnum=$1
symdh=$2
timestep=$3
userid=$4

rundir=/misc/whome/tmarchok/plottrak
netdir=/misc/whome/${userid}/plottrak/tracks

ndate=/misc/whome/tmarchok/plottrak/ndate.ksh


datefile=${netdir}/vlist.${stlistnum}

first_vdate=` cat ${datefile} | head -1 | cut -c1-10`
last_vdate=`  cat ${datefile} | tail -1 | cut -c1-10`

eymdh=` $ndate ${timestep} ${symdh}`

if [ ${eymdh} -gt ${last_vdate} ]; then
  echo "995  995"
  exit
fi

if [ ${eymdh} -lt ${first_vdate} ]; then
  echo "900  900"
  exit
fi

datecheck=` grep ${eymdh} ${datefile} | wc -l`
if [ ${datecheck} -gt 0 ]; then
  echo "${eymdh}  ${eymdh}"
else
  echo "500  ${eymdh}"
fi
