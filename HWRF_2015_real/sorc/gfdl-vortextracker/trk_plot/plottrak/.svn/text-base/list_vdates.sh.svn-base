#!/bin/ksh

# This script was written by Tim Marchok and is part of the
# GrADS ATCF track & intensity plotting script.  If there are
# any problems, contact timothy.marchok@noaa.gov
#

if [ $# -lt 1 ]; then
  echo " "
  echo "!!! ERROR: You need to enter at least 1 argument"
  echo "!!!  "
  echo "!!! USAGE: `basename $0` atcfunix_file_name"
  echo " "
  exit 8
fi

# This script is called by newtrk.gs when a new storm is
# selected from the menu.  It picks out the CARQ (verification)
# records and sorts them (unique) for each ymdh.  It also 
# pulls out the storm name, for use in the plotting of the
# title.

rundir=/misc/whome/tmarchok/plottrak

atcffile=$1

grep "CARQ,   0" $atcffile                   |\
     awk 'substr($0,65,1) ~ /3/ {print $0}'  |\
     sort -k3 -u                             |\
     awk '{
       printf ("%10s  %10s\n",
       substr($0,9,10),
       substr($0,150,10))
       }'          
