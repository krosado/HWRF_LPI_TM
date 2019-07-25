#!/bin/bash
#
# NOTE: this script is to plot the correlation of the first 6-h change of
#       hurricane Pmin and Vmax, given an adeck file and bdeck file, and 
#       the forecast hour that you want to compute tendency
#
# HOW TO RUN: TO run this script, you only need to edit
#       1. pw_corelation.sh: (one time) change the path to adeck/bdeck 
#       2. pw_namelist.txt: select the adeck files (storms) you want to
#          run. 
#        After that, run:
#       ./pw_corelation.sh model HH
#       You should see a text file: pw_out_$model_$HH.txt after the script
#       finished. The file lists the 6-h change of vmax and pmin for all
#       forecast cycles of all storms listed on the namelist.txt  
#
# HIST: Jan, 24 2012: created by CK  
#
# AUTHOR: Chanh Kieu (chanh.kieu@noaa.gov)   
#
#=======================================================================
home_path="/mnt/lfs1/projects/hwrfv3/Chanh.Kieu/programs"
adeck_path="$home_path/Statistics/baseline"
bdeck_path="$home_path/Statistics/baseline"
count_jan=0
count_feb=0
count_mar=0
count_apr=0
count_may=0
count_jun=0
count_jul=0
count_aug=0
count_sep=0
count_oct=0
count_nov=0
count_dec=0
rm -rf fort.*
for bfile in $( ls ${bdeck_path}/ba*2011.dat )
do
 echo "working with bdeck file $bfile"
 while read iline
 do 
  month=`echo ${iline} | cut -c13-14`
  echo "month = $month"
  if [ "$month" == "01" ]; then
   count_jan=$(($count_jan+1))
   echo "$iline" >> fort.01
  elif [ "$month" == "02" ]; then
   count_feb=$(($count_feb+1))
   echo "$iline" >> fort.02
  elif [ "$month" == "03" ]; then
   count_mar=$(($count_mar+1))
   echo "$iline" >> fort.03
  elif [ "$month" == "04" ]; then
   count_apr=$(($count_apr+1))
   echo "$iline" >> fort.04
  elif [ "$month" == "05" ]; then
   count_may=$(($count_may+1))
   echo "$iline" >> fort.05
  elif [ "$month" == "06" ]; then
   count_jun=$(($count_jun+1))
   echo "$iline" >> fort.06
  elif [ "$month" == "07" ]; then
   count_jul=$(($count_jul+1))
   echo "$iline" >> fort.07
  elif [ "$month" == "08" ]; then
   count_aug=$(($count_aug+1))
   echo "$iline" >> fort.08
  elif [ "$month" == "09" ]; then
   count_sep=$(($count_sep+1))
   echo "$iline" >> fort.09
  elif [ "$month" == "10" ]; then
   count_oct=$(($count_oct+1))
   echo "$iline" >> fort.10
  elif [ "$month" == "11" ]; then
   count_nov=$(($count_nov+1))
   echo "$iline" >> fort.11
  elif [ "$month" == "12" ]; then
   count_dec=$(($count_dec+1))
   echo "$iline" >> fort.12
  fi
 done < $bfile
done
echo "SUMMARY FOR 2011 STRATIFICATION"
echo "count Jan = $count_jan"
echo "count Feb = $count_feb"
echo "count Mar = $count_mar"
echo "count Apr = $count_apr"
echo "count May = $count_may"
echo "count Jun = $count_jun"
echo "count Jul = $count_jul"
echo "count Aug = $count_aug"
echo "count Sep = $count_sep"
echo "count Oct = $count_oct"
echo "count Nov = $count_nov"
echo "count Dec = $count_dec"
