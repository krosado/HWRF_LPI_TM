#!/bin/ksh
#
# NOTE: This script is a dirver for making the HTML/JAVA files for the HWRF webpage
#       that is based on Vijay's web.sh core. Use for ATLANTIC or PACIFIC storms. 
#
# HIST: - 19 Jun 2012: Written by Chanh Kieu
#
# Usage:   sh web_driver.sh NAME_old  STORMSTORMID  STORMID STARTDATE   NO_OF_CASES
# Example: sh web_driver.sh NEW       INVEST96L       96L   2007070312      20 
# Example: sh web_driver.sh INVEST96L IRENE09L        09L   2007081318      20 
# Example: sh web_driver.sh ADD       IRENE09L        09L   2007081318      20
# Example: sh web_driver.sh CHOP      DEAN04L         04L   2007081418      20 
# Example: sh web_driver.sh INSERT    DEAN04L         04L   2007081418  "No run this cycle"
#--------------------------------------------------
# get the input variables 
if [ $# -eq 5 ]; then 
   smode=$1
   storm=$2
   id=$3
   start_date=$4
   cases=$5
else
   echo "USAGE:   sh web_driver.sh NAME_old  STORM     STORMID STARTDATE  NO_OF_CASES/MESSAGE    "
   echo "EXAMPLE: sh web_driver.sh NEW       INVEST96L    96L  2007070312       20               " 
   echo "EXAMPLE: sh web_driver.sh INVEST96L FOUR04L      04L  2007081318       20               "
   echo "EXAMPLE: sh web_driver.sh ADD       DEAN04L      04L  2007081418       20               "  
   echo "EXAMPLE: sh web_driver.sh CHOP      DEAN04L      04L  2007081418       20               "
   echo 'EXAMPLE: sh web_driver.sh INSERT    DEAN04L      04L  2007081418   "No run this cycle"  '
   echo "NEED 5 ARGUMENTS"
   echo "SCRIPT WILL EXIT" 
   exit 1
fi 
echo 'smode      = ' $smode 
echo 'storm      = ' $storm
echo 'id         = ' $id
echo 'start_date = ' $start_date
echo 'cases      = ' $cases
message="$cases"
lstorm=`echo ${storm} | tr '[A-Z]' '[a-z]'`
lsmode=`echo ${smode} | tr '[A-Z]' '[a-z]'`
echo 'lower case storm = ' $lstorm
echo 'lower case smode = ' $lsmode
set -x
#
# switch between different mode
#
if [ "$smode" == "NEW" ]; then
 echo "Creating a new index.html"
 mv -f ./index.html ./index.html.bk
 sh web_core.sh $storm $id $start_date $cases
elif [ "$smode" == "INSERT" ]; then
 echo "Interting a message $message into the index.html at $start_date"
 cp -f ./index.html ./index.html.bk
 sline=`cat -n index.html | grep $start_date | head -n 1 | awk '{print $1+2}'`
 eline=`cat -n index.html | grep $start_date | tail -n 1 | awk '{print $1}'`
 echo "Starting line to change is: $sline"
 echo "Ending line to change is: $eline"
 sed -e ${sline},${eline}d ./index.html.bk > ./tem2.html
 echo $sline i  '\<'th id=storm_inv_goes_head\" colspan=\"22'">'$message'<'/th'>' > tem1.txt
 sed -f  ./tem1.txt ./tem2.html > ./index.html 
 rm tem1.txt ./tem2.html
elif [ "$smode" == "ADD" ]; then
 echo "Appending the index file for $storm starting from $start_date"
 cp -f ./index.html ./index.html.bk
#
# check to see if the cycle is ready in the index file first. IF so, skip 
#
 check_cycle=`grep $start_date index.html | head -n 1`
 if [ "$check_cycle" != "" ]; then 
  echo "cycle is existing...skip"
 else
  sh web_core.sh $storm $id $start_date $cases
  sline=`cat -n index.html | grep -w Diagnostics | tail -n 1 | awk '{print $1+2}'`
  eline=`cat -n index.html | grep $start_date | tail -n 1 | awk '{print $1+3}'`
  echo "Starting line to change is: $sline"
  echo "Ending line to change is: $eline"
  sed -n ${sline},${eline}p ./index.html > ./log1.txt
  line1=`cat -n index.html.bk | grep -w Diagnostics | tail -n 1 | awk '{print $1+2}'` 
  line2=`cat -n index.html.bk | grep -w Diagnostics | tail -n 1 | awk '{print $1+3}'`
  fline=`cat -n index.html.bk | tail -n 1 | awk '{print $1}'`
  sed -n 1,${line1}p ./index.html.bk > ./log2.txt 
  sed -n ${line2},${fline}p ./index.html.bk > ./log3.txt
  cat log2.txt > index.html
  cat log1.txt >> index.html
  cat log3.txt >> index.html
  rm log*
 fi
elif [ "$smode" == "CHOP" ]; then
 echo "Chopping the index file for $storm starting from $start_date"
 cp -f ./index.html ./index.html.bk
 sline=`cat -n index.html | grep $start_date | head -n 1 | awk '{print $1}'`
 eline=`cat -n index.html | grep $storm | tail -n 1 | awk '{print $1-1}'`
 echo "Starting line to change is: $sline"
 echo "Ending line to change is: $eline"
 sed -e ${sline},${eline}d ./index.html.bk > ./index.html 
else
 echo "Renaming index file from $smode to $storm starting from $start_date"
 cp -f ./index.html ./index.html.bk
 sline=`cat -n index.html | grep -w Diagnostics | tail -n 1 | awk '{print $1+2}'`
 eline=`cat -n index.html | grep -w $smode | grep $start_date | head -n 1 | awk '{print $1}'`
 hline=`cat -n index.html | grep -w storm_inv_content | awk '{print $1}'`
 echo "Head line for the index file is at: $hline"
 echo "Starting line to change is: $sline"
 echo "Ending line to change is: $eline"
 if [ "$eline" != "" ] && [ "$sline" != "" ]; then
#
# change the head line of index.html
#
  echo "1,${hline}s/${smode}/${storm}/g"            >  ./changeit
  echo "${sline},${eline}s/${smode}/${storm}/g"     >> ./changeit
  echo "${sline},${eline}s/${lsmode}/${lstorm}/g"   >> ./changeit
  sed -f ./changeit index.html.bk > index.html
  rm -f changeit
 else
#
# the name $smode and $star_date does not exist in the index.html. This 
# means the new pair ($storm,$star_date) must be added as a new cycle 
#
  sh web_core.sh $storm $id $start_date $cases
  #sline=`cat -n index.html | grep -w Diagnostics | tail -n 1 | awk '{print $1+2}'`
  sline=1
  eline=`cat -n index.html | grep $start_date | tail -n 1 | awk '{print $1+3}'`
  echo "Starting line to change is: $sline"
  echo "Ending line to change is: $eline"
  sed -n ${sline},${eline}p ./index.html > ./log1.txt
  line1=`cat -n index.html.bk | grep -w Diagnostics | tail -n 1 | awk '{print $1+2}'`
  line2=`cat -n index.html.bk | grep -w Diagnostics | tail -n 1 | awk '{print $1+3}'`
  fline=`cat -n index.html.bk | tail -n 1 | awk '{print $1}'`
  sed -n 1,${line1}p ./index.html.bk > ./log2.txt
  sed -n ${line2},${fline}p ./index.html.bk > ./log3.txt
  #cat log2.txt > index.html
  cat log1.txt > index.html
  cat log3.txt >> index.html
  rm -f log* 
 fi
fi  
