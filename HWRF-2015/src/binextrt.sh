#!/bin/ksh

# get the input variables
if [ $# -eq 2 ]
then
   ifile=${1}
   vari=${2}

else
   echo 'Usage:   sh binextrt.sh          grib_file_name           variable' 
   echo 'Example: sh binextrt.sh bill03l.2009081900.hwrfprs_n.grbf00 TMP'
   echo 'NEED TWO ARGUMENTS'
   echo 'SCRIPT WILL EXIT'
   exit 1
fi

 ofile=${vari}.bin
 wgrib -s $ifile | egrep $vari | egrep mb | wgrib  -s $ifile -i -bin -o $ofile -nh
    echo "$ofile was generated"
