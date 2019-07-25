#!/bin/ksh
#
# NOTE:  This script plots the 10 m wind swath and accumulated rainfall
#
# Usage:   sh plot_swath.sh STORM   stormid YYYYMMDDHH MC_MODEL 
# Example: sh plot_swath.sh TWELVE    12l   2007092506  H211
# 
# HIST: - 2009: Taken from hwrf_graphics_lib.bash script by Sam Trahan 
#       - 2011: Edited for 2011 by Janna Durante
#       - 2012: Modified by Chanh Kieu for real-time graphics
#############################################################
set -x 
#-------------------------------------------------------
# get the input variables 
if [ $# -eq 6 ] 
then
   STORM=${1}
   stormid=${2}
   YYYYMMDDHH=${3}
   MC_MODEL=$4
   sorc=$5
   tmp_dir=$6
   echo 'STORM      = ' ${STORM}
   echo 'stormid    = ' ${stormid} 
   echo 'YYYYMMDDHH = ' ${YYYYMMDDHH} 
   echo 'MC_MODEL   = ' ${MC_MODEL}
   echo 'sorc       = ' $sorc
else
   echo 'Usage:   sh plot_swath.sh STORM   stormid YYYYMMDDHH MC_MODEL' 
   echo 'NEED FOUR ARGUMENTS'
   echo 'SCRIPT WILL EXIT'
   exit 1 
fi 
#------------------------------------------------------
# create useful uppercase and lowercase names used in pathways
# This script copies swath data files from the ./hwrf/com/
# directory where lower case names are used. 
storm=`  echo ${STORM}   | tr '[A-Z]' '[a-z]'`
STORMID=`echo ${stormid} | tr '[a-z]' '[A-Z]'`
echo 'storm   = ' ${storm}
echo 'STORMID = ' ${STORMID} 
STORMNAME=${STORM}${STORMID}
stormname=${storm}${stormid}
GRDFILE=${STORMNAME}.${YYYYMMDDHH}.swath.dat 
grdfile=${stormname}.${YYYYMMDDHH}.swath.dat 
TEXTFILE=${STORMNAME}.${YYYYMMDDHH}.wind10m.ascii
textfile=${stormname}.${YYYYMMDDHH}.wind10m.ascii
SHORTFILE=${STORMNAME}.${YYYYMMDDHH}.stats.short
shortfile=${stormname}.${YYYYMMDDHH}.stats.short  
swathctl=${stormname}.${YYYYMMDDHH}.swath.ctl
echo 'STORMNAME = ' ${STORMNAME}  
echo 'stormname = ' ${stormname}  
echo 'GRDFILE   = ' ${GRDFILE}
echo 'grdfile   = ' ${grdfile}
echo 'TEXTFILE  = ' ${TEXTFILE}  
echo 'textfile  = ' ${textfile}  
echo 'SHORTFILE = ' ${SHORTFILE}  
echo 'shortfile = ' ${shortfile}  
letter=`echo ${stormid} | cut -c3`
echo 'letter = ' ${letter}  
# 
#------------------------------------------------------
# define the pathways
user=`whoami`
ndate="${NDATE_PATH}"  
work="${tmp_dir}/tmp/SWATHS/${STORM}${STORMID}"  
data_dir="${tmp_dir}/tmp/${YYYYMMDDHH}/${stormid}"
archive_base="${tmp_dir}/figures" 
#------------------------------------------------------
# set archive directory on DEW from basin of storm 
# note that CENTRAL Pacific will go to EASTPAC directory
if [ ${letter} = 'l' ]
then
archive_dir="${archive_base}/RT_ATLANTIC/${STORM}${STORMID}/${STORM}${STORMID}.${YYYYMMDDHH}"  
elif [ ${letter} = 'e' ]
then
archive_dir="${archive_base}/RT_EASTPAC/${STORM}${STORMID}/${STORM}${STORMID}.${YYYYMMDDHH}"  
elif [ ${letter} = 'c' ]
then
archive_dir="${archive_base}/RT_CPAC/${STORM}${STORMID}/${STORM}${STORMID}.${YYYYMMDDHH}"  
elif [ ${letter} = 'w' ] || [ ${letter} = 'b' ] || [ ${letter} = 's' ] || [ ${letter} = 'p' ]
then
archive_dir="${archive_base}/RT_WPAC/${STORM}${STORMID}/${STORM}${STORMID}.${YYYYMMDDHH}"
else
   echo "BASIN DESIGNATION LETTER letter = ${letter} NOT LOWER CASE l, e, or c"
   echo 'SCRIPT WILL EXIT'
   exit 1
fi 
#------------------------------------------------------
echo 'sorc     = ' ${sorc}
echo 'work     = ' ${work}
echo 'data_dir = ' ${data_dir}  
echo 'archive_dir = ' ${archive_dir}  
#------------------------------------------------------
# make the working directory or remove all files; and change to it
if [ -d ${work} ]
then 
   echo "${work} directory exists" 
   cd ${work}
   /bin/rm -f ${work}/* 
else
   mkdir -p ${work}
   cd ${work}
fi
pwd
#------------------------------------------------------
# date manipulation 
#------------------------------------------------------
YYYY=`echo $YYYYMMDDHH | cut -c1-4`
MM=`  echo $YYYYMMDDHH | cut -c5-6`
DD=`  echo $YYYYMMDDHH | cut -c7-8`
HH=`  echo $YYYYMMDDHH | cut -c9-10`

if [ $MM -eq '01' ];then MON=JAN;fi
if [ $MM -eq '02' ];then MON=FEB;fi
if [ $MM -eq '03' ];then MON=MAR;fi
if [ $MM -eq '04' ];then MON=APR;fi
if [ $MM -eq '05' ];then MON=MAY;fi
if [ $MM -eq '06' ];then MON=JUN;fi
if [ $MM -eq '07' ];then MON=JUL;fi
if [ $MM -eq '08' ];then MON=AUG;fi
if [ $MM -eq '09' ];then MON=SEP;fi
if [ $MM -eq '10' ];then MON=OCT;fi
if [ $MM -eq '11' ];then MON=NOV;fi
if [ $MM -eq '12' ];then MON=DEC;fi

echo 'YYYY = ' ${YYYY}
echo 'MM   = ' ${MM}
echo 'MON  = ' ${MON}
echo 'DD   = ' ${DD}
echo 'HH   = ' ${HH}  
#------------------------------------------------------
# if necessary input data files are present, copy to
# working directory; otherwise exit 
trackfile=${storm}${stormid}.${YYYYMMDDHH}.trak.hwrf.atcfunix 
/bin/cp -f ${data_dir}/${trackfile}  ${work}/.
sed "s/HWRF/${MC_MODEL}/" ${work}/${trackfile} > ${work}/test.dat 
mv ${work}/test.dat  ${archive_dir}/${trackfile}
#------------------------------------------------------
# copy wind10m.ascii file 
if [ -s ${data_dir}/${textfile} ]
then
   ln -sf ${data_dir}/${textfile}   ${work}/
   ln -sf ${data_dir}/${grdfile}    ${work}/${grdfile}
   ln -sf ${data_dir}/${grdfile}    ${work}/${GRDFILE}
   ln -sf ${data_dir}/${trackfile}  ${work}/
   ln -sf ${data_dir}/${swathctl}   ${work}/ctl_file
   ZEUS_check=`env | grep zeus`
   if [ "$ZEUS_check" != "" ]; then 
    sed -i '/UNDEF/i OPTIONS byteswapped' ${work}/ctl_file
   fi
else
   echo "FILE ${data_dir}/${textfile} OR ${grdfile} NOT PRESENT OR SIZE ZERO"
   echo 'SCRIPT WILL EXIT'
   exit 1
fi 
#------------------------------------------------------
# copy the executable to make hwrf.stats file to working directory    
#------------------------------------------------------
cp -f ${sorc}/../sorc//hwrf-utilities/exec/grp_atcf_to_stats.exe  ${work}/atcf_to_stats.exe
#------------------------------------------------------
# run executable to produce file hwrf.stats
#------------------------------------------------------
/bin/cp -f ${work}/${trackfile}  ${work}/hwrf.atcfunix
${work}/atcf_to_stats.exe 1> ${work}/hwrf.stats.out 2> ${work}/hwrf.stats.err
#------------------------------------------------------
# add a blank column after the  HOUR: in the hwrf.stats file
#------------------------------------------------------
sed "s/HOUR\:/HOUR\: /g" ${work}/hwrf.stats > ${work}/statsin
#------------------------------------------------------
# extract lon,lat,time variables from statsin file for 
# use in labeling plots  
#------------------------------------------------------
init_long=`head -1 ${work}/statsin | awk '{print $4}'`
init_lat=` head -1 ${work}/statsin | awk '{print $6}'` 
end_long=` tail -1 ${work}/statsin | awk '{print $4}'`
end_lat=`  tail -1 ${work}/statsin | awk '{print $6}'`
end_hr=`   tail -1 ${work}/statsin | cut -c7-9`

echo 'init_long = ' ${init_long}
echo 'init_lat  = ' ${init_lat}
echo 'end_long  = ' ${end_long}
echo 'end_lat   = ' ${end_lat}  
echo 'end_hr    = ' ${end_hr}  

vdate=`${ndate} ${end_hr} ${YYYYMMDDHH}`
echo 'vdate     = ' ${vdate}
#------------------------------------------------------
# obtain lon/lat of SW corner and number of grid points in X and Y 

WLON=`head -1  ${work}/${textfile} | awk '{print $1}'`
SLAT=`head -1  ${work}/${textfile} | awk '{print $3}'` 
XPTS=`head -1  ${work}/${textfile} | awk '{print $6}'` 
YPTS=`head -1  ${work}/${textfile} | awk '{print $7}'`  

echo 'WLON = ' ${WLON}
echo 'SLAT = ' ${SLAT}
echo 'XPTS = ' ${XPTS}
echo 'YPTS = ' ${YPTS}  
#------------------------------------------------------
# copy the grads template file and grads scripts to the working directory 
/bin/cp -f ${sorc}/lib-gs/para_plot_swath.gs      ${work}/  
/bin/cp -f ${sorc}/lib-gs/trakplotx.gs            ${work}/
/bin/cp -f ${sorc}/lib-gs/cbarn.gs                ${work}/
/bin/cp -f ${sorc}/lib-gs/setup.gs                ${work}/  
/bin/cp -f ${sorc}/lib-gs/rgbset.gs               ${work}/
#------------------------------------------------------
# use the stream editor to change the para_plot_swath.gs 
#------------------------------------------------------
echo "s/STORM/${STORMNAME}/g"         > ${work}/changedate
echo "s/YYYYMMDDHH/${YYYYMMDDHH}/g"  >> ${work}/changedate 
echo "s/HOUR/${end_hr}/g"            >> ${work}/changedate
echo "s/vdate/${vdate}/g"            >> ${work}/changedate 
echo "s/FirstLon/${init_long}/g"     >> ${work}/changedate
echo "s/FirstLat/${init_lat}/g"      >> ${work}/changedate  
echo "s/LastLon/${end_long}/g"       >> ${work}/changedate
echo "s/LastLat/${end_lat}/g"        >> ${work}/changedate  
echo "s/end_hr/${end_hr}/g"          >> ${work}/changedate   
echo "s/MC_MODEL/${MC_MODEL}/g"      >> ${work}/changedate
#------------------------------------------------------
sed -f ${work}/changedate ${work}/para_plot_swath.gs \
                        > ${work}/hwrf_swath.gs   
#------------------------------------------------------
# run the grads script to make swath plot  
grads -xlbc "run hwrf_swath.gs"   
#------------------------------------------------------
# check to see if swath .png files produced, and rename 
if [ -s ${work}/swath_10m.png ]
then
   /bin/mv -f ${work}/swath_10m.png    \
        ${work}/swath_10m.${STORM}${STORMID}.${YYYYMMDDHH}.png 
else
   echo 'FILE swath_10m.png NOT CREATED'
   echo 'SCRIPT WILL EXIT'
   exit 1
fi  

if [ -s ${work}/swath_rain.png ]
then
   /bin/mv -f ${work}/swath_rain.png    \
        ${work}/swath_rain.${STORM}${STORMID}.${YYYYMMDDHH}.png 
else
   echo 'FILE swath_rain.png NOT CREATED'
   echo 'SCRIPT WILL EXIT'
   exit 1
fi  
#------------------------------------------------------
# copy all .png files to archive directory to save
if [ -d ${archive_dir} ]
then
  echo "${archive_dir} exists" 
else
  mkdir -p ${archive_dir} 
fi 
mv -f ${work}/*.png  ${archive_dir}/
cp -f ${work}/${trackfile} ${archive_dir}/
cd ${archive_dir}/
rm -f swath.${STORM}${STORMID}.${YYYYMMDDHH}.png
montage -mode concatenate -tile 2x1 swath_10m.${STORM}${STORMID}.${YYYYMMDDHH}.png                      \
         swath_rain.${STORM}${STORMID}.${YYYYMMDDHH}.png swath.${STORM}${STORMID}.${YYYYMMDDHH}.png
rm -f swath_10m.${STORM}${STORMID}.${YYYYMMDDHH}.png swath_rain.${STORM}${STORMID}.${YYYYMMDDHH}.png         
rm -r ${work}
exit
