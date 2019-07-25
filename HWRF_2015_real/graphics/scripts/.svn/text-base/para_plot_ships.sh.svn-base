#!/bin/ksh --login
# NOTE: This unix shell script uses script written by CIRA to
#       produce a diagnostics text file 
#
# Usage:   sh para_plot_ships.sh STORM   stormid yyyymmddhh MC_MODEL 
# Example: sh para_plot_ships.sh KATRINA   12l   2005082612  HWRF
#
# HIST:
#      2013: modified for real-time graphics by Chanh Kieu
################################################################
set -x 
#-------------------------------------------------------
# get the input variables 
PBS=0
if [ $# -eq 6 ] || [ "$PBS" == "1" ] 
then
  if [ "$PBS" == "0" ]; then
   STORM=${1}
   stormid=${2}
   yyyymmddhh=${3}
   MC_MODEL=$4
   sorc=$5
   tmp_dir=$6
  else
   yyyymmddhh=${YYYYMMDDHH}
   sorc=${HOME_PATH}
  fi
  echo 'STORM      = ' ${STORM}
  echo 'stormid    = ' ${stormid} 
  echo 'yyyymmddhh = ' ${yyyymmddhh} 
  echo 'MC_MODEL   = ' ${MC_MODEL}
  echo 'sorc       = ' $sorc
else
   echo 'Usage: ./para_plot_ships.sh STORM stormid yyyymmddhh MC_MODEL $path_to_para' 
   echo 'SCRIPT WILL EXIT'
   exit 1 
fi 
#------------------------------------------------------
# create the lower case storm name 
storm=`echo ${STORM} | tr '[A-Z]' '[a-z]'`

# create the upper case storm number
STORMID=`echo ${stormid} | tr '[a-z]' '[A-Z]'`

# create the HOUR variable
HOUR=`echo $yyyymmddhh | cut -c9-10`

# create lower case MC_MODEL
mc_model=`echo ${MC_MODEL} | tr '[A-Z]' '[a-z]'`

# create yyyy
yyyy=`echo ${yyyymmddhh} | cut -c1-4`

# create StormNum
StormNum=`echo ${stormid} | cut -c1-2`

echo 'storm = ' ${storm}  
echo 'STORMID = ' ${STORMID} 
echo 'HOUR    = ' ${HOUR} 
#------------------------------------------------------
# define the pathways
work="${tmp_dir}/tmp/DIAGFILE/${STORM}${STORMID}/${yyyymmddhh}"
data_dir="${tmp_dir}/tmp/${yyyymmddhh}/${stormid}"
ndate="${utilexec}/ndate" 
archive_base="${tmp_dir}/figures/"
#------------------------------------------------------
# Determine basin of storm from third letter of stormid 
# (l=ATLANTIC, e=EASTPAC, c=CENTRAL PACIFIC) 
letter=`echo ${stormid} | cut -c3`
echo 'letter = ' ${letter}
#------------------------------------------------------
# set archive directory  
#------------------------------------------------------
if [ ${letter} = 'l' ]
then
archive_dir="${archive_base}/RT_ATLANTIC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"  
Basin2="al"
elif [ ${letter} = 'e' ]
then
archive_dir="${archive_base}/RT_EASTPAC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"  
Basin2="ep"
elif [ ${letter} = 'c' ]
then
archive_dir="${archive_base}/RT_CPAC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"  
Basin2="cp"
elif [ ${letter} = 'w' ] || [ ${letter} = 'b' ] || \
     [ ${letter} = 's' ] || [ ${letter} = 'p' ]
then
archive_dir="${archive_base}/RT_WPAC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"
Basin2="wp"
else
   echo "BASIN DESIGNATION LETTER letter = ${letter} NOT LOWER CASE l, e, or c"
   echo 'SCRIPT WILL EXIT'
   exit 1
fi 
#------------------------------------------------------
echo 'user     = ' ${user}
echo 'sorc     = ' ${sorc}
echo 'work     = ' ${work}  
echo 'data_dir = ' ${data_dir} 
echo 'archive_dir = ' ${archive_dir} 
#------------------------------------------------------
# make the working directory; remove all files; and cd to it
if [ -d ${work} ]
then 
   echo "${work} exists"
   cd ${work}
   /bin/rm -f ${work}/* 
else
   mkdir -p ${work} 
   cd ${work}
fi  
pwd
#------------------------------------------------------
# this for loop sequentially copies each NEST grib file
# for hours hh = 00, 06,..., 126 to the working directory.
#
 for hh in 00 06 12 18 24 30 36 42 48 54 60  \
           66 72 78 84 90 96 102 108 114 120 126
do
#------------------------------------------------------
grib1file=${storm}${stormid}.${yyyymmddhh}.hwrfprs_p.grbf${hh}
grib2file=${storm}${stormid}.${yyyymmddhh}.hwrfprs_p.grb2f${hh}
grib1nest=${storm}${stormid}.${yyyymmddhh}.hwrfprs_n.grbf${hh}
grib2nest=${storm}${stormid}.${yyyymmddhh}.hwrfprs_n.grb2f${hh}
echo 'grib2file = ' ${grib2file}
if [ -s ${data_dir}/${grib1file} ]
then
   ln -sf -f ${data_dir}/${grib1file} ${work}
   ln -sf -f ${data_dir}/${grib1nest} ${work}
   cnvgrib -g12 -p32 -nv $grib1file $grib2file
   cnvgrib -g12 -p32 -nv $grib1nest $grib2nest
else
   echo "FILE ${data_dir}/${grib2file} NOT PRESENT"
   echo 'SCRIPT WILL EXIT'
   exit 1
fi
#-------------------------------------------------------------
done
#-------------------------------------------------------------
# copy a-deck file to working directory
#-------------------------------------------------------------
   /bin/cp -f ${data_dir}/*3hour*   ${work}/.
#-------------------------------------------------------------
# copy the necessary files as a template to working directory
#-------------------------------------------------------------
   /bin/cp -f ${sorc}/scripts/para_create_ships.sh      ${work}/create_diags.sh
   /bin/cp -f ${sorc}/utils/input.plvls                 ${work}/input.plvls
   /bin/cp -f ${sorc}/utils/dataformats.inc             ${work}/
   /bin/cp -r ${sorc}/utils/include                     ${work}/
    cp -f ${sorc}/../sorc/hwrf-utilities/exec/grp_getcenter.exe    ${work}/getcenter.x
    cp -f ${sorc}/../sorc/hwrf-utilities/exec/grp_gridparse.exe    ${work}/gridparse.x
    cp -f ${sorc}/../sorc/hwrf-utilities/exec/grp_inddiag.exe      ${work}/inddiag.x
    cp -f ${sorc}/../sorc/hwrf-utilities/exec/grp_inddiagnull.exe  ${work}/inddiagnull.x
    cp -f ${sorc}/../sorc/hwrf-utilities/exec/grp_nameparse.exe    ${work}/nameparse.x
    cp -f ${sorc}/../sorc/hwrf-utilities/exec/grp_totaldiag.exe    ${work}/totaldiag.x
#--------------------------------------------------------------
#  modify three of the template files for the storm name and date 
#-----------------------------------------------------------------
gribit="${storm}${stormid}.${yyyymmddhh}.hwrfprs_p.grb2f00"
   echo "s/MODL2/${mc_model}/g"       >> ${work}/changedate
   echo "s/GRIBFILE/${gribit}/g"        >> ${work}/changedate 
#-----------------------------------------------------------------
#  apply changes to template file with stream editor
#-----------------------------------------------------------------
   echo "${tmp_dir}/tmp/DIAGFILE/${STORM}${STORMID}/${yyyymmddhh}/GRIBFILE" > ./input.list.in
   sed -f ${work}/changedate ${work}/input.list.in > ${work}/input.list 
   echo "20 "         > ${work}/input.params 
   echo "126 "       >> ${work}/input.params
   echo "6"          >> ${work}/input.params
   echo "1"          >> ${work}/input.params
   echo "$mc_model"  >> ${work}/input.params
   echo "d"          >> ${work}/input.params
   echo "oper"       >> ${work}/input.params
#-----------------------------------------------------------------
#  run the create_diags script  
#-----------------------------------------------------------------
   sh ./create_diags.sh 
#-----------------------------------------------------------------
# Rename the txt file
#-----------------------------------------------------------------
tempfile="s${Basin2}${StormNum}${yyyy}_${mc_model}_${yyyymmddhh}_diag.dat"
outfile="${STORM}${STORMID}.${yyyymmddhh}.txt"
cp ${work}/${tempfile}  ${work}/${outfile}
#-----------------------------------------------------------------
# copy the .txt file to archive directory to save
#-----------------------------------------------------------------
if [ -d ${archive_dir} ]
then
   echo "${archive_dir} exists" 
else
   mkdir -p ${archive_dir} 
fi 
/bin/cp -f ${work}/*.dat      ${archive_dir}/
/bin/cp -f ${work}/${outfile} ${archive_dir}/
/bin/cp -f ${work}/*3hour*    ${archive_dir}/
rm -rf ${work}
#-----------------------------------------------------------------
exit
#-----------------------------------------------------------------
# end of script 
