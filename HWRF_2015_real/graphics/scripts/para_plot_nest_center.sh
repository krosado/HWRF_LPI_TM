#! /bin/ksh --login
#
# NOTE: Produces plots of the nest domain for one forecast time
# Usage:   sh plot_nest_center.sh STORM   stormid yyyymmddhh MC_MODEL 
# Example: sh plot_nest_center.sh KATRINA   12l   2005082612  H211 
#
# HIST: 2009 taken from hwrf_graphics_lib.bash script by Sam Trahan 
#       2011: Edited by Janna Durante
#       2013: modififed for real-time graphics by Chanh Kieu
#####################################################################
set -x
#-------------------------------------------------------
# get the input variables 
PBS=0
if [ $# -eq 6 ] || [ "$PBS" == "1" ]; then
   if [ "$PBS" == "0" ]; then
    STORM=$1
    stormid=$2
    yyyymmddhh=$3
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
   echo 'home dir   = ' $sorc
else
   echo 'Usage:   sh plot_nest_center.sh STORM   stormid yyyymmddhh MC_MODEL' 
   echo 'NEED FOUR ARGUMENTS'
   echo 'SCRIPT WILL EXIT'
   exit 1 
fi 
#------------------------------------------------------
# create the lower case storm name 
storm=`echo ${STORM} | tr '[A-Z]' '[a-z]'`
# create the upper case storm number
STORMID=`echo ${stormid} | tr '[a-z]' '[A-Z]'`
echo 'storm = ' ${storm}  
echo 'STORMID = ' ${STORMID} 
#
# define the pathways and Determine basin of storm from third letter of
# stormid (l=ATLANTIC, e=EASTPAC, c=CENTRAL PACIFIC)
#
work="${tmp_dir}/tmp/NEST_CENTER/${STORM}${STORMID}/${yyyymmddhh}"
data_dir="${tmp_dir}/tmp/${yyyymmddhh}/${stormid}"
ndate="${utilexec}/ndate"
archive_base="${tmp_dir}/figures"
letter=`echo ${stormid} | cut -c3`
echo 'letter = ' ${letter}
#
# set archive directory on DEW from basin of storm 
# note that CENTRAL Pacific will go to EASTPAC directory
if [ ${letter} = 'l' ]
then
archive_dir="${archive_base}/RT_ATLANTIC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"  
elif [ ${letter} = 'e' ]
then
archive_dir="${archive_base}/RT_EASTPAC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"  
elif [ ${letter} = 'c' ]
then
archive_dir="${archive_base}/RT_CPAC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"  
elif [ ${letter} = 'w' ] || [ ${letter} = 'b' ] || [ ${letter} = 's' ] || [ ${letter} = 'p' ]
then
archive_dir="${archive_base}/RT_WPAC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"
else
   echo "BASIN DESIGNATION LETTER letter = ${letter} NOT LOWER CASE l, e, or c"
   echo 'SCRIPT WILL EXIT'
   exit 1
fi 
#------------------------------------------------------
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
   mkdir -p ${tmp_dir}/tmp/NEST_CENTER/${STORM}${STORMID}/
   mkdir -p ${work} 
   cd ${work}
fi  
pwd
#------------------------------------------------------
# this for loop sequentially copies each 
# file hwrf nest gribfile for hours hh = 00, 06,..., 126  
# to the working directory.   

for hh in 00 03 06 09 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60  \
          63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 
do 
#------------------------------------------------------
#  copy the grib data file to the working directory 
#------------------------------------------------------
   file_name=${storm}${stormid}.${yyyymmddhh}.hwrfprs_n.grbf${hh}  
   /bin/rm -f ${work}/${file_name}  
   echo "processing file = ${file_name}"  
   if [ -s ${data_dir}/${file_name} ]
   then 
      ln -sf ${data_dir}/${file_name}  ${work}/${file_name}   
   else
      echo "FILE ${data_dir}/${file_name} NOT PRESENT OR SIZE ZERO"
      echo 'SCRIPT WILL EXIT'
      exit 1
   fi  
#------------------------------------------------------
#  remove old ctl file 
   /bin/rm -f ${work}/ctl*
#------------------------------------------------------
#  copy the grads scripts to the working directory 
   /bin/cp -f ${sorc}/lib-gs/cbarn.gs                   ${work}/ 
   /bin/cp -f ${sorc}/lib-gs/rgbset.gs                  ${work}/
   /bin/cp -f ${sorc}/lib-gs/para_plot_nest_center.gs   ${work}/ 
   /bin/cp -f ${sorc}/utils/animation_template.in       ${work}/
   /bin/cp -f ${sorc}/utils/cgi_redirect.in             ${work}/
   /bin/cp -f ${sorc}/utils/grib2ctl.pl                 ${work}/
   /bin/cp -f ${utilexec}/wgrib                         ${work}/
#------------------------------------------------------
#  create a grads ctl file
#------------------------------------------------------ 
   ./grib2ctl.pl -verf ${work}/${file_name}   > ${work}/ctl_file
   /bin/rm -f ${work}/*idx
   gribmap -i ${work}/ctl_file
#------------------------------------------------------
#  determine the valid (forecast) date of file 
   vdate=`${ndate} ${hh} ${yyyymmddhh}`
   echo 'vdate = ' ${vdate}
#------------------------------------------------------
#  modify the grads nest template file for the storm name and date 
#  create list of changes for date
#------------------------------------------------------ 
   /bin/rm -f ${work}/changedate 
   /bin/rm -f ${work}/nest_center.gs 
   echo "s/MC_MODEL/${MC_MODEL}/g"        > ${work}/changedate
   echo "s/STORM/${STORM}/g"             >> ${work}/changedate
   echo "s/stormid/${stormid}/g"         >> ${work}/changedate
   echo "s/YYYYMMDDHH/${yyyymmddhh}/g"   >> ${work}/changedate 
   echo "s/HOUR/${hh}/g"                 >> ${work}/changedate
   echo "s/vdate/${vdate}/g"             >> ${work}/changedate 
#------------------------------------------------------
#  apply changes to template file with stream editor
   sed -f ${work}/changedate ${work}/para_plot_nest_center.gs > \
                             ${work}/nest_center.gs 
#------------------------------------------------------
#  run the grads script to make plot for this time  
#  (l=landscape, p=portrait, b=batch, c=execute supplied command) 
   grads -xlbc "run nest_center.gs"  
#------------------------------------------------------ 
done 
#----------------------------------------------------------
# rename the 43 individual .png files with storm and date
/bin/mv -f ${work}/nest_f00.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr1.png
/bin/mv -f ${work}/nest_f03.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr2.png
/bin/mv -f ${work}/nest_f06.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr3.png
/bin/mv -f ${work}/nest_f09.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr4.png
/bin/mv -f ${work}/nest_f12.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr5.png
/bin/mv -f ${work}/nest_f15.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr6.png
/bin/mv -f ${work}/nest_f18.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr7.png
/bin/mv -f ${work}/nest_f21.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr8.png
/bin/mv -f ${work}/nest_f24.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr9.png
/bin/mv -f ${work}/nest_f27.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr10.png
/bin/mv -f ${work}/nest_f30.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr11.png
/bin/mv -f ${work}/nest_f33.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr12.png
/bin/mv -f ${work}/nest_f36.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr13.png
/bin/mv -f ${work}/nest_f39.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr14.png
/bin/mv -f ${work}/nest_f42.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr15.png
/bin/mv -f ${work}/nest_f45.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr16.png
/bin/mv -f ${work}/nest_f48.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr17.png
/bin/mv -f ${work}/nest_f51.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr18.png
/bin/mv -f ${work}/nest_f54.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr19.png
/bin/mv -f ${work}/nest_f57.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr20.png
/bin/mv -f ${work}/nest_f60.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr21.png
/bin/mv -f ${work}/nest_f63.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr22.png
/bin/mv -f ${work}/nest_f66.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr23.png
/bin/mv -f ${work}/nest_f69.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr24.png
/bin/mv -f ${work}/nest_f72.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr25.png
/bin/mv -f ${work}/nest_f75.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr26.png
/bin/mv -f ${work}/nest_f78.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr27.png
/bin/mv -f ${work}/nest_f81.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr28.png
/bin/mv -f ${work}/nest_f84.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr29.png
/bin/mv -f ${work}/nest_f87.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr30.png
/bin/mv -f ${work}/nest_f90.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr31.png
/bin/mv -f ${work}/nest_f93.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr32.png
/bin/mv -f ${work}/nest_f96.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr33.png
/bin/mv -f ${work}/nest_f99.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr34.png
/bin/mv -f ${work}/nest_f102.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr35.png
/bin/mv -f ${work}/nest_f105.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr36.png
/bin/mv -f ${work}/nest_f108.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr37.png
/bin/mv -f ${work}/nest_f111.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr38.png
/bin/mv -f ${work}/nest_f114.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr39.png
/bin/mv -f ${work}/nest_f117.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr40.png
/bin/mv -f ${work}/nest_f120.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr41.png
/bin/mv -f ${work}/nest_f123.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr42.png
/bin/mv -f ${work}/nest_f126.png ${work}/${STORM}${STORMID}.${yyyymmddhh}.ctr43.png
#----------------------------------------------------------
# Make the animation files for Nest Center
#----------------------------------------------------------
ANIM_SUB="Nest"
FIRST=1
LAST=43
NAMEIT=${STORM}${STORMID}.${yyyymmddhh}.ctr
echo "s/MC_MODEL/${MC_MODEL}/g"              > ${work}/changeit
echo "s/STORMNAME/${STORM}/g"                >> ${work}/changeit
echo "s/STORMID/${STORMID}/g"                >> ${work}/changeit
echo "s/YYYYMMDDHH/${yyyymmddhh}/g"          >> ${work}/changeit
echo "s/IMAGE_PREFIX/${NAMEIT}/g"            >> ${work}/changeit
echo "s/ANIMATION_SUBTITLE/${ANIM_SUB}/g"    >> ${work}/changeit
echo "s/FIRST_IMAGE/${FIRST}/g"              >> ${work}/changeit
echo "s/LAST_IMAGE/${LAST}/g"                >> ${work}/changeit
echo "s/REDIRECT_TITLE/hwrf_ctr.html/g"        > ${work}/changeit2
echo "s/REDIRECT_URL/hwrf_ctr.html/g"          >> ${work}/changeit2
#----------------------------------------------------------
# Edit the cgi and html Templates for Nest Center
#----------------------------------------------------------
sed -f ${work}/changeit ${work}/animation_template.in  > ${work}/nest_temp.in
cp -f ${work}/nest_temp.in  ${work}/hwrf_ctr.html
sed -f ${work}/changeit ${work}/cgi_redirect.in  > ${work}/cgi_temp.in
cp -f ${work}/cgi_temp.in  ${work}/hwrf_ctr.cgi
#----------------------------------------------------------
# copy all .png files to archive directory to save
#----------------------------------------------------------
if [ -d ${archive_dir} ]
then
   echo "${archive_dir} exists" 
else
   mkdir -p ${archive_dir} 
fi 
/bin/mv -f ${work}/*.png  ${archive_dir}/
/bin/mv -f ${work}/*.html  ${archive_dir}/
/bin/mv -f ${work}/*.cgi  ${archive_dir}/
rm -r ${work}
exit
# end of script 
