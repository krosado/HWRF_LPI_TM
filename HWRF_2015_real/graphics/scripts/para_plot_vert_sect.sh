#!/bin/ksh  --login
# NOTE:
#      Produces vertical cross section plots of the nest
# Usage:   sh plot_vert_sect.sh STORM   stormid yyyymmddhh MC_MODEL 
# Example: sh plot_vert_sect.sh KATRINA   12l   2005082612   H211
#
# HIST: 2009: Taken from hwrf_graphics_lib.bash script by Sam Trahan
#       2011: Edited for 2011 by Janna Durante
#       2013: revised by Chanh Kieu for real-time graphics
#===================================================================
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
   echo 'sorc       = ' $sorc
else
   echo 'Usage:   sh plot_vert_sect.sh STORM   stormid yyyymmddhh MC_MODEL' 
   echo 'NEED FOUR ARGUMENTS'
   echo 'SCRIPT WILL EXIT'
   exit 1 
fi 
#------------------------------------------------------
# create the lower case storm name 
storm=`echo ${STORM} | tr '[A-Z]' '[a-z]'`
echo 'storm = ' ${storm}  
STORMID=`echo ${stormid} | tr '[a-z]' '[A-Z]'`
echo 'STORMID = ' ${STORMID} 
#
# define the pathways
#
user=`whoami`
work="${tmp_dir}/tmp/NEST_VERT/${STORM}${STORMID}/${yyyymmddhh}"
data_dir="${tmp_dir}/tmp/${yyyymmddhh}/${stormid}"
ndate="${utilexec}/ndate"
output="${storm}${stormid}.${yyyymmddhh}.2ctrs"  
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
echo 'user     = ' ${user}
echo 'sorc     = ' ${sorc}
echo 'work     = ' ${work}  
echo 'data_dir = ' ${data_dir} 
echo 'archive_dir = ' ${archive_dir}  
echo 'output      = ' ${output}  
#------------------------------------------------------
# make the working directory; remove all files; and cd to it
if [ -d ${work} ]
then 
   echo "${work} exists"
   cd ${work}
   /bin/rm -f ${work}/* 
else
   mkdir -p ${tmp_dir}/tmp/NEST_VERT/${STORM}${STORMID}/
   mkdir -p ${work} 
   cd ${work}
fi  
#------------------------------------------------------
# copy the atcf track file to the working directory 
trackfile=${storm}${stormid}.${yyyymmddhh}.trak.hwrf.atcfunix
/bin/cp -f ${data_dir}/$trackfile           ${work}/. 
#------------------------------------------------------------------
# Make stats file
#------------------------------------------------------------------
cp -f ${sorc}/../sorc//hwrf-utilities/exec/grp_atcf_to_stats.exe  ${work}/atcf_to_stats.exe
cp -f ${work}/${trackfile}  ${work}/hwrf.atcfunix
${work}/atcf_to_stats.exe 1> ${work}/hwrf.stats.out 2> ${work}/hwrf.stats.err
#-----------------------------------------------------------------
# add a blank column after the  HOUR: in the hwrf.stats file
sed "s/HOUR\:/HOUR\: /g" ${work}/hwrf.stats > ${work}/statsin
#-----------------------------------------------------------------
# this for loop sequentially copies each NEST grib file  
# for hours hh = 00, 06,..., 126 to the working directory. 
#
 for hh in 00 06 12 18 24 30 36 42 48 54 60  \
           66 72 78 84 90 96 102 108 114 120 126 
do 
#------------------------------------------------------------------
#  copy the grib data file to the working directory 
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
#------------------------------------------------------------------
   if [ ${hh} = '00' ] ;  then lineno=1;  fi
   if [ ${hh} = '06' ] ;  then lineno=2;  fi
   if [ ${hh} = '12' ] ;  then lineno=3;  fi
   if [ ${hh} = '18' ] ;  then lineno=4;  fi
   if [ ${hh} = '24' ] ;  then lineno=5;  fi
   if [ ${hh} = '30' ] ;  then lineno=6;  fi
   if [ ${hh} = '36' ] ;  then lineno=7;  fi
   if [ ${hh} = '42' ] ;  then lineno=8;  fi
   if [ ${hh} = '48' ] ;  then lineno=9;  fi
   if [ ${hh} = '54' ] ;  then lineno=10; fi
   if [ ${hh} = '60' ] ;  then lineno=11; fi
   if [ ${hh} = '66' ] ;  then lineno=12; fi
   if [ ${hh} = '72' ] ;  then lineno=13; fi
   if [ ${hh} = '78' ] ;  then lineno=14; fi
   if [ ${hh} = '84' ] ;  then lineno=15; fi
   if [ ${hh} = '90' ] ;  then lineno=16; fi
   if [ ${hh} = '96' ] ;  then lineno=17; fi
   if [ ${hh} = '102' ] ; then lineno=18; fi
   if [ ${hh} = '108' ] ; then lineno=19; fi
   if [ ${hh} = '114' ] ; then lineno=20; fi
   if [ ${hh} = '120' ] ; then lineno=21; fi
   if [ ${hh} = '126' ] ; then lineno=22; fi
#------------------------------------------------------------------
#  extract the line number of the statsin file corresponding
#  to the forecast hour
#------------------------------------------------------------------
   hour=`head -${lineno} ${work}/statsin | tail -1 | awk '{print $2}'`
   clat=`head -${lineno} ${work}/statsin | tail -1 | awk '{print $6}'`
   if [ "$letter" == "w"  ] || [ "$letter" == "b"  ] ||  \
      [ "$letter" == "s"  ] || [ "$letter" == "p"  ]; then
    clon=`head -${lineno} ${work}/statsin | tail -1 | awk '{print -1*$4}'`
   else
    clon=`head -${lineno} ${work}/statsin | tail -1 | awk '{print $4}'`
   fi
   echo 'hour = ' ${hour} '  clat = ' ${clat} '  clon = ' ${clon}
#------------------------------------------------------
#  copy the grads scripts to the working directory 
   /bin/cp -f ${sorc}/lib-gs/cbarn.gs                   ${work}/ 
   /bin/cp -f ${sorc}/lib-gs/rgbset.gs                  ${work}/
   /bin/cp -f ${sorc}/lib-gs/para_plot_nest_vertXZ.gs   ${work}/ 
   /bin/cp -f ${sorc}/lib-gs/para_plot_nest_vertYZ.gs   ${work}/
   /bin/cp -f ${sorc}/utils/animation_template.in       ${work}/
   /bin/cp -f ${sorc}/utils/cgi_redirect.in             ${work}/
   /bin/cp -f ${sorc}/utils/grib2ctl.pl                 ${work}/
   /bin/cp -f ${utilexec}/wgrib                         ${work}/
#------------------------------------------------------------------
#  remove old ctl file 
   /bin/rm -f ${work}/ctl*
#------------------------------------------------------------------
#  create a grads ctl file 
   ./grib2ctl.pl -verf ${work}/${file_name} > ${work}/ctl_file
   /bin/rm -f ${work}/*idx
   gribmap -i ${work}/ctl_file
#------------------------------------------------------
#  determine the valid (forecast) date of file 
   vdate=`${ndate} ${hh} ${yyyymmddhh}`
   echo 'vdate = ' ${vdate}
#------------------------------------------------------
#  modify the grads vertical cross section template files for the storm 
#  name, date, and lon/lat position of storm center 
#  create list of changes for date 
   /bin/rm -f ${work}/changedate 
   /bin/rm -f ${work}/nest_vertXZ.gs 
   /bin/rm -f ${work}/nest_vertYZ.gs  
   echo "s/STORM/${STORM}/g"           > ${work}/changedate
   echo "s/stormid/${stormid}/g"       >> ${work}/changedate
   echo "s/YYYYMMDDHH/${yyyymmddhh}/g" >> ${work}/changedate 
   echo "s/HOUR/${hh}/g"               >> ${work}/changedate
   echo "s/vdate/${vdate}/g"           >> ${work}/changedate 
   echo "s/CENTER_LAT/${clat}/g"       >> ${work}/changedate
   echo "s/CENTER_LON/${clon}/g"       >> ${work}/changedate
   echo "s/MC_MODEL/${MC_MODEL}/g"     >> ${work}/changedate
#------------------------------------------------------
#  apply changes to template files with stream editor
#  for XZ (east-west) cross section
   sed -f ${work}/changedate ${work}/para_plot_nest_vertXZ.gs > \
                             ${work}/nest_vertXZ.gs 
#------------------------------------------------------
#  for YZ (north-south) cross section 
   sed -f ${work}/changedate ${work}/para_plot_nest_vertYZ.gs > \
                             ${work}/nest_vertYZ.gs 
#------------------------------------------------------
#  run the grads script to make plot in XZ plane for this time  
#  (l=landscape, p=portrait, b=batch, c=execute supplied command) 
   grads -xlbc "run nest_vertXZ.gs"  
#------------------------------------------------------
#  run the grads script to make plot in YZ plane for this time  
#  (l=landscape, p=portrait, b=batch, c=execute supplied command) 
   grads -xlbc "run nest_vertYZ.gs"  
#------------------------------------------------------
done 
#------------------------------------------------------
# rename the 22 individual .png files with storm name and date for XZ cross sections 
/bin/mv -f ${work}/vXZ_f00.png    ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ1.png 
/bin/mv -f ${work}/vXZ_f06.png    ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ2.png 
/bin/mv -f ${work}/vXZ_f12.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ3.png 
/bin/mv -f ${work}/vXZ_f18.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ4.png 
/bin/mv -f ${work}/vXZ_f24.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ5.png 
/bin/mv -f ${work}/vXZ_f30.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ6.png 
/bin/mv -f ${work}/vXZ_f36.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ7.png 
/bin/mv -f ${work}/vXZ_f42.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ8.png 
/bin/mv -f ${work}/vXZ_f48.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ9.png 
/bin/mv -f ${work}/vXZ_f54.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ10.png 
/bin/mv -f ${work}/vXZ_f60.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ11.png 
/bin/mv -f ${work}/vXZ_f66.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ12.png 
/bin/mv -f ${work}/vXZ_f72.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ13.png 
/bin/mv -f ${work}/vXZ_f78.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ14.png 
/bin/mv -f ${work}/vXZ_f84.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ15.png 
/bin/mv -f ${work}/vXZ_f90.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ16.png 
/bin/mv -f ${work}/vXZ_f96.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ17.png 
/bin/mv -f ${work}/vXZ_f102.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ18.png 
/bin/mv -f ${work}/vXZ_f108.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ19.png 
/bin/mv -f ${work}/vXZ_f114.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ20.png 
/bin/mv -f ${work}/vXZ_f120.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ21.png 
/bin/mv -f ${work}/vXZ_f126.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.vXZ22.png 
#------------------------------------------------------
# rename the 22 individual .png files with storm name and date for YZ cross sections 
/bin/mv -f ${work}/vYZ_f00.png    ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ1.png 
/bin/mv -f ${work}/vYZ_f06.png    ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ2.png 
/bin/mv -f ${work}/vYZ_f12.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ3.png 
/bin/mv -f ${work}/vYZ_f18.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ4.png 
/bin/mv -f ${work}/vYZ_f24.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ5.png 
/bin/mv -f ${work}/vYZ_f30.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ6.png 
/bin/mv -f ${work}/vYZ_f36.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ7.png 
/bin/mv -f ${work}/vYZ_f42.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ8.png 
/bin/mv -f ${work}/vYZ_f48.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ9.png 
/bin/mv -f ${work}/vYZ_f54.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ10.png 
/bin/mv -f ${work}/vYZ_f60.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ11.png 
/bin/mv -f ${work}/vYZ_f66.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ12.png 
/bin/mv -f ${work}/vYZ_f72.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ13.png 
/bin/mv -f ${work}/vYZ_f78.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ14.png 
/bin/mv -f ${work}/vYZ_f84.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ15.png 
/bin/mv -f ${work}/vYZ_f90.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ16.png 
/bin/mv -f ${work}/vYZ_f96.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ17.png 
/bin/mv -f ${work}/vYZ_f102.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ18.png 
/bin/mv -f ${work}/vYZ_f108.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ19.png 
/bin/mv -f ${work}/vYZ_f114.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ20.png 
/bin/mv -f ${work}/vYZ_f120.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ21.png 
/bin/mv -f ${work}/vYZ_f126.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.vYZ22.png 
#------------------------------------------------------
# Make the animation files for XZ (E-W) 
#----------------------------------------------------------
ANIM_SUB="Meridional"
FIRST=1
LAST=22
NAMEIT=${STORM}${STORMID}.${yyyymmddhh}.vXZ

echo "s/MC_MODEL/${MC_MODEL}/g"              > ${work}/changeit
echo "s/STORMNAME/${STORM}/g"                >> ${work}/changeit
echo "s/STORMID/${STORMID}/g"                >> ${work}/changeit
echo "s/YYYYMMDDHH/${yyyymmddhh}/g"          >> ${work}/changeit
echo "s/IMAGE_PREFIX/${NAMEIT}/g"            >> ${work}/changeit
echo "s/ANIMATION_SUBTITLE/${ANIM_SUB}/g"    >> ${work}/changeit
echo "s/FIRST_IMAGE/${FIRST}/g"              >> ${work}/changeit
echo "s/LAST_IMAGE/${LAST}/g"                >> ${work}/changeit
#----------------------------------------------------------
# Edit the cgi and html Templates for XZ (E-W) 
#----------------------------------------------------------
sed -f ${work}/changeit ${work}/animation_template.in  > ${work}/nest_temp.in
cp -f ${work}/nest_temp.in  ${work}/hwrf_ew.html
sed -f ${work}/changeit ${work}/cgi_redirect.in  > ${work}/cgi_temp.in
cp -f ${work}/cgi_temp.in  ${work}/hwrf_ew.cgi
#----------------------------------------------------------
# Make the animation files for YZ (N-S)
#----------------------------------------------------------
ANIM_SUB="Zonal"
FIRST=1
LAST=22
NAMEIT=${STORM}${STORMID}.${yyyymmddhh}.vYZ

echo "s/MC_MODEL/${MC_MODEL}/g"              > ${work}/changeit
echo "s/STORMNAME/${STORM}/g"                >> ${work}/changeit
echo "s/STORMID/${STORMID}/g"                >> ${work}/changeit
echo "s/YYYYMMDDHH/${yyyymmddhh}/g"          >> ${work}/changeit
echo "s/IMAGE_PREFIX/${NAMEIT}/g"            >> ${work}/changeit
echo "s/ANIMATION_SUBTITLE/${ANIM_SUB}/g"    >> ${work}/changeit
echo "s/FIRST_IMAGE/${FIRST}/g"              >> ${work}/changeit
echo "s/LAST_IMAGE/${LAST}/g"                >> ${work}/changeit
#----------------------------------------------------------
# Edit the cgi and html Templates for YZ (N-S)
#----------------------------------------------------------
sed -f ${work}/changeit ${work}/animation_template.in  > ${work}/nest_temp.in
cp -f ${work}/nest_temp.in  ${work}/hwrf_ns.html
sed -f ${work}/changeit ${work}/cgi_redirect.in  > ${work}/cgi_temp.in
cp -f ${work}/cgi_temp.in  ${work}/hwrf_ns.cgi
#----------------------------------------------------------
# Make the animation files for combination of XZ and YZ (N-S)
#----------------------------------------------------------
ANIM_SUB="Zonal and Meridional"
FIRST=1
LAST=22
NAMEIT=${STORM}${STORMID}.${yyyymmddhh}.vx
echo "s/MC_MODEL/${MC_MODEL}/g"              > ${work}/changeit
echo "s/STORMNAME/${STORM}/g"                >> ${work}/changeit
echo "s/STORMID/${STORMID}/g"                >> ${work}/changeit
echo "s/YYYYMMDDHH/${yyyymmddhh}/g"          >> ${work}/changeit
echo "s/IMAGE_PREFIX/${NAMEIT}/g"            >> ${work}/changeit
echo "s/ANIMATION_SUBTITLE/${ANIM_SUB}/g"    >> ${work}/changeit
echo "s/FIRST_IMAGE/${FIRST}/g"              >> ${work}/changeit
echo "s/LAST_IMAGE/${LAST}/g"                >> ${work}/changeit
echo "s/768)/540)/g"                         >> ${work}/changeit
echo "s/1024,/1324,/g"                       >> ${work}/changeit
#----------------------------------------------------------
# Edit the cgi and html Templates for YZ (N-S)
#----------------------------------------------------------
sed -f ${work}/changeit ${work}/animation_template.in  > ${work}/nest_temp.in
cp -f ${work}/nest_temp.in  ${work}/hwrf_x.html
sed -f ${work}/changeit ${work}/cgi_redirect.in  > ${work}/cgi_temp.in
cp -f ${work}/cgi_temp.in  ${work}/hwrf_x.cgi
#----------------------------------------------------------
# copy all .png files to archive directory to save
if [ -d ${archive_dir} ]
then
   echo "${archive_dir} exists" 
else
   mkdir -p ${archive_dir} 
fi 
/bin/mv -f ${work}/*.png   ${archive_dir}/
/bin/mv -f ${work}/*.html  ${archive_dir}/
/bin/mv -f ${work}/*.cgi   ${archive_dir}/
rm -r ${work}
cd ${archive_dir}
#
# merge vertical cross sections
#
rm -rf *.vx*.png
for icount in {1..22}
do
 montage -mode concatenate -tile 2x1 ${STORM}${STORMID}.${yyyymmddhh}.vXZ${icount}.png       \
          ${STORM}${STORMID}.${yyyymmddhh}.vYZ${icount}.png ${STORM}${STORMID}.${yyyymmddhh}.vx${icount}.png
 rm -f ${STORM}${STORMID}.${yyyymmddhh}.vXZ${icount}.png ${STORM}${STORMID}.${yyyymmddhh}.vYZ${icount}.png
 icount=$(($icount+1))
done
exit
# end of script 
