#! /bin/ksh --login
#
# NOTE: This unix shell script plots HWRF-simulated GOES images 
#
# Usage:   sh plot_goes.sh STORM   stormid yyyymmddhh MC_MODEL 
# Example: sh plot_goes.sh KATRINA   12l   2005082612  HWRF
#
# HIST: 2009: Taken from hwrf_graphics_lib.bash script by Sam Trahan
#       2011: Edited for 2011 by Janna Durante
#       2013: modified for real-time graphics
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
   echo 'Usage:   sh plot_goes.sh STORM   stormid yyyymmddhh MC_MODEL' 
   echo 'NEED FOUR ARGUMENTS'
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

echo 'storm = ' ${storm}  
echo 'STORMID = ' ${STORMID} 
echo 'HOUR    = ' ${HOUR} 
#------------------------------------------------------
# define the pathways
user=`whoami`
work="${tmp_dir}/tmp/GOES/${STORM}${STORMID}/${yyyymmddhh}"
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
elif [ ${letter} = 'e' ]
then
archive_dir="${archive_base}/RT_EASTPAC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"  
elif [ ${letter} = 'c' ]
then
archive_dir="${archive_base}/RT_CPAC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"  
elif [ ${letter} = 'a' ]
then
archive_dir="${archive_base}/RT_ARABIAN/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"
elif [ ${letter} = 'b' ]
then
archive_dir="${archive_base}/RT_WPAC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"
elif [ ${letter} = 'w' ] 
then
archive_dir="${archive_base}/RT_WPAC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"
elif [ ${letter} = 's' ]
then
archive_dir="${archive_base}/RT_WPAC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"
elif [ ${letter} = 'p' ]
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
#------------------------------------------------------
# make the working directory; remove all files; and cd to it
if [ -d ${work} ]
then 
   echo "${work} exists"
   cd ${work}
   /bin/rm -f ${work}/* 
else
   mkdir -p ${tmp_dir}/tmp/GOES/${STORM}${STORMID}
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
gribfile=${storm}${stormid}.${yyyymmddhh}.hwrfsat_p.grbf${hh}
center=${storm}${stormid}.${yyyymmddhh}.storm.center
echo 'gribfile = ' ${gribfile}
if [ -s ${data_dir}/${gribfile} ]
then
   ln -sf ${data_dir}/${gribfile} ${work}
   ln -sf ${data_dir}/${center} ${work}
else
   echo "FILE ${data_dir}/${gribfile} NOT PRESENT"
   echo 'SCRIPT WILL EXIT'
   exit 1
fi
#-----------------------------------------------------------------
# copy the grads scripts as a template to working directory
   /bin/cp -f ${sorc}/lib-gs/mikewv.gs             ${work}/
   /bin/cp -f ${sorc}/lib-gs/cbarn2.gs             ${work}/
   /bin/cp -f ${sorc}/lib-gs/mikeir.gs             ${work}/
if [ ${letter} = 'l' ]
then 
   /bin/cp -f ${sorc}/lib-gs/para_plot_goes.gs            ${work}/
elif [ ${letter} = 'a' ]
then
   /bin/cp -f ${sorc}/lib-gs/para_plot_goes.gs            ${work}/
elif [ ${letter} = 'w' ] || [ ${letter} = 'b' ] || \
     [ ${letter} = 's' ] || [ ${letter} = 'p' ]
then
   /bin/cp -f ${sorc}/lib-gs/para_plot_mtsat_wp.gs        ${work}/para_plot_goes.gs
else
   /bin/cp -f ${sorc}/lib-gs/para_plot_goes_ep.gs         ${work}/para_plot_goes.gs
fi
   /bin/cp -f ${sorc}/lib-gs/cbarm.gs                     ${work}/
   /bin/cp -f ${sorc}/lib-gs/colormap-37ghz-withland.gs   ${work}/
   /bin/cp -f ${sorc}/lib-gs/colormap-89ghz-withland.gs   ${work}/
   /bin/cp -f ${sorc}/lib-gs/para_plot_microwave.gs       ${work}/
   /bin/cp -f ${sorc}/utils/animation_template.in         ${work}/
   /bin/cp -f ${sorc}/utils/cgi_redirect.in               ${work}/
   /bin/cp -f ${sorc}/utils/grib2ctl.pl                   ${work}/
   /bin/cp -f ${utilexec}/wgrib                           ${work}/
#-------------------------------------------------------------
# create a control file for the GOES data
./grib2ctl.pl -verf ${work}/${gribfile} > ${work}/ctl_file
gribmap -v -i  ${work}/ctl_file

#--------------------------------------------------------------
#  determine the valid (forecast) date of file 
   vdate=`${ndate} ${hh} ${yyyymmddhh}`
   echo 'vdate = ' ${vdate}
#-----------------------------------------------------------------
#  modify the grads parent template file for the storm name and date 
#-----------------------------------------------------------------
   /bin/rm -f ${work}/changedate 
   /bin/rm -f ${work}/goes.gs 
   /bin/rm -f ${work}/microwave.gs
   echo "s/STORM/${STORM}/g"            > ${work}/changedate
   echo "s/stormid/${stormid}/g"       >> ${work}/changedate
   echo "s/YYYYMMDDHH/${yyyymmddhh}/g" >> ${work}/changedate 
   echo "s/HOUR/${hh}/g"               >> ${work}/changedate
   echo "s/vdate/${vdate}/g"           >> ${work}/changedate 
   echo "s/MC_MODEL/${MC_MODEL}/g"     >> ${work}/changedate
#   if [ ${letter} = 'b' ]; then
#    echo "s/BRTMP117/BRTMP167/g"       >> ${work}/changedate 
#   fi
#-----------------------------------------------------------------
#  apply changes to template file with stream editor
   sed -f ${work}/changedate ${work}/para_plot_goes.gs > ${work}/goes.gs 
   sed -f ${work}/changedate ${work}/para_plot_microwave.gs > ${work}/microwave.gs
#-----------------------------------------------------------------
#  run the grads script to make plot for this time  
   if [ ${letter} = 'b' ]; then
    sed -i 's/BRTMP2_3cm/SBT123toa/g' goes.gs
    sed -i 's/BRTMP2_1cm/SBT124toa/g' goes.gs
   fi
   grads -xlbc "run goes.gs" 
   grads -xlbc "run microwave.gs" 
#-----------------------------------------------------------------
done
#-----------------------------------------------------------------
# rename the 21 individual .png files with storm and date 
#-----------------------------------------------------------------
/bin/mv -f ${work}/par_goesWV_f00.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV1.png  
/bin/mv -f ${work}/par_goesWV_f06.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV2.png  
/bin/mv -f ${work}/par_goesWV_f12.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV3.png  
/bin/mv -f ${work}/par_goesWV_f18.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV4.png  
/bin/mv -f ${work}/par_goesWV_f24.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV5.png  
/bin/mv -f ${work}/par_goesWV_f30.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV6.png  
/bin/mv -f ${work}/par_goesWV_f36.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV7.png  
/bin/mv -f ${work}/par_goesWV_f42.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV8.png  
/bin/mv -f ${work}/par_goesWV_f48.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV9.png  
/bin/mv -f ${work}/par_goesWV_f54.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV10.png  
/bin/mv -f ${work}/par_goesWV_f60.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV11.png  
/bin/mv -f ${work}/par_goesWV_f66.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV12.png  
/bin/mv -f ${work}/par_goesWV_f72.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV13.png  
/bin/mv -f ${work}/par_goesWV_f78.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV14.png  
/bin/mv -f ${work}/par_goesWV_f84.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV15.png  
/bin/mv -f ${work}/par_goesWV_f90.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV16.png  
/bin/mv -f ${work}/par_goesWV_f96.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV17.png  
/bin/mv -f ${work}/par_goesWV_f102.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV18.png  
/bin/mv -f ${work}/par_goesWV_f108.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV19.png  
/bin/mv -f ${work}/par_goesWV_f114.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV20.png  
/bin/mv -f ${work}/par_goesWV_f120.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV21.png  
/bin/mv -f ${work}/par_goesWV_f126.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesWV22.png  
#-----------------------------------------------------------------
# rename the 21 individual .png files with storm and date
#-----------------------------------------------------------------
/bin/mv -f ${work}/par_goesIR_f00.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR1.png
/bin/mv -f ${work}/par_goesIR_f06.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR2.png
/bin/mv -f ${work}/par_goesIR_f12.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR3.png
/bin/mv -f ${work}/par_goesIR_f18.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR4.png
/bin/mv -f ${work}/par_goesIR_f24.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR5.png
/bin/mv -f ${work}/par_goesIR_f30.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR6.png
/bin/mv -f ${work}/par_goesIR_f36.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR7.png
/bin/mv -f ${work}/par_goesIR_f42.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR8.png
/bin/mv -f ${work}/par_goesIR_f48.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR9.png
/bin/mv -f ${work}/par_goesIR_f54.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR10.png
/bin/mv -f ${work}/par_goesIR_f60.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR11.png
/bin/mv -f ${work}/par_goesIR_f66.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR12.png
/bin/mv -f ${work}/par_goesIR_f72.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR13.png
/bin/mv -f ${work}/par_goesIR_f78.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR14.png
/bin/mv -f ${work}/par_goesIR_f84.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR15.png
/bin/mv -f ${work}/par_goesIR_f90.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR16.png
/bin/mv -f ${work}/par_goesIR_f96.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR17.png
/bin/mv -f ${work}/par_goesIR_f102.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR18.png
/bin/mv -f ${work}/par_goesIR_f108.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR19.png
/bin/mv -f ${work}/par_goesIR_f114.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR20.png
/bin/mv -f ${work}/par_goesIR_f120.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR21.png
/bin/mv -f ${work}/par_goesIR_f126.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.par_goesIR22.png
#----------------------------------------------------------
# rename the 21 individual .png files with storm and date
#----------------------------------------------------------
/bin/mv -f ${work}/microwave_37_f00.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_1.png
/bin/mv -f ${work}/microwave_37_f06.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_2.png
/bin/mv -f ${work}/microwave_37_f12.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_3.png
/bin/mv -f ${work}/microwave_37_f18.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_4.png
/bin/mv -f ${work}/microwave_37_f24.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_5.png
/bin/mv -f ${work}/microwave_37_f30.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_6.png
/bin/mv -f ${work}/microwave_37_f36.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_7.png
/bin/mv -f ${work}/microwave_37_f42.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_8.png
/bin/mv -f ${work}/microwave_37_f48.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_9.png
/bin/mv -f ${work}/microwave_37_f54.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_10.png
/bin/mv -f ${work}/microwave_37_f60.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_11.png
/bin/mv -f ${work}/microwave_37_f66.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_12.png
/bin/mv -f ${work}/microwave_37_f72.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_13.png
/bin/mv -f ${work}/microwave_37_f78.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_14.png
/bin/mv -f ${work}/microwave_37_f84.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_15.png
/bin/mv -f ${work}/microwave_37_f90.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_16.png
/bin/mv -f ${work}/microwave_37_f96.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_17.png
/bin/mv -f ${work}/microwave_37_f102.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_18.png
/bin/mv -f ${work}/microwave_37_f108.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_19.png
/bin/mv -f ${work}/microwave_37_f114.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_20.png
/bin/mv -f ${work}/microwave_37_f120.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_21.png
/bin/mv -f ${work}/microwave_37_f126.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro37_22.png
#----------------------------------------------------------
# rename the 21 individual .png files with storm and date
#----------------------------------------------------------
/bin/mv -f ${work}/microwave_89_f00.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_1.png
/bin/mv -f ${work}/microwave_89_f06.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_2.png
/bin/mv -f ${work}/microwave_89_f12.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_3.png
/bin/mv -f ${work}/microwave_89_f18.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_4.png
/bin/mv -f ${work}/microwave_89_f24.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_5.png
/bin/mv -f ${work}/microwave_89_f30.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_6.png
/bin/mv -f ${work}/microwave_89_f36.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_7.png
/bin/mv -f ${work}/microwave_89_f42.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_8.png
/bin/mv -f ${work}/microwave_89_f48.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_9.png
/bin/mv -f ${work}/microwave_89_f54.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_10.png
/bin/mv -f ${work}/microwave_89_f60.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_11.png
/bin/mv -f ${work}/microwave_89_f66.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_12.png
/bin/mv -f ${work}/microwave_89_f72.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_13.png
/bin/mv -f ${work}/microwave_89_f78.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_14.png
/bin/mv -f ${work}/microwave_89_f84.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_15.png
/bin/mv -f ${work}/microwave_89_f90.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_16.png
/bin/mv -f ${work}/microwave_89_f96.png   ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_17.png
/bin/mv -f ${work}/microwave_89_f102.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_18.png
/bin/mv -f ${work}/microwave_89_f108.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_19.png
/bin/mv -f ${work}/microwave_89_f114.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_20.png
/bin/mv -f ${work}/microwave_89_f120.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_21.png
/bin/mv -f ${work}/microwave_89_f126.png  ${work}/${STORM}${STORMID}.${yyyymmddhh}.micro89_22.png
#-----------------------------------------------------------------
# Make the animation files for GOES WV 
#-----------------------------------------------------------------
ANIM_SUB="WV"
FIRST=1
LAST=22
NAMEIT=${STORM}${STORMID}.${yyyymmddhh}.par_goesWV
echo "s/MC_MODEL/${MC_MODEL}/g"               > ${work}/changeit
echo "s/STORMNAME/${STORM}/g"                >> ${work}/changeit
echo "s/STORMID/${STORMID}/g"                >> ${work}/changeit
echo "s/YYYYMMDDHH/${yyyymmddhh}/g"          >> ${work}/changeit
echo "s/IMAGE_PREFIX/${NAMEIT}/g"            >> ${work}/changeit
echo "s/ANIMATION_SUBTITLE/${ANIM_SUB}/g"    >> ${work}/changeit
echo "s/FIRST_IMAGE/${FIRST}/g"              >> ${work}/changeit
echo "s/LAST_IMAGE/${LAST}/g"                >> ${work}/changeit
echo "s/768)/720)/g"                         >> ${work}/changeit
echo "s/1024,/1324,/g"                       >> ${work}/changeit
echo "s/REDIRECT_TITLE/par_GOESWV.html/g"     > ${work}/changeit2
echo "s/REDIRECT_URL/par_GOESWV.html/g"      >> ${work}/changeit2
sed -f ${work}/changeit ${work}/animation_template.in  > ${work}/nest_temp.in
cp -f ${work}/nest_temp.in  ${work}/par_GOESWV.html
sed -f ${work}/changeit ${work}/cgi_redirect.in  > ${work}/cgi_temp.in
cp -f ${work}/cgi_temp.in  ${work}/par_GOESWV.cgi
#-----------------------------------------------------------------
# Make the animation files for GOES IR
#-----------------------------------------------------------------
ANIM_SUB="IR"
FIRST=1
LAST=22
NAMEIT=${STORM}${STORMID}.${yyyymmddhh}.par_goesIR
echo "s/MC_MODEL/${MC_MODEL}/g"               > ${work}/changeit
echo "s/STORMNAME/${STORM}/g"                >> ${work}/changeit
echo "s/STORMID/${STORMID}/g"                >> ${work}/changeit
echo "s/YYYYMMDDHH/${yyyymmddhh}/g"          >> ${work}/changeit
echo "s/IMAGE_PREFIX/${NAMEIT}/g"            >> ${work}/changeit
echo "s/ANIMATION_SUBTITLE/${ANIM_SUB}/g"    >> ${work}/changeit
echo "s/FIRST_IMAGE/${FIRST}/g"              >> ${work}/changeit
echo "s/LAST_IMAGE/${LAST}/g"                >> ${work}/changeit
echo "s/768)/720)/g"                         >> ${work}/changeit
echo "s/1024,/1324,/g"                       >> ${work}/changeit
echo "s/REDIRECT_TITLE/par_GOESIR.html/g"     > ${work}/changeit2
echo "s/REDIRECT_URL/par_GOESIR.html/g"      >> ${work}/changeit2
sed -f ${work}/changeit ${work}/animation_template.in  > ${work}/nest_temp.in
cp -f ${work}/nest_temp.in  ${work}/par_GOESIR.html
sed -f ${work}/changeit ${work}/cgi_redirect.in  > ${work}/cgi_temp.in
cp -f ${work}/cgi_temp.in  ${work}/par_GOESIR.cgi
#-----------------------------------------------------------------
# Make the animation files for 37GHz Microwave Images
#-----------------------------------------------------------------
ANIM_SUB="37GHz"
FIRST=1
LAST=22
NAMEIT=${STORM}${STORMID}.${yyyymmddhh}.micro37_
echo "s/MC_MODEL/${MC_MODEL}/g"               > ${work}/changeit
echo "s/STORMNAME/${STORM}/g"                >> ${work}/changeit
echo "s/STORMID/${STORMID}/g"                >> ${work}/changeit
echo "s/YYYYMMDDHH/${yyyymmddhh}/g"          >> ${work}/changeit
echo "s/IMAGE_PREFIX/${NAMEIT}/g"            >> ${work}/changeit
echo "s/ANIMATION_SUBTITLE/${ANIM_SUB}/g"    >> ${work}/changeit
echo "s/FIRST_IMAGE/${FIRST}/g"              >> ${work}/changeit
echo "s/LAST_IMAGE/${LAST}/g"                >> ${work}/changeit
echo "s/768)/720)/g"                         >> ${work}/changeit
echo "s/1024,/1324,/g"                       >> ${work}/changeit
echo "s/REDIRECT_TITLE/micro_37.html/g"       > ${work}/changeit2
echo "s/REDIRECT_URL/micro_37.html/g"        >> ${work}/changeit2
sed -f ${work}/changeit ${work}/animation_template.in  > ${work}/nest_temp.in
cp -f ${work}/nest_temp.in  ${work}/micro_37.html
sed -f ${work}/changeit ${work}/cgi_redirect.in  > ${work}/cgi_temp.in
cp -f ${work}/cgi_temp.in  ${work}/micro_37.cgi
#-----------------------------------------------------------------
# Make the animation files for 89GHz Microwave Images
#-----------------------------------------------------------------
ANIM_SUB="89GHz"
FIRST=1
LAST=22
NAMEIT=${STORM}${STORMID}.${yyyymmddhh}.micro89_
echo "s/MC_MODEL/${MC_MODEL}/g"               > ${work}/changeit
echo "s/STORMNAME/${STORM}/g"                >> ${work}/changeit
echo "s/STORMID/${STORMID}/g"                >> ${work}/changeit
echo "s/YYYYMMDDHH/${yyyymmddhh}/g"          >> ${work}/changeit
echo "s/IMAGE_PREFIX/${NAMEIT}/g"            >> ${work}/changeit
echo "s/ANIMATION_SUBTITLE/${ANIM_SUB}/g"    >> ${work}/changeit
echo "s/FIRST_IMAGE/${FIRST}/g"              >> ${work}/changeit
echo "s/LAST_IMAGE/${LAST}/g"                >> ${work}/changeit
echo "s/768)/720)/g"                         >> ${work}/changeit
echo "s/1024,/1324,/g"                       >> ${work}/changeit
echo "s/REDIRECT_TITLE/micro_89.html/g"       > ${work}/changeit2
echo "s/REDIRECT_URL/micro_89.html/g"        >> ${work}/changeit2
sed -f ${work}/changeit ${work}/animation_template.in  > ${work}/nest_temp.in
cp -f ${work}/nest_temp.in  ${work}/micro_89.html
sed -f ${work}/changeit ${work}/cgi_redirect.in  > ${work}/cgi_temp.in
cp -f ${work}/cgi_temp.in  ${work}/micro_89.cgi
#-----------------------------------------------------------------
# copy all .png files to archive directory to save
#-----------------------------------------------------------------
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
#-----------------------------------------------------------------
exit
#-----------------------------------------------------------------
# end of script 
