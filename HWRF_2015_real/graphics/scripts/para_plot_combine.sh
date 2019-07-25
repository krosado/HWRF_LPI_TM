#! /bin/ksh --login
# NOTE: Produces the combined and nest movement "movie" (series of 
# independent image files) using the combined parent-nest grib files
#
# Usage:   sh movie_combine.sh STORM stormid yyyymmddhh MC_MODEL 
# Example: sh movie_combine.sh BARRY   02l   2007060118  H211
#
# HIST:
#      2009: Taken from hwrf_graphics_lib.bash script written by Sam Trahan
#      2011: Edited for 2011 by Janna Durante
#      2012: re-desinged by Chanh Kieu for more efficient control
##################################################################
set -x 
#------------------------------------------------------
# get the input variables 
PBS=0
if [ $# -eq 6 ] || [ "$PBS" == "1" ]
then
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
   echo 'USAGE:   sh movie_combine.sh STORM stormid yyyymmddhh MC_MODEL'
   echo 'NEED FOUR ARGUMENTS - SCRIPT WILL EXIT'
   exit 1  
fi
#
# create the lower case storm name and create upper case storm number
#
storm=`echo ${STORM} | tr '[A-Z]' '[a-z]'`
cyc=`echo ${yyyymmddhh} | cut -c9-10`
STORMID=`echo ${stormid} | tr '[a-z]' '[A-Z]' ` 
echo 'storm = ' ${storm}
echo 'cyc   = ' ${cyc}  
echo 'STORMID = ' ${STORMID}  
#
# set the pathways. Note that data_dir is the path of the HWRF model 
# output data files, and contains data files for the PARENT, NEST,
# and COMBINE domains. 
#
data_dir="${tmp_dir}/tmp/${yyyymmddhh}/${stormid}"
work="${tmp_dir}/tmp/COMBINE_MOVIE/${STORM}${STORMID}/$yyyymmddhh"  
archive_base="${tmp_dir}/figures"  
#
# Determine basin of storm from third letter of stormid 
# (l=ATLANTIC, e=EASTPAC, c=CENTRAL PACIFIC) and set archive directory
# on DEW from basin of storm note that CENTRAL Pacific will go to EASTPAC dir 
#
letter=`echo ${stormid} | cut -c3`
echo 'letter = ' ${letter}
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
echo 'sorc =      ' ${sorc}
echo 'data_dir =  ' ${data_dir} 
echo 'work      = ' ${work}  
echo 'archive_dir = ' ${archive_dir}  
#-------------------------------------------------------
# make the working directory; remove all files; and cd to it
if [ -d ${work} ]
then
   echo "${work} exists"
   cd ${work}
   /bin/rm -f ${work}/* 
else
   mkdir -p ${tmp_dir}/tmp/COMBINE_MOVIE/${STORM}${STORMID}/
   mkdir -p ${work}
   cd ${work}
fi 
pwd 
#
# check to see if the grib cycle files for the COMBINE and NEST domains
# are present and copy it to working directory; else exit
#
gribfile=${storm}${stormid}.${yyyymmddhh}.hwrfprs_p.grbf
nestfile=${storm}${stormid}.${yyyymmddhh}.hwrfprs_n.grbf
trackfile=${storm}${stormid}.${yyyymmddhh}.trak.hwrf.atcfunix
echo 'gribfile = ' ${gribfile}  
if [ -d ${data_dir} ]; then
   ln -sf ${data_dir}/${gribfile}*  ${work}
   ln -sf ${data_dir}/${nestfile}*  ${work} 
   ln -sf ${data_dir}/${trackfile} ${work} 
else
   echo "FILE ${trackfile} NOT PRESENT"  
   echo 'SCRIPT WILL EXIT'
   exit 1   
fi
#
# copy the grads scripts as a template to working directory 
#
/bin/cp -f ${sorc}/lib-gs/para_plot_streamline.gs    ${work}/         
/bin/cp -f ${sorc}/lib-gs/para_plot_nest_move.gs     ${work}/
/bin/cp -f ${sorc}/lib-gs/cbarn.gs                   ${work}/
/bin/cp -f ${sorc}/lib-gs/rgbset.gs                  ${work}/
/bin/cp -f ${sorc}/lib-gs/para_plot_single_track.gs  ${work}/
/bin/cp -f ${sorc}/scripts/para_plot_combine_core.sh ${work}/
/bin/cp -f ${sorc}/utils/animation_template.in       ${work}/
/bin/cp -f ${sorc}/utils/cgi_redirect.in             ${work}/
/bin/cp -f ${sorc}/utils/grib2ctl.pl                 ${work}/
/bin/cp -f ${utilexec}/wgrib                         ${work}/
sh ./para_plot_combine_core.sh ${storm} ${stormid} ${yyyymmddhh} ${MC_MODEL}
#
# determine the date for title on plot to be changed in file combine.gs and nest_movie.gs 
#
yyyy=`echo $yyyymmddhh | cut -c1-4`
mm=`echo $yyyymmddhh | cut -c5-6`
dd=`echo $yyyymmddhh | cut -c7-8`
hh=`echo $yyyymmddhh | cut -c9-10`
echo 'yyyy = ' ${yyyy}
echo 'mm   = ' ${mm}
echo 'dd   = ' ${dd}
echo 'hh   = ' ${hh} 
if [ $mm -eq '01' ];then MM=JAN;fi
if [ $mm -eq '02' ];then MM=FEB;fi
if [ $mm -eq '03' ];then MM=MAR;fi
if [ $mm -eq '04' ];then MM=APR;fi
if [ $mm -eq '05' ];then MM=MAY;fi
if [ $mm -eq '06' ];then MM=JUN;fi
if [ $mm -eq '07' ];then MM=JUL;fi
if [ $mm -eq '08' ];then MM=AUG;fi
if [ $mm -eq '09' ];then MM=SEP;fi
if [ $mm -eq '10' ];then MM=OCT;fi
if [ $mm -eq '11' ];then MM=NOV;fi
if [ $mm -eq '12' ];then MM=DEC;fi
#--------------------------------------------------------------
# Create the stats file to use for plotting the Nest
#--------------------------------------------------------------
cp -f ${sorc}/../sorc//hwrf-utilities/exec/grp_atcf_to_stats.exe  ${work}/atcf_to_stats.exe
cp -f ${work}/${trackfile} ${work}/hwrf.atcfunix
${work}/atcf_to_stats.exe 1> ${work}/hwrf.stats.out 2> ${work}/hwrf.stats.err
#--------------------------------------------------------------
# add a blank column after the  HOUR: in the hwrf.stats file
sed "s/HOUR\:/HOUR\: /g" ${work}/hwrf.stats > ${work}/statsin
#------------------------------------------------------
# copy fortran program to find boundaries of domain for plotting
cp -f ${sorc}/../sorc//hwrf-utilities/exec/grp_statsin_domain.exe ${work}/statsin_domain.exe
${work}/statsin_domain.exe 1> ${work}/statsin_domain.out   \
                       2> ${work}/statsin_domain.err
#-----------------------------------------------------------------
# get the lon/lat boundary values from file HWRF.track 
slat=`cat ${work}/bndry_pts.txt | awk '{ print ($1) }'`
nlat=`cat ${work}/bndry_pts.txt | awk '{ print ($2) }'`
wlon=`cat ${work}/bndry_pts.txt | awk '{ print ($3) }'`
elon=`cat ${work}/bndry_pts.txt | awk '{ print ($4) }'`
#----------------------------------------------------------
# create a file of commands to be used by stream editor to
# change the grads script 'hwrf_gr_combine.gs' and 'hwrf_gr_nest_movie.gs' 
#
echo "s/STRMONTH/$MM/g"            > ${work}/changedate
echo "s/DAY/${dd}/g"               >> ${work}/changedate
echo "s/YEAR/${yyyy}/g"            >> ${work}/changedate
echo "s/HOUR/${hh}/g"              >> ${work}/changedate
echo "s/STORM/${STORM}/g"          >> ${work}/changedate  
echo "s/stormid/${stormid}/g"      >> ${work}/changedate  
echo "s/MC_MODEL/${MC_MODEL}/g"    >> ${work}/changedate
echo "s/slat/$slat/g"              >> ${work}/changedate
echo "s/nlat/$nlat/g"              >> ${work}/changedate
echo "s/elon/$elon/g"              >> ${work}/changedate
echo "s/wlon/$wlon/g"              >> ${work}/changedate
#----------------------------------------------------------
# Make the animation files for Combined
#----------------------------------------------------------
ANIM_SUB="Combined"
FIRST=0
LAST=42
NAMEIT=${STORM}${STORMID}.${yyyymmddhh}.m
echo "s/MC_MODEL/${MC_MODEL}/g"              > ${work}/changeit
echo "s/STORMNAME/${STORM}/g"                >> ${work}/changeit
echo "s/STORMID/${STORMID}/g"                >> ${work}/changeit
echo "s/YYYYMMDDHH/${yyyymmddhh}/g"          >> ${work}/changeit
echo "s/IMAGE_PREFIX/${NAMEIT}/g"            >> ${work}/changeit
echo "s/ANIMATION_SUBTITLE/${ANIM_SUB}/g"    >> ${work}/changeit
echo "s/FIRST_IMAGE/${FIRST}/g"              >> ${work}/changeit
echo "s/LAST_IMAGE/${LAST}/g"                >> ${work}/changeit
echo "s/768)/711)/g"                         >> ${work}/changeit
echo "s/REDIRECT_TITLE/hwrf_c.html/g"        > ${work}/changeit2
echo "s/REDIRECT_URL/hwrf_c.html/g"          >> ${work}/changeit2
#----------------------------------------------------------
# Edit the cgi and html Templates for Combined
#----------------------------------------------------------
sed -f ${work}/changeit ${work}/animation_template.in  > ${work}/ctr_temp.in
cp -f ${work}/ctr_temp.in  ${work}/hwrf_c.html
sed -f ${work}/changeit ${work}/cgi_redirect.in  > ${work}/cgi_temp.in
cp -f ${work}/cgi_temp.in  ${work}/hwrf_c.cgi
#----------------------------------------------------------
# Make the animation files for Nest
#----------------------------------------------------------
ANIM_SUB="Nest"
FIRST=0
LAST=42
NAMEIT=${STORM}${STORMID}.${yyyymmddhh}.n
echo "s/MC_MODEL/${MC_MODEL}/g"              > ${work}/changeit
echo "s/STORMNAME/${STORM}/g"                >> ${work}/changeit
echo "s/STORMID/${STORMID}/g"                >> ${work}/changeit
echo "s/YYYYMMDDHH/${yyyymmddhh}/g"          >> ${work}/changeit
echo "s/IMAGE_PREFIX/${NAMEIT}/g"            >> ${work}/changeit
echo "s/ANIMATION_SUBTITLE/${ANIM_SUB}/g"    >> ${work}/changeit
echo "s/FIRST_IMAGE/${FIRST}/g"              >> ${work}/changeit
echo "s/LAST_IMAGE/${LAST}/g"                >> ${work}/changeit
echo "s/REDIRECT_TITLE/hwrf_n.html/g"        > ${work}/changeit2
echo "s/REDIRECT_URL/hwrf_n.html/g"          >> ${work}/changeit2
#----------------------------------------------------------
# Edit the cgi and html Templates for Nest
#----------------------------------------------------------
sed -f ${work}/changeit ${work}/animation_template.in  > ${work}/nest_temp.in
cp -f ${work}/nest_temp.in  ${work}/hwrf_n.html
#----------------------------------------------------------
sed -f ${work}/changeit ${work}/cgi_redirect.in  > ${work}/cgi_temp.in
cp -f ${work}/cgi_temp.in  ${work}/hwrf_n.cgi
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
cp -f ${work}/${trackfile} ${archive_dir}/
rm -r ${work}
# end of script 
