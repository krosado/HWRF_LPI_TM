#! /bin/ksh 
# This script makes a grads plot of the wind intensity and MSLP 
#
# USAGE:  ./para_plot_intensity.sh STORM stormid yyyymmddhh MC_MODEL \
#           path_to_para_root $bdeck_path $adeck_path $MODEL_REF'
# EXAMPLE: ./para_plot_intensity.sh TWO 02e 2007053012  HWRF \
#           /scratch1/portfolios/NCEPDEV/hwrf/noscrub/Chanh.Kieu/PARA/   \
#           /scratch1/portfolios/NCEPDEV/hwrf/noscrub/Chanh.Kieu/abdeck  \
#           /scratch1/portfolios/NCEPDEV/hwrf/noscrub/Chanh.Kieu/abdeck  \
#           'GFDL AVNO'
#
# Sep 2007  Vijay Tallapragada and William O'Connor 
# Jun 2011  Edited for 2011 Oper. by Janna Durante
# Jan 2012  Edited for 2012 Baseline by Janna Durante
######################################################################
set -x 
GRADDIR="${GRAD_PATH:-/apps/grads/2.0.1/bin}"
COPYGBDIR="${COPYGB_PATH:-/home/Chanh.Kieu/bin}"
#---------------------------------------------------------------------
# get the input variables
#--------------------------------------------------------------------- 
if [ $# -eq 9 ]
then
   STORM=$1
   stormid=$2
   yyyymmddhh=$3
   MC_MODEL=$4
   sorc=$5
   bdeck_path=$6
   adeck_path=$7
   MODEL_REF=$8
   MODEL_LIST="$MC_MODEL $MODEL_REF"
   tmp_dir=$9
   echo 'STORM      = ' ${STORM}
   echo 'stormid    = ' ${stormid}
   echo 'yyyymmddhh = ' ${yyyymmddhh}  
   echo 'MC_MODEL   = ' ${MC_MODEL}
   echo 'sorc       = ' $sorc
   echo 'bdeck_path = ' $bdeck_path
   echo 'adeck_path = ' $adeck_path
else
   echo 'USAGE:'
   echo './para_plot_intensity.sh STORM stormid yyyymmddhh MC_MODEL \ '
   echo 'path_to_para_root $bdeck_path $adeck_path $MODEL_REF'
   echo 'NEED EIGHT ARGUMENTS - SCRIPT WILL EXIT' 
   exit 1 
fi 
#---------------------------------------------------------------------
# create the lower case storm name 
storm=`echo ${STORM} | tr '[A-Z]' '[a-z]'` 

# create the upper case storm number 
STORMID=`echo ${stormid} | tr '[a-z]' '[A-Z]'` 

# create a two digit storm number 
num=`echo ${stormid} | cut -c1-2`

# create the year 
yyyy=`echo ${yyyymmddhh} | cut -c1-4`
mm=`echo ${yyyymmddhh} | cut -c5-6`
dd=`echo ${yyyymmddhh} | cut -c7-8`
hh=`echo ${yyyymmddhh} | cut -c9-10`

echo 'storm      = ' ${storm}
echo 'STORMID    = ' ${STORMID} 
echo 'yyyy       = ' ${yyyy} 
echo 'num        = ' ${num}  

#---------------------------------------------------------------------
# set the pathways 
#---------------------------------------------------------------------
user=`whoami`
ndate="${NDATE_PATH}"
work="${tmp_dir}/tmp/INTENSITY/${STORM}${STORMID}/${yyyymmddhh}/" 
work_tracks="${tmp_dir}/tmp/TRACKS/${STORM}${STORMID}/${yyyymmddhh}/"
tpc_dir="${adeck_path}"
best_dir="${bdeck_path}"
archive_base="${tmp_dir}/figures"
echo 'user        = ' ${user}
echo 'sorc        = ' ${sorc}
echo 'work        = ' ${work}
#---------------------------------------------------------------------
# Determine basin of storm from third letter of stormid 
# (l=ATLANTIC, e=EASTPAC, c=CENTRAL PACIFIC)
#--------------------------------------------------------------------- 
letter=`echo ${stormid} | cut -c3`
echo 'letter = ' ${letter}
#
# set archive directory from basin of storm 
# note that CENTRAL Pacific will go to EASTPAC directory
#
if [ ${letter} = 'l' ]
then
archive_dir="${archive_base}/RT_ATLANTIC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"  
elif [ ${letter} = 'e' ]
then
archive_dir="${archive_base}/RT_EASTPAC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"  
elif [ ${letter} = 'c' ]
then
archive_dir="${archive_base}/RT_CPAC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"  
elif [ ${letter} = 'w' ] || [ ${letter} = 'b' ] || [ ${letter} = 'a' ] || \
     [ ${letter} = 's' ] || [ ${letter} = 'p' ]
then
archive_dir="${archive_base}/RT_WPAC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"
else
   echo "BASIN DESIGNATION LETTER letter = ${letter} NOT LOWER CASE l, e, or c"
   echo 'SCRIPT WILL EXIT'
   exit 1
fi 
echo 'archive_dir = ' ${archive_dir}  
#---------------------------------------------------------------------
# make the working directory; remove all files, and cd to it
#---------------------------------------------------------------------
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
#---------------------------------------------------------------------
# determine the name of the track file 
# create the basin letters
basin_let1=`echo ${stormid} | cut -c3`
echo 'basin_let1 = ' ${basin_let1} 

if [ ${basin_let1} = 'l' ]
then 
   basin_let2='al'
elif [ ${basin_let1} = 'e' ]
then
   basin_let2='ep'
elif [ ${basin_let1} = 'c' ]
then
   basin_let2='cp' 
elif [ ${basin_let1} = 'w' ]
then
   basin_let2='wp'
elif [ ${basin_let1} = 'b' ]
then
   basin_let2='io'
elif [ ${basin_let1} = 'a' ]
then
   basin_let2='aa'
elif [ ${basin_let1} = 's' ]
then
   basin_let2='sh'
elif [ ${basin_let1} = 'p' ]
then
   basin_let2='sh'
else
   echo "BASIN LETTER basin_let1 = ${basin_let1} NOT l, e, OR c" 
   echo 'SCRIPT WILL EXIT'
   exit 1
fi 
echo 'basin_let2 = ' ${basin_let2} 

# the track file name is  
track_file=a${basin_let2}${num}${yyyy}.dat
echo 'track_file = ' ${track_file}  

#the best track file name is
best_file=b${basin_let2}${num}${yyyy}.dat
echo 'best_file = ' ${best_file}

scp ${adeck_path}/${track_file} ${work}
scp ${bdeck_path}/${best_file} ${work}
#---------------------------------------------------------------------
# set remote directory on rzdm from basin of storm 
# note that CENTRAL will go to EASTPAC directory 
#---------------------------------------------------------------------
if [ ${basin_let1} = 'l' ]
then 
 remote_base="/home/others/people/emc/www/htdocs/gc_wmb/vxt/HR12/${yyyy}_ATLANTIC"
elif [ ${basin_let1} = 'e' ]
then 
 remote_base="/home/others/people/emc/www/htdocs/gc_wmb/vxt/HR12/${yyyy}_EASTPAC"
elif [ ${basin_let1} = 'c' ]
then
 remote_base="/home/others/people/emc/www/htdocs/gc_wmb/vxt/HR12/${yyyy}_EASTPAC"
fi
 
remote_dir=${remote_base}/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}  
echo 'remote_dir = ' ${remote_dir}  
scp='/usr/bin/scp'  
#---------------------------------------------------------------------
# extract the track name and date from the track file and
# put in individual files 
#---------------------------------------------------------------------
for TRACK in ${MODEL_LIST} 
do 
     grep ${TRACK} ${work}/${track_file} | grep ${yyyymmddhh} > ${work}/${TRACK}.dat 
done 
cp ${work_tracks}/BEST.dat ${work}/. 
#---------------------------------------------------------------------
# copy the fortran program to the working directory, that
# will reformat the track files 
#---------------------------------------------------------------------
/bin/cp -f ${sorc}/../sorc//hwrf-utilities/exec/grp_hwrf_atcf_intensity.exe  ${work}/hwrf_atcf_intensity.exe 

# see that program compiled 
if [ -s ${work}/hwrf_atcf_intensity.exe ]
then
   echo 'executable hwrf_atcf_intensity.exe present, program compiled' 
else
   echo 'EXECUTABLE hwrf_atcf_intensity.exe SIZE ZERO OR NOT PRESENT'
   echo 'PROGRAM hwrf_atcf_intensity.f DID NOT COMPILE'
   echo 'SCRIPT WILL EXIT'
   exit 1 
fi  
#---------------------------------------------------------------------
# copy the grads scripts to the working directory
#---------------------------------------------------------------------
/bin/cp -f ${sorc}/lib-gs/cbar.gs                        ${work}/  
/bin/cp -f ${sorc}/lib-gs/rgbset.gs                      ${work}/  
if [ "$letter" == "w" ] || [ "$letter" == "b" ] || [ "$letter" == "a" ] || \
   [ "$letter" == "s" ] || [ "$letter" == "p" ]; then
 /bin/cp -f ${sorc}/lib-gs/para_plot_vmax_wp.gs          ${work}/para_plot_vmax.gs
 /bin/cp -f ${sorc}/lib-gs/para_plot_pmin_wp.gs          ${work}/para_plot_pmin.gs
else
 /bin/cp -f ${sorc}/lib-gs/para_plot_vmax_al.gs          ${work}/para_plot_vmax.gs
 /bin/cp -f ${sorc}/lib-gs/para_plot_pmin_al.gs          ${work}/para_plot_pmin.gs
fi
/bin/cp -f ${sorc}/utils/dummy_track.ctl                 ${work}/dummy.ctl  
/bin/cp -f ${sorc}/utils/dummy.dat                       ${work}/  

if [ $mm -eq '01' ]; then MM=JAN;fi
if [ $mm -eq '02' ]; then MM=FEB;fi
if [ $mm -eq '03' ]; then MM=MAR;fi
if [ $mm -eq '04' ]; then MM=APR;fi
if [ $mm -eq '05' ]; then MM=MAY;fi
if [ $mm -eq '06' ]; then MM=JUN;fi
if [ $mm -eq '07' ]; then MM=JUL;fi
if [ $mm -eq '08' ]; then MM=AUG;fi
if [ $mm -eq '09' ]; then MM=SEP;fi
if [ $mm -eq '10' ]; then MM=OCT;fi
if [ $mm -eq '11' ]; then MM=NOV;fi
if [ $mm -eq '12' ]; then MM=DEC;fi

DATE=$yyyymmddhh
IDATE=${hh}z${dd}${MM}${yyyy}
EDATE=`$ndate 126 ${DATE}`
YYYY=`echo ${EDATE} | cut -c1-4`
mm=`echo ${EDATE} | cut -c5-6`
dd=`echo ${EDATE} | cut -c7-8`
hh=`echo ${EDATE} | cut -c9-10`
if [ $mm -eq '01' ]; then MM=JAN;fi
if [ $mm -eq '02' ]; then MM=FEB;fi
if [ $mm -eq '03' ]; then MM=MAR;fi
if [ $mm -eq '04' ]; then MM=APR;fi
if [ $mm -eq '05' ]; then MM=MAY;fi
if [ $mm -eq '06' ]; then MM=JUN;fi
if [ $mm -eq '07' ]; then MM=JUL;fi
if [ $mm -eq '08' ]; then MM=AUG;fi
if [ $mm -eq '09' ]; then MM=SEP;fi
if [ $mm -eq '10' ]; then MM=OCT;fi
if [ $mm -eq '11' ]; then MM=NOV;fi
if [ $mm -eq '12' ]; then MM=DEC;fi
ENDDATE=${hh}z${dd}${MM}${YYYY}
echo ${ENDDATE}
#---------------------------------------------------------------------
# create a file of commands to be used by the stream editor
# to change the grads script 
#---------------------------------------------------------------------
echo "s/STID/${STORMID}/g"           >  ${work}/change_dims
echo "s/STORMNAME/${STORM}/g"       >>  ${work}/change_dims
echo "s/YYYYMMDDHH/${yyyymmddhh}/g" >>  ${work}/change_dims
echo "s/IDATE/${IDATE}/g"           >>  ${work}/change_dims
echo "s/ENDDATE/${ENDDATE}/g"       >>  ${work}/change_dims
echo "s/MC_MODEL/${MC_MODEL}/g"     >>  ${work}/change_dims
echo "s/DESC/2011_PARA/g"           >>  ${work}/change_dims
echo "s/slat/${slat}/g"             >>  ${work}/change_dims
echo "s/nlat/${nlat}/g"             >>  ${work}/change_dims
echo "s/wlon/${wlon}/g"             >>  ${work}/change_dims
echo "s/elon/${elon}/g"             >>  ${work}/change_dims
#---------------------------------------------------------------------
# apply these changes to grads script with stream editor
sed -f ${work}/change_dims ${work}/para_plot_vmax.gs > \
                           ${work}/plot_vmax.gs  

sed -f ${work}/change_dims ${work}/para_plot_pmin.gs > \
			   ${work}/plot_pmin.gs
#---------------------------------------------------------------------
#   FOR THE WIND INTENSITY FILE
#---------------------------------------------------------------------
# run executable to reformat the files ${TRACK}.dat 
#---------------------------------------------------------------------
for TRACK in ${MODEL_LIST} BEST 
do 
/bin/rm -f ${work}/hwrf.atcfunix  
/bin/rm -f ${work}/hwrf.stats 
/bin/rm -f ${work}/header 
/bin/cp -f ${work}/${TRACK}.dat ${work}/hwrf.atcfunix 
${work}/hwrf_atcf_intensity.exe 1> ${work}/${TRACK}.stats.out 2> ${work}/${TRACK}.stats.err
#---------------------------------------------------------------------
# add the storm name as a first line header to the file hwrf.stats 
# and rename the file  
#---------------------------------------------------------------------
echo ${TRACK} > ${work}/header 
cat ${work}/header ${work}/hwrf.stats > ${work}/${TRACK}.track  
cat ${work}/hwrf.stats >> ${work}/all.tracks 
sed -e "s/H=/ /g" ${work}/all.tracks >${work}/all.tracks1
mv  -f ${work}/all.tracks1 ${work}/all.tracks
#---------------------------------------------------------------------
h0=$yyyymmddhh
YYYY=`echo ${h0} | cut -c1-4`
mm=`echo ${h0} | cut -c5-6`
dd=`echo ${h0} | cut -c7-8`
hh=`echo ${h0} | cut -c9-10`
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
v0=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h03=`$ndate 03 $h0`
YYYY=`echo ${h03} | cut -c1-4`
mm=`echo ${h03} | cut -c5-6`
dd=`echo ${h03} | cut -c7-8`
hh=`echo ${h03} | cut -c9-10`
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
v03=${hh}z${dd}${MM}${YYYY}
echo 'h03, v03', $h03 $v03
#---------------------------------------------------------------------
h1=`$ndate 06 $h0`
YYYY=`echo ${h1} | cut -c1-4`
mm=`echo ${h1} | cut -c5-6`
dd=`echo ${h1} | cut -c7-8`
hh=`echo ${h1} | cut -c9-10`
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
v1=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h2=`$ndate 06 $h1`
YYYY=`echo ${h2} | cut -c1-4`
mm=`echo ${h2} | cut -c5-6`
dd=`echo ${h2} | cut -c7-8`
hh=`echo ${h2} | cut -c9-10`
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
v2=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h3=`$ndate 06 $h2`
YYYY=`echo ${h3} | cut -c1-4`
mm=`echo ${h3} | cut -c5-6`
dd=`echo ${h3} | cut -c7-8`
hh=`echo ${h3} | cut -c9-10`
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
v3=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h4=`$ndate 06 $h3`
YYYY=`echo ${h4} | cut -c1-4`
mm=`echo ${h4} | cut -c5-6`
dd=`echo ${h4} | cut -c7-8`
hh=`echo ${h4} | cut -c9-10`
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
v4=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h5=`$ndate 06 $h4`
YYYY=`echo ${h5} | cut -c1-4`
mm=`echo ${h5} | cut -c5-6`
dd=`echo ${h5} | cut -c7-8`
hh=`echo ${h5} | cut -c9-10`
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
v5=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h6=`$ndate 06 $h5`
YYYY=`echo ${h6} | cut -c1-4`
mm=`echo ${h6} | cut -c5-6`
dd=`echo ${h6} | cut -c7-8`
hh=`echo ${h6} | cut -c9-10`
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
v6=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h7=`$ndate 06 $h6`
YYYY=`echo ${h7} | cut -c1-4`
mm=`echo ${h7} | cut -c5-6`
dd=`echo ${h7} | cut -c7-8`
hh=`echo ${h7} | cut -c9-10`
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
v7=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h8=`$ndate 06 $h7`
YYYY=`echo ${h8} | cut -c1-4`
mm=`echo ${h8} | cut -c5-6`
dd=`echo ${h8} | cut -c7-8`
hh=`echo ${h8} | cut -c9-10`
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
v8=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h9=`$ndate 06 $h8`
YYYY=`echo ${h9} | cut -c1-4`
mm=`echo ${h9} | cut -c5-6`
dd=`echo ${h9} | cut -c7-8`
hh=`echo ${h9} | cut -c9-10`
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
v9=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h10=`$ndate 06 $h9`
YYYY=`echo ${h10} | cut -c1-4`
mm=`echo ${h10} | cut -c5-6`
dd=`echo ${h10} | cut -c7-8`
hh=`echo ${h10} | cut -c9-10`
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
v10=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h11=`$ndate 06 $h10`
YYYY=`echo ${h11} | cut -c1-4`
mm=`echo ${h11} | cut -c5-6`
dd=`echo ${h11} | cut -c7-8`
hh=`echo ${h11} | cut -c9-10`
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
v11=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h12=`$ndate 06 $h11`
YYYY=`echo ${h12} | cut -c1-4`
mm=`echo ${h12} | cut -c5-6`
dd=`echo ${h12} | cut -c7-8`
hh=`echo ${h12} | cut -c9-10`
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
v12=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h13=`$ndate 06 $h12`
YYYY=`echo ${h13} | cut -c1-4`
mm=`echo ${h13} | cut -c5-6`
dd=`echo ${h13} | cut -c7-8`
hh=`echo ${h13} | cut -c9-10`
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
v13=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h14=`$ndate 06 $h13`
YYYY=`echo ${h14} | cut -c1-4`
mm=`echo ${h14} | cut -c5-6`
dd=`echo ${h14} | cut -c7-8`
hh=`echo ${h14} | cut -c9-10`
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
v14=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h15=`$ndate 06 $h14`
YYYY=`echo ${h15} | cut -c1-4`
mm=`echo ${h15} | cut -c5-6`
dd=`echo ${h15} | cut -c7-8`
hh=`echo ${h15} | cut -c9-10`
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
v15=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h16=`$ndate 06 $h15`
YYYY=`echo ${h16} | cut -c1-4`
mm=`echo ${h16} | cut -c5-6`
dd=`echo ${h16} | cut -c7-8`
hh=`echo ${h16} | cut -c9-10`
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
v16=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h17=`$ndate 06 $h16`
YYYY=`echo ${h17} | cut -c1-4`
mm=`echo ${h17} | cut -c5-6`
dd=`echo ${h17} | cut -c7-8`
hh=`echo ${h17} | cut -c9-10`
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
v17=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h18=`$ndate 06 $h17`
YYYY=`echo ${h18} | cut -c1-4`
mm=`echo ${h18} | cut -c5-6`
dd=`echo ${h18} | cut -c7-8`
hh=`echo ${h18} | cut -c9-10`
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
v18=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h19=`$ndate 06 $h18`
YYYY=`echo ${h19} | cut -c1-4`
mm=`echo ${h19} | cut -c5-6`
dd=`echo ${h19} | cut -c7-8`
hh=`echo ${h19} | cut -c9-10`
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
v19=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h20=`$ndate 06 $h19`
YYYY=`echo ${h20} | cut -c1-4`
mm=`echo ${h20} | cut -c5-6`
dd=`echo ${h20} | cut -c7-8`
hh=`echo ${h20} | cut -c9-10`
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
v20=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h21=`$ndate 06 $h20`
YYYY=`echo ${h21} | cut -c1-4`
mm=`echo ${h21} | cut -c5-6`
dd=`echo ${h21} | cut -c7-8`
hh=`echo ${h21} | cut -c9-10`
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
v21=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
echo "s/H=   0.0/${v0}/"      >  ${work}/change_hours
echo "s/H=   3.0/${v03}/"     >>  ${work}/change_hours
echo "s/H=   6.0/${v1}/"      >>  ${work}/change_hours
echo "s/H=  12.0/${v2}/"      >>  ${work}/change_hours
echo "s/H=  18.0/${v3}/"      >>  ${work}/change_hours
echo "s/H=  24.0/${v4}/"      >>  ${work}/change_hours
echo "s/H=  30.0/${v5}/"      >>  ${work}/change_hours
echo "s/H=  36.0/${v6}/"      >>  ${work}/change_hours
echo "s/H=  42.0/${v7}/"      >>  ${work}/change_hours
echo "s/H=  48.0/${v8}/"      >>  ${work}/change_hours
echo "s/H=  54.0/${v9}/"      >>  ${work}/change_hours
echo "s/H=  60.0/${v10}/"     >>  ${work}/change_hours
echo "s/H=  66.0/${v11}/"     >>  ${work}/change_hours
echo "s/H=  72.0/${v12}/"     >>  ${work}/change_hours
echo "s/H=  78.0/${v13}/"     >>  ${work}/change_hours
echo "s/H=  84.0/${v14}/"     >>  ${work}/change_hours
echo "s/H=  90.0/${v15}/"     >>  ${work}/change_hours
echo "s/H=  96.0/${v16}/"     >>  ${work}/change_hours
echo "s/H= 102.0/${v17}/"     >>  ${work}/change_hours
echo "s/H= 108.0/${v18}/"     >>  ${work}/change_hours
echo "s/H= 114.0/${v19}/"     >>  ${work}/change_hours
echo "s/H= 120.0/${v20}/"     >>  ${work}/change_hours
echo "s/H= 126.0/${v21}/"     >>  ${work}/change_hours
sed -f ${work}/change_hours ${work}/${TRACK}.track > ${work}/${TRACK}.intensity
sed -i '/H=/d' ${work}/${TRACK}.intensity
done 
#---------------------------------------------------------------------
#   FOR THE PMIN INTENSITY FILE
#---------------------------------------------------------------------
# run executable to reformat the files ${TRACK}.dat 
#---------------------------------------------------------------------
for TRACK in ${MC_MODEL} BEST 
do 
/bin/rm -f ${work}/hwrf.atcfunix  
/bin/rm -f ${work}/hwrf.stats 
/bin/rm -f ${work}/header 
/bin/cp -f ${work}/${TRACK}.dat ${work}/hwrf.atcfunix 
${work}/hwrf_atcf_intensity.exe > ${work}/${TRACK}.stats.out 2> ${work}/${TRACK}.stats.err
# add the storm name as a first line header to the file hwrf.stats 
# and rename the file  
echo ${TRACK} > ${work}/header 
cat ${work}/header ${work}/hwrf.stats > ${work}/${TRACK}.track  
cat ${work}/hwrf.stats >> ${work}/all_pmin.tracks 
sed -e "s/H=/ /g" ${work}/all_pmin.tracks >${work}/all_pmin.tracks1
mv  -f ${work}/all_pmin.tracks1 ${work}/all_pmin.tracks
#---------------------------------------------------------------------
h0=$yyyymmddhh
YYYY=`echo ${h0} | cut -c1-4`
mm=`echo ${h0} | cut -c5-6`
dd=`echo ${h0} | cut -c7-8`
hh=`echo ${h0} | cut -c9-10`
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
v0=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h03=`$ndate 03 $h0`
YYYY=`echo ${h03} | cut -c1-4`
mm=`echo ${h03} | cut -c5-6`
dd=`echo ${h03} | cut -c7-8`
hh=`echo ${h03} | cut -c9-10`
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
v03=${hh}z${dd}${MM}${YYYY}
echo 'h03, v03', $h03 $v03
#---------------------------------------------------------------------
h1=`$ndate 06 $h0`
YYYY=`echo ${h1} | cut -c1-4`
mm=`echo ${h1} | cut -c5-6`
dd=`echo ${h1} | cut -c7-8`
hh=`echo ${h1} | cut -c9-10`
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
v1=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h2=`$ndate 06 $h1`
YYYY=`echo ${h2} | cut -c1-4`
mm=`echo ${h2} | cut -c5-6`
dd=`echo ${h2} | cut -c7-8`
hh=`echo ${h2} | cut -c9-10`
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
v2=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h3=`$ndate 06 $h2`
YYYY=`echo ${h3} | cut -c1-4`
mm=`echo ${h3} | cut -c5-6`
dd=`echo ${h3} | cut -c7-8`
hh=`echo ${h3} | cut -c9-10`
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
v3=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h4=`$ndate 06 $h3`
YYYY=`echo ${h4} | cut -c1-4`
mm=`echo ${h4} | cut -c5-6`
dd=`echo ${h4} | cut -c7-8`
hh=`echo ${h4} | cut -c9-10`
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
v4=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h5=`$ndate 06 $h4`
YYYY=`echo ${h5} | cut -c1-4`
mm=`echo ${h5} | cut -c5-6`
dd=`echo ${h5} | cut -c7-8`
hh=`echo ${h5} | cut -c9-10`
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
v5=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h6=`$ndate 06 $h5`
YYYY=`echo ${h6} | cut -c1-4`
mm=`echo ${h6} | cut -c5-6`
dd=`echo ${h6} | cut -c7-8`
hh=`echo ${h6} | cut -c9-10`
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
v6=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h7=`$ndate 06 $h6`
YYYY=`echo ${h7} | cut -c1-4`
mm=`echo ${h7} | cut -c5-6`
dd=`echo ${h7} | cut -c7-8`
hh=`echo ${h7} | cut -c9-10`
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
v7=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h8=`$ndate 06 $h7`
YYYY=`echo ${h8} | cut -c1-4`
mm=`echo ${h8} | cut -c5-6`
dd=`echo ${h8} | cut -c7-8`
hh=`echo ${h8} | cut -c9-10`
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
v8=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h9=`$ndate 06 $h8`
YYYY=`echo ${h9} | cut -c1-4`
mm=`echo ${h9} | cut -c5-6`
dd=`echo ${h9} | cut -c7-8`
hh=`echo ${h9} | cut -c9-10`
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
v9=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h10=`$ndate 06 $h9`
YYYY=`echo ${h10} | cut -c1-4`
mm=`echo ${h10} | cut -c5-6`
dd=`echo ${h10} | cut -c7-8`
hh=`echo ${h10} | cut -c9-10`
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
v10=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h11=`$ndate 06 $h10`
YYYY=`echo ${h11} | cut -c1-4`
mm=`echo ${h11} | cut -c5-6`
dd=`echo ${h11} | cut -c7-8`
hh=`echo ${h11} | cut -c9-10`
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
v11=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h12=`$ndate 06 $h11`
YYYY=`echo ${h12} | cut -c1-4`
mm=`echo ${h12} | cut -c5-6`
dd=`echo ${h12} | cut -c7-8`
hh=`echo ${h12} | cut -c9-10`
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
v12=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h13=`$ndate 06 $h12`
YYYY=`echo ${h13} | cut -c1-4`
mm=`echo ${h13} | cut -c5-6`
dd=`echo ${h13} | cut -c7-8`
hh=`echo ${h13} | cut -c9-10`
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
v13=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h14=`$ndate 06 $h13`
YYYY=`echo ${h14} | cut -c1-4`
mm=`echo ${h14} | cut -c5-6`
dd=`echo ${h14} | cut -c7-8`
hh=`echo ${h14} | cut -c9-10`
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
v14=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h15=`$ndate 06 $h14`
YYYY=`echo ${h15} | cut -c1-4`
mm=`echo ${h15} | cut -c5-6`
dd=`echo ${h15} | cut -c7-8`
hh=`echo ${h15} | cut -c9-10`
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
v15=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h16=`$ndate 06 $h15`
YYYY=`echo ${h16} | cut -c1-4`
mm=`echo ${h16} | cut -c5-6`
dd=`echo ${h16} | cut -c7-8`
hh=`echo ${h16} | cut -c9-10`
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
v16=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h17=`$ndate 06 $h16`
YYYY=`echo ${h17} | cut -c1-4`
mm=`echo ${h17} | cut -c5-6`
dd=`echo ${h17} | cut -c7-8`
hh=`echo ${h17} | cut -c9-10`
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
v17=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h18=`$ndate 06 $h17`
YYYY=`echo ${h18} | cut -c1-4`
mm=`echo ${h18} | cut -c5-6`
dd=`echo ${h18} | cut -c7-8`
hh=`echo ${h18} | cut -c9-10`
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
v18=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h19=`$ndate 06 $h18`
YYYY=`echo ${h19} | cut -c1-4`
mm=`echo ${h19} | cut -c5-6`
dd=`echo ${h19} | cut -c7-8`
hh=`echo ${h19} | cut -c9-10`
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
v19=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h20=`$ndate 06 $h19`
YYYY=`echo ${h20} | cut -c1-4`
mm=`echo ${h20} | cut -c5-6`
dd=`echo ${h20} | cut -c7-8`
hh=`echo ${h20} | cut -c9-10`
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
v20=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
h21=`$ndate 06 $h20`
YYYY=`echo ${h21} | cut -c1-4`
mm=`echo ${h21} | cut -c5-6`
dd=`echo ${h21} | cut -c7-8`
hh=`echo ${h21} | cut -c9-10`
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
v21=${hh}z${dd}${MM}${YYYY}
#---------------------------------------------------------------------
echo "s/H=   0.0/${v0}/"      >  ${work}/change_hours
echo "s/H=   3.0/${v03}/"     >>  ${work}/change_hours
echo "s/H=   6.0/${v1}/"      >>  ${work}/change_hours
echo "s/H=  12.0/${v2}/"      >>  ${work}/change_hours
echo "s/H=  18.0/${v3}/"      >>  ${work}/change_hours
echo "s/H=  24.0/${v4}/"      >>  ${work}/change_hours
echo "s/H=  30.0/${v5}/"      >>  ${work}/change_hours
echo "s/H=  36.0/${v6}/"      >>  ${work}/change_hours
echo "s/H=  42.0/${v7}/"      >>  ${work}/change_hours
echo "s/H=  48.0/${v8}/"      >>  ${work}/change_hours
echo "s/H=  54.0/${v9}/"      >>  ${work}/change_hours
echo "s/H=  60.0/${v10}/"     >>  ${work}/change_hours
echo "s/H=  66.0/${v11}/"     >>  ${work}/change_hours
echo "s/H=  72.0/${v12}/"     >>  ${work}/change_hours
echo "s/H=  78.0/${v13}/"     >>  ${work}/change_hours
echo "s/H=  84.0/${v14}/"     >>  ${work}/change_hours
echo "s/H=  90.0/${v15}/"     >>  ${work}/change_hours
echo "s/H=  96.0/${v16}/"     >>  ${work}/change_hours
echo "s/H= 102.0/${v17}/"     >>  ${work}/change_hours
echo "s/H= 108.0/${v18}/"     >>  ${work}/change_hours
echo "s/H= 114.0/${v19}/"     >>  ${work}/change_hours
echo "s/H= 120.0/${v20}/"     >>  ${work}/change_hours
echo "s/H= 126.0/${v21}/"     >>  ${work}/change_hours
sed -f ${work}/change_hours ${work}/${TRACK}.track > \
                           ${work}/${TRACK}.intensity
sed -i '/H=/d' ${work}/${TRACK}.intensity
done 
#---------------------------------------------------------------------
# run the grads script 
#---------------------------------------------------------------------
grads -blc "plot_vmax.gs" 
grads -blc "plot_pmin.gs"
#---------------------------------------------------------------------
# check to see if file 'Vmax.png' is created; else exit
if [ -s ${work}/Vmax.png ] 
then
   echo "${work}/Vmax.png file created" 
else
   echo "FILE ${work}/Vmax.png NOT CREATED"
   echo 'SCRIPT WILL EXIT'
   exit 1 
fi 
#---------------------------------------------------------------------
# check to see if file 'Pmin.png' is created; else exit
if [ -s ${work}/Pmin.png ]
then
   echo "${work}/Pmin.png file created"
else
    echo "FILE ${work}/Pmin.png NOT CREATED"
    echo 'SCRIPT WILL EXIT'
    exit 1
    fi
#---------------------------------------------------------------------
# copy the Vmax.png file to the archive directory to save 
if [ -d ${archive_dir} ]
then
   echo "${archive_dir} exists"
else
   mkdir -p ${archive_dir} 
fi 
cp -f ${work}/Vmax.png ${archive_dir}/${STORM}${STORMID}.${yyyymmddhh}.Vmax.png 
#---------------------------------------------------------------------
# copy the Pmin.png file to the archive directory to save
if [ -d ${archive_dir} ]
then
   echo "${archive_dir} exists"
else
   mkdir -p ${archive_dir}
fi
cp -f ${work}/Pmin.png ${archive_dir}/${STORM}${STORMID}.${yyyymmddhh}.Pmin.png
#
# do a quick merge of track/intensity for fast deliver of figures
#
cd ${archive_dir}/
rm -f ${STORM}${STORMID}.${yyyymmddhh}.fsct.png
montage -geometry 540x540 -mode concatenate -tile 1x2 ${STORM}${STORMID}.${yyyymmddhh}.Vmax.png         \
         ${STORM}${STORMID}.${yyyymmddhh}.Pmin.png temp1.png
montage -mode concatenate -tile 2x1 ${STORM}${STORMID}.${yyyymmddhh}.track.png temp1.png                \
         ${STORM}${STORMID}.${yyyymmddhh}.fsct.png
rm -f temp1.png ${STORM}${STORMID}.${yyyymmddhh}.Vmax.png ${STORM}${STORMID}.${yyyymmddhh}.Pmin.png 
rm -f ${STORM}${STORMID}.${yyyymmddhh}.track.png
exit
