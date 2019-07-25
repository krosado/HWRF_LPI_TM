#! /bin/ksh 
# Makes track plots  
#
# USAGE:  ./para_plot_track.sh STORM stormid yyyymmddhh MC_MODEL \
#           path_to_para_root $bdeck_path $adeck_path $MODEL_REF'
# EXAMPLE: ./para_plot_track.sh TWO 02e 2007053012  HWRF \
#           /scratch1/portfolios/NCEPDEV/hwrf/noscrub/Chanh.Kieu/PARA/   \
#           /scratch1/portfolios/NCEPDEV/hwrf/noscrub/Chanh.Kieu/abdeck  \
#           /scratch1/portfolios/NCEPDEV/hwrf/noscrub/Chanh.Kieu/abdeck  \
#           'GFDL AVNO'
#
# Sep 2007  William O'Connor and Vijay Tallapragada   
# Jun 2011  Edited for 2011 Operations by Janna Durante
#########################################################################
set -x 
GRADDIR="${GRAD_PATH:-/apps/grads/2.0.1/bin}"
COPYGBDIR="${COPYGB_PATH:-/home/Chanh.Kieu/bin}"
NDATEDIR="${NDATE_PATH:-/home/Chanh.Kieu/bin}"
#------------------------------------------------------------------------
# get the input variables 
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
   echo './para_plot_track.sh STORM stormid yyyymmddhh MC_MODEL \ '
   echo 'path_to_para_root $bdeck_path $adeck_path $MODEL_REF'
   echo 'NEED EIGHT ARGUMENTS - SCRIPT WILL EXIT'
   exit 1 
fi 
#-------------------------------------------------------------------------
# create the lower case storm name 
storm=`echo ${STORM} | tr '[A-Z]' '[a-z]'` 

# create the upper case storm number 
STORMID=`echo ${stormid} | tr '[a-z]' '[A-Z]'` 

# create a two digit storm number 
num=`echo ${stormid} | cut -c1-2`

# create the year 
yyyy=`echo ${yyyymmddhh} | cut -c1-4`
YYYY=`echo ${yyyy} | tr '[a-z]' '[A-Z]'`
YYYYMMDDHH=`echo ${yyyymmddhh} | tr '[a-z]' '[A-Z]'`

# create the hour
hh=`echo ${yyyymmddhh} | cut -c9-10`

# create the yyyymmdd
yyyymmdd=`echo ${yyyymmddhh} | cut -c1-8`

echo 'storm      = ' ${storm}
echo 'STORMID    = ' ${STORMID} 
echo 'yyyy       = ' ${yyyy} 
echo 'num        = ' ${num} 
echo 'hh         = ' ${hh}
echo 'yyyymmdd   = ' ${yyyymmdd}

#--------------------------------------------------------------------------
# set the pathways 
work=${tmp_dir}/tmp/TRACKS/${STORM}${STORMID}/${yyyymmddhh}/
user=`whoami`
tpc_dir=${adeck_path}
archive_base="${tmp_dir}/figures"
echo 'user        = ' ${user}
echo 'sorc        = ' ${sorc}
echo 'work        = ' ${work}
#--------------------------------------------------------------------------
# Determine basin of storm from third letter of stormid 
# (l=ATLANTIC, e=EASTPAC, c=CENTRAL PACIFIC) 
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
#--------------------------------------------------------------------------
# make the working directory; remove all files, and cd to it
#--------------------------------------------------------------------------
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
#--------------------------------------------------------------------------
# determine the name of the track file 
# create the basin letters
#--------------------------------------------------------------------------
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
   echo 'basin letter not l, e, or c'
   echo 'SCRIPT WILL EXIT'
   exit 1
fi 
echo 'basin_let2 = ' ${basin_let2} 

BASIN_LET1=`echo ${basin_let1} | tr '[a-z]' '[A-Z]'`
BASIN_LET2=`echo ${basin_let2} | tr '[a-z]' '[A-Z]'`

# the track file name is  
track_file=a${basin_let2}${num}${yyyy}.dat
echo 'track_file = ' ${track_file}  
#---------------------------------------------------------------------------
# set remote directory on rzdm from basin of storm 
# note that the CENTRAL will go to EASTPAC directory
#--------------------------------------------------------------------------- 
if [ ${basin_let1} = 'l' ]
then 
 remote_base='/home/others/people/emc/www/htdocs/gc_wmb/vxt/${MC_MODEL}'
elif [ ${basin_let1} = 'e' ]
then 
 remote_base='/home/others/people/emc/www/htdocs/gc_wmb/vxt/${MC_MODEL}'
elif [ ${basin_let1} = 'c' ]
then
 remote_base='/home/others/people/emc/www/htdocs/gc_wmb/vxt/${MC_MODEL}'
fi
remote_dir=${remote_base}/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}  
echo 'remote_dir = ' ${remote_dir}  
scp='/usr/bin/scp'  
#--------------------------------------------------------------------------
# see if track file is present and copy to working 
# directory; else exit    
#-------------------------------------------------------------------------- 
if [ -s ${adeck_path}/${track_file} ]
then 
   /bin/cp -f ${adeck_path}/${track_file}  ${work}/  
else
   echo "FILE ${adeck_path}/${track_file} SIZE ZERO OR NOT PRESENT"
   echo 'SCRIPT WILL EXIT'
   exit 1
fi 
#
# check if MC_MODEL exists in the adeck file here first. If not, then stop
# right away as it will cause weird track/intensity plots
#
check_mc_model=`grep ${MC_MODEL}, ${work}/${track_file} | grep ${yyyymmddhh}`
if [ "$check_mc_model" == "" ]; then
 echo "there is no $MC_MODEL data in the ${adeck_path}/${track_file} file...exit 1"
 exit 1
fi
#--------------------------------------------------------------------------
# extract the track name and date from the track file and
# put in individual files
#------------------------------------------------------------------------- 
for TRACK in ${MODEL_LIST}
do 
grep ${TRACK}, ${work}/${track_file} | grep ${yyyymmddhh} > ${work}/${TRACK}.dat 
done 
#hwrf_file=${storm}${stormid}.${yyyymmddhh}.trak.hwrf.atcfunix
#/bin/cp -f /lfs2/projects/hwrf-vd/hurrun/ytmp/hurrun/hwrf/com/${yyyymmddhh}/$stormid/${hwrf_file} ${work}/HWRF.dat
#-------------------------------------------------------------------------
#Manipulate the best track values to resemble the forecasts
#taken from the a-deck file
#-------------------------------------------------------------------------
ndate="${NDATEDIR}/ndate"
best_file="b${basin_let2}${num}${yyyy}.dat"
echo ${best_file}
scp ${bdeck_path}/${best_file}  ${work}/.
echo $ndate
count=0
count1=1
while [[ $count -le 126 && $count1 -le 22 ]]
do
  date=`$ndate $count $yyyymmddhh`
  grep ${date} ${work}/${best_file} >  ${work}/BEST${count1}.dat
  count=`expr $count + 6`
  count1=`expr $count1 + 1`
  echo $count
  echo $count1
done
#-------------------------------------------------------------------------
cd ${work}
  sed -e "s/,   ,/, 01,/" BEST1.dat > BEST1.out
  sed -e "s/BEST,   0,/BEST,   0,/" BEST1.out > BEST1.dat
  sed -e "s/,   ,/, 01,/" BEST2.dat > BEST2.out
  sed -e "s/BEST,   0,/BEST,   6,/" BEST2.out > BEST2.dat
  sed -e "s/,   ,/, 01,/" BEST3.dat > BEST3.out
  sed -e "s/BEST,   0,/BEST,  12,/" BEST3.out > BEST3.dat
  sed -e "s/,   ,/, 01,/" BEST4.dat > BEST4.out
  sed -e "s/BEST,   0,/BEST,  18,/" BEST4.out > BEST4.dat
  sed -e "s/,   ,/, 01,/" BEST5.dat > BEST5.out
  sed -e "s/BEST,   0,/BEST,  24,/" BEST5.out > BEST5.dat
  sed -e "s/,   ,/, 01,/" BEST6.dat > BEST6.out
  sed -e "s/BEST,   0,/BEST,  30,/" BEST6.out > BEST6.dat
  sed -e "s/,   ,/, 01,/" BEST7.dat > BEST7.out
  sed -e "s/BEST,   0,/BEST,  36,/" BEST7.out > BEST7.dat
  sed -e "s/,   ,/, 01,/" BEST8.dat > BEST8.out
  sed -e "s/BEST,   0,/BEST,  42,/" BEST8.out > BEST8.dat
  sed -e "s/,   ,/, 01,/" BEST9.dat > BEST9.out
  sed -e "s/BEST,   0,/BEST,  48,/" BEST9.out > BEST9.dat
  sed -e "s/,   ,/, 01,/" BEST10.dat > BEST10.out
  sed -e "s/BEST,   0,/BEST,  54,/" BEST10.out > BEST10.dat
  sed -e "s/,   ,/, 01,/" BEST11.dat > BEST11.out
  sed -e "s/BEST,   0,/BEST,  60,/" BEST11.out > BEST11.dat
  sed -e "s/,   ,/, 01,/" BEST12.dat > BEST12.out
  sed -e "s/BEST,   0,/BEST,  66,/" BEST12.out > BEST12.dat
  sed -e "s/,   ,/, 01,/" BEST13.dat > BEST13.out
  sed -e "s/BEST,   0,/BEST,  72,/" BEST13.out > BEST13.dat
  sed -e "s/,   ,/, 01,/" BEST14.dat > BEST14.out
  sed -e "s/BEST,   0,/BEST,  78,/" BEST14.out > BEST14.dat
  sed -e "s/,   ,/, 01,/" BEST15.dat > BEST15.out
  sed -e "s/BEST,   0,/BEST,  84,/" BEST15.out > BEST15.dat
  sed -e "s/,   ,/, 01,/" BEST16.dat > BEST16.out
  sed -e "s/BEST,   0,/BEST,  90,/" BEST16.out > BEST16.dat
  sed -e "s/,   ,/, 01,/" BEST17.dat > BEST17.out
  sed -e "s/BEST,   0,/BEST,  96,/" BEST17.out > BEST17.dat
  sed -e "s/,   ,/, 01,/" BEST18.dat > BEST18.out
  sed -e "s/BEST,   0,/BEST, 102,/" BEST18.out > BEST18.dat
  sed -e "s/,   ,/, 01,/" BEST19.dat > BEST19.out
  sed -e "s/BEST,   0,/BEST, 108,/" BEST19.out > BEST19.dat
  sed -e "s/,   ,/, 01,/" BEST20.dat > BEST20.out
  sed -e "s/BEST,   0,/BEST, 114,/" BEST20.out > BEST20.dat
  sed -e "s/,   ,/, 01,/" BEST21.dat > BEST21.out
  sed -e "s/BEST,   0,/BEST, 120,/" BEST21.out > BEST21.dat
  sed -e "s/,   ,/, 01,/" BEST22.dat > BEST22.out
  sed -e "s/BEST,   0,/BEST, 126,/" BEST22.out > BEST22.dat
#--------------------------------------------------------------------------
cat BEST1.dat BEST2.dat BEST3.dat BEST4.dat BEST5.dat BEST6.dat BEST7.dat > ${work}/BEST.dat
cat BEST8.dat BEST9.dat BEST10.dat BEST11.dat BEST12.dat BEST13.dat BEST14.dat >> ${work}/BEST.dat
cat BEST15.dat BEST16.dat BEST17.dat BEST18.dat BEST19.dat BEST20.dat BEST21.dat >> ${work}/BEST.dat
cat BEST22.dat >> BEST.dat
#--------------------------------------------------------------------------
# copy the fortran program to the working directory, that
# will reformat the track files 
#--------------------------------------------------------------------------
/bin/cp -f ${sorc}/../sorc//hwrf-utilities/exec/grp_hwrf_atcf_tracks.exe ${work}/hwrf_atcf_tracks.exe  

# see that program compiled 
if [ -s ${work}/hwrf_atcf_tracks.exe ]
then
   echo 'executable hwrf_atcf_tracks.exe present, program compiled' 
else
   echo 'EXECUTABLE hwrf_atcf_tracks.exe SIZE ZERO OR NOT PRESENT'
   echo 'PROGRAM hwrf_atcf_tracks.f DID NOT COMPILE'
   echo 'SCRIPT WILL EXIT'
   exit 1 
fi  
#--------------------------------------------------------------------------
# The fortran program/executable hwrf_atcf_tracks.exe 
# reformats a track file. The input file is always 
# named hwrf.atcfunix and the output file is always 
# named hwrf.stats. 
#--------------------------------------------------------------------------
# run executable to reformat the files ${TRACK}.dat 
for TRACK in ${MODEL_LIST} BEST 
do 
 /bin/rm -f ${work}/hwrf.atcfunix  
 /bin/rm -f ${work}/hwrf.stats 
 /bin/rm -f ${work}/header 
# cat ${work}/${TRACK}.dat | awk '{if($1=="WP,") print $0}' | sed 's/W,/E,/g' > ${work}/hwrf.atcfunix
 /bin/cp -f ${work}/${TRACK}.dat ${work}/hwrf.atcfunix 
 ${work}/hwrf_atcf_tracks.exe 
# add the storm name as a first line header to the file hwrf.stats 
# and rename the file  
 echo ${TRACK} > ${work}/header 
 cat ${work}/header ${work}/hwrf.stats > ${work}/${TRACK}.track  
done 
#--------------------------------------------------------------------------
# copy fortran program to find boundaries of domain for plotting
#--------------------------------------------------------------------------
/bin/cp -f ${sorc}/../sorc//hwrf-utilities/exec/grp_statsin_domain_TI.exe ${work}/statsin_domain_TI.exe
cp -f ${work}/${MC_MODEL}.track ${work}/statsin
${work}/statsin_domain_TI.exe

if [ -s ${work}/bndry_pts.txt ]
then
echo 'file bndry_pts.txt created'
else
echo 'FILE bndry_pts.txt SIZE ZERO OR NOT CREATED'
echo 'SCRIPT WILL EXIT'
exit 1
fi
#--------------------------------------------------------------------------
# get the lon/lat boundary values from file bndry_pts.txt
#--------------------------------------------------------------------------
slat=`cat ${work}/bndry_pts.txt | awk '{ print ($1) }'`
nlat=`cat ${work}/bndry_pts.txt | awk '{ print ($2) }'`
wlon=`cat ${work}/bndry_pts.txt | awk '{ print ($3) }'`
elon=`cat ${work}/bndry_pts.txt | awk '{ print ($4) }'`
check_lon=`echo $elon | awk '{if($1<180) print 1; else print 0}'`
echo "wlon = $wlon; elon = $elon"
#--------------------------------------------------------------------------
# copy the grads scripts to the working directory
#--------------------------------------------------------------------------
/bin/cp -f ${sorc}/lib-gs/cbar.gs                 ${work}/  
/bin/cp -f ${sorc}/lib-gs/rgbset.gs               ${work}/  
/bin/cp -f ${sorc}/lib-gs/track.gs                ${work}/
if [ "$letter" == "w" ] || [ "$letter" == "b" ] || [ "$letter" == "a" ] || \
   [ "$letter" == "s" ] || [ "$letter" == "p" ]; then
 /bin/cp -f ${sorc}/lib-gs/para_plot_tracks_wp.gs ${work}/para_plot_tracks.gs
else
 /bin/cp -f ${sorc}/lib-gs/para_plot_tracks_al.gs ${work}/para_plot_tracks.gs
fi
/bin/cp -f ${sorc}/utils/dummy_track.ctl          ${work}/dummy.ctl  
/bin/cp -f ${sorc}/utils/dummy.dat                ${work}/  
#--------------------------------------------------------------------------
# create a file of commands to be used by the stream editor
# to change the grads script 
#--------------------------------------------------------------------------
echo "s/STID/${STORMID}/g"            >  ${work}/change_dims
echo "s/STORMNAME/${STORM}/g"        >>  ${work}/change_dims
echo "s/YYYYMMDDHH/${yyyymmddhh}/g"  >>  ${work}/change_dims
echo "s/MC_MODEL/${MC_MODEL}/g"      >>  ${work}/change_dims
echo "s/DESC/2011_PARA/g"            >>  ${work}/change_dims
echo "s/slat/${slat}/g"              >>  ${work}/change_dims
echo "s/nlat/${nlat}/g"              >>  ${work}/change_dims
echo "s/wlon/${wlon}/g"              >>  ${work}/change_dims
echo "s/elon/${elon}/g"              >>  ${work}/change_dims

# apply these changes to grads script with stream editor
sed -f ${work}/change_dims ${work}/para_plot_tracks.gs > \
                           ${work}/plot_tracks.gs  
#--------------------------------------------------------------------------
# run the grads script in batch mode (b=batch,l=landscape,
# c=execute supplied command). This produces the file track.png 
grads -blc "plot_tracks.gs" 
#--------------------------------------------------------------------------
# check to see if file 'track.png' is created; else exit
if [ -s ${work}/track.png ] 
then
   echo "${work}/track.png file created" 
else
   echo "FILE ${work}/track.png NOT CREATED"
   echo 'SCRIPT WILL EXIT'
   exit 1 
fi 
#--------------------------------------------------------------------------
# copy the track.png file to the archive directory to save 
# and rename it
#--------------------------------------------------------------------------
if [ -d ${archive_dir} ]
then
   echo "${archive_dir} exists"
else
   mkdir -p ${archive_dir} 
fi 
/bin/cp -f ${work}/track.png ${archive_dir}/${STORM}${STORMID}.${yyyymmddhh}.track.png 
#
# dont remove the old track log as this is needed for intensity plot... dont know
# why they design the plotting this way but hey it is a history
#
#rm -rf ${work}
#--------------------------------------------------------------------------
# copy the track.png file to the rzdm 
#${scp} -p ${archive_dir}/${STORM}${STORMID}.${yyyymmddhh}.track.png  wd20vxt@rzdm.ncep.noaa.gov:${remote_dir}/. 

# end of script 
