#!/bin/ksh
set -x
home_dir="/export/emc-lw-ckieu/wd20ck/web/"
jet_dir="/mnt/lfs2/projects/hwrfv3/hurrun/PARA_wpac/figures"
rzdm_dir="/home/others/people/emc/www/htdocs/gc_wmb/vxt"
cd ${home_dir}
echo "Starting at $( date )"  
rm -rf realtime_cycle.txt
scp -r Chanh.Kieu@jetscp.rdhpcs.noaa.gov:${jet_dir}/realtime_cycle.txt ${home_dir}
if [ -s ./realtime_cycle.txt ]; then
 echo "The index file realtime_cycle.txt has been scped from jet"
else
 touch ./realtime_cycle.txt
fi
while read istorm
do
 storm=`echo $istorm | awk '{print $1}'`
 yyyymmddhh=`echo $istorm | awk '{print $2}'`
 ustorm=`echo $storm | tr '[a-z]' '[A-Z]'`
 lstorm=`echo $storm | tr '[A-Z]' '[a-z]'`
 slength=`echo ${#ustorm}`
 #basin=`echo ${lstorm:(-1)}`
 basin=`echo $lstorm | cut -c $slength-$slength`
 echo 'basin = ' ${basin}
 if [ ${basin} = 'l' ]; then
  icycle="${jet_dir}/RT_ATLANTIC/${ustorm}"
  dcycle="${home_dir}/RT_ATLANTIC/${ustorm}"
  rcycle="${rzdm_dir}/RT_ATLANTIC/${ustorm}"
 elif [ ${basin} = 'e' ]; then
  icycle="${jet_dir}/RT_EASTPAC/${ustorm}"
  dcycle="${home_dir}/RT_EASTPAC/${ustorm}/"
  rcycle="${rzdm_dir}/RT_EASTPAC/${ustorm}/"
 elif [ ${basin} = 'c' ];then
  icycle="${jet_dir}/RT_CWPAC/${ustorm}"
  dcycle="${home_dir}/RT_CWPAC/${ustorm}"
  rcycle="${rzdm_dir}/RT_CWPAC/${ustorm}"
 elif [ ${basin} = 'w' ]; then
  icycle="${jet_dir}/RT_WPAC/${ustorm}"
  dcycle="${home_dir}/RT_WPAC/${ustorm}" 
  rcycle="/home/others/people/emc/www/htdocs/HWRF/WestPacific/${ustorm}"
 else
   echo "BASIN DESIGNATION LETTER basin = ${basin} NOT LOWER CASE l, e, or c"
   echo 'SCRIPT WILL EXIT'
   exit 1
 fi
 mkdir -p $dcycle
 if [ -d $dcycle/${ustorm}.${yyyymmddhh} ]; then
  echo "The cycle: $dcycle/${ustorm}.${yyyymmddhh} is existing... transfer track/intensity files only"
  ssh -n  wd20vxt@emcrzdm.ncep.noaa.gov  mkdir -p ${rcycle}
  scp -r Chanh.Kieu@jetscp.rdhpcs.noaa.gov:$icycle/${ustorm}.${yyyymmddhh}/\*fsct.png $dcycle/${ustorm}.${yyyymmddhh}/ > /dev/null
  scp -r $dcycle/${ustorm}.${yyyymmddhh} wd20vxt@emcrzdm:$rcycle > /dev/null
  scp $dcycle/index.html wd20vxt@emcrzdm:$rcycle/ > /dev/null
  ssh -n  wd20vxt@emcrzdm.ncep.noaa.gov chmod -R uog+xr ${rcycle} 
 else
  echo "Jet cycle dir: $icycle/${ustorm}.${yyyymmddhh}" 
  echo "Local cycle dir: $dcycle" 
  echo "rzdm cycle dir: $rcycle" 
  ssh -n  wd20vxt@emcrzdm.ncep.noaa.gov  mkdir -p ${rcycle}
  scp -r Chanh.Kieu@jetscp.rdhpcs.noaa.gov:$icycle/${ustorm}.${yyyymmddhh} $dcycle/ > /dev/null
  scp -r Chanh.Kieu@jetscp.rdhpcs.noaa.gov:$icycle/index.html $dcycle/ > /dev/null
  scp -r $dcycle/${ustorm}.${yyyymmddhh} wd20vxt@emcrzdm:$rcycle/ > /dev/null
  scp $dcycle/index.html wd20vxt@emcrzdm:$rcycle/ > /dev/null
  ssh -n  wd20vxt@emcrzdm.ncep.noaa.gov chmod -R uog+xr ${rcycle}
 fi
done < ./realtime_cycle.txt
