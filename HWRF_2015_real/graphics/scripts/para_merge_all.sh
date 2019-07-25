#! /bin/ksh --login
#
# NOTE: this script is for producing all of the JTWC graphics
#       accornding to their color bars.
#
# Usage:   sh para_plot_jtwc.sh STORM stormid yyyymmddhh MC_MODEL 
# Example: sh para_plot_jtwc.sh BARRY   02l   2007060118  H211
#
# HIST:
#      05, June 2012: written by C. Kieu (chanh.kieu@noaa.gov)
#
##################################################################
#PBS -l procs=1
#PBS -l walltime=1:00:00
#PBS -d .
#PBS -N plot_merge
#PBS -A ACCOUNTLL
#PBS -l flags=ADVRES:JET_RESERVATION
#PBS -l partition=ujet:tjet:njet
#PBS -o DIRLOG_OUT
#PBS -e DIRLOG_ERR
set -x 
if [ -s ./set_path.sh ]; then
. ./set_path.sh
else
 PBS=0
fi
GRADDIR="${GRAD_PATH:-/apps/grads/2.0.1/bin}"
COPYGBDIR="${COPYGB_PATH:-/home/Chanh.Kieu/bin}"
NDATEDIR="${NDATE_PATH:-/home/Chanh.Kieu/bin}"
WGRIBDIR="${WGRIB_PATH:-/home/Chanh.Kieu/bin}"
#------------------------------------------------------
# get the input variables 
if [ $# -eq 5 ] || [ "$PBS" == "1" ]; then
   if [ "$PBS" == "0" ]; then
    STORM=$1
    stormid=$2
    yyyymmddhh=$3
    MC_MODEL=$4
    sorc=$5
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
   echo 'USAGE: sh para_plot_jtwc.sh STORM stormid yyyymmddhh MC_MODEL'
   echo 'NEED FOUR ARGUMENTS - SCRIPT WILL EXIT'
   exit 1  
fi
#
# create the lower case storm name and create upper case storm number
#
storm=`echo ${STORM} | tr '[A-Z]' '[a-z]'`
cyc=`echo ${yyyymmddhh} | cut -c9-10`
lstormid=${stormid}
STORMID=`echo ${stormid} | tr '[a-z]' '[A-Z]' ` 
echo 'storm = ' ${storm}
echo 'cyc   = ' ${cyc}  
echo 'STORMID = ' ${STORMID}  
#
# set the pathways. Note that data_dir is the path of the HWRF model 
# output data files, and contains data files for the PARENT, NEST,
# and COMBINE domains. 
#
data_dir="${sorc}/tmp/${yyyymmddhh}/${stormid}"
archive_base="${sorc}/figures"  
#
# Determine basin of storm from third letter of stormid 
# (l=ATLANTIC, e=EASTPAC, c=CENTRAL PACIFIC) and set archive directory
# on DEW from basin of storm note that CENTRAL Pacific will go to EASTPAC dir 
#
letter=`echo ${stormid} | cut -c3`
echo 'letter = ' ${letter}
if [ ${letter} = 'l' ]; then
 archive_dir="${archive_base}/RT_ATLANTIC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"  
elif [ ${letter} = 'e' ]; then
 archive_dir="${archive_base}/RT_EASTPAC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"  
elif [ ${letter} = 'c' ]; then
 archive_dir="${archive_base}/RT_CPAC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"  
elif [ ${letter} = 'w' ] || [ ${letter} = 'b' ] || [ ${letter} = 'a' ] || \
     [ ${letter} = 's' ] || [ ${letter} = 'p' ]; then
 archive_dir="${archive_base}/RT_WPAC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"
else
 echo "BASIN DESIGNATION LETTER letter = ${letter} NOT LOWER CASE l, e, or c"
 echo 'SCRIPT WILL EXIT'
 exit 1
fi 
echo 'sorc =      ' ${sorc}
echo 'data_dir =  ' ${data_dir} 
echo 'archive_dir = ' ${archive_dir}  
#
# check if time to merge yet
#
scount=1
until [ -s "${archive_dir}/${STORM}${STORMID}.${yyyymmddhh}.c42.png" ]
do
 echo "${archive_dir}/${STORM}${STORMID}.${yyyymmddhh}.c42.png is not existing...sleep 30s"
 scount=$(($scount+1))
 if [ $scount -lt "1800" ]; then
  sleep 30
 else
  echo "Sleep for too long...exit 1"
  exit 1
 fi
done
cd $archive_dir
#
# merge combined domain and track
#
for icount in {0..42}
do
 icount1=$(($icount+1))
 rm -rf ${STORM}${STORMID}.${yyyymmddhh}.m${icount}.png tem1.png
 montage -geometry 512x768 -mode concatenate -tile 1x2 ${STORM}${STORMID}.${yyyymmddhh}.n${icount}.png  \
         ${STORM}${STORMID}.${yyyymmddhh}.ctr${icount1}.png temp1.png
 montage -trim -mode concatenate -tile 2x1 ${STORM}${STORMID}.${yyyymmddhh}.c${icount}.png              \
          temp1.png ${STORM}${STORMID}.${yyyymmddhh}.m${icount}.png 
 rm -f temp1.png
done
#
# merge satelite product
#
rm -rf *.37Ghz.png *.89Ghz.png *.IR.png *.WV.png
for icount in {1..22}
do
 montage -trim -mode concatenate -tile 2x1 ${STORM}${STORMID}.${yyyymmddhh}.micro37_${icount}.png       \
          ${STORM}${STORMID}.${yyyymmddhh}.3km_37h_f${icount}.gif ${STORM}${STORMID}.${yyyymmddhh}.37Ghz${icount}.png
 montage -trim -mode concatenate -tile 2x1 ${STORM}${STORMID}.${yyyymmddhh}.micro89_${icount}.png       \
          ${STORM}${STORMID}.${yyyymmddhh}.3km_91h_f${icount}.gif ${STORM}${STORMID}.${yyyymmddhh}.89Ghz${icount}.png
 montage -trim -mode concatenate -tile 2x1 ${STORM}${STORMID}.${yyyymmddhh}.par_goesIR${icount}.png     \
          ${STORM}${STORMID}.${yyyymmddhh}.3km_IR_f${icount}.gif ${STORM}${STORMID}.${yyyymmddhh}.IR${icount}.png
 montage -trim -mode concatenate -tile 2x1 ${STORM}${STORMID}.${yyyymmddhh}.par_goesWV${icount}.png     \
          ${STORM}${STORMID}.${yyyymmddhh}.3km_WV_f${icount}.gif ${STORM}${STORMID}.${yyyymmddhh}.WV${icount}.png
 icount=$(($icount+1))
done
#
# merge vertical cross section
#
rm -rf *.vx*.png
for icount in {1..22}
do
 montage -mode concatenate -tile 2x1 ${STORM}${STORMID}.${yyyymmddhh}.vXZ${icount}.png       \
          ${STORM}${STORMID}.${yyyymmddhh}.vYZ${icount}.png ${STORM}${STORMID}.${yyyymmddhh}.vx${icount}.png
 icount=$(($icount+1))
done
#
# create looping for In-Hyuk stuff
#
scount=1
until [ -s "./total/${STORM}${lstormid}.${yyyymmddhh}_18h_${MC_MODEL}.diag2.png" ]
do
 echo "./total/${STORM}${lstormid}.${yyyymmddhh}_18h_${MC_MODEL}.diag2.png is not existing...sleep 30s"
 scount=$(($scount+1))
 if [ $scount -lt "45" ]; then
  sleep 30
 else
  echo "Sleep for too long...exit 1"
  exit 1
 fi
done
scount=1
until [ -s "./total/${STORM}${lstormid}.${yyyymmddhh}_72h_${MC_MODEL}.diag2.png" ]
do
 echo "./total/${STORM}${lstormid}.${yyyymmddhh}_72h_${MC_MODEL}.diag2.png is not existing...sleep 30s"
 scount=$(($scount+1))
 if [ $scount -lt "45" ]; then
  sleep 30
 else
  echo "Sleep for too long...exit 1"
  exit 1
 fi
done
scount=1
until [ -s "./total/${STORM}${lstormid}.${yyyymmddhh}_126h_${MC_MODEL}.diag2.png" ]
do
 echo "./total/${STORM}${lstormid}.${yyyymmddhh}_126h_${MC_MODEL}.diag2.png is not existing...sleep 30s"
 scount=$(($scount+1))
 if [ $scount -lt "45" ]; then
  sleep 30
 else
  echo "Sleep for too long...exit 1"
  exit 1
 fi
done

icount=0
for hh in 00 03 06 09 12 15 18 \
          24 30 36 42 48 54 60 66 72 \
          78 84 90 96 102 108 114 120 126

do
 mv -f ./total/${STORM}${lstormid}.${yyyymmddhh}_${hh}h_${MC_MODEL}.diag1.png ${STORM}${STORMID}.${yyyymmddhh}.diag1_f${icount}.png
 mv -f ./total/${STORM}${lstormid}.${yyyymmddhh}_${hh}h_${MC_MODEL}.diag2.png ${STORM}${STORMID}.${yyyymmddhh}.diag2_f${icount}.png
 icount=`expr $icount + 1`
done
#
# merge for diag
#
sed 's/dlm_p_f/diag1_f/g' hwrf_dlm.html > hwrf_diag1.html
sed -i 's/42,/24,/' hwrf_diag1.html
sed -i 's/1024,/1140,/' hwrf_diag1.html
sed -i 's/768)/1000)/' hwrf_diag1.html
sed -i 's/Deep_layer_mean/Wind Structure/'  hwrf_diag1.html
sed 's/dlm_p_f/diag2_f/g' hwrf_dlm.html > hwrf_diag2.html
sed -i 's/42,/24,/' hwrf_diag2.html
sed -i 's/1024,/1140,/' hwrf_diag2.html
sed -i 's/768)/1000)/' hwrf_diag2.html
sed -i 's/Deep_layer_mean/Thermal Structure/'  hwrf_diag2.html
rm -rf ${sorc}/tmp/diagnosis/${STORM}${STORMID}/${yyyymmddhh}.*
echo 'para_merge_all completed'
