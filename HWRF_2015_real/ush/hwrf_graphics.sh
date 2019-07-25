#! /bin/ksh
# [NOTE]
#       This script makes graphics for parallel HWRF runs
#
# [USAGE]   
#       sh ./para_graphics.sh STORM STORMID YYYYMMDDHH MODEL runmode \
#       tmp_dir home_dir comout_dir
#             
#       E.g. sh ./para_graphics.sh IGOR 02e 2011062000 HWRF \
#            /pan2/projects/hwrfv3/usr/ptmp/usr/hwrf/2013110312/31w \              
#            /mnt/lfs2/projects/hwrfv3/usr/hwrf-southern-hemis/ush \ 
#            /pan2/projects/hwrfv3/usr/ptmp/usr/hwrf/com/2013110312/31w           
#
# [INPUT]
#     1. best track data (not in the real time mode): controlled by
#        $bdeck_path (see below)
#     2. adeck file: controlled by $adeck_path, which contains all 
#        models in the track ($MODEL_REFT) and intensity reference
#        $MODEL_REFI  
#     3. full field forecast grib data: controlled by $archive_dir
#     4. NOSCRUB directory: controlled by $noscrub_dir, which is
#        needed for the *.trackunix file. This however should be
#        replaced as this information is already in the com dir.
#     5. List of reference model for track ($MODEL_REFT) and
#        intensity ($MODEL_REFI). NOTE particularly that these
#        models have to be consistent with what listed under
#        ../lib-gs/para_plot_track.gs, ../lib-gs/para_plot_vmax.gs
#        and ../lib-gs/para_plot_pmin.gs. This is inconvenient
#        but it is a historical design. Should be changed soon!
#
#        WARNING: for LOOP=1, all the paths and model lists will
#        be passed from the para_driver.sh!
#
# [OUTPUT] 
#       Full list of figures under the directory ./figures/
#
# [HIST]
#       00 May 2011: edited as backup for 2011 oper runs by Janna 
#       13 Feb 2012: taken from Janna and revise for better strucutre
#                    by Chanh K.
#       22 Mar 2012: edited the entire package for better control and
#                    get rid of the time-combined grib data that is
#                    no longer archived by CK
#       04 Apr 2012: the HPSS retrieve in the LOOP run has been moved
#                    to the main driver to speed up the next cycle
#       25 Nov 2013: modified for real-time graphics
#
#=====================================================================
set -x
#
# get the input variable 
#
if [ $# -eq 7 ]; then 
   STORM=$1
   stormid=$2
   YYYYMMDDHH=$3
   MC_MODEL=$4
   tmp_dir=$5
   graf_dir=$6
   archive_dir=$7
   echo 'STORM      = ' ${STORM}
   echo 'stormid    = ' ${stormid}
   echo 'YYYYMMDDHH = ' ${YYYYMMDDHH}
   echo 'MC_MODEL   = ' ${MC_MODEL}
   echo 'tmp_dir   = ' ${tmp_dir}
else
   echo 'Usage: sh hwrf_graphics.sh STORM STORMID YYYYMMDDHH MC_MODEL tmp_dir'
   echo 'E.g., sh para_graphics.sh BUD 02e 2012052500 HWRF HISTORY 0'
   echo 'NEED SIX INPUT ARGUMENTS'
   echo 'SCRIPT WILL EXIT'
   exit 1 
fi 
work_dir="${tmp_dir}/tmp/"
diag_dir="${tmp_dir}/TCdiagnosis/diagnose_total"
storm=`  echo ${STORM}   | tr '[A-Z]' '[a-z]'`
STORMID=`echo ${stormid} | tr '[a-z]' '[A-Z]'`
bdeck_path=${graf_dir}/../abdeck/
adeck_path=${graf_dir}/../abdeck/
mkdir -p ${work_dir} ${tmp_dir}/figures
echo 'tmp_dir = ' ${tmp_dir} 
echo 'work_dir = ' ${work_dir}
echo 'STORM      = ' ${STORM}
echo 'storm      = ' ${storm} 
echo 'STORMID    = ' ${STORMID}
echo 'stormid    = ' ${stormid}
#
# Copy the files on JET to my directory, and check to see if trakfile 
# present. otherwise skip to process next storm in loop
#
mkdir -p ${work_dir}/${YYYYMMDDHH}
mkdir -p ${work_dir}/${YYYYMMDDHH}/${stormid}
cd ${work_dir}/${YYYYMMDDHH}/${stormid}
trakfile=${storm}${stormid}.${YYYYMMDDHH}.trak.hwrf.atcfunix
#
# now link the data from the com/hpss to the working dir and untar
# if necessary
#
runmode="REALTIME"
if [ "$cluster" == "ZEUS" ] && [ "$LOOP" == "0" ] && [ "$runmode" == "HISTORY" ]; then
 echo "working ZEUS machine"
 hsi get ${archive_dir}/${storm}${stormid}.${YYYYMMDDHH}.tar
 tar -xf ./${storm}${stormid}.${YYYYMMDDHH}.tar
elif [ "$cluster" == "JET" ] && [ "$LOOP" == "0" ] && [ "$runmode" == "HISTORY" ]; then
 echo "working JET machine"
 ln -sf ${archive_dir}/${storm}${stormid}.${YYYYMMDDHH}.tar.gz ./
 tar -xf ./${storm}${stormid}.${YYYYMMDDHH}.tar*
 cp ${noscrub_dir}/${trakfile} .
elif [ "$LOOP" == "1" ] && [ "$runmode" == "HISTORY" ]; then
 echo "LOOP = 1, HPSS step was finished in the driver script"
 tar -xf ./${storm}*.${YYYYMMDDHH}.tar*
elif [ "$runmode" == "REALTIME" ]; then
 echo "runmode = $runmode. Data is ready"
 if [ -d ${archive_dir} ]; then
  ln -sf ${archive_dir}/* ./
 else
  echo "${archive_dir} does not exit"
  echo "data dir must exist in tmp before moving on...recheck"
  exit 1
 fi
else
 echo "$cluster option is not supported..exit 1"
 exit 1
fi
if [ -s ${work_dir}/${YYYYMMDDHH}/${stormid}/${trakfile} ]
then
   echo "${trakfile} present, will proceed" 
else
   echo "${trakfile} NOT PRESENT"
   echo "SCRIPT WILL NOT PROCESS ${stormlabel} ${STORM} ${STORMID}"
   exit 1
fi
#
# check for basin to plot the appropriate models
#
letter=`echo ${stormid} | cut -c3`
echo 'letter = ' ${letter}
if [ ${letter} = 'l' ]; then
 MODEL_REFT="HWRF OFCL AVNO GFDL"
 MODEL_REFI="GFDN HWRF GFDL SHF5"
elif [ ${letter} = 'e' ]; then
 MODEL_REFT="HWRF OFCL AVNO GFDL"
 MODEL_REFI="GFDN HWRF GFDL SHF5"
elif [ ${letter} = 'c' ]; then
 MODEL_REFT="HWRF OFCL AVNO GFDL"
 MODEL_REFI="GFDN HWRF GFDL SHF5"
elif [ ${letter} = 'w' ] || [ ${letter} = 'b' ] || [ ${letter} = 'a' ] \
     [ ${letter} = 's' ] || [ ${letter} = 'p' ]; then
 MODEL_REFT="NGPS JTWC AVNO GFDN COTC CTCX"
 MODEL_REFI="GFDN JTWC COTC CTCX"
else
 echo "BASIN DESIGNATION LETTER letter = ${letter} NOT LOWER CASE l, e, w, s, p, a, or c"
 echo 'SCRIPT WILL EXIT'
 exit 1
fi
if [ ${letter} = 'l' ]; then
     output_dir="${tmp_dir}/figures/RT_ATLANTIC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"
elif [ ${letter} = 'e' ]; then
     output_dir="${tmp_dir}/figures/RT_EASTPAC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"
elif [ ${letter} = 'c' ]; then
     output_dir="${tmp_dir}/figures/RT_CPAC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"
elif [ ${letter} = 'w' ] || [ ${letter} = 'b' ] || [ ${letter} = 'a' ] \
         [ ${letter} = 's' ] || [ ${letter} = 'p' ]; then
     output_dir="${tmp_dir}/figures/RT_WPAC/${STORM}${STORMID}/${STORM}${STORMID}.${yyyymmddhh}"
else
     echo "BASIN DESIGNATION LETTER letter = ${letter} NOT LOWER CASE l, e, a, w, s, p, or c"
     echo 'SCRIPT WILL EXIT'
     exit 1
fi
#
# Finally, call each individual script to plot the vars we want as follows: 
#
SHIP_DIAG_ONLY="YES"
if [ ${SHIP_DIAG_ONLY} = 'NO' ]; then
# make the track plots
    sh ${graf_dir}/scripts/para_plot_track.sh ${STORM} ${stormid} ${YYYYMMDDHH} ${MC_MODEL}     \
     ${graf_dir} ${bdeck_path} ${adeck_path} "${MODEL_REFT}" ${tmp_dir}                         \
     1> ${work_dir}/plot_track.out.${stormid}.${YYYYMMDDHH}                                     \
     2> ${work_dir}/plot_track.err.${stormid}.${YYYYMMDDHH}
    
# make the intensity plots
    sh ${graf_dir}/scripts/para_plot_intensity.sh ${STORM} ${stormid} ${YYYYMMDDHH} ${MC_MODEL} \
     ${graf_dir} ${bdeck_path} ${adeck_path} "${MODEL_REFI}" ${tmp_dir}                         \
     1> ${work_dir}/plot_intensity.out.${stormid}.${YYYYMMDDHH}                                 \
     2> ${work_dir}/plot_intensity.err.${stormid}.${YYYYMMDDHH}

# make swath plots
    sh ${graf_dir}/scripts/para_plot_swath.sh ${STORM} ${stormid} ${YYYYMMDDHH} ${MC_MODEL}     \
     ${graf_dir} ${tmp_dir} 1> ${work_dir}/plot_swath.out.${stormid}.${YYYYMMDDHH}              \
     2> ${work_dir}/plot_swath.err.${stormid}.${YYYYMMDDHH}                                     &  
    
# make GOES images
    sh ${graf_dir}/scripts/para_plot_goes.sh ${STORM} ${stormid} ${YYYYMMDDHH} ${MC_MODEL}      \
     ${graf_dir} ${tmp_dir} 1> ${work_dir}/plot_goes.out.${stormid}.${YYYYMMDDHH}               \
     2> ${work_dir}/plot_goes.err.${stormid}.${YYYYMMDDHH}                                      &

# make vertical cross section images
    sh ${graf_dir}/scripts/para_plot_vert_sect.sh ${STORM} ${stormid} ${YYYYMMDDHH} ${MC_MODEL} \
     ${graf_dir} ${tmp_dir} 1> ${work_dir}/plot_vert_sect.out.${stormid}.${YYYYMMDDHH}          \
     2> ${work_dir}/plot_vert_sect.err.${stormid}.${YYYYMMDDHH}                                 &

# make combine plots
    sh ${graf_dir}/scripts/para_plot_combine.sh ${STORM} ${stormid} ${YYYYMMDDHH} ${MC_MODEL}   \
     ${graf_dir} ${tmp_dir} 1> ${work_dir}/plot_combine.out.${stormid}.${YYYYMMDDHH}            \
     2> ${work_dir}/plot_combine.err.${stormid}.${YYYYMMDDHH}                                   &

# make nest_center plots
    sh ${graf_dir}/scripts/para_plot_nest_center.sh ${STORM} ${stormid} ${YYYYMMDDHH} ${MC_MODEL} \
     ${graf_dir} ${tmp_dir} 1> ${work_dir}/plot_nest_center.out.${stormid}.${YYYYMMDDHH}          \
     2> ${work_dir}/plot_nest_center.err.${stormid}.${YYYYMMDDHH}                                 &
#
# check to see if all graphics are produced 
#
    scount=1
    until [ -s "${output_dir}/${STORM}${STORMID}.${yyyymmddhh}.c42.png" ] && \
          [ -s "${output_dir}/${STORM}${STORMID}.${yyyymmddhh}.n42.png" ]
    do
         echo "${output_dir}/${STORM}${STORMID}.${yyyymmddhh}.c42.png is not existing...sleep 30s"
         scount=$(($scount+1))
         if [ $scount -lt "1800" ]; then
              sleep 30
         else
              echo "Sleep for too long...exit 1"
              exit 1
         fi
    done
    cd $output_dir
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
         rm -f temp1.png ${STORM}${STORMID}.${yyyymmddhh}.n${icount}.png 
         rm -f ${STORM}${STORMID}.${yyyymmddhh}.c${icount}.png ${STORM}${STORMID}.${yyyymmddhh}.ctr${icount1}.png
    done
fi
#
# make ships diagnostics outside of the main graphics sequence
#
sh ${graf_dir}/scripts/para_plot_ships.sh ${STORM} ${stormid} ${YYYYMMDDHH} ${MC_MODEL}     \
 ${graf_dir} ${tmp_dir} 1> ${work_dir}/plot_ships.out.${stormid}.${YYYYMMDDHH}              \
 2> ${work_dir}/plot_ships.err.${stormid}.${YYYYMMDDHH}                                     
#
# move the output figure and assoicated html to com dir
#
mkdir -p ${archive_dir}/figures
if [ "$output_dir" != ""  ]; then
 mv $output_dir/* ${archive_dir}/figures/
fi
#
# end of graphics script 
#
