#!/bin/bash
#
# NOTE: This script is for looping the entire parent (27km) and (3km) to
#       generate the combined domain and moving nest plots for the HWRF
#       website
#
# HIST: Mar 21, 2012: created by CK (chanh.kieu@noaa.gov)
#
#=======================================================================
#
# read the input parameter and filter
#
set -x
if [ $# -eq 4 ]; then
   STORM=$1
   stormid=$2
   cycle=$3
   MC_MODEL=$4
   echo 'STORM      = ' ${STORM}
   echo 'stormid    = ' ${stormid} 
   echo 'cycle      = ' ${cycle}  
   echo 'MC_MODEL   = ' ${MC_MODEL}
else
   echo 'USAGE:   sh movie_combine.sh STORM stormid yyyymmddhh MC_MODEL'
   echo 'NEED FOUR ARGUMENTS - SCRIPT WILL EXIT'
   exit 1  
fi
storm=`echo ${STORM} | tr '[A-Z]' '[a-z]'`
storm1=`echo ${storm} | tr '[a-z]' '[A-Z]'`
stormid=`echo ${stormid} | tr '[A-Z]' '[a-z]'`
stormid1=`echo ${stormid} | tr '[a-z]' '[A-Z]'`
#
# serching for plotting domain
#
atcf_file=${storm}${stormid}.${cycle}.trak.hwrf.atcfunix
sline=`head -n 1 $atcf_file`
eline=`tail -n 1 $atcf_file` 
lon1=`awk '{if ($12!="50," && $12!="64,") print $8}' ${atcf_file} |            \
      sed 's/W,/ W/g' | sed 's/E,/ E/g' | awk '{if($2=="W") print 360-0.1*$1;  \
      if($2=="E") print 0.1*$1}' | awk '{if(pm=="")(pm=$1); if($1<pm)(pm=$1)} \
      END {print pm}'`
lon2=`awk '{if ($12!="50," && $12!="64,") print $8}' ${atcf_file} |            \
      sed 's/W,/ W/g' | sed 's/E,/ E/g' | awk '{if($2=="W") print 360-0.1*$1;  \
      if($2=="E") print 0.1*$1}' | awk '{if(pm=="")(pm=$1); if($1>pm)(pm=$1)} \
      END {print pm}'`
lat1=`awk '{if ($12!="50," && $12!="64,") print $7}' ${atcf_file} |            \
      sed 's/S,/ S/g' | sed 's/N,/ N/g' | awk '{if($2=="S") print -0.1*$1;     \
      if($2=="N") print 0.1*$1}' | awk '{if(pm=="")(pm=$1); if($1<pm)(pm=$1)}  \
      END {print pm}'`
lat2=`awk '{if ($12!="50," && $12!="64,") print $7}' ${atcf_file} |            \
      sed 's/S,/ S/g' | sed 's/N,/ N/g' | awk '{if($2=="S") print -0.1*$1;     \
      if($2=="N") print 0.1*$1}' | awk '{if(pm=="")(pm=$1); if($1>pm)(pm=$1)}  \
      END {print pm}'`
#
# create a track file for imposing on the nest-move plots
#
awk '{if ($12!="50," && $12!="64,") print $8" "$7}' ${atcf_file} |              \
     sed 's/W,/ W/g' | sed 's/E,/ E/g' | sed 's/S,/ S/g' | sed 's/N,/ N/g' |    \
     awk '{if($2=="W" && $4=="N") print 360-0.1*$1" "0.1*$3; else if($2=="W" && \
     $4=="S") print 360-0.1*$1" "-0.1*$3; else if($2=="E" && $4=="S") print     \
     0.1*$1" "-0.1*$3; else if($2=="E" && $4=="N") print 0.1*$1" "0.1*$3}' > fort.30
echo "$MC_MODEL forecast at ${cycle} for ${STORM} (${stormid})" > fort.32
echo "1 0.13" >> fort.32
echo "5 1 18 " >> fort.32
echo "0 6 " >> fort.32
#
# lat1=`echo $lat0 | sed 's/[A-Z]//g'`
# lon1=`echo $lon0 | sed 's/[A-Z]//g'`
# lat_id=`echo $lat0 | awk '{print substr($0,length,1)}'`
# lon_id=`echo $lon0 | awk '{print substr($0,length,1)}'`
#
echo $lat1 $lat2 $lon1 $lon2
#check_lat=`echo "$lat1 < $lat2" | bc -l`
#check_lon=`echo "$lon1 < $lon2" | bc -l`
check_lat=`echo "$lat1 $lat2" | awk '{if ($1 < $2) print 1; else print 0}'`
check_lon=`echo "$lon1 $lon2" | awk '{if ($1 < $2) print 1; else print 0}'`
if [ "$check_lat" == "1" ]; then
 blat=`echo "$lat1 - 7" | bc -l`
 elat=`echo "$lat2 + 7" | bc -l`
else
 blat=`echo "$lat2 - 7" | bc -l`
 elat=`echo "$lat1 + 7" | bc -l`
fi
if [ "$check_lon" == "1"  ]; then
 blon=`echo "$lon1 - 7" | bc -l`
 elon=`echo "$lon2 + 7" | bc -l`
else
 blon=`echo "$lon2 - 7" | bc -l`
 elon=`echo "$lon1 + 7" | bc -l`
fi
echo $blat $elat $blon $elon
vmax=`awk '{print $9}' $atcf_file | sed 's/,//g' | awk \
      '{if(max=="")(max=$1); if($1>max)(max=$1)} END {print max}'`
lead_time=48
fsct=$cycle
#
# loop thru all the files and  plot the parent/combine domain
# and nest moving 
#
prefix="${storm}${stormid}.${cycle}.hwrfprs_p"
rm -f ${prefix}*.idx  ${prefix}*.ctl grads_*.*
count=0
flag_t00="0"
for ifile in $(ls ${prefix}* )
do
 nfile=`echo $ifile | sed 's/hwrfprs_p/hwrfprs_n/'`
 echo "working with file = $ifile; $nfile"
 temp=`echo ${ifile##*.} | sed s/grbf//g` 
 lead_time=`echo "$temp + 0" | bc -l`
 ./grib2ctl.pl -verf ${ifile} > ${ifile}.ctl
 gribmap -i ${ifile}.ctl
 ln -sf ${ifile}.ctl ./grads_p.ctl
 if [ "$flag_t00" == "0" ]; then
  nnx=`awk '{if($1=="xdef" && $3=="linear") print $2}' ./grads_p.ctl`  
  slon=`awk '{if($1=="xdef" && $3=="linear") print $4}' ./grads_p.ctl`
  ddx=`awk '{if($1=="xdef" && $3=="linear") print $5}' ./grads_p.ctl`
  nny=`awk '{if($1=="ydef" && $3=="linear") print $2}' ./grads_p.ctl`
  slat=`awk '{if($1=="ydef" && $3=="linear") print $4}' ./grads_p.ctl`
  ddy=`awk '{if($1=="ydef" && $3=="linear") print $5}' ./grads_p.ctl`
  ddx=`echo "$ddx*1000" | bc -l`
  ddy=`echo "$ddy*1000" | bc -l`
  slon=`echo "$slon*1000" | bc -l`
  slat=`echo "$slat*1000" | bc -l`
  flon=`echo "$slon + $nnx*$ddx" | bc -l`
  flat=`echo "$slat + $nny*$ddy" | bc -l`
  clon=`echo "($flon+$slon)/2" | bc -l`
  clat=`echo "($flat+$slat)/2" | bc -l` 
  lon1=`printf "%.0f\n" $slon`
  lon2=`printf "%.0f\n" $flon`
  lat1=`printf "%.0f\n" $slat`
  lat2=`printf "%.0f\n" $flat`
  flag_t00="1"
  echo "lon1 = $lon1; lon2 = $lon2"
  echo "lat1 = $lat1; lat2 = $lat2"
  if [[ "$lon1" -gt 360000 ]]; then
   cat fort.30 | awk '{print $1+360" "$2}' > fort.31   
  else
   cat fort.30 | awk '{print $1" "$2}' > fort.31
  fi
  icount=0
  while read iline
  do
   echo $icount $iline >> fort.32
   icount=$(($icount+6))
  done < fort.31
 fi
 copygb -g"255 0 111 91 $lat2 $lon1 136 $lat1 $lon2 1000 1000 0" -x ${ifile} grads_c.grb
 grib2ctl.pl -verf grads_c.grb > grads_c.ctl
 gribmap -i grads_c.ctl
 copygb -g"255 0 441 361 $lat2 $lon1 136 $lat1 $lon2 250 250 0" -x ${nfile} grads_n.grb
 grib2ctl.pl -verf grads_n.grb > grads_n.ctl
 gribmap -i grads_n.ctl
 fsct=`ndate ${lead_time} ${cycle}`
 count=$(($lead_time/3))
 echo $storm1 > fort.10
 echo $stormid1 >> fort.10
 echo $cycle >> fort.10
 echo $fsct >> fort.10
 echo $lead_time >> fort.10
 echo "$blat $elat" >> fort.10
 if [[ "$lon1" -gt 360000 ]]; then
  blon_fix=`echo $blon + 360 | bc -l`
  elon_fix=`echo $elon + 360 | bc -l`
 else
  blon_fix=$blon
  elon_fix=$elon
 fi
 echo "$blon_fix $elon_fix" >> fort.10
 echo $vmax >> fort.10
 if [ -s ./grads_c.grb ]; then
  grads -xlbc para_plot_streamline.gs
  mv out.png ${storm1}${stormid1}.${cycle}.c${count}.png
  mv grads_c.grb backup_c.grb
  mv grads_c.ctl backup_c.ctl
 else
  echo "Something wrong with the copygb: use the backup_p from previous time...skip"
  echo ""
 fi
 if [ -s ./grads_n.grb ]; then
  grads -xlbc para_plot_nest_move.gs
  mv out.png ${storm1}${stormid1}.${cycle}.n${count}.png
  mv grads_n.grb backup_n.grb
  mv grads_n.ctl backup_n.ctl
 else 
  echo "Something wrong with the copygb: use the backup_n from previous time...skip"
  echo ""
 fi
 cat fort.10
done

