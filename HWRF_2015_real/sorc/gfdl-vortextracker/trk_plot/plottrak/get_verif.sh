#!/bin/ksh

# This script was written by Tim Marchok and is part of the
# GrADS ATCF track & intensity plotting script.  If there are
# any problems, contact timothy.marchok@noaa.gov
#

if [ $# -lt 1 ]; then
  echo " "
  echo " !!! ERROR: You must enter at least 5 arguments:"
  echo " !!!"
  echo " !!! USAGE: `basename $0` -i interval -t vtype -f afile -d ymdh -u userid model1 .... [model(n)]"
  echo " "
  echo "     interval = track verification interval ('every6','every12','every24')"
  echo "     vtype    = type of verification length (next72, previous120, beginning to now, etc...)"
  echo "     afile    = atcfname in format of al042001, ep112002, etc...."
  echo "     ymdh     = first starting time of date"
  echo "     userid   = tpm, mb, etc...."
  echo " "
  exit 8
fi

# set -x

while getopts i:t:f:d:u: opt
do
  case $opt in
    i) interval="$OPTARG";;
    t) vtype="$OPTARG";;
    f) afile="$OPTARG";;
    d) ymdh="$OPTARG";;
    u) userid="$OPTARG";;
    \?) err=1;;
  esac
done

rundir=/gpfs/blhome/${USER}/HWRF/src/gfdl-vortextracker/trk_plot/plottrak/
netdir=/gpfs/blhome/${USER}/HWRF/src/gfdl-vortextracker/trk_plot/plottrak/tracks
ndate=/gpfs/blhome/${USER}/HWRF/src/gfdl-vortextracker/trk_plot/plottrak/ndate.ksh
nhour=/gpfs/blhome/${USER}/HWRF/src/gfdl-vortextracker/trk_plot/plottrak/nhour.ksh

# echo " "
# echo "After while loop, vtype=    $vtype"
# echo "                  afile=    $afile"
# echo "                  interval= $interval"
# echo "                  ymdh=     $ymdh"
# echo " "

#---------------------------------------------
# First, sort the atcf file, grep out only the
# CARQ cards for 35 kt winds for the "0" lead 
# time and put these into a temporary 
# verification file.

atcffile=a${afile}.dat
grep "CARQ,   0" $atcffile                    |\
      awk 'substr($0,65,1) ~ /3/ {print $0}'  |\
      sort -k3 -n                             |\
      awk '{
        lat10 = substr($0,36,3)
        latns = substr($0,39,1)
        if (latns == "N")  ylat = lat10/10.0
        if (latns == "S")  ylat = lat10/-10.0

        lon10 = substr($0,42,4)
        lonew = substr($0,46,1)
        if (lonew == "W")  xlon = lon10/10.0
        if (lonew == "E")  xlon = 360. - lon10/10.0

        maxwind = substr($0,49,3)
        minprs  = substr($0,54,4)

        printf ("%10s  %4s  %3d  %5.1f  %5.1f  %2s%2s%2s  %10s  %3d  %4d\n",
        substr($0,9,10),
        substr($0,25,4),
        substr($0,31,3),
        ylat,
        xlon,
        substr($0,1,2),substr($0,5,2),substr($0,11,2),
        substr($0,150,10),
        maxwind,
        minprs)
      }' >${netdir}/vfile.temp

case $interval in
#   UPDATE 12/11/2007: Changed so that *ALL* observed times are written
#   to the observed file, not just every so many hours.  Then let the 
#   plotting script determine what interval gets marked.  -tpm
#  "every6")  hrint=6;;
#  "every12") hrint=12;;
#  "every24") hrint=24;;
  "every6")  hrint=6;;
  "every12") hrint=6;;
  "every24") hrint=6;;
esac

#---------------------------------------------
# Based on what type of verification plotting 
# has been chosen, set up the beginning and 
# the end times for the range to grep out.

case $vtype in

  "next72")  begymdh=${ymdh}
             endymdh=`$ndate 72 ${begymdh}`
             iymdh=${begymdh};;

  "next84")  begymdh=${ymdh}
             endymdh=`$ndate 84 ${begymdh}`
             iymdh=${begymdh};;

  "next96")  begymdh=${ymdh}
             endymdh=`$ndate 96 ${begymdh}`
             iymdh=${begymdh};;

  "next120") begymdh=${ymdh}
             endymdh=`$ndate 120 ${begymdh}`
             iymdh=${begymdh};;

  "next144") begymdh=${ymdh}
             endymdh=`$ndate 144 ${begymdh}`
             iymdh=${begymdh};;

  "next168") begymdh=${ymdh}
             endymdh=`$ndate 168 ${begymdh}`
             iymdh=${begymdh};;

  "prev72")  begymdh=`$ndate -72 ${ymdh}`
             endymdh=${ymdh}
             iymdh=${begymdh};;

  "prev96")  begymdh=`$ndate -96 ${ymdh}`
             endymdh=${ymdh}
             iymdh=${begymdh};;

  "prev120") begymdh=`$ndate -120 ${ymdh}`
             endymdh=${ymdh}
             iymdh=${begymdh};;

  "prev144") begymdh=`$ndate -144 ${ymdh}`
             endymdh=${ymdh}
             iymdh=${begymdh};;

  "prev168") begymdh=`$ndate -168 ${ymdh}`
             endymdh=${ymdh}
             iymdh=${begymdh};;

  "begtonow") begymdh=` cat ${netdir}/vfile.temp         |\
                head -1                                  |\
                awk '{printf ("%10s\n",$1)}'`
              endymdh=${ymdh}
              iymdh=${begymdh};;

  "onlynow") endymdh=${ymdh}
             iymdh=${ymdh};;

  "remainder") endymdh=` cat ${netdir}/vfile.temp        |\
               tail -1                                   |\
               awk '{printf ("%10s\n",$1)}'`
               iymdh=${ymdh};;

  "entire")  begymdh=` cat ${netdir}/vfile.temp          |\
               head -1                                   |\
               awk '{printf ("%10s\n",$1)}'`
             endymdh=` cat ${netdir}/vfile.temp          |\
               tail -1                                   |\
               awk '{printf ("%10s\n",$1)}'`
             iymdh=${begymdh};;

esac


# echo "before loop, iymdh= $iymdh, endymdh= $endymdh, hrint= $hrint"

# UPDATE (December 2007):  For the intensity plotting, we now need 
# to get the lead time from the observed data.  Since we're pulling
# out the "CARQ" records from the a-decks, these only have "0" for 
# the hour.  Therefore, we need to do some work with the nhour 
# utility and a paste command in this case statement below.


>${netdir}/xmgr_obs_intensity

case $vtype in 

  prev72|prev96|prev120|prev144|prev168|begtonow)

    # Start with the current date and go backwards.  If we did it the
    # other way, and started at the beginning of the storm, then 
    # depending on the hour interval chosen, we might not actually plot
    # the verification for the current time, which is critical.
     
    rm ${netdir}/vfile.flipped
    refymdh=${ymdh}
    eymdh=${endymdh}
    while [ ${eymdh} -ge ${iymdh} ]
    do
      fhr=` $nhour ${eymdh} ${refymdh}`
      grep ${eymdh} ${netdir}/vfile.temp  >${netdir}/vrec.atcf_data.temp
      check_temp=` cat ${netdir}/vrec.atcf_data.temp | wc -l`
      echo "$fhr"                         >${netdir}/vrec.fhr.temp
      if [ ${check_temp} -gt 0 ]; then
        paste ${netdir}/vrec.atcf_data.temp \
              ${netdir}/vrec.fhr.temp      >>${netdir}/vfile.flipped
      fi
      eymdh=`$ndate -${hrint} $eymdh`
    done

    cat ${netdir}/vfile.flipped | sort -k1 -n | tee ${netdir}/vfile.sorted 
    cat ${netdir}/vfile.sorted | awk '{printf ("%4d  %3d  %4d\n",$10,$8,$9)}' \
                       >${netdir}/xmgr_obs_intensity;;

  entire)

    # The key here is to make sure we include the current, analysis time.
    # If we just start from the beginning, then depending on the hour 
    # interval chosen, we might not actually plot the verification for 
    # the current time.  So we need to do this in 2 parts.  The first 
    # part is to get from the current time back to the beginning.  The
    # next part is to get from the current time to the end.

    rm ${netdir}/vfile.flipped
    refymdh=${ymdh}
    eymdh=${ymdh}
    while [ ${eymdh} -ge ${iymdh} ]
    do
      fhr=` $nhour ${eymdh} ${refymdh}`
      grep ${eymdh} ${netdir}/vfile.temp  >${netdir}/vrec.atcf_data.temp
      check_temp=` cat ${netdir}/vrec.atcf_data.temp | wc -l`
      echo "$fhr"                         >${netdir}/vrec.fhr.temp
      if [ ${check_temp} -gt 0 ]; then
        paste ${netdir}/vrec.atcf_data.temp \
              ${netdir}/vrec.fhr.temp      >>${netdir}/vfile.flipped
      fi
      eymdh=`$ndate -${hrint} $eymdh`
    done

    cat ${netdir}/vfile.flipped | sort -k1 -n >${netdir}/vfile.begtonow

    rm ${netdir}/vfile.nowtoend
    refymdh=${ymdh}
    iymdh=${ymdh}
    while [ $iymdh -le $endymdh ]
    do
      fhr=` $nhour ${iymdh} ${refymdh}`
      grep ${iymdh} ${netdir}/vfile.temp  >${netdir}/vrec.atcf_data.temp
      check_temp=` cat ${netdir}/vrec.atcf_data.temp | wc -l`
      echo "$fhr"                         >${netdir}/vrec.fhr.temp
      if [ ${check_temp} -gt 0 ]; then
        paste ${netdir}/vrec.atcf_data.temp \
              ${netdir}/vrec.fhr.temp      >>${netdir}/vfile.nowtoend
      fi
      iymdh=`$ndate $hrint $iymdh`
    done

    cat ${netdir}/vfile.begtonow ${netdir}/vfile.nowtoend | sort -k1 -n | \
                                 tee ${netdir}/vfile.sorted
    cat ${netdir}/vfile.sorted | awk '{printf ("%4d  %3d  %4d\n",$10,$8,$9)}' \
                       >${netdir}/xmgr_obs_intensity;;

  *)

    >${netdir}/vfile.sorted
    refymdh=${iymdh} 
    while [ $iymdh -le $endymdh ]
    do
      fhr=` $nhour ${iymdh} ${refymdh}`
      grep ${iymdh} ${netdir}/vfile.temp  >${netdir}/vrec.atcf_data.temp
      check_temp=` cat ${netdir}/vrec.atcf_data.temp | wc -l`
      echo "$fhr"                         >${netdir}/vrec.fhr.temp
      if [ ${check_temp} -gt 0 ]; 
      then
        paste ${netdir}/vrec.atcf_data.temp ${netdir}/vrec.fhr.temp | \
              tee -a ${netdir}/vfile.sorted
      fi
      iymdh=`$ndate $hrint $iymdh`
    done
    cat ${netdir}/vfile.sorted | awk '{printf ("%4d  %3d  %4d\n",$10,$8,$9)}' \
                       >${netdir}/xmgr_obs_intensity;;

esac
