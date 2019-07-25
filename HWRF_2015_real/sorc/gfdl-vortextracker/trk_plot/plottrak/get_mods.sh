#!/bin/ksh

#set -x

# This script was written by Tim Marchok and is part of the 
# GrADS ATCF track & intensity plotting script.  If there are
# any problems, contact timothy.marchok@noaa.gov
#

if [ $# -lt 1 ]; then 
  echo " "
  echo " !!! ERROR: You must enter at least 5 arguments:"
  echo " !!!"
  echo " !!! USAGE: `basename $0` -t fcst_type -d ymdh -f atcfname -u userid -i trk_int_flag -model1 .... [model(n)]"
  echo " "
  echo "     fcst_type = single, mult06, mult12, mult24"
  echo "     ymdh      = In format of 2001091012, 2002081400, etc...."
  echo "     atcfname  = In format of al042001, ep112002, etc...."
  echo "     userid    = tpm, mb, etc...."
  echo "     trk_int_flag = track,trkinset,intensity"
  echo "     model     = GFDL, LBAR, OFCL, AVNO, etc....."
  echo " "
  exit 8
fi

#ps

while getopts t:f:d:u:i: opt
do
  case $opt in
    t) ftype="$OPTARG";;
    d) ymdh="$OPTARG";;
    f) afile="$OPTARG";;
    u) userid="$OPTARG";;
    i) trk_int_flag="$OPTARG";;
    \?) err=1;;
  esac
done

rundir="/gpfs/blhome/${USER}/HWRF/src/gfdl-vortextracker/trk_plot/plottrak/"
netdir="/gpfs/blhome/${USER}/HWRF/src/gfdl-vortextracker/trk_plot/plottrak/tracks"
ndate=/gpfs/blhome/${USER}/HWRF/src/gfdl-vortextracker/trk_plot/plottrak/ndate.ksh 
nhour=/gpfs/blhome/${USER}/HWRF/src/gfdl-vortextracker/trk_plot/plottrak/nhour.ksh

# echo " "
# echo "After while loop, ftype= $ftype"
# echo "                  afile= $afile"
# echo "                   ymdh= $ymdh"
# echo " "

let shiftnum=OPTIND-1
shift $shiftnum

modlist=""

#---------------------------------
# Process the list of models that
# were entered on the input line.

modct=0
while [ $# -gt 0 ] 
do

  let modct=modct+1
  model=$1

  modlist="$modlist $model"

  shift

done

# echo " "
# echo "After while loop, modlist follows: "
# echo " "
# echo "$modlist"
# echo " "

#--------------------------------
# grep out the dates we'll be using
# and then grep the models out of 
# there and into a temp flist file.

atcffile=a${afile}.dat

if [ -s ${netdir}/tempfile2 ]; then
  rm    ${netdir}/tempfile2
  touch ${netdir}/tempfile2
fi

if [ ${ftype} = 'single' ]; then

  # For single forecast times, we can just parse out the max wind and
  # min pressure data right now....

  grep ${ymdh} ${atcffile} >${netdir}/tempfile

  if [ ${trk_int_flag} != "track" ]; then
    >${netdir}/xmgr_fcst_intensity
  fi

  for model in $modlist
  do

    grep -i "${model}," ${netdir}/tempfile |\
      awk '{  
              initymdh = substr($0,9,10)
              cmodel   = substr($0,25,4)
              fhr      = substr($0,31,3)

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

              printf ("%10s  %4s  %3d  %5.1f  %5.1f  %2s%2s%2s  %3d  %3d  %4d\n",
              initymdh,
              cmodel,
              fhr,
              ylat,
              xlon,
              substr($0,1,2),substr($0,5,2),substr($0,11,2),
              fhr,
              maxwind,
              minprs)
      }' | sort -k3 -n >${netdir}/tempfile2

    if [ ${trk_int_flag} = "track" ]; then 
      cat ${netdir}/tempfile2
    else
      cat ${netdir}/tempfile2
      cat ${netdir}/tempfile2 | \
         awk '{printf ("%3d  %3d  %4d  %4s\n",$7,$8,$9,$2)}' \
         >>${netdir}/xmgr_fcst_intensity
      echo "     "  >> ${netdir}/xmgr_fcst_intensity
    fi

  done 

else

  # For multiple forecast times, we do the initial processing, then, if
  # it's an intensity plot, we need to read through and re-calculate 
  # (actually, shift) the forecast lead times so that they fit into our 
  # modified 0-360 x-axis.

  if [ $modct -gt 1 ]; then
    echo " "
    echo "!!! ERROR: For displaying multiple forecasts from multiple"
    echo "!!!        times, you can only pick one model, and you"
    echo "!!!        have picked ${modct}:"
    echo "!!!     "
    echo "!!!        $modlist"
    echo " "
    exit 8
  fi

  grep -i "${model}," ${atcffile} |\
    awk '{ 
           initymdh = substr($0,9,10)
           cmodel   = substr($0,25,4)
           fhr      = substr($0,31,3)

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

           printf ("%10s  %4s  %3d  %5.1f  %5.1f  %2s%2s%2s  %3d  %3d  %4d\n",
           initymdh,
           cmodel,
           fhr,
           ylat,
           xlon,
           substr($0,1,2),substr($0,5,2),substr($0,11,2),
           fhr,
           maxwind,
           minprs)
     }' >${netdir}/tempfile

  begymdh=${ymdh}
  endymdh=` cat ${netdir}/tempfile |\
             sort -k1              |\
             tail -1               |\
             awk '{print $1}'`

  case $ftype in
    "mult06") hrint=6;;
    "mult12") hrint=12;;
    "mult24") hrint=24;;
  esac

  if [ -s ${netdir}/tempfile2 ]; then 
    rm    ${netdir}/tempfile2
    touch ${netdir}/tempfile2
  fi

  iymdh=${begymdh}
  while [ $iymdh -le $endymdh ]
  do
    grep ${iymdh} ${netdir}/tempfile | \
      sort -k3 -n >>${netdir}/tempfile2
    iymdh=`$ndate $hrint $iymdh`
  done

  if [ ${trk_int_flag} = "track" ]; then

    cat ${netdir}/tempfile2

  else

    >${netdir}/xmgr_fcst_intensity

    oldymdh=${begymdh}
    fhr_offset=0

    while read inprec
    do
    
      iword=1
      for word in $inprec
        do
        if [ $iword -eq 1 ]; then iymdh=$word; fi
        if [ $iword -eq 2 ]; then cmodel=$word; fi
        if [ $iword -eq 3 ]; then fhr=$word; fi
        if [ $iword -eq 4 ]; then ylat=$word; fi
        if [ $iword -eq 5 ]; then xlon=$word; fi
        if [ $iword -eq 6 ]; then atcfid=$word; fi
        if [ $iword -eq 7 ]; then stdfhr=$word; fi
        if [ $iword -eq 8 ]; then maxwind=$word; fi
        if [ $iword -eq 9 ]; then minprs=$word; fi
        let iword=iword+1
      done

      if [ ${iymdh} -ne ${oldymdh} ]; then

        echo "             "  >> ${netdir}/xmgr_fcst_intensity

        # If it's a new initial time, calculate the number of hours 
        # between our first initial time and this initial time.  This
        # will be our new forecast hour offset.

        fhr_offset=` $nhour ${iymdh} ${begymdh}`
        oldymdh=${iymdh}

      fi

#      (( adjfhr = fhr_offset + stdfhr ))

      let adjfhr=fhr_offset+stdfhr

      echo $adjfhr $maxwind $minprs $cmodel >> ${netdir}/xmgr_fcst_intensity

      echo $iymdh $cmodel $fhr $ylat $xlon $atcfid $adjfhr $maxwind $minprs | \
      awk '
        {
         printf ("%10s  %4s  %3d  %5.1f  %5.1f  %6s  %3d  %3d  %4d\n",
                 $1,$2,$3,$4,$5,$6,$7,$8,$9)
        }'

    done < ${netdir}/tempfile2

  fi

fi
