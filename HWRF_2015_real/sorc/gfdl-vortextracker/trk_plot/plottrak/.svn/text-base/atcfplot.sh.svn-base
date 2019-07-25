#!/bin/ksh

# This script was written by Tim Marchok and is part of the
# GrADS ATCF track & intensity plotting script.  If there are
# any problems, contact timothy.marchok@noaa.gov
#
# This script prepares the user's environment to use GrADS v1.8
# for linux, which is what is needed for the GrADS interactive 
# script that displays the hurricane data.
#
# USAGE:  trakplot.sh <yyyy> <basin>
#
#         where basin = "al", "ep" or "wp"  (al is the default)
#

arg1=$1

if [ $# -eq 0 ]; then
  echo " "
  echo " +++ NOTE: User did not enter any arguments"
  echo " +++"
  echo " +++       Current year of `date +%Y` will be used for the year."
  echo " +++       Atlantic basin (al) will be used for the input basin."
  echo " "
  yyyy=`date +%Y`
else
  if [ ${arg1} = "h" -o ${arg1} = "-h" -o ${arg1} = "help" -o ${arg1} = "-help" ]; then
    echo " "
    echo " +++ USAGE: `basename $0` <yyyy> <inp_basin>"
    echo " "
    echo "     yyyy  = 4-digit year (Default is current year)"
    echo " "
    echo "     inp_basin = al, ep, cp, wp, si (Default is al)"
    echo " "
    exit 0
  else
    yyyy=$1
  fi
fi

#----------------------------------------------------------

inpbas=$2

holdinput=${inpbas}

if [ ${#inpbas} -eq 0 ]; then
  basin=al
else
  inpbas=` echo ${inpbas} | tr '[A-Z]' '[a-z]'`
  if [ ${inpbas} = 'al' -o ${inpbas} = 'atl' ]; then
    basin=al
  elif [ ${inpbas} = 'ep' -o ${inpbas} = 'epac' ]; then
    basin=ep
  elif [ ${inpbas} = 'wp' -o ${inpbas} = 'wpac' ]; then
    basin=wp
  elif [ ${inpbas} = 'si' -o ${inpbas} = 'sio' ]; then
    basin=si
  else
    set +x
    echo " "
    echo " !!! ERROR: You entered an input basin that is not"
    echo " !!!        Atlantic (al), Eastern Pacific (ep),"
    echo " !!!        Western Pacific (wp), or South Indian Ocean (si)."
    echo " !!! "
    echo " !!!    YOU ENTERED --->${holdinput}<---"
    echo " !!! "
    echo " !!! EXITING... "
    echo " !!! "
    echo " !!! USAGE: `basename $0` <yyyy> <basin>"
    echo " !!!         where basin = "al", "ep", "wp", or "si"  (al is the default)"
    echo " !!! "
    echo " "
    exit 8
  fi
fi

echo " "
echo "+++ Basin to be used is ${basin}"
echo " "

#--------------------------------------------------

export gradsv2=/contrib/grads/bin/gradsc
export GADDIR=/contrib/grads/lib

export scrdir=/gpfs/blhome/${USER}/HWRF/src/gfdl-vortextracker/trk_plot/plottrak
export plotdir=/gpfs/blhome/${USER}/HWRF/src/gfdl-vortextracker/trk_plot/plottrak/tracks

echo "$LOGNAME `date`" >>${scrdir}/use.log

cd $scrdir

if [ ! -d $plotdir ]; then
  mkdir -p $plotdir
fi

${gradsv2} -cl "run atcfplot.gs ${LOGNAME} ${basin} ${yyyy}"
