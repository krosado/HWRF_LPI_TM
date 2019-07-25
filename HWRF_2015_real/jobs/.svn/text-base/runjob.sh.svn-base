#! /bin/sh

# This script is run manually by EMC personnel to test the J-Jobs.  It
# sets the bare minimum number of environment variables necessary to
# run the J-Jobs in the HWRF model.

set -ue
EXPT=test-jjobs  # do not export
base=/ptmpp2/$USER/${EXPT}-com   # do not export

set +e
mkdir -p "$base"
mkdir -p "$base"
mkdir -p "$base"
set -e

# do not export these
ymdh=$1
ymdhm6=$( /nwprod/util/exec/ndate -6 $ymdh )
cycm6=${ymdhm6:8:2}
LOGhwrf=$base/logs   # do not export

# Must export these though:
export storm_num=$2
export cyc=${ymdh:8:2} PDY=${ymdh:0:8}
export COMIN=$base/hwrf.${ymdh}
export COMOUT=$base/hwrf.${ymdh}
export HISTDATA=$base/hwrf.${ymdhm6}
export DATA=$base/hwrf${storm_num}_${cyc}_prod
export mesagdir=$base/messages/$PDY$cyc
export jlogfile=$LOGhwrf/jlogfile
export HOMEhwrf=/hwrf/save/$USER/${EXPT}

# Set up and export non-NCO track delivery override variables:
export ATCFdir=$base/ATCFdir
export gltrkdir=$base/gltrkdir
export HWRF_TRACK_EMAIL_LIST="$USER@noaa.gov"
if ( echo "$HWRF_TRACK_EMAIL_LIST" | grep -E 'emc.*para@noaa.gov' ) ; then
    echo "Invalid email address: $HWRF_TRACK_EMAIL_LIST"
    echo "Edit your \$HWRF_TRACK_EMAIL_LIST when using para accounts"
    exit 99
fi
export HWRF_EMAIL_SSH_MACHINE=$( hostname|cut -c1-1 )20a1  # g20a1/t20a1

# do not export these either:
stormlabel=storm${storm_num}
JJOB=$( echo "$3"|tr a-z A-Z )
jjob=$( echo "$3"|tr A-Z a-z )
js=${stormlabel}_$jjob
jhwrf=$HOMEhwrf/jobs/JHWRF_$JJOB

# Set some vars and stdout/err log based on arguments:
case "$JJOB" in
    LAUNCH|BUFRPREP|MERGE|OCEAN_INIT|PRODUCTS|POST|UNPOST) : ;;
    OUTPUT|ENSDA_PRE|ENSDA_OUTPUT|GSI_POST) : ;;
    FORECAST)
        echo 'DID YOU REMEMBER TO EDIT THE JJOB FOR NODE CONFIG?'
        echo 'RUN_COUPLED=YES => -n 528'
        echo 'RUN_COUPLED=NO  => -n 504'
        echo '(hit enter to continue or control-C to abort)'
        read nothing
        ;;
    GSI)
        echo 'DID YOU REMEMBER TO EDIT THE JJOB FOR NODE CONFIG?'
        echo 'd02 => -n 60 ptile=12 affinity=core(2)'
        echo 'd03 => -n 120 ptile=24 affinity=core'
        echo '(hit enter to continue or control-C to abort)'
        read nothing
        export GSI_DOMAIN=$( echo $4 | tr A-Z a-z )
        js=${stormlabel}_gsi_${GSI_DOMAIN}
        ;;
    ENSDA)
        export ENSDA_MEMB="$4"
        if [[ ! ( "$ENSDA_MEMB" -gt 0 ) &&
                ( "$ENSDA_MEMB" -lt 41 ) ]] ; then
            echo "ENSDA_MEMB must be 1..40"
            exit 99
        fi
        js=$( printf "%s_ensda_mem%02d" ${stormlabel} ${ENSDA_MEMB} )
        ;;
    INIT) 
        echo 'DID YOU REMEMBER TO EDIT THE JJOB?'
        echo 'BDY   => -n 24 -W 00:40'
        echo '3DVAR => -n 48 -W 00:30'
        echo '(hit enter to continue or control-C to abort)'
        read nothing
        export INIT_PARTS="$4"
        if [[ "$INIT_PARTS" == BDY ]] ; then
            export INIT_FHR=0 INIT_MODEL=GFS
            js=${stormlabel}_bdy
        elif [[ "$INIT_PARTS" == 3DVAR ]] ; then
            export INIT_FHR="$5"
            if [[ "$INIT_FHR" -eq 0 ]] ; then
                export INIT_MODEL=GFS
                js=${js}_gfs
            else
                export INIT_MODEL=GDAS1
                js=${js}_gdas_$INIT_FHR
            fi
        else
            echo bad init parts: $INIT_PARTS
            exit 99
        fi
        ;;
    RELOCATE)
        export INIT_FHR="${4:-0}"
        if [[ "$INIT_FHR" -eq 0 ]] ; then
            export INIT_MODEL=GFS
            js=${js}_gfs
        else
            export INIT_MODEL=GDAS1
            js=${js}_gdas_$INIT_FHR
        fi
        ;;
    *) echo script does not know how to run JHWRF_$JJOB yet
        exit 99
esac

# Print what we're doing and run bsub:
echo $jhwrf
set -x
exec bsub -oo $LOGhwrf/${ymdh}_$js.log -eo $LOGhwrf/${ymdh}_$js.log < $jhwrf