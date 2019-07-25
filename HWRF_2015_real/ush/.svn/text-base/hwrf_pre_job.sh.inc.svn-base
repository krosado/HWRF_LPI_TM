# This is a Posix SH include file that should be sourced at the top of
# every job.  It initializes the environment for various clusters so
# that the rest of the HWRF or Rocoto can run properly.

if [[ "Q${HWRF_FORCE_TMPDIR:-}" != "Q" ]] ; then
    if [[ ! -d "$HWRF_FORCE_TMPDIR" ]] ; then
        if ( mkdir -p "$HWRF_FORCE_TMPDIR" ) ; then
            :  # trick to avoid triggering a set -e during a race condition
        fi
    fi
    export TMPDIR="$HWRF_FORCE_TMPDIR"
fi

if [[ "QQ$WHERE_AM_I" == QQ ]] ; then
   if [[ -d /lfs3 ]] ; then
       export WHERE_AM_I=jet
   elif [[ -d /scratch3 ]] ; then
       if ( grep E5-2690 /proc/cpuinfo > /dev/null 2>&1 ) ; then
           export WHERE_AM_I=theia
       else
           export WHERE_AM_I=zeus
       fi
   elif [[ -d /dcom && -d /hwrf ]] ; then
       export WHERE_AM_I=wcoss
   elif [[ -d /glade ]] ; then
       export WHERE_AM_I=yellowstone
   fi
fi

moduleuse=$HOMEhwrf/modulefiles/$WHERE_AM_I
case "${WHERE_AM_I:-unknown}" in
    jet)          . /apps/lmod/lmod/init/sh ;;
    zeus)         . /usr/share/Modules/init/sh ;;
    ttfri)        . /opt/modules/Modules/3.2.9/init/sh ;;
    yellowstone)
        . /usr/share/Modules/init/sh
        export PATH=/glade/p/work/strahan/rocoto/hwrf-rocoto/bin:$PATH
        moduleuse=no
        ;;
    wcoss*)
        moduleuse=$HOMEhwrf/modulefiles/wcoss
        . /usrx/local/Modules/default/init/sh 
        module purge
        ;;
    theia)        : ;;
    *)
        echo "WARNING: UNSUPPORTED SYSTEM $WHERE_AM_I" 1>&2
esac

if [[ "$moduleuse" != no ]] ; then
    module use $moduleuse 
    
    if [[ "${GSI_DOMAIN:-}QQ" == QQ ]] ; then
        echo NOT A GSI JOB - LOADING HWRF/run
        module load HWRF/run
    else
        echo GSI JOB - LOAD HWRF/rungsi
        module load HWRF/rungsi
    fi
fi

if [[ "${QUIET_PRE_JOB:-NO}" != YES ]] ; then
    if [[ "$WHERE_AM_I" == zeus || "$WHERE_AM_I" == jet || "$WHERE_AM_I" == ttfri ]] ; then 
        echo ====================PBS_NODEFILE CONTENTS===================
        ( set +eu ; cat "$PBS_NODEFILE" ; /bin/true )
    fi
    echo =======================/proc/meminfo========================
    cat /proc/meminfo
    echo ========================ps auxwww===========================
    ps auxwww
    echo ===========================env==============================
    env
    echo ======================df -h /dev/shm========================
    df -h /dev/shm
    echo ========================ulimit -a===========================
    ulimit -a
    echo ============================================================
fi
