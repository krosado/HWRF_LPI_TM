#! /bin/ksh

@** if JJOBNAME==README

########################################################################
# The jobs/JHWRF* files of past years were extremely repetative which
# led to many errors on the part of both EMC and NCO during the
# operational transition of the HWRF.  Coders would often forget to
# make a change to one or more of the J-Job files, all of which should
# be nearly identical.  Since the J-Jobs are almost the same, we now
# generate them from this one file using the ush/hwrf_make_jobs.py
# script.  
#
# Run the ush/hwrf_make_jobs.py to create all jobs/JHWRF_* job files.
# That script parses this file with various values for JJOBNAME, using
# the produtil.atparser.ATParser parser.  If you look through this
# file, you will see "@**if" statements (and @**elseif and @**else)
# which insert different text for each J-Job file, based on the
# JJOBNAME variable.  That variable contains the name of the J-Job
# file, minus the "jobs/JHWRF" part.
########################################################################

@** elseif JJOBNAME==GSI
# -- see below for #BSUB job cards --
:
# JHWRF_@[JJOBNAME] - this job card is used for two different jobs:
# the 2km and 6km GSI.  It can only be run for the North Atlantic
# basin.  You tell the JHWRF_@[JJOBNAME] which domain is being run
# with the GSI_DOMAIN environment variable:
#
#    GSI_DOMAIN=d02
#    GSI_DOMAIN=d03
#
# Note that those are case-sensitive.
#
# Dependencies:
#    North Atlantic (L) basin.
#    All three JHWRF_RELOCATE jobs for INIT_MODEL=GDAS
#    The JHWRF_INIT job for INIT_MODEL=GFS INIT_PARTS=3DVAR
#    The JHWRF_BUFRPREP job
#
# Next jobs: JHWRF_GSI_POST and JHWRF_MERGE (after both GSI jobs are
# done)
####################################
# GSI_DOMAIN=d02
# #BSUB -J @[JJOBNAME]@[JOBMORE:+_${JOBMORE.uc}]
# #BSUB -P HWRF-T2O
# #BSUB -q @[PARQ]
# #BSUB -R affinity[core(2)]
# #BSUB -a poe
# #BSUB -n 60
# #BSUB -R span[ptile=12]
# #BSUB -W 00:29
# #BSUB -x
####################################
# GSI_DOMAIN=d03
# #BSUB -J @[JJOBNAME]@[JOBMORE:+_${JOBMORE.uc}]
# #BSUB -P HWRF-T2O
# #BSUB -q @[PARQ]
# #BSUB -R affinity[core]
# #BSUB -a poe
# #BSUB -n 120
# #BSUB -R span[ptile=24]
# #BSUB -W 00:29
# #BSUB -x
# export OMP_NUM_THREADS=1
####################################

# Make sure $GSI_DOMAIN is set:
set -u +x
echo "GSI_DOMAIN=$GSI_DOMAIN"
if [[ "$GSI_DOMAIN" != d02 && "$GSI_DOMAIN" != d03 ]] ; then
    echo "Wrong GSI_DOMAIN value; must be d02 or d03"
    exit 99
fi
set +u -x
@** elseif JJOBNAME==BUFRPREP
#BSUB -J @[JJOBNAME]@[JOBMORE:+_${JOBMORE.uc}]
#BSUB -P HWRF-T2O
#BSUB -q @[PARQ]
#BSUB -R affinity[core]
#BSUB -a poe
#BSUB -n 1
#BSUB -R span[ptile=1]
#BSUB -W 00:29
#BSUB -x
:
# JHWRF_@[JJOBNAME] - this job prepares the bufr data for the Tail
# Doppler Radar, if present.  It creates a GSI status file telling if
# the data is present or not.
#
# Dependencies:
#    North Atlantic (L) basin.
#    JHWRF_LAUNCH
#
# Next jobs: JHWRF_GSI for d02 and d03, after other dependencies are met
@** elseif JJOBNAME==ENSDA
#BSUB -J @[JJOBNAME]@[JOBMORE:+_${JOBMORE.uc}]
#BSUB -P HWRF-T2O
#BSUB -q @[PARQ]
#BSUB -R affinity[core]
#BSUB -a poe
#BSUB -n 48
#BSUB -R span[ptile=24]
#BSUB -W 00:49
#BSUB -x
:
# JHWRF_@[JJOBNAME] - this job runs one of the forty members of the
# HWRF six hour forecast ensemble.
#
# Dependencies:
#   North Atlantic (L) basin.
#   JHWRF_OUTPUT
#   JHWRF_ENSDA_PRE
#   "RUN_ENSDA=YES" in the $COMOUT/$stormlabel.run_ensda
#
# Next job: JHWRF_ENSDA_OUTPUT (after all forty JHWRF_ENSDA jobs are done)

set +u
echo "ENSDA member is $ENSDA_MEMB" # ensure this variable is set
set -u

@** elseif JJOBNAME==ENSDA_OUTPUT
#BSUB -J @[JJOBNAME]@[JOBMORE:+_${JOBMORE.uc}]
#BSUB -P HWRF-T2O
#BSUB -q @[SHAREQ]
#BSUB -R affinity[core]
#BSUB -a poe
#BSUB -n 1
#BSUB -R span[ptile=1]
#BSUB -W 00:05
#BSUB -M 1024
:
# JHWRF_@[JJOBNAME] - this job copies files to com that have not
# already been copied.  It creates a flag file to indicate that the
# ENSDA ran to completion.
#
# Dependencies:
#   JHWRF_ENSDA (all forty of them!)
#
# Next jobs: none!

@** elseif JJOBNAME==ENSDA_PRE
#BSUB -J @[JJOBNAME]@[JOBMORE:+_${JOBMORE.uc}]
#BSUB -P HWRF-T2O
#BSUB -q @[SHAREQ]
#BSUB -R affinity[core]
#BSUB -a poe
#BSUB -n 1
#BSUB -R span[ptile=1]
#BSUB -W 00:35
#BSUB -M 1024
:
# JHWRF_@[JJOBNAME] - this job decides if the HWRF ENSDA is to be run
# for this storm.
#
# Dependencies:
#   North Atlantic (L) basin.
#   JHWRF_OUTPUT
#   Six hours and thirty minutes (6:30) past the synoptic time.
#
# Next jobs: Forty JHWRF_ENSDA jobs, if ENSDA was enabled.
@** elseif JJOBNAME==FORECAST
# -- see below for #BSUB job cards --
:
# JHWRF_@[JJOBNAME] - this job runs the 126hr HWRF forecast, either
# coupled or uncoupled.  The coupled forecast requires one more
# compute node (-n 528) than the uncoupled (-n 504).  
#
# Runs in parallel with:
#   JHWRF_PRODUCTS
#   JHWRF_POST
#
# Dependencies:
#   JHWRF_OCEAN_INIT
#   JHWRF_INIT with INIT_MODEL=GFS and INIT_PARTS=BDY
#   If North Atlantic (L) basin:
#       JHWRF_MERGE
#   else:
#       JHWRF_RELOCATE with INIT_MODEL=GFS and INIT_FHR=0
#
# Next Job:   JHWRF_UNPOST
#
# NOTE:
#   If "RUN_COUPLED=YES" in $stormlabel.ocean_status, send 528 processors
#   If "RUN_COUPLED=NO" or missing, send 504 processors.
#
####################################
# When RUN_COUPLED=YES
# #BSUB -J @[JJOBNAME]@[JOBMORE:+_${JOBMORE.uc}]
# #BSUB -P HWRF-T2O
# #BSUB -q @[PARQ]
# #BSUB -R affinity[core]
# #BSUB -a poe
# #BSUB -n 528
# #BSUB -R span[ptile=24]
# #BSUB -W 02:00
####################################
# When RUN_COUPLED=NO or is missing
# #BSUB -J @[JJOBNAME]@[JOBMORE:+_${JOBMORE.uc}]
# #BSUB -P HWRF-T2O
# #BSUB -q @[PARQ]
# #BSUB -R affinity[core]
# #BSUB -a poe
# #BSUB -n 504
# #BSUB -R span[ptile=24]
# #BSUB -W 02:00
####################################
@** elseif JJOBNAME==GSI_POST
#BSUB -J @[JJOBNAME]@[JOBMORE:+_${JOBMORE.uc}]
#BSUB -P HWRF-T2O
#BSUB -q @[PARQ]
#BSUB -R affinity[core]
#BSUB -a poe
#BSUB -n 24
#BSUB -R span[ptile=24]
#BSUB -W 00:20
:
# JHWRF_@[JJOBNAME] - runs the post-processor on the GSI input and output
#
# Dependencies:
#   JHWRF_GSI for GSI_DOMAIN=d02
#   JHWRF_GSI for GSI_DOMAIN=d03
#   JHWRF_BUFRPREP
#
# Next jobs: none.
@** elseif JJOBNAME==INIT
# -- see below for #BSUB job cards --
:
# JHWRF_@[JJOBNAME] - this job runs the WPS, prep_hybrid, real and
# other small HWRF initialization programs on either GFS or GDAS data.
# It is controlled by three environment variables:
#   INIT_MODEL = GFS or GDAS
#   INIT_FHR = 0 3 6 or 9
#   INIT_PARTS = 3DVAR or BDY
#
# Jobs and Dependencies:
#
# Five of these jobs are run, and all require the JHWRF_LAUNCH.  In
# addition, each has one more dependency:
#
#  MODEL FHR PARTS | DEPENDENCIES
# ---------------------------------------------------------------
#   GFS   0  3DVAR | GFS hour 0 forecast files available
#   GFS   0   BDY  | JHWRF_INIT for MODEL=GFS, FHR=0, PARTS=3DVAR
#  GDAS   3  3DVAR | GDAS hour 3 forecast files available
#  GDAS   6  3DVAR | GDAS hour 3 forecast files available
#  GDAS   9  3DVAR | GDAS hour 3 forecast files available
####################################
# When INIT_PARTS=3DVAR
# #BSUB -J @[JJOBNAME]@[JOBMORE:+_${JOBMORE.uc}]
# #BSUB -P HWRF-T2O
# #BSUB -q @[PARQ]
# #BSUB -R affinity[core]
# #BSUB -a poe
# #BSUB -n 48
# #BSUB -R span[ptile=24]
# #BSUB -W 00:30
####################################
# When INIT_PARTS==BDY
# #BSUB -J @[JJOBNAME]@[JOBMORE:+_${JOBMORE.uc}]
# #BSUB -P HWRF-T2O
# #BSUB -q @[PARQ]
# #BSUB -R affinity[core]
# #BSUB -a poe
# #BSUB -n 24
# #BSUB -R span[ptile=24]
# #BSUB -W 00:30
####################################

set +u
echo "Checking mandatory init job variables:"
echo "INIT_MODEL=$INIT_MODEL"
echo "INIT_FHR=$INIT_FHR"
echo "INIT_PARTS=$INIT_PARTS"
set -u
@** elseif JJOBNAME==LAUNCH
#BSUB -J @[JJOBNAME]@[JOBMORE:+_${JOBMORE.uc}]
#BSUB -P HWRF-T2O
#BSUB -q @[SHAREQ]
#BSUB -R affinity[core]
#BSUB -a poe
#BSUB -n 1
#BSUB -R span[ptile=1]
#BSUB -W 00:05
#BSUB -M 1024
:
# JHWRF_@[JJOBNAME] - this job replaces the JHWRF_PRE_MASTER and
# JHWRF_PRE_ATMOS of previous years.  It prepares the initial
# directory structure, configures the model for the chosen ocean basin
# and forecast center, and produces a few initial files.  All later
# jobs are dependent on this one.  
#
# Dependencies:
#    GDAS hour 3 forecast files available (different trigger than last year!)
#    SDM has run the ../ush/setup_hurricane script
#
# Next job: depends on basin.
#  All basins:
#    JHWRF_INIT for INIT_MODEL=GFS, INIT_PARTS=3DVAR, INIT_FHR=0
#    JHWRF_OCEAN_INIT
#  North Atlantic (L) basin only:
#    JHWRF_INIT for INIT_MODEL=GDAS, INIT_PARTS=3DVAR, INIT_FHR=3
#    JHWRF_INIT for INIT_MODEL=GDAS, INIT_PARTS=3DVAR, INIT_FHR=6
#    JHWRF_INIT for INIT_MODEL=GDAS, INIT_PARTS=3DVAR, INIT_FHR=9
#    JHWRF_BUFRPREP

@** elseif JJOBNAME==OUTPUT
#BSUB -J @[JJOBNAME]@[JOBMORE:+_${JOBMORE.uc}]
#BSUB -P HWRF-T2O
#BSUB -q @[SHAREQ]
#BSUB -R affinity[core]
#BSUB -a poe
#BSUB -n 1
#BSUB -R span[ptile=1]
#BSUB -W 00:15
#BSUB -M 1024
:
# JHWRF_@[JJOBNAME] - this job copies files to com that have not
# already been copied.  It also emails the AFOS file to the SDM.
#
# Dependencies:
#   JHWRF_PRODUCTS
#   JHWRF_POST (all of them, if you run multiple)
#   JHWRF_FORECAST (coupled or uncoupled)
#
# Next jobs: JHWRF_ENSDA_PRE, after other dependencies are met.

@** elseif JJOBNAME==UNPOST
#BSUB -J @[JJOBNAME]@[JOBMORE:+_${JOBMORE.uc}]
#BSUB -P HWRF-T2O
#BSUB -q @[SHAREQ]
#BSUB -R affinity[core]
#BSUB -a poe
#BSUB -n 1
#BSUB -R span[ptile=1]
#BSUB -W 00:05
#BSUB -M 1024
:
# JHWRF_@[JJOBNAME] - this job marks all post-processing and product
# delivery as incomplete so that the JHWRF_POST and JHWRF_PRODUCTS
# jobs will redo all of their work if they are resubmitted.  In a
# normal situation, this job does nothing - the work already is marked
# "incomplete" by the JHWRF_LAUNCH.  However, if the post-processing
# needs to be entirely redone, then one must submit THIS job before
# the post or products jobs.
#
# Dependencies:
#   JHWRF_FORECAST
# 
# Next jobs:
#   JHWRF_POST
#   JHWRF_PRODUCTS

@** elseif JJOBNAME==MERGE
#BSUB -J @[JJOBNAME]@[JOBMORE:+_${JOBMORE.uc}]
#BSUB -P HWRF-T2O
#BSUB -q @[PARQ]
#BSUB -R affinity[core(8)]
#BSUB -a poe
#BSUB -n 1
#BSUB -R span[ptile=1]
#BSUB -W 00:29
#BSUB -x
:
# JHWRF_@[JJOBNAME] -- merges the storm-removed large-scale fields
#     with the new vortex, possibly after GSI has processed the data.
#
# Dependencies - 
#   JHWRF_GSI (both of them)
#   All three JHWRF_RELOCATE with INIT_MODEL=GDAS

@** elseif JJOBNAME==RELOCATE
#BSUB -J @[JJOBNAME]@[JOBMORE:+_${JOBMORE.uc}]
#BSUB -P HWRF-T2O
#BSUB -q @[PARQ]
#BSUB -R affinity[core(8)]
#BSUB -a poe
#BSUB -n 1
#BSUB -R span[ptile=1]
#BSUB -W 00:29
#BSUB -x
:
# JHWRF_@[JJOBNAME]: Serves two purposes depending on INIT_MODEL=GFS
# or GDAS.
#   INIT_MODEL=GDAS, INIT_FHR=3, 6, or 9 - relocates the GDAS vortex
#     for GDAS hour 3, 6 or 9.  You must set INIT_FHR to 3 6 or 9 before
#     calling.  Uses WRF output and parent model track from the
#     JHWRF_INIT job for the same INIT_FHR, as well as the prior cycle's
#     vortex at forecast hours 3, 6 or 9.
#   INIT_MODEL=GFS, INIT_FHR=0 - relocates the GFS vortex
#
# Note that the INIT_MODEL=GDAS ones are only run for the North
# Atlantic (L) basin.
#
# Dependencies:
#   1. JHWRF_INIT with INIT_PARTS=3DVAR, INIT_MODEL=GFS and INIT_FHR=0
#   2. The JHWRF_INIT job with INIT_PARTS=3DVAR and INIT_MODEL and
#      INIT_FHR set the same as for the JHWRF_RELOCATE job
#   3. If INIT_MODEL=GDAS, then this must be a North Atlantic (L)
#      basin storm.

set +u
echo "Checking mandatory relocate job variables:"
echo "INIT_MODEL=$INIT_MODEL"
echo "INIT_FHR=$INIT_FHR"
set -u
@** elseif JJOBNAME==OCEAN_INIT
#BSUB -J @[JJOBNAME]@[JOBMORE:+_${JOBMORE.uc}]
#BSUB -P HWRF-T2O
#BSUB -q @[PARQ]
#BSUB -R affinity[core]
#BSUB -a poe
#BSUB -n 9
#BSUB -R span[ptile=9]
#BSUB -W 00:59
#BSUB -x
:
# JHWRF_@[JJOBNAME] -- this job produces input files needed by the
#    JHWRF_FORECAST job to initialize the POM ocean model.  It
#    also determines whether coupling should be done.  It passes
#    that information through the ksh-readable ocean_status files:
#
#       ${COMOUT}/ocean_status.${STORM}${STORMID}.${PDY}${cyc}
#       ${COMOUT}/$stormlabel.ocean_status
#   
#    which sets RUN_COUPLED=YES when coupling should be done.
#    The second file is new this year: it is the same as the first,
#    but has a filename that can be predicted in advance.
#
# Dependency:
#    JHWRF_LAUNCH
# 
# Next jobs:
#    JHWRF_FORECAST
@** elseif JJOBNAME==POST
#BSUB -J @[JJOBNAME]@[JOBMORE:+_${JOBMORE.uc}]
#BSUB -P HWRF-T2O
#BSUB -q @[PARQ]
#BSUB -R affinity[core]
#BSUB -a poe
#BSUB -n 24
#BSUB -R span[ptile=24]
#BSUB -W 02:20
:
# JHWRF_@[JJOBNAME] - Converts the WRF output files to E grid GRIB
# files.  If this job fails, and you resubmit it, it will restart
# where it left off instead of rerunning everything.  To rerun all
# post-processing, resubmit the JHWRF_UNPOST instead.
#
# Runs in parallel with:
#   JHWRF_PRODUCTS
#   JHWRF_FORECAST
#
# Dependencies:
#   JHWRF_UNPOST
#
# Next jobs:
#   JHWRF_OUTPUT (after all of its other dependencies are met)

@** elseif JJOBNAME==PRODUCTS
#BSUB -J @[JJOBNAME]@[JOBMORE:+_${JOBMORE.uc}]
#BSUB -P HWRF-T2O
#BSUB -q @[PARQ]
#BSUB -R affinity[core]
#BSUB -a poe
#BSUB -n 9
#BSUB -R span[ptile=9]
#BSUB -W 02:20
:
# JHWRF_@[JJOBNAME] - Converts the JHWRF_POST's E grid GRIB files to
# lat-lon GRIB2 files, runs the tracker, and copies WRF native files
# to com as compress NetCDF4.  If this job fails, and you resubmit it,
# it will restart where it left off instead of rerunning everything
# (except for the tracker).  To rerun all post-processing, resubmit
# the JHWRF_UNPOST instead.
#
# Runs in parallel with:
#   JHWRF_POST
#   JHWRF_FORECAST
#
# Dependencies:
#   JHWRF_UNPOST
#
# Next jobs:
#   JHWRF_OUTPUT (after all of its other dependencies are met)
@** else
@** abort INVALID JJOB NAME @[JJOBNAME]
@** endif

########################################################################

# Setup shell:
set -xa
date
export PS4='$SECONDS + '

echo "load environment"
. /usrx/local/Modules/default/init/ksh
module purge
@** if JJOBNAME==GSI
module load ibmpe/1.3.0.8p
@** else
module load ibmpe   # NOTE: GSI jobs use ibmpe/1.3.0.8p instead
@** endif
module load ics/15.0.1 nco/4.4.4 HDF5/1.8.9/serial NetCDF/4.2/serial
module load PNetCDF/1.5.0 lsf hpss
module load prod_util grib_util

####################################
# obtain unique process id (pid) and make temp directory
####################################
export DATA=${DATA:-${DATAROOT}/hwrf${storm_num}_${cyc}_${envir}}
@** if JJOBNAME==LAUNCH
# The database is reset after this job, so intermediate files must be
# deleted as well, or later jobs get confused:
rm -rf "$DATA"
@** endif
mkdir -p "$DATA"
cd $DATA

export cycle=t${cyc}z pid=$$ pgmerr=errfile
export job="${job:-JHWRF_@[JJOBNAME]${WORKER:+_$WORKER}}"

setup.sh
setpdy.sh
. ./PDY

# Make sure all mandatory variables are set: PDY, cyc, storm_num
set -u +x
echo "Checking for mandatory variables:"
echo "PDY=$PDY cyc=$cyc storm_num=$storm_num"
set +u -x

export MP_IOAGENT_CNT=all
export MP_IO_BUFFER_SIZE=8M

####################################
# Set default values if not yet set
# values from ecFlow or modules will override these
####################################
export PARAFLAG=${PARAFLAG:-NO} # set by ecFlow, should be NO
export envir=${envir:-prod} # set by ecFlow
export NWPROD=${NWPROD:-${NWROOT}}
export utilexec=${utilexec:-${UTILROOT}/exec} # set by prod_util module
export utilscript=${utilscript:-${UTILROOT}/ush} # set by prod_util module
export PARAFLAG=${PARAFLAG:-NO} # always "NO" for NCO
export gltrkdir=${gltrkdir:-${COMROOTp1}/hur/${envir}/global}
export ATCFdir=${ATCFdir:-${COMROOTp1}/nhc/${envir}/atcf}

###############################################################
# This block can be modified for different Production test
# environment. This is used for operational tests.
###############################################################
if [ ${PARAFLAG} == NO -a $envir != prod ]; then
  export SENDDBN=${SENDDBN:-YES}
  export jlogfile=${jlogfile:-${COMROOT}/logs/${envir}/jlogfile}
  export DBN_ALERT_TYPE=TBD_PARA
  export DBNROOT=/nwprod/spa_util/fakedbn
  export DBNLOG=${DBNLOG:-YES}
fi

####################################
# Specify NET and RUN Name and model
####################################
export NET=hur
export RUN=hwrf

####################################
# File To Log Msgs
####################################
export jlogfile=${jlogfile:-${COMROOT}/logs/jlogfiles/jlogfile.${job}.${pid}}

####################################
# Determine Job Output Name on System
####################################
export outid="LL$job"
export jobid="${outid}.o${pid}"
export pgmout="OUTPUT.${pid}"
export pgmerr=errfile

####################################
# SENDECF  - Flag Events on ECF
# SENDCOM  - Copy Files From TMPDIR to $COMOUT
# SENDDBN  - Issue DBNet Client Calls
####################################
export SENDECF=${SENDECF:-YES}
export SENDCOM=${SENDCOM:-YES}
export SENDDBN=${SENDDBN:-YES}
export EMAIL_SDM=YES

####################################
# Specify HWRF version number
####################################
export HWRF_VERSION=${HWRF_VERSION:-${hwrf_ver/v/}}

#NCO Testing
module use /nwpara2/modulefiles/
module load mpiserial/v1.1.0
export HWRF_TRACK_EMAIL_LIST=floyd.fayton@noaa.gov,sdm@noaa.gov
#FAF canned test
#export COMINGFS="${COMROOTp1}/gfs/test"
#export COMINGDAS="${COMROOTp1}/gfs/test"
#export DCOMROOT=/dcomdev/us007003
#export mesagdir=${mesagdir:-${COMROOT}/hur/${envir}/inphwrf/${PDY}${cyc}}
export mesagdir=${mesagdir:-${COMROOT}/hur/${envir}/inphwrf}
#export COMTPC=${COMTPC:-${COMROOTp1}/nhc/${envir}/storm-data/ncep}
#FAF setup_hurricane test, live storm
#export mesagdir=${mesagdir:-${COMROOT}/hur/test/inphwrf}
#FAF test, live storm
#export mesagdir=${mesagdir:-${COMROOTp1}/hur/prod/inpdata}

####################################
# Set COM variables and read holdvars
####################################
export HOMEhwrf=${HOMEhwrf:-${NWROOT}/hwrf.v$HWRF_VERSION}
export COMINGFS=${COMINGFS:-${COMROOTp1}/gfs/prod}
export COMINGDAS=${COMINGDAS:-${COMROOTp1}/gfs/prod}
export COMINARCH=${COMINARCH:-${COMROOTp1}/arch/prod/syndat}
export mesagdir=${mesagdir:-${COMROOT}/hur/${envir}/inphwrf}
export COMTPC=${COMTPC:-${COMROOTp1}/nhc/prod/storm-data/ncep}
export COMIN=${COMIN:-${COMROOT}/$NET/${envir}/$RUN.$PDY$cyc}
export COMOUT=${COMOUT:-${COMROOT}/$NET/${envir}/$RUN.$PDY$cyc}
export DCOM=${DCOM:-${DCOMROOT}}
priorymdh=$( $NDATE -6 "$PDY$cyc" )
export HISTDATA=${HISTDATA:-${COMROOT}/$NET/${envir}/$RUN.$priorymdh}
export stormlabel=${stormlabel:-storm$storm_num}
export CONFhwrf=$COMIN/$stormlabel.conf
@** if JJOBNAME!=LAUNCH
. "$COMIN/$stormlabel.holdvars.txt"
@** else
# Do not source the holdvars for the launch job since the holdvars
# does not exist yet!
# We do need to set these variables that holdvars normally sets:
export USHhwrf=$HOMEhwrf/ush
export EXhwrf=$HOMEhwrf/scripts
export PARMhwrf=$HOMEhwrf/parm
@** endif

####################################
# Set script-specific environment
####################################

@** if JJOBNAME==LAUNCH
abort_storm() {
    # Called when the script decides the storm will not run.  Must
    # abort the entire workflow in ecFlow.
    what="$1"
    why="$2"
    if [[ "$SENDECF" == "YES" ]] ; then
        # IMPORTANT NOTE TO SPA:
        # This code was copied from the HWRF 8.1.0 and will not match
        # the current system.
        NETNAME=`echo $ECF_NAME | sed "s+/hwrf${storm_num}/prep/jhwrf_launch++g"`
        ecflow_client --label $stormlabel "$what canceled: $why"
        ecflow_client --force complete recursive $NETNAME/hwrf$storm_num
    fi
    postmsg "$jlogfile" "$what $why"
    exit 0
}

label_storm() {
    # Called when the script decides the storm WILL run.
    what="$1"
    why="$2"
    if [[ "$SENDECF" == "YES" ]] ; then
        ecflow_client --label $stormlabel "$what will run: $why"
    fi
    postmsg "$jlogfile" "$what will run: $why"
}

nstorms=$( head -1 $mesagdir/nstorms ) # number of storms: 0-7
messagefile=$mesagdir/message$storm_num
message=$( head -1 "$messagefile" ) # full message contents
if [[ -z "$message" ]] ; then
    abort_storm "message$storm_num" "message file $messagefile is empty or missing"
fi
center=$( echo ${message:0:4} ) # echo strips the trailing space in "NHC "
storm=${message:5:3} # 93L
basin1=${message:7:1} # L
name=$( echo ${message:10:9} )
mYMDH=${PDY:0:2}${message:19:6}${message:26:2} # 2015110418

if [[ ! ( "$storm_num" -le "$nstorms" ) ]] ; then
    abort_storm "$storm $name" "I am storm $storm_num but only $nstorms storms are requested."
elif [[ "$mYMDH" != "$PDY$cyc" ]] ; then
    abort_storm "$storm $name" "message cycle $mYMDH is not current cycle $PDY$cyc"
fi

if [[ "$basin1" == L ]] ; then
    configmore=" "
    label_storm "run $storm $mYMDH using AL config"
elif [[ "$basin1" == E ]] ; then
    configmore="${PARMhwrf}/hwrf_EP.conf"
    label_storm "run $storm $mYMDH using EP config"
elif [[ "$basin1" == C ]] ; then
    configmore="${PARMhwrf}/hwrf_CP.conf"
    label_storm "run $storm $mYMDH using CP config"
else # Q basin or JTWC
    configmore="${PARMhwrf}/hwrf_JTWC.conf"
    label_storm "run $storm $mYMDH using JTWC config"
fi

if [[ "$SENDECF" == YES  && ( "$basin1" != L && "$basin1" != E )]] ; then
    postmsg "$jlogfile" "Not a North Atlantic storm.  Skipping GSI-related jobs."
    NETNAME=`echo $ECF_NAME | sed "s+/hwrf${storm_num}/prep/jhwrf_launch++g"`
    ecflow_client --event not_running_GSI
    ecflow_client --force complete $NETNAME/hwrf${storm_num}/analysis/jhwrf_fgat_relocate_gdas_f03
    ecflow_client --force complete $NETNAME/hwrf${storm_num}/analysis/jhwrf_fgat_relocate_gdas_f06
    ecflow_client --force complete $NETNAME/hwrf${storm_num}/analysis/jhwrf_fgat_relocate_gdas_f09
    ecflow_client --force complete $NETNAME/hwrf${storm_num}/prep/jhwrf_init_gdas_f03
    ecflow_client --force complete $NETNAME/hwrf${storm_num}/prep/jhwrf_init_gdas_f06
    ecflow_client --force complete $NETNAME/hwrf${storm_num}/prep/jhwrf_init_gdas_f09
else
    ecflow_client --event running_GSI
fi

# Add the contents of the $HWRF_MORE_CONF variable if it is specified.
# This allows ecFlow to send additional configuration settings:
if [[ "${HWRF_MORE_CONF:-}Q" != "Q" ]] ; then
    configmore="$configmore $HWRF_MORE_CONF"
fi

@** elseif JJOBNAME==ENSDA_PRE
run_ensda() {
    # This script is called to alert the running of all forty ENSDA jobs and the
    # ENSDA_OUTPUT job.
    if [[ "$SENDECF" == YES ]] ; then
        ecflow_client --event run_ensda
        # FIXME: NCO SPA: ENTER CODE HERE
    fi
}
cancel_ensda() {
    # This script is called to cancel all forty ENSDA jobs and the
    # ENSDA_OUTPUT job.
    if [[ "$SENDECF" == YES ]] ; then
        NETNAME=`echo $ECF_NAME | sed "s+/hwrf${storm_num}/ensda/jhwrf_ensda_pre++g"`
        ecflow_client --event no_ensda
        ecflow_client --force complete $NETNAME/hwrf${storm_num}/ensda/jhwrf_ensda_output
        ecflow_client --force complete recursive $NETNAME/hwrf${storm_num}/ensda/members
        # FIXME: NCO SPA: ENTER CODE HERE
    fi
}
@** elseif JJOBNAME==OCEAN_INIT
coupled_forecast() {
    # This script is called if the forecast is to run coupled.
    if [[ "$SENDECF" == YES ]] ; then
        ecflow_client --event run_couple
        NETNAME=`echo $ECF_NAME | sed "s+/hwrf${storm_num}/prep/jhwrf_ocean_init++g"`
        ecflow_client --force complete $NETNAME/hwrf${storm_num}/fcst/jhwrf_noncouple_forecast
    fi
}

uncoupled_forecast() {
    # This script is called if the forecast is to run coupled.
    if [[ "$SENDECF" == YES ]] ; then
        ecflow_client --event run_noncouple
        NETNAME=`echo $ECF_NAME | sed "s+/hwrf${storm_num}/prep/jhwrf_ocean_init++g"`
        ecflow_client --force complete $NETNAME/hwrf${storm_num}/fcst/jhwrf_couple_forecast
    fi
}
@** else
# No extra vars or logic needed for this job.
@** endif

####################################
# Run ex-script
####################################

set -u

# Prepend HWRF's ush to Python path:
export PYTHONPATH=$USHhwrf${PYTHONPATH:+:$PYTHONPATH}

# Announce intent:
postmsg "$jlogfile" "Starting exhwrf_@[EXNAME].py."

@** if JJOBNAME==LAUNCH
# Launcher script needs extra args:
$EXhwrf/exhwrf_@[EXNAME].py ${mYMDH} $storm FORECAST $PARMhwrf $configmore
err=$?
@** elseif JJOBNAME==OCEAN_INIT
$EXhwrf/exhwrf_@[EXNAME].py
err=$?
if ( grep 'RUN_COUPLED=YES' $COMOUT/$stormlabel.ocean_status ) ; then
    postmsg "$jlogfile" "JHWRF_OCEAN_INIT $stormlabel wants a coupled forecast"
    coupled_forecast
elif ( grep 'RUN_COUPLED=NO' $COMOUT/$stormlabel.ocean_status ) ; then
    postmsg "$jlogfile" "JHWRF_OCEAN_INIT $stormlabel wants an uncoupled forecast"
    uncoupled_forecast
else
    postmsg "$jlogfile" "could not see RUN_COUPLED=YES or NO in $COMOUT/$stormlabel.ocean_status - JHWRF_OCEAN_INIT failed for $stormlabel"
    uncoupled_forecast
fi
@** elseif JJOBNAME==ENSDA_PRE
rm -f "$COMOUT/$stormlabel.run_ensda"   # make sure we get a fresh copy

# Run script
$EXhwrf/exhwrf_@[EXNAME].py
err=$?

if ( grep 'RUN_ENSDA=YES' $COMOUT/$stormlabel.run_ensda ) ; then
    postmsg "$jlogfile" "RUN_ENSDA=YES in $stormlabel.run_ensda - run ensda jobs"
    run_ensda
elif ( grep 'RUN_ENSDA=NO' $COMOUT/$stormlabel.run_ensda ) ; then
    postmsg "$jlogfile" "RUN_ENSDA=NO in $stormlabel.run_ensda - do NOT run ensda jobs"
    cancel_ensda
else
    err_exit "did not see RUN_ENSDA=YES or NO in $COMOUT/$stormlabel.run_ensda - script failed"
fi
@** else
# Run script:
$EXhwrf/exhwrf_@[EXNAME].py
err=$?
@** endif

# Announce success or failure
err_chk
if [[ "$err" != 0 ]] ; then
    postmsg "$jlogfile" "err_chk did not exit on err=$err -- exit 99"
    exit 99
fi

@** if JJOBNAME==BUFRPREP
set -u
basin1=${STORMID:2:1}
tdrflagfile=$COMIN/$stormlabel.tdr
if [[ "$basin1" != E ]] ; then
    postmsg "$jlogfile" "Basin is \"$basin1\" so I will run the full GSI workflow."
elif [[ -s "$tdrflagfile" ]] ; then
    postmsg "$jlogfile" "$tdrflagfile: exists so I will run the full GSI workflow."
else
    postmsg "$jlogfile" "Basin is $basin1 and $tdrflagfile is missing so I will skip GSI and merge"
    set +u
    if [[ "$SENDECF" == YES ]] ; then
        NETNAME=`echo $ECF_NAME | sed "s+/hwrf${storm_num}/analysis/jhwrf_bufrprep++g"`
        ecflow_client --force complete $NETNAME/hwrf${storm_num}/post/jhwrf_gsi_post
        ecflow_client --force complete $NETNAME/hwrf${storm_num}/analysis/jhwrf_merge
        ecflow_client --force complete $NETNAME/hwrf${storm_num}/analysis/jhwrf_nmm_gsi_d2
        ecflow_client --force complete $NETNAME/hwrf${storm_num}/analysis/jhwrf_nmm_gsi_d3
    fi
fi
    
@** endif
postmsg "$jlogfile" "Ended normally."
