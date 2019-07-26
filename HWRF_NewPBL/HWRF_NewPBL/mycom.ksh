#!/bin/ksh --login

set -x

module purge
module use /hwrf/save/Weiguo.Wang/trunk20150901/modulefiles/wcoss
module load HWRF/build

 export PNETCDF_QUILT=1
 export WRF_NMM_CORE=1
 export WRF_NMM_NEST=1
 export HWRF=1


#configure

./compile nmm_real>&log99&

