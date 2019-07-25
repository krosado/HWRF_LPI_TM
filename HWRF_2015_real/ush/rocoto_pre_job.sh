#! /bin/sh --login
set -x -u -e
date
. $USHhwrf/hwrf_pre_job.sh.inc
exec "$@"
