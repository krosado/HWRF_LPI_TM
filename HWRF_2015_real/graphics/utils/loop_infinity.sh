#!/bin/bash
#
# NOTE: this script is to automate the transfer graphics from Jet 
#       to emcrzdm infinitely every 1 hour. This is because the
#       crontab has been disabled after the new upgrade of this
#       desktop
#
# HIST:
#       June 2013: created by Chanh Kieu
#===============================================================
set -x
offset=$1
#sleep $offset
loop="true"
while [ "$loop" == "true" ]; do
 check_time=`date | cut -d ':' -f 2`
 if [ "$check_time" == "05" ] || [ "$check_time" == "30" ]; then
  echo "calling the ATEP trasnfer for FY2013 Parallel run on Jet at $( date )"
  sh ./jet_transfer_atep.sh
  echo "calling the WPAC trasnfer for FY2013 Parallel run on Jet at $( date )"
  sh ./jet_transfer_wpac.sh
 else
  echo "Waiting until 05 mn to start transfer...."
  sleep 30
 fi
done
