#!/bin/bash
#exp=$1
exp="ideal_lpi"
if [ "$exp" == "" ]; then
    echo "Have to input an exp name to proceed.... exit 1"
    exit 1
fi
prefix="wrfout_d03_2008"
rm -f time_series_${exp}.txt
hh=0
interval=3
for ifile in `ls ${prefix}*`
do
    echo "Working with file $ifile"
    rm -f ./grads.nc out.txt outp.txt
    ln -sf $ifile ./grads.nc
    grads -xlbc grads.gs
    vmax=`cat out.txt`
    pmin=`cat outp.txt`
    if [ "$vmax" == "" ] || [ "$vmax" == "file" ]; then
        echo "vmax is not valid...stop: $vmax"
        exit 1
    fi
    echo "$hh $vmax $pmin" >> time_series_${exp}.txt 
    hh=$(($hh+$interval))
done
