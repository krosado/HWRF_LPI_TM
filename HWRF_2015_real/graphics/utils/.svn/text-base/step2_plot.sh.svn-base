#!/bin/sh
#
# plot grads for each date
#
rm -rf *.gif grads.ctl
for ifile in `ls ./plots/*9km*.ctl`
do
 echo "ifile = $ifile"
 cut1=${ifile%.*}
 cut2=`echo ${cut1} | sed -e 's/\///g'`
 cut3=`echo ${cut2} | sed -e 's/\.//g'`
 cut4=`echo ${cut3} | sed -e 's/plots//'`
 prefix=`echo ${cut4} | sed -e 's/9km//'`
 echo "prefix=$prefix"
# ofile="./9km.${ext}"
# echo "Working with the ctl $ifile"
# itime=`echo $ifile | sed 's/irene//' | sed 's/\.ctl//'`
# ln -sf ${ifile} ./grads.ctl
# grads -lxbc plot_trk.gs
# mv out.gif vort_${itime}.gif
# echo " ==> time get is $itime" 
done

