#!/bin/sh
#
# create ctl files first
#
# ./copygb -g"255 0 441 361 62200 220000 136 -27800 330000 250 250 0" -x
hr_grid=`echo \"255 0 441 361 62200 220000 136 -27800 330000 250 250 0\"`
echo "hr_grid=$hr_grid"
cgb="./copygb"
mpath="/misc/whome/chanh.kieu/opt/bin"
#
# convert the 3-km domain first
#
#while read file_t
#do
# echo "file_t = $file_t"
# y=${file_t%.*}
# b=`echo $file_t | sed 's/\///g'`
# a=`echo $y | sed 's/\///g'`
# ext=`echo $b | sed s/$a.//`
# ofile="./3km.${ext}"
# ./copygb -g"255 0 441 361 62200 220000 136 -27800 330000 250 250 0" -x $file_t ${ofile}
# grib2ctl.pl -verf $ofile > ${ofile}.ctl
# gribmap -i ${ofile}.ctl 
# echo "Got file $ofile"
#done < infile_t.txt
#
# convert the 9-km domain first
#
while read file_t
do
 echo "file_t = $file_t"
 y=${file_t%.*}
 b=`echo $file_t | sed 's/\///g'`
 a=`echo $y | sed 's/\///g'`
 ext=`echo $b | sed s/$a.//`
 ofile="./9km.${ext}"
 ${mpath}/copygb -g"255 0 441 361 62200 220000 136 -27800 330000 250 250 0" -x $file_t ${ofile}
 ${mpath}/grib2ctl.pl -verf $ofile > ${ofile}.ctl
 gribmap -i ${ofile}.ctl
 echo "Got file $ofile"
done < infile_n.txt
#
# convert the 27-km domain first
#
while read file_t
do
 echo "file_t = $file_t"
 y=${file_t%.*}
 b=`echo $file_t | sed 's/\///g'`
 a=`echo $y | sed 's/\///g'`
 ext=`echo $b | sed s/$a.//`
 ofile="./27km.${ext}"
 ${mpath}/copygb -g"255 0 441 361 62200 220000 136 -27800 330000 250 250 0" -x $file_t ${ofile}
 ${mpath}/grib2ctl.pl -verf $ofile > ${ofile}.ctl
 gribmap -i ${ofile}.ctl
 echo "Got file $ofile"
done < infile_p.txt
#
# plot grads for each date
#
#rm -rf *.gif grads.ctl
#for ifile in `ls *.ctl`
#do
# echo "Working with the ctl $ifile"
# itime=`echo $ifile | sed 's/irene//' | sed 's/\.ctl//'`
# ln -sf ${ifile} ./grads.ctl
# grads -lxbc plot_trk.gs
# mv out.gif vort_${itime}.gif
# echo " ==> time get is $itime" 
#done

