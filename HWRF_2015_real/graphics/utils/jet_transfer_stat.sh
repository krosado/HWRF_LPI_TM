#!/bin/ksh
set -x
home_dir="/export/emc-lw-ckieu/wd20ck/web/"
jet_dir="/mnt/lfs2/projects/hwrfv3/Chanh.Kieu/programs/NHC_realtime/run/figures"
rzdm_dir="/home/others/people/emc/www/htdocs/gc_wmb/vxt/STAT_2013"
cd ${home_dir}
echo "Starting at $( date )"  
echo "Jet cycle dir: ${jet_dir}" 
echo "Local cycle dir: ${home_dir}/STAT_2013/" 
echo "rzdm cycle dir: ${rzdm_dir}" 
scp Chanh.Kieu@jetscp.rdhpcs.noaa.gov:${jet_dir}/\*AL2013/fig_\*tker.png ${home_dir}/STAT_2013/ > /dev/null
scp Chanh.Kieu@jetscp.rdhpcs.noaa.gov:${jet_dir}/\*AL2013/fig_\*wind.png ${home_dir}/STAT_2013/ > /dev/null
scp Chanh.Kieu@jetscp.rdhpcs.noaa.gov:${jet_dir}/\*AL2013/fig_\*bias.png ${home_dir}/STAT_2013/ > /dev/null
scp ${home_dir}/STAT_2013/* wd20vxt@emcrzdm:${rzdm_dir}/ > /dev/null
