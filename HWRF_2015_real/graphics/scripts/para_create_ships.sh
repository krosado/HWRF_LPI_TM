#!/bin/bash
# script create_diagfiles.sh
#####################################################################
# This script takes grib2 input and makes diagnostic files
# for each of the model initial times specified in input.list.
# See README for further details.
#####################################################################
# Required input files:
#   -input.params
#   -input.list
#   -input.plvls   -added in Version 1.1
#   -parent and nested grid files 
#      -for cases specified in input.list
#      -for all times specified in input.params
#   -adeck (track) file(s) for storm(s) in input.list
#####################################################################
# Version 2.1, last modified 12/31/2012
#   -added alternate field names for SLP, SST for nested grid
#   -added control flag and files for printing averaging radii
#   -adjusted exit on missing nest file to not occur when parent file
#    is missing as well
# Version 2.0, last modified 08/07/2012, created 03/02/2012
#   -added ifort compilation support
#   -adjusted handling of global models to adjust for lack of nests
#   -shifted from ascii data files to binary data files
#   -added alternate field names for surface T, SLP, and SST
#   -code maintenance
# Version 1.1, last modified 02/11/2011
#   -added pressure level specification support, text field headers
#   -addressed missing vortex cases
#   -fixed missing fields, including surface fields and nested fields
# Version 1, 01/11/2011
#####################################################################

date

#:starting from home directory
#cd
homedir=`echo $PWD`

#:specify directories for model grib2 files and diag files
maindir="${homedir}/"
outputdir="${homedir}/"
adeckdir="${homedir}/"
echo "maindir = $maindir"

cd ${maindir}

#:retrieve the maximum time, the time interval, and the model override
#: from the input.params file
#:-note: tmax, tint (in hrs); not currently set up for minutes
set `cat ${maindir}'input.params'`
nlvls=${1} tmax=${2} tint=${3} mnested=${4}
smodel2=${5} sruntype=${6} sversion=${7}

imiss=-9999
imissn=$(( ${imiss} * -1 ))

#:specify text strings for finding and printing fields
fieldbegT='TMP:'
fieldbegR='RH:'
fieldbegZ='HGT:'
fieldbegU='UGRD:'
fieldbegV='VGRD:'
fieldend=' mb'
fieldtextbeg='FIELD:'
fieldT='T_'
fieldR='R_'
fieldZ='Z_'
fieldU='U_'
fieldV='V_'
fieldP='P_'
fieldSST='SST'
fieldOHC='OHC'
fieldTPW='TPW'
fieldsurf='SURF'

#:loop through all available input files - initial times
#:-will only accept pressure lvl data on regular lat/lon grid
for initfile in `cat ${maindir}input.list`
do
#:process filename
   test -e ${initfile}
   if [ $? -ne 0 ]
   then
      echo 'initial file:'${initfile}' not found'
      exit 1
   fi
   initbase=`basename ${initfile}`
   initdir=`dirname ${initfile}`
   echo ${initbase} > ${maindir}'tname.txt'
   ./nameparse.x
   set `cat ${maindir}'tname2.txt'`
   sname=${1} snum=${2} sbasin=${3} sdtg=${4}
   syr=${5} smo=${6} sda=${7} sti=${8}
   smodel=${9} svert=${10} sgrid=${11} sfitype=${12}
   sforeti=${13} sfint=${14} sfipregr=${15} sfipostgrnt=${16}

#: specify diagnostic filename and adeck filename
   smodeluc=`echo ${smodel} | tr 'a-z' 'A-Z'`
   smodel2uc=`echo ${smodel2} | tr 'a-z' 'A-Z'`
   sfiout='s'${sbasin}${snum}${syr}'_'${smodel2}'_'${sdtg}'_diag.dat'
   sfiadeck=${adeckdir}'a'${sbasin}${snum}${syr}'.dat'
   echo "chanh: sfiadeck = $sfiadeck"

#:check for adeck/track file
   test -e ${sfiadeck}
   if [ $? -ne 0 ]
   then
      echo 'track file:'${sfiadeck}' not found'
      if [ "$sbasin" == "ep" ]; then
       sfiadeck="${adeckdir}${sname}${snum}e.${sdtg}.trak.hwrf.3hourly"
      elif [ "$sbasin" == "al" ]; then
       sfiadeck="${adeckdir}${sname}${snum}l.${sdtg}.trak.hwrf.3hourly"
      elif [ "$sbasin" == "wp" ]; then
       sfiadeck="${adeckdir}${sname}${snum}w.${sdtg}.trak.hwrf.3hourly"
      elif [ "$sbasin" == "sh" ]; then
       sfiadeck="${adeckdir}${sname}${snum}p.${sdtg}.trak.hwrf.3hourly"
      fi
      #sfiadeck=${adeckdir}'a'${sbasin}${snum}${syr}'_'${smodeluc}'_hPARA_'${sdtg}'.dat'
      echo 'looking for track file:'${sfiadeck}
      test -e ${sfiadeck}
      if [ $? -ne 0 ]
      then
         echo 'track file:'${sfiadeck}' not found'
         exit 1
      fi
   fi
   sed 's/HWRF/'${smodel2uc}'/g' ${sfiadeck} > ./tempadeck.dat
   sed -i '/50, NEQ/d' ./tempadeck.dat
   sed -i '/64, NEQ/d' ./tempadeck.dat

#:set up control file for creating total diagnostic files
   dtxt='diaginfo.txt'
   echo ${nlvls} > ${dtxt}
   echo ${mnested} >> ${dtxt}
   echo ${tmax} >> ${dtxt}
   echo ${tint} >> ${dtxt}
   echo ${sdtg} >> ${dtxt}
   echo ${smodel} >> ${dtxt}
   echo ${sbasin} >> ${dtxt}
   echo ${snum} >> ${dtxt}
   echo ${sname} >> ${dtxt}

#:set base filenames for the chosen run
   pbase=${initdir}'/'${sfipregr}'p'${sfipostgrnt}
   nbase=${initdir}'/'${sfipregr}'n'${sfipostgrnt}

#:set output filenames for current forecast time (text files)
   outptxt=${maindir}temp_fieldp.txt
   outntxt=${maindir}temp_fieldn.txt

#:loop through times indicated by tmax and tint (starting at 0)
   currtime=0
   while [ $currtime -le $tmax ]
   do
      if [ $currtime -lt 10 ]
      then
         pcurr=${pbase}'0'${currtime}
         ncurr=${nbase}'0'${currtime}
         fcurr='f00'${currtime}
         diagcurr='mdiagf00'${currtime}'.dat'
      elif [ $currtime -lt 100 ]
      then
         pcurr=${pbase}${currtime}
         ncurr=${nbase}${currtime}
         fcurr='f0'${currtime}
         diagcurr='mdiagf0'${currtime}'.dat'
      else
         pcurr=${pbase}${currtime}
         ncurr=${nbase}${currtime}
         fcurr='f'${currtime}
         diagcurr='mdiagf'${currtime}'.dat'
      fi
#:tests for existence of parent grid
      test -e ${pcurr}
      if [ $? -ne 0 ]
      then
         echo "parent grid:${pcurr} not found"
         pgexist=0
      else
         pgexist=1
      fi
#:tests for existence of nested grid
      if [ ${mnested} -eq 1 ]
      then
         test -e ${ncurr}
         if [ $? -ne 0 ]
         then
            echo "nested grid:${ncurr} not found"
            if [ ${pgexist} -eq 1 ]
            then
               echo "create_diagfiles.sh exiting"
#              remove temporary files
               rm tname.txt
               rm tname2.txt
               rm temp*
               rm lsdiagradii.txt
               rm printradiiflag.txt
#              remove temporary parameter files (individual times)
               rm center*
               rm params*
               rm diaginfo.txt
               exit 1
            fi
         fi
      fi
#:get current center location from tmpadeck.dat
      echo ${sdtg} ${smodel2uc} ${imiss} ${currtime} > tempadinfo.txt
#      cp tempadinfo.txt tempadinfo${fcurr}.txt
      ./getcenter.x
#      cp center.txt center${fcurr}.txt
#      echo center${fcurr}.txt >> ${dtxt}

      set `cat ${maindir}'center.txt'`
      centerlat=${1} centerlon=${2}
#:if center location exists at current time, get parent and nested grids
#: otherwise, skip grids and call null case for parameter file fill-in
      if [ ${centerlat} -eq ${imissn} ] || [ ${centerlon} -eq ${imissn} ]
      then
#:    fill in the missing value array here for current time
         ./inddiagnull.x
         mv params.txt params${fcurr}.txt
         echo params${fcurr}.txt >> ${dtxt}
#:if parent grid is missing at current time, fill in the missing value array
      elif [ ${pgexist} -eq 0 ]
      then
         ./inddiagnull.x
         mv params.txt params${fcurr}.txt
         echo params${fcurr}.txt >> ${dtxt}
#:get parent grid information (nx, ny, lat, lon, lat/lon intervals)
      else
         wgrib2 -nxny -d 1 ${pcurr} > tempnxny.txt
         wgrib2 -grid -d 1 ${pcurr} > templatlon.txt
         ./gridparse.x
         mv tempgrid.txt temp_gridp.txt

#:get parent grid fields
#        T 2m
         outpbin=${fieldT}${fieldsurf}'_p.bin'
         wgrib2 -match "TMP:2 m" -bin ${outpbin} ${pcurr}
         if [ ! -s "$outpbin" ]
         then
            echo "Warning: No field for ${outpbin}, skipping."
            rm ${outpbin}
#           added search for alternate field name for surface temperature
            wgrib2 -match "TMP:surf" -bin ${outpbin} ${pcurr}
            if [ ! -s "$outpbin" ]
            then
               echo "Warning: No field for ${outpbin} alternate name, skipping."
               rm ${outpbin}
            fi
#           end alternate field name search for surface temperature
         fi
#        RH 2m
         outpbin=${fieldR}${fieldsurf}'_p.bin'
         wgrib2 -match "RH:2 m" -bin ${outpbin} ${pcurr}
         if [ ! -s "$outpbin" ]
         then
            echo "Warning: No field for ${outpbin}, skipping."
            rm ${outpbin}
         fi
#        U 10m
         outpbin=${fieldU}${fieldsurf}'_p.bin'
         wgrib2 -match "UGRD:10 m" -bin ${outpbin} ${pcurr}
         if [ ! -s "$outpbin" ]
         then
            echo "Warning: No field for ${outpbin}, skipping."
            rm ${outpbin}
         fi
#        V 10m
         outpbin=${fieldV}${fieldsurf}'_p.bin'
         wgrib2 -match "VGRD:10 m" -bin ${outpbin} ${pcurr}
         if [ ! -s "$outpbin" ]
         then
            echo "Warning: No field for ${outpbin}, skipping."
            rm ${outpbin}
         fi
#        SLP
         outpbin=${fieldP}${fieldsurf}'_p.bin'
         wgrib2 -match "PRMSL" -bin ${outpbin} ${pcurr}
         if [ ! -s "$outpbin" ]
         then
            echo "Warning: No field for ${outpbin}, skipping."
            rm ${outpbin}
#           added search for alternate field name(s) for SLP
            wgrib2 -match "MSL" -bin ${outpbin} ${pcurr}
            if [ ! -s "$outpbin" ]
            then
               echo "Warning: No field for ${outpbin} alternate name, skipping."
               rm ${outpbin}
               wgrib2 -match "mean sea level" -bin ${outpbin} ${pcurr}
               if [ ! -s "$outpbin" ]
               then
                  echo "Warning: No field for ${outpbin} alternate names, skipping."
                  rm ${outpbin}
               fi
            fi
#           end alternate field name search for SLP
         fi

#:cycle through specified pressure levels to retrieve sounding data
         for currplvl in `cat ${maindir}input.plvls`
         do
            if [ $currplvl -lt 10 ]
            then
               currplvlt='000'${currplvl}
            elif [ $currplvl -lt 100 ]
            then
               currplvlt='00'${currplvl}
            elif [ $currplvl -lt 1000 ]
            then
               currplvlt='0'${currplvl}
            else
               currplvlt=${currplvl}
            fi
#           T
            fieldname=${fieldbegT}${currplvl}${fieldend}
            outpbin=${fieldT}${currplvlt}'_p.bin'
            wgrib2 -match "${fieldname}" -bin ${outpbin} ${pcurr}
            if [ ! -s "$outpbin" ]
            then
               echo "Warning: No field for ${outpbin}, skipping."
               rm ${outpbin}
            fi
#           RH
            fieldname=${fieldbegR}${currplvl}${fieldend}
            outpbin=${fieldR}${currplvlt}'_p.bin'
            wgrib2 -match "${fieldname}" -bin ${outpbin} ${pcurr}
            if [ ! -s "$outpbin" ]
            then
               echo "Warning: No field for ${outpbin}, skipping."
               rm ${outpbin}
            fi
#           Z
            fieldname=${fieldbegZ}${currplvl}${fieldend}
            outpbin=${fieldZ}${currplvlt}'_p.bin'
            wgrib2 -match "${fieldname}" -bin ${outpbin} ${pcurr}
            if [ ! -s "$outpbin" ]
            then
               echo "Warning: No field for ${outpbin}, skipping."
               rm ${outpbin}
            fi
#           U
            fieldname=${fieldbegU}${currplvl}${fieldend}
            outpbin=${fieldU}${currplvlt}'_p.bin'
            wgrib2 -match "${fieldname}" -bin ${outpbin} ${pcurr}
            if [ ! -s "$outpbin" ]
            then
               echo "Warning: No field for ${outpbin}, skipping."
               rm ${outpbin}
            fi
#           V
            fieldname=${fieldbegV}${currplvl}${fieldend}
            outpbin=${fieldV}${currplvlt}'_p.bin'
            wgrib2 -match "${fieldname}" -bin ${outpbin} ${pcurr}
            if [ ! -s "$outpbin" ]
            then
               echo "Warning: No field for ${outpbin}, skipping."
               rm ${outpbin}
            fi
         done

#        TPW
         outpbin=${fieldTPW}'_p.bin'
         wgrib2 -match "PWAT" -bin ${outpbin} ${pcurr}
         if [ ! -s "$outpbin" ]
         then
            echo "Warning: No field for ${outpbin}, skipping."
            rm ${outpbin}
         fi
#        SST
         outpbin=${fieldSST}'_p.bin'
         wgrib2 -match "WTMP:surf" -bin ${outpbin} ${pcurr}
         if [ ! -s "$outpbin" ]
         then
            echo "Warning: No field for ${outpbin}, skipping."
            rm ${outpbin}
#           added search for alternate field name for SST
            wgrib2 -match "TMP:surf" -bin ${outpbin} ${pcurr}
            if [ ! -s "$outpbin" ]
            then
               echo "Warning: No field for ${outpbin} alternate name, skipping."
               rm ${outpbin}
            fi
#           end alternate field name search for SST
         fi
#:list the _p.bin files into outptxt
         ls -1 *_p.bin > ${outptxt}
#:if no parent fields are found make an empty list file
         if [ ! -f "$outptxt" ]
         then
            touch ${outptxt}
         fi

#:get nested grid if nested grid is specified
         if [ ${mnested} -eq 1 ]
         then
#:get nested grid information (nx, ny, lat, lon, lat/lon intervals)
            wgrib2 -nxny -d 1 ${ncurr} > tempnxny.txt
            wgrib2 -grid -d 1 ${ncurr} > templatlon.txt
            ./gridparse.x
            mv tempgrid.txt temp_gridn.txt
#:get nested grid fields
#           U 10m
            outnbin=${fieldU}${fieldsurf}'_n.bin'
            wgrib2 -match "UGRD:10 m" -bin ${outnbin} ${ncurr}
            if [ ! -s "$outnbin" ]
            then
               echo "Warning: No field for ${outnbin}, skipping."
               rm ${outnbin}
            fi
#           V 10m
            outnbin=${fieldV}${fieldsurf}'_n.bin'
            wgrib2 -match "VGRD:10 m" -bin ${outnbin} ${ncurr}
            if [ ! -s "$outnbin" ]
            then
               echo "Warning: No field for ${outnbin}, skipping."
               rm ${outnbin}
            fi
#           SLP
            outnbin=${fieldP}${fieldsurf}'_n.bin'
            wgrib2 -match "PRMSL" -bin ${outnbin} ${ncurr}
            if [ ! -s "$outnbin" ]
            then
               echo "Warning: No field for ${outnbin}, skipping."
               rm ${outnbin}
#              added search for alternate field name(s) for SLP
               wgrib2 -match "MSL" -bin ${outnbin} ${ncurr}
               if [ ! -s "$outnbin" ]
               then
                  echo "Warning: No field for ${outnbin} alternate name, skipping."
                  rm ${outnbin}
                  wgrib2 -match "mean sea level" -bin ${outnbin} ${ncurr}
                  if [ ! -s "$outnbin" ]
                  then
                     echo "Warning: No field for ${outnbin} alternate names, skipping."
                     rm ${outnbin}
                  fi
               fi
#              end alternate field name search for SLP
            fi
#           SST
            outnbin=${fieldSST}'_n.bin'
            wgrib2 -match "WTMP:surf" -bin ${outnbin} ${ncurr}
            if [ ! -s "$outnbin" ]
            then
               echo "Warning: No field for ${outnbin}, skipping."
               rm ${outnbin}
#              added search for alternate field name for SST
               wgrib2 -match "TMP:surf" -bin ${outnbin} ${ncurr}
               if [ ! -s "$outnbin" ]
               then
                  echo "Warning: No field for ${outnbin} alternate name, skipping."
                  rm ${outnbin}
               fi
#              end alternate field name search for SST
            fi
#:list the _n.bin files into outntxt
            ls -1 *_n.bin > ${outntxt}
#:if no nested fields are found make an empty list file
            if [ ! -f "$outntxt" ]
            then
               touch ${outntxt}
            fi
         fi

#:setup file to specify radii output for comment section
         if [ $currtime -eq 0 ]
         then
            echo "1" > printradiiflag.txt
         else
            echo "0" > printradiiflag.txt
         fi

#:run diagnostic parameter calculation for current time
         ./inddiag.x
         mv params.txt params${fcurr}.txt
         echo params${fcurr}.txt >> ${dtxt}

#:remove binary files before starting next time
         rm *.bin
         rm ${outptxt}
         if [ ${mnested} -eq 1 ]
         then
            rm ${outntxt}
         fi

      fi   #finish if statement for reading and calculating parameters

#:calculate next forecast time
      currtime=$(( $currtime + $tint ))
   done   #finish loop for each individual time

#:check for existence of radii file before including in diag file
   if [ -s lsdiagradii.txt ]
   then
      echo "1" > printradiiflag.txt
   else
      echo "0" > printradiiflag.txt
   fi

#:run diagnostic file output from accumulation of individual times
   ./totaldiag.x
   mv diag.txt ${outputdir}${sfiout}

#:remove temporary files
   rm tname.txt
   rm tname2.txt
   rm temp*
   rm lsdiagradii.txt
   rm printradiiflag.txt
#:remove temporary parameter files (individual times)
   rm center*
   rm params*
   rm diaginfo.txt

done   #finish loop for each specified initial file

date

exit 0
