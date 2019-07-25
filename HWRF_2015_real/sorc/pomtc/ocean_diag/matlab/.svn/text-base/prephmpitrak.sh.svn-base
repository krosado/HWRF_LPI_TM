#!/bin/sh
mpth=/lfs3/projects/hur-uri/Richard.Yablonsky/MPI_POMTC_20140930/sorc/pomtc/ocean_diag/matlab # Current directory
opth=${mpth}/pngs                                                                             # Path for diagnostic output
#spth=/lfs3/projects/hur-uri/Richard.Yablonsky/pytmp/MPI_POMTC_20140930                        # Path to HWRF run $(WORKhwrf)
spth=/lfs1/projects/dtc-hurr/Ligia.Bernardet/pytmp/rocHWRF                                    # Path to HWRF run $(WORKhwrf)
snam=edouard                                                                                  # Storm name
stid=06l                                                                                      # Storm ID & basin identifier
sdat=2014091218                                                                               # Storm cycle (YYYYMMDDHH)

hwrff=${spth}/com/${sdat}/${stid^^}/${snam}${stid}.${sdat}.trak.hwrf.3hourly
sed "s/[A-Z]//g" ${hwrff} > ${opth}/${snam}${stid}.${sdat}.trak.hwrf.3hourly.dat
