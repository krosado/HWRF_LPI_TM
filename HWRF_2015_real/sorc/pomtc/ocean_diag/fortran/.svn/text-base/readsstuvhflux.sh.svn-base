#!/bin/sh
fpth=/lfs3/projects/hur-uri/Richard.Yablonsky/MPI_POMTC_20140930/sorc/pomtc/ocean_diag/fortran # Current directory
#spth=/lfs3/projects/hur-uri/Richard.Yablonsky/pytmp/MPI_POMTC_20140930                         # Path to HWRF run $(WORKhwrf)
spth=/lfs1/projects/dtc-hurr/Ligia.Bernardet/pytmp/rocHWRF                                     # Path to HWRF run $(WORKhwrf)
snam=edouard                                                                                   # Storm name
stid=06l                                                                                       # Storm ID & basin identifier
sdat=2014091218                                                                                # Storm cycle (YYYYMMDDHH)

cd ${spth}/${sdat}/${stid^^}/runhwrf
ln -s -f ${spth}/com/${sdat}/${stid^^}/${snam}${stid}.${sdat}.trak.hwrf.3hourly fort.10
${fpth}/../../ocean_exec/readsstuvhflux.exe
