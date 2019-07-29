# HWRF_LPI_TM
The code is available at https://dtcenter.org/HurrWRF/users/downloads/index.php HWRF System components - V3.7a 2015
After dowloding the files in this repository and dowloading the code:

Rename: 
module_mp_thompson_lpi.F module_mp_thompson.F
module_PHYSICS_CALLS_LPI.F module_PHYSICS_CALLS.F
solve_nmm_LPI.F solve_nmm.F

Replace: 
"nameofyourdirecotry"/WRFV3/phys/module_mp_thompson.F
"nameofyourdirecotory"/WRFV3/dyn_nmm/module_PHYSICS_CALLS.F
"nameofyourdirecotory"/WRFV3/dyn_nmm/solve_nmm.F

To configure and compile the code please follow the user guide: 
https://dtcenter.org/HurrWRF/users/docs/users_guide/noaa_11345_DS1.pdf

Input data for the idealized case is 0825012000000 and 0825512000000 
Input data for Earl is fnl_20100828_00_00.grib2.nc 
Input data for Igor is fnl_20100910_06_00.grib2.nc

Namelist input files: 
Ideal case namelist.input.1
Ideal NewPBL namelist.input.2
Earl namelist.input.3
Igor namelist.input.4

The HWRF code for the ideal experiemnts is also provided: 
HWRF-2015 and HWRF_NewPBL

HWRF code for real experiments: HWRF_2015_real
