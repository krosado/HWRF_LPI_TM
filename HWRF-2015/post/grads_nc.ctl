dset ^grads.nc
dtype netcdf
undef -888
TITLE WRF Output Grid Coordinates: south_north, west_east
xdef 264 linear 1 1
*xdef 197 linear 1 1
ydef 471 linear 1 1
*ydef 353 linear 1 1
zdef 60  linear 1 1
tdef 1 linear 11jun2002 3hr
vars 5
U=>uu      60  t,z,y,x   U-wind
V=>vv      60  t,z,y,x   V-wind
U10=>u10m   0  t,y,x     U-wind
V10=>v10m   0  t,y,x     V10-wind
MSLP=>slp   0  t,y,x     SLP
endvars
