dset ^grads.nc
dtype netcdf
undef -888
TITLE WRF Output Grid Coordinates: south_north, west_east
xdef 497 linear 1 1
ydef 431 linear 1 1
zdef 61  linear 1 1
tdef 1 linear 11jun2002 3hr
vars 4
U=>uu  61  t,z,y,x   U-wind
V=>vv  61  t,z,y,x   U-wind
GLON=>xlon  0  t,y,x   U-wind
GLAT=>xlat  0  t,y,x   U-wind 
endvars
