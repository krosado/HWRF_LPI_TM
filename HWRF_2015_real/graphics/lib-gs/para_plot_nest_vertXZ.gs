*****************************************************************
* hwrf_gr_nest_vertXZ_.gs
*****************************************************************
'open ctl_file'  
'set display color white' 
'set xlopts 1 7 0.14'
'set ylopts 1 7 0.14'
'set xlint 1'
'run rgbset.gs'
'q dims'
rc=sublin(result,2)
lon_min=subwrd(rc,6)
lon_max=subwrd(rc,8)
rc=sublin(result,3)
lat_min=subwrd(rc,6)
lat_max=subwrd(rc,8)
say lon_min' 'lon_max' 'lat_min' 'lat_max  
*-----------------------------------------------------------------
* make vertical cross section plot in the XZ plane (LAT=const) 
outfile='vXZ_fHOUR.png'
'set grads off' 
*-----------------------------------------------------------------
* plot vertical (XZ) cross section of wind in knots (shaded and contours) 
*-----------------------------------------------------------------
'set lat CENTER_LAT' 
'q dims'
rc=sublin(result,3)
lat_center=subwrd(rc,6)
say lat_center
'set zlog on'
'set z 1 39'
if (lat_center > lat_min) & (lat_center < lat_max)
 'define vkts=1.944*VGRDprs'  
 'set stat on'
 'd abs(vkts)'
 stats=result
 line8=sublin(stats,8)
 maxval=subwrd(line8,5)
 'set stat off'
 if (maxval > 0 & maxval < 270)
  'set gxout shaded'  
  'set clevs 5 15 25 35 45 55 65 75 85 95 105 115 125 135 145 155' 
  'set ccols 0  0 21 23 25 27 28 29 42 52 56 81 85 87 73 75 78'
  'd abs(vkts)'
  'set gxout contour' 
  'set clevs 25 35 45 55 65 75 85 95 105 115 125 135 145 155' 
  'run cbarn.gs'
 else
  'set y 10'
  'set ccolor 0'
  'd abs(lon)'
  'draw title MC_MODEL STORM stormid E-W CROSS SECT LAT=CENTER_LAT'
  'set string 1 bl 5'
  'set strsiz 0.10 0.10'
  'draw string 3.0 7.75 INIT YYYYMMDDHH Z for HOUR h FCST VALID vdate Z'
  'set string 2 c 18'
  'set strsiz 0.25 0.25'
  'draw string 5.8 5.0 *** WARNING ***'
  'draw string 5.8 4.5 STORM LATITUDE IS OUT OF DOMAIN 3'
  'draw string 5.8 4.0 or STORM DISSIPATED'
  maxval="Not defined"
 endif
 'draw title MC_MODEL STORM stormid E-W CROSS SECT LAT=CENTER_LAT'  
 'set string 1 bl 5'
 'set strsiz 0.10 0.10' 
 'draw string 3.0 7.75 INIT YYYYMMDDHH Z for HOUR h FCST VALID vdate Z' 
 'draw string 2.0 7.50 ISOTACHS (KTS)-SOLID LINES, COLORS; ISOTHERMS (C)-DASHES'  
 'draw string 2.0 7.25 MAX N-S WIND (kts) ' maxval
 'draw string 1.75 4.00 WEST'
 'draw string 9.75 4.00 EAST' 
*-----------------------------------------------------------------
* plot temperature dashed contours every 10 deg C 
*-----------------------------------------------------------------
 'set gxout contour' 
 'define TMPC=TMPprs-273.'  
 'set clevs -40 -30 -20 -10 0 10 20 30' 
 'set cstyle 3'
 'd TMPC'
else
 alat=(lat_min+lat_max)/2
 'set lat 'alat
 'set gxout contour'
 'set clevs 1000'
 'd abs(lon)' 
 'draw title MC_MODEL STORM stormid E-W CROSS SECT LAT=CENTER_LAT'
 'set string 1 bl 5'
 'set strsiz 0.10 0.10'
 'draw string 3.0 7.75 INIT YYYYMMDDHH Z for HOUR h FCST VALID vdate Z'
 'set string 2 c 18'
 'set strsiz 0.25 0.25'
 'draw string 5.8 5.0 *** WARNING ***'
 'draw string 5.8 4.5 STORM LATITUDE IS OUT OF DOMAIN 3'
 'draw string 5.8 4.0 or STORM DISSIPATED'
endif 
*-----------------------------------------------------------------
* draw NCEP logo  
xstart = 10.00
ystart = 0.05
hsiz = 0.09
vsiz = 0.09
cstr = 'NCEP Hurricane Forecast Project'
'set string 230 br 4 0'
'set strsiz 'hsiz' 'vsiz
'draw string 'xstart' 'ystart' 'cstr
* label on left side for hPa
'set string 230 br 5 90'
'set strsiz 0.10 0.10' 
'draw string 1.00 5.00 PRESSURE (hPa)' 
*----------------------------------------------------------------- 
'printim 'outfile' x1024 y768' 
'c' 
*-----------------------------------------------------------------
'quit'
