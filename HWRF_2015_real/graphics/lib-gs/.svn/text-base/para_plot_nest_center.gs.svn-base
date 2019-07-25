**************************************************
* hwrf_gr_nest_center.gs
**************************************************
'open ctl_file'  
 'set display color white' 
 'set xlopts 1 7 0.14'
 'set ylopts 1 7 0.14'
 'run rgbset.gs'  
 outfile='nest_fHOUR.png'
 'set grads off' 
*---------------------------------------------------
* plot 10 m surface wind in knots (shaded and contours) 
*---------------------------------------------------
'set parea 0.6 10.7 1 7.3'
'set xlint 1'
'set ylint 1'
'define ukts=1.944*UGRD10m'
'define vkts=1.944*VGRD10m'  
'set gxout shaded'  
'set clevs 5 15 25 35 45 55 65 75 85 95 105 115 125 135 145 155' 
'set ccols 0  0 21 23 25 27 28 29 42 52 56 81 85 87 73 75 78'
'd mag(ukts,vkts)'
'set gxout contour' 
'set clevs 25 35 45 55 65 75 85 95 105 115 125 135 145 155' 
'set stat on'
'd mag (ukts,vkts)'
stats=result
line8=sublin(stats,8) 
maxval=subwrd(line8,5) 
*---------------------------------------------------
* draw wind barbs
*--------------------------------------------------- 
'set gxout barb'
'set ccolor 1'
'd skip(ukts,16,16);vkts' 
'set stat off' 
*'run cbarn.gs' 
'set string 1 r 6'
'set strsiz 0.18 0.18'
'draw string 9.85 0.5 STORM stormid SFC Pressure (hPa) AND 10 M WIND (kts)'
'set string 1 c 6'
'set strsiz 0.14 0.14'
'draw string 5.6 7.7 INIT YYYYMMDDHH Z for HOUR h FCST VALID vdate Z'
*-------------------------------------------------------------
* plot surface pressure in hPa 
*-------------------------------------------------------------
'define pmb=PRMSLmsl/100.' 
'set stat on' 
'set clevs 960 964 968 972 976 980 984 988 992 996 1000 1004 1008 1012 1016 1020 1024' 
'd pmb' 
 stats=result
 line8=sublin(stats,8)
 minval=subwrd(line8,4)
'set stat off' 
'set string 1 bc 5'
'set strsiz 0.14 0.14'
'draw string 7.5 7.40 MAX WIND (kts) ' maxval
'draw string 3.55 7.40 MIN PSR (hPa) ' minval
*-------------------------------------------------------------
* draw NCEP logo  
*
*xstart = 10.00
*ystart = 0.05
*hsiz = 0.09
*vsiz = 0.09
*cstr = 'NCEP Hurricane Forecast Project'
*'set string 230 br 4 0'
*'set strsiz 'hsiz' 'vsiz
*'draw string 'xstart' 'ystart' 'cstr
*-------------------------------------------------------------
'printim 'outfile' x1024 y768' 
'quit'
