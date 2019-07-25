***********************************************************
*  hwrf_gr_swath.gs
***********************************************************
'reinit'
'set display color white'
'open ctl_file'  
'set xlopts 1 7 0.14'
'set ylopts 1 7 0.14'
'set xlint 5'
'set ylint 5'
'set parea 0.6 10.7 0.5 7.3'

print_lims('top')

'set gxout shaded' 
'run rgbset.gs'  
'set map 5 1 7'
***********************************************************
'set mpdset mres'
***********************************************************
* plot 10m wind swath 
*-----------------------------------------------------------
'enable print swath_10m_YYYYMMDDHH'
outfile='swath_10m.png'
print_lims(outfile)
'set grads off' 
*-----------------------------------------------------------
* make shaded plot of windspeed 
*-----------------------------------------------------------
'set gxout shaded' 
'set clevs 34 64 83 96 114 135 ' 
'set ccols 0 23 27 28  42 54 57'
*
'set stat on' 
'd wind10'
stats=result
line8=sublin(stats,8)
maxval=subwrd(line8,5) 
'set stat off' 
print_lims('before trakplotx')
'run trakplotx.gs statsin'
print_lims('before cbar')
'run cbarn.gs' 
print_lims('after cbar')
*
* plot isotachs in knots 
'set gxout contour' 
'set clevs 34 64 83 96 114 135 '
'set clab off'
'd wind10' 
*-----------------------------------------------------------
'set string 1 bl 8'
'set strsiz 0.21 0.21'
'draw string 2.5 7.9 MC_MODEL 10M MAX WIND(KTS) STORM '
'set string 1 bl 5'
'set strsiz 0.13 0.13' 
'draw string 2.5 8.3 INIT YYYYMMDDHH Z for HOUR h FCST VALID vdate Z' 
'draw string 0.50 7.50 START POS (FirstLat LAT, FirstLon LON)'
'draw string 4.75 7.50 FINAL POS (LastLat LAT, LastLon LON)'
'draw string 8.75 7.50 X=12 h POS' 
'draw string 4.0 0.05 MAX WIND (KTS) ' maxval
*-----------------------------------------------------------
* draw NCEP logo 
*----------------------------------------------------------- 
xstart = 10.50
ystart = 0.05
hsiz = 0.09
vsiz = 0.09
cstr = 'NCEP Hurricane Forecast Project'
'set string 230 br 4 0'
'set strsiz 'hsiz' 'vsiz
'draw string 'xstart' 'ystart' 'cstr
*-----------------------------------------------------------
'printim 'outfile' x1024 y768' 
'print'
'disable print'
'c'
********************************************************************
* plot storm-total precipitation  (shaded)
******************************************************************** 
'enable print swath_rain_YYYYMMDDHH' 
outfile=swath_rain.png  
'set grads off' 
'set gxout shaded'  
'set parea 0.6 10.7 0.5 7.5'
'set clevs  0  1  2  4  8 16 24 32 ' 
'set ccols 21 22 23 25 27 28 29 42 '  
'set xlint 5'
'set ylint 5'
'set stat on' 
'd rain'
stats=result
line8=sublin(stats,8)
maxval=subwrd(line8,5) 
'set stat off' 
'run trakplotx.gs statsin'
'run cbarn.gs' 
*-----------------------------------------------------------
* plot rainfall contours in inches 
*-----------------------------------------------------------
'set gxout contour' 
'set clevs 0 1 2 4 8 16 24 32' 
'd rain'
*-----------------------------------------------------------
'set string 1 bl 8'
'set strsiz 0.21 0.21'
'draw string 2.5 7.9 MC_MODEL TOTAL RAINFALL(IN) STORM '
'set string 1 bl 5'
'set strsiz 0.13 0.13' 
'draw string 2.5 8.25 INIT YYYYMMDDHH Z for HOUR h FCST VALID vdate Z' 
*-----------------------------------------------------------
* draw position label near top of plot  
'draw string 0.50 7.50 START POS (FirstLat LAT, FirstLon LON)'
'draw string 4.75 7.50 FINAL POS (LastLat LAT, LastLon LON)'
'draw string 8.75 7.50 X=12 h POS' 
'draw string 4.0 0.05 MAX RAINFALL (IN) ' maxval
*-----------------------------------------------------------
* draw NCEP logo  
xstart = 10.00
ystart = 0.05

hsiz = 0.09
vsiz = 0.09

cstr = 'NCEP Hurricane Forecast Project'
'set string 230 br 4 0'
'set strsiz 'hsiz' 'vsiz
'draw string 'xstart' 'ystart' 'cstr
*-----------------------------------------------------------
'printim 'outfile' x1024 y768' 
'print'
'disable print'
'c' 
*-----------------------------------------------------------
'quit'

function print_lims(where)
say 'GET LIMS AT 'where
'query gxinfo'
say result

* END OF hwrf_gr_swath.gs
