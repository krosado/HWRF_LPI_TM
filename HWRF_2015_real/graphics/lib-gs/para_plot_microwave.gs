*----------------------------------------------------------------
* hwrf_gr_microwave.gs
*----------------------------------------------------------------
'open ctl_file'  
'set display color white' 
'set grads off' 
'set xlopts 1 7 0.14'
'set ylopts 1 7 0.14'
'set xlint 10'
'set ylint 10'
*----------------------------------------------------------------
* Plot Parent Simulated AMSR-E BT for HWRF for 37GHz 
*----------------------------------------------------------------
 outfile='microwave_37_fHOUR.png'
'set map 1 1 4'
'set clopts -1 -1 0.09'
'set gxout shaded'
'run colormap-37ghz-withland.gs'
'set parea 0.5 9.8 0.5 8.0'
'set mpdset mres'
*'set rbcols'
*'set clevs 100 110 120 130 140 150 160 170 180 190 200 210 220 230 240 250 260 270 280 290 300 310 320'
*'d NLATtoa'
'd BRTMP117_15cm'
'draw title MC_MODEL STORM stormid Simulated Microwave TB (K) H 37 GHz'
'set string 1 bl 7'
'set strsiz 0.10 0.10'
'draw string 3.0 8.05 INIT YYYYMMDDHH Z for HOUR h FCST VALID vdate Z'
'run cbarm.gs'
'printim 'outfile' x1024 y768'
'clear'
*-------------------------------------------------------------
* Plot Parent Simulated AMSR-E BT for HWRF for 89GHz
*------------------------------------------------------------------------
 outfile='microwave_89_fHOUR.png'
'set gxout shaded'
'run colormap-89ghz-withland.gs'
'set parea 0.5 9.8 0.5 8.0'
'set mpdset mres'
*'d ICMRtoa'
'd BRTMP117_18cm'
'draw title MC_MODEL STORM stormid Simulated Microwave TB (K) H 91 GHz'
'set string 1 bl 7'
'set strsiz 0.10 0.10'
'draw string 3.0 8.05 INIT YYYYMMDDHH Z for HOUR h FCST VALID vdate Z'
'run cbarm.gs'
'printim 'outfile' x1024 y768' 
'clear'
*------------------------------------------------------------------------
* draw NCEP logo  
xstart = 10.00
ystart = 0.05

hsiz = 0.09
vsiz = 0.09

cstr = 'NCEP Hurricane Forecast Project'
'set string 230 br 4 0'
'set strsiz 'hsiz' 'vsiz
'draw string 'xstart' 'ystart' 'cstr
*-------------------------------------------------------------
'quit'
