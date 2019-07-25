************************************************************************
* hwrf_gr_goes.gs
************************************************************************
'open ctl_file'  
*---------------------------------------------------------------------
'set display color white' 
'set grads off' 
'set xlopts 1 7 0.14'
'set ylopts 1 7 0.14'
'set xlint 10'
'set ylint 10'
*---------------------------------------------------------------------
* Plot Parent WV Simulated MTSAT TB
*---------------------------------------------------------------------
outfile='par_goesWV_fHOUR.png'
'set map 1 1 4'
'set clopts -1 -1 0.09'
'set gxout shaded'
'run mikewv.gs'
'set parea 0.5 9.8 0.5 8.0'
'set mpdset mres'
'd BRTMP2_3cm-273.16'
'draw title MC_MODEL STORM stormid Simulated MTSAT WV TB (K)'
'set string 1 bl 7'
'set strsiz 0.10 0.10'
'draw string 3.0 8.05 INIT YYYYMMDDHH Z for HOUR h FCST VALID vdate Z'
'run cbarn2.gs 1.0 1 0.50'
'printim 'outfile' x1024 y768 white'
'clear'
*---------------------------------------------------------------------
* Plot Parent IR Simulated MTSAT TB
*---------------------------------------------------------------------
outfile='par_goesIR_fHOUR.png'
'set gxout shaded'
'run mikeir.gs'
'set parea 0.5 9.8 0.5 8.0'
'set mpdset mres'
'd BRTMP2_1cm-273.16'
'draw title MC_MODEL STORM stormid Simulated MTSAT IR TB (K)'
'set string 1 bl 7'
'set strsiz 0.10 0.10'
'draw string 3.0 8.05 INIT YYYYMMDDHH Z for HOUR h FCST VALID vdate Z'
'run cbarn2.gs 1.0 1 0.50'
'printim 'outfile' x1024 y768 white'
'clear'
*----------------------------------------
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
