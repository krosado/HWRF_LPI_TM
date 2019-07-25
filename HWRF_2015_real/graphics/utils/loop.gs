'reinit'
'open grads.ctl'
'set xlopts 1 7 0.14'
'set ylopts 1 7 0.14'
*'set lat 0 63'
*'set lon 240 325'
'set parea 2 9 1 7.5'
'set annot 0'
'set mpdset hires'
*
* plot vorticity first
*
'set grads off'
'set xlint 30'
'set ylint 20'
'set lat 0 63'
'set lon 233 342'
'set gxout shaded'
'set clevs 2 4 6 8 10 12 14 16 18 20 22 24 26'
'set ccols 0 9 14 4 11 5 13 3 10 7 12 8 2 6'
'd mag(ugrd10m,vgrd10m)'
'cbarn.gs 1 0'
'set gxout barb'
'track_plot1.gs'
'track_plot2.gs'
*'d skip(ugrd10m,40,40);vgrd10m'
'set annot 1'
'q time'
result1 = sublin(result,1)
wtime = subwrd(result1,3)
'draw title HWRF forecast @ 'wtime' UTC time'
'printim out.gif'

