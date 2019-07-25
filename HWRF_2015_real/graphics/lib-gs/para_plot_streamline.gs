'reinit'
'open grads_c.ctl'
'open grads_p.ctl'
'set parea 0.3 10.6 1 7.5'
'set xlopts 1 7 0.13'
'set ylopts 1 7 0.13'
'set map 1 1 5'
'set lev 850'
'set mpdset hires'
'set grads off'
'set xlint 20'
'set ylint 20'
'rgbset.gs'
*
* read header infomation
*
rc=read('fort.10')
stormname=sublin(rc,2)
rc=read('fort.10')
stormid=sublin(rc,2)
rc=read('fort.10')
itime=sublin(rc,2)
rc=read('fort.10')
ftime=sublin(rc,2)
rc=read('fort.10')
ltime=sublin(rc,2)
*
* plot isotach
*
'set gxout shaded'
*'set clevs 16 20 24 28 32 36 40 50 60 70 80 90 100'
*'set ccols 0 9 14 4 11 5 13 3 10 7 12 8 2 6'
'set clevs  5 15 25 35 45 55 65 75 85 95 105 115 125 135 145 155'
'set ccols  0  0 21 23 25 27 28 29 42 52 56 81 85 87 73 75 78'
'd mag(ugrdprs.2,vgrdprs.2)*1.981'
'cbarn.gs 1 1 9.1 4.1'
*
* plot streamline
*
'set gxout stream'
'set strmden -5 0.5 0.07 1'
'set cthick 7'
'set ccolor 4'
'd ugrdprs;vgrdprs'
*
* legend and print out
*
'set strsiz 0.18'
'set string 1 c 7 0'
'draw string 5.5 7.7 HWRF COMBINE DOMAIN 'stormname' 'stormid
'set strsiz 0.16'
'set string 1 l 7 0'
'draw string 1.4 8.1 Initialized at 'itime' - 'ltime' (h) fsct valid at 'ftime
'set string 1 c 7 0'
'draw string 5.5 0.5 850 hPa STREAMLINE and ISOTACH (kts)'
'set strsiz 0.1'
'set string 1 r 5 0'
'draw string 9.9 0.14 HWRF Project at NOAA/NWS/NCEP/EMC'
'printim out.png x1024 y768 white'
*'enable print out.gmf'
*'print'
*'disable print'
*'!gxps -c -i out.gmf -o out.ps'
*'!convert -rotate 90 -density 45 -trim -geometry 450x450 out.ps out.png'
