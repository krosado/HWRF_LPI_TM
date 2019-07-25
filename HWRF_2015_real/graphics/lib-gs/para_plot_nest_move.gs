'reinit'
'open grads_n.ctl'
'set parea 0.8 9.9 1 7.5'
'set xlopts 1 7 0.13'
'set ylopts 1 7 0.13'
'set map 15 1 7'
'set lev 850'
'set lon 232 340'
'set mpdset hires'
'set grads off'
'set xlint 5'
'set ylint 5'
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
rc=read('fort.10')
iline=sublin(rc,2)
slat=subwrd(iline,1)
elat=subwrd(iline,2)
rc=read('fort.10')
iline=sublin(rc,2)
slon=subwrd(iline,1)
elon=subwrd(iline,2)
*rc=read('fort.10')
*vmax=sublin(rc,2)
'set lat 'slat' 'elat
'set lon 'slon' 'elon
*
* plot isotach
*
'define spd=mag(ugrdprs,vgrdprs)*1.98'
'define spd10m=mag(ugrd10m,vgrd10m)*1.98'
'define smax=max(max(spd,x=1,x=440),y=1,y=360)'
'd smax'
rc=sublin(result,1)
tmax=subwrd(rc,4)
vmax=math_nint(tmax)
if (vmax > 0 & vmax < 270)
 'set gxout shaded'
 'set clevs  5 15 25 35 45 55 65 75 85 95 105 115 125 135 145 155'
 'set ccols  0  0 21 23 25 27 28 29 42 52 56 81 85 87 73 75 78'
 'd spd'
*'cbarn.gs 1 1 10 4.1'
*
* plot streamline
*
 'set gxout stream'
 'set strmden 5 0.5 0.07 1'
 'set cthick 7'
 'set ccolor 4'
 'd ugrdprs;vgrdprs'
else
 'set ccolor 0'
 'd lat'
 vmax="Not defined"
endif
*
* plot track now. Need an input fort.32
*
'para_plot_single_track.gs'
*
* legend and print out
*
'set strsiz 0.18'
'set string 1 c 7 0'
'draw string 5.5 7.7 HWRF MOVING NEST 'stormname' 'stormid
'set strsiz 0.16'
'set string 1 l 7 0'
'draw string 1.2 8.1 Initialized at 'itime': 'ltime' (h) fsct. Valid at 'ftime
'set string 1 r 7 0'
'draw string 9.2 0.5 850mb STREAMLINE and ISOTACH (kts) - VMAX 'vmax' (kts)'
*'set strsiz 0.1'
*'set string 1 r 5 0'
*'draw string 9.5 0.14 HWRF Project at NOAA/NWS/NCEP/EMC'
'printim out.png x1024 y768 white'
