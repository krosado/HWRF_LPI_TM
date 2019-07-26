'reinit'
'open wind_po.ctl'
'open radius.ctl'

'set stat on'
'run myrgbset.gs'
'set parea 2 8 1.55 6.55'

*----------------- 10m wind speed in polar coord.
'set mproj scaled'
'set xlab %g'
'set xlint 45'
'set ylab %g'
'set mpdraw off'
'set grads off'
'set gxout shaded'
'set clevs  10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160 '
'set ccols 0  21 22 23 24 25 26 27 28 29  30  31  32  33  34  35  36'
'd fig.1 *1.944'
 stats=result
 line8=sublin(stats,8)
 minv1=subwrd(line8,4)
 maxv1=subwrd(line8,5)


if(maxv1 < 10000)
 'run cbarn.gs 0.75 1 8.3 4'
 'draw xlab Azimuth (degree)'
 'draw ylab Radius (km)'

 'set y 1'
 'set vrange 0 400'
 'set xlab off'
 'set ylab off'
 'set cthick 12'
 'set ccolor 2'
 'set cmark 0'
 'd fig.2(t=1)'
 'set ccolor 4'
 'set cmark 0'
 'd fig.2(t=2)'
 'set ccolor 13'
 'set cmark 0'
 'd fig.2(t=3)'

 'set line 2 1 10'
 'draw line 6.3 5.7 6.8 5.7'
 'draw string 6.9 5.7 RMW'
 'set line 4 1 10'
 'draw line 6.3 5.95 6.8 5.95'
 'draw string 6.9 5.95 50 kt radius'
 'set line 13 1 10'
 'draw line 6.3 6.2 6.8 6.2'
 'draw string 6.9 6.2 34 kt radius'

 'draw title 10m wind speed in polar coord.'
 'set strsiz 0.12 0.13'
 'draw string 1.4 0.8 FRANCISCO 26w, d23, 10 m, 2013101712, 72 h FCST'
 'draw string 1.4 0.57 10m wind speed (shaded), Min='minv1', Max='maxv1' kts'

else
 'draw title 10m wind speed in polar coord.'
 'set strsiz 0.12 0.13'
 'draw string 1.4 0.8 FRANCISCO 26w, d23, 10 m, 2013101712, 72 h FCST'
 'draw string 1.4 0.57 10m wind speed (shaded)'
 'set string 2 c 18'
 'set strsiz 0.25 0.25'
 'draw string 5 4.0 *** WARNING ***'
 'draw string 5 3.5 ST0RM CENTER IS FAR AWAY FROM'
 'draw string 5 3.1 THE NEST DOMAIN CENTER'
endif

'printim fig23.png white'

'quit'
