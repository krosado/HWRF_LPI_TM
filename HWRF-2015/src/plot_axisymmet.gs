'reinit'
'open hgtaxis.ctl'
'open vraxis.ctl'
'open grwind.ctl'
'open vtaxis.ctl'
'open thydro.ctl'
'open tmpaxis.ctl'
'open spfhaxis.ctl'
'open rhaxis.ctl'
'open vvaxis.ctl'
'open amaxis.ctl'

'set stat on'
'run myrgbset.gs'
'set parea 2 8 1.55 6.55'

'define ah = amean(fig.1,x=165,x=185,y=1,y=1)'
'define at = amean(fig.6,x=165,x=185,y=1,y=1)'
'define ath = amean(fig.5,x=165,x=185,y=1,y=1)'
'define ahv = amean(fig.7,x=165,x=185,y=1,y=1)'
'define atv = amean(fig.8,x=165,x=185,y=1,y=1)'
'set zlog on'
'set lev 1000 50'

*------------------------------------- Geopotential height

*------------------------------------- Absolute angular momentum
'c'
'set gxout shaded'
'set grads off'
'set xlab %g'
'set xlint 50'
'set clevs  -40 -35 -30 -25 -20 -15 -10 -5   0  5 10 15 20 25 30 35 40'
'set ccols 59  58  57  56  55  54  53  52  51 41 42 43 44 45 46 47 48 49'
'd fig.2*1.944'
 stats=result
 line8=sublin(stats,8)
 minv1=subwrd(line8,4)
 maxv1=subwrd(line8,5)
'run cbarn.gs 0.75 1 8.3 4'

'set gxout contour'
'set grads off'
'set ccolor 1'
'set cint 1000000'
'set clskip 2'
'd fig.10'
 stats=result
 line8=sublin(stats,8)
 minv2=subwrd(line8,4)
 maxv2=subwrd(line8,5)

'draw title Absolute Angular momentum'
'draw xlab Radial distance (km)'
'draw ylab Pressure (hPa)'

'set strsiz 0.12 0.13'
'draw string 1.4 0.7 FRANCISCO 26w, d23, Azimuthally averaged, 2013101712, 72 h FCST'
'draw string 1.4 0.47 Angular momentum (contour), Min='minv2', Max='maxv2' m`a2`n/s'
'draw string 1.4 0.24 Radial wind (shaded), Min='minv1', Max='maxv1' kts'
'printim fig27.png white'
pull a
*------------------------------------- radial angular momentum advection
'c'
'set gxout shaded'
'set grads off'
'set xlab %g'
'set xlint 50'
'set clevs  -300 -200 -100  0   50 100 200 300 400 500 600 700 800 900 '
'set ccols 54   53   52   51 22   23  24  25  26  27  28  29  30  31  32'
'd fig.2*-1*cdiff(fig.10,x)/10000'
 stats=result
 line8=sublin(stats,8)
 minv1=subwrd(line8,4)
 maxv1=subwrd(line8,5)
'run cbarn.gs 0.75 1 8.3 4'

'set gxout contour'
'set grads off'
'set ccolor 0'
'set cint 50'
'set clab off'
'd fig.2*-1*cdiff(fig.10,x)/10000'
'set clab on'

'draw title Radial angular momentum advection'
'draw xlab Radial distance (km)'
'draw ylab Pressure (hPa)'

'set strsiz 0.12 0.13'
'draw string 1.4 0.7 FRANCISCO 26w, d23, Azimuthally averaged, 2013101712, 72 h FCST'
'draw string 1.4 0.47 Angular momentum advection (shaded)'
'draw string 1.4 0.24 : Min='minv1', Max='maxv1' m`a2`n/s`a2`n'
'printim fig28.png white'

*------------------------------------- Secondary Circulation
'c'
'd fig.9'
 stats=result
 line8=sublin(stats,8)
 minv0=subwrd(line8,4)

'c'
'set gxout shaded'
'set grads off'
'set xlab %g'
'set xlint 50'
'set clevs  -40 -35 -30 -25 -20 -15 -10 -5   0  5 10 15 20 25 30 35 40' 
'set ccols 59  58  57  56  55  54  53  52  51 41 42 43 44 45 46 47 48 49'
'd fig.2*1.944'
 stats=result
 line8=sublin(stats,8)
 minv1=subwrd(line8,4)
 maxv1=subwrd(line8,5)
'run cbarn.gs 0.75 1 8.3 4'

'set gxout stream'
'set strmden 4'
'set grads off'
'd fig.2;(fig.9*-1)'
'set gxout contour'

'draw title Secondary Circulation'
'draw xlab Radial distance (km)'
'draw ylab Pressure (hPa)'

'set strsiz 0.12 0.13'
'draw string 1.4 0.7 FRANCISCO 26w, d23, Azimuthally averaged, 2013101712, 72 h FCST'
'draw string 1.4 0.47 Radial wind (shaded), Min='minv1', Max='maxv1' kts'
'draw string 1.4 0.24 Radial-vertical flow (streamline), Pressure velocity peak='minv0' Pa/s'
'printim fig12.png white'

*------------------------------------- Gradient wind

*------------------------------------- Tangential wind
'c'
'set cint 10'
'set grads off'
'set xlab %g'
'set xlint 50'
'set ccolor 1'
'd fig.4*1.944'
 stats=result
 line8=sublin(stats,8)
 minv1=subwrd(line8,4)
 maxv1=subwrd(line8,5)

'draw title Tangential wind'
'draw xlab Radial distance (km)'
'draw ylab Pressure (hPa)'

'set strsiz 0.12 0.13'
'draw string 1.4 0.7 FRANCISCO 26w, d23, Azimuthally averaged, 2013101712, 72 h FCST'
'draw string 1.4 0.47 Tangential wind (contour), Min='minv1', Max='maxv1' kts'
'printim fig14.png white'

*------------------------------------- Wind difference
'c'
'set grads off'
'set xlab %g'
'set xlint 50'
'set gxout shaded'
'set clevs  -15 -10 -5  0  5 10 15 20 25 30 35 40 45 50 55 60'
'set ccols 54  53  52 51 22 23 24 25 26 27 28 29 30 31 32 33 35 '

'd fig.4*1.944 - fig.3*1.944'
 stats=result
 line8=sublin(stats,8)
 minv1=subwrd(line8,4)
 maxv1=subwrd(line8,5)
'run cbarn.gs 0.75 1 8.3 4'

'set grads off'
'set gxout contour'
'set clab off'
'set cint 5'
'set ccolor 0'
'd fig.4*1.944 - fig.3*1.944'

'draw title Wind difference'
'draw xlab Radial distance (km)'
'draw ylab Pressure (hPa)'

'set strsiz 0.12 0.13'
'draw string 1.4 0.7 FRANCISCO 26w, d23, Azimuthally averaged, 2013101712, 72 h FCST'
'draw string 1.4 0.47 Tangential wind - Gradient wind, Min='minv1', Max='maxv1' kts'
'printim fig15.png white'

*------------------------------------- Hydrostatic balanced temperature

*------------------------------------- Temperature
'c'
'set gxout shaded'
'set grads off'
'set xlab %g'
'set xlint 50'
'set x 1 185'
'set clevs  -1  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14'
'set ccols 52 51 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36'
'd fig.6-at'
 stats=result
 line8=sublin(stats,8)
 minv1=subwrd(line8,4)
 maxv1=subwrd(line8,5)
'run cbarn.gs 0.75 1 8.3 4'

'set gxout contour'
'set grads off'
'd fig.6-273.15'
 stats=result
 line8=sublin(stats,8)
 minv2=subwrd(line8,4)
 maxv2=subwrd(line8,5)

'draw title Temperature'
'draw xlab Radial distance (km)'
'draw ylab Pressure (hPa)'

'set strsiz 0.12 0.13'
'draw string 1.4 0.7 FRANCISCO 26w, d23, Azimuthally averaged, 2013101712, 72 h FCST'
'draw string 1.4 0.47 Temperature deviation (shaded), Min='minv1', Max='maxv1' `ao`nC'
'draw string 1.4 0.24 Temperature (contour), Min='minv2', Max='maxv2' `ao`nC'
"draw string 8.56 1.730 '"
'printim fig17.png white'

*------------------------------------- Temperature difference

*------------------------------------- Humidity

*------------------------------------- Radial moisture flux convergence
'c'
'set gxout shaded'
'set grads off'
'set xlab %g'
'set xlint 50'
'set clevs  -0.3 -0.2 -0.1  0  0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0'
'set ccols 54   53   52   51 22   23  24  25  26  27  28  29  30  31   32'
'd fig.7*cdiff(fig.10,x)/10000 +fig.10*cdiff(fig.7,x)/10000'
 stats=result
 line8=sublin(stats,8)
 minv1=subwrd(line8,4)
 maxv1=subwrd(line8,5)
'run cbarn.gs 0.75 1 8.3 4'

'set gxout contour'
'set grads off'
'set ccolor 0'
'set clab off'
'set cint 0.1'
'd fig.7*cdiff(fig.10,x)/10000 +fig.10*cdiff(fig.7,x)/10000'
'set clab on'

'draw title Radial moisture flux convergence'
'draw xlab Radial distance (km)'
'draw ylab Pressure (hPa)'

'set strsiz 0.12 0.13'
'draw string 1.4 0.7 FRANCISCO 26w, d23, Azimuthally averaged, 2013101712, 72 h FCST'
'draw string 1.4 0.47 Specific humidity flux convergence (shaded)'
'draw string 1.4 0.24 : Min='minv1', Max='maxv1' kg/kgs'
'printim fig29.png white'

'quit'
