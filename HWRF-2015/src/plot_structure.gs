'reinit'
'open ctl_file'
'open vtgrd.ctl'
'open vrgrd.ctl'
'open vtgrd10m.ctl'
'open vrgrd10m.ctl'

'set stat on'
'run myrgbset.gs'
'set parea 2 8 1.55 6.55'
'set map 15'
'set gxout barb'
'set xlint 1'
'set ylint 1'

*============================================== Surface fields
*----------------- Latent heat flux
'c'
'd mag(UGRD10m*1.944,VGRD10m*1.944)'
 stats=result
 line8=sublin(stats,8)
 minv2=subwrd(line8,4)
 maxv2=subwrd(line8,5)
'c'
'set grads off'
'set gxout shaded'
'set clevs  100 200 300 400 500 600 700 800 900 1000 1100 1200 1300 1400 1500 1600'
'set ccols 0  21 22 23 24 25 26 27 28 29  30  31  32  33  34  35  36'
'd lhtflsfc'
 stats=result
 line8=sublin(stats,8)
 minv1=subwrd(line8,4)
 maxv1=subwrd(line8,5)
'run cbarn.gs 0.75 1 8.3 4'

'set ccolor 3'
'set grads off'
'set digsize 0.05'
'd skip(UGRD10m*1.944,27);VGRD10m*1.944'

'draw title Latent heat flux'
'set strsiz 0.12 0.13'
'draw string 2.1 0.9 FRANCISCO 26w, d23, surface, 2013101712, 72 h FCST'
'draw string 2.1 0.67 Latent heat flux (shaded), Min='minv1', Max='maxv1' W/m`a2`n'
'draw string 2.1 0.44 10m wind (bar), Min='minv2', Max='maxv2' kts'
"draw string 8.74 6.22 '"
'printim fig26.png white'

*----------------- 10m wind speed

*----------------- surface temp.
'c'
'set gxout shaded'
'set clevs   15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31'
'set ccols 59  58  57  56  55  54  53  52  51  41  42  43  44  45  46  47  48  49'
'set grads off'
'd TMPsfc-273.15'
 stats=result
 line8=sublin(stats,8)
 minv1=subwrd(line8,4)
 maxv1=subwrd(line8,5)
'run cbarn.gs 0.75 1 8.3 4'

'set gxout contour'
'set clab off'
'set clevs 14 14.5 15 15.5 16 16.5 17 17.5 18 18.5 19 19.5 20 20.5 21 21.5 22 22.5 23 23.5 24 24.5 25 25.5 26 26.5 27 27.5 28 28.5 29 29.5 30 30.5 31 31.5 32'
'set cint 0.5'
'set cstyle 3'
'set ccolor 0'
'set grads off'
'd TMPsfc-273.15'

'set clab on'
'set cint 5'
'set ccolor 1'
'set grads off'
'd prmslmsl*0.01'
 stats=result
 line8=sublin(stats,8)
 minv2=subwrd(line8,4)
 maxv2=subwrd(line8,5)

'draw title Surface Temperature'
'set strsiz 0.12 0.13'
'draw string 2.1 0.9 FRANCISCO 26w, d23, surface, 2013101712, 72 h FCST'
'draw string 2.1 0.67 Surface temperature (shaded), Min='minv1', Max='maxv1' `ao`nC'
'draw string 2.1 0.44 Sea level pressure (contour), Min='minv2', Max='maxv2' hPa'
'printim fig03.png white'

*============================================== 200 hPa fields
'set lev 200'
*----------------- Divergence

*============================================== 850 hPa fields
'set lev 850'
*----------------- Geopotential height & Wind speed

*============================================== 700 hPa fields
'set lev 700'
*----------------- Pressure velocity
'c'
'set grads off'
'set gxout shaded'
'set clevs  -16 -14 -12 -10 -8 -6 -4 -2  0  2  4  6  8 10 12 14 16'
'set ccols 49  48  47  46  45 44 43 42 41 51 52 53 54 55 56 57 58 59'
'd vvelprs'
 stats=result
 line8=sublin(stats,8)
 minval=subwrd(line8,4)
 maxval=subwrd(line8,5)
'run cbarn.gs 0.75 1 8.3 4.0'

'draw title Pressure velocity'
'set strsiz 0.12 0.13'
'draw string 2.1 0.9 FRANCISCO 26w, d23, 700 hPa, 2013101712, 72 h FCST'
'draw string 2.1 0.67 Pressure velocity (shaded), Min='minval', Max='maxval' Pa/s'
"draw string 8.675 2.55 '"
'printim fig05.png white'

*============================================== 900 hPa fields
'set lev 900'
*----------------- Humidity
'c'
'set grads off'
'set gxout shaded'
'set clevs  10 20 30 40 50 60 70 80 90'
'set ccols 56 55 54 52 51 41 43 44 46 47'
'd RHprs'
 stats=result
 line8=sublin(stats,8)
 minv1=subwrd(line8,4)
 maxv1=subwrd(line8,5)
'run cbarn.gs 0.75 1 8.3 4'

'set grads off'
'set gxout contour'
'set ccolor 0'
'set cint 2'
'set clab forced'
'set clskip 5'
'd SPFHprs*1000'
 stats=result
 line8=sublin(stats,8)
 minv2=subwrd(line8,4)
 maxv2=subwrd(line8,5)
'draw title Humidity'
'set strsiz 0.12 0.13'
'draw string 2.1 0.9 FRANCISCO 26w, d23, 900 hPa, 2013101712, 72 h FCST'
'draw string 2.1 0.67 Relative humidity (shaded), Min='minv1', Max='maxv1' %'
'draw string 2.1 0.44 Specific humidity (contour), Min='minv2', Max='maxv2' g/kg'
'printim fig25.png white'

*----------------- Tangential wind 

*----------------- Radial wind

*============================================== Surface fields
*----------------- 10m tangential wind

*----------------- 10m radial wind

*============================================== Cross section
*----------------- Zonal wind speed & Secondary circulation
'set lat 18.991'
'q dims'
 stats=result
 line3=sublin(stats,3)
 llat=subwrd(line3,6)

'set gxout stream'
'set zlog on'
'set lev 1000 50'

*----------------- Horizontal wind speed
'c'
'set grads off'
'set gxout shaded'
'set clevs  10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160 '
'set ccols 0  21 22 23 24 25 26 27 28 29  30  31  32  33  34  35  36'
'd mag(ugrdprs*1.944,vgrdprs*1.944)'
'run cbarn.gs 0.75 1 8.3 4.0'

'set grads off'
'set gxout contour'
'set ccolor 0'
'set cint 10'
'set clab off'
'd mag(ugrdprs*1.944,vgrdprs*1.944)'
 stats=result
 line8=sublin(stats,8)
 maxval=subwrd(line8,5)
'set clab on'

'draw title Horizontal wind speed'
'draw xlab Longitude (degree)'
'draw ylab Pressure (hPa)'
'set strsiz 0.12 0.13'
'draw string 1.4 0.6 FRANCISCO 26w, d23, lat:18.991, 2013101712, 72 h FCST'
'draw string 1.4 0.37 Horizontal wind speed (shaded), Max='maxval' kts'
'draw string 2.2 3.9 WEST'
'draw string 7.45 3.9 EAST'
'draw string 6.1 6.4 Latitude: ' llat

'printim fig07.png white'
'quit'
