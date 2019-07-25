function main(inpargs)

*---------------------------------------------------*
* This script was written by Tim Marchok (GFDL)
* timothy.marchok@noaa.gov  609-452-6534
*---------------------------------------------------*

'reinit'
'open plottrak.ctl'
*'open ensplot_any.ctl'
'clear'
'reset'
'clear events'

'q dims'
drec = sublin(result,5)
_zdate = subwrd(drec,6)

_userid = subwrd(inpargs,1)
_basin  = subwrd(inpargs,2)
_yyyy   = subwrd(inpargs,3)

_rundir="/gpfs/blhome/shaowu/HWRF/src/gfdl-vortextracker/trk_plot/plottrak/"
_netdir="/gpfs/blhome/shaowu/HWRF/src/gfdl-vortextracker/trk_plot/plottrak/tracks"

say 'Top of script, _userid= '_userid
say '               _basin=  '_basin
say '               _yyyy=   '_yyyy

*------------------------------------------*
* In this first section are many defaults
* which are set for various display 
* features.  Many of these can be reset
* using the various buttons on the 
* graphical interface....
*------------------------------------------*

_startdate = 9999
_freshcascade = yes
_plotfcst  = yes
_plotverif = yes
_trkfcsttype = single
_trkmrkrintrvl = every12
_trkmrkrint    = 12
_trkverfintrvl = every12
_vrfmrkrint = 12
_trkverflen = next120
_legendloc = upright
_modcoltype = rainbow
_fcstlen = 120
_verifcol = 1
_mapcol = 5
_gridcol = 15
_gridstyle = 5
_zoom = no
_outcount = 0
_drawsig = no
_plotstrike = no
_strikenm = 65
_strikebar = yes
_strikehr = 00
_plottype = track
_intdisp  = windonly
_insetloc = upleft
_insetxpct = 0.40
_insetypct = 0.40

*------------------------------------------*

'set font 0'
'set lev 850'
*'set lev 1'
'set mpdset hires'
'set map '_mapcol' 1 4'
'set grid on '_gridstyle' '_gridcol

rc = rdstorm()
rc = mainbox()
rc = gettrkopts()
rc = modnames()
rc = settrkcols()
rc = setbtncols()
_randnum = getrandom()

* 'set vpage 0 11 0 8.5'
* 'set parea 1.6 10.95 0.9 7.7'

_par_left  =  1.6
_par_right = 10.55
_par_bot   =  1.1
_par_top   =  7.7
'set parea '_par_left' '_par_right' '_par_bot' '_par_top

while (1)

  'clear'
  rc = initmenu()
  rc = modelsoff()

  while (1)

    'q pos'
    mousenum = subwrd(result,5)
    widclass = subwrd(result,6)
    button   = subwrd(result,7)

*    say result

    if (button = 1)
*     "Main" dropmenu....
      item   = subwrd(result,8)
      choice = _mainfunc.item
      if (choice = "return")
        say "!!! Exiting script...."
        return
      else
        if (choice = "quit")
          say "!!! Quitting GrADS...."
          quit
        else
*          say 'calling process_main, choice= 'choice
          rc = process_main(choice)
        endif
       endif
    endif

    if (button = 2)
*     "Storms" dropmenu....
      _stlistnum = subwrd(result,8)
*      say 'storms dropmenu, _stlistnum= '_stlistnum
      if (_stlistnum > 0)
        _freshcascade = yes
        _startdate = 9999
        rc = rddates(_stlistnum)
        rc = redomenu(yes)
*        rc = modelsoff()
*        rc = printmodstat()
      endif
    endif

    if (button = 3)
*     "Dates" dropmenu....
      cnum = subwrd(result,9)
      citem = subwrd(result,10)
      if (citem > 0)
        _startdate = _ymdh.cnum.citem
        _zoom = no
        say '_startdate= '_startdate
      endif
    endif

    if (button = 4)
*     "Fcst/Obs" dropmenu....
      item = subwrd(result,8)
      rc = gettrkbtn(result)
      say ' '
      say 'trkfcsttype= '_trkfcsttype
      say 'trkverfintrvl= '_trkverfintrvl
      say 'trkverflen= '_trkverflen
      say 'trkmrkrint= '_trkmrkrint
    endif

    if (button = 5)
*     "Opts_1" dropmenu....
      item = subwrd(result,8)
      rc = getopts1(result)
*      say 'Opts_1 menu, item choice = 'item
    endif

*   NOTE: button 6 is used by the rband in rbandzoom.

    if (button = 7)
*     "Opts_2" dropmenu....
      rc = getopts2(result)
    endif

    if (button > 100 & button < 200)
      rc = clickmodel(button)
    endif

    if (button = 201)
      rc = makeplot()
    endif

    if (button = 202)
      rc = rbandzoom()
    endif

    if (button = 203)
      rc = zoomin5()
    endif
 
    if (button = 204)
      rc = zoomout5()
    endif
 
    if (button = 205)
      rc = zoomorig()
    endif

    if  (button >= 206 & button <= 211)
      rc = steptime(button)
      say 'would be doing +6/-6 buttons now....'
    endif
 
    if (button = 98)
      say '!!! Exiting script....'
      'redraw button 98 0'
      return
    endif

    if (button = 99)
      say '!!! QUITTING GrADS....'
      'redraw button 99 0'
      quit
    endif

  endwhile

endwhile

return

*---------------------------
*
*---------------------------
function makeplot()

if (_plotfcst = yes)
  rc = makeflist()
  if (rc = 99)
    return
  endif
endif
if (_plotverif = yes)
  rc = makevlist()
endif

say 'Top of makeplot, _zoom= '_zoom

if (_zoom = no)
  say 'makeplot no b4 getbounds: slat= '_southlat' nlat= '_northlat' wlon= '_westlon' elon= '_eastlon
  rc = getbounds(1)
  say 'makeplot no after getbounds: slat= '_southlat' nlat= '_northlat' wlon= '_westlon' elon= '_eastlon
  'set lat '_southlat' '_northlat
  'set lon '_westlon' '_eastlon
else
  if (_zoom = yes)
    say 'makeplot yes: slat= '_southlat' nlat= '_northlat' wlon= '_westlon' elon= '_eastlon
    'set lat '_southlat' '_northlat
    'set lon '_westlon' '_eastlon
  else
    if (_zoom = original)
      say 'makeplot orig: slat= '_southlat' nlat= '_northlat' wlon= '_westlon' elon= '_eastlon
      'set lat '_origslat' '_orignlat
      'set lon '_origwlon' '_origelon
    endif
  endif
endif

'clear graphics'
'set grads off'

strkrc = 999
if (_plottype = track | _plottype = trkinset)

  'set mpdraw on'

  if (_plotstrike = yes)
    say '+++ calling plotstrk'
    strkrc = plotstrk()
  else
    say '!!! NOT CALLING plotstrk'
  endif

  if (strkrc != 0)
    'set dfile 1'
    'q dims'
    say result
    'set time '_zdate
    'set black -10000 10000'
    'set gxout contour'
    'set clab off'
    'd u'
  endif

endif

*-----------------------------------------------------------
* For an intensity plot, we need to redefine the x- and
* y-axes in order to make a linear plot....
*-----------------------------------------------------------

if (_plottype = intensity)

  say 'in makeplot, _Tmin= '_Tmin'  _Tmax= '_Tmax
  say 'in makeplot, _Vmin= '_Vmin'  _Vmax= '_Vmax
  say 'in makeplot, _Pmin= '_Pmin'  _Pmax= '_Pmax

  'set xlopts 1 4 0.12'
  'set xaxis '_Tmin' '_Tmax' 12'

  if (_intdisp = mslponly)
    _nameprefix="Pmin"
    'set yaxis '_Pmin' '_Pmax' 10'
    'set ylpos 0 l'
  endif
  if (_intdisp = windonly)
    _nameprefix="Vmax"
    'set yaxis ' _Vmin' '_Vmax' 10'
    'set ylpos 0 l'
  endif

  'set black -10000 10000'
  'set mpdraw off'
  'set grads off'
  'd u'

endif
  
*-----------------------------------------------------------
* For either a regular track plot, a regular intensity plot,
* or the track portion of a track inset plot, this next bit
* of code is always executed...
*-----------------------------------------------------------

rc = getgxinfo()
if (_plotfcst = yes)
  rc = drawfcst(1)
endif
if (_plotverif = yes)
  rc = drawverif(1)
endif

if (_plottype = track | _plottype = trkinset)
*  rc = markaust()
endif

*-----------------------------------------------------------
* If we have a track inset plot, then we now run the 
* following bit of code to create the inset intensity
* plot.  The track portion of the plot was already 
* created in the calls to drawfcst and drawverif above.
*-----------------------------------------------------------

if (_plottype = trkinset)

  say 'in makeplot trkinset, _Tmin= '_Tmin'  _Tmax= '_Tmax
  say 'in makeplot trkinset, _Vmin= '_Vmin'  _Vmax= '_Vmax
  say 'in makeplot trkinset, _Pmin= '_Pmin'  _Pmax= '_Pmax
 
  'q dims'
  say result

  xlin = sublin(result,2)
  ylin = sublin(result,3)
  holdwest = subwrd(xlin,6)
  holdeast = subwrd(xlin,8)
  holdsouth = subwrd(ylin,6)
  holdnorth = subwrd(ylin,8)

  rc = getbounds(2)
  'set lat '_southlat' '_northlat
  'set lon '_westlon' '_eastlon

  rc = prepinset()

  'set xlopts 1 4 0.08'
  'set ylopts 1 4 0.08'
  'set grid horizontal'

  'set xaxis '_Tmin' '_Tmax' 12'

  if (_intdisp = mslponly)
    _nameprefix="Pmin"
    'set yaxis '_Pmin' '_Pmax' 10'
    'set ylpos 0 l'
  endif
  if (_intdisp = windonly)
    _nameprefix="Vmax"
    'set yaxis ' _Vmin' '_Vmax' 10'
    'set ylpos 0 l'
  endif

  'set black -10000 10000'
  'set mpdraw off'
  'set grads off'
  'd u'

  rc = getgxinfo()
  if (_plotfcst = yes)
    rc = drawfcst(2)
  endif
  if (_plotverif = yes)
    rc = drawverif(2)
  endif

  'set parea '_par_left' '_par_right' '_par_bot' '_par_top

* The purpose of the display in this next section is simply to 
* return the "parea" definitions to the bigger track
* dimensions so that the plot title and the labelling under
* the x-axis are put under the big plot and not still under
* the track inset plot that we just made.

  'clear events'
  'set lat 'holdsouth' 'holdnorth
  'set lon 'holdwest' 'holdeast
  'set black -10000 10000'
  'set mpdraw off'
  'set grid off'
  'set xlab off'
  'set ylab off'
  'set frame off'
  'set grads off'
  'd u'
  'set grid on'
  'set mpdraw on'
  'set xlab on'
  'set ylab on'
  'set frame on'

* Be sure to now set xlopts and ylopts back to their 
* default values....

  'set xlopts 1 4 0.12'
  'set ylopts 1 4 0.12'

  say 'Near end of makeplot, q dims results follow: '
  'q dims'
  say result

  _southlat = holdsouth
  _northlat = holdnorth
  _westlon  = holdwest
  _eastlon  = holdeast

endif

rc = drawtitle()
if (_drawsig = yes)
  rc = plotsig()
endif

* rc = testcorner()

rc = redomenu(yes)

return

*---------------------------
*
*---------------------------
function markaust()

'query ll2xy 262.15 30.38'
xdum  = sublin(result,1)
xaust = subwrd(xdum,1)
yaust = subwrd(xdum,2)
'set line 8 1 6'
'draw mark 1 'xaust' 'yaust' 0.14'
'draw mark 2 'xaust' 'yaust' 0.14'

'query ll2xy 264.943 29.14'
xdum  = sublin(result,1)
xaust = subwrd(xdum,1)
yaust = subwrd(xdum,2)
'set line 8 1 6'
'draw mark 1 'xaust' 'yaust' 0.14'
'draw mark 2 'xaust' 'yaust' 0.14'

'query ll2xy 264.493 30.203'
xdum  = sublin(result,1)
xaust = subwrd(xdum,1)
yaust = subwrd(xdum,2)
'set line 8 1 6'
'draw mark 1 'xaust' 'yaust' 0.14'
'draw mark 2 'xaust' 'yaust' 0.14'

return

*---------------------------
*
*---------------------------
function getbounds(plotnum)

* This function looks through the forecast tracks points
* and/or the observed track points and figures out 
* which points are the farthest N,E,S and W.  Remember, 
* we are using degrees positive westward, the TPC standard.

say 'top of getbounds, plotnum= 'plotnum

_northlat = -99.0 
_southlat =  99.0
_westlon  =   0.0
_eastlon  = 361.0

timeendf = -9999.0
timebegf =  9999.0
timeendo = -9999.0
timebego =  9999.0

pmax   = -9999
pmin   =  9999
vmax   = -9999
vmin   =  9999

fcstfile = _netdir%flist
verffile = _netdir%vlist

say 'fcstfile= 'fcstfile
say 'verffile= 'verffile

ict = 1

if (_plotfcst = yes)

  while (1)
  
    res = read(fcstfile)
    rc  = sublin(res,1)
    if(rc != 0)
      if(rc = 2)
        say 'End of forecast track file '
        say ' '
        break
      endif
      if(rc = 1); say 'rc=1: OPEN ERROR FOR 'fcstfile; endif
      if(rc = 8); say 'rc=8: 'fcstfile' OPEN FOR WRITE ONLY'; endif
      if(rc = 9); say 'rc=9: I/O ERROR FOR 'fcstfile; endif
      return 99
    endif

    rec = sublin(res,2)
*    say rec

    model   = subwrd(rec,2)
    thour   = subwrd(rec,3)
    ylat    = subwrd(rec,4)
    xlon    = subwrd(rec,5)
    adjfhr  = subwrd(rec,7)
    maxwind = subwrd(rec,8)
    minprs  = subwrd(rec,9)

    if (adjfhr > timeendf) 
      timeendf = adjfhr 
    endif   
    if (adjfhr < timebegf) 
      timebegf = adjfhr 
    endif

    if (ylat > _northlat)
      _northlat = ylat
    endif
    if (ylat < _southlat)
      _southlat = ylat
    endif
    if (xlon > _westlon)
      _westlon = xlon
    endif
    if (xlon < _eastlon)
      _eastlon = xlon
    endif

    if (maxwind != ' ')
      if (maxwind > vmax & maxwind > 0)
        vmax = maxwind
      endif
      if (maxwind < vmin & maxwind > 0)
        vmin = maxwind
      endif
    endif

    if (minprs != ' ')
      if (minprs > pmax & minprs > 0)
        pmax = minprs
      endif
      if (minprs < pmin & minprs > 0)
        pmin = minprs
      endif 
    endif

*    say ' '
*    say 'model= 'model' thour= 'thour' ylat= 'ylat' xlon= 'xlon
*    say '  northlat = '_northlat'  southlat = '_southlat
*    say '  westlon  = '_westlon'   eastlon  = '_eastlon

  endwhile

  ict = ict + 1

  rc = close(fcstfile)

endif

if (_plotverif = yes)

  while (1)

    res = read(verffile)
    rc  = sublin(res,1)
    if(rc != 0)
      if(rc = 2)
        say 'End of observed file '
        say ' '
        break
      endif
      if(rc = 1); say 'rc=1: OPEN ERROR FOR 'verffile; endif
      if(rc = 8); say 'rc=8: 'verffile' OPEN FOR WRITE ONLY'; endif
      if(rc = 9); say 'rc=9: I/O ERROR FOR 'verffile; endif
      return 99
    endif

    rec = sublin(res,2)

    thour   = subwrd(rec,3)
    ylat    = subwrd(rec,4)
    xlon    = subwrd(rec,5)
    maxwind = subwrd(rec,8)
    minprs  = subwrd(rec,9)
    adjfhr  = subwrd(rec,10)

    if (adjfhr > timeendo)
      timeendo = adjfhr
    endif
    if (adjfhr < timebego)
      timebego = adjfhr
    endif

    if (ylat > _northlat)
      _northlat = ylat
    endif
    if (ylat < _southlat)
      _southlat = ylat
    endif
    if (xlon > _westlon)
      _westlon = xlon
    endif
    if (xlon < _eastlon)
      _eastlon = xlon
    endif

    if (maxwind != ' ')
      if (maxwind > vmax & maxwind > 0)
        vmax = maxwind
      endif
      if (maxwind < vmin & maxwind > 0)
        vmin = maxwind
      endif
    endif

    if (minprs != ' ')
      if (minprs > pmax & minprs > 0)
        pmax = minprs
      endif
      if (minprs < pmin & minprs > 0)
        pmin = minprs
      endif
    endif

*    say ' '
*    say 'CARQ:          thour= 'thour' ylat= 'ylat' xlon= 'xlon
*    say '  northlat = '_northlat'  southlat = '_southlat
*    say '  westlon  = '_westlon'   eastlon  = '_eastlon

  endwhile

  rc = close(verffile)
  
endif

* If plottype is intensity, we are re-defining the axes, and
* we will want to make sure we are using the full 0-360 (lon)
* and -90 to 90 (lat) boundaries before re-defining....

if (_plottype = trkinset & plotnum = 2)
  intifflag = if
endif

if (_plottype = intensity | intifflag = if)
  _southlat = -90
  _northlat =  90
  _westlon  =   0
  _eastlon  = 360
else

* For all track plots, switch to positive degrees eastward, for 
* the sake of matching up with the GrADS .ctl for the data.

  _northlat = _northlat + 10
  _southlat = _southlat - 10
  _westlon  = _westlon  + 10
  _eastlon  = _eastlon  - 10

  if (_eastlon < 0)
    say ' '
    say '!!! eastlon being forced to be only at 0 degrees E'
    say ' '
    _eastlon = 0
  endif

  _westlon  = 360 - _westlon
  _eastlon  = 360 - _eastlon

endif

* say ' '
* say 'At end of getbounds....'
* say '  northlat = '_northlat'  southlat = '_southlat
* say '  westlon  = '_westlon'   eastlon  = '_eastlon

if (plotnum = 1)
* The check of plotnum is to make sure that we don't set
* the original lats & lons to be those from the intensity
* pass through this function....
  _origwlon = _westlon
  _origelon = _eastlon
  _orignlat = _northlat
  _origslat = _southlat
endif


* Now check the min & max ranges for those values related to 
* the axes for intensity

if (vmax > -9999)
  _Vmax = vmax + 5
endif
if (vmin < 9999)
  _Vmin = vmin - 5
endif
if (pmax > -9999)
  _Pmax = pmax + 5
endif
if (pmin < 9999)
  _Pmin = pmin - 5
endif

say 'in getbounds, _Vmax= '_Vmax'  _Vmin= '_Vmin
say 'in getbounds,  vmax= 'vmax'   vmin= 'vmin
say 'in getbounds, _Pmax= '_Pmax'  _Pmin= '_Pmin
say 'in getbounds,  pmax= 'pmax'   pmin= 'pmin

if (_plottype = intensity | intifflag = if)

  if  (_plotverif = yes)

*   Always use the beginning time for the observed data, as long
*   as the user has chosen to display the observed data.

    if (timebego < 0)
      _Tmin = timebego
    else
      _Tmin = 0
    endif
  else
    if (timebegf < 0)
      _Tmin = timebegf
    else
      _Tmin = 0
    endif
  endif

  if (_fcstlen != 999)
    timeendf = _fcstlen
  endif

  if (timeendo > timeendf)
    _Tmax = timeendo
  else
    if (_fcstlen = 999)
      _Tmax = timeendf
    else
      _Tmax = _fcstlen
    endif
  endif

endif

return

*---------------------------
*
*---------------------------
function getgxinfo()

'q gxinfo'
rec = sublin(result,1)
_lastgraph = subwrd(rec,4)
xrec = sublin(result,3)
_xleft  = subwrd(xrec,4)
_xright = subwrd(xrec,6)
yrec = sublin(result,4)
_ylo = subwrd(yrec,4)
_yhi = subwrd(yrec,6)

return


*---------------------------
*
*---------------------------
function prepinset()

* This function prepares the parea for the inset 
* window in which intensity plots will appear
* overlaid on a track plot.

*'draw mark 3 '_xleft' '_yhi' 0.12'
*'draw mark 3 '_xright' '_yhi' 0.12'
*'draw mark 3 '_xleft' '_ylo' 0.12'
*'draw mark 3 '_xright' '_ylo' 0.12'
*
*insetxpct = 0.40
*insetypct = 0.40
*
*'set line 2 1 6'
*'draw mark 6 '_par_left' '_par_top' 0.12'
*'draw mark 6 '_par_right' '_par_top' 0.12'
*'draw mark 6 '_par_left' '_par_bot' 0.12'
*'draw mark 6 '_par_right' '_par_bot' 0.12'

xdist = _insetxpct * (_xright - _xleft)
ydist = _insetypct * (_yhi - _ylo)
recright = _xleft + xdist
recbot   = _yhi - ydist

if (_insetloc = upleft)
  box_left  = _xleft
  box_right = _xleft + xdist
  box_top   = _yhi
  box_bot   = _yhi - ydist

  pa_left  = box_left * 1.15
  pa_right = box_right * 0.98
  pa_top   = box_top * 0.98
  pa_bot   = box_bot * 1.04
endif

if (_insetloc = upright)
  box_left  = _xright - xdist
  box_right = _xright
  box_top   = _yhi
  box_bot   = _yhi - ydist

  pa_left  = box_left * 1.04
  pa_right = box_right * 0.98
  pa_top   = box_top * 0.98
  pa_bot   = box_bot * 1.04
endif

if (_insetloc = lowright)
  box_left  = _xright - xdist
  box_right = _xright
  box_top   = _ylo + ydist
  box_bot   = _ylo

  pa_left  = box_left * 1.04
  pa_right = box_right * 0.98
  pa_top   = box_top * 0.98
  pa_bot   = box_bot * 1.04
endif

if (_insetloc = lowleft)
  box_left  = _xleft
  box_right = _xleft + xdist
  box_top   = _ylo + ydist
  box_bot   = _ylo

  pa_left  = box_left * 1.15
  pa_right = box_right * 0.98
  pa_top   = box_top * 0.98
  pa_bot   = box_bot * 1.04
endif

say '_insetloc= '_insetloc'  recright= 'recright'  recbot= 'recbot

say 'box_left  = 'box_left
say 'box_right = 'box_right
say 'box_top   = 'box_top
say 'box_bot   = 'box_bot

say 'pa_left  = 'pa_left
say 'pa_right = 'pa_right
say 'pa_top   = 'pa_top
say 'pa_bot   = 'pa_bot

'set rgb 60  50  65  50'
'set rgb 61 125 150 125'
'set rgb 62  30  45  30'

'set line 60 1 6'
'draw recf 'box_left' 'box_bot' 'box_right' 'box_top

*'set line 60 1 6'
*'draw recf 'pa_left' 'pa_bot' 'pa_right' 'pa_top

'set parea 'pa_left' 'pa_right' 'pa_bot' 'pa_top


return

*----------------------------
*
*----------------------------
function rbandzoom()

if (_plottype = intensity)
  'redraw button 202 0'
  return
endif

'q gxinfo'
rec = sublin(result,1)
_lastgraph = subwrd(rec,4)
xrec = sublin(result,3)
xl  = subwrd(xrec,4)
xr = subwrd(xrec,6)
yrec = sublin(result,4)
ylo = subwrd(yrec,4)
yhi = subwrd(yrec,6)

* 'q xy2w 'xl' 'ylo
* say result
* txlon1 = subwrd(result,3)
* ttlat1 = subwrd(result,6)
* 'q xy2w 'xr' 'yhi
* say result
* txlon2 = subwrd(result,3)
* ttlat2 = subwrd(result,6)
* 
* ttlon1 = 360 - txlon1
* ttlon2 = 360 - txlon2
* 
* say 'RBZ: xl= 'xl' ylo= 'ylo' xr= 'xr' yhi= 'yhi
* say 'RBZ: tlon1= 'ttlon1' tlon2= 'ttlon2' tlat1= 'ttlat1' tlat2= 'ttlat2
* 
* 'set line 2 1 6'
* 'draw mark 6 'xl' 'ylo' 0.2'
* 'draw mark 6 'xl' 'yhi' 0.2'
* 'draw mark 6 'xr' 'ylo' 0.2'
* 'draw mark 6 'xr' 'yhi' 0.2'

'set rband 6 box 'xl' 'ylo' 'xr' 'yhi
'query pos'
say result

vpx1 = subwrd(result,3)
vpx2 = subwrd(result,8)
vpy1 = subwrd(result,4)
vpy2 = subwrd(result,9)

widgetclass = subwrd(result,6)
if (widgetclass != 2)
* User clicked outside of plot area.  Ignore and return....
  'redraw button 202 0'
  return
endif

* say 'vpx1= 'vpx1' vpx2= 'vpx2' vpy1= 'vpy1' vpy2= 'vpy2

'q xy2w 'vpx1' 'vpy1
say result
rblon1 = subwrd(result,3)
rblat1 = subwrd(result,6)
'q xy2w 'vpx2' 'vpy2
say result
rblon2 = subwrd(result,3)
rblat2 = subwrd(result,6)

if (rblon1 = rblon2 | rblat1 = rblat2)
  return
endif

if (rblon1 < rblon2)
  _westlon = rblon1
  _eastlon = rblon2
else
  _westlon = rblon2
  _eastlon = rblon1
endif

if (rblat1 < rblat2)
  _southlat = rblat1
  _northlat = rblat2
else
  _southlat = rblat2
  _northlat = rblat1
endif

* say 'rblon1= 'rblon1'  rblat1= 'rblat1
* say 'rblon2= 'rblon2'  rblat2= 'rblat2

_zoom = yes

rc = makeplot()

return

*----------------------------
*
*----------------------------
function zoomin5()

if (_plottype = intensity)
  'redraw button 203 0'
  return
endif

tmpnlat = _northlat - 5
tmpslat = _southlat + 5
tmpwlon = _westlon + 5
tmpelon = _eastlon - 5

if (tmpnlat <= tmpslat | tmpelon <= tmpwlon)
  'set line 2 1 6'
  'draw recf 1.5 5.0 10.5 6.0'
  'set line 1 1 6'
  'draw recf 1.6 5.1 10.4 5.9'
  'set string 11 c 6'
  'set strsiz 0.14 0.14'
  dispstr = '!!! Cannot "ZOOM IN 5" any further.  Hit "PLOT" button to continue....'
  'draw string 6.0 5.5 'dispstr
  'redraw button 203 0'
  return
endif

_northlat = tmpnlat
_southlat = tmpslat
_westlon  = tmpwlon
_eastlon  = tmpelon

_zoom = yes

rc = makeplot()

return

*----------------------------
*
*----------------------------
function zoomout5()

if (_plottype = intensity)
'redraw button 204 0'
  return
endif

tmpnlat = _northlat + 5
tmpslat = _southlat - 5
tmpwlon = _westlon - 5
tmpelon = _eastlon + 5

if (tmpnlat > 90 | tmpslat < -90 | tmpwlon < 0 | tmpelon > 359)
  'set line 2 1 6'
  'draw recf 1.5 5.0 10.5 6.0'
  'set line 1 1 6'
  'draw recf 1.6 5.1 10.4 5.9'
  'set string 9 c 6'
  'set strsiz 0.14 0.14'
  dispstr = '!!! Cannot "ZOOM OUT 5" any further.  Hit "PLOT" button to continue....'
  'draw string 6.0 5.5 'dispstr
  'redraw button 204 0'
  return
endif

_northlat = tmpnlat
_southlat = tmpslat
_westlon  = tmpwlon
_eastlon  = tmpelon

_zoom = yes

rc = makeplot()

return

*----------------------------
*
*----------------------------
function zoomorig()

if (_plottype = intensity)
  'redraw button 205 0'
  return
endif

_northlat = _orignlat
_southlat = _origslat
_westlon  = _origwlon
_eastlon  = _origelon

_zoom = original

rc = makeplot()

return

*----------------------------
* 
*----------------------------
function steptime(button)

if (button = 206)
  timestep = -24
endif
if (button = 207)
  timestep = -12
endif
if (button = 208)
  timestep = -6
endif
if (button = 209)
  timestep = 6
endif
if (button = 210)
  timestep = 12
endif
if (button = 211)
  timestep = 24
endif

'!'_rundir'step_dates.sh '_stlistnum' '_startdate' 'timestep' '_userid' >'_netdir'stepcheck.txt'
stepfile=_netdir%'stepcheck.txt'

res = read(stepfile)
rc  = sublin(res,1)
if(rc != 0)
  if(rc = 2)
    say 'End of stepfile: 'stepfile
    say ' '
  endif
  if(rc = 1); say 'rc=1: OPEN ERROR FOR 'stepfile; endif
  if(rc = 8); say 'rc=8: 'stepfile' OPEN FOR WRITE ONLY'; endif
  if(rc = 9); say 'rc=9: I/O ERROR FOR 'stepfile; endif
  return 99
endif

rc = close (stepfile)

steprec = sublin(res,2)
stepval = subwrd(steprec,1)
eymdh   = subwrd(steprec,2)

if (stepval = 900)
  'set line 2 1 6'
  'draw recf 1.5 1.0 10.5 2.5'
  'set line 1 1 6'
  'draw recf 1.6 1.1 10.4 2.4'
  'set string 2 c 6'
  'set strsiz 0.14 0.14'
  dispstr1 = '!!! ERROR: Cannot step back 'timestep'h from '_startdate' for this storm.'
  'draw string 6.0 2.0 'dispstr1
  dispstr2 = '!!! Hit "PLOT" button to continue....'
  'draw string 6.0 1.5 'dispstr2
  return
endif

if (stepval = 995)
  'set line 2 1 6'
  'draw recf 1.5 1.0 10.5 2.5'
  'set line 1 1 6'
  'draw recf 1.6 1.1 10.4 2.4'
  'set string 2 c 6'
  'set strsiz 0.14 0.14'
  dispstr1 = '!!! ERROR: Cannot step forward 'timestep'h from '_startdate' for this storm.'
  'draw string 6.0 2.0 'dispstr1
  dispstr2 = '!!! Hit "PLOT" button to continue....'
  'draw string 6.0 1.5 'dispstr2
  return
endif

if (stepval = 500)
  'set line 2 1 6'
  'draw recf 1.5 1.0 10.5 2.5'
  'set line 1 1 6'
  'draw recf 1.6 1.1 10.4 2.4'
  'set string 2 c 6'
  'set strsiz 0.14 0.14'
  dispstr1 = '!!! ERROR: Requested step date of 'eymdh' is missing from a-deck for this storm.'
  'draw string 6.0 2.0 'dispstr1
  dispstr2 = '!!! Hit "PLOT" button to continue....'
  'draw string 6.0 1.5 'dispstr2
  return
endif

_startdate = stepval
  
rc = makeplot()

return

*----------------------------
*
*----------------------------
function drawfcst(plotnum)

* The input variable, plotnum, is for the plot number.  For a 
* basic _plottype of either "track" or "intensity", this 
* plotnum will only be 1, since we're only making one plot.
* But for a _plottype of "trkinset", this drawfcst function 
* will get called twice, the first time for the track plot
* (plotnum=1), and the second time for the intensity plot that
* will be inset, or overlaid, on the track plot (plotnum=2).

fcstfile = _netdir%flist

modelct  = 0
oldmodel = XXXX
oldymdh  = 9999999999

if (plotnum = 1)
  modelsize = 0.14
else
  modelsize = 0.11
endif

while (1)

  res = read(fcstfile)
  rc  = sublin(res,1)
  if(rc != 0)
    if(rc = 2)
      say 'End of track file '
      say ' '
      break
    endif
    if(rc = 1); say 'rc=1: OPEN ERROR FOR 'fcstfile; endif
    if(rc = 8); say 'rc=8: 'fcstfile' OPEN FOR WRITE ONLY'; endif
    if(rc = 9); say 'rc=9: I/O ERROR FOR 'fcstfile; endif
    return 99
  endif

  rec = sublin(res,2)

  ymdh     = subwrd(rec,1)
  model    = subwrd(rec,2)
  fhr      = subwrd(rec,3)
  newlat   = subwrd(rec,4)
  newlon   = subwrd(rec,5)
  _atcfnum = subwrd(rec,6)
  adjfhr   = subwrd(rec,7)
  newwind  = subwrd(rec,8)
  newprs   = subwrd(rec,9)

*  say ' '
*  say '+++newrec'
*  say '   rec= 'rec
*  say '   newwind= 'newwind'  newprs= 'newprs
*  say '   _Pmax= '_Pmax'  _Pmin= '_Pmin
*  say '   _Vmax= '_Vmax'  _Vmin= '_Vmin
*  say '   _Tmax= '_Tmax'  _Tmin= '_Tmin
*  say '   adjfhr= 'adjfhr

  if (_plottype = trkinset & plotnum = 2)
    intifflag = if
  endif

  if (_plottype = intensity | intifflag = if)
    if (_intdisp = mslponly)
      a      = 180./(_Pmax-_Pmin)
      b      = 90. - a*_Pmax
      newlat = a*newprs + b
    else
      a      = 180./(_Vmax-_Vmin)
      b      = 90. - a*_Vmax
      newlat = a*newwind + b
    endif

*    say '   Lat:  a= 'a'  b= 'b'  newlat= 'newlat

    a      = 360./(_Tmax-_Tmin)
    b      = -a*_Tmin
    newlon = a*adjfhr + b

  else
    newlon = 360.0 - newlon
  endif

*  say '   LON:  a= 'a'  b= 'b'                    newlon= 'newlon

  modelsubstr = substr(model,1,3)
  if (modelsubstr = 'AP0' | modelsubstr = 'AP1' | modelsubstr = 'AP2')
    modtype = ensemble
  else
    modtype = regular
  endif

  if (model = oldmodel & ymdh = oldymdh)

    'query ll2xy 'lastlon' 'lastlat
    xdum  = sublin(result,1)
    xlast = subwrd(xdum,1)
    ylast = subwrd(xdum,2)

    'query ll2xy 'newlon' 'newlat
    xdum = sublin(result,1)
    xnew = subwrd(xdum,1)
    ynew = subwrd(xdum,2)

*    say '   IF:  lastlon= 'lastlon'  lastlat= 'lastlat
*    say '   IF:  xlast= 'xlast'  ylast= 'ylast
*    say '   IF:  newlon= 'newlon'  newlat= 'newlat
*    say '   IF:  xnew= 'xnew'  ynew= 'ynew

    oldbound = in
    newbound = in
    if (xlast > _xright | xlast < _xleft | ylast > _yhi | ylast < _ylo)
      oldbound = out
    endif
    if (xnew > _xright | xnew < _xleft | ynew > _yhi | ynew < _ylo)
      newbound = out
    endif

    if (oldbound = in | newbound = in)
      if (fhr <= _fcstlen)
*TM        if (modtype = ensemble)
        if (modtype = xxxemble)
          'set line 'trackcolor' 1 3'
        else
          'set line 'trackcolor' 1 7'
        endif
      
        if ( lastlat < -90. | newlat < -90.)
          say 'No forecast data reported at ' fhr 'for' model
        else
          'draw line 'xlast' 'ylast' 'xnew' 'ynew
        endif

        checkmark = math_fmod(fhr,_trkmrkrint)
*        say 'fhr= 'fhr' model= 'model' trkmrkrint= '_trkmrkrint' checkmark= 'checkmark
        if (checkmark = 0 & _trkmrkrintrvl != none)
          if (modtype = ensemble)
*TM            'set line 23 1 4'
            'set line 'trackcolor' 1 4'
            'draw mark 3 'xnew' 'ynew' 0.05'
          else
            'set string 'trackcolor' c 6'
            'set strsiz 'modelsize' 'modelsize
            if ( newlat >= -90. & newlat <= 90. )
              'draw string 'xnew' 'ynew' 'modelct
            endif
          endif
        endif
      endif

    endif

  else

    modelct = modelct + 1

    if (_modcoltype = rainbow)
*TM      if (modtype = ensemble)
      if (modtype = xxxemble)
        trackcolor = 23
      else
        colorix = math_fmod(modelct,_numtrkcols)
        if (colorix = 0)
          ntix = _numtrkcols
          trackcolor = _trkcol.ntix
        else
          trackcolor = _trkcol.colorix
        endif
      endif
    else
      trackcolor = _modmonocol
    endif

    if (modelct = 1) 
      _ftitleymdh = ymdh
      _ftitlemodel = model
    endif

    'query ll2xy 'newlon' 'newlat
    xdum = sublin(result,1)
    xnew = subwrd(xdum,1)
    ynew = subwrd(xdum,2)

*    say '   ELSE:  newlon= 'newlon'  newlat= 'newlat
*    say '   ELSE:  xnew= 'xnew'  ynew= 'ynew

    if (_trkmrkrintrvl != none & modtype != ensemble)
      if (xnew <= _xright & xnew >= _xleft & ynew <= _yhi & ynew >= _ylo & fhr <= _fcstlen)
        'set string 'trackcolor' c 6'
        'set strsiz 'modelsize' 'modelsize
        'draw string 'xnew' 'ynew' 'modelct
      endif
    endif

    if (_legendloc != none & _trkmrkrintrvl != none & modtype != ensemble)
      if (plotnum != 2)
        rc = drawlegend(modelct,model,trackcolor,ymdh)
      endif
    endif

  endif
     
  oldmodel = model
  oldymdh  = ymdh
  lastlon  = newlon
  lastlat  = newlat

endwhile

rc = close(fcstfile)

return

*--------------------------
*
*--------------------------
function drawverif(plotnum)

verffile = _netdir%vlist

oldmodel = XXXX
oldymdh  = 9999999999

'q dims'
xlin = sublin(result,2)
ylin = sublin(result,3)
xl1  = subwrd(xlin,6)
xl2  = subwrd(xlin,8)
yl1  = subwrd(xlin,6)
yl2  = subwrd(xlin,8)
xdiff = xl2 - xl1
ydiff = yl2 - yl1

if (plotnum = 1)
* This is either a track plot, intensity plot, or a track inset plot
* where we are just plotting the main track part of the plot...
  if (xdiff > 50 & ydiff > 50)
    hurrsize = 0.30
  else
    hurrsize = 0.30
  endif
else
* This is a track inset plot where we are plotting the intensity 
* inset part of the plot
  hurrsize = 0.15
endif

if (plotnum = 1)
  tcsym = 40
else
  tcsym = 41
endif

vct = 0

while (1)

  res = read(verffile)
  rc  = sublin(res,1)
  if(rc != 0)
    if(rc = 2)
      say 'End of observed file '
      say ' '
      break
    endif
    if(rc = 1); say 'rc=1: OPEN ERROR FOR 'verffile; endif
    if(rc = 8); say 'rc=8: 'verffile' OPEN FOR WRITE ONLY'; endif
    if(rc = 9); say 'rc=9: I/O ERROR FOR 'verffile; endif
    return 99
  endif

  rec = sublin(res,2)

  ymdh     = subwrd(rec,1)
  model    = subwrd(rec,2)
  fhr      = subwrd(rec,3)
  newlat   = subwrd(rec,4)
  newlon   = subwrd(rec,5)
  _atcfnum = subwrd(rec,6)
*  _stname  = subwrd(rec,7)  * This is now read in in  rddates
  newwind  = subwrd(rec,8)
  newprs   = subwrd(rec,9)
  adjfhr   = subwrd(rec,10)

  if (_plottype = trkinset & plotnum = 2)
    intifflag = if
  endif

  if (_plottype = intensity | intifflag = if)
    if (_intdisp = mslponly)
      a      = 180./(_Pmax-_Pmin)
      b      = 90. - a*_Pmax
      newlat = a*newprs + b
    else    
      a      = 180./(_Vmax-_Vmin)
      b      = 90. - a*_Vmax
      newlat = a*newwind + b 
    endif   
              
    a      = 360./(_Tmax-_Tmin)
    b      = -a*_Tmin
    newlon = a*adjfhr + b
      
  else
    newlon = 360.0 - newlon
  endif

  if (oldymdh < 9999999999)

    'query ll2xy 'lastlon' 'lastlat
    xdum  = sublin(result,1)
    xlast = subwrd(xdum,1)
    ylast = subwrd(xdum,2)

    'query ll2xy 'newlon' 'newlat
    xdum = sublin(result,1)
    xnew = subwrd(xdum,1)
    ynew = subwrd(xdum,2)

    oldbound = in
    newbound = in
    if (xlast > _xright | xlast < _xleft | ylast > _yhi | ylast < _ylo)
      oldbound = out
    endif
    if (xnew > _xright | xnew < _xleft | ynew > _yhi | ynew < _ylo)
      newbound = out
    endif

*   To not draw lines between verif points, comment out the
*   draw line statement 3 lines down...

    if (oldbound = in | newbound = in)
      'set line '_verifcol' 1 4'
*     Avoid plotting missing values...
      if ( ylast < 0. | ynew < 0.)
        say 'no data reported at ' fhr 'for' model
      else
        'draw line 'xlast' 'ylast' 'xnew' 'ynew
      endif

      say 'drawverif, adjfhr= 'adjfhr',  _vrfmrkrint= '_vrfmrkrint'  checkmark= 'checkmark
      checkmark = math_fmod(adjfhr,_vrfmrkrint)
      if (checkmark = 0)
        'set line 1 1 4'
        if (_plotstrike = yes)
          'draw wxsym 'tcsym' 'xnew' 'ynew' 'hurrsize' '_verifcol' 4'
        else
          'draw wxsym 'tcsym' 'xnew' 'ynew' 'hurrsize' '_verifcol' 4'
        endif
      endif

    endif

  else

    'query ll2xy 'newlon' 'newlat
    xdum = sublin(result,1)
    xnew = subwrd(xdum,1)
    ynew = subwrd(xdum,2)

    vct = vct + 1
    if (vct = 1)
      _vtitleymdh = ymdh
    endif

    if (xnew <= _xright & xnew >= _xleft & ynew <= _yhi & ynew >= _ylo)
      'set line 1 1 4'
      if (_plotstrike = yes)
        'draw wxsym 'tcsym' 'xnew' 'ynew' 'hurrsize' '_verifcol' 4'
      else
        'draw wxsym 'tcsym' 'xnew' 'ynew' 'hurrsize' '_verifcol' 4'
      endif
    endif

  endif

  oldymdh = ymdh
  lastlon  = newlon
  lastlat  = newlat

endwhile

rc = close(verffile)

return

*---------------------*
*
*---------------------*
function initmenu()

modelwid=0.50
modelhgt=0.25

ensemhgt=0.16
ensemwid=0.33

stephgt=0.20

'set dropmenu 1 91 90 92 6 91 90 92 1 91 90 92 90 92 6'

'draw dropmenu 1 0.75 8.40 0.85 0.25 Main | '_mainstuff
'draw dropmenu 2 1.75 8.40 0.85 0.25 Storms | '_stormlist
'draw dropmenu 3 2.75 8.40 0.85 0.25 Dates | No storm picked yet'
'draw dropmenu 4 3.75 8.40 0.85 0.25 Fcst/Obs | '_trkstuff
'draw dropmenu 5 4.75 8.40 0.85 0.25 Opts_1 |Track forecasts |Intensity forecasts >30>|Plot forecasts? >19>|Plot observed? >20>|Plot ensemble strike probabilities? >28>|'
'draw dropmenu 7 5.75 8.40 0.85 0.25 Opts_2 |Model colors >21>|Observed color >22>|Model legend >24>|Map Color >22>|Grid Options >27>|Draw Signature? >19>|'

'draw dropmenu 15 cascade Every 6h |Every 12h |Every 24h |None'
'draw dropmenu 16 cascade Every 6h |Every 12h |Every 24h'
'draw dropmenu 17 cascade Every 6h |Every 12h |Every 24h'
'draw dropmenu 18 cascade Next 72h |Next 84h |Next 96h |Next 120h |Next 144h |Next 168h |Previous 72h |Previous 96h |Previous 120h |Previous 144h |Previous 168h |Beginning to now |Only now |Remainder of storm |Entire storm'
'draw dropmenu 19 cascade Yes |No'
'draw dropmenu 20 cascade Yes |No'
'draw dropmenu 21 cascade Rainbow | Monochrome >23>|'
'draw dropmenu 22 cascade black |white |red |green |blue |cyan |magenta |yellow |orange |purple |yellow/green |medium blue |dark yellow |aqua |dark purple |grey'
'draw dropmenu 23 cascade Douglas Fir |Steel |Ketchup |Mustard |Grape |Water |Bubble Gum |Charcoal'
'draw dropmenu 24 cascade Upper right |Lower right |Lower left |Upper left |None'
'draw dropmenu 25 cascade 72 |78 |84 |96 |120 |126 |144 |168 |All available'
'draw dropmenu 27 cascade Dashed Lines |Solid Lines |No Lines |- - GRID COLORS - - |black |white |red |green |blue |cyan |magenta |yellow |orange |purple |yellow/green |medium blue |dark yellow |aqua |dark purple |grey'
'draw dropmenu 28 cascade No | - - Accum Probs - -| Yes: Accum 65 nm| Yes: Accum 100 nm| Yes: Accum 200 nm| - - Indiv Hour Probs - -| Yes: Indiv 65 nm| Yes: Indiv 100 nm| Yes: Indiv 200 nm| Pick Indiv Hours >29>| - - Prob Color Bar - -| Plot color bar| DO NOT plot color bar'
'draw dropmenu 29 cascade 00 |12 |24 |36 |48 |60 |72 |84'
'draw dropmenu 30 cascade - - Intensity Full screen - - |  MSLP |  Winds |- - Intensity on track plot inset - - |  MSLP |  Winds |- - Track inset location - - |  Upper left |  Upper right |  Lower right |  Lower left |- - Track inset window size % - - |  25% |  30% |  35% |  40% |  45% |  50%'

* 'set button  0 15 10  3  0  3 10  3 6'
***'set button  0  3 10  3  0 15 10  3 6'
'set button 1 40 41 42 5 40 42 41 6'

'draw button 101  0.30 7.60 'modelwid' 'modelhgt'  GFDL'
'draw button 102  0.85 7.60 'modelwid' 'modelhgt'  GFDI'
'draw button 103  0.30 7.30 'modelwid' 'modelhgt'  AEMN'
'draw button 104  0.85 7.30 'modelwid' 'modelhgt'  AVNO'
'draw button 105  0.30 7.00 'modelwid' 'modelhgt'   UKM'
'draw button 106  0.85 7.00 'modelwid' 'modelhgt'  HRE2'
'draw button 107  0.30 6.70 'modelwid' 'modelhgt'  HREL'
'draw button 108  0.85 6.70 'modelwid' 'modelhgt'  APTS'
'draw button 109  0.30 6.40 'modelwid' 'modelhgt'  HWRF'
'draw button 110  0.85 6.40 'modelwid' 'modelhgt'  DSHP'
'draw button 111  0.30 6.10 'modelwid' 'modelhgt'  AEMI'
'draw button 112  0.85 6.10 'modelwid' 'modelhgt'  SHIP'
'draw button 113  0.30 5.80 'modelwid' 'modelhgt'  SRMN'
'draw button 114  0.85 5.80 'modelwid' 'modelhgt'  UKMI'
'draw button 115  0.30 5.50 'modelwid' 'modelhgt'  GFDH'
'draw button 116  0.85 5.50 'modelwid' 'modelhgt'  GFTI'
'draw button 117  0.30 5.20 'modelwid' 'modelhgt'  AVNI'
'draw button 118  0.85 5.20 'modelwid' 'modelhgt'  GFSO'
'draw button 119  0.30 4.90 'modelwid' 'modelhgt'  TVCN'
'draw button 120  0.85 4.90 'modelwid' 'modelhgt'   EMX'
'draw button 121  0.30 4.60 'modelwid' 'modelhgt'   JSG'
'draw button 122  0.85 4.60 'modelwid' 'modelhgt'  NGPS'
'draw button 123  0.30 4.30 'modelwid' 'modelhgt'  HTUT'
'draw button 124  0.85 4.30 'modelwid' 'modelhgt'  HCYC'
'draw button 125  0.30 4.00 'modelwid' 'modelhgt'  AHWT'
'draw button 126  0.85 4.00 'modelwid' 'modelhgt'  SRMN'

'draw button 127  0.19 1.80 'ensemwid' 'ensemhgt'   P01'
'draw button 128  0.55 1.80 'ensemwid' 'ensemhgt'   P02'
'draw button 129  0.90 1.80 'ensemwid' 'ensemhgt'   P03'
'draw button 130  0.19 1.60 'ensemwid' 'ensemhgt'   P04'
'draw button 131  0.55 1.60 'ensemwid' 'ensemhgt'   P05'
'draw button 132  0.90 1.60 'ensemwid' 'ensemhgt'   P06'
'draw button 133  0.19 1.40 'ensemwid' 'ensemhgt'   P07'
'draw button 134  0.55 1.40 'ensemwid' 'ensemhgt'   P08'
'draw button 135  0.90 1.40 'ensemwid' 'ensemhgt'   P09'
'draw button 136  0.19 1.20 'ensemwid' 'ensemhgt'   P10'
'draw button 137  0.55 1.20 'ensemwid' 'ensemhgt'   P11'
'draw button 138  0.90 1.20 'ensemwid' 'ensemhgt'   P12'
'draw button 139  0.19 1.00 'ensemwid' 'ensemhgt'   P13'
'draw button 140  0.55 1.00 'ensemwid' 'ensemhgt'   P14'
'draw button 141  0.90 1.00 'ensemwid' 'ensemhgt'   P15'
'draw button 142  0.19 0.80 'ensemwid' 'ensemhgt'   P16'
'draw button 143  0.55 0.80 'ensemwid' 'ensemhgt'   P17'
'draw button 144  0.90 0.80 'ensemwid' 'ensemhgt'   P18'
'draw button 145  0.19 0.60 'ensemwid' 'ensemhgt'   P19'
'draw button 146  0.55 0.60 'ensemwid' 'ensemhgt'   P20'

'set button 1 40 41 42 1 40 42 41 6'
if (_plottype = intensity)
  'draw button 201  0.55 3.55 0.75 0.375 PLOT'
  'draw button 202  0.55 3.15 1.05 0.250 N/A'
  'draw button 203  0.55 2.85 1.05 0.250 N/A'
  'draw button 204  0.55 2.55 1.05 0.250 N/A'
  'draw button 205  0.55 2.25 1.05 0.250 N/A'
else
  'draw button 201  0.55 3.55 0.75 0.375 PLOT'
  'draw button 202  0.55 3.15 1.05 0.250 ZOOM --->'
  'draw button 203  0.55 2.85 1.05 0.250 ZOOM IN 5'
  'draw button 204  0.55 2.55 1.05 0.250 ZOOM OUT 5'
  'draw button 205  0.55 2.25 1.05 0.250 ORIGINAL'
endif

'draw button 206  1.60 0.13 'modelwid' 'stephgt' -24h'
'draw button 207  2.15 0.13 'modelwid' 'stephgt' -12h'
'draw button 208  2.70 0.13 'modelwid' 'stephgt' -6h'
'draw button 209  3.25 0.13 'modelwid' 'stephgt' +6h'
'draw button 210  3.80 0.13 'modelwid' 'stephgt' +12h'
'draw button 211  4.35 0.13 'modelwid' 'stephgt' +24h'

*'set button  0  2 13 14  0  2 13 14 6'
'set button 1 50 51 52 1 50 52 51 6'
'draw button 98  0.30 0.15 'modelwid' 'modelhgt' EXIT'
'draw button 99  0.85 0.15 'modelwid' 'modelhgt' QUIT'

return

*---------------------*
*
*---------------------*
function redomenu(preserve)

modelwid=0.50
modelhgt=0.25

ensemhgt=0.16
ensemwid=0.33

stephgt=0.20

'clear dropmenu  1'
'clear dropmenu  2'
'clear dropmenu  3'
'clear dropmenu  4'
'clear dropmenu  5'
'clear dropmenu  7'
'clear dropmenu 15'
'clear dropmenu 16'
'clear dropmenu 17'
'clear dropmenu 18'
'clear dropmenu 19'
'clear dropmenu 20'
'clear dropmenu 21'
'clear dropmenu 22'
'clear dropmenu 23'
'clear dropmenu 24'
'clear dropmenu 25'
'clear dropmenu 27'

if (_freshcascade != yes)
  ymdix  = 34
  ymdmax = ymdix + _ymdct
  while (ymdix < ymdmax)
    ymdix = ymdix + 1
    'clear dropmenu 'ymdix
  endwhile
endif

'set dropmenu 1 91 90 92 6 91 90 92 1 91 90 92 90 92 6'

'draw dropmenu 1 0.75 8.40 0.85 0.25 Main | '_mainstuff
'draw dropmenu 2 1.75 8.40 0.85 0.25 Storms | '_stormlist
'draw dropmenu 3 2.75 8.40 0.85 0.25 Dates | '_ymdlist
'draw dropmenu 4 3.75 8.40 0.85 0.25 Fcst/Obs | '_trkstuff
'draw dropmenu 5 4.75 8.40 0.85 0.25 Opts_1 |Track forecasts |Intensity forecasts >30>|Plot forecasts? >19>|Plot observed? >20>|Plot ensemble strike probabilities? >28>|'
'draw dropmenu 7 5.75 8.40 0.85 0.25 Opts_2 |Model colors >21>|Observed color >22>|Model legend >24>|Map Color >22>|Grid Options >27>|Draw Signature? >19>|'

'draw dropmenu 15 cascade Every 6h |Every 12h |Every 24h |None'
'draw dropmenu 16 cascade Every 6h |Every 12h |Every 24h'
'draw dropmenu 17 cascade Every 6h |Every 12h |Every 24h'
'draw dropmenu 18 cascade Next 72h |Next 84h |Next 96h |Next 120h |Next 144h |Next 168h |Previous 72h |Previous 96h |Previous 120h |Previous 144h |Previous 168h |Beginning to now |Only now |Remainder of storm |Entire storm'
'draw dropmenu 19 cascade Yes |No'
'draw dropmenu 20 cascade Yes |No'
'draw dropmenu 21 cascade Rainbow | Monochrome >23>|'
'draw dropmenu 22 cascade black |white |red |green |blue |cyan |magenta |yellow |orange |purple |yellow/green |medium blue |dark yellow |aqua |dark purple |grey'
'draw dropmenu 23 cascade Douglas Fir |Steel |Ketchup |Mustard |Grape |Water |Bubble Gum |Charcoal'
'draw dropmenu 24 cascade Upper right |Lower right |Lower left |Upper left |None'
'draw dropmenu 25 cascade 72 |78 |84 |96 |120 |126 |144 |168 |All available'
'draw dropmenu 27 cascade Dashed Lines |Solid Lines |No Lines |- - GRID COLORS - - |black |white |red |green |blue |cyan |magenta |yellow |orange |purple |yellow/green |medium blue |dark yellow |aqua |dark purple |grey'
'draw dropmenu 28 cascade No | - - Accum Probs - -| Yes: Accum 65 nm| Yes: Accum 100 nm| Yes: Accum 200 nm| - - Indiv Hour Probs - -| Yes: Indiv 65 nm| Yes: Indiv 100 nm| Yes: Indiv 200 nm| Pick Indiv Hours >29>| - - Prob Color Bar - -| Plot color bar| DO NOT plot color bar'
'draw dropmenu 29 cascade 00 |12 |24 |36 |48 |60 |72 |84'
'draw dropmenu 30 cascade - - Intensity Full screen - - |  MSLP |  Winds |- - Intensity on track plot inset - - |  MSLP |  Winds |- - Track inset location - - |  Upper left |  Upper right |  Lower right |  Lower left |- - Track inset window size % - - |  25% |  30% |  35% |  40% |  45% |  50%'

ymdix  = 34
ymdmax = ymdix + _ymdct
while (ymdix < ymdmax)
  ymdix = ymdix + 1
  'draw dropmenu 'ymdix' cascade '_hhstring.ymdix
endwhile 
_freshcascade = no

*'set button  0 15 10  3  0  3 10  3 6'
***'set button  0  3 10  3  0 15 10  3 6'
'set button 1 40 41 42 5 40 42 41 6'

'clear button 101'
'clear button 102'
'clear button 103'
'clear button 104'
'clear button 105'
'clear button 106'
'clear button 107'
'clear button 108'
'clear button 109'
'clear button 110'
'clear button 111'
'clear button 112'
'clear button 113'
'clear button 114'
'clear button 115'
'clear button 116'
'clear button 117'
'clear button 118'
'clear button 119'
'clear button 120'
'clear button 121'
'clear button 122'
'clear button 123'
'clear button 124'
'clear button 125'
'clear button 126'
'clear button 127'
'clear button 128'
'clear button 129'
'clear button 130'
'clear button 131'
'clear button 132'
'clear button 133'
'clear button 134'
'clear button 135'
'clear button 136'
'clear button 137'
'clear button 138'
'clear button 139'
'clear button 140'
'clear button 141'
'clear button 142'
'clear button 143'
'clear button 144'
'clear button 145'
'clear button 146'

'draw button 101  0.30 7.60 'modelwid' 'modelhgt'  GFDL'
'draw button 102  0.85 7.60 'modelwid' 'modelhgt'  GFDI'
'draw button 103  0.30 7.30 'modelwid' 'modelhgt'  AEMN'
'draw button 104  0.85 7.30 'modelwid' 'modelhgt'  AVNO'
'draw button 105  0.30 7.00 'modelwid' 'modelhgt'   UKM'
'draw button 106  0.85 7.00 'modelwid' 'modelhgt'  HRE2'
'draw button 107  0.30 6.70 'modelwid' 'modelhgt'  HREL'
'draw button 108  0.85 6.70 'modelwid' 'modelhgt'  APTS'
'draw button 109  0.30 6.40 'modelwid' 'modelhgt'  HWRF'
'draw button 110  0.85 6.40 'modelwid' 'modelhgt'  DSHP'
'draw button 111  0.30 6.10 'modelwid' 'modelhgt'  AEMI'
'draw button 112  0.85 6.10 'modelwid' 'modelhgt'  SHIP'
'draw button 113  0.30 5.80 'modelwid' 'modelhgt'  SRMN'
'draw button 114  0.85 5.80 'modelwid' 'modelhgt'  UKMI'
'draw button 115  0.30 5.50 'modelwid' 'modelhgt'  GFDH'
'draw button 116  0.85 5.50 'modelwid' 'modelhgt'  GFTI'
'draw button 117  0.30 5.20 'modelwid' 'modelhgt'  AVNI'
'draw button 118  0.85 5.20 'modelwid' 'modelhgt'  GFSO'
'draw button 119  0.30 4.90 'modelwid' 'modelhgt'  TVCN'
'draw button 120  0.85 4.90 'modelwid' 'modelhgt'   EMX'
'draw button 121  0.30 4.60 'modelwid' 'modelhgt'   JSG'
'draw button 122  0.85 4.60 'modelwid' 'modelhgt'  NGPS'
'draw button 123  0.30 4.30 'modelwid' 'modelhgt'  HTUT'
'draw button 124  0.85 4.30 'modelwid' 'modelhgt'  HCYC'
'draw button 125  0.30 4.00 'modelwid' 'modelhgt'  AHWT'
'draw button 126  0.85 4.00 'modelwid' 'modelhgt'  SRMN'

'draw button 127  0.19 1.80 'ensemwid' 'ensemhgt'   P01'
'draw button 128  0.55 1.80 'ensemwid' 'ensemhgt'   P02'
'draw button 129  0.90 1.80 'ensemwid' 'ensemhgt'   P03'
'draw button 130  0.19 1.60 'ensemwid' 'ensemhgt'   P04'
'draw button 131  0.55 1.60 'ensemwid' 'ensemhgt'   P05'
'draw button 132  0.90 1.60 'ensemwid' 'ensemhgt'   P06'
'draw button 133  0.19 1.40 'ensemwid' 'ensemhgt'   P07'
'draw button 134  0.55 1.40 'ensemwid' 'ensemhgt'   P08'
'draw button 135  0.90 1.40 'ensemwid' 'ensemhgt'   P09'
'draw button 136  0.19 1.20 'ensemwid' 'ensemhgt'   P10'
'draw button 137  0.55 1.20 'ensemwid' 'ensemhgt'   P11'
'draw button 138  0.90 1.20 'ensemwid' 'ensemhgt'   P12'
'draw button 139  0.19 1.00 'ensemwid' 'ensemhgt'   P13'
'draw button 140  0.55 1.00 'ensemwid' 'ensemhgt'   P14'
'draw button 141  0.90 1.00 'ensemwid' 'ensemhgt'   P15'
'draw button 142  0.19 0.80 'ensemwid' 'ensemhgt'   P16'
'draw button 143  0.55 0.80 'ensemwid' 'ensemhgt'   P17'
'draw button 144  0.90 0.80 'ensemwid' 'ensemhgt'   P18'
'draw button 145  0.19 0.60 'ensemwid' 'ensemhgt'   P19'                                         
'draw button 146  0.55 0.60 'ensemwid' 'ensemhgt'   P20'

if (preserve = yes)
  m = 101
  while (m < 147)
    if (_modstatus.m = 1)
      'redraw button 'm' 1'
    else
      'redraw button 'm' 0'
    endif
    m = m + 1
  endwhile
endif

'set button 1 40 41 42 1 40 42 41 6'
if (_plottype = intensity)
  'draw button 201  0.55 3.55 0.75 0.375 PLOT'
  'draw button 202  0.55 3.15 1.05 0.250 N/A'
  'draw button 203  0.55 2.85 1.05 0.250 N/A'
  'draw button 204  0.55 2.55 1.05 0.250 N/A'
  'draw button 205  0.55 2.25 1.05 0.250 N/A'
else
  'draw button 201  0.55 3.55 0.75 0.375 PLOT'
  'draw button 202  0.55 3.15 1.05 0.250 ZOOM --->'
  'draw button 203  0.55 2.85 1.05 0.250 ZOOM IN 5'
  'draw button 204  0.55 2.55 1.05 0.250 ZOOM OUT 5'
  'draw button 205  0.55 2.25 1.05 0.250 ORIGINAL'
endif

'draw button 206  1.60 0.15 'modelwid' 'stephgt' -24h'
'draw button 207  2.15 0.15 'modelwid' 'stephgt' -12h'
'draw button 208  2.70 0.15 'modelwid' 'stephgt' -6h'
'draw button 209  3.25 0.15 'modelwid' 'stephgt' +6h'
'draw button 210  3.80 0.15 'modelwid' 'stephgt' +12h'
'draw button 211  4.35 0.15 'modelwid' 'stephgt' +24h'

*'set button  0  2 13 14  0  2 13 14 6'
'set button 1 50 51 52 1 50 52 51 6'
'draw button 98  0.30 0.15 'modelwid' 'modelhgt' EXIT'
'draw button 99  0.85 0.15 'modelwid' 'modelhgt' QUIT'

return

*---------------------*
*
*---------------------*
function modelsoff()

* Redraw all the buttons to the off position/color and set
* the model status array to off as well.

m = 101
while (m < 147)
  'redraw button 'm' 1'
  _modstatus.m = 1
  m = m + 1
endwhile

return

*---------------------*
*
*---------------------*
function printmodstat()

* Diagnostic function: print status of models....

m = 101
while (m < 147)
  say 'm= 'm' model= '_modname.m' modstatus= '_modstatus.m
  m = m + 1
endwhile

return

*---------------------*
*
*---------------------*
function clickmodel(m)

* First check to see if the input model number is 108, which is
* for "APTS", which means to toggle all ensemble members either
* on or off.  Some of the members may have been turned on or
* off separately, so we need to do a wholesale change:  First,
* check to see what we have just changed the value of the
* "APTS" button to, and then we change all ensemble members to 
* be the same as it.

if (m = 108)
* Check status of "APTS" button and switch it....
  if (_modstatus.108 = 0)
*     setting to 1 (OFF) for 108 (APTS)
    _modstatus.108 = 1
  else
*     setting to 0 (ON) for 108 (APTS)
    _modstatus.108 = 0
  endif
* Now force all the ensemble members to match the APTS button
  itemp = 127
  while (itemp <= 146)
    if (_modstatus.108 = 0)
*     setting to 1 (OFF) for m = 'itemp
      _modstatus.itemp = 0
    else
*       setting to 0 (ON) for m = 'itemp
      _modstatus.itemp = 1
    endif 
    itemp = itemp + 1
  endwhile

  rc = redomenu(yes)
else
  if (_modstatus.m = 0)
*    say '!!! clickmodel, setting to 1 (OFF) for m = 'm
    _modstatus.m = 1
  else
*    say '!!! clickmodel, setting to 0 (ON) for m = 'm
    _modstatus.m = 0
  endif
endif

return

*---------------------*
*
*---------------------*
function mainbox()
  _mainstuff="Clear| Print B&W Postscript| Print Color Postscript (black bg)| Print Color Postscript (white bg)| Print JPEG (black bg)| Print JPEG (white bg)| Print PNG (black bg)| Print PNG (white bg)| Print color EPS (black bg)| Print color EPS (white bg)| Exit Script| - - - - - - - | EXIT GrADS"
  _mainfunc.1="clr"
  _mainfunc.2="printbw"
  _mainfunc.3="printclbbg"
  _mainfunc.4="printclwbg"
  _mainfunc.5="printjpegblk"
  _mainfunc.6="printjpegwht"
  _mainfunc.7="printpngblk"
  _mainfunc.8="printpngwht"
  _mainfunc.9="printepsblk"
  _mainfunc.10="printepswht"
  _mainfunc.11="return"
  _mainfunc.12="null"
  _mainfunc.13="quit"
return

*---------------------*
*
*---------------------*
function gettrkopts()

_trkstuff="- - FORECAST - -| Marker Interval >15>| Single Forecast| Multiple Forecast >16>| Forecast Length >25>|- - OBSERVED - -| Marker Interval >17>| Observed Length >18>|"
_trkfunc.1 = "null"
_trkfunc.2 = "null"
_trkfunc.3 = "single_fcst"
_trkfunc.4 = "null"
_trkfunc.5 = "null"
_trkfunc.6 = "null"
_trkfunc.7 = "null"
_trkfunc.8 = "null"

return

*---------------------*
*
*---------------------*
function gettrkbtn(btnstr)

* This function parses the button (q pos) string
* returned from the user to see what input he 
* entered under the "Track" button.

topbtn = subwrd(btnstr,8)
subbtn = subwrd(btnstr,10)

if (topbtn = 2)
  if (subbtn = 1)
    _trkmrkrintrvl = "every6"
    _trkmrkrint    = 6
  endif
  if (subbtn = 2)
    _trkmrkrintrvl = "every12"
    _trkmrkrint    = 12
  endif
  if (subbtn = 3)
    _trkmrkrintrvl = "every24"
    _trkmrkrint    = 24
  endif
  if (subbtn = 4)
    _trkmrkrintrvl = "none"
    _trkmrkrint    = 0
  endif
endif

if (topbtn = 3)
  _trkfcsttype = "single"
endif

if (topbtn = 4)
  if (subbtn = 1)
    _trkfcsttype = "mult06"
  endif
  if (subbtn = 2)
    _trkfcsttype = "mult12"
  endif
  if (subbtn = 3)
    _trkfcsttype = "mult24"
  endif
endif

if (topbtn = 5) 
  if (subbtn = 1)
    _fcstlen = 72
  endif
  if (subbtn = 2)
    _fcstlen = 78
  endif
  if (subbtn = 3)
    _fcstlen = 84
  endif
  if (subbtn = 4)
    _fcstlen = 96
  endif
  if (subbtn = 5)
    _fcstlen = 120
  endif
  if (subbtn = 6)
    _fcstlen = 126
  endif
  if (subbtn = 7)
    _fcstlen = 144
  endif
  if (subbtn = 8)
    _fcstlen = 168
  endif
  if (subbtn = 9)
    _fcstlen = 999
  endif
endif

if (topbtn = 7)
  if (subbtn = 1)
    _trkverfintrvl = "every6"
    _vrfmrkrint    = 6
  endif
  if (subbtn = 2)
    _trkverfintrvl = "every12"
    _vrfmrkrint    = 12
  endif
  if (subbtn = 3)
    _trkverfintrvl = "every24"
    _vrfmrkrint    = 24
  endif
endif

if (topbtn = 8)
  if (subbtn = 1)
    _trkverflen = "next72"
  endif
  if (subbtn = 2)
    _trkverflen = "next84"
  endif
  if (subbtn = 3)
    _trkverflen = "next96"
  endif
  if (subbtn = 4)
    _trkverflen = "next120"
  endif
  if (subbtn = 5)
    _trkverflen = "next144"
  endif
  if (subbtn = 6)
    _trkverflen = "next168"
  endif
  if (subbtn = 7)
    _trkverflen = "prev72"
  endif
  if (subbtn = 8)
    _trkverflen = "prev96"
  endif
  if (subbtn = 9)
    _trkverflen = "prev120"
  endif
  if (subbtn = 10)
    _trkverflen = "prev144"
  endif
  if (subbtn = 11)
    _trkverflen = "prev168"
  endif
  if (subbtn = 12)
    _trkverflen = "begtonow"
  endif
  if (subbtn = 13)
    _trkverflen = "onlynow"
  endif
  if (subbtn = 14)
    _trkverflen = "remainder"
  endif
  if (subbtn = 15)
    _trkverflen = "entire"
  endif
endif

return

*-----------------------*
*
*-----------------------*
function getopts1(btnstr)

topbtn = subwrd(btnstr,8)
subbtn = subwrd(btnstr,10)
btn12  = subwrd(btnstr,12)
say btnstr

if (topbtn = 1)
  _plottype = track
  _zoom     = no
endif
if (topbtn = 2)
  if (subbtn = 2)
    _plottype = intensity
    _intdisp  = mslponly
    _zoom     = no
  endif
  if (subbtn = 3)
    _plottype = intensity
    _intdisp  = windonly
    _zoom     = no
  endif
  if (subbtn = 5)
    _plottype = trkinset
    _intdisp  = mslponly
    _zoom     = no
  endif
  if (subbtn = 6)
    _plottype = trkinset
    _intdisp  = windonly
    _zoom     = no
  endif
  if (subbtn = 8)
    _insetloc  = upleft
  endif
  if (subbtn = 9)
    _insetloc  = upright
  endif
  if (subbtn = 10)
    _insetloc  = lowright
  endif
  if (subbtn = 11)
    _insetloc  = lowleft
  endif
  if (subbtn = 13)
    _insetxpct = 0.25
    _insetypct = 0.25
  endif
  if (subbtn = 14)
    _insetxpct = 0.30
    _insetypct = 0.30
  endif
  if (subbtn = 15)
    _insetxpct = 0.35
    _insetypct = 0.35
  endif
  if (subbtn = 16)
    _insetxpct = 0.40
    _insetypct = 0.40
  endif
  if (subbtn = 17)
    _insetxpct = 0.45
    _insetypct = 0.45
  endif
  if (subbtn = 18)
    _insetxpct = 0.50
    _insetypct = 0.50
  endif
endif
if (topbtn = 3)
  if (subbtn = 1)
    _plotfcst = yes
  endif
  if (subbtn = 2)
    _plotfcst = no
  endif
endif
if (topbtn = 4)
  if (subbtn = 1)
    _plotverif = yes
  endif
  if (subbtn = 2)
    _plotverif = no
  endif
endif
if (topbtn = 5)
  if (subbtn = 1)
    _plotstrike = no
*    say 'STRIKE BUTTON NO: 'btnstr
  endif
  if (subbtn = 3)
    _plotstrike = yes
    _strikenm = 65
    _striketyp = accum
    _strikehr = 00
*    say 'STRIKE BUTTON YES ACCUM 65: 'btnstr
  endif
  if (subbtn = 4)
    _plotstrike = yes
    _strikenm = 100
    _striketyp = accum
    _strikehr = 00
*    say 'STRIKE BUTTON YES ACCUM 100: 'btnstr
  endif
  if (subbtn = 5)
    _plotstrike = yes
    _strikenm = 200
    _striketyp = accum
    _strikehr = 00
*    say 'STRIKE BUTTON YES ACCUM 200: 'btnstr
  endif
  if (subbtn = 7)
    _plotstrike = yes
    _strikenm = 65
    _striketyp = indiv
*    say 'STRIKE BUTTON YES INDIV 65: 'btnstr
  endif
  if (subbtn = 8)
    _plotstrike = yes
    _strikenm = 100
    _striketyp = indiv
*    say 'STRIKE BUTTON YES INDIV 100: 'btnstr
  endif
  if (subbtn = 9) 
    _plotstrike = yes
    _strikenm = 200
    _striketyp = indiv
*    say 'STRIKE BUTTON YES INDIV 200: 'btnstr
  endif
  if (subbtn = 10) 
    if (btn12 = 1)
      _strikehr = 00
    endif
    if (btn12 = 2)
      _strikehr = 12
    endif
    if (btn12 = 3)
      _strikehr = 24
    endif
    if (btn12 = 4)
      _strikehr = 36
    endif
    if (btn12 = 5)
      _strikehr = 48
    endif
    if (btn12 = 6)
      _strikehr = 60
    endif
    if (btn12 = 7)
      _strikehr = 72
    endif
    if (btn12 = 8)
      _strikehr = 84
    endif
    say 'INDIVIDUAL STRIKE HOUR = '_strikehr
  endif
  if (subbtn = 12)
    _strikebar = yes
  endif
  if (subbtn = 13)
    _strikebar = no
  endif
endif

return

*-----------------------*
*
*-----------------------*
function getopts2(btnstr)

btn8  = subwrd(btnstr,8)
btn9  = subwrd(btnstr,9)
btn10 = subwrd(btnstr,10)
btn11 = subwrd(btnstr,11)
btn12 = subwrd(btnstr,12)

if (btn8 = 1)
* looking under the model colors sub-menu....
  if (btn9 = 21)
    if (btn10 = 1)
      _modcoltype = rainbow
      return
    endif
    if (btn10 = 2)
      _modcoltype = mono
      _modmonocol = _trkcol.btn12
      return
    endif
  endif
endif

if (btn8 = 2)
* looking under the observed colors sub-menu....
  _verifcol = btn10 - 1
  return
endif

if (btn8 = 3)
* looking under the legend location sub-menu
  if (btn10 = 1)
    _legendloc = upright
    return
  endif
  if (btn10 = 2)
    _legendloc = lowright
    return
  endif
  if (btn10 = 3)
    _legendloc = lowleft
    return
  endif
  if (btn10 = 4)
    _legendloc = upleft
    return
  endif
  if (btn10 = 5)
    _legendloc = none
    return
  endif
endif

if (btn8 = 4)
* looking under the map colors sub-menu....
  _mapcol = btn10 - 1
  'set map '_mapcol' 1 4'
  return
endif

if (btn8 = 5)
* looking under the grid options sub-menu....
  if (btn10 = 1)
    _gridstyle = 5
    'set grid on 5 '_gridcol
    return
  endif
  if (btn10 = 2)
    _gridstyle = 1
    'set grid on 1 '_gridcol
    return
  endif
  if (btn10 = 3)
    _gridstyle = -999
    'set grid off'
    return
  endif
  if (btn10 = 4)
    return
  endif
  if (btn10 > 4)
    _gridcol = btn10 - 5
    if (_gridstyle > -999)
      'set grid on '_gridstyle' '_gridcol
      return
    endif
  endif
endif

if (btn8 = 6)
* looking under the "draw signature?" sub-menu....
  if (btn10 = 1)
    _drawsig = yes
  endif
  if (btn10 = 2)
    _drawsig = no
  endif
endif

return

*-----------------------*
*
*-----------------------*
function process_main(choice)

* say 'beginning of process_main, choice= 'choice

if (choice = "clr")
  'clear graphics'
  rc = redomenu()
endif
if (choice = "printbw" | choice = "printclbbg" | choice = "printclwbg" | choice = "printjpegblk" | choice = "printjpegwht" | choice = "printpngblk" | choice = "printpngwht" | choice = "printepsblk" | choice = "printepswht")
  _hardcopy = choice
  rc = output()
endif

return

*-----------------------*
*
*-----------------------*
function output()

say ' '
say ' OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO'
say ' '
say ' Output file is being written.  Please wait....'
say ' '

ifile = _netdir%'hur.grads.meta'
'enable print 'ifile
'print'
'disable print'

_outcount = _outcount + 1

if (_hardcopy = "printbw")
  ofile = _netdir%_atcfname%'.'%_startdate%'.'%_randnum%'.'%_outcount%'.ps'
  '!/opt/grads/2.0.a2/bin/gxps -i 'ifile' -o 'ofile
endif
if (_hardcopy = "printclwbg")
  ofile = _netdir%_atcfname%'.'%_startdate%'.'%_randnum%'.'%_outcount%'.ps'
  '!/opt/grads/2.0.a2/bin/gxps -c -i 'ifile' -o 'ofile
endif
if (_hardcopy = "printclbbg")
  ofile = _netdir%_atcfname%'.'%_startdate%'.'%_randnum%'.'%_outcount%'.ps'
  '!/opt/grads/2.0.a2/bin/gxps -c -r -i 'ifile' -o 'ofile
endif
if (_hardcopy = "printjpegblk")
  ofile = _netdir%_atcfname%'.'%_startdate%'.'%_randnum%'.'%_outcount%'.jpeg'
  'printim 'ofile' x1000 y773 black'
endif
if (_hardcopy = "printjpegwht")
  ofile = _netdir%_atcfname%'.'%_startdate%'.'%_randnum%'.'%_outcount%'.jpeg'
  'printim 'ofile' x1000 y773 white'
endif
if (_hardcopy = "printpngblk")
  ofile = _netdir%_atcfname%'.'%_startdate%'.'%_randnum%'.'%_outcount%'.png'
  'printim 'ofile' x1000 y773 black'
endif
if (_hardcopy = "printpngwht")
  ofile = _netdir%_atcfname%'.'%_startdate%'.'%_randnum%'.'%_outcount%'.png'
  'printim 'ofile' x1000 y773 white'
endif
if (_hardcopy = "printepswht")
  ofile = _netdir%_atcfname%'.'%_startdate%'.'%_randnum%'.'%_outcount%'.eps'
  '!/opt/grads/2.0.a2/bin/gxeps -c -i 'ifile' -o 'ofile
endif
if (_hardcopy = "printepsblk")
  ofile = _netdir%_atcfname%'.'%_startdate%'.'%_randnum%'.'%_outcount%'.eps'
  '!/opt/grads/2.0.a2/bin/gxeps -c -r -i 'ifile' -o 'ofile
endif

say ' '
say ' Output image has been written to the following file: '
say ' '
say ' 'ofile
say ' '
say ' OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO'
say ' OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO'
say ' '

return

*-----------------------*
*
*-----------------------*
function plotstrk()

if (_striketyp = accum)
  _strikehr = 0
endif

say 'startdate= '_startdate
say 'atcfnum=   '_atcfnum
say 'strikenm=  '_strikenm
say 'striketyp= '_striketyp
say 'strikehr=  '_strikehr

'!'_rundir'check_strike.sh '_startdate' '_atcfname' '_strikenm' '_striketyp' '_strikehr' >'_netdir'plotstrk.dat'
strkfile=_netdir%'plotstrk.dat'

res = read(strkfile)
rc  = sublin(res,1)
say ' '
say '!!! HEY:  RC FROM read of strkfile = 'rc
say ' '
if (rc != 0)
  if (rc = 2)
    say 'End of plot strike file: 'strkfile
    say ' '
  endif
  if(rc = 1); say 'rc=1: OPEN ERROR FOR 'strkfile; endif
  if(rc = 8); say 'rc=8: 'strkfile' OPEN FOR WRITE ONLY'; endif
  if(rc = 9); say 'rc=9: I/O ERROR FOR 'strkfile; endif
  return 99
endif

rc = close (strkfile)

strkrec = sublin(res,2)
foundflag = subwrd(strkrec,1)
grdsdate  = subwrd(strkrec,2)
ctlfile   = subwrd(strkrec,3)

if (foundflag = found)
  'open 'ctlfile
  'set dfile 2'
  'set time 'grdsdate
  if (_striketyp = accum) 
    say 'running at A'
    'run set_nhc_prob_cols.gs shaded'
  else
    say 'running at B'
    'run set_ind_prob_cols.gs shaded'
  endif
  'set gxout shaded'
  'd tprob*100'

  'q gxinfo'
  xdum=sublin(result,3)
  ydum=sublin(result,4)
  xl = subwrd(xdum,4)
  xr = subwrd(xdum,6)
  ylo = subwrd(ydum,4)
  yhi = subwrd(ydum,6)

  xst = xr + 0.3
  ytemp = ylo + yhi
  yst = ytemp * 0.5

*  xst = _xright + 0.3
*  ytemp = _ylo + _yhi
*  yst = ytemp * 0.5

  if (_strikebar = yes) 
    'run cbarn.gs 1 1 'xst' 'yst
  endif

  'set gxout contour'
  if (_striketyp = accum)
    'run set_nhc_prob_cols.gs contour'
  else
    'run set_ind_prob_cols.gs contour'
  endif
  'set clab off'
  'd tprob*100'
  'close 2'
  'set dfile 1'
  return 0
else
  say ' '
  say '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  say '   NOTE:  Strike probability file not found for'
  say '   startdate= '_startdate' atcfname= '_atcfname' strike_distance= '_strikenm
  say ' '
  return 99
endif

return

*-----------------------*
*
*-----------------------*
function getrandom()

'!'_rundir'make_random.sh >'_netdir'random_num.txt'
randfile=_netdir%'random_num.txt'

res = read(randfile)
rc  = sublin(res,1)
if (rc != 0)
  if (rc = 2)
    say 'End of random file: 'randfile
    say ' '
  endif
  if(rc = 1); say 'rc=1: OPEN ERROR FOR 'randfile; endif
  if(rc = 8); say 'rc=8: 'randfile' OPEN FOR WRITE ONLY'; endif
  if(rc = 9); say 'rc=9: I/O ERROR FOR 'randfile; endif
  return 99
endif
irand = sublin(res,2)

return irand

*-----------------------*
*
*-----------------------*
function rdstorm()

* Read in the list of storms & dates from the input file.
* Put the storms and associated dates into a string that
* will be displayed in a dropmenu.

stormfile=_netdir%'slist'
*'!ls -1 aal*2002.dat aep*2002.dat awp*2002.dat >'stormfile
*'!ls -1 aal*2002.dat aal*2003.dat >'stormfile
*'!ls -1 aal*2003.dat aal*1999.dat aep*2003.dat >'stormfile
*'!ls -1 aal*2004.dat aal102002.dat aep*2004.dat awp*2004.dat >'stormfile
*'!ls -1 aal*2003.dat aal*2004.dat aal*2002.dat >'stormfile
*'!ls -1 aal*2005.dat a*2006.dat >'stormfile
*'!ls -1 a*2006.dat >'stormfile
* '!ls -1 a*2007.dat >'stormfile
*'!ls -1 a*2003.dat >'stormfile
*'!ls -1 aal132003.dat aal162003.dat aal*2005.dat aep*2005.dat awp*2005.dat >'stormfile

'!'_rundir'get_storms.sh '_basin' '_yyyy' >'stormfile

nst = 0
_stormlist = ""
while (1)
  res = read(stormfile)
  rc  = sublin(res,1)
  if(rc != 0)
    if(rc = 2)
      say 'End of storms file: 'stormfile
      say nst' storms are available...'
      say ' '
      break
    endif
    if(rc = 1); say 'rc=1: OPEN ERROR FOR 'stormfile; endif
    if(rc = 8); say 'rc=8: 'stormfile' OPEN FOR WRITE ONLY'; endif
    if(rc = 9); say 'rc=9: I/O ERROR FOR 'stormfile; endif
    return 99
  endif
  nst = nst + 1
  _stormfile.nst = sublin(res,2)
  stormrec       = sublin(res,2)
  _atcfid.nst = substr(_stormfile.nst,2,8)
  stormname   = subwrd(stormrec,2)
  if (nst = 1)
    _stormlist = _stormlist%_atcfid.nst%" "%stormname%" "
  else
    _stormlist = _stormlist%"|"%_atcfid.nst%" "%stormname%" "
  endif
endwhile

rc = close(stormfile)

return

*-------------------------*
*
*-------------------------*
function rddates(item)

* Run a unix script to awk out the CARQ records, picking
* out only the ones for 35 kt winds and sorting out any
* duplicates.  Dump this out into a file that we will read
* for the dates and will also be used for the verifying
* positions.

_atcfname = _atcfid.item
afile     = _stormfile.item
scriptarg = _rundir%afile

'!'_rundir'list_vdates.sh 'scriptarg' >'_netdir'vlist.'item
veriffile=_netdir%'vlist.'item

_ymdct = 0
hhct   = 0
ndt    = 0
_ymdlist = ""
prevymd = 99999999
while (1)

  res = read(veriffile)
  rc  = sublin(res,1)
  if(rc != 0)
    if(rc = 2)
      say 'End of verif file: 'veriffile
      say ndt' cases are available for this storm...'
      say ' '
      break
    endif
    if(rc = 1); say 'rc=1: OPEN ERROR FOR 'veriffile; endif
    if(rc = 8); say 'rc=8: 'veriffile' OPEN FOR WRITE ONLY'; endif
    if(rc = 9); say 'rc=9: I/O ERROR FOR 'veriffile; endif
    return 99
  endif

  ndt   = ndt + 1
  drec  = sublin(res,2)
  yyyymmddhh = subwrd(drec,1)
  ymd   = substr(yyyymmddhh,1,8)
  hh    = substr(yyyymmddhh,9,2)
  _stname = subwrd(drec,2)
  _titleyear = substr(yyyymmddhh,1,4)

  if (ymd = prevymd)
    _hhstring.ymdix = _hhstring.ymdix%"|"%hh
    hhct = hhct + 1 
    _ymdh.ymdix.hhct = yyyymmddhh
  else
    hhct = 1
    _ymdct = _ymdct + 1
    ymdix = 34 + _ymdct
    ymdstring = ymd' >'ymdix'>'
    _hhstring.ymdix = hh
    _ymdlist = _ymdlist%ymdstring%"|"
    _ymdh.ymdix.hhct = yyyymmddhh

*    if (ndt = 1)
*      _ymdlist = _ymdlist%ymdstring
*    else
*      _ymdlist = _ymdlist%"|"%ymdstring
*    endif

  endif

  prevymd = ymd

endwhile

rc = close(veriffile)

_zoom = no

return

*-------------------------*
*
*-------------------------*
function makeflist()

* This whole script will work by using as 
* its input a file called flist (for the forecasts) and a file 
* called vlist (for the observed).  We need to use a unix 
* script to create these files on the fly.  This function will
* invoke a unix script to create just the flist file for now.
* There are two arguments sent to the get_mods.sh script.  The
* first, noted by the "-t", is for the forecast type, which 
* can be "single", "mult06", "mult12" or "mult24".  The second
* is the starting date for the forecast.  For example, if you 
* pick "mult06" for your type, this script will plot the 
* forecasts for your storm every 6h beginning with the starting
* date you have chosen.

* _trkfcsttype = single

mcount = 0
m = 101
modstring=' '
while (m < 147)
  if (_modstatus.m = 0)
    charmod = _modname.m
    modstring = modstring' '_modname.m
    mcount = mcount + 1
*    say '+++ YES: makeflist, modstring= 'modstring
*  else
*    say '---  NO: makeflist, m= 'm
  endif
  m = m + 1
endwhile

say ' '
say 'In makeflist....'
say '_trkfcsttype= '_trkfcsttype
say '_startdate=   '_startdate
say '_atcfname=    '_atcfname
say '_userid=      '_userid
say '_plottype=    '_plottype
say 'modstring=    'modstring
say ' '

if (_startdate = 9999)
  'set line 2 1 6'
  'draw recf 2.5 5.0 9.5 6.0'
  'set line 1 1 6'
  'draw recf 2.6 5.1 9.4 5.9'
  'set string 2 c 6'
  'set strsiz 0.14 0.14'
  dispstr = '!!! ERROR: You have not selected a starting date yet.'
  'draw string 6.0 5.5 'dispstr
  'redraw button 201 0'
  'redraw button 202 0'
  'redraw button 203 0'
  'redraw button 204 0'
  'redraw button 205 0'
  'redraw button 206 0'
  'redraw button 207 0'
  'redraw button 208 0'
  'redraw button 209 0'
  'redraw button 210 0'
  'redraw button 211 0'
  return 99
endif

if (mcount > 0)
  if (_trkfcsttype = single)
    '! '_rundir'get_mods.sh -t '_trkfcsttype' -d '_startdate' -f '_atcfname' -u '_userid' -i '_plottype' 'modstring' >'_netdir'flist'
    return 0
  else
    if (mcount > 1)
      'set line 2 1 6'
      'draw recf 1.3 5.0 10.7 6.0'
      'set line 1 1 6'
      'draw recf 1.4 5.1 10.6 5.9'
      'set string 12 c 6'
      'set strsiz 0.14 0.14'
      dispstr = '!!! ERROR: Only 1 model can be selected for the multiple-time forecast option.'
      'draw string 6.0 5.5 'dispstr
      'redraw button 201 0'
      'redraw button 202 0'
      'redraw button 203 0'
      'redraw button 204 0'
      'redraw button 205 0'
      'redraw button 206 0'
      'redraw button 207 0'
      'redraw button 208 0'
      'redraw button 209 0'
      'redraw button 210 0'
      'redraw button 211 0'
      return 99
    else
      '! '_rundir'get_mods.sh -t '_trkfcsttype' -d '_startdate' -f '_atcfname' -u '_userid' -i '_plottype' 'modstring' >'_netdir'flist'
      return 0
    endif
  endif
else
  'set line 2 1 6'
  'draw recf 2.5 5.0 9.5 6.0'
  'set line 1 1 6'
  'draw recf 2.6 5.1 9.4 5.9'
  'set string 4 c 6'
  'set strsiz 0.14 0.14'
  dispstr = '!!! ERROR: You have not selected any models to plot'
  'draw string 6.0 5.5 'dispstr
  'redraw button 201 0'
  'redraw button 202 0'
  'redraw button 203 0'
  'redraw button 204 0'
  'redraw button 205 0'
  'redraw button 206 0'
  'redraw button 207 0'
  'redraw button 208 0'
  'redraw button 209 0'
  'redraw button 210 0'
  'redraw button 211 0'
  rc = modelsoff()
  return 99
endif

return

*-------------------------*
*
*-------------------------*
function makevlist()

* This script will use your options to go and pick through
* the atcfunix file and get the "CARQ" observed records.

say ' '
say 'in makevlist....'
say '_trkverfintrvl= '_trkverfintrvl
say '_trkverflen= '_trkverflen
say '_atcfname= '_atcfname
say '_startdate= '_startdate

'! '_rundir'get_verif.sh -i '_trkverfintrvl' -t '_trkverflen' -f '_atcfname' -d '_startdate' -u '_userid' >'_netdir'vlist'

return

*-------------------------*
*
*-------------------------*
function modnames()

_modname.101 = GFDL
_modname.102 = GFDI
_modname.103 = AEMN
_modname.104 = AVNO
_modname.105 =  UKM
_modname.106 = HRE2
_modname.107 = HREL
_modname.108 = APTS
_modname.109 = HWRF
_modname.110 = DSHP

_modname.111 = AEMI
_modname.112 = SHIP
_modname.113 = SRMN
_modname.114 = UKMI
_modname.115 = GFDH
_modname.116 = GFTI
_modname.117 = AVNI
_modname.118 = GFSO
_modname.119 = TVCN
_modname.120 =  EMX

_modname.121 =  JSG
_modname.122 = NGPS
_modname.123 = HTUT
_modname.124 = HCYC
_modname.125 = AHWT
_modname.126 = SRMN
_modname.127 = AP01
_modname.128 = AP02
_modname.129 = AP03
_modname.130 = AP04

_modname.131 = AP05
_modname.132 = AP06
_modname.133 = AP07
_modname.134 = AP08
_modname.135 = AP09
_modname.136 = AP10
_modname.137 = AP11
_modname.138 = AP12
_modname.139 = AP13
_modname.140 = AP14

_modname.141 = AP15
_modname.142 = AP16
_modname.143 = AP17
_modname.144 = AP18
_modname.145 = AP19
_modname.146 = AP20

return

*-------------------------*
*
*-------------------------*
function settrkcols()

* This function sets the colors that will be used
* for the various track lines.  The colors were 
* chosen so that they would appear distinct enough
* with either a black or white background, and 
* were limited to only 7 colors so as not to have 
* too many that would be confused with each other.

* 21 = dark green
* 22 = bright steel blue
* 23 = tomato red
* 24 = mustard yellow
* 25 = purple
* 26 = light blue
* 27 = pink
* 28 = grey

'set rgb 21   50 150   0'
'set rgb 22   10  70 225'
'set rgb 23  200  25   0'
'set rgb 24  225 180   0'
'set rgb 25  170  40 190'
'set rgb 26    0 170 170'
'set rgb 27  255  15 150'
'set rgb 28  140 140 140'

_trkcol.1 = 21
_trkcol.2 = 22
_trkcol.3 = 23
_trkcol.4 = 24
_trkcol.5 = 25
_trkcol.6 = 26
_trkcol.7 = 27
_trkcol.8 = 28

_numtrkcols = 8

return

*-------------------------*
*
*-------------------------*
function setbtncols()

* This function sets the colors that will be used for the 
* colors of the buttons.

* Gray for dropmenu
* 'set rgb 90 100 100 100'
* 'set rgb 91 150 150 150'
* 'set rgb 92 200 200 200'

* Blue for dropmenu
'set rgb 90   0   0 100'
'set rgb 91   0  50 150'
'set rgb 92   0 100 250'

* Gray for model buttons....
'set rgb 40 100 100 100'
'set rgb 41  50  50  50'
'set rgb 42 200 200 200'

* Red for quit buttons....
'set rgb 50 175   0   0'
'set rgb 51 100   0   0'
'set rgb 52 255   0   0'

return
*-------------------------*
*
*-------------------------*
function drawlegend(modelct,model,trackcolor,ymdh)

* This function draws the legend of the models on 
* the plot.  The variables "_yhi", "_ylo", "_xleft"
* and "_xright" come from the function "getgxinfo".

if (_trkfcsttype = single)
  modstring = modelct%' '%model
  xroffset = 0
else
  mmddhh = substr(ymdh,5,6)
  modstring = modelct%' '%mmddhh
  xroffset = 0.22
endif

if (_legendloc = upright | _legendloc = upleft)

  yleghi = _yhi - 0.1
  ydecr  = (modelct - 1) * 0.2
  ystart = yleghi - ydecr
  
  if (_legendloc = upright)
    if (modelct > 9)
      xstart = _xright - (0.80 + xroffset)
    else
      xstart = _xright - (0.70 + xroffset)
    endif
  else
    if (modelct <= 9)
      xstart = _xleft + 0.12
    else
      xstart = _xleft + 0.02
    endif
  endif

  'set string 'trackcolor' l 6'
  'set strsiz 0.11 0.11'
  'draw string 'xstart' 'ystart' 'modstring

endif

if (_legendloc = lowright | _legendloc = lowleft)

  yleglo = _ylo + 0.1
  yincr  = (modelct - 1) * 0.2
  ystart = yleglo + yincr
 
  if (_legendloc = lowright)
    if (modelct > 9)
      xstart = _xright - (0.80 + xroffset)
    else
      xstart = _xright - (0.70 + xroffset)
    endif
  else
    if (modelct <= 9)
      xstart = _xleft + 0.12
    else
      xstart = _xleft + 0.02
    endif
  endif

  'set string 'trackcolor' l 6'
  'set strsiz 0.11 0.11'
  'draw string 'xstart' 'ystart' 'modstring

endif

return

*-------------------------*
*
*-------------------------*
function drawtitle()

'q gxinfo'
xdum=sublin(result,3)
ydum=sublin(result,4)
xl = subwrd(xdum,4)
xr = subwrd(xdum,6)
ylo = subwrd(ydum,4)
yhi = subwrd(ydum,6)
xdiff = xr - xl
ydiff = yhi - ylo
hsiz = ((xdiff * 0.02) + (ydiff * 0.025)) / 2
vsiz = hsiz

hsiz = 0.13
vsiz = 0.13

xstart = (xdiff/2) + xl
if (_plotstrike = yes)
  ystart = yhi + 0.65
else
  ystart = yhi + 0.40
endif

if(_plottype = track)
  cstr = _titleyear%' Tropical Cyclone Tracks'
else
  cstr = _titleyear%' Tropical Cyclone Intensity'
endif
'set string 1 c 5'
'set strsiz 'hsiz' 'vsiz
'draw string 'xstart' 'ystart' 'cstr

cstr = 'Storm: '_atcfnum' ('_stname')'
ystart = ystart - 0.25
'set string 1 c 5'
'set strsiz 'hsiz' 'vsiz
'draw string 'xstart' 'ystart' 'cstr

if (_plotstrike = yes)
  if (_striketyp = accum)
    cstr = 'Probability (%) of storm passing within '_strikenm'nm during next 72h'
  else
    cstr = 'Probability (%) of storm being within '_strikenm'nm at '_strikehr'h' 
  endif
  ystart = ystart - 0.25
  'set string 1 c 5' 
  'set strsiz 'hsiz' 'vsiz
  'draw string 'xstart' 'ystart' 'cstr
endif

hsiz = 0.12
vsiz = 0.12

ystart = ylo
if (_plotfcst = yes)
  ystart = ystart - 0.35
  if (_trkfcsttype = single)
    cstr = 'Forecasts: Beginning '_ftitleymdh
  else
    cstr = 'Forecasts: Beginning '_ftitleymdh' for '_ftitlemodel' model'
  endif
  'set string 1 c 5'
  'set strsiz 'hsiz' 'vsiz
  'draw string 'xstart' 'ystart' 'cstr
endif

if (_trkverfintrvl = every6)
  vhrint = 6
endif
if (_trkverfintrvl = every12)
  vhrint = 12
endif
if (_trkverfintrvl = every24)
  vhrint = 24
endif

if (_plotverif = yes)
  ystart = ystart - 0.25
  if (_plotfcst = no)
    ystart = ystart - 0.10
  endif
  cstr = 'Observed: Beginning '_vtitleymdh', every 'vhrint' hours'
  'set string 1 c 5'
  'set strsiz 'hsiz' 'vsiz
  'draw string 'xstart' 'ystart' 'cstr
endif

return

*-----------------------------
*
*-----------------------------
function plotsig()

* This function will plot your signature in the
* lower left corner of the frame.  If the signature
* is too small or too big, change the value of
* charparm.

charparm = 0.013

'q gxinfo'
xdum=sublin(result,3)
ydum=sublin(result,4)
xl = subwrd(xdum,4)
xr = subwrd(xdum,6)
yhi = subwrd(ydum,6)
xdiff = xr - xl
hsiz = xdiff * charparm
vsiz = hsiz + 0.02

xstart = 10.95
ystart = 0.05

*say 'plotsig, hsiz= 'hsiz' vsiz= 'vsiz
*hsiz= 0.08453926 vsiz= 0.10453926
hsiz = 0.09
vsiz = 0.09

cstr = 'GFDL Hurricane Dynamics Group'
* cstr = 'NCEP Hurricane Forecast Project'
'set string 7 br 4 0'
'set strsiz 'hsiz' 'vsiz
'draw string 'xstart' 'ystart' 'cstr

return
