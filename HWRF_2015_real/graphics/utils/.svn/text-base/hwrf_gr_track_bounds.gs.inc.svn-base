************************************************************************
************************************************************************
* BEGIN FILE hwrf_gr_track_bounds.gs.inc

* IMPORTANT: MAKE SURE YOU INCLUDE hwrf_gr_time_manip.gs.inc IF YOU
* INCLUDE THIS FILE

function mnytrk_minp_bnd(time0,tmin,tmax,tstep,vpad)

bounds_init_impl()

FN=1
while(FN <= _count)
  if(_defined.FN)
    file_bounds_impl(_model.FN % ".track",0)
  endif
  FN=FN+1
endwhile

fn_minp_bnd_impl(time0,tmin,tmax,tstep,vpad)

* END FUNCTION mnytrk_maxw_bnd

************************************************************************

function mnytrk_maxw_bnd(time0,tmin,tmax,tstep,vpad)

bounds_init_impl()

FN=1
while(FN <= _count)
  if(_defined.FN)
    file_bounds_impl(_model.FN % ".track",0)
  endif
  FN=FN+1
endwhile

fn_maxw_bnd_impl(time0,tmin,tmax,tstep,vpad)

* END FUNCTION mnytrk_maxw_bnd

************************************************************************

function many_track_bound(ratio,border,maxborder)

bounds_init_impl()

FN=1
while(FN <= _count)
  if(_defined.FN)
    file_bounds_impl(_model.FN % ".track",0)
  endif
  FN=FN+1
endwhile

final_bound_impl(ratio,border,maxborder)

* END FUNCTION calc_plot_bounds

************************************************************************

function one_track_bound(filename,ratio,border,maxborder,wordy)

bounds_init_impl()
file_bounds_impl(filename,wordy)
final_bound_impl(ratio,border,maxborder)

* END FUNCTION file_plot_bounds

************************************************************************

function prev_track_bound()

set_bounds_impl()

* END FUNCTION set_prior_bounds

************************************************************************

function set_track_bound(nlat,slat,wlon,elon,wantratio)

decide_bnd_impl(nlat,slat,wlon,elon,wantratio)
set_bounds_impl()

* END FUNCTION set_bounds

************************************************************************

function stringify_bounds()

return _bounds_nlat ' ' _bounds_slat ' ' _bounds_elon ' ' _bounds_wlon

* END FUNCTION stringify_bounds

************************************************************************
** PRIVATE IMPLEMENTATION FUNCTIONS ************************************
************************************************************************

* DO NOT CALL ANY FUNCTIONS IN hwrf_gr_track_bounds.gs.inc BELOW THIS
* LINE.  THEY ARE ONLY MEANT TO BE CALLED BY THE PUBLIC IMPLEMENTATION
* FUNCTIONS ABOVE

function set_bounds_impl()

have_bounds_impl()
check_init_impl()

nlat=_bounds_nlat
slat=_bounds_slat
elon=_bounds_elon
wlon=_bounds_wlon

* say 'Final bounds:    N S E W = ' nlat ' ' slat ' ' elon ' ' wlon

* day 'set lat ' slat ' ' nlat
* day 'set lon ' wlon ' ' elon

'set lat ' slat ' ' nlat
'set lon ' wlon ' ' elon

* END FUNCTION set_bounds_impl

************************************************************************

function have_bounds_impl()

if(_have_bounds != 'YES')
  say 'WARNING: I do not have any bounds information to use.  I will use the'
  say '  internal default bounds instead.  You probably forgot to call'
  say '  calc_plot_bounds, file_plot_bounds or fixed_bounds, OR all tracks'
  say '  you provided were empty or invalid.'
endif

* END FUNCTION have_bounds_impl

************************************************************************

function check_init_impl()

if( _bounds_init != 'YES' )
  say 'WARNING: trying to use bounds information before initializing bounds '
  say '  code.  Either you never called calc_plot_bounds,'
  say '  file_plot_bounds or fixed_bounds, OR you are calling internal'
  say '  implementation functions directly (do not do that).'
endif

* END FUNCTION check_init_impl

************************************************************************

function decide_bnd_impl(nlat,slat,wlon,elon,wantratio)

check_init_impl()

* day 'Requested bound: N S E W = ' nlat ' ' slat ' ' elon ' ' wlon

dlat=(nlat-slat)
dlon=(elon-wlon)
ratio=dlat/dlon

if(ratio>wantratio)
*  say "EXPAND LON"
  newdlon=dlat/wantratio
  ddlon=newdlon-dlon
  wlon=wlon-ddlon/2
  elon=elon+ddlon/2
*  say 'Plot bounds now: N S E W = ' nlat ' ' slat ' ' elon ' ' wlon
endif

if(ratio<wantratio)
*  say "EXPAND LAT"
  newdlat=dlon*wantratio
  ddlat=newdlat-dlat
  nlat=nlat+ddlat/2
  slat=slat-ddlat/2
*  say 'Plot bounds now: N S E W = ' nlat ' ' slat ' ' elon ' ' wlon
endif

if(wlon<0)
  wlon=wlon+360
endif
if(elon<0)
  elon=elon+360
endif

_bounds_nlat=nlat
_bounds_slat=slat
_bounds_elon=elon
_bounds_wlon=wlon
_have_bounds='YES'

* END FUNCTION decide_bnd_impl

************************************************************************

function file_bounds_impl(filename,wordy)

if(wordy = '')
*  say 'wordy not defined so setting to 0'
  wordy=0
endif

check_init_impl()

*say 'read "' filename '"'
str1 = read(filename)
code = subwrd(str1,1) 
if(wordy = 1)
  titl='????'
else
  titl = sublin(str1,2) 
  str1 = read(filename)
  code = subwrd(str1,1)
endif
while (code=0 & code!='')
*  say 'code='code
  str2 = sublin(str1,2)
*  say 'line "' str2 '"'
  if(wordy = 1)
    i=1
    while(i<=strlen(str2) & substr(str2,i,1)!=':')
      i=i+1
    endwhile
    if(i>strlen(str2))
*      say filename ': invalid line "' str2 '"'
      str1 = read(filename)
      code = subwrd(str1,1)
      continue
*    else
*      say 'first colon at index 'i
    endif
    rest=substr(str2,i,strlen(str2)-i+1)
*    say 'after first ":" is "' rest '"'
    time = subwrd(rest,1)
    xlon = subwrd(rest,3)
    ylat = subwrd(rest,5)
    pres = subwrd(rest,9)
    wind = subwrd(rest,14)
  else
    time = subwrd(str2,1)
    xlon = subwrd(str2,2)
    ylat = subwrd(str2,3)
    pres = subwrd(str2,4)
    wind = subwrd(str2,5)
  endif

  if(xlon>180)
    xlon=xlon-360
  endif

*  say titl ' ' time ' ' xlon ' ' ylat ' ' pres ' ' wind

  if(titl = 'BEST')
    if(_bhave=0)
      _bnlat=ylat
      _bslat=ylat
      _belon=xlon
      _bwlon=xlon
      _bhave=1
      if(wind>0)
        _bminwind=wind
        _bmaxwind=wind
      endif
      if(pres>0)
        _bminpres=pres
        _bmaxpres=pres
      endif
      _bminhour=time
      _bmaxhour=time
    endif

    if(wind>0)
      if(wind<_bminwind); _bminwind=wind ; endif
      if(wind>_bmaxwind); _bmaxwind=wind ; endif
    endif
    if(pres>0)
      if(pres<_bminpres); _bminpres=pres ; endif
      if(pres>_bmaxpres); _bmaxpres=pres ; endif
    endif
    if(time<_bmintime); _bmintime=time ; endif
    if(time>_bmaxtime); _bmaxtime=time ; endif
 
    pastw=math_fmod(_bwlon-xlon,360)
    paste=math_fmod(xlon-_belon,360)

    if(pastw > 0)
        _bwlon=_bwlon-pastw
    else
      if(paste > 0)
        _belon=_belon+paste
      endif
    endif

    if(ylat > _bnlat)
      _bnlat=ylat
    endif
  
    if(ylat < _bslat)
      _bslat=ylat
    endif
  endif

  if(_mhave=0)
    _mnlat=ylat
    _mslat=ylat
    _melon=xlon
    _mwlon=xlon
    _mhave=1
    if(wind>0)
      _mminwind=wind
      _mmaxwind=wind
    endif
    if(pres>0)
      _mminpres=pres
      _mmaxpres=pres
    endif
    _mminhour=time
    _mmaxhour=time
  endif

  if(wind>0)
    if(wind<_mminwind); _mminwind=wind ; endif
    if(wind>_mmaxwind); _mmaxwind=wind ; endif
  endif
  if(pres>0)
    if(pres<_mminpres); _mminpres=pres ; endif
    if(pres>_mmaxpres); _mmaxpres=pres ; endif
  endif
  if(time<_mmintime); _mmintime=time ; endif
  if(time>_mmaxtime); _mmaxtime=time ; endif

  pastw=math_fmod(_mwlon-xlon,360)
  paste=math_fmod(xlon-_melon,360)

  if(pastw > 0)
      _mwlon=_mwlon-pastw
  else
    if(paste > 0)
      _melon=_melon+paste
    endif
  endif

  if(ylat > _mnlat)
    _mnlat=ylat
  endif

  if(ylat < _mslat)
    _mslat=ylat
  endif

  str1 = read(filename)
  code = subwrd(str1,1)

*  say 'Maximum bounds:  N S E W = ' _mnlat ' ' _mslat ' ' _melon ' ' _mwlon
*  say 'Best track:      N S E W = ' _mnlat ' ' _mslat ' ' _melon ' ' _mwlon

endwhile
close(filename)

* END FUNCTION file_bounds_impl

************************************************************************

function bounds_init_impl()

_mhave=0
_mnlat=70
_mslat=-30
_mwlon=-160
_melon=60
_mminwind=0
_mmaxwind=200
_mminpres=850
_mmaxpres=1050
_mminhour=0
_mmaxhour=126

_bhave=0
_bnlat=70
_bslat=-30
_bwlon=-160
_belon=60
_bminwind=0
_bmaxwind=200
_bminpres=850
_bmaxpres=1050
_bminhour=0
_bmaxhour=126

_bounds_init='YES'
_have_bounds='NO'

* END FUNCTION bounds_init

************************************************************************

function set_txaxis_impl(time0,tmin,tmax,tstep,ymin,ymax)

ttt=cycle_add(time0,tmin)
time1=cycle_to_grads(ttt)
ttt=cycle_add(time0,tmax)
time2=cycle_to_grads(ttt)

'set x 1'
'set y 1'
'set vrange 'ymin' 'ymax
*say 'set time 'time1' 'time2
'set time 'time1' 'time2
*say 'set xaxis 'tmin' 'tmax' 'tstep
'set xaxis 'tmin' 'tmax' 'tstep
*  using internal variable lat, set up plot area
*  setting style and mark for that line to 0 so that it won't show up
'set font 0'
'set grid on' 
'set grads off'
'set timelab off'
'set cstyle 0'
'set cmark 0'
'd lat'
*say 'ymax' ymax 'ymin 'ymin
'd lat*0-'ymax'+'ymin

*say 'quitting early deliberately'
*'quit'

* END FUNCTION set_xyaxis_impl

************************************************************************

function set_tpaxis_impl(time0,tmin,tmax,tstep,ymin,ymax)

ttt=cycle_add(time0,tmin)
time1=cycle_to_grads(ttt)
ttt=cycle_add(time0,tmax)
time2=cycle_to_grads(ttt)

'set x 1'
'set y 1'
'set vrange 'ymin' 'ymax
*say 'set time 'time1' 'time2
'set time 'time1' 'time2
*say 'set xaxis 'tmin' 'tmax' 'tstep
'set xaxis 'tmin' 'tmax' 'tstep
*  using internal variable lat, set up plot area
*  setting style and mark for that line to 0 so that it won't show up
'set font 0'
'set grid on' 
'set grads off'
'set timelab off'
'set cstyle 0'
'set cmark 0'
'd lat'
*say 'ymax' ymax 'ymin 'ymin
'd lat*0-'ymax'+'ymin

*say 'quitting early deliberately'
*'quit'

* END FUNCTION set_xyaxis_impl

************************************************************************

function fn_maxw_bnd_impl(time0,tmin,tmax,tstep,vpad)

set_txaxis_impl(time0,tmin,tmax,tstep,_mminwind-vpad,_mmaxwind+vpad)

* END FUNCTION fn_maxw_bnd_impl

************************************************************************

function fn_minp_bnd_impl(time0,tmin,tmax,tstep,vpad)

set_tpaxis_impl(time0,tmin,tmax,tstep,_mminpres-vpad,_mmaxpres+vpad)

* END FUNCTION fn_minp_bnd_impl

************************************************************************

function final_bound_impl(ratio,border,maxborder)

*say 'Maximum bounds:  N S E W = ' _mnlat ' ' _mslat ' ' _melon ' ' _mwlon
*say 'Best track:      N S E W = ' _mnlat ' ' _mslat ' ' _melon ' ' _mwlon

_mwlon=_mwlon-maxborder
_melon=_melon+maxborder
_mnlat=_mnlat+maxborder
_mslat=_mslat-maxborder

*if(_bhave)
if(1 > 2)
*  say 'Clip bounds to within ' border ' degrees of best track bounds'
*** Expand best track bounds by "border" degrees
  _bwlon=_bwlon-border
  _belon=_belon+border
  _bnlat=_bnlat+border
  _bslat=_bslat-border

*** Clip bounds to within the newly expanded best track bounds
  if(_mwlon<_bwlon)
    _mwlon=_bwlon
  endif
  if(_melon>_belon)
    _melon=_belon
  endif
  if(_mnlat>_bnlat)
    _mnlat=_bnlat
  endif
  if(_mslat<_bslat)
    _mslat=_bslat
  endif
endif

*say 'Plot bounds now: N S E W = ' _mnlat ' ' _mslat ' ' _melon ' ' _mwlon

decide_bnd_impl(_mnlat,_mslat,_mwlon,_melon,ratio)
set_bounds_impl()

* END FUNCTION final_bound_impl

* END FILE hwrf_gr_track_bounds.gs.inc
************************************************************************
************************************************************************
