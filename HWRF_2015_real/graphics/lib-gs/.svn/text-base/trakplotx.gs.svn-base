function drawverif(args)

verffile = args            

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
if (xdiff > 50 & ydiff > 50)
  hurrsize = 0.30
else
  hurrsize = 0.30
endif

vct = 0

while (1)

  res = read(verffile)
  rc  = sublin(res,1)
  if(rc != 0)
    if(rc = 2)
      say 'End of verification file '
      say ' '
      break
    endif
    if(rc = 1); say 'rc=1: OPEN ERROR FOR 'verffile; endif
    if(rc = 8); say 'rc=8: 'verffile' OPEN FOR WRITE ONLY'; endif
    if(rc = 9); say 'rc=9: I/O ERROR FOR 'verffile; endif
    return 99
  endif

  rec = sublin(res,2)

*  ymdh     = subwrd(rec,1)
*  model    = subwrd(rec,2)
  fhr      = subwrd(rec,2)
  newlat   = subwrd(rec,6)
  newlon   = subwrd(rec,4)
*  _atcfnum = subwrd(rec,6)
*  _stname  = subwrd(rec,7)  * This is now read in in rddates

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
*    if (xlast > _xright | xlast < _xleft | ylast > _yhi | ylast < _ylo)
*      oldbound = out
*    endif
*    if (xnew > _xright | xnew < _xleft | ynew > _yhi | ynew < _ylo)
*      newbound = out
*    endif

    if (oldbound = in | newbound = in)
      'set line 4 1 5'
      'draw line 'xlast' 'ylast' 'xnew' 'ynew
      'draw line 'lastlat' 'lastlon' 'newlat' 'newlon
      check12 = math_fmod(fhr,12)
      if (check12 = 0)
        'set line 4 1 5'
        ' set string 4 c 6'
        'draw string  'xnew' 'ynew' 'X
        'draw string  'newlon' 'newlat' 'X    
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

*    if (xnew <= _xright & xnew >= _xleft & ynew <= _yhi & ynew >= _ylo)
      'set line 4 1 5'
      ' set string 4 c 6'
      'draw string  'xnew' 'ynew' 'X                    
      'draw string  'newlon' 'newlat' 'X
*    endif

  endif

  oldymdh = 95000000
  lastlon  = newlon
  lastlat  = newlat


endwhile

rc = close(verffile)

return
