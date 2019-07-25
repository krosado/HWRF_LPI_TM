*
*  GraDS script - storm.gs
*
*-------------------------------------------------------------

say 'TOP OF hwrf_gr_track.gs'

numcaps=0
* open dummy file.for grads
*
*'open dummy-for-storm.ctl'
*'set font 5'
**'set lat 30 50'   
**'set lon -80 -50'
**'set map 1 1 9'
**'set vpage 1.5 8.5 1.5 11'
*'set cint 100000'
*'set black -10 10'
*'set grads off'
*'set grid off'
*'d ugrdprs'
*--------------------------------------------------------------------
*
'query gxinfo'
rec3 = sublin(result,3) 
rec4 = sublin(result,4) 
xlim1 = subwrd(rec3,4)
xlim2 = subwrd(rec3,6)
ylim1 = subwrd(rec4,4)
ylim2 = subwrd(rec4,6)
xd = xlim2 - xlim1
yd = ylim2 - ylim1
*say 'xd = 'xd'  yd = 'yd
*say 'xlim1 = 'xlim1'  ylim1 = 'ylim1
'query dims'
rec3 = sublin(result,2) 
rec4 = sublin(result,3) 
xl1 = subwrd(rec3,6)
xl2 = subwrd(rec3,8)
      if (xl1 < 0)
      xl1 = xl1  + 360.0
      endif
      if (xl2 < 0)
      xl2 = xl2  + 360.0
      endif
yl1 = subwrd(rec4,6)
yl2 = subwrd(rec4,8)
lonw = xl1
lats = yl1
xld = xl2 - xl1
yld = yl2 - yl1
*say 'xld = 'xld'  yld = 'yld
*say 'lats = 'lats'  lonw = 'lonw
*--------------------------------------------------------------------
*-------------------------------------------------------------
m00=2
m06=3
m12=4
m18=5
00UTC='00 UTC'
12UTC='12 UTC'
*-------------------------------------------------------------
dfile.1=1
if(dfile.1 = 1)
*
numcaps=numcaps+1
cap.1='Regional'
cl1=1
color.1=2
*-------------------------------------------------------------
*  open top box storm track file
*
*-------------------------------------------------------------
name1 = 'STORM.track'        
*
flag=0
*
* Setup dims
'set gxout line'
*'set mpdset hires'
'set mpdset lowres'
'set map 5 1 6'
str1 = read(name1)
code = subwrd(str1,1) 
titl = sublin(str1,2) 
*say 'Title = 'titl
*'set string 1 tc 6'
*'draw string 4.5 5.6 'titl''
* Pull storms coords
*
say 'in hwrf_gr_track.gs, enter loop...'
while (code!=2)
  str1 = read(name1)
  say 'in hwrf_gr_track.gs, code "'str1'"...'
  code = subwrd(str1,1)
  str2 = sublin(str1,2)
  say 'in hwrf_gr_track.gs, read in "'str2'"...'
  ylat = subwrd(str2,1)
  xlon = subwrd(str2,2)
  tim  = subwrd(str2,3)
  say 'in hwrf_gr_track.gs, tim='tim' lat='ylat' lon='xlon
*  date = subwrd(str2,4)
*  pres = subwrd(str2,5)
*  wind = subwrd(str2,6)
  if (code !=2)
      if(xlon < 0 )
      xlon = xlon + 360.0
      endif
    ylat = (ylat - lats)/ yld * yd
    xlon = (xlon - lonw)/ xld * xd
    ylat = ylat + ylim1
    xlon = xlon + xlim1
*   if (wind <= 33)
*       sym = 40
*       siz = 4 
*       clr=4
*    endif
*    if (wind > 33 & wind <= 63)
*       sym = 40
*       siz = 6
*       clr=3
*    endif
*    if (wind >= 64 )
*       sym = 41
*       siz = 10  
*       clr=2
*    endif
siz=0.3
    'set line 'cl1'  1 6'
    if (flag > 0)
       'draw line 'xold' 'yold' 'xlon' 'ylat
    endif
    if (tim = 00 )
    'draw mark 'm00' 'xlon' 'ylat' .051'
    endif
    if (tim = 12 )
    'draw mark 'm12' 'xlon' 'ylat'  .01'
    endif
    xold = xlon
    yold = ylat
    flag = flag + 1
  endif
endwhile
*--------------------------------------------------------------------
endif
dfile.2=2
if(dfile.2 = 1 )
numcaps=numcaps+1
cap.2='Observed'    
color.'numcaps'=4
cl2=1
*
*  open next the control track file.
*name1 = 'hwrf.track'
*
*--------------------------------------------------------------------
flag=0
str1 = read(name1)
code = subwrd(str1,1) 
titl = sublin(str1,2) 
*say 'Title = 'titl
*--------------------------------------------------------------------
*
* Pull storms coords
*
*--------------------------------------------------------------------
*
while (code!=2)
  str1 = read(name1)
  code = subwrd(str1,1)
  str2 = sublin(str1,2)
  ylat = subwrd(str2,1)
  xlon = subwrd(str2,2)
  tim  = subwrd(str2,3)
*  date = subwrd(str2,4)
*  pres = subwrd(str2,5)
*  wind = subwrd(str2,6)
  if (code !=2)
      if(xlon < 0 )
      xlon = xlon + 360.0
      endif
    ylat = (ylat - lats)/ yld * yd
    xlon = (xlon - lonw)/ xld * xd
    ylat = ylat + ylim1
    xlon = xlon + xlim1
*    if (wind <= 33)
*       sym = 40
*       siz = 4 
*       clr=4
*    endif
*    if (wind > 33 & wind <= 63)
*       sym = 40
*       siz = 6
*       clr=3
*    endif
*    if (wind >= 64 )
*       sym = 41
*       siz = 10  
*       clr=2
*    endif
 siz=0.3
    'set line 'cl2'  2 6'
    if (flag > 0)
       'draw line 'xold' 'yold' 'xlon' 'ylat
    endif
    if (tim = 00 )
    'draw mark 'm00' 'xlon' 'ylat' .1'
    endif
    if (tim = 12 )
    'draw mark 'm12' 'xlon' 'ylat' .1'
    endif
*   'draw wxsym 'sym' 'xlon' 'ylat' 'siz' 1 6'
    xold = xlon
    yold = ylat
    flag = flag + 1
  endif
endwhile
*--------------------------------------------------------------------
endif
