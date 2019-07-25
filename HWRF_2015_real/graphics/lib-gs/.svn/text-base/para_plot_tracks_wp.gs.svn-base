*************************************************************
*  GraDS script - hwrf_gr_plot_tracks_HR12.gs 
*
*************************************************************
'set display color white'
numcaps=0
'open dummy.ctl' 
outfile='track.png'
'set display color white' 
'set vpage off'
'set parea 0.6 10.7 0.5 6.7'
'set xlopts 1 7 0.14'
'set ylopts 1 7 0.14'
'set xlint 5'
'set ylint 5'
'set font 0'
'set map 15 1 6'
'set mpdset hires'
'set grid on 5 15' 
'set lat slat nlat '
'set lon wlon elon '
*************************************************************
'set black -1000000 1000000'
'set grads off'
*'set timelab on'
*'d dum'
*************************************************************
* Draw the Title
*************************************************************
'set string 1 c 6'
'set strsiz 0.24 0.24'
'draw string 5.5 8.15 MC_MODEL 2013 Real time: TC Tracks'
'set strsiz 0.18 0.18'
'draw string 5.5 7.7 Storm: STORMNAME (STID) valid YYYYMMDDHH '
'draw map'
'd lat'
*************************************************************
'query gxinfo'
rec3 = sublin(result,3) 
rec4 = sublin(result,4) 
xlim1 = subwrd(rec3,4)
xlim2 = subwrd(rec3,6)
ylim1 = subwrd(rec4,4)
ylim2 = subwrd(rec4,6)
xd = xlim2 - xlim1
yd = ylim2 - ylim1
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
say 'xld = 'xld'  yld = 'yld
say 'lats = 'lats'  lonw = 'lonw
*************************************************************
*************************************************************
dfile.1=1
if(dfile.1 = 1)
*
numcaps=numcaps+1
cap.1 = 'JTWC: JTWC Official' 
color.1=9
mark.1=2
line.1=1
thick.1=6
*************************************************************
*  open top box storm track file
*************************************************************
name1 = 'JTWC.track'        
*
flag=0
*
******************** Setup dims
'set gxout line'
str1 = read(name1)
code = subwrd(str1,1) 
titl = sublin(str1,2) 
say 'Title = 'titl
******************** Pull storms coords
firstpoint=1
while (code!=2 & code!='')
  str1 = read(name1)
  code = subwrd(str1,1)
  str2 = sublin(str1,2)
 say name1 ': "' str2 '"' 
  ylat = subwrd(str2,3)
  xlon = subwrd(str2,2)
  tim  = subwrd(str2,1)
  say 'parsed: xlon=' xlon ' ylat=' ylat ' tim=' tim ' titl=' titl
  if (xlon='' | ylat = '' | tim = '')
   code=2
  endif
  if (code !=2)
    if(xlon < 0 )
      xlon = xlon + 360.0
    endif
*********** test for track crossing 0 (360) begins
          if (tim = 0.0)
            xlon_first = xlon
          endif
          if( xlon_first - xlon > 300 )
            xlon = xlon + 360.0
          endif
*********** test ends
    ylat = (ylat - lats)/ yld * yd
    xlon = (xlon - lonw)/ xld * xd
    ylat = ylat + ylim1
    xlon = xlon + xlim1
    siz=0.3
    'set line 'color.1' 1 ' thick.1
          if (firstpoint=0)
            'draw line 'xold' 'yold' 'xlon' 'ylat
          else
            firstpoint=0
          endif

    if (flag > 0)
    'draw line 'xold' 'yold' 'xlon' 'ylat
    'draw mark 'mark.1' 'xold' 'yold' .08'
    'draw mark 'mark.1' 'xlon' 'ylat' .08'
    if (flag = 1)
    'draw wxsym 40 'xold' 'yold' 'siz' 1 6'
    endif
    endif
    xold = xlon
    yold = ylat
    flag = flag + 1
  endif
endwhile
*************************************************************
endif
*************************************************************
dfile.2=2
if(dfile.2 = 2)
*
numcaps=numcaps+1
cap.2 = 'NGPS: NGPS Forecast' 
color.2=5
mark.2=3
line.2=1
thick.2=6
*************************************************************
*  open top box storm track file
*************************************************************
name2 = 'NGPS.track'        
*
flag=0
*
******************** Setup dims
'set gxout line'
str1 = read(name2)
code = subwrd(str1,1) 
titl = sublin(str1,2) 
say 'Title = 'titl
******************** Pull storms coords
firstpoint=1
while (code!=2 & code!='')
  str1 = read(name2)
  code = subwrd(str1,1)
  str2 = sublin(str1,2)
 say name2 ': "' str2 '"' 
  ylat = subwrd(str2,3)
  xlon = subwrd(str2,2)
  tim  = subwrd(str2,1)
  say 'parsed: xlon=' xlon ' ylat=' ylat ' tim=' tim ' titl=' titl
  if (xlon='' | ylat = '' | tim = '')
   code=2
  endif
  if (code !=2)
    if(xlon < 0 )
      xlon = xlon + 360.0
    endif
*********** test for track crossing 0 (360) begins
          if (tim = 0.0)
            xlon_first = xlon
          endif
          if( xlon_first - xlon > 300 )
            xlon = xlon + 360.0
          endif
*********** test ends
    ylat = (ylat - lats)/ yld * yd
    xlon = (xlon - lonw)/ xld * xd
    ylat = ylat + ylim1
    xlon = xlon + xlim1
    siz=0.3
    'set line 'color.2' 1 ' thick.2
          if (firstpoint=0)
            'draw line 'xold' 'yold' 'xlon' 'ylat
          else
            firstpoint=0
          endif

    if (flag > 0)
    'draw line 'xold' 'yold' 'xlon' 'ylat
    'draw mark 'mark.2' 'xold' 'yold' .08'
    'draw mark 'mark.2' 'xlon' 'ylat' .08'
    endif
    xold = xlon
    yold = ylat
    flag = flag + 1
  endif
endwhile
*************************************************************
endif
*************************************************************
dfile.3=3
if(dfile.3 = 3)
*
numcaps=numcaps+1
cap.3 = 'HWRF: 2013 Oper.'
color.3=2
mark.3=4
line.3=1
thick.3=6
*************************************************************
*  open top box storm track file
*************************************************************
name3 = 'HWRF.track'
*
flag=0
*
******************** Setup dims
'set gxout line'
str1 = read(name3)
code = subwrd(str1,1)
titl = sublin(str1,2)
say 'Title = 'titl
******************** Pull storms coords
firstpoint=1
while (code!=2 & code!='')
  str1 = read(name3)
  code = subwrd(str1,1)
  str2 = sublin(str1,2)
 say name3 ': "' str2 '"'
  ylat = subwrd(str2,3)
  xlon = subwrd(str2,2)
  tim  = subwrd(str2,1)
  say 'parsed: xlon=' xlon ' ylat=' ylat ' tim=' tim ' titl=' titl
  if (xlon='' | ylat = '' | tim = '')
   code=2
  endif
  if (code !=2)
    if(xlon < 0 )
      xlon = xlon + 360.0
    endif
*********** test for track crossing 0 (360) begins
          if (tim = 0.0)
            xlon_first = xlon
          endif
          if( xlon_first - xlon > 300 )
            xlon = xlon + 360.0
          endif
*********** test ends
    ylat = (ylat - lats)/ yld * yd
    xlon = (xlon - lonw)/ xld * xd
    ylat = ylat + ylim1
    xlon = xlon + xlim1
    siz=0.3
    'set line 'color.3' 1 ' thick.3
          if (firstpoint=0)
            'draw line 'xold' 'yold' 'xlon' 'ylat
          else
            firstpoint=0
          endif

    if (flag > 0)
    'draw line 'xold' 'yold' 'xlon' 'ylat
    'draw mark 'mark.3' 'xold' 'yold' .08'
    'draw mark 'mark.3' 'xlon' 'ylat' .08'
    endif
    xold = xlon
    yold = ylat
    flag = flag + 1
  endif
endwhile
*************************************************************
endif
*************************************************************
dfile.4=4
if(dfile.4 = 4)
*
numcaps=numcaps+1
cap.4 = 'GFDN: GFDN Oper.' 
color.4=3
mark.4=5
line.4=1
thick.4=6
*************************************************************
*  open top box storm track file
*************************************************************
name4 = 'GFDN.track'
*
flag=0
*
******************** Setup dims
'set gxout line'
str1 = read(name4)
code = subwrd(str1,1)
titl = sublin(str1,2)
say 'Title = 'titl
******************** Pull storms coords
firstpoint=1
while (code!=2 & code!='')
  str1 = read(name4)
  code = subwrd(str1,1)
  str2 = sublin(str1,2)
 say name4 ': "' str2 '"'
  ylat = subwrd(str2,3)
  xlon = subwrd(str2,2)
  tim  = subwrd(str2,1)
  say 'parsed: xlon=' xlon ' ylat=' ylat ' tim=' tim ' titl=' titl
  if (xlon='' | ylat = '' | tim = '')
   code=2
  endif
  if (code !=2)
    if(xlon < 0 )
      xlon = xlon + 360.0
    endif
*********** test for track crossing 0 (360) begins
          if (tim = 0.0)
            xlon_first = xlon
          endif
          if( xlon_first - xlon > 300 )
            xlon = xlon + 360.0
          endif
*********** test ends
    ylat = (ylat - lats)/ yld * yd
    xlon = (xlon - lonw)/ xld * xd
    ylat = ylat + ylim1
    xlon = xlon + xlim1
    siz=0.3
    'set line 'color.4' 1 ' thick.4
          if (firstpoint=0)
            'draw line 'xold' 'yold' 'xlon' 'ylat
          else
            firstpoint=0
          endif

    if (flag > 0)
    'draw line 'xold' 'yold' 'xlon' 'ylat
    'draw mark 'mark.4' 'xold' 'yold' .08'
    'draw mark 'mark.4' 'xlon' 'ylat' .08'
    endif
    xold = xlon
    yold = ylat
    flag = flag + 1
  endif
endwhile
*************************************************************
endif
*************************************************************
dfile.5=5
if(dfile.5 = 5)
*
numcaps=numcaps+1
cap.5 = 'AVNO: Oper. GFS' 
color.5=4
mark.5=6
line.5=1
thick.5=6
*************************************************************
*  open top box storm track file
*************************************************************
name5 = 'AVNO.track'
*
flag=0
*
******************** Setup dims
'set gxout line'
str1 = read(name5)
code = subwrd(str1,1)
titl = sublin(str1,2)
say 'Title = 'titl
******************** Pull storms coords
firstpoint=1
while (code!=2 & code!='')
  str1 = read(name5)
  code = subwrd(str1,1)
  str2 = sublin(str1,2)
 say name5 ': "' str2 '"'
  ylat = subwrd(str2,3)
  xlon = subwrd(str2,2)
  tim  = subwrd(str2,1)
  say 'parsed: xlon=' xlon ' ylat=' ylat ' tim=' tim ' titl=' titl
  if (xlon='' | ylat = '' | tim = '')
   code=2
  endif
  if (code !=2)
    if(xlon < 0 )
      xlon = xlon + 360.0
    endif
*********** test for track crossing 0 (360) begins
          if (tim = 0.0)
            xlon_first = xlon
          endif
          if( xlon_first - xlon > 300 )
            xlon = xlon + 360.0
          endif
*********** test ends
    ylat = (ylat - lats)/ yld * yd
    xlon = (xlon - lonw)/ xld * xd
    ylat = ylat + ylim1
    xlon = xlon + xlim1
    siz=0.3
    'set line 'color.5' 1 ' thick.5
          if (firstpoint=0)
            'draw line 'xold' 'yold' 'xlon' 'ylat
          else
            firstpoint=0
          endif

    if (flag > 0)
    'draw line 'xold' 'yold' 'xlon' 'ylat
    'draw mark 'mark.5' 'xold' 'yold' .08'
    'draw mark 'mark.5' 'xlon' 'ylat' .08'
    endif
    xold = xlon
    yold = ylat
    flag = flag + 1
  endif
endwhile
*************************************************************
endif
*************************************************************
dfile.6=6
if(dfile.6 = 6)
*
numcaps=numcaps+1
cap.6 = 'COTC: COAMP Forecast' 
color.6=7
mark.6=7
line.6=1
thick.6=9
*************************************************************
*  open top box storm track file
*************************************************************
name6 = 'COTC.track'        
*
flag=0
*
******************** Setup dims
'set gxout line'
str1 = read(name6)
code = subwrd(str1,1) 
titl = sublin(str1,2) 
say 'Title = 'titl
******************** Pull storms coords
firstpoint=1
while (code!=2 & code!='')
  str1 = read(name6)
  code = subwrd(str1,1)
  str2 = sublin(str1,2)
 say name6 ': "' str2 '"' 
  ylat = subwrd(str2,3)
  xlon = subwrd(str2,2)
  tim  = subwrd(str2,1)
  say 'parsed: xlon=' xlon ' ylat=' ylat ' tim=' tim ' titl=' titl
  if (xlon='' | ylat = '' | tim = '')
   code=2
  endif
  if (code !=2)
    if(xlon < 0 )
      xlon = xlon + 360.0
    endif
*********** test for track crossing 0 (360) begins
          if (tim = 0.0)
            xlon_first = xlon
          endif
          if( xlon_first - xlon > 300 )
            xlon = xlon + 360.0
          endif
*********** test ends
    ylat = (ylat - lats)/ yld * yd
    xlon = (xlon - lonw)/ xld * xd
    ylat = ylat + ylim1
    xlon = xlon + xlim1
    siz=0.3
    'set line 'color.6' 1 ' thick.6
          if (firstpoint=0)
            'draw line 'xold' 'yold' 'xlon' 'ylat
          else
            firstpoint=0
          endif

    if (flag > 0)
    'draw line 'xold' 'yold' 'xlon' 'ylat
    'draw mark 'mark.6' 'xold' 'yold' .08'
    'draw mark 'mark.6' 'xlon' 'ylat' .08'
    if (flag = 1)
    'draw wxsym 40 'xold' 'yold' 'siz' 1 6'
    endif
    endif
    xold = xlon
    yold = ylat
    flag = flag + 1
  endif
endwhile
*************************************************************
endif
*************************************************************
dfile.7=7
if(dfile.7 = 7)
*
numcaps=numcaps+1
cap.7 = 'BEST: Best Track'
color.7=1
mark.7 = wxsym 40
line.7=1
thick.7=12
*************************************************************
*  open top box storm track file
*************************************************************
name6 = 'BEST.track'
*
flag=0
*
******************** Setup dims
'set gxout line'
str1 = read(name6)
code = subwrd(str1,1)
titl = sublin(str1,2)
say 'Title = 'titl
******************** Pull storms coords
firstpoint=1
while (code!=2 & code!='')
  str1 = read(name6)
  code = subwrd(str1,1)
  str2 = sublin(str1,2)
 say name6 ': "' str2 '"'
  ylat = subwrd(str2,3)
  xlon = subwrd(str2,2)
  tim  = subwrd(str2,1)
  say 'parsed: xlon=' xlon ' ylat=' ylat ' tim=' tim ' titl=' titl
  if (xlon='' | ylat = '' | tim = '')
   code=2
  endif
  if (code !=2)
    if(xlon < 0 )
      xlon = xlon + 360.0
    endif
*********** test for track crossing 0 (360) begins
          if (tim = 0.0)
            xlon_first = xlon
          endif
          if( xlon_first - xlon > 300 )
            xlon = xlon + 360.0
          endif
*********** test ends
    ylat = (ylat - lats)/ yld * yd
    xlon = (xlon - lonw)/ xld * xd
    ylat = ylat + ylim1
    xlon = xlon + xlim1
    siz=0.3
    'set line 'color.7' 1 ' thick.7
          if (firstpoint=0)
            'draw line 'xold' 'yold' 'xlon' 'ylat
          else
            firstpoint=0
          endif

    if (flag > 0)
    'draw line 'xold' 'yold' 'xlon' 'ylat
    'draw wxsym 40 'xold' 'yold' 'siz' 1 7'
    'draw wxsym 40 'xlon' 'ylat' 'siz' 1 7'
    if (flag = 1)
    'draw wxsym 40 'xold' 'yold' 'siz' 1 7' 
    endif
    endif
    xold = xlon
    yold = ylat
    flag = flag + 1
  endif
endwhile
*************************************************************
endif
*************************************************************
dfile.8=8
if(dfile.8 = 8)
*
numcaps=numcaps+1
cap.8 = 'CTCX: COAMPTC-GFS' 
color.8=10
mark.8=9
line.8=1
thick.8=7
*************************************************************
*  open top box storm track file
*************************************************************
name1 = 'CTCX.track'        
*
flag=0
*
******************** Setup dims
'set gxout line'
str1 = read(name1)
code = subwrd(str1,1) 
titl = sublin(str1,2) 
say 'Title = 'titl
******************** Pull storms coords
firstpoint=1
while (code!=2 & code!='')
  str1 = read(name1)
  code = subwrd(str1,1)
  str2 = sublin(str1,2)
 say name1 ': "' str2 '"' 
  ylat = subwrd(str2,3)
  xlon = subwrd(str2,2)
  tim  = subwrd(str2,1)
  say 'parsed: xlon=' xlon ' ylat=' ylat ' tim=' tim ' titl=' titl
  if (xlon='' | ylat = '' | tim = '')
   code=2
  endif
  if (code !=2)
    if(xlon < 0 )
      xlon = xlon + 360.0
    endif
*********** test for track crossing 0 (360) begins
          if (tim = 0.0)
            xlon_first = xlon
          endif
          if( xlon_first - xlon > 300 )
            xlon = xlon + 360.0
          endif
*********** test ends
    ylat = (ylat - lats)/ yld * yd
    xlon = (xlon - lonw)/ xld * xd
    ylat = ylat + ylim1
    xlon = xlon + xlim1
    siz=0.3
    'set line 'color.8' 1 ' thick.8
          if (firstpoint=0)
            'draw line 'xold' 'yold' 'xlon' 'ylat
          else
            firstpoint=0
          endif

    if (flag > 0)
    'draw line 'xold' 'yold' 'xlon' 'ylat
    'draw mark 'mark.8' 'xold' 'yold' .08'
    'draw mark 'mark.8' 'xlon' 'ylat' .08'
    if (flag = 1)
    'draw wxsym 40 'xold' 'yold' 'siz' 1 6'
    endif
    endif
    xold = xlon
    yold = ylat
    flag = flag + 1
  endif
endwhile
*************************************************************
endif
*************************************************************
*        Now draw the legend
*************************************************************
*
startx=1.0
starty=7.3
lineh=0.35
markpad=0.08
markw=0.25
charw=0.11
charh=0.13
maxlegy=2
rightpad=0
count=8

legx=1
legy=1
legstarty=starty
legstartx=startx
maxwidcol=0
pastwid=0
FN=1
while(FN <= count)
    xloc=legstartx+pastwid
    yloc=legstarty-(legy-1)*lineh

    'set line ' color.FN ' ' line.FN ' ' thick.FN
    'draw line ' markpad+xloc ' ' yloc ' ' xloc+markw+markpad ' ' yloc
    'draw mark ' mark.FN ' ' markpad+xloc+markw/2 ' ' yloc ' ' 0.08

    'set string ' color.FN ' l'
    'set strsiz ' charw ' ' charh
    'draw string ' xloc+markw+2*markpad ' ' yloc ' ' cap.FN

    legwid=3*markpad+markw+charw*strlen(cap.FN)

    if(legwid>maxwidcol)
      maxwidcol=legwid
    endif

    legy=legy+1
    if(legy > maxlegy)
      legy=1
      legx=legx+1
      pastwid=pastwid+maxwidcol+rightpad
      maxwidcol=0
    endif
  FN=FN+1
endwhile
*************************************************************
* Draw the footer
*************************************************************
cstr = 'NCEP Hurricane Forecast Project'
'set string 1 br 4 0'
'set strsiz 0.09 0.08' 
*'draw string 10.97 0.05 'cstr
*************************************************************
'printim 'outfile' png x1024 y768' 
*************************************************************
'quit'
