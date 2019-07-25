*
*  Script to plot a colorbar
*
*  The script will assume a colorbar is wanted even if there is 
*  not room -- it will plot on the side or the bottom if there is
*  room in either place, otherwise it will plot along the bottom and
*  overlay labels there if any.  This can be dealt with via 
*  the 'set parea' command.  In version 2 the default parea will
*  be changed, but we want to guarantee upward compatibility in
*  sub-releases.
*
*
*	modifications by mike fiorino 940614
*
*	- the extreme colors are plotted as triangles
*	- the colors are boxed in white
*	- input arguments in during a run execution:
* 
*	run cbarn sf vert xmid ymid
*
*	sf   - scale the whole bar 1.0 = original 0.5 half the size, etc.
*	vert - 0 FORCES a horizontal bar = 1 a vertical bar
*	xmid - the x position on the virtual page the center the bar
*	ymid - the x position on the virtual page the center the bar
*
*	if vert,xmid,ymid are not specified, they are selected
*	as in the original algorithm
*  

function cbarn (args)

drawbox=0
strinc=8

setlbo=1
lbo.1=40
lbo.2=30
lbo.3=20
lbo.4=10
lbo.5=0
lbo.6=-10
lbo.7=-20
lbo.8=-30
lbo.9=-40
lbo.10=-50
lbo.11=-60
lbo.12=-70
lbo.13=-80
lbo.14=-90
lbo.15=-100

nlbo=15

dlbo.1=0
dlbo.2=0
dlbo.3=0
dlbo.4=0
dlbo.5=0
dlbo.5=0
dlbo.6=0
dlbo.7=0
dlbo.8=0
dlbo.9=0
dlbo.10=0
dlbo.11=0
dlbo.12=0
dlbo.13=0
dlbo.14=0
dlbo.15=0

j=1
while(j<=nlbo)
  clbo=math_format('%4.0f',lbo.j)
  lbo.j=clbo
  j=j+1
endwhile

i=1
sf=subwrd(args,i) ; i=i+1 
vert=subwrd(args,i) ; i=i+1 
#xmid=subwrd(args,i) ; i=i+1 
#ymid=subwrd(args,i) ; i=i+1 
sfstr=subwrd(args,i) ; i=i+1 

if(vert='');vert='def';endif
if(xmid='' | xmid = 'xmid')  ; xmid='def';endif
if(ymid=' '| ymid = 'ymid' ) ; ymid='def';endif
if(sf='');sf=1.0;endif
if(sfstr='');sfstr=1.0;endif

labrot=-45
labrot=0
###########print 'ASDFASDF vert 'vert' 'xmid' 'ymid' 'sf' 'sfstr

*
*  Check shading information
*
  'query shades'
  shdinfo = result
  if (subwrd(shdinfo,1)='None') 
    say 'Cannot plot color bar: No shading information'
    return
  endif

* 
*  Get plot size info
*
  'query gxinfo'
  rec2 = sublin(result,2)
  rec3 = sublin(result,3)
  rec4 = sublin(result,4)
  xsiz = subwrd(rec2,4)
  ysiz = subwrd(rec2,6)
  ylo = subwrd(rec4,4)
  xhi = subwrd(rec3,6)
  xd = xsiz - xhi

  ylolim=0.6*sf
  xdlim1=1.0*sf
  xdlim2=1.5*sf  
  barsf=0.8*sf
  yoffset=0.2*sf
  stroff=0.05*sf
  strxsiz=0.12*sf*sfstr
  strysiz=0.13*sf*sfstr
*
*  Decide if horizontal or vertical color bar
*  and set up constants.
*
  if (ylo<ylolim & xd<xdlim1) 
    say "Not enough room in plot for a colorbar"
    return
  endif
  cnum = subwrd(shdinfo,5)
*
*	logic for setting the bar orientation with user overides
*
  if (ylo<ylolim | xd>xdlim1)
    vchk = 1
    if(vert = 0) ; vchk = 0 ; endif
  else
    vchk = 0
    if(vert = 1) ; vchk = 1 ; endif
  endif
*
*	vertical bar
*

  if (vchk = 1 )

    if(xmid='def') ; xmid = xhi+xd/2 ; endif
    xwid = 0.2*sf
    ywid = 0.5*sf
    
    xl = xmid-xwid/2
    xr = xl + xwid
    if (ywid*cnum > ysiz*barsf) 
      ywid = ysiz*barsf/cnum
    endif
    if(ymid='def') ; ymid = ysiz/2 ; endif
    yb = ymid - ywid*cnum/2
    'set string 1 l 5 0'
    vert = 1

  else

*
*	horizontal bar
*

    ywid = 0.4
    xwid = 0.8

    if(ymid = 'def') ; ymid = ylo/2-ywid/2 ; endif
    yt = ymid + yoffset
    yb = ymid
    if(xmid = 'def') ; xmid = xsiz/2 ; endif
    if (xwid*cnum > xsiz*barsf)
      xwid = xsiz*barsf/cnum
    endif
    xl = xmid - xwid*cnum/2
    'set string 1 tc 5 0'
    vert = 0
  endif


*
*  Plot colorbar
*


  'set strsiz 'strxsiz' 'strysiz
  num = 0
  while (num<cnum) 
    rec = sublin(shdinfo,num+2)
    col = subwrd(rec,1)
    hi = subwrd(rec,3)
    if (vert) 
      yt = yb + ywid
    else 
      xr = xl + xwid
    endif

    if(num!=0 & num!= cnum-1)
    'set line 'col
    'draw recf 'xl' 'yb' 'xr' 'yt

if(drawbox=1)
    'set line 1 1 5'
    'draw rec 'xl' 'yb' 'xr' 'yt
endif

    hi=math_format('%4.0f',hi)
    dohi=0
    if( ( setlbo = 0) & ( (num<cnum-1 & math_mod(num,strinc) = 0) & (num != 0 & strinc > 1) ) | num =0 ) ; dohi=1 ; endif
    if( setlbo = 1)
      j=1
      while(j<=nlbo)
        if(hi=lbo.j)
           if(dlbo.j = 0) ; dlbo.j=1 ; dohi=1 ; endif
        endif
        j=j+1
     endwhile

    endif
   

    if(dohi) 
      if (vert) 
        xp=xr+stroff
        'draw string 'xp' 'yt' 'hi
      else
        yp=yb-stroff
        'draw string 'xr' 'yp' 'hi
      endif

    endif
    endif

    if(num = 0 )

      if(vert = 1)

        xm=(xl+xr)*0.5


        'set line 'col
        'draw polyf 'xl' 'yt' 'xm' 'yb' 'xr' 'yt' 'xl' 'yt

if(drawbox=1)
        'set line 1 1 5'
        'draw line 'xl' 'yt' 'xm' 'yb
        'draw line 'xm' 'yb' 'xr' 'yt
        'draw line 'xr' 'yt' 'xl' 'yt
 endif

      else

        ym=(yb+yt)*0.5


        'set line 'col
        'draw polyf 'xl' 'ym' 'xr' 'yb' 'xr' 'yt' 'xl' 'ym

if(drawbox=1)
        'set line 1 1 5'
        'draw line 'xl' 'ym' 'xr' 'yb
        'draw line 'xr' 'yb' 'xr' 'yt
        'draw line 'xr' 'yt' 'xl' 'ym
endif

      endif

     if( (hi != '<') & ( setlbo = 0) ) 
        hi=math_format('%4.0f',hi)
        if (vert)
          xp=xr+stroff 
          'draw string 'xp' 'yt' 'hi
        else
           yp=yb-stroff
          'draw string 'xr' 'yp' 'hi
        endif
      endif
    endif


    if(num = cnum-1 )

      if( vert = 1)

        'set line 'col
        'draw polyf 'xl' 'yb' 'xm' 'yt' 'xr' 'yb' 'xl' 'yb

if(drawbox=1)
        'set line 1 1 5'
        'draw line 'xl' 'yb' 'xm' 'yt
        'draw line 'xm' 'yt' 'xr' 'yb
        'draw line 'xr' 'yb' 'xl' 'yb
endif

      else

        'set line 'col
        'draw polyf 'xr' 'ym' 'xl' 'yb' 'xl' 'yt' 'xr' 'ym

if(drawbox=1)
        'set line 1 1 5'
        'draw line 'xr' 'ym' 'xl' 'yb
        'draw line 'xl' 'yb' 'xl' 'yt
        'draw line 'xl' 'yt' 'xr' 'ym
endif
        
      endif


       if( (hi != '>') & ( setlbo = 0) )
          hi=math_format('%4.0f',hi)
          'set string 1 l 5 'labrot
          if (vert) 
            xp=xr+stroff
            'draw string 'xp' 'yt' 'hi
          else
            yp=yb-stroff
            'draw string 'xr' 'yp' 'hi
          endif
        endif


    endif


    num = num + 1
    if (vert); yb = yt;
    else; xl = xr; endif;
  endwhile

'set string 1 c 5 0'

return

