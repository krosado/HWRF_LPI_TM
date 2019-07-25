**************************************************************
*  GraDS script - hwrf_gr_plot_pmin_HR12.gs 
**************************************************************
'set display color white'
numcaps=0

'open dummy.ctl' 

time0='YYYYMMDDHH'
outfile=Pmin.png
'set display color white'
'set vpage off'
'set parea 0.9 10.7 0.65 6.7'
'set xlopts 1 7 0.14'
'set ylopts 1 7 0.14'
'set font 0'
'set map 1 1 9'
'set mpdset hires'
'set grid on'
'set grads off'
*'set timelab on'

infile=all_pmin.tracks

**************************************************************
* first, loop thru data to get date and data ranges
* set minval and maxval to first val
**************************************************************
  res = read(infile)
  rc = sublin(res,1)
  if (rc>0); say 'ERROR READING 'infile'.  Exiting...';exit; endif
  rec = sublin(res,2)
  hour = subwrd(rec,1)
  start_hour=hour
  val = subwrd(rec,4)
  minval=val
  maxval=val
  while (1)
    res = read(infile)
    rc = sublin(res,1)
    if (rc>0); break; endif
    rec = sublin(res,2)
    hour = subwrd(rec,1)
    val = subwrd(rec,4)
    if(val<minval);minval=val;endif
    if(val>maxval);maxval=val;endif
*    say date' 'val
  endwhile
  say 'min and max are 'minval' 'maxval
  end_hour=hour
  say 'start and end hours are 'start_hour' 'end_hour

**************************************************************
* this needs work... setting y-axis range
**************************************************************
  range=maxval-minval
  say 'range is 'range
* pad=math_int(range/10)
* if(pad<1);pad=1;endif
  pad=10
  ymin=minval-pad
  ymax=maxval+pad
 say 'ymin=' ymin 'ymax=' ymax
  'set x 1'
  'set y 1'
  'set vrange 'ymin' 'ymax
  'set time IDATE ENDDATE'
  'set xaxis 00 126 12'
*  using internal variable lat, set up plot area
*  setting style and mark for that line to 0 so that it won't show up
*'set timelab on'
  'set cstyle 0'
  'set cmark 0'
  'd lat'
  say 'ymax' ymax 'ymin 'ymin
  'd lat*0-'ymax'+'ymin
'draw ylab Intensity (MSLP, hPa)'
* now close the input file in order to process each data point
  res = close(infile)
**************************************************************
*        Now draw the title
**************************************************************
'set string 1 c 6'
'set strsiz 0.24 0.24'
'draw string 5.5 8.15 MC_MODEL 2013 Real time: TC Intensity Pmin ' 
'set strsiz 0.18 0.18'
subtitle='Storm: STORMNAME (STID) valid YYYYMMDDHH '
'draw string 5.5 7.7 ' subtitle
'set strsiz 0.12'
'draw string 5.5 0.25 Forecast time (hr)'
**************************************************************
dfile.1=1
if(dfile.1 = 1)
*
numcaps=numcaps+1
cap.1 = 'H213: FY13 final baseline'
color.1=2
mark.1=2
line.1=1
thick.1=7
**************************************************************
*  open top box storm track file
**************************************************************
name1 = 'H213.intensity'        
*
'set gxout line'
str1 = read(name1)
code = subwrd(str1,1) 
titl = sublin(str1,2) 
say 'Title = 'titl
*************** Pull storms coords
flag=0
while (1)
    res = read(name1)
    rc = sublin(res,1)
    if (rc>0); break; endif
    rec = sublin(res,2)
    hour = subwrd(rec,1)
    val = subwrd(rec,4)
    say name1': hour='hour' Vmax='val 
*************** Convert world coordinates to xy plot coordinates
    'query w2xy 'hour' 'val
    res=result
    say res
    xx=subwrd(res,3)
    yy=subwrd(res,6)
    say xx' 'yy
**************** Connect consecutive points with a line`
  if(flag >0)
   'set line ' color.1 '  1 ' thick.1  
   'draw mark ' mark.1 ' 'xold' 'yold' 0.1'
   'draw mark ' mark.1 ' 'xx' 'yy' 0.1'
   'draw line 'xold' 'yold' 'xx' 'yy
   endif
    xold=xx
    yold=yy
    flag=flag+1
  endwhile
**************************************************************
endif
**************************************************************
dfile.2=2
if(dfile.2 = 2)
*
numcaps=numcaps+1
cap.2 = 'GFDN: GFDN model'
color.2=5
mark.2=3
line.2=1
thick.2=7
**************************************************************
*  open top box storm track file
**************************************************************
name2 = 'GFDN.intensity'
*
'set gxout line'
str1 = read(name2)
code = subwrd(str1,1)
titl = sublin(str1,2)
say 'Title = 'titl
*************** Pull storms coords
flag=0
while (1)
    res = read(name2)
    rc = sublin(res,1)
    if (rc>0); break; endif
    rec = sublin(res,2)
    hour = subwrd(rec,1)
    val = subwrd(rec,4)
    say name2': hour='hour' Vmax='val
*************** Convert world coordinates to xy plot coordinates
    'query w2xy 'hour' 'val
    res=result
    say res
    xx=subwrd(res,3)
    yy=subwrd(res,6)
    say xx' 'yy
**************** Connect consecutive points with a line`
  if(flag >0)
   'set line ' color.2 '  1 ' thick.2
   'draw mark ' mark.2 ' 'xold' 'yold' 0.1'
   'draw mark ' mark.2 ' 'xx' 'yy' 0.1'
   'draw line 'xold' 'yold' 'xx' 'yy
  endif
    xold=xx
    yold=yy
    flag=flag+1
  endwhile
**************************************************************
endif
**************************************************************
dfile.3=3
if(dfile.3 = 3)
*
numcaps=numcaps+1
cap.3 = 'HWRF: 2013 Oper.'
color.3=9
mark.3=4
line.3=1
thick.3=7
**************************************************************
*  open top box storm track file
**************************************************************
name3 = 'HWRF.intensity'
*
'set gxout line'
str1 = read(name3)
code = subwrd(str1,1)
titl = sublin(str1,2)
say 'Title = 'titl
*************** Pull storms coords
flag=0
while (1)
    res = read(name3)
    rc = sublin(res,1)
    if (rc>0); break; endif
    rec = sublin(res,2)
    hour = subwrd(rec,1)
    val = subwrd(rec,4)
    say name3': hour='hour' Vmax='val
*************** Convert world coordinates to xy plot coordinates
    'query w2xy 'hour' 'val
    res=result
    say res
    xx=subwrd(res,3)
    yy=subwrd(res,6)
    say xx' 'yy
**************** Connect consecutive points with a line`
  if(flag >0)
   'set line ' color.3 '  1 ' thick.3
   'draw mark ' mark.3 ' 'xold' 'yold' 0.1'
   'draw mark ' mark.3 ' 'xx' 'yy' 0.1'
   'draw line 'xold' 'yold' 'xx' 'yy
  endif
    xold=xx
    yold=yy
    flag=flag+1
  endwhile
**************************************************************
endif
**************************************************************
dfile.4=4
if(dfile.4 = 4)
*
numcaps=numcaps+1
cap.4 = 'GFDL: GFDL Oper.'
color.4=3
mark.4=5
line.4=1
thick.4=7
**************************************************************
*  open top box storm track file
**************************************************************
name4 = 'GFDL.intensity'
*
'set gxout line'
str1 = read(name4)
code = subwrd(str1,1)
titl = sublin(str1,2)
say 'Title = 'titl
*************** Pull storms coords
flag=0
while (1)
    res = read(name4)
    rc = sublin(res,1)
    if (rc>0); break; endif
    rec = sublin(res,2)
    hour = subwrd(rec,1)
    val = subwrd(rec,4)
    say name4': hour='hour' Vmax='val
*************** Convert world coordinates to xy plot coordinates
    'query w2xy 'hour' 'val
    res=result
    say res
    xx=subwrd(res,3)
    yy=subwrd(res,6)
    say xx' 'yy
**************** Connect consecutive points with a line`
  if(flag >0)
   'set line ' color.4 '  1 ' thick.4
   'draw mark ' mark.4 ' 'xold' 'yold' 0.1'
   'draw mark ' mark.4 ' 'xx' 'yy' 0.1'
   'draw line 'xold' 'yold' 'xx' 'yy
  endif
    xold=xx
    yold=yy
    flag=flag+1
  endwhile
**************************************************************
endif
**************************************************************
dfile.5=5
if(dfile.5 = 5)
*
numcaps=numcaps+1
cap.5 = 'SHF5: SHIFOR 5-day'
color.5=7
mark.5=1
line.5=1
thick.5=7
**************************************************************
*  open top box storm track file
**************************************************************
name5 = 'SHF5.intensity'
*
'set gxout line'
str1 = read(name5)
code = subwrd(str1,1)
titl = sublin(str1,2)
say 'Title = 'titl
*************** Pull storms coords
flag=0
while (1)
    res = read(name5)
    rc = sublin(res,1)
    if (rc>0); break; endif
    rec = sublin(res,2)
    hour = subwrd(rec,1)
    val = subwrd(rec,4)
    say name5': hour='hour' Vmax='val
*************** Convert world coordinates to xy plot coordinates
    'query w2xy 'hour' 'val
    res=result
    say res
    xx=subwrd(res,3)
    yy=subwrd(res,6)
    say xx' 'yy
**************** Connect consecutive points with a line`
  if(flag >0)
   'set line ' color.5 '  1 ' thick.5
   'draw mark ' mark.5 ' 'xold' 'yold' 0.1'
   'draw mark ' mark.5 ' 'xx' 'yy' 0.1'
   'draw line 'xold' 'yold' 'xx' 'yy
  endif
    xold=xx
    yold=yy
    flag=flag+1
  endwhile
**************************************************************
endif
**************************************************************
dfile.6=6
if(dfile.6 = 6)
*
numcaps=numcaps+1
cap.6 = 'BEST: Best Track'
color.6=1
mark.6=wxsym 40
line.6=1
thick.6=12
**************************************************************
*  open top box storm track file
**************************************************************
name6 = 'BEST.intensity'
*
'set gxout line'
str1 = read(name6)
code = subwrd(str1,1)
titl = sublin(str1,2)
say 'Title = 'titl
*************** Pull storms coords
flag=0
while (1)
    res = read(name6)
    rc = sublin(res,1)
    if (rc>0); break; endif
    rec = sublin(res,2)
    hour = subwrd(rec,1)
    val = subwrd(rec,4)
    say name6': hour='hour' Vmax='val
*************** Convert world coordinates to xy plot coordinates
    'query w2xy 'hour' 'val
    res=result
    say res
    xx=subwrd(res,3)
    yy=subwrd(res,6)
    say xx' 'yy
**************** Connect consecutive points with a line`
  if(flag >0)
   'set line ' color.6 '  1 ' thick.6
   'draw wxsym 40 'xold' 'yold' 0.3 1 7'
   'draw wxsym 40 'xx' 'yy' 0.3 1 7'
   'draw line 'xold' 'yold' 'xx' 'yy
  endif
    xold=xx
    yold=yy
    flag=flag+1
  endwhile
**************************************************************
endif
**************************************************************
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
count=6

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
**************************************************************
'printim 'outfile' png x1024 y768' 
**************************************************************

'quit'

**************************************************************

function cycle_to_grads(cycle)

*say 'CYCLE TO GRADS: cycle="'cycle'"'

year=substr(cycle,1,4)+0
month=substr(cycle,5,2)+0
day=substr(cycle,7,2)+0
hour=substr(cycle,9,2)+0

*say 'CYCLE TO GRADS: year="'year'" month="'month'" day="'day'" hour="'hour'"'

out=zeropad(hour,2)%'z'
out=out % zeropad(day,2)
out=out % string_month(month)
out=out % zeropad(year,4)

*say 'CYCLE TO GRADS RESULT: "'out'"'

return out

* END FUNCTION cycle_to_grads
**************************************************************
function cycle_add(cycle,dhour)

year=substr(cycle,1,4)
month=substr(cycle,5,2)
day=substr(cycle,7,2)
hour=substr(cycle,9,2)

*say 'CYCLE_ADD cycle="'cycle'" dhour="'dhour'"'
*say 'BEFORE LOOP: year="'year'" month="'month'" day="'day'" hour="'hour'"'

hour=hour+dhour
if(hour>0)
  daydiff=math_int(hour/24)
else
  daydiff=math_int(hour/24)
endif
hour=hour-daydiff*24
day=day+daydiff
baddate=1
while(baddate)
*  say 'LOOP TOP: year="'year'" month="'month'" day="'day'" hour="'hour'"'

  while(month<1)
    year=year-1
    month=month+12
  endwhile
  while(month>12)
    year=year+1
    month=month-12
  endwhile
  if(day<1)
    month=month-1
    day=day+month_length(month,year)
  endif
  ml=month_length(month,year)
  if(day>ml)
    day=day-ml
    month=month+1
  endif

  baddate=(month>=13 | month<=0 | day<1 | day>month_length(month,year))
endwhile

*say 'AFTER LOOP: year="'year'" month="'month'" day="'day'" hour="'hour'"'

out=zeropad(year,4)
out=out % zeropad(month,2)
out=out % zeropad(day,2)
out=out % zeropad(hour,2)

*say 'RESULT: "'out'"'

return out

* END FUNCTION cycle_add
**************************************************************
