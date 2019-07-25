************************************************************************
************************************************************************
* BEGIN FILE hwrf_gr_stat_utils.gs.inc

function statut_init(plotinfo,model_list,ymin,ymax,validrange)

_su_plotinfo=plotinfo

* Read in the plot information, and set _color.*, _tracktitle, _count and
* many other _whatever variables that we use farther on:
x=plot_info(_su_plotinfo)

* Uncomment the below line to dump the contents of the file we just
* read in (the filename parameter here is just so that dumpinfo can
* print out the filename):
*
****    x=dumpinfo(_su_plotinfo)

yrange=ymax-ymin
if(yrange<=0)
  say 'YMIN>=YMAX.  ABORTING.'
  'quit'
endif

if(validrange='')
  _su_minval=ymin-yrange
  _su_maxval=ymax+yrange
  say 'Setting valid data range to defaults: [ymin-yrange,ymax+yrange]=['_su_minval','_su_maxval']'
else
  nvalid=count_words(validrange)
  if(nvalid!=2)
    say 'The VALID_RANGE variable must contain exactly two values.  Aborting.'
    'quit'
  else
    val1=subwrd(validrange,1)
    val2=subwrd(validrange,2)
    if(val1<val2)
      _su_minval=val1
      _su_maxval=val2
    else
      _su_minval=val2
      _su_maxval=val1
    endif
    say 'Setting valid data range to $VALID_RANGE values: ['_su_minval','_su_maxval']'
  endif
endif

* The bulk of the initialization is performed in statut_reinit, which the
* caller can call again later on to re-initialize everything for a second
* plot.
x=statut_reinit(model_list)

* END FUNCTION statut_init

************************************************************************

function statut_reinit(model_list)

_su_mlist=model_list
'set display color white'
'clear'

* _su_pleft and friends: plot boundaries in inches, used primarily by
* set parea:
_su_pleft=0.9
_su_pright=10.7
_su_pbottom=0.9
_su_ptop=6.7

* _su_plot_bound: how far we allow lines and points to go outside of
* the plot, in inches.  This should be large enough so that the marks
* and wxsyms plotted on the plot boundary will be fully displayed.
_su_plot_bound=0.08

'set display color white'
'set vpage off' 
'set parea '_su_pleft' '_su_pright' '_su_pbottom' '_su_ptop
'set font 0'
'set map 1 1 9'
'set mpdset hires'
'set grid on' 

******************** Parse the model list
_su_nmodels=0
while(1=1)
  _su_nmodels=_su_nmodels+1
  MD=subwrd(_su_mlist,_su_nmodels)
  if(MD='') ; break ; endif
  if(! ( _dh.MD = 'YES') )
    say 'ERROR: MODEL "' MD '" DOES NOT EXIST IN PLOTINFO FILE!!!'
    'quit'
  endif

  if(MD='')
***** Out of models
    break
  endif
  say 'Model ' _su_nmodels ': ' MD
  _su_prevx.MD='*'
  _su_prevy.MD='*'
endwhile

* time0 is arbitrary -- You don't need to change time0.
_su_time0='2001103112'

_su_meta_n=0

* END FUNCTION statut_reinit

************************************************************************

function statut_clip(border)

* Sets the clipping region to the plot area (parea), plus a border
* region.  If the border is positive, then the clipping region will go
* that far (in inches) outside of the plot area.  Negative borders
* will reduce the clipping region to within the plot area by that
* much.  The plot area used is the one that was set in statut_reinit.

if(border='border')
  border=_su_plot_bound
endif
'set clip '_su_pleft-border' '_su_pright+border' '_su_pbottom-border' '_su_ptop+border
* END FUNCTION statut_clip_on

************************************************************************

function statut_noclip()
* Resets the clipping region to the entire 11x8.5 (landscape) page

'set clip 0 11 0 8.5'
* END FUNCTION statut_clip_off

************************************************************************

function statut_axes(hourtext,hourstep,ymin,ymax,xlabel,ylabel)

hours=statut_text2hrs(hourtext)
ntime=count_words(hours)

if(xlabel='')
  _su_have_xlabel=0
else
  _su_have_xlabel=1
endif

maxhour=-999999
minhour=999999

ihour=0
while(ihour<ntime)
  ihour=ihour+1
  hour=subwrd(hours,ihour)
  if(hour>maxhour) ; maxhour=hour ; endif
  if(hour<minhour) ; minhour=hour ; endif
endwhile

'set xlpos 100'
say 'set_tpaxis_impl('_su_time0','minhour','maxhour','hourstep','ymin','ymax')'
set_tpaxis_impl(_su_time0,minhour,maxhour,hourstep,ymin,ymax)

if(ylabel!='')
  'draw ylab 'ylabel
endif
_su_xlab_btm=_su_pbottom-0.2
if(xlabel!='')
  'draw xlab 'xlabel
else
    'set string 1 bl 6'
    'set strsiz 0.10 0.10'
    'draw string 0.05 '_su_xlab_btm' Fcst.Hr:'
endif

******************** Draw the X axis

ihour=0
while(ihour<ntime)
  ihour=ihour+1
  val=subwrd(hourtext,ihour)
  hour=subwrd(hours,ihour)

  xy=statut_w2xy(hour,0)
  xx=subwrd(xy,1)

  'set string 1 bc 3'
  'set strsiz 0.10 0.10'
  'draw string 'xx' '_su_xlab_btm' 'val
endwhile

* END FUNCTION statut_axes

************************************************************************

function statut_titles(title,subtitle)

'set string 1 c 6'
'set strsiz 0.24 0.24'
if(title='')
  title=_su_linedata
endif
if(subtitle!='')
  'draw string 5.5 8.15 ' title
  'set strsiz 0.18 0.18'
  'draw string 5.5 7.7 ' subtitle
else
*** No subtitle, so move title down a bit
  'draw string 5.5 7.95 ' title
endif

* END FUNCTION statut_titles

************************************************************************

function statut_decorate()

******************** DRAW THE LEGEND

draw_legend(_legstartx,7.47-_lineh/2,_su_mlist)

******************** DRAW THE WATERMARK

'set string 15 br 6 0'
'set strsiz 0.09 0.08'
footer='NCEP Hurricane Forecast Project'
'draw string 10.97 0.05 ' footer

* END FUNCTION statut_decorate

************************************************************************

function statut_w2xy(hour,y)
******* convert world coordinates to xy plot coordinates
  ttt=cycle_add(_su_time0,hour)
  gradstime=cycle_to_grads(ttt)
  'query w2xy 'gradstime' 'y
  say 'query w2xy 'gradstime' 'y
  res=result
  say res
  xx=subwrd(res,3)
  yy=subwrd(res,6)
  return(xx%' '%yy)

* END FUNCTION statut_w2xy

************************************************************************

function quit_on_ioerr(res,filename)

iostat=subwrd(res,1)
if(iostat!=0)
  say 'Unexpected IO error or end of text in "'filename'".  IO status is 'iostat
  'quit'
endif

* END FUNCTION quit_on_ioerr

************************************************************************

function skip_nonblanklns(filename)

return skip_blank_lines(filename,1)

* END FUNCTION skip_nonblanklns

************************************************************************

function skip_blank_lines(filename,skip_nonblank)

if(skip_nonblank='skip_nonblank')
  skip_nonblank=0
  line=''
else
  skip_nonblank=1
  line='a non-blank line'
endif

say 'SKIP NONBLANK='skip_nonblank

while(is_blank(line)!=skip_nonblank)
  x=read(filename)
  if(sublin(x,1)!=0)
    'quit'
  endif
  line=sublin(x,2)
  say 'IN SKIP: "'line'".'
endwhile

return(x)

* END FUNCTION skip_blank_lines

************************************************************************

function statut_meta_new(name)

_su_meta_n=_su_meta_n+1
index=_su_meta_n
_su_mn.index=name

meta_y=_su_pbottom-0.2*(index+1)

if(!_su_have_xlabel)
  'set string 1 bl 6'
  'set strsiz 0.10 0.10'
  'draw string 0.05 'meta_y' 'name
endif

return index

* END FUNCTION statut_meta_new

************************************************************************

function statut_meta_idx(name)

if(_su_meta_n=0) ; return(statut_meta_new(name)) ; endif
i=1
while(i<=_su_meta_n)
  if(_su_mn.i=name) ; return(i) ; endif
  i=i+1
endwhile
return(statut_meta_new(name))

* END FUNCTION statut_meta_idx

************************************************************************

function statut_draw_data(MD,hour,val)

if(val<_su_minval | val>_su_maxval)
  say 'NOT IN VALID DATA RANGE: MD="'MD'" hour='hour' data value='val' okay range=['_su_minval','_su_maxval']'

*** Clear the previous point so that this point will show up as a
*** break in the line on the plot.
  _su_prevx.MD='*'
  _su_prevy.MD='*'

*** Return so that we don't draw anything
  return 0
endif

say 'draw MD='MD' hour='hour' val='val
FN=_mi.MD

xy=statut_w2xy(hour,val)
xx=subwrd(xy,1)
yy=subwrd(xy,2)

'set line ' _color.FN '  1 ' _thick.FN
*******From Janna
***********
********'set line ' _color.FN  _linestyle.FN  _thick.FN

statut_clip()

if(_su_prevx.MD != '*' & _su_prevy.MD!='*')
********* There is a previous point for this model, so draw a line from it:
  say 'draw line '_su_prevx.MD' '_su_prevy.MD' 'xx' 'yy 
  'draw line '_su_prevx.MD' '_su_prevy.MD' 'xx' 'yy 
endif

******* Draw the point for this data value:
say 'draw ' _mark.FN ' 'xx' 'yy' ' _darg12hr.FN
'draw ' _mark.FN ' 'xx' 'yy' ' _darg12hr.FN
_su_prevx.MD=xx
_su_prevy.MD=yy

statut_noclip()

return 1

* END FUNCTION statut_draw_data

************************************************************************

function statut_draw_meta(name,hour,val)

index=statut_meta_idx(name)

if(_su_have_xlabel) ; return ; endif

meta_y=_su_pbottom-0.2*(index+1)

xy=statut_w2xy(hour,0)
xx=subwrd(xy,1)
yy=subwrd(xy,2)

'set string 1 bc 6'
'set strsiz 0.10 0.10'
'draw string 'xx' 'meta_y' 'val

* END FUNCTION statut_draw_meta

************************************************************************

function skip_to_line(filename,linedata)

* Reads data from the specified file until it finds a line that
* "matches" linedata.  The line is said to "match" if all words in
* linedata (from subwrd(linedata,i)) are equal to the corresponding
* word in the line of the file.  Hence:
*  
*  "A B C D" matches "    A B C D   "
*  "Q W E R" matches " Q W E R T Y U I O P"
*  "J K L P" does NOT match " JK LP" or " - J K L P" or "J K L"
*
* Returns the return value from read(filename) that provided the
* matching line.
*
* NOTE: If an end of file or other non-zero status is returned from
* read(filename) then this function will immediately return the return
* value from read(filename).  Hence, the line returned by this
* function is sublin(retval,2) if and only if sublin(retval,1)=0.

  there=0
  while(there=0)
    ret=read(filename)

    status=sublin(ret,1)
    if(status!=0)
      break
    endif

    line=sublin(ret,2)

    there=1
    iword=1
    while(subwrd(linedata,iword) != '')
      if(subwrd(linedata,iword)!=subwrd(line,iword))
        there=0
        break
      endif
      iword=iword+1
    endwhile
  endwhile

  return(ret)

* END FUNCTION skip_to_line

************************************************************************

function statut_text2hrs(hourtext)
* Converts the text in hourtext to a list of integers in the hours
* variable.  This handles text like "00 12 24 ..." as well as "00-12
* 12-24 24-36 ..." (by grabbing the first hour in each pair) and
* "00-12 00-24 00-36 ..." (by grabbing the last hour in each pair).
* Other formats may or may not work.


* First pass: get the first integer in each entry:

iword=0
hours=''
while(1)
  hour=subwrd(hourtext,iword+1)
  if(hour='') ; break ; endif
  hour=first_integer(hour)
  iword=iword+1
  if(hour='')
    say 'INVALID TEXT "'subwrd(hourtext,iword+1)'" IN HOUR LIST "'hourtext.".  ABORTING."'
    'quit'
  endif
  hours=hours' 'hour
endwhile
say 'hours="'hours'".'

if(subwrd(hours,1)=subwrd(hours,2))
*** This is probably a table of the form 00-12 00-24 00-36, etc.
*** We'll try the last hour in each entry instead.
  iword=0
  hours=''
  while(1)
    hour=subwrd(hourtext,iword+1)
    if(hour='') ; break ; endif
    hour=last_integer(hour)
    iword=iword+1
    if(hour='')
      say 'INVALID TEXT "'subwrd(hourtext,iword+1)'" IN HOUR LIST "'hourtext.".  ABORTING."'
      'quit'
    endif
    hours=hours' 'hour
  endwhile
  say 'second try: hours="'hours'".'
endif

if(subwrd(hours,1)=subwrd(hours,2))
  say 'ERROR: UNABLE TO DETERMINE HOUR LIST FROM "'hourtext'".  ABORTING."'
  'quit'
endif

return(hours)

* END FUNCTION statut_text2hrs

************************************************************************

function find_meta(line,before)

index=strstr(line,before)
if(index<0)
  return ''
endif

idx=index+strlen(before)
sub=substr0(line,idx,strlen(line)-idx+1)

fr= first_real(sub)

return(fr)

* END FUNCTION find_meta

************************************************************************

function strstr(haystack,needle)

nh=strlen(haystack)
nn=strlen(needle)

if(nn>nh)
  return(-1)
endif

i=0
lastpossible=nh-nn+1
while(i<lastpossible)
  i=i+1
  if(substr0(haystack,i,1)=substr0(needle,1,1))
    if(substr0(haystack,i,nn)=needle)
      return(i)
    endif
  endif
endwhile

* END FUNCTION strstr

************************************************************************

function last_integer(string)

  n=strlen(string)
  e=n
  while(substr0(string,e,1)=' ' & e>0) ; e=e-1 ; endwhile

  b=e
  ok=1
  while(ok & b>0)
    c='f'%substr0(string,b,1)
    if(!(c='f1'|c='f2'|c='f3'|c='f4'|c='f5'|c='f6'|c='f7'|c='f8'|c='f9'|c='f0'))
      break
    endif
    b=b-1
  endwhile

  fi=substr0(string,b+1,e-b)
  return(fi)

* END FUNCTION last_integer

************************************************************************

function substr0(string,i,n)

len=strlen(string)

if(n=0) ; return('') ; endif
if(i>len) ; return('') ; endif
return(substr(string,i,n))

* END FUNCTION substr0

************************************************************************

function first_integer(string)

  n=strlen(string)
  b=1
  while(substr0(string,b,1)=' ' & b<=n) ; b=b+1 ; endwhile

  e=b
  ok=1
  while(ok & e<=n)
    c='f'%substr0(string,e,1)
    if(!(c='f1'|c='f2'|c='f3'|c='f4'|c='f5'|c='f6'|c='f7'|c='f8'|c='f9'|c='f0'))
      break
    endif
    e=e+1
  endwhile

  say 'e-b='%(e-b)
  fi=substr0(string,b,e-b)
  return(fi)

* END FUNCTION first_integer

************************************************************************

function first_real(string)

  n=strlen(string)
  b=1
  while(substr0(string,b,1)=' ' & b<=n) ; b=b+1 ; endwhile

  e=b
  ok=1
  while(ok & e<=n)
    c='f'%substr0(string,e,1)
    if(!(c='f1'|c='f2'|c='f3'|c='f4'|c='f5'|c='f6'|c='f7'|c='f8'|c='f9'|c='f0'|c='f.'))
      break
    endif
    e=e+1
  endwhile

  fi=substr0(string,b,e-b)
  return(fi)

* END FUNCTION first_integer

************************************************************************

function is_blank(string)
* Returns true if the string has zero length or contains only spaces
* and tabs.  Returns false otherwise.

  i=0
  while(i<strlen(string))
    i=i+1
    c=substr0(string,i,1)
***** Note: there should be a tab character in the second string here:
    if(c!=' ' & c!='	')
      return(0)
    endif
  endwhile

  return(1)
* END FUNCTION is_blank

************************************************************************

function count_words(string)
* Returns the number of words in the string.  The number returned is
* the smallest integer i, greater than or equal to 0, such that
* subwrd(string,i)=''

iword=1
while(subwrd(string,iword)!='')
  iword=iword+1
endwhile

return(iword-1)

* END FUNCTION count_words

************************************************************************

function strrepl(haystack,needle,replacement)

out=''
str=haystack
nn=strlen(needle)

there=999
while(there>0)
  there=strstr(str,needle)
  if(there>0)
    out=out%substr0(str,1,there-1)%replacement
    str=substr0(str,there+nn,strlen(str)-there-nn+1)
  else
    out=out%str
    break
  endif
endwhile

return out

* END FUNCTION strrepl

************************************************************************

function word_index(list,element)

* The "list" parameter is a list of words.  This function returns the
* one-based index of the first appearance of the specified element.
*
* ARGUMENTS:
*   list -- a string that contains whitespace-separated words
*   element -- a word
* RETURNS: the smallest integer N such that N>0 and
*   subwrd(list,N)=elment.  Returns 0 if the word is not found in the
*   list.
* NOTE: We limit the set to 10000 elements to prevent accidental
*   infinite loops.

  iword=1
  while(iword<=10000)
    word=subwrd(list,iword)
    if(word='') ; return(0) ; endif

    if(element=word) ; return(iword) ; endif
    iword=iword+1
  endwhile

  return(0)
* END FUNCTION word_index

************************************************************************

function set_contains(set,element)
* Return true if the set contains the specified element
* ARGUMENTS:
*   set -- a string that contains whitespace-separated words
*   element -- a word
* RETURNS: 1 if the element (word) is equal to one of the words in the
*   set string, and 0 otherwise.
* NOTE: We limit the set to 10000 elements to prevent accidental
*   infinite loops.

if(word_index(set,element)=0)
  return 0
else
  return 1
endif

* END FUNCTION set_contains

************************************************************************

function set_intersection(set1,set2)

ret=''
word='a non-empty string'
i=0
while(word!='')
  i=i+1
  word=subwrd(set1,i)
  if(word!='')
    if(set_contains(set2,word))
      ret=ret' 'word
    endif
  endif
endwhile

return(ret)

* END FUNCTION set_intersection

************************************************************************

function fixuptitle(title,basin2,stormnum,stormyear,stormname)

  say 'FIXUP "'title'" basin2="'basin2'" stormnum="'stormnum'" stormyear="'stormyear'" stormname="'stormname'"...'

  basin1='?'
  if(basin2='AL' | basin2='al') ; basin1='L' ; endif
  if(basin2='EP' | basin2='ep') ; basin1='E' ; endif
  if(basin2='CP' | basin2='cp') ; basin1='C' ; endif

  x=title

  x=strrepl(x,'[BASIN2]',basin2)
  x=strrepl(x,'[BASIN1]',basin1)
  x=strrepl(x,'[BASIN]',basin1)

  x=strrepl(x,'[STORMID]',stormnum%basin1)
  x=strrepl(x,'[STID]',stormnum%basin1)

  x=strrepl(x,'[STORMNUM]',stormnum)
  x=strrepl(x,'[NUM]',stormnum)

  x=strrepl(x,'[STORMYEAR]',stormyear)
  x=strrepl(x,'[YEAR]',stormyear)

  x=strrepl(x,'[STORMNAME]',stormname)
  x=strrepl(x,'[NAME]',stormname)

  say 'FIXUP: "'title'" ==> "'x'".'

  return(x)

* END FUNCTION fixuptitle

* END FILE hwrf_gr_stat_utils.gs.inc
************************************************************************
************************************************************************

