* Begin plot_info.gs.inc

* This grads file contains the plot_info info function which reads in
* a file that contains plot attributes, and places those attributes in
* global variables.  This is intended assist automation of bulk
* plotting by preventing the necessity of modifying the original grads
* script.  To use this function, include the entire contents of this
* file at the bottom of your grads script and run:
*
* plot_info(filename)
*
* The files have this format:
* 
* DEFAULTS
*  thick = 3
*  line = 3
*  tracktitle = A Fine Plot Title
*  maxwtitle = A Fine Plot Title
*  minptitle = A Fine Plot Title
*
* HWRF
*  mark = mark 4
*  color = 3
*  desc = Operational HWRF
*
* BEST
*  mark = wxsym 40
*  color = 1
*  desc = Best Track
*
* The file can also contain comments -- any line beginning with a "*"
* or "#" is a comment line.
*
* The plot_info function will set global variables based on the file
* provided so that you can then use those variables to customize your
* plots.  The variables that are set describe plot attributes for each
* of the models listed in the file.  The variables thus far are listed
* below.  The "index" is a number from 1 to the total number of models
* read in.  The special "DEFAULTS" section of the file describes the
* default values for all of the models.  Any values that are not set
* get the values from DEFAULTS, except for _desc and _model (explained
* below).
*
*   _defined.index    =YES if the index is in use
*   _desc.index
*   _darg6hr.index
*   _darg12hr.index
*   _legdarg.index
*   _legmark.index
*   _line.index
*   _mark.index
*   _model.index
*   _thick.index
*   
*   _tracktitle -- plot title for track plot.
*   _maxwtitle -- plot title for max wind plot.
*   _minptitle -- plot title for min pressure plot.
*   _count -- number of models read in (max value of index)
*
* It is easy to add new variables to be parsed.  You have to add code
* in certain places.  Look for "NEW VARIABLES" in the below code for
* instructions.

function plot_info(filename)

* Set the default default values in the .0 subvariables:
_color.0 = 1
_mark.0 = "mark 1"
_legmark.0 = "mark 1"
_line.0 = 1
_darg6hr.0 = " "
_darg12hr.0 = " "
_legdarg.0 = " "
_thick.0 = 1
_desc.0 = "Results"
_tracktitle = "(You forgot the tracktitle= line.)"
_maxwtitle = "(You forgot the maxwtitle= line.)"
_minptitle = "(You forgot the minptitle= line.)"
_font = 0.20
*_charw = 0.16
*_charh = 0.18
_charw = 0.11
_charh = 0.13
_legstartx = 0.3
_lineh = 0.35
_maxlegy = 2
_degratio = 0.5
_bestborder = 6
_maxborder = 2
_markpad = 0.08
_rightpad = 0
_markw = 0.25

* NEW VARIABLES: add a line above to set the default default value of
* your new variable.  (The value that is assigned to the defaults, in
* case the user does not provide a value in the file's DEFAULTS
* record.)
*
*   _mynewvar.0 = "default value"
*
* OR if you want a variable that is not defined per model (like _tracktitle):
*
*   _mynewvar = "default value"
*

* Loop over the contents of the file:
status=0
_count=0
i=0
lineno=1
while(status=0)
  x=read(filename)
  status=sublin(x,1)
  linetext=sublin(x,2)
  linetext=strtrim(linetext)
  if(status = 0)
      mod=subwrd(linetext,1)
      op=subwrd(linetext,2)
      char1=substr(linetext,1,1)
      if(mod = '' | char1 = '*' | char1 = '#')
        continue
      endif
      if(op = "=")
********* Handle assignment lines
        if(mod = "tracktitle")
          _tracktitle=get_assign_rval(linetext)
        endif
        if(mod = "minptitle")
          _minptitle=get_assign_rval(linetext)
        endif
        if(mod = "maxwtitle")
          _maxwtitle=get_assign_rval(linetext)
        endif
        if(mod = "font")
          _font=get_assign_rval(linetext)
        endif
        if(mod = "color")
          _color.i=get_assign_rval(linetext)
        endif
        if(mod = "mark")
          _mark.i=get_assign_rval(linetext)
        endif
        if(mod = "legmark")
          _legmark.i=get_assign_rval(linetext)
        endif
        if(mod = "line")
          _line.i=get_assign_rval(linetext)
        endif
        if(mod = "thick")
          _thick.i=get_assign_rval(linetext)
        endif
        if(mod = "desc")
          _desc.i=get_assign_rval(linetext)
        endif
        if(mod = "drawargs6hr")
          _darg6hr.i=get_assign_rval(linetext)
        endif
        if(mod = "drawargs12hr")
          _darg12hr.i=get_assign_rval(linetext)
        endif
        if(mod = "legdrawargs")
          _legdarg.i=get_assign_rval(linetext)
        endif
        if(mod = "charw")
          _charw=get_assign_rval(linetext)
        endif
        if(mod = "charh")
          _charh=get_assign_rval(linetext)
        endif
        if(mod = "lineh")
          _lineh=get_assign_rval(linetext)
        endif
        if(mod = "maxlegy")
          _maxlegy=get_assign_rval(linetext)
        endif
        if(mod = "legstartx")
          _legstartx=get_assign_rval(linetext)
        endif
        if(mod = "degratio")
          _degratio=get_assign_rval(linetext)
        endif
        if(mod = "bestborder")
          _bestborder=get_assign_rval(linetext)
        endif
        if(mod = "maxborder")
          _maxborder=get_assign_rval(linetext)
        endif
        if(mod = "markpad")
          _markpad=get_assign_rval(linetext)
        endif
        if(mod = "markw")
          _markw=get_assign_rval(linetext)
        endif
        if(mod = "rightpad")
          _rightpad=get_assign_rval(linetext)
        endif

* NEW VARIABLES: Add an if statement here to assign the value of your
* variable:
*
*       if(mod = "mynewvar")
*         _mynewvar.i=get_assign_rval(linetext)
*       endif
*
* OR if you are making a variable that isn't set per model (like _tracktitle):
*
*       if(mod = "mynewvar")
*         _mynewvar=get_assign_rval(linetext)
*       endif
      endif
      if(op = "")
********* Handle model name and "default" lines
        if(mod = "defaults" | mod = "DEFAULTS")
          i=0
        else
          if(strlen(mod)>4)
            say filename ":" lineno ": model name ``" mod "'' is longer than four characters"
          endif
          _count=_count+1
          i=_count
          _model.i=mod
          _color.i=_color.0
          _mark.i=_mark.0
          _legmark.i=_legmark.0
          _line.i=_line.0
          _thick.i=_thick.0
          _darg6hr.i=_darg6hr.0
          _darg12hr.i=_darg12hr.0
          _legdarg.i=_font/2
          _desc.i=mod
          _defined.i="YES"
          _dh.mod="YES"
          _mi.mod=i
* NEW VARIABLES: If your variable is set on a per-model basis, then you need
* to add code here to copy the value from the defaults list to the model here:
*
*         _mynewvar.i=_mynewvar.0
*
* You ONLY need to add code here if your variable is set on a
* per-model basis (unlike _tracktitle)
        endif        
      endif
  endif
  lineno=lineno+1
endwhile

return

************************************************************************

function join_desc(sep)

i=1
out=""
while(i <= _count)
  if(_defined.i = "YES")
    out=out%_desc.i
  endif
  if(i < _count)
    out=out%sep
  endif
  i=i+1
endwhile

return out

************************************************************************

function dumpinfo(filename)
say "************************************************************************"
say "** DUMP OF PARSING RESULTS OF ``"%filename%"''"
say "************************************************************************"
say " "
i=1
while(i <= _count)
  if(_defined.i = "YES")
    say "MODEL " i ": " _model.i
    say "  color        = " _color.i
    say "  desc         = " _desc.i
    say "  drawargs6hr  = " _darg6hr.i
    say "  drawargs12hr = " _darg12hr.i
    say "  legdrawargs  = " _legdarg.i
    say "  legmark      = " _legmark.i
    say "  line         = " _line.i
    say "  mark         = " _mark.i
    say "  thick        = " _thick.i

* NEW VARIBLES: If you want the dumpinfo code to print out the value of your
* variable, add a line here:
*
*   say "mynewvar= " _mynewvar.i
*
* If your variable is NOT set on a per-model basis, then it goes below instead.

    say " "
  endif
  i=i+1
endwhile

* Print out variables that are not set on a per-model basis:

say 'tracktitle = "' _tracktitle '"'
say 'maxwtitle  = "' _maxwtitle '"'
say 'minptitle  = "' _minptitle '"'
say "charh      = " _charh
say "charw      = " _charw
say "lineh      = " _lineh
say "markw      = " _markw
say "markpad    = " _markpad
say "maxlegy    = " _maxlegy
say "legstartx  = " _legstartx
say "degratio   = " _degratio
say "bestborder = " _bestborder
say "maxborder  = " _maxborder
say "rightpad   = " _rightpad

* NEW VARIABLES: If your new variable is NOT set on a per-model basis,
* and you want dumpinfo to print it out, then add a line here:
* say "mynewvar   = " _mynewvar

say " "
say "************************************************************************"

return

************************************************************************

function strtrim(line)

lidx=1
while(1)
  char=substr(line,lidx,1)
  if( char = '' )
    return ""
  endif
  if( char != " ")
    break
  endif
endwhile

* lidx now points to the first char in line that is not whitespace

* Next, we have to find the first non-whitespace character scanning
* from the RIGHT side of the string.
ridx=strlen(line)

while(ridx>=1)
  char=substr(line,ridx,1)
  if(char = '')
*** we scanned past the left side of the string
    return ""
  else
    if(char != " ")
***** Found the last non-whitespace character
      break
    endif
  endif
  ridx=ridx-1
endwhile

return substr(line,lidx,ridx-lidx+1)

************************************************************************

function get_assign_rval(line_in)
* This function takes a string of the form:
*    varname = value
* Where the value can be any arbitrary text.  This function returns the value.
* It skips any whitespace that may be after the equal sign, and any whitespace
* beteween the value and the end of the string.
*
* Examples:
*   get_assign_rval("  joe = 3 ")
* would return "3"

line=strtrim(line_in)

lidx=1
while(1)
  char=substr(line,lidx,1)
  if(char = '')
*** There is no equal sign on the line.
    return ""
  else
    if(char = '=')
      break
    endif
  endif
  lidx=lidx+1
endwhile

* Currently, lidx is set to the location within the line where the
* first equal sign lies.  Move lidx forward to the first char after the
* equal sign:
lidx=lidx+1

* Now skip over whitespace
while(1)
  char=substr(line,lidx,1)
  if(char = '')
*** The text is entirely whitespace
    return ""
  else
    if(char != " ")
      break
    endif
  endif
  lidx=lidx+1
endwhile

* Now, lidx points to the first non-whitespace character in the line.
return substr(line,lidx,strlen(line)-lidx+1)

************************************************************************

* end plot_info.gs.inc
