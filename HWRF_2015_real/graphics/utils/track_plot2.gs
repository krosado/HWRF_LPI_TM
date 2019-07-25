*
*  [PURPOSE]
*
*  Script to draw an hurricane-track plot.  
*  Does little error checking on the input file. 
*  Assumes the input file is set up as follows:
*
*  [HISTORY] - Chanh updated on August 5 for an ensemble plot. A 
*              quick guide is also added.
*
*  [GUIDE]
*
*  To use this, you first open a ctl file that supposed to cover
*  the entire track. Then prepare an input file as instructed.
*  Next, open the ctl file and PLOT a background map so that 
*  w2xy command is active. At the prompt, type track_plot.gs
*  and enter the input file. Suppose to see some track by now.
*
*  [INPUT]
*
*    Line 1:  Title
*    Line 2:  Drawing primitives for marks: marktype size 
*    Line 3:  Drawing primitives for lines: color style thickness
*    Line 4:  Starting hour and the interval of plotting points
*             e.g., 0 6 means that track starts at 0 hour and mark 
*                   will be plotted every 6 hours.
*    Rest of lines:  hour  long.  lat.
*             e.g.,   0    -70.5  25.0
*                     6    -71.8  25.2
*                            :
*                            : 
*
*  Also assumes that a file has been opened (any file, doesn't
*  matter -- the set command doesn't work until a file has been
*  opened).
*

function main()

scolor=1
nesem=1
say 'Enter the ensemble member wanted to plot'
*pull pmem
pmem=2 
ie=1
while (ie <= nesem)
 fname.ie='track_mem_'pmem'.txt'
 ie = ie + 1
endwhile

icount=1

while (icount <=nesem)

fname=fname.icount
say file to be opened' 'fname.icount

*  Read the 1st record: Title

  ret = read(fname)
  rc = sublin(ret,1)
  if (rc>0) 
      say 'File Error 1'
      return
  endif
  title = sublin(ret,2)
  say title

*  Read the drawing primitives

  ret = read(fname)
  rc = sublin(ret,1)
  if (rc>0)
     say 'File Error 2' 
     return
  endif
  dpline = sublin(ret,2)
  marktype = subwrd(dpline,1)
  marksize = subwrd(dpline,2)
  ret = read(fname)
  rc = sublin(ret,1)
  if (rc>0)
     say 'File Error 3' 
     return
  endif
  dpline = sublin(ret,2)
  lcolor = subwrd(dpline,1)
  lstyle = subwrd(dpline,2)
  lthick = subwrd(dpline,3)
  say ' marktype, marksize, lcolor, lstyle and lthick:'
  say ' 'marktype ' ' marksize ' ' lcolor ' ' lstyle ' ' lthick

* Read starting hour and the interval hours of plotting points

  ret = read(fname)
  rc = sublin(ret,1)
  if (rc>0)
     say 'File Error 4'
     return
  endif
  dhour = sublin(ret,2) 
  start = subwrd(dhour,1)
  jump = subwrd(dhour,2)
  say ' starting hour and the interval hours of plotting points:'
  say '  'start' 'jump
   
*  Read all data points

  ret = read(fname)
  rc = sublin(ret,1)
  while (rc = 0) 
      loc = sublin(ret,2)
      hour = subwrd(loc,1)
      dlong.hour = subwrd(loc,2)
      dlat.hour = subwrd(loc,3)
*      prompt ' hour ' hour ' are read,'
*      say ' long=' dlong.hour '    lat=' dlat.hour
      ret = read(fname)
      rc = sublin(ret,1)
  endwhile

  if (rc!=2 & rc!=0) 
         say 'File Error 5, rc=' rc
         return
  endif

  endhour = hour
  say ' endhour=' endhour

* Plotting

*  lcolor = scolor 
  'set line 'lcolor' 'lstyle' 'lthick
  'query w2xy 'dlong.start' 'dlat.start
  xprev = subwrd(result,3) 
  yprev = subwrd(result,6) 
  'draw mark 'marktype' 'xprev' 'yprev' 'marksize
  next = start+jump 
  while (next <= endhour)
      'query w2xy 'dlong.next' 'dlat.next
      xnext = subwrd(result,3)
      ynext = subwrd(result,6)
      'draw line 'xprev' 'yprev' 'xnext' 'ynext
      'draw mark 'marktype' 'xnext' 'ynext' 'marksize
      next = next+jump
      xprev = xnext
      yprev = ynext
  endwhile

  say fname ' is working fine.'
 
 scolor = scolor + 1 
 if (scolor > 14) ; scolor=1 ; endif
 icount = icount + 1
endwhile

