function draw_legend(startx,starty,model_list)

*-----------------------------------------------------------------------
*        Now draw the legend
*-----------------------------------------------------------------------

legx=1
legy=1
legstarty=starty
legstartx=startx
maxwidcol=0
pastwid=0
FN=1
while(FN <= _count)
  if(model_list != 'model_list')
    if(!set_contains(model_list,_model.FN))
******* Skip unwanted models.
      FN=FN+1
      continue
    endif
  endif
  if(_defined.FN = 'YES')
    xloc=legstartx+pastwid
    yloc=legstarty-(legy-1)*_lineh

    "set line " _color.FN " " _line.FN " " _thick.FN
    "draw line " _markpad+xloc " " yloc " " xloc+_markw+_markpad " " yloc
    "draw " _legmark.FN " " _markpad+xloc+_markw/2 " " yloc " " _legdarg.FN

    "set string " _color.FN " l"
    "set strsiz " _charw " " _charh
    "draw string " xloc+_markw+2*_markpad " " yloc " " _desc.FN

    legwid=3*_markpad+_markw+_charw*strlen(_desc.FN)
    
    if(legwid>maxwidcol)
      maxwidcol=legwid
    endif

    legy=legy+1
    if(legy > _maxlegy)
      legy=1
      legx=legx+1
      pastwid=pastwid+maxwidcol+_rightpad
      maxwidcol=0
    endif
  endif
  FN=FN+1
endwhile

