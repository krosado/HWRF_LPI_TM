PROGRAM readstmid

  implicit none 

  integer :: lnbufr,lunout,iret,idate,levs,nmrecs
  character(len=80) hdrstr, bfile
  character(len=12) acid,acrn
  character(len=4) stmid,stmidin
  character(8) subset,subset_check
  real(8) :: hdr(3)

  equivalence (hdr(1), acid)
  equivalence (hdr(2), acrn)
  equivalence (hdr(3), stmid)

  data lnbufr/10/
  data hdrstr /'ACID ACRN STMID'/

  lunout = 70

  nmrecs=0

  subset_check ='NC006070'

  read(5,*)stmidin

  open(lnbufr,file='tldplrbufr',form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call readmg(lnbufr,subset,idate,iret)

  if(iret/=0) then
    write(6,*)'READ_RADAR: problem reading tail Doppler radar bufr file tldplrbufr'
    call closbf(lnbufr)
    go to 1100
  end if

!    Big loop over bufr file

70   call readsb(lnbufr,iret)
80   continue
  if(iret/=0) then
    call readmg(lnbufr,subset,idate,iret)
    if(iret/=0) go to 1100
    go to 70
  end if
  if(subset/=subset_check) then
    iret=99
    go to 80
  end if

  nmrecs = nmrecs+1

!    Read header.  Extract station infomration
  call ufbint(lnbufr,hdr,3,1,levs,hdrstr)

  if(stmid==stmidin)then
    print *,'same storm: ',stmid
    open(lunout,file='stmid.dat',form='formatted')
    write(lunout,*)trim(stmid)
    close(lunout)
    go to 1100
  else
    go to 70
  endif

! End of bufr read loop

69 continue

! Normal exit
1100 continue

  print *,'nmrecs=',nmrecs
  print *,'stmid=',stmid
  print *,'stmidin=',stmidin

  call closbf(lnbufr)


! Close unit to bufr file
  close(lnbufr)

END
