PROGRAM readtdrtime

  implicit none 

  integer :: lnbufr,lunout,iret,idate,levs,nmrecs
  character(len=80) hdrstr, bfile
  character(len=12) acid,acrn
  character(len=4) stmid,stmidin
  character(8) subset,subset_check
  character(10) date
  real(8) :: hdr(12)
  real :: r60inv,timeo,timemin, timemax
  integer :: iyr,imo,idy,ihr,imn,isc
  integer :: minobs,mincy
  integer,dimension(5):: idate5

  data lnbufr/10/
  data hdrstr /'PTID YEAR MNTH DAYS HOUR MINU SECO CLAT CLON HSMSL ANAZ ANEL'/

  lunout = 70

  nmrecs=0

  r60inv=1.0/60.0
  timemin=99999999.9
  timemax=-99999999.9

  subset_check ='NC006070'

  open(lnbufr,file='tldplrbufr',form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call readmg(lnbufr,subset,idate,iret)

  if(iret/=0) then
    write(6,*)'READ_RADAR: problem reading tail Doppler radar bufr file abrobsbufr'
    call closbf(lnbufr)
    go to 1100
  end if

  write(date,'( i10)') idate
  read (date,'(i4,3i2)') iyr,imo,idy,ihr
  write(6,*)'READ_RADAR: bufr file date is ',iyr,imo,idy,ihr

  idate5(1) = iyr    ! year
  idate5(2) = imo    ! month
  idate5(3) = idy   ! day
  idate5(4) = ihr   ! hour
  idate5(5) = 0     ! minute
  call w3fs21(idate5,mincy)

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
  call ufbint(lnbufr,hdr,12,1,levs,hdrstr)

  iyr = hdr(2)
  imo = hdr(3)
  idy = hdr(4)
  ihr = hdr(5)
  imn = hdr(6)
  isc = hdr(7)

  idate5(1) = iyr
  idate5(2) = imo
  idate5(3) = idy
  idate5(4) = ihr
  idate5(5) = imn

  call w3fs21(idate5,minobs)

  timeo = real(minobs-mincy)*r60inv

  timemin=min(timemin,timeo)
  timemax=max(timemax,timeo)

  go to 70

! End of bufr read loop

69 continue

! Normal exit
1100 continue

  print *,'nmrecs=',nmrecs
  print *,'timemin=',timemin,'timemax=',timemax

  if(timemin > 2.0 .or. timemax < -2.0 .or. abs(timemax-timemin) < 0.5)then

    open(lunout,file='tdrflag',form='formatted')
    write(lunout,*)'timemin=',timemin,'timemax=',timemax
    close(lunout)

  end if


  call closbf(lnbufr)


! Close unit to bufr file
  close(lnbufr)

END
