PROGRAM readtdrtrigger

  implicit none 

  integer :: lnbufr,lunout,iret,idate,levs,nmrecs
  character(len=80) hdrstr1, hdrstr2, bfile
  character(len=12) acid,acrn
  character(len=4) stmid,stmidin
  character(8) subset,subset_check
  character(10) date
  real(8) :: hdr1(3), hdr2(12)
  real :: r60inv,timeo
  integer :: iyr,imo,idy,ihr,imn,isc
  integer :: minobs,mincy
  integer,dimension(5):: idate5

  equivalence (hdr1(1), acid)
  equivalence (hdr1(2), acrn)
  equivalence (hdr1(3), stmid)

  data lnbufr/10/
  data hdrstr1 /'ACID ACRN STMID'/
  data hdrstr2 /'PTID YEAR MNTH DAYS HOUR MINU SECO CLAT CLON HSMSL ANAZ ANEL'/

  lunout = 70

  nmrecs=0

  r60inv=1.0/60.0

  subset_check ='NC006070'

  read(5,*)stmidin, date

  open(lnbufr,file='tldplrbufr',form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call readmg(lnbufr,subset,idate,iret)

  if(iret/=0) then
    write(6,*)'READ_RADAR: problem reading tail Doppler radar bufr file abrobsbufr'
    call closbf(lnbufr)
    go to 1100
  end if

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
  call ufbint(lnbufr,hdr1,3,1,levs,hdrstr1)
  if(stmid==stmidin)then
     go to 90
  else
     go to 70
  end if

90   continue

  call ufbint(lnbufr,hdr2,12,1,levs,hdrstr2)

  iyr = hdr2(2)
  imo = hdr2(3)
  idy = hdr2(4)
  ihr = hdr2(5)
  imn = hdr2(6)
  isc = hdr2(7)

  idate5(1) = iyr
  idate5(2) = imo
  idate5(3) = idy
  idate5(4) = ihr
  idate5(5) = imn

  call w3fs21(idate5,minobs)

  timeo = real(minobs-mincy)*r60inv

  if(timeo > -3.0 .and. timeo < 3.0) go to 69

  go to 70

! End of bufr read loop

69 continue

  print *,'nmrecs=',nmrecs
  print *,'same storm: ', stmid
  print *,'timeo=',timeo,'timemax=',timeo


  open(lunout,file='runensda',form='formatted')
  write(lunout,*)'runensda=YES'
  close(lunout)

! Normal exit
1100 continue


  call closbf(lnbufr)


! Close unit to bufr file
  close(lnbufr)

END
