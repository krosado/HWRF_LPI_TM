PROGRAM readtdrtrack

  implicit none 

  integer :: lnbufr,lunout,iret,idate,levs,nmrecs,nread
  character(len=80) hdrstr1, hdrstr2, datstr
  character(len=12) acid,acrn
  character(len=4) stmid
  character(8) subset,subset_check,cstaid
  character(10) date
  real(8) :: hdr1(3), hdr2(12)
  real :: r60inv,timeo
  integer :: iyr,imo,idy,ihr,imn,isc
  integer :: minobs,mincy,k,timemin, timemax
  integer,parameter:: maxlevs=1500
  integer,dimension(5):: idate5
  real(8),dimension(4,maxlevs):: tdr_obs
  real :: stalat,stalon,stahgt,tilt,azimuth,vr,vrange,obs

  integer :: NLEV, NFLAG
  real :: RLAT, RLON, TIM
  character(8) :: STID,stidtmp

  equivalence (hdr1(1), acid)
  equivalence (hdr1(2), acrn)
  equivalence (hdr1(3), stmid)

  data lnbufr/10/
  data hdrstr1 /'ACID ACRN STMID'/
  data hdrstr2 /'PTID YEAR MNTH DAYS HOUR MINU SECO CLAT CLON HSMSL ANAZ ANEL'/
  data datstr / 'DIST HREF DMVR DVSW' /

  lunout = 70

  timemin=99999999
  timemax=-99999999

  stidtmp = '        '
  TIM = 0.0
  NLEV = 1
  NFLAG = 1

  open(700,file='p3track.dat',form='unformatted')

  nmrecs=0

  r60inv=1.0/60.0

  subset_check ='NC006070'

  open(lnbufr,file='tldplrbufr',form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call readmg(lnbufr,subset,idate,iret)

  if(iret/=0) then
    write(6,*)'READ_RADAR: problem reading tail Doppler radar bufr file abrobsbufr'
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
  if(nmrecs == 1)then 
     call ufbint(lnbufr,hdr1,3,1,levs,hdrstr1)
     print *,'Storm ID:', stmid
  end if

  call ufbint(lnbufr,hdr2,12,1,levs,hdrstr2)

  if(hdr2(1) == 0)then
     cstaid='NOAA    '
  else if(hdr2(1) == 1)then
     cstaid='FRENCH  '
  else if(hdr2(1)== 2)then
     cstaid='G-IV    '
  else if(hdr2(1)== 3)then
     cstaid='AOC     '
  else
     cstaid='UNKNOWN '
  endif

  if(nmrecs==1)print *,'Antenna ID:', hdr2(1)

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
 
  if(nmrecs==1) print *,'iyr, imo, idy, ihr, imn, isc=', &
               iyr, imo, idy, ihr, imn, isc

  if(minobs < timemin)then
     print *,'timemin: idy, ihr, imn, isc=', &
              idy, ihr, imn, isc
  end if
  if(minobs > timemax)then
     print *,'timemax: idy, ihr, imn, isc=', &
              idy, ihr, imn, isc
  end if

  timemin=min(timemin,minobs)
  timemax=max(timemax,minobs)

  stalat=hdr2(8)
  stalon=hdr2(9)
  stahgt=hdr2(10)
  azimuth=hdr2(11)
  tilt=hdr2(12)

!  if(nmrecs<=400)print *,'stahgt,azimuth,tilt=', &
!                 stahgt,azimuth,tilt

  if(mod(nmrecs,10)==0)then
     STID='P3-TDR  '
     RLAT=stalat
     RLON=stalon
     obs=10.0
     WRITE (700) STID,RLAT,RLON,TIM,NLEV,NFLAG
     WRITE (700) obs
  endif

  call ufbint(lnbufr,tdr_obs,4,maxlevs,levs,datstr)

  nread=0
  do k=1,levs
     nread=nread+1
     vr=tdr_obs(3,k)
     vrange=tdr_obs(1,k)
     
  end do

! End of bufr read loop

  go to 70

! Normal exit
1100 continue

  call closbf(lnbufr)


! Close unit to bufr file
  close(lnbufr)

  STID = '        '
  NLEV = 0
  NFLAG = 0
  RLAT = 0.0
  RLON = 0.0

  WRITE (700) STID,RLAT,RLON,TIM,NLEV,NFLAG
  close(700)
  call write_station_ctl('p3track')

  print *,'number of record:', nmrecs

END PROGRAM readtdrtrack

subroutine write_station_ctl(label)

  character(*) label
  integer i,k
  real(4) undef
  character(1) blank
  character(80) datdes(13)
  integer unit_des
  character(80) label_dat
  character(80) label_des
  character(80) label_map

  blank=' '
  undef=-9.99e33

! create names of grads control and data files

  write(label_des,'(a,".ctl")')trim(label)
  write(label_dat,'(a,".dat")')trim(label)
  write(label_map,'(a,".map")')trim(label)

! find unused unit number

  unit_des=12

! initialize counters for this set of output fields

  startp=1.
  pinc=1.
  ntime=1
  do i=1,13
     write(datdes(i),'(80a1)')(blank,k=1,80)
  end do
  write(datdes(1),'("DSET ^",a)')trim(label_dat)
  write(datdes(2),'("DTYPE  station")')
  write(datdes(3),'("options big_endian sequential")')
  write(datdes(4),'("STNMAP ",a)')trim(label_map)
  write(datdes(5),'("UNDEF ",e11.2)')undef
  write(datdes(6),'("TITLE Station Data")')
  write(datdes(7),'("TDEF 1 LINEAR 00z28may2015 12hr")')
  write(datdes(8),'("VARS 1")')
  write(datdes(9),'("obs  0   99 obs    ")')
  write(datdes(11),'("ENDVARS")')
! write out datdes

  open(unit_des,file=label_des,form='formatted')
  rewind unit_des
  write(unit_des,'(a80)')datdes
  close(unit_des)

end subroutine write_station_ctl

