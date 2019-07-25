program combinetrack
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! PROGRAM COMBINETRACK: takes an ATCFUNIX file (argument 1) and an
  !! HTCF file (argument 2), and outputs a new ATCFUNIX file (argument
  !! 3) containing the track from the ATCFUNIX file and the intensity
  !! from the HTCF file.  The output track will be terminated either
  !! when an input track runs out of data, or when the ATCFUNIX storm
  !! location varies from the HTCF nest center location by
  !! >cloc_too_far meters, if the pressure minimum is >ploc_too_far
  !! meters from the ATCFUNIX storm center, or if the wind maximum is
  !! >wloc_too_far meters from the ATCFUNIX storm center.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  ! NOTE: if you change these, make sure you change the error messages too:
  real, parameter :: cloc_too_far = 200000 ! meters
  real, parameter :: wloc_too_far = 300000 ! meters
  real, parameter :: ploc_too_far = 200000 ! meters
  real, parameter :: close_enough = 30.0*60.0 ! seconds

  real :: atcftime,htcftime,oldtime,ahtime,dist,atcfwind,atcfpres
  integer :: atcferr,htcferr,outerr

  logical :: nohtcf

  real :: wlat,wlon,wval
  real :: plat,plon,pval
  real :: clat,clon
  real :: lat,lon
  logical :: first

  character*2 :: basin2,method2,storm2
  character*4 :: model4
  integer :: ymdh

  character*2 :: out_basin2,out_method2, out_storm2
  character*4 :: out_model4
  integer :: out_ymdh

  integer :: hLUN,aLUN,oLUN
  character*1000 :: hfile,afile,outfile,errortext,message,longmodel

  first=.true.
  errortext='*** ERROR ***'
  hfile=errortext
  afile=errortext
  outfile=errortext
  longmodel=errortext

  call getarg(1,hfile)
  if(hfile==errortext .or. hfile==' ') then
     call fail('Input HTCF file (argument 1) unspecified.')
  endif

  call getarg(2,afile)
  if(afile==errortext .or. afile==' ') then
     call fail('Input ATCF file (argument 2) unspecified.')
  endif

  call getarg(3,outfile)
  if(outfile==errortext .or. outfile==' ') then
     call fail('Output ATCF file (argument 3) unspecified.')
  endif

  call getarg(4,longmodel)
  if(longmodel==errortext .or. longmodel==' ') then
     call fail('Model name (argument 4) unspecified.')
  endif
  if(len_trim(longmodel)>4) then
     call fail('Model name must be no more than 4 characters long.')
  end if

  hLUN=30
  aLUN=40
  oLUN=50

  if(trim(hfile) == '-nohtcf') then
     write(0,*) 'Running without an HTCF file.'
     nohtcf=.true.
  else
     open(unit=hLUN,file=trim(hfile),status='OLD',err=100)
     nohtcf=.false.
  endif
  open(unit=aLUN,file=trim(afile),status='OLD',err=200)
  open(unit=oLUN,file=trim(outfile),err=300)

  atcferr=0
  htcferr=0
  atcftime=-9999999.0
  htcftime=-9999999.0

  bigloop: do
     ! Read the ATCF track for the next forecast hour:
     oldtime=atcftime
     atcfloop: do
        call read_atcfunix(aLUN,atcferr,atcftime,lat,lon,basin2,method2,model4,storm2,ymdh,atcfwind,atcfpres)
        if(atcferr>0) then
           call fail('Error reading ATCFUNIX file')
        endif
        if(atcferr<0) then
           call done('End of file on ATCFUNIX file')
        endif
        if(first) then
           first=.false.
           out_basin2=basin2
           out_model4=model4
           out_method2=method2
           out_storm2=storm2
           out_ymdh=ymdh
        endif
        if(atcftime > oldtime) then
           exit atcfloop
        endif
     enddo atcfloop

     write(0,*) 'Found ATCFUNIX time: ',atcftime

     if_nohtcf: if(nohtcf) then
        call write_track(oLUN,outerr,atcftime,out_basin2,longmodel, &
             out_method2,out_storm2,out_ymdh,lat,lon,atcfwind,atcfpres)
     else
        ! Read the first HTCF track that has a forecast second
        ! >= to the ATCF forecast hour * 3600
        oldtime=htcftime
        ahtime=3600*atcftime
        htcfloop: do
           htcferr=0
           call read_htcf(hLUN,htcferr,htcftime,wlat,wlon,wval,plat,plon,pval,clat,clon)
           if(htcferr>0) then
              call fail('Error reading on HTCF file')
           endif
           if(htcferr<0) then
              ! hit end of file.  Are we pretty close to the ATCF time?
              if(htcftime >= ahtime-close_enough) then
                 write(0,*) 'Found HTCF time: ',htcftime/3600
                 write(0,*) 'That is not the ATCFUNIX time, but we are at EOF and it is close enough.'
                 exit htcfloop
              else
                 call fail('End of file on HTCF file before finding ATCFUNIX time.')
              endif
           endif

           if(htcftime >= ahtime) then
              write(0,*) 'Found HTCF time: ',htcftime/3600.0
              exit htcfloop
           endif
        enddo htcfloop

        dist=greatarc(clat,clon,lat,lon)
        if(dist > cloc_too_far) then
           call done('Tracks vary by >200km')
        endif

        dist=greatarc(wlat,wlon,lat,lon)
        if(dist > wloc_too_far) then
           call done('Velocity maximum is >300km from NCEP Track location.')
        endif

        dist=greatarc(plat,plon,lat,lon)
        if(dist > ploc_too_far) then
           call done('Pressure minimum is >200km from NCEP Track location.')
        endif


        call write_track(oLUN,outerr,atcftime,out_basin2,longmodel, &
             out_method2,out_storm2,out_ymdh,lat,lon,wval,pval)
     endif if_nohtcf
  enddo bigloop

  stop 98 ! should never get here

100 continue
  write(message,'("Unable to open ATCF file ",A1,A,A1," for reading.")') &
       '"',trim(hfile),'"'
  call fail(message)
  stop 99 ! should never get here

200 continue
  write(message,'("Unable to open HTCF file ",A1,A,A1," for reading.")') &
       '"',trim(afile),'"'
  call fail(message)
  stop 99 ! should never get here

300 continue
  write(message,'("Unable to open output ATCF file ",A1,A,A1," for writing.")') &
       '"',trim(outfile),'"'
  call fail(message)
  stop 99 ! should never get here

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! FAIL: prints a message to stdout and stderr and aborts the
  !! program with status 13
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine fail(why)
    implicit none
    character*(*),intent(in) :: why

    write(0,*) trim(why)

    stop 13
  end subroutine fail

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! DONE: prints a message to stdout and stderr and exits the
  !! program with status 0 indicating successful completion
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine done(why)
    implicit none
    character*(*),intent(in) :: why

    write(0,*) trim(why)

    write(0,*) 'SUCCESSFUL COMPLETION'

    stop 0
  end subroutine done

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! WRITE_TRACK : takes track information as input, and writes it out
  !! in ATCFUNIX format to logical unit number oLUN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine write_track(LUN,outerr,atcftime,basin2,model,method2,storm2,ymdh, &
       lat,lon,wval,pval)
    implicit none

    character*(*), intent(in) :: model
    real, intent(in) :: lat,lon,wval,pval,atcftime
    integer, intent(out) :: outerr
    character*2, intent(in) :: basin2,method2,storm2
    integer, intent(in) :: ymdh, LUN
    integer :: ifcsthour, ilat, ilon, iwind, ipres
    character*1 :: ns,ew

    ilat=nint(lat*10.0)
    ilon=nint(lon*10.0)
    if(ilat<0) then
       ilat=-ilat
       ns='S'
    else
       ns='N'
    endif
    if(ilon<0) then
       ilon=-ilon
       ew='W'
    else
       ew='E'
    endif
    ifcsthour=nint(atcftime)
    iwind=nint(wval)
    ipres=nint(pval)

    outerr=0
    write(LUN,2281,iostat=outerr) basin2,storm2,ymdh,method2,trim(model), &
         ifcsthour,ilat,ns,ilon,ew,iwind,ipres

2281 format (a2,', ',a2,', ',i10.10,', ',a2,', ',a4,', ',i3.3,', ',i3,a1, &
         ', ',i4,a1,', ',i3,', ',i4)
  end subroutine write_track

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! GREATARC: calculates the great arc distance between two points.
  !! the Earth radius used is the approximate Earth radius at the 
  !! mean of the two latitudes.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  real function greatarc(lat1,lon1,lat2,lon2)
    implicit none
    real, intent(in) :: lat1,lon1, lat2,lon2
    real :: rlat1,rlon1, rlat2,rlon2
    real :: Rearth1,Rearth2
    real, parameter :: pi=3.141592653589793238
    real, parameter :: Requator=6378137,deg2rad=pi/180.0
    real, parameter :: flattening_inv=298.247 ! Requator/(Requator-Rpole)

    rlat1=lat1*deg2rad  ;  rlon1=lon1*deg2rad
    rlat2=lat2*deg2rad  ;  rlon2=lon2*deg2rad

    Rearth1=Requator*(1-sin(rlat1)**2/flattening_inv)
    Rearth2=Requator*(1-sin(rlat2)**2/flattening_inv)

    greatarc=(Rearth1+Rearth2)*asin(min(1.0,sqrt( &
         sin((rlat1-rlat2)/2)**2+ &
         cos(rlat1)*cos(rlat2)*sin((rlon1-rlon2)/2)**2)))
  end function greatarc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! READ_ATCFUNIX: reads one line from the ATCFUNIX file specified opened
  !! for reading in logical unit number LUN.  On failure or end of file
  !! atcferr will be set non-zero.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine read_atcfunix(LUN,atcferr,atcftime,lat,lon,basin2,method2,model4,storm2,ymdh,wind,pres)
    implicit none
    character*2, intent(out) :: basin2,method2,storm2
    character*4, intent(out) :: model4
    integer, intent(out) :: ymdh
    integer, intent(inout) :: atcferr
    integer, intent(in) :: LUN
    real, intent(out) :: atcftime, lat,lon,wind,pres
    integer :: ifcsthour, ilat, ilon, iwind, ipres
    character*1 :: ns,ew

    atcferr=0
    read(LUN,1181,iostat=atcferr) basin2,storm2,ymdh,method2,model4, &
         ifcsthour,ilat,ns,ilon,ew,iwind,ipres
    if(atcferr/=0) return

    atcftime=real(ifcsthour)
    lat=real(ilat)/10.0
    lon=real(ilon)/10.0
    wind=real(iwind)
    pres=real(ipres)

    if(ns=='S') lat=-lat
    if(ew=='W') lon=-lon

1181 format (a2,', ',a2,', ',i10.10,', ',a2,', ',a4,', ',i3.3,', ',i3,a1, &
         ', ',i4,a1,', ',i3,', ',i4)
  end subroutine read_atcfunix

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! READ_HTCF: reads one line from the HTCF file specified opened
  !! for reading in logical unit number LUN.  On failure or end of file
  !! atcferr will be set non-zero.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine read_htcf(LUN,htcferr,htcftime,wlat,wlon,wval,plat,plon,pval,clat,clon)
    implicit none
    real, intent(inout) :: htcftime,wlat,wlon,wval,plat,plon,pval,clat,clon
    integer, intent(inout) :: htcferr
    integer, intent(in) :: LUN
    character*1 :: pns,pew,wns,wew,cns,cew
    character*36 :: junk

    htcferr=0

1313 format(A36,", ",F11.2,", ", &
         F8.3,", ",F6.3,A1,", ",F7.3,A1,", ", &
         F7.3,", ",F6.3,A1,", ",F7.3,A1,", ", &
         F6.3,A1,", ",F7.3,A1)
    read(LUN,1313,iostat=htcferr) &
         junk, htcftime, &
         pval,plat,pns,plon,pew, &
         wval,wlat,wns,wlon,wew, &
         clat,cns,clon,cew

    if(htcferr /= 0) return

    if(pns == 'S') plat=-plat
    if(wns == 'S') wlat=-wlat
    if(cns == 'S') clat=-clat

    if(pew == 'W') plon=-plon
    if(wew == 'W') wlon=-wlon
    if(cew == 'W') clon=-clon

  end subroutine read_htcf

end program combinetrack
