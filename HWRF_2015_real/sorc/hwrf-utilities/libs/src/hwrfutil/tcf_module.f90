module tcf_module
  ! This module contains subroutines for reading, writing and
  ! manipulating Automated Tropical Cyclone Forecast (ATCF) and
  ! High-frequency Tropical Cyclone Forecast (HTCF) files, as well as
  ! some simple products generated from them (afos, stats.short,
  ! stats.tpc and htcfstats)
  use sysutil_module
  implicit none

  private
  public :: atcf, htcf, write_stats_short, write_header, write_disclaimer, &
       write_afos, write_stats_tpc, read_tcvitals_as_atcf, read_atcf, &
       read_htcf, write_htcf, interp_atcf_to_htcf, htcf_stats, &
       parse_mdstatus, TierI_filename

  type atcf
     ! Usual ATCF file information:
     character*2 :: basin2,method2,storm2
     character*4 :: model4
     character*4 :: centername
     integer :: ymdh
     real :: fcsthour, lat,lon,wind,pres

     ! Extra things for the tcvitals:
     real :: stormspeed, stormdir ! ATCF warning: calculated from prior loc
     character*9 :: stormname ! ATCF warning: always blank
     character*1 :: basin1 ! ATCF warning: invalid in IO, SH

     ! epoch: unix epoch time (seconds since the beginning of 1970)
     real(kind=8) :: epoch

     ! valid: should this be used for statistics? 
     ! actually a dummy -- this is always .TRUE. for ATCF
     logical :: valid
  end type atcf

  type htcf
     real :: fcstsec,wlat,wlon,wval,plat,plon,pval,clat,clon
     character*2 :: basin2,storm2
     integer :: ymdh,minute
     character*4 :: model4,submodel4

     ! epoch: unix epoch time (seconds since the beginning of 1970)
     real(kind=8) :: epoch

     ! valid: should this be used for statistics?
     ! (not a dummy, unlike ATCF)
     ! moved: has the nest moved since the last HTCF time?
     logical :: valid, moved

     ! movedist: if moved==.true. then this is the distance it moved in km
     real :: movedist

     ! ATCF values, interpolated to HTCF times.  The atcfdist is
     ! the distance from the HTCF location to the ATCF location
     real :: atcfdist, atcfpres, atcfwind, atcflat, atcflon
  end type htcf

contains

  subroutine TierI_filename(atcf0,outname,model,submodel,realtime)
    ! Generates the TCMT Tier I filename from the tcvitals
    type(atcf), intent(in) :: atcf0
    character*(4), intent(in) :: model, submodel
    logical, intent(in) :: realtime

    character*(*), intent(out) :: outname

    character*1 :: hd
    character*200 :: work
    integer :: i,n,c
100 format('a',A2,A2,I4,'_',A,'_',A1,A,'_',I10,'.dat')

    if(realtime) then
       hd='d'
    else
       hd='h'
    endif

    write(work,100) atcf0%basin2,atcf0%storm2,atcf0%ymdh/1000000,trim(model), &
         hd, trim(submodel),atcf0%ymdh

    ! Convert basin to lower-case 
    n=len_trim(work)
    do i=1,n
       c=ichar(work(i:i))
       if(c==32) then
          work(i:i)='0'
       elseif(i<4 .and. c>=65 .and. c<=90) then
          work(i:i)=char(c+32)
       endif
    enddo
    outname=trim(work)
  end subroutine TierI_filename

  subroutine parse_MDstatus(filename,fcst_len,last_hr_coupled,coupler_dt,grace_period)
    ! Parse the MDstatus file to find the first hour after the grace
    ! period at which the model lost coupling.
    !   filename - input: path to the MDstatus file
    !   fcst_len - input: full length of forecast
    !   last_hr_coupled - output: last coupled forecast hour or -99
    !   grace_period - length in hours of a "grace period" at the beginning of
    !     the forecast during which the model is allowed to be uncoupled.
    !     As long as it regains coupling by the end of the grace period,
    !     the model is considered coupled.
    !   coupler_dt - coupling timestep
    real, intent(inout) :: last_hr_coupled
    real, intent(in) :: coupler_dt, fcst_len, grace_period
    character*(*), intent(in) :: filename
    integer :: iunit, ierr
    character*(400) :: line1
    real :: fread, last_hour_seen, this_hour
    logical :: coupled_last_seen

    iunit=get_unit()
    open(iunit,file=trim(filename),status='OLD',form='FORMATTED',iostat=ierr)
    openable: if(ierr/=0) then
       call warn('Cannot open MDstatus file: forecast was run uncoupled.')
       last_hr_coupled=-99.
    else
       last_hour_seen=0         ! last time coupled/uncoupled status changed
       coupled_last_seen=.true. ! was the model coupled at that time?

       readloop: do while (.true.)
          read(iunit,'(A)',iostat=ierr) line1
          if(ierr/=0) exit readloop
          if(index(line1,'dry')>0) cycle readloop

          read(line1,'(F8.0)',iostat=ierr) fread
          if(ierr/=0) exit readloop
          this_hour=(fread-coupler_dt)/3600.

          if(index(line1,'=T')>0) then
             if(coupled_last_seen) then
                coupled_last_seen=.false.
                last_hour_seen=this_hour
                print *,'no longer coupled at ',this_hour
             else
                ! We were told twice that coupling is not happening
                print *,'already uncoupled ',this_hour
             endif
          else
             if(.not.coupled_last_seen) then
                if(this_hour > grace_period) then
                   ! Coupling started after the grace period, so
                   ! assume no coupling.
                   print *,'tried to re-enter coupling after grace period at ',this_hour
                   exit readloop
                endif
                print *,'now coupled at ',this_hour
                coupled_last_seen=.true.
                last_hour_seen=this_hour
             else
                ! We were told twice that coupling is happening
                print *,'already coupled at ',this_hour
             endif
          endif
       enddo readloop
       close(iunit)
       if(coupled_last_seen) then
          last_hr_coupled=fcst_len
       else
          last_hr_coupled=last_hour_seen
       endif
    endif openable
  end subroutine parse_MDstatus

  subroutine write_stats_short(iunit,N,A,atcf0)
    implicit none
    type(atcf), intent(in) :: A(N), atcf0
    integer, intent(in) :: iunit, N
    integer :: i,isymtyp

103 format("HOUR:",F5.1,"  LONG: ",F7.2,"  LAT: ",F6.2, &
         "  MIN PRES (hPa): ",F7.2,"   MAX SURF WIND (KNOTS): ",F5.2)

    if(N<1) then
       write(iunit,103)  atcf0%fcsthour, atcf0%lon, atcf0%lat, atcf0%pres, atcf0%wind
    endif
    do i=1,N
       write(iunit,103) A(i)%fcsthour, A(i)%lon, A(i)%lat, A(i)%pres, A(i)%wind
    end do

  end subroutine write_stats_short

  subroutine write_header(iunit,atcf0,model_name)
    integer, intent(in) :: iunit
    type(atcf), intent(in) :: atcf0
    integer :: istmtyp
    character*(*) :: model_name
    character*(19), parameter, dimension(3) :: stmtyp= &
         (/'TROPICAL DEPRESSION','TROPICAL STORM','HURRICANE'/)

    integer :: imonth
    character*(3), parameter :: month(12) = &
         (/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP', &
         'OCT','NOV','DEC'/)

101 format("ATTENTION...",A)
102 format(A," FORECAST MADE FOR")
103 format(A," ",A9," ",A2,A1)
104 format('INITIAL TIME  ',I2,'Z ',A3,' ',I2)

    imonth=max(1,min(12,mod(atcf0%ymdh/10000,100)))

    ! Use the exact cutoffs from hwrf_afos for storm type:
    if(atcf0%wind <= 34.0) then
       istmtyp=1
    elseif(atcf0%wind <= 66.0) then
       istmtyp=2
    else
       istmtyp=3
    endif

    if(trim(atcf0%centername)=='NHC') then
       write(iunit,101) 'NATIONAL HURRICANE CENTER'
    elseif(trim(atcf0%centername)=='JTWC') then
       write(iunit,101) 'JOINT TYPHOON WARNING CENTER'
    else
       write(iunit,101) 'FORECASTING CENTER'
    endif
    write(iunit,*)
    write(iunit,102) trim(model_name)
    write(iunit,*)
    write(iunit,103) trim(stmtyp(istmtyp)), atcf0%stormname,atcf0%storm2,atcf0%basin1
    write(iunit,*)
    write(iunit,104) mod(atcf0%ymdh,100),month(imonth),mod(atcf0%ymdh/100,100)
  end subroutine write_header

  subroutine write_disclaimer(iunit,atcf0)
    integer, intent(in) :: iunit
    type(atcf),intent(in) :: atcf0
101 format(A)
102 format(A,A)
    write(iunit,101) "DISCLAIMER ...  THIS INFORMATION IS PROVIDED AS GUIDANCE.  IT"
    write(iunit,101) "REQUIRES INTERPRETATION BY HURRICANE SPECIALISTS AND SHOULD"
    write(iunit,102) "NOT BE CONSIDERED AS A FINAL PRODUCT.  PLEASE SEE THE ",&
         trim(atcf0%centername)
    write(iunit,101) "OFFICIAL FORECAST."
  end subroutine write_disclaimer

  subroutine write_afos(iunit,N,A,atcf0)
    ! Writes the AFOS file, with the exception of the header.  To
    ! write the header, call write_header first.
    implicit none
    integer, intent(in) :: iunit, N
    type(atcf), intent(in) :: A(N), atcf0
    integer :: i
301 format(I4,"          ",F6.1,"          ",F7.1,"           ",F4.0,"/",F4.1)

    write(iunit,'(A)') "FORECAST STORM POSITION"
    write(iunit,*)
    write(iunit,'(A)') 'HOUR        LATITUDE        LONGITUDE        HEADING/SPEED(KT)'
    write(iunit,*)
    if(N<1) then
       write(iunit,301) nint(atcf0%fcsthour),atcf0%lat,-atcf0%lon,atcf0%stormdir,atcf0%stormspeed
    else
       write(iunit,301) nint(A(1)%fcsthour),A(1)%lat,-A(1)%lon,atcf0%stormdir,atcf0%stormspeed
    endif
    do i=2,N
       write(iunit,301) nint(A(i)%fcsthour),A(i)%lat,-A(i)%lon,A(i)%stormdir,A(i)%stormspeed
    end do
  end subroutine write_afos

  subroutine write_stats_tpc(iunit,N,A,atcf0,last_hr_coupled,min_track_len)
    ! Writes the data portion of the stats.tpc file.  To write the
    ! header, call write_header first.  To write the disclaimer, call
    ! write_disclaimer.
    implicit none
    type(atcf), intent(in) :: A(N), atcf0
    integer, intent(in) :: iunit, N
    real, intent(in) :: last_hr_coupled, min_track_len
    integer :: i, istmtyp
    character*(2) :: stid2

102 format("HOUR: ",F5.1,"  LONG: ",F7.2,"  LAT: ",F6.2, &
         "  MIN PRES (hPa): ",F7.2,"   MAX SURF WIND (KNOTS): ",F6.2)
103 format('    FORECAST RAN COUPLED TO HOUR:',F6.1)
104 format('    STORM DISSIPATED AT ',I0,' HOURS AT ABOVE POSITION.')

    write(iunit,'(A)') "FORECAST POSITIONS"
    write(iunit,*)
    write(iunit,'(A)') "HOUR     LATITUDE    LONGITUDE    MIN PRESS (hPa)     MAX SFC WIND (KTS)"
    write(iunit,*)
    if(N<1) then
       write(iunit,102) atcf0%fcsthour,atcf0%lon,atcf0%lat,atcf0%pres,atcf0%wind
    endif
    do i=1,N
       write(iunit,102) A(i)%fcsthour, A(i)%lon, A(i)%lat, A(i)%pres, A(i)%wind
    end do
    if(last_hr_coupled<0 .or. N<1) then
       write(iunit,'("    ",A)') 'FORECAST RAN UNCOUPLED'
    elseif(last_hr_coupled>=A(N)%fcsthour) then
       write(iunit,'("    ",A)') 'FORECAST RAN COUPLED FOR THE ENTIRE PERIOD...'
    else
       write(iunit,103) last_hr_coupled
    endif

    if(N>=1) then
       if(A(N)%fcsthour+1e-3<min_track_len) then
          write(iunit,104) nint(A(N)%fcsthour)
       endif
    endif
  end subroutine write_stats_tpc

  logical function is_any(s)
    implicit none
    character*(*), intent(in) :: s
    is_any=.false.
    if(len_trim(s)/=3) return
    if(s(1:1)/='A') return
    if(s(2:2)/='N') return
    if(s(3:3)/='Y') return
    is_any=.true.
  end function is_any

  subroutine read_tcvitals_as_atcf(filename,atcf0,want_stid,want_centername,want_ymdh)
    ! This reads tcvitals data into a type(atcf) structure atcf0.
    ! There may be many more lines in the file for many other storms
    ! and times.  This reads the last time it sees that meets the
    ! specifications (want_stid, want_centername, want_ymdh)
    type(atcf), intent(inout) :: atcf0
    character*(*), intent(in) :: filename

    integer :: yyyymmdd, hh, ilat, ilon, ipres, iwind, ymdh, idir,ispeed
    integer :: xyyyymmdd, xhh, xilat, xilon, xipres, xiwind, xidir, xispeed, xymdh
    integer :: want_ymdh
    character*(3) :: want_stid, stid, xstid
    character*(1) :: clat,clon, xclat,xclon
    character*(9) :: stormname, xstormname
    character*(4) :: centername, xcentername, want_centername
    integer :: ierr, xierr, inunit
    character*(1000) :: message
    logical :: found
    character*(1000) :: line
8   format(A1000)
9   format (A4,X,A3,X,A9,X,I8,X,I2,3X,I3,A1,X,I4,A1,X,I3,X,I3,X,    I4,11X,I2)

    inunit=get_unit()
    open(inunit,file=filename,status='OLD',form='FORMATTED')

    stid=' '
    centername=' '
    ierr=0
    found=.false.
    readloop: do while(ierr==0)
       ierr=0
       read(inunit,'(A)',iostat=ierr) line
       if(ierr<=0) then
          xierr=0
          read(line,9,iostat=xierr) xcentername,xstid,xstormname, &
               xyyyymmdd, xhh, xilat,xclat, xilon,xclon,         &
               xidir,xispeed,  xipres, xiwind
          xymdh=xyyyymmdd*100+xhh
          if(xstid==want_stid .and. (is_any(want_centername) .or. &
               trim(xcentername)==trim(want_centername) ) &
               .and. xymdh==want_ymdh .and. xierr==0) then
             centername = xcentername
             stid       = xstid
             stormname  = xstormname
             yyyymmdd   = xyyyymmdd
             hh         = xhh
             ymdh       = xymdh
             ilat       = xilat
             clat       = xclat
             ilon       = xilon
             clon       = xclon
             idir       = xidir
             ispeed     = xispeed
             ipres      = xipres
             iwind      = xiwind
             found=.true.
          else
             if(trim(want_centername)=='ANY') then
302             format(A,": '",A,"'/='",A,"' or '",A,"'/='",A,"' or ",I0,"/=0")
                write(message,302) trim(filename),xstid,want_stid,xymdh,want_ymdh,xierr
             else
301             format(A,": '",A,"'/='",A,"' or '",A,"'/='",A,"' or ",I10,"/=",I10," or ",I0,"/=0")
                write(message,301) trim(filename),xstid,want_stid,trim(xcentername),trim(want_centername),xymdh,want_ymdh,xierr
             endif
             call warn(message)
          endif
       else
          write(message,'(A,": read error or eof")') trim(filename)
          call warn(message)
       endif
    end do readloop

    havedata: if(.not. found) then
10     format(A,": cannot find storm ",A," cycle ",I10," for forecasting center ",A," in tcvitals")
       write(message,10) trim(filename),trim(want_stid),want_ymdh,trim(want_centername)
       call fail(trim(message))
    else
       atcf0%basin1=stid(3:3)
       atcf0%storm2=stid(1:2)
       atcf0%method2='-9'
       atcf0%model4='TCVT'
       atcf0%stormname=stormname
       atcf0%centername=centername
       atcf0%ymdh=yyyymmdd*100+hh

       if(idir<=0 .or. ispeed<=0) then
          atcf0%stormspeed=0.0
          atcf0%stormdir=0.0
       else
          atcf0%stormspeed=real(ispeed)*1.940/10.0
          atcf0%stormdir=real(idir)
       endif

       if(clat=='S') then
          atcf0%lat=real(-ilat)/10.0
       else
          atcf0%lat=real(ilat)/10.0
       endif
       if(clon=='W') then
          atcf0%lon=real(-ilon)/10.0
       else
          atcf0%lon=real(ilon)/10.0
       endif
       atcf0%wind=real(iwind)/0.51444
       atcf0%pres=real(ipres)
       atcf0%valid=.true.
       atcf0%epoch=epochtime(atcf0%ymdh)
       checkbasin: select case(atcf0%basin1)
       case('L')
          if(atcf0%lat<0) then
             atcf0%basin2='SL'
          else
             atcf0%basin2='AL'
          endif
       case('Q')
          if(atcf0%lat<0) then
             atcf0%basin2='SL'
          else
             atcf0%basin2='AL'
          endif
       case('E')
          atcf0%basin2='EP'
       case('C')
          atcf0%basin2='CP'
       case('W')
          atcf0%basin2='WP'
       case('A')
          atcf0%basin2='IO'
       case('B')
          atcf0%basin2='IO'
       case('S')
          atcf0%basin2='SH'
       case('P')
          atcf0%basin2='SH'
       case default
          atcf0%basin2='XX'
       end select checkbasin
    end if havedata
  end subroutine read_tcvitals_as_atcf

  ! ------------------------------------------------------------

  function epochtime(ymdh)
    ! epochtime -- returns the unix epoch time for a YYYYMMDDHH time.
    !          This is a simple wrapper around the C c_etime subroutine.
    !   ymdh -- an integer of the form YYYYMMDDHH specifying a date/time
    !   returns -- number of seconds since midnight, January 1, 1970 UTC
    implicit none
    logical, save :: inited=.false.
    integer, intent(in) :: ymdh
    real(kind=8) :: epochtime
    integer(kind=8) :: temp
    integer :: err

    if(.not.inited) then
       call c_etimeinit(err)
       inited=.true.
    endif

    call c_etime(ymdh,temp)
    epochtime=real(temp,kind=8)
  end function epochtime

  ! ------------------------------------------------------------

  function ymdhtime(epoch)
    ! ymdhtime -- returns an integer YYYYMMDDHH time for a unix epoch time
    !       This is a simple wrapper around the C c_ytime subroutine.
    !     epoch -- integer number of seconds since midnight, January 1, 1970 UTC
    !     returns -- the equivalent integer YYYYMMDDHH time
    implicit none
    real(kind=8), intent(in) :: epoch
    integer :: ymdhtime
    integer(kind=8) :: temp
    temp=nint(epoch,kind=8)
    call c_ytime(temp,ymdhtime)
  end function ymdhtime

  ! ------------------------------------------------------------

  subroutine read_atcf(filename,out,len,used)
    ! read_atcf -- reads an atcf file into an array of atcf objects
    !     filename -- file to read
    !     out -- len-length array of type(atcf) to receive data
    !     len -- length of out
    !     used -- will be set to the number of atcf objects read, or
    !             0 if the file was empty or an error occured.
    ! NOTE: If the file has more entries than "len" then only the
    !     first "len" entries will be read.
    implicit none
    integer, intent(in) :: len
    integer, intent(out) :: used
    character(*), intent(in) :: filename
    type(atcf), intent(out) :: out(len)
    integer :: ifcsthour, ilat, ilon, iwind, ipres, u, atcferr,unit
    real :: lastlat, lastlon
    real :: lasttime, sdir,sdist
    character*1 :: ns,ew
    character*200 :: message
    logical firsttime

    unit=get_unit()
    open(unit=unit,file=trim(filename),status='OLD',err=1010)

    atcferr=0
    firsttime=.true.
    lasttime=-9999.9
    used=0
    atcfloop: do while(atcferr==0) 
       u=used+1
       !write(0,*) 'u is now ',u
       if(u>len) then
          write(message,1002) len
          call warn(message)
          exit atcfloop ! stop reading if we run out of space
       end if
       read(unit,1001,iostat=atcferr) &
            out(u)%basin2,out(u)%storm2,out(u)%ymdh,out(u)%method2,out(u)%model4, &
            ifcsthour,ilat,ns,ilon,ew,iwind,ipres

       !write(0,*) 'u=',u
       !write(0,*) 'ymdh=',out(u)%ymdh
       !write(0,*) 'method2=',out(u)%method2
       !write(0,*) 'model4=',out(u)%model4
       !write(0,*) 'ifcsthour=',ifcsthour

       if(atcferr/=0) then
          ! Stop reading at error or end of file.
          !write(0,*) 'error or end of file'
          exit atcfloop
       endif

       if(ifcsthour<0 .or. ilat<0 .or. ilon<0 .or. iwind<0 .or. ipres<0 .or. &
            (ns/='N' .and. ns/='S') .or. (ew/='E' .and. ew/='W')) then
          ! Skip invalid data
          !write(0,*) 'skip invalid data'
          cycle atcfloop
       endif

       out(u)%fcsthour=real(ifcsthour)

       if(out(u)%fcsthour<=lasttime) then
          ! Skip duplicate lines, which contain different wind radii,
          ! since we don't use them.
          !write(0,*) 'skip duplicate lines'
          cycle atcfloop
       endif

       out(u)%epoch=epochtime(out(u)%ymdh) + 3600.0*out(u)%fcsthour

       !write(0,*) 'epoch=',out(u)%epoch

       out(u)%lat=real(ilat)/10.0
       out(u)%lon=real(ilon)/10.0
       out(u)%wind=real(iwind)
       out(u)%pres=real(ipres)
       out(u)%valid=.TRUE.

       if(ns=='S') out(u)%lat=-out(u)%lat
       if(ew=='W') out(u)%lon=-out(u)%lon

       out(u)%valid=.true.
       used=u

       ! Calculate the storm speed based on the prior location
       if(firsttime) then
          out(u)%stormdir=0.0
          out(u)%stormspeed=0.0
          firsttime=.false.
       else
          sdir=atan2d(out(u)%lon-lastlon,out(u)%lat-lastlat)
          if(sdir<0) sdir=sdir+360.
          out(u)%stormdir=sdir
          sdist=greatarc(out(u)%lat,out(u)%lon,lastlat,lastlon)
          out(u)%stormspeed=sdist/(3600.*real(out(u)%fcsthour-lasttime)) * 1.940
       endif

       out(u)%stormname=' '
       select case(out(u)%basin2)
       case("AL")
          if(out(u)%lat<0) then
             out(u)%basin1='Q'
          else
             out(u)%basin1='L'
          endif
       case("EP")
          out(u)%basin1='E'
       case('CP')
          out(u)%basin1='C'
       case('SL','LS')
          if(out(u)%lat<0) then
             out(u)%basin1='Q'
          else
             out(u)%basin1='L'
          endif
       case('WP')
          out(u)%basin1='W'
       case('IO')
          out(u)%basin1='B'
       case('SH')
          out(u)%basin1='S'
       case default
          out(u)%basin1='X'
       end select

       ! Now that we're done loading this one, make it the "last time read":
       lasttime=out(u)%fcsthour
       lastlat=out(u)%lat
       lastlon=out(u)%lon
    enddo atcfloop

    close(unit)

    return

1001 format (a2,', ',a2,', ',i10.10,', ',a2,', ',a4,', ',i3.3,', ',i3,a1, &
         ', ',i4,a1,', ',i3,', ',i4)
1002 format('Array length is too small in read_atcf.  Ran out of space at ',&
         I0,' lines.')
1003 format(a,': unable to open for reading')
1010 continue
    write(message,1003) filename
    call fail(message)
  end subroutine read_atcf

  ! ------------------------------------------------------------

  subroutine read_htcf(filename,out,len,used,model4,submodel4,tcvitals)
    ! read_htcf -- reads an HTCF file into htcf objects.  Up to "len"
    !       entries are read into "out" stopping at end of file or
    !       error.  The "used" variable will contain the number of
    !       entries read.
    !     filename -- the file to read
    !     out -- the htcf array to store the data
    !     len -- the length of that array
    !     used -- (output) the number of entries used
    implicit none
    integer, intent(in) :: len
    integer, intent(out) :: used
    character*(*), intent(in) :: filename
    type(htcf), intent(out) :: out(len)
    type(htcf) :: consthtcf
    character*1 :: pns,pew,wns,wew,cns,cew
    integer :: u,htcferr, unit
    character*1000 :: message
    logical :: use_linepre
    character*(4), intent(in), optional :: model4,submodel4
    type(atcf), intent(in), optional :: tcvitals

    unit=get_unit()
    open(unit=unit,file=trim(filename),status='OLD',err=1110)

    use_linepre=.false.
    if((present(model4) .neqv. present(submodel4)) .or. &
         (present(model4) .neqv. present(tcvitals))) then
       call fail('In read_htcf you must specify all of model4, submodel4 and tcvitals, or none of them.')
    elseif(present(model4)) then
       use_linepre=.true.
       out(1)%basin2=tcvitals%basin2
       out(1)%storm2=tcvitals%storm2
       out(1)%ymdh=tcvitals%ymdh
       out(1)%minute=0
       out(1)%model4=model4
       out(1)%submodel4=submodel4
    endif

    htcferr=0
    used=0
    htcfloop: do while(htcferr==0)
       u=used+1
       if(u>len) then
          write(message,1102) len
          call warn(message)
          return ! stop reading if we run out of space
       end if
       readin: if(use_linepre) then
          read(unit,1132,iostat=htcferr) out(u)%fcstsec, &
               out(u)%pval,out(u)%plat,pns,out(u)%plon,pew, &
               out(u)%wval,out(u)%wlat,wns,out(u)%wlon,wew, &
               out(u)%clat,cns,out(u)%clon,cew
          if(u>1) then
             out(u)%basin2    = out(1)%basin2
             out(u)%storm2    = out(1)%storm2
             out(u)%ymdh      = out(1)%ymdh
             out(u)%minute    = out(1)%minute
             out(u)%model4    = out(1)%model4
             out(u)%submodel4 = out(1)%submodel4
          endif
       else
          read(unit,1100,iostat=htcferr) &
               out(u)%basin2,out(u)%storm2,out(u)%ymdh,out(u)%minute, &
               out(u)%model4,out(u)%submodel4, out(u)%fcstsec, &
               out(u)%pval,out(u)%plat,pns,out(u)%plon,pew, &
               out(u)%wval,out(u)%wlat,wns,out(u)%wlon,wew, &
               out(u)%clat,cns,out(u)%clon,cew
       endif readin

       if(htcferr /= 0) then
          ! Stop reading at error or end of file
          exit htcfloop
       endif

       if(  out(u)%pval<0 .or. out(u)%wval<0 .or. &
            out(u)%plat<0 .or. out(u)%plon<0 .or. &
            out(u)%wlat<0 .or. out(u)%wlon<0 .or. &
            out(u)%clat<0 .or. out(u)%clon<0) then
          ! Skip invalid data
          cycle htcfloop
       end if

       out(u)%epoch=epochtime(out(u)%ymdh) + out(u)%fcstsec

       if(pns == 'S') out(u)%plat=-out(u)%plat
       if(wns == 'S') out(u)%wlat=-out(u)%wlat
       if(cns == 'S') out(u)%clat=-out(u)%clat

       if(pew == 'W') out(u)%plon=-out(u)%plon
       if(wew == 'W') out(u)%wlon=-out(u)%wlon
       if(cew == 'W') out(u)%clon=-out(u)%clon

       out(u)%valid=.true.

       used=u
    enddo htcfloop
    return
1100 format(A2,", ",A2,", ",I10.10,I2.2,", ", &
         A4,", ",A4,", ",F11.2,", ", &
         F8.3,", ",F6.3,A1,", ",F7.3,A1,", ", &
         F7.3,", ",F6.3,A1,", ",F7.3,A1,", ", &
         F6.3,A1,", ",F7.3,A1)
1131 format(A2,", ",A2,", ",I10.10,I2.2,", ", &
         A4,", ",A4,", ")
1132 format(F11.2,", ", &
         F8.3,", ",F6.3,A1,", ",F7.3,A1,", ", &
         F7.3,", ",F6.3,A1,", ",F7.3,A1,", ", &
         F6.3,A1,", ",F7.3,A1)

1102 format('Array length is too small in read_htcf.  Ran out of space at ',&
         I0,' lines.')
1103 format(a,': unable to open for reading')
1110 continue
    write(message,1103) filename
    call fail(message)
  end subroutine read_htcf

  ! ------------------------------------------------------------

  function to_ns(lat) result(ns)
    character*1 :: ns
    real, intent(in) :: lat
    if(lat<0) then
       ns='S'
    else
       ns='N'
    endif
  end function to_ns

  function to_ew(lon) result(ew)
    character*1 :: ew
    real, intent(in) :: lon
    if(lon<0) then
       ew='W'
    else
       ew='E'
    endif
  end function to_ew

  subroutine write_htcf(iunit,htcfs,nhtcf)
    integer,intent(in) :: iunit,nhtcf
    type(htcf), intent(in) :: htcfs(nhtcf)
    integer :: i
    character*1 :: pns,pew,wns,wew,cns,cew
    character*(1),dimension(-2:2),parameter :: ns=(/'S','S','N','N','N'/)
    character*(1),dimension(-2:2),parameter :: ew=(/'W','W','E','E','E'/)

    writeloop: do i=1,nhtcf
       pns=to_ns(htcfs(i)%plat)
       pew=to_ew(htcfs(i)%plon)
       wns=to_ns(htcfs(i)%wlat)
       wew=to_ew(htcfs(i)%wlon)
       cns=to_ns(htcfs(i)%clat)
       cew=to_ew(htcfs(i)%clon)

       write(iunit,1100) &
            htcfs(i)%basin2,htcfs(i)%storm2,htcfs(i)%ymdh,htcfs(i)%minute, &
            htcfs(i)%model4,htcfs(i)%submodel4, htcfs(i)%fcstsec, &
            htcfs(i)%pval,abs(htcfs(i)%plat),pns,abs(htcfs(i)%plon),pew, &
            htcfs(i)%wval,abs(htcfs(i)%wlat),wns,abs(htcfs(i)%wlon),wew, &
            abs(htcfs(i)%clat),cns,abs(htcfs(i)%clon),cew
    enddo writeloop
1100 format(A2,", ",A2,", ",I10.10,I2.2,", ", &
         A4,", ",A4,", ",F11.2,", ", &
         F8.3,", ",F6.3,A1,", ",F7.3,A1,", ", &
         F7.3,", ",F6.3,A1,", ",F7.3,A1,", ", &
         F6.3,A1,", ",F7.3,A1)
  end subroutine write_htcf

  ! ------------------------------------------------------------

  real function greatarc(lat1,lon1,lat2,lon2)
    ! greatarc -- gets the great arc distance (along-Earth distance)
    !       between two points.  The earth radius used in the
    !       calculation is the average of the radius at the two
    !       points.
    !     lat1,lon1 -- the first point's latitude
    !     lat2,lon2 -- the second point's latitude
    !     returns -- the distance in meters.
    use constants_module, only: Requator,pi,DEGRAD,flattening
    implicit none
    real, intent(in) :: lat1,lon1, lat2,lon2
    real :: rlat1,rlon1, rlat2,rlon2
    real :: Rearth1,Rearth2
    real, parameter :: deg2rad=DEGRAD
    real, parameter :: flattening_inv=1/flattening

    rlat1=lat1*deg2rad  ;  rlon1=lon1*deg2rad
    rlat2=lat2*deg2rad  ;  rlon2=lon2*deg2rad

    Rearth1=Requator*(1-sin(rlat1)**2/flattening_inv)
    Rearth2=Requator*(1-sin(rlat2)**2/flattening_inv)

    greatarc=(Rearth1+Rearth2)*asin(min(1.0,sqrt( &
         sin((rlat1-rlat2)/2)**2+ &
         cos(rlat1)*cos(rlat2)*sin((rlon1-rlon2)/2)**2)))
  end function greatarc

  ! ------------------------------------------------------------

  subroutine interp_atcf_to_htcf(hs,nh,as,na,distcutoff)
    implicit none
    integer, intent(in) :: nh,na
    type(atcf), target, intent(in) :: as(na)
    type(htcf), target, intent(inout) :: hs(nh)
    real, intent(in) :: distcutoff
    integer :: ia,ih,old_ih
    real :: w1,w2,wdenom, prevlat,prevlon

    type(atcf), pointer :: a1,a2
    type(htcf), pointer :: h

    if(na<2) &
         call fail("At least two ATCF times are required when running htcfstats.  I will produce no htcfstats or resolution files.")

    ih=1
    h=>hs(ih)
    aloop: do ia=1,na-1
       a1=>as(ia)
       a2=>as(ia+1)
       wdenom=a2%epoch-a1%epoch
       hloop: do while(h%epoch<a2%epoch)
          w2=min(1.0,max(0.0,real(abs((h%epoch-a1%epoch)/wdenom))))
          w1=1.0-w2
          h%atcfpres = a1%pres*w1 + a2%pres*w2
          h%atcfwind = a1%wind*w1 + a2%wind*w2
          h%atcflat  =  a1%lat*w1 +  a2%lat*w2
          h%atcflon  =  a1%lon*w1 +  a2%lon*w2

          h%atcfdist = greatarc(h%clat,h%clon,h%atcflat,h%atcflon)

          if(ih>1) then
             if(prevlat/=h%clat .or. prevlon/=h%clon) then
                h%moved=.true.
                h%movedist=greatarc(prevlat,prevlon,h%clat,h%clon)
             else
                h%moved=.false.
                h%movedist=0.0
             endif
          else
             h%moved=.false.
             h%movedist=0.0
          endif

          prevlat=h%clat
          prevlon=h%clon

          h%valid=h%valid.and.h%atcfdist<distcutoff

          ih=ih+1
          if(ih<=nh) then
             h=>hs(ih)
          else
             ! We ran out of HTCF entries.  Time to stop looping.
             exit aloop
          endif
       enddo hloop
    enddo aloop

    ! If the ATCF track is cut off, mark all HTCF entries after the
    ! last one with ATCF data as invalid.
    old_ih=ih
    do ih=old_ih,nh
       hs(ih)%valid=.false.
    enddo
  end subroutine interp_atcf_to_htcf

  ! ------------------------------------------------------------

  subroutine htcf_stats(hs,nh,as,na,hourstep,filename,resfilename, &
       cutoffs,ncut, moad_resolution, parent_grid_ratio)
    implicit none
    type(atcf), target, intent(in) :: as(na)
    type(htcf), target, intent(in) :: hs(nh)
    real, intent(in) :: cutoffs(ncut)
    integer, intent(in) :: nh,na,ncut,parent_grid_ratio(ncut)
    real, intent(in) :: hourstep, moad_resolution
    character(*), intent(in) :: filename, resfilename

    real, parameter :: s2h=3600.0
    character*1000 :: message

    integer :: unit, ia,ih
    real(kind=8) :: start,time1,time2,now
    real :: minhour,maxhour,hour

    real :: adist,hdist
    integer :: movecount

    real :: cumadist,cumhdist
    integer :: cumcount

    real :: maxdist,maxmaxdist

    real :: resolution
    integer :: runit,ir

    ! HTCFSTATS format:
    ! AL, 12, 2012083018, 03, HWRF,   0,   3,   79,  129,    8,   118.61,    79,   129,     8,   118.61
10  format (a2,', ',a2,', ',i10,', ',a2,', ',a4,', ',i3,', ',i3,', ', &
         i4,', ',i4,', ',i4,', ',f8.2,', ', &
         i5,', ',i5,', ',i5,', ',f8.2)

    ! RESOLUTION format:
    ! AL, 12, 2012083018, 03, HWRF,   0,   3,     3,     118,     8
20  format (a2,', ',a2,', ',i10,', ',a2,', ',a4,', ',i3,', ',i3,', ', &
         i5,', ',i7,', ',i5)

    unit=get_unit()
    open(unit=unit,file=filename,err=1610)
    runit=get_unit()
    open(unit=runit,file=resfilename,err=1620)

    start=epochtime(as(1)%ymdh)
    minhour=nint((as(1)%epoch-start)/s2h)
    maxhour=nint((as(na)%epoch-start+30)/s2h)

    !    print *,'maxhour=',maxhour

    ia=1 ; ih=1
    cumadist=0.0 ; cumhdist=0.0 ; cumcount=0

    maxdist=0.0 ; maxmaxdist=0.0

    write(runit,"(A)") "BS, ID,      CYCLE, MT, MODL, HR1, HR2, RESKM, MAXDIST, MOVES"

    hour=minhour
    statloop: do while(hour<maxhour-1e-3 .and. ih<nh .and. ia<=na)
       !       print *,'LOOP TOP WITH HOUR ',hour
       !       print *,'IA HOUR IS ',as(ia)%fcsthour
       now=start+hour*real(3600.0,kind=8)
       time1=now
       time2=now+hourstep*3600.0

       maxdist=0.0
       movecount=0
       adist=0.0
       hdist=0.0

       htcfmove: do while(hs(ih)%epoch<=time2+10)
          !          print *,'ih=',ih,' atcfdist=',hs(ih)%atcfdist,' maxdist=',maxdist,' fcsthr=',hs(ih)%fcstsec/3600.0
          if(hs(ih)%atcfdist>maxdist) then
             !             print *,'  -- maximum exceeded: ',hs(ih)%atcfdist,">",maxdist
             maxdist=hs(ih)%atcfdist
          endif
          if(hs(ih)%moved) then
             movecount=movecount+1
             hdist=hdist+hs(ih)%movedist/1000.0
             !             print *,'movement at ih=',ih,' distance=',hs(ih)%movedist/1000.0
          endif
          ih=ih+1
          if(ih>=nh) then
             ! This cannot be in the while() since fortran does not
             ! guarantee a short-circuit "and" operator, and we have
             ! to avoid doing hs(nh)%epoch
             exit htcfmove
          endif
       enddo htcfmove

       atcfmove: do while(as(ia)%epoch<=time2+10 .and. ia<na)
          if(ia>1) then
             adist=adist+greatarc(as(ia)%lat,as(ia)%lon, &
                  as(ia-1)%lat,as(ia-1)%lon)/1000.0
          endif
          ia=ia+1
       end do atcfmove
       ia=ia-1

       cumcount=cumcount+movecount
       cumadist=cumadist+adist
       cumhdist=cumhdist+hdist
       if(maxdist>maxmaxdist) maxmaxdist=maxdist

       resolution=moad_resolution
       resloop: do ir=1,ncut
          if(maxdist/1e3>cutoffs(ir)) exit
          resolution=resolution/parent_grid_ratio(ir)
       enddo resloop

       write(unit,10) as(1)%basin2,as(1)%storm2,as(1)%ymdh, &
            as(1)%method2,as(1)%model4,nint(hour),nint(hour+hourstep), &
            nint(adist),nint(hdist),movecount,maxdist/1e3, &
            nint(cumadist),nint(cumhdist),cumcount,maxmaxdist/1e3

       write(runit,20) as(1)%basin2,as(1)%storm2,as(1)%ymdh, &
            as(1)%method2,as(1)%model4,nint(hour),nint(hour+hourstep), &
            ! Bizarre floor use here is to mimic the old *.resolution files:
            floor(resolution),floor(maxdist/1e3+0.005),movecount

       hour=hour+hourstep
    enddo statloop

    return

1601 format(a,': unable to open htcf stats output file')
1603 format(a,': unable to open htcf resolution stats output file')
1610 write(message,1601) filename
    call fail(message)
1620 write(message,1603) resfilename
    call fail(message)

  end subroutine htcf_stats

end module tcf_module
