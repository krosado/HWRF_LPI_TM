program stats
  implicit none
  type atcf
     ! Usual ATCF file information:
    character*2 :: basin2,method2,storm2
    character*4 :: model4
    integer :: ymdh
    real :: fcsthour, lat,lon,wind,pres

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

  type data
     integer :: N
     real(kind=8) :: reltime
     integer, pointer :: order(:)
     real, pointer :: time(:),wind(:),wlat(:),wlon(:),pres(:),plat(:),plon(:),dist(:),clat(:),clon(:)
     real, pointer :: stime(:),swind(:),swlat(:),swlon(:),spres(:),splat(:),splon(:),sdist(:),sclat(:),sclon(:)
  end type data

  ! ------------------------------------------------------------

  ! Local variables

  ! Maxatcf: enough for 126 hours of per-minute output, plus some padding:
  integer, parameter :: maxatcf=126*60 + 100
  ! Maxhtcf: enough for one second output for 126 hours, plus some padding:
  integer, parameter :: maxhtcf=126*3600 + 1000
  type(atcf) :: atcfdat(maxatcf)
  type(htcf) :: htcfdat(maxhtcf)
  integer :: na,nh
  character(1000) :: inatcf,inhtcf,outprefix

  real, parameter :: distcutoff = 150000
  real, parameter :: trackwidth=1800.0, intenswidth=1800.0
  real, parameter :: windpercentile=95.0, prespercentile=5.0
!  real, parameter :: statsstep=3.0,trackstep=3.0

  character*20 :: validity
  integer :: i
  real :: hourstep

  ! ------------------------------------------------------------

  ! Main program:

  call getarg_check(1,inatcf,'Argument 1 (path to ATCF file) is missing')  
  call getarg_check(2,inhtcf,'Argument 2 (path to HTCF file) is missing')
  call getarg_check(3,outprefix,'Argument 3 (output file path and prefix) is missing')

101 format(A,': "',A,'"')
  write(0,101) 'ATCF input file',trim(inatcf)
  write(0,101) 'HTCF input file',trim(inhtcf)
  write(0,101) 'output prefix',trim(outprefix)

  call read_atcf(trim(inatcf),atcfdat,maxatcf,na)

!   print *,'ATCF file had ',na,' lines.'
!   do i=1,na
! 105 format("i=",i2,": ",a2,', ',a2,', ',a2,', ',a4,', ',i10.10,', fcsthour=',f8.3,', epoch=',f14.3,' lat=',f8.3,' lon=',f8.3,', blah blah blah')
!      print 105,i,atcfdat(i)%basin2,atcfdat(i)%method2,atcfdat(i)%storm2, &
!           atcfdat(i)%model4,atcfdat(i)%ymdh,atcfdat(i)%fcsthour, &
!           atcfdat(i)%epoch,atcfdat(i)%lat,atcfdat(i)%lon
!   enddo

  call read_htcf(trim(inhtcf),htcfdat,maxhtcf,nh)


  call interp_atcf_to_htcf(htcfdat,nh,atcfdat,na,distcutoff)

!   print *,'HTCF file had ',nh,' lines.'
!   do i=1,nh ! 1,10
! 110  format("i=",i7,": ",a2,', ',a2,', ',a4,', ',i10.10,', fcstsec=',f9.3,', fcsthour=',f5.1,', epoch=',f14.3,' hlat=',f8.3,' hlon=',f8.3,' alat=',f8.3,' alon=',f8.3,', atcfdist=',f12.3,', ',a)
!      if(htcfdat(i)%valid) then
!         validity='valid'
!      else
!         validity='!! INVALID !!'
!      endif
!       print 110,i,htcfdat(i)%basin2,htcfdat(i)%storm2, &
!            htcfdat(i)%model4,htcfdat(i)%ymdh,htcfdat(i)%fcstsec, &
!            htcfdat(i)%fcstsec/3600.0, &
!            htcfdat(i)%epoch,htcfdat(i)%clat,htcfdat(i)%clon, &
!            htcfdat(i)%atcflat,htcfdat(i)%atcflon,htcfdat(i)%atcfdist, &
!            trim(validity)
!   enddo
  ! print *,'... more junk ...'
  ! do i=max(11,nh-9),nh
  !     print 110,i,htcfdat(i)%basin2,htcfdat(i)%storm2, &
  !          htcfdat(i)%model4,htcfdat(i)%ymdh,htcfdat(i)%fcstsec, &
  !          htcfdat(i)%epoch,htcfdat(i)%clat,htcfdat(i)%clon, &
  !          htcfdat(i)%atcfdist
  ! enddo

  hourstep=real(atcfdat(2)%epoch-atcfdat(1)%epoch)/3600.0

  call htcf_stats(htcfdat,nh,atcfdat,na,hourstep, &
       trim(outprefix)//'.htcfstats')

!  call htcf_tracks(htcfdat,nh,atcfdat,nh, &
!       hourstep,trackwidth,intenswidth, &
!       windpercentile,prespercentile, &
!       trim(outprefix)//'.windatcf', &
!       trim(outprefix)//'.presatcf', &
!       trim(outprefix)//'.centatcf')

contains

  ! ------------------------------------------------------------

  subroutine fail(why)
    ! fail -- quits the program with an error message, after trim()ing it
    !   message -- an error message
    ! Does not return.
    implicit none
    character*(*),intent(in) :: why
    write(0,*) trim(why)
    stop 13
  end subroutine fail

  ! ------------------------------------------------------------
  subroutine warn(warning)
    ! warn -- prints a warning to the log file or unit after
    !   trimming it
    implicit none
    character(*), intent(in) :: warning
    write(0,*) trim(warning)
  end subroutine warn

  ! ------------------------------------------------------------

  subroutine mergesort(N,data,idx)
    implicit none
    integer, intent(in) :: N
    real, intent(in) :: data(N)
    integer, intent(inout) :: idx(N)
    integer :: i
    
    do i=1,N
       idx(i)=i
    end do
    call mergesort_impl(N,data,idx,1,N)
  end subroutine mergesort

  recursive subroutine mergesort_impl(N,data,idx,n1,n2)
    implicit none
    integer, intent(in) :: N,n1,n2
    real, intent(in) :: data(N)
    integer, intent(inout) :: idx(N)
    integer :: mid,temp,isorted,ileft,iright,iresult
    integer :: result(n1:n2)

    ! Handle lists of length 1 or 2
    if(n1==n2) return
    if(n1+1==n2) then
       if(data(idx(n1))>data(idx(n2))) then
          temp=idx(n1)
          idx(n1)=idx(n2)
          idx(n2)=temp
       endif
       return
    endif

    ! Split list in half
    mid=(n1+n2)/2

    ! Sort left and right halves
    if(mid>n1) call mergesort_impl(N,data,idx,n1,mid)
    if(mid+1<n2) call mergesort_impl(N,data,idx,mid+1,n2)

    ! Merge left and right halves in a temporary array
    ileft=n1
    iright=mid+1
    iresult=n1
    do while(ileft<=mid .or. iright<=n2)
       if(ileft<=mid .and. iright<=n2) then
          if(data(idx(ileft))<=data(idx(iright))) then
             result(iresult)=idx(ileft)
             ileft=ileft+1
             iresult=iresult+1
          else
             result(iresult)=idx(iright)
             iright=iright+1
             iresult=iresult+1
          endif
       elseif(ileft<=mid) then
          result(iresult)=idx(ileft)
          ileft=ileft+1
          iresult=iresult+1
       else
          result(iresult)=idx(iright)
          iright=iright+1
          iresult=iresult+1
       endif
    enddo

    ! Copy the temporary array back to the index array
    idx(n1:n2)=result
  end subroutine mergesort_impl

  ! ------------------------------------------------------------

  function epochtime(ymdh)
    ! epochtime -- returns the unix epoch time for a YYYYMMDDHH time.
    !          This is a simple wrapper around the C etime subroutine.
    !   ymdh -- an integer of the form YYYYMMDDHH specifying a date/time
    !   returns -- number of seconds since midnight, January 1, 1970 UTC
    implicit none
    logical, save :: inited=.false.
    integer, intent(in) :: ymdh
    real(kind=8) :: epochtime
    integer(kind=8) :: temp
    integer :: err

    if(.not.inited) then
       call etimeinit(err)
       inited=.true.
    endif

    call etime(ymdh,temp)
    epochtime=real(temp,kind=8)
  end function epochtime

  ! ------------------------------------------------------------

  function ymdhtime(epoch)
    ! ymdhtime -- returns an integer YYYYMMDDHH time for a unix epoch time
    !       This is a simple wrapper around the C ytime subroutine.
    !     epoch -- integer number of seconds since midnight, January 1, 1970 UTC
    !     returns -- the equivalent integer YYYYMMDDHH time
    implicit none
    real(kind=8), intent(in) :: epoch
    integer :: ymdhtime
    integer(kind=8) :: temp
    temp=nint(epoch,kind=8)
    call ytime(temp,ymdhtime)
  end function ymdhtime

  ! ------------------------------------------------------------
  
  integer function get_unit()
    implicit none
    logical :: opened
    integer, parameter :: start=100,stop=1000

    get_unit=-1
    do get_unit=start,stop
       inquire(unit=get_unit,opened=opened)
       if(.not.opened) return
    enddo
    call fail('Unable to find an unused unit number in htcfstats.')
  end function get_unit

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
    real :: lasttime
    character*1 :: ns,ew
    character*200 :: message

    unit=get_unit()
    open(unit=unit,file=trim(filename),status='OLD',err=1010)

    atcferr=0
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

       lasttime=out(u)%fcsthour
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
       !write(0,*) 'used=',used
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

  subroutine read_htcf(filename,out,len,used)
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
    character(*), intent(in) :: filename
    type(htcf), intent(out) :: out(len)
    character*1 :: pns,pew,wns,wew,cns,cew
    integer :: u,htcferr, unit
    character*1000 :: message

    unit=get_unit()
    open(unit=unit,file=trim(filename),status='OLD',err=1110)

    htcferr=0
    used=0
    htcfloop: do while(htcferr==0)
       u=used+1
       if(u>len) then
          write(message,1102) len
          call warn(message)
          return ! stop reading if we run out of space
       end if
       read(unit,1100,iostat=htcferr) &
            out(u)%basin2,out(u)%storm2,out(u)%ymdh,out(u)%minute, &
            out(u)%model4,out(u)%submodel4, out(u)%fcstsec, &
            out(u)%pval,out(u)%plat,pns,out(u)%plon,pew, &
            out(u)%wval,out(u)%wlat,wns,out(u)%wlon,wew, &
            out(u)%clat,cns,out(u)%clon,cew

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
1102 format('Array length is too small in read_htcf.  Ran out of space at ',&
         I0,' lines.')
1103 format(a,': unable to open for reading')
1110 continue
    write(message,1103) filename
    call fail(message)
  end subroutine read_htcf

  ! ------------------------------------------------------------

  real function greatarc(lat1,lon1,lat2,lon2)
    ! greatarc -- gets the great arc distance (along-Earth distance)
    !       between two points.  The earth radius used in the
    !       calculation is the average of the radius at the two
    !       points.
    !     lat1,lon1 -- the first point's latitude
    !     lat2,lon2 -- the second point's latitude
    !     returns -- the distance in meters.
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
         call fail("At least two ATCF times are required when running htcfstats.")

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

  subroutine reorder(N,indat,outdat,order)
    implicit none
    integer, intent(in) :: N
    real, intent(in) :: indat(N)
    real, intent(out) :: outdat(N)
    integer, intent(in) :: order(N)
    integer :: i

    do i=1,N
       outdat(i)=indat(order(i))
    enddo
  end subroutine reorder

  ! ------------------------------------------------------------

  function htcf_get(hs,nh,epoch1in,epoch2in,reltime,locs,vals,sort) result(out)
    implicit none
    type(data) :: out
    integer, intent(in) :: nh
    type(htcf), intent(in) :: hs(nh)
    real(kind=8),intent(in) :: epoch1in,epoch2in,reltime
    logical, intent(in) :: locs,vals,sort
    real(kind=8) :: epoch1,epoch2
    integer :: bound1,bound2,middle
    integer :: start,count,i,j

    epoch1=epoch1in ; epoch2=epoch2in

    if(epoch1<hs(1)%epoch) then
       epoch1=hs(1)%epoch
    endif
    if(epoch2>hs(nh)%epoch) then
       epoch2=hs(nh)%epoch
    endif

    ! Do a binary search to find epoch1 in hs(*)%epoch
    bound1=1
    bound2=nh
    do while(bound2>bound1)
       middle=(bound1+bound2)/2
       if(hs(middle)%epoch>epoch1) then
          bound2=middle-1
       endif
    enddo
    start=bound1

    ! Find the number of valid elements
    count=0
    do i=start,nh
       if(hs(i)%epoch>epoch2) exit
       if(hs(i)%valid) count=count+1
    enddo

    ! Allocate needed space for unsorted data
    out%reltime=reltime
    out%N=count
    allocate(out%time(count),out%stime(count))
    allocate(out%order(count))
    if(locs) then
       allocate(out%wlat(count),out%wlon(count))
       allocate(out%swlat(count),out%swlon(count))
       allocate(out%plat(count),out%plon(count))
       allocate(out%splat(count),out%splon(count))
    else
       nullify(out%wlat) ; nullify(out%swlat)
       nullify(out%wlon) ; nullify(out%swlon)
       nullify(out%plat) ; nullify(out%splat)
       nullify(out%plon) ; nullify(out%splon)
       nullify(out%clat) ; nullify(out%sclat)
       nullify(out%clon) ; nullify(out%sclon)
    endif
    if(vals) then
       allocate(out%wind(count),out%pres(count),out%dist(count))
       allocate(out%swind(count),out%spres(count),out%sdist(count))
    else
       nullify(out%wind,out%pres,out%dist)
       nullify(out%swind,out%spres,out%sdist)
    end if

    ! Grab data.
    j=1
    do i=start,nh
       if(hs(i)%valid) then
          out%time(j)=real(hs(i)%epoch-reltime)
          if(locs) then
             out%wlat(j)=hs(i)%wlat
             out%wlon(j)=hs(i)%wlon
             out%plat(j)=hs(i)%plat
             out%plon(j)=hs(i)%plon
             out%clat(j)=hs(i)%clat
             out%clon(j)=hs(j)%clon
          endif
          if(vals) then
             out%wind(j)=hs(i)%wval
             out%pres(j)=hs(j)%pval
             out%dist(j)=hs(j)%atcfdist
          endif

          j=j+1
       endif
    enddo

    if(sort) then
       ! Find the sorting indices of the time array
       call mergesort(out%N,out%time,out%order)
       
       ! Create ordered data arrays
       call reorder(out%N,out%time,out%stime,out%order)
       if(locs) then
          call reorder(out%N,out%wlat,out%swlat,out%order)
          call reorder(out%N,out%wlon,out%swlon,out%order)
          call reorder(out%N,out%plat,out%splat,out%order)
          call reorder(out%N,out%plon,out%splon,out%order)
       endif
       if(vals) then
          call reorder(out%N,out%wind,out%swind,out%order)
          call reorder(out%N,out%pres,out%spres,out%order)
          call reorder(out%N,out%dist,out%sdist,out%order)
       endif
    endif
  end function htcf_get

  ! ------------------------------------------------------------

  subroutine free_data(D)
    implicit none
    type(data), intent(inout) :: D
    
    if(associated(D%time)) then
       deallocate(D%time)
       nullify(D%time)
    end if

    if(associated(D%wind)) then
       deallocate(D%wind)
       nullify(D%wind)
    end if

    if(associated(D%wlat)) then
       deallocate(D%wlat)
       nullify(D%wlat)
    end if

    if(associated(D%wlon)) then
       deallocate(D%wind)
       nullify(D%wlon)
    end if

    if(associated(D%pres)) then
       deallocate(D%wind)
       nullify(D%pres)
    end if

    if(associated(D%plat)) then
       deallocate(D%wind)
       nullify(D%plat)
    end if

    if(associated(D%plon)) then
       deallocate(D%wind)
       nullify(D%plon)
    end if

    if(associated(D%dist)) then
       deallocate(D%wind)
       nullify(D%dist)
    end if

    if(associated(D%stime)) then
       deallocate(D%wind)
       nullify(D%stime)
    end if

    if(associated(D%swind)) then
       deallocate(D%wind)
       nullify(D%swind)
    end if

    if(associated(D%swlat)) then
       deallocate(D%wind)
       nullify(D%swlat)
    end if

    if(associated(D%swlon)) then
       deallocate(D%wind)
       nullify(D%swlon)
    end if

    if(associated(D%spres)) then
       deallocate(D%wind)
       nullify(D%spres)
    end if

    if(associated(D%splat)) then
       deallocate(D%wind)
       nullify(D%splat)
    end if

    if(associated(D%splon)) then
       deallocate(D%wind)
       nullify(D%splon)
    end if

    if(associated(D%sdist)) then
       deallocate(D%wind)
       nullify(D%sdist)
    end if

    if(associated(D%clat)) then
       deallocate(D%wind)
       nullify(D%clat)
    end if

    if(associated(D%clon)) then
       deallocate(D%wind)
       nullify(D%clon)
    end if

    if(associated(D%sclat)) then
       deallocate(D%wind)
       nullify(D%sclat)
    end if

    if(associated(D%sclon)) then
       deallocate(D%wind)
       nullify(D%sclon)
    end if

  end subroutine free_data

  ! ------------------------------------------------------------

  real function meanreal(N,data)
    implicit none
    integer, intent(in) :: N
    real, intent(in) :: data(N)
    real(kind=8) :: sum
    integer :: i

    sum=0
    do i=1,N
       sum=sum+real(data(i),kind=8)
    enddo
    meanreal=real(sum/real(N,kind=8))
  end function meanreal

  ! ------------------------------------------------------------

  subroutine htcf_tracks(hs,nh,as,na,hourstep,trackwidth,intenswidth, &
                         windpercentile,prespercentile, &
                         windfile,presfile,centfile)
    implicit none
    character*1, parameter :: snn(-1:1) = (/ 'S','N','N' /)
    character*1, parameter :: wee(-1:1) = (/ 'W','E','E' /)

    type(atcf), target, intent(in) :: as(na)
    type(htcf), target, intent(in) :: hs(nh)
    integer, intent(in) :: na,nh
    character(*), intent(in), optional :: windfile,presfile,centfile
    real, intent(in) :: windpercentile, prespercentile
    real, intent(in) :: hourstep, trackwidth, intenswidth

    integer :: iwindlat,iwindlon,ipreslat,ipreslon,icentlat,icentlon
    type(data) :: data
    integer :: ndata
    real(kind=8), parameter :: s2h=3600.0
    real(kind=8) :: start,now
    real :: hour,minhour,maxhour

    character*1 :: windns,windew,presns,presew,centns,centew

    real :: windlat,windlon, preslat,preslon, centlat,centlon
    real :: windpeak,prespeak

    character*500 :: message
    integer :: windunit,presunit,centunit

    if(present(windfile)) then
       windunit=get_unit()
       open(unit=windunit,file=windfile,err=1510)
    end if
    if(present(presfile)) then
       presunit=get_unit()
       open(unit=presunit,file=presfile,err=1520)
    endif
    if(present(centfile)) then
       centunit=get_unit()
       open(unit=centunit,file=centfile,err=1530)
    endif

    start=epochtime(as(1)%ymdh)
    minhour=nint((as(1)%epoch-start)/s2h)
    maxhour=nint((as(na)%epoch-start)/s2h)

    hour=minhour
    hourloop: do while(hour<=maxhour)
       now=start+hour*real(3600.0,kind=8)

       ! Get the "peak" wind and pressure:
       data=htcf_get(hs,nh,now-intenswidth/2,now+intenswidth/2,now, &
            locs=.false.,vals=.true.,sort=.true.)
       windpeak=data%swind(1+nint(windpercentile/100.0*data%N))
       prespeak=data%spres(1+nint(prespercentile/100.0*data%N))
       call free_data(data)

       ! Get the wind, pressure and center positions:
       data=htcf_get(hs,nh,now-trackwidth/2,now+trackwidth/2,now, &
            locs=.true.,vals=.false.,sort=.false.)

       ! Calculate the mean locations and output the track
       if(present(windfile)) then
          iwindlat=nint(meanreal(data%N,data%wlat)*10.0)
          iwindlon=nint(meanreal(data%N,data%wlon)*10.0)
          write(windunit,1500) as(1)%basin2,as(1)%storm2,as(1)%ymdh, &
               as(1)%method2,as(1)%model4,nint(hour), &
               abs(iwindlat),snn(sign(1,iwindlat)), &
               abs(iwindlon),wee(sign(1,iwindlon)), &
               windpeak,prespeak
       endif

       if(present(presfile)) then
          ipreslat=nint(meanreal(data%N,data%plat)*10.0)
          ipreslon=nint(meanreal(data%N,data%plon)*10.0)
          write(presunit,1500) as(1)%basin2,as(1)%storm2,as(1)%ymdh, &
               as(1)%method2,as(1)%model4,nint(hour), &
               abs(ipreslat),snn(sign(1,ipreslat)), &
               abs(ipreslon),wee(sign(1,ipreslon)), &
               windpeak,prespeak
       endif

       if(present(centfile)) then
          icentlat=nint(meanreal(data%N,data%clat)*10.0)
          icentlon=nint(meanreal(data%N,data%clon)*10.0)
          write(centunit,1500) as(1)%basin2,as(1)%storm2,as(1)%ymdh, &
               as(1)%method2,as(1)%model4,nint(hour), &
               abs(icentlat),snn(sign(1,icentlat)), &
               abs(icentlon),wee(sign(1,icentlon)), &
               windpeak,prespeak
       endif

       call free_data(data)
       hour=hour+hourstep
    enddo hourloop

    if(present(windfile)) close(windunit)
    if(present(presfile)) close(presunit)
    if(present(centfile)) close(centunit)

    return

1500 format (a2,', ',a2,', ',i10.10,', ',a2,', ',a4,', ',i3.3,', ',i3,a1, &
          ', ',i4,a1,', ',i3,', ',i4)
1505 format(a,': unable to open ',a,' track output file')

1510 write(message,1505) windfile,'wind'
     call fail(message)

1520 write(message,1505) presfile,'pres'
     call fail(message)

1530 write(message,1505) centfile,'domain center'
     call fail(message)
  end subroutine htcf_tracks

  ! ------------------------------------------------------------

  subroutine htcf_stats(hs,nh,as,na,hourstep,filename)
    implicit none
    type(atcf), target, intent(in) :: as(na)
    type(htcf), target, intent(in) :: hs(nh)
    integer, intent(in) :: nh,na
    real, intent(in) :: hourstep
    character(*), intent(in) :: filename

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

    unit=get_unit()
    open(unit=unit,file=filename,err=1610)
    
    start=epochtime(as(1)%ymdh)
    minhour=nint((as(1)%epoch-start)/s2h)
    maxhour=nint((as(na)%epoch-start+30)/s2h)

!    print *,'maxhour=',maxhour
    
    ia=1 ; ih=1
    cumadist=0.0 ; cumhdist=0.0 ; cumcount=0

    maxdist=0.0 ; maxmaxdist=0.0

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

       write(unit,1602) as(1)%basin2,as(1)%storm2,as(1)%ymdh, &
            as(1)%method2,as(1)%model4,nint(hour),nint(hour+hourstep), &
            nint(adist),nint(hdist),movecount,maxdist/1e3, &
            nint(cumadist),nint(cumhdist),cumcount,maxmaxdist/1e3

       hour=hour+hourstep
    enddo statloop

    return

1601 format(a,': unable to open htcf stats output file')
1602 format (a2,', ',a2,', ',i10,', ',a2,', ',a4,', ',i3,', ',i3,', ', &
          i4,', ',i4,', ',i4,', ',f8.2,', ', &
          i5,', ',i5,', ',i5,', ',f8.2)
1610 write(message,1601) filename
     call fail(message)

  end subroutine htcf_stats

  subroutine getarg_check(arg,str,err)
    integer, intent(in) :: arg
    character*(1000), intent(out) :: str
    character(*), intent(in) :: err
    character*(1000), parameter :: errtext='**** UNINITIALIZED ****'

    str=errtext
    call getarg(arg,str)
    if(str==errtext .or. str==' ') then
       call fail(err)
    endif
  end subroutine getarg_check

end program stats
