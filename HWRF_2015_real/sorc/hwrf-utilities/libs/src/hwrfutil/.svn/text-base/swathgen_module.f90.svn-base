module swathgen_module
  use sysutil_module, only: fail, warn
  use wrfdiag_module!, only: wrfdiag_file, init_wrfdiag_file, wrfdiag_var
  use vardata_module!, only: vardata_real, init_vardata_real
  use projection_module !, only: proj_nmme, proj_latlon, init_proj_latlon
  use decomp_module !, only: decomp
  use interp_module !, only: bilinear_real, init_bilinear_real

  implicit none

  private
  public :: swathgen, init_swathgen

  type swathgen
     type(wrfdiag_file), pointer :: files(:) => NULL()
     type(bilinear_real), pointer :: interp(:) => NULL()
     type(wrfdiag_var), pointer :: precip_vars(:) => NULL()
     type(wrfdiag_var), pointer :: wind_vars(:) => NULL()
     type(vardata_real) :: wind,precip,lats,lons
     type(proj_latlon) :: proj
     type(decomp) :: dec
     real, pointer :: track_lat(:)=>NULL(), track_lon(:)=>NULL(), &
          track_wind(:)=>NULL()
     real :: file_dtime, nauxphr
     integer :: maxnfile=0, nfiles=0, itime=0, ntimes=0, nx=0, ny=0
     logical :: have_proj=.false., inited=.false., did_first_interp=.false.
     logical :: in_crazy_us_units=.false., have_track=.false.
   contains
     procedure :: add_file
     procedure, private :: first_file
     procedure :: free => free_swathgen
     procedure :: setup_proj
     procedure :: interp_time
     procedure :: write
     procedure :: to_crazy_us_units
     procedure :: discard_track
     procedure :: get_track
  end type swathgen

contains

  subroutine to_crazy_us_units(this)
    ! Convert winds to knots and precip to inches
    class(swathgen), intent(inout) :: this
    real, parameter :: mps2knots=3600./1852. ! exact
    real, parameter :: meters2inches=100./2.54 ! exact
    integer :: i,j

    if(this%in_crazy_us_units) return
    !$OMP PARALLEL DO PRIVATE(i,j)
    do  j = 1 , this%ny
       do  i = 1 , this%nx
          this%wind%rdata(i,j,1)=this%wind%rdata(i,j,1)*mps2knots
          this%precip%rdata(i,j,1)=this%precip%rdata(i,j,1)*meters2inches
       enddo
    enddo
    !$OMP END PARALLEL DO
    this%in_crazy_us_units=.true.
  end subroutine to_crazy_us_units

  subroutine discard_track(this)
    class(swathgen), intent(inout) :: this
    ! Do not deallocate track: it is allocated in another object.
    nullify(this%track_lat,this%track_wind,this%track_lon)
  end subroutine discard_track

  subroutine get_track(this)
    class(swathgen), intent(inout) :: this
    integer :: i
    if(this%have_track) return
    call this%discard_track()
    fileloop: do i=1,this%nfiles
       if(this%files(i)%have_track) then
          if(size(this%files(i)%track_lat)>=this%ntimes) then
             this%track_lat=>this%files(i)%track_lat
             this%track_lon=>this%files(i)%track_lon
             this%track_wind=>this%files(i)%track_wind
             this%have_track=.true.
             exit fileloop
          endif
       endif
    enddo fileloop

    if(.not. this%have_track) then
       call fail('swathgen: in get_track: no input files have valid internal HWRF tracker information.  Did you forget to include domain 3?  Or did you forget to set vortex_tracker=6 or 7 in your namelist.input?')
    endif

  end subroutine get_track

  subroutine write(this,outprefix,track,ntrack,grads_byteswap)
    use tcf_module, only: atcf
    use sysutil_module, only: get_unit
    ! This subroutine writes out the various swath output files.  This
    ! is translated from Bob Tuleya's original source code.  It now
    ! uses the internal HWRF tracker, and the swathgen output swath
    ! data.  The external tracker's ATCF track is only used to decide
    ! the cutoff hour, beyond which to produce no swath.
    class(swathgen), intent(inout) :: this
    character*(*), intent(in) :: outprefix
    type(atcf), intent(in) :: track(ntrack)
    integer, intent(in) :: ntrack
    logical, intent(in) :: grads_byteswap
    integer :: iwindhrly,iwind10m,irainfall, i,j, iswathdat,igrads
    integer :: lastitime,itime,ihour, nx,ny, ifile
    real :: lat,lon,tmns, hour, lasthour

    if(ntrack<1) return ! no ATCF, so no storm
    lasthour=track(ntrack)%fcsthour*this%nauxphr
    if(lasthour<1e-5) return ! no forecast, so no swath

    call this%get_track() ! find hourly track from internal HWRF tracker

    iwindhrly=get_unit()
    open(iwindhrly,file=trim(outprefix)//'wind10hrly.ascii',form='FORMATTED',status='UNKNOWN')

    iwind10m=get_unit()
    open(iwind10m,file=trim(outprefix)//'wind10m.ascii',form='FORMATTED',status='UNKNOWN')

    irainfall=get_unit()
    open(irainfall,file=trim(outprefix)//'rainfall.ascii',form='FORMATTED',status='UNKNOWN')

    iswathdat=get_unit()
    open(iswathdat,file=trim(outprefix)//'swath.dat',form='unformatted', &
         ACCESS='DIRECT',RECL=(this%nx*this%ny*4),STATUS='UNKNOWN')

    igrads=get_unit()
    open(igrads,file=trim(outprefix)//'swath.ctl',form='FORMATTED', &
         status='UNKNOWN')

    nx=this%nx
    ny=this%ny

    ! Fill wind and precip with badval outside the mask:

    !$OMP PARALLEL DO PRIVATE(i,j)
    badvalloop: do j=1,this%ny
       do i=1,this%nx
          if(.not.this%wind%mask(i,j,1)) this%wind%rdata(i,j,1)=-999
          if(.not.this%precip%mask(i,j,1)) this%precip%rdata(i,j,1)=-999
       enddo
    enddo badvalloop
    lastitime=this%ntimes
    ! Print the max wind for forecast hours, and find the last index
    ! with a forecast time that is at or before the last ATCF time.
    windloop: do itime=2,this%ntimes
       hour=(itime-1) / this%nauxphr
       if(hour>lasthour) then
          lastitime=itime-1
          exit windloop
       endif
       write(iwindhrly,2900) hour, this%track_wind(itime)
2900   format(1x,'HOUR: ', f5.1,5x, 'MAX SURF WIND (KNOTS): ', f5.1)
    enddo windloop
    
    ! Original comment from Bob Tuleya:
    !     now write data out to text file for NHC
    !     make the ascii files for Michelle's color plots
    write(iwind10m,3332) this%proj%lon1, this%proj%lon1+this%proj%dlon*(nx-1), &
         this%proj%lat1, this%proj%lat1+this%proj%dlat*(ny-1), &
         (this%proj%dlon+this%proj%dlat)/2., this%nx,this%ny
    write(irainfall,3332) this%proj%lon1, this%proj%lon1+this%proj%dlon*(nx-1), &
         this%proj%lat1, this%proj%lat1+this%proj%dlat*(ny-1), &
         (this%proj%dlon+this%proj%dlat)/2., this%nx,this%ny
3333 format(3f9.2)
3332 format(5f9.2,2i5)
    ! Write the lats & lons to the wind10m and rainfall files for the
    ! color plots.  Note no OpenMP here since it is a write loop.

    !$OMP PARALLEL DO PRIVATE(i,j,ifile) DEFAULT(SHARED) SCHEDULE(STATIC)
    do ifile=1,4
       if(ifile<3) then
          do  j = 1 , ny
             do  i = 1 , nx
                if(ifile==1) then
                   write(iwind10m,3333) this%lats%rdata(i,j,1),this%lons%rdata(i,j,1), &
                        this%wind%rdata(i,j,1)
                else
                   write(irainfall,3333) this%lats%rdata(i,j,1),this%lons%rdata(i,j,1), &
                        this%precip%rdata(i,j,1)
                endif
             enddo
          enddo
       elseif(ifile==3) then
          ! Output the swath.dat binary file used for plotting swath data
          write(iswathdat,rec=1) this%wind%rdata
          write(iswathdat,rec=2) this%precip%rdata
          close(iswathdat)
       elseif(ifile==4) then
          ! Output a GrADS control file:
          write(igrads,*)'DSET ^'//trim(outprefix)//'swath.dat'
          write(igrads,*)'TITLE 10m max wind and rain'
          write(igrads,*)'UNDEF -999'
          if(grads_byteswap) then
             write(igrads,*)'OPTIONS byteswapped'
          endif
          write(igrads,9100)this%nx,this%proj%lon1,this%proj%dlon
9100      format(' XDEF ',I5,' LINEAR ',F7.2,1x,F6.3)
          write(igrads,9101)this%ny,this%proj%lat1,this%proj%dlat
9101      format(' YDEF ',I5,' LINEAR ',F7.2,1x,F6.3)
          write(igrads,*)'ZDEF 1 LEVELS 0'
          write(igrads,*)'tdef 1 linear 00Z01JAN2011 1mon'
          write(igrads,*)'VARS 2'
          if(this%in_crazy_us_units) then
             write(igrads,*)'wind10 0 99 10-m max wind (kt)'
             write(igrads,*)'rain   0 99 accumulated precipitation(IN)'
          else
             write(igrads,*)'wind10 0 99 10-m max wind (m/s)'
             write(igrads,*)'rain   0 99 accumulated precipitation(m)'
          endif
          write(igrads,*)'ENDVARS'
          close(igrads)
       endif
    enddo
    !$OMP END PARALLEL DO

    tmns = -99.0
    write(iwind10m,3333)tmns,tmns,tmns
    write(irainfall,3333)tmns,tmns,tmns
    !     include duration & track of storm  
    write(iwind10m,3335)nint(lasthour)
    write(irainfall,3335)nint(lasthour)
3335 format(i5)
    do i = 2 , lastitime
       write(iwind10m,3334) this%track_lat(i),this%track_lon(i)
       write(irainfall,3334) this%track_lat(i),this%track_lon(i)
3334   format(2f8.2)
    enddo

    close(iwindhrly)
    close(iwind10m)
    close(irainfall)

    call make_pgm('debug-wind.pgm',this%nx,this%ny,this%wind%rdata,this%wind%mask,0.,4.)
    call make_pgm('debug-precip.pgm',this%nx,this%ny,this%precip%rdata,this%precip%mask,0.,200.)
  end subroutine write

  subroutine make_pgm(filename,nx,ny,var,mask,sub,scale)
    implicit none
    character*(*), intent(in) :: filename
    integer(kind=1) :: bytes(nx,ny)
    real, intent(in) :: var(nx,ny), sub,scale
    integer, intent(in) :: nx,ny
    logical, intent(in) :: mask(nx,ny)
    integer :: i,j

    !$OMP PARALLEL DO PRIVATE(i,j)
    do j=1,ny
       do i=1,nx
          if(mask(i,j)) then
             bytes(i,j) = max(0,min(199,nint((var(i,j)-sub) * scale))) + 50
          else
             bytes(i,j) = 0
          endif
       enddo
    enddo

33  format('P5',/,I0,' ',I0,' 255')

    open(unit=1033,file=trim(filename),status='NEW',form='FORMATTED')
    write(1033,33) nx,ny
    close(1033)
    open(unit=1033,file=trim(filename),status='OLD',access='STREAM',position='APPEND')
    write(1033) bytes
    close(1033)
  end subroutine make_pgm

  subroutine free_swathgen(this)
    class(swathgen), intent(inout) :: this
    integer :: i
    call this%discard_track()
    if(associated(this%files)) then
       do i=1,min(this%nfiles,size(this%files))
          call this%files(i)%close()
       enddo
       deallocate(this%files)
       nullify(this%files)
    endif
    if(associated(this%interp)) then
       do i=1,min(this%nfiles,size(this%interp))
          call this%interp(i)%free()
       enddo
       deallocate(this%interp)
       nullify(this%interp)
    endif
    if(associated(this%precip_vars)) then
       do i=1,min(this%nfiles,size(this%precip_vars))
          call this%precip_vars(i)%free()
       enddo
       deallocate(this%precip_vars)
       nullify(this%precip_vars)
    endif
    if(associated(this%wind_vars)) then
       do i=1,min(this%nfiles,size(this%wind_vars))
          call this%wind_vars(i)%free()
       enddo
       deallocate(this%wind_vars)
       nullify(this%wind_vars)
    endif
    if(this%have_proj) then
       call this%wind%free()
       call this%precip%free()
       call this%proj%free()
       call this%dec%free()
       call this%lats%free()
       call this%lons%free()
    endif
    this%maxnfile=0
    this%nfiles=0
    this%itime=0
    this%ntimes=0
    this%nx=0
    this%ny=0
    this%have_proj=.false.
    this%inited=.false.
    this%did_first_interp=.false.
    this%have_track=.false.
  end subroutine free_swathgen

  subroutine init_swathgen(this,maxnfile)
    integer, intent(in) :: maxnfile
    class(swathgen), intent(inout) :: this
    integer :: i

    allocate(this%files(maxnfile))
    do i=1,maxnfile
       call init_wrfdiag_file(this%files(i))
    enddo
    this%nfiles=0
    this%maxnfile=maxnfile
    this%in_crazy_us_units=.false.
    call init_vardata_real(this%lats)
    call init_vardata_real(this%lons)
  end subroutine init_swathgen

  subroutine add_file(this,filename)
    class(swathgen), intent(inout) :: this
    character*(*), intent(in) :: filename
    class(wrfdiag_file), pointer :: f
    integer :: ifile
    character*255 message
    ifile=this%nfiles+1

    if(ifile>this%maxnfile) then
3011   format(A,': too many files.  Could not add this one.  Increase maxnfile in call to init_swathgen.  You provided maxnfile=',I0) ! ,/,' -- PROGRAM IS ABORTING DUE TO INSUFFICIENT MAXNFILE DIMENSION IN CALL TO INIT_SWATHGEN --')
       write(message,3011) trim(filename),this%maxnfile
       call fail(message)
    endif

    this%in_crazy_us_units=.false.
    this%nfiles=ifile
    f=>this%files(ifile)
    call init_wrfdiag_file(f,trim(filename))
    !call f%make_latlonrot(2) ! 2 = first non-analysis time
    if(ifile==1) call this%first_file(f)
    if(f%ntimes /= this%ntimes) then
       write(message,30) trim(f%path),f%ntimes,trim(this%files(1)%path),this%files(1)%ntimes
       call fail(message)
30     format(A,': ntimes mismatch.  File has ',I0,' times, while first file (',A,') has ',I0,'.',/,'  -- PROGRAM IS ABORTING DUE TO MISMATCH IN TIME COUNT --')
    endif
  end subroutine add_file

  subroutine first_file(this,f)
    ! Called when the first file is read in.  Only argument is the
    ! first file's wrfdiag_file object.  
    class(swathgen), intent(inout) :: this
    class(wrfdiag_file), pointer, intent(inout) :: f
    this%ntimes = f%ntimes
    this%nauxphr = 3600./(f%epochtimes(2)-f%epochtimes(1))
  end subroutine first_file

  subroutine setup_proj(this, latres, lonres, latpad1, latpad2, lonpad1, lonpad2, first_time, last_time)
    class(swathgen), intent(inout) :: this
    class(wrfdiag_file), pointer :: f
    integer, optional, intent(in) :: first_time, last_time
    real, intent(in) :: latres,lonres, lonpad1,lonpad2, latpad1,latpad2
    integer :: ifile, nlat,nlon, ilast
    real :: lat1f,lon1f,lat2f,lon2f,latmidf,lonmidf
    real :: lat1,lon1,lat2,lon2,latmid,lonmid
    real(kind=8) :: hourdiff, hourdiff0
    character*256 :: message

    print '(A)','Setup swath projection...'
    if(this%nfiles<1) then
       call fail('In swathgen, no files provided before call to setup_proj.')
    endif

    lat1=1e19 ; lat2=-1e19
    lon1=1e19 ; lon2=-1e19
    do ifile=1,this%nfiles
       f=>this%files(ifile)
       call f%latlon_bounds(lat1f,latmidf,lat2f, lon1f,lonmidf,lon2f, &
            first_time=first_time, last_time=last_time)
       if(lat1f<lat1) lat1=lat1f
       if(lon1f<lon1) lon1=lon1f
       if(lat2f>lat2) lat2=lat2f
       if(lon2f>lon2) lon2=lon2f
       hourdiff=(f%epochtimes(2)-f%epochtimes(1))/3600.
       if(ifile==1) then
          hourdiff0=hourdiff
       elseif(hourdiff0/=hourdiff) then
          write(message,3301) hourdiff0,hourdiff
3301      format('Error: mismatch in output frequencies between files (',F0.3,' hours vs. ',F0.3,' hours).  All files must have the same output frequency.')
       endif
    enddo
    if(hourdiff0<1/3600.) then
       call fail('Likely an error: wrfdiag output frequency is less than a second.  Aborting.')
    endif
    this%nauxphr=1./hourdiff0

    lat1 = latres *  floor (lat1/latres) - latpad1
    lon1 = lonres *  floor (lon1/lonres) - lonpad1
    lat2 = latres * ceiling(lat2/latres) + latpad2
    lon2 = lonres * ceiling(lon2/lonres) + lonpad2
    nlat = floor((lat2-lat1-latres/2)/latres)+1
    nlon = floor(mod(3600+lon2-lon1-lonres/2,360.)/lonres)+1
    this%ny=nlat
    this%nx=nlon

    call init_proj_latlon(this%proj,lat1,lon1,latres,lonres,nlon,nlat, &
         lon_inner=.true.,cyclic_lon=.false.,north_polar=.false.,      &
         south_polar=.false.)

    call init_decomp(this%dec,          &
                     1,nlon,1,nlat,1,1, &
                     1,nlon,1,nlat,1,1  )

104 format('Generate projection with nx=',I0,' ny=',I0,' lat1=',F0.3,' lon1=',F0.3,' dlat=',F0.3,' dlon=',F0.3)
    print 104, nlon,nlat,lat1,lon1,latres,lonres

    this%in_crazy_us_units=.false.
    this%have_proj=.true.
  end subroutine setup_proj

  subroutine interp_time(this,itime)
    class(swathgen), target, intent(inout) :: this
    integer, intent(in) :: itime
    integer :: ifile, i,j
    class(decomp), pointer :: fd, sd ! file and swath dimensions
    class(wrfdiag_file), pointer :: f
    class(wrfdiag_var), pointer :: w1,p1 ! wind and precip input
    real :: w,p
    logical :: m
    character*255 message

    if(this%nfiles<1) then
       call fail('In swathgen, no files provided before call to interp_next_time.')
    endif

    if(itime>this%ntimes .or. itime<2) then
30     format('Error: interp_time called with invalid itime.  The itime argument must be between 2 and ntimes (',I0,'), inclusive.  You provided ',I0,'.')
       write(message,30) this%ntimes,itime
       call fail(message)
    endif
    
    this%itime=itime
    sd=>this%dec
    f=>this%files(1)

    ! Do the initial stuff at the first time:
    if(.not. this%did_first_interp) then
       print '(A)','Do first interp initialization...'
       if(.not. this%have_proj) &
            call this%setup_proj(0.1,0.1,0.5,0.5,0.5,0.5)

       ! Allocate data:
       call init_vardata_real(this%wind,this%proj,this%dec,mask=.false.,alloc=.true.)
       call init_vardata_real(this%precip,this%proj,this%dec,mask=.false.,alloc=.true.)
       call init_vardata_real(this%lats,this%proj,this%dec,mask=.false.,alloc=.true.)
       call init_vardata_real(this%lons,this%proj,this%dec,mask=.false.,alloc=.true.)

       ! Get lats & lons:
       call this%proj%projinfo(1,this%nx,1,this%ny,1,1, &
                               1,this%nx,1,this%ny,1,1, &
                               1,this%nx,1,this%ny,1,1, &
                               lat=this%lats%rdata,     &
                               lon=this%lons%rdata      )

       ! Initialize wind and precip to zero:
       !$OMP PARALLEL DO PRIVATE(i,j)
       do j=sd%jps,sd%jpe
          do i=sd%ips,sd%ipe
             this%wind%rdata(i,j,1)=0
             this%precip%rdata(i,j,1)=0
          enddo
       enddo

       ! Allocate the mask and set it to "no data here" for all locations:
       call this%wind%alloc_mask(fill=.false.)
       call this%precip%alloc_mask(fill=.false.)

       ! Allocate the arrays used to store per-file precip and wind:
       allocate(this%interp(this%nfiles))
       allocate(this%precip_vars(this%nfiles))
       allocate(this%wind_vars(this%nfiles))
       do ifile=1,this%nfiles
          call init_wrfdiag_var(this%precip_vars(ifile))
          call init_wrfdiag_var(this%wind_vars(ifile))
          call init_bilinear_real(this%interp(ifile))
       enddo
       this%did_first_interp=.true.
    endif

    ! Process all files in order received:
    fileloop: do ifile=1,this%nfiles
       f=>this%files(ifile)
       fd=>f%de_ij

333    format('Projection for file ',I0,' time ',I0)
       print 333, ifile,itime
       call f%eproj(itime)%print(unit=6)

       ! Read the next time's precip_swath and windsq_swath
       p1=>this%precip_vars(ifile)
       w1=>this%wind_vars(ifile)
       call init_wrfdiag_var(p1,f,'PRECIP_SWATH',itime)
       call init_wrfdiag_var(w1,f,'WINDSQ_SWATH',itime)

       ! Initialize the interpolator
       call this%interp(ifile)%free()
       call init_bilinear_real(this%interp(ifile),this%wind,this%wind_vars(ifile))
       call this%interp(ifile)%prep()

       ! Allocate the mask if it isn't there already:
       call p1%alloc_mask(fill=.false.)
       call w1%alloc_mask(fill=.false.)

       ! Replace the square of the wind with the wind, and update
       ! the bitmask:
       !$OMP PARALLEL DO PRIVATE(i,j,w,p,m)
       jfixit: do j=fd%jps,fd%jpe
          ifixit: do i=fd%ips,fd%ipe
             w=max(0.,w1%rdata(i,j,1)) ! discard negative windsq
             p=max(0.,p1%rdata(i,j,1)) ! discard negative precip
             w=sqrt(w)                 ! wind is sqrt(windsq)

             ! Store back corrected values and non-squared winds:
             w1%rdata(i,j,1) = w
             p1%rdata(i,j,1) = p

             ! Calculate the mask.
             m=(w>0 .or. p>0) 
             w1%mask(i,j,1) = m
             p1%mask(i,j,1) = m
             ! In the mask generation, note the intentional bitwise
             ! zero comparison, with no epsilon.  This is because
             ! the only bitwise zeros in windsq should be places
             ! where there is no swath.  Near-zero wind is possible
             ! in areas where wind is very small due to dynamical
             ! reasons.  Wind that is bitwise zero is extremely
             ! unlikely, except where there is no swath data.
          enddo ifixit
       enddo jfixit

       ! Interpolate wind and precip, storing the maximum of the
       ! current and prior data:
       call this%interp(ifile)%scalar(this%wind,w1, op=OP_MAX)
       call this%interp(ifile)%scalar(this%precip,p1, op=OP_MAX)
    enddo fileloop
  end subroutine interp_time
  
end module swathgen_module
