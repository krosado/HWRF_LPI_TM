program wave_sample
  use sysutil_module
  use vardata_module
  use wrfdiag_module
  use projection_module
  use decomp_module
  use interp_module
  implicit none

  type data
     type(wrfdiag_file) :: file
     type(wrfdiag_var) :: u10,v10,tlow,zlow,sm,mslp
     type(vardata_real) :: u10norot,v10norot
     type(bilinear_real) :: latlon_interp, norot_interp
     type(proj_nmme) :: norot ! WRF-NMM projection with Earth winds
     integer :: unit=-99 ! unit for outputting track
  end type data

  integer, parameter :: maxfiles=20, nmlunit=30

  real :: lat1,lon1, dlat,dlon, interpfactor
  integer :: nlat,nlon, nfiles
  character(len=200),target :: wrfdiag_in(maxfiles)

  type(data), target :: d(maxfiles)
  type(proj_latlon), target :: latlon
  type(decomp), target :: latlon_dec
  integer :: f,t ! file and time indices
  type(vardata_real) :: latlon_u10,latlon_v10,latlon_tlow,latlon_zlow, &
       latlon_sm,latlon_mslp
  real :: fcsthour
  logical :: reinit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call read_namelist('wave_sample.nml')
  if(nfiles<1) &
       call fail('Error: specify at least one filename in wave_sample.nml')

  ! Initialize the lat-lon projection.  Note that if you set lon_inner
  ! to .false., you will need to swap nlon and nlat in the call.
  call init_proj_latlon(latlon,lat1,lon1,dlat,dlon,nlon,nlat,lon_inner=.true.)
  call init_decomp(latlon_dec,  &
                   1,nlon,1,nlat,1,1, &  ! Domain dimensions
                   1,nlon,1,nlat,1,1)    ! Memory dimensions

  ! Allocate variables, filling the mask with .false.
  call full_init_vardata_real(latlon_u10,latlon,latlon_dec,alloc=.true., &
       mask=.true.,fill=.false.)
  call init_vardata_real(latlon_v10,latlon,latlon_dec,alloc=.true., &
       mask=.true.,fill=.false.)
  call init_vardata_real(latlon_tlow,latlon,latlon_dec,alloc=.true., &
       mask=.true.,fill=.false.)
  call init_vardata_real(latlon_zlow,latlon,latlon_dec,alloc=.true., &
       mask=.true.,fill=.false.)
  call init_vardata_real(latlon_sm,latlon,latlon_dec,alloc=.true., &
       mask=.true.,fill=.false.)
  call init_vardata_real(latlon_mslp,latlon,latlon_dec,alloc=.true., &
       mask=.true.,fill=.false.)

  openfiles: do f=1,nfiles
     call init_wrfdiag_file(d(f)%file,wrfdiag_in(f))
  enddo openfiles

  ! For analysis time, only generate track:
  fcsthour=0
  call clear_latlon_variables(-999.)
  do f=1,nfiles
     call read_and_interp(d(f)%file,1,.false.)
     call d(f)%file%make_latlonrot(1) ! generate lat & lon arrays
     call output_track(f,1,d(f))
  enddo

  ! Generate native grid, lat-lon grid and track at all later times:
  reinit=.true.
  timeloop: do t=2,d(1)%file%ntimes
     fcsthour=(d(1)%file%epochtimes(2)-d(1)%file%epochtimes(1))/3600.
     call clear_latlon_variables(-999.)
     do f=1,nfiles
        call read_and_interp(d(f)%file,t,reinit)
        call d(f)%file%make_latlonrot(t) ! generate lat & lon arrays

        ! Output native grid data:
        call output_native_grid(t,f,fcsthour,d(f)%file%de_ij, &
             d(f)%file%lats%rdata, &
             d(f)%file%lons%rdata,d(f)%u10%rdata,d(f)%v10%rdata, &
             d(f)%tlow%rdata,d(f)%zlow%rdata,d(f)%sm%rdata, &
             d(f)%mslp%rdata)
        call output_track(f,t,d(f))
        if(mod(t,8)==4) then
           ! Display images of data in a window for debugging:
           !call view_pgm('mslp native',t,f,d(f)%mslp)
           !call view_pgm('u10 native',t,f,d(f)%u10norot)
        endif
     enddo

     ! Output lat-lon grid data:
     call output_latlon_grid(t,fcsthour,d(f)%file%de_ij, &
          latlon_u10%rdata,   &
          latlon_v10%rdata,latlon_tlow%rdata,latlon_zlow%rdata, &
          latlon_sm%rdata,latlon_mslp%rdata)
     if(mod(t,8)==4) then
        ! Display images of data in a window for debugging:
        !call view_pgm('tlow latlon',t,0,latlon_tlow)
        !call view_pgm('zlow latlon',t,0,latlon_zlow)
        !call view_pgm('mslp latlon',t,0,latlon_mslp)
        !call view_pgm('sm latlon',t,0,latlon_sm)
        !call view_pgm('u10 latlon',t,0,latlon_u10)
        !call view_pgm('v10 latlon',t,0,latlon_v10)
     end if
  enddo timeloop

contains

  subroutine output_track(ifile,itime,d)
    use sysutil_module,only: get_unit, warn
    use wrfdiag_module, only: wrfdiag_file, time_len
    implicit none
    type(data), target, intent(inout) :: d
    integer, intent(in) :: itime, ifile
    integer(kind=4) :: ierr
    integer(kind=8) :: atime, ftime
    class(decomp), pointer :: b
    class(wrfdiag_file),pointer::f
    integer :: fhr, lat10, lon10, wind10, pres10
    character(len=time_len), pointer :: a, t
    character(len=1) :: lonhemi,lathemi
    real :: maxwind, minpres, wind, pres, lat, lon
    character*100 :: outfile
    character*255 :: message
    integer :: i,j

    f=>d%file
    b=>f%de_ij
    if(.not. f%have_track) then
       if(itime<2) then
20        format('File ',I0,': no track information.  Will not write an hourly track file.')
          write(message,20) ifile
          call warn(message)
       endif
       return
    endif
    if(d%unit<0)then
       print *, 'call getunit for ',ifile
       d%unit=get_unit(400,499)
       print *, 'got unit ',d%unit
30     format('hourly-track-',I0,'.atcf')
       write(outfile,30) ifile
       open(unit=d%unit,file=trim(outfile),form='FORMATTED',position='APPEND')
35     format(A,': open for wrfdiag ',I0)
       write(message,35) trim(outfile),ifile
       
    endif
    if(d%unit<400 .or. d%unit>499) then
       call fail('invalid d%unit')
    endif
    a=>f%times(1) ! time 1 = analysis time
    if (itime==1) then
       ! Data is invalid at the analysis time.  Have to get creative.
       !   lat & lon = actual domain center
       !   wind = actual max wind
       !   pres = actual min pres
       fhr=0
       maxwind=-999.
       minpres=999999.
       !$OMP PARALLEL DO PRIVATE(i,j,wind,pres) REDUCTION(max:maxwind) REDUCTION(min:minpres)
       do j=b%jps,b%jpe
          do i=b%ips,b%ipe
             wind=sqrt(d%u10%rdata(i,j,1)**2 + d%v10%rdata(i,j,1)**2)
             if(wind<100 .and. wind>0 .and. wind>maxwind) maxwind=wind
             pres=d%mslp%rdata(i,j,1)
             if(pres<103000.0 .and. pres>84000.0 .and. pres<minpres) minpres=pres
          enddo
       enddo
       !$OMP END PARALLEL DO
       if(maxwind<0) then
          call warn('Wind values are all missing or non-physical at analysis time.  Inserting wind=0 for analysis time.')
          maxwind=0.
       endif
       if(minpres>103000.0) then
          call warn('Pressure values are all missing or non physical at analysis time.  Using MSLP=0.')
          minpres=0.
       endif
       wind10=nint(maxwind)
       pres10=nint(minpres/100.0)
       lat=f%lats%rdata( (b%ide+b%ids)/2, (b%jde+b%jds)/2, b%kps )
       lon=f%lons%rdata( (b%ide+b%ids)/2, (b%jde+b%jds)/2, b%kps )
    else
       ! Not the analysis time, so get the actual data.
       t=>f%times(itime) ! forecast time
       call c_wrf2epoch(a,atime,ierr)
       call c_wrf2epoch(t,ftime,ierr)
       fhr=(ftime-atime+1800)/3600
       wind10=nint(f%track_wind(itime))
       pres10=nint(f%track_mslp(itime)/100.)
       lat=f%track_lat(itime)
       lon=f%track_lon(itime)
    endif

    lat10=nint(lat*10)
    lathemi=merge('S','N',lat10<0)
    lat10=abs(lat10)

    lon10=nint(lon*10)
    lonhemi=merge('W','E',lon10<0)
    lon10=abs(lon10)

    write(d%unit,303) a(1:4)//a(6:7)//a(9:10)//a(12:13),fhr,lat10,lathemi, &
                      lon10,lonhemi,wind10,pres10
303 format('XX, 00, ',A10,', 03, HWRF, ',I3.3,', ',I3.3,A1,', ',I4.4,A1,', ',I3.3,', ',I4.4)
  end subroutine output_track

  subroutine read_and_interp(file,itime,reinit)
    implicit none
    class(wrfdiag_file), target :: file
    class(decomp), pointer :: dec
    class(proj_nmme), pointer :: proj
    integer, intent(in) :: itime    
    integer :: i,j
    logical :: m
    logical, intent(in) :: reinit

    dec=>file%de_ij ! grid bounds for IJ variables in current file
    proj=>file%eproj(itime) ! E grid projection for current file and time

123 format(A,': read time ',I0)
    print 123,trim(file%path),itime
    
    ! Read variables for the current time:
    call init_wrfdiag_var(d(f)%u10,file,'U10',itime)
    call init_wrfdiag_var(d(f)%v10,file,'V10',itime)
    call init_wrfdiag_var(d(f)%tlow,file,'TLOW',itime)
    call init_wrfdiag_var(d(f)%zlow,file,'ZLOW',itime)
    call init_wrfdiag_var(d(f)%sm,file,'SM',itime) ! SM = Sea Mask (1=water, 0=land)
    call init_wrfdiag_var(d(f)%mslp,file,'BEST_MSLP',itime)

    ! Reset (or set) the mask to true for all points on all variables:
    call d(f)%u10%alloc_mask(fill=.true.)
    call d(f)%v10%alloc_mask(fill=.true.)
    call d(f)%tlow%alloc_mask(fill=.true.)
    call d(f)%zlow%alloc_mask(fill=.true.)
    call d(f)%sm%alloc_mask(fill=.true.)
    call d(f)%mslp%alloc_mask(fill=.true.)

    ! Unrotate winds but keep on current grid:
    call init_proj_nmme(d(f)%norot,proj,earth_winds=.true.)
    call init_vardata_real(d(f)%u10norot,d(f)%norot,dec,alloc=.true.,reinit=reinit, &
         mask=.true.,fill=.false.) ! initialize mask to false; will be replaced
    call init_vardata_real(d(f)%v10norot,d(f)%norot,dec,alloc=.true.,reinit=reinit, &
         mask=.true.,fill=.false.) ! initialize mask to false; will be replaced
    call init_bilinear_real(d(f)%norot_interp,d(f)%u10norot,d(f)%u10)
    call d(f)%norot_interp%prep()
    call d(f)%norot_interp%hwind(d(f)%u10norot,d(f)%v10norot, &
         d(f)%u10,d(f)%v10)

    ! Now fill the mask for all variables except sm with .false. in
    ! areas where there is no sea:

    !$OMP PARALLEL DO PRIVATE(i,j,m)
    do j=dec%jps,dec%jpe
       do i=dec%ips,dec%ipe
          m = ( d(f)%sm%rdata(i,j,1) > 0.5 .and. d(f)%sm%rdata(i,j,1)<1.05 )
          !m=.true.
          d(f)%u10%mask(i,j,1)=m
          d(f)%v10%mask(i,j,1)=m
          d(f)%tlow%mask(i,j,1)=m
          d(f)%zlow%mask(i,j,1)=m
          d(f)%mslp%mask(i,j,1)=m
       enddo
    enddo

    ! Interpolate to lat-lon grid:
    call init_bilinear_real(d(f)%latlon_interp,latlon,latlon_dec, &
         file%eproj(itime),file%de_ij)
    call d(f)%latlon_interp%prep()
    call d(f)%latlon_interp%hwind(latlon_u10,latlon_v10,d(f)%u10,d(f)%v10)
    call d(f)%latlon_interp%scalar(latlon_tlow,d(f)%tlow)
    call d(f)%latlon_interp%scalar(latlon_zlow,d(f)%zlow)
    call d(f)%latlon_interp%scalar(latlon_sm,d(f)%sm)
    call d(f)%latlon_interp%scalar(latlon_mslp,d(f)%mslp)
  end subroutine read_and_interp

  subroutine read_namelist(nmlfile)
    implicit none
    character*(*) :: nmlfile

    namelist/waves4hwrf/ lat1,lon1,dlat,dlon,nlat,nlon,interpfactor,wrfdiag_in

    ! Set default values:
    wrfdiag_in(:)=' '
    lon1=261.75
    lat1=-0.25
    dlat=0.25
    dlon=0.25
    nlat=203
    nlon=275

    ! Read namelist:
    open(nmlunit,file=nmlfile,status='OLD')
    read(nmlunit,nml=waves4hwrf)
    close(nmlunit)

    ! Count files:
    nfiles=0
    countfiles: do nfiles=1,maxfiles
       if(len_trim(wrfdiag_in(nfiles))<2 .and. wrfdiag_in(nfiles)(1:1)==' ') then
          exit countfiles
       endif
    end do countfiles
    nfiles=nfiles-1
    print '(A,I0,A)', 'Processing ',nfiles,' wrfdiag files.'
  end subroutine read_namelist
  
  subroutine clear_latlon_variables(badval)
    implicit none
    real, intent(in) :: badval
    integer :: i,j
    class(decomp), pointer :: dec
    dec=>latlon_dec ! to save typing

    ! Reset all masks to .false. for lat-lon output variables:
    call latlon_u10%fill_mask(.false.)
    call latlon_v10%fill_mask(.false.)
    call latlon_tlow%fill_mask(.false.)
    call latlon_zlow%fill_mask(.false.)
    call latlon_sm%fill_mask(.false.)
    call latlon_mslp%fill_mask(.false.)

    ! Fill with badval:
    !$OMP PARALLEL DO PRIVATE(i,j)
    do j=dec%jps,dec%jpe
       do i=dec%ips,dec%ipe
          latlon_u10%rdata(i,j,1)=badval
          latlon_v10%rdata(i,j,1)=badval
          latlon_tlow%rdata(i,j,1)=badval
          latlon_zlow%rdata(i,j,1)=badval
          latlon_sm%rdata(i,j,1)=badval
          latlon_mslp%rdata(i,j,1)=badval
       enddo
    enddo
  end subroutine clear_latlon_variables

  subroutine view_pgm(name,itime,ifile,var)
    ! This subroutine is for debugging.  It creates an image file for
    ! the input data and views it with "display"
    class(vardata), target, intent(inout) :: var
    integer, intent(in) :: itime,ifile
    character*(*), intent(in) :: name

    class(decomp), pointer :: dec
    real, pointer :: data(:,:,:)
    logical, pointer :: mask(:,:,:)
    integer(kind=1) :: bytes(var%dc%ids:var%dc%ide,var%dc%jds:var%dc%jde)
    integer :: i,j,ni,nj
    integer(kind=8) :: count
    real :: maxdata,mindata,sub,scale

30  format('Calling "display" to view variable: ',A,' time index ',I0,' file ',I0)
31  format('Calling "display" to view variable: ',A,' time index ',I0)
    if(ifile>0) then
       print 30, trim(name),itime,ifile
    else
       print 31, trim(name),itime
    endif

    dec=>var%dc
    data=>var%getreal()
    mask=>var%getmask()

    ni=dec%ide-dec%ids+1
    nj=dec%jde-dec%jds+1

    maxdata=-1e19
    mindata=1e19
    count=0
    !$OMP PARALLEL DO PRIVATE(i,j) REDUCTION(min:mindata) &
    !$OMP    REDUCTION(max:maxdata) REDUCTION(+:count)
    do j=dec%jps,dec%jpe
       do i=dec%ips,dec%ipe
          if(mask(i,j,1)) then
             count=count+1
             if(data(i,j,1)>maxdata) maxdata=data(i,j,1)
             if(data(i,j,1)<mindata) mindata=data(i,j,1)
          endif
       enddo
    enddo

    ! Convert to greyscale.  Color range:
    !   0      = masked out
    !   30-229 = data
    !   255    = special: whole image is 255 if all data is masked out
    nodata: if(count==0) then
       ! No data; all data is masked out.  Use special value 255
       !$OMP PARALLEL DO PRIVATE(i,j)
       do j=dec%jps,dec%jpe
          do i=dec%ips,dec%ipe
             bytes(i,j)=255
          enddo
       enddo
    else
       sub=mindata
       scale=200/(maxdata-mindata)
       print *,'    base value ',sub
       print *,'    scaling by ',scale
       !$OMP PARALLEL DO PRIVATE(i,j)
       do j=dec%jps,dec%jpe
          do i=dec%ips,dec%ipe
             if(mask(i,j,1)) then
                bytes(i,j)= (data(i,j,1)-sub)*scale + 30
             else
                bytes(i,j)=0
             endif
          enddo
       enddo
    endif nodata

    !bytes=0

    ! Write the image file
    open(unit=300,file='viewme.pgm',form='FORMATTED')
303 format('P5',/,I0,' ',I0,' 255')
    write(300,303) ni,nj
    close(300)
    open(unit=300,file='viewme.pgm',access='STREAM',position='APPEND', &
         status='OLD')
    write(300) bytes
    close(300)

    ! View the image
    call system('/usr/bin/env display viewme.pgm')
    
    ! Delete the file
    !open(300,file='viewme.pgm',access='STREAM',position='APPEND')
    !close(300,status='DELETE')
  end subroutine view_pgm

  subroutine output_native_grid(t,domainid,fcsthour,d,lat,lon,u10,v10,tlow,zlow,sm,mslp)
    implicit none
    type(decomp), intent(in) :: d
    integer, intent(in) :: domainid
    real, intent(in) :: fcsthour
    real, intent(in), dimension(d%ims:d%ime,d%jms:d%jme,1) :: &
         u10,v10,tlow,zlow,sm,mslp, lat,lon
    integer, intent(in) :: t ! time index (analysis time is t=1)
    character*80 :: filename
    integer :: u

    u=200+domainid

    if(t==2) then
       ! First output time opens the file and writes the dimensions.
303    format('native-file-',I0,'.dat')
       write(filename,303) domainid
       open(u,file=trim(filename),form='UNFORMATTED')
       write(u) 'NATIVE  '
       write(u) 'DIMS    '
       write(u) d%ide-d%ids+1, d%jde-d%jds+1  ! write i, j dim size
    endif

    write(u) 'FCSTHOUR'
    write(u) fcsthour
    write(u) 'LAT     '
    write(u) lat
    write(u) 'LON     '
    write(u) lon
    write(u) 'U10     '
    write(u) u10
    write(u) 'V10     '
    write(u) v10
    write(u) 'TLOW    '
    write(u) tlow
    write(u) 'ZLOW    '
    write(u) zlow
    write(u) 'SEAMASK '
    write(u) sm
    write(u) 'MSLP    '
    write(u) mslp
  end subroutine output_native_grid

  subroutine output_latlon_grid(t,fcsthour,d,u10,v10,tlow,zlow,sm,mslp)
    implicit none
    real, intent(in) :: fcsthour
    type(decomp), intent(in) :: d
    real, intent(in), dimension(d%ims:d%ime,d%jms:d%jme,1) :: &
         u10,v10,tlow,zlow,sm,mslp
    integer, intent(in) :: t ! time index (analysis time is t=1)

    real :: lats(d%jds:d%jde), lons(d%ids:d%ide)
    integer :: i,j

    do i=d%ids,d%ide
       lons(i)=latlon%lon1+latlon%dlon*(i-d%ids)
    enddo
    do j=d%jds,d%jde
       lats(j)=latlon%lat1+latlon%dlat*(j-d%jds)
    enddo

    if(t==2) then
       ! First output time, so we write extra things
       open(100,file='latlon.dat',form='UNFORMATTED')
       write(100) 'LATLON  '
       write(100) 'DIMS    '
       write(100) d%ide-d%ids+1, d%jde-d%jds+1  ! write i, j dim size
       ! write lat and lon locations
       write(100) 'LON     '
       write(100) lons
       write(100) 'LAT     '
       write(100) lats
    endif

    write(100) 'FCSTHOUR'
    write(100) fcsthour
    write(100) 'U10     '
    write(100) u10
    write(100) 'V10     '
    write(100) v10
    write(100) 'TLOW    '
    write(100) tlow
    write(100) 'ZLOW    '
    write(100) zlow
    write(100) 'SEAMASK '
    write(100) sm
    write(100) 'MSLP    '
    write(100) mslp
  end subroutine output_latlon_grid
end program wave_sample
