module wrfdiag_module
  use sysutil_module, only: fail, warn
  use vardata_module, only: vardata, init_vardata, &
       vardata_real, init_vardata_real, free_vardata
  use decomp_module, only: decomp, init_decomp
  use projection_module, only: proj_nmme, init_proj_nmme
  use constants_module, only: piconst=>pi, DEGRAD, Requator, flattening
  use tcf_module, only: atcf
  implicit none
  private

  public :: wrfdiag_file, wrfdiag_var, init_wrfdiag_file, init_wrfdiag_var, open_var

  public :: wrfdiag_abort       ! abort with error message

  integer, parameter, public :: path_len=160, varname_len=160 ! longer is better
  integer, parameter, public :: read_integer=1, read_real=2, read_auto=0

  ! verbose, abort_crashes: for debugging
  !   verbose=.true. turns on extra debug messages
  !   abort_crashes=.true. turns on a print statement, which prints an
  !     uninitialized variable.  If you compile with debugging, some
  !     compilers can catch use of an uninitialized variable and will
  !     abort, giving a way to find a stack trace.
  logical :: verbose=.false., abort_crashes=.true.

  integer, parameter, public :: time_len=19 ! do not change! (must match WRF)

  character(len=varname_len), parameter :: no_varname = "(*unnamed*)"
  character(len=path_len), parameter :: no_path = "(**no*file*loaded**)"

  type wrfdiag_file
     ! This type represents an opened wrfdiag_file
     real :: cenlat=0,cenlon=0    ! projection center lat & lon
     real :: h2v_dx=0,h2v_dy=0    ! resolution information

     real, pointer :: wbd0(:)=>NULL(), sbd0(:)=>NULL() ! south-west corner locations
     real, pointer :: boundinfo(:,:)=>NULL()

     type(vardata_real) :: lats, lons, srot, crot ! projection info for one time
     integer :: itime_latlonrot=-99 ! time index for which projection info is valid

     integer :: timeid=-99, ntimes=0 ! Time dimension and length
     integer :: nxid=-99, nx=0      ! west_east dimension id and length-1
     integer :: nyid=0, ny=0      ! south_north dimension id and length-1
     integer :: nzid=0, nz=0      ! bottom_top dimension id and length
     integer :: ncid=-99, status=0 ! file id and I/O status

     logical :: have_wbd0var=.false.
     logical :: have_sbd0var=.false.
     logical :: have_track=.false.

     real, pointer :: track_lat(:)=>NULL(),  track_lon(:)=>NULL(), &
                      track_wind(:)=>NULL(), track_mslp(:)=>NULL()

     character(len=path_len) :: path=no_path ! file path

     ! times: array of time strings.  Each string is 19 chars:
     !     2013-10-30_21:00:00
     !     YYYY-MM-DD_HH:MM:SS
     ! In a standard HWRF run, the times are in UTC, and each time
     ! will be one hour apart.
     ! epochtimes: times in seconds since beginning of Jan 1, 1970
     character(len=time_len), pointer :: times(:) => NULL()
     real(kind=8), pointer :: epochtimes(:) => NULL()

     type(atcf), pointer :: track(:)
     type(proj_nmme), pointer :: eproj(:)
     type(decomp) :: de_ij, de_ijk_masswind, de_ijk_interface
   contains
     procedure close => close_file
     procedure free => close_file ! to match other classes
     procedure open => open_file
     procedure free_latlonrot
     procedure make_latlonrot
     procedure latlon_bounds
     procedure, private :: read_global_meta
     procedure, private :: read_dimensions
     procedure, private :: setup_decomp_proj
     procedure, private :: read_track
     procedure, private :: discard_track
     procedure, private :: try_read_timevar
  end type wrfdiag_file

  interface init_wrfdiag_file
     module procedure empty_file
     module procedure open_file
  end interface

  type, extends(vardata) :: wrfdiag_var
     ! This type represents a variable that has been read in from a
     ! wrfdiag file
     character(len=varname_len) :: varname=no_varname  ! variable name from file
     integer :: dims(3)=(/0,0,0/), &     ! dimension length
          varid=-99, &          ! NetCDF variable ID
          status=0              ! Last NetCDF error (or nf90_noerr if no error)
     class(wrfdiag_file), pointer :: file=>NULL() ! pointer to source wrfdiag_file

     integer :: read_type=read_auto ! integer or real type (this%open)
     integer :: itime=0 ! time index (1=analysis time)

     ! rdata, idata - one of these contains the data read in from the
     !   file.
     real, pointer :: rdata(:,:,:)=>NULL()
     integer, pointer :: idata(:,:,:)=>NULL()
   contains
     procedure open => open_var
     procedure load => load_var
     procedure free => free_wrfdiag_var
     procedure set_time
     procedure alloc_real
     procedure alloc_int
     procedure getreal
     procedure getint
  end type wrfdiag_var

  interface init_wrfdiag_var
     module procedure empty_var
     module procedure open_var
     module procedure dup_var
  end interface

contains

  subroutine latlon_bounds(f, lat1,latmid,lat2, lon1,lonmid,lon2, itime, last_time, first_time)
    ! Finds latitude/longitude bounds for one time, if specified, or
    ! the entire file.
    integer, intent(in), optional :: itime, first_time, last_time
    real, intent(inout) :: lat1,lon1, lat2,lon2
    real, intent(out) :: latmid,lonmid
    class(wrfdiag_file), intent(inout) :: f
    integer :: i, ifirst, ilast
    ifirst=1
    ilast=f%ntimes
    if(present(first_time)) ifirst=min(f%ntimes,max(1,first_time))
    if(present(last_time)) ilast=min(f%ntimes,max(1,last_time))
    if(.not.associated(f%boundinfo)) then
       allocate(f%boundinfo(f%ntimes,6))
       !$OMP PARALLEL DO PRIVATE(i)
       do i=1,f%ntimes
          call omp_set_nested(1)
          call omp_set_dynamic(0)
          call omp_set_num_threads(1)
          call f%eproj(i)%latlon_bounds(f%boundinfo(i,1),f%boundinfo(i,2), &
               f%boundinfo(i,3),f%boundinfo(i,4),f%boundinfo(i,5),f%boundinfo(i,6))
       enddo
       !$OMP END PARALLEL DO
    endif
    if(present(itime)) then
       lat1=f%boundinfo(itime,1)
       latmid=f%boundinfo(itime,2)
       lat2=f%boundinfo(itime,3)
       lon1=f%boundinfo(itime,4)
       lonmid=f%boundinfo(itime,5)
       lon2=f%boundinfo(itime,6)
    else
       lat1=minval(f%boundinfo(ifirst:ilast,1))
       lat2=maxval(f%boundinfo(ifirst:ilast,3))
       lon1=minval(f%boundinfo(ifirst:ilast,4))
       lon2=maxval(f%boundinfo(ifirst:ilast,6))
       latmid=(lat1+lat2)/2
       lonmid=(lon1+lon2)/2
    endif
  end subroutine latlon_bounds

  subroutine make_latlonrot(f,itime)
    ! This subroutine calculates the projection information for a
    ! wrfdiag file.  That includes point lats & lons, as well as the
    ! cosine and sine of wind rotation angles.
    class(wrfdiag_file), intent(inout), target :: f
    class(decomp), pointer :: d
    integer, intent(in) :: itime
    logical reinit
    character*255 message
33  format('Error: invalid time ',I0,' sent to make_latlonrot.  Must be >=1 and <=ntimes=',I0,/, &
         'Program is aborting due to request of invalid time.')
    if(itime<1 .or. itime>f%ntimes) then
       write(message,33) itime,f%ntimes
       call fail(message)
    endif

    reinit = ( f%itime_latlonrot>0 .and. f%itime_latlonrot<=f%ntimes )
    ! if(f%itime_latlonrot>0 .and. f%itime_latlonrot/=itime) then
    !    call free_vardata_real(f%lats)
    !    call free_vardata_real(f%lons)
    !    call free_vardata_real(f%crot)
    !    call free_vardata_real(f%srot)
    ! end if

    call init_vardata_real(f%lats,f%eproj(itime),f%de_ij,alloc=.true.,reinit=reinit)
    call init_vardata_real(f%lons,f%eproj(itime),f%de_ij,alloc=.true.,reinit=reinit)
    call init_vardata_real(f%crot,f%eproj(itime),f%de_ij,alloc=.true.,reinit=reinit)
    call init_vardata_real(f%srot,f%eproj(itime),f%de_ij,alloc=.true.,reinit=reinit)

    f%lats%rdata=-999
    f%lons%rdata=-998
    f%crot%rdata=-99
    f%srot%rdata=-98

    d=>f%de_ij
    call f%eproj(itime)%projinfo(                    &
         d%ips,d%ipe,d%jps,d%jpe,d%kps,d%kps,        &
         d%ips,d%ipe,d%jps,d%jpe,d%kps,d%kps,        &
         d%ips,d%ipe,d%jps,d%jpe,d%kps,kpe=d%kps,    &
         lat =f%lats%rdata,  lon=f%lons%rdata,       &
         crot=f%crot%rdata, srot=f%srot%rdata        )
    f%itime_latlonrot=itime
  end subroutine make_latlonrot

  subroutine free_latlonrot(f)
    class(wrfdiag_file), intent(inout) :: f
    call f%lats%free()
    call f%lons%free()
    call f%crot%free()
    call f%srot%free()
  end subroutine free_latlonrot

  subroutine wrfdiag_abort(why,f,att,dim,var,itime,ntimes,mask,justwarn)
    ! This is an internal implementation routine.  Do not call it
    ! directly.  This subroutine is used by the rest of this module to
    ! print an informative error message and abort.  It is a wrapper
    ! around "fail" and "warn" in sysutil_module, but it automatically
    ! generates the error message, including for NetCDF errors.

    ! Arguments:
    !   why - mandatory: why are we aborting?
    !   f - if present, the wrfdiag_file
    !   att - if present, the error is related to this attribute
    !   dim - if present, the error is related to this dimension
    !   var - if present, the error is related to this variable
    !   mask - var must also be present: indicates the problem is with the mask
    !   itime, ntimes - if both are present, the caller requested an
    !     invalid time "itime", which is either <=0 or >ntimes
    !   justwarn - don't abort: if present and .true., then print 
    !     the error message without aborting
    use netcdf, only: nf90_strerror, nf90_noerr
    class(wrfdiag_file), optional, intent(in) :: f
    integer, intent(in), optional :: itime,ntimes
    logical, optional, intent(in) :: justwarn
    character*(*), intent(in) :: why
    character*(*), intent(in), optional :: att,dim,var,mask
    character*2000 :: message
    character*80 :: nf90mess
    character(len=path_len+2) :: path
    logical stopme
    integer intentionally_uninitialized

    stopme=.true.
    nf90mess=' '
    path=' '

    if(present(justwarn)) stopme=.not.justwarn
    if(present(f)) then
       if(f%status/=nf90_noerr)  &
            nf90mess='NetCDF says: '//nf90_strerror(f%status)
    endif
    if(present(f)) then
       path=trim(f%path)//':'
    elseif(stopme) then
       path='error:'
    else
       path='warning:'
    endif

20  format(A,' ',A,' (',A,' ',A,'). ',A)
22  format(A,' ',A,' (',A,' ',A,', ',A,' ',A,'). ',A)
30  format(A,' ',A,'. ',A)
40  format(A,' Invalid time: itime=',I0,' is larger than the number of times (',I0,').')
    if(present(att)) then
       write(message,20) trim(path),trim(why),'attribute',trim(att),trim(nf90mess)
    elseif(present(dim)) then
       write(message,20) trim(path),trim(why),'dimension',trim(dim),trim(nf90mess)
    elseif(present(var)) then
       if(present(mask)) then
          write(message,22) trim(path),trim(why),'variable',trim(var), &
               'mask',trim(mask),trim(nf90mess)
       else
          write(message,20) trim(path),trim(why),'variable',trim(var),trim(nf90mess)
       endif
    elseif( present(itime) .and. present(ntimes)) then
       write(message,40) trim(path),itime,ntimes
    elseif(.not. present(f)) then
       message=why
    else
       write(message,30) trim(path),trim(why),trim(nf90mess)
    endif
    write(0,'(A)')  trim(message)
    write(6,'(A)')  trim(message)
    if(stopme) then
       if(abort_crashes) then 
          call warn(message)
          print *,intentionally_uninitialized
          ! Printing an uninitialized variable causes an abort with a
          ! stack trace on the Intel compiler when compiling with
          ! "-check all -traceback".  Other compilers may have similar
          ! capabilities.
       else
          call fail(message)
       endif
    else
       call warn(message)
    endif
  end subroutine wrfdiag_abort

  subroutine empty_file(f)
    ! Fills a wrfdiag_file with zeros or other invalid data
    class(wrfdiag_file), intent(inout) :: f
    f%cenlat=0 ; f%cenlon=0
    f%h2v_dx=0 ; f%h2v_dy=0
    f%ncid=-99 ; f%timeid=-99
    f%nxid=-99 ; f%nyid=-99   ; f%nzid=-99
    f%nx=0     ; f%ny=0       ; f%nz=0
    f%have_track=.false.
    f%have_sbd0var=.false. ; f%have_wbd0var=.false.
    nullify(f%times,f%eproj,f%sbd0,f%wbd0,f%boundinfo)
    nullify(f%epochtimes,f%track_lat,f%track_lon,f%track_wind,f%track_mslp)
    f%itime_latlonrot=-99
    f%status=0
    f%ntimes=0
    f%path="(**no*file*loaded**)"
  end subroutine empty_file

  subroutine free_wrfdiag_var(this)
    ! Deallocates a wrfdiag_var and fills metadata with invalid info
    class(wrfdiag_var), intent(inout) :: this
    if(associated(this%rdata)) deallocate(this%rdata)
    if(associated(this%idata)) deallocate(this%idata)
    call free_vardata(this)  ! call superclass destructor
    call empty_var(this) ! fill with invalid data
  end subroutine free_wrfdiag_var

  subroutine empty_var(v)
    ! Fills a wrfdiag_var with nullified pointers and invalid metadata
    class(wrfdiag_var), intent(inout) :: v
    v%varname="(*unnamed*)"
    v%dims=-99
    v%varid=-99
    nullify(v%file)
    nullify(v%rdata)
    nullify(v%idata)
    call init_vardata(v)
  end subroutine empty_var
  
  subroutine dup_var(dest,src)
    ! Duplicates a variable from src to dest
    class(wrfdiag_var), intent(inout) :: dest
    class(wrfdiag_var), intent(in) :: src
    dest%varname =  src%varname
    dest%dims    =  src%dims
    dest%varid   =  src%varid
    dest%file    => src%file
    dest%status  =  src%status

    if(associated(src%rdata)) then
       call dest%alloc_real(src%dims)
       dest%rdata=src%rdata
    elseif(associated(src%idata)) then
       call dest%alloc_int(src%dims)
       dest%idata=src%idata
    else
       if(associated(dest%rdata)) then
          deallocate(dest%rdata)
          nullify(dest%rdata)
       endif
       if(associated(dest%idata)) then
          deallocate(dest%idata)
          nullify(dest%idata)
       endif
    endif
  end subroutine dup_var

  subroutine init_wrfdiag_vars(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15)
    ! This is a convenience function to initialize multiple variables
    ! via wrfdiag_init_var.  The first two arguments are mandatory
    ! wrfdiag_var objects, while the rest are optional.
    class(wrfdiag_var), intent(inout) :: v1,v2
    class(wrfdiag_var), optional, intent(inout) :: v3,v4,v5,v6,v7,v8,v9, &
         v10,v11,v12,v13,v14,v15
    call init_wrfdiag_var(v1)
    call init_wrfdiag_var(v2)
    if(present(v3)) call init_wrfdiag_var(v3)
    if(present(v4)) call init_wrfdiag_var(v4)
    if(present(v5)) call init_wrfdiag_var(v5)
    if(present(v6)) call init_wrfdiag_var(v6)
    if(present(v7)) call init_wrfdiag_var(v7)
    if(present(v8)) call init_wrfdiag_var(v8) 
    if(present(v9)) call init_wrfdiag_var(v9)
    if(present(v10)) call init_wrfdiag_var(v10)
    if(present(v11)) call init_wrfdiag_var(v11)
    if(present(v12)) call init_wrfdiag_var(v12)
    if(present(v13)) call init_wrfdiag_var(v13)
    if(present(v14)) call init_wrfdiag_var(v14)
    if(present(v15)) call init_wrfdiag_var(v15)
  end subroutine init_wrfdiag_vars

  subroutine free_wrfdiag_vars(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15)
    ! This is a convenience function to deallocate multiple variables
    ! via wrfdiag_free.  The first two arguments are mandatory
    ! wrfdiag_var objects, while the rest are optional.
    class(wrfdiag_var), intent(inout) :: v1,v2
    class(wrfdiag_var), optional, intent(inout) :: v3,v4,v5,v6,v7,v8,v9, &
         v10,v11,v12,v13,v14,v15
    call v1%free()
    call v2%free()
    if(present(v3)) call v3%free()
    if(present(v4)) call v4%free()
    if(present(v5)) call v5%free()
    if(present(v6)) call v6%free()
    if(present(v7)) call v7%free()
    if(present(v8)) call v8%free()
    if(present(v9)) call v9%free()
    if(present(v10)) call v10%free()
    if(present(v11)) call v11%free()
    if(present(v12)) call v12%free()
    if(present(v13)) call v13%free()
    if(present(v14)) call v14%free()
    if(present(v15)) call v15%free()
  end subroutine free_wrfdiag_vars

  subroutine open_var(this,f,name,itime,read_type)
    class(wrfdiag_file), target, intent(inout) :: f
    class(wrfdiag_var), intent(inout) :: this
    character*(*), intent(in) :: name
    integer, intent(in), optional :: read_type
    integer, intent(in) :: itime
    if(associated(this%file)) then
       if(.not.associated(this%file,f)) call this%free
    endif
    this%read_type=read_auto
    if(present(read_type)) this%read_type=read_type
    this%varname=name
    this%itime=itime
    this%file=>f
    call this%load() ! actually read the data
  end subroutine open_var

  subroutine set_time(v,itime)
    class(wrfdiag_var), intent(inout) :: v
    integer, intent(in) :: itime
    if(itime==v%itime) return ! same time: nothing to do
    v%itime=itime
    call v%load()
  end subroutine set_time

  subroutine load_var(v)
    ! Reads the variable specified by "name" from time index "itime"
    ! into the wrfdiag_var v from the wrfdiag_file f.  This routine
    ! can read byte, short, int, float or double data and
    ! automatically convert to real*4 or integer*4.

    ! The "ctype" specifies the output datatype.  It is optional and
    ! can be INT or REAL; if missing, the type will be automatically
    ! detected.  If "INT" then data will be cast to integer*4 and
    ! placed in v%idata.  If "REAL" then data will be cast to real*4
    ! and placed in v%rdata.  If cdata is unspecified, then data will
    ! be cast to integer*4 and placed in v%idata if it is of an
    ! integer type (byte, short or int) or real*4 in v%rdata if it is
    ! of a real type (float or double).  Values other than "INT" or
    ! "REAL" will cause an abort with an appropriate error message.
    use netcdf
    class(wrfdiag_var), intent(inout) :: v
    integer :: ndims,dimids(NF90_MAX_VAR_DIMS), idim, itime, nontime(NF90_MAX_VAR_DIMS)
    integer :: counts(NF90_MAX_VAR_DIMS),starts(NF90_MAX_VAR_DIMS),allocs(3)
    character*(80) :: dimname, stagger
    integer :: outdims, xtype, read_type
    class(wrfdiag_file), pointer :: f

    f=>v%file
    read_type=v%read_type
    itime=v%itime

    v%status=nf90_inq_varid(f%ncid,trim(v%varname),v%varid)
    if(v%status/=nf90_noerr) &
         call wrfdiag_abort('cannot inquire varid',f,var=trim(v%varname))
    v%status=nf90_inquire_variable(f%ncid,v%varid,ndims=ndims,dimids=dimids,xtype=xtype,name=v%varname)
    if(v%status/=nf90_noerr) &
         call wrfdiag_abort('cannot inquire variable dimensions',f,var=trim(v%varname))
    if(xtype/=NF90_BYTE .and. xtype/=NF90_SHORT .and. xtype/=NF90_INT .and. xtype/=NF90_FLOAT .and. xtype/=NF90_DOUBLE) then
       call wrfdiag_abort('cannot read variable: unrecognized type (only byte, short, int, float and double are supported)',f,var=trim(v%varname))
    endif
    outdims=0
    starts=1
    allocs=1
    dimdo: do idim=1,ndims
       dimname=' '
       counts(idim)=-99
       v%status=nf90_inquire_dimension(f%ncid,dimids(idim),dimname,counts(idim))
       if(v%status/=nf90_noerr .or. counts(idim)<=0) then
          call wrfdiag_abort('cannot inquire dimension information',f,var=trim(v%varname))
       endif
       if(dimids(idim)==f%timeid) then
          ! time dimension: grab time of interest
          starts(idim)=itime
          if(itime>counts(idim) .or. itime<1) then
             call wrfdiag_abort("invalid time",f,itime=itime,ntimes=counts(idim))
          endif
          counts(idim)=1  ! read only one time
       else
          ! Non-time dimension.  For the nx and ny dimensions: use
          ! len-1 as size.  Otherwise, use full len.
          if(dimids(idim)==f%nxid .or. dimids(idim)==f%nyid) then
             counts(idim)=counts(idim)-1
          endif
          outdims=outdims+1
          if(outdims>3) &
            call wrfdiag_abort("too many dimensions: maximum of 3 non-time dimensions allowed", &
                               f,var=trim(v%varname))
          allocs(outdims)=counts(idim)
          nontime(outdims)=dimids(idim)
       endif
    enddo dimdo

    v%dims=allocs

    if(v%read_type==read_auto) then
       if(xtype==NF90_INT .or. xtype==NF90_SHORT .or. xtype==NF90_BYTE) then
          read_type=read_integer
       else
          read_type=read_real
       endif
    endif

    if(outdims==2 .and. nontime(1)==f%nxid .and. nontime(2)==f%nyid) then
       call init_vardata(v,f%eproj(itime),f%de_ij,alloc=.false.)
    elseif(outdims==3 .and. nontime(1)==f%nxid .and. nontime(2)==f%nyid) then
       ! FIXME: add IJK interface staggering and IKJ
       call init_vardata(v,f%eproj(itime),f%de_ijk_masswind,alloc=.false.)
    else
       write(0,*) 'outdims=',outdims
       write(0,*) 'nontime(1), f%nxid = ',nontime(1),f%nxid
       write(0,*) 'nontime(2), f%nyid = ',nontime(2),f%nyid
       write(0,*) 'timeid, nzid = ',f%timeid,f%nzid
       call wrfdiag_abort("cannot figure out decomposition for variable (only IJ and IJK are supported)",f,var=trim(v%varname))
    endif
          
    if(read_type==read_real) then
       call v%alloc_real(allocs)
       v%status=nf90_get_var(f%ncid,v%varid,v%rdata,count=counts,start=starts)
       if(v%status/=nf90_noerr) &
            call wrfdiag_abort("cannot read variable (as real)",f,var=trim(v%varname))
    else ! read_type==read_integer
       call v%alloc_int(allocs)
       v%status=nf90_get_var(f%ncid,v%varid,v%idata,count=counts,start=starts)
       if(v%status/=nf90_noerr) &
            call wrfdiag_abort("cannot read variable (as integer)",f,var=trim(v%varname))
    endif

  end subroutine load_var

  function getreal(this) result(rdata)
    ! In subclasses, returns this variable's real data, if there is
    ! any, or nullifies the pointer if there isn't.  May do other
    ! things too, such as reading in the data, or allocating and
    ! initializing a blank array.
    class(wrfdiag_var), intent(inout) :: this
    real, pointer :: rdata(:,:,:)
    rdata=>this%rdata
  end function getreal
  
  function getint(this) result(idata)
    class(wrfdiag_var), intent(inout) :: this
    integer, pointer :: idata(:,:,:)
    idata=>this%idata
  end function getint
  
  subroutine alloc_real(v,sizes)
    class(wrfdiag_var), intent(inout) :: v
    integer, intent(in) :: sizes(3)
    logical :: needalloc
    needalloc=.true.
    if(associated(v%rdata)) then
       needalloc = .not. ( size(v%rdata,1)==sizes(1) .and. &
                           size(v%rdata,2)==sizes(2) .and. &
                           size(v%rdata,3)==sizes(3) )
       if(needalloc) then
          if(verbose) then
             print *,'wrong size:'
             print *,'  old: ',size(v%rdata)
             print *,'  new: ',sizes
          endif
          deallocate(v%rdata)
       endif
    endif
    if(needalloc) then
       if(verbose) print *,trim(v%varname),': allocate rdata: size = ',sizes
       allocate(v%rdata(sizes(1),sizes(2),sizes(3)))
    endif
    if(associated(v%idata)) then
       deallocate(v%idata)
       nullify(v%idata)
    endif
    v%dims=sizes
  end subroutine alloc_real

  subroutine alloc_int(v,sizes)
    class(wrfdiag_var), intent(inout) :: v
    integer, intent(in) :: sizes(3)
    logical :: needalloc
    needalloc=.true.
    if(associated(v%idata)) then
       needalloc = .not. ( size(v%idata,1)==sizes(1) .and. &
                           size(v%idata,2)==sizes(2) .and. &
                           size(v%idata,3)==sizes(3) )
       if(needalloc) then
          if(verbose) then
             print *,'wrong size:'
             print *,'  old: ',size(v%idata)
             print *,'  new: ',sizes
          endif
          deallocate(v%idata)
       endif
    endif
    if(needalloc) then
       if(verbose) print *,trim(v%varname),': allocate idata: size = ',sizes
       allocate(v%idata(sizes(1),sizes(2),sizes(3)))
    endif
    if(associated(v%rdata)) then
       deallocate(v%rdata)
       nullify(v%rdata)
    endif
    v%dims=sizes
  end subroutine alloc_int

  subroutine read_one_global_real(f,name,len,out)
    ! Internal implementation function: do not call directly.  Reads
    ! the global attribute "name" from the file with the specified
    ! ncid, into an array with length "len" and places the first array
    ! element into "out" as a real
    use netcdf
    class(wrfdiag_file), intent(inout) :: f
    real :: data(len)
    real, intent(out) :: out
    character*(*),intent(in) :: name
    integer,intent(in) :: len

    f%status=nf90_get_att(f%ncid,nf90_global,trim(name),data)
    if(f%status/=nf90_noerr) &
         call wrfdiag_abort('cannot get global attribute',f,att=trim(name))
    out=data(1)
  end subroutine read_one_global_real

  function try_read_timevar(f,varname,ptr) result(success)
    use netcdf
    class(wrfdiag_file), intent(inout) :: f
    logical :: success
    character*(*), intent(in) :: varname
    real, pointer, intent(inout) :: ptr(:)
    integer :: varid

    success=.false.
    f%status=nf90_inq_varid(f%ncid,trim(varname),varid)
    if(f%status==nf90_noerr) then
       allocate(ptr(f%ntimes))
       f%status=nf90_get_var(f%ncid,varid,ptr, &
            count=(/f%ntimes,1,1,1,1/),start=(/1,1,1,1,1/))
       if(f%status==nf90_noerr) then
          !print *, 'Values for ',varname,' are ',ptr
          success=.true.
       endif
    endif
  end function try_read_timevar

  subroutine close_file(f)
    ! Closes the specified wrfdiag_file
    use netcdf
    class(wrfdiag_file), intent(inout) :: f
    integer i

    f%status=nf90_close(f%ncid)
    if(f%status/=nf90_noerr) &
         call wrfdiag_abort('error closing file',f)

    if(associated(f%wbd0)) then
       deallocate(f%wbd0)
       nullify(f%wbd0)
    endif

    if(associated(f%sbd0)) then
       deallocate(f%sbd0)
       nullify(f%sbd0)
    endif

    if(associated(f%boundinfo)) then
       deallocate(f%boundinfo)
       nullify(f%boundinfo)
    endif

    if(associated(f%eproj)) then
       do i=1,f%ntimes
          call f%eproj(i)%free()
       enddo
       deallocate(f%eproj)
       nullify(f%eproj)
    endif
    call f%de_ij%free()
    call f%de_ijk_masswind%free()
    call f%de_ijk_interface%free()

    if(associated(f%times)) then
       deallocate(f%times)
       nullify(f%times)
    endif
    if(associated(f%epochtimes)) then
       deallocate(f%epochtimes)
       nullify(f%epochtimes)
    endif
    call f%discard_track()
    call empty_file(f)
  end subroutine close_file

  subroutine discard_track(f)
    class(wrfdiag_file), intent(inout) :: f
       if(associated(f%track_lon)) then
          deallocate(f%track_lon)
          nullify(f%track_lon)
       endif
       if(associated(f%track_lat)) then
          deallocate(f%track_lat)
          nullify(f%track_lat)
       endif
       if(associated(f%track_wind)) then
          deallocate(f%track_wind)
          nullify(f%track_wind)
       endif
       if(associated(f%track_mslp)) then
          deallocate(f%track_mslp)
          nullify(f%track_mslp)
       endif
  end subroutine discard_track

  subroutine read_dimensions(f)
    use netcdf
    class(wrfdiag_file), intent(inout) :: f
    integer :: len, timesid, ndims, xtype,namelen, itime, ivar
    integer, dimension(NF90_MAX_VAR_DIMS) :: counts,dimids
    integer(kind=8) :: i64time
    integer(kind=4) :: ierr
    character*80 :: dimname
    character*255 :: message
    
    ! -------------------- READ "west_east" AND "south_north" DIMENSIONS

    ! West-East dimension.  WRF adds 1 to it, which we subtract here:
    f%status=nf90_inq_dimid(f%ncid,'west_east',f%nxid)
    if(f%status/=nf90_noerr) &
         call wrfdiag_abort('cannot inquire about dimension',f,dim='west_east')
    f%status=nf90_inquire_dimension(f%ncid,dimid=f%nxid,len=f%nx)
    if(f%status/=nf90_noerr) &
         call wrfdiag_abort('cannot get dimension length',f,dim='west_east')
    f%nx=f%nx-1

    ! South-North dimension.  WRF adds 1 to it, which we subtract here:
    f%status=nf90_inq_dimid(f%ncid,'south_north',f%nyid)
    if(f%status/=nf90_noerr) &
         call wrfdiag_abort('cannot inquire about dimension',f,dim='south_north')
    f%status=nf90_inquire_dimension(f%ncid,dimid=f%nyid,len=f%ny)
    if(f%status/=nf90_noerr) &
         call wrfdiag_abort('cannot get dimension length',f,dim='south_north')
    f%ny=f%ny-1

    ! Bottom-Top dimension (subtract 1 for mass and wind, don't for interface)
    f%status=nf90_inq_dimid(f%ncid,'bottom_top_stag',f%nzid)
    if(f%status/=nf90_noerr) &
         call wrfdiag_abort('cannot inquire about dimension',f,dim='bottom_top_stag')
    f%status=nf90_inquire_dimension(f%ncid,dimid=f%nzid,len=f%nz)
    if(f%status/=nf90_noerr) &
         call wrfdiag_abort('cannot get dimension length',f,dim='bottom_top_stag')

    ! -------------------- READ "Time" DIMENSION

    f%status=nf90_inq_dimid(f%ncid,'Time',f%timeid)

303 format(A,' dim id is ',I0)
    ! print 303,'west_east',f%nxid
    ! print 303,'south_north',f%nyid
    ! print 303,'Time',f%timeid

    if(f%status/=nf90_noerr) &
         call wrfdiag_abort('cannot inquire about dimension',f,dim='Time')

    f%status=nf90_inquire_dimension(f%ncid,dimid=f%timeid,len=f%ntimes)
    if(f%status/=nf90_noerr) &
         call wrfdiag_abort('cannot get dimension length',f,dim='Time')

    ! -------------------- CHECK FOR "Times" VARIABLE

    f%status=nf90_inq_varid(f%ncid,'Times',timesid)
    if(f%status/=nf90_noerr) &
         call wrfdiag_abort('cannot inquire varid',f,var='Times')

    f%status=nf90_inquire_variable(f%ncid,timesid,ndims=ndims,dimids=dimids,xtype=xtype)
    if(ndims/=2 .or. xtype/=NF90_CHAR) &
         call wrfdiag_abort('Times must be a two-dimensional char array (it is not)', &
                    f,var='Times')
    if(dimids(2)/=f%timeid) &
         call wrfdiag_abort('Times second dimension must be the Time dimension.', &
                    f,var='Times')

    f%status=nf90_inquire_dimension(f%ncid,dimids(1),dimname,namelen)
    if(f%status/=nf90_noerr) &
         call wrfdiag_abort('cannot get first dimension length',f,var='Times')
    if(namelen/=19) then
308    format("time string length must be 19, but is ",I0)
       write(message,308) namelen
       call wrfdiag_abort(trim(message),f,var='Times')
    endif

    ! -------------------- READ TIMES
    allocate(f%times(f%ntimes),f%epochtimes(f%ntimes))
    counts=1
    counts(1)=namelen
    counts(2)=f%ntimes
    f%status=nf90_get_var(f%ncid,timesid,f%times,count=counts)
    if(f%status/=0) &
         call wrfdiag_abort('cannot read time list',f,var='Times')
111 format('Time ',I3,'/',I3,' = ',A19)
    do itime=1,f%ntimes
       call c_wrf2epoch(f%times(itime),i64time,ierr)
       if(ierr/=0) then
330       format('Error: cannot parse time from WRF: "',A19,'" (error code ',I0,').')
          write(message,330) f%times(itime),ierr
          call fail(message)
       endif
       f%epochtimes(itime)=real(i64time,kind=8)
    enddo

    if(1==2) then
       do itime=1,f%ntimes
          print 111,itime,f%ntimes,f%times(itime)
       enddo
    endif

  end subroutine read_dimensions

  subroutine read_track(f)
    use netcdf
    class(wrfdiag_file), intent(inout) :: f
    logical :: have_lon, have_lat, have_mslp, have_wind
    integer :: i
    call f%discard_track()
    have_lon=f%try_read_timevar('TRACKER_FIXLON',f%track_lon)
    have_lat=f%try_read_timevar('TRACKER_FIXLAT',f%track_lat)
    have_mslp=f%try_read_timevar('TRACKER_PMIN',f%track_mslp)
    have_wind=f%try_read_timevar('TRACKER_VMAX',f%track_wind)
    f%have_track = (have_lon .and. have_lat .and. have_mslp .and. have_wind)
    if(f%have_track) then
       f%have_track=.false.
       checktrack: do i=1,f%ntimes
          if(f%track_wind(i)>0. .and. f%track_mslp(i)>80000. .and. &
               f%track_mslp(i)<130000.) then
             f%have_track=.true.
             exit checktrack
          endif
       enddo checktrack
    endif
    if(.not.f%have_track) call f%discard_track()
  end subroutine read_track

  subroutine read_global_meta(f)
    use netcdf
    class(wrfdiag_file), intent(inout) :: f
    integer :: len
    
    ! Projection center longitude
    f%status=nf90_inquire_attribute(f%ncid,nf90_global,'CEN_LON',len=len)
    if(f%status/=nf90_noerr) &
         call wrfdiag_abort('cannot inquire about global attribute',f,att='CEN_LON')
    call read_one_global_real(f,'CEN_LON',len,f%cenlon)

    ! Projection center latitude
    f%status=nf90_inquire_attribute(f%ncid,nf90_global,'CEN_LAT',len=len)
    if(f%status/=nf90_noerr) &
         call wrfdiag_abort('inq att',f,att='CEN_LAT')
    call read_one_global_real(f,'CEN_LAT',len,f%cenlat)

    ! H-to-V point distance in rotated longitude direction
    f%status=nf90_inquire_attribute(f%ncid,nf90_global,'DX',len=len)
    if(f%status/=nf90_noerr) &
         call wrfdiag_abort('cannot inquire about global attribute',f,att='DX')
    call read_one_global_real(f,'DX',len,f%h2v_dx)

    ! H-to-V point distance in rotated latitude direction
    f%status=nf90_inquire_attribute(f%ncid,nf90_global,'DY',len=len)
    if(f%status/=nf90_noerr) &
         call wrfdiag_abort('inq att',f,att='DY')
    call read_one_global_real(f,'DY',len,f%h2v_dy)
  end subroutine read_global_meta

  subroutine setup_decomp_proj(f) 
    use netcdf
    class(wrfdiag_file), intent(inout) :: f
    real, pointer :: data(:)
    character*4 :: name
    integer :: ivar, varid, itime, i, ndims, xtype
    logical :: have, found
    integer, dimension(NF90_MAX_VAR_DIMS) :: counts,dimids

    f%have_wbd0var=f%try_read_timevar('WBD0VAR',f%wbd0)
    if(.not.f%have_wbd0var) then
       call wrfdiag_abort('Cannot read WBD0VAR (will use HLON(1,1,itime) instead).',f,var='WBD0VAR',justwarn=.true.)
    endif

    f%have_sbd0var=f%try_read_timevar('SBD0VAR',f%sbd0)
    if(.not.f%have_sbd0var) then
       call wrfdiag_abort('Cannot read SBD0VAR (will use HLAT(1,1,itime) instead).',f,var='SBD0VAR',justwarn=.true.)
    endif

    do ivar=1,2
       if(ivar==1) then
          name='HLAT'
          data=>f%sbd0
          have=f%have_sbd0var
       else
          name='HLON'
          data=>f%wbd0
          have=f%have_wbd0var
       endif
       !if(.not. have) then
          f%status=nf90_inq_varid(f%ncid,name,varid)
          if(f%status/=nf90_noerr) &
               call wrfdiag_abort('cannot determine varid',f,var=name)
          f%status=nf90_inquire_variable(f%ncid,varid,ndims=ndims,dimids=dimids)
          if(f%status/=nf90_noerr) &
               call wrfdiag_abort('cannot inquire_variable',f,var=name)
          counts=1
          found=.false.
          do i=1,ndims
             if(dimids(i)==f%timeid) then
                counts(i)=f%ntimes
                found=.true.
             endif
          enddo
          if(f%ntimes>1 .and. .not.found) &
            call wrfdiag_abort('cannot find time dimension in variable dims',f,var=name)
          f%status=nf90_get_var(f%ncid,varid,data,count=counts,start=(/1,1,1,1,1/))
          if(f%status/=nf90_noerr) &
               call wrfdiag_abort('cannot read data',f,var=name)
       !endif
    enddo
    
    call init_decomp(f%de_ij,           1,f%nx,1,f%ny,1,     1, &
                                           1,f%nx,1,f%ny,1,     1  )
    call init_decomp(f%de_ijk_masswind, 1,f%nx,1,f%ny,1,f%nz-1, &
                                           1,f%nx,1,f%ny,1,f%nz-1  )
    call init_decomp(f%de_ijk_masswind, 1,f%nx,1,f%ny,1,f%nz  , &
                                           1,f%nx,1,f%ny,1,f%nz    )

    allocate(f%eproj(f%ntimes))
    do itime=1,f%ntimes
       call init_proj_nmme(f%eproj(itime), f%cenlat,f%cenlon, f%nx,f%ny, &
            f%h2v_dx,f%h2v_dy, f%sbd0(itime),f%wbd0(itime), .false.,.false.)
    enddo
  end subroutine setup_decomp_proj

  subroutine open_file(f,path)
    ! Opens the NetCDF wrfdiag file specified in "path" in the
    ! wrfdiag_file object "f".  Reads in certain global attributes and
    ! the time list.
    use netcdf
    class(wrfdiag_file), intent(inout) :: f
    character*(*), intent(in) :: path
    integer :: len, timesid, ndims, xtype,namelen, itime, ivar
    integer, dimension(NF90_MAX_VAR_DIMS) :: counts,dimids
    character*80 :: dimname
    character*80 :: message
    integer :: varid, i
    real, pointer :: data(:)
    character*4 :: name
    logical :: found, have, have_lon, have_lat, have_mslp, have_wind

10  format(A,": cannot open: ",A)
    print *,'read ',trim(path)
    f%path=path

    f%status=nf90_open(path=trim(path),mode=nf90_share,ncid=f%ncid)
    if(f%status/=nf90_noerr) &
         call wrfdiag_abort('error opening file',f)

    call f%read_global_meta()
    call f%read_dimensions()
    call f%setup_decomp_proj()
    call f%read_track()
  end subroutine open_file
end module wrfdiag_module
