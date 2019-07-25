module height_reader

  implicit none
 
  private
  public :: read_geogrid_heights, rotlatlon_info

  ! height_real: real value size in bytes for height, lat and lon arrays.
  ! The size of the height real numbers must be 4 bytes since that is what
  ! is used by the WRF IO framework
  integer, public, parameter :: height_real = 4

  type rotlatlon_info
     ! Grid dimensions:
     INTEGER          :: nnxp ! number of mass points in an odd row
     INTEGER          :: nnyp ! number of rows

     ! Distance between mass point and nearest velocity points in 
     ! rotated grid X and Y directions in degrees:
     REAL             :: dx, dy

     real :: clat, clon ! Projection center lat,lon in degrees (NOT domain center)
  end type rotlatlon_info

  integer, parameter :: origform=-999  ! indicates we have not checked $IO_FORM
  integer, parameter :: defaultform=1  ! used if $IO_FORM is unset
  integer, save :: saveform=origform   ! requested IO form

contains

  subroutine set_io_form(form)
    implicit none
    integer, intent(in) :: form
    saveform=form
  end subroutine set_io_form

  integer function get_io_form()
    implicit none
    integer :: form
    character(17039) :: formstr

    if(saveform==origform) then
       ! We have not yet checked the io form and the user has not provided
       ! one via set_io_form.  Now check the IO_FORM env. var.
       saveform=1
       form=1
       formstr=" "
       
       call getenv("IO_FORM",formstr)
       write(6,*) 'WHAZZOW!! IO_FORM="',formstr,'".  I am gonna parse that.'
       if(formstr == " ") then
          form=defaultform
          return
       endif
       
       read(formstr,'(I1)') saveform
       write(6,*) '  WELL.  I GOT FORM ',saveform,' FROM THAT.'
       if(saveform/=1 .and. saveform/=2) then
          write(6,*) 'BAD NAUGHTY FORM!! $IO_FORM="',trim(formstr),'", but it is only allowed to be 1 (binary) or 2 (netcdf).  NAUGHTY!! FIX IT!'
          write(0,*) 'BAD NAUGHTY FORM!! $IO_FORM="',trim(formstr),'", but it is only allowed to be 1 (binary) or 2 (netcdf).  NAUGHTY!! FIX IT!'
          stop 21
       endif
    endif

    get_io_form=saveform

  end function get_io_form

  SUBROUTINE gen_lat_lon(proj,lat,lon,hgrid)

    ! This routine was modified from ijll_rotlatlon in the WPS

    IMPLICIT NONE

    ! Arguments
    REAL(kind=height_real), allocatable, dimension(:,:), INTENT(OUT) :: lat, lon
    logical, optional :: hgrid
    TYPE (rotlatlon_info), INTENT(IN) :: proj
    integer :: modeq

    ! Local variables
    INTEGER :: i,j,index
    INTEGER :: midcol,midrow,ncol
    real :: phi,lambda, tmplat, tmplon
    REAL :: dphd,dlmd !Grid increments, degrees
    REAL(KIND=8) :: arg1,arg2,d2r,fctr,glatr,glatd,glond,pi, &
         r2d,tlatd,tlond,tlatr,tlonr,tlm0,tph0

    modeq=0
    if(present(hgrid)) then
       if(.not. hgrid) then
          modeq=1
       endif
    endif

    allocate(lat(proj%nnxp,proj%nnyp))
    allocate(lon(proj%nnxp,proj%nnyp))

    dphd=proj%dy
    dlmd=proj%dx

    pi = ACOS(-1.0)
    d2r = pi/180.
    r2d = 1./d2r
    tph0 = proj%clat*d2r
    tlm0 = -proj%clon*d2r

    midrow = (proj%nnyp+1)/2
    midcol = proj%nnxp

    do j=1,proj%nnyp
       do index=1,proj%nnxp
          if(mod(j,2)==modeq) then ! modeq==0 means H grid, 1 means V grid
             i=index*2
          else
             i=index*2-1
          endif
          tlatd = (j-midrow)*dphd
          tlond = (i-midcol)*dlmd
          
          tlatr = tlatd*d2r
          tlonr = tlond*d2r
          arg1 = SIN(tlatr)*COS(tph0)+COS(tlatr)*SIN(tph0)*COS(tlonr)
          glatr = ASIN(arg1)
          
          glatd = glatr*r2d
          
          arg2 = COS(tlatr)*COS(tlonr)/(COS(glatr)*COS(tph0))-TAN(glatr)*TAN(tph0)
          IF (ABS(arg2) > 1.) arg2 = ABS(arg2)/arg2
          fctr = 1.
          IF (tlond > 0.) fctr = -1.
          
          glond = tlm0*r2d+fctr*ACOS(arg2)*r2d
          
          tmplat = glatd
          tmplon = -glond
          
          IF (tmplon .GT. +180.) tmplon = tmplon - 360.
          IF (tmplon .LT. -180.) tmplon = tmplon + 360.
          
          lat(index,j) = tmplat*d2r
          lon(index,j) = tmplon*d2r
       enddo
    enddo

  END SUBROUTINE gen_lat_lon

  subroutine skip_var_metadata(handle)
    integer, intent(in) :: handle
    integer :: istatus,csx,csy,junk
    character(len=1000) :: cunits,cdesc,cstagger

    if(get_io_form() == 1) then
       istatus=0
       do while (istatus == 0) 
          call ext_int_get_var_ti_char(handle, 'units', 'VAR', cunits, istatus)
          if (istatus == 0) then
             call ext_int_get_var_ti_char(handle, 'description', 'VAR', cdesc, istatus)
             if (istatus == 0) then
                call ext_int_get_var_ti_char(handle, 'stagger', 'VAR', cstagger, istatus)
                if (istatus == 0) then
                   call ext_int_get_var_ti_integer(handle, 'sr_x', &
                        'VAR', csx, 1, junk, istatus)
                   if(istatus==0) then
                      call ext_int_get_var_ti_integer(handle, 'sr_y', &
                           'VAR', csy, 1, junk, istatus)
                   end if
                end if
             end if
          end if
       end do
    endif
  end subroutine skip_var_metadata

!  subroutine read_geogrid_heights(proj,heights,lats,lons,filename,ierr)
  subroutine read_geogrid_heights(proj,heights,landmask,filename, &
       expect_nx,expect_ny,ierr)

    ! This function reads the HGT_M and LANDMASK variables from the
    ! specified file, reads projection information, and generates latitudes
    ! and longitudes for that projection.  The file is assumed to be a
    ! WPS version 3.2 file written out by the geogrid program.  
    !
    ! The ierr variable will be set to one of the following values:
    !  0 = success
    !  1 = unable to initialize wrf io_int library
    !  2 = unable to open the file for reading
    !  3 = unable to find the initial date in the file
    !  4 = unable to read a field from the file (either an IO error,
    !      or we hit the end of the file without finding one of the
    !      variables)
    !  5 = found a variable, but was unable to read the variable
    !      info (ext_int_get_var_info)
    !  6 = found a variable but was unable to read the variable
    !      data (ext_int_read_field)
    !  11-14 = unable to read dx, dy, cen_lat or cen_lon (in that order)
    !  30 = a variable was 3D (it should be 2D)
    !  40 = invalid or missing projection information in file
    !  50 = an array was too small in X or Y direction (dim<=1)
    !  61 = X dimension did not have expected size
    !  62 = Y dimension did not have expected size
    !  -999 = internal error; ierr was never set (should never happen)

    implicit none

#include "wrf_io_flags.h"

    integer, intent(out) :: ierr
    real(kind=height_real), intent(out),allocatable,dimension(:,:) :: heights !,lats,lons
    real(kind=height_real), intent(out),allocatable,dimension(:,:) :: landmask !,lats,lons
    integer, intent(in) :: expect_nx, expect_ny
    character(*), intent(in) :: filename

    real(kind=height_real),allocatable, dimension(:,:,:) :: real_domain
    character(len=128) :: cname,stagger,varname
    character(len=3) :: memorder
    integer, dimension(3) :: domain_start,domain_end
    integer, dimension(3) :: dstart,dend,mstart,mend,pstart,pend
    integer :: comm_1,comm_2,handle,istatus,ndim,wrftype,iostatus,vars_found
    character(len=20) :: datestr
    character(len=128),dimension(3) :: dimnames

    logical :: rdalloced
    real :: dx,dy,cen_lon,cen_lat
    integer :: nnxp,nnyp,outcount,x,y

    integer, parameter :: max_loop_count = 1000

    integer :: loop_count

    type(rotlatlon_info) :: proj

    rdalloced=.false.

    cname=' '
    memorder=' '
    stagger=' '
    varname=' '
    dimnames=' '
    datestr=' '

    ierr=-999
    comm_1=1
    comm_2=1

    ! First, initialize the relevant WRF I/O library, and open the file for reading.
    ! Also, with intio, we have to skip variable metadata here, otherwise
    ! the read calls farther down will fail.

    iostatus=0
    if(get_io_form()==1) then
       write(6,*) 'EXT_INT_IOINIT...'

       call ext_int_ioinit('sysdep info', istatus)
       ! iostatus is never set by ext_int_ioinit, so it ends up getting filled with
       ! a junk value (whatever happens to be on the stack upon exit of that
       ! subroutine).  Hence, we have to disable the check here
       !  if(istatus/=0) then
       !     write(6,*) trim(filename),': unable to initialize WRF io_int library.  istatus=',istatus
       !     ierr=1
       !     return
       !  end if
       
       write(6,*) 'EXT_INT_OPEN_FOR_READ "',filename,'"...'
       call ext_int_open_for_read(trim(filename), comm_1, comm_2, &
            'sysdep info', handle, istatus)
       ! ext_int_open_for_read also does not bother setting the istatus
       !   if(istatus/=0) then
       !      write(6,*) trim(filename),': unable to open file for reading.  istatus=',istatus
       !      ierr=2
       !      return
       !   end if
       
       write(6,*) 'SKIP VARIABLE METADATA...'
       call skip_var_metadata(handle)
    else
       write(6,*) 'EXT_NCD_IOINIT...'
       call ext_ncd_ioinit('sysdep info',istatus)
       if(istatus/=0) then
          write(6,*) trim(filename),': unable to initialize WRF NetCDF I/O library.  istatus=',istatus
          ierr=1
          goto 111
       end if

       write(6,*) 'EXT_NCD_OPEN_FOR_READ "',filename,'"...'
       call ext_ncd_open_for_read(trim(filename), comm_1, comm_2, &
            'sysdep info', handle, istatus)
       if(istatus/=0) then
          write(6,*) trim(filename),': unable to open.  istatus=',istatus
          ierr=2
          goto 111
       end if

       write(6,*) 'SKIP VARIABLE METADATA...'
       call skip_var_metadata(handle)
    endif

    ! Read projection information:
    dx=-999.999
    dy=-999.999
    cen_lon=-999.999
    cen_lat=-999.999
    write(6,*) 'GET SOME DOMAIN METADATA...'

    if(.not. getvar(dx,'DX',11)) goto 999
    if(.not. getvar(dy,'DY',12)) goto 999
    if(.not. getvar(cen_lat,'CEN_LAT',13)) goto 999
    if(.not. getvar(cen_lon,'CEN_LON',14)) goto 999

    ! Check projection information for sanity:
    write(6,*) 'dx=',dx,' dy=',dy,' cen_lat=',cen_lat,' cen_lon=',cen_lon
    if(cen_lat<-90.01 .or. cen_lat>90.01 .or. dx<=0 .or. dy<=0) then
       write(6,*) 'INVALID PROJECTION INFORMATION'
       ierr=40
       goto 999
    endif

    ! Seek to the first output time:
    if(get_io_form()==1) then
       write(6,*) 'EXT_INT_GET_NEXT_TIME'
       call ext_int_get_next_time(handle, datestr, istatus)
    else
       write(6,*) 'EXT_NCD_GET_NEXT_TIME'
       call ext_ncd_get_next_time(handle, datestr, istatus)
    endif
    if(istatus/=0) then
       write(6,*) trim(filename),': unable to find first date entry.  istatus=',istatus
       ierr=3
       goto 999
    end if
    write(6,*) 'EXT_*_GET_NEXT_TIME SAID DATESTR="',trim(datestr),'".'

    ! Keep track of how many variables we have found.  We should finish with 
    ! vars_found=2 when we have found LANDMASK and HGT_M (other variables
    ! do not count towards this total):
    vars_found=0

    ! Avoid infinite loops:
    loop_count=-1

    ! Loop until we have both LANDMASK and HGT_M:
    do while(vars_found<2)
       loop_count=loop_count+1

       if(loop_count > max_loop_count) then
          write(6,*) filename,': looped over more than ',max_loop_count,' variables, and did not find both vars (HGT_M and LANDMASK).  Aborting.'
          ierr=4
          goto 999
       endif

       cname=' '

       ! Determine which variable we are processing now.
       if(get_io_form()==1) then
          ! intio has to seek through variables one-by-one, so we need
          ! a call to ext_get_next_var here, which will fill cname with
          ! the name of the next variable in the file.
          write(6,*) 'EXT_INT_GET_NEXT_VAR...'
          call ext_int_get_next_var(handle, cname, istatus)
          write(6,*) 'EXT_*_GET_NEXT_VAR SAID "',trim(cname),'"!!'
          write(6,*) 'FOUND ME A(N) ',cname,'!!  GONNA READ IT!!'
       else
          ! NetCDF does not have to seek through all variables one by one,
          ! so no call to ext_ncd_get_next_var is needed.  Instead, we will
          ! let ext_ncd_get_var_info automatically seek to the wanted
          ! variable for us.
          if(vars_found==0) then
             cname='LANDMASK'
          else
             cname='HGT_M'
          endif
          istatus=0
       endif
       if(istatus/=0) then
          write(6,*) trim(filename),': unable to read next field, and have not found surface height and/or land mask yet.  istatus=',istatus
          ierr=4
          goto 999
       end if

       varname=trim(cname)

       ! Avoid processing unwanted variables:
       if(trim(cname) /= 'HGT_M' .and. trim(cname) /= 'LANDMASK') then
          write(6,*) trim(filename),': ignoring unneeded var "',trim(cname),'".'
          cycle
       endif

       ! Initialize variables to values that will let us work around buggy
       ! library calls that don't bother to set some variables' contents:
       domain_start=-999999
       domain_end=-999999
       wrftype=0
       memorder=' '
       stagger=' '
       
       ! Read array sizes, types, etc:
       if(get_io_form()==1) then
          write(6,*) 'EXT_INT_GET_VAR_INFO cname="',trim(cname),'"...'
          call ext_int_get_var_info(handle, cname, ndim, memorder, stagger, domain_start, domain_end, wrftype, istatus)
       else
          write(0,*) 'EXT_NCD_GET_VAR_INFO cname="',trim(cname),'"...'
          write(6,*) 'EXT_NCD_GET_VAR_INFO cname="',trim(cname),'"...'
          call ext_ncd_get_var_info(handle, 'HGT_M', ndim, memorder, stagger, domain_start, domain_end, wrftype, istatus)
          write(0,*) 'BACK FROM EXT_NCD_GET_VAR_INFO'
          write(6,*) 'BACK FROM EXT_NCD_GET_VAR_INFO'
       endif

       ! See if the third dimension size was ever modified:
       if(domain_start(3)==-999999 .or. domain_end(3)==-999999) then
          ! Either begin or end array index was not changed, so set both to 1.
          domain_start(3)=1
          domain_end(3)=1
       endif
       
       if(istatus/=0) then
          write(6,*) trim(filename),': unable to read field "',trim(cname),'".  istatus=',istatus
          ierr=5
          goto 999
       end if
       
       write(6,*) 'domain_start=',domain_start
       write(6,*) 'domain_end=',domain_end
       
       ! Determine array sizes in X&Y:
       nnxp=domain_end(1)-domain_start(1)+1
       nnyp=domain_end(2)-domain_start(2)+1

       ! Check for common errors in third domain size.  The wrf intio
       ! library can be pretty sloppy since many input and output
       ! variables in its subroutines are uninitialized:
       if(domain_start(3)<0 .or. domain_end(3)<0 &
            .or. domain_start(3)/=domain_end(3)) then
          domain_start(3)=1
          domain_end(3)=1
       endif
       
       ! Ensure that the data is not flat in the X and Y dimension:
       if(nnxp<=1 .or. nnyp<=1) then
          write(6,*) 'ERROR: X OR Y DIMENSION IS TOO SMALL (must be >1)'
          ierr=50
          goto 999
       endif

       ! Make sure we have the correct X and Y dimension size
       if(nnxp /= expect_nx) then
          write(6,*) 'X dimension = ',nnxp,' BUT ',expect_nx,' WAS EXPECTED.'
          ierr=61
          goto 999
       end if
       if(nnyp /= expect_ny) then
          write(6,*) 'Y dimension = ',nnyp,' BUT ',expect_ny,' WAS EXPECTED.'
          ierr=62
          goto 999
       end if

       ! Allocate the real_domain array, which we will use for reading:
       write(6,*) 'ALLOCATE real_domain nnxp=',nnxp,' nnyp=',nnyp,' z dim = ', &
            domain_end(3)-domain_start(3)+1
       write(6,*) '  real_domain(',domain_start(1),':',domain_end(1),',', &
                                   domain_start(2),':',domain_end(2),',', &
                                   domain_start(3),':',domain_end(3),')'

       allocate(real_domain(domain_start(1):domain_end(1), &
            domain_start(2):domain_end(2), &
            domain_start(3):domain_end(3)))
       rdalloced=.true.

       ! Create temporary copies of the real_domain array sizes for convenience:
       dstart=domain_start
       dend=domain_end
       mstart=domain_start
       mend=domain_end
       pstart=domain_start
       pend=domain_end

       ! Read the data into the real_domain variable:
       real_domain=-888.888
       if(get_io_form()==1) then
          write(6,*) 'EXT_INT_READ_FIELD...'
          call ext_int_read_field(handle, '0000-00-00_00:00:00', varname, real_domain, WRF_REAL, &
               1, 1, 0, memorder, stagger, &
               dimnames, dstart,dend,mstart,mend,pstart,pend,istatus)
       else
          write(6,*) 'EXT_NCD_READ_FIELD...'
          call ext_ncd_read_field(handle, '0000-00-00_00:00:00', varname, real_domain, WRF_REAL, &
               1, 1, 0, memorder, stagger, &
               dimnames, dstart,dend,mstart,mend,pstart,pend,istatus)
       endif
       if(istatus/=0) then
          write(6,*) trim(filename),': unable to read field "',cname,'".  istatus=',istatus
          ierr=6
          goto 888
       end if

       ! Make sure we are given a variable that is flat in the vertical:
       if(domain_start(3)/=domain_end(3)) then
          write(6,*) trim(filename),': HGT_M variable was 3D but should have been 2D.'
          ierr=30
          goto 888
       endif

       ! Allocate the output array, and copy the data there.
       if(trim(cname) == 'HGT_M') then
          write(6,*) 'ALLOCATE HEIGHTS ARRAY...'
          allocate(heights(1:nnxp,1:nnyp))
          write(6,*) 'COPY OVER TO DATA ARRAY...'
          heights=-999.999
          do y=1,nnyp
             do x=1,nnxp
                heights(x,y)=real_domain(domain_start(1)+x-1, &
                     domain_start(2)+y-1,domain_start(3))
             enddo
          enddo
       elseif(trim(cname) == 'LANDMASK') then
          write(6,*) 'ALLOCATE LANDMASK ARRAY...'
          allocate(landmask(1:nnxp,1:nnyp))
          write(6,*) 'COPY OVER TO DATA ARRAY...'
          landmask=-999.999
          do y=1,nnyp
             do x=1,nnxp
                landmask(x,y)=real_domain(domain_start(1)+x-1, &
                     domain_start(2)+y-1,domain_start(3))
             enddo
          enddo
       else
          write(6,*) trim(filename),': INTERNAL ERROR!!!!  SHOULD NEVER REACH THIS LINE.  trim(cname)="',trim(cname),'" != HGT_M or LANDMASK.'
          stop 234
       endif

       if(rdalloced) then
          write(6,*) 'Deallocating real_domain.'
          deallocate(real_domain)
          rdalloced=.false.
       endif
       
       ! Record that we processed one more variable so the loop will exit
       ! at the right time, and so that the netcdf mode will switch to
       ! the next variable.
       vars_found=vars_found+1
    enddo

    ! We only get here if we successfully read in the heights and landmask.
    ! Now we need to calculate the lat & lon
    write(6,*) 'GET LATS AND LONS...'
    proj%nnxp=nnxp
    proj%nnyp=nnyp
    proj%dx=dx
    proj%dy=dy
    proj%clat=cen_lat
    proj%clon=cen_lon

!    call gen_lat_lon(proj,lats,lons)

    write(6,*) 'SUCCESS IN READ_GEOGRID_HEIGHTS.'

    write(6,*) 'proj = ',proj
    write(6,*) 'heights(1:10,1) = ',heights(1:10,1)
!    write(6,*) '   lats(1:10,1) = ',lats(1:10,1)
!    write(6,*) '   lons(1:10,1) = ',lons(1:10,1)
    write(6,*) 'heights(1,1:10) = ',heights(1,1:10)
!    write(6,*) '   lats(1,1:10) = ',lats(1,1:10)
!    write(6,*) '   lons(1,1:10) = ',lons(1,1:10)
    
    ierr=0
    
888 continue   ! we jump here on error, if we have allocated real_domain
    if(rdalloced) then
       write(6,*) 'Deallocating real_domain.'
       deallocate(real_domain)
    endif

999 continue   ! we jump here on error if we have NOT allocated real_domain
    if(get_io_form()==1) then
       call ext_int_ioclose(handle,istatus)
    else
       call ext_ncd_ioclose(handle,istatus)
    endif
111 continue ! we jump here on error if we have not yet opened the file
    return

  contains
    logical function getvar(outval,name,errnum)
      character(*),intent(in) :: name
      integer, intent(in) :: errnum
      real, intent(out) :: outval

      getvar=.false.
      if(get_io_form()==1) then
         call ext_int_get_dom_ti_real   (handle,trim(name),outval,1,outcount,istatus)
      else
         call ext_ncd_get_dom_ti_real   (handle,trim(name),outval,1,outcount,istatus)
      endif
      if(istatus/=0) then
         write(6,*) trim(filename),': unable to read ',trim(name)
         ierr=errnum
         return
      end if
      getvar=.true.
    end function getvar

  end subroutine read_geogrid_heights
end module height_reader
