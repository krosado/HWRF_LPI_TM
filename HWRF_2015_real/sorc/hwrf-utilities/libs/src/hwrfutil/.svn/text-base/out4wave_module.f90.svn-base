module out4wave_module
  ! This module reads or writes out4wave files.  It keeps in memory
  ! only one output time's data in a type(data4wave) derived type.
  ! See the test4wave.f90 example program for usage information.

  use sysutil_module, only: fail, get_unit
  implicit none

  private
  public :: data4wave, open4wave, read4wave, write4wave, close4wave, init_data4wave

  type data4wave
     ! This derived type stores data for one output time of an
     ! out4wave file
     integer :: iunit=-1, itime=0, im=0, jm=0, ierr=0
     logical :: reading=.true.
     real, dimension(:,:), pointer :: u10=>NULL(), v10=>NULL(), lat=>NULL()
     real, dimension(:,:), pointer :: lon=>NULL(), acprec=>NULL()
     ! NOTE: If you add pointers, make sure you update
     ! invalidate4wave, close4wave and open4wave
   contains
     procedure free => close4wave
     procedure close => close4wave
     procedure invalidate => invalidate4wave
     procedure write => write4wave
     procedure read => read4wave
  end type data4wave

  interface init_data4wave
     module procedure invalidate4wave
     module procedure open4wave
  end interface

contains
  subroutine invalidate4wave(dat)
    ! To assist in finding future bugs: initializes all variables to
    ! invalid values and nullifies all pointers
    class(data4wave), intent(out) :: dat
    nullify(dat%u10)
    nullify(dat%v10)
    nullify(dat%lat)
    nullify(dat%lon)
    nullify(dat%acprec)
    dat%im=0
    dat%jm=0
    dat%itime=-1
    dat%iunit=-1
  end subroutine invalidate4wave

  subroutine close4wave(this)
    ! Closes the file opened by open4wave and calls invalidate4wave
    class(data4wave), intent(inout) :: this
    if(associated(this%u10)) deallocate(this%u10)
    if(associated(this%v10)) deallocate(this%v10)
    if(associated(this%lat)) deallocate(this%lat)
    if(associated(this%lon)) deallocate(this%lon)
    if(associated(this%acprec)) deallocate(this%acprec)
    if(this%iunit>=0)      close(this%iunit)
    call invalidate4wave(this)
  end subroutine close4wave

  subroutine open4wave(dat,filepattern,idom,im_out,jm_out)
    ! Opens the data4wave file for the specified domain in the given
    ! unit, reads or writes the domain size and allocates all
    ! variables.
    !
    ! To open for writing, specify the im_out and jm_out.  Those will
    ! be the dimensions of the output grid.  To open for reading, omit
    ! those values: the dat%im and dat%jm will be set to the input
    ! file's values.
    !
    ! This subroutine does not return on error.
    implicit none
    class(data4wave), intent(out) :: dat
    character(*), intent(in) :: filepattern ! input format
    integer, intent(in) :: idom   ! domain ID
    integer, intent(in), optional :: im_out, jm_out

    character*(8), parameter :: p='<DOMAIN>'
    integer i,j,l, iunit, im,jm
    character*2 :: domainid
    character*(3) :: status

    if(present(im_out) .neqv. present(jm_out)) then
       call fail('INTERNAL ERROR: when writing, both im_out and jm_out must be specified.')
    elseif(present(im_out)) then 
       status='NEW'
    else
       status='OLD'
    endif

    if(present(im_out)) then
       if(im_out<2) call fail('Invalid im_out sent to open4wave: be >2')
    endif

    if(present(jm_out)) then
       if(jm_out<2) call fail('Invalid jm_out sent to open4wave: must be >2')
    endif

    iunit=get_unit()

    call invalidate4wave(dat)
    dat%iunit=iunit

    i=index(filepattern,p)
    if(i==0) then
       open(dat%iunit,file=trim(filepattern),status=status,form='UNFORMATTED')
    else
       l=len(trim(filepattern))
101    format("0",I1) 
102    format(I2)
       if(idom<10) then
          write(domainid,101) idom
       else
          write(domainid,102) idom
       endif
       open(dat%iunit,file=trim(filepattern(1:i-1) // domainid // &
            filepattern(i+8:l)),status=status,form='UNFORMATTED')
    endif

    if(status=='NEW') then
       im=im_out
       jm=jm_out
       write(dat%iunit) im,jm
    else
       call fail('should not reach here')
       read(dat%iunit) im,jm
    endif
    dat%im=im
    dat%jm=jm

    write(0,*) 'im=',im
    write(0,*) 'jm=',jm
    ! Allocate all arrays:
    ALLOCATE (dat%U10(IM,JM))
    ALLOCATE (dat%V10(IM,JM))
    ALLOCATE (dat%LAT(IM,JM))
    ALLOCATE (dat%LON(IM,JM))
    ALLOCATE (dat%ACPREC(IM,JM))

    ! Fill with zeros:
    !$OMP PARALLEL DO PRIVATE(i,j)
    do j=1,jm
       do i=1,im
          dat%u10(i,j)=0
          dat%v10(i,j)=0
          dat%lat(i,j)=0
          dat%lon(i,j)=0
          dat%acprec(i,j)=0
       enddo
    enddo
  end subroutine open4wave

  function write4wave(dat) result(ierr)
    ! Writes one output time to the out4wave file and increments
    ! dat%itime.  If the write fails, ierr will be non-zero.
    class(data4wave), intent(inout) :: dat
    integer :: ierr
    ierr=0
    write(dat%iunit,iostat=ierr) dat%u10
    if(ierr/=0) goto 10
    write(dat%iunit,iostat=ierr) dat%v10
    if(ierr/=0) goto 10
    write(dat%iunit,iostat=ierr) dat%lat
    if(ierr/=0) goto 10
    write(dat%iunit,iostat=ierr) dat%lon
    if(ierr/=0) goto 10
    write(dat%iunit,iostat=ierr) dat%acprec
    if(ierr/=0) goto 10
    dat%itime=dat%itime+1
    dat%ierr=0
    return
10  continue ! error recovery
    dat%ierr=ierr
    ! NOTE: Do not change itime so that it still points to last time seen
  end function write4wave

  function read4wave(dat) result(ierr)
    ! Reads one input time from the out4wave file and increments
    ! dat%itime.  If the read fails or end-of-file (EOF) is struck,
    ! ierr will be the iostat value from the read.
    class(data4wave), intent(inout) :: dat
    integer :: ierr
    ierr=0
    read(dat%iunit,iostat=ierr) dat%u10
    if(ierr/=0) goto 10
    read(dat%iunit,iostat=ierr) dat%v10
    if(ierr/=0) goto 10
    read(dat%iunit,iostat=ierr) dat%lat
    if(ierr/=0) goto 10
    read(dat%iunit,iostat=ierr) dat%lon
    if(ierr/=0) goto 10
    read(dat%iunit,iostat=ierr) dat%acprec
    if(ierr/=0) goto 10
    dat%itime=dat%itime+1
    dat%ierr=0
    return
10  continue ! error recovery
    dat%u10=-999
    dat%v10=-999
    dat%lat=-999
    dat%lon=-999
    dat%acprec=-999
    dat%ierr=ierr
    ! NOTE: Do not change itime so that it still points to last time seen
  end function read4wave
end module out4wave_module
