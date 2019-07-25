program simple_aux_rw

  ! PROGRAM SIMPLE_AUX_RW: Fhis program reads in WRF NetCDF or intio files and
  !   writes simple fortran "unformatted" files that contain the HLON, HLAT,
  !   ACPREC, U10 and V10 WRF output variables.  The input and output files, 
  !   and the format (NetCDF or intio) are specified in the namelist
  !   aux.namelist.  
  ! 
  ! AUTHOR: Sam Trahan
  ! DATE: October 18, 2010

  implicit none

  integer, parameter :: realkind=4
  character(*), parameter :: namelist_filename='aux.namelist'

  ! Namelist variables:
  integer ioform  ! 1=intio 2=NetCDF (same meanings as in hwrf_driver.sh and WRF)
  character(1000) :: infile, outfile ! input, output filenames
  logical diagout ! =.true. gives more diagnostic output

  namelist /aux/ ioform,infile,outfile,diagout

  character(80) :: SysDepInfo
  character(19) :: DateStr
  integer :: status
  integer :: dh ! data handle for WRF I/O

  real(kind=realkind), allocatable :: v10(:,:), u10(:,:), &
       gdlat(:,:), gdlon(:,:), acprec(:,:), tmpvar(:,:)
  logical :: dimknown
  integer :: im, jm

  dimknown=.false. ! true = we know the grid dimensions

  ! Initialize namelist variables so we know if they were specified:
  ioform=-99
  infile=' '
  outfile=' '
  diagout=.false.

  open(unit=13,file=trim(namelist_filename),status='old')
  read(13,aux)
  close(13)

  if(ioform==2) then
     ! Initialize the WRF NetCDF IO library:
     status=999
     call ext_ncd_ioinit(SysDepInfo,status)
     call check(status==0,trim(infile),'INTIO init failed')

     ! Open the input file:
     status=999
     call ext_ncd_open_for_read( trim(infile), 0, 0, " ", &
          dh, Status)
     CALL check(status==0,trim(infile),'COULD NOT OPEN INPUT FILE')

     ! Seek to the first output time:
     status=999
     call ext_ncd_get_next_time( dh, DateStr, Status)
     call check(status==0,trim(infile),'COULD NOT FIND FIRST TIME IN FILE')

     ! Read the first variable to get the array dimensions:
     call get_variable('HLON')
     allocate(v10(im,jm),u10(im,jm),gdlat(im,jm),gdlon(im,jm),acprec(im,jm))

     ! Store the first variable's data, and read the remaining variables:
     gdlon=tmpvar
     call get_variable('HLAT')
     gdlat=tmpvar
     call get_variable('ACPREC')
     acprec=tmpvar
     call get_variable('U10')
     u10=tmpvar
     call get_variable('V10')
     v10=tmpvar

     ! Close the file:
     call ext_ncd_ioclose(dh,Status)
  elseif(ioform==1) then
     ! Initialize the WRF intio library:
     call ext_int_ioinit(SysDepInfo,status)
     call check(status==0,trim(infile),'INTIO init failed',fatal=.false.)

     ! Open the input file:
     status=999
     call ext_int_open_for_read( trim(infile), 0, 0, " ", &
          dh, Status)
     CALL check(status==0,trim(infile),'COULD NOT OPEN INPUT FILE')

     ! Find the first time in the input file:
     call ext_int_get_next_time( dh, DateStr, Status)
     call check(status==0,trim(infile),'COULD NOT FIND FIRST TIME IN FILE')

     ! Read the first variable so we know the grid dimensions:
     call get_variable('HLON')

     ! Allocate all variables to have the size of the first variable:
     allocate(v10(im,jm),u10(im,jm),gdlat(im,jm),gdlon(im,jm),acprec(im,jm))

     ! Read in all remaining variables:
     gdlon=tmpvar
     call get_variable('HLAT')
     gdlat=tmpvar
     call get_variable('ACPREC')
     acprec=tmpvar
     call get_variable('U10')
     u10=tmpvar
     call get_variable('V10')
     v10=tmpvar

     ! Close the file:
     call ext_int_ioclose(dh,Status)
  else
     ! Should only get here if the output was not netcdf or intio or
     ! if the io form was unspecified:
     write(0,*) 'Unknown value of IOFORM: ',ioform,'.  Please specify ioform=1 (intio) or ioform=2 (NetCDF) in the namelist.'
     write(6,*) 'Unknown value of IOFORM: ',ioform,'.  Please specify ioform=1 (intio) or ioform=2 (NetCDF) in the namelist.'
     stop 2
  end if

  ! Open the output file:
  open(77,file=trim(outfile),form='unformatted',STATUS='UNKNOWN')

  ! Write the grid dimensions:
  WRITE(77) im,jm
  PRINT *,'I J ',im,jm

  ! Write out all variables:
  WRITE(77) U10
  call var_summary('U10',U10,im,jm,diagout,'10-meter Grid-Relative U wind (m/s)')
  WRITE(77) V10
  call var_summary('V10',V10,im,jm,diagout,'10-meter Grid-Relative V wind (m/s)')
  WRITE(77) GDLAT
  call var_summary('HLAT',GDLAT,im,jm,diagout,'Mass point latitudes (degrees)')
  WRITE(77) GDLON
  call var_summary('HLON',GDLON,im,jm,diagout,'Mass point longitudes (degrees)')
  WRITE(77) ACPREC
  call var_summary('ACPREC',ACPREC,im,jm,diagout,'One hour accumulated precipitation (m)')
  CLOSE(77)

contains

  subroutine check(status,str1,str2,fatal)
    ! This is a convenience function that aborts with a useful message if the
    ! logical value "status" is false.
    implicit none
    logical, intent(in) :: status
    logical, intent(in), optional :: fatal
    character(*), intent(in) :: str1,str2

    if(.not. status) then
       write(0,*) trim(str1),': ',trim(str2)
       write(6,*) trim(str1),': ',trim(str2)
       if(present(fatal)) then
          if(.not.fatal) then
             write(0,*) trim(str1),': continuing anyway...'
             write(6,*) trim(str1),': continuing anyway...'
             return
          endif
       endif
       stop 99
    end if
  end subroutine check

  subroutine get_variable(varname)
    ! This function reads the specified variable into tmpvar.  If this
    ! is the first variable read in, then it will also set IM and JM to the
    ! grid dimensions.  Otherwise, it aborts if the variable does not have
    ! dimensions IM x JM.
    implicit none
    character(*), intent(in) :: varname
    integer, dimension(4) :: si,ei,si2,ei2 ! start and end indices
    integer :: ndim, WrfType, ierr
    real(kind=realkind), allocatable :: data(:,:,:,:)
    character(3) :: ordering
    character(4) :: staggering
    character(80) :: dimnames
    character(1000) :: nametemp

    character(132) :: Stagger
    integer :: i,j

    write(0,*) 'Read: ',trim(varname)
    write(6,*) 'Read: ',trim(varname)

    si=1
    ei=1

    if(ioform==1) then
       do
          ierr=-999
          nametemp=' '
          call ext_int_get_next_var(dh,nametemp,ierr)
          call check(ierr==0,nametemp,'unable to find in input file')
          if(nametemp == varname) then
             ! Found the variable
             exit
          endif
       end do
    end if

    if(ioform==2) then
       call ext_ncd_get_var_info(dh,trim(varname),ndim,ordering,Stagger, &
            si,ei,WrfType,ierr)
    else
       call ext_int_get_var_info(dh,trim(varname),ndim,ordering,Stagger, &
            si,ei,WrfType,ierr)
    endif
    call check(ierr==0,trim(varname),'unable to get var info')
    ! should check this, but need to change makefile to use cpp:
    ! call check(WrfType==WRF_REAL,trim(varname),'is not a 32-bit real variable')

    call check(ndim==2,trim(varname),'is not a two dimensional variable')

    if(dimknown) then
       call check(im==ei(1)-si(1)+1,trim(varname),'has different X dimensions that other variables')
       call check(jm==ei(2)-si(2)+1,trim(varname),'has different Y dimensions that other variables')
    else
       im=ei(1)-si(1)+1
       jm=ei(2)-si(2)+1
       write(0,*) 'From "',trim(varname),'", IM & JM = ',im,jm
       write(6,*) 'From "',trim(varname),'", IM & JM = ',im,jm
       allocate(tmpvar(im,jm))
       tmpvar=-888
       dimknown=.true.
    endif

    print *,'Allocate, ',si(1),ei(1),si(2),ei(2),si(3),ei(3),si(4),ei(4)

    allocate(data(si(1):ei(1),si(2):ei(2),si(3):ei(3),si(4):ei(4)))
    data=-999

    si2=si
    ei2=ei

    ierr=999
    if(ioform==2) then
       call ext_ncd_read_field(dh,DateStr,trim(varname),data,WrfType,0,0,0, &
            ordering,staggering,dimnames, si2,ei2,si2,ei2,si2,ei2,ierr)
    else
       call ext_int_read_field(dh,DateStr,trim(varname),data,WrfType,0,0,0, &
            ordering,staggering,dimnames, si2,ei2,si2,ei2,si2,ei2,ierr)
    end if
    call check(ierr==0,trim(varname),'unable to read var from file')

    tmpvar=-888
    do i=1,im
       do j=1,jm
          tmpvar(i,j)=data(si(1)+i-1,si(2)+j-1,si(3),si(4))
       enddo
    enddo

    deallocate(data)

  end subroutine get_variable

end program simple_aux_rw
