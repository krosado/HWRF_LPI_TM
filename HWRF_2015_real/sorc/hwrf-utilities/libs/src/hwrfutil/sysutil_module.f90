module sysutil_module
  implicit none
  integer, parameter :: arglen=1000
  integer, parameter :: firstunit=100, lastunit=1000
  integer, parameter :: logunit = 0

contains

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

  integer function get_unit(first,last)
    ! Returns the first ununsed unit number found between first and
    ! last, inclusive.  Both arguments are optional: module variables
    ! firstunit and lastunit are the defaults.
    implicit none
    integer, optional :: first,last
    logical :: opened
    integer :: ifirst,ilast
    ifirst=firstunit
    ilast=lastunit
    if(present(first)) ifirst=first
    if(present(last)) ilast=last

    get_unit=-1
    do get_unit=ifirst,ilast
       inquire(unit=get_unit,opened=opened)
       if(.not.opened) return
    enddo
    print *,'return unit ',get_unit
    call fail('Unable to find an unused unit number in get_unit.')
  end function get_unit

  subroutine fail(why)
    ! fail -- quits the program with an error message, after trim()ing it
    !   message -- an error message
    ! Does not return.
    implicit none
    integer :: intentionally_uninitialized
    character*(*),intent(in) :: why
    write(logunit,'(A)') trim(why)
    print *,intentionally_uninitialized
    stop 13
  end subroutine fail

  subroutine warn(warning)
    ! warn -- prints a warning to the log file or unit after
    !   trimming it
    implicit none
    character(*), intent(in) :: warning
    write(logunit,'(A)') trim(warning)
  end subroutine warn

end module sysutil_module
