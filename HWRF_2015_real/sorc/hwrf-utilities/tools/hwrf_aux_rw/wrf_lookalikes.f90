! this file contains non-MPI versions of some WRF routines

subroutine wrf_message(str)
  implicit none
  character(*), intent(in) :: str
  write(0,*) trim(str)
  write(6,*) trim(str)
end subroutine wrf_message

subroutine wrf_error_fatal(str)
  implicit none
  character(*), intent(in) :: str
  call wrf_message(str)
  stop 9
end subroutine wrf_error_fatal

subroutine wrf_debug(level,str)
  implicit none
  character(*), intent(in) :: str
  integer, intent(in) :: level
  if(level<2) then
     call wrf_message(str)
  end if
end subroutine wrf_debug

