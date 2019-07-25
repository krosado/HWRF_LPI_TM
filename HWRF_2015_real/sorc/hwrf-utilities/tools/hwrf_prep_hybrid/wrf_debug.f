      subroutine wrf_debug(i,s)
        implicit none
        integer :: i
        character(*) :: s
        write(6,*) s
      end subroutine wrf_debug
