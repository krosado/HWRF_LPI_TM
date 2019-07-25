c     This is a fake "wrf_abort" that replaces the one in module_dm.
c     This is necessary to prevent the prep_hybrid from having to be
c     compiled as an MPI program if WRF was (which would prevent us from
c     running multiple seperate prep_hybrids in the same job)

      subroutine wrf_abort
      implicit none
      stop 93
      end subroutine wrf_abort
