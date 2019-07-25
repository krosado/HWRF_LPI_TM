	
      SUBROUTINE make_om_msk(fsm_g, dum_g, dvm_g)

!
! Author: Biju Thomas  on 2012/09/28
!

      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INCLUDE 'pom.h'
      REAL, DIMENSION(im_global,jm_global), INTENT(OUT) :: 
     &fsm_g,dum_g,dvm_g 


      CALL assemble_mpi(fsm, fsm_g)
      CALL assemble_mpi(dum, dum_g)
      CALL assemble_mpi(dvm, dvm_g)
 

      RETURN
      END SUBROUTINE make_om_msk

      
