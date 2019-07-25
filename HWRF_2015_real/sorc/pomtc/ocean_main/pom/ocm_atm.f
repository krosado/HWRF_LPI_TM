	
      SUBROUTINE ocm_atm(tma,success)

!
! Author: Biju Thomas  on 2012/09/28
! Providing bottom boundary condition  to component model
!


      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INCLUDE 'pom.h'
      LOGICAL,                 INTENT(OUT) :: success
      INTEGER i,j,ierr,nday
      REAL, DIMENSION(im_global,jm_global),INTENT(OUT) :: 
     & tma
      REAL, DIMENSION(im_local,jm_local) :: t2d
      CHARACTER                          :: fn*15, dout*4
      

      tma(:,:) = 0.;

      DO i=1,im
      DO j=1,jm
       t2d(i,j)= t(i,j,1) + tbias + 273.15
!Biju       IF (fsm(i,j) .LT. 0.001) t2d(i,j)=-999.999
      ENDDO
      ENDDO
      CALL assemble_mpi(t2d, tma)

      IF(my_task .EQ. 0) THEN
       IF (MOD(iint*dti,86400.) .EQ. 0. ) THEN
          nday = iint*dti/86400.
          WRITE(dout,'(I4.4)')nday
          fn='sst.'//dout 
          OPEN(1,FILE=fn,FORM='UNFORMATTED')
          WRITE(1)TMA
          CLOSE(1)
       ENDIF
      ENDIF


      success = .true.

      RETURN
      END SUBROUTINE ocm_atm

      
