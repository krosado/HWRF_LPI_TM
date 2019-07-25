	
      SUBROUTINE atm_ocm(wusurf_g, wvsurf_g, wtsurf_g,
     &        swrad_g, success)

!
! Author: Biju Thomas  on 2012/09/28
! Providing forcing terms to component model 
!

      IMPLICIT NONE
      INCLUDE 'pom.h'

      REAL, DIMENSION(im_global,jm_global), INTENT(IN) ::
     & wusurf_g, wvsurf_g, wtsurf_g, swrad_g
      REAL, DIMENSION(im_global,jm_global)  :: usurf,vsurf,hsurf
      LOGICAL,                 INTENT(OUT) :: success
      INTEGER i,j , nday
      CHARACTER                          :: fn*15, dout*4


      IF (nbct .eq. 1) THEN
       CALL spread_mpi(wtsurf_g + swrad_g, wtsurf)
      ELSE
       CALL spread_mpi(wtsurf_g, wtsurf)
      END IF
      CALL spread_mpi(swrad_g, swrad)
      CALL spread_mpi(wusurf_g, wusurf)
      CALL spread_mpi(wvsurf_g, wvsurf)

 
      success = .true.

      IF(iint .eq. 2 ) THEN
       CALL assemble_mpi(wusurf, usurf)
       CALL assemble_mpi(wvsurf, vsurf)
       CALL assemble_mpi(wtsurf, hsurf)
      ENDIF

      IF( MOD(iint*dti,86400.0) .EQ. 0.)THEN
        CALL assemble_mpi(wusurf, usurf)
        CALL assemble_mpi(wvsurf, vsurf)
        CALL assemble_mpi(wtsurf, hsurf)
        IF( my_task .EQ. 0 ) THEN
          nday = iint*dti/86400.
          WRITE(dout,'(I4.4)')nday
          fn='flux.'//dout
          OPEN(1,FILE=fn,FORM='UNFORMATTED')
          WRITE(1)usurf
          WRITE(1)vsurf
          WRITE(1)hsurf
          CLOSE(1)
        ENDIF
      ENDIF


      RETURN
      END SUBROUTINE atm_ocm

