	
      SUBROUTINE assemble_mpi(brick,wall)
!
! Author: Biju Thomas, URI/GSO,  on 2012/09/28
!
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INCLUDE 'pom.h'
      
      REAL, DIMENSION(im_local,jm_local),INTENT(IN):: brick
      REAL, DIMENSION(im_global,jm_global),INTENT(OUT) :: wall
      INTEGER i,j,is,ie,js,je
      INTEGER ierr
      REAL, DIMENSION(im_global,jm_global)  :: tmp


      wall(:,:) = 0.0

      IF (i_global(1) .GT. 1 .AND. i_global(im) .LT. 
     &                im_global) THEN 
        is = 2
        ie = imm1
      END IF
      IF (i_global(1) .EQ. 1 .AND. i_global(im) .LT. 
     &                im_global) THEN 
        is = 1
        ie = imm1
      END IF
      IF (i_global(1) .GT. 1 .AND. i_global(im) .EQ.
     &                im_global) THEN
        is = 2
        ie = im
      ENDIF 
      IF (i_global(1) .EQ. 1 .AND. i_global(im) .EQ.
     &                im_global) THEN
        is = 1
        ie = im
      ENDIF


      IF (j_global(1) .GT. 1 .AND. j_global(jm) .LT.
     &                jm_global) THEN
        js = 2
        je = jmm1
      END IF
      IF (j_global(1) .EQ. 1 .AND. j_global(jm) .LT.
     &                jm_global) THEN
        js = 1
        je = jmm1
      END IF
      IF (j_global(1) .GT. 1 .AND. j_global(jm) .EQ.
     &                jm_global) THEN
        js = 2
        je = jm
      ENDIF
      IF (j_global(1) .EQ. 1 .AND. j_global(jm) .EQ.
     &                jm_global) THEN
        js = 1
        je = jm
      ENDIF

      DO j=js, je
      DO i=is, ie
        tmp(i_global(i),j_global(j)) = brick(i,j)
      ENDDO
      ENDDO

      CALL  sum_om_mpi(im_global,jm_global, tmp, wall)

      RETURN
      END SUBROUTINE assemble_mpi

      SUBROUTINE spread_mpi(wall,brick)
!
! Author: Biju Thomas, URI/GSO,  on 2012/09/28
!

      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INCLUDE 'pom.h'

      REAL, DIMENSION(im_global,jm_global),INTENT(IN) :: wall
      REAL, DIMENSION(im_local,jm_local),INTENT(OUT) :: brick
      INTEGER i,j


      brick(:,:) = 0.0

      DO j=1, jm_local
      DO i=1, im_local
        brick(i,j) = wall(i_global(i),j_global(j))
      ENDDO
      ENDDO

      RETURN
      END SUBROUTINE spread_mpi


      SUBROUTINE sum_om_mpi(nx, ny, input, output) 
!
! Author: Biju Thomas, URI/GSO,  on 2012/09/28
!

      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INCLUDE 'pom.h'

      INTEGER, INTENT(IN)   :: nx, ny
      REAL, DIMENSION(nx,ny), INTENT(IN)   :: input
      REAL, DIMENSION(nx,ny), INTENT(OUT)  :: output       
      INTEGER ierr 
      
      CALL mpi_allreduce(input, output, nx*ny, mpi_REAL, 
     $mpi_sum, pom_comm, ierr) 

      RETURN
      END SUBROUTINE sum_om_mpi
      
