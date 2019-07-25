	
      SUBROUTINE make_om_grid(xgrid_t,xgrid_u,xgrid_v,
     & ygrid_t,ygrid_u,ygrid_v)

C Providing bottom boundary condition  to component model
C Author: Biju Thomas  on 2012/08/28


      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INCLUDE 'pom.h'
      INTEGER                        :: i, j
      REAL, DIMENSION(im_global,jm_global), INTENT(OUT) :: 
     &xgrid_t,ygrid_t,xgrid_u,ygrid_u,xgrid_v,ygrid_v 


       CALL assemble_mpi(east_e, xgrid_t)
       CALL assemble_mpi(north_e,ygrid_t)
       
       CALL assemble_mpi(east_u, xgrid_u) 
       CALL assemble_mpi(north_u,ygrid_u)

       CALL assemble_mpi(east_v, xgrid_v)
       CALL assemble_mpi(north_v,ygrid_v)
       IF (xgrid_t(1,1) .LT. 0.0) THEN
          xgrid_t = (xgrid_t + 360.) * deg2rad
          xgrid_u = (xgrid_u + 360.) * deg2rad
          xgrid_v = (xgrid_v + 360.) * deg2rad
       ELSE
          xgrid_t = xgrid_t * deg2rad
          xgrid_u = xgrid_u * deg2rad
          xgrid_v = xgrid_v * deg2rad
       ENDIF
       ygrid_t = ygrid_t * deg2rad
       ygrid_u = ygrid_u * deg2rad
       ygrid_v = ygrid_v * deg2rad

      RETURN
      END SUBROUTINE make_om_grid

      
