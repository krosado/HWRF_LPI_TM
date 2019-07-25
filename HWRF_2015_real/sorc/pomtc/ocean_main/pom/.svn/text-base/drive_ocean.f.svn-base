      PROGRAM  drive_ocean
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C
C MAIN PROGRAM: DRIVE_OCEAN
C
C PRGMMR: Biju Thomas, URI/GSO,  DATE: 2012/10/01
C ABSTRACT: 
C

      USE Oc_cc, ONLY: MPI_COMM_Ocean
      IMPLICIT NONE
      INCLUDE 'pom.h'
      include 'mpif.h'
      INTEGER                              :: icoupling, ierr
cc      REAL                                 :: xcen,ycen,timev
      character*15 filename
      LOGICAL                              :: success
      REAL, DIMENSION(im_global,jm_global) ::
     &xgrid_t,ygrid_t,xgrid_u,ygrid_u,xgrid_v,ygrid_v,
     &fsm_g,dum_g,dvm_g,tma,wusurf_g, wvsurf_g, 
     &wtsurf_g,swrad_g 
      icoupling = 1
      
c!
      CALL oc_cmp_start
      CALL oc_init(im_global,jm_global)
c!
      pom_comm=MPI_COMM_Ocean 

      CALL initialize
      CALL ocm_atm(tma,success)

      CALL make_om_grid(xgrid_t,xgrid_u,xgrid_v,
     &                  ygrid_t,ygrid_u,ygrid_v)
      CALL make_om_msk(fsm_g, dum_g, dvm_g)

c!
      CALL oc_sendgrids(xgrid_t,ygrid_t,xgrid_u,ygrid_u,
     &                       xgrid_v,ygrid_v,2.*dti) 
      CALL oc_sendslm(fsm_g, dum_g, dvm_g)
c!

      DO  iint=1,iend  ! IINT Loop begins
c!
       IF(MOD(iint+1,2) .EQ. 0) THEN
        CALL oc_sendsst(tma)
       END IF
c!        
      
       IF(MOD(iint+1,2) .EQ. 0 ) THEN
        IF( my_task .EQ. 0) THEN
        CALL oc_recv_sbc(wtsurf_g,1./(rho_0*4200.))
        CALL oc_recv_sbc(swrad_g,1./(rho_0*4200.))
        CALL oc_recv_sbc(wusurf_g,1./rho_0)
        CALL oc_recv_sbc(wvsurf_g,1./rho_0)
        ENDIF
        CALL mpi_barrier(pom_comm,ierr )
        call MPI_BCAST(wusurf_g,im_global*jm_global,MPI_REAL,0,
     &                 pom_comm, ierr)
        call MPI_BCAST(wvsurf_g,im_global*jm_global,MPI_REAL,0,
     &                 pom_comm, ierr)
        call MPI_BCAST(wtsurf_g,im_global*jm_global,MPI_REAL,0,
     &                 pom_comm, ierr)
        call MPI_BCAST(swrad_g,im_global*jm_global,MPI_REAL,0,
     &                 pom_comm, ierr)

       ENDIF
         
       
       CALL atm_ocm(wusurf_g, wvsurf_g, wtsurf_g,
     &        swrad_g, success)
       
       CALL advance(icoupling)
       CALL ocm_atm(tma,success)
      ENDDO ! IINT Loop Ends
 
      CALL finalize_mpi         

      STOP
      END PROGRAM  drive_ocean

