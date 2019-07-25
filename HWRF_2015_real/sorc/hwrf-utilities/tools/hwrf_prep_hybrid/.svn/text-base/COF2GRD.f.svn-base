      SUBROUTINE COF2GRD (LUN1,NC,JROMB,JCAP,KMAX,
     &     IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     &     KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX)
C     
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C     .      .    .                                       .
C     SUBPROGRAM:    COF2GRD     CONVERT ONE RECORD OF SIGMA COEFF FILE
C     TO LAT/LON GRID
C     PRGMMR: ROGERS           ORG: W/NP22     DATE: 99-01-28
C     
C     ABSTRACT: CONVERT SIGMA COEFFICIENT RECORD TO GRID SPACE USING
C     SPLIB ROUTINES. THESE ROUTINES WILL RETURN A GLOBAL
C     LAT/LON GRID WHOSE RESOLUTION IS DETERMINED BY THE 
C     NUMBER OF GRID POINTS. THEN, THE RELEVENT SUBSET FOR
C     WHICH WE HAVE HIGH-RES OROGRAPHY IS EXTRACTED (DIMENSION
C     OF BOTH THE EXTRACTED GRID AND GLOBAL GRID SET IN 
C     parmanl FILE)
C     
C     PROGRAM HISTORY LOG:
C     99-01-28  ROGERS
C     
C     USAGE:    CALL COF2GRD(LUN1,NC,KMAX,JROMB, 
C     JCAP,XGRID,PGRID)
C     
C     INPUT ARGUMENT LIST:
C     LUN1     - FORTRAN UNIT FOR SIGMA FILE
C     NC       - LENGTH OF SIGMA RECORD = RES+1*RES+2
C     JROMB    - SPECTRAL DOMAIN SHAPE (0 FOR TRIANGULAR, 
C     1 FOR RHOMBOIDAL)
C     JCAP     - SPECTRAL TRUNCATION
C     
C     OUTPUT FILES:
C     KMAX     - NUMBER OF SIGMA LEVELS IN GLOBAL MODEL
C     XGRID    - ARRAY holding the IMAX x JMAX grids at KMAX levels
C     FOR FOUR VARIABLES:  1-Tv  2-U  3-V  4-q
C     PGRID    - ARRAY holding the IMAX x JMAX grids of Z* and p*
C     
C     ATTRIBUTES:
C     LANGUAGE: FORTRAN-90
C     MACHINE: CRAY C-90
C     
C$$$  

      USE SP_GRID_MODULE, ONLY: TGRID,UGRID,VGRID,QGRID,CWMGRID,
     &     PRESGRID,PINTGRID,PGRID, SPNLEVS, REAL_32
      USE SIGIO_MODULE
      IMPLICIT NONE
      TYPE(sigio_head):: HEAD
      TYPE(sigio_data):: DATA

      INTEGER :: NGUSED,NGBD,LTBGRI,LBDIM
      INTEGER :: I,J,IRET,L,IER,K,LLL,JCAP,KMAX,JROMB,LUN1,NC
      INTEGER :: LUN1HOLD,nthreads

      REAL    :: POLEI,POLEJ,XMESHL,ALONVT
      REAL    :: TPH0D,TLM0D,WBD,SBD

      LOGICAL :: POLA,NORTH

      INCLUDE "parmlbc"
C     
C     XMESHL = EAST-WEST GRID INCREMENT
C     POLEI  = SOUTH-NORTH GRID INCREMENT
C     POLEJ  = WESTERN BOUNDARY OF LAT/LON GRID
C     ALONVT = SOUTHERN BOUNDARY OF LAT/LON GRID
C     
      CHARACTER HOLDFIL*80,FILENAME*80
C     
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::GRD
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::MYGRD
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::MYGRD2,PD
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::DWORK,ZWORK
C     

      print *,'entering cof2grd',jromb,jcap,imax,jmax,kmax
C     
      write(filename,633) LUN1
 633  format('fort.',I2.2)

!     CALL sigio_srohdc(12,'gfsbc1',head,data,iret)
!     write(6,*) 'iret from sigio_srohdc: ', iret
!     write(6,*) 'head%levs: ', head%levs

      CLOSE(LUN1)

      CALL sigio_srohdc(LUN1,trim(filename),head,data,iret)
      write(6,*) 'iret from sigio_srohdc: ', iret
      write(6,*) 'head%levs: ', head%levs
      write(6,*) 'head%jcap: ', head%jcap
      write(6,*) 'head%ntrac: ', head%ntrac
      write(6,*) 'From file: ',filename
      write(6,*) 'start COF2GRD'
      call summary()

      write(6,*) 'to allocates'

      ALLOCATE(MYGRD(IMAX,JMAX))
      ALLOCATE(MYGRD2(IMAX,JMAX,head%levs))
      ALLOCATE(PD(IMAX,JMAX,head%levs))
      ALLOCATE(GRD(IMAX,JMAX))

      SPNLEVS=head%levs
      ALLOCATE(TGRID(IMAX,JMAX,head%levs))
      ALLOCATE(UGRID(IMAX,JMAX,head%levs))
      ALLOCATE(VGRID(IMAX,JMAX,head%levs))
      ALLOCATE(QGRID(IMAX,JMAX,head%levs))
      ALLOCATE(CWMGRID(IMAX,JMAX,head%levs))
      ALLOCATE(PRESGRID(IMAX,JMAX,head%levs))
      ALLOCATE(PINTGRID(IMAX,JMAX,head%levs+1))
      ALLOCATE(PGRID(IMAX,JMAX,3))


!     hs (surface topo)

      write(6,*) 'Process data%hs...'
      CALL SPTRAN(0,head%JCAP,0,IMAX,JMAX,1,0,0,
     2     -IMAX,IMAX,
     1     0,0,0,0,1,data%hs,MYGRD(1,JMAX),MYGRD(1,1),1)

      DO J=1,JMAX 
         DO I=1,IMAX
            PGRID(I,J,2)=MYGRD(I,J)
         ENDDO
      ENDDO
      write(6,*) '  ... done.'

      write(6,*) 'data%hs onto PGRID'
      do J=1,JMAX,JMAX/40
         write(6,617) (PGRID(I,J,2),I=1,IMAX,IMAX/20)
      enddo

 617  format(30(f5.0,1x))

!     ps and midlayer P values


      write(6,*) 'Process data%ps...'
      CALL SPTRAN(0,head%JCAP,0,IMAX,JMAX,1,0,0,
     2     -IMAX,IMAX,
     1     0,0,0,0,1,data%ps,MYGRD(1,JMAX),MYGRD(1,1),1)
      write(6,*) '  ... done.'

C$OMP PARALLEL DO PRIVATE(I,J)
      DO J=1,JMAX 
         DO I=1,IMAX
            PGRID(I,J,1)=1000.*EXP(MYGRD(I,J))
            MYGRD(I,J)=PGRID(I,J,1)
         ENDDO
      ENDDO
C$OMP END PARALLEL DO

      write(6,*) 'data%ps onto PGRID'
      do J=1,JMAX,JMAX/40
         write(6,617) (PGRID(I,J,1)/100.,I=1,IMAX,IMAX/20)
      enddo


      write(6,*) 'Run sigio_modpr...'
      CALL sigio_modpr(IMAX*JMAX,IMAX*JMAX,head%levs,
     +     head%nvcoord,head%idvc,head%idsl,          
     +     head%vcoord,iret,ps=MYGRD,pm=MYGRD2,
     +     pd=PD)
      write(6,*) '  ... done.'

!     temperatures

      write(6,*) 'before 3D read of T'
C$OMP PARALLEL DO PRIVATE(I,J)
      DO J=1,JMAX
         DO I=1,IMAX
            PINTGRID(I,J,1)=MYGRD(I,J)
         ENDDO
      ENDDO
C$OMP END PARALLEL DO


      write(6,*) 'Process data%t...'
      CALL SPTRAN(0,head%JCAP,0,IMAX,JMAX,head%levs,0,0,
     2        -IMAX,IMAX,
     1        0,0,0,0,1,data%t,TGRID(1,JMAX,1),TGRID(1,1,1),1)
      write(6,*) '  ... done.'

      DO L=1,head%levs
C$OMP PARALLEL DO PRIVATE(I,J)
         DO J=1,JMAX
          DO I=1,IMAX
             PINTGRID(I,J,L+1)=PINTGRID(I,J,L)-PD(I,J,L)
             IF (L .eq. head%levs .and. PINTGRID(I,J,L+1) .lt. 5.) then
                PINTGRID(I,J,L+1)=5.
             ENDIF
               
            ENDDO
         ENDDO
C$OMP END PARALLEL DO
      ENDDO

      I=1
      J=1
      DO L=1,head%levs
         write(6,*) '1,1,PINT,PD:: ', 
     &        I,J,L,PINTGRID(I,J,L+1),PD(I,J,L)
      ENDDO

      write(6,*) 'after 3D read of T'
      call summary()

      print *,'ok after terrain coeffs'
C     
C     READ SFC PRESSURE COEFFICIENTS
C     

C$OMP PARALLEL DO PRIVATE(I,J,L)
      DO L = 1, head%levs
         DO J = 1, JMAX
            DO I = 1, IMAX
               PRESGRID(I,J,L) =  MYGRD2(I,J,L)
            ENDDO
         ENDDO
      ENDDO
C$END PARALLEL DO

      I=1
      J=1
      DO L = 1, head%levs
         write(6,*) 'L, PRESGRID(1,1,L): ', L, PRESGRID(1,1,L)
      ENDDO

      DEALLOCATE(GRD)
C     
C     READ DIVERGENCE AND VORTICITY COEFFICIENTS
C     
      ALLOCATE(DWORK(NC))
      ALLOCATE(ZWORK(NC))
      write(6,*) 'Process data%d and data%z (velocity)...'
      CALL SPTRANV(0,head%jcap,0,IMAX,JMAX,head%levs,0,0,-IMAX,IMAX,
     1   0,0,0,0,1,data%d,data%z,UGRID(1,JMAX,1),UGRID(1,1,1),
     2   VGRID(1,JMAX,1),VGRID(1,1,1),1)
      write(6,*) '  ... done.'
      DO L = 1, head%levs
        write(6,*) 'U(50,50),V(50,50): ', 
     &      L, UGRID(50,50,L),VGRID(50,50,L)
      ENDDO
C       DO L = 1, head%levs
C         CALL SPTRANV(0,head%jcap,0,IMAX,JMAX,1,0,0,-IMAX,IMAX,
C      1   0,0,0,0,1,data%d(:,L),data%z(:,L),UGRID(1,JMAX,L),UGRID(1,1,L),
C      2   VGRID(1,JMAX,L),VGRID(1,1,L),1)
C         write(6,*) 'U(50,50),V(50,50): ', 
C      &      L, UGRID(50,50,L),VGRID(50,50,L)
C       ENDDO
      DEALLOCATE(DWORK)
      DEALLOCATE(ZWORK)
      print *,'ok after div/vort coeffs'

C     
C     READ SPECIFIC HUMIDITY COEFFICIENTS
C     
      write(6,*) 'Process data%q(:,:,1) (water vapor)...'
      CALL SPTRAN(0,head%JCAP,0,IMAX,JMAX,head%levs,0,0,-IMAX,IMAX,
     1     0,0,0,0,1,data%q(:,:,1),QGRID(1,JMAX,1),QGRID(1,1,1),1)
      write(6,*) '  ... done.'
C       DO L = 1, head%levs
C          CALL SPTRAN(0,head%JCAP,0,IMAX,JMAX,1,0,0,-IMAX,IMAX,
C      1        0,0,0,0,1,data%q(:,L,1),QGRID(1,JMAX,L),QGRID(1,1,L),1)
C       ENDDO
      print *,'ok after q coeffs'

C$OMP PARALLEL DO PRIVATE(I,J,K)
      DO K = 1, head%levs
         DO J = 1, JMAX
            DO I = 1, IMAX
               QGRID(I,J,K) = AMAX1(QGRID(I,J,K),1.0E-12)
            ENDDO
         ENDDO
      ENDDO
C$OMP END PARALLEL DO

      do L=1,head%levs
         write(6,*) 'q element 1...L,Q(10,180,L): ', L, QGRID(10,180,L)
      enddo
C     

      CWMGRID=-9999.
      write(6,*) 'Process data%q(:,:,3) (total condensate)...'
      CALL SPTRAN(0,head%JCAP,0,IMAX,JMAX,head%levs,0,0,-IMAX,IMAX,
     1     0,0,0,0,1,data%q(:,:,3),CWMGRID(1,JMAX,1),CWMGRID(1,1,1),1)
      write(6,*) '  ... done.'
C       DO L = 1, head%levs
C          CALL SPTRAN(0,head%JCAP,0,IMAX,JMAX,1,0,0,-IMAX,IMAX,
C      1     0,0,0,0,1,data%q(:,L,3),CWMGRID(1,JMAX,L),CWMGRID(1,1,L),1)
C       ENDDO
      write(6,*) 'maxval CWMGRID: ', maxval(CWMGRID)
      do L=1,head%levs
         write(6,*) 'q(3) ..L,CWMGRID(10,180,L): ',L,CWMGRID(10,180,L)
      enddo

      LUN1HOLD=LUN1+100
      WRITE(HOLDFIL,1000)LUN1
 1000 FORMAT('holdsig',I3.3)
c$$$      OPEN(UNIT=LUN1HOLD,FILE=HOLDFIL,FORM='UNFORMATTED',IOSTAT=IER)
c$$$C     
c$$$      write(6,*) 'TGRID(1,1,1): ', TGRID(1,1,1)
c$$$      WRITE(LUN1HOLD)TGRID
c$$$      write(6,*) 'UGRID(1,1,1): ', UGRID(1,1,1)
c$$$      WRITE(LUN1HOLD)UGRID
c$$$      write(6,*) 'VGRID(1,1,1): ', VGRID(1,1,1)
c$$$      WRITE(LUN1HOLD)VGRID
c$$$      write(6,*) 'QGRID(1,1,1): ', QGRID(1,1,1)
c$$$      WRITE(LUN1HOLD)QGRID
c$$$      write(6,*) 'CWMGRID(1,1,1): ', CWMGRID(1,1,1)
c$$$      WRITE(LUN1HOLD)CWMGRID
c$$$      write(6,*) 'PRESGRID(1,1,1): ', PRESGRID(1,1,1)
c$$$      WRITE(LUN1HOLD)PRESGRID
c$$$      write(6,*) 'PINTGRID(1,1,1): ', PINTGRID(1,1,1)
c$$$      WRITE(LUN1HOLD)PINTGRID
c$$$      write(6,*) 'PGRID(1,1,1): ', PGRID(1,1,1)
c$$$      WRITE(LUN1HOLD)PGRID
c$$$      CLOSE(LUN1HOLD)
c$$$      DEALLOCATE(TGRID)
c$$$      DEALLOCATE(UGRID)
c$$$      DEALLOCATE(VGRID)
c$$$      DEALLOCATE(QGRID)
c$$$      DEALLOCATE(CWMGRID)
c$$$      DEALLOCATE(PRESGRID)
c$$$      DEALLOCATE(PINTGRID)
c$$$      DEALLOCATE(PGRID)
c$$$      DEALLOCATE(PD)

      KMAX=head%levs

      write(6,*) 'end COF2GRD'
      call summary()
C     


C Deallocate data:
      CALL sigio_axdata(data,iret)


      RETURN
      END
