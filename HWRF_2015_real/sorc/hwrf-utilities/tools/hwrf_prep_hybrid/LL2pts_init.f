      SUBROUTINE LL2pts_init
     &  (ALL,bndpts,lbdpts,XLONPT,YLATPT,ALAT,ALON,IFIRST,
     &  WIJ,WIPJ,WIJP,WIPJP,
     &  KIJ,KIPJ,KIJP,KIPJP,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    LL2pts      INTERPOLATES FROM LL TO bnd points
C   PRGMMR: DEAVEN           ORG: W/NMC22    DATE: 86-07-21
C
C ABSTRACT: INTERPOLATES FROM LL TO boundary points
C
C PROGRAM HISTORY LOG:
C   86-07-21  D DEAVEN
C   88-09-21  B SCHMIDT ADDED THE DOCBLOCK
C   90-07-02  G DiMego modified to interpolate to string
C                of lateral boundary points (NGM or ETA)
C
C USAGE:    CALL LL2pts(ALL, bndpts)
C          $            lbdpts,XLONPT,YLATPT,IFIRST)
C   INPUT ARGUMENT LIST:
C     ALL      - ARRAY OF source VALUES OF LL GRID
C     IFIRST   - set to zero first-time-thru so that
C                weighting coefficients are recalculated
C
C   OUTPUT ARGUMENT LIST:
C     bndpts   - ARRAY OF target VALUES along boundary
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       COMMON   - /GRIDS/ 
C
C ATTRIBUTES:
C   LANGUAGE: STANDARD FORTRAN
C   MACHINE:
C
C$$$
      integer,parameter::real_32=selected_real_kind(6,30)
      INCLUDE "parmlbc"
C
      REAL ALAT( IMAX , JMAX ),ALON( IMAX , JMAX )
!zhang      COMMON /WGTS_INIT/WIJ(IMJM),WIPJ(IMJM),WIJP(IMJM),WIPJP(IMJM),
!zhang     1  KIJ(2,IMJM),KIPJ(2,IMJM),KIJP(2,IMJM),KIPJP(2,IMJM)
      dimension WIJ(IMJM),WIPJ(IMJM),WIJP(IMJM),WIPJP(IMJM),
     1  KIJ(2,IMJM),KIPJ(2,IMJM),KIJP(2,IMJM),KIPJP(2,IMJM)
      REAL, INTENT(INOUT) :: XLONPT( IMJM ), YLATPT( IMJM )
C
      REAL, INTENT(INOUT) :: ALL(  IMAX , JMAX    ),bndpts( IMJM )
c    3     AIJ( IMJM ),AIPJ( IMJM ),
c    4     AIJP( IMJM ),AIPJP( IMJM )
C
      REAL(REAL_32),ALLOCATABLE :: AIJ(:),AIPJ(:),AIJP(:),AIPJP(:)
      integer :: iq2w6e,jj,kj,kipj,kii,kij,kijp,kipjp
      integer :: ij,ki,ii,iip1
      real :: dx2,dxx2,dy1,y2,dx1,dxx1,dyy1,wipjp,dyy,dxx,wijp
      real :: wipj,wij
      real :: dy,dy2,dyy2,dx,y1,yy,xx,alipjp,x1,x2,alipj
      integer, dimension(2) :: IQ2W6F
C
c     print *,'in ll2pts ',lbdpts,kb,imax,jmax,ifirst
C

      IF ( IFIRST.NE.0 ) GO TO 101
C$OMP CRITICAL
      IF ( IFIRST.NE.0 ) GO TO 100
C**********************************************************************
C  FIRST COLLECT THE I,J POINTS ON THE LL GRID CORRESPONDING TO THE
C   bnd points (FOUR SETS OF THEM) FOR (I,J), (I+1,J), (I,J+1), AND
C     (I+1,J+1)
C**********************************************************************
       DO 1 IJ = 1,lbdpts
       YY =  YLATPT(IJ)
       XX =  XLONPT(IJ)
       IF(XX.GE.360.0)XX = XX - 360.0
       IF(XX.LT.0.0)XX = XX + 360.0
       DO 2 KJ = 1, JMAX
       IF ( YY .GT. ALAT(1,KJ)) GO TO 2
       JJ = KJ
       GO TO 3
2      CONTINUE
       JJ = JMAX
3      DO 4 KI = 1, IMAX
       IF( XX .LT. ALON(KI,JJ)) GO TO 4
       II = KI
       GO TO 5
4      CONTINUE
       GO TO 9876
5      CONTINUE
       IF(II.EQ.1)GO TO 9876
       IIP1 = II
       II = II - 1
       GO TO 9874
C  these statements will treat interpolation as cyclic east-to-west
9876   IIP1 = 1
       II = IMAX
9874   CONTINUE
       JJ = MAX(1,JJ-1)
       ALIPJ   = ALON(IIP1,JJ)
       ALIPJP = ALON(IIP1,JJ+1)
       IF(IIP1.EQ.1)ALIPJ = 360.0 - ALIPJ
       IF(IIP1.EQ.1)ALIPJP = 360.0 - ALIPJP
       KIJ(:,IJ) = (/ II,JJ /)                                          ! (JJ-1) *  IMAX + II
       KIPJ(:,IJ) = (/ IIP1,JJ /)                                       ! (JJ-1) * IMAX + IIP1
       KIJP(:,IJ) = (/ II,JJ+1 /)                                       ! JJ * IMAX + II
       KIPJP(:,IJ) = (/ IIP1,JJ+1 /)                                    ! JJ * IMAX + IIP1
       X1 = ALIPJ  - ALON(II,JJ)
       X2 = ALIPJP  - ALON(II,JJ+1)
       Y1 = ALAT(II,JJ+1) - ALAT(II,JJ)
       Y2 = Y1
       DX1 = (XX - ALON(II,JJ)) / X1
       DXX1 = (ALIPJ - XX) / X1
       DX2 = (XX - ALON(II,JJ+1)) / X2
       DXX2 = (ALIPJP - XX) / X2
       DY1 = (YY - ALAT(II,JJ)) / Y1
       DYY1 = (ALAT(II,JJ+1) - YY) / Y1
       DY2 = (YY - ALAT(II,JJ)) / Y2
       DYY2 = (ALAT(II,JJ+1) - YY) / Y2
       DX = 0.5 * (DX1 + DX2)
       DXX = 0.5 * (DXX1 + DXX2)
       DY = 0.5 * (DY1 + DY2)
       DYY = 0.5 * (DYY1 + DYY2)
       WIJ(IJ) = DXX * DYY
       WIPJ(IJ) = DX * DYY
       WIJP(IJ) = DXX * DY
       WIPJP(IJ) = DX * DY
1      CONTINUE
      IFIRST = IFIRST + 1
100    CONTINUE
C$OMP END CRITICAL
 101   CONTINUE

C*********************************************************************
C  ALL INDEX AND WEIGHT VECTORS HAVE BEEN COMPUTED ABOVE
C  NOW DO THE GATHER AND INTERPOLATION BELOW AT VECTOR SPEED
C*********************************************************************
C
      ALLOCATE(AIJ(IMJM))
      ALLOCATE(AIPJ(IMJM))
      ALLOCATE(AIJP(IMJM))
      ALLOCATE(AIPJP(IMJM))
C
      DO 42 IQ2W6E=1,lbdpts
      IQ2W6F=KIJ(:,IQ2W6E)
      AIJ(IQ2W6E)=ALL(IQ2W6F(1),IQ2W6F(2))
   42 CONTINUE
      DO 52 IQ2W6E=1,lbdpts
      IQ2W6F=KIPJ(:,IQ2W6E)
      AIPJ(IQ2W6E)=ALL(IQ2W6F(1),IQ2W6F(2))
   52 CONTINUE
      DO 62 IQ2W6E=1,lbdpts
      IQ2W6F=KIJP(:,IQ2W6E)
      AIJP(IQ2W6E)=ALL(IQ2W6F(1),IQ2W6F(2))
   62 CONTINUE
      DO 72 IQ2W6E=1,lbdpts
      IQ2W6F=KIPJP(:,IQ2W6E)
      AIPJP(IQ2W6E)=ALL(IQ2W6F(1),IQ2W6F(2))
   72 CONTINUE
      DO 82 IQ2W6E=1,lbdpts
         bndpts(IQ2W6E)=WIJ(IQ2W6E)*AIJ(IQ2W6E)+
     *                  WIPJ(IQ2W6E)*AIPJ(IQ2W6E)+
     *                  WIJP(IQ2W6E)*AIJP(IQ2W6E)+
     *                 WIPJP(IQ2W6E)*AIPJP(IQ2W6E)
   82 CONTINUE
C
      DEALLOCATE(AIJ)
      DEALLOCATE(AIPJ)
      DEALLOCATE(AIJP)
      DEALLOCATE(AIPJP)
C      
                  RETURN
                  END
