         SUBROUTINE MAKBND 
     & (IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,DLMD,DPHD,IMAX,JMAX,narg) !Zhang
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: MKBND126     CREATE values along boundaries of ngm/eta
C   PRGMMR: ROGERS           ORG: NP22        DATE: 1998-10-19
C
C ABSTRACT: READS IN spectral COEFFICIENT FORM (KMAX LEVELS OF
C   T126 COEFFICIENTS) AND CONVERTS IT TO A IMAX X JMAX "basis" grid
C   for interpolation to the boundary points of the NGM or ETA models
C
C PROGRAM HISTORY LOG:
C   90-05-21  DIMEGO/PAN         CRAY CODE WITH A NEW
C                                SUBROUTINE INTERFACE FOR TRANSFORMS
C   99-02-05  ROGERS             GENERALIZED VERSION WHICH WILL WORK
C                                FOR ANY GLOBAL MODEL RESOLUTION
C                                ADDED PTETABC CODE (INTERPOLATES ETA
C                                BOUNDARY POINTS FROM SIGMA TO ETA)
C                                AS A SUBROUTINE
C   06-10-25  YOUNG KWON         ADD FUNCTION OF PRODUCING THE INITIAL 
C                                DATA TOO 
C
C   12-05-23  InHyuk KWON        VERTICAL INTERPOLATION MODIFIED
C
C
C USAGE:
C   INPUT FILES:
C     UNIT11    - sigma COEFFICIENTS FROM THE GDAS or aviation
C               - ON KMAX SIGMA LEVELS
C
C   OUTPUT FILES:
C     UNIT6    - DIAGNOSTICS AND PRINT OUTPUT
C     UNIT51   - output FILE OF lateral boundary values
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - COF2GRD  COF2xx   DZTOUV   PPZT     PPUV
C                  CMPIND   CMPWND   TRANSI   TRANVI
C                  PLN2H    SUMSHS   SUMVHS   FFTidim
C
C     LIBRARY:
C       SPLIB    - SPTRAN SPTRANV
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C   REMARKS: SEE COMMENT CARDS FOLLOWING DOCBLOCK
C
C ATTRIBUTES:
C   LANGUAGE: STANDARD FORTRAN
C   MACHINE:
C
C$$$
CC
C      VARIABLES IN NAMELIST RGRID READ INTO MAIN PROGRAM
CC
C   POLA    - TYPE OF baSIS GRID  (DEFAULT=FALSE)
C   NORTH   - HEMISPHERE SWITCH FOR baSIS GRID  (DEFAULT=TRUE)
C   ALONVT  - REFERENCE LONGITUDE (+W)      FOR POLA = TRUE
C             STARTING LATITUDE   (+N)      FOR POLA = FALSE
C   POLEI   - I INDEX OF POLE               FOR POLA = TRUE 
C             INCREMENT FOR LATITUDE        FOR POLA = FALSE
C   POLEJ   - J INDEX OF POLE               FOR POLA = TRUE
C             STARTING LONGITUDE  (+W)      FOR POLA = FALSE GRNWCH=360
C   XMESHL  - MESH LENGTH (KM) AT 60N       FOR POLA = TRUE
C             INCREMENT FOR LONGITUDE       FOR POLA = FALSE
C   SI2     - LMAXP1 SIGMA INTERFACE DEFINITIONS
CC
C
C   ASSUME MAXIMUM # GLOBAL LEVELS = 50
C
      USE SIGIO_MODULE
      USE MODULE_SIG2HYB
      USE MODULE_SIG2HYB_INIT
      USE SP_GRID_MODULE, ONLY: PS=>PGRID,TGRID,UGRID,VGRID,QGRID,
     &     CWMGRID,PRESGRID,PINTGRID, SPNLEVS
      INCLUDE "parmlbc"

      integer,parameter::real_32=selected_real_kind(6,30)
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & HPTLAT(KB), HPTLON(KB), UPTLAT(KB), UPTLON(KB)
     &,VPTLAT(KB), VPTLON(KB)
C
      DIMENSION ALL(245)
C
C.. LAT LON OVER WHOLE DOMAIN. FOR THE INITIAL CONDITION
C
      DIMENSION HLAT (IMJM),HLON (IMJM),VLAT (IMJM),VLON (IMJM)   !KWON
      DIMENSION GLAT (IMJM),GLON (IMJM)
C
      DIMENSION IDATE(4)
C
!zhang
      dimension WIJ(KB),WIPJ(KB),WIJP(KB),WIPJP(KB),
     1  KIJ(2,KB),KIPJ(2,KB),KIJP(2,KB),KIPJP(2,KB)
      integer :: WIJ,WIPJ,KIJ,KIPJ,KIJP
      real :: WIJP,WIPJP
      dimension WIJ_init(IMJM),WIPJ_init(IMJM),
     1         WIJP_init(IMJM),WIPJP_init(IMJM),
     1  KIJ_init(2,IMJM),KIPJ_init(2,IMJM),
     1  KIJP_init(2,IMJM),KIPJP_init(2,IMJM)
      integer :: WIJ_init,WIPJ_init,KIJ_init,
     1 KIPJ_init,KIJP_init
      real :: WIJP_init,WIPJP_init
      real :: DLMD,DPHD

c$$$      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::TEMP,QTEMP
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::UATHPTS,VATHPTS
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::TATHPTS,QATHPTS
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::CWMATHPTS,PRATHPTS
      REAL(REAL_32),POINTER,DIMENSION(:,:)::PRB
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::PSATHPTS,ZSATHPTS
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::UATUPTS,VATUPTS
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::TATUPTS,QATUPTS
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::PSATUPTS,ZSATUPTS
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::UATVPTS,VATVPTS
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::TATVPTS,QATVPTS
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::PSATVPTS,ZSATVPTS
c$$$      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::TGRID,UGRID
c$$$      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::VGRID,QGRID,CWMGRID
c$$$      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::PRESGRID,PS,PINTGRID
c$$$      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::RHGRID
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::TMN0,AREA,DTMN,SI,SL
      REAL(REAL_32),POINTER :: PINTATHPTS(:,:)
C
C... HORIZONTAL INTERPOLATION OUTPUT FOR THE INITIAL DATA
C                      YC KWON
C
      REAL(REAL_32),POINTER,DIMENSION(:,:)::TINIT,QINIT,CWMINIT
      REAL(REAL_32),POINTER,DIMENSION(:,:)::UINIT,VINIT
      REAL(REAL_32),POINTER,DIMENSION(:,:)::PMID,PINT
      REAL(REAL_32),POINTER,DIMENSION(:)::PSINIT,ZSINIT
C
      REAL(REAL_32),POINTER,DIMENSION(:,:)::UB,VB,TB,QB,CWMB
      REAL(REAL_32),POINTER,DIMENSION(:)::PB,ZB
C
C      PARAMETER(IDIM=IMAX,JDIM=JMAX,
C     1          NVAR=4,NPOINT=3)
C
      CHARACTER HOLDFIL*80
      CHARACTER HOLDFILS*80
C
      LOGICAL POLA,NORTH
      COMMON /GRID/ POLA,NORTH,ALONVT,POLEI,POLEJ,XMESHL
      NAMELIST/RGRID/POLA,NORTH,ALONVT,POLEI,POLEJ,XMESHL
      EQUIVALENCE (YLATS,ALONVT) , (DLAT,POLEI)
      EQUIVALENCE (XLONW,POLEJ) , (DLON,XMESHL)
C
      NAMELIST/PRMFLD/NTIMES
Czhang add namelist for model top and sigma levels:
      INTEGER , PARAMETER :: max_eta         =   501
      NAMELIST/domain/ p_top_requested, ptsgm,  levels
      integer :: ptsgm
      real :: p_top_requested
      real, dimension(max_eta) :: levels
      INTEGER ITM, IERR
C
      DIMENSION CON(24),XLIM(4),YLIM(4)
      CHARACTER*4 YTITLE(20) , BLANK
      LOGICAL IACROS,XREV,YREV,TICK,ALLCON,NL_EXISTS
      CHARACTER*1 XTITLE(30) , BLNK
      DATA BLANK/'    '/ , BLNK/' '/
C
C      COMMON /GRIDS/ ALAT( IMAX , JMAX ),ALON( IMAX , JMAX )
      REAL, ALLOCATABLE:: ALAT(:,:),ALON(:,:)
      INTEGER :: nthreads
C
      CALL W3TAGB('MKBND   ',1998,0292,0082,'NP22   ')
C
C.. READ ITIME TO CHECK WHETHER IT IS INITIAL TIME OR NOT
C                         YC KWON
C
      READ(44,*) ITM
      WRITE(6,*) 'ITIME  ',ITM
      PI = 3.141592654
      DTR = PI / 180.0
C
      WRITE(6,1)
    1 FORMAT('1  WELCOME TO THE GENERAL BOUNDARY VALUE PROCESSOR',/,
     + ' WITH NEW TRANSFORMs -ETA/NGM- CRAY VERSION December 6,1990',//)
      MI = IMAX
      LID = MI
      LJ = JMAX
      DO I=1,30
        XTITLE(I) = BLNK
      ENDDO
      DO I=1,20
        YTITLE(I) = BLANK
      ENDDO
C
C  YC=NPRC/IMAX and XC=NPRL/JMAX
C   THESE ARE FOR half grid NPRC=68 and NPRL=41 small screen
C
      YC = (68.) / FLOAT(IMAX)
      XL = (41.) / FLOAT(JMAX)
      CON(1) = 1.0
      CON(2) = 1.0
      ICT = 3
      DEGRAD = 3.14159265 / 180.
C
C  SET PRINT FOR 12 VALUES PER LINE
C
C     NAMELIST/RGRID/POLA,NORTH,ALONVT,POLEI,POLEJ,XMESHL
c     LOGICAL POLA,NORTH
      POLA = .FALSE.
      NORTH = .TRUE.
      ALONVT = 0.5
      POLEI  = 1.0
      POLEJ  = 360.
      XMESHL = 1.0
      p_top_requested= 50.0
      DATA LEVELS/1.0, .9919699, .9827400, .9721600, .9600599, .9462600,
     1    .9306099,    .9129300, .8930600, .8708600, .8462000, .8190300,
     1    .7893100,    .7570800, .7224600, .6856500, .6469100, .6066099,
     1    .5651600,    .5230500, .4807700, .4388600, .3978000, .3580500,
     1    .3200099,    .2840100, .2502900, .2190100, .1902600, .1640600,
     1    .1403600,    .1190600, .1000500, .0831600, .0682400, .0551200,
     1    .0436200,    .0335700, .0248200, .0172200, .0106300, .0049200,
     1    .0000000, 458*0.0/

      NL_EXISTS = .FALSE.
      INQUIRE(FILE="prep_hybrid.nl", EXIST=NL_EXISTS)
      IF (NL_EXISTS .EQV. .TRUE.) THEN
          OPEN(5,FILE='prep_hybrid.nl')
      ENDIF

      READ(5,RGRID)
      WRITE(6,4)ALONVT,POLEI,POLEJ,XMESHL
    4 FORMAT('0  &RGRID  LIMITS ',4G12.5)
      NTIMES = 1
      READ(5,PRMFLD)
      WRITE(6,PRMFLD)
      NUNIT = 50
Czhang
       read(5,domain,IOSTAT=ierr)
       if (ierr .ne. 0 ) then
           write(0,*) "Warning:Unable to read DOMAIN info from namelist,
     & Pl. add var. LEVELS in namelist or use defaults (no inputs)."
       endif

      write(6,*) "HWRF model top ==", p_top_requested
      write(6,*) "HWRF ptsgm ==", float(ptsgm)
      write(6,*) "HWRF sigma levels", lmp1
      write(6,*) (levels(kzz),kzz=1,lmp1)
C
      IF( KBETA.EQ.KB )
     &  CALL etaPTS(HPTLAT,HPTLON,NHPT,
     &           UPTLAT,UPTLON,NUPT,
     &           VPTLAT,VPTLON,NVPT,
     &           HLAT,HLON,VLAT,VLON,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,
     & KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,DLMD,DPHD) !Zhang
      WRITE(6,*) 'AFTER CALLING ETAPTS'
      write(6,*) 'nhpt=',nhpt,' nupt=',nupt,' nvpt=',nvpt,' kb=',kb
C
C.. CHECK GLAT GLON TO COMPARE GLAT GLON IN WRFINPUT
C..                    KWON
C
      DO JJ = 1,IMJM
      GLAT(JJ) = HLAT(JJ) * DTR
      GLON(JJ) = HLON(JJ) * DTR
      ENDDO
C
C      OPEN(55,FILE='glat_mkbnd',FORM='FORMATTED',STATUS='UNKNOWN')
C      OPEN(56,FILE='glon_mkbnd',FORM='FORMATTED',STATUS='UNKNOWN')
C      WRITE(55,222) GLAT
C      WRITE(56,222) GLON
C222   FORMAT(7F9.6)
C      CLOSE (55)
C      CLOSE (56)
C........................
C
C      DO 9800 NT = 1,NTIMES
C        IFCTHR = (NT-1) * 6
C        ITIME  = (IFCTHR-12) * 3600
        NUNIT = NUNIT + 1
C
C      READ and process the SIGGES COEFFICIENTS
C      FIRST READ THE SECOND RECORD TO GET GLOBAL MODEL
C      SPECS WHICH ARE SENT TO THE COF2GRD ROUTINE
C
      NT = 1
      LUN1= 11 + (NT -1)
      LUN1HOLD = LUN1 + 100
C
      JROMB=0   !zhang 
      JCAP=0    !zhang
      KMAX=0    !zhang
      CALL COF2GRD(LUN1,NC,JROMB,JCAP,KMAX,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
      print *,'ok after cof2grd'

        write(6,*) 'IMAX, JMAX: ', IMAX,JMAX
        write(6,*) 'KMAX: ', KMAX,JROMB,JCAP

C
C
C
C  the XX array is now complete
C     var: 1-T  2-U  3-V  4-q
C
C***********************************************************************
C SET UP LAT,LON OF THE IMAX x JMAX "basis" grid
C***********************************************************************
C
      ALLOCATE(ALAT(IMAX,JMAX))
      ALLOCATE(ALON(IMAX,JMAX))
      DO 521 J=1, JMAX
      YJ = J
      DO 520 I=1, IMAX
      XI = I
      CALL IJ2LL(XI,YJ,YLAT,WLON)
      ALAT(I,J) = YLAT
      ALON(I,J) = WLON
  520 CONTINUE
  521 CONTINUE
      print *,'ok after ij2ll'
C
c$$$      ALLOCATE(TGRID(IMAX,JMAX,KMAX))
c$$$      ALLOCATE(UGRID(IMAX,JMAX,KMAX))
c$$$      ALLOCATE(VGRID(IMAX,JMAX,KMAX))
c$$$      ALLOCATE(QGRID(IMAX,JMAX,KMAX))
c$$$      ALLOCATE(CWMGRID(IMAX,JMAX,KMAX))
c$$$      ALLOCATE(PRESGRID(IMAX,JMAX,KMAX))
c$$$      ALLOCATE(PINTGRID(IMAX,JMAX,KMAX+1))
c$$$      ALLOCATE(RHGRID(IMAX,JMAX,KMAX))
c$$$      ALLOCATE(PS(IMAX,JMAX,3))
C
c$$$      WRITE(HOLDFILS,98765)LUN1
c$$$98765 FORMAT('holdsig',I3.3)
c$$$      OPEN(UNIT=LUN1HOLD,FILE=HOLDFILS,FORM='UNFORMATTED',IOSTAT=IER)
c$$$C
c$$$	write(6,*) 'TGRID dims in MAKBND: ', size(TGRID,dim=1),
c$$$     &  SIZE(TGRID,dim=2),size(TGRID,dim=3)
c$$$      READ(LUN1HOLD)TGRID
c$$$	write(6,*) 'TGRID(1,1,1) read in: ', TGRID(1,1,1)
c$$$      READ(LUN1HOLD)UGRID
c$$$	write(6,*) 'UGRID(1,1,1) read in: ', UGRID(1,1,1)
c$$$      READ(LUN1HOLD)VGRID
c$$$	write(6,*) 'VGRID(1,1,1) read in: ', VGRID(1,1,1)
c$$$      READ(LUN1HOLD)QGRID
c$$$	write(6,*) 'QGRID(1,1,1) read in: ', QGRID(1,1,1)
c$$$      READ(LUN1HOLD)CWMGRID
c$$$	write(6,*) 'CWMGRID(1,1,1) read in: ', CWMGRID(1,1,1)
c$$$      READ(LUN1HOLD)PRESGRID
c$$$	write(6,*) 'PRESGRID(1,1,1) read in: ', PRESGRID(1,1,1)
c$$$      READ(LUN1HOLD)PINTGRID
c$$$	write(6,*) 'PINTGRID(1,1,1) read in: ', PINTGRID(1,1,1)
c$$$      READ(LUN1HOLD)PS
c$$$	write(6,*) 'PS(1,1,1) read in: ', PS(1,1,1)


!--------- For verification: IH Kwon
!      write(6,*) 'print globalout.bin'
!      open(76,file='globalout.bin',
!    &     form='unformatted',access='direct',recl=IMAX*JMAX*4)
!
!     do k=1,KMAX
!        write(76,rec=k     ) TGRID(:,:,k)
!        write(76,rec=k+KMAX) QGRID(:,:,k)
!     enddo
C
C.. WRITE TGRID FOR COMPARISON WITH INIT DATA AFTER H. V. INTERPOATION TO HWRF GRIDS
C
C      OPEN(333,FILE='global.dat',STATUS='UNKNOWN')
C      DO K = 1,KMAX
C      DO J = 1,JMAX
C      WRITE(333,2222) (TGRID(I,J,K),I=1,IMAX)
C      WRITE(333,2222) (UGRID(I,J,K),I=1,IMAX)
C      WRITE(333,2222) (VGRID(I,J,K),I=1,IMAX)
C      WRITE(333,2222) (PINTGRID(I,J,K),I=1,IMAX)
C2222  FORMAT(7(F13.2,1X))
C      ENDDO
C      ENDDO
C      CLOSE(333)
C
C***********************************************************************
C adjust virtual to sensible temperature for NGM and
C SET UP the mean (sensible) temperature for NGM adjustment
C***********************************************************************
C
C -------- CHANGE VIRTUAL TEMP TO TEMP ----
C
       write(6,*) 'KB,KBETA ',kb,kbeta
       print*,     'IDIM,JDIM,KDIMQ,KDIM',KDIMQ,KDIM
       write(6,*)  'IDIM,JDIM,KDIMQ,KDIM',KDIMQ,KDIM

      IF( KB.NE.KBETA ) THEN

       write(6,*) 'KB.NE.KBETA ',kb,kbeta
        IDIM=IMAX
        JDIM=JMAX
       XKAPA = 287.05e0 / 1005.0e0
       A0 = 6371220.0
       DEGREE = 90. / ASIN(1.0)
C
c$$$       ALLOCATE(TEMP(IMAX,JMAX,KMAX))
c$$$       ALLOCATE(QTEMP(IMAX,JMAX,KMAX))

C$OMP PARALLEL DO
C$OMP& PRIVATE(I,J,K)
       DO K=1,KDIMQ
        DO J=1,JDIM
         DO I=1,IDIM
c         XX(I,J,K,1)=XX(I,J,K,1)/(1.0+0.602*XX(I,J,K,4))
c         TGRID(I,J,K)=TEMP(I,J,K)/(1.0+0.602*QTEMP(I,J,K))

C This is pointless:
C          TGRID(I,J,K)=TEMP(I,J,K)/(1.0+0.608*QTEMP(I,J,K))

C You do not need the two temporary arrays:
          TGRID(I,J,K)=TGRID(I,J,K)/(1.0+0.608*QGRID(I,J,K))
         ENDDO
        ENDDO
       ENDDO
C$OMP END PARALLEL DO
C
C -------- DO TEMPERATURE ADJUSTMENT -----
        DATA ITADJ/0/
        IF( ITADJ .EQ. 0 ) THEN
          ITADJ=1
          PRINT *,' ------ INITIAL TEMPERATURE (MEAN) ----'
          DO 420 K=1,KDIM
            TSUM=0.0e0
            ASUM=0.0e0
            DO 410 J=1,JDIM
              DPHI=DLAT*A0/DEGREE
              RLAT=ALAT(1,J)/DEGREE
              DLAM=DLON*A0*COS(RLAT)/DEGREE
              DAREA=DLAM*DPHI
              DO 400 I=1,IDIM
                TSUM=TSUM+TGRID(I,J,K)*DAREA
                ASUM=ASUM+DAREA
 400          CONTINUE
 410        CONTINUE
            TMN0(K)=TSUM/ASUM
            AREA(K)=ASUM
            DTMN(K)=0.0e0
            PRINT *,' K= ',K,' TMEAN=',TMN0(K)
 420      CONTINUE
        ELSE
          PRINT *,' ------ ADJUSTED TEMPERATURE ----'
          write(0,*) 'tried to use an unintialied variable AREA'
          stop 33
          DO 460 K=1,KDIM
            TSUM=0.0e0
            DO 450 J=1,JDIM
              DPHI=DLAT*A0/DEGREE
              RLAT=ALAT(1,J)/DEGREE
              DLAM=DLON*A0*COS(RLAT)/DEGREE
              DAREA=DLAM*DPHI
              DO 440 I=1,IDIM
                TSUM=TSUM+TGRID(I,J,K)*DAREA
 440          CONTINUE
 450        CONTINUE
            TTM=TSUM/AREA(K)
            DTMN(K)=TMN0(K)-TTM
            PRINT *,' K=',K,' TMEAN=',TMN0(K),' T=',TTM,' DT=',DTMN(K)
 460      CONTINUE
        ENDIF

      ENDIF    !! ( KB.NE.KBETA )

!--------- For verification: IH Kwon
!                TGRID is still virtual temperature

!     pq0=379.90516
!     a1 =0.608
!     a2 =17.2693882
!     a3 =273.16
!     a4 =35.86

!     do k=1,KMAX
!       do j=1,JMAX
!       do i=1,IMAX
!          pmd=(PINTGRID(i,j,k)+PINTGRID(i,j,k+1))*0.5
!          tmp=TGRID(i,j,k)/(1+0.608*QGRID(i,j,k))
!          qsat= pq0/pmd *exp(a2*(tmp-a3)/(tmp-a4))
!          RHGRID(i,j,k)= QGRID(i,j,k) / qsat
!       enddo
!       enddo
!     enddo

!     do k=1,KMAX
!        write(76,rec=k+KMAX*2) TGRID(:,:,k)
!        write(76,rec=k+KMAX*3) RHGRID(:,:,k)
!        write(76,rec=k+KMAX*4) UGRID(:,:,k)
!        write(76,rec=k+KMAX*5) VGRID(:,:,k)
!        write(76,rec=k+KMAX*6) CWMGRID(:,:,k)
!        write(76,rec=k+KMAX*7) PRESGRID(:,:,k)
!     enddo
!     do k=1,KMAX+1
!        write(76,rec=k+KMAX*8) PINTGRID(:,:,k)
!     enddo
!     do k=1,3
!        write(76,rec=k+KMAX*8+1) PS(:,:,k)
!     enddo
!        write(76,rec=1+KMAX*9+4) ALON
!        write(76,rec=2+KMAX*9+4) ALAT
!     close(76)


C***********************************************************************
C  interpolate to the mass points (HPTS)
C***********************************************************************
C
c    & UATHPTS(KB,KMAX), VATHPTS(KB,KMAX),
c    & TATHPTS(KB,KMAX), QATHPTS(KB,KMAX),
c    &     PSATHPTS(KB), ZSATHPTS(KB),
c    & UATUPTS(KB,KMAX), VATUPTS(KB,KMAX), TATUPTS(KB,KMAX),
c    &     PSATUPTS(KB), ZSATUPTS(KB),
c    & UATVPTS(KB,KMAX), VATVPTS(KB,KMAX), TATVPTS(KB,KMAX),
c    &     PSATVPTS(KB), ZSATVPTS(KB)
C
      ALLOCATE(PSATHPTS(KB))
      ALLOCATE(ZSATHPTS(KB))
      ALLOCATE(TATHPTS(KB,KMAX))
      ALLOCATE(PINTATHPTS(KB,KMAX+1))
      ALLOCATE(UATHPTS(KB,KMAX))
      ALLOCATE(VATHPTS(KB,KMAX))
      ALLOCATE(QATHPTS(KB,KMAX))
      ALLOCATE(PRATHPTS(KB,KMAX))
      ALLOCATE(CWMATHPTS(KB,KMAX))
C
      write(6,*) 'before calling ll2pts for ps'
      IFIRST = 0
C  surface pressure at mass points 
      CALL LL2pts(PS(1,1,1),PSATHPTS,
     &            NHPT,HPTLON,HPTLAT,ALAT,ALON,IFIRST,
     &  WIJ,WIPJ,WIJP,WIPJP,
     &  KIJ,KIPJ,KIJP,KIPJP,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
      print *,'psathpts ',nhpt,kmax,ifirst
c     print 5444,(PSATHPTS(I),I=1,NHPT,150)
c5444 FORMAT(' some pts ',10e12.5)
C  topography at mass points 
      CALL LL2pts(PS(1,1,2),ZSATHPTS,
     &            NHPT,HPTLON,HPTLAT,ALAT,ALON,IFIRST,
     &  WIJ,WIPJ,WIJP,WIPJP,
     &  KIJ,KIPJ,KIJP,KIPJP,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
      print *,'zsathpts ',nhpt,kmax,ifirst
c     print 5444,(ZSATHPTS(5),I=1,NHPT,150)
      IFIRST = 0

        do I=1,NHPT !  min(300,NHPT)
!        write(0,*) 'I, PS, ZS:: ', I, PSATHPTS(I),ZSATHPTS(I)
          if (ZSATHPTS(I) .lt. 8860. .and. ZSATHPTS(I) .gt. -1000.) then
          else
            write(0,*) 'faulty I,ZSATHPTS(I):: ', I,ZSATHPTS(I)
            STOP 99
          endif
        enddo

C$OMP PARALLEL DO PRIVATE(K)
      DO K=1,KMAX
C  temperature at mass points and level k
!      print *,'before tathpts ',nhpt,kmax,ifirst
      CALL LL2pts(TGRID(1,1,K),TATHPTS(1,K),
     &            NHPT,HPTLON,HPTLAT,ALAT,ALON,IFIRST,
     &  WIJ,WIPJ,WIJP,WIPJP,
     &  KIJ,KIPJ,KIJP,KIPJP,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
!      print *,'tathpts', k,ifirst
c     print 5444,(TATHPTS(I,K),I=1,NHPT,150)
C  u (zonal) wind component at mass points and level k
      CALL LL2pts(UGRID(1,1,K),UATHPTS(1,K),
     &            NHPT,HPTLON,HPTLAT,ALAT,ALON,IFIRST,
     &  WIJ,WIPJ,WIJP,WIPJP,
     &  KIJ,KIPJ,KIJP,KIPJP,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
!      print *,'uathpts', k
c     print 5444,(UATHPTS(I,K),I=1,NHPT,150)
C  v (meridional) wind component at mass points and level k
      CALL LL2pts(VGRID(1,1,K),VATHPTS(1,K),
     &            NHPT,HPTLON,HPTLAT,ALAT,ALON,IFIRST,
     &  WIJ,WIPJ,WIJP,WIPJP,
     &  KIJ,KIPJ,KIJP,KIPJP,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
!      print *,'vathpts', k
c     print 5444,(VATHPTS(I,K),I=1,NHPT,150)
C  specific humidity at mass points and level k
      CALL LL2pts(QGRID(1,1,K),QATHPTS(1,K),
     &            NHPT,HPTLON,HPTLAT,ALAT,ALON,IFIRST,
     &  WIJ,WIPJ,WIJP,WIPJP,
     &  KIJ,KIPJ,KIJP,KIPJP,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
!      print *,'qathpts', k
c     print 5444,(QATHPTS(I,K),I=1,NHPT,150)
C  cloud water at mass points and level k
      CALL LL2pts(CWMGRID(1,1,K),CWMATHPTS(1,K),
     &            NHPT,HPTLON,HPTLAT,ALAT,ALON,IFIRST,
     &  WIJ,WIPJ,WIJP,WIPJP,
     &  KIJ,KIPJ,KIJP,KIPJP,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang

C  3D pressure at mass points and level k
      CALL LL2pts(PRESGRID(1,1,K),PRATHPTS(1,K),
     &            NHPT,HPTLON,HPTLAT,ALAT,ALON,IFIRST,
     &  WIJ,WIPJ,WIJP,WIPJP,
     &  KIJ,KIPJ,KIJP,KIPJP,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
!      print 5444,(PRATHPTS(I,K),I=1,NHPT,150)


      END DO
C$OMP END PARALLEL DO
      IFIRST=0
C$OMP PARALLEL DO PRIVATE(K)
        DO K=1,KMAX+1
C  3D interface pressure at mass points and level k
      CALL LL2pts(PINTGRID(1,1,K),PINTATHPTS(1,K),
     &            NHPT,HPTLON,HPTLAT,ALAT,ALON,IFIRST,
     &  WIJ,WIPJ,WIJP,WIPJP,
     &  KIJ,KIPJ,KIJP,KIPJP,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
        ENDDO
C$OMP END PARALLEL DO
C
C***********************************************************************
C  interpolate to the u-comp wind points (UPTS)
C***********************************************************************
C
      ALLOCATE(PSATUPTS(KB))
      ALLOCATE(ZSATUPTS(KB))
      ALLOCATE(TATUPTS(KB,KMAX))
      ALLOCATE(UATUPTS(KB,KMAX))
      ALLOCATE(VATUPTS(KB,KMAX))
      ALLOCATE(QATUPTS(KB,KMAX))
C
      IFIRST = 0
C  surface pressure at u-comp wind points 
      CALL LL2pts(PS(1,1,1),PSATUPTS,
     &            NUPT,UPTLON,UPTLAT,ALAT,ALON,IFIRST,
     &  WIJ,WIPJ,WIJP,WIPJP,
     &  KIJ,KIPJ,KIJP,KIPJP,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
c     print 5444,(PSATUPTS(I),I=1,NUPT,150)
C  topography at u-comp wind points 
      CALL LL2pts(PS(1,1,2),ZSATUPTS,
     &            NUPT,UPTLON,UPTLAT,ALAT,ALON,IFIRST,
     &  WIJ,WIPJ,WIJP,WIPJP,
     &  KIJ,KIPJ,KIJP,KIPJP,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
c     print 5444,(ZSATUPTS(I),I=1,NUPT,150)

C$OMP PARALLEL DO PRIVATE(K)
      DO K=1,KMAX
C  temperature at u-comp wind points and level k
      CALL LL2pts(TGRID(1,1,K),TATUPTS(1,K),
     &            NUPT,UPTLON,UPTLAT,ALAT,ALON,IFIRST,
     &  WIJ,WIPJ,WIJP,WIPJP,
     &  KIJ,KIPJ,KIJP,KIPJP,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
c     print 5444,(TATUPTS(I,K),I=1,NUPT,150)
C  u (zonal) wind component at u-comp wind points and level k
      CALL LL2pts(UGRID(1,1,K),UATUPTS(1,K),
     &            NUPT,UPTLON,UPTLAT,ALAT,ALON,IFIRST,
     &  WIJ,WIPJ,WIJP,WIPJP,
     &  KIJ,KIPJ,KIJP,KIPJP,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
c     print 5444,(UATUPTS(I,K),I=1,NUPT,150)
C  v (meridional) wind component at u-comp wind points and level k
      CALL LL2pts(VGRID(1,1,K),VATUPTS(1,K),
     &            NUPT,UPTLON,UPTLAT,ALAT,ALON,IFIRST,
     &  WIJ,WIPJ,WIJP,WIPJP,
     &  KIJ,KIPJ,KIJP,KIPJP,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
c     print 5444,(VATUPTS(I,K),I=1,NUPT,150)
      ENDDO
C$OMP END PARALLEL DO
C
      
      write(0,*) 'K,UPTLON(K),UPTLAT(K),UATUPTS(K,1),VATUPTS(K,1)'
      do K=855,KB
      write(0,*)  K,UPTLON(K),UPTLAT(K),UATUPTS(K,1),VATUPTS(K,1)
      enddo
      write(0,*) 'L,UATUPTS(857,L),UATUPTS(858,L)'
      do L=1,KMAX
      write(0,*)  L,UATUPTS(857,L),UATUPTS(858,L)
      enddo
     
C***********************************************************************
C  interpolate to the v-comp wind points (VPTS)
C***********************************************************************
C
      ALLOCATE(PSATVPTS(KB))
      ALLOCATE(ZSATVPTS(KB))
      ALLOCATE(TATVPTS(KB,KMAX))
      ALLOCATE(UATVPTS(KB,KMAX))
      ALLOCATE(VATVPTS(KB,KMAX))
      ALLOCATE(QATVPTS(KB,KMAX))
C
      IFIRST = 0
C  surface pressure at v-comp wind points 
      CALL LL2pts(PS(1,1,1),PSATVPTS,
     &            NVPT,VPTLON,VPTLAT,ALAT,ALON,IFIRST,
     &  WIJ,WIPJ,WIJP,WIPJP,
     &  KIJ,KIPJ,KIJP,KIPJP,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
c     print 5444,(PSATVPTS(I),I=1,NVPT,150)
C  topography at v-comp wind points 
      CALL LL2pts(PS(1,1,2),ZSATVPTS,
     &            NVPT,VPTLON,VPTLAT,ALAT,ALON,IFIRST,
     &  WIJ,WIPJ,WIJP,WIPJP,
     &  KIJ,KIPJ,KIJP,KIPJP,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
c     print 5444,(ZSATVPTS(I),I=1,NVPT,150)
C$OMP PARALLEL DO PRIVATE(K)
      DO K=1,KMAX
C  temperature at v-comp wind points and level k
      CALL LL2pts(TGRID(1,1,K),TATVPTS(1,K),
     &            NVPT,VPTLON,VPTLAT,ALAT,ALON,IFIRST,
     &  WIJ,WIPJ,WIJP,WIPJP,
     &  KIJ,KIPJ,KIJP,KIPJP,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
c     print 5444,(TATVPTS(I,K),I=1,NVPT,150)
C  u (zonal) wind component at v-comp wind points and level k
      CALL LL2pts(UGRID(1,1,K),UATVPTS(1,K),
     &            NVPT,VPTLON,VPTLAT,ALAT,ALON,IFIRST,
     &  WIJ,WIPJ,WIJP,WIPJP,
     &  KIJ,KIPJ,KIJP,KIPJP,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
c     print 5444,(UATVPTS(I,K),I=1,NVPT,150)
C  v (meridional) wind component at v-comp wind points and level k
      CALL LL2pts(VGRID(1,1,K),VATVPTS(1,K),
     &            NVPT,VPTLON,VPTLAT,ALAT,ALON,IFIRST,
     &  WIJ,WIPJ,WIJP,WIPJP,
     &  KIJ,KIPJ,KIJP,KIPJP,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
c     print 5444,(VATVPTS(I,K),I=1,NVPT,150)
      ENDDO
C$OMP END PARALLEL DO
C
C
      IF( KBETA.EQ.KB ) THEN
c       CALL WRTETA(TATHPTS,UATUPTS,VATUPTS,QATHPTS,PSATHPTS,
c    &   ZSATHPTS,NUNIT)
C
       ALLOCATE(UB(KB,KMAX))
       ALLOCATE(VB(KB,KMAX))
       ALLOCATE(TB(KB,KMAX))
       ALLOCATE(QB(KB,KMAX))
       ALLOCATE(CWMB(KB,KMAX))
       ALLOCATE(PB(KB))
       ALLOCATE(ZB(KB))
       ALLOCATE(PRB(KB,KMAX))
C
C   WRITE BOUNDARY DATA FOR PETABC
C

	DO L = 1, KMAX
      write(6,*)'K=1,L,T,U,V,Q,CWM,pmid,pint: ',L,TATHPTS(1,L),
     &     UATUPTS(1,L),VATUPTS(1,L),QATHPTS(1,L),CWMATHPTS(1,L),
     &                  PRATHPTS(1,L),PINTATHPTS(1,L)
	enddo

C$OMP PARALLEL DO PRIVATE(K,L)
       DO K = 1, KMAX
        DO L = 1, KB
          IF(K. EQ. 1) THEN
            PB(L) = PSATHPTS(L)
            ZB(L) = ZSATHPTS(L)
          ENDIF
          UB(L,K) = UATUPTS(L,K)
          VB(L,K) = VATUPTS(L,K)
          TB(L,K) = TATHPTS(L,K)
          QB(L,K) = QATHPTS(L,K)
          CWMB(L,K) = CWMATHPTS(L,K)
          PRB(L,K) = PRATHPTS(L,K)
        ENDDO
       ENDDO
C$OMP END PARALLEL DO

C	write(6,*) 'dumping out wiht KMAX, KB: ', KMAX, KB
C	write(6,*) 'output unit #: ',NUNITHOLD
C	write(6,*) 'output file name: ',HOLDFIL
C       NUNITHOLD=NUNIT+100
C       WRITE(HOLDFIL,99999)NUNITHOLD
C
C       DEALLOCATE(SI)
C       DEALLOCATE(SL)
C
       DEALLOCATE(PSATHPTS)
       DEALLOCATE(ZSATHPTS)
       DEALLOCATE(TATHPTS)
       DEALLOCATE(UATHPTS)
       DEALLOCATE(VATHPTS)
       DEALLOCATE(QATHPTS)
       DEALLOCATE(CWMATHPTS)
       DEALLOCATE(PSATUPTS)
       DEALLOCATE(ZSATUPTS)
       DEALLOCATE(TATUPTS)
       DEALLOCATE(UATUPTS)
       DEALLOCATE(VATUPTS)
       DEALLOCATE(QATUPTS)
       DEALLOCATE(PSATVPTS)
       DEALLOCATE(ZSATVPTS)
       DEALLOCATE(TATVPTS)
       DEALLOCATE(UATVPTS)
       DEALLOCATE(VATVPTS)
       DEALLOCATE(QATVPTS)
c       
c     ELSE
c       CALL WRITER(NUPT,NVPT,NHPT,NUNIT,ITIME,DLAM0,
c    1   JCAP)
      END IF
C
C   INTERPOLATE TO ETA VERTICAL COORDINATE
C
!      CALL PETABCS(VPTLAT,VPTLON,KMAX,NUNITHOLD)
	write(6,*) 'call SIG2HYB'
       CALL SIG2HYB(VPTLAT,VPTLON,KMAX,NUNITHOLD,
     & p_top_requested,ptsgm,levels,                                      !zhang for model sigma levels
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,
     & KMAX,TB,UB,VB,QB,CWMB,PRB,PINTATHPTS,PB,ZB)
       DEALLOCATE(TB,UB,VB,QB,CWMB,PRB,PINTATHPTS,PB,ZB)
	write(6,*) 'return SIG2HYB'
C 9800 CONTINUE
C
C.. HORIZONTAL INTERPOLATION FOR THE INITIAL FIELDS (WHEN ITM=0)
C                        YC KWON
C
      IF(ITM.EQ.0) THEN
      ALLOCATE(UINIT(IMJM,KMAX))
      ALLOCATE(VINIT(IMJM,KMAX))
      ALLOCATE(TINIT(IMJM,KMAX))
      ALLOCATE(QINIT(IMJM,KMAX))
      ALLOCATE(CWMINIT(IMJM,KMAX))
      ALLOCATE(PMID(IMJM,KMAX))
      ALLOCATE(PINT(IMJM,KMAX+1))
      ALLOCATE(PSINIT(IMJM))
      ALLOCATE(ZSINIT(IMJM))
      WRITE(6,*) 'ITM = 0 (AFTER ALLOCATION)'
C
      IFIRST=0
      CALL LL2PTS_INIT(PS(1,1,1),PSINIT,IMJM,HLON,HLAT,
     1    ALAT,ALON,IFIRST,
     &  WIJ_init,WIPJ_init,WIJP_init,WIPJP_init,
     &  KIJ_init,KIPJ_init,KIJP_init,KIPJP_init,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
      WRITE(6,*) 'ITM = 0 (AFTER LL2PTS_INIT PS)'
      CALL LL2PTS_INIT(PS(1,1,2),ZSINIT,IMJM,HLON,HLAT,
     1    ALAT,ALON,IFIRST,
     &  WIJ_init,WIPJ_init,WIJP_init,WIPJP_init,
     &  KIJ_init,KIPJ_init,KIJP_init,KIPJP_init,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
      WRITE(6,*) 'ITM = 0 (AFTER LL2PTS_INIT ZS)'

      IFIRST=0
C$OMP PARALLEL DO PRIVATE(K)
      DO K = 1,KMAX
      CALL LL2PTS_INIT(TGRID(1,1,K),TINIT(1,K),IMJM,HLON,HLAT,
     1    ALAT,ALON,IFIRST,
     &  WIJ_init,WIPJ_init,WIJP_init,WIPJP_init,
     &  KIJ_init,KIPJ_init,KIJP_init,KIPJP_init,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
      CALL LL2PTS_INIT(QGRID(1,1,K),QINIT(1,K),IMJM,HLON,HLAT,
     1    ALAT,ALON,IFIRST,
     &  WIJ_init,WIPJ_init,WIJP_init,WIPJP_init,
     &  KIJ_init,KIPJ_init,KIJP_init,KIPJP_init,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
      CALL LL2PTS_INIT(CWMGRID(1,1,K),CWMINIT(1,K),IMJM,HLON,HLAT,
     1    ALAT,ALON,IFIRST,
     &  WIJ_init,WIPJ_init,WIJP_init,WIPJP_init,
     &  KIJ_init,KIPJ_init,KIJP_init,KIPJP_init,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
      CALL LL2PTS_INIT(PRESGRID(1,1,K),PMID(1,K),IMJM,HLON,HLAT,
     1    ALAT,ALON,IFIRST,
     &  WIJ_init,WIPJ_init,WIJP_init,WIPJP_init,
     &  KIJ_init,KIPJ_init,KIJP_init,KIPJP_init,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
      ENDDO
C$OMP END PARALLEL DO

      IFIRST=0
C$OMP PARALLEL DO PRIVATE(K)
      DO K=1,KMAX+1
      CALL LL2pts_INIT(PINTGRID(1,1,K),PINT(1,K),IMJM,HLON,HLAT,
     1    ALAT,ALON,IFIRST,
     &  WIJ_init,WIPJ_init,WIJP_init,WIPJP_init,
     &  KIJ_init,KIPJ_init,KIJP_init,KIPJP_init,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
      ENDDO
C$OMP END PARALLEL DO

      IFIRST=0
C$OMP PARALLEL DO PRIVATE(K)
      DO K = 1,KMAX
      CALL LL2PTS_INIT(UGRID(1,1,K),UINIT(1,K),IMJM,VLON,VLAT,
     1    ALAT,ALON,IFIRST,
     &  WIJ_init,WIPJ_init,WIJP_init,WIPJP_init,
     &  KIJ_init,KIPJ_init,KIJP_init,KIPJP_init,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
      CALL LL2PTS_INIT(VGRID(1,1,K),VINIT(1,K),IMJM,VLON,VLAT,
     1    ALAT,ALON,IFIRST,
     &  WIJ_init,WIPJ_init,WIJP_init,WIPJP_init,
     &  KIJ_init,KIPJ_init,KIJP_init,KIPJP_init,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,IMAX,JMAX) !Zhang
      ENDDO
C$OMP END PARALLEL DO

      write(0,*) 'L,UINIT(91806,L),UINIT(92235,L)'
      do L=1,KMAX
      write(0,*)  L,UINIT(91806,L),UINIT(92235,L)
      enddo

C
C.. WRITE TEST DATA TO CHECK WHETHER THEHORIZONTAL INTERPOLATION IS CORRECT OR NOT
C..            TEMPERATURE
C
C       OPEN(345,FILE='/ptmp/wx22yk/MKBND/t-ll2pts.dat',STATUS='UNKNOWN')
C       DO K = 1,KMAX
C       WRITE(345,346) (TINIT(I,K),I=1,IMJM)
C346    FORMAT(10F10.4)
C       ENDDO
C
C.. WRITE HORIZONTALLY INTERPLOTED DATA FOR THE VERTICAL INTERPOLATION
C
      WRITE(6,*) '==CHECK T OF SIGMA FILES AT 985 (127,5)=='
      DO L = 1,KMAX
      WRITE(6,*)'===SIGMA T AFTER H. INTERP ',L,TINIT(985,L)
      ENDDO
C
      WRITE(6,*) '========= VALUES RIGHT AFTER LL2PTS ========='
      WRITE(6,*) 'T 100 30 ',TINIT(100,30)
      WRITE(6,*) 'U 100 30 ',UINIT(100,30)
      WRITE(6,*) 'V 100 30 ',VINIT(100,30)
      WRITE(6,*) 'U IMJM 30 ',UINIT(IMJM,30)
      WRITE(6,*) 'V IMJM 30 ',VINIT(IMJM,30)
      WRITE(6,*) 'U IMJM-1 30 ',UINIT(IMJM-1,30)
      WRITE(6,*) 'V IMJM-1 30 ',VINIT(IMJM-1,30)
      WRITE(6,*) 'Q 100 30 ',QINIT(100,30)
      WRITE(6,*) 'CWM 100 30 ',CWMINIT(100,30)
      WRITE(6,*) 'PSFC 100 ',PSINIT(100)
      WRITE(6,*) 'ZSFC 100 ',ZSINIT(100)
      WRITE(6,*) '============================================='
C
c$$$       OPEN(225,FILE='hold_init',FORM='UNFORMATTED',IOSTAT=IER)
c$$$       WRITE(225)TINIT,UINIT,VINIT,QINIT,CWMINIT,PMID,PINT,PSINIT,ZSINIT
c$$$       CLOSE(225)
C
C.. VERTICAL INTERPLOATION
C  NOTE: TINIT, and the other *INIT vars are modified during this call:
      CALL SIG2HYB_INIT(VLAT,VLON,KMAX,225,
     & p_top_requested,ptsgm,levels,                                      !zhang for model sigma levels
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,
     & KMAX,TINIT,UINIT,VINIT,QINIT,CWMINIT,PMID,PINT,PSINIT,ZSINIT)
C
C.. END OF PREPARING THE INITIAL CONDITION
C                   YC KWON
C
       DEALLOCATE(TGRID)
       DEALLOCATE(UGRID)
       DEALLOCATE(VGRID)
       DEALLOCATE(QGRID)
       DEALLOCATE(CWMGRID)
       DEALLOCATE(PRESGRID)
       DEALLOCATE(PS)
       DEALLOCATE(PINTGRID)
C
      DEALLOCATE(UINIT)
      DEALLOCATE(VINIT)
      DEALLOCATE(TINIT)
      DEALLOCATE(QINIT)
      DEALLOCATE(CWMINIT)
      DEALLOCATE(PSINIT)
      DEALLOCATE(ZSINIT)
C      DEALLOCATE(SI)
C      DEALLOCATE(SL)
      ENDIF         !ITM=0
C
      CALL W3TAGE('MKBND   ')
        return
        END


      module module_wrf_error_dummy
        INTEGER           :: wrf_debug_level = 0
        CHARACTER*256     :: wrf_err_message
      
        LOGICAL :: silence=.false.  ! T = this process should not log.
        LOGICAL :: buffered=.false. ! T = messages sent via clog_write
        LOGICAL :: stderrlog=.false.! T = send to write(0,...) if buffered=F
      
        INTEGER, PARAMETER :: wrf_log_flush=0
        INTEGER, PARAMETER :: wrf_log_set_buffer_size=1
        INTEGER, PARAMETER :: wrf_log_write=2
      
        !NOTE: Make sure silence, buffered and stderrlog settings match the
        ! namelist defaults in init_module_wrf_error.
      
      ! min_allowed_buffer_size: requested buffer sizes smaller than this
      ! will simply result in disabling of log file buffering.  This number
      ! should be larger than any line WRF prints frequently.  If you set it 
      ! too small, the buffering code will still work.  However, any line 
      ! that is larger than the buffer will result in two writes: one for 
      ! the line and one for the end-of-line character at the end.
        integer, parameter :: min_allowed_buffer_size=200
      end module module_wrf_error_dummy
      
      SUBROUTINE wrf_abort
        STOP 99
      END SUBROUTINE wrf_abort
      SUBROUTINE get_current_time_string( time_str )
        CHARACTER(LEN=*), INTENT(OUT) :: time_str
        time_str = ' '
      END SUBROUTINE get_current_time_string
      
      SUBROUTINE get_current_grid_name( grid_str )
        CHARACTER(LEN=*), INTENT(OUT) :: grid_str
        grid_str = ' '
      END SUBROUTINE get_current_grid_name
      
      SUBROUTINE wrf_error_fatal(s)
        implicit none
        character(*) :: s
        write(0,*) s
        write(6,*) s
        call wrf_abort()
      END SUBROUTINE wrf_error_fatal
      
      SUBROUTINE wrf_message(s)
        implicit none
        character(*) :: s
        write(6,*) s
      END SUBROUTINE wrf_message
      
      SUBROUTINE wrf_debug(i,s)
        use module_wrf_error_dummy
        implicit none
        integer :: i
        character(*) :: s
        if(i<=wrf_debug_level) write(6,*) s
      END SUBROUTINE wrf_debug
      
      SUBROUTINE set_wrf_debug_level(i)
        use module_wrf_error_dummy
        implicit none
        integer i
        wrf_debug_level=i
      END SUBROUTINE set_wrf_debug_level
