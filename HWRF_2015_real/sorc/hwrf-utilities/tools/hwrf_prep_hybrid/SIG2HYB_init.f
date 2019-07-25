      MODULE MODULE_SIG2HYB_INIT
      PRIVATE
      PUBLIC :: SIG2HYB_INIT
      CONTAINS

      SUBROUTINE SIG2HYB_INIT(ULATB,UWLONB,KMAX,INUNIT,
     & p_top_requested,ptsgm1,levels,                                      !zhang for model sigma levels
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,
     & LDM,T,U,V,Q,CWM,PRES,DUM,PSFC,ZSFC)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: SIG2HYB         INTERPOLATES NMM BND VALUES FROM SIGMA TO HYB
C   PRGMMR: ROGERS           ORG: NP22        DATE: 97-12-31  
C
C ABSTRACT: PERFORMS VERTICAL INTERPOLATION (QUADRATIC IN LOG P
C   FOR HEIGHT, LINEAR IN LOG P FOR WIND/SPECIFIC HUMIDITY) OF THE
C   ETA MODEL BOUNDARY VALUES FROM THE AVN SIGMA LEVELS TO ETA LEVELS.
C
C PROGRAM HISTORY LOG:
C   ??-??-??  T BLACK
C   95-02-07  E ROGERS ADDED DOCBLOCK
C   96-11-18  E ROGERS ADDED INCLUDES FOR E-GRID DIMENSIONS AND GLOBAL
C             MODEL VERTICAL STRUCTURE
C   98-03-08  E ROGERS ADDED READ OF GDAS SIGMA VALUES FROM BINARY FILE
C   99-02-10  E ROGERS COMBINED MKBND AND PETABCS INTO ONE CODE
C   04-12-06  M PYLE TOOK PETABCS CODE, AND MODIFIED IT TO HANDLE THE
C             NMM HYBRID VERTICAL COORDINATE
C      09-06  YOUNG KWON MODIFIED FOR HWRF 
C             READ TOPO FROM SI STATIC OUTPUT
C             INTERPOLATE GLOBAL SPECTRAL DATA OVER WHOLE HWRF DOMAIN AT INITIAL TIME
C
C   12-05-23  InHyuk KWON MODIFIED
C             VIRTUAL TEMPERATURE IS INTERPOLATED (NOT CALCULATED FROM H)
C             LAPSE RATE IS APPLIED FOR BELOW GROUND VALUES
C             BUG FIXED THAT CAUSES NOISE AT SURFACE LAYER
C
C USAGE:
C
C   INPUT FILES:
C     UNIT45     - DEPTH OF THE ETA MODEL LAYERS
C     UNIT51     - ETA BOUNDARY VALUES FROM AVN FORECAST IN SIGMA
C
C   OUTPUT FILES:
C     UNIT52     - ETA BOUNDARY VALUES FROM AVN FORECAST IN ETA
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - FLIP, ROTLLE
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C   REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: STANDARD FORTRAN
C   MACHINE: CRAY YMP/C90
C
C
C     SETTING UP OF THE VERTICAL GRID VARIABLES, AND
C     VERTICAL INTERPOLATION, PRESSURE TO ETA SURFACES
C     THIS VERSION USES INPUT SIGMA DATA INSTEAD OF
C     STANDARD LEVEL PRESSURE DATA
C$$$
C----------------------------------------------------------------
C----------------------------------------------------------------
C
        use height_reader
        use MODULE_FLIP
        integer,parameter::real_32=selected_real_kind(6,30)
        INCLUDE "parmlbc"
C
!zhang                        P A R A M E T E R
!zhang     1 (LMM1=LM-1,LMP1=LM+1)
!zhang                        P A R A M E T E R
!zhang     1 (IMT=2*IM-1,JMT=JM/2+1)
                        P A R A M E T E R
     1 (CM1=2937.4,CM2=4.9283,CM3=23.5518,EPS=0.622)
C----------------------------------------------------------------
                        D I M E N S I O N
     1  UWLONB(IMJM),ETA(LMP1),DETA(LM),tmp(LM)
     2, ULATB(IMJM),K1(IMJM),K2(IMJM)
C
      INTEGER, INTENT(IN) :: LDM
      REAL(REAL_32),POINTER,INTENT(INOUT),DIMENSION(:,:) ::
     &                       T,U,V,Q,CWM,PRES
      REAL(REAL_32), INTENT(INOUT), POINTER, DIMENSION(:,:) :: DUM
      REAL(REAL_32), INTENT(INOUT), POINTER, DIMENSION(:) :: ZSFC,PSFC


      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::TB,UB,VB,QB,CWMB
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::PINT_3D,PINT_V
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::PMID_3D,PMID_V
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::H,HETA
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::QETA,U_OUT,V_OUT
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::PINT,PMID
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::PDO_2D,PDO_V
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::PINT_V1,PMID_V1
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::dpdin_in,PIO_2D
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::Z_OUT,T_OUT
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::Q_OUT,CWM_OUT
!      REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::hlatb,hlonb
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::PD,HGT
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::EXNL,REF,TSGBT
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::ULONB,PSFC_OUT
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::PDO,PIO,PMO,Y2,PDO_V1
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::DUM1,DUM2
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::SI,SL,SII,SLL
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::SIGINT,SIGMID
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::PIN,UIN,VIN,TMPVRT
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::PDB
      REAL LEVELS(LMP1)

        INTEGER,ALLOCATABLE,DIMENSION(:)::INPUT_TOP
C
      REAL hlat(im,jm),hlon(im,jm)

      REAL, ALLOCATABLE, DIMENSION(:):: DSG,DSG1,DSG2,
     &                                   SG, SG1, SG2,
     &                                   SGML1, SGML2
C
                             P A R A M E T E R
     & (K15=SELECTED_REAL_KIND(15))
C
                             R E A L
!     & (KIND=K15) DLT(LM),ALPR(101),ALP(101)
     & (KIND=K15) ALPR(KMAX-1),ALP(KMAX)
C
      CHARACTER HOLDFILP*80, C32_MAPROJ*32
      REAL DELTAX, DELTAY, STD_LON,STD_LAT, STD_LAT2
      REAL, ALLOCATABLE :: DATA(:,:,:)
      REAL PDINIT(IM,JM),TINIT(IM,LM,JM),QINIT(IM,LM,JM)
      REAL CWMINIT(IM,LM,JM),UINIT(IM,LM,JM),VINIT(IM,LM,JM)
      REAL PINIT(IM,LM+1,JM)
      REAL INEXP
      integer ierr

      REAL, dimension(:,:), allocatable :: lats,lons,heights
      REAL, dimension(:,:), allocatable :: landmask
      type(rotlatlon_info) :: proj
C----------------------------------------------------------------
C
C  SIGMA SURFACES FOR GLOBAL SYSTEM
C  NOW READ IN FROM SUBROUTINE CALL
C
                            D A T A
     1  LINI/51/, LOUT/53/
C***********************************************************************
czhang adding namelist for levels and etop
       integer :: ptsgm1
       real :: p_top_requested
       real  :: ptsgm

!zz      DATA eta_levels/1.0,0.995253,0.990479,0.985679,0.980781,0.975782,
!zz     1       0.970684,0.965486,0.960187,0.954689,0.948991,0.943093,
!zz     1       0.936895,0.930397,0.923599,0.916402,0.908404,0.899507,
!zz     1       0.888811,0.876814,0.862914,0.847114,0.829314,0.809114,
!zz     1       0.786714,0.762114,0.735314,0.706714,0.676614,0.645814,
!zz     1       0.614214,0.582114,0.549714,0.517114,0.484394,0.451894,
!zz     1       0.419694,0.388094,0.356994,0.326694,0.297694,0.270694,
!zz     1       0.245894,0.223694,0.203594,0.185494,0.169294,0.154394,
!zz     1       0.140494,0.127094,0.114294,0.101894,0.089794,0.078094,
!zz     1       0.066594,0.055294,0.044144,0.033054,0.022004,0.010994,
!zz     1       .0000000/
Czhang added 61 vertical levels
czhang      DATA LEVELS/1.0, .9919699, .9827400, .9721600, .9600599, .9462600,
czhang     1    .9306099,    .9129300, .8930600, .8708600, .8462000, .8190300,
czhang     1    .7893100,    .7570800, .7224600, .6856500, .6469100, .6066099,
czhang     1    .5651600,    .5230500, .4807700, .4388600, .3978000, .3580500,
czhang     1    .3200099,    .2840100, .2502900, .2190100, .1902600, .1640600,
czhang     1    .1403600,    .1190600, .1000500, .0831600, .0682400, .0551200,
czhang     1    .0436200,    .0335700, .0248200, .0172200, .0106300, .0049200,
czhang     1    .0000000/
czhang      DATA LEVELS/1.0,0.995253,0.990479,0.985679,0.980781,0.975782,
czhang     1       0.970684,0.965486,0.960187,0.954689,0.948991,0.943093,
czhang     1       0.936895,0.930397,0.923599,0.916402,0.908404,0.899507,
czhang     1       0.888811,0.876814,0.862914,0.847114,0.829314,0.809114,
czhang     1       0.786714,0.762114,0.735314,0.706714,0.676614,0.645814,
czhang     1       0.614214,0.582114,0.549714,0.517114,0.484394,0.451894,
czhang     1       0.419694,0.388094,0.356994,0.326694,0.297694,0.270694,
czhang     1       0.245894,0.223694,0.203594,0.185494,0.169294,0.154394,
czhang     1       0.140494,0.127094,0.114294,0.101894,0.089794,0.078094,
czhang     1       0.066594,0.055294,0.044144,0.033054,0.022004,0.010994,
czhang     1       .0000000/
czhang end of modification
      LDMP=LDM+1
      LDMM=LDM-1
C
!      DO IER=1,101
!        ALPR(IER)=0.0
!        ALP(IER)=0.0
!      ENDDO
C
      print *,'petabcs ',kmax,ldm,ldmp,ldmm
C
      REWIND LINI
C
C      READ(41,MODTOP)      !KWON
C     READ(41,MODTOP,END=10)
Czhang      ETOP = 50.            !KWON PTOP OF HWRF IS 50MB
      ETOP = p_top_requested  !Zhang PTOP OF basin scale of HWRF IS 2MB
   10 CONTINUE
C      REWIND 41            !KWON
C
      print*, "zhang debug ==", etop
      print*, "zhang debug ==", levels
      ALLOCATE(SI(KMAX+1))
      ALLOCATE(SII(KMAX+1))
      ALLOCATE(SL(KMAX))
      ALLOCATE(SLL(KMAX))
C
C
C      READ(INUNIT)SI,SL
C
C   SWITCH INPUT LONGITUDE BACK TO DEGREES EAST
C
      ALLOCATE(ULONB(IMJM))
C
      DO K = 1, IMJM
        ULONB(K) = 360.0 - UWLONB(K)
      ENDDO
C
      DO 1000 KTIME=1,1
C
      ALLOCATE(PINT(IMJM,LDM+2))
      ALLOCATE(PSFC_OUT(IMJM))
C***
C***  READ GLOBAL DATA ON BOUNDARY (OUTPUT OF LL2PTS)
C***
      write(0,*) 'T 100 30 ',T(100,30)
      write(0,*) 'U 100 30 ',U(100,30)
      write(0,*) 'V 100 30 ',V(100,30)
      write(0,*) 'U IMJM 30 ',U(IMJM,30)
      write(0,*) 'V IMJM 30 ',V(IMJM,30)
      write(0,*) 'U IMJM-1 30 ',U(IMJM-1,30)
      write(0,*) 'V IMJM-1 30 ',V(IMJM-1,30)
      write(0,*) 'Q 100 30 ',Q(100,30)
      write(0,*) 'CWM 100 30 ',CWM(100,30)
      write(0,*) 'PSFC 100 ',PSFC(100)
      write(0,*) 'ZSFC 100 ',ZSFC(100)
C
      write(0,*)'AFTER READ INITIAL DATA OVER GLOBAL MODEL LEVELS'
C

!!! have PSFC, U, and V on GFS vertical coord

!	do K=1,10
!	write(6,*) 'lev 1 K, T,U,V: ', K, T(K,1),U(K,1),V(K,1)
!	write(6,*) 'lev LDM K, T,U,V: ', K, T(K,LDM),U(K,LDM),V(K,LDM)
!	enddo

      write(6,12121)INUNIT
12121 FORMAT(' READ PTETBC  INUNIT=',I3)
C***
C***  FLIP 3-DIMENSIONAL ARRAYS BEFORE INTERPOLATION
C***
      CALL FLIP_PTR(T,LDM,IMJM,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT) !Zhang
      CALL FLIP_PTR(U,LDM,IMJM,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT) !Zhang
      CALL FLIP_PTR(V,LDM,IMJM,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT) !Zhang
      CALL FLIP_PTR(Q,LDM,IMJM,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT) !Zhang
      CALL FLIP_PTR(CWM,LDM,IMJM,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT) !Zhang ! not done previously
      CALL FLIP_PTR(PRES,LDM,IMJM,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT) !Zhang ! new variable
      CALL FLIP_PTR(DUM,LDM+1,IMJM,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT) !Zhang ! new variable
C
      DO K = 1,IMJM
      DO I = 1,LDM+1
      PINT(K,I) = DUM(K,I)
      ENDDO
      ENDDO
C
      write(0,*)'===AFTER FLIP==='
      write(0,*) 'T 100 30 ',T(100,30)
      write(0,*) 'U 100 30 ',U(100,30)
      write(0,*) 'V 100 30 ',V(100,30)
      write(0,*) 'U IMJM 30 ',U(IMJM,30)
      write(0,*) 'V IMJM 30 ',V(IMJM,30)
      write(0,*) 'Q 100 30 ',Q(100,30)
C***
C***  ROTATE WINDS FROM LAT/LONG GRID TO ETA GRID
C***

      CALL ROTLLE(U,V,ULATB,ULONB,LDM,IMJM)
	write(0,*) 'call rotlle'
      write(0,*)'===AFTER ROTLLE==='
      write(0,*) 'U 100 30 ',U(100,30)
      write(0,*) 'V 100 30 ',V(100,30)
      write(0,*) 'U IMJM 30 ',U(IMJM,30)
      write(0,*) 'V IMJM 30 ',V(IMJM,30)
C***
C***  CONVERT SURFACE PRESSURE TO PASCALS
C***
!      ALLOCATE(TSGBT(IMJM))
C
      DO 50 K=1,IMJM
!       PSFC(K)=PSFC(K)*100.
!       IF(PSFC(K).LE.0.) write(6,*)'PSFC<=0 AT ',K
!       TSGBT(K)=T(K,LDM)
   50 CONTINUE
C***
C***  DEFINITION OF CONSTANTS NEEDED FOR VERTICAL INTERPOLATION
C***

!!!
!!!	 USE WRF STANDARDS HERE...
!!!
          ! Virtual temperature
      G=9.81
      R=287.04
      CP=1004.6
      P00=1.0E5
      PRF0=101325.
      GAMMA=0.0065
      T0=288.
      ROG=R/G
      CPOG=CP/G
      CAPA=R/CP
      GORG=G/(R*GAMMA)
      RGOG=R*GAMMA/G

! ------------------------------------------ IH Kwon
      pq0=379.90516
      a1 =0.608
      a2 =17.2693882
      a3 =273.16
      a4 =35.86
! ------------------------------------------

C***
C***  DEFINITION OF PRESSURE DATA AND ETA LAYER CONSTANTS
C***
      LDMM1=LDM-1
C***
C*** NOTE: THE PRESSURE AT THE TOP OF THE MODEL IS ETOP MB
C***
      PT= ETOP*100.
      ALPT=ALOG(PT)
C
C------- DEPTH OF HYBRID LAYERS ----------------------------------------
C
C      write(6,*) 'BEFORE READ 42'
C      READ(42,V_LEVELS)     !READ FULL LEVEL SIGMA (1-0) LM+1

C      write(6,132) LEVELS(1)
C132   FORMAT(1X,'levels at l=1 ',F10.2)

C SHOULD BE CHANGED FOR FULL LEVELS

	do L=1,LMP1
	ETA(L)=LEVELS(L)
	write(0,*) 'L, ETA(L): ', L, ETA(L)
	enddo

	do L=1,LM
	DETA(L) = ETA(L) - ETA(L+1)
	enddo

C***  MAJOR INSERT OF CODE FROM WRFSI VINTERP


!      ptsgm=42000.  ! Pa
      ptsgm=float(ptsgm1)  ! Pa
      write(0,*) 'ptsgm== ',ptsgm,ptsgm1

!       lpt2=0
        lpt2=LMP1

        write(0,*) 'pt= ', pt

        DO L=LMP1,1,-1
          pl=ETA(L)*(PRF0-pt)+pt
          write(0,*) 'L, PL: ', L, PL
          if(pl.lt.ptsgm) lpt2=l
        ENDDO

!      IF(lpt2.gt.0) THEN
      IF(lpt2.lt.LMP1) THEN
        pt2=ETA(lpt2)*(PRF0-pt)+pt
        write(0,*) 'ETA, PRF0, PT, PT2: ', ETA(LPT2),PRF0, PT, PT2
      ELSE
        pt2=pt
      ENDIF

!new
!       if (lpt2.gt.0.) lpt2=lpt2-1 ! zj addition (accounts for
!                                    !              layer/interface diffs?)
!new

      print*,'*** Sigma system starts at ',pt2,' Pa, from level ',lpt2

      pdtop=pt2-pt

        write(0,*) 'allocating dsg,dsg1,dsg2 as ', LM

        ALLOCATE(DSG(LM))
        ALLOCATE(DSG1(LM))
        ALLOCATE(DSG2(LM))

        DSG=-99.
        DSG1=-99.
        DSG2=-99.

      DO L=1,LM
        dsg(L)=DETA(L)
      ENDDO

        dsg1=0.
        dsg2=0.

      DO L=LM,1,-1

       IF(L.ge.lpt2) then
        dsg1(L)=dsg(L)
       ELSE
        dsg2(L)=dsg(L)
       ENDIF


      ENDDO

        ALLOCATE(SG1(LMP1))
        ALLOCATE(SG2(LMP1))
        ALLOCATE(SGML1(LM))
        ALLOCATE(SGML2(LM))

        SGML1=-99.
        SGML2=-99.

!      IF(lpt2.gt.0) THEN
       IF(lpt2.le.LMP1) THEN

        DO L=LMP1,lpt2,-1
        sg2(L)=0.
        ENDDO

       DO L=lpt2,2,-1
        sg2(L-1)=sg2(L)+dsg2(L-1)
       ENDDO

        DO L=lpt2-1,1,-1
        sg2(L)=sg2(L)/sg2(1)
        ENDDO
        sg2(1)=1.

       DO L=lpt2-1,1,-1
        dsg2(L)=sg2(L)-sg2(L+1)
        sgml2(l)=(sg2(l)+sg2(l+1))*0.5
       ENDDO

!!      SGML2(LPT2-1)=0.

      ENDIF

      DO L=LM,lpt2,-1
        dsg2(L)=0.
        sgml2(L)=0.
      ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        sg1(LMP1)=0.

      DO L=LMP1,lpt2,-1
       sg1(L-1)=sg1(L)+dsg1(L-1)
      ENDDO

      DO L=LM,lpt2,-1
       sg1(L)=sg1(L)/sg1(lpt2-1)
      ENDDO

        sg1(lpt2-1)=1.

       do l=lpt2-2,1,-1
        sg1(l)=1.
       enddo


      DO L=LM,lpt2,-1
       dsg1(L)=sg1(L)-sg1(L+1)
       sgml1(L)=(sg1(L)+sg1(L+1))*0.5
      ENDDO

      DO L=lpt2-1,1,-1
               dsg1(L)=0.
               sgml1(L)=1.
      ENDDO

 110  format('l,output_levels,psig,sg1,sg2,phyb,phybm')
 111  format(' ',i4,f7.4,f10.2,2f7.4,2f10.2)

      write(*,110)

      do l=1,LMP1
        psig=ETA(L)*(PRF0-pt)+pt
        phyb=sg1(l)*pdtop+sg2(l)*(PRF0-pdtop-pt)+pt
        if(l.lt.LMP1) then
          phybm=sgml1(l)*pdtop+sgml2(l)*(PRF0-pdtop-pt)+pt
        else
          phybm=-99.
        endif

        write(*,111) l,ETA(L),psig
     &         ,sg1(l),sg2(l),phyb,phybm
      enddo


C**** END MAJOR INSERT OF CODE


C
      ALLOCATE(HGT(IMJM))
C      ALLOCATE(hlatb(IMJM))
C      ALLOCATE(hlonb(IMJM))
C      ALLOCATE(hlat(IM,JM))
C      ALLOCATE(hlon(IM,JM))
C
!!!       YC KWON
!!!     READ TOPO DATA FROM SI OUTPUT
!!!     READ 2D DATA AND SHOULD BE TRANSFORMED TO PROPER 1D ARRAY.
!!!
!!!	HGT=0. is an Eta model item
!!!
!!!	NEED TO PULL IN THE NMM GRID TOPOGRAPHY, ASSIGN IT TO
!!!     THE BOUNDARY

!!!     LINK APPROPRIATE STATIC FILE TO UNIT 40
!!!     THIS IS BINARY OUPUT OF WRFSI  (FROM DUSAN)
      call read_geogrid_heights(proj,heights,landmask, !lats,lons, 
     &     'geogrid.out',IM,JM,ierr)
      if(ierr/=0) then
         write(6,*) 'Unable to read heights from geogrid file \"'
     &        // '"geogrid.out".  Error=',ierr
         stop 83
      endif
      call boundary_smooth(heights,landmask,real_nsmth,real_nrow,IM,JM)
      deallocate(landmask)
      write(0,*) 'proj = ',proj
      write(0,*) 'heights(1:10,1) = ',heights(1:10,1)
!     write(0,*) '   lats(1:10,1) = ',lats(1:10,1)
!     write(0,*) '   lons(1:10,1) = ',lons(1:10,1)
      write(0,*) 'heights(1,1:10) = ',heights(1,1:10)
!     write(0,*) '   lats(1,1:10) = ',lats(1,1:10)
!     write(0,*) '   lons(1,1:10) = ',lons(1,1:10)
      deltax=proj%dx
      deltay=proj%dy
C
C      DO 33 I = IM
C      DO 33 J = JM
C      DUM2D(I,J) = data(I,J,18)
C33    CONTINUE
C
C
        write(0,*) 'READ TOPO'
C        WRITE(43,1111) data(:,:,18)
C        READ(40,*) hlon
C        write(6,*) 'READ HLON'
C        WRITE(43,1111) HLON
C	 READ(40,*) hlat
C        write(6,*) 'READ HLAT'
C        WRITE(43,1111) HLAT
1111    FORMAT(1X,10F10.3,1X)

	write(0,*) 'WRF-NMM TOPOGRAPHY'
        do J=JM,1,-JM/20
        write(0,633) (heights(I,J),I=1,IM,IM/10)
	enddo
	!write(0,*) 'WRF-NMM DATA(I,J,19)'
        !do J=JM,1,-JM/20
        !write(0,633) (data(I,J,19),I=1,IM,IM/10)
	!enddo
  633	format(25(f5.0,1x))
C
C.. PUT 2D TOPO DATA TO THE PROPER 1D ARRAY
C                      YC KWON
C
      NCO = 0
      DO J = 1,JM
      IF(MOD(J,2).EQ.0) THEN
      II = IM-1
      ELSE
      II = IM
      ENDIF
      DO I = 1,II
      NCO = NCO + 1
      HGT(NCO) = heights(I,J)
      ENDDO
      ENDDO
C
      write(0,*) 'NCO SHOULD BE SAME AS IMJM  ',NCO, IMJM  
C
!      DO 175 K=1,IMJM
!      write(6,*) 'K, HGT(K)', K, HGT(K)
!  175 CONTINUE
      deallocate(heights)
      !deallocate(lats)
      !deallocate(lons)
C
!      DO 180 K=1,IMJM
!      PD(K)=1.
!     REF(K)=1.
!  180 CONTINUE
C***
C***  FIND PRESSURES ON SIGMA LAYER INTERFACES & MIDPOINTS
C***  FIND HEIGHTS ON SIGMA LAYER INTERFACES
C***
C
      ALLOCATE(PDO(IMJM))
      ALLOCATE(PDO_V1(IMJM))
      ALLOCATE(PDO_2D(IM,JM))
      ALLOCATE(PDO_V(IM,JM))
      ALLOCATE(PMO(LDM))
      ALLOCATE(PIO(LDM+1))
      ALLOCATE(PIO_2D(IMJM,LDM+1))
      ALLOCATE(PIN(LDM+1))
      ALLOCATE(UIN(LDM+1))
      ALLOCATE(VIN(LDM+1))
      ALLOCATE(Y2(LDM+2))
      ALLOCATE(DUM1(LDM+1))
      ALLOCATE(DUM2(LDM+1))

      ALLOCATE(PMID(IMJM,LDM))
      ALLOCATE(DPDIN_IN(IMJM,LDM))
      ALLOCATE(INPUT_TOP(IMJM))
      ALLOCATE(PINT_V1(IMJM,LDM+1))
      ALLOCATE(PINT_3D(IM,JM,LDM+1))
      ALLOCATE(PINT_V(IM,JM,LDM+1))
      ALLOCATE(PMID_V1(IMJM,LDM+1))
      ALLOCATE(PMID_3D(IM,JM,LDM+1))
      ALLOCATE(PMID_V(IM,JM,LDM+1))
      ALLOCATE(H(IMJM,LDM+2))
c     ALLOCATE(H(IMJM,LDM+1))

      PINT_3D = 0.
      PMID_3D = 0.
      PDO_2D = 0.

      write(0,*) 'past alloc block'

       KCHK=9653
      write(0,*) 'K Check point:',KCHK
C
      DO 225 K=1,IMJM
      DO 200 L=1,LDM
      PMID(K,L)=PRES(K,L)
  200 CONTINUE
C
        DO 210 L=1,LDMP
!      PINT(K,L)=SIGINT(L)*PSFC(K)
!
! CHECK PINT AT K = KCHK !! KWON
!
        IF(K.EQ.KCHK) THEN
        write(0,*) '====== CHECK PINT ======'
        write(0,*) '===PINT ',L,PINT(K,L)
        write(0,*) '===PSFC ',PSFC(K)
        ENDIF

!	if (K .le. 10 .and. L .ge. LDM-3) then
!	write(6,*) 'K,L,PINT(K,L): ', K,L,PINT(K,L)
!	endif

  210 CONTINUE
      H(K,LDMP)=ZSFC(K)
c     IF(ZSFC(K) .LT. 0.0) THEN
c       H(K,LDMP) = 0.0
c     ENDIF

  225 CONTINUE

!     DEALLOCATE(ZSFC)            ! IH Kwon
      ALLOCATE(EXNL(IMJM))
C
C        CALL FLIP(PMID,LDM)
C        CALL FLIP(T,LDM)
C        CALL FLIP(PINT,LDM+1)

! -------------------------------------------- IH Kwon
! Calculate H
!
!     DO 250 LL=1,LDMM
!       L=LDMP-LL
!       DO 250 K=1,IMJM
!       IF(L.EQ.LDM)EXNL(K)=(PSFC(K)/P00)**CAPA
!       EXNT=(PINT(K,L)/P00)**CAPA
!        write(6,*) 'T(K,L),PMID(K,L) ',T(K,L),PMID(K,L)
!        write(6,*) 'EXNT,EXNL(K),CPOG ',EXNT,EXNL(K),CPOG
!       H(K,L)=H(K,L+1)-CPOG*T(K,L)*(EXNT-EXNL(K))
!    1       *(P00/PMID(K,L))**CAPA

!	if (K .le. 10 .and. L .ge. LDM-3) then
!	write(6,*) 'K,L,H(K,L),PMID,T: ', 
!     &        K,L,H(K,L),PMID(K,L),T(K,L)
!	endif
!
! CHECK H AT K = KCHK !! KWON
!
!       IF(K.EQ.KCHK) THEN
!       write(0,*) '====== CHECK H ======'
!       write(0,*) '===H,LL,L(REVERSE) ',LL,L,H(K,L)
!       ENDIF

C
!       IF(K.EQ.5) THEN
!         write(6,251)K,L,T(K,L),H(K,L),H(K,L+1),
!     1     PINT(K,L),PMID(K,L),EXNL(K),EXNT,PSFC(K),ZSFC(K)
!251     FORMAT(2X,2I3,9(E12.5,1X))
!       ENDIF
C
!       EXNL(K)=EXNT

! 250 CONTINUE

      DO LL=1,LDM
         L=LDMP-LL
        DO K=1,IMJM
           H(K,L)=H(K,L+1)
     &          + R*T(K,L) *log(PINT(K,L+1)/PINT(K,L)) /G
        ENDDO
      ENDDO

        write(0,*) '====== CHECK H ======'
      DO LL=1,LDM
         L=LDMP-LL
        write(0,*) '===LL,L,H(REVERSE) ',LL,L,H(KCHK,L)
      ENDDO
! --------------------------------------------


! WRITE PINT & H   !TODO

      ALLOCATE(Z_OUT(IMJM,LM+1))
      ALLOCATE(T_OUT(IMJM,LM))
      ALLOCATE(Q_OUT(IMJM,LM))
      ALLOCATE(CWM_OUT(IMJM,LM))
      ALLOCATE(U_OUT(IMJM,LM))
      ALLOCATE(V_OUT(IMJM,LM))

      do K=1,IMJM

!!!   AT THIS POINT SHOULD BE ABLE TO COMPUTE SURFACE PRESSURE

        if (k.eq.KCHK) then
          write(0,*)' hgt test 1 ',k,ldmp,HGT(K),H(K,LDMP)
        endif

	IF ( HGT(K) .lt. H(K,LDMP) ) THEN    ! extrapolate downward

! ------------------------------------- IH Kwon
!                         Using a lapse rate instead of extrapolation

! 	  DLNPDZ=( LOG(PINT(K,LDMP))-LOG(PINT(K,LDMP-1)) ) / 
!    &          ( H(K,LDMP)-H(K,LDMP-1))
!       PSFC_OUT(K)=EXP( LOG(PINT(K,LDMP))+DLNPDZ*(HGT(K)-H(K,LDMP)))

!         if (k .eq. KCHK) then
!         write(0,*) ' '
!         write(0,*) 'K, PINT vals for DLNPDZ: ', K, PINT(K,LDMP),
!    &    PINT(K,LDMP-1)
!         write(0,*) 'K, H vals for DLNPDZ: ', K, H(K,LDMP),
!    &    H(K,LDMP-1)
!         write(0,*) 'LDMP, DLNPDZ, HGT: ', LDMP, DLNPDZ, HGT(K)
!         write(0,*) '(a)K, PSFC_OUT: ', K, PSFC_OUT(K)
!         endif

        TMEAN= T(K,LDM) +0.5*GAMMA*( H(K,LDM )-HGT(K) )

        DLNPDZ=(H(K,LDMP)-HGT(K)) *G /(R*TMEAN)
        PSFC_OUT(K)=PINT(K,LDMP) *EXP(DLNPDZ)

          if (k .eq. KCHK) then
             write(6,*) 'HGT(K).lt.H(K,LDMP): PSFC_OUT from lapse rate'
             write(6,*)  HGT(K),H(K,LDMP)
             write(6,*)  'T(K,LDM),TMEAN,H(K,LDM)-HGT(K),PSFC_OUT(K)'
             write(6,*)   T(K,LDM),TMEAN,H(K,LDM)-HGT(K),PSFC_OUT(K)
          endif

! -------------------------------------

        ELSE     ! target level bounded by input levels

        DO L=LDMP,1,-1
C
          if(k.eq.KCHK) then
            write(0,*)' hgt test #2 ',k,l,ldmp,HGT(K),H(K,L),
     &        HGT(K),H(K,L-1)
          endif
C           IF (HGT(K) .gt. H(K,L) .and. 
C    &                             HGT(K) .lt. H(K,L-1)) then
	    IF (HGT(K) .ge. H(K,L) .and. 
     &                             HGT(K) .le. H(K,L-1)) then
              DLNPDZ=(LOG(PINT(K,L))-LOG(PINT(K,L-1))) /
     &                 (H(K,L)-H(K,L-1))

           PSFC_OUT(K)=EXP( LOG(PINT(K,L))+DLNPDZ*(HGT(K)-H(K,L)))

	if (K .eq. KCHK) then
	write(0,*) ' '
	write(0,*) 'K, PINT vals for DLNPDZ: ', K, PINT(K,L),
     &      PINT(K,L-1)
	write(0,*) 'K, H vals for DLNPDZ: ', K, H(K,L),
     &      H(K,L-1)
	write(0,*) 'L,  DLNPDZ, HGT: ', L, DLNPDZ,HGT(K) 
	write(0,*) '(b)K, PSFC_OUT: ', K, PSFC_OUT(K)
	endif

	goto 979
            ENDIF
          ENDDO

        ENDIF

  979	continue

	PDO(K)=PSFC_OUT(K)-pdtop-pt
	
!	write(0,*) 'K, PDO(K): ', K, PDO(K)

!---------now output pressure levels can be defined---------------------

        DO L=1,LM
	  PMO(L)=SGML2(L)*PDO(K)+SGML1(L)*PDTOP+PT
!	write(0,*) 'L, PMO(L): ', L, PMO(L)
        ENDDO
  
        DO L=1,LDM+2
	  Y2(L)=0.
        ENDDO

        DO L=1,LM+1
          PIO(L)=SG2(L)*PDO(K)+SG1(L)*PDTOP+PT
          PIO_2D(K,L)=PIO(L)
!	write(6,*) 'L, PIO(L): ', L, PIO(L)
	ENDDO

        IF(K.EQ.KCHK) THEN
        DO L=1,LDM
	write(0,*) 'L,H,PINT,PMID:',L,H(K,L),PINT(K,L),PMID(K,L)
	ENDDO
        L=LDM+1
	write(0,*) 'L,H,PINT,PMID:',L,H(K,L),PINT(K,L)
        ENDIF

! ------------------------------------- IH Kwon
!          VIRTUAL TEMPERATURE IS INTERPOLATED (NOT CALCULATED FROM H)

!	IF ( (H(K,LDMP)-HGT(K)) .gt. 0.5) THEN

!       basically use computed surface pressure as another data point

!       H(K,LDM+2)=HGT(K)
!       PINT(K,LDM+2)=pio(1)

! 	write(0,*) 'BONUS LEVEL for K: ', K
!
!mp        call SPLINE2(1,1,LDMP,PINT,H,y2,LM+1, 
!       call SPLINE2(K,LDM+1,PINT(K,2:LDM+2),H(K,2:LDM+2),
!    &                y2(2:LDMP),LM+1, 
!    &                pio,Z_OUT(K,:),dum1,dum2 )

!       ELSE
! 	write(0,*) 'no bonus LEVEL for K: ', K

!       call SPLINE2(K,LDM,PINT(K,2:LDMP),H(K,2:LDMP),
!    &                y2(2:LDMP),LM+1, 
!    &                pio,Z_OUT(K,:),dum1,dum2 )

!       ENDIF

!       write(0,*) 'PMID(k,:),T(K,;)'
!     do l=1,LDM
!       write(0,*) l,PMID(k,l),T(K,l)
!     enddo
!       write(0,*) 'PMO,T_OUT'
!     do l=1,LM
!       write(0,*) l,PMO(l),T_OUT(K,l)
!     enddo
        
!     if (K .eq. KCHK) then
!     do L=1,LM+1
!     write(0,*) 'L,PIO(L),Z_OUT(K,L): ', L,PIO(L),Z_OUT(K,L)
!     enddo
!     endif

!!!     VIRTUAL TEMPERATURE FOR THE TIME BEING

!       do L=1,LM
!        T_OUT(K,L)=-(g/R)* 
!     &     (z_out(K,L)-z_out(K,L+1))/(log(pio(L))-log(pio(L+1)))
!        if(k.eq.KCHK) then
!          write(6,*) 'tout ',k,l,z_out(K,L),z_out(K,L+1),pio(L),
!     &       pio(L+1),log(pio(L)),log(pio(L+1)),t_out(k,l)
!        endif
!	enddo
         
! ------------------------------------- 

! FOR CHECKING VERTICAL INTERPOLATION AT (100,5), KB=KCHK
!                        KWON
!        if(k.eq.KCHK) then
!        print *,'==== T Z PIO AT K=KCHK (100,5) ===='
!        do LL = 1,LM
!        print *,'===T ',LL,T_OUT(K,LL)
!        print *,'===TOPO ',HGT(K)
!        print *,'===Z1 Z2 ',z_out(K,LL),z_out(K,LL+1)
!        print *,'===PIO1 PIO2 ',pio(LL),PIO(LL+1)
!        print *,'==================================='
!        enddo
!        endif
!
!        if(k.ge.147.and.k.le.149) then
!        print *,'==== T Z PIO AT K=147 TO 149  ===='
!        do LL = 1,LM
!        print *,'===T ',K,LL,T_OUT(K,LL)
!        print *,'===TOPO ',HGT(K)
!        print *,'===Z1 Z2 ',z_out(K,LL),z_out(K,LL+1)
!        print *,'===PIO1 PIO2 ',pio(LL),PIO(LL+1)
!        print *,'==================================='
!        enddo
!        endif

        call SPLINE2(K,LDM,PMID(K,1:LDM),T(K,1:LDM),
     &                y2(1:LDM),LM,
     &                PMO,T_OUT(K,:),dum1,dum2 )

!                 T & T_OUT: Virtual temperature

      do l=1,LM
        if(PMO(l) > PMID(k,LDM))then
           T_OUT(K,l)= T(K,LDM) *( PMO(l)/PMID(k,LDM) )**RGOG
        endif
      enddo

      Z_OUT(K,1)=HGT(K)

      do l=2,LM+1
           Z_OUT(K,l)=Z_OUT(k,l-1)
     &               + R*T_OUT(K,l-1)*log( pio(l-1)/pio(l) )/G
      enddo

      if (k .eq. KCHK) then
        write(0,*) 'K= ', K
        DO L=1,3
          write(0,*) 'L, Z_OUT, TV_OUT: ', L, Z_OUT(K,L), T_OUT(K,L)
        enddo
      endif

      enddo  !!!! END LOOP OVER BOUNDARY POINTS K

! ------------------------------------- IH Kwon

      write(0,*) 'printout sig2hyb_output'
      open(77,file='sig2hyb_output',
     &     form='unformatted')
      write(77) H,zsfc,hgt,psfc_out,z_out,t_out

! -------------------------------------

	write(0,*) 'done with loop, dealloc EXNL, T'

      DEALLOCATE(EXNL)

!     DEALLOCATE(T)               !! IH Kwon
	write(6,*) 'past dealloc EXNL, T'

C***
C***  VERTICAL INTERPOLATION OF HEIGHTS AND SPECFIC HUMIDITY
C***  QUADRATICALLY IN LN P AND OF WINDS LINEARLY IN LN P.
C***
C
	write(0,*) 'SIG_INIT allocated H? ', allocated(H)
      DEALLOCATE(H)
	write(0,*) 'SIG_INIT past deallocate of EXNL, T, H'
C
C***
C
C.. CALCULATE PDO & PINT AT V POINTS USING AVERAGE
C                       YC KWON
C.. FIRST, MAKE PDO AND PINT TO 2D AND 3D VARIABLES, RESPECTIVELY
C                       IH KWON
C.. FIX A BUG and ADD PMID_3D, PMID_V and PMID_V1
C
C
      NN = 0
      DO J = 1,JM
      IF(MOD(J,2).EQ.1) THEN
      II = IM
      ELSE
      II = IM-1
      ENDIF
      DO I = 1,II
      NN = NN+1
      PDO_2D(I,J) = PDO(NN)
C
      DO K = 1,LDM+1
      PINT_3D(I,J,K) = PINT(NN,K)
      ENDDO
C
      DO K = 1,LDM
      PMID_3D(I,J,K) = PMID(NN,K)
      ENDDO
C
      ENDDO     !I-LOOP
      ENDDO     !J-LOOP
C
      write(0,*) 'SIG_INIT 2D TO 3D'
C
C.. SECOND, GENERATING PDO & PINIT OVER V-POINTS
C
C.. INTERIOR
C
      DO J = 2,JM-1
      write(0,*) 'J  ',J
      DO I = 2,IM-1
      IF(MOD(J,2).EQ.0) THEN
      PDO_V(I,J) = (PDO_2D(I-1,J)+PDO_2D(I,J+1)+
     &              PDO_2D(I,J)+PDO_2D(I,J-1))*0.25
      ELSE
      PDO_V(I,J) = (PDO_2D(I+1,J)+PDO_2D(I,J+1)+
     &              PDO_2D(I,J)+PDO_2D(I,J-1))*0.25
      ENDIF
C
      DO K = 1,LDM+1
      IF(MOD(J,2).EQ.0) THEN
      PINT_V(I,J,K)=(PINT_3D(I-1,J,K)+PINT_3D(I,J+1,K)+
     &               PINT_3D(I,J,K)+PINT_3D(I,J-1,K))*0.25
      ELSE
      PINT_V(I,J,K)=(PINT_3D(I+1,J,K)+PINT_3D(I,J+1,K)+
     &               PINT_3D(I,J,K)+PINT_3D(I,J-1,K))*0.25
      ENDIF
      ENDDO
C
      DO K = 1,LDM
      IF(MOD(J,2).EQ.0) THEN
      PMID_V(I,J,K)=(PMID_3D(I-1,J,K)+PMID_3D(I,J+1,K)+
     &               PMID_3D(I,J,K)+PMID_3D(I,J-1,K))*0.25
      ELSE
      PMID_V(I,J,K)=(PMID_3D(I+1,J,K)+PMID_3D(I,J+1,K)+
     &               PMID_3D(I,J,K)+PMID_3D(I,J-1,K))*0.25
      ENDIF
      ENDDO
C
      ENDDO
      ENDDO
      write(0,*) 'SIG_INIT INTERIOR PD'
C
C.. ON THE BOUNDARIES
C
C.. WEST
      DO J=2,JM-1
! ------------------------------------------------- IH Kwon
!     PDO_V(1,J) = (PDO_2D(1,J-1)+PDO_2D(1,J+1))/2.
      I=1
      IF(MOD(J,2).EQ.0) THEN

      PDO_V(I,J) = (PDO_2D(I,J-I)+PDO_2D(I,J+1))/2.
      DO K=1,LDM+1
      PINT_V(I,J,K)=(PINT_3D(I,J-1,K)+PINT_3D(I,J+1,K))/2.
      ENDDO
      DO K=1,LDM
      PMID_V(I,J,K)=(PMID_3D(I,J-1,K)+PMID_3D(I,J+1,K))/2.
      ENDDO

      ELSE
      PDO_V(I,J) = (PDO_2D(I+1,J)+PDO_2D(I,J+1)+
     &              PDO_2D(I,J)+PDO_2D(I,J-1))*0.25
      DO K=1,LDM+1
      PINT_V(I,J,K)= (PINT_3D(I+1,J,K)+PINT_3D(I,J+1,K)+
     &                PINT_3D(I,J,K)+PINT_3D(I,J-1,K))*0.25
      ENDDO
      DO K=1,LDM
      PMID_V(I,J,K)= (PMID_3D(I+1,J,K)+PMID_3D(I,J+1,K)+
     &                PMID_3D(I,J,K)+PMID_3D(I,J-1,K))*0.25
      ENDDO

      ENDIF
! -------------------------------------------------
      ENDDO
      write(0,*) 'SIG_INIT WEST PD'
C
C.. EAST
      DO J=2,JM-1
      I=IM
!     PDO_V(I,J) = (PDO_2D(I,J-1)+PDO_2D(I-1,J)+PDO_2D(I,J+1))/3.
      PDO_V(I,J) = (PDO_2D(I,J-1)+PDO_2D(I,J+1))/2.
      DO K=1,LDM+1
!     PINT_V(I,J,K)=(PINT_3D(I,J-1,K)+PINT_3D(I,J+1,K)+
!    &               PINT_3D(I-1,J,K))/3.
      PINT_V(I,J,K)=(PINT_3D(I,J-1,K)+PINT_3D(I,J+1,K))/2.
      ENDDO
      DO K=1,LDM
!     PMID_V(I,J,K)=(PMID_3D(I,J-1,K)+PMID_3D(I,J+1,K)+
!    &               PMID_3D(I-1,J,K))/3.
      PMID_V(I,J,K)=(PMID_3D(I,J-1,K)+PMID_3D(I,J+1,K))/2.
      ENDDO
      ENDDO
      write(0,*) 'SIG_INIT EAST PD'
C
C.. NORTH
      DO I=1,IM-1
      PDO_V(I,JM) = (PDO_2D(I,JM)+PDO_2D(I+1,JM))/2.
      DO K=1,LDM+1
      PINT_V(I,JM,K)=(PINT_3D(I,JM,K)+
     &                PINT_3D(I+1,JM,K))/2.
      ENDDO
      DO K=1,LDM
      PMID_V(I,JM,K)=(PMID_3D(I,JM,K)+
     &                PMID_3D(I+1,JM,K))/2.
      ENDDO
      ENDDO
      write(0,*) 'SIG_INIT NORTH PD'
C
C.. SOUTH
      DO I=1,IM-1
      PDO_V(I,1) = (PDO_2D(I,1)+PDO_2D(I+1,1))/2.
      DO K=1,LDM+1
      PINT_V(I,1,K)=(PINT_3D(I,1,K)+
     &                PINT_3D(I+1,1,K))/2.
      ENDDO
      DO K=1,LDM
      PMID_V(I,1,K)=(PMID_3D(I,1,K)+
     &                PMID_3D(I+1,1,K))/2.
      ENDDO
      ENDDO
      write(0,*) 'SIG_INIT SOUTH PD'
C
C.. REDUCE THE HORIZONTAL DIMENSION TO 1D
C

!... Verification
      write(0,*) 'H(214,3) V(215,3) H(215,3) V(215,3)'
      write(0,*) 'V(214,2) H(215,2) V(215,2) H(215,2)'
      write(0,*) 'H(214,1) V(215,1) H(215,1) V(215,1)'
      write(0,*) '---PD------------------------------'
      write(0,*) PDO_2D(IM-1,3),PDO_V(IM-1,3),PDO_2D(IM,3),PDO_V(IM,3)
      write(0,*) PDO_V(IM-1,2),PDO_2D(IM-1,2),PDO_V(IM,2),PDO_2D(IM,2)
      write(0,*) PDO_2D(IM-1,1),PDO_V(IM-1,1),PDO_2D(IM,1),PDO_V(IM,1)
      write(0,*) '---PMID----------------------------'
      write(0,*)
     & PMID_3D(IM-1,3,1),PMID_V(IM-1,3,1),PMID_3D(IM,3,1),PMID_V(IM,3,1)
      write(0,*)
     & PMID_V(IM-1,2,1),PMID_3D(IM-1,2,1),PMID_V(IM,2,1),PMID_3D(IM,2,1)
      write(0,*)
     & PMID_3D(IM-1,1,1),PMID_V(IM-1,1,1),PMID_3D(IM,1,1),PMID_V(IM,1,1)

      NN = 0
      DO J = 1,JM
! ------------------------------------------------- IH Kwon
!     IF(MOD(J,2).EQ.1) THEN
!     II = IM
!     ELSE
!     II = IM-1
!     ENDIF
      IF(MOD(J,2).EQ.1) THEN
      II = IM-1
      ELSE
      II = IM
      ENDIF
! -------------------------------------------------
      DO I = 1,II
      NN = NN + 1
      PDO_V1(NN) = PDO_V(I,J)
      DO K = 1,LDM+1
      PINT_V1(NN,K) = PINT_V(I,J,K)
      ENDDO
      DO K = 1,LDM
      PMID_V1(NN,K) = PMID_V(I,J,K)
      ENDDO
C
      ENDDO
      ENDDO
C
C.. END OF MODIFICATION (INSERT) BY YC KWON
      write(0,*) 'SIG_INIT END OF MODIFICATION'
C
      DO K=1,IMJM
         if(k.eq.KCHK) print *,'in 600 ',k,lm,t_out(k,lm)
C      PDVP=0.5*(PDO(K1K)+PDO(K2K))
         PDVP = PDO_V1(K)       !KWON

!     flux 
         INPUT_TOP(K)=1
         
         DO L=1,LDM
C        AVG1=0.5*(PINT(K1K,L)+PINT(K2K,L))
            AVG1 = PINT_V1(K,L) !KWON
            if (AVG1 .lt. PT) INPUT_TOP(K)=L ! if above top, keep incrementing
C     AVG2=0.5*(PINT(K1K,L+1)+PINT(K2K,L+1))
            AVG2 = PINT_V1(K,L+1)
            DPDIN_IN(K,L)=AVG2-AVG1
         ENDDO
!flux

C
C***
C***  SPLINE COMPUTATION OF WINDS
C***
         DO  L=1,LM
            PMO(L)=SGML2(L)*PDVP+SGML1(L)*PDTOP+PT
         enddo

!  Generate 1-D arrays of P, U, and V for splines

         DO L=1,LDM
!           PIN(L)=PMID(K,L)
            PIN(L)=PMID_V1(K,L)               ! IH Kwon
            UIN(L)=U(K,L)
            VIN(L)=V(K,L)
         ENDDO

!       EXTEND AT UPPER BOUNDARY

         IF (PIN(1) .gt. PMO(LM)) then
            D1=(PMO(LM)-PIN(1))/(PIN(1)-PIN(2))
            UIN(1)=UIN(1)+D1*(UIN(1)-UIN(2))
            VIN(1)=VIN(1)+D1*(VIN(1)-VIN(2))
            PIN(1)=PMO(LM)
            write(0,*) 'Above the top of reference levels',K
         ENDIF

!        IF (PIN(LDM) .LT. PMO(1)) THEN
!           
!           write(0,*) 'SIG_INIT adding another level, PMID, PMO: ', 
!    &           K, PMID(K,LDM),PMO(1)
!           
!           PIN(LDM+1)=PMO(1)
!           UIN(LDM+1)=UIN(LDM)
!           VIN(LDM+1)=VIN(LDM)
!           if (K .eq. KCHK) then
!              do L=1,LDM+1
!                 write(0,*) 'SIG_INIT L,PIN,UIN,VIN: ', L,PIN(L),
!    &                 UIN(L),VIN(L)
!              enddo
!           endif

!           CALL SPLINE2(K,LDM+1,PIN,UIN,Y2,LM,PMO,
!    &           U_OUT(K,:),DUM1,DUM2)
!           CALL SPLINE2(K,LDM+1,PIN,VIN,Y2,LM,PMO,
!    &           V_OUT(K,:),DUM1,DUM2)
!           
!        ELSE
!     write(0,*) 'SIG_INIT NOT adding another level, PMID, PMO: ', 
!     &                  K, PMID(K,LDM),PMO(1)
            
            if (K .eq. KCHK) then
               do L=1,LDM
                  write(0,*) 'SIG_INIT L,PIN,UIN,VIN: ', L,PIN(L),
     &                 UIN(L),VIN(L)
               enddo
            endif
            
            CALL SPLINE2(K,LDM,PIN(1:LDM),UIN(1:LDM),Y2,LM,PMO,
     &           U_OUT(K,:),DUM1,DUM2)
            CALL SPLINE2(K,LDM,PIN(1:LDM),VIN(1:LDM),Y2,LM,PMO,
     &           V_OUT(K,:),DUM1,DUM2)

!        ENDIF
         
         if (K .eq. KCHK) then
            
            do L=1,LM
               write(0,*) 'SIG_INIT L,PMO, U_OUT(K,L),V_OUT(K,L): ', 
     &              L,PMO(L), U_OUT(K,L),V_OUT(K,L),
     &              T_OUT(K,L)
            enddo
            
         endif
         
      enddo
      
      write(0,*) 'SIG_INIT END OF LOOP 600'
      
        goto 997        ! IH Kwon: SKIP this Flux adjustment
!!!!  Flux adjustment/comparison

        do K=1,IMJM

        sfluxui=0.
        sfluxvi=0.

        dpdinsum=0.

        do L=INPUT_TOP(K),LDM
        sfluxui=sfluxui+u(K,L)*dpdin_in(K,L)
        sfluxvi=sfluxvi+v(K,L)*dpdin_in(K,L)
        dpdinsum=dpdinsum+dpdin_in(K,L)
        enddo

        sfluxuo=0.
        sfluxvo=0.
        dpdosum=0.

        do L=1,LM
        dpdo=dsg2(L)*PDO(K)+dsg1(L)*PDTOP
        sfluxuo=sfluxuo+u_out(K,L)*dpdo
        sfluxvo=sfluxvo+v_out(K,L)*dpdo
        dpdosum=dpdosum+dpdo
        enddo


	if (sfluxui/sfluxuo .lt. 0.5 .or. sfluxui/sfluxuo .gt. 2.0 .or.
     &   sfluxvi/sfluxvo .lt. 0.5 .or. sfluxvi/sfluxvo .gt. 2.0 ) then
        write(0,*) 'SIG_INIT K, INPUT_TOP, dpdinsum, dpdosum: ', K, 
     &                 INPUT_TOP(K),dpdinsum,dpdosum
        write(0,*) 'SIG_INIT K, ui/uo,vi/vo: ',
     &            K,sfluxui/sfluxuo,sfluxvi/sfluxvo
	write(0,*) 'SIG_INIT sfluxui, sfluxuo: ', sfluxui, sfluxuo
	write(0,*) 'SIG_INIT sfluxvi, sfluxvo: ', sfluxvi, sfluxvo
	write(0,*) ' ------------- '

	

!	do L=INPUT_TOP(K),LDM
!	write(6,*) 'PMID,U,V inputs: ', PMID(K,L),U(K,L),V(K,L)
!	enddo

        endif

        rdpdo=1./dpdosum

        if (abs((sfluxui-sfluxuo)*rdpdo) .gt. 0.3) then
        write(0,*) 'SIG_INIT big U change..',
     &    K,abs((sfluxui-sfluxuo)*rdpdo)
        endif
        if (abs((sfluxvi-sfluxvo)*rdpdo) .gt. 0.3) then
        write(0,*) 'SIG_INIT big V change..',
     &      K,abs((sfluxvi-sfluxvo)*rdpdo)
        endif

        do L=1,LM

	  PMO(L)=SGML2(L)*PDO(K)+SGML1(L)*PDTOP+PT

	if (sfluxui/sfluxuo .lt. 0.5 .or. sfluxui/sfluxuo .gt. 2.0 .or.
     &   sfluxvi/sfluxvo .lt. 0.5 .or. sfluxvi/sfluxvo .gt. 2.0 ) then
!	write(0,*)'PMO, u_out,v_out (prev): ',PMO(L),
!     &     u_out(K,L),v_out(K,L)
	endif

        u_out(K,L)=u_out(K,L)+(sfluxui-sfluxuo)*rdpdo
        v_out(K,L)=v_out(K,L)+(sfluxvi-sfluxvo)*rdpdo

	if (sfluxui/sfluxuo .lt. 0.5 .or. sfluxui/sfluxuo .gt. 2.0 .or.
     &   sfluxvi/sfluxvo .lt. 0.5 .or. sfluxvi/sfluxvo .gt. 2.0 ) then
!	write(6,*)'PMO, u_out,v_out (post): ',
!     &    PMO(L),u_out(K,L),v_out(K,L)
	endif

        enddo

!!!! double check

        sfluxui=0.
        sfluxvi=0.

        do L=INPUT_TOP(K),LDM
        sfluxui=sfluxui+u(K,L)*dpdin_in(K,L)
        sfluxvi=sfluxvi+v(K,L)*dpdin_in(K,L)
        enddo

        sfluxuo=0.
        sfluxvo=0.
        dpdosum=0.

        do L=1,LM
        dpdo=dsg2(L)*PDO(K)+dsg1(L)*PDTOP
        sfluxuo=sfluxuo+u_out(K,L)*dpdo
        sfluxvo=sfluxvo+v_out(K,L)*dpdo
        dpdosum=dpdosum+dpdo
        enddo

        if (sfluxui/sfluxuo.gt.1.002.or.sfluxui/sfluxuo.lt.0.998 
     & .or. sfluxvi/sfluxvo.gt.1.002.or.sfluxvi/sfluxvo.lt.0.998) then
        write(0,*) 'bad REV.. K, ui/uo,vi/vo: ',K, 
     &           sfluxui/sfluxuo,sfluxvi/sfluxvo
        endif

        end do   ! K points

  997 continue

!!!! end flux adjustment/comparison
C
C
      DEALLOCATE(INPUT_TOP)
	write(0,*) 'past spline, deallocate of U,V'
C
C     INTERPOLATE Q LINEARLY IN LN(P) FROM SIGMA TO HYBRID
C

         cmax=0.

	ALLOCATE(TMPVRT(LDM))
      DO 700 K=1,IMJM

!	write(6,*) 'work Q interpolation, K', K


!	flip PMID so it is consistent with the Q order

	
!       DO L=1,LDM 
!	TMPVRT(L)=PMID(K,L)
!       ENDDO

!       DO L=1,LDM 
!	PMID(K,L)=TMPVRT(LDM-L+1)
!       ENDDO

       DO LD=1,LDM-1 
        ALPR(LD)=ALOG(PMID(K,LD)/PMID(K,LD+1))
       ENDDO

       DO LD=1,LDM 
        ALP(LD)=ALOG(PMID(K,LD))
	if (K .eq. KCHK) then
	write(0,*) 'LD,PMID, Q): ', LD,PMID(1,LD), Q(1,LD)
	endif
       ENDDO

!       integrating from TOA downward...

           cint= 0.01

        DO L=LM,1,-1
          ALPOUT=LOG(PT+SGML1(L)*PDTOP+SGML2(L)*PDO(K))
          PMIDO=PT+SGML1(L)*PDTOP+SGML2(L)*PDO(K)
          Q_OUT(K,L)=-9999.

          IF (PMIDO .gt. PMID(K,LDM)) THEN  ! extrap downward

! ----------------------------------- IH Kwon
!                        Calculation from RH & Tv instead of extrapolation

!           DLNQDLNP= (log(Q(K,LDM))-log(Q(K,LDM-1))) / 
!    &                (log(PMID(K,LDM))-log(PMID(K,LDM-1)))
!           Q_OUT(K,L)=  EXP(log(Q(K,LDM)) + DLNQDLNP * 
!    &                (log(PMIDO)-log(PMID(K,LDM))) )

            diff=PMIDO-PMID(K,LDM)
            if(diff>cmax)then
               cmax=diff
               maxposi=K
            endif
            
            tbot= T(K,LDM)/(1.+ a1*Q(K,LDM))
            qsatbot= pq0/PMID(K,LDM) *exp(a2*(tbot-a3)/(tbot-a4))
            rhbot= Q(K,LDM)/qsatbot
           if(rhbot > 1.) rhbot=1.

              tl= T_OUT(K,L)
              rh0= 0.
           do while(rh0 <= rhbot)
              tmp1= (T_OUT(K,L)-tl) /(a1*tl)
              tmp2= pq0 /PMIDO *exp( a2*(tl-a3)/(tl-a4) )
              rh0 = tmp1 /tmp2
              tl= tl -cint
           enddo
            Q_OUT(K,L)= (T_OUT(K,L)-tl) /(a1*tl)

           if (K .eq. KCHK)then
              write(6,*) 'PMIDO .gt. PMID(K,LDM): Q_OUT from RH & Tv'
              write(6,*)  PMIDO,PMID(K,LDM)
              write(6,*) 'T(K,LDM),Q(K,LDM),rhbot'
              write(6,*)  T(K,LDM),Q(K,LDM),rhbot
              write(6,*) 'L,T_OUT(K,L),Q_OUT(K,L),rh'
              write(6,*)  L,T_OUT(K,L),Q_OUT(K,L),rh0
           endif

! -----------------------------------

          ELSEIF (PMIDO .lt. PMID(K,1)) THEN ! extrap up
	    if (K .eq. KCHK) 
     &        write(0,*)'L, extrap up, PMID(K,1): ',L,PMID(K,LDM)

           DLNQDLNP= (log(Q(K,2))- log(Q(K,1))) /
     &               ( log(PMID(K,1))- log(PMID(K,1)) )

           Q_OUT(K,L)= exp( log(Q(K,1)) + DLNQDLNP *
     &                    ( log(PMIDO)-log(PMID(K,1))) )

          ELSE ! normal case

	    DO LD=LDM-1,1,-1

	LL=LDMP-LD

        IF (PMIDO .GE. PMID(K,LD) .and. PMIDO .LE. PMID(K,LD+1)) THEN
            QLD=AMAX1(Q(K,LD+1),1.E-10)
            CF=(ALPOUT-ALP(LD+1))/ALPR(LD)
            Q_OUT(K,L)=EXP(ALOG(QLD)+(ALOG(Q(K,LD))-ALOG(QLD))*CF)

	if (K .eq. KCHK) then
	write(0,*) 'L, Q(LD+1),Q(LD), Q_OUT: ', 
     &                         L, QLD, Q(K,LD), Q_OUT(K,L)
	endif

        ENDIF
            
            END DO
          ENDIF

         denom=T_OUT(K,L)-CM2*ALOG10(T_OUT(K,L))+CM3
         if(k.eq.KCHK) then
C         write(6,1001)k,l,denom,T_OUT(K,L),ALOG10(T_OUT(K,L)),
C     1    hlatb(k),hlonb(k),hgt(k)
1001     format(1x,2i5,6(1x,e12.5))
         endif
         CLOGES=-CM1/T_OUT(K,L)-CM2*ALOG10(T_OUT(K,L))+CM3
         ESE=10.**(CLOGES+2.)
         QSX=EPS*ESE/(PMIDO-ESE*(1.-EPS))
         if(QSX.LE.1.E-10) then
         Q_OUT(K,L)=AMAX1(Q_OUT(K,L),1.E-10)
         else
         QSMX=0.98*QSX
         QTMP=AMAX1(Q_OUT(K,L),1.E-10)
         Q_OUT(K,L)=AMIN1(QTMP,QSMX)
         endif

       END DO  ! L loop

!!!!!!!!!!!!!!!!!!!!!!!!!!1


!       integrating from TOA downward for CWM

        DO L=LM,1,-1
          ALPOUT=LOG(PT+SGML1(L)*PDTOP+SGML2(L)*PDO(K))
          PMIDO=PT+SGML1(L)*PDTOP+SGML2(L)*PDO(K)
          CWM_OUT(K,L)=-9999.

            CWM(K,LDM)=AMAX1(CWM(K,LDM),1.E-10)
            CWM(K,LDM-1)=AMAX1(CWM(K,LDM-1),1.E-10)

          IF (PMIDO .gt. PMID(K,LDM)) THEN  ! extrap downward
	if (K .eq. KCHK) write(0,*) 'L, extrap down, PMID(K,LDM): '
     &                   ,L,PMID(K,LDM)
            DLNQDLNP= (log(CWM(K,LDM))-log(CWM(K,LDM-1))) / 
     &                (log(PMID(K,LDM))-log(PMID(K,LDM-1)))

!Kwon's doing

      INEXP=log(CWM(K,LDM))+DLNQDLNP*(log(PMIDO)-log(PMID(K,LDM)))

      IF(INEXP.GT.0.) THEN
        IF(L.EQ.LM) THEN
          CWM_OUT(K,L)=0.
        ELSE
          CWM_OUT(K,L)=CWM_OUT(K,L+1)
        ENDIF
      ELSE
        CWM_OUT(K,L)=  EXP(log(CWM(K,LDM)) + DLNQDLNP *
     &                 (log(PMIDO)-log(PMID(K,LDM))) )
      ENDIF
!END OF Kwon's doing


	if ( abs(CWM_OUT(K,L)) .lt. 1.e-1) then
	else
	write(0,*) 'bad extrapdown cwm_out: ', K,L,cwm_out(K,L)
	write(0,*) 'CWM,DLNQDLNP: ', CWM(K,LDM), DLNQDLNP
	endif

          ELSEIF (PMIDO .lt. PMID(K,1)) THEN ! extrap up
	    if (K .eq. KCHK) 
     &        write(0,*)'L, extrap up, PMID(K,1): ',L,PMID(K,LDM)

            CWM(K,1)=AMAX1(CWM(K,1),1.E-10)
            CWM(K,2)=AMAX1(CWM(K,2),1.E-10)

           DLNQDLNP= (log(CWM(K,2))- log(CWM(K,1))) /
     &               ( log(PMID(K,1))- log(PMID(K,1)) )

           CWM_OUT(K,L)= exp( log(CWM(K,1)) + DLNQDLNP *
     &                    ( log(PMIDO)-log(PMID(K,1))) )

	if ( abs(CWM_OUT(K,L)) .lt. 1.e-1) then
	else
	write(0,*) 'bad extrap_up cwm_out: ', K,L,cwm_out(K,L)
	write(0,*) 'CWM,DLNQDLNP: ', CWM(K,1), DLNQDLNP
	endif

          ELSE ! normal case

	    DO LD=LDM-1,1,-1

	LL=LDMP-LD

        IF (PMIDO .GE. PMID(K,LD) .and. PMIDO .LE. PMID(K,LD+1)) THEN
            QLD=AMAX1(CWM(K,LD+1),1.E-10)
            CWM(K,LD)=AMAX1(CWM(K,LD),1.e-10)
            CF=(ALPOUT-ALP(LD+1))/ALPR(LD)
            CWM_OUT(K,L)=EXP(ALOG(QLD)+(ALOG(CWM(K,LD))-ALOG(QLD))*CF)

	if ( abs(CWM_OUT(K,L)) .lt. 1.e-1) then
	else
	write(0,*) 'bad normextrap cwm_out: ', K,L,cwm_out(K,L)
	write(0,*) 'CWM(LD+1),CWM(LD),CF: ', QLD, CWM(K,LD),CF
	endif

	if (K .eq. KCHK) then
	write(0,*) 'L, CWM(LD+1),CWM(LD), CWM_OUT: ', 
     &                         L, QLD, CWM(K,LD), CWM_OUT(K,L)
	endif

        ENDIF
            
            END DO            ! LD loop
          ENDIF

       END DO ! L loop

           if (K .eq. KCHK) write(0,*) 'okay CWM'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  700	CONTINUE

         write(0,*) 'cmax,maxposi',cmax,maxposi

	write(0,*) 'SIG_INIT past Q def to deallocs'

	do L=1,LM
	write(0,*) 'SIG_INIT N=1, L, TV, Q: ',
     &    L, T_OUT(1,L),Q_OUT(1,L)
	enddo
C
      DEALLOCATE(TMPVRT)
      DEALLOCATE(HGT)
	write(0,*) 'SIG_INIT past HGT,TMPVRT dealloc'

C***
C***  CONVERT FROM VIRTUAL TO TRUE TEMPERATURE.
C***

         cmax=0.

      DO 750 K=1,IMJM
!      DO 750 L=4,LM
      DO 750 L=1,LM

      IF(T_OUT(K,L).GT.325.)T_OUT(K,L)=325.
      IF(T_OUT(K,L).LT.160.)T_OUT(K,L)=160.
      TV=T_OUT(K,L)
!      DO 725 ITER=1,3
      DO 725 ITER=1,1
      T_OUT(K,L)=TV/(1.+0.608*Q_OUT(K,L))
      CLOGES=-CM1/T_OUT(K,L)-CM2*ALOG10(T_OUT(K,L))+CM3
      ESE=10.**(CLOGES+2.)
      QS=EPS*ESE/((PT+SGML1(L)*PDTOP+SGML2(L)*PDO(K))-ESE*(1.-EPS))
      QSMX=0.98*QS
        if(QS.LE.1.E-10) then
      Q_OUT(K,L)=AMAX1(Q_OUT(K,L),1.E-10)
        else
!      IF(Q_OUT(K,L).LT.QSMX)GO TO 750
!      Q_OUT(K,L)=QSMX
      QTMP=AMAX1(Q_OUT(K,L),1.E-10)
      Q_OUT(K,L)=AMIN1(QTMP,QSMX)
        endif
  725 CONTINUE
! ---------------------------------------------- IH Kwon    
!                     Q adjustment: RH dose not exceed 100%
      PMIDO=PT+SGML1(L)*PDTOP +SGML2(L)*PDO(K)
      qsat= pq0/PMIDO *exp(a2*(T_OUT(K,L)-a3)/(T_OUT(K,L)-a4))
      rh0 = Q_OUT(K,L) /qsat
      if(rh0 > 1.)then
        if(rh0>cmax)then
           cmax=rh0
        endif
         rh0=1.
         Q_OUT(K,L)= rh0 *qsat
      endif
! ----------------------------------------------

  750 CONTINUE
       write(0,*) 'max RH before Q_modification',cmax

       write(0,*) 'SIG_INIT past T devirt'
C
!      DEALLOCATE(REF)
      DEALLOCATE(PMID)
      DEALLOCATE(DPDIN_IN)
      DEALLOCATE(PINT)

      write(77) T_OUT,Q_OUT,U_OUT,V_OUT,CWM_OUT
      close(77)

	write(0,*) 'SIG_INIT to allocate of TB...PDB'
      ALLOCATE(TB(IMJM,LM))
      ALLOCATE(QB(IMJM,LM))
      ALLOCATE(CWMB(IMJM,LM))
      ALLOCATE(UB(IMJM,LM))
      ALLOCATE(VB(IMJM,LM))
      ALLOCATE(PDB(IMJM))
	write(0,*) 'SIG_INIT past allocate of TB...PDB'
C***
C***  THE BOUNDARY VALUES AND TENDENCIES.
C***
      DO 800 N=1,IMJM
      PDB(N)=PDO(N)
C      PDB(N,2)=0.
      DO 800 L=1,LM
      TB(N,L)=T_OUT(N,L)
C      TB(N,L,2)=0.
      QB(N,L)=Q_OUT(N,L)
C      QB(N,L,2)=0.
      CWMB(N,L)=CWM_OUT(N,L)
C      CWMB(N,L,2)=0.
      UB(N,L)=U_OUT(N,L)
C      UB(N,L)=0.
      VB(N,L)=V_OUT(N,L)
C      VB(N,L)=0.
  800 CONTINUE
c
      DEALLOCATE(T_OUT)
      DEALLOCATE(U_OUT)
      DEALLOCATE(V_OUT)
      DEALLOCATE(Q_OUT)
      DEALLOCATE(CWM_OUT)
C
C***
C***  WRITE THE BOUNDARY VALUES.
C***
	write(0,*) 'SIG_INIT to boundary writes'
      DO LI=1,LM
	write(0,*) 'SIG_INIT LI: ', LI
      PDBMAX=-9999999.9
      TBMAX=-9999999.9 
      QBMAX=-9999999.9
      CWMBMAX=-9999999.9
      UBMAX=-9999999.9
      VBMAX=-9999999.9
      PDBMIN=9999999.9
      TBMIN=9999999.9
      QBMIN=9999999.9
      CWMBMIN=9999999.9
      UBMIN=9999999.9
      VBMIN=9999999.9
        DO KI=1,IMJM
        PDBMAX=AMAX1(PDB(KI),PDBMAX)
        TBMAX=AMAX1(TB(KI,LI),TBMAX)
        QBMAX=AMAX1(QB(KI,LI),QBMAX)
        CWMBMAX=AMAX1(CWMB(KI,LI),CWMBMAX)
        UBMAX=AMAX1(UB(KI,LI),UBMAX)
        VBMAX=AMAX1(VB(KI,LI),VBMAX)
        PDBMIN=AMIN1(PDB(KI),PDBMIN)
        TBMIN=AMIN1(TB(KI,LI),TBMIN)
        QBMIN=AMIN1(QB(KI,LI),QBMIN)
        CWMBMIN=AMIN1(CWMB(KI,LI),CWMBMIN)
        UBMIN=AMIN1(UB(KI,LI),UBMIN)
        VBMIN=AMIN1(VB(KI,LI),VBMIN)
        ENDDO
        write(0,830)INUNIT,LI,PDBMAX,PDBMIN
        write(0,831)INUNIT,LI,TBMAX,TBMIN
        write(0,832)INUNIT,LI,QBMAX,QBMIN
        write(0,835)INUNIT,LI,CWMBMAX,CWMBMIN
        write(0,833)INUNIT,LI,UBMAX,UBMIN
        write(0,834)INUNIT,LI,VBMAX,VBMIN
  830 FORMAT('SIG_INIT PDB  ', 2X,' INUNIT=',2I3,2(E12.5,1X))
  831 FORMAT('SIG_INIT  TB  ', 2X,' INUNIT=',2I3,2(E12.5,1X))
  832 FORMAT('SIG_INIT  QB  ', 2X,' INUNIT=',2I3,2(E12.5,1X))
  835 FORMAT('SIG_INITCWMB  ', 2X,' INUNIT=',2I3,2(E12.5,1X))
  833 FORMAT('SIG_INIT  UB  ', 2X,' INUNIT=',2I3,2(E12.5,1X))
  834 FORMAT('SIG_INIT  VB  ', 2X,' INUNIT=',2I3,2(E12.5,1X))
      ENDDO
C
C      WRITE(LOUT)PDB,TB,QB,CWMB,UB,VB
C
C..  1D & 2D INITIAL DATA TO 2D & 3D DATA, RESPECTIVELY
C..                   YC KWON
C      FIRST, MASS POINTS
C
      NCN = 0
      DO J = 1,JM
      IF(MOD(J,2).EQ.1) THEN
      II = IM
      ELSE
      II = IM-1 
      ENDIF
C
      DO I = 1,II
      NCN = NCN + 1
      PDINIT(I,J) = PDB(NCN)
      IF(I.EQ.(IM-1).AND.II.EQ.(IM-1))PDINIT(IM,J)=PDINIT(IM-1,J)
C
      DO K = 1,LM
      TINIT(I,K,J) = TB(NCN,K)
      QINIT(I,K,J) = QB(NCN,K)
      CWMINIT(I,K,J) = CWMB(NCN,K)
      IF(I.EQ.(IM-1).AND.II.EQ.(IM-1)) THEN
      TINIT(IM,K,J) = TINIT(IM-1,K,J)
      QINIT(IM,K,J) = QINIT(IM-1,K,J)
      CWMINIT(IM,K,J) = CWMINIT(IM-1,K,J)
      ENDIF
      IF(I.EQ.1.AND.K.EQ.40) THEN
      write(0,*) 'J T AT W BD FROM INIT ',J,TINIT(I,K,J)
      ENDIF
      ENDDO    !K
C
      DO K = 1,LM+1
      PINIT(I,K,J) = PIO_2D(NCN,K)
      IF(I.EQ.(IM-1).AND.II.EQ.(IM-1)) THEN
      PINIT(IM,K,J) = PINIT(IM-1,K,J)
      ENDIF
      ENDDO    !K
C
      ENDDO    !I
      ENDDO    !J
C
C.. SECOND, WIND POINTS
C
      NCN = 0
      DO J = 1,JM
      IF(MOD(J,2).EQ.1) THEN
      II = IM-1
      ELSE
      II = IM
      ENDIF
C
      DO I = 1,II
      NCN = NCN + 1

C
      DO K = 1,LM
      UINIT(I,K,J) = UB(NCN,K)
      VINIT(I,K,J) = VB(NCN,K)
      IF(I.EQ.(IM-1).AND.II.EQ.(IM-1)) THEN
      UINIT(IM,K,J) = UINIT(IM-1,K,J)
      VINIT(IM,K,J) = VINIT(IM-1,K,J)
      ENDIF
      ENDDO

      if(NCN==KCHK)then
      write(0,*) 'NCN,UB(NCN,1):',NCN,UB(NCN,1)
      write(0,*) 'I,J,II,VINIT(I,1,J)',I,J,II,VINIT(I,1,J)
      endif

      ENDDO
      ENDDO
C
      DO K = 1,LM
      DO J = 1,JM
      DO I = 1,IM
C                        ------------------------ IH Kwon: Skip This
!     IF(MOD(J,2).EQ.0.AND.I.EQ.1) THEN
!     UINIT(I,K,J) = (UINIT(I,K,J-1)+UINIT(I,K,J+1))/2.
!     VINIT(I,K,J) = (VINIT(I,K,J-1)+VINIT(I,K,J+1))/2.
!     ENDIF
C
      IF(I.EQ.2.AND.K.EQ.40) THEN
      write(0,*)'U AT WBD INIT ',J,UINIT(1,K,J),UINIT(2,K,J)
      ENDIF
C
      ENDDO
      ENDDO
      ENDDO
C
      WRITE(LOUT)PDINIT,TINIT,QINIT,CWMINIT,UINIT,VINIT,PINIT
      write(0,*) 'U V T AT 10 10 10 ',
     & UINIT(10,10,10),VINIT(10,10,10),TINIT(10,10,10)
C
C.. TEST CONSISTENCY OF BD AND INIT
C
C      WRITE(345,*) 'WEST BD FOR Tinit FROM INIT'
C      DO J = 3,JM-2,2
C      WRITE(345,346) TINIT(1,30,J)
C346   FORMAT(F12.3)
C      ENDDO
C
C      WRITE(347,*) 'SOUTH BD FOR Tinit FROM INIT'
C      DO I = 1,IM
C      WRITE(347,346) TINIT(I,30,1)
C      ENDDO
C
C
C
C.. WRITE TEST DATA TO CHECK WHETHER THEHORIZONTAL INTERPOLATION IS CORRECT OR NOT
C..            TEMPERATURE                  YC KWON
C
C       OPEN(345,FILE='sig2hyb.dat',STATUS='UNKNOWN')
C       DO K = 1,LM
C       WRITE(345,346) ((TINIT(I,K,J),I=1,IM),J=1,JM)
C       WRITE(345,346) ((UINIT(I,K,J),I=1,IM),J=1,JM)
C       WRITE(345,346) ((VINIT(I,K,J),I=1,IM),J=1,JM)
C       WRITE(345,346) ((PINIT(I,K,J),I=1,IM),J=1,JM)
C346    FORMAT(7(F13.2,1X))
C       ENDDO
C       CLOSE(345)
C
       write(0,*)'UB AT SOUTHERN BOUNDARY AT K=1 FROM INIT'
       write(0,112) (UB(I,1),I=1,IM)
112    FORMAT(10(F5.1,1X))
       write(0,*)'END OF SOUTHEN U INIT'
C
      DEALLOCATE(TB)
      DEALLOCATE(QB)
      DEALLOCATE(CWMB)
      DEALLOCATE(UB)
      DEALLOCATE(VB)
      DEALLOCATE(PDB)
 
 1000 CONTINUE
 1500 CONTINUE
C
      DEALLOCATE(ULONB)
C
C      DEALLOCATE(SIGMID)
C      DEALLOCATE(SIGINT)
      DEALLOCATE(Z_OUT)
      DEALLOCATE(PSFC_OUT)
      DEALLOCATE(PDO,PMO,PIO)
      DEALLOCATE(Y2,DUM1,DUM2)
      DEALLOCATE(UIN,VIN,PIN)
      DEALLOCATE(DSG,DSG1,DSG2,SG1,SG2,SGML1,SGML2)
czhang
      return
  888 continue
      print*, 'Error opening file namelist file'
czhang
      RETURN
      END SUBROUTINE SIG2HYB_INIT
      END MODULE MODULE_SIG2HYB_INIT
