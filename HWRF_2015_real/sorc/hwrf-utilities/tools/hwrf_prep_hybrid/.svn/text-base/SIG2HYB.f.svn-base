      MODULE MODULE_SIG2HYB
      PRIVATE
      PUBLIC :: SIG2HYB
      CONTAINS
      SUBROUTINE SIG2HYB(ULATB,UWLONB,KMAX,INUNIT,
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
C   04-09-06  YOUNG KWON MODIFIED FOR HWRF 
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

C     found: T=was able to determine surface presure, F=not able to
        logical :: found
C
!zhang                        P A R A M E T E R
!zhang     1 (LMM1=LM-1,LMP1=LM+1)
!zhang                        P A R A M E T E R
!zhang     1 (IMT=2*IM-1,JMT=JM/2+1)
                        P A R A M E T E R
     1 (CM1=2937.4,CM2=4.9283,CM3=23.5518,EPS=0.622)
C----------------------------------------------------------------
                        D I M E N S I O N
     1  UWLONB(KB),ETA(LMP1),DETA(LM),tmp(LM)
     2, ULATB(KB),K1(KB),K2(KB)
C
        INTEGER, INTENT(IN) :: LDM
        REAL(REAL_32),INTENT(INOUT),POINTER,DIMENSION(:,:) :: 
     &                       T,U,V,Q,CWM,PRES
        REAL(REAL_32),INTENT(INOUT),POINTER,DIMENSION(:,:) :: DUM
        REAL(REAL_32),INTENT(INOUT),POINTER,DIMENSION(:) :: PSFC,ZSFC
C
        REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::TB,UB,VB,QB,CWMB
        REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::H,HETA
        REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::QETA,U_OUT,V_OUT
        REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::PDB,PINT,PMID
        REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::dpdin_in
        REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::Z_OUT,T_OUT
        REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::Q_OUT,CWM_OUT
!        REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::hlatb,hlonb
        REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::PD,HGT
        REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::EXNL,REF,TSGBT
        REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::ULONB,PSFC_OUT
        REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::PDO,PIO,PMO,Y2
        REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::DUM1,DUM2
        REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::SI,SL,SII,SLL
        REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::SIGINT,SIGMID
        REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::PIN,UIN,VIN,TMPVRT
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
      REAL DELTAX, DELTAY, STD_LON,STD_LAT, STD_LAT2, INEXP
      integer :: ierr
      REAL, ALLOCATABLE :: DATA(:,:,:)

      REAL, dimension(:,:), allocatable :: lats,lons,heights,landmask
      type(rotlatlon_info) :: proj
C----------------------------------------------------------------
C
C  SIGMA SURFACES FOR GLOBAL SYSTEM
C  NOW READ IN FROM SUBROUTINE CALL
C
                            D A T A
     1  LINI/51/, LOUT/52/

czhang adding namelist for levels and etop
!      integer :: p_top_requested,ptsgm1
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

       CMAX=0

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
!      ETOP = float(p_top_requested)            
       ETOP = p_top_requested            
   10 CONTINUE
C      REWIND 41            !KWON
C
      print*, "zhang1 debug ==", etop
      print*, "zhang1 debug ==", levels
      ALLOCATE(SI(KMAX+1))
      ALLOCATE(SII(KMAX+1))
      ALLOCATE(SL(KMAX))
      ALLOCATE(SLL(KMAX))
C
c      WRITE(HOLDFILP,99999)INUNIT
c99999 FORMAT('holdeta',I3.3)
c      OPEN(UNIT=INUNIT,FILE=HOLDFILP,FORM='UNFORMATTED',IOSTAT=IER)
C
C      READ(INUNIT)SI,SL
C
C
C   SWITCH INPUT LONGITUDE BACK TO DEGREES EAST
C
      ALLOCATE(ULONB(KB))
C
      DO K = 1, KB
        ULONB(K) = 360.0 - UWLONB(K)
      ENDDO
C
      DO 1000 KTIME=1,1
C
      ALLOCATE(PSFC_OUT(KB))
      ALLOCATE(PINT(KB,LDM+2))
C***
C***  READ GLOBAL DATA ON BOUNDARY (OUTPUT OF LL2PTS)
C***
C      READ(INUNIT,END=1500)T,U,V,Q,CWM,PRES,DUM,PSFC,ZSFC


!!! have PSFC, U, and V on GFS vertical coord

!	do K=1,10
!	write(6,*) 'lev 1 K, T,U,V: ', K, T(K,1),U(K,1),V(K,1)
!	write(6,*) 'lev LDM K, T,U,V: ', K, T(K,LDM),U(K,LDM),V(K,LDM)
!	enddo

C      write(6,12121)INUNIT
12121 FORMAT(' READ PTETBC  INUNIT=',I3)
C***
C***  FLIP 3-DIMENSIONAL ARRAYS BEFORE INTERPOLATION
C***
      CALL FLIP_PTR(T,LDM,KB,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT) !Zhang
      CALL FLIP_PTR(U,LDM,KB,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT) !Zhang
      CALL FLIP_PTR(V,LDM,KB,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT) !Zhang
      CALL FLIP_PTR(Q,LDM,KB,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT) !Zhang
      CALL FLIP_PTR(CWM,LDM,KB,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT) !Zhang ! not done previously
      CALL FLIP_PTR(PRES,LDM,KB,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT) !Zhang ! new variable
      CALL FLIP_PTR(DUM,LDM+1,KB,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT) !Zhang ! new variable

      DO K = 1,KB
      DO I = 1,LDM+1
      PINT(K,I) = DUM(K,I)
      ENDDO
      ENDDO

C***
C***  ROTATE WINDS FROM LAT/LONG GRID TO ETA GRID
C***

	write(0,*) 'call rotlle'
      CALL ROTLLE(U,V,ULATB,ULONB,LDM,KB)
C***
C***  CONVERT SURFACE PRESSURE TO PASCALS
C***
!      ALLOCATE(TSGBT(KB))
C
      DO 50 K=1,KB
!       PSFC(K)=PSFC(K)*100.
!       TSGBT(K)=T(K,LDM)
   50 CONTINUE
C***
C***  DEFINITION OF CONSTANTS NEEDED FOR VERTICAL INTERPOLATION
C***

!!!
!!!	 USE WRF STANDARDS HERE...
!!!

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

      KCHK=858
      write(0,*) 'K Check point (in SIG2HYB):',KCHK
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
C      write(0,*) 'BEFORE READ 42'
C      READ(42,V_LEVELS)     !READ FULL LEVEL SIGMA (1-0) LM+1

C      write(0,132) LEVELS(1)
C132   FORMAT(1X,'levels at l=1 ',F10.2)

C SHOULD BE CHANGED FOR FULL LEVELS

	do L=1,LMP1
	ETA(L)=LEVELS(L)
!	write(0,*) 'L, ETA(L): ', L, ETA(L)
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
      ALLOCATE(HGT(KB))
!      ALLOCATE(hlatb(KB))
!      ALLOCATE(hlonb(KB))
C      ALLOCATE(hlat(IM,JM))
C      ALLOCATE(hlon(IM,JM))
C

!!!	HGT=0. is an Eta model item
!!!
!!!	NEED TO PULL IN THE NMM GRID TOPOGRAPHY, ASSIGN IT TO
!!!     THE BOUNDARY

!!!     LINK APPROPRIATE STATIC FILE TO UNIT 40
!!!     THIS IS BINARY OUPUT OF WRFSI  (DUSAN)

      call read_geogrid_heights(proj,heights,landmask, !lats,lons, 
     &     'geogrid.out',IM,JM,ierr)
      if(ierr/=0) then
         write(6,*) 'Unable to read heights and/or landmask ',          &
     &              ' from geogrid file \"',                            &
     &              '"geogrid.out".  Error=',ierr
         stop 83
      endif
      call boundary_smooth(heights,landmask,real_nsmth,real_nrow,IM,JM) &
      deallocate(landmask)
      write(0,*) 'proj = ',proj
      write(0,*) 'heights(1:10,1) = ',heights(1:10,1)
      write(0,*) 'heights(1,1:10) = ',heights(1,1:10)
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
C        write(0,*) 'READ HLON'
C        WRITE(43,1111) HLON
C	 READ(40,*) hlat
C        write(0,*) 'READ HLAT'
C        WRITE(43,1111) HLAT
1111    FORMAT(1X,10F10.3,1X)

	write(0,*) 'WRF-NMM TOPOGRAPHY'
        do J=JM,1,-JM/20
        write(0,633) (heights(I,J),I=1,IM,IM/10)
	enddo
  633	format(25(f5.0,1x))

        N=1

        DO I=1,IM
	  HGT(N)=heights(I,1)
!          hlatb(n)=lats(i,1)
!          hlonb(n)=lons(i,1)
          N=N+1
        ENDDO

	write(0,*) 'after first set, N= ', N

        DO I=1,IM
	  HGT(N)=heights(I,JM)
!         hlatb(N)=lats(I,JM)
!	  hlonb(N)=lons(I,JM)
	  N=N+1
        ENDDO
	write(0,*) 'after second set, N= ', N

        DO J=3,JM-2,2
          HGT(N)=heights(1,J)
!          hlatb(N)=lats(1,J)
!          hlonb(N)=lons(1,J)
          N=N+1
	ENDDO
	write(0,*) 'after third set, N= ', N

        DO J=3,JM-2,2
          HGT(N)=heights(IM,J)
!          hlatb(N)=lats(IM,J)
!          hlonb(N)=lons(IM,J)
          N=N+1
	ENDDO

!      DO 175 K=1,KB
!      write(0,*) 'K, HGT(K)', K, HGT(K)
!  175 CONTINUE
        !deallocate(lats)
        !deallocate(lons)
        deallocate(heights)
C
C.. WRITE HLATB, HLONB FOR COMPARISON    !YC KWON
C
C      OPEN(987,FILE='hlatb-hlonb-sig2hyb',STATUS='UNKNOWN')
C      DO JJ = 1,KB
C      WRITE(987,989) JJ,HLATB(JJ),HLONB(JJ)
C989   FORMAT(1X,I4,2X,F10.4,2X,F10.4)
C      ENDDO
C
C.. WRITE HLAT HLON FOR COMPARISON     !YC KWON
C
C      OPEN(876,FILE='hlat-hlon-sig2hyb',STATUS='UNKNOWN')
C      DO JJ = 1,JM
C      DO II = 1,IM
C      WRITE(876,877) data(II,JJ,1),data(II,JJ,2)
C877   FORMAT(1X,2(F10.4,1X))
C      ENDDO
C      ENDDO
C
!      DO 180 K=1,KB
!      PD(K)=1.
!     REF(K)=1.
!  180 CONTINUE
C***
C***  FIND PRESSURES ON SIGMA LAYER INTERFACES & MIDPOINTS
C***  FIND HEIGHTS ON SIGMA LAYER INTERFACES
C***
C
      ALLOCATE(PDO(KB))
      ALLOCATE(PMO(LDM))
      ALLOCATE(PIO(LDM+1))
      ALLOCATE(PIN(LDM+1))
      ALLOCATE(UIN(LDM+1))
      ALLOCATE(VIN(LDM+1))
      ALLOCATE(Y2(LDM+2))
      ALLOCATE(DUM1(LDM+1))
      ALLOCATE(DUM2(LDM+1))

      ALLOCATE(PMID(KB,LDM))
      ALLOCATE(DPDIN_IN(KB,LDM))
      ALLOCATE(INPUT_TOP(KB))
      ALLOCATE(H(KB,LDM+2))
C      DEALLOCATE(PINT)
C      ALLOCATE(PINT(KB,LDM+2))
c     ALLOCATE(H(KB,LDM+1))

	write(0,*) 'past alloc block'
	write(0,*) 'KB and LDM ',KB,LDM
C
      DO  K=1,KB
         DO L=1,LDM
            PMID(K,L)=PRES(K,L)
            if (K .eq. 1 ) then
               write(0,*) 'K,L,PMID(K,L): ', K,L,PMID(K,L)
            endif
         enddo
C
         DO L=1,LDMP
            
!     if (K .le. 10 .and. L .ge. LDM-3) then
!	write(0,*) 'K,L,PINT(K,L): ', K,L,PINT(K,L)
!     endif

         enddo
         H(K,LDMP)=ZSFC(K)
c     IF(ZSFC(K) .LT. 0.0) THEN
c     H(K,LDMP) = 0.0
c     ENDIF

      enddo

      ALLOCATE(EXNL(KB))
C
!        CALL FLIP(PMID,LDM)
!        CALL FLIP(T,LDM)
!        CALL FLIP(PINT,LDM+1)
!
! -------------------------------------------- IH Kwon
! Calculate H
!
!     DO LL=1,LDMM
!        L=LDMP-LL
!        DO K=1,KB
!           IF(L.EQ.LDM)EXNL(K)=(PSFC(K)/P00)**CAPA
!           EXNT=(PINT(K,L)/P00)**CAPA
!           H(K,L)=H(K,L+1)-CPOG*T(K,L)*(EXNT-EXNL(K))
!    1           *(P00/PMID(K,L))**CAPA
!           
!           if ( K .le. 10 .and. L .ge. LDM-3) then
!              write(0,*) 'K,L,H(K,L),PMID,T: ', 
!    &              K,L,H(K,L),PMID(K,L),T(K,L)
!           endif

!           if ( K==118 ) then
!              write(0,*) 'K,L,H(K,L),PMID,T: ', 
!    &              K,L,H(K,L),PMID(K,L),T(K,L)
!           endif
!           
C     
!     IF(K.EQ.5) THEN
!     write(0,251)K,L,T(K,L),H(K,L),H(K,L+1),
!     1     PINT(K,L),PMID(K,L),EXNL(K),EXNT,PSFC(K),ZSFC(K)
 251        FORMAT(2X,2I3,9(E12.5,1X))
!     ENDIF
C     
!           EXNL(K)=EXNT
!           
!        enddo
!     enddo

      DO LL=1,LDM
         L=LDMP-LL
        write(0,*) 'LL,L,T(1,L)',LL,L,T(1,L)
   
        DO K=1,KB
           H(K,L)=H(K,L+1)
     &          + R*T(K,L) *log(PINT(K,L+1)/PINT(K,L)) /G
        ENDDO
      ENDDO

!        CALL FLIP(PMID,LDM)
!        CALL FLIP(PINT,LDM+1)
!        CALL FLIP(H,LDM+1)
!        CALL FLIP(T,LDM)

      ALLOCATE(Z_OUT(KB,LM+1))
      ALLOCATE(T_OUT(KB,LM))
      ALLOCATE(Q_OUT(KB,LM))
      ALLOCATE(CWM_OUT(KB,LM))
      ALLOCATE(U_OUT(KB,LM))
      ALLOCATE(V_OUT(KB,LM))

	do K=1,KB
!!!   AT THIS POINT SHOULD BE ABLE TO COMPUTE SURFACE PRESSURE

!          Have not found surface pressure for point k yet:
           found=.false.
           
           if (k.eq.1 .or. k==77) then
              write(0,*)' hgt test 1 ',k,ldmp,HGT(K),H(K,LDMP)
              write(0,*)' h(k,:) = ',h(k,:)
           endif
           
           IF ( HGT(K) .lt. H(K,LDMP) ) THEN ! extrapolate downward
! ------------------------------------- IH Kwon
!                         Using a lapse rate instead of extrapolation
!             
!             DLNPDZ=( LOG(PINT(K,LDMP))-LOG(PINT(K,LDMP-1)) ) / 
!    &             ( H(K,LDMP)-H(K,LDMP-1))
!             
!             PSFC_OUT(K)=EXP( LOG(PINT(K,LDMP))+
!    &                         DLNPDZ*(HGT(K)-H(K,LDMP)))
              found=.true.
!             if (k .eq. 1) then
!                write(0,*) ' '
!                write(0,*) 'K, PINT vals for DLNPDZ: ', K, 
!    &                PINT(K,LDMP), PINT(K,LDMP-1)
!                write(0,*) 'K, H vals for DLNPDZ: ', K, H(K,LDMP),
!    &                H(K,LDMP-1)
!                write(0,*) '(a)K, PSFC_OUT: ', K, PSFC_OUT(K)
!             endif

        TMEAN= T(K,LDM) +0.5*GAMMA*( H(K,LDM )-HGT(K) )

        DLNPDZ=(H(K,LDMP)-HGT(K)) *G /(R*TMEAN)
        PSFC_OUT(K)=PINT(K,LDMP) *EXP(DLNPDZ)
                 write(0,*) 'LDMP, DLNPDZ, HGT: ', LDMP, DLNPDZ, HGT(K)

          if (k .eq. KCHK) then
             write(6,*) 'HGT(K).lt.H(K,LDMP): PSFC_OUT from lapse rate'
             write(6,*)  HGT(K),H(K,LDMP)
             write(6,*)  'T(K,LDM),TMEAN,H(K,LDM)-HGT(K),PSFC_OUT(K)'
             write(6,*)   T(K,LDM),TMEAN,H(K,LDM)-HGT(K),PSFC_OUT(K)
          endif

! -------------------------------------

           ELSE                 ! target level bounded by input levels
              
              DO L=LDMP,2,-1
C     
                 if(k.eq.1) then
                    write(0,*)' hgt test #2 ',k,l,ldmp,HGT(K),H(K,L),
     &                   HGT(K),H(K,L-1)
                 endif
C     IF (HGT(K) .gt. H(K,L) .and. 
C     &                             HGT(K) .lt. H(K,L-1)) then
                 IF (HGT(K) .ge. H(K,L) .and. 
     &                HGT(K) .le. H(K,L-1)) then
                    DLNPDZ=(LOG(PINT(K,L))-LOG(PINT(K,L-1))) /
     &                   (H(K,L)-H(K,L-1))
                    
                    PSFC_OUT(K)=EXP( LOG(PINT(K,L))+DLNPDZ*
     &                   (HGT(K)-H(K,L)))
                    
                    found=.true.
                    if (K .eq. 1) then
                       write(0,*) ' '
                       write(0,*) 'K, PINT vals for DLNPDZ: ',
     &                      K, PINT(K,L), PINT(K,L-1)
                       write(0,*) 'K, H vals for DLNPDZ: ', K, 
     &                      H(K,L),H(K,L-1)
                       write(0,*) 'L,  DLNPDZ, HGT: ', 
     &                      L, DLNPDZ,HGT(K) 
                       write(0,*) '(b)K, PSFC_OUT: ', K, PSFC_OUT(K)
                    endif
                    
!                   Leave innermost do loop:
                    exit
                 ENDIF
              ENDDO
              
           ENDIF

           if( .not. found) then
              write(6,*) 'Unable to determine surface pressure',
     &             ' at k=',k,' hgt(k)=',hgt(k),' ldmp=',ldmp,
     &             ' h(k,ldmp)=',h(k,ldmp),
     &             ' h(k,:)=',h(k,:)
              stop 39
           endif

           PDO(K)=PSFC_OUT(K)-pdtop-pt
           
!           write(0,*)'K,PSFC_OUT,PDO(K),pdtop,pt: ',
!     1          K,PSFC_OUT(k),PDO(K),pdtop,pt
           
!---------now output pressure levels can be defined---------------------
           
           DO L=1,LM
              PMO(L)=SGML2(L)*PDO(K)+SGML1(L)*PDTOP+PT
!     write(0,*) 'L, PMO(L): ', L, PMO(L)
           ENDDO
           
           DO L=1,LDM+2
              Y2(L)=0.
           ENDDO
           
           DO L=1,LM+1
              PIO(L)=SG2(L)*PDO(K)+SG1(L)*PDTOP+PT
!     write(0,*) 'L, PIO(L): ', L, PIO(L)
           ENDDO
           
           DO L=1,LDM+1
!     write(0,*) 'L, H(K,L),PINT(K,L):', L,H(K,L),PINT(K,L)
           ENDDO

! ------------------------------------- IH Kwon
!          VIRTUAL TEMPERATURE IS INTERPOLATED (NOT CALCULATED FROM H)
           
!          IF ( (H(K,LDMP)-HGT(K)) .gt. 0.5) THEN
              
!     basically use computed surface pressure as another data point
!             
!             H(K,LDM+2)=HGT(K)
!             PINT(K,LDM+2)=pio(1)
!             
!              write(0,*) 'BONUS LEVEL for K: ', K
!     
!     mp        call SPLINE2(1,1,LDMP,PINT,H,y2,LM+1, 
!             call SPLINE2(K,LDM+1,PINT(K,2:LDM+2),H(K,2:LDM+2),
!    &             y2(2:LDMP),LM+1, 
!    &             pio,Z_OUT(K,:),dum1,dum2 )
!             
!          ELSE
!              write(0,*) 'no bonus LEVEL for K: ', K
!             
!             call SPLINE2(K,LDM,PINT(K,2:LDMP),H(K,2:LDMP),
!    &             y2(2:LDMP),LM+1, 
!    &             pio,Z_OUT(K,:),dum1,dum2 )
!             
!             
!          ENDIF
!          
!          if (K .eq. 1) then
!             do L=1,LM+1
!                write(0,*) 'L,PIO(L),Z_OUT(K,L): ', L,PIO(L),Z_OUT(K,L)
!             enddo
!          endif
!          
!!!   VIRTUAL TEMPERATURE FOR THE TIME BEING
!          
!          do L=1,LM
!             T_OUT(K,L)=-(g/R)* 
!    &             (z_out(K,L)-z_out(K,L+1))/(log(pio(L))-log(pio(L+1)))
!             if(k.eq.1) then
!                write(0,*) 'tout ',k,l,z_out(K,L),z_out(K,L+1),pio(L),
!    &                pio(L+1),log(pio(L)),log(pio(L+1)),t_out(k,l)
!             endif
!          enddo
!          
!          if (k .eq. 1) then
!             write(0,*) 'K= ', K
!             DO L=1,3
!                write(0,*) 'L, Z_OUT, TV_OUT: ', L, Z_OUT(K,L), 
!    &                T_OUT(K,L)
!             enddo
!          endif

        call SPLINE2(K,LDM,PMID(K,1:LDM),T(K,1:LDM),
     &                y2(1:LDM),LM,
     &                PMO,T_OUT(K,:),dum1,dum2 )

!                     T & T_OUT: Virtual temperature

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
! -------------------------------------

           
	enddo                   !!!! END LOOP OVER BOUNDARY POINTS K
        
	write(0,*) 'done with loop, dealloc EXNL, T'
        
      DEALLOCATE(EXNL)
!     DEALLOCATE(T)                                !! IH Kwon
	write(0,*) 'past dealloc EXNL, T'

C***
C***  VERTICAL INTERPOLATION OF HEIGHTS AND SPECFIC HUMIDITY
C***  QUADRATICALLY IN LN P AND OF WINDS LINEARLY IN LN P.
C***
C
	write(0,*) 'allocated H? ', allocated(H)
      DEALLOCATE(H)
	write(0,*) 'past deallocate of EXNL, T, H'
C***
C***  ARRANGEMENT OF LOCATIONS IN THE V BOUNDARY FILE
C***
C                       2ND
C           IBBP--------->----------ITB
C           ILB                     KB
C            |                       |
C       3RD /|\                     /|\ 4TH
C            |                       |
C           IMT                     ILBP
C            1----------->----------IBB
C                       1ST
      IBB=IMT/2
      IBBP=IBB+1
      ITB=IMT-1
      ILB=IMT+JMT-2
      ILBP=ILB+1
      ILBP2=ILB+2
      ILBM=ILB-1
      IMTP=IMT+1
      KBM=KB-1
C***
C***  FIND AVERAGE PD OVER VELOCITY BOUNDARY POINTS
C***
      DO 505 K=1,IBB
      K1(K)=K
      K2(K)=K+1
      write(0,*) 'k,k1,k2:',k,k1(k),k2(k)
  505 CONTINUE
      DO 506 K=IBBP,ITB
      K1(K)=K+1
      K2(K)=K+2
      write(0,*) 'k,k1,k2:',k,k1(k),k2(k)
  506 CONTINUE
      DO 507 K=IMTP,ILBM
      K1(K)=K+1
      K2(K)=K+2
      write(0,*) 'k,k1,k2:',k,k1(k),k2(k)
  507 CONTINUE
      DO 508 K=ILBP2,KBM
      K1(K)=K
      K2(K)=K+1
      write(0,*) 'k,k1,k2:',k,k1(k),k2(k)
  508 CONTINUE
      K1(IMT)=1
      K2(IMT)=IMT+2
      K1(ILB)=ILB+1
      K2(ILB)=IBB+2
      K1(ILBP)=IBB+1
      K2(ILBP)=ILBP+1
      K1(KB)=KB
      K2(KB)=ITB+2
      write(0,*) 'k,k1,k2:',imt,k1(imt),k2(imt)
      write(0,*) 'k,k1,k2:',ilb,k1(ilb),k2(ilb)
      write(0,*) 'k,k1,k2:',ilbp,k1(ilbp),k2(ilbp)
      write(0,*) 'k,k1,k2:',kb,k1(kb),k2(kb)
C
      L=LM
      DO 600 K=1,KB

      if(k.eq.1) print *,'in 600 ',k,l,t_out(k,l)
      K1K=K1(K)
      K2K=K2(K)
      PDVP=0.5*(PDO(K1K)+PDO(K2K))

!flux 
	INPUT_TOP(K)=1

      DO L=1,LDM
	AVG1=0.5*(PINT(K1K,L)+PINT(K2K,L))
	if (AVG1 .lt. PT) INPUT_TOP(K)=L ! if above top, keep incrementing
	AVG2=0.5*(PINT(K1K,L+1)+PINT(K2K,L+1))
        DPDIN_IN(K,L)=AVG2-AVG1



!	if (mod(K,50) .eq. 0) then
!	write(0,*) 'L, PINT(L),PINT(L+1),DPDIN: ', 
!     &               L, PINT(K1K,L),PINT(K1K,L+1),DPDIN_IN(K,L)
!	endif
      ENDDO
!flux

C
C***
C***  SPLINE COMPUTATION OF WINDS
C***
      DO 550 L=1,LM
	PMO(L)=SGML2(L)*PDVP+SGML1(L)*PDTOP+PT
  550 CONTINUE

!  Generate 1-D arrays of P, U, and V for splines

      DO L=1,LDM
!       PIN(L)=PMID(K,L)
        PIN(L)=0.5*(PMID(K1K,L)+PMID(K2K,L))             ! IH Kwon
        UIN(L)=U(K,L)
        VIN(L)=V(K,L)
      ENDDO

!       EXTEND AT UPPER BOUNDARY

        IF (PIN(1) .gt. PMO(LM)) then
          D1=(PMO(LM)-PIN(1))/(PIN(1)-PIN(2))
          UIN(1)=UIN(1)+D1*(UIN(1)-UIN(2))
          VIN(1)=VIN(1)+D1*(VIN(1)-VIN(2))
          PIN(1)=PMO(LM)
        ENDIF

! ------------------------------------------------------------- IH Kwon
!       IF (PIN(LDM) .LT. PMO(1)) THEN

! 	write(0,*) 'adding another level, PMID, PMO: ', 
!     &                  K, PMID(K,LDM),PMO(1)

!       PIN(LDM+1)=PMO(1)
!       UIN(LDM+1)=UIN(LDM)
!       VIN(LDM+1)=VIN(LDM)
!	if (K .eq. 1) then
!	do L=1,LDM+1
!	write(0,*) 'L,PIN,UIN,VIN: ', L,PIN(L),UIN(L),VIN(L)
!	enddo
!	endif
!
!       CALL SPLINE2(K,LDM+1,PIN,UIN,Y2,LM,PMO,
!    &               U_OUT(K,:),DUM1,DUM2)
!       CALL SPLINE2(K,LDM+1,PIN,VIN,Y2,LM,PMO,
!    &               V_OUT(K,:),DUM1,DUM2)

!       ELSE
!	write(0,*) 'NOT adding another level, PMID, PMO: ', 
!    &                  K, PMID(K,LDM),PMO(1)

! 	if (K .eq. 1) then
! 	do L=1,LDM
! 	write(0,*) 'L,PIN,UIN,VIN: ', L,PIN(L),UIN(L),VIN(L)
! 	enddo
! 	endif

!      CALL SPLINE2(K,LDM,PIN(1:LDM),UIN(1:LDM),Y2,LM,PMO,
!    &               U_OUT(K,:),DUM1,DUM2)
!      CALL SPLINE2(K,LDM,PIN(1:LDM),VIN(1:LDM),Y2,LM,PMO,
!    &               V_OUT(K,:),DUM1,DUM2)

!      ENDIF

! 	if (K .eq. 1) then

! 	do L=1,LM
! 	write(0,*) 'L,PMO, U_OUT(K,L),V_OUT(K,L): ', 
!     &                    L,PMO(L), U_OUT(K,L),V_OUT(K,L),
!     &                    T_OUT(K,L)
! 	enddo

! 	endif

       if (K .eq. 857 .or. K .eq. KCHK) then
       write(0,*) 'L,PIN,UIN,VIN: '
       do L=1,LDM
       write(0,*) L,PIN(L),UIN(L),VIN(L)
       enddo
       endif

       CALL SPLINE2(K,LDM,PIN(1:LDM),UIN(1:LDM),Y2,LM,PMO,
     &               U_OUT(K,:),DUM1,DUM2)
       CALL SPLINE2(K,LDM,PIN(1:LDM),VIN(1:LDM),Y2,LM,PMO,
     &               V_OUT(K,:),DUM1,DUM2)

       if (K .eq. 857 .or. K .eq. KCHK) then
       write(0,*) 'L,PMO,U_OUT,V_OUT:'
       do L=LM,1,-1
       write(0,*) L,PMO(L),U_OUT(K,L),V_OUT(K,L)
       enddo
       endif
! -------------------------------------------------------------

  600 CONTINUE

      write(0,*) 'Skip the flux adjustment'

      goto 997        ! IH Kwon: SKIP this Flux adjustment

!!!! Flux adjustment/comparison

        do K=1,KB

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
!        write(0,*) 'K, INPUT_TOP, dpdinsum, dpdosum: ', K, 
!     &                 INPUT_TOP(K),dpdinsum,dpdosum
!        write(0,*) 'K, ui/uo,vi/vo: ',K,sfluxui/sfluxuo,sfluxvi/sfluxvo
!	write(0,*) 'sfluxui, sfluxuo: ', sfluxui, sfluxuo
!	write(0,*) 'sfluxvi, sfluxvo: ', sfluxvi, sfluxvo
!	write(0,*) ' ------------- '

	

!	do L=INPUT_TOP(K),LDM
!	write(0,*) 'PMID,U,V inputs: ', PMID(K,L),U(K,L),V(K,L)
!	enddo

        endif

        rdpdo=1./dpdosum

        if (abs((sfluxui-sfluxuo)*rdpdo) .gt. 0.3) then
        write(0,*) 'big U change..', K,abs((sfluxui-sfluxuo)*rdpdo)
        endif
        if (abs((sfluxvi-sfluxvo)*rdpdo) .gt. 0.3) then
        write(0,*) 'big V change..', K,abs((sfluxvi-sfluxvo)*rdpdo)
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
!	write(0,*)'PMO, u_out,v_out (post): ',
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

!!!! end flux adjustment/comparison

  997 continue                   ! IH Kwon:  Flux adjustment is skipped

C
C
      DEALLOCATE(INPUT_TOP)
      write(0,*) 'past spline, deallocate of U,V'
C
C     INTERPOLATE Q LINEARLY IN LN(P) FROM SIGMA TO HYBRID
C

	ALLOCATE(TMPVRT(LDM))
      DO 700 K=1,KB

!	write(0,*) 'work Q interpolation, K', K


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
	!if (K .eq. 1) then
	!write(0,*) 'LD,PMID, Q): ', LD,PMID(1,LD), Q(1,LD)
	!endif
       ENDDO

!       integrating from TOA downward...

        DO L=LM,1,-1
          ALPOUT=LOG(PT+SGML1(L)*PDTOP+SGML2(L)*PDO(K))
          PMIDO=PT+SGML1(L)*PDTOP+SGML2(L)*PDO(K)
          Q_OUT(K,L)=-9999.

          IF (PMIDO .gt. PMID(K,LDM)) THEN  ! extrap downward

! ----------------------------------- IH Kwon
!                        Calculation from RH & Tv instead of extrapolation
!
!       if (K .eq. 1) write(0,*) 'L, extrap down, PMID(K,LDM): '
!    &                   ,L,PMID(K,LDM)
!           DLNQDLNP= (log(Q(K,LDM))-log(Q(K,LDM-1))) / 
!    &                (log(PMID(K,LDM))-log(PMID(K,LDM-1)))

!       Q_OUT(K,L)=  EXP(log(Q(K,LDM)) + DLNQDLNP * 
!    &                 (log(PMIDO)-log(PMID(K,LDM))) )

            diff=PMIDO-PMID(K,LDM)
            if(diff>cmax)then
               cmax=diff
               maxposi=K
            endif

            tbot= T(K,LDM)/(1.+ a1*Q(K,LDM))
            qsatbot= pq0/PMID(K,LDM) *exp(a2*(tbot-a3)/(tbot-a4))
            rhbot= Q(K,LDM)/qsatbot
           if(rhbot > 1.) rhbot=1.

              cint= 0.01
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
	    if (K .eq. 1) 
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

!	if (K .eq. 1) then
!	write(0,*) 'L, Q(LD+1),Q(LD), Q_OUT: ', 
!     &                         L, QLD, Q(K,LD), Q_OUT(K,L)
!	endif

        ENDIF
            
            END DO
          ENDIF

         denom=T_OUT(K,L)-CM2*ALOG10(T_OUT(K,L))+CM3
!         if(k.eq.1) then
!         write(0,1001)k,l,denom,T_OUT(K,L),ALOG10(T_OUT(K,L)),
!     1    hlatb(k),hlonb(k),hgt(k)
1001     format(1x,2i5,6(1x,e12.5))
!         endif
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
	if (K .eq. 1) write(0,*) 'L, extrap down, PMID(K,LDM): '
     &                   ,L,PMID(K,LDM)
            DLNQDLNP= (log(CWM(K,LDM))-log(CWM(K,LDM-1))) / 
     &                (log(PMID(K,LDM))-log(PMID(K,LDM-1)))

!       write(0,*)'check CWM_OUT '
!       write(0,*)'logCWM(K,LDM) DLNQDLNP ',log(CWM(K,LDM)),DLNQDLNP
!       write(0,*)'log(PMIDO) log(PMID) ',log(PMIDO),log(PMID(K,LDM))
!       write(0,*)'inside exp ',
!     & log(CWM(K,LDM))+DLNQDLNP*(log(PMIDO)-log(PMID(K,LDM)))
!       write(0,*)'end check'


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
!        write(0,*) 'ok extrapdown cwm_out: ', K,L,cwm_out(K,L)
!	write(0,*) 'CWM,DLNQDLNP: ', CWM(K,LDM), DLNQDLNP
	else
	write(0,*) 'bad extrapdown cwm_out: ', K,L,cwm_out(K,L)
        write(0,*) 'CWM,DLNQDLNP: ', CWM(K,LDM), DLNQDLNP
	endif

          ELSEIF (PMIDO .lt. PMID(K,1)) THEN ! extrap up
	    if (K .eq. 1) 
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

!	if (K .eq. 1) then
!	write(0,*) 'L, CWM(LD+1),CWM(LD), CWM_OUT: ', 
!     &                         L, QLD, CWM(K,LD), CWM_OUT(K,L)
!	endif

        ENDIF
            
            END DO
          ENDIF

       END DO ! L loop

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  700	CONTINUE

	write(0,*) 'past Q def to deallocs'

	do L=1,LM
	write(0,*) 'N=1, L, T, Q: ', L, T_OUT(1,L),Q_OUT(1,L)
	enddo
C
      DEALLOCATE(TMPVRT)
      DEALLOCATE(HGT)
	write(0,*) 'past HGT,TMPVRT dealloc'

C***
C***  CONVERT FROM VIRTUAL TO TRUE TEMPERATURE.
C***
	
      DO 750 K=1,KB
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
	write(0,*) 'past T devirt'

C
!      DEALLOCATE(REF)
      DEALLOCATE(PMID)
      DEALLOCATE(DPDIN_IN)
      DEALLOCATE(PINT)

	write(0,*) 'to allocate of TB...PDB'
      ALLOCATE(TB(KB,LM,2))
      ALLOCATE(QB(KB,LM,2))
      ALLOCATE(CWMB(KB,LM,2))
      ALLOCATE(UB(KB,LM,2))
      ALLOCATE(VB(KB,LM,2))
      ALLOCATE(PDB(KB,2))
	write(0,*) 'past allocate of TB...PDB'
C***
C***  THE BOUNDARY VALUES AND TENDENCIES.
C***
      DO 800 N=1,KB
      PDB(N,1)=PDO(N)
      PDB(N,2)=0.
      DO 800 L=1,LM
      TB(N,L,1)=T_OUT(N,L)
      TB(N,L,2)=0.
      QB(N,L,1)=Q_OUT(N,L)
      QB(N,L,2)=0.
      CWMB(N,L,1)=CWM_OUT(N,L)
      CWMB(N,L,2)=0.
      UB(N,L,1)=U_OUT(N,L)
      IF(L.EQ.40) write(0,*) 'N U AT BD FROM BDY ',N,U_OUT(N,L)
      UB(N,L,2)=0.
      VB(N,L,1)=V_OUT(N,L)
      VB(N,L,2)=0.
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
	write(0,*) 'to boundary writes'
      DO LI=1,LM
	write(0,*) 'LI: ', LI
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
        DO KI=1,KB
        PDBMAX=AMAX1(PDB(KI,1),PDBMAX)
        TBMAX=AMAX1(TB(KI,LI,1),TBMAX)
        QBMAX=AMAX1(QB(KI,LI,1),QBMAX)
        CWMBMAX=AMAX1(CWMB(KI,LI,1),CWMBMAX)
        UBMAX=AMAX1(UB(KI,LI,1),UBMAX)
        VBMAX=AMAX1(VB(KI,LI,1),VBMAX)
        PDBMIN=AMIN1(PDB(KI,1),PDBMIN)
        TBMIN=AMIN1(TB(KI,LI,1),TBMIN)
        QBMIN=AMIN1(QB(KI,LI,1),QBMIN)
        CWMBMIN=AMIN1(CWMB(KI,LI,1),CWMBMIN)
        UBMIN=AMIN1(UB(KI,LI,1),UBMIN)
        VBMIN=AMIN1(VB(KI,LI,1),VBMIN)
        ENDDO
        write(0,830)INUNIT,LI,PDBMAX,PDBMIN
        write(0,831)INUNIT,LI,TBMAX,TBMIN
        write(0,832)INUNIT,LI,QBMAX,QBMIN
        write(0,835)INUNIT,LI,CWMBMAX,CWMBMIN
        write(0,833)INUNIT,LI,UBMAX,UBMIN
        write(0,834)INUNIT,LI,VBMAX,VBMIN
  830   FORMAT(' PDB  ', 2X,' INUNIT=',2I3,2(E12.5,1X))
  831   FORMAT('  TB  ', 2X,' INUNIT=',2I3,2(E12.5,1X))
  832   FORMAT('  QB  ', 2X,' INUNIT=',2I3,2(E12.5,1X))
  835   FORMAT('CWMB  ', 2X,' INUNIT=',2I3,2(E12.5,1X))
  833   FORMAT('  UB  ', 2X,' INUNIT=',2I3,2(E12.5,1X))
  834   FORMAT('  VB  ', 2X,' INUNIT=',2I3,2(E12.5,1X))
      ENDDO
C
      WRITE(LOUT)PDB,TB,QB,CWMB,UB,VB
C
C.. CHECK SOUTHERN BOUNDARY WITH INITIAL DATA
C                                      KWON
      write(0,*) 'SOUTHERN BOUNDARY U DATA AT K = 1 FROM BC'
      write(0,212) (UB(I,1,1),I=1,IM)
212   FORMAT(10(F5.1,1X))
      write(0,*) 'END OF SOUTHERN U BC'
C
C.. TEST FOR BC
C
C      WRITE(333,*) 'WEST BD FOR TB FROM BDY'
C      DO II=431,644
C      WRITE(333,222) TB(II,30,1)
C222   FORMAT(F12.3)
C      ENDDO
C
C      WRITE(334,*) 'SOUTH BD FOR TB FROM BDY'
C      DO II=1,215
C      WRITE(334,222) TB(II,30,1)
C      ENDDO
C
C
      DEALLOCATE(TB)
      DEALLOCATE(QB)
      DEALLOCATE(CWMB)
      DEALLOCATE(UB)
      DEALLOCATE(VB)
      DEALLOCATE(PDB)
 
 1000 CONTINUE
      write(0,*) 'BETWEEN 1000 1500 '
 1500 CONTINUE
      write(0,*) 'AFTER 1500 '
C
      DEALLOCATE(ULONB)
      write(0,*) 'AFTER DEALLOCATE ULONB'
C
C      DEALLOCATE(SIGMID)
C      write(0,*) 'AFTER DEALLOCATE SIGMID'
C      DEALLOCATE(SIGINT)
C      write(0,*) 'AFTER DEALLOCATE SIGINT'
      DEALLOCATE(Z_OUT)
      write(0,*) 'AFTER DEALLOCATE ZOUT'
      DEALLOCATE(PSFC_OUT)
      write(0,*) 'AFTER DEALLOCATE PSFC_OUT'
      DEALLOCATE(PDO,PMO,PIO)
      DEALLOCATE(Y2,DUM1,DUM2)
      DEALLOCATE(UIN,VIN,PIN)
      DEALLOCATE(DSG,DSG1,DSG2,SG1,SG2,SGML1,SGML2)
      write(0,*) 'the end of sig2hyb'
czhang
      return
  888 continue
      print*, 'Error opening file namelist file'
czhang
      RETURN
      END SUBROUTINE SIG2HYB

      END MODULE MODULE_SIG2HYB
