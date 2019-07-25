      PROGRAM HWRF_MERGE_NEST2

!??????????????????????????????????????????????????????????
!     
! ABSTRACT: CREATE 3x DEGREE2 3KM RESOLUTION DOMAIN, the center of the domain
!           is the same as the newly created wrfinput_d01 and wrfanl_d02
!     
! ORIGINAL AUTHOR: QINGFU LIU, NCEP/EMC, 2007
! REVISED  AUTHOR: XUEJIN ZHANG (AOML/HRD), Qingfu Liu, JULY 2011 
!                : Extend the initialization to 3km
!
!     DECLARE VARIABLES
!
!      IMPLICIT NONE

      INTEGER I,J,K,NX,NY,NZ,NST,IFLAG,JX,JY,KZ
!
      PARAMETER (NST=5)
      PARAMETER (max_domain=3)
!      PARAMETER (IX=117,IY=225)
!      PARAMETER (NX=215,NY=431,NZ=42,NST=5)
!      PARAMETER (JX=393,JY=735)       ! fixed for 9 km resolution
      PARAMETER (GAMMA=6.5E-3,G=9.8,Rd=287.05,D608=0.608)
      PARAMETER (Cp=1004.)

! Variables on new outer nest hybrid coordinate (d01)

      REAL(4)  DLMD0,DPHD0,WBD0,SBD0,CLON0,CLAT0
      REAL(4), ALLOCATABLE :: HLON0(:,:),HLAT0(:,:),VLON0(:,:),VLAT0(:,:)

! Variables on new inner nest hybrid coordinate (d03)

      REAL(4), ALLOCATABLE :: HLON4(:,:),HLAT4(:,:),VLON4(:,:),VLAT4(:,:)
      REAL slon0,slat0,rlon0,rlat0,clon05,clat05

! Variables for 3x working domain (30x30 degress, 3km)

      REAL(4)  CLON_NHC,CLAT_NHC
      REAL(4)  DLMD3,DPHD3,PT3,PDTOP3              ! use the new inner nest data
      REAL(4)  WBD3,SBD3,WBD21,SBD21,CLON3,CLAT3

      REAL(4), ALLOCATABLE :: HLON3(:,:),HLAT3(:,:)
      REAL(4), ALLOCATABLE :: VLON3(:,:),VLAT3(:,:)

      REAL(4), ALLOCATABLE :: T3(:,:,:),Q3(:,:,:)
      REAL(4), ALLOCATABLE :: U3(:,:,:),V3(:,:,:)
      REAL(4), ALLOCATABLE :: Z3(:,:,:),P3(:,:,:)
      REAL(4), ALLOCATABLE :: PD3(:,:),PMID3(:,:,:)
      REAL(4), ALLOCATABLE :: PMV3(:,:,:)
      REAL(4), ALLOCATABLE :: A13(:,:),B13(:,:),C13(:,:)

      REAL(4), ALLOCATABLE :: SLP3(:,:)
      REAL(4), ALLOCATABLE :: ZS3(:,:),TS3(:,:),QS3(:,:)

! working arrays used for outer nest interpolation  (parent)

      integer(4), ALLOCATABLE :: IIH(:,:,:),JJH(:,:,:)
      integer(4), ALLOCATABLE :: IIV(:,:,:),JJV(:,:,:)
      REAL(4),    ALLOCATABLE :: HBWGT(:,:,:),VBWGT(:,:,:)

! working arrays used for inner nest interpolation  (d02)

      integer(4), ALLOCATABLE :: IIH1(:,:,:),JJH1(:,:,:)
      integer(4), ALLOCATABLE :: IIV1(:,:,:),JJV1(:,:,:)
      REAL(4),    ALLOCATABLE :: HBWGT1(:,:,:),VBWGT1(:,:,:)

! working arrays used for inner nest interpolation  (d03)

      integer(4), ALLOCATABLE :: IIH2(:,:,:),JJH2(:,:,:)
      integer(4), ALLOCATABLE :: IIV2(:,:,:),JJV2(:,:,:)
      REAL(4),    ALLOCATABLE :: HBWGT2(:,:,:),VBWGT2(:,:,:)

! working array

      REAL(4), ALLOCATABLE :: T21(:,:,:,:),Q21(:,:,:,:)
      REAL(4), ALLOCATABLE :: U21(:,:,:,:),V21(:,:,:,:)
      REAL(4), ALLOCATABLE :: SLP21(:,:,:)

      integer(4) IH1(4),JH1(4),IV1(4),JV1(4)
      integer(4) I_START(3), J_START(3)
      integer(4) CURRENT_DOMAIN_ID, RATIO

! Variables from outer nest (d01)

      REAL(4) DLMD1,DPHD1,PT1,PDTOP1
      REAL(4) WBD1,SBD1,CLON1,CLAT1

      REAL(4), ALLOCATABLE :: T1(:,:,:),Q1(:,:,:)
      REAL(4), ALLOCATABLE :: U1(:,:,:),V1(:,:,:) 
      REAL(4), ALLOCATABLE :: Z1(:,:,:),P1(:,:,:)
      REAL(4), ALLOCATABLE :: HLON1(:,:),HLAT1(:,:),VLON1(:,:),VLAT1(:,:)
      REAL(4), ALLOCATABLE :: ETA1(:),ETA2(:)
      REAL(4), ALLOCATABLE :: PD1(:,:)
      REAL(4), ALLOCATABLE :: A101(:,:),B101(:,:)

      REAL(4), ALLOCATABLE :: SLP1(:,:)
      REAL(4), ALLOCATABLE :: PMID1(:,:,:),ZMID1(:,:,:)
      REAL(4), ALLOCATABLE :: PMV1(:,:,:)
    
! Variables from inner nest (d02)

      REAL(4) DLMD2,DPHD2,PT2,PDTOP2
      REAL(4) WBD2,SBD2,CLON2,CLAT2

      REAL(4), ALLOCATABLE :: T2(:,:,:),Q2(:,:,:)
      REAL(4), ALLOCATABLE :: U2(:,:,:),V2(:,:,:)
      REAL(4), ALLOCATABLE :: Z2(:,:,:),P2(:,:,:)
      REAL(4), ALLOCATABLE :: HLON2(:,:),HLAT2(:,:)
      REAL(4), ALLOCATABLE :: VLON2(:,:),VLAT2(:,:)
      REAL(4), ALLOCATABLE :: PD2(:,:),A102(:,:),B102(:,:),C102(:,:)
   
      REAL(4), ALLOCATABLE :: SLP2(:,:)
      REAL(4), ALLOCATABLE :: PMID2(:,:,:),ZMID2(:,:,:)
      REAL(4), ALLOCATABLE :: PMV2(:,:,:)

! Variables from inner nest (d03)

      REAL(4) DLMD5,DPHD5,PT5,PDTOP5
      REAL(4) WBD5,SBD5,CLON5,CLAT5

      REAL(4) DX_S,DY_S

      REAL(4), ALLOCATABLE :: T5(:,:,:),Q5(:,:,:)
      REAL(4), ALLOCATABLE :: U5(:,:,:),V5(:,:,:)
      REAL(4), ALLOCATABLE :: Z5(:,:,:),P5(:,:,:)
      REAL(4), ALLOCATABLE :: HLON5(:,:),HLAT5(:,:)
      REAL(4), ALLOCATABLE :: VLON5(:,:),VLAT5(:,:)
      REAL(4), ALLOCATABLE :: PD5(:,:),A105(:,:),B105(:,:),C105(:,:)
   
      REAL(4), ALLOCATABLE :: SLP5(:,:)
      REAL(4), ALLOCATABLE :: PMID5(:,:,:),ZMID5(:,:,:)
      REAL(4), ALLOCATABLE :: PMV5(:,:,:)
      
      CHARACTER*1 SN,EW
!zhang
      character*2 basin

!!!!!!!!!!!!!!!!11


      COEF1=Rd/Cp
      COEF3=Rd*GAMMA/G
      COEF2=1./COEF3

      GRD=G/Rd

      pi=4.*atan(1.)
      pi_deg=180./pi
      pi180=1./pi_deg

      DIST1=6.371E3*pi180

!zhang: added basin domain shift option
      READ(5,*)ITIM,IVOBS,IBGS,CLAT0,CLON0,basin               ! CLAT0,CLON0 is the new domain center

      IVOBS_CUT=5.

! READ PARENT DATA (d01)   ! 6 hour forecast data

      IUNIT=20+ITIM

      READ(IUNIT) KX0,KY0,KZ0

      print*,'KX0,KY0,KZ0=',KX0,KY0,KZ0

      KX01 = KX0 + 1
      KY01 = KY0 + 1

      ALLOCATE ( HLON1(KX0,KY0),HLAT1(KX0,KY0) )

      READ(IUNIT) DLMD0,DPHD0,CLON1,CLAT1
      READ(IUNIT) ! PT1,PDTOP1
      READ(IUNIT) ! T1
      READ(IUNIT) ! Q1
      READ(IUNIT) ! U1
      READ(IUNIT) ! V1
      READ(IUNIT) ! Z1
      READ(IUNIT) HLON1,HLAT1  ! ,VLON1,VLAT1
      READ(IUNIT) ! P1
      READ(IUNIT) ! PD1
      READ(IUNIT) ! ETA1
      READ(IUNIT) ! ETA2

      REWIND(IUNIT)

! read in storm center

       read(11,11)ICLAT,SN,ICLON,EW
  11   format(33x,I3,A1,I5,A1)
       CLAT_NHC=ICLAT*0.1
       CLON_NHC=ICLON*0.1

       IF(SN.eq.'S')CLAT_NHC=-CLAT_NHC
       IF(EW.eq.'W')CLON_NHC=-CLON_NHC

      I360=180
      if(abs(CLON_NHC).gt.90.)then
         I360=360
      end if

      print*,'I360=',I360

!      if(abs(CLON0).gt.90..or.abs(CLON1).gt.90..or.      &
!         abs(CLON_NHC).gt.90..or.                        &
!         abs(HLON1(KX0/2,KY0/2)).gt.90.)then
!         I360=360
!      end if
!
      if(I360.eq.360) then
        IF(CLON0.GT.0.)CLON0=CLON0-360.
        IF(CLON1.GT.0.)CLON1=CLON1-360.
        if(CLON_NHC.gt.0.)CLON_NHC=CLON_NHC-360.
        DO J=1,KY0
        DO I=1,KX0
	  IF(HLON1(I,J).GT.0.)HLON1(I,J)=HLON1(I,J)-360.
        END DO
        END DO
      endif

! check domain center

!      dc_dist=abs(CLON1-CLON0)+abs(CLAT1-CLAT0)
!      if(dc_dist.gt.0.01)then
!        print*,'WARNING! domain center is different, check input data'
!        CLON0=CLON1
!        CLAT0=CLAT1
!      end if

      print*,'DLMD0,DPHD0,CLON0,CLAT0=',DLMD0,DPHD0,CLON0,CLAT0

       WBD0=-(KX0-1)*DLMD0                  ! PARENT wbd
       SBD0=-((KY0-1)/2)*DPHD0              ! PARENT SBD

      ALLOCATE ( HLON0(KX0,KY0),HLAT0(KX0,KY0),VLON0(KX0,KY0),VLAT0(KX0,KY0) )

      CALL EARTH_LATLON ( HLAT0,HLON0,VLAT0,VLON0,        &  !Earth lat,lon at H and V points
                          DLMD0,DPHD0,WBD0,SBD0,          &  !input res,west & south boundaries,
                          CLAT1,CLON1,                    &  ! central lat,lon, all in degrees
                          1,KX01,1,KY01,1,1,              &
                          1,KX0 ,1,KY0 ,1,1,              &
                          1,KX0 ,1,KY0 ,1,1         )

      if(I360.eq.360) then
        DO J=1,KY0
        DO I=1,KX0
	  IF(HLON0(I,J).GT.0.)HLON0(I,J)=HLON0(I,J)-360.
	  IF(VLON0(I,J).GT.0.)VLON0(I,J)=VLON0(I,J)-360.
        END DO
        END DO
      endif

      print*,'HLAT0,HLON0,VLAT0,VLON0,1,1=',                  &
              HLAT0(1,1),HLON0(1,1),VLAT0(1,1),VLON0(1,1)
      print*,'HLAT0,HLON0,VLAT0,VLON0,KX0,KY0=',                  &
              HLAT0(KX0,KY0),HLON0(KX0,KY0),VLAT0(KX0,KY0),VLON0(KX0,KY0)

      print*,'HLAT1,HLON1,1,1=',HLAT1(1,1),HLON1(1,1)
      print*,'CLON0,CLAT0=',CLON0,CLAT0

      print*,'this is check:'
      DO J=1,KY0
      DO I=1,KX0
         DIF1=abs(HLON1(I,J)-HLON0(I,J))+abs(HLAT1(I,J)-HLAT0(I,J))
         IF(DIF1.GT.0.001)THEN
            print*,'need to check hlat0 and hlon0'
            print*,'DIF1=',DIF1,I,J
            stop
         END IF
      END DO
      END DO

! READ INNER NEST DATA (d02)   ! 6 hour forecast data

      IUNIT=30+ITIM

      READ(IUNIT) KX2,KY2,KZ2              ! KZ2==NZ

      print*,'KX2,KY2,KZ2=',KX2,KY2,KZ2

      ALLOCATE ( HLON2(KX2,KY2),HLAT2(KX2,KY2) )
      ALLOCATE ( VLON2(KX2,KY2),VLAT2(KX2,KY2) )

      READ(IUNIT) DLMD2,DPHD2,CLON2,CLAT2
      READ(IUNIT) ! PT2,PDTOP2
      READ(IUNIT) ! T2
      READ(IUNIT) ! Q2
      READ(IUNIT) ! U2
      READ(IUNIT) ! V2
      READ(IUNIT) ! Z2
      READ(IUNIT) HLON2,HLAT2,VLON2,VLAT2
      READ(IUNIT) ! P2
      READ(IUNIT) ! PD2
      READ(IUNIT) ! ETA1
      READ(IUNIT) ! ETA2

      REWIND(IUNIT)

      if(I360.eq.360) then
        IF(CLON2.GT.0.)CLON2=CLON2-360.
        DO J=1,KY2
        DO I=1,KX2
	  IF(HLON2(I,J).GT.0.)HLON2(I,J)=HLON2(I,J)-360.
	  IF(VLON2(I,J).GT.0.)VLON2(I,J)=VLON2(I,J)-360.
        END DO
        END DO
      endif

      ERR=1.e20
      DO J=1,KY0
      DO I=1,KX0
        DIF1=abs(HLON2(1,1)-HLON1(I,J))+abs(HLAT2(1,1)-HLAT1(I,J))
        IF(DIF1.LT.ERR)THEN
          ILOC=I
          JLOC=J
          ERR=DIF1
        END IF
      END DO
      END DO

      I_START(1)=0
      J_START(1)=0
      I_START(2)=ILOC
      J_START(2)=JLOC

      WBD21= WBD0 + (ILOC -1)*2.*DLMD0 + MOD(JLOC+1,2)*DLMD0
      SBD21= SBD0 + (JLOC -1)*DPHD0

      print*,'ILOC,JLOC,ERR=',ILOC,JLOC,ERR
      print*,'WBD0,SBD0,WBD21,SBD21=',WBD0,SBD0,WBD21,SBD21

! READ 6h FORECAST OUTER NEST (d03)

      IUNIT=40+ITIM

      READ(IUNIT)KX,KY,KZ
      print*,'KX,KY,KZ=',KX,KY,KZ

      KX1=KX+1
      KY1=KY+1
      KZ1=KZ+1

      ALLOCATE ( HLON5(KX,KY),HLAT5(KX,KY) )
      ALLOCATE ( VLON5(KX,KY),VLAT5(KX,KY) )

      READ(IUNIT) DLMD5,DPHD5,CLON3,CLAT3 ! DLMD3,DPHD3,CLON3,CLAT3
      READ(IUNIT) ! DT5,PDTOP5
      READ(IUNIT) ! T5
      READ(IUNIT) ! Q5
      READ(IUNIT) ! U5
      READ(IUNIT) ! V5
      READ(IUNIT) ! Z5
      READ(IUNIT) HLON5,HLAT5,VLON5,VLAT5
      READ(IUNIT) ! P5
      READ(IUNIT) ! PD5
      READ(IUNIT) ! ETA1
      READ(IUNIT) ! ETA2

      REWIND(IUNIT)
      
      if(I360.eq.360) then
        IF(CLON3.GT.0.)CLON3=CLON3-360.
        DO J=1,KY
        DO I=1,KX
	  IF(HLON5(I,J).GT.0.)HLON5(I,J)=HLON5(I,J)-360.
	  IF(VLON5(I,J).GT.0.)VLON5(I,J)=VLON5(I,J)-360.
        END DO
        END DO
      endif

      ERR=1.e20
      DO J=1,KY2
      DO I=1,KX2
        DIF1=abs(HLON5(1,1)-HLON2(I,J))+abs(HLAT5(1,1)-HLAT2(I,J))
        IF(DIF1.LT.ERR)THEN
          ILOC=I
          JLOC=J
          ERR=DIF1
        END IF
      END DO
      END DO

      I_START(3)=ILOC
      J_START(3)=JLOC

      print*,'I_START=',I_START
      print*,'J_START=',J_START

      IF(IBGS.EQ.1)THEN

      dc_dist=abs(CLON3-CLON0)+abs(CLAT3-CLAT0)
      if(dc_dist.gt.0.01)then
        print*,'WARNING! domain center is different, check input data'
        CLON3=CLON0
        CLAT3=CLAT0
      end if

      END IF

! create new inner nest grid
! HLAT4,HLON4,VLAT4,VLON4 are the new inner nest grid

      DLMD21=DLMD2
      DPHD21=DPHD2

      WBD4= WBD21 + (ILOC -1)*2.*DLMD21 + MOD(JLOC+1,2)*DLMD21
      SBD4= SBD21 + (JLOC -1)*DPHD21

      print*,'ILOC,JLOC,ERR,WBD4,SBD4=',ILOC,JLOC,ERR,WBD4,SBD4

      
! LON & LAT at T,U,V
        
      ALLOCATE ( HLON4(KX,KY),HLAT4(KX,KY),VLON4(KX,KY),VLAT4(KX,KY) )

       CALL EARTH_LATLON ( HLAT4,HLON4,VLAT4,VLON4,        &  !Earth lat,lon at H and V points
                           DLMD5,DPHD5,WBD4,SBD4,          &  !input res,west & south boundaries,
                           CLAT1,CLON1,                    &  ! central lat,lon, all in degrees
                           1,KX1,1,KY1,1,1,                &
                           1,KX ,1,KY ,1,1,                &
                           1,KX ,1,KY ,1,1         )
        
      if(I360.eq.360) then
        DO J=1,KY
        DO I=1,KX
	  IF(HLON4(I,J).GT.0.)HLON4(I,J)=HLON4(I,J)-360.
	  IF(VLON4(I,J).GT.0.)VLON4(I,J)=VLON4(I,J)-360.
        END DO
        END DO
      endif

       print*,'HLAT4,HLON4,VLAT4,VLON4,1,1=',                  &
               HLAT4(1,1),HLON4(1,1),VLAT4(1,1),VLON4(1,1)
       print*,'HLAT4,HLON4,VLAT4,VLON4,KX,KY=',                  &
               HLAT4(KX,KY),HLON4(KX,KY),VLAT4(KX,KY),VLON4(KX,KY)

          DO J=1,KY
          DO I=1,KX
            DIFFI=abs(HLON4(I,J)-HLON5(I,J))+    &
                  abs(HLAT4(I,J)-HLAT5(I,J))
           IF(DIFFI.GT.0.001)THEN
             print*,'I,J,DIFFI=',I,J,DIFFI
             print*,'HLON54=',HLON5(I,J),HLON4(I,J)    
             print*,'HLAT54=',HLAT5(I,J),HLAT4(I,J)
             STOP 3333
           END IF
          END DO
          END DO

! create 3x high resolution domain

!     JX=9*105-1                   ! hard wired for relocation domain and for the bogus storm
!     JY=9*205-1                   ! only dependent on inner nest grid resolution
      JX=3*(3*84-2)-2              ! hard wired for relocation domain and for the bogus storm
      JY=3*(3*168-2)-2                      ! only dependent on inner nest grid resolution
      JX1=JX+1
      JY1=JY+1

      DLMD3=0.02
      DPHD3=0.02

      print*,'DLMD0,DPHD0=',DLMD0,DPHD0,DLMD3,DPHD3

      iloc=-99
      jloc=-99
      slon0=CLON_NHC
      slat0=CLAT_NHC
      clon05=clon0
      clat05=clat0
!      ratio=9
      ratio=DLMD0/DLMD3
      call Convert_LatLon(1,1,1,slon0,slat0,rlon0,rlat0,clon05,clat05)
      iloc0=nint(rlon0/(dlmd0*2))
      jloc0=nint(rlat0/dphd0)
      print *, 'rlon0=',rlon0, iloc0, dlmd0, 'rlat0=',rlat0, jloc0, dphd0
      iloc =(KX0/2)+iloc0-nint((JX1-2)*0.5/ratio)
      jloc =(KY0/2)+jloc0-nint((JY1-2)*0.5/ratio)
      if(mod(jloc,2).eq.0) then
        jloc=jloc-1
      else
        jloc=jloc
      end if

      print*,'inside merge_nest1_3n,ILOC,JLOC new=',iloc,jloc

! make JLOC odd number

      JLOC=(JLOC/2)*2+1

      WBD3= WBD0 + (ILOC -1)*2.*DLMD0 + MOD(JLOC+1,2)*DLMD0
      SBD3= SBD0 + (JLOC -1)*DPHD0
      
      print*,'ILOC,JLOC=',ILOC,JLOC,WBD3,SBD3,ERR

      write(*,*)'DLMD0,DPHD0=',DLMD0,DPHD0
      write(*,*)'WBD3,SBD3,CLON0,CLAT0=',    &
                 WBD3,SBD3,CLON0,CLAT0

      ALLOCATE ( HLON3(JX,JY),HLAT3(JX,JY) )
      ALLOCATE ( VLON3(JX,JY),VLAT3(JX,JY) )

      ALLOCATE ( T3(JX,JY,KZ),Q3(JX,JY,KZ) )
      ALLOCATE ( U3(JX,JY,KZ),V3(JX,JY,KZ) )
      ALLOCATE ( Z3(JX,JY,KZ1),P3(JX,JY,KZ1) )

      ALLOCATE ( PD3(JX,JY), PMID3(JX,JY,KZ) )

      ALLOCATE ( SLP3(JX,JY) )
      ALLOCATE ( ZS3(JX,JY),TS3(JX,JY),QS3(JX,JY) )

      ALLOCATE ( IIH(JX,JY,4),JJH(JX,JY,4) )
      ALLOCATE ( IIV(JX,JY,4),JJV(JX,JY,4) )
      ALLOCATE ( HBWGT(JX,JY,4),VBWGT(JX,JY,4) )

      ALLOCATE ( IIH1(JX,JY,4),JJH1(JX,JY,4) )
      ALLOCATE ( IIV1(JX,JY,4),JJV1(JX,JY,4) )
      ALLOCATE ( HBWGT1(JX,JY,4),VBWGT1(JX,JY,4) )

      ALLOCATE ( IIH2(JX,JY,4),JJH2(JX,JY,4) )
      ALLOCATE ( IIV2(JX,JY,4),JJV2(JX,JY,4) )
      ALLOCATE ( HBWGT2(JX,JY,4),VBWGT2(JX,JY,4) )

      ALLOCATE ( A13(JX,JY),B13(JX,JY),C13(JX,JY) )

! working array

      ALLOCATE ( T21(JX,JY,KZ,4),Q21(JX,JY,KZ,4) )
      ALLOCATE ( U21(JX,JY,KZ,4),V21(JX,JY,KZ,4) )
      ALLOCATE ( SLP21(JX,JY,4) )
      ALLOCATE ( PMV3(JX,JY,KZ) )

! LON & LAT at T,U,V

       CALL EARTH_LATLON ( HLAT3,HLON3,VLAT3,VLON3,        &  !Earth lat,lon at H and V points
                           DLMD3,DPHD3,WBD3,SBD3,          &  !input res,west & south boundaries,
                           CLAT0,CLON0,                    &  ! central lat,lon, all in degrees
                           1,JX1,1,JY1,1,1,                &  
                           1,JX ,1,JY ,1,1,                &
                           1,JX ,1,JY ,1,1         )

      if(I360.eq.360) then
        DO J=1,JY
        DO I=1,JX
	  IF(HLON3(I,J).GT.0.)HLON3(I,J)=HLON3(I,J)-360.
	  IF(VLON3(I,J).GT.0.)VLON3(I,J)=VLON3(I,J)-360.
        END DO
        END DO
      endif

       print*,'HLAT3,HLON3,VLAT3,VLON3,1,1=',                  &
               HLAT3(1,1),HLON3(1,1),VLAT3(1,1),VLON3(1,1)
       print*,'HLAT3,HLON3,VLAT3,VLON3,JX,JY=',                  &
               HLAT3(JX,JY),HLON3(JX,JY),VLAT3(JX,JY),VLON3(JX,JY)

! find the grid index at the lower left corner
! double check

! inner nest vs. parent (40x40)

      ERR=1.e20
      DO J=1,KY0
      DO I=1,KX0
        DIF1=abs(HLON4(1,1)-HLON0(I,J))+abs(HLAT4(1,1)-HLAT0(I,J))
        IF(DIF1.LT.ERR)THEN
          I_ST=I
          J_ST=J
          ERR=DIF1
        END IF
      END DO
      END DO
                 
      print*,'Large ERR should be OK'
      PRINT*,'nest vs. parent, I_ST,J_ST,ERR 1=',I_ST,J_ST,ERR 

! 3x vs. parent

      ERR=1.e20
      DO J=1,KY0
      DO I=1,KX0
        DIF1=abs(HLON3(1,1)-HLON1(I,J))+abs(HLAT3(1,1)-HLAT1(I,J))
        IF(DIF1.LT.ERR)THEN
          I_ST4=I
          J_ST4=J
          ERR=DIF1
        END IF
      END DO
      END DO

      print*,'ERR should be ver small'
      PRINT*,'3x vs. parent, I_ST4,J_ST4,ERR 2=',I_ST4,J_ST4,ERR

! inner nest vs. 3x

      FLON=HLON4(1,1)
      FLAT=HLAT4(1,1)

      print*,'FLON,FLAT=',FLON,FLAT

      ERR=1.e20
      DO J=1,JY
      DO I=1,JX
        DIF1=abs(FLON-HLON3(I,J))+abs(FLAT-HLAT3(I,J))
        IF(DIF1.LT.ERR)THEN
          I_ST14=I
          J_ST14=J
          ERR=DIF1
        END IF
      END DO
      END DO

      print*,'ERR should be ver small'
      PRINT*,'3x vs. nest, I_ST14,J_ST14,ERR=',I_ST14,J_ST14,ERR

      IF(IBGS.EQ.1.and.abs(DLMD5/DLMD3-1.).lt.0.01)THEN

          DO J=1,KY
          DO I=1,KX
            I1=I+I_ST14-1
            J1=J+J_ST14-1
            DIFFI=abs(HLON4(I,J)-HLON3(I1,J1))+    &
                  abs(HLAT4(I,J)-HLAT3(I1,J1))
           IF(DIFFI.GT.0.001)THEN
             print*,'I,J,DIFFI=',I,J,DIFFI
             print*,'I1,J1=',I1,J1,I_ST14,J_ST14
             print*,'HLON34=',HLON3(I,J),HLON4(I1,J1)    
             print*,'HLAT34=',HLAT3(I,J),HLAT4(I1,J1)
             STOP 33
           END IF
          END DO
          END DO
    
      END IF

     DEALLOCATE ( HLON1,HLAT1 )

! READ PARENT DATA (d01)   ! 6 hour forecast data

      IUNIT=20+ITIM

      READ(IUNIT) NX,NY,NZ

      print*,'NX,NY,NZ=',NX,NY,NZ

      NX1=NX+1
      NY1=NY+1
      NZ1=NZ+1

      ALLOCATE ( T1(NX,NY,NZ),Q1(NX,NY,NZ) )
      ALLOCATE ( U1(NX,NY,NZ),V1(NX,NY,NZ) )
      ALLOCATE ( Z1(NX,NY,NZ1),P1(NX,NY,NZ1) )
      ALLOCATE ( HLON1(NX,NY),HLAT1(NX,NY),VLON1(NX,NY),VLAT1(NX,NY) )
      ALLOCATE ( PD1(NX,NY),A101(NX,NY),B101(NX,NY) )
      ALLOCATE ( ETA1(KZ1),ETA2(KZ1) )

      READ(IUNIT) DLMD1,DPHD1,CLON1,CLAT1
      READ(IUNIT) PT1,PDTOP1
      READ(IUNIT) T1
      READ(IUNIT) Q1
      READ(IUNIT) U1
      READ(IUNIT) V1
      READ(IUNIT) Z1
      READ(IUNIT) HLON1,HLAT1   !,VLON1,VLAT1
      READ(IUNIT) P1
      READ(IUNIT) PD1
      READ(IUNIT) ETA1
      READ(IUNIT) ETA2


!      IF(IBGS.NE.2)THEN

        print*,'reading A101'
        READ(IUNIT) A101                        ! A101 = land sea mask, B101 = ZNT
        READ(IUNIT) B101
        print*,'finishing reading A101'

!      END IF

      REWIND(IUNIT)
      WRITE(IUNIT) NX,NY,NZ,I360
      WRITE(IUNIT) DLMD1,DPHD1,CLON1,CLAT1
      WRITE(IUNIT) PT1,PDTOP1
      WRITE(IUNIT) T1
      WRITE(IUNIT) Q1
      WRITE(IUNIT) U1
      WRITE(IUNIT) V1
      WRITE(IUNIT) Z1
      WRITE(IUNIT) HLON0,HLAT0,VLON0,VLAT0
      WRITE(IUNIT) P1
      WRITE(IUNIT) PD1
      WRITE(IUNIT) ETA1
      WRITE(IUNIT) ETA2
      WRITE(IUNIT) A101                        ! A101 = land sea mask, B101 = ZNT
      WRITE(IUNIT) B101
 
      CLOSE(IUNIT)

      HLON1=HLON0
      HLAT1=HLAT0
      VLON1=VLON0
      VLAT1=VLAT0 
      
      DEALLOCATE ( HLON0,HLAT0,VLON0,VLAT0 )

       print*,'read in d01, HLON1,HLAT1,VLON1,VLAT1=',     &
              HLON1(1,1),HLAT1(1,1),VLON1(1,1),VLAT1(1,1)
!
! READ INNER NEST DATA (d02)   ! 6 hour forecast data

      IUNIT=30+ITIM

      READ(IUNIT) IX,IY,IZ              ! IZ==NZ

      print*,'IX,IY,IZ=',IX,IY,IZ

      IX1=IX+1
      IY1=IY+1
      IZ1=IZ+1

      ALLOCATE ( T2(IX,IY,IZ),Q2(IX,IY,IZ) )
      ALLOCATE ( U2(IX,IY,IZ),V2(IX,IY,IZ) )
      ALLOCATE ( Z2(IX,IY,IZ1),P2(IX,IY,IZ1) )
      ALLOCATE ( A102(IX,IY),B102(IX,IY),C102(IX,IY) )
      ALLOCATE ( PD2(IX,IY) )

!       A102=1.                 ! all ocean
!       B102=0.002
!       C102=0.88
 
      READ(IUNIT) DLMD2,DPHD2   !,CLON2,CLAT2
      READ(IUNIT) PT2,PDTOP2
      READ(IUNIT) T2
      READ(IUNIT) Q2
      READ(IUNIT) U2
      READ(IUNIT) V2
      READ(IUNIT) Z2
      READ(IUNIT)    ! HLON2,HLAT2,VLON2,VLAT2
      READ(IUNIT) P2
      READ(IUNIT) PD2
      READ(IUNIT) ETA1
      READ(IUNIT) ETA2

      READ(IUNIT) A102
      READ(IUNIT) B102
      READ(IUNIT) C102
    
      IF(I360.eq.360)THEN

        REWIND(IUNIT)
        WRITE(IUNIT) IX,IY,IZ              ! IZ==NZ
        WRITE(IUNIT) DLMD2,DPHD2,CLON2,CLAT2
        WRITE(IUNIT) PT2,PDTOP2
        WRITE(IUNIT) T2
        WRITE(IUNIT) Q2
        WRITE(IUNIT) U2
        WRITE(IUNIT) V2
        WRITE(IUNIT) Z2
        WRITE(IUNIT) HLON2,HLAT2,VLON2,VLAT2
        WRITE(IUNIT) P2
        WRITE(IUNIT) PD2
        WRITE(IUNIT) ETA1
        WRITE(IUNIT) ETA2
        WRITE(IUNIT) A102
        WRITE(IUNIT) B102
        WRITE(IUNIT) C102

      END IF

      DO J=1,IY
      DO I=1,IX
        wind_s=sqrt(U2(I,J,1)**2+V2(I,J,1)**2)+1.E-10
        C102(I,J)=min(1.0,C102(I,J)/wind_s)
      END DO
      END DO

      CLOSE(IUNIT)
!

      print*,'read in d02, HLON2,HLAT2,VLON2,VLAT2=',     &
              HLON2(1,1),HLAT2(1,1),VLON2(1,1),VLAT2(1,1)

! READ 6h FORECAST OUTER NEST (d03)

      IUNIT=40+ITIM

      READ(IUNIT)NX5,NY5,NZ5
      print*,'NX5,NY5,NZ5=',NX5,NY5,NZ5

      ALLOCATE ( T5(NX5,NY5,NZ5),Q5(NX5,NY5,NZ5) )
      ALLOCATE ( U5(NX5,NY5,NZ5),V5(NX5,NY5,NZ5) )
      ALLOCATE ( Z5(NX5,NY5,NZ1),P5(NX5,NY5,NZ1) )
      ALLOCATE ( A105(NX5,NY5),B105(NX5,NY5),C105(NX5,NY5) )
      ALLOCATE ( PD5(NX5,NY5) )

!       A105=1.                 ! all ocean
!       B105=0.002
!       C105=0.88

      READ(IUNIT) DLMD5,DPHD5,CLON5,CLAT5
      READ(IUNIT) DT5,PDTOP5
      READ(IUNIT) T5
      READ(IUNIT) Q5
      READ(IUNIT) U5
      READ(IUNIT) V5
      READ(IUNIT) Z5
      READ(IUNIT)   ! HLON5,HLAT5,VLON5,VLAT5
      READ(IUNIT) P5
      READ(IUNIT) PD5
      READ(IUNIT) ETA1
      READ(IUNIT) ETA2
      READ(IUNIT) A105
      READ(IUNIT) B105
      READ(IUNIT) C105

      IF(I360.eq.360)THEN

        IF(CLON5.GT.0.)CLON5=CLON5-360.

        REWIND(IUNIT)
        WRITE(IUNIT) NX5,NY5,NZ5
        WRITE(IUNIT) DLMD5,DPHD5,CLON5,CLAT5
        WRITE(IUNIT) DT5,PDTOP5
        WRITE(IUNIT) T5
        WRITE(IUNIT) Q5
        WRITE(IUNIT) U5
        WRITE(IUNIT) V5
        WRITE(IUNIT) Z5
        WRITE(IUNIT) HLON5,HLAT5,VLON5,VLAT5
        WRITE(IUNIT) P5
        WRITE(IUNIT) PD5
        WRITE(IUNIT) ETA1
        WRITE(IUNIT) ETA2
        WRITE(IUNIT) A105
        WRITE(IUNIT) B105
        WRITE(IUNIT) C105

      END IF

      DO J=1,NY5
      DO I=1,NX5
        wind_s=sqrt(U5(I,J,1)**2+V5(I,J,1)**2)+1.E-10
        C105(I,J)=min(1.0,C105(I,J)/wind_s)
      END DO
      END DO

      CLOSE(IUNIT)

       print*,'read in d03, HLON5,HLAT5,VLON5,VLAT5=',     &
               HLON5(1,1),HLAT5(1,1),VLON5(1,1),VLAT5(1,1)


      ERR=1.e20
      DO J=1,NY
      DO I=1,NX
        DIF1=abs(HLON2(1,1)-HLON1(I,J))+abs(HLAT2(1,1)-HLAT1(I,J))
        IF(DIF1.LT.ERR)THEN
          ILOC=I
          JLOC=J
          ERR=DIF1
        END IF
      END DO
      END DO

      print*,'new test11111'

      do k=1,kz1
        print*,k,p1(nx/2,ny/2,k),p2(ix/2,iy/2,k),p5(nx5/2,ny5/2,k)
      end do


       WBD1=-(NX-1)*DLMD1                 ! PARENT wbd
       SBD1=-(NY/2)*DPHD1       ! PARENT SBD

      WBD2= WBD1 + (ILOC -1)*2.*DLMD1 + MOD(JLOC+1,2)*DLMD1
      SBD2= SBD1 + (JLOC -1)*DPHD1

!      print*,'ERR should be ver small'
      PRINT*,'ILOC,JLOC,ERR=',ILOC,JLOC,ERR

      ERR=1.e20
      DO J=1,IY
      DO I=1,IX
        DIF1=abs(HLON5(1,1)-HLON2(I,J))+abs(HLAT5(1,1)-HLAT2(I,J))
        IF(DIF1.LT.ERR)THEN
          ILOC=I
          JLOC=J
          ERR=DIF1
        END IF
      END DO
      END DO

      print*,'ERR should be ver small'
      PRINT*,'ILOC,JLOC,ERR=',ILOC,JLOC,ERR

      WBD5= WBD2 + (ILOC -1)*2.*DLMD2 + MOD(JLOC+1,2)*DLMD2
      SBD5= SBD2 + (JLOC -1)*DPHD2


      IF(IBGS.EQ.1.and.abs(DLMD5/DLMD3-1.).lt.0.01)THEN

         print*,'check if the nest domain created correctly'

         DO J=1,NY5
         DO I=1,NX5
           DIFFI=abs(HLON4(I,J)-HLON5(I,J))+    &
                 abs(HLAT4(I,J)-HLAT5(I,J))
           IF(DIFFI.GT.0.001)THEN
             print*,'I,J,DIFFI=',I,J,DIFFI
             STOP 333
           END IF
         END DO
         END DO

         print*,'finished checking nest domain created correctly'

      END IF

!!!!!!!!!!!!!!!!!!!!

       print*,'NX,NY,KZ=',NX,NY,KZ
       write(*,*)'inside merge K,T1,Q1,U1,V1,Z1,P1='
       do k=1,kz
         write(*,32)K,T1(9,9,K),            &
           Q1(9,9,K),U1(9,9,K),V1(9,9,K),Z1(9,9,K),P1(9,9,K)
       end do

       print*,'IX,IY,KZ=',IX,IY,KZ
       write(*,*)'inside merge K,T2,Q2,U2,V2,Z2,P2='
       do k=1,kz
         write(*,32)K,T2(9,9,K),            &
           Q2(9,9,K),U2(9,9,K),V2(9,9,K),Z2(9,9,K),P2(9,9,K)
       end do

       print*,'NX5,NY5,KZ=',NX5,NY5,KZ
       write(*,*)'inside merge K,T5,Q5,U5,V5,Z5,P5='
       do k=1,kz
         write(*,32)K,T5(9,9,K),            &
           Q5(9,9,K),U5(9,9,K),V5(9,9,K),Z5(9,9,K),P5(9,9,K)
       end do

      PT3=PT1
      PDTOP3=PDTOP1


      print*,'DLMD1,DPHD1,PT1,PDTOP1=',DLMD1,DPHD1,PT1,PDTOP1
      print*,'HLAT1,HLON1=',HLAT1(1,1),HLON1(1,1)


      ALLOCATE ( SLP1(NX,NY) )
      ALLOCATE ( PMID1(NX,NY,NZ),ZMID1(NX,NY,NZ) )
      ALLOCATE ( PMV1(NX,NY,NZ) )

       DO K=1,NZ
       DO J=1,NY
       DO I=1,NX
         PMID1(I,J,K)=EXP((ALOG(P1(I,J,K))+ALOG(P1(I,J,K+1)))*0.5)
         ZMID1(I,J,K)=0.5*(Z1(I,J,K)+Z1(I,J,K+1))
       ENDDO
       ENDDO
       ENDDO

!C        COMPUTE SEA LEVEL PRESSURE.
!C
       DO J=1,NY
       DO I=1,NX
         ZSF1 = ZMID1(I,J,1)
         PSF1 = PMID1(I,J,1)
         TV1 = T1(I,J,1)*(1.+D608*Q1(I,J,1))
         A = (GAMMA * ZSF1) / TV1
         SLP1(I,J) = PSF1*(1+A)**COEF2
      ENDDO
      ENDDO

      print *,'fort.63','NX=',NX,'NY=',NY

!      WRITE(63)((SLP1(I,J),I=1,NX),J=1,NY,2)
!      DO K=1,NZ+1
!        WRITE(63)((Z1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,NZ+1
!        WRITE(63)((P1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,NZ
!        WRITE(63)((T1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,NZ
!        WRITE(63)((Q1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,NZ
!        WRITE(63)((U1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,NZ
!        WRITE(63)((V1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO

!  parameters for the outer nest

     print*,'before call, HLAT1,HLON1=',HLAT1(1,1),HLON1(1,1)

!     CALL EARTH_LATLON (HLAT1,HLON1,VLAT1,VLON1,         &
!                        DLMD1,DPHD1,WBD1,SBD1,           &
!                        CLAT1,CLON1,               &
!                        1,NX1,1,NY1,1,1,                 &
!                        1,NX ,1,NY ,1,1,                 &
!                        1,NX ,1,NY ,1,1         )

     print*,'WBD1,SBD1,NX,NY=',WBD1,SBD1,NX,NY
     print*,'after call, HLAT1,HLON1=',HLAT1(1,1),HLON1(1,1)

      print*,'CLON1,CLAT1=CLON0,CLAT0',CLON1,CLAT1,CLON0,CLAT0
      print*,'Qingfu BD1,SBD1,NX,NY=',WBD1,SBD1,NX,NY
      IIH=999999
      JJH=999999
      IIV=999999
      JJV=999999
      HBWGT=0.
      VBWGT=0.      
 
!      RATIO=9
      RATIO=DLMD1/DLMD3
      CURRENT_DOMAIN_ID=1
      DX_S=DLMD1
      DY_S=DPHD1

      print*,'DX_S,DY_S, 1=',DX_S,DY_S
!
        CALL G2T2H_egrid( IIH,JJH,                            & ! output grid index and weights
                          HBWGT,                              & ! output weights in terms of parent grid
                          CLAT1, CLON1,                       & ! input source domain center
                          CURRENT_DOMAIN_ID,                  & ! input source domain ID
                          DX_S,DY_S,                          & ! input source outmost domain resolution
                          HLAT1,HLON1,                        & ! input source point regular lat and lon
                          HLAT3,HLON3,                        & ! input target point regular lat and lon
                          1,NX,1,NY,                          & ! input source outmost domain dimensions
                          1,NX,1,NY,                          & ! source dimensions
                          1,JX,1,JY                    )        ! target dimensions


        CALL G2T2V_egrid( IIV,JJV,                            & ! output grid index and weights
                          VBWGT,                              & ! output weights in terms of parent grid
                          CLAT1, CLON1,                       & ! input source domain center(-6h)
                          CURRENT_DOMAIN_ID,                  & ! input source domain ID
                          DX_S,DY_S,                          & ! input source outmost domain resolution
                          VLAT1,VLON1,                        & ! input source point regular lat and lon
                          VLAT3,VLON3,                        & ! input target point regular lat and lon
                          1,NX,1,NY,                          & ! input source outmost domain dimensions
                          1,NX,1,NY,                          & ! source dimensions
                          1,JX,1,JY                    )        ! target dimensions
!

       ZS3=0.

       DO J=1,JY
       DO I=1,JX
         DO N1=1,4
           IH1(N1)=IIH(I,J,N1)
           JH1(N1)=JJH(I,J,N1)
         END DO
         do iq=1,4
            if(IH1(iq)<1 .or. IH1(iq)>=JX .or. JH1(iq)<1 .or. JH1(iq)>=JY) then
               write(0,*) 'BAD BAD!  (IH1(iq),JH1(iq)) outside of (1..JX,1..JY) bound: iq,IH1,JH1=',iq,IH1(iq),JH1(iq)
               write(6,*) 'BAD BAD!  (IH1(iq),JH1(iq)) outside of (1..JX,1..JY) bound: iq,IH1,JH1=',iq,IH1(iq),JH1(iq)
               stop 19
            endif
         enddo
            
         ZS3(I,J) = HBWGT(I,J,1)*Z1(IH1(1),JH1(1),1)             &
                  + HBWGT(I,J,2)*Z1(IH1(2),JH1(2),1)             &
                  + HBWGT(I,J,3)*Z1(IH1(3),JH1(3),1)             &
                  + HBWGT(I,J,4)*Z1(IH1(4),JH1(4),1)
       ENDDO
       ENDDO

       DO J=1,JY
       DO I=1,JX
         Z3(I,J,1)=ZS3(I,J)
       END DO
       END DO

       print*,'test2='

       DO K=1,IZ
         v_maxk=0.
         DO J=1,IY
         DO I=1,IX
  	   v_max1=U2(I,J,K)*U2(I,J,K)+V2(I,J,K)*V2(I,J,K)
	   if(v_maxk.lt.v_max1)v_maxk=v_max1
	 END DO
	 END DO
	 print*,'native grid data K,v_maxk=',K,sqrt(v_maxk)
       END DO

      ALLOCATE ( SLP2(IX,IY) )
      ALLOCATE ( PMID2(IX,IY,IZ),ZMID2(IX,IY,IZ) )
      ALLOCATE ( PMV2(IX,IY,IZ) )

       DO K=1,IZ
       DO J=1,IY
       DO I=1,IX
         PMID2(I,J,K)=EXP((ALOG(P2(I,J,K))+ALOG(P2(I,J,K+1)))*0.5)
         ZMID2(I,J,K)=0.5*(Z2(I,J,K)+Z2(I,J,K+1))
       ENDDO
       ENDDO
       ENDDO

       DO J=1,IY
       DO I=1,IX
         ZSF2 = ZMID2(I,J,1)
         PSF2 = PMID2(I,J,1)
         TV2 = T2(I,J,1)*(1.+D608*Q2(I,J,1))
!C
!C        COMPUTE SEA LEVEL PRESSURE.
         A = (GAMMA * ZSF2) / TV2
         SLP2(I,J) = PSF2*(1.+A)**COEF2
      ENDDO
      ENDDO

       print *,'fort.62','IX=',IX,'IY=',IY

!      WRITE(62)((SLP2(I,J),I=1,IX),J=1,IY,2)
!      DO K=1,IZ+1
!        WRITE(62)((Z2(I,J,K),I=1,IX),J=1,IY,2)
!      END DO
!      DO K=1,IZ+1
!        WRITE(62)((P2(I,J,K),I=1,IX),J=1,IY,2)
!      END DO
!      DO K=1,IZ
!        WRITE(62)((T2(I,J,K),I=1,IX),J=1,IY,2)
!      END DO
!      DO K=1,IZ
!        WRITE(62)((Q2(I,J,K),I=1,IX),J=1,IY,2)
!      END DO
!      DO K=1,IZ
!        WRITE(62)((U2(I,J,K),I=1,IX),J=1,IY,2)
!      END DO
!      DO K=1,IZ
!        WRITE(62)((V2(I,J,K),I=1,IX),J=1,IY,2)
!      END DO

! variables from d03

      print*,'test1 qqqqq'
!
      print*,'test2 qqqqq'

      ALLOCATE ( SLP5(NX5,NY5) )
      ALLOCATE ( PMID5(NX5,NY5,NZ5),ZMID5(NX5,NY5,NZ5) )
      ALLOCATE ( PMV5(NX5,NY5,NZ5) )

      print*,'test21 qqqqq'
       DO K=1,NZ5
       DO J=1,NY5
       DO I=1,NX5
         PMID5(I,J,K)=EXP((ALOG(P5(I,J,K))+ALOG(P5(I,J,K+1)))*0.5)
         ZMID5(I,J,K)=0.5*(Z5(I,J,K)+Z5(I,J,K+1))
       ENDDO
       ENDDO
       ENDDO

      print*,'test3 qqqqq'
       DO J=1,NY5
       DO I=1,NX5
         ZSF5 = ZMID5(I,J,1)
         PSF5 = PMID5(I,J,1)
         TV5 = T5(I,J,1)*(1.+D608*Q5(I,J,1))
!C
!C        COMPUTE SEA LEVEL PRESSURE.
         A = (GAMMA * ZSF5) / TV5
         SLP5(I,J) = PSF5*(1.+A)**COEF2
      ENDDO
      ENDDO

      print*,'test4 qqqqq'
      NX22=NX5/2
      NY22=NY5/2
      print*,'SLP5,T5,Z5,P5=',SLP5(NX22,NY22),T5(NX22,NY22,1),     &
              Q5(NX22,NY22,1),Z5(NX22,NY22,1),P5(NX22,NY22,1)

!      WRITE(62)((SLP2(I,J),I=1,IX),J=1,IY,2)
!      DO K=1,IZ+1
!        WRITE(62)((Z2(I,J,K),I=1,IX),J=1,IY,2)
!      END DO
!      DO K=1,IZ+1
!        WRITE(62)((P2(I,J,K),I=1,IX),J=1,IY,2)
!      END DO
!      DO K=1,IZ
!        WRITE(62)((T2(I,J,K),I=1,IX),J=1,IY,2)
!      END DO
!      DO K=1,IZ
!        WRITE(62)((Q2(I,J,K),I=1,IX),J=1,IY,2)
!      END DO
!      DO K=1,IZ
!        WRITE(62)((U2(I,J,K),I=1,IX),J=1,IY,2)
!      END DO
!      DO K=1,IZ
!        WRITE(62)((V2(I,J,K),I=1,IX),J=1,IY,2)
!      END DO

! compute interpolation for outer  nest

       NX_1=NX-2
       NY_1=NY-2              

! First, compute variables at surface level

!$omp parallel do &
!$omp& private(i,j,k,n1,DZ1,ZDIF1,FACT1,IH1,JH1,K1)
       DO J=1,JY
       CYC_40: DO I=1,JX
         IF(IIH(I,J,1).LT.1.or.IIH(I,J,1).GT.NX_1)CYCLE CYC_40
         IF(JJH(I,J,1).LE.2.or.JJH(I,J,1).GE.NY_1)CYCLE CYC_40
         DO N1=1,4
           IH1(N1)=IIH(I,J,N1)
           JH1(N1)=JJH(I,J,N1)
         END DO
         DO N1=1,4
           SLP21(I,J,N1)=SLP1(IH1(N1),JH1(N1))
         END DO
         K=1             ! surface
         CYC_38: DO N1=1,4
           IF(Z3(I,J,K).LT.ZMID1(IH1(N1),JH1(N1),1))THEN
             DZ1=ZMID1(IH1(N1),JH1(N1),1)-Z3(I,J,K)
             T21(I,J,K,N1)=T1(IH1(N1),JH1(N1),1)+GAMMA*DZ1
             Q21(I,J,K,N1)=Q1(IH1(N1),JH1(N1),1) 
           ELSE IF(Z3(I,J,K).GE.ZMID1(IH1(N1),JH1(N1),NZ))THEN  ! never occur for K=1
             DZ1=ZMID1(IH1(N1),JH1(N1),NZ)-Z3(I,J,K)
             T21(I,J,K,N1)=T1(IH1(N1),JH1(N1),NZ)+GAMMA*DZ1
             Q21(I,J,K,N1)=Q1(IH1(N1),JH1(N1),NZ)
           ELSE
             DO K1=2,NZ
               ZDIF1=ZMID1(IH1(N1),JH1(N1),K1)-Z3(I,J,K)
               IF(ZDIF1.GE.0.)THEN
                 FACT1=ZDIF1/(ZMID1(IH1(N1),JH1(N1),K1)-   &
                              ZMID1(IH1(N1),JH1(N1),K1-1))
                 T21(I,J,K,N1)=T1(IH1(N1),JH1(N1),K1)*(1.-FACT1)     &
                              +T1(IH1(N1),JH1(N1),K1-1)*FACT1
                 Q21(I,J,K,N1)=Q1(IH1(N1),JH1(N1),K1)*(1.-FACT1)     &
                              +Q1(IH1(N1),JH1(N1),K1-1)*FACT1
                 CYCLE CYC_38
               END IF
             END DO
           END IF
         END DO CYC_38
       ENDDO CYC_40
       ENDDO

       print*,'test3='

!$omp parallel do &
!$omp& private(i,j)
       DO J=1,JY
       CYC_44: DO I=1,JX
         IF(IIH(I,J,1).LT.1.or.IIH(I,J,1).GT.NX_1)CYCLE CYC_44
         IF(JJH(I,J,1).LE.2.or.JJH(I,J,1).GE.NY_1)CYCLE CYC_44
         SLP3(I,J) = HBWGT(I,J,1)*SLP21(I,J,1)             &
                   + HBWGT(I,J,2)*SLP21(I,J,2)             &
                   + HBWGT(I,J,3)*SLP21(I,J,3)             &
                   + HBWGT(I,J,4)*SLP21(I,J,4)
         TS3(I,J)  = HBWGT(I,J,1)*T21(I,J,1,1)             &
                   + HBWGT(I,J,2)*T21(I,J,1,2)             &
                   + HBWGT(I,J,3)*T21(I,J,1,3)             &
                   + HBWGT(I,J,4)*T21(I,J,1,4)
         QS3(I,J)  = HBWGT(I,J,1)*Q21(I,J,1,1)             &
                   + HBWGT(I,J,2)*Q21(I,J,1,2)             &
                   + HBWGT(I,J,3)*Q21(I,J,1,3)             &
                   + HBWGT(I,J,4)*Q21(I,J,1,4)
       ENDDO CYC_44
       ENDDO

!      print*,'1st merge,488,744=',SLP3(488,744),TS3(488,744),QS3(488,744)

      SP_max=-1.e20
      SP_min=1.e20
      DO J=1,NY
      DO I=1,NX
        IF(SP_max.lt.SLP1(i,j))then
          SP_max=SLP1(i,j)
        end if
        if(SP_min.gt.SLP1(i,j))then
           SP_min=SLP1(i,j)
        end if
      END DO
      END DO
      print*,'SP1_max,SP1_min=',SP_max,SP_min

      SP_max=-1.e20
      SP_min=1.e20
      DO J=1,JY
      DO I=1,JX
        IF(SP_max.lt.SLP3(i,j))then
          SP_max=SLP3(i,j)
        end if
        if(SP_min.gt.SLP3(i,j))then
           SP_min=SLP3(i,j)
        end if
      END DO
      END DO
      print*,'SP_max,SP_min=',SP_max,SP_min

!      DO J=1,JY
!      DO I=1,JX
!        IF(I.EQ.J)THEN
!          PRINT*,'I,J,SLP3,TS3,QS3=',I,J,SLP3(I,J),TS3(I,J),QS3(I,J)
!        END IF
!      END DO
!      END DO

! compute interpolation for d02

!      IF(IVOBS.LT.IVOBS_CUT)GO TO 888

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      NDX=0
      NDY=0

      IXDX=IX+NDX
      IYDY=IY+NDY

!      WBD2=WBD2-NDX*DLMD2                ! WBD2=WBD2-DLMD2*2*NDX/2
!      SBD2=SBD2-NDY*DPHD2/2

 
      IIH1=999999
      JJH1=999999
      IIV1=999999
      JJV1=999999
      HBWGT1=0.
      VBWGT1=0.

!      RATIO=3
      RATIO=DLMD2/DLMD3
      CURRENT_DOMAIN_ID=2
      DX_S=DLMD2
      DY_S=DPHD2

       print*,'DX_S,DY_S, 2=',DX_S,DY_S

        CALL G2T2H_egrid( IIH1,JJH1,                          & ! output grid index and weights
                          HBWGT1,                             & ! output weights in terms of parent grid
                          CLAT1, CLON1,                       & ! input source domain center
                          CURRENT_DOMAIN_ID,                  & ! input source domain ID
                          DX_S,DY_S,                          & ! input source outmost domain resolution
                          HLAT2,HLON2,                        & ! input source point regular lat and lon
                          HLAT3,HLON3,                        & ! input target point regular lat and lon
                          1,IXDX,1,IYDY,                      & ! input source outmost domain dimensions
                          1,IXDX,1,IYDY,                      & ! source dimensions
                          1,JX,1,JY                    )        ! target dimensions


        CALL G2T2V_egrid( IIV1,JJV1,                          & ! output grid index and weights
                          VBWGT1,                             & ! output weights in terms of parent grid
                          CLAT1, CLON1,                       & ! input source domain center(-6h)
                          CURRENT_DOMAIN_ID,                  & ! input source domain ID
                          DX_S,DY_S,                          & ! input source outmost domain resolution
                          VLAT2,VLON2,                        & ! input source point regular lat and lon
                          VLAT3,VLON3,                        & ! input target point regular lat and lon
                          1,IXDX,1,IYDY,                      & ! input source outmost domain dimensions
                          1,IXDX,1,IYDY,                      & ! source dimensions
                          1,JX,1,JY                    )        ! target dimensions


       print*,'IIH1(1,1,1),JJH1(1,1,1)=',IIH1(1,1,1),JJH1(1,1,1)
       print*,'IIH1(JX,JY,1),JJH1(JX,JY,1)=',IIH1(JX,JY,1),JJH1(JX,JY,1)
       print*,'IIV1(1,1,1),JJV1(1,1,1)=',IIV1(1,1,1),JJV1(1,1,1)
       print*,'IIV1(JX,JY,1),JJV1(JX,JY,1)=',IIV1(JX,JY,1),JJV1(JX,JY,1)
               
       print*,'test66'
    
! replace the topgraphy data for the inner nest

       A13=1.                 ! all ocean
       B13=0.002
       C13=0.88

         IX_1=IX-2
         IY_1=IY-2

!$omp parallel do &
!$omp& private(i,j,N1,IH1,JH1)
         DO J=1,JY
         CYC_34: DO I=1,JX
           IF(IIH1(I,J,1).LT.2.or.IIH1(I,J,1).GT.IX_1)CYCLE CYC_34
           IF(JJH1(I,J,1).LE.2.or.JJH1(I,J,1).GE.IY_1)CYCLE CYC_34
           DO N1=1,4
             IH1(N1)=IIH1(I,J,N1)
             JH1(N1)=JJH1(I,J,N1)
           END DO
           ZS3(I,J) = HBWGT1(I,J,1)*Z2(IH1(1),JH1(1),1)             &
                    + HBWGT1(I,J,2)*Z2(IH1(2),JH1(2),1)             &
                    + HBWGT1(I,J,3)*Z2(IH1(3),JH1(3),1)             &
                    + HBWGT1(I,J,4)*Z2(IH1(4),JH1(4),1)
         END DO CYC_34
         END DO

       DO J=1,JY
       DO I=1,JX
         Z3(I,J,1)=ZS3(I,J)
       END DO
       END DO

       IF(IVOBS.LT.IVOBS_CUT)GO TO 887
             
!!!!!!!!!!!!!

!$omp parallel do &
!$omp& private(i,j,k,n1,DZ1,ZDIF1,FACT1,IH1,JH1,K1)
       DO J=1,JY
       CYC_45: DO I=1,JX
         IF(IIH1(I,J,1).LT.2.or.IIH1(I,J,1).GT.IX_1)CYCLE CYC_45
         IF(JJH1(I,J,1).LE.2.or.JJH1(I,J,1).GE.IY_1)CYCLE CYC_45
         DO N1=1,4
           IH1(N1)=IIH1(I,J,N1)
           JH1(N1)=JJH1(I,J,N1)
         END DO
         DO N1=1,4
           SLP21(I,J,N1)=SLP2(IH1(N1),JH1(N1))
         END DO
         K=1             ! surface
         CYC_39: DO N1=1,4
           IF(Z3(I,J,K).LT.ZMID2(IH1(N1),JH1(N1),1))THEN
             DZ1=ZMID2(IH1(N1),JH1(N1),1)-Z3(I,J,K)
             T21(I,J,K,N1)=T2(IH1(N1),JH1(N1),1)+GAMMA*DZ1
             Q21(I,J,K,N1)=Q2(IH1(N1),JH1(N1),1)
           ELSE IF(Z3(I,J,K).GE.ZMID2(IH1(N1),JH1(N1),NZ))THEN  ! never occur for K=1
             DZ1=ZMID2(IH1(N1),JH1(N1),NZ)-Z3(I,J,K)
             T21(I,J,K,N1)=T2(IH1(N1),JH1(N1),NZ)+GAMMA*DZ1
             Q21(I,J,K,N1)=Q2(IH1(N1),JH1(N1),NZ)
           ELSE
             DO K1=2,IZ
               ZDIF1=ZMID2(IH1(N1),JH1(N1),K1)-Z3(I,J,K)
               IF(ZDIF1.GE.0.)THEN
                 FACT1=ZDIF1/(ZMID2(IH1(N1),JH1(N1),K1)-   &
                              ZMID2(IH1(N1),JH1(N1),K1-1))
                 T21(I,J,K,N1)=T2(IH1(N1),JH1(N1),K1)*(1.-FACT1)     &
                              +T2(IH1(N1),JH1(N1),K1-1)*FACT1
                 Q21(I,J,K,N1)=Q2(IH1(N1),JH1(N1),K1)*(1.-FACT1)     &
                              +Q2(IH1(N1),JH1(N1),K1-1)*FACT1
                 CYCLE CYC_39
               END IF
             END DO
           END IF
         END DO CYC_39
       ENDDO CYC_45
       ENDDO
       
       print*,'test67'

!$omp parallel do &
!$omp& private(i,j)
       DO J=1,JY
       CYC_47: DO I=1,JX
         IF(IIH1(I,J,1).LT.2.or.IIH1(I,J,1).GT.IX_1)CYCLE CYC_47
         IF(JJH1(I,J,1).LE.2.or.JJH1(I,J,1).GE.IY_1)CYCLE CYC_47
         SLP3(I,J) = HBWGT1(I,J,1)*SLP21(I,J,1)             &
                   + HBWGT1(I,J,2)*SLP21(I,J,2)             &
                   + HBWGT1(I,J,3)*SLP21(I,J,3)             &
                   + HBWGT1(I,J,4)*SLP21(I,J,4)
         TS3(I,J)  = HBWGT1(I,J,1)*T21(I,J,1,1)             &
                   + HBWGT1(I,J,2)*T21(I,J,1,2)             &
                   + HBWGT1(I,J,3)*T21(I,J,1,3)             &
                   + HBWGT1(I,J,4)*T21(I,J,1,4)
         QS3(I,J)  = HBWGT1(I,J,1)*Q21(I,J,1,1)             &
                   + HBWGT1(I,J,2)*Q21(I,J,1,2)             &
                   + HBWGT1(I,J,3)*Q21(I,J,1,3)             &
                   + HBWGT1(I,J,4)*Q21(I,J,1,4)
       ENDDO CYC_47
       ENDDO

 887   CONTINUE

!      print*,'2st merge,488,744=',SLP3(488,744),TS3(488,744),QS3(488,744)
!??????????????????

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  interpolate from d03
!
      NDX=0
      NDY=0

      NXDX=NX5+NDX
      NYDY=NY5+NDY

      WBD5=WBD5-NDX*DLMD5                ! WBD2=WBD2-DLMD2*2*NDX/2
      SBD5=SBD5-NDY*DPHD5/2

 
      IIH2=999999
      JJH2=999999
      IIV2=999999
      JJV2=999999
      HBWGT2=0.
      VBWGT2=0.

!      RATIO=1
      RATIO=DLMD5/DLMD3
      CURRENT_DOMAIN_ID=3
      DX_S=DLMD5
      DY_S=DPHD5

      print*,'DX_S,DY_S, 3=',DX_S,DY_S

        CALL G2T2H_egrid( IIH2,JJH2,                          & ! output grid index and weights
                          HBWGT2,                             & ! output weights in terms of parent grid
                          CLAT1, CLON1,                       & ! input source domain center
                          CURRENT_DOMAIN_ID,                  & ! input source domain ID
                          DX_S,DY_S,                          & ! input source outmost domain resolution
                          HLAT5,HLON5,                        & ! input source point regular lat and lon
                          HLAT3,HLON3,                        & ! input target point regular lat and lon
                          1,NXDX,1,NYDY,                      & ! input source outmost domain dimensions
                          1,NXDX,1,NYDY,                      & ! source dimensions
                          1,JX,1,JY                    )        ! target dimensions


        CALL G2T2V_egrid( IIV2,JJV2,                          & ! output grid index and weights
                          VBWGT2,                             & ! output weights in terms of parent grid
                          CLAT1, CLON1,                       & ! input source domain center(-6h)
                          CURRENT_DOMAIN_ID,                  & ! input source domain ID
                          DX_S,DY_S,                          & ! input source outmost domain resolution
                          VLAT5,VLON5,                        & ! input source point regular lat and lon
                          VLAT3,VLON3,                        & ! input target point regular lat and lon
                          1,NXDX,1,NYDY,                      & ! input source outmost domain dimensions
                          1,NXDX,1,NYDY,                      & ! source dimensions
                          1,JX,1,JY                    )        ! target dimensions


       print*,'IIH2(1,1,1),JJH2(1,1,1)=',IIH2(1,1,1),JJH2(1,1,1)
       print*,'IIH2(JX,JY,1),JJH2(JX,JY,1)=',IIH2(JX,JY,1),JJH2(JX,JY,1)
       print*,'IIV2(1,1,1),JJV2(1,1,1)=',IIV2(1,1,1),JJV2(1,1,1)
       print*,'IIV2(JX,JY,1),JJV2(JX,JY,1)=',IIV2(JX,JY,1),JJV2(JX,JY,1)
               
       print*,'test66'
    
! replace the topgraphy data for the inner nest

       A13=1.                 ! all ocean
       B13=0.002
       C13=0.88

         NX5_1=NX5-2
         NY5_1=NY5-2

       IF(IBGS.EQ.1.and.abs(DLMD5/DLMD3-1.).lt.0.01)THEN

!         print*,'check if the nest domain created correctly'

!         DO J=1,NY5
!         DO I=1,NX5
!           DIFFI=abs(HLON4(I,J)-HLON5(I,J))+    &
!                 abs(HLAT4(I,J)-HLAT5(I,J))
!           IF(DIFFI.GT.0.001)THEN
!             print*,'I,J,DIFFI=',I,J,DIFFI
!             STOP 333
!           END IF
!         END DO
!         END DO

!         print*,'finished checking nest domain created correctly'

         DO J=1,NY5
         DO I=1,NX5
           J1=J+J_ST14-1
           I1=I+I_ST14-1
           ZS3(I1,J1)=Z5(I,J,1)
           A13(I1,J1)=A105(I,J)
           B13(I1,J1)=B105(I,J)
           C13(I1,J1)=C105(I,J)
         END DO
         END DO

       ELSE

!$omp parallel do &
!$omp& private(i,j,N1,IH1,JH1)
         DO J=1,JY
         CYC_344: DO I=1,JX
           IF(IIH2(I,J,1).LT.2.or.IIH2(I,J,1).GT.NX5_1)CYCLE CYC_344
           IF(JJH2(I,J,1).LE.2.or.JJH2(I,J,1).GE.NY5_1)CYCLE CYC_344
           DO N1=1,4
             IH1(N1)=IIH2(I,J,N1)
             JH1(N1)=JJH2(I,J,N1)
           END DO
           ZS3(I,J) = HBWGT2(I,J,1)*Z5(IH1(1),JH1(1),1)             &
                    + HBWGT2(I,J,2)*Z5(IH1(2),JH1(2),1)             &
                    + HBWGT2(I,J,3)*Z5(IH1(3),JH1(3),1)             &
                    + HBWGT2(I,J,4)*Z5(IH1(4),JH1(4),1)
         END DO CYC_344
         END DO
             
         DO J=1,JY
         DO I=1,JX
           Z3(I,J,1)=ZS3(I,J)
         END DO
         END DO

!$omp parallel do &
!$omp& private(i,j,n1,IV1,JV1)
       DO J=1,JY
       CYC_443: DO I=1,JX
         IF(IIV1(I,J,1).LT.2.or.IIV1(I,J,1).GT.IX_1)CYCLE CYC_443
         IF(JJV1(I,J,1).LE.2.or.JJV1(I,J,1).GE.IY_1)CYCLE CYC_443
         DO N1=1,4
           IV1(N1)=IIV1(I,J,N1)
           JV1(N1)=JJV1(I,J,N1)
         END DO
         A13(I,J) = VBWGT1(I,J,1)*A102(IV1(1),JV1(1))             &
                  + VBWGT1(I,J,2)*A102(IV1(2),JV1(2))             &
                  + VBWGT1(I,J,3)*A102(IV1(3),JV1(3))             &
                  + VBWGT1(I,J,4)*A102(IV1(4),JV1(4))
         B13(I,J) = VBWGT1(I,J,1)*B102(IV1(1),JV1(1))             &
                  + VBWGT1(I,J,2)*B102(IV1(2),JV1(2))             &
                  + VBWGT1(I,J,3)*B102(IV1(3),JV1(3))             &
                  + VBWGT1(I,J,4)*B102(IV1(4),JV1(4))
         C13(I,J) = VBWGT1(I,J,1)*C102(IV1(1),JV1(1))             &
                  + VBWGT1(I,J,2)*C102(IV1(2),JV1(2))             &
                  + VBWGT1(I,J,3)*C102(IV1(3),JV1(3))             &
                  + VBWGT1(I,J,4)*C102(IV1(4),JV1(4))
       END DO CYC_443
       END DO
!$omp parallel do &
!$omp& private(i,j,n1,IV1,JV1)
       DO J=1,JY
       CYC_441: DO I=1,JX
         IF(IIV2(I,J,1).LT.2.or.IIV2(I,J,1).GT.NX5_1)CYCLE CYC_441
         IF(JJV2(I,J,1).LE.2.or.JJV2(I,J,1).GE.NY5_1)CYCLE CYC_441
         DO N1=1,4
           IV1(N1)=IIV2(I,J,N1)
           JV1(N1)=JJV2(I,J,N1)
         END DO
         A13(I,J) = VBWGT2(I,J,1)*A105(IV1(1),JV1(1))             &
                  + VBWGT2(I,J,2)*A105(IV1(2),JV1(2))             &
                  + VBWGT2(I,J,3)*A105(IV1(3),JV1(3))             &
                  + VBWGT2(I,J,4)*A105(IV1(4),JV1(4))
         B13(I,J) = VBWGT2(I,J,1)*B105(IV1(1),JV1(1))             &
                  + VBWGT2(I,J,2)*B105(IV1(2),JV1(2))             &
                  + VBWGT2(I,J,3)*B105(IV1(3),JV1(3))             &
                  + VBWGT2(I,J,4)*B105(IV1(4),JV1(4))
         C13(I,J) = VBWGT2(I,J,1)*C105(IV1(1),JV1(1))             &
                  + VBWGT2(I,J,2)*C105(IV1(2),JV1(2))             &
                  + VBWGT2(I,J,3)*C105(IV1(3),JV1(3))             &
                  + VBWGT2(I,J,4)*C105(IV1(4),JV1(4))
       END DO CYC_441
       END DO

       END IF

       print*,'test2 C13(jx/2,jy/2)=',C13(jx/2,jy/2)
!!!!!!!!!!!!!

       IF(IVOBS.LT.IVOBS_CUT)GO TO 888

!$omp parallel do &
!$omp& private(i,j,k,n1,DZ1,ZDIF1,FACT1,IH1,JH1,K1)
       DO J=1,JY
       CYC_445: DO I=1,JX
         IF(IIH2(I,J,1).LT.2.or.IIH2(I,J,1).GT.NX5_1)CYCLE CYC_445
         IF(JJH2(I,J,1).LE.2.or.JJH2(I,J,1).GE.NY5_1)CYCLE CYC_445
         DO N1=1,4
           IH1(N1)=IIH2(I,J,N1)
           JH1(N1)=JJH2(I,J,N1)
         END DO
         DO N1=1,4
           SLP21(I,J,N1)=SLP5(IH1(N1),JH1(N1))
         END DO
         K=1             ! surface
         CYC_339: DO N1=1,4
           IF(Z3(I,J,K).LT.ZMID5(IH1(N1),JH1(N1),1))THEN
             DZ1=ZMID5(IH1(N1),JH1(N1),1)-Z3(I,J,K)
             T21(I,J,K,N1)=T5(IH1(N1),JH1(N1),1)+GAMMA*DZ1
             Q21(I,J,K,N1)=Q5(IH1(N1),JH1(N1),1)
           ELSE IF(Z3(I,J,K).GE.ZMID5(IH1(N1),JH1(N1),NZ))THEN  ! never occur for K=1
             DZ1=ZMID5(IH1(N1),JH1(N1),NZ)-Z3(I,J,K)
             T21(I,J,K,N1)=T5(IH1(N1),JH1(N1),NZ)+GAMMA*DZ1
             Q21(I,J,K,N1)=Q5(IH1(N1),JH1(N1),NZ)
           ELSE
             DO K1=2,IZ
               ZDIF1=ZMID5(IH1(N1),JH1(N1),K1)-Z3(I,J,K)
               IF(ZDIF1.GE.0.)THEN
                 FACT1=ZDIF1/(ZMID5(IH1(N1),JH1(N1),K1)-   &
                              ZMID5(IH1(N1),JH1(N1),K1-1))
                 T21(I,J,K,N1)=T5(IH1(N1),JH1(N1),K1)*(1.-FACT1)     &
                              +T5(IH1(N1),JH1(N1),K1-1)*FACT1
                 Q21(I,J,K,N1)=Q5(IH1(N1),JH1(N1),K1)*(1.-FACT1)     &
                              +Q5(IH1(N1),JH1(N1),K1-1)*FACT1
                 CYCLE CYC_339
               END IF
             END DO
           END IF
         END DO CYC_339
       ENDDO CYC_445
       ENDDO
       
       print*,'test67'

!$omp parallel do &
!$omp& private(i,j)
       DO J=1,JY
       CYC_447: DO I=1,JX
         IF(IIH2(I,J,1).LT.2.or.IIH2(I,J,1).GT.NX5_1)CYCLE CYC_447
         IF(JJH2(I,J,1).LE.2.or.JJH2(I,J,1).GE.NY5_1)CYCLE CYC_447
         SLP3(I,J) = HBWGT2(I,J,1)*SLP21(I,J,1)             &
                   + HBWGT2(I,J,2)*SLP21(I,J,2)             &
                   + HBWGT2(I,J,3)*SLP21(I,J,3)             &
                   + HBWGT2(I,J,4)*SLP21(I,J,4)
         TS3(I,J)  = HBWGT2(I,J,1)*T21(I,J,1,1)             &
                   + HBWGT2(I,J,2)*T21(I,J,1,2)             &
                   + HBWGT2(I,J,3)*T21(I,J,1,3)             &
                   + HBWGT2(I,J,4)*T21(I,J,1,4)
         QS3(I,J)  = HBWGT2(I,J,1)*Q21(I,J,1,1)             &
                   + HBWGT2(I,J,2)*Q21(I,J,1,2)             &
                   + HBWGT2(I,J,3)*Q21(I,J,1,3)             &
                   + HBWGT2(I,J,4)*Q21(I,J,1,4)
       ENDDO CYC_447
       ENDDO

 888   CONTINUE

!      print*,'3st merge,488,744=',SLP3(488,744),TS3(488,744),QS3(488,744)
!      print*,'HBWGT2(488,744)=',HBWGT2(488,744,1),HBWGT2(488,744,2),HBWGT2(488,744,3),HBWGT2(488,744,4)
!      print*,'SLP21(488,744)=',SLP21(488,744,1),SLP21(488,744,2),SLP21(488,744,3),SLP21(488,744,4)
!??????????????????

       print*,'test68'

! 
! Construct 3D pressure grid

!$omp parallel do &
!$omp& private(i,j,ZSFC,TSFC,A)
       DO J=1,JY
       DO I=1,JX
         ZSFC = ZS3(I,J)
         TSFC = TS3(I,J)*(1.+D608*QS3(I,J))
         A = (GAMMA * ZSFC) / TSFC
         P3(I,J,1) = SLP3(I,J)/(1+A)**COEF2
         PD3(I,J)=P3(I,J,1)-PDTOP3-PT3
       ENDDO
       ENDDO
                                                                                                                                      
! PD(I,J)=P1(I,J,1)-PDTOP-PT=PSFC(I,J)-PDTOP-PT
!$omp parallel do &
!$omp& private(i,j,k)
       DO K=1,KZ+1
       DO J=1,JY
       DO I=1,JX
         P3(I,J,K)=PT3+PDTOP3*ETA1(K)+PD3(I,J)*ETA2(K)     ! PD(I,J) changed
       ENDDO
       ENDDO
       ENDDO

!$omp parallel do &
!$omp& private(i,j,k)
       DO K=1,KZ
       DO J=1,JY
       DO I=1,JX
         PMID3(I,J,K)=EXP((ALOG(P3(I,J,K))+ALOG(P3(I,J,K+1)))*0.5)
       ENDDO
       ENDDO
       ENDDO

! interpolate vertically to 3D-P level in new coordinate  (H Points)
! from outer nest data

!$omp parallel do &
!$omp& private(i,j,k,n1,DZ1,PDIF1,FACT1,IH1,JH1,K1)
       DO J=1,JY
       CYC_70: DO I=1,JX
         IF(IIH(I,J,1).LT.1.or.IIH(I,J,1).GT.NX_1)CYCLE CYC_70
         IF(JJH(I,J,1).LE.2.or.JJH(I,J,1).GE.NY_1)CYCLE CYC_70
         DO N1=1,4
           IH1(N1)=IIH(I,J,N1)
           JH1(N1)=JJH(I,J,N1)
         END DO
!         DO N1=1,4
!           SLP21(I,J,N1)=SLP2(IH1(N1),JH1(N1))
!         END DO
         DO K=1,KZ
         CYC_50: DO N1=1,4
           IF(PMID3(I,J,K).GT.PMID1(IH1(N1),JH1(N1),1))THEN
             DZ1=T1(IH1(N1),JH1(N1),1)      &
                 /GAMMA*(1.-(PMID3(I,J,K)/PMID1(IH1(N1),JH1(N1),1))**COEF3)
             T21(I,J,K,N1)=T1(IH1(N1),JH1(N1),1)-GAMMA*DZ1
             Q21(I,J,K,N1)=Q1(IH1(N1),JH1(N1),1)
           ELSE IF(PMID3(I,J,K).LE.PMID1(IH1(N1),JH1(N1),KZ))THEN
             DZ1=T1(IH1(N1),JH1(N1),KZ)      &
                 /GAMMA*(1.-(PMID3(I,J,K)/PMID1(IH1(N1),JH1(N1),KZ))**COEF3)
             T21(I,J,K,N1)=T1(IH1(N1),JH1(N1),KZ)-GAMMA*DZ1
             Q21(I,J,K,N1)=Q1(IH1(N1),JH1(N1),KZ)
           ELSE
             DO K1=2,KZ
               PDIF1=ALOG(PMID3(I,J,K))-ALOG(PMID1(IH1(N1),JH1(N1),K1))
               IF(PDIF1.GE.0.)THEN
                 FACT1=PDIF1/(ALOG(PMID1(IH1(N1),JH1(N1),K1-1))-   &
                               ALOG(PMID1(IH1(N1),JH1(N1),K1)))
                 T21(I,J,K,N1)=T1(IH1(N1),JH1(N1),K1)*(1.-FACT1)     &
                              +T1(IH1(N1),JH1(N1),K1-1)*FACT1
                 Q21(I,J,K,N1)=Q1(IH1(N1),JH1(N1),K1)*(1.-FACT1)     &
                              +Q1(IH1(N1),JH1(N1),K1-1)*FACT1
                 CYCLE CYC_50
               END IF
             END DO
           END IF
         END DO CYC_50
         END DO
       ENDDO CYC_70
       ENDDO

       print*,'test69'
!
!$omp parallel do &
!$omp& private(i,j,k)
       DO J=1,JY
       CYC_120: DO I=1,JX
         IF(IIH(I,J,1).LT.1.or.IIH(I,J,1).GT.NX_1)CYCLE CYC_120
         IF(JJH(I,J,1).LE.2.or.JJH(I,J,1).GE.NY_1)CYCLE CYC_120
         DO K=1,KZ
            T3(I,J,K) =                               &
                HBWGT(I,J,1)*T21(I,J,K,1)             &
              + HBWGT(I,J,2)*T21(I,J,K,2)             &
              + HBWGT(I,J,3)*T21(I,J,K,3)             &
              + HBWGT(I,J,4)*T21(I,J,K,4)
            Q3(I,J,K) =                               &
                HBWGT(I,J,1)*Q21(I,J,K,1)             &
              + HBWGT(I,J,2)*Q21(I,J,K,2)             &
              + HBWGT(I,J,3)*Q21(I,J,K,3)             &
              + HBWGT(I,J,4)*Q21(I,J,K,4)
         ENDDO
       ENDDO CYC_120
       ENDDO

!      print*,'1st merge,488,744=',SLP3(488,744),Z3(488,744,1),P3(488,744,1),T3(488,744,1),Q3(488,744,1)
       print*,'test70'

       IF(IVOBS.LT.IVOBS_CUT)GO TO 889

! interpolate vertically to 3D-P level in new coordinate  (H Points)
! from inner nest data

!$omp parallel do &
!$omp& private(i,j,k,n1,DZ1,PDIF1,FACT1,IH1,JH1,K1)
       DO J=1,JY
       CYC_73: DO I=1,JX
         IF(IIH1(I,J,1).LT.2.or.IIH1(I,J,1).GT.IX_1)CYCLE CYC_73
         IF(JJH1(I,J,1).LE.2.or.JJH1(I,J,1).GE.IY_1)CYCLE CYC_73
         DO N1=1,4
           IH1(N1)=IIH1(I,J,N1)
           JH1(N1)=JJH1(I,J,N1)
         END DO
!         DO N1=1,4
!           SLP21(I,J,N1)=SLP2(IH1(N1),JH1(N1))
!         END DO
         DO K=1,KZ
         CYC_53: DO N1=1,4
           IF(PMID3(I,J,K).GT.PMID2(IH1(N1),JH1(N1),1))THEN
             DZ1=T2(IH1(N1),JH1(N1),1)      &
                 /GAMMA*(1.-(PMID3(I,J,K)/PMID2(IH1(N1),JH1(N1),1))**COEF3)
             T21(I,J,K,N1)=T2(IH1(N1),JH1(N1),1)-GAMMA*DZ1
             Q21(I,J,K,N1)=Q2(IH1(N1),JH1(N1),1)
           ELSE IF(PMID3(I,J,K).LE.PMID2(IH1(N1),JH1(N1),KZ))THEN
             DZ1=T2(IH1(N1),JH1(N1),KZ)      &
                 /GAMMA*(1.-(PMID3(I,J,K)/PMID2(IH1(N1),JH1(N1),KZ))**COEF3)
             T21(I,J,K,N1)=T2(IH1(N1),JH1(N1),KZ)-GAMMA*DZ1
             Q21(I,J,K,N1)=Q2(IH1(N1),JH1(N1),KZ)
           ELSE
             DO K1=2,KZ
               PDIF1=ALOG(PMID3(I,J,K))-ALOG(PMID2(IH1(N1),JH1(N1),K1))
               IF(PDIF1.GE.0.)THEN
                 FACT1=PDIF1/(ALOG(PMID2(IH1(N1),JH1(N1),K1-1))-   &
                      ALOG(PMID2(IH1(N1),JH1(N1),K1)))
                 T21(I,J,K,N1)=T2(IH1(N1),JH1(N1),K1)*(1.-FACT1)     &
                              +T2(IH1(N1),JH1(N1),K1-1)*FACT1
                 Q21(I,J,K,N1)=Q2(IH1(N1),JH1(N1),K1)*(1.-FACT1)     &
                              +Q2(IH1(N1),JH1(N1),K1-1)*FACT1
                 CYCLE CYC_53
               END IF
             END DO
           END IF
         END DO CYC_53
         END DO
       ENDDO CYC_73
       ENDDO

!$omp parallel do &
!$omp& private(i,j,k)
       DO J=1,JY
       CYC_123: DO I=1,JX
         IF(IIH1(I,J,1).LT.2.or.IIH1(I,J,1).GT.IX_1)CYCLE CYC_123
         IF(JJH1(I,J,1).LE.2.or.JJH1(I,J,1).GE.IY_1)CYCLE CYC_123
         DO K=1,KZ
            T3(I,J,K) =                               &
                HBWGT1(I,J,1)*T21(I,J,K,1)             &
              + HBWGT1(I,J,2)*T21(I,J,K,2)             &
              + HBWGT1(I,J,3)*T21(I,J,K,3)             &
              + HBWGT1(I,J,4)*T21(I,J,K,4)             
            Q3(I,J,K) =                               &
                HBWGT1(I,J,1)*Q21(I,J,K,1)             &
              + HBWGT1(I,J,2)*Q21(I,J,K,2)             &
              + HBWGT1(I,J,3)*Q21(I,J,K,3)             &
              + HBWGT1(I,J,4)*Q21(I,J,K,4)             
         ENDDO
       ENDDO CYC_123
       ENDDO

!      print*,'2st merge,488,744=',SLP3(488,744),Z3(488,744,1),P3(488,744,1),T3(488,744,1),Q3(488,744,1)
! interpolate from d03

!       GO TO 889

!$omp parallel do &
!$omp& private(i,j,k,n1,DZ1,PDIF1,FACT1,IH1,JH1,K1)
       DO J=1,JY
       CYC_773: DO I=1,JX
         IF(IIH2(I,J,1).LT.2.or.IIH2(I,J,1).GT.NX5_1)CYCLE CYC_773
         IF(JJH2(I,J,1).LE.2.or.JJH2(I,J,1).GE.NY5_1)CYCLE CYC_773
         DO N1=1,4
           IH1(N1)=IIH2(I,J,N1)
           JH1(N1)=JJH2(I,J,N1)
         END DO
!         DO K=1,KZ
!           print*,'tttt test1=',PMID3(I,J,K),IIH2(I,J),JJH2(I,J),    &
!                   (PMID5(IH1(N1),JH1(N1),K),N1=1,4)
!         END DO
!         STOP
!         DO N1=1,4
!           SLP21(I,J,N1)=SLP2(IH1(N1),JH1(N1))
!         END DO
         DO K=1,KZ
         CYC_553: DO N1=1,4
           IF(PMID3(I,J,K).GT.PMID5(IH1(N1),JH1(N1),1))THEN
             DZ1=T5(IH1(N1),JH1(N1),1)      &
                 /GAMMA*(1.-(PMID3(I,J,K)/PMID5(IH1(N1),JH1(N1),1))**COEF3)
             T21(I,J,K,N1)=T5(IH1(N1),JH1(N1),1)-GAMMA*DZ1
             Q21(I,J,K,N1)=Q5(IH1(N1),JH1(N1),1)
!             IF(N1.EQ.1)PRINT*,'hhhh test K 1 =',K,PMID3(I,J,K),PMID5(IH1(N1),JH1(N1),1)
           ELSE IF(PMID3(I,J,K).LE.PMID5(IH1(N1),JH1(N1),KZ))THEN
             DZ1=T5(IH1(N1),JH1(N1),KZ)      &
                 /GAMMA*(1.-(PMID3(I,J,K)/PMID5(IH1(N1),JH1(N1),KZ))**COEF3)
             T21(I,J,K,N1)=T5(IH1(N1),JH1(N1),KZ)-GAMMA*DZ1
             Q21(I,J,K,N1)=Q5(IH1(N1),JH1(N1),KZ)
!             IF(N1.EQ.1)PRINT*,'hhhh test K KZ =',K,PMID3(I,J,K),PMID5(IH1(N1),JH1(N1),KZ)
           ELSE
             DO K1=2,KZ
               PDIF1=ALOG(PMID3(I,J,K))-ALOG(PMID5(IH1(N1),JH1(N1),K1))
               IF(PDIF1.GE.0.)THEN
                 FACT1=PDIF1/(ALOG(PMID5(IH1(N1),JH1(N1),K1-1))-   &
                      ALOG(PMID5(IH1(N1),JH1(N1),K1)))
                 T21(I,J,K,N1)=T5(IH1(N1),JH1(N1),K1)*(1.-FACT1)     &
                              +T5(IH1(N1),JH1(N1),K1-1)*FACT1
                 Q21(I,J,K,N1)=Q5(IH1(N1),JH1(N1),K1)*(1.-FACT1)     &
                              +Q5(IH1(N1),JH1(N1),K1-1)*FACT1
!             IF(N1.EQ.1)PRINT*,'hhhh test K K1 =',K,K1,PMID3(I,J,K),PMID5(IH1(N1),JH1(N1),K1)
!                 print*,'hhhh test=',K,K1,PMID3(I,J,K),PMID5(IH1(N1),JH1(N1),K1)
                 CYCLE CYC_553
               END IF
             END DO
           END IF
         END DO CYC_553
         END DO
       ENDDO CYC_773
       ENDDO

!$omp parallel do &
!$omp& private(i,j,k,T33)
       DO J=1,JY
       CYC_223: DO I=1,JX
         IF(IIH2(I,J,1).LT.2.or.IIH2(I,J,1).GT.NX5_1)CYCLE CYC_223
         IF(JJH2(I,J,1).LE.2.or.JJH2(I,J,1).GE.NY5_1)CYCLE CYC_223
         DO K=1,KZ
            T33=T3(I,J,K)
            T3(I,J,K) =                               &
                HBWGT2(I,J,1)*T21(I,J,K,1)             &
              + HBWGT2(I,J,2)*T21(I,J,K,2)             &
              + HBWGT2(I,J,3)*T21(I,J,K,3)             &
              + HBWGT2(I,J,4)*T21(I,J,K,4)             
            Q3(I,J,K) =                               &
                HBWGT2(I,J,1)*Q21(I,J,K,1)             &
              + HBWGT2(I,J,2)*Q21(I,J,K,2)             &
              + HBWGT2(I,J,3)*Q21(I,J,K,3)             &
              + HBWGT2(I,J,4)*Q21(I,J,K,4)            
            IF(abs(T33-T3(I,J,K)).GT.10.)THEN
              print*,'k,T33,T3=',K,T33,T3(I,J,K),(T21(I,J,K,M1),M1=1,4)
            END IF 
         ENDDO
       ENDDO CYC_223
       ENDDO

 889   CONTINUE

!      print*,'3st merge,488,744=',SLP3(488,744),Z3(488,744,1),P3(488,744,1),T3(488,744,1),Q3(488,744,1)
       print*,'test71'
 
! Compute Geopotentital height, INTEGRATE HEIGHT HYDROSTATICLY
            
!$omp parallel do &
!$omp& private(i,j,ZSFC,TSFC,A)
       DO J=1,JY
       DO I=1,JX
         ZSFC = ZS3(I,J)
         TSFC = T3(I,J,1)*(1.+D608*Q3(I,J,1))
         A = (GAMMA * ZSFC) / TSFC
         P3(I,J,1) = SLP3(I,J)/(1+A)**COEF2
         PD3(I,J)=P3(I,J,1)-PDTOP3-PT3
       ENDDO
       ENDDO
                                                                                                                                                                                         
! PD(I,J)=P1(I,J,1)-PDTOP-PT=PSFC(I,J)-PDTOP-PT
!$omp parallel do &
!$omp& private(i,j,k)
       DO K=1,KZ+1
       DO J=1,JY
       DO I=1,JX
         P3(I,J,K)=PT3+PDTOP3*ETA1(K)+PD3(I,J)*ETA2(K)     ! PD(I,J) changed
       ENDDO
       ENDDO
       ENDDO
                                                                                                                                          
      do j = 1,JY
      do i = 1,JX
        Z3(I,J,1)=ZS3(I,J)
        DO L=2,KZ+1
          Z3(I,J,L)=Z3(I,J,L-1)+T3(I,J,L-1)*          &
              (Q3(I,J,L-1)*0.608+1.0)*287.04*         &
              (ALOG(P3(I,J,L-1))-ALOG(P3(I,J,L)))/G
        ENDDO
       ENDDO
      END DO

!$omp parallel do &
!$omp& private(i,j,k)
       DO K=1,KZ
       DO J=1,JY
       DO I=1,JX
         PMID3(I,J,K)=EXP((ALOG(P3(I,J,K))+ALOG(P3(I,J,K+1)))*0.5)
       ENDDO
       ENDDO
       ENDDO

       PMV3=PMID3

       print*,'test711'

! interpolate vertically to P level in new coordinate  (V Points)
!$omp parallel do &
!$omp& private(i,j,k)
       DO J=2,JY-1
         IF(MOD(J,2).NE.0.)THEN
           DO K=1,KZ
           DO I=2,JX-1
             PMV3(I,J,K)=0.25*(PMID3(I,J,K)+PMID3(I+1,J,K)+            &
                               PMID3(I,J-1,K)+PMID3(I,J+1,K))
           END DO
           END DO
         ELSE
           DO K=1,KZ
             DO I=2,JX-1
               PMV3(I,J,K)=0.25*(PMID3(I-1,J,K)+PMID3(I,J,K)+            &
                                 PMID3(I,J-1,K)+PMID3(I,J+1,K))
             END DO
           END DO
         END IF
       END DO

       PMV1=PMID1     
 
!$omp parallel do &
!$omp& private(i,j,k)
       DO J=2,NY-1
         IF(MOD(J,2).NE.0.)THEN
           DO I=2,NX-1
           DO K=1,NZ
             PMV1(I,J,K)=0.25*(PMID1(I,J,K)+PMID1(I+1,J,K)+            &
                               PMID1(I,J-1,K)+PMID1(I,J+1,K))
           END DO
           END DO
         ELSE
           DO I=2,NX-1
           DO K=1,NZ
             PMV1(I,J,K)=0.25*(PMID1(I-1,J,K)+PMID1(I,J,K)+            &
                               PMID1(I,J-1,K)+PMID1(I,J+1,K))
           END DO
           END DO
         END IF
       END DO

       print*,'test712'

!$omp parallel do &
!$omp& private(i,j,k,n1,DP1,PDIF1,FACT1,IV1,JV1,K1)
       DO J=1,JY
       CYC_80: DO I=1,JX
         IF(IIV(I,J,1).LT.1.or.IIV(I,J,1).GT.NX_1)CYCLE CYC_80
         IF(JJV(I,J,1).LE.2.or.JJV(I,J,1).GE.NY_1)CYCLE CYC_80
         DO N1=1,4
           IV1(N1)=IIV(I,J,N1)
           JV1(N1)=JJV(I,J,N1)
         END DO
!         A13(I,J) = VBWGT(I,J,1)*A101(IV1(1),JV1(1))             &
!                  + VBWGT(I,J,2)*A101(IV1(2),JV1(2))             &
!                  + VBWGT(I,J,3)*A101(IV1(3),JV1(3))             &
!                  + VBWGT(I,J,4)*A101(IV1(4),JV1(4))

         DO K=1,KZ
           CYC_60: DO N1=1,4
             IF(PMV3(I,J,K).GT.PMV1(IV1(N1),JV1(N1),1))THEN
               DP1=PMV3(I,J,K)-PMV1(IV1(N1),JV1(N1),1)
!               U21(I,J,K,N1)=U1(IV1(N1),JV1(N1),1)*(1.-DP1*1.4E-5)
!               V21(I,J,K,N1)=V1(IV1(N1),JV1(N1),1)*(1.-DP1*1.4E-5)
               U21(I,J,K,N1)=U1(IV1(N1),JV1(N1),1)
               V21(I,J,K,N1)=V1(IV1(N1),JV1(N1),1)
             ELSE IF(PMV3(I,J,K).LE.PMV1(IV1(N1),JV1(N1),NZ))THEN
               U21(I,J,K,N1)=U1(IV1(N1),JV1(N1),NZ)
               V21(I,J,K,N1)=V1(IV1(N1),JV1(N1),NZ)
             ELSE
               DO K1=2,KZ
                 PDIF1=ALOG(PMV3(I,J,K))-ALOG(PMV1(IV1(N1),JV1(N1),K1))
                 IF(PDIF1.GE.0.)THEN
                   FACT1=PDIF1/(ALOG(PMV1(IV1(N1),JV1(N1),K1-1))-             &
                                ALOG(PMV1(IV1(N1),JV1(N1),K1)))
                   U21(I,J,K,N1)=U1(IV1(N1),JV1(N1),K1)*(1.-FACT1)     &
                                +U1(IV1(N1),JV1(N1),K1-1)*FACT1
                   V21(I,J,K,N1)=V1(IV1(N1),JV1(N1),K1)*(1.-FACT1)     &
                                +V1(IV1(N1),JV1(N1),K1-1)*FACT1
                   CYCLE CYC_60
                 END IF
               END DO
             END IF
           END DO CYC_60
         END DO
       END DO CYC_80
       END DO

       print*,'test72'

!23456789012345678901234567890123456789012345678901234567890123456789012

!$omp parallel do &
!$omp& private(i,j,k)
       DO J=1,JY
       CYC_110: DO I=1,JX
         IF(IIV(I,J,1).LT.1.or.IIV(I,J,1).GT.NX_1)CYCLE CYC_110
         IF(JJV(I,J,1).LE.2.or.JJV(I,J,1).GE.NY_1)CYCLE CYC_110
         DO K=1,KZ
            U3(I,J,K) =                       & 
               VBWGT(I,J,1)*U21(I,J,K,1)      &
             + VBWGT(I,J,2)*U21(I,J,K,2)      &
             + VBWGT(I,J,3)*U21(I,J,K,3)      &
             + VBWGT(I,J,4)*U21(I,J,K,4)
            V3(I,J,K) =                       &
               VBWGT(I,J,1)*V21(I,J,K,1)      &
             + VBWGT(I,J,2)*V21(I,J,K,2)      &
             + VBWGT(I,J,3)*V21(I,J,K,3)      &
             + VBWGT(I,J,4)*V21(I,J,K,4)
         END DO
!         if(J.GT.0.95*JY.and.I.gt.0.95*JX)print*,'test72111',I,J
       ENDDO CYC_110
       ENDDO

        print*,'JX,JY,KZ=',JX,JY,KZ
        write(*,*)'interpolation after d01='
      do k=1,kz
        write(*,32)K,T3(9,9,K),            &
          Q3(9,9,K),U3(9,9,K),V3(9,9,K),Z3(9,9,K),P3(9,9,K)
      end do

       print*,'test721'

!      WRITE(64)((SLP3(I,J),I=1,JX),J=1,JY,2)
!      DO K=1,KZ+1
!        WRITE(64)((Z3(I,J,K),I=1,JX),J=1,JY,2)
!      END DO
!      DO K=1,KZ+1
!        WRITE(64)((P3(I,J,K),I=1,JX),J=1,JY,2)
!      END DO
!      DO K=1,KZ
!        WRITE(64)((T3(I,J,K),I=1,JX),J=1,JY,2)
!      END DO
!      DO K=1,KZ
!        WRITE(64)((Q3(I,J,K),I=1,JX),J=1,JY,2)
!      END DO
!      DO K=1,KZ
!        WRITE(64)((U3(I,J,K),I=1,JX),J=1,JY,2)
!      END DO
!      DO K=1,KZ
!        WRITE(64)((V3(I,J,K),I=1,JX),J=1,JY,2)
!      END DO

       print*,'test73'

      IF(IVOBS.LT.IVOBS_CUT)GO TO 890

       PMV2=PMID2     
 
!$omp parallel do &
!$omp& private(i,j,k)
       DO J=2,IY-1
         IF(MOD(J,2).NE.0.)THEN
           DO I=2,IX-1
           DO K=1,IZ
             PMV2(I,J,K)=0.25*(PMID2(I,J,K)+PMID2(I+1,J,K)+            &
                               PMID2(I,J-1,K)+PMID2(I,J+1,K))
           END DO
           END DO
         ELSE
           DO I=2,IX-1
           DO K=1,IZ
             PMV2(I,J,K)=0.25*(PMID2(I-1,J,K)+PMID2(I,J,K)+            &
                               PMID2(I,J-1,K)+PMID2(I,J+1,K))
           END DO
           END DO
         END IF
       END DO

       IF(IBGS.EQ.1.and.abs(DLMD5/DLMD3-1.).lt.0.01)THEN
!$omp parallel do &
!$omp& private(i,j,n1,IV1,JV1)
       DO J=1,JY
       CYC_37: DO I=1,JX
         IF(IIV1(I,J,1).LT.2.or.IIV1(I,J,1).GT.IX_1)CYCLE CYC_37
         IF(JJV1(I,J,1).LE.2.or.JJV1(I,J,1).GE.IY_1)CYCLE CYC_37
         DO N1=1,4
           IV1(N1)=IIV1(I,J,N1)
           JV1(N1)=JJV1(I,J,N1)
         END DO
         A13(I,J) = VBWGT1(I,J,1)*A102(IV1(1),JV1(1))             &
                  + VBWGT1(I,J,2)*A102(IV1(2),JV1(2))             &
                  + VBWGT1(I,J,3)*A102(IV1(3),JV1(3))             &
                  + VBWGT1(I,J,4)*A102(IV1(4),JV1(4))
         C13(I,J) = VBWGT1(I,J,1)*C102(IV1(1),JV1(1))             &
                  + VBWGT1(I,J,2)*C102(IV1(2),JV1(2))             &
                  + VBWGT1(I,J,3)*C102(IV1(3),JV1(3))             &
                  + VBWGT1(I,J,4)*C102(IV1(4),JV1(4))

       END DO CYC_37
       END DO
       ELSE
!$omp parallel do &
!$omp& private(i,j,n1,IV1,JV1)
       DO J=1,JY
       CYC_43: DO I=1,JX
         IF(IIV1(I,J,1).LT.2.or.IIV1(I,J,1).GT.IX_1)CYCLE CYC_43
         IF(JJV1(I,J,1).LE.2.or.JJV1(I,J,1).GE.IY_1)CYCLE CYC_43
         DO N1=1,4
           IV1(N1)=IIV1(I,J,N1)
           JV1(N1)=JJV1(I,J,N1)
         END DO
         A13(I,J) = VBWGT1(I,J,1)*A102(IV1(1),JV1(1))             &
                  + VBWGT1(I,J,2)*A102(IV1(2),JV1(2))             &
                  + VBWGT1(I,J,3)*A102(IV1(3),JV1(3))             &
                  + VBWGT1(I,J,4)*A102(IV1(4),JV1(4))
         B13(I,J) = VBWGT1(I,J,1)*B102(IV1(1),JV1(1))             &
                  + VBWGT1(I,J,2)*B102(IV1(2),JV1(2))             &
                  + VBWGT1(I,J,3)*B102(IV1(3),JV1(3))             &
                  + VBWGT1(I,J,4)*B102(IV1(4),JV1(4))
         C13(I,J) = VBWGT1(I,J,1)*C102(IV1(1),JV1(1))             &
                  + VBWGT1(I,J,2)*C102(IV1(2),JV1(2))             &
                  + VBWGT1(I,J,3)*C102(IV1(3),JV1(3))             &
                  + VBWGT1(I,J,4)*C102(IV1(4),JV1(4))
       END DO CYC_43
       END DO
!$omp parallel do &
!$omp& private(i,j,n1,IV1,JV1)
       DO J=1,JY
       CYC_41: DO I=1,JX
         IF(IIV2(I,J,1).LT.2.or.IIV2(I,J,1).GT.NX5_1)CYCLE CYC_41
         IF(JJV2(I,J,1).LE.2.or.JJV2(I,J,1).GE.NY5_1)CYCLE CYC_41
         DO N1=1,4
           IV1(N1)=IIV2(I,J,N1)
           JV1(N1)=JJV2(I,J,N1)
         END DO
         A13(I,J) = VBWGT2(I,J,1)*A105(IV1(1),JV1(1))             &
                  + VBWGT2(I,J,2)*A105(IV1(2),JV1(2))             &
                  + VBWGT2(I,J,3)*A105(IV1(3),JV1(3))             &
                  + VBWGT2(I,J,4)*A105(IV1(4),JV1(4))
         B13(I,J) = VBWGT2(I,J,1)*B105(IV1(1),JV1(1))             &
                  + VBWGT2(I,J,2)*B105(IV1(2),JV1(2))             &
                  + VBWGT2(I,J,3)*B105(IV1(3),JV1(3))             &
                  + VBWGT2(I,J,4)*B105(IV1(4),JV1(4))
         C13(I,J) = VBWGT2(I,J,1)*C105(IV1(1),JV1(1))             &
                  + VBWGT2(I,J,2)*C105(IV1(2),JV1(2))             &
                  + VBWGT2(I,J,3)*C105(IV1(3),JV1(3))             &
                  + VBWGT2(I,J,4)*C105(IV1(4),JV1(4))
       END DO CYC_41
       END DO

       END IF

!$omp parallel do &
!$omp& private(i,j,k,n1,DP1,PDIF1,FACT1,IV1,JV1,K1)
       DO J=1,JY
       CYC_85: DO I=1,JX
         IF(IIV1(I,J,1).LT.2.or.IIV1(I,J,1).GT.IX_1)CYCLE CYC_85
         IF(JJV1(I,J,1).LE.2.or.JJV1(I,J,1).GE.IY_1)CYCLE CYC_85
         DO N1=1,4
           IV1(N1)=IIV1(I,J,N1)
           JV1(N1)=JJV1(I,J,N1)
         END DO
         DO K=1,KZ
           CYC_65: DO N1=1,4
             IF(PMV3(I,J,K).GT.PMV2(IV1(N1),JV1(N1),1))THEN
               DP1=PMV3(I,J,K)-PMV2(IV1(N1),JV1(N1),1)
!               U21(I,J,K,N1)=U2(IV1(N1),JV1(N1),1)*(1.-DP1*1.4E-5)
!               V21(I,J,K,N1)=V2(IV1(N1),JV1(N1),1)*(1.-DP1*1.4E-5)
               U21(I,J,K,N1)=U2(IV1(N1),JV1(N1),1)
               V21(I,J,K,N1)=V2(IV1(N1),JV1(N1),1)
             ELSE IF(PMV3(I,J,K).LE.PMV2(IV1(N1),JV1(N1),NZ))THEN
               U21(I,J,K,N1)=U2(IV1(N1),JV1(N1),NZ)
               V21(I,J,K,N1)=V2(IV1(N1),JV1(N1),NZ)
             ELSE
               DO K1=2,KZ
                 PDIF1=ALOG(PMV3(I,J,K))-ALOG(PMV2(IV1(N1),JV1(N1),K1))
                 IF(PDIF1.GE.0.)THEN
                   FACT1=PDIF1/(ALOG(PMV2(IV1(N1),JV1(N1),K1-1))-             &
                                ALOG(PMV2(IV1(N1),JV1(N1),K1)))
                   U21(I,J,K,N1)=U2(IV1(N1),JV1(N1),K1)*(1.-FACT1)     &
                                +U2(IV1(N1),JV1(N1),K1-1)*FACT1
                   V21(I,J,K,N1)=V2(IV1(N1),JV1(N1),K1)*(1.-FACT1)     &
                                +V2(IV1(N1),JV1(N1),K1-1)*FACT1
                   CYCLE CYC_65
                 END IF
               END DO
             END IF
           END DO CYC_65
         END DO
       END DO CYC_85
       END DO
!23456789012345678901234567890123456789012345678901234567890123456789012

!$omp parallel do &
!$omp& private(i,j,k)
       DO J=1,JY
       CYC_115: DO I=1,JX
         IF(IIV1(I,J,1).LT.2.or.IIV1(I,J,1).GT.IX_1)CYCLE CYC_115
         IF(JJV1(I,J,1).LE.2.or.JJV1(I,J,1).GE.IY_1)CYCLE CYC_115
         DO K=1,KZ
            U3(I,J,K) =                       & 
               VBWGT1(I,J,1)*U21(I,J,K,1)      &
             + VBWGT1(I,J,2)*U21(I,J,K,2)      &
             + VBWGT1(I,J,3)*U21(I,J,K,3)      &
             + VBWGT1(I,J,4)*U21(I,J,K,4)
            V3(I,J,K) =                       &
               VBWGT1(I,J,1)*V21(I,J,K,1)      &
             + VBWGT1(I,J,2)*V21(I,J,K,2)      &
             + VBWGT1(I,J,3)*V21(I,J,K,3)      &
             + VBWGT1(I,J,4)*V21(I,J,K,4)
         END DO
       ENDDO CYC_115
       ENDDO

        print*,'JX,JY,KZ=',JX,JY,KZ
        write(*,*)'interpolation after d02='
      do k=1,kz
        write(*,32)K,T3(9,9,K),            &
          Q3(9,9,K),U3(9,9,K),V3(9,9,K),Z3(9,9,K),P3(9,9,K)
      end do

! interpolate from d03

!       GO TO 890

       PMV5=PMID5     
 
!$omp parallel do &
!$omp& private(i,j,k)
       DO J=2,NY5-1
         IF(MOD(J,2).NE.0.)THEN
           DO I=2,NX5-1
           DO K=1,IZ
             PMV5(I,J,K)=0.25*(PMID5(I,J,K)+PMID5(I+1,J,K)+            &
                               PMID5(I,J-1,K)+PMID5(I,J+1,K))
           END DO
           END DO
         ELSE
           DO I=2,NX5-1
           DO K=1,IZ
             PMV5(I,J,K)=0.25*(PMID5(I-1,J,K)+PMID5(I,J,K)+            &
                               PMID5(I,J-1,K)+PMID5(I,J+1,K))
           END DO
           END DO
         END IF
       END DO

!$omp parallel do &
!$omp& private(i,j,k,n1,DP1,PDIF1,FACT1,IV1,JV1,K1)
       DO J=1,JY
       CYC_885: DO I=1,JX
         IF(IIV2(I,J,1).LT.2.or.IIV2(I,J,1).GT.NX5_1)CYCLE CYC_885
         IF(JJV2(I,J,1).LE.2.or.JJV2(I,J,1).GE.NY5_1)CYCLE CYC_885
         DO N1=1,4
           IV1(N1)=IIV2(I,J,N1)
           JV1(N1)=JJV2(I,J,N1)
         END DO
!         A13(I,J) = VBWGT1(I,J,1)*A102(IV1(1),JV1(1))             &
!                  + VBWGT1(I,J,2)*A102(IV1(2),JV1(2))             &
!                  + VBWGT1(I,J,3)*A102(IV1(3),JV1(3))             &
!                  + VBWGT1(I,J,4)*A102(IV1(4),JV1(4))

         DO K=1,KZ
           CYC_665: DO N1=1,4
             IF(PMV3(I,J,K).GT.PMV5(IV1(N1),JV1(N1),1))THEN
               DP1=PMV3(I,J,K)-PMV5(IV1(N1),JV1(N1),1)
               U21(I,J,K,N1)=U5(IV1(N1),JV1(N1),1)
               V21(I,J,K,N1)=V5(IV1(N1),JV1(N1),1)
             ELSE IF(PMV3(I,J,K).LE.PMV5(IV1(N1),JV1(N1),NZ))THEN
               U21(I,J,K,N1)=U5(IV1(N1),JV1(N1),NZ)
               V21(I,J,K,N1)=V5(IV1(N1),JV1(N1),NZ)
             ELSE
               DO K1=2,KZ
                 PDIF1=ALOG(PMV3(I,J,K))-ALOG(PMV5(IV1(N1),JV1(N1),K1))
                 IF(PDIF1.GE.0.)THEN
                   FACT1=PDIF1/(ALOG(PMV5(IV1(N1),JV1(N1),K1-1))-             &
                                ALOG(PMV5(IV1(N1),JV1(N1),K1)))
                   U21(I,J,K,N1)=U5(IV1(N1),JV1(N1),K1)*(1.-FACT1)     &
                                +U5(IV1(N1),JV1(N1),K1-1)*FACT1
                   V21(I,J,K,N1)=V5(IV1(N1),JV1(N1),K1)*(1.-FACT1)     &
                                +V5(IV1(N1),JV1(N1),K1-1)*FACT1
                   CYCLE CYC_665
                 END IF
               END DO
             END IF
           END DO CYC_665
         END DO
       END DO CYC_885
       END DO
!23456789012345678901234567890123456789012345678901234567890123456789012

!$omp parallel do &
!$omp& private(i,j,k)
       DO J=1,JY
       CYC_215: DO I=1,JX
         IF(IIV2(I,J,1).LT.2.or.IIV2(I,J,1).GT.NX5_1)CYCLE CYC_215
         IF(JJV2(I,J,1).LE.2.or.JJV2(I,J,1).GE.NY5_1)CYCLE CYC_215
         DO K=1,KZ
            U3(I,J,K) =                       & 
               VBWGT2(I,J,1)*U21(I,J,K,1)      &
             + VBWGT2(I,J,2)*U21(I,J,K,2)      &
             + VBWGT2(I,J,3)*U21(I,J,K,3)      &
             + VBWGT2(I,J,4)*U21(I,J,K,4)
            V3(I,J,K) =                       &
               VBWGT2(I,J,1)*V21(I,J,K,1)      &
             + VBWGT2(I,J,2)*V21(I,J,K,2)      &
             + VBWGT2(I,J,3)*V21(I,J,K,3)      &
             + VBWGT2(I,J,4)*V21(I,J,K,4)
         END DO
       ENDDO CYC_215
       ENDDO

 890   CONTINUE

        print*,'test74'

! save 4x data

      IUNIT=50+ITIM

      WRITE(IUNIT) JX,JY,KZ,I360
      WRITE(IUNIT) DLMD3,DPHD3,CLON0,CLAT0
      WRITE(IUNIT) PT3,PDTOP3,WBD3,SBD3
      WRITE(IUNIT) T3
      WRITE(IUNIT) Q3
      WRITE(IUNIT) U3
      WRITE(IUNIT) V3
      WRITE(IUNIT) Z3
      WRITE(IUNIT) HLON3,HLAT3,VLON3,VLAT3             ! in degree
      WRITE(IUNIT) P3
      WRITE(IUNIT) PD3
      WRITE(IUNIT) ETA1
      WRITE(IUNIT) ETA2

      CLOSE(IUNIT)

! save A13 

      IF(IBGS.EQ.1.and.abs(DLMD5/DLMD3-1.).lt.0.01)THEN
         DO J=1,NY5
         DO I=1,NX5
           J1=J+J_ST14-1
           I1=I+I_ST14-1
           A13(I1,J1)=A105(I,J)
           B13(I1,J1)=B105(I,J)
           C13(I1,J1)=C105(I,J)
         END DO
         END DO
       END IF

       print*,'test4 C13(jx/2,jy/2)=',C13(jx/2,jy/2)

         IUNIT=60+ITIM

         WRITE(IUNIT) JX,JY,I360
         WRITE(IUNIT) HLON3,HLAT3,VLON3,VLAT3
         WRITE(IUNIT) A13
         WRITE(IUNIT) B13
         WRITE(IUNIT) C13

         CLOSE(IUNIT) 

        print*,'JX,JY,KZ=',JX,JY,KZ
        write(*,*)'inside merge K,T1,Q1,U1,V1,Z1,P1='
      do k=1,kz
        write(*,32)K,T3(9,9,K),            &
          Q3(9,9,K),U3(9,9,K),V3(9,9,K),Z3(9,9,K),P3(9,9,K)
      end do
 32   format(I3,6F12.2)

      print *,'fort.61','JX=',JX,'JY=',JY

!      print*,'1st merge,488,744=',SLP3(488,744),Z3(488,744,1),P3(488,744,1),T3(488,744,1),Q3(488,744,1)

      WRITE(61)((SLP3(I,J),I=1,JX),J=1,JY,2)
      DO K=1,KZ+1
        WRITE(61)((Z3(I,J,K),I=1,JX),J=1,JY,2)
      END DO
      DO K=1,KZ+1
        WRITE(61)((P3(I,J,K),I=1,JX),J=1,JY,2)
      END DO
      DO K=1,KZ
        WRITE(61)((T3(I,J,K),I=1,JX),J=1,JY,2)
      END DO
      DO K=1,KZ
        WRITE(61)((Q3(I,J,K),I=1,JX),J=1,JY,2)
      END DO
      DO K=1,KZ
        WRITE(61)((U3(I,J,K),I=1,JX),J=1,JY,2)
      END DO
      DO K=1,KZ
        WRITE(61)((V3(I,J,K),I=1,JX),J=1,JY,2)
      END DO


       END

!

