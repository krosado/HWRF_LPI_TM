
      PROGRAM HWRF_MERGE_NEST

!??????????????????????????????????????????????????????????
!     
! ABSTRACT: CREATE 4x DEGREE2 HIGH RESOLUTION DOMAIN
!     
!     DECLARE VARIABLES
!
!      IMPLICIT NONE

      INTEGER I,J,K,NX,NY,NZ,NST,IFLAG
!
      PARAMETER (NST=5)
!      PARAMETER (IX=117,IY=225)
!      PARAMETER (NX=215,NY=431,NZ=42,NST=5)
!      PARAMETER (JX=393,JY=735)       ! fixed for 9 km resolution
      PARAMETER (GAMMA=6.5E-3,G=9.8,Rd=287.05,D608=0.608)
      PARAMETER (Cp=1004.)

! Variables on new outer nest hybrid coordinate  (GFS data)

      REAL(4) DLMD3,DPHD3,PT3,PDTOP3              ! use the new inner nest data
      REAL(4) WBD3,SBD3,CLON3,CLAT3

      REAL(4), ALLOCATABLE :: HLON3(:,:),HLAT3(:,:)
      REAL(4), ALLOCATABLE :: VLON3(:,:),VLAT3(:,:)

      REAL(4), ALLOCATABLE :: T4(:,:,:),Q4(:,:,:),U4(:,:,:),V4(:,:,:)
      REAL(4), ALLOCATABLE :: SLP4(:,:)

      REAL(4), ALLOCATABLE :: T3(:,:,:),Q3(:,:,:)
      REAL(4), ALLOCATABLE :: U3(:,:,:),V3(:,:,:)
      REAL(4), ALLOCATABLE :: Z3(:,:,:),P3(:,:,:)
      REAL(4), ALLOCATABLE :: ETA1(:),ETA2(:)
      REAL(4), ALLOCATABLE :: PD3(:,:),PMID3(:,:,:),ZMID3(:,:,:)

      REAL(4), ALLOCATABLE :: SLP3(:,:),SLPV3(:,:)
      REAL(4), ALLOCATABLE :: ZS3(:,:),TS3(:,:),QS3(:,:)

! working arrays used for outer nest interpolation (HWRF 6h forecast data)

      integer(4), ALLOCATABLE :: IIH(:,:),JJH(:,:)
      integer(4), ALLOCATABLE :: IIV(:,:),JJV(:,:)
      REAL(4), ALLOCATABLE :: HBWGT(:,:,:),VBWGT(:,:,:)

! working arrays used for 4x data

      integer(4), ALLOCATABLE :: IIH1(:,:),JJH1(:,:)
      integer(4), ALLOCATABLE :: IIV1(:,:),JJV1(:,:)
      REAL(4), ALLOCATABLE :: HBWGT1(:,:,:),VBWGT1(:,:,:)

! working array

      REAL(4), ALLOCATABLE :: T21(:,:,:,:),Q21(:,:,:,:)
      REAL(4), ALLOCATABLE :: U21(:,:,:,:),V21(:,:,:,:)
      REAL(4), ALLOCATABLE :: SLP21(:,:,:)
      REAL(4), ALLOCATABLE :: PMV1(:,:,:),PMV2(:,:,:),PMV3(:,:,:)

      integer(4) IH1(4),JH1(4),IV1(4),JV1(4)

! Variables old outer nest (6 hour WRF FORECAST)

      REAL(4) DLMD1,DPHD1,PT1,PDTOP1
      REAL(4) WBD1,SBD1,CLON1,CLAT1

      REAL(4), ALLOCATABLE :: T1(:,:,:),Q1(:,:,:)
      REAL(4), ALLOCATABLE :: U1(:,:,:),V1(:,:,:) 
      REAL(4), ALLOCATABLE :: Z1(:,:,:),P1(:,:,:)
      REAL(4), ALLOCATABLE :: HLON1(:,:),HLAT1(:,:)
      REAL(4), ALLOCATABLE :: VLON1(:,:),VLAT1(:,:)
      REAL(4), ALLOCATABLE :: PD1(:,:)

      REAL(4), ALLOCATABLE :: SLP1(:,:)
      REAL(4), ALLOCATABLE :: PMID1(:,:,:),ZMID1(:,:,:)
    
! Variables from 4x data

      REAL(4) DLMD2,DPHD2,PT2,PDTOP2
      REAL(4) WBD2,SBD2,CLON2,CLAT2

      REAL(4), ALLOCATABLE :: T2(:,:,:),Q2(:,:,:)
      REAL(4), ALLOCATABLE :: U2(:,:,:),V2(:,:,:)
      REAL(4), ALLOCATABLE :: Z2(:,:,:),P2(:,:,:)
      REAL(4), ALLOCATABLE :: HLON2(:,:),HLAT2(:,:)
      REAL(4), ALLOCATABLE :: VLON2(:,:),VLAT2(:,:)
      REAL(4), ALLOCATABLE :: PD2(:,:) 
   
      REAL(4), ALLOCATABLE :: SLP2(:,:)
      REAL(4), ALLOCATABLE :: PMID2(:,:,:),ZMID2(:,:,:)

      REAL(4), ALLOCATABLE :: wk1(:,:,:)

      REAL(4), ALLOCATABLE :: WTH(:)

      REAL(8), ALLOCATABLE :: XX1(:),YY1(:)

      REAL(4) RMN

!zhang: added basin domain shift option
      CHARACTER*2 :: basin
      integer, external :: omp_get_max_threads
!!!!!!!!!!!!!!!!11
333   format('Have ',I0,' OpenMP threads.')
      print 333,omp_get_max_threads()

      COEF1=Rd/Cp
      COEF3=Rd*GAMMA/G
      COEF2=1./COEF3

      GRD=G/Rd

      pi=4.*atan(1.)
      pi_deg=180./pi
      pi180=1./pi_deg

      DIST1=6.371E3*pi180

      READ(5,*)ITIM,basin

      print*,'ITIM=',ITIM

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! READ NEW OUTER NEST DATA    ! new data

      IUNIT=40+ITIM
                                                                                                                                                                                         
      READ(IUNIT)JX,JY,KZ
      
      JX1=JX+1
      JY1=JY+1
      KZ1=KZ+1

      print*,'JX,JY,KZ=',JX,JY,KZ

      ALLOCATE ( HLON3(JX,JY),HLAT3(JX,JY) )
      ALLOCATE ( VLON3(JX,JY),VLAT3(JX,JY) )

      ALLOCATE ( T3(JX,JY,KZ),Q3(JX,JY,KZ) )
      ALLOCATE ( U3(JX,JY,KZ),V3(JX,JY,KZ) )
      ALLOCATE ( Z3(JX,JY,KZ1),P3(JX,JY,KZ1) )

      ALLOCATE ( ETA1(KZ1),ETA2(KZ1) )
      ALLOCATE ( PD3(JX,JY) )

      READ(IUNIT) DLMD3,DPHD3,CLON3,CLAT3
      READ(IUNIT) PT3,PDTOP3
      READ(IUNIT) T3
      READ(IUNIT) Q3
      READ(IUNIT) U3
      READ(IUNIT) V3
      READ(IUNIT) Z3
      READ(IUNIT) HLON3,HLAT3,VLON3,VLAT3
      READ(IUNIT) P3
      READ(IUNIT) PD3
      READ(IUNIT) ETA1
      READ(IUNIT) ETA2
                                                                                                                                                                                         
      CLOSE(IUNIT)

      ALLOCATE ( SLP3(JX,JY),SLPV3(JX,JY) )
      ALLOCATE ( PMID3(JX,JY,KZ),ZMID3(JX,JY,KZ) )
      ALLOCATE ( ZS3(JX,JY),TS3(JX,JY),QS3(JX,JY) )

      ALLOCATE ( T4(JX,JY,KZ),Q4(JX,JY,KZ) )
      ALLOCATE ( U4(JX,JY,KZ),V4(JX,JY,KZ) )
      ALLOCATE ( SLP4(JX,JY) )

! save for future use

       T4=T3
       Q4=Q3
       U4=U3
       V4=V3

       DO K=1,KZ
       DO J=1,JY
       DO I=1,JX
         PMID3(I,J,K)=EXP((ALOG(P3(I,J,K))+ALOG(P3(I,J,K+1)))*0.5)
         ZMID3(I,J,K)=0.5*(Z3(I,J,K)+Z3(I,J,K+1))
       ENDDO
       ENDDO
       ENDDO

       IC1=JX/2
       JC1=JY/2
       DO K=1,KZ
         IF(P3(IC1,JC1,K).LE.10000.)THEN
           KTP1=K
           GO TO 456
         END IF
       END DO
 456   CONTINUE        
     
       ALLOCATE ( WTH(KZ) )
 
       WTH=0.           
       DO K=1,KTP1-1
         WTH(K)=1.0
       END DO
       WTH(KTP1)=0.5
                                                                                                                                                                            
!C        COMPUTE SEA LEVEL PRESSURE.
!C
       DO J=1,JY
       DO I=1,JX
         ZSF1 = ZMID3(I,J,1)
         PSF1 = PMID3(I,J,1)
         TV1 = T3(I,J,1)*(1.+D608*Q3(I,J,1))
         A = (GAMMA * ZSF1) / TV1
         SLP3(I,J) = PSF1*(1+A)**COEF2
         TS3(I,J)=T3(I,J,1)+GAMMA*(Z3(I,J,2)-Z3(I,J,1))*0.5
         QS3(I,J)=Q3(I,J,1)
      ENDDO
      ENDDO

      SLP4=SLP3

!      print*,'writing 64 data'

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


      WBD3=-(JX-1)*DLMD3
      SBD3=-((JY-1)/2)*DPHD3

      write(*,*)'DLMD3,DPHD3,PT3,PDTOP3=',DLMD3,DPHD3,PT3,PDTOP3
      write(*,*)'WBD3,SBD3,CLON3,CLAT3=',    &
                 WBD3,SBD3,CLON3,CLAT3


      ALLOCATE ( IIH(JX,JY),JJH(JX,JY) )
      ALLOCATE ( IIV(JX,JY),JJV(JX,JY) )

      ALLOCATE ( IIH1(JX,JY),JJH1(JX,JY) )
      ALLOCATE ( IIV1(JX,JY),JJV1(JX,JY) )

      ALLOCATE ( HBWGT(JX,JY,4),VBWGT(JX,JY,4) )
      ALLOCATE ( HBWGT1(JX,JY,4),VBWGT1(JX,JY,4) )

! working array

      ALLOCATE ( T21(JX,JY,KZ,4),Q21(JX,JY,KZ,4) )
      ALLOCATE ( U21(JX,JY,KZ,4),V21(JX,JY,KZ,4) )
      ALLOCATE ( SLP21(JX,JY,4) )
      ALLOCATE ( PMV3(JX,JY,KZ) )

! LON & LAT at T,U,V

!       CALL EARTH_LATLON ( HLAT3,HLON3,VLAT3,VLON3,        &  !Earth lat,lon at H and V points
!                           DLMD3,DPHD3,WBD3,SBD3,          &  !input res,west & south boundaries,
!                           CLAT3,CLON3,                    &  ! central lat,lon, all in degrees
!                           1,JX1,1,JY1,1,1,                &  
!                           1,JX ,1,JY ,1,1,                &
!                           1,JX ,1,JY ,1,1         )

       print*,'HLAT3,HLON3,VLAT3,VLON3=',                  &
               HLAT3(1,1),HLON3(1,1),VLAT3(1,1),VLON3(1,1)

! READ OUTER NEST DATA    ! 6 hour forecast data

      IUNIT=20+ITIM

      READ(IUNIT) NX,NY,NZ

      print*,'NX,NY,NZ=',NX,NY,NZ

      NX1=NX+1
      NY1=NY+1
      NZ1=NZ+1

      GO TO 728

      ALLOCATE ( T1(NX,NY,NZ),Q1(NX,NY,NZ) )
      ALLOCATE ( U1(NX,NY,NZ),V1(NX,NY,NZ) )
      ALLOCATE ( Z1(NX,NY,NZ1),P1(NX,NY,NZ1) )
      ALLOCATE ( HLON1(NX,NY),HLAT1(NX,NY) )
      ALLOCATE ( VLON1(NX,NY),VLAT1(NX,NY) )
      ALLOCATE ( PD1(NX,NY) )

      READ(IUNIT) DLMD1,DPHD1,CLON1,CLAT1
      READ(IUNIT) PT1,PDTOP1
      READ(IUNIT) T1
      READ(IUNIT) Q1
      READ(IUNIT) U1
      READ(IUNIT) V1
      READ(IUNIT) Z1
      READ(IUNIT) HLON1,HLAT1,VLON1,VLAT1
      READ(IUNIT) P1
      READ(IUNIT) PD1
!      READ(IUNIT) ETA1
!      READ(IUNIT) ETA2
 
      CLOSE(IUNIT)

 728  CONTINUE

      SLPV3=SLP3

      DO J=2,JY-1
        IF(MOD(J,2).NE.0.)THEN
           DO I=1,JX-1
             SLPV3(I,J)=0.25*(SLP3(I,J)+SLP3(I+1,J)+          &
                             SLP3(I,J-1)+SLP3(I,J+1))
           END DO
        ELSE
           DO I=2,JX
             SLPV3(I,J)=0.25*(SLP3(I-1,J)+SLP3(I,J)+          &
                            SLP3(I,J-1)+SLP3(I,J+1))
           END DO
        END IF
      END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! READ 4x NEST DATA    ! 6 hour forecast data

      IUNIT=30+ITIM

      READ(IUNIT) IX,IY,IZ                ! IZ==NZ

      IX1=IX+1
      IY1=IY+1
      IZ1=IZ+1

      ALLOCATE ( T2(IX,IY,IZ),Q2(IX,IY,IZ) )
      ALLOCATE ( U2(IX,IY,IZ),V2(IX,IY,IZ) )
      ALLOCATE ( Z2(IX,IY,IZ1),P2(IX,IY,IZ1) )
      ALLOCATE ( HLON2(IX,IY),HLAT2(IX,IY) )
      ALLOCATE ( VLON2(IX,IY),VLAT2(IX,IY) )
      ALLOCATE ( PD2(IX,IY) )
      
      READ(IUNIT) DLMD2,DPHD2,CLON2,CLAT2
      READ(IUNIT) PT2,PDTOP2
      READ(IUNIT) T2
      READ(IUNIT) Q2
      READ(IUNIT) U2
      READ(IUNIT) V2
      READ(IUNIT) Z2
      READ(IUNIT) HLON2,HLAT2,VLON2,VLAT2
      READ(IUNIT) P2
      READ(IUNIT) PD2
!      READ(IUNIT) ETA1
!      READ(IUNIT) ETA2

      CLOSE(IUNIT)

      do j = 1,IY
      do i = 1,IX
        DO L=2,KZ+1
          Z2(I,J,L)=Z2(I,J,L-1)+T2(I,J,L-1)*          &
              (Q2(I,J,L-1)*0.608+1.0)*287.04*         &
              (ALOG(P2(I,J,L-1))-ALOG(P2(I,J,L)))/G
        ENDDO
       ENDDO
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



!  computer interpolation index for inner nest

       print*,'CLON2,CLAT2=',CLON2,CLAT2
      
      ILOC1=(JX+1)/2-(IX+1)/6                    ! 6=3*2            for 1:3 ratio grid
      JLOC1=(JY+1)/2-(IY+1)/6

      
! inner nest vs. 4x
                                                                                                         
      FLON=HLON2(1,1)
      FLAT=HLAT2(1,1)
                                                                                                         
      print*,'FLON,FLAT=',FLON,FLAT
                                                                                                         
      ERR=1.e20
      DO J=1,JY
      DO I=1,JX
        DIF1=abs(FLON-HLON3(I,J))+abs(FLAT-HLAT3(I,J))
        IF(DIF1.LT.ERR)THEN
          ILOC=I
          JLOC=J
          ERR=DIF1
        END IF
      END DO
      END DO
!                                                                                                         
      WBD2= WBD3 + (ILOC -1)*2.*DLMD3 + MOD(JLOC+1,2)*DLMD3
      SBD2= SBD3 + (JLOC -1)*DPHD3
                                                                                                                               
      print*,'ILOC2,JLOC2=',ILOC,JLOC,WBD2,SBD2,ILOC1,JLOC1
            
      CLON32=HLON2(1+(IX-1)/2,1+(IY-1)/2)
      CLAT32=HLAT2(1+(IX-1)/2,1+(IY-1)/2)

      print*,'news CLON32,CLAT32=',CLON32,CLAT32

      WBD2=-(IX-1)*DLMD2
      SBD2=-((IY-1)/2)*DPHD2
      
      IIH1=999999
      JJH1=999999
      IIV1=999999
      JJV1=999999
      HBWGT1=0.
      VBWGT1=0.
 
      CURRENT_DOMAIN_ID=2
                                                                                                            
       CALL G2T2H_egrid( IIH1,JJH1,                & ! output grid index and weights
                   HBWGT1(1,1,1),HBWGT1(1,1,2),    &
                   HBWGT1(1,1,3),HBWGT1(1,1,4),    &
                   CLAT2,CLON2,                    & ! parent central lat,lon, all in degrees
                   CURRENT_DOMAIN_ID,              & 
                   DLMD2,DPHD2,                    & ! parent res, western and south boundaries
                   HLAT2,HLON2,                    & ! parent  (nest) input lat lon in degrees
                   HLAT3,HLON3,                    & ! target (nest) input lat lon in degrees
                   1,IX,1,IY,                      & ! parent imax and jmax
                   1,IX,1,IY,                      & ! parent imax and jmax
                   1,JX,1,JY         )
                                                                                                          
                                                                                                          
       CALL G2T2V_egrid( IIV1,JJV1,                & ! output grid index and weights
                   VBWGT1(1,1,1),VBWGT1(1,1,2),    &
                   VBWGT1(1,1,3),VBWGT1(1,1,4),    &
                   CLAT2,CLON2,                    & ! parent central lat,lon, all in degrees
                   CURRENT_DOMAIN_ID,              &
                   DLMD2,DPHD2,                    & ! parent res, western and south boundaries
                   VLAT2,VLON2,                    & ! parent (nest) input lat lon in degrees
                   VLAT3,VLON3,                    & ! target (nest) input lat lon in degrees
                   1,IX,1,IY,                      & ! parent imax and jmax
                   1,IX,1,IY,                      & ! parent imax and jmax
                   1,JX,1,JY         )
  
       IX_1=IX-1
       IY_1=IY-1

!$omp parallel do &
!$omp& private(i,j,k,n1,DZ1,ZDIF1,FACT1,IH1,JH1,K1)
       DO J=1,JY
       CYC_45: DO I=1,JX
         IF(IIH1(I,J).LT.1.or.IIH1(I,J).GT.IX_1)CYCLE CYC_45
         IF(JJH1(I,J).LE.2.or.JJH1(I,J).GE.IY_1)CYCLE CYC_45
         IF(MOD(JJH1(I,J),2) .NE. 0)THEN    ! 1,3,5,7
           IH1(1)=IIH1(I,J)
           JH1(1)=JJH1(I,J)
           IH1(2)=IIH1(I,J)+1
           JH1(2)=JJH1(I,J)
           IH1(3)=IIH1(I,J)
           JH1(3)=JJH1(I,J)-1
           IH1(4)=IIH1(I,J)
           JH1(4)=JJH1(I,J)+1
         ELSE
           IH1(1)=IIH1(I,J)
           JH1(1)=JJH1(I,J)
           IH1(2)=IIH1(I,J)+1
           JH1(2)=JJH1(I,J)
           IH1(3)=IIH1(I,J)+1
           JH1(3)=JJH1(I,J)-1
           IH1(4)=IIH1(I,J)+1
           JH1(4)=JJH1(I,J)+1
         END IF
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

       print*,'inside 4to6 test1'

!$omp parallel do &
!$omp& private(i,j)
       DO J=1,JY
       CYC_47: DO I=1,JX
         IF(IIH1(I,J).LT.1.or.IIH1(I,J).GT.IX_1)CYCLE CYC_47
         IF(JJH1(I,J).LE.2.or.JJH1(I,J).GE.IY_1)CYCLE CYC_47
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

      READ(85)RMN
                                                                                                                  
      RMN=min(RMN+5.0,12.)
                                                                                                                  
      cost=cos(CLAT2*pi180)
                                                                                                                  
      RMN5=RMN+1.5

!$omp parallel do &
!$omp& private(i,j,RDST,WT2)
       DO J=1,JY
       DO I=1,JX
         RDST=SQRT(((HLON3(I,J)-CLON32)*cost)**2+(HLAT3(I,J)-CLAT32)**2)
         IF(RDST.GE.RMN5)THEN
           WT2=0.
         ELSE IF(RDST.GT.RMN.and.RDST.LT.RMN5)THEN
           WT2=(RMN5-RDST)/(RMN5-RMN)
           WT2=WT2*WT2*(3.-2.*WT2)
         ELSE
           WT2=1.
         END IF
         SLP3(I,J)=SLP4(I,J)+WT2*(SLP3(I,J)-SLP4(I,J))
       ENDDO
       ENDDO

!??????????????????

! 
! Construct 3D pressure grid

       DO J=1,JY
       DO I=1,JX
         ZS3(I,J)=Z3(I,J,1)
         ZSFC = ZS3(I,J)
         TSFC = TS3(I,J)*(1.+D608*QS3(I,J))
         A = (GAMMA * ZSFC) / TSFC
         P3(I,J,1) = SLP3(I,J)/(1+A)**COEF2
         PD3(I,J)=P3(I,J,1)-PDTOP3-PT3
       ENDDO
       ENDDO
                                                                                                                                      
! PD(I,J)=P1(I,J,1)-PDTOP-PT=PSFC(I,J)-PDTOP-PT
       DO K=1,KZ+1
       DO J=1,JY
       DO I=1,JX
         P3(I,J,K)=PT3+PDTOP3*ETA1(K)+PD3(I,J)*ETA2(K)     ! PD(I,J) changed
       ENDDO
       ENDDO
       ENDDO

       DO K=1,KZ
       DO J=1,JY
       DO I=1,JX
         PMID3(I,J,K)=EXP((ALOG(P3(I,J,K))+ALOG(P3(I,J,K+1)))*0.5)
       ENDDO
       ENDDO
       ENDDO

       GO TO 929

! interpolate vertically to 3D-P level in new coordinate  (H Points)
! from outer nest data

       DO J=1,JY
       DO I=1,JX
         IF(IIH(I,J).LT.2.or.IIH(I,J).GT.NX_1)GO TO 70
         IF(JJH(I,J).LE.2.or.JJH(I,J).GE.NY_1)GO TO 70
         IF(MOD(JJH(I,J),2) .NE. 0)THEN    ! 1,3,5,7
           IH1(1)=IIH(I,J)
           JH1(1)=JJH(I,J)
           IH1(2)=IIH(I,J)+1
           JH1(2)=JJH(I,J)
           IH1(3)=IIH(I,J)
           JH1(3)=JJH(I,J)-1
           IH1(4)=IIH(I,J)
           JH1(4)=JJH(I,J)+1
         ELSE
           IH1(1)=IIH(I,J)
           JH1(1)=JJH(I,J)
           IH1(2)=IIH(I,J)+1
           JH1(2)=JJH(I,J)
           IH1(3)=IIH(I,J)+1
           JH1(3)=JJH(I,J)-1
           IH1(4)=IIH(I,J)+1
           JH1(4)=JJH(I,J)+1
         END IF
!         DO N1=1,4
!           SLP21(I,J,N1)=SLP2(IH1(N1),JH1(N1))
!         END DO
         DO K=1,KZ
         DO N1=1,4
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
                 GO TO 50
               END IF
             END DO
           END IF
 50      CONTINUE
         END DO
         END DO
 70      CONTINUE
       ENDDO
       ENDDO

!
       DO J=1,JY
       DO I=1,JX
         IF(IIH(I,J).LT.2.or.IIH(I,J).GT.NX_1)GO TO 120
         IF(JJH(I,J).LE.2.or.JJH(I,J).GE.NY_1)GO TO 120
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
 120     CONTINUE
       ENDDO
       ENDDO

       DO J=1,JY
       DO I=1,JX
         IF(T3(I,J,1).LT.150.)THEN
           PRINT*,'I,J5=',I,J,T3(I,J,1)
           GO TO 961
         END IF
       ENDDO
       ENDDO
 961   CONTINUE


 929   CONTINUE

! interpolate vertically to 3D-P level in new coordinate  (H Points)
! from inner nest data

!$omp parallel do &
!$omp& private(i,j,k,n1,DZ1,PDIF1,FACT1,IH1,JH1,K1)
       DO J=1,JY
       CYC_73: DO I=1,JX
         IF(IIH1(I,J).LT.1.or.IIH1(I,J).GT.IX_1)CYCLE CYC_73
         IF(JJH1(I,J).LE.2.or.JJH1(I,J).GE.IY_1)CYCLE CYC_73
         IF(MOD(JJH1(I,J),2) .NE. 0)THEN    ! 1,3,5,7
           IH1(1)=IIH1(I,J)
           JH1(1)=JJH1(I,J)
           IH1(2)=IIH1(I,J)+1
           JH1(2)=JJH1(I,J)
           IH1(3)=IIH1(I,J)
           JH1(3)=JJH1(I,J)-1
           IH1(4)=IIH1(I,J)
           JH1(4)=JJH1(I,J)+1
         ELSE
           IH1(1)=IIH1(I,J)
           JH1(1)=JJH1(I,J)
           IH1(2)=IIH1(I,J)+1
           JH1(2)=JJH1(I,J)
           IH1(3)=IIH1(I,J)+1
           JH1(3)=JJH1(I,J)-1
           IH1(4)=IIH1(I,J)+1
           JH1(4)=JJH1(I,J)+1
         END IF
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
 
!
!$omp parallel do &
!$omp& private(i,j,k)
       DO J=1,JY
       CYC_123: DO I=1,JX
         IF(IIH1(I,J).LT.1.or.IIH1(I,J).GT.IX_1)CYCLE CYC_123
         IF(JJH1(I,J).LE.2.or.JJH1(I,J).GE.IY_1)CYCLE CYC_123
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


       DO J=1,JY
       DO I=1,JX
       DO K=1,KZ
         T3(I,J,K)=T4(I,J,K)+wth(K)*(T3(I,J,K)-T4(I,J,K))
         Q3(I,J,K)=Q4(I,J,K)+wth(K)*(Q3(I,J,K)-Q4(I,J,K))
       ENDDO
       ENDDO
       ENDDO

! Replace the data outside 4x to be GFS new data

       Allocate ( XX1(JX),YY1(JY) )

       XX1=0.
       YY1=0.

! 4x vs. new outer nest

       ERR=1.e20
       FLON1=HLON2(1,1)
       FLAT1=HLAT2(1,1)
       DO J=1,JY
       DO I=1,JX
         DIF1=abs(FLON1-HLON3(I,J))+abs(FLAT1-HLAT3(I,J))
         IF(DIF1.LT.ERR)THEN
           I_ST1=I
           J_ST1=J
           ERR=DIF1
         END IF
       END DO
       END DO
       ERR1=ERR

       print*,'I_ST1,J_ST1,ERR1=',I_ST1,J_ST1,ERR1

       ERR=1.e20
       FLON4=HLON2(IX,IY)
       FLAT4=HLAT2(IX,IY)
       DO J=1,JY
       DO I=1,JX
         DIF1=abs(FLON4-HLON3(I,J))+abs(FLAT4-HLAT3(I,J))
         IF(DIF1.LT.ERR)THEN
           I_ST4=I
           J_ST4=J
           ERR=DIF1
         END IF
       END DO
       END DO
       ERR4=ERR

       print*,'I_ST4,J_ST4,ERR4=',I_ST4,J_ST4,ERR4

       I_ST1=I_ST1+2
       J_ST1=J_ST1+2
       I_ST4=I_ST4-2
       J_ST4=J_ST4-2


! Merge two data T1 and T3
                                                                                                                 
      NDTX3=INT(5./(2.*DLMD3))               ! 5 degree
      I_ST2=I_ST1+NDTX3
      I_ST3=I_ST4-NDTX3

      nit=2
      do i=I_ST1,I_ST2
        xxi=FLOAT(i-I_ST1)/FLOAT(NDTX3)
        xx1(i)=xxi
!        call dbend(nit,xxi,xx1(i))
      end do
      do i=I_ST2,I_ST3
        xx1(i)=1.0
      end do
      do i=I_ST3,I_ST4
        xxi=float(I_ST4-i)/FLOAT(NDTX3)
        xx1(i)=xxi
!        call dbend(nit,xxi,xx1(i))
      end do

      NDTY3=INT(5./DPHD3)
      J_ST2=J_ST1+NDTY3
      J_ST3=J_ST4-NDTY3

      nit=2
      do i=J_ST1,J_ST2
        yyi=FLOAT(i-J_ST1)/FLOAT(NDTY3)
        yy1(i)=yyi
!        call dbend(nit,yyi,yy1(i))
      end do
      do i=J_ST2,J_ST3
        yy1(i)=1.0
      end do
      do i=J_ST3,J_ST4
        yyi=float(J_ST4-i)/FLOAT(NDTY3)
        yy1(i)=yyi
!        call dbend(nit,yyi,yy1(i))
      end do

      print*,'I_ST1,I_ST2,I_ST3,I_ST4=',I_ST1,I_ST2,I_ST3,I_ST4
      print*,'J_ST1,J_ST2,J_ST3,J_ST4=',J_ST1,J_ST2,J_ST3,J_ST4

      print*,'NDTY3,NDTX3=',NDTY3,NDTX3

       print*,'xx1,yy1'
       do J=1,JY
         print*,'j,yyi=',J,YY1(J)
       end do
       do I=1,JX
         print*,'i,xxi=',i,XX1(I)
       end do 

!$omp parallel do &
!$omp& private(i,j,k,RDST,WT2)
       DO J=1,JY
       DO I=1,JX
!         WT2=XX1(I)*YY1(J)
         RDST=SQRT(((HLON3(I,J)-CLON32)*cost)**2+(HLAT3(I,J)-CLAT32)**2)
         IF(RDST.GE.RMN5)THEN
           WT2=0.
         ELSE IF(RDST.GT.RMN.and.RDST.LT.RMN5)THEN
           WT2=(RMN5-RDST)/(RMN5-RMN)
           WT2=WT2*WT2*(3.-2.*WT2)
         ELSE
           WT2=1.
         END IF
         DO K=1,KZ
           T3(I,J,K)=T4(I,J,K)+WT2*(T3(I,J,K)-T4(I,J,K))
           Q3(I,J,K)=Q4(I,J,K)+WT2*(Q3(I,J,K)-Q4(I,J,K))
         END DO
       ENDDO
       ENDDO

 
       DO J=1,JY
       DO I=1,JX
         IF(T3(I,J,1).LT.150.)THEN
           PRINT*,'I,J3=',I,J,T3(I,J,1)
           GO TO 971
         END IF
       ENDDO
       ENDDO
 971   CONTINUE


! Compute Geopotentital height, INTEGRATE HEIGHT HYDROSTATICLY
            
!$omp parallel do &
!$omp& private(i,j,ZSFC,TSFC11,TSFC,A)
       DO J=1,JY
       DO I=1,JX
         ZSFC = ZS3(I,J)
	 TSFC11=T3(I,J,1)+GAMMA*(Z3(I,J,2)-Z3(I,J,1))*0.5
         TSFC = TSFC11*(1.+D608*Q3(I,J,1))
         A = (GAMMA * ZSFC) / TSFC
         P3(I,J,1) = SLP3(I,J)/(1+A)**COEF2
         PD3(I,J)=P3(I,J,1)-PDTOP3-PT3
       ENDDO
       ENDDO
                                                                                                                                                                                         
! PD(I,J)=P1(I,J,1)-PDTOP-PT=PSFC(I,J)-PDTOP-PT
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

       PMV3=PMID3


! interpolate vertically to P level in new coordinate  (V Points)
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

       GO TO 931

       PMV1=PMID1     
 
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

       DO J=1,JY
       CYC_80: DO I=1,JX
         IF(IIV(I,J).LT.1.or.IIV(I,J).GT.NX_1)CYCLE CYC_80
         IF(JJV(I,J).LE.2.or.JJV(I,J).GE.NY_1)CYCLE CYC_80
         IF(MOD(JJV(I,J),2) .NE. 0)THEN    ! 1,3,5,7
           IV1(1)=IIV(I,J)
           JV1(1)=JJV(I,J)
           IV1(2)=IIV(I,J)+1
           JV1(2)=JJV(I,J)
           IV1(3)=IIV(I,J)+1
           JV1(3)=JJV(I,J)-1
           IV1(4)=IIV(I,J)+1
           JV1(4)=JJV(I,J)+1
         ELSE
           IV1(1)=IIV(I,J)
           JV1(1)=JJV(I,J)
           IV1(2)=IIV(I,J)+1
           JV1(2)=JJV(I,J)
           IV1(3)=IIV(I,J)
           JV1(3)=JJV(I,J)-1
           IV1(4)=IIV(I,J)
           JV1(4)=JJV(I,J)+1
         END IF
         DO K=1,KZ
           CYC_60: DO N1=1,4
             IF(PMV3(I,J,K).GT.PMV1(IV1(N1),JV1(N1),1))THEN
               DP1=PMV3(I,J,K)-PMV1(IV1(N1),JV1(N1),1)
               U21(I,J,K,N1)=U1(IV1(N1),JV1(N1),1)*(1.-DP1*1.4E-5)
               V21(I,J,K,N1)=V1(IV1(N1),JV1(N1),1)*(1.-DP1*1.4E-5)
!               U21(I,J,K,N1)=U1(IV1(N1),JV1(N1),1)
!               V21(I,J,K,N1)=V1(IV1(N1),JV1(N1),1)
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
!23456789012345678901234567890123456789012345678901234567890123456789012

       DO J=1,JY
       CYC_110: DO I=1,JX
         IF(IIV(I,J).LT.1.or.IIV(I,J).GT.NX_1)CYCLE CYC_110
         IF(JJV(I,J).LE.2.or.JJV(I,J).GE.NY_1)CYCLE CYC_110
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
       ENDDO CYC_110
       ENDDO

 931   CONTINUE

       PMV2=PMID2     
 
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

!$omp parallel do &
!$omp& private(i,j,k,n1,DP1,PDIF1,FACT1,IV1,JV1,K1)
       DO J=1,JY
       CYC_85: DO I=1,JX
         IF(IIV1(I,J).LT.1.or.IIV1(I,J).GT.IX_1)CYCLE CYC_85
         IF(JJV1(I,J).LE.2.or.JJV1(I,J).GE.IY_1)CYCLE CYC_85
         IF(MOD(JJV1(I,J),2) .NE. 0)THEN    ! 1,3,5,7
           IV1(1)=IIV1(I,J)
           JV1(1)=JJV1(I,J)
           IV1(2)=IIV1(I,J)+1
           JV1(2)=JJV1(I,J)
           IV1(3)=IIV1(I,J)+1
           JV1(3)=JJV1(I,J)-1
           IV1(4)=IIV1(I,J)+1
           JV1(4)=JJV1(I,J)+1
         ELSE
           IV1(1)=IIV1(I,J)
           JV1(1)=JJV1(I,J)
           IV1(2)=IIV1(I,J)+1
           JV1(2)=JJV1(I,J)
           IV1(3)=IIV1(I,J)
           JV1(3)=JJV1(I,J)-1
           IV1(4)=IIV1(I,J)
           JV1(4)=JJV1(I,J)+1
         END IF
         DO K=1,KZ
           CYC_65: DO N1=1,4
             IF(PMV3(I,J,K).GT.PMV2(IV1(N1),JV1(N1),1))THEN
               DP1=PMV3(I,J,K)-PMV2(IV1(N1),JV1(N1),1)
               U21(I,J,K,N1)=U2(IV1(N1),JV1(N1),1)*(1.-DP1*1.4E-5)
               V21(I,J,K,N1)=V2(IV1(N1),JV1(N1),1)*(1.-DP1*1.4E-5)
!               U21(I,J,K,N1)=U2(IV1(N1),JV1(N1),1)
!               V21(I,J,K,N1)=V2(IV1(N1),JV1(N1),1)
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
         IF(IIV1(I,J).LT.1.or.IIV1(I,J).GT.IX_1)CYCLE CYC_115
         IF(JJV1(I,J).LE.2.or.JJV1(I,J).GE.IY_1)CYCLE CYC_115
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

!$omp parallel do &
!$omp& private(i,j,k,RDST,WT2)
       DO J=1,JY
       DO I=1,JX
!         WT2=XX1(I)*YY1(J)
         RDST=SQRT(((VLON3(I,J)-CLON32)*cost)**2+(VLAT3(I,J)-CLAT32)**2)
         IF(RDST.GE.RMN5)THEN
           WT2=0.
         ELSE IF(RDST.GT.RMN.and.RDST.LT.RMN5)THEN
           WT2=(RMN5-RDST)/(RMN5-RMN)
           WT2=WT2*WT2*(3.-2.*WT2)
         ELSE
           WT2=1.
         END IF
         DO K=1,KZ
           U3(I,J,K)=U4(I,J,K)+WT2*(U3(I,J,K)-U4(I,J,K))
           V3(I,J,K)=V4(I,J,K)+WT2*(V3(I,J,K)-V4(I,J,K))
         END DO
       ENDDO
       ENDDO

!      WRITE(63)((SLP3(I,J),I=1,JX),J=1,JY,2)
!      DO K=1,KZ+1
!        WRITE(63)((Z3(I,J,K),I=1,JX),J=1,JY,2)
!      END DO
!      DO K=1,KZ+1
!        WRITE(63)((P3(I,J,K),I=1,JX),J=1,JY,2)
!      END DO
!      DO K=1,KZ
!        WRITE(63)((T3(I,J,K),I=1,JX),J=1,JY,2)
!      END DO
!      DO K=1,KZ
!        WRITE(63)((Q3(I,J,K),I=1,JX),J=1,JY,2)
!      END DO
!      DO K=1,KZ
!        WRITE(63)((U3(I,J,K),I=1,JX),J=1,JY,2)
!      END DO
!      DO K=1,KZ
!        WRITE(63)((V3(I,J,K),I=1,JX),J=1,JY,2)
!      END DO

! save 4x data

      IUNIT=50+ITIM

      WRITE(IUNIT) JX,JY,KZ
      WRITE(IUNIT) DLMD3,DPHD3,CLON3,CLAT3
      WRITE(IUNIT) PT3,PDTOP3
      WRITE(IUNIT) T3
      WRITE(IUNIT) Q3
      WRITE(IUNIT) U3
      WRITE(IUNIT) V3
      WRITE(IUNIT) Z3
      WRITE(IUNIT) HLON3,HLAT3,VLON3,VLAT3
      WRITE(IUNIT) P3
      WRITE(IUNIT) PD3
      WRITE(IUNIT) ETA1
      WRITE(IUNIT) ETA2

      CLOSE(IUNIT)

       END

!=============================================================================
subroutine dbend(nit,x,y)
!=============================================================================
! Evaluate a smooth monotonic increasing blending function y from 0 to 1
! for x in the interval [0,1] having continuity in at least the first nit
! derivatives at the ends of this interval. (nit .ge. 0).
!=============================================================================
implicit none
integer,intent(IN ):: nit
real(8), intent(IN ):: x
real(8),intent(OUT):: y
!-----------------------------------------------------------------------------
integer            :: it
!=============================================================================
y=2*x-1; do it=1,nit; y=y*(3-y*y)/2; enddo; y=(y+1)/2
end subroutine dbend

