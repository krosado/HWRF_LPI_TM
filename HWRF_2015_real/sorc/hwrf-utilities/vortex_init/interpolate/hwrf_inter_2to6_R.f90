
      PROGRAM HWRF_MERGE_NEST

!??????????????????????????????????????????????????????????
!     
! ABSTRACT: CREATE 6x DEGREE2 HIGH RESOLUTION DOMAIN
!     
!     DECLARE VARIABLES
!
!      IMPLICIT NONE

      INTEGER I,J,K,NX,NY,NZ,NST,iblend,idomain
      REAL(8) xxi,yyi
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

      REAL(4), ALLOCATABLE :: HLON3A(:,:),HLAT3A(:,:)
      REAL(4), ALLOCATABLE :: VLON3A(:,:),VLAT3A(:,:)

      REAL(4), ALLOCATABLE :: T3(:,:,:),Q3(:,:,:)
      REAL(4), ALLOCATABLE :: U3(:,:,:),V3(:,:,:)
      REAL(4), ALLOCATABLE :: Z3(:,:,:),P3(:,:,:)
      REAL(4), ALLOCATABLE :: ETA1(:),ETA2(:)
      REAL(4), ALLOCATABLE :: PD3(:,:),PMID3(:,:,:),ZMID3(:,:,:)

      REAL(4), ALLOCATABLE :: SLP3(:,:),SLPV3(:,:)
      REAL(4), ALLOCATABLE :: ZS3(:,:),TS3(:,:),QS3(:,:)

! working arrays used for outer nest interpolation (HWRF 6h forecast data)

! working arrays used for 6x data

      integer(4), ALLOCATABLE :: IIH1(:,:),JJH1(:,:)
      integer(4), ALLOCATABLE :: IIV1(:,:),JJV1(:,:)
      REAL(4), ALLOCATABLE :: HBWGT1(:,:,:),VBWGT1(:,:,:)

! working array

      REAL(4), ALLOCATABLE :: T21(:,:,:,:),Q21(:,:,:,:)
      REAL(4), ALLOCATABLE :: U21(:,:,:,:),V21(:,:,:,:)
      REAL(4), ALLOCATABLE :: SLP21(:,:,:)
      REAL(4), ALLOCATABLE :: PMV2(:,:,:),PMV3(:,:,:)

      integer(4) IH1(4),JH1(4),IV1(4),JV1(4)

! Variables old outer nest (6 hour WRF FORECAST)

      REAL(4) DLMD1,DPHD1,PT1,PDTOP1
      REAL(4) WBD1,SBD1,CLON1,CLAT1

      REAL(4), ALLOCATABLE :: T1(:,:,:),Q1(:,:,:)
      REAL(4), ALLOCATABLE :: U1(:,:,:),V1(:,:,:) 
      REAL(4), ALLOCATABLE :: Z1(:,:,:),P1(:,:,:)
      REAL(4), ALLOCATABLE :: PD1(:,:)

      REAL(4), ALLOCATABLE :: SLP1(:,:)
      REAL(4), ALLOCATABLE :: PMID1(:,:,:),ZMID1(:,:,:)
    
! Variables from 2x data

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

      REAL(8), ALLOCATABLE :: xx1(:),yy1(:)
      REAL(4), ALLOCATABLE :: DUMA(:,:),blend(:,:),blenda(:,:)

! Variables from inner nest

      REAL(4), ALLOCATABLE :: HLON1(:,:),HLAT1(:,:)
      integer, external :: omp_get_max_threads
      REAL(4) RMN

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

      READ(5,*)ITIM,iblend,idomain

      print*,'ITIM=',ITIM

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! READ NEW OUTER NEST DATA    ! new data

      IUNIT=40+ITIM
                                                                                                                                                                                         
      READ(IUNIT)JX,JY,KZ
      
      JX1=JX+1
      JY1=JY+1
      KZ1=KZ+1

      NZ=KZ

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

      do j = 1,JY
      do i = 1,JX
        DO L=2,KZ+1
          Z3(I,J,L)=Z3(I,J,L-1)+T3(I,J,L-1)*          &
              (Q3(I,J,L-1)*0.608+1.0)*287.04*         &
              (ALOG(P3(I,J,L-1))-ALOG(P3(I,J,L)))/G
        ENDDO
      ENDDO
      END DO
                                                                                                                                                  
!  save T3,Q3,U3,V3,P3, Z3 for later use

      ALLOCATE ( T1(JX,JY,KZ),Q1(JX,JY,KZ) )
      ALLOCATE ( U1(JX,JY,KZ),V1(JX,JY,KZ) )
      ALLOCATE ( Z1(JX,JY,KZ1),P1(JX,JY,KZ1) )
      ALLOCATE ( SLP1(JX,JY) )
 

      T1=T3
      Q1=Q3 
      U1=U3
      V1=V3
      P1=P3
      Z1=Z3


      ALLOCATE ( SLP3(JX,JY),SLPV3(JX,JY) )
      ALLOCATE ( PMID3(JX,JY,KZ),ZMID3(JX,JY,KZ) )
      ALLOCATE ( ZS3(JX,JY),TS3(JX,JY),QS3(JX,JY) )


       DO K=1,KZ
       DO J=1,JY
       DO I=1,JX
         PMID3(I,J,K)=EXP((ALOG(P3(I,J,K))+ALOG(P3(I,J,K+1)))*0.5)
         ZMID3(I,J,K)=0.5*(Z3(I,J,K)+Z3(I,J,K+1))
       ENDDO
       ENDDO
       ENDDO
      

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

      SLP1=SLP3

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


      ALLOCATE ( IIH1(JX,JY),JJH1(JX,JY) )
      ALLOCATE ( IIV1(JX,JY),JJV1(JX,JY) )

      ALLOCATE ( HBWGT1(JX,JY,4),VBWGT1(JX,JY,4) )

! working array

      ALLOCATE ( T21(JX,JY,KZ,4),Q21(JX,JY,KZ,4) )
      ALLOCATE ( U21(JX,JY,KZ,4),V21(JX,JY,KZ,4) )
      ALLOCATE ( SLP21(JX,JY,4) )
      ALLOCATE ( PMV3(JX,JY,KZ) )


       print*,'HLAT3,HLON3,VLAT3,VLON3=',                  &
               HLAT3(1,1),HLON3(1,1),VLAT3(1,1),VLON3(1,1)


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
! READ INNER NEST DATA

      IUNIT=20+ITIM
                                                                                                                                  
      READ(IUNIT) KX,KY

      ALLOCATE ( HLON1(KX,KY),HLAT1(KX,KY) )

      READ(IUNIT)
      READ(IUNIT)
      READ(IUNIT)
      READ(IUNIT)   ! Q2
      READ(IUNIT)   ! U2
      READ(IUNIT)   ! V2
      READ(IUNIT)   ! Z2
      READ(IUNIT) HLON1,HLAT1
      READ(IUNIT)   ! P2
      READ(IUNIT)   ! PD2
!      READ(IUNIT) ETA1
!      READ(IUNIT) ETA2
                                                                                                                                  
      CLOSE(IUNIT)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! READ 2x NEST DATA    ! New analysis data

      IUNIT=30+ITIM

      READ(IUNIT) IX,IY,IZ                ! IZ==NZ

      IX1=IX+1
      IY1=IY+1
      IZ1=IZ+1

      print*,'IX,IY,IZ=',IX,IY,IZ

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
      READ(IUNIT) ETA1
      READ(IUNIT) ETA2

      CLOSE(IUNIT)

      HLON2_C=HLON2(IX/2,IY/2)
      I360=180
      if(abs(HLON2_C).gt.90.)then
         I360=360
      end if

      if(I360.eq.360) then
        IF(CLON3.GT.0.)CLON3=CLON3-360.
        IF(CLON2.GT.0.)CLON2=CLON2-360.
        DO J=1,JY
        DO I=1,JX
           IF(HLON3(I,J).GT.0.)HLON3(I,J)=HLON3(I,J)-360.
           IF(VLON3(I,J).GT.0.)VLON3(I,J)=VLON3(I,J)-360.
        END DO
        END DO
        DO J=1,IY
        DO I=1,IX
           IF(HLON2(I,J).GT.0.)HLON2(I,J)=HLON2(I,J)-360.
           IF(VLON2(I,J).GT.0.)VLON2(I,J)=VLON2(I,J)-360.
        END DO
        END DO
        DO J=1,KY
        DO I=1,KX
           IF(HLON1(I,J).GT.0.)HLON1(I,J)=HLON1(I,J)-360.
        END DO
        END DO
      END IF

      DO K=1,IZ1
      DO J=1,IY
      DO I=1,IX
	P2(I,J,K)=PT2+PDTOP2*ETA1(K)+PD2(I,J)*ETA2(K)     ! PD(I,J) changed
      END DO
      END DO
      END DO


      do j = 1,IY
      do i = 1,IX
        DO L=2,IZ1
          Z2(I,J,L)=Z2(I,J,L-1)+T2(I,J,L-1)*          &
              (Q2(I,J,L-1)*0.608+1.0)*287.04*         &
              (ALOG(P2(I,J,L-1))-ALOG(P2(I,J,L)))/G
        ENDDO
       ENDDO
      END DO

      ALLOCATE ( SLP2(IX,IY) )
      ALLOCATE ( PMID2(IX,IY,IZ),ZMID2(IX,IY,IZ) )
      ALLOCATE ( PMV2(IX,IY,IZ) )

      write(*,*)P2(41,3,1),P2(41,3,2)
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

      write(*,*)P2(41,3,1),P2(41,3,2)
      write(*,58)ZMID2(41,3,1),PMID2(42,3,1),T2(41,2,1),Q2(41,4,1)
      write(*,58)SLP2(41,3),SLP2(42,3),SLP2(41,2),SLP2(41,4)
 58   format(10F10.2)

!  computer interpolation index for inner nest

       print*,'CLON2,CLAT2=',CLON2,CLAT2
     
      ERR=1.e20
      DO J=1,JY
      DO I=1,JX
        DIF1=abs(HLON2(1,1)-HLON3(I,J))+abs(HLAT2(1,1)-HLAT3(I,J))
        IF(DIF1.LT.ERR)THEN
          ILOC=I
          JLOC=J
          ERR=DIF1
        END IF
      END DO
      END DO

      print*,'ILOC,JLOC,ERR=',ILOC,JLOC,ERR

      WBD2=WBD3 + (ILOC -1)*2.*DLMD3 + MOD(JLOC+1,2)*DLMD3
      SBD2=SBD3 + (JLOC -1)*DPHD3

!      NDX=(ILOC+100)*3*2
!      NDY=(JLOC+100)*3*2
      NDX=0
      NDY=0
      IXDX=IX+NDX
      IYDY=IY+NDY
 
      WBD2=WBD2-DLMD2*NDX
      SBD2=SBD2-DPHD2*(NDY/2)

      CLON32=HLON2(1+(IX-1)/2,1+(IY-1)/2)
      CLAT32=HLAT2(1+(IX-1)/2,1+(IY-1)/2)

      print*,'news CLON32,CLAT32=',CLON32,CLAT32

      WBD2=-(IXDX-1)*DLMD2
      SBD2=-((IYDY-1)/2)*DPHD2

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
                   CURRENT_DOMAIN_ID,              & ! input source domain ID
                   DLMD2,DPHD2,                    & ! parent res, western and south boundaries
                   HLAT2,HLON2,                    & ! parent (nest) input lat lon in degrees
                   HLAT3,HLON3,                    & ! target (nest) input lat lon in degrees
                   1,IX,1,IY,                      & ! parent imax and jmax
                   1,IX,1,IY,                      & ! parent imax and jmax
                   1,JX,1,JY         )
                                                                                                          
                                                                                                          
       CALL G2T2V_egrid( IIV1,JJV1,                & ! output grid index and weights
                   VBWGT1(1,1,1),VBWGT1(1,1,2),    &
                   VBWGT1(1,1,3),VBWGT1(1,1,4),    &
                   CLAT2,CLON2,                    & ! parent central lat,lon, all in degrees
                   CURRENT_DOMAIN_ID,              & ! input source domain ID
                   DLMD2,DPHD2,                    & ! parent res, western and south boundaries
                   VLAT2,VLON2,                    & ! target (nest) input lat lon in degrees
                   VLAT3,VLON3,                    & ! target (nest) input lat lon in degrees
                   1,IX,1,IY,                      & ! parent imax and jmax
                   1,IX,1,IY,                      & ! parent imax and jmax
                   1,JX,1,JY         )

       
       DO J=1,JY
       DO I=1,JX
         IIH1(I,J)=IIH1(I,J)-NDX/2
         JJH1(I,J)=JJH1(I,J)-NDY/2
         IIV1(I,J)=IIV1(I,J)-NDX/2
         JJV1(I,J)=JJV1(I,J)-NDY/2
       END DO
       END DO

       IX_1=IX-1
       IY_1=IY-1

       SLP21=0.
       T21=0.
       Q21=0.

!$omp parallel do &
!$omp& private(i,j,k,n1,DZ1,ZDIF1,FACT1,IH1,JH1,K1)
       DO J=1,JY
       CYC_45: DO I=1,JX
         IF(IIH1(I,J).LT.1.or.IIH1(I,J).GT.IX_1)CYCLE CYC_45
         IF(JJH1(I,J).LE.2.or.JJH1(I,J).GE.IY_1)CYCLE CYC_45
!           write(98,*)I,J,IIH1(I,J),JJH1(I,J),                     &
!		  HBWGT1(I,J,1),HBWGT1(I,J,2),HBWGT1(I,J,3),HBWGT1(I,J,4)
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
!           IF(I.Eq.84.and.J.eq.121)then
!             print*,'SLP21,SLP2=',SLP21(I,J,N1),SLP2(IH1(N1),JH1(N1))
!             print*,'I,J,IH1,JH1=',I,J,IH1(N1),JH1(N1)
!           end if
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


      if(iblend == 1)then
         if(idomain == 1)then
            READ(85)RMN
            RMN=min(RMN+5.,11.0)
         else
            RMN=3.5
         end if

         cost=cos(CLAT2*pi180)

         R_NEST=SQRT((cost*(HLON1(KX,KY)-HLON1(1,1)))**2+     &
                  (HLAT1(KX,KY)-HLAT1(1,1))**2)
         RMN=MAX(RMN,R_NEST/2.)
         if(idomain == 1)then
            RMN5=RMN+1.5
         else
            RMN5=RMN+1.0
         end if

         print*,'inside _R RMN=',RMN

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
             SLP3(I,J)=SLP1(I,J)+WT2*(SLP3(I,J)-SLP1(I,J))
           ENDDO
         ENDDO

      else

         JX2=JX*2-1

         ALLOCATE( HLON3A(JX2,JY),HLAT3A(JX2,JY) )
         ALLOCATE ( xx1(JX2),yy1(JY) )

         call fill_nmm_gridg(HLON3,JX,JY,HLON3A,1)
         call fill_nmm_gridg(HLAT3,JX,JY,HLAT3A,1)

! lower left corner for the 2x dat & the inner nest

         ERR=1.e20
         FLON1=HLON2(1,1)
         FLAT1=HLAT2(1,1)
         DO J=1,JY
         DO I=1,JX2
           DIF1=abs(FLON1-HLON3A(I,J))+abs(FLAT1-HLAT3A(I,J))
           IF(DIF1.LT.ERR)THEN
             I_ST1=I
             J_ST1=J
             ERR=DIF1
              END IF
         END DO
         END DO
         ERR1=ERR
   
         ERR=1.e20
         FLON2=HLON1(1,1)
         FLAT2=HLAT1(1,1)
         DO J=1,JY
         DO I=1,JX2
           DIF1=abs(FLON2-HLON3A(I,J))+abs(FLAT2-HLAT3A(I,J))
           IF(DIF1.LT.ERR)THEN
             I_ST2=I
             J_ST2=J
             ERR=DIF1
           END IF
         END DO
         END DO
         ERR2=ERR
   
! upper right corner for the inner nest and the 2x data
   
         ERR=1.e20
         FLON3=HLON1(KX,KY)
         FLAT3=HLAT1(KX,KY)
         DO J=1,JY
         DO I=1,JX2
           DIF1=abs(FLON3-HLON3A(I,J))+abs(FLAT3-HLAT3A(I,J))
           IF(DIF1.LT.ERR)THEN
             I_ST3=I
             J_ST3=J
             ERR=DIF1
           END IF
         END DO
         END DO
         ERR3=ERR
   
         ERR=1.e20
         FLON4=HLON2(IX,IY)
         FLAT4=HLAT2(IX,IY)
         DO J=1,JY
         DO I=1,JX2
           DIF1=abs(FLON4-HLON3A(I,J))+abs(FLAT4-HLAT3A(I,J))
           IF(DIF1.LT.ERR)THEN
             I_ST4=I
             J_ST4=J
             ERR=DIF1
           END IF
         END DO
         END DO
         ERR4=ERR
   
         DEALLOCATE(HLON3A,HLAT3A)
   
         I_ST1=I_ST1+2
         J_ST1=J_ST1+2
         I_ST2=I_ST2-2
         J_ST2=J_ST2-2
         I_ST3=I_ST3+2
         J_ST3=J_ST3+2
         I_ST4=I_ST4-2
         J_ST4=J_ST4-2

         print *,'FLON1,FLAT1=',FLON1,FLAT1
         print *,'FLON2,FLAT2=',FLON2,FLAT2
         print *,'FLON3,FLAT3=',FLON3,FLAT3
         print *,'FLON4,FLAT4=',FLON4,FLAT4
  
         IXD1=I_ST2-I_ST1
         IYD1=J_ST2-J_ST1
         IXD2=I_ST4-I_ST3
         IYD2=J_ST4-J_ST3
   
         print *,'IXD1,IYD1,IXD2,IYD2=',IXD1,IYD1,IXD2,IYD2

         xx1=0.
         yy1=0.

         nit=1
         do i=I_ST1,I_ST2
           xxi=FLOAT(i-I_ST1)/FLOAT(IXD1)
           xx1(i)=xxi
           call dbend(nit,xxi,xx1(i))
         end do
         do i=I_ST2+1,I_ST3-1
           xx1(i)=1.0
         end do
         do i=I_ST3,I_ST4
           xxi=float(I_ST4-i)/FLOAT(IXD2)
           xx1(i)=xxi
           call dbend(nit,xxi,xx1(i))
         end do
         do i=J_ST1,J_ST2
           yyi=FLOAT(i-J_ST1)/FLOAT(IYD1)
           yy1(i)=yyi
           call dbend(nit,yyi,yy1(i))
         end do
         do i=J_ST2+1,J_ST3-1
           yy1(i)=1.0
         end do
         do i=J_ST3,J_ST4
           yyi=float(J_ST4-i)/FLOAT(IYD2)
           yy1(i)=yyi
           call dbend(nit,yyi,yy1(i))
         end do

         print*,'I_ST1,J_ST1,ERR=',I_ST1,J_ST1,ERR1
         print*,'I_ST2,J_ST2,ERR=',I_ST2,J_ST2,ERR2
         print*,'I_ST3,J_ST3,ERR=',I_ST3,J_ST3,ERR3
         print*,'I_ST4,J_ST4,ERR=',I_ST4,J_ST4,ERR4

         ALLOCATE(blenda(JX2,JY))
         blenda=0.

         DO J=1,JY
            DO I=1,JX2
               blenda(I,J)=xx1(I)*yy1(J)
            END DO
         END DO

         call open_grads('blenda',JX2,JY,1,1.0,1.0,1.0,1.0)
         open(91,file='blenda',form='unformatted')
         rewind 91

         write(91) blenda

         close(91)

         ALLOCATE(blend(JX,JY))
         call unfill_nmm_gridg(blenda,JX,JY,blend,1)

         do J=1,JY
            do I=1,JX
               SLP3(I,J)=SLP1(I,J)+blend(I,J)*(SLP3(I,J)-SLP1(I,J))
            end do
         end do
       end if

!??????????????????
! 
! Construct 3D pressure grid

       DO J=1,JY
       DO I=1,JX
         ZS3(I,J)=Z1(I,J,1)
         ZSFC = ZS3(I,J)
         TSFC = TS3(I,J)*(1.+D608*QS3(I,J))
         A = (GAMMA * ZSFC) / TSFC
         P3(I,J,1) = SLP3(I,J)/(1+A)**COEF2
         PD3(I,J)=P3(I,J,1)-PDTOP3-PT3
       ENDDO
       ENDDO
                                                                                                                                      
       print*,'TS3=',TS3(100,200),P3(100,200,1)

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
        
          print*,'PD3(I,J)=',Z3(84,158,1),SLP3(84,158),P3(84,158,1)

          print*,'SLP21=',SLP21(84,158,1),SLP21(84,158,2),SLP21(84,158,3),SLP21(84,158,4)

          print*,'PD3(I,J)=',PD3(84,158),ZS3(84,158),TS3(84,158),QS3(84,158)

               DO K=1,KZ
                 print*,'K,PMID3,PMID2=',K,PMID3(84,158,K),P3(84,158,K)
               END DO

! interpolate vertically to 3D-P level in new coordinate  (H Points)
! from outer nest data

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

!            IF(T21(I,J,K,2).lt.100.)print*,'T21_2=',T21(I,J,K,2)
!            IF(T21(I,J,K,3).lt.100.)print*,'T21_3=',T21(I,J,K,3)
!            IF(T21(I,J,K,4).lt.100.)print*,'T21_4=',T21(I,J,K,4)

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

  
       if(iblend == 1)then
!$omp parallel do &
!$omp& private(i,j,k,RDST,WT2)
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
            DO K=1,KZ
              T3(I,J,K)=T1(I,J,K)+WT2*(T3(I,J,K)-T1(I,J,K))
              Q3(I,J,K)=Q1(I,J,K)+WT2*(Q3(I,J,K)-Q1(I,J,K))
            END DO
          ENDDO
          ENDDO
       else
!$omp parallel do &
!$omp& private(i,j,k)
          DO K=1,KZ
            DO J=1,JY
              DO I=1,JX
                 T3(I,J,K)=T1(I,J,K)+blend(I,J)*(T3(I,J,K)-T1(I,J,K))
                 Q3(I,J,K)=Q1(I,J,K)+blend(I,J)*(Q3(I,J,K)-Q1(I,J,K))
              END DO
            ENDDO
          ENDDO

          call open_grads('blendh',JX2,JY,1,1.0,1.0,1.0,1.0)
          open(91,file='blendh',form='unformatted')
          rewind 91
          ALLOCATE(DUMA(JX2,JY))
          DUMA=0.
          call fill_nmm_gridg(blend,JX,JY,DUMA,1)
          write(91) DUMA
          close(91)
          DEALLOCATE(DUMA)
       end if

       DO J=1,JY
       DO I=1,JX
         IF(abs(T3(I,J,10)-T1(I,J,10)).GT.5.)THEN
           PRINT*,'I,J3=',I,J,T3(I,J,10),T1(I,J,10)
           PRINT*,'T21=',T21(I,J,10,1),T21(I,J,10,2),T21(I,J,10,3),T21(I,J,10,4)
           PRINT*,'IIH1(I,J),JJH1(I,J)=',IIH1(I,J),JJH1(I,J)
           GO TO 971
         END IF
       ENDDO
       ENDDO
 971   CONTINUE

!       DO K=1,KZ
!         print*,'z1,z3=',k,Z1(100,200,k),Z3(100,200,k),      &
!                 T1(100,200,k),T3(100,200,k),T2(9,9,k)
!       END DO


! Compute Geopotentital height, INTEGRATE HEIGHT HYDROSTATICLY
            
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

       if(iblend == 1)then
!$omp parallel do &
!$omp& private(i,j,k,RDST,WT2)
          DO J=1,JY
          DO I=1,JX
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
              U3(I,J,K)=U1(I,J,K)+WT2*(U3(I,J,K)-U1(I,J,K))
              V3(I,J,K)=V1(I,J,K)+WT2*(V3(I,J,K)-V1(I,J,K))
            END DO
          ENDDO
          ENDDO

       else
          blend=0.
          call unfill_nmm_gridg(blenda,JX,JY,blend,2)
!$omp parallel do &
!$omp& private(i,j,k)
          DO K=1,KZ
             DO J=1,JY
                DO I=1,JX
                   U3(I,J,K)=U1(I,J,K)+blend(I,J)*(U3(I,J,K)-U1(I,J,K))
                   V3(I,J,K)=V1(I,J,K)+blend(I,J)*(V3(I,J,K)-V1(I,J,K))
                END DO
             END DO
          END DO

          call open_grads('blendv',JX2,JY,1,1.0,1.0,1.0,1.0)

          open(91,file='blendv',form='unformatted')
          rewind 91

          ALLOCATE(DUMA(JX2,JY))
          DUMA=0.
          call fill_nmm_gridg(blend,JX,JY,DUMA,2)
          write(91) DUMA
          close(91)
          DEALLOCATE(DUMA)

       end if

! save 6x data

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

!      WRITE(61)((SLP3(I,J),I=1,JX),J=1,JY,2)
!      DO K=1,KZ+1
!        WRITE(61)((Z3(I,J,K),I=1,JX),J=1,JY,2)
!      END DO
!      DO K=1,KZ+1
!        WRITE(61)((P3(I,J,K),I=1,JX),J=1,JY,2)
!      END DO
!      DO K=1,KZ
!        WRITE(61)((T3(I,J,K),I=1,JX),J=1,JY,2)
!      END DO
!      DO K=1,KZ
!        WRITE(61)((Q3(I,J,K),I=1,JX),J=1,JY,2)
!      END DO
!      DO K=1,KZ
!        WRITE(61)((U3(I,J,K),I=1,JX),J=1,JY,2)
!      END DO
!      DO K=1,KZ
!        WRITE(61)((V3(I,J,K),I=1,JX),J=1,JY,2)
!      END DO
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
