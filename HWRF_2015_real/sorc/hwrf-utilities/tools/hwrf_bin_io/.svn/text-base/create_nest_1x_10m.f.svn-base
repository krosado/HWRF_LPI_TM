
      PROGRAM HWRF_MERGE_NEST

!??????????????????????????????????????????????????????????
!     
!     DECLARE VARIABLES
!
!      IMPLICIT NONE

      INTEGER I,J,K,NX,NY,NZ,NST,IFLAG,JX,JY,KZ
!
      PARAMETER (NST=5)
!      PARAMETER (IX=117,IY=225)
!      PARAMETER (NX=215,NY=431,NZ=42,NST=5)
!      PARAMETER (JX=393,JY=735)       ! fixed for 9 km resolution
      PARAMETER (GAMMA=6.5E-3,G=9.8,Rd=287.05,D608=0.608)
      PARAMETER (Cp=1004.)

! Variables on new inner nest hybrid coordinate

      REAL(4), ALLOCATABLE :: T4(:,:,:),Q4(:,:,:)
      REAL(4), ALLOCATABLE :: U4(:,:,:),V4(:,:,:)
      REAL(4), ALLOCATABLE :: Z4(:,:,:),P4(:,:,:)
      REAL(4), ALLOCATABLE :: ETA1(:),ETA2(:)

      REAL(4), ALLOCATABLE :: A101(:,:),B101(:,:)

! Variables for 1x working domain

      REAL(4) DLMD3,DPHD3,PT3,PDTOP3              ! use the new inner nest data
      REAL(4) WBD3,SBD3,CLON3,CLAT3

      REAL(4), ALLOCATABLE :: HLON3(:,:),HLAT3(:,:)
      REAL(4), ALLOCATABLE :: VLON3(:,:),VLAT3(:,:)

      REAL(4), ALLOCATABLE :: T3(:,:,:),Q3(:,:,:)
      REAL(4), ALLOCATABLE :: U3(:,:,:),V3(:,:,:)
      REAL(4), ALLOCATABLE :: Z3(:,:,:),P3(:,:,:)
      REAL(4), ALLOCATABLE :: PD3(:,:),PMID3(:,:,:)

      REAL(4), ALLOCATABLE :: SLP3(:,:),SLPV3(:,:)
      REAL(4), ALLOCATABLE :: ZS3(:,:),TS3(:,:),QS3(:,:)

      REAL(4), ALLOCATABLE :: A102(:,:),B102(:,:)

! working arrays used for outer nest interpolation

      integer(4), ALLOCATABLE :: IIH(:,:),JJH(:,:)
      integer(4), ALLOCATABLE :: IIV(:,:),JJV(:,:)
      REAL(4), ALLOCATABLE :: HBWGT(:,:,:),VBWGT(:,:,:)

! working arrays used for inner nest interpolation

      integer(4), ALLOCATABLE :: IIH1(:,:),JJH1(:,:)
      integer(4), ALLOCATABLE :: IIV1(:,:),JJV1(:,:)
      REAL(4), ALLOCATABLE :: HBWGT1(:,:,:),VBWGT1(:,:,:)

! working array

      REAL(4), ALLOCATABLE :: T21(:,:,:,:),Q21(:,:,:,:)
      REAL(4), ALLOCATABLE :: U21(:,:,:,:),V21(:,:,:,:)
      REAL(4), ALLOCATABLE :: SLP21(:,:,:)
      REAL(4), ALLOCATABLE :: PMV1(:,:,:),PMV3(:,:,:)

      integer(4) IH1(4),JH1(4),IV1(4),JV1(4)

! Variables from outer nest

      REAL(4) DLMD1,DPHD1,PT1,PDTOP1
      REAL(4) WBD1,SBD1,CLON1,CLAT1

      REAL(4), ALLOCATABLE :: T1(:,:,:),Q1(:,:,:)
      REAL(4), ALLOCATABLE :: U1(:,:,:),V1(:,:,:) 
      REAL(4), ALLOCATABLE :: Z1(:,:,:),P1(:,:,:)
      REAL(4), ALLOCATABLE :: GLON1(:,:),GLAT1(:,:)
      REAL(4), ALLOCATABLE :: HLON1(:,:),HLAT1(:,:)
      REAL(4), ALLOCATABLE :: VLON1(:,:),VLAT1(:,:)
      REAL(4), ALLOCATABLE :: PD1(:,:)

      REAL(4), ALLOCATABLE :: SLP1(:,:)
      REAL(4), ALLOCATABLE :: PMID1(:,:,:),ZMID1(:,:,:)
    
! Variables from inner nest

      REAL(4) DLMD2,DPHD2,PT2,PDTOP2
      REAL(4) WBD2,SBD2,CLON2,CLAT2

!!!!!!!!!!!!!!!!11


      COEF1=Rd/Cp
      COEF3=Rd*GAMMA/G
      COEF2=1./COEF3

      GRD=G/Rd

      pi=4.*atan(1.)
      pi_deg=180./pi
      pi180=1./pi_deg

      DIST1=6.371E3*pi180

      READ(5,*)ITIM

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! define DLMD3,DPHD3,PT3,PDTOP3,CLON3,CLAT3 for 4x degree2 domain
!
! READ INNER NEST DATA    ! new data

      IUNIT=40+ITIM
                                                                                                                                                                                         
      READ(IUNIT)JX,JY,KZ

      print*,'JX,JY,KZ (46) =',JX,JY,KZ                                                                                                                                                                                         
      JX1=JX+1
      JY1=JY+1
      KZ1=KZ+1

      ALLOCATE ( T4(JX,JY,KZ),Q4(JX,JY,KZ) )
      ALLOCATE ( U4(JX,JY,KZ),V4(JX,JY,KZ) )
      ALLOCATE ( Z4(JX,JY,KZ1),P4(JX,JY,KZ1) )
      ALLOCATE ( ETA1(KZ1),ETA2(KZ1) )

      ALLOCATE ( HLON3(JX,JY),HLAT3(JX,JY) )
      ALLOCATE ( VLON3(JX,JY),VLAT3(JX,JY) )

      ALLOCATE ( A101(JX,JY),B101(JX,JY) )

      READ(IUNIT) DLMD3,DPHD3,CLON3,CLAT3
      PRINT *,'READ CLON '
      READ(IUNIT) PT3,PDTOP3
      PRINT *,'READ PT3 '
      READ(IUNIT) T4
      PRINT *,'READ T4 '
      READ(IUNIT) Q4
      PRINT *,'READ Q4 '
      READ(IUNIT) U4
      PRINT *,'READ U4 '
      READ(IUNIT) V4
      PRINT *,'READ V4 '
      READ(IUNIT) Z4
      PRINT *,'READ Z4 '
      READ(IUNIT) HLON3,HLAT3,VLON3,VLAT3
      PRINT *,'READ HLON '
      READ(IUNIT) P4
      PRINT *,'READ P4 '
      READ(IUNIT) 
      PRINT *,'READ NULL '
      READ(IUNIT) ETA1
      PRINT *,'READ ETA1 '
      READ(IUNIT) ETA2
      PRINT *,'READ ETA2 '
                                         
      READ(IUNIT) A101
      PRINT *,'READ A101 '
      READ(IUNIT) B101
      PRINT *,'READ B101 '

      CLOSE(IUNIT)


! create inner high resolution domain


      PRINT*,'READ in CLON3,CLAT3=',CLON3,CLAT3

      write(*,*)'DLMD3,DPHD3,PT3,PDTOP3=',DLMD3,DPHD3,PT3,PDTOP3

      ALLOCATE ( T3(JX,JY,KZ),Q3(JX,JY,KZ) )
      ALLOCATE ( U3(JX,JY,KZ),V3(JX,JY,KZ) )
      ALLOCATE ( Z3(JX,JY,KZ1),P3(JX,JY,KZ1) )

      T3=T4
      Q3=Q4
      U3=U4
      V3=V4
      Z3=Z4
      P3=P4  


      ALLOCATE ( PD3(JX,JY), PMID3(JX,JY,KZ) )

      ALLOCATE ( SLP3(JX,JY),SLPV3(JX,JY) )
      ALLOCATE ( ZS3(JX,JY),TS3(JX,JY),QS3(JX,JY) )

      ALLOCATE ( IIH(JX,JY),JJH(JX,JY) )
      ALLOCATE ( IIV(JX,JY),JJV(JX,JY) )
      ALLOCATE ( HBWGT(JX,JY,4),VBWGT(JX,JY,4) )

      ALLOCATE ( IIH1(JX,JY),JJH1(JX,JY) )
      ALLOCATE ( IIV1(JX,JY),JJV1(JX,JY) )
      ALLOCATE ( HBWGT1(JX,JY,4),VBWGT1(JX,JY,4) )

! working array

      ALLOCATE ( T21(JX,JY,KZ,4),Q21(JX,JY,KZ,4) )
      ALLOCATE ( U21(JX,JY,KZ,4),V21(JX,JY,KZ,4) )
      ALLOCATE ( SLP21(JX,JY,4) )
      ALLOCATE ( PMV3(JX,JY,KZ) )

! LON & LAT at T,U,V

       print*,'HLAT3,HLON3,VLAT3,VLON3=',                  &
               HLAT3(1,1),HLON3(1,1),VLAT3(1,1),VLON3(1,1)

! find the grid index at the lower left corner

      FLON=HLON3(1,1)
      FLAT=HLAT3(1,1)

      print*,'FLON,FLAT=',FLON,FLAT


! READ OUTER NEST DATA    ! 6 hour forecast data

      IUNIT=20+ITIM

      READ(IUNIT) NX,NY,NZ

      print*,'NX,NY,NZ=',NX,NY,NZ

      NX1=NX+1
      NY1=NY+1
      NZ1=NZ+1

      ALLOCATE ( T1(NX,NY,NZ),Q1(NX,NY,NZ) )
      ALLOCATE ( U1(NX,NY,NZ),V1(NX,NY,NZ) )
      ALLOCATE ( Z1(NX,NY,NZ1),P1(NX,NY,NZ1) )
      ALLOCATE ( GLON1(NX,NY),GLAT1(NX,NY) )
      ALLOCATE ( HLON1(NX,NY),HLAT1(NX,NY) )
      ALLOCATE ( VLON1(NX,NY),VLAT1(NX,NY) )
      ALLOCATE ( PD1(NX,NY),A102(NX,NY),B102(NX,NY) )

      READ(IUNIT) DLMD1,DPHD1,CLON1,CLAT1
      READ(IUNIT) PT1,PDTOP1
      READ(IUNIT) T1
      READ(IUNIT) Q1
      READ(IUNIT) U1
      READ(IUNIT) V1
      READ(IUNIT) Z1
      READ(IUNIT) GLON1,GLAT1
      READ(IUNIT) P1
      READ(IUNIT) PD1
      READ(IUNIT) ETA1
      READ(IUNIT) ETA2

      READ(IUNIT) A102
      READ(IUNIT) B102
 
      CLOSE(IUNIT)

      print*,'GLON1(1,1),GLAT1(1,1)=',GLON1(1,1),GLAT1(1,1)
      print*,'GLON1(NX,NY),GLAT1(NX,NY)=',GLON1(NX,NY),GLAT1(NX,NY)

     
      WBD1=-(NX-1)*DLMD1                  ! PARENT wbd
      SBD1=-((NY-1)/2)*DPHD1              ! PARENT SBD
                                                                                                         
       CALL EARTH_LATLON ( HLAT1,HLON1,VLAT1,VLON1,        &  !Earth lat,lon at H and V points
                           DLMD1,DPHD1,WBD1,SBD1,          &  !input res,west & south boundaries,
                           CLAT1,CLON1,                    &  ! central lat,lon, all in degrees
                           1,NX1,1,NY1,1,1,                &
                           1,NX ,1,NY ,1,1,                &
                           1,NX ,1,NY ,1,1         )
                                                                                                         
      print*,'HLON1(1,1),HLAT1(1,1)=',HLON1(1,1),HLAT1(1,1)
      print*,'HLON1(NX,NY),HLAT1(NX,NY)=',HLON1(NX,NY),HLAT1(NX,NY)
                                                                                                         

! save data
       
       
      IUNIT=51+ITIM
       
       
      WRITE(IUNIT) NX,NY,NZ
      WRITE(IUNIT) DLMD1,DPHD1,CLON1,CLAT1
      WRITE(IUNIT) PT1,PDTOP1
      WRITE(IUNIT) T1
      WRITE(IUNIT) Q1
      WRITE(IUNIT) U1
      WRITE(IUNIT) V1
      WRITE(IUNIT) Z1
      WRITE(IUNIT) HLON1,HLAT1,VLON1,VLAT1
      WRITE(IUNIT) P1
      WRITE(IUNIT) PD1
      WRITE(IUNIT) ETA1
      WRITE(IUNIT) ETA2

      WRITE(IUNIT) A102
      WRITE(IUNIT) B102
       
      CLOSE(IUNIT)


      DO J=1,NY
      DO I=1,NX
        DIF1=abs(GLON1(I,J)-HLON1(I,J))+abs(GLAT1(I,J)-HLAT1(I,J))
        IF(DIF1.GT.0.01)THEN
          print*,'I,J,DIFFT=',I,J,DIF1
          STOP
        END IF
      END DO
      END DO


      ERR=1.e20
      DO J=1,NY
      DO I=1,NX
        DIF1=abs(FLON-HLON1(I,J))+abs(FLAT-HLAT1(I,J))
        IF(DIF1.LT.ERR)THEN
          I_ST=I
          J_ST=J
          ERR=DIF1
        END IF
      END DO
      END DO
                                                                                                                                    
      PRINT*,'I_ST,J_ST,ERR=',I_ST,J_ST,ERR


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


!  parameters for the outer nest

!      WBD1=-(NX-1)*DLMD1
!      SBD1=-((NY-1)/2)*DPHD1

      print*,'CLON1,CLAT1=',CLON1,CLAT1
      print*,'WBD1,SBD1,NX,NY=',WBD1,SBD1,NX,NY

       CALL G2T2H( IIH,JJH,                       & ! output grid index and weights
                   HBWGT(1,1,1),HBWGT(1,1,2),     &
                   HBWGT(1,1,3),HBWGT(1,1,4),     &
                   HLAT3,HLON3,                   & ! target (nest) input lat lon in degrees
                   DLMD1,DPHD1,WBD1,SBD1,         & ! parent res, western and south boundaries
                   CLAT1,CLON1,                   & ! parent central lat,lon, all in degrees
                   NX,NY,                     & ! parent imax and jmax
                   1,JX1,1,JY1,1,1,               & ! target (nest) grid dimensions
                   1,JX ,1,JY ,1,1,               &
                   1,JX ,1,JY ,1,1         )
                                                                                                                                               
       CALL G2T2V( IIV,JJV,                       & ! output grid index and weights
                   VBWGT(1,1,1),VBWGT(1,1,2),     &
                   VBWGT(1,1,3),VBWGT(1,1,4),     &
                   VLAT3,VLON3,                   & ! target (nest) input lat lon in degrees
                   DLMD1,DPHD1,WBD1,SBD1,         & ! parent res, western and south boundaries
                   CLAT1,CLON1,                   & ! parent central lat,lon, all in degrees
                   NX,NY,                     & ! parent imax and jmax
                   1,JX1,1,JY1,1,1,               & ! target (nest) grid dimensions
                   1,JX ,1,JY ,1,1,               &
                   1,JX ,1,JY ,1,1         )

              
       print*,'IIH(1,1),JJH(1,1)=',IIH(1,1),JJH(1,1)
       print*,'IIH(JX,JY),JJH(JX,JY)=',IIH(JX,JY),JJH(JX,JY)
       print*,'IIV(1,1),JJV(1,1)=',IIV(1,1),JJV(1,1)
       print*,'IIV(JX,JY),JJV(JX,JY)=',IIV(JX,JY),JJV(JX,JY)


! replace the topgraphy data for the inner nest

       I_ST=1
       J_ST=1

       DO J=1,JY
       DO I=1,JX
         J1=J+J_ST-1
         I1=I+I_ST-1
         ZS3(I1,J1)=Z4(I,J,1)
       END DO
       END DO

!!!!!!!!!!!!!

       Z3=0.
       DO J=1,JY
       DO I=1,JX
         Z3(I,J,1)=ZS3(I,J)
       END DO
       END DO

! compute interpolation for outer  nest

       NX_1=NX-2
       NY_1=NY-2              

! First, compute variables at surface level

       DO J=1,JY
       DO I=1,JX
         IF(IIH(I,J).LT.2.or.IIH(I,J).GT.NX_1)GO TO 40
         IF(JJH(I,J).LE.2.or.JJH(I,J).GE.NY_1)GO TO 40
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
         DO N1=1,4
           SLP21(I,J,N1)=SLP1(IH1(N1),JH1(N1))
         END DO
         K=1             ! surface
         DO N1=1,4
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
                 GO TO 38 
               END IF
             END DO
           END IF
 38        CONTINUE
         END DO
 40      CONTINUE
       ENDDO
       ENDDO

       DO J=1,JY
       DO I=1,JX
         IF(IIH(I,J).LT.2.or.IIH(I,J).GT.NX_1)GO TO 44
         IF(JJH(I,J).LE.2.or.JJH(I,J).GE.NY_1)GO TO 44
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
 44      CONTINUE
       ENDDO
       ENDDO

      WRITE(64)((SLP3(I,J),I=1,JX),J=1,JY,2)
      DO K=1,KZ+1
        WRITE(64)((Z4(I,J,K),I=1,JX),J=1,JY,2)
      END DO
      DO K=1,KZ+1
        WRITE(64)((P4(I,J,K),I=1,JX),J=1,JY,2)
      END DO
      DO K=1,KZ
        WRITE(64)((T4(I,J,K),I=1,JX),J=1,JY,2)
      END DO
      DO K=1,KZ
        WRITE(64)((Q4(I,J,K),I=1,JX),J=1,JY,2)
      END DO
      DO K=1,KZ
        WRITE(64)((U4(I,J,K),I=1,JX),J=1,JY,2)
      END DO
      DO K=1,KZ
        WRITE(64)((V4(I,J,K),I=1,JX),J=1,JY,2)
      END DO

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


      print*,'NX,NY,NZ=',NX,NY,NZ

      WRITE(63)((SLP1(I,J),I=1,NX),J=1,NY,2)
      DO K=1,NZ+1
        WRITE(63)((Z1(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,NZ+1
        WRITE(63)((P1(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,NZ
        WRITE(63)((T1(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,NZ
        WRITE(63)((Q1(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,NZ
        WRITE(63)((U1(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,NZ
        WRITE(63)((V1(I,J,K),I=1,NX),J=1,NY,2)
      END DO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Construct 3D pressure grid

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

       print*,'P3_1,PMID3_1=',P3(9,9,1),PMID3(9,9,1)
       print*,'P3_KZ,PMID3_KZ=',P3(9,9,KZ),PMID3(9,9,KZ)


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
 50        CONTINUE
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

! Compute Geopotentital height, INTEGRATE HEIGHT HYDROSTATICLY
            
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

       print*,'P3_1,PMID3_1=',P3(9,9,1),PMID3(9,9,1)
       print*,'P3_KZ,PMID3_KZ=',P3(9,9,KZ),PMID3(9,9,KZ)

!!!      GO TO 991
     
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

       print*,'P3_1,PMID3_1=',P3(9,9,1),PMID3(9,9,1)
       print*,'P3_KZ,PMID3_KZ=',P3(9,9,KZ),PMID3(9,9,KZ)

       print*,'4p=',PMID3(9,9,1),PMID3(10,9,1),PMID3(9,8,1),PMID3(9,10,1)

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

       print*,'PMV3_1,PMV1_1=',PMV3(9,9,1),PMV1(9,9,1)
       print*,'PMV3_NZ,PMV1_NZ=',PMV3(9,9,NZ),PMV1(9,9,NZ)

       DO J=1,JY
       DO I=1,JX
         IF(IIV(I,J).LT.2.or.IIV(I,J).GT.NX_1)GO TO 80
         IF(JJV(I,J).LE.2.or.JJV(I,J).GE.NY_1)GO TO 80
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
           DO N1=1,4
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
                   GO TO 60
                 END IF
               END DO
             END IF
 60          CONTINUE
           END DO
         END DO
 80      CONTINUE
       END DO
       END DO

!23456789012345678901234567890123456789012345678901234567890123456789012

       DO J=1,JY
       DO I=1,JX
         IF(IIV(I,J).LT.2.or.IIV(I,J).GT.NX_1)GO TO 110
         IF(JJV(I,J).LE.2.or.JJV(I,J).GE.NY_1)GO TO 110
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
 110     CONTINUE
       ENDDO
       ENDDO

 991   CONTINUE


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

      WRITE(IUNIT) A101
      WRITE(IUNIT) B101

        print*,'JX,JY,KZ=',JX,JY,KZ,T3(9,9,42)
        write(*,*)'inside merge K,T1,Q1,U1,V1,Z1,P1='
      do k=1,kz
        write(*,32)K,T3(9,9,K),            &
          Q3(9,9,K),U3(9,9,K),V3(9,9,K),Z3(9,9,K),P3(9,9,K)
      end do
 32   format(I3,6F13.2)

      CLOSE(IUNIT)

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

