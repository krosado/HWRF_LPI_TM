
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

!       need to change to interpolation for bogus storm

! Variables for 4x working domain

      REAL(4) DLMD3,DPHD3,PT3,PDTOP3              ! use the new inner nest data
      REAL(4) WBD3,SBD3,CLON3,CLAT3

      REAL(4), ALLOCATABLE :: T3(:,:,:),Q3(:,:,:)
      REAL(4), ALLOCATABLE :: U3(:,:,:),V3(:,:,:),PMV3(:,:,:)
      REAL(4), ALLOCATABLE :: Z3(:,:,:),P3(:,:,:),PMID3(:,:,:)
      REAL(4), ALLOCATABLE :: HLON3(:,:),HLAT3(:,:)   ! same as HLON3,HLAT3
      REAL(4), ALLOCATABLE :: PD3(:,:),ETA1(:),ETA2(:),SLP3(:,:)

! Variables from inner nest

      REAL(4) DLMD2,DPHD2,PT2,PDTOP2
      REAL(4) WBD2,SBD2,CLON2,CLAT2

      REAL(4), ALLOCATABLE :: T2(:,:,:),Q2(:,:,:)
      REAL(4), ALLOCATABLE :: U2(:,:,:),V2(:,:,:),PMV2(:,:,:)
      REAL(4), ALLOCATABLE :: Z2(:,:,:),P2(:,:,:),PMID2(:,:,:)
      REAL(4), ALLOCATABLE :: HLON2(:,:),HLAT2(:,:)
      REAL(4), ALLOCATABLE :: VLON2(:,:),VLAT2(:,:)
      REAL(4), ALLOCATABLE :: PD2(:,:),SLP2(:,:) 

      REAL(4), ALLOCATABLE :: WTH(:)
   
      REAL(4), ALLOCATABLE :: T4(:,:,:),Q4(:,:,:)
      REAL(4), ALLOCATABLE :: U4(:,:,:),V4(:,:,:)

      REAL(8) CLON_NEW,CLAT_NEW
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! define DLMD3,DPHD3,PT3,PDTOP3,CLON3,CLAT3 for 4x degree2 domain
!
! READ 4x DATA    ! new data

      IUNIT=20+ITIM

      READ(IUNIT) NX,NY,NZ,I360

      NX1=NX+1
      NY1=NY+1
      NZ1=NZ+1

      ALLOCATE ( T3(NX,NY,NZ),Q3(NX,NY,NZ) )
      ALLOCATE ( U3(NX,NY,NZ),V3(NX,NY,NZ),PMV3(NX,NY,NZ) )
      ALLOCATE ( Z3(NX,NY,NZ1),P3(NX,NY,NZ1),PMID3(NX,NY,NZ) )
      ALLOCATE ( HLON3(NX,NY),HLAT3(NX,NY) )
      ALLOCATE ( PD3(NX,NY),ETA1(NZ1),ETA2(NZ1),SLP3(NX,NY) )

      READ(IUNIT) DLMD3,DPHD3,CLON3,CLAT3
      READ(IUNIT) PT3,PDTOP3
      READ(IUNIT) T3
      READ(IUNIT) Q3
      READ(IUNIT) U3
      READ(IUNIT) V3
      READ(IUNIT) Z3
      READ(IUNIT) HLON3,HLAT3
      READ(IUNIT) P3
      READ(IUNIT) PD3
      READ(IUNIT) ETA1
      READ(IUNIT) ETA2
 
      CLOSE(IUNIT)

      DO K=1,NZ
      DO J=1,NY
      DO I=1,NX
        PMID3(I,J,K)=EXP((ALOG(P3(I,J,K))+ALOG(P3(I,J,K+1)))*0.5)
      ENDDO
      ENDDO
      ENDDO

      DO J=1,NY
      DO I=1,NX
        ZSF1 = 0.5*(Z3(I,J,1)+Z3(I,J,2))
        PSF1 = PMID3(I,J,1)
        TV1 = T3(I,J,1)*(1.+D608*Q3(I,J,1))
        A = (GAMMA * ZSF1) / TV1
        SLP3(I,J) = PSF1*(1+A)**COEF2
      ENDDO
      ENDDO

      WRITE(64)((SLP3(I,J),I=1,NX),J=1,NY,2)
      DO K=1,NZ+1
        WRITE(64)((Z3(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,NZ+1
        WRITE(64)((P3(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,NZ
        WRITE(64)((T3(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,NZ
        WRITE(64)((Q3(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,NZ
        WRITE(64)((U3(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,NZ
        WRITE(64)((V3(I,J,K),I=1,NX),J=1,NY,2)
      END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! READ 2x DATA   ! new data

      IUNIT=30+ITIM

      READ(IUNIT) IX,IY,IZ                ! IZ==NZ

      IX1=IX+1
      IY1=IY+1
      IZ1=IZ+1

      ALLOCATE ( T2(IX,IY,IZ),Q2(IX,IY,IZ) )
      ALLOCATE ( U2(IX,IY,IZ),V2(IX,IY,IZ),PMV2(IX,IY,IZ) )
      ALLOCATE ( Z2(IX,IY,IZ1),P2(IX,IY,IZ1),PMID2(IX,IY,IZ) )
      ALLOCATE ( HLON2(IX,IY),HLAT2(IX,IY) )
      ALLOCATE ( VLON2(IX,IY),VLAT2(IX,IY) )
      ALLOCATE ( PD2(IX,IY), SLP2(IX,IY) )
      
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

      if(I360.eq.360) then
        IF(CLON2.GT.0.)CLON2=CLON2-360.
        DO J=1,KY
        DO I=1,KX
	  IF(HLON2(I,J).GT.0.)HLON2(I,J)=HLON2(I,J)-360.
	  IF(VLON2(I,J).GT.0.)VLON2(I,J)=VLON2(I,J)-360.
        END DO
        END DO
      endif

       print*,'2x CLON2,CLAT2=',CLON2,CLAT2

      FLON=HLON2(1,1)
      FLAT=HLAT2(1,1)

      print*,'FLON,FLAT=',FLON,FLAT

      ERR=1.e20
      DO J=1,NY
      DO I=1,NX
        DIF1=abs(FLON-HLON3(I,J))+abs(FLAT-HLAT3(I,J))
        IF(DIF1.LT.ERR)THEN
          I_ST=I
          J_ST=J
          ERR=DIF1
        END IF
      END DO
      END DO

      print*,'I_ST,J_ST,ERR=',I_ST,J_ST,ERR
      print*,'IX,IY,NZ=',IX,IY,NZ
      print*,'NX,NY,NZ=',NX,NY,NZ

       DO J=1,IY
       DO I=1,IX
         J1=J+J_ST-1
         I1=I+I_ST-1
         DIFFL=abs(HLON2(I,J)-HLON3(I1,J1))+abs(HLAT2(I,J)-HLAT3(I1,J1))
         IF(DIFFL.GT.0.01)then
           PRINT*,'I,J,DIFFL=',I,J,    &
                  abs(HLON2(I,J)-HLON3(I1,J1)),abs(HLAT2(I,J)-HLAT3(I1,J1))
           print*,'need to change the ghost domain size, try NY=NY-6'
           STOP 343
         END IF
       END DO
       END DO
        
! replace SLP 

       DO J=1,IY
       DO I=1,IX
         J1=J+J_ST-1
         I1=I+I_ST-1
	 SLP2(I,J)=SLP3(I1,J1)
       END DO
       END DO

! based on Ts, Zs, SLP1 ==> PS1  ==> P1

       DO J=1,IY
       DO I=1,IX
         ZS2 = Z2(I,J,1)
	 TS2 = T2(I,J,1)+GAMMA*(Z2(I,J,2)-Z2(I,J,1))*0.5
         TVS2 = TS2*(1.+D608*Q2(I,J,1))
         A = (GAMMA * ZS2) / TVS2
         P2(I,J,1) = SLP2(I,J)/(1+A)**COEF2
         PD2(I,J)=P2(I,J,1)-PDTOP3-PT3
       ENDDO
       ENDDO

! PD(I,J)=P1(I,J,1)-PDTOP-PT=PSFC(I,J)-PDTOP-PT
       DO K=1,IZ1
       DO J=1,IY
       DO I=1,IX
         P2(I,J,K)=PT3+PDTOP3*ETA1(K)+PD2(I,J)*ETA2(K)     ! PD(I,J) changed
       ENDDO
       ENDDO
       ENDDO


       DO J=1,IY
       DO I=1,IX
         J1=J+J_ST-1
         I1=I+I_ST-1
         DO L=2,IZ1
           Z2(I,J,L)=Z2(I,J,L-1)+T2(I,J,L-1)*          &
               (Q2(I,J,L-1)*0.608+1.0)*287.04*         &
               (ALOG(P2(I,J,L-1))-ALOG(P2(I,J,L)))/G
         ENDDO
       ENDDO
       END DO

      DO K=1,IZ
      DO J=1,IY
      DO I=1,IX
	PMID2(I,J,K)=EXP((ALOG(P2(I,J,K))+ALOG(P2(I,J,K+1)))*0.5)
      END DO
      END DO
      END DO

       IC1=NX/2
       JC1=NY/2
       DO K=1,NZ
         IF(P3(IC1,JC1,K).LE.10000.)THEN
           KTP1=K
           GO TO 456
         END IF
       END DO
 456   CONTINUE

       ALLOCATE ( WTH(NZ) )

       WTH=0.
       DO K=1,KTP1-1
         WTH(K)=1.0
       END DO
       WTH(KTP1)=0.5

      ALLOCATE ( T4(IX,IY,IZ),Q4(IX,IY,IZ) )
      ALLOCATE ( U4(IX,IY,IZ),V4(IX,IY,IZ) )

      T4=T2
      Q4=Q2
      U4=U2
      V4=V2


      DO J=1,IY
      DO I=1,IX
	J1=J+J_ST-1
	I1=I+I_ST-1
	DO K=1,IZ
	  IF(PMID2(I,J,K).GT.PMID3(I1,J1,1))THEN
	    DZ1=T3(I1,J1,1)/GAMMA*(1.-(PMID2(I,J,K)/PMID3(I1,J1,1))**COEF3)
	    T4(I,J,K)=T3(I1,J1,1)-GAMMA*DZ1
	    Q4(I,J,K)=Q3(I1,J1,1)
          ELSE IF(PMID2(I,J,K).LE.PMID3(I1,J1,NZ))THEN
	    DZ1=T3(I1,J1,NZ)/GAMMA*(1.-(PMID2(I,J,K)/PMID3(I1,J1,NZ))**COEF3)
	    T4(I,J,K)=T3(I1,J1,NZ)-GAMMA*DZ1
	    Q4(I,J,K)=Q3(I1,J1,NZ)
	  ELSE
	    DO K1=2,NZ
              PDIF1=ALOG(PMID2(I,J,K))-ALOG(PMID3(I1,J1,K1))
              IF(PDIF1.GE.0.)THEN
                FACT1=PDIF1/(ALOG(PMID3(I1,J1,K1-1))-ALOG(PMID3(I1,J1,K1)))
	        T4(I,J,K)=T3(I1,J1,K1)*(1.-FACT1)+T3(I1,J1,K1-1)*FACT1
	        Q4(I,J,K)=Q3(I1,J1,K1)*(1.-FACT1)+Q3(I1,J1,K1-1)*FACT1
                GO TO 53
	      END IF
	    END DO
	  END IF
 53       CONTINUE
        END DO
      END DO
      END DO

       PMV3=PMID3

! interpolate vertically to P level in new coordinate  (V Points)
      DO J=2,NY-1
        IF(MOD(J,2).NE.0.)THEN
          DO I=2,NX-1
          DO K=1,NZ
            PMV3(I,J,K)=0.25*(PMID3(I,J,K)+PMID3(I+1,J,K)+            &
                              PMID3(I,J-1,K)+PMID3(I,J+1,K))
          END DO
          END DO
        ELSE
          DO I=2,NX-1
          DO K=1,NZ
            PMV3(I,J,K)=0.25*(PMID3(I-1,J,K)+PMID3(I,J,K)+            &
                              PMID3(I,J-1,K)+PMID3(I,J+1,K))
          END DO
          END DO
        END IF
      END DO

       PMV2=PMID2

! interpolate vertically to P level in new coordinate  (V Points)
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

      DO J=1,IY
      DO I=1,IX
	J1=J+J_ST-1
	I1=I+I_ST-1
	DO K=1,IZ
	  IF(PMV2(I,J,K).GT.PMV3(I1,J1,1))THEN
	    U4(I,J,K)=U3(I1,J1,1)
	    V4(I,J,K)=V3(I1,J1,1)
          ELSE IF(PMV2(I,J,K).LE.PMV3(I1,J1,NZ))THEN
	    U4(I,J,K)=U3(I1,J1,NZ)
	    V4(I,J,K)=V3(I1,J1,NZ)
	  ELSE
	    DO K1=2,NZ
              PDIF1=ALOG(PMV2(I,J,K))-ALOG(PMV3(I1,J1,K1))
              IF(PDIF1.GE.0.)THEN
                FACT1=PDIF1/(ALOG(PMV3(I1,J1,K1-1))-ALOG(PMV3(I1,J1,K1)))
	        U4(I,J,K)=U3(I1,J1,K1)*(1.-FACT1)+U3(I1,J1,K1-1)*FACT1
	        V4(I,J,K)=V3(I1,J1,K1)*(1.-FACT1)+V3(I1,J1,K1-1)*FACT1
                GO TO 65
	      END IF
	    END DO
	  END IF
 65       CONTINUE
        END DO
      END DO
      END DO

! replace all the data for the 2x domain



       DO J=1,IY
       DO I=1,IX
         DO K=1,IZ
           T2(I,J,K)=T4(I,J,K)*WTH(K)+T2(I,J,K)*(1.-WTH(K))
           Q2(I,J,K)=Q4(I,J,K)*WTH(K)+Q2(I,J,K)*(1.-WTH(K))
           U2(I,J,K)=U4(I,J,K)
           V2(I,J,K)=V4(I,J,K)
         END DO
       END DO
       END DO

! based on Ts, Zs, SLP1 ==> PS1  ==> P1

       DO J=1,IY
       DO I=1,IX
         ZS2 = Z2(I,J,1)
	 TS2 = T2(I,J,1)+GAMMA*(Z2(I,J,2)-Z2(I,J,1))*0.5
         TVS2 = TS2*(1.+D608*Q2(I,J,1))
         A = (GAMMA * ZS2) / TVS2
         P2(I,J,1) = SLP2(I,J)/(1+A)**COEF2
         PD2(I,J)=P2(I,J,1)-PDTOP3-PT3
       ENDDO
       ENDDO

! PD(I,J)=P1(I,J,1)-PDTOP-PT=PSFC(I,J)-PDTOP-PT
       DO K=1,IZ1
       DO J=1,IY
       DO I=1,IX
         P2(I,J,K)=PT3+PDTOP3*ETA1(K)+PD2(I,J)*ETA2(K)     ! PD(I,J) changed
       ENDDO
       ENDDO
       ENDDO


       DO J=1,IY
       DO I=1,IX
         J1=J+J_ST-1
         I1=I+I_ST-1
         DO L=2,IZ1
           Z2(I,J,L)=Z2(I,J,L-1)+T2(I,J,L-1)*          &
               (Q2(I,J,L-1)*0.608+1.0)*287.04*         &
               (ALOG(P2(I,J,L-1))-ALOG(P2(I,J,L)))/G
         ENDDO
       ENDDO
       END DO


      print*,'wrte test'
      write(*,58)Z2(41,3,1),T2(41,2,1),Q2(41,4,1)
      write(*,58)P2(41,3,1),P2(42,3,1),P2(41,2,1),P2(41,4,1)
 58   format(10F10.2)


! save 2x data

      IUNIT=50+ITIM

      WRITE(IUNIT) IX,IY,NZ,I360
      WRITE(IUNIT) DLMD3,DPHD3,CLON3,CLAT3
      WRITE(IUNIT) PT3,PDTOP3
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

      CLOSE(IUNIT)

! test
      
!      DO K=1,3
!      DO J=1,IY
!      DO I=1,IX
!	P2(I,J,K)=PT+PDTOP*ETA1(K)+PD2(I,J)*ETA2(K)
!      ENDDO
!      ENDDO
!      ENDDO

!       DO J=1,IY
!       DO I=1,IX
!         ZSF1 = 0.5*(Z2(I,J,1)+Z2(I,J,2))
!         PSF1 = EXP((ALOG(P2(I,J,1))+ALOG(P2(I,J,2)))*0.5)
!         TV1 = T2(I,J,1)*(1.+D608*Q2(I,J,1))
!         A = (GAMMA * ZSF1) / TV1
!         SLP1(I,J) = PSF1*(1+A)**COEF2
!      ENDDO
!      ENDDO

      
!       press1=1.E20
!       DO J=1,IY
!       DO I=1,IX
!       if(press1.gt.SLP1(I,J))press1=SLP1(I,J)
!       END DO
!       END DO
                                                                                                           
!       print*,'surface pressure=',press1


!      CLON_NEW=-75.8
!      CLAT_NEW=23.3
                                                                                          
!      CALL FIND_NEWCT1(IX,IY,U2(1,1,10),V2(1,1,10),HLON2,HLAT2,CLON_NEW,CLAT_NEW)
                                                                                                       
                                                                                                       
!      print*,'storm center 4 =',CLON_NEW,CLAT_NEW
                                                                                                       
!      CLON_NEW=-75.8
!      CLAT_NEW=23.3
          
          
!      CALL FIND_NEWCT1(NX,NY,U3(1,1,10),V3(1,1,10),HLON3,HLAT3,CLON_NEW,CLAT_NEW)
          
          
!      print*,'storm center 41 =',CLON_NEW,CLAT_NEW

       END

      SUBROUTINE FIND_NEWCT1(IX,JX,UD,VD,GLON2,GLAT2,    &
                             CLON_NEW1,CLAT_NEW1)
                                                                                                       
                                                                                                       
!      PARAMETER (IR=100,IT=24,IX=254,JX=254)
      PARAMETER (IR=30,IT=24)
      PARAMETER (ID=61,JD=61,DTX=0.05,DTY=0.05)    ! Search x-Domain (ID-1)*DTX
      REAL (4) UD(IX,JX),VD(IX,JX),GLON2(IX,JX),GLAT2(IX,JX)
!      DIMENSION RWM(IR+1),TWM(IR+1)
      DIMENSION TNMX(ID,JD),RX(ID,JD),WTM(IR)
      REAL (8) CLON_NEW1,CLAT_NEW1
                                                                                                       
      PI=ASIN(1.)*2.
      RAD=PI/180.
                                                                                                       
      ddr=0.05
                                                                                                       
      pi180=RAD
      cost=cos(clat_new*pi180)
                                                                                                       
      ix2=ix/2
      jx2=jx/2
      DDS=(((GLON2(ix2+1,jx2)-GLON2(ix2,jx2))*cost)**2+     &
          (GLAT2(ix2,jx2+1)-GLAT2(ix2,jx2))**2)*1.5
                                                                                                       
                                                                                                       
       print*,'ix,jx,ix2,jx2=',ix,jx,ix2,jx2
       print*,'CLON_NEW,CLAT_NEW=',CLON_NEW1,CLAT_NEW1
       print*,'GLON2,GLAT2=',GLON2(1,1),GLAT2(1,1)
                                                                                                       
                                                                                                       
      XLAT = CLAT_NEW1-(JD-1)*DTY/2.
      XLON = CLON_NEW1-(ID-1)*DTX/2.
                  
!c      print *,'STARTING LAT, LON AT FIND NEW CENTER ',XLAT,XLON
                  
      DO J=1,JD
      DO I=1,ID
      TNMX(I,J) = 0.
      RX(i,j)=0.
      BLON = XLON + (I-1)*DTX
      BLAT = XLAT + (J-1)*DTY
                  
!.. CALCULATE TANGENTIAL WIND EVERY 0.2 deg INTERVAL
!..  10*10 deg AROUND 1ST GUESS VORTEX CENTER
                  
      DO 10 JL=1,IR
      WTS= 0.
      DO 20 IL=1,IT
      DR = JL*ddr
!      DR = JL
      DD = (IL-1)*15*RAD
      DLON = DR*COS(DD)
      DLAT = DR*SIN(DD)
      TLON = BLON + DLON
      TLAT = BLAT + DLAT
                  
!C.. INTERPOLATION U, V AT TLON,TLAT AND CLACULATE TANGENTIAL WIND
                  
      u1=0.
      v1=0.
      sum1=0.
      DO j1=jx2-40,jx2+40
      DO i1=ix2-40,ix2+40
        dist=(((GLON2(i1,j1)-TLON)*cost)**2+(GLAT2(i1,j1)-TLAT)**2)
        if(dist.lt.DDS)THEN
          dist1=1./dist
          sum1=sum1+dist1
          u1=u1+UD(i1,j1)*dist1
          v1=v1+VD(i1,j1)*dist1
        end if
      end do
      end do
            
      UT=u1/sum1
      VT=v1/sum1
                
!C.. TANGENTIAL WIND
      WT = -SIN(DD)*UT + COS(DD)*VT
      WTS = WTS+WT
20    CONTINUE
      WTM(JL) = WTS/24.
10    CONTINUE
                  
!C Southern Hemisphere
      IF(CLAT_NEW.LT.0)THEN
        DO JL=1,IR
          WTM(JL)=-WTM(JL)
        END DO
      END IF
!C EnD SH
            
!      print*,'test1'
                  
      TX = -10000000.
      DO KL = 1,IR
      IF(WTM(KL).GE.TX) THEN
      TX = WTM(KL)
      RRX = KL*ddr
      ENDIF
      ENDDO
!        DO KL=1,IR
!          TWM(KL)=WTM(KL)
!          RWM(KL)=KL*ddr
!        END DO
!        TWM(IR+1)=TX
!        RWM(IR+1)=RRX
                  
      TNMX(I,J) = TX
      RX(I,J)=RRX
      ENDDO
      ENDDO
!C.. FIND NEW CENTER
      TTX = -1000000.
      DO I=1,ID
      DO J=1,JD
      IF(TNMX(I,J).GE.TTX) THEN
      TTX = TNMX(I,J)
      NIC = I
      NJC = J
      ENDIF
      ENDDO
      ENDDO
           
! QLIU test
!      print*,XLAT+30*DTY,XLON+30*DTX,TNMX(30,30)
      print*,'max WTM=',TTX
                  
      CLAT_NEW1 = XLAT + (NJC-1)*DTY
      CLON_NEW1 = XLON + (NIC-1)*DTX
                  
!      print *,'NEW CENTER,  I, J IS   ',NIC,NJC
      print *,'NEW CENTER, LAT,LON IS ',CLAT_NEW1,CLON_NEW1
!      print *,'MAX TAN. WIND AT NEW CENTER IS ',TTX
                  
      RETURN
      END

