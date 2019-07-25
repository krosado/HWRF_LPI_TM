
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
      REAL(4), ALLOCATABLE :: PD3(:,:),SLP3(:,:)
      REAL(4), ALLOCATABLE :: ETA1(:),ETA2(:)

! Variables from inner nest

      REAL(4) DLMD2,DPHD2,PT2,PDTOP2
      REAL(4) WBD2,SBD2,CLON2,CLAT2

      REAL(4), ALLOCATABLE :: T2(:,:,:),Q2(:,:,:)
      REAL(4), ALLOCATABLE :: U2(:,:,:),V2(:,:,:),PMV2(:,:,:)
      REAL(4), ALLOCATABLE :: Z2(:,:,:),P2(:,:,:),PMID2(:,:,:)
      REAL(4), ALLOCATABLE :: HLON2(:,:),HLAT2(:,:),VLON2(:,:),VLAT2(:,:)
      REAL(4), ALLOCATABLE :: PD2(:,:),SLP2(:,:)
   
      integer, external :: omp_get_max_threads
      CHARACTER*2 :: basin !zhang
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

      READ(5,*)ITIM,basin   !zhang: added basin domain shift option

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! define DLMD3,DPHD3,PT3,PDTOP3,CLON3,CLAT3 for 4x degree2 domain
!
! READ 2x DATA    ! new data after analysis

      IUNIT=20+ITIM

      READ(IUNIT) NX,NY,NZ

      NX1=NX+1
      NY1=NY+1
      NZ1=NZ+1

      ALLOCATE ( T3(NX,NY,NZ),Q3(NX,NY,NZ) )
      ALLOCATE ( U3(NX,NY,NZ),V3(NX,NY,NZ),PMV3(NX,NY,NZ) )
      ALLOCATE ( Z3(NX,NY,NZ1),P3(NX,NY,NZ1),PMID3(NX,NY,NZ) )
      ALLOCATE ( HLON3(NX,NY),HLAT3(NX,NY) )
      ALLOCATE ( PD3(NX,NY),SLP3(NX,NY) )
      ALLOCATE ( ETA1(NZ1),ETA2(NZ1) )

      READ(IUNIT) DLMD3,DPHD3
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

      DO K=1,NZ1
      DO J=1,NY
      DO I=1,NX
	P3(I,J,K)=PT3+PDTOP3*ETA1(K)+PD3(I,J)*ETA2(K)     ! PD(I,J) changed
      ENDDO
      ENDDO
      ENDDO

      DO K=1,NZ
      DO J=1,NY
      DO I=1,NX
	PMID3(I,J,K)=EXP((ALOG(P3(I,J,K))+ALOG(P3(I,J,K+1)))*0.5)
      END DO
      END DO
      END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 1x data

      IUNIT=30+ITIM

      READ(IUNIT) IX,IY,IZ                ! IZ==NZ

      IX1=IX+1
      IY1=IY+1
      IZ1=IZ+1

      ALLOCATE ( T2(IX,IY,IZ),Q2(IX,IY,IZ) )
      ALLOCATE ( U2(IX,IY,IZ),V2(IX,IY,IZ),PMV2(IX,IY,IZ) )
      ALLOCATE ( Z2(IX,IY,IZ1),P2(IX,IY,IZ1),PMID2(IX,IY,IZ) )
      ALLOCATE ( HLON2(IX,IY),HLAT2(IX,IY),VLON2(IX,IY),VLAT2(IX,IY) )
      ALLOCATE ( PD2(IX,IY),SLP2(IX,IY) )
      
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
        I1=I+I_ST-1
        J1=J+J_ST-1
        DIFFI=abs(HLON2(I,J)-HLON3(I1,J1))+    &
              abs(HLAT2(I,J)-HLAT3(I1,J1))
        IF(DIFFI.GT.0.01)THEN
          print*,'I,J,DIFFQ=',I,J,DIFFI
          STOP 353
        END IF
      END DO
      END DO


      z_diff=0.

      DO J=1,IY
      DO I=1,IX
        I1=I+I_ST-1
	J1=J+J_ST-1
	z_diff1=ABS(Z2(I,J,1)-Z3(I1,J1,1))
	IF(z_diff1.GT.z_diff)z_diff=z_diff1
      END DO
      END DO

      IF(z_diff.LT.10.)THEN

! replace all the data for the 1x domain

       DO J=1,IY
       DO I=1,IX
         J1=J+J_ST-1
         I1=I+I_ST-1
         DO K=1,NZ
           T2(I,J,K)=T3(I1,J1,K)
           Q2(I,J,K)=Q3(I1,J1,K)
           U2(I,J,K)=U3(I1,J1,K)
           V2(I,J,K)=V3(I1,J1,K)
         END DO
         DO K=1,NZ1
           Z2(I,J,K)=Z3(I1,J1,K)
           P2(I,J,K)=P3(I1,J1,K)
         END DO 
         PD2(I,J)=PD3(I1,J1)
       END DO
       END DO

       ELSE

       DO J=1,NY
       DO I=1,NX
	 ZSF1 = 0.5*(Z3(I,J,1)+Z3(I,J,2))
	 PSF1 = EXP((ALOG(P3(I,J,1))+ALOG(P3(I,J,2)))*0.5)
	 TV1 = T3(I,J,1)*(1.+D608*Q3(I,J,1))
	 A = (GAMMA * ZSF1) / TV1
	 SLP3(I,J) = PSF1*(1+A)**COEF2
       END DO
       END DO

       
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
	 PD2(I,J)=P2(I,J,1)-PDTOP2-PT2
       ENDDO
       ENDDO

! PD(I,J)=P1(I,J,1)-PDTOP-PT=PSFC(I,J)-PDTOP-PT
       DO K=1,IZ1
       DO J=1,IY
       DO I=1,IX
         P2(I,J,K)=PT2+PDTOP2*ETA1(K)+PD2(I,J)*ETA2(K)     ! PD(I,J) changed
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

      DO J=1,IY
      DO I=1,IX
	J1=J+J_ST-1
	I1=I+I_ST-1
	DO K=1,IZ
	  IF(PMID2(I,J,K).GT.PMID3(I1,J1,1))THEN
	    DZ1=T3(I1,J1,1)/GAMMA*(1.-(PMID2(I,J,K)/PMID3(I1,J1,1))**COEF3)
	    T2(I,J,K)=T3(I1,J1,1)-GAMMA*DZ1
	    Q2(I,J,K)=Q3(I1,J1,1)
          ELSE IF(PMID2(I,J,K).LE.PMID3(I1,J1,NZ))THEN
	    DZ1=T3(I1,J1,NZ)/GAMMA*(1.-(PMID2(I,J,K)/PMID3(I1,J1,NZ))**COEF3)
	    T2(I,J,K)=T3(I1,J1,NZ)-GAMMA*DZ1
	    Q2(I,J,K)=Q3(I1,J1,NZ)
	  ELSE
	    DO K1=2,NZ
              PDIF1=ALOG(PMID2(I,J,K))-ALOG(PMID3(I1,J1,K1))
              IF(PDIF1.GE.0.)THEN
                FACT1=PDIF1/(ALOG(PMID3(I1,J1,K1-1))-ALOG(PMID3(I1,J1,K1)))
	        T2(I,J,K)=T3(I1,J1,K1)*(1.-FACT1)+T3(I1,J1,K1-1)*FACT1
	        Q2(I,J,K)=Q3(I1,J1,K1)*(1.-FACT1)+Q3(I1,J1,K1-1)*FACT1
                GO TO 53
	      END IF
	    END DO
	  END IF
!	    if(K.eq.1)print*,'T2,Q2=',T2(I,J,K),Q2(I,J,K),PMID2(I,J,K),PMID3(I1,J1,1)
 53       CONTINUE
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
         PD2(I,J)=P2(I,J,1)-PDTOP2-PT2
       ENDDO
       ENDDO

! PD(I,J)=P1(I,J,1)-PDTOP-PT=PSFC(I,J)-PDTOP-PT
       DO K=1,IZ1
       DO J=1,IY
       DO I=1,IX
         P2(I,J,K)=PT2+PDTOP2*ETA1(K)+PD2(I,J)*ETA2(K)     ! PD(I,J) changed
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
	    U2(I,J,K)=U3(I1,J1,1)
	    V2(I,J,K)=V3(I1,J1,1)
          ELSE IF(PMV2(I,J,K).LE.PMV3(I1,J1,NZ))THEN
	    U2(I,J,K)=U3(I1,J1,NZ)
	    V2(I,J,K)=V3(I1,J1,NZ)
	  ELSE
	    DO K1=2,NZ
              PDIF1=ALOG(PMV2(I,J,K))-ALOG(PMV3(I1,J1,K1))
              IF(PDIF1.GE.0.)THEN
                FACT1=PDIF1/(ALOG(PMV3(I1,J1,K1-1))-ALOG(PMV3(I1,J1,K1)))
	        U2(I,J,K)=U3(I1,J1,K1)*(1.-FACT1)+U3(I1,J1,K1-1)*FACT1
	        V2(I,J,K)=V3(I1,J1,K1)*(1.-FACT1)+V3(I1,J1,K1-1)*FACT1
                GO TO 65
	      END IF
	    END DO
	  END IF
 65       CONTINUE
        END DO
      END DO
      END DO

       END IF

! set bc to be zero (consistent with the inner nest dynamics)

       DO J=1,IY,2
       DO K=1,NZ
         U2(IX,J,K)=0.
         V2(IX,J,K)=0.
       END DO
       END DO

! save 1x data

      IUNIT=50+ITIM

      WRITE(IUNIT) IX,IY,NZ
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

      CLOSE(IUNIT)

!      WRITE(61)((SLP2(I,J),I=1,IX),J=1,IY,2)
!      DO K=1,NZ+1
!        WRITE(61)((Z2(I,J,K),I=1,IX),J=1,IY,2)
!      END DO
!      DO K=1,NZ+1
!        WRITE(61)((P2(I,J,K),I=1,IX),J=1,IY,2)
!      END DO
!      DO K=1,NZ
!        WRITE(61)((T2(I,J,K),I=1,IX),J=1,IY,2)
!      END DO
!      DO K=1,NZ
!        WRITE(61)((Q2(I,J,K),I=1,IX),J=1,IY,2)
!      END DO
!      DO K=1,NZ
!        WRITE(61)((U2(I,J,K),I=1,IX),J=1,IY,2)
!      END DO
!      DO K=1,NZ
!        WRITE(61)((V2(I,J,K),I=1,IX),J=1,IY,2)
!      END DO


       END

