
      PROGRAM HWRF_MERGE_NEST

!??????????????????????????????????????????????????????????
!     
! ABSTRACT: Blend GSI analysis data with vortex initialization data
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

      REAL(4), ALLOCATABLE :: T1(:,:,:),Q1(:,:,:)
      REAL(4), ALLOCATABLE :: U1(:,:,:),V1(:,:,:),PMV1(:,:,:)
      REAL(4), ALLOCATABLE :: Z1(:,:,:),P1(:,:,:),PMID1(:,:,:)
      REAL(4), ALLOCATABLE :: SLP1(:,:)

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
      REAL(4), ALLOCATABLE :: HLON2(:,:),HLAT2(:,:)
      REAL(4), ALLOCATABLE :: VLON2(:,:),VLAT2(:,:)
      REAL(4), ALLOCATABLE :: PD2(:,:),SLP2(:,:)
   
      CHARACTER*2 :: basin !zhang

      CHARACTER SN*1,EW*1,DEPTH*1
      integer id_storm
      integer Ir_v4(4)

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

!!!!!!!!!!!!!!!!!!* Read TC vitals ...
      read(11,11)id_storm,ICLAT,SN,ICLON,EW,Ipsfc,Ipcls,      &
                 Irmax,ivobs,Ir_vobs,(Ir_v4(I),I=1,4),DEPTH
 11   format(5x,I2,26x,I3,A1,I5,A1,9x,I4,1x,I4,1x,I4,I3,I4,4I5,1x,A1)

      print *,'ICLAT, SN, ICLON, EW=', ICLAT, SN, ICLON, EW

       if(SN.eq.'S')ICLAT=-ICLAT
!
       if(EW.eq.'W')ICLON=-ICLON
       CLAT_NHC=ICLAT*0.1
       CLON_NHC=ICLON*0.1
       VRmax=Ir_vobs*1.  !* RMW  (km)

!       r_blend=max(150.,1.5*VRmax)/DIST1
       RMN=max(150.,1.5*VRmax)/DIST1
       RMN5=RMN+150./DIST1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! define DLMD3,DPHD3,PT3,PDTOP3,CLON3,CLAT3 for 4x degree2 domain
!
! READ DATA    ! new data after GSI analysis

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

      if(basin == "AL" .or. basin == "EP" .or. basin == "CP" ) then
      IF(CLON_NHC.GT.60.)CLON_NHC=CLON_NHC-360.
      DO J=1,NY
      DO I=1,NX
        IF(HLON3(I,J).GT.60.)HLON3(I,J)=HLON3(I,J)-360.
      END DO
      END DO
      endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! READ DATA    ! new data before GSI analysis

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
      ALLOCATE ( PD2(IX,IY),SLP2(IX,IY) )
      
      READ(IUNIT) DLMD2,DPHD2
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

      if(basin == "AL" .or. basin == "EP" .or. basin == "CP" ) then
      DO J=1,IY
      DO I=1,IX
        IF(HLON2(I,J).GT.60.)HLON2(I,J)=HLON2(I,J)-360.
      END DO
      END DO
      endif

      FLON=HLON2(1,1)
      FLAT=HLAT2(1,1)

      print*,'FLON,FLAT=',FLON,FLAT
      print*,'CLON_NHC,CLAT_NHC=',CLON_NHC,CLAT_NHC

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

      
      iy2=iy/2
      ix2=ix/2
      k_top=10
      delt_p=1.e20
      do k=1,iz1
        delt_p1=abs(p2(ix2,iy2,k)-40000.)          ! 400mb
        if(delt_p.gt.delt_p1)then
          delt_p=delt_p1
          k_top=k
        end if
      end do

      k_bot=10
      delt_p=1.e20
      do k=1,iz1
        delt_p1=abs(p2(ix2,iy2,k)-60000.)           ! 600mb
        if(delt_p.gt.delt_p1)then
          delt_p=delt_p1
          k_bot=k
        end if
      end do

      ALLOCATE ( T1(NX,NY,NZ),Q1(NX,NY,NZ) )
      ALLOCATE ( U1(NX,NY,NZ),V1(NX,NY,NZ),PMV1(NX,NY,NZ) )
      ALLOCATE ( Z1(NX,NY,NZ1),P1(NX,NY,NZ1),PMID1(NX,NY,NZ) )
      ALLOCATE ( SLP1(NX,NY) )

      T1=T3
      Q1=Q3
      U1=U3
      V1=V3
      Z1=Z3
      P1=P3

      rad_fact=1.0
      delt_p_invt=1./(z2(i,j,k_top)-z2(i,j,k_bot))
      do j = 1,NY
      do i = 1,NX
        DO k=1,k_top
          coeff1=max((z2(i,j,k)-z2(i,j,k_bot)),0.)*delt_p_invt
          coeff=coeff1*coeff1*(3.-2.*coeff1)*rad_fact
          T1(i,j,k)=T2(i,j,k)*(1.-coeff)+T3(i,j,k)*coeff
          Q1(i,j,k)=Q2(i,j,k)*(1.-coeff)+Q3(i,j,k)*coeff
          U1(i,j,k)=U2(i,j,k)*(1.-coeff)+U3(i,j,k)*coeff
          V1(i,j,k)=V2(i,j,k)*(1.-coeff)+V3(i,j,k)*coeff
          P1(i,j,k)=P2(i,j,k)*(1.-coeff)+P3(i,j,k)*coeff
        END DO
      end do
      end do

      cost=cos(pi180*CLAT_NHC)

!!$omp parallel do &
!!$omp& private(i,j,k,RDST,WT2)
       DO J=1,NY
       DO I=1,NX
!         WT2=XX1(I)*YY1(J)
         RDST=SQRT(((HLON3(I,J)-CLON_NHC)*cost)**2+(HLAT3(I,J)-CLAT_NHC)**2)
         IF(RDST.GE.RMN5)THEN
           WT2=0.
         ELSE IF(RDST.GT.RMN.and.RDST.LT.RMN5)THEN
           WT2=(RMN5-RDST)/(RMN5-RMN)
           WT2=WT2*WT2*(3.-2.*WT2)
         ELSE
           WT2=1.
         END IF
         DO K=1,NZ
           T2(I,J,K)=WT2*T1(I,J,K)+(1.-WT2)*T3(I,J,K)
           Q2(I,J,K)=WT2*Q1(I,J,K)+(1.-WT2)*Q3(I,J,K)
           U2(I,J,K)=WT2*U1(I,J,K)+(1.-WT2)*U3(I,J,K)
           V2(I,J,K)=WT2*V1(I,J,K)+(1.-WT2)*V3(I,J,K)
           P2(I,J,K)=WT2*P1(I,J,K)+(1.-WT2)*P3(I,J,K)
         END DO
         PD2(I,J)=WT2*PD2(I,J)+(1.-WT2)*PD3(I,J)
       ENDDO
       ENDDO

      if(basin == "AL" .or. basin == "EP" .or. basin == "CP" ) then
      DO J=1,IY
      DO I=1,IX
        IF(HLON2(I,J).LT.(-180.))HLON2(I,J)=HLON2(I,J)+360.
      END DO
      END DO
      endif

      IUNIT=50+ITIM

      WRITE(IUNIT) IX,IY,NZ
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

