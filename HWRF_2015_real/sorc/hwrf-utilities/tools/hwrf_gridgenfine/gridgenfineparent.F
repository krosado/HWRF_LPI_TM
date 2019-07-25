 program earth_latlon_grid

 IMPLICIT NONE
 INTEGER                           :: IDE,JDE,P_IDE,P_JDE,I,J,II,JJ,NII,NJJ
 INTEGER                           :: ITS,ITE,JTS,JTE,KTS,KTE,ILOC,JLOC
 REAL                              :: DLMD,DPHD,SBD,WBD,WBD0,SBD0,DLMD0,DPHD0,CLAT0,CLON0
 REAL                              :: CENTRAL_LAT,CENTRAL_LON,SW_LATD,SW_LOND
 REAL, DIMENSION(:,:),    ALLOCATABLE :: HLAT,HLON,VLAT,VLON 
 REAL, DIMENSION(:,:),    ALLOCATABLE :: NHLAT,NHLON,NVLAT,NVLON 
 REAL, DIMENSION(:,:,:),  ALLOCATABLE :: HBWGT,VBWGT
 INTEGER, DIMENSION(:,:), ALLOCATABLE :: IIH,JJH,IIV,JJV

!    inputs

      DLMD=0.18 
      DPHD=0.18
      DLMD0=DLMD
      DPHD0=DPHD
      CENTRAL_LAT=35.0
      CENTRAL_LAT=25.2
      CENTRAL_LON=-81.9
!     read in cental lat and lon
      read(10,*) CENTRAL_LAT
      read(10,*) CENTRAL_LON
!
      CLAT0=CENTRAL_LAT
      CLON0=CENTRAL_LON
!
      IDE= 160                  ! ACTUALLY ODD; LAST COLUMN IS DUMMY IN WRF
      IDE= 216
      JDE= 310                  ! ACTUALLY ODD; LAST COLUMN IS DUMMY IN WRF 
      JDE= 432
      WBD=-(IDE -2)*DLMD        ! DLMD: dlamda in degrees
      SBD=-(JDE -2)/2*DPHD      ! DPHD: dphi in degrees
      WBD0=WBD
      SBD0=SBD
      P_IDE=IDE
      P_JDE=JDE

     ALLOCATE ( HLAT(IDE,JDE)  )
     ALLOCATE ( HLON(IDE,JDE)  )
     ALLOCATE ( VLAT(IDE,JDE)  )
     ALLOCATE ( VLON(IDE,JDE)  )

     CALL EARTH_LATLON (HLAT,HLON,VLAT,VLON,            &   
                        DLMD,DPHD,WBD,SBD,              &
                        CENTRAL_LAT,CENTRAL_LON,        &
                        1,ide, 1,jde, 1,1,              &
                        1,ide, 1,jde, 1,1,              &
                        1,ide, 1,jde, 1,1               )

    do j=1,jde
    do i=1,ide
    WRITE(60,*) 'parent hlon,hlat ',i,j,hlon(i,j),hlat(i,j)
    enddo
    enddo

! now do nest over all a parent domain...
!old      DLMD=DLMD/3. ! 2.
!old      DPHD=DPHD/3. ! 2.
!     Domain 03
      DLMD=DLMD/3./3.0  ! 2.
      DPHD=DPHD/3./3.0  ! 2.
      IDE=160
      IDE= 647                   !3x215+2
      IDE= 1937                   !9x215+2
      JDE=310
      JDE= 1294                  !3x433+1
      JDE= 3880                  !9x431+1
      ILOC=1
      JLOC=1
      WBD= WBD0 + (ILOC -1)*2.*DLMD0 + MOD(JLOC+1,2)*DLMD0
      SBD= SBD0 + (JLOC -1)*DPHD0

     ALLOCATE ( NHLAT(IDE,JDE)  )
     ALLOCATE ( NHLON(IDE,JDE)  )
     ALLOCATE ( NVLAT(IDE,JDE)  )
     ALLOCATE ( NVLON(IDE,JDE)  )



     CALL EARTH_LATLON (  NHLAT,NHLON,NVLAT,NVLON,  &    ! rotated lat,lon at H and V points
                          DLMD,DPHD,WBD,SBD,        &
                          CENTRAL_LAT,CENTRAL_LON,  &
                          1,ide, 1,jde, 1,1,        &
                          1,ide, 1,jde, 1,1,        &
                          1,ide, 1,jde, 1,1         )
    do j=1,jde
    do i=1,ide
    WRITE(65,*) i,j,nhlon(i,j),nhlat(i,j) ,' fine  nhlon,nhlat '
    enddo
    enddo


    stop

end program earth_latlon_grid 

!-----------------------------------------------------------------------------------------

SUBROUTINE EARTH_LATLON ( HLAT,HLON,VLAT,VLON,     & !Earth lat,lon at H and V points
                          DLMD,DPHD,WBD,SBD,       & !input res,west & south boundaries,
                          CENTRAL_LAT,CENTRAL_LON, & ! central lat,lon, all in degrees   
                          IDS,IDE,JDS,JDE,KDS,KDE, &  
                          IMS,IME,JMS,JME,KMS,KME, &
                          ITS,ITE,JTS,JTE,KTS,KTE  )
!============================================================================
!
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME 
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE  
 REAL,       INTENT(IN   )                            :: DLMD,DPHD,WBD,SBD
 REAL,       INTENT(IN   )                            :: CENTRAL_LAT,CENTRAL_LON
 REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(OUT)        :: HLAT,HLON,VLAT,VLON

! local

 INTEGER,PARAMETER                           :: KNUM=SELECTED_REAL_KIND(13) 
 INTEGER                                     :: I,J
 REAL(KIND=KNUM)                             :: WB,SB,DLM,DPH,TPH0,STPH0,CTPH0
 REAL(KIND=KNUM)                             :: TDLM,TDPH,TLMH,TLMV,TPHH,TPHV,DTR
 REAL(KIND=KNUM)                             :: STPH,CTPH,STPV,CTPV,PI_2
 REAL(KIND=KNUM)                             :: SPHH,CLMH,FACTH,SPHV,CLMV,FACTV
 REAL(KIND=KNUM), DIMENSION(IMS:IME,JMS:JME) :: GLATH,GLONH,GLATV,GLONV
!-------------------------------------------------------------------------

!

      PI_2 = ACOS(0.)
      DTR  = PI_2/90.
      WB   = WBD * DTR                 ! WB:   western boundary in radians
      SB   = SBD * DTR                 ! SB:   southern boundary in radians
      DLM  = DLMD * DTR                ! DLM:  dlamda in radians 
      DPH  = DPHD * DTR                ! DPH:  dphi   in radians
      TDLM = DLM + DLM                 ! TDLM: 2.0*dlamda 
      TDPH = DPH + DPH                 ! TDPH: 2.0*DPH 

!     For earth lat lon only

      TPH0  = CENTRAL_LAT*DTR                ! TPH0: central lat in radians 
      STPH0 = SIN(TPH0)
      CTPH0 = COS(TPH0)

      WRITE(6,*) 'WB,SB,DLM,DPH,DTR: ',WBD,SBD,DLM,DPH,DTR      
                                                !    .H
      DO J = JDS,JDE                            ! H./    This loop takes care of zig-zag 
!                                               !   \.H  starting points along j  
         TLMH = WB - TDLM + MOD(J+1,2) * DLM    !  ./    TLMH (rotated lats at H points)
         TLMV = WB - TDLM + MOD(J,2) * DLM      !  H     (//ly for V points) 
         TPHH = SB + (J-1)*DPH                  !   TPHH (rotated lons at H points) are simple trans.
         TPHV = TPHH                            !   TPHV (rotated lons at V points) are simple trans.
         STPH = SIN(TPHH)
         CTPH = COS(TPHH)
         STPV = SIN(TPHV)
         CTPV = COS(TPHV)

                                                              !   .H
         DO I = IDS,IDE                                       !  / 
           TLMH = TLMH + TDLM                                 !  \.H   .U   .H 
!                                                             !H./ ----><----
           SPHH = CTPH0 * STPH + STPH0 * CTPH * COS(TLMH)     !     DLM + DLM
           GLATH(i,j)=ASIN(SPHH)                              ! GLATH: Earth Lat in radians 
           CLMH = CTPH*COS(TLMH)/(COS(GLATH(I,J))*CTPH0) &
                - TAN(GLATH(I,J))*TAN(TPH0)
           IF(CLMH .GT. 1.) CLMH = 1.0
           IF(CLMH .LT. -1.) CLMH = -1.0
           FACTH = 1.
           IF(TLMH .GT. 0.) FACTH = -1.
           GLONH(I,J) = -CENTRAL_LON*DTR + FACTH*ACOS(CLMH)
         ENDDO                                    

         DO I = IDS,IDE
           TLMV = TLMV + TDLM
           SPHV = CTPH0 * STPV + STPH0 * CTPV * COS(TLMV)
           GLATV(I,J) = ASIN(SPHV)
           CLMV = CTPV*COS(TLMV)/(COS(GLATV(I,J))*CTPH0) &
                - TAN(GLATV(I,J))*TAN(TPH0)
           IF(CLMV .GT. 1.) CLMV = 1.
           IF(CLMV .LT. -1.) CLMV = -1.
           FACTV = 1.
           IF(TLMV .GT. 0.) FACTV = -1.
           GLONV(I,J) = -CENTRAL_LON*DTR + FACTV*ACOS(CLMV)
         ENDDO

      ENDDO


!     Conversion to degrees (may not be required, eventually)

      DO J = JDS,JDE
       DO I = IDS,IDE
          HLAT(I,J) = GLATH(I,J) / DTR
          HLON(I,J)= -GLONH(I,J)/DTR
          IF(HLON(I,J) .GT. 180.) HLON(I,J) = HLON(I,J)  - 360.
          IF(HLON(I,J) .LT. -180.) HLON(I,J) = HLON(I,J) + 360.
!
          VLAT(I,J) = GLATV(I,J) / DTR
          VLON(I,J) = -GLONV(I,J) / DTR
          IF(VLON(I,J) .GT. 180.) VLON(I,J) = VLON(I,J)  - 360.
          IF(VLON(I,J) .LT. -180.) VLON(I,J) = VLON(I,J) + 360.
       ENDDO
      ENDDO

END SUBROUTINE EARTH_LATLON
