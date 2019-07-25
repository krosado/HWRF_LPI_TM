 program nest_start 

!==========================================================================================
!
! This program produces i_start and j_start for the nested domain depending on the 
! central lat-lon of the storm. This is gopal's doing
!
!==========================================================================================

 IMPLICIT NONE
 INTEGER                              :: I,J,PIDE,PJDE,NIDE,NJDE,ILOC,JLOC,RATIO,ITER
 REAL                                 :: DLMD,DPHD,SBD,WBD,DIFF1,DIFF2,ERR
 REAL                                 :: CLAT0,CLON0,SLAT0,SLON0
 REAL, DIMENSION(:,:), ALLOCATABLE    :: HLAT,HLON,VLAT,VLON 
 REAL                                 :: check_west, check_east
!zhang:added basin domain shift option
 CHARACTER*2 :: basin

!     inputs

      OPEN(1,FILE='domain.center',STATUS='OLD')
      OPEN(2,FILE='storm.center',STATUS='OLD')
      OPEN(11,FILE='set_nest',STATUS='UNKNOWN')
!
      READ(1,*)CLAT0              ! CENTRAL LAT THAT GOES INTO THE SI
      READ(1,*)CLON0              ! CENTRAL LON THAT GOES INTO THE SI
      READ(2,*)SLAT0              ! STORM LAT
      READ(2,*)SLON0              ! STORM LON
      READ(5,*) basin

      CALL read_namelist(PIDE,PJDE,NIDE,NJDE,DLMD,DPHD,RATIO)  

      WRITE(0,*)PIDE,PJDE,NIDE,NJDE,DLMD,DPHD,RATIO 

!
      WBD   =-(PIDE -2)*DLMD
      SBD   =-(PJDE -2)/2*DPHD


      ALLOCATE ( HLAT(PIDE,PJDE)  )
      ALLOCATE ( HLON(PIDE,PJDE)  )
      ALLOCATE ( VLAT(PIDE,PJDE)  )
      ALLOCATE ( VLON(PIDE,PJDE)  )

     CALL EARTH_LATLON (HLAT,HLON,VLAT,VLON,            &   
                        DLMD,DPHD,WBD,SBD,              &
                        CLAT0,CLON0,                    &
                        1,PIDE, 1,PJDE, 1,1,            &
                        1,PIDE, 1,PJDE, 1,1,            &
                        1,PIDE, 1,PJDE, 1,1             )

     ILOC=-99
     JLOC=-99
     ERR=0.01
     ITER=1
100  CONTINUE
!
     DO J = 1,PJDE
      DO I = 1,PIDE
        DIFF1 = ABS(SLAT0 - HLAT(I,J))
        DIFF2 = ABS(MOD(3600.+180.+(SLON0-HLON(I,J)),360.)-180.)
        IF(DIFF1 .LE. ERR .AND. DIFF2 .LE. ERR)THEN
         ILOC=I
         JLOC=J
         WRITE(0,*)I,J,HLAT(I,J),SLAT0,HLON(I,J),SLON0
        ENDIF 
      ENDDO
     ENDDO
 
     IF(ILOC .EQ. -99 .AND. JLOC .EQ. -99)THEN
       ERR=ERR+0.1
       ITER=ITER+1
       IF(ITER .GE. 100)THEN
        WRITE(0,*)'********** ERROR IN STORM CENTER *********'
        WRITE(0,*)'CHECK STORMLAT AND STORMLON IN history1 FILE'
        WRITE(0,*)'STORMLAT SHOULD BE POSITIVE IN NORTHERN HEMISPHERE'
        WRITE(0,*)'STORMLON SHOULD BE NEGATIVE WEST OF THE DATELINE'
        STOP  
       ENDIF
       GO TO 100
     ENDIF

     IF(ILOC .NE. -99 .AND. JLOC .NE. -99)THEN
       WRITE(0,*)'NOTE: I_PARENT_START AND J_PARENT_START FOUND FOR THE NESTED DOMAIN CONFIGURATION AT ITER=',ITER
       WRITE(11,1010)'istart=',ILOC-(NIDE-2)/6
       WRITE(11,1010)'jstart=',int(JLOC-(NJDE-2)/6+1)/2*2-1
     ELSE
       WRITE(0,*)'WARNING: COULD NOT LOCATE I_PARENT_START AND J_PARENT_START FROM INPUT STORM INFO'
       WRITE(11,1010)'export ISTART=',PIDE/3
       WRITE(11,1010)'export JSTART=',PJDE/3
     ENDIF

1010 format(A7,I5.5)

     DEALLOCATE ( HLAT )
     DEALLOCATE ( HLON )
     DEALLOCATE ( VLAT )
     DEALLOCATE ( VLON )

contains

!-----------------------------------------------------------------------------------------
SUBROUTINE EARTH_LATLON ( HLAT,HLON,VLAT,VLON,     & !Earth lat,lon at H and V points
                          DLMD1,DPHD1,WBD1,SBD1,   & !input res,west & south boundaries,
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
 REAL,       INTENT(IN   )                            :: DLMD1,DPHD1,WBD1,SBD1
 REAL,       INTENT(IN   )                            :: CENTRAL_LAT,CENTRAL_LON
 REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(OUT)        :: HLAT,HLON,VLAT,VLON

! local


 INTEGER,PARAMETER                           :: KNUM=SELECTED_REAL_KIND(13) 
 INTEGER                                     :: I,J
 REAL(KIND=KNUM)                             :: WB,SB,DLM,DPH,TPH0,STPH0,CTPH0
 REAL(KIND=KNUM)                             :: TDLM,TDPH,TLMH,TLMV,TLMH0,TLMV0,TPHH,TPHV,DTR
 REAL(KIND=KNUM)                             :: STPH,CTPH,STPV,CTPV,PI_2
 REAL(KIND=KNUM)                             :: SPHH,CLMH,FACTH,SPHV,CLMV,FACTV
 REAL(KIND=KNUM), DIMENSION(IMS:IME,JMS:JME) :: GLATH,GLONH,GLATV,GLONV
 REAL(KIND=KNUM) :: DLMD8,DPHD8,WBD8,SBD8,CLAT8,CLON8
 REAL(KIND=KNUM) :: CPHH, CPHV
!-------------------------------------------------------------------------
 DLMD8=DLMD1
 DPHD8=DPHD1
 WBD8=WBD1
 SBD8=SBD1
 CLAT8=CENTRAL_LAT
 CLON8=CENTRAL_LON
!
      PI_2 = ACOS(0.)
      DTR  = PI_2/90.
      WB   = WBD8 * DTR                 ! WB:   western boundary in radians
      SB   = SBD8 * DTR                 ! SB:   southern boundary in radians
      DLM  = DLMD8 * DTR                ! DLM:  dlamda in radians 
      DPH  = DPHD8 * DTR                ! DPH:  dphi   in radians
      TDLM = DLM + DLM                  ! TDLM: 2.0*dlamda 
      TDPH = DPH + DPH                  ! TDPH: 2.0*DPH 

!     For earth lat lon only

      TPH0  = CLAT8*DTR                ! TPH0: central lat in radians 
      STPH0 = SIN(TPH0)
      CTPH0 = COS(TPH0)

                                                !    .H
      DO J = JTS,MIN(JTE,JDE-1)                 ! H./    This loop takes care of zig-zag 
!                                               !   \.H  starting points along j  
         TLMH0 = WB - TDLM + MOD(J+1,2) * DLM   !  ./    TLMH (rotated lats at H points)
         TLMV0 = WB - TDLM + MOD(J,2) * DLM     !  H     (//ly for V points) 
         TPHH = SB + (J-1)*DPH                  !   TPHH (rotated lons at H points) are simple trans.
         TPHV = TPHH                            !   TPHV (rotated lons at V points) are simple trans.
         STPH = SIN(TPHH)
         CTPH = COS(TPHH)
         STPV = SIN(TPHV)
         CTPV = COS(TPHV)
                                                              !   .H
         DO I = ITS,MIN(ITE,IDE-1)                            !  / 
           TLMH = TLMH0 + I*TDLM                              !  \.H   .U   .H 
!                                                             !H./ ----><----
           SPHH = CTPH0 * STPH + STPH0 * CTPH * COS(TLMH)     !     DLM + DLM
           CPHH = sqrt(1-SPHH**2)
           GLATH(I,J)=ASIN(SPHH)                              ! GLATH: Earth Lat in radians 
           !CLMH = CTPH*COS(TLMH)/(COS(GLATH(I,J))*CTPH0) &
           !     - TAN(GLATH(I,J))*TAN(TPH0)
           CLMH = (CTPH*COS(TLMH)-SPHH*STPH0) / (CPHH*CTPH0)
           IF(CLMH .GT. 1.) CLMH = 1.0
           IF(CLMH .LT. -1.) CLMH = -1.0
           FACTH = 1.
           IF(TLMH .GT. 0.) FACTH = -1.
           GLONH(I,J) = -CLON8*DTR + FACTH*ACOS(CLMH)

         ENDDO                                    

         DO I = ITS,MIN(ITE,IDE-1)
           TLMV = TLMV0 + I*TDLM
           SPHV = CTPH0 * STPV + STPH0 * CTPV * COS(TLMV)
           CPHV = sqrt(1-SPHV**2)
           GLATV(I,J) = ASIN(SPHV)
           !CLMV = CTPV*COS(TLMV)/(COS(GLATV(I,J))*CTPH0) &
           !     - TAN(GLATV(I,J))*TAN(TPH0)
           CLMV = (CTPV*COS(TLMV)-SPHV*STPH0) / (CPHV*CTPH0)
           IF(CLMV .GT. 1.) CLMV = 1.
           IF(CLMV .LT. -1.) CLMV = -1.
           FACTV = 1.
           IF(TLMV .GT. 0.) FACTV = -1.
           GLONV(I,J) = -CLON8*DTR + FACTV*ACOS(CLMV)

         ENDDO

      ENDDO

!     Conversion to degrees (may not be required, eventually)

      DO J = JTS, MIN(JTE,JDE-1)
       DO I = ITS, MIN(ITE,IDE-1)
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

!-----------------------------------------------------------------------
      SUBROUTINE read_namelist(PIDE,PJDE,NIDE,NJDE,DLMD,DPHD,RATIO)

!-----------------------------------------------------------------------
!     Adopted from Tom's routine in ./frame/module_io_quilt.F
!-----------------------------------------------------------------------
      implicit none
!-----------------------------------------------------------------------
      CHARACTER*80 :: filename
      integer, intent(out)       :: PIDE,PJDE,NIDE,NJDE,RATIO
      real,    intent(out)       :: DLMD,DPHD

      integer, parameter :: md=20             ! maximum domains

      integer :: i_parent_start(md),j_parent_start(md)
      integer :: time_step,time_step_fract_num,time_step_fract_den,                 & 
                 max_dom,s_we(MD),e_we(MD),s_sn(MD),e_sn(MD),s_vert(MD),e_vert(MD), &
                 grid_id(MD),tile_sz_x,tile_sz_y,numtiles,nproc_x,                  &
                 nproc_y,level,parent_id(MD),                                       &
                 parent_grid_ratio(MD),parent_time_step_ratio(MD),feedback,         &
                 smooth_option,num_moves,move_id,move_interval,                     &
                 move_cd_x,move_cd_y,num_metgrid_levels,p_top_requested,ptsgm

!zhang      real   ::  dx(MD),dy(MD),ztop
      INTEGER , PARAMETER :: max_eta         =   501
!zhang      real   ::  dx(MD),dy(MD),ztop,eta_levels(43)
      real   ::  dx(MD),dy(MD),ztop,eta_levels(max_eta)
      logical :: use_prep_hybrid
      integer :: num_metgrid_soil_levels, halo_debug,coral_x(MD),coral_y(MD)

      namelist /domains/time_step,time_step_fract_num,time_step_fract_den,     &
                        max_dom,s_we,e_we,s_sn,e_sn,s_vert,e_vert,dx,dy,       &
                        grid_id,tile_sz_x,tile_sz_y,numtiles,nproc_x,          &
                        nproc_y,parent_id,i_parent_start,j_parent_start,       &
                        parent_grid_ratio,parent_time_step_ratio,feedback,     &
                        num_moves,num_metgrid_levels,p_top_requested,          &
                  ptsgm, eta_levels,use_prep_hybrid, num_metgrid_soil_levels,  &
                  halo_debug,smooth_option,coral_x,coral_y
!-----------------------------------------------------------------------

!        open(unit=12,file="namelist_move.input",form="formatted"          &
!     &,      status="old")


!        read(*,*)filename
!        write(*,*)trim(filename)
!        open(unit=12,file=trim(filename),form="formatted"          &
!     &,      status="old")

        read(12,domains)

        PIDE = e_we(1)
        PJDE = e_sn(1)
        NIDE = e_we(2)
        NJDE = e_sn(2)
        DLMD = dx(1)
        DPHD = dy(1)
        RATIO= parent_grid_ratio(2) 

        close(12)
!-----------------------------------------------------------------------
      END SUBROUTINE read_namelist 
!-----------------------------------------------------------------------
end program nest_start 
