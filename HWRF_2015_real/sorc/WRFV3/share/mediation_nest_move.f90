
SUBROUTINE med_nest_move ( parent, nest )
  
   USE module_domain, ONLY : domain, get_ijk_from_grid, adjust_domain_dims_for_move
   USE module_utility
   USE module_timing
   USE module_configure, ONLY : grid_config_rec_type, model_config_rec, model_to_grid_config_rec
   USE module_state_description

   USE module_dm, ONLY : wrf_dm_move_nest
   TYPE(domain) , POINTER                     :: parent, nest, grid
   INTEGER dx, dy       

  
   CHARACTER*256 mess
   INTEGER i, j, p, parent_grid_ratio
   INTEGER px, py       
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   INTEGER ierr, fid

   REAL,PARAMETER           :: con_g       =9.80665e+0
   REAL,PARAMETER           :: con_rd      =2.8705e+2 
   REAL                     :: TLAP,TBAR,EPSI

   LOGICAL input_from_hires
   LOGICAL saved_restart_value
   TYPE (grid_config_rec_type)   :: config_flags
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   LOGICAL, EXTERNAL :: should_not_move


   INTEGER                  :: k,idum1,idum2 
   INTEGER                  :: ITS,ITE,JTS,JTE,KTS,KTE




   INTERFACE
     SUBROUTINE med_interp_domain ( parent , nest )
        USE module_domain, ONLY : domain
        IMPLICIT NONE
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_interp_domain



     SUBROUTINE start_domain ( grid , allowed_to_move )
        USE module_domain, ONLY : domain
        IMPLICIT NONE
        TYPE(domain) :: grid
        LOGICAL, INTENT(IN) :: allowed_to_move
     END SUBROUTINE start_domain

     SUBROUTINE med_nest_egrid_configure ( parent , nest )
        USE module_domain
        IMPLICIT NONE
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_nest_egrid_configure

     SUBROUTINE med_construct_egrid_weights ( parent , nest )
        USE module_domain
        IMPLICIT NONE
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_construct_egrid_weights

     SUBROUTINE BASE_STATE_PARENT ( Z3d,Q3d,T3d,PSTD,        &
                                    PINT,T,Q,CWM,            &
                                    FIS,QSH,PD,PDTOP,PTOP,   &
                                    ETA1,ETA2,               &
                                    DETA1,DETA2,             &
                                    IDS,IDE,JDS,JDE,KDS,KDE, &
                                    IMS,IME,JMS,JME,KMS,KME, &
                                    IPS,IPE,JPS,JPE,KPS,KPE  )


         USE MODULE_MODEL_CONSTANTS
         IMPLICIT NONE
         INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
         INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
         INTEGER,    INTENT(IN   )                            :: IPS,IPE,JPS,JPE,KPS,KPE
         REAL,       INTENT(IN   )                            :: PDTOP,PTOP
         REAL, DIMENSION(KMS:KME),                 INTENT(IN) :: ETA1,ETA2,DETA1,DETA2
         REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(IN) :: FIS,PD,QSH
         REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME), INTENT(IN) :: PINT,T,Q,CWM
         REAL, DIMENSION(KMS:KME)                , INTENT(OUT):: PSTD
         REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME), INTENT(OUT):: Z3d,Q3d,T3d

     END SUBROUTINE BASE_STATE_PARENT

     SUBROUTINE NEST_TERRAIN ( nest, config_flags )
       USE module_domain, ONLY : domain
       USE module_configure, ONLY : grid_config_rec_type
       IMPLICIT NONE
       TYPE(domain) , POINTER                        :: nest
       TYPE(grid_config_rec_type) , INTENT(IN)       :: config_flags
     END SUBROUTINE NEST_TERRAIN

     SUBROUTINE med_init_domain_constants_nmm ( parent, nest )
        USE module_domain, ONLY : domain
        IMPLICIT NONE
        TYPE(domain) , POINTER                    :: parent , nest
     END SUBROUTINE med_init_domain_constants_nmm

     SUBROUTINE shift_domain_nmm ( grid, disp_x, disp_y &







,szj,s1z,spz,tcs,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist, &
dfi_moist_bxs,dfi_moist_bxe,dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar, &
scalar_bxs,scalar_bxe,scalar_bys,scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs, &
dfi_scalar_bxe,dfi_scalar_bys,dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,chem,ozmixm &


                           )
        USE module_domain
        IMPLICIT NONE
        INTEGER disp_x, disp_y
        TYPE(domain) , POINTER                 :: grid






real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_szj)           :: szj
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_s1z)           :: s1z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_spz)           :: spz
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tcs)           :: tcs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)           :: dfi_moist
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)           :: dfi_scalar
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32,num_chem)           :: chem
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%levsiz,grid%sm32:grid%em32,num_ozmixm)           :: ozmixm

     END SUBROUTINE shift_domain_nmm




     LOGICAL FUNCTION direction_of_move ( parent , nest , dx , dy )
        USE module_domain, ONLY : domain
        IMPLICIT NONE
        TYPE(domain) , POINTER    :: parent , nest
        INTEGER, INTENT(OUT)      :: dx , dy
     END FUNCTION direction_of_move




   END INTERFACE

  
   grid => nest

   IF ( should_not_move( nest%id ) ) THEN
      CALL wrf_message( 'Nest movement is disabled because of namelist settings' )
      RETURN
   ENDIF


   IF ( WRFU_ClockIsStopTime(nest%domain_clock ,rc=ierr) ) RETURN



  check_direction_of_move: IF ( direction_of_move ( parent , nest , dx, dy ) ) THEN

     IF(MOD(dy,2) .NE. 0)THEN
       dy=dy+sign(1,dy)
       WRITE(mess,*)'WARNING: DY REDEFINED FOR THE NMM CORE AND RE-SET TO MASS POINT dy=',dy
       call wrf_debug(1,mess)
     ENDIF

     IF ( dx .gt. 1 .or. dx .lt. -1 .or. dy .gt. 2 .or. dy .lt. -2 ) THEN
3038 format("med_nest_move: TRIED TO SHIFT TOO FAR: dx must be in [-1,1] and dy in [-2,2] but dx=",I0," and dy=",I0)
       WRITE(mess,3038) dx,dy
       CALL wrf_error_fatal3("<stdin>",215,&
mess )
     ENDIF

     IF (  wrf_dm_on_monitor() ) THEN
       WRITE(mess,*)' moving ',grid%id,dx,dy
       CALL wrf_message(mess)
     ENDIF

     CALL get_ijk_from_grid (  grid ,                   &
                               ids, ide, jds, jde, kds, kde,    &
                               ims, ime, jms, jme, kms, kme,    &
                               ips, ipe, jps, jpe, kps, kpe    )

     CALL wrf_dm_move_nest ( parent, nest%intermediate_grid, dx, dy )

     CALL adjust_domain_dims_for_move( nest%intermediate_grid , dx, dy )

     CALL get_ijk_from_grid (  grid ,                   &
                               ids, ide, jds, jde, kds, kde,    &
                               ims, ime, jms, jme, kms, kme,    &
                               ips, ipe, jps, jpe, kps, kpe    )

     grid => nest 

     CALL shift_domain_nmm( grid, dx, dy &







,grid%szj,grid%s1z,grid%spz,grid%tcs,grid%moist,grid%moist_bxs,grid%moist_bxe,grid%moist_bys,grid%moist_bye,grid%moist_btxs, &
grid%moist_btxe,grid%moist_btys,grid%moist_btye,grid%dfi_moist,grid%dfi_moist_bxs,grid%dfi_moist_bxe,grid%dfi_moist_bys, &
grid%dfi_moist_bye,grid%dfi_moist_btxs,grid%dfi_moist_btxe,grid%dfi_moist_btys,grid%dfi_moist_btye,grid%scalar,grid%scalar_bxs, &
grid%scalar_bxe,grid%scalar_bys,grid%scalar_bye,grid%scalar_btxs,grid%scalar_btxe,grid%scalar_btys,grid%scalar_btye, &
grid%dfi_scalar,grid%dfi_scalar_bxs,grid%dfi_scalar_bxe,grid%dfi_scalar_bys,grid%dfi_scalar_bye,grid%dfi_scalar_btxs, &
grid%dfi_scalar_btxe,grid%dfi_scalar_btys,grid%dfi_scalar_btye,grid%chem,grid%ozmixm &


                          )

     px = grid%parent_grid_ratio*dx
     py = grid%parent_grid_ratio*dy

     grid%i_parent_start = grid%i_parent_start + px / grid%parent_grid_ratio 
     CALL nl_set_i_parent_start( grid%id, grid%i_parent_start )
     grid%j_parent_start = grid%j_parent_start + py / grid%parent_grid_ratio
     CALL nl_set_j_parent_start( grid%id, grid%j_parent_start )

     IF ( wrf_dm_on_monitor() ) THEN
       write(mess,*)  &
         'Grid ',grid%id,' New SW corner (in parent x and y):',grid%i_parent_start, grid%j_parent_start
       CALL wrf_message(TRIM(mess))
     ENDIF






    CALL med_nest_egrid_configure ( parent , nest )





    CALL med_construct_egrid_weights ( parent, nest )






    CALL model_to_grid_config_rec ( nest%id , model_config_rec , config_flags )

    CALL NEST_TERRAIN ( nest, config_flags )

    CALL get_ijk_from_grid ( nest ,                   &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )



      TLAP=6.1/(con_g*1000.)
    DO J = MAX(JPS,JDS-PY), MIN(JPE,JDE-1-PY)
     DO I = MAX(IPS,IDS-PX), MIN(IPE,IDE-1-PX)
       if(  nest%fis(I,J).ne.nest%hres_fis(I,J) ) then
       if( nest%T(I,J,1).gt.150. .and. nest%T(I,J,1).lt.400.) then
       TBAR=ALOG(1.0+TLAP*(nest%fis(I,J)-nest%hres_fis(I,J)) /nest%T(I,J,1))
       EPSI=TBAR/(con_rd*TLAP)

       nest%PINT(I,J,1)=nest%PD(I,J)+nest%pdtop+nest%pt
       nest%PINT(I,J,1)=nest%PINT(I,J,1)*EXP(EPSI)
       nest%PD(I,J)=nest%PINT(I,J,1)-nest%pdtop-nest%pt  


       endif
       endif
     ENDDO
    ENDDO

    DO J = JPS, MIN(JPE,JDE-1)
      DO I = IPS, MIN(IPE,IDE-1)
       nest%fis(I,J)=nest%hres_fis(I,J)
     ENDDO
    ENDDO










    CALL get_ijk_from_grid ( parent ,                   &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )

    
    
    
    
    
    
    
    



    nest%PSTD=parent%PSTD
    nest%KZMAX=KME
    parent%KZMAX=KME  



    DO J = MIN(JPE,JDE-1), JPS, -1
       IF ( MOD(J,2) /= 0 ) THEN

       ELSE

       END IF
    ENDDO


     CALL med_interp_domain( parent, nest )






    CALL med_init_domain_constants_nmm ( parent, nest )




     nest%moved = .true.


     saved_restart_value = config_flags%restart
     config_flags%restart = .FALSE.
     CALL nl_set_restart ( 1, .FALSE. )
     grid%restart = .FALSE.
     CALL start_domain ( nest , .FALSE. )
     config_flags%restart = saved_restart_value
     grid%restart = saved_restart_value
     CALL nl_set_restart ( 1,  saved_restart_value )
     nest%moved = .false.
      





   ENDIF check_direction_of_move

END SUBROUTINE med_nest_move

LOGICAL FUNCTION time_for_move2 ( parent , grid , move_cd_x, move_cd_y )
  
   USE module_domain, ONLY : domain, domain_clock_get, get_ijk_from_grid, adjust_domain_dims_for_move

   USE module_driver_constants, ONLY : max_moves
   USE module_compute_geop
   USE module_dm, ONLY : wrf_dm_max_real, wrf_dm_move_nest
   USE module_utility
   USE module_streams, ONLY : compute_vortex_center_alarm
   IMPLICIT NONE

   TYPE(domain) , POINTER    :: parent, grid
   INTEGER, INTENT(OUT)      :: move_cd_x , move_cd_y

   INTEGER  num_moves, rc
   INTEGER  move_interval , move_id
   TYPE(WRFU_Time) :: ct, st
   TYPE(WRFU_TimeInterval) :: ti
   CHARACTER*256 mess, timestr
   INTEGER     :: ids, ide, jds, jde, kds, kde, &
                  ims, ime, jms, jme, kms, kme, &
                  ips, ipe, jps, jpe, kps, kpe
   INTEGER :: is, ie, js, je, ierr
   REAL    :: ipbar, pbar, jpbar, fact
   REAL    :: last_vc_i , last_vc_j

   REAL, ALLOCATABLE, DIMENSION(:,:) :: height_l, height
   REAL, ALLOCATABLE, DIMENSION(:,:) :: psfc, xlat, xlong, terrain
   REAL :: minh, maxh
   INTEGER :: mini, minj, maxi, maxj, i, j, pgr, irad
   REAL :: disp_x, disp_y, lag, radius, center_i, center_j, dx
   REAL :: dijsmooth, vmax, vmin, a, b
   REAL :: dc_i, dc_j   
   REAL :: maxws, ws
   REAL :: pmin
   INTEGER imploc, jmploc 

   INTEGER :: fje, fjs, fie, fis, fimloc, fjmloc, imloc, jmloc
   INTEGER :: i_parent_start, j_parent_start
   INTEGER :: max_vortex_speed, vortex_interval  
   INTEGER :: track_level
   REAL    :: rsmooth = 100000.  

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

character*256 message, message2



   move_cd_x = 0
   move_cd_y = 0
   time_for_move2 = .FALSE.
   CALL domain_clock_get( grid, current_time=ct, start_time=st )
   CALL nl_get_num_moves( 1, num_moves )
   IF ( num_moves .GT. max_moves ) THEN
     WRITE(mess,*)'time_for_moves2: num_moves (',num_moves,') .GT. max_moves (',max_moves,')'
     CALL wrf_error_fatal3("<stdin>",455,&
TRIM(mess) )
   ENDIF
   DO i = 1, num_moves
     CALL nl_get_move_id( i, move_id )
     IF ( move_id .EQ. grid%id ) THEN
       CALL nl_get_move_interval( i, move_interval )
       IF ( move_interval .LT. 999999999 ) THEN
         CALL WRFU_TimeIntervalSet ( ti, M=move_interval, rc=rc )
         IF ( ct .GE. st + ti ) THEN
           CALL nl_get_move_cd_x ( i, move_cd_x )
           CALL nl_get_move_cd_y ( i, move_cd_y )
           CALL nl_set_move_interval ( i, 999999999 )
           time_for_move2 = .TRUE.
           EXIT
         ENDIF
       ENDIF
     ENDIF
   ENDDO
   RETURN
END FUNCTION time_for_move2

LOGICAL FUNCTION time_for_move ( parent , grid , move_cd_x, move_cd_y )
   USE module_domain, ONLY : domain, get_ijk_from_grid, adjust_domain_dims_for_move

   USE module_dm, ONLY : wrf_dm_move_nest
USE module_timing
   USE module_utility
   IMPLICIT NONE

   TYPE(domain) , POINTER    :: parent, grid, par, nst
   INTEGER, INTENT(OUT)      :: move_cd_x , move_cd_y

   INTEGER     :: corral_dist, kid
   INTEGER     :: dw, de, ds, dn, pgr
   INTEGER     :: would_move_x, would_move_y
   INTEGER     :: cids, cide, cjds, cjde, ckds, ckde, &
                  cims, cime, cjms, cjme, ckms, ckme, &
                  cips, cipe, cjps, cjpe, ckps, ckpe, &
                  nids, nide, njds, njde, nkds, nkde, &
                  nims, nime, njms, njme, nkms, nkme, &
                  nips, nipe, njps, njpe, nkps, nkpe
   REAL        :: xtime, time_to_move

   INTERFACE
     LOGICAL FUNCTION time_for_move2 ( parent , nest , dx , dy )
        USE module_domain, ONLY : domain
        TYPE(domain) , POINTER    :: parent , nest
        INTEGER, INTENT(OUT)      :: dx , dy
     END FUNCTION time_for_move2
   END INTERFACE





   IF   ( grid%num_nests .GT. 1 ) THEN
     CALL wrf_error_fatal3("<stdin>",512,&
'domains in moving nest simulations can have only 1 nest' )
   ENDIF
   kid = 1



   IF   ( grid%num_nests .EQ. 0 ) THEN
     
     time_for_move = time_for_move2 ( parent , grid , move_cd_x, move_cd_y )

     
     
     par => grid%parents(1)%ptr
     nst => grid

     would_move_x = move_cd_x 
     would_move_y = move_cd_y

     
100  CONTINUE
       CALL nl_get_corral_dist ( nst%id , corral_dist )
       CALL get_ijk_from_grid (  nst ,                               &
                                 nids, nide, njds, njde, nkds, nkde, &
                                 nims, nime, njms, njme, nkms, nkme, &
                                 nips, nipe, njps, njpe, nkps, nkpe  )
       CALL get_ijk_from_grid (  par ,                               &
                                 cids, cide, cjds, cjde, ckds, ckde, &
                                 cims, cime, cjms, cjme, ckms, ckme, &
                                 cips, cipe, cjps, cjpe, ckps, ckpe  )
       CALL nl_get_parent_grid_ratio ( nst%id , pgr )
       
       
       dw = nst%i_parent_start + would_move_x - cids
       
       ds = nst%j_parent_start + would_move_y - cjds
       
       de = cide - ( nst%i_parent_start + (nide-nids+1)/pgr + would_move_x )
       
       dn = cjde - ( nst%j_parent_start + (njde-njds+1)/pgr + would_move_y )

       
       would_move_x = 0
       would_move_y = 0
       if ( dw .LE. corral_dist ) would_move_x = would_move_x - 1
       if ( de .LE. corral_dist ) would_move_x = would_move_x + 1
       if ( ds .LE. corral_dist ) would_move_y = would_move_y - 1
       if ( dn .LE. corral_dist ) would_move_y = would_move_y + 1

     IF ( par%id .EQ. 1 ) THEN
         IF ( would_move_x .NE. 0 .AND. move_cd_x .NE. 0 ) THEN
           CALL wrf_message('MOAD can not move. Cancelling nest move in X')
           if ( grid%num_nests .eq. 0 ) grid%vc_i = grid%vc_i + move_cd_x * pgr  
           move_cd_x = 0
         ENDIF
         IF ( would_move_y .NE. 0 .AND. move_cd_y .NE. 0 ) THEN
           CALL wrf_message('MOAD can not move. Cancelling nest move in Y')
           if ( grid%num_nests .eq. 0 ) grid%vc_j = grid%vc_j + move_cd_y * pgr  
           move_cd_y = 0
         ENDIF
     ELSE
         nst => par
         par => nst%parents(1)%ptr
         GOTO 100
     ENDIF


     time_for_move = ( move_cd_x .NE. 0 ) .OR. ( move_cd_y .NE. 0 )

   ELSE
     
     
     CALL nl_get_corral_dist ( grid%nests(kid)%ptr%id , corral_dist )
     
     CALL get_ijk_from_grid (  grid%nests(kid)%ptr ,               &
                               nids, nide, njds, njde, nkds, nkde, &
                               nims, nime, njms, njme, nkms, nkme, &
                               nips, nipe, njps, njpe, nkps, nkpe  )
     CALL get_ijk_from_grid (  grid ,                              &
                               cids, cide, cjds, cjde, ckds, ckde, &
                               cims, cime, cjms, cjme, ckms, ckme, &
                               cips, cipe, cjps, cjpe, ckps, ckpe  )
     CALL nl_get_parent_grid_ratio ( grid%nests(kid)%ptr%id , pgr )
     
     
     dw = grid%nests(kid)%ptr%i_parent_start - 1
     
     ds = grid%nests(kid)%ptr%j_parent_start - 1
     
     de = cide - ( grid%nests(kid)%ptr%i_parent_start + (nide-nids+1)/pgr )
     
     dn = cjde - ( grid%nests(kid)%ptr%j_parent_start + (njde-njds+1)/pgr )

     
     
     
     move_cd_x = 0
     move_cd_y = 0
     if ( dw .LE. corral_dist ) move_cd_x = move_cd_x - 1
     if ( de .LE. corral_dist ) move_cd_x = move_cd_x + 1
     if ( ds .LE. corral_dist ) move_cd_y = move_cd_y - 1
     if ( dn .LE. corral_dist ) move_cd_y = move_cd_y + 1

     time_for_move = ( move_cd_x .NE. 0 ) .OR. ( move_cd_y .NE. 0 )

     IF ( time_for_move ) THEN
       IF ( grid%id .EQ. 1 ) THEN

         CALL wrf_message( 'DANGER: Nest has moved too close to boundary of outermost domain.' )
         time_for_move = .FALSE.

       ELSE
         
         

         CALL wrf_dm_move_nest ( grid , grid%nests(kid)%ptr%intermediate_grid , -move_cd_x*pgr, -move_cd_y*pgr )
         CALL adjust_domain_dims_for_move( grid%nests(kid)%ptr%intermediate_grid , -move_cd_x*pgr, -move_cd_y*pgr )
         grid%nests(kid)%ptr%i_parent_start = grid%nests(kid)%ptr%i_parent_start - move_cd_x*pgr
         CALL nl_set_i_parent_start( grid%nests(kid)%ptr%id, grid%nests(kid)%ptr%i_parent_start )
         grid%nests(kid)%ptr%j_parent_start = grid%nests(kid)%ptr%j_parent_start - move_cd_y*pgr
         CALL nl_set_j_parent_start( grid%nests(kid)%ptr%id, grid%nests(kid)%ptr%j_parent_start )

       ENDIF
     ENDIF 

   ENDIF

   RETURN
END FUNCTION time_for_move


LOGICAL FUNCTION should_not_move ( id )
  USE module_state_description

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: id
 
  LOGICAL retval
  INTEGER cu_physics, ra_sw_physics, ra_lw_physics, sf_urban_physics, sf_surface_physics, obs_nudge_opt

  retval = .FALSE.

  CALL nl_get_cu_physics( id , cu_physics )
  IF ( cu_physics .EQ. GDSCHEME ) THEN
    CALL wrf_message('Grell cumulus can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF

  CALL nl_get_ra_sw_physics( id , ra_sw_physics )
  IF ( ra_sw_physics .EQ. CAMSWSCHEME ) THEN
    CALL wrf_message('CAM SW radiation can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF
  CALL nl_get_ra_lw_physics( id , ra_lw_physics )
  IF ( ra_lw_physics .EQ. CAMLWSCHEME ) THEN
    CALL wrf_message('CAM LW radiation can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF

  CALL nl_get_sf_urban_physics( id , sf_urban_physics )
  IF ( sf_urban_physics .EQ. 1 .OR. sf_urban_physics .EQ. 2 ) THEN
    CALL wrf_message('UCMs Noah LSM can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF

  CALL nl_get_sf_surface_physics( id , sf_surface_physics )
  IF ( sf_surface_physics .EQ. PXLSMSCHEME ) THEN
    CALL wrf_message('PX LSM can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF
  should_not_move = retval
END FUNCTION

LOGICAL FUNCTION direction_of_move2 ( parent , grid , move_cd_x, move_cd_y )
   USE module_domain
   USE module_configure
   USE module_dm
   IMPLICIT NONE

   TYPE(domain) , POINTER    :: parent, grid, kid
   LOGICAL, EXTERNAL         :: wrf_dm_on_monitor
   INTEGER, INTENT(OUT)      :: move_cd_x , move_cd_y
   CHARACTER*256 mess

   INTEGER     :: coral_dist, ikid
   INTEGER     :: dw, de, ds, dn, idum, jdum
   INTEGER     :: cids, cide, cjds, cjde, ckds, ckde, &
                  cims, cime, cjms, cjme, ckms, ckme, &
                  cips, cipe, cjps, cjpe, ckps, ckpe, &
                  nids, nide, njds, njde, nkds, nkde, &
                  nims, nime, njms, njme, nkms, nkme, &
                  nips, nipe, njps, njpe, nkps, nkpe
   real :: dx,dy,kid_ic,kid_jc,my_ic,my_jc,pgr,pgrn,hr,two_dt,when,before,after
   real, parameter :: pmult=1.35
   integer :: inew,jnew,coral_x,coral_y
   integer :: min_coral_x, min_coral_y
   logical :: abort

























   if(grid%id==1) then
      min_coral_x=7
      min_coral_y=12
   else
      min_coral_x=5
      min_coral_y=5
   endif

   abort=.false. 



   
   move_cd_x=0
   move_cd_y=0
   direction_of_move2 = .false.
   grid%moved = .false.

   
   
   if(grid%num_nests .gt. 1) then
      write(mess,'("d",I0,": not moving because it has more than one nest")') grid%id
      call WRF_MESSAGE(trim(mess))
      abort=.true.
   endif

   
   if(grid%nomove_freq_hr>0) then
      hr=(grid%ntsd*grid%dt)/3600.0
      when=anint(hr/grid%nomove_freq_hr)*grid%nomove_freq_hr

      before=when-3.0/60.0-grid%dt*2.0/3600.0
      after=when+grid%dt*2.0/3600.0

      if(hr>before.and.hr<after) then
         abort=.true.
         write(mess,'("d",I0,": cannot move: forecast hour too close to a ",F0.3,"-hourly time")') grid%id,grid%nomove_freq_hr
         call wrf_message(trim(mess))
      endif
   endif

   

   coral_x = max(min_coral_x,grid%coral_x)
   coral_y = max(min_coral_y,grid%coral_y)

   coral_dist=(grid%ed31+grid%parent_grid_ratio-1)/grid%parent_grid_ratio
   IF(grid%i_parent_start .le. coral_x) then
      abort=.true.
      write(mess,'("d",I0,": cannot move: too close to parent d",I0," -X boundary")') grid%id,parent%id
      call wrf_message(trim(mess))
   ELSEIF((grid%i_parent_start+coral_dist) .ge. parent%ed31 - coral_x)THEN  
      abort=.true.
      write(mess,'("d",I0,": cannot move: too close to parent d",I0," +X boundary")') grid%id,parent%id
      call wrf_message(trim(mess))
  ENDIF

   coral_dist=(grid%ed32+grid%parent_grid_ratio-1)/grid%parent_grid_ratio
   IF(grid%j_parent_start .le. coral_y) THEN
      abort=.true.
      write(mess,'("d",I0,": cannot move: too close to parent d",I0," -Y boundary")') grid%id,parent%id
      call wrf_message(trim(mess))
   ELSEIF((grid%j_parent_start+coral_dist) .ge. parent%ed32 - coral_y)THEN
      abort=.true.
      write(mess,'("d",I0,": cannot move: too close to parent d",I0," +Y boundary")') grid%id,parent%id
      call wrf_message(trim(mess))
   ENDIF
   
   
   
   can_move: if(grid%num_moves.eq.-99 .and. grid%mvnest .and. .not. abort) then

      if(wrf_dm_on_monitor() .and. .not. abort) then
         WRITE(mess,*)'vortex tracking: id,mvnest,num_moves,num_nests: ', &
              grid%id,grid%mvnest,grid%num_moves,grid%num_nests
         call wrf_debug(1,mess)
         
         WRITE(mess,*)'vortex tracking: xloc_1,xloc_2,yloc_y,yloc_2,vortex_tracker: ', &
              grid%XLOC_1,grid%XLOC_2,grid%YLOC_1,grid%YLOC_2,grid%vortex_tracker
         call wrf_debug(1,mess)
      endif

      nest_following: IF(grid%vortex_tracker==2)THEN
         
         pgr=grid%parent_grid_ratio+0.01
         pgrn=grid%parent_grid_ratio-0.01
         
         kid=>grid%nests(1)%ptr 
         
         
         my_ic = grid%ed31/2.0
         my_jc = grid%ed32/2.0
         
         
         kid_ic = kid%i_parent_start + kid%ed31/2.0/kid%parent_grid_ratio - 1
         kid_jc = kid%j_parent_start + kid%ed32/2.0/kid%parent_grid_ratio - 1
         
         
         dx=kid_ic-my_ic
         dy=kid_jc-my_jc

         if(wrf_dm_on_monitor()) then
            write(mess,'("d",I0," following nest d",I0,": parent ",F0.1,"x",F0.1," nest ",F0.1,"x",F0.1," move ",F0.1,"x",F0.1)') &
                 grid%id,kid%id,my_ic,my_jc,kid_ic,kid_jc,dx,dy
            call wrf_debug(1,trim(mess))
         endif

         
         if(dx<-pgr) then
            move_cd_x=-1
         elseif(dx>pgr) then
            move_cd_x=1
         endif

         if(dy>2.*pgr) then
            move_cd_y=2
         elseif(dy<-2.*pgr) then
            move_cd_y=-2
         endif

         

         
         
         
         

         
         
         
         
         if(move_cd_x==0 .and. move_cd_y/=0) then
            
            if(dy>-2.*pmult*pgrn .and. dy<2.*pmult*pgrn) then
               
               move_cd_y=0
            endif
         endif
         if(move_cd_x/=0 .and. move_cd_y==0) then
            
            if(dx>-pmult*pgrn .and. dx<pmult*pgrn) then
               
               move_cd_x=0
            endif
         endif

         
         if(wrf_dm_on_monitor()) then
            if(move_cd_x/=0 .or. move_cd_y/=0) then
               write(mess,'("d",I0," moving x",SP,I0," y",I0,SS," to follow d",I0)') &
                    grid%id,move_cd_x,move_cd_y,kid%id
               call wrf_debug(1,trim(mess))
            endif
         endif
      endif nest_following
      revised_nest_motion: if(grid%vortex_tracker>3) then
         if((grid%XLOC_1-grid%XLOC_2) .GE. 3) then
            move_cd_x=-1
         elseif((grid%XLOC_2-grid%XLOC_1) .GE. 3) then
            move_cd_x=1
         else
            move_cd_x=0
         endif
         if((grid%YLOC_2-grid%YLOC_1) .GE. 6) then
            move_cd_y=2
         elseif((grid%YLOC_1-grid%YLOC_2) .GE. 6) then
            move_cd_y=-2
         else
            move_cd_y=0
         endif
         if(wrf_dm_on_monitor()) then
            if(move_cd_x/=0 .or. move_cd_y/=0) then
               write(mess,'("d",I0," moving x",SP,I0," y",I0,SS," to follow vortex")') &
                    grid%id,move_cd_x,move_cd_y
               call wrf_debug(1,trim(mess))
            endif
         endif
      endif revised_nest_motion
      vortex_following: IF(grid%vortex_tracker==3 .or. grid%vortex_tracker==1)THEN
         IF((grid%XLOC_1-grid%XLOC_2) .GE. 3)THEN 
            move_cd_x  = -1
            IF((grid%YLOC_2-grid%YLOC_1) .GE. 3)THEN
               move_cd_y  = +1 
            ENDIF
         ELSE IF((grid%XLOC_2-grid%XLOC_1) .GE. 3)THEN        
            move_cd_x  = +1 
            IF((grid%YLOC_2-grid%YLOC_1) .GE. 3)THEN
               move_cd_y  = -1 
            ENDIF
         ELSE IF ((grid%YLOC_2-grid%YLOC_1) .GE. 3 .and. grid%vortex_tracker==1) THEN
            
            
            
            move_cd_y  = 2
         ELSE IF ((grid%YLOC_2-grid%YLOC_1) .GE. 6)THEN 
            move_cd_y  = 2 
         ELSE IF ((grid%YLOC_1-grid%YLOC_2) .GE. 6)THEN    
            move_cd_y  = -2 
         ENDIF

         if(wrf_dm_on_monitor()) then
            if(move_cd_x/=0 .or. move_cd_y/=0) then
               write(mess,'("d",I0," moving x",SP,I0," y",I0,SS," to follow vortex")') &
                    grid%id,move_cd_x,move_cd_y
               call wrf_debug(1,trim(mess))
            endif
         endif
      ENDIF vortex_following
   endif can_move

   
   
   nest_safety: IF ( grid%num_nests .GT. 0 .and. ( move_cd_x/=0 .or. move_cd_y/=0 ) ) THEN
     abort=.false.
     nest_loop: do ikid=1,grid%num_nests
        kid=>grid%nests(ikid)%ptr
        inew=kid%i_parent_start-move_cd_x*kid%parent_grid_ratio
        jnew=kid%j_parent_start-move_cd_y*kid%parent_grid_ratio
        
        coral_dist=(kid%ed31+kid%parent_grid_ratio-1)/kid%parent_grid_ratio
        coral_x = max(min_coral_x,grid%coral_x)
        coral_y = max(min_coral_y,grid%coral_y)
        IF(inew <= coral_x)THEN  
           abort=.true.
           write(mess,'("d",I0,": cannot move: nest d",I0," would be too close to -X bdy")') grid%id,kid%id
           call wrf_message(mess)
        ELSEIF((inew+coral_dist) >= grid%ed31 - coral_x) THEN
           abort=.true.
           write(mess,'("d",I0,": cannot move: nest d",I0," would be too close to +X bdy")') grid%id,kid%id
           call wrf_message(mess)
        ENDIF

        coral_dist=(kid%ed32+kid%parent_grid_ratio-1)/kid%parent_grid_ratio
        IF(jnew .le. coral_y)THEN
           abort=.true.
           write(mess,'("d",I0,": cannot move: nest d",I0," would be too close to -Y bdy")') grid%id,kid%id
           call wrf_message(mess)
        ELSEIF((jnew+coral_dist) .ge. grid%ed32 - coral_y) THEN
           abort=.true.
           write(mess,'("d",I0,": cannot move: nest d",I0," would be too close to +Y bdy")') grid%id,kid%id
           call wrf_message(mess)
        ENDIF
     enddo nest_loop
  ENDIF nest_safety

  if(abort) then
     grid%mvnest=.false.
     move_cd_x=0
     move_cd_y=0
     grid%moved=.false.
     direction_of_move2=.false.
     grid%mvnest=.false.
     write(mess,'("d",I0,"; motion has been aborted.")') grid%id
     call wrf_message(mess)
  endif
  
  if(move_cd_x/=0 .or. move_cd_y/=0) then
     direction_of_move2 = .true.
     grid%moved = .true.
     if(grid%vortex_tracker==2) then
        grid%ntime0 = grid%ntsd
     else
        
     endif
  endif
  
  RETURN

END FUNCTION direction_of_move2


LOGICAL FUNCTION direction_of_move ( parent , grid , move_cd_x, move_cd_y )





   USE module_domain
   USE module_configure
   USE module_dm
   IMPLICIT NONE

   TYPE(domain) , POINTER    :: parent, grid, par, nst
   INTEGER, INTENT(OUT)      :: move_cd_x , move_cd_y

   INTEGER     :: corral_dist, kid
   INTEGER     :: dw, de, ds, dn, pgr
   INTEGER     :: would_move_x, would_move_y
   INTEGER     :: cids, cide, cjds, cjde, ckds, ckde, &
                  cims, cime, cjms, cjme, ckms, ckme, &
                  cips, cipe, cjps, cjpe, ckps, ckpe, &
                  nids, nide, njds, njde, nkds, nkde, &
                  nims, nime, njms, njme, nkms, nkme, &
                  nips, nipe, njps, njpe, nkps, nkpe
      INTEGER                          :: IDS,IDE,JDS,JDE,KDS,KDE
      INTEGER                          :: IMS,IME,JMS,JME,KMS,KME
      INTEGER                          :: ITS,ITE,JTS,JTE,KTS,KTE
   character*255 :: message

   INTERFACE
     LOGICAL FUNCTION direction_of_move2 ( parent , nest , dx , dy )
        USE module_domain
        USE module_utility
        TYPE(domain) , POINTER    :: parent , nest
        INTEGER, INTENT(OUT)      :: dx , dy
     END FUNCTION direction_of_move2
     SUBROUTINE G2T2H_new( IIH,JJH,                            & 
                           HBWGT1,HBWGT2,                      & 
                           HBWGT3,HBWGT4,                      &
                           I_PARENT_START,J_PARENT_START,      & 
                           RATIO,                              & 
                           IDS,IDE,JDS,JDE,KDS,KDE,            & 
                           IMS,IME,JMS,JME,KMS,KME,            &
                           ITS,ITE,JTS,JTE,KTS,KTE      )
      IMPLICIT NONE
      INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
      INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
      INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
      INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
      INTEGER,    INTENT(IN   )                            :: RATIO
      REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
      INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIH,JJH
     END SUBROUTINE G2T2H_new
     SUBROUTINE G2T2V_new( IIV,JJV,                            & 
                           VBWGT1,VBWGT2,                      & 
                           VBWGT3,VBWGT4,                      &
                           I_PARENT_START,J_PARENT_START,      & 
                           RATIO,                              & 
                           IDS,IDE,JDS,JDE,KDS,KDE,            & 
                           IMS,IME,JMS,JME,KMS,KME,            &
                           ITS,ITE,JTS,JTE,KTS,KTE      )
      IMPLICIT NONE
      INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
      INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
      INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
      INTEGER,    INTENT(IN   )                            :: I_PARENT_START,J_PARENT_START
      INTEGER,    INTENT(IN   )                            :: RATIO
      REAL,    DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: VBWGT1,VBWGT2,VBWGT3,VBWGT4
      INTEGER, DIMENSION(IMS:IME,JMS:JME),    INTENT(OUT)  :: IIV,JJV
     END SUBROUTINE G2T2V_new
     subroutine init_hnear(iih,jjh,hbwgt1,hbwgt2,hbwgt3,hbwgt4, &
          hnear_i,hnear_j,                     &
          IDS,IDE,JDS,JDE,KDS,KDE,             &
          IMS,IME,JMS,JME,KMS,KME,             &
          ITS,ITE,JTS,JTE,KTS,KTE)
       implicit none
       integer, intent(in) :: ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte, &
            iih(ims:ime,jms:jme), jjh(ims:ime,jms:jme)
       integer, intent(out), dimension(ims:ime,jms:jme) :: hnear_i,hnear_j
       real, dimension(ims:ime,jms:jme), intent(in) :: hbwgt1, hbwgt2, hbwgt3, hbwgt4
     end subroutine init_hnear
   END INTERFACE





   IF   ( grid%num_nests .GT. 1 ) THEN
     CALL wrf_error_fatal3("<stdin>",1094,&
'domains in moving nest simulations can have only 1 nest' )
   ENDIF
   kid = 1
   write(message,*) 'grid%num_nests=',grid%num_nests
   call wrf_debug(5,message)

   direction_of_move = direction_of_move2 ( parent , grid , move_cd_x, move_cd_y )

   if(grid%vortex_tracker == 1) then
      return 
   endif



   IF   ( grid%num_nests .EQ. 0 ) THEN
      

     direction_of_move = ( move_cd_x .NE. 0 ) .OR. ( move_cd_y .NE. 0 )

   ELSE
     
     
     
     move_domain: IF ( direction_of_move ) THEN
        no_nests: IF ( grid%id .EQ. 1 ) THEN

         CALL wrf_message( 'DANGER: Nest has moved too close to boundary of outermost domain.' )
         move_cd_x = 0
         move_cd_y = 0
         direction_of_move = .FALSE.

       ELSE

          CALL get_ijk_from_grid (  grid%nests(kid)%ptr ,               &
               nids, nide, njds, njde, nkds, nkde, &
               nims, nime, njms, njme, nkms, nkme, &
               nips, nipe, njps, njpe, nkps, nkpe  )
          CALL get_ijk_from_grid (  grid ,                              &
               cids, cide, cjds, cjde, ckds, ckde, &
               cims, cime, cjms, cjme, ckms, ckme, &
               cips, cipe, cjps, cjpe, ckps, ckpe  )
          CALL nl_get_parent_grid_ratio ( grid%nests(kid)%ptr%id , pgr )

         
         
         IF(MOD(move_cd_y,2) .NE. 0)THEN
           move_cd_y=move_cd_y+sign(1,move_cd_y)
            WRITE(message,*)'WARNING: move_cd_y REDEFINED FOR THE NMM CORE AND RE-SET TO MASS POINT move_cd_y=',move_cd_y
            call wrf_debug(1,message)
         ENDIF

         CALL wrf_dm_move_nest ( grid , grid%nests(kid)%ptr%intermediate_grid , -move_cd_x*pgr, -move_cd_y*pgr )
         CALL adjust_domain_dims_for_move( grid%nests(kid)%ptr%intermediate_grid , -move_cd_x*pgr, -move_cd_y*pgr )
         grid%nests(kid)%ptr%i_parent_start = grid%nests(kid)%ptr%i_parent_start - move_cd_x*pgr
         write(message,*)'grid%nests(kid)%ptr%i_parent_start =',grid%nests(kid)%ptr%i_parent_start,grid%nests(kid)%ptr%id
         call wrf_debug(1,message)
         CALL nl_set_i_parent_start( grid%nests(kid)%ptr%id, grid%nests(kid)%ptr%i_parent_start )
         grid%nests(kid)%ptr%j_parent_start = grid%nests(kid)%ptr%j_parent_start - move_cd_y*pgr
         write(message,*)'grid%nests(kid)%ptr%j_parent_start =',grid%nests(kid)%ptr%j_parent_start,grid%nests(kid)%ptr%id
         call wrf_debug(1,message)
         CALL nl_set_j_parent_start( grid%nests(kid)%ptr%id, grid%nests(kid)%ptr%j_parent_start )
         IDS = grid%nests(kid)%ptr%sd31
         IDE = grid%nests(kid)%ptr%ed31
         JDS = grid%nests(kid)%ptr%sd32
         JDE = grid%nests(kid)%ptr%ed32
         KDS = grid%nests(kid)%ptr%sd33
         KDE = grid%nests(kid)%ptr%ed33

         IMS = grid%nests(kid)%ptr%sm31
         IME = grid%nests(kid)%ptr%em31
         JMS = grid%nests(kid)%ptr%sm32
         JME = grid%nests(kid)%ptr%em32
         KMS = grid%nests(kid)%ptr%sm33
         KME = grid%nests(kid)%ptr%em33

         ITS  = grid%nests(kid)%ptr%sp31
         ITE  = grid%nests(kid)%ptr%ep31
         JTS  = grid%nests(kid)%ptr%sp32
         JTE  = grid%nests(kid)%ptr%ep32
         KTS  = grid%nests(kid)%ptr%sp33
         KTE  = grid%nests(kid)%ptr%ep33

         CALL G2T2H_new(    grid%nests(kid)%ptr%IIH,grid%nests(kid)%ptr%JJH,                            & 
                       grid%nests(kid)%ptr%HBWGT1,grid%nests(kid)%ptr%HBWGT2,                      & 
                       grid%nests(kid)%ptr%HBWGT3,grid%nests(kid)%ptr%HBWGT4,                      &
                       grid%nests(kid)%ptr%I_PARENT_START,grid%nests(kid)%ptr%J_PARENT_START,      & 
                       3,                              & 
                       IDS,IDE,JDS,JDE,KDS,KDE,            & 
                       IMS,IME,JMS,JME,KMS,KME,            &
                       ITS,ITE,JTS,JTE,KTS,KTE      )
         CALL G2T2V_new(    grid%nests(kid)%ptr%IIV,grid%nests(kid)%ptr%JJV,                            & 
                       grid%nests(kid)%ptr%VBWGT1,grid%nests(kid)%ptr%VBWGT2,                      & 
                       grid%nests(kid)%ptr%VBWGT3,grid%nests(kid)%ptr%VBWGT4,                      &
                       grid%nests(kid)%ptr%I_PARENT_START,grid%nests(kid)%ptr%J_PARENT_START,      & 
                       3,                              & 
                       IDS,IDE,JDS,JDE,KDS,KDE,            & 
                       IMS,IME,JMS,JME,KMS,KME,            &
                       ITS,ITE,JTS,JTE,KTS,KTE      )

         CALL init_hnear(    grid%nests(kid)%ptr%IIH,grid%nests(kid)%ptr%JJH,                            & 
                       grid%nests(kid)%ptr%HBWGT1,grid%nests(kid)%ptr%HBWGT2,                      & 
                       grid%nests(kid)%ptr%HBWGT3,grid%nests(kid)%ptr%HBWGT4,                      &
                       grid%nests(kid)%ptr%hnear_i,grid%nests(kid)%ptr%hnear_j,                    &
                       IDS,IDE,JDS,JDE,KDS,KDE,            & 
                       IMS,IME,JMS,JME,KMS,KME,            &
                       ITS,ITE,JTS,JTE,KTS,KTE      )

       ENDIF no_nests
       if(grid%vortex_tracker == 6 .or. grid%vortex_tracker == 7) then
            
            
            call nmm_med_tracker_post_move(grid)
       endif

       if(grid%swath_mode==1) then
          
          grid%update_interest=.true.
          
          
          
          if(parent%interest_kids/=0) then
38           format('grid ',I2,' updating grid ',I2,' area of interest due to nest motion')
             write(message,38) grid%id,parent%id
             call wrf_debug(1,trim(message))
             parent%update_interest=.true.
          else
39           format('grid ',I2,' not updating grid ',I2,' area of interest because interest_kids is 0')
             write(message,39) grid%id,parent%id
             call wrf_debug(1,trim(message))
          endif
       endif
     ENDIF move_domain

   ENDIF

   RETURN
END FUNCTION direction_of_move
