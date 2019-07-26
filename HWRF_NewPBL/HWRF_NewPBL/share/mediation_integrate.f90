




SUBROUTINE med_calc_model_time ( grid , config_flags )
  
   USE module_domain	, ONLY : domain, domain_clock_get
   USE module_configure	, ONLY : grid_config_rec_type
  
   USE module_date_time

   IMPLICIT NONE

  
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  
   REAL                                       :: time 






END SUBROUTINE med_calc_model_time

SUBROUTINE med_before_solve_io ( grid , config_flags )
  
   USE module_state_description
   USE module_domain	, ONLY : domain, domain_clock_get
   USE module_configure	, ONLY : grid_config_rec_type
   USE module_streams
  
   USE module_utility

   IMPLICIT NONE

  
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  
   INTEGER                                    :: ialarm
   INTEGER                                    :: rc
   TYPE(WRFU_Time) :: currTime, startTime

   INTEGER :: hr, min, sec, ms,julyr,julday
   REAL :: GMT


   CHARACTER*256          :: message


 CALL WRFU_ClockGet( grid%domain_clock, CurrTime=currTime, StartTime=startTime )
 
 IF( WRFU_AlarmIsRinging( grid%alarms( HISTORY_ALARM ), rc=rc ) .AND. &
       (grid%dfi_write_dfi_history .OR. grid%dfi_stage == DFI_FST .OR. grid%dfi_opt == DFI_NODFI) ) THEN
     IF       ( ( config_flags%restart )                    .AND. &
                ( config_flags%write_hist_at_0h_rst )       .AND. &
                ( currTime .EQ. startTime )                       ) THEN






       
       CALL med_hist_out ( grid , HISTORY_ALARM, config_flags )
      
     ELSE IF  ( ( config_flags%restart )                    .AND. &
                ( .NOT. config_flags%write_hist_at_0h_rst ) .AND. &
                ( currTime .EQ. startTime )                       ) THEN
       
     ELSE
       CALL med_hist_out ( grid , HISTORY_ALARM, config_flags )
       
     END IF
     CALL WRFU_AlarmRingerOff( grid%alarms( HISTORY_ALARM ), rc=rc )
   ELSE IF  ( (config_flags%restart) .AND. ( currTime .EQ. startTime ) .AND. &
              ( config_flags%write_hist_at_0h_rst ) ) THEN
     
     CALL med_hist_out ( grid , HISTORY_ALARM, config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( HISTORY_ALARM ), rc=rc )
   ENDIF

   IF( WRFU_AlarmIsRinging( grid%alarms( INPUTOUT_ALARM ), rc=rc ) ) THEN
     CALL med_filter_out  ( grid , config_flags )
     CALL WRFU_AlarmRingerOff( grid%alarms( INPUTOUT_ALARM ), rc=rc )
   ENDIF

   DO ialarm = first_auxhist, last_auxhist
     IF ( .FALSE.) THEN
       rc = 1  
     ELSE IF( WRFU_AlarmIsRinging( grid%alarms( ialarm ), rc=rc ) ) THEN
       IF ( grid%dfi_stage == DFI_FST .OR. grid%dfi_opt == DFI_NODFI ) THEN
          CALL med_hist_out ( grid , ialarm, config_flags )
       END IF
       CALL WRFU_AlarmRingerOff( grid%alarms( ialarm ), rc=rc )
     ENDIF
   ENDDO

   DO ialarm = first_auxinput, last_auxinput
     IF ( .FALSE.) THEN
       rc = 1  
     ELSE IF( WRFU_AlarmIsRinging( grid%alarms( ialarm ), rc=rc ) ) THEN
       CALL med_auxinput_in ( grid, ialarm, config_flags )
       WRITE ( message , FMT='(A,i3,A,i3)' )  'Input data processed for aux input ' , &
          ialarm - first_auxinput + 1, ' for domain ',grid%id
       CALL wrf_debug ( 0 , message )
       CALL WRFU_AlarmRingerOff( grid%alarms( ialarm ), rc=rc )
     ENDIF
   ENDDO


   CALL WRFU_ClockGet( grid%domain_clock, CurrTime=currTime, StartTime=startTime )
   IF ( ( WRFU_AlarmIsRinging( grid%alarms( RESTART_ALARM ), rc=rc ) ) .AND. &
        ( currTime .NE. startTime ) ) THEN

     CALL domain_clock_get( grid, current_time=CurrTime )
     CALL WRFU_TimeGet( CurrTime, YY=julyr, dayOfYear=julday, H=hr, M=min, S=sec, MS=ms, rc=rc)
     gmt=hr+real(min)/60.+real(sec)/3600.+real(ms)/(1000*3600)
      if (grid%id .eq. 2) call med_namelist_out ( grid , config_flags )

     IF ( grid%id .EQ. 1 ) THEN
       
       
       
       CALL med_restart_out ( grid , config_flags )
     ENDIF
     CALL WRFU_AlarmRingerOff( grid%alarms( RESTART_ALARM ), rc=rc )
   ELSE
     CALL WRFU_AlarmRingerOff( grid%alarms( RESTART_ALARM ), rc=rc )
   ENDIF


   CALL med_latbound_in ( grid , config_flags )

   RETURN
END SUBROUTINE med_before_solve_io

SUBROUTINE med_after_solve_io ( grid , config_flags )
  
   USE module_domain	, ONLY : domain
   USE module_timing
   USE module_configure	, ONLY : grid_config_rec_type
  

   IMPLICIT NONE

  
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

   
   CALL calc_ts(grid)

   
   CALL track_driver(grid)

   RETURN
END SUBROUTINE med_after_solve_io

SUBROUTINE med_pre_nest_initial ( parent , newid , config_flags )
  
   USE module_domain	, ONLY : domain, domain_clock_get
   USE module_utility   , ONLY : WRFU_Time, WRFU_TimeEQ
   USE module_timing
   USE module_io_domain
   USE module_configure	, ONLY : grid_config_rec_type
  

   IMPLICIT NONE

  
   TYPE(domain) , POINTER                      :: parent
   INTEGER, INTENT(IN)                         :: newid
   TYPE (grid_config_rec_type) , INTENT(INOUT) :: config_flags
   TYPE (grid_config_rec_type)                 :: nest_config_flags

  
   INTEGER                :: itmp, fid, ierr, icnt
   CHARACTER*256          :: rstname, message, timestr

   TYPE(WRFU_Time)        :: strt_time, cur_time


   CALL domain_clock_get( parent, current_timestr=timestr, start_time=strt_time, current_time=cur_time )
   CALL construct_filename2a ( rstname , config_flags%rst_inname , newid , 2 , timestr )

    IF ( config_flags%restart .AND. WRFU_TimeEQ(cur_time,strt_time) ) THEN
     WRITE(message,*)'RESTART: nest, opening ',TRIM(rstname),' for reading header information only'
     CALL wrf_message ( message )
  
  
  
     CALL open_r_dataset ( fid , TRIM(rstname) , parent , config_flags , "DATASET=RESTART", ierr )
     IF ( ierr .NE. 0 ) THEN
       WRITE( message , '("program wrf: error opening ",A32," for reading")') TRIM(rstname)
       CALL wrf_error_fatal3("mediation_integrate.b",270,&
message )
     ENDIF

  
     CALL wrf_get_dom_ti_integer ( fid , 'I_PARENT_START' ,  itmp  , 1 , icnt, ierr )
     IF ( ierr .EQ. 0 ) THEN
       config_flags%i_parent_start = itmp
       CALL nl_set_i_parent_start ( newid , config_flags%i_parent_start )
     ENDIF
     CALL wrf_get_dom_ti_integer ( fid , 'J_PARENT_START' ,  itmp  , 1 , icnt, ierr )
     IF ( ierr .EQ. 0 ) THEN
       config_flags%j_parent_start = itmp
       CALL nl_set_j_parent_start ( newid , config_flags%j_parent_start )
     ENDIF

     CALL close_dataset ( fid , config_flags , "DATASET=RESTART" )
   ENDIF

END SUBROUTINE med_pre_nest_initial


SUBROUTINE med_nest_initial ( parent , nest , config_flags )
  
   USE module_domain	, ONLY : domain , domain_clock_get , get_ijk_from_grid
   USE module_timing
   USE module_io_domain
   USE module_configure	, ONLY : grid_config_rec_type
   USE module_utility
  

   IMPLICIT NONE

  
   TYPE(domain) , POINTER                     :: parent, nest
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   TYPE (grid_config_rec_type)                :: nest_config_flags

  
   LOGICAL, EXTERNAL      :: wrf_dm_on_monitor
   TYPE(WRFU_Time)        :: strt_time, cur_time
   CHARACTER * 80         :: rstname , timestr
   CHARACTER * 256        :: message
   INTEGER                :: fid
   INTEGER                :: ierr
   INTEGER                :: i , j, rc
   INTEGER                :: ids , ide , jds , jde , kds , kde , &
                             ims , ime , jms , jme , kms , kme , &
                             ips , ipe , jps , jpe , kps , kpe






   INTERFACE

     SUBROUTINE med_nest_egrid_configure ( parent , nest )
        USE module_domain	, ONLY : domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_nest_egrid_configure 

     SUBROUTINE med_construct_egrid_weights ( parent , nest )
        USE module_domain	, ONLY : domain
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
       USE module_domain	, ONLY : domain
       USE module_configure	, ONLY : grid_config_rec_type
       TYPE(domain) , POINTER                        :: nest
       TYPE(grid_config_rec_type) , INTENT(IN)       :: config_flags
     END SUBROUTINE NEST_TERRAIN

    SUBROUTINE med_interp_domain ( parent , nest )
        USE module_domain	, ONLY : domain
        TYPE(domain) , POINTER                 :: parent , nest
    END SUBROUTINE med_interp_domain

    SUBROUTINE med_init_domain_constants_nmm ( parent, nest )
        USE module_domain	, ONLY : domain
        TYPE(domain) , POINTER                    :: parent , nest
    END SUBROUTINE med_init_domain_constants_nmm

    SUBROUTINE start_domain ( grid , allowed_to_move )
        USE module_domain	, ONLY : domain
        TYPE(domain) :: grid
        LOGICAL, INTENT(IN) :: allowed_to_move
    END SUBROUTINE start_domain

   END INTERFACE


   if (config_flags%restart .or. nest%analysis) then
   nest%first_force = .true.
   else
   nest%first_force = .false.
   endif


   

   
   
   

  IF(.not. nest%analysis .and. .not. config_flags%restart)THEN    





   CALL med_nest_egrid_configure ( parent , nest )





    CALL med_construct_egrid_weights ( parent, nest )











    IDS = parent%sd31
    IDE = parent%ed31
    JDS = parent%sd32
    JDE = parent%ed32
    KDS = parent%sd33
    KDE = parent%ed33

    IMS = parent%sm31
    IME = parent%em31
    JMS = parent%sm32
    JME = parent%em32
    KMS = parent%sm33
    KME = parent%em33

    IPS = parent%sp31
    IPE = parent%ep31
    JPS = parent%sp32
    JPE = parent%ep32
    KPS = parent%sp33
    KPE = parent%ep33

    
    
    
    
    
    
    
    





    IDS = nest%sd31
    IDE = nest%ed31
    JDS = nest%sd32
    JDE = nest%ed32
    KDS = nest%sd33
    KDE = nest%ed33

    IMS = nest%sm31
    IME = nest%em31
    JMS = nest%sm32
    JME = nest%em32
    KMS = nest%sm33
    KME = nest%em33

    IPS = nest%sp31
    IPE = nest%ep31
    JPS = nest%sp32
    JPE = nest%ep32
    KPS = nest%sp33
    KPE = nest%ep33


    CALL NEST_TERRAIN ( nest, config_flags )



    nest%PSTD=parent%PSTD
    nest%KZMAX=KME
    parent%KZMAX=KME  

    DO J = JPS, MIN(JPE,JDE-1)
      DO I = IPS, MIN(IPE,IDE-1)
       nest%fis(I,J)=nest%hres_fis(I,J)
     ENDDO
    ENDDO







    nest%imask_nostag = 0 
    nest%imask_xstag  = 0 
    nest%imask_ystag  = 0 
    nest%imask_xystag = 0 

   CALL med_interp_domain( parent, nest )




    CALL med_init_domain_constants_nmm ( parent, nest )





    CALL start_domain ( nest, .TRUE.)




    CALL nl_set_isice ( nest%id , config_flags%isice )   
    CALL nl_set_isoilwater ( nest%id , config_flags%isoilwater )   
    CALL nl_set_isurban ( nest%id , config_flags%isurban )   
    CALL nl_set_gmt    ( nest%id , config_flags%gmt    )   
    CALL nl_set_julyr (nest%id, config_flags%julyr)       
    CALL nl_set_julday ( nest%id , config_flags%julday )

     CALL model_to_grid_config_rec ( nest%id , model_config_rec , nest_config_flags )
    CALL med_analysis_out ( nest, nest_config_flags )

   ELSE






  IF( nest%analysis .and. .not. config_flags%restart)THEN
   CALL med_analysis_in ( nest, config_flags )
  ELSE IF (config_flags%restart)THEN
   CALL med_restart_in ( nest, config_flags )
  ENDIF






   CALL med_nest_egrid_configure ( parent , nest )





   CALL med_construct_egrid_weights ( parent, nest )

   nest%imask_nostag = 0
   nest%imask_xstag  = 0
   nest%imask_ystag  = 0
   nest%imask_xystag = 0





    CALL med_init_domain_constants_nmm ( parent, nest )







    CALL start_domain ( nest, .TRUE.)

    nest%analysis=.FALSE.
    CALL nl_set_analysis( nest%id, nest%analysis)

  ENDIF





  RETURN
END SUBROUTINE med_nest_initial

SUBROUTINE init_domain_constants ( parent , nest )
   USE module_domain	, ONLY : domain
   IMPLICIT NONE
   TYPE(domain) :: parent , nest
END SUBROUTINE init_domain_constants


SUBROUTINE med_nest_force ( parent , nest )
  
   USE module_domain	, ONLY : domain
   USE module_timing
   USE module_configure	, ONLY : grid_config_rec_type
  
  
   USE module_utility

   IMPLICIT NONE

  
   TYPE(domain) , POINTER                     :: parent, nest
  
   INTEGER                                    :: idum1 , idum2 , fid, rc

   INTEGER                  :: IDS,IDE,JDS,JDE,KDS,KDE     
   INTEGER                  :: IMS,IME,JMS,JME,KMS,KME
   INTEGER                  :: ITS,ITE,JTS,JTE,KTS,KTE

   INTERFACE
     SUBROUTINE med_force_domain ( parent , nest )
        USE module_domain	, ONLY : domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_force_domain
     SUBROUTINE med_interp_domain ( parent , nest )
        USE module_domain	, ONLY : domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_interp_domain




     SUBROUTINE BASE_STATE_PARENT ( Z3d,Q3d,T3d,PSTD,        &
                                    PINT,T,Q,CWM,            &
                                    FIS,QSH,PD,PDTOP,PTOP,   &
                                    ETA1,ETA2,               &
                                    DETA1,DETA2,             &
                                    IDS,IDE,JDS,JDE,KDS,KDE, &
                                    IMS,IME,JMS,JME,KMS,KME, &
                                    ITS,ITE,JTS,JTE,KTS,KTE  )


         USE MODULE_MODEL_CONSTANTS
         IMPLICIT NONE
         INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
         INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
         INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
         REAL,       INTENT(IN   )                            :: PDTOP,PTOP
         REAL, DIMENSION(KMS:KME),                 INTENT(IN) :: ETA1,ETA2,DETA1,DETA2
         REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(IN) :: FIS,PD,QSH
         REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME), INTENT(IN) :: PINT,T,Q,CWM
         REAL, DIMENSION(KMS:KME)                , INTENT(OUT):: PSTD
         REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME), INTENT(OUT):: Z3d,Q3d,T3d

     END SUBROUTINE BASE_STATE_PARENT

   END INTERFACE




    IDS = parent%sd31
    IDE = parent%ed31
    JDS = parent%sd32
    JDE = parent%ed32
    KDS = parent%sd33
    KDE = parent%ed33

    IMS = parent%sm31
    IME = parent%em31
    JMS = parent%sm32
    JME = parent%em32
    KMS = parent%sm33
    KME = parent%em33

    ITS = parent%sp31
    ITE = parent%ep31
    JTS = parent%sp32
    JTE = parent%ep32
    KTS = parent%sp33
    KTE = parent%ep33


    
    
    
    
    
    
    
    


   IF ( .NOT. WRFU_ClockIsStopTime(nest%domain_clock ,rc=rc) ) THEN

     nest%imask_nostag = 1
     nest%imask_xstag = 1
     nest%imask_ystag = 1
     nest%imask_xystag = 1
     CALL med_force_domain( parent, nest )
   ENDIF



   RETURN
END SUBROUTINE med_nest_force

SUBROUTINE med_nest_feedback ( parent , nest , config_flags )
  
   USE module_domain	, ONLY : domain , get_ijk_from_grid
   USE module_timing
   USE module_configure	, ONLY : grid_config_rec_type
  
  
   USE module_utility
   IMPLICIT NONE


  
   TYPE(domain) , POINTER                     :: parent, nest
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  
   INTEGER                                    :: idum1 , idum2 , fid, rc
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   INTEGER i,j

   INTERFACE
     SUBROUTINE med_feedback_domain ( parent , nest )
        USE module_domain	, ONLY : domain
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_feedback_domain
   END INTERFACE


    IF ( config_flags%feedback .NE. 0 ) THEN
      CALL med_feedback_domain( parent, nest )
      CALL get_ijk_from_grid (  parent ,                         &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )

    END IF

   RETURN
END SUBROUTINE med_nest_feedback

SUBROUTINE med_last_solve_io ( grid , config_flags )
  
   USE module_state_description
   USE module_domain	, ONLY : domain, domain_clock_get
   USE module_configure	, ONLY : grid_config_rec_type
   USE module_utility
   USE module_streams
  

   IMPLICIT NONE

  
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  
   INTEGER                                    :: rc

   TYPE(WRFU_Time) :: CurrTime  
   INTEGER :: hr, min, sec, ms,julyr,julday
   REAL :: GMT



   IF( WRFU_AlarmIsRinging( grid%alarms( HISTORY_ALARM ), rc=rc ) .AND. &
       (grid%dfi_write_dfi_history .OR. grid%dfi_stage == DFI_FST .OR. grid%dfi_opt == DFI_NODFI) ) THEN



     CALL med_hist_out ( grid , HISTORY_ALARM , config_flags )
   ENDIF

   IF( WRFU_AlarmIsRinging( grid%alarms( INPUTOUT_ALARM ), rc=rc ) ) THEN
     CALL med_filter_out  ( grid , config_flags )
   ENDIF





   IF ( grid%dfi_stage == DFI_FST .OR. grid%dfi_opt == DFI_NODFI ) THEN
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/med_last_solve_io.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST1_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST1_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST2_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST2_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST3_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST3_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST4_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST4_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST5_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST5_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST6_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST6_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST7_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST7_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST8_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST8_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST9_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST9_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST10_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST10_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST11_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST11_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST12_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST12_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST13_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST13_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST14_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST14_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST15_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST15_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST16_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST16_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST17_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST17_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST18_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST18_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST19_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST19_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST20_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST20_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST21_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST21_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST22_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST22_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST23_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST23_ALARM , config_flags )
 ENDIF
 IF( WRFU_AlarmIsRinging( grid%alarms( AUXHIST24_ALARM ), rc=rc ) ) THEN
   CALL med_hist_out ( grid , AUXHIST24_ALARM , config_flags )
 ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
   END IF


   IF( WRFU_AlarmIsRinging( grid%alarms( RESTART_ALARM ), rc=rc ) ) THEN


     CALL domain_clock_get( grid, current_time=CurrTime )
     CALL WRFU_TimeGet( CurrTime, YY=julyr, dayOfYear=julday, H=hr, M=min, S=sec, MS=ms, rc=rc)
     gmt=hr+real(min)/60.+real(sec)/3600.+real(ms)/(1000*3600)
      if (grid%id .eq. 2) call med_namelist_out ( grid , config_flags )

     IF ( grid%id .EQ. 1 ) THEN
       CALL med_restart_out ( grid , config_flags )
     ENDIF
   ENDIF

   
   CALL write_ts( grid )

   RETURN
END SUBROUTINE med_last_solve_io









SUBROUTINE med_analysis_in ( grid , config_flags )
  
   USE module_domain	, ONLY : domain, domain_clock_get
   USE module_io_domain
   USE module_timing
  
   USE module_configure	, ONLY : grid_config_rec_type
   USE module_bc_time_utilities


   IMPLICIT NONE

  
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*80                           :: rstname , outname
   INTEGER                                :: fid , rid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc

   TYPE(WRFU_Time)                        :: CurrTime
   CHARACTER*80                           :: timestr

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

   rid=grid%id



   CALL domain_clock_get( grid, current_timestr=timestr )
   CALL construct_filename2a ( rstname ,config_flags%anl_outname, grid%id , 2 , timestr )

   WRITE( message , '("med_analysis_in: opening ",A," for reading")' ) TRIM ( rstname )
   CALL wrf_debug( 1 , message )
   CALL open_r_dataset ( rid, TRIM(rstname), grid , &
                         config_flags , "DATASET=RESTART", ierr )

   IF ( ierr .NE. 0 ) THEN
      

      write(message,'(A,I0,A,A,A)') 'ERROR: Domain ',grid%id,' analysis file ',trim(rstname),' is missing.'
      call wrf_error_fatal3("mediation_integrate.b",1313,&
message)

      
      
      
      

      
      write(message,'(A,I0,A)') '-------> Domain ',grid%id,' running as a cold start (interp from parent).'
      call wrf_message(message)

      if(wrf_dm_on_monitor()) then
         WRITE ( message , FMT = '("Failing to read restart for domain ",I8)' ) grid%id
         CALL end_timing ( TRIM(message) )
      endif

      return
   ELSE
      

      CALL input_restart ( rid, grid , config_flags , ierr )
      IF ( wrf_dm_on_monitor() ) THEN
         WRITE ( message , FMT = '("Reading restart for domain ",I8)' ) grid%id
         CALL end_timing ( TRIM(message) )
      END IF
      CALL close_dataset ( rid , config_flags , "DATASET=RESTART" )
   ENDIF

   RETURN

END SUBROUTINE med_analysis_in


SUBROUTINE med_analysis_out ( grid , config_flags )
  
   USE module_domain	, ONLY : domain, domain_clock_get
   USE module_io_domain
   USE module_timing
  
   USE module_configure	, ONLY : grid_config_rec_type
   USE module_bc_time_utilities


   IMPLICIT NONE

  
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*80                           :: rstname , outname
   INTEGER                                :: fid , rid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc

   TYPE(WRFU_Time)                        :: CurrTime
   CHARACTER*80                           :: timestr

   if(.not. config_flags%write_analysis) then
      write(message,'("Writing of an analysis file is disabled for domain ",I0," because write_analysis=F")') grid%id
      call wrf_debug(1,message)
      return
   endif

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

   rid=grid%id



   CALL domain_clock_get( grid, current_timestr=timestr )
   CALL construct_filename2a ( rstname ,config_flags%anl_outname, grid%id , 2 , timestr )

   WRITE( message , '("med_analysis_out: opening ",A," for writing")' ) TRIM ( rstname )
   CALL wrf_debug( 1 , message )
   CALL open_w_dataset ( rid, TRIM(rstname), grid , &
                         config_flags , output_restart , "DATASET=RESTART", ierr )

   IF ( ierr .NE. 0 ) THEN
     CALL WRF_message( message )
   ENDIF
   CALL output_restart ( rid, grid , config_flags , ierr )
   IF ( wrf_dm_on_monitor() ) THEN
     WRITE ( message , FMT = '("Writing restart for domain ",I8)' ) grid%id
     CALL end_timing ( TRIM(message) )
   END IF
   CALL close_dataset ( rid , config_flags , "DATASET=RESTART" )
   RETURN
END SUBROUTINE med_analysis_out


RECURSIVE SUBROUTINE med_restart_out ( grid , config_flags )
  
   USE module_domain	, ONLY : domain , domain_clock_get
   USE module_io_domain
   USE module_timing
   USE module_configure	, ONLY : grid_config_rec_type
  

   USE module_utility

   IMPLICIT NONE

  
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*80                           :: rstname , outname
   INTEGER                                :: fid , rid, kid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc
   CHARACTER*80                           :: timestr
   TYPE (grid_config_rec_type)            :: kid_config_flags

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF




   

   CALL domain_clock_get( grid, current_timestr=timestr )
   CALL construct_filename2a ( rstname , config_flags%rst_outname , grid%id , 2 , timestr )

   WRITE( message , '("med_restart_out: opening ",A," for writing")' ) TRIM ( rstname )
   CALL wrf_debug( 1 , message )
   CALL open_w_dataset ( rid, TRIM(rstname), grid , &
                         config_flags , output_restart , "DATASET=RESTART", ierr )

   IF ( ierr .NE. 0 ) THEN
     CALL WRF_message( message )
   ENDIF
   CALL output_restart ( rid, grid , config_flags , ierr )
   IF ( wrf_dm_on_monitor() ) THEN
     WRITE ( message , FMT = '("Writing restart for domain ",I8)' ) grid%id
     CALL end_timing ( TRIM(message) )
   END IF
   CALL close_dataset ( rid , config_flags , "DATASET=RESTART" )

   
   DO kid = 1, max_nests
      IF ( ASSOCIATED( grid%nests(kid)%ptr ) ) THEN
        CALL model_to_grid_config_rec ( grid%nests(kid)%ptr%id , model_config_rec , kid_config_flags )
        CALL med_restart_out ( grid%nests(kid)%ptr , kid_config_flags ) 
      ENDIF
   ENDDO

   RETURN
END SUBROUTINE med_restart_out





SUBROUTINE med_restart_in ( grid , config_flags )
  
   USE module_domain	, ONLY : domain, domain_clock_get
   USE module_io_domain
   USE module_timing
  
   USE module_configure	, ONLY : grid_config_rec_type
   USE module_bc_time_utilities

   IMPLICIT NONE

  
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*80                           :: rstname , outname
   INTEGER                                :: fid , rid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc

   TYPE(WRFU_Time)                        :: CurrTime
   CHARACTER*80                           :: timestr

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

   rid=grid%id



   CALL domain_clock_get( grid, current_timestr=timestr )
   CALL construct_filename2a ( rstname ,config_flags%rst_outname, grid%id , 2 , timestr )

   WRITE( message , '("med_restart_in: opening ",A," for reading")' ) TRIM ( rstname )
   CALL wrf_debug( 1 , message )
   CALL open_r_dataset ( rid, TRIM(rstname), grid , &
                         config_flags , "DATASET=RESTART", ierr )

   IF ( ierr .NE. 0 ) THEN

     CALL wrf_error_fatal3("mediation_integrate.b",1573,&
'NESTED DOMAIN ERROR: FOR ANALYSIS SET TO TRUE, YOU NEED wrfanal FILE')
   ENDIF
   CALL input_restart ( rid, grid , config_flags , ierr )
   IF ( wrf_dm_on_monitor() ) THEN
     WRITE ( message , FMT = '("Reading restart for domain ",I8)' ) grid%id
     CALL end_timing ( TRIM(message) )
   END IF
   CALL close_dataset ( rid , config_flags , "DATASET=RESTART" )
   RETURN

END SUBROUTINE med_restart_in


SUBROUTINE med_hist_out ( grid , stream, config_flags )
  
   USE module_domain	, ONLY : domain
   USE module_timing
   USE module_io_domain
   USE module_configure	, ONLY : grid_config_rec_type

   USE module_utility

   IMPLICIT NONE
  
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   INTEGER , INTENT(IN)                       :: stream
  
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*80                           :: fname, n2
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

   IF ( stream .LT. first_history .OR. stream .GT. last_auxhist ) THEN
     WRITE(message,*)'med_hist_out: invalid history stream ',stream
     CALL wrf_error_fatal3("mediation_integrate.b",1614,&
message )
   ENDIF

       
       
       
      if ( (stream==HISTORY_ALARM .or. stream==AUXHIST2_ALARM) .and. &
           WRFU_AlarmIsRinging( grid%alarms(AUXHIST3_ALARM) ) ) then
         CALL WRFU_AlarmRingerOff(grid%alarms(AUXHIST3_ALARM))
      endif
      if ( stream==HISTORY_ALARM .and. &
           WRFU_AlarmIsRinging( grid%alarms(AUXHIST2_ALARM) ) ) then
         CALL WRFU_AlarmRingerOff(grid%alarms(AUXHIST2_ALARM))
      endif

   SELECT CASE( stream )
     CASE ( HISTORY_ALARM )
       CALL open_hist_w( grid, config_flags, stream, HISTORY_ALARM, &
                         config_flags%history_outname, grid%oid,    &
                         output_history, fname, n2, ierr )
       CALL output_history ( grid%oid, grid , config_flags , ierr )


!STARTOFREGISTRYGENERATEDINCLUDE 'inc/med_hist_out_opens.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
 CASE ( AUXHIST1_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST1_ALARM,       &
                     config_flags%auxhist1_outname, grid%auxhist1_oid, &
                     output_auxhist1, fname, n2, ierr )
   CALL output_auxhist1 ( grid%auxhist1_oid, grid , config_flags , ierr )
 CASE ( AUXHIST2_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST2_ALARM,       &
                     config_flags%auxhist2_outname, grid%auxhist2_oid, &
                     output_auxhist2, fname, n2, ierr )
   CALL output_auxhist2 ( grid%auxhist2_oid, grid , config_flags , ierr )
 CASE ( AUXHIST3_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST3_ALARM,       &
                     config_flags%auxhist3_outname, grid%auxhist3_oid, &
                     output_auxhist3, fname, n2, ierr )
   CALL output_auxhist3 ( grid%auxhist3_oid, grid , config_flags , ierr )
 CASE ( AUXHIST4_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST4_ALARM,       &
                     config_flags%auxhist4_outname, grid%auxhist4_oid, &
                     output_auxhist4, fname, n2, ierr )
   CALL output_auxhist4 ( grid%auxhist4_oid, grid , config_flags , ierr )
 CASE ( AUXHIST5_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST5_ALARM,       &
                     config_flags%auxhist5_outname, grid%auxhist5_oid, &
                     output_auxhist5, fname, n2, ierr )
   CALL output_auxhist5 ( grid%auxhist5_oid, grid , config_flags , ierr )
 CASE ( AUXHIST6_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST6_ALARM,       &
                     config_flags%auxhist6_outname, grid%auxhist6_oid, &
                     output_auxhist6, fname, n2, ierr )
   CALL output_auxhist6 ( grid%auxhist6_oid, grid , config_flags , ierr )
 CASE ( AUXHIST7_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST7_ALARM,       &
                     config_flags%auxhist7_outname, grid%auxhist7_oid, &
                     output_auxhist7, fname, n2, ierr )
   CALL output_auxhist7 ( grid%auxhist7_oid, grid , config_flags , ierr )
 CASE ( AUXHIST8_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST8_ALARM,       &
                     config_flags%auxhist8_outname, grid%auxhist8_oid, &
                     output_auxhist8, fname, n2, ierr )
   CALL output_auxhist8 ( grid%auxhist8_oid, grid , config_flags , ierr )
 CASE ( AUXHIST9_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST9_ALARM,       &
                     config_flags%auxhist9_outname, grid%auxhist9_oid, &
                     output_auxhist9, fname, n2, ierr )
   CALL output_auxhist9 ( grid%auxhist9_oid, grid , config_flags , ierr )
 CASE ( AUXHIST10_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST10_ALARM,       &
                     config_flags%auxhist10_outname, grid%auxhist10_oid, &
                     output_auxhist10, fname, n2, ierr )
   CALL output_auxhist10 ( grid%auxhist10_oid, grid , config_flags , ierr )
 CASE ( AUXHIST11_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST11_ALARM,       &
                     config_flags%auxhist11_outname, grid%auxhist11_oid, &
                     output_auxhist11, fname, n2, ierr )
   CALL output_auxhist11 ( grid%auxhist11_oid, grid , config_flags , ierr )
 CASE ( AUXHIST12_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST12_ALARM,       &
                     config_flags%auxhist12_outname, grid%auxhist12_oid, &
                     output_auxhist12, fname, n2, ierr )
   CALL output_auxhist12 ( grid%auxhist12_oid, grid , config_flags , ierr )
 CASE ( AUXHIST13_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST13_ALARM,       &
                     config_flags%auxhist13_outname, grid%auxhist13_oid, &
                     output_auxhist13, fname, n2, ierr )
   CALL output_auxhist13 ( grid%auxhist13_oid, grid , config_flags , ierr )
 CASE ( AUXHIST14_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST14_ALARM,       &
                     config_flags%auxhist14_outname, grid%auxhist14_oid, &
                     output_auxhist14, fname, n2, ierr )
   CALL output_auxhist14 ( grid%auxhist14_oid, grid , config_flags , ierr )
 CASE ( AUXHIST15_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST15_ALARM,       &
                     config_flags%auxhist15_outname, grid%auxhist15_oid, &
                     output_auxhist15, fname, n2, ierr )
   CALL output_auxhist15 ( grid%auxhist15_oid, grid , config_flags , ierr )
 CASE ( AUXHIST16_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST16_ALARM,       &
                     config_flags%auxhist16_outname, grid%auxhist16_oid, &
                     output_auxhist16, fname, n2, ierr )
   CALL output_auxhist16 ( grid%auxhist16_oid, grid , config_flags , ierr )
 CASE ( AUXHIST17_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST17_ALARM,       &
                     config_flags%auxhist17_outname, grid%auxhist17_oid, &
                     output_auxhist17, fname, n2, ierr )
   CALL output_auxhist17 ( grid%auxhist17_oid, grid , config_flags , ierr )
 CASE ( AUXHIST18_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST18_ALARM,       &
                     config_flags%auxhist18_outname, grid%auxhist18_oid, &
                     output_auxhist18, fname, n2, ierr )
   CALL output_auxhist18 ( grid%auxhist18_oid, grid , config_flags , ierr )
 CASE ( AUXHIST19_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST19_ALARM,       &
                     config_flags%auxhist19_outname, grid%auxhist19_oid, &
                     output_auxhist19, fname, n2, ierr )
   CALL output_auxhist19 ( grid%auxhist19_oid, grid , config_flags , ierr )
 CASE ( AUXHIST20_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST20_ALARM,       &
                     config_flags%auxhist20_outname, grid%auxhist20_oid, &
                     output_auxhist20, fname, n2, ierr )
   CALL output_auxhist20 ( grid%auxhist20_oid, grid , config_flags , ierr )
 CASE ( AUXHIST21_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST21_ALARM,       &
                     config_flags%auxhist21_outname, grid%auxhist21_oid, &
                     output_auxhist21, fname, n2, ierr )
   CALL output_auxhist21 ( grid%auxhist21_oid, grid , config_flags , ierr )
 CASE ( AUXHIST22_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST22_ALARM,       &
                     config_flags%auxhist22_outname, grid%auxhist22_oid, &
                     output_auxhist22, fname, n2, ierr )
   CALL output_auxhist22 ( grid%auxhist22_oid, grid , config_flags , ierr )
 CASE ( AUXHIST23_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST23_ALARM,       &
                     config_flags%auxhist23_outname, grid%auxhist23_oid, &
                     output_auxhist23, fname, n2, ierr )
   CALL output_auxhist23 ( grid%auxhist23_oid, grid , config_flags , ierr )
 CASE ( AUXHIST24_ALARM )
   CALL open_hist_w( grid, config_flags, stream, AUXHIST24_ALARM,       &
                     config_flags%auxhist24_outname, grid%auxhist24_oid, &
                     output_auxhist24, fname, n2, ierr )
   CALL output_auxhist24 ( grid%auxhist24_oid, grid , config_flags , ierr )
!ENDOFREGISTRYGENERATEDINCLUDE

   END SELECT

   WRITE(message,*)'med_hist_out: opened ',TRIM(fname),' as ',TRIM(n2)
   CALL wrf_debug( 1, message )

   grid%nframes(stream) = grid%nframes(stream) + 1

   SELECT CASE( stream )
     CASE ( HISTORY_ALARM )
       IF ( grid%nframes(stream) >= config_flags%frames_per_outfile ) THEN
         CALL close_dataset ( grid%oid , config_flags , n2 ) 
         grid%oid = 0
         grid%nframes(stream) = 0
       ENDIF

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/med_hist_out_closes.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
 CASE ( AUXHIST1_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist1 ) THEN
       CALL close_dataset ( grid%auxhist1_oid , config_flags , n2 )
       grid%auxhist1_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST2_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist2 ) THEN
       CALL close_dataset ( grid%auxhist2_oid , config_flags , n2 )
       grid%auxhist2_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST3_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist3 ) THEN
       CALL close_dataset ( grid%auxhist3_oid , config_flags , n2 )
       grid%auxhist3_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST4_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist4 ) THEN
       CALL close_dataset ( grid%auxhist4_oid , config_flags , n2 )
       grid%auxhist4_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST5_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist5 ) THEN
       CALL close_dataset ( grid%auxhist5_oid , config_flags , n2 )
       grid%auxhist5_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST6_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist6 ) THEN
       CALL close_dataset ( grid%auxhist6_oid , config_flags , n2 )
       grid%auxhist6_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST7_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist7 ) THEN
       CALL close_dataset ( grid%auxhist7_oid , config_flags , n2 )
       grid%auxhist7_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST8_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist8 ) THEN
       CALL close_dataset ( grid%auxhist8_oid , config_flags , n2 )
       grid%auxhist8_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST9_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist9 ) THEN
       CALL close_dataset ( grid%auxhist9_oid , config_flags , n2 )
       grid%auxhist9_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST10_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist10 ) THEN
       CALL close_dataset ( grid%auxhist10_oid , config_flags , n2 )
       grid%auxhist10_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST11_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist11 ) THEN
       CALL close_dataset ( grid%auxhist11_oid , config_flags , n2 )
       grid%auxhist11_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST12_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist12 ) THEN
       CALL close_dataset ( grid%auxhist12_oid , config_flags , n2 )
       grid%auxhist12_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST13_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist13 ) THEN
       CALL close_dataset ( grid%auxhist13_oid , config_flags , n2 )
       grid%auxhist13_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST14_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist14 ) THEN
       CALL close_dataset ( grid%auxhist14_oid , config_flags , n2 )
       grid%auxhist14_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST15_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist15 ) THEN
       CALL close_dataset ( grid%auxhist15_oid , config_flags , n2 )
       grid%auxhist15_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST16_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist16 ) THEN
       CALL close_dataset ( grid%auxhist16_oid , config_flags , n2 )
       grid%auxhist16_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST17_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist17 ) THEN
       CALL close_dataset ( grid%auxhist17_oid , config_flags , n2 )
       grid%auxhist17_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST18_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist18 ) THEN
       CALL close_dataset ( grid%auxhist18_oid , config_flags , n2 )
       grid%auxhist18_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST19_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist19 ) THEN
       CALL close_dataset ( grid%auxhist19_oid , config_flags , n2 )
       grid%auxhist19_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST20_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist20 ) THEN
       CALL close_dataset ( grid%auxhist20_oid , config_flags , n2 )
       grid%auxhist20_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST21_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist21 ) THEN
       CALL close_dataset ( grid%auxhist21_oid , config_flags , n2 )
       grid%auxhist21_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST22_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist22 ) THEN
       CALL close_dataset ( grid%auxhist22_oid , config_flags , n2 )
       grid%auxhist22_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST23_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist23 ) THEN
       CALL close_dataset ( grid%auxhist23_oid , config_flags , n2 )
       grid%auxhist23_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXHIST24_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxhist24 ) THEN
       CALL close_dataset ( grid%auxhist24_oid , config_flags , n2 )
       grid%auxhist24_oid = 0
       grid%nframes(stream) = 0
     ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE

   END SELECT
   IF ( wrf_dm_on_monitor() ) THEN
     WRITE ( message , FMT = '("Writing ",A30," for domain ",I8)' )TRIM(fname),grid%id
     CALL end_timing ( TRIM(message) )
   END IF

   
   call nmm_request_tg_reset(grid,config_flags,stream)

   RETURN
END SUBROUTINE med_hist_out

SUBROUTINE med_fddaobs_in ( grid , config_flags )
   USE module_domain	, ONLY : domain
   USE module_configure	, ONLY : grid_config_rec_type
   IMPLICIT NONE
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   CALL wrf_fddaobs_in( grid, config_flags )
   RETURN
END SUBROUTINE med_fddaobs_in

SUBROUTINE med_auxinput_in ( grid , stream, config_flags )
  
   USE module_domain	, ONLY : domain
   USE module_io_domain
  
   USE module_configure	, ONLY : grid_config_rec_type

   USE module_utility

   IMPLICIT NONE
  
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
   INTEGER , INTENT(IN)                       :: stream
  
   CHARACTER (LEN=256)                        :: message
   INTEGER :: ierr

   IF ( stream .LT. first_auxinput .OR. stream .GT. last_auxinput ) THEN
     WRITE(message,*)'med_auxinput_in: invalid input stream ',stream
     CALL wrf_error_fatal3("mediation_integrate.b",1705,&
message )
   ENDIF

   grid%nframes(stream) = grid%nframes(stream) + 1

   SELECT CASE( stream )






!STARTOFREGISTRYGENERATEDINCLUDE 'inc/med_auxinput_in.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
 CASE ( AUXINPUT1_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT1_ALARM,       &
                    config_flags%auxinput1_inname, grid%auxinput1_oid, &
                    input_auxinput1, ierr )
   CALL input_auxinput1 ( grid%auxinput1_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT2_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT2_ALARM,       &
                    config_flags%auxinput2_inname, grid%auxinput2_oid, &
                    input_auxinput2, ierr )
   CALL input_auxinput2 ( grid%auxinput2_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT3_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT3_ALARM,       &
                    config_flags%auxinput3_inname, grid%auxinput3_oid, &
                    input_auxinput3, ierr )
   CALL input_auxinput3 ( grid%auxinput3_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT4_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT4_ALARM,       &
                    config_flags%auxinput4_inname, grid%auxinput4_oid, &
                    input_auxinput4, ierr )
   CALL input_auxinput4 ( grid%auxinput4_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT5_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT5_ALARM,       &
                    config_flags%auxinput5_inname, grid%auxinput5_oid, &
                    input_auxinput5, ierr )
   CALL input_auxinput5 ( grid%auxinput5_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT6_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT6_ALARM,       &
                    config_flags%auxinput6_inname, grid%auxinput6_oid, &
                    input_auxinput6, ierr )
   CALL input_auxinput6 ( grid%auxinput6_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT7_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT7_ALARM,       &
                    config_flags%auxinput7_inname, grid%auxinput7_oid, &
                    input_auxinput7, ierr )
   CALL input_auxinput7 ( grid%auxinput7_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT8_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT8_ALARM,       &
                    config_flags%auxinput8_inname, grid%auxinput8_oid, &
                    input_auxinput8, ierr )
   CALL input_auxinput8 ( grid%auxinput8_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT9_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT9_ALARM,       &
                    config_flags%auxinput9_inname, grid%auxinput9_oid, &
                    input_auxinput9, ierr )
   CALL input_auxinput9 ( grid%auxinput9_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT10_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT10_ALARM,       &
                    config_flags%auxinput10_inname, grid%auxinput10_oid, &
                    input_auxinput10, ierr )
   CALL input_auxinput10 ( grid%auxinput10_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT11_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT11_ALARM,       &
                    config_flags%auxinput11_inname, grid%auxinput11_oid, &
                    input_auxinput11, ierr )
   CALL input_auxinput11 ( grid%auxinput11_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT12_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT12_ALARM,       &
                    config_flags%auxinput12_inname, grid%auxinput12_oid, &
                    input_auxinput12, ierr )
   CALL input_auxinput12 ( grid%auxinput12_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT13_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT13_ALARM,       &
                    config_flags%auxinput13_inname, grid%auxinput13_oid, &
                    input_auxinput13, ierr )
   CALL input_auxinput13 ( grid%auxinput13_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT14_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT14_ALARM,       &
                    config_flags%auxinput14_inname, grid%auxinput14_oid, &
                    input_auxinput14, ierr )
   CALL input_auxinput14 ( grid%auxinput14_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT15_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT15_ALARM,       &
                    config_flags%auxinput15_inname, grid%auxinput15_oid, &
                    input_auxinput15, ierr )
   CALL input_auxinput15 ( grid%auxinput15_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT16_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT16_ALARM,       &
                    config_flags%auxinput16_inname, grid%auxinput16_oid, &
                    input_auxinput16, ierr )
   CALL input_auxinput16 ( grid%auxinput16_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT17_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT17_ALARM,       &
                    config_flags%auxinput17_inname, grid%auxinput17_oid, &
                    input_auxinput17, ierr )
   CALL input_auxinput17 ( grid%auxinput17_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT18_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT18_ALARM,       &
                    config_flags%auxinput18_inname, grid%auxinput18_oid, &
                    input_auxinput18, ierr )
   CALL input_auxinput18 ( grid%auxinput18_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT19_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT19_ALARM,       &
                    config_flags%auxinput19_inname, grid%auxinput19_oid, &
                    input_auxinput19, ierr )
   CALL input_auxinput19 ( grid%auxinput19_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT20_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT20_ALARM,       &
                    config_flags%auxinput20_inname, grid%auxinput20_oid, &
                    input_auxinput20, ierr )
   CALL input_auxinput20 ( grid%auxinput20_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT21_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT21_ALARM,       &
                    config_flags%auxinput21_inname, grid%auxinput21_oid, &
                    input_auxinput21, ierr )
   CALL input_auxinput21 ( grid%auxinput21_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT22_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT22_ALARM,       &
                    config_flags%auxinput22_inname, grid%auxinput22_oid, &
                    input_auxinput22, ierr )
   CALL input_auxinput22 ( grid%auxinput22_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT23_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT23_ALARM,       &
                    config_flags%auxinput23_inname, grid%auxinput23_oid, &
                    input_auxinput23, ierr )
   CALL input_auxinput23 ( grid%auxinput23_oid, grid , config_flags , ierr )
 CASE ( AUXINPUT24_ALARM )
   CALL open_aux_u( grid, config_flags, stream, AUXINPUT24_ALARM,       &
                    config_flags%auxinput24_inname, grid%auxinput24_oid, &
                    input_auxinput24, ierr )
   CALL input_auxinput24 ( grid%auxinput24_oid, grid , config_flags , ierr )
!ENDOFREGISTRYGENERATEDINCLUDE
   END SELECT

   SELECT CASE( stream )

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/med_auxinput_in_closes.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
 CASE ( AUXINPUT1_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput1 ) THEN
       CALL close_dataset ( grid%auxinput1_oid , config_flags , "DATASET=AUXINPUT1" )
       grid%auxinput1_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT2_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput2 ) THEN
       CALL close_dataset ( grid%auxinput2_oid , config_flags , "DATASET=AUXINPUT2" )
       grid%auxinput2_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT3_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput3 ) THEN
       CALL close_dataset ( grid%auxinput3_oid , config_flags , "DATASET=AUXINPUT3" )
       grid%auxinput3_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT4_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput4 ) THEN
       CALL close_dataset ( grid%auxinput4_oid , config_flags , "DATASET=AUXINPUT4" )
       grid%auxinput4_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT5_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput5 ) THEN
       CALL close_dataset ( grid%auxinput5_oid , config_flags , "DATASET=AUXINPUT5" )
       grid%auxinput5_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT6_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput6 ) THEN
       CALL close_dataset ( grid%auxinput6_oid , config_flags , "DATASET=AUXINPUT6" )
       grid%auxinput6_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT7_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput7 ) THEN
       CALL close_dataset ( grid%auxinput7_oid , config_flags , "DATASET=AUXINPUT7" )
       grid%auxinput7_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT8_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput8 ) THEN
       CALL close_dataset ( grid%auxinput8_oid , config_flags , "DATASET=AUXINPUT8" )
       grid%auxinput8_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT9_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput9 ) THEN
       CALL close_dataset ( grid%auxinput9_oid , config_flags , "DATASET=AUXINPUT9" )
       grid%auxinput9_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT10_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput10 ) THEN
       CALL close_dataset ( grid%auxinput10_oid , config_flags , "DATASET=AUXINPUT10" )
       grid%auxinput10_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT11_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput11 ) THEN
       CALL close_dataset ( grid%auxinput11_oid , config_flags , "DATASET=AUXINPUT11" )
       grid%auxinput11_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT12_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput12 ) THEN
       CALL close_dataset ( grid%auxinput12_oid , config_flags , "DATASET=AUXINPUT12" )
       grid%auxinput12_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT13_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput13 ) THEN
       CALL close_dataset ( grid%auxinput13_oid , config_flags , "DATASET=AUXINPUT13" )
       grid%auxinput13_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT14_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput14 ) THEN
       CALL close_dataset ( grid%auxinput14_oid , config_flags , "DATASET=AUXINPUT14" )
       grid%auxinput14_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT15_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput15 ) THEN
       CALL close_dataset ( grid%auxinput15_oid , config_flags , "DATASET=AUXINPUT15" )
       grid%auxinput15_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT16_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput16 ) THEN
       CALL close_dataset ( grid%auxinput16_oid , config_flags , "DATASET=AUXINPUT16" )
       grid%auxinput16_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT17_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput17 ) THEN
       CALL close_dataset ( grid%auxinput17_oid , config_flags , "DATASET=AUXINPUT17" )
       grid%auxinput17_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT18_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput18 ) THEN
       CALL close_dataset ( grid%auxinput18_oid , config_flags , "DATASET=AUXINPUT18" )
       grid%auxinput18_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT19_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput19 ) THEN
       CALL close_dataset ( grid%auxinput19_oid , config_flags , "DATASET=AUXINPUT19" )
       grid%auxinput19_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT20_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput20 ) THEN
       CALL close_dataset ( grid%auxinput20_oid , config_flags , "DATASET=AUXINPUT20" )
       grid%auxinput20_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT21_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput21 ) THEN
       CALL close_dataset ( grid%auxinput21_oid , config_flags , "DATASET=AUXINPUT21" )
       grid%auxinput21_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT22_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput22 ) THEN
       CALL close_dataset ( grid%auxinput22_oid , config_flags , "DATASET=AUXINPUT22" )
       grid%auxinput22_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT23_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput23 ) THEN
       CALL close_dataset ( grid%auxinput23_oid , config_flags , "DATASET=AUXINPUT23" )
       grid%auxinput23_oid = 0
       grid%nframes(stream) = 0
     ENDIF
 CASE ( AUXINPUT24_ALARM )
     IF ( grid%nframes(stream) >= config_flags%frames_per_auxinput24 ) THEN
       CALL close_dataset ( grid%auxinput24_oid , config_flags , "DATASET=AUXINPUT24" )
       grid%auxinput24_oid = 0
       grid%nframes(stream) = 0
     ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
   END SELECT

   RETURN
END SUBROUTINE med_auxinput_in

SUBROUTINE med_filter_out ( grid , config_flags )
  
   USE module_domain	, ONLY : domain , domain_clock_get
   USE module_io_domain
   USE module_timing
   USE module_configure	, ONLY : grid_config_rec_type
  
   USE module_bc_time_utilities

   IMPLICIT NONE

  
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*80                           :: rstname , outname
   INTEGER                                :: fid , rid
   CHARACTER (LEN=256)                    :: message
   INTEGER                                :: ierr
   INTEGER                                :: myproc
   CHARACTER*80                           :: timestr

   IF ( config_flags%write_input ) THEN

   IF ( wrf_dm_on_monitor() ) THEN
     CALL start_timing
   END IF

     CALL domain_clock_get( grid, current_timestr=timestr )
     CALL construct_filename2a ( outname , config_flags%input_outname , grid%id , 2 , timestr )

     WRITE ( message , '("med_filter_out 1: opening ",A," for writing. ")') TRIM ( outname )
     CALL wrf_debug( 1, message )

     CALL open_w_dataset ( fid, TRIM(outname), grid ,  &
                           config_flags , output_input , "DATASET=INPUT", ierr )
     IF ( ierr .NE. 0 ) THEN
       CALL wrf_error_fatal3("mediation_integrate.b",1767,&
message )
     ENDIF

     IF ( ierr .NE. 0 ) THEN
       CALL wrf_error_fatal3("mediation_integrate.b",1772,&
message )
     ENDIF

   CALL output_input ( fid, grid , config_flags , ierr )
   CALL close_dataset ( fid , config_flags , "DATASET=INPUT" )

   IF ( wrf_dm_on_monitor() ) THEN
     WRITE ( message , FMT = '("Writing filter output for domain ",I8)' ) grid%id
     CALL end_timing ( TRIM(message) )
   END IF
   ENDIF

   RETURN
END SUBROUTINE med_filter_out

SUBROUTINE med_latbound_in ( grid , config_flags )
  
   USE module_domain	, ONLY : domain , domain_clock_get, head_grid
   USE module_io_domain
   USE module_timing
   USE module_configure	, ONLY : grid_config_rec_type
  

   USE module_utility

   IMPLICIT NONE

  
!WRF Error and Warning messages (1-999)
!All i/o package-specific status codes you may want to add must be handled by your package (see below)
! WRF handles these and netCDF messages only
  integer, parameter  :: WRF_NO_ERR                  =  0       !no error
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       !file not found, or incomplete
  integer, parameter  :: WRF_WARN_MD_NF              = -2       !metadata not found
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       !timestamp not found
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       !no more timestamps
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       !variable not found
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       !no more variables for the current time
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       !too many open files
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       !data type mismatch
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       !attempt to write readonly file
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      !attempt to read writeonly file
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      !attempt to access unopened file
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      !attempt to do 2 trainings for 1 variable
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      !attempt to read past EOF
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      !bad data handle
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      !write length not equal to training length
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      !more dimensions requested than training
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      !attempt to read more data than exists
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      !input dimension inconsistent
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      !input MemoryOrder not recognized
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      !a dimension name with 2 different lengths
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      !string longer than provided storage
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      !function not supportable
  integer, parameter  :: WRF_WARN_NOOP               = -23      !package implements this routine as NOOP

!Fatal errors 
  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 !allocation error
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 !dealloc error
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 !bad file status


!Package specific errors (1000+)        
!Netcdf status codes
!WRF will accept status codes of 1000+, but it is up to the package to handle
! and return the status to the user.

  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    !this code not in either spec?
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    !this code not in either spec?
  integer, parameter  :: WRF_WARN_FILE_OPEN_FOR_READ = -1009
  integer, parameter  :: WRF_IO_NOT_INITIALIZED      = -1010
  integer, parameter  :: WRF_WARN_MD_AFTER_OPEN      = -1011
  integer, parameter  :: WRF_WARN_TOO_MANY_VARIABLES = -1012
  integer, parameter  :: WRF_WARN_DRYRUN_CLOSE       = -1013
  integer, parameter  :: WRF_WARN_DATESTR_BAD_LENGTH = -1014
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_READ   = -1015
  integer, parameter  :: WRF_WARN_DATA_TYPE_NOT_FOUND = -1016
  integer, parameter  :: WRF_WARN_DATESTR_ERROR      = -1017
  integer, parameter  :: WRF_WARN_DRYRUN_READ        = -1018
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_GET    = -1019
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_PUT    = -1020
  integer, parameter  :: WRF_WARN_NETCDF             = -1021    
  integer, parameter  :: WRF_WARN_LENGTH_LESS_THAN_1 = -1022    
  integer, parameter  :: WRF_WARN_MORE_DATA_IN_FILE  = -1023    
  integer, parameter  :: WRF_WARN_DATE_LT_LAST_DATE  = -1024

! For HDF5 only
  integer, parameter  :: WRF_HDF5_ERR_FILE                 = -200
  integer, parameter  :: WRF_HDF5_ERR_MD                   = -201
  integer, parameter  :: WRF_HDF5_ERR_TIME                 = -202
  integer, parameter  :: WRF_HDF5_ERR_TIME_EOF             = -203
  integer, parameter  :: WRF_HDF5_ERR_MORE_DATA_IN_FILE    = -204
  integer, parameter  :: WRF_HDF5_ERR_DATE_LT_LAST_DATE    = -205
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_FILES       = -206
  integer, parameter  :: WRF_HDF5_ERR_TYPE_MISMATCH        = -207
  integer, parameter  :: WRF_HDF5_ERR_LENGTH_LESS_THAN_1   = -208
  integer, parameter  :: WRF_HDF5_ERR_WRITE_RONLY_FILE     = -209
  integer, parameter  :: WRF_HDF5_ERR_READ_WONLY_FILE      = -210
  integer, parameter  :: WRF_HDF5_ERR_FILE_NOT_OPENED      = -211
  integer, parameter  :: WRF_HDF5_ERR_DATESTR_ERROR        = -212
  integer, parameter  :: WRF_HDF5_ERR_DRYRUN_READ          = -213
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_GET      = -214
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_PUT      = -215
  integer, parameter  :: WRF_HDF5_ERR_2DRYRUNS_1VARIABLE   = -216
  integer, parameter  :: WRF_HDF5_ERR_DATA_TYPE_NOTFOUND   = -217
  integer, parameter  :: WRF_HDF5_ERR_READ_PAST_EOF        = -218
  integer, parameter  :: WRF_HDF5_ERR_BAD_DATA_HANDLE      = -219
  integer, parameter  :: WRF_HDF5_ERR_WRTLEN_NE_DRRUNLEN   = -220
  integer, parameter  :: WRF_HDF5_ERR_DRYRUN_CLOSE         = -221
  integer, parameter  :: WRF_HDF5_ERR_DATESTR_BAD_LENGTH   = -222
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_READ     = -223
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_DIMS        = -224
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_VARIABLES   = -225
  integer, parameter  :: WRF_HDF5_ERR_COUNT_TOO_LONG       = -226
  integer, parameter  :: WRF_HDF5_ERR_DIMENSION_ERROR      = -227
  integer, parameter  :: WRF_HDF5_ERR_BAD_MEMORYORDER      = -228
  integer, parameter  :: WRF_HDF5_ERR_DIMNAME_REDEFINED    = -229
  integer, parameter  :: WRF_HDF5_ERR_MD_AFTER_OPEN        = -230
  integer, parameter  :: WRF_HDF5_ERR_CHARSTR_GT_LENDATA   = -231
  integer, parameter  :: WRF_HDF5_ERR_BAD_DATA_TYPE        = -232
  integer, parameter  :: WRF_HDF5_ERR_FILE_NOT_COMMITTED   = -233

  integer, parameter  :: WRF_HDF5_ERR_ALLOCATION        = -2001
  integer, parameter  :: WRF_HDF5_ERR_DEALLOCATION      = -2002
  integer, parameter  :: WRF_HDF5_ERR_BAD_FILE_STATUS   = -2003
  integer, parameter  :: WRF_HDF5_ERR_BAD_VARIABLE_DIM  = -2004
  integer, parameter  :: WRF_HDF5_ERR_MDVAR_DIM_NOT_1D  = -2005
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_TIMES    = -2006
  integer, parameter ::  WRF_HDF5_ERR_DATA_ID_NOTFOUND  = -2007

  integer, parameter ::  WRF_HDF5_ERR_DATASPACE         = -300
  integer, parameter ::  WRF_HDF5_ERR_DATATYPE          = -301
  integer, parameter :: WRF_HDF5_ERR_PROPERTY_LIST      = -302

  integer, parameter :: WRF_HDF5_ERR_DATASET_CREATE     = -303
  integer, parameter :: WRF_HDF5_ERR_DATASET_READ       = -304
  integer, parameter :: WRF_HDF5_ERR_DATASET_WRITE      = -305
  integer, parameter :: WRF_HDF5_ERR_DATASET_OPEN       = -306
  integer, parameter :: WRF_HDF5_ERR_DATASET_GENERAL    = -307
  integer, parameter :: WRF_HDF5_ERR_GROUP              = -308

  integer, parameter :: WRF_HDF5_ERR_FILE_OPEN          = -309
  integer, parameter :: WRF_HDF5_ERR_FILE_CREATE        = -310
  integer, parameter :: WRF_HDF5_ERR_DATASET_CLOSE      = -311
  integer, parameter :: WRF_HDF5_ERR_FILE_CLOSE         = -312
  integer, parameter :: WRF_HDF5_ERR_CLOSE_GENERAL      = -313

  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_CREATE   = -314
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_READ     = -315
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_WRITE    = -316
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_OPEN     = -317
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_GENERAL  = -318
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_CLOSE    = -319

  integer, parameter :: WRF_HDF5_ERR_OTHERS             = -320
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_OTHERS   = -321


  
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  
   LOGICAL, EXTERNAL                      :: wrf_dm_on_monitor
   LOGICAL                                :: lbc_opened
   INTEGER                                :: idum1 , idum2 , ierr , open_status , fid, rc
   REAL                                   :: bfrq
   CHARACTER (LEN=256)                    :: message
   CHARACTER (LEN=80)                     :: bdyname
   Type (WRFU_Time )                      :: startTime, stopTime, currentTime
   Type (WRFU_TimeInterval )              :: stepTime
integer myproc,i,j,k

      integer, parameter  :: WRF_FILE_NOT_OPENED                  = 100
      integer, parameter  :: WRF_FILE_OPENED_NOT_COMMITTED        = 101
      integer, parameter  :: WRF_FILE_OPENED_FOR_WRITE            = 102
      integer, parameter  :: WRF_FILE_OPENED_FOR_READ             = 103
      integer, parameter  :: WRF_REAL                             = 104
      integer, parameter  :: WRF_DOUBLE                           = 105
      integer, parameter  :: WRF_FLOAT=WRF_REAL
      integer, parameter  :: WRF_INTEGER                          = 106
      integer, parameter  :: WRF_LOGICAL                          = 107
      integer, parameter  :: WRF_COMPLEX                          = 108
      integer, parameter  :: WRF_DOUBLE_COMPLEX                   = 109
      integer, parameter  :: WRF_FILE_OPENED_FOR_UPDATE           = 110
! This bit is for backwards compatibility with old variants of these flags 
! that are still being used in io_grib1 and io_phdf5.  It should be removed!  
      integer, parameter  :: WRF_FILE_OPENED_AND_COMMITTED        = 102

   CALL wrf_debug ( 200 , 'in med_latbound_in' )


   
   
   IF ( (grid%dfi_opt .EQ. DFI_DDFI .OR. grid%dfi_opt .EQ. DFI_TDFI) .AND. grid%dfi_stage .EQ. DFI_FWD ) RETURN


   IF ( grid%id .EQ. 1 .AND. config_flags%specified .AND. config_flags%io_form_boundary .GT. 0 ) THEN

     CALL domain_clock_get( grid, current_time=currentTime, &
                                  start_time=startTime,     &
                                  stop_time=stopTime,       &
                                  time_step=stepTime )













     IF ( WRFU_AlarmIsRinging( grid%alarms( BOUNDARY_ALARM ), rc=rc ) ) THEN
       CALL wrf_debug ( 100 , 'in med_latbound_in preparing to read' )
       CALL WRFU_AlarmRingerOff( grid%alarms( BOUNDARY_ALARM ), rc=rc )
       IF ( wrf_dm_on_monitor() ) CALL start_timing


       CALL construct_filename2a ( bdyname , config_flags%bdy_inname , grid%id , 2 , " " )

       CALL wrf_inquire_opened(grid%lbc_fid , TRIM(bdyname) , open_status , ierr ) 
       IF ( open_status .EQ. WRF_FILE_OPENED_FOR_READ ) THEN
         lbc_opened = .TRUE.
       ELSE
         lbc_opened = .FALSE.
       ENDIF
       CALL wrf_dm_bcast_bytes ( lbc_opened , 4 )
       IF ( .NOT. lbc_opened ) THEN
         CALL construct_filename2a ( bdyname , config_flags%bdy_inname , grid%id , 2 , " " )
          WRITE(message,*)'Opening: ',TRIM(bdyname)
          CALL wrf_debug(100,TRIM(message))
          CALL open_r_dataset ( grid%lbc_fid, TRIM(bdyname) , grid , config_flags , "DATASET=BOUNDARY", ierr )
          IF ( ierr .NE. 0 ) THEN
            WRITE( message, * ) 'med_latbound_in: error opening ',TRIM(bdyname), ' for reading. IERR = ',ierr
            CALL wrf_error_fatal3("mediation_integrate.b",1881,&
message )
          ENDIF
       ELSE
         CALL wrf_debug( 100 , bdyname // ' is already opened' )
       ENDIF
       CALL wrf_debug( 100 , 'med_latbound_in: calling input_boundary ' )
       CALL input_boundary ( grid%lbc_fid, grid , config_flags , ierr )


       IF ( (config_flags%dfi_opt .NE. DFI_NODFI) .AND. (head_grid%dfi_stage .NE. DFI_FST) ) THEN
          CALL wrf_debug( 100 , 'med_latbound_in: closing boundary file ' )
          CALL close_dataset ( grid%lbc_fid , config_flags , "DATASET=BOUNDARY" )
       END IF


       CALL domain_clock_get( grid, current_time=currentTime )
       DO WHILE (currentTime .GE. grid%next_bdy_time )         
         CALL wrf_debug( 100 , 'med_latbound_in: calling input_boundary ' )
         CALL input_boundary ( grid%lbc_fid, grid , config_flags , ierr )
       ENDDO
       CALL WRFU_AlarmSet( grid%alarms( BOUNDARY_ALARM ), RingTime=grid%next_bdy_time, rc=rc )

       IF ( ierr .NE. 0 .and. ierr .NE. WRF_WARN_NETCDF ) THEN
         WRITE( message, * ) 'med_latbound_in: error reading ',TRIM(bdyname), ' IERR = ',ierr
         CALL wrf_error_fatal3("mediation_integrate.b",1910,&
message )
       ENDIF
       IF ( currentTime .EQ. grid%this_bdy_time ) grid%dtbc = 0.
  
       IF ( wrf_dm_on_monitor() ) THEN
         WRITE ( message , FMT = '("processing lateral boundary for domain ",I8)' ) grid%id
         CALL end_timing ( TRIM(message) )
       ENDIF
     ENDIF
   ENDIF
   RETURN
END SUBROUTINE med_latbound_in

SUBROUTINE med_setup_step ( grid , config_flags )
  
   USE module_domain	, ONLY : domain
   USE module_configure	, ONLY : grid_config_rec_type
  

   IMPLICIT NONE











  
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags
  
   INTEGER                                    :: idum1 , idum2

   CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )

   RETURN

END SUBROUTINE med_setup_step

SUBROUTINE med_endup_step ( grid , config_flags )
  
   USE module_domain	, ONLY : domain
   USE module_configure	, ONLY : grid_config_rec_type, model_config_rec
  

   IMPLICIT NONE











  
   TYPE(domain)                               :: grid
   TYPE (grid_config_rec_type) , INTENT(OUT)   :: config_flags
  
   INTEGER                                    :: idum1 , idum2

   IF ( grid%id .EQ. 1 ) THEN
     
     model_config_rec%restart = .FALSE.
     config_flags%restart = .FALSE.
     CALL nl_set_restart(1, .FALSE.)

   ENDIF

   RETURN

END SUBROUTINE med_endup_step

SUBROUTINE open_aux_u ( grid , config_flags, stream, alarm_id, &
                        auxinput_inname, oid, insub, ierr )
  
   USE module_domain	, ONLY : domain , domain_clock_get
   USE module_io_domain
  
   USE module_configure	, ONLY : grid_config_rec_type

   USE module_utility

   IMPLICIT NONE
  
   TYPE(domain)                                :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)    :: config_flags
   INTEGER ,                     INTENT(IN)    :: stream
   INTEGER ,                     INTENT(IN)    :: alarm_id
   CHARACTER*(*) ,               INTENT(IN)    :: auxinput_inname
   INTEGER ,                     INTENT(INOUT) :: oid
   EXTERNAL                                       insub
   INTEGER ,                     INTENT(OUT)   :: ierr
  
   CHARACTER*80                           :: fname, n2
   CHARACTER (LEN=256)                    :: message
   CHARACTER*80                           :: timestr
   TYPE(WRFU_Time)                        :: ST,CT
   LOGICAL                                :: adjust

   IF ( stream .LT. first_stream .OR. stream .GT. last_stream ) THEN
     WRITE(message,*)'open_aux_u: invalid input stream ',stream
     CALL wrf_error_fatal3("mediation_integrate.b",2019,&
message )
   ENDIF

   ierr = 0

   IF ( oid .eq. 0 ) THEN
     CALL domain_clock_get( grid, current_time=CT, start_time=ST, &
                            current_timestr=timestr )
     CALL nl_get_adjust_input_times( grid%id, adjust )
     IF ( adjust ) THEN 
       CALL adjust_io_timestr( grid%io_intervals( alarm_id ), CT, ST, timestr )
     ENDIF
     CALL construct_filename2a ( fname , auxinput_inname, &
                                 grid%id , 2 , timestr )
     IF      ( stream-first_input .EQ. 10 ) THEN
       WRITE(n2,'("DATASET=AUXINPUT10")')
     ELSE IF ( stream-first_input .EQ. 11 ) THEN
       WRITE(n2,'("DATASET=AUXINPUT11")')
     ELSE IF ( stream-first_input .GE. 10 ) THEN
       WRITE(n2,'("DATASET=AUXINPUT",I2)')stream-first_input
     ELSE
       WRITE(n2,'("DATASET=AUXINPUT",I1)')stream-first_input
     ENDIF
     WRITE ( message , '("open_aux_u : opening ",A," for reading. DATASET ",A)') TRIM ( fname ),TRIM(n2)
     CALL wrf_debug( 1, message )








     CALL open_u_dataset ( oid, TRIM(fname), grid ,  &
                           config_flags , insub , n2, ierr )
   ENDIF
   IF ( ierr .NE. 0 ) THEN
     WRITE ( message , '("open_aux_u : error opening ",A," for reading. ",I3)') &
       TRIM ( fname ), ierr
     CALL wrf_message( message )
   ENDIF
   RETURN
END SUBROUTINE open_aux_u

SUBROUTINE open_hist_w ( grid , config_flags, stream, alarm_id, &
                         hist_outname, oid, outsub, fname, n2, ierr )
  
   USE module_domain	, ONLY : domain , domain_clock_get
   USE module_io_domain
  
   USE module_configure	, ONLY : grid_config_rec_type

   USE module_utility
   IMPLICIT NONE
  
   TYPE(domain)                                :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)    :: config_flags
   INTEGER ,                     INTENT(IN)    :: stream
   INTEGER ,                     INTENT(IN)    :: alarm_id
   CHARACTER*(*) ,               INTENT(IN)    :: hist_outname
   INTEGER ,                     INTENT(INOUT) :: oid
   EXTERNAL                                       outsub
   CHARACTER*(*) ,               INTENT(OUT)   :: fname, n2
   INTEGER ,                     INTENT(OUT)   :: ierr
  
   INTEGER                                :: len_n2
   CHARACTER (LEN=256)                    :: message
   CHARACTER*80                           :: timestr
   TYPE(WRFU_Time)                        :: ST,CT
   LOGICAL                                :: adjust

   IF ( stream .LT. first_history .OR. stream .GT. last_history ) THEN
     WRITE(message,*)'open_hist_w: invalid history stream ',stream
     CALL wrf_error_fatal3("mediation_integrate.b",2093,&
message )
   ENDIF

   ierr = 0

   
   
   CALL domain_clock_get( grid, current_time=CT, start_time=ST, &
                          current_timestr=timestr )
   CALL nl_get_adjust_output_times( grid%id, adjust )
   IF ( adjust ) THEN 
     CALL adjust_io_timestr( grid%io_intervals( alarm_id ), CT, ST, timestr )
   ENDIF
   CALL construct_filename2a ( fname , hist_outname, &
                               grid%id , 2 , timestr )
   IF ( stream .EQ. history_only ) THEN
     WRITE(n2,'("DATASET=HISTORY")')
   ELSE IF ( stream-first_history .GE. 10 ) THEN
     WRITE(n2,'("DATASET=AUXHIST",I2)')stream-first_history
   ELSE
     WRITE(n2,'("DATASET=AUXHIST",I1)')stream-first_history
   ENDIF
   IF ( oid .eq. 0 ) THEN
     WRITE ( message , '("open_hist_w : opening ",A," for writing. ")') TRIM ( fname )
     CALL wrf_debug( 1, message )








     CALL open_w_dataset ( oid, TRIM(fname), grid ,  &
                           config_flags , outsub , n2, ierr )
   ENDIF
   IF ( ierr .NE. 0 ) THEN
     WRITE ( message , '("open_hist_w : error opening ",A," for writing. ",I3)') &
       TRIM ( fname ), ierr
     CALL wrf_message( message )
   ENDIF

   RETURN
END SUBROUTINE open_hist_w

 






RECURSIVE SUBROUTINE med_namelist_out ( grid , config_flags )
  
   USE module_domain	, ONLY : domain, domain_clock_get
   USE module_io_domain
   USE module_timing
  
   USE module_configure	, ONLY : grid_config_rec_type
   USE module_bc_time_utilities

   USE module_utility


   IMPLICIT NONE

  
   TYPE(domain), INTENT(IN)                   :: grid
   TYPE (grid_config_rec_type) , INTENT(IN)   :: config_flags

  

   TYPE(WRFU_Time) :: CurrTime
   INTEGER                                :: nout,rc,kid
   INTEGER                                :: hr, min, sec, ms,julyr,julday
   REAL                                   :: GMT
   CHARACTER*80                           :: prefix, outname
   CHARACTER*80                           :: timestr
   LOGICAL                                :: exist
   LOGICAL,EXTERNAL :: wrf_dm_on_monitor

   TYPE (grid_config_rec_type)            :: kid_config_flags

   prefix = "wrfnamelist_d<domain>_<date>"
   nout = 99



   CALL domain_clock_get( grid, current_timestr=timestr )

   CALL construct_filename2a ( outname , prefix, grid%id , 2 , timestr )

   IF ( wrf_dm_on_monitor() ) THEN

   CLOSE (NOUT)
   OPEN ( FILE   = trim(outname) , UNIT   = nout, STATUS = 'UNKNOWN', FORM   = 'FORMATTED')

   CALL domain_clock_get( grid, current_time=CurrTime )
   CALL WRFU_TimeGet( CurrTime, YY=julyr, dayOfYear=julday, H=hr, M=min, S=sec, MS=ms, rc=rc)

   gmt=hr+real(min)/60.+real(sec)/3600.+real(ms)/(1000*3600)
   WRITE(NOUT,*) grid%i_parent_start
   WRITE(NOUT,*) grid%j_parent_start
   WRITE(NOUT,*) julyr
   WRITE(NOUT,*) julday
   WRITE(NOUT,*) gmt

   CLOSE (NOUT)
   ENDIF

   
   DO kid = 1, max_nests
      IF ( ASSOCIATED( grid%nests(kid)%ptr ) ) THEN
        CALL model_to_grid_config_rec ( grid%nests(kid)%ptr%id , model_config_rec , kid_config_flags )
        CALL med_namelist_out ( grid%nests(kid)%ptr , kid_config_flags )
      ENDIF
   ENDDO

   RETURN
END SUBROUTINE med_namelist_out

