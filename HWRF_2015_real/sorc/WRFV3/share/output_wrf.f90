

  SUBROUTINE output_wrf ( fid , grid , config_flags, switch , ierr )
    USE module_io
    USE module_wrf_error
    USE module_io_wrf
    USE module_domain
    USE module_domain_type, ONLY : fieldlist
    USE module_state_description
    USE module_configure

    USE module_model_constants
    USE module_utility
    IMPLICIT NONE
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

    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(INOUT)    :: config_flags
    INTEGER, INTENT(IN) :: fid, switch
    INTEGER, INTENT(INOUT) :: ierr

    
    INTEGER ids , ide , jds , jde , kds , kde , &
            ims , ime , jms , jme , kms , kme , &
            ips , ipe , jps , jpe , kps , kpe

    TYPE( fieldlist ), POINTER :: p

    INTEGER newswitch, itrace

    INTEGER , DIMENSION(3) :: domain_start , domain_end
    INTEGER , DIMENSION(3) :: memory_start , memory_end
    INTEGER , DIMENSION(3) :: patch_start , patch_end
    INTEGER i,j
    INTEGER julyr, julday, idt, iswater , islake, map_proj
    INTEGER filestate
    LOGICAL dryrun
    REAL    gmt, cen_lat, cen_lon, bdyfrq , truelat1 , truelat2 , moad_cen_lat , stand_lon
    INTEGER km_opt, diff_opt, damp_opt,  &
            mp_physics, ra_lw_physics, ra_sw_physics, sf_sfclay_physics, &
            sf_surface_physics, bl_pbl_physics, cu_physics, hypsometric_opt, sf_lake_physics, &
            use_theta_m
    INTEGER swint_opt, aer_type,aer_aod550_opt,aer_angexp_opt,aer_ssa_opt,aer_asy_opt, aer_opt
    REAL    aer_aod550_val,aer_angexp_val,aer_ssa_val,aer_asy_val
    REAL    khdif, kvdif, swrad_scat, dampcoef,radt,bldt,cudt
    REAL    dt, adapt_dt_start, adapt_dt_min, adapt_dt_max
    INTEGER sf_urban_physics, w_damping, smooth_option, feedback, surface_input_source, sst_update
    INTEGER skebs_on, sppt_on, rand_perturb_on, nens,ISEED_SKEBS,ISEED_SPPT,ISEED_RAND_PERT
    INTEGER skebs_vertstruc, sppt_vertstruc, rand_pert_vertstruc
    INTEGER LMINFORC,LMAXFORC,KMINFORC,KMAXFORC,LMINFORCT,LMAXFORCT,KMINFORCT,KMAXFORCT
    REAL    gridpt_stddev_rand_pert,stddev_cutoff_rand_pert,timescale_rand_pert
    REAL    gridpt_stddev_sppt,stddev_cutoff_sppt,timescale_sppt
    REAL    tot_backscat_psi,tot_backscat_t,REXPONENT_PSI,REXPONENT_T,ZTAU_PSI,ZTAU_T
    CHARACTER (len=19) simulation_start_date
    CHARACTER (len=len_current_date) current_date_save
    INTEGER simulation_start_year   , &
            simulation_start_month  , &
            simulation_start_day    , &
            simulation_start_hour   , &
            simulation_start_minute , &
            simulation_start_second
    INTEGER rc
    INTEGER :: io_form
    LOGICAL, EXTERNAL :: multi_files
    INTEGER, EXTERNAL :: use_package
    INTEGER p_hr, p_min, p_sec, p_ms

    CHARACTER*80  dname, memord
    CHARACTER*256 message
    CHARACTER*80  fname
    CHARACTER*80  char_junk
    CHARACTER(LEN=256) :: MMINLU
    INTEGER    ibuf(1)
    REAL       rbuf(1)
    TYPE(WRFU_TimeInterval) :: bdy_increment
    TYPE(WRFU_Time)         :: next_time, currentTime, startTime
    CHARACTER*40            :: next_datestr
    INTEGER :: start_year , start_month , start_day , start_hour , start_minute , start_second
    LOGICAL :: adjust

    TYPE(WRFU_Time) :: ringTime, stopTime, curtime
    TYPE(WRFU_TimeInterval) :: interval, tmpinterval
    CHARACTER*80 alarmname, timestring, debuggal
    INTEGER seconds, seconds2, iring
    INTEGER :: nio_tasks_per_group

    LOGICAL, EXTERNAL :: wrf_dm_on_monitor

    WRITE(wrf_err_message,*)'output_wrf: begin, fid = ',fid
    CALL wrf_debug( 300 , wrf_err_message )

    CALL modify_io_masks ( grid%id )   

    CALL wrf_inquire_filename ( fid , fname , filestate , ierr )
    IF ( ierr /= 0 ) THEN
      WRITE(wrf_err_message,*)'module_io_wrf: output_wrf: wrf_inquire_filename Status = ',ierr
      CALL wrf_error_fatal3("output_wrf.b",111,&
wrf_err_message )
    ENDIF

    WRITE(wrf_err_message,*)'output_wrf: fid,filestate = ',fid,filestate
    CALL wrf_debug( 300 , wrf_err_message )

    
    
    io_form = io_form_for_stream( switch ) 

    dryrun       = ( filestate .EQ. WRF_FILE_OPENED_NOT_COMMITTED )

    WRITE(wrf_err_message,*)'output_wrf: dryrun = ',dryrun
    CALL wrf_debug( 300 , wrf_err_message )

    CALL get_ijk_from_grid (  grid ,                        &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              ips, ipe, jps, jpe, kps, kpe    )

    call nl_get_diff_opt           ( grid%id , diff_opt           )
    call nl_get_km_opt             ( grid%id,  km_opt             )
    call nl_get_damp_opt           ( 1, damp_opt                  )
    call nl_get_dampcoef           ( grid%id,  dampcoef           )
    call nl_get_khdif              ( grid%id,  khdif              )
    call nl_get_kvdif              ( grid%id,  kvdif              )
    call nl_get_mp_physics         ( grid%id,  mp_physics         )
    call nl_get_ra_lw_physics      ( grid%id,  ra_lw_physics      )
    call nl_get_ra_sw_physics      ( grid%id,  ra_sw_physics      )
    call nl_get_sf_sfclay_physics  ( grid%id,  sf_sfclay_physics  )
    call nl_get_sf_surface_physics ( grid%id,  sf_surface_physics )
    call nl_get_bl_pbl_physics     ( grid%id,  bl_pbl_physics     )
    call nl_get_cu_physics         ( grid%id,  cu_physics         )
    call nl_get_radt               ( grid%id,  radt               )
    call nl_get_bldt               ( grid%id,  bldt               )
    call nl_get_cudt               ( grid%id,  cudt               )
    call nl_get_aer_opt            ( grid%id,  aer_opt            )
    call nl_get_swint_opt          ( grid%id,  swint_opt          )
    call nl_get_aer_type           ( grid%id,  aer_type           )
    call nl_get_aer_aod550_opt     ( grid%id,  aer_aod550_opt     )
    call nl_get_aer_angexp_opt     ( grid%id,  aer_angexp_opt     )
    call nl_get_aer_ssa_opt        ( grid%id,  aer_ssa_opt        )
    call nl_get_aer_asy_opt        ( grid%id,  aer_asy_opt        )
    call nl_get_aer_aod550_val     ( grid%id,  aer_aod550_val     )
    call nl_get_aer_angexp_val     ( grid%id,  aer_angexp_val     )
    call nl_get_aer_ssa_opt        ( grid%id,  aer_ssa_val        )
    call nl_get_aer_asy_opt        ( grid%id,  aer_asy_val        )
    call nl_get_sf_lake_physics    ( grid%id,  sf_lake_physics    )



    call nl_get_surface_input_source ( 1      ,  surface_input_source )
    call nl_get_sst_update           ( 1      ,  sst_update           )
    call nl_get_feedback             ( 1      ,  feedback             )
    call nl_get_smooth_option        ( 1      ,  smooth_option        )
    call nl_get_swrad_scat           ( 1      ,  swrad_scat           )
    call nl_get_sf_urban_physics     ( 1      ,  sf_urban_physics     )
    call nl_get_w_damping            ( 1      ,  w_damping            )



    CALL nl_get_gmt (grid%id, gmt)
    CALL nl_get_julyr (grid%id, julyr)
    CALL nl_get_julday (grid%id, julday)
    CALL nl_get_mminlu ( grid%id, mminlu )
    call wrf_debug(300,"OUTPUT_WRF:  mminlu = " // mminlu )
    CALL nl_get_iswater (grid%id, iswater )
    CALL nl_get_islake (grid%id, islake )
    CALL nl_get_cen_lat ( grid%id , cen_lat )
    CALL nl_get_cen_lon ( grid%id , cen_lon )
    CALL nl_get_truelat1 ( grid%id , truelat1 )
    CALL nl_get_truelat2 ( grid%id , truelat2 )
    CALL nl_get_moad_cen_lat ( grid%id , moad_cen_lat )
    CALL nl_get_stand_lon ( grid%id , stand_lon )
    CALL nl_get_map_proj ( grid%id , map_proj )


    CALL domain_clockprint(150, grid, &
           'DEBUG output_wrf():  before call to domain_clock_get,')
    CALL domain_clock_get( grid, current_time=currentTime, &
                                 start_time=startTime,     &
                                 current_timestr=current_date )

    IF (switch .EQ. history_only) THEN
      CALL nl_get_adjust_output_times( grid%id, adjust )
      IF ( adjust ) THEN
        CALL adjust_io_timestr( grid%io_intervals( history_alarm ), currentTime, startTime, timestring )
        current_date_save = current_date
        current_date = timestring
      ENDIF
    ENDIF

    WRITE ( wrf_err_message , * ) 'output_wrf: begin, current_date=',current_date
    CALL wrf_debug ( 300 , wrf_err_message )

    WRITE( message , * ) "OUTPUT FROM " , TRIM(program_name)
    CALL wrf_put_dom_ti_char ( fid , 'TITLE' , TRIM(message) , ierr )
    
    IF ( ( use_package( io_form ) == IO_GRIB1 ) .OR. &
         ( use_package( io_form ) == IO_GRIB2 ) ) THEN
      CALL wrf_put_dom_ti_char ( fid, 'PROGRAM_NAME', TRIM(program_name) , ierr )
    ENDIF
    CALL nl_get_start_year(grid%id,start_year)
    CALL nl_get_start_month(grid%id,start_month)
    CALL nl_get_start_day(grid%id,start_day)
    CALL nl_get_start_hour(grid%id,start_hour)
    CALL nl_get_start_minute(grid%id,start_minute)
    CALL nl_get_start_second(grid%id,start_second)
    WRITE ( start_date , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
            start_year,start_month,start_day,start_hour,start_minute,start_second
    CALL wrf_put_dom_ti_char ( fid , 'START_DATE', TRIM(start_date) , ierr )
    IF ( switch .EQ. input_only) THEN
       CALL wrf_put_dom_ti_char ( fid , 'SIMULATION_START_DATE', TRIM(start_date) , ierr )
    ELSE
       CALL nl_get_simulation_start_year   ( 1, simulation_start_year   )
       CALL nl_get_simulation_start_month  ( 1, simulation_start_month  )
       CALL nl_get_simulation_start_day    ( 1, simulation_start_day    )
       CALL nl_get_simulation_start_hour   ( 1, simulation_start_hour   )
       CALL nl_get_simulation_start_minute ( 1, simulation_start_minute )
       CALL nl_get_simulation_start_second ( 1, simulation_start_second )
       WRITE ( simulation_start_date , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
               simulation_start_year,simulation_start_month,simulation_start_day,&
               simulation_start_hour,simulation_start_minute,simulation_start_second
       CALL wrf_put_dom_ti_char ( fid , 'SIMULATION_START_DATE', TRIM(simulation_start_date) , ierr )
    END IF

    IF ( switch .EQ. restart_only ) THEN
       
       ibuf(1) = MAX_WRF_ALARMS
       CALL wrf_put_dom_ti_integer( fid, 'MAX_WRF_ALARMS', ibuf, 1, ierr )
       curtime = domain_get_current_time( grid )
       DO i = 1, MAX_WRF_ALARMS
         IF ( grid%alarms_created(i)  ) THEN
           IF ( i .LT. 10 ) THEN
             write(alarmname,'("WRF_ALARM_ISRINGING_0",i1)')i
           ELSE
             write(alarmname,'("WRF_ALARM_ISRINGING_",i2)')i
           ENDIF
           IF ( WRFU_AlarmIsRinging( grid%alarms( i ), rc=rc ) ) THEN
             iring = 1
           ELSE
             iring = 0
           ENDIF
           CALL wrf_put_dom_ti_integer( fid, TRIM(alarmname), iring, 1, ierr )

           CALL WRFU_AlarmGet( grid%alarms(i),PrevRingTime=ringTime,RingInterval=interval,rc=rc)


           
           tmpinterval = curtime - ringTime

           IF ( i .LT. 10 ) THEN
             write(alarmname,'("WRF_ALARM_SECS_TIL_NEXT_RING_0",i1)')i
           ELSE
             write(alarmname,'("WRF_ALARM_SECS_TIL_NEXT_RING_",i2)')i
           ENDIF

           if(wrf_dm_on_monitor()) then
             CALL WRFU_TimeIntervalGet(interval,S=seconds)
             CALL WRFU_TimeIntervalGet(tmpinterval,S=seconds2)

           endif

           call wrf_dm_bcast_integer(seconds, 1)
           call wrf_dm_bcast_integer(seconds2, 1)

           IF ( seconds .GE. 1700000000 .OR. seconds .LE. -1700000000 ) THEN   
             CALL wrf_put_dom_ti_integer( fid, TRIM(alarmname), seconds, 1, ierr )
           ELSE
             CALL wrf_put_dom_ti_integer( fid, TRIM(alarmname), seconds-seconds2, 1, ierr )
           ENDIF

         ENDIF
       ENDDO
    ENDIF

    ibuf(1) = config_flags%e_we - config_flags%s_we + 1
    CALL wrf_put_dom_ti_integer ( fid , 'WEST-EAST_GRID_DIMENSION' ,  ibuf , 1 , ierr )

    ibuf(1) = config_flags%e_sn - config_flags%s_sn + 1
    CALL wrf_put_dom_ti_integer ( fid , 'SOUTH-NORTH_GRID_DIMENSION' , ibuf , 1 , ierr )

    IF ( program_name(1:5) .EQ. 'TC_EM' ) THEN
       ibuf(1) = config_flags%num_metgrid_levels
       CALL wrf_put_dom_ti_integer ( fid , 'BOTTOM-TOP_GRID_DIMENSION' , ibuf , 1 , ierr )
    ELSE
       ibuf(1) = config_flags%e_vert - config_flags%s_vert + 1
       CALL wrf_put_dom_ti_integer ( fid , 'BOTTOM-TOP_GRID_DIMENSION' , ibuf , 1 , ierr )
    END IF


    CALL wrf_put_dom_ti_real ( fid , 'WBD0' , grid%wbd0 , 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'SBD0' , grid%sbd0 , 1 , ierr )






          CALL wrf_put_dom_ti_char ( fid , 'GRIDTYPE',  'E' , ierr )


    ibuf(1) = diff_opt
    CALL wrf_put_dom_ti_integer ( fid , 'DIFF_OPT' ,  ibuf , 1 , ierr )
    ibuf(1) = km_opt
    CALL wrf_put_dom_ti_integer ( fid , 'KM_OPT' ,  ibuf , 1 , ierr )
    ibuf(1) = damp_opt
    CALL wrf_put_dom_ti_integer ( fid , 'DAMP_OPT' ,  ibuf , 1 , ierr )
    rbuf(1) = dampcoef
    CALL wrf_put_dom_ti_real    ( fid , 'DAMPCOEF' ,  rbuf , 1 , ierr )
    rbuf(1) = khdif
    CALL wrf_put_dom_ti_real    ( fid , 'KHDIF' ,  rbuf , 1 , ierr )
    rbuf(1) = kvdif
    CALL wrf_put_dom_ti_real    ( fid , 'KVDIF' ,  rbuf , 1 , ierr )
    ibuf(1) = mp_physics
    CALL wrf_put_dom_ti_integer ( fid , 'MP_PHYSICS' ,  ibuf , 1 , ierr )
    ibuf(1) = ra_lw_physics
    CALL wrf_put_dom_ti_integer ( fid , 'RA_LW_PHYSICS' ,  ibuf , 1 , ierr )
    ibuf(1) = ra_sw_physics
    CALL wrf_put_dom_ti_integer ( fid , 'RA_SW_PHYSICS' ,  ibuf , 1 , ierr )
    ibuf(1) = sf_sfclay_physics
    CALL wrf_put_dom_ti_integer ( fid , 'SF_SFCLAY_PHYSICS' ,  ibuf , 1 , ierr )
    ibuf(1) = sf_surface_physics
    CALL wrf_put_dom_ti_integer ( fid , 'SF_SURFACE_PHYSICS' ,  ibuf , 1 , ierr )
    ibuf(1) = bl_pbl_physics
    CALL wrf_put_dom_ti_integer ( fid , 'BL_PBL_PHYSICS' ,  ibuf , 1 , ierr )
    ibuf(1) = cu_physics
    CALL wrf_put_dom_ti_integer ( fid , 'CU_PHYSICS' ,  ibuf , 1 , ierr )
    ibuf(1) = sf_lake_physics
    CALL wrf_put_dom_ti_integer ( fid , 'SF_LAKE_PHYSICS' ,  ibuf , 1 , ierr )

    
    IF ( ( use_package( io_form ) == IO_NETCDF ) .OR. &
         ( use_package( io_form ) == IO_PHDF5  ) .OR. &
         ( use_package( io_form ) == IO_PIO    ) .OR. &
         ( use_package( io_form ) == IO_PNETCDF ) ) THEN
      CALL wrf_put_dom_ti_integer ( fid, 'SURFACE_INPUT_SOURCE', surface_input_source , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid, 'SST_UPDATE', sst_update , 1 , ierr )

      IF ( switch .EQ. history_only ) THEN
      CALL wrf_put_dom_ti_integer ( fid, 'SF_URBAN_PHYSICS', sf_urban_physics , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid, 'SHCU_PHYSICS',     config_flags%shcu_physics , 1 , ierr ) 
      CALL wrf_put_dom_ti_integer ( fid, 'MFSHCONV',         config_flags%mfshconv , 1 , ierr ) 
      CALL wrf_put_dom_ti_integer ( fid, 'FEEDBACK', feedback , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid, 'SMOOTH_OPTION', smooth_option , 1 , ierr )
      CALL wrf_put_dom_ti_real    ( fid, 'SWRAD_SCAT', swrad_scat , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid, 'W_DAMPING', w_damping , 1 , ierr )


      call wrf_put_dom_ti_real    ( fid, 'RADT', radt, 1 , ierr)
      call wrf_put_dom_ti_real    ( fid, 'BLDT', bldt, 1 , ierr)
      call wrf_put_dom_ti_real    ( fid, 'CUDT', cudt, 1 , ierr)
      call wrf_put_dom_ti_integer ( fid, 'AER_OPT'        , aer_opt        , 1 , ierr)
      call wrf_put_dom_ti_integer ( fid, 'SWINT_OPT'      , swint_opt      , 1 , ierr)
      call wrf_put_dom_ti_integer ( fid, 'AER_TYPE'       , aer_type       , 1 , ierr )
      call wrf_put_dom_ti_integer ( fid, 'AER_AOD550_OPT' , aer_aod550_opt , 1 , ierr )
      call wrf_put_dom_ti_integer ( fid, 'AER_ANGEXP_OPT' , aer_angexp_opt , 1 , ierr )
      call wrf_put_dom_ti_integer ( fid, 'AER_SSA_OPT'    , aer_ssa_opt    , 1 , ierr )
      call wrf_put_dom_ti_integer ( fid, 'AER_ASY_OPT'    , aer_asy_opt    , 1 , ierr )
      call wrf_put_dom_ti_real    ( fid, 'AER_AOD550_VAL' , aer_aod550_val , 1 , ierr )
      call wrf_put_dom_ti_real    ( fid, 'AER_ANGEXP_VAL' , aer_angexp_val , 1 , ierr )
      call wrf_put_dom_ti_real    ( fid, 'AER_SSA_VAL'    , aer_ssa_val    , 1 , ierr )
      call wrf_put_dom_ti_real    ( fid, 'AER_ASY_VAL'    , aer_asy_val    , 1 , ierr )


      IF ( sf_surface_physics == 4 ) THEN
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_SFC',     config_flags%opt_sfc     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'DVEG',        config_flags%dveg        , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_CRS',     config_flags%opt_crs     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_BTR',     config_flags%opt_btr     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_RUN',     config_flags%opt_run     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_FRZ',     config_flags%opt_frz     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_INF',     config_flags%opt_inf     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_RAD',     config_flags%opt_rad     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_ALB',     config_flags%opt_alb     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_SNF',     config_flags%opt_snf     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_TBOT',    config_flags%opt_tbot    , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_STC',     config_flags%opt_stc     , 1 , ierr )
      ENDIF


      CALL wrf_put_dom_ti_integer   ( fid, 'DFI_OPT',          config_flags%dfi_opt      , 1 , ierr ) 

      ENDIF 

    ENDIF





    ibuf(1) = MAX(ips,ids)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = ids
    CALL wrf_put_dom_ti_integer ( fid , 'WEST-EAST_PATCH_START_UNSTAG' ,  ibuf , 1 , ierr )
    ibuf(1) = MIN(ipe,ide-1)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = ide - 1
    CALL wrf_put_dom_ti_integer ( fid , 'WEST-EAST_PATCH_END_UNSTAG' ,  ibuf , 1 , ierr )
    ibuf(1) = MAX(ips,ids)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = ids
    CALL wrf_put_dom_ti_integer ( fid , 'WEST-EAST_PATCH_START_STAG' ,  ibuf , 1 , ierr )
    ibuf(1) = MIN(ipe,ide)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = ide
    CALL wrf_put_dom_ti_integer ( fid , 'WEST-EAST_PATCH_END_STAG' ,  ibuf , 1 , ierr )
    ibuf(1) = MAX(jps,jds)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = jds
    CALL wrf_put_dom_ti_integer ( fid , 'SOUTH-NORTH_PATCH_START_UNSTAG' ,  ibuf , 1 , ierr )
    ibuf(1) = MIN(jpe,jde-1)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = jde - 1
    CALL wrf_put_dom_ti_integer ( fid , 'SOUTH-NORTH_PATCH_END_UNSTAG' ,  ibuf , 1 , ierr )
    ibuf(1) = MAX(jps,jds)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = jds
    CALL wrf_put_dom_ti_integer ( fid , 'SOUTH-NORTH_PATCH_START_STAG' ,  ibuf , 1 , ierr )
    ibuf(1) = MIN(jpe,jde)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = jde
    CALL wrf_put_dom_ti_integer ( fid , 'SOUTH-NORTH_PATCH_END_STAG' ,  ibuf , 1 , ierr )

    ibuf(1) = MAX(kps,kds)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = kds
    CALL wrf_put_dom_ti_integer ( fid , 'BOTTOM-TOP_PATCH_START_UNSTAG' ,  ibuf , 1 , ierr )
    ibuf(1) = MIN(kpe,kde-1)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = kde - 1
    CALL wrf_put_dom_ti_integer ( fid , 'BOTTOM-TOP_PATCH_END_UNSTAG' ,  ibuf , 1 , ierr )
    ibuf(1) = MAX(kps,kds)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = kds
    CALL wrf_put_dom_ti_integer ( fid , 'BOTTOM-TOP_PATCH_START_STAG' ,  ibuf , 1 , ierr )
    ibuf(1) = MIN(kpe,kde)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = kde
    CALL wrf_put_dom_ti_integer ( fid , 'BOTTOM-TOP_PATCH_END_STAG' ,  ibuf , 1 , ierr )



    CALL wrf_put_dom_ti_real ( fid , 'DX' ,  config_flags%dx , 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'DY' ,  config_flags%dy , 1 , ierr )

    CALL wrf_put_dom_ti_real ( fid , 'DT' ,  grid%dt , 1 , ierr )

    CALL wrf_put_dom_ti_real ( fid , 'CEN_LAT' ,  config_flags%cen_lat , 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'CEN_LON' ,  config_flags%cen_lon , 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'TRUELAT1',  config_flags%truelat1, 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'TRUELAT2',  config_flags%truelat2, 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'MOAD_CEN_LAT',  config_flags%moad_cen_lat, 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'STAND_LON',  config_flags%stand_lon, 1 , ierr )
    IF ( switch .NE. boundary_only .AND. switch .NE. auxinput9_only .AND. switch .NE. auxinput10_only ) THEN
      CALL wrf_put_dom_ti_real ( fid , 'GMT' ,  config_flags%gmt , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'JULYR' ,  config_flags%julyr , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'JULDAY' ,  config_flags%julday , 1 , ierr )
    ENDIF

    CALL wrf_put_dom_ti_integer ( fid , 'MAP_PROJ' ,  config_flags%map_proj , 1 , ierr )
    IF      ( config_flags%map_proj .EQ.   0 ) THEN
       CALL wrf_put_dom_ti_char ( fid , 'MAP_PROJ_CHAR' ,  "Cartesian"               , ierr )
    ELSE IF ( config_flags%map_proj .EQ.   1 ) THEN
       CALL wrf_put_dom_ti_char ( fid , 'MAP_PROJ_CHAR' ,  "Lambert Conformal"       , ierr )
    ELSE IF ( config_flags%map_proj .EQ.   2 ) THEN
       CALL wrf_put_dom_ti_char ( fid , 'MAP_PROJ_CHAR' ,  "Polar Stereographic"     , ierr )
    ELSE IF ( config_flags%map_proj .EQ.   3 ) THEN
       CALL wrf_put_dom_ti_char ( fid , 'MAP_PROJ_CHAR' ,  "Mercator"                , ierr )
    ELSE IF ( config_flags%map_proj .EQ.   6 ) THEN
       CALL wrf_put_dom_ti_char ( fid , 'MAP_PROJ_CHAR' ,  "Cylindrical Equidistant" , ierr )
    ELSE IF ( config_flags%map_proj .EQ. 203 ) THEN
       CALL wrf_put_dom_ti_char ( fid , 'MAP_PROJ_CHAR' ,  "NMM Rotated Lat Lon"     , ierr )
    END IF

    IF(MMINLU(1:4) == 'USGS') THEN
      MMINLU='USGS'
    ENDIF
	
    IF(MMINLU(1:1) .EQ. " ")THEN
       CALL wrf_put_dom_ti_char ( fid , 'MMINLU',  "    "       , ierr )
    ELSE
       CALL wrf_put_dom_ti_char ( fid , 'MMINLU',  TRIM(mminlu) , ierr )
    END IF
    call wrf_put_dom_ti_integer ( fid , 'NUM_LAND_CAT', config_flags%num_land_cat, 1, ierr)
    CALL wrf_put_dom_ti_integer ( fid , 'ISWATER' ,  iswater , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( fid , 'ISLAKE' ,   islake , 1 , ierr )

    CALL wrf_put_dom_ti_integer ( fid , 'ISICE' ,  config_flags%isice , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( fid , 'ISURBAN' ,  config_flags%isurban , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( fid , 'ISOILWATER' ,  config_flags%isoilwater , 1 , ierr )


    CALL wrf_put_dom_ti_integer ( fid , 'I_PARENT_START' ,  grid%i_parent_start  , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( fid , 'J_PARENT_START' ,  grid%j_parent_start  , 1 , ierr )

    IF ( switch .EQ. boundary_only ) THEN
        CALL WRFU_TimeIntervalSet( bdy_increment, S=NINT(config_flags%bdyfrq),rc=rc)
        next_time = currentTime + bdy_increment
        CALL wrf_timetoa ( next_time, next_datestr )
        CALL wrf_put_dom_td_char ( fid , 'THISBDYTIME' ,  current_date(1:19), current_date(1:19), ierr )
        CALL wrf_put_dom_td_char ( fid , 'NEXTBDYTIME' ,  current_date(1:19), next_datestr(1:19), ierr )
    ENDIF

    
    IF ( use_package( io_form ) == IO_GRIB2 ) THEN
      CALL wrf_put_dom_ti_integer ( fid , 'BACKGROUND_PROC_ID' , config_flags%background_proc_id , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'FORECAST_PROC_ID' , config_flags%forecast_proc_id , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'PRODUCTION_STATUS' , config_flags%production_status , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'COMPRESSION' , config_flags%compression , 1 , ierr )
    ENDIF


    IF ( (first_input   .LE. switch .AND. switch .LE. last_input) .OR. &
         (first_history .LE. switch .AND. switch .LE. last_history ) .OR. &
          switch .EQ. restart_only    ) THEN
      newswitch = switch 
      p => grid%head_statevars%next
      DO WHILE ( ASSOCIATED( p ) ) 
        IF ( p%ProcOrient .NE. 'X' .AND. p%ProcOrient .NE. 'Y' ) THEN   
          IF ( p%Ndim .EQ. 0 ) THEN
            IF ((p%Restart.AND.switch.EQ.restart_only).OR.on_stream(p%streams,newswitch)) THEN
              IF ( in_use_for_config(grid%id,TRIM(p%VarName)) ) THEN
                dname = p%DataName
                IF (p%Ntl.GT.0.AND.switch.NE.restart_only)dname=dname(1:len(TRIM(dname))-2)
                memord = p%MemoryOrder


                IF      ( p%Type .EQ. 'r' ) THEN
                  CALL wrf_ext_write_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(p%DataName)        , & 
                                    p%rfield_0d             , & 
                                    WRF_FLOAT               , & 
                                    grid                    , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    dryrun                  , & 
                                    '0'                     , & 
                                    ''                      , & 
                                    ''                      , & 
                                    ''                      , & 
                                    ''                      , & 
                                    TRIM(p%Description)     , & 
                                    TRIM(p%Units)           , & 
                    "output_wrf.b" // ' writing 0d real ' // TRIM(p%VarName)     , & 
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    ierr )
                ELSE IF ( p%Type .EQ. 'd' ) THEN
                  CALL wrf_ext_write_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(p%DataName)        , & 
                                    p%dfield_0d             , & 
                                    WRF_DOUBLE              , & 
                                    grid                    , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    dryrun                  , & 
                                    '0'                     , & 
                                    ''                      , & 
                                    ''                      , & 
                                    ''                      , & 
                                    ''                      , & 
                                    TRIM(p%Description)     , & 
                                    TRIM(p%Units)           , & 
                    "output_wrf.b" // ' writing 0d double ' // TRIM(p%VarName)     , & 
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    ierr )
                ELSE IF ( p%Type .EQ. 'i' ) THEN
                  CALL wrf_ext_write_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(p%DataName)        , & 
                                    p%ifield_0d             , & 
                                    WRF_INTEGER             , & 
                                    grid                    , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    dryrun                  , & 
                                    '0'                     , & 
                                    ''                      , & 
                                    ''                      , & 
                                    ''                      , & 
                                    ''                      , & 
                                    TRIM(p%Description)     , & 
                                    TRIM(p%Units)           , & 
                    "output_wrf.b" // ' writing 0d integer ' // TRIM(p%VarName)     , & 
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    ierr )
                ELSE IF ( p%Type .EQ. 'l' ) THEN
                  CALL wrf_ext_write_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(p%DataName)        , & 
                                    p%lfield_0d             , & 
                                    WRF_LOGICAL             , & 
                                    grid                    , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    dryrun                  , & 
                                    '0'                     , & 
                                    ''                      , & 
                                    ''                      , & 
                                    ''                      , & 
                                    ''                      , & 
                                    TRIM(p%Description)     , & 
                                    TRIM(p%Units)           , & 
                    "output_wrf.b" // ' writing 0d logical ' // TRIM(p%VarName)     , & 
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    ierr )
                ENDIF
              ENDIF
            ENDIF
          ELSE IF ( p%Ndim .EQ. 1 ) THEN
            IF ((p%Restart.AND.switch.EQ.restart_only).OR.on_stream(p%streams,newswitch)) THEN
              IF ( in_use_for_config(grid%id,TRIM(p%VarName)) ) THEN
                IF (switch.EQ.restart_only.OR.p%Ntl/100.EQ.mod(p%Ntl,100)) THEN
                  dname = p%DataName
                  IF (p%Ntl.GT.0.AND.switch.NE.restart_only)dname=dname(1:len(TRIM(dname))-2)
                  memord = p%MemoryOrder
                  IF      ( p%Type .EQ. 'r' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%rfield_1d             , & 
                                    WRF_FLOAT               , & 
                                    grid                    , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    dryrun                  , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                                    TRIM(p%dimname1)        , & 
                                    TRIM(p%dimname2)        , & 
                                    TRIM(p%dimname3)        , & 
                                    TRIM(p%Description)     , & 
                                    TRIM(p%Units)           , & 
                     "output_wrf.b" // ' writing 1d real ' // TRIM(p%VarName)     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'd' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%dfield_1d             , & 
                                    WRF_DOUBLE              , & 
                                    grid                    , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    dryrun                  , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                                    TRIM(p%dimname1)        , & 
                                    TRIM(p%dimname2)        , & 
                                    TRIM(p%dimname3)        , & 
                                    TRIM(p%Description)     , & 
                                    TRIM(p%Units)           , & 
                     "output_wrf.b" // ' writing 1d double ' // TRIM(p%VarName)     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'i' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%ifield_1d             , & 
                                    WRF_INTEGER             , & 
                                    grid                    , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    dryrun                  , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                                    TRIM(p%dimname1)        , & 
                                    TRIM(p%dimname2)        , & 
                                    TRIM(p%dimname3)        , & 
                                    TRIM(p%Description)     , & 
                                    TRIM(p%Units)           , & 
                     "output_wrf.b" // ' writing 1d integer ' // TRIM(p%VarName)     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'l' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%lfield_1d             , & 
                                    WRF_LOGICAL             , & 
                                    grid                    , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    dryrun                  , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                                    TRIM(p%dimname1)        , & 
                                    TRIM(p%dimname2)        , & 
                                    TRIM(p%dimname3)        , & 
                                    TRIM(p%Description)     , & 
                                    TRIM(p%Units)           , & 
                     "output_wrf.b" // ' writing 1d logical ' // TRIM(p%VarName)     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE IF ( p%Ndim .EQ. 2 ) THEN
            IF ((p%Restart.AND.switch.EQ.restart_only).OR.on_stream(p%streams,newswitch)) THEN
              IF ( in_use_for_config(grid%id,TRIM(p%VarName)) .AND.  &
                   ( .NOT. p%subgrid_x .OR. (p%subgrid_x .AND. grid%sr_x .GT. 0) ) .AND. &
                   ( .NOT. p%subgrid_y .OR. (p%subgrid_y .AND. grid%sr_y .GT. 0) )       &
                 ) THEN
                IF (switch.EQ.restart_only.OR.p%Ntl/100.EQ.mod(p%Ntl,100)) THEN
                  dname = p%DataName
                  IF (p%Ntl.GT.0.AND.switch.NE.restart_only)dname=dname(1:len(TRIM(dname))-2)
                  memord = p%MemoryOrder
                  IF      ( p%Type .EQ. 'r' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%rfield_2d             , & 
                                    WRF_FLOAT               , & 
                                    grid                    , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    dryrun                  , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                                    TRIM(p%dimname1)        , & 
                                    TRIM(p%dimname2)        , & 
                                    TRIM(p%dimname3)        , & 
                                    TRIM(p%Description)     , & 
                                    TRIM(p%Units)           , & 
                     "output_wrf.b" // ' writing 2d real ' // TRIM(p%VarName)     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'd' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%dfield_2d             , & 
                                    WRF_DOUBLE              , & 
                                    grid                    , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    dryrun                  , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                                    TRIM(p%dimname1)        , & 
                                    TRIM(p%dimname2)        , & 
                                    TRIM(p%dimname3)        , & 
                                    TRIM(p%Description)     , & 
                                    TRIM(p%Units)           , & 
                     "output_wrf.b" // ' writing 2d double ' // TRIM(p%VarName)     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'i' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%ifield_2d             , & 
                                    WRF_INTEGER             , & 
                                    grid                    , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    dryrun                  , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                                    TRIM(p%dimname1)        , & 
                                    TRIM(p%dimname2)        , & 
                                    TRIM(p%dimname3)        , & 
                                    TRIM(p%Description)     , & 
                                    TRIM(p%Units)           , & 
                     "output_wrf.b" // ' writing 2d integer ' // TRIM(p%VarName)     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'l' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%lfield_2d             , & 
                                    WRF_LOGICAL             , & 
                                    grid                    , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    dryrun                  , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                                    TRIM(p%dimname1)        , & 
                                    TRIM(p%dimname2)        , & 
                                    TRIM(p%dimname3)        , & 
                                    TRIM(p%Description)     , & 
                                    TRIM(p%Units)           , & 
                     "output_wrf.b" // ' writing 2d logical ' // TRIM(p%VarName)     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE IF ( p%Ndim .EQ. 3 ) THEN
            IF ((p%Restart.AND.switch.EQ.restart_only).OR.on_stream(p%streams,newswitch)) THEN
              IF ( in_use_for_config(grid%id,TRIM(p%VarName)) .AND.  &
                   ( .NOT. p%subgrid_x .OR. (p%subgrid_x .AND. grid%sr_x .GT. 0) ) .AND. &
                   ( .NOT. p%subgrid_y .OR. (p%subgrid_y .AND. grid%sr_y .GT. 0) )       &
                 ) THEN
                IF (switch.EQ.restart_only.OR.p%Ntl/100.EQ.mod(p%Ntl,100)) THEN
                  dname = p%DataName
                  IF (p%Ntl.GT.0.AND.switch.NE.restart_only)dname=dname(1:len(TRIM(dname))-2)
                  memord = p%MemoryOrder
                  IF      ( p%Type .EQ. 'r' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%rfield_3d             , & 
                                    WRF_FLOAT               , & 
                                    grid                    , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    dryrun                  , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                                    TRIM(p%dimname1)        , & 
                                    TRIM(p%dimname2)        , & 
                                    TRIM(p%dimname3)        , & 
                                    TRIM(p%Description)     , & 
                                    TRIM(p%Units)           , & 
                     "output_wrf.b" // ' writing 3d real ' // TRIM(p%VarName)     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'd' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%dfield_3d             , & 
                                    WRF_DOUBLE              , & 
                                    grid                    , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    dryrun                  , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                                    TRIM(p%dimname1)        , & 
                                    TRIM(p%dimname2)        , & 
                                    TRIM(p%dimname3)        , & 
                                    TRIM(p%Description)     , & 
                                    TRIM(p%Units)           , & 
                     "output_wrf.b" // ' writing 3d double ' // TRIM(p%VarName)     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'i' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(dname)             , & 
                                    p%ifield_3d             , & 
                                    WRF_INTEGER             , & 
                                    grid                    , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    dryrun                  , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                                    TRIM(p%dimname1)        , & 
                                    TRIM(p%dimname2)        , & 
                                    TRIM(p%dimname3)        , & 
                                    TRIM(p%Description)     , & 
                                    TRIM(p%Units)           , & 
                     "output_wrf.b" // ' writing 3d integer ' // TRIM(p%VarName)     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )

                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE IF ( p%Ndim .EQ. 4 .AND. p%scalar_array ) THEN
              IF (switch.EQ.restart_only.OR.p%Ntl/100.EQ.mod(p%Ntl,100)) THEN




              DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                IF ((p%Restart.AND.switch.EQ.restart_only).OR.on_stream(p%streams_table(grid%id,itrace)%stream,newswitch)) THEN
                  dname = p%dname_table( grid%id, itrace )
                  IF (p%Ntl.GT.0.AND.switch.NE.restart_only)dname=dname(1:len(TRIM(dname))-2)
                  memord = p%MemoryOrder
                  IF      ( p%Type .EQ. 'r' ) THEN
                    CALL wrf_ext_write_field_arr (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(p%dname_table( grid%id, itrace ))         , & 
                                    p%rfield_4d             , & 
                                    itrace, 1, 1, 1         , & 
                                    1, 1, 1                 , & 
                                    4               , &
                                    WRF_FLOAT               , & 
                                    grid                    , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    dryrun                  , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                                    TRIM(p%dimname1)        , & 
                                    TRIM(p%dimname2)        , & 
                                    TRIM(p%dimname3)        , & 
                                    TRIM(p%desc_table( grid%id, itrace))     , & 
                                    TRIM(p%units_table( grid%id, itrace))           , & 
                     "output_wrf.b" // ' writing 4d real ' // TRIM(p%dname_table(grid%id,itrace))     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'd' ) THEN
                    CALL wrf_ext_write_field_arr (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(p%dname_table( grid%id, itrace ))         , & 
                                    p%dfield_4d             , & 
                                    itrace, 1, 1, 1         , & 
                                    1, 1, 1                 , & 
                                    8               , &
                                    WRF_DOUBLE              , & 
                                    grid                    , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    dryrun                  , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                                    TRIM(p%dimname1)        , & 
                                    TRIM(p%dimname2)        , & 
                                    TRIM(p%dimname3)        , & 
                                    TRIM(p%desc_table( grid%id, itrace))     , & 
                                    TRIM(p%units_table( grid%id, itrace))           , & 
                     "output_wrf.b" // ' writing 4d double ' // TRIM(p%dname_table(grid%id,itrace))     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'i' ) THEN
                    CALL wrf_ext_write_field_arr (  &
                                    fid                     , & 
                                    current_date(1:19)      , & 
                                    TRIM(p%dname_table( grid%id, itrace ))         , & 
                                    p%ifield_4d             , & 
                                    itrace, 1, 1, 1         , & 
                                    1, 1, 1                 , & 
                                    4               , &
                                    WRF_INTEGER             , & 
                                    grid                    , & 
                                    grid%domdesc            , & 
                                    grid%bdy_mask           , & 
                                    dryrun                  , & 
                                    TRIM(memord)            , & 
                                    TRIM(p%Stagger)         , & 
                                    TRIM(p%dimname1)        , & 
                                    TRIM(p%dimname2)        , & 
                                    TRIM(p%dimname3)        , & 
                                    TRIM(p%desc_table( grid%id, itrace))     , & 
                                    TRIM(p%units_table( grid%id, itrace))           , & 
                     "output_wrf.b" // ' writing 4d integer ' // TRIM(p%dname_table(grid%id,itrace))     , & 
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ENDIF
                ENDIF
              ENDDO  
            ENDIF  
          ENDIF
        ENDIF
        p => p%next
      ENDDO
    ELSE
       IF ( switch .EQ. boundary_only ) THEN
         CALL wrf_debug ( 300 , 'output_wrf: calling code in wrf_bdyout.inc' )
         CALL wrf_bdyout( fid , grid , config_flags, switch, dryrun,  ierr )
       ENDIF
    ENDIF

    IF ( switch .EQ. history_only ) THEN
      IF (adjust) THEN
        current_date = current_date_save
      ENDIF
    ENDIF


    IF ( .NOT. dryrun ) THEN
       CALL wrf_debug ( 300 , 'output_wrf: calling wrf_iosync ' )
       CALL wrf_iosync ( fid , ierr )
       CALL wrf_debug ( 300 , 'output_wrf: back from wrf_iosync ' )

       
       
       
       
       
       
       
       
       

       IF ( switch .EQ. history_only .AND. config_flags%output_ready_flag ) THEN
          WRITE ( wrf_err_message , FMT='(I2.2)' ) grid%id
          CALL get_nio_tasks_in_group ( nio_tasks_per_group )
          IF ( nio_tasks_per_group .EQ. 0 ) THEN
             OPEN ( UNIT   = 99 , &
                    FILE   = 'wrfoutReady_d' // wrf_err_message(1:2) // '_' // TRIM(current_date) , &
                    STATUS = 'UNKNOWN' , &
                    ACCESS = 'SEQUENTIAL' )
             CLOSE (99)
          ENDIF
       ENDIF
    ENDIF

    WRITE(wrf_err_message,*)'output_wrf: end, fid = ',fid
    CALL wrf_debug( 300 , wrf_err_message )

    RETURN
  END SUBROUTINE output_wrf

  SUBROUTINE traverse_statevars_debug (s,l)
    USE module_domain
    IMPLICIT NONE
    character*(*)s
    integer l, itrace
    TYPE( fieldlist ), POINTER :: p
    p => head_grid%head_statevars%next

    DO WHILE ( ASSOCIATED( p ) )








      p => p%next
    ENDDO
    RETURN
  END SUBROUTINE traverse_statevars_debug

