MODULE module_si_io_nmm

   USE module_optional_input



   IMPLICIT NONE


















  integer, parameter, public  :: i_byte  = selected_int_kind(1)      
  integer, parameter, public  :: i_short = selected_int_kind(4)      
  integer, parameter, public  :: i_long  = selected_int_kind(8)      
  integer, parameter, private :: llong_t = selected_int_kind(16)     
  integer, parameter, public  :: i_llong = max( llong_t, i_long )


  integer, parameter, public :: num_bytes_for_i_byte  = 1
  integer, parameter, public :: num_bytes_for_i_short = 2
  integer, parameter, public :: num_bytes_for_i_long  = 4
  integer, parameter, public :: num_bytes_for_i_llong = 8


  integer, parameter, private :: num_i_kinds = 4
  integer, parameter, dimension( num_i_kinds ), private :: integer_types = (/ &
       i_byte, i_short, i_long,  i_llong  /)
  integer, parameter, dimension( num_i_kinds ), private :: integer_byte_sizes = (/ &
       num_bytes_for_i_byte, num_bytes_for_i_short, &
       num_bytes_for_i_long, num_bytes_for_i_llong  /)



  integer, parameter, private :: default_integer = 2  
                                                      
                                                      
                                                      
  integer, parameter, public  :: i_kind = integer_types( default_integer )
  integer, parameter, public  :: num_bytes_for_i_kind = &
       integer_byte_sizes( default_integer )





  integer, parameter, public  :: r_single = selected_real_kind(6)  
  integer, parameter, public  :: r_double = selected_real_kind(15) 
  integer, parameter, private :: quad_t   = selected_real_kind(20) 
  integer, parameter, public  :: r_quad   = max( quad_t, r_double )


  integer, parameter, public :: num_bytes_for_r_single = 4
  integer, parameter, public :: num_bytes_for_r_double = 8
  integer, parameter, public :: num_bytes_for_r_quad   = 16


  integer, parameter, private :: num_r_kinds = 3
  integer, parameter, dimension( num_r_kinds ), private :: real_kinds = (/ &
       r_single, r_double, r_quad    /)
  integer, parameter, dimension( num_r_kinds ), private :: real_byte_sizes = (/ &
       num_bytes_for_r_single, num_bytes_for_r_double, &
       num_bytes_for_r_quad    /)



  integer, parameter, private :: default_real = 2  
                                                   


      

      REAL , DIMENSION(:,:,:) , ALLOCATABLE :: u_input , v_input , &
                                               q_input , t_input

      

      REAL , DIMENSION(:,:,:) , ALLOCATABLE :: landuse_frac_input , &
                                               soil_top_cat_input , &
                                               soil_bot_cat_input

      REAL, ALLOCATABLE:: htm_in(:,:,:),vtm_in(:,:,:)

      

      REAL , DIMENSION(:,:)   , ALLOCATABLE,save :: soilt010_input , soilt040_input , &
                                               soilt100_input , soilt200_input , &
                                               soilm010_input , soilm040_input , &
                                               soilm100_input , soilm200_input , &
                                               psfc_in,pmsl

      REAL , DIMENSION(:,:)   , ALLOCATABLE :: lat_wind, lon_wind 

      REAL , DIMENSION(:)     , ALLOCATABLE :: DETA_in, AETA_in, ETAX_in
      REAL , DIMENSION(:)     , ALLOCATABLE :: DETA1_in, AETA1_in, ETA1_in
      REAL , DIMENSION(:)     , ALLOCATABLE :: DETA2_in, AETA2_in, ETA2_in, DFL_in

      REAL , DIMENSION(:,:,:), ALLOCATABLE,save :: st_inputx , sm_inputx, sw_inputx

      

      REAL,DIMENSION(:,:),ALLOCATABLE :: dum2d
      INTEGER,DIMENSION(:,:),ALLOCATABLE :: idum2d
      REAL,DIMENSION(:,:,:),ALLOCATABLE :: dum3d

      LOGICAL , SAVE :: first_time_in = .TRUE.

      INTEGER :: flag_soilt010 , flag_soilt100 , flag_soilt200 , &
        	 flag_soilm010 , flag_soilm100 , flag_soilm200




      INTEGER, PARAMETER          :: var_maxdims = 5
      INTEGER, PARAMETER          :: max_staggers_xy_new = 4
      INTEGER, PARAMETER          :: max_staggers_xy_old = 3
      INTEGER, PARAMETER          :: max_staggers_z = 2
      INTEGER, PARAMETER          :: max_standard_lats = 4
      INTEGER, PARAMETER          :: max_standard_lons = 4  
      INTEGER, PARAMETER          :: max_fg_variables = 200
      INTEGER, PARAMETER          :: max_vertical_levels = 2000























      TYPE wrf_var_metadata
	 CHARACTER (LEN=8)         :: name 
	 CHARACTER (LEN=16)        :: units
	 CHARACTER (LEN=80)        :: description
	 INTEGER                   :: domain_id
	 INTEGER                   :: ndim
	 INTEGER                   :: dim_val (var_maxdims)
	 CHARACTER(LEN=4)          :: dim_desc (var_maxdims)
	 INTEGER                   :: start_index(var_maxdims)
	 INTEGER                   :: stop_index(var_maxdims)
	 INTEGER                   :: h_stagger_index
	 INTEGER                   :: v_stagger_index
	 CHARACTER(LEN=8)          :: array_order
	 CHARACTER(LEN=4)          :: field_type
	 CHARACTER(LEN=8)          :: field_source_prog
	 CHARACTER(LEN=80)         :: source_desc
	 CHARACTER(LEN=8)          :: field_time_type
	 INTEGER                   :: vt_date_start
	 REAL                      :: vt_time_start
	 INTEGER                   :: vt_date_stop
	 REAL                      :: vt_time_stop
      END TYPE wrf_var_metadata

      TYPE(wrf_var_metadata)  :: var_meta , var_info

      TYPE wrf_domain_metadata
	 INTEGER                   :: id
	 INTEGER                   :: parent_id
	 CHARACTER(LEN=8)          :: dyn_init_src
	 CHARACTER(LEN=8)          :: static_init_src 
	 INTEGER                   :: vt_date
	 REAL                      :: vt_time
	 INTEGER                   :: origin_parent_x
	 INTEGER                   :: origin_parent_y
	 INTEGER                   :: ratio_to_parent
	 REAL                      :: delta_x
	 REAL                      :: delta_y
	 REAL                      :: top_level
	 INTEGER                   :: origin_parent_z
	 REAL                      :: corner_lats_new(4,max_staggers_xy_new)
	 REAL                      :: corner_lons_new(4,max_staggers_xy_new)
	 REAL                      :: corner_lats_old(4,max_staggers_xy_old)
	 REAL                      :: corner_lons_old(4,max_staggers_xy_old)
	 INTEGER                   :: xdim
	 INTEGER                   :: ydim
	 INTEGER                   :: zdim
      END TYPE wrf_domain_metadata
      TYPE(wrf_domain_metadata) :: dom_meta

      TYPE wrf_global_metadata
	 CHARACTER(LEN=80)         :: simulation_name
	 CHARACTER(LEN=80)         :: user_desc
	 INTEGER                   :: si_version
	 INTEGER                   :: analysis_version  
	 INTEGER                   :: wrf_version
	 INTEGER                   :: post_version
	 CHARACTER(LEN=32)         :: map_projection
	 REAL                      :: moad_known_lat
	 REAL                      :: moad_known_lon
	 CHARACTER(LEN=8)          :: moad_known_loc
	 REAL                      :: moad_stand_lats(max_standard_lats)
	 REAL                      :: moad_stand_lons(max_standard_lons)
	 REAL                      :: moad_delta_x
	 REAL                      :: moad_delta_y
	 CHARACTER(LEN=4)          :: horiz_stagger_type
	 INTEGER                   :: num_stagger_xy
	 REAL                      :: stagger_dir_x_new(max_staggers_xy_new)
	 REAL                      :: stagger_dir_y_new(max_staggers_xy_new)
	 REAL                      :: stagger_dir_x_old(max_staggers_xy_old)
	 REAL                      :: stagger_dir_y_old(max_staggers_xy_old)
	 INTEGER                   :: num_stagger_z    
	 REAL                      :: stagger_dir_z(max_staggers_z)
	 CHARACTER(LEN=8)          :: vertical_coord
	 INTEGER                   :: num_domains
	 INTEGER                   :: init_date
	 REAL                      :: init_time
	 INTEGER                   :: end_date
	 REAL                      :: end_time
	 CHARACTER(LEN=4)          :: lu_source
	 INTEGER                   :: lu_water
	 INTEGER                   :: lu_ice  
      END TYPE wrf_global_metadata
      TYPE(wrf_global_metadata)   :: global_meta

CONTAINS

   SUBROUTINE read_si ( grid, file_date_string )

      USE module_soil_pre
      USE module_domain

      IMPLICIT NONE

      TYPE(domain) , INTENT(INOUT)  :: grid
      CHARACTER (LEN=19) , INTENT(IN) :: file_date_string

      INTEGER :: ids,ide,jds,jde,kds,kde           &
                ,ims,ime,jms,jme,kms,kme           &
                ,its,ite,jts,jte,kts,kte

      INTEGER :: i , j , k , loop, IMAX, JMAX

      REAL :: dummy

      CHARACTER (LEN= 8) :: dummy_char
      CHARACTER (LEN=256) :: dummy_char_256

      INTEGER :: ok , map_proj , ok_open
      REAL :: pt
      INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat

      SELECT CASE ( model_data_order )
         CASE ( DATA_ORDER_ZXY )
            kds = grid%sd31 ; kde = grid%ed31 ;
            ids = grid%sd32 ; ide = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            kms = grid%sm31 ; kme = grid%em31 ;
            ims = grid%sm32 ; ime = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            kts = grid%sp31 ; kte = grid%ep31 ; 
            its = grid%sp32 ; ite = grid%ep32 ; 
            jts = grid%sp33 ; jte = grid%ep33 ; 

         CASE ( DATA_ORDER_XYZ )
            ids = grid%sd31 ; ide = grid%ed31 ;
            jds = grid%sd32 ; jde = grid%ed32 ;
            kds = grid%sd33 ; kde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            jms = grid%sm32 ; jme = grid%em32 ;
            kms = grid%sm33 ; kme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ; 
            jts = grid%sp32 ; jte = grid%ep32 ; 
            kts = grid%sp33 ; kte = grid%ep33 ; 

         CASE ( DATA_ORDER_XZY )
            ids = grid%sd31 ; ide = grid%ed31 ;
            kds = grid%sd32 ; kde = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            kms = grid%sm32 ; kme = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ; 
            kts = grid%sp32 ; kte = grid%ep32 ; 
            jts = grid%sp33 ; jte = grid%ep33 ; 

      END SELECT

      





      flag_st000010 = 0
      flag_st010040 = 0
      flag_st040100 = 0
      flag_st100200 = 0
      flag_sm000010 = 0 
      flag_sm010040 = 0
      flag_sm040100 = 0
      flag_sm100200 = 0
      flag_st010200 = 0
      flag_sm010200 = 0

      flag_soilt010 = 0
      flag_soilt040 = 0
      flag_soilt100 = 0
      flag_soilt200 = 0 
      flag_soilm010 = 0 
      flag_soilm040 = 0
      flag_soilm100 = 0
      flag_soilm200 = 0

      flag_sst      = 0
      flag_toposoil = 0

      

      num_st_levels_input = 0
      num_sm_levels_input = 0
      st_levels_input = -1
      sm_levels_input = -1

      

        write(6,*) 'enter read_si...first_time_in:: ', first_time_in

      IF ( first_time_in ) THEN

         CLOSE(12)
         OPEN ( FILE   = 'real_input_nm.global.metadata' , &
                UNIT   = 12                              , &
                STATUS = 'OLD'                           , &
                ACCESS = 'SEQUENTIAL'                    , &
                FORM   = 'UNFORMATTED'                   , &
                IOSTAT = ok_open                           )

         IF ( ok_open .NE. 0 ) THEN
            PRINT '(A)','You asked for WRF SI data, but no real_input_nm.global.metadata file exists.'
            STOP 'No_real_input_nm.global.metadata_exists'
         END IF

         READ(12) global_meta%simulation_name, global_meta%user_desc, &
                  global_meta%si_version, global_meta%analysis_version, &
                  global_meta%wrf_version, global_meta%post_version
   
         REWIND (12)

         IF      ( global_meta%si_version .EQ. 1 ) THEN
            READ(12) global_meta%simulation_name, global_meta%user_desc, &
                     global_meta%si_version, global_meta%analysis_version, &
                     global_meta%wrf_version, global_meta%post_version, &
                     global_meta%map_projection, global_meta%moad_known_lat, &
                     global_meta%moad_known_lon, global_meta%moad_known_loc, &
                     global_meta%moad_stand_lats, global_meta%moad_stand_lons, &
                     global_meta%moad_delta_x, global_meta%moad_delta_y, &
                     global_meta%horiz_stagger_type, global_meta%num_stagger_xy, &
                     global_meta%stagger_dir_x_old, global_meta%stagger_dir_y_old, &
                     global_meta%num_stagger_z, global_meta%stagger_dir_z, &
                     global_meta%vertical_coord, global_meta%num_domains, &
                     global_meta%init_date, global_meta%init_time, &
                     global_meta%end_date, global_meta%end_time
         ELSE IF ( global_meta%si_version .EQ. 2 ) THEN
            READ(12) global_meta%simulation_name, global_meta%user_desc, &
                     global_meta%si_version, global_meta%analysis_version, &
                     global_meta%wrf_version, global_meta%post_version, &
                     global_meta%map_projection, global_meta%moad_known_lat, &
                     global_meta%moad_known_lon, global_meta%moad_known_loc, &
                     global_meta%moad_stand_lats, global_meta%moad_stand_lons, &
                     global_meta%moad_delta_x, global_meta%moad_delta_y, &
                     global_meta%horiz_stagger_type, global_meta%num_stagger_xy, &
                     global_meta%stagger_dir_x_new, global_meta%stagger_dir_y_new, &
                     global_meta%num_stagger_z, global_meta%stagger_dir_z, &
                     global_meta%vertical_coord, global_meta%num_domains, &
                     global_meta%init_date, global_meta%init_time, &
                     global_meta%end_date, global_meta%end_time , &
                     global_meta%lu_source, global_meta%lu_water, global_meta%lu_ice
         END IF
         CLOSE (12)
   
         print *,'GLOBAL METADATA'
         print *,'global_meta%simulation_name', global_meta%simulation_name
         print *,'global_meta%user_desc', global_meta%user_desc
         print *,'global_meta%user_desc', global_meta%user_desc
         print *,'global_meta%si_version', global_meta%si_version
         print *,'global_meta%analysis_version', global_meta%analysis_version
         print *,'global_meta%wrf_version', global_meta%wrf_version
         print *,'global_meta%post_version', global_meta%post_version
         print *,'global_meta%map_projection', global_meta%map_projection
         print *,'global_meta%moad_known_lat', global_meta%moad_known_lat
         print *,'global_meta%moad_known_lon', global_meta%moad_known_lon
         print *,'global_meta%moad_known_loc', global_meta%moad_known_loc
         print *,'global_meta%moad_stand_lats', global_meta%moad_stand_lats
         print *,'global_meta%moad_stand_lons', global_meta%moad_stand_lons
         print *,'global_meta%moad_delta_x', global_meta%moad_delta_x
         print *,'global_meta%moad_delta_y', global_meta%moad_delta_y
         print *,'global_meta%horiz_stagger_type', global_meta%horiz_stagger_type
         print *,'global_meta%num_stagger_xy', global_meta%num_stagger_xy
         IF      ( global_meta%si_version .EQ. 1 ) THEN
            print *,'global_meta%stagger_dir_x', global_meta%stagger_dir_x_old
            print *,'global_meta%stagger_dir_y', global_meta%stagger_dir_y_old
         ELSE IF ( global_meta%si_version .EQ. 2 ) THEN
            print *,'global_meta%stagger_dir_x', global_meta%stagger_dir_x_new
            print *,'global_meta%stagger_dir_y', global_meta%stagger_dir_y_new
         END IF
         print *,'global_meta%num_stagger_z', global_meta%num_stagger_z
         print *,'global_meta%stagger_dir_z', global_meta%stagger_dir_z
         print *,'global_meta%vertical_coord', global_meta%vertical_coord
         print *,'global_meta%num_domains', global_meta%num_domains
         print *,'global_meta%init_date', global_meta%init_date
         print *,'global_meta%init_time', global_meta%init_time
         print *,'global_meta%end_date', global_meta%end_date
         print *,'global_meta%end_time', global_meta%end_time
         IF ( global_meta%si_version .EQ. 2 ) THEN
            print *,'global_meta%lu_source', global_meta%lu_source
            print *,'global_meta%lu_water', global_meta%lu_water
            print *,'global_meta%lu_ice', global_meta%lu_ice
         END IF
         print *,' '

         

        IF (.NOT. ALLOCATED (DETA_in)) ALLOCATE(DETA_in(kds:kde-1))
        IF (.NOT. ALLOCATED (AETA_in)) ALLOCATE(AETA_in(kds:kde-1))
        IF (.NOT. ALLOCATED (ETAX_in)) ALLOCATE(ETAX_in(kds:kde))

        IF (.NOT. ALLOCATED (DETA1_in)) ALLOCATE(DETA1_in(kds:kde-1))
        IF (.NOT. ALLOCATED (AETA1_in)) ALLOCATE(AETA1_in(kds:kde-1))
        IF (.NOT. ALLOCATED (ETA1_in))  ALLOCATE(ETA1_in(kds:kde))

        IF (.NOT. ALLOCATED (DETA2_in)) ALLOCATE(DETA2_in(kds:kde-1))
        IF (.NOT. ALLOCATED (AETA2_in)) ALLOCATE(AETA2_in(kds:kde-1))
        IF (.NOT. ALLOCATED (ETA2_in)) ALLOCATE(ETA2_in(kds:kde))

        IF (.NOT. ALLOCATED (DFL_in)) ALLOCATE(DFL_in(kds:kde))

         

        IF (.NOT. ALLOCATED (u_input)  ) ALLOCATE ( u_input(its:ite,jts:jte,kts:kte) )
        IF (.NOT. ALLOCATED (v_input)  ) ALLOCATE ( v_input(its:ite,jts:jte,kts:kte) )
        IF (.NOT. ALLOCATED (q_input)  ) ALLOCATE ( q_input(its:ite,jts:jte,kts:kte) )
        IF (.NOT. ALLOCATED (t_input)  ) ALLOCATE ( t_input(its:ite,jts:jte,kts:kte) )
        IF (.NOT. ALLOCATED (htm_in)  ) ALLOCATE ( htm_in(its:ite,jts:jte,kts:kte) )
        IF (.NOT. ALLOCATED (vtm_in)  ) ALLOCATE ( vtm_in(its:ite,jts:jte,kts:kte) )

        

        IF (.NOT. ALLOCATED (pmsl)              ) ALLOCATE ( pmsl(its:ite,jts:jte) )
        IF (.NOT. ALLOCATED (psfc_in)           ) ALLOCATE ( psfc_in(its:ite,jts:jte) )

        

        

        IF (.NOT. ALLOCATED (st_inputx)) ALLOCATE (st_inputx(its:ite,jts:jte,num_st_levels_alloc))
        IF (.NOT. ALLOCATED (sm_inputx)) ALLOCATE (sm_inputx(its:ite,jts:jte,num_st_levels_alloc))
        IF (.NOT. ALLOCATED (sw_inputx)) ALLOCATE (sw_inputx(its:ite,jts:jte,num_st_levels_alloc))

        IF (.NOT. ALLOCATED (soilt010_input)    ) ALLOCATE ( soilt010_input(its:ite,jts:jte) )
        IF (.NOT. ALLOCATED (soilt040_input)    ) ALLOCATE ( soilt040_input(its:ite,jts:jte) )
        IF (.NOT. ALLOCATED (soilt100_input)    ) ALLOCATE ( soilt100_input(its:ite,jts:jte) )
        IF (.NOT. ALLOCATED (soilt200_input)    ) ALLOCATE ( soilt200_input(its:ite,jts:jte) )
        IF (.NOT. ALLOCATED (soilm010_input)    ) ALLOCATE ( soilm010_input(its:ite,jts:jte) )
        IF (.NOT. ALLOCATED (soilm040_input)    ) ALLOCATE ( soilm040_input(its:ite,jts:jte) )
        IF (.NOT. ALLOCATED (soilm100_input)    ) ALLOCATE ( soilm100_input(its:ite,jts:jte) )
        IF (.NOT. ALLOCATED (soilm200_input)    ) ALLOCATE ( soilm200_input(its:ite,jts:jte) )

        IF (.NOT. ALLOCATED (lat_wind)          ) ALLOCATE (lat_wind(its:ite,jts:jte))
        IF (.NOT. ALLOCATED (lon_wind)          ) ALLOCATE (lon_wind(its:ite,jts:jte))

        
        IF (.NOT. ALLOCATED (dum2d)             ) ALLOCATE (dum2d(IDS:IDE-1,JDS:JDE-1))
        IF (.NOT. ALLOCATED (idum2d)            ) ALLOCATE (idum2d(IDS:IDE-1,JDS:JDE-1))
        IF (.NOT. ALLOCATED (dum3d)             ) ALLOCATE (dum3d(IDS:IDE-1,JDS:JDE-1,KDS:KDE-1))


      END IF

      CLOSE(13)

      write(6,*) 'file_date_string: ', file_date_string
      write(6,*) 'opening real_input_nm.d01.'//file_date_string//' as unit 13'
      OPEN ( FILE   = 'real_input_nm.d01.'//file_date_string , &
             UNIT   = 13                                     , &
             STATUS = 'OLD'                                  , &
             ACCESS = 'SEQUENTIAL'                           , &
             FORM   = 'UNFORMATTED'                            )

      IF      ( global_meta%si_version .EQ. 1 ) THEN
         READ (13) dom_meta%id,dom_meta%parent_id,dom_meta%dyn_init_src,&
                   dom_meta%static_init_src, dom_meta%vt_date, dom_meta%vt_time, &
                   dom_meta%origin_parent_x, dom_meta%origin_parent_y, &
                   dom_meta%ratio_to_parent, dom_meta%delta_x, dom_meta%delta_y, &
                   dom_meta%top_level, dom_meta%origin_parent_z, &
                   dom_meta%corner_lats_old, dom_meta%corner_lons_old, dom_meta%xdim, &
                   dom_meta%ydim, dom_meta%zdim
      ELSE IF ( global_meta%si_version .EQ. 2 ) THEN
         READ (13) dom_meta%id,dom_meta%parent_id,dom_meta%dyn_init_src,&
                   dom_meta%static_init_src, dom_meta%vt_date, dom_meta%vt_time, &
                   dom_meta%origin_parent_x, dom_meta%origin_parent_y, &
                   dom_meta%ratio_to_parent, dom_meta%delta_x, dom_meta%delta_y, &
                   dom_meta%top_level, dom_meta%origin_parent_z, &
                   dom_meta%corner_lats_new, dom_meta%corner_lons_new, dom_meta%xdim, &
                   dom_meta%ydim, dom_meta%zdim
      END IF

      print *,'DOMAIN METADATA'
      print *,'dom_meta%id=', dom_meta%id
      print *,'dom_meta%parent_id=', dom_meta%parent_id
      print *,'dom_meta%dyn_init_src=', dom_meta%dyn_init_src
      print *,'dom_meta%static_init_src=', dom_meta%static_init_src
      print *,'dom_meta%vt_date=', dom_meta%vt_date
      print *,'dom_meta%vt_time=', dom_meta%vt_time
      print *,'dom_meta%origin_parent_x=', dom_meta%origin_parent_x
      print *,'dom_meta%origin_parent_y=', dom_meta%origin_parent_y
      print *,'dom_meta%ratio_to_parent=', dom_meta%ratio_to_parent
      print *,'dom_meta%delta_x=', dom_meta%delta_x
      print *,'dom_meta%delta_y=', dom_meta%delta_y
      print *,'dom_meta%top_level=', dom_meta%top_level
      print *,'dom_meta%origin_parent_z=', dom_meta%origin_parent_z
      IF      ( global_meta%si_version .EQ. 1 ) THEN
         print *,'dom_meta%corner_lats=', dom_meta%corner_lats_old
         print *,'dom_meta%corner_lons=', dom_meta%corner_lons_old
      ELSE IF ( global_meta%si_version .EQ. 2 ) THEN
         print *,'dom_meta%corner_lats=', dom_meta%corner_lats_new
         print *,'dom_meta%corner_lons=', dom_meta%corner_lons_new
      END IF
      print *,'dom_meta%xdim=', dom_meta%xdim
      print *,'dom_meta%ydim=', dom_meta%ydim
      print *,'dom_meta%zdim=', dom_meta%zdim
      print *,' '

      
    




      IF (  abs(dom_meta%xdim - (ide-1)) .gt. 1 &
       .OR. abs(dom_meta%ydim - (jde-1)) .gt. 1 &
       .OR. abs(dom_meta%zdim - (kde-1)) .gt. 1) THEN
         PRINT '(A)','Namelist does not match the input data.'
         PRINT '(A,3I5,A)','Namelist dimensions =',ide-1,jde-1,kde-1,'.'
         PRINT '(A,3I5,A)','Input data dimensions =',dom_meta%xdim,dom_meta%ydim,dom_meta%zdim,'.'
         STOP 'Wrong_data_size'
      END IF

      

      IF        ( global_meta%si_version .EQ. 1 ) THEN
         CALL nl_set_cen_lat ( grid%id , ( dom_meta%corner_lats_old(1,1) + dom_meta%corner_lats_old(2,1) +        &
                                        dom_meta%corner_lats_old(3,1) + dom_meta%corner_lats_old(4,1) ) * 0.25 ) 
      ELSE IF ( ( global_meta%si_version .EQ. 2 ) .AND. ( global_meta%moad_known_loc(1:6) .EQ. 'CENTER' ) ) THEN
         CALL nl_set_cen_lat ( grid%id , global_meta%moad_known_lat )
      ELSE IF   ( global_meta%si_version .EQ. 2 ) THEN
         CALL nl_set_cen_lat ( grid%id , ( dom_meta%corner_lats_new(1,1) + dom_meta%corner_lats_new(2,1) +        &
                                        dom_meta%corner_lats_new(3,1) + dom_meta%corner_lats_new(4,1) ) * 0.25 ) 
      END IF




      CALL nl_set_cen_lon ( grid%id , global_meta%moad_stand_lons(1) )

      write(6,*) 'set_cen_lat... global_meta%moad_stand_lats(1): ', global_meta%moad_stand_lats(1)
      CALL nl_set_cen_lat ( grid%id , global_meta%moad_stand_lats(1) )

      CALL nl_set_truelat1 ( grid%id , global_meta%moad_stand_lats(1) )
      CALL nl_set_truelat2 ( grid%id , global_meta%moad_stand_lats(2) )

      pt = dom_meta%top_level

      IF      ( global_meta%map_projection(1:17) .EQ. 'LAMBERT CONFORMAL'   ) THEN
         map_proj = 1
      ELSE IF ( global_meta%map_projection(1:19) .EQ. 'POLAR STEREOGRAPHIC' ) THEN
         map_proj = 2
      ELSE IF ( global_meta%map_projection(1: 8) .EQ. 'MERCATOR'            ) THEN
         map_proj = 3
      ELSE IF ( global_meta%map_projection(1:14) .EQ. 'ROTATED LATLON' ) THEN
         map_proj = 203 
      ELSE
         PRINT '(A,A,A)','Undefined map projection: ',TRIM(global_meta%map_projection(1:20)),'.'
         STOP 'Undefined_map_proj_si'
      END IF
      CALL nl_set_map_proj ( grid%id , map_proj ) 
     



      IF      ( global_meta%si_version .EQ. 1 ) THEN
         CALL nl_set_mminlu (grid%id, 'USGS' )
         CALL nl_set_iswater (grid%id, 16 )
      ELSE IF ( global_meta%si_version .EQ. 2 ) THEN
         CALL nl_set_mminlu ( grid%id, global_meta%lu_source )
         CALL nl_set_iswater (grid%id, global_meta%lu_water )
         CALL nl_set_isice (grid%id, global_meta%lu_ice )
      END IF

      CALL nl_set_gmt (grid%id, dom_meta%vt_time / 3600. )
      CALL nl_set_julyr (grid%id, dom_meta%vt_date / 1000 )
      CALL nl_set_julday (grid%id, dom_meta%vt_date - ( dom_meta%vt_date / 1000 ) * 1000 )

      write(6,*) 'start reading from unit 13'
      read_all_the_data : DO

         READ (13,IOSTAT=OK) var_info%name, var_info%units, &
                             var_info%description, var_info%domain_id, var_info%ndim, &
                             var_info%dim_val, var_info%dim_desc, var_info%start_index, &
                             var_info%stop_index, var_info%h_stagger_index, var_info%v_stagger_index,&
                             var_info%array_order, var_info%field_type, var_info%field_source_prog, &
                             var_info%source_desc, var_info%field_time_type, var_info%vt_date_start, &
                             var_info%vt_time_start, var_info%vt_date_stop, var_info%vt_time_stop

         IF ( OK .NE. 0 ) THEN
            PRINT '(A,A,A)','End of file found for real_input_nm.d01.',file_date_string,'.'
            EXIT read_all_the_data
         END IF


         PRINT '(A,A)','var_info%name=', var_info%name 




















        JMAX=min(JDE-1,JTE)
        IMAX=min(IDE-1,ITE)
         



         IF      ( var_info%name(1:8) .EQ. 'T       ' ) THEN
            READ (13) dum3d
            do k=kts,kte-1
            do j=jts,JMAX
            do i=its,IMAX
              t_input(i,j,k)=dum3d(i,j,k)
            enddo
            enddo
            enddo

         ELSE IF      ( var_info%name(1:8) .EQ. 'U       ' ) THEN
            READ (13) dum3d
            do k=kts,kte-1
            do j=jts,JMAX
            do i=its,IMAX
              u_input(i,j,k)=dum3d(i,j,k)
            enddo
            enddo
            enddo

         ELSE IF ( var_info%name(1:8) .EQ. 'V       ' ) THEN
            READ (13) dum3d
            do k=kts,kte-1
            do j=jts,JMAX
            do i=its,IMAX
              v_input(i,j,k)=dum3d(i,j,k)
            enddo
            enddo
            enddo

         ELSE IF ( var_info%name(1:8) .EQ. 'Q      ' ) THEN
            READ (13) dum3d
            do k=kts,kte-1
            do j=jts,JMAX
            do i=its,IMAX
              q_input(i,j,k)=dum3d(i,j,k)
            enddo
            enddo
            enddo

         

         ELSE IF ( var_info%name(1:8) .EQ. 'LANDUSEF' ) THEN
            IF ( ( first_time_in ) .AND. ( .NOT. ALLOCATED ( landuse_frac_input) ) ) THEN
               ALLOCATE (landuse_frac_input(its:ite,jts:jte,var_info%dim_val(3)) )
            END IF
            READ (13) (((dum3d(i,j,k),i=ids,ide-1),j=jds,jde-1),k=1,var_info%dim_val(3))
            do k=1,var_info%dim_val(3)
            do j=jts,JMAX
            do i=its,IMAX
              landuse_frac_input(i,j,k)=dum3d(i,j,k)
            enddo
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILCTOP' ) THEN
            IF ( ( first_time_in ) .AND. ( .NOT. ALLOCATED ( soil_top_cat_input) ) ) THEN
               ALLOCATE (soil_top_cat_input(its:ite,jts:jte,var_info%dim_val(3)) )
            END IF
            READ (13) (((dum3d(i,j,k),i=ids,ide-1),j=jds,jde-1),k=1,var_info%dim_val(3))
            do k=1,var_info%dim_val(3)
            do j=jts,JMAX
            do i=its,IMAX
              soil_top_cat_input(i,j,k)=dum3d(i,j,k)
            enddo
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILCBOT' ) THEN
            IF ( ( first_time_in ) .AND. ( .NOT. ALLOCATED ( soil_bot_cat_input) ) ) THEN
               ALLOCATE (soil_bot_cat_input(its:ite,jts:jte,var_info%dim_val(3)) )
            END IF
            READ (13) (((dum3d(i,j,k),i=ids,ide-1),j=jds,jde-1),k=1,var_info%dim_val(3))
            do k=1,var_info%dim_val(3)
            do j=jts,JMAX
            do i=its,IMAX
              soil_bot_cat_input(i,j,k)=dum3d(i,j,k)
            enddo
            enddo
            enddo

         

         ELSE IF ( var_info%name(1:8) .EQ. 'PD      ' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%pd(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'PSFC    ' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              psfc_in(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'PMSL    ' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              pmsl(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'PDTOP   ' ) THEN
            READ (13) grid%pdtop

         ELSE IF ( var_info%name(1:8) .EQ. 'PT      ' ) THEN
            READ (13) grid%pt

         

        ELSE IF ( var_info%name(1:8) .eq. 'GLAT    ' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%glat(i,j)=dum2d(i,j)
            enddo
            enddo
        ELSE IF ( var_info%name(1:8) .eq. 'GLON    ' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%glon(i,j)=dum2d(i,j)
            enddo
            enddo
        ELSE IF ( var_info%name(1:8) .eq. 'LAT_V   ' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              lat_wind(i,j)=dum2d(i,j)
            enddo
            enddo
        ELSE IF ( var_info%name(1:8) .eq. 'LON_V   ' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              lon_wind(i,j)=dum2d(i,j)
            enddo
            enddo

         ELSE IF ( var_info%name(1:8) .EQ. 'ST000010' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%st000010(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_st000010 = 1
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(var_info%name(3:8))
            do j=jts,JMAX
            do i=its,IMAX
              st_inputx(I,J,num_st_levels_input + 1) = grid%st000010(i,j)
            enddo
            enddo

         ELSE IF ( var_info%name(1:8) .EQ. 'ST010040' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%st010040(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_st010040 = 1
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(var_info%name(3:8))
            do j=jts,JMAX
            do i=its,IMAX
              st_inputx(I,J,num_st_levels_input + 1) = grid%st010040(i,j)
            enddo
            enddo

         ELSE IF ( var_info%name(1:8) .EQ. 'ST040100' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%st040100(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_st040100 = 1
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(var_info%name(3:8))
            do j=jts,JMAX
            do i=its,IMAX
              st_inputx(I,J,num_st_levels_input + 1) = grid%st040100(i,j)
            enddo
            enddo

         ELSE IF ( var_info%name(1:8) .EQ. 'ST100200' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%st100200(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_st100200 = 1
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(var_info%name(3:8))
            do j=jts,JMAX
            do i=its,IMAX
              st_inputx(I,J,num_st_levels_input + 1) = grid%st100200(i,j)
            enddo
            enddo

         ELSE IF ( var_info%name(1:8) .EQ. 'ST010200' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%st010200(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_st010200 = 1
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(var_info%name(3:8))
            do j=jts,JMAX
            do i=its,IMAX
              st_inputx(I,J,num_st_levels_input + 1) = grid%st010200(i,j)
            enddo
            enddo

        ELSE IF ( var_info%name(1:8) .EQ. 'SM000010' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%sm000010(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_sm000010 = 1
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(var_info%name(3:8))
            do j=jts,JMAX
            do i=its,IMAX
              sm_inputx(I,J,num_sm_levels_input + 1) = grid%sm000010(i,j)
            enddo
            enddo

         ELSE IF ( var_info%name(1:8) .EQ. 'SM010040' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%sm010040(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_sm010040 = 1
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(var_info%name(3:8))
            do j=jts,JMAX
            do i=its,IMAX
              sm_inputx(I,J,num_sm_levels_input + 1) = grid%sm010040(i,j)
            enddo
            enddo

         ELSE IF ( var_info%name(1:8) .EQ. 'SM040100' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%sm040100(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_sm040100 = 1
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(var_info%name(3:8))
            do j=jts,JMAX
            do i=its,IMAX
              sm_inputx(I,J,num_sm_levels_input + 1) = grid%sm040100(i,j)
            enddo
            enddo

         ELSE IF ( var_info%name(1:8) .EQ. 'SM100200' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%sm100200(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_sm100200 = 1
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(var_info%name(3:8))
            do j=jts,JMAX
            do i=its,IMAX
              sm_inputx(I,J,num_sm_levels_input + 1) = grid%sm100200(i,j)
            enddo
            enddo

         ELSE IF ( var_info%name(1:8) .EQ. 'SM010200' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%sm010200(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_sm010200 = 1
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(var_info%name(3:8))
            do j=jts,JMAX
            do i=its,IMAX
               sm_inputx(I,J,num_sm_levels_input + 1) = grid%sm010200(i,j)
            enddo
            enddo

         ELSE IF ( var_info%name(1:8) .EQ. 'SOILT010' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              soilt010_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_soilt010 = 1
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int1(var_info%name(6:8))

            do j=jts,JMAX
            do i=its,IMAX
              st_inputx(I,J,num_st_levels_input + 1) = soilt010_input(I,J)
            enddo
            enddo
            write(6,*) 'num_st_levels_input=',num_st_levels_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILT040' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              soilt040_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_soilt040 = 1
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int1(var_info%name(6:8))

            do j=jts,JMAX
            do i=its,IMAX
              st_inputx(I,J,num_st_levels_input + 1) = soilt040_input(I,J)
            enddo
            enddo
            write(6,*) 'num_st_levels_input=',num_st_levels_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILT100' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              soilt100_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_soilt100 = 1
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int1(var_info%name(6:8))

            do j=jts,JMAX
            do i=its,IMAX
              st_inputx(I,J,num_st_levels_input + 1) = soilt100_input(I,J)
            enddo
            enddo
            write(6,*) 'num_st_levels_input=',num_st_levels_input
        ELSE IF ( var_info%name(1:8) .EQ. 'SOILT200' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              soilt200_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_soilt200 = 1
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int1(var_info%name(6:8))

            do j=jts,JMAX
            do i=its,IMAX
              st_inputx(I,J,num_st_levels_input + 1) = soilt200_input(I,J)
            enddo
            enddo
            write(6,*) 'num_st_levels_input=',num_st_levels_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILM010' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              soilm010_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_soilm010 = 1
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int1(var_info%name(6:8))

            do j=jts,JMAX
            do i=its,IMAX
              sm_inputx(I,J,num_sm_levels_input + 1) = soilm010_input(I,J)
            enddo
            enddo

         ELSE IF ( var_info%name(1:8) .EQ. 'SOILM040' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              soilm040_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_soilm040 = 1
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int1(var_info%name(6:8))

            do j=jts,JMAX
            do i=its,IMAX
              sm_inputx(I,J,num_sm_levels_input + 1) = soilm040_input(I,J)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILM100' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              soilm100_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_soilm100 = 1
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int1(var_info%name(6:8))

            do j=jts,JMAX
            do i=its,IMAX
              sm_inputx(I,J,num_sm_levels_input + 1) = soilm100_input(I,J)
            enddo
            enddo

         ELSE IF ( var_info%name(1:8) .EQ. 'SOILM200' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              soilm200_input(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_soilm200 = 1
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int1(var_info%name(6:8))

            do j=jts,JMAX
            do i=its,IMAX
              sm_inputx(I,J,num_sm_levels_input + 1) = soilm200_input(I,J)
            enddo
            enddo

         ELSE IF ( var_info%name(1:8) .EQ. 'SEAICE  ' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%xice(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'WEASD   ' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%weasd(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'CANWAT  ' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%canwat(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'LANDMASK' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%landmask(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'SKINTEMP' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%nmm_tsk(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'TGROUND ' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
             grid%tg(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILTB  ' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
             grid%soiltb(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'SST     ' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
               grid%sst(i,j)=dum2d(i,j)
            enddo
            enddo
            flag_sst = 1
         ELSE IF ( var_info%name(1:8) .EQ. 'GREENFRC' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%vegfrc(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'ISLOPE  ' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%islope(i,j)=nint(dum2d(i,j))
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'GREENMAX' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%greenmax(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'GREENMIN' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
               grid%greenmin(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'FIS     ' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%fis(i,j)=dum2d(i,j)
            enddo
            enddo
        ELSE IF ( var_info%name(1:8) .EQ. 'Z0      ' ) THEN

            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%z0(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'CMC     ' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%cmc(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'HTM     ' ) THEN
            READ (13) dum3d
            do k=kts,kte-1
            do j=jts,JMAX
            do i=its,IMAX
              htm_in(i,j,k)=dum3d(i,j,k)
            enddo
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'VTM     ' ) THEN
            READ (13) dum3d
            do k=kts,kte-1
            do j=jts,JMAX
            do i=its,IMAX
              vtm_in(i,j,k)=dum3d(i,j,k)
            enddo
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'SM      ' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%sm(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'ALBASE  ' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%albase(i,j)=dum2d(i,j)
            enddo
            enddo
         ELSE IF ( var_info%name(1:8) .EQ. 'MXSNAL  ' ) THEN
            READ (13) dum2d
            do j=jts,JMAX
            do i=its,IMAX
              grid%mxsnal(i,j)=dum2d(i,j)
            enddo
            enddo

         

          ELSE IF ( var_info%name(1:8) .EQ. 'DETA    ' ) THEN
             READ(13) DETA_in
          ELSE IF ( var_info%name(1:8) .EQ. 'DETA1   ' ) THEN
             READ(13) DETA1_in
          ELSE IF ( var_info%name(1:8) .EQ. 'DETA2   ' ) THEN
             READ(13) DETA2_in
          ELSE IF ( var_info%name(1:8) .EQ. 'ETAX    ' ) THEN
             READ(13) ETAX_in
          ELSE IF ( var_info%name(1:8) .EQ. 'ETA1    ' ) THEN
             READ(13) ETA1_in
          ELSE IF ( var_info%name(1:8) .EQ. 'ETA2    ' ) THEN
             READ(13) ETA2_in
          ELSE IF ( var_info%name(1:8) .EQ. 'AETA    ' ) THEN
             READ(13) AETA_in
          ELSE IF ( var_info%name(1:8) .EQ. 'AETA1   ' ) THEN
             READ(13) AETA1_in
          ELSE IF ( var_info%name(1:8) .EQ. 'AETA2   ' ) THEN
             READ(13) AETA2_in
          ELSE IF ( var_info%name(1:8) .EQ. 'DFL     ' ) THEN
             READ(13) DFL_in






         

         ELSE IF ( var_info%name(1:8) .EQ. 'ZETAFULL' ) THEN
            PRINT '(A)','Oops, you put in the height data.'
            STOP 'this_is_mass_not_height'
 

         

         ELSE
print *,'------------------> skipping ', var_info%name(1:8)
            READ (13) dummy
         END IF

      END DO read_all_the_data

      CLOSE (13)

      first_time_in = .FALSE.


        sw_inputx=0.


      do k=kts,kte-1
      do j=jts,JMAX
      do i=its,IMAX
        grid%U(I,J,K)=U_input(I,J,K)
        grid%V(I,J,K)=V_input(I,J,K)
        grid%T(I,J,K)=T_input(I,J,K)
        grid%Q(I,J,K)=Q_input(I,J,K)
      enddo
      enddo
      enddo



      sw_input=0.







        do J=JTS,min(JDE-1,JTE)
         do K=1,num_st_levels_alloc
          do I=ITS,min(IDE-1,ITE)
             st_input(I,K,J)=st_inputx(I,J,K)
             sm_input(I,K,J)=sm_inputx(I,J,K)
             sw_input(I,K,J)=sw_inputx(I,J,K)
          enddo
         enddo
        enddo







         num_veg_cat      = SIZE ( grid%landusef , DIM=2 )
         num_soil_top_cat = SIZE ( grid%soilctop , DIM=2 )
         num_soil_bot_cat = SIZE ( grid%soilcbot , DIM=2 )

        do J=JTS,min(JDE-1,JTE)
         do K=1,num_soil_top_cat
          do I=ITS,min(IDE-1,ITE)
          grid%SOILCTOP(I,K,J)=soil_top_cat_input(I,J,K)
          grid%SOILCTOP_gc(I,J,K)=soil_top_cat_input(I,J,K)
          enddo
         enddo
        enddo

        do J=JTS,min(JDE-1,JTE)
         do K=1,num_soil_bot_cat
          do I=ITS,min(IDE-1,ITE)
          grid%SOILCBOT(I,K,J)=soil_bot_cat_input(I,J,K)
          grid%SOILCBOT_gc(I,J,K)=soil_bot_cat_input(I,J,K)
          enddo
         enddo
        enddo

        do J=JTS,min(JDE-1,JTE)
         do K=1,num_veg_cat
          do I=ITS,min(IDE-1,ITE)
          grid%LANDUSEF(I,K,J)=landuse_frac_input(I,J,K)
          grid%LANDUSEF_gc(I,J,K)=landuse_frac_input(I,J,K)
          enddo
         enddo
        enddo


      do K=KDS,KDE
        grid%ETAX(K)=ETAX_in(KDE-K+1)
        grid%ETA1(K)=ETA1_in(KDE-K+1)
        grid%ETA2(K)=ETA2_in(KDE-K+1)
        grid%DFL(K)=DFL_in(KDE-K+1)
      enddo

      do K=KDS,KDE-1
        grid%DETA(K)=DETA_in(KDE-K)
        grid%DETA1(K)=DETA1_in(KDE-K)
        grid%DETA2(K)=DETA2_in(KDE-K)
        grid%AETA(K)=AETA_in(KDE-K)
        grid%AETA1(K)=AETA1_in(KDE-K)
        grid%AETA2(K)=AETA2_in(KDE-K)
      enddo

   END SUBROUTINE read_si








   SUBROUTINE read_wps ( grid, filename, file_date_string, num_metgrid_levels )

      USE module_soil_pre
      USE module_domain
      USE module_io_int_idx, only: io_int_index, r_info
      USE module_io_int_read, only: io_int_fetch_data

      IMPLICIT NONE

      INCLUDE "mpif.h"

      TYPE(domain) ,       INTENT(INOUT)  :: grid
      CHARACTER (*) ,      INTENT(IN)     :: filename
      CHARACTER (LEN=19) , INTENT(IN)     :: file_date_string 
      INTEGER ,            INTENT(IN)     :: num_metgrid_levels
      CHARACTER (LEN=19)              :: VarName
      CHARACTER (LEN=150)             :: chartemp
      CHARACTER (LEN=256)             :: mminlu_loc, message

      INTEGER :: ids,ide,jds,jde,kds,kde, &
                 ims,ime,jms,jme,kms,kme, &
                 its,ite,jts,jte,kts,kte

      INTEGER :: i , j , k , IMAX, JMAX
      INTEGER :: iunit, ierr

      REAL    :: garb
      INTEGER :: igarb
      INTEGER :: num_veg_cat, num_gfrac, &
                 num_soil_top_cat , num_soil_bot_cat

      type(r_info), pointer :: r(:) => NULL()


      CALL get_ijk_from_grid (grid ,                        &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              its, ite, jts, jte, kts, kte  )
      IMAX=min(IDE-1,ITE)
      JMAX=min(JDE-1,JTE)

      mminlu_loc=''

      
      flag_st000010 = 0
      flag_st010040 = 0
      flag_st010200 = 0
      flag_st040100 = 0
      flag_st100200 = 0

      flag_sm000010 = 0
      flag_sm010040 = 0
      flag_sm010200 = 0
      flag_sm040100 = 0
      flag_sm100200 = 0

      flag_soilt010 = 0
      flag_soilt040 = 0
      flag_soilt100 = 0
      flag_soilt200 = 0

      flag_soilm010 = 0
      flag_soilm040 = 0
      flag_soilm100 = 0
      flag_soilm200 = 0

      flag_sst      = 0
      flag_toposoil = 0

      
      num_st_levels_input = 0
      num_sm_levels_input = 0
      st_levels_input = -1
      sm_levels_input = -1

      num_soil_top_cat = SIZE(grid%soilctop_gc, DIM=3)
      num_soil_bot_cat = SIZE(grid%soilcbot_gc, DIM=3)
      num_gfrac        = SIZE(grid%greenfrac_gc, DIM=3)

      

      IF (.NOT. ALLOCATED (pmsl)              ) ALLOCATE ( pmsl(its:ite,jts:jte) )
      IF (.NOT. ALLOCATED (psfc_in)           ) ALLOCATE ( psfc_in(its:ite,jts:jte) )
      IF (.NOT. ALLOCATED (st_inputx)) ALLOCATE (st_inputx(its:ite,jts:jte,num_st_levels_alloc))
      IF (.NOT. ALLOCATED (sm_inputx)) ALLOCATE (sm_inputx(its:ite,jts:jte,num_st_levels_alloc))
      IF (.NOT. ALLOCATED (sw_inputx)) ALLOCATE (sw_inputx(its:ite,jts:jte,num_st_levels_alloc))
      IF (.NOT. ALLOCATED (soilt010_input)    ) ALLOCATE ( soilt010_input(its:ite,jts:jte) )
      IF (.NOT. ALLOCATED (soilt040_input)    ) ALLOCATE ( soilt040_input(its:ite,jts:jte) )
      IF (.NOT. ALLOCATED (soilt100_input)    ) ALLOCATE ( soilt100_input(its:ite,jts:jte) )
      IF (.NOT. ALLOCATED (soilt200_input)    ) ALLOCATE ( soilt200_input(its:ite,jts:jte) )
      IF (.NOT. ALLOCATED (soilm010_input)    ) ALLOCATE ( soilm010_input(its:ite,jts:jte) )
      IF (.NOT. ALLOCATED (soilm040_input)    ) ALLOCATE ( soilm040_input(its:ite,jts:jte) )
      IF (.NOT. ALLOCATED (soilm100_input)    ) ALLOCATE ( soilm100_input(its:ite,jts:jte) )
      IF (.NOT. ALLOCATED (soilm200_input)    ) ALLOCATE ( soilm200_input(its:ite,jts:jte) )

      
      call io_int_index(filename, r, ierr)
      if (ierr .ne. 0) then
          call wrf_error_fatal3("<stdin>",1495,&
"Error obtaining index")
          return
      end if

      call mpi_file_open(mpi_comm_world, trim(filename), &
                         mpi_mode_rdonly, mpi_info_null, &
                         iunit, ierr)
      if (ierr /= 0) then
          write(message,*) 'Error opening ', trim(filename), &
                           'with MPI IO code: ', ierr
          call wrf_error_fatal3("module_si_io_nmm.F", 1526, message)
      end if

      call io_int_fetch_data(iunit, r, 'CEN_LAT', garb, ierr)
      call nl_set_cen_lat(grid%id , garb)

      call io_int_fetch_data(iunit, r, 'CEN_LON', garb, ierr)
      call nl_set_cen_lon(grid%id , garb)
      call nl_set_stand_lon(grid%id , garb)

      call io_int_fetch_data(iunit, r, 'TRUELAT1', garb, ierr)
      call nl_set_truelat1(grid%id , garb)

      call io_int_fetch_data(iunit, r, 'TRUELAT2', garb, ierr)
      call nl_set_truelat2(grid%id , garb)

      call io_int_fetch_data(iunit, r, 'MAP_PROJ', igarb, ierr)
      call nl_set_map_proj(grid%id , igarb)

      call io_int_fetch_data(iunit, r, 'NUM_LAND_CAT', num_veg_cat, ierr)

      call io_int_fetch_data(iunit, r, 'ISWATER', igarb, ierr)
      call nl_set_iswater(grid%id, igarb)

      call io_int_fetch_data(iunit, r, 'ISICE', igarb, ierr)
      call nl_set_isice(grid%id, igarb)

      call io_int_fetch_data(iunit, r, 'ISLAKE', igarb, ierr)
      call nl_set_islake(grid%id, igarb)

      call io_int_fetch_data(iunit, r, 'FLAG_SNOWH', flag_snowh, ierr)

      call io_int_fetch_data(iunit, r, 'MMINLU', mminlu_loc, ierr)
      call nl_set_mminlu(grid%id, mminlu_loc)



      call io_int_fetch_data(iunit, r, 'PRES', &
                         grid%p_gc(its:imax, jts:jmax, kts:num_metgrid_levels),&
                         ierr)

      call io_int_fetch_data(iunit, r, 'GHT', &
                       grid%ght_gc(its:imax, jts:jmax, kts:num_metgrid_levels),&
                         ierr)

      call io_int_fetch_data(iunit, r, 'SNOWH', &
                         grid%snowh(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'SNOW', &
                         grid%snow(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'SKINTEMP', &
                         grid%tsk_gc(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'SOILHGT', &
                         grid%toposoil(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'SEAICE', &
                         grid%xice_gc(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'ST100200', &
                         grid%st100200(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'ST040100', &
                         grid%st040100(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'ST010040', &
                         grid%st010040(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'ST000010', &
                         grid%st000010(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'SM100200', &
                         grid%sm100200(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'SM040100', &
                         grid%sm040100(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'SM010040', &
                         grid%sm010040(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'SM000010', &
                         grid%sm000010(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'PSFC', &
                         grid%psfc(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'RH', &
                       grid%rh_gc(its:imax, jts:jmax, kts:num_metgrid_levels),&
                         ierr)

      call io_int_fetch_data(iunit, r, 'VV', &
                       grid%v_gc(its:imax, jts:jmax, kts:num_metgrid_levels),&
                         ierr)

      call io_int_fetch_data(iunit, r, 'UU', &
                       grid%u_gc(its:imax, jts:jmax, kts:num_metgrid_levels),&
                         ierr)

      call io_int_fetch_data(iunit, r, 'TT', &
                       grid%t_gc(its:imax, jts:jmax, kts:num_metgrid_levels),&
                         ierr)

      call io_int_fetch_data(iunit, r, 'SLOPECAT', &
                         grid%slopecat(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'SNOALB', &
                         grid%snoalb(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'GREENFRAC', &
                         grid%greenfrac_gc(its:imax, jts:jmax, kts:num_gfrac), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'ALBEDO12M', &
                         grid%albedo12m_gc(its:imax, jts:jmax, kts:num_gfrac), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'SOILCBOT', &
                  grid%soilcbot_gc(its:imax, jts:jmax, kts:num_soil_bot_cat),&
                         ierr)

      call io_int_fetch_data(iunit, r, 'SOILCTOP', &
                    grid%soilctop_gc(its:imax, jts:jmax, kts:num_soil_bot_cat),&
                         ierr)

      call io_int_fetch_data(iunit, r, 'SOILTEMP', &
                         grid%tmn_gc(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'HGT_V', &
                         grid%htv_gc(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'HGT_M', &
                         grid%ht_gc(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'HZMAX', &
                         grid%hzmax(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'HANGL', &
                         grid%hangl(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'HSLOP', &
                         grid%hslop(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'HLENNW', &
                         grid%hlennw(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'HLENSW', &
                         grid%hlensw(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'HLENS', &
                         grid%hlens(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'HLENW', &
                         grid%hlenw(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'HASYNW', &
                         grid%hasynw(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'HASYSW', &
                         grid%hasysw(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'HASYS', &
                         grid%hasys(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'HASYW', &
                         grid%hasyw(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'HSTDV', &
                         grid%hstdv(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'HCNVX', &
                         grid%hcnvx(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'LU_INDEX', &
                         grid%lu_index(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'LANDUSEF', &
                         grid%landusef_gc(its:imax, jts:jmax, kts:num_veg_cat),&
                         ierr)

      call io_int_fetch_data(iunit, r, 'LANDMASK', &
                         grid%landmask(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'XLONG_V', &
                         grid%vlon_gc(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'XLAT_V', &
                         grid%vlat_gc(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'XLONG_M', &
                         grid%hlon_gc(its:imax, jts:jmax), &
                         ierr)

      call io_int_fetch_data(iunit, r, 'XLAT_M', &
                         grid%hlat_gc(its:imax, jts:jmax), &
                         ierr)

















      call mpi_file_close(iunit, ierr)

       varName='ST000010'
       flag_st000010 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
           st_inputx(I,J,num_st_levels_input + 1) = grid%st000010(i,j)
        ENDDO
       ENDDO

       varName='ST010040'
       flag_st010040 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
            st_inputx(I,J,num_st_levels_input + 1) = grid%st010040(i,j)
        ENDDO
       ENDDO

       varName='ST040100'
       flag_st040100 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              st_inputx(I,J,num_st_levels_input + 1) = grid%st040100(i,j)
        ENDDO
       ENDDO

       varName='ST100200'
       flag_st100200 = 1
       num_st_levels_input = num_st_levels_input + 1
       st_levels_input(num_st_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              st_inputx(I,J,num_st_levels_input + 1) = grid%st100200(i,j)
        ENDDO
       ENDDO

       varName='SM000010'
       flag_sm000010 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
           sm_inputx(I,J,num_sm_levels_input + 1) = grid%sm000010(i,j)
        ENDDO
       ENDDO

       varName='SM010040'
       flag_sm010040 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
            sm_inputx(I,J,num_sm_levels_input + 1) = grid%sm010040(i,j)
        ENDDO
       ENDDO

       varName='SM040100'
       flag_sm040100 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              sm_inputx(I,J,num_sm_levels_input + 1) = grid%sm040100(i,j)
        ENDDO
       ENDDO

       varName='SM100200'
       flag_sm100200 = 1
       num_sm_levels_input = num_sm_levels_input + 1
       sm_levels_input(num_sm_levels_input) = char2int2(varName(3:8))
       DO J=JTS,min(JTE,JDE-1)
        DO I=ITS,min(ITE,IDE-1)
              sm_inputx(I,J,num_sm_levels_input + 1) = grid%sm100200(i,j)
        ENDDO
       ENDDO

       flag_sst  = 1
       sw_inputx = 0.
       sw_input  = 0.


        do J=JTS,min(JDE-1,JTE)
         do K=1,num_st_levels_alloc
          do I=ITS,min(IDE-1,ITE)
             st_input(I,K,J)=st_inputx(I,J,K)
             sm_input(I,K,J)=sm_inputx(I,J,K)
             sw_input(I,K,J)=sw_inputx(I,J,K)
          enddo
         enddo
        enddo

     end subroutine read_wps

END MODULE module_si_io_nmm
