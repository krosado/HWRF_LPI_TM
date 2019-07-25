


MODULE module_scalar_tables
  USE module_driver_constants
  USE module_state_description
  USE module_domain_type, ONLY : streamrec






  INTEGER, TARGET :: szj_index_table( param_num_szj, max_domains )
  INTEGER, TARGET :: szj_num_table( max_domains )
  TYPE(streamrec), TARGET :: szj_streams_table( max_domains, param_num_szj )
  LOGICAL, TARGET :: szj_boundary_table( max_domains, param_num_szj )
  CHARACTER*256, TARGET :: szj_dname_table( max_domains, param_num_szj )
  CHARACTER*256, TARGET :: szj_desc_table( max_domains, param_num_szj )
  CHARACTER*256, TARGET :: szj_units_table( max_domains, param_num_szj )
  INTEGER, TARGET :: s1z_index_table( param_num_s1z, max_domains )
  INTEGER, TARGET :: s1z_num_table( max_domains )
  TYPE(streamrec), TARGET :: s1z_streams_table( max_domains, param_num_s1z )
  LOGICAL, TARGET :: s1z_boundary_table( max_domains, param_num_s1z )
  CHARACTER*256, TARGET :: s1z_dname_table( max_domains, param_num_s1z )
  CHARACTER*256, TARGET :: s1z_desc_table( max_domains, param_num_s1z )
  CHARACTER*256, TARGET :: s1z_units_table( max_domains, param_num_s1z )
  INTEGER, TARGET :: spz_index_table( param_num_spz, max_domains )
  INTEGER, TARGET :: spz_num_table( max_domains )
  TYPE(streamrec), TARGET :: spz_streams_table( max_domains, param_num_spz )
  LOGICAL, TARGET :: spz_boundary_table( max_domains, param_num_spz )
  CHARACTER*256, TARGET :: spz_dname_table( max_domains, param_num_spz )
  CHARACTER*256, TARGET :: spz_desc_table( max_domains, param_num_spz )
  CHARACTER*256, TARGET :: spz_units_table( max_domains, param_num_spz )
  INTEGER, TARGET :: tcs_index_table( param_num_tcs, max_domains )
  INTEGER, TARGET :: tcs_num_table( max_domains )
  TYPE(streamrec), TARGET :: tcs_streams_table( max_domains, param_num_tcs )
  LOGICAL, TARGET :: tcs_boundary_table( max_domains, param_num_tcs )
  CHARACTER*256, TARGET :: tcs_dname_table( max_domains, param_num_tcs )
  CHARACTER*256, TARGET :: tcs_desc_table( max_domains, param_num_tcs )
  CHARACTER*256, TARGET :: tcs_units_table( max_domains, param_num_tcs )
  INTEGER, TARGET :: moist_index_table( param_num_moist, max_domains )
  INTEGER, TARGET :: moist_num_table( max_domains )
  TYPE(streamrec), TARGET :: moist_streams_table( max_domains, param_num_moist )
  LOGICAL, TARGET :: moist_boundary_table( max_domains, param_num_moist )
  CHARACTER*256, TARGET :: moist_dname_table( max_domains, param_num_moist )
  CHARACTER*256, TARGET :: moist_desc_table( max_domains, param_num_moist )
  CHARACTER*256, TARGET :: moist_units_table( max_domains, param_num_moist )
  INTEGER, TARGET :: dfi_moist_index_table( param_num_dfi_moist, max_domains )
  INTEGER, TARGET :: dfi_moist_num_table( max_domains )
  TYPE(streamrec), TARGET :: dfi_moist_streams_table( max_domains, param_num_dfi_moist )
  LOGICAL, TARGET :: dfi_moist_boundary_table( max_domains, param_num_dfi_moist )
  CHARACTER*256, TARGET :: dfi_moist_dname_table( max_domains, param_num_dfi_moist )
  CHARACTER*256, TARGET :: dfi_moist_desc_table( max_domains, param_num_dfi_moist )
  CHARACTER*256, TARGET :: dfi_moist_units_table( max_domains, param_num_dfi_moist )
  INTEGER, TARGET :: scalar_index_table( param_num_scalar, max_domains )
  INTEGER, TARGET :: scalar_num_table( max_domains )
  TYPE(streamrec), TARGET :: scalar_streams_table( max_domains, param_num_scalar )
  LOGICAL, TARGET :: scalar_boundary_table( max_domains, param_num_scalar )
  CHARACTER*256, TARGET :: scalar_dname_table( max_domains, param_num_scalar )
  CHARACTER*256, TARGET :: scalar_desc_table( max_domains, param_num_scalar )
  CHARACTER*256, TARGET :: scalar_units_table( max_domains, param_num_scalar )
  INTEGER, TARGET :: dfi_scalar_index_table( param_num_dfi_scalar, max_domains )
  INTEGER, TARGET :: dfi_scalar_num_table( max_domains )
  TYPE(streamrec), TARGET :: dfi_scalar_streams_table( max_domains, param_num_dfi_scalar )
  LOGICAL, TARGET :: dfi_scalar_boundary_table( max_domains, param_num_dfi_scalar )
  CHARACTER*256, TARGET :: dfi_scalar_dname_table( max_domains, param_num_dfi_scalar )
  CHARACTER*256, TARGET :: dfi_scalar_desc_table( max_domains, param_num_dfi_scalar )
  CHARACTER*256, TARGET :: dfi_scalar_units_table( max_domains, param_num_dfi_scalar )
  INTEGER, TARGET :: chem_index_table( param_num_chem, max_domains )
  INTEGER, TARGET :: chem_num_table( max_domains )
  TYPE(streamrec), TARGET :: chem_streams_table( max_domains, param_num_chem )
  LOGICAL, TARGET :: chem_boundary_table( max_domains, param_num_chem )
  CHARACTER*256, TARGET :: chem_dname_table( max_domains, param_num_chem )
  CHARACTER*256, TARGET :: chem_desc_table( max_domains, param_num_chem )
  CHARACTER*256, TARGET :: chem_units_table( max_domains, param_num_chem )
  INTEGER, TARGET :: ozmixm_index_table( param_num_ozmixm, max_domains )
  INTEGER, TARGET :: ozmixm_num_table( max_domains )
  TYPE(streamrec), TARGET :: ozmixm_streams_table( max_domains, param_num_ozmixm )
  LOGICAL, TARGET :: ozmixm_boundary_table( max_domains, param_num_ozmixm )
  CHARACTER*256, TARGET :: ozmixm_dname_table( max_domains, param_num_ozmixm )
  CHARACTER*256, TARGET :: ozmixm_desc_table( max_domains, param_num_ozmixm )
  CHARACTER*256, TARGET :: ozmixm_units_table( max_domains, param_num_ozmixm )

CONTAINS
  SUBROUTINE init_module_scalar_tables
     INTEGER i , j
     DO j = 1, max_domains






  szj_num_table( j ) = 1
  s1z_num_table( j ) = 1
  spz_num_table( j ) = 1
  tcs_num_table( j ) = 1
  moist_num_table( j ) = 1
  dfi_moist_num_table( j ) = 1
  scalar_num_table( j ) = 1
  dfi_scalar_num_table( j ) = 1
  chem_num_table( j ) = 1
  ozmixm_num_table( j ) = 1

     END DO
  END SUBROUTINE init_module_scalar_tables
END MODULE module_scalar_tables

MODULE module_configure

   USE module_driver_constants
   USE module_state_description
   USE module_wrf_error

   TYPE model_config_rec_type
      SEQUENCE











integer    :: first_item_in_struct
real , DIMENSION(max_domains) :: lakedepth_default
real , DIMENSION(max_domains) :: lake_min_elev
integer , DIMENSION(max_domains) :: use_lakedepth
integer :: halo_debug
integer :: ntracers
integer , DIMENSION(max_domains) :: vortex_tracker
real , DIMENSION(max_domains) :: interest_rad_storm
real , DIMENSION(max_domains) :: interest_rad_parent
real , DIMENSION(max_domains) :: interest_rad_self
integer , DIMENSION(max_domains) :: interest_kids
integer , DIMENSION(max_domains) :: interest_self
integer , DIMENSION(max_domains) :: interest_storms
integer :: swath_mode
integer :: num_old_fixes
real , DIMENSION(max_domains) :: vt4_radius
real , DIMENSION(max_domains) :: vt4_weightexp
real , DIMENSION(max_domains) :: vt4_pmax
real , DIMENSION(max_domains) :: vt4_noise_pmax
real , DIMENSION(max_domains) :: vt4_noise_pmin
real , DIMENSION(max_domains) :: vt4_noise_dpdr
integer , DIMENSION(max_domains) :: vt4_noise_iter
real , DIMENSION(max_domains) :: nomove_freq
integer , DIMENSION(max_domains) :: coral_x
integer , DIMENSION(max_domains) :: coral_y
integer :: tg_reset_stream
integer :: tg_option
integer , DIMENSION(max_domains) :: ntornado
real , DIMENSION(max_domains) :: wbd0
real , DIMENSION(max_domains) :: sbd0
logical , DIMENSION(max_domains) :: analysis
logical , DIMENSION(max_domains) :: write_analysis
integer :: io_form_auxinput2
logical :: high_freq
integer :: high_dom
integer :: swint_opt
integer , DIMENSION(max_domains) :: aer_type
integer , DIMENSION(max_domains) :: aer_aod550_opt
integer , DIMENSION(max_domains) :: aer_angexp_opt
integer , DIMENSION(max_domains) :: aer_ssa_opt
integer , DIMENSION(max_domains) :: aer_asy_opt
real , DIMENSION(max_domains) :: aer_aod550_val
real , DIMENSION(max_domains) :: aer_angexp_val
real , DIMENSION(max_domains) :: aer_ssa_val
real , DIMENSION(max_domains) :: aer_asy_val
integer :: dveg
integer :: opt_crs
integer :: opt_btr
integer :: opt_run
integer :: opt_sfc
integer :: opt_frz
integer :: opt_inf
integer :: opt_rad
integer :: opt_alb
integer :: opt_snf
integer :: opt_tbot
integer :: opt_stc
integer :: wrf_hydro
integer :: run_days
integer :: run_hours
integer :: run_minutes
integer :: run_seconds
integer , DIMENSION(max_domains) :: start_year
integer , DIMENSION(max_domains) :: start_month
integer , DIMENSION(max_domains) :: start_day
integer , DIMENSION(max_domains) :: start_hour
integer , DIMENSION(max_domains) :: start_minute
integer , DIMENSION(max_domains) :: start_second
integer , DIMENSION(max_domains) :: end_year
integer , DIMENSION(max_domains) :: end_month
integer , DIMENSION(max_domains) :: end_day
integer , DIMENSION(max_domains) :: end_hour
integer , DIMENSION(max_domains) :: end_minute
integer , DIMENSION(max_domains) :: end_second
integer :: interval_seconds
logical , DIMENSION(max_domains) :: input_from_file
integer , DIMENSION(max_domains) :: fine_input_stream
character*256 :: auxinput1_inname
integer :: io_form_auxinput1
logical :: override_restart_timers
character*256 :: auxhist1_inname
character*256 :: auxhist1_outname
integer , DIMENSION(max_domains) :: auxhist1_interval_y
integer , DIMENSION(max_domains) :: auxhist1_interval_d
integer , DIMENSION(max_domains) :: auxhist1_interval_h
integer , DIMENSION(max_domains) :: auxhist1_interval_m
integer , DIMENSION(max_domains) :: auxhist1_interval_s
integer , DIMENSION(max_domains) :: auxhist1_interval
integer , DIMENSION(max_domains) :: auxhist1_begin_y
integer , DIMENSION(max_domains) :: auxhist1_begin_d
integer , DIMENSION(max_domains) :: auxhist1_begin_h
integer , DIMENSION(max_domains) :: auxhist1_begin_m
integer , DIMENSION(max_domains) :: auxhist1_begin_s
integer , DIMENSION(max_domains) :: auxhist1_begin
integer , DIMENSION(max_domains) :: auxhist1_end_y
integer , DIMENSION(max_domains) :: auxhist1_end_d
integer , DIMENSION(max_domains) :: auxhist1_end_h
integer , DIMENSION(max_domains) :: auxhist1_end_m
integer , DIMENSION(max_domains) :: auxhist1_end_s
integer , DIMENSION(max_domains) :: auxhist1_end
integer :: io_form_auxhist1
integer , DIMENSION(max_domains) :: frames_per_auxhist1
character*256 :: auxhist2_inname
character*256 :: auxhist2_outname
integer , DIMENSION(max_domains) :: auxhist2_interval_y
integer , DIMENSION(max_domains) :: auxhist2_interval_d
integer , DIMENSION(max_domains) :: auxhist2_interval_h
integer , DIMENSION(max_domains) :: auxhist2_interval_m
integer , DIMENSION(max_domains) :: auxhist2_interval_s
integer , DIMENSION(max_domains) :: auxhist2_interval
integer , DIMENSION(max_domains) :: auxhist2_begin_y
integer , DIMENSION(max_domains) :: auxhist2_begin_d
integer , DIMENSION(max_domains) :: auxhist2_begin_h
integer , DIMENSION(max_domains) :: auxhist2_begin_m
integer , DIMENSION(max_domains) :: auxhist2_begin_s
integer , DIMENSION(max_domains) :: auxhist2_begin
integer , DIMENSION(max_domains) :: auxhist2_end_y
integer , DIMENSION(max_domains) :: auxhist2_end_d
integer , DIMENSION(max_domains) :: auxhist2_end_h
integer , DIMENSION(max_domains) :: auxhist2_end_m
integer , DIMENSION(max_domains) :: auxhist2_end_s
integer , DIMENSION(max_domains) :: auxhist2_end
integer :: io_form_auxhist2
integer , DIMENSION(max_domains) :: frames_per_auxhist2
character*256 :: auxhist3_inname
character*256 :: auxhist3_outname
integer , DIMENSION(max_domains) :: auxhist3_interval_y
integer , DIMENSION(max_domains) :: auxhist3_interval_d
integer , DIMENSION(max_domains) :: auxhist3_interval_h
integer , DIMENSION(max_domains) :: auxhist3_interval_m
integer , DIMENSION(max_domains) :: auxhist3_interval_s
integer , DIMENSION(max_domains) :: auxhist3_interval
integer , DIMENSION(max_domains) :: auxhist3_begin_y
integer , DIMENSION(max_domains) :: auxhist3_begin_d
integer , DIMENSION(max_domains) :: auxhist3_begin_h
integer , DIMENSION(max_domains) :: auxhist3_begin_m
integer , DIMENSION(max_domains) :: auxhist3_begin_s
integer , DIMENSION(max_domains) :: auxhist3_begin
integer , DIMENSION(max_domains) :: auxhist3_end_y
integer , DIMENSION(max_domains) :: auxhist3_end_d
integer , DIMENSION(max_domains) :: auxhist3_end_h
integer , DIMENSION(max_domains) :: auxhist3_end_m
integer , DIMENSION(max_domains) :: auxhist3_end_s
integer , DIMENSION(max_domains) :: auxhist3_end
integer :: io_form_auxhist3
integer , DIMENSION(max_domains) :: frames_per_auxhist3
character*256 :: auxhist4_inname
character*256 :: auxhist4_outname
integer , DIMENSION(max_domains) :: auxhist4_interval_y
integer , DIMENSION(max_domains) :: auxhist4_interval_d
integer , DIMENSION(max_domains) :: auxhist4_interval_h
integer , DIMENSION(max_domains) :: auxhist4_interval_m
integer , DIMENSION(max_domains) :: auxhist4_interval_s
integer , DIMENSION(max_domains) :: auxhist4_interval
integer , DIMENSION(max_domains) :: auxhist4_begin_y
integer , DIMENSION(max_domains) :: auxhist4_begin_d
integer , DIMENSION(max_domains) :: auxhist4_begin_h
integer , DIMENSION(max_domains) :: auxhist4_begin_m
integer , DIMENSION(max_domains) :: auxhist4_begin_s
integer , DIMENSION(max_domains) :: auxhist4_begin
integer , DIMENSION(max_domains) :: auxhist4_end_y
integer , DIMENSION(max_domains) :: auxhist4_end_d
integer , DIMENSION(max_domains) :: auxhist4_end_h
integer , DIMENSION(max_domains) :: auxhist4_end_m
integer , DIMENSION(max_domains) :: auxhist4_end_s
integer , DIMENSION(max_domains) :: auxhist4_end
integer :: io_form_auxhist4
integer , DIMENSION(max_domains) :: frames_per_auxhist4
character*256 :: auxhist5_inname
character*256 :: auxhist5_outname
integer , DIMENSION(max_domains) :: auxhist5_interval_y
integer , DIMENSION(max_domains) :: auxhist5_interval_d
integer , DIMENSION(max_domains) :: auxhist5_interval_h
integer , DIMENSION(max_domains) :: auxhist5_interval_m
integer , DIMENSION(max_domains) :: auxhist5_interval_s
integer , DIMENSION(max_domains) :: auxhist5_interval
integer , DIMENSION(max_domains) :: auxhist5_begin_y
integer , DIMENSION(max_domains) :: auxhist5_begin_d
integer , DIMENSION(max_domains) :: auxhist5_begin_h
integer , DIMENSION(max_domains) :: auxhist5_begin_m
integer , DIMENSION(max_domains) :: auxhist5_begin_s
integer , DIMENSION(max_domains) :: auxhist5_begin
integer , DIMENSION(max_domains) :: auxhist5_end_y
integer , DIMENSION(max_domains) :: auxhist5_end_d
integer , DIMENSION(max_domains) :: auxhist5_end_h
integer , DIMENSION(max_domains) :: auxhist5_end_m
integer , DIMENSION(max_domains) :: auxhist5_end_s
integer , DIMENSION(max_domains) :: auxhist5_end
integer :: io_form_auxhist5
integer , DIMENSION(max_domains) :: frames_per_auxhist5
character*256 :: auxhist6_inname
character*256 :: auxhist6_outname
integer , DIMENSION(max_domains) :: auxhist6_interval_y
integer , DIMENSION(max_domains) :: auxhist6_interval_d
integer , DIMENSION(max_domains) :: auxhist6_interval_h
integer , DIMENSION(max_domains) :: auxhist6_interval_m
integer , DIMENSION(max_domains) :: auxhist6_interval_s
integer , DIMENSION(max_domains) :: auxhist6_interval
integer , DIMENSION(max_domains) :: auxhist6_begin_y
integer , DIMENSION(max_domains) :: auxhist6_begin_d
integer , DIMENSION(max_domains) :: auxhist6_begin_h
integer , DIMENSION(max_domains) :: auxhist6_begin_m
integer , DIMENSION(max_domains) :: auxhist6_begin_s
integer , DIMENSION(max_domains) :: auxhist6_begin
integer , DIMENSION(max_domains) :: auxhist6_end_y
integer , DIMENSION(max_domains) :: auxhist6_end_d
integer , DIMENSION(max_domains) :: auxhist6_end_h
integer , DIMENSION(max_domains) :: auxhist6_end_m
integer , DIMENSION(max_domains) :: auxhist6_end_s
integer , DIMENSION(max_domains) :: auxhist6_end
integer :: io_form_auxhist6
integer , DIMENSION(max_domains) :: frames_per_auxhist6
character*256 :: auxhist7_inname
character*256 :: auxhist7_outname
integer , DIMENSION(max_domains) :: auxhist7_interval_y
integer , DIMENSION(max_domains) :: auxhist7_interval_d
integer , DIMENSION(max_domains) :: auxhist7_interval_h
integer , DIMENSION(max_domains) :: auxhist7_interval_m
integer , DIMENSION(max_domains) :: auxhist7_interval_s
integer , DIMENSION(max_domains) :: auxhist7_interval
integer , DIMENSION(max_domains) :: auxhist7_begin_y
integer , DIMENSION(max_domains) :: auxhist7_begin_d
integer , DIMENSION(max_domains) :: auxhist7_begin_h
integer , DIMENSION(max_domains) :: auxhist7_begin_m
integer , DIMENSION(max_domains) :: auxhist7_begin_s
integer , DIMENSION(max_domains) :: auxhist7_begin
integer , DIMENSION(max_domains) :: auxhist7_end_y
integer , DIMENSION(max_domains) :: auxhist7_end_d
integer , DIMENSION(max_domains) :: auxhist7_end_h
integer , DIMENSION(max_domains) :: auxhist7_end_m
integer , DIMENSION(max_domains) :: auxhist7_end_s
integer , DIMENSION(max_domains) :: auxhist7_end
integer :: io_form_auxhist7
integer , DIMENSION(max_domains) :: frames_per_auxhist7
character*256 :: auxhist8_inname
character*256 :: auxhist8_outname
integer , DIMENSION(max_domains) :: auxhist8_interval_y
integer , DIMENSION(max_domains) :: auxhist8_interval_d
integer , DIMENSION(max_domains) :: auxhist8_interval_h
integer , DIMENSION(max_domains) :: auxhist8_interval_m
integer , DIMENSION(max_domains) :: auxhist8_interval_s
integer , DIMENSION(max_domains) :: auxhist8_interval
integer , DIMENSION(max_domains) :: auxhist8_begin_y
integer , DIMENSION(max_domains) :: auxhist8_begin_d
integer , DIMENSION(max_domains) :: auxhist8_begin_h
integer , DIMENSION(max_domains) :: auxhist8_begin_m
integer , DIMENSION(max_domains) :: auxhist8_begin_s
integer , DIMENSION(max_domains) :: auxhist8_begin
integer , DIMENSION(max_domains) :: auxhist8_end_y
integer , DIMENSION(max_domains) :: auxhist8_end_d
integer , DIMENSION(max_domains) :: auxhist8_end_h
integer , DIMENSION(max_domains) :: auxhist8_end_m
integer , DIMENSION(max_domains) :: auxhist8_end_s
integer , DIMENSION(max_domains) :: auxhist8_end
integer :: io_form_auxhist8
integer , DIMENSION(max_domains) :: frames_per_auxhist8
character*256 :: auxhist9_inname
character*256 :: auxhist9_outname
integer , DIMENSION(max_domains) :: auxhist9_interval_y
integer , DIMENSION(max_domains) :: auxhist9_interval_d
integer , DIMENSION(max_domains) :: auxhist9_interval_h
integer , DIMENSION(max_domains) :: auxhist9_interval_m
integer , DIMENSION(max_domains) :: auxhist9_interval_s
integer , DIMENSION(max_domains) :: auxhist9_interval
integer , DIMENSION(max_domains) :: auxhist9_begin_y
integer , DIMENSION(max_domains) :: auxhist9_begin_d
integer , DIMENSION(max_domains) :: auxhist9_begin_h
integer , DIMENSION(max_domains) :: auxhist9_begin_m
integer , DIMENSION(max_domains) :: auxhist9_begin_s
integer , DIMENSION(max_domains) :: auxhist9_begin
integer , DIMENSION(max_domains) :: auxhist9_end_y
integer , DIMENSION(max_domains) :: auxhist9_end_d
integer , DIMENSION(max_domains) :: auxhist9_end_h
integer , DIMENSION(max_domains) :: auxhist9_end_m
integer , DIMENSION(max_domains) :: auxhist9_end_s
integer , DIMENSION(max_domains) :: auxhist9_end
integer :: io_form_auxhist9
integer , DIMENSION(max_domains) :: frames_per_auxhist9
character*256 :: auxhist10_inname
character*256 :: auxhist10_outname
integer , DIMENSION(max_domains) :: auxhist10_interval_y
integer , DIMENSION(max_domains) :: auxhist10_interval_d
integer , DIMENSION(max_domains) :: auxhist10_interval_h
integer , DIMENSION(max_domains) :: auxhist10_interval_m
integer , DIMENSION(max_domains) :: auxhist10_interval_s
integer , DIMENSION(max_domains) :: auxhist10_interval
integer , DIMENSION(max_domains) :: auxhist10_begin_y
integer , DIMENSION(max_domains) :: auxhist10_begin_d
integer , DIMENSION(max_domains) :: auxhist10_begin_h
integer , DIMENSION(max_domains) :: auxhist10_begin_m
integer , DIMENSION(max_domains) :: auxhist10_begin_s
integer , DIMENSION(max_domains) :: auxhist10_begin
integer , DIMENSION(max_domains) :: auxhist10_end_y
integer , DIMENSION(max_domains) :: auxhist10_end_d
integer , DIMENSION(max_domains) :: auxhist10_end_h
integer , DIMENSION(max_domains) :: auxhist10_end_m
integer , DIMENSION(max_domains) :: auxhist10_end_s
integer , DIMENSION(max_domains) :: auxhist10_end
integer :: io_form_auxhist10
integer , DIMENSION(max_domains) :: frames_per_auxhist10
character*256 :: auxhist11_inname
character*256 :: auxhist11_outname
integer , DIMENSION(max_domains) :: auxhist11_interval_y
integer , DIMENSION(max_domains) :: auxhist11_interval_d
integer , DIMENSION(max_domains) :: auxhist11_interval_h
integer , DIMENSION(max_domains) :: auxhist11_interval_m
integer , DIMENSION(max_domains) :: auxhist11_interval_s
integer , DIMENSION(max_domains) :: auxhist11_interval
integer , DIMENSION(max_domains) :: auxhist11_begin_y
integer , DIMENSION(max_domains) :: auxhist11_begin_d
integer , DIMENSION(max_domains) :: auxhist11_begin_h
integer , DIMENSION(max_domains) :: auxhist11_begin_m
integer , DIMENSION(max_domains) :: auxhist11_begin_s
integer , DIMENSION(max_domains) :: auxhist11_begin
integer , DIMENSION(max_domains) :: auxhist11_end_y
integer , DIMENSION(max_domains) :: auxhist11_end_d
integer , DIMENSION(max_domains) :: auxhist11_end_h
integer , DIMENSION(max_domains) :: auxhist11_end_m
integer , DIMENSION(max_domains) :: auxhist11_end_s
integer , DIMENSION(max_domains) :: auxhist11_end
integer :: io_form_auxhist11
integer , DIMENSION(max_domains) :: frames_per_auxhist11
character*256 :: auxhist12_inname
character*256 :: auxhist12_outname
integer , DIMENSION(max_domains) :: auxhist12_interval_y
integer , DIMENSION(max_domains) :: auxhist12_interval_d
integer , DIMENSION(max_domains) :: auxhist12_interval_h
integer , DIMENSION(max_domains) :: auxhist12_interval_m
integer , DIMENSION(max_domains) :: auxhist12_interval_s
integer , DIMENSION(max_domains) :: auxhist12_interval
integer , DIMENSION(max_domains) :: auxhist12_begin_y
integer , DIMENSION(max_domains) :: auxhist12_begin_d
integer , DIMENSION(max_domains) :: auxhist12_begin_h
integer , DIMENSION(max_domains) :: auxhist12_begin_m
integer , DIMENSION(max_domains) :: auxhist12_begin_s
integer , DIMENSION(max_domains) :: auxhist12_begin
integer , DIMENSION(max_domains) :: auxhist12_end_y
integer , DIMENSION(max_domains) :: auxhist12_end_d
integer , DIMENSION(max_domains) :: auxhist12_end_h
integer , DIMENSION(max_domains) :: auxhist12_end_m
integer , DIMENSION(max_domains) :: auxhist12_end_s
integer , DIMENSION(max_domains) :: auxhist12_end
integer :: io_form_auxhist12
integer , DIMENSION(max_domains) :: frames_per_auxhist12
character*256 :: auxhist13_inname
character*256 :: auxhist13_outname
integer , DIMENSION(max_domains) :: auxhist13_interval_y
integer , DIMENSION(max_domains) :: auxhist13_interval_d
integer , DIMENSION(max_domains) :: auxhist13_interval_h
integer , DIMENSION(max_domains) :: auxhist13_interval_m
integer , DIMENSION(max_domains) :: auxhist13_interval_s
integer , DIMENSION(max_domains) :: auxhist13_interval
integer , DIMENSION(max_domains) :: auxhist13_begin_y
integer , DIMENSION(max_domains) :: auxhist13_begin_d
integer , DIMENSION(max_domains) :: auxhist13_begin_h
integer , DIMENSION(max_domains) :: auxhist13_begin_m
integer , DIMENSION(max_domains) :: auxhist13_begin_s
integer , DIMENSION(max_domains) :: auxhist13_begin
integer , DIMENSION(max_domains) :: auxhist13_end_y
integer , DIMENSION(max_domains) :: auxhist13_end_d
integer , DIMENSION(max_domains) :: auxhist13_end_h
integer , DIMENSION(max_domains) :: auxhist13_end_m
integer , DIMENSION(max_domains) :: auxhist13_end_s
integer , DIMENSION(max_domains) :: auxhist13_end
integer :: io_form_auxhist13
integer , DIMENSION(max_domains) :: frames_per_auxhist13
character*256 :: auxhist14_inname
character*256 :: auxhist14_outname
integer , DIMENSION(max_domains) :: auxhist14_interval_y
integer , DIMENSION(max_domains) :: auxhist14_interval_d
integer , DIMENSION(max_domains) :: auxhist14_interval_h
integer , DIMENSION(max_domains) :: auxhist14_interval_m
integer , DIMENSION(max_domains) :: auxhist14_interval_s
integer , DIMENSION(max_domains) :: auxhist14_interval
integer , DIMENSION(max_domains) :: auxhist14_begin_y
integer , DIMENSION(max_domains) :: auxhist14_begin_d
integer , DIMENSION(max_domains) :: auxhist14_begin_h
integer , DIMENSION(max_domains) :: auxhist14_begin_m
integer , DIMENSION(max_domains) :: auxhist14_begin_s
integer , DIMENSION(max_domains) :: auxhist14_begin
integer , DIMENSION(max_domains) :: auxhist14_end_y
integer , DIMENSION(max_domains) :: auxhist14_end_d
integer , DIMENSION(max_domains) :: auxhist14_end_h
integer , DIMENSION(max_domains) :: auxhist14_end_m
integer , DIMENSION(max_domains) :: auxhist14_end_s
integer , DIMENSION(max_domains) :: auxhist14_end
integer :: io_form_auxhist14
integer , DIMENSION(max_domains) :: frames_per_auxhist14
character*256 :: auxhist15_inname
character*256 :: auxhist15_outname
integer , DIMENSION(max_domains) :: auxhist15_interval_y
integer , DIMENSION(max_domains) :: auxhist15_interval_d
integer , DIMENSION(max_domains) :: auxhist15_interval_h
integer , DIMENSION(max_domains) :: auxhist15_interval_m
integer , DIMENSION(max_domains) :: auxhist15_interval_s
integer , DIMENSION(max_domains) :: auxhist15_interval
integer , DIMENSION(max_domains) :: auxhist15_begin_y
integer , DIMENSION(max_domains) :: auxhist15_begin_d
integer , DIMENSION(max_domains) :: auxhist15_begin_h
integer , DIMENSION(max_domains) :: auxhist15_begin_m
integer , DIMENSION(max_domains) :: auxhist15_begin_s
integer , DIMENSION(max_domains) :: auxhist15_begin
integer , DIMENSION(max_domains) :: auxhist15_end_y
integer , DIMENSION(max_domains) :: auxhist15_end_d
integer , DIMENSION(max_domains) :: auxhist15_end_h
integer , DIMENSION(max_domains) :: auxhist15_end_m
integer , DIMENSION(max_domains) :: auxhist15_end_s
integer , DIMENSION(max_domains) :: auxhist15_end
integer :: io_form_auxhist15
integer , DIMENSION(max_domains) :: frames_per_auxhist15
character*256 :: auxhist16_inname
character*256 :: auxhist16_outname
integer , DIMENSION(max_domains) :: auxhist16_interval_y
integer , DIMENSION(max_domains) :: auxhist16_interval_d
integer , DIMENSION(max_domains) :: auxhist16_interval_h
integer , DIMENSION(max_domains) :: auxhist16_interval_m
integer , DIMENSION(max_domains) :: auxhist16_interval_s
integer , DIMENSION(max_domains) :: auxhist16_interval
integer , DIMENSION(max_domains) :: auxhist16_begin_y
integer , DIMENSION(max_domains) :: auxhist16_begin_d
integer , DIMENSION(max_domains) :: auxhist16_begin_h
integer , DIMENSION(max_domains) :: auxhist16_begin_m
integer , DIMENSION(max_domains) :: auxhist16_begin_s
integer , DIMENSION(max_domains) :: auxhist16_begin
integer , DIMENSION(max_domains) :: auxhist16_end_y
integer , DIMENSION(max_domains) :: auxhist16_end_d
integer , DIMENSION(max_domains) :: auxhist16_end_h
integer , DIMENSION(max_domains) :: auxhist16_end_m
integer , DIMENSION(max_domains) :: auxhist16_end_s
integer , DIMENSION(max_domains) :: auxhist16_end
integer :: io_form_auxhist16
integer , DIMENSION(max_domains) :: frames_per_auxhist16
character*256 :: auxhist17_inname
character*256 :: auxhist17_outname
integer , DIMENSION(max_domains) :: auxhist17_interval_y
integer , DIMENSION(max_domains) :: auxhist17_interval_d
integer , DIMENSION(max_domains) :: auxhist17_interval_h
integer , DIMENSION(max_domains) :: auxhist17_interval_m
integer , DIMENSION(max_domains) :: auxhist17_interval_s
integer , DIMENSION(max_domains) :: auxhist17_interval
integer , DIMENSION(max_domains) :: auxhist17_begin_y
integer , DIMENSION(max_domains) :: auxhist17_begin_d
integer , DIMENSION(max_domains) :: auxhist17_begin_h
integer , DIMENSION(max_domains) :: auxhist17_begin_m
integer , DIMENSION(max_domains) :: auxhist17_begin_s
integer , DIMENSION(max_domains) :: auxhist17_begin
integer , DIMENSION(max_domains) :: auxhist17_end_y
integer , DIMENSION(max_domains) :: auxhist17_end_d
integer , DIMENSION(max_domains) :: auxhist17_end_h
integer , DIMENSION(max_domains) :: auxhist17_end_m
integer , DIMENSION(max_domains) :: auxhist17_end_s
integer , DIMENSION(max_domains) :: auxhist17_end
integer :: io_form_auxhist17
integer , DIMENSION(max_domains) :: frames_per_auxhist17
character*256 :: auxhist18_inname
character*256 :: auxhist18_outname
integer , DIMENSION(max_domains) :: auxhist18_interval_y
integer , DIMENSION(max_domains) :: auxhist18_interval_d
integer , DIMENSION(max_domains) :: auxhist18_interval_h
integer , DIMENSION(max_domains) :: auxhist18_interval_m
integer , DIMENSION(max_domains) :: auxhist18_interval_s
integer , DIMENSION(max_domains) :: auxhist18_interval
integer , DIMENSION(max_domains) :: auxhist18_begin_y
integer , DIMENSION(max_domains) :: auxhist18_begin_d
integer , DIMENSION(max_domains) :: auxhist18_begin_h
integer , DIMENSION(max_domains) :: auxhist18_begin_m
integer , DIMENSION(max_domains) :: auxhist18_begin_s
integer , DIMENSION(max_domains) :: auxhist18_begin
integer , DIMENSION(max_domains) :: auxhist18_end_y
integer , DIMENSION(max_domains) :: auxhist18_end_d
integer , DIMENSION(max_domains) :: auxhist18_end_h
integer , DIMENSION(max_domains) :: auxhist18_end_m
integer , DIMENSION(max_domains) :: auxhist18_end_s
integer , DIMENSION(max_domains) :: auxhist18_end
integer :: io_form_auxhist18
integer , DIMENSION(max_domains) :: frames_per_auxhist18
character*256 :: auxhist19_inname
character*256 :: auxhist19_outname
integer , DIMENSION(max_domains) :: auxhist19_interval_y
integer , DIMENSION(max_domains) :: auxhist19_interval_d
integer , DIMENSION(max_domains) :: auxhist19_interval_h
integer , DIMENSION(max_domains) :: auxhist19_interval_m
integer , DIMENSION(max_domains) :: auxhist19_interval_s
integer , DIMENSION(max_domains) :: auxhist19_interval
integer , DIMENSION(max_domains) :: auxhist19_begin_y
integer , DIMENSION(max_domains) :: auxhist19_begin_d
integer , DIMENSION(max_domains) :: auxhist19_begin_h
integer , DIMENSION(max_domains) :: auxhist19_begin_m
integer , DIMENSION(max_domains) :: auxhist19_begin_s
integer , DIMENSION(max_domains) :: auxhist19_begin
integer , DIMENSION(max_domains) :: auxhist19_end_y
integer , DIMENSION(max_domains) :: auxhist19_end_d
integer , DIMENSION(max_domains) :: auxhist19_end_h
integer , DIMENSION(max_domains) :: auxhist19_end_m
integer , DIMENSION(max_domains) :: auxhist19_end_s
integer , DIMENSION(max_domains) :: auxhist19_end
integer :: io_form_auxhist19
integer , DIMENSION(max_domains) :: frames_per_auxhist19
character*256 :: auxhist20_inname
character*256 :: auxhist20_outname
integer , DIMENSION(max_domains) :: auxhist20_interval_y
integer , DIMENSION(max_domains) :: auxhist20_interval_d
integer , DIMENSION(max_domains) :: auxhist20_interval_h
integer , DIMENSION(max_domains) :: auxhist20_interval_m
integer , DIMENSION(max_domains) :: auxhist20_interval_s
integer , DIMENSION(max_domains) :: auxhist20_interval
integer , DIMENSION(max_domains) :: auxhist20_begin_y
integer , DIMENSION(max_domains) :: auxhist20_begin_d
integer , DIMENSION(max_domains) :: auxhist20_begin_h
integer , DIMENSION(max_domains) :: auxhist20_begin_m
integer , DIMENSION(max_domains) :: auxhist20_begin_s
integer , DIMENSION(max_domains) :: auxhist20_begin
integer , DIMENSION(max_domains) :: auxhist20_end_y
integer , DIMENSION(max_domains) :: auxhist20_end_d
integer , DIMENSION(max_domains) :: auxhist20_end_h
integer , DIMENSION(max_domains) :: auxhist20_end_m
integer , DIMENSION(max_domains) :: auxhist20_end_s
integer , DIMENSION(max_domains) :: auxhist20_end
integer :: io_form_auxhist20
integer , DIMENSION(max_domains) :: frames_per_auxhist20
character*256 :: auxhist21_inname
character*256 :: auxhist21_outname
integer , DIMENSION(max_domains) :: auxhist21_interval_y
integer , DIMENSION(max_domains) :: auxhist21_interval_d
integer , DIMENSION(max_domains) :: auxhist21_interval_h
integer , DIMENSION(max_domains) :: auxhist21_interval_m
integer , DIMENSION(max_domains) :: auxhist21_interval_s
integer , DIMENSION(max_domains) :: auxhist21_interval
integer , DIMENSION(max_domains) :: auxhist21_begin_y
integer , DIMENSION(max_domains) :: auxhist21_begin_d
integer , DIMENSION(max_domains) :: auxhist21_begin_h
integer , DIMENSION(max_domains) :: auxhist21_begin_m
integer , DIMENSION(max_domains) :: auxhist21_begin_s
integer , DIMENSION(max_domains) :: auxhist21_begin
integer , DIMENSION(max_domains) :: auxhist21_end_y
integer , DIMENSION(max_domains) :: auxhist21_end_d
integer , DIMENSION(max_domains) :: auxhist21_end_h
integer , DIMENSION(max_domains) :: auxhist21_end_m
integer , DIMENSION(max_domains) :: auxhist21_end_s
integer , DIMENSION(max_domains) :: auxhist21_end
integer :: io_form_auxhist21
integer , DIMENSION(max_domains) :: frames_per_auxhist21
character*256 :: auxhist22_inname
character*256 :: auxhist22_outname
integer , DIMENSION(max_domains) :: auxhist22_interval_y
integer , DIMENSION(max_domains) :: auxhist22_interval_d
integer , DIMENSION(max_domains) :: auxhist22_interval_h
integer , DIMENSION(max_domains) :: auxhist22_interval_m
integer , DIMENSION(max_domains) :: auxhist22_interval_s
integer , DIMENSION(max_domains) :: auxhist22_interval
integer , DIMENSION(max_domains) :: auxhist22_begin_y
integer , DIMENSION(max_domains) :: auxhist22_begin_d
integer , DIMENSION(max_domains) :: auxhist22_begin_h
integer , DIMENSION(max_domains) :: auxhist22_begin_m
integer , DIMENSION(max_domains) :: auxhist22_begin_s
integer , DIMENSION(max_domains) :: auxhist22_begin
integer , DIMENSION(max_domains) :: auxhist22_end_y
integer , DIMENSION(max_domains) :: auxhist22_end_d
integer , DIMENSION(max_domains) :: auxhist22_end_h
integer , DIMENSION(max_domains) :: auxhist22_end_m
integer , DIMENSION(max_domains) :: auxhist22_end_s
integer , DIMENSION(max_domains) :: auxhist22_end
integer :: io_form_auxhist22
integer , DIMENSION(max_domains) :: frames_per_auxhist22
character*256 :: auxhist23_inname
character*256 :: auxhist23_outname
integer , DIMENSION(max_domains) :: auxhist23_interval_y
integer , DIMENSION(max_domains) :: auxhist23_interval_d
integer , DIMENSION(max_domains) :: auxhist23_interval_h
integer , DIMENSION(max_domains) :: auxhist23_interval_m
integer , DIMENSION(max_domains) :: auxhist23_interval_s
integer , DIMENSION(max_domains) :: auxhist23_interval
integer , DIMENSION(max_domains) :: auxhist23_begin_y
integer , DIMENSION(max_domains) :: auxhist23_begin_d
integer , DIMENSION(max_domains) :: auxhist23_begin_h
integer , DIMENSION(max_domains) :: auxhist23_begin_m
integer , DIMENSION(max_domains) :: auxhist23_begin_s
integer , DIMENSION(max_domains) :: auxhist23_begin
integer , DIMENSION(max_domains) :: auxhist23_end_y
integer , DIMENSION(max_domains) :: auxhist23_end_d
integer , DIMENSION(max_domains) :: auxhist23_end_h
integer , DIMENSION(max_domains) :: auxhist23_end_m
integer , DIMENSION(max_domains) :: auxhist23_end_s
integer , DIMENSION(max_domains) :: auxhist23_end
integer :: io_form_auxhist23
integer , DIMENSION(max_domains) :: frames_per_auxhist23
character*256 :: auxhist24_inname
character*256 :: auxhist24_outname
integer , DIMENSION(max_domains) :: auxhist24_interval_y
integer , DIMENSION(max_domains) :: auxhist24_interval_d
integer , DIMENSION(max_domains) :: auxhist24_interval_h
integer , DIMENSION(max_domains) :: auxhist24_interval_m
integer , DIMENSION(max_domains) :: auxhist24_interval_s
integer , DIMENSION(max_domains) :: auxhist24_interval
integer , DIMENSION(max_domains) :: auxhist24_begin_y
integer , DIMENSION(max_domains) :: auxhist24_begin_d
integer , DIMENSION(max_domains) :: auxhist24_begin_h
integer , DIMENSION(max_domains) :: auxhist24_begin_m
integer , DIMENSION(max_domains) :: auxhist24_begin_s
integer , DIMENSION(max_domains) :: auxhist24_begin
integer , DIMENSION(max_domains) :: auxhist24_end_y
integer , DIMENSION(max_domains) :: auxhist24_end_d
integer , DIMENSION(max_domains) :: auxhist24_end_h
integer , DIMENSION(max_domains) :: auxhist24_end_m
integer , DIMENSION(max_domains) :: auxhist24_end_s
integer , DIMENSION(max_domains) :: auxhist24_end
integer :: io_form_auxhist24
integer , DIMENSION(max_domains) :: frames_per_auxhist24
character*256 :: auxinput1_outname
integer , DIMENSION(max_domains) :: auxinput1_interval_y
integer , DIMENSION(max_domains) :: auxinput1_interval_d
integer , DIMENSION(max_domains) :: auxinput1_interval_h
integer , DIMENSION(max_domains) :: auxinput1_interval_m
integer , DIMENSION(max_domains) :: auxinput1_interval_s
integer , DIMENSION(max_domains) :: auxinput1_interval
integer , DIMENSION(max_domains) :: auxinput1_begin_y
integer , DIMENSION(max_domains) :: auxinput1_begin_d
integer , DIMENSION(max_domains) :: auxinput1_begin_h
integer , DIMENSION(max_domains) :: auxinput1_begin_m
integer , DIMENSION(max_domains) :: auxinput1_begin_s
integer , DIMENSION(max_domains) :: auxinput1_begin
integer , DIMENSION(max_domains) :: auxinput1_end_y
integer , DIMENSION(max_domains) :: auxinput1_end_d
integer , DIMENSION(max_domains) :: auxinput1_end_h
integer , DIMENSION(max_domains) :: auxinput1_end_m
integer , DIMENSION(max_domains) :: auxinput1_end_s
integer , DIMENSION(max_domains) :: auxinput1_end
integer , DIMENSION(max_domains) :: frames_per_auxinput1
character*256 :: auxinput2_inname
character*256 :: auxinput2_outname
integer , DIMENSION(max_domains) :: auxinput2_interval_y
integer , DIMENSION(max_domains) :: auxinput2_interval_d
integer , DIMENSION(max_domains) :: auxinput2_interval_h
integer , DIMENSION(max_domains) :: auxinput2_interval_m
integer , DIMENSION(max_domains) :: auxinput2_interval_s
integer , DIMENSION(max_domains) :: auxinput2_interval
integer , DIMENSION(max_domains) :: auxinput2_begin_y
integer , DIMENSION(max_domains) :: auxinput2_begin_d
integer , DIMENSION(max_domains) :: auxinput2_begin_h
integer , DIMENSION(max_domains) :: auxinput2_begin_m
integer , DIMENSION(max_domains) :: auxinput2_begin_s
integer , DIMENSION(max_domains) :: auxinput2_begin
integer , DIMENSION(max_domains) :: auxinput2_end_y
integer , DIMENSION(max_domains) :: auxinput2_end_d
integer , DIMENSION(max_domains) :: auxinput2_end_h
integer , DIMENSION(max_domains) :: auxinput2_end_m
integer , DIMENSION(max_domains) :: auxinput2_end_s
integer , DIMENSION(max_domains) :: auxinput2_end
integer , DIMENSION(max_domains) :: frames_per_auxinput2
character*256 :: auxinput3_inname
character*256 :: auxinput3_outname
integer , DIMENSION(max_domains) :: auxinput3_interval_y
integer , DIMENSION(max_domains) :: auxinput3_interval_d
integer , DIMENSION(max_domains) :: auxinput3_interval_h
integer , DIMENSION(max_domains) :: auxinput3_interval_m
integer , DIMENSION(max_domains) :: auxinput3_interval_s
integer , DIMENSION(max_domains) :: auxinput3_interval
integer , DIMENSION(max_domains) :: auxinput3_begin_y
integer , DIMENSION(max_domains) :: auxinput3_begin_d
integer , DIMENSION(max_domains) :: auxinput3_begin_h
integer , DIMENSION(max_domains) :: auxinput3_begin_m
integer , DIMENSION(max_domains) :: auxinput3_begin_s
integer , DIMENSION(max_domains) :: auxinput3_begin
integer , DIMENSION(max_domains) :: auxinput3_end_y
integer , DIMENSION(max_domains) :: auxinput3_end_d
integer , DIMENSION(max_domains) :: auxinput3_end_h
integer , DIMENSION(max_domains) :: auxinput3_end_m
integer , DIMENSION(max_domains) :: auxinput3_end_s
integer , DIMENSION(max_domains) :: auxinput3_end
integer :: io_form_auxinput3
integer , DIMENSION(max_domains) :: frames_per_auxinput3
character*256 :: auxinput4_inname
character*256 :: auxinput4_outname
integer , DIMENSION(max_domains) :: auxinput4_interval_y
integer , DIMENSION(max_domains) :: auxinput4_interval_d
integer , DIMENSION(max_domains) :: auxinput4_interval_h
integer , DIMENSION(max_domains) :: auxinput4_interval_m
integer , DIMENSION(max_domains) :: auxinput4_interval_s
integer , DIMENSION(max_domains) :: auxinput4_interval
integer , DIMENSION(max_domains) :: auxinput4_begin_y
integer , DIMENSION(max_domains) :: auxinput4_begin_d
integer , DIMENSION(max_domains) :: auxinput4_begin_h
integer , DIMENSION(max_domains) :: auxinput4_begin_m
integer , DIMENSION(max_domains) :: auxinput4_begin_s
integer , DIMENSION(max_domains) :: auxinput4_begin
integer , DIMENSION(max_domains) :: auxinput4_end_y
integer , DIMENSION(max_domains) :: auxinput4_end_d
integer , DIMENSION(max_domains) :: auxinput4_end_h
integer , DIMENSION(max_domains) :: auxinput4_end_m
integer , DIMENSION(max_domains) :: auxinput4_end_s
integer , DIMENSION(max_domains) :: auxinput4_end
integer :: io_form_auxinput4
integer , DIMENSION(max_domains) :: frames_per_auxinput4
character*256 :: auxinput5_inname
character*256 :: auxinput5_outname
integer , DIMENSION(max_domains) :: auxinput5_interval_y
integer , DIMENSION(max_domains) :: auxinput5_interval_d
integer , DIMENSION(max_domains) :: auxinput5_interval_h
integer , DIMENSION(max_domains) :: auxinput5_interval_m
integer , DIMENSION(max_domains) :: auxinput5_interval_s
integer , DIMENSION(max_domains) :: auxinput5_interval
integer , DIMENSION(max_domains) :: auxinput5_begin_y
integer , DIMENSION(max_domains) :: auxinput5_begin_d
integer , DIMENSION(max_domains) :: auxinput5_begin_h
integer , DIMENSION(max_domains) :: auxinput5_begin_m
integer , DIMENSION(max_domains) :: auxinput5_begin_s
integer , DIMENSION(max_domains) :: auxinput5_begin
integer , DIMENSION(max_domains) :: auxinput5_end_y
integer , DIMENSION(max_domains) :: auxinput5_end_d
integer , DIMENSION(max_domains) :: auxinput5_end_h
integer , DIMENSION(max_domains) :: auxinput5_end_m
integer , DIMENSION(max_domains) :: auxinput5_end_s
integer , DIMENSION(max_domains) :: auxinput5_end
integer :: io_form_auxinput5
integer , DIMENSION(max_domains) :: frames_per_auxinput5
character*256 :: auxinput6_inname
character*256 :: auxinput6_outname
integer , DIMENSION(max_domains) :: auxinput6_interval_y
integer , DIMENSION(max_domains) :: auxinput6_interval_d
integer , DIMENSION(max_domains) :: auxinput6_interval_h
integer , DIMENSION(max_domains) :: auxinput6_interval_m
integer , DIMENSION(max_domains) :: auxinput6_interval_s
integer , DIMENSION(max_domains) :: auxinput6_interval
integer , DIMENSION(max_domains) :: auxinput6_begin_y
integer , DIMENSION(max_domains) :: auxinput6_begin_d
integer , DIMENSION(max_domains) :: auxinput6_begin_h
integer , DIMENSION(max_domains) :: auxinput6_begin_m
integer , DIMENSION(max_domains) :: auxinput6_begin_s
integer , DIMENSION(max_domains) :: auxinput6_begin
integer , DIMENSION(max_domains) :: auxinput6_end_y
integer , DIMENSION(max_domains) :: auxinput6_end_d
integer , DIMENSION(max_domains) :: auxinput6_end_h
integer , DIMENSION(max_domains) :: auxinput6_end_m
integer , DIMENSION(max_domains) :: auxinput6_end_s
integer , DIMENSION(max_domains) :: auxinput6_end
integer :: io_form_auxinput6
integer , DIMENSION(max_domains) :: frames_per_auxinput6
character*256 :: auxinput7_inname
character*256 :: auxinput7_outname
integer , DIMENSION(max_domains) :: auxinput7_interval_y
integer , DIMENSION(max_domains) :: auxinput7_interval_d
integer , DIMENSION(max_domains) :: auxinput7_interval_h
integer , DIMENSION(max_domains) :: auxinput7_interval_m
integer , DIMENSION(max_domains) :: auxinput7_interval_s
integer , DIMENSION(max_domains) :: auxinput7_interval
integer , DIMENSION(max_domains) :: auxinput7_begin_y
integer , DIMENSION(max_domains) :: auxinput7_begin_d
integer , DIMENSION(max_domains) :: auxinput7_begin_h
integer , DIMENSION(max_domains) :: auxinput7_begin_m
integer , DIMENSION(max_domains) :: auxinput7_begin_s
integer , DIMENSION(max_domains) :: auxinput7_begin
integer , DIMENSION(max_domains) :: auxinput7_end_y
integer , DIMENSION(max_domains) :: auxinput7_end_d
integer , DIMENSION(max_domains) :: auxinput7_end_h
integer , DIMENSION(max_domains) :: auxinput7_end_m
integer , DIMENSION(max_domains) :: auxinput7_end_s
integer , DIMENSION(max_domains) :: auxinput7_end
integer :: io_form_auxinput7
integer , DIMENSION(max_domains) :: frames_per_auxinput7
character*256 :: auxinput8_inname
character*256 :: auxinput8_outname
integer , DIMENSION(max_domains) :: auxinput8_interval_y
integer , DIMENSION(max_domains) :: auxinput8_interval_d
integer , DIMENSION(max_domains) :: auxinput8_interval_h
integer , DIMENSION(max_domains) :: auxinput8_interval_m
integer , DIMENSION(max_domains) :: auxinput8_interval_s
integer , DIMENSION(max_domains) :: auxinput8_interval
integer , DIMENSION(max_domains) :: auxinput8_begin_y
integer , DIMENSION(max_domains) :: auxinput8_begin_d
integer , DIMENSION(max_domains) :: auxinput8_begin_h
integer , DIMENSION(max_domains) :: auxinput8_begin_m
integer , DIMENSION(max_domains) :: auxinput8_begin_s
integer , DIMENSION(max_domains) :: auxinput8_begin
integer , DIMENSION(max_domains) :: auxinput8_end_y
integer , DIMENSION(max_domains) :: auxinput8_end_d
integer , DIMENSION(max_domains) :: auxinput8_end_h
integer , DIMENSION(max_domains) :: auxinput8_end_m
integer , DIMENSION(max_domains) :: auxinput8_end_s
integer , DIMENSION(max_domains) :: auxinput8_end
integer :: io_form_auxinput8
integer , DIMENSION(max_domains) :: frames_per_auxinput8
character*256 :: auxinput9_inname
character*256 :: auxinput9_outname
integer , DIMENSION(max_domains) :: auxinput9_interval_y
integer , DIMENSION(max_domains) :: auxinput9_interval_d
integer , DIMENSION(max_domains) :: auxinput9_interval_h
integer , DIMENSION(max_domains) :: auxinput9_interval_m
integer , DIMENSION(max_domains) :: auxinput9_interval_s
integer , DIMENSION(max_domains) :: auxinput9_interval
integer , DIMENSION(max_domains) :: auxinput9_begin_y
integer , DIMENSION(max_domains) :: auxinput9_begin_d
integer , DIMENSION(max_domains) :: auxinput9_begin_h
integer , DIMENSION(max_domains) :: auxinput9_begin_m
integer , DIMENSION(max_domains) :: auxinput9_begin_s
integer , DIMENSION(max_domains) :: auxinput9_begin
integer , DIMENSION(max_domains) :: auxinput9_end_y
integer , DIMENSION(max_domains) :: auxinput9_end_d
integer , DIMENSION(max_domains) :: auxinput9_end_h
integer , DIMENSION(max_domains) :: auxinput9_end_m
integer , DIMENSION(max_domains) :: auxinput9_end_s
integer , DIMENSION(max_domains) :: auxinput9_end
integer :: io_form_auxinput9
integer , DIMENSION(max_domains) :: frames_per_auxinput9
character*256 :: auxinput10_inname
character*256 :: auxinput10_outname
integer , DIMENSION(max_domains) :: auxinput10_interval_y
integer , DIMENSION(max_domains) :: auxinput10_interval_d
integer , DIMENSION(max_domains) :: auxinput10_interval_h
integer , DIMENSION(max_domains) :: auxinput10_interval_m
integer , DIMENSION(max_domains) :: auxinput10_interval_s
integer , DIMENSION(max_domains) :: auxinput10_interval
integer , DIMENSION(max_domains) :: auxinput10_begin_y
integer , DIMENSION(max_domains) :: auxinput10_begin_d
integer , DIMENSION(max_domains) :: auxinput10_begin_h
integer , DIMENSION(max_domains) :: auxinput10_begin_m
integer , DIMENSION(max_domains) :: auxinput10_begin_s
integer , DIMENSION(max_domains) :: auxinput10_begin
integer , DIMENSION(max_domains) :: auxinput10_end_y
integer , DIMENSION(max_domains) :: auxinput10_end_d
integer , DIMENSION(max_domains) :: auxinput10_end_h
integer , DIMENSION(max_domains) :: auxinput10_end_m
integer , DIMENSION(max_domains) :: auxinput10_end_s
integer , DIMENSION(max_domains) :: auxinput10_end
integer :: io_form_auxinput10
integer , DIMENSION(max_domains) :: frames_per_auxinput10
character*256 :: auxinput11_inname
character*256 :: auxinput11_outname
integer , DIMENSION(max_domains) :: auxinput11_interval_y
integer , DIMENSION(max_domains) :: auxinput11_interval_d
integer , DIMENSION(max_domains) :: auxinput11_interval_h
integer , DIMENSION(max_domains) :: auxinput11_interval_m
integer , DIMENSION(max_domains) :: auxinput11_interval_s
integer , DIMENSION(max_domains) :: auxinput11_interval
integer , DIMENSION(max_domains) :: auxinput11_begin_y
integer , DIMENSION(max_domains) :: auxinput11_begin_d
integer , DIMENSION(max_domains) :: auxinput11_begin_h
integer , DIMENSION(max_domains) :: auxinput11_begin_m
integer , DIMENSION(max_domains) :: auxinput11_begin_s
integer , DIMENSION(max_domains) :: auxinput11_begin
integer , DIMENSION(max_domains) :: auxinput11_end_y
integer , DIMENSION(max_domains) :: auxinput11_end_d
integer , DIMENSION(max_domains) :: auxinput11_end_h
integer , DIMENSION(max_domains) :: auxinput11_end_m
integer , DIMENSION(max_domains) :: auxinput11_end_s
integer , DIMENSION(max_domains) :: auxinput11_end
integer :: io_form_auxinput11
integer , DIMENSION(max_domains) :: frames_per_auxinput11
character*256 :: auxinput12_inname
character*256 :: auxinput12_outname
integer , DIMENSION(max_domains) :: auxinput12_interval_y
integer , DIMENSION(max_domains) :: auxinput12_interval_d
integer , DIMENSION(max_domains) :: auxinput12_interval_h
integer , DIMENSION(max_domains) :: auxinput12_interval_m
integer , DIMENSION(max_domains) :: auxinput12_interval_s
integer , DIMENSION(max_domains) :: auxinput12_interval
integer , DIMENSION(max_domains) :: auxinput12_begin_y
integer , DIMENSION(max_domains) :: auxinput12_begin_d
integer , DIMENSION(max_domains) :: auxinput12_begin_h
integer , DIMENSION(max_domains) :: auxinput12_begin_m
integer , DIMENSION(max_domains) :: auxinput12_begin_s
integer , DIMENSION(max_domains) :: auxinput12_begin
integer , DIMENSION(max_domains) :: auxinput12_end_y
integer , DIMENSION(max_domains) :: auxinput12_end_d
integer , DIMENSION(max_domains) :: auxinput12_end_h
integer , DIMENSION(max_domains) :: auxinput12_end_m
integer , DIMENSION(max_domains) :: auxinput12_end_s
integer , DIMENSION(max_domains) :: auxinput12_end
integer :: io_form_auxinput12
integer , DIMENSION(max_domains) :: frames_per_auxinput12
character*256 :: auxinput13_inname
character*256 :: auxinput13_outname
integer , DIMENSION(max_domains) :: auxinput13_interval_y
integer , DIMENSION(max_domains) :: auxinput13_interval_d
integer , DIMENSION(max_domains) :: auxinput13_interval_h
integer , DIMENSION(max_domains) :: auxinput13_interval_m
integer , DIMENSION(max_domains) :: auxinput13_interval_s
integer , DIMENSION(max_domains) :: auxinput13_interval
integer , DIMENSION(max_domains) :: auxinput13_begin_y
integer , DIMENSION(max_domains) :: auxinput13_begin_d
integer , DIMENSION(max_domains) :: auxinput13_begin_h
integer , DIMENSION(max_domains) :: auxinput13_begin_m
integer , DIMENSION(max_domains) :: auxinput13_begin_s
integer , DIMENSION(max_domains) :: auxinput13_begin
integer , DIMENSION(max_domains) :: auxinput13_end_y
integer , DIMENSION(max_domains) :: auxinput13_end_d
integer , DIMENSION(max_domains) :: auxinput13_end_h
integer , DIMENSION(max_domains) :: auxinput13_end_m
integer , DIMENSION(max_domains) :: auxinput13_end_s
integer , DIMENSION(max_domains) :: auxinput13_end
integer :: io_form_auxinput13
integer , DIMENSION(max_domains) :: frames_per_auxinput13
character*256 :: auxinput14_inname
character*256 :: auxinput14_outname
integer , DIMENSION(max_domains) :: auxinput14_interval_y
integer , DIMENSION(max_domains) :: auxinput14_interval_d
integer , DIMENSION(max_domains) :: auxinput14_interval_h
integer , DIMENSION(max_domains) :: auxinput14_interval_m
integer , DIMENSION(max_domains) :: auxinput14_interval_s
integer , DIMENSION(max_domains) :: auxinput14_interval
integer , DIMENSION(max_domains) :: auxinput14_begin_y
integer , DIMENSION(max_domains) :: auxinput14_begin_d
integer , DIMENSION(max_domains) :: auxinput14_begin_h
integer , DIMENSION(max_domains) :: auxinput14_begin_m
integer , DIMENSION(max_domains) :: auxinput14_begin_s
integer , DIMENSION(max_domains) :: auxinput14_begin
integer , DIMENSION(max_domains) :: auxinput14_end_y
integer , DIMENSION(max_domains) :: auxinput14_end_d
integer , DIMENSION(max_domains) :: auxinput14_end_h
integer , DIMENSION(max_domains) :: auxinput14_end_m
integer , DIMENSION(max_domains) :: auxinput14_end_s
integer , DIMENSION(max_domains) :: auxinput14_end
integer :: io_form_auxinput14
integer , DIMENSION(max_domains) :: frames_per_auxinput14
character*256 :: auxinput15_inname
character*256 :: auxinput15_outname
integer , DIMENSION(max_domains) :: auxinput15_interval_y
integer , DIMENSION(max_domains) :: auxinput15_interval_d
integer , DIMENSION(max_domains) :: auxinput15_interval_h
integer , DIMENSION(max_domains) :: auxinput15_interval_m
integer , DIMENSION(max_domains) :: auxinput15_interval_s
integer , DIMENSION(max_domains) :: auxinput15_interval
integer , DIMENSION(max_domains) :: auxinput15_begin_y
integer , DIMENSION(max_domains) :: auxinput15_begin_d
integer , DIMENSION(max_domains) :: auxinput15_begin_h
integer , DIMENSION(max_domains) :: auxinput15_begin_m
integer , DIMENSION(max_domains) :: auxinput15_begin_s
integer , DIMENSION(max_domains) :: auxinput15_begin
integer , DIMENSION(max_domains) :: auxinput15_end_y
integer , DIMENSION(max_domains) :: auxinput15_end_d
integer , DIMENSION(max_domains) :: auxinput15_end_h
integer , DIMENSION(max_domains) :: auxinput15_end_m
integer , DIMENSION(max_domains) :: auxinput15_end_s
integer , DIMENSION(max_domains) :: auxinput15_end
integer :: io_form_auxinput15
integer , DIMENSION(max_domains) :: frames_per_auxinput15
character*256 :: auxinput16_inname
character*256 :: auxinput16_outname
integer , DIMENSION(max_domains) :: auxinput16_interval_y
integer , DIMENSION(max_domains) :: auxinput16_interval_d
integer , DIMENSION(max_domains) :: auxinput16_interval_h
integer , DIMENSION(max_domains) :: auxinput16_interval_m
integer , DIMENSION(max_domains) :: auxinput16_interval_s
integer , DIMENSION(max_domains) :: auxinput16_interval
integer , DIMENSION(max_domains) :: auxinput16_begin_y
integer , DIMENSION(max_domains) :: auxinput16_begin_d
integer , DIMENSION(max_domains) :: auxinput16_begin_h
integer , DIMENSION(max_domains) :: auxinput16_begin_m
integer , DIMENSION(max_domains) :: auxinput16_begin_s
integer , DIMENSION(max_domains) :: auxinput16_begin
integer , DIMENSION(max_domains) :: auxinput16_end_y
integer , DIMENSION(max_domains) :: auxinput16_end_d
integer , DIMENSION(max_domains) :: auxinput16_end_h
integer , DIMENSION(max_domains) :: auxinput16_end_m
integer , DIMENSION(max_domains) :: auxinput16_end_s
integer , DIMENSION(max_domains) :: auxinput16_end
integer :: io_form_auxinput16
integer , DIMENSION(max_domains) :: frames_per_auxinput16
character*256 :: auxinput17_inname
character*256 :: auxinput17_outname
integer , DIMENSION(max_domains) :: auxinput17_interval_y
integer , DIMENSION(max_domains) :: auxinput17_interval_d
integer , DIMENSION(max_domains) :: auxinput17_interval_h
integer , DIMENSION(max_domains) :: auxinput17_interval_m
integer , DIMENSION(max_domains) :: auxinput17_interval_s
integer , DIMENSION(max_domains) :: auxinput17_interval
integer , DIMENSION(max_domains) :: auxinput17_begin_y
integer , DIMENSION(max_domains) :: auxinput17_begin_d
integer , DIMENSION(max_domains) :: auxinput17_begin_h
integer , DIMENSION(max_domains) :: auxinput17_begin_m
integer , DIMENSION(max_domains) :: auxinput17_begin_s
integer , DIMENSION(max_domains) :: auxinput17_begin
integer , DIMENSION(max_domains) :: auxinput17_end_y
integer , DIMENSION(max_domains) :: auxinput17_end_d
integer , DIMENSION(max_domains) :: auxinput17_end_h
integer , DIMENSION(max_domains) :: auxinput17_end_m
integer , DIMENSION(max_domains) :: auxinput17_end_s
integer , DIMENSION(max_domains) :: auxinput17_end
integer :: io_form_auxinput17
integer , DIMENSION(max_domains) :: frames_per_auxinput17
character*256 :: auxinput18_inname
character*256 :: auxinput18_outname
integer , DIMENSION(max_domains) :: auxinput18_interval_y
integer , DIMENSION(max_domains) :: auxinput18_interval_d
integer , DIMENSION(max_domains) :: auxinput18_interval_h
integer , DIMENSION(max_domains) :: auxinput18_interval_m
integer , DIMENSION(max_domains) :: auxinput18_interval_s
integer , DIMENSION(max_domains) :: auxinput18_interval
integer , DIMENSION(max_domains) :: auxinput18_begin_y
integer , DIMENSION(max_domains) :: auxinput18_begin_d
integer , DIMENSION(max_domains) :: auxinput18_begin_h
integer , DIMENSION(max_domains) :: auxinput18_begin_m
integer , DIMENSION(max_domains) :: auxinput18_begin_s
integer , DIMENSION(max_domains) :: auxinput18_begin
integer , DIMENSION(max_domains) :: auxinput18_end_y
integer , DIMENSION(max_domains) :: auxinput18_end_d
integer , DIMENSION(max_domains) :: auxinput18_end_h
integer , DIMENSION(max_domains) :: auxinput18_end_m
integer , DIMENSION(max_domains) :: auxinput18_end_s
integer , DIMENSION(max_domains) :: auxinput18_end
integer :: io_form_auxinput18
integer , DIMENSION(max_domains) :: frames_per_auxinput18
character*256 :: auxinput19_inname
character*256 :: auxinput19_outname
integer , DIMENSION(max_domains) :: auxinput19_interval_y
integer , DIMENSION(max_domains) :: auxinput19_interval_d
integer , DIMENSION(max_domains) :: auxinput19_interval_h
integer , DIMENSION(max_domains) :: auxinput19_interval_m
integer , DIMENSION(max_domains) :: auxinput19_interval_s
integer , DIMENSION(max_domains) :: auxinput19_interval
integer , DIMENSION(max_domains) :: auxinput19_begin_y
integer , DIMENSION(max_domains) :: auxinput19_begin_d
integer , DIMENSION(max_domains) :: auxinput19_begin_h
integer , DIMENSION(max_domains) :: auxinput19_begin_m
integer , DIMENSION(max_domains) :: auxinput19_begin_s
integer , DIMENSION(max_domains) :: auxinput19_begin
integer , DIMENSION(max_domains) :: auxinput19_end_y
integer , DIMENSION(max_domains) :: auxinput19_end_d
integer , DIMENSION(max_domains) :: auxinput19_end_h
integer , DIMENSION(max_domains) :: auxinput19_end_m
integer , DIMENSION(max_domains) :: auxinput19_end_s
integer , DIMENSION(max_domains) :: auxinput19_end
integer :: io_form_auxinput19
integer , DIMENSION(max_domains) :: frames_per_auxinput19
character*256 :: auxinput20_inname
character*256 :: auxinput20_outname
integer , DIMENSION(max_domains) :: auxinput20_interval_y
integer , DIMENSION(max_domains) :: auxinput20_interval_d
integer , DIMENSION(max_domains) :: auxinput20_interval_h
integer , DIMENSION(max_domains) :: auxinput20_interval_m
integer , DIMENSION(max_domains) :: auxinput20_interval_s
integer , DIMENSION(max_domains) :: auxinput20_interval
integer , DIMENSION(max_domains) :: auxinput20_begin_y
integer , DIMENSION(max_domains) :: auxinput20_begin_d
integer , DIMENSION(max_domains) :: auxinput20_begin_h
integer , DIMENSION(max_domains) :: auxinput20_begin_m
integer , DIMENSION(max_domains) :: auxinput20_begin_s
integer , DIMENSION(max_domains) :: auxinput20_begin
integer , DIMENSION(max_domains) :: auxinput20_end_y
integer , DIMENSION(max_domains) :: auxinput20_end_d
integer , DIMENSION(max_domains) :: auxinput20_end_h
integer , DIMENSION(max_domains) :: auxinput20_end_m
integer , DIMENSION(max_domains) :: auxinput20_end_s
integer , DIMENSION(max_domains) :: auxinput20_end
integer :: io_form_auxinput20
integer , DIMENSION(max_domains) :: frames_per_auxinput20
character*256 :: auxinput21_inname
character*256 :: auxinput21_outname
integer , DIMENSION(max_domains) :: auxinput21_interval_y
integer , DIMENSION(max_domains) :: auxinput21_interval_d
integer , DIMENSION(max_domains) :: auxinput21_interval_h
integer , DIMENSION(max_domains) :: auxinput21_interval_m
integer , DIMENSION(max_domains) :: auxinput21_interval_s
integer , DIMENSION(max_domains) :: auxinput21_interval
integer , DIMENSION(max_domains) :: auxinput21_begin_y
integer , DIMENSION(max_domains) :: auxinput21_begin_d
integer , DIMENSION(max_domains) :: auxinput21_begin_h
integer , DIMENSION(max_domains) :: auxinput21_begin_m
integer , DIMENSION(max_domains) :: auxinput21_begin_s
integer , DIMENSION(max_domains) :: auxinput21_begin
integer , DIMENSION(max_domains) :: auxinput21_end_y
integer , DIMENSION(max_domains) :: auxinput21_end_d
integer , DIMENSION(max_domains) :: auxinput21_end_h
integer , DIMENSION(max_domains) :: auxinput21_end_m
integer , DIMENSION(max_domains) :: auxinput21_end_s
integer , DIMENSION(max_domains) :: auxinput21_end
integer :: io_form_auxinput21
integer , DIMENSION(max_domains) :: frames_per_auxinput21
character*256 :: auxinput22_inname
character*256 :: auxinput22_outname
integer , DIMENSION(max_domains) :: auxinput22_interval_y
integer , DIMENSION(max_domains) :: auxinput22_interval_d
integer , DIMENSION(max_domains) :: auxinput22_interval_h
integer , DIMENSION(max_domains) :: auxinput22_interval_m
integer , DIMENSION(max_domains) :: auxinput22_interval_s
integer , DIMENSION(max_domains) :: auxinput22_interval
integer , DIMENSION(max_domains) :: auxinput22_begin_y
integer , DIMENSION(max_domains) :: auxinput22_begin_d
integer , DIMENSION(max_domains) :: auxinput22_begin_h
integer , DIMENSION(max_domains) :: auxinput22_begin_m
integer , DIMENSION(max_domains) :: auxinput22_begin_s
integer , DIMENSION(max_domains) :: auxinput22_begin
integer , DIMENSION(max_domains) :: auxinput22_end_y
integer , DIMENSION(max_domains) :: auxinput22_end_d
integer , DIMENSION(max_domains) :: auxinput22_end_h
integer , DIMENSION(max_domains) :: auxinput22_end_m
integer , DIMENSION(max_domains) :: auxinput22_end_s
integer , DIMENSION(max_domains) :: auxinput22_end
integer :: io_form_auxinput22
integer , DIMENSION(max_domains) :: frames_per_auxinput22
character*256 :: auxinput23_inname
character*256 :: auxinput23_outname
integer , DIMENSION(max_domains) :: auxinput23_interval_y
integer , DIMENSION(max_domains) :: auxinput23_interval_d
integer , DIMENSION(max_domains) :: auxinput23_interval_h
integer , DIMENSION(max_domains) :: auxinput23_interval_m
integer , DIMENSION(max_domains) :: auxinput23_interval_s
integer , DIMENSION(max_domains) :: auxinput23_interval
integer , DIMENSION(max_domains) :: auxinput23_begin_y
integer , DIMENSION(max_domains) :: auxinput23_begin_d
integer , DIMENSION(max_domains) :: auxinput23_begin_h
integer , DIMENSION(max_domains) :: auxinput23_begin_m
integer , DIMENSION(max_domains) :: auxinput23_begin_s
integer , DIMENSION(max_domains) :: auxinput23_begin
integer , DIMENSION(max_domains) :: auxinput23_end_y
integer , DIMENSION(max_domains) :: auxinput23_end_d
integer , DIMENSION(max_domains) :: auxinput23_end_h
integer , DIMENSION(max_domains) :: auxinput23_end_m
integer , DIMENSION(max_domains) :: auxinput23_end_s
integer , DIMENSION(max_domains) :: auxinput23_end
integer :: io_form_auxinput23
integer , DIMENSION(max_domains) :: frames_per_auxinput23
character*256 :: auxinput24_inname
character*256 :: auxinput24_outname
integer , DIMENSION(max_domains) :: auxinput24_interval_y
integer , DIMENSION(max_domains) :: auxinput24_interval_d
integer , DIMENSION(max_domains) :: auxinput24_interval_h
integer , DIMENSION(max_domains) :: auxinput24_interval_m
integer , DIMENSION(max_domains) :: auxinput24_interval_s
integer , DIMENSION(max_domains) :: auxinput24_interval
integer , DIMENSION(max_domains) :: auxinput24_begin_y
integer , DIMENSION(max_domains) :: auxinput24_begin_d
integer , DIMENSION(max_domains) :: auxinput24_begin_h
integer , DIMENSION(max_domains) :: auxinput24_begin_m
integer , DIMENSION(max_domains) :: auxinput24_begin_s
integer , DIMENSION(max_domains) :: auxinput24_begin
integer , DIMENSION(max_domains) :: auxinput24_end_y
integer , DIMENSION(max_domains) :: auxinput24_end_d
integer , DIMENSION(max_domains) :: auxinput24_end_h
integer , DIMENSION(max_domains) :: auxinput24_end_m
integer , DIMENSION(max_domains) :: auxinput24_end_s
integer , DIMENSION(max_domains) :: auxinput24_end
integer :: io_form_auxinput24
integer , DIMENSION(max_domains) :: frames_per_auxinput24
integer , DIMENSION(max_domains) :: history_interval
integer , DIMENSION(max_domains) :: frames_per_outfile
logical :: restart
integer :: restart_interval
integer :: io_form_input
integer :: io_form_history
integer :: io_form_restart
integer :: io_form_boundary
integer :: debug_level
logical :: self_test_domain
character*256 :: history_outname
character*256 :: history_inname
logical :: use_netcdf_classic
integer , DIMENSION(max_domains) :: history_interval_d
integer , DIMENSION(max_domains) :: history_interval_h
integer , DIMENSION(max_domains) :: history_interval_m
integer , DIMENSION(max_domains) :: history_interval_s
integer , DIMENSION(max_domains) :: inputout_interval_d
integer , DIMENSION(max_domains) :: inputout_interval_h
integer , DIMENSION(max_domains) :: inputout_interval_m
integer , DIMENSION(max_domains) :: inputout_interval_s
integer , DIMENSION(max_domains) :: inputout_interval
integer :: restart_interval_d
integer :: restart_interval_h
integer :: restart_interval_m
integer :: restart_interval_s
integer , DIMENSION(max_domains) :: history_begin_y
integer , DIMENSION(max_domains) :: history_begin_d
integer , DIMENSION(max_domains) :: history_begin_h
integer , DIMENSION(max_domains) :: history_begin_m
integer , DIMENSION(max_domains) :: history_begin_s
integer , DIMENSION(max_domains) :: history_begin
integer , DIMENSION(max_domains) :: inputout_begin_y
integer , DIMENSION(max_domains) :: inputout_begin_d
integer , DIMENSION(max_domains) :: inputout_begin_h
integer , DIMENSION(max_domains) :: inputout_begin_m
integer , DIMENSION(max_domains) :: inputout_begin_s
integer :: restart_begin_y
integer :: restart_begin_d
integer :: restart_begin_h
integer :: restart_begin_m
integer :: restart_begin_s
integer :: restart_begin
integer , DIMENSION(max_domains) :: history_end_y
integer , DIMENSION(max_domains) :: history_end_d
integer , DIMENSION(max_domains) :: history_end_h
integer , DIMENSION(max_domains) :: history_end_m
integer , DIMENSION(max_domains) :: history_end_s
integer , DIMENSION(max_domains) :: history_end
integer , DIMENSION(max_domains) :: inputout_end_y
integer , DIMENSION(max_domains) :: inputout_end_d
integer , DIMENSION(max_domains) :: inputout_end_h
integer , DIMENSION(max_domains) :: inputout_end_m
integer , DIMENSION(max_domains) :: inputout_end_s
integer :: simulation_start_year
integer :: simulation_start_month
integer :: simulation_start_day
integer :: simulation_start_hour
integer :: simulation_start_minute
integer :: simulation_start_second
logical :: reset_simulation_start
integer , DIMENSION(max_domains) :: sr_x
integer , DIMENSION(max_domains) :: sr_y
character*256 , DIMENSION(max_domains) :: iofields_filename
logical :: ignore_iofields_warning
logical :: ncd_nofill
integer , DIMENSION(max_domains) :: julyr
integer , DIMENSION(max_domains) :: julday
real , DIMENSION(max_domains) :: gmt
character*256 :: high_freq_outname
character*256 :: partial_atcf_outname
character*256 :: input_inname
character*256 :: input_outname
character*256 :: bdy_inname
character*256 :: bdy_outname
character*256 :: rst_inname
character*256 :: rst_outname
character*256 , DIMENSION(max_domains) :: anl_outname
logical :: write_input
logical :: write_restart_at_0h
logical :: write_hist_at_0h_rst
logical :: adjust_output_times
logical :: adjust_input_times
real , DIMENSION(max_domains) :: tstart
logical :: nocolons
logical :: cycling
logical :: output_ready_flag
integer :: dfi_opt
integer :: dfi_savehydmeteors
integer :: dfi_nfilter
logical :: dfi_write_filtered_input
logical :: dfi_write_dfi_history
integer :: dfi_cutoff_seconds
integer :: dfi_time_dim
integer :: dfi_fwdstop_year
integer :: dfi_fwdstop_month
integer :: dfi_fwdstop_day
integer :: dfi_fwdstop_hour
integer :: dfi_fwdstop_minute
integer :: dfi_fwdstop_second
integer :: dfi_bckstop_year
integer :: dfi_bckstop_month
integer :: dfi_bckstop_day
integer :: dfi_bckstop_hour
integer :: dfi_bckstop_minute
integer :: dfi_bckstop_second
integer :: time_step
integer :: time_step_fract_num
integer :: time_step_fract_den
integer :: time_step_dfi
integer :: max_dom
integer , DIMENSION(max_domains) :: s_we
integer , DIMENSION(max_domains) :: e_we
integer , DIMENSION(max_domains) :: s_sn
integer , DIMENSION(max_domains) :: e_sn
integer , DIMENSION(max_domains) :: s_vert
integer , DIMENSION(max_domains) :: e_vert
integer :: num_metgrid_soil_levels
real , DIMENSION(max_domains) :: dx
real , DIMENSION(max_domains) :: dy
integer , DIMENSION(max_domains) :: grid_id
logical , DIMENSION(max_domains) :: grid_allowed
integer , DIMENSION(max_domains) :: parent_id
integer , DIMENSION(max_domains) :: i_parent_start
integer , DIMENSION(max_domains) :: j_parent_start
integer , DIMENSION(max_domains) :: parent_grid_ratio
integer , DIMENSION(max_domains) :: parent_time_step_ratio
integer :: feedback
integer :: smooth_option
real , DIMENSION(max_domains) :: ztop
integer , DIMENSION(max_domains) :: moad_grid_ratio
integer , DIMENSION(max_domains) :: moad_time_step_ratio
integer , DIMENSION(max_domains) :: shw
integer :: tile_sz_x
integer :: tile_sz_y
integer :: numtiles
integer :: numtiles_inc
integer :: numtiles_x
integer :: numtiles_y
integer :: tile_strategy
integer :: nproc_x
integer :: nproc_y
integer :: irand
real , DIMENSION(max_domains) :: dt
integer :: ts_buf_size
integer :: max_ts_locs
integer :: num_moves
integer , DIMENSION(max_domains) :: vortex_interval
integer , DIMENSION(max_domains) :: corral_dist
integer , DIMENSION(max_moves) :: move_id
integer , DIMENSION(max_moves) :: move_interval
integer , DIMENSION(max_moves) :: move_cd_x
integer , DIMENSION(max_moves) :: move_cd_y
logical , DIMENSION(max_domains) :: swap_x
logical , DIMENSION(max_domains) :: swap_y
logical , DIMENSION(max_domains) :: cycle_x
logical , DIMENSION(max_domains) :: cycle_y
logical :: reorder_mesh
logical :: perturb_input
real , DIMENSION(max_eta) :: eta_levels
real :: ptsgm
integer :: num_metgrid_levels
real :: p_top_requested
logical :: use_prep_hybrid
logical :: force_read_thompson
logical :: write_thompson_tables
integer , DIMENSION(max_domains) :: mp_physics
real , DIMENSION(max_domains) :: mommix
logical , DIMENSION(max_domains) :: disheat
integer :: do_radar_ref
integer :: compute_radar_ref
integer , DIMENSION(max_domains) :: ra_lw_physics
integer , DIMENSION(max_domains) :: ra_sw_physics
real , DIMENSION(max_domains) :: radt
integer , DIMENSION(max_domains) :: sf_sfclay_physics
integer , DIMENSION(max_domains) :: sf_surface_physics
integer , DIMENSION(max_domains) :: bl_pbl_physics
integer , DIMENSION(max_domains) :: ysu_topdown_pblmix
integer , DIMENSION(max_domains) :: shinhong_tke_diag
integer , DIMENSION(max_domains) :: windfarm_opt
integer :: windfarm_ij
integer , DIMENSION(max_domains) :: mfshconv
real , DIMENSION(max_domains) :: bldt
integer , DIMENSION(max_domains) :: cu_physics
integer , DIMENSION(max_domains) :: shcu_physics
integer , DIMENSION(max_domains) :: cu_diag
real , DIMENSION(max_domains) :: gfs_alpha
real , DIMENSION(max_domains) :: cudt
real , DIMENSION(max_domains) :: gsmdt
integer :: isfflx
integer :: ideal_xland
integer :: ifsnow
integer :: icloud
real :: swrad_scat
integer :: surface_input_source
integer :: num_soil_layers
integer :: num_urban_layers
integer :: sf_surface_mosaic
integer :: mosaic_cat
integer :: mosaic_cat_soil
integer :: num_urban_hi
integer :: mosaic_lu
integer :: mosaic_soil
integer :: maxiens
integer :: maxens
integer :: maxens2
integer :: maxens3
integer :: ensdim
integer , DIMENSION(max_domains) :: chem_opt
integer :: num_land_cat
integer :: num_soil_cat
integer , DIMENSION(max_domains) :: topo_wind
integer :: mp_zero_out
real :: mp_zero_out_thresh
real :: seaice_threshold
integer :: fractional_seaice
integer :: seaice_albedo_opt
real :: seaice_albedo_default
integer :: seaice_snowdepth_opt
real :: seaice_snowdepth_max
real :: seaice_snowdepth_min
integer :: seaice_thickness_opt
real :: seaice_thickness_default
logical :: tice2tsk_if2cold
integer :: sst_update
integer , DIMENSION(max_domains) :: sf_urban_physics
logical :: usemonalb
logical :: rdmaxalb
logical :: rdlai2d
logical :: ua_phys
integer , DIMENSION(max_domains) :: gwd_opt
integer :: iz0tlnd
real , DIMENSION(max_domains) :: sas_pgcon
real , DIMENSION(max_domains) :: sas_shal_pgcon
integer , DIMENSION(max_domains) :: sas_shal_conv
real , DIMENSION(max_domains) :: sas_mass_flux
real :: var_ric
real :: coef_ric_l
real :: coef_ric_s
integer , DIMENSION(max_domains) :: random_seed
integer , DIMENSION(max_domains) :: icoef_sf
logical , DIMENSION(max_domains) :: lcurr_sf
integer , DIMENSION(max_domains) :: ens_random_seed
logical :: pert_sas
logical :: pert_pbl
real , DIMENSION(max_domains) :: ens_sasamp
real , DIMENSION(max_domains) :: ens_pblamp
integer , DIMENSION(max_domains) :: idtad
integer , DIMENSION(max_domains) :: nsoil
integer , DIMENSION(max_domains) :: nphs
integer , DIMENSION(max_domains) :: ncnvc
integer , DIMENSION(max_domains) :: nrand
integer , DIMENSION(max_domains) :: nrads
integer , DIMENSION(max_domains) :: nradl
real , DIMENSION(max_domains) :: tprec
real , DIMENSION(max_domains) :: theat
real , DIMENSION(max_domains) :: tclod
real , DIMENSION(max_domains) :: trdsw
real , DIMENSION(max_domains) :: trdlw
real , DIMENSION(max_domains) :: tsrfc
logical , DIMENSION(max_domains) :: pcpflg
integer , DIMENSION(max_domains) :: sigma
real , DIMENSION(max_domains) :: sfenth
integer :: co2tf
integer :: ra_call_offset
real :: cam_abs_freq_s
integer :: levsiz
integer :: paerlev
integer :: cam_abs_dim1
integer :: cam_abs_dim2
integer :: no_src_types
integer :: alevsiz
integer :: o3input
integer :: aer_opt
logical , DIMENSION(max_domains) :: cu_rad_feedback
integer :: icloud_cu
real , DIMENSION(max_domains) :: h_diff
integer , DIMENSION(max_domains) :: movemin
integer :: num_snso_layers
integer :: num_snow_layers
logical :: use_aero_icbc
real :: ccn_conc
integer :: hail_opt
integer , DIMENSION(max_domains) :: sf_lake_physics
integer :: dyn_opt
integer :: rk_ord
integer :: w_damping
integer , DIMENSION(max_domains) :: diff_opt
integer , DIMENSION(max_domains) :: km_opt
integer :: damp_opt
real , DIMENSION(max_domains) :: zdamp
real :: base_pres
real :: base_temp
real :: base_lapse
real :: iso_temp
real , DIMENSION(max_domains) :: dampcoef
real , DIMENSION(max_domains) :: khdif
real , DIMENSION(max_domains) :: kvdif
real , DIMENSION(max_domains) :: c_s
real , DIMENSION(max_domains) :: c_k
real , DIMENSION(max_domains) :: smdiv
real , DIMENSION(max_domains) :: emdiv
real , DIMENSION(max_domains) :: epssm
logical , DIMENSION(max_domains) :: non_hydrostatic
integer , DIMENSION(max_domains) :: time_step_sound
integer , DIMENSION(max_domains) :: h_mom_adv_order
integer , DIMENSION(max_domains) :: v_mom_adv_order
integer , DIMENSION(max_domains) :: h_sca_adv_order
integer , DIMENSION(max_domains) :: v_sca_adv_order
logical , DIMENSION(max_domains) :: top_radiation
real , DIMENSION(max_domains) :: tke_upper_bound
real , DIMENSION(max_domains) :: tke_drag_coefficient
real , DIMENSION(max_domains) :: tke_heat_flux
logical , DIMENSION(max_domains) :: pert_coriolis
logical :: euler_adv
integer :: idtadt
integer :: idtadc
real , DIMENSION(max_domains) :: codamp
real , DIMENSION(max_domains) :: coac
real , DIMENSION(max_domains) :: slophc
real , DIMENSION(max_domains) :: wp
integer :: terrain_smoothing
integer :: spec_bdy_width
integer :: spec_zone
integer :: relax_zone
logical , DIMENSION(max_domains) :: specified
logical , DIMENSION(max_domains) :: periodic_x
logical , DIMENSION(max_domains) :: symmetric_xs
logical , DIMENSION(max_domains) :: symmetric_xe
logical , DIMENSION(max_domains) :: open_xs
logical , DIMENSION(max_domains) :: open_xe
logical , DIMENSION(max_domains) :: periodic_y
logical , DIMENSION(max_domains) :: symmetric_ys
logical , DIMENSION(max_domains) :: symmetric_ye
logical , DIMENSION(max_domains) :: open_ys
logical , DIMENSION(max_domains) :: open_ye
logical , DIMENSION(max_domains) :: polar
logical , DIMENSION(max_domains) :: nested
integer :: real_data_init_type
integer :: background_proc_id
integer :: forecast_proc_id
integer :: production_status
integer :: compression
real , DIMENSION(max_domains) :: cen_lat
real , DIMENSION(max_domains) :: cen_lon
real , DIMENSION(max_domains) :: truelat1
real , DIMENSION(max_domains) :: truelat2
real , DIMENSION(max_domains) :: moad_cen_lat
real , DIMENSION(max_domains) :: stand_lon
integer :: flag_metgrid
integer :: flag_snow
integer :: flag_psfc
integer :: flag_sm000010
integer :: flag_sm010040
integer :: flag_sm040100
integer :: flag_sm100200
integer :: flag_st000010
integer :: flag_st010040
integer :: flag_st040100
integer :: flag_st100200
integer :: flag_slp
integer :: flag_soilhgt
integer :: flag_mf_xy
real , DIMENSION(max_domains) :: bdyfrq
character*256 , DIMENSION(max_domains) :: mminlu
integer , DIMENSION(max_domains) :: iswater
integer , DIMENSION(max_domains) :: islake
integer , DIMENSION(max_domains) :: isice
integer , DIMENSION(max_domains) :: isurban
integer , DIMENSION(max_domains) :: isoilwater
integer , DIMENSION(max_domains) :: map_proj
integer :: dfi_stage
integer , DIMENSION(max_domains) :: mp_physics_dfi
integer :: maxpatch
integer    :: last_item_in_struct

   END TYPE model_config_rec_type

   TYPE grid_config_rec_type






integer    :: first_item_in_struct
real :: lakedepth_default
real :: lake_min_elev
integer :: use_lakedepth
integer :: halo_debug
integer :: ntracers
integer :: vortex_tracker
real :: interest_rad_storm
real :: interest_rad_parent
real :: interest_rad_self
integer :: interest_kids
integer :: interest_self
integer :: interest_storms
integer :: swath_mode
integer :: num_old_fixes
real :: vt4_radius
real :: vt4_weightexp
real :: vt4_pmax
real :: vt4_noise_pmax
real :: vt4_noise_pmin
real :: vt4_noise_dpdr
integer :: vt4_noise_iter
real :: nomove_freq
integer :: coral_x
integer :: coral_y
integer :: tg_reset_stream
integer :: tg_option
integer :: ntornado
real :: wbd0
real :: sbd0
logical :: analysis
logical :: write_analysis
integer :: io_form_auxinput2
logical :: high_freq
integer :: high_dom
integer :: swint_opt
integer :: aer_type
integer :: aer_aod550_opt
integer :: aer_angexp_opt
integer :: aer_ssa_opt
integer :: aer_asy_opt
real :: aer_aod550_val
real :: aer_angexp_val
real :: aer_ssa_val
real :: aer_asy_val
integer :: dveg
integer :: opt_crs
integer :: opt_btr
integer :: opt_run
integer :: opt_sfc
integer :: opt_frz
integer :: opt_inf
integer :: opt_rad
integer :: opt_alb
integer :: opt_snf
integer :: opt_tbot
integer :: opt_stc
integer :: wrf_hydro
integer :: run_days
integer :: run_hours
integer :: run_minutes
integer :: run_seconds
integer :: start_year
integer :: start_month
integer :: start_day
integer :: start_hour
integer :: start_minute
integer :: start_second
integer :: end_year
integer :: end_month
integer :: end_day
integer :: end_hour
integer :: end_minute
integer :: end_second
integer :: interval_seconds
logical :: input_from_file
integer :: fine_input_stream
character*256 :: auxinput1_inname
integer :: io_form_auxinput1
logical :: override_restart_timers
character*256 :: auxhist1_inname
character*256 :: auxhist1_outname
integer :: auxhist1_interval_y
integer :: auxhist1_interval_d
integer :: auxhist1_interval_h
integer :: auxhist1_interval_m
integer :: auxhist1_interval_s
integer :: auxhist1_interval
integer :: auxhist1_begin_y
integer :: auxhist1_begin_d
integer :: auxhist1_begin_h
integer :: auxhist1_begin_m
integer :: auxhist1_begin_s
integer :: auxhist1_begin
integer :: auxhist1_end_y
integer :: auxhist1_end_d
integer :: auxhist1_end_h
integer :: auxhist1_end_m
integer :: auxhist1_end_s
integer :: auxhist1_end
integer :: io_form_auxhist1
integer :: frames_per_auxhist1
character*256 :: auxhist2_inname
character*256 :: auxhist2_outname
integer :: auxhist2_interval_y
integer :: auxhist2_interval_d
integer :: auxhist2_interval_h
integer :: auxhist2_interval_m
integer :: auxhist2_interval_s
integer :: auxhist2_interval
integer :: auxhist2_begin_y
integer :: auxhist2_begin_d
integer :: auxhist2_begin_h
integer :: auxhist2_begin_m
integer :: auxhist2_begin_s
integer :: auxhist2_begin
integer :: auxhist2_end_y
integer :: auxhist2_end_d
integer :: auxhist2_end_h
integer :: auxhist2_end_m
integer :: auxhist2_end_s
integer :: auxhist2_end
integer :: io_form_auxhist2
integer :: frames_per_auxhist2
character*256 :: auxhist3_inname
character*256 :: auxhist3_outname
integer :: auxhist3_interval_y
integer :: auxhist3_interval_d
integer :: auxhist3_interval_h
integer :: auxhist3_interval_m
integer :: auxhist3_interval_s
integer :: auxhist3_interval
integer :: auxhist3_begin_y
integer :: auxhist3_begin_d
integer :: auxhist3_begin_h
integer :: auxhist3_begin_m
integer :: auxhist3_begin_s
integer :: auxhist3_begin
integer :: auxhist3_end_y
integer :: auxhist3_end_d
integer :: auxhist3_end_h
integer :: auxhist3_end_m
integer :: auxhist3_end_s
integer :: auxhist3_end
integer :: io_form_auxhist3
integer :: frames_per_auxhist3
character*256 :: auxhist4_inname
character*256 :: auxhist4_outname
integer :: auxhist4_interval_y
integer :: auxhist4_interval_d
integer :: auxhist4_interval_h
integer :: auxhist4_interval_m
integer :: auxhist4_interval_s
integer :: auxhist4_interval
integer :: auxhist4_begin_y
integer :: auxhist4_begin_d
integer :: auxhist4_begin_h
integer :: auxhist4_begin_m
integer :: auxhist4_begin_s
integer :: auxhist4_begin
integer :: auxhist4_end_y
integer :: auxhist4_end_d
integer :: auxhist4_end_h
integer :: auxhist4_end_m
integer :: auxhist4_end_s
integer :: auxhist4_end
integer :: io_form_auxhist4
integer :: frames_per_auxhist4
character*256 :: auxhist5_inname
character*256 :: auxhist5_outname
integer :: auxhist5_interval_y
integer :: auxhist5_interval_d
integer :: auxhist5_interval_h
integer :: auxhist5_interval_m
integer :: auxhist5_interval_s
integer :: auxhist5_interval
integer :: auxhist5_begin_y
integer :: auxhist5_begin_d
integer :: auxhist5_begin_h
integer :: auxhist5_begin_m
integer :: auxhist5_begin_s
integer :: auxhist5_begin
integer :: auxhist5_end_y
integer :: auxhist5_end_d
integer :: auxhist5_end_h
integer :: auxhist5_end_m
integer :: auxhist5_end_s
integer :: auxhist5_end
integer :: io_form_auxhist5
integer :: frames_per_auxhist5
character*256 :: auxhist6_inname
character*256 :: auxhist6_outname
integer :: auxhist6_interval_y
integer :: auxhist6_interval_d
integer :: auxhist6_interval_h
integer :: auxhist6_interval_m
integer :: auxhist6_interval_s
integer :: auxhist6_interval
integer :: auxhist6_begin_y
integer :: auxhist6_begin_d
integer :: auxhist6_begin_h
integer :: auxhist6_begin_m
integer :: auxhist6_begin_s
integer :: auxhist6_begin
integer :: auxhist6_end_y
integer :: auxhist6_end_d
integer :: auxhist6_end_h
integer :: auxhist6_end_m
integer :: auxhist6_end_s
integer :: auxhist6_end
integer :: io_form_auxhist6
integer :: frames_per_auxhist6
character*256 :: auxhist7_inname
character*256 :: auxhist7_outname
integer :: auxhist7_interval_y
integer :: auxhist7_interval_d
integer :: auxhist7_interval_h
integer :: auxhist7_interval_m
integer :: auxhist7_interval_s
integer :: auxhist7_interval
integer :: auxhist7_begin_y
integer :: auxhist7_begin_d
integer :: auxhist7_begin_h
integer :: auxhist7_begin_m
integer :: auxhist7_begin_s
integer :: auxhist7_begin
integer :: auxhist7_end_y
integer :: auxhist7_end_d
integer :: auxhist7_end_h
integer :: auxhist7_end_m
integer :: auxhist7_end_s
integer :: auxhist7_end
integer :: io_form_auxhist7
integer :: frames_per_auxhist7
character*256 :: auxhist8_inname
character*256 :: auxhist8_outname
integer :: auxhist8_interval_y
integer :: auxhist8_interval_d
integer :: auxhist8_interval_h
integer :: auxhist8_interval_m
integer :: auxhist8_interval_s
integer :: auxhist8_interval
integer :: auxhist8_begin_y
integer :: auxhist8_begin_d
integer :: auxhist8_begin_h
integer :: auxhist8_begin_m
integer :: auxhist8_begin_s
integer :: auxhist8_begin
integer :: auxhist8_end_y
integer :: auxhist8_end_d
integer :: auxhist8_end_h
integer :: auxhist8_end_m
integer :: auxhist8_end_s
integer :: auxhist8_end
integer :: io_form_auxhist8
integer :: frames_per_auxhist8
character*256 :: auxhist9_inname
character*256 :: auxhist9_outname
integer :: auxhist9_interval_y
integer :: auxhist9_interval_d
integer :: auxhist9_interval_h
integer :: auxhist9_interval_m
integer :: auxhist9_interval_s
integer :: auxhist9_interval
integer :: auxhist9_begin_y
integer :: auxhist9_begin_d
integer :: auxhist9_begin_h
integer :: auxhist9_begin_m
integer :: auxhist9_begin_s
integer :: auxhist9_begin
integer :: auxhist9_end_y
integer :: auxhist9_end_d
integer :: auxhist9_end_h
integer :: auxhist9_end_m
integer :: auxhist9_end_s
integer :: auxhist9_end
integer :: io_form_auxhist9
integer :: frames_per_auxhist9
character*256 :: auxhist10_inname
character*256 :: auxhist10_outname
integer :: auxhist10_interval_y
integer :: auxhist10_interval_d
integer :: auxhist10_interval_h
integer :: auxhist10_interval_m
integer :: auxhist10_interval_s
integer :: auxhist10_interval
integer :: auxhist10_begin_y
integer :: auxhist10_begin_d
integer :: auxhist10_begin_h
integer :: auxhist10_begin_m
integer :: auxhist10_begin_s
integer :: auxhist10_begin
integer :: auxhist10_end_y
integer :: auxhist10_end_d
integer :: auxhist10_end_h
integer :: auxhist10_end_m
integer :: auxhist10_end_s
integer :: auxhist10_end
integer :: io_form_auxhist10
integer :: frames_per_auxhist10
character*256 :: auxhist11_inname
character*256 :: auxhist11_outname
integer :: auxhist11_interval_y
integer :: auxhist11_interval_d
integer :: auxhist11_interval_h
integer :: auxhist11_interval_m
integer :: auxhist11_interval_s
integer :: auxhist11_interval
integer :: auxhist11_begin_y
integer :: auxhist11_begin_d
integer :: auxhist11_begin_h
integer :: auxhist11_begin_m
integer :: auxhist11_begin_s
integer :: auxhist11_begin
integer :: auxhist11_end_y
integer :: auxhist11_end_d
integer :: auxhist11_end_h
integer :: auxhist11_end_m
integer :: auxhist11_end_s
integer :: auxhist11_end
integer :: io_form_auxhist11
integer :: frames_per_auxhist11
character*256 :: auxhist12_inname
character*256 :: auxhist12_outname
integer :: auxhist12_interval_y
integer :: auxhist12_interval_d
integer :: auxhist12_interval_h
integer :: auxhist12_interval_m
integer :: auxhist12_interval_s
integer :: auxhist12_interval
integer :: auxhist12_begin_y
integer :: auxhist12_begin_d
integer :: auxhist12_begin_h
integer :: auxhist12_begin_m
integer :: auxhist12_begin_s
integer :: auxhist12_begin
integer :: auxhist12_end_y
integer :: auxhist12_end_d
integer :: auxhist12_end_h
integer :: auxhist12_end_m
integer :: auxhist12_end_s
integer :: auxhist12_end
integer :: io_form_auxhist12
integer :: frames_per_auxhist12
character*256 :: auxhist13_inname
character*256 :: auxhist13_outname
integer :: auxhist13_interval_y
integer :: auxhist13_interval_d
integer :: auxhist13_interval_h
integer :: auxhist13_interval_m
integer :: auxhist13_interval_s
integer :: auxhist13_interval
integer :: auxhist13_begin_y
integer :: auxhist13_begin_d
integer :: auxhist13_begin_h
integer :: auxhist13_begin_m
integer :: auxhist13_begin_s
integer :: auxhist13_begin
integer :: auxhist13_end_y
integer :: auxhist13_end_d
integer :: auxhist13_end_h
integer :: auxhist13_end_m
integer :: auxhist13_end_s
integer :: auxhist13_end
integer :: io_form_auxhist13
integer :: frames_per_auxhist13
character*256 :: auxhist14_inname
character*256 :: auxhist14_outname
integer :: auxhist14_interval_y
integer :: auxhist14_interval_d
integer :: auxhist14_interval_h
integer :: auxhist14_interval_m
integer :: auxhist14_interval_s
integer :: auxhist14_interval
integer :: auxhist14_begin_y
integer :: auxhist14_begin_d
integer :: auxhist14_begin_h
integer :: auxhist14_begin_m
integer :: auxhist14_begin_s
integer :: auxhist14_begin
integer :: auxhist14_end_y
integer :: auxhist14_end_d
integer :: auxhist14_end_h
integer :: auxhist14_end_m
integer :: auxhist14_end_s
integer :: auxhist14_end
integer :: io_form_auxhist14
integer :: frames_per_auxhist14
character*256 :: auxhist15_inname
character*256 :: auxhist15_outname
integer :: auxhist15_interval_y
integer :: auxhist15_interval_d
integer :: auxhist15_interval_h
integer :: auxhist15_interval_m
integer :: auxhist15_interval_s
integer :: auxhist15_interval
integer :: auxhist15_begin_y
integer :: auxhist15_begin_d
integer :: auxhist15_begin_h
integer :: auxhist15_begin_m
integer :: auxhist15_begin_s
integer :: auxhist15_begin
integer :: auxhist15_end_y
integer :: auxhist15_end_d
integer :: auxhist15_end_h
integer :: auxhist15_end_m
integer :: auxhist15_end_s
integer :: auxhist15_end
integer :: io_form_auxhist15
integer :: frames_per_auxhist15
character*256 :: auxhist16_inname
character*256 :: auxhist16_outname
integer :: auxhist16_interval_y
integer :: auxhist16_interval_d
integer :: auxhist16_interval_h
integer :: auxhist16_interval_m
integer :: auxhist16_interval_s
integer :: auxhist16_interval
integer :: auxhist16_begin_y
integer :: auxhist16_begin_d
integer :: auxhist16_begin_h
integer :: auxhist16_begin_m
integer :: auxhist16_begin_s
integer :: auxhist16_begin
integer :: auxhist16_end_y
integer :: auxhist16_end_d
integer :: auxhist16_end_h
integer :: auxhist16_end_m
integer :: auxhist16_end_s
integer :: auxhist16_end
integer :: io_form_auxhist16
integer :: frames_per_auxhist16
character*256 :: auxhist17_inname
character*256 :: auxhist17_outname
integer :: auxhist17_interval_y
integer :: auxhist17_interval_d
integer :: auxhist17_interval_h
integer :: auxhist17_interval_m
integer :: auxhist17_interval_s
integer :: auxhist17_interval
integer :: auxhist17_begin_y
integer :: auxhist17_begin_d
integer :: auxhist17_begin_h
integer :: auxhist17_begin_m
integer :: auxhist17_begin_s
integer :: auxhist17_begin
integer :: auxhist17_end_y
integer :: auxhist17_end_d
integer :: auxhist17_end_h
integer :: auxhist17_end_m
integer :: auxhist17_end_s
integer :: auxhist17_end
integer :: io_form_auxhist17
integer :: frames_per_auxhist17
character*256 :: auxhist18_inname
character*256 :: auxhist18_outname
integer :: auxhist18_interval_y
integer :: auxhist18_interval_d
integer :: auxhist18_interval_h
integer :: auxhist18_interval_m
integer :: auxhist18_interval_s
integer :: auxhist18_interval
integer :: auxhist18_begin_y
integer :: auxhist18_begin_d
integer :: auxhist18_begin_h
integer :: auxhist18_begin_m
integer :: auxhist18_begin_s
integer :: auxhist18_begin
integer :: auxhist18_end_y
integer :: auxhist18_end_d
integer :: auxhist18_end_h
integer :: auxhist18_end_m
integer :: auxhist18_end_s
integer :: auxhist18_end
integer :: io_form_auxhist18
integer :: frames_per_auxhist18
character*256 :: auxhist19_inname
character*256 :: auxhist19_outname
integer :: auxhist19_interval_y
integer :: auxhist19_interval_d
integer :: auxhist19_interval_h
integer :: auxhist19_interval_m
integer :: auxhist19_interval_s
integer :: auxhist19_interval
integer :: auxhist19_begin_y
integer :: auxhist19_begin_d
integer :: auxhist19_begin_h
integer :: auxhist19_begin_m
integer :: auxhist19_begin_s
integer :: auxhist19_begin
integer :: auxhist19_end_y
integer :: auxhist19_end_d
integer :: auxhist19_end_h
integer :: auxhist19_end_m
integer :: auxhist19_end_s
integer :: auxhist19_end
integer :: io_form_auxhist19
integer :: frames_per_auxhist19
character*256 :: auxhist20_inname
character*256 :: auxhist20_outname
integer :: auxhist20_interval_y
integer :: auxhist20_interval_d
integer :: auxhist20_interval_h
integer :: auxhist20_interval_m
integer :: auxhist20_interval_s
integer :: auxhist20_interval
integer :: auxhist20_begin_y
integer :: auxhist20_begin_d
integer :: auxhist20_begin_h
integer :: auxhist20_begin_m
integer :: auxhist20_begin_s
integer :: auxhist20_begin
integer :: auxhist20_end_y
integer :: auxhist20_end_d
integer :: auxhist20_end_h
integer :: auxhist20_end_m
integer :: auxhist20_end_s
integer :: auxhist20_end
integer :: io_form_auxhist20
integer :: frames_per_auxhist20
character*256 :: auxhist21_inname
character*256 :: auxhist21_outname
integer :: auxhist21_interval_y
integer :: auxhist21_interval_d
integer :: auxhist21_interval_h
integer :: auxhist21_interval_m
integer :: auxhist21_interval_s
integer :: auxhist21_interval
integer :: auxhist21_begin_y
integer :: auxhist21_begin_d
integer :: auxhist21_begin_h
integer :: auxhist21_begin_m
integer :: auxhist21_begin_s
integer :: auxhist21_begin
integer :: auxhist21_end_y
integer :: auxhist21_end_d
integer :: auxhist21_end_h
integer :: auxhist21_end_m
integer :: auxhist21_end_s
integer :: auxhist21_end
integer :: io_form_auxhist21
integer :: frames_per_auxhist21
character*256 :: auxhist22_inname
character*256 :: auxhist22_outname
integer :: auxhist22_interval_y
integer :: auxhist22_interval_d
integer :: auxhist22_interval_h
integer :: auxhist22_interval_m
integer :: auxhist22_interval_s
integer :: auxhist22_interval
integer :: auxhist22_begin_y
integer :: auxhist22_begin_d
integer :: auxhist22_begin_h
integer :: auxhist22_begin_m
integer :: auxhist22_begin_s
integer :: auxhist22_begin
integer :: auxhist22_end_y
integer :: auxhist22_end_d
integer :: auxhist22_end_h
integer :: auxhist22_end_m
integer :: auxhist22_end_s
integer :: auxhist22_end
integer :: io_form_auxhist22
integer :: frames_per_auxhist22
character*256 :: auxhist23_inname
character*256 :: auxhist23_outname
integer :: auxhist23_interval_y
integer :: auxhist23_interval_d
integer :: auxhist23_interval_h
integer :: auxhist23_interval_m
integer :: auxhist23_interval_s
integer :: auxhist23_interval
integer :: auxhist23_begin_y
integer :: auxhist23_begin_d
integer :: auxhist23_begin_h
integer :: auxhist23_begin_m
integer :: auxhist23_begin_s
integer :: auxhist23_begin
integer :: auxhist23_end_y
integer :: auxhist23_end_d
integer :: auxhist23_end_h
integer :: auxhist23_end_m
integer :: auxhist23_end_s
integer :: auxhist23_end
integer :: io_form_auxhist23
integer :: frames_per_auxhist23
character*256 :: auxhist24_inname
character*256 :: auxhist24_outname
integer :: auxhist24_interval_y
integer :: auxhist24_interval_d
integer :: auxhist24_interval_h
integer :: auxhist24_interval_m
integer :: auxhist24_interval_s
integer :: auxhist24_interval
integer :: auxhist24_begin_y
integer :: auxhist24_begin_d
integer :: auxhist24_begin_h
integer :: auxhist24_begin_m
integer :: auxhist24_begin_s
integer :: auxhist24_begin
integer :: auxhist24_end_y
integer :: auxhist24_end_d
integer :: auxhist24_end_h
integer :: auxhist24_end_m
integer :: auxhist24_end_s
integer :: auxhist24_end
integer :: io_form_auxhist24
integer :: frames_per_auxhist24
character*256 :: auxinput1_outname
integer :: auxinput1_interval_y
integer :: auxinput1_interval_d
integer :: auxinput1_interval_h
integer :: auxinput1_interval_m
integer :: auxinput1_interval_s
integer :: auxinput1_interval
integer :: auxinput1_begin_y
integer :: auxinput1_begin_d
integer :: auxinput1_begin_h
integer :: auxinput1_begin_m
integer :: auxinput1_begin_s
integer :: auxinput1_begin
integer :: auxinput1_end_y
integer :: auxinput1_end_d
integer :: auxinput1_end_h
integer :: auxinput1_end_m
integer :: auxinput1_end_s
integer :: auxinput1_end
integer :: frames_per_auxinput1
character*256 :: auxinput2_inname
character*256 :: auxinput2_outname
integer :: auxinput2_interval_y
integer :: auxinput2_interval_d
integer :: auxinput2_interval_h
integer :: auxinput2_interval_m
integer :: auxinput2_interval_s
integer :: auxinput2_interval
integer :: auxinput2_begin_y
integer :: auxinput2_begin_d
integer :: auxinput2_begin_h
integer :: auxinput2_begin_m
integer :: auxinput2_begin_s
integer :: auxinput2_begin
integer :: auxinput2_end_y
integer :: auxinput2_end_d
integer :: auxinput2_end_h
integer :: auxinput2_end_m
integer :: auxinput2_end_s
integer :: auxinput2_end
integer :: frames_per_auxinput2
character*256 :: auxinput3_inname
character*256 :: auxinput3_outname
integer :: auxinput3_interval_y
integer :: auxinput3_interval_d
integer :: auxinput3_interval_h
integer :: auxinput3_interval_m
integer :: auxinput3_interval_s
integer :: auxinput3_interval
integer :: auxinput3_begin_y
integer :: auxinput3_begin_d
integer :: auxinput3_begin_h
integer :: auxinput3_begin_m
integer :: auxinput3_begin_s
integer :: auxinput3_begin
integer :: auxinput3_end_y
integer :: auxinput3_end_d
integer :: auxinput3_end_h
integer :: auxinput3_end_m
integer :: auxinput3_end_s
integer :: auxinput3_end
integer :: io_form_auxinput3
integer :: frames_per_auxinput3
character*256 :: auxinput4_inname
character*256 :: auxinput4_outname
integer :: auxinput4_interval_y
integer :: auxinput4_interval_d
integer :: auxinput4_interval_h
integer :: auxinput4_interval_m
integer :: auxinput4_interval_s
integer :: auxinput4_interval
integer :: auxinput4_begin_y
integer :: auxinput4_begin_d
integer :: auxinput4_begin_h
integer :: auxinput4_begin_m
integer :: auxinput4_begin_s
integer :: auxinput4_begin
integer :: auxinput4_end_y
integer :: auxinput4_end_d
integer :: auxinput4_end_h
integer :: auxinput4_end_m
integer :: auxinput4_end_s
integer :: auxinput4_end
integer :: io_form_auxinput4
integer :: frames_per_auxinput4
character*256 :: auxinput5_inname
character*256 :: auxinput5_outname
integer :: auxinput5_interval_y
integer :: auxinput5_interval_d
integer :: auxinput5_interval_h
integer :: auxinput5_interval_m
integer :: auxinput5_interval_s
integer :: auxinput5_interval
integer :: auxinput5_begin_y
integer :: auxinput5_begin_d
integer :: auxinput5_begin_h
integer :: auxinput5_begin_m
integer :: auxinput5_begin_s
integer :: auxinput5_begin
integer :: auxinput5_end_y
integer :: auxinput5_end_d
integer :: auxinput5_end_h
integer :: auxinput5_end_m
integer :: auxinput5_end_s
integer :: auxinput5_end
integer :: io_form_auxinput5
integer :: frames_per_auxinput5
character*256 :: auxinput6_inname
character*256 :: auxinput6_outname
integer :: auxinput6_interval_y
integer :: auxinput6_interval_d
integer :: auxinput6_interval_h
integer :: auxinput6_interval_m
integer :: auxinput6_interval_s
integer :: auxinput6_interval
integer :: auxinput6_begin_y
integer :: auxinput6_begin_d
integer :: auxinput6_begin_h
integer :: auxinput6_begin_m
integer :: auxinput6_begin_s
integer :: auxinput6_begin
integer :: auxinput6_end_y
integer :: auxinput6_end_d
integer :: auxinput6_end_h
integer :: auxinput6_end_m
integer :: auxinput6_end_s
integer :: auxinput6_end
integer :: io_form_auxinput6
integer :: frames_per_auxinput6
character*256 :: auxinput7_inname
character*256 :: auxinput7_outname
integer :: auxinput7_interval_y
integer :: auxinput7_interval_d
integer :: auxinput7_interval_h
integer :: auxinput7_interval_m
integer :: auxinput7_interval_s
integer :: auxinput7_interval
integer :: auxinput7_begin_y
integer :: auxinput7_begin_d
integer :: auxinput7_begin_h
integer :: auxinput7_begin_m
integer :: auxinput7_begin_s
integer :: auxinput7_begin
integer :: auxinput7_end_y
integer :: auxinput7_end_d
integer :: auxinput7_end_h
integer :: auxinput7_end_m
integer :: auxinput7_end_s
integer :: auxinput7_end
integer :: io_form_auxinput7
integer :: frames_per_auxinput7
character*256 :: auxinput8_inname
character*256 :: auxinput8_outname
integer :: auxinput8_interval_y
integer :: auxinput8_interval_d
integer :: auxinput8_interval_h
integer :: auxinput8_interval_m
integer :: auxinput8_interval_s
integer :: auxinput8_interval
integer :: auxinput8_begin_y
integer :: auxinput8_begin_d
integer :: auxinput8_begin_h
integer :: auxinput8_begin_m
integer :: auxinput8_begin_s
integer :: auxinput8_begin
integer :: auxinput8_end_y
integer :: auxinput8_end_d
integer :: auxinput8_end_h
integer :: auxinput8_end_m
integer :: auxinput8_end_s
integer :: auxinput8_end
integer :: io_form_auxinput8
integer :: frames_per_auxinput8
character*256 :: auxinput9_inname
character*256 :: auxinput9_outname
integer :: auxinput9_interval_y
integer :: auxinput9_interval_d
integer :: auxinput9_interval_h
integer :: auxinput9_interval_m
integer :: auxinput9_interval_s
integer :: auxinput9_interval
integer :: auxinput9_begin_y
integer :: auxinput9_begin_d
integer :: auxinput9_begin_h
integer :: auxinput9_begin_m
integer :: auxinput9_begin_s
integer :: auxinput9_begin
integer :: auxinput9_end_y
integer :: auxinput9_end_d
integer :: auxinput9_end_h
integer :: auxinput9_end_m
integer :: auxinput9_end_s
integer :: auxinput9_end
integer :: io_form_auxinput9
integer :: frames_per_auxinput9
character*256 :: auxinput10_inname
character*256 :: auxinput10_outname
integer :: auxinput10_interval_y
integer :: auxinput10_interval_d
integer :: auxinput10_interval_h
integer :: auxinput10_interval_m
integer :: auxinput10_interval_s
integer :: auxinput10_interval
integer :: auxinput10_begin_y
integer :: auxinput10_begin_d
integer :: auxinput10_begin_h
integer :: auxinput10_begin_m
integer :: auxinput10_begin_s
integer :: auxinput10_begin
integer :: auxinput10_end_y
integer :: auxinput10_end_d
integer :: auxinput10_end_h
integer :: auxinput10_end_m
integer :: auxinput10_end_s
integer :: auxinput10_end
integer :: io_form_auxinput10
integer :: frames_per_auxinput10
character*256 :: auxinput11_inname
character*256 :: auxinput11_outname
integer :: auxinput11_interval_y
integer :: auxinput11_interval_d
integer :: auxinput11_interval_h
integer :: auxinput11_interval_m
integer :: auxinput11_interval_s
integer :: auxinput11_interval
integer :: auxinput11_begin_y
integer :: auxinput11_begin_d
integer :: auxinput11_begin_h
integer :: auxinput11_begin_m
integer :: auxinput11_begin_s
integer :: auxinput11_begin
integer :: auxinput11_end_y
integer :: auxinput11_end_d
integer :: auxinput11_end_h
integer :: auxinput11_end_m
integer :: auxinput11_end_s
integer :: auxinput11_end
integer :: io_form_auxinput11
integer :: frames_per_auxinput11
character*256 :: auxinput12_inname
character*256 :: auxinput12_outname
integer :: auxinput12_interval_y
integer :: auxinput12_interval_d
integer :: auxinput12_interval_h
integer :: auxinput12_interval_m
integer :: auxinput12_interval_s
integer :: auxinput12_interval
integer :: auxinput12_begin_y
integer :: auxinput12_begin_d
integer :: auxinput12_begin_h
integer :: auxinput12_begin_m
integer :: auxinput12_begin_s
integer :: auxinput12_begin
integer :: auxinput12_end_y
integer :: auxinput12_end_d
integer :: auxinput12_end_h
integer :: auxinput12_end_m
integer :: auxinput12_end_s
integer :: auxinput12_end
integer :: io_form_auxinput12
integer :: frames_per_auxinput12
character*256 :: auxinput13_inname
character*256 :: auxinput13_outname
integer :: auxinput13_interval_y
integer :: auxinput13_interval_d
integer :: auxinput13_interval_h
integer :: auxinput13_interval_m
integer :: auxinput13_interval_s
integer :: auxinput13_interval
integer :: auxinput13_begin_y
integer :: auxinput13_begin_d
integer :: auxinput13_begin_h
integer :: auxinput13_begin_m
integer :: auxinput13_begin_s
integer :: auxinput13_begin
integer :: auxinput13_end_y
integer :: auxinput13_end_d
integer :: auxinput13_end_h
integer :: auxinput13_end_m
integer :: auxinput13_end_s
integer :: auxinput13_end
integer :: io_form_auxinput13
integer :: frames_per_auxinput13
character*256 :: auxinput14_inname
character*256 :: auxinput14_outname
integer :: auxinput14_interval_y
integer :: auxinput14_interval_d
integer :: auxinput14_interval_h
integer :: auxinput14_interval_m
integer :: auxinput14_interval_s
integer :: auxinput14_interval
integer :: auxinput14_begin_y
integer :: auxinput14_begin_d
integer :: auxinput14_begin_h
integer :: auxinput14_begin_m
integer :: auxinput14_begin_s
integer :: auxinput14_begin
integer :: auxinput14_end_y
integer :: auxinput14_end_d
integer :: auxinput14_end_h
integer :: auxinput14_end_m
integer :: auxinput14_end_s
integer :: auxinput14_end
integer :: io_form_auxinput14
integer :: frames_per_auxinput14
character*256 :: auxinput15_inname
character*256 :: auxinput15_outname
integer :: auxinput15_interval_y
integer :: auxinput15_interval_d
integer :: auxinput15_interval_h
integer :: auxinput15_interval_m
integer :: auxinput15_interval_s
integer :: auxinput15_interval
integer :: auxinput15_begin_y
integer :: auxinput15_begin_d
integer :: auxinput15_begin_h
integer :: auxinput15_begin_m
integer :: auxinput15_begin_s
integer :: auxinput15_begin
integer :: auxinput15_end_y
integer :: auxinput15_end_d
integer :: auxinput15_end_h
integer :: auxinput15_end_m
integer :: auxinput15_end_s
integer :: auxinput15_end
integer :: io_form_auxinput15
integer :: frames_per_auxinput15
character*256 :: auxinput16_inname
character*256 :: auxinput16_outname
integer :: auxinput16_interval_y
integer :: auxinput16_interval_d
integer :: auxinput16_interval_h
integer :: auxinput16_interval_m
integer :: auxinput16_interval_s
integer :: auxinput16_interval
integer :: auxinput16_begin_y
integer :: auxinput16_begin_d
integer :: auxinput16_begin_h
integer :: auxinput16_begin_m
integer :: auxinput16_begin_s
integer :: auxinput16_begin
integer :: auxinput16_end_y
integer :: auxinput16_end_d
integer :: auxinput16_end_h
integer :: auxinput16_end_m
integer :: auxinput16_end_s
integer :: auxinput16_end
integer :: io_form_auxinput16
integer :: frames_per_auxinput16
character*256 :: auxinput17_inname
character*256 :: auxinput17_outname
integer :: auxinput17_interval_y
integer :: auxinput17_interval_d
integer :: auxinput17_interval_h
integer :: auxinput17_interval_m
integer :: auxinput17_interval_s
integer :: auxinput17_interval
integer :: auxinput17_begin_y
integer :: auxinput17_begin_d
integer :: auxinput17_begin_h
integer :: auxinput17_begin_m
integer :: auxinput17_begin_s
integer :: auxinput17_begin
integer :: auxinput17_end_y
integer :: auxinput17_end_d
integer :: auxinput17_end_h
integer :: auxinput17_end_m
integer :: auxinput17_end_s
integer :: auxinput17_end
integer :: io_form_auxinput17
integer :: frames_per_auxinput17
character*256 :: auxinput18_inname
character*256 :: auxinput18_outname
integer :: auxinput18_interval_y
integer :: auxinput18_interval_d
integer :: auxinput18_interval_h
integer :: auxinput18_interval_m
integer :: auxinput18_interval_s
integer :: auxinput18_interval
integer :: auxinput18_begin_y
integer :: auxinput18_begin_d
integer :: auxinput18_begin_h
integer :: auxinput18_begin_m
integer :: auxinput18_begin_s
integer :: auxinput18_begin
integer :: auxinput18_end_y
integer :: auxinput18_end_d
integer :: auxinput18_end_h
integer :: auxinput18_end_m
integer :: auxinput18_end_s
integer :: auxinput18_end
integer :: io_form_auxinput18
integer :: frames_per_auxinput18
character*256 :: auxinput19_inname
character*256 :: auxinput19_outname
integer :: auxinput19_interval_y
integer :: auxinput19_interval_d
integer :: auxinput19_interval_h
integer :: auxinput19_interval_m
integer :: auxinput19_interval_s
integer :: auxinput19_interval
integer :: auxinput19_begin_y
integer :: auxinput19_begin_d
integer :: auxinput19_begin_h
integer :: auxinput19_begin_m
integer :: auxinput19_begin_s
integer :: auxinput19_begin
integer :: auxinput19_end_y
integer :: auxinput19_end_d
integer :: auxinput19_end_h
integer :: auxinput19_end_m
integer :: auxinput19_end_s
integer :: auxinput19_end
integer :: io_form_auxinput19
integer :: frames_per_auxinput19
character*256 :: auxinput20_inname
character*256 :: auxinput20_outname
integer :: auxinput20_interval_y
integer :: auxinput20_interval_d
integer :: auxinput20_interval_h
integer :: auxinput20_interval_m
integer :: auxinput20_interval_s
integer :: auxinput20_interval
integer :: auxinput20_begin_y
integer :: auxinput20_begin_d
integer :: auxinput20_begin_h
integer :: auxinput20_begin_m
integer :: auxinput20_begin_s
integer :: auxinput20_begin
integer :: auxinput20_end_y
integer :: auxinput20_end_d
integer :: auxinput20_end_h
integer :: auxinput20_end_m
integer :: auxinput20_end_s
integer :: auxinput20_end
integer :: io_form_auxinput20
integer :: frames_per_auxinput20
character*256 :: auxinput21_inname
character*256 :: auxinput21_outname
integer :: auxinput21_interval_y
integer :: auxinput21_interval_d
integer :: auxinput21_interval_h
integer :: auxinput21_interval_m
integer :: auxinput21_interval_s
integer :: auxinput21_interval
integer :: auxinput21_begin_y
integer :: auxinput21_begin_d
integer :: auxinput21_begin_h
integer :: auxinput21_begin_m
integer :: auxinput21_begin_s
integer :: auxinput21_begin
integer :: auxinput21_end_y
integer :: auxinput21_end_d
integer :: auxinput21_end_h
integer :: auxinput21_end_m
integer :: auxinput21_end_s
integer :: auxinput21_end
integer :: io_form_auxinput21
integer :: frames_per_auxinput21
character*256 :: auxinput22_inname
character*256 :: auxinput22_outname
integer :: auxinput22_interval_y
integer :: auxinput22_interval_d
integer :: auxinput22_interval_h
integer :: auxinput22_interval_m
integer :: auxinput22_interval_s
integer :: auxinput22_interval
integer :: auxinput22_begin_y
integer :: auxinput22_begin_d
integer :: auxinput22_begin_h
integer :: auxinput22_begin_m
integer :: auxinput22_begin_s
integer :: auxinput22_begin
integer :: auxinput22_end_y
integer :: auxinput22_end_d
integer :: auxinput22_end_h
integer :: auxinput22_end_m
integer :: auxinput22_end_s
integer :: auxinput22_end
integer :: io_form_auxinput22
integer :: frames_per_auxinput22
character*256 :: auxinput23_inname
character*256 :: auxinput23_outname
integer :: auxinput23_interval_y
integer :: auxinput23_interval_d
integer :: auxinput23_interval_h
integer :: auxinput23_interval_m
integer :: auxinput23_interval_s
integer :: auxinput23_interval
integer :: auxinput23_begin_y
integer :: auxinput23_begin_d
integer :: auxinput23_begin_h
integer :: auxinput23_begin_m
integer :: auxinput23_begin_s
integer :: auxinput23_begin
integer :: auxinput23_end_y
integer :: auxinput23_end_d
integer :: auxinput23_end_h
integer :: auxinput23_end_m
integer :: auxinput23_end_s
integer :: auxinput23_end
integer :: io_form_auxinput23
integer :: frames_per_auxinput23
character*256 :: auxinput24_inname
character*256 :: auxinput24_outname
integer :: auxinput24_interval_y
integer :: auxinput24_interval_d
integer :: auxinput24_interval_h
integer :: auxinput24_interval_m
integer :: auxinput24_interval_s
integer :: auxinput24_interval
integer :: auxinput24_begin_y
integer :: auxinput24_begin_d
integer :: auxinput24_begin_h
integer :: auxinput24_begin_m
integer :: auxinput24_begin_s
integer :: auxinput24_begin
integer :: auxinput24_end_y
integer :: auxinput24_end_d
integer :: auxinput24_end_h
integer :: auxinput24_end_m
integer :: auxinput24_end_s
integer :: auxinput24_end
integer :: io_form_auxinput24
integer :: frames_per_auxinput24
integer :: history_interval
integer :: frames_per_outfile
logical :: restart
integer :: restart_interval
integer :: io_form_input
integer :: io_form_history
integer :: io_form_restart
integer :: io_form_boundary
integer :: debug_level
logical :: self_test_domain
character*256 :: history_outname
character*256 :: history_inname
logical :: use_netcdf_classic
integer :: history_interval_d
integer :: history_interval_h
integer :: history_interval_m
integer :: history_interval_s
integer :: inputout_interval_d
integer :: inputout_interval_h
integer :: inputout_interval_m
integer :: inputout_interval_s
integer :: inputout_interval
integer :: restart_interval_d
integer :: restart_interval_h
integer :: restart_interval_m
integer :: restart_interval_s
integer :: history_begin_y
integer :: history_begin_d
integer :: history_begin_h
integer :: history_begin_m
integer :: history_begin_s
integer :: history_begin
integer :: inputout_begin_y
integer :: inputout_begin_d
integer :: inputout_begin_h
integer :: inputout_begin_m
integer :: inputout_begin_s
integer :: restart_begin_y
integer :: restart_begin_d
integer :: restart_begin_h
integer :: restart_begin_m
integer :: restart_begin_s
integer :: restart_begin
integer :: history_end_y
integer :: history_end_d
integer :: history_end_h
integer :: history_end_m
integer :: history_end_s
integer :: history_end
integer :: inputout_end_y
integer :: inputout_end_d
integer :: inputout_end_h
integer :: inputout_end_m
integer :: inputout_end_s
integer :: simulation_start_year
integer :: simulation_start_month
integer :: simulation_start_day
integer :: simulation_start_hour
integer :: simulation_start_minute
integer :: simulation_start_second
logical :: reset_simulation_start
integer :: sr_x
integer :: sr_y
character*256 :: iofields_filename
logical :: ignore_iofields_warning
logical :: ncd_nofill
integer :: julyr
integer :: julday
real :: gmt
character*256 :: high_freq_outname
character*256 :: partial_atcf_outname
character*256 :: input_inname
character*256 :: input_outname
character*256 :: bdy_inname
character*256 :: bdy_outname
character*256 :: rst_inname
character*256 :: rst_outname
character*256 :: anl_outname
logical :: write_input
logical :: write_restart_at_0h
logical :: write_hist_at_0h_rst
logical :: adjust_output_times
logical :: adjust_input_times
real :: tstart
logical :: nocolons
logical :: cycling
logical :: output_ready_flag
integer :: dfi_opt
integer :: dfi_savehydmeteors
integer :: dfi_nfilter
logical :: dfi_write_filtered_input
logical :: dfi_write_dfi_history
integer :: dfi_cutoff_seconds
integer :: dfi_time_dim
integer :: dfi_fwdstop_year
integer :: dfi_fwdstop_month
integer :: dfi_fwdstop_day
integer :: dfi_fwdstop_hour
integer :: dfi_fwdstop_minute
integer :: dfi_fwdstop_second
integer :: dfi_bckstop_year
integer :: dfi_bckstop_month
integer :: dfi_bckstop_day
integer :: dfi_bckstop_hour
integer :: dfi_bckstop_minute
integer :: dfi_bckstop_second
integer :: time_step
integer :: time_step_fract_num
integer :: time_step_fract_den
integer :: time_step_dfi
integer :: max_dom
integer :: s_we
integer :: e_we
integer :: s_sn
integer :: e_sn
integer :: s_vert
integer :: e_vert
integer :: num_metgrid_soil_levels
real :: dx
real :: dy
integer :: grid_id
logical :: grid_allowed
integer :: parent_id
integer :: i_parent_start
integer :: j_parent_start
integer :: parent_grid_ratio
integer :: parent_time_step_ratio
integer :: feedback
integer :: smooth_option
real :: ztop
integer :: moad_grid_ratio
integer :: moad_time_step_ratio
integer :: shw
integer :: tile_sz_x
integer :: tile_sz_y
integer :: numtiles
integer :: numtiles_inc
integer :: numtiles_x
integer :: numtiles_y
integer :: tile_strategy
integer :: nproc_x
integer :: nproc_y
integer :: irand
real :: dt
integer :: ts_buf_size
integer :: max_ts_locs
integer :: num_moves
integer :: vortex_interval
integer :: corral_dist
integer :: move_id
integer :: move_interval
integer :: move_cd_x
integer :: move_cd_y
logical :: swap_x
logical :: swap_y
logical :: cycle_x
logical :: cycle_y
logical :: reorder_mesh
logical :: perturb_input
real :: eta_levels
real :: ptsgm
integer :: num_metgrid_levels
real :: p_top_requested
logical :: use_prep_hybrid
logical :: force_read_thompson
logical :: write_thompson_tables
integer :: mp_physics
real :: mommix
logical :: disheat
integer :: do_radar_ref
integer :: compute_radar_ref
integer :: ra_lw_physics
integer :: ra_sw_physics
real :: radt
integer :: sf_sfclay_physics
integer :: sf_surface_physics
integer :: bl_pbl_physics
integer :: ysu_topdown_pblmix
integer :: shinhong_tke_diag
integer :: windfarm_opt
integer :: windfarm_ij
integer :: mfshconv
real :: bldt
integer :: cu_physics
integer :: shcu_physics
integer :: cu_diag
real :: gfs_alpha
real :: cudt
real :: gsmdt
integer :: isfflx
integer :: ideal_xland
integer :: ifsnow
integer :: icloud
real :: swrad_scat
integer :: surface_input_source
integer :: num_soil_layers
integer :: num_urban_layers
integer :: sf_surface_mosaic
integer :: mosaic_cat
integer :: mosaic_cat_soil
integer :: num_urban_hi
integer :: mosaic_lu
integer :: mosaic_soil
integer :: maxiens
integer :: maxens
integer :: maxens2
integer :: maxens3
integer :: ensdim
integer :: chem_opt
integer :: num_land_cat
integer :: num_soil_cat
integer :: topo_wind
integer :: mp_zero_out
real :: mp_zero_out_thresh
real :: seaice_threshold
integer :: fractional_seaice
integer :: seaice_albedo_opt
real :: seaice_albedo_default
integer :: seaice_snowdepth_opt
real :: seaice_snowdepth_max
real :: seaice_snowdepth_min
integer :: seaice_thickness_opt
real :: seaice_thickness_default
logical :: tice2tsk_if2cold
integer :: sst_update
integer :: sf_urban_physics
logical :: usemonalb
logical :: rdmaxalb
logical :: rdlai2d
logical :: ua_phys
integer :: gwd_opt
integer :: iz0tlnd
real :: sas_pgcon
real :: sas_shal_pgcon
integer :: sas_shal_conv
real :: sas_mass_flux
real :: var_ric
real :: coef_ric_l
real :: coef_ric_s
integer :: random_seed
integer :: icoef_sf
logical :: lcurr_sf
integer :: ens_random_seed
logical :: pert_sas
logical :: pert_pbl
real :: ens_sasamp
real :: ens_pblamp
integer :: idtad
integer :: nsoil
integer :: nphs
integer :: ncnvc
integer :: nrand
integer :: nrads
integer :: nradl
real :: tprec
real :: theat
real :: tclod
real :: trdsw
real :: trdlw
real :: tsrfc
logical :: pcpflg
integer :: sigma
real :: sfenth
integer :: co2tf
integer :: ra_call_offset
real :: cam_abs_freq_s
integer :: levsiz
integer :: paerlev
integer :: cam_abs_dim1
integer :: cam_abs_dim2
integer :: no_src_types
integer :: alevsiz
integer :: o3input
integer :: aer_opt
logical :: cu_rad_feedback
integer :: icloud_cu
real :: h_diff
integer :: movemin
integer :: num_snso_layers
integer :: num_snow_layers
logical :: use_aero_icbc
real :: ccn_conc
integer :: hail_opt
integer :: sf_lake_physics
integer :: dyn_opt
integer :: rk_ord
integer :: w_damping
integer :: diff_opt
integer :: km_opt
integer :: damp_opt
real :: zdamp
real :: base_pres
real :: base_temp
real :: base_lapse
real :: iso_temp
real :: dampcoef
real :: khdif
real :: kvdif
real :: c_s
real :: c_k
real :: smdiv
real :: emdiv
real :: epssm
logical :: non_hydrostatic
integer :: time_step_sound
integer :: h_mom_adv_order
integer :: v_mom_adv_order
integer :: h_sca_adv_order
integer :: v_sca_adv_order
logical :: top_radiation
real :: tke_upper_bound
real :: tke_drag_coefficient
real :: tke_heat_flux
logical :: pert_coriolis
logical :: euler_adv
integer :: idtadt
integer :: idtadc
real :: codamp
real :: coac
real :: slophc
real :: wp
integer :: terrain_smoothing
integer :: spec_bdy_width
integer :: spec_zone
integer :: relax_zone
logical :: specified
logical :: periodic_x
logical :: symmetric_xs
logical :: symmetric_xe
logical :: open_xs
logical :: open_xe
logical :: periodic_y
logical :: symmetric_ys
logical :: symmetric_ye
logical :: open_ys
logical :: open_ye
logical :: polar
logical :: nested
integer :: real_data_init_type
integer :: background_proc_id
integer :: forecast_proc_id
integer :: production_status
integer :: compression
real :: cen_lat
real :: cen_lon
real :: truelat1
real :: truelat2
real :: moad_cen_lat
real :: stand_lon
integer :: flag_metgrid
integer :: flag_snow
integer :: flag_psfc
integer :: flag_sm000010
integer :: flag_sm010040
integer :: flag_sm040100
integer :: flag_sm100200
integer :: flag_st000010
integer :: flag_st010040
integer :: flag_st040100
integer :: flag_st100200
integer :: flag_slp
integer :: flag_soilhgt
integer :: flag_mf_xy
real :: bdyfrq
character*256 :: mminlu
integer :: iswater
integer :: islake
integer :: isice
integer :: isurban
integer :: isoilwater
integer :: map_proj
integer :: dfi_stage
integer :: mp_physics_dfi
integer :: maxpatch
integer    :: last_item_in_struct

   END TYPE grid_config_rec_type

   TYPE(model_config_rec_type) :: model_config_rec







CONTAINS




   SUBROUTINE initial_config







































      IMPLICIT NONE

      INTEGER              :: io_status
      INTEGER              :: i

      LOGICAL              :: nml_read_error

      CHARACTER (LEN=1024) :: nml_name

      INTEGER, PARAMETER :: nml_write_unit= 9
      INTEGER, PARAMETER :: nml_read_unit = 10









integer    :: first_item_in_struct
real , DIMENSION(max_domains) :: lakedepth_default
real , DIMENSION(max_domains) :: lake_min_elev
integer , DIMENSION(max_domains) :: use_lakedepth
integer :: halo_debug
integer :: ntracers
integer , DIMENSION(max_domains) :: vortex_tracker
real , DIMENSION(max_domains) :: interest_rad_storm
real , DIMENSION(max_domains) :: interest_rad_parent
real , DIMENSION(max_domains) :: interest_rad_self
integer , DIMENSION(max_domains) :: interest_kids
integer , DIMENSION(max_domains) :: interest_self
integer , DIMENSION(max_domains) :: interest_storms
integer :: swath_mode
integer :: num_old_fixes
real , DIMENSION(max_domains) :: vt4_radius
real , DIMENSION(max_domains) :: vt4_weightexp
real , DIMENSION(max_domains) :: vt4_pmax
real , DIMENSION(max_domains) :: vt4_noise_pmax
real , DIMENSION(max_domains) :: vt4_noise_pmin
real , DIMENSION(max_domains) :: vt4_noise_dpdr
integer , DIMENSION(max_domains) :: vt4_noise_iter
real , DIMENSION(max_domains) :: nomove_freq
integer , DIMENSION(max_domains) :: coral_x
integer , DIMENSION(max_domains) :: coral_y
integer :: tg_reset_stream
integer :: tg_option
integer , DIMENSION(max_domains) :: ntornado
real , DIMENSION(max_domains) :: wbd0
real , DIMENSION(max_domains) :: sbd0
logical , DIMENSION(max_domains) :: analysis
logical , DIMENSION(max_domains) :: write_analysis
integer :: io_form_auxinput2
logical :: high_freq
integer :: high_dom
integer :: swint_opt
integer , DIMENSION(max_domains) :: aer_type
integer , DIMENSION(max_domains) :: aer_aod550_opt
integer , DIMENSION(max_domains) :: aer_angexp_opt
integer , DIMENSION(max_domains) :: aer_ssa_opt
integer , DIMENSION(max_domains) :: aer_asy_opt
real , DIMENSION(max_domains) :: aer_aod550_val
real , DIMENSION(max_domains) :: aer_angexp_val
real , DIMENSION(max_domains) :: aer_ssa_val
real , DIMENSION(max_domains) :: aer_asy_val
integer :: dveg
integer :: opt_crs
integer :: opt_btr
integer :: opt_run
integer :: opt_sfc
integer :: opt_frz
integer :: opt_inf
integer :: opt_rad
integer :: opt_alb
integer :: opt_snf
integer :: opt_tbot
integer :: opt_stc
integer :: wrf_hydro
integer :: run_days
integer :: run_hours
integer :: run_minutes
integer :: run_seconds
integer , DIMENSION(max_domains) :: start_year
integer , DIMENSION(max_domains) :: start_month
integer , DIMENSION(max_domains) :: start_day
integer , DIMENSION(max_domains) :: start_hour
integer , DIMENSION(max_domains) :: start_minute
integer , DIMENSION(max_domains) :: start_second
integer , DIMENSION(max_domains) :: end_year
integer , DIMENSION(max_domains) :: end_month
integer , DIMENSION(max_domains) :: end_day
integer , DIMENSION(max_domains) :: end_hour
integer , DIMENSION(max_domains) :: end_minute
integer , DIMENSION(max_domains) :: end_second
integer :: interval_seconds
logical , DIMENSION(max_domains) :: input_from_file
integer , DIMENSION(max_domains) :: fine_input_stream
character*256 :: auxinput1_inname
integer :: io_form_auxinput1
logical :: override_restart_timers
character*256 :: auxhist1_inname
character*256 :: auxhist1_outname
integer , DIMENSION(max_domains) :: auxhist1_interval_y
integer , DIMENSION(max_domains) :: auxhist1_interval_d
integer , DIMENSION(max_domains) :: auxhist1_interval_h
integer , DIMENSION(max_domains) :: auxhist1_interval_m
integer , DIMENSION(max_domains) :: auxhist1_interval_s
integer , DIMENSION(max_domains) :: auxhist1_interval
integer , DIMENSION(max_domains) :: auxhist1_begin_y
integer , DIMENSION(max_domains) :: auxhist1_begin_d
integer , DIMENSION(max_domains) :: auxhist1_begin_h
integer , DIMENSION(max_domains) :: auxhist1_begin_m
integer , DIMENSION(max_domains) :: auxhist1_begin_s
integer , DIMENSION(max_domains) :: auxhist1_begin
integer , DIMENSION(max_domains) :: auxhist1_end_y
integer , DIMENSION(max_domains) :: auxhist1_end_d
integer , DIMENSION(max_domains) :: auxhist1_end_h
integer , DIMENSION(max_domains) :: auxhist1_end_m
integer , DIMENSION(max_domains) :: auxhist1_end_s
integer , DIMENSION(max_domains) :: auxhist1_end
integer :: io_form_auxhist1
integer , DIMENSION(max_domains) :: frames_per_auxhist1
character*256 :: auxhist2_inname
character*256 :: auxhist2_outname
integer , DIMENSION(max_domains) :: auxhist2_interval_y
integer , DIMENSION(max_domains) :: auxhist2_interval_d
integer , DIMENSION(max_domains) :: auxhist2_interval_h
integer , DIMENSION(max_domains) :: auxhist2_interval_m
integer , DIMENSION(max_domains) :: auxhist2_interval_s
integer , DIMENSION(max_domains) :: auxhist2_interval
integer , DIMENSION(max_domains) :: auxhist2_begin_y
integer , DIMENSION(max_domains) :: auxhist2_begin_d
integer , DIMENSION(max_domains) :: auxhist2_begin_h
integer , DIMENSION(max_domains) :: auxhist2_begin_m
integer , DIMENSION(max_domains) :: auxhist2_begin_s
integer , DIMENSION(max_domains) :: auxhist2_begin
integer , DIMENSION(max_domains) :: auxhist2_end_y
integer , DIMENSION(max_domains) :: auxhist2_end_d
integer , DIMENSION(max_domains) :: auxhist2_end_h
integer , DIMENSION(max_domains) :: auxhist2_end_m
integer , DIMENSION(max_domains) :: auxhist2_end_s
integer , DIMENSION(max_domains) :: auxhist2_end
integer :: io_form_auxhist2
integer , DIMENSION(max_domains) :: frames_per_auxhist2
character*256 :: auxhist3_inname
character*256 :: auxhist3_outname
integer , DIMENSION(max_domains) :: auxhist3_interval_y
integer , DIMENSION(max_domains) :: auxhist3_interval_d
integer , DIMENSION(max_domains) :: auxhist3_interval_h
integer , DIMENSION(max_domains) :: auxhist3_interval_m
integer , DIMENSION(max_domains) :: auxhist3_interval_s
integer , DIMENSION(max_domains) :: auxhist3_interval
integer , DIMENSION(max_domains) :: auxhist3_begin_y
integer , DIMENSION(max_domains) :: auxhist3_begin_d
integer , DIMENSION(max_domains) :: auxhist3_begin_h
integer , DIMENSION(max_domains) :: auxhist3_begin_m
integer , DIMENSION(max_domains) :: auxhist3_begin_s
integer , DIMENSION(max_domains) :: auxhist3_begin
integer , DIMENSION(max_domains) :: auxhist3_end_y
integer , DIMENSION(max_domains) :: auxhist3_end_d
integer , DIMENSION(max_domains) :: auxhist3_end_h
integer , DIMENSION(max_domains) :: auxhist3_end_m
integer , DIMENSION(max_domains) :: auxhist3_end_s
integer , DIMENSION(max_domains) :: auxhist3_end
integer :: io_form_auxhist3
integer , DIMENSION(max_domains) :: frames_per_auxhist3
character*256 :: auxhist4_inname
character*256 :: auxhist4_outname
integer , DIMENSION(max_domains) :: auxhist4_interval_y
integer , DIMENSION(max_domains) :: auxhist4_interval_d
integer , DIMENSION(max_domains) :: auxhist4_interval_h
integer , DIMENSION(max_domains) :: auxhist4_interval_m
integer , DIMENSION(max_domains) :: auxhist4_interval_s
integer , DIMENSION(max_domains) :: auxhist4_interval
integer , DIMENSION(max_domains) :: auxhist4_begin_y
integer , DIMENSION(max_domains) :: auxhist4_begin_d
integer , DIMENSION(max_domains) :: auxhist4_begin_h
integer , DIMENSION(max_domains) :: auxhist4_begin_m
integer , DIMENSION(max_domains) :: auxhist4_begin_s
integer , DIMENSION(max_domains) :: auxhist4_begin
integer , DIMENSION(max_domains) :: auxhist4_end_y
integer , DIMENSION(max_domains) :: auxhist4_end_d
integer , DIMENSION(max_domains) :: auxhist4_end_h
integer , DIMENSION(max_domains) :: auxhist4_end_m
integer , DIMENSION(max_domains) :: auxhist4_end_s
integer , DIMENSION(max_domains) :: auxhist4_end
integer :: io_form_auxhist4
integer , DIMENSION(max_domains) :: frames_per_auxhist4
character*256 :: auxhist5_inname
character*256 :: auxhist5_outname
integer , DIMENSION(max_domains) :: auxhist5_interval_y
integer , DIMENSION(max_domains) :: auxhist5_interval_d
integer , DIMENSION(max_domains) :: auxhist5_interval_h
integer , DIMENSION(max_domains) :: auxhist5_interval_m
integer , DIMENSION(max_domains) :: auxhist5_interval_s
integer , DIMENSION(max_domains) :: auxhist5_interval
integer , DIMENSION(max_domains) :: auxhist5_begin_y
integer , DIMENSION(max_domains) :: auxhist5_begin_d
integer , DIMENSION(max_domains) :: auxhist5_begin_h
integer , DIMENSION(max_domains) :: auxhist5_begin_m
integer , DIMENSION(max_domains) :: auxhist5_begin_s
integer , DIMENSION(max_domains) :: auxhist5_begin
integer , DIMENSION(max_domains) :: auxhist5_end_y
integer , DIMENSION(max_domains) :: auxhist5_end_d
integer , DIMENSION(max_domains) :: auxhist5_end_h
integer , DIMENSION(max_domains) :: auxhist5_end_m
integer , DIMENSION(max_domains) :: auxhist5_end_s
integer , DIMENSION(max_domains) :: auxhist5_end
integer :: io_form_auxhist5
integer , DIMENSION(max_domains) :: frames_per_auxhist5
character*256 :: auxhist6_inname
character*256 :: auxhist6_outname
integer , DIMENSION(max_domains) :: auxhist6_interval_y
integer , DIMENSION(max_domains) :: auxhist6_interval_d
integer , DIMENSION(max_domains) :: auxhist6_interval_h
integer , DIMENSION(max_domains) :: auxhist6_interval_m
integer , DIMENSION(max_domains) :: auxhist6_interval_s
integer , DIMENSION(max_domains) :: auxhist6_interval
integer , DIMENSION(max_domains) :: auxhist6_begin_y
integer , DIMENSION(max_domains) :: auxhist6_begin_d
integer , DIMENSION(max_domains) :: auxhist6_begin_h
integer , DIMENSION(max_domains) :: auxhist6_begin_m
integer , DIMENSION(max_domains) :: auxhist6_begin_s
integer , DIMENSION(max_domains) :: auxhist6_begin
integer , DIMENSION(max_domains) :: auxhist6_end_y
integer , DIMENSION(max_domains) :: auxhist6_end_d
integer , DIMENSION(max_domains) :: auxhist6_end_h
integer , DIMENSION(max_domains) :: auxhist6_end_m
integer , DIMENSION(max_domains) :: auxhist6_end_s
integer , DIMENSION(max_domains) :: auxhist6_end
integer :: io_form_auxhist6
integer , DIMENSION(max_domains) :: frames_per_auxhist6
character*256 :: auxhist7_inname
character*256 :: auxhist7_outname
integer , DIMENSION(max_domains) :: auxhist7_interval_y
integer , DIMENSION(max_domains) :: auxhist7_interval_d
integer , DIMENSION(max_domains) :: auxhist7_interval_h
integer , DIMENSION(max_domains) :: auxhist7_interval_m
integer , DIMENSION(max_domains) :: auxhist7_interval_s
integer , DIMENSION(max_domains) :: auxhist7_interval
integer , DIMENSION(max_domains) :: auxhist7_begin_y
integer , DIMENSION(max_domains) :: auxhist7_begin_d
integer , DIMENSION(max_domains) :: auxhist7_begin_h
integer , DIMENSION(max_domains) :: auxhist7_begin_m
integer , DIMENSION(max_domains) :: auxhist7_begin_s
integer , DIMENSION(max_domains) :: auxhist7_begin
integer , DIMENSION(max_domains) :: auxhist7_end_y
integer , DIMENSION(max_domains) :: auxhist7_end_d
integer , DIMENSION(max_domains) :: auxhist7_end_h
integer , DIMENSION(max_domains) :: auxhist7_end_m
integer , DIMENSION(max_domains) :: auxhist7_end_s
integer , DIMENSION(max_domains) :: auxhist7_end
integer :: io_form_auxhist7
integer , DIMENSION(max_domains) :: frames_per_auxhist7
character*256 :: auxhist8_inname
character*256 :: auxhist8_outname
integer , DIMENSION(max_domains) :: auxhist8_interval_y
integer , DIMENSION(max_domains) :: auxhist8_interval_d
integer , DIMENSION(max_domains) :: auxhist8_interval_h
integer , DIMENSION(max_domains) :: auxhist8_interval_m
integer , DIMENSION(max_domains) :: auxhist8_interval_s
integer , DIMENSION(max_domains) :: auxhist8_interval
integer , DIMENSION(max_domains) :: auxhist8_begin_y
integer , DIMENSION(max_domains) :: auxhist8_begin_d
integer , DIMENSION(max_domains) :: auxhist8_begin_h
integer , DIMENSION(max_domains) :: auxhist8_begin_m
integer , DIMENSION(max_domains) :: auxhist8_begin_s
integer , DIMENSION(max_domains) :: auxhist8_begin
integer , DIMENSION(max_domains) :: auxhist8_end_y
integer , DIMENSION(max_domains) :: auxhist8_end_d
integer , DIMENSION(max_domains) :: auxhist8_end_h
integer , DIMENSION(max_domains) :: auxhist8_end_m
integer , DIMENSION(max_domains) :: auxhist8_end_s
integer , DIMENSION(max_domains) :: auxhist8_end
integer :: io_form_auxhist8
integer , DIMENSION(max_domains) :: frames_per_auxhist8
character*256 :: auxhist9_inname
character*256 :: auxhist9_outname
integer , DIMENSION(max_domains) :: auxhist9_interval_y
integer , DIMENSION(max_domains) :: auxhist9_interval_d
integer , DIMENSION(max_domains) :: auxhist9_interval_h
integer , DIMENSION(max_domains) :: auxhist9_interval_m
integer , DIMENSION(max_domains) :: auxhist9_interval_s
integer , DIMENSION(max_domains) :: auxhist9_interval
integer , DIMENSION(max_domains) :: auxhist9_begin_y
integer , DIMENSION(max_domains) :: auxhist9_begin_d
integer , DIMENSION(max_domains) :: auxhist9_begin_h
integer , DIMENSION(max_domains) :: auxhist9_begin_m
integer , DIMENSION(max_domains) :: auxhist9_begin_s
integer , DIMENSION(max_domains) :: auxhist9_begin
integer , DIMENSION(max_domains) :: auxhist9_end_y
integer , DIMENSION(max_domains) :: auxhist9_end_d
integer , DIMENSION(max_domains) :: auxhist9_end_h
integer , DIMENSION(max_domains) :: auxhist9_end_m
integer , DIMENSION(max_domains) :: auxhist9_end_s
integer , DIMENSION(max_domains) :: auxhist9_end
integer :: io_form_auxhist9
integer , DIMENSION(max_domains) :: frames_per_auxhist9
character*256 :: auxhist10_inname
character*256 :: auxhist10_outname
integer , DIMENSION(max_domains) :: auxhist10_interval_y
integer , DIMENSION(max_domains) :: auxhist10_interval_d
integer , DIMENSION(max_domains) :: auxhist10_interval_h
integer , DIMENSION(max_domains) :: auxhist10_interval_m
integer , DIMENSION(max_domains) :: auxhist10_interval_s
integer , DIMENSION(max_domains) :: auxhist10_interval
integer , DIMENSION(max_domains) :: auxhist10_begin_y
integer , DIMENSION(max_domains) :: auxhist10_begin_d
integer , DIMENSION(max_domains) :: auxhist10_begin_h
integer , DIMENSION(max_domains) :: auxhist10_begin_m
integer , DIMENSION(max_domains) :: auxhist10_begin_s
integer , DIMENSION(max_domains) :: auxhist10_begin
integer , DIMENSION(max_domains) :: auxhist10_end_y
integer , DIMENSION(max_domains) :: auxhist10_end_d
integer , DIMENSION(max_domains) :: auxhist10_end_h
integer , DIMENSION(max_domains) :: auxhist10_end_m
integer , DIMENSION(max_domains) :: auxhist10_end_s
integer , DIMENSION(max_domains) :: auxhist10_end
integer :: io_form_auxhist10
integer , DIMENSION(max_domains) :: frames_per_auxhist10
character*256 :: auxhist11_inname
character*256 :: auxhist11_outname
integer , DIMENSION(max_domains) :: auxhist11_interval_y
integer , DIMENSION(max_domains) :: auxhist11_interval_d
integer , DIMENSION(max_domains) :: auxhist11_interval_h
integer , DIMENSION(max_domains) :: auxhist11_interval_m
integer , DIMENSION(max_domains) :: auxhist11_interval_s
integer , DIMENSION(max_domains) :: auxhist11_interval
integer , DIMENSION(max_domains) :: auxhist11_begin_y
integer , DIMENSION(max_domains) :: auxhist11_begin_d
integer , DIMENSION(max_domains) :: auxhist11_begin_h
integer , DIMENSION(max_domains) :: auxhist11_begin_m
integer , DIMENSION(max_domains) :: auxhist11_begin_s
integer , DIMENSION(max_domains) :: auxhist11_begin
integer , DIMENSION(max_domains) :: auxhist11_end_y
integer , DIMENSION(max_domains) :: auxhist11_end_d
integer , DIMENSION(max_domains) :: auxhist11_end_h
integer , DIMENSION(max_domains) :: auxhist11_end_m
integer , DIMENSION(max_domains) :: auxhist11_end_s
integer , DIMENSION(max_domains) :: auxhist11_end
integer :: io_form_auxhist11
integer , DIMENSION(max_domains) :: frames_per_auxhist11
character*256 :: auxhist12_inname
character*256 :: auxhist12_outname
integer , DIMENSION(max_domains) :: auxhist12_interval_y
integer , DIMENSION(max_domains) :: auxhist12_interval_d
integer , DIMENSION(max_domains) :: auxhist12_interval_h
integer , DIMENSION(max_domains) :: auxhist12_interval_m
integer , DIMENSION(max_domains) :: auxhist12_interval_s
integer , DIMENSION(max_domains) :: auxhist12_interval
integer , DIMENSION(max_domains) :: auxhist12_begin_y
integer , DIMENSION(max_domains) :: auxhist12_begin_d
integer , DIMENSION(max_domains) :: auxhist12_begin_h
integer , DIMENSION(max_domains) :: auxhist12_begin_m
integer , DIMENSION(max_domains) :: auxhist12_begin_s
integer , DIMENSION(max_domains) :: auxhist12_begin
integer , DIMENSION(max_domains) :: auxhist12_end_y
integer , DIMENSION(max_domains) :: auxhist12_end_d
integer , DIMENSION(max_domains) :: auxhist12_end_h
integer , DIMENSION(max_domains) :: auxhist12_end_m
integer , DIMENSION(max_domains) :: auxhist12_end_s
integer , DIMENSION(max_domains) :: auxhist12_end
integer :: io_form_auxhist12
integer , DIMENSION(max_domains) :: frames_per_auxhist12
character*256 :: auxhist13_inname
character*256 :: auxhist13_outname
integer , DIMENSION(max_domains) :: auxhist13_interval_y
integer , DIMENSION(max_domains) :: auxhist13_interval_d
integer , DIMENSION(max_domains) :: auxhist13_interval_h
integer , DIMENSION(max_domains) :: auxhist13_interval_m
integer , DIMENSION(max_domains) :: auxhist13_interval_s
integer , DIMENSION(max_domains) :: auxhist13_interval
integer , DIMENSION(max_domains) :: auxhist13_begin_y
integer , DIMENSION(max_domains) :: auxhist13_begin_d
integer , DIMENSION(max_domains) :: auxhist13_begin_h
integer , DIMENSION(max_domains) :: auxhist13_begin_m
integer , DIMENSION(max_domains) :: auxhist13_begin_s
integer , DIMENSION(max_domains) :: auxhist13_begin
integer , DIMENSION(max_domains) :: auxhist13_end_y
integer , DIMENSION(max_domains) :: auxhist13_end_d
integer , DIMENSION(max_domains) :: auxhist13_end_h
integer , DIMENSION(max_domains) :: auxhist13_end_m
integer , DIMENSION(max_domains) :: auxhist13_end_s
integer , DIMENSION(max_domains) :: auxhist13_end
integer :: io_form_auxhist13
integer , DIMENSION(max_domains) :: frames_per_auxhist13
character*256 :: auxhist14_inname
character*256 :: auxhist14_outname
integer , DIMENSION(max_domains) :: auxhist14_interval_y
integer , DIMENSION(max_domains) :: auxhist14_interval_d
integer , DIMENSION(max_domains) :: auxhist14_interval_h
integer , DIMENSION(max_domains) :: auxhist14_interval_m
integer , DIMENSION(max_domains) :: auxhist14_interval_s
integer , DIMENSION(max_domains) :: auxhist14_interval
integer , DIMENSION(max_domains) :: auxhist14_begin_y
integer , DIMENSION(max_domains) :: auxhist14_begin_d
integer , DIMENSION(max_domains) :: auxhist14_begin_h
integer , DIMENSION(max_domains) :: auxhist14_begin_m
integer , DIMENSION(max_domains) :: auxhist14_begin_s
integer , DIMENSION(max_domains) :: auxhist14_begin
integer , DIMENSION(max_domains) :: auxhist14_end_y
integer , DIMENSION(max_domains) :: auxhist14_end_d
integer , DIMENSION(max_domains) :: auxhist14_end_h
integer , DIMENSION(max_domains) :: auxhist14_end_m
integer , DIMENSION(max_domains) :: auxhist14_end_s
integer , DIMENSION(max_domains) :: auxhist14_end
integer :: io_form_auxhist14
integer , DIMENSION(max_domains) :: frames_per_auxhist14
character*256 :: auxhist15_inname
character*256 :: auxhist15_outname
integer , DIMENSION(max_domains) :: auxhist15_interval_y
integer , DIMENSION(max_domains) :: auxhist15_interval_d
integer , DIMENSION(max_domains) :: auxhist15_interval_h
integer , DIMENSION(max_domains) :: auxhist15_interval_m
integer , DIMENSION(max_domains) :: auxhist15_interval_s
integer , DIMENSION(max_domains) :: auxhist15_interval
integer , DIMENSION(max_domains) :: auxhist15_begin_y
integer , DIMENSION(max_domains) :: auxhist15_begin_d
integer , DIMENSION(max_domains) :: auxhist15_begin_h
integer , DIMENSION(max_domains) :: auxhist15_begin_m
integer , DIMENSION(max_domains) :: auxhist15_begin_s
integer , DIMENSION(max_domains) :: auxhist15_begin
integer , DIMENSION(max_domains) :: auxhist15_end_y
integer , DIMENSION(max_domains) :: auxhist15_end_d
integer , DIMENSION(max_domains) :: auxhist15_end_h
integer , DIMENSION(max_domains) :: auxhist15_end_m
integer , DIMENSION(max_domains) :: auxhist15_end_s
integer , DIMENSION(max_domains) :: auxhist15_end
integer :: io_form_auxhist15
integer , DIMENSION(max_domains) :: frames_per_auxhist15
character*256 :: auxhist16_inname
character*256 :: auxhist16_outname
integer , DIMENSION(max_domains) :: auxhist16_interval_y
integer , DIMENSION(max_domains) :: auxhist16_interval_d
integer , DIMENSION(max_domains) :: auxhist16_interval_h
integer , DIMENSION(max_domains) :: auxhist16_interval_m
integer , DIMENSION(max_domains) :: auxhist16_interval_s
integer , DIMENSION(max_domains) :: auxhist16_interval
integer , DIMENSION(max_domains) :: auxhist16_begin_y
integer , DIMENSION(max_domains) :: auxhist16_begin_d
integer , DIMENSION(max_domains) :: auxhist16_begin_h
integer , DIMENSION(max_domains) :: auxhist16_begin_m
integer , DIMENSION(max_domains) :: auxhist16_begin_s
integer , DIMENSION(max_domains) :: auxhist16_begin
integer , DIMENSION(max_domains) :: auxhist16_end_y
integer , DIMENSION(max_domains) :: auxhist16_end_d
integer , DIMENSION(max_domains) :: auxhist16_end_h
integer , DIMENSION(max_domains) :: auxhist16_end_m
integer , DIMENSION(max_domains) :: auxhist16_end_s
integer , DIMENSION(max_domains) :: auxhist16_end
integer :: io_form_auxhist16
integer , DIMENSION(max_domains) :: frames_per_auxhist16
character*256 :: auxhist17_inname
character*256 :: auxhist17_outname
integer , DIMENSION(max_domains) :: auxhist17_interval_y
integer , DIMENSION(max_domains) :: auxhist17_interval_d
integer , DIMENSION(max_domains) :: auxhist17_interval_h
integer , DIMENSION(max_domains) :: auxhist17_interval_m
integer , DIMENSION(max_domains) :: auxhist17_interval_s
integer , DIMENSION(max_domains) :: auxhist17_interval
integer , DIMENSION(max_domains) :: auxhist17_begin_y
integer , DIMENSION(max_domains) :: auxhist17_begin_d
integer , DIMENSION(max_domains) :: auxhist17_begin_h
integer , DIMENSION(max_domains) :: auxhist17_begin_m
integer , DIMENSION(max_domains) :: auxhist17_begin_s
integer , DIMENSION(max_domains) :: auxhist17_begin
integer , DIMENSION(max_domains) :: auxhist17_end_y
integer , DIMENSION(max_domains) :: auxhist17_end_d
integer , DIMENSION(max_domains) :: auxhist17_end_h
integer , DIMENSION(max_domains) :: auxhist17_end_m
integer , DIMENSION(max_domains) :: auxhist17_end_s
integer , DIMENSION(max_domains) :: auxhist17_end
integer :: io_form_auxhist17
integer , DIMENSION(max_domains) :: frames_per_auxhist17
character*256 :: auxhist18_inname
character*256 :: auxhist18_outname
integer , DIMENSION(max_domains) :: auxhist18_interval_y
integer , DIMENSION(max_domains) :: auxhist18_interval_d
integer , DIMENSION(max_domains) :: auxhist18_interval_h
integer , DIMENSION(max_domains) :: auxhist18_interval_m
integer , DIMENSION(max_domains) :: auxhist18_interval_s
integer , DIMENSION(max_domains) :: auxhist18_interval
integer , DIMENSION(max_domains) :: auxhist18_begin_y
integer , DIMENSION(max_domains) :: auxhist18_begin_d
integer , DIMENSION(max_domains) :: auxhist18_begin_h
integer , DIMENSION(max_domains) :: auxhist18_begin_m
integer , DIMENSION(max_domains) :: auxhist18_begin_s
integer , DIMENSION(max_domains) :: auxhist18_begin
integer , DIMENSION(max_domains) :: auxhist18_end_y
integer , DIMENSION(max_domains) :: auxhist18_end_d
integer , DIMENSION(max_domains) :: auxhist18_end_h
integer , DIMENSION(max_domains) :: auxhist18_end_m
integer , DIMENSION(max_domains) :: auxhist18_end_s
integer , DIMENSION(max_domains) :: auxhist18_end
integer :: io_form_auxhist18
integer , DIMENSION(max_domains) :: frames_per_auxhist18
character*256 :: auxhist19_inname
character*256 :: auxhist19_outname
integer , DIMENSION(max_domains) :: auxhist19_interval_y
integer , DIMENSION(max_domains) :: auxhist19_interval_d
integer , DIMENSION(max_domains) :: auxhist19_interval_h
integer , DIMENSION(max_domains) :: auxhist19_interval_m
integer , DIMENSION(max_domains) :: auxhist19_interval_s
integer , DIMENSION(max_domains) :: auxhist19_interval
integer , DIMENSION(max_domains) :: auxhist19_begin_y
integer , DIMENSION(max_domains) :: auxhist19_begin_d
integer , DIMENSION(max_domains) :: auxhist19_begin_h
integer , DIMENSION(max_domains) :: auxhist19_begin_m
integer , DIMENSION(max_domains) :: auxhist19_begin_s
integer , DIMENSION(max_domains) :: auxhist19_begin
integer , DIMENSION(max_domains) :: auxhist19_end_y
integer , DIMENSION(max_domains) :: auxhist19_end_d
integer , DIMENSION(max_domains) :: auxhist19_end_h
integer , DIMENSION(max_domains) :: auxhist19_end_m
integer , DIMENSION(max_domains) :: auxhist19_end_s
integer , DIMENSION(max_domains) :: auxhist19_end
integer :: io_form_auxhist19
integer , DIMENSION(max_domains) :: frames_per_auxhist19
character*256 :: auxhist20_inname
character*256 :: auxhist20_outname
integer , DIMENSION(max_domains) :: auxhist20_interval_y
integer , DIMENSION(max_domains) :: auxhist20_interval_d
integer , DIMENSION(max_domains) :: auxhist20_interval_h
integer , DIMENSION(max_domains) :: auxhist20_interval_m
integer , DIMENSION(max_domains) :: auxhist20_interval_s
integer , DIMENSION(max_domains) :: auxhist20_interval
integer , DIMENSION(max_domains) :: auxhist20_begin_y
integer , DIMENSION(max_domains) :: auxhist20_begin_d
integer , DIMENSION(max_domains) :: auxhist20_begin_h
integer , DIMENSION(max_domains) :: auxhist20_begin_m
integer , DIMENSION(max_domains) :: auxhist20_begin_s
integer , DIMENSION(max_domains) :: auxhist20_begin
integer , DIMENSION(max_domains) :: auxhist20_end_y
integer , DIMENSION(max_domains) :: auxhist20_end_d
integer , DIMENSION(max_domains) :: auxhist20_end_h
integer , DIMENSION(max_domains) :: auxhist20_end_m
integer , DIMENSION(max_domains) :: auxhist20_end_s
integer , DIMENSION(max_domains) :: auxhist20_end
integer :: io_form_auxhist20
integer , DIMENSION(max_domains) :: frames_per_auxhist20
character*256 :: auxhist21_inname
character*256 :: auxhist21_outname
integer , DIMENSION(max_domains) :: auxhist21_interval_y
integer , DIMENSION(max_domains) :: auxhist21_interval_d
integer , DIMENSION(max_domains) :: auxhist21_interval_h
integer , DIMENSION(max_domains) :: auxhist21_interval_m
integer , DIMENSION(max_domains) :: auxhist21_interval_s
integer , DIMENSION(max_domains) :: auxhist21_interval
integer , DIMENSION(max_domains) :: auxhist21_begin_y
integer , DIMENSION(max_domains) :: auxhist21_begin_d
integer , DIMENSION(max_domains) :: auxhist21_begin_h
integer , DIMENSION(max_domains) :: auxhist21_begin_m
integer , DIMENSION(max_domains) :: auxhist21_begin_s
integer , DIMENSION(max_domains) :: auxhist21_begin
integer , DIMENSION(max_domains) :: auxhist21_end_y
integer , DIMENSION(max_domains) :: auxhist21_end_d
integer , DIMENSION(max_domains) :: auxhist21_end_h
integer , DIMENSION(max_domains) :: auxhist21_end_m
integer , DIMENSION(max_domains) :: auxhist21_end_s
integer , DIMENSION(max_domains) :: auxhist21_end
integer :: io_form_auxhist21
integer , DIMENSION(max_domains) :: frames_per_auxhist21
character*256 :: auxhist22_inname
character*256 :: auxhist22_outname
integer , DIMENSION(max_domains) :: auxhist22_interval_y
integer , DIMENSION(max_domains) :: auxhist22_interval_d
integer , DIMENSION(max_domains) :: auxhist22_interval_h
integer , DIMENSION(max_domains) :: auxhist22_interval_m
integer , DIMENSION(max_domains) :: auxhist22_interval_s
integer , DIMENSION(max_domains) :: auxhist22_interval
integer , DIMENSION(max_domains) :: auxhist22_begin_y
integer , DIMENSION(max_domains) :: auxhist22_begin_d
integer , DIMENSION(max_domains) :: auxhist22_begin_h
integer , DIMENSION(max_domains) :: auxhist22_begin_m
integer , DIMENSION(max_domains) :: auxhist22_begin_s
integer , DIMENSION(max_domains) :: auxhist22_begin
integer , DIMENSION(max_domains) :: auxhist22_end_y
integer , DIMENSION(max_domains) :: auxhist22_end_d
integer , DIMENSION(max_domains) :: auxhist22_end_h
integer , DIMENSION(max_domains) :: auxhist22_end_m
integer , DIMENSION(max_domains) :: auxhist22_end_s
integer , DIMENSION(max_domains) :: auxhist22_end
integer :: io_form_auxhist22
integer , DIMENSION(max_domains) :: frames_per_auxhist22
character*256 :: auxhist23_inname
character*256 :: auxhist23_outname
integer , DIMENSION(max_domains) :: auxhist23_interval_y
integer , DIMENSION(max_domains) :: auxhist23_interval_d
integer , DIMENSION(max_domains) :: auxhist23_interval_h
integer , DIMENSION(max_domains) :: auxhist23_interval_m
integer , DIMENSION(max_domains) :: auxhist23_interval_s
integer , DIMENSION(max_domains) :: auxhist23_interval
integer , DIMENSION(max_domains) :: auxhist23_begin_y
integer , DIMENSION(max_domains) :: auxhist23_begin_d
integer , DIMENSION(max_domains) :: auxhist23_begin_h
integer , DIMENSION(max_domains) :: auxhist23_begin_m
integer , DIMENSION(max_domains) :: auxhist23_begin_s
integer , DIMENSION(max_domains) :: auxhist23_begin
integer , DIMENSION(max_domains) :: auxhist23_end_y
integer , DIMENSION(max_domains) :: auxhist23_end_d
integer , DIMENSION(max_domains) :: auxhist23_end_h
integer , DIMENSION(max_domains) :: auxhist23_end_m
integer , DIMENSION(max_domains) :: auxhist23_end_s
integer , DIMENSION(max_domains) :: auxhist23_end
integer :: io_form_auxhist23
integer , DIMENSION(max_domains) :: frames_per_auxhist23
character*256 :: auxhist24_inname
character*256 :: auxhist24_outname
integer , DIMENSION(max_domains) :: auxhist24_interval_y
integer , DIMENSION(max_domains) :: auxhist24_interval_d
integer , DIMENSION(max_domains) :: auxhist24_interval_h
integer , DIMENSION(max_domains) :: auxhist24_interval_m
integer , DIMENSION(max_domains) :: auxhist24_interval_s
integer , DIMENSION(max_domains) :: auxhist24_interval
integer , DIMENSION(max_domains) :: auxhist24_begin_y
integer , DIMENSION(max_domains) :: auxhist24_begin_d
integer , DIMENSION(max_domains) :: auxhist24_begin_h
integer , DIMENSION(max_domains) :: auxhist24_begin_m
integer , DIMENSION(max_domains) :: auxhist24_begin_s
integer , DIMENSION(max_domains) :: auxhist24_begin
integer , DIMENSION(max_domains) :: auxhist24_end_y
integer , DIMENSION(max_domains) :: auxhist24_end_d
integer , DIMENSION(max_domains) :: auxhist24_end_h
integer , DIMENSION(max_domains) :: auxhist24_end_m
integer , DIMENSION(max_domains) :: auxhist24_end_s
integer , DIMENSION(max_domains) :: auxhist24_end
integer :: io_form_auxhist24
integer , DIMENSION(max_domains) :: frames_per_auxhist24
character*256 :: auxinput1_outname
integer , DIMENSION(max_domains) :: auxinput1_interval_y
integer , DIMENSION(max_domains) :: auxinput1_interval_d
integer , DIMENSION(max_domains) :: auxinput1_interval_h
integer , DIMENSION(max_domains) :: auxinput1_interval_m
integer , DIMENSION(max_domains) :: auxinput1_interval_s
integer , DIMENSION(max_domains) :: auxinput1_interval
integer , DIMENSION(max_domains) :: auxinput1_begin_y
integer , DIMENSION(max_domains) :: auxinput1_begin_d
integer , DIMENSION(max_domains) :: auxinput1_begin_h
integer , DIMENSION(max_domains) :: auxinput1_begin_m
integer , DIMENSION(max_domains) :: auxinput1_begin_s
integer , DIMENSION(max_domains) :: auxinput1_begin
integer , DIMENSION(max_domains) :: auxinput1_end_y
integer , DIMENSION(max_domains) :: auxinput1_end_d
integer , DIMENSION(max_domains) :: auxinput1_end_h
integer , DIMENSION(max_domains) :: auxinput1_end_m
integer , DIMENSION(max_domains) :: auxinput1_end_s
integer , DIMENSION(max_domains) :: auxinput1_end
integer , DIMENSION(max_domains) :: frames_per_auxinput1
character*256 :: auxinput2_inname
character*256 :: auxinput2_outname
integer , DIMENSION(max_domains) :: auxinput2_interval_y
integer , DIMENSION(max_domains) :: auxinput2_interval_d
integer , DIMENSION(max_domains) :: auxinput2_interval_h
integer , DIMENSION(max_domains) :: auxinput2_interval_m
integer , DIMENSION(max_domains) :: auxinput2_interval_s
integer , DIMENSION(max_domains) :: auxinput2_interval
integer , DIMENSION(max_domains) :: auxinput2_begin_y
integer , DIMENSION(max_domains) :: auxinput2_begin_d
integer , DIMENSION(max_domains) :: auxinput2_begin_h
integer , DIMENSION(max_domains) :: auxinput2_begin_m
integer , DIMENSION(max_domains) :: auxinput2_begin_s
integer , DIMENSION(max_domains) :: auxinput2_begin
integer , DIMENSION(max_domains) :: auxinput2_end_y
integer , DIMENSION(max_domains) :: auxinput2_end_d
integer , DIMENSION(max_domains) :: auxinput2_end_h
integer , DIMENSION(max_domains) :: auxinput2_end_m
integer , DIMENSION(max_domains) :: auxinput2_end_s
integer , DIMENSION(max_domains) :: auxinput2_end
integer , DIMENSION(max_domains) :: frames_per_auxinput2
character*256 :: auxinput3_inname
character*256 :: auxinput3_outname
integer , DIMENSION(max_domains) :: auxinput3_interval_y
integer , DIMENSION(max_domains) :: auxinput3_interval_d
integer , DIMENSION(max_domains) :: auxinput3_interval_h
integer , DIMENSION(max_domains) :: auxinput3_interval_m
integer , DIMENSION(max_domains) :: auxinput3_interval_s
integer , DIMENSION(max_domains) :: auxinput3_interval
integer , DIMENSION(max_domains) :: auxinput3_begin_y
integer , DIMENSION(max_domains) :: auxinput3_begin_d
integer , DIMENSION(max_domains) :: auxinput3_begin_h
integer , DIMENSION(max_domains) :: auxinput3_begin_m
integer , DIMENSION(max_domains) :: auxinput3_begin_s
integer , DIMENSION(max_domains) :: auxinput3_begin
integer , DIMENSION(max_domains) :: auxinput3_end_y
integer , DIMENSION(max_domains) :: auxinput3_end_d
integer , DIMENSION(max_domains) :: auxinput3_end_h
integer , DIMENSION(max_domains) :: auxinput3_end_m
integer , DIMENSION(max_domains) :: auxinput3_end_s
integer , DIMENSION(max_domains) :: auxinput3_end
integer :: io_form_auxinput3
integer , DIMENSION(max_domains) :: frames_per_auxinput3
character*256 :: auxinput4_inname
character*256 :: auxinput4_outname
integer , DIMENSION(max_domains) :: auxinput4_interval_y
integer , DIMENSION(max_domains) :: auxinput4_interval_d
integer , DIMENSION(max_domains) :: auxinput4_interval_h
integer , DIMENSION(max_domains) :: auxinput4_interval_m
integer , DIMENSION(max_domains) :: auxinput4_interval_s
integer , DIMENSION(max_domains) :: auxinput4_interval
integer , DIMENSION(max_domains) :: auxinput4_begin_y
integer , DIMENSION(max_domains) :: auxinput4_begin_d
integer , DIMENSION(max_domains) :: auxinput4_begin_h
integer , DIMENSION(max_domains) :: auxinput4_begin_m
integer , DIMENSION(max_domains) :: auxinput4_begin_s
integer , DIMENSION(max_domains) :: auxinput4_begin
integer , DIMENSION(max_domains) :: auxinput4_end_y
integer , DIMENSION(max_domains) :: auxinput4_end_d
integer , DIMENSION(max_domains) :: auxinput4_end_h
integer , DIMENSION(max_domains) :: auxinput4_end_m
integer , DIMENSION(max_domains) :: auxinput4_end_s
integer , DIMENSION(max_domains) :: auxinput4_end
integer :: io_form_auxinput4
integer , DIMENSION(max_domains) :: frames_per_auxinput4
character*256 :: auxinput5_inname
character*256 :: auxinput5_outname
integer , DIMENSION(max_domains) :: auxinput5_interval_y
integer , DIMENSION(max_domains) :: auxinput5_interval_d
integer , DIMENSION(max_domains) :: auxinput5_interval_h
integer , DIMENSION(max_domains) :: auxinput5_interval_m
integer , DIMENSION(max_domains) :: auxinput5_interval_s
integer , DIMENSION(max_domains) :: auxinput5_interval
integer , DIMENSION(max_domains) :: auxinput5_begin_y
integer , DIMENSION(max_domains) :: auxinput5_begin_d
integer , DIMENSION(max_domains) :: auxinput5_begin_h
integer , DIMENSION(max_domains) :: auxinput5_begin_m
integer , DIMENSION(max_domains) :: auxinput5_begin_s
integer , DIMENSION(max_domains) :: auxinput5_begin
integer , DIMENSION(max_domains) :: auxinput5_end_y
integer , DIMENSION(max_domains) :: auxinput5_end_d
integer , DIMENSION(max_domains) :: auxinput5_end_h
integer , DIMENSION(max_domains) :: auxinput5_end_m
integer , DIMENSION(max_domains) :: auxinput5_end_s
integer , DIMENSION(max_domains) :: auxinput5_end
integer :: io_form_auxinput5
integer , DIMENSION(max_domains) :: frames_per_auxinput5
character*256 :: auxinput6_inname
character*256 :: auxinput6_outname
integer , DIMENSION(max_domains) :: auxinput6_interval_y
integer , DIMENSION(max_domains) :: auxinput6_interval_d
integer , DIMENSION(max_domains) :: auxinput6_interval_h
integer , DIMENSION(max_domains) :: auxinput6_interval_m
integer , DIMENSION(max_domains) :: auxinput6_interval_s
integer , DIMENSION(max_domains) :: auxinput6_interval
integer , DIMENSION(max_domains) :: auxinput6_begin_y
integer , DIMENSION(max_domains) :: auxinput6_begin_d
integer , DIMENSION(max_domains) :: auxinput6_begin_h
integer , DIMENSION(max_domains) :: auxinput6_begin_m
integer , DIMENSION(max_domains) :: auxinput6_begin_s
integer , DIMENSION(max_domains) :: auxinput6_begin
integer , DIMENSION(max_domains) :: auxinput6_end_y
integer , DIMENSION(max_domains) :: auxinput6_end_d
integer , DIMENSION(max_domains) :: auxinput6_end_h
integer , DIMENSION(max_domains) :: auxinput6_end_m
integer , DIMENSION(max_domains) :: auxinput6_end_s
integer , DIMENSION(max_domains) :: auxinput6_end
integer :: io_form_auxinput6
integer , DIMENSION(max_domains) :: frames_per_auxinput6
character*256 :: auxinput7_inname
character*256 :: auxinput7_outname
integer , DIMENSION(max_domains) :: auxinput7_interval_y
integer , DIMENSION(max_domains) :: auxinput7_interval_d
integer , DIMENSION(max_domains) :: auxinput7_interval_h
integer , DIMENSION(max_domains) :: auxinput7_interval_m
integer , DIMENSION(max_domains) :: auxinput7_interval_s
integer , DIMENSION(max_domains) :: auxinput7_interval
integer , DIMENSION(max_domains) :: auxinput7_begin_y
integer , DIMENSION(max_domains) :: auxinput7_begin_d
integer , DIMENSION(max_domains) :: auxinput7_begin_h
integer , DIMENSION(max_domains) :: auxinput7_begin_m
integer , DIMENSION(max_domains) :: auxinput7_begin_s
integer , DIMENSION(max_domains) :: auxinput7_begin
integer , DIMENSION(max_domains) :: auxinput7_end_y
integer , DIMENSION(max_domains) :: auxinput7_end_d
integer , DIMENSION(max_domains) :: auxinput7_end_h
integer , DIMENSION(max_domains) :: auxinput7_end_m
integer , DIMENSION(max_domains) :: auxinput7_end_s
integer , DIMENSION(max_domains) :: auxinput7_end
integer :: io_form_auxinput7
integer , DIMENSION(max_domains) :: frames_per_auxinput7
character*256 :: auxinput8_inname
character*256 :: auxinput8_outname
integer , DIMENSION(max_domains) :: auxinput8_interval_y
integer , DIMENSION(max_domains) :: auxinput8_interval_d
integer , DIMENSION(max_domains) :: auxinput8_interval_h
integer , DIMENSION(max_domains) :: auxinput8_interval_m
integer , DIMENSION(max_domains) :: auxinput8_interval_s
integer , DIMENSION(max_domains) :: auxinput8_interval
integer , DIMENSION(max_domains) :: auxinput8_begin_y
integer , DIMENSION(max_domains) :: auxinput8_begin_d
integer , DIMENSION(max_domains) :: auxinput8_begin_h
integer , DIMENSION(max_domains) :: auxinput8_begin_m
integer , DIMENSION(max_domains) :: auxinput8_begin_s
integer , DIMENSION(max_domains) :: auxinput8_begin
integer , DIMENSION(max_domains) :: auxinput8_end_y
integer , DIMENSION(max_domains) :: auxinput8_end_d
integer , DIMENSION(max_domains) :: auxinput8_end_h
integer , DIMENSION(max_domains) :: auxinput8_end_m
integer , DIMENSION(max_domains) :: auxinput8_end_s
integer , DIMENSION(max_domains) :: auxinput8_end
integer :: io_form_auxinput8
integer , DIMENSION(max_domains) :: frames_per_auxinput8
character*256 :: auxinput9_inname
character*256 :: auxinput9_outname
integer , DIMENSION(max_domains) :: auxinput9_interval_y
integer , DIMENSION(max_domains) :: auxinput9_interval_d
integer , DIMENSION(max_domains) :: auxinput9_interval_h
integer , DIMENSION(max_domains) :: auxinput9_interval_m
integer , DIMENSION(max_domains) :: auxinput9_interval_s
integer , DIMENSION(max_domains) :: auxinput9_interval
integer , DIMENSION(max_domains) :: auxinput9_begin_y
integer , DIMENSION(max_domains) :: auxinput9_begin_d
integer , DIMENSION(max_domains) :: auxinput9_begin_h
integer , DIMENSION(max_domains) :: auxinput9_begin_m
integer , DIMENSION(max_domains) :: auxinput9_begin_s
integer , DIMENSION(max_domains) :: auxinput9_begin
integer , DIMENSION(max_domains) :: auxinput9_end_y
integer , DIMENSION(max_domains) :: auxinput9_end_d
integer , DIMENSION(max_domains) :: auxinput9_end_h
integer , DIMENSION(max_domains) :: auxinput9_end_m
integer , DIMENSION(max_domains) :: auxinput9_end_s
integer , DIMENSION(max_domains) :: auxinput9_end
integer :: io_form_auxinput9
integer , DIMENSION(max_domains) :: frames_per_auxinput9
character*256 :: auxinput10_inname
character*256 :: auxinput10_outname
integer , DIMENSION(max_domains) :: auxinput10_interval_y
integer , DIMENSION(max_domains) :: auxinput10_interval_d
integer , DIMENSION(max_domains) :: auxinput10_interval_h
integer , DIMENSION(max_domains) :: auxinput10_interval_m
integer , DIMENSION(max_domains) :: auxinput10_interval_s
integer , DIMENSION(max_domains) :: auxinput10_interval
integer , DIMENSION(max_domains) :: auxinput10_begin_y
integer , DIMENSION(max_domains) :: auxinput10_begin_d
integer , DIMENSION(max_domains) :: auxinput10_begin_h
integer , DIMENSION(max_domains) :: auxinput10_begin_m
integer , DIMENSION(max_domains) :: auxinput10_begin_s
integer , DIMENSION(max_domains) :: auxinput10_begin
integer , DIMENSION(max_domains) :: auxinput10_end_y
integer , DIMENSION(max_domains) :: auxinput10_end_d
integer , DIMENSION(max_domains) :: auxinput10_end_h
integer , DIMENSION(max_domains) :: auxinput10_end_m
integer , DIMENSION(max_domains) :: auxinput10_end_s
integer , DIMENSION(max_domains) :: auxinput10_end
integer :: io_form_auxinput10
integer , DIMENSION(max_domains) :: frames_per_auxinput10
character*256 :: auxinput11_inname
character*256 :: auxinput11_outname
integer , DIMENSION(max_domains) :: auxinput11_interval_y
integer , DIMENSION(max_domains) :: auxinput11_interval_d
integer , DIMENSION(max_domains) :: auxinput11_interval_h
integer , DIMENSION(max_domains) :: auxinput11_interval_m
integer , DIMENSION(max_domains) :: auxinput11_interval_s
integer , DIMENSION(max_domains) :: auxinput11_interval
integer , DIMENSION(max_domains) :: auxinput11_begin_y
integer , DIMENSION(max_domains) :: auxinput11_begin_d
integer , DIMENSION(max_domains) :: auxinput11_begin_h
integer , DIMENSION(max_domains) :: auxinput11_begin_m
integer , DIMENSION(max_domains) :: auxinput11_begin_s
integer , DIMENSION(max_domains) :: auxinput11_begin
integer , DIMENSION(max_domains) :: auxinput11_end_y
integer , DIMENSION(max_domains) :: auxinput11_end_d
integer , DIMENSION(max_domains) :: auxinput11_end_h
integer , DIMENSION(max_domains) :: auxinput11_end_m
integer , DIMENSION(max_domains) :: auxinput11_end_s
integer , DIMENSION(max_domains) :: auxinput11_end
integer :: io_form_auxinput11
integer , DIMENSION(max_domains) :: frames_per_auxinput11
character*256 :: auxinput12_inname
character*256 :: auxinput12_outname
integer , DIMENSION(max_domains) :: auxinput12_interval_y
integer , DIMENSION(max_domains) :: auxinput12_interval_d
integer , DIMENSION(max_domains) :: auxinput12_interval_h
integer , DIMENSION(max_domains) :: auxinput12_interval_m
integer , DIMENSION(max_domains) :: auxinput12_interval_s
integer , DIMENSION(max_domains) :: auxinput12_interval
integer , DIMENSION(max_domains) :: auxinput12_begin_y
integer , DIMENSION(max_domains) :: auxinput12_begin_d
integer , DIMENSION(max_domains) :: auxinput12_begin_h
integer , DIMENSION(max_domains) :: auxinput12_begin_m
integer , DIMENSION(max_domains) :: auxinput12_begin_s
integer , DIMENSION(max_domains) :: auxinput12_begin
integer , DIMENSION(max_domains) :: auxinput12_end_y
integer , DIMENSION(max_domains) :: auxinput12_end_d
integer , DIMENSION(max_domains) :: auxinput12_end_h
integer , DIMENSION(max_domains) :: auxinput12_end_m
integer , DIMENSION(max_domains) :: auxinput12_end_s
integer , DIMENSION(max_domains) :: auxinput12_end
integer :: io_form_auxinput12
integer , DIMENSION(max_domains) :: frames_per_auxinput12
character*256 :: auxinput13_inname
character*256 :: auxinput13_outname
integer , DIMENSION(max_domains) :: auxinput13_interval_y
integer , DIMENSION(max_domains) :: auxinput13_interval_d
integer , DIMENSION(max_domains) :: auxinput13_interval_h
integer , DIMENSION(max_domains) :: auxinput13_interval_m
integer , DIMENSION(max_domains) :: auxinput13_interval_s
integer , DIMENSION(max_domains) :: auxinput13_interval
integer , DIMENSION(max_domains) :: auxinput13_begin_y
integer , DIMENSION(max_domains) :: auxinput13_begin_d
integer , DIMENSION(max_domains) :: auxinput13_begin_h
integer , DIMENSION(max_domains) :: auxinput13_begin_m
integer , DIMENSION(max_domains) :: auxinput13_begin_s
integer , DIMENSION(max_domains) :: auxinput13_begin
integer , DIMENSION(max_domains) :: auxinput13_end_y
integer , DIMENSION(max_domains) :: auxinput13_end_d
integer , DIMENSION(max_domains) :: auxinput13_end_h
integer , DIMENSION(max_domains) :: auxinput13_end_m
integer , DIMENSION(max_domains) :: auxinput13_end_s
integer , DIMENSION(max_domains) :: auxinput13_end
integer :: io_form_auxinput13
integer , DIMENSION(max_domains) :: frames_per_auxinput13
character*256 :: auxinput14_inname
character*256 :: auxinput14_outname
integer , DIMENSION(max_domains) :: auxinput14_interval_y
integer , DIMENSION(max_domains) :: auxinput14_interval_d
integer , DIMENSION(max_domains) :: auxinput14_interval_h
integer , DIMENSION(max_domains) :: auxinput14_interval_m
integer , DIMENSION(max_domains) :: auxinput14_interval_s
integer , DIMENSION(max_domains) :: auxinput14_interval
integer , DIMENSION(max_domains) :: auxinput14_begin_y
integer , DIMENSION(max_domains) :: auxinput14_begin_d
integer , DIMENSION(max_domains) :: auxinput14_begin_h
integer , DIMENSION(max_domains) :: auxinput14_begin_m
integer , DIMENSION(max_domains) :: auxinput14_begin_s
integer , DIMENSION(max_domains) :: auxinput14_begin
integer , DIMENSION(max_domains) :: auxinput14_end_y
integer , DIMENSION(max_domains) :: auxinput14_end_d
integer , DIMENSION(max_domains) :: auxinput14_end_h
integer , DIMENSION(max_domains) :: auxinput14_end_m
integer , DIMENSION(max_domains) :: auxinput14_end_s
integer , DIMENSION(max_domains) :: auxinput14_end
integer :: io_form_auxinput14
integer , DIMENSION(max_domains) :: frames_per_auxinput14
character*256 :: auxinput15_inname
character*256 :: auxinput15_outname
integer , DIMENSION(max_domains) :: auxinput15_interval_y
integer , DIMENSION(max_domains) :: auxinput15_interval_d
integer , DIMENSION(max_domains) :: auxinput15_interval_h
integer , DIMENSION(max_domains) :: auxinput15_interval_m
integer , DIMENSION(max_domains) :: auxinput15_interval_s
integer , DIMENSION(max_domains) :: auxinput15_interval
integer , DIMENSION(max_domains) :: auxinput15_begin_y
integer , DIMENSION(max_domains) :: auxinput15_begin_d
integer , DIMENSION(max_domains) :: auxinput15_begin_h
integer , DIMENSION(max_domains) :: auxinput15_begin_m
integer , DIMENSION(max_domains) :: auxinput15_begin_s
integer , DIMENSION(max_domains) :: auxinput15_begin
integer , DIMENSION(max_domains) :: auxinput15_end_y
integer , DIMENSION(max_domains) :: auxinput15_end_d
integer , DIMENSION(max_domains) :: auxinput15_end_h
integer , DIMENSION(max_domains) :: auxinput15_end_m
integer , DIMENSION(max_domains) :: auxinput15_end_s
integer , DIMENSION(max_domains) :: auxinput15_end
integer :: io_form_auxinput15
integer , DIMENSION(max_domains) :: frames_per_auxinput15
character*256 :: auxinput16_inname
character*256 :: auxinput16_outname
integer , DIMENSION(max_domains) :: auxinput16_interval_y
integer , DIMENSION(max_domains) :: auxinput16_interval_d
integer , DIMENSION(max_domains) :: auxinput16_interval_h
integer , DIMENSION(max_domains) :: auxinput16_interval_m
integer , DIMENSION(max_domains) :: auxinput16_interval_s
integer , DIMENSION(max_domains) :: auxinput16_interval
integer , DIMENSION(max_domains) :: auxinput16_begin_y
integer , DIMENSION(max_domains) :: auxinput16_begin_d
integer , DIMENSION(max_domains) :: auxinput16_begin_h
integer , DIMENSION(max_domains) :: auxinput16_begin_m
integer , DIMENSION(max_domains) :: auxinput16_begin_s
integer , DIMENSION(max_domains) :: auxinput16_begin
integer , DIMENSION(max_domains) :: auxinput16_end_y
integer , DIMENSION(max_domains) :: auxinput16_end_d
integer , DIMENSION(max_domains) :: auxinput16_end_h
integer , DIMENSION(max_domains) :: auxinput16_end_m
integer , DIMENSION(max_domains) :: auxinput16_end_s
integer , DIMENSION(max_domains) :: auxinput16_end
integer :: io_form_auxinput16
integer , DIMENSION(max_domains) :: frames_per_auxinput16
character*256 :: auxinput17_inname
character*256 :: auxinput17_outname
integer , DIMENSION(max_domains) :: auxinput17_interval_y
integer , DIMENSION(max_domains) :: auxinput17_interval_d
integer , DIMENSION(max_domains) :: auxinput17_interval_h
integer , DIMENSION(max_domains) :: auxinput17_interval_m
integer , DIMENSION(max_domains) :: auxinput17_interval_s
integer , DIMENSION(max_domains) :: auxinput17_interval
integer , DIMENSION(max_domains) :: auxinput17_begin_y
integer , DIMENSION(max_domains) :: auxinput17_begin_d
integer , DIMENSION(max_domains) :: auxinput17_begin_h
integer , DIMENSION(max_domains) :: auxinput17_begin_m
integer , DIMENSION(max_domains) :: auxinput17_begin_s
integer , DIMENSION(max_domains) :: auxinput17_begin
integer , DIMENSION(max_domains) :: auxinput17_end_y
integer , DIMENSION(max_domains) :: auxinput17_end_d
integer , DIMENSION(max_domains) :: auxinput17_end_h
integer , DIMENSION(max_domains) :: auxinput17_end_m
integer , DIMENSION(max_domains) :: auxinput17_end_s
integer , DIMENSION(max_domains) :: auxinput17_end
integer :: io_form_auxinput17
integer , DIMENSION(max_domains) :: frames_per_auxinput17
character*256 :: auxinput18_inname
character*256 :: auxinput18_outname
integer , DIMENSION(max_domains) :: auxinput18_interval_y
integer , DIMENSION(max_domains) :: auxinput18_interval_d
integer , DIMENSION(max_domains) :: auxinput18_interval_h
integer , DIMENSION(max_domains) :: auxinput18_interval_m
integer , DIMENSION(max_domains) :: auxinput18_interval_s
integer , DIMENSION(max_domains) :: auxinput18_interval
integer , DIMENSION(max_domains) :: auxinput18_begin_y
integer , DIMENSION(max_domains) :: auxinput18_begin_d
integer , DIMENSION(max_domains) :: auxinput18_begin_h
integer , DIMENSION(max_domains) :: auxinput18_begin_m
integer , DIMENSION(max_domains) :: auxinput18_begin_s
integer , DIMENSION(max_domains) :: auxinput18_begin
integer , DIMENSION(max_domains) :: auxinput18_end_y
integer , DIMENSION(max_domains) :: auxinput18_end_d
integer , DIMENSION(max_domains) :: auxinput18_end_h
integer , DIMENSION(max_domains) :: auxinput18_end_m
integer , DIMENSION(max_domains) :: auxinput18_end_s
integer , DIMENSION(max_domains) :: auxinput18_end
integer :: io_form_auxinput18
integer , DIMENSION(max_domains) :: frames_per_auxinput18
character*256 :: auxinput19_inname
character*256 :: auxinput19_outname
integer , DIMENSION(max_domains) :: auxinput19_interval_y
integer , DIMENSION(max_domains) :: auxinput19_interval_d
integer , DIMENSION(max_domains) :: auxinput19_interval_h
integer , DIMENSION(max_domains) :: auxinput19_interval_m
integer , DIMENSION(max_domains) :: auxinput19_interval_s
integer , DIMENSION(max_domains) :: auxinput19_interval
integer , DIMENSION(max_domains) :: auxinput19_begin_y
integer , DIMENSION(max_domains) :: auxinput19_begin_d
integer , DIMENSION(max_domains) :: auxinput19_begin_h
integer , DIMENSION(max_domains) :: auxinput19_begin_m
integer , DIMENSION(max_domains) :: auxinput19_begin_s
integer , DIMENSION(max_domains) :: auxinput19_begin
integer , DIMENSION(max_domains) :: auxinput19_end_y
integer , DIMENSION(max_domains) :: auxinput19_end_d
integer , DIMENSION(max_domains) :: auxinput19_end_h
integer , DIMENSION(max_domains) :: auxinput19_end_m
integer , DIMENSION(max_domains) :: auxinput19_end_s
integer , DIMENSION(max_domains) :: auxinput19_end
integer :: io_form_auxinput19
integer , DIMENSION(max_domains) :: frames_per_auxinput19
character*256 :: auxinput20_inname
character*256 :: auxinput20_outname
integer , DIMENSION(max_domains) :: auxinput20_interval_y
integer , DIMENSION(max_domains) :: auxinput20_interval_d
integer , DIMENSION(max_domains) :: auxinput20_interval_h
integer , DIMENSION(max_domains) :: auxinput20_interval_m
integer , DIMENSION(max_domains) :: auxinput20_interval_s
integer , DIMENSION(max_domains) :: auxinput20_interval
integer , DIMENSION(max_domains) :: auxinput20_begin_y
integer , DIMENSION(max_domains) :: auxinput20_begin_d
integer , DIMENSION(max_domains) :: auxinput20_begin_h
integer , DIMENSION(max_domains) :: auxinput20_begin_m
integer , DIMENSION(max_domains) :: auxinput20_begin_s
integer , DIMENSION(max_domains) :: auxinput20_begin
integer , DIMENSION(max_domains) :: auxinput20_end_y
integer , DIMENSION(max_domains) :: auxinput20_end_d
integer , DIMENSION(max_domains) :: auxinput20_end_h
integer , DIMENSION(max_domains) :: auxinput20_end_m
integer , DIMENSION(max_domains) :: auxinput20_end_s
integer , DIMENSION(max_domains) :: auxinput20_end
integer :: io_form_auxinput20
integer , DIMENSION(max_domains) :: frames_per_auxinput20
character*256 :: auxinput21_inname
character*256 :: auxinput21_outname
integer , DIMENSION(max_domains) :: auxinput21_interval_y
integer , DIMENSION(max_domains) :: auxinput21_interval_d
integer , DIMENSION(max_domains) :: auxinput21_interval_h
integer , DIMENSION(max_domains) :: auxinput21_interval_m
integer , DIMENSION(max_domains) :: auxinput21_interval_s
integer , DIMENSION(max_domains) :: auxinput21_interval
integer , DIMENSION(max_domains) :: auxinput21_begin_y
integer , DIMENSION(max_domains) :: auxinput21_begin_d
integer , DIMENSION(max_domains) :: auxinput21_begin_h
integer , DIMENSION(max_domains) :: auxinput21_begin_m
integer , DIMENSION(max_domains) :: auxinput21_begin_s
integer , DIMENSION(max_domains) :: auxinput21_begin
integer , DIMENSION(max_domains) :: auxinput21_end_y
integer , DIMENSION(max_domains) :: auxinput21_end_d
integer , DIMENSION(max_domains) :: auxinput21_end_h
integer , DIMENSION(max_domains) :: auxinput21_end_m
integer , DIMENSION(max_domains) :: auxinput21_end_s
integer , DIMENSION(max_domains) :: auxinput21_end
integer :: io_form_auxinput21
integer , DIMENSION(max_domains) :: frames_per_auxinput21
character*256 :: auxinput22_inname
character*256 :: auxinput22_outname
integer , DIMENSION(max_domains) :: auxinput22_interval_y
integer , DIMENSION(max_domains) :: auxinput22_interval_d
integer , DIMENSION(max_domains) :: auxinput22_interval_h
integer , DIMENSION(max_domains) :: auxinput22_interval_m
integer , DIMENSION(max_domains) :: auxinput22_interval_s
integer , DIMENSION(max_domains) :: auxinput22_interval
integer , DIMENSION(max_domains) :: auxinput22_begin_y
integer , DIMENSION(max_domains) :: auxinput22_begin_d
integer , DIMENSION(max_domains) :: auxinput22_begin_h
integer , DIMENSION(max_domains) :: auxinput22_begin_m
integer , DIMENSION(max_domains) :: auxinput22_begin_s
integer , DIMENSION(max_domains) :: auxinput22_begin
integer , DIMENSION(max_domains) :: auxinput22_end_y
integer , DIMENSION(max_domains) :: auxinput22_end_d
integer , DIMENSION(max_domains) :: auxinput22_end_h
integer , DIMENSION(max_domains) :: auxinput22_end_m
integer , DIMENSION(max_domains) :: auxinput22_end_s
integer , DIMENSION(max_domains) :: auxinput22_end
integer :: io_form_auxinput22
integer , DIMENSION(max_domains) :: frames_per_auxinput22
character*256 :: auxinput23_inname
character*256 :: auxinput23_outname
integer , DIMENSION(max_domains) :: auxinput23_interval_y
integer , DIMENSION(max_domains) :: auxinput23_interval_d
integer , DIMENSION(max_domains) :: auxinput23_interval_h
integer , DIMENSION(max_domains) :: auxinput23_interval_m
integer , DIMENSION(max_domains) :: auxinput23_interval_s
integer , DIMENSION(max_domains) :: auxinput23_interval
integer , DIMENSION(max_domains) :: auxinput23_begin_y
integer , DIMENSION(max_domains) :: auxinput23_begin_d
integer , DIMENSION(max_domains) :: auxinput23_begin_h
integer , DIMENSION(max_domains) :: auxinput23_begin_m
integer , DIMENSION(max_domains) :: auxinput23_begin_s
integer , DIMENSION(max_domains) :: auxinput23_begin
integer , DIMENSION(max_domains) :: auxinput23_end_y
integer , DIMENSION(max_domains) :: auxinput23_end_d
integer , DIMENSION(max_domains) :: auxinput23_end_h
integer , DIMENSION(max_domains) :: auxinput23_end_m
integer , DIMENSION(max_domains) :: auxinput23_end_s
integer , DIMENSION(max_domains) :: auxinput23_end
integer :: io_form_auxinput23
integer , DIMENSION(max_domains) :: frames_per_auxinput23
character*256 :: auxinput24_inname
character*256 :: auxinput24_outname
integer , DIMENSION(max_domains) :: auxinput24_interval_y
integer , DIMENSION(max_domains) :: auxinput24_interval_d
integer , DIMENSION(max_domains) :: auxinput24_interval_h
integer , DIMENSION(max_domains) :: auxinput24_interval_m
integer , DIMENSION(max_domains) :: auxinput24_interval_s
integer , DIMENSION(max_domains) :: auxinput24_interval
integer , DIMENSION(max_domains) :: auxinput24_begin_y
integer , DIMENSION(max_domains) :: auxinput24_begin_d
integer , DIMENSION(max_domains) :: auxinput24_begin_h
integer , DIMENSION(max_domains) :: auxinput24_begin_m
integer , DIMENSION(max_domains) :: auxinput24_begin_s
integer , DIMENSION(max_domains) :: auxinput24_begin
integer , DIMENSION(max_domains) :: auxinput24_end_y
integer , DIMENSION(max_domains) :: auxinput24_end_d
integer , DIMENSION(max_domains) :: auxinput24_end_h
integer , DIMENSION(max_domains) :: auxinput24_end_m
integer , DIMENSION(max_domains) :: auxinput24_end_s
integer , DIMENSION(max_domains) :: auxinput24_end
integer :: io_form_auxinput24
integer , DIMENSION(max_domains) :: frames_per_auxinput24
integer , DIMENSION(max_domains) :: history_interval
integer , DIMENSION(max_domains) :: frames_per_outfile
logical :: restart
integer :: restart_interval
integer :: io_form_input
integer :: io_form_history
integer :: io_form_restart
integer :: io_form_boundary
integer :: debug_level
logical :: self_test_domain
character*256 :: history_outname
character*256 :: history_inname
logical :: use_netcdf_classic
integer , DIMENSION(max_domains) :: history_interval_d
integer , DIMENSION(max_domains) :: history_interval_h
integer , DIMENSION(max_domains) :: history_interval_m
integer , DIMENSION(max_domains) :: history_interval_s
integer , DIMENSION(max_domains) :: inputout_interval_d
integer , DIMENSION(max_domains) :: inputout_interval_h
integer , DIMENSION(max_domains) :: inputout_interval_m
integer , DIMENSION(max_domains) :: inputout_interval_s
integer , DIMENSION(max_domains) :: inputout_interval
integer :: restart_interval_d
integer :: restart_interval_h
integer :: restart_interval_m
integer :: restart_interval_s
integer , DIMENSION(max_domains) :: history_begin_y
integer , DIMENSION(max_domains) :: history_begin_d
integer , DIMENSION(max_domains) :: history_begin_h
integer , DIMENSION(max_domains) :: history_begin_m
integer , DIMENSION(max_domains) :: history_begin_s
integer , DIMENSION(max_domains) :: history_begin
integer , DIMENSION(max_domains) :: inputout_begin_y
integer , DIMENSION(max_domains) :: inputout_begin_d
integer , DIMENSION(max_domains) :: inputout_begin_h
integer , DIMENSION(max_domains) :: inputout_begin_m
integer , DIMENSION(max_domains) :: inputout_begin_s
integer :: restart_begin_y
integer :: restart_begin_d
integer :: restart_begin_h
integer :: restart_begin_m
integer :: restart_begin_s
integer :: restart_begin
integer , DIMENSION(max_domains) :: history_end_y
integer , DIMENSION(max_domains) :: history_end_d
integer , DIMENSION(max_domains) :: history_end_h
integer , DIMENSION(max_domains) :: history_end_m
integer , DIMENSION(max_domains) :: history_end_s
integer , DIMENSION(max_domains) :: history_end
integer , DIMENSION(max_domains) :: inputout_end_y
integer , DIMENSION(max_domains) :: inputout_end_d
integer , DIMENSION(max_domains) :: inputout_end_h
integer , DIMENSION(max_domains) :: inputout_end_m
integer , DIMENSION(max_domains) :: inputout_end_s
integer :: simulation_start_year
integer :: simulation_start_month
integer :: simulation_start_day
integer :: simulation_start_hour
integer :: simulation_start_minute
integer :: simulation_start_second
logical :: reset_simulation_start
integer , DIMENSION(max_domains) :: sr_x
integer , DIMENSION(max_domains) :: sr_y
character*256 , DIMENSION(max_domains) :: iofields_filename
logical :: ignore_iofields_warning
logical :: ncd_nofill
integer , DIMENSION(max_domains) :: julyr
integer , DIMENSION(max_domains) :: julday
real , DIMENSION(max_domains) :: gmt
character*256 :: high_freq_outname
character*256 :: partial_atcf_outname
character*256 :: input_inname
character*256 :: input_outname
character*256 :: bdy_inname
character*256 :: bdy_outname
character*256 :: rst_inname
character*256 :: rst_outname
character*256 , DIMENSION(max_domains) :: anl_outname
logical :: write_input
logical :: write_restart_at_0h
logical :: write_hist_at_0h_rst
logical :: adjust_output_times
logical :: adjust_input_times
real , DIMENSION(max_domains) :: tstart
logical :: nocolons
logical :: cycling
logical :: output_ready_flag
integer :: dfi_opt
integer :: dfi_savehydmeteors
integer :: dfi_nfilter
logical :: dfi_write_filtered_input
logical :: dfi_write_dfi_history
integer :: dfi_cutoff_seconds
integer :: dfi_time_dim
integer :: dfi_fwdstop_year
integer :: dfi_fwdstop_month
integer :: dfi_fwdstop_day
integer :: dfi_fwdstop_hour
integer :: dfi_fwdstop_minute
integer :: dfi_fwdstop_second
integer :: dfi_bckstop_year
integer :: dfi_bckstop_month
integer :: dfi_bckstop_day
integer :: dfi_bckstop_hour
integer :: dfi_bckstop_minute
integer :: dfi_bckstop_second
integer :: time_step
integer :: time_step_fract_num
integer :: time_step_fract_den
integer :: time_step_dfi
integer :: max_dom
integer , DIMENSION(max_domains) :: s_we
integer , DIMENSION(max_domains) :: e_we
integer , DIMENSION(max_domains) :: s_sn
integer , DIMENSION(max_domains) :: e_sn
integer , DIMENSION(max_domains) :: s_vert
integer , DIMENSION(max_domains) :: e_vert
integer :: num_metgrid_soil_levels
real , DIMENSION(max_domains) :: dx
real , DIMENSION(max_domains) :: dy
integer , DIMENSION(max_domains) :: grid_id
logical , DIMENSION(max_domains) :: grid_allowed
integer , DIMENSION(max_domains) :: parent_id
integer , DIMENSION(max_domains) :: i_parent_start
integer , DIMENSION(max_domains) :: j_parent_start
integer , DIMENSION(max_domains) :: parent_grid_ratio
integer , DIMENSION(max_domains) :: parent_time_step_ratio
integer :: feedback
integer :: smooth_option
real , DIMENSION(max_domains) :: ztop
integer , DIMENSION(max_domains) :: moad_grid_ratio
integer , DIMENSION(max_domains) :: moad_time_step_ratio
integer , DIMENSION(max_domains) :: shw
integer :: tile_sz_x
integer :: tile_sz_y
integer :: numtiles
integer :: numtiles_inc
integer :: numtiles_x
integer :: numtiles_y
integer :: tile_strategy
integer :: nproc_x
integer :: nproc_y
integer :: irand
real , DIMENSION(max_domains) :: dt
integer :: ts_buf_size
integer :: max_ts_locs
integer :: num_moves
integer , DIMENSION(max_domains) :: vortex_interval
integer , DIMENSION(max_domains) :: corral_dist
integer , DIMENSION(max_moves) :: move_id
integer , DIMENSION(max_moves) :: move_interval
integer , DIMENSION(max_moves) :: move_cd_x
integer , DIMENSION(max_moves) :: move_cd_y
logical , DIMENSION(max_domains) :: swap_x
logical , DIMENSION(max_domains) :: swap_y
logical , DIMENSION(max_domains) :: cycle_x
logical , DIMENSION(max_domains) :: cycle_y
logical :: reorder_mesh
logical :: perturb_input
real , DIMENSION(max_eta) :: eta_levels
real :: ptsgm
integer :: num_metgrid_levels
real :: p_top_requested
logical :: use_prep_hybrid
logical :: force_read_thompson
logical :: write_thompson_tables
integer , DIMENSION(max_domains) :: mp_physics
real , DIMENSION(max_domains) :: mommix
logical , DIMENSION(max_domains) :: disheat
integer :: do_radar_ref
integer :: compute_radar_ref
integer , DIMENSION(max_domains) :: ra_lw_physics
integer , DIMENSION(max_domains) :: ra_sw_physics
real , DIMENSION(max_domains) :: radt
integer , DIMENSION(max_domains) :: sf_sfclay_physics
integer , DIMENSION(max_domains) :: sf_surface_physics
integer , DIMENSION(max_domains) :: bl_pbl_physics
integer , DIMENSION(max_domains) :: ysu_topdown_pblmix
integer , DIMENSION(max_domains) :: shinhong_tke_diag
integer , DIMENSION(max_domains) :: windfarm_opt
integer :: windfarm_ij
integer , DIMENSION(max_domains) :: mfshconv
real , DIMENSION(max_domains) :: bldt
integer , DIMENSION(max_domains) :: cu_physics
integer , DIMENSION(max_domains) :: shcu_physics
integer , DIMENSION(max_domains) :: cu_diag
real , DIMENSION(max_domains) :: gfs_alpha
real , DIMENSION(max_domains) :: cudt
real , DIMENSION(max_domains) :: gsmdt
integer :: isfflx
integer :: ideal_xland
integer :: ifsnow
integer :: icloud
real :: swrad_scat
integer :: surface_input_source
integer :: num_soil_layers
integer :: num_urban_layers
integer :: sf_surface_mosaic
integer :: mosaic_cat
integer :: mosaic_cat_soil
integer :: num_urban_hi
integer :: mosaic_lu
integer :: mosaic_soil
integer :: maxiens
integer :: maxens
integer :: maxens2
integer :: maxens3
integer :: ensdim
integer , DIMENSION(max_domains) :: chem_opt
integer :: num_land_cat
integer :: num_soil_cat
integer , DIMENSION(max_domains) :: topo_wind
integer :: mp_zero_out
real :: mp_zero_out_thresh
real :: seaice_threshold
integer :: fractional_seaice
integer :: seaice_albedo_opt
real :: seaice_albedo_default
integer :: seaice_snowdepth_opt
real :: seaice_snowdepth_max
real :: seaice_snowdepth_min
integer :: seaice_thickness_opt
real :: seaice_thickness_default
logical :: tice2tsk_if2cold
integer :: sst_update
integer , DIMENSION(max_domains) :: sf_urban_physics
logical :: usemonalb
logical :: rdmaxalb
logical :: rdlai2d
logical :: ua_phys
integer , DIMENSION(max_domains) :: gwd_opt
integer :: iz0tlnd
real , DIMENSION(max_domains) :: sas_pgcon
real , DIMENSION(max_domains) :: sas_shal_pgcon
integer , DIMENSION(max_domains) :: sas_shal_conv
real , DIMENSION(max_domains) :: sas_mass_flux
real :: var_ric
real :: coef_ric_l
real :: coef_ric_s
integer , DIMENSION(max_domains) :: random_seed
integer , DIMENSION(max_domains) :: icoef_sf
logical , DIMENSION(max_domains) :: lcurr_sf
integer , DIMENSION(max_domains) :: ens_random_seed
logical :: pert_sas
logical :: pert_pbl
real , DIMENSION(max_domains) :: ens_sasamp
real , DIMENSION(max_domains) :: ens_pblamp
integer , DIMENSION(max_domains) :: idtad
integer , DIMENSION(max_domains) :: nsoil
integer , DIMENSION(max_domains) :: nphs
integer , DIMENSION(max_domains) :: ncnvc
integer , DIMENSION(max_domains) :: nrand
integer , DIMENSION(max_domains) :: nrads
integer , DIMENSION(max_domains) :: nradl
real , DIMENSION(max_domains) :: tprec
real , DIMENSION(max_domains) :: theat
real , DIMENSION(max_domains) :: tclod
real , DIMENSION(max_domains) :: trdsw
real , DIMENSION(max_domains) :: trdlw
real , DIMENSION(max_domains) :: tsrfc
logical , DIMENSION(max_domains) :: pcpflg
integer , DIMENSION(max_domains) :: sigma
real , DIMENSION(max_domains) :: sfenth
integer :: co2tf
integer :: ra_call_offset
real :: cam_abs_freq_s
integer :: levsiz
integer :: paerlev
integer :: cam_abs_dim1
integer :: cam_abs_dim2
integer :: no_src_types
integer :: alevsiz
integer :: o3input
integer :: aer_opt
logical , DIMENSION(max_domains) :: cu_rad_feedback
integer :: icloud_cu
real , DIMENSION(max_domains) :: h_diff
integer , DIMENSION(max_domains) :: movemin
integer :: num_snso_layers
integer :: num_snow_layers
logical :: use_aero_icbc
real :: ccn_conc
integer :: hail_opt
integer , DIMENSION(max_domains) :: sf_lake_physics
integer :: dyn_opt
integer :: rk_ord
integer :: w_damping
integer , DIMENSION(max_domains) :: diff_opt
integer , DIMENSION(max_domains) :: km_opt
integer :: damp_opt
real , DIMENSION(max_domains) :: zdamp
real :: base_pres
real :: base_temp
real :: base_lapse
real :: iso_temp
real , DIMENSION(max_domains) :: dampcoef
real , DIMENSION(max_domains) :: khdif
real , DIMENSION(max_domains) :: kvdif
real , DIMENSION(max_domains) :: c_s
real , DIMENSION(max_domains) :: c_k
real , DIMENSION(max_domains) :: smdiv
real , DIMENSION(max_domains) :: emdiv
real , DIMENSION(max_domains) :: epssm
logical , DIMENSION(max_domains) :: non_hydrostatic
integer , DIMENSION(max_domains) :: time_step_sound
integer , DIMENSION(max_domains) :: h_mom_adv_order
integer , DIMENSION(max_domains) :: v_mom_adv_order
integer , DIMENSION(max_domains) :: h_sca_adv_order
integer , DIMENSION(max_domains) :: v_sca_adv_order
logical , DIMENSION(max_domains) :: top_radiation
real , DIMENSION(max_domains) :: tke_upper_bound
real , DIMENSION(max_domains) :: tke_drag_coefficient
real , DIMENSION(max_domains) :: tke_heat_flux
logical , DIMENSION(max_domains) :: pert_coriolis
logical :: euler_adv
integer :: idtadt
integer :: idtadc
real , DIMENSION(max_domains) :: codamp
real , DIMENSION(max_domains) :: coac
real , DIMENSION(max_domains) :: slophc
real , DIMENSION(max_domains) :: wp
integer :: terrain_smoothing
integer :: spec_bdy_width
integer :: spec_zone
integer :: relax_zone
logical , DIMENSION(max_domains) :: specified
logical , DIMENSION(max_domains) :: periodic_x
logical , DIMENSION(max_domains) :: symmetric_xs
logical , DIMENSION(max_domains) :: symmetric_xe
logical , DIMENSION(max_domains) :: open_xs
logical , DIMENSION(max_domains) :: open_xe
logical , DIMENSION(max_domains) :: periodic_y
logical , DIMENSION(max_domains) :: symmetric_ys
logical , DIMENSION(max_domains) :: symmetric_ye
logical , DIMENSION(max_domains) :: open_ys
logical , DIMENSION(max_domains) :: open_ye
logical , DIMENSION(max_domains) :: polar
logical , DIMENSION(max_domains) :: nested
integer :: real_data_init_type
integer :: background_proc_id
integer :: forecast_proc_id
integer :: production_status
integer :: compression
real , DIMENSION(max_domains) :: cen_lat
real , DIMENSION(max_domains) :: cen_lon
real , DIMENSION(max_domains) :: truelat1
real , DIMENSION(max_domains) :: truelat2
real , DIMENSION(max_domains) :: moad_cen_lat
real , DIMENSION(max_domains) :: stand_lon
integer :: flag_metgrid
integer :: flag_snow
integer :: flag_psfc
integer :: flag_sm000010
integer :: flag_sm010040
integer :: flag_sm040100
integer :: flag_sm100200
integer :: flag_st000010
integer :: flag_st010040
integer :: flag_st040100
integer :: flag_st100200
integer :: flag_slp
integer :: flag_soilhgt
integer :: flag_mf_xy
real , DIMENSION(max_domains) :: bdyfrq
character*256 , DIMENSION(max_domains) :: mminlu
integer , DIMENSION(max_domains) :: iswater
integer , DIMENSION(max_domains) :: islake
integer , DIMENSION(max_domains) :: isice
integer , DIMENSION(max_domains) :: isurban
integer , DIMENSION(max_domains) :: isoilwater
integer , DIMENSION(max_domains) :: map_proj
integer :: dfi_stage
integer , DIMENSION(max_domains) :: mp_physics_dfi
integer :: maxpatch
integer    :: last_item_in_struct









NAMELIST /physics/ lakedepth_default
NAMELIST /physics/ lake_min_elev
NAMELIST /physics/ use_lakedepth
NAMELIST /domains/ halo_debug
NAMELIST /physics/ ntracers
NAMELIST /physics/ vortex_tracker
NAMELIST /physics/ interest_rad_storm
NAMELIST /physics/ interest_rad_parent
NAMELIST /physics/ interest_rad_self
NAMELIST /physics/ interest_kids
NAMELIST /physics/ interest_self
NAMELIST /physics/ interest_storms
NAMELIST /physics/ swath_mode
NAMELIST /physics/ num_old_fixes
NAMELIST /physics/ vt4_radius
NAMELIST /physics/ vt4_weightexp
NAMELIST /physics/ vt4_pmax
NAMELIST /physics/ vt4_noise_pmax
NAMELIST /physics/ vt4_noise_pmin
NAMELIST /physics/ vt4_noise_dpdr
NAMELIST /physics/ vt4_noise_iter
NAMELIST /physics/ nomove_freq
NAMELIST /domains/ coral_x
NAMELIST /domains/ coral_y
NAMELIST /time_control/ tg_reset_stream
NAMELIST /physics/ tg_option
NAMELIST /physics/ ntornado
NAMELIST /time_control/ analysis
NAMELIST /time_control/ write_analysis
NAMELIST /time_control/ io_form_auxinput2
NAMELIST /time_control/ high_freq
NAMELIST /time_control/ high_dom
NAMELIST /physics/ swint_opt
NAMELIST /physics/ aer_type
NAMELIST /physics/ aer_aod550_opt
NAMELIST /physics/ aer_angexp_opt
NAMELIST /physics/ aer_ssa_opt
NAMELIST /physics/ aer_asy_opt
NAMELIST /physics/ aer_aod550_val
NAMELIST /physics/ aer_angexp_val
NAMELIST /physics/ aer_ssa_val
NAMELIST /physics/ aer_asy_val
NAMELIST /noah_mp/ dveg
NAMELIST /noah_mp/ opt_crs
NAMELIST /noah_mp/ opt_btr
NAMELIST /noah_mp/ opt_run
NAMELIST /noah_mp/ opt_sfc
NAMELIST /noah_mp/ opt_frz
NAMELIST /noah_mp/ opt_inf
NAMELIST /noah_mp/ opt_rad
NAMELIST /noah_mp/ opt_alb
NAMELIST /noah_mp/ opt_snf
NAMELIST /noah_mp/ opt_tbot
NAMELIST /noah_mp/ opt_stc
NAMELIST /time_control/ run_days
NAMELIST /time_control/ run_hours
NAMELIST /time_control/ run_minutes
NAMELIST /time_control/ run_seconds
NAMELIST /time_control/ start_year
NAMELIST /time_control/ start_month
NAMELIST /time_control/ start_day
NAMELIST /time_control/ start_hour
NAMELIST /time_control/ start_minute
NAMELIST /time_control/ start_second
NAMELIST /time_control/ end_year
NAMELIST /time_control/ end_month
NAMELIST /time_control/ end_day
NAMELIST /time_control/ end_hour
NAMELIST /time_control/ end_minute
NAMELIST /time_control/ end_second
NAMELIST /time_control/ interval_seconds
NAMELIST /time_control/ input_from_file
NAMELIST /time_control/ fine_input_stream
NAMELIST /time_control/ auxinput1_inname
NAMELIST /time_control/ io_form_auxinput1
NAMELIST /time_control/ override_restart_timers
NAMELIST /time_control/ auxhist1_inname
NAMELIST /time_control/ auxhist1_outname
NAMELIST /time_control/ auxhist1_interval_y
NAMELIST /time_control/ auxhist1_interval_d
NAMELIST /time_control/ auxhist1_interval_h
NAMELIST /time_control/ auxhist1_interval_m
NAMELIST /time_control/ auxhist1_interval_s
NAMELIST /time_control/ auxhist1_interval
NAMELIST /time_control/ auxhist1_begin_y
NAMELIST /time_control/ auxhist1_begin_d
NAMELIST /time_control/ auxhist1_begin_h
NAMELIST /time_control/ auxhist1_begin_m
NAMELIST /time_control/ auxhist1_begin_s
NAMELIST /time_control/ auxhist1_begin
NAMELIST /time_control/ auxhist1_end_y
NAMELIST /time_control/ auxhist1_end_d
NAMELIST /time_control/ auxhist1_end_h
NAMELIST /time_control/ auxhist1_end_m
NAMELIST /time_control/ auxhist1_end_s
NAMELIST /time_control/ auxhist1_end
NAMELIST /time_control/ io_form_auxhist1
NAMELIST /time_control/ frames_per_auxhist1
NAMELIST /time_control/ auxhist2_inname
NAMELIST /time_control/ auxhist2_outname
NAMELIST /time_control/ auxhist2_interval_y
NAMELIST /time_control/ auxhist2_interval_d
NAMELIST /time_control/ auxhist2_interval_h
NAMELIST /time_control/ auxhist2_interval_m
NAMELIST /time_control/ auxhist2_interval_s
NAMELIST /time_control/ auxhist2_interval
NAMELIST /time_control/ auxhist2_begin_y
NAMELIST /time_control/ auxhist2_begin_d
NAMELIST /time_control/ auxhist2_begin_h
NAMELIST /time_control/ auxhist2_begin_m
NAMELIST /time_control/ auxhist2_begin_s
NAMELIST /time_control/ auxhist2_begin
NAMELIST /time_control/ auxhist2_end_y
NAMELIST /time_control/ auxhist2_end_d
NAMELIST /time_control/ auxhist2_end_h
NAMELIST /time_control/ auxhist2_end_m
NAMELIST /time_control/ auxhist2_end_s
NAMELIST /time_control/ auxhist2_end
NAMELIST /time_control/ io_form_auxhist2
NAMELIST /time_control/ frames_per_auxhist2
NAMELIST /time_control/ auxhist3_inname
NAMELIST /time_control/ auxhist3_outname
NAMELIST /time_control/ auxhist3_interval_y
NAMELIST /time_control/ auxhist3_interval_d
NAMELIST /time_control/ auxhist3_interval_h
NAMELIST /time_control/ auxhist3_interval_m
NAMELIST /time_control/ auxhist3_interval_s
NAMELIST /time_control/ auxhist3_interval
NAMELIST /time_control/ auxhist3_begin_y
NAMELIST /time_control/ auxhist3_begin_d
NAMELIST /time_control/ auxhist3_begin_h
NAMELIST /time_control/ auxhist3_begin_m
NAMELIST /time_control/ auxhist3_begin_s
NAMELIST /time_control/ auxhist3_begin
NAMELIST /time_control/ auxhist3_end_y
NAMELIST /time_control/ auxhist3_end_d
NAMELIST /time_control/ auxhist3_end_h
NAMELIST /time_control/ auxhist3_end_m
NAMELIST /time_control/ auxhist3_end_s
NAMELIST /time_control/ auxhist3_end
NAMELIST /time_control/ io_form_auxhist3
NAMELIST /time_control/ frames_per_auxhist3
NAMELIST /time_control/ auxhist4_inname
NAMELIST /time_control/ auxhist4_outname
NAMELIST /time_control/ auxhist4_interval_y
NAMELIST /time_control/ auxhist4_interval_d
NAMELIST /time_control/ auxhist4_interval_h
NAMELIST /time_control/ auxhist4_interval_m
NAMELIST /time_control/ auxhist4_interval_s
NAMELIST /time_control/ auxhist4_interval
NAMELIST /time_control/ auxhist4_begin_y
NAMELIST /time_control/ auxhist4_begin_d
NAMELIST /time_control/ auxhist4_begin_h
NAMELIST /time_control/ auxhist4_begin_m
NAMELIST /time_control/ auxhist4_begin_s
NAMELIST /time_control/ auxhist4_begin
NAMELIST /time_control/ auxhist4_end_y
NAMELIST /time_control/ auxhist4_end_d
NAMELIST /time_control/ auxhist4_end_h
NAMELIST /time_control/ auxhist4_end_m
NAMELIST /time_control/ auxhist4_end_s
NAMELIST /time_control/ auxhist4_end
NAMELIST /time_control/ io_form_auxhist4
NAMELIST /time_control/ frames_per_auxhist4
NAMELIST /time_control/ auxhist5_inname
NAMELIST /time_control/ auxhist5_outname
NAMELIST /time_control/ auxhist5_interval_y
NAMELIST /time_control/ auxhist5_interval_d
NAMELIST /time_control/ auxhist5_interval_h
NAMELIST /time_control/ auxhist5_interval_m
NAMELIST /time_control/ auxhist5_interval_s
NAMELIST /time_control/ auxhist5_interval
NAMELIST /time_control/ auxhist5_begin_y
NAMELIST /time_control/ auxhist5_begin_d
NAMELIST /time_control/ auxhist5_begin_h
NAMELIST /time_control/ auxhist5_begin_m
NAMELIST /time_control/ auxhist5_begin_s
NAMELIST /time_control/ auxhist5_begin
NAMELIST /time_control/ auxhist5_end_y
NAMELIST /time_control/ auxhist5_end_d
NAMELIST /time_control/ auxhist5_end_h
NAMELIST /time_control/ auxhist5_end_m
NAMELIST /time_control/ auxhist5_end_s
NAMELIST /time_control/ auxhist5_end
NAMELIST /time_control/ io_form_auxhist5
NAMELIST /time_control/ frames_per_auxhist5
NAMELIST /time_control/ auxhist6_inname
NAMELIST /time_control/ auxhist6_outname
NAMELIST /time_control/ auxhist6_interval_y
NAMELIST /time_control/ auxhist6_interval_d
NAMELIST /time_control/ auxhist6_interval_h
NAMELIST /time_control/ auxhist6_interval_m
NAMELIST /time_control/ auxhist6_interval_s
NAMELIST /time_control/ auxhist6_interval
NAMELIST /time_control/ auxhist6_begin_y
NAMELIST /time_control/ auxhist6_begin_d
NAMELIST /time_control/ auxhist6_begin_h
NAMELIST /time_control/ auxhist6_begin_m
NAMELIST /time_control/ auxhist6_begin_s
NAMELIST /time_control/ auxhist6_begin
NAMELIST /time_control/ auxhist6_end_y
NAMELIST /time_control/ auxhist6_end_d
NAMELIST /time_control/ auxhist6_end_h
NAMELIST /time_control/ auxhist6_end_m
NAMELIST /time_control/ auxhist6_end_s
NAMELIST /time_control/ auxhist6_end
NAMELIST /time_control/ io_form_auxhist6
NAMELIST /time_control/ frames_per_auxhist6
NAMELIST /time_control/ auxhist7_inname
NAMELIST /time_control/ auxhist7_outname
NAMELIST /time_control/ auxhist7_interval_y
NAMELIST /time_control/ auxhist7_interval_d
NAMELIST /time_control/ auxhist7_interval_h
NAMELIST /time_control/ auxhist7_interval_m
NAMELIST /time_control/ auxhist7_interval_s
NAMELIST /time_control/ auxhist7_interval
NAMELIST /time_control/ auxhist7_begin_y
NAMELIST /time_control/ auxhist7_begin_d
NAMELIST /time_control/ auxhist7_begin_h
NAMELIST /time_control/ auxhist7_begin_m
NAMELIST /time_control/ auxhist7_begin_s
NAMELIST /time_control/ auxhist7_begin
NAMELIST /time_control/ auxhist7_end_y
NAMELIST /time_control/ auxhist7_end_d
NAMELIST /time_control/ auxhist7_end_h
NAMELIST /time_control/ auxhist7_end_m
NAMELIST /time_control/ auxhist7_end_s
NAMELIST /time_control/ auxhist7_end
NAMELIST /time_control/ io_form_auxhist7
NAMELIST /time_control/ frames_per_auxhist7
NAMELIST /time_control/ auxhist8_inname
NAMELIST /time_control/ auxhist8_outname
NAMELIST /time_control/ auxhist8_interval_y
NAMELIST /time_control/ auxhist8_interval_d
NAMELIST /time_control/ auxhist8_interval_h
NAMELIST /time_control/ auxhist8_interval_m
NAMELIST /time_control/ auxhist8_interval_s
NAMELIST /time_control/ auxhist8_interval
NAMELIST /time_control/ auxhist8_begin_y
NAMELIST /time_control/ auxhist8_begin_d
NAMELIST /time_control/ auxhist8_begin_h
NAMELIST /time_control/ auxhist8_begin_m
NAMELIST /time_control/ auxhist8_begin_s
NAMELIST /time_control/ auxhist8_begin
NAMELIST /time_control/ auxhist8_end_y
NAMELIST /time_control/ auxhist8_end_d
NAMELIST /time_control/ auxhist8_end_h
NAMELIST /time_control/ auxhist8_end_m
NAMELIST /time_control/ auxhist8_end_s
NAMELIST /time_control/ auxhist8_end
NAMELIST /time_control/ io_form_auxhist8
NAMELIST /time_control/ frames_per_auxhist8
NAMELIST /time_control/ auxhist9_inname
NAMELIST /time_control/ auxhist9_outname
NAMELIST /time_control/ auxhist9_interval_y
NAMELIST /time_control/ auxhist9_interval_d
NAMELIST /time_control/ auxhist9_interval_h
NAMELIST /time_control/ auxhist9_interval_m
NAMELIST /time_control/ auxhist9_interval_s
NAMELIST /time_control/ auxhist9_interval
NAMELIST /time_control/ auxhist9_begin_y
NAMELIST /time_control/ auxhist9_begin_d
NAMELIST /time_control/ auxhist9_begin_h
NAMELIST /time_control/ auxhist9_begin_m
NAMELIST /time_control/ auxhist9_begin_s
NAMELIST /time_control/ auxhist9_begin
NAMELIST /time_control/ auxhist9_end_y
NAMELIST /time_control/ auxhist9_end_d
NAMELIST /time_control/ auxhist9_end_h
NAMELIST /time_control/ auxhist9_end_m
NAMELIST /time_control/ auxhist9_end_s
NAMELIST /time_control/ auxhist9_end
NAMELIST /time_control/ io_form_auxhist9
NAMELIST /time_control/ frames_per_auxhist9
NAMELIST /time_control/ auxhist10_inname
NAMELIST /time_control/ auxhist10_outname
NAMELIST /time_control/ auxhist10_interval_y
NAMELIST /time_control/ auxhist10_interval_d
NAMELIST /time_control/ auxhist10_interval_h
NAMELIST /time_control/ auxhist10_interval_m
NAMELIST /time_control/ auxhist10_interval_s
NAMELIST /time_control/ auxhist10_interval
NAMELIST /time_control/ auxhist10_begin_y
NAMELIST /time_control/ auxhist10_begin_d
NAMELIST /time_control/ auxhist10_begin_h
NAMELIST /time_control/ auxhist10_begin_m
NAMELIST /time_control/ auxhist10_begin_s
NAMELIST /time_control/ auxhist10_begin
NAMELIST /time_control/ auxhist10_end_y
NAMELIST /time_control/ auxhist10_end_d
NAMELIST /time_control/ auxhist10_end_h
NAMELIST /time_control/ auxhist10_end_m
NAMELIST /time_control/ auxhist10_end_s
NAMELIST /time_control/ auxhist10_end
NAMELIST /time_control/ io_form_auxhist10
NAMELIST /time_control/ frames_per_auxhist10
NAMELIST /time_control/ auxhist11_inname
NAMELIST /time_control/ auxhist11_outname
NAMELIST /time_control/ auxhist11_interval_y
NAMELIST /time_control/ auxhist11_interval_d
NAMELIST /time_control/ auxhist11_interval_h
NAMELIST /time_control/ auxhist11_interval_m
NAMELIST /time_control/ auxhist11_interval_s
NAMELIST /time_control/ auxhist11_interval
NAMELIST /time_control/ auxhist11_begin_y
NAMELIST /time_control/ auxhist11_begin_d
NAMELIST /time_control/ auxhist11_begin_h
NAMELIST /time_control/ auxhist11_begin_m
NAMELIST /time_control/ auxhist11_begin_s
NAMELIST /time_control/ auxhist11_begin
NAMELIST /time_control/ auxhist11_end_y
NAMELIST /time_control/ auxhist11_end_d
NAMELIST /time_control/ auxhist11_end_h
NAMELIST /time_control/ auxhist11_end_m
NAMELIST /time_control/ auxhist11_end_s
NAMELIST /time_control/ auxhist11_end
NAMELIST /time_control/ io_form_auxhist11
NAMELIST /time_control/ frames_per_auxhist11
NAMELIST /time_control/ auxhist12_inname
NAMELIST /time_control/ auxhist12_outname
NAMELIST /time_control/ auxhist12_interval_y
NAMELIST /time_control/ auxhist12_interval_d
NAMELIST /time_control/ auxhist12_interval_h
NAMELIST /time_control/ auxhist12_interval_m
NAMELIST /time_control/ auxhist12_interval_s
NAMELIST /time_control/ auxhist12_interval
NAMELIST /time_control/ auxhist12_begin_y
NAMELIST /time_control/ auxhist12_begin_d
NAMELIST /time_control/ auxhist12_begin_h
NAMELIST /time_control/ auxhist12_begin_m
NAMELIST /time_control/ auxhist12_begin_s
NAMELIST /time_control/ auxhist12_begin
NAMELIST /time_control/ auxhist12_end_y
NAMELIST /time_control/ auxhist12_end_d
NAMELIST /time_control/ auxhist12_end_h
NAMELIST /time_control/ auxhist12_end_m
NAMELIST /time_control/ auxhist12_end_s
NAMELIST /time_control/ auxhist12_end
NAMELIST /time_control/ io_form_auxhist12
NAMELIST /time_control/ frames_per_auxhist12
NAMELIST /time_control/ auxhist13_inname
NAMELIST /time_control/ auxhist13_outname
NAMELIST /time_control/ auxhist13_interval_y
NAMELIST /time_control/ auxhist13_interval_d
NAMELIST /time_control/ auxhist13_interval_h
NAMELIST /time_control/ auxhist13_interval_m
NAMELIST /time_control/ auxhist13_interval_s
NAMELIST /time_control/ auxhist13_interval
NAMELIST /time_control/ auxhist13_begin_y
NAMELIST /time_control/ auxhist13_begin_d
NAMELIST /time_control/ auxhist13_begin_h
NAMELIST /time_control/ auxhist13_begin_m
NAMELIST /time_control/ auxhist13_begin_s
NAMELIST /time_control/ auxhist13_begin
NAMELIST /time_control/ auxhist13_end_y
NAMELIST /time_control/ auxhist13_end_d
NAMELIST /time_control/ auxhist13_end_h
NAMELIST /time_control/ auxhist13_end_m
NAMELIST /time_control/ auxhist13_end_s
NAMELIST /time_control/ auxhist13_end
NAMELIST /time_control/ io_form_auxhist13
NAMELIST /time_control/ frames_per_auxhist13
NAMELIST /time_control/ auxhist14_inname
NAMELIST /time_control/ auxhist14_outname
NAMELIST /time_control/ auxhist14_interval_y
NAMELIST /time_control/ auxhist14_interval_d
NAMELIST /time_control/ auxhist14_interval_h
NAMELIST /time_control/ auxhist14_interval_m
NAMELIST /time_control/ auxhist14_interval_s
NAMELIST /time_control/ auxhist14_interval
NAMELIST /time_control/ auxhist14_begin_y
NAMELIST /time_control/ auxhist14_begin_d
NAMELIST /time_control/ auxhist14_begin_h
NAMELIST /time_control/ auxhist14_begin_m
NAMELIST /time_control/ auxhist14_begin_s
NAMELIST /time_control/ auxhist14_begin
NAMELIST /time_control/ auxhist14_end_y
NAMELIST /time_control/ auxhist14_end_d
NAMELIST /time_control/ auxhist14_end_h
NAMELIST /time_control/ auxhist14_end_m
NAMELIST /time_control/ auxhist14_end_s
NAMELIST /time_control/ auxhist14_end
NAMELIST /time_control/ io_form_auxhist14
NAMELIST /time_control/ frames_per_auxhist14
NAMELIST /time_control/ auxhist15_inname
NAMELIST /time_control/ auxhist15_outname
NAMELIST /time_control/ auxhist15_interval_y
NAMELIST /time_control/ auxhist15_interval_d
NAMELIST /time_control/ auxhist15_interval_h
NAMELIST /time_control/ auxhist15_interval_m
NAMELIST /time_control/ auxhist15_interval_s
NAMELIST /time_control/ auxhist15_interval
NAMELIST /time_control/ auxhist15_begin_y
NAMELIST /time_control/ auxhist15_begin_d
NAMELIST /time_control/ auxhist15_begin_h
NAMELIST /time_control/ auxhist15_begin_m
NAMELIST /time_control/ auxhist15_begin_s
NAMELIST /time_control/ auxhist15_begin
NAMELIST /time_control/ auxhist15_end_y
NAMELIST /time_control/ auxhist15_end_d
NAMELIST /time_control/ auxhist15_end_h
NAMELIST /time_control/ auxhist15_end_m
NAMELIST /time_control/ auxhist15_end_s
NAMELIST /time_control/ auxhist15_end
NAMELIST /time_control/ io_form_auxhist15
NAMELIST /time_control/ frames_per_auxhist15
NAMELIST /time_control/ auxhist16_inname
NAMELIST /time_control/ auxhist16_outname
NAMELIST /time_control/ auxhist16_interval_y
NAMELIST /time_control/ auxhist16_interval_d
NAMELIST /time_control/ auxhist16_interval_h
NAMELIST /time_control/ auxhist16_interval_m
NAMELIST /time_control/ auxhist16_interval_s
NAMELIST /time_control/ auxhist16_interval
NAMELIST /time_control/ auxhist16_begin_y
NAMELIST /time_control/ auxhist16_begin_d
NAMELIST /time_control/ auxhist16_begin_h
NAMELIST /time_control/ auxhist16_begin_m
NAMELIST /time_control/ auxhist16_begin_s
NAMELIST /time_control/ auxhist16_begin
NAMELIST /time_control/ auxhist16_end_y
NAMELIST /time_control/ auxhist16_end_d
NAMELIST /time_control/ auxhist16_end_h
NAMELIST /time_control/ auxhist16_end_m
NAMELIST /time_control/ auxhist16_end_s
NAMELIST /time_control/ auxhist16_end
NAMELIST /time_control/ io_form_auxhist16
NAMELIST /time_control/ frames_per_auxhist16
NAMELIST /time_control/ auxhist17_inname
NAMELIST /time_control/ auxhist17_outname
NAMELIST /time_control/ auxhist17_interval_y
NAMELIST /time_control/ auxhist17_interval_d
NAMELIST /time_control/ auxhist17_interval_h
NAMELIST /time_control/ auxhist17_interval_m
NAMELIST /time_control/ auxhist17_interval_s
NAMELIST /time_control/ auxhist17_interval
NAMELIST /time_control/ auxhist17_begin_y
NAMELIST /time_control/ auxhist17_begin_d
NAMELIST /time_control/ auxhist17_begin_h
NAMELIST /time_control/ auxhist17_begin_m
NAMELIST /time_control/ auxhist17_begin_s
NAMELIST /time_control/ auxhist17_begin
NAMELIST /time_control/ auxhist17_end_y
NAMELIST /time_control/ auxhist17_end_d
NAMELIST /time_control/ auxhist17_end_h
NAMELIST /time_control/ auxhist17_end_m
NAMELIST /time_control/ auxhist17_end_s
NAMELIST /time_control/ auxhist17_end
NAMELIST /time_control/ io_form_auxhist17
NAMELIST /time_control/ frames_per_auxhist17
NAMELIST /time_control/ auxhist18_inname
NAMELIST /time_control/ auxhist18_outname
NAMELIST /time_control/ auxhist18_interval_y
NAMELIST /time_control/ auxhist18_interval_d
NAMELIST /time_control/ auxhist18_interval_h
NAMELIST /time_control/ auxhist18_interval_m
NAMELIST /time_control/ auxhist18_interval_s
NAMELIST /time_control/ auxhist18_interval
NAMELIST /time_control/ auxhist18_begin_y
NAMELIST /time_control/ auxhist18_begin_d
NAMELIST /time_control/ auxhist18_begin_h
NAMELIST /time_control/ auxhist18_begin_m
NAMELIST /time_control/ auxhist18_begin_s
NAMELIST /time_control/ auxhist18_begin
NAMELIST /time_control/ auxhist18_end_y
NAMELIST /time_control/ auxhist18_end_d
NAMELIST /time_control/ auxhist18_end_h
NAMELIST /time_control/ auxhist18_end_m
NAMELIST /time_control/ auxhist18_end_s
NAMELIST /time_control/ auxhist18_end
NAMELIST /time_control/ io_form_auxhist18
NAMELIST /time_control/ frames_per_auxhist18
NAMELIST /time_control/ auxhist19_inname
NAMELIST /time_control/ auxhist19_outname
NAMELIST /time_control/ auxhist19_interval_y
NAMELIST /time_control/ auxhist19_interval_d
NAMELIST /time_control/ auxhist19_interval_h
NAMELIST /time_control/ auxhist19_interval_m
NAMELIST /time_control/ auxhist19_interval_s
NAMELIST /time_control/ auxhist19_interval
NAMELIST /time_control/ auxhist19_begin_y
NAMELIST /time_control/ auxhist19_begin_d
NAMELIST /time_control/ auxhist19_begin_h
NAMELIST /time_control/ auxhist19_begin_m
NAMELIST /time_control/ auxhist19_begin_s
NAMELIST /time_control/ auxhist19_begin
NAMELIST /time_control/ auxhist19_end_y
NAMELIST /time_control/ auxhist19_end_d
NAMELIST /time_control/ auxhist19_end_h
NAMELIST /time_control/ auxhist19_end_m
NAMELIST /time_control/ auxhist19_end_s
NAMELIST /time_control/ auxhist19_end
NAMELIST /time_control/ io_form_auxhist19
NAMELIST /time_control/ frames_per_auxhist19
NAMELIST /time_control/ auxhist20_inname
NAMELIST /time_control/ auxhist20_outname
NAMELIST /time_control/ auxhist20_interval_y
NAMELIST /time_control/ auxhist20_interval_d
NAMELIST /time_control/ auxhist20_interval_h
NAMELIST /time_control/ auxhist20_interval_m
NAMELIST /time_control/ auxhist20_interval_s
NAMELIST /time_control/ auxhist20_interval
NAMELIST /time_control/ auxhist20_begin_y
NAMELIST /time_control/ auxhist20_begin_d
NAMELIST /time_control/ auxhist20_begin_h
NAMELIST /time_control/ auxhist20_begin_m
NAMELIST /time_control/ auxhist20_begin_s
NAMELIST /time_control/ auxhist20_begin
NAMELIST /time_control/ auxhist20_end_y
NAMELIST /time_control/ auxhist20_end_d
NAMELIST /time_control/ auxhist20_end_h
NAMELIST /time_control/ auxhist20_end_m
NAMELIST /time_control/ auxhist20_end_s
NAMELIST /time_control/ auxhist20_end
NAMELIST /time_control/ io_form_auxhist20
NAMELIST /time_control/ frames_per_auxhist20
NAMELIST /time_control/ auxhist21_inname
NAMELIST /time_control/ auxhist21_outname
NAMELIST /time_control/ auxhist21_interval_y
NAMELIST /time_control/ auxhist21_interval_d
NAMELIST /time_control/ auxhist21_interval_h
NAMELIST /time_control/ auxhist21_interval_m
NAMELIST /time_control/ auxhist21_interval_s
NAMELIST /time_control/ auxhist21_interval
NAMELIST /time_control/ auxhist21_begin_y
NAMELIST /time_control/ auxhist21_begin_d
NAMELIST /time_control/ auxhist21_begin_h
NAMELIST /time_control/ auxhist21_begin_m
NAMELIST /time_control/ auxhist21_begin_s
NAMELIST /time_control/ auxhist21_begin
NAMELIST /time_control/ auxhist21_end_y
NAMELIST /time_control/ auxhist21_end_d
NAMELIST /time_control/ auxhist21_end_h
NAMELIST /time_control/ auxhist21_end_m
NAMELIST /time_control/ auxhist21_end_s
NAMELIST /time_control/ auxhist21_end
NAMELIST /time_control/ io_form_auxhist21
NAMELIST /time_control/ frames_per_auxhist21
NAMELIST /time_control/ auxhist22_inname
NAMELIST /time_control/ auxhist22_outname
NAMELIST /time_control/ auxhist22_interval_y
NAMELIST /time_control/ auxhist22_interval_d
NAMELIST /time_control/ auxhist22_interval_h
NAMELIST /time_control/ auxhist22_interval_m
NAMELIST /time_control/ auxhist22_interval_s
NAMELIST /time_control/ auxhist22_interval
NAMELIST /time_control/ auxhist22_begin_y
NAMELIST /time_control/ auxhist22_begin_d
NAMELIST /time_control/ auxhist22_begin_h
NAMELIST /time_control/ auxhist22_begin_m
NAMELIST /time_control/ auxhist22_begin_s
NAMELIST /time_control/ auxhist22_begin
NAMELIST /time_control/ auxhist22_end_y
NAMELIST /time_control/ auxhist22_end_d
NAMELIST /time_control/ auxhist22_end_h
NAMELIST /time_control/ auxhist22_end_m
NAMELIST /time_control/ auxhist22_end_s
NAMELIST /time_control/ auxhist22_end
NAMELIST /time_control/ io_form_auxhist22
NAMELIST /time_control/ frames_per_auxhist22
NAMELIST /time_control/ auxhist23_inname
NAMELIST /time_control/ auxhist23_outname
NAMELIST /time_control/ auxhist23_interval_y
NAMELIST /time_control/ auxhist23_interval_d
NAMELIST /time_control/ auxhist23_interval_h
NAMELIST /time_control/ auxhist23_interval_m
NAMELIST /time_control/ auxhist23_interval_s
NAMELIST /time_control/ auxhist23_interval
NAMELIST /time_control/ auxhist23_begin_y
NAMELIST /time_control/ auxhist23_begin_d
NAMELIST /time_control/ auxhist23_begin_h
NAMELIST /time_control/ auxhist23_begin_m
NAMELIST /time_control/ auxhist23_begin_s
NAMELIST /time_control/ auxhist23_begin
NAMELIST /time_control/ auxhist23_end_y
NAMELIST /time_control/ auxhist23_end_d
NAMELIST /time_control/ auxhist23_end_h
NAMELIST /time_control/ auxhist23_end_m
NAMELIST /time_control/ auxhist23_end_s
NAMELIST /time_control/ auxhist23_end
NAMELIST /time_control/ io_form_auxhist23
NAMELIST /time_control/ frames_per_auxhist23
NAMELIST /time_control/ auxhist24_inname
NAMELIST /time_control/ auxhist24_outname
NAMELIST /time_control/ auxhist24_interval_y
NAMELIST /time_control/ auxhist24_interval_d
NAMELIST /time_control/ auxhist24_interval_h
NAMELIST /time_control/ auxhist24_interval_m
NAMELIST /time_control/ auxhist24_interval_s
NAMELIST /time_control/ auxhist24_interval
NAMELIST /time_control/ auxhist24_begin_y
NAMELIST /time_control/ auxhist24_begin_d
NAMELIST /time_control/ auxhist24_begin_h
NAMELIST /time_control/ auxhist24_begin_m
NAMELIST /time_control/ auxhist24_begin_s
NAMELIST /time_control/ auxhist24_begin
NAMELIST /time_control/ auxhist24_end_y
NAMELIST /time_control/ auxhist24_end_d
NAMELIST /time_control/ auxhist24_end_h
NAMELIST /time_control/ auxhist24_end_m
NAMELIST /time_control/ auxhist24_end_s
NAMELIST /time_control/ auxhist24_end
NAMELIST /time_control/ io_form_auxhist24
NAMELIST /time_control/ frames_per_auxhist24
NAMELIST /time_control/ auxinput1_outname
NAMELIST /time_control/ auxinput1_interval_y
NAMELIST /time_control/ auxinput1_interval_d
NAMELIST /time_control/ auxinput1_interval_h
NAMELIST /time_control/ auxinput1_interval_m
NAMELIST /time_control/ auxinput1_interval_s
NAMELIST /time_control/ auxinput1_interval
NAMELIST /time_control/ auxinput1_begin_y
NAMELIST /time_control/ auxinput1_begin_d
NAMELIST /time_control/ auxinput1_begin_h
NAMELIST /time_control/ auxinput1_begin_m
NAMELIST /time_control/ auxinput1_begin_s
NAMELIST /time_control/ auxinput1_begin
NAMELIST /time_control/ auxinput1_end_y
NAMELIST /time_control/ auxinput1_end_d
NAMELIST /time_control/ auxinput1_end_h
NAMELIST /time_control/ auxinput1_end_m
NAMELIST /time_control/ auxinput1_end_s
NAMELIST /time_control/ auxinput1_end
NAMELIST /time_control/ frames_per_auxinput1
NAMELIST /time_control/ auxinput2_inname
NAMELIST /time_control/ auxinput2_outname
NAMELIST /time_control/ auxinput2_interval_y
NAMELIST /time_control/ auxinput2_interval_d
NAMELIST /time_control/ auxinput2_interval_h
NAMELIST /time_control/ auxinput2_interval_m
NAMELIST /time_control/ auxinput2_interval_s
NAMELIST /time_control/ auxinput2_interval
NAMELIST /time_control/ auxinput2_begin_y
NAMELIST /time_control/ auxinput2_begin_d
NAMELIST /time_control/ auxinput2_begin_h
NAMELIST /time_control/ auxinput2_begin_m
NAMELIST /time_control/ auxinput2_begin_s
NAMELIST /time_control/ auxinput2_begin
NAMELIST /time_control/ auxinput2_end_y
NAMELIST /time_control/ auxinput2_end_d
NAMELIST /time_control/ auxinput2_end_h
NAMELIST /time_control/ auxinput2_end_m
NAMELIST /time_control/ auxinput2_end_s
NAMELIST /time_control/ auxinput2_end
NAMELIST /time_control/ frames_per_auxinput2
NAMELIST /time_control/ auxinput3_inname
NAMELIST /time_control/ auxinput3_outname
NAMELIST /time_control/ auxinput3_interval_y
NAMELIST /time_control/ auxinput3_interval_d
NAMELIST /time_control/ auxinput3_interval_h
NAMELIST /time_control/ auxinput3_interval_m
NAMELIST /time_control/ auxinput3_interval_s
NAMELIST /time_control/ auxinput3_interval
NAMELIST /time_control/ auxinput3_begin_y
NAMELIST /time_control/ auxinput3_begin_d
NAMELIST /time_control/ auxinput3_begin_h
NAMELIST /time_control/ auxinput3_begin_m
NAMELIST /time_control/ auxinput3_begin_s
NAMELIST /time_control/ auxinput3_begin
NAMELIST /time_control/ auxinput3_end_y
NAMELIST /time_control/ auxinput3_end_d
NAMELIST /time_control/ auxinput3_end_h
NAMELIST /time_control/ auxinput3_end_m
NAMELIST /time_control/ auxinput3_end_s
NAMELIST /time_control/ auxinput3_end
NAMELIST /time_control/ io_form_auxinput3
NAMELIST /time_control/ frames_per_auxinput3
NAMELIST /time_control/ auxinput4_inname
NAMELIST /time_control/ auxinput4_outname
NAMELIST /time_control/ auxinput4_interval_y
NAMELIST /time_control/ auxinput4_interval_d
NAMELIST /time_control/ auxinput4_interval_h
NAMELIST /time_control/ auxinput4_interval_m
NAMELIST /time_control/ auxinput4_interval_s
NAMELIST /time_control/ auxinput4_interval
NAMELIST /time_control/ auxinput4_begin_y
NAMELIST /time_control/ auxinput4_begin_d
NAMELIST /time_control/ auxinput4_begin_h
NAMELIST /time_control/ auxinput4_begin_m
NAMELIST /time_control/ auxinput4_begin_s
NAMELIST /time_control/ auxinput4_begin
NAMELIST /time_control/ auxinput4_end_y
NAMELIST /time_control/ auxinput4_end_d
NAMELIST /time_control/ auxinput4_end_h
NAMELIST /time_control/ auxinput4_end_m
NAMELIST /time_control/ auxinput4_end_s
NAMELIST /time_control/ auxinput4_end
NAMELIST /time_control/ io_form_auxinput4
NAMELIST /time_control/ frames_per_auxinput4
NAMELIST /time_control/ auxinput5_inname
NAMELIST /time_control/ auxinput5_outname
NAMELIST /time_control/ auxinput5_interval_y
NAMELIST /time_control/ auxinput5_interval_d
NAMELIST /time_control/ auxinput5_interval_h
NAMELIST /time_control/ auxinput5_interval_m
NAMELIST /time_control/ auxinput5_interval_s
NAMELIST /time_control/ auxinput5_interval
NAMELIST /time_control/ auxinput5_begin_y
NAMELIST /time_control/ auxinput5_begin_d
NAMELIST /time_control/ auxinput5_begin_h
NAMELIST /time_control/ auxinput5_begin_m
NAMELIST /time_control/ auxinput5_begin_s
NAMELIST /time_control/ auxinput5_begin
NAMELIST /time_control/ auxinput5_end_y
NAMELIST /time_control/ auxinput5_end_d
NAMELIST /time_control/ auxinput5_end_h
NAMELIST /time_control/ auxinput5_end_m
NAMELIST /time_control/ auxinput5_end_s
NAMELIST /time_control/ auxinput5_end
NAMELIST /time_control/ io_form_auxinput5
NAMELIST /time_control/ frames_per_auxinput5
NAMELIST /time_control/ auxinput6_inname
NAMELIST /time_control/ auxinput6_outname
NAMELIST /time_control/ auxinput6_interval_y
NAMELIST /time_control/ auxinput6_interval_d
NAMELIST /time_control/ auxinput6_interval_h
NAMELIST /time_control/ auxinput6_interval_m
NAMELIST /time_control/ auxinput6_interval_s
NAMELIST /time_control/ auxinput6_interval
NAMELIST /time_control/ auxinput6_begin_y
NAMELIST /time_control/ auxinput6_begin_d
NAMELIST /time_control/ auxinput6_begin_h
NAMELIST /time_control/ auxinput6_begin_m
NAMELIST /time_control/ auxinput6_begin_s
NAMELIST /time_control/ auxinput6_begin
NAMELIST /time_control/ auxinput6_end_y
NAMELIST /time_control/ auxinput6_end_d
NAMELIST /time_control/ auxinput6_end_h
NAMELIST /time_control/ auxinput6_end_m
NAMELIST /time_control/ auxinput6_end_s
NAMELIST /time_control/ auxinput6_end
NAMELIST /time_control/ io_form_auxinput6
NAMELIST /time_control/ frames_per_auxinput6
NAMELIST /time_control/ auxinput7_inname
NAMELIST /time_control/ auxinput7_outname
NAMELIST /time_control/ auxinput7_interval_y
NAMELIST /time_control/ auxinput7_interval_d
NAMELIST /time_control/ auxinput7_interval_h
NAMELIST /time_control/ auxinput7_interval_m
NAMELIST /time_control/ auxinput7_interval_s
NAMELIST /time_control/ auxinput7_interval
NAMELIST /time_control/ auxinput7_begin_y
NAMELIST /time_control/ auxinput7_begin_d
NAMELIST /time_control/ auxinput7_begin_h
NAMELIST /time_control/ auxinput7_begin_m
NAMELIST /time_control/ auxinput7_begin_s
NAMELIST /time_control/ auxinput7_begin
NAMELIST /time_control/ auxinput7_end_y
NAMELIST /time_control/ auxinput7_end_d
NAMELIST /time_control/ auxinput7_end_h
NAMELIST /time_control/ auxinput7_end_m
NAMELIST /time_control/ auxinput7_end_s
NAMELIST /time_control/ auxinput7_end
NAMELIST /time_control/ io_form_auxinput7
NAMELIST /time_control/ frames_per_auxinput7
NAMELIST /time_control/ auxinput8_inname
NAMELIST /time_control/ auxinput8_outname
NAMELIST /time_control/ auxinput8_interval_y
NAMELIST /time_control/ auxinput8_interval_d
NAMELIST /time_control/ auxinput8_interval_h
NAMELIST /time_control/ auxinput8_interval_m
NAMELIST /time_control/ auxinput8_interval_s
NAMELIST /time_control/ auxinput8_interval
NAMELIST /time_control/ auxinput8_begin_y
NAMELIST /time_control/ auxinput8_begin_d
NAMELIST /time_control/ auxinput8_begin_h
NAMELIST /time_control/ auxinput8_begin_m
NAMELIST /time_control/ auxinput8_begin_s
NAMELIST /time_control/ auxinput8_begin
NAMELIST /time_control/ auxinput8_end_y
NAMELIST /time_control/ auxinput8_end_d
NAMELIST /time_control/ auxinput8_end_h
NAMELIST /time_control/ auxinput8_end_m
NAMELIST /time_control/ auxinput8_end_s
NAMELIST /time_control/ auxinput8_end
NAMELIST /time_control/ io_form_auxinput8
NAMELIST /time_control/ frames_per_auxinput8
NAMELIST /time_control/ auxinput9_inname
NAMELIST /time_control/ auxinput9_outname
NAMELIST /time_control/ auxinput9_interval_y
NAMELIST /time_control/ auxinput9_interval_d
NAMELIST /time_control/ auxinput9_interval_h
NAMELIST /time_control/ auxinput9_interval_m
NAMELIST /time_control/ auxinput9_interval_s
NAMELIST /time_control/ auxinput9_interval
NAMELIST /time_control/ auxinput9_begin_y
NAMELIST /time_control/ auxinput9_begin_d
NAMELIST /time_control/ auxinput9_begin_h
NAMELIST /time_control/ auxinput9_begin_m
NAMELIST /time_control/ auxinput9_begin_s
NAMELIST /time_control/ auxinput9_begin
NAMELIST /time_control/ auxinput9_end_y
NAMELIST /time_control/ auxinput9_end_d
NAMELIST /time_control/ auxinput9_end_h
NAMELIST /time_control/ auxinput9_end_m
NAMELIST /time_control/ auxinput9_end_s
NAMELIST /time_control/ auxinput9_end
NAMELIST /time_control/ io_form_auxinput9
NAMELIST /time_control/ frames_per_auxinput9
NAMELIST /time_control/ auxinput10_inname
NAMELIST /time_control/ auxinput10_outname
NAMELIST /time_control/ auxinput10_interval_y
NAMELIST /time_control/ auxinput10_interval_d
NAMELIST /time_control/ auxinput10_interval_h
NAMELIST /time_control/ auxinput10_interval_m
NAMELIST /time_control/ auxinput10_interval_s
NAMELIST /time_control/ auxinput10_interval
NAMELIST /time_control/ auxinput10_begin_y
NAMELIST /time_control/ auxinput10_begin_d
NAMELIST /time_control/ auxinput10_begin_h
NAMELIST /time_control/ auxinput10_begin_m
NAMELIST /time_control/ auxinput10_begin_s
NAMELIST /time_control/ auxinput10_begin
NAMELIST /time_control/ auxinput10_end_y
NAMELIST /time_control/ auxinput10_end_d
NAMELIST /time_control/ auxinput10_end_h
NAMELIST /time_control/ auxinput10_end_m
NAMELIST /time_control/ auxinput10_end_s
NAMELIST /time_control/ auxinput10_end
NAMELIST /time_control/ io_form_auxinput10
NAMELIST /time_control/ frames_per_auxinput10
NAMELIST /time_control/ auxinput11_inname
NAMELIST /time_control/ auxinput11_outname
NAMELIST /time_control/ auxinput11_interval_y
NAMELIST /time_control/ auxinput11_interval_d
NAMELIST /time_control/ auxinput11_interval_h
NAMELIST /time_control/ auxinput11_interval_m
NAMELIST /time_control/ auxinput11_interval_s
NAMELIST /time_control/ auxinput11_interval
NAMELIST /time_control/ auxinput11_begin_y
NAMELIST /time_control/ auxinput11_begin_d
NAMELIST /time_control/ auxinput11_begin_h
NAMELIST /time_control/ auxinput11_begin_m
NAMELIST /time_control/ auxinput11_begin_s
NAMELIST /time_control/ auxinput11_begin
NAMELIST /time_control/ auxinput11_end_y
NAMELIST /time_control/ auxinput11_end_d
NAMELIST /time_control/ auxinput11_end_h
NAMELIST /time_control/ auxinput11_end_m
NAMELIST /time_control/ auxinput11_end_s
NAMELIST /time_control/ auxinput11_end
NAMELIST /time_control/ io_form_auxinput11
NAMELIST /time_control/ frames_per_auxinput11
NAMELIST /time_control/ auxinput12_inname
NAMELIST /time_control/ auxinput12_outname
NAMELIST /time_control/ auxinput12_interval_y
NAMELIST /time_control/ auxinput12_interval_d
NAMELIST /time_control/ auxinput12_interval_h
NAMELIST /time_control/ auxinput12_interval_m
NAMELIST /time_control/ auxinput12_interval_s
NAMELIST /time_control/ auxinput12_interval
NAMELIST /time_control/ auxinput12_begin_y
NAMELIST /time_control/ auxinput12_begin_d
NAMELIST /time_control/ auxinput12_begin_h
NAMELIST /time_control/ auxinput12_begin_m
NAMELIST /time_control/ auxinput12_begin_s
NAMELIST /time_control/ auxinput12_begin
NAMELIST /time_control/ auxinput12_end_y
NAMELIST /time_control/ auxinput12_end_d
NAMELIST /time_control/ auxinput12_end_h
NAMELIST /time_control/ auxinput12_end_m
NAMELIST /time_control/ auxinput12_end_s
NAMELIST /time_control/ auxinput12_end
NAMELIST /time_control/ io_form_auxinput12
NAMELIST /time_control/ frames_per_auxinput12
NAMELIST /time_control/ auxinput13_inname
NAMELIST /time_control/ auxinput13_outname
NAMELIST /time_control/ auxinput13_interval_y
NAMELIST /time_control/ auxinput13_interval_d
NAMELIST /time_control/ auxinput13_interval_h
NAMELIST /time_control/ auxinput13_interval_m
NAMELIST /time_control/ auxinput13_interval_s
NAMELIST /time_control/ auxinput13_interval
NAMELIST /time_control/ auxinput13_begin_y
NAMELIST /time_control/ auxinput13_begin_d
NAMELIST /time_control/ auxinput13_begin_h
NAMELIST /time_control/ auxinput13_begin_m
NAMELIST /time_control/ auxinput13_begin_s
NAMELIST /time_control/ auxinput13_begin
NAMELIST /time_control/ auxinput13_end_y
NAMELIST /time_control/ auxinput13_end_d
NAMELIST /time_control/ auxinput13_end_h
NAMELIST /time_control/ auxinput13_end_m
NAMELIST /time_control/ auxinput13_end_s
NAMELIST /time_control/ auxinput13_end
NAMELIST /time_control/ io_form_auxinput13
NAMELIST /time_control/ frames_per_auxinput13
NAMELIST /time_control/ auxinput14_inname
NAMELIST /time_control/ auxinput14_outname
NAMELIST /time_control/ auxinput14_interval_y
NAMELIST /time_control/ auxinput14_interval_d
NAMELIST /time_control/ auxinput14_interval_h
NAMELIST /time_control/ auxinput14_interval_m
NAMELIST /time_control/ auxinput14_interval_s
NAMELIST /time_control/ auxinput14_interval
NAMELIST /time_control/ auxinput14_begin_y
NAMELIST /time_control/ auxinput14_begin_d
NAMELIST /time_control/ auxinput14_begin_h
NAMELIST /time_control/ auxinput14_begin_m
NAMELIST /time_control/ auxinput14_begin_s
NAMELIST /time_control/ auxinput14_begin
NAMELIST /time_control/ auxinput14_end_y
NAMELIST /time_control/ auxinput14_end_d
NAMELIST /time_control/ auxinput14_end_h
NAMELIST /time_control/ auxinput14_end_m
NAMELIST /time_control/ auxinput14_end_s
NAMELIST /time_control/ auxinput14_end
NAMELIST /time_control/ io_form_auxinput14
NAMELIST /time_control/ frames_per_auxinput14
NAMELIST /time_control/ auxinput15_inname
NAMELIST /time_control/ auxinput15_outname
NAMELIST /time_control/ auxinput15_interval_y
NAMELIST /time_control/ auxinput15_interval_d
NAMELIST /time_control/ auxinput15_interval_h
NAMELIST /time_control/ auxinput15_interval_m
NAMELIST /time_control/ auxinput15_interval_s
NAMELIST /time_control/ auxinput15_interval
NAMELIST /time_control/ auxinput15_begin_y
NAMELIST /time_control/ auxinput15_begin_d
NAMELIST /time_control/ auxinput15_begin_h
NAMELIST /time_control/ auxinput15_begin_m
NAMELIST /time_control/ auxinput15_begin_s
NAMELIST /time_control/ auxinput15_begin
NAMELIST /time_control/ auxinput15_end_y
NAMELIST /time_control/ auxinput15_end_d
NAMELIST /time_control/ auxinput15_end_h
NAMELIST /time_control/ auxinput15_end_m
NAMELIST /time_control/ auxinput15_end_s
NAMELIST /time_control/ auxinput15_end
NAMELIST /time_control/ io_form_auxinput15
NAMELIST /time_control/ frames_per_auxinput15
NAMELIST /time_control/ auxinput16_inname
NAMELIST /time_control/ auxinput16_outname
NAMELIST /time_control/ auxinput16_interval_y
NAMELIST /time_control/ auxinput16_interval_d
NAMELIST /time_control/ auxinput16_interval_h
NAMELIST /time_control/ auxinput16_interval_m
NAMELIST /time_control/ auxinput16_interval_s
NAMELIST /time_control/ auxinput16_interval
NAMELIST /time_control/ auxinput16_begin_y
NAMELIST /time_control/ auxinput16_begin_d
NAMELIST /time_control/ auxinput16_begin_h
NAMELIST /time_control/ auxinput16_begin_m
NAMELIST /time_control/ auxinput16_begin_s
NAMELIST /time_control/ auxinput16_begin
NAMELIST /time_control/ auxinput16_end_y
NAMELIST /time_control/ auxinput16_end_d
NAMELIST /time_control/ auxinput16_end_h
NAMELIST /time_control/ auxinput16_end_m
NAMELIST /time_control/ auxinput16_end_s
NAMELIST /time_control/ auxinput16_end
NAMELIST /time_control/ io_form_auxinput16
NAMELIST /time_control/ frames_per_auxinput16
NAMELIST /time_control/ auxinput17_inname
NAMELIST /time_control/ auxinput17_outname
NAMELIST /time_control/ auxinput17_interval_y
NAMELIST /time_control/ auxinput17_interval_d
NAMELIST /time_control/ auxinput17_interval_h
NAMELIST /time_control/ auxinput17_interval_m
NAMELIST /time_control/ auxinput17_interval_s
NAMELIST /time_control/ auxinput17_interval
NAMELIST /time_control/ auxinput17_begin_y
NAMELIST /time_control/ auxinput17_begin_d
NAMELIST /time_control/ auxinput17_begin_h
NAMELIST /time_control/ auxinput17_begin_m
NAMELIST /time_control/ auxinput17_begin_s
NAMELIST /time_control/ auxinput17_begin
NAMELIST /time_control/ auxinput17_end_y
NAMELIST /time_control/ auxinput17_end_d
NAMELIST /time_control/ auxinput17_end_h
NAMELIST /time_control/ auxinput17_end_m
NAMELIST /time_control/ auxinput17_end_s
NAMELIST /time_control/ auxinput17_end
NAMELIST /time_control/ io_form_auxinput17
NAMELIST /time_control/ frames_per_auxinput17
NAMELIST /time_control/ auxinput18_inname
NAMELIST /time_control/ auxinput18_outname
NAMELIST /time_control/ auxinput18_interval_y
NAMELIST /time_control/ auxinput18_interval_d
NAMELIST /time_control/ auxinput18_interval_h
NAMELIST /time_control/ auxinput18_interval_m
NAMELIST /time_control/ auxinput18_interval_s
NAMELIST /time_control/ auxinput18_interval
NAMELIST /time_control/ auxinput18_begin_y
NAMELIST /time_control/ auxinput18_begin_d
NAMELIST /time_control/ auxinput18_begin_h
NAMELIST /time_control/ auxinput18_begin_m
NAMELIST /time_control/ auxinput18_begin_s
NAMELIST /time_control/ auxinput18_begin
NAMELIST /time_control/ auxinput18_end_y
NAMELIST /time_control/ auxinput18_end_d
NAMELIST /time_control/ auxinput18_end_h
NAMELIST /time_control/ auxinput18_end_m
NAMELIST /time_control/ auxinput18_end_s
NAMELIST /time_control/ auxinput18_end
NAMELIST /time_control/ io_form_auxinput18
NAMELIST /time_control/ frames_per_auxinput18
NAMELIST /time_control/ auxinput19_inname
NAMELIST /time_control/ auxinput19_outname
NAMELIST /time_control/ auxinput19_interval_y
NAMELIST /time_control/ auxinput19_interval_d
NAMELIST /time_control/ auxinput19_interval_h
NAMELIST /time_control/ auxinput19_interval_m
NAMELIST /time_control/ auxinput19_interval_s
NAMELIST /time_control/ auxinput19_interval
NAMELIST /time_control/ auxinput19_begin_y
NAMELIST /time_control/ auxinput19_begin_d
NAMELIST /time_control/ auxinput19_begin_h
NAMELIST /time_control/ auxinput19_begin_m
NAMELIST /time_control/ auxinput19_begin_s
NAMELIST /time_control/ auxinput19_begin
NAMELIST /time_control/ auxinput19_end_y
NAMELIST /time_control/ auxinput19_end_d
NAMELIST /time_control/ auxinput19_end_h
NAMELIST /time_control/ auxinput19_end_m
NAMELIST /time_control/ auxinput19_end_s
NAMELIST /time_control/ auxinput19_end
NAMELIST /time_control/ io_form_auxinput19
NAMELIST /time_control/ frames_per_auxinput19
NAMELIST /time_control/ auxinput20_inname
NAMELIST /time_control/ auxinput20_outname
NAMELIST /time_control/ auxinput20_interval_y
NAMELIST /time_control/ auxinput20_interval_d
NAMELIST /time_control/ auxinput20_interval_h
NAMELIST /time_control/ auxinput20_interval_m
NAMELIST /time_control/ auxinput20_interval_s
NAMELIST /time_control/ auxinput20_interval
NAMELIST /time_control/ auxinput20_begin_y
NAMELIST /time_control/ auxinput20_begin_d
NAMELIST /time_control/ auxinput20_begin_h
NAMELIST /time_control/ auxinput20_begin_m
NAMELIST /time_control/ auxinput20_begin_s
NAMELIST /time_control/ auxinput20_begin
NAMELIST /time_control/ auxinput20_end_y
NAMELIST /time_control/ auxinput20_end_d
NAMELIST /time_control/ auxinput20_end_h
NAMELIST /time_control/ auxinput20_end_m
NAMELIST /time_control/ auxinput20_end_s
NAMELIST /time_control/ auxinput20_end
NAMELIST /time_control/ io_form_auxinput20
NAMELIST /time_control/ frames_per_auxinput20
NAMELIST /time_control/ auxinput21_inname
NAMELIST /time_control/ auxinput21_outname
NAMELIST /time_control/ auxinput21_interval_y
NAMELIST /time_control/ auxinput21_interval_d
NAMELIST /time_control/ auxinput21_interval_h
NAMELIST /time_control/ auxinput21_interval_m
NAMELIST /time_control/ auxinput21_interval_s
NAMELIST /time_control/ auxinput21_interval
NAMELIST /time_control/ auxinput21_begin_y
NAMELIST /time_control/ auxinput21_begin_d
NAMELIST /time_control/ auxinput21_begin_h
NAMELIST /time_control/ auxinput21_begin_m
NAMELIST /time_control/ auxinput21_begin_s
NAMELIST /time_control/ auxinput21_begin
NAMELIST /time_control/ auxinput21_end_y
NAMELIST /time_control/ auxinput21_end_d
NAMELIST /time_control/ auxinput21_end_h
NAMELIST /time_control/ auxinput21_end_m
NAMELIST /time_control/ auxinput21_end_s
NAMELIST /time_control/ auxinput21_end
NAMELIST /time_control/ io_form_auxinput21
NAMELIST /time_control/ frames_per_auxinput21
NAMELIST /time_control/ auxinput22_inname
NAMELIST /time_control/ auxinput22_outname
NAMELIST /time_control/ auxinput22_interval_y
NAMELIST /time_control/ auxinput22_interval_d
NAMELIST /time_control/ auxinput22_interval_h
NAMELIST /time_control/ auxinput22_interval_m
NAMELIST /time_control/ auxinput22_interval_s
NAMELIST /time_control/ auxinput22_interval
NAMELIST /time_control/ auxinput22_begin_y
NAMELIST /time_control/ auxinput22_begin_d
NAMELIST /time_control/ auxinput22_begin_h
NAMELIST /time_control/ auxinput22_begin_m
NAMELIST /time_control/ auxinput22_begin_s
NAMELIST /time_control/ auxinput22_begin
NAMELIST /time_control/ auxinput22_end_y
NAMELIST /time_control/ auxinput22_end_d
NAMELIST /time_control/ auxinput22_end_h
NAMELIST /time_control/ auxinput22_end_m
NAMELIST /time_control/ auxinput22_end_s
NAMELIST /time_control/ auxinput22_end
NAMELIST /time_control/ io_form_auxinput22
NAMELIST /time_control/ frames_per_auxinput22
NAMELIST /time_control/ auxinput23_inname
NAMELIST /time_control/ auxinput23_outname
NAMELIST /time_control/ auxinput23_interval_y
NAMELIST /time_control/ auxinput23_interval_d
NAMELIST /time_control/ auxinput23_interval_h
NAMELIST /time_control/ auxinput23_interval_m
NAMELIST /time_control/ auxinput23_interval_s
NAMELIST /time_control/ auxinput23_interval
NAMELIST /time_control/ auxinput23_begin_y
NAMELIST /time_control/ auxinput23_begin_d
NAMELIST /time_control/ auxinput23_begin_h
NAMELIST /time_control/ auxinput23_begin_m
NAMELIST /time_control/ auxinput23_begin_s
NAMELIST /time_control/ auxinput23_begin
NAMELIST /time_control/ auxinput23_end_y
NAMELIST /time_control/ auxinput23_end_d
NAMELIST /time_control/ auxinput23_end_h
NAMELIST /time_control/ auxinput23_end_m
NAMELIST /time_control/ auxinput23_end_s
NAMELIST /time_control/ auxinput23_end
NAMELIST /time_control/ io_form_auxinput23
NAMELIST /time_control/ frames_per_auxinput23
NAMELIST /time_control/ auxinput24_inname
NAMELIST /time_control/ auxinput24_outname
NAMELIST /time_control/ auxinput24_interval_y
NAMELIST /time_control/ auxinput24_interval_d
NAMELIST /time_control/ auxinput24_interval_h
NAMELIST /time_control/ auxinput24_interval_m
NAMELIST /time_control/ auxinput24_interval_s
NAMELIST /time_control/ auxinput24_interval
NAMELIST /time_control/ auxinput24_begin_y
NAMELIST /time_control/ auxinput24_begin_d
NAMELIST /time_control/ auxinput24_begin_h
NAMELIST /time_control/ auxinput24_begin_m
NAMELIST /time_control/ auxinput24_begin_s
NAMELIST /time_control/ auxinput24_begin
NAMELIST /time_control/ auxinput24_end_y
NAMELIST /time_control/ auxinput24_end_d
NAMELIST /time_control/ auxinput24_end_h
NAMELIST /time_control/ auxinput24_end_m
NAMELIST /time_control/ auxinput24_end_s
NAMELIST /time_control/ auxinput24_end
NAMELIST /time_control/ io_form_auxinput24
NAMELIST /time_control/ frames_per_auxinput24
NAMELIST /time_control/ history_interval
NAMELIST /time_control/ frames_per_outfile
NAMELIST /time_control/ restart
NAMELIST /time_control/ restart_interval
NAMELIST /time_control/ io_form_input
NAMELIST /time_control/ io_form_history
NAMELIST /time_control/ io_form_restart
NAMELIST /time_control/ io_form_boundary
NAMELIST /time_control/ debug_level
NAMELIST /time_control/ self_test_domain
NAMELIST /time_control/ history_outname
NAMELIST /time_control/ history_inname
NAMELIST /time_control/ use_netcdf_classic
NAMELIST /time_control/ history_interval_d
NAMELIST /time_control/ history_interval_h
NAMELIST /time_control/ history_interval_m
NAMELIST /time_control/ history_interval_s
NAMELIST /time_control/ inputout_interval_d
NAMELIST /time_control/ inputout_interval_h
NAMELIST /time_control/ inputout_interval_m
NAMELIST /time_control/ inputout_interval_s
NAMELIST /time_control/ inputout_interval
NAMELIST /time_control/ restart_interval_d
NAMELIST /time_control/ restart_interval_h
NAMELIST /time_control/ restart_interval_m
NAMELIST /time_control/ restart_interval_s
NAMELIST /time_control/ history_begin_y
NAMELIST /time_control/ history_begin_d
NAMELIST /time_control/ history_begin_h
NAMELIST /time_control/ history_begin_m
NAMELIST /time_control/ history_begin_s
NAMELIST /time_control/ history_begin
NAMELIST /time_control/ inputout_begin_y
NAMELIST /time_control/ inputout_begin_d
NAMELIST /time_control/ inputout_begin_h
NAMELIST /time_control/ inputout_begin_m
NAMELIST /time_control/ inputout_begin_s
NAMELIST /time_control/ restart_begin_y
NAMELIST /time_control/ restart_begin_d
NAMELIST /time_control/ restart_begin_h
NAMELIST /time_control/ restart_begin_m
NAMELIST /time_control/ restart_begin_s
NAMELIST /time_control/ restart_begin
NAMELIST /time_control/ history_end_y
NAMELIST /time_control/ history_end_d
NAMELIST /time_control/ history_end_h
NAMELIST /time_control/ history_end_m
NAMELIST /time_control/ history_end_s
NAMELIST /time_control/ history_end
NAMELIST /time_control/ inputout_end_y
NAMELIST /time_control/ inputout_end_d
NAMELIST /time_control/ inputout_end_h
NAMELIST /time_control/ inputout_end_m
NAMELIST /time_control/ inputout_end_s
NAMELIST /time_control/ reset_simulation_start
NAMELIST /domains/ sr_x
NAMELIST /domains/ sr_y
NAMELIST /time_control/ iofields_filename
NAMELIST /time_control/ ignore_iofields_warning
NAMELIST /time_control/ ncd_nofill
NAMELIST /time_control/ julyr
NAMELIST /time_control/ julday
NAMELIST /time_control/ gmt
NAMELIST /time_control/ high_freq_outname
NAMELIST /time_control/ partial_atcf_outname
NAMELIST /time_control/ input_inname
NAMELIST /time_control/ input_outname
NAMELIST /time_control/ bdy_inname
NAMELIST /time_control/ bdy_outname
NAMELIST /time_control/ rst_inname
NAMELIST /time_control/ rst_outname
NAMELIST /time_control/ anl_outname
NAMELIST /time_control/ write_input
NAMELIST /time_control/ write_restart_at_0h
NAMELIST /time_control/ write_hist_at_0h_rst
NAMELIST /time_control/ adjust_output_times
NAMELIST /time_control/ adjust_input_times
NAMELIST /time_control/ tstart
NAMELIST /time_control/ nocolons
NAMELIST /time_control/ cycling
NAMELIST /time_control/ output_ready_flag
NAMELIST /dfi_control/ dfi_opt
NAMELIST /dfi_control/ dfi_savehydmeteors
NAMELIST /dfi_control/ dfi_nfilter
NAMELIST /dfi_control/ dfi_write_filtered_input
NAMELIST /dfi_control/ dfi_write_dfi_history
NAMELIST /dfi_control/ dfi_cutoff_seconds
NAMELIST /dfi_control/ dfi_time_dim
NAMELIST /dfi_control/ dfi_fwdstop_year
NAMELIST /dfi_control/ dfi_fwdstop_month
NAMELIST /dfi_control/ dfi_fwdstop_day
NAMELIST /dfi_control/ dfi_fwdstop_hour
NAMELIST /dfi_control/ dfi_fwdstop_minute
NAMELIST /dfi_control/ dfi_fwdstop_second
NAMELIST /dfi_control/ dfi_bckstop_year
NAMELIST /dfi_control/ dfi_bckstop_month
NAMELIST /dfi_control/ dfi_bckstop_day
NAMELIST /dfi_control/ dfi_bckstop_hour
NAMELIST /dfi_control/ dfi_bckstop_minute
NAMELIST /dfi_control/ dfi_bckstop_second
NAMELIST /domains/ time_step
NAMELIST /domains/ time_step_fract_num
NAMELIST /domains/ time_step_fract_den
NAMELIST /domains/ time_step_dfi
NAMELIST /domains/ max_dom
NAMELIST /domains/ s_we
NAMELIST /domains/ e_we
NAMELIST /domains/ s_sn
NAMELIST /domains/ e_sn
NAMELIST /domains/ s_vert
NAMELIST /domains/ e_vert
NAMELIST /domains/ num_metgrid_soil_levels
NAMELIST /domains/ dx
NAMELIST /domains/ dy
NAMELIST /domains/ grid_id
NAMELIST /domains/ grid_allowed
NAMELIST /domains/ parent_id
NAMELIST /domains/ i_parent_start
NAMELIST /domains/ j_parent_start
NAMELIST /domains/ parent_grid_ratio
NAMELIST /domains/ parent_time_step_ratio
NAMELIST /domains/ feedback
NAMELIST /domains/ smooth_option
NAMELIST /domains/ ztop
NAMELIST /domains/ moad_grid_ratio
NAMELIST /domains/ moad_time_step_ratio
NAMELIST /domains/ shw
NAMELIST /domains/ tile_sz_x
NAMELIST /domains/ tile_sz_y
NAMELIST /domains/ numtiles
NAMELIST /domains/ numtiles_inc
NAMELIST /domains/ numtiles_x
NAMELIST /domains/ numtiles_y
NAMELIST /domains/ tile_strategy
NAMELIST /domains/ nproc_x
NAMELIST /domains/ nproc_y
NAMELIST /domains/ irand
NAMELIST /domains/ ts_buf_size
NAMELIST /domains/ max_ts_locs
NAMELIST /domains/ num_moves
NAMELIST /domains/ vortex_interval
NAMELIST /domains/ corral_dist
NAMELIST /domains/ move_id
NAMELIST /domains/ move_interval
NAMELIST /domains/ move_cd_x
NAMELIST /domains/ move_cd_y
NAMELIST /domains/ swap_x
NAMELIST /domains/ swap_y
NAMELIST /domains/ cycle_x
NAMELIST /domains/ cycle_y
NAMELIST /domains/ reorder_mesh
NAMELIST /domains/ perturb_input
NAMELIST /domains/ eta_levels
NAMELIST /domains/ ptsgm
NAMELIST /domains/ num_metgrid_levels
NAMELIST /domains/ p_top_requested
NAMELIST /domains/ use_prep_hybrid
NAMELIST /physics/ force_read_thompson
NAMELIST /physics/ write_thompson_tables
NAMELIST /physics/ mp_physics
NAMELIST /physics/ mommix
NAMELIST /physics/ disheat
NAMELIST /physics/ do_radar_ref
NAMELIST /physics/ ra_lw_physics
NAMELIST /physics/ ra_sw_physics
NAMELIST /physics/ radt
NAMELIST /physics/ sf_sfclay_physics
NAMELIST /physics/ sf_surface_physics
NAMELIST /physics/ bl_pbl_physics
NAMELIST /physics/ ysu_topdown_pblmix
NAMELIST /physics/ shinhong_tke_diag
NAMELIST /physics/ windfarm_opt
NAMELIST /physics/ windfarm_ij
NAMELIST /physics/ mfshconv
NAMELIST /physics/ bldt
NAMELIST /physics/ cu_physics
NAMELIST /physics/ shcu_physics
NAMELIST /physics/ cu_diag
NAMELIST /physics/ gfs_alpha
NAMELIST /physics/ cudt
NAMELIST /physics/ gsmdt
NAMELIST /physics/ isfflx
NAMELIST /physics/ ideal_xland
NAMELIST /physics/ ifsnow
NAMELIST /physics/ icloud
NAMELIST /physics/ swrad_scat
NAMELIST /physics/ surface_input_source
NAMELIST /physics/ num_soil_layers
NAMELIST /physics/ num_urban_layers
NAMELIST /physics/ sf_surface_mosaic
NAMELIST /physics/ mosaic_cat
NAMELIST /physics/ num_urban_hi
NAMELIST /physics/ mosaic_lu
NAMELIST /physics/ mosaic_soil
NAMELIST /physics/ maxiens
NAMELIST /physics/ maxens
NAMELIST /physics/ maxens2
NAMELIST /physics/ maxens3
NAMELIST /physics/ ensdim
NAMELIST /physics/ chem_opt
NAMELIST /physics/ num_land_cat
NAMELIST /physics/ num_soil_cat
NAMELIST /physics/ topo_wind
NAMELIST /physics/ mp_zero_out
NAMELIST /physics/ mp_zero_out_thresh
NAMELIST /physics/ seaice_threshold
NAMELIST /physics/ fractional_seaice
NAMELIST /physics/ seaice_albedo_opt
NAMELIST /physics/ seaice_albedo_default
NAMELIST /physics/ seaice_snowdepth_opt
NAMELIST /physics/ seaice_snowdepth_max
NAMELIST /physics/ seaice_snowdepth_min
NAMELIST /physics/ seaice_thickness_opt
NAMELIST /physics/ seaice_thickness_default
NAMELIST /physics/ tice2tsk_if2cold
NAMELIST /physics/ sst_update
NAMELIST /physics/ sf_urban_physics
NAMELIST /physics/ usemonalb
NAMELIST /physics/ rdmaxalb
NAMELIST /physics/ rdlai2d
NAMELIST /physics/ ua_phys
NAMELIST /physics/ gwd_opt
NAMELIST /physics/ iz0tlnd
NAMELIST /physics/ sas_pgcon
NAMELIST /physics/ sas_shal_pgcon
NAMELIST /physics/ sas_shal_conv
NAMELIST /physics/ sas_mass_flux
NAMELIST /physics/ var_ric
NAMELIST /physics/ coef_ric_l
NAMELIST /physics/ coef_ric_s
NAMELIST /physics/ random_seed
NAMELIST /physics/ icoef_sf
NAMELIST /physics/ lcurr_sf
NAMELIST /physics/ ens_random_seed
NAMELIST /physics/ pert_sas
NAMELIST /physics/ pert_pbl
NAMELIST /physics/ ens_sasamp
NAMELIST /physics/ ens_pblamp
NAMELIST /physics/ idtad
NAMELIST /physics/ nsoil
NAMELIST /physics/ nphs
NAMELIST /physics/ ncnvc
NAMELIST /physics/ nrand
NAMELIST /physics/ nrads
NAMELIST /physics/ nradl
NAMELIST /physics/ tprec
NAMELIST /physics/ theat
NAMELIST /physics/ tclod
NAMELIST /physics/ trdsw
NAMELIST /physics/ trdlw
NAMELIST /physics/ tsrfc
NAMELIST /physics/ pcpflg
NAMELIST /physics/ sigma
NAMELIST /physics/ sfenth
NAMELIST /physics/ co2tf
NAMELIST /physics/ ra_call_offset
NAMELIST /physics/ cam_abs_freq_s
NAMELIST /physics/ levsiz
NAMELIST /physics/ paerlev
NAMELIST /physics/ cam_abs_dim1
NAMELIST /physics/ cam_abs_dim2
NAMELIST /physics/ no_src_types
NAMELIST /physics/ alevsiz
NAMELIST /physics/ o3input
NAMELIST /physics/ aer_opt
NAMELIST /physics/ cu_rad_feedback
NAMELIST /physics/ h_diff
NAMELIST /physics/ movemin
NAMELIST /physics/ num_snso_layers
NAMELIST /physics/ num_snow_layers
NAMELIST /physics/ use_aero_icbc
NAMELIST /physics/ ccn_conc
NAMELIST /physics/ hail_opt
NAMELIST /physics/ sf_lake_physics
NAMELIST /dynamics/ dyn_opt
NAMELIST /dynamics/ rk_ord
NAMELIST /dynamics/ w_damping
NAMELIST /dynamics/ diff_opt
NAMELIST /dynamics/ km_opt
NAMELIST /dynamics/ damp_opt
NAMELIST /dynamics/ zdamp
NAMELIST /dynamics/ base_pres
NAMELIST /dynamics/ base_temp
NAMELIST /dynamics/ base_lapse
NAMELIST /dynamics/ iso_temp
NAMELIST /dynamics/ dampcoef
NAMELIST /dynamics/ khdif
NAMELIST /dynamics/ kvdif
NAMELIST /dynamics/ c_s
NAMELIST /dynamics/ c_k
NAMELIST /dynamics/ smdiv
NAMELIST /dynamics/ emdiv
NAMELIST /dynamics/ epssm
NAMELIST /dynamics/ non_hydrostatic
NAMELIST /dynamics/ time_step_sound
NAMELIST /dynamics/ h_mom_adv_order
NAMELIST /dynamics/ v_mom_adv_order
NAMELIST /dynamics/ h_sca_adv_order
NAMELIST /dynamics/ v_sca_adv_order
NAMELIST /dynamics/ top_radiation
NAMELIST /dynamics/ tke_upper_bound
NAMELIST /dynamics/ tke_drag_coefficient
NAMELIST /dynamics/ tke_heat_flux
NAMELIST /dynamics/ pert_coriolis
NAMELIST /dynamics/ euler_adv
NAMELIST /dynamics/ idtadt
NAMELIST /dynamics/ idtadc
NAMELIST /dynamics/ codamp
NAMELIST /dynamics/ coac
NAMELIST /dynamics/ slophc
NAMELIST /dynamics/ wp
NAMELIST /dynamics/ terrain_smoothing
NAMELIST /bdy_control/ spec_bdy_width
NAMELIST /bdy_control/ spec_zone
NAMELIST /bdy_control/ relax_zone
NAMELIST /bdy_control/ specified
NAMELIST /bdy_control/ periodic_x
NAMELIST /bdy_control/ symmetric_xs
NAMELIST /bdy_control/ symmetric_xe
NAMELIST /bdy_control/ open_xs
NAMELIST /bdy_control/ open_xe
NAMELIST /bdy_control/ periodic_y
NAMELIST /bdy_control/ symmetric_ys
NAMELIST /bdy_control/ symmetric_ye
NAMELIST /bdy_control/ open_ys
NAMELIST /bdy_control/ open_ye
NAMELIST /bdy_control/ polar
NAMELIST /bdy_control/ nested
NAMELIST /bdy_control/ real_data_init_type
NAMELIST /grib2/ background_proc_id
NAMELIST /grib2/ forecast_proc_id
NAMELIST /grib2/ production_status
NAMELIST /grib2/ compression
NAMELIST /physics/ maxpatch


      OPEN ( UNIT   = nml_read_unit    ,      &
             FILE   = "namelist.input" ,      &
             FORM   = "FORMATTED"      ,      &
             STATUS = "OLD"            ,      &
             IOSTAT = io_status         )

      IF ( io_status .NE. 0 ) THEN
        CALL wrf_error_fatal3("<stdin>",6220,&
'ERROR OPENING namelist.input' )
      ENDIF

      OPEN ( UNIT   = nml_write_unit    ,      &
             FILE   = "namelist.output" ,      &
             FORM   = "FORMATTED"      ,      &
             STATUS = "REPLACE"        ,      &
             IOSTAT = io_status         )

      IF ( io_status .NE. 0 ) THEN
        CALL wrf_error_fatal3("<stdin>",6231,&
'ERROR OPENING namelist.output' )
      ENDIF








lakedepth_default = 50
lake_min_elev = 5
use_lakedepth = 1
halo_debug = 0
ntracers = 4
vortex_tracker = 1
interest_rad_storm = 500
interest_rad_parent = 500
interest_rad_self = 500
interest_kids = 1
interest_self = 0
interest_storms = 1
swath_mode = 1
num_old_fixes = 5
vt4_radius = 150000.
vt4_weightexp = 1.
vt4_pmax = -1.
vt4_noise_pmax = 103000.
vt4_noise_pmin = 85000.
vt4_noise_dpdr = 0.6
vt4_noise_iter = 2
nomove_freq = -1.0
coral_x = 5
coral_y = 5
tg_reset_stream = 0
tg_option = 0
ntornado = 1
wbd0 = 0
sbd0 = 0
analysis = .false.
write_analysis = .true.
io_form_auxinput2 = 2
high_freq = .true.
high_dom = -99
swint_opt = 0
aer_type = 1
aer_aod550_opt = 1
aer_angexp_opt = 1
aer_ssa_opt = 1
aer_asy_opt = 1
aer_aod550_val = 0.12
aer_angexp_val = 1.3
aer_ssa_val = 0.85
aer_asy_val = 0.90
dveg = 4
opt_crs = 1
opt_btr = 1
opt_run = 1
opt_sfc = 1
opt_frz = 1
opt_inf = 1
opt_rad = 3
opt_alb = 2
opt_snf = 1
opt_tbot = 2
opt_stc = 1
wrf_hydro = 0
run_days = 0
run_hours = 0
run_minutes = 0
run_seconds = 0
start_year = 1993
start_month = 03
start_day = 13
start_hour = 12
start_minute = 00
start_second = 00
end_year = 1993
end_month = 03
end_day = 14
end_hour = 12
end_minute = 00
end_second = 00
interval_seconds = 43200
input_from_file = .false.
fine_input_stream = 0
auxinput1_inname = "met_nmm.d<domain>.<date>"
io_form_auxinput1 = 2
override_restart_timers = .false.
auxhist1_inname = "auxhist1_d<domain>_<date>"
auxhist1_outname = "auxhist1_d<domain>_<date>"
auxhist1_interval_y = 0
auxhist1_interval_d = 0
auxhist1_interval_h = 0
auxhist1_interval_m = 0
auxhist1_interval_s = 0
auxhist1_interval = 0
auxhist1_begin_y = 0
auxhist1_begin_d = 0
auxhist1_begin_h = 0
auxhist1_begin_m = 0
auxhist1_begin_s = 0
auxhist1_begin = 0
auxhist1_end_y = 0
auxhist1_end_d = 0
auxhist1_end_h = 0
auxhist1_end_m = 0
auxhist1_end_s = 0
auxhist1_end = 0
io_form_auxhist1 = 0
frames_per_auxhist1 = 999999
auxhist2_inname = "auxhist2_d<domain>_<date>"
auxhist2_outname = "auxhist2_d<domain>_<date>"
auxhist2_interval_y = 0
auxhist2_interval_d = 0
auxhist2_interval_h = 0
auxhist2_interval_m = 0
auxhist2_interval_s = 0
auxhist2_interval = 0
auxhist2_begin_y = 0
auxhist2_begin_d = 0
auxhist2_begin_h = 0
auxhist2_begin_m = 0
auxhist2_begin_s = 0
auxhist2_begin = 0
auxhist2_end_y = 0
auxhist2_end_d = 0
auxhist2_end_h = 0
auxhist2_end_m = 0
auxhist2_end_s = 0
auxhist2_end = 0
io_form_auxhist2 = 0
frames_per_auxhist2 = 999999
auxhist3_inname = "auxhist3_d<domain>_<date>"
auxhist3_outname = "auxhist3_d<domain>_<date>"
auxhist3_interval_y = 0
auxhist3_interval_d = 0
auxhist3_interval_h = 0
auxhist3_interval_m = 0
auxhist3_interval_s = 0
auxhist3_interval = 0
auxhist3_begin_y = 0
auxhist3_begin_d = 0
auxhist3_begin_h = 0
auxhist3_begin_m = 0
auxhist3_begin_s = 0
auxhist3_begin = 0
auxhist3_end_y = 0
auxhist3_end_d = 0
auxhist3_end_h = 0
auxhist3_end_m = 0
auxhist3_end_s = 0
auxhist3_end = 0
io_form_auxhist3 = 0
frames_per_auxhist3 = 999999
auxhist4_inname = "auxhist4_d<domain>_<date>"
auxhist4_outname = "auxhist4_d<domain>_<date>"
auxhist4_interval_y = 0
auxhist4_interval_d = 0
auxhist4_interval_h = 0
auxhist4_interval_m = 0
auxhist4_interval_s = 0
auxhist4_interval = 0
auxhist4_begin_y = 0
auxhist4_begin_d = 0
auxhist4_begin_h = 0
auxhist4_begin_m = 0
auxhist4_begin_s = 0
auxhist4_begin = 0
auxhist4_end_y = 0
auxhist4_end_d = 0
auxhist4_end_h = 0
auxhist4_end_m = 0
auxhist4_end_s = 0
auxhist4_end = 0
io_form_auxhist4 = 0
frames_per_auxhist4 = 999999
auxhist5_inname = "auxhist5_d<domain>_<date>"
auxhist5_outname = "auxhist5_d<domain>_<date>"
auxhist5_interval_y = 0
auxhist5_interval_d = 0
auxhist5_interval_h = 0
auxhist5_interval_m = 0
auxhist5_interval_s = 0
auxhist5_interval = 0
auxhist5_begin_y = 0
auxhist5_begin_d = 0
auxhist5_begin_h = 0
auxhist5_begin_m = 0
auxhist5_begin_s = 0
auxhist5_begin = 0
auxhist5_end_y = 0
auxhist5_end_d = 0
auxhist5_end_h = 0
auxhist5_end_m = 0
auxhist5_end_s = 0
auxhist5_end = 0
io_form_auxhist5 = 0
frames_per_auxhist5 = 999999
auxhist6_inname = "auxhist6_d<domain>_<date>"
auxhist6_outname = "auxhist6_d<domain>_<date>"
auxhist6_interval_y = 0
auxhist6_interval_d = 0
auxhist6_interval_h = 0
auxhist6_interval_m = 0
auxhist6_interval_s = 0
auxhist6_interval = 0
auxhist6_begin_y = 0
auxhist6_begin_d = 0
auxhist6_begin_h = 0
auxhist6_begin_m = 0
auxhist6_begin_s = 0
auxhist6_begin = 0
auxhist6_end_y = 0
auxhist6_end_d = 0
auxhist6_end_h = 0
auxhist6_end_m = 0
auxhist6_end_s = 0
auxhist6_end = 0
io_form_auxhist6 = 0
frames_per_auxhist6 = 999999
auxhist7_inname = "auxhist7_d<domain>_<date>"
auxhist7_outname = "auxhist7_d<domain>_<date>"
auxhist7_interval_y = 0
auxhist7_interval_d = 0
auxhist7_interval_h = 0
auxhist7_interval_m = 0
auxhist7_interval_s = 0
auxhist7_interval = 0
auxhist7_begin_y = 0
auxhist7_begin_d = 0
auxhist7_begin_h = 0
auxhist7_begin_m = 0
auxhist7_begin_s = 0
auxhist7_begin = 0
auxhist7_end_y = 0
auxhist7_end_d = 0
auxhist7_end_h = 0
auxhist7_end_m = 0
auxhist7_end_s = 0
auxhist7_end = 0
io_form_auxhist7 = 0
frames_per_auxhist7 = 999999
auxhist8_inname = "auxhist8_d<domain>_<date>"
auxhist8_outname = "auxhist8_d<domain>_<date>"
auxhist8_interval_y = 0
auxhist8_interval_d = 0
auxhist8_interval_h = 0
auxhist8_interval_m = 0
auxhist8_interval_s = 0
auxhist8_interval = 0
auxhist8_begin_y = 0
auxhist8_begin_d = 0
auxhist8_begin_h = 0
auxhist8_begin_m = 0
auxhist8_begin_s = 0
auxhist8_begin = 0
auxhist8_end_y = 0
auxhist8_end_d = 0
auxhist8_end_h = 0
auxhist8_end_m = 0
auxhist8_end_s = 0
auxhist8_end = 0
io_form_auxhist8 = 0
frames_per_auxhist8 = 999999
auxhist9_inname = "auxhist9_d<domain>_<date>"
auxhist9_outname = "auxhist9_d<domain>_<date>"
auxhist9_interval_y = 0
auxhist9_interval_d = 0
auxhist9_interval_h = 0
auxhist9_interval_m = 0
auxhist9_interval_s = 0
auxhist9_interval = 0
auxhist9_begin_y = 0
auxhist9_begin_d = 0
auxhist9_begin_h = 0
auxhist9_begin_m = 0
auxhist9_begin_s = 0
auxhist9_begin = 0
auxhist9_end_y = 0
auxhist9_end_d = 0
auxhist9_end_h = 0
auxhist9_end_m = 0
auxhist9_end_s = 0
auxhist9_end = 0
io_form_auxhist9 = 0
frames_per_auxhist9 = 999999
auxhist10_inname = "auxhist10_d<domain>_<date>"
auxhist10_outname = "auxhist10_d<domain>_<date>"
auxhist10_interval_y = 0
auxhist10_interval_d = 0
auxhist10_interval_h = 0
auxhist10_interval_m = 0
auxhist10_interval_s = 0
auxhist10_interval = 0
auxhist10_begin_y = 0
auxhist10_begin_d = 0
auxhist10_begin_h = 0
auxhist10_begin_m = 0
auxhist10_begin_s = 0
auxhist10_begin = 0
auxhist10_end_y = 0
auxhist10_end_d = 0
auxhist10_end_h = 0
auxhist10_end_m = 0
auxhist10_end_s = 0
auxhist10_end = 0
io_form_auxhist10 = 0
frames_per_auxhist10 = 999999
auxhist11_inname = "auxhist11_d<domain>_<date>"
auxhist11_outname = "auxhist11_d<domain>_<date>"
auxhist11_interval_y = 0
auxhist11_interval_d = 0
auxhist11_interval_h = 0
auxhist11_interval_m = 0
auxhist11_interval_s = 0
auxhist11_interval = 0
auxhist11_begin_y = 0
auxhist11_begin_d = 0
auxhist11_begin_h = 0
auxhist11_begin_m = 0
auxhist11_begin_s = 0
auxhist11_begin = 0
auxhist11_end_y = 0
auxhist11_end_d = 0
auxhist11_end_h = 0
auxhist11_end_m = 0
auxhist11_end_s = 0
auxhist11_end = 0
io_form_auxhist11 = 0
frames_per_auxhist11 = 999999
auxhist12_inname = "auxhist12_d<domain>_<date>"
auxhist12_outname = "auxhist12_d<domain>_<date>"
auxhist12_interval_y = 0
auxhist12_interval_d = 0
auxhist12_interval_h = 0
auxhist12_interval_m = 0
auxhist12_interval_s = 0
auxhist12_interval = 0
auxhist12_begin_y = 0
auxhist12_begin_d = 0
auxhist12_begin_h = 0
auxhist12_begin_m = 0
auxhist12_begin_s = 0
auxhist12_begin = 0
auxhist12_end_y = 0
auxhist12_end_d = 0
auxhist12_end_h = 0
auxhist12_end_m = 0
auxhist12_end_s = 0
auxhist12_end = 0
io_form_auxhist12 = 0
frames_per_auxhist12 = 999999
auxhist13_inname = "auxhist13_d<domain>_<date>"
auxhist13_outname = "auxhist13_d<domain>_<date>"
auxhist13_interval_y = 0
auxhist13_interval_d = 0
auxhist13_interval_h = 0
auxhist13_interval_m = 0
auxhist13_interval_s = 0
auxhist13_interval = 0
auxhist13_begin_y = 0
auxhist13_begin_d = 0
auxhist13_begin_h = 0
auxhist13_begin_m = 0
auxhist13_begin_s = 0
auxhist13_begin = 0
auxhist13_end_y = 0
auxhist13_end_d = 0
auxhist13_end_h = 0
auxhist13_end_m = 0
auxhist13_end_s = 0
auxhist13_end = 0
io_form_auxhist13 = 0
frames_per_auxhist13 = 999999
auxhist14_inname = "auxhist14_d<domain>_<date>"
auxhist14_outname = "auxhist14_d<domain>_<date>"
auxhist14_interval_y = 0
auxhist14_interval_d = 0
auxhist14_interval_h = 0
auxhist14_interval_m = 0
auxhist14_interval_s = 0
auxhist14_interval = 0
auxhist14_begin_y = 0
auxhist14_begin_d = 0
auxhist14_begin_h = 0
auxhist14_begin_m = 0
auxhist14_begin_s = 0
auxhist14_begin = 0
auxhist14_end_y = 0
auxhist14_end_d = 0
auxhist14_end_h = 0
auxhist14_end_m = 0
auxhist14_end_s = 0
auxhist14_end = 0
io_form_auxhist14 = 0
frames_per_auxhist14 = 999999
auxhist15_inname = "auxhist15_d<domain>_<date>"
auxhist15_outname = "auxhist15_d<domain>_<date>"
auxhist15_interval_y = 0
auxhist15_interval_d = 0
auxhist15_interval_h = 0
auxhist15_interval_m = 0
auxhist15_interval_s = 0
auxhist15_interval = 0
auxhist15_begin_y = 0
auxhist15_begin_d = 0
auxhist15_begin_h = 0
auxhist15_begin_m = 0
auxhist15_begin_s = 0
auxhist15_begin = 0
auxhist15_end_y = 0
auxhist15_end_d = 0
auxhist15_end_h = 0
auxhist15_end_m = 0
auxhist15_end_s = 0
auxhist15_end = 0
io_form_auxhist15 = 0
frames_per_auxhist15 = 999999
auxhist16_inname = "auxhist16_d<domain>_<date>"
auxhist16_outname = "auxhist16_d<domain>_<date>"
auxhist16_interval_y = 0
auxhist16_interval_d = 0
auxhist16_interval_h = 0
auxhist16_interval_m = 0
auxhist16_interval_s = 0
auxhist16_interval = 0
auxhist16_begin_y = 0
auxhist16_begin_d = 0
auxhist16_begin_h = 0
auxhist16_begin_m = 0
auxhist16_begin_s = 0
auxhist16_begin = 0
auxhist16_end_y = 0
auxhist16_end_d = 0
auxhist16_end_h = 0
auxhist16_end_m = 0
auxhist16_end_s = 0
auxhist16_end = 0
io_form_auxhist16 = 0
frames_per_auxhist16 = 999999
auxhist17_inname = "auxhist17_d<domain>_<date>"
auxhist17_outname = "auxhist17_d<domain>_<date>"
auxhist17_interval_y = 0
auxhist17_interval_d = 0
auxhist17_interval_h = 0
auxhist17_interval_m = 0
auxhist17_interval_s = 0
auxhist17_interval = 0
auxhist17_begin_y = 0
auxhist17_begin_d = 0
auxhist17_begin_h = 0
auxhist17_begin_m = 0
auxhist17_begin_s = 0
auxhist17_begin = 0
auxhist17_end_y = 0
auxhist17_end_d = 0
auxhist17_end_h = 0
auxhist17_end_m = 0
auxhist17_end_s = 0
auxhist17_end = 0
io_form_auxhist17 = 0
frames_per_auxhist17 = 999999
auxhist18_inname = "auxhist18_d<domain>_<date>"
auxhist18_outname = "auxhist18_d<domain>_<date>"
auxhist18_interval_y = 0
auxhist18_interval_d = 0
auxhist18_interval_h = 0
auxhist18_interval_m = 0
auxhist18_interval_s = 0
auxhist18_interval = 0
auxhist18_begin_y = 0
auxhist18_begin_d = 0
auxhist18_begin_h = 0
auxhist18_begin_m = 0
auxhist18_begin_s = 0
auxhist18_begin = 0
auxhist18_end_y = 0
auxhist18_end_d = 0
auxhist18_end_h = 0
auxhist18_end_m = 0
auxhist18_end_s = 0
auxhist18_end = 0
io_form_auxhist18 = 0
frames_per_auxhist18 = 999999
auxhist19_inname = "auxhist19_d<domain>_<date>"
auxhist19_outname = "auxhist19_d<domain>_<date>"
auxhist19_interval_y = 0
auxhist19_interval_d = 0
auxhist19_interval_h = 0
auxhist19_interval_m = 0
auxhist19_interval_s = 0
auxhist19_interval = 0
auxhist19_begin_y = 0
auxhist19_begin_d = 0
auxhist19_begin_h = 0
auxhist19_begin_m = 0
auxhist19_begin_s = 0
auxhist19_begin = 0
auxhist19_end_y = 0
auxhist19_end_d = 0
auxhist19_end_h = 0
auxhist19_end_m = 0
auxhist19_end_s = 0
auxhist19_end = 0
io_form_auxhist19 = 0
frames_per_auxhist19 = 999999
auxhist20_inname = "auxhist20_d<domain>_<date>"
auxhist20_outname = "auxhist20_d<domain>_<date>"
auxhist20_interval_y = 0
auxhist20_interval_d = 0
auxhist20_interval_h = 0
auxhist20_interval_m = 0
auxhist20_interval_s = 0
auxhist20_interval = 0
auxhist20_begin_y = 0
auxhist20_begin_d = 0
auxhist20_begin_h = 0
auxhist20_begin_m = 0
auxhist20_begin_s = 0
auxhist20_begin = 0
auxhist20_end_y = 0
auxhist20_end_d = 0
auxhist20_end_h = 0
auxhist20_end_m = 0
auxhist20_end_s = 0
auxhist20_end = 0
io_form_auxhist20 = 0
frames_per_auxhist20 = 999999
auxhist21_inname = "auxhist21_d<domain>_<date>"
auxhist21_outname = "auxhist21_d<domain>_<date>"
auxhist21_interval_y = 0
auxhist21_interval_d = 0
auxhist21_interval_h = 0
auxhist21_interval_m = 0
auxhist21_interval_s = 0
auxhist21_interval = 0
auxhist21_begin_y = 0
auxhist21_begin_d = 0
auxhist21_begin_h = 0
auxhist21_begin_m = 0
auxhist21_begin_s = 0
auxhist21_begin = 0
auxhist21_end_y = 0
auxhist21_end_d = 0
auxhist21_end_h = 0
auxhist21_end_m = 0
auxhist21_end_s = 0
auxhist21_end = 0
io_form_auxhist21 = 0
frames_per_auxhist21 = 999999
auxhist22_inname = "auxhist22_d<domain>_<date>"
auxhist22_outname = "auxhist22_d<domain>_<date>"
auxhist22_interval_y = 0
auxhist22_interval_d = 0
auxhist22_interval_h = 0
auxhist22_interval_m = 0
auxhist22_interval_s = 0
auxhist22_interval = 0
auxhist22_begin_y = 0
auxhist22_begin_d = 0
auxhist22_begin_h = 0
auxhist22_begin_m = 0
auxhist22_begin_s = 0
auxhist22_begin = 0
auxhist22_end_y = 0
auxhist22_end_d = 0
auxhist22_end_h = 0
auxhist22_end_m = 0
auxhist22_end_s = 0
auxhist22_end = 0
io_form_auxhist22 = 0
frames_per_auxhist22 = 999999
auxhist23_inname = "auxhist23_d<domain>_<date>"
auxhist23_outname = "auxhist23_d<domain>_<date>"
auxhist23_interval_y = 0
auxhist23_interval_d = 0
auxhist23_interval_h = 0
auxhist23_interval_m = 0
auxhist23_interval_s = 0
auxhist23_interval = 0
auxhist23_begin_y = 0
auxhist23_begin_d = 0
auxhist23_begin_h = 0
auxhist23_begin_m = 0
auxhist23_begin_s = 0
auxhist23_begin = 0
auxhist23_end_y = 0
auxhist23_end_d = 0
auxhist23_end_h = 0
auxhist23_end_m = 0
auxhist23_end_s = 0
auxhist23_end = 0
io_form_auxhist23 = 0
frames_per_auxhist23 = 999999
auxhist24_inname = "auxhist24_d<domain>_<date>"
auxhist24_outname = "auxhist24_d<domain>_<date>"
auxhist24_interval_y = 0
auxhist24_interval_d = 0
auxhist24_interval_h = 0
auxhist24_interval_m = 0
auxhist24_interval_s = 0
auxhist24_interval = 0
auxhist24_begin_y = 0
auxhist24_begin_d = 0
auxhist24_begin_h = 0
auxhist24_begin_m = 0
auxhist24_begin_s = 0
auxhist24_begin = 0
auxhist24_end_y = 0
auxhist24_end_d = 0
auxhist24_end_h = 0
auxhist24_end_m = 0
auxhist24_end_s = 0
auxhist24_end = 0
io_form_auxhist24 = 0
frames_per_auxhist24 = 999999
auxinput1_outname = "auxinput1_d<domain>_<date>"
auxinput1_interval_y = 0
auxinput1_interval_d = 0
auxinput1_interval_h = 0
auxinput1_interval_m = 0
auxinput1_interval_s = 0
auxinput1_interval = 0
auxinput1_begin_y = 0
auxinput1_begin_d = 0
auxinput1_begin_h = 0
auxinput1_begin_m = 0
auxinput1_begin_s = 0
auxinput1_begin = 0
auxinput1_end_y = 0
auxinput1_end_d = 0
auxinput1_end_h = 0
auxinput1_end_m = 0
auxinput1_end_s = 0
auxinput1_end = 0
frames_per_auxinput1 = 999999
auxinput2_inname = "auxinput2_d<domain>_<date>"
auxinput2_outname = "auxinput2_d<domain>_<date>"
auxinput2_interval_y = 0
auxinput2_interval_d = 0
auxinput2_interval_h = 0
auxinput2_interval_m = 0
auxinput2_interval_s = 0
auxinput2_interval = 0
auxinput2_begin_y = 0
auxinput2_begin_d = 0
auxinput2_begin_h = 0
auxinput2_begin_m = 0
auxinput2_begin_s = 0
auxinput2_begin = 0
auxinput2_end_y = 0
auxinput2_end_d = 0
auxinput2_end_h = 0
auxinput2_end_m = 0
auxinput2_end_s = 0
auxinput2_end = 0
frames_per_auxinput2 = 999999
auxinput3_inname = "auxinput3_d<domain>_<date>"
auxinput3_outname = "auxinput3_d<domain>_<date>"
auxinput3_interval_y = 0
auxinput3_interval_d = 0
auxinput3_interval_h = 0
auxinput3_interval_m = 0
auxinput3_interval_s = 0
auxinput3_interval = 0
auxinput3_begin_y = 0
auxinput3_begin_d = 0
auxinput3_begin_h = 0
auxinput3_begin_m = 0
auxinput3_begin_s = 0
auxinput3_begin = 0
auxinput3_end_y = 0
auxinput3_end_d = 0
auxinput3_end_h = 0
auxinput3_end_m = 0
auxinput3_end_s = 0
auxinput3_end = 0
io_form_auxinput3 = 0
frames_per_auxinput3 = 999999
auxinput4_inname = "auxinput4_d<domain>_<date>"
auxinput4_outname = "auxinput4_d<domain>_<date>"
auxinput4_interval_y = 0
auxinput4_interval_d = 0
auxinput4_interval_h = 0
auxinput4_interval_m = 0
auxinput4_interval_s = 0
auxinput4_interval = 0
auxinput4_begin_y = 0
auxinput4_begin_d = 0
auxinput4_begin_h = 0
auxinput4_begin_m = 0
auxinput4_begin_s = 0
auxinput4_begin = 0
auxinput4_end_y = 0
auxinput4_end_d = 0
auxinput4_end_h = 0
auxinput4_end_m = 0
auxinput4_end_s = 0
auxinput4_end = 0
io_form_auxinput4 = 0
frames_per_auxinput4 = 999999
auxinput5_inname = "auxinput5_d<domain>_<date>"
auxinput5_outname = "auxinput5_d<domain>_<date>"
auxinput5_interval_y = 0
auxinput5_interval_d = 0
auxinput5_interval_h = 0
auxinput5_interval_m = 0
auxinput5_interval_s = 0
auxinput5_interval = 0
auxinput5_begin_y = 0
auxinput5_begin_d = 0
auxinput5_begin_h = 0
auxinput5_begin_m = 0
auxinput5_begin_s = 0
auxinput5_begin = 0
auxinput5_end_y = 0
auxinput5_end_d = 0
auxinput5_end_h = 0
auxinput5_end_m = 0
auxinput5_end_s = 0
auxinput5_end = 0
io_form_auxinput5 = 0
frames_per_auxinput5 = 999999
auxinput6_inname = "auxinput6_d<domain>_<date>"
auxinput6_outname = "auxinput6_d<domain>_<date>"
auxinput6_interval_y = 0
auxinput6_interval_d = 0
auxinput6_interval_h = 0
auxinput6_interval_m = 0
auxinput6_interval_s = 0
auxinput6_interval = 0
auxinput6_begin_y = 0
auxinput6_begin_d = 0
auxinput6_begin_h = 0
auxinput6_begin_m = 0
auxinput6_begin_s = 0
auxinput6_begin = 0
auxinput6_end_y = 0
auxinput6_end_d = 0
auxinput6_end_h = 0
auxinput6_end_m = 0
auxinput6_end_s = 0
auxinput6_end = 0
io_form_auxinput6 = 0
frames_per_auxinput6 = 999999
auxinput7_inname = "auxinput7_d<domain>_<date>"
auxinput7_outname = "auxinput7_d<domain>_<date>"
auxinput7_interval_y = 0
auxinput7_interval_d = 0
auxinput7_interval_h = 0
auxinput7_interval_m = 0
auxinput7_interval_s = 0
auxinput7_interval = 0
auxinput7_begin_y = 0
auxinput7_begin_d = 0
auxinput7_begin_h = 0
auxinput7_begin_m = 0
auxinput7_begin_s = 0
auxinput7_begin = 0
auxinput7_end_y = 0
auxinput7_end_d = 0
auxinput7_end_h = 0
auxinput7_end_m = 0
auxinput7_end_s = 0
auxinput7_end = 0
io_form_auxinput7 = 0
frames_per_auxinput7 = 999999
auxinput8_inname = "auxinput8_d<domain>_<date>"
auxinput8_outname = "auxinput8_d<domain>_<date>"
auxinput8_interval_y = 0
auxinput8_interval_d = 0
auxinput8_interval_h = 0
auxinput8_interval_m = 0
auxinput8_interval_s = 0
auxinput8_interval = 0
auxinput8_begin_y = 0
auxinput8_begin_d = 0
auxinput8_begin_h = 0
auxinput8_begin_m = 0
auxinput8_begin_s = 0
auxinput8_begin = 0
auxinput8_end_y = 0
auxinput8_end_d = 0
auxinput8_end_h = 0
auxinput8_end_m = 0
auxinput8_end_s = 0
auxinput8_end = 0
io_form_auxinput8 = 0
frames_per_auxinput8 = 999999
auxinput9_inname = "auxinput9_d<domain>_<date>"
auxinput9_outname = "auxinput9_d<domain>_<date>"
auxinput9_interval_y = 0
auxinput9_interval_d = 0
auxinput9_interval_h = 0
auxinput9_interval_m = 0
auxinput9_interval_s = 0
auxinput9_interval = 0
auxinput9_begin_y = 0
auxinput9_begin_d = 0
auxinput9_begin_h = 0
auxinput9_begin_m = 0
auxinput9_begin_s = 0
auxinput9_begin = 0
auxinput9_end_y = 0
auxinput9_end_d = 0
auxinput9_end_h = 0
auxinput9_end_m = 0
auxinput9_end_s = 0
auxinput9_end = 0
io_form_auxinput9 = 0
frames_per_auxinput9 = 999999
auxinput10_inname = "auxinput10_d<domain>_<date>"
auxinput10_outname = "auxinput10_d<domain>_<date>"
auxinput10_interval_y = 0
auxinput10_interval_d = 0
auxinput10_interval_h = 0
auxinput10_interval_m = 0
auxinput10_interval_s = 0
auxinput10_interval = 0
auxinput10_begin_y = 0
auxinput10_begin_d = 0
auxinput10_begin_h = 0
auxinput10_begin_m = 0
auxinput10_begin_s = 0
auxinput10_begin = 0
auxinput10_end_y = 0
auxinput10_end_d = 0
auxinput10_end_h = 0
auxinput10_end_m = 0
auxinput10_end_s = 0
auxinput10_end = 0
io_form_auxinput10 = 0
frames_per_auxinput10 = 999999
auxinput11_inname = "auxinput11_d<domain>_<date>"
auxinput11_outname = "auxinput11_d<domain>_<date>"
auxinput11_interval_y = 0
auxinput11_interval_d = 0
auxinput11_interval_h = 0
auxinput11_interval_m = 0
auxinput11_interval_s = 0
auxinput11_interval = 0
auxinput11_begin_y = 0
auxinput11_begin_d = 0
auxinput11_begin_h = 0
auxinput11_begin_m = 0
auxinput11_begin_s = 0
auxinput11_begin = 0
auxinput11_end_y = 0
auxinput11_end_d = 0
auxinput11_end_h = 0
auxinput11_end_m = 0
auxinput11_end_s = 0
auxinput11_end = 0
io_form_auxinput11 = 0
frames_per_auxinput11 = 999999
auxinput12_inname = "auxinput12_d<domain>_<date>"
auxinput12_outname = "auxinput12_d<domain>_<date>"
auxinput12_interval_y = 0
auxinput12_interval_d = 0
auxinput12_interval_h = 0
auxinput12_interval_m = 0
auxinput12_interval_s = 0
auxinput12_interval = 0
auxinput12_begin_y = 0
auxinput12_begin_d = 0
auxinput12_begin_h = 0
auxinput12_begin_m = 0
auxinput12_begin_s = 0
auxinput12_begin = 0
auxinput12_end_y = 0
auxinput12_end_d = 0
auxinput12_end_h = 0
auxinput12_end_m = 0
auxinput12_end_s = 0
auxinput12_end = 0
io_form_auxinput12 = 0
frames_per_auxinput12 = 999999
auxinput13_inname = "auxinput13_d<domain>_<date>"
auxinput13_outname = "auxinput13_d<domain>_<date>"
auxinput13_interval_y = 0
auxinput13_interval_d = 0
auxinput13_interval_h = 0
auxinput13_interval_m = 0
auxinput13_interval_s = 0
auxinput13_interval = 0
auxinput13_begin_y = 0
auxinput13_begin_d = 0
auxinput13_begin_h = 0
auxinput13_begin_m = 0
auxinput13_begin_s = 0
auxinput13_begin = 0
auxinput13_end_y = 0
auxinput13_end_d = 0
auxinput13_end_h = 0
auxinput13_end_m = 0
auxinput13_end_s = 0
auxinput13_end = 0
io_form_auxinput13 = 0
frames_per_auxinput13 = 999999
auxinput14_inname = "auxinput14_d<domain>_<date>"
auxinput14_outname = "auxinput14_d<domain>_<date>"
auxinput14_interval_y = 0
auxinput14_interval_d = 0
auxinput14_interval_h = 0
auxinput14_interval_m = 0
auxinput14_interval_s = 0
auxinput14_interval = 0
auxinput14_begin_y = 0
auxinput14_begin_d = 0
auxinput14_begin_h = 0
auxinput14_begin_m = 0
auxinput14_begin_s = 0
auxinput14_begin = 0
auxinput14_end_y = 0
auxinput14_end_d = 0
auxinput14_end_h = 0
auxinput14_end_m = 0
auxinput14_end_s = 0
auxinput14_end = 0
io_form_auxinput14 = 0
frames_per_auxinput14 = 999999
auxinput15_inname = "auxinput15_d<domain>_<date>"
auxinput15_outname = "auxinput15_d<domain>_<date>"
auxinput15_interval_y = 0
auxinput15_interval_d = 0
auxinput15_interval_h = 0
auxinput15_interval_m = 0
auxinput15_interval_s = 0
auxinput15_interval = 0
auxinput15_begin_y = 0
auxinput15_begin_d = 0
auxinput15_begin_h = 0
auxinput15_begin_m = 0
auxinput15_begin_s = 0
auxinput15_begin = 0
auxinput15_end_y = 0
auxinput15_end_d = 0
auxinput15_end_h = 0
auxinput15_end_m = 0
auxinput15_end_s = 0
auxinput15_end = 0
io_form_auxinput15 = 0
frames_per_auxinput15 = 999999
auxinput16_inname = "auxinput16_d<domain>_<date>"
auxinput16_outname = "auxinput16_d<domain>_<date>"
auxinput16_interval_y = 0
auxinput16_interval_d = 0
auxinput16_interval_h = 0
auxinput16_interval_m = 0
auxinput16_interval_s = 0
auxinput16_interval = 0
auxinput16_begin_y = 0
auxinput16_begin_d = 0
auxinput16_begin_h = 0
auxinput16_begin_m = 0
auxinput16_begin_s = 0
auxinput16_begin = 0
auxinput16_end_y = 0
auxinput16_end_d = 0
auxinput16_end_h = 0
auxinput16_end_m = 0
auxinput16_end_s = 0
auxinput16_end = 0
io_form_auxinput16 = 0
frames_per_auxinput16 = 999999
auxinput17_inname = "auxinput17_d<domain>_<date>"
auxinput17_outname = "auxinput17_d<domain>_<date>"
auxinput17_interval_y = 0
auxinput17_interval_d = 0
auxinput17_interval_h = 0
auxinput17_interval_m = 0
auxinput17_interval_s = 0
auxinput17_interval = 0
auxinput17_begin_y = 0
auxinput17_begin_d = 0
auxinput17_begin_h = 0
auxinput17_begin_m = 0
auxinput17_begin_s = 0
auxinput17_begin = 0
auxinput17_end_y = 0
auxinput17_end_d = 0
auxinput17_end_h = 0
auxinput17_end_m = 0
auxinput17_end_s = 0
auxinput17_end = 0
io_form_auxinput17 = 0
frames_per_auxinput17 = 999999
auxinput18_inname = "auxinput18_d<domain>_<date>"
auxinput18_outname = "auxinput18_d<domain>_<date>"
auxinput18_interval_y = 0
auxinput18_interval_d = 0
auxinput18_interval_h = 0
auxinput18_interval_m = 0
auxinput18_interval_s = 0
auxinput18_interval = 0
auxinput18_begin_y = 0
auxinput18_begin_d = 0
auxinput18_begin_h = 0
auxinput18_begin_m = 0
auxinput18_begin_s = 0
auxinput18_begin = 0
auxinput18_end_y = 0
auxinput18_end_d = 0
auxinput18_end_h = 0
auxinput18_end_m = 0
auxinput18_end_s = 0
auxinput18_end = 0
io_form_auxinput18 = 0
frames_per_auxinput18 = 999999
auxinput19_inname = "auxinput19_d<domain>_<date>"
auxinput19_outname = "auxinput19_d<domain>_<date>"
auxinput19_interval_y = 0
auxinput19_interval_d = 0
auxinput19_interval_h = 0
auxinput19_interval_m = 0
auxinput19_interval_s = 0
auxinput19_interval = 0
auxinput19_begin_y = 0
auxinput19_begin_d = 0
auxinput19_begin_h = 0
auxinput19_begin_m = 0
auxinput19_begin_s = 0
auxinput19_begin = 0
auxinput19_end_y = 0
auxinput19_end_d = 0
auxinput19_end_h = 0
auxinput19_end_m = 0
auxinput19_end_s = 0
auxinput19_end = 0
io_form_auxinput19 = 0
frames_per_auxinput19 = 999999
auxinput20_inname = "auxinput20_d<domain>_<date>"
auxinput20_outname = "auxinput20_d<domain>_<date>"
auxinput20_interval_y = 0
auxinput20_interval_d = 0
auxinput20_interval_h = 0
auxinput20_interval_m = 0
auxinput20_interval_s = 0
auxinput20_interval = 0
auxinput20_begin_y = 0
auxinput20_begin_d = 0
auxinput20_begin_h = 0
auxinput20_begin_m = 0
auxinput20_begin_s = 0
auxinput20_begin = 0
auxinput20_end_y = 0
auxinput20_end_d = 0
auxinput20_end_h = 0
auxinput20_end_m = 0
auxinput20_end_s = 0
auxinput20_end = 0
io_form_auxinput20 = 0
frames_per_auxinput20 = 999999
auxinput21_inname = "auxinput21_d<domain>_<date>"
auxinput21_outname = "auxinput21_d<domain>_<date>"
auxinput21_interval_y = 0
auxinput21_interval_d = 0
auxinput21_interval_h = 0
auxinput21_interval_m = 0
auxinput21_interval_s = 0
auxinput21_interval = 0
auxinput21_begin_y = 0
auxinput21_begin_d = 0
auxinput21_begin_h = 0
auxinput21_begin_m = 0
auxinput21_begin_s = 0
auxinput21_begin = 0
auxinput21_end_y = 0
auxinput21_end_d = 0
auxinput21_end_h = 0
auxinput21_end_m = 0
auxinput21_end_s = 0
auxinput21_end = 0
io_form_auxinput21 = 0
frames_per_auxinput21 = 999999
auxinput22_inname = "auxinput22_d<domain>_<date>"
auxinput22_outname = "auxinput22_d<domain>_<date>"
auxinput22_interval_y = 0
auxinput22_interval_d = 0
auxinput22_interval_h = 0
auxinput22_interval_m = 0
auxinput22_interval_s = 0
auxinput22_interval = 0
auxinput22_begin_y = 0
auxinput22_begin_d = 0
auxinput22_begin_h = 0
auxinput22_begin_m = 0
auxinput22_begin_s = 0
auxinput22_begin = 0
auxinput22_end_y = 0
auxinput22_end_d = 0
auxinput22_end_h = 0
auxinput22_end_m = 0
auxinput22_end_s = 0
auxinput22_end = 0
io_form_auxinput22 = 0
frames_per_auxinput22 = 999999
auxinput23_inname = "auxinput23_d<domain>_<date>"
auxinput23_outname = "auxinput23_d<domain>_<date>"
auxinput23_interval_y = 0
auxinput23_interval_d = 0
auxinput23_interval_h = 0
auxinput23_interval_m = 0
auxinput23_interval_s = 0
auxinput23_interval = 0
auxinput23_begin_y = 0
auxinput23_begin_d = 0
auxinput23_begin_h = 0
auxinput23_begin_m = 0
auxinput23_begin_s = 0
auxinput23_begin = 0
auxinput23_end_y = 0
auxinput23_end_d = 0
auxinput23_end_h = 0
auxinput23_end_m = 0
auxinput23_end_s = 0
auxinput23_end = 0
io_form_auxinput23 = 0
frames_per_auxinput23 = 999999
auxinput24_inname = "auxinput24_d<domain>_<date>"
auxinput24_outname = "auxinput24_d<domain>_<date>"
auxinput24_interval_y = 0
auxinput24_interval_d = 0
auxinput24_interval_h = 0
auxinput24_interval_m = 0
auxinput24_interval_s = 0
auxinput24_interval = 0
auxinput24_begin_y = 0
auxinput24_begin_d = 0
auxinput24_begin_h = 0
auxinput24_begin_m = 0
auxinput24_begin_s = 0
auxinput24_begin = 0
auxinput24_end_y = 0
auxinput24_end_d = 0
auxinput24_end_h = 0
auxinput24_end_m = 0
auxinput24_end_s = 0
auxinput24_end = 0
io_form_auxinput24 = 0
frames_per_auxinput24 = 999999
history_interval = 0
frames_per_outfile = 10
restart = .false.
restart_interval = 0
io_form_input = 2
io_form_history = 2
io_form_restart = 2
io_form_boundary = 2
debug_level = 0
self_test_domain = .false.
history_outname = "wrfout_d<domain>_<date>"
history_inname = "wrfhist_d<domain>_<date>"
use_netcdf_classic = .false.
history_interval_d = 0
history_interval_h = 0
history_interval_m = 0
history_interval_s = 0
inputout_interval_d = 0
inputout_interval_h = 0
inputout_interval_m = 0
inputout_interval_s = 0
inputout_interval = 0
restart_interval_d = 0
restart_interval_h = 0
restart_interval_m = 0
restart_interval_s = 0
history_begin_y = 0
history_begin_d = 0
history_begin_h = 0
history_begin_m = 0
history_begin_s = 0
history_begin = 0
inputout_begin_y = 0
inputout_begin_d = 0
inputout_begin_h = 0
inputout_begin_m = 0
inputout_begin_s = 0
restart_begin_y = 0
restart_begin_d = 0
restart_begin_h = 0
restart_begin_m = 0
restart_begin_s = 0
restart_begin = 0
history_end_y = 0
history_end_d = 0
history_end_h = 0
history_end_m = 0
history_end_s = 0
history_end = 0
inputout_end_y = 0
inputout_end_d = 0
inputout_end_h = 0
inputout_end_m = 0
inputout_end_s = 0
simulation_start_year = 0
simulation_start_month = 0
simulation_start_day = 0
simulation_start_hour = 0
simulation_start_minute = 0
simulation_start_second = 0
reset_simulation_start = .false.
sr_x = 0
sr_y = 0
iofields_filename = "NONE_SPECIFIED"
ignore_iofields_warning = .true.
ncd_nofill = .true.
julyr = 0
julday = 1
gmt = 0.
high_freq_outname = "hifreq_d<domain>.htcf"
partial_atcf_outname = "track_d<domain>.patcf"
input_inname = "wrfinput_d<domain>"
input_outname = "wrfinput_d<domain>"
bdy_inname = "wrfbdy_d<domain>"
bdy_outname = "wrfbdy_d<domain>"
rst_inname = "wrfrst_d<domain>_<date>"
rst_outname = "wrfrst_d<domain>_<date>"
anl_outname = "wrfanl_d<domain>_<date>"
write_input = .false.
write_restart_at_0h = .false.
write_hist_at_0h_rst = .false.
adjust_output_times = .false.
adjust_input_times = .false.
tstart = 0.
nocolons = .false.
cycling = .false.
output_ready_flag = .false.
dfi_opt = 0
dfi_savehydmeteors = 0
dfi_nfilter = 7
dfi_write_filtered_input = .true.
dfi_write_dfi_history = .false.
dfi_cutoff_seconds = 3600
dfi_time_dim = 1000
dfi_fwdstop_year = 2004
dfi_fwdstop_month = 03
dfi_fwdstop_day = 13
dfi_fwdstop_hour = 12
dfi_fwdstop_minute = 00
dfi_fwdstop_second = 00
dfi_bckstop_year = 2004
dfi_bckstop_month = 03
dfi_bckstop_day = 14
dfi_bckstop_hour = 12
dfi_bckstop_minute = 00
dfi_bckstop_second = 00
time_step_fract_num = 0
time_step_fract_den = 1
max_dom = 1
s_we = 1
e_we = 32
s_sn = 1
e_sn = 32
s_vert = 1
e_vert = 31
num_metgrid_soil_levels = 4
dx = 200
dy = 200
grid_id = 1
grid_allowed = .true.
parent_id = 0
i_parent_start = 1
j_parent_start = 1
parent_grid_ratio = 1
parent_time_step_ratio = 1
feedback = 0
smooth_option = 2
ztop = 15000.
moad_grid_ratio = 1
moad_time_step_ratio = 1
shw = 2
tile_sz_x = 0
tile_sz_y = 0
numtiles = 1
numtiles_inc = 0
numtiles_x = 0
numtiles_y = 0
tile_strategy = 0
nproc_x = -1
nproc_y = -1
irand = 0
dt = 2.
ts_buf_size = 200
max_ts_locs = 5
num_moves = 0
vortex_interval = 15
corral_dist = 8
move_id = 0
move_interval = 999999999
move_cd_x = 0
move_cd_y = 0
swap_x = .false.
swap_y = .false.
cycle_x = .false.
cycle_y = .false.
reorder_mesh = .false.
perturb_input = .false.
eta_levels = -1.
ptsgm = 42000.
num_metgrid_levels = 43
p_top_requested = 5000
use_prep_hybrid = .false.
force_read_thompson = .false.
write_thompson_tables = .true.
mp_physics = 0
mommix = 0.7
disheat = .true.
do_radar_ref = 0
compute_radar_ref = 0
ra_lw_physics = 0
ra_sw_physics = 0
radt = 0
sf_sfclay_physics = 0
sf_surface_physics = 0
bl_pbl_physics = 0
ysu_topdown_pblmix = 0
shinhong_tke_diag = 0
windfarm_opt = 0
windfarm_ij = 0
mfshconv = 1
bldt = 0
cu_physics = 0
shcu_physics = 0
cu_diag = 0
gfs_alpha = 1
cudt = 0
gsmdt = 0
isfflx = 1
ideal_xland = 1
ifsnow = 1
icloud = 1
swrad_scat = 1
surface_input_source = 1
num_soil_layers = 5
num_urban_layers = 400
sf_surface_mosaic = 0
mosaic_cat = 3
mosaic_cat_soil = 12
num_urban_hi = 15
mosaic_lu = 0
mosaic_soil = 0
maxiens = 1
maxens = 3
maxens2 = 3
maxens3 = 16
ensdim = 144
chem_opt = 0
num_land_cat = 24
num_soil_cat = 16
topo_wind = 0
mp_zero_out = 0
mp_zero_out_thresh = 1.e-8
seaice_threshold = 100
fractional_seaice = 0
seaice_albedo_opt = 0
seaice_albedo_default = 0.65
seaice_snowdepth_opt = 0
seaice_snowdepth_max = 1.e10
seaice_snowdepth_min = 0.001
seaice_thickness_opt = 0
seaice_thickness_default = 3.0
tice2tsk_if2cold = .false.
sst_update = 0
sf_urban_physics = 0
usemonalb = .true.
rdmaxalb = .true.
rdlai2d = .false.
ua_phys = .false.
gwd_opt = 2
iz0tlnd = 0
sas_pgcon = 0.55
sas_shal_pgcon = -1
sas_shal_conv = 1
sas_mass_flux = 9e9
var_ric = 1.
coef_ric_l = 0.16
coef_ric_s = 0.16
random_seed = 0
icoef_sf = 0
lcurr_sf = .false.
ens_random_seed = -1
pert_sas = .false.
pert_pbl = .false.
ens_sasamp = 50.
ens_pblamp = 0.2
idtad = 2
nsoil = 4
nphs = 10
ncnvc = 10
nrand = 10
nrads = 200
nradl = 200
tprec = 385.
theat = 385.
tclod = 385.
trdsw = 385.
trdlw = 385.
tsrfc = 385.
pcpflg = .false.
sigma = 1
sfenth = 0.0
co2tf = 0
ra_call_offset = -1
cam_abs_freq_s = 21600.
levsiz = 1
paerlev = 1
cam_abs_dim1 = 1
cam_abs_dim2 = 1
no_src_types = 1
alevsiz = 1
o3input = 2
aer_opt = 0
cu_rad_feedback = .false.
icloud_cu = 0
h_diff = 0.1
movemin = 10
num_snso_layers = 7
num_snow_layers = 3
use_aero_icbc = .false.
ccn_conc = 1.0e8
hail_opt = 0
sf_lake_physics = 0
rk_ord = 3
w_damping = 0
diff_opt = -1
km_opt = -1
damp_opt = 1
zdamp = 5000.
base_pres = 100000.
base_temp = 290.
base_lapse = 50.
iso_temp = 0.
dampcoef = 0.2
khdif = 0
kvdif = 0
c_s = 0.25
c_k = 0.15
smdiv = 0.
emdiv = 0.
epssm = .1
non_hydrostatic = .true.
time_step_sound = 10
h_mom_adv_order = 3
v_mom_adv_order = 3
h_sca_adv_order = 3
v_sca_adv_order = 3
top_radiation = .false.
tke_upper_bound = 1000.
tke_drag_coefficient = 0.
tke_heat_flux = 0.
pert_coriolis = .false.
euler_adv = .false.
idtadt = 1
idtadc = 1
codamp = 6.4
coac = 1.6
slophc = 6.363961e-3
wp = 0.
terrain_smoothing = 1
spec_bdy_width = 5
spec_zone = 1
relax_zone = 4
specified = .false.
periodic_x = .false.
symmetric_xs = .false.
symmetric_xe = .false.
open_xs = .false.
open_xe = .false.
periodic_y = .false.
symmetric_ys = .false.
symmetric_ye = .false.
open_ys = .false.
open_ye = .false.
polar = .false.
nested = .false.
real_data_init_type = 1
background_proc_id = 255
forecast_proc_id = 255
production_status = 255
compression = 40
cen_lat = 0
cen_lon = 0
truelat1 = 0
truelat2 = 0
moad_cen_lat = 0
stand_lon = 0
flag_metgrid = 0
flag_snow = 0
flag_psfc = 0
flag_sm000010 = 0
flag_sm010040 = 0
flag_sm040100 = 0
flag_sm100200 = 0
flag_st000010 = 0
flag_st010040 = 0
flag_st040100 = 0
flag_st100200 = 0
flag_slp = 0
flag_soilhgt = 0
flag_mf_xy = 0
bdyfrq = 0
mminlu = " "
iswater = 0
islake = 0
isice = 0
isurban = 0
isoilwater = 0
map_proj = 0
dfi_stage = 3
mp_physics_dfi = -1
maxpatch = 10












 nml_read_error = .FALSE.
 NML_LOOP : DO i=1,8
    REWIND ( UNIT = nml_read_unit )
    SELECT CASE ( i )
       CASE ( 1 ) 
          nml_name = "physics"
          READ   ( UNIT = nml_read_unit , NML = physics , ERR=9201, END=9202 )
          WRITE ( UNIT = nml_write_unit, NML = physics )
          CYCLE NML_LOOP
       CASE ( 2 ) 
          nml_name = "domains"
          READ   ( UNIT = nml_read_unit , NML = domains , ERR=9201, END=9202 )
          WRITE ( UNIT = nml_write_unit, NML = domains )
          CYCLE NML_LOOP
       CASE ( 3 ) 
          nml_name = "time_control"
          READ   ( UNIT = nml_read_unit , NML = time_control , ERR=9201, END=9202 )
          WRITE ( UNIT = nml_write_unit, NML = time_control )
          CYCLE NML_LOOP
       CASE ( 4 ) 
          nml_name = "noah_mp"
          READ   ( UNIT = nml_read_unit , NML = noah_mp , ERR=9201, END=9202 )
          WRITE ( UNIT = nml_write_unit, NML = noah_mp )
          CYCLE NML_LOOP
       CASE ( 5 ) 
          nml_name = "dfi_control"
          READ   ( UNIT = nml_read_unit , NML = dfi_control , ERR=9201, END=9202 )
          WRITE ( UNIT = nml_write_unit, NML = dfi_control )
          CYCLE NML_LOOP
       CASE ( 6 ) 
          nml_name = "dynamics"
          READ   ( UNIT = nml_read_unit , NML = dynamics , ERR=9201, END=9202 )
          WRITE ( UNIT = nml_write_unit, NML = dynamics )
          CYCLE NML_LOOP
       CASE ( 7 ) 
          nml_name = "bdy_control"
          READ   ( UNIT = nml_read_unit , NML = bdy_control , ERR=9201, END=9202 )
          WRITE ( UNIT = nml_write_unit, NML = bdy_control )
          CYCLE NML_LOOP
       CASE ( 8 ) 
          nml_name = "grib2"
          READ   ( UNIT = nml_read_unit , NML = grib2 , ERR=9201, END=9202 )
          WRITE ( UNIT = nml_write_unit, NML = grib2 )
          CYCLE NML_LOOP
    END SELECT
9201 CALL wrf_message("  ------ ERROR while reading namelist "//TRIM(nml_name)//" ------")
    nml_read_error = .TRUE.
    CALL wrf_alt_nml_obsolete(nml_read_unit, TRIM(nml_name))
    CYCLE NML_LOOP
9202 CALL wrf_debug(1,"Namelist "//TRIM(nml_name)//" not found in namelist.input.")
     CALL wrf_debug(1," --> Using registry defaults for variables in "//TRIM(nml_name))
 END DO NML_LOOP
 
 IF ( nml_read_error ) CALL wrf_error_fatal3("<stdin>",7810,&
"ERRORS while reading one or more namelists from namelist.input.")










      DO i = 1, max_dom
         mp_physics(i) = mp_physics(max_dom)
      ENDDO










 model_config_rec % lakedepth_default          =  lakedepth_default 
 model_config_rec % lake_min_elev              =  lake_min_elev 
 model_config_rec % use_lakedepth              =  use_lakedepth 
 model_config_rec % halo_debug                 =  halo_debug 
 model_config_rec % ntracers                   =  ntracers 
 model_config_rec % vortex_tracker             =  vortex_tracker 
 model_config_rec % interest_rad_storm         =  interest_rad_storm 
 model_config_rec % interest_rad_parent        =  interest_rad_parent 
 model_config_rec % interest_rad_self          =  interest_rad_self 
 model_config_rec % interest_kids              =  interest_kids 
 model_config_rec % interest_self              =  interest_self 
 model_config_rec % interest_storms            =  interest_storms 
 model_config_rec % swath_mode                 =  swath_mode 
 model_config_rec % num_old_fixes              =  num_old_fixes 
 model_config_rec % vt4_radius                 =  vt4_radius 
 model_config_rec % vt4_weightexp              =  vt4_weightexp 
 model_config_rec % vt4_pmax                   =  vt4_pmax 
 model_config_rec % vt4_noise_pmax             =  vt4_noise_pmax 
 model_config_rec % vt4_noise_pmin             =  vt4_noise_pmin 
 model_config_rec % vt4_noise_dpdr             =  vt4_noise_dpdr 
 model_config_rec % vt4_noise_iter             =  vt4_noise_iter 
 model_config_rec % nomove_freq                =  nomove_freq 
 model_config_rec % coral_x                    =  coral_x 
 model_config_rec % coral_y                    =  coral_y 
 model_config_rec % tg_reset_stream            =  tg_reset_stream 
 model_config_rec % tg_option                  =  tg_option 
 model_config_rec % ntornado                   =  ntornado 
 model_config_rec % wbd0                       =  wbd0 
 model_config_rec % sbd0                       =  sbd0 
 model_config_rec % analysis                   =  analysis 
 model_config_rec % write_analysis             =  write_analysis 
 model_config_rec % io_form_auxinput2          =  io_form_auxinput2 
 model_config_rec % high_freq                  =  high_freq 
 model_config_rec % high_dom                   =  high_dom 
 model_config_rec % swint_opt                  =  swint_opt 
 model_config_rec % aer_type                   =  aer_type 
 model_config_rec % aer_aod550_opt             =  aer_aod550_opt 
 model_config_rec % aer_angexp_opt             =  aer_angexp_opt 
 model_config_rec % aer_ssa_opt                =  aer_ssa_opt 
 model_config_rec % aer_asy_opt                =  aer_asy_opt 
 model_config_rec % aer_aod550_val             =  aer_aod550_val 
 model_config_rec % aer_angexp_val             =  aer_angexp_val 
 model_config_rec % aer_ssa_val                =  aer_ssa_val 
 model_config_rec % aer_asy_val                =  aer_asy_val 
 model_config_rec % dveg                       =  dveg 
 model_config_rec % opt_crs                    =  opt_crs 
 model_config_rec % opt_btr                    =  opt_btr 
 model_config_rec % opt_run                    =  opt_run 
 model_config_rec % opt_sfc                    =  opt_sfc 
 model_config_rec % opt_frz                    =  opt_frz 
 model_config_rec % opt_inf                    =  opt_inf 
 model_config_rec % opt_rad                    =  opt_rad 
 model_config_rec % opt_alb                    =  opt_alb 
 model_config_rec % opt_snf                    =  opt_snf 
 model_config_rec % opt_tbot                   =  opt_tbot 
 model_config_rec % opt_stc                    =  opt_stc 
 model_config_rec % wrf_hydro                  =  wrf_hydro 
 model_config_rec % run_days                   =  run_days 
 model_config_rec % run_hours                  =  run_hours 
 model_config_rec % run_minutes                =  run_minutes 
 model_config_rec % run_seconds                =  run_seconds 
 model_config_rec % start_year                 =  start_year 
 model_config_rec % start_month                =  start_month 
 model_config_rec % start_day                  =  start_day 
 model_config_rec % start_hour                 =  start_hour 
 model_config_rec % start_minute               =  start_minute 
 model_config_rec % start_second               =  start_second 
 model_config_rec % end_year                   =  end_year 
 model_config_rec % end_month                  =  end_month 
 model_config_rec % end_day                    =  end_day 
 model_config_rec % end_hour                   =  end_hour 
 model_config_rec % end_minute                 =  end_minute 
 model_config_rec % end_second                 =  end_second 
 model_config_rec % interval_seconds           =  interval_seconds 
 model_config_rec % input_from_file            =  input_from_file 
 model_config_rec % fine_input_stream          =  fine_input_stream 
 model_config_rec % auxinput1_inname           =  auxinput1_inname 
 model_config_rec % io_form_auxinput1          =  io_form_auxinput1 
 model_config_rec % override_restart_timers    =  override_restart_timers 
 model_config_rec % auxhist1_inname            =  auxhist1_inname 
 model_config_rec % auxhist1_outname           =  auxhist1_outname 
 model_config_rec % auxhist1_interval_y        =  auxhist1_interval_y 
 model_config_rec % auxhist1_interval_d        =  auxhist1_interval_d 
 model_config_rec % auxhist1_interval_h        =  auxhist1_interval_h 
 model_config_rec % auxhist1_interval_m        =  auxhist1_interval_m 
 model_config_rec % auxhist1_interval_s        =  auxhist1_interval_s 
 model_config_rec % auxhist1_interval          =  auxhist1_interval 
 model_config_rec % auxhist1_begin_y           =  auxhist1_begin_y 
 model_config_rec % auxhist1_begin_d           =  auxhist1_begin_d 
 model_config_rec % auxhist1_begin_h           =  auxhist1_begin_h 
 model_config_rec % auxhist1_begin_m           =  auxhist1_begin_m 
 model_config_rec % auxhist1_begin_s           =  auxhist1_begin_s 
 model_config_rec % auxhist1_begin             =  auxhist1_begin 
 model_config_rec % auxhist1_end_y             =  auxhist1_end_y 
 model_config_rec % auxhist1_end_d             =  auxhist1_end_d 
 model_config_rec % auxhist1_end_h             =  auxhist1_end_h 
 model_config_rec % auxhist1_end_m             =  auxhist1_end_m 
 model_config_rec % auxhist1_end_s             =  auxhist1_end_s 
 model_config_rec % auxhist1_end               =  auxhist1_end 
 model_config_rec % io_form_auxhist1           =  io_form_auxhist1 
 model_config_rec % frames_per_auxhist1        =  frames_per_auxhist1 
 model_config_rec % auxhist2_inname            =  auxhist2_inname 
 model_config_rec % auxhist2_outname           =  auxhist2_outname 
 model_config_rec % auxhist2_interval_y        =  auxhist2_interval_y 
 model_config_rec % auxhist2_interval_d        =  auxhist2_interval_d 
 model_config_rec % auxhist2_interval_h        =  auxhist2_interval_h 
 model_config_rec % auxhist2_interval_m        =  auxhist2_interval_m 
 model_config_rec % auxhist2_interval_s        =  auxhist2_interval_s 
 model_config_rec % auxhist2_interval          =  auxhist2_interval 
 model_config_rec % auxhist2_begin_y           =  auxhist2_begin_y 
 model_config_rec % auxhist2_begin_d           =  auxhist2_begin_d 
 model_config_rec % auxhist2_begin_h           =  auxhist2_begin_h 
 model_config_rec % auxhist2_begin_m           =  auxhist2_begin_m 
 model_config_rec % auxhist2_begin_s           =  auxhist2_begin_s 
 model_config_rec % auxhist2_begin             =  auxhist2_begin 
 model_config_rec % auxhist2_end_y             =  auxhist2_end_y 
 model_config_rec % auxhist2_end_d             =  auxhist2_end_d 
 model_config_rec % auxhist2_end_h             =  auxhist2_end_h 
 model_config_rec % auxhist2_end_m             =  auxhist2_end_m 
 model_config_rec % auxhist2_end_s             =  auxhist2_end_s 
 model_config_rec % auxhist2_end               =  auxhist2_end 
 model_config_rec % io_form_auxhist2           =  io_form_auxhist2 
 model_config_rec % frames_per_auxhist2        =  frames_per_auxhist2 
 model_config_rec % auxhist3_inname            =  auxhist3_inname 
 model_config_rec % auxhist3_outname           =  auxhist3_outname 
 model_config_rec % auxhist3_interval_y        =  auxhist3_interval_y 
 model_config_rec % auxhist3_interval_d        =  auxhist3_interval_d 
 model_config_rec % auxhist3_interval_h        =  auxhist3_interval_h 
 model_config_rec % auxhist3_interval_m        =  auxhist3_interval_m 
 model_config_rec % auxhist3_interval_s        =  auxhist3_interval_s 
 model_config_rec % auxhist3_interval          =  auxhist3_interval 
 model_config_rec % auxhist3_begin_y           =  auxhist3_begin_y 
 model_config_rec % auxhist3_begin_d           =  auxhist3_begin_d 
 model_config_rec % auxhist3_begin_h           =  auxhist3_begin_h 
 model_config_rec % auxhist3_begin_m           =  auxhist3_begin_m 
 model_config_rec % auxhist3_begin_s           =  auxhist3_begin_s 
 model_config_rec % auxhist3_begin             =  auxhist3_begin 
 model_config_rec % auxhist3_end_y             =  auxhist3_end_y 
 model_config_rec % auxhist3_end_d             =  auxhist3_end_d 
 model_config_rec % auxhist3_end_h             =  auxhist3_end_h 
 model_config_rec % auxhist3_end_m             =  auxhist3_end_m 
 model_config_rec % auxhist3_end_s             =  auxhist3_end_s 
 model_config_rec % auxhist3_end               =  auxhist3_end 
 model_config_rec % io_form_auxhist3           =  io_form_auxhist3 
 model_config_rec % frames_per_auxhist3        =  frames_per_auxhist3 
 model_config_rec % auxhist4_inname            =  auxhist4_inname 
 model_config_rec % auxhist4_outname           =  auxhist4_outname 
 model_config_rec % auxhist4_interval_y        =  auxhist4_interval_y 
 model_config_rec % auxhist4_interval_d        =  auxhist4_interval_d 
 model_config_rec % auxhist4_interval_h        =  auxhist4_interval_h 
 model_config_rec % auxhist4_interval_m        =  auxhist4_interval_m 
 model_config_rec % auxhist4_interval_s        =  auxhist4_interval_s 
 model_config_rec % auxhist4_interval          =  auxhist4_interval 
 model_config_rec % auxhist4_begin_y           =  auxhist4_begin_y 
 model_config_rec % auxhist4_begin_d           =  auxhist4_begin_d 
 model_config_rec % auxhist4_begin_h           =  auxhist4_begin_h 
 model_config_rec % auxhist4_begin_m           =  auxhist4_begin_m 
 model_config_rec % auxhist4_begin_s           =  auxhist4_begin_s 
 model_config_rec % auxhist4_begin             =  auxhist4_begin 
 model_config_rec % auxhist4_end_y             =  auxhist4_end_y 
 model_config_rec % auxhist4_end_d             =  auxhist4_end_d 
 model_config_rec % auxhist4_end_h             =  auxhist4_end_h 
 model_config_rec % auxhist4_end_m             =  auxhist4_end_m 
 model_config_rec % auxhist4_end_s             =  auxhist4_end_s 
 model_config_rec % auxhist4_end               =  auxhist4_end 
 model_config_rec % io_form_auxhist4           =  io_form_auxhist4 
 model_config_rec % frames_per_auxhist4        =  frames_per_auxhist4 
 model_config_rec % auxhist5_inname            =  auxhist5_inname 
 model_config_rec % auxhist5_outname           =  auxhist5_outname 
 model_config_rec % auxhist5_interval_y        =  auxhist5_interval_y 
 model_config_rec % auxhist5_interval_d        =  auxhist5_interval_d 
 model_config_rec % auxhist5_interval_h        =  auxhist5_interval_h 
 model_config_rec % auxhist5_interval_m        =  auxhist5_interval_m 
 model_config_rec % auxhist5_interval_s        =  auxhist5_interval_s 
 model_config_rec % auxhist5_interval          =  auxhist5_interval 
 model_config_rec % auxhist5_begin_y           =  auxhist5_begin_y 
 model_config_rec % auxhist5_begin_d           =  auxhist5_begin_d 
 model_config_rec % auxhist5_begin_h           =  auxhist5_begin_h 
 model_config_rec % auxhist5_begin_m           =  auxhist5_begin_m 
 model_config_rec % auxhist5_begin_s           =  auxhist5_begin_s 
 model_config_rec % auxhist5_begin             =  auxhist5_begin 
 model_config_rec % auxhist5_end_y             =  auxhist5_end_y 
 model_config_rec % auxhist5_end_d             =  auxhist5_end_d 
 model_config_rec % auxhist5_end_h             =  auxhist5_end_h 
 model_config_rec % auxhist5_end_m             =  auxhist5_end_m 
 model_config_rec % auxhist5_end_s             =  auxhist5_end_s 
 model_config_rec % auxhist5_end               =  auxhist5_end 
 model_config_rec % io_form_auxhist5           =  io_form_auxhist5 
 model_config_rec % frames_per_auxhist5        =  frames_per_auxhist5 
 model_config_rec % auxhist6_inname            =  auxhist6_inname 
 model_config_rec % auxhist6_outname           =  auxhist6_outname 
 model_config_rec % auxhist6_interval_y        =  auxhist6_interval_y 
 model_config_rec % auxhist6_interval_d        =  auxhist6_interval_d 
 model_config_rec % auxhist6_interval_h        =  auxhist6_interval_h 
 model_config_rec % auxhist6_interval_m        =  auxhist6_interval_m 
 model_config_rec % auxhist6_interval_s        =  auxhist6_interval_s 
 model_config_rec % auxhist6_interval          =  auxhist6_interval 
 model_config_rec % auxhist6_begin_y           =  auxhist6_begin_y 
 model_config_rec % auxhist6_begin_d           =  auxhist6_begin_d 
 model_config_rec % auxhist6_begin_h           =  auxhist6_begin_h 
 model_config_rec % auxhist6_begin_m           =  auxhist6_begin_m 
 model_config_rec % auxhist6_begin_s           =  auxhist6_begin_s 
 model_config_rec % auxhist6_begin             =  auxhist6_begin 
 model_config_rec % auxhist6_end_y             =  auxhist6_end_y 
 model_config_rec % auxhist6_end_d             =  auxhist6_end_d 
 model_config_rec % auxhist6_end_h             =  auxhist6_end_h 
 model_config_rec % auxhist6_end_m             =  auxhist6_end_m 
 model_config_rec % auxhist6_end_s             =  auxhist6_end_s 
 model_config_rec % auxhist6_end               =  auxhist6_end 
 model_config_rec % io_form_auxhist6           =  io_form_auxhist6 
 model_config_rec % frames_per_auxhist6        =  frames_per_auxhist6 
 model_config_rec % auxhist7_inname            =  auxhist7_inname 
 model_config_rec % auxhist7_outname           =  auxhist7_outname 
 model_config_rec % auxhist7_interval_y        =  auxhist7_interval_y 
 model_config_rec % auxhist7_interval_d        =  auxhist7_interval_d 
 model_config_rec % auxhist7_interval_h        =  auxhist7_interval_h 
 model_config_rec % auxhist7_interval_m        =  auxhist7_interval_m 
 model_config_rec % auxhist7_interval_s        =  auxhist7_interval_s 
 model_config_rec % auxhist7_interval          =  auxhist7_interval 
 model_config_rec % auxhist7_begin_y           =  auxhist7_begin_y 
 model_config_rec % auxhist7_begin_d           =  auxhist7_begin_d 
 model_config_rec % auxhist7_begin_h           =  auxhist7_begin_h 
 model_config_rec % auxhist7_begin_m           =  auxhist7_begin_m 
 model_config_rec % auxhist7_begin_s           =  auxhist7_begin_s 
 model_config_rec % auxhist7_begin             =  auxhist7_begin 
 model_config_rec % auxhist7_end_y             =  auxhist7_end_y 
 model_config_rec % auxhist7_end_d             =  auxhist7_end_d 
 model_config_rec % auxhist7_end_h             =  auxhist7_end_h 
 model_config_rec % auxhist7_end_m             =  auxhist7_end_m 
 model_config_rec % auxhist7_end_s             =  auxhist7_end_s 
 model_config_rec % auxhist7_end               =  auxhist7_end 
 model_config_rec % io_form_auxhist7           =  io_form_auxhist7 
 model_config_rec % frames_per_auxhist7        =  frames_per_auxhist7 
 model_config_rec % auxhist8_inname            =  auxhist8_inname 
 model_config_rec % auxhist8_outname           =  auxhist8_outname 
 model_config_rec % auxhist8_interval_y        =  auxhist8_interval_y 
 model_config_rec % auxhist8_interval_d        =  auxhist8_interval_d 
 model_config_rec % auxhist8_interval_h        =  auxhist8_interval_h 
 model_config_rec % auxhist8_interval_m        =  auxhist8_interval_m 
 model_config_rec % auxhist8_interval_s        =  auxhist8_interval_s 
 model_config_rec % auxhist8_interval          =  auxhist8_interval 
 model_config_rec % auxhist8_begin_y           =  auxhist8_begin_y 
 model_config_rec % auxhist8_begin_d           =  auxhist8_begin_d 
 model_config_rec % auxhist8_begin_h           =  auxhist8_begin_h 
 model_config_rec % auxhist8_begin_m           =  auxhist8_begin_m 
 model_config_rec % auxhist8_begin_s           =  auxhist8_begin_s 
 model_config_rec % auxhist8_begin             =  auxhist8_begin 
 model_config_rec % auxhist8_end_y             =  auxhist8_end_y 
 model_config_rec % auxhist8_end_d             =  auxhist8_end_d 
 model_config_rec % auxhist8_end_h             =  auxhist8_end_h 
 model_config_rec % auxhist8_end_m             =  auxhist8_end_m 
 model_config_rec % auxhist8_end_s             =  auxhist8_end_s 
 model_config_rec % auxhist8_end               =  auxhist8_end 
 model_config_rec % io_form_auxhist8           =  io_form_auxhist8 
 model_config_rec % frames_per_auxhist8        =  frames_per_auxhist8 
 model_config_rec % auxhist9_inname            =  auxhist9_inname 
 model_config_rec % auxhist9_outname           =  auxhist9_outname 
 model_config_rec % auxhist9_interval_y        =  auxhist9_interval_y 
 model_config_rec % auxhist9_interval_d        =  auxhist9_interval_d 
 model_config_rec % auxhist9_interval_h        =  auxhist9_interval_h 
 model_config_rec % auxhist9_interval_m        =  auxhist9_interval_m 
 model_config_rec % auxhist9_interval_s        =  auxhist9_interval_s 
 model_config_rec % auxhist9_interval          =  auxhist9_interval 
 model_config_rec % auxhist9_begin_y           =  auxhist9_begin_y 
 model_config_rec % auxhist9_begin_d           =  auxhist9_begin_d 
 model_config_rec % auxhist9_begin_h           =  auxhist9_begin_h 
 model_config_rec % auxhist9_begin_m           =  auxhist9_begin_m 
 model_config_rec % auxhist9_begin_s           =  auxhist9_begin_s 
 model_config_rec % auxhist9_begin             =  auxhist9_begin 
 model_config_rec % auxhist9_end_y             =  auxhist9_end_y 
 model_config_rec % auxhist9_end_d             =  auxhist9_end_d 
 model_config_rec % auxhist9_end_h             =  auxhist9_end_h 
 model_config_rec % auxhist9_end_m             =  auxhist9_end_m 
 model_config_rec % auxhist9_end_s             =  auxhist9_end_s 
 model_config_rec % auxhist9_end               =  auxhist9_end 
 model_config_rec % io_form_auxhist9           =  io_form_auxhist9 
 model_config_rec % frames_per_auxhist9        =  frames_per_auxhist9 
 model_config_rec % auxhist10_inname           =  auxhist10_inname 
 model_config_rec % auxhist10_outname          =  auxhist10_outname 
 model_config_rec % auxhist10_interval_y       =  auxhist10_interval_y 
 model_config_rec % auxhist10_interval_d       =  auxhist10_interval_d 
 model_config_rec % auxhist10_interval_h       =  auxhist10_interval_h 
 model_config_rec % auxhist10_interval_m       =  auxhist10_interval_m 
 model_config_rec % auxhist10_interval_s       =  auxhist10_interval_s 
 model_config_rec % auxhist10_interval         =  auxhist10_interval 
 model_config_rec % auxhist10_begin_y          =  auxhist10_begin_y 
 model_config_rec % auxhist10_begin_d          =  auxhist10_begin_d 
 model_config_rec % auxhist10_begin_h          =  auxhist10_begin_h 
 model_config_rec % auxhist10_begin_m          =  auxhist10_begin_m 
 model_config_rec % auxhist10_begin_s          =  auxhist10_begin_s 
 model_config_rec % auxhist10_begin            =  auxhist10_begin 
 model_config_rec % auxhist10_end_y            =  auxhist10_end_y 
 model_config_rec % auxhist10_end_d            =  auxhist10_end_d 
 model_config_rec % auxhist10_end_h            =  auxhist10_end_h 
 model_config_rec % auxhist10_end_m            =  auxhist10_end_m 
 model_config_rec % auxhist10_end_s            =  auxhist10_end_s 
 model_config_rec % auxhist10_end              =  auxhist10_end 
 model_config_rec % io_form_auxhist10          =  io_form_auxhist10 
 model_config_rec % frames_per_auxhist10       =  frames_per_auxhist10 
 model_config_rec % auxhist11_inname           =  auxhist11_inname 
 model_config_rec % auxhist11_outname          =  auxhist11_outname 
 model_config_rec % auxhist11_interval_y       =  auxhist11_interval_y 
 model_config_rec % auxhist11_interval_d       =  auxhist11_interval_d 
 model_config_rec % auxhist11_interval_h       =  auxhist11_interval_h 
 model_config_rec % auxhist11_interval_m       =  auxhist11_interval_m 
 model_config_rec % auxhist11_interval_s       =  auxhist11_interval_s 
 model_config_rec % auxhist11_interval         =  auxhist11_interval 
 model_config_rec % auxhist11_begin_y          =  auxhist11_begin_y 
 model_config_rec % auxhist11_begin_d          =  auxhist11_begin_d 
 model_config_rec % auxhist11_begin_h          =  auxhist11_begin_h 
 model_config_rec % auxhist11_begin_m          =  auxhist11_begin_m 
 model_config_rec % auxhist11_begin_s          =  auxhist11_begin_s 
 model_config_rec % auxhist11_begin            =  auxhist11_begin 
 model_config_rec % auxhist11_end_y            =  auxhist11_end_y 
 model_config_rec % auxhist11_end_d            =  auxhist11_end_d 
 model_config_rec % auxhist11_end_h            =  auxhist11_end_h 
 model_config_rec % auxhist11_end_m            =  auxhist11_end_m 
 model_config_rec % auxhist11_end_s            =  auxhist11_end_s 
 model_config_rec % auxhist11_end              =  auxhist11_end 
 model_config_rec % io_form_auxhist11          =  io_form_auxhist11 
 model_config_rec % frames_per_auxhist11       =  frames_per_auxhist11 
 model_config_rec % auxhist12_inname           =  auxhist12_inname 
 model_config_rec % auxhist12_outname          =  auxhist12_outname 
 model_config_rec % auxhist12_interval_y       =  auxhist12_interval_y 
 model_config_rec % auxhist12_interval_d       =  auxhist12_interval_d 
 model_config_rec % auxhist12_interval_h       =  auxhist12_interval_h 
 model_config_rec % auxhist12_interval_m       =  auxhist12_interval_m 
 model_config_rec % auxhist12_interval_s       =  auxhist12_interval_s 
 model_config_rec % auxhist12_interval         =  auxhist12_interval 
 model_config_rec % auxhist12_begin_y          =  auxhist12_begin_y 
 model_config_rec % auxhist12_begin_d          =  auxhist12_begin_d 
 model_config_rec % auxhist12_begin_h          =  auxhist12_begin_h 
 model_config_rec % auxhist12_begin_m          =  auxhist12_begin_m 
 model_config_rec % auxhist12_begin_s          =  auxhist12_begin_s 
 model_config_rec % auxhist12_begin            =  auxhist12_begin 
 model_config_rec % auxhist12_end_y            =  auxhist12_end_y 
 model_config_rec % auxhist12_end_d            =  auxhist12_end_d 
 model_config_rec % auxhist12_end_h            =  auxhist12_end_h 
 model_config_rec % auxhist12_end_m            =  auxhist12_end_m 
 model_config_rec % auxhist12_end_s            =  auxhist12_end_s 
 model_config_rec % auxhist12_end              =  auxhist12_end 
 model_config_rec % io_form_auxhist12          =  io_form_auxhist12 
 model_config_rec % frames_per_auxhist12       =  frames_per_auxhist12 
 model_config_rec % auxhist13_inname           =  auxhist13_inname 
 model_config_rec % auxhist13_outname          =  auxhist13_outname 
 model_config_rec % auxhist13_interval_y       =  auxhist13_interval_y 
 model_config_rec % auxhist13_interval_d       =  auxhist13_interval_d 
 model_config_rec % auxhist13_interval_h       =  auxhist13_interval_h 
 model_config_rec % auxhist13_interval_m       =  auxhist13_interval_m 
 model_config_rec % auxhist13_interval_s       =  auxhist13_interval_s 
 model_config_rec % auxhist13_interval         =  auxhist13_interval 
 model_config_rec % auxhist13_begin_y          =  auxhist13_begin_y 
 model_config_rec % auxhist13_begin_d          =  auxhist13_begin_d 
 model_config_rec % auxhist13_begin_h          =  auxhist13_begin_h 
 model_config_rec % auxhist13_begin_m          =  auxhist13_begin_m 
 model_config_rec % auxhist13_begin_s          =  auxhist13_begin_s 
 model_config_rec % auxhist13_begin            =  auxhist13_begin 
 model_config_rec % auxhist13_end_y            =  auxhist13_end_y 
 model_config_rec % auxhist13_end_d            =  auxhist13_end_d 
 model_config_rec % auxhist13_end_h            =  auxhist13_end_h 
 model_config_rec % auxhist13_end_m            =  auxhist13_end_m 
 model_config_rec % auxhist13_end_s            =  auxhist13_end_s 
 model_config_rec % auxhist13_end              =  auxhist13_end 
 model_config_rec % io_form_auxhist13          =  io_form_auxhist13 
 model_config_rec % frames_per_auxhist13       =  frames_per_auxhist13 
 model_config_rec % auxhist14_inname           =  auxhist14_inname 
 model_config_rec % auxhist14_outname          =  auxhist14_outname 
 model_config_rec % auxhist14_interval_y       =  auxhist14_interval_y 
 model_config_rec % auxhist14_interval_d       =  auxhist14_interval_d 
 model_config_rec % auxhist14_interval_h       =  auxhist14_interval_h 
 model_config_rec % auxhist14_interval_m       =  auxhist14_interval_m 
 model_config_rec % auxhist14_interval_s       =  auxhist14_interval_s 
 model_config_rec % auxhist14_interval         =  auxhist14_interval 
 model_config_rec % auxhist14_begin_y          =  auxhist14_begin_y 
 model_config_rec % auxhist14_begin_d          =  auxhist14_begin_d 
 model_config_rec % auxhist14_begin_h          =  auxhist14_begin_h 
 model_config_rec % auxhist14_begin_m          =  auxhist14_begin_m 
 model_config_rec % auxhist14_begin_s          =  auxhist14_begin_s 
 model_config_rec % auxhist14_begin            =  auxhist14_begin 
 model_config_rec % auxhist14_end_y            =  auxhist14_end_y 
 model_config_rec % auxhist14_end_d            =  auxhist14_end_d 
 model_config_rec % auxhist14_end_h            =  auxhist14_end_h 
 model_config_rec % auxhist14_end_m            =  auxhist14_end_m 
 model_config_rec % auxhist14_end_s            =  auxhist14_end_s 
 model_config_rec % auxhist14_end              =  auxhist14_end 
 model_config_rec % io_form_auxhist14          =  io_form_auxhist14 
 model_config_rec % frames_per_auxhist14       =  frames_per_auxhist14 
 model_config_rec % auxhist15_inname           =  auxhist15_inname 
 model_config_rec % auxhist15_outname          =  auxhist15_outname 
 model_config_rec % auxhist15_interval_y       =  auxhist15_interval_y 
 model_config_rec % auxhist15_interval_d       =  auxhist15_interval_d 
 model_config_rec % auxhist15_interval_h       =  auxhist15_interval_h 
 model_config_rec % auxhist15_interval_m       =  auxhist15_interval_m 
 model_config_rec % auxhist15_interval_s       =  auxhist15_interval_s 
 model_config_rec % auxhist15_interval         =  auxhist15_interval 
 model_config_rec % auxhist15_begin_y          =  auxhist15_begin_y 
 model_config_rec % auxhist15_begin_d          =  auxhist15_begin_d 
 model_config_rec % auxhist15_begin_h          =  auxhist15_begin_h 
 model_config_rec % auxhist15_begin_m          =  auxhist15_begin_m 
 model_config_rec % auxhist15_begin_s          =  auxhist15_begin_s 
 model_config_rec % auxhist15_begin            =  auxhist15_begin 
 model_config_rec % auxhist15_end_y            =  auxhist15_end_y 
 model_config_rec % auxhist15_end_d            =  auxhist15_end_d 
 model_config_rec % auxhist15_end_h            =  auxhist15_end_h 
 model_config_rec % auxhist15_end_m            =  auxhist15_end_m 
 model_config_rec % auxhist15_end_s            =  auxhist15_end_s 
 model_config_rec % auxhist15_end              =  auxhist15_end 
 model_config_rec % io_form_auxhist15          =  io_form_auxhist15 
 model_config_rec % frames_per_auxhist15       =  frames_per_auxhist15 
 model_config_rec % auxhist16_inname           =  auxhist16_inname 
 model_config_rec % auxhist16_outname          =  auxhist16_outname 
 model_config_rec % auxhist16_interval_y       =  auxhist16_interval_y 
 model_config_rec % auxhist16_interval_d       =  auxhist16_interval_d 
 model_config_rec % auxhist16_interval_h       =  auxhist16_interval_h 
 model_config_rec % auxhist16_interval_m       =  auxhist16_interval_m 
 model_config_rec % auxhist16_interval_s       =  auxhist16_interval_s 
 model_config_rec % auxhist16_interval         =  auxhist16_interval 
 model_config_rec % auxhist16_begin_y          =  auxhist16_begin_y 
 model_config_rec % auxhist16_begin_d          =  auxhist16_begin_d 
 model_config_rec % auxhist16_begin_h          =  auxhist16_begin_h 
 model_config_rec % auxhist16_begin_m          =  auxhist16_begin_m 
 model_config_rec % auxhist16_begin_s          =  auxhist16_begin_s 
 model_config_rec % auxhist16_begin            =  auxhist16_begin 
 model_config_rec % auxhist16_end_y            =  auxhist16_end_y 
 model_config_rec % auxhist16_end_d            =  auxhist16_end_d 
 model_config_rec % auxhist16_end_h            =  auxhist16_end_h 
 model_config_rec % auxhist16_end_m            =  auxhist16_end_m 
 model_config_rec % auxhist16_end_s            =  auxhist16_end_s 
 model_config_rec % auxhist16_end              =  auxhist16_end 
 model_config_rec % io_form_auxhist16          =  io_form_auxhist16 
 model_config_rec % frames_per_auxhist16       =  frames_per_auxhist16 
 model_config_rec % auxhist17_inname           =  auxhist17_inname 
 model_config_rec % auxhist17_outname          =  auxhist17_outname 
 model_config_rec % auxhist17_interval_y       =  auxhist17_interval_y 
 model_config_rec % auxhist17_interval_d       =  auxhist17_interval_d 
 model_config_rec % auxhist17_interval_h       =  auxhist17_interval_h 
 model_config_rec % auxhist17_interval_m       =  auxhist17_interval_m 
 model_config_rec % auxhist17_interval_s       =  auxhist17_interval_s 
 model_config_rec % auxhist17_interval         =  auxhist17_interval 
 model_config_rec % auxhist17_begin_y          =  auxhist17_begin_y 
 model_config_rec % auxhist17_begin_d          =  auxhist17_begin_d 
 model_config_rec % auxhist17_begin_h          =  auxhist17_begin_h 
 model_config_rec % auxhist17_begin_m          =  auxhist17_begin_m 
 model_config_rec % auxhist17_begin_s          =  auxhist17_begin_s 
 model_config_rec % auxhist17_begin            =  auxhist17_begin 
 model_config_rec % auxhist17_end_y            =  auxhist17_end_y 
 model_config_rec % auxhist17_end_d            =  auxhist17_end_d 
 model_config_rec % auxhist17_end_h            =  auxhist17_end_h 
 model_config_rec % auxhist17_end_m            =  auxhist17_end_m 
 model_config_rec % auxhist17_end_s            =  auxhist17_end_s 
 model_config_rec % auxhist17_end              =  auxhist17_end 
 model_config_rec % io_form_auxhist17          =  io_form_auxhist17 
 model_config_rec % frames_per_auxhist17       =  frames_per_auxhist17 
 model_config_rec % auxhist18_inname           =  auxhist18_inname 
 model_config_rec % auxhist18_outname          =  auxhist18_outname 
 model_config_rec % auxhist18_interval_y       =  auxhist18_interval_y 
 model_config_rec % auxhist18_interval_d       =  auxhist18_interval_d 
 model_config_rec % auxhist18_interval_h       =  auxhist18_interval_h 
 model_config_rec % auxhist18_interval_m       =  auxhist18_interval_m 
 model_config_rec % auxhist18_interval_s       =  auxhist18_interval_s 
 model_config_rec % auxhist18_interval         =  auxhist18_interval 
 model_config_rec % auxhist18_begin_y          =  auxhist18_begin_y 
 model_config_rec % auxhist18_begin_d          =  auxhist18_begin_d 
 model_config_rec % auxhist18_begin_h          =  auxhist18_begin_h 
 model_config_rec % auxhist18_begin_m          =  auxhist18_begin_m 
 model_config_rec % auxhist18_begin_s          =  auxhist18_begin_s 
 model_config_rec % auxhist18_begin            =  auxhist18_begin 
 model_config_rec % auxhist18_end_y            =  auxhist18_end_y 
 model_config_rec % auxhist18_end_d            =  auxhist18_end_d 
 model_config_rec % auxhist18_end_h            =  auxhist18_end_h 
 model_config_rec % auxhist18_end_m            =  auxhist18_end_m 
 model_config_rec % auxhist18_end_s            =  auxhist18_end_s 
 model_config_rec % auxhist18_end              =  auxhist18_end 
 model_config_rec % io_form_auxhist18          =  io_form_auxhist18 
 model_config_rec % frames_per_auxhist18       =  frames_per_auxhist18 
 model_config_rec % auxhist19_inname           =  auxhist19_inname 
 model_config_rec % auxhist19_outname          =  auxhist19_outname 
 model_config_rec % auxhist19_interval_y       =  auxhist19_interval_y 
 model_config_rec % auxhist19_interval_d       =  auxhist19_interval_d 
 model_config_rec % auxhist19_interval_h       =  auxhist19_interval_h 
 model_config_rec % auxhist19_interval_m       =  auxhist19_interval_m 
 model_config_rec % auxhist19_interval_s       =  auxhist19_interval_s 
 model_config_rec % auxhist19_interval         =  auxhist19_interval 
 model_config_rec % auxhist19_begin_y          =  auxhist19_begin_y 
 model_config_rec % auxhist19_begin_d          =  auxhist19_begin_d 
 model_config_rec % auxhist19_begin_h          =  auxhist19_begin_h 
 model_config_rec % auxhist19_begin_m          =  auxhist19_begin_m 
 model_config_rec % auxhist19_begin_s          =  auxhist19_begin_s 
 model_config_rec % auxhist19_begin            =  auxhist19_begin 
 model_config_rec % auxhist19_end_y            =  auxhist19_end_y 
 model_config_rec % auxhist19_end_d            =  auxhist19_end_d 
 model_config_rec % auxhist19_end_h            =  auxhist19_end_h 
 model_config_rec % auxhist19_end_m            =  auxhist19_end_m 
 model_config_rec % auxhist19_end_s            =  auxhist19_end_s 
 model_config_rec % auxhist19_end              =  auxhist19_end 
 model_config_rec % io_form_auxhist19          =  io_form_auxhist19 
 model_config_rec % frames_per_auxhist19       =  frames_per_auxhist19 
 model_config_rec % auxhist20_inname           =  auxhist20_inname 
 model_config_rec % auxhist20_outname          =  auxhist20_outname 
 model_config_rec % auxhist20_interval_y       =  auxhist20_interval_y 
 model_config_rec % auxhist20_interval_d       =  auxhist20_interval_d 
 model_config_rec % auxhist20_interval_h       =  auxhist20_interval_h 
 model_config_rec % auxhist20_interval_m       =  auxhist20_interval_m 
 model_config_rec % auxhist20_interval_s       =  auxhist20_interval_s 
 model_config_rec % auxhist20_interval         =  auxhist20_interval 
 model_config_rec % auxhist20_begin_y          =  auxhist20_begin_y 
 model_config_rec % auxhist20_begin_d          =  auxhist20_begin_d 
 model_config_rec % auxhist20_begin_h          =  auxhist20_begin_h 
 model_config_rec % auxhist20_begin_m          =  auxhist20_begin_m 
 model_config_rec % auxhist20_begin_s          =  auxhist20_begin_s 
 model_config_rec % auxhist20_begin            =  auxhist20_begin 
 model_config_rec % auxhist20_end_y            =  auxhist20_end_y 
 model_config_rec % auxhist20_end_d            =  auxhist20_end_d 
 model_config_rec % auxhist20_end_h            =  auxhist20_end_h 
 model_config_rec % auxhist20_end_m            =  auxhist20_end_m 
 model_config_rec % auxhist20_end_s            =  auxhist20_end_s 
 model_config_rec % auxhist20_end              =  auxhist20_end 
 model_config_rec % io_form_auxhist20          =  io_form_auxhist20 
 model_config_rec % frames_per_auxhist20       =  frames_per_auxhist20 
 model_config_rec % auxhist21_inname           =  auxhist21_inname 
 model_config_rec % auxhist21_outname          =  auxhist21_outname 
 model_config_rec % auxhist21_interval_y       =  auxhist21_interval_y 
 model_config_rec % auxhist21_interval_d       =  auxhist21_interval_d 
 model_config_rec % auxhist21_interval_h       =  auxhist21_interval_h 
 model_config_rec % auxhist21_interval_m       =  auxhist21_interval_m 
 model_config_rec % auxhist21_interval_s       =  auxhist21_interval_s 
 model_config_rec % auxhist21_interval         =  auxhist21_interval 
 model_config_rec % auxhist21_begin_y          =  auxhist21_begin_y 
 model_config_rec % auxhist21_begin_d          =  auxhist21_begin_d 
 model_config_rec % auxhist21_begin_h          =  auxhist21_begin_h 
 model_config_rec % auxhist21_begin_m          =  auxhist21_begin_m 
 model_config_rec % auxhist21_begin_s          =  auxhist21_begin_s 
 model_config_rec % auxhist21_begin            =  auxhist21_begin 
 model_config_rec % auxhist21_end_y            =  auxhist21_end_y 
 model_config_rec % auxhist21_end_d            =  auxhist21_end_d 
 model_config_rec % auxhist21_end_h            =  auxhist21_end_h 
 model_config_rec % auxhist21_end_m            =  auxhist21_end_m 
 model_config_rec % auxhist21_end_s            =  auxhist21_end_s 
 model_config_rec % auxhist21_end              =  auxhist21_end 
 model_config_rec % io_form_auxhist21          =  io_form_auxhist21 
 model_config_rec % frames_per_auxhist21       =  frames_per_auxhist21 
 model_config_rec % auxhist22_inname           =  auxhist22_inname 
 model_config_rec % auxhist22_outname          =  auxhist22_outname 
 model_config_rec % auxhist22_interval_y       =  auxhist22_interval_y 
 model_config_rec % auxhist22_interval_d       =  auxhist22_interval_d 
 model_config_rec % auxhist22_interval_h       =  auxhist22_interval_h 
 model_config_rec % auxhist22_interval_m       =  auxhist22_interval_m 
 model_config_rec % auxhist22_interval_s       =  auxhist22_interval_s 
 model_config_rec % auxhist22_interval         =  auxhist22_interval 
 model_config_rec % auxhist22_begin_y          =  auxhist22_begin_y 
 model_config_rec % auxhist22_begin_d          =  auxhist22_begin_d 
 model_config_rec % auxhist22_begin_h          =  auxhist22_begin_h 
 model_config_rec % auxhist22_begin_m          =  auxhist22_begin_m 
 model_config_rec % auxhist22_begin_s          =  auxhist22_begin_s 
 model_config_rec % auxhist22_begin            =  auxhist22_begin 
 model_config_rec % auxhist22_end_y            =  auxhist22_end_y 
 model_config_rec % auxhist22_end_d            =  auxhist22_end_d 
 model_config_rec % auxhist22_end_h            =  auxhist22_end_h 
 model_config_rec % auxhist22_end_m            =  auxhist22_end_m 
 model_config_rec % auxhist22_end_s            =  auxhist22_end_s 
 model_config_rec % auxhist22_end              =  auxhist22_end 
 model_config_rec % io_form_auxhist22          =  io_form_auxhist22 
 model_config_rec % frames_per_auxhist22       =  frames_per_auxhist22 
 model_config_rec % auxhist23_inname           =  auxhist23_inname 
 model_config_rec % auxhist23_outname          =  auxhist23_outname 
 model_config_rec % auxhist23_interval_y       =  auxhist23_interval_y 
 model_config_rec % auxhist23_interval_d       =  auxhist23_interval_d 
 model_config_rec % auxhist23_interval_h       =  auxhist23_interval_h 
 model_config_rec % auxhist23_interval_m       =  auxhist23_interval_m 
 model_config_rec % auxhist23_interval_s       =  auxhist23_interval_s 
 model_config_rec % auxhist23_interval         =  auxhist23_interval 
 model_config_rec % auxhist23_begin_y          =  auxhist23_begin_y 
 model_config_rec % auxhist23_begin_d          =  auxhist23_begin_d 
 model_config_rec % auxhist23_begin_h          =  auxhist23_begin_h 
 model_config_rec % auxhist23_begin_m          =  auxhist23_begin_m 
 model_config_rec % auxhist23_begin_s          =  auxhist23_begin_s 
 model_config_rec % auxhist23_begin            =  auxhist23_begin 
 model_config_rec % auxhist23_end_y            =  auxhist23_end_y 
 model_config_rec % auxhist23_end_d            =  auxhist23_end_d 
 model_config_rec % auxhist23_end_h            =  auxhist23_end_h 
 model_config_rec % auxhist23_end_m            =  auxhist23_end_m 
 model_config_rec % auxhist23_end_s            =  auxhist23_end_s 
 model_config_rec % auxhist23_end              =  auxhist23_end 
 model_config_rec % io_form_auxhist23          =  io_form_auxhist23 
 model_config_rec % frames_per_auxhist23       =  frames_per_auxhist23 
 model_config_rec % auxhist24_inname           =  auxhist24_inname 
 model_config_rec % auxhist24_outname          =  auxhist24_outname 
 model_config_rec % auxhist24_interval_y       =  auxhist24_interval_y 
 model_config_rec % auxhist24_interval_d       =  auxhist24_interval_d 
 model_config_rec % auxhist24_interval_h       =  auxhist24_interval_h 
 model_config_rec % auxhist24_interval_m       =  auxhist24_interval_m 
 model_config_rec % auxhist24_interval_s       =  auxhist24_interval_s 
 model_config_rec % auxhist24_interval         =  auxhist24_interval 
 model_config_rec % auxhist24_begin_y          =  auxhist24_begin_y 
 model_config_rec % auxhist24_begin_d          =  auxhist24_begin_d 
 model_config_rec % auxhist24_begin_h          =  auxhist24_begin_h 
 model_config_rec % auxhist24_begin_m          =  auxhist24_begin_m 
 model_config_rec % auxhist24_begin_s          =  auxhist24_begin_s 
 model_config_rec % auxhist24_begin            =  auxhist24_begin 
 model_config_rec % auxhist24_end_y            =  auxhist24_end_y 
 model_config_rec % auxhist24_end_d            =  auxhist24_end_d 
 model_config_rec % auxhist24_end_h            =  auxhist24_end_h 
 model_config_rec % auxhist24_end_m            =  auxhist24_end_m 
 model_config_rec % auxhist24_end_s            =  auxhist24_end_s 
 model_config_rec % auxhist24_end              =  auxhist24_end 
 model_config_rec % io_form_auxhist24          =  io_form_auxhist24 
 model_config_rec % frames_per_auxhist24       =  frames_per_auxhist24 
 model_config_rec % auxinput1_outname          =  auxinput1_outname 
 model_config_rec % auxinput1_interval_y       =  auxinput1_interval_y 
 model_config_rec % auxinput1_interval_d       =  auxinput1_interval_d 
 model_config_rec % auxinput1_interval_h       =  auxinput1_interval_h 
 model_config_rec % auxinput1_interval_m       =  auxinput1_interval_m 
 model_config_rec % auxinput1_interval_s       =  auxinput1_interval_s 
 model_config_rec % auxinput1_interval         =  auxinput1_interval 
 model_config_rec % auxinput1_begin_y          =  auxinput1_begin_y 
 model_config_rec % auxinput1_begin_d          =  auxinput1_begin_d 
 model_config_rec % auxinput1_begin_h          =  auxinput1_begin_h 
 model_config_rec % auxinput1_begin_m          =  auxinput1_begin_m 
 model_config_rec % auxinput1_begin_s          =  auxinput1_begin_s 
 model_config_rec % auxinput1_begin            =  auxinput1_begin 
 model_config_rec % auxinput1_end_y            =  auxinput1_end_y 
 model_config_rec % auxinput1_end_d            =  auxinput1_end_d 
 model_config_rec % auxinput1_end_h            =  auxinput1_end_h 
 model_config_rec % auxinput1_end_m            =  auxinput1_end_m 
 model_config_rec % auxinput1_end_s            =  auxinput1_end_s 
 model_config_rec % auxinput1_end              =  auxinput1_end 
 model_config_rec % frames_per_auxinput1       =  frames_per_auxinput1 
 model_config_rec % auxinput2_inname           =  auxinput2_inname 
 model_config_rec % auxinput2_outname          =  auxinput2_outname 
 model_config_rec % auxinput2_interval_y       =  auxinput2_interval_y 
 model_config_rec % auxinput2_interval_d       =  auxinput2_interval_d 
 model_config_rec % auxinput2_interval_h       =  auxinput2_interval_h 
 model_config_rec % auxinput2_interval_m       =  auxinput2_interval_m 
 model_config_rec % auxinput2_interval_s       =  auxinput2_interval_s 
 model_config_rec % auxinput2_interval         =  auxinput2_interval 
 model_config_rec % auxinput2_begin_y          =  auxinput2_begin_y 
 model_config_rec % auxinput2_begin_d          =  auxinput2_begin_d 
 model_config_rec % auxinput2_begin_h          =  auxinput2_begin_h 
 model_config_rec % auxinput2_begin_m          =  auxinput2_begin_m 
 model_config_rec % auxinput2_begin_s          =  auxinput2_begin_s 
 model_config_rec % auxinput2_begin            =  auxinput2_begin 
 model_config_rec % auxinput2_end_y            =  auxinput2_end_y 
 model_config_rec % auxinput2_end_d            =  auxinput2_end_d 
 model_config_rec % auxinput2_end_h            =  auxinput2_end_h 
 model_config_rec % auxinput2_end_m            =  auxinput2_end_m 
 model_config_rec % auxinput2_end_s            =  auxinput2_end_s 
 model_config_rec % auxinput2_end              =  auxinput2_end 
 model_config_rec % frames_per_auxinput2       =  frames_per_auxinput2 
 model_config_rec % auxinput3_inname           =  auxinput3_inname 
 model_config_rec % auxinput3_outname          =  auxinput3_outname 
 model_config_rec % auxinput3_interval_y       =  auxinput3_interval_y 
 model_config_rec % auxinput3_interval_d       =  auxinput3_interval_d 
 model_config_rec % auxinput3_interval_h       =  auxinput3_interval_h 
 model_config_rec % auxinput3_interval_m       =  auxinput3_interval_m 
 model_config_rec % auxinput3_interval_s       =  auxinput3_interval_s 
 model_config_rec % auxinput3_interval         =  auxinput3_interval 
 model_config_rec % auxinput3_begin_y          =  auxinput3_begin_y 
 model_config_rec % auxinput3_begin_d          =  auxinput3_begin_d 
 model_config_rec % auxinput3_begin_h          =  auxinput3_begin_h 
 model_config_rec % auxinput3_begin_m          =  auxinput3_begin_m 
 model_config_rec % auxinput3_begin_s          =  auxinput3_begin_s 
 model_config_rec % auxinput3_begin            =  auxinput3_begin 
 model_config_rec % auxinput3_end_y            =  auxinput3_end_y 
 model_config_rec % auxinput3_end_d            =  auxinput3_end_d 
 model_config_rec % auxinput3_end_h            =  auxinput3_end_h 
 model_config_rec % auxinput3_end_m            =  auxinput3_end_m 
 model_config_rec % auxinput3_end_s            =  auxinput3_end_s 
 model_config_rec % auxinput3_end              =  auxinput3_end 
 model_config_rec % io_form_auxinput3          =  io_form_auxinput3 
 model_config_rec % frames_per_auxinput3       =  frames_per_auxinput3 
 model_config_rec % auxinput4_inname           =  auxinput4_inname 
 model_config_rec % auxinput4_outname          =  auxinput4_outname 
 model_config_rec % auxinput4_interval_y       =  auxinput4_interval_y 
 model_config_rec % auxinput4_interval_d       =  auxinput4_interval_d 
 model_config_rec % auxinput4_interval_h       =  auxinput4_interval_h 
 model_config_rec % auxinput4_interval_m       =  auxinput4_interval_m 
 model_config_rec % auxinput4_interval_s       =  auxinput4_interval_s 
 model_config_rec % auxinput4_interval         =  auxinput4_interval 
 model_config_rec % auxinput4_begin_y          =  auxinput4_begin_y 
 model_config_rec % auxinput4_begin_d          =  auxinput4_begin_d 
 model_config_rec % auxinput4_begin_h          =  auxinput4_begin_h 
 model_config_rec % auxinput4_begin_m          =  auxinput4_begin_m 
 model_config_rec % auxinput4_begin_s          =  auxinput4_begin_s 
 model_config_rec % auxinput4_begin            =  auxinput4_begin 
 model_config_rec % auxinput4_end_y            =  auxinput4_end_y 
 model_config_rec % auxinput4_end_d            =  auxinput4_end_d 
 model_config_rec % auxinput4_end_h            =  auxinput4_end_h 
 model_config_rec % auxinput4_end_m            =  auxinput4_end_m 
 model_config_rec % auxinput4_end_s            =  auxinput4_end_s 
 model_config_rec % auxinput4_end              =  auxinput4_end 
 model_config_rec % io_form_auxinput4          =  io_form_auxinput4 
 model_config_rec % frames_per_auxinput4       =  frames_per_auxinput4 
 model_config_rec % auxinput5_inname           =  auxinput5_inname 
 model_config_rec % auxinput5_outname          =  auxinput5_outname 
 model_config_rec % auxinput5_interval_y       =  auxinput5_interval_y 
 model_config_rec % auxinput5_interval_d       =  auxinput5_interval_d 
 model_config_rec % auxinput5_interval_h       =  auxinput5_interval_h 
 model_config_rec % auxinput5_interval_m       =  auxinput5_interval_m 
 model_config_rec % auxinput5_interval_s       =  auxinput5_interval_s 
 model_config_rec % auxinput5_interval         =  auxinput5_interval 
 model_config_rec % auxinput5_begin_y          =  auxinput5_begin_y 
 model_config_rec % auxinput5_begin_d          =  auxinput5_begin_d 
 model_config_rec % auxinput5_begin_h          =  auxinput5_begin_h 
 model_config_rec % auxinput5_begin_m          =  auxinput5_begin_m 
 model_config_rec % auxinput5_begin_s          =  auxinput5_begin_s 
 model_config_rec % auxinput5_begin            =  auxinput5_begin 
 model_config_rec % auxinput5_end_y            =  auxinput5_end_y 
 model_config_rec % auxinput5_end_d            =  auxinput5_end_d 
 model_config_rec % auxinput5_end_h            =  auxinput5_end_h 
 model_config_rec % auxinput5_end_m            =  auxinput5_end_m 
 model_config_rec % auxinput5_end_s            =  auxinput5_end_s 
 model_config_rec % auxinput5_end              =  auxinput5_end 
 model_config_rec % io_form_auxinput5          =  io_form_auxinput5 
 model_config_rec % frames_per_auxinput5       =  frames_per_auxinput5 
 model_config_rec % auxinput6_inname           =  auxinput6_inname 
 model_config_rec % auxinput6_outname          =  auxinput6_outname 
 model_config_rec % auxinput6_interval_y       =  auxinput6_interval_y 
 model_config_rec % auxinput6_interval_d       =  auxinput6_interval_d 
 model_config_rec % auxinput6_interval_h       =  auxinput6_interval_h 
 model_config_rec % auxinput6_interval_m       =  auxinput6_interval_m 
 model_config_rec % auxinput6_interval_s       =  auxinput6_interval_s 
 model_config_rec % auxinput6_interval         =  auxinput6_interval 
 model_config_rec % auxinput6_begin_y          =  auxinput6_begin_y 
 model_config_rec % auxinput6_begin_d          =  auxinput6_begin_d 
 model_config_rec % auxinput6_begin_h          =  auxinput6_begin_h 
 model_config_rec % auxinput6_begin_m          =  auxinput6_begin_m 
 model_config_rec % auxinput6_begin_s          =  auxinput6_begin_s 
 model_config_rec % auxinput6_begin            =  auxinput6_begin 
 model_config_rec % auxinput6_end_y            =  auxinput6_end_y 
 model_config_rec % auxinput6_end_d            =  auxinput6_end_d 
 model_config_rec % auxinput6_end_h            =  auxinput6_end_h 
 model_config_rec % auxinput6_end_m            =  auxinput6_end_m 
 model_config_rec % auxinput6_end_s            =  auxinput6_end_s 
 model_config_rec % auxinput6_end              =  auxinput6_end 
 model_config_rec % io_form_auxinput6          =  io_form_auxinput6 
 model_config_rec % frames_per_auxinput6       =  frames_per_auxinput6 
 model_config_rec % auxinput7_inname           =  auxinput7_inname 
 model_config_rec % auxinput7_outname          =  auxinput7_outname 
 model_config_rec % auxinput7_interval_y       =  auxinput7_interval_y 
 model_config_rec % auxinput7_interval_d       =  auxinput7_interval_d 
 model_config_rec % auxinput7_interval_h       =  auxinput7_interval_h 
 model_config_rec % auxinput7_interval_m       =  auxinput7_interval_m 
 model_config_rec % auxinput7_interval_s       =  auxinput7_interval_s 
 model_config_rec % auxinput7_interval         =  auxinput7_interval 
 model_config_rec % auxinput7_begin_y          =  auxinput7_begin_y 
 model_config_rec % auxinput7_begin_d          =  auxinput7_begin_d 
 model_config_rec % auxinput7_begin_h          =  auxinput7_begin_h 
 model_config_rec % auxinput7_begin_m          =  auxinput7_begin_m 
 model_config_rec % auxinput7_begin_s          =  auxinput7_begin_s 
 model_config_rec % auxinput7_begin            =  auxinput7_begin 
 model_config_rec % auxinput7_end_y            =  auxinput7_end_y 
 model_config_rec % auxinput7_end_d            =  auxinput7_end_d 
 model_config_rec % auxinput7_end_h            =  auxinput7_end_h 
 model_config_rec % auxinput7_end_m            =  auxinput7_end_m 
 model_config_rec % auxinput7_end_s            =  auxinput7_end_s 
 model_config_rec % auxinput7_end              =  auxinput7_end 
 model_config_rec % io_form_auxinput7          =  io_form_auxinput7 
 model_config_rec % frames_per_auxinput7       =  frames_per_auxinput7 
 model_config_rec % auxinput8_inname           =  auxinput8_inname 
 model_config_rec % auxinput8_outname          =  auxinput8_outname 
 model_config_rec % auxinput8_interval_y       =  auxinput8_interval_y 
 model_config_rec % auxinput8_interval_d       =  auxinput8_interval_d 
 model_config_rec % auxinput8_interval_h       =  auxinput8_interval_h 
 model_config_rec % auxinput8_interval_m       =  auxinput8_interval_m 
 model_config_rec % auxinput8_interval_s       =  auxinput8_interval_s 
 model_config_rec % auxinput8_interval         =  auxinput8_interval 
 model_config_rec % auxinput8_begin_y          =  auxinput8_begin_y 
 model_config_rec % auxinput8_begin_d          =  auxinput8_begin_d 
 model_config_rec % auxinput8_begin_h          =  auxinput8_begin_h 
 model_config_rec % auxinput8_begin_m          =  auxinput8_begin_m 
 model_config_rec % auxinput8_begin_s          =  auxinput8_begin_s 
 model_config_rec % auxinput8_begin            =  auxinput8_begin 
 model_config_rec % auxinput8_end_y            =  auxinput8_end_y 
 model_config_rec % auxinput8_end_d            =  auxinput8_end_d 
 model_config_rec % auxinput8_end_h            =  auxinput8_end_h 
 model_config_rec % auxinput8_end_m            =  auxinput8_end_m 
 model_config_rec % auxinput8_end_s            =  auxinput8_end_s 
 model_config_rec % auxinput8_end              =  auxinput8_end 
 model_config_rec % io_form_auxinput8          =  io_form_auxinput8 
 model_config_rec % frames_per_auxinput8       =  frames_per_auxinput8 
 model_config_rec % auxinput9_inname           =  auxinput9_inname 
 model_config_rec % auxinput9_outname          =  auxinput9_outname 
 model_config_rec % auxinput9_interval_y       =  auxinput9_interval_y 
 model_config_rec % auxinput9_interval_d       =  auxinput9_interval_d 
 model_config_rec % auxinput9_interval_h       =  auxinput9_interval_h 
 model_config_rec % auxinput9_interval_m       =  auxinput9_interval_m 
 model_config_rec % auxinput9_interval_s       =  auxinput9_interval_s 
 model_config_rec % auxinput9_interval         =  auxinput9_interval 
 model_config_rec % auxinput9_begin_y          =  auxinput9_begin_y 
 model_config_rec % auxinput9_begin_d          =  auxinput9_begin_d 
 model_config_rec % auxinput9_begin_h          =  auxinput9_begin_h 
 model_config_rec % auxinput9_begin_m          =  auxinput9_begin_m 
 model_config_rec % auxinput9_begin_s          =  auxinput9_begin_s 
 model_config_rec % auxinput9_begin            =  auxinput9_begin 
 model_config_rec % auxinput9_end_y            =  auxinput9_end_y 
 model_config_rec % auxinput9_end_d            =  auxinput9_end_d 
 model_config_rec % auxinput9_end_h            =  auxinput9_end_h 
 model_config_rec % auxinput9_end_m            =  auxinput9_end_m 
 model_config_rec % auxinput9_end_s            =  auxinput9_end_s 
 model_config_rec % auxinput9_end              =  auxinput9_end 
 model_config_rec % io_form_auxinput9          =  io_form_auxinput9 
 model_config_rec % frames_per_auxinput9       =  frames_per_auxinput9 
 model_config_rec % auxinput10_inname          =  auxinput10_inname 
 model_config_rec % auxinput10_outname         =  auxinput10_outname 
 model_config_rec % auxinput10_interval_y      =  auxinput10_interval_y 
 model_config_rec % auxinput10_interval_d      =  auxinput10_interval_d 
 model_config_rec % auxinput10_interval_h      =  auxinput10_interval_h 
 model_config_rec % auxinput10_interval_m      =  auxinput10_interval_m 
 model_config_rec % auxinput10_interval_s      =  auxinput10_interval_s 
 model_config_rec % auxinput10_interval        =  auxinput10_interval 
 model_config_rec % auxinput10_begin_y         =  auxinput10_begin_y 
 model_config_rec % auxinput10_begin_d         =  auxinput10_begin_d 
 model_config_rec % auxinput10_begin_h         =  auxinput10_begin_h 
 model_config_rec % auxinput10_begin_m         =  auxinput10_begin_m 
 model_config_rec % auxinput10_begin_s         =  auxinput10_begin_s 
 model_config_rec % auxinput10_begin           =  auxinput10_begin 
 model_config_rec % auxinput10_end_y           =  auxinput10_end_y 
 model_config_rec % auxinput10_end_d           =  auxinput10_end_d 
 model_config_rec % auxinput10_end_h           =  auxinput10_end_h 
 model_config_rec % auxinput10_end_m           =  auxinput10_end_m 
 model_config_rec % auxinput10_end_s           =  auxinput10_end_s 
 model_config_rec % auxinput10_end             =  auxinput10_end 
 model_config_rec % io_form_auxinput10         =  io_form_auxinput10 
 model_config_rec % frames_per_auxinput10      =  frames_per_auxinput10 
 model_config_rec % auxinput11_inname          =  auxinput11_inname 
 model_config_rec % auxinput11_outname         =  auxinput11_outname 
 model_config_rec % auxinput11_interval_y      =  auxinput11_interval_y 
 model_config_rec % auxinput11_interval_d      =  auxinput11_interval_d 
 model_config_rec % auxinput11_interval_h      =  auxinput11_interval_h 
 model_config_rec % auxinput11_interval_m      =  auxinput11_interval_m 
 model_config_rec % auxinput11_interval_s      =  auxinput11_interval_s 
 model_config_rec % auxinput11_interval        =  auxinput11_interval 
 model_config_rec % auxinput11_begin_y         =  auxinput11_begin_y 
 model_config_rec % auxinput11_begin_d         =  auxinput11_begin_d 
 model_config_rec % auxinput11_begin_h         =  auxinput11_begin_h 
 model_config_rec % auxinput11_begin_m         =  auxinput11_begin_m 
 model_config_rec % auxinput11_begin_s         =  auxinput11_begin_s 
 model_config_rec % auxinput11_begin           =  auxinput11_begin 
 model_config_rec % auxinput11_end_y           =  auxinput11_end_y 
 model_config_rec % auxinput11_end_d           =  auxinput11_end_d 
 model_config_rec % auxinput11_end_h           =  auxinput11_end_h 
 model_config_rec % auxinput11_end_m           =  auxinput11_end_m 
 model_config_rec % auxinput11_end_s           =  auxinput11_end_s 
 model_config_rec % auxinput11_end             =  auxinput11_end 
 model_config_rec % io_form_auxinput11         =  io_form_auxinput11 
 model_config_rec % frames_per_auxinput11      =  frames_per_auxinput11 
 model_config_rec % auxinput12_inname          =  auxinput12_inname 
 model_config_rec % auxinput12_outname         =  auxinput12_outname 
 model_config_rec % auxinput12_interval_y      =  auxinput12_interval_y 
 model_config_rec % auxinput12_interval_d      =  auxinput12_interval_d 
 model_config_rec % auxinput12_interval_h      =  auxinput12_interval_h 
 model_config_rec % auxinput12_interval_m      =  auxinput12_interval_m 
 model_config_rec % auxinput12_interval_s      =  auxinput12_interval_s 
 model_config_rec % auxinput12_interval        =  auxinput12_interval 
 model_config_rec % auxinput12_begin_y         =  auxinput12_begin_y 
 model_config_rec % auxinput12_begin_d         =  auxinput12_begin_d 
 model_config_rec % auxinput12_begin_h         =  auxinput12_begin_h 
 model_config_rec % auxinput12_begin_m         =  auxinput12_begin_m 
 model_config_rec % auxinput12_begin_s         =  auxinput12_begin_s 
 model_config_rec % auxinput12_begin           =  auxinput12_begin 
 model_config_rec % auxinput12_end_y           =  auxinput12_end_y 
 model_config_rec % auxinput12_end_d           =  auxinput12_end_d 
 model_config_rec % auxinput12_end_h           =  auxinput12_end_h 
 model_config_rec % auxinput12_end_m           =  auxinput12_end_m 
 model_config_rec % auxinput12_end_s           =  auxinput12_end_s 
 model_config_rec % auxinput12_end             =  auxinput12_end 
 model_config_rec % io_form_auxinput12         =  io_form_auxinput12 
 model_config_rec % frames_per_auxinput12      =  frames_per_auxinput12 
 model_config_rec % auxinput13_inname          =  auxinput13_inname 
 model_config_rec % auxinput13_outname         =  auxinput13_outname 
 model_config_rec % auxinput13_interval_y      =  auxinput13_interval_y 
 model_config_rec % auxinput13_interval_d      =  auxinput13_interval_d 
 model_config_rec % auxinput13_interval_h      =  auxinput13_interval_h 
 model_config_rec % auxinput13_interval_m      =  auxinput13_interval_m 
 model_config_rec % auxinput13_interval_s      =  auxinput13_interval_s 
 model_config_rec % auxinput13_interval        =  auxinput13_interval 
 model_config_rec % auxinput13_begin_y         =  auxinput13_begin_y 
 model_config_rec % auxinput13_begin_d         =  auxinput13_begin_d 
 model_config_rec % auxinput13_begin_h         =  auxinput13_begin_h 
 model_config_rec % auxinput13_begin_m         =  auxinput13_begin_m 
 model_config_rec % auxinput13_begin_s         =  auxinput13_begin_s 
 model_config_rec % auxinput13_begin           =  auxinput13_begin 
 model_config_rec % auxinput13_end_y           =  auxinput13_end_y 
 model_config_rec % auxinput13_end_d           =  auxinput13_end_d 
 model_config_rec % auxinput13_end_h           =  auxinput13_end_h 
 model_config_rec % auxinput13_end_m           =  auxinput13_end_m 
 model_config_rec % auxinput13_end_s           =  auxinput13_end_s 
 model_config_rec % auxinput13_end             =  auxinput13_end 
 model_config_rec % io_form_auxinput13         =  io_form_auxinput13 
 model_config_rec % frames_per_auxinput13      =  frames_per_auxinput13 
 model_config_rec % auxinput14_inname          =  auxinput14_inname 
 model_config_rec % auxinput14_outname         =  auxinput14_outname 
 model_config_rec % auxinput14_interval_y      =  auxinput14_interval_y 
 model_config_rec % auxinput14_interval_d      =  auxinput14_interval_d 
 model_config_rec % auxinput14_interval_h      =  auxinput14_interval_h 
 model_config_rec % auxinput14_interval_m      =  auxinput14_interval_m 
 model_config_rec % auxinput14_interval_s      =  auxinput14_interval_s 
 model_config_rec % auxinput14_interval        =  auxinput14_interval 
 model_config_rec % auxinput14_begin_y         =  auxinput14_begin_y 
 model_config_rec % auxinput14_begin_d         =  auxinput14_begin_d 
 model_config_rec % auxinput14_begin_h         =  auxinput14_begin_h 
 model_config_rec % auxinput14_begin_m         =  auxinput14_begin_m 
 model_config_rec % auxinput14_begin_s         =  auxinput14_begin_s 
 model_config_rec % auxinput14_begin           =  auxinput14_begin 
 model_config_rec % auxinput14_end_y           =  auxinput14_end_y 
 model_config_rec % auxinput14_end_d           =  auxinput14_end_d 
 model_config_rec % auxinput14_end_h           =  auxinput14_end_h 
 model_config_rec % auxinput14_end_m           =  auxinput14_end_m 
 model_config_rec % auxinput14_end_s           =  auxinput14_end_s 
 model_config_rec % auxinput14_end             =  auxinput14_end 
 model_config_rec % io_form_auxinput14         =  io_form_auxinput14 
 model_config_rec % frames_per_auxinput14      =  frames_per_auxinput14 
 model_config_rec % auxinput15_inname          =  auxinput15_inname 
 model_config_rec % auxinput15_outname         =  auxinput15_outname 
 model_config_rec % auxinput15_interval_y      =  auxinput15_interval_y 
 model_config_rec % auxinput15_interval_d      =  auxinput15_interval_d 
 model_config_rec % auxinput15_interval_h      =  auxinput15_interval_h 
 model_config_rec % auxinput15_interval_m      =  auxinput15_interval_m 
 model_config_rec % auxinput15_interval_s      =  auxinput15_interval_s 
 model_config_rec % auxinput15_interval        =  auxinput15_interval 
 model_config_rec % auxinput15_begin_y         =  auxinput15_begin_y 
 model_config_rec % auxinput15_begin_d         =  auxinput15_begin_d 
 model_config_rec % auxinput15_begin_h         =  auxinput15_begin_h 
 model_config_rec % auxinput15_begin_m         =  auxinput15_begin_m 
 model_config_rec % auxinput15_begin_s         =  auxinput15_begin_s 
 model_config_rec % auxinput15_begin           =  auxinput15_begin 
 model_config_rec % auxinput15_end_y           =  auxinput15_end_y 
 model_config_rec % auxinput15_end_d           =  auxinput15_end_d 
 model_config_rec % auxinput15_end_h           =  auxinput15_end_h 
 model_config_rec % auxinput15_end_m           =  auxinput15_end_m 
 model_config_rec % auxinput15_end_s           =  auxinput15_end_s 
 model_config_rec % auxinput15_end             =  auxinput15_end 
 model_config_rec % io_form_auxinput15         =  io_form_auxinput15 
 model_config_rec % frames_per_auxinput15      =  frames_per_auxinput15 
 model_config_rec % auxinput16_inname          =  auxinput16_inname 
 model_config_rec % auxinput16_outname         =  auxinput16_outname 
 model_config_rec % auxinput16_interval_y      =  auxinput16_interval_y 
 model_config_rec % auxinput16_interval_d      =  auxinput16_interval_d 
 model_config_rec % auxinput16_interval_h      =  auxinput16_interval_h 
 model_config_rec % auxinput16_interval_m      =  auxinput16_interval_m 
 model_config_rec % auxinput16_interval_s      =  auxinput16_interval_s 
 model_config_rec % auxinput16_interval        =  auxinput16_interval 
 model_config_rec % auxinput16_begin_y         =  auxinput16_begin_y 
 model_config_rec % auxinput16_begin_d         =  auxinput16_begin_d 
 model_config_rec % auxinput16_begin_h         =  auxinput16_begin_h 
 model_config_rec % auxinput16_begin_m         =  auxinput16_begin_m 
 model_config_rec % auxinput16_begin_s         =  auxinput16_begin_s 
 model_config_rec % auxinput16_begin           =  auxinput16_begin 
 model_config_rec % auxinput16_end_y           =  auxinput16_end_y 
 model_config_rec % auxinput16_end_d           =  auxinput16_end_d 
 model_config_rec % auxinput16_end_h           =  auxinput16_end_h 
 model_config_rec % auxinput16_end_m           =  auxinput16_end_m 
 model_config_rec % auxinput16_end_s           =  auxinput16_end_s 
 model_config_rec % auxinput16_end             =  auxinput16_end 
 model_config_rec % io_form_auxinput16         =  io_form_auxinput16 
 model_config_rec % frames_per_auxinput16      =  frames_per_auxinput16 
 model_config_rec % auxinput17_inname          =  auxinput17_inname 
 model_config_rec % auxinput17_outname         =  auxinput17_outname 
 model_config_rec % auxinput17_interval_y      =  auxinput17_interval_y 
 model_config_rec % auxinput17_interval_d      =  auxinput17_interval_d 
 model_config_rec % auxinput17_interval_h      =  auxinput17_interval_h 
 model_config_rec % auxinput17_interval_m      =  auxinput17_interval_m 
 model_config_rec % auxinput17_interval_s      =  auxinput17_interval_s 
 model_config_rec % auxinput17_interval        =  auxinput17_interval 
 model_config_rec % auxinput17_begin_y         =  auxinput17_begin_y 
 model_config_rec % auxinput17_begin_d         =  auxinput17_begin_d 
 model_config_rec % auxinput17_begin_h         =  auxinput17_begin_h 
 model_config_rec % auxinput17_begin_m         =  auxinput17_begin_m 
 model_config_rec % auxinput17_begin_s         =  auxinput17_begin_s 
 model_config_rec % auxinput17_begin           =  auxinput17_begin 
 model_config_rec % auxinput17_end_y           =  auxinput17_end_y 
 model_config_rec % auxinput17_end_d           =  auxinput17_end_d 
 model_config_rec % auxinput17_end_h           =  auxinput17_end_h 
 model_config_rec % auxinput17_end_m           =  auxinput17_end_m 
 model_config_rec % auxinput17_end_s           =  auxinput17_end_s 
 model_config_rec % auxinput17_end             =  auxinput17_end 
 model_config_rec % io_form_auxinput17         =  io_form_auxinput17 
 model_config_rec % frames_per_auxinput17      =  frames_per_auxinput17 
 model_config_rec % auxinput18_inname          =  auxinput18_inname 
 model_config_rec % auxinput18_outname         =  auxinput18_outname 
 model_config_rec % auxinput18_interval_y      =  auxinput18_interval_y 
 model_config_rec % auxinput18_interval_d      =  auxinput18_interval_d 
 model_config_rec % auxinput18_interval_h      =  auxinput18_interval_h 
 model_config_rec % auxinput18_interval_m      =  auxinput18_interval_m 
 model_config_rec % auxinput18_interval_s      =  auxinput18_interval_s 
 model_config_rec % auxinput18_interval        =  auxinput18_interval 
 model_config_rec % auxinput18_begin_y         =  auxinput18_begin_y 
 model_config_rec % auxinput18_begin_d         =  auxinput18_begin_d 
 model_config_rec % auxinput18_begin_h         =  auxinput18_begin_h 
 model_config_rec % auxinput18_begin_m         =  auxinput18_begin_m 
 model_config_rec % auxinput18_begin_s         =  auxinput18_begin_s 
 model_config_rec % auxinput18_begin           =  auxinput18_begin 
 model_config_rec % auxinput18_end_y           =  auxinput18_end_y 
 model_config_rec % auxinput18_end_d           =  auxinput18_end_d 
 model_config_rec % auxinput18_end_h           =  auxinput18_end_h 
 model_config_rec % auxinput18_end_m           =  auxinput18_end_m 
 model_config_rec % auxinput18_end_s           =  auxinput18_end_s 
 model_config_rec % auxinput18_end             =  auxinput18_end 
 model_config_rec % io_form_auxinput18         =  io_form_auxinput18 
 model_config_rec % frames_per_auxinput18      =  frames_per_auxinput18 
 model_config_rec % auxinput19_inname          =  auxinput19_inname 
 model_config_rec % auxinput19_outname         =  auxinput19_outname 
 model_config_rec % auxinput19_interval_y      =  auxinput19_interval_y 
 model_config_rec % auxinput19_interval_d      =  auxinput19_interval_d 
 model_config_rec % auxinput19_interval_h      =  auxinput19_interval_h 
 model_config_rec % auxinput19_interval_m      =  auxinput19_interval_m 
 model_config_rec % auxinput19_interval_s      =  auxinput19_interval_s 
 model_config_rec % auxinput19_interval        =  auxinput19_interval 
 model_config_rec % auxinput19_begin_y         =  auxinput19_begin_y 
 model_config_rec % auxinput19_begin_d         =  auxinput19_begin_d 
 model_config_rec % auxinput19_begin_h         =  auxinput19_begin_h 
 model_config_rec % auxinput19_begin_m         =  auxinput19_begin_m 
 model_config_rec % auxinput19_begin_s         =  auxinput19_begin_s 
 model_config_rec % auxinput19_begin           =  auxinput19_begin 
 model_config_rec % auxinput19_end_y           =  auxinput19_end_y 
 model_config_rec % auxinput19_end_d           =  auxinput19_end_d 
 model_config_rec % auxinput19_end_h           =  auxinput19_end_h 
 model_config_rec % auxinput19_end_m           =  auxinput19_end_m 
 model_config_rec % auxinput19_end_s           =  auxinput19_end_s 
 model_config_rec % auxinput19_end             =  auxinput19_end 
 model_config_rec % io_form_auxinput19         =  io_form_auxinput19 
 model_config_rec % frames_per_auxinput19      =  frames_per_auxinput19 
 model_config_rec % auxinput20_inname          =  auxinput20_inname 
 model_config_rec % auxinput20_outname         =  auxinput20_outname 
 model_config_rec % auxinput20_interval_y      =  auxinput20_interval_y 
 model_config_rec % auxinput20_interval_d      =  auxinput20_interval_d 
 model_config_rec % auxinput20_interval_h      =  auxinput20_interval_h 
 model_config_rec % auxinput20_interval_m      =  auxinput20_interval_m 
 model_config_rec % auxinput20_interval_s      =  auxinput20_interval_s 
 model_config_rec % auxinput20_interval        =  auxinput20_interval 
 model_config_rec % auxinput20_begin_y         =  auxinput20_begin_y 
 model_config_rec % auxinput20_begin_d         =  auxinput20_begin_d 
 model_config_rec % auxinput20_begin_h         =  auxinput20_begin_h 
 model_config_rec % auxinput20_begin_m         =  auxinput20_begin_m 
 model_config_rec % auxinput20_begin_s         =  auxinput20_begin_s 
 model_config_rec % auxinput20_begin           =  auxinput20_begin 
 model_config_rec % auxinput20_end_y           =  auxinput20_end_y 
 model_config_rec % auxinput20_end_d           =  auxinput20_end_d 
 model_config_rec % auxinput20_end_h           =  auxinput20_end_h 
 model_config_rec % auxinput20_end_m           =  auxinput20_end_m 
 model_config_rec % auxinput20_end_s           =  auxinput20_end_s 
 model_config_rec % auxinput20_end             =  auxinput20_end 
 model_config_rec % io_form_auxinput20         =  io_form_auxinput20 
 model_config_rec % frames_per_auxinput20      =  frames_per_auxinput20 
 model_config_rec % auxinput21_inname          =  auxinput21_inname 
 model_config_rec % auxinput21_outname         =  auxinput21_outname 
 model_config_rec % auxinput21_interval_y      =  auxinput21_interval_y 
 model_config_rec % auxinput21_interval_d      =  auxinput21_interval_d 
 model_config_rec % auxinput21_interval_h      =  auxinput21_interval_h 
 model_config_rec % auxinput21_interval_m      =  auxinput21_interval_m 
 model_config_rec % auxinput21_interval_s      =  auxinput21_interval_s 
 model_config_rec % auxinput21_interval        =  auxinput21_interval 
 model_config_rec % auxinput21_begin_y         =  auxinput21_begin_y 
 model_config_rec % auxinput21_begin_d         =  auxinput21_begin_d 
 model_config_rec % auxinput21_begin_h         =  auxinput21_begin_h 
 model_config_rec % auxinput21_begin_m         =  auxinput21_begin_m 
 model_config_rec % auxinput21_begin_s         =  auxinput21_begin_s 
 model_config_rec % auxinput21_begin           =  auxinput21_begin 
 model_config_rec % auxinput21_end_y           =  auxinput21_end_y 
 model_config_rec % auxinput21_end_d           =  auxinput21_end_d 
 model_config_rec % auxinput21_end_h           =  auxinput21_end_h 
 model_config_rec % auxinput21_end_m           =  auxinput21_end_m 
 model_config_rec % auxinput21_end_s           =  auxinput21_end_s 
 model_config_rec % auxinput21_end             =  auxinput21_end 
 model_config_rec % io_form_auxinput21         =  io_form_auxinput21 
 model_config_rec % frames_per_auxinput21      =  frames_per_auxinput21 
 model_config_rec % auxinput22_inname          =  auxinput22_inname 
 model_config_rec % auxinput22_outname         =  auxinput22_outname 
 model_config_rec % auxinput22_interval_y      =  auxinput22_interval_y 
 model_config_rec % auxinput22_interval_d      =  auxinput22_interval_d 
 model_config_rec % auxinput22_interval_h      =  auxinput22_interval_h 
 model_config_rec % auxinput22_interval_m      =  auxinput22_interval_m 
 model_config_rec % auxinput22_interval_s      =  auxinput22_interval_s 
 model_config_rec % auxinput22_interval        =  auxinput22_interval 
 model_config_rec % auxinput22_begin_y         =  auxinput22_begin_y 
 model_config_rec % auxinput22_begin_d         =  auxinput22_begin_d 
 model_config_rec % auxinput22_begin_h         =  auxinput22_begin_h 
 model_config_rec % auxinput22_begin_m         =  auxinput22_begin_m 
 model_config_rec % auxinput22_begin_s         =  auxinput22_begin_s 
 model_config_rec % auxinput22_begin           =  auxinput22_begin 
 model_config_rec % auxinput22_end_y           =  auxinput22_end_y 
 model_config_rec % auxinput22_end_d           =  auxinput22_end_d 
 model_config_rec % auxinput22_end_h           =  auxinput22_end_h 
 model_config_rec % auxinput22_end_m           =  auxinput22_end_m 
 model_config_rec % auxinput22_end_s           =  auxinput22_end_s 
 model_config_rec % auxinput22_end             =  auxinput22_end 
 model_config_rec % io_form_auxinput22         =  io_form_auxinput22 
 model_config_rec % frames_per_auxinput22      =  frames_per_auxinput22 
 model_config_rec % auxinput23_inname          =  auxinput23_inname 
 model_config_rec % auxinput23_outname         =  auxinput23_outname 
 model_config_rec % auxinput23_interval_y      =  auxinput23_interval_y 
 model_config_rec % auxinput23_interval_d      =  auxinput23_interval_d 
 model_config_rec % auxinput23_interval_h      =  auxinput23_interval_h 
 model_config_rec % auxinput23_interval_m      =  auxinput23_interval_m 
 model_config_rec % auxinput23_interval_s      =  auxinput23_interval_s 
 model_config_rec % auxinput23_interval        =  auxinput23_interval 
 model_config_rec % auxinput23_begin_y         =  auxinput23_begin_y 
 model_config_rec % auxinput23_begin_d         =  auxinput23_begin_d 
 model_config_rec % auxinput23_begin_h         =  auxinput23_begin_h 
 model_config_rec % auxinput23_begin_m         =  auxinput23_begin_m 
 model_config_rec % auxinput23_begin_s         =  auxinput23_begin_s 
 model_config_rec % auxinput23_begin           =  auxinput23_begin 
 model_config_rec % auxinput23_end_y           =  auxinput23_end_y 
 model_config_rec % auxinput23_end_d           =  auxinput23_end_d 
 model_config_rec % auxinput23_end_h           =  auxinput23_end_h 
 model_config_rec % auxinput23_end_m           =  auxinput23_end_m 
 model_config_rec % auxinput23_end_s           =  auxinput23_end_s 
 model_config_rec % auxinput23_end             =  auxinput23_end 
 model_config_rec % io_form_auxinput23         =  io_form_auxinput23 
 model_config_rec % frames_per_auxinput23      =  frames_per_auxinput23 
 model_config_rec % auxinput24_inname          =  auxinput24_inname 
 model_config_rec % auxinput24_outname         =  auxinput24_outname 
 model_config_rec % auxinput24_interval_y      =  auxinput24_interval_y 
 model_config_rec % auxinput24_interval_d      =  auxinput24_interval_d 
 model_config_rec % auxinput24_interval_h      =  auxinput24_interval_h 
 model_config_rec % auxinput24_interval_m      =  auxinput24_interval_m 
 model_config_rec % auxinput24_interval_s      =  auxinput24_interval_s 
 model_config_rec % auxinput24_interval        =  auxinput24_interval 
 model_config_rec % auxinput24_begin_y         =  auxinput24_begin_y 
 model_config_rec % auxinput24_begin_d         =  auxinput24_begin_d 
 model_config_rec % auxinput24_begin_h         =  auxinput24_begin_h 
 model_config_rec % auxinput24_begin_m         =  auxinput24_begin_m 
 model_config_rec % auxinput24_begin_s         =  auxinput24_begin_s 
 model_config_rec % auxinput24_begin           =  auxinput24_begin 
 model_config_rec % auxinput24_end_y           =  auxinput24_end_y 
 model_config_rec % auxinput24_end_d           =  auxinput24_end_d 
 model_config_rec % auxinput24_end_h           =  auxinput24_end_h 
 model_config_rec % auxinput24_end_m           =  auxinput24_end_m 
 model_config_rec % auxinput24_end_s           =  auxinput24_end_s 
 model_config_rec % auxinput24_end             =  auxinput24_end 
 model_config_rec % io_form_auxinput24         =  io_form_auxinput24 
 model_config_rec % frames_per_auxinput24      =  frames_per_auxinput24 
 model_config_rec % history_interval           =  history_interval 
 model_config_rec % frames_per_outfile         =  frames_per_outfile 
 model_config_rec % restart                    =  restart 
 model_config_rec % restart_interval           =  restart_interval 
 model_config_rec % io_form_input              =  io_form_input 
 model_config_rec % io_form_history            =  io_form_history 
 model_config_rec % io_form_restart            =  io_form_restart 
 model_config_rec % io_form_boundary           =  io_form_boundary 
 model_config_rec % debug_level                =  debug_level 
 model_config_rec % self_test_domain           =  self_test_domain 
 model_config_rec % history_outname            =  history_outname 
 model_config_rec % history_inname             =  history_inname 
 model_config_rec % use_netcdf_classic         =  use_netcdf_classic 
 model_config_rec % history_interval_d         =  history_interval_d 
 model_config_rec % history_interval_h         =  history_interval_h 
 model_config_rec % history_interval_m         =  history_interval_m 
 model_config_rec % history_interval_s         =  history_interval_s 
 model_config_rec % inputout_interval_d        =  inputout_interval_d 
 model_config_rec % inputout_interval_h        =  inputout_interval_h 
 model_config_rec % inputout_interval_m        =  inputout_interval_m 
 model_config_rec % inputout_interval_s        =  inputout_interval_s 
 model_config_rec % inputout_interval          =  inputout_interval 
 model_config_rec % restart_interval_d         =  restart_interval_d 
 model_config_rec % restart_interval_h         =  restart_interval_h 
 model_config_rec % restart_interval_m         =  restart_interval_m 
 model_config_rec % restart_interval_s         =  restart_interval_s 
 model_config_rec % history_begin_y            =  history_begin_y 
 model_config_rec % history_begin_d            =  history_begin_d 
 model_config_rec % history_begin_h            =  history_begin_h 
 model_config_rec % history_begin_m            =  history_begin_m 
 model_config_rec % history_begin_s            =  history_begin_s 
 model_config_rec % history_begin              =  history_begin 
 model_config_rec % inputout_begin_y           =  inputout_begin_y 
 model_config_rec % inputout_begin_d           =  inputout_begin_d 
 model_config_rec % inputout_begin_h           =  inputout_begin_h 
 model_config_rec % inputout_begin_m           =  inputout_begin_m 
 model_config_rec % inputout_begin_s           =  inputout_begin_s 
 model_config_rec % restart_begin_y            =  restart_begin_y 
 model_config_rec % restart_begin_d            =  restart_begin_d 
 model_config_rec % restart_begin_h            =  restart_begin_h 
 model_config_rec % restart_begin_m            =  restart_begin_m 
 model_config_rec % restart_begin_s            =  restart_begin_s 
 model_config_rec % restart_begin              =  restart_begin 
 model_config_rec % history_end_y              =  history_end_y 
 model_config_rec % history_end_d              =  history_end_d 
 model_config_rec % history_end_h              =  history_end_h 
 model_config_rec % history_end_m              =  history_end_m 
 model_config_rec % history_end_s              =  history_end_s 
 model_config_rec % history_end                =  history_end 
 model_config_rec % inputout_end_y             =  inputout_end_y 
 model_config_rec % inputout_end_d             =  inputout_end_d 
 model_config_rec % inputout_end_h             =  inputout_end_h 
 model_config_rec % inputout_end_m             =  inputout_end_m 
 model_config_rec % inputout_end_s             =  inputout_end_s 
 model_config_rec % simulation_start_year      =  simulation_start_year 
 model_config_rec % simulation_start_month     =  simulation_start_month 
 model_config_rec % simulation_start_day       =  simulation_start_day 
 model_config_rec % simulation_start_hour      =  simulation_start_hour 
 model_config_rec % simulation_start_minute    =  simulation_start_minute 
 model_config_rec % simulation_start_second    =  simulation_start_second 
 model_config_rec % reset_simulation_start     =  reset_simulation_start 
 model_config_rec % sr_x                       =  sr_x 
 model_config_rec % sr_y                       =  sr_y 
 model_config_rec % iofields_filename          =  iofields_filename 
 model_config_rec % ignore_iofields_warning    =  ignore_iofields_warning 
 model_config_rec % ncd_nofill                 =  ncd_nofill 
 model_config_rec % julyr                      =  julyr 
 model_config_rec % julday                     =  julday 
 model_config_rec % gmt                        =  gmt 
 model_config_rec % high_freq_outname          =  high_freq_outname 
 model_config_rec % partial_atcf_outname       =  partial_atcf_outname 
 model_config_rec % input_inname               =  input_inname 
 model_config_rec % input_outname              =  input_outname 
 model_config_rec % bdy_inname                 =  bdy_inname 
 model_config_rec % bdy_outname                =  bdy_outname 
 model_config_rec % rst_inname                 =  rst_inname 
 model_config_rec % rst_outname                =  rst_outname 
 model_config_rec % anl_outname                =  anl_outname 
 model_config_rec % write_input                =  write_input 
 model_config_rec % write_restart_at_0h        =  write_restart_at_0h 
 model_config_rec % write_hist_at_0h_rst       =  write_hist_at_0h_rst 
 model_config_rec % adjust_output_times        =  adjust_output_times 
 model_config_rec % adjust_input_times         =  adjust_input_times 
 model_config_rec % tstart                     =  tstart 
 model_config_rec % nocolons                   =  nocolons 
 model_config_rec % cycling                    =  cycling 
 model_config_rec % output_ready_flag          =  output_ready_flag 
 model_config_rec % dfi_opt                    =  dfi_opt 
 model_config_rec % dfi_savehydmeteors         =  dfi_savehydmeteors 
 model_config_rec % dfi_nfilter                =  dfi_nfilter 
 model_config_rec % dfi_write_filtered_input   =  dfi_write_filtered_input 
 model_config_rec % dfi_write_dfi_history      =  dfi_write_dfi_history 
 model_config_rec % dfi_cutoff_seconds         =  dfi_cutoff_seconds 
 model_config_rec % dfi_time_dim               =  dfi_time_dim 
 model_config_rec % dfi_fwdstop_year           =  dfi_fwdstop_year 
 model_config_rec % dfi_fwdstop_month          =  dfi_fwdstop_month 
 model_config_rec % dfi_fwdstop_day            =  dfi_fwdstop_day 
 model_config_rec % dfi_fwdstop_hour           =  dfi_fwdstop_hour 
 model_config_rec % dfi_fwdstop_minute         =  dfi_fwdstop_minute 
 model_config_rec % dfi_fwdstop_second         =  dfi_fwdstop_second 
 model_config_rec % dfi_bckstop_year           =  dfi_bckstop_year 
 model_config_rec % dfi_bckstop_month          =  dfi_bckstop_month 
 model_config_rec % dfi_bckstop_day            =  dfi_bckstop_day 
 model_config_rec % dfi_bckstop_hour           =  dfi_bckstop_hour 
 model_config_rec % dfi_bckstop_minute         =  dfi_bckstop_minute 
 model_config_rec % dfi_bckstop_second         =  dfi_bckstop_second 
 model_config_rec % time_step                  =  time_step 
 model_config_rec % time_step_fract_num        =  time_step_fract_num 
 model_config_rec % time_step_fract_den        =  time_step_fract_den 
 model_config_rec % time_step_dfi              =  time_step_dfi 
 model_config_rec % max_dom                    =  max_dom 
 model_config_rec % s_we                       =  s_we 
 model_config_rec % e_we                       =  e_we 
 model_config_rec % s_sn                       =  s_sn 
 model_config_rec % e_sn                       =  e_sn 
 model_config_rec % s_vert                     =  s_vert 
 model_config_rec % e_vert                     =  e_vert 
 model_config_rec % num_metgrid_soil_levels    =  num_metgrid_soil_levels 
 model_config_rec % dx                         =  dx 
 model_config_rec % dy                         =  dy 
 model_config_rec % grid_id                    =  grid_id 
 model_config_rec % grid_allowed               =  grid_allowed 
 model_config_rec % parent_id                  =  parent_id 
 model_config_rec % i_parent_start             =  i_parent_start 
 model_config_rec % j_parent_start             =  j_parent_start 
 model_config_rec % parent_grid_ratio          =  parent_grid_ratio 
 model_config_rec % parent_time_step_ratio     =  parent_time_step_ratio 
 model_config_rec % feedback                   =  feedback 
 model_config_rec % smooth_option              =  smooth_option 
 model_config_rec % ztop                       =  ztop 
 model_config_rec % moad_grid_ratio            =  moad_grid_ratio 
 model_config_rec % moad_time_step_ratio       =  moad_time_step_ratio 
 model_config_rec % shw                        =  shw 
 model_config_rec % tile_sz_x                  =  tile_sz_x 
 model_config_rec % tile_sz_y                  =  tile_sz_y 
 model_config_rec % numtiles                   =  numtiles 
 model_config_rec % numtiles_inc               =  numtiles_inc 
 model_config_rec % numtiles_x                 =  numtiles_x 
 model_config_rec % numtiles_y                 =  numtiles_y 
 model_config_rec % tile_strategy              =  tile_strategy 
 model_config_rec % nproc_x                    =  nproc_x 
 model_config_rec % nproc_y                    =  nproc_y 
 model_config_rec % irand                      =  irand 
 model_config_rec % dt                         =  dt 
 model_config_rec % ts_buf_size                =  ts_buf_size 
 model_config_rec % max_ts_locs                =  max_ts_locs 
 model_config_rec % num_moves                  =  num_moves 
 model_config_rec % vortex_interval            =  vortex_interval 
 model_config_rec % corral_dist                =  corral_dist 
 model_config_rec % move_id                    =  move_id 
 model_config_rec % move_interval              =  move_interval 
 model_config_rec % move_cd_x                  =  move_cd_x 
 model_config_rec % move_cd_y                  =  move_cd_y 
 model_config_rec % swap_x                     =  swap_x 
 model_config_rec % swap_y                     =  swap_y 
 model_config_rec % cycle_x                    =  cycle_x 
 model_config_rec % cycle_y                    =  cycle_y 
 model_config_rec % reorder_mesh               =  reorder_mesh 
 model_config_rec % perturb_input              =  perturb_input 
 model_config_rec % eta_levels                 =  eta_levels 
 model_config_rec % ptsgm                      =  ptsgm 
 model_config_rec % num_metgrid_levels         =  num_metgrid_levels 
 model_config_rec % p_top_requested            =  p_top_requested 
 model_config_rec % use_prep_hybrid            =  use_prep_hybrid 
 model_config_rec % force_read_thompson        =  force_read_thompson 
 model_config_rec % write_thompson_tables      =  write_thompson_tables 
 model_config_rec % mp_physics                 =  mp_physics 
 model_config_rec % mommix                     =  mommix 
 model_config_rec % disheat                    =  disheat 
 model_config_rec % do_radar_ref               =  do_radar_ref 
 model_config_rec % compute_radar_ref          =  compute_radar_ref 
 model_config_rec % ra_lw_physics              =  ra_lw_physics 
 model_config_rec % ra_sw_physics              =  ra_sw_physics 
 model_config_rec % radt                       =  radt 
 model_config_rec % sf_sfclay_physics          =  sf_sfclay_physics 
 model_config_rec % sf_surface_physics         =  sf_surface_physics 
 model_config_rec % bl_pbl_physics             =  bl_pbl_physics 
 model_config_rec % ysu_topdown_pblmix         =  ysu_topdown_pblmix 
 model_config_rec % shinhong_tke_diag          =  shinhong_tke_diag 
 model_config_rec % windfarm_opt               =  windfarm_opt 
 model_config_rec % windfarm_ij                =  windfarm_ij 
 model_config_rec % mfshconv                   =  mfshconv 
 model_config_rec % bldt                       =  bldt 
 model_config_rec % cu_physics                 =  cu_physics 
 model_config_rec % shcu_physics               =  shcu_physics 
 model_config_rec % cu_diag                    =  cu_diag 
 model_config_rec % gfs_alpha                  =  gfs_alpha 
 model_config_rec % cudt                       =  cudt 
 model_config_rec % gsmdt                      =  gsmdt 
 model_config_rec % isfflx                     =  isfflx 
 model_config_rec % ideal_xland                =  ideal_xland 
 model_config_rec % ifsnow                     =  ifsnow 
 model_config_rec % icloud                     =  icloud 
 model_config_rec % swrad_scat                 =  swrad_scat 
 model_config_rec % surface_input_source       =  surface_input_source 
 model_config_rec % num_soil_layers            =  num_soil_layers 
 model_config_rec % num_urban_layers           =  num_urban_layers 
 model_config_rec % sf_surface_mosaic          =  sf_surface_mosaic 
 model_config_rec % mosaic_cat                 =  mosaic_cat 
 model_config_rec % mosaic_cat_soil            =  mosaic_cat_soil 
 model_config_rec % num_urban_hi               =  num_urban_hi 
 model_config_rec % mosaic_lu                  =  mosaic_lu 
 model_config_rec % mosaic_soil                =  mosaic_soil 
 model_config_rec % maxiens                    =  maxiens 
 model_config_rec % maxens                     =  maxens 
 model_config_rec % maxens2                    =  maxens2 
 model_config_rec % maxens3                    =  maxens3 
 model_config_rec % ensdim                     =  ensdim 
 model_config_rec % chem_opt                   =  chem_opt 
 model_config_rec % num_land_cat               =  num_land_cat 
 model_config_rec % num_soil_cat               =  num_soil_cat 
 model_config_rec % topo_wind                  =  topo_wind 
 model_config_rec % mp_zero_out                =  mp_zero_out 
 model_config_rec % mp_zero_out_thresh         =  mp_zero_out_thresh 
 model_config_rec % seaice_threshold           =  seaice_threshold 
 model_config_rec % fractional_seaice          =  fractional_seaice 
 model_config_rec % seaice_albedo_opt          =  seaice_albedo_opt 
 model_config_rec % seaice_albedo_default      =  seaice_albedo_default 
 model_config_rec % seaice_snowdepth_opt       =  seaice_snowdepth_opt 
 model_config_rec % seaice_snowdepth_max       =  seaice_snowdepth_max 
 model_config_rec % seaice_snowdepth_min       =  seaice_snowdepth_min 
 model_config_rec % seaice_thickness_opt       =  seaice_thickness_opt 
 model_config_rec % seaice_thickness_default   =  seaice_thickness_default 
 model_config_rec % tice2tsk_if2cold           =  tice2tsk_if2cold 
 model_config_rec % sst_update                 =  sst_update 
 model_config_rec % sf_urban_physics           =  sf_urban_physics 
 model_config_rec % usemonalb                  =  usemonalb 
 model_config_rec % rdmaxalb                   =  rdmaxalb 
 model_config_rec % rdlai2d                    =  rdlai2d 
 model_config_rec % ua_phys                    =  ua_phys 
 model_config_rec % gwd_opt                    =  gwd_opt 
 model_config_rec % iz0tlnd                    =  iz0tlnd 
 model_config_rec % sas_pgcon                  =  sas_pgcon 
 model_config_rec % sas_shal_pgcon             =  sas_shal_pgcon 
 model_config_rec % sas_shal_conv              =  sas_shal_conv 
 model_config_rec % sas_mass_flux              =  sas_mass_flux 
 model_config_rec % var_ric                    =  var_ric 
 model_config_rec % coef_ric_l                 =  coef_ric_l 
 model_config_rec % coef_ric_s                 =  coef_ric_s 
 model_config_rec % random_seed                =  random_seed 
 model_config_rec % icoef_sf                   =  icoef_sf 
 model_config_rec % lcurr_sf                   =  lcurr_sf 
 model_config_rec % ens_random_seed            =  ens_random_seed 
 model_config_rec % pert_sas                   =  pert_sas 
 model_config_rec % pert_pbl                   =  pert_pbl 
 model_config_rec % ens_sasamp                 =  ens_sasamp 
 model_config_rec % ens_pblamp                 =  ens_pblamp 
 model_config_rec % idtad                      =  idtad 
 model_config_rec % nsoil                      =  nsoil 
 model_config_rec % nphs                       =  nphs 
 model_config_rec % ncnvc                      =  ncnvc 
 model_config_rec % nrand                      =  nrand 
 model_config_rec % nrads                      =  nrads 
 model_config_rec % nradl                      =  nradl 
 model_config_rec % tprec                      =  tprec 
 model_config_rec % theat                      =  theat 
 model_config_rec % tclod                      =  tclod 
 model_config_rec % trdsw                      =  trdsw 
 model_config_rec % trdlw                      =  trdlw 
 model_config_rec % tsrfc                      =  tsrfc 
 model_config_rec % pcpflg                     =  pcpflg 
 model_config_rec % sigma                      =  sigma 
 model_config_rec % sfenth                     =  sfenth 
 model_config_rec % co2tf                      =  co2tf 
 model_config_rec % ra_call_offset             =  ra_call_offset 
 model_config_rec % cam_abs_freq_s             =  cam_abs_freq_s 
 model_config_rec % levsiz                     =  levsiz 
 model_config_rec % paerlev                    =  paerlev 
 model_config_rec % cam_abs_dim1               =  cam_abs_dim1 
 model_config_rec % cam_abs_dim2               =  cam_abs_dim2 
 model_config_rec % no_src_types               =  no_src_types 
 model_config_rec % alevsiz                    =  alevsiz 
 model_config_rec % o3input                    =  o3input 
 model_config_rec % aer_opt                    =  aer_opt 
 model_config_rec % cu_rad_feedback            =  cu_rad_feedback 
 model_config_rec % icloud_cu                  =  icloud_cu 
 model_config_rec % h_diff                     =  h_diff 
 model_config_rec % movemin                    =  movemin 
 model_config_rec % num_snso_layers            =  num_snso_layers 
 model_config_rec % num_snow_layers            =  num_snow_layers 
 model_config_rec % use_aero_icbc              =  use_aero_icbc 
 model_config_rec % ccn_conc                   =  ccn_conc 
 model_config_rec % hail_opt                   =  hail_opt 
 model_config_rec % sf_lake_physics            =  sf_lake_physics 
 model_config_rec % dyn_opt                    =  dyn_opt 
 model_config_rec % rk_ord                     =  rk_ord 
 model_config_rec % w_damping                  =  w_damping 
 model_config_rec % diff_opt                   =  diff_opt 
 model_config_rec % km_opt                     =  km_opt 
 model_config_rec % damp_opt                   =  damp_opt 
 model_config_rec % zdamp                      =  zdamp 
 model_config_rec % base_pres                  =  base_pres 
 model_config_rec % base_temp                  =  base_temp 
 model_config_rec % base_lapse                 =  base_lapse 
 model_config_rec % iso_temp                   =  iso_temp 
 model_config_rec % dampcoef                   =  dampcoef 
 model_config_rec % khdif                      =  khdif 
 model_config_rec % kvdif                      =  kvdif 
 model_config_rec % c_s                        =  c_s 
 model_config_rec % c_k                        =  c_k 
 model_config_rec % smdiv                      =  smdiv 
 model_config_rec % emdiv                      =  emdiv 
 model_config_rec % epssm                      =  epssm 
 model_config_rec % non_hydrostatic            =  non_hydrostatic 
 model_config_rec % time_step_sound            =  time_step_sound 
 model_config_rec % h_mom_adv_order            =  h_mom_adv_order 
 model_config_rec % v_mom_adv_order            =  v_mom_adv_order 
 model_config_rec % h_sca_adv_order            =  h_sca_adv_order 
 model_config_rec % v_sca_adv_order            =  v_sca_adv_order 
 model_config_rec % top_radiation              =  top_radiation 
 model_config_rec % tke_upper_bound            =  tke_upper_bound 
 model_config_rec % tke_drag_coefficient       =  tke_drag_coefficient 
 model_config_rec % tke_heat_flux              =  tke_heat_flux 
 model_config_rec % pert_coriolis              =  pert_coriolis 
 model_config_rec % euler_adv                  =  euler_adv 
 model_config_rec % idtadt                     =  idtadt 
 model_config_rec % idtadc                     =  idtadc 
 model_config_rec % codamp                     =  codamp 
 model_config_rec % coac                       =  coac 
 model_config_rec % slophc                     =  slophc 
 model_config_rec % wp                         =  wp 
 model_config_rec % terrain_smoothing          =  terrain_smoothing 
 model_config_rec % spec_bdy_width             =  spec_bdy_width 
 model_config_rec % spec_zone                  =  spec_zone 
 model_config_rec % relax_zone                 =  relax_zone 
 model_config_rec % specified                  =  specified 
 model_config_rec % periodic_x                 =  periodic_x 
 model_config_rec % symmetric_xs               =  symmetric_xs 
 model_config_rec % symmetric_xe               =  symmetric_xe 
 model_config_rec % open_xs                    =  open_xs 
 model_config_rec % open_xe                    =  open_xe 
 model_config_rec % periodic_y                 =  periodic_y 
 model_config_rec % symmetric_ys               =  symmetric_ys 
 model_config_rec % symmetric_ye               =  symmetric_ye 
 model_config_rec % open_ys                    =  open_ys 
 model_config_rec % open_ye                    =  open_ye 
 model_config_rec % polar                      =  polar 
 model_config_rec % nested                     =  nested 
 model_config_rec % real_data_init_type        =  real_data_init_type 
 model_config_rec % background_proc_id         =  background_proc_id 
 model_config_rec % forecast_proc_id           =  forecast_proc_id 
 model_config_rec % production_status          =  production_status 
 model_config_rec % compression                =  compression 
 model_config_rec % cen_lat                    =  cen_lat 
 model_config_rec % cen_lon                    =  cen_lon 
 model_config_rec % truelat1                   =  truelat1 
 model_config_rec % truelat2                   =  truelat2 
 model_config_rec % moad_cen_lat               =  moad_cen_lat 
 model_config_rec % stand_lon                  =  stand_lon 
 model_config_rec % flag_metgrid               =  flag_metgrid 
 model_config_rec % flag_snow                  =  flag_snow 
 model_config_rec % flag_psfc                  =  flag_psfc 
 model_config_rec % flag_sm000010              =  flag_sm000010 
 model_config_rec % flag_sm010040              =  flag_sm010040 
 model_config_rec % flag_sm040100              =  flag_sm040100 
 model_config_rec % flag_sm100200              =  flag_sm100200 
 model_config_rec % flag_st000010              =  flag_st000010 
 model_config_rec % flag_st010040              =  flag_st010040 
 model_config_rec % flag_st040100              =  flag_st040100 
 model_config_rec % flag_st100200              =  flag_st100200 
 model_config_rec % flag_slp                   =  flag_slp 
 model_config_rec % flag_soilhgt               =  flag_soilhgt 
 model_config_rec % flag_mf_xy                 =  flag_mf_xy 
 model_config_rec % bdyfrq                     =  bdyfrq 
 model_config_rec % mminlu                     =  mminlu 
 model_config_rec % iswater                    =  iswater 
 model_config_rec % islake                     =  islake 
 model_config_rec % isice                      =  isice 
 model_config_rec % isurban                    =  isurban 
 model_config_rec % isoilwater                 =  isoilwater 
 model_config_rec % map_proj                   =  map_proj 
 model_config_rec % dfi_stage                  =  dfi_stage 
 model_config_rec % mp_physics_dfi             =  mp_physics_dfi 
 model_config_rec % maxpatch                   =  maxpatch 



      CLOSE ( UNIT = nml_read_unit , IOSTAT = io_status )

      IF ( io_status .NE. 0 ) THEN
        CALL wrf_error_fatal3("<stdin>",9347,&
'ERROR CLOSING namelist.input' )
      ENDIF

      CLOSE ( UNIT = nml_write_unit , IOSTAT = io_status )

      IF ( io_status .NE. 0 ) THEN
        CALL wrf_error_fatal3("<stdin>",9354,&
'ERROR CLOSING namelist.output' )
      ENDIF


      RETURN

   END SUBROUTINE initial_config

   SUBROUTINE get_config_as_buffer( buffer, buflen, ncopied )

      INTEGER,   INTENT(INOUT) ::  buffer(*)
      INTEGER,   INTENT(IN)    ::  buflen
      INTEGER,   INTENT(OUT)   ::  ncopied

      INTEGER :: nbytes
      CALL wrf_num_bytes_between ( model_config_rec%last_item_in_struct ,   &
                                   model_config_rec%first_item_in_struct ,  &
                                   nbytes )


      IF ( nbytes .gt. buflen ) THEN
        CALL wrf_error_fatal3("<stdin>",9376,&
        "get_config_rec_as_buffer: buffer size too small for config_rec" )
      ENDIF
      CALL wrf_mem_copy( model_config_rec, buffer, nbytes )
      ncopied = nbytes
      RETURN
   END SUBROUTINE get_config_as_buffer

   SUBROUTINE set_config_as_buffer( buffer, buflen )

      INTEGER,   INTENT(INOUT) ::  buffer(*)
      INTEGER,   INTENT(IN)    ::  buflen

      INTEGER :: nbytes
      CALL wrf_num_bytes_between ( model_config_rec%last_item_in_struct ,  &
                                   model_config_rec%first_item_in_struct , &
                                   nbytes )


      IF ( nbytes .gt. buflen ) THEN
        CALL wrf_error_fatal3("<stdin>",9396,&
        "set_config_rec_as_buffer: buffer length too small to fill model config record" )
      ENDIF
      CALL wrf_mem_copy( buffer, model_config_rec, nbytes )
      RETURN
   END SUBROUTINE set_config_as_buffer

   SUBROUTINE model_to_grid_config_rec ( id_id , model_config_rec , grid_config_rec )
      INTEGER , INTENT(IN)                         ::  id_id
      TYPE ( model_config_rec_type ) , INTENT(IN)  ::  model_config_rec
      TYPE ( grid_config_rec_type  ) , INTENT(OUT) ::  grid_config_rec


































 grid_config_rec % lakedepth_default          = model_config_rec % lakedepth_default (id_id)
 grid_config_rec % lake_min_elev              = model_config_rec % lake_min_elev (id_id)
 grid_config_rec % use_lakedepth              = model_config_rec % use_lakedepth (id_id)
 grid_config_rec % halo_debug                 = model_config_rec % halo_debug 
 grid_config_rec % ntracers                   = model_config_rec % ntracers 
 grid_config_rec % vortex_tracker             = model_config_rec % vortex_tracker (id_id)
 grid_config_rec % interest_rad_storm         = model_config_rec % interest_rad_storm (id_id)
 grid_config_rec % interest_rad_parent        = model_config_rec % interest_rad_parent (id_id)
 grid_config_rec % interest_rad_self          = model_config_rec % interest_rad_self (id_id)
 grid_config_rec % interest_kids              = model_config_rec % interest_kids (id_id)
 grid_config_rec % interest_self              = model_config_rec % interest_self (id_id)
 grid_config_rec % interest_storms            = model_config_rec % interest_storms (id_id)
 grid_config_rec % swath_mode                 = model_config_rec % swath_mode 
 grid_config_rec % num_old_fixes              = model_config_rec % num_old_fixes 
 grid_config_rec % vt4_radius                 = model_config_rec % vt4_radius (id_id)
 grid_config_rec % vt4_weightexp              = model_config_rec % vt4_weightexp (id_id)
 grid_config_rec % vt4_pmax                   = model_config_rec % vt4_pmax (id_id)
 grid_config_rec % vt4_noise_pmax             = model_config_rec % vt4_noise_pmax (id_id)
 grid_config_rec % vt4_noise_pmin             = model_config_rec % vt4_noise_pmin (id_id)
 grid_config_rec % vt4_noise_dpdr             = model_config_rec % vt4_noise_dpdr (id_id)
 grid_config_rec % vt4_noise_iter             = model_config_rec % vt4_noise_iter (id_id)
 grid_config_rec % nomove_freq                = model_config_rec % nomove_freq (id_id)
 grid_config_rec % coral_x                    = model_config_rec % coral_x (id_id)
 grid_config_rec % coral_y                    = model_config_rec % coral_y (id_id)
 grid_config_rec % tg_reset_stream            = model_config_rec % tg_reset_stream 
 grid_config_rec % tg_option                  = model_config_rec % tg_option 
 grid_config_rec % ntornado                   = model_config_rec % ntornado (id_id)
 grid_config_rec % wbd0                       = model_config_rec % wbd0 (id_id)
 grid_config_rec % sbd0                       = model_config_rec % sbd0 (id_id)
 grid_config_rec % analysis                   = model_config_rec % analysis (id_id)
 grid_config_rec % write_analysis             = model_config_rec % write_analysis (id_id)
 grid_config_rec % io_form_auxinput2          = model_config_rec % io_form_auxinput2 
 grid_config_rec % high_freq                  = model_config_rec % high_freq 
 grid_config_rec % high_dom                   = model_config_rec % high_dom 
 grid_config_rec % swint_opt                  = model_config_rec % swint_opt 
 grid_config_rec % aer_type                   = model_config_rec % aer_type (id_id)
 grid_config_rec % aer_aod550_opt             = model_config_rec % aer_aod550_opt (id_id)
 grid_config_rec % aer_angexp_opt             = model_config_rec % aer_angexp_opt (id_id)
 grid_config_rec % aer_ssa_opt                = model_config_rec % aer_ssa_opt (id_id)
 grid_config_rec % aer_asy_opt                = model_config_rec % aer_asy_opt (id_id)
 grid_config_rec % aer_aod550_val             = model_config_rec % aer_aod550_val (id_id)
 grid_config_rec % aer_angexp_val             = model_config_rec % aer_angexp_val (id_id)
 grid_config_rec % aer_ssa_val                = model_config_rec % aer_ssa_val (id_id)
 grid_config_rec % aer_asy_val                = model_config_rec % aer_asy_val (id_id)
 grid_config_rec % dveg                       = model_config_rec % dveg 
 grid_config_rec % opt_crs                    = model_config_rec % opt_crs 
 grid_config_rec % opt_btr                    = model_config_rec % opt_btr 
 grid_config_rec % opt_run                    = model_config_rec % opt_run 
 grid_config_rec % opt_sfc                    = model_config_rec % opt_sfc 
 grid_config_rec % opt_frz                    = model_config_rec % opt_frz 
 grid_config_rec % opt_inf                    = model_config_rec % opt_inf 
 grid_config_rec % opt_rad                    = model_config_rec % opt_rad 
 grid_config_rec % opt_alb                    = model_config_rec % opt_alb 
 grid_config_rec % opt_snf                    = model_config_rec % opt_snf 
 grid_config_rec % opt_tbot                   = model_config_rec % opt_tbot 
 grid_config_rec % opt_stc                    = model_config_rec % opt_stc 
 grid_config_rec % wrf_hydro                  = model_config_rec % wrf_hydro 
 grid_config_rec % run_days                   = model_config_rec % run_days 
 grid_config_rec % run_hours                  = model_config_rec % run_hours 
 grid_config_rec % run_minutes                = model_config_rec % run_minutes 
 grid_config_rec % run_seconds                = model_config_rec % run_seconds 
 grid_config_rec % start_year                 = model_config_rec % start_year (id_id)
 grid_config_rec % start_month                = model_config_rec % start_month (id_id)
 grid_config_rec % start_day                  = model_config_rec % start_day (id_id)
 grid_config_rec % start_hour                 = model_config_rec % start_hour (id_id)
 grid_config_rec % start_minute               = model_config_rec % start_minute (id_id)
 grid_config_rec % start_second               = model_config_rec % start_second (id_id)
 grid_config_rec % end_year                   = model_config_rec % end_year (id_id)
 grid_config_rec % end_month                  = model_config_rec % end_month (id_id)
 grid_config_rec % end_day                    = model_config_rec % end_day (id_id)
 grid_config_rec % end_hour                   = model_config_rec % end_hour (id_id)
 grid_config_rec % end_minute                 = model_config_rec % end_minute (id_id)
 grid_config_rec % end_second                 = model_config_rec % end_second (id_id)
 grid_config_rec % interval_seconds           = model_config_rec % interval_seconds 
 grid_config_rec % input_from_file            = model_config_rec % input_from_file (id_id)
 grid_config_rec % fine_input_stream          = model_config_rec % fine_input_stream (id_id)
 grid_config_rec % auxinput1_inname           = model_config_rec % auxinput1_inname 
 grid_config_rec % io_form_auxinput1          = model_config_rec % io_form_auxinput1 
 grid_config_rec % override_restart_timers    = model_config_rec % override_restart_timers 
 grid_config_rec % auxhist1_inname            = model_config_rec % auxhist1_inname 
 grid_config_rec % auxhist1_outname           = model_config_rec % auxhist1_outname 
 grid_config_rec % auxhist1_interval_y        = model_config_rec % auxhist1_interval_y (id_id)
 grid_config_rec % auxhist1_interval_d        = model_config_rec % auxhist1_interval_d (id_id)
 grid_config_rec % auxhist1_interval_h        = model_config_rec % auxhist1_interval_h (id_id)
 grid_config_rec % auxhist1_interval_m        = model_config_rec % auxhist1_interval_m (id_id)
 grid_config_rec % auxhist1_interval_s        = model_config_rec % auxhist1_interval_s (id_id)
 grid_config_rec % auxhist1_interval          = model_config_rec % auxhist1_interval (id_id)
 grid_config_rec % auxhist1_begin_y           = model_config_rec % auxhist1_begin_y (id_id)
 grid_config_rec % auxhist1_begin_d           = model_config_rec % auxhist1_begin_d (id_id)
 grid_config_rec % auxhist1_begin_h           = model_config_rec % auxhist1_begin_h (id_id)
 grid_config_rec % auxhist1_begin_m           = model_config_rec % auxhist1_begin_m (id_id)
 grid_config_rec % auxhist1_begin_s           = model_config_rec % auxhist1_begin_s (id_id)
 grid_config_rec % auxhist1_begin             = model_config_rec % auxhist1_begin (id_id)
 grid_config_rec % auxhist1_end_y             = model_config_rec % auxhist1_end_y (id_id)
 grid_config_rec % auxhist1_end_d             = model_config_rec % auxhist1_end_d (id_id)
 grid_config_rec % auxhist1_end_h             = model_config_rec % auxhist1_end_h (id_id)
 grid_config_rec % auxhist1_end_m             = model_config_rec % auxhist1_end_m (id_id)
 grid_config_rec % auxhist1_end_s             = model_config_rec % auxhist1_end_s (id_id)
 grid_config_rec % auxhist1_end               = model_config_rec % auxhist1_end (id_id)
 grid_config_rec % io_form_auxhist1           = model_config_rec % io_form_auxhist1 
 grid_config_rec % frames_per_auxhist1        = model_config_rec % frames_per_auxhist1 (id_id)
 grid_config_rec % auxhist2_inname            = model_config_rec % auxhist2_inname 
 grid_config_rec % auxhist2_outname           = model_config_rec % auxhist2_outname 
 grid_config_rec % auxhist2_interval_y        = model_config_rec % auxhist2_interval_y (id_id)
 grid_config_rec % auxhist2_interval_d        = model_config_rec % auxhist2_interval_d (id_id)
 grid_config_rec % auxhist2_interval_h        = model_config_rec % auxhist2_interval_h (id_id)
 grid_config_rec % auxhist2_interval_m        = model_config_rec % auxhist2_interval_m (id_id)
 grid_config_rec % auxhist2_interval_s        = model_config_rec % auxhist2_interval_s (id_id)
 grid_config_rec % auxhist2_interval          = model_config_rec % auxhist2_interval (id_id)
 grid_config_rec % auxhist2_begin_y           = model_config_rec % auxhist2_begin_y (id_id)
 grid_config_rec % auxhist2_begin_d           = model_config_rec % auxhist2_begin_d (id_id)
 grid_config_rec % auxhist2_begin_h           = model_config_rec % auxhist2_begin_h (id_id)
 grid_config_rec % auxhist2_begin_m           = model_config_rec % auxhist2_begin_m (id_id)
 grid_config_rec % auxhist2_begin_s           = model_config_rec % auxhist2_begin_s (id_id)
 grid_config_rec % auxhist2_begin             = model_config_rec % auxhist2_begin (id_id)
 grid_config_rec % auxhist2_end_y             = model_config_rec % auxhist2_end_y (id_id)
 grid_config_rec % auxhist2_end_d             = model_config_rec % auxhist2_end_d (id_id)
 grid_config_rec % auxhist2_end_h             = model_config_rec % auxhist2_end_h (id_id)
 grid_config_rec % auxhist2_end_m             = model_config_rec % auxhist2_end_m (id_id)
 grid_config_rec % auxhist2_end_s             = model_config_rec % auxhist2_end_s (id_id)
 grid_config_rec % auxhist2_end               = model_config_rec % auxhist2_end (id_id)
 grid_config_rec % io_form_auxhist2           = model_config_rec % io_form_auxhist2 
 grid_config_rec % frames_per_auxhist2        = model_config_rec % frames_per_auxhist2 (id_id)
 grid_config_rec % auxhist3_inname            = model_config_rec % auxhist3_inname 
 grid_config_rec % auxhist3_outname           = model_config_rec % auxhist3_outname 
 grid_config_rec % auxhist3_interval_y        = model_config_rec % auxhist3_interval_y (id_id)
 grid_config_rec % auxhist3_interval_d        = model_config_rec % auxhist3_interval_d (id_id)
 grid_config_rec % auxhist3_interval_h        = model_config_rec % auxhist3_interval_h (id_id)
 grid_config_rec % auxhist3_interval_m        = model_config_rec % auxhist3_interval_m (id_id)
 grid_config_rec % auxhist3_interval_s        = model_config_rec % auxhist3_interval_s (id_id)
 grid_config_rec % auxhist3_interval          = model_config_rec % auxhist3_interval (id_id)
 grid_config_rec % auxhist3_begin_y           = model_config_rec % auxhist3_begin_y (id_id)
 grid_config_rec % auxhist3_begin_d           = model_config_rec % auxhist3_begin_d (id_id)
 grid_config_rec % auxhist3_begin_h           = model_config_rec % auxhist3_begin_h (id_id)
 grid_config_rec % auxhist3_begin_m           = model_config_rec % auxhist3_begin_m (id_id)
 grid_config_rec % auxhist3_begin_s           = model_config_rec % auxhist3_begin_s (id_id)
 grid_config_rec % auxhist3_begin             = model_config_rec % auxhist3_begin (id_id)
 grid_config_rec % auxhist3_end_y             = model_config_rec % auxhist3_end_y (id_id)
 grid_config_rec % auxhist3_end_d             = model_config_rec % auxhist3_end_d (id_id)
 grid_config_rec % auxhist3_end_h             = model_config_rec % auxhist3_end_h (id_id)
 grid_config_rec % auxhist3_end_m             = model_config_rec % auxhist3_end_m (id_id)
 grid_config_rec % auxhist3_end_s             = model_config_rec % auxhist3_end_s (id_id)
 grid_config_rec % auxhist3_end               = model_config_rec % auxhist3_end (id_id)
 grid_config_rec % io_form_auxhist3           = model_config_rec % io_form_auxhist3 
 grid_config_rec % frames_per_auxhist3        = model_config_rec % frames_per_auxhist3 (id_id)
 grid_config_rec % auxhist4_inname            = model_config_rec % auxhist4_inname 
 grid_config_rec % auxhist4_outname           = model_config_rec % auxhist4_outname 
 grid_config_rec % auxhist4_interval_y        = model_config_rec % auxhist4_interval_y (id_id)
 grid_config_rec % auxhist4_interval_d        = model_config_rec % auxhist4_interval_d (id_id)
 grid_config_rec % auxhist4_interval_h        = model_config_rec % auxhist4_interval_h (id_id)
 grid_config_rec % auxhist4_interval_m        = model_config_rec % auxhist4_interval_m (id_id)
 grid_config_rec % auxhist4_interval_s        = model_config_rec % auxhist4_interval_s (id_id)
 grid_config_rec % auxhist4_interval          = model_config_rec % auxhist4_interval (id_id)
 grid_config_rec % auxhist4_begin_y           = model_config_rec % auxhist4_begin_y (id_id)
 grid_config_rec % auxhist4_begin_d           = model_config_rec % auxhist4_begin_d (id_id)
 grid_config_rec % auxhist4_begin_h           = model_config_rec % auxhist4_begin_h (id_id)
 grid_config_rec % auxhist4_begin_m           = model_config_rec % auxhist4_begin_m (id_id)
 grid_config_rec % auxhist4_begin_s           = model_config_rec % auxhist4_begin_s (id_id)
 grid_config_rec % auxhist4_begin             = model_config_rec % auxhist4_begin (id_id)
 grid_config_rec % auxhist4_end_y             = model_config_rec % auxhist4_end_y (id_id)
 grid_config_rec % auxhist4_end_d             = model_config_rec % auxhist4_end_d (id_id)
 grid_config_rec % auxhist4_end_h             = model_config_rec % auxhist4_end_h (id_id)
 grid_config_rec % auxhist4_end_m             = model_config_rec % auxhist4_end_m (id_id)
 grid_config_rec % auxhist4_end_s             = model_config_rec % auxhist4_end_s (id_id)
 grid_config_rec % auxhist4_end               = model_config_rec % auxhist4_end (id_id)
 grid_config_rec % io_form_auxhist4           = model_config_rec % io_form_auxhist4 
 grid_config_rec % frames_per_auxhist4        = model_config_rec % frames_per_auxhist4 (id_id)
 grid_config_rec % auxhist5_inname            = model_config_rec % auxhist5_inname 
 grid_config_rec % auxhist5_outname           = model_config_rec % auxhist5_outname 
 grid_config_rec % auxhist5_interval_y        = model_config_rec % auxhist5_interval_y (id_id)
 grid_config_rec % auxhist5_interval_d        = model_config_rec % auxhist5_interval_d (id_id)
 grid_config_rec % auxhist5_interval_h        = model_config_rec % auxhist5_interval_h (id_id)
 grid_config_rec % auxhist5_interval_m        = model_config_rec % auxhist5_interval_m (id_id)
 grid_config_rec % auxhist5_interval_s        = model_config_rec % auxhist5_interval_s (id_id)
 grid_config_rec % auxhist5_interval          = model_config_rec % auxhist5_interval (id_id)
 grid_config_rec % auxhist5_begin_y           = model_config_rec % auxhist5_begin_y (id_id)
 grid_config_rec % auxhist5_begin_d           = model_config_rec % auxhist5_begin_d (id_id)
 grid_config_rec % auxhist5_begin_h           = model_config_rec % auxhist5_begin_h (id_id)
 grid_config_rec % auxhist5_begin_m           = model_config_rec % auxhist5_begin_m (id_id)
 grid_config_rec % auxhist5_begin_s           = model_config_rec % auxhist5_begin_s (id_id)
 grid_config_rec % auxhist5_begin             = model_config_rec % auxhist5_begin (id_id)
 grid_config_rec % auxhist5_end_y             = model_config_rec % auxhist5_end_y (id_id)
 grid_config_rec % auxhist5_end_d             = model_config_rec % auxhist5_end_d (id_id)
 grid_config_rec % auxhist5_end_h             = model_config_rec % auxhist5_end_h (id_id)
 grid_config_rec % auxhist5_end_m             = model_config_rec % auxhist5_end_m (id_id)
 grid_config_rec % auxhist5_end_s             = model_config_rec % auxhist5_end_s (id_id)
 grid_config_rec % auxhist5_end               = model_config_rec % auxhist5_end (id_id)
 grid_config_rec % io_form_auxhist5           = model_config_rec % io_form_auxhist5 
 grid_config_rec % frames_per_auxhist5        = model_config_rec % frames_per_auxhist5 (id_id)
 grid_config_rec % auxhist6_inname            = model_config_rec % auxhist6_inname 
 grid_config_rec % auxhist6_outname           = model_config_rec % auxhist6_outname 
 grid_config_rec % auxhist6_interval_y        = model_config_rec % auxhist6_interval_y (id_id)
 grid_config_rec % auxhist6_interval_d        = model_config_rec % auxhist6_interval_d (id_id)
 grid_config_rec % auxhist6_interval_h        = model_config_rec % auxhist6_interval_h (id_id)
 grid_config_rec % auxhist6_interval_m        = model_config_rec % auxhist6_interval_m (id_id)
 grid_config_rec % auxhist6_interval_s        = model_config_rec % auxhist6_interval_s (id_id)
 grid_config_rec % auxhist6_interval          = model_config_rec % auxhist6_interval (id_id)
 grid_config_rec % auxhist6_begin_y           = model_config_rec % auxhist6_begin_y (id_id)
 grid_config_rec % auxhist6_begin_d           = model_config_rec % auxhist6_begin_d (id_id)
 grid_config_rec % auxhist6_begin_h           = model_config_rec % auxhist6_begin_h (id_id)
 grid_config_rec % auxhist6_begin_m           = model_config_rec % auxhist6_begin_m (id_id)
 grid_config_rec % auxhist6_begin_s           = model_config_rec % auxhist6_begin_s (id_id)
 grid_config_rec % auxhist6_begin             = model_config_rec % auxhist6_begin (id_id)
 grid_config_rec % auxhist6_end_y             = model_config_rec % auxhist6_end_y (id_id)
 grid_config_rec % auxhist6_end_d             = model_config_rec % auxhist6_end_d (id_id)
 grid_config_rec % auxhist6_end_h             = model_config_rec % auxhist6_end_h (id_id)
 grid_config_rec % auxhist6_end_m             = model_config_rec % auxhist6_end_m (id_id)
 grid_config_rec % auxhist6_end_s             = model_config_rec % auxhist6_end_s (id_id)
 grid_config_rec % auxhist6_end               = model_config_rec % auxhist6_end (id_id)
 grid_config_rec % io_form_auxhist6           = model_config_rec % io_form_auxhist6 
 grid_config_rec % frames_per_auxhist6        = model_config_rec % frames_per_auxhist6 (id_id)
 grid_config_rec % auxhist7_inname            = model_config_rec % auxhist7_inname 
 grid_config_rec % auxhist7_outname           = model_config_rec % auxhist7_outname 
 grid_config_rec % auxhist7_interval_y        = model_config_rec % auxhist7_interval_y (id_id)
 grid_config_rec % auxhist7_interval_d        = model_config_rec % auxhist7_interval_d (id_id)
 grid_config_rec % auxhist7_interval_h        = model_config_rec % auxhist7_interval_h (id_id)
 grid_config_rec % auxhist7_interval_m        = model_config_rec % auxhist7_interval_m (id_id)
 grid_config_rec % auxhist7_interval_s        = model_config_rec % auxhist7_interval_s (id_id)
 grid_config_rec % auxhist7_interval          = model_config_rec % auxhist7_interval (id_id)
 grid_config_rec % auxhist7_begin_y           = model_config_rec % auxhist7_begin_y (id_id)
 grid_config_rec % auxhist7_begin_d           = model_config_rec % auxhist7_begin_d (id_id)
 grid_config_rec % auxhist7_begin_h           = model_config_rec % auxhist7_begin_h (id_id)
 grid_config_rec % auxhist7_begin_m           = model_config_rec % auxhist7_begin_m (id_id)
 grid_config_rec % auxhist7_begin_s           = model_config_rec % auxhist7_begin_s (id_id)
 grid_config_rec % auxhist7_begin             = model_config_rec % auxhist7_begin (id_id)
 grid_config_rec % auxhist7_end_y             = model_config_rec % auxhist7_end_y (id_id)
 grid_config_rec % auxhist7_end_d             = model_config_rec % auxhist7_end_d (id_id)
 grid_config_rec % auxhist7_end_h             = model_config_rec % auxhist7_end_h (id_id)
 grid_config_rec % auxhist7_end_m             = model_config_rec % auxhist7_end_m (id_id)
 grid_config_rec % auxhist7_end_s             = model_config_rec % auxhist7_end_s (id_id)
 grid_config_rec % auxhist7_end               = model_config_rec % auxhist7_end (id_id)
 grid_config_rec % io_form_auxhist7           = model_config_rec % io_form_auxhist7 
 grid_config_rec % frames_per_auxhist7        = model_config_rec % frames_per_auxhist7 (id_id)
 grid_config_rec % auxhist8_inname            = model_config_rec % auxhist8_inname 
 grid_config_rec % auxhist8_outname           = model_config_rec % auxhist8_outname 
 grid_config_rec % auxhist8_interval_y        = model_config_rec % auxhist8_interval_y (id_id)
 grid_config_rec % auxhist8_interval_d        = model_config_rec % auxhist8_interval_d (id_id)
 grid_config_rec % auxhist8_interval_h        = model_config_rec % auxhist8_interval_h (id_id)
 grid_config_rec % auxhist8_interval_m        = model_config_rec % auxhist8_interval_m (id_id)
 grid_config_rec % auxhist8_interval_s        = model_config_rec % auxhist8_interval_s (id_id)
 grid_config_rec % auxhist8_interval          = model_config_rec % auxhist8_interval (id_id)
 grid_config_rec % auxhist8_begin_y           = model_config_rec % auxhist8_begin_y (id_id)
 grid_config_rec % auxhist8_begin_d           = model_config_rec % auxhist8_begin_d (id_id)
 grid_config_rec % auxhist8_begin_h           = model_config_rec % auxhist8_begin_h (id_id)
 grid_config_rec % auxhist8_begin_m           = model_config_rec % auxhist8_begin_m (id_id)
 grid_config_rec % auxhist8_begin_s           = model_config_rec % auxhist8_begin_s (id_id)
 grid_config_rec % auxhist8_begin             = model_config_rec % auxhist8_begin (id_id)
 grid_config_rec % auxhist8_end_y             = model_config_rec % auxhist8_end_y (id_id)
 grid_config_rec % auxhist8_end_d             = model_config_rec % auxhist8_end_d (id_id)
 grid_config_rec % auxhist8_end_h             = model_config_rec % auxhist8_end_h (id_id)
 grid_config_rec % auxhist8_end_m             = model_config_rec % auxhist8_end_m (id_id)
 grid_config_rec % auxhist8_end_s             = model_config_rec % auxhist8_end_s (id_id)
 grid_config_rec % auxhist8_end               = model_config_rec % auxhist8_end (id_id)
 grid_config_rec % io_form_auxhist8           = model_config_rec % io_form_auxhist8 
 grid_config_rec % frames_per_auxhist8        = model_config_rec % frames_per_auxhist8 (id_id)
 grid_config_rec % auxhist9_inname            = model_config_rec % auxhist9_inname 
 grid_config_rec % auxhist9_outname           = model_config_rec % auxhist9_outname 
 grid_config_rec % auxhist9_interval_y        = model_config_rec % auxhist9_interval_y (id_id)
 grid_config_rec % auxhist9_interval_d        = model_config_rec % auxhist9_interval_d (id_id)
 grid_config_rec % auxhist9_interval_h        = model_config_rec % auxhist9_interval_h (id_id)
 grid_config_rec % auxhist9_interval_m        = model_config_rec % auxhist9_interval_m (id_id)
 grid_config_rec % auxhist9_interval_s        = model_config_rec % auxhist9_interval_s (id_id)
 grid_config_rec % auxhist9_interval          = model_config_rec % auxhist9_interval (id_id)
 grid_config_rec % auxhist9_begin_y           = model_config_rec % auxhist9_begin_y (id_id)
 grid_config_rec % auxhist9_begin_d           = model_config_rec % auxhist9_begin_d (id_id)
 grid_config_rec % auxhist9_begin_h           = model_config_rec % auxhist9_begin_h (id_id)
 grid_config_rec % auxhist9_begin_m           = model_config_rec % auxhist9_begin_m (id_id)
 grid_config_rec % auxhist9_begin_s           = model_config_rec % auxhist9_begin_s (id_id)
 grid_config_rec % auxhist9_begin             = model_config_rec % auxhist9_begin (id_id)
 grid_config_rec % auxhist9_end_y             = model_config_rec % auxhist9_end_y (id_id)
 grid_config_rec % auxhist9_end_d             = model_config_rec % auxhist9_end_d (id_id)
 grid_config_rec % auxhist9_end_h             = model_config_rec % auxhist9_end_h (id_id)
 grid_config_rec % auxhist9_end_m             = model_config_rec % auxhist9_end_m (id_id)
 grid_config_rec % auxhist9_end_s             = model_config_rec % auxhist9_end_s (id_id)
 grid_config_rec % auxhist9_end               = model_config_rec % auxhist9_end (id_id)
 grid_config_rec % io_form_auxhist9           = model_config_rec % io_form_auxhist9 
 grid_config_rec % frames_per_auxhist9        = model_config_rec % frames_per_auxhist9 (id_id)
 grid_config_rec % auxhist10_inname           = model_config_rec % auxhist10_inname 
 grid_config_rec % auxhist10_outname          = model_config_rec % auxhist10_outname 
 grid_config_rec % auxhist10_interval_y       = model_config_rec % auxhist10_interval_y (id_id)
 grid_config_rec % auxhist10_interval_d       = model_config_rec % auxhist10_interval_d (id_id)
 grid_config_rec % auxhist10_interval_h       = model_config_rec % auxhist10_interval_h (id_id)
 grid_config_rec % auxhist10_interval_m       = model_config_rec % auxhist10_interval_m (id_id)
 grid_config_rec % auxhist10_interval_s       = model_config_rec % auxhist10_interval_s (id_id)
 grid_config_rec % auxhist10_interval         = model_config_rec % auxhist10_interval (id_id)
 grid_config_rec % auxhist10_begin_y          = model_config_rec % auxhist10_begin_y (id_id)
 grid_config_rec % auxhist10_begin_d          = model_config_rec % auxhist10_begin_d (id_id)
 grid_config_rec % auxhist10_begin_h          = model_config_rec % auxhist10_begin_h (id_id)
 grid_config_rec % auxhist10_begin_m          = model_config_rec % auxhist10_begin_m (id_id)
 grid_config_rec % auxhist10_begin_s          = model_config_rec % auxhist10_begin_s (id_id)
 grid_config_rec % auxhist10_begin            = model_config_rec % auxhist10_begin (id_id)
 grid_config_rec % auxhist10_end_y            = model_config_rec % auxhist10_end_y (id_id)
 grid_config_rec % auxhist10_end_d            = model_config_rec % auxhist10_end_d (id_id)
 grid_config_rec % auxhist10_end_h            = model_config_rec % auxhist10_end_h (id_id)
 grid_config_rec % auxhist10_end_m            = model_config_rec % auxhist10_end_m (id_id)
 grid_config_rec % auxhist10_end_s            = model_config_rec % auxhist10_end_s (id_id)
 grid_config_rec % auxhist10_end              = model_config_rec % auxhist10_end (id_id)
 grid_config_rec % io_form_auxhist10          = model_config_rec % io_form_auxhist10 
 grid_config_rec % frames_per_auxhist10       = model_config_rec % frames_per_auxhist10 (id_id)
 grid_config_rec % auxhist11_inname           = model_config_rec % auxhist11_inname 
 grid_config_rec % auxhist11_outname          = model_config_rec % auxhist11_outname 
 grid_config_rec % auxhist11_interval_y       = model_config_rec % auxhist11_interval_y (id_id)
 grid_config_rec % auxhist11_interval_d       = model_config_rec % auxhist11_interval_d (id_id)
 grid_config_rec % auxhist11_interval_h       = model_config_rec % auxhist11_interval_h (id_id)
 grid_config_rec % auxhist11_interval_m       = model_config_rec % auxhist11_interval_m (id_id)
 grid_config_rec % auxhist11_interval_s       = model_config_rec % auxhist11_interval_s (id_id)
 grid_config_rec % auxhist11_interval         = model_config_rec % auxhist11_interval (id_id)
 grid_config_rec % auxhist11_begin_y          = model_config_rec % auxhist11_begin_y (id_id)
 grid_config_rec % auxhist11_begin_d          = model_config_rec % auxhist11_begin_d (id_id)
 grid_config_rec % auxhist11_begin_h          = model_config_rec % auxhist11_begin_h (id_id)
 grid_config_rec % auxhist11_begin_m          = model_config_rec % auxhist11_begin_m (id_id)
 grid_config_rec % auxhist11_begin_s          = model_config_rec % auxhist11_begin_s (id_id)
 grid_config_rec % auxhist11_begin            = model_config_rec % auxhist11_begin (id_id)
 grid_config_rec % auxhist11_end_y            = model_config_rec % auxhist11_end_y (id_id)
 grid_config_rec % auxhist11_end_d            = model_config_rec % auxhist11_end_d (id_id)
 grid_config_rec % auxhist11_end_h            = model_config_rec % auxhist11_end_h (id_id)
 grid_config_rec % auxhist11_end_m            = model_config_rec % auxhist11_end_m (id_id)
 grid_config_rec % auxhist11_end_s            = model_config_rec % auxhist11_end_s (id_id)
 grid_config_rec % auxhist11_end              = model_config_rec % auxhist11_end (id_id)
 grid_config_rec % io_form_auxhist11          = model_config_rec % io_form_auxhist11 
 grid_config_rec % frames_per_auxhist11       = model_config_rec % frames_per_auxhist11 (id_id)
 grid_config_rec % auxhist12_inname           = model_config_rec % auxhist12_inname 
 grid_config_rec % auxhist12_outname          = model_config_rec % auxhist12_outname 
 grid_config_rec % auxhist12_interval_y       = model_config_rec % auxhist12_interval_y (id_id)
 grid_config_rec % auxhist12_interval_d       = model_config_rec % auxhist12_interval_d (id_id)
 grid_config_rec % auxhist12_interval_h       = model_config_rec % auxhist12_interval_h (id_id)
 grid_config_rec % auxhist12_interval_m       = model_config_rec % auxhist12_interval_m (id_id)
 grid_config_rec % auxhist12_interval_s       = model_config_rec % auxhist12_interval_s (id_id)
 grid_config_rec % auxhist12_interval         = model_config_rec % auxhist12_interval (id_id)
 grid_config_rec % auxhist12_begin_y          = model_config_rec % auxhist12_begin_y (id_id)
 grid_config_rec % auxhist12_begin_d          = model_config_rec % auxhist12_begin_d (id_id)
 grid_config_rec % auxhist12_begin_h          = model_config_rec % auxhist12_begin_h (id_id)
 grid_config_rec % auxhist12_begin_m          = model_config_rec % auxhist12_begin_m (id_id)
 grid_config_rec % auxhist12_begin_s          = model_config_rec % auxhist12_begin_s (id_id)
 grid_config_rec % auxhist12_begin            = model_config_rec % auxhist12_begin (id_id)
 grid_config_rec % auxhist12_end_y            = model_config_rec % auxhist12_end_y (id_id)
 grid_config_rec % auxhist12_end_d            = model_config_rec % auxhist12_end_d (id_id)
 grid_config_rec % auxhist12_end_h            = model_config_rec % auxhist12_end_h (id_id)
 grid_config_rec % auxhist12_end_m            = model_config_rec % auxhist12_end_m (id_id)
 grid_config_rec % auxhist12_end_s            = model_config_rec % auxhist12_end_s (id_id)
 grid_config_rec % auxhist12_end              = model_config_rec % auxhist12_end (id_id)
 grid_config_rec % io_form_auxhist12          = model_config_rec % io_form_auxhist12 
 grid_config_rec % frames_per_auxhist12       = model_config_rec % frames_per_auxhist12 (id_id)
 grid_config_rec % auxhist13_inname           = model_config_rec % auxhist13_inname 
 grid_config_rec % auxhist13_outname          = model_config_rec % auxhist13_outname 
 grid_config_rec % auxhist13_interval_y       = model_config_rec % auxhist13_interval_y (id_id)
 grid_config_rec % auxhist13_interval_d       = model_config_rec % auxhist13_interval_d (id_id)
 grid_config_rec % auxhist13_interval_h       = model_config_rec % auxhist13_interval_h (id_id)
 grid_config_rec % auxhist13_interval_m       = model_config_rec % auxhist13_interval_m (id_id)
 grid_config_rec % auxhist13_interval_s       = model_config_rec % auxhist13_interval_s (id_id)
 grid_config_rec % auxhist13_interval         = model_config_rec % auxhist13_interval (id_id)
 grid_config_rec % auxhist13_begin_y          = model_config_rec % auxhist13_begin_y (id_id)
 grid_config_rec % auxhist13_begin_d          = model_config_rec % auxhist13_begin_d (id_id)
 grid_config_rec % auxhist13_begin_h          = model_config_rec % auxhist13_begin_h (id_id)
 grid_config_rec % auxhist13_begin_m          = model_config_rec % auxhist13_begin_m (id_id)
 grid_config_rec % auxhist13_begin_s          = model_config_rec % auxhist13_begin_s (id_id)
 grid_config_rec % auxhist13_begin            = model_config_rec % auxhist13_begin (id_id)
 grid_config_rec % auxhist13_end_y            = model_config_rec % auxhist13_end_y (id_id)
 grid_config_rec % auxhist13_end_d            = model_config_rec % auxhist13_end_d (id_id)
 grid_config_rec % auxhist13_end_h            = model_config_rec % auxhist13_end_h (id_id)
 grid_config_rec % auxhist13_end_m            = model_config_rec % auxhist13_end_m (id_id)
 grid_config_rec % auxhist13_end_s            = model_config_rec % auxhist13_end_s (id_id)
 grid_config_rec % auxhist13_end              = model_config_rec % auxhist13_end (id_id)
 grid_config_rec % io_form_auxhist13          = model_config_rec % io_form_auxhist13 
 grid_config_rec % frames_per_auxhist13       = model_config_rec % frames_per_auxhist13 (id_id)
 grid_config_rec % auxhist14_inname           = model_config_rec % auxhist14_inname 
 grid_config_rec % auxhist14_outname          = model_config_rec % auxhist14_outname 
 grid_config_rec % auxhist14_interval_y       = model_config_rec % auxhist14_interval_y (id_id)
 grid_config_rec % auxhist14_interval_d       = model_config_rec % auxhist14_interval_d (id_id)
 grid_config_rec % auxhist14_interval_h       = model_config_rec % auxhist14_interval_h (id_id)
 grid_config_rec % auxhist14_interval_m       = model_config_rec % auxhist14_interval_m (id_id)
 grid_config_rec % auxhist14_interval_s       = model_config_rec % auxhist14_interval_s (id_id)
 grid_config_rec % auxhist14_interval         = model_config_rec % auxhist14_interval (id_id)
 grid_config_rec % auxhist14_begin_y          = model_config_rec % auxhist14_begin_y (id_id)
 grid_config_rec % auxhist14_begin_d          = model_config_rec % auxhist14_begin_d (id_id)
 grid_config_rec % auxhist14_begin_h          = model_config_rec % auxhist14_begin_h (id_id)
 grid_config_rec % auxhist14_begin_m          = model_config_rec % auxhist14_begin_m (id_id)
 grid_config_rec % auxhist14_begin_s          = model_config_rec % auxhist14_begin_s (id_id)
 grid_config_rec % auxhist14_begin            = model_config_rec % auxhist14_begin (id_id)
 grid_config_rec % auxhist14_end_y            = model_config_rec % auxhist14_end_y (id_id)
 grid_config_rec % auxhist14_end_d            = model_config_rec % auxhist14_end_d (id_id)
 grid_config_rec % auxhist14_end_h            = model_config_rec % auxhist14_end_h (id_id)
 grid_config_rec % auxhist14_end_m            = model_config_rec % auxhist14_end_m (id_id)
 grid_config_rec % auxhist14_end_s            = model_config_rec % auxhist14_end_s (id_id)
 grid_config_rec % auxhist14_end              = model_config_rec % auxhist14_end (id_id)
 grid_config_rec % io_form_auxhist14          = model_config_rec % io_form_auxhist14 
 grid_config_rec % frames_per_auxhist14       = model_config_rec % frames_per_auxhist14 (id_id)
 grid_config_rec % auxhist15_inname           = model_config_rec % auxhist15_inname 
 grid_config_rec % auxhist15_outname          = model_config_rec % auxhist15_outname 
 grid_config_rec % auxhist15_interval_y       = model_config_rec % auxhist15_interval_y (id_id)
 grid_config_rec % auxhist15_interval_d       = model_config_rec % auxhist15_interval_d (id_id)
 grid_config_rec % auxhist15_interval_h       = model_config_rec % auxhist15_interval_h (id_id)
 grid_config_rec % auxhist15_interval_m       = model_config_rec % auxhist15_interval_m (id_id)
 grid_config_rec % auxhist15_interval_s       = model_config_rec % auxhist15_interval_s (id_id)
 grid_config_rec % auxhist15_interval         = model_config_rec % auxhist15_interval (id_id)
 grid_config_rec % auxhist15_begin_y          = model_config_rec % auxhist15_begin_y (id_id)
 grid_config_rec % auxhist15_begin_d          = model_config_rec % auxhist15_begin_d (id_id)
 grid_config_rec % auxhist15_begin_h          = model_config_rec % auxhist15_begin_h (id_id)
 grid_config_rec % auxhist15_begin_m          = model_config_rec % auxhist15_begin_m (id_id)
 grid_config_rec % auxhist15_begin_s          = model_config_rec % auxhist15_begin_s (id_id)
 grid_config_rec % auxhist15_begin            = model_config_rec % auxhist15_begin (id_id)
 grid_config_rec % auxhist15_end_y            = model_config_rec % auxhist15_end_y (id_id)
 grid_config_rec % auxhist15_end_d            = model_config_rec % auxhist15_end_d (id_id)
 grid_config_rec % auxhist15_end_h            = model_config_rec % auxhist15_end_h (id_id)
 grid_config_rec % auxhist15_end_m            = model_config_rec % auxhist15_end_m (id_id)
 grid_config_rec % auxhist15_end_s            = model_config_rec % auxhist15_end_s (id_id)
 grid_config_rec % auxhist15_end              = model_config_rec % auxhist15_end (id_id)
 grid_config_rec % io_form_auxhist15          = model_config_rec % io_form_auxhist15 
 grid_config_rec % frames_per_auxhist15       = model_config_rec % frames_per_auxhist15 (id_id)
 grid_config_rec % auxhist16_inname           = model_config_rec % auxhist16_inname 
 grid_config_rec % auxhist16_outname          = model_config_rec % auxhist16_outname 
 grid_config_rec % auxhist16_interval_y       = model_config_rec % auxhist16_interval_y (id_id)
 grid_config_rec % auxhist16_interval_d       = model_config_rec % auxhist16_interval_d (id_id)
 grid_config_rec % auxhist16_interval_h       = model_config_rec % auxhist16_interval_h (id_id)
 grid_config_rec % auxhist16_interval_m       = model_config_rec % auxhist16_interval_m (id_id)
 grid_config_rec % auxhist16_interval_s       = model_config_rec % auxhist16_interval_s (id_id)
 grid_config_rec % auxhist16_interval         = model_config_rec % auxhist16_interval (id_id)
 grid_config_rec % auxhist16_begin_y          = model_config_rec % auxhist16_begin_y (id_id)
 grid_config_rec % auxhist16_begin_d          = model_config_rec % auxhist16_begin_d (id_id)
 grid_config_rec % auxhist16_begin_h          = model_config_rec % auxhist16_begin_h (id_id)
 grid_config_rec % auxhist16_begin_m          = model_config_rec % auxhist16_begin_m (id_id)
 grid_config_rec % auxhist16_begin_s          = model_config_rec % auxhist16_begin_s (id_id)
 grid_config_rec % auxhist16_begin            = model_config_rec % auxhist16_begin (id_id)
 grid_config_rec % auxhist16_end_y            = model_config_rec % auxhist16_end_y (id_id)
 grid_config_rec % auxhist16_end_d            = model_config_rec % auxhist16_end_d (id_id)
 grid_config_rec % auxhist16_end_h            = model_config_rec % auxhist16_end_h (id_id)
 grid_config_rec % auxhist16_end_m            = model_config_rec % auxhist16_end_m (id_id)
 grid_config_rec % auxhist16_end_s            = model_config_rec % auxhist16_end_s (id_id)
 grid_config_rec % auxhist16_end              = model_config_rec % auxhist16_end (id_id)
 grid_config_rec % io_form_auxhist16          = model_config_rec % io_form_auxhist16 
 grid_config_rec % frames_per_auxhist16       = model_config_rec % frames_per_auxhist16 (id_id)
 grid_config_rec % auxhist17_inname           = model_config_rec % auxhist17_inname 
 grid_config_rec % auxhist17_outname          = model_config_rec % auxhist17_outname 
 grid_config_rec % auxhist17_interval_y       = model_config_rec % auxhist17_interval_y (id_id)
 grid_config_rec % auxhist17_interval_d       = model_config_rec % auxhist17_interval_d (id_id)
 grid_config_rec % auxhist17_interval_h       = model_config_rec % auxhist17_interval_h (id_id)
 grid_config_rec % auxhist17_interval_m       = model_config_rec % auxhist17_interval_m (id_id)
 grid_config_rec % auxhist17_interval_s       = model_config_rec % auxhist17_interval_s (id_id)
 grid_config_rec % auxhist17_interval         = model_config_rec % auxhist17_interval (id_id)
 grid_config_rec % auxhist17_begin_y          = model_config_rec % auxhist17_begin_y (id_id)
 grid_config_rec % auxhist17_begin_d          = model_config_rec % auxhist17_begin_d (id_id)
 grid_config_rec % auxhist17_begin_h          = model_config_rec % auxhist17_begin_h (id_id)
 grid_config_rec % auxhist17_begin_m          = model_config_rec % auxhist17_begin_m (id_id)
 grid_config_rec % auxhist17_begin_s          = model_config_rec % auxhist17_begin_s (id_id)
 grid_config_rec % auxhist17_begin            = model_config_rec % auxhist17_begin (id_id)
 grid_config_rec % auxhist17_end_y            = model_config_rec % auxhist17_end_y (id_id)
 grid_config_rec % auxhist17_end_d            = model_config_rec % auxhist17_end_d (id_id)
 grid_config_rec % auxhist17_end_h            = model_config_rec % auxhist17_end_h (id_id)
 grid_config_rec % auxhist17_end_m            = model_config_rec % auxhist17_end_m (id_id)
 grid_config_rec % auxhist17_end_s            = model_config_rec % auxhist17_end_s (id_id)
 grid_config_rec % auxhist17_end              = model_config_rec % auxhist17_end (id_id)
 grid_config_rec % io_form_auxhist17          = model_config_rec % io_form_auxhist17 
 grid_config_rec % frames_per_auxhist17       = model_config_rec % frames_per_auxhist17 (id_id)
 grid_config_rec % auxhist18_inname           = model_config_rec % auxhist18_inname 
 grid_config_rec % auxhist18_outname          = model_config_rec % auxhist18_outname 
 grid_config_rec % auxhist18_interval_y       = model_config_rec % auxhist18_interval_y (id_id)
 grid_config_rec % auxhist18_interval_d       = model_config_rec % auxhist18_interval_d (id_id)
 grid_config_rec % auxhist18_interval_h       = model_config_rec % auxhist18_interval_h (id_id)
 grid_config_rec % auxhist18_interval_m       = model_config_rec % auxhist18_interval_m (id_id)
 grid_config_rec % auxhist18_interval_s       = model_config_rec % auxhist18_interval_s (id_id)
 grid_config_rec % auxhist18_interval         = model_config_rec % auxhist18_interval (id_id)
 grid_config_rec % auxhist18_begin_y          = model_config_rec % auxhist18_begin_y (id_id)
 grid_config_rec % auxhist18_begin_d          = model_config_rec % auxhist18_begin_d (id_id)
 grid_config_rec % auxhist18_begin_h          = model_config_rec % auxhist18_begin_h (id_id)
 grid_config_rec % auxhist18_begin_m          = model_config_rec % auxhist18_begin_m (id_id)
 grid_config_rec % auxhist18_begin_s          = model_config_rec % auxhist18_begin_s (id_id)
 grid_config_rec % auxhist18_begin            = model_config_rec % auxhist18_begin (id_id)
 grid_config_rec % auxhist18_end_y            = model_config_rec % auxhist18_end_y (id_id)
 grid_config_rec % auxhist18_end_d            = model_config_rec % auxhist18_end_d (id_id)
 grid_config_rec % auxhist18_end_h            = model_config_rec % auxhist18_end_h (id_id)
 grid_config_rec % auxhist18_end_m            = model_config_rec % auxhist18_end_m (id_id)
 grid_config_rec % auxhist18_end_s            = model_config_rec % auxhist18_end_s (id_id)
 grid_config_rec % auxhist18_end              = model_config_rec % auxhist18_end (id_id)
 grid_config_rec % io_form_auxhist18          = model_config_rec % io_form_auxhist18 
 grid_config_rec % frames_per_auxhist18       = model_config_rec % frames_per_auxhist18 (id_id)
 grid_config_rec % auxhist19_inname           = model_config_rec % auxhist19_inname 
 grid_config_rec % auxhist19_outname          = model_config_rec % auxhist19_outname 
 grid_config_rec % auxhist19_interval_y       = model_config_rec % auxhist19_interval_y (id_id)
 grid_config_rec % auxhist19_interval_d       = model_config_rec % auxhist19_interval_d (id_id)
 grid_config_rec % auxhist19_interval_h       = model_config_rec % auxhist19_interval_h (id_id)
 grid_config_rec % auxhist19_interval_m       = model_config_rec % auxhist19_interval_m (id_id)
 grid_config_rec % auxhist19_interval_s       = model_config_rec % auxhist19_interval_s (id_id)
 grid_config_rec % auxhist19_interval         = model_config_rec % auxhist19_interval (id_id)
 grid_config_rec % auxhist19_begin_y          = model_config_rec % auxhist19_begin_y (id_id)
 grid_config_rec % auxhist19_begin_d          = model_config_rec % auxhist19_begin_d (id_id)
 grid_config_rec % auxhist19_begin_h          = model_config_rec % auxhist19_begin_h (id_id)
 grid_config_rec % auxhist19_begin_m          = model_config_rec % auxhist19_begin_m (id_id)
 grid_config_rec % auxhist19_begin_s          = model_config_rec % auxhist19_begin_s (id_id)
 grid_config_rec % auxhist19_begin            = model_config_rec % auxhist19_begin (id_id)
 grid_config_rec % auxhist19_end_y            = model_config_rec % auxhist19_end_y (id_id)
 grid_config_rec % auxhist19_end_d            = model_config_rec % auxhist19_end_d (id_id)
 grid_config_rec % auxhist19_end_h            = model_config_rec % auxhist19_end_h (id_id)
 grid_config_rec % auxhist19_end_m            = model_config_rec % auxhist19_end_m (id_id)
 grid_config_rec % auxhist19_end_s            = model_config_rec % auxhist19_end_s (id_id)
 grid_config_rec % auxhist19_end              = model_config_rec % auxhist19_end (id_id)
 grid_config_rec % io_form_auxhist19          = model_config_rec % io_form_auxhist19 
 grid_config_rec % frames_per_auxhist19       = model_config_rec % frames_per_auxhist19 (id_id)
 grid_config_rec % auxhist20_inname           = model_config_rec % auxhist20_inname 
 grid_config_rec % auxhist20_outname          = model_config_rec % auxhist20_outname 
 grid_config_rec % auxhist20_interval_y       = model_config_rec % auxhist20_interval_y (id_id)
 grid_config_rec % auxhist20_interval_d       = model_config_rec % auxhist20_interval_d (id_id)
 grid_config_rec % auxhist20_interval_h       = model_config_rec % auxhist20_interval_h (id_id)
 grid_config_rec % auxhist20_interval_m       = model_config_rec % auxhist20_interval_m (id_id)
 grid_config_rec % auxhist20_interval_s       = model_config_rec % auxhist20_interval_s (id_id)
 grid_config_rec % auxhist20_interval         = model_config_rec % auxhist20_interval (id_id)
 grid_config_rec % auxhist20_begin_y          = model_config_rec % auxhist20_begin_y (id_id)
 grid_config_rec % auxhist20_begin_d          = model_config_rec % auxhist20_begin_d (id_id)
 grid_config_rec % auxhist20_begin_h          = model_config_rec % auxhist20_begin_h (id_id)
 grid_config_rec % auxhist20_begin_m          = model_config_rec % auxhist20_begin_m (id_id)
 grid_config_rec % auxhist20_begin_s          = model_config_rec % auxhist20_begin_s (id_id)
 grid_config_rec % auxhist20_begin            = model_config_rec % auxhist20_begin (id_id)
 grid_config_rec % auxhist20_end_y            = model_config_rec % auxhist20_end_y (id_id)
 grid_config_rec % auxhist20_end_d            = model_config_rec % auxhist20_end_d (id_id)
 grid_config_rec % auxhist20_end_h            = model_config_rec % auxhist20_end_h (id_id)
 grid_config_rec % auxhist20_end_m            = model_config_rec % auxhist20_end_m (id_id)
 grid_config_rec % auxhist20_end_s            = model_config_rec % auxhist20_end_s (id_id)
 grid_config_rec % auxhist20_end              = model_config_rec % auxhist20_end (id_id)
 grid_config_rec % io_form_auxhist20          = model_config_rec % io_form_auxhist20 
 grid_config_rec % frames_per_auxhist20       = model_config_rec % frames_per_auxhist20 (id_id)
 grid_config_rec % auxhist21_inname           = model_config_rec % auxhist21_inname 
 grid_config_rec % auxhist21_outname          = model_config_rec % auxhist21_outname 
 grid_config_rec % auxhist21_interval_y       = model_config_rec % auxhist21_interval_y (id_id)
 grid_config_rec % auxhist21_interval_d       = model_config_rec % auxhist21_interval_d (id_id)
 grid_config_rec % auxhist21_interval_h       = model_config_rec % auxhist21_interval_h (id_id)
 grid_config_rec % auxhist21_interval_m       = model_config_rec % auxhist21_interval_m (id_id)
 grid_config_rec % auxhist21_interval_s       = model_config_rec % auxhist21_interval_s (id_id)
 grid_config_rec % auxhist21_interval         = model_config_rec % auxhist21_interval (id_id)
 grid_config_rec % auxhist21_begin_y          = model_config_rec % auxhist21_begin_y (id_id)
 grid_config_rec % auxhist21_begin_d          = model_config_rec % auxhist21_begin_d (id_id)
 grid_config_rec % auxhist21_begin_h          = model_config_rec % auxhist21_begin_h (id_id)
 grid_config_rec % auxhist21_begin_m          = model_config_rec % auxhist21_begin_m (id_id)
 grid_config_rec % auxhist21_begin_s          = model_config_rec % auxhist21_begin_s (id_id)
 grid_config_rec % auxhist21_begin            = model_config_rec % auxhist21_begin (id_id)
 grid_config_rec % auxhist21_end_y            = model_config_rec % auxhist21_end_y (id_id)
 grid_config_rec % auxhist21_end_d            = model_config_rec % auxhist21_end_d (id_id)
 grid_config_rec % auxhist21_end_h            = model_config_rec % auxhist21_end_h (id_id)
 grid_config_rec % auxhist21_end_m            = model_config_rec % auxhist21_end_m (id_id)
 grid_config_rec % auxhist21_end_s            = model_config_rec % auxhist21_end_s (id_id)
 grid_config_rec % auxhist21_end              = model_config_rec % auxhist21_end (id_id)
 grid_config_rec % io_form_auxhist21          = model_config_rec % io_form_auxhist21 
 grid_config_rec % frames_per_auxhist21       = model_config_rec % frames_per_auxhist21 (id_id)
 grid_config_rec % auxhist22_inname           = model_config_rec % auxhist22_inname 
 grid_config_rec % auxhist22_outname          = model_config_rec % auxhist22_outname 
 grid_config_rec % auxhist22_interval_y       = model_config_rec % auxhist22_interval_y (id_id)
 grid_config_rec % auxhist22_interval_d       = model_config_rec % auxhist22_interval_d (id_id)
 grid_config_rec % auxhist22_interval_h       = model_config_rec % auxhist22_interval_h (id_id)
 grid_config_rec % auxhist22_interval_m       = model_config_rec % auxhist22_interval_m (id_id)
 grid_config_rec % auxhist22_interval_s       = model_config_rec % auxhist22_interval_s (id_id)
 grid_config_rec % auxhist22_interval         = model_config_rec % auxhist22_interval (id_id)
 grid_config_rec % auxhist22_begin_y          = model_config_rec % auxhist22_begin_y (id_id)
 grid_config_rec % auxhist22_begin_d          = model_config_rec % auxhist22_begin_d (id_id)
 grid_config_rec % auxhist22_begin_h          = model_config_rec % auxhist22_begin_h (id_id)
 grid_config_rec % auxhist22_begin_m          = model_config_rec % auxhist22_begin_m (id_id)
 grid_config_rec % auxhist22_begin_s          = model_config_rec % auxhist22_begin_s (id_id)
 grid_config_rec % auxhist22_begin            = model_config_rec % auxhist22_begin (id_id)
 grid_config_rec % auxhist22_end_y            = model_config_rec % auxhist22_end_y (id_id)
 grid_config_rec % auxhist22_end_d            = model_config_rec % auxhist22_end_d (id_id)
 grid_config_rec % auxhist22_end_h            = model_config_rec % auxhist22_end_h (id_id)
 grid_config_rec % auxhist22_end_m            = model_config_rec % auxhist22_end_m (id_id)
 grid_config_rec % auxhist22_end_s            = model_config_rec % auxhist22_end_s (id_id)
 grid_config_rec % auxhist22_end              = model_config_rec % auxhist22_end (id_id)
 grid_config_rec % io_form_auxhist22          = model_config_rec % io_form_auxhist22 
 grid_config_rec % frames_per_auxhist22       = model_config_rec % frames_per_auxhist22 (id_id)
 grid_config_rec % auxhist23_inname           = model_config_rec % auxhist23_inname 
 grid_config_rec % auxhist23_outname          = model_config_rec % auxhist23_outname 
 grid_config_rec % auxhist23_interval_y       = model_config_rec % auxhist23_interval_y (id_id)
 grid_config_rec % auxhist23_interval_d       = model_config_rec % auxhist23_interval_d (id_id)
 grid_config_rec % auxhist23_interval_h       = model_config_rec % auxhist23_interval_h (id_id)
 grid_config_rec % auxhist23_interval_m       = model_config_rec % auxhist23_interval_m (id_id)
 grid_config_rec % auxhist23_interval_s       = model_config_rec % auxhist23_interval_s (id_id)
 grid_config_rec % auxhist23_interval         = model_config_rec % auxhist23_interval (id_id)
 grid_config_rec % auxhist23_begin_y          = model_config_rec % auxhist23_begin_y (id_id)
 grid_config_rec % auxhist23_begin_d          = model_config_rec % auxhist23_begin_d (id_id)
 grid_config_rec % auxhist23_begin_h          = model_config_rec % auxhist23_begin_h (id_id)
 grid_config_rec % auxhist23_begin_m          = model_config_rec % auxhist23_begin_m (id_id)
 grid_config_rec % auxhist23_begin_s          = model_config_rec % auxhist23_begin_s (id_id)
 grid_config_rec % auxhist23_begin            = model_config_rec % auxhist23_begin (id_id)
 grid_config_rec % auxhist23_end_y            = model_config_rec % auxhist23_end_y (id_id)
 grid_config_rec % auxhist23_end_d            = model_config_rec % auxhist23_end_d (id_id)
 grid_config_rec % auxhist23_end_h            = model_config_rec % auxhist23_end_h (id_id)
 grid_config_rec % auxhist23_end_m            = model_config_rec % auxhist23_end_m (id_id)
 grid_config_rec % auxhist23_end_s            = model_config_rec % auxhist23_end_s (id_id)
 grid_config_rec % auxhist23_end              = model_config_rec % auxhist23_end (id_id)
 grid_config_rec % io_form_auxhist23          = model_config_rec % io_form_auxhist23 
 grid_config_rec % frames_per_auxhist23       = model_config_rec % frames_per_auxhist23 (id_id)
 grid_config_rec % auxhist24_inname           = model_config_rec % auxhist24_inname 
 grid_config_rec % auxhist24_outname          = model_config_rec % auxhist24_outname 
 grid_config_rec % auxhist24_interval_y       = model_config_rec % auxhist24_interval_y (id_id)
 grid_config_rec % auxhist24_interval_d       = model_config_rec % auxhist24_interval_d (id_id)
 grid_config_rec % auxhist24_interval_h       = model_config_rec % auxhist24_interval_h (id_id)
 grid_config_rec % auxhist24_interval_m       = model_config_rec % auxhist24_interval_m (id_id)
 grid_config_rec % auxhist24_interval_s       = model_config_rec % auxhist24_interval_s (id_id)
 grid_config_rec % auxhist24_interval         = model_config_rec % auxhist24_interval (id_id)
 grid_config_rec % auxhist24_begin_y          = model_config_rec % auxhist24_begin_y (id_id)
 grid_config_rec % auxhist24_begin_d          = model_config_rec % auxhist24_begin_d (id_id)
 grid_config_rec % auxhist24_begin_h          = model_config_rec % auxhist24_begin_h (id_id)
 grid_config_rec % auxhist24_begin_m          = model_config_rec % auxhist24_begin_m (id_id)
 grid_config_rec % auxhist24_begin_s          = model_config_rec % auxhist24_begin_s (id_id)
 grid_config_rec % auxhist24_begin            = model_config_rec % auxhist24_begin (id_id)
 grid_config_rec % auxhist24_end_y            = model_config_rec % auxhist24_end_y (id_id)
 grid_config_rec % auxhist24_end_d            = model_config_rec % auxhist24_end_d (id_id)
 grid_config_rec % auxhist24_end_h            = model_config_rec % auxhist24_end_h (id_id)
 grid_config_rec % auxhist24_end_m            = model_config_rec % auxhist24_end_m (id_id)
 grid_config_rec % auxhist24_end_s            = model_config_rec % auxhist24_end_s (id_id)
 grid_config_rec % auxhist24_end              = model_config_rec % auxhist24_end (id_id)
 grid_config_rec % io_form_auxhist24          = model_config_rec % io_form_auxhist24 
 grid_config_rec % frames_per_auxhist24       = model_config_rec % frames_per_auxhist24 (id_id)
 grid_config_rec % auxinput1_outname          = model_config_rec % auxinput1_outname 
 grid_config_rec % auxinput1_interval_y       = model_config_rec % auxinput1_interval_y (id_id)
 grid_config_rec % auxinput1_interval_d       = model_config_rec % auxinput1_interval_d (id_id)
 grid_config_rec % auxinput1_interval_h       = model_config_rec % auxinput1_interval_h (id_id)
 grid_config_rec % auxinput1_interval_m       = model_config_rec % auxinput1_interval_m (id_id)
 grid_config_rec % auxinput1_interval_s       = model_config_rec % auxinput1_interval_s (id_id)
 grid_config_rec % auxinput1_interval         = model_config_rec % auxinput1_interval (id_id)
 grid_config_rec % auxinput1_begin_y          = model_config_rec % auxinput1_begin_y (id_id)
 grid_config_rec % auxinput1_begin_d          = model_config_rec % auxinput1_begin_d (id_id)
 grid_config_rec % auxinput1_begin_h          = model_config_rec % auxinput1_begin_h (id_id)
 grid_config_rec % auxinput1_begin_m          = model_config_rec % auxinput1_begin_m (id_id)
 grid_config_rec % auxinput1_begin_s          = model_config_rec % auxinput1_begin_s (id_id)
 grid_config_rec % auxinput1_begin            = model_config_rec % auxinput1_begin (id_id)
 grid_config_rec % auxinput1_end_y            = model_config_rec % auxinput1_end_y (id_id)
 grid_config_rec % auxinput1_end_d            = model_config_rec % auxinput1_end_d (id_id)
 grid_config_rec % auxinput1_end_h            = model_config_rec % auxinput1_end_h (id_id)
 grid_config_rec % auxinput1_end_m            = model_config_rec % auxinput1_end_m (id_id)
 grid_config_rec % auxinput1_end_s            = model_config_rec % auxinput1_end_s (id_id)
 grid_config_rec % auxinput1_end              = model_config_rec % auxinput1_end (id_id)
 grid_config_rec % frames_per_auxinput1       = model_config_rec % frames_per_auxinput1 (id_id)
 grid_config_rec % auxinput2_inname           = model_config_rec % auxinput2_inname 
 grid_config_rec % auxinput2_outname          = model_config_rec % auxinput2_outname 
 grid_config_rec % auxinput2_interval_y       = model_config_rec % auxinput2_interval_y (id_id)
 grid_config_rec % auxinput2_interval_d       = model_config_rec % auxinput2_interval_d (id_id)
 grid_config_rec % auxinput2_interval_h       = model_config_rec % auxinput2_interval_h (id_id)
 grid_config_rec % auxinput2_interval_m       = model_config_rec % auxinput2_interval_m (id_id)
 grid_config_rec % auxinput2_interval_s       = model_config_rec % auxinput2_interval_s (id_id)
 grid_config_rec % auxinput2_interval         = model_config_rec % auxinput2_interval (id_id)
 grid_config_rec % auxinput2_begin_y          = model_config_rec % auxinput2_begin_y (id_id)
 grid_config_rec % auxinput2_begin_d          = model_config_rec % auxinput2_begin_d (id_id)
 grid_config_rec % auxinput2_begin_h          = model_config_rec % auxinput2_begin_h (id_id)
 grid_config_rec % auxinput2_begin_m          = model_config_rec % auxinput2_begin_m (id_id)
 grid_config_rec % auxinput2_begin_s          = model_config_rec % auxinput2_begin_s (id_id)
 grid_config_rec % auxinput2_begin            = model_config_rec % auxinput2_begin (id_id)
 grid_config_rec % auxinput2_end_y            = model_config_rec % auxinput2_end_y (id_id)
 grid_config_rec % auxinput2_end_d            = model_config_rec % auxinput2_end_d (id_id)
 grid_config_rec % auxinput2_end_h            = model_config_rec % auxinput2_end_h (id_id)
 grid_config_rec % auxinput2_end_m            = model_config_rec % auxinput2_end_m (id_id)
 grid_config_rec % auxinput2_end_s            = model_config_rec % auxinput2_end_s (id_id)
 grid_config_rec % auxinput2_end              = model_config_rec % auxinput2_end (id_id)
 grid_config_rec % frames_per_auxinput2       = model_config_rec % frames_per_auxinput2 (id_id)
 grid_config_rec % auxinput3_inname           = model_config_rec % auxinput3_inname 
 grid_config_rec % auxinput3_outname          = model_config_rec % auxinput3_outname 
 grid_config_rec % auxinput3_interval_y       = model_config_rec % auxinput3_interval_y (id_id)
 grid_config_rec % auxinput3_interval_d       = model_config_rec % auxinput3_interval_d (id_id)
 grid_config_rec % auxinput3_interval_h       = model_config_rec % auxinput3_interval_h (id_id)
 grid_config_rec % auxinput3_interval_m       = model_config_rec % auxinput3_interval_m (id_id)
 grid_config_rec % auxinput3_interval_s       = model_config_rec % auxinput3_interval_s (id_id)
 grid_config_rec % auxinput3_interval         = model_config_rec % auxinput3_interval (id_id)
 grid_config_rec % auxinput3_begin_y          = model_config_rec % auxinput3_begin_y (id_id)
 grid_config_rec % auxinput3_begin_d          = model_config_rec % auxinput3_begin_d (id_id)
 grid_config_rec % auxinput3_begin_h          = model_config_rec % auxinput3_begin_h (id_id)
 grid_config_rec % auxinput3_begin_m          = model_config_rec % auxinput3_begin_m (id_id)
 grid_config_rec % auxinput3_begin_s          = model_config_rec % auxinput3_begin_s (id_id)
 grid_config_rec % auxinput3_begin            = model_config_rec % auxinput3_begin (id_id)
 grid_config_rec % auxinput3_end_y            = model_config_rec % auxinput3_end_y (id_id)
 grid_config_rec % auxinput3_end_d            = model_config_rec % auxinput3_end_d (id_id)
 grid_config_rec % auxinput3_end_h            = model_config_rec % auxinput3_end_h (id_id)
 grid_config_rec % auxinput3_end_m            = model_config_rec % auxinput3_end_m (id_id)
 grid_config_rec % auxinput3_end_s            = model_config_rec % auxinput3_end_s (id_id)
 grid_config_rec % auxinput3_end              = model_config_rec % auxinput3_end (id_id)
 grid_config_rec % io_form_auxinput3          = model_config_rec % io_form_auxinput3 
 grid_config_rec % frames_per_auxinput3       = model_config_rec % frames_per_auxinput3 (id_id)
 grid_config_rec % auxinput4_inname           = model_config_rec % auxinput4_inname 
 grid_config_rec % auxinput4_outname          = model_config_rec % auxinput4_outname 
 grid_config_rec % auxinput4_interval_y       = model_config_rec % auxinput4_interval_y (id_id)
 grid_config_rec % auxinput4_interval_d       = model_config_rec % auxinput4_interval_d (id_id)
 grid_config_rec % auxinput4_interval_h       = model_config_rec % auxinput4_interval_h (id_id)
 grid_config_rec % auxinput4_interval_m       = model_config_rec % auxinput4_interval_m (id_id)
 grid_config_rec % auxinput4_interval_s       = model_config_rec % auxinput4_interval_s (id_id)
 grid_config_rec % auxinput4_interval         = model_config_rec % auxinput4_interval (id_id)
 grid_config_rec % auxinput4_begin_y          = model_config_rec % auxinput4_begin_y (id_id)
 grid_config_rec % auxinput4_begin_d          = model_config_rec % auxinput4_begin_d (id_id)
 grid_config_rec % auxinput4_begin_h          = model_config_rec % auxinput4_begin_h (id_id)
 grid_config_rec % auxinput4_begin_m          = model_config_rec % auxinput4_begin_m (id_id)
 grid_config_rec % auxinput4_begin_s          = model_config_rec % auxinput4_begin_s (id_id)
 grid_config_rec % auxinput4_begin            = model_config_rec % auxinput4_begin (id_id)
 grid_config_rec % auxinput4_end_y            = model_config_rec % auxinput4_end_y (id_id)
 grid_config_rec % auxinput4_end_d            = model_config_rec % auxinput4_end_d (id_id)
 grid_config_rec % auxinput4_end_h            = model_config_rec % auxinput4_end_h (id_id)
 grid_config_rec % auxinput4_end_m            = model_config_rec % auxinput4_end_m (id_id)
 grid_config_rec % auxinput4_end_s            = model_config_rec % auxinput4_end_s (id_id)
 grid_config_rec % auxinput4_end              = model_config_rec % auxinput4_end (id_id)
 grid_config_rec % io_form_auxinput4          = model_config_rec % io_form_auxinput4 
 grid_config_rec % frames_per_auxinput4       = model_config_rec % frames_per_auxinput4 (id_id)
 grid_config_rec % auxinput5_inname           = model_config_rec % auxinput5_inname 
 grid_config_rec % auxinput5_outname          = model_config_rec % auxinput5_outname 
 grid_config_rec % auxinput5_interval_y       = model_config_rec % auxinput5_interval_y (id_id)
 grid_config_rec % auxinput5_interval_d       = model_config_rec % auxinput5_interval_d (id_id)
 grid_config_rec % auxinput5_interval_h       = model_config_rec % auxinput5_interval_h (id_id)
 grid_config_rec % auxinput5_interval_m       = model_config_rec % auxinput5_interval_m (id_id)
 grid_config_rec % auxinput5_interval_s       = model_config_rec % auxinput5_interval_s (id_id)
 grid_config_rec % auxinput5_interval         = model_config_rec % auxinput5_interval (id_id)
 grid_config_rec % auxinput5_begin_y          = model_config_rec % auxinput5_begin_y (id_id)
 grid_config_rec % auxinput5_begin_d          = model_config_rec % auxinput5_begin_d (id_id)
 grid_config_rec % auxinput5_begin_h          = model_config_rec % auxinput5_begin_h (id_id)
 grid_config_rec % auxinput5_begin_m          = model_config_rec % auxinput5_begin_m (id_id)
 grid_config_rec % auxinput5_begin_s          = model_config_rec % auxinput5_begin_s (id_id)
 grid_config_rec % auxinput5_begin            = model_config_rec % auxinput5_begin (id_id)
 grid_config_rec % auxinput5_end_y            = model_config_rec % auxinput5_end_y (id_id)
 grid_config_rec % auxinput5_end_d            = model_config_rec % auxinput5_end_d (id_id)
 grid_config_rec % auxinput5_end_h            = model_config_rec % auxinput5_end_h (id_id)
 grid_config_rec % auxinput5_end_m            = model_config_rec % auxinput5_end_m (id_id)
 grid_config_rec % auxinput5_end_s            = model_config_rec % auxinput5_end_s (id_id)
 grid_config_rec % auxinput5_end              = model_config_rec % auxinput5_end (id_id)
 grid_config_rec % io_form_auxinput5          = model_config_rec % io_form_auxinput5 
 grid_config_rec % frames_per_auxinput5       = model_config_rec % frames_per_auxinput5 (id_id)
 grid_config_rec % auxinput6_inname           = model_config_rec % auxinput6_inname 
 grid_config_rec % auxinput6_outname          = model_config_rec % auxinput6_outname 
 grid_config_rec % auxinput6_interval_y       = model_config_rec % auxinput6_interval_y (id_id)
 grid_config_rec % auxinput6_interval_d       = model_config_rec % auxinput6_interval_d (id_id)
 grid_config_rec % auxinput6_interval_h       = model_config_rec % auxinput6_interval_h (id_id)
 grid_config_rec % auxinput6_interval_m       = model_config_rec % auxinput6_interval_m (id_id)
 grid_config_rec % auxinput6_interval_s       = model_config_rec % auxinput6_interval_s (id_id)
 grid_config_rec % auxinput6_interval         = model_config_rec % auxinput6_interval (id_id)
 grid_config_rec % auxinput6_begin_y          = model_config_rec % auxinput6_begin_y (id_id)
 grid_config_rec % auxinput6_begin_d          = model_config_rec % auxinput6_begin_d (id_id)
 grid_config_rec % auxinput6_begin_h          = model_config_rec % auxinput6_begin_h (id_id)
 grid_config_rec % auxinput6_begin_m          = model_config_rec % auxinput6_begin_m (id_id)
 grid_config_rec % auxinput6_begin_s          = model_config_rec % auxinput6_begin_s (id_id)
 grid_config_rec % auxinput6_begin            = model_config_rec % auxinput6_begin (id_id)
 grid_config_rec % auxinput6_end_y            = model_config_rec % auxinput6_end_y (id_id)
 grid_config_rec % auxinput6_end_d            = model_config_rec % auxinput6_end_d (id_id)
 grid_config_rec % auxinput6_end_h            = model_config_rec % auxinput6_end_h (id_id)
 grid_config_rec % auxinput6_end_m            = model_config_rec % auxinput6_end_m (id_id)
 grid_config_rec % auxinput6_end_s            = model_config_rec % auxinput6_end_s (id_id)
 grid_config_rec % auxinput6_end              = model_config_rec % auxinput6_end (id_id)
 grid_config_rec % io_form_auxinput6          = model_config_rec % io_form_auxinput6 
 grid_config_rec % frames_per_auxinput6       = model_config_rec % frames_per_auxinput6 (id_id)
 grid_config_rec % auxinput7_inname           = model_config_rec % auxinput7_inname 
 grid_config_rec % auxinput7_outname          = model_config_rec % auxinput7_outname 
 grid_config_rec % auxinput7_interval_y       = model_config_rec % auxinput7_interval_y (id_id)
 grid_config_rec % auxinput7_interval_d       = model_config_rec % auxinput7_interval_d (id_id)
 grid_config_rec % auxinput7_interval_h       = model_config_rec % auxinput7_interval_h (id_id)
 grid_config_rec % auxinput7_interval_m       = model_config_rec % auxinput7_interval_m (id_id)
 grid_config_rec % auxinput7_interval_s       = model_config_rec % auxinput7_interval_s (id_id)
 grid_config_rec % auxinput7_interval         = model_config_rec % auxinput7_interval (id_id)
 grid_config_rec % auxinput7_begin_y          = model_config_rec % auxinput7_begin_y (id_id)
 grid_config_rec % auxinput7_begin_d          = model_config_rec % auxinput7_begin_d (id_id)
 grid_config_rec % auxinput7_begin_h          = model_config_rec % auxinput7_begin_h (id_id)
 grid_config_rec % auxinput7_begin_m          = model_config_rec % auxinput7_begin_m (id_id)
 grid_config_rec % auxinput7_begin_s          = model_config_rec % auxinput7_begin_s (id_id)
 grid_config_rec % auxinput7_begin            = model_config_rec % auxinput7_begin (id_id)
 grid_config_rec % auxinput7_end_y            = model_config_rec % auxinput7_end_y (id_id)
 grid_config_rec % auxinput7_end_d            = model_config_rec % auxinput7_end_d (id_id)
 grid_config_rec % auxinput7_end_h            = model_config_rec % auxinput7_end_h (id_id)
 grid_config_rec % auxinput7_end_m            = model_config_rec % auxinput7_end_m (id_id)
 grid_config_rec % auxinput7_end_s            = model_config_rec % auxinput7_end_s (id_id)
 grid_config_rec % auxinput7_end              = model_config_rec % auxinput7_end (id_id)
 grid_config_rec % io_form_auxinput7          = model_config_rec % io_form_auxinput7 
 grid_config_rec % frames_per_auxinput7       = model_config_rec % frames_per_auxinput7 (id_id)
 grid_config_rec % auxinput8_inname           = model_config_rec % auxinput8_inname 
 grid_config_rec % auxinput8_outname          = model_config_rec % auxinput8_outname 
 grid_config_rec % auxinput8_interval_y       = model_config_rec % auxinput8_interval_y (id_id)
 grid_config_rec % auxinput8_interval_d       = model_config_rec % auxinput8_interval_d (id_id)
 grid_config_rec % auxinput8_interval_h       = model_config_rec % auxinput8_interval_h (id_id)
 grid_config_rec % auxinput8_interval_m       = model_config_rec % auxinput8_interval_m (id_id)
 grid_config_rec % auxinput8_interval_s       = model_config_rec % auxinput8_interval_s (id_id)
 grid_config_rec % auxinput8_interval         = model_config_rec % auxinput8_interval (id_id)
 grid_config_rec % auxinput8_begin_y          = model_config_rec % auxinput8_begin_y (id_id)
 grid_config_rec % auxinput8_begin_d          = model_config_rec % auxinput8_begin_d (id_id)
 grid_config_rec % auxinput8_begin_h          = model_config_rec % auxinput8_begin_h (id_id)
 grid_config_rec % auxinput8_begin_m          = model_config_rec % auxinput8_begin_m (id_id)
 grid_config_rec % auxinput8_begin_s          = model_config_rec % auxinput8_begin_s (id_id)
 grid_config_rec % auxinput8_begin            = model_config_rec % auxinput8_begin (id_id)
 grid_config_rec % auxinput8_end_y            = model_config_rec % auxinput8_end_y (id_id)
 grid_config_rec % auxinput8_end_d            = model_config_rec % auxinput8_end_d (id_id)
 grid_config_rec % auxinput8_end_h            = model_config_rec % auxinput8_end_h (id_id)
 grid_config_rec % auxinput8_end_m            = model_config_rec % auxinput8_end_m (id_id)
 grid_config_rec % auxinput8_end_s            = model_config_rec % auxinput8_end_s (id_id)
 grid_config_rec % auxinput8_end              = model_config_rec % auxinput8_end (id_id)
 grid_config_rec % io_form_auxinput8          = model_config_rec % io_form_auxinput8 
 grid_config_rec % frames_per_auxinput8       = model_config_rec % frames_per_auxinput8 (id_id)
 grid_config_rec % auxinput9_inname           = model_config_rec % auxinput9_inname 
 grid_config_rec % auxinput9_outname          = model_config_rec % auxinput9_outname 
 grid_config_rec % auxinput9_interval_y       = model_config_rec % auxinput9_interval_y (id_id)
 grid_config_rec % auxinput9_interval_d       = model_config_rec % auxinput9_interval_d (id_id)
 grid_config_rec % auxinput9_interval_h       = model_config_rec % auxinput9_interval_h (id_id)
 grid_config_rec % auxinput9_interval_m       = model_config_rec % auxinput9_interval_m (id_id)
 grid_config_rec % auxinput9_interval_s       = model_config_rec % auxinput9_interval_s (id_id)
 grid_config_rec % auxinput9_interval         = model_config_rec % auxinput9_interval (id_id)
 grid_config_rec % auxinput9_begin_y          = model_config_rec % auxinput9_begin_y (id_id)
 grid_config_rec % auxinput9_begin_d          = model_config_rec % auxinput9_begin_d (id_id)
 grid_config_rec % auxinput9_begin_h          = model_config_rec % auxinput9_begin_h (id_id)
 grid_config_rec % auxinput9_begin_m          = model_config_rec % auxinput9_begin_m (id_id)
 grid_config_rec % auxinput9_begin_s          = model_config_rec % auxinput9_begin_s (id_id)
 grid_config_rec % auxinput9_begin            = model_config_rec % auxinput9_begin (id_id)
 grid_config_rec % auxinput9_end_y            = model_config_rec % auxinput9_end_y (id_id)
 grid_config_rec % auxinput9_end_d            = model_config_rec % auxinput9_end_d (id_id)
 grid_config_rec % auxinput9_end_h            = model_config_rec % auxinput9_end_h (id_id)
 grid_config_rec % auxinput9_end_m            = model_config_rec % auxinput9_end_m (id_id)
 grid_config_rec % auxinput9_end_s            = model_config_rec % auxinput9_end_s (id_id)
 grid_config_rec % auxinput9_end              = model_config_rec % auxinput9_end (id_id)
 grid_config_rec % io_form_auxinput9          = model_config_rec % io_form_auxinput9 
 grid_config_rec % frames_per_auxinput9       = model_config_rec % frames_per_auxinput9 (id_id)
 grid_config_rec % auxinput10_inname          = model_config_rec % auxinput10_inname 
 grid_config_rec % auxinput10_outname         = model_config_rec % auxinput10_outname 
 grid_config_rec % auxinput10_interval_y      = model_config_rec % auxinput10_interval_y (id_id)
 grid_config_rec % auxinput10_interval_d      = model_config_rec % auxinput10_interval_d (id_id)
 grid_config_rec % auxinput10_interval_h      = model_config_rec % auxinput10_interval_h (id_id)
 grid_config_rec % auxinput10_interval_m      = model_config_rec % auxinput10_interval_m (id_id)
 grid_config_rec % auxinput10_interval_s      = model_config_rec % auxinput10_interval_s (id_id)
 grid_config_rec % auxinput10_interval        = model_config_rec % auxinput10_interval (id_id)
 grid_config_rec % auxinput10_begin_y         = model_config_rec % auxinput10_begin_y (id_id)
 grid_config_rec % auxinput10_begin_d         = model_config_rec % auxinput10_begin_d (id_id)
 grid_config_rec % auxinput10_begin_h         = model_config_rec % auxinput10_begin_h (id_id)
 grid_config_rec % auxinput10_begin_m         = model_config_rec % auxinput10_begin_m (id_id)
 grid_config_rec % auxinput10_begin_s         = model_config_rec % auxinput10_begin_s (id_id)
 grid_config_rec % auxinput10_begin           = model_config_rec % auxinput10_begin (id_id)
 grid_config_rec % auxinput10_end_y           = model_config_rec % auxinput10_end_y (id_id)
 grid_config_rec % auxinput10_end_d           = model_config_rec % auxinput10_end_d (id_id)
 grid_config_rec % auxinput10_end_h           = model_config_rec % auxinput10_end_h (id_id)
 grid_config_rec % auxinput10_end_m           = model_config_rec % auxinput10_end_m (id_id)
 grid_config_rec % auxinput10_end_s           = model_config_rec % auxinput10_end_s (id_id)
 grid_config_rec % auxinput10_end             = model_config_rec % auxinput10_end (id_id)
 grid_config_rec % io_form_auxinput10         = model_config_rec % io_form_auxinput10 
 grid_config_rec % frames_per_auxinput10      = model_config_rec % frames_per_auxinput10 (id_id)
 grid_config_rec % auxinput11_inname          = model_config_rec % auxinput11_inname 
 grid_config_rec % auxinput11_outname         = model_config_rec % auxinput11_outname 
 grid_config_rec % auxinput11_interval_y      = model_config_rec % auxinput11_interval_y (id_id)
 grid_config_rec % auxinput11_interval_d      = model_config_rec % auxinput11_interval_d (id_id)
 grid_config_rec % auxinput11_interval_h      = model_config_rec % auxinput11_interval_h (id_id)
 grid_config_rec % auxinput11_interval_m      = model_config_rec % auxinput11_interval_m (id_id)
 grid_config_rec % auxinput11_interval_s      = model_config_rec % auxinput11_interval_s (id_id)
 grid_config_rec % auxinput11_interval        = model_config_rec % auxinput11_interval (id_id)
 grid_config_rec % auxinput11_begin_y         = model_config_rec % auxinput11_begin_y (id_id)
 grid_config_rec % auxinput11_begin_d         = model_config_rec % auxinput11_begin_d (id_id)
 grid_config_rec % auxinput11_begin_h         = model_config_rec % auxinput11_begin_h (id_id)
 grid_config_rec % auxinput11_begin_m         = model_config_rec % auxinput11_begin_m (id_id)
 grid_config_rec % auxinput11_begin_s         = model_config_rec % auxinput11_begin_s (id_id)
 grid_config_rec % auxinput11_begin           = model_config_rec % auxinput11_begin (id_id)
 grid_config_rec % auxinput11_end_y           = model_config_rec % auxinput11_end_y (id_id)
 grid_config_rec % auxinput11_end_d           = model_config_rec % auxinput11_end_d (id_id)
 grid_config_rec % auxinput11_end_h           = model_config_rec % auxinput11_end_h (id_id)
 grid_config_rec % auxinput11_end_m           = model_config_rec % auxinput11_end_m (id_id)
 grid_config_rec % auxinput11_end_s           = model_config_rec % auxinput11_end_s (id_id)
 grid_config_rec % auxinput11_end             = model_config_rec % auxinput11_end (id_id)
 grid_config_rec % io_form_auxinput11         = model_config_rec % io_form_auxinput11 
 grid_config_rec % frames_per_auxinput11      = model_config_rec % frames_per_auxinput11 (id_id)
 grid_config_rec % auxinput12_inname          = model_config_rec % auxinput12_inname 
 grid_config_rec % auxinput12_outname         = model_config_rec % auxinput12_outname 
 grid_config_rec % auxinput12_interval_y      = model_config_rec % auxinput12_interval_y (id_id)
 grid_config_rec % auxinput12_interval_d      = model_config_rec % auxinput12_interval_d (id_id)
 grid_config_rec % auxinput12_interval_h      = model_config_rec % auxinput12_interval_h (id_id)
 grid_config_rec % auxinput12_interval_m      = model_config_rec % auxinput12_interval_m (id_id)
 grid_config_rec % auxinput12_interval_s      = model_config_rec % auxinput12_interval_s (id_id)
 grid_config_rec % auxinput12_interval        = model_config_rec % auxinput12_interval (id_id)
 grid_config_rec % auxinput12_begin_y         = model_config_rec % auxinput12_begin_y (id_id)
 grid_config_rec % auxinput12_begin_d         = model_config_rec % auxinput12_begin_d (id_id)
 grid_config_rec % auxinput12_begin_h         = model_config_rec % auxinput12_begin_h (id_id)
 grid_config_rec % auxinput12_begin_m         = model_config_rec % auxinput12_begin_m (id_id)
 grid_config_rec % auxinput12_begin_s         = model_config_rec % auxinput12_begin_s (id_id)
 grid_config_rec % auxinput12_begin           = model_config_rec % auxinput12_begin (id_id)
 grid_config_rec % auxinput12_end_y           = model_config_rec % auxinput12_end_y (id_id)
 grid_config_rec % auxinput12_end_d           = model_config_rec % auxinput12_end_d (id_id)
 grid_config_rec % auxinput12_end_h           = model_config_rec % auxinput12_end_h (id_id)
 grid_config_rec % auxinput12_end_m           = model_config_rec % auxinput12_end_m (id_id)
 grid_config_rec % auxinput12_end_s           = model_config_rec % auxinput12_end_s (id_id)
 grid_config_rec % auxinput12_end             = model_config_rec % auxinput12_end (id_id)
 grid_config_rec % io_form_auxinput12         = model_config_rec % io_form_auxinput12 
 grid_config_rec % frames_per_auxinput12      = model_config_rec % frames_per_auxinput12 (id_id)
 grid_config_rec % auxinput13_inname          = model_config_rec % auxinput13_inname 
 grid_config_rec % auxinput13_outname         = model_config_rec % auxinput13_outname 
 grid_config_rec % auxinput13_interval_y      = model_config_rec % auxinput13_interval_y (id_id)
 grid_config_rec % auxinput13_interval_d      = model_config_rec % auxinput13_interval_d (id_id)
 grid_config_rec % auxinput13_interval_h      = model_config_rec % auxinput13_interval_h (id_id)
 grid_config_rec % auxinput13_interval_m      = model_config_rec % auxinput13_interval_m (id_id)
 grid_config_rec % auxinput13_interval_s      = model_config_rec % auxinput13_interval_s (id_id)
 grid_config_rec % auxinput13_interval        = model_config_rec % auxinput13_interval (id_id)
 grid_config_rec % auxinput13_begin_y         = model_config_rec % auxinput13_begin_y (id_id)
 grid_config_rec % auxinput13_begin_d         = model_config_rec % auxinput13_begin_d (id_id)
 grid_config_rec % auxinput13_begin_h         = model_config_rec % auxinput13_begin_h (id_id)
 grid_config_rec % auxinput13_begin_m         = model_config_rec % auxinput13_begin_m (id_id)
 grid_config_rec % auxinput13_begin_s         = model_config_rec % auxinput13_begin_s (id_id)
 grid_config_rec % auxinput13_begin           = model_config_rec % auxinput13_begin (id_id)
 grid_config_rec % auxinput13_end_y           = model_config_rec % auxinput13_end_y (id_id)
 grid_config_rec % auxinput13_end_d           = model_config_rec % auxinput13_end_d (id_id)
 grid_config_rec % auxinput13_end_h           = model_config_rec % auxinput13_end_h (id_id)
 grid_config_rec % auxinput13_end_m           = model_config_rec % auxinput13_end_m (id_id)
 grid_config_rec % auxinput13_end_s           = model_config_rec % auxinput13_end_s (id_id)
 grid_config_rec % auxinput13_end             = model_config_rec % auxinput13_end (id_id)
 grid_config_rec % io_form_auxinput13         = model_config_rec % io_form_auxinput13 
 grid_config_rec % frames_per_auxinput13      = model_config_rec % frames_per_auxinput13 (id_id)
 grid_config_rec % auxinput14_inname          = model_config_rec % auxinput14_inname 
 grid_config_rec % auxinput14_outname         = model_config_rec % auxinput14_outname 
 grid_config_rec % auxinput14_interval_y      = model_config_rec % auxinput14_interval_y (id_id)
 grid_config_rec % auxinput14_interval_d      = model_config_rec % auxinput14_interval_d (id_id)
 grid_config_rec % auxinput14_interval_h      = model_config_rec % auxinput14_interval_h (id_id)
 grid_config_rec % auxinput14_interval_m      = model_config_rec % auxinput14_interval_m (id_id)
 grid_config_rec % auxinput14_interval_s      = model_config_rec % auxinput14_interval_s (id_id)
 grid_config_rec % auxinput14_interval        = model_config_rec % auxinput14_interval (id_id)
 grid_config_rec % auxinput14_begin_y         = model_config_rec % auxinput14_begin_y (id_id)
 grid_config_rec % auxinput14_begin_d         = model_config_rec % auxinput14_begin_d (id_id)
 grid_config_rec % auxinput14_begin_h         = model_config_rec % auxinput14_begin_h (id_id)
 grid_config_rec % auxinput14_begin_m         = model_config_rec % auxinput14_begin_m (id_id)
 grid_config_rec % auxinput14_begin_s         = model_config_rec % auxinput14_begin_s (id_id)
 grid_config_rec % auxinput14_begin           = model_config_rec % auxinput14_begin (id_id)
 grid_config_rec % auxinput14_end_y           = model_config_rec % auxinput14_end_y (id_id)
 grid_config_rec % auxinput14_end_d           = model_config_rec % auxinput14_end_d (id_id)
 grid_config_rec % auxinput14_end_h           = model_config_rec % auxinput14_end_h (id_id)
 grid_config_rec % auxinput14_end_m           = model_config_rec % auxinput14_end_m (id_id)
 grid_config_rec % auxinput14_end_s           = model_config_rec % auxinput14_end_s (id_id)
 grid_config_rec % auxinput14_end             = model_config_rec % auxinput14_end (id_id)
 grid_config_rec % io_form_auxinput14         = model_config_rec % io_form_auxinput14 
 grid_config_rec % frames_per_auxinput14      = model_config_rec % frames_per_auxinput14 (id_id)
 grid_config_rec % auxinput15_inname          = model_config_rec % auxinput15_inname 
 grid_config_rec % auxinput15_outname         = model_config_rec % auxinput15_outname 
 grid_config_rec % auxinput15_interval_y      = model_config_rec % auxinput15_interval_y (id_id)
 grid_config_rec % auxinput15_interval_d      = model_config_rec % auxinput15_interval_d (id_id)
 grid_config_rec % auxinput15_interval_h      = model_config_rec % auxinput15_interval_h (id_id)
 grid_config_rec % auxinput15_interval_m      = model_config_rec % auxinput15_interval_m (id_id)
 grid_config_rec % auxinput15_interval_s      = model_config_rec % auxinput15_interval_s (id_id)
 grid_config_rec % auxinput15_interval        = model_config_rec % auxinput15_interval (id_id)
 grid_config_rec % auxinput15_begin_y         = model_config_rec % auxinput15_begin_y (id_id)
 grid_config_rec % auxinput15_begin_d         = model_config_rec % auxinput15_begin_d (id_id)
 grid_config_rec % auxinput15_begin_h         = model_config_rec % auxinput15_begin_h (id_id)
 grid_config_rec % auxinput15_begin_m         = model_config_rec % auxinput15_begin_m (id_id)
 grid_config_rec % auxinput15_begin_s         = model_config_rec % auxinput15_begin_s (id_id)
 grid_config_rec % auxinput15_begin           = model_config_rec % auxinput15_begin (id_id)
 grid_config_rec % auxinput15_end_y           = model_config_rec % auxinput15_end_y (id_id)
 grid_config_rec % auxinput15_end_d           = model_config_rec % auxinput15_end_d (id_id)
 grid_config_rec % auxinput15_end_h           = model_config_rec % auxinput15_end_h (id_id)
 grid_config_rec % auxinput15_end_m           = model_config_rec % auxinput15_end_m (id_id)
 grid_config_rec % auxinput15_end_s           = model_config_rec % auxinput15_end_s (id_id)
 grid_config_rec % auxinput15_end             = model_config_rec % auxinput15_end (id_id)
 grid_config_rec % io_form_auxinput15         = model_config_rec % io_form_auxinput15 
 grid_config_rec % frames_per_auxinput15      = model_config_rec % frames_per_auxinput15 (id_id)
 grid_config_rec % auxinput16_inname          = model_config_rec % auxinput16_inname 
 grid_config_rec % auxinput16_outname         = model_config_rec % auxinput16_outname 
 grid_config_rec % auxinput16_interval_y      = model_config_rec % auxinput16_interval_y (id_id)
 grid_config_rec % auxinput16_interval_d      = model_config_rec % auxinput16_interval_d (id_id)
 grid_config_rec % auxinput16_interval_h      = model_config_rec % auxinput16_interval_h (id_id)
 grid_config_rec % auxinput16_interval_m      = model_config_rec % auxinput16_interval_m (id_id)
 grid_config_rec % auxinput16_interval_s      = model_config_rec % auxinput16_interval_s (id_id)
 grid_config_rec % auxinput16_interval        = model_config_rec % auxinput16_interval (id_id)
 grid_config_rec % auxinput16_begin_y         = model_config_rec % auxinput16_begin_y (id_id)
 grid_config_rec % auxinput16_begin_d         = model_config_rec % auxinput16_begin_d (id_id)
 grid_config_rec % auxinput16_begin_h         = model_config_rec % auxinput16_begin_h (id_id)
 grid_config_rec % auxinput16_begin_m         = model_config_rec % auxinput16_begin_m (id_id)
 grid_config_rec % auxinput16_begin_s         = model_config_rec % auxinput16_begin_s (id_id)
 grid_config_rec % auxinput16_begin           = model_config_rec % auxinput16_begin (id_id)
 grid_config_rec % auxinput16_end_y           = model_config_rec % auxinput16_end_y (id_id)
 grid_config_rec % auxinput16_end_d           = model_config_rec % auxinput16_end_d (id_id)
 grid_config_rec % auxinput16_end_h           = model_config_rec % auxinput16_end_h (id_id)
 grid_config_rec % auxinput16_end_m           = model_config_rec % auxinput16_end_m (id_id)
 grid_config_rec % auxinput16_end_s           = model_config_rec % auxinput16_end_s (id_id)
 grid_config_rec % auxinput16_end             = model_config_rec % auxinput16_end (id_id)
 grid_config_rec % io_form_auxinput16         = model_config_rec % io_form_auxinput16 
 grid_config_rec % frames_per_auxinput16      = model_config_rec % frames_per_auxinput16 (id_id)
 grid_config_rec % auxinput17_inname          = model_config_rec % auxinput17_inname 
 grid_config_rec % auxinput17_outname         = model_config_rec % auxinput17_outname 
 grid_config_rec % auxinput17_interval_y      = model_config_rec % auxinput17_interval_y (id_id)
 grid_config_rec % auxinput17_interval_d      = model_config_rec % auxinput17_interval_d (id_id)
 grid_config_rec % auxinput17_interval_h      = model_config_rec % auxinput17_interval_h (id_id)
 grid_config_rec % auxinput17_interval_m      = model_config_rec % auxinput17_interval_m (id_id)
 grid_config_rec % auxinput17_interval_s      = model_config_rec % auxinput17_interval_s (id_id)
 grid_config_rec % auxinput17_interval        = model_config_rec % auxinput17_interval (id_id)
 grid_config_rec % auxinput17_begin_y         = model_config_rec % auxinput17_begin_y (id_id)
 grid_config_rec % auxinput17_begin_d         = model_config_rec % auxinput17_begin_d (id_id)
 grid_config_rec % auxinput17_begin_h         = model_config_rec % auxinput17_begin_h (id_id)
 grid_config_rec % auxinput17_begin_m         = model_config_rec % auxinput17_begin_m (id_id)
 grid_config_rec % auxinput17_begin_s         = model_config_rec % auxinput17_begin_s (id_id)
 grid_config_rec % auxinput17_begin           = model_config_rec % auxinput17_begin (id_id)
 grid_config_rec % auxinput17_end_y           = model_config_rec % auxinput17_end_y (id_id)
 grid_config_rec % auxinput17_end_d           = model_config_rec % auxinput17_end_d (id_id)
 grid_config_rec % auxinput17_end_h           = model_config_rec % auxinput17_end_h (id_id)
 grid_config_rec % auxinput17_end_m           = model_config_rec % auxinput17_end_m (id_id)
 grid_config_rec % auxinput17_end_s           = model_config_rec % auxinput17_end_s (id_id)
 grid_config_rec % auxinput17_end             = model_config_rec % auxinput17_end (id_id)
 grid_config_rec % io_form_auxinput17         = model_config_rec % io_form_auxinput17 
 grid_config_rec % frames_per_auxinput17      = model_config_rec % frames_per_auxinput17 (id_id)
 grid_config_rec % auxinput18_inname          = model_config_rec % auxinput18_inname 
 grid_config_rec % auxinput18_outname         = model_config_rec % auxinput18_outname 
 grid_config_rec % auxinput18_interval_y      = model_config_rec % auxinput18_interval_y (id_id)
 grid_config_rec % auxinput18_interval_d      = model_config_rec % auxinput18_interval_d (id_id)
 grid_config_rec % auxinput18_interval_h      = model_config_rec % auxinput18_interval_h (id_id)
 grid_config_rec % auxinput18_interval_m      = model_config_rec % auxinput18_interval_m (id_id)
 grid_config_rec % auxinput18_interval_s      = model_config_rec % auxinput18_interval_s (id_id)
 grid_config_rec % auxinput18_interval        = model_config_rec % auxinput18_interval (id_id)
 grid_config_rec % auxinput18_begin_y         = model_config_rec % auxinput18_begin_y (id_id)
 grid_config_rec % auxinput18_begin_d         = model_config_rec % auxinput18_begin_d (id_id)
 grid_config_rec % auxinput18_begin_h         = model_config_rec % auxinput18_begin_h (id_id)
 grid_config_rec % auxinput18_begin_m         = model_config_rec % auxinput18_begin_m (id_id)
 grid_config_rec % auxinput18_begin_s         = model_config_rec % auxinput18_begin_s (id_id)
 grid_config_rec % auxinput18_begin           = model_config_rec % auxinput18_begin (id_id)
 grid_config_rec % auxinput18_end_y           = model_config_rec % auxinput18_end_y (id_id)
 grid_config_rec % auxinput18_end_d           = model_config_rec % auxinput18_end_d (id_id)
 grid_config_rec % auxinput18_end_h           = model_config_rec % auxinput18_end_h (id_id)
 grid_config_rec % auxinput18_end_m           = model_config_rec % auxinput18_end_m (id_id)
 grid_config_rec % auxinput18_end_s           = model_config_rec % auxinput18_end_s (id_id)
 grid_config_rec % auxinput18_end             = model_config_rec % auxinput18_end (id_id)
 grid_config_rec % io_form_auxinput18         = model_config_rec % io_form_auxinput18 
 grid_config_rec % frames_per_auxinput18      = model_config_rec % frames_per_auxinput18 (id_id)
 grid_config_rec % auxinput19_inname          = model_config_rec % auxinput19_inname 
 grid_config_rec % auxinput19_outname         = model_config_rec % auxinput19_outname 
 grid_config_rec % auxinput19_interval_y      = model_config_rec % auxinput19_interval_y (id_id)
 grid_config_rec % auxinput19_interval_d      = model_config_rec % auxinput19_interval_d (id_id)
 grid_config_rec % auxinput19_interval_h      = model_config_rec % auxinput19_interval_h (id_id)
 grid_config_rec % auxinput19_interval_m      = model_config_rec % auxinput19_interval_m (id_id)
 grid_config_rec % auxinput19_interval_s      = model_config_rec % auxinput19_interval_s (id_id)
 grid_config_rec % auxinput19_interval        = model_config_rec % auxinput19_interval (id_id)
 grid_config_rec % auxinput19_begin_y         = model_config_rec % auxinput19_begin_y (id_id)
 grid_config_rec % auxinput19_begin_d         = model_config_rec % auxinput19_begin_d (id_id)
 grid_config_rec % auxinput19_begin_h         = model_config_rec % auxinput19_begin_h (id_id)
 grid_config_rec % auxinput19_begin_m         = model_config_rec % auxinput19_begin_m (id_id)
 grid_config_rec % auxinput19_begin_s         = model_config_rec % auxinput19_begin_s (id_id)
 grid_config_rec % auxinput19_begin           = model_config_rec % auxinput19_begin (id_id)
 grid_config_rec % auxinput19_end_y           = model_config_rec % auxinput19_end_y (id_id)
 grid_config_rec % auxinput19_end_d           = model_config_rec % auxinput19_end_d (id_id)
 grid_config_rec % auxinput19_end_h           = model_config_rec % auxinput19_end_h (id_id)
 grid_config_rec % auxinput19_end_m           = model_config_rec % auxinput19_end_m (id_id)
 grid_config_rec % auxinput19_end_s           = model_config_rec % auxinput19_end_s (id_id)
 grid_config_rec % auxinput19_end             = model_config_rec % auxinput19_end (id_id)
 grid_config_rec % io_form_auxinput19         = model_config_rec % io_form_auxinput19 
 grid_config_rec % frames_per_auxinput19      = model_config_rec % frames_per_auxinput19 (id_id)
 grid_config_rec % auxinput20_inname          = model_config_rec % auxinput20_inname 
 grid_config_rec % auxinput20_outname         = model_config_rec % auxinput20_outname 
 grid_config_rec % auxinput20_interval_y      = model_config_rec % auxinput20_interval_y (id_id)
 grid_config_rec % auxinput20_interval_d      = model_config_rec % auxinput20_interval_d (id_id)
 grid_config_rec % auxinput20_interval_h      = model_config_rec % auxinput20_interval_h (id_id)
 grid_config_rec % auxinput20_interval_m      = model_config_rec % auxinput20_interval_m (id_id)
 grid_config_rec % auxinput20_interval_s      = model_config_rec % auxinput20_interval_s (id_id)
 grid_config_rec % auxinput20_interval        = model_config_rec % auxinput20_interval (id_id)
 grid_config_rec % auxinput20_begin_y         = model_config_rec % auxinput20_begin_y (id_id)
 grid_config_rec % auxinput20_begin_d         = model_config_rec % auxinput20_begin_d (id_id)
 grid_config_rec % auxinput20_begin_h         = model_config_rec % auxinput20_begin_h (id_id)
 grid_config_rec % auxinput20_begin_m         = model_config_rec % auxinput20_begin_m (id_id)
 grid_config_rec % auxinput20_begin_s         = model_config_rec % auxinput20_begin_s (id_id)
 grid_config_rec % auxinput20_begin           = model_config_rec % auxinput20_begin (id_id)
 grid_config_rec % auxinput20_end_y           = model_config_rec % auxinput20_end_y (id_id)
 grid_config_rec % auxinput20_end_d           = model_config_rec % auxinput20_end_d (id_id)
 grid_config_rec % auxinput20_end_h           = model_config_rec % auxinput20_end_h (id_id)
 grid_config_rec % auxinput20_end_m           = model_config_rec % auxinput20_end_m (id_id)
 grid_config_rec % auxinput20_end_s           = model_config_rec % auxinput20_end_s (id_id)
 grid_config_rec % auxinput20_end             = model_config_rec % auxinput20_end (id_id)
 grid_config_rec % io_form_auxinput20         = model_config_rec % io_form_auxinput20 
 grid_config_rec % frames_per_auxinput20      = model_config_rec % frames_per_auxinput20 (id_id)
 grid_config_rec % auxinput21_inname          = model_config_rec % auxinput21_inname 
 grid_config_rec % auxinput21_outname         = model_config_rec % auxinput21_outname 
 grid_config_rec % auxinput21_interval_y      = model_config_rec % auxinput21_interval_y (id_id)
 grid_config_rec % auxinput21_interval_d      = model_config_rec % auxinput21_interval_d (id_id)
 grid_config_rec % auxinput21_interval_h      = model_config_rec % auxinput21_interval_h (id_id)
 grid_config_rec % auxinput21_interval_m      = model_config_rec % auxinput21_interval_m (id_id)
 grid_config_rec % auxinput21_interval_s      = model_config_rec % auxinput21_interval_s (id_id)
 grid_config_rec % auxinput21_interval        = model_config_rec % auxinput21_interval (id_id)
 grid_config_rec % auxinput21_begin_y         = model_config_rec % auxinput21_begin_y (id_id)
 grid_config_rec % auxinput21_begin_d         = model_config_rec % auxinput21_begin_d (id_id)
 grid_config_rec % auxinput21_begin_h         = model_config_rec % auxinput21_begin_h (id_id)
 grid_config_rec % auxinput21_begin_m         = model_config_rec % auxinput21_begin_m (id_id)
 grid_config_rec % auxinput21_begin_s         = model_config_rec % auxinput21_begin_s (id_id)
 grid_config_rec % auxinput21_begin           = model_config_rec % auxinput21_begin (id_id)
 grid_config_rec % auxinput21_end_y           = model_config_rec % auxinput21_end_y (id_id)
 grid_config_rec % auxinput21_end_d           = model_config_rec % auxinput21_end_d (id_id)
 grid_config_rec % auxinput21_end_h           = model_config_rec % auxinput21_end_h (id_id)
 grid_config_rec % auxinput21_end_m           = model_config_rec % auxinput21_end_m (id_id)
 grid_config_rec % auxinput21_end_s           = model_config_rec % auxinput21_end_s (id_id)
 grid_config_rec % auxinput21_end             = model_config_rec % auxinput21_end (id_id)
 grid_config_rec % io_form_auxinput21         = model_config_rec % io_form_auxinput21 
 grid_config_rec % frames_per_auxinput21      = model_config_rec % frames_per_auxinput21 (id_id)
 grid_config_rec % auxinput22_inname          = model_config_rec % auxinput22_inname 
 grid_config_rec % auxinput22_outname         = model_config_rec % auxinput22_outname 
 grid_config_rec % auxinput22_interval_y      = model_config_rec % auxinput22_interval_y (id_id)
 grid_config_rec % auxinput22_interval_d      = model_config_rec % auxinput22_interval_d (id_id)
 grid_config_rec % auxinput22_interval_h      = model_config_rec % auxinput22_interval_h (id_id)
 grid_config_rec % auxinput22_interval_m      = model_config_rec % auxinput22_interval_m (id_id)
 grid_config_rec % auxinput22_interval_s      = model_config_rec % auxinput22_interval_s (id_id)
 grid_config_rec % auxinput22_interval        = model_config_rec % auxinput22_interval (id_id)
 grid_config_rec % auxinput22_begin_y         = model_config_rec % auxinput22_begin_y (id_id)
 grid_config_rec % auxinput22_begin_d         = model_config_rec % auxinput22_begin_d (id_id)
 grid_config_rec % auxinput22_begin_h         = model_config_rec % auxinput22_begin_h (id_id)
 grid_config_rec % auxinput22_begin_m         = model_config_rec % auxinput22_begin_m (id_id)
 grid_config_rec % auxinput22_begin_s         = model_config_rec % auxinput22_begin_s (id_id)
 grid_config_rec % auxinput22_begin           = model_config_rec % auxinput22_begin (id_id)
 grid_config_rec % auxinput22_end_y           = model_config_rec % auxinput22_end_y (id_id)
 grid_config_rec % auxinput22_end_d           = model_config_rec % auxinput22_end_d (id_id)
 grid_config_rec % auxinput22_end_h           = model_config_rec % auxinput22_end_h (id_id)
 grid_config_rec % auxinput22_end_m           = model_config_rec % auxinput22_end_m (id_id)
 grid_config_rec % auxinput22_end_s           = model_config_rec % auxinput22_end_s (id_id)
 grid_config_rec % auxinput22_end             = model_config_rec % auxinput22_end (id_id)
 grid_config_rec % io_form_auxinput22         = model_config_rec % io_form_auxinput22 
 grid_config_rec % frames_per_auxinput22      = model_config_rec % frames_per_auxinput22 (id_id)
 grid_config_rec % auxinput23_inname          = model_config_rec % auxinput23_inname 
 grid_config_rec % auxinput23_outname         = model_config_rec % auxinput23_outname 
 grid_config_rec % auxinput23_interval_y      = model_config_rec % auxinput23_interval_y (id_id)
 grid_config_rec % auxinput23_interval_d      = model_config_rec % auxinput23_interval_d (id_id)
 grid_config_rec % auxinput23_interval_h      = model_config_rec % auxinput23_interval_h (id_id)
 grid_config_rec % auxinput23_interval_m      = model_config_rec % auxinput23_interval_m (id_id)
 grid_config_rec % auxinput23_interval_s      = model_config_rec % auxinput23_interval_s (id_id)
 grid_config_rec % auxinput23_interval        = model_config_rec % auxinput23_interval (id_id)
 grid_config_rec % auxinput23_begin_y         = model_config_rec % auxinput23_begin_y (id_id)
 grid_config_rec % auxinput23_begin_d         = model_config_rec % auxinput23_begin_d (id_id)
 grid_config_rec % auxinput23_begin_h         = model_config_rec % auxinput23_begin_h (id_id)
 grid_config_rec % auxinput23_begin_m         = model_config_rec % auxinput23_begin_m (id_id)
 grid_config_rec % auxinput23_begin_s         = model_config_rec % auxinput23_begin_s (id_id)
 grid_config_rec % auxinput23_begin           = model_config_rec % auxinput23_begin (id_id)
 grid_config_rec % auxinput23_end_y           = model_config_rec % auxinput23_end_y (id_id)
 grid_config_rec % auxinput23_end_d           = model_config_rec % auxinput23_end_d (id_id)
 grid_config_rec % auxinput23_end_h           = model_config_rec % auxinput23_end_h (id_id)
 grid_config_rec % auxinput23_end_m           = model_config_rec % auxinput23_end_m (id_id)
 grid_config_rec % auxinput23_end_s           = model_config_rec % auxinput23_end_s (id_id)
 grid_config_rec % auxinput23_end             = model_config_rec % auxinput23_end (id_id)
 grid_config_rec % io_form_auxinput23         = model_config_rec % io_form_auxinput23 
 grid_config_rec % frames_per_auxinput23      = model_config_rec % frames_per_auxinput23 (id_id)
 grid_config_rec % auxinput24_inname          = model_config_rec % auxinput24_inname 
 grid_config_rec % auxinput24_outname         = model_config_rec % auxinput24_outname 
 grid_config_rec % auxinput24_interval_y      = model_config_rec % auxinput24_interval_y (id_id)
 grid_config_rec % auxinput24_interval_d      = model_config_rec % auxinput24_interval_d (id_id)
 grid_config_rec % auxinput24_interval_h      = model_config_rec % auxinput24_interval_h (id_id)
 grid_config_rec % auxinput24_interval_m      = model_config_rec % auxinput24_interval_m (id_id)
 grid_config_rec % auxinput24_interval_s      = model_config_rec % auxinput24_interval_s (id_id)
 grid_config_rec % auxinput24_interval        = model_config_rec % auxinput24_interval (id_id)
 grid_config_rec % auxinput24_begin_y         = model_config_rec % auxinput24_begin_y (id_id)
 grid_config_rec % auxinput24_begin_d         = model_config_rec % auxinput24_begin_d (id_id)
 grid_config_rec % auxinput24_begin_h         = model_config_rec % auxinput24_begin_h (id_id)
 grid_config_rec % auxinput24_begin_m         = model_config_rec % auxinput24_begin_m (id_id)
 grid_config_rec % auxinput24_begin_s         = model_config_rec % auxinput24_begin_s (id_id)
 grid_config_rec % auxinput24_begin           = model_config_rec % auxinput24_begin (id_id)
 grid_config_rec % auxinput24_end_y           = model_config_rec % auxinput24_end_y (id_id)
 grid_config_rec % auxinput24_end_d           = model_config_rec % auxinput24_end_d (id_id)
 grid_config_rec % auxinput24_end_h           = model_config_rec % auxinput24_end_h (id_id)
 grid_config_rec % auxinput24_end_m           = model_config_rec % auxinput24_end_m (id_id)
 grid_config_rec % auxinput24_end_s           = model_config_rec % auxinput24_end_s (id_id)
 grid_config_rec % auxinput24_end             = model_config_rec % auxinput24_end (id_id)
 grid_config_rec % io_form_auxinput24         = model_config_rec % io_form_auxinput24 
 grid_config_rec % frames_per_auxinput24      = model_config_rec % frames_per_auxinput24 (id_id)
 grid_config_rec % history_interval           = model_config_rec % history_interval (id_id)
 grid_config_rec % frames_per_outfile         = model_config_rec % frames_per_outfile (id_id)
 grid_config_rec % restart                    = model_config_rec % restart 
 grid_config_rec % restart_interval           = model_config_rec % restart_interval 
 grid_config_rec % io_form_input              = model_config_rec % io_form_input 
 grid_config_rec % io_form_history            = model_config_rec % io_form_history 
 grid_config_rec % io_form_restart            = model_config_rec % io_form_restart 
 grid_config_rec % io_form_boundary           = model_config_rec % io_form_boundary 
 grid_config_rec % debug_level                = model_config_rec % debug_level 
 grid_config_rec % self_test_domain           = model_config_rec % self_test_domain 
 grid_config_rec % history_outname            = model_config_rec % history_outname 
 grid_config_rec % history_inname             = model_config_rec % history_inname 
 grid_config_rec % use_netcdf_classic         = model_config_rec % use_netcdf_classic 
 grid_config_rec % history_interval_d         = model_config_rec % history_interval_d (id_id)
 grid_config_rec % history_interval_h         = model_config_rec % history_interval_h (id_id)
 grid_config_rec % history_interval_m         = model_config_rec % history_interval_m (id_id)
 grid_config_rec % history_interval_s         = model_config_rec % history_interval_s (id_id)
 grid_config_rec % inputout_interval_d        = model_config_rec % inputout_interval_d (id_id)
 grid_config_rec % inputout_interval_h        = model_config_rec % inputout_interval_h (id_id)
 grid_config_rec % inputout_interval_m        = model_config_rec % inputout_interval_m (id_id)
 grid_config_rec % inputout_interval_s        = model_config_rec % inputout_interval_s (id_id)
 grid_config_rec % inputout_interval          = model_config_rec % inputout_interval (id_id)
 grid_config_rec % restart_interval_d         = model_config_rec % restart_interval_d 
 grid_config_rec % restart_interval_h         = model_config_rec % restart_interval_h 
 grid_config_rec % restart_interval_m         = model_config_rec % restart_interval_m 
 grid_config_rec % restart_interval_s         = model_config_rec % restart_interval_s 
 grid_config_rec % history_begin_y            = model_config_rec % history_begin_y (id_id)
 grid_config_rec % history_begin_d            = model_config_rec % history_begin_d (id_id)
 grid_config_rec % history_begin_h            = model_config_rec % history_begin_h (id_id)
 grid_config_rec % history_begin_m            = model_config_rec % history_begin_m (id_id)
 grid_config_rec % history_begin_s            = model_config_rec % history_begin_s (id_id)
 grid_config_rec % history_begin              = model_config_rec % history_begin (id_id)
 grid_config_rec % inputout_begin_y           = model_config_rec % inputout_begin_y (id_id)
 grid_config_rec % inputout_begin_d           = model_config_rec % inputout_begin_d (id_id)
 grid_config_rec % inputout_begin_h           = model_config_rec % inputout_begin_h (id_id)
 grid_config_rec % inputout_begin_m           = model_config_rec % inputout_begin_m (id_id)
 grid_config_rec % inputout_begin_s           = model_config_rec % inputout_begin_s (id_id)
 grid_config_rec % restart_begin_y            = model_config_rec % restart_begin_y 
 grid_config_rec % restart_begin_d            = model_config_rec % restart_begin_d 
 grid_config_rec % restart_begin_h            = model_config_rec % restart_begin_h 
 grid_config_rec % restart_begin_m            = model_config_rec % restart_begin_m 
 grid_config_rec % restart_begin_s            = model_config_rec % restart_begin_s 
 grid_config_rec % restart_begin              = model_config_rec % restart_begin 
 grid_config_rec % history_end_y              = model_config_rec % history_end_y (id_id)
 grid_config_rec % history_end_d              = model_config_rec % history_end_d (id_id)
 grid_config_rec % history_end_h              = model_config_rec % history_end_h (id_id)
 grid_config_rec % history_end_m              = model_config_rec % history_end_m (id_id)
 grid_config_rec % history_end_s              = model_config_rec % history_end_s (id_id)
 grid_config_rec % history_end                = model_config_rec % history_end (id_id)
 grid_config_rec % inputout_end_y             = model_config_rec % inputout_end_y (id_id)
 grid_config_rec % inputout_end_d             = model_config_rec % inputout_end_d (id_id)
 grid_config_rec % inputout_end_h             = model_config_rec % inputout_end_h (id_id)
 grid_config_rec % inputout_end_m             = model_config_rec % inputout_end_m (id_id)
 grid_config_rec % inputout_end_s             = model_config_rec % inputout_end_s (id_id)
 grid_config_rec % simulation_start_year      = model_config_rec % simulation_start_year 
 grid_config_rec % simulation_start_month     = model_config_rec % simulation_start_month 
 grid_config_rec % simulation_start_day       = model_config_rec % simulation_start_day 
 grid_config_rec % simulation_start_hour      = model_config_rec % simulation_start_hour 
 grid_config_rec % simulation_start_minute    = model_config_rec % simulation_start_minute 
 grid_config_rec % simulation_start_second    = model_config_rec % simulation_start_second 
 grid_config_rec % reset_simulation_start     = model_config_rec % reset_simulation_start 
 grid_config_rec % sr_x                       = model_config_rec % sr_x (id_id)
 grid_config_rec % sr_y                       = model_config_rec % sr_y (id_id)
 grid_config_rec % iofields_filename          = model_config_rec % iofields_filename (id_id)
 grid_config_rec % ignore_iofields_warning    = model_config_rec % ignore_iofields_warning 
 grid_config_rec % ncd_nofill                 = model_config_rec % ncd_nofill 
 grid_config_rec % julyr                      = model_config_rec % julyr (id_id)
 grid_config_rec % julday                     = model_config_rec % julday (id_id)
 grid_config_rec % gmt                        = model_config_rec % gmt (id_id)
 grid_config_rec % high_freq_outname          = model_config_rec % high_freq_outname 
 grid_config_rec % partial_atcf_outname       = model_config_rec % partial_atcf_outname 
 grid_config_rec % input_inname               = model_config_rec % input_inname 
 grid_config_rec % input_outname              = model_config_rec % input_outname 
 grid_config_rec % bdy_inname                 = model_config_rec % bdy_inname 
 grid_config_rec % bdy_outname                = model_config_rec % bdy_outname 
 grid_config_rec % rst_inname                 = model_config_rec % rst_inname 
 grid_config_rec % rst_outname                = model_config_rec % rst_outname 
 grid_config_rec % anl_outname                = model_config_rec % anl_outname (id_id)
 grid_config_rec % write_input                = model_config_rec % write_input 
 grid_config_rec % write_restart_at_0h        = model_config_rec % write_restart_at_0h 
 grid_config_rec % write_hist_at_0h_rst       = model_config_rec % write_hist_at_0h_rst 
 grid_config_rec % adjust_output_times        = model_config_rec % adjust_output_times 
 grid_config_rec % adjust_input_times         = model_config_rec % adjust_input_times 
 grid_config_rec % tstart                     = model_config_rec % tstart (id_id)
 grid_config_rec % nocolons                   = model_config_rec % nocolons 
 grid_config_rec % cycling                    = model_config_rec % cycling 
 grid_config_rec % output_ready_flag          = model_config_rec % output_ready_flag 
 grid_config_rec % dfi_opt                    = model_config_rec % dfi_opt 
 grid_config_rec % dfi_savehydmeteors         = model_config_rec % dfi_savehydmeteors 
 grid_config_rec % dfi_nfilter                = model_config_rec % dfi_nfilter 
 grid_config_rec % dfi_write_filtered_input   = model_config_rec % dfi_write_filtered_input 
 grid_config_rec % dfi_write_dfi_history      = model_config_rec % dfi_write_dfi_history 
 grid_config_rec % dfi_cutoff_seconds         = model_config_rec % dfi_cutoff_seconds 
 grid_config_rec % dfi_time_dim               = model_config_rec % dfi_time_dim 
 grid_config_rec % dfi_fwdstop_year           = model_config_rec % dfi_fwdstop_year 
 grid_config_rec % dfi_fwdstop_month          = model_config_rec % dfi_fwdstop_month 
 grid_config_rec % dfi_fwdstop_day            = model_config_rec % dfi_fwdstop_day 
 grid_config_rec % dfi_fwdstop_hour           = model_config_rec % dfi_fwdstop_hour 
 grid_config_rec % dfi_fwdstop_minute         = model_config_rec % dfi_fwdstop_minute 
 grid_config_rec % dfi_fwdstop_second         = model_config_rec % dfi_fwdstop_second 
 grid_config_rec % dfi_bckstop_year           = model_config_rec % dfi_bckstop_year 
 grid_config_rec % dfi_bckstop_month          = model_config_rec % dfi_bckstop_month 
 grid_config_rec % dfi_bckstop_day            = model_config_rec % dfi_bckstop_day 
 grid_config_rec % dfi_bckstop_hour           = model_config_rec % dfi_bckstop_hour 
 grid_config_rec % dfi_bckstop_minute         = model_config_rec % dfi_bckstop_minute 
 grid_config_rec % dfi_bckstop_second         = model_config_rec % dfi_bckstop_second 
 grid_config_rec % time_step                  = model_config_rec % time_step 
 grid_config_rec % time_step_fract_num        = model_config_rec % time_step_fract_num 
 grid_config_rec % time_step_fract_den        = model_config_rec % time_step_fract_den 
 grid_config_rec % time_step_dfi              = model_config_rec % time_step_dfi 
 grid_config_rec % max_dom                    = model_config_rec % max_dom 
 grid_config_rec % s_we                       = model_config_rec % s_we (id_id)
 grid_config_rec % e_we                       = model_config_rec % e_we (id_id)
 grid_config_rec % s_sn                       = model_config_rec % s_sn (id_id)
 grid_config_rec % e_sn                       = model_config_rec % e_sn (id_id)
 grid_config_rec % s_vert                     = model_config_rec % s_vert (id_id)
 grid_config_rec % e_vert                     = model_config_rec % e_vert (id_id)
 grid_config_rec % num_metgrid_soil_levels    = model_config_rec % num_metgrid_soil_levels 
 grid_config_rec % dx                         = model_config_rec % dx (id_id)
 grid_config_rec % dy                         = model_config_rec % dy (id_id)
 grid_config_rec % grid_id                    = model_config_rec % grid_id (id_id)
 grid_config_rec % grid_allowed               = model_config_rec % grid_allowed (id_id)
 grid_config_rec % parent_id                  = model_config_rec % parent_id (id_id)
 grid_config_rec % i_parent_start             = model_config_rec % i_parent_start (id_id)
 grid_config_rec % j_parent_start             = model_config_rec % j_parent_start (id_id)
 grid_config_rec % parent_grid_ratio          = model_config_rec % parent_grid_ratio (id_id)
 grid_config_rec % parent_time_step_ratio     = model_config_rec % parent_time_step_ratio (id_id)
 grid_config_rec % feedback                   = model_config_rec % feedback 
 grid_config_rec % smooth_option              = model_config_rec % smooth_option 
 grid_config_rec % ztop                       = model_config_rec % ztop (id_id)
 grid_config_rec % moad_grid_ratio            = model_config_rec % moad_grid_ratio (id_id)
 grid_config_rec % moad_time_step_ratio       = model_config_rec % moad_time_step_ratio (id_id)
 grid_config_rec % shw                        = model_config_rec % shw (id_id)
 grid_config_rec % tile_sz_x                  = model_config_rec % tile_sz_x 
 grid_config_rec % tile_sz_y                  = model_config_rec % tile_sz_y 
 grid_config_rec % numtiles                   = model_config_rec % numtiles 
 grid_config_rec % numtiles_inc               = model_config_rec % numtiles_inc 
 grid_config_rec % numtiles_x                 = model_config_rec % numtiles_x 
 grid_config_rec % numtiles_y                 = model_config_rec % numtiles_y 
 grid_config_rec % tile_strategy              = model_config_rec % tile_strategy 
 grid_config_rec % nproc_x                    = model_config_rec % nproc_x 
 grid_config_rec % nproc_y                    = model_config_rec % nproc_y 
 grid_config_rec % irand                      = model_config_rec % irand 
 grid_config_rec % dt                         = model_config_rec % dt (id_id)
 grid_config_rec % ts_buf_size                = model_config_rec % ts_buf_size 
 grid_config_rec % max_ts_locs                = model_config_rec % max_ts_locs 
 grid_config_rec % num_moves                  = model_config_rec % num_moves 
 grid_config_rec % vortex_interval            = model_config_rec % vortex_interval (id_id)
 grid_config_rec % corral_dist                = model_config_rec % corral_dist (id_id)
 grid_config_rec % move_id                    = model_config_rec % move_id (id_id)
 grid_config_rec % move_interval              = model_config_rec % move_interval (id_id)
 grid_config_rec % move_cd_x                  = model_config_rec % move_cd_x (id_id)
 grid_config_rec % move_cd_y                  = model_config_rec % move_cd_y (id_id)
 grid_config_rec % swap_x                     = model_config_rec % swap_x (id_id)
 grid_config_rec % swap_y                     = model_config_rec % swap_y (id_id)
 grid_config_rec % cycle_x                    = model_config_rec % cycle_x (id_id)
 grid_config_rec % cycle_y                    = model_config_rec % cycle_y (id_id)
 grid_config_rec % reorder_mesh               = model_config_rec % reorder_mesh 
 grid_config_rec % perturb_input              = model_config_rec % perturb_input 
 grid_config_rec % eta_levels                 = model_config_rec % eta_levels (id_id)
 grid_config_rec % ptsgm                      = model_config_rec % ptsgm 
 grid_config_rec % num_metgrid_levels         = model_config_rec % num_metgrid_levels 
 grid_config_rec % p_top_requested            = model_config_rec % p_top_requested 
 grid_config_rec % use_prep_hybrid            = model_config_rec % use_prep_hybrid 
 grid_config_rec % force_read_thompson        = model_config_rec % force_read_thompson 
 grid_config_rec % write_thompson_tables      = model_config_rec % write_thompson_tables 
 grid_config_rec % mp_physics                 = model_config_rec % mp_physics (id_id)
 grid_config_rec % mommix                     = model_config_rec % mommix (id_id)
 grid_config_rec % disheat                    = model_config_rec % disheat (id_id)
 grid_config_rec % do_radar_ref               = model_config_rec % do_radar_ref 
 grid_config_rec % compute_radar_ref          = model_config_rec % compute_radar_ref 
 grid_config_rec % ra_lw_physics              = model_config_rec % ra_lw_physics (id_id)
 grid_config_rec % ra_sw_physics              = model_config_rec % ra_sw_physics (id_id)
 grid_config_rec % radt                       = model_config_rec % radt (id_id)
 grid_config_rec % sf_sfclay_physics          = model_config_rec % sf_sfclay_physics (id_id)
 grid_config_rec % sf_surface_physics         = model_config_rec % sf_surface_physics (id_id)
 grid_config_rec % bl_pbl_physics             = model_config_rec % bl_pbl_physics (id_id)
 grid_config_rec % ysu_topdown_pblmix         = model_config_rec % ysu_topdown_pblmix (id_id)
 grid_config_rec % shinhong_tke_diag          = model_config_rec % shinhong_tke_diag (id_id)
 grid_config_rec % windfarm_opt               = model_config_rec % windfarm_opt (id_id)
 grid_config_rec % windfarm_ij                = model_config_rec % windfarm_ij 
 grid_config_rec % mfshconv                   = model_config_rec % mfshconv (id_id)
 grid_config_rec % bldt                       = model_config_rec % bldt (id_id)
 grid_config_rec % cu_physics                 = model_config_rec % cu_physics (id_id)
 grid_config_rec % shcu_physics               = model_config_rec % shcu_physics (id_id)
 grid_config_rec % cu_diag                    = model_config_rec % cu_diag (id_id)
 grid_config_rec % gfs_alpha                  = model_config_rec % gfs_alpha (id_id)
 grid_config_rec % cudt                       = model_config_rec % cudt (id_id)
 grid_config_rec % gsmdt                      = model_config_rec % gsmdt (id_id)
 grid_config_rec % isfflx                     = model_config_rec % isfflx 
 grid_config_rec % ideal_xland                = model_config_rec % ideal_xland 
 grid_config_rec % ifsnow                     = model_config_rec % ifsnow 
 grid_config_rec % icloud                     = model_config_rec % icloud 
 grid_config_rec % swrad_scat                 = model_config_rec % swrad_scat 
 grid_config_rec % surface_input_source       = model_config_rec % surface_input_source 
 grid_config_rec % num_soil_layers            = model_config_rec % num_soil_layers 
 grid_config_rec % num_urban_layers           = model_config_rec % num_urban_layers 
 grid_config_rec % sf_surface_mosaic          = model_config_rec % sf_surface_mosaic 
 grid_config_rec % mosaic_cat                 = model_config_rec % mosaic_cat 
 grid_config_rec % mosaic_cat_soil            = model_config_rec % mosaic_cat_soil 
 grid_config_rec % num_urban_hi               = model_config_rec % num_urban_hi 
 grid_config_rec % mosaic_lu                  = model_config_rec % mosaic_lu 
 grid_config_rec % mosaic_soil                = model_config_rec % mosaic_soil 
 grid_config_rec % maxiens                    = model_config_rec % maxiens 
 grid_config_rec % maxens                     = model_config_rec % maxens 
 grid_config_rec % maxens2                    = model_config_rec % maxens2 
 grid_config_rec % maxens3                    = model_config_rec % maxens3 
 grid_config_rec % ensdim                     = model_config_rec % ensdim 
 grid_config_rec % chem_opt                   = model_config_rec % chem_opt (id_id)
 grid_config_rec % num_land_cat               = model_config_rec % num_land_cat 
 grid_config_rec % num_soil_cat               = model_config_rec % num_soil_cat 
 grid_config_rec % topo_wind                  = model_config_rec % topo_wind (id_id)
 grid_config_rec % mp_zero_out                = model_config_rec % mp_zero_out 
 grid_config_rec % mp_zero_out_thresh         = model_config_rec % mp_zero_out_thresh 
 grid_config_rec % seaice_threshold           = model_config_rec % seaice_threshold 
 grid_config_rec % fractional_seaice          = model_config_rec % fractional_seaice 
 grid_config_rec % seaice_albedo_opt          = model_config_rec % seaice_albedo_opt 
 grid_config_rec % seaice_albedo_default      = model_config_rec % seaice_albedo_default 
 grid_config_rec % seaice_snowdepth_opt       = model_config_rec % seaice_snowdepth_opt 
 grid_config_rec % seaice_snowdepth_max       = model_config_rec % seaice_snowdepth_max 
 grid_config_rec % seaice_snowdepth_min       = model_config_rec % seaice_snowdepth_min 
 grid_config_rec % seaice_thickness_opt       = model_config_rec % seaice_thickness_opt 
 grid_config_rec % seaice_thickness_default   = model_config_rec % seaice_thickness_default 
 grid_config_rec % tice2tsk_if2cold           = model_config_rec % tice2tsk_if2cold 
 grid_config_rec % sst_update                 = model_config_rec % sst_update 
 grid_config_rec % sf_urban_physics           = model_config_rec % sf_urban_physics (id_id)
 grid_config_rec % usemonalb                  = model_config_rec % usemonalb 
 grid_config_rec % rdmaxalb                   = model_config_rec % rdmaxalb 
 grid_config_rec % rdlai2d                    = model_config_rec % rdlai2d 
 grid_config_rec % ua_phys                    = model_config_rec % ua_phys 
 grid_config_rec % gwd_opt                    = model_config_rec % gwd_opt (id_id)
 grid_config_rec % iz0tlnd                    = model_config_rec % iz0tlnd 
 grid_config_rec % sas_pgcon                  = model_config_rec % sas_pgcon (id_id)
 grid_config_rec % sas_shal_pgcon             = model_config_rec % sas_shal_pgcon (id_id)
 grid_config_rec % sas_shal_conv              = model_config_rec % sas_shal_conv (id_id)
 grid_config_rec % sas_mass_flux              = model_config_rec % sas_mass_flux (id_id)
 grid_config_rec % var_ric                    = model_config_rec % var_ric 
 grid_config_rec % coef_ric_l                 = model_config_rec % coef_ric_l 
 grid_config_rec % coef_ric_s                 = model_config_rec % coef_ric_s 
 grid_config_rec % random_seed                = model_config_rec % random_seed (id_id)
 grid_config_rec % icoef_sf                   = model_config_rec % icoef_sf (id_id)
 grid_config_rec % lcurr_sf                   = model_config_rec % lcurr_sf (id_id)
 grid_config_rec % ens_random_seed            = model_config_rec % ens_random_seed (id_id)
 grid_config_rec % pert_sas                   = model_config_rec % pert_sas 
 grid_config_rec % pert_pbl                   = model_config_rec % pert_pbl 
 grid_config_rec % ens_sasamp                 = model_config_rec % ens_sasamp (id_id)
 grid_config_rec % ens_pblamp                 = model_config_rec % ens_pblamp (id_id)
 grid_config_rec % idtad                      = model_config_rec % idtad (id_id)
 grid_config_rec % nsoil                      = model_config_rec % nsoil (id_id)
 grid_config_rec % nphs                       = model_config_rec % nphs (id_id)
 grid_config_rec % ncnvc                      = model_config_rec % ncnvc (id_id)
 grid_config_rec % nrand                      = model_config_rec % nrand (id_id)
 grid_config_rec % nrads                      = model_config_rec % nrads (id_id)
 grid_config_rec % nradl                      = model_config_rec % nradl (id_id)
 grid_config_rec % tprec                      = model_config_rec % tprec (id_id)
 grid_config_rec % theat                      = model_config_rec % theat (id_id)
 grid_config_rec % tclod                      = model_config_rec % tclod (id_id)
 grid_config_rec % trdsw                      = model_config_rec % trdsw (id_id)
 grid_config_rec % trdlw                      = model_config_rec % trdlw (id_id)
 grid_config_rec % tsrfc                      = model_config_rec % tsrfc (id_id)
 grid_config_rec % pcpflg                     = model_config_rec % pcpflg (id_id)
 grid_config_rec % sigma                      = model_config_rec % sigma (id_id)
 grid_config_rec % sfenth                     = model_config_rec % sfenth (id_id)
 grid_config_rec % co2tf                      = model_config_rec % co2tf 
 grid_config_rec % ra_call_offset             = model_config_rec % ra_call_offset 
 grid_config_rec % cam_abs_freq_s             = model_config_rec % cam_abs_freq_s 
 grid_config_rec % levsiz                     = model_config_rec % levsiz 
 grid_config_rec % paerlev                    = model_config_rec % paerlev 
 grid_config_rec % cam_abs_dim1               = model_config_rec % cam_abs_dim1 
 grid_config_rec % cam_abs_dim2               = model_config_rec % cam_abs_dim2 
 grid_config_rec % no_src_types               = model_config_rec % no_src_types 
 grid_config_rec % alevsiz                    = model_config_rec % alevsiz 
 grid_config_rec % o3input                    = model_config_rec % o3input 
 grid_config_rec % aer_opt                    = model_config_rec % aer_opt 
 grid_config_rec % cu_rad_feedback            = model_config_rec % cu_rad_feedback (id_id)
 grid_config_rec % icloud_cu                  = model_config_rec % icloud_cu 
 grid_config_rec % h_diff                     = model_config_rec % h_diff (id_id)
 grid_config_rec % movemin                    = model_config_rec % movemin (id_id)
 grid_config_rec % num_snso_layers            = model_config_rec % num_snso_layers 
 grid_config_rec % num_snow_layers            = model_config_rec % num_snow_layers 
 grid_config_rec % use_aero_icbc              = model_config_rec % use_aero_icbc 
 grid_config_rec % ccn_conc                   = model_config_rec % ccn_conc 
 grid_config_rec % hail_opt                   = model_config_rec % hail_opt 
 grid_config_rec % sf_lake_physics            = model_config_rec % sf_lake_physics (id_id)
 grid_config_rec % dyn_opt                    = model_config_rec % dyn_opt 
 grid_config_rec % rk_ord                     = model_config_rec % rk_ord 
 grid_config_rec % w_damping                  = model_config_rec % w_damping 
 grid_config_rec % diff_opt                   = model_config_rec % diff_opt (id_id)
 grid_config_rec % km_opt                     = model_config_rec % km_opt (id_id)
 grid_config_rec % damp_opt                   = model_config_rec % damp_opt 
 grid_config_rec % zdamp                      = model_config_rec % zdamp (id_id)
 grid_config_rec % base_pres                  = model_config_rec % base_pres 
 grid_config_rec % base_temp                  = model_config_rec % base_temp 
 grid_config_rec % base_lapse                 = model_config_rec % base_lapse 
 grid_config_rec % iso_temp                   = model_config_rec % iso_temp 
 grid_config_rec % dampcoef                   = model_config_rec % dampcoef (id_id)
 grid_config_rec % khdif                      = model_config_rec % khdif (id_id)
 grid_config_rec % kvdif                      = model_config_rec % kvdif (id_id)
 grid_config_rec % c_s                        = model_config_rec % c_s (id_id)
 grid_config_rec % c_k                        = model_config_rec % c_k (id_id)
 grid_config_rec % smdiv                      = model_config_rec % smdiv (id_id)
 grid_config_rec % emdiv                      = model_config_rec % emdiv (id_id)
 grid_config_rec % epssm                      = model_config_rec % epssm (id_id)
 grid_config_rec % non_hydrostatic            = model_config_rec % non_hydrostatic (id_id)
 grid_config_rec % time_step_sound            = model_config_rec % time_step_sound (id_id)
 grid_config_rec % h_mom_adv_order            = model_config_rec % h_mom_adv_order (id_id)
 grid_config_rec % v_mom_adv_order            = model_config_rec % v_mom_adv_order (id_id)
 grid_config_rec % h_sca_adv_order            = model_config_rec % h_sca_adv_order (id_id)
 grid_config_rec % v_sca_adv_order            = model_config_rec % v_sca_adv_order (id_id)
 grid_config_rec % top_radiation              = model_config_rec % top_radiation (id_id)
 grid_config_rec % tke_upper_bound            = model_config_rec % tke_upper_bound (id_id)
 grid_config_rec % tke_drag_coefficient       = model_config_rec % tke_drag_coefficient (id_id)
 grid_config_rec % tke_heat_flux              = model_config_rec % tke_heat_flux (id_id)
 grid_config_rec % pert_coriolis              = model_config_rec % pert_coriolis (id_id)
 grid_config_rec % euler_adv                  = model_config_rec % euler_adv 
 grid_config_rec % idtadt                     = model_config_rec % idtadt 
 grid_config_rec % idtadc                     = model_config_rec % idtadc 
 grid_config_rec % codamp                     = model_config_rec % codamp (id_id)
 grid_config_rec % coac                       = model_config_rec % coac (id_id)
 grid_config_rec % slophc                     = model_config_rec % slophc (id_id)
 grid_config_rec % wp                         = model_config_rec % wp (id_id)
 grid_config_rec % terrain_smoothing          = model_config_rec % terrain_smoothing 
 grid_config_rec % spec_bdy_width             = model_config_rec % spec_bdy_width 
 grid_config_rec % spec_zone                  = model_config_rec % spec_zone 
 grid_config_rec % relax_zone                 = model_config_rec % relax_zone 
 grid_config_rec % specified                  = model_config_rec % specified (id_id)
 grid_config_rec % periodic_x                 = model_config_rec % periodic_x (id_id)
 grid_config_rec % symmetric_xs               = model_config_rec % symmetric_xs (id_id)
 grid_config_rec % symmetric_xe               = model_config_rec % symmetric_xe (id_id)
 grid_config_rec % open_xs                    = model_config_rec % open_xs (id_id)
 grid_config_rec % open_xe                    = model_config_rec % open_xe (id_id)
 grid_config_rec % periodic_y                 = model_config_rec % periodic_y (id_id)
 grid_config_rec % symmetric_ys               = model_config_rec % symmetric_ys (id_id)
 grid_config_rec % symmetric_ye               = model_config_rec % symmetric_ye (id_id)
 grid_config_rec % open_ys                    = model_config_rec % open_ys (id_id)
 grid_config_rec % open_ye                    = model_config_rec % open_ye (id_id)
 grid_config_rec % polar                      = model_config_rec % polar (id_id)
 grid_config_rec % nested                     = model_config_rec % nested (id_id)
 grid_config_rec % real_data_init_type        = model_config_rec % real_data_init_type 
 grid_config_rec % background_proc_id         = model_config_rec % background_proc_id 
 grid_config_rec % forecast_proc_id           = model_config_rec % forecast_proc_id 
 grid_config_rec % production_status          = model_config_rec % production_status 
 grid_config_rec % compression                = model_config_rec % compression 
 grid_config_rec % cen_lat                    = model_config_rec % cen_lat (id_id)
 grid_config_rec % cen_lon                    = model_config_rec % cen_lon (id_id)
 grid_config_rec % truelat1                   = model_config_rec % truelat1 (id_id)
 grid_config_rec % truelat2                   = model_config_rec % truelat2 (id_id)
 grid_config_rec % moad_cen_lat               = model_config_rec % moad_cen_lat (id_id)
 grid_config_rec % stand_lon                  = model_config_rec % stand_lon (id_id)
 grid_config_rec % flag_metgrid               = model_config_rec % flag_metgrid 
 grid_config_rec % flag_snow                  = model_config_rec % flag_snow 
 grid_config_rec % flag_psfc                  = model_config_rec % flag_psfc 
 grid_config_rec % flag_sm000010              = model_config_rec % flag_sm000010 
 grid_config_rec % flag_sm010040              = model_config_rec % flag_sm010040 
 grid_config_rec % flag_sm040100              = model_config_rec % flag_sm040100 
 grid_config_rec % flag_sm100200              = model_config_rec % flag_sm100200 
 grid_config_rec % flag_st000010              = model_config_rec % flag_st000010 
 grid_config_rec % flag_st010040              = model_config_rec % flag_st010040 
 grid_config_rec % flag_st040100              = model_config_rec % flag_st040100 
 grid_config_rec % flag_st100200              = model_config_rec % flag_st100200 
 grid_config_rec % flag_slp                   = model_config_rec % flag_slp 
 grid_config_rec % flag_soilhgt               = model_config_rec % flag_soilhgt 
 grid_config_rec % flag_mf_xy                 = model_config_rec % flag_mf_xy 
 grid_config_rec % bdyfrq                     = model_config_rec % bdyfrq (id_id)
 grid_config_rec % mminlu                     = model_config_rec % mminlu (id_id)
 grid_config_rec % iswater                    = model_config_rec % iswater (id_id)
 grid_config_rec % islake                     = model_config_rec % islake (id_id)
 grid_config_rec % isice                      = model_config_rec % isice (id_id)
 grid_config_rec % isurban                    = model_config_rec % isurban (id_id)
 grid_config_rec % isoilwater                 = model_config_rec % isoilwater (id_id)
 grid_config_rec % map_proj                   = model_config_rec % map_proj (id_id)
 grid_config_rec % dfi_stage                  = model_config_rec % dfi_stage 
 grid_config_rec % mp_physics_dfi             = model_config_rec % mp_physics_dfi (id_id)
 grid_config_rec % maxpatch                   = model_config_rec % maxpatch 

   END SUBROUTINE model_to_grid_config_rec


   FUNCTION in_use_for_config ( id, vname ) RESULT ( in_use )
     INTEGER, INTENT(IN) :: id
     CHARACTER*(*), INTENT(IN) :: vname
     LOGICAL in_use
     INTEGER uses

     uses = 0
     in_use = .TRUE.

     IF      ( vname(1:1) .GE. 'x' ) THEN






IF(TRIM(vname).EQ.'z_lake3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'z3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'zi3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'zlow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%tg_option.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%tg_option.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'zwtxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'zsnsoxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'xsaixy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF

     ELSE IF ( vname(1:1) .GE. 't' ) THEN






IF(TRIM(vname).EQ.'t_grnd2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_lake3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t_soisno3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'watsat3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tkmg3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tkdry3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tksatu3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'windsq_swath')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%swath_mode.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%swath_mode.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'weightout')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tracker_fixes')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tracker_distsq')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tracker_fixes')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tracker_distsq')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tg_max_m10wind')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%tg_option.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%tg_option.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tg_max_wwind')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%tg_option.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%tg_option.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tg_min_wwind')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%tg_option.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%tg_option.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tg_max_zhel_25')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%tg_option.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%tg_option.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tg_min_zhel_25')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%tg_option.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%tg_option.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tg_max_zhel_03')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%tg_option.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%tg_option.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tg_min_zhel_03')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%tg_option.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%tg_option.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tg_interval_start')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%tg_option.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%tg_option.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tg_interval_end')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%tg_option.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%tg_option.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tg_duration')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%tg_option.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%tg_option.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tg_total_precip')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%tg_option.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%tg_option.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tlow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%tg_option.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%tg_option.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tg_updhel03')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%tg_option.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%tg_option.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tg_updhel25')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%tg_option.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%tg_option.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tg_max_updhel03')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%tg_option.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%tg_option.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tg_max_updhel25')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%tg_option.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%tg_option.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tvxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tahxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wslakexy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'waxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wtxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tsnoxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'woodxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'taussxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2mvxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'t2mbxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tradxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'wgapxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgvxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'tgbxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'trxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF

     ELSE IF ( vname(1:1) .GE. 'o' ) THEN






IF(TRIM(vname).EQ.'savedtke12d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'snowdp2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'snl2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'precip_swath')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%swath_mode.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%swath_mode.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pdyn_parent')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pdyn_smooth')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pdyn_parent')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pdyn_smooth')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pdyn_parent')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pdyn_smooth')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pdyn_parent')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pdyn_smooth')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pdyn_parent')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pdyn_smooth')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p850rv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p700rv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p850wind')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p700wind')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p850z')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p700z')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'sp850rv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'sp700rv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'sp850wind')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'sp700wind')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'sp850z')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'sp700z')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'sm10wind')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'sm10rv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'smslp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pdyn_parent')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'pdyn_smooth')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p850rv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p700rv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p850wind')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p700wind')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p850z')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p700z')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'sp850rv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'sp700rv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'sp850z')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'sp700z')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'sm10rv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'smslp')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p500u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p500v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p700u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p700v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p850u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'p850v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.28
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.28
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.28
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.28
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.28
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.28
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.16
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.16
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.16
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.16
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.16
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.16
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.17
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.17
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.17
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.17
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.17
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.17
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.18
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.18
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.18
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.18
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.18
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.18
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.22
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.22
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.22
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.22
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.22
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.22
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'refl_10cm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%compute_radar_ref.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%compute_radar_ref.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'refd_max')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%compute_radar_ref.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%compute_radar_ref.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'o3rad')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'o3rad')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'o3rad')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'swdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'o3rad')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'sneqvoxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'qsnowxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'snicexy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'snliqxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rtmassxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'stmassxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'stblcpxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'q2mvxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'q2mbxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'qinxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'runsfxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'runsbxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'psnxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'savxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'sagxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rssunxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rsshaxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'shgxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'shcxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'shbxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'smoiseq')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'smcwtdxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rechxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'rc2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'randstate1')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'randstate2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'randstate3')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'randstate4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'random')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'snowsi')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%seaice_snowdepth_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%seaice_snowdepth_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'soldrain')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%wrf_hydro.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%wrf_hydro.EQ.1
  ENDIF
ENDIF

     ELSE IF ( vname(1:1) .GE. 'l' ) THEN






IF(TRIM(vname).EQ.'lake2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lakedepth2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lake_icefrac3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'mslp_noisy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'m10wind')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'m10rv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'m10wind')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'m10rv')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lwdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lfmassxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'neexy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'nppxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'lake_depth')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF

     ELSE IF ( vname(1:1) .GE. 'g' ) THEN






IF(TRIM(vname).EQ.'h2osno2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'h2osoi_ice3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'h2osoi_liq3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'h2osoi_vol3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'interesting')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%swath_mode.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%swath_mode.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'isnowxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'gppxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ghvxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ghbxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'irgxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ircxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'irbxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'hpbl2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'heat2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'hpbl2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.83
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.83
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'heat2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.83
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.83
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'hpbl2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.84
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.84
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'heat2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.84
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.84
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'hpbl2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.85
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.85
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'heat2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.85
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.85
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'gd_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_diag(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_diag(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'gd_cloud2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_diag(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_diag(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'gd_cldfr')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_diag(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_diag(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'gd_cloud_a')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_diag(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_diag(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'gd_cloud2_a')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_diag(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_diag(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'kbcon_deep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_diag(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_diag(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ktop_deep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_diag(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_diag(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'k22_deep')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_diag(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_diag(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'icedepth')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%seaice_thickness_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%seaice_thickness_opt.EQ.1
  ENDIF
ENDIF

     ELSE IF ( vname(1:1) .GE. 'd' ) THEN






IF(TRIM(vname).EQ.'dz_lake3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dz3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'distsq')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'distsq')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'distsq')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'distsq')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%vortex_tracker(id).EQ.7
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%vortex_tracker(id).EQ.7
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'f_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'f_rain')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'f_rimef')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.5
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.5
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'f_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.85
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.85
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'f_rain')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.85
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.85
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'f_rimef')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.85
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.85
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'f_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.95
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.95
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'f_rain')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.95
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.95
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'f_rimef')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics(id).EQ.95
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics(id).EQ.95
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.6
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.6
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.8
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.8
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.28
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.28
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.28
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.28
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.28
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.28
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.14
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.14
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_cloud')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.16
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.16
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_ice')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.16
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.16
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_re_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%mp_physics_dfi(id).EQ.16
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%mp_physics_dfi(id).EQ.16
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'flx4')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fvb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fbur')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fgsn')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'eahxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fwetxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fastcpxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fvegxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'ecanxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'edirxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'etranxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'fsaxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'firaxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'evgxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'evbxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'evcxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'deeprechxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'evap2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'evap2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%bl_pbl_physics(id).EQ.83
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%bl_pbl_physics(id).EQ.83
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'evap2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.84
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.84
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'evap2d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%cu_physics(id).EQ.85
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%cu_physics(id).EQ.85
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pd')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pint')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_dwdt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_t')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_q')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_q2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_cwm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_rrw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_stc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_sh2o')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_canwat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_nmm_tsk')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pd')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pint')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_dwdt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_t')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_q')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_q2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_cwm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_rrw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_stc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_sh2o')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_canwat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_nmm_tsk')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pd')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pint')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_dwdt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_t')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_q')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_q2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_cwm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_rrw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_stc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_sh2o')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_canwat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_nmm_tsk')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF

     ELSE 






IF(TRIM(vname).EQ.'csol3d')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_lake_physics(id).EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_lake_physics(id).EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_lw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_lw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aclwdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswuptc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdntc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswupbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnb')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'acswdnbc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%ra_sw_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%ra_sw_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'canliqxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'canicexy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'cmxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'alboldxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'aparxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'bgapxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chvxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chbxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chleafxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chucxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chv2xy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chb2xy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'chstarxy')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%sf_surface_physics(id).EQ.4
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%sf_surface_physics(id).EQ.4
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'albsi')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%seaice_albedo_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%seaice_albedo_opt.EQ.2
  ENDIF
ENDIF

     ENDIF

     RETURN
   END FUNCTION





   SUBROUTINE init_module_configure
     USE module_scalar_tables
     IMPLICIT NONE
     CALL init_module_scalar_tables
   END SUBROUTINE init_module_configure

   SUBROUTINE wrf_alt_nml_obsolete (nml_read_unit, nml_name)









     IMPLICIT NONE
     INTEGER, INTENT(IN)       :: nml_read_unit
     CHARACTER*(*), INTENT(IN) :: nml_name
     INTEGER                   :: nml_error







integer    :: first_item_in_struct
real , DIMENSION(max_domains) :: lakedepth_default
real , DIMENSION(max_domains) :: lake_min_elev
integer , DIMENSION(max_domains) :: use_lakedepth
integer :: halo_debug
integer :: ntracers
integer , DIMENSION(max_domains) :: vortex_tracker
real , DIMENSION(max_domains) :: interest_rad_storm
real , DIMENSION(max_domains) :: interest_rad_parent
real , DIMENSION(max_domains) :: interest_rad_self
integer , DIMENSION(max_domains) :: interest_kids
integer , DIMENSION(max_domains) :: interest_self
integer , DIMENSION(max_domains) :: interest_storms
integer :: swath_mode
integer :: num_old_fixes
real , DIMENSION(max_domains) :: vt4_radius
real , DIMENSION(max_domains) :: vt4_weightexp
real , DIMENSION(max_domains) :: vt4_pmax
real , DIMENSION(max_domains) :: vt4_noise_pmax
real , DIMENSION(max_domains) :: vt4_noise_pmin
real , DIMENSION(max_domains) :: vt4_noise_dpdr
integer , DIMENSION(max_domains) :: vt4_noise_iter
real , DIMENSION(max_domains) :: nomove_freq
integer , DIMENSION(max_domains) :: coral_x
integer , DIMENSION(max_domains) :: coral_y
integer :: tg_reset_stream
integer :: tg_option
integer , DIMENSION(max_domains) :: ntornado
real , DIMENSION(max_domains) :: wbd0
real , DIMENSION(max_domains) :: sbd0
logical , DIMENSION(max_domains) :: analysis
logical , DIMENSION(max_domains) :: write_analysis
integer :: io_form_auxinput2
logical :: high_freq
integer :: high_dom
integer :: swint_opt
integer , DIMENSION(max_domains) :: aer_type
integer , DIMENSION(max_domains) :: aer_aod550_opt
integer , DIMENSION(max_domains) :: aer_angexp_opt
integer , DIMENSION(max_domains) :: aer_ssa_opt
integer , DIMENSION(max_domains) :: aer_asy_opt
real , DIMENSION(max_domains) :: aer_aod550_val
real , DIMENSION(max_domains) :: aer_angexp_val
real , DIMENSION(max_domains) :: aer_ssa_val
real , DIMENSION(max_domains) :: aer_asy_val
integer :: dveg
integer :: opt_crs
integer :: opt_btr
integer :: opt_run
integer :: opt_sfc
integer :: opt_frz
integer :: opt_inf
integer :: opt_rad
integer :: opt_alb
integer :: opt_snf
integer :: opt_tbot
integer :: opt_stc
integer :: wrf_hydro
integer :: run_days
integer :: run_hours
integer :: run_minutes
integer :: run_seconds
integer , DIMENSION(max_domains) :: start_year
integer , DIMENSION(max_domains) :: start_month
integer , DIMENSION(max_domains) :: start_day
integer , DIMENSION(max_domains) :: start_hour
integer , DIMENSION(max_domains) :: start_minute
integer , DIMENSION(max_domains) :: start_second
integer , DIMENSION(max_domains) :: end_year
integer , DIMENSION(max_domains) :: end_month
integer , DIMENSION(max_domains) :: end_day
integer , DIMENSION(max_domains) :: end_hour
integer , DIMENSION(max_domains) :: end_minute
integer , DIMENSION(max_domains) :: end_second
integer :: interval_seconds
logical , DIMENSION(max_domains) :: input_from_file
integer , DIMENSION(max_domains) :: fine_input_stream
character*256 :: auxinput1_inname
integer :: io_form_auxinput1
logical :: override_restart_timers
character*256 :: auxhist1_inname
character*256 :: auxhist1_outname
integer , DIMENSION(max_domains) :: auxhist1_interval_y
integer , DIMENSION(max_domains) :: auxhist1_interval_d
integer , DIMENSION(max_domains) :: auxhist1_interval_h
integer , DIMENSION(max_domains) :: auxhist1_interval_m
integer , DIMENSION(max_domains) :: auxhist1_interval_s
integer , DIMENSION(max_domains) :: auxhist1_interval
integer , DIMENSION(max_domains) :: auxhist1_begin_y
integer , DIMENSION(max_domains) :: auxhist1_begin_d
integer , DIMENSION(max_domains) :: auxhist1_begin_h
integer , DIMENSION(max_domains) :: auxhist1_begin_m
integer , DIMENSION(max_domains) :: auxhist1_begin_s
integer , DIMENSION(max_domains) :: auxhist1_begin
integer , DIMENSION(max_domains) :: auxhist1_end_y
integer , DIMENSION(max_domains) :: auxhist1_end_d
integer , DIMENSION(max_domains) :: auxhist1_end_h
integer , DIMENSION(max_domains) :: auxhist1_end_m
integer , DIMENSION(max_domains) :: auxhist1_end_s
integer , DIMENSION(max_domains) :: auxhist1_end
integer :: io_form_auxhist1
integer , DIMENSION(max_domains) :: frames_per_auxhist1
character*256 :: auxhist2_inname
character*256 :: auxhist2_outname
integer , DIMENSION(max_domains) :: auxhist2_interval_y
integer , DIMENSION(max_domains) :: auxhist2_interval_d
integer , DIMENSION(max_domains) :: auxhist2_interval_h
integer , DIMENSION(max_domains) :: auxhist2_interval_m
integer , DIMENSION(max_domains) :: auxhist2_interval_s
integer , DIMENSION(max_domains) :: auxhist2_interval
integer , DIMENSION(max_domains) :: auxhist2_begin_y
integer , DIMENSION(max_domains) :: auxhist2_begin_d
integer , DIMENSION(max_domains) :: auxhist2_begin_h
integer , DIMENSION(max_domains) :: auxhist2_begin_m
integer , DIMENSION(max_domains) :: auxhist2_begin_s
integer , DIMENSION(max_domains) :: auxhist2_begin
integer , DIMENSION(max_domains) :: auxhist2_end_y
integer , DIMENSION(max_domains) :: auxhist2_end_d
integer , DIMENSION(max_domains) :: auxhist2_end_h
integer , DIMENSION(max_domains) :: auxhist2_end_m
integer , DIMENSION(max_domains) :: auxhist2_end_s
integer , DIMENSION(max_domains) :: auxhist2_end
integer :: io_form_auxhist2
integer , DIMENSION(max_domains) :: frames_per_auxhist2
character*256 :: auxhist3_inname
character*256 :: auxhist3_outname
integer , DIMENSION(max_domains) :: auxhist3_interval_y
integer , DIMENSION(max_domains) :: auxhist3_interval_d
integer , DIMENSION(max_domains) :: auxhist3_interval_h
integer , DIMENSION(max_domains) :: auxhist3_interval_m
integer , DIMENSION(max_domains) :: auxhist3_interval_s
integer , DIMENSION(max_domains) :: auxhist3_interval
integer , DIMENSION(max_domains) :: auxhist3_begin_y
integer , DIMENSION(max_domains) :: auxhist3_begin_d
integer , DIMENSION(max_domains) :: auxhist3_begin_h
integer , DIMENSION(max_domains) :: auxhist3_begin_m
integer , DIMENSION(max_domains) :: auxhist3_begin_s
integer , DIMENSION(max_domains) :: auxhist3_begin
integer , DIMENSION(max_domains) :: auxhist3_end_y
integer , DIMENSION(max_domains) :: auxhist3_end_d
integer , DIMENSION(max_domains) :: auxhist3_end_h
integer , DIMENSION(max_domains) :: auxhist3_end_m
integer , DIMENSION(max_domains) :: auxhist3_end_s
integer , DIMENSION(max_domains) :: auxhist3_end
integer :: io_form_auxhist3
integer , DIMENSION(max_domains) :: frames_per_auxhist3
character*256 :: auxhist4_inname
character*256 :: auxhist4_outname
integer , DIMENSION(max_domains) :: auxhist4_interval_y
integer , DIMENSION(max_domains) :: auxhist4_interval_d
integer , DIMENSION(max_domains) :: auxhist4_interval_h
integer , DIMENSION(max_domains) :: auxhist4_interval_m
integer , DIMENSION(max_domains) :: auxhist4_interval_s
integer , DIMENSION(max_domains) :: auxhist4_interval
integer , DIMENSION(max_domains) :: auxhist4_begin_y
integer , DIMENSION(max_domains) :: auxhist4_begin_d
integer , DIMENSION(max_domains) :: auxhist4_begin_h
integer , DIMENSION(max_domains) :: auxhist4_begin_m
integer , DIMENSION(max_domains) :: auxhist4_begin_s
integer , DIMENSION(max_domains) :: auxhist4_begin
integer , DIMENSION(max_domains) :: auxhist4_end_y
integer , DIMENSION(max_domains) :: auxhist4_end_d
integer , DIMENSION(max_domains) :: auxhist4_end_h
integer , DIMENSION(max_domains) :: auxhist4_end_m
integer , DIMENSION(max_domains) :: auxhist4_end_s
integer , DIMENSION(max_domains) :: auxhist4_end
integer :: io_form_auxhist4
integer , DIMENSION(max_domains) :: frames_per_auxhist4
character*256 :: auxhist5_inname
character*256 :: auxhist5_outname
integer , DIMENSION(max_domains) :: auxhist5_interval_y
integer , DIMENSION(max_domains) :: auxhist5_interval_d
integer , DIMENSION(max_domains) :: auxhist5_interval_h
integer , DIMENSION(max_domains) :: auxhist5_interval_m
integer , DIMENSION(max_domains) :: auxhist5_interval_s
integer , DIMENSION(max_domains) :: auxhist5_interval
integer , DIMENSION(max_domains) :: auxhist5_begin_y
integer , DIMENSION(max_domains) :: auxhist5_begin_d
integer , DIMENSION(max_domains) :: auxhist5_begin_h
integer , DIMENSION(max_domains) :: auxhist5_begin_m
integer , DIMENSION(max_domains) :: auxhist5_begin_s
integer , DIMENSION(max_domains) :: auxhist5_begin
integer , DIMENSION(max_domains) :: auxhist5_end_y
integer , DIMENSION(max_domains) :: auxhist5_end_d
integer , DIMENSION(max_domains) :: auxhist5_end_h
integer , DIMENSION(max_domains) :: auxhist5_end_m
integer , DIMENSION(max_domains) :: auxhist5_end_s
integer , DIMENSION(max_domains) :: auxhist5_end
integer :: io_form_auxhist5
integer , DIMENSION(max_domains) :: frames_per_auxhist5
character*256 :: auxhist6_inname
character*256 :: auxhist6_outname
integer , DIMENSION(max_domains) :: auxhist6_interval_y
integer , DIMENSION(max_domains) :: auxhist6_interval_d
integer , DIMENSION(max_domains) :: auxhist6_interval_h
integer , DIMENSION(max_domains) :: auxhist6_interval_m
integer , DIMENSION(max_domains) :: auxhist6_interval_s
integer , DIMENSION(max_domains) :: auxhist6_interval
integer , DIMENSION(max_domains) :: auxhist6_begin_y
integer , DIMENSION(max_domains) :: auxhist6_begin_d
integer , DIMENSION(max_domains) :: auxhist6_begin_h
integer , DIMENSION(max_domains) :: auxhist6_begin_m
integer , DIMENSION(max_domains) :: auxhist6_begin_s
integer , DIMENSION(max_domains) :: auxhist6_begin
integer , DIMENSION(max_domains) :: auxhist6_end_y
integer , DIMENSION(max_domains) :: auxhist6_end_d
integer , DIMENSION(max_domains) :: auxhist6_end_h
integer , DIMENSION(max_domains) :: auxhist6_end_m
integer , DIMENSION(max_domains) :: auxhist6_end_s
integer , DIMENSION(max_domains) :: auxhist6_end
integer :: io_form_auxhist6
integer , DIMENSION(max_domains) :: frames_per_auxhist6
character*256 :: auxhist7_inname
character*256 :: auxhist7_outname
integer , DIMENSION(max_domains) :: auxhist7_interval_y
integer , DIMENSION(max_domains) :: auxhist7_interval_d
integer , DIMENSION(max_domains) :: auxhist7_interval_h
integer , DIMENSION(max_domains) :: auxhist7_interval_m
integer , DIMENSION(max_domains) :: auxhist7_interval_s
integer , DIMENSION(max_domains) :: auxhist7_interval
integer , DIMENSION(max_domains) :: auxhist7_begin_y
integer , DIMENSION(max_domains) :: auxhist7_begin_d
integer , DIMENSION(max_domains) :: auxhist7_begin_h
integer , DIMENSION(max_domains) :: auxhist7_begin_m
integer , DIMENSION(max_domains) :: auxhist7_begin_s
integer , DIMENSION(max_domains) :: auxhist7_begin
integer , DIMENSION(max_domains) :: auxhist7_end_y
integer , DIMENSION(max_domains) :: auxhist7_end_d
integer , DIMENSION(max_domains) :: auxhist7_end_h
integer , DIMENSION(max_domains) :: auxhist7_end_m
integer , DIMENSION(max_domains) :: auxhist7_end_s
integer , DIMENSION(max_domains) :: auxhist7_end
integer :: io_form_auxhist7
integer , DIMENSION(max_domains) :: frames_per_auxhist7
character*256 :: auxhist8_inname
character*256 :: auxhist8_outname
integer , DIMENSION(max_domains) :: auxhist8_interval_y
integer , DIMENSION(max_domains) :: auxhist8_interval_d
integer , DIMENSION(max_domains) :: auxhist8_interval_h
integer , DIMENSION(max_domains) :: auxhist8_interval_m
integer , DIMENSION(max_domains) :: auxhist8_interval_s
integer , DIMENSION(max_domains) :: auxhist8_interval
integer , DIMENSION(max_domains) :: auxhist8_begin_y
integer , DIMENSION(max_domains) :: auxhist8_begin_d
integer , DIMENSION(max_domains) :: auxhist8_begin_h
integer , DIMENSION(max_domains) :: auxhist8_begin_m
integer , DIMENSION(max_domains) :: auxhist8_begin_s
integer , DIMENSION(max_domains) :: auxhist8_begin
integer , DIMENSION(max_domains) :: auxhist8_end_y
integer , DIMENSION(max_domains) :: auxhist8_end_d
integer , DIMENSION(max_domains) :: auxhist8_end_h
integer , DIMENSION(max_domains) :: auxhist8_end_m
integer , DIMENSION(max_domains) :: auxhist8_end_s
integer , DIMENSION(max_domains) :: auxhist8_end
integer :: io_form_auxhist8
integer , DIMENSION(max_domains) :: frames_per_auxhist8
character*256 :: auxhist9_inname
character*256 :: auxhist9_outname
integer , DIMENSION(max_domains) :: auxhist9_interval_y
integer , DIMENSION(max_domains) :: auxhist9_interval_d
integer , DIMENSION(max_domains) :: auxhist9_interval_h
integer , DIMENSION(max_domains) :: auxhist9_interval_m
integer , DIMENSION(max_domains) :: auxhist9_interval_s
integer , DIMENSION(max_domains) :: auxhist9_interval
integer , DIMENSION(max_domains) :: auxhist9_begin_y
integer , DIMENSION(max_domains) :: auxhist9_begin_d
integer , DIMENSION(max_domains) :: auxhist9_begin_h
integer , DIMENSION(max_domains) :: auxhist9_begin_m
integer , DIMENSION(max_domains) :: auxhist9_begin_s
integer , DIMENSION(max_domains) :: auxhist9_begin
integer , DIMENSION(max_domains) :: auxhist9_end_y
integer , DIMENSION(max_domains) :: auxhist9_end_d
integer , DIMENSION(max_domains) :: auxhist9_end_h
integer , DIMENSION(max_domains) :: auxhist9_end_m
integer , DIMENSION(max_domains) :: auxhist9_end_s
integer , DIMENSION(max_domains) :: auxhist9_end
integer :: io_form_auxhist9
integer , DIMENSION(max_domains) :: frames_per_auxhist9
character*256 :: auxhist10_inname
character*256 :: auxhist10_outname
integer , DIMENSION(max_domains) :: auxhist10_interval_y
integer , DIMENSION(max_domains) :: auxhist10_interval_d
integer , DIMENSION(max_domains) :: auxhist10_interval_h
integer , DIMENSION(max_domains) :: auxhist10_interval_m
integer , DIMENSION(max_domains) :: auxhist10_interval_s
integer , DIMENSION(max_domains) :: auxhist10_interval
integer , DIMENSION(max_domains) :: auxhist10_begin_y
integer , DIMENSION(max_domains) :: auxhist10_begin_d
integer , DIMENSION(max_domains) :: auxhist10_begin_h
integer , DIMENSION(max_domains) :: auxhist10_begin_m
integer , DIMENSION(max_domains) :: auxhist10_begin_s
integer , DIMENSION(max_domains) :: auxhist10_begin
integer , DIMENSION(max_domains) :: auxhist10_end_y
integer , DIMENSION(max_domains) :: auxhist10_end_d
integer , DIMENSION(max_domains) :: auxhist10_end_h
integer , DIMENSION(max_domains) :: auxhist10_end_m
integer , DIMENSION(max_domains) :: auxhist10_end_s
integer , DIMENSION(max_domains) :: auxhist10_end
integer :: io_form_auxhist10
integer , DIMENSION(max_domains) :: frames_per_auxhist10
character*256 :: auxhist11_inname
character*256 :: auxhist11_outname
integer , DIMENSION(max_domains) :: auxhist11_interval_y
integer , DIMENSION(max_domains) :: auxhist11_interval_d
integer , DIMENSION(max_domains) :: auxhist11_interval_h
integer , DIMENSION(max_domains) :: auxhist11_interval_m
integer , DIMENSION(max_domains) :: auxhist11_interval_s
integer , DIMENSION(max_domains) :: auxhist11_interval
integer , DIMENSION(max_domains) :: auxhist11_begin_y
integer , DIMENSION(max_domains) :: auxhist11_begin_d
integer , DIMENSION(max_domains) :: auxhist11_begin_h
integer , DIMENSION(max_domains) :: auxhist11_begin_m
integer , DIMENSION(max_domains) :: auxhist11_begin_s
integer , DIMENSION(max_domains) :: auxhist11_begin
integer , DIMENSION(max_domains) :: auxhist11_end_y
integer , DIMENSION(max_domains) :: auxhist11_end_d
integer , DIMENSION(max_domains) :: auxhist11_end_h
integer , DIMENSION(max_domains) :: auxhist11_end_m
integer , DIMENSION(max_domains) :: auxhist11_end_s
integer , DIMENSION(max_domains) :: auxhist11_end
integer :: io_form_auxhist11
integer , DIMENSION(max_domains) :: frames_per_auxhist11
character*256 :: auxhist12_inname
character*256 :: auxhist12_outname
integer , DIMENSION(max_domains) :: auxhist12_interval_y
integer , DIMENSION(max_domains) :: auxhist12_interval_d
integer , DIMENSION(max_domains) :: auxhist12_interval_h
integer , DIMENSION(max_domains) :: auxhist12_interval_m
integer , DIMENSION(max_domains) :: auxhist12_interval_s
integer , DIMENSION(max_domains) :: auxhist12_interval
integer , DIMENSION(max_domains) :: auxhist12_begin_y
integer , DIMENSION(max_domains) :: auxhist12_begin_d
integer , DIMENSION(max_domains) :: auxhist12_begin_h
integer , DIMENSION(max_domains) :: auxhist12_begin_m
integer , DIMENSION(max_domains) :: auxhist12_begin_s
integer , DIMENSION(max_domains) :: auxhist12_begin
integer , DIMENSION(max_domains) :: auxhist12_end_y
integer , DIMENSION(max_domains) :: auxhist12_end_d
integer , DIMENSION(max_domains) :: auxhist12_end_h
integer , DIMENSION(max_domains) :: auxhist12_end_m
integer , DIMENSION(max_domains) :: auxhist12_end_s
integer , DIMENSION(max_domains) :: auxhist12_end
integer :: io_form_auxhist12
integer , DIMENSION(max_domains) :: frames_per_auxhist12
character*256 :: auxhist13_inname
character*256 :: auxhist13_outname
integer , DIMENSION(max_domains) :: auxhist13_interval_y
integer , DIMENSION(max_domains) :: auxhist13_interval_d
integer , DIMENSION(max_domains) :: auxhist13_interval_h
integer , DIMENSION(max_domains) :: auxhist13_interval_m
integer , DIMENSION(max_domains) :: auxhist13_interval_s
integer , DIMENSION(max_domains) :: auxhist13_interval
integer , DIMENSION(max_domains) :: auxhist13_begin_y
integer , DIMENSION(max_domains) :: auxhist13_begin_d
integer , DIMENSION(max_domains) :: auxhist13_begin_h
integer , DIMENSION(max_domains) :: auxhist13_begin_m
integer , DIMENSION(max_domains) :: auxhist13_begin_s
integer , DIMENSION(max_domains) :: auxhist13_begin
integer , DIMENSION(max_domains) :: auxhist13_end_y
integer , DIMENSION(max_domains) :: auxhist13_end_d
integer , DIMENSION(max_domains) :: auxhist13_end_h
integer , DIMENSION(max_domains) :: auxhist13_end_m
integer , DIMENSION(max_domains) :: auxhist13_end_s
integer , DIMENSION(max_domains) :: auxhist13_end
integer :: io_form_auxhist13
integer , DIMENSION(max_domains) :: frames_per_auxhist13
character*256 :: auxhist14_inname
character*256 :: auxhist14_outname
integer , DIMENSION(max_domains) :: auxhist14_interval_y
integer , DIMENSION(max_domains) :: auxhist14_interval_d
integer , DIMENSION(max_domains) :: auxhist14_interval_h
integer , DIMENSION(max_domains) :: auxhist14_interval_m
integer , DIMENSION(max_domains) :: auxhist14_interval_s
integer , DIMENSION(max_domains) :: auxhist14_interval
integer , DIMENSION(max_domains) :: auxhist14_begin_y
integer , DIMENSION(max_domains) :: auxhist14_begin_d
integer , DIMENSION(max_domains) :: auxhist14_begin_h
integer , DIMENSION(max_domains) :: auxhist14_begin_m
integer , DIMENSION(max_domains) :: auxhist14_begin_s
integer , DIMENSION(max_domains) :: auxhist14_begin
integer , DIMENSION(max_domains) :: auxhist14_end_y
integer , DIMENSION(max_domains) :: auxhist14_end_d
integer , DIMENSION(max_domains) :: auxhist14_end_h
integer , DIMENSION(max_domains) :: auxhist14_end_m
integer , DIMENSION(max_domains) :: auxhist14_end_s
integer , DIMENSION(max_domains) :: auxhist14_end
integer :: io_form_auxhist14
integer , DIMENSION(max_domains) :: frames_per_auxhist14
character*256 :: auxhist15_inname
character*256 :: auxhist15_outname
integer , DIMENSION(max_domains) :: auxhist15_interval_y
integer , DIMENSION(max_domains) :: auxhist15_interval_d
integer , DIMENSION(max_domains) :: auxhist15_interval_h
integer , DIMENSION(max_domains) :: auxhist15_interval_m
integer , DIMENSION(max_domains) :: auxhist15_interval_s
integer , DIMENSION(max_domains) :: auxhist15_interval
integer , DIMENSION(max_domains) :: auxhist15_begin_y
integer , DIMENSION(max_domains) :: auxhist15_begin_d
integer , DIMENSION(max_domains) :: auxhist15_begin_h
integer , DIMENSION(max_domains) :: auxhist15_begin_m
integer , DIMENSION(max_domains) :: auxhist15_begin_s
integer , DIMENSION(max_domains) :: auxhist15_begin
integer , DIMENSION(max_domains) :: auxhist15_end_y
integer , DIMENSION(max_domains) :: auxhist15_end_d
integer , DIMENSION(max_domains) :: auxhist15_end_h
integer , DIMENSION(max_domains) :: auxhist15_end_m
integer , DIMENSION(max_domains) :: auxhist15_end_s
integer , DIMENSION(max_domains) :: auxhist15_end
integer :: io_form_auxhist15
integer , DIMENSION(max_domains) :: frames_per_auxhist15
character*256 :: auxhist16_inname
character*256 :: auxhist16_outname
integer , DIMENSION(max_domains) :: auxhist16_interval_y
integer , DIMENSION(max_domains) :: auxhist16_interval_d
integer , DIMENSION(max_domains) :: auxhist16_interval_h
integer , DIMENSION(max_domains) :: auxhist16_interval_m
integer , DIMENSION(max_domains) :: auxhist16_interval_s
integer , DIMENSION(max_domains) :: auxhist16_interval
integer , DIMENSION(max_domains) :: auxhist16_begin_y
integer , DIMENSION(max_domains) :: auxhist16_begin_d
integer , DIMENSION(max_domains) :: auxhist16_begin_h
integer , DIMENSION(max_domains) :: auxhist16_begin_m
integer , DIMENSION(max_domains) :: auxhist16_begin_s
integer , DIMENSION(max_domains) :: auxhist16_begin
integer , DIMENSION(max_domains) :: auxhist16_end_y
integer , DIMENSION(max_domains) :: auxhist16_end_d
integer , DIMENSION(max_domains) :: auxhist16_end_h
integer , DIMENSION(max_domains) :: auxhist16_end_m
integer , DIMENSION(max_domains) :: auxhist16_end_s
integer , DIMENSION(max_domains) :: auxhist16_end
integer :: io_form_auxhist16
integer , DIMENSION(max_domains) :: frames_per_auxhist16
character*256 :: auxhist17_inname
character*256 :: auxhist17_outname
integer , DIMENSION(max_domains) :: auxhist17_interval_y
integer , DIMENSION(max_domains) :: auxhist17_interval_d
integer , DIMENSION(max_domains) :: auxhist17_interval_h
integer , DIMENSION(max_domains) :: auxhist17_interval_m
integer , DIMENSION(max_domains) :: auxhist17_interval_s
integer , DIMENSION(max_domains) :: auxhist17_interval
integer , DIMENSION(max_domains) :: auxhist17_begin_y
integer , DIMENSION(max_domains) :: auxhist17_begin_d
integer , DIMENSION(max_domains) :: auxhist17_begin_h
integer , DIMENSION(max_domains) :: auxhist17_begin_m
integer , DIMENSION(max_domains) :: auxhist17_begin_s
integer , DIMENSION(max_domains) :: auxhist17_begin
integer , DIMENSION(max_domains) :: auxhist17_end_y
integer , DIMENSION(max_domains) :: auxhist17_end_d
integer , DIMENSION(max_domains) :: auxhist17_end_h
integer , DIMENSION(max_domains) :: auxhist17_end_m
integer , DIMENSION(max_domains) :: auxhist17_end_s
integer , DIMENSION(max_domains) :: auxhist17_end
integer :: io_form_auxhist17
integer , DIMENSION(max_domains) :: frames_per_auxhist17
character*256 :: auxhist18_inname
character*256 :: auxhist18_outname
integer , DIMENSION(max_domains) :: auxhist18_interval_y
integer , DIMENSION(max_domains) :: auxhist18_interval_d
integer , DIMENSION(max_domains) :: auxhist18_interval_h
integer , DIMENSION(max_domains) :: auxhist18_interval_m
integer , DIMENSION(max_domains) :: auxhist18_interval_s
integer , DIMENSION(max_domains) :: auxhist18_interval
integer , DIMENSION(max_domains) :: auxhist18_begin_y
integer , DIMENSION(max_domains) :: auxhist18_begin_d
integer , DIMENSION(max_domains) :: auxhist18_begin_h
integer , DIMENSION(max_domains) :: auxhist18_begin_m
integer , DIMENSION(max_domains) :: auxhist18_begin_s
integer , DIMENSION(max_domains) :: auxhist18_begin
integer , DIMENSION(max_domains) :: auxhist18_end_y
integer , DIMENSION(max_domains) :: auxhist18_end_d
integer , DIMENSION(max_domains) :: auxhist18_end_h
integer , DIMENSION(max_domains) :: auxhist18_end_m
integer , DIMENSION(max_domains) :: auxhist18_end_s
integer , DIMENSION(max_domains) :: auxhist18_end
integer :: io_form_auxhist18
integer , DIMENSION(max_domains) :: frames_per_auxhist18
character*256 :: auxhist19_inname
character*256 :: auxhist19_outname
integer , DIMENSION(max_domains) :: auxhist19_interval_y
integer , DIMENSION(max_domains) :: auxhist19_interval_d
integer , DIMENSION(max_domains) :: auxhist19_interval_h
integer , DIMENSION(max_domains) :: auxhist19_interval_m
integer , DIMENSION(max_domains) :: auxhist19_interval_s
integer , DIMENSION(max_domains) :: auxhist19_interval
integer , DIMENSION(max_domains) :: auxhist19_begin_y
integer , DIMENSION(max_domains) :: auxhist19_begin_d
integer , DIMENSION(max_domains) :: auxhist19_begin_h
integer , DIMENSION(max_domains) :: auxhist19_begin_m
integer , DIMENSION(max_domains) :: auxhist19_begin_s
integer , DIMENSION(max_domains) :: auxhist19_begin
integer , DIMENSION(max_domains) :: auxhist19_end_y
integer , DIMENSION(max_domains) :: auxhist19_end_d
integer , DIMENSION(max_domains) :: auxhist19_end_h
integer , DIMENSION(max_domains) :: auxhist19_end_m
integer , DIMENSION(max_domains) :: auxhist19_end_s
integer , DIMENSION(max_domains) :: auxhist19_end
integer :: io_form_auxhist19
integer , DIMENSION(max_domains) :: frames_per_auxhist19
character*256 :: auxhist20_inname
character*256 :: auxhist20_outname
integer , DIMENSION(max_domains) :: auxhist20_interval_y
integer , DIMENSION(max_domains) :: auxhist20_interval_d
integer , DIMENSION(max_domains) :: auxhist20_interval_h
integer , DIMENSION(max_domains) :: auxhist20_interval_m
integer , DIMENSION(max_domains) :: auxhist20_interval_s
integer , DIMENSION(max_domains) :: auxhist20_interval
integer , DIMENSION(max_domains) :: auxhist20_begin_y
integer , DIMENSION(max_domains) :: auxhist20_begin_d
integer , DIMENSION(max_domains) :: auxhist20_begin_h
integer , DIMENSION(max_domains) :: auxhist20_begin_m
integer , DIMENSION(max_domains) :: auxhist20_begin_s
integer , DIMENSION(max_domains) :: auxhist20_begin
integer , DIMENSION(max_domains) :: auxhist20_end_y
integer , DIMENSION(max_domains) :: auxhist20_end_d
integer , DIMENSION(max_domains) :: auxhist20_end_h
integer , DIMENSION(max_domains) :: auxhist20_end_m
integer , DIMENSION(max_domains) :: auxhist20_end_s
integer , DIMENSION(max_domains) :: auxhist20_end
integer :: io_form_auxhist20
integer , DIMENSION(max_domains) :: frames_per_auxhist20
character*256 :: auxhist21_inname
character*256 :: auxhist21_outname
integer , DIMENSION(max_domains) :: auxhist21_interval_y
integer , DIMENSION(max_domains) :: auxhist21_interval_d
integer , DIMENSION(max_domains) :: auxhist21_interval_h
integer , DIMENSION(max_domains) :: auxhist21_interval_m
integer , DIMENSION(max_domains) :: auxhist21_interval_s
integer , DIMENSION(max_domains) :: auxhist21_interval
integer , DIMENSION(max_domains) :: auxhist21_begin_y
integer , DIMENSION(max_domains) :: auxhist21_begin_d
integer , DIMENSION(max_domains) :: auxhist21_begin_h
integer , DIMENSION(max_domains) :: auxhist21_begin_m
integer , DIMENSION(max_domains) :: auxhist21_begin_s
integer , DIMENSION(max_domains) :: auxhist21_begin
integer , DIMENSION(max_domains) :: auxhist21_end_y
integer , DIMENSION(max_domains) :: auxhist21_end_d
integer , DIMENSION(max_domains) :: auxhist21_end_h
integer , DIMENSION(max_domains) :: auxhist21_end_m
integer , DIMENSION(max_domains) :: auxhist21_end_s
integer , DIMENSION(max_domains) :: auxhist21_end
integer :: io_form_auxhist21
integer , DIMENSION(max_domains) :: frames_per_auxhist21
character*256 :: auxhist22_inname
character*256 :: auxhist22_outname
integer , DIMENSION(max_domains) :: auxhist22_interval_y
integer , DIMENSION(max_domains) :: auxhist22_interval_d
integer , DIMENSION(max_domains) :: auxhist22_interval_h
integer , DIMENSION(max_domains) :: auxhist22_interval_m
integer , DIMENSION(max_domains) :: auxhist22_interval_s
integer , DIMENSION(max_domains) :: auxhist22_interval
integer , DIMENSION(max_domains) :: auxhist22_begin_y
integer , DIMENSION(max_domains) :: auxhist22_begin_d
integer , DIMENSION(max_domains) :: auxhist22_begin_h
integer , DIMENSION(max_domains) :: auxhist22_begin_m
integer , DIMENSION(max_domains) :: auxhist22_begin_s
integer , DIMENSION(max_domains) :: auxhist22_begin
integer , DIMENSION(max_domains) :: auxhist22_end_y
integer , DIMENSION(max_domains) :: auxhist22_end_d
integer , DIMENSION(max_domains) :: auxhist22_end_h
integer , DIMENSION(max_domains) :: auxhist22_end_m
integer , DIMENSION(max_domains) :: auxhist22_end_s
integer , DIMENSION(max_domains) :: auxhist22_end
integer :: io_form_auxhist22
integer , DIMENSION(max_domains) :: frames_per_auxhist22
character*256 :: auxhist23_inname
character*256 :: auxhist23_outname
integer , DIMENSION(max_domains) :: auxhist23_interval_y
integer , DIMENSION(max_domains) :: auxhist23_interval_d
integer , DIMENSION(max_domains) :: auxhist23_interval_h
integer , DIMENSION(max_domains) :: auxhist23_interval_m
integer , DIMENSION(max_domains) :: auxhist23_interval_s
integer , DIMENSION(max_domains) :: auxhist23_interval
integer , DIMENSION(max_domains) :: auxhist23_begin_y
integer , DIMENSION(max_domains) :: auxhist23_begin_d
integer , DIMENSION(max_domains) :: auxhist23_begin_h
integer , DIMENSION(max_domains) :: auxhist23_begin_m
integer , DIMENSION(max_domains) :: auxhist23_begin_s
integer , DIMENSION(max_domains) :: auxhist23_begin
integer , DIMENSION(max_domains) :: auxhist23_end_y
integer , DIMENSION(max_domains) :: auxhist23_end_d
integer , DIMENSION(max_domains) :: auxhist23_end_h
integer , DIMENSION(max_domains) :: auxhist23_end_m
integer , DIMENSION(max_domains) :: auxhist23_end_s
integer , DIMENSION(max_domains) :: auxhist23_end
integer :: io_form_auxhist23
integer , DIMENSION(max_domains) :: frames_per_auxhist23
character*256 :: auxhist24_inname
character*256 :: auxhist24_outname
integer , DIMENSION(max_domains) :: auxhist24_interval_y
integer , DIMENSION(max_domains) :: auxhist24_interval_d
integer , DIMENSION(max_domains) :: auxhist24_interval_h
integer , DIMENSION(max_domains) :: auxhist24_interval_m
integer , DIMENSION(max_domains) :: auxhist24_interval_s
integer , DIMENSION(max_domains) :: auxhist24_interval
integer , DIMENSION(max_domains) :: auxhist24_begin_y
integer , DIMENSION(max_domains) :: auxhist24_begin_d
integer , DIMENSION(max_domains) :: auxhist24_begin_h
integer , DIMENSION(max_domains) :: auxhist24_begin_m
integer , DIMENSION(max_domains) :: auxhist24_begin_s
integer , DIMENSION(max_domains) :: auxhist24_begin
integer , DIMENSION(max_domains) :: auxhist24_end_y
integer , DIMENSION(max_domains) :: auxhist24_end_d
integer , DIMENSION(max_domains) :: auxhist24_end_h
integer , DIMENSION(max_domains) :: auxhist24_end_m
integer , DIMENSION(max_domains) :: auxhist24_end_s
integer , DIMENSION(max_domains) :: auxhist24_end
integer :: io_form_auxhist24
integer , DIMENSION(max_domains) :: frames_per_auxhist24
character*256 :: auxinput1_outname
integer , DIMENSION(max_domains) :: auxinput1_interval_y
integer , DIMENSION(max_domains) :: auxinput1_interval_d
integer , DIMENSION(max_domains) :: auxinput1_interval_h
integer , DIMENSION(max_domains) :: auxinput1_interval_m
integer , DIMENSION(max_domains) :: auxinput1_interval_s
integer , DIMENSION(max_domains) :: auxinput1_interval
integer , DIMENSION(max_domains) :: auxinput1_begin_y
integer , DIMENSION(max_domains) :: auxinput1_begin_d
integer , DIMENSION(max_domains) :: auxinput1_begin_h
integer , DIMENSION(max_domains) :: auxinput1_begin_m
integer , DIMENSION(max_domains) :: auxinput1_begin_s
integer , DIMENSION(max_domains) :: auxinput1_begin
integer , DIMENSION(max_domains) :: auxinput1_end_y
integer , DIMENSION(max_domains) :: auxinput1_end_d
integer , DIMENSION(max_domains) :: auxinput1_end_h
integer , DIMENSION(max_domains) :: auxinput1_end_m
integer , DIMENSION(max_domains) :: auxinput1_end_s
integer , DIMENSION(max_domains) :: auxinput1_end
integer , DIMENSION(max_domains) :: frames_per_auxinput1
character*256 :: auxinput2_inname
character*256 :: auxinput2_outname
integer , DIMENSION(max_domains) :: auxinput2_interval_y
integer , DIMENSION(max_domains) :: auxinput2_interval_d
integer , DIMENSION(max_domains) :: auxinput2_interval_h
integer , DIMENSION(max_domains) :: auxinput2_interval_m
integer , DIMENSION(max_domains) :: auxinput2_interval_s
integer , DIMENSION(max_domains) :: auxinput2_interval
integer , DIMENSION(max_domains) :: auxinput2_begin_y
integer , DIMENSION(max_domains) :: auxinput2_begin_d
integer , DIMENSION(max_domains) :: auxinput2_begin_h
integer , DIMENSION(max_domains) :: auxinput2_begin_m
integer , DIMENSION(max_domains) :: auxinput2_begin_s
integer , DIMENSION(max_domains) :: auxinput2_begin
integer , DIMENSION(max_domains) :: auxinput2_end_y
integer , DIMENSION(max_domains) :: auxinput2_end_d
integer , DIMENSION(max_domains) :: auxinput2_end_h
integer , DIMENSION(max_domains) :: auxinput2_end_m
integer , DIMENSION(max_domains) :: auxinput2_end_s
integer , DIMENSION(max_domains) :: auxinput2_end
integer , DIMENSION(max_domains) :: frames_per_auxinput2
character*256 :: auxinput3_inname
character*256 :: auxinput3_outname
integer , DIMENSION(max_domains) :: auxinput3_interval_y
integer , DIMENSION(max_domains) :: auxinput3_interval_d
integer , DIMENSION(max_domains) :: auxinput3_interval_h
integer , DIMENSION(max_domains) :: auxinput3_interval_m
integer , DIMENSION(max_domains) :: auxinput3_interval_s
integer , DIMENSION(max_domains) :: auxinput3_interval
integer , DIMENSION(max_domains) :: auxinput3_begin_y
integer , DIMENSION(max_domains) :: auxinput3_begin_d
integer , DIMENSION(max_domains) :: auxinput3_begin_h
integer , DIMENSION(max_domains) :: auxinput3_begin_m
integer , DIMENSION(max_domains) :: auxinput3_begin_s
integer , DIMENSION(max_domains) :: auxinput3_begin
integer , DIMENSION(max_domains) :: auxinput3_end_y
integer , DIMENSION(max_domains) :: auxinput3_end_d
integer , DIMENSION(max_domains) :: auxinput3_end_h
integer , DIMENSION(max_domains) :: auxinput3_end_m
integer , DIMENSION(max_domains) :: auxinput3_end_s
integer , DIMENSION(max_domains) :: auxinput3_end
integer :: io_form_auxinput3
integer , DIMENSION(max_domains) :: frames_per_auxinput3
character*256 :: auxinput4_inname
character*256 :: auxinput4_outname
integer , DIMENSION(max_domains) :: auxinput4_interval_y
integer , DIMENSION(max_domains) :: auxinput4_interval_d
integer , DIMENSION(max_domains) :: auxinput4_interval_h
integer , DIMENSION(max_domains) :: auxinput4_interval_m
integer , DIMENSION(max_domains) :: auxinput4_interval_s
integer , DIMENSION(max_domains) :: auxinput4_interval
integer , DIMENSION(max_domains) :: auxinput4_begin_y
integer , DIMENSION(max_domains) :: auxinput4_begin_d
integer , DIMENSION(max_domains) :: auxinput4_begin_h
integer , DIMENSION(max_domains) :: auxinput4_begin_m
integer , DIMENSION(max_domains) :: auxinput4_begin_s
integer , DIMENSION(max_domains) :: auxinput4_begin
integer , DIMENSION(max_domains) :: auxinput4_end_y
integer , DIMENSION(max_domains) :: auxinput4_end_d
integer , DIMENSION(max_domains) :: auxinput4_end_h
integer , DIMENSION(max_domains) :: auxinput4_end_m
integer , DIMENSION(max_domains) :: auxinput4_end_s
integer , DIMENSION(max_domains) :: auxinput4_end
integer :: io_form_auxinput4
integer , DIMENSION(max_domains) :: frames_per_auxinput4
character*256 :: auxinput5_inname
character*256 :: auxinput5_outname
integer , DIMENSION(max_domains) :: auxinput5_interval_y
integer , DIMENSION(max_domains) :: auxinput5_interval_d
integer , DIMENSION(max_domains) :: auxinput5_interval_h
integer , DIMENSION(max_domains) :: auxinput5_interval_m
integer , DIMENSION(max_domains) :: auxinput5_interval_s
integer , DIMENSION(max_domains) :: auxinput5_interval
integer , DIMENSION(max_domains) :: auxinput5_begin_y
integer , DIMENSION(max_domains) :: auxinput5_begin_d
integer , DIMENSION(max_domains) :: auxinput5_begin_h
integer , DIMENSION(max_domains) :: auxinput5_begin_m
integer , DIMENSION(max_domains) :: auxinput5_begin_s
integer , DIMENSION(max_domains) :: auxinput5_begin
integer , DIMENSION(max_domains) :: auxinput5_end_y
integer , DIMENSION(max_domains) :: auxinput5_end_d
integer , DIMENSION(max_domains) :: auxinput5_end_h
integer , DIMENSION(max_domains) :: auxinput5_end_m
integer , DIMENSION(max_domains) :: auxinput5_end_s
integer , DIMENSION(max_domains) :: auxinput5_end
integer :: io_form_auxinput5
integer , DIMENSION(max_domains) :: frames_per_auxinput5
character*256 :: auxinput6_inname
character*256 :: auxinput6_outname
integer , DIMENSION(max_domains) :: auxinput6_interval_y
integer , DIMENSION(max_domains) :: auxinput6_interval_d
integer , DIMENSION(max_domains) :: auxinput6_interval_h
integer , DIMENSION(max_domains) :: auxinput6_interval_m
integer , DIMENSION(max_domains) :: auxinput6_interval_s
integer , DIMENSION(max_domains) :: auxinput6_interval
integer , DIMENSION(max_domains) :: auxinput6_begin_y
integer , DIMENSION(max_domains) :: auxinput6_begin_d
integer , DIMENSION(max_domains) :: auxinput6_begin_h
integer , DIMENSION(max_domains) :: auxinput6_begin_m
integer , DIMENSION(max_domains) :: auxinput6_begin_s
integer , DIMENSION(max_domains) :: auxinput6_begin
integer , DIMENSION(max_domains) :: auxinput6_end_y
integer , DIMENSION(max_domains) :: auxinput6_end_d
integer , DIMENSION(max_domains) :: auxinput6_end_h
integer , DIMENSION(max_domains) :: auxinput6_end_m
integer , DIMENSION(max_domains) :: auxinput6_end_s
integer , DIMENSION(max_domains) :: auxinput6_end
integer :: io_form_auxinput6
integer , DIMENSION(max_domains) :: frames_per_auxinput6
character*256 :: auxinput7_inname
character*256 :: auxinput7_outname
integer , DIMENSION(max_domains) :: auxinput7_interval_y
integer , DIMENSION(max_domains) :: auxinput7_interval_d
integer , DIMENSION(max_domains) :: auxinput7_interval_h
integer , DIMENSION(max_domains) :: auxinput7_interval_m
integer , DIMENSION(max_domains) :: auxinput7_interval_s
integer , DIMENSION(max_domains) :: auxinput7_interval
integer , DIMENSION(max_domains) :: auxinput7_begin_y
integer , DIMENSION(max_domains) :: auxinput7_begin_d
integer , DIMENSION(max_domains) :: auxinput7_begin_h
integer , DIMENSION(max_domains) :: auxinput7_begin_m
integer , DIMENSION(max_domains) :: auxinput7_begin_s
integer , DIMENSION(max_domains) :: auxinput7_begin
integer , DIMENSION(max_domains) :: auxinput7_end_y
integer , DIMENSION(max_domains) :: auxinput7_end_d
integer , DIMENSION(max_domains) :: auxinput7_end_h
integer , DIMENSION(max_domains) :: auxinput7_end_m
integer , DIMENSION(max_domains) :: auxinput7_end_s
integer , DIMENSION(max_domains) :: auxinput7_end
integer :: io_form_auxinput7
integer , DIMENSION(max_domains) :: frames_per_auxinput7
character*256 :: auxinput8_inname
character*256 :: auxinput8_outname
integer , DIMENSION(max_domains) :: auxinput8_interval_y
integer , DIMENSION(max_domains) :: auxinput8_interval_d
integer , DIMENSION(max_domains) :: auxinput8_interval_h
integer , DIMENSION(max_domains) :: auxinput8_interval_m
integer , DIMENSION(max_domains) :: auxinput8_interval_s
integer , DIMENSION(max_domains) :: auxinput8_interval
integer , DIMENSION(max_domains) :: auxinput8_begin_y
integer , DIMENSION(max_domains) :: auxinput8_begin_d
integer , DIMENSION(max_domains) :: auxinput8_begin_h
integer , DIMENSION(max_domains) :: auxinput8_begin_m
integer , DIMENSION(max_domains) :: auxinput8_begin_s
integer , DIMENSION(max_domains) :: auxinput8_begin
integer , DIMENSION(max_domains) :: auxinput8_end_y
integer , DIMENSION(max_domains) :: auxinput8_end_d
integer , DIMENSION(max_domains) :: auxinput8_end_h
integer , DIMENSION(max_domains) :: auxinput8_end_m
integer , DIMENSION(max_domains) :: auxinput8_end_s
integer , DIMENSION(max_domains) :: auxinput8_end
integer :: io_form_auxinput8
integer , DIMENSION(max_domains) :: frames_per_auxinput8
character*256 :: auxinput9_inname
character*256 :: auxinput9_outname
integer , DIMENSION(max_domains) :: auxinput9_interval_y
integer , DIMENSION(max_domains) :: auxinput9_interval_d
integer , DIMENSION(max_domains) :: auxinput9_interval_h
integer , DIMENSION(max_domains) :: auxinput9_interval_m
integer , DIMENSION(max_domains) :: auxinput9_interval_s
integer , DIMENSION(max_domains) :: auxinput9_interval
integer , DIMENSION(max_domains) :: auxinput9_begin_y
integer , DIMENSION(max_domains) :: auxinput9_begin_d
integer , DIMENSION(max_domains) :: auxinput9_begin_h
integer , DIMENSION(max_domains) :: auxinput9_begin_m
integer , DIMENSION(max_domains) :: auxinput9_begin_s
integer , DIMENSION(max_domains) :: auxinput9_begin
integer , DIMENSION(max_domains) :: auxinput9_end_y
integer , DIMENSION(max_domains) :: auxinput9_end_d
integer , DIMENSION(max_domains) :: auxinput9_end_h
integer , DIMENSION(max_domains) :: auxinput9_end_m
integer , DIMENSION(max_domains) :: auxinput9_end_s
integer , DIMENSION(max_domains) :: auxinput9_end
integer :: io_form_auxinput9
integer , DIMENSION(max_domains) :: frames_per_auxinput9
character*256 :: auxinput10_inname
character*256 :: auxinput10_outname
integer , DIMENSION(max_domains) :: auxinput10_interval_y
integer , DIMENSION(max_domains) :: auxinput10_interval_d
integer , DIMENSION(max_domains) :: auxinput10_interval_h
integer , DIMENSION(max_domains) :: auxinput10_interval_m
integer , DIMENSION(max_domains) :: auxinput10_interval_s
integer , DIMENSION(max_domains) :: auxinput10_interval
integer , DIMENSION(max_domains) :: auxinput10_begin_y
integer , DIMENSION(max_domains) :: auxinput10_begin_d
integer , DIMENSION(max_domains) :: auxinput10_begin_h
integer , DIMENSION(max_domains) :: auxinput10_begin_m
integer , DIMENSION(max_domains) :: auxinput10_begin_s
integer , DIMENSION(max_domains) :: auxinput10_begin
integer , DIMENSION(max_domains) :: auxinput10_end_y
integer , DIMENSION(max_domains) :: auxinput10_end_d
integer , DIMENSION(max_domains) :: auxinput10_end_h
integer , DIMENSION(max_domains) :: auxinput10_end_m
integer , DIMENSION(max_domains) :: auxinput10_end_s
integer , DIMENSION(max_domains) :: auxinput10_end
integer :: io_form_auxinput10
integer , DIMENSION(max_domains) :: frames_per_auxinput10
character*256 :: auxinput11_inname
character*256 :: auxinput11_outname
integer , DIMENSION(max_domains) :: auxinput11_interval_y
integer , DIMENSION(max_domains) :: auxinput11_interval_d
integer , DIMENSION(max_domains) :: auxinput11_interval_h
integer , DIMENSION(max_domains) :: auxinput11_interval_m
integer , DIMENSION(max_domains) :: auxinput11_interval_s
integer , DIMENSION(max_domains) :: auxinput11_interval
integer , DIMENSION(max_domains) :: auxinput11_begin_y
integer , DIMENSION(max_domains) :: auxinput11_begin_d
integer , DIMENSION(max_domains) :: auxinput11_begin_h
integer , DIMENSION(max_domains) :: auxinput11_begin_m
integer , DIMENSION(max_domains) :: auxinput11_begin_s
integer , DIMENSION(max_domains) :: auxinput11_begin
integer , DIMENSION(max_domains) :: auxinput11_end_y
integer , DIMENSION(max_domains) :: auxinput11_end_d
integer , DIMENSION(max_domains) :: auxinput11_end_h
integer , DIMENSION(max_domains) :: auxinput11_end_m
integer , DIMENSION(max_domains) :: auxinput11_end_s
integer , DIMENSION(max_domains) :: auxinput11_end
integer :: io_form_auxinput11
integer , DIMENSION(max_domains) :: frames_per_auxinput11
character*256 :: auxinput12_inname
character*256 :: auxinput12_outname
integer , DIMENSION(max_domains) :: auxinput12_interval_y
integer , DIMENSION(max_domains) :: auxinput12_interval_d
integer , DIMENSION(max_domains) :: auxinput12_interval_h
integer , DIMENSION(max_domains) :: auxinput12_interval_m
integer , DIMENSION(max_domains) :: auxinput12_interval_s
integer , DIMENSION(max_domains) :: auxinput12_interval
integer , DIMENSION(max_domains) :: auxinput12_begin_y
integer , DIMENSION(max_domains) :: auxinput12_begin_d
integer , DIMENSION(max_domains) :: auxinput12_begin_h
integer , DIMENSION(max_domains) :: auxinput12_begin_m
integer , DIMENSION(max_domains) :: auxinput12_begin_s
integer , DIMENSION(max_domains) :: auxinput12_begin
integer , DIMENSION(max_domains) :: auxinput12_end_y
integer , DIMENSION(max_domains) :: auxinput12_end_d
integer , DIMENSION(max_domains) :: auxinput12_end_h
integer , DIMENSION(max_domains) :: auxinput12_end_m
integer , DIMENSION(max_domains) :: auxinput12_end_s
integer , DIMENSION(max_domains) :: auxinput12_end
integer :: io_form_auxinput12
integer , DIMENSION(max_domains) :: frames_per_auxinput12
character*256 :: auxinput13_inname
character*256 :: auxinput13_outname
integer , DIMENSION(max_domains) :: auxinput13_interval_y
integer , DIMENSION(max_domains) :: auxinput13_interval_d
integer , DIMENSION(max_domains) :: auxinput13_interval_h
integer , DIMENSION(max_domains) :: auxinput13_interval_m
integer , DIMENSION(max_domains) :: auxinput13_interval_s
integer , DIMENSION(max_domains) :: auxinput13_interval
integer , DIMENSION(max_domains) :: auxinput13_begin_y
integer , DIMENSION(max_domains) :: auxinput13_begin_d
integer , DIMENSION(max_domains) :: auxinput13_begin_h
integer , DIMENSION(max_domains) :: auxinput13_begin_m
integer , DIMENSION(max_domains) :: auxinput13_begin_s
integer , DIMENSION(max_domains) :: auxinput13_begin
integer , DIMENSION(max_domains) :: auxinput13_end_y
integer , DIMENSION(max_domains) :: auxinput13_end_d
integer , DIMENSION(max_domains) :: auxinput13_end_h
integer , DIMENSION(max_domains) :: auxinput13_end_m
integer , DIMENSION(max_domains) :: auxinput13_end_s
integer , DIMENSION(max_domains) :: auxinput13_end
integer :: io_form_auxinput13
integer , DIMENSION(max_domains) :: frames_per_auxinput13
character*256 :: auxinput14_inname
character*256 :: auxinput14_outname
integer , DIMENSION(max_domains) :: auxinput14_interval_y
integer , DIMENSION(max_domains) :: auxinput14_interval_d
integer , DIMENSION(max_domains) :: auxinput14_interval_h
integer , DIMENSION(max_domains) :: auxinput14_interval_m
integer , DIMENSION(max_domains) :: auxinput14_interval_s
integer , DIMENSION(max_domains) :: auxinput14_interval
integer , DIMENSION(max_domains) :: auxinput14_begin_y
integer , DIMENSION(max_domains) :: auxinput14_begin_d
integer , DIMENSION(max_domains) :: auxinput14_begin_h
integer , DIMENSION(max_domains) :: auxinput14_begin_m
integer , DIMENSION(max_domains) :: auxinput14_begin_s
integer , DIMENSION(max_domains) :: auxinput14_begin
integer , DIMENSION(max_domains) :: auxinput14_end_y
integer , DIMENSION(max_domains) :: auxinput14_end_d
integer , DIMENSION(max_domains) :: auxinput14_end_h
integer , DIMENSION(max_domains) :: auxinput14_end_m
integer , DIMENSION(max_domains) :: auxinput14_end_s
integer , DIMENSION(max_domains) :: auxinput14_end
integer :: io_form_auxinput14
integer , DIMENSION(max_domains) :: frames_per_auxinput14
character*256 :: auxinput15_inname
character*256 :: auxinput15_outname
integer , DIMENSION(max_domains) :: auxinput15_interval_y
integer , DIMENSION(max_domains) :: auxinput15_interval_d
integer , DIMENSION(max_domains) :: auxinput15_interval_h
integer , DIMENSION(max_domains) :: auxinput15_interval_m
integer , DIMENSION(max_domains) :: auxinput15_interval_s
integer , DIMENSION(max_domains) :: auxinput15_interval
integer , DIMENSION(max_domains) :: auxinput15_begin_y
integer , DIMENSION(max_domains) :: auxinput15_begin_d
integer , DIMENSION(max_domains) :: auxinput15_begin_h
integer , DIMENSION(max_domains) :: auxinput15_begin_m
integer , DIMENSION(max_domains) :: auxinput15_begin_s
integer , DIMENSION(max_domains) :: auxinput15_begin
integer , DIMENSION(max_domains) :: auxinput15_end_y
integer , DIMENSION(max_domains) :: auxinput15_end_d
integer , DIMENSION(max_domains) :: auxinput15_end_h
integer , DIMENSION(max_domains) :: auxinput15_end_m
integer , DIMENSION(max_domains) :: auxinput15_end_s
integer , DIMENSION(max_domains) :: auxinput15_end
integer :: io_form_auxinput15
integer , DIMENSION(max_domains) :: frames_per_auxinput15
character*256 :: auxinput16_inname
character*256 :: auxinput16_outname
integer , DIMENSION(max_domains) :: auxinput16_interval_y
integer , DIMENSION(max_domains) :: auxinput16_interval_d
integer , DIMENSION(max_domains) :: auxinput16_interval_h
integer , DIMENSION(max_domains) :: auxinput16_interval_m
integer , DIMENSION(max_domains) :: auxinput16_interval_s
integer , DIMENSION(max_domains) :: auxinput16_interval
integer , DIMENSION(max_domains) :: auxinput16_begin_y
integer , DIMENSION(max_domains) :: auxinput16_begin_d
integer , DIMENSION(max_domains) :: auxinput16_begin_h
integer , DIMENSION(max_domains) :: auxinput16_begin_m
integer , DIMENSION(max_domains) :: auxinput16_begin_s
integer , DIMENSION(max_domains) :: auxinput16_begin
integer , DIMENSION(max_domains) :: auxinput16_end_y
integer , DIMENSION(max_domains) :: auxinput16_end_d
integer , DIMENSION(max_domains) :: auxinput16_end_h
integer , DIMENSION(max_domains) :: auxinput16_end_m
integer , DIMENSION(max_domains) :: auxinput16_end_s
integer , DIMENSION(max_domains) :: auxinput16_end
integer :: io_form_auxinput16
integer , DIMENSION(max_domains) :: frames_per_auxinput16
character*256 :: auxinput17_inname
character*256 :: auxinput17_outname
integer , DIMENSION(max_domains) :: auxinput17_interval_y
integer , DIMENSION(max_domains) :: auxinput17_interval_d
integer , DIMENSION(max_domains) :: auxinput17_interval_h
integer , DIMENSION(max_domains) :: auxinput17_interval_m
integer , DIMENSION(max_domains) :: auxinput17_interval_s
integer , DIMENSION(max_domains) :: auxinput17_interval
integer , DIMENSION(max_domains) :: auxinput17_begin_y
integer , DIMENSION(max_domains) :: auxinput17_begin_d
integer , DIMENSION(max_domains) :: auxinput17_begin_h
integer , DIMENSION(max_domains) :: auxinput17_begin_m
integer , DIMENSION(max_domains) :: auxinput17_begin_s
integer , DIMENSION(max_domains) :: auxinput17_begin
integer , DIMENSION(max_domains) :: auxinput17_end_y
integer , DIMENSION(max_domains) :: auxinput17_end_d
integer , DIMENSION(max_domains) :: auxinput17_end_h
integer , DIMENSION(max_domains) :: auxinput17_end_m
integer , DIMENSION(max_domains) :: auxinput17_end_s
integer , DIMENSION(max_domains) :: auxinput17_end
integer :: io_form_auxinput17
integer , DIMENSION(max_domains) :: frames_per_auxinput17
character*256 :: auxinput18_inname
character*256 :: auxinput18_outname
integer , DIMENSION(max_domains) :: auxinput18_interval_y
integer , DIMENSION(max_domains) :: auxinput18_interval_d
integer , DIMENSION(max_domains) :: auxinput18_interval_h
integer , DIMENSION(max_domains) :: auxinput18_interval_m
integer , DIMENSION(max_domains) :: auxinput18_interval_s
integer , DIMENSION(max_domains) :: auxinput18_interval
integer , DIMENSION(max_domains) :: auxinput18_begin_y
integer , DIMENSION(max_domains) :: auxinput18_begin_d
integer , DIMENSION(max_domains) :: auxinput18_begin_h
integer , DIMENSION(max_domains) :: auxinput18_begin_m
integer , DIMENSION(max_domains) :: auxinput18_begin_s
integer , DIMENSION(max_domains) :: auxinput18_begin
integer , DIMENSION(max_domains) :: auxinput18_end_y
integer , DIMENSION(max_domains) :: auxinput18_end_d
integer , DIMENSION(max_domains) :: auxinput18_end_h
integer , DIMENSION(max_domains) :: auxinput18_end_m
integer , DIMENSION(max_domains) :: auxinput18_end_s
integer , DIMENSION(max_domains) :: auxinput18_end
integer :: io_form_auxinput18
integer , DIMENSION(max_domains) :: frames_per_auxinput18
character*256 :: auxinput19_inname
character*256 :: auxinput19_outname
integer , DIMENSION(max_domains) :: auxinput19_interval_y
integer , DIMENSION(max_domains) :: auxinput19_interval_d
integer , DIMENSION(max_domains) :: auxinput19_interval_h
integer , DIMENSION(max_domains) :: auxinput19_interval_m
integer , DIMENSION(max_domains) :: auxinput19_interval_s
integer , DIMENSION(max_domains) :: auxinput19_interval
integer , DIMENSION(max_domains) :: auxinput19_begin_y
integer , DIMENSION(max_domains) :: auxinput19_begin_d
integer , DIMENSION(max_domains) :: auxinput19_begin_h
integer , DIMENSION(max_domains) :: auxinput19_begin_m
integer , DIMENSION(max_domains) :: auxinput19_begin_s
integer , DIMENSION(max_domains) :: auxinput19_begin
integer , DIMENSION(max_domains) :: auxinput19_end_y
integer , DIMENSION(max_domains) :: auxinput19_end_d
integer , DIMENSION(max_domains) :: auxinput19_end_h
integer , DIMENSION(max_domains) :: auxinput19_end_m
integer , DIMENSION(max_domains) :: auxinput19_end_s
integer , DIMENSION(max_domains) :: auxinput19_end
integer :: io_form_auxinput19
integer , DIMENSION(max_domains) :: frames_per_auxinput19
character*256 :: auxinput20_inname
character*256 :: auxinput20_outname
integer , DIMENSION(max_domains) :: auxinput20_interval_y
integer , DIMENSION(max_domains) :: auxinput20_interval_d
integer , DIMENSION(max_domains) :: auxinput20_interval_h
integer , DIMENSION(max_domains) :: auxinput20_interval_m
integer , DIMENSION(max_domains) :: auxinput20_interval_s
integer , DIMENSION(max_domains) :: auxinput20_interval
integer , DIMENSION(max_domains) :: auxinput20_begin_y
integer , DIMENSION(max_domains) :: auxinput20_begin_d
integer , DIMENSION(max_domains) :: auxinput20_begin_h
integer , DIMENSION(max_domains) :: auxinput20_begin_m
integer , DIMENSION(max_domains) :: auxinput20_begin_s
integer , DIMENSION(max_domains) :: auxinput20_begin
integer , DIMENSION(max_domains) :: auxinput20_end_y
integer , DIMENSION(max_domains) :: auxinput20_end_d
integer , DIMENSION(max_domains) :: auxinput20_end_h
integer , DIMENSION(max_domains) :: auxinput20_end_m
integer , DIMENSION(max_domains) :: auxinput20_end_s
integer , DIMENSION(max_domains) :: auxinput20_end
integer :: io_form_auxinput20
integer , DIMENSION(max_domains) :: frames_per_auxinput20
character*256 :: auxinput21_inname
character*256 :: auxinput21_outname
integer , DIMENSION(max_domains) :: auxinput21_interval_y
integer , DIMENSION(max_domains) :: auxinput21_interval_d
integer , DIMENSION(max_domains) :: auxinput21_interval_h
integer , DIMENSION(max_domains) :: auxinput21_interval_m
integer , DIMENSION(max_domains) :: auxinput21_interval_s
integer , DIMENSION(max_domains) :: auxinput21_interval
integer , DIMENSION(max_domains) :: auxinput21_begin_y
integer , DIMENSION(max_domains) :: auxinput21_begin_d
integer , DIMENSION(max_domains) :: auxinput21_begin_h
integer , DIMENSION(max_domains) :: auxinput21_begin_m
integer , DIMENSION(max_domains) :: auxinput21_begin_s
integer , DIMENSION(max_domains) :: auxinput21_begin
integer , DIMENSION(max_domains) :: auxinput21_end_y
integer , DIMENSION(max_domains) :: auxinput21_end_d
integer , DIMENSION(max_domains) :: auxinput21_end_h
integer , DIMENSION(max_domains) :: auxinput21_end_m
integer , DIMENSION(max_domains) :: auxinput21_end_s
integer , DIMENSION(max_domains) :: auxinput21_end
integer :: io_form_auxinput21
integer , DIMENSION(max_domains) :: frames_per_auxinput21
character*256 :: auxinput22_inname
character*256 :: auxinput22_outname
integer , DIMENSION(max_domains) :: auxinput22_interval_y
integer , DIMENSION(max_domains) :: auxinput22_interval_d
integer , DIMENSION(max_domains) :: auxinput22_interval_h
integer , DIMENSION(max_domains) :: auxinput22_interval_m
integer , DIMENSION(max_domains) :: auxinput22_interval_s
integer , DIMENSION(max_domains) :: auxinput22_interval
integer , DIMENSION(max_domains) :: auxinput22_begin_y
integer , DIMENSION(max_domains) :: auxinput22_begin_d
integer , DIMENSION(max_domains) :: auxinput22_begin_h
integer , DIMENSION(max_domains) :: auxinput22_begin_m
integer , DIMENSION(max_domains) :: auxinput22_begin_s
integer , DIMENSION(max_domains) :: auxinput22_begin
integer , DIMENSION(max_domains) :: auxinput22_end_y
integer , DIMENSION(max_domains) :: auxinput22_end_d
integer , DIMENSION(max_domains) :: auxinput22_end_h
integer , DIMENSION(max_domains) :: auxinput22_end_m
integer , DIMENSION(max_domains) :: auxinput22_end_s
integer , DIMENSION(max_domains) :: auxinput22_end
integer :: io_form_auxinput22
integer , DIMENSION(max_domains) :: frames_per_auxinput22
character*256 :: auxinput23_inname
character*256 :: auxinput23_outname
integer , DIMENSION(max_domains) :: auxinput23_interval_y
integer , DIMENSION(max_domains) :: auxinput23_interval_d
integer , DIMENSION(max_domains) :: auxinput23_interval_h
integer , DIMENSION(max_domains) :: auxinput23_interval_m
integer , DIMENSION(max_domains) :: auxinput23_interval_s
integer , DIMENSION(max_domains) :: auxinput23_interval
integer , DIMENSION(max_domains) :: auxinput23_begin_y
integer , DIMENSION(max_domains) :: auxinput23_begin_d
integer , DIMENSION(max_domains) :: auxinput23_begin_h
integer , DIMENSION(max_domains) :: auxinput23_begin_m
integer , DIMENSION(max_domains) :: auxinput23_begin_s
integer , DIMENSION(max_domains) :: auxinput23_begin
integer , DIMENSION(max_domains) :: auxinput23_end_y
integer , DIMENSION(max_domains) :: auxinput23_end_d
integer , DIMENSION(max_domains) :: auxinput23_end_h
integer , DIMENSION(max_domains) :: auxinput23_end_m
integer , DIMENSION(max_domains) :: auxinput23_end_s
integer , DIMENSION(max_domains) :: auxinput23_end
integer :: io_form_auxinput23
integer , DIMENSION(max_domains) :: frames_per_auxinput23
character*256 :: auxinput24_inname
character*256 :: auxinput24_outname
integer , DIMENSION(max_domains) :: auxinput24_interval_y
integer , DIMENSION(max_domains) :: auxinput24_interval_d
integer , DIMENSION(max_domains) :: auxinput24_interval_h
integer , DIMENSION(max_domains) :: auxinput24_interval_m
integer , DIMENSION(max_domains) :: auxinput24_interval_s
integer , DIMENSION(max_domains) :: auxinput24_interval
integer , DIMENSION(max_domains) :: auxinput24_begin_y
integer , DIMENSION(max_domains) :: auxinput24_begin_d
integer , DIMENSION(max_domains) :: auxinput24_begin_h
integer , DIMENSION(max_domains) :: auxinput24_begin_m
integer , DIMENSION(max_domains) :: auxinput24_begin_s
integer , DIMENSION(max_domains) :: auxinput24_begin
integer , DIMENSION(max_domains) :: auxinput24_end_y
integer , DIMENSION(max_domains) :: auxinput24_end_d
integer , DIMENSION(max_domains) :: auxinput24_end_h
integer , DIMENSION(max_domains) :: auxinput24_end_m
integer , DIMENSION(max_domains) :: auxinput24_end_s
integer , DIMENSION(max_domains) :: auxinput24_end
integer :: io_form_auxinput24
integer , DIMENSION(max_domains) :: frames_per_auxinput24
integer , DIMENSION(max_domains) :: history_interval
integer , DIMENSION(max_domains) :: frames_per_outfile
logical :: restart
integer :: restart_interval
integer :: io_form_input
integer :: io_form_history
integer :: io_form_restart
integer :: io_form_boundary
integer :: debug_level
logical :: self_test_domain
character*256 :: history_outname
character*256 :: history_inname
logical :: use_netcdf_classic
integer , DIMENSION(max_domains) :: history_interval_d
integer , DIMENSION(max_domains) :: history_interval_h
integer , DIMENSION(max_domains) :: history_interval_m
integer , DIMENSION(max_domains) :: history_interval_s
integer , DIMENSION(max_domains) :: inputout_interval_d
integer , DIMENSION(max_domains) :: inputout_interval_h
integer , DIMENSION(max_domains) :: inputout_interval_m
integer , DIMENSION(max_domains) :: inputout_interval_s
integer , DIMENSION(max_domains) :: inputout_interval
integer :: restart_interval_d
integer :: restart_interval_h
integer :: restart_interval_m
integer :: restart_interval_s
integer , DIMENSION(max_domains) :: history_begin_y
integer , DIMENSION(max_domains) :: history_begin_d
integer , DIMENSION(max_domains) :: history_begin_h
integer , DIMENSION(max_domains) :: history_begin_m
integer , DIMENSION(max_domains) :: history_begin_s
integer , DIMENSION(max_domains) :: history_begin
integer , DIMENSION(max_domains) :: inputout_begin_y
integer , DIMENSION(max_domains) :: inputout_begin_d
integer , DIMENSION(max_domains) :: inputout_begin_h
integer , DIMENSION(max_domains) :: inputout_begin_m
integer , DIMENSION(max_domains) :: inputout_begin_s
integer :: restart_begin_y
integer :: restart_begin_d
integer :: restart_begin_h
integer :: restart_begin_m
integer :: restart_begin_s
integer :: restart_begin
integer , DIMENSION(max_domains) :: history_end_y
integer , DIMENSION(max_domains) :: history_end_d
integer , DIMENSION(max_domains) :: history_end_h
integer , DIMENSION(max_domains) :: history_end_m
integer , DIMENSION(max_domains) :: history_end_s
integer , DIMENSION(max_domains) :: history_end
integer , DIMENSION(max_domains) :: inputout_end_y
integer , DIMENSION(max_domains) :: inputout_end_d
integer , DIMENSION(max_domains) :: inputout_end_h
integer , DIMENSION(max_domains) :: inputout_end_m
integer , DIMENSION(max_domains) :: inputout_end_s
integer :: simulation_start_year
integer :: simulation_start_month
integer :: simulation_start_day
integer :: simulation_start_hour
integer :: simulation_start_minute
integer :: simulation_start_second
logical :: reset_simulation_start
integer , DIMENSION(max_domains) :: sr_x
integer , DIMENSION(max_domains) :: sr_y
character*256 , DIMENSION(max_domains) :: iofields_filename
logical :: ignore_iofields_warning
logical :: ncd_nofill
integer , DIMENSION(max_domains) :: julyr
integer , DIMENSION(max_domains) :: julday
real , DIMENSION(max_domains) :: gmt
character*256 :: high_freq_outname
character*256 :: partial_atcf_outname
character*256 :: input_inname
character*256 :: input_outname
character*256 :: bdy_inname
character*256 :: bdy_outname
character*256 :: rst_inname
character*256 :: rst_outname
character*256 , DIMENSION(max_domains) :: anl_outname
logical :: write_input
logical :: write_restart_at_0h
logical :: write_hist_at_0h_rst
logical :: adjust_output_times
logical :: adjust_input_times
real , DIMENSION(max_domains) :: tstart
logical :: nocolons
logical :: cycling
logical :: output_ready_flag
integer :: dfi_opt
integer :: dfi_savehydmeteors
integer :: dfi_nfilter
logical :: dfi_write_filtered_input
logical :: dfi_write_dfi_history
integer :: dfi_cutoff_seconds
integer :: dfi_time_dim
integer :: dfi_fwdstop_year
integer :: dfi_fwdstop_month
integer :: dfi_fwdstop_day
integer :: dfi_fwdstop_hour
integer :: dfi_fwdstop_minute
integer :: dfi_fwdstop_second
integer :: dfi_bckstop_year
integer :: dfi_bckstop_month
integer :: dfi_bckstop_day
integer :: dfi_bckstop_hour
integer :: dfi_bckstop_minute
integer :: dfi_bckstop_second
integer :: time_step
integer :: time_step_fract_num
integer :: time_step_fract_den
integer :: time_step_dfi
integer :: max_dom
integer , DIMENSION(max_domains) :: s_we
integer , DIMENSION(max_domains) :: e_we
integer , DIMENSION(max_domains) :: s_sn
integer , DIMENSION(max_domains) :: e_sn
integer , DIMENSION(max_domains) :: s_vert
integer , DIMENSION(max_domains) :: e_vert
integer :: num_metgrid_soil_levels
real , DIMENSION(max_domains) :: dx
real , DIMENSION(max_domains) :: dy
integer , DIMENSION(max_domains) :: grid_id
logical , DIMENSION(max_domains) :: grid_allowed
integer , DIMENSION(max_domains) :: parent_id
integer , DIMENSION(max_domains) :: i_parent_start
integer , DIMENSION(max_domains) :: j_parent_start
integer , DIMENSION(max_domains) :: parent_grid_ratio
integer , DIMENSION(max_domains) :: parent_time_step_ratio
integer :: feedback
integer :: smooth_option
real , DIMENSION(max_domains) :: ztop
integer , DIMENSION(max_domains) :: moad_grid_ratio
integer , DIMENSION(max_domains) :: moad_time_step_ratio
integer , DIMENSION(max_domains) :: shw
integer :: tile_sz_x
integer :: tile_sz_y
integer :: numtiles
integer :: numtiles_inc
integer :: numtiles_x
integer :: numtiles_y
integer :: tile_strategy
integer :: nproc_x
integer :: nproc_y
integer :: irand
real , DIMENSION(max_domains) :: dt
integer :: ts_buf_size
integer :: max_ts_locs
integer :: num_moves
integer , DIMENSION(max_domains) :: vortex_interval
integer , DIMENSION(max_domains) :: corral_dist
integer , DIMENSION(max_moves) :: move_id
integer , DIMENSION(max_moves) :: move_interval
integer , DIMENSION(max_moves) :: move_cd_x
integer , DIMENSION(max_moves) :: move_cd_y
logical , DIMENSION(max_domains) :: swap_x
logical , DIMENSION(max_domains) :: swap_y
logical , DIMENSION(max_domains) :: cycle_x
logical , DIMENSION(max_domains) :: cycle_y
logical :: reorder_mesh
logical :: perturb_input
real , DIMENSION(max_eta) :: eta_levels
real :: ptsgm
integer :: num_metgrid_levels
real :: p_top_requested
logical :: use_prep_hybrid
logical :: force_read_thompson
logical :: write_thompson_tables
integer , DIMENSION(max_domains) :: mp_physics
real , DIMENSION(max_domains) :: mommix
logical , DIMENSION(max_domains) :: disheat
integer :: do_radar_ref
integer :: compute_radar_ref
integer , DIMENSION(max_domains) :: ra_lw_physics
integer , DIMENSION(max_domains) :: ra_sw_physics
real , DIMENSION(max_domains) :: radt
integer , DIMENSION(max_domains) :: sf_sfclay_physics
integer , DIMENSION(max_domains) :: sf_surface_physics
integer , DIMENSION(max_domains) :: bl_pbl_physics
integer , DIMENSION(max_domains) :: ysu_topdown_pblmix
integer , DIMENSION(max_domains) :: shinhong_tke_diag
integer , DIMENSION(max_domains) :: windfarm_opt
integer :: windfarm_ij
integer , DIMENSION(max_domains) :: mfshconv
real , DIMENSION(max_domains) :: bldt
integer , DIMENSION(max_domains) :: cu_physics
integer , DIMENSION(max_domains) :: shcu_physics
integer , DIMENSION(max_domains) :: cu_diag
real , DIMENSION(max_domains) :: gfs_alpha
real , DIMENSION(max_domains) :: cudt
real , DIMENSION(max_domains) :: gsmdt
integer :: isfflx
integer :: ideal_xland
integer :: ifsnow
integer :: icloud
real :: swrad_scat
integer :: surface_input_source
integer :: num_soil_layers
integer :: num_urban_layers
integer :: sf_surface_mosaic
integer :: mosaic_cat
integer :: mosaic_cat_soil
integer :: num_urban_hi
integer :: mosaic_lu
integer :: mosaic_soil
integer :: maxiens
integer :: maxens
integer :: maxens2
integer :: maxens3
integer :: ensdim
integer , DIMENSION(max_domains) :: chem_opt
integer :: num_land_cat
integer :: num_soil_cat
integer , DIMENSION(max_domains) :: topo_wind
integer :: mp_zero_out
real :: mp_zero_out_thresh
real :: seaice_threshold
integer :: fractional_seaice
integer :: seaice_albedo_opt
real :: seaice_albedo_default
integer :: seaice_snowdepth_opt
real :: seaice_snowdepth_max
real :: seaice_snowdepth_min
integer :: seaice_thickness_opt
real :: seaice_thickness_default
logical :: tice2tsk_if2cold
integer :: sst_update
integer , DIMENSION(max_domains) :: sf_urban_physics
logical :: usemonalb
logical :: rdmaxalb
logical :: rdlai2d
logical :: ua_phys
integer , DIMENSION(max_domains) :: gwd_opt
integer :: iz0tlnd
real , DIMENSION(max_domains) :: sas_pgcon
real , DIMENSION(max_domains) :: sas_shal_pgcon
integer , DIMENSION(max_domains) :: sas_shal_conv
real , DIMENSION(max_domains) :: sas_mass_flux
real :: var_ric
real :: coef_ric_l
real :: coef_ric_s
integer , DIMENSION(max_domains) :: random_seed
integer , DIMENSION(max_domains) :: icoef_sf
logical , DIMENSION(max_domains) :: lcurr_sf
integer , DIMENSION(max_domains) :: ens_random_seed
logical :: pert_sas
logical :: pert_pbl
real , DIMENSION(max_domains) :: ens_sasamp
real , DIMENSION(max_domains) :: ens_pblamp
integer , DIMENSION(max_domains) :: idtad
integer , DIMENSION(max_domains) :: nsoil
integer , DIMENSION(max_domains) :: nphs
integer , DIMENSION(max_domains) :: ncnvc
integer , DIMENSION(max_domains) :: nrand
integer , DIMENSION(max_domains) :: nrads
integer , DIMENSION(max_domains) :: nradl
real , DIMENSION(max_domains) :: tprec
real , DIMENSION(max_domains) :: theat
real , DIMENSION(max_domains) :: tclod
real , DIMENSION(max_domains) :: trdsw
real , DIMENSION(max_domains) :: trdlw
real , DIMENSION(max_domains) :: tsrfc
logical , DIMENSION(max_domains) :: pcpflg
integer , DIMENSION(max_domains) :: sigma
real , DIMENSION(max_domains) :: sfenth
integer :: co2tf
integer :: ra_call_offset
real :: cam_abs_freq_s
integer :: levsiz
integer :: paerlev
integer :: cam_abs_dim1
integer :: cam_abs_dim2
integer :: no_src_types
integer :: alevsiz
integer :: o3input
integer :: aer_opt
logical , DIMENSION(max_domains) :: cu_rad_feedback
integer :: icloud_cu
real , DIMENSION(max_domains) :: h_diff
integer , DIMENSION(max_domains) :: movemin
integer :: num_snso_layers
integer :: num_snow_layers
logical :: use_aero_icbc
real :: ccn_conc
integer :: hail_opt
integer , DIMENSION(max_domains) :: sf_lake_physics
integer :: dyn_opt
integer :: rk_ord
integer :: w_damping
integer , DIMENSION(max_domains) :: diff_opt
integer , DIMENSION(max_domains) :: km_opt
integer :: damp_opt
real , DIMENSION(max_domains) :: zdamp
real :: base_pres
real :: base_temp
real :: base_lapse
real :: iso_temp
real , DIMENSION(max_domains) :: dampcoef
real , DIMENSION(max_domains) :: khdif
real , DIMENSION(max_domains) :: kvdif
real , DIMENSION(max_domains) :: c_s
real , DIMENSION(max_domains) :: c_k
real , DIMENSION(max_domains) :: smdiv
real , DIMENSION(max_domains) :: emdiv
real , DIMENSION(max_domains) :: epssm
logical , DIMENSION(max_domains) :: non_hydrostatic
integer , DIMENSION(max_domains) :: time_step_sound
integer , DIMENSION(max_domains) :: h_mom_adv_order
integer , DIMENSION(max_domains) :: v_mom_adv_order
integer , DIMENSION(max_domains) :: h_sca_adv_order
integer , DIMENSION(max_domains) :: v_sca_adv_order
logical , DIMENSION(max_domains) :: top_radiation
real , DIMENSION(max_domains) :: tke_upper_bound
real , DIMENSION(max_domains) :: tke_drag_coefficient
real , DIMENSION(max_domains) :: tke_heat_flux
logical , DIMENSION(max_domains) :: pert_coriolis
logical :: euler_adv
integer :: idtadt
integer :: idtadc
real , DIMENSION(max_domains) :: codamp
real , DIMENSION(max_domains) :: coac
real , DIMENSION(max_domains) :: slophc
real , DIMENSION(max_domains) :: wp
integer :: terrain_smoothing
integer :: spec_bdy_width
integer :: spec_zone
integer :: relax_zone
logical , DIMENSION(max_domains) :: specified
logical , DIMENSION(max_domains) :: periodic_x
logical , DIMENSION(max_domains) :: symmetric_xs
logical , DIMENSION(max_domains) :: symmetric_xe
logical , DIMENSION(max_domains) :: open_xs
logical , DIMENSION(max_domains) :: open_xe
logical , DIMENSION(max_domains) :: periodic_y
logical , DIMENSION(max_domains) :: symmetric_ys
logical , DIMENSION(max_domains) :: symmetric_ye
logical , DIMENSION(max_domains) :: open_ys
logical , DIMENSION(max_domains) :: open_ye
logical , DIMENSION(max_domains) :: polar
logical , DIMENSION(max_domains) :: nested
integer :: real_data_init_type
integer :: background_proc_id
integer :: forecast_proc_id
integer :: production_status
integer :: compression
real , DIMENSION(max_domains) :: cen_lat
real , DIMENSION(max_domains) :: cen_lon
real , DIMENSION(max_domains) :: truelat1
real , DIMENSION(max_domains) :: truelat2
real , DIMENSION(max_domains) :: moad_cen_lat
real , DIMENSION(max_domains) :: stand_lon
integer :: flag_metgrid
integer :: flag_snow
integer :: flag_psfc
integer :: flag_sm000010
integer :: flag_sm010040
integer :: flag_sm040100
integer :: flag_sm100200
integer :: flag_st000010
integer :: flag_st010040
integer :: flag_st040100
integer :: flag_st100200
integer :: flag_slp
integer :: flag_soilhgt
integer :: flag_mf_xy
real , DIMENSION(max_domains) :: bdyfrq
character*256 , DIMENSION(max_domains) :: mminlu
integer , DIMENSION(max_domains) :: iswater
integer , DIMENSION(max_domains) :: islake
integer , DIMENSION(max_domains) :: isice
integer , DIMENSION(max_domains) :: isurban
integer , DIMENSION(max_domains) :: isoilwater
integer , DIMENSION(max_domains) :: map_proj
integer :: dfi_stage
integer , DIMENSION(max_domains) :: mp_physics_dfi
integer :: maxpatch
integer    :: last_item_in_struct







NAMELIST /physics/ lakedepth_default
NAMELIST /physics/ lake_min_elev
NAMELIST /physics/ use_lakedepth
NAMELIST /domains/ halo_debug
NAMELIST /physics/ ntracers
NAMELIST /physics/ vortex_tracker
NAMELIST /physics/ interest_rad_storm
NAMELIST /physics/ interest_rad_parent
NAMELIST /physics/ interest_rad_self
NAMELIST /physics/ interest_kids
NAMELIST /physics/ interest_self
NAMELIST /physics/ interest_storms
NAMELIST /physics/ swath_mode
NAMELIST /physics/ num_old_fixes
NAMELIST /physics/ vt4_radius
NAMELIST /physics/ vt4_weightexp
NAMELIST /physics/ vt4_pmax
NAMELIST /physics/ vt4_noise_pmax
NAMELIST /physics/ vt4_noise_pmin
NAMELIST /physics/ vt4_noise_dpdr
NAMELIST /physics/ vt4_noise_iter
NAMELIST /physics/ nomove_freq
NAMELIST /domains/ coral_x
NAMELIST /domains/ coral_y
NAMELIST /time_control/ tg_reset_stream
NAMELIST /physics/ tg_option
NAMELIST /physics/ ntornado
NAMELIST /time_control/ analysis
NAMELIST /time_control/ write_analysis
NAMELIST /time_control/ io_form_auxinput2
NAMELIST /time_control/ high_freq
NAMELIST /time_control/ high_dom
NAMELIST /physics/ swint_opt
NAMELIST /physics/ aer_type
NAMELIST /physics/ aer_aod550_opt
NAMELIST /physics/ aer_angexp_opt
NAMELIST /physics/ aer_ssa_opt
NAMELIST /physics/ aer_asy_opt
NAMELIST /physics/ aer_aod550_val
NAMELIST /physics/ aer_angexp_val
NAMELIST /physics/ aer_ssa_val
NAMELIST /physics/ aer_asy_val
NAMELIST /noah_mp/ dveg
NAMELIST /noah_mp/ opt_crs
NAMELIST /noah_mp/ opt_btr
NAMELIST /noah_mp/ opt_run
NAMELIST /noah_mp/ opt_sfc
NAMELIST /noah_mp/ opt_frz
NAMELIST /noah_mp/ opt_inf
NAMELIST /noah_mp/ opt_rad
NAMELIST /noah_mp/ opt_alb
NAMELIST /noah_mp/ opt_snf
NAMELIST /noah_mp/ opt_tbot
NAMELIST /noah_mp/ opt_stc
NAMELIST /time_control/ run_days
NAMELIST /time_control/ run_hours
NAMELIST /time_control/ run_minutes
NAMELIST /time_control/ run_seconds
NAMELIST /time_control/ start_year
NAMELIST /time_control/ start_month
NAMELIST /time_control/ start_day
NAMELIST /time_control/ start_hour
NAMELIST /time_control/ start_minute
NAMELIST /time_control/ start_second
NAMELIST /time_control/ end_year
NAMELIST /time_control/ end_month
NAMELIST /time_control/ end_day
NAMELIST /time_control/ end_hour
NAMELIST /time_control/ end_minute
NAMELIST /time_control/ end_second
NAMELIST /time_control/ interval_seconds
NAMELIST /time_control/ input_from_file
NAMELIST /time_control/ fine_input_stream
NAMELIST /time_control/ auxinput1_inname
NAMELIST /time_control/ io_form_auxinput1
NAMELIST /time_control/ override_restart_timers
NAMELIST /time_control/ auxhist1_inname
NAMELIST /time_control/ auxhist1_outname
NAMELIST /time_control/ auxhist1_interval_y
NAMELIST /time_control/ auxhist1_interval_d
NAMELIST /time_control/ auxhist1_interval_h
NAMELIST /time_control/ auxhist1_interval_m
NAMELIST /time_control/ auxhist1_interval_s
NAMELIST /time_control/ auxhist1_interval
NAMELIST /time_control/ auxhist1_begin_y
NAMELIST /time_control/ auxhist1_begin_d
NAMELIST /time_control/ auxhist1_begin_h
NAMELIST /time_control/ auxhist1_begin_m
NAMELIST /time_control/ auxhist1_begin_s
NAMELIST /time_control/ auxhist1_begin
NAMELIST /time_control/ auxhist1_end_y
NAMELIST /time_control/ auxhist1_end_d
NAMELIST /time_control/ auxhist1_end_h
NAMELIST /time_control/ auxhist1_end_m
NAMELIST /time_control/ auxhist1_end_s
NAMELIST /time_control/ auxhist1_end
NAMELIST /time_control/ io_form_auxhist1
NAMELIST /time_control/ frames_per_auxhist1
NAMELIST /time_control/ auxhist2_inname
NAMELIST /time_control/ auxhist2_outname
NAMELIST /time_control/ auxhist2_interval_y
NAMELIST /time_control/ auxhist2_interval_d
NAMELIST /time_control/ auxhist2_interval_h
NAMELIST /time_control/ auxhist2_interval_m
NAMELIST /time_control/ auxhist2_interval_s
NAMELIST /time_control/ auxhist2_interval
NAMELIST /time_control/ auxhist2_begin_y
NAMELIST /time_control/ auxhist2_begin_d
NAMELIST /time_control/ auxhist2_begin_h
NAMELIST /time_control/ auxhist2_begin_m
NAMELIST /time_control/ auxhist2_begin_s
NAMELIST /time_control/ auxhist2_begin
NAMELIST /time_control/ auxhist2_end_y
NAMELIST /time_control/ auxhist2_end_d
NAMELIST /time_control/ auxhist2_end_h
NAMELIST /time_control/ auxhist2_end_m
NAMELIST /time_control/ auxhist2_end_s
NAMELIST /time_control/ auxhist2_end
NAMELIST /time_control/ io_form_auxhist2
NAMELIST /time_control/ frames_per_auxhist2
NAMELIST /time_control/ auxhist3_inname
NAMELIST /time_control/ auxhist3_outname
NAMELIST /time_control/ auxhist3_interval_y
NAMELIST /time_control/ auxhist3_interval_d
NAMELIST /time_control/ auxhist3_interval_h
NAMELIST /time_control/ auxhist3_interval_m
NAMELIST /time_control/ auxhist3_interval_s
NAMELIST /time_control/ auxhist3_interval
NAMELIST /time_control/ auxhist3_begin_y
NAMELIST /time_control/ auxhist3_begin_d
NAMELIST /time_control/ auxhist3_begin_h
NAMELIST /time_control/ auxhist3_begin_m
NAMELIST /time_control/ auxhist3_begin_s
NAMELIST /time_control/ auxhist3_begin
NAMELIST /time_control/ auxhist3_end_y
NAMELIST /time_control/ auxhist3_end_d
NAMELIST /time_control/ auxhist3_end_h
NAMELIST /time_control/ auxhist3_end_m
NAMELIST /time_control/ auxhist3_end_s
NAMELIST /time_control/ auxhist3_end
NAMELIST /time_control/ io_form_auxhist3
NAMELIST /time_control/ frames_per_auxhist3
NAMELIST /time_control/ auxhist4_inname
NAMELIST /time_control/ auxhist4_outname
NAMELIST /time_control/ auxhist4_interval_y
NAMELIST /time_control/ auxhist4_interval_d
NAMELIST /time_control/ auxhist4_interval_h
NAMELIST /time_control/ auxhist4_interval_m
NAMELIST /time_control/ auxhist4_interval_s
NAMELIST /time_control/ auxhist4_interval
NAMELIST /time_control/ auxhist4_begin_y
NAMELIST /time_control/ auxhist4_begin_d
NAMELIST /time_control/ auxhist4_begin_h
NAMELIST /time_control/ auxhist4_begin_m
NAMELIST /time_control/ auxhist4_begin_s
NAMELIST /time_control/ auxhist4_begin
NAMELIST /time_control/ auxhist4_end_y
NAMELIST /time_control/ auxhist4_end_d
NAMELIST /time_control/ auxhist4_end_h
NAMELIST /time_control/ auxhist4_end_m
NAMELIST /time_control/ auxhist4_end_s
NAMELIST /time_control/ auxhist4_end
NAMELIST /time_control/ io_form_auxhist4
NAMELIST /time_control/ frames_per_auxhist4
NAMELIST /time_control/ auxhist5_inname
NAMELIST /time_control/ auxhist5_outname
NAMELIST /time_control/ auxhist5_interval_y
NAMELIST /time_control/ auxhist5_interval_d
NAMELIST /time_control/ auxhist5_interval_h
NAMELIST /time_control/ auxhist5_interval_m
NAMELIST /time_control/ auxhist5_interval_s
NAMELIST /time_control/ auxhist5_interval
NAMELIST /time_control/ auxhist5_begin_y
NAMELIST /time_control/ auxhist5_begin_d
NAMELIST /time_control/ auxhist5_begin_h
NAMELIST /time_control/ auxhist5_begin_m
NAMELIST /time_control/ auxhist5_begin_s
NAMELIST /time_control/ auxhist5_begin
NAMELIST /time_control/ auxhist5_end_y
NAMELIST /time_control/ auxhist5_end_d
NAMELIST /time_control/ auxhist5_end_h
NAMELIST /time_control/ auxhist5_end_m
NAMELIST /time_control/ auxhist5_end_s
NAMELIST /time_control/ auxhist5_end
NAMELIST /time_control/ io_form_auxhist5
NAMELIST /time_control/ frames_per_auxhist5
NAMELIST /time_control/ auxhist6_inname
NAMELIST /time_control/ auxhist6_outname
NAMELIST /time_control/ auxhist6_interval_y
NAMELIST /time_control/ auxhist6_interval_d
NAMELIST /time_control/ auxhist6_interval_h
NAMELIST /time_control/ auxhist6_interval_m
NAMELIST /time_control/ auxhist6_interval_s
NAMELIST /time_control/ auxhist6_interval
NAMELIST /time_control/ auxhist6_begin_y
NAMELIST /time_control/ auxhist6_begin_d
NAMELIST /time_control/ auxhist6_begin_h
NAMELIST /time_control/ auxhist6_begin_m
NAMELIST /time_control/ auxhist6_begin_s
NAMELIST /time_control/ auxhist6_begin
NAMELIST /time_control/ auxhist6_end_y
NAMELIST /time_control/ auxhist6_end_d
NAMELIST /time_control/ auxhist6_end_h
NAMELIST /time_control/ auxhist6_end_m
NAMELIST /time_control/ auxhist6_end_s
NAMELIST /time_control/ auxhist6_end
NAMELIST /time_control/ io_form_auxhist6
NAMELIST /time_control/ frames_per_auxhist6
NAMELIST /time_control/ auxhist7_inname
NAMELIST /time_control/ auxhist7_outname
NAMELIST /time_control/ auxhist7_interval_y
NAMELIST /time_control/ auxhist7_interval_d
NAMELIST /time_control/ auxhist7_interval_h
NAMELIST /time_control/ auxhist7_interval_m
NAMELIST /time_control/ auxhist7_interval_s
NAMELIST /time_control/ auxhist7_interval
NAMELIST /time_control/ auxhist7_begin_y
NAMELIST /time_control/ auxhist7_begin_d
NAMELIST /time_control/ auxhist7_begin_h
NAMELIST /time_control/ auxhist7_begin_m
NAMELIST /time_control/ auxhist7_begin_s
NAMELIST /time_control/ auxhist7_begin
NAMELIST /time_control/ auxhist7_end_y
NAMELIST /time_control/ auxhist7_end_d
NAMELIST /time_control/ auxhist7_end_h
NAMELIST /time_control/ auxhist7_end_m
NAMELIST /time_control/ auxhist7_end_s
NAMELIST /time_control/ auxhist7_end
NAMELIST /time_control/ io_form_auxhist7
NAMELIST /time_control/ frames_per_auxhist7
NAMELIST /time_control/ auxhist8_inname
NAMELIST /time_control/ auxhist8_outname
NAMELIST /time_control/ auxhist8_interval_y
NAMELIST /time_control/ auxhist8_interval_d
NAMELIST /time_control/ auxhist8_interval_h
NAMELIST /time_control/ auxhist8_interval_m
NAMELIST /time_control/ auxhist8_interval_s
NAMELIST /time_control/ auxhist8_interval
NAMELIST /time_control/ auxhist8_begin_y
NAMELIST /time_control/ auxhist8_begin_d
NAMELIST /time_control/ auxhist8_begin_h
NAMELIST /time_control/ auxhist8_begin_m
NAMELIST /time_control/ auxhist8_begin_s
NAMELIST /time_control/ auxhist8_begin
NAMELIST /time_control/ auxhist8_end_y
NAMELIST /time_control/ auxhist8_end_d
NAMELIST /time_control/ auxhist8_end_h
NAMELIST /time_control/ auxhist8_end_m
NAMELIST /time_control/ auxhist8_end_s
NAMELIST /time_control/ auxhist8_end
NAMELIST /time_control/ io_form_auxhist8
NAMELIST /time_control/ frames_per_auxhist8
NAMELIST /time_control/ auxhist9_inname
NAMELIST /time_control/ auxhist9_outname
NAMELIST /time_control/ auxhist9_interval_y
NAMELIST /time_control/ auxhist9_interval_d
NAMELIST /time_control/ auxhist9_interval_h
NAMELIST /time_control/ auxhist9_interval_m
NAMELIST /time_control/ auxhist9_interval_s
NAMELIST /time_control/ auxhist9_interval
NAMELIST /time_control/ auxhist9_begin_y
NAMELIST /time_control/ auxhist9_begin_d
NAMELIST /time_control/ auxhist9_begin_h
NAMELIST /time_control/ auxhist9_begin_m
NAMELIST /time_control/ auxhist9_begin_s
NAMELIST /time_control/ auxhist9_begin
NAMELIST /time_control/ auxhist9_end_y
NAMELIST /time_control/ auxhist9_end_d
NAMELIST /time_control/ auxhist9_end_h
NAMELIST /time_control/ auxhist9_end_m
NAMELIST /time_control/ auxhist9_end_s
NAMELIST /time_control/ auxhist9_end
NAMELIST /time_control/ io_form_auxhist9
NAMELIST /time_control/ frames_per_auxhist9
NAMELIST /time_control/ auxhist10_inname
NAMELIST /time_control/ auxhist10_outname
NAMELIST /time_control/ auxhist10_interval_y
NAMELIST /time_control/ auxhist10_interval_d
NAMELIST /time_control/ auxhist10_interval_h
NAMELIST /time_control/ auxhist10_interval_m
NAMELIST /time_control/ auxhist10_interval_s
NAMELIST /time_control/ auxhist10_interval
NAMELIST /time_control/ auxhist10_begin_y
NAMELIST /time_control/ auxhist10_begin_d
NAMELIST /time_control/ auxhist10_begin_h
NAMELIST /time_control/ auxhist10_begin_m
NAMELIST /time_control/ auxhist10_begin_s
NAMELIST /time_control/ auxhist10_begin
NAMELIST /time_control/ auxhist10_end_y
NAMELIST /time_control/ auxhist10_end_d
NAMELIST /time_control/ auxhist10_end_h
NAMELIST /time_control/ auxhist10_end_m
NAMELIST /time_control/ auxhist10_end_s
NAMELIST /time_control/ auxhist10_end
NAMELIST /time_control/ io_form_auxhist10
NAMELIST /time_control/ frames_per_auxhist10
NAMELIST /time_control/ auxhist11_inname
NAMELIST /time_control/ auxhist11_outname
NAMELIST /time_control/ auxhist11_interval_y
NAMELIST /time_control/ auxhist11_interval_d
NAMELIST /time_control/ auxhist11_interval_h
NAMELIST /time_control/ auxhist11_interval_m
NAMELIST /time_control/ auxhist11_interval_s
NAMELIST /time_control/ auxhist11_interval
NAMELIST /time_control/ auxhist11_begin_y
NAMELIST /time_control/ auxhist11_begin_d
NAMELIST /time_control/ auxhist11_begin_h
NAMELIST /time_control/ auxhist11_begin_m
NAMELIST /time_control/ auxhist11_begin_s
NAMELIST /time_control/ auxhist11_begin
NAMELIST /time_control/ auxhist11_end_y
NAMELIST /time_control/ auxhist11_end_d
NAMELIST /time_control/ auxhist11_end_h
NAMELIST /time_control/ auxhist11_end_m
NAMELIST /time_control/ auxhist11_end_s
NAMELIST /time_control/ auxhist11_end
NAMELIST /time_control/ io_form_auxhist11
NAMELIST /time_control/ frames_per_auxhist11
NAMELIST /time_control/ auxhist12_inname
NAMELIST /time_control/ auxhist12_outname
NAMELIST /time_control/ auxhist12_interval_y
NAMELIST /time_control/ auxhist12_interval_d
NAMELIST /time_control/ auxhist12_interval_h
NAMELIST /time_control/ auxhist12_interval_m
NAMELIST /time_control/ auxhist12_interval_s
NAMELIST /time_control/ auxhist12_interval
NAMELIST /time_control/ auxhist12_begin_y
NAMELIST /time_control/ auxhist12_begin_d
NAMELIST /time_control/ auxhist12_begin_h
NAMELIST /time_control/ auxhist12_begin_m
NAMELIST /time_control/ auxhist12_begin_s
NAMELIST /time_control/ auxhist12_begin
NAMELIST /time_control/ auxhist12_end_y
NAMELIST /time_control/ auxhist12_end_d
NAMELIST /time_control/ auxhist12_end_h
NAMELIST /time_control/ auxhist12_end_m
NAMELIST /time_control/ auxhist12_end_s
NAMELIST /time_control/ auxhist12_end
NAMELIST /time_control/ io_form_auxhist12
NAMELIST /time_control/ frames_per_auxhist12
NAMELIST /time_control/ auxhist13_inname
NAMELIST /time_control/ auxhist13_outname
NAMELIST /time_control/ auxhist13_interval_y
NAMELIST /time_control/ auxhist13_interval_d
NAMELIST /time_control/ auxhist13_interval_h
NAMELIST /time_control/ auxhist13_interval_m
NAMELIST /time_control/ auxhist13_interval_s
NAMELIST /time_control/ auxhist13_interval
NAMELIST /time_control/ auxhist13_begin_y
NAMELIST /time_control/ auxhist13_begin_d
NAMELIST /time_control/ auxhist13_begin_h
NAMELIST /time_control/ auxhist13_begin_m
NAMELIST /time_control/ auxhist13_begin_s
NAMELIST /time_control/ auxhist13_begin
NAMELIST /time_control/ auxhist13_end_y
NAMELIST /time_control/ auxhist13_end_d
NAMELIST /time_control/ auxhist13_end_h
NAMELIST /time_control/ auxhist13_end_m
NAMELIST /time_control/ auxhist13_end_s
NAMELIST /time_control/ auxhist13_end
NAMELIST /time_control/ io_form_auxhist13
NAMELIST /time_control/ frames_per_auxhist13
NAMELIST /time_control/ auxhist14_inname
NAMELIST /time_control/ auxhist14_outname
NAMELIST /time_control/ auxhist14_interval_y
NAMELIST /time_control/ auxhist14_interval_d
NAMELIST /time_control/ auxhist14_interval_h
NAMELIST /time_control/ auxhist14_interval_m
NAMELIST /time_control/ auxhist14_interval_s
NAMELIST /time_control/ auxhist14_interval
NAMELIST /time_control/ auxhist14_begin_y
NAMELIST /time_control/ auxhist14_begin_d
NAMELIST /time_control/ auxhist14_begin_h
NAMELIST /time_control/ auxhist14_begin_m
NAMELIST /time_control/ auxhist14_begin_s
NAMELIST /time_control/ auxhist14_begin
NAMELIST /time_control/ auxhist14_end_y
NAMELIST /time_control/ auxhist14_end_d
NAMELIST /time_control/ auxhist14_end_h
NAMELIST /time_control/ auxhist14_end_m
NAMELIST /time_control/ auxhist14_end_s
NAMELIST /time_control/ auxhist14_end
NAMELIST /time_control/ io_form_auxhist14
NAMELIST /time_control/ frames_per_auxhist14
NAMELIST /time_control/ auxhist15_inname
NAMELIST /time_control/ auxhist15_outname
NAMELIST /time_control/ auxhist15_interval_y
NAMELIST /time_control/ auxhist15_interval_d
NAMELIST /time_control/ auxhist15_interval_h
NAMELIST /time_control/ auxhist15_interval_m
NAMELIST /time_control/ auxhist15_interval_s
NAMELIST /time_control/ auxhist15_interval
NAMELIST /time_control/ auxhist15_begin_y
NAMELIST /time_control/ auxhist15_begin_d
NAMELIST /time_control/ auxhist15_begin_h
NAMELIST /time_control/ auxhist15_begin_m
NAMELIST /time_control/ auxhist15_begin_s
NAMELIST /time_control/ auxhist15_begin
NAMELIST /time_control/ auxhist15_end_y
NAMELIST /time_control/ auxhist15_end_d
NAMELIST /time_control/ auxhist15_end_h
NAMELIST /time_control/ auxhist15_end_m
NAMELIST /time_control/ auxhist15_end_s
NAMELIST /time_control/ auxhist15_end
NAMELIST /time_control/ io_form_auxhist15
NAMELIST /time_control/ frames_per_auxhist15
NAMELIST /time_control/ auxhist16_inname
NAMELIST /time_control/ auxhist16_outname
NAMELIST /time_control/ auxhist16_interval_y
NAMELIST /time_control/ auxhist16_interval_d
NAMELIST /time_control/ auxhist16_interval_h
NAMELIST /time_control/ auxhist16_interval_m
NAMELIST /time_control/ auxhist16_interval_s
NAMELIST /time_control/ auxhist16_interval
NAMELIST /time_control/ auxhist16_begin_y
NAMELIST /time_control/ auxhist16_begin_d
NAMELIST /time_control/ auxhist16_begin_h
NAMELIST /time_control/ auxhist16_begin_m
NAMELIST /time_control/ auxhist16_begin_s
NAMELIST /time_control/ auxhist16_begin
NAMELIST /time_control/ auxhist16_end_y
NAMELIST /time_control/ auxhist16_end_d
NAMELIST /time_control/ auxhist16_end_h
NAMELIST /time_control/ auxhist16_end_m
NAMELIST /time_control/ auxhist16_end_s
NAMELIST /time_control/ auxhist16_end
NAMELIST /time_control/ io_form_auxhist16
NAMELIST /time_control/ frames_per_auxhist16
NAMELIST /time_control/ auxhist17_inname
NAMELIST /time_control/ auxhist17_outname
NAMELIST /time_control/ auxhist17_interval_y
NAMELIST /time_control/ auxhist17_interval_d
NAMELIST /time_control/ auxhist17_interval_h
NAMELIST /time_control/ auxhist17_interval_m
NAMELIST /time_control/ auxhist17_interval_s
NAMELIST /time_control/ auxhist17_interval
NAMELIST /time_control/ auxhist17_begin_y
NAMELIST /time_control/ auxhist17_begin_d
NAMELIST /time_control/ auxhist17_begin_h
NAMELIST /time_control/ auxhist17_begin_m
NAMELIST /time_control/ auxhist17_begin_s
NAMELIST /time_control/ auxhist17_begin
NAMELIST /time_control/ auxhist17_end_y
NAMELIST /time_control/ auxhist17_end_d
NAMELIST /time_control/ auxhist17_end_h
NAMELIST /time_control/ auxhist17_end_m
NAMELIST /time_control/ auxhist17_end_s
NAMELIST /time_control/ auxhist17_end
NAMELIST /time_control/ io_form_auxhist17
NAMELIST /time_control/ frames_per_auxhist17
NAMELIST /time_control/ auxhist18_inname
NAMELIST /time_control/ auxhist18_outname
NAMELIST /time_control/ auxhist18_interval_y
NAMELIST /time_control/ auxhist18_interval_d
NAMELIST /time_control/ auxhist18_interval_h
NAMELIST /time_control/ auxhist18_interval_m
NAMELIST /time_control/ auxhist18_interval_s
NAMELIST /time_control/ auxhist18_interval
NAMELIST /time_control/ auxhist18_begin_y
NAMELIST /time_control/ auxhist18_begin_d
NAMELIST /time_control/ auxhist18_begin_h
NAMELIST /time_control/ auxhist18_begin_m
NAMELIST /time_control/ auxhist18_begin_s
NAMELIST /time_control/ auxhist18_begin
NAMELIST /time_control/ auxhist18_end_y
NAMELIST /time_control/ auxhist18_end_d
NAMELIST /time_control/ auxhist18_end_h
NAMELIST /time_control/ auxhist18_end_m
NAMELIST /time_control/ auxhist18_end_s
NAMELIST /time_control/ auxhist18_end
NAMELIST /time_control/ io_form_auxhist18
NAMELIST /time_control/ frames_per_auxhist18
NAMELIST /time_control/ auxhist19_inname
NAMELIST /time_control/ auxhist19_outname
NAMELIST /time_control/ auxhist19_interval_y
NAMELIST /time_control/ auxhist19_interval_d
NAMELIST /time_control/ auxhist19_interval_h
NAMELIST /time_control/ auxhist19_interval_m
NAMELIST /time_control/ auxhist19_interval_s
NAMELIST /time_control/ auxhist19_interval
NAMELIST /time_control/ auxhist19_begin_y
NAMELIST /time_control/ auxhist19_begin_d
NAMELIST /time_control/ auxhist19_begin_h
NAMELIST /time_control/ auxhist19_begin_m
NAMELIST /time_control/ auxhist19_begin_s
NAMELIST /time_control/ auxhist19_begin
NAMELIST /time_control/ auxhist19_end_y
NAMELIST /time_control/ auxhist19_end_d
NAMELIST /time_control/ auxhist19_end_h
NAMELIST /time_control/ auxhist19_end_m
NAMELIST /time_control/ auxhist19_end_s
NAMELIST /time_control/ auxhist19_end
NAMELIST /time_control/ io_form_auxhist19
NAMELIST /time_control/ frames_per_auxhist19
NAMELIST /time_control/ auxhist20_inname
NAMELIST /time_control/ auxhist20_outname
NAMELIST /time_control/ auxhist20_interval_y
NAMELIST /time_control/ auxhist20_interval_d
NAMELIST /time_control/ auxhist20_interval_h
NAMELIST /time_control/ auxhist20_interval_m
NAMELIST /time_control/ auxhist20_interval_s
NAMELIST /time_control/ auxhist20_interval
NAMELIST /time_control/ auxhist20_begin_y
NAMELIST /time_control/ auxhist20_begin_d
NAMELIST /time_control/ auxhist20_begin_h
NAMELIST /time_control/ auxhist20_begin_m
NAMELIST /time_control/ auxhist20_begin_s
NAMELIST /time_control/ auxhist20_begin
NAMELIST /time_control/ auxhist20_end_y
NAMELIST /time_control/ auxhist20_end_d
NAMELIST /time_control/ auxhist20_end_h
NAMELIST /time_control/ auxhist20_end_m
NAMELIST /time_control/ auxhist20_end_s
NAMELIST /time_control/ auxhist20_end
NAMELIST /time_control/ io_form_auxhist20
NAMELIST /time_control/ frames_per_auxhist20
NAMELIST /time_control/ auxhist21_inname
NAMELIST /time_control/ auxhist21_outname
NAMELIST /time_control/ auxhist21_interval_y
NAMELIST /time_control/ auxhist21_interval_d
NAMELIST /time_control/ auxhist21_interval_h
NAMELIST /time_control/ auxhist21_interval_m
NAMELIST /time_control/ auxhist21_interval_s
NAMELIST /time_control/ auxhist21_interval
NAMELIST /time_control/ auxhist21_begin_y
NAMELIST /time_control/ auxhist21_begin_d
NAMELIST /time_control/ auxhist21_begin_h
NAMELIST /time_control/ auxhist21_begin_m
NAMELIST /time_control/ auxhist21_begin_s
NAMELIST /time_control/ auxhist21_begin
NAMELIST /time_control/ auxhist21_end_y
NAMELIST /time_control/ auxhist21_end_d
NAMELIST /time_control/ auxhist21_end_h
NAMELIST /time_control/ auxhist21_end_m
NAMELIST /time_control/ auxhist21_end_s
NAMELIST /time_control/ auxhist21_end
NAMELIST /time_control/ io_form_auxhist21
NAMELIST /time_control/ frames_per_auxhist21
NAMELIST /time_control/ auxhist22_inname
NAMELIST /time_control/ auxhist22_outname
NAMELIST /time_control/ auxhist22_interval_y
NAMELIST /time_control/ auxhist22_interval_d
NAMELIST /time_control/ auxhist22_interval_h
NAMELIST /time_control/ auxhist22_interval_m
NAMELIST /time_control/ auxhist22_interval_s
NAMELIST /time_control/ auxhist22_interval
NAMELIST /time_control/ auxhist22_begin_y
NAMELIST /time_control/ auxhist22_begin_d
NAMELIST /time_control/ auxhist22_begin_h
NAMELIST /time_control/ auxhist22_begin_m
NAMELIST /time_control/ auxhist22_begin_s
NAMELIST /time_control/ auxhist22_begin
NAMELIST /time_control/ auxhist22_end_y
NAMELIST /time_control/ auxhist22_end_d
NAMELIST /time_control/ auxhist22_end_h
NAMELIST /time_control/ auxhist22_end_m
NAMELIST /time_control/ auxhist22_end_s
NAMELIST /time_control/ auxhist22_end
NAMELIST /time_control/ io_form_auxhist22
NAMELIST /time_control/ frames_per_auxhist22
NAMELIST /time_control/ auxhist23_inname
NAMELIST /time_control/ auxhist23_outname
NAMELIST /time_control/ auxhist23_interval_y
NAMELIST /time_control/ auxhist23_interval_d
NAMELIST /time_control/ auxhist23_interval_h
NAMELIST /time_control/ auxhist23_interval_m
NAMELIST /time_control/ auxhist23_interval_s
NAMELIST /time_control/ auxhist23_interval
NAMELIST /time_control/ auxhist23_begin_y
NAMELIST /time_control/ auxhist23_begin_d
NAMELIST /time_control/ auxhist23_begin_h
NAMELIST /time_control/ auxhist23_begin_m
NAMELIST /time_control/ auxhist23_begin_s
NAMELIST /time_control/ auxhist23_begin
NAMELIST /time_control/ auxhist23_end_y
NAMELIST /time_control/ auxhist23_end_d
NAMELIST /time_control/ auxhist23_end_h
NAMELIST /time_control/ auxhist23_end_m
NAMELIST /time_control/ auxhist23_end_s
NAMELIST /time_control/ auxhist23_end
NAMELIST /time_control/ io_form_auxhist23
NAMELIST /time_control/ frames_per_auxhist23
NAMELIST /time_control/ auxhist24_inname
NAMELIST /time_control/ auxhist24_outname
NAMELIST /time_control/ auxhist24_interval_y
NAMELIST /time_control/ auxhist24_interval_d
NAMELIST /time_control/ auxhist24_interval_h
NAMELIST /time_control/ auxhist24_interval_m
NAMELIST /time_control/ auxhist24_interval_s
NAMELIST /time_control/ auxhist24_interval
NAMELIST /time_control/ auxhist24_begin_y
NAMELIST /time_control/ auxhist24_begin_d
NAMELIST /time_control/ auxhist24_begin_h
NAMELIST /time_control/ auxhist24_begin_m
NAMELIST /time_control/ auxhist24_begin_s
NAMELIST /time_control/ auxhist24_begin
NAMELIST /time_control/ auxhist24_end_y
NAMELIST /time_control/ auxhist24_end_d
NAMELIST /time_control/ auxhist24_end_h
NAMELIST /time_control/ auxhist24_end_m
NAMELIST /time_control/ auxhist24_end_s
NAMELIST /time_control/ auxhist24_end
NAMELIST /time_control/ io_form_auxhist24
NAMELIST /time_control/ frames_per_auxhist24
NAMELIST /time_control/ auxinput1_outname
NAMELIST /time_control/ auxinput1_interval_y
NAMELIST /time_control/ auxinput1_interval_d
NAMELIST /time_control/ auxinput1_interval_h
NAMELIST /time_control/ auxinput1_interval_m
NAMELIST /time_control/ auxinput1_interval_s
NAMELIST /time_control/ auxinput1_interval
NAMELIST /time_control/ auxinput1_begin_y
NAMELIST /time_control/ auxinput1_begin_d
NAMELIST /time_control/ auxinput1_begin_h
NAMELIST /time_control/ auxinput1_begin_m
NAMELIST /time_control/ auxinput1_begin_s
NAMELIST /time_control/ auxinput1_begin
NAMELIST /time_control/ auxinput1_end_y
NAMELIST /time_control/ auxinput1_end_d
NAMELIST /time_control/ auxinput1_end_h
NAMELIST /time_control/ auxinput1_end_m
NAMELIST /time_control/ auxinput1_end_s
NAMELIST /time_control/ auxinput1_end
NAMELIST /time_control/ frames_per_auxinput1
NAMELIST /time_control/ auxinput2_inname
NAMELIST /time_control/ auxinput2_outname
NAMELIST /time_control/ auxinput2_interval_y
NAMELIST /time_control/ auxinput2_interval_d
NAMELIST /time_control/ auxinput2_interval_h
NAMELIST /time_control/ auxinput2_interval_m
NAMELIST /time_control/ auxinput2_interval_s
NAMELIST /time_control/ auxinput2_interval
NAMELIST /time_control/ auxinput2_begin_y
NAMELIST /time_control/ auxinput2_begin_d
NAMELIST /time_control/ auxinput2_begin_h
NAMELIST /time_control/ auxinput2_begin_m
NAMELIST /time_control/ auxinput2_begin_s
NAMELIST /time_control/ auxinput2_begin
NAMELIST /time_control/ auxinput2_end_y
NAMELIST /time_control/ auxinput2_end_d
NAMELIST /time_control/ auxinput2_end_h
NAMELIST /time_control/ auxinput2_end_m
NAMELIST /time_control/ auxinput2_end_s
NAMELIST /time_control/ auxinput2_end
NAMELIST /time_control/ frames_per_auxinput2
NAMELIST /time_control/ auxinput3_inname
NAMELIST /time_control/ auxinput3_outname
NAMELIST /time_control/ auxinput3_interval_y
NAMELIST /time_control/ auxinput3_interval_d
NAMELIST /time_control/ auxinput3_interval_h
NAMELIST /time_control/ auxinput3_interval_m
NAMELIST /time_control/ auxinput3_interval_s
NAMELIST /time_control/ auxinput3_interval
NAMELIST /time_control/ auxinput3_begin_y
NAMELIST /time_control/ auxinput3_begin_d
NAMELIST /time_control/ auxinput3_begin_h
NAMELIST /time_control/ auxinput3_begin_m
NAMELIST /time_control/ auxinput3_begin_s
NAMELIST /time_control/ auxinput3_begin
NAMELIST /time_control/ auxinput3_end_y
NAMELIST /time_control/ auxinput3_end_d
NAMELIST /time_control/ auxinput3_end_h
NAMELIST /time_control/ auxinput3_end_m
NAMELIST /time_control/ auxinput3_end_s
NAMELIST /time_control/ auxinput3_end
NAMELIST /time_control/ io_form_auxinput3
NAMELIST /time_control/ frames_per_auxinput3
NAMELIST /time_control/ auxinput4_inname
NAMELIST /time_control/ auxinput4_outname
NAMELIST /time_control/ auxinput4_interval_y
NAMELIST /time_control/ auxinput4_interval_d
NAMELIST /time_control/ auxinput4_interval_h
NAMELIST /time_control/ auxinput4_interval_m
NAMELIST /time_control/ auxinput4_interval_s
NAMELIST /time_control/ auxinput4_interval
NAMELIST /time_control/ auxinput4_begin_y
NAMELIST /time_control/ auxinput4_begin_d
NAMELIST /time_control/ auxinput4_begin_h
NAMELIST /time_control/ auxinput4_begin_m
NAMELIST /time_control/ auxinput4_begin_s
NAMELIST /time_control/ auxinput4_begin
NAMELIST /time_control/ auxinput4_end_y
NAMELIST /time_control/ auxinput4_end_d
NAMELIST /time_control/ auxinput4_end_h
NAMELIST /time_control/ auxinput4_end_m
NAMELIST /time_control/ auxinput4_end_s
NAMELIST /time_control/ auxinput4_end
NAMELIST /time_control/ io_form_auxinput4
NAMELIST /time_control/ frames_per_auxinput4
NAMELIST /time_control/ auxinput5_inname
NAMELIST /time_control/ auxinput5_outname
NAMELIST /time_control/ auxinput5_interval_y
NAMELIST /time_control/ auxinput5_interval_d
NAMELIST /time_control/ auxinput5_interval_h
NAMELIST /time_control/ auxinput5_interval_m
NAMELIST /time_control/ auxinput5_interval_s
NAMELIST /time_control/ auxinput5_interval
NAMELIST /time_control/ auxinput5_begin_y
NAMELIST /time_control/ auxinput5_begin_d
NAMELIST /time_control/ auxinput5_begin_h
NAMELIST /time_control/ auxinput5_begin_m
NAMELIST /time_control/ auxinput5_begin_s
NAMELIST /time_control/ auxinput5_begin
NAMELIST /time_control/ auxinput5_end_y
NAMELIST /time_control/ auxinput5_end_d
NAMELIST /time_control/ auxinput5_end_h
NAMELIST /time_control/ auxinput5_end_m
NAMELIST /time_control/ auxinput5_end_s
NAMELIST /time_control/ auxinput5_end
NAMELIST /time_control/ io_form_auxinput5
NAMELIST /time_control/ frames_per_auxinput5
NAMELIST /time_control/ auxinput6_inname
NAMELIST /time_control/ auxinput6_outname
NAMELIST /time_control/ auxinput6_interval_y
NAMELIST /time_control/ auxinput6_interval_d
NAMELIST /time_control/ auxinput6_interval_h
NAMELIST /time_control/ auxinput6_interval_m
NAMELIST /time_control/ auxinput6_interval_s
NAMELIST /time_control/ auxinput6_interval
NAMELIST /time_control/ auxinput6_begin_y
NAMELIST /time_control/ auxinput6_begin_d
NAMELIST /time_control/ auxinput6_begin_h
NAMELIST /time_control/ auxinput6_begin_m
NAMELIST /time_control/ auxinput6_begin_s
NAMELIST /time_control/ auxinput6_begin
NAMELIST /time_control/ auxinput6_end_y
NAMELIST /time_control/ auxinput6_end_d
NAMELIST /time_control/ auxinput6_end_h
NAMELIST /time_control/ auxinput6_end_m
NAMELIST /time_control/ auxinput6_end_s
NAMELIST /time_control/ auxinput6_end
NAMELIST /time_control/ io_form_auxinput6
NAMELIST /time_control/ frames_per_auxinput6
NAMELIST /time_control/ auxinput7_inname
NAMELIST /time_control/ auxinput7_outname
NAMELIST /time_control/ auxinput7_interval_y
NAMELIST /time_control/ auxinput7_interval_d
NAMELIST /time_control/ auxinput7_interval_h
NAMELIST /time_control/ auxinput7_interval_m
NAMELIST /time_control/ auxinput7_interval_s
NAMELIST /time_control/ auxinput7_interval
NAMELIST /time_control/ auxinput7_begin_y
NAMELIST /time_control/ auxinput7_begin_d
NAMELIST /time_control/ auxinput7_begin_h
NAMELIST /time_control/ auxinput7_begin_m
NAMELIST /time_control/ auxinput7_begin_s
NAMELIST /time_control/ auxinput7_begin
NAMELIST /time_control/ auxinput7_end_y
NAMELIST /time_control/ auxinput7_end_d
NAMELIST /time_control/ auxinput7_end_h
NAMELIST /time_control/ auxinput7_end_m
NAMELIST /time_control/ auxinput7_end_s
NAMELIST /time_control/ auxinput7_end
NAMELIST /time_control/ io_form_auxinput7
NAMELIST /time_control/ frames_per_auxinput7
NAMELIST /time_control/ auxinput8_inname
NAMELIST /time_control/ auxinput8_outname
NAMELIST /time_control/ auxinput8_interval_y
NAMELIST /time_control/ auxinput8_interval_d
NAMELIST /time_control/ auxinput8_interval_h
NAMELIST /time_control/ auxinput8_interval_m
NAMELIST /time_control/ auxinput8_interval_s
NAMELIST /time_control/ auxinput8_interval
NAMELIST /time_control/ auxinput8_begin_y
NAMELIST /time_control/ auxinput8_begin_d
NAMELIST /time_control/ auxinput8_begin_h
NAMELIST /time_control/ auxinput8_begin_m
NAMELIST /time_control/ auxinput8_begin_s
NAMELIST /time_control/ auxinput8_begin
NAMELIST /time_control/ auxinput8_end_y
NAMELIST /time_control/ auxinput8_end_d
NAMELIST /time_control/ auxinput8_end_h
NAMELIST /time_control/ auxinput8_end_m
NAMELIST /time_control/ auxinput8_end_s
NAMELIST /time_control/ auxinput8_end
NAMELIST /time_control/ io_form_auxinput8
NAMELIST /time_control/ frames_per_auxinput8
NAMELIST /time_control/ auxinput9_inname
NAMELIST /time_control/ auxinput9_outname
NAMELIST /time_control/ auxinput9_interval_y
NAMELIST /time_control/ auxinput9_interval_d
NAMELIST /time_control/ auxinput9_interval_h
NAMELIST /time_control/ auxinput9_interval_m
NAMELIST /time_control/ auxinput9_interval_s
NAMELIST /time_control/ auxinput9_interval
NAMELIST /time_control/ auxinput9_begin_y
NAMELIST /time_control/ auxinput9_begin_d
NAMELIST /time_control/ auxinput9_begin_h
NAMELIST /time_control/ auxinput9_begin_m
NAMELIST /time_control/ auxinput9_begin_s
NAMELIST /time_control/ auxinput9_begin
NAMELIST /time_control/ auxinput9_end_y
NAMELIST /time_control/ auxinput9_end_d
NAMELIST /time_control/ auxinput9_end_h
NAMELIST /time_control/ auxinput9_end_m
NAMELIST /time_control/ auxinput9_end_s
NAMELIST /time_control/ auxinput9_end
NAMELIST /time_control/ io_form_auxinput9
NAMELIST /time_control/ frames_per_auxinput9
NAMELIST /time_control/ auxinput10_inname
NAMELIST /time_control/ auxinput10_outname
NAMELIST /time_control/ auxinput10_interval_y
NAMELIST /time_control/ auxinput10_interval_d
NAMELIST /time_control/ auxinput10_interval_h
NAMELIST /time_control/ auxinput10_interval_m
NAMELIST /time_control/ auxinput10_interval_s
NAMELIST /time_control/ auxinput10_interval
NAMELIST /time_control/ auxinput10_begin_y
NAMELIST /time_control/ auxinput10_begin_d
NAMELIST /time_control/ auxinput10_begin_h
NAMELIST /time_control/ auxinput10_begin_m
NAMELIST /time_control/ auxinput10_begin_s
NAMELIST /time_control/ auxinput10_begin
NAMELIST /time_control/ auxinput10_end_y
NAMELIST /time_control/ auxinput10_end_d
NAMELIST /time_control/ auxinput10_end_h
NAMELIST /time_control/ auxinput10_end_m
NAMELIST /time_control/ auxinput10_end_s
NAMELIST /time_control/ auxinput10_end
NAMELIST /time_control/ io_form_auxinput10
NAMELIST /time_control/ frames_per_auxinput10
NAMELIST /time_control/ auxinput11_inname
NAMELIST /time_control/ auxinput11_outname
NAMELIST /time_control/ auxinput11_interval_y
NAMELIST /time_control/ auxinput11_interval_d
NAMELIST /time_control/ auxinput11_interval_h
NAMELIST /time_control/ auxinput11_interval_m
NAMELIST /time_control/ auxinput11_interval_s
NAMELIST /time_control/ auxinput11_interval
NAMELIST /time_control/ auxinput11_begin_y
NAMELIST /time_control/ auxinput11_begin_d
NAMELIST /time_control/ auxinput11_begin_h
NAMELIST /time_control/ auxinput11_begin_m
NAMELIST /time_control/ auxinput11_begin_s
NAMELIST /time_control/ auxinput11_begin
NAMELIST /time_control/ auxinput11_end_y
NAMELIST /time_control/ auxinput11_end_d
NAMELIST /time_control/ auxinput11_end_h
NAMELIST /time_control/ auxinput11_end_m
NAMELIST /time_control/ auxinput11_end_s
NAMELIST /time_control/ auxinput11_end
NAMELIST /time_control/ io_form_auxinput11
NAMELIST /time_control/ frames_per_auxinput11
NAMELIST /time_control/ auxinput12_inname
NAMELIST /time_control/ auxinput12_outname
NAMELIST /time_control/ auxinput12_interval_y
NAMELIST /time_control/ auxinput12_interval_d
NAMELIST /time_control/ auxinput12_interval_h
NAMELIST /time_control/ auxinput12_interval_m
NAMELIST /time_control/ auxinput12_interval_s
NAMELIST /time_control/ auxinput12_interval
NAMELIST /time_control/ auxinput12_begin_y
NAMELIST /time_control/ auxinput12_begin_d
NAMELIST /time_control/ auxinput12_begin_h
NAMELIST /time_control/ auxinput12_begin_m
NAMELIST /time_control/ auxinput12_begin_s
NAMELIST /time_control/ auxinput12_begin
NAMELIST /time_control/ auxinput12_end_y
NAMELIST /time_control/ auxinput12_end_d
NAMELIST /time_control/ auxinput12_end_h
NAMELIST /time_control/ auxinput12_end_m
NAMELIST /time_control/ auxinput12_end_s
NAMELIST /time_control/ auxinput12_end
NAMELIST /time_control/ io_form_auxinput12
NAMELIST /time_control/ frames_per_auxinput12
NAMELIST /time_control/ auxinput13_inname
NAMELIST /time_control/ auxinput13_outname
NAMELIST /time_control/ auxinput13_interval_y
NAMELIST /time_control/ auxinput13_interval_d
NAMELIST /time_control/ auxinput13_interval_h
NAMELIST /time_control/ auxinput13_interval_m
NAMELIST /time_control/ auxinput13_interval_s
NAMELIST /time_control/ auxinput13_interval
NAMELIST /time_control/ auxinput13_begin_y
NAMELIST /time_control/ auxinput13_begin_d
NAMELIST /time_control/ auxinput13_begin_h
NAMELIST /time_control/ auxinput13_begin_m
NAMELIST /time_control/ auxinput13_begin_s
NAMELIST /time_control/ auxinput13_begin
NAMELIST /time_control/ auxinput13_end_y
NAMELIST /time_control/ auxinput13_end_d
NAMELIST /time_control/ auxinput13_end_h
NAMELIST /time_control/ auxinput13_end_m
NAMELIST /time_control/ auxinput13_end_s
NAMELIST /time_control/ auxinput13_end
NAMELIST /time_control/ io_form_auxinput13
NAMELIST /time_control/ frames_per_auxinput13
NAMELIST /time_control/ auxinput14_inname
NAMELIST /time_control/ auxinput14_outname
NAMELIST /time_control/ auxinput14_interval_y
NAMELIST /time_control/ auxinput14_interval_d
NAMELIST /time_control/ auxinput14_interval_h
NAMELIST /time_control/ auxinput14_interval_m
NAMELIST /time_control/ auxinput14_interval_s
NAMELIST /time_control/ auxinput14_interval
NAMELIST /time_control/ auxinput14_begin_y
NAMELIST /time_control/ auxinput14_begin_d
NAMELIST /time_control/ auxinput14_begin_h
NAMELIST /time_control/ auxinput14_begin_m
NAMELIST /time_control/ auxinput14_begin_s
NAMELIST /time_control/ auxinput14_begin
NAMELIST /time_control/ auxinput14_end_y
NAMELIST /time_control/ auxinput14_end_d
NAMELIST /time_control/ auxinput14_end_h
NAMELIST /time_control/ auxinput14_end_m
NAMELIST /time_control/ auxinput14_end_s
NAMELIST /time_control/ auxinput14_end
NAMELIST /time_control/ io_form_auxinput14
NAMELIST /time_control/ frames_per_auxinput14
NAMELIST /time_control/ auxinput15_inname
NAMELIST /time_control/ auxinput15_outname
NAMELIST /time_control/ auxinput15_interval_y
NAMELIST /time_control/ auxinput15_interval_d
NAMELIST /time_control/ auxinput15_interval_h
NAMELIST /time_control/ auxinput15_interval_m
NAMELIST /time_control/ auxinput15_interval_s
NAMELIST /time_control/ auxinput15_interval
NAMELIST /time_control/ auxinput15_begin_y
NAMELIST /time_control/ auxinput15_begin_d
NAMELIST /time_control/ auxinput15_begin_h
NAMELIST /time_control/ auxinput15_begin_m
NAMELIST /time_control/ auxinput15_begin_s
NAMELIST /time_control/ auxinput15_begin
NAMELIST /time_control/ auxinput15_end_y
NAMELIST /time_control/ auxinput15_end_d
NAMELIST /time_control/ auxinput15_end_h
NAMELIST /time_control/ auxinput15_end_m
NAMELIST /time_control/ auxinput15_end_s
NAMELIST /time_control/ auxinput15_end
NAMELIST /time_control/ io_form_auxinput15
NAMELIST /time_control/ frames_per_auxinput15
NAMELIST /time_control/ auxinput16_inname
NAMELIST /time_control/ auxinput16_outname
NAMELIST /time_control/ auxinput16_interval_y
NAMELIST /time_control/ auxinput16_interval_d
NAMELIST /time_control/ auxinput16_interval_h
NAMELIST /time_control/ auxinput16_interval_m
NAMELIST /time_control/ auxinput16_interval_s
NAMELIST /time_control/ auxinput16_interval
NAMELIST /time_control/ auxinput16_begin_y
NAMELIST /time_control/ auxinput16_begin_d
NAMELIST /time_control/ auxinput16_begin_h
NAMELIST /time_control/ auxinput16_begin_m
NAMELIST /time_control/ auxinput16_begin_s
NAMELIST /time_control/ auxinput16_begin
NAMELIST /time_control/ auxinput16_end_y
NAMELIST /time_control/ auxinput16_end_d
NAMELIST /time_control/ auxinput16_end_h
NAMELIST /time_control/ auxinput16_end_m
NAMELIST /time_control/ auxinput16_end_s
NAMELIST /time_control/ auxinput16_end
NAMELIST /time_control/ io_form_auxinput16
NAMELIST /time_control/ frames_per_auxinput16
NAMELIST /time_control/ auxinput17_inname
NAMELIST /time_control/ auxinput17_outname
NAMELIST /time_control/ auxinput17_interval_y
NAMELIST /time_control/ auxinput17_interval_d
NAMELIST /time_control/ auxinput17_interval_h
NAMELIST /time_control/ auxinput17_interval_m
NAMELIST /time_control/ auxinput17_interval_s
NAMELIST /time_control/ auxinput17_interval
NAMELIST /time_control/ auxinput17_begin_y
NAMELIST /time_control/ auxinput17_begin_d
NAMELIST /time_control/ auxinput17_begin_h
NAMELIST /time_control/ auxinput17_begin_m
NAMELIST /time_control/ auxinput17_begin_s
NAMELIST /time_control/ auxinput17_begin
NAMELIST /time_control/ auxinput17_end_y
NAMELIST /time_control/ auxinput17_end_d
NAMELIST /time_control/ auxinput17_end_h
NAMELIST /time_control/ auxinput17_end_m
NAMELIST /time_control/ auxinput17_end_s
NAMELIST /time_control/ auxinput17_end
NAMELIST /time_control/ io_form_auxinput17
NAMELIST /time_control/ frames_per_auxinput17
NAMELIST /time_control/ auxinput18_inname
NAMELIST /time_control/ auxinput18_outname
NAMELIST /time_control/ auxinput18_interval_y
NAMELIST /time_control/ auxinput18_interval_d
NAMELIST /time_control/ auxinput18_interval_h
NAMELIST /time_control/ auxinput18_interval_m
NAMELIST /time_control/ auxinput18_interval_s
NAMELIST /time_control/ auxinput18_interval
NAMELIST /time_control/ auxinput18_begin_y
NAMELIST /time_control/ auxinput18_begin_d
NAMELIST /time_control/ auxinput18_begin_h
NAMELIST /time_control/ auxinput18_begin_m
NAMELIST /time_control/ auxinput18_begin_s
NAMELIST /time_control/ auxinput18_begin
NAMELIST /time_control/ auxinput18_end_y
NAMELIST /time_control/ auxinput18_end_d
NAMELIST /time_control/ auxinput18_end_h
NAMELIST /time_control/ auxinput18_end_m
NAMELIST /time_control/ auxinput18_end_s
NAMELIST /time_control/ auxinput18_end
NAMELIST /time_control/ io_form_auxinput18
NAMELIST /time_control/ frames_per_auxinput18
NAMELIST /time_control/ auxinput19_inname
NAMELIST /time_control/ auxinput19_outname
NAMELIST /time_control/ auxinput19_interval_y
NAMELIST /time_control/ auxinput19_interval_d
NAMELIST /time_control/ auxinput19_interval_h
NAMELIST /time_control/ auxinput19_interval_m
NAMELIST /time_control/ auxinput19_interval_s
NAMELIST /time_control/ auxinput19_interval
NAMELIST /time_control/ auxinput19_begin_y
NAMELIST /time_control/ auxinput19_begin_d
NAMELIST /time_control/ auxinput19_begin_h
NAMELIST /time_control/ auxinput19_begin_m
NAMELIST /time_control/ auxinput19_begin_s
NAMELIST /time_control/ auxinput19_begin
NAMELIST /time_control/ auxinput19_end_y
NAMELIST /time_control/ auxinput19_end_d
NAMELIST /time_control/ auxinput19_end_h
NAMELIST /time_control/ auxinput19_end_m
NAMELIST /time_control/ auxinput19_end_s
NAMELIST /time_control/ auxinput19_end
NAMELIST /time_control/ io_form_auxinput19
NAMELIST /time_control/ frames_per_auxinput19
NAMELIST /time_control/ auxinput20_inname
NAMELIST /time_control/ auxinput20_outname
NAMELIST /time_control/ auxinput20_interval_y
NAMELIST /time_control/ auxinput20_interval_d
NAMELIST /time_control/ auxinput20_interval_h
NAMELIST /time_control/ auxinput20_interval_m
NAMELIST /time_control/ auxinput20_interval_s
NAMELIST /time_control/ auxinput20_interval
NAMELIST /time_control/ auxinput20_begin_y
NAMELIST /time_control/ auxinput20_begin_d
NAMELIST /time_control/ auxinput20_begin_h
NAMELIST /time_control/ auxinput20_begin_m
NAMELIST /time_control/ auxinput20_begin_s
NAMELIST /time_control/ auxinput20_begin
NAMELIST /time_control/ auxinput20_end_y
NAMELIST /time_control/ auxinput20_end_d
NAMELIST /time_control/ auxinput20_end_h
NAMELIST /time_control/ auxinput20_end_m
NAMELIST /time_control/ auxinput20_end_s
NAMELIST /time_control/ auxinput20_end
NAMELIST /time_control/ io_form_auxinput20
NAMELIST /time_control/ frames_per_auxinput20
NAMELIST /time_control/ auxinput21_inname
NAMELIST /time_control/ auxinput21_outname
NAMELIST /time_control/ auxinput21_interval_y
NAMELIST /time_control/ auxinput21_interval_d
NAMELIST /time_control/ auxinput21_interval_h
NAMELIST /time_control/ auxinput21_interval_m
NAMELIST /time_control/ auxinput21_interval_s
NAMELIST /time_control/ auxinput21_interval
NAMELIST /time_control/ auxinput21_begin_y
NAMELIST /time_control/ auxinput21_begin_d
NAMELIST /time_control/ auxinput21_begin_h
NAMELIST /time_control/ auxinput21_begin_m
NAMELIST /time_control/ auxinput21_begin_s
NAMELIST /time_control/ auxinput21_begin
NAMELIST /time_control/ auxinput21_end_y
NAMELIST /time_control/ auxinput21_end_d
NAMELIST /time_control/ auxinput21_end_h
NAMELIST /time_control/ auxinput21_end_m
NAMELIST /time_control/ auxinput21_end_s
NAMELIST /time_control/ auxinput21_end
NAMELIST /time_control/ io_form_auxinput21
NAMELIST /time_control/ frames_per_auxinput21
NAMELIST /time_control/ auxinput22_inname
NAMELIST /time_control/ auxinput22_outname
NAMELIST /time_control/ auxinput22_interval_y
NAMELIST /time_control/ auxinput22_interval_d
NAMELIST /time_control/ auxinput22_interval_h
NAMELIST /time_control/ auxinput22_interval_m
NAMELIST /time_control/ auxinput22_interval_s
NAMELIST /time_control/ auxinput22_interval
NAMELIST /time_control/ auxinput22_begin_y
NAMELIST /time_control/ auxinput22_begin_d
NAMELIST /time_control/ auxinput22_begin_h
NAMELIST /time_control/ auxinput22_begin_m
NAMELIST /time_control/ auxinput22_begin_s
NAMELIST /time_control/ auxinput22_begin
NAMELIST /time_control/ auxinput22_end_y
NAMELIST /time_control/ auxinput22_end_d
NAMELIST /time_control/ auxinput22_end_h
NAMELIST /time_control/ auxinput22_end_m
NAMELIST /time_control/ auxinput22_end_s
NAMELIST /time_control/ auxinput22_end
NAMELIST /time_control/ io_form_auxinput22
NAMELIST /time_control/ frames_per_auxinput22
NAMELIST /time_control/ auxinput23_inname
NAMELIST /time_control/ auxinput23_outname
NAMELIST /time_control/ auxinput23_interval_y
NAMELIST /time_control/ auxinput23_interval_d
NAMELIST /time_control/ auxinput23_interval_h
NAMELIST /time_control/ auxinput23_interval_m
NAMELIST /time_control/ auxinput23_interval_s
NAMELIST /time_control/ auxinput23_interval
NAMELIST /time_control/ auxinput23_begin_y
NAMELIST /time_control/ auxinput23_begin_d
NAMELIST /time_control/ auxinput23_begin_h
NAMELIST /time_control/ auxinput23_begin_m
NAMELIST /time_control/ auxinput23_begin_s
NAMELIST /time_control/ auxinput23_begin
NAMELIST /time_control/ auxinput23_end_y
NAMELIST /time_control/ auxinput23_end_d
NAMELIST /time_control/ auxinput23_end_h
NAMELIST /time_control/ auxinput23_end_m
NAMELIST /time_control/ auxinput23_end_s
NAMELIST /time_control/ auxinput23_end
NAMELIST /time_control/ io_form_auxinput23
NAMELIST /time_control/ frames_per_auxinput23
NAMELIST /time_control/ auxinput24_inname
NAMELIST /time_control/ auxinput24_outname
NAMELIST /time_control/ auxinput24_interval_y
NAMELIST /time_control/ auxinput24_interval_d
NAMELIST /time_control/ auxinput24_interval_h
NAMELIST /time_control/ auxinput24_interval_m
NAMELIST /time_control/ auxinput24_interval_s
NAMELIST /time_control/ auxinput24_interval
NAMELIST /time_control/ auxinput24_begin_y
NAMELIST /time_control/ auxinput24_begin_d
NAMELIST /time_control/ auxinput24_begin_h
NAMELIST /time_control/ auxinput24_begin_m
NAMELIST /time_control/ auxinput24_begin_s
NAMELIST /time_control/ auxinput24_begin
NAMELIST /time_control/ auxinput24_end_y
NAMELIST /time_control/ auxinput24_end_d
NAMELIST /time_control/ auxinput24_end_h
NAMELIST /time_control/ auxinput24_end_m
NAMELIST /time_control/ auxinput24_end_s
NAMELIST /time_control/ auxinput24_end
NAMELIST /time_control/ io_form_auxinput24
NAMELIST /time_control/ frames_per_auxinput24
NAMELIST /time_control/ history_interval
NAMELIST /time_control/ frames_per_outfile
NAMELIST /time_control/ restart
NAMELIST /time_control/ restart_interval
NAMELIST /time_control/ io_form_input
NAMELIST /time_control/ io_form_history
NAMELIST /time_control/ io_form_restart
NAMELIST /time_control/ io_form_boundary
NAMELIST /time_control/ debug_level
NAMELIST /time_control/ self_test_domain
NAMELIST /time_control/ history_outname
NAMELIST /time_control/ history_inname
NAMELIST /time_control/ use_netcdf_classic
NAMELIST /time_control/ history_interval_d
NAMELIST /time_control/ history_interval_h
NAMELIST /time_control/ history_interval_m
NAMELIST /time_control/ history_interval_s
NAMELIST /time_control/ inputout_interval_d
NAMELIST /time_control/ inputout_interval_h
NAMELIST /time_control/ inputout_interval_m
NAMELIST /time_control/ inputout_interval_s
NAMELIST /time_control/ inputout_interval
NAMELIST /time_control/ restart_interval_d
NAMELIST /time_control/ restart_interval_h
NAMELIST /time_control/ restart_interval_m
NAMELIST /time_control/ restart_interval_s
NAMELIST /time_control/ history_begin_y
NAMELIST /time_control/ history_begin_d
NAMELIST /time_control/ history_begin_h
NAMELIST /time_control/ history_begin_m
NAMELIST /time_control/ history_begin_s
NAMELIST /time_control/ history_begin
NAMELIST /time_control/ inputout_begin_y
NAMELIST /time_control/ inputout_begin_d
NAMELIST /time_control/ inputout_begin_h
NAMELIST /time_control/ inputout_begin_m
NAMELIST /time_control/ inputout_begin_s
NAMELIST /time_control/ restart_begin_y
NAMELIST /time_control/ restart_begin_d
NAMELIST /time_control/ restart_begin_h
NAMELIST /time_control/ restart_begin_m
NAMELIST /time_control/ restart_begin_s
NAMELIST /time_control/ restart_begin
NAMELIST /time_control/ history_end_y
NAMELIST /time_control/ history_end_d
NAMELIST /time_control/ history_end_h
NAMELIST /time_control/ history_end_m
NAMELIST /time_control/ history_end_s
NAMELIST /time_control/ history_end
NAMELIST /time_control/ inputout_end_y
NAMELIST /time_control/ inputout_end_d
NAMELIST /time_control/ inputout_end_h
NAMELIST /time_control/ inputout_end_m
NAMELIST /time_control/ inputout_end_s
NAMELIST /time_control/ reset_simulation_start
NAMELIST /domains/ sr_x
NAMELIST /domains/ sr_y
NAMELIST /time_control/ iofields_filename
NAMELIST /time_control/ ignore_iofields_warning
NAMELIST /time_control/ ncd_nofill
NAMELIST /time_control/ julyr
NAMELIST /time_control/ julday
NAMELIST /time_control/ gmt
NAMELIST /time_control/ high_freq_outname
NAMELIST /time_control/ partial_atcf_outname
NAMELIST /time_control/ input_inname
NAMELIST /time_control/ input_outname
NAMELIST /time_control/ bdy_inname
NAMELIST /time_control/ bdy_outname
NAMELIST /time_control/ rst_inname
NAMELIST /time_control/ rst_outname
NAMELIST /time_control/ anl_outname
NAMELIST /time_control/ write_input
NAMELIST /time_control/ write_restart_at_0h
NAMELIST /time_control/ write_hist_at_0h_rst
NAMELIST /time_control/ adjust_output_times
NAMELIST /time_control/ adjust_input_times
NAMELIST /time_control/ tstart
NAMELIST /time_control/ nocolons
NAMELIST /time_control/ cycling
NAMELIST /time_control/ output_ready_flag
NAMELIST /dfi_control/ dfi_opt
NAMELIST /dfi_control/ dfi_savehydmeteors
NAMELIST /dfi_control/ dfi_nfilter
NAMELIST /dfi_control/ dfi_write_filtered_input
NAMELIST /dfi_control/ dfi_write_dfi_history
NAMELIST /dfi_control/ dfi_cutoff_seconds
NAMELIST /dfi_control/ dfi_time_dim
NAMELIST /dfi_control/ dfi_fwdstop_year
NAMELIST /dfi_control/ dfi_fwdstop_month
NAMELIST /dfi_control/ dfi_fwdstop_day
NAMELIST /dfi_control/ dfi_fwdstop_hour
NAMELIST /dfi_control/ dfi_fwdstop_minute
NAMELIST /dfi_control/ dfi_fwdstop_second
NAMELIST /dfi_control/ dfi_bckstop_year
NAMELIST /dfi_control/ dfi_bckstop_month
NAMELIST /dfi_control/ dfi_bckstop_day
NAMELIST /dfi_control/ dfi_bckstop_hour
NAMELIST /dfi_control/ dfi_bckstop_minute
NAMELIST /dfi_control/ dfi_bckstop_second
NAMELIST /domains/ time_step
NAMELIST /domains/ time_step_fract_num
NAMELIST /domains/ time_step_fract_den
NAMELIST /domains/ time_step_dfi
NAMELIST /domains/ max_dom
NAMELIST /domains/ s_we
NAMELIST /domains/ e_we
NAMELIST /domains/ s_sn
NAMELIST /domains/ e_sn
NAMELIST /domains/ s_vert
NAMELIST /domains/ e_vert
NAMELIST /domains/ num_metgrid_soil_levels
NAMELIST /domains/ dx
NAMELIST /domains/ dy
NAMELIST /domains/ grid_id
NAMELIST /domains/ grid_allowed
NAMELIST /domains/ parent_id
NAMELIST /domains/ i_parent_start
NAMELIST /domains/ j_parent_start
NAMELIST /domains/ parent_grid_ratio
NAMELIST /domains/ parent_time_step_ratio
NAMELIST /domains/ feedback
NAMELIST /domains/ smooth_option
NAMELIST /domains/ ztop
NAMELIST /domains/ moad_grid_ratio
NAMELIST /domains/ moad_time_step_ratio
NAMELIST /domains/ shw
NAMELIST /domains/ tile_sz_x
NAMELIST /domains/ tile_sz_y
NAMELIST /domains/ numtiles
NAMELIST /domains/ numtiles_inc
NAMELIST /domains/ numtiles_x
NAMELIST /domains/ numtiles_y
NAMELIST /domains/ tile_strategy
NAMELIST /domains/ nproc_x
NAMELIST /domains/ nproc_y
NAMELIST /domains/ irand
NAMELIST /domains/ ts_buf_size
NAMELIST /domains/ max_ts_locs
NAMELIST /domains/ num_moves
NAMELIST /domains/ vortex_interval
NAMELIST /domains/ corral_dist
NAMELIST /domains/ move_id
NAMELIST /domains/ move_interval
NAMELIST /domains/ move_cd_x
NAMELIST /domains/ move_cd_y
NAMELIST /domains/ swap_x
NAMELIST /domains/ swap_y
NAMELIST /domains/ cycle_x
NAMELIST /domains/ cycle_y
NAMELIST /domains/ reorder_mesh
NAMELIST /domains/ perturb_input
NAMELIST /domains/ eta_levels
NAMELIST /domains/ ptsgm
NAMELIST /domains/ num_metgrid_levels
NAMELIST /domains/ p_top_requested
NAMELIST /domains/ use_prep_hybrid
NAMELIST /physics/ force_read_thompson
NAMELIST /physics/ write_thompson_tables
NAMELIST /physics/ mp_physics
NAMELIST /physics/ mommix
NAMELIST /physics/ disheat
NAMELIST /physics/ do_radar_ref
NAMELIST /physics/ ra_lw_physics
NAMELIST /physics/ ra_sw_physics
NAMELIST /physics/ radt
NAMELIST /physics/ sf_sfclay_physics
NAMELIST /physics/ sf_surface_physics
NAMELIST /physics/ bl_pbl_physics
NAMELIST /physics/ ysu_topdown_pblmix
NAMELIST /physics/ shinhong_tke_diag
NAMELIST /physics/ windfarm_opt
NAMELIST /physics/ windfarm_ij
NAMELIST /physics/ mfshconv
NAMELIST /physics/ bldt
NAMELIST /physics/ cu_physics
NAMELIST /physics/ shcu_physics
NAMELIST /physics/ cu_diag
NAMELIST /physics/ gfs_alpha
NAMELIST /physics/ cudt
NAMELIST /physics/ gsmdt
NAMELIST /physics/ isfflx
NAMELIST /physics/ ideal_xland
NAMELIST /physics/ ifsnow
NAMELIST /physics/ icloud
NAMELIST /physics/ swrad_scat
NAMELIST /physics/ surface_input_source
NAMELIST /physics/ num_soil_layers
NAMELIST /physics/ num_urban_layers
NAMELIST /physics/ sf_surface_mosaic
NAMELIST /physics/ mosaic_cat
NAMELIST /physics/ num_urban_hi
NAMELIST /physics/ mosaic_lu
NAMELIST /physics/ mosaic_soil
NAMELIST /physics/ maxiens
NAMELIST /physics/ maxens
NAMELIST /physics/ maxens2
NAMELIST /physics/ maxens3
NAMELIST /physics/ ensdim
NAMELIST /physics/ chem_opt
NAMELIST /physics/ num_land_cat
NAMELIST /physics/ num_soil_cat
NAMELIST /physics/ topo_wind
NAMELIST /physics/ mp_zero_out
NAMELIST /physics/ mp_zero_out_thresh
NAMELIST /physics/ seaice_threshold
NAMELIST /physics/ fractional_seaice
NAMELIST /physics/ seaice_albedo_opt
NAMELIST /physics/ seaice_albedo_default
NAMELIST /physics/ seaice_snowdepth_opt
NAMELIST /physics/ seaice_snowdepth_max
NAMELIST /physics/ seaice_snowdepth_min
NAMELIST /physics/ seaice_thickness_opt
NAMELIST /physics/ seaice_thickness_default
NAMELIST /physics/ tice2tsk_if2cold
NAMELIST /physics/ sst_update
NAMELIST /physics/ sf_urban_physics
NAMELIST /physics/ usemonalb
NAMELIST /physics/ rdmaxalb
NAMELIST /physics/ rdlai2d
NAMELIST /physics/ ua_phys
NAMELIST /physics/ gwd_opt
NAMELIST /physics/ iz0tlnd
NAMELIST /physics/ sas_pgcon
NAMELIST /physics/ sas_shal_pgcon
NAMELIST /physics/ sas_shal_conv
NAMELIST /physics/ sas_mass_flux
NAMELIST /physics/ var_ric
NAMELIST /physics/ coef_ric_l
NAMELIST /physics/ coef_ric_s
NAMELIST /physics/ random_seed
NAMELIST /physics/ icoef_sf
NAMELIST /physics/ lcurr_sf
NAMELIST /physics/ ens_random_seed
NAMELIST /physics/ pert_sas
NAMELIST /physics/ pert_pbl
NAMELIST /physics/ ens_sasamp
NAMELIST /physics/ ens_pblamp
NAMELIST /physics/ idtad
NAMELIST /physics/ nsoil
NAMELIST /physics/ nphs
NAMELIST /physics/ ncnvc
NAMELIST /physics/ nrand
NAMELIST /physics/ nrads
NAMELIST /physics/ nradl
NAMELIST /physics/ tprec
NAMELIST /physics/ theat
NAMELIST /physics/ tclod
NAMELIST /physics/ trdsw
NAMELIST /physics/ trdlw
NAMELIST /physics/ tsrfc
NAMELIST /physics/ pcpflg
NAMELIST /physics/ sigma
NAMELIST /physics/ sfenth
NAMELIST /physics/ co2tf
NAMELIST /physics/ ra_call_offset
NAMELIST /physics/ cam_abs_freq_s
NAMELIST /physics/ levsiz
NAMELIST /physics/ paerlev
NAMELIST /physics/ cam_abs_dim1
NAMELIST /physics/ cam_abs_dim2
NAMELIST /physics/ no_src_types
NAMELIST /physics/ alevsiz
NAMELIST /physics/ o3input
NAMELIST /physics/ aer_opt
NAMELIST /physics/ cu_rad_feedback
NAMELIST /physics/ h_diff
NAMELIST /physics/ movemin
NAMELIST /physics/ num_snso_layers
NAMELIST /physics/ num_snow_layers
NAMELIST /physics/ use_aero_icbc
NAMELIST /physics/ ccn_conc
NAMELIST /physics/ hail_opt
NAMELIST /physics/ sf_lake_physics
NAMELIST /dynamics/ dyn_opt
NAMELIST /dynamics/ rk_ord
NAMELIST /dynamics/ w_damping
NAMELIST /dynamics/ diff_opt
NAMELIST /dynamics/ km_opt
NAMELIST /dynamics/ damp_opt
NAMELIST /dynamics/ zdamp
NAMELIST /dynamics/ base_pres
NAMELIST /dynamics/ base_temp
NAMELIST /dynamics/ base_lapse
NAMELIST /dynamics/ iso_temp
NAMELIST /dynamics/ dampcoef
NAMELIST /dynamics/ khdif
NAMELIST /dynamics/ kvdif
NAMELIST /dynamics/ c_s
NAMELIST /dynamics/ c_k
NAMELIST /dynamics/ smdiv
NAMELIST /dynamics/ emdiv
NAMELIST /dynamics/ epssm
NAMELIST /dynamics/ non_hydrostatic
NAMELIST /dynamics/ time_step_sound
NAMELIST /dynamics/ h_mom_adv_order
NAMELIST /dynamics/ v_mom_adv_order
NAMELIST /dynamics/ h_sca_adv_order
NAMELIST /dynamics/ v_sca_adv_order
NAMELIST /dynamics/ top_radiation
NAMELIST /dynamics/ tke_upper_bound
NAMELIST /dynamics/ tke_drag_coefficient
NAMELIST /dynamics/ tke_heat_flux
NAMELIST /dynamics/ pert_coriolis
NAMELIST /dynamics/ euler_adv
NAMELIST /dynamics/ idtadt
NAMELIST /dynamics/ idtadc
NAMELIST /dynamics/ codamp
NAMELIST /dynamics/ coac
NAMELIST /dynamics/ slophc
NAMELIST /dynamics/ wp
NAMELIST /dynamics/ terrain_smoothing
NAMELIST /bdy_control/ spec_bdy_width
NAMELIST /bdy_control/ spec_zone
NAMELIST /bdy_control/ relax_zone
NAMELIST /bdy_control/ specified
NAMELIST /bdy_control/ periodic_x
NAMELIST /bdy_control/ symmetric_xs
NAMELIST /bdy_control/ symmetric_xe
NAMELIST /bdy_control/ open_xs
NAMELIST /bdy_control/ open_xe
NAMELIST /bdy_control/ periodic_y
NAMELIST /bdy_control/ symmetric_ys
NAMELIST /bdy_control/ symmetric_ye
NAMELIST /bdy_control/ open_ys
NAMELIST /bdy_control/ open_ye
NAMELIST /bdy_control/ polar
NAMELIST /bdy_control/ nested
NAMELIST /bdy_control/ real_data_init_type
NAMELIST /grib2/ background_proc_id
NAMELIST /grib2/ forecast_proc_id
NAMELIST /grib2/ production_status
NAMELIST /grib2/ compression
NAMELIST /physics/ maxpatch



     logical , DIMENSION(max_domains) :: pd_moist, pd_chem, pd_tke, pd_scalar
     NAMELIST /dynamics/                 pd_moist, pd_chem, pd_tke, pd_scalar

     integer , DIMENSION(max_domains) :: ucmcall
     NAMELIST /physics/                  ucmcall

     integer , DIMENSION(max_domains) :: obs_nobs_prt
     NAMELIST /fdda/                     obs_nobs_prt

     LOGICAL ::         global, print_detail_airep, print_detail_timing
     NAMELIST /wrfvar1/ global, print_detail_airep, print_detail_timing

     LOGICAL ::         write_qcw, write_qrn, write_qci, write_qsn
     NAMELIST /wrfvar2/ write_qcw, write_qrn, write_qci, write_qsn
     LOGICAL ::          write_qgr, write_filtered_obs
     NAMELIST /wrfvar2/  write_qgr, write_filtered_obs

     LOGICAL ::         use_eos_radobs
     NAMELIST /wrfvar4/ use_eos_radobs

     LOGICAL             :: use_crtm_kmatrix_fast
     NAMELIST /wrfvar14/    use_crtm_kmatrix_fast
     CHARACTER (LEN=256) :: spccoeff_file, taucoeff_file, aerosolcoeff_file
     NAMELIST /wrfvar14/    spccoeff_file, taucoeff_file, aerosolcoeff_file
     CHARACTER (LEN=256) :: cloudcoeff_file, emiscoeff_file
     NAMELIST /wrfvar14/    cloudcoeff_file, emiscoeff_file







     REWIND ( UNIT = nml_read_unit )


     if ( TRIM(nml_name) .eq. "dynamics" ) then

        READ   ( UNIT = nml_read_unit , NML = dynamics , iostat=nml_error )

        IF ( nml_error .EQ. 0 ) then    
           CALL wrf_debug(0, "-- Are pd_moist, pd_chem, pd_tke, or pd_scalar still in your "// &
                               TRIM(nml_name)//" namelist?")
           CALL wrf_debug(0, "-- Replace them with moist_adv_opt, chem_adv_opt, tke_adv_opt "// &
                             " and scalar_adv_opt, respectively.")
        ENDIF


     else if ( TRIM(nml_name) .eq. "physics" ) then

        READ   ( UNIT = nml_read_unit , NML = physics , iostat=nml_error )

        IF ( nml_error .EQ. 0 ) then    
           CALL wrf_debug(0, "-- Is ucmcall still in your "// TRIM(nml_name)//" namelist?")
           CALL wrf_debug(0, "-- Replace it with sf_urban_physics")
        ENDIF


     else if ( TRIM(nml_name) .eq. "fdda" ) then

        READ   ( UNIT = nml_read_unit , NML = fdda , iostat=nml_error )

        IF ( nml_error .EQ. 0 ) then    
           CALL wrf_debug(0, "-- Is obs_nobs_prt still in your "// TRIM(nml_name)//" namelist?")
           CALL wrf_debug(0, "-- Replace it with obs_prt_max")
        ENDIF


     else if ( TRIM(nml_name) .eq. "wrfvar1" ) then

        READ   ( UNIT = nml_read_unit , NML = wrfvar1 , iostat=nml_error )

        IF ( nml_error .EQ. 0 ) then    
           CALL wrf_debug(0, "-- Are global, print_detail_airep, print_detail_timing still in your "// &
                              TRIM(nml_name)//" namelist?")
           CALL wrf_debug(0, "-- Remove global, print_detail_airep, print_detail_timing "// &
                             "from wrfvar1 namelist as they are obsolete.")
        ENDIF


     else if ( TRIM(nml_name) .eq. "wrfvar2" ) then

        READ   ( UNIT = nml_read_unit , NML = wrfvar2 , iostat=nml_error )

        IF ( nml_error .EQ. 0 ) then    
           CALL wrf_debug(0, "-- Are write_qcw, write_qrn, write_qci, write_qsn, write_qgr, "// &
                             "write_filtered_obs still in your "// &
                              TRIM(nml_name)//" namelist?")
           CALL wrf_debug(0, "-- Remove write_qcw, write_qrn, write_qci, write_qsn, write_qgr, "// &
                             "write_filtered_obs as they are obsolete.")
        ENDIF


     else if ( TRIM(nml_name) .eq. "wrfvar4" ) then

        READ   ( UNIT = nml_read_unit , NML = wrfvar4 , iostat=nml_error )

        IF ( nml_error .EQ. 0 ) then    
           CALL wrf_debug(0, "-- Is use_eos_radobs still in your "// &
                              TRIM(nml_name)//" namelist?")
           CALL wrf_debug(0, "-- Remove use_eos_radobs as it is obsolete.")
        ENDIF


     else if ( TRIM(nml_name) .eq. "wrfvar14" ) then

     READ   ( UNIT = nml_read_unit , NML = wrfvar14 , iostat=nml_error )

        IF ( nml_error .EQ. 0 ) then    
           CALL wrf_debug(0, "-- Are use_crtm_kmatrix_fast, spccoeff_file, taucoeff_file, "// &
                             "aerosolcoeff_file, cloudcoeff_file, emiscoeff_file still in your "// &
                              TRIM(nml_name)//" namelist?")
           CALL wrf_debug(0, "-- Remove them as they are obsolete.")
        ENDIF


     else
         IF ( &







&      (TRIM(nml_name) .EQ. 'physics') &
& .OR. (TRIM(nml_name) .EQ. 'domains') &
& .OR. (TRIM(nml_name) .EQ. 'time_control') &
& .OR. (TRIM(nml_name) .EQ. 'noah_mp') &
& .OR. (TRIM(nml_name) .EQ. 'dfi_control') &
& .OR. (TRIM(nml_name) .EQ. 'dynamics') &
& .OR. (TRIM(nml_name) .EQ. 'bdy_control') &
& .OR. (TRIM(nml_name) .EQ. 'grib2') &
              ) THEN
            nml_error = 0
         ELSE
            CALL wrf_debug(0, TRIM(nml_name)//" is not a valid namelist name")
         ENDIF
     end if

     IF ( nml_error .NE. 0 ) then    
        return
     ENDIF

   END SUBROUTINE wrf_alt_nml_obsolete

END MODULE module_configure


SUBROUTINE set_scalar_indices_from_config ( idomain , dummy2, dummy1 )
  USE module_driver_constants
  USE module_state_description
  USE module_wrf_error
  USE module_configure, ONLY : model_config_rec
  USE module_scalar_tables
  IMPLICIT NONE
  INTEGER , INTENT(IN)  :: idomain
  INTEGER               :: dummy1
  INTEGER               :: dummy2
































  P_szj1 = 1 ; F_szj1 = .FALSE. 
  P_szj2 = 1 ; F_szj2 = .FALSE. 
  P_szj3 = 1 ; F_szj3 = .FALSE. 
  P_szj4 = 1 ; F_szj4 = .FALSE. 
  P_s1z1 = 1 ; F_s1z1 = .FALSE. 
  P_s1z2 = 1 ; F_s1z2 = .FALSE. 
  P_s1z3 = 1 ; F_s1z3 = .FALSE. 
  P_s1z4 = 1 ; F_s1z4 = .FALSE. 
  P_spz1 = 1 ; F_spz1 = .FALSE. 
  P_spz2 = 1 ; F_spz2 = .FALSE. 
  P_spz3 = 1 ; F_spz3 = .FALSE. 
  P_spz4 = 1 ; F_spz4 = .FALSE. 
  P_tcs1 = 1 ; F_tcs1 = .FALSE. 
  P_tcs2 = 1 ; F_tcs2 = .FALSE. 
  P_tcs3 = 1 ; F_tcs3 = .FALSE. 
  P_tcs4 = 1 ; F_tcs4 = .FALSE. 
  P_qv = 1 ; F_qv = .FALSE. 
  P_qc = 1 ; F_qc = .FALSE. 
  P_qr = 1 ; F_qr = .FALSE. 
  P_qi = 1 ; F_qi = .FALSE. 
  P_qs = 1 ; F_qs = .FALSE. 
  P_qg = 1 ; F_qg = .FALSE. 
  P_qh = 1 ; F_qh = .FALSE. 
  P_dfi_qv = 1 ; F_dfi_qv = .FALSE. 
  P_dfi_qc = 1 ; F_dfi_qc = .FALSE. 
  P_dfi_qr = 1 ; F_dfi_qr = .FALSE. 
  P_dfi_qi = 1 ; F_dfi_qi = .FALSE. 
  P_dfi_qs = 1 ; F_dfi_qs = .FALSE. 
  P_dfi_qg = 1 ; F_dfi_qg = .FALSE. 
  P_dfi_qh = 1 ; F_dfi_qh = .FALSE. 
  P_qni = 1 ; F_qni = .FALSE. 
  P_qt = 1 ; F_qt = .FALSE. 
  P_qns = 1 ; F_qns = .FALSE. 
  P_qnr = 1 ; F_qnr = .FALSE. 
  P_qng = 1 ; F_qng = .FALSE. 
  P_qnh = 1 ; F_qnh = .FALSE. 
  P_qnn = 1 ; F_qnn = .FALSE. 
  P_qnc = 1 ; F_qnc = .FALSE. 
  P_qvolg = 1 ; F_qvolg = .FALSE. 
  P_qnwfa = 1 ; F_qnwfa = .FALSE. 
  P_qnifa = 1 ; F_qnifa = .FALSE. 
  P_qndrop = 1 ; F_qndrop = .FALSE. 
  P_qvolh = 1 ; F_qvolh = .FALSE. 
  P_dfi_qndrop = 1 ; F_dfi_qndrop = .FALSE. 
  P_dfi_qni = 1 ; F_dfi_qni = .FALSE. 
  P_dfi_qt = 1 ; F_dfi_qt = .FALSE. 
  P_dfi_qns = 1 ; F_dfi_qns = .FALSE. 
  P_dfi_qnr = 1 ; F_dfi_qnr = .FALSE. 
  P_dfi_qng = 1 ; F_dfi_qng = .FALSE. 
  P_dfi_qnh = 1 ; F_dfi_qnh = .FALSE. 
  P_dfi_qnn = 1 ; F_dfi_qnn = .FALSE. 
  P_dfi_qnc = 1 ; F_dfi_qnc = .FALSE. 
  P_dfi_qnwfa = 1 ; F_dfi_qnwfa = .FALSE. 
  P_dfi_qnifa = 1 ; F_dfi_qnifa = .FALSE. 
  P_mth01 = 1 ; F_mth01 = .FALSE. 
  P_mth02 = 1 ; F_mth02 = .FALSE. 
  P_mth03 = 1 ; F_mth03 = .FALSE. 
  P_mth04 = 1 ; F_mth04 = .FALSE. 
  P_mth05 = 1 ; F_mth05 = .FALSE. 
  P_mth06 = 1 ; F_mth06 = .FALSE. 
  P_mth07 = 1 ; F_mth07 = .FALSE. 
  P_mth08 = 1 ; F_mth08 = .FALSE. 
  P_mth09 = 1 ; F_mth09 = .FALSE. 
  P_mth10 = 1 ; F_mth10 = .FALSE. 
  P_mth11 = 1 ; F_mth11 = .FALSE. 
  P_mth12 = 1 ; F_mth12 = .FALSE. 
  IF (model_config_rec%sf_lake_physics(idomain)==0)THEN
  END IF
  IF (model_config_rec%sf_lake_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%ntracers==4)THEN
   IF ( szj_index_table( PARAM_szj1 , idomain ) .lt. 1 ) THEN
     szj_num_table(idomain) = szj_num_table(idomain) + 1
     P_szj1 = szj_num_table(idomain)
     szj_index_table( PARAM_szj1 , idomain ) = P_szj1
   ELSE
     P_szj1 = szj_index_table( PARAM_szj1 , idomain )
   END IF
   szj_boundary_table( idomain, P_szj1 ) = .FALSE.
   szj_dname_table( idomain, P_szj1 ) = 'szj1'
   szj_desc_table( idomain, P_szj1 ) = 'szj'
   szj_units_table( idomain, P_szj1 ) = 'units'
   szj_streams_table( idomain, P_szj1 )%stream(1) = 0 
   szj_streams_table( idomain, P_szj1 )%stream(2) = 2097152 
   F_szj1 = .TRUE.
   IF ( szj_index_table( PARAM_szj2 , idomain ) .lt. 1 ) THEN
     szj_num_table(idomain) = szj_num_table(idomain) + 1
     P_szj2 = szj_num_table(idomain)
     szj_index_table( PARAM_szj2 , idomain ) = P_szj2
   ELSE
     P_szj2 = szj_index_table( PARAM_szj2 , idomain )
   END IF
   szj_boundary_table( idomain, P_szj2 ) = .FALSE.
   szj_dname_table( idomain, P_szj2 ) = 'szj2'
   szj_desc_table( idomain, P_szj2 ) = 'szj'
   szj_units_table( idomain, P_szj2 ) = 'units'
   szj_streams_table( idomain, P_szj2 )%stream(1) = 0 
   szj_streams_table( idomain, P_szj2 )%stream(2) = 2097152 
   F_szj2 = .TRUE.
   IF ( szj_index_table( PARAM_szj3 , idomain ) .lt. 1 ) THEN
     szj_num_table(idomain) = szj_num_table(idomain) + 1
     P_szj3 = szj_num_table(idomain)
     szj_index_table( PARAM_szj3 , idomain ) = P_szj3
   ELSE
     P_szj3 = szj_index_table( PARAM_szj3 , idomain )
   END IF
   szj_boundary_table( idomain, P_szj3 ) = .FALSE.
   szj_dname_table( idomain, P_szj3 ) = 'szj3'
   szj_desc_table( idomain, P_szj3 ) = 'szj'
   szj_units_table( idomain, P_szj3 ) = 'units'
   szj_streams_table( idomain, P_szj3 )%stream(1) = 0 
   szj_streams_table( idomain, P_szj3 )%stream(2) = 2097152 
   F_szj3 = .TRUE.
   IF ( szj_index_table( PARAM_szj4 , idomain ) .lt. 1 ) THEN
     szj_num_table(idomain) = szj_num_table(idomain) + 1
     P_szj4 = szj_num_table(idomain)
     szj_index_table( PARAM_szj4 , idomain ) = P_szj4
   ELSE
     P_szj4 = szj_index_table( PARAM_szj4 , idomain )
   END IF
   szj_boundary_table( idomain, P_szj4 ) = .FALSE.
   szj_dname_table( idomain, P_szj4 ) = 'szj4'
   szj_desc_table( idomain, P_szj4 ) = 'szj'
   szj_units_table( idomain, P_szj4 ) = 'units'
   szj_streams_table( idomain, P_szj4 )%stream(1) = 0 
   szj_streams_table( idomain, P_szj4 )%stream(2) = 2097152 
   F_szj4 = .TRUE.
   IF ( s1z_index_table( PARAM_s1z1 , idomain ) .lt. 1 ) THEN
     s1z_num_table(idomain) = s1z_num_table(idomain) + 1
     P_s1z1 = s1z_num_table(idomain)
     s1z_index_table( PARAM_s1z1 , idomain ) = P_s1z1
   ELSE
     P_s1z1 = s1z_index_table( PARAM_s1z1 , idomain )
   END IF
   s1z_boundary_table( idomain, P_s1z1 ) = .FALSE.
   s1z_dname_table( idomain, P_s1z1 ) = 's1z1'
   s1z_desc_table( idomain, P_s1z1 ) = 's1z'
   s1z_units_table( idomain, P_s1z1 ) = 'units'
   s1z_streams_table( idomain, P_s1z1 )%stream(1) = 0 
   s1z_streams_table( idomain, P_s1z1 )%stream(2) = 2097152 
   F_s1z1 = .TRUE.
   IF ( s1z_index_table( PARAM_s1z2 , idomain ) .lt. 1 ) THEN
     s1z_num_table(idomain) = s1z_num_table(idomain) + 1
     P_s1z2 = s1z_num_table(idomain)
     s1z_index_table( PARAM_s1z2 , idomain ) = P_s1z2
   ELSE
     P_s1z2 = s1z_index_table( PARAM_s1z2 , idomain )
   END IF
   s1z_boundary_table( idomain, P_s1z2 ) = .FALSE.
   s1z_dname_table( idomain, P_s1z2 ) = 's1z2'
   s1z_desc_table( idomain, P_s1z2 ) = 's1z'
   s1z_units_table( idomain, P_s1z2 ) = 'units'
   s1z_streams_table( idomain, P_s1z2 )%stream(1) = 0 
   s1z_streams_table( idomain, P_s1z2 )%stream(2) = 2097152 
   F_s1z2 = .TRUE.
   IF ( s1z_index_table( PARAM_s1z3 , idomain ) .lt. 1 ) THEN
     s1z_num_table(idomain) = s1z_num_table(idomain) + 1
     P_s1z3 = s1z_num_table(idomain)
     s1z_index_table( PARAM_s1z3 , idomain ) = P_s1z3
   ELSE
     P_s1z3 = s1z_index_table( PARAM_s1z3 , idomain )
   END IF
   s1z_boundary_table( idomain, P_s1z3 ) = .FALSE.
   s1z_dname_table( idomain, P_s1z3 ) = 's1z3'
   s1z_desc_table( idomain, P_s1z3 ) = 's1z'
   s1z_units_table( idomain, P_s1z3 ) = 'units'
   s1z_streams_table( idomain, P_s1z3 )%stream(1) = 0 
   s1z_streams_table( idomain, P_s1z3 )%stream(2) = 2097152 
   F_s1z3 = .TRUE.
   IF ( s1z_index_table( PARAM_s1z4 , idomain ) .lt. 1 ) THEN
     s1z_num_table(idomain) = s1z_num_table(idomain) + 1
     P_s1z4 = s1z_num_table(idomain)
     s1z_index_table( PARAM_s1z4 , idomain ) = P_s1z4
   ELSE
     P_s1z4 = s1z_index_table( PARAM_s1z4 , idomain )
   END IF
   s1z_boundary_table( idomain, P_s1z4 ) = .FALSE.
   s1z_dname_table( idomain, P_s1z4 ) = 's1z4'
   s1z_desc_table( idomain, P_s1z4 ) = 's1z'
   s1z_units_table( idomain, P_s1z4 ) = 'units'
   s1z_streams_table( idomain, P_s1z4 )%stream(1) = 0 
   s1z_streams_table( idomain, P_s1z4 )%stream(2) = 2097152 
   F_s1z4 = .TRUE.
   IF ( spz_index_table( PARAM_spz1 , idomain ) .lt. 1 ) THEN
     spz_num_table(idomain) = spz_num_table(idomain) + 1
     P_spz1 = spz_num_table(idomain)
     spz_index_table( PARAM_spz1 , idomain ) = P_spz1
   ELSE
     P_spz1 = spz_index_table( PARAM_spz1 , idomain )
   END IF
   spz_boundary_table( idomain, P_spz1 ) = .FALSE.
   spz_dname_table( idomain, P_spz1 ) = 'spz1'
   spz_desc_table( idomain, P_spz1 ) = 'spz'
   spz_units_table( idomain, P_spz1 ) = 'units'
   spz_streams_table( idomain, P_spz1 )%stream(1) = 0 
   spz_streams_table( idomain, P_spz1 )%stream(2) = 2097152 
   F_spz1 = .TRUE.
   IF ( spz_index_table( PARAM_spz2 , idomain ) .lt. 1 ) THEN
     spz_num_table(idomain) = spz_num_table(idomain) + 1
     P_spz2 = spz_num_table(idomain)
     spz_index_table( PARAM_spz2 , idomain ) = P_spz2
   ELSE
     P_spz2 = spz_index_table( PARAM_spz2 , idomain )
   END IF
   spz_boundary_table( idomain, P_spz2 ) = .FALSE.
   spz_dname_table( idomain, P_spz2 ) = 'spz2'
   spz_desc_table( idomain, P_spz2 ) = 'spz'
   spz_units_table( idomain, P_spz2 ) = 'units'
   spz_streams_table( idomain, P_spz2 )%stream(1) = 0 
   spz_streams_table( idomain, P_spz2 )%stream(2) = 2097152 
   F_spz2 = .TRUE.
   IF ( spz_index_table( PARAM_spz3 , idomain ) .lt. 1 ) THEN
     spz_num_table(idomain) = spz_num_table(idomain) + 1
     P_spz3 = spz_num_table(idomain)
     spz_index_table( PARAM_spz3 , idomain ) = P_spz3
   ELSE
     P_spz3 = spz_index_table( PARAM_spz3 , idomain )
   END IF
   spz_boundary_table( idomain, P_spz3 ) = .FALSE.
   spz_dname_table( idomain, P_spz3 ) = 'spz3'
   spz_desc_table( idomain, P_spz3 ) = 'spz'
   spz_units_table( idomain, P_spz3 ) = 'units'
   spz_streams_table( idomain, P_spz3 )%stream(1) = 0 
   spz_streams_table( idomain, P_spz3 )%stream(2) = 2097152 
   F_spz3 = .TRUE.
   IF ( spz_index_table( PARAM_spz4 , idomain ) .lt. 1 ) THEN
     spz_num_table(idomain) = spz_num_table(idomain) + 1
     P_spz4 = spz_num_table(idomain)
     spz_index_table( PARAM_spz4 , idomain ) = P_spz4
   ELSE
     P_spz4 = spz_index_table( PARAM_spz4 , idomain )
   END IF
   spz_boundary_table( idomain, P_spz4 ) = .FALSE.
   spz_dname_table( idomain, P_spz4 ) = 'spz4'
   spz_desc_table( idomain, P_spz4 ) = 'spz'
   spz_units_table( idomain, P_spz4 ) = 'units'
   spz_streams_table( idomain, P_spz4 )%stream(1) = 0 
   spz_streams_table( idomain, P_spz4 )%stream(2) = 2097152 
   F_spz4 = .TRUE.
   IF ( tcs_index_table( PARAM_tcs1 , idomain ) .lt. 1 ) THEN
     tcs_num_table(idomain) = tcs_num_table(idomain) + 1
     P_tcs1 = tcs_num_table(idomain)
     tcs_index_table( PARAM_tcs1 , idomain ) = P_tcs1
   ELSE
     P_tcs1 = tcs_index_table( PARAM_tcs1 , idomain )
   END IF
   tcs_boundary_table( idomain, P_tcs1 ) = .FALSE.
   tcs_dname_table( idomain, P_tcs1 ) = 'tcs1'
   tcs_desc_table( idomain, P_tcs1 ) = 'tcs'
   tcs_units_table( idomain, P_tcs1 ) = 'units'
   tcs_streams_table( idomain, P_tcs1 )%stream(1) = 0 
   tcs_streams_table( idomain, P_tcs1 )%stream(2) = 2097152 
   F_tcs1 = .TRUE.
   IF ( tcs_index_table( PARAM_tcs2 , idomain ) .lt. 1 ) THEN
     tcs_num_table(idomain) = tcs_num_table(idomain) + 1
     P_tcs2 = tcs_num_table(idomain)
     tcs_index_table( PARAM_tcs2 , idomain ) = P_tcs2
   ELSE
     P_tcs2 = tcs_index_table( PARAM_tcs2 , idomain )
   END IF
   tcs_boundary_table( idomain, P_tcs2 ) = .FALSE.
   tcs_dname_table( idomain, P_tcs2 ) = 'tcs2'
   tcs_desc_table( idomain, P_tcs2 ) = 'tcs'
   tcs_units_table( idomain, P_tcs2 ) = 'units'
   tcs_streams_table( idomain, P_tcs2 )%stream(1) = 0 
   tcs_streams_table( idomain, P_tcs2 )%stream(2) = 2097152 
   F_tcs2 = .TRUE.
   IF ( tcs_index_table( PARAM_tcs3 , idomain ) .lt. 1 ) THEN
     tcs_num_table(idomain) = tcs_num_table(idomain) + 1
     P_tcs3 = tcs_num_table(idomain)
     tcs_index_table( PARAM_tcs3 , idomain ) = P_tcs3
   ELSE
     P_tcs3 = tcs_index_table( PARAM_tcs3 , idomain )
   END IF
   tcs_boundary_table( idomain, P_tcs3 ) = .FALSE.
   tcs_dname_table( idomain, P_tcs3 ) = 'tcs3'
   tcs_desc_table( idomain, P_tcs3 ) = 'tcs'
   tcs_units_table( idomain, P_tcs3 ) = 'units'
   tcs_streams_table( idomain, P_tcs3 )%stream(1) = 0 
   tcs_streams_table( idomain, P_tcs3 )%stream(2) = 2097152 
   F_tcs3 = .TRUE.
   IF ( tcs_index_table( PARAM_tcs4 , idomain ) .lt. 1 ) THEN
     tcs_num_table(idomain) = tcs_num_table(idomain) + 1
     P_tcs4 = tcs_num_table(idomain)
     tcs_index_table( PARAM_tcs4 , idomain ) = P_tcs4
   ELSE
     P_tcs4 = tcs_index_table( PARAM_tcs4 , idomain )
   END IF
   tcs_boundary_table( idomain, P_tcs4 ) = .FALSE.
   tcs_dname_table( idomain, P_tcs4 ) = 'tcs4'
   tcs_desc_table( idomain, P_tcs4 ) = 'tcs'
   tcs_units_table( idomain, P_tcs4 ) = 'units'
   tcs_streams_table( idomain, P_tcs4 )%stream(1) = 0 
   tcs_streams_table( idomain, P_tcs4 )%stream(2) = 2097152 
   F_tcs4 = .TRUE.
  END IF
  IF (model_config_rec%swath_mode==1)THEN
  END IF
  IF (model_config_rec%vortex_tracker(idomain)==1)THEN
  END IF
  IF (model_config_rec%vortex_tracker(idomain)==2)THEN
  END IF
  IF (model_config_rec%vortex_tracker(idomain)==3)THEN
  END IF
  IF (model_config_rec%vortex_tracker(idomain)==4)THEN
  END IF
  IF (model_config_rec%vortex_tracker(idomain)==5)THEN
  END IF
  IF (model_config_rec%vortex_tracker(idomain)==6)THEN
  END IF
  IF (model_config_rec%vortex_tracker(idomain)==7)THEN
  END IF
  IF (model_config_rec%tg_option==0)THEN
  END IF
  IF (model_config_rec%tg_option==1)THEN
  END IF
  IF (model_config_rec%dyn_opt==5)THEN
  END IF
  IF (model_config_rec%chem_opt(idomain)==0)THEN
  END IF
  IF (model_config_rec%mp_physics(idomain)==0)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==1)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==2)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 
   F_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==3)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==4)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 
   F_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==5)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 
   F_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==6)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 
   F_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==7)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 
   F_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==8)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 
   F_qg = .TRUE.
   IF ( scalar_index_table( PARAM_qni , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qni = scalar_num_table(idomain)
     scalar_index_table( PARAM_qni , idomain ) = P_qni
   ELSE
     P_qni = scalar_index_table( PARAM_qni , idomain )
   END IF
   scalar_boundary_table( idomain, P_qni ) = .TRUE.
   scalar_dname_table( idomain, P_qni ) = 'QNICE'
   scalar_desc_table( idomain, P_qni ) = 'Ice Number concentration'
   scalar_units_table( idomain, P_qni ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qni )%stream(1) = 100663309 
   scalar_streams_table( idomain, P_qni )%stream(2) = 2097152 
   F_qni = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnr ) = .TRUE.
   scalar_dname_table( idomain, P_qnr ) = 'QNRAIN'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnr )%stream(1) = 100663309 
   scalar_streams_table( idomain, P_qnr )%stream(2) = 2097152 
   F_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==28)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 
   F_qg = .TRUE.
   IF ( scalar_index_table( PARAM_qni , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qni = scalar_num_table(idomain)
     scalar_index_table( PARAM_qni , idomain ) = P_qni
   ELSE
     P_qni = scalar_index_table( PARAM_qni , idomain )
   END IF
   scalar_boundary_table( idomain, P_qni ) = .TRUE.
   scalar_dname_table( idomain, P_qni ) = 'QNICE'
   scalar_desc_table( idomain, P_qni ) = 'Ice Number concentration'
   scalar_units_table( idomain, P_qni ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qni )%stream(1) = 100663309 
   scalar_streams_table( idomain, P_qni )%stream(2) = 2097152 
   F_qni = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnr ) = .TRUE.
   scalar_dname_table( idomain, P_qnr ) = 'QNRAIN'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnr )%stream(1) = 100663309 
   scalar_streams_table( idomain, P_qnr )%stream(2) = 2097152 
   F_qnr = .TRUE.
   IF ( scalar_index_table( PARAM_qnc , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnc = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnc , idomain ) = P_qnc
   ELSE
     P_qnc = scalar_index_table( PARAM_qnc , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnc ) = .TRUE.
   scalar_dname_table( idomain, P_qnc ) = 'QNCLOUD'
   scalar_desc_table( idomain, P_qnc ) = 'cloud water Number concentration'
   scalar_units_table( idomain, P_qnc ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnc )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qnc )%stream(2) = 2097152 
   F_qnc = .TRUE.
   IF ( scalar_index_table( PARAM_qnwfa , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnwfa = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnwfa , idomain ) = P_qnwfa
   ELSE
     P_qnwfa = scalar_index_table( PARAM_qnwfa , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnwfa ) = .TRUE.
   scalar_dname_table( idomain, P_qnwfa ) = 'QNWFA'
   scalar_desc_table( idomain, P_qnwfa ) = 'water-friendly aerosol number con'
   scalar_units_table( idomain, P_qnwfa ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnwfa )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qnwfa )%stream(2) = 2097152 
   F_qnwfa = .TRUE.
   IF ( scalar_index_table( PARAM_qnifa , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnifa = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnifa , idomain ) = P_qnifa
   ELSE
     P_qnifa = scalar_index_table( PARAM_qnifa , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnifa ) = .TRUE.
   scalar_dname_table( idomain, P_qnifa ) = 'QNIFA'
   scalar_desc_table( idomain, P_qnifa ) = 'ice-friendly aerosol number con'
   scalar_units_table( idomain, P_qnifa ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnifa )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qnifa )%stream(2) = 2097152 
   F_qnifa = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==9)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 
   F_qg = .TRUE.
   IF ( moist_index_table( PARAM_qh , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qh = moist_num_table(idomain)
     moist_index_table( PARAM_qh , idomain ) = P_qh
   ELSE
     P_qh = moist_index_table( PARAM_qh , idomain )
   END IF
   moist_boundary_table( idomain, P_qh ) = .TRUE.
   moist_dname_table( idomain, P_qh ) = 'QHAIL'
   moist_desc_table( idomain, P_qh ) = 'Hail mixing ratio'
   moist_units_table( idomain, P_qh ) = 'kg kg-1'
   moist_streams_table( idomain, P_qh )%stream(1) = 1 
   moist_streams_table( idomain, P_qh )%stream(2) = 2097152 
   F_qh = .TRUE.
   IF ( scalar_index_table( PARAM_qnc , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnc = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnc , idomain ) = P_qnc
   ELSE
     P_qnc = scalar_index_table( PARAM_qnc , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnc ) = .TRUE.
   scalar_dname_table( idomain, P_qnc ) = 'QNCLOUD'
   scalar_desc_table( idomain, P_qnc ) = 'cloud water Number concentration'
   scalar_units_table( idomain, P_qnc ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnc )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qnc )%stream(2) = 2097152 
   F_qnc = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnr ) = .TRUE.
   scalar_dname_table( idomain, P_qnr ) = 'QNRAIN'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnr )%stream(1) = 100663309 
   scalar_streams_table( idomain, P_qnr )%stream(2) = 2097152 
   F_qnr = .TRUE.
   IF ( scalar_index_table( PARAM_qni , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qni = scalar_num_table(idomain)
     scalar_index_table( PARAM_qni , idomain ) = P_qni
   ELSE
     P_qni = scalar_index_table( PARAM_qni , idomain )
   END IF
   scalar_boundary_table( idomain, P_qni ) = .TRUE.
   scalar_dname_table( idomain, P_qni ) = 'QNICE'
   scalar_desc_table( idomain, P_qni ) = 'Ice Number concentration'
   scalar_units_table( idomain, P_qni ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qni )%stream(1) = 100663309 
   scalar_streams_table( idomain, P_qni )%stream(2) = 2097152 
   F_qni = .TRUE.
   IF ( scalar_index_table( PARAM_qns , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qns = scalar_num_table(idomain)
     scalar_index_table( PARAM_qns , idomain ) = P_qns
   ELSE
     P_qns = scalar_index_table( PARAM_qns , idomain )
   END IF
   scalar_boundary_table( idomain, P_qns ) = .TRUE.
   scalar_dname_table( idomain, P_qns ) = 'QNSNOW'
   scalar_desc_table( idomain, P_qns ) = 'Snow Number concentration'
   scalar_units_table( idomain, P_qns ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qns )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qns )%stream(2) = 2097152 
   F_qns = .TRUE.
   IF ( scalar_index_table( PARAM_qng , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qng = scalar_num_table(idomain)
     scalar_index_table( PARAM_qng , idomain ) = P_qng
   ELSE
     P_qng = scalar_index_table( PARAM_qng , idomain )
   END IF
   scalar_boundary_table( idomain, P_qng ) = .TRUE.
   scalar_dname_table( idomain, P_qng ) = 'QNGRAUP'
   scalar_desc_table( idomain, P_qng ) = 'Graupel Number concentration'
   scalar_units_table( idomain, P_qng ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qng )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qng )%stream(2) = 2097152 
   F_qng = .TRUE.
   IF ( scalar_index_table( PARAM_qnh , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnh = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnh , idomain ) = P_qnh
   ELSE
     P_qnh = scalar_index_table( PARAM_qnh , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnh ) = .TRUE.
   scalar_dname_table( idomain, P_qnh ) = 'QNHAIL'
   scalar_desc_table( idomain, P_qnh ) = 'Hail Number concentration'
   scalar_units_table( idomain, P_qnh ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnh )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qnh )%stream(2) = 2097152 
   F_qnh = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==10)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 
   F_qg = .TRUE.
   IF ( scalar_index_table( PARAM_qni , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qni = scalar_num_table(idomain)
     scalar_index_table( PARAM_qni , idomain ) = P_qni
   ELSE
     P_qni = scalar_index_table( PARAM_qni , idomain )
   END IF
   scalar_boundary_table( idomain, P_qni ) = .TRUE.
   scalar_dname_table( idomain, P_qni ) = 'QNICE'
   scalar_desc_table( idomain, P_qni ) = 'Ice Number concentration'
   scalar_units_table( idomain, P_qni ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qni )%stream(1) = 100663309 
   scalar_streams_table( idomain, P_qni )%stream(2) = 2097152 
   F_qni = .TRUE.
   IF ( scalar_index_table( PARAM_qns , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qns = scalar_num_table(idomain)
     scalar_index_table( PARAM_qns , idomain ) = P_qns
   ELSE
     P_qns = scalar_index_table( PARAM_qns , idomain )
   END IF
   scalar_boundary_table( idomain, P_qns ) = .TRUE.
   scalar_dname_table( idomain, P_qns ) = 'QNSNOW'
   scalar_desc_table( idomain, P_qns ) = 'Snow Number concentration'
   scalar_units_table( idomain, P_qns ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qns )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qns )%stream(2) = 2097152 
   F_qns = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnr ) = .TRUE.
   scalar_dname_table( idomain, P_qnr ) = 'QNRAIN'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnr )%stream(1) = 100663309 
   scalar_streams_table( idomain, P_qnr )%stream(2) = 2097152 
   F_qnr = .TRUE.
   IF ( scalar_index_table( PARAM_qng , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qng = scalar_num_table(idomain)
     scalar_index_table( PARAM_qng , idomain ) = P_qng
   ELSE
     P_qng = scalar_index_table( PARAM_qng , idomain )
   END IF
   scalar_boundary_table( idomain, P_qng ) = .TRUE.
   scalar_dname_table( idomain, P_qng ) = 'QNGRAUP'
   scalar_desc_table( idomain, P_qng ) = 'Graupel Number concentration'
   scalar_units_table( idomain, P_qng ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qng )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qng )%stream(2) = 2097152 
   F_qng = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==13)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 
   F_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==14)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 
   F_qs = .TRUE.
   IF ( scalar_index_table( PARAM_qnn , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnn = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnn , idomain ) = P_qnn
   ELSE
     P_qnn = scalar_index_table( PARAM_qnn , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnn ) = .TRUE.
   scalar_dname_table( idomain, P_qnn ) = 'QNCCN'
   scalar_desc_table( idomain, P_qnn ) = 'CCN Number concentration'
   scalar_units_table( idomain, P_qnn ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnn )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qnn )%stream(2) = 2097152 
   F_qnn = .TRUE.
   IF ( scalar_index_table( PARAM_qnc , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnc = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnc , idomain ) = P_qnc
   ELSE
     P_qnc = scalar_index_table( PARAM_qnc , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnc ) = .TRUE.
   scalar_dname_table( idomain, P_qnc ) = 'QNCLOUD'
   scalar_desc_table( idomain, P_qnc ) = 'cloud water Number concentration'
   scalar_units_table( idomain, P_qnc ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnc )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qnc )%stream(2) = 2097152 
   F_qnc = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnr ) = .TRUE.
   scalar_dname_table( idomain, P_qnr ) = 'QNRAIN'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnr )%stream(1) = 100663309 
   scalar_streams_table( idomain, P_qnr )%stream(2) = 2097152 
   F_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==16)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 
   F_qg = .TRUE.
   IF ( scalar_index_table( PARAM_qnn , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnn = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnn , idomain ) = P_qnn
   ELSE
     P_qnn = scalar_index_table( PARAM_qnn , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnn ) = .TRUE.
   scalar_dname_table( idomain, P_qnn ) = 'QNCCN'
   scalar_desc_table( idomain, P_qnn ) = 'CCN Number concentration'
   scalar_units_table( idomain, P_qnn ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnn )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qnn )%stream(2) = 2097152 
   F_qnn = .TRUE.
   IF ( scalar_index_table( PARAM_qnc , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnc = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnc , idomain ) = P_qnc
   ELSE
     P_qnc = scalar_index_table( PARAM_qnc , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnc ) = .TRUE.
   scalar_dname_table( idomain, P_qnc ) = 'QNCLOUD'
   scalar_desc_table( idomain, P_qnc ) = 'cloud water Number concentration'
   scalar_units_table( idomain, P_qnc ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnc )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qnc )%stream(2) = 2097152 
   F_qnc = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnr ) = .TRUE.
   scalar_dname_table( idomain, P_qnr ) = 'QNRAIN'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnr )%stream(1) = 100663309 
   scalar_streams_table( idomain, P_qnr )%stream(2) = 2097152 
   F_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==17)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 
   F_qg = .TRUE.
   IF ( moist_index_table( PARAM_qh , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qh = moist_num_table(idomain)
     moist_index_table( PARAM_qh , idomain ) = P_qh
   ELSE
     P_qh = moist_index_table( PARAM_qh , idomain )
   END IF
   moist_boundary_table( idomain, P_qh ) = .TRUE.
   moist_dname_table( idomain, P_qh ) = 'QHAIL'
   moist_desc_table( idomain, P_qh ) = 'Hail mixing ratio'
   moist_units_table( idomain, P_qh ) = 'kg kg-1'
   moist_streams_table( idomain, P_qh )%stream(1) = 1 
   moist_streams_table( idomain, P_qh )%stream(2) = 2097152 
   F_qh = .TRUE.
   IF ( scalar_index_table( PARAM_qndrop , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qndrop = scalar_num_table(idomain)
     scalar_index_table( PARAM_qndrop , idomain ) = P_qndrop
   ELSE
     P_qndrop = scalar_index_table( PARAM_qndrop , idomain )
   END IF
   scalar_boundary_table( idomain, P_qndrop ) = .TRUE.
   scalar_dname_table( idomain, P_qndrop ) = 'QNDROP'
   scalar_desc_table( idomain, P_qndrop ) = 'Droplet number mixing ratio'
   scalar_units_table( idomain, P_qndrop ) = '  kg-1'
   scalar_streams_table( idomain, P_qndrop )%stream(1) = 100663309 
   scalar_streams_table( idomain, P_qndrop )%stream(2) = 2097152 
   F_qndrop = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnr ) = .TRUE.
   scalar_dname_table( idomain, P_qnr ) = 'QNRAIN'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnr )%stream(1) = 100663309 
   scalar_streams_table( idomain, P_qnr )%stream(2) = 2097152 
   F_qnr = .TRUE.
   IF ( scalar_index_table( PARAM_qni , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qni = scalar_num_table(idomain)
     scalar_index_table( PARAM_qni , idomain ) = P_qni
   ELSE
     P_qni = scalar_index_table( PARAM_qni , idomain )
   END IF
   scalar_boundary_table( idomain, P_qni ) = .TRUE.
   scalar_dname_table( idomain, P_qni ) = 'QNICE'
   scalar_desc_table( idomain, P_qni ) = 'Ice Number concentration'
   scalar_units_table( idomain, P_qni ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qni )%stream(1) = 100663309 
   scalar_streams_table( idomain, P_qni )%stream(2) = 2097152 
   F_qni = .TRUE.
   IF ( scalar_index_table( PARAM_qns , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qns = scalar_num_table(idomain)
     scalar_index_table( PARAM_qns , idomain ) = P_qns
   ELSE
     P_qns = scalar_index_table( PARAM_qns , idomain )
   END IF
   scalar_boundary_table( idomain, P_qns ) = .TRUE.
   scalar_dname_table( idomain, P_qns ) = 'QNSNOW'
   scalar_desc_table( idomain, P_qns ) = 'Snow Number concentration'
   scalar_units_table( idomain, P_qns ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qns )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qns )%stream(2) = 2097152 
   F_qns = .TRUE.
   IF ( scalar_index_table( PARAM_qng , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qng = scalar_num_table(idomain)
     scalar_index_table( PARAM_qng , idomain ) = P_qng
   ELSE
     P_qng = scalar_index_table( PARAM_qng , idomain )
   END IF
   scalar_boundary_table( idomain, P_qng ) = .TRUE.
   scalar_dname_table( idomain, P_qng ) = 'QNGRAUP'
   scalar_desc_table( idomain, P_qng ) = 'Graupel Number concentration'
   scalar_units_table( idomain, P_qng ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qng )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qng )%stream(2) = 2097152 
   F_qng = .TRUE.
   IF ( scalar_index_table( PARAM_qnh , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnh = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnh , idomain ) = P_qnh
   ELSE
     P_qnh = scalar_index_table( PARAM_qnh , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnh ) = .TRUE.
   scalar_dname_table( idomain, P_qnh ) = 'QNHAIL'
   scalar_desc_table( idomain, P_qnh ) = 'Hail Number concentration'
   scalar_units_table( idomain, P_qnh ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnh )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qnh )%stream(2) = 2097152 
   F_qnh = .TRUE.
   IF ( scalar_index_table( PARAM_qvolg , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qvolg = scalar_num_table(idomain)
     scalar_index_table( PARAM_qvolg , idomain ) = P_qvolg
   ELSE
     P_qvolg = scalar_index_table( PARAM_qvolg , idomain )
   END IF
   scalar_boundary_table( idomain, P_qvolg ) = .TRUE.
   scalar_dname_table( idomain, P_qvolg ) = 'QVGRAUPEL'
   scalar_desc_table( idomain, P_qvolg ) = 'Graupel Particle Volume'
   scalar_units_table( idomain, P_qvolg ) = 'm(3) kg(-1)'
   scalar_streams_table( idomain, P_qvolg )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qvolg )%stream(2) = 2097152 
   F_qvolg = .TRUE.
   IF ( scalar_index_table( PARAM_qvolh , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qvolh = scalar_num_table(idomain)
     scalar_index_table( PARAM_qvolh , idomain ) = P_qvolh
   ELSE
     P_qvolh = scalar_index_table( PARAM_qvolh , idomain )
   END IF
   scalar_boundary_table( idomain, P_qvolh ) = .TRUE.
   scalar_dname_table( idomain, P_qvolh ) = '0)'
   scalar_desc_table( idomain, P_qvolh ) = 'QVHAIL'
   scalar_units_table( idomain, P_qvolh ) = 'Hail Particle Volume'
   scalar_streams_table( idomain, P_qvolh )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qvolh )%stream(2) = 2097152 
   F_qvolh = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==18)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 
   F_qg = .TRUE.
   IF ( moist_index_table( PARAM_qh , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qh = moist_num_table(idomain)
     moist_index_table( PARAM_qh , idomain ) = P_qh
   ELSE
     P_qh = moist_index_table( PARAM_qh , idomain )
   END IF
   moist_boundary_table( idomain, P_qh ) = .TRUE.
   moist_dname_table( idomain, P_qh ) = 'QHAIL'
   moist_desc_table( idomain, P_qh ) = 'Hail mixing ratio'
   moist_units_table( idomain, P_qh ) = 'kg kg-1'
   moist_streams_table( idomain, P_qh )%stream(1) = 1 
   moist_streams_table( idomain, P_qh )%stream(2) = 2097152 
   F_qh = .TRUE.
   IF ( scalar_index_table( PARAM_qnn , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnn = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnn , idomain ) = P_qnn
   ELSE
     P_qnn = scalar_index_table( PARAM_qnn , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnn ) = .TRUE.
   scalar_dname_table( idomain, P_qnn ) = 'QNCCN'
   scalar_desc_table( idomain, P_qnn ) = 'CCN Number concentration'
   scalar_units_table( idomain, P_qnn ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnn )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qnn )%stream(2) = 2097152 
   F_qnn = .TRUE.
   IF ( scalar_index_table( PARAM_qndrop , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qndrop = scalar_num_table(idomain)
     scalar_index_table( PARAM_qndrop , idomain ) = P_qndrop
   ELSE
     P_qndrop = scalar_index_table( PARAM_qndrop , idomain )
   END IF
   scalar_boundary_table( idomain, P_qndrop ) = .TRUE.
   scalar_dname_table( idomain, P_qndrop ) = 'QNDROP'
   scalar_desc_table( idomain, P_qndrop ) = 'Droplet number mixing ratio'
   scalar_units_table( idomain, P_qndrop ) = '  kg-1'
   scalar_streams_table( idomain, P_qndrop )%stream(1) = 100663309 
   scalar_streams_table( idomain, P_qndrop )%stream(2) = 2097152 
   F_qndrop = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnr ) = .TRUE.
   scalar_dname_table( idomain, P_qnr ) = 'QNRAIN'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnr )%stream(1) = 100663309 
   scalar_streams_table( idomain, P_qnr )%stream(2) = 2097152 
   F_qnr = .TRUE.
   IF ( scalar_index_table( PARAM_qni , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qni = scalar_num_table(idomain)
     scalar_index_table( PARAM_qni , idomain ) = P_qni
   ELSE
     P_qni = scalar_index_table( PARAM_qni , idomain )
   END IF
   scalar_boundary_table( idomain, P_qni ) = .TRUE.
   scalar_dname_table( idomain, P_qni ) = 'QNICE'
   scalar_desc_table( idomain, P_qni ) = 'Ice Number concentration'
   scalar_units_table( idomain, P_qni ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qni )%stream(1) = 100663309 
   scalar_streams_table( idomain, P_qni )%stream(2) = 2097152 
   F_qni = .TRUE.
   IF ( scalar_index_table( PARAM_qns , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qns = scalar_num_table(idomain)
     scalar_index_table( PARAM_qns , idomain ) = P_qns
   ELSE
     P_qns = scalar_index_table( PARAM_qns , idomain )
   END IF
   scalar_boundary_table( idomain, P_qns ) = .TRUE.
   scalar_dname_table( idomain, P_qns ) = 'QNSNOW'
   scalar_desc_table( idomain, P_qns ) = 'Snow Number concentration'
   scalar_units_table( idomain, P_qns ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qns )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qns )%stream(2) = 2097152 
   F_qns = .TRUE.
   IF ( scalar_index_table( PARAM_qng , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qng = scalar_num_table(idomain)
     scalar_index_table( PARAM_qng , idomain ) = P_qng
   ELSE
     P_qng = scalar_index_table( PARAM_qng , idomain )
   END IF
   scalar_boundary_table( idomain, P_qng ) = .TRUE.
   scalar_dname_table( idomain, P_qng ) = 'QNGRAUP'
   scalar_desc_table( idomain, P_qng ) = 'Graupel Number concentration'
   scalar_units_table( idomain, P_qng ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qng )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qng )%stream(2) = 2097152 
   F_qng = .TRUE.
   IF ( scalar_index_table( PARAM_qnh , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnh = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnh , idomain ) = P_qnh
   ELSE
     P_qnh = scalar_index_table( PARAM_qnh , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnh ) = .TRUE.
   scalar_dname_table( idomain, P_qnh ) = 'QNHAIL'
   scalar_desc_table( idomain, P_qnh ) = 'Hail Number concentration'
   scalar_units_table( idomain, P_qnh ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnh )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qnh )%stream(2) = 2097152 
   F_qnh = .TRUE.
   IF ( scalar_index_table( PARAM_qvolg , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qvolg = scalar_num_table(idomain)
     scalar_index_table( PARAM_qvolg , idomain ) = P_qvolg
   ELSE
     P_qvolg = scalar_index_table( PARAM_qvolg , idomain )
   END IF
   scalar_boundary_table( idomain, P_qvolg ) = .TRUE.
   scalar_dname_table( idomain, P_qvolg ) = 'QVGRAUPEL'
   scalar_desc_table( idomain, P_qvolg ) = 'Graupel Particle Volume'
   scalar_units_table( idomain, P_qvolg ) = 'm(3) kg(-1)'
   scalar_streams_table( idomain, P_qvolg )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qvolg )%stream(2) = 2097152 
   F_qvolg = .TRUE.
   IF ( scalar_index_table( PARAM_qvolh , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qvolh = scalar_num_table(idomain)
     scalar_index_table( PARAM_qvolh , idomain ) = P_qvolh
   ELSE
     P_qvolh = scalar_index_table( PARAM_qvolh , idomain )
   END IF
   scalar_boundary_table( idomain, P_qvolh ) = .TRUE.
   scalar_dname_table( idomain, P_qvolh ) = '0)'
   scalar_desc_table( idomain, P_qvolh ) = 'QVHAIL'
   scalar_units_table( idomain, P_qvolh ) = 'Hail Particle Volume'
   scalar_streams_table( idomain, P_qvolh )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qvolh )%stream(2) = 2097152 
   F_qvolh = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==19)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 
   F_qg = .TRUE.
   IF ( moist_index_table( PARAM_qh , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qh = moist_num_table(idomain)
     moist_index_table( PARAM_qh , idomain ) = P_qh
   ELSE
     P_qh = moist_index_table( PARAM_qh , idomain )
   END IF
   moist_boundary_table( idomain, P_qh ) = .TRUE.
   moist_dname_table( idomain, P_qh ) = 'QHAIL'
   moist_desc_table( idomain, P_qh ) = 'Hail mixing ratio'
   moist_units_table( idomain, P_qh ) = 'kg kg-1'
   moist_streams_table( idomain, P_qh )%stream(1) = 1 
   moist_streams_table( idomain, P_qh )%stream(2) = 2097152 
   F_qh = .TRUE.
   IF ( scalar_index_table( PARAM_qvolg , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qvolg = scalar_num_table(idomain)
     scalar_index_table( PARAM_qvolg , idomain ) = P_qvolg
   ELSE
     P_qvolg = scalar_index_table( PARAM_qvolg , idomain )
   END IF
   scalar_boundary_table( idomain, P_qvolg ) = .TRUE.
   scalar_dname_table( idomain, P_qvolg ) = 'QVGRAUPEL'
   scalar_desc_table( idomain, P_qvolg ) = 'Graupel Particle Volume'
   scalar_units_table( idomain, P_qvolg ) = 'm(3) kg(-1)'
   scalar_streams_table( idomain, P_qvolg )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qvolg )%stream(2) = 2097152 
   F_qvolg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==21)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 
   F_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==22)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 
   F_qg = .TRUE.
   IF ( scalar_index_table( PARAM_qndrop , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qndrop = scalar_num_table(idomain)
     scalar_index_table( PARAM_qndrop , idomain ) = P_qndrop
   ELSE
     P_qndrop = scalar_index_table( PARAM_qndrop , idomain )
   END IF
   scalar_boundary_table( idomain, P_qndrop ) = .TRUE.
   scalar_dname_table( idomain, P_qndrop ) = 'QNDROP'
   scalar_desc_table( idomain, P_qndrop ) = 'Droplet number mixing ratio'
   scalar_units_table( idomain, P_qndrop ) = '  kg-1'
   scalar_streams_table( idomain, P_qndrop )%stream(1) = 100663309 
   scalar_streams_table( idomain, P_qndrop )%stream(2) = 2097152 
   F_qndrop = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnr ) = .TRUE.
   scalar_dname_table( idomain, P_qnr ) = 'QNRAIN'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnr )%stream(1) = 100663309 
   scalar_streams_table( idomain, P_qnr )%stream(2) = 2097152 
   F_qnr = .TRUE.
   IF ( scalar_index_table( PARAM_qni , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qni = scalar_num_table(idomain)
     scalar_index_table( PARAM_qni , idomain ) = P_qni
   ELSE
     P_qni = scalar_index_table( PARAM_qni , idomain )
   END IF
   scalar_boundary_table( idomain, P_qni ) = .TRUE.
   scalar_dname_table( idomain, P_qni ) = 'QNICE'
   scalar_desc_table( idomain, P_qni ) = 'Ice Number concentration'
   scalar_units_table( idomain, P_qni ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qni )%stream(1) = 100663309 
   scalar_streams_table( idomain, P_qni )%stream(2) = 2097152 
   F_qni = .TRUE.
   IF ( scalar_index_table( PARAM_qns , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qns = scalar_num_table(idomain)
     scalar_index_table( PARAM_qns , idomain ) = P_qns
   ELSE
     P_qns = scalar_index_table( PARAM_qns , idomain )
   END IF
   scalar_boundary_table( idomain, P_qns ) = .TRUE.
   scalar_dname_table( idomain, P_qns ) = 'QNSNOW'
   scalar_desc_table( idomain, P_qns ) = 'Snow Number concentration'
   scalar_units_table( idomain, P_qns ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qns )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qns )%stream(2) = 2097152 
   F_qns = .TRUE.
   IF ( scalar_index_table( PARAM_qng , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qng = scalar_num_table(idomain)
     scalar_index_table( PARAM_qng , idomain ) = P_qng
   ELSE
     P_qng = scalar_index_table( PARAM_qng , idomain )
   END IF
   scalar_boundary_table( idomain, P_qng ) = .TRUE.
   scalar_dname_table( idomain, P_qng ) = 'QNGRAUP'
   scalar_desc_table( idomain, P_qng ) = 'Graupel Number concentration'
   scalar_units_table( idomain, P_qng ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qng )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qng )%stream(2) = 2097152 
   F_qng = .TRUE.
   IF ( scalar_index_table( PARAM_qvolg , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qvolg = scalar_num_table(idomain)
     scalar_index_table( PARAM_qvolg , idomain ) = P_qvolg
   ELSE
     P_qvolg = scalar_index_table( PARAM_qvolg , idomain )
   END IF
   scalar_boundary_table( idomain, P_qvolg ) = .TRUE.
   scalar_dname_table( idomain, P_qvolg ) = 'QVGRAUPEL'
   scalar_desc_table( idomain, P_qvolg ) = 'Graupel Particle Volume'
   scalar_units_table( idomain, P_qvolg ) = 'm(3) kg(-1)'
   scalar_streams_table( idomain, P_qvolg )%stream(1) = 100663297 
   scalar_streams_table( idomain, P_qvolg )%stream(2) = 2097152 
   F_qvolg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==85)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 
   F_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==95)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 
   F_qs = .TRUE.
  END IF
  IF (model_config_rec%compute_radar_ref==1)THEN
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==-1)THEN
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==0)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 
   F_dfi_qv = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==1)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 
   F_dfi_qr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==2)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qg ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(2) = 2097152 
   F_dfi_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==3)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 
   F_dfi_qr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==4)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 
   F_dfi_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==5)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 
   F_dfi_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==6)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qg ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(2) = 2097152 
   F_dfi_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==7)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qg ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(2) = 2097152 
   F_dfi_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==8)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qg ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(2) = 2097152 
   F_dfi_qg = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qni , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qni = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qni , idomain ) = P_dfi_qni
   ELSE
     P_dfi_qni = dfi_scalar_index_table( PARAM_dfi_qni , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qni ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qni ) = 'DFI_QNICE'
   dfi_scalar_desc_table( idomain, P_dfi_qni ) = 'DFI Ice Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qni ) = '  kg-1'
   dfi_scalar_streams_table( idomain, P_dfi_qni )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qni )%stream(2) = 2097152 
   F_dfi_qni = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnr = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) = P_dfi_qnr
   ELSE
     P_dfi_qnr = dfi_scalar_index_table( PARAM_dfi_qnr , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnr ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnr ) = 'DFI_QNRAIN'
   dfi_scalar_desc_table( idomain, P_dfi_qnr ) = 'DFI Rain Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnr ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(2) = 2097152 
   F_dfi_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==28)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qg ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(2) = 2097152 
   F_dfi_qg = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qni , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qni = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qni , idomain ) = P_dfi_qni
   ELSE
     P_dfi_qni = dfi_scalar_index_table( PARAM_dfi_qni , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qni ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qni ) = 'DFI_QNICE'
   dfi_scalar_desc_table( idomain, P_dfi_qni ) = 'DFI Ice Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qni ) = '  kg-1'
   dfi_scalar_streams_table( idomain, P_dfi_qni )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qni )%stream(2) = 2097152 
   F_dfi_qni = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnr = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) = P_dfi_qnr
   ELSE
     P_dfi_qnr = dfi_scalar_index_table( PARAM_dfi_qnr , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnr ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnr ) = 'DFI_QNRAIN'
   dfi_scalar_desc_table( idomain, P_dfi_qnr ) = 'DFI Rain Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnr ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(2) = 2097152 
   F_dfi_qnr = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnc = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) = P_dfi_qnc
   ELSE
     P_dfi_qnc = dfi_scalar_index_table( PARAM_dfi_qnc , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnc ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnc ) = 'DFI_QNCLOUD'
   dfi_scalar_desc_table( idomain, P_dfi_qnc ) = 'DFI Cloud Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnc ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnc )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qnc )%stream(2) = 2097152 
   F_dfi_qnc = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnwfa , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnwfa = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnwfa , idomain ) = P_dfi_qnwfa
   ELSE
     P_dfi_qnwfa = dfi_scalar_index_table( PARAM_dfi_qnwfa , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnwfa ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnwfa ) = 'DFI_QNWFA'
   dfi_scalar_desc_table( idomain, P_dfi_qnwfa ) = 'DFI water-friendly aerosol number con'
   dfi_scalar_units_table( idomain, P_dfi_qnwfa ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnwfa )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qnwfa )%stream(2) = 2097152 
   F_dfi_qnwfa = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnifa , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnifa = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnifa , idomain ) = P_dfi_qnifa
   ELSE
     P_dfi_qnifa = dfi_scalar_index_table( PARAM_dfi_qnifa , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnifa ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnifa ) = 'DFI_QNIFA'
   dfi_scalar_desc_table( idomain, P_dfi_qnifa ) = 'DFI ice-friendly aerosol number con'
   dfi_scalar_units_table( idomain, P_dfi_qnifa ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnifa )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qnifa )%stream(2) = 2097152 
   F_dfi_qnifa = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==9)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qg ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(2) = 2097152 
   F_dfi_qg = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qh , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qh = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qh , idomain ) = P_dfi_qh
   ELSE
     P_dfi_qh = dfi_moist_index_table( PARAM_dfi_qh , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qh ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qh ) = 'QHAIL'
   dfi_moist_desc_table( idomain, P_dfi_qh ) = 'Hail mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qh ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qh )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qh )%stream(2) = 2097152 
   F_dfi_qh = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnc = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) = P_dfi_qnc
   ELSE
     P_dfi_qnc = dfi_scalar_index_table( PARAM_dfi_qnc , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnc ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnc ) = 'DFI_QNCLOUD'
   dfi_scalar_desc_table( idomain, P_dfi_qnc ) = 'DFI Cloud Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnc ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnc )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qnc )%stream(2) = 2097152 
   F_dfi_qnc = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnr = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) = P_dfi_qnr
   ELSE
     P_dfi_qnr = dfi_scalar_index_table( PARAM_dfi_qnr , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnr ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnr ) = 'DFI_QNRAIN'
   dfi_scalar_desc_table( idomain, P_dfi_qnr ) = 'DFI Rain Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnr ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(2) = 2097152 
   F_dfi_qnr = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qni , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qni = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qni , idomain ) = P_dfi_qni
   ELSE
     P_dfi_qni = dfi_scalar_index_table( PARAM_dfi_qni , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qni ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qni ) = 'DFI_QNICE'
   dfi_scalar_desc_table( idomain, P_dfi_qni ) = 'DFI Ice Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qni ) = '  kg-1'
   dfi_scalar_streams_table( idomain, P_dfi_qni )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qni )%stream(2) = 2097152 
   F_dfi_qni = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qns , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qns = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qns , idomain ) = P_dfi_qns
   ELSE
     P_dfi_qns = dfi_scalar_index_table( PARAM_dfi_qns , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qns ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qns ) = 'DFI_QNSNOW'
   dfi_scalar_desc_table( idomain, P_dfi_qns ) = 'DFI Snow Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qns ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qns )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qns )%stream(2) = 2097152 
   F_dfi_qns = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qng , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qng = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qng , idomain ) = P_dfi_qng
   ELSE
     P_dfi_qng = dfi_scalar_index_table( PARAM_dfi_qng , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qng ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qng ) = 'DFI_QNGRAUPEL'
   dfi_scalar_desc_table( idomain, P_dfi_qng ) = 'DFI Graupel Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qng ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qng )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qng )%stream(2) = 2097152 
   F_dfi_qng = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnh , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnh = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnh , idomain ) = P_dfi_qnh
   ELSE
     P_dfi_qnh = dfi_scalar_index_table( PARAM_dfi_qnh , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnh ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnh ) = 'QNHAIL'
   dfi_scalar_desc_table( idomain, P_dfi_qnh ) = 'Hail Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnh ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnh )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qnh )%stream(2) = 2097152 
   F_dfi_qnh = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==10)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qg ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(2) = 2097152 
   F_dfi_qg = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qni , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qni = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qni , idomain ) = P_dfi_qni
   ELSE
     P_dfi_qni = dfi_scalar_index_table( PARAM_dfi_qni , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qni ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qni ) = 'DFI_QNICE'
   dfi_scalar_desc_table( idomain, P_dfi_qni ) = 'DFI Ice Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qni ) = '  kg-1'
   dfi_scalar_streams_table( idomain, P_dfi_qni )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qni )%stream(2) = 2097152 
   F_dfi_qni = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qns , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qns = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qns , idomain ) = P_dfi_qns
   ELSE
     P_dfi_qns = dfi_scalar_index_table( PARAM_dfi_qns , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qns ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qns ) = 'DFI_QNSNOW'
   dfi_scalar_desc_table( idomain, P_dfi_qns ) = 'DFI Snow Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qns ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qns )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qns )%stream(2) = 2097152 
   F_dfi_qns = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnr = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) = P_dfi_qnr
   ELSE
     P_dfi_qnr = dfi_scalar_index_table( PARAM_dfi_qnr , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnr ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnr ) = 'DFI_QNRAIN'
   dfi_scalar_desc_table( idomain, P_dfi_qnr ) = 'DFI Rain Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnr ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(2) = 2097152 
   F_dfi_qnr = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qng , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qng = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qng , idomain ) = P_dfi_qng
   ELSE
     P_dfi_qng = dfi_scalar_index_table( PARAM_dfi_qng , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qng ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qng ) = 'DFI_QNGRAUPEL'
   dfi_scalar_desc_table( idomain, P_dfi_qng ) = 'DFI Graupel Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qng ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qng )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qng )%stream(2) = 2097152 
   F_dfi_qng = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==13)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 
   F_dfi_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==14)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 
   F_dfi_qs = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnn , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnn = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnn , idomain ) = P_dfi_qnn
   ELSE
     P_dfi_qnn = dfi_scalar_index_table( PARAM_dfi_qnn , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnn ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnn ) = 'DFI_QNCC'
   dfi_scalar_desc_table( idomain, P_dfi_qnn ) = 'DFI CNN Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnn ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnn )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qnn )%stream(2) = 2097152 
   F_dfi_qnn = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnc = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) = P_dfi_qnc
   ELSE
     P_dfi_qnc = dfi_scalar_index_table( PARAM_dfi_qnc , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnc ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnc ) = 'DFI_QNCLOUD'
   dfi_scalar_desc_table( idomain, P_dfi_qnc ) = 'DFI Cloud Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnc ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnc )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qnc )%stream(2) = 2097152 
   F_dfi_qnc = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnr = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) = P_dfi_qnr
   ELSE
     P_dfi_qnr = dfi_scalar_index_table( PARAM_dfi_qnr , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnr ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnr ) = 'DFI_QNRAIN'
   dfi_scalar_desc_table( idomain, P_dfi_qnr ) = 'DFI Rain Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnr ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(2) = 2097152 
   F_dfi_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==16)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qg ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(2) = 2097152 
   F_dfi_qg = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnn , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnn = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnn , idomain ) = P_dfi_qnn
   ELSE
     P_dfi_qnn = dfi_scalar_index_table( PARAM_dfi_qnn , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnn ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnn ) = 'DFI_QNCC'
   dfi_scalar_desc_table( idomain, P_dfi_qnn ) = 'DFI CNN Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnn ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnn )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qnn )%stream(2) = 2097152 
   F_dfi_qnn = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnc = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) = P_dfi_qnc
   ELSE
     P_dfi_qnc = dfi_scalar_index_table( PARAM_dfi_qnc , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnc ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnc ) = 'DFI_QNCLOUD'
   dfi_scalar_desc_table( idomain, P_dfi_qnc ) = 'DFI Cloud Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnc ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnc )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qnc )%stream(2) = 2097152 
   F_dfi_qnc = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnr = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) = P_dfi_qnr
   ELSE
     P_dfi_qnr = dfi_scalar_index_table( PARAM_dfi_qnr , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnr ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnr ) = 'DFI_QNRAIN'
   dfi_scalar_desc_table( idomain, P_dfi_qnr ) = 'DFI Rain Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnr ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(1) = 0 
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(2) = 2097152 
   F_dfi_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==95)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 
   F_dfi_qs = .TRUE.
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==3)THEN
   IF ( ozmixm_index_table( PARAM_mth01 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth01 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth01 , idomain ) = P_mth01
   ELSE
     P_mth01 = ozmixm_index_table( PARAM_mth01 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth01 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth01 ) = 'OZMIXMTH01'
   ozmixm_desc_table( idomain, P_mth01 ) = 'Month 1 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth01 ) = '-'
   ozmixm_streams_table( idomain, P_mth01 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth01 )%stream(2) = 0 
   F_mth01 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth02 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth02 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth02 , idomain ) = P_mth02
   ELSE
     P_mth02 = ozmixm_index_table( PARAM_mth02 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth02 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth02 ) = 'OZMIXMTH02'
   ozmixm_desc_table( idomain, P_mth02 ) = 'Month 2 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth02 ) = '-'
   ozmixm_streams_table( idomain, P_mth02 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth02 )%stream(2) = 0 
   F_mth02 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth03 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth03 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth03 , idomain ) = P_mth03
   ELSE
     P_mth03 = ozmixm_index_table( PARAM_mth03 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth03 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth03 ) = 'OZMIXMTH03'
   ozmixm_desc_table( idomain, P_mth03 ) = 'Month 3 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth03 ) = '-'
   ozmixm_streams_table( idomain, P_mth03 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth03 )%stream(2) = 0 
   F_mth03 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth04 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth04 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth04 , idomain ) = P_mth04
   ELSE
     P_mth04 = ozmixm_index_table( PARAM_mth04 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth04 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth04 ) = 'OZMIXMTH04'
   ozmixm_desc_table( idomain, P_mth04 ) = 'Month 4 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth04 ) = '-'
   ozmixm_streams_table( idomain, P_mth04 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth04 )%stream(2) = 0 
   F_mth04 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth05 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth05 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth05 , idomain ) = P_mth05
   ELSE
     P_mth05 = ozmixm_index_table( PARAM_mth05 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth05 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth05 ) = 'OZMIXMTH05'
   ozmixm_desc_table( idomain, P_mth05 ) = 'Month 5 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth05 ) = '-'
   ozmixm_streams_table( idomain, P_mth05 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth05 )%stream(2) = 0 
   F_mth05 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth06 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth06 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth06 , idomain ) = P_mth06
   ELSE
     P_mth06 = ozmixm_index_table( PARAM_mth06 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth06 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth06 ) = 'OZMIXMTH06'
   ozmixm_desc_table( idomain, P_mth06 ) = 'Month 6 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth06 ) = '-'
   ozmixm_streams_table( idomain, P_mth06 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth06 )%stream(2) = 0 
   F_mth06 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth07 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth07 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth07 , idomain ) = P_mth07
   ELSE
     P_mth07 = ozmixm_index_table( PARAM_mth07 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth07 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth07 ) = 'OZMIXMTH07'
   ozmixm_desc_table( idomain, P_mth07 ) = 'Month 7 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth07 ) = '-'
   ozmixm_streams_table( idomain, P_mth07 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth07 )%stream(2) = 0 
   F_mth07 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth08 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth08 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth08 , idomain ) = P_mth08
   ELSE
     P_mth08 = ozmixm_index_table( PARAM_mth08 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth08 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth08 ) = 'OZMIXMTH08'
   ozmixm_desc_table( idomain, P_mth08 ) = 'Month 8 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth08 ) = '-'
   ozmixm_streams_table( idomain, P_mth08 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth08 )%stream(2) = 0 
   F_mth08 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth09 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth09 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth09 , idomain ) = P_mth09
   ELSE
     P_mth09 = ozmixm_index_table( PARAM_mth09 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth09 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth09 ) = 'OZMIXMTH09'
   ozmixm_desc_table( idomain, P_mth09 ) = 'Month 9 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth09 ) = '-'
   ozmixm_streams_table( idomain, P_mth09 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth09 )%stream(2) = 0 
   F_mth09 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth10 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth10 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth10 , idomain ) = P_mth10
   ELSE
     P_mth10 = ozmixm_index_table( PARAM_mth10 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth10 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth10 ) = 'OZMIXMTH10'
   ozmixm_desc_table( idomain, P_mth10 ) = 'Month 10 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth10 ) = '-'
   ozmixm_streams_table( idomain, P_mth10 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth10 )%stream(2) = 0 
   F_mth10 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth11 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth11 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth11 , idomain ) = P_mth11
   ELSE
     P_mth11 = ozmixm_index_table( PARAM_mth11 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth11 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth11 ) = 'OZMIXMTH11'
   ozmixm_desc_table( idomain, P_mth11 ) = 'Month 11 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth11 ) = '-'
   ozmixm_streams_table( idomain, P_mth11 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth11 )%stream(2) = 0 
   F_mth11 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth12 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth12 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth12 , idomain ) = P_mth12
   ELSE
     P_mth12 = ozmixm_index_table( PARAM_mth12 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth12 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth12 ) = 'OZMIXMTH12'
   ozmixm_desc_table( idomain, P_mth12 ) = 'Month 12 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth12 ) = '-'
   ozmixm_streams_table( idomain, P_mth12 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth12 )%stream(2) = 0 
   F_mth12 = .TRUE.
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==4)THEN
   IF ( ozmixm_index_table( PARAM_mth01 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth01 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth01 , idomain ) = P_mth01
   ELSE
     P_mth01 = ozmixm_index_table( PARAM_mth01 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth01 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth01 ) = 'OZMIXMTH01'
   ozmixm_desc_table( idomain, P_mth01 ) = 'Month 1 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth01 ) = '-'
   ozmixm_streams_table( idomain, P_mth01 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth01 )%stream(2) = 0 
   F_mth01 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth02 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth02 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth02 , idomain ) = P_mth02
   ELSE
     P_mth02 = ozmixm_index_table( PARAM_mth02 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth02 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth02 ) = 'OZMIXMTH02'
   ozmixm_desc_table( idomain, P_mth02 ) = 'Month 2 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth02 ) = '-'
   ozmixm_streams_table( idomain, P_mth02 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth02 )%stream(2) = 0 
   F_mth02 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth03 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth03 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth03 , idomain ) = P_mth03
   ELSE
     P_mth03 = ozmixm_index_table( PARAM_mth03 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth03 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth03 ) = 'OZMIXMTH03'
   ozmixm_desc_table( idomain, P_mth03 ) = 'Month 3 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth03 ) = '-'
   ozmixm_streams_table( idomain, P_mth03 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth03 )%stream(2) = 0 
   F_mth03 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth04 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth04 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth04 , idomain ) = P_mth04
   ELSE
     P_mth04 = ozmixm_index_table( PARAM_mth04 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth04 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth04 ) = 'OZMIXMTH04'
   ozmixm_desc_table( idomain, P_mth04 ) = 'Month 4 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth04 ) = '-'
   ozmixm_streams_table( idomain, P_mth04 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth04 )%stream(2) = 0 
   F_mth04 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth05 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth05 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth05 , idomain ) = P_mth05
   ELSE
     P_mth05 = ozmixm_index_table( PARAM_mth05 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth05 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth05 ) = 'OZMIXMTH05'
   ozmixm_desc_table( idomain, P_mth05 ) = 'Month 5 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth05 ) = '-'
   ozmixm_streams_table( idomain, P_mth05 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth05 )%stream(2) = 0 
   F_mth05 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth06 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth06 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth06 , idomain ) = P_mth06
   ELSE
     P_mth06 = ozmixm_index_table( PARAM_mth06 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth06 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth06 ) = 'OZMIXMTH06'
   ozmixm_desc_table( idomain, P_mth06 ) = 'Month 6 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth06 ) = '-'
   ozmixm_streams_table( idomain, P_mth06 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth06 )%stream(2) = 0 
   F_mth06 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth07 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth07 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth07 , idomain ) = P_mth07
   ELSE
     P_mth07 = ozmixm_index_table( PARAM_mth07 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth07 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth07 ) = 'OZMIXMTH07'
   ozmixm_desc_table( idomain, P_mth07 ) = 'Month 7 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth07 ) = '-'
   ozmixm_streams_table( idomain, P_mth07 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth07 )%stream(2) = 0 
   F_mth07 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth08 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth08 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth08 , idomain ) = P_mth08
   ELSE
     P_mth08 = ozmixm_index_table( PARAM_mth08 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth08 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth08 ) = 'OZMIXMTH08'
   ozmixm_desc_table( idomain, P_mth08 ) = 'Month 8 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth08 ) = '-'
   ozmixm_streams_table( idomain, P_mth08 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth08 )%stream(2) = 0 
   F_mth08 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth09 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth09 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth09 , idomain ) = P_mth09
   ELSE
     P_mth09 = ozmixm_index_table( PARAM_mth09 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth09 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth09 ) = 'OZMIXMTH09'
   ozmixm_desc_table( idomain, P_mth09 ) = 'Month 9 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth09 ) = '-'
   ozmixm_streams_table( idomain, P_mth09 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth09 )%stream(2) = 0 
   F_mth09 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth10 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth10 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth10 , idomain ) = P_mth10
   ELSE
     P_mth10 = ozmixm_index_table( PARAM_mth10 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth10 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth10 ) = 'OZMIXMTH10'
   ozmixm_desc_table( idomain, P_mth10 ) = 'Month 10 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth10 ) = '-'
   ozmixm_streams_table( idomain, P_mth10 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth10 )%stream(2) = 0 
   F_mth10 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth11 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth11 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth11 , idomain ) = P_mth11
   ELSE
     P_mth11 = ozmixm_index_table( PARAM_mth11 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth11 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth11 ) = 'OZMIXMTH11'
   ozmixm_desc_table( idomain, P_mth11 ) = 'Month 11 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth11 ) = '-'
   ozmixm_streams_table( idomain, P_mth11 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth11 )%stream(2) = 0 
   F_mth11 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth12 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth12 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth12 , idomain ) = P_mth12
   ELSE
     P_mth12 = ozmixm_index_table( PARAM_mth12 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth12 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth12 ) = 'OZMIXMTH12'
   ozmixm_desc_table( idomain, P_mth12 ) = 'Month 12 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth12 ) = '-'
   ozmixm_streams_table( idomain, P_mth12 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth12 )%stream(2) = 0 
   F_mth12 = .TRUE.
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==24)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==5)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==7)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==98)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==3)THEN
   IF ( ozmixm_index_table( PARAM_mth01 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth01 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth01 , idomain ) = P_mth01
   ELSE
     P_mth01 = ozmixm_index_table( PARAM_mth01 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth01 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth01 ) = 'OZMIXMTH01'
   ozmixm_desc_table( idomain, P_mth01 ) = 'Month 1 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth01 ) = '-'
   ozmixm_streams_table( idomain, P_mth01 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth01 )%stream(2) = 0 
   F_mth01 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth02 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth02 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth02 , idomain ) = P_mth02
   ELSE
     P_mth02 = ozmixm_index_table( PARAM_mth02 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth02 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth02 ) = 'OZMIXMTH02'
   ozmixm_desc_table( idomain, P_mth02 ) = 'Month 2 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth02 ) = '-'
   ozmixm_streams_table( idomain, P_mth02 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth02 )%stream(2) = 0 
   F_mth02 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth03 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth03 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth03 , idomain ) = P_mth03
   ELSE
     P_mth03 = ozmixm_index_table( PARAM_mth03 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth03 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth03 ) = 'OZMIXMTH03'
   ozmixm_desc_table( idomain, P_mth03 ) = 'Month 3 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth03 ) = '-'
   ozmixm_streams_table( idomain, P_mth03 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth03 )%stream(2) = 0 
   F_mth03 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth04 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth04 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth04 , idomain ) = P_mth04
   ELSE
     P_mth04 = ozmixm_index_table( PARAM_mth04 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth04 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth04 ) = 'OZMIXMTH04'
   ozmixm_desc_table( idomain, P_mth04 ) = 'Month 4 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth04 ) = '-'
   ozmixm_streams_table( idomain, P_mth04 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth04 )%stream(2) = 0 
   F_mth04 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth05 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth05 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth05 , idomain ) = P_mth05
   ELSE
     P_mth05 = ozmixm_index_table( PARAM_mth05 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth05 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth05 ) = 'OZMIXMTH05'
   ozmixm_desc_table( idomain, P_mth05 ) = 'Month 5 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth05 ) = '-'
   ozmixm_streams_table( idomain, P_mth05 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth05 )%stream(2) = 0 
   F_mth05 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth06 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth06 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth06 , idomain ) = P_mth06
   ELSE
     P_mth06 = ozmixm_index_table( PARAM_mth06 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth06 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth06 ) = 'OZMIXMTH06'
   ozmixm_desc_table( idomain, P_mth06 ) = 'Month 6 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth06 ) = '-'
   ozmixm_streams_table( idomain, P_mth06 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth06 )%stream(2) = 0 
   F_mth06 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth07 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth07 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth07 , idomain ) = P_mth07
   ELSE
     P_mth07 = ozmixm_index_table( PARAM_mth07 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth07 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth07 ) = 'OZMIXMTH07'
   ozmixm_desc_table( idomain, P_mth07 ) = 'Month 7 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth07 ) = '-'
   ozmixm_streams_table( idomain, P_mth07 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth07 )%stream(2) = 0 
   F_mth07 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth08 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth08 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth08 , idomain ) = P_mth08
   ELSE
     P_mth08 = ozmixm_index_table( PARAM_mth08 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth08 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth08 ) = 'OZMIXMTH08'
   ozmixm_desc_table( idomain, P_mth08 ) = 'Month 8 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth08 ) = '-'
   ozmixm_streams_table( idomain, P_mth08 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth08 )%stream(2) = 0 
   F_mth08 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth09 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth09 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth09 , idomain ) = P_mth09
   ELSE
     P_mth09 = ozmixm_index_table( PARAM_mth09 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth09 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth09 ) = 'OZMIXMTH09'
   ozmixm_desc_table( idomain, P_mth09 ) = 'Month 9 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth09 ) = '-'
   ozmixm_streams_table( idomain, P_mth09 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth09 )%stream(2) = 0 
   F_mth09 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth10 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth10 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth10 , idomain ) = P_mth10
   ELSE
     P_mth10 = ozmixm_index_table( PARAM_mth10 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth10 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth10 ) = 'OZMIXMTH10'
   ozmixm_desc_table( idomain, P_mth10 ) = 'Month 10 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth10 ) = '-'
   ozmixm_streams_table( idomain, P_mth10 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth10 )%stream(2) = 0 
   F_mth10 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth11 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth11 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth11 , idomain ) = P_mth11
   ELSE
     P_mth11 = ozmixm_index_table( PARAM_mth11 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth11 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth11 ) = 'OZMIXMTH11'
   ozmixm_desc_table( idomain, P_mth11 ) = 'Month 11 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth11 ) = '-'
   ozmixm_streams_table( idomain, P_mth11 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth11 )%stream(2) = 0 
   F_mth11 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth12 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth12 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth12 , idomain ) = P_mth12
   ELSE
     P_mth12 = ozmixm_index_table( PARAM_mth12 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth12 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth12 ) = 'OZMIXMTH12'
   ozmixm_desc_table( idomain, P_mth12 ) = 'Month 12 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth12 ) = '-'
   ozmixm_streams_table( idomain, P_mth12 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth12 )%stream(2) = 0 
   F_mth12 = .TRUE.
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==4)THEN
   IF ( ozmixm_index_table( PARAM_mth01 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth01 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth01 , idomain ) = P_mth01
   ELSE
     P_mth01 = ozmixm_index_table( PARAM_mth01 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth01 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth01 ) = 'OZMIXMTH01'
   ozmixm_desc_table( idomain, P_mth01 ) = 'Month 1 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth01 ) = '-'
   ozmixm_streams_table( idomain, P_mth01 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth01 )%stream(2) = 0 
   F_mth01 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth02 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth02 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth02 , idomain ) = P_mth02
   ELSE
     P_mth02 = ozmixm_index_table( PARAM_mth02 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth02 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth02 ) = 'OZMIXMTH02'
   ozmixm_desc_table( idomain, P_mth02 ) = 'Month 2 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth02 ) = '-'
   ozmixm_streams_table( idomain, P_mth02 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth02 )%stream(2) = 0 
   F_mth02 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth03 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth03 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth03 , idomain ) = P_mth03
   ELSE
     P_mth03 = ozmixm_index_table( PARAM_mth03 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth03 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth03 ) = 'OZMIXMTH03'
   ozmixm_desc_table( idomain, P_mth03 ) = 'Month 3 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth03 ) = '-'
   ozmixm_streams_table( idomain, P_mth03 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth03 )%stream(2) = 0 
   F_mth03 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth04 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth04 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth04 , idomain ) = P_mth04
   ELSE
     P_mth04 = ozmixm_index_table( PARAM_mth04 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth04 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth04 ) = 'OZMIXMTH04'
   ozmixm_desc_table( idomain, P_mth04 ) = 'Month 4 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth04 ) = '-'
   ozmixm_streams_table( idomain, P_mth04 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth04 )%stream(2) = 0 
   F_mth04 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth05 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth05 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth05 , idomain ) = P_mth05
   ELSE
     P_mth05 = ozmixm_index_table( PARAM_mth05 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth05 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth05 ) = 'OZMIXMTH05'
   ozmixm_desc_table( idomain, P_mth05 ) = 'Month 5 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth05 ) = '-'
   ozmixm_streams_table( idomain, P_mth05 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth05 )%stream(2) = 0 
   F_mth05 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth06 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth06 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth06 , idomain ) = P_mth06
   ELSE
     P_mth06 = ozmixm_index_table( PARAM_mth06 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth06 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth06 ) = 'OZMIXMTH06'
   ozmixm_desc_table( idomain, P_mth06 ) = 'Month 6 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth06 ) = '-'
   ozmixm_streams_table( idomain, P_mth06 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth06 )%stream(2) = 0 
   F_mth06 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth07 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth07 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth07 , idomain ) = P_mth07
   ELSE
     P_mth07 = ozmixm_index_table( PARAM_mth07 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth07 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth07 ) = 'OZMIXMTH07'
   ozmixm_desc_table( idomain, P_mth07 ) = 'Month 7 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth07 ) = '-'
   ozmixm_streams_table( idomain, P_mth07 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth07 )%stream(2) = 0 
   F_mth07 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth08 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth08 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth08 , idomain ) = P_mth08
   ELSE
     P_mth08 = ozmixm_index_table( PARAM_mth08 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth08 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth08 ) = 'OZMIXMTH08'
   ozmixm_desc_table( idomain, P_mth08 ) = 'Month 8 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth08 ) = '-'
   ozmixm_streams_table( idomain, P_mth08 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth08 )%stream(2) = 0 
   F_mth08 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth09 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth09 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth09 , idomain ) = P_mth09
   ELSE
     P_mth09 = ozmixm_index_table( PARAM_mth09 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth09 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth09 ) = 'OZMIXMTH09'
   ozmixm_desc_table( idomain, P_mth09 ) = 'Month 9 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth09 ) = '-'
   ozmixm_streams_table( idomain, P_mth09 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth09 )%stream(2) = 0 
   F_mth09 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth10 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth10 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth10 , idomain ) = P_mth10
   ELSE
     P_mth10 = ozmixm_index_table( PARAM_mth10 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth10 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth10 ) = 'OZMIXMTH10'
   ozmixm_desc_table( idomain, P_mth10 ) = 'Month 10 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth10 ) = '-'
   ozmixm_streams_table( idomain, P_mth10 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth10 )%stream(2) = 0 
   F_mth10 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth11 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth11 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth11 , idomain ) = P_mth11
   ELSE
     P_mth11 = ozmixm_index_table( PARAM_mth11 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth11 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth11 ) = 'OZMIXMTH11'
   ozmixm_desc_table( idomain, P_mth11 ) = 'Month 11 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth11 ) = '-'
   ozmixm_streams_table( idomain, P_mth11 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth11 )%stream(2) = 0 
   F_mth11 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth12 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth12 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth12 , idomain ) = P_mth12
   ELSE
     P_mth12 = ozmixm_index_table( PARAM_mth12 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth12 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth12 ) = 'OZMIXMTH12'
   ozmixm_desc_table( idomain, P_mth12 ) = 'Month 12 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth12 ) = '-'
   ozmixm_streams_table( idomain, P_mth12 )%stream(1) = 0 
   ozmixm_streams_table( idomain, P_mth12 )%stream(2) = 0 
   F_mth12 = .TRUE.
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==24)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==5)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==7)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==98)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==31)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==91)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==88)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==4)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==7)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==10)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==89)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==12)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==4)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==5)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==88)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==7)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==8)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==83)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==4)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==7)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==8)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==9)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==10)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==11)THEN
  END IF
  IF (model_config_rec%windfarm_opt(idomain)==1)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==93)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==84)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==85)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==4)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==5)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==7)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==10)THEN
  END IF
  IF (model_config_rec%cu_diag(idomain)==1)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==6)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==16)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==14)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%shcu_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%shcu_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%shcu_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%dfi_stage==0)THEN
  END IF
  IF (model_config_rec%dfi_stage==1)THEN
  END IF
  IF (model_config_rec%dfi_stage==2)THEN
  END IF
  IF (model_config_rec%dfi_stage==3)THEN
  END IF
  IF (model_config_rec%dfi_stage==4)THEN
  END IF
  IF (model_config_rec%dfi_stage==5)THEN
  END IF
  IF (model_config_rec%dfi_opt==0)THEN
  END IF
  IF (model_config_rec%dfi_opt==1)THEN
  END IF
  IF (model_config_rec%dfi_opt==2)THEN
  END IF
  IF (model_config_rec%dfi_opt==3)THEN
  END IF
  IF (model_config_rec%seaice_albedo_opt==0)THEN
  END IF
  IF (model_config_rec%seaice_albedo_opt==1)THEN
  END IF
  IF (model_config_rec%seaice_albedo_opt==2)THEN
  END IF
  IF (model_config_rec%seaice_snowdepth_opt==0)THEN
  END IF
  IF (model_config_rec%seaice_snowdepth_opt==1)THEN
  END IF
  IF (model_config_rec%seaice_thickness_opt==0)THEN
  END IF
  IF (model_config_rec%seaice_thickness_opt==1)THEN
  END IF
  IF (model_config_rec%io_form_restart==1)THEN
  END IF
  IF (model_config_rec%io_form_restart==2)THEN
  END IF
  IF (model_config_rec%io_form_restart==3)THEN
  END IF
  IF (model_config_rec%io_form_restart==4)THEN
  END IF
  IF (model_config_rec%io_form_restart==5)THEN
  END IF
  IF (model_config_rec%io_form_restart==6)THEN
  END IF
  IF (model_config_rec%io_form_restart==7)THEN
  END IF
  IF (model_config_rec%io_form_restart==8)THEN
  END IF
  IF (model_config_rec%io_form_restart==9)THEN
  END IF
  IF (model_config_rec%io_form_restart==10)THEN
  END IF
  IF (model_config_rec%io_form_restart==11)THEN
  END IF
  IF (model_config_rec%io_form_restart==12)THEN
  END IF
  IF (model_config_rec%wrf_hydro==0)THEN
  END IF
  IF (model_config_rec%wrf_hydro==1)THEN
  END IF







  num_szj = szj_num_table( idomain )
  num_s1z = s1z_num_table( idomain )
  num_spz = spz_num_table( idomain )
  num_tcs = tcs_num_table( idomain )
  num_moist = moist_num_table( idomain )
  num_dfi_moist = dfi_moist_num_table( idomain )
  num_scalar = scalar_num_table( idomain )
  num_dfi_scalar = dfi_scalar_num_table( idomain )
  num_chem = chem_num_table( idomain )
  num_ozmixm = ozmixm_num_table( idomain )

  RETURN
END SUBROUTINE set_scalar_indices_from_config

