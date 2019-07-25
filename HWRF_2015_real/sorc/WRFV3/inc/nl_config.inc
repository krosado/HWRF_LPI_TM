!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nl_config.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
#ifdef NL_get_ROUTINES
#if (NNN == 0)
SUBROUTINE nl_get_lakedepth_default ( id_id , lakedepth_default )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: lakedepth_default
  INTEGER id_id
  lakedepth_default = model_config_rec%lakedepth_default(id_id)
  RETURN
END SUBROUTINE nl_get_lakedepth_default
SUBROUTINE nl_get_lake_min_elev ( id_id , lake_min_elev )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: lake_min_elev
  INTEGER id_id
  lake_min_elev = model_config_rec%lake_min_elev(id_id)
  RETURN
END SUBROUTINE nl_get_lake_min_elev
SUBROUTINE nl_get_use_lakedepth ( id_id , use_lakedepth )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: use_lakedepth
  INTEGER id_id
  use_lakedepth = model_config_rec%use_lakedepth(id_id)
  RETURN
END SUBROUTINE nl_get_use_lakedepth
SUBROUTINE nl_get_halo_debug ( id_id , halo_debug )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: halo_debug
  INTEGER id_id
  halo_debug = model_config_rec%halo_debug
  RETURN
END SUBROUTINE nl_get_halo_debug
SUBROUTINE nl_get_ntracers ( id_id , ntracers )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ntracers
  INTEGER id_id
  ntracers = model_config_rec%ntracers
  RETURN
END SUBROUTINE nl_get_ntracers
SUBROUTINE nl_get_vortex_tracker ( id_id , vortex_tracker )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: vortex_tracker
  INTEGER id_id
  vortex_tracker = model_config_rec%vortex_tracker(id_id)
  RETURN
END SUBROUTINE nl_get_vortex_tracker
SUBROUTINE nl_get_interest_rad_storm ( id_id , interest_rad_storm )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: interest_rad_storm
  INTEGER id_id
  interest_rad_storm = model_config_rec%interest_rad_storm(id_id)
  RETURN
END SUBROUTINE nl_get_interest_rad_storm
SUBROUTINE nl_get_interest_rad_parent ( id_id , interest_rad_parent )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: interest_rad_parent
  INTEGER id_id
  interest_rad_parent = model_config_rec%interest_rad_parent(id_id)
  RETURN
END SUBROUTINE nl_get_interest_rad_parent
SUBROUTINE nl_get_interest_rad_self ( id_id , interest_rad_self )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: interest_rad_self
  INTEGER id_id
  interest_rad_self = model_config_rec%interest_rad_self(id_id)
  RETURN
END SUBROUTINE nl_get_interest_rad_self
SUBROUTINE nl_get_interest_kids ( id_id , interest_kids )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: interest_kids
  INTEGER id_id
  interest_kids = model_config_rec%interest_kids(id_id)
  RETURN
END SUBROUTINE nl_get_interest_kids
SUBROUTINE nl_get_interest_self ( id_id , interest_self )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: interest_self
  INTEGER id_id
  interest_self = model_config_rec%interest_self(id_id)
  RETURN
END SUBROUTINE nl_get_interest_self
SUBROUTINE nl_get_interest_storms ( id_id , interest_storms )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: interest_storms
  INTEGER id_id
  interest_storms = model_config_rec%interest_storms(id_id)
  RETURN
END SUBROUTINE nl_get_interest_storms
SUBROUTINE nl_get_swath_mode ( id_id , swath_mode )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: swath_mode
  INTEGER id_id
  swath_mode = model_config_rec%swath_mode
  RETURN
END SUBROUTINE nl_get_swath_mode
SUBROUTINE nl_get_num_old_fixes ( id_id , num_old_fixes )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_old_fixes
  INTEGER id_id
  num_old_fixes = model_config_rec%num_old_fixes
  RETURN
END SUBROUTINE nl_get_num_old_fixes
SUBROUTINE nl_get_vt4_radius ( id_id , vt4_radius )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: vt4_radius
  INTEGER id_id
  vt4_radius = model_config_rec%vt4_radius(id_id)
  RETURN
END SUBROUTINE nl_get_vt4_radius
SUBROUTINE nl_get_vt4_weightexp ( id_id , vt4_weightexp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: vt4_weightexp
  INTEGER id_id
  vt4_weightexp = model_config_rec%vt4_weightexp(id_id)
  RETURN
END SUBROUTINE nl_get_vt4_weightexp
SUBROUTINE nl_get_vt4_pmax ( id_id , vt4_pmax )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: vt4_pmax
  INTEGER id_id
  vt4_pmax = model_config_rec%vt4_pmax(id_id)
  RETURN
END SUBROUTINE nl_get_vt4_pmax
SUBROUTINE nl_get_vt4_noise_pmax ( id_id , vt4_noise_pmax )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: vt4_noise_pmax
  INTEGER id_id
  vt4_noise_pmax = model_config_rec%vt4_noise_pmax(id_id)
  RETURN
END SUBROUTINE nl_get_vt4_noise_pmax
SUBROUTINE nl_get_vt4_noise_pmin ( id_id , vt4_noise_pmin )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: vt4_noise_pmin
  INTEGER id_id
  vt4_noise_pmin = model_config_rec%vt4_noise_pmin(id_id)
  RETURN
END SUBROUTINE nl_get_vt4_noise_pmin
SUBROUTINE nl_get_vt4_noise_dpdr ( id_id , vt4_noise_dpdr )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: vt4_noise_dpdr
  INTEGER id_id
  vt4_noise_dpdr = model_config_rec%vt4_noise_dpdr(id_id)
  RETURN
END SUBROUTINE nl_get_vt4_noise_dpdr
SUBROUTINE nl_get_vt4_noise_iter ( id_id , vt4_noise_iter )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: vt4_noise_iter
  INTEGER id_id
  vt4_noise_iter = model_config_rec%vt4_noise_iter(id_id)
  RETURN
END SUBROUTINE nl_get_vt4_noise_iter
SUBROUTINE nl_get_nomove_freq ( id_id , nomove_freq )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: nomove_freq
  INTEGER id_id
  nomove_freq = model_config_rec%nomove_freq(id_id)
  RETURN
END SUBROUTINE nl_get_nomove_freq
SUBROUTINE nl_get_coral_x ( id_id , coral_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: coral_x
  INTEGER id_id
  coral_x = model_config_rec%coral_x(id_id)
  RETURN
END SUBROUTINE nl_get_coral_x
SUBROUTINE nl_get_coral_y ( id_id , coral_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: coral_y
  INTEGER id_id
  coral_y = model_config_rec%coral_y(id_id)
  RETURN
END SUBROUTINE nl_get_coral_y
SUBROUTINE nl_get_tg_reset_stream ( id_id , tg_reset_stream )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: tg_reset_stream
  INTEGER id_id
  tg_reset_stream = model_config_rec%tg_reset_stream
  RETURN
END SUBROUTINE nl_get_tg_reset_stream
SUBROUTINE nl_get_tg_option ( id_id , tg_option )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: tg_option
  INTEGER id_id
  tg_option = model_config_rec%tg_option
  RETURN
END SUBROUTINE nl_get_tg_option
SUBROUTINE nl_get_ntornado ( id_id , ntornado )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ntornado
  INTEGER id_id
  ntornado = model_config_rec%ntornado(id_id)
  RETURN
END SUBROUTINE nl_get_ntornado
SUBROUTINE nl_get_wbd0 ( id_id , wbd0 )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: wbd0
  INTEGER id_id
  wbd0 = model_config_rec%wbd0(id_id)
  RETURN
END SUBROUTINE nl_get_wbd0
SUBROUTINE nl_get_sbd0 ( id_id , sbd0 )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: sbd0
  INTEGER id_id
  sbd0 = model_config_rec%sbd0(id_id)
  RETURN
END SUBROUTINE nl_get_sbd0
SUBROUTINE nl_get_analysis ( id_id , analysis )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: analysis
  INTEGER id_id
  analysis = model_config_rec%analysis(id_id)
  RETURN
END SUBROUTINE nl_get_analysis
SUBROUTINE nl_get_write_analysis ( id_id , write_analysis )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: write_analysis
  INTEGER id_id
  write_analysis = model_config_rec%write_analysis(id_id)
  RETURN
END SUBROUTINE nl_get_write_analysis
SUBROUTINE nl_get_io_form_auxinput2 ( id_id , io_form_auxinput2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput2
  INTEGER id_id
  io_form_auxinput2 = model_config_rec%io_form_auxinput2
  RETURN
END SUBROUTINE nl_get_io_form_auxinput2
SUBROUTINE nl_get_high_freq ( id_id , high_freq )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: high_freq
  INTEGER id_id
  high_freq = model_config_rec%high_freq
  RETURN
END SUBROUTINE nl_get_high_freq
SUBROUTINE nl_get_high_dom ( id_id , high_dom )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: high_dom
  INTEGER id_id
  high_dom = model_config_rec%high_dom
  RETURN
END SUBROUTINE nl_get_high_dom
SUBROUTINE nl_get_swint_opt ( id_id , swint_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: swint_opt
  INTEGER id_id
  swint_opt = model_config_rec%swint_opt
  RETURN
END SUBROUTINE nl_get_swint_opt
SUBROUTINE nl_get_aer_type ( id_id , aer_type )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: aer_type
  INTEGER id_id
  aer_type = model_config_rec%aer_type(id_id)
  RETURN
END SUBROUTINE nl_get_aer_type
SUBROUTINE nl_get_aer_aod550_opt ( id_id , aer_aod550_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: aer_aod550_opt
  INTEGER id_id
  aer_aod550_opt = model_config_rec%aer_aod550_opt(id_id)
  RETURN
END SUBROUTINE nl_get_aer_aod550_opt
SUBROUTINE nl_get_aer_angexp_opt ( id_id , aer_angexp_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: aer_angexp_opt
  INTEGER id_id
  aer_angexp_opt = model_config_rec%aer_angexp_opt(id_id)
  RETURN
END SUBROUTINE nl_get_aer_angexp_opt
SUBROUTINE nl_get_aer_ssa_opt ( id_id , aer_ssa_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: aer_ssa_opt
  INTEGER id_id
  aer_ssa_opt = model_config_rec%aer_ssa_opt(id_id)
  RETURN
END SUBROUTINE nl_get_aer_ssa_opt
SUBROUTINE nl_get_aer_asy_opt ( id_id , aer_asy_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: aer_asy_opt
  INTEGER id_id
  aer_asy_opt = model_config_rec%aer_asy_opt(id_id)
  RETURN
END SUBROUTINE nl_get_aer_asy_opt
SUBROUTINE nl_get_aer_aod550_val ( id_id , aer_aod550_val )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: aer_aod550_val
  INTEGER id_id
  aer_aod550_val = model_config_rec%aer_aod550_val(id_id)
  RETURN
END SUBROUTINE nl_get_aer_aod550_val
SUBROUTINE nl_get_aer_angexp_val ( id_id , aer_angexp_val )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: aer_angexp_val
  INTEGER id_id
  aer_angexp_val = model_config_rec%aer_angexp_val(id_id)
  RETURN
END SUBROUTINE nl_get_aer_angexp_val
SUBROUTINE nl_get_aer_ssa_val ( id_id , aer_ssa_val )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: aer_ssa_val
  INTEGER id_id
  aer_ssa_val = model_config_rec%aer_ssa_val(id_id)
  RETURN
END SUBROUTINE nl_get_aer_ssa_val
SUBROUTINE nl_get_aer_asy_val ( id_id , aer_asy_val )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: aer_asy_val
  INTEGER id_id
  aer_asy_val = model_config_rec%aer_asy_val(id_id)
  RETURN
END SUBROUTINE nl_get_aer_asy_val
SUBROUTINE nl_get_dveg ( id_id , dveg )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dveg
  INTEGER id_id
  dveg = model_config_rec%dveg
  RETURN
END SUBROUTINE nl_get_dveg
SUBROUTINE nl_get_opt_crs ( id_id , opt_crs )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: opt_crs
  INTEGER id_id
  opt_crs = model_config_rec%opt_crs
  RETURN
END SUBROUTINE nl_get_opt_crs
SUBROUTINE nl_get_opt_btr ( id_id , opt_btr )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: opt_btr
  INTEGER id_id
  opt_btr = model_config_rec%opt_btr
  RETURN
END SUBROUTINE nl_get_opt_btr
SUBROUTINE nl_get_opt_run ( id_id , opt_run )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: opt_run
  INTEGER id_id
  opt_run = model_config_rec%opt_run
  RETURN
END SUBROUTINE nl_get_opt_run
SUBROUTINE nl_get_opt_sfc ( id_id , opt_sfc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: opt_sfc
  INTEGER id_id
  opt_sfc = model_config_rec%opt_sfc
  RETURN
END SUBROUTINE nl_get_opt_sfc
SUBROUTINE nl_get_opt_frz ( id_id , opt_frz )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: opt_frz
  INTEGER id_id
  opt_frz = model_config_rec%opt_frz
  RETURN
END SUBROUTINE nl_get_opt_frz
SUBROUTINE nl_get_opt_inf ( id_id , opt_inf )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: opt_inf
  INTEGER id_id
  opt_inf = model_config_rec%opt_inf
  RETURN
END SUBROUTINE nl_get_opt_inf
SUBROUTINE nl_get_opt_rad ( id_id , opt_rad )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: opt_rad
  INTEGER id_id
  opt_rad = model_config_rec%opt_rad
  RETURN
END SUBROUTINE nl_get_opt_rad
SUBROUTINE nl_get_opt_alb ( id_id , opt_alb )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: opt_alb
  INTEGER id_id
  opt_alb = model_config_rec%opt_alb
  RETURN
END SUBROUTINE nl_get_opt_alb
SUBROUTINE nl_get_opt_snf ( id_id , opt_snf )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: opt_snf
  INTEGER id_id
  opt_snf = model_config_rec%opt_snf
  RETURN
END SUBROUTINE nl_get_opt_snf
SUBROUTINE nl_get_opt_tbot ( id_id , opt_tbot )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: opt_tbot
  INTEGER id_id
  opt_tbot = model_config_rec%opt_tbot
  RETURN
END SUBROUTINE nl_get_opt_tbot
SUBROUTINE nl_get_opt_stc ( id_id , opt_stc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: opt_stc
  INTEGER id_id
  opt_stc = model_config_rec%opt_stc
  RETURN
END SUBROUTINE nl_get_opt_stc
SUBROUTINE nl_get_wrf_hydro ( id_id , wrf_hydro )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: wrf_hydro
  INTEGER id_id
  wrf_hydro = model_config_rec%wrf_hydro
  RETURN
END SUBROUTINE nl_get_wrf_hydro
SUBROUTINE nl_get_run_days ( id_id , run_days )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: run_days
  INTEGER id_id
  run_days = model_config_rec%run_days
  RETURN
END SUBROUTINE nl_get_run_days
SUBROUTINE nl_get_run_hours ( id_id , run_hours )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: run_hours
  INTEGER id_id
  run_hours = model_config_rec%run_hours
  RETURN
END SUBROUTINE nl_get_run_hours
SUBROUTINE nl_get_run_minutes ( id_id , run_minutes )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: run_minutes
  INTEGER id_id
  run_minutes = model_config_rec%run_minutes
  RETURN
END SUBROUTINE nl_get_run_minutes
SUBROUTINE nl_get_run_seconds ( id_id , run_seconds )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: run_seconds
  INTEGER id_id
  run_seconds = model_config_rec%run_seconds
  RETURN
END SUBROUTINE nl_get_run_seconds
SUBROUTINE nl_get_start_year ( id_id , start_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: start_year
  INTEGER id_id
  start_year = model_config_rec%start_year(id_id)
  RETURN
END SUBROUTINE nl_get_start_year
SUBROUTINE nl_get_start_month ( id_id , start_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: start_month
  INTEGER id_id
  start_month = model_config_rec%start_month(id_id)
  RETURN
END SUBROUTINE nl_get_start_month
SUBROUTINE nl_get_start_day ( id_id , start_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: start_day
  INTEGER id_id
  start_day = model_config_rec%start_day(id_id)
  RETURN
END SUBROUTINE nl_get_start_day
SUBROUTINE nl_get_start_hour ( id_id , start_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: start_hour
  INTEGER id_id
  start_hour = model_config_rec%start_hour(id_id)
  RETURN
END SUBROUTINE nl_get_start_hour
SUBROUTINE nl_get_start_minute ( id_id , start_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: start_minute
  INTEGER id_id
  start_minute = model_config_rec%start_minute(id_id)
  RETURN
END SUBROUTINE nl_get_start_minute
SUBROUTINE nl_get_start_second ( id_id , start_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: start_second
  INTEGER id_id
  start_second = model_config_rec%start_second(id_id)
  RETURN
END SUBROUTINE nl_get_start_second
SUBROUTINE nl_get_end_year ( id_id , end_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: end_year
  INTEGER id_id
  end_year = model_config_rec%end_year(id_id)
  RETURN
END SUBROUTINE nl_get_end_year
SUBROUTINE nl_get_end_month ( id_id , end_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: end_month
  INTEGER id_id
  end_month = model_config_rec%end_month(id_id)
  RETURN
END SUBROUTINE nl_get_end_month
SUBROUTINE nl_get_end_day ( id_id , end_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: end_day
  INTEGER id_id
  end_day = model_config_rec%end_day(id_id)
  RETURN
END SUBROUTINE nl_get_end_day
SUBROUTINE nl_get_end_hour ( id_id , end_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: end_hour
  INTEGER id_id
  end_hour = model_config_rec%end_hour(id_id)
  RETURN
END SUBROUTINE nl_get_end_hour
SUBROUTINE nl_get_end_minute ( id_id , end_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: end_minute
  INTEGER id_id
  end_minute = model_config_rec%end_minute(id_id)
  RETURN
END SUBROUTINE nl_get_end_minute
SUBROUTINE nl_get_end_second ( id_id , end_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: end_second
  INTEGER id_id
  end_second = model_config_rec%end_second(id_id)
  RETURN
END SUBROUTINE nl_get_end_second
SUBROUTINE nl_get_interval_seconds ( id_id , interval_seconds )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: interval_seconds
  INTEGER id_id
  interval_seconds = model_config_rec%interval_seconds
  RETURN
END SUBROUTINE nl_get_interval_seconds
SUBROUTINE nl_get_input_from_file ( id_id , input_from_file )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: input_from_file
  INTEGER id_id
  input_from_file = model_config_rec%input_from_file(id_id)
  RETURN
END SUBROUTINE nl_get_input_from_file
SUBROUTINE nl_get_fine_input_stream ( id_id , fine_input_stream )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: fine_input_stream
  INTEGER id_id
  fine_input_stream = model_config_rec%fine_input_stream(id_id)
  RETURN
END SUBROUTINE nl_get_fine_input_stream
SUBROUTINE nl_get_auxinput1_inname ( id_id , auxinput1_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput1_inname
  INTEGER id_id
  auxinput1_inname = trim(model_config_rec%auxinput1_inname)
  RETURN
END SUBROUTINE nl_get_auxinput1_inname
SUBROUTINE nl_get_io_form_auxinput1 ( id_id , io_form_auxinput1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput1
  INTEGER id_id
  io_form_auxinput1 = model_config_rec%io_form_auxinput1
  RETURN
END SUBROUTINE nl_get_io_form_auxinput1
SUBROUTINE nl_get_override_restart_timers ( id_id , override_restart_timers )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: override_restart_timers
  INTEGER id_id
  override_restart_timers = model_config_rec%override_restart_timers
  RETURN
END SUBROUTINE nl_get_override_restart_timers
SUBROUTINE nl_get_auxhist1_inname ( id_id , auxhist1_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist1_inname
  INTEGER id_id
  auxhist1_inname = trim(model_config_rec%auxhist1_inname)
  RETURN
END SUBROUTINE nl_get_auxhist1_inname
SUBROUTINE nl_get_auxhist1_outname ( id_id , auxhist1_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist1_outname
  INTEGER id_id
  auxhist1_outname = trim(model_config_rec%auxhist1_outname)
  RETURN
END SUBROUTINE nl_get_auxhist1_outname
SUBROUTINE nl_get_auxhist1_interval_y ( id_id , auxhist1_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_interval_y
  INTEGER id_id
  auxhist1_interval_y = model_config_rec%auxhist1_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval_y
SUBROUTINE nl_get_auxhist1_interval_d ( id_id , auxhist1_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_interval_d
  INTEGER id_id
  auxhist1_interval_d = model_config_rec%auxhist1_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval_d
SUBROUTINE nl_get_auxhist1_interval_h ( id_id , auxhist1_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_interval_h
  INTEGER id_id
  auxhist1_interval_h = model_config_rec%auxhist1_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval_h
SUBROUTINE nl_get_auxhist1_interval_m ( id_id , auxhist1_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_interval_m
  INTEGER id_id
  auxhist1_interval_m = model_config_rec%auxhist1_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval_m
SUBROUTINE nl_get_auxhist1_interval_s ( id_id , auxhist1_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_interval_s
  INTEGER id_id
  auxhist1_interval_s = model_config_rec%auxhist1_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval_s
SUBROUTINE nl_get_auxhist1_interval ( id_id , auxhist1_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_interval
  INTEGER id_id
  auxhist1_interval = model_config_rec%auxhist1_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_interval
SUBROUTINE nl_get_auxhist1_begin_y ( id_id , auxhist1_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_begin_y
  INTEGER id_id
  auxhist1_begin_y = model_config_rec%auxhist1_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_y
SUBROUTINE nl_get_auxhist1_begin_d ( id_id , auxhist1_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_begin_d
  INTEGER id_id
  auxhist1_begin_d = model_config_rec%auxhist1_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_d
SUBROUTINE nl_get_auxhist1_begin_h ( id_id , auxhist1_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_begin_h
  INTEGER id_id
  auxhist1_begin_h = model_config_rec%auxhist1_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_h
SUBROUTINE nl_get_auxhist1_begin_m ( id_id , auxhist1_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_begin_m
  INTEGER id_id
  auxhist1_begin_m = model_config_rec%auxhist1_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_m
SUBROUTINE nl_get_auxhist1_begin_s ( id_id , auxhist1_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_begin_s
  INTEGER id_id
  auxhist1_begin_s = model_config_rec%auxhist1_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin_s
SUBROUTINE nl_get_auxhist1_begin ( id_id , auxhist1_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_begin
  INTEGER id_id
  auxhist1_begin = model_config_rec%auxhist1_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_begin
SUBROUTINE nl_get_auxhist1_end_y ( id_id , auxhist1_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_end_y
  INTEGER id_id
  auxhist1_end_y = model_config_rec%auxhist1_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_y
SUBROUTINE nl_get_auxhist1_end_d ( id_id , auxhist1_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_end_d
  INTEGER id_id
  auxhist1_end_d = model_config_rec%auxhist1_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_d
SUBROUTINE nl_get_auxhist1_end_h ( id_id , auxhist1_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_end_h
  INTEGER id_id
  auxhist1_end_h = model_config_rec%auxhist1_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_h
SUBROUTINE nl_get_auxhist1_end_m ( id_id , auxhist1_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_end_m
  INTEGER id_id
  auxhist1_end_m = model_config_rec%auxhist1_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_m
SUBROUTINE nl_get_auxhist1_end_s ( id_id , auxhist1_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_end_s
  INTEGER id_id
  auxhist1_end_s = model_config_rec%auxhist1_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end_s
SUBROUTINE nl_get_auxhist1_end ( id_id , auxhist1_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist1_end
  INTEGER id_id
  auxhist1_end = model_config_rec%auxhist1_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist1_end
SUBROUTINE nl_get_io_form_auxhist1 ( id_id , io_form_auxhist1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist1
  INTEGER id_id
  io_form_auxhist1 = model_config_rec%io_form_auxhist1
  RETURN
END SUBROUTINE nl_get_io_form_auxhist1
SUBROUTINE nl_get_frames_per_auxhist1 ( id_id , frames_per_auxhist1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist1
  INTEGER id_id
  frames_per_auxhist1 = model_config_rec%frames_per_auxhist1(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist1
SUBROUTINE nl_get_auxhist2_inname ( id_id , auxhist2_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist2_inname
  INTEGER id_id
  auxhist2_inname = trim(model_config_rec%auxhist2_inname)
  RETURN
END SUBROUTINE nl_get_auxhist2_inname
SUBROUTINE nl_get_auxhist2_outname ( id_id , auxhist2_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist2_outname
  INTEGER id_id
  auxhist2_outname = trim(model_config_rec%auxhist2_outname)
  RETURN
END SUBROUTINE nl_get_auxhist2_outname
SUBROUTINE nl_get_auxhist2_interval_y ( id_id , auxhist2_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_interval_y
  INTEGER id_id
  auxhist2_interval_y = model_config_rec%auxhist2_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval_y
SUBROUTINE nl_get_auxhist2_interval_d ( id_id , auxhist2_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_interval_d
  INTEGER id_id
  auxhist2_interval_d = model_config_rec%auxhist2_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval_d
SUBROUTINE nl_get_auxhist2_interval_h ( id_id , auxhist2_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_interval_h
  INTEGER id_id
  auxhist2_interval_h = model_config_rec%auxhist2_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval_h
SUBROUTINE nl_get_auxhist2_interval_m ( id_id , auxhist2_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_interval_m
  INTEGER id_id
  auxhist2_interval_m = model_config_rec%auxhist2_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval_m
SUBROUTINE nl_get_auxhist2_interval_s ( id_id , auxhist2_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_interval_s
  INTEGER id_id
  auxhist2_interval_s = model_config_rec%auxhist2_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval_s
SUBROUTINE nl_get_auxhist2_interval ( id_id , auxhist2_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_interval
  INTEGER id_id
  auxhist2_interval = model_config_rec%auxhist2_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_interval
SUBROUTINE nl_get_auxhist2_begin_y ( id_id , auxhist2_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_begin_y
  INTEGER id_id
  auxhist2_begin_y = model_config_rec%auxhist2_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_y
SUBROUTINE nl_get_auxhist2_begin_d ( id_id , auxhist2_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_begin_d
  INTEGER id_id
  auxhist2_begin_d = model_config_rec%auxhist2_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_d
SUBROUTINE nl_get_auxhist2_begin_h ( id_id , auxhist2_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_begin_h
  INTEGER id_id
  auxhist2_begin_h = model_config_rec%auxhist2_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_h
SUBROUTINE nl_get_auxhist2_begin_m ( id_id , auxhist2_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_begin_m
  INTEGER id_id
  auxhist2_begin_m = model_config_rec%auxhist2_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_m
SUBROUTINE nl_get_auxhist2_begin_s ( id_id , auxhist2_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_begin_s
  INTEGER id_id
  auxhist2_begin_s = model_config_rec%auxhist2_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin_s
SUBROUTINE nl_get_auxhist2_begin ( id_id , auxhist2_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_begin
  INTEGER id_id
  auxhist2_begin = model_config_rec%auxhist2_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_begin
SUBROUTINE nl_get_auxhist2_end_y ( id_id , auxhist2_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_end_y
  INTEGER id_id
  auxhist2_end_y = model_config_rec%auxhist2_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_y
SUBROUTINE nl_get_auxhist2_end_d ( id_id , auxhist2_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_end_d
  INTEGER id_id
  auxhist2_end_d = model_config_rec%auxhist2_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_d
SUBROUTINE nl_get_auxhist2_end_h ( id_id , auxhist2_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_end_h
  INTEGER id_id
  auxhist2_end_h = model_config_rec%auxhist2_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_h
SUBROUTINE nl_get_auxhist2_end_m ( id_id , auxhist2_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_end_m
  INTEGER id_id
  auxhist2_end_m = model_config_rec%auxhist2_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_m
SUBROUTINE nl_get_auxhist2_end_s ( id_id , auxhist2_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_end_s
  INTEGER id_id
  auxhist2_end_s = model_config_rec%auxhist2_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end_s
SUBROUTINE nl_get_auxhist2_end ( id_id , auxhist2_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist2_end
  INTEGER id_id
  auxhist2_end = model_config_rec%auxhist2_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist2_end
SUBROUTINE nl_get_io_form_auxhist2 ( id_id , io_form_auxhist2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist2
  INTEGER id_id
  io_form_auxhist2 = model_config_rec%io_form_auxhist2
  RETURN
END SUBROUTINE nl_get_io_form_auxhist2
SUBROUTINE nl_get_frames_per_auxhist2 ( id_id , frames_per_auxhist2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist2
  INTEGER id_id
  frames_per_auxhist2 = model_config_rec%frames_per_auxhist2(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist2
SUBROUTINE nl_get_auxhist3_inname ( id_id , auxhist3_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist3_inname
  INTEGER id_id
  auxhist3_inname = trim(model_config_rec%auxhist3_inname)
  RETURN
END SUBROUTINE nl_get_auxhist3_inname
SUBROUTINE nl_get_auxhist3_outname ( id_id , auxhist3_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist3_outname
  INTEGER id_id
  auxhist3_outname = trim(model_config_rec%auxhist3_outname)
  RETURN
END SUBROUTINE nl_get_auxhist3_outname
SUBROUTINE nl_get_auxhist3_interval_y ( id_id , auxhist3_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_interval_y
  INTEGER id_id
  auxhist3_interval_y = model_config_rec%auxhist3_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval_y
SUBROUTINE nl_get_auxhist3_interval_d ( id_id , auxhist3_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_interval_d
  INTEGER id_id
  auxhist3_interval_d = model_config_rec%auxhist3_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval_d
SUBROUTINE nl_get_auxhist3_interval_h ( id_id , auxhist3_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_interval_h
  INTEGER id_id
  auxhist3_interval_h = model_config_rec%auxhist3_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval_h
SUBROUTINE nl_get_auxhist3_interval_m ( id_id , auxhist3_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_interval_m
  INTEGER id_id
  auxhist3_interval_m = model_config_rec%auxhist3_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval_m
SUBROUTINE nl_get_auxhist3_interval_s ( id_id , auxhist3_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_interval_s
  INTEGER id_id
  auxhist3_interval_s = model_config_rec%auxhist3_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval_s
SUBROUTINE nl_get_auxhist3_interval ( id_id , auxhist3_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_interval
  INTEGER id_id
  auxhist3_interval = model_config_rec%auxhist3_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_interval
SUBROUTINE nl_get_auxhist3_begin_y ( id_id , auxhist3_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_begin_y
  INTEGER id_id
  auxhist3_begin_y = model_config_rec%auxhist3_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_y
SUBROUTINE nl_get_auxhist3_begin_d ( id_id , auxhist3_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_begin_d
  INTEGER id_id
  auxhist3_begin_d = model_config_rec%auxhist3_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_d
SUBROUTINE nl_get_auxhist3_begin_h ( id_id , auxhist3_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_begin_h
  INTEGER id_id
  auxhist3_begin_h = model_config_rec%auxhist3_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_h
SUBROUTINE nl_get_auxhist3_begin_m ( id_id , auxhist3_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_begin_m
  INTEGER id_id
  auxhist3_begin_m = model_config_rec%auxhist3_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_m
SUBROUTINE nl_get_auxhist3_begin_s ( id_id , auxhist3_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_begin_s
  INTEGER id_id
  auxhist3_begin_s = model_config_rec%auxhist3_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin_s
SUBROUTINE nl_get_auxhist3_begin ( id_id , auxhist3_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_begin
  INTEGER id_id
  auxhist3_begin = model_config_rec%auxhist3_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_begin
SUBROUTINE nl_get_auxhist3_end_y ( id_id , auxhist3_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_end_y
  INTEGER id_id
  auxhist3_end_y = model_config_rec%auxhist3_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_y
SUBROUTINE nl_get_auxhist3_end_d ( id_id , auxhist3_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_end_d
  INTEGER id_id
  auxhist3_end_d = model_config_rec%auxhist3_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_d
SUBROUTINE nl_get_auxhist3_end_h ( id_id , auxhist3_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_end_h
  INTEGER id_id
  auxhist3_end_h = model_config_rec%auxhist3_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_h
SUBROUTINE nl_get_auxhist3_end_m ( id_id , auxhist3_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_end_m
  INTEGER id_id
  auxhist3_end_m = model_config_rec%auxhist3_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_m
SUBROUTINE nl_get_auxhist3_end_s ( id_id , auxhist3_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_end_s
  INTEGER id_id
  auxhist3_end_s = model_config_rec%auxhist3_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end_s
SUBROUTINE nl_get_auxhist3_end ( id_id , auxhist3_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist3_end
  INTEGER id_id
  auxhist3_end = model_config_rec%auxhist3_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist3_end
SUBROUTINE nl_get_io_form_auxhist3 ( id_id , io_form_auxhist3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist3
  INTEGER id_id
  io_form_auxhist3 = model_config_rec%io_form_auxhist3
  RETURN
END SUBROUTINE nl_get_io_form_auxhist3
SUBROUTINE nl_get_frames_per_auxhist3 ( id_id , frames_per_auxhist3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist3
  INTEGER id_id
  frames_per_auxhist3 = model_config_rec%frames_per_auxhist3(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist3
SUBROUTINE nl_get_auxhist4_inname ( id_id , auxhist4_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist4_inname
  INTEGER id_id
  auxhist4_inname = trim(model_config_rec%auxhist4_inname)
  RETURN
END SUBROUTINE nl_get_auxhist4_inname
SUBROUTINE nl_get_auxhist4_outname ( id_id , auxhist4_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist4_outname
  INTEGER id_id
  auxhist4_outname = trim(model_config_rec%auxhist4_outname)
  RETURN
END SUBROUTINE nl_get_auxhist4_outname
SUBROUTINE nl_get_auxhist4_interval_y ( id_id , auxhist4_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_interval_y
  INTEGER id_id
  auxhist4_interval_y = model_config_rec%auxhist4_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval_y
SUBROUTINE nl_get_auxhist4_interval_d ( id_id , auxhist4_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_interval_d
  INTEGER id_id
  auxhist4_interval_d = model_config_rec%auxhist4_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval_d
SUBROUTINE nl_get_auxhist4_interval_h ( id_id , auxhist4_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_interval_h
  INTEGER id_id
  auxhist4_interval_h = model_config_rec%auxhist4_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval_h
SUBROUTINE nl_get_auxhist4_interval_m ( id_id , auxhist4_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_interval_m
  INTEGER id_id
  auxhist4_interval_m = model_config_rec%auxhist4_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval_m
SUBROUTINE nl_get_auxhist4_interval_s ( id_id , auxhist4_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_interval_s
  INTEGER id_id
  auxhist4_interval_s = model_config_rec%auxhist4_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval_s
SUBROUTINE nl_get_auxhist4_interval ( id_id , auxhist4_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_interval
  INTEGER id_id
  auxhist4_interval = model_config_rec%auxhist4_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_interval
SUBROUTINE nl_get_auxhist4_begin_y ( id_id , auxhist4_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_begin_y
  INTEGER id_id
  auxhist4_begin_y = model_config_rec%auxhist4_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_y
SUBROUTINE nl_get_auxhist4_begin_d ( id_id , auxhist4_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_begin_d
  INTEGER id_id
  auxhist4_begin_d = model_config_rec%auxhist4_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_d
SUBROUTINE nl_get_auxhist4_begin_h ( id_id , auxhist4_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_begin_h
  INTEGER id_id
  auxhist4_begin_h = model_config_rec%auxhist4_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_h
SUBROUTINE nl_get_auxhist4_begin_m ( id_id , auxhist4_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_begin_m
  INTEGER id_id
  auxhist4_begin_m = model_config_rec%auxhist4_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_m
SUBROUTINE nl_get_auxhist4_begin_s ( id_id , auxhist4_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_begin_s
  INTEGER id_id
  auxhist4_begin_s = model_config_rec%auxhist4_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin_s
SUBROUTINE nl_get_auxhist4_begin ( id_id , auxhist4_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_begin
  INTEGER id_id
  auxhist4_begin = model_config_rec%auxhist4_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_begin
SUBROUTINE nl_get_auxhist4_end_y ( id_id , auxhist4_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_end_y
  INTEGER id_id
  auxhist4_end_y = model_config_rec%auxhist4_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_y
SUBROUTINE nl_get_auxhist4_end_d ( id_id , auxhist4_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_end_d
  INTEGER id_id
  auxhist4_end_d = model_config_rec%auxhist4_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_d
SUBROUTINE nl_get_auxhist4_end_h ( id_id , auxhist4_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_end_h
  INTEGER id_id
  auxhist4_end_h = model_config_rec%auxhist4_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_h
SUBROUTINE nl_get_auxhist4_end_m ( id_id , auxhist4_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_end_m
  INTEGER id_id
  auxhist4_end_m = model_config_rec%auxhist4_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_m
SUBROUTINE nl_get_auxhist4_end_s ( id_id , auxhist4_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_end_s
  INTEGER id_id
  auxhist4_end_s = model_config_rec%auxhist4_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end_s
SUBROUTINE nl_get_auxhist4_end ( id_id , auxhist4_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist4_end
  INTEGER id_id
  auxhist4_end = model_config_rec%auxhist4_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist4_end
SUBROUTINE nl_get_io_form_auxhist4 ( id_id , io_form_auxhist4 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist4
  INTEGER id_id
  io_form_auxhist4 = model_config_rec%io_form_auxhist4
  RETURN
END SUBROUTINE nl_get_io_form_auxhist4
SUBROUTINE nl_get_frames_per_auxhist4 ( id_id , frames_per_auxhist4 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist4
  INTEGER id_id
  frames_per_auxhist4 = model_config_rec%frames_per_auxhist4(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist4
SUBROUTINE nl_get_auxhist5_inname ( id_id , auxhist5_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist5_inname
  INTEGER id_id
  auxhist5_inname = trim(model_config_rec%auxhist5_inname)
  RETURN
END SUBROUTINE nl_get_auxhist5_inname
SUBROUTINE nl_get_auxhist5_outname ( id_id , auxhist5_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist5_outname
  INTEGER id_id
  auxhist5_outname = trim(model_config_rec%auxhist5_outname)
  RETURN
END SUBROUTINE nl_get_auxhist5_outname
SUBROUTINE nl_get_auxhist5_interval_y ( id_id , auxhist5_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_interval_y
  INTEGER id_id
  auxhist5_interval_y = model_config_rec%auxhist5_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval_y
SUBROUTINE nl_get_auxhist5_interval_d ( id_id , auxhist5_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_interval_d
  INTEGER id_id
  auxhist5_interval_d = model_config_rec%auxhist5_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval_d
SUBROUTINE nl_get_auxhist5_interval_h ( id_id , auxhist5_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_interval_h
  INTEGER id_id
  auxhist5_interval_h = model_config_rec%auxhist5_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval_h
SUBROUTINE nl_get_auxhist5_interval_m ( id_id , auxhist5_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_interval_m
  INTEGER id_id
  auxhist5_interval_m = model_config_rec%auxhist5_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval_m
SUBROUTINE nl_get_auxhist5_interval_s ( id_id , auxhist5_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_interval_s
  INTEGER id_id
  auxhist5_interval_s = model_config_rec%auxhist5_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval_s
SUBROUTINE nl_get_auxhist5_interval ( id_id , auxhist5_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_interval
  INTEGER id_id
  auxhist5_interval = model_config_rec%auxhist5_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_interval
SUBROUTINE nl_get_auxhist5_begin_y ( id_id , auxhist5_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_begin_y
  INTEGER id_id
  auxhist5_begin_y = model_config_rec%auxhist5_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_y
SUBROUTINE nl_get_auxhist5_begin_d ( id_id , auxhist5_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_begin_d
  INTEGER id_id
  auxhist5_begin_d = model_config_rec%auxhist5_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_d
SUBROUTINE nl_get_auxhist5_begin_h ( id_id , auxhist5_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_begin_h
  INTEGER id_id
  auxhist5_begin_h = model_config_rec%auxhist5_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_h
SUBROUTINE nl_get_auxhist5_begin_m ( id_id , auxhist5_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_begin_m
  INTEGER id_id
  auxhist5_begin_m = model_config_rec%auxhist5_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_m
SUBROUTINE nl_get_auxhist5_begin_s ( id_id , auxhist5_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_begin_s
  INTEGER id_id
  auxhist5_begin_s = model_config_rec%auxhist5_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin_s
SUBROUTINE nl_get_auxhist5_begin ( id_id , auxhist5_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_begin
  INTEGER id_id
  auxhist5_begin = model_config_rec%auxhist5_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_begin
SUBROUTINE nl_get_auxhist5_end_y ( id_id , auxhist5_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_end_y
  INTEGER id_id
  auxhist5_end_y = model_config_rec%auxhist5_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_y
SUBROUTINE nl_get_auxhist5_end_d ( id_id , auxhist5_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_end_d
  INTEGER id_id
  auxhist5_end_d = model_config_rec%auxhist5_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_d
SUBROUTINE nl_get_auxhist5_end_h ( id_id , auxhist5_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_end_h
  INTEGER id_id
  auxhist5_end_h = model_config_rec%auxhist5_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_h
SUBROUTINE nl_get_auxhist5_end_m ( id_id , auxhist5_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_end_m
  INTEGER id_id
  auxhist5_end_m = model_config_rec%auxhist5_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_m
SUBROUTINE nl_get_auxhist5_end_s ( id_id , auxhist5_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_end_s
  INTEGER id_id
  auxhist5_end_s = model_config_rec%auxhist5_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end_s
SUBROUTINE nl_get_auxhist5_end ( id_id , auxhist5_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist5_end
  INTEGER id_id
  auxhist5_end = model_config_rec%auxhist5_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist5_end
SUBROUTINE nl_get_io_form_auxhist5 ( id_id , io_form_auxhist5 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist5
  INTEGER id_id
  io_form_auxhist5 = model_config_rec%io_form_auxhist5
  RETURN
END SUBROUTINE nl_get_io_form_auxhist5
SUBROUTINE nl_get_frames_per_auxhist5 ( id_id , frames_per_auxhist5 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist5
  INTEGER id_id
  frames_per_auxhist5 = model_config_rec%frames_per_auxhist5(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist5
#endif
#if (NNN == 1)
SUBROUTINE nl_get_auxhist6_inname ( id_id , auxhist6_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist6_inname
  INTEGER id_id
  auxhist6_inname = trim(model_config_rec%auxhist6_inname)
  RETURN
END SUBROUTINE nl_get_auxhist6_inname
SUBROUTINE nl_get_auxhist6_outname ( id_id , auxhist6_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist6_outname
  INTEGER id_id
  auxhist6_outname = trim(model_config_rec%auxhist6_outname)
  RETURN
END SUBROUTINE nl_get_auxhist6_outname
SUBROUTINE nl_get_auxhist6_interval_y ( id_id , auxhist6_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_interval_y
  INTEGER id_id
  auxhist6_interval_y = model_config_rec%auxhist6_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_interval_y
SUBROUTINE nl_get_auxhist6_interval_d ( id_id , auxhist6_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_interval_d
  INTEGER id_id
  auxhist6_interval_d = model_config_rec%auxhist6_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_interval_d
SUBROUTINE nl_get_auxhist6_interval_h ( id_id , auxhist6_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_interval_h
  INTEGER id_id
  auxhist6_interval_h = model_config_rec%auxhist6_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_interval_h
SUBROUTINE nl_get_auxhist6_interval_m ( id_id , auxhist6_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_interval_m
  INTEGER id_id
  auxhist6_interval_m = model_config_rec%auxhist6_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_interval_m
SUBROUTINE nl_get_auxhist6_interval_s ( id_id , auxhist6_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_interval_s
  INTEGER id_id
  auxhist6_interval_s = model_config_rec%auxhist6_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_interval_s
SUBROUTINE nl_get_auxhist6_interval ( id_id , auxhist6_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_interval
  INTEGER id_id
  auxhist6_interval = model_config_rec%auxhist6_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_interval
SUBROUTINE nl_get_auxhist6_begin_y ( id_id , auxhist6_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_begin_y
  INTEGER id_id
  auxhist6_begin_y = model_config_rec%auxhist6_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_begin_y
SUBROUTINE nl_get_auxhist6_begin_d ( id_id , auxhist6_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_begin_d
  INTEGER id_id
  auxhist6_begin_d = model_config_rec%auxhist6_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_begin_d
SUBROUTINE nl_get_auxhist6_begin_h ( id_id , auxhist6_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_begin_h
  INTEGER id_id
  auxhist6_begin_h = model_config_rec%auxhist6_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_begin_h
SUBROUTINE nl_get_auxhist6_begin_m ( id_id , auxhist6_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_begin_m
  INTEGER id_id
  auxhist6_begin_m = model_config_rec%auxhist6_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_begin_m
SUBROUTINE nl_get_auxhist6_begin_s ( id_id , auxhist6_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_begin_s
  INTEGER id_id
  auxhist6_begin_s = model_config_rec%auxhist6_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_begin_s
SUBROUTINE nl_get_auxhist6_begin ( id_id , auxhist6_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_begin
  INTEGER id_id
  auxhist6_begin = model_config_rec%auxhist6_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_begin
SUBROUTINE nl_get_auxhist6_end_y ( id_id , auxhist6_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_end_y
  INTEGER id_id
  auxhist6_end_y = model_config_rec%auxhist6_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_end_y
SUBROUTINE nl_get_auxhist6_end_d ( id_id , auxhist6_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_end_d
  INTEGER id_id
  auxhist6_end_d = model_config_rec%auxhist6_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_end_d
SUBROUTINE nl_get_auxhist6_end_h ( id_id , auxhist6_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_end_h
  INTEGER id_id
  auxhist6_end_h = model_config_rec%auxhist6_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_end_h
SUBROUTINE nl_get_auxhist6_end_m ( id_id , auxhist6_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_end_m
  INTEGER id_id
  auxhist6_end_m = model_config_rec%auxhist6_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_end_m
SUBROUTINE nl_get_auxhist6_end_s ( id_id , auxhist6_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_end_s
  INTEGER id_id
  auxhist6_end_s = model_config_rec%auxhist6_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_end_s
SUBROUTINE nl_get_auxhist6_end ( id_id , auxhist6_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist6_end
  INTEGER id_id
  auxhist6_end = model_config_rec%auxhist6_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist6_end
SUBROUTINE nl_get_io_form_auxhist6 ( id_id , io_form_auxhist6 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist6
  INTEGER id_id
  io_form_auxhist6 = model_config_rec%io_form_auxhist6
  RETURN
END SUBROUTINE nl_get_io_form_auxhist6
SUBROUTINE nl_get_frames_per_auxhist6 ( id_id , frames_per_auxhist6 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist6
  INTEGER id_id
  frames_per_auxhist6 = model_config_rec%frames_per_auxhist6(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist6
SUBROUTINE nl_get_auxhist7_inname ( id_id , auxhist7_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist7_inname
  INTEGER id_id
  auxhist7_inname = trim(model_config_rec%auxhist7_inname)
  RETURN
END SUBROUTINE nl_get_auxhist7_inname
SUBROUTINE nl_get_auxhist7_outname ( id_id , auxhist7_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist7_outname
  INTEGER id_id
  auxhist7_outname = trim(model_config_rec%auxhist7_outname)
  RETURN
END SUBROUTINE nl_get_auxhist7_outname
SUBROUTINE nl_get_auxhist7_interval_y ( id_id , auxhist7_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_interval_y
  INTEGER id_id
  auxhist7_interval_y = model_config_rec%auxhist7_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_interval_y
SUBROUTINE nl_get_auxhist7_interval_d ( id_id , auxhist7_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_interval_d
  INTEGER id_id
  auxhist7_interval_d = model_config_rec%auxhist7_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_interval_d
SUBROUTINE nl_get_auxhist7_interval_h ( id_id , auxhist7_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_interval_h
  INTEGER id_id
  auxhist7_interval_h = model_config_rec%auxhist7_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_interval_h
SUBROUTINE nl_get_auxhist7_interval_m ( id_id , auxhist7_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_interval_m
  INTEGER id_id
  auxhist7_interval_m = model_config_rec%auxhist7_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_interval_m
SUBROUTINE nl_get_auxhist7_interval_s ( id_id , auxhist7_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_interval_s
  INTEGER id_id
  auxhist7_interval_s = model_config_rec%auxhist7_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_interval_s
SUBROUTINE nl_get_auxhist7_interval ( id_id , auxhist7_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_interval
  INTEGER id_id
  auxhist7_interval = model_config_rec%auxhist7_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_interval
SUBROUTINE nl_get_auxhist7_begin_y ( id_id , auxhist7_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_begin_y
  INTEGER id_id
  auxhist7_begin_y = model_config_rec%auxhist7_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_begin_y
SUBROUTINE nl_get_auxhist7_begin_d ( id_id , auxhist7_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_begin_d
  INTEGER id_id
  auxhist7_begin_d = model_config_rec%auxhist7_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_begin_d
SUBROUTINE nl_get_auxhist7_begin_h ( id_id , auxhist7_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_begin_h
  INTEGER id_id
  auxhist7_begin_h = model_config_rec%auxhist7_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_begin_h
SUBROUTINE nl_get_auxhist7_begin_m ( id_id , auxhist7_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_begin_m
  INTEGER id_id
  auxhist7_begin_m = model_config_rec%auxhist7_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_begin_m
SUBROUTINE nl_get_auxhist7_begin_s ( id_id , auxhist7_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_begin_s
  INTEGER id_id
  auxhist7_begin_s = model_config_rec%auxhist7_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_begin_s
SUBROUTINE nl_get_auxhist7_begin ( id_id , auxhist7_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_begin
  INTEGER id_id
  auxhist7_begin = model_config_rec%auxhist7_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_begin
SUBROUTINE nl_get_auxhist7_end_y ( id_id , auxhist7_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_end_y
  INTEGER id_id
  auxhist7_end_y = model_config_rec%auxhist7_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_end_y
SUBROUTINE nl_get_auxhist7_end_d ( id_id , auxhist7_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_end_d
  INTEGER id_id
  auxhist7_end_d = model_config_rec%auxhist7_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_end_d
SUBROUTINE nl_get_auxhist7_end_h ( id_id , auxhist7_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_end_h
  INTEGER id_id
  auxhist7_end_h = model_config_rec%auxhist7_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_end_h
SUBROUTINE nl_get_auxhist7_end_m ( id_id , auxhist7_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_end_m
  INTEGER id_id
  auxhist7_end_m = model_config_rec%auxhist7_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_end_m
SUBROUTINE nl_get_auxhist7_end_s ( id_id , auxhist7_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_end_s
  INTEGER id_id
  auxhist7_end_s = model_config_rec%auxhist7_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_end_s
SUBROUTINE nl_get_auxhist7_end ( id_id , auxhist7_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist7_end
  INTEGER id_id
  auxhist7_end = model_config_rec%auxhist7_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist7_end
SUBROUTINE nl_get_io_form_auxhist7 ( id_id , io_form_auxhist7 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist7
  INTEGER id_id
  io_form_auxhist7 = model_config_rec%io_form_auxhist7
  RETURN
END SUBROUTINE nl_get_io_form_auxhist7
SUBROUTINE nl_get_frames_per_auxhist7 ( id_id , frames_per_auxhist7 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist7
  INTEGER id_id
  frames_per_auxhist7 = model_config_rec%frames_per_auxhist7(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist7
SUBROUTINE nl_get_auxhist8_inname ( id_id , auxhist8_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist8_inname
  INTEGER id_id
  auxhist8_inname = trim(model_config_rec%auxhist8_inname)
  RETURN
END SUBROUTINE nl_get_auxhist8_inname
SUBROUTINE nl_get_auxhist8_outname ( id_id , auxhist8_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist8_outname
  INTEGER id_id
  auxhist8_outname = trim(model_config_rec%auxhist8_outname)
  RETURN
END SUBROUTINE nl_get_auxhist8_outname
SUBROUTINE nl_get_auxhist8_interval_y ( id_id , auxhist8_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_interval_y
  INTEGER id_id
  auxhist8_interval_y = model_config_rec%auxhist8_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_interval_y
SUBROUTINE nl_get_auxhist8_interval_d ( id_id , auxhist8_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_interval_d
  INTEGER id_id
  auxhist8_interval_d = model_config_rec%auxhist8_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_interval_d
SUBROUTINE nl_get_auxhist8_interval_h ( id_id , auxhist8_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_interval_h
  INTEGER id_id
  auxhist8_interval_h = model_config_rec%auxhist8_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_interval_h
SUBROUTINE nl_get_auxhist8_interval_m ( id_id , auxhist8_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_interval_m
  INTEGER id_id
  auxhist8_interval_m = model_config_rec%auxhist8_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_interval_m
SUBROUTINE nl_get_auxhist8_interval_s ( id_id , auxhist8_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_interval_s
  INTEGER id_id
  auxhist8_interval_s = model_config_rec%auxhist8_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_interval_s
SUBROUTINE nl_get_auxhist8_interval ( id_id , auxhist8_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_interval
  INTEGER id_id
  auxhist8_interval = model_config_rec%auxhist8_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_interval
SUBROUTINE nl_get_auxhist8_begin_y ( id_id , auxhist8_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_begin_y
  INTEGER id_id
  auxhist8_begin_y = model_config_rec%auxhist8_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_begin_y
SUBROUTINE nl_get_auxhist8_begin_d ( id_id , auxhist8_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_begin_d
  INTEGER id_id
  auxhist8_begin_d = model_config_rec%auxhist8_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_begin_d
SUBROUTINE nl_get_auxhist8_begin_h ( id_id , auxhist8_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_begin_h
  INTEGER id_id
  auxhist8_begin_h = model_config_rec%auxhist8_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_begin_h
SUBROUTINE nl_get_auxhist8_begin_m ( id_id , auxhist8_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_begin_m
  INTEGER id_id
  auxhist8_begin_m = model_config_rec%auxhist8_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_begin_m
SUBROUTINE nl_get_auxhist8_begin_s ( id_id , auxhist8_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_begin_s
  INTEGER id_id
  auxhist8_begin_s = model_config_rec%auxhist8_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_begin_s
SUBROUTINE nl_get_auxhist8_begin ( id_id , auxhist8_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_begin
  INTEGER id_id
  auxhist8_begin = model_config_rec%auxhist8_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_begin
SUBROUTINE nl_get_auxhist8_end_y ( id_id , auxhist8_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_end_y
  INTEGER id_id
  auxhist8_end_y = model_config_rec%auxhist8_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_end_y
SUBROUTINE nl_get_auxhist8_end_d ( id_id , auxhist8_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_end_d
  INTEGER id_id
  auxhist8_end_d = model_config_rec%auxhist8_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_end_d
SUBROUTINE nl_get_auxhist8_end_h ( id_id , auxhist8_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_end_h
  INTEGER id_id
  auxhist8_end_h = model_config_rec%auxhist8_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_end_h
SUBROUTINE nl_get_auxhist8_end_m ( id_id , auxhist8_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_end_m
  INTEGER id_id
  auxhist8_end_m = model_config_rec%auxhist8_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_end_m
SUBROUTINE nl_get_auxhist8_end_s ( id_id , auxhist8_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_end_s
  INTEGER id_id
  auxhist8_end_s = model_config_rec%auxhist8_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_end_s
SUBROUTINE nl_get_auxhist8_end ( id_id , auxhist8_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist8_end
  INTEGER id_id
  auxhist8_end = model_config_rec%auxhist8_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist8_end
SUBROUTINE nl_get_io_form_auxhist8 ( id_id , io_form_auxhist8 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist8
  INTEGER id_id
  io_form_auxhist8 = model_config_rec%io_form_auxhist8
  RETURN
END SUBROUTINE nl_get_io_form_auxhist8
SUBROUTINE nl_get_frames_per_auxhist8 ( id_id , frames_per_auxhist8 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist8
  INTEGER id_id
  frames_per_auxhist8 = model_config_rec%frames_per_auxhist8(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist8
SUBROUTINE nl_get_auxhist9_inname ( id_id , auxhist9_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist9_inname
  INTEGER id_id
  auxhist9_inname = trim(model_config_rec%auxhist9_inname)
  RETURN
END SUBROUTINE nl_get_auxhist9_inname
SUBROUTINE nl_get_auxhist9_outname ( id_id , auxhist9_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist9_outname
  INTEGER id_id
  auxhist9_outname = trim(model_config_rec%auxhist9_outname)
  RETURN
END SUBROUTINE nl_get_auxhist9_outname
SUBROUTINE nl_get_auxhist9_interval_y ( id_id , auxhist9_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_interval_y
  INTEGER id_id
  auxhist9_interval_y = model_config_rec%auxhist9_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_interval_y
SUBROUTINE nl_get_auxhist9_interval_d ( id_id , auxhist9_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_interval_d
  INTEGER id_id
  auxhist9_interval_d = model_config_rec%auxhist9_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_interval_d
SUBROUTINE nl_get_auxhist9_interval_h ( id_id , auxhist9_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_interval_h
  INTEGER id_id
  auxhist9_interval_h = model_config_rec%auxhist9_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_interval_h
SUBROUTINE nl_get_auxhist9_interval_m ( id_id , auxhist9_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_interval_m
  INTEGER id_id
  auxhist9_interval_m = model_config_rec%auxhist9_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_interval_m
SUBROUTINE nl_get_auxhist9_interval_s ( id_id , auxhist9_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_interval_s
  INTEGER id_id
  auxhist9_interval_s = model_config_rec%auxhist9_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_interval_s
SUBROUTINE nl_get_auxhist9_interval ( id_id , auxhist9_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_interval
  INTEGER id_id
  auxhist9_interval = model_config_rec%auxhist9_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_interval
SUBROUTINE nl_get_auxhist9_begin_y ( id_id , auxhist9_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_begin_y
  INTEGER id_id
  auxhist9_begin_y = model_config_rec%auxhist9_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_begin_y
SUBROUTINE nl_get_auxhist9_begin_d ( id_id , auxhist9_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_begin_d
  INTEGER id_id
  auxhist9_begin_d = model_config_rec%auxhist9_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_begin_d
SUBROUTINE nl_get_auxhist9_begin_h ( id_id , auxhist9_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_begin_h
  INTEGER id_id
  auxhist9_begin_h = model_config_rec%auxhist9_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_begin_h
SUBROUTINE nl_get_auxhist9_begin_m ( id_id , auxhist9_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_begin_m
  INTEGER id_id
  auxhist9_begin_m = model_config_rec%auxhist9_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_begin_m
SUBROUTINE nl_get_auxhist9_begin_s ( id_id , auxhist9_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_begin_s
  INTEGER id_id
  auxhist9_begin_s = model_config_rec%auxhist9_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_begin_s
SUBROUTINE nl_get_auxhist9_begin ( id_id , auxhist9_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_begin
  INTEGER id_id
  auxhist9_begin = model_config_rec%auxhist9_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_begin
SUBROUTINE nl_get_auxhist9_end_y ( id_id , auxhist9_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_end_y
  INTEGER id_id
  auxhist9_end_y = model_config_rec%auxhist9_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_end_y
SUBROUTINE nl_get_auxhist9_end_d ( id_id , auxhist9_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_end_d
  INTEGER id_id
  auxhist9_end_d = model_config_rec%auxhist9_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_end_d
SUBROUTINE nl_get_auxhist9_end_h ( id_id , auxhist9_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_end_h
  INTEGER id_id
  auxhist9_end_h = model_config_rec%auxhist9_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_end_h
SUBROUTINE nl_get_auxhist9_end_m ( id_id , auxhist9_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_end_m
  INTEGER id_id
  auxhist9_end_m = model_config_rec%auxhist9_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_end_m
SUBROUTINE nl_get_auxhist9_end_s ( id_id , auxhist9_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_end_s
  INTEGER id_id
  auxhist9_end_s = model_config_rec%auxhist9_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_end_s
SUBROUTINE nl_get_auxhist9_end ( id_id , auxhist9_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist9_end
  INTEGER id_id
  auxhist9_end = model_config_rec%auxhist9_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist9_end
SUBROUTINE nl_get_io_form_auxhist9 ( id_id , io_form_auxhist9 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist9
  INTEGER id_id
  io_form_auxhist9 = model_config_rec%io_form_auxhist9
  RETURN
END SUBROUTINE nl_get_io_form_auxhist9
SUBROUTINE nl_get_frames_per_auxhist9 ( id_id , frames_per_auxhist9 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist9
  INTEGER id_id
  frames_per_auxhist9 = model_config_rec%frames_per_auxhist9(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist9
SUBROUTINE nl_get_auxhist10_inname ( id_id , auxhist10_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist10_inname
  INTEGER id_id
  auxhist10_inname = trim(model_config_rec%auxhist10_inname)
  RETURN
END SUBROUTINE nl_get_auxhist10_inname
SUBROUTINE nl_get_auxhist10_outname ( id_id , auxhist10_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist10_outname
  INTEGER id_id
  auxhist10_outname = trim(model_config_rec%auxhist10_outname)
  RETURN
END SUBROUTINE nl_get_auxhist10_outname
SUBROUTINE nl_get_auxhist10_interval_y ( id_id , auxhist10_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_interval_y
  INTEGER id_id
  auxhist10_interval_y = model_config_rec%auxhist10_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_interval_y
SUBROUTINE nl_get_auxhist10_interval_d ( id_id , auxhist10_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_interval_d
  INTEGER id_id
  auxhist10_interval_d = model_config_rec%auxhist10_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_interval_d
SUBROUTINE nl_get_auxhist10_interval_h ( id_id , auxhist10_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_interval_h
  INTEGER id_id
  auxhist10_interval_h = model_config_rec%auxhist10_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_interval_h
SUBROUTINE nl_get_auxhist10_interval_m ( id_id , auxhist10_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_interval_m
  INTEGER id_id
  auxhist10_interval_m = model_config_rec%auxhist10_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_interval_m
SUBROUTINE nl_get_auxhist10_interval_s ( id_id , auxhist10_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_interval_s
  INTEGER id_id
  auxhist10_interval_s = model_config_rec%auxhist10_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_interval_s
SUBROUTINE nl_get_auxhist10_interval ( id_id , auxhist10_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_interval
  INTEGER id_id
  auxhist10_interval = model_config_rec%auxhist10_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_interval
SUBROUTINE nl_get_auxhist10_begin_y ( id_id , auxhist10_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_begin_y
  INTEGER id_id
  auxhist10_begin_y = model_config_rec%auxhist10_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_begin_y
SUBROUTINE nl_get_auxhist10_begin_d ( id_id , auxhist10_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_begin_d
  INTEGER id_id
  auxhist10_begin_d = model_config_rec%auxhist10_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_begin_d
SUBROUTINE nl_get_auxhist10_begin_h ( id_id , auxhist10_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_begin_h
  INTEGER id_id
  auxhist10_begin_h = model_config_rec%auxhist10_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_begin_h
SUBROUTINE nl_get_auxhist10_begin_m ( id_id , auxhist10_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_begin_m
  INTEGER id_id
  auxhist10_begin_m = model_config_rec%auxhist10_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_begin_m
SUBROUTINE nl_get_auxhist10_begin_s ( id_id , auxhist10_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_begin_s
  INTEGER id_id
  auxhist10_begin_s = model_config_rec%auxhist10_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_begin_s
SUBROUTINE nl_get_auxhist10_begin ( id_id , auxhist10_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_begin
  INTEGER id_id
  auxhist10_begin = model_config_rec%auxhist10_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_begin
SUBROUTINE nl_get_auxhist10_end_y ( id_id , auxhist10_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_end_y
  INTEGER id_id
  auxhist10_end_y = model_config_rec%auxhist10_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_end_y
SUBROUTINE nl_get_auxhist10_end_d ( id_id , auxhist10_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_end_d
  INTEGER id_id
  auxhist10_end_d = model_config_rec%auxhist10_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_end_d
SUBROUTINE nl_get_auxhist10_end_h ( id_id , auxhist10_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_end_h
  INTEGER id_id
  auxhist10_end_h = model_config_rec%auxhist10_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_end_h
SUBROUTINE nl_get_auxhist10_end_m ( id_id , auxhist10_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_end_m
  INTEGER id_id
  auxhist10_end_m = model_config_rec%auxhist10_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_end_m
SUBROUTINE nl_get_auxhist10_end_s ( id_id , auxhist10_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_end_s
  INTEGER id_id
  auxhist10_end_s = model_config_rec%auxhist10_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_end_s
SUBROUTINE nl_get_auxhist10_end ( id_id , auxhist10_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist10_end
  INTEGER id_id
  auxhist10_end = model_config_rec%auxhist10_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist10_end
SUBROUTINE nl_get_io_form_auxhist10 ( id_id , io_form_auxhist10 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist10
  INTEGER id_id
  io_form_auxhist10 = model_config_rec%io_form_auxhist10
  RETURN
END SUBROUTINE nl_get_io_form_auxhist10
SUBROUTINE nl_get_frames_per_auxhist10 ( id_id , frames_per_auxhist10 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist10
  INTEGER id_id
  frames_per_auxhist10 = model_config_rec%frames_per_auxhist10(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist10
SUBROUTINE nl_get_auxhist11_inname ( id_id , auxhist11_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist11_inname
  INTEGER id_id
  auxhist11_inname = trim(model_config_rec%auxhist11_inname)
  RETURN
END SUBROUTINE nl_get_auxhist11_inname
SUBROUTINE nl_get_auxhist11_outname ( id_id , auxhist11_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist11_outname
  INTEGER id_id
  auxhist11_outname = trim(model_config_rec%auxhist11_outname)
  RETURN
END SUBROUTINE nl_get_auxhist11_outname
SUBROUTINE nl_get_auxhist11_interval_y ( id_id , auxhist11_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_interval_y
  INTEGER id_id
  auxhist11_interval_y = model_config_rec%auxhist11_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_interval_y
SUBROUTINE nl_get_auxhist11_interval_d ( id_id , auxhist11_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_interval_d
  INTEGER id_id
  auxhist11_interval_d = model_config_rec%auxhist11_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_interval_d
SUBROUTINE nl_get_auxhist11_interval_h ( id_id , auxhist11_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_interval_h
  INTEGER id_id
  auxhist11_interval_h = model_config_rec%auxhist11_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_interval_h
SUBROUTINE nl_get_auxhist11_interval_m ( id_id , auxhist11_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_interval_m
  INTEGER id_id
  auxhist11_interval_m = model_config_rec%auxhist11_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_interval_m
SUBROUTINE nl_get_auxhist11_interval_s ( id_id , auxhist11_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_interval_s
  INTEGER id_id
  auxhist11_interval_s = model_config_rec%auxhist11_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_interval_s
SUBROUTINE nl_get_auxhist11_interval ( id_id , auxhist11_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_interval
  INTEGER id_id
  auxhist11_interval = model_config_rec%auxhist11_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_interval
SUBROUTINE nl_get_auxhist11_begin_y ( id_id , auxhist11_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_begin_y
  INTEGER id_id
  auxhist11_begin_y = model_config_rec%auxhist11_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_begin_y
SUBROUTINE nl_get_auxhist11_begin_d ( id_id , auxhist11_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_begin_d
  INTEGER id_id
  auxhist11_begin_d = model_config_rec%auxhist11_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_begin_d
SUBROUTINE nl_get_auxhist11_begin_h ( id_id , auxhist11_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_begin_h
  INTEGER id_id
  auxhist11_begin_h = model_config_rec%auxhist11_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_begin_h
SUBROUTINE nl_get_auxhist11_begin_m ( id_id , auxhist11_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_begin_m
  INTEGER id_id
  auxhist11_begin_m = model_config_rec%auxhist11_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_begin_m
SUBROUTINE nl_get_auxhist11_begin_s ( id_id , auxhist11_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_begin_s
  INTEGER id_id
  auxhist11_begin_s = model_config_rec%auxhist11_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_begin_s
SUBROUTINE nl_get_auxhist11_begin ( id_id , auxhist11_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_begin
  INTEGER id_id
  auxhist11_begin = model_config_rec%auxhist11_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_begin
SUBROUTINE nl_get_auxhist11_end_y ( id_id , auxhist11_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_end_y
  INTEGER id_id
  auxhist11_end_y = model_config_rec%auxhist11_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_end_y
SUBROUTINE nl_get_auxhist11_end_d ( id_id , auxhist11_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_end_d
  INTEGER id_id
  auxhist11_end_d = model_config_rec%auxhist11_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_end_d
SUBROUTINE nl_get_auxhist11_end_h ( id_id , auxhist11_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_end_h
  INTEGER id_id
  auxhist11_end_h = model_config_rec%auxhist11_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_end_h
SUBROUTINE nl_get_auxhist11_end_m ( id_id , auxhist11_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_end_m
  INTEGER id_id
  auxhist11_end_m = model_config_rec%auxhist11_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_end_m
SUBROUTINE nl_get_auxhist11_end_s ( id_id , auxhist11_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_end_s
  INTEGER id_id
  auxhist11_end_s = model_config_rec%auxhist11_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_end_s
SUBROUTINE nl_get_auxhist11_end ( id_id , auxhist11_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist11_end
  INTEGER id_id
  auxhist11_end = model_config_rec%auxhist11_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist11_end
SUBROUTINE nl_get_io_form_auxhist11 ( id_id , io_form_auxhist11 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist11
  INTEGER id_id
  io_form_auxhist11 = model_config_rec%io_form_auxhist11
  RETURN
END SUBROUTINE nl_get_io_form_auxhist11
SUBROUTINE nl_get_frames_per_auxhist11 ( id_id , frames_per_auxhist11 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist11
  INTEGER id_id
  frames_per_auxhist11 = model_config_rec%frames_per_auxhist11(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist11
SUBROUTINE nl_get_auxhist12_inname ( id_id , auxhist12_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist12_inname
  INTEGER id_id
  auxhist12_inname = trim(model_config_rec%auxhist12_inname)
  RETURN
END SUBROUTINE nl_get_auxhist12_inname
SUBROUTINE nl_get_auxhist12_outname ( id_id , auxhist12_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist12_outname
  INTEGER id_id
  auxhist12_outname = trim(model_config_rec%auxhist12_outname)
  RETURN
END SUBROUTINE nl_get_auxhist12_outname
SUBROUTINE nl_get_auxhist12_interval_y ( id_id , auxhist12_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist12_interval_y
  INTEGER id_id
  auxhist12_interval_y = model_config_rec%auxhist12_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist12_interval_y
SUBROUTINE nl_get_auxhist12_interval_d ( id_id , auxhist12_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist12_interval_d
  INTEGER id_id
  auxhist12_interval_d = model_config_rec%auxhist12_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist12_interval_d
SUBROUTINE nl_get_auxhist12_interval_h ( id_id , auxhist12_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist12_interval_h
  INTEGER id_id
  auxhist12_interval_h = model_config_rec%auxhist12_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist12_interval_h
SUBROUTINE nl_get_auxhist12_interval_m ( id_id , auxhist12_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist12_interval_m
  INTEGER id_id
  auxhist12_interval_m = model_config_rec%auxhist12_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist12_interval_m
SUBROUTINE nl_get_auxhist12_interval_s ( id_id , auxhist12_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist12_interval_s
  INTEGER id_id
  auxhist12_interval_s = model_config_rec%auxhist12_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist12_interval_s
SUBROUTINE nl_get_auxhist12_interval ( id_id , auxhist12_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist12_interval
  INTEGER id_id
  auxhist12_interval = model_config_rec%auxhist12_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist12_interval
SUBROUTINE nl_get_auxhist12_begin_y ( id_id , auxhist12_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist12_begin_y
  INTEGER id_id
  auxhist12_begin_y = model_config_rec%auxhist12_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist12_begin_y
SUBROUTINE nl_get_auxhist12_begin_d ( id_id , auxhist12_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist12_begin_d
  INTEGER id_id
  auxhist12_begin_d = model_config_rec%auxhist12_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist12_begin_d
SUBROUTINE nl_get_auxhist12_begin_h ( id_id , auxhist12_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist12_begin_h
  INTEGER id_id
  auxhist12_begin_h = model_config_rec%auxhist12_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist12_begin_h
SUBROUTINE nl_get_auxhist12_begin_m ( id_id , auxhist12_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist12_begin_m
  INTEGER id_id
  auxhist12_begin_m = model_config_rec%auxhist12_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist12_begin_m
SUBROUTINE nl_get_auxhist12_begin_s ( id_id , auxhist12_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist12_begin_s
  INTEGER id_id
  auxhist12_begin_s = model_config_rec%auxhist12_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist12_begin_s
SUBROUTINE nl_get_auxhist12_begin ( id_id , auxhist12_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist12_begin
  INTEGER id_id
  auxhist12_begin = model_config_rec%auxhist12_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist12_begin
SUBROUTINE nl_get_auxhist12_end_y ( id_id , auxhist12_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist12_end_y
  INTEGER id_id
  auxhist12_end_y = model_config_rec%auxhist12_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist12_end_y
SUBROUTINE nl_get_auxhist12_end_d ( id_id , auxhist12_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist12_end_d
  INTEGER id_id
  auxhist12_end_d = model_config_rec%auxhist12_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist12_end_d
SUBROUTINE nl_get_auxhist12_end_h ( id_id , auxhist12_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist12_end_h
  INTEGER id_id
  auxhist12_end_h = model_config_rec%auxhist12_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist12_end_h
SUBROUTINE nl_get_auxhist12_end_m ( id_id , auxhist12_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist12_end_m
  INTEGER id_id
  auxhist12_end_m = model_config_rec%auxhist12_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist12_end_m
SUBROUTINE nl_get_auxhist12_end_s ( id_id , auxhist12_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist12_end_s
  INTEGER id_id
  auxhist12_end_s = model_config_rec%auxhist12_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist12_end_s
SUBROUTINE nl_get_auxhist12_end ( id_id , auxhist12_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist12_end
  INTEGER id_id
  auxhist12_end = model_config_rec%auxhist12_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist12_end
SUBROUTINE nl_get_io_form_auxhist12 ( id_id , io_form_auxhist12 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist12
  INTEGER id_id
  io_form_auxhist12 = model_config_rec%io_form_auxhist12
  RETURN
END SUBROUTINE nl_get_io_form_auxhist12
SUBROUTINE nl_get_frames_per_auxhist12 ( id_id , frames_per_auxhist12 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist12
  INTEGER id_id
  frames_per_auxhist12 = model_config_rec%frames_per_auxhist12(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist12
SUBROUTINE nl_get_auxhist13_inname ( id_id , auxhist13_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist13_inname
  INTEGER id_id
  auxhist13_inname = trim(model_config_rec%auxhist13_inname)
  RETURN
END SUBROUTINE nl_get_auxhist13_inname
SUBROUTINE nl_get_auxhist13_outname ( id_id , auxhist13_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist13_outname
  INTEGER id_id
  auxhist13_outname = trim(model_config_rec%auxhist13_outname)
  RETURN
END SUBROUTINE nl_get_auxhist13_outname
SUBROUTINE nl_get_auxhist13_interval_y ( id_id , auxhist13_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist13_interval_y
  INTEGER id_id
  auxhist13_interval_y = model_config_rec%auxhist13_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist13_interval_y
SUBROUTINE nl_get_auxhist13_interval_d ( id_id , auxhist13_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist13_interval_d
  INTEGER id_id
  auxhist13_interval_d = model_config_rec%auxhist13_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist13_interval_d
SUBROUTINE nl_get_auxhist13_interval_h ( id_id , auxhist13_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist13_interval_h
  INTEGER id_id
  auxhist13_interval_h = model_config_rec%auxhist13_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist13_interval_h
SUBROUTINE nl_get_auxhist13_interval_m ( id_id , auxhist13_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist13_interval_m
  INTEGER id_id
  auxhist13_interval_m = model_config_rec%auxhist13_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist13_interval_m
SUBROUTINE nl_get_auxhist13_interval_s ( id_id , auxhist13_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist13_interval_s
  INTEGER id_id
  auxhist13_interval_s = model_config_rec%auxhist13_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist13_interval_s
SUBROUTINE nl_get_auxhist13_interval ( id_id , auxhist13_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist13_interval
  INTEGER id_id
  auxhist13_interval = model_config_rec%auxhist13_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist13_interval
SUBROUTINE nl_get_auxhist13_begin_y ( id_id , auxhist13_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist13_begin_y
  INTEGER id_id
  auxhist13_begin_y = model_config_rec%auxhist13_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist13_begin_y
SUBROUTINE nl_get_auxhist13_begin_d ( id_id , auxhist13_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist13_begin_d
  INTEGER id_id
  auxhist13_begin_d = model_config_rec%auxhist13_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist13_begin_d
SUBROUTINE nl_get_auxhist13_begin_h ( id_id , auxhist13_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist13_begin_h
  INTEGER id_id
  auxhist13_begin_h = model_config_rec%auxhist13_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist13_begin_h
SUBROUTINE nl_get_auxhist13_begin_m ( id_id , auxhist13_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist13_begin_m
  INTEGER id_id
  auxhist13_begin_m = model_config_rec%auxhist13_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist13_begin_m
SUBROUTINE nl_get_auxhist13_begin_s ( id_id , auxhist13_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist13_begin_s
  INTEGER id_id
  auxhist13_begin_s = model_config_rec%auxhist13_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist13_begin_s
SUBROUTINE nl_get_auxhist13_begin ( id_id , auxhist13_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist13_begin
  INTEGER id_id
  auxhist13_begin = model_config_rec%auxhist13_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist13_begin
SUBROUTINE nl_get_auxhist13_end_y ( id_id , auxhist13_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist13_end_y
  INTEGER id_id
  auxhist13_end_y = model_config_rec%auxhist13_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist13_end_y
SUBROUTINE nl_get_auxhist13_end_d ( id_id , auxhist13_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist13_end_d
  INTEGER id_id
  auxhist13_end_d = model_config_rec%auxhist13_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist13_end_d
SUBROUTINE nl_get_auxhist13_end_h ( id_id , auxhist13_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist13_end_h
  INTEGER id_id
  auxhist13_end_h = model_config_rec%auxhist13_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist13_end_h
SUBROUTINE nl_get_auxhist13_end_m ( id_id , auxhist13_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist13_end_m
  INTEGER id_id
  auxhist13_end_m = model_config_rec%auxhist13_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist13_end_m
SUBROUTINE nl_get_auxhist13_end_s ( id_id , auxhist13_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist13_end_s
  INTEGER id_id
  auxhist13_end_s = model_config_rec%auxhist13_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist13_end_s
SUBROUTINE nl_get_auxhist13_end ( id_id , auxhist13_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist13_end
  INTEGER id_id
  auxhist13_end = model_config_rec%auxhist13_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist13_end
SUBROUTINE nl_get_io_form_auxhist13 ( id_id , io_form_auxhist13 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist13
  INTEGER id_id
  io_form_auxhist13 = model_config_rec%io_form_auxhist13
  RETURN
END SUBROUTINE nl_get_io_form_auxhist13
SUBROUTINE nl_get_frames_per_auxhist13 ( id_id , frames_per_auxhist13 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist13
  INTEGER id_id
  frames_per_auxhist13 = model_config_rec%frames_per_auxhist13(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist13
SUBROUTINE nl_get_auxhist14_inname ( id_id , auxhist14_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist14_inname
  INTEGER id_id
  auxhist14_inname = trim(model_config_rec%auxhist14_inname)
  RETURN
END SUBROUTINE nl_get_auxhist14_inname
SUBROUTINE nl_get_auxhist14_outname ( id_id , auxhist14_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist14_outname
  INTEGER id_id
  auxhist14_outname = trim(model_config_rec%auxhist14_outname)
  RETURN
END SUBROUTINE nl_get_auxhist14_outname
SUBROUTINE nl_get_auxhist14_interval_y ( id_id , auxhist14_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist14_interval_y
  INTEGER id_id
  auxhist14_interval_y = model_config_rec%auxhist14_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist14_interval_y
SUBROUTINE nl_get_auxhist14_interval_d ( id_id , auxhist14_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist14_interval_d
  INTEGER id_id
  auxhist14_interval_d = model_config_rec%auxhist14_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist14_interval_d
SUBROUTINE nl_get_auxhist14_interval_h ( id_id , auxhist14_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist14_interval_h
  INTEGER id_id
  auxhist14_interval_h = model_config_rec%auxhist14_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist14_interval_h
SUBROUTINE nl_get_auxhist14_interval_m ( id_id , auxhist14_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist14_interval_m
  INTEGER id_id
  auxhist14_interval_m = model_config_rec%auxhist14_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist14_interval_m
SUBROUTINE nl_get_auxhist14_interval_s ( id_id , auxhist14_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist14_interval_s
  INTEGER id_id
  auxhist14_interval_s = model_config_rec%auxhist14_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist14_interval_s
SUBROUTINE nl_get_auxhist14_interval ( id_id , auxhist14_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist14_interval
  INTEGER id_id
  auxhist14_interval = model_config_rec%auxhist14_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist14_interval
SUBROUTINE nl_get_auxhist14_begin_y ( id_id , auxhist14_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist14_begin_y
  INTEGER id_id
  auxhist14_begin_y = model_config_rec%auxhist14_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist14_begin_y
SUBROUTINE nl_get_auxhist14_begin_d ( id_id , auxhist14_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist14_begin_d
  INTEGER id_id
  auxhist14_begin_d = model_config_rec%auxhist14_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist14_begin_d
SUBROUTINE nl_get_auxhist14_begin_h ( id_id , auxhist14_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist14_begin_h
  INTEGER id_id
  auxhist14_begin_h = model_config_rec%auxhist14_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist14_begin_h
SUBROUTINE nl_get_auxhist14_begin_m ( id_id , auxhist14_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist14_begin_m
  INTEGER id_id
  auxhist14_begin_m = model_config_rec%auxhist14_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist14_begin_m
SUBROUTINE nl_get_auxhist14_begin_s ( id_id , auxhist14_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist14_begin_s
  INTEGER id_id
  auxhist14_begin_s = model_config_rec%auxhist14_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist14_begin_s
#endif
#if (NNN == 2)
SUBROUTINE nl_get_auxhist14_begin ( id_id , auxhist14_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist14_begin
  INTEGER id_id
  auxhist14_begin = model_config_rec%auxhist14_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist14_begin
SUBROUTINE nl_get_auxhist14_end_y ( id_id , auxhist14_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist14_end_y
  INTEGER id_id
  auxhist14_end_y = model_config_rec%auxhist14_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist14_end_y
SUBROUTINE nl_get_auxhist14_end_d ( id_id , auxhist14_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist14_end_d
  INTEGER id_id
  auxhist14_end_d = model_config_rec%auxhist14_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist14_end_d
SUBROUTINE nl_get_auxhist14_end_h ( id_id , auxhist14_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist14_end_h
  INTEGER id_id
  auxhist14_end_h = model_config_rec%auxhist14_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist14_end_h
SUBROUTINE nl_get_auxhist14_end_m ( id_id , auxhist14_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist14_end_m
  INTEGER id_id
  auxhist14_end_m = model_config_rec%auxhist14_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist14_end_m
SUBROUTINE nl_get_auxhist14_end_s ( id_id , auxhist14_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist14_end_s
  INTEGER id_id
  auxhist14_end_s = model_config_rec%auxhist14_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist14_end_s
SUBROUTINE nl_get_auxhist14_end ( id_id , auxhist14_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist14_end
  INTEGER id_id
  auxhist14_end = model_config_rec%auxhist14_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist14_end
SUBROUTINE nl_get_io_form_auxhist14 ( id_id , io_form_auxhist14 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist14
  INTEGER id_id
  io_form_auxhist14 = model_config_rec%io_form_auxhist14
  RETURN
END SUBROUTINE nl_get_io_form_auxhist14
SUBROUTINE nl_get_frames_per_auxhist14 ( id_id , frames_per_auxhist14 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist14
  INTEGER id_id
  frames_per_auxhist14 = model_config_rec%frames_per_auxhist14(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist14
SUBROUTINE nl_get_auxhist15_inname ( id_id , auxhist15_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist15_inname
  INTEGER id_id
  auxhist15_inname = trim(model_config_rec%auxhist15_inname)
  RETURN
END SUBROUTINE nl_get_auxhist15_inname
SUBROUTINE nl_get_auxhist15_outname ( id_id , auxhist15_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist15_outname
  INTEGER id_id
  auxhist15_outname = trim(model_config_rec%auxhist15_outname)
  RETURN
END SUBROUTINE nl_get_auxhist15_outname
SUBROUTINE nl_get_auxhist15_interval_y ( id_id , auxhist15_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist15_interval_y
  INTEGER id_id
  auxhist15_interval_y = model_config_rec%auxhist15_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist15_interval_y
SUBROUTINE nl_get_auxhist15_interval_d ( id_id , auxhist15_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist15_interval_d
  INTEGER id_id
  auxhist15_interval_d = model_config_rec%auxhist15_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist15_interval_d
SUBROUTINE nl_get_auxhist15_interval_h ( id_id , auxhist15_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist15_interval_h
  INTEGER id_id
  auxhist15_interval_h = model_config_rec%auxhist15_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist15_interval_h
SUBROUTINE nl_get_auxhist15_interval_m ( id_id , auxhist15_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist15_interval_m
  INTEGER id_id
  auxhist15_interval_m = model_config_rec%auxhist15_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist15_interval_m
SUBROUTINE nl_get_auxhist15_interval_s ( id_id , auxhist15_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist15_interval_s
  INTEGER id_id
  auxhist15_interval_s = model_config_rec%auxhist15_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist15_interval_s
SUBROUTINE nl_get_auxhist15_interval ( id_id , auxhist15_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist15_interval
  INTEGER id_id
  auxhist15_interval = model_config_rec%auxhist15_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist15_interval
SUBROUTINE nl_get_auxhist15_begin_y ( id_id , auxhist15_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist15_begin_y
  INTEGER id_id
  auxhist15_begin_y = model_config_rec%auxhist15_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist15_begin_y
SUBROUTINE nl_get_auxhist15_begin_d ( id_id , auxhist15_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist15_begin_d
  INTEGER id_id
  auxhist15_begin_d = model_config_rec%auxhist15_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist15_begin_d
SUBROUTINE nl_get_auxhist15_begin_h ( id_id , auxhist15_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist15_begin_h
  INTEGER id_id
  auxhist15_begin_h = model_config_rec%auxhist15_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist15_begin_h
SUBROUTINE nl_get_auxhist15_begin_m ( id_id , auxhist15_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist15_begin_m
  INTEGER id_id
  auxhist15_begin_m = model_config_rec%auxhist15_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist15_begin_m
SUBROUTINE nl_get_auxhist15_begin_s ( id_id , auxhist15_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist15_begin_s
  INTEGER id_id
  auxhist15_begin_s = model_config_rec%auxhist15_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist15_begin_s
SUBROUTINE nl_get_auxhist15_begin ( id_id , auxhist15_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist15_begin
  INTEGER id_id
  auxhist15_begin = model_config_rec%auxhist15_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist15_begin
SUBROUTINE nl_get_auxhist15_end_y ( id_id , auxhist15_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist15_end_y
  INTEGER id_id
  auxhist15_end_y = model_config_rec%auxhist15_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist15_end_y
SUBROUTINE nl_get_auxhist15_end_d ( id_id , auxhist15_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist15_end_d
  INTEGER id_id
  auxhist15_end_d = model_config_rec%auxhist15_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist15_end_d
SUBROUTINE nl_get_auxhist15_end_h ( id_id , auxhist15_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist15_end_h
  INTEGER id_id
  auxhist15_end_h = model_config_rec%auxhist15_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist15_end_h
SUBROUTINE nl_get_auxhist15_end_m ( id_id , auxhist15_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist15_end_m
  INTEGER id_id
  auxhist15_end_m = model_config_rec%auxhist15_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist15_end_m
SUBROUTINE nl_get_auxhist15_end_s ( id_id , auxhist15_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist15_end_s
  INTEGER id_id
  auxhist15_end_s = model_config_rec%auxhist15_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist15_end_s
SUBROUTINE nl_get_auxhist15_end ( id_id , auxhist15_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist15_end
  INTEGER id_id
  auxhist15_end = model_config_rec%auxhist15_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist15_end
SUBROUTINE nl_get_io_form_auxhist15 ( id_id , io_form_auxhist15 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist15
  INTEGER id_id
  io_form_auxhist15 = model_config_rec%io_form_auxhist15
  RETURN
END SUBROUTINE nl_get_io_form_auxhist15
SUBROUTINE nl_get_frames_per_auxhist15 ( id_id , frames_per_auxhist15 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist15
  INTEGER id_id
  frames_per_auxhist15 = model_config_rec%frames_per_auxhist15(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist15
SUBROUTINE nl_get_auxhist16_inname ( id_id , auxhist16_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist16_inname
  INTEGER id_id
  auxhist16_inname = trim(model_config_rec%auxhist16_inname)
  RETURN
END SUBROUTINE nl_get_auxhist16_inname
SUBROUTINE nl_get_auxhist16_outname ( id_id , auxhist16_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist16_outname
  INTEGER id_id
  auxhist16_outname = trim(model_config_rec%auxhist16_outname)
  RETURN
END SUBROUTINE nl_get_auxhist16_outname
SUBROUTINE nl_get_auxhist16_interval_y ( id_id , auxhist16_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist16_interval_y
  INTEGER id_id
  auxhist16_interval_y = model_config_rec%auxhist16_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist16_interval_y
SUBROUTINE nl_get_auxhist16_interval_d ( id_id , auxhist16_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist16_interval_d
  INTEGER id_id
  auxhist16_interval_d = model_config_rec%auxhist16_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist16_interval_d
SUBROUTINE nl_get_auxhist16_interval_h ( id_id , auxhist16_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist16_interval_h
  INTEGER id_id
  auxhist16_interval_h = model_config_rec%auxhist16_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist16_interval_h
SUBROUTINE nl_get_auxhist16_interval_m ( id_id , auxhist16_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist16_interval_m
  INTEGER id_id
  auxhist16_interval_m = model_config_rec%auxhist16_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist16_interval_m
SUBROUTINE nl_get_auxhist16_interval_s ( id_id , auxhist16_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist16_interval_s
  INTEGER id_id
  auxhist16_interval_s = model_config_rec%auxhist16_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist16_interval_s
SUBROUTINE nl_get_auxhist16_interval ( id_id , auxhist16_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist16_interval
  INTEGER id_id
  auxhist16_interval = model_config_rec%auxhist16_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist16_interval
SUBROUTINE nl_get_auxhist16_begin_y ( id_id , auxhist16_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist16_begin_y
  INTEGER id_id
  auxhist16_begin_y = model_config_rec%auxhist16_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist16_begin_y
SUBROUTINE nl_get_auxhist16_begin_d ( id_id , auxhist16_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist16_begin_d
  INTEGER id_id
  auxhist16_begin_d = model_config_rec%auxhist16_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist16_begin_d
SUBROUTINE nl_get_auxhist16_begin_h ( id_id , auxhist16_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist16_begin_h
  INTEGER id_id
  auxhist16_begin_h = model_config_rec%auxhist16_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist16_begin_h
SUBROUTINE nl_get_auxhist16_begin_m ( id_id , auxhist16_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist16_begin_m
  INTEGER id_id
  auxhist16_begin_m = model_config_rec%auxhist16_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist16_begin_m
SUBROUTINE nl_get_auxhist16_begin_s ( id_id , auxhist16_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist16_begin_s
  INTEGER id_id
  auxhist16_begin_s = model_config_rec%auxhist16_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist16_begin_s
SUBROUTINE nl_get_auxhist16_begin ( id_id , auxhist16_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist16_begin
  INTEGER id_id
  auxhist16_begin = model_config_rec%auxhist16_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist16_begin
SUBROUTINE nl_get_auxhist16_end_y ( id_id , auxhist16_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist16_end_y
  INTEGER id_id
  auxhist16_end_y = model_config_rec%auxhist16_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist16_end_y
SUBROUTINE nl_get_auxhist16_end_d ( id_id , auxhist16_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist16_end_d
  INTEGER id_id
  auxhist16_end_d = model_config_rec%auxhist16_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist16_end_d
SUBROUTINE nl_get_auxhist16_end_h ( id_id , auxhist16_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist16_end_h
  INTEGER id_id
  auxhist16_end_h = model_config_rec%auxhist16_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist16_end_h
SUBROUTINE nl_get_auxhist16_end_m ( id_id , auxhist16_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist16_end_m
  INTEGER id_id
  auxhist16_end_m = model_config_rec%auxhist16_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist16_end_m
SUBROUTINE nl_get_auxhist16_end_s ( id_id , auxhist16_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist16_end_s
  INTEGER id_id
  auxhist16_end_s = model_config_rec%auxhist16_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist16_end_s
SUBROUTINE nl_get_auxhist16_end ( id_id , auxhist16_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist16_end
  INTEGER id_id
  auxhist16_end = model_config_rec%auxhist16_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist16_end
SUBROUTINE nl_get_io_form_auxhist16 ( id_id , io_form_auxhist16 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist16
  INTEGER id_id
  io_form_auxhist16 = model_config_rec%io_form_auxhist16
  RETURN
END SUBROUTINE nl_get_io_form_auxhist16
SUBROUTINE nl_get_frames_per_auxhist16 ( id_id , frames_per_auxhist16 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist16
  INTEGER id_id
  frames_per_auxhist16 = model_config_rec%frames_per_auxhist16(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist16
SUBROUTINE nl_get_auxhist17_inname ( id_id , auxhist17_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist17_inname
  INTEGER id_id
  auxhist17_inname = trim(model_config_rec%auxhist17_inname)
  RETURN
END SUBROUTINE nl_get_auxhist17_inname
SUBROUTINE nl_get_auxhist17_outname ( id_id , auxhist17_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist17_outname
  INTEGER id_id
  auxhist17_outname = trim(model_config_rec%auxhist17_outname)
  RETURN
END SUBROUTINE nl_get_auxhist17_outname
SUBROUTINE nl_get_auxhist17_interval_y ( id_id , auxhist17_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist17_interval_y
  INTEGER id_id
  auxhist17_interval_y = model_config_rec%auxhist17_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist17_interval_y
SUBROUTINE nl_get_auxhist17_interval_d ( id_id , auxhist17_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist17_interval_d
  INTEGER id_id
  auxhist17_interval_d = model_config_rec%auxhist17_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist17_interval_d
SUBROUTINE nl_get_auxhist17_interval_h ( id_id , auxhist17_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist17_interval_h
  INTEGER id_id
  auxhist17_interval_h = model_config_rec%auxhist17_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist17_interval_h
SUBROUTINE nl_get_auxhist17_interval_m ( id_id , auxhist17_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist17_interval_m
  INTEGER id_id
  auxhist17_interval_m = model_config_rec%auxhist17_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist17_interval_m
SUBROUTINE nl_get_auxhist17_interval_s ( id_id , auxhist17_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist17_interval_s
  INTEGER id_id
  auxhist17_interval_s = model_config_rec%auxhist17_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist17_interval_s
SUBROUTINE nl_get_auxhist17_interval ( id_id , auxhist17_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist17_interval
  INTEGER id_id
  auxhist17_interval = model_config_rec%auxhist17_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist17_interval
SUBROUTINE nl_get_auxhist17_begin_y ( id_id , auxhist17_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist17_begin_y
  INTEGER id_id
  auxhist17_begin_y = model_config_rec%auxhist17_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist17_begin_y
SUBROUTINE nl_get_auxhist17_begin_d ( id_id , auxhist17_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist17_begin_d
  INTEGER id_id
  auxhist17_begin_d = model_config_rec%auxhist17_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist17_begin_d
SUBROUTINE nl_get_auxhist17_begin_h ( id_id , auxhist17_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist17_begin_h
  INTEGER id_id
  auxhist17_begin_h = model_config_rec%auxhist17_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist17_begin_h
SUBROUTINE nl_get_auxhist17_begin_m ( id_id , auxhist17_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist17_begin_m
  INTEGER id_id
  auxhist17_begin_m = model_config_rec%auxhist17_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist17_begin_m
SUBROUTINE nl_get_auxhist17_begin_s ( id_id , auxhist17_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist17_begin_s
  INTEGER id_id
  auxhist17_begin_s = model_config_rec%auxhist17_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist17_begin_s
SUBROUTINE nl_get_auxhist17_begin ( id_id , auxhist17_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist17_begin
  INTEGER id_id
  auxhist17_begin = model_config_rec%auxhist17_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist17_begin
SUBROUTINE nl_get_auxhist17_end_y ( id_id , auxhist17_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist17_end_y
  INTEGER id_id
  auxhist17_end_y = model_config_rec%auxhist17_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist17_end_y
SUBROUTINE nl_get_auxhist17_end_d ( id_id , auxhist17_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist17_end_d
  INTEGER id_id
  auxhist17_end_d = model_config_rec%auxhist17_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist17_end_d
SUBROUTINE nl_get_auxhist17_end_h ( id_id , auxhist17_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist17_end_h
  INTEGER id_id
  auxhist17_end_h = model_config_rec%auxhist17_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist17_end_h
SUBROUTINE nl_get_auxhist17_end_m ( id_id , auxhist17_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist17_end_m
  INTEGER id_id
  auxhist17_end_m = model_config_rec%auxhist17_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist17_end_m
SUBROUTINE nl_get_auxhist17_end_s ( id_id , auxhist17_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist17_end_s
  INTEGER id_id
  auxhist17_end_s = model_config_rec%auxhist17_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist17_end_s
SUBROUTINE nl_get_auxhist17_end ( id_id , auxhist17_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist17_end
  INTEGER id_id
  auxhist17_end = model_config_rec%auxhist17_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist17_end
SUBROUTINE nl_get_io_form_auxhist17 ( id_id , io_form_auxhist17 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist17
  INTEGER id_id
  io_form_auxhist17 = model_config_rec%io_form_auxhist17
  RETURN
END SUBROUTINE nl_get_io_form_auxhist17
SUBROUTINE nl_get_frames_per_auxhist17 ( id_id , frames_per_auxhist17 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist17
  INTEGER id_id
  frames_per_auxhist17 = model_config_rec%frames_per_auxhist17(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist17
SUBROUTINE nl_get_auxhist18_inname ( id_id , auxhist18_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist18_inname
  INTEGER id_id
  auxhist18_inname = trim(model_config_rec%auxhist18_inname)
  RETURN
END SUBROUTINE nl_get_auxhist18_inname
SUBROUTINE nl_get_auxhist18_outname ( id_id , auxhist18_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist18_outname
  INTEGER id_id
  auxhist18_outname = trim(model_config_rec%auxhist18_outname)
  RETURN
END SUBROUTINE nl_get_auxhist18_outname
SUBROUTINE nl_get_auxhist18_interval_y ( id_id , auxhist18_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist18_interval_y
  INTEGER id_id
  auxhist18_interval_y = model_config_rec%auxhist18_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist18_interval_y
SUBROUTINE nl_get_auxhist18_interval_d ( id_id , auxhist18_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist18_interval_d
  INTEGER id_id
  auxhist18_interval_d = model_config_rec%auxhist18_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist18_interval_d
SUBROUTINE nl_get_auxhist18_interval_h ( id_id , auxhist18_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist18_interval_h
  INTEGER id_id
  auxhist18_interval_h = model_config_rec%auxhist18_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist18_interval_h
SUBROUTINE nl_get_auxhist18_interval_m ( id_id , auxhist18_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist18_interval_m
  INTEGER id_id
  auxhist18_interval_m = model_config_rec%auxhist18_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist18_interval_m
SUBROUTINE nl_get_auxhist18_interval_s ( id_id , auxhist18_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist18_interval_s
  INTEGER id_id
  auxhist18_interval_s = model_config_rec%auxhist18_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist18_interval_s
SUBROUTINE nl_get_auxhist18_interval ( id_id , auxhist18_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist18_interval
  INTEGER id_id
  auxhist18_interval = model_config_rec%auxhist18_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist18_interval
SUBROUTINE nl_get_auxhist18_begin_y ( id_id , auxhist18_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist18_begin_y
  INTEGER id_id
  auxhist18_begin_y = model_config_rec%auxhist18_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist18_begin_y
SUBROUTINE nl_get_auxhist18_begin_d ( id_id , auxhist18_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist18_begin_d
  INTEGER id_id
  auxhist18_begin_d = model_config_rec%auxhist18_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist18_begin_d
SUBROUTINE nl_get_auxhist18_begin_h ( id_id , auxhist18_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist18_begin_h
  INTEGER id_id
  auxhist18_begin_h = model_config_rec%auxhist18_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist18_begin_h
SUBROUTINE nl_get_auxhist18_begin_m ( id_id , auxhist18_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist18_begin_m
  INTEGER id_id
  auxhist18_begin_m = model_config_rec%auxhist18_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist18_begin_m
SUBROUTINE nl_get_auxhist18_begin_s ( id_id , auxhist18_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist18_begin_s
  INTEGER id_id
  auxhist18_begin_s = model_config_rec%auxhist18_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist18_begin_s
SUBROUTINE nl_get_auxhist18_begin ( id_id , auxhist18_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist18_begin
  INTEGER id_id
  auxhist18_begin = model_config_rec%auxhist18_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist18_begin
SUBROUTINE nl_get_auxhist18_end_y ( id_id , auxhist18_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist18_end_y
  INTEGER id_id
  auxhist18_end_y = model_config_rec%auxhist18_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist18_end_y
SUBROUTINE nl_get_auxhist18_end_d ( id_id , auxhist18_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist18_end_d
  INTEGER id_id
  auxhist18_end_d = model_config_rec%auxhist18_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist18_end_d
SUBROUTINE nl_get_auxhist18_end_h ( id_id , auxhist18_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist18_end_h
  INTEGER id_id
  auxhist18_end_h = model_config_rec%auxhist18_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist18_end_h
SUBROUTINE nl_get_auxhist18_end_m ( id_id , auxhist18_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist18_end_m
  INTEGER id_id
  auxhist18_end_m = model_config_rec%auxhist18_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist18_end_m
SUBROUTINE nl_get_auxhist18_end_s ( id_id , auxhist18_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist18_end_s
  INTEGER id_id
  auxhist18_end_s = model_config_rec%auxhist18_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist18_end_s
SUBROUTINE nl_get_auxhist18_end ( id_id , auxhist18_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist18_end
  INTEGER id_id
  auxhist18_end = model_config_rec%auxhist18_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist18_end
SUBROUTINE nl_get_io_form_auxhist18 ( id_id , io_form_auxhist18 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist18
  INTEGER id_id
  io_form_auxhist18 = model_config_rec%io_form_auxhist18
  RETURN
END SUBROUTINE nl_get_io_form_auxhist18
SUBROUTINE nl_get_frames_per_auxhist18 ( id_id , frames_per_auxhist18 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist18
  INTEGER id_id
  frames_per_auxhist18 = model_config_rec%frames_per_auxhist18(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist18
SUBROUTINE nl_get_auxhist19_inname ( id_id , auxhist19_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist19_inname
  INTEGER id_id
  auxhist19_inname = trim(model_config_rec%auxhist19_inname)
  RETURN
END SUBROUTINE nl_get_auxhist19_inname
SUBROUTINE nl_get_auxhist19_outname ( id_id , auxhist19_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist19_outname
  INTEGER id_id
  auxhist19_outname = trim(model_config_rec%auxhist19_outname)
  RETURN
END SUBROUTINE nl_get_auxhist19_outname
SUBROUTINE nl_get_auxhist19_interval_y ( id_id , auxhist19_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist19_interval_y
  INTEGER id_id
  auxhist19_interval_y = model_config_rec%auxhist19_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist19_interval_y
SUBROUTINE nl_get_auxhist19_interval_d ( id_id , auxhist19_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist19_interval_d
  INTEGER id_id
  auxhist19_interval_d = model_config_rec%auxhist19_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist19_interval_d
SUBROUTINE nl_get_auxhist19_interval_h ( id_id , auxhist19_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist19_interval_h
  INTEGER id_id
  auxhist19_interval_h = model_config_rec%auxhist19_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist19_interval_h
SUBROUTINE nl_get_auxhist19_interval_m ( id_id , auxhist19_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist19_interval_m
  INTEGER id_id
  auxhist19_interval_m = model_config_rec%auxhist19_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist19_interval_m
SUBROUTINE nl_get_auxhist19_interval_s ( id_id , auxhist19_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist19_interval_s
  INTEGER id_id
  auxhist19_interval_s = model_config_rec%auxhist19_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist19_interval_s
SUBROUTINE nl_get_auxhist19_interval ( id_id , auxhist19_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist19_interval
  INTEGER id_id
  auxhist19_interval = model_config_rec%auxhist19_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist19_interval
SUBROUTINE nl_get_auxhist19_begin_y ( id_id , auxhist19_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist19_begin_y
  INTEGER id_id
  auxhist19_begin_y = model_config_rec%auxhist19_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist19_begin_y
SUBROUTINE nl_get_auxhist19_begin_d ( id_id , auxhist19_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist19_begin_d
  INTEGER id_id
  auxhist19_begin_d = model_config_rec%auxhist19_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist19_begin_d
SUBROUTINE nl_get_auxhist19_begin_h ( id_id , auxhist19_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist19_begin_h
  INTEGER id_id
  auxhist19_begin_h = model_config_rec%auxhist19_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist19_begin_h
SUBROUTINE nl_get_auxhist19_begin_m ( id_id , auxhist19_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist19_begin_m
  INTEGER id_id
  auxhist19_begin_m = model_config_rec%auxhist19_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist19_begin_m
SUBROUTINE nl_get_auxhist19_begin_s ( id_id , auxhist19_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist19_begin_s
  INTEGER id_id
  auxhist19_begin_s = model_config_rec%auxhist19_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist19_begin_s
SUBROUTINE nl_get_auxhist19_begin ( id_id , auxhist19_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist19_begin
  INTEGER id_id
  auxhist19_begin = model_config_rec%auxhist19_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist19_begin
SUBROUTINE nl_get_auxhist19_end_y ( id_id , auxhist19_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist19_end_y
  INTEGER id_id
  auxhist19_end_y = model_config_rec%auxhist19_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist19_end_y
SUBROUTINE nl_get_auxhist19_end_d ( id_id , auxhist19_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist19_end_d
  INTEGER id_id
  auxhist19_end_d = model_config_rec%auxhist19_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist19_end_d
SUBROUTINE nl_get_auxhist19_end_h ( id_id , auxhist19_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist19_end_h
  INTEGER id_id
  auxhist19_end_h = model_config_rec%auxhist19_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist19_end_h
SUBROUTINE nl_get_auxhist19_end_m ( id_id , auxhist19_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist19_end_m
  INTEGER id_id
  auxhist19_end_m = model_config_rec%auxhist19_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist19_end_m
SUBROUTINE nl_get_auxhist19_end_s ( id_id , auxhist19_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist19_end_s
  INTEGER id_id
  auxhist19_end_s = model_config_rec%auxhist19_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist19_end_s
SUBROUTINE nl_get_auxhist19_end ( id_id , auxhist19_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist19_end
  INTEGER id_id
  auxhist19_end = model_config_rec%auxhist19_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist19_end
SUBROUTINE nl_get_io_form_auxhist19 ( id_id , io_form_auxhist19 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist19
  INTEGER id_id
  io_form_auxhist19 = model_config_rec%io_form_auxhist19
  RETURN
END SUBROUTINE nl_get_io_form_auxhist19
SUBROUTINE nl_get_frames_per_auxhist19 ( id_id , frames_per_auxhist19 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist19
  INTEGER id_id
  frames_per_auxhist19 = model_config_rec%frames_per_auxhist19(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist19
SUBROUTINE nl_get_auxhist20_inname ( id_id , auxhist20_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist20_inname
  INTEGER id_id
  auxhist20_inname = trim(model_config_rec%auxhist20_inname)
  RETURN
END SUBROUTINE nl_get_auxhist20_inname
SUBROUTINE nl_get_auxhist20_outname ( id_id , auxhist20_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist20_outname
  INTEGER id_id
  auxhist20_outname = trim(model_config_rec%auxhist20_outname)
  RETURN
END SUBROUTINE nl_get_auxhist20_outname
SUBROUTINE nl_get_auxhist20_interval_y ( id_id , auxhist20_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist20_interval_y
  INTEGER id_id
  auxhist20_interval_y = model_config_rec%auxhist20_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist20_interval_y
SUBROUTINE nl_get_auxhist20_interval_d ( id_id , auxhist20_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist20_interval_d
  INTEGER id_id
  auxhist20_interval_d = model_config_rec%auxhist20_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist20_interval_d
SUBROUTINE nl_get_auxhist20_interval_h ( id_id , auxhist20_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist20_interval_h
  INTEGER id_id
  auxhist20_interval_h = model_config_rec%auxhist20_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist20_interval_h
SUBROUTINE nl_get_auxhist20_interval_m ( id_id , auxhist20_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist20_interval_m
  INTEGER id_id
  auxhist20_interval_m = model_config_rec%auxhist20_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist20_interval_m
SUBROUTINE nl_get_auxhist20_interval_s ( id_id , auxhist20_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist20_interval_s
  INTEGER id_id
  auxhist20_interval_s = model_config_rec%auxhist20_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist20_interval_s
SUBROUTINE nl_get_auxhist20_interval ( id_id , auxhist20_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist20_interval
  INTEGER id_id
  auxhist20_interval = model_config_rec%auxhist20_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist20_interval
SUBROUTINE nl_get_auxhist20_begin_y ( id_id , auxhist20_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist20_begin_y
  INTEGER id_id
  auxhist20_begin_y = model_config_rec%auxhist20_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist20_begin_y
SUBROUTINE nl_get_auxhist20_begin_d ( id_id , auxhist20_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist20_begin_d
  INTEGER id_id
  auxhist20_begin_d = model_config_rec%auxhist20_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist20_begin_d
SUBROUTINE nl_get_auxhist20_begin_h ( id_id , auxhist20_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist20_begin_h
  INTEGER id_id
  auxhist20_begin_h = model_config_rec%auxhist20_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist20_begin_h
SUBROUTINE nl_get_auxhist20_begin_m ( id_id , auxhist20_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist20_begin_m
  INTEGER id_id
  auxhist20_begin_m = model_config_rec%auxhist20_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist20_begin_m
SUBROUTINE nl_get_auxhist20_begin_s ( id_id , auxhist20_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist20_begin_s
  INTEGER id_id
  auxhist20_begin_s = model_config_rec%auxhist20_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist20_begin_s
SUBROUTINE nl_get_auxhist20_begin ( id_id , auxhist20_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist20_begin
  INTEGER id_id
  auxhist20_begin = model_config_rec%auxhist20_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist20_begin
SUBROUTINE nl_get_auxhist20_end_y ( id_id , auxhist20_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist20_end_y
  INTEGER id_id
  auxhist20_end_y = model_config_rec%auxhist20_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist20_end_y
SUBROUTINE nl_get_auxhist20_end_d ( id_id , auxhist20_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist20_end_d
  INTEGER id_id
  auxhist20_end_d = model_config_rec%auxhist20_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist20_end_d
SUBROUTINE nl_get_auxhist20_end_h ( id_id , auxhist20_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist20_end_h
  INTEGER id_id
  auxhist20_end_h = model_config_rec%auxhist20_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist20_end_h
SUBROUTINE nl_get_auxhist20_end_m ( id_id , auxhist20_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist20_end_m
  INTEGER id_id
  auxhist20_end_m = model_config_rec%auxhist20_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist20_end_m
SUBROUTINE nl_get_auxhist20_end_s ( id_id , auxhist20_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist20_end_s
  INTEGER id_id
  auxhist20_end_s = model_config_rec%auxhist20_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist20_end_s
SUBROUTINE nl_get_auxhist20_end ( id_id , auxhist20_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist20_end
  INTEGER id_id
  auxhist20_end = model_config_rec%auxhist20_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist20_end
SUBROUTINE nl_get_io_form_auxhist20 ( id_id , io_form_auxhist20 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist20
  INTEGER id_id
  io_form_auxhist20 = model_config_rec%io_form_auxhist20
  RETURN
END SUBROUTINE nl_get_io_form_auxhist20
SUBROUTINE nl_get_frames_per_auxhist20 ( id_id , frames_per_auxhist20 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist20
  INTEGER id_id
  frames_per_auxhist20 = model_config_rec%frames_per_auxhist20(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist20
SUBROUTINE nl_get_auxhist21_inname ( id_id , auxhist21_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist21_inname
  INTEGER id_id
  auxhist21_inname = trim(model_config_rec%auxhist21_inname)
  RETURN
END SUBROUTINE nl_get_auxhist21_inname
SUBROUTINE nl_get_auxhist21_outname ( id_id , auxhist21_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist21_outname
  INTEGER id_id
  auxhist21_outname = trim(model_config_rec%auxhist21_outname)
  RETURN
END SUBROUTINE nl_get_auxhist21_outname
SUBROUTINE nl_get_auxhist21_interval_y ( id_id , auxhist21_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist21_interval_y
  INTEGER id_id
  auxhist21_interval_y = model_config_rec%auxhist21_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist21_interval_y
SUBROUTINE nl_get_auxhist21_interval_d ( id_id , auxhist21_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist21_interval_d
  INTEGER id_id
  auxhist21_interval_d = model_config_rec%auxhist21_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist21_interval_d
SUBROUTINE nl_get_auxhist21_interval_h ( id_id , auxhist21_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist21_interval_h
  INTEGER id_id
  auxhist21_interval_h = model_config_rec%auxhist21_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist21_interval_h
SUBROUTINE nl_get_auxhist21_interval_m ( id_id , auxhist21_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist21_interval_m
  INTEGER id_id
  auxhist21_interval_m = model_config_rec%auxhist21_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist21_interval_m
SUBROUTINE nl_get_auxhist21_interval_s ( id_id , auxhist21_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist21_interval_s
  INTEGER id_id
  auxhist21_interval_s = model_config_rec%auxhist21_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist21_interval_s
SUBROUTINE nl_get_auxhist21_interval ( id_id , auxhist21_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist21_interval
  INTEGER id_id
  auxhist21_interval = model_config_rec%auxhist21_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist21_interval
SUBROUTINE nl_get_auxhist21_begin_y ( id_id , auxhist21_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist21_begin_y
  INTEGER id_id
  auxhist21_begin_y = model_config_rec%auxhist21_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist21_begin_y
SUBROUTINE nl_get_auxhist21_begin_d ( id_id , auxhist21_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist21_begin_d
  INTEGER id_id
  auxhist21_begin_d = model_config_rec%auxhist21_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist21_begin_d
SUBROUTINE nl_get_auxhist21_begin_h ( id_id , auxhist21_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist21_begin_h
  INTEGER id_id
  auxhist21_begin_h = model_config_rec%auxhist21_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist21_begin_h
SUBROUTINE nl_get_auxhist21_begin_m ( id_id , auxhist21_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist21_begin_m
  INTEGER id_id
  auxhist21_begin_m = model_config_rec%auxhist21_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist21_begin_m
SUBROUTINE nl_get_auxhist21_begin_s ( id_id , auxhist21_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist21_begin_s
  INTEGER id_id
  auxhist21_begin_s = model_config_rec%auxhist21_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist21_begin_s
SUBROUTINE nl_get_auxhist21_begin ( id_id , auxhist21_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist21_begin
  INTEGER id_id
  auxhist21_begin = model_config_rec%auxhist21_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist21_begin
SUBROUTINE nl_get_auxhist21_end_y ( id_id , auxhist21_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist21_end_y
  INTEGER id_id
  auxhist21_end_y = model_config_rec%auxhist21_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist21_end_y
SUBROUTINE nl_get_auxhist21_end_d ( id_id , auxhist21_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist21_end_d
  INTEGER id_id
  auxhist21_end_d = model_config_rec%auxhist21_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist21_end_d
SUBROUTINE nl_get_auxhist21_end_h ( id_id , auxhist21_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist21_end_h
  INTEGER id_id
  auxhist21_end_h = model_config_rec%auxhist21_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist21_end_h
SUBROUTINE nl_get_auxhist21_end_m ( id_id , auxhist21_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist21_end_m
  INTEGER id_id
  auxhist21_end_m = model_config_rec%auxhist21_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist21_end_m
SUBROUTINE nl_get_auxhist21_end_s ( id_id , auxhist21_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist21_end_s
  INTEGER id_id
  auxhist21_end_s = model_config_rec%auxhist21_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist21_end_s
SUBROUTINE nl_get_auxhist21_end ( id_id , auxhist21_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist21_end
  INTEGER id_id
  auxhist21_end = model_config_rec%auxhist21_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist21_end
SUBROUTINE nl_get_io_form_auxhist21 ( id_id , io_form_auxhist21 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist21
  INTEGER id_id
  io_form_auxhist21 = model_config_rec%io_form_auxhist21
  RETURN
END SUBROUTINE nl_get_io_form_auxhist21
SUBROUTINE nl_get_frames_per_auxhist21 ( id_id , frames_per_auxhist21 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist21
  INTEGER id_id
  frames_per_auxhist21 = model_config_rec%frames_per_auxhist21(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist21
SUBROUTINE nl_get_auxhist22_inname ( id_id , auxhist22_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist22_inname
  INTEGER id_id
  auxhist22_inname = trim(model_config_rec%auxhist22_inname)
  RETURN
END SUBROUTINE nl_get_auxhist22_inname
SUBROUTINE nl_get_auxhist22_outname ( id_id , auxhist22_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist22_outname
  INTEGER id_id
  auxhist22_outname = trim(model_config_rec%auxhist22_outname)
  RETURN
END SUBROUTINE nl_get_auxhist22_outname
SUBROUTINE nl_get_auxhist22_interval_y ( id_id , auxhist22_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist22_interval_y
  INTEGER id_id
  auxhist22_interval_y = model_config_rec%auxhist22_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist22_interval_y
SUBROUTINE nl_get_auxhist22_interval_d ( id_id , auxhist22_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist22_interval_d
  INTEGER id_id
  auxhist22_interval_d = model_config_rec%auxhist22_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist22_interval_d
SUBROUTINE nl_get_auxhist22_interval_h ( id_id , auxhist22_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist22_interval_h
  INTEGER id_id
  auxhist22_interval_h = model_config_rec%auxhist22_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist22_interval_h
SUBROUTINE nl_get_auxhist22_interval_m ( id_id , auxhist22_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist22_interval_m
  INTEGER id_id
  auxhist22_interval_m = model_config_rec%auxhist22_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist22_interval_m
SUBROUTINE nl_get_auxhist22_interval_s ( id_id , auxhist22_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist22_interval_s
  INTEGER id_id
  auxhist22_interval_s = model_config_rec%auxhist22_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist22_interval_s
SUBROUTINE nl_get_auxhist22_interval ( id_id , auxhist22_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist22_interval
  INTEGER id_id
  auxhist22_interval = model_config_rec%auxhist22_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist22_interval
SUBROUTINE nl_get_auxhist22_begin_y ( id_id , auxhist22_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist22_begin_y
  INTEGER id_id
  auxhist22_begin_y = model_config_rec%auxhist22_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist22_begin_y
SUBROUTINE nl_get_auxhist22_begin_d ( id_id , auxhist22_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist22_begin_d
  INTEGER id_id
  auxhist22_begin_d = model_config_rec%auxhist22_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist22_begin_d
SUBROUTINE nl_get_auxhist22_begin_h ( id_id , auxhist22_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist22_begin_h
  INTEGER id_id
  auxhist22_begin_h = model_config_rec%auxhist22_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist22_begin_h
SUBROUTINE nl_get_auxhist22_begin_m ( id_id , auxhist22_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist22_begin_m
  INTEGER id_id
  auxhist22_begin_m = model_config_rec%auxhist22_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist22_begin_m
SUBROUTINE nl_get_auxhist22_begin_s ( id_id , auxhist22_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist22_begin_s
  INTEGER id_id
  auxhist22_begin_s = model_config_rec%auxhist22_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist22_begin_s
SUBROUTINE nl_get_auxhist22_begin ( id_id , auxhist22_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist22_begin
  INTEGER id_id
  auxhist22_begin = model_config_rec%auxhist22_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist22_begin
SUBROUTINE nl_get_auxhist22_end_y ( id_id , auxhist22_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist22_end_y
  INTEGER id_id
  auxhist22_end_y = model_config_rec%auxhist22_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist22_end_y
SUBROUTINE nl_get_auxhist22_end_d ( id_id , auxhist22_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist22_end_d
  INTEGER id_id
  auxhist22_end_d = model_config_rec%auxhist22_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist22_end_d
SUBROUTINE nl_get_auxhist22_end_h ( id_id , auxhist22_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist22_end_h
  INTEGER id_id
  auxhist22_end_h = model_config_rec%auxhist22_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist22_end_h
SUBROUTINE nl_get_auxhist22_end_m ( id_id , auxhist22_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist22_end_m
  INTEGER id_id
  auxhist22_end_m = model_config_rec%auxhist22_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist22_end_m
SUBROUTINE nl_get_auxhist22_end_s ( id_id , auxhist22_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist22_end_s
  INTEGER id_id
  auxhist22_end_s = model_config_rec%auxhist22_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist22_end_s
SUBROUTINE nl_get_auxhist22_end ( id_id , auxhist22_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist22_end
  INTEGER id_id
  auxhist22_end = model_config_rec%auxhist22_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist22_end
SUBROUTINE nl_get_io_form_auxhist22 ( id_id , io_form_auxhist22 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist22
  INTEGER id_id
  io_form_auxhist22 = model_config_rec%io_form_auxhist22
  RETURN
END SUBROUTINE nl_get_io_form_auxhist22
SUBROUTINE nl_get_frames_per_auxhist22 ( id_id , frames_per_auxhist22 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist22
  INTEGER id_id
  frames_per_auxhist22 = model_config_rec%frames_per_auxhist22(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist22
SUBROUTINE nl_get_auxhist23_inname ( id_id , auxhist23_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist23_inname
  INTEGER id_id
  auxhist23_inname = trim(model_config_rec%auxhist23_inname)
  RETURN
END SUBROUTINE nl_get_auxhist23_inname
SUBROUTINE nl_get_auxhist23_outname ( id_id , auxhist23_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist23_outname
  INTEGER id_id
  auxhist23_outname = trim(model_config_rec%auxhist23_outname)
  RETURN
END SUBROUTINE nl_get_auxhist23_outname
SUBROUTINE nl_get_auxhist23_interval_y ( id_id , auxhist23_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist23_interval_y
  INTEGER id_id
  auxhist23_interval_y = model_config_rec%auxhist23_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist23_interval_y
SUBROUTINE nl_get_auxhist23_interval_d ( id_id , auxhist23_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist23_interval_d
  INTEGER id_id
  auxhist23_interval_d = model_config_rec%auxhist23_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist23_interval_d
#endif
#if (NNN == 3)
SUBROUTINE nl_get_auxhist23_interval_h ( id_id , auxhist23_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist23_interval_h
  INTEGER id_id
  auxhist23_interval_h = model_config_rec%auxhist23_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist23_interval_h
SUBROUTINE nl_get_auxhist23_interval_m ( id_id , auxhist23_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist23_interval_m
  INTEGER id_id
  auxhist23_interval_m = model_config_rec%auxhist23_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist23_interval_m
SUBROUTINE nl_get_auxhist23_interval_s ( id_id , auxhist23_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist23_interval_s
  INTEGER id_id
  auxhist23_interval_s = model_config_rec%auxhist23_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist23_interval_s
SUBROUTINE nl_get_auxhist23_interval ( id_id , auxhist23_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist23_interval
  INTEGER id_id
  auxhist23_interval = model_config_rec%auxhist23_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist23_interval
SUBROUTINE nl_get_auxhist23_begin_y ( id_id , auxhist23_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist23_begin_y
  INTEGER id_id
  auxhist23_begin_y = model_config_rec%auxhist23_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist23_begin_y
SUBROUTINE nl_get_auxhist23_begin_d ( id_id , auxhist23_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist23_begin_d
  INTEGER id_id
  auxhist23_begin_d = model_config_rec%auxhist23_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist23_begin_d
SUBROUTINE nl_get_auxhist23_begin_h ( id_id , auxhist23_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist23_begin_h
  INTEGER id_id
  auxhist23_begin_h = model_config_rec%auxhist23_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist23_begin_h
SUBROUTINE nl_get_auxhist23_begin_m ( id_id , auxhist23_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist23_begin_m
  INTEGER id_id
  auxhist23_begin_m = model_config_rec%auxhist23_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist23_begin_m
SUBROUTINE nl_get_auxhist23_begin_s ( id_id , auxhist23_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist23_begin_s
  INTEGER id_id
  auxhist23_begin_s = model_config_rec%auxhist23_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist23_begin_s
SUBROUTINE nl_get_auxhist23_begin ( id_id , auxhist23_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist23_begin
  INTEGER id_id
  auxhist23_begin = model_config_rec%auxhist23_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist23_begin
SUBROUTINE nl_get_auxhist23_end_y ( id_id , auxhist23_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist23_end_y
  INTEGER id_id
  auxhist23_end_y = model_config_rec%auxhist23_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist23_end_y
SUBROUTINE nl_get_auxhist23_end_d ( id_id , auxhist23_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist23_end_d
  INTEGER id_id
  auxhist23_end_d = model_config_rec%auxhist23_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist23_end_d
SUBROUTINE nl_get_auxhist23_end_h ( id_id , auxhist23_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist23_end_h
  INTEGER id_id
  auxhist23_end_h = model_config_rec%auxhist23_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist23_end_h
SUBROUTINE nl_get_auxhist23_end_m ( id_id , auxhist23_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist23_end_m
  INTEGER id_id
  auxhist23_end_m = model_config_rec%auxhist23_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist23_end_m
SUBROUTINE nl_get_auxhist23_end_s ( id_id , auxhist23_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist23_end_s
  INTEGER id_id
  auxhist23_end_s = model_config_rec%auxhist23_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist23_end_s
SUBROUTINE nl_get_auxhist23_end ( id_id , auxhist23_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist23_end
  INTEGER id_id
  auxhist23_end = model_config_rec%auxhist23_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist23_end
SUBROUTINE nl_get_io_form_auxhist23 ( id_id , io_form_auxhist23 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist23
  INTEGER id_id
  io_form_auxhist23 = model_config_rec%io_form_auxhist23
  RETURN
END SUBROUTINE nl_get_io_form_auxhist23
SUBROUTINE nl_get_frames_per_auxhist23 ( id_id , frames_per_auxhist23 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist23
  INTEGER id_id
  frames_per_auxhist23 = model_config_rec%frames_per_auxhist23(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist23
SUBROUTINE nl_get_auxhist24_inname ( id_id , auxhist24_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist24_inname
  INTEGER id_id
  auxhist24_inname = trim(model_config_rec%auxhist24_inname)
  RETURN
END SUBROUTINE nl_get_auxhist24_inname
SUBROUTINE nl_get_auxhist24_outname ( id_id , auxhist24_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxhist24_outname
  INTEGER id_id
  auxhist24_outname = trim(model_config_rec%auxhist24_outname)
  RETURN
END SUBROUTINE nl_get_auxhist24_outname
SUBROUTINE nl_get_auxhist24_interval_y ( id_id , auxhist24_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist24_interval_y
  INTEGER id_id
  auxhist24_interval_y = model_config_rec%auxhist24_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist24_interval_y
SUBROUTINE nl_get_auxhist24_interval_d ( id_id , auxhist24_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist24_interval_d
  INTEGER id_id
  auxhist24_interval_d = model_config_rec%auxhist24_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist24_interval_d
SUBROUTINE nl_get_auxhist24_interval_h ( id_id , auxhist24_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist24_interval_h
  INTEGER id_id
  auxhist24_interval_h = model_config_rec%auxhist24_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist24_interval_h
SUBROUTINE nl_get_auxhist24_interval_m ( id_id , auxhist24_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist24_interval_m
  INTEGER id_id
  auxhist24_interval_m = model_config_rec%auxhist24_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist24_interval_m
SUBROUTINE nl_get_auxhist24_interval_s ( id_id , auxhist24_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist24_interval_s
  INTEGER id_id
  auxhist24_interval_s = model_config_rec%auxhist24_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist24_interval_s
SUBROUTINE nl_get_auxhist24_interval ( id_id , auxhist24_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist24_interval
  INTEGER id_id
  auxhist24_interval = model_config_rec%auxhist24_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist24_interval
SUBROUTINE nl_get_auxhist24_begin_y ( id_id , auxhist24_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist24_begin_y
  INTEGER id_id
  auxhist24_begin_y = model_config_rec%auxhist24_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist24_begin_y
SUBROUTINE nl_get_auxhist24_begin_d ( id_id , auxhist24_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist24_begin_d
  INTEGER id_id
  auxhist24_begin_d = model_config_rec%auxhist24_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist24_begin_d
SUBROUTINE nl_get_auxhist24_begin_h ( id_id , auxhist24_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist24_begin_h
  INTEGER id_id
  auxhist24_begin_h = model_config_rec%auxhist24_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist24_begin_h
SUBROUTINE nl_get_auxhist24_begin_m ( id_id , auxhist24_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist24_begin_m
  INTEGER id_id
  auxhist24_begin_m = model_config_rec%auxhist24_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist24_begin_m
SUBROUTINE nl_get_auxhist24_begin_s ( id_id , auxhist24_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist24_begin_s
  INTEGER id_id
  auxhist24_begin_s = model_config_rec%auxhist24_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist24_begin_s
SUBROUTINE nl_get_auxhist24_begin ( id_id , auxhist24_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist24_begin
  INTEGER id_id
  auxhist24_begin = model_config_rec%auxhist24_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist24_begin
SUBROUTINE nl_get_auxhist24_end_y ( id_id , auxhist24_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist24_end_y
  INTEGER id_id
  auxhist24_end_y = model_config_rec%auxhist24_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist24_end_y
SUBROUTINE nl_get_auxhist24_end_d ( id_id , auxhist24_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist24_end_d
  INTEGER id_id
  auxhist24_end_d = model_config_rec%auxhist24_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist24_end_d
SUBROUTINE nl_get_auxhist24_end_h ( id_id , auxhist24_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist24_end_h
  INTEGER id_id
  auxhist24_end_h = model_config_rec%auxhist24_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist24_end_h
SUBROUTINE nl_get_auxhist24_end_m ( id_id , auxhist24_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist24_end_m
  INTEGER id_id
  auxhist24_end_m = model_config_rec%auxhist24_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist24_end_m
SUBROUTINE nl_get_auxhist24_end_s ( id_id , auxhist24_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist24_end_s
  INTEGER id_id
  auxhist24_end_s = model_config_rec%auxhist24_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist24_end_s
SUBROUTINE nl_get_auxhist24_end ( id_id , auxhist24_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxhist24_end
  INTEGER id_id
  auxhist24_end = model_config_rec%auxhist24_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxhist24_end
SUBROUTINE nl_get_io_form_auxhist24 ( id_id , io_form_auxhist24 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxhist24
  INTEGER id_id
  io_form_auxhist24 = model_config_rec%io_form_auxhist24
  RETURN
END SUBROUTINE nl_get_io_form_auxhist24
SUBROUTINE nl_get_frames_per_auxhist24 ( id_id , frames_per_auxhist24 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxhist24
  INTEGER id_id
  frames_per_auxhist24 = model_config_rec%frames_per_auxhist24(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxhist24
SUBROUTINE nl_get_auxinput1_outname ( id_id , auxinput1_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput1_outname
  INTEGER id_id
  auxinput1_outname = trim(model_config_rec%auxinput1_outname)
  RETURN
END SUBROUTINE nl_get_auxinput1_outname
SUBROUTINE nl_get_auxinput1_interval_y ( id_id , auxinput1_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_interval_y
  INTEGER id_id
  auxinput1_interval_y = model_config_rec%auxinput1_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval_y
SUBROUTINE nl_get_auxinput1_interval_d ( id_id , auxinput1_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_interval_d
  INTEGER id_id
  auxinput1_interval_d = model_config_rec%auxinput1_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval_d
SUBROUTINE nl_get_auxinput1_interval_h ( id_id , auxinput1_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_interval_h
  INTEGER id_id
  auxinput1_interval_h = model_config_rec%auxinput1_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval_h
SUBROUTINE nl_get_auxinput1_interval_m ( id_id , auxinput1_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_interval_m
  INTEGER id_id
  auxinput1_interval_m = model_config_rec%auxinput1_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval_m
SUBROUTINE nl_get_auxinput1_interval_s ( id_id , auxinput1_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_interval_s
  INTEGER id_id
  auxinput1_interval_s = model_config_rec%auxinput1_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval_s
SUBROUTINE nl_get_auxinput1_interval ( id_id , auxinput1_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_interval
  INTEGER id_id
  auxinput1_interval = model_config_rec%auxinput1_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_interval
SUBROUTINE nl_get_auxinput1_begin_y ( id_id , auxinput1_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_begin_y
  INTEGER id_id
  auxinput1_begin_y = model_config_rec%auxinput1_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_y
SUBROUTINE nl_get_auxinput1_begin_d ( id_id , auxinput1_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_begin_d
  INTEGER id_id
  auxinput1_begin_d = model_config_rec%auxinput1_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_d
SUBROUTINE nl_get_auxinput1_begin_h ( id_id , auxinput1_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_begin_h
  INTEGER id_id
  auxinput1_begin_h = model_config_rec%auxinput1_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_h
SUBROUTINE nl_get_auxinput1_begin_m ( id_id , auxinput1_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_begin_m
  INTEGER id_id
  auxinput1_begin_m = model_config_rec%auxinput1_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_m
SUBROUTINE nl_get_auxinput1_begin_s ( id_id , auxinput1_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_begin_s
  INTEGER id_id
  auxinput1_begin_s = model_config_rec%auxinput1_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin_s
SUBROUTINE nl_get_auxinput1_begin ( id_id , auxinput1_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_begin
  INTEGER id_id
  auxinput1_begin = model_config_rec%auxinput1_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_begin
SUBROUTINE nl_get_auxinput1_end_y ( id_id , auxinput1_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_end_y
  INTEGER id_id
  auxinput1_end_y = model_config_rec%auxinput1_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_y
SUBROUTINE nl_get_auxinput1_end_d ( id_id , auxinput1_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_end_d
  INTEGER id_id
  auxinput1_end_d = model_config_rec%auxinput1_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_d
SUBROUTINE nl_get_auxinput1_end_h ( id_id , auxinput1_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_end_h
  INTEGER id_id
  auxinput1_end_h = model_config_rec%auxinput1_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_h
SUBROUTINE nl_get_auxinput1_end_m ( id_id , auxinput1_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_end_m
  INTEGER id_id
  auxinput1_end_m = model_config_rec%auxinput1_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_m
SUBROUTINE nl_get_auxinput1_end_s ( id_id , auxinput1_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_end_s
  INTEGER id_id
  auxinput1_end_s = model_config_rec%auxinput1_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end_s
SUBROUTINE nl_get_auxinput1_end ( id_id , auxinput1_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput1_end
  INTEGER id_id
  auxinput1_end = model_config_rec%auxinput1_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput1_end
SUBROUTINE nl_get_frames_per_auxinput1 ( id_id , frames_per_auxinput1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput1
  INTEGER id_id
  frames_per_auxinput1 = model_config_rec%frames_per_auxinput1(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput1
SUBROUTINE nl_get_auxinput2_inname ( id_id , auxinput2_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput2_inname
  INTEGER id_id
  auxinput2_inname = trim(model_config_rec%auxinput2_inname)
  RETURN
END SUBROUTINE nl_get_auxinput2_inname
SUBROUTINE nl_get_auxinput2_outname ( id_id , auxinput2_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput2_outname
  INTEGER id_id
  auxinput2_outname = trim(model_config_rec%auxinput2_outname)
  RETURN
END SUBROUTINE nl_get_auxinput2_outname
SUBROUTINE nl_get_auxinput2_interval_y ( id_id , auxinput2_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_interval_y
  INTEGER id_id
  auxinput2_interval_y = model_config_rec%auxinput2_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval_y
SUBROUTINE nl_get_auxinput2_interval_d ( id_id , auxinput2_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_interval_d
  INTEGER id_id
  auxinput2_interval_d = model_config_rec%auxinput2_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval_d
SUBROUTINE nl_get_auxinput2_interval_h ( id_id , auxinput2_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_interval_h
  INTEGER id_id
  auxinput2_interval_h = model_config_rec%auxinput2_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval_h
SUBROUTINE nl_get_auxinput2_interval_m ( id_id , auxinput2_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_interval_m
  INTEGER id_id
  auxinput2_interval_m = model_config_rec%auxinput2_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval_m
SUBROUTINE nl_get_auxinput2_interval_s ( id_id , auxinput2_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_interval_s
  INTEGER id_id
  auxinput2_interval_s = model_config_rec%auxinput2_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval_s
SUBROUTINE nl_get_auxinput2_interval ( id_id , auxinput2_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_interval
  INTEGER id_id
  auxinput2_interval = model_config_rec%auxinput2_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_interval
SUBROUTINE nl_get_auxinput2_begin_y ( id_id , auxinput2_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_begin_y
  INTEGER id_id
  auxinput2_begin_y = model_config_rec%auxinput2_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_y
SUBROUTINE nl_get_auxinput2_begin_d ( id_id , auxinput2_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_begin_d
  INTEGER id_id
  auxinput2_begin_d = model_config_rec%auxinput2_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_d
SUBROUTINE nl_get_auxinput2_begin_h ( id_id , auxinput2_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_begin_h
  INTEGER id_id
  auxinput2_begin_h = model_config_rec%auxinput2_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_h
SUBROUTINE nl_get_auxinput2_begin_m ( id_id , auxinput2_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_begin_m
  INTEGER id_id
  auxinput2_begin_m = model_config_rec%auxinput2_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_m
SUBROUTINE nl_get_auxinput2_begin_s ( id_id , auxinput2_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_begin_s
  INTEGER id_id
  auxinput2_begin_s = model_config_rec%auxinput2_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin_s
SUBROUTINE nl_get_auxinput2_begin ( id_id , auxinput2_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_begin
  INTEGER id_id
  auxinput2_begin = model_config_rec%auxinput2_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_begin
SUBROUTINE nl_get_auxinput2_end_y ( id_id , auxinput2_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_end_y
  INTEGER id_id
  auxinput2_end_y = model_config_rec%auxinput2_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_y
SUBROUTINE nl_get_auxinput2_end_d ( id_id , auxinput2_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_end_d
  INTEGER id_id
  auxinput2_end_d = model_config_rec%auxinput2_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_d
SUBROUTINE nl_get_auxinput2_end_h ( id_id , auxinput2_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_end_h
  INTEGER id_id
  auxinput2_end_h = model_config_rec%auxinput2_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_h
SUBROUTINE nl_get_auxinput2_end_m ( id_id , auxinput2_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_end_m
  INTEGER id_id
  auxinput2_end_m = model_config_rec%auxinput2_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_m
SUBROUTINE nl_get_auxinput2_end_s ( id_id , auxinput2_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_end_s
  INTEGER id_id
  auxinput2_end_s = model_config_rec%auxinput2_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end_s
SUBROUTINE nl_get_auxinput2_end ( id_id , auxinput2_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput2_end
  INTEGER id_id
  auxinput2_end = model_config_rec%auxinput2_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput2_end
SUBROUTINE nl_get_frames_per_auxinput2 ( id_id , frames_per_auxinput2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput2
  INTEGER id_id
  frames_per_auxinput2 = model_config_rec%frames_per_auxinput2(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput2
SUBROUTINE nl_get_auxinput3_inname ( id_id , auxinput3_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput3_inname
  INTEGER id_id
  auxinput3_inname = trim(model_config_rec%auxinput3_inname)
  RETURN
END SUBROUTINE nl_get_auxinput3_inname
SUBROUTINE nl_get_auxinput3_outname ( id_id , auxinput3_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput3_outname
  INTEGER id_id
  auxinput3_outname = trim(model_config_rec%auxinput3_outname)
  RETURN
END SUBROUTINE nl_get_auxinput3_outname
SUBROUTINE nl_get_auxinput3_interval_y ( id_id , auxinput3_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_interval_y
  INTEGER id_id
  auxinput3_interval_y = model_config_rec%auxinput3_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval_y
SUBROUTINE nl_get_auxinput3_interval_d ( id_id , auxinput3_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_interval_d
  INTEGER id_id
  auxinput3_interval_d = model_config_rec%auxinput3_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval_d
SUBROUTINE nl_get_auxinput3_interval_h ( id_id , auxinput3_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_interval_h
  INTEGER id_id
  auxinput3_interval_h = model_config_rec%auxinput3_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval_h
SUBROUTINE nl_get_auxinput3_interval_m ( id_id , auxinput3_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_interval_m
  INTEGER id_id
  auxinput3_interval_m = model_config_rec%auxinput3_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval_m
SUBROUTINE nl_get_auxinput3_interval_s ( id_id , auxinput3_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_interval_s
  INTEGER id_id
  auxinput3_interval_s = model_config_rec%auxinput3_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval_s
SUBROUTINE nl_get_auxinput3_interval ( id_id , auxinput3_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_interval
  INTEGER id_id
  auxinput3_interval = model_config_rec%auxinput3_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_interval
SUBROUTINE nl_get_auxinput3_begin_y ( id_id , auxinput3_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_begin_y
  INTEGER id_id
  auxinput3_begin_y = model_config_rec%auxinput3_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_y
SUBROUTINE nl_get_auxinput3_begin_d ( id_id , auxinput3_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_begin_d
  INTEGER id_id
  auxinput3_begin_d = model_config_rec%auxinput3_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_d
SUBROUTINE nl_get_auxinput3_begin_h ( id_id , auxinput3_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_begin_h
  INTEGER id_id
  auxinput3_begin_h = model_config_rec%auxinput3_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_h
SUBROUTINE nl_get_auxinput3_begin_m ( id_id , auxinput3_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_begin_m
  INTEGER id_id
  auxinput3_begin_m = model_config_rec%auxinput3_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_m
SUBROUTINE nl_get_auxinput3_begin_s ( id_id , auxinput3_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_begin_s
  INTEGER id_id
  auxinput3_begin_s = model_config_rec%auxinput3_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin_s
SUBROUTINE nl_get_auxinput3_begin ( id_id , auxinput3_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_begin
  INTEGER id_id
  auxinput3_begin = model_config_rec%auxinput3_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_begin
SUBROUTINE nl_get_auxinput3_end_y ( id_id , auxinput3_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_end_y
  INTEGER id_id
  auxinput3_end_y = model_config_rec%auxinput3_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_y
SUBROUTINE nl_get_auxinput3_end_d ( id_id , auxinput3_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_end_d
  INTEGER id_id
  auxinput3_end_d = model_config_rec%auxinput3_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_d
SUBROUTINE nl_get_auxinput3_end_h ( id_id , auxinput3_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_end_h
  INTEGER id_id
  auxinput3_end_h = model_config_rec%auxinput3_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_h
SUBROUTINE nl_get_auxinput3_end_m ( id_id , auxinput3_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_end_m
  INTEGER id_id
  auxinput3_end_m = model_config_rec%auxinput3_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_m
SUBROUTINE nl_get_auxinput3_end_s ( id_id , auxinput3_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_end_s
  INTEGER id_id
  auxinput3_end_s = model_config_rec%auxinput3_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end_s
SUBROUTINE nl_get_auxinput3_end ( id_id , auxinput3_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput3_end
  INTEGER id_id
  auxinput3_end = model_config_rec%auxinput3_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput3_end
SUBROUTINE nl_get_io_form_auxinput3 ( id_id , io_form_auxinput3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput3
  INTEGER id_id
  io_form_auxinput3 = model_config_rec%io_form_auxinput3
  RETURN
END SUBROUTINE nl_get_io_form_auxinput3
SUBROUTINE nl_get_frames_per_auxinput3 ( id_id , frames_per_auxinput3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput3
  INTEGER id_id
  frames_per_auxinput3 = model_config_rec%frames_per_auxinput3(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput3
SUBROUTINE nl_get_auxinput4_inname ( id_id , auxinput4_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput4_inname
  INTEGER id_id
  auxinput4_inname = trim(model_config_rec%auxinput4_inname)
  RETURN
END SUBROUTINE nl_get_auxinput4_inname
SUBROUTINE nl_get_auxinput4_outname ( id_id , auxinput4_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput4_outname
  INTEGER id_id
  auxinput4_outname = trim(model_config_rec%auxinput4_outname)
  RETURN
END SUBROUTINE nl_get_auxinput4_outname
SUBROUTINE nl_get_auxinput4_interval_y ( id_id , auxinput4_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_interval_y
  INTEGER id_id
  auxinput4_interval_y = model_config_rec%auxinput4_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval_y
SUBROUTINE nl_get_auxinput4_interval_d ( id_id , auxinput4_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_interval_d
  INTEGER id_id
  auxinput4_interval_d = model_config_rec%auxinput4_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval_d
SUBROUTINE nl_get_auxinput4_interval_h ( id_id , auxinput4_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_interval_h
  INTEGER id_id
  auxinput4_interval_h = model_config_rec%auxinput4_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval_h
SUBROUTINE nl_get_auxinput4_interval_m ( id_id , auxinput4_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_interval_m
  INTEGER id_id
  auxinput4_interval_m = model_config_rec%auxinput4_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval_m
SUBROUTINE nl_get_auxinput4_interval_s ( id_id , auxinput4_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_interval_s
  INTEGER id_id
  auxinput4_interval_s = model_config_rec%auxinput4_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval_s
SUBROUTINE nl_get_auxinput4_interval ( id_id , auxinput4_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_interval
  INTEGER id_id
  auxinput4_interval = model_config_rec%auxinput4_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_interval
SUBROUTINE nl_get_auxinput4_begin_y ( id_id , auxinput4_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_begin_y
  INTEGER id_id
  auxinput4_begin_y = model_config_rec%auxinput4_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_y
SUBROUTINE nl_get_auxinput4_begin_d ( id_id , auxinput4_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_begin_d
  INTEGER id_id
  auxinput4_begin_d = model_config_rec%auxinput4_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_d
SUBROUTINE nl_get_auxinput4_begin_h ( id_id , auxinput4_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_begin_h
  INTEGER id_id
  auxinput4_begin_h = model_config_rec%auxinput4_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_h
SUBROUTINE nl_get_auxinput4_begin_m ( id_id , auxinput4_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_begin_m
  INTEGER id_id
  auxinput4_begin_m = model_config_rec%auxinput4_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_m
SUBROUTINE nl_get_auxinput4_begin_s ( id_id , auxinput4_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_begin_s
  INTEGER id_id
  auxinput4_begin_s = model_config_rec%auxinput4_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin_s
SUBROUTINE nl_get_auxinput4_begin ( id_id , auxinput4_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_begin
  INTEGER id_id
  auxinput4_begin = model_config_rec%auxinput4_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_begin
SUBROUTINE nl_get_auxinput4_end_y ( id_id , auxinput4_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_end_y
  INTEGER id_id
  auxinput4_end_y = model_config_rec%auxinput4_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_y
SUBROUTINE nl_get_auxinput4_end_d ( id_id , auxinput4_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_end_d
  INTEGER id_id
  auxinput4_end_d = model_config_rec%auxinput4_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_d
SUBROUTINE nl_get_auxinput4_end_h ( id_id , auxinput4_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_end_h
  INTEGER id_id
  auxinput4_end_h = model_config_rec%auxinput4_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_h
SUBROUTINE nl_get_auxinput4_end_m ( id_id , auxinput4_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_end_m
  INTEGER id_id
  auxinput4_end_m = model_config_rec%auxinput4_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_m
SUBROUTINE nl_get_auxinput4_end_s ( id_id , auxinput4_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_end_s
  INTEGER id_id
  auxinput4_end_s = model_config_rec%auxinput4_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end_s
SUBROUTINE nl_get_auxinput4_end ( id_id , auxinput4_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput4_end
  INTEGER id_id
  auxinput4_end = model_config_rec%auxinput4_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput4_end
SUBROUTINE nl_get_io_form_auxinput4 ( id_id , io_form_auxinput4 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput4
  INTEGER id_id
  io_form_auxinput4 = model_config_rec%io_form_auxinput4
  RETURN
END SUBROUTINE nl_get_io_form_auxinput4
SUBROUTINE nl_get_frames_per_auxinput4 ( id_id , frames_per_auxinput4 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput4
  INTEGER id_id
  frames_per_auxinput4 = model_config_rec%frames_per_auxinput4(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput4
SUBROUTINE nl_get_auxinput5_inname ( id_id , auxinput5_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput5_inname
  INTEGER id_id
  auxinput5_inname = trim(model_config_rec%auxinput5_inname)
  RETURN
END SUBROUTINE nl_get_auxinput5_inname
SUBROUTINE nl_get_auxinput5_outname ( id_id , auxinput5_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput5_outname
  INTEGER id_id
  auxinput5_outname = trim(model_config_rec%auxinput5_outname)
  RETURN
END SUBROUTINE nl_get_auxinput5_outname
SUBROUTINE nl_get_auxinput5_interval_y ( id_id , auxinput5_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_interval_y
  INTEGER id_id
  auxinput5_interval_y = model_config_rec%auxinput5_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval_y
SUBROUTINE nl_get_auxinput5_interval_d ( id_id , auxinput5_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_interval_d
  INTEGER id_id
  auxinput5_interval_d = model_config_rec%auxinput5_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval_d
SUBROUTINE nl_get_auxinput5_interval_h ( id_id , auxinput5_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_interval_h
  INTEGER id_id
  auxinput5_interval_h = model_config_rec%auxinput5_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval_h
SUBROUTINE nl_get_auxinput5_interval_m ( id_id , auxinput5_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_interval_m
  INTEGER id_id
  auxinput5_interval_m = model_config_rec%auxinput5_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval_m
SUBROUTINE nl_get_auxinput5_interval_s ( id_id , auxinput5_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_interval_s
  INTEGER id_id
  auxinput5_interval_s = model_config_rec%auxinput5_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval_s
SUBROUTINE nl_get_auxinput5_interval ( id_id , auxinput5_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_interval
  INTEGER id_id
  auxinput5_interval = model_config_rec%auxinput5_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_interval
SUBROUTINE nl_get_auxinput5_begin_y ( id_id , auxinput5_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_begin_y
  INTEGER id_id
  auxinput5_begin_y = model_config_rec%auxinput5_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_y
SUBROUTINE nl_get_auxinput5_begin_d ( id_id , auxinput5_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_begin_d
  INTEGER id_id
  auxinput5_begin_d = model_config_rec%auxinput5_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_d
SUBROUTINE nl_get_auxinput5_begin_h ( id_id , auxinput5_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_begin_h
  INTEGER id_id
  auxinput5_begin_h = model_config_rec%auxinput5_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_h
SUBROUTINE nl_get_auxinput5_begin_m ( id_id , auxinput5_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_begin_m
  INTEGER id_id
  auxinput5_begin_m = model_config_rec%auxinput5_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_m
SUBROUTINE nl_get_auxinput5_begin_s ( id_id , auxinput5_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_begin_s
  INTEGER id_id
  auxinput5_begin_s = model_config_rec%auxinput5_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin_s
SUBROUTINE nl_get_auxinput5_begin ( id_id , auxinput5_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_begin
  INTEGER id_id
  auxinput5_begin = model_config_rec%auxinput5_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_begin
SUBROUTINE nl_get_auxinput5_end_y ( id_id , auxinput5_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_end_y
  INTEGER id_id
  auxinput5_end_y = model_config_rec%auxinput5_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_y
SUBROUTINE nl_get_auxinput5_end_d ( id_id , auxinput5_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_end_d
  INTEGER id_id
  auxinput5_end_d = model_config_rec%auxinput5_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_d
SUBROUTINE nl_get_auxinput5_end_h ( id_id , auxinput5_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_end_h
  INTEGER id_id
  auxinput5_end_h = model_config_rec%auxinput5_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_h
SUBROUTINE nl_get_auxinput5_end_m ( id_id , auxinput5_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_end_m
  INTEGER id_id
  auxinput5_end_m = model_config_rec%auxinput5_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_m
SUBROUTINE nl_get_auxinput5_end_s ( id_id , auxinput5_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_end_s
  INTEGER id_id
  auxinput5_end_s = model_config_rec%auxinput5_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end_s
SUBROUTINE nl_get_auxinput5_end ( id_id , auxinput5_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput5_end
  INTEGER id_id
  auxinput5_end = model_config_rec%auxinput5_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput5_end
SUBROUTINE nl_get_io_form_auxinput5 ( id_id , io_form_auxinput5 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput5
  INTEGER id_id
  io_form_auxinput5 = model_config_rec%io_form_auxinput5
  RETURN
END SUBROUTINE nl_get_io_form_auxinput5
SUBROUTINE nl_get_frames_per_auxinput5 ( id_id , frames_per_auxinput5 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput5
  INTEGER id_id
  frames_per_auxinput5 = model_config_rec%frames_per_auxinput5(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput5
SUBROUTINE nl_get_auxinput6_inname ( id_id , auxinput6_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput6_inname
  INTEGER id_id
  auxinput6_inname = trim(model_config_rec%auxinput6_inname)
  RETURN
END SUBROUTINE nl_get_auxinput6_inname
SUBROUTINE nl_get_auxinput6_outname ( id_id , auxinput6_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput6_outname
  INTEGER id_id
  auxinput6_outname = trim(model_config_rec%auxinput6_outname)
  RETURN
END SUBROUTINE nl_get_auxinput6_outname
SUBROUTINE nl_get_auxinput6_interval_y ( id_id , auxinput6_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_interval_y
  INTEGER id_id
  auxinput6_interval_y = model_config_rec%auxinput6_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_interval_y
SUBROUTINE nl_get_auxinput6_interval_d ( id_id , auxinput6_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_interval_d
  INTEGER id_id
  auxinput6_interval_d = model_config_rec%auxinput6_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_interval_d
SUBROUTINE nl_get_auxinput6_interval_h ( id_id , auxinput6_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_interval_h
  INTEGER id_id
  auxinput6_interval_h = model_config_rec%auxinput6_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_interval_h
SUBROUTINE nl_get_auxinput6_interval_m ( id_id , auxinput6_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_interval_m
  INTEGER id_id
  auxinput6_interval_m = model_config_rec%auxinput6_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_interval_m
SUBROUTINE nl_get_auxinput6_interval_s ( id_id , auxinput6_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_interval_s
  INTEGER id_id
  auxinput6_interval_s = model_config_rec%auxinput6_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_interval_s
SUBROUTINE nl_get_auxinput6_interval ( id_id , auxinput6_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_interval
  INTEGER id_id
  auxinput6_interval = model_config_rec%auxinput6_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_interval
SUBROUTINE nl_get_auxinput6_begin_y ( id_id , auxinput6_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_begin_y
  INTEGER id_id
  auxinput6_begin_y = model_config_rec%auxinput6_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_begin_y
SUBROUTINE nl_get_auxinput6_begin_d ( id_id , auxinput6_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_begin_d
  INTEGER id_id
  auxinput6_begin_d = model_config_rec%auxinput6_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_begin_d
SUBROUTINE nl_get_auxinput6_begin_h ( id_id , auxinput6_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_begin_h
  INTEGER id_id
  auxinput6_begin_h = model_config_rec%auxinput6_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_begin_h
SUBROUTINE nl_get_auxinput6_begin_m ( id_id , auxinput6_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_begin_m
  INTEGER id_id
  auxinput6_begin_m = model_config_rec%auxinput6_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_begin_m
SUBROUTINE nl_get_auxinput6_begin_s ( id_id , auxinput6_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_begin_s
  INTEGER id_id
  auxinput6_begin_s = model_config_rec%auxinput6_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_begin_s
SUBROUTINE nl_get_auxinput6_begin ( id_id , auxinput6_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_begin
  INTEGER id_id
  auxinput6_begin = model_config_rec%auxinput6_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_begin
SUBROUTINE nl_get_auxinput6_end_y ( id_id , auxinput6_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_end_y
  INTEGER id_id
  auxinput6_end_y = model_config_rec%auxinput6_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_end_y
SUBROUTINE nl_get_auxinput6_end_d ( id_id , auxinput6_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_end_d
  INTEGER id_id
  auxinput6_end_d = model_config_rec%auxinput6_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_end_d
SUBROUTINE nl_get_auxinput6_end_h ( id_id , auxinput6_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_end_h
  INTEGER id_id
  auxinput6_end_h = model_config_rec%auxinput6_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_end_h
SUBROUTINE nl_get_auxinput6_end_m ( id_id , auxinput6_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_end_m
  INTEGER id_id
  auxinput6_end_m = model_config_rec%auxinput6_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_end_m
SUBROUTINE nl_get_auxinput6_end_s ( id_id , auxinput6_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_end_s
  INTEGER id_id
  auxinput6_end_s = model_config_rec%auxinput6_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_end_s
SUBROUTINE nl_get_auxinput6_end ( id_id , auxinput6_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput6_end
  INTEGER id_id
  auxinput6_end = model_config_rec%auxinput6_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput6_end
SUBROUTINE nl_get_io_form_auxinput6 ( id_id , io_form_auxinput6 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput6
  INTEGER id_id
  io_form_auxinput6 = model_config_rec%io_form_auxinput6
  RETURN
END SUBROUTINE nl_get_io_form_auxinput6
SUBROUTINE nl_get_frames_per_auxinput6 ( id_id , frames_per_auxinput6 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput6
  INTEGER id_id
  frames_per_auxinput6 = model_config_rec%frames_per_auxinput6(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput6
SUBROUTINE nl_get_auxinput7_inname ( id_id , auxinput7_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput7_inname
  INTEGER id_id
  auxinput7_inname = trim(model_config_rec%auxinput7_inname)
  RETURN
END SUBROUTINE nl_get_auxinput7_inname
SUBROUTINE nl_get_auxinput7_outname ( id_id , auxinput7_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput7_outname
  INTEGER id_id
  auxinput7_outname = trim(model_config_rec%auxinput7_outname)
  RETURN
END SUBROUTINE nl_get_auxinput7_outname
SUBROUTINE nl_get_auxinput7_interval_y ( id_id , auxinput7_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_interval_y
  INTEGER id_id
  auxinput7_interval_y = model_config_rec%auxinput7_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_interval_y
SUBROUTINE nl_get_auxinput7_interval_d ( id_id , auxinput7_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_interval_d
  INTEGER id_id
  auxinput7_interval_d = model_config_rec%auxinput7_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_interval_d
SUBROUTINE nl_get_auxinput7_interval_h ( id_id , auxinput7_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_interval_h
  INTEGER id_id
  auxinput7_interval_h = model_config_rec%auxinput7_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_interval_h
SUBROUTINE nl_get_auxinput7_interval_m ( id_id , auxinput7_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_interval_m
  INTEGER id_id
  auxinput7_interval_m = model_config_rec%auxinput7_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_interval_m
SUBROUTINE nl_get_auxinput7_interval_s ( id_id , auxinput7_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_interval_s
  INTEGER id_id
  auxinput7_interval_s = model_config_rec%auxinput7_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_interval_s
SUBROUTINE nl_get_auxinput7_interval ( id_id , auxinput7_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_interval
  INTEGER id_id
  auxinput7_interval = model_config_rec%auxinput7_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_interval
SUBROUTINE nl_get_auxinput7_begin_y ( id_id , auxinput7_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_begin_y
  INTEGER id_id
  auxinput7_begin_y = model_config_rec%auxinput7_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_begin_y
SUBROUTINE nl_get_auxinput7_begin_d ( id_id , auxinput7_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_begin_d
  INTEGER id_id
  auxinput7_begin_d = model_config_rec%auxinput7_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_begin_d
SUBROUTINE nl_get_auxinput7_begin_h ( id_id , auxinput7_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_begin_h
  INTEGER id_id
  auxinput7_begin_h = model_config_rec%auxinput7_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_begin_h
SUBROUTINE nl_get_auxinput7_begin_m ( id_id , auxinput7_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_begin_m
  INTEGER id_id
  auxinput7_begin_m = model_config_rec%auxinput7_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_begin_m
SUBROUTINE nl_get_auxinput7_begin_s ( id_id , auxinput7_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_begin_s
  INTEGER id_id
  auxinput7_begin_s = model_config_rec%auxinput7_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_begin_s
SUBROUTINE nl_get_auxinput7_begin ( id_id , auxinput7_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_begin
  INTEGER id_id
  auxinput7_begin = model_config_rec%auxinput7_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_begin
SUBROUTINE nl_get_auxinput7_end_y ( id_id , auxinput7_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_end_y
  INTEGER id_id
  auxinput7_end_y = model_config_rec%auxinput7_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_end_y
SUBROUTINE nl_get_auxinput7_end_d ( id_id , auxinput7_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_end_d
  INTEGER id_id
  auxinput7_end_d = model_config_rec%auxinput7_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_end_d
SUBROUTINE nl_get_auxinput7_end_h ( id_id , auxinput7_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_end_h
  INTEGER id_id
  auxinput7_end_h = model_config_rec%auxinput7_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_end_h
SUBROUTINE nl_get_auxinput7_end_m ( id_id , auxinput7_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_end_m
  INTEGER id_id
  auxinput7_end_m = model_config_rec%auxinput7_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_end_m
SUBROUTINE nl_get_auxinput7_end_s ( id_id , auxinput7_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_end_s
  INTEGER id_id
  auxinput7_end_s = model_config_rec%auxinput7_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_end_s
SUBROUTINE nl_get_auxinput7_end ( id_id , auxinput7_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput7_end
  INTEGER id_id
  auxinput7_end = model_config_rec%auxinput7_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput7_end
#endif
#if (NNN == 4)
SUBROUTINE nl_get_io_form_auxinput7 ( id_id , io_form_auxinput7 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput7
  INTEGER id_id
  io_form_auxinput7 = model_config_rec%io_form_auxinput7
  RETURN
END SUBROUTINE nl_get_io_form_auxinput7
SUBROUTINE nl_get_frames_per_auxinput7 ( id_id , frames_per_auxinput7 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput7
  INTEGER id_id
  frames_per_auxinput7 = model_config_rec%frames_per_auxinput7(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput7
SUBROUTINE nl_get_auxinput8_inname ( id_id , auxinput8_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput8_inname
  INTEGER id_id
  auxinput8_inname = trim(model_config_rec%auxinput8_inname)
  RETURN
END SUBROUTINE nl_get_auxinput8_inname
SUBROUTINE nl_get_auxinput8_outname ( id_id , auxinput8_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput8_outname
  INTEGER id_id
  auxinput8_outname = trim(model_config_rec%auxinput8_outname)
  RETURN
END SUBROUTINE nl_get_auxinput8_outname
SUBROUTINE nl_get_auxinput8_interval_y ( id_id , auxinput8_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_interval_y
  INTEGER id_id
  auxinput8_interval_y = model_config_rec%auxinput8_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_interval_y
SUBROUTINE nl_get_auxinput8_interval_d ( id_id , auxinput8_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_interval_d
  INTEGER id_id
  auxinput8_interval_d = model_config_rec%auxinput8_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_interval_d
SUBROUTINE nl_get_auxinput8_interval_h ( id_id , auxinput8_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_interval_h
  INTEGER id_id
  auxinput8_interval_h = model_config_rec%auxinput8_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_interval_h
SUBROUTINE nl_get_auxinput8_interval_m ( id_id , auxinput8_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_interval_m
  INTEGER id_id
  auxinput8_interval_m = model_config_rec%auxinput8_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_interval_m
SUBROUTINE nl_get_auxinput8_interval_s ( id_id , auxinput8_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_interval_s
  INTEGER id_id
  auxinput8_interval_s = model_config_rec%auxinput8_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_interval_s
SUBROUTINE nl_get_auxinput8_interval ( id_id , auxinput8_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_interval
  INTEGER id_id
  auxinput8_interval = model_config_rec%auxinput8_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_interval
SUBROUTINE nl_get_auxinput8_begin_y ( id_id , auxinput8_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_begin_y
  INTEGER id_id
  auxinput8_begin_y = model_config_rec%auxinput8_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_begin_y
SUBROUTINE nl_get_auxinput8_begin_d ( id_id , auxinput8_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_begin_d
  INTEGER id_id
  auxinput8_begin_d = model_config_rec%auxinput8_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_begin_d
SUBROUTINE nl_get_auxinput8_begin_h ( id_id , auxinput8_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_begin_h
  INTEGER id_id
  auxinput8_begin_h = model_config_rec%auxinput8_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_begin_h
SUBROUTINE nl_get_auxinput8_begin_m ( id_id , auxinput8_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_begin_m
  INTEGER id_id
  auxinput8_begin_m = model_config_rec%auxinput8_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_begin_m
SUBROUTINE nl_get_auxinput8_begin_s ( id_id , auxinput8_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_begin_s
  INTEGER id_id
  auxinput8_begin_s = model_config_rec%auxinput8_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_begin_s
SUBROUTINE nl_get_auxinput8_begin ( id_id , auxinput8_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_begin
  INTEGER id_id
  auxinput8_begin = model_config_rec%auxinput8_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_begin
SUBROUTINE nl_get_auxinput8_end_y ( id_id , auxinput8_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_end_y
  INTEGER id_id
  auxinput8_end_y = model_config_rec%auxinput8_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_end_y
SUBROUTINE nl_get_auxinput8_end_d ( id_id , auxinput8_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_end_d
  INTEGER id_id
  auxinput8_end_d = model_config_rec%auxinput8_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_end_d
SUBROUTINE nl_get_auxinput8_end_h ( id_id , auxinput8_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_end_h
  INTEGER id_id
  auxinput8_end_h = model_config_rec%auxinput8_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_end_h
SUBROUTINE nl_get_auxinput8_end_m ( id_id , auxinput8_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_end_m
  INTEGER id_id
  auxinput8_end_m = model_config_rec%auxinput8_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_end_m
SUBROUTINE nl_get_auxinput8_end_s ( id_id , auxinput8_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_end_s
  INTEGER id_id
  auxinput8_end_s = model_config_rec%auxinput8_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_end_s
SUBROUTINE nl_get_auxinput8_end ( id_id , auxinput8_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput8_end
  INTEGER id_id
  auxinput8_end = model_config_rec%auxinput8_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput8_end
SUBROUTINE nl_get_io_form_auxinput8 ( id_id , io_form_auxinput8 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput8
  INTEGER id_id
  io_form_auxinput8 = model_config_rec%io_form_auxinput8
  RETURN
END SUBROUTINE nl_get_io_form_auxinput8
SUBROUTINE nl_get_frames_per_auxinput8 ( id_id , frames_per_auxinput8 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput8
  INTEGER id_id
  frames_per_auxinput8 = model_config_rec%frames_per_auxinput8(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput8
SUBROUTINE nl_get_auxinput9_inname ( id_id , auxinput9_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput9_inname
  INTEGER id_id
  auxinput9_inname = trim(model_config_rec%auxinput9_inname)
  RETURN
END SUBROUTINE nl_get_auxinput9_inname
SUBROUTINE nl_get_auxinput9_outname ( id_id , auxinput9_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput9_outname
  INTEGER id_id
  auxinput9_outname = trim(model_config_rec%auxinput9_outname)
  RETURN
END SUBROUTINE nl_get_auxinput9_outname
SUBROUTINE nl_get_auxinput9_interval_y ( id_id , auxinput9_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput9_interval_y
  INTEGER id_id
  auxinput9_interval_y = model_config_rec%auxinput9_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_interval_y
SUBROUTINE nl_get_auxinput9_interval_d ( id_id , auxinput9_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput9_interval_d
  INTEGER id_id
  auxinput9_interval_d = model_config_rec%auxinput9_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_interval_d
SUBROUTINE nl_get_auxinput9_interval_h ( id_id , auxinput9_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput9_interval_h
  INTEGER id_id
  auxinput9_interval_h = model_config_rec%auxinput9_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_interval_h
SUBROUTINE nl_get_auxinput9_interval_m ( id_id , auxinput9_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput9_interval_m
  INTEGER id_id
  auxinput9_interval_m = model_config_rec%auxinput9_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_interval_m
SUBROUTINE nl_get_auxinput9_interval_s ( id_id , auxinput9_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput9_interval_s
  INTEGER id_id
  auxinput9_interval_s = model_config_rec%auxinput9_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_interval_s
SUBROUTINE nl_get_auxinput9_interval ( id_id , auxinput9_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput9_interval
  INTEGER id_id
  auxinput9_interval = model_config_rec%auxinput9_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_interval
SUBROUTINE nl_get_auxinput9_begin_y ( id_id , auxinput9_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput9_begin_y
  INTEGER id_id
  auxinput9_begin_y = model_config_rec%auxinput9_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_begin_y
SUBROUTINE nl_get_auxinput9_begin_d ( id_id , auxinput9_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput9_begin_d
  INTEGER id_id
  auxinput9_begin_d = model_config_rec%auxinput9_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_begin_d
SUBROUTINE nl_get_auxinput9_begin_h ( id_id , auxinput9_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput9_begin_h
  INTEGER id_id
  auxinput9_begin_h = model_config_rec%auxinput9_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_begin_h
SUBROUTINE nl_get_auxinput9_begin_m ( id_id , auxinput9_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput9_begin_m
  INTEGER id_id
  auxinput9_begin_m = model_config_rec%auxinput9_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_begin_m
SUBROUTINE nl_get_auxinput9_begin_s ( id_id , auxinput9_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput9_begin_s
  INTEGER id_id
  auxinput9_begin_s = model_config_rec%auxinput9_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_begin_s
SUBROUTINE nl_get_auxinput9_begin ( id_id , auxinput9_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput9_begin
  INTEGER id_id
  auxinput9_begin = model_config_rec%auxinput9_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_begin
SUBROUTINE nl_get_auxinput9_end_y ( id_id , auxinput9_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput9_end_y
  INTEGER id_id
  auxinput9_end_y = model_config_rec%auxinput9_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_end_y
SUBROUTINE nl_get_auxinput9_end_d ( id_id , auxinput9_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput9_end_d
  INTEGER id_id
  auxinput9_end_d = model_config_rec%auxinput9_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_end_d
SUBROUTINE nl_get_auxinput9_end_h ( id_id , auxinput9_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput9_end_h
  INTEGER id_id
  auxinput9_end_h = model_config_rec%auxinput9_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_end_h
SUBROUTINE nl_get_auxinput9_end_m ( id_id , auxinput9_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput9_end_m
  INTEGER id_id
  auxinput9_end_m = model_config_rec%auxinput9_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_end_m
SUBROUTINE nl_get_auxinput9_end_s ( id_id , auxinput9_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput9_end_s
  INTEGER id_id
  auxinput9_end_s = model_config_rec%auxinput9_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_end_s
SUBROUTINE nl_get_auxinput9_end ( id_id , auxinput9_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput9_end
  INTEGER id_id
  auxinput9_end = model_config_rec%auxinput9_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput9_end
SUBROUTINE nl_get_io_form_auxinput9 ( id_id , io_form_auxinput9 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput9
  INTEGER id_id
  io_form_auxinput9 = model_config_rec%io_form_auxinput9
  RETURN
END SUBROUTINE nl_get_io_form_auxinput9
SUBROUTINE nl_get_frames_per_auxinput9 ( id_id , frames_per_auxinput9 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput9
  INTEGER id_id
  frames_per_auxinput9 = model_config_rec%frames_per_auxinput9(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput9
SUBROUTINE nl_get_auxinput10_inname ( id_id , auxinput10_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput10_inname
  INTEGER id_id
  auxinput10_inname = trim(model_config_rec%auxinput10_inname)
  RETURN
END SUBROUTINE nl_get_auxinput10_inname
SUBROUTINE nl_get_auxinput10_outname ( id_id , auxinput10_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput10_outname
  INTEGER id_id
  auxinput10_outname = trim(model_config_rec%auxinput10_outname)
  RETURN
END SUBROUTINE nl_get_auxinput10_outname
SUBROUTINE nl_get_auxinput10_interval_y ( id_id , auxinput10_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput10_interval_y
  INTEGER id_id
  auxinput10_interval_y = model_config_rec%auxinput10_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput10_interval_y
SUBROUTINE nl_get_auxinput10_interval_d ( id_id , auxinput10_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput10_interval_d
  INTEGER id_id
  auxinput10_interval_d = model_config_rec%auxinput10_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput10_interval_d
SUBROUTINE nl_get_auxinput10_interval_h ( id_id , auxinput10_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput10_interval_h
  INTEGER id_id
  auxinput10_interval_h = model_config_rec%auxinput10_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput10_interval_h
SUBROUTINE nl_get_auxinput10_interval_m ( id_id , auxinput10_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput10_interval_m
  INTEGER id_id
  auxinput10_interval_m = model_config_rec%auxinput10_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput10_interval_m
SUBROUTINE nl_get_auxinput10_interval_s ( id_id , auxinput10_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput10_interval_s
  INTEGER id_id
  auxinput10_interval_s = model_config_rec%auxinput10_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput10_interval_s
SUBROUTINE nl_get_auxinput10_interval ( id_id , auxinput10_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput10_interval
  INTEGER id_id
  auxinput10_interval = model_config_rec%auxinput10_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput10_interval
SUBROUTINE nl_get_auxinput10_begin_y ( id_id , auxinput10_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput10_begin_y
  INTEGER id_id
  auxinput10_begin_y = model_config_rec%auxinput10_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput10_begin_y
SUBROUTINE nl_get_auxinput10_begin_d ( id_id , auxinput10_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput10_begin_d
  INTEGER id_id
  auxinput10_begin_d = model_config_rec%auxinput10_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput10_begin_d
SUBROUTINE nl_get_auxinput10_begin_h ( id_id , auxinput10_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput10_begin_h
  INTEGER id_id
  auxinput10_begin_h = model_config_rec%auxinput10_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput10_begin_h
SUBROUTINE nl_get_auxinput10_begin_m ( id_id , auxinput10_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput10_begin_m
  INTEGER id_id
  auxinput10_begin_m = model_config_rec%auxinput10_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput10_begin_m
SUBROUTINE nl_get_auxinput10_begin_s ( id_id , auxinput10_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput10_begin_s
  INTEGER id_id
  auxinput10_begin_s = model_config_rec%auxinput10_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput10_begin_s
SUBROUTINE nl_get_auxinput10_begin ( id_id , auxinput10_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput10_begin
  INTEGER id_id
  auxinput10_begin = model_config_rec%auxinput10_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput10_begin
SUBROUTINE nl_get_auxinput10_end_y ( id_id , auxinput10_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput10_end_y
  INTEGER id_id
  auxinput10_end_y = model_config_rec%auxinput10_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput10_end_y
SUBROUTINE nl_get_auxinput10_end_d ( id_id , auxinput10_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput10_end_d
  INTEGER id_id
  auxinput10_end_d = model_config_rec%auxinput10_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput10_end_d
SUBROUTINE nl_get_auxinput10_end_h ( id_id , auxinput10_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput10_end_h
  INTEGER id_id
  auxinput10_end_h = model_config_rec%auxinput10_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput10_end_h
SUBROUTINE nl_get_auxinput10_end_m ( id_id , auxinput10_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput10_end_m
  INTEGER id_id
  auxinput10_end_m = model_config_rec%auxinput10_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput10_end_m
SUBROUTINE nl_get_auxinput10_end_s ( id_id , auxinput10_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput10_end_s
  INTEGER id_id
  auxinput10_end_s = model_config_rec%auxinput10_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput10_end_s
SUBROUTINE nl_get_auxinput10_end ( id_id , auxinput10_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput10_end
  INTEGER id_id
  auxinput10_end = model_config_rec%auxinput10_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput10_end
SUBROUTINE nl_get_io_form_auxinput10 ( id_id , io_form_auxinput10 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput10
  INTEGER id_id
  io_form_auxinput10 = model_config_rec%io_form_auxinput10
  RETURN
END SUBROUTINE nl_get_io_form_auxinput10
SUBROUTINE nl_get_frames_per_auxinput10 ( id_id , frames_per_auxinput10 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput10
  INTEGER id_id
  frames_per_auxinput10 = model_config_rec%frames_per_auxinput10(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput10
SUBROUTINE nl_get_auxinput11_inname ( id_id , auxinput11_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput11_inname
  INTEGER id_id
  auxinput11_inname = trim(model_config_rec%auxinput11_inname)
  RETURN
END SUBROUTINE nl_get_auxinput11_inname
SUBROUTINE nl_get_auxinput11_outname ( id_id , auxinput11_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput11_outname
  INTEGER id_id
  auxinput11_outname = trim(model_config_rec%auxinput11_outname)
  RETURN
END SUBROUTINE nl_get_auxinput11_outname
SUBROUTINE nl_get_auxinput11_interval_y ( id_id , auxinput11_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_interval_y
  INTEGER id_id
  auxinput11_interval_y = model_config_rec%auxinput11_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_interval_y
SUBROUTINE nl_get_auxinput11_interval_d ( id_id , auxinput11_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_interval_d
  INTEGER id_id
  auxinput11_interval_d = model_config_rec%auxinput11_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_interval_d
SUBROUTINE nl_get_auxinput11_interval_h ( id_id , auxinput11_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_interval_h
  INTEGER id_id
  auxinput11_interval_h = model_config_rec%auxinput11_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_interval_h
SUBROUTINE nl_get_auxinput11_interval_m ( id_id , auxinput11_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_interval_m
  INTEGER id_id
  auxinput11_interval_m = model_config_rec%auxinput11_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_interval_m
SUBROUTINE nl_get_auxinput11_interval_s ( id_id , auxinput11_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_interval_s
  INTEGER id_id
  auxinput11_interval_s = model_config_rec%auxinput11_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_interval_s
SUBROUTINE nl_get_auxinput11_interval ( id_id , auxinput11_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_interval
  INTEGER id_id
  auxinput11_interval = model_config_rec%auxinput11_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_interval
SUBROUTINE nl_get_auxinput11_begin_y ( id_id , auxinput11_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_begin_y
  INTEGER id_id
  auxinput11_begin_y = model_config_rec%auxinput11_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_begin_y
SUBROUTINE nl_get_auxinput11_begin_d ( id_id , auxinput11_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_begin_d
  INTEGER id_id
  auxinput11_begin_d = model_config_rec%auxinput11_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_begin_d
SUBROUTINE nl_get_auxinput11_begin_h ( id_id , auxinput11_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_begin_h
  INTEGER id_id
  auxinput11_begin_h = model_config_rec%auxinput11_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_begin_h
SUBROUTINE nl_get_auxinput11_begin_m ( id_id , auxinput11_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_begin_m
  INTEGER id_id
  auxinput11_begin_m = model_config_rec%auxinput11_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_begin_m
SUBROUTINE nl_get_auxinput11_begin_s ( id_id , auxinput11_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_begin_s
  INTEGER id_id
  auxinput11_begin_s = model_config_rec%auxinput11_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_begin_s
SUBROUTINE nl_get_auxinput11_begin ( id_id , auxinput11_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_begin
  INTEGER id_id
  auxinput11_begin = model_config_rec%auxinput11_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_begin
SUBROUTINE nl_get_auxinput11_end_y ( id_id , auxinput11_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_end_y
  INTEGER id_id
  auxinput11_end_y = model_config_rec%auxinput11_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_end_y
SUBROUTINE nl_get_auxinput11_end_d ( id_id , auxinput11_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_end_d
  INTEGER id_id
  auxinput11_end_d = model_config_rec%auxinput11_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_end_d
SUBROUTINE nl_get_auxinput11_end_h ( id_id , auxinput11_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_end_h
  INTEGER id_id
  auxinput11_end_h = model_config_rec%auxinput11_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_end_h
SUBROUTINE nl_get_auxinput11_end_m ( id_id , auxinput11_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_end_m
  INTEGER id_id
  auxinput11_end_m = model_config_rec%auxinput11_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_end_m
SUBROUTINE nl_get_auxinput11_end_s ( id_id , auxinput11_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_end_s
  INTEGER id_id
  auxinput11_end_s = model_config_rec%auxinput11_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_end_s
SUBROUTINE nl_get_auxinput11_end ( id_id , auxinput11_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput11_end
  INTEGER id_id
  auxinput11_end = model_config_rec%auxinput11_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput11_end
SUBROUTINE nl_get_io_form_auxinput11 ( id_id , io_form_auxinput11 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput11
  INTEGER id_id
  io_form_auxinput11 = model_config_rec%io_form_auxinput11
  RETURN
END SUBROUTINE nl_get_io_form_auxinput11
SUBROUTINE nl_get_frames_per_auxinput11 ( id_id , frames_per_auxinput11 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput11
  INTEGER id_id
  frames_per_auxinput11 = model_config_rec%frames_per_auxinput11(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput11
SUBROUTINE nl_get_auxinput12_inname ( id_id , auxinput12_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput12_inname
  INTEGER id_id
  auxinput12_inname = trim(model_config_rec%auxinput12_inname)
  RETURN
END SUBROUTINE nl_get_auxinput12_inname
SUBROUTINE nl_get_auxinput12_outname ( id_id , auxinput12_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput12_outname
  INTEGER id_id
  auxinput12_outname = trim(model_config_rec%auxinput12_outname)
  RETURN
END SUBROUTINE nl_get_auxinput12_outname
SUBROUTINE nl_get_auxinput12_interval_y ( id_id , auxinput12_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput12_interval_y
  INTEGER id_id
  auxinput12_interval_y = model_config_rec%auxinput12_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput12_interval_y
SUBROUTINE nl_get_auxinput12_interval_d ( id_id , auxinput12_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput12_interval_d
  INTEGER id_id
  auxinput12_interval_d = model_config_rec%auxinput12_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput12_interval_d
SUBROUTINE nl_get_auxinput12_interval_h ( id_id , auxinput12_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput12_interval_h
  INTEGER id_id
  auxinput12_interval_h = model_config_rec%auxinput12_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput12_interval_h
SUBROUTINE nl_get_auxinput12_interval_m ( id_id , auxinput12_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput12_interval_m
  INTEGER id_id
  auxinput12_interval_m = model_config_rec%auxinput12_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput12_interval_m
SUBROUTINE nl_get_auxinput12_interval_s ( id_id , auxinput12_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput12_interval_s
  INTEGER id_id
  auxinput12_interval_s = model_config_rec%auxinput12_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput12_interval_s
SUBROUTINE nl_get_auxinput12_interval ( id_id , auxinput12_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput12_interval
  INTEGER id_id
  auxinput12_interval = model_config_rec%auxinput12_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput12_interval
SUBROUTINE nl_get_auxinput12_begin_y ( id_id , auxinput12_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput12_begin_y
  INTEGER id_id
  auxinput12_begin_y = model_config_rec%auxinput12_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput12_begin_y
SUBROUTINE nl_get_auxinput12_begin_d ( id_id , auxinput12_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput12_begin_d
  INTEGER id_id
  auxinput12_begin_d = model_config_rec%auxinput12_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput12_begin_d
SUBROUTINE nl_get_auxinput12_begin_h ( id_id , auxinput12_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput12_begin_h
  INTEGER id_id
  auxinput12_begin_h = model_config_rec%auxinput12_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput12_begin_h
SUBROUTINE nl_get_auxinput12_begin_m ( id_id , auxinput12_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput12_begin_m
  INTEGER id_id
  auxinput12_begin_m = model_config_rec%auxinput12_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput12_begin_m
SUBROUTINE nl_get_auxinput12_begin_s ( id_id , auxinput12_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput12_begin_s
  INTEGER id_id
  auxinput12_begin_s = model_config_rec%auxinput12_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput12_begin_s
SUBROUTINE nl_get_auxinput12_begin ( id_id , auxinput12_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput12_begin
  INTEGER id_id
  auxinput12_begin = model_config_rec%auxinput12_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput12_begin
SUBROUTINE nl_get_auxinput12_end_y ( id_id , auxinput12_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput12_end_y
  INTEGER id_id
  auxinput12_end_y = model_config_rec%auxinput12_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput12_end_y
SUBROUTINE nl_get_auxinput12_end_d ( id_id , auxinput12_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput12_end_d
  INTEGER id_id
  auxinput12_end_d = model_config_rec%auxinput12_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput12_end_d
SUBROUTINE nl_get_auxinput12_end_h ( id_id , auxinput12_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput12_end_h
  INTEGER id_id
  auxinput12_end_h = model_config_rec%auxinput12_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput12_end_h
SUBROUTINE nl_get_auxinput12_end_m ( id_id , auxinput12_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput12_end_m
  INTEGER id_id
  auxinput12_end_m = model_config_rec%auxinput12_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput12_end_m
SUBROUTINE nl_get_auxinput12_end_s ( id_id , auxinput12_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput12_end_s
  INTEGER id_id
  auxinput12_end_s = model_config_rec%auxinput12_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput12_end_s
SUBROUTINE nl_get_auxinput12_end ( id_id , auxinput12_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput12_end
  INTEGER id_id
  auxinput12_end = model_config_rec%auxinput12_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput12_end
SUBROUTINE nl_get_io_form_auxinput12 ( id_id , io_form_auxinput12 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput12
  INTEGER id_id
  io_form_auxinput12 = model_config_rec%io_form_auxinput12
  RETURN
END SUBROUTINE nl_get_io_form_auxinput12
SUBROUTINE nl_get_frames_per_auxinput12 ( id_id , frames_per_auxinput12 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput12
  INTEGER id_id
  frames_per_auxinput12 = model_config_rec%frames_per_auxinput12(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput12
SUBROUTINE nl_get_auxinput13_inname ( id_id , auxinput13_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput13_inname
  INTEGER id_id
  auxinput13_inname = trim(model_config_rec%auxinput13_inname)
  RETURN
END SUBROUTINE nl_get_auxinput13_inname
SUBROUTINE nl_get_auxinput13_outname ( id_id , auxinput13_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput13_outname
  INTEGER id_id
  auxinput13_outname = trim(model_config_rec%auxinput13_outname)
  RETURN
END SUBROUTINE nl_get_auxinput13_outname
SUBROUTINE nl_get_auxinput13_interval_y ( id_id , auxinput13_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput13_interval_y
  INTEGER id_id
  auxinput13_interval_y = model_config_rec%auxinput13_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput13_interval_y
SUBROUTINE nl_get_auxinput13_interval_d ( id_id , auxinput13_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput13_interval_d
  INTEGER id_id
  auxinput13_interval_d = model_config_rec%auxinput13_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput13_interval_d
SUBROUTINE nl_get_auxinput13_interval_h ( id_id , auxinput13_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput13_interval_h
  INTEGER id_id
  auxinput13_interval_h = model_config_rec%auxinput13_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput13_interval_h
SUBROUTINE nl_get_auxinput13_interval_m ( id_id , auxinput13_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput13_interval_m
  INTEGER id_id
  auxinput13_interval_m = model_config_rec%auxinput13_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput13_interval_m
SUBROUTINE nl_get_auxinput13_interval_s ( id_id , auxinput13_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput13_interval_s
  INTEGER id_id
  auxinput13_interval_s = model_config_rec%auxinput13_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput13_interval_s
SUBROUTINE nl_get_auxinput13_interval ( id_id , auxinput13_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput13_interval
  INTEGER id_id
  auxinput13_interval = model_config_rec%auxinput13_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput13_interval
SUBROUTINE nl_get_auxinput13_begin_y ( id_id , auxinput13_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput13_begin_y
  INTEGER id_id
  auxinput13_begin_y = model_config_rec%auxinput13_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput13_begin_y
SUBROUTINE nl_get_auxinput13_begin_d ( id_id , auxinput13_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput13_begin_d
  INTEGER id_id
  auxinput13_begin_d = model_config_rec%auxinput13_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput13_begin_d
SUBROUTINE nl_get_auxinput13_begin_h ( id_id , auxinput13_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput13_begin_h
  INTEGER id_id
  auxinput13_begin_h = model_config_rec%auxinput13_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput13_begin_h
SUBROUTINE nl_get_auxinput13_begin_m ( id_id , auxinput13_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput13_begin_m
  INTEGER id_id
  auxinput13_begin_m = model_config_rec%auxinput13_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput13_begin_m
SUBROUTINE nl_get_auxinput13_begin_s ( id_id , auxinput13_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput13_begin_s
  INTEGER id_id
  auxinput13_begin_s = model_config_rec%auxinput13_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput13_begin_s
SUBROUTINE nl_get_auxinput13_begin ( id_id , auxinput13_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput13_begin
  INTEGER id_id
  auxinput13_begin = model_config_rec%auxinput13_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput13_begin
SUBROUTINE nl_get_auxinput13_end_y ( id_id , auxinput13_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput13_end_y
  INTEGER id_id
  auxinput13_end_y = model_config_rec%auxinput13_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput13_end_y
SUBROUTINE nl_get_auxinput13_end_d ( id_id , auxinput13_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput13_end_d
  INTEGER id_id
  auxinput13_end_d = model_config_rec%auxinput13_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput13_end_d
SUBROUTINE nl_get_auxinput13_end_h ( id_id , auxinput13_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput13_end_h
  INTEGER id_id
  auxinput13_end_h = model_config_rec%auxinput13_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput13_end_h
SUBROUTINE nl_get_auxinput13_end_m ( id_id , auxinput13_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput13_end_m
  INTEGER id_id
  auxinput13_end_m = model_config_rec%auxinput13_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput13_end_m
SUBROUTINE nl_get_auxinput13_end_s ( id_id , auxinput13_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput13_end_s
  INTEGER id_id
  auxinput13_end_s = model_config_rec%auxinput13_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput13_end_s
SUBROUTINE nl_get_auxinput13_end ( id_id , auxinput13_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput13_end
  INTEGER id_id
  auxinput13_end = model_config_rec%auxinput13_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput13_end
SUBROUTINE nl_get_io_form_auxinput13 ( id_id , io_form_auxinput13 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput13
  INTEGER id_id
  io_form_auxinput13 = model_config_rec%io_form_auxinput13
  RETURN
END SUBROUTINE nl_get_io_form_auxinput13
SUBROUTINE nl_get_frames_per_auxinput13 ( id_id , frames_per_auxinput13 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput13
  INTEGER id_id
  frames_per_auxinput13 = model_config_rec%frames_per_auxinput13(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput13
SUBROUTINE nl_get_auxinput14_inname ( id_id , auxinput14_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput14_inname
  INTEGER id_id
  auxinput14_inname = trim(model_config_rec%auxinput14_inname)
  RETURN
END SUBROUTINE nl_get_auxinput14_inname
SUBROUTINE nl_get_auxinput14_outname ( id_id , auxinput14_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput14_outname
  INTEGER id_id
  auxinput14_outname = trim(model_config_rec%auxinput14_outname)
  RETURN
END SUBROUTINE nl_get_auxinput14_outname
SUBROUTINE nl_get_auxinput14_interval_y ( id_id , auxinput14_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput14_interval_y
  INTEGER id_id
  auxinput14_interval_y = model_config_rec%auxinput14_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput14_interval_y
SUBROUTINE nl_get_auxinput14_interval_d ( id_id , auxinput14_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput14_interval_d
  INTEGER id_id
  auxinput14_interval_d = model_config_rec%auxinput14_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput14_interval_d
SUBROUTINE nl_get_auxinput14_interval_h ( id_id , auxinput14_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput14_interval_h
  INTEGER id_id
  auxinput14_interval_h = model_config_rec%auxinput14_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput14_interval_h
SUBROUTINE nl_get_auxinput14_interval_m ( id_id , auxinput14_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput14_interval_m
  INTEGER id_id
  auxinput14_interval_m = model_config_rec%auxinput14_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput14_interval_m
SUBROUTINE nl_get_auxinput14_interval_s ( id_id , auxinput14_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput14_interval_s
  INTEGER id_id
  auxinput14_interval_s = model_config_rec%auxinput14_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput14_interval_s
SUBROUTINE nl_get_auxinput14_interval ( id_id , auxinput14_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput14_interval
  INTEGER id_id
  auxinput14_interval = model_config_rec%auxinput14_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput14_interval
SUBROUTINE nl_get_auxinput14_begin_y ( id_id , auxinput14_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput14_begin_y
  INTEGER id_id
  auxinput14_begin_y = model_config_rec%auxinput14_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput14_begin_y
SUBROUTINE nl_get_auxinput14_begin_d ( id_id , auxinput14_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput14_begin_d
  INTEGER id_id
  auxinput14_begin_d = model_config_rec%auxinput14_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput14_begin_d
SUBROUTINE nl_get_auxinput14_begin_h ( id_id , auxinput14_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput14_begin_h
  INTEGER id_id
  auxinput14_begin_h = model_config_rec%auxinput14_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput14_begin_h
SUBROUTINE nl_get_auxinput14_begin_m ( id_id , auxinput14_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput14_begin_m
  INTEGER id_id
  auxinput14_begin_m = model_config_rec%auxinput14_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput14_begin_m
SUBROUTINE nl_get_auxinput14_begin_s ( id_id , auxinput14_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput14_begin_s
  INTEGER id_id
  auxinput14_begin_s = model_config_rec%auxinput14_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput14_begin_s
SUBROUTINE nl_get_auxinput14_begin ( id_id , auxinput14_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput14_begin
  INTEGER id_id
  auxinput14_begin = model_config_rec%auxinput14_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput14_begin
SUBROUTINE nl_get_auxinput14_end_y ( id_id , auxinput14_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput14_end_y
  INTEGER id_id
  auxinput14_end_y = model_config_rec%auxinput14_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput14_end_y
SUBROUTINE nl_get_auxinput14_end_d ( id_id , auxinput14_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput14_end_d
  INTEGER id_id
  auxinput14_end_d = model_config_rec%auxinput14_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput14_end_d
SUBROUTINE nl_get_auxinput14_end_h ( id_id , auxinput14_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput14_end_h
  INTEGER id_id
  auxinput14_end_h = model_config_rec%auxinput14_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput14_end_h
SUBROUTINE nl_get_auxinput14_end_m ( id_id , auxinput14_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput14_end_m
  INTEGER id_id
  auxinput14_end_m = model_config_rec%auxinput14_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput14_end_m
SUBROUTINE nl_get_auxinput14_end_s ( id_id , auxinput14_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput14_end_s
  INTEGER id_id
  auxinput14_end_s = model_config_rec%auxinput14_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput14_end_s
SUBROUTINE nl_get_auxinput14_end ( id_id , auxinput14_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput14_end
  INTEGER id_id
  auxinput14_end = model_config_rec%auxinput14_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput14_end
SUBROUTINE nl_get_io_form_auxinput14 ( id_id , io_form_auxinput14 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput14
  INTEGER id_id
  io_form_auxinput14 = model_config_rec%io_form_auxinput14
  RETURN
END SUBROUTINE nl_get_io_form_auxinput14
SUBROUTINE nl_get_frames_per_auxinput14 ( id_id , frames_per_auxinput14 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput14
  INTEGER id_id
  frames_per_auxinput14 = model_config_rec%frames_per_auxinput14(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput14
SUBROUTINE nl_get_auxinput15_inname ( id_id , auxinput15_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput15_inname
  INTEGER id_id
  auxinput15_inname = trim(model_config_rec%auxinput15_inname)
  RETURN
END SUBROUTINE nl_get_auxinput15_inname
SUBROUTINE nl_get_auxinput15_outname ( id_id , auxinput15_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput15_outname
  INTEGER id_id
  auxinput15_outname = trim(model_config_rec%auxinput15_outname)
  RETURN
END SUBROUTINE nl_get_auxinput15_outname
SUBROUTINE nl_get_auxinput15_interval_y ( id_id , auxinput15_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput15_interval_y
  INTEGER id_id
  auxinput15_interval_y = model_config_rec%auxinput15_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput15_interval_y
SUBROUTINE nl_get_auxinput15_interval_d ( id_id , auxinput15_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput15_interval_d
  INTEGER id_id
  auxinput15_interval_d = model_config_rec%auxinput15_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput15_interval_d
SUBROUTINE nl_get_auxinput15_interval_h ( id_id , auxinput15_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput15_interval_h
  INTEGER id_id
  auxinput15_interval_h = model_config_rec%auxinput15_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput15_interval_h
SUBROUTINE nl_get_auxinput15_interval_m ( id_id , auxinput15_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput15_interval_m
  INTEGER id_id
  auxinput15_interval_m = model_config_rec%auxinput15_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput15_interval_m
SUBROUTINE nl_get_auxinput15_interval_s ( id_id , auxinput15_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput15_interval_s
  INTEGER id_id
  auxinput15_interval_s = model_config_rec%auxinput15_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput15_interval_s
SUBROUTINE nl_get_auxinput15_interval ( id_id , auxinput15_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput15_interval
  INTEGER id_id
  auxinput15_interval = model_config_rec%auxinput15_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput15_interval
SUBROUTINE nl_get_auxinput15_begin_y ( id_id , auxinput15_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput15_begin_y
  INTEGER id_id
  auxinput15_begin_y = model_config_rec%auxinput15_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput15_begin_y
SUBROUTINE nl_get_auxinput15_begin_d ( id_id , auxinput15_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput15_begin_d
  INTEGER id_id
  auxinput15_begin_d = model_config_rec%auxinput15_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput15_begin_d
SUBROUTINE nl_get_auxinput15_begin_h ( id_id , auxinput15_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput15_begin_h
  INTEGER id_id
  auxinput15_begin_h = model_config_rec%auxinput15_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput15_begin_h
SUBROUTINE nl_get_auxinput15_begin_m ( id_id , auxinput15_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput15_begin_m
  INTEGER id_id
  auxinput15_begin_m = model_config_rec%auxinput15_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput15_begin_m
SUBROUTINE nl_get_auxinput15_begin_s ( id_id , auxinput15_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput15_begin_s
  INTEGER id_id
  auxinput15_begin_s = model_config_rec%auxinput15_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput15_begin_s
SUBROUTINE nl_get_auxinput15_begin ( id_id , auxinput15_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput15_begin
  INTEGER id_id
  auxinput15_begin = model_config_rec%auxinput15_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput15_begin
SUBROUTINE nl_get_auxinput15_end_y ( id_id , auxinput15_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput15_end_y
  INTEGER id_id
  auxinput15_end_y = model_config_rec%auxinput15_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput15_end_y
SUBROUTINE nl_get_auxinput15_end_d ( id_id , auxinput15_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput15_end_d
  INTEGER id_id
  auxinput15_end_d = model_config_rec%auxinput15_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput15_end_d
SUBROUTINE nl_get_auxinput15_end_h ( id_id , auxinput15_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput15_end_h
  INTEGER id_id
  auxinput15_end_h = model_config_rec%auxinput15_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput15_end_h
SUBROUTINE nl_get_auxinput15_end_m ( id_id , auxinput15_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput15_end_m
  INTEGER id_id
  auxinput15_end_m = model_config_rec%auxinput15_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput15_end_m
SUBROUTINE nl_get_auxinput15_end_s ( id_id , auxinput15_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput15_end_s
  INTEGER id_id
  auxinput15_end_s = model_config_rec%auxinput15_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput15_end_s
SUBROUTINE nl_get_auxinput15_end ( id_id , auxinput15_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput15_end
  INTEGER id_id
  auxinput15_end = model_config_rec%auxinput15_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput15_end
SUBROUTINE nl_get_io_form_auxinput15 ( id_id , io_form_auxinput15 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput15
  INTEGER id_id
  io_form_auxinput15 = model_config_rec%io_form_auxinput15
  RETURN
END SUBROUTINE nl_get_io_form_auxinput15
SUBROUTINE nl_get_frames_per_auxinput15 ( id_id , frames_per_auxinput15 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput15
  INTEGER id_id
  frames_per_auxinput15 = model_config_rec%frames_per_auxinput15(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput15
SUBROUTINE nl_get_auxinput16_inname ( id_id , auxinput16_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput16_inname
  INTEGER id_id
  auxinput16_inname = trim(model_config_rec%auxinput16_inname)
  RETURN
END SUBROUTINE nl_get_auxinput16_inname
SUBROUTINE nl_get_auxinput16_outname ( id_id , auxinput16_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput16_outname
  INTEGER id_id
  auxinput16_outname = trim(model_config_rec%auxinput16_outname)
  RETURN
END SUBROUTINE nl_get_auxinput16_outname
SUBROUTINE nl_get_auxinput16_interval_y ( id_id , auxinput16_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput16_interval_y
  INTEGER id_id
  auxinput16_interval_y = model_config_rec%auxinput16_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput16_interval_y
SUBROUTINE nl_get_auxinput16_interval_d ( id_id , auxinput16_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput16_interval_d
  INTEGER id_id
  auxinput16_interval_d = model_config_rec%auxinput16_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput16_interval_d
SUBROUTINE nl_get_auxinput16_interval_h ( id_id , auxinput16_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput16_interval_h
  INTEGER id_id
  auxinput16_interval_h = model_config_rec%auxinput16_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput16_interval_h
SUBROUTINE nl_get_auxinput16_interval_m ( id_id , auxinput16_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput16_interval_m
  INTEGER id_id
  auxinput16_interval_m = model_config_rec%auxinput16_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput16_interval_m
SUBROUTINE nl_get_auxinput16_interval_s ( id_id , auxinput16_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput16_interval_s
  INTEGER id_id
  auxinput16_interval_s = model_config_rec%auxinput16_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput16_interval_s
SUBROUTINE nl_get_auxinput16_interval ( id_id , auxinput16_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput16_interval
  INTEGER id_id
  auxinput16_interval = model_config_rec%auxinput16_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput16_interval
SUBROUTINE nl_get_auxinput16_begin_y ( id_id , auxinput16_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput16_begin_y
  INTEGER id_id
  auxinput16_begin_y = model_config_rec%auxinput16_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput16_begin_y
SUBROUTINE nl_get_auxinput16_begin_d ( id_id , auxinput16_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput16_begin_d
  INTEGER id_id
  auxinput16_begin_d = model_config_rec%auxinput16_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput16_begin_d
SUBROUTINE nl_get_auxinput16_begin_h ( id_id , auxinput16_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput16_begin_h
  INTEGER id_id
  auxinput16_begin_h = model_config_rec%auxinput16_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput16_begin_h
#endif
#if (NNN == 5)
SUBROUTINE nl_get_auxinput16_begin_m ( id_id , auxinput16_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput16_begin_m
  INTEGER id_id
  auxinput16_begin_m = model_config_rec%auxinput16_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput16_begin_m
SUBROUTINE nl_get_auxinput16_begin_s ( id_id , auxinput16_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput16_begin_s
  INTEGER id_id
  auxinput16_begin_s = model_config_rec%auxinput16_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput16_begin_s
SUBROUTINE nl_get_auxinput16_begin ( id_id , auxinput16_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput16_begin
  INTEGER id_id
  auxinput16_begin = model_config_rec%auxinput16_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput16_begin
SUBROUTINE nl_get_auxinput16_end_y ( id_id , auxinput16_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput16_end_y
  INTEGER id_id
  auxinput16_end_y = model_config_rec%auxinput16_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput16_end_y
SUBROUTINE nl_get_auxinput16_end_d ( id_id , auxinput16_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput16_end_d
  INTEGER id_id
  auxinput16_end_d = model_config_rec%auxinput16_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput16_end_d
SUBROUTINE nl_get_auxinput16_end_h ( id_id , auxinput16_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput16_end_h
  INTEGER id_id
  auxinput16_end_h = model_config_rec%auxinput16_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput16_end_h
SUBROUTINE nl_get_auxinput16_end_m ( id_id , auxinput16_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput16_end_m
  INTEGER id_id
  auxinput16_end_m = model_config_rec%auxinput16_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput16_end_m
SUBROUTINE nl_get_auxinput16_end_s ( id_id , auxinput16_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput16_end_s
  INTEGER id_id
  auxinput16_end_s = model_config_rec%auxinput16_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput16_end_s
SUBROUTINE nl_get_auxinput16_end ( id_id , auxinput16_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput16_end
  INTEGER id_id
  auxinput16_end = model_config_rec%auxinput16_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput16_end
SUBROUTINE nl_get_io_form_auxinput16 ( id_id , io_form_auxinput16 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput16
  INTEGER id_id
  io_form_auxinput16 = model_config_rec%io_form_auxinput16
  RETURN
END SUBROUTINE nl_get_io_form_auxinput16
SUBROUTINE nl_get_frames_per_auxinput16 ( id_id , frames_per_auxinput16 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput16
  INTEGER id_id
  frames_per_auxinput16 = model_config_rec%frames_per_auxinput16(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput16
SUBROUTINE nl_get_auxinput17_inname ( id_id , auxinput17_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput17_inname
  INTEGER id_id
  auxinput17_inname = trim(model_config_rec%auxinput17_inname)
  RETURN
END SUBROUTINE nl_get_auxinput17_inname
SUBROUTINE nl_get_auxinput17_outname ( id_id , auxinput17_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput17_outname
  INTEGER id_id
  auxinput17_outname = trim(model_config_rec%auxinput17_outname)
  RETURN
END SUBROUTINE nl_get_auxinput17_outname
SUBROUTINE nl_get_auxinput17_interval_y ( id_id , auxinput17_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput17_interval_y
  INTEGER id_id
  auxinput17_interval_y = model_config_rec%auxinput17_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput17_interval_y
SUBROUTINE nl_get_auxinput17_interval_d ( id_id , auxinput17_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput17_interval_d
  INTEGER id_id
  auxinput17_interval_d = model_config_rec%auxinput17_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput17_interval_d
SUBROUTINE nl_get_auxinput17_interval_h ( id_id , auxinput17_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput17_interval_h
  INTEGER id_id
  auxinput17_interval_h = model_config_rec%auxinput17_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput17_interval_h
SUBROUTINE nl_get_auxinput17_interval_m ( id_id , auxinput17_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput17_interval_m
  INTEGER id_id
  auxinput17_interval_m = model_config_rec%auxinput17_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput17_interval_m
SUBROUTINE nl_get_auxinput17_interval_s ( id_id , auxinput17_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput17_interval_s
  INTEGER id_id
  auxinput17_interval_s = model_config_rec%auxinput17_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput17_interval_s
SUBROUTINE nl_get_auxinput17_interval ( id_id , auxinput17_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput17_interval
  INTEGER id_id
  auxinput17_interval = model_config_rec%auxinput17_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput17_interval
SUBROUTINE nl_get_auxinput17_begin_y ( id_id , auxinput17_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput17_begin_y
  INTEGER id_id
  auxinput17_begin_y = model_config_rec%auxinput17_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput17_begin_y
SUBROUTINE nl_get_auxinput17_begin_d ( id_id , auxinput17_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput17_begin_d
  INTEGER id_id
  auxinput17_begin_d = model_config_rec%auxinput17_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput17_begin_d
SUBROUTINE nl_get_auxinput17_begin_h ( id_id , auxinput17_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput17_begin_h
  INTEGER id_id
  auxinput17_begin_h = model_config_rec%auxinput17_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput17_begin_h
SUBROUTINE nl_get_auxinput17_begin_m ( id_id , auxinput17_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput17_begin_m
  INTEGER id_id
  auxinput17_begin_m = model_config_rec%auxinput17_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput17_begin_m
SUBROUTINE nl_get_auxinput17_begin_s ( id_id , auxinput17_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput17_begin_s
  INTEGER id_id
  auxinput17_begin_s = model_config_rec%auxinput17_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput17_begin_s
SUBROUTINE nl_get_auxinput17_begin ( id_id , auxinput17_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput17_begin
  INTEGER id_id
  auxinput17_begin = model_config_rec%auxinput17_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput17_begin
SUBROUTINE nl_get_auxinput17_end_y ( id_id , auxinput17_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput17_end_y
  INTEGER id_id
  auxinput17_end_y = model_config_rec%auxinput17_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput17_end_y
SUBROUTINE nl_get_auxinput17_end_d ( id_id , auxinput17_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput17_end_d
  INTEGER id_id
  auxinput17_end_d = model_config_rec%auxinput17_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput17_end_d
SUBROUTINE nl_get_auxinput17_end_h ( id_id , auxinput17_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput17_end_h
  INTEGER id_id
  auxinput17_end_h = model_config_rec%auxinput17_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput17_end_h
SUBROUTINE nl_get_auxinput17_end_m ( id_id , auxinput17_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput17_end_m
  INTEGER id_id
  auxinput17_end_m = model_config_rec%auxinput17_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput17_end_m
SUBROUTINE nl_get_auxinput17_end_s ( id_id , auxinput17_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput17_end_s
  INTEGER id_id
  auxinput17_end_s = model_config_rec%auxinput17_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput17_end_s
SUBROUTINE nl_get_auxinput17_end ( id_id , auxinput17_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput17_end
  INTEGER id_id
  auxinput17_end = model_config_rec%auxinput17_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput17_end
SUBROUTINE nl_get_io_form_auxinput17 ( id_id , io_form_auxinput17 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput17
  INTEGER id_id
  io_form_auxinput17 = model_config_rec%io_form_auxinput17
  RETURN
END SUBROUTINE nl_get_io_form_auxinput17
SUBROUTINE nl_get_frames_per_auxinput17 ( id_id , frames_per_auxinput17 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput17
  INTEGER id_id
  frames_per_auxinput17 = model_config_rec%frames_per_auxinput17(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput17
SUBROUTINE nl_get_auxinput18_inname ( id_id , auxinput18_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput18_inname
  INTEGER id_id
  auxinput18_inname = trim(model_config_rec%auxinput18_inname)
  RETURN
END SUBROUTINE nl_get_auxinput18_inname
SUBROUTINE nl_get_auxinput18_outname ( id_id , auxinput18_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput18_outname
  INTEGER id_id
  auxinput18_outname = trim(model_config_rec%auxinput18_outname)
  RETURN
END SUBROUTINE nl_get_auxinput18_outname
SUBROUTINE nl_get_auxinput18_interval_y ( id_id , auxinput18_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput18_interval_y
  INTEGER id_id
  auxinput18_interval_y = model_config_rec%auxinput18_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput18_interval_y
SUBROUTINE nl_get_auxinput18_interval_d ( id_id , auxinput18_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput18_interval_d
  INTEGER id_id
  auxinput18_interval_d = model_config_rec%auxinput18_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput18_interval_d
SUBROUTINE nl_get_auxinput18_interval_h ( id_id , auxinput18_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput18_interval_h
  INTEGER id_id
  auxinput18_interval_h = model_config_rec%auxinput18_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput18_interval_h
SUBROUTINE nl_get_auxinput18_interval_m ( id_id , auxinput18_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput18_interval_m
  INTEGER id_id
  auxinput18_interval_m = model_config_rec%auxinput18_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput18_interval_m
SUBROUTINE nl_get_auxinput18_interval_s ( id_id , auxinput18_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput18_interval_s
  INTEGER id_id
  auxinput18_interval_s = model_config_rec%auxinput18_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput18_interval_s
SUBROUTINE nl_get_auxinput18_interval ( id_id , auxinput18_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput18_interval
  INTEGER id_id
  auxinput18_interval = model_config_rec%auxinput18_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput18_interval
SUBROUTINE nl_get_auxinput18_begin_y ( id_id , auxinput18_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput18_begin_y
  INTEGER id_id
  auxinput18_begin_y = model_config_rec%auxinput18_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput18_begin_y
SUBROUTINE nl_get_auxinput18_begin_d ( id_id , auxinput18_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput18_begin_d
  INTEGER id_id
  auxinput18_begin_d = model_config_rec%auxinput18_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput18_begin_d
SUBROUTINE nl_get_auxinput18_begin_h ( id_id , auxinput18_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput18_begin_h
  INTEGER id_id
  auxinput18_begin_h = model_config_rec%auxinput18_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput18_begin_h
SUBROUTINE nl_get_auxinput18_begin_m ( id_id , auxinput18_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput18_begin_m
  INTEGER id_id
  auxinput18_begin_m = model_config_rec%auxinput18_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput18_begin_m
SUBROUTINE nl_get_auxinput18_begin_s ( id_id , auxinput18_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput18_begin_s
  INTEGER id_id
  auxinput18_begin_s = model_config_rec%auxinput18_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput18_begin_s
SUBROUTINE nl_get_auxinput18_begin ( id_id , auxinput18_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput18_begin
  INTEGER id_id
  auxinput18_begin = model_config_rec%auxinput18_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput18_begin
SUBROUTINE nl_get_auxinput18_end_y ( id_id , auxinput18_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput18_end_y
  INTEGER id_id
  auxinput18_end_y = model_config_rec%auxinput18_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput18_end_y
SUBROUTINE nl_get_auxinput18_end_d ( id_id , auxinput18_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput18_end_d
  INTEGER id_id
  auxinput18_end_d = model_config_rec%auxinput18_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput18_end_d
SUBROUTINE nl_get_auxinput18_end_h ( id_id , auxinput18_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput18_end_h
  INTEGER id_id
  auxinput18_end_h = model_config_rec%auxinput18_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput18_end_h
SUBROUTINE nl_get_auxinput18_end_m ( id_id , auxinput18_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput18_end_m
  INTEGER id_id
  auxinput18_end_m = model_config_rec%auxinput18_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput18_end_m
SUBROUTINE nl_get_auxinput18_end_s ( id_id , auxinput18_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput18_end_s
  INTEGER id_id
  auxinput18_end_s = model_config_rec%auxinput18_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput18_end_s
SUBROUTINE nl_get_auxinput18_end ( id_id , auxinput18_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput18_end
  INTEGER id_id
  auxinput18_end = model_config_rec%auxinput18_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput18_end
SUBROUTINE nl_get_io_form_auxinput18 ( id_id , io_form_auxinput18 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput18
  INTEGER id_id
  io_form_auxinput18 = model_config_rec%io_form_auxinput18
  RETURN
END SUBROUTINE nl_get_io_form_auxinput18
SUBROUTINE nl_get_frames_per_auxinput18 ( id_id , frames_per_auxinput18 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput18
  INTEGER id_id
  frames_per_auxinput18 = model_config_rec%frames_per_auxinput18(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput18
SUBROUTINE nl_get_auxinput19_inname ( id_id , auxinput19_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput19_inname
  INTEGER id_id
  auxinput19_inname = trim(model_config_rec%auxinput19_inname)
  RETURN
END SUBROUTINE nl_get_auxinput19_inname
SUBROUTINE nl_get_auxinput19_outname ( id_id , auxinput19_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput19_outname
  INTEGER id_id
  auxinput19_outname = trim(model_config_rec%auxinput19_outname)
  RETURN
END SUBROUTINE nl_get_auxinput19_outname
SUBROUTINE nl_get_auxinput19_interval_y ( id_id , auxinput19_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput19_interval_y
  INTEGER id_id
  auxinput19_interval_y = model_config_rec%auxinput19_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput19_interval_y
SUBROUTINE nl_get_auxinput19_interval_d ( id_id , auxinput19_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput19_interval_d
  INTEGER id_id
  auxinput19_interval_d = model_config_rec%auxinput19_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput19_interval_d
SUBROUTINE nl_get_auxinput19_interval_h ( id_id , auxinput19_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput19_interval_h
  INTEGER id_id
  auxinput19_interval_h = model_config_rec%auxinput19_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput19_interval_h
SUBROUTINE nl_get_auxinput19_interval_m ( id_id , auxinput19_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput19_interval_m
  INTEGER id_id
  auxinput19_interval_m = model_config_rec%auxinput19_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput19_interval_m
SUBROUTINE nl_get_auxinput19_interval_s ( id_id , auxinput19_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput19_interval_s
  INTEGER id_id
  auxinput19_interval_s = model_config_rec%auxinput19_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput19_interval_s
SUBROUTINE nl_get_auxinput19_interval ( id_id , auxinput19_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput19_interval
  INTEGER id_id
  auxinput19_interval = model_config_rec%auxinput19_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput19_interval
SUBROUTINE nl_get_auxinput19_begin_y ( id_id , auxinput19_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput19_begin_y
  INTEGER id_id
  auxinput19_begin_y = model_config_rec%auxinput19_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput19_begin_y
SUBROUTINE nl_get_auxinput19_begin_d ( id_id , auxinput19_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput19_begin_d
  INTEGER id_id
  auxinput19_begin_d = model_config_rec%auxinput19_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput19_begin_d
SUBROUTINE nl_get_auxinput19_begin_h ( id_id , auxinput19_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput19_begin_h
  INTEGER id_id
  auxinput19_begin_h = model_config_rec%auxinput19_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput19_begin_h
SUBROUTINE nl_get_auxinput19_begin_m ( id_id , auxinput19_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput19_begin_m
  INTEGER id_id
  auxinput19_begin_m = model_config_rec%auxinput19_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput19_begin_m
SUBROUTINE nl_get_auxinput19_begin_s ( id_id , auxinput19_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput19_begin_s
  INTEGER id_id
  auxinput19_begin_s = model_config_rec%auxinput19_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput19_begin_s
SUBROUTINE nl_get_auxinput19_begin ( id_id , auxinput19_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput19_begin
  INTEGER id_id
  auxinput19_begin = model_config_rec%auxinput19_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput19_begin
SUBROUTINE nl_get_auxinput19_end_y ( id_id , auxinput19_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput19_end_y
  INTEGER id_id
  auxinput19_end_y = model_config_rec%auxinput19_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput19_end_y
SUBROUTINE nl_get_auxinput19_end_d ( id_id , auxinput19_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput19_end_d
  INTEGER id_id
  auxinput19_end_d = model_config_rec%auxinput19_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput19_end_d
SUBROUTINE nl_get_auxinput19_end_h ( id_id , auxinput19_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput19_end_h
  INTEGER id_id
  auxinput19_end_h = model_config_rec%auxinput19_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput19_end_h
SUBROUTINE nl_get_auxinput19_end_m ( id_id , auxinput19_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput19_end_m
  INTEGER id_id
  auxinput19_end_m = model_config_rec%auxinput19_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput19_end_m
SUBROUTINE nl_get_auxinput19_end_s ( id_id , auxinput19_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput19_end_s
  INTEGER id_id
  auxinput19_end_s = model_config_rec%auxinput19_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput19_end_s
SUBROUTINE nl_get_auxinput19_end ( id_id , auxinput19_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput19_end
  INTEGER id_id
  auxinput19_end = model_config_rec%auxinput19_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput19_end
SUBROUTINE nl_get_io_form_auxinput19 ( id_id , io_form_auxinput19 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput19
  INTEGER id_id
  io_form_auxinput19 = model_config_rec%io_form_auxinput19
  RETURN
END SUBROUTINE nl_get_io_form_auxinput19
SUBROUTINE nl_get_frames_per_auxinput19 ( id_id , frames_per_auxinput19 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput19
  INTEGER id_id
  frames_per_auxinput19 = model_config_rec%frames_per_auxinput19(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput19
SUBROUTINE nl_get_auxinput20_inname ( id_id , auxinput20_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput20_inname
  INTEGER id_id
  auxinput20_inname = trim(model_config_rec%auxinput20_inname)
  RETURN
END SUBROUTINE nl_get_auxinput20_inname
SUBROUTINE nl_get_auxinput20_outname ( id_id , auxinput20_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput20_outname
  INTEGER id_id
  auxinput20_outname = trim(model_config_rec%auxinput20_outname)
  RETURN
END SUBROUTINE nl_get_auxinput20_outname
SUBROUTINE nl_get_auxinput20_interval_y ( id_id , auxinput20_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput20_interval_y
  INTEGER id_id
  auxinput20_interval_y = model_config_rec%auxinput20_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput20_interval_y
SUBROUTINE nl_get_auxinput20_interval_d ( id_id , auxinput20_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput20_interval_d
  INTEGER id_id
  auxinput20_interval_d = model_config_rec%auxinput20_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput20_interval_d
SUBROUTINE nl_get_auxinput20_interval_h ( id_id , auxinput20_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput20_interval_h
  INTEGER id_id
  auxinput20_interval_h = model_config_rec%auxinput20_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput20_interval_h
SUBROUTINE nl_get_auxinput20_interval_m ( id_id , auxinput20_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput20_interval_m
  INTEGER id_id
  auxinput20_interval_m = model_config_rec%auxinput20_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput20_interval_m
SUBROUTINE nl_get_auxinput20_interval_s ( id_id , auxinput20_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput20_interval_s
  INTEGER id_id
  auxinput20_interval_s = model_config_rec%auxinput20_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput20_interval_s
SUBROUTINE nl_get_auxinput20_interval ( id_id , auxinput20_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput20_interval
  INTEGER id_id
  auxinput20_interval = model_config_rec%auxinput20_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput20_interval
SUBROUTINE nl_get_auxinput20_begin_y ( id_id , auxinput20_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput20_begin_y
  INTEGER id_id
  auxinput20_begin_y = model_config_rec%auxinput20_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput20_begin_y
SUBROUTINE nl_get_auxinput20_begin_d ( id_id , auxinput20_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput20_begin_d
  INTEGER id_id
  auxinput20_begin_d = model_config_rec%auxinput20_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput20_begin_d
SUBROUTINE nl_get_auxinput20_begin_h ( id_id , auxinput20_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput20_begin_h
  INTEGER id_id
  auxinput20_begin_h = model_config_rec%auxinput20_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput20_begin_h
SUBROUTINE nl_get_auxinput20_begin_m ( id_id , auxinput20_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput20_begin_m
  INTEGER id_id
  auxinput20_begin_m = model_config_rec%auxinput20_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput20_begin_m
SUBROUTINE nl_get_auxinput20_begin_s ( id_id , auxinput20_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput20_begin_s
  INTEGER id_id
  auxinput20_begin_s = model_config_rec%auxinput20_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput20_begin_s
SUBROUTINE nl_get_auxinput20_begin ( id_id , auxinput20_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput20_begin
  INTEGER id_id
  auxinput20_begin = model_config_rec%auxinput20_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput20_begin
SUBROUTINE nl_get_auxinput20_end_y ( id_id , auxinput20_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput20_end_y
  INTEGER id_id
  auxinput20_end_y = model_config_rec%auxinput20_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput20_end_y
SUBROUTINE nl_get_auxinput20_end_d ( id_id , auxinput20_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput20_end_d
  INTEGER id_id
  auxinput20_end_d = model_config_rec%auxinput20_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput20_end_d
SUBROUTINE nl_get_auxinput20_end_h ( id_id , auxinput20_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput20_end_h
  INTEGER id_id
  auxinput20_end_h = model_config_rec%auxinput20_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput20_end_h
SUBROUTINE nl_get_auxinput20_end_m ( id_id , auxinput20_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput20_end_m
  INTEGER id_id
  auxinput20_end_m = model_config_rec%auxinput20_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput20_end_m
SUBROUTINE nl_get_auxinput20_end_s ( id_id , auxinput20_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput20_end_s
  INTEGER id_id
  auxinput20_end_s = model_config_rec%auxinput20_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput20_end_s
SUBROUTINE nl_get_auxinput20_end ( id_id , auxinput20_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput20_end
  INTEGER id_id
  auxinput20_end = model_config_rec%auxinput20_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput20_end
SUBROUTINE nl_get_io_form_auxinput20 ( id_id , io_form_auxinput20 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput20
  INTEGER id_id
  io_form_auxinput20 = model_config_rec%io_form_auxinput20
  RETURN
END SUBROUTINE nl_get_io_form_auxinput20
SUBROUTINE nl_get_frames_per_auxinput20 ( id_id , frames_per_auxinput20 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput20
  INTEGER id_id
  frames_per_auxinput20 = model_config_rec%frames_per_auxinput20(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput20
SUBROUTINE nl_get_auxinput21_inname ( id_id , auxinput21_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput21_inname
  INTEGER id_id
  auxinput21_inname = trim(model_config_rec%auxinput21_inname)
  RETURN
END SUBROUTINE nl_get_auxinput21_inname
SUBROUTINE nl_get_auxinput21_outname ( id_id , auxinput21_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput21_outname
  INTEGER id_id
  auxinput21_outname = trim(model_config_rec%auxinput21_outname)
  RETURN
END SUBROUTINE nl_get_auxinput21_outname
SUBROUTINE nl_get_auxinput21_interval_y ( id_id , auxinput21_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput21_interval_y
  INTEGER id_id
  auxinput21_interval_y = model_config_rec%auxinput21_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput21_interval_y
SUBROUTINE nl_get_auxinput21_interval_d ( id_id , auxinput21_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput21_interval_d
  INTEGER id_id
  auxinput21_interval_d = model_config_rec%auxinput21_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput21_interval_d
SUBROUTINE nl_get_auxinput21_interval_h ( id_id , auxinput21_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput21_interval_h
  INTEGER id_id
  auxinput21_interval_h = model_config_rec%auxinput21_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput21_interval_h
SUBROUTINE nl_get_auxinput21_interval_m ( id_id , auxinput21_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput21_interval_m
  INTEGER id_id
  auxinput21_interval_m = model_config_rec%auxinput21_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput21_interval_m
SUBROUTINE nl_get_auxinput21_interval_s ( id_id , auxinput21_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput21_interval_s
  INTEGER id_id
  auxinput21_interval_s = model_config_rec%auxinput21_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput21_interval_s
SUBROUTINE nl_get_auxinput21_interval ( id_id , auxinput21_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput21_interval
  INTEGER id_id
  auxinput21_interval = model_config_rec%auxinput21_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput21_interval
SUBROUTINE nl_get_auxinput21_begin_y ( id_id , auxinput21_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput21_begin_y
  INTEGER id_id
  auxinput21_begin_y = model_config_rec%auxinput21_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput21_begin_y
SUBROUTINE nl_get_auxinput21_begin_d ( id_id , auxinput21_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput21_begin_d
  INTEGER id_id
  auxinput21_begin_d = model_config_rec%auxinput21_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput21_begin_d
SUBROUTINE nl_get_auxinput21_begin_h ( id_id , auxinput21_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput21_begin_h
  INTEGER id_id
  auxinput21_begin_h = model_config_rec%auxinput21_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput21_begin_h
SUBROUTINE nl_get_auxinput21_begin_m ( id_id , auxinput21_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput21_begin_m
  INTEGER id_id
  auxinput21_begin_m = model_config_rec%auxinput21_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput21_begin_m
SUBROUTINE nl_get_auxinput21_begin_s ( id_id , auxinput21_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput21_begin_s
  INTEGER id_id
  auxinput21_begin_s = model_config_rec%auxinput21_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput21_begin_s
SUBROUTINE nl_get_auxinput21_begin ( id_id , auxinput21_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput21_begin
  INTEGER id_id
  auxinput21_begin = model_config_rec%auxinput21_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput21_begin
SUBROUTINE nl_get_auxinput21_end_y ( id_id , auxinput21_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput21_end_y
  INTEGER id_id
  auxinput21_end_y = model_config_rec%auxinput21_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput21_end_y
SUBROUTINE nl_get_auxinput21_end_d ( id_id , auxinput21_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput21_end_d
  INTEGER id_id
  auxinput21_end_d = model_config_rec%auxinput21_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput21_end_d
SUBROUTINE nl_get_auxinput21_end_h ( id_id , auxinput21_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput21_end_h
  INTEGER id_id
  auxinput21_end_h = model_config_rec%auxinput21_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput21_end_h
SUBROUTINE nl_get_auxinput21_end_m ( id_id , auxinput21_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput21_end_m
  INTEGER id_id
  auxinput21_end_m = model_config_rec%auxinput21_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput21_end_m
SUBROUTINE nl_get_auxinput21_end_s ( id_id , auxinput21_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput21_end_s
  INTEGER id_id
  auxinput21_end_s = model_config_rec%auxinput21_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput21_end_s
SUBROUTINE nl_get_auxinput21_end ( id_id , auxinput21_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput21_end
  INTEGER id_id
  auxinput21_end = model_config_rec%auxinput21_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput21_end
SUBROUTINE nl_get_io_form_auxinput21 ( id_id , io_form_auxinput21 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput21
  INTEGER id_id
  io_form_auxinput21 = model_config_rec%io_form_auxinput21
  RETURN
END SUBROUTINE nl_get_io_form_auxinput21
SUBROUTINE nl_get_frames_per_auxinput21 ( id_id , frames_per_auxinput21 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput21
  INTEGER id_id
  frames_per_auxinput21 = model_config_rec%frames_per_auxinput21(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput21
SUBROUTINE nl_get_auxinput22_inname ( id_id , auxinput22_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput22_inname
  INTEGER id_id
  auxinput22_inname = trim(model_config_rec%auxinput22_inname)
  RETURN
END SUBROUTINE nl_get_auxinput22_inname
SUBROUTINE nl_get_auxinput22_outname ( id_id , auxinput22_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput22_outname
  INTEGER id_id
  auxinput22_outname = trim(model_config_rec%auxinput22_outname)
  RETURN
END SUBROUTINE nl_get_auxinput22_outname
SUBROUTINE nl_get_auxinput22_interval_y ( id_id , auxinput22_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput22_interval_y
  INTEGER id_id
  auxinput22_interval_y = model_config_rec%auxinput22_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput22_interval_y
SUBROUTINE nl_get_auxinput22_interval_d ( id_id , auxinput22_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput22_interval_d
  INTEGER id_id
  auxinput22_interval_d = model_config_rec%auxinput22_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput22_interval_d
SUBROUTINE nl_get_auxinput22_interval_h ( id_id , auxinput22_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput22_interval_h
  INTEGER id_id
  auxinput22_interval_h = model_config_rec%auxinput22_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput22_interval_h
SUBROUTINE nl_get_auxinput22_interval_m ( id_id , auxinput22_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput22_interval_m
  INTEGER id_id
  auxinput22_interval_m = model_config_rec%auxinput22_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput22_interval_m
SUBROUTINE nl_get_auxinput22_interval_s ( id_id , auxinput22_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput22_interval_s
  INTEGER id_id
  auxinput22_interval_s = model_config_rec%auxinput22_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput22_interval_s
SUBROUTINE nl_get_auxinput22_interval ( id_id , auxinput22_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput22_interval
  INTEGER id_id
  auxinput22_interval = model_config_rec%auxinput22_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput22_interval
SUBROUTINE nl_get_auxinput22_begin_y ( id_id , auxinput22_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput22_begin_y
  INTEGER id_id
  auxinput22_begin_y = model_config_rec%auxinput22_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput22_begin_y
SUBROUTINE nl_get_auxinput22_begin_d ( id_id , auxinput22_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput22_begin_d
  INTEGER id_id
  auxinput22_begin_d = model_config_rec%auxinput22_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput22_begin_d
SUBROUTINE nl_get_auxinput22_begin_h ( id_id , auxinput22_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput22_begin_h
  INTEGER id_id
  auxinput22_begin_h = model_config_rec%auxinput22_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput22_begin_h
SUBROUTINE nl_get_auxinput22_begin_m ( id_id , auxinput22_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput22_begin_m
  INTEGER id_id
  auxinput22_begin_m = model_config_rec%auxinput22_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput22_begin_m
SUBROUTINE nl_get_auxinput22_begin_s ( id_id , auxinput22_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput22_begin_s
  INTEGER id_id
  auxinput22_begin_s = model_config_rec%auxinput22_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput22_begin_s
SUBROUTINE nl_get_auxinput22_begin ( id_id , auxinput22_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput22_begin
  INTEGER id_id
  auxinput22_begin = model_config_rec%auxinput22_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput22_begin
SUBROUTINE nl_get_auxinput22_end_y ( id_id , auxinput22_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput22_end_y
  INTEGER id_id
  auxinput22_end_y = model_config_rec%auxinput22_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput22_end_y
SUBROUTINE nl_get_auxinput22_end_d ( id_id , auxinput22_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput22_end_d
  INTEGER id_id
  auxinput22_end_d = model_config_rec%auxinput22_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput22_end_d
SUBROUTINE nl_get_auxinput22_end_h ( id_id , auxinput22_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput22_end_h
  INTEGER id_id
  auxinput22_end_h = model_config_rec%auxinput22_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput22_end_h
SUBROUTINE nl_get_auxinput22_end_m ( id_id , auxinput22_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput22_end_m
  INTEGER id_id
  auxinput22_end_m = model_config_rec%auxinput22_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput22_end_m
SUBROUTINE nl_get_auxinput22_end_s ( id_id , auxinput22_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput22_end_s
  INTEGER id_id
  auxinput22_end_s = model_config_rec%auxinput22_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput22_end_s
SUBROUTINE nl_get_auxinput22_end ( id_id , auxinput22_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput22_end
  INTEGER id_id
  auxinput22_end = model_config_rec%auxinput22_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput22_end
SUBROUTINE nl_get_io_form_auxinput22 ( id_id , io_form_auxinput22 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput22
  INTEGER id_id
  io_form_auxinput22 = model_config_rec%io_form_auxinput22
  RETURN
END SUBROUTINE nl_get_io_form_auxinput22
SUBROUTINE nl_get_frames_per_auxinput22 ( id_id , frames_per_auxinput22 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput22
  INTEGER id_id
  frames_per_auxinput22 = model_config_rec%frames_per_auxinput22(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput22
SUBROUTINE nl_get_auxinput23_inname ( id_id , auxinput23_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput23_inname
  INTEGER id_id
  auxinput23_inname = trim(model_config_rec%auxinput23_inname)
  RETURN
END SUBROUTINE nl_get_auxinput23_inname
SUBROUTINE nl_get_auxinput23_outname ( id_id , auxinput23_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput23_outname
  INTEGER id_id
  auxinput23_outname = trim(model_config_rec%auxinput23_outname)
  RETURN
END SUBROUTINE nl_get_auxinput23_outname
SUBROUTINE nl_get_auxinput23_interval_y ( id_id , auxinput23_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput23_interval_y
  INTEGER id_id
  auxinput23_interval_y = model_config_rec%auxinput23_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput23_interval_y
SUBROUTINE nl_get_auxinput23_interval_d ( id_id , auxinput23_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput23_interval_d
  INTEGER id_id
  auxinput23_interval_d = model_config_rec%auxinput23_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput23_interval_d
SUBROUTINE nl_get_auxinput23_interval_h ( id_id , auxinput23_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput23_interval_h
  INTEGER id_id
  auxinput23_interval_h = model_config_rec%auxinput23_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput23_interval_h
SUBROUTINE nl_get_auxinput23_interval_m ( id_id , auxinput23_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput23_interval_m
  INTEGER id_id
  auxinput23_interval_m = model_config_rec%auxinput23_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput23_interval_m
SUBROUTINE nl_get_auxinput23_interval_s ( id_id , auxinput23_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput23_interval_s
  INTEGER id_id
  auxinput23_interval_s = model_config_rec%auxinput23_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput23_interval_s
SUBROUTINE nl_get_auxinput23_interval ( id_id , auxinput23_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput23_interval
  INTEGER id_id
  auxinput23_interval = model_config_rec%auxinput23_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput23_interval
SUBROUTINE nl_get_auxinput23_begin_y ( id_id , auxinput23_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput23_begin_y
  INTEGER id_id
  auxinput23_begin_y = model_config_rec%auxinput23_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput23_begin_y
SUBROUTINE nl_get_auxinput23_begin_d ( id_id , auxinput23_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput23_begin_d
  INTEGER id_id
  auxinput23_begin_d = model_config_rec%auxinput23_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput23_begin_d
SUBROUTINE nl_get_auxinput23_begin_h ( id_id , auxinput23_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput23_begin_h
  INTEGER id_id
  auxinput23_begin_h = model_config_rec%auxinput23_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput23_begin_h
SUBROUTINE nl_get_auxinput23_begin_m ( id_id , auxinput23_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput23_begin_m
  INTEGER id_id
  auxinput23_begin_m = model_config_rec%auxinput23_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput23_begin_m
SUBROUTINE nl_get_auxinput23_begin_s ( id_id , auxinput23_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput23_begin_s
  INTEGER id_id
  auxinput23_begin_s = model_config_rec%auxinput23_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput23_begin_s
SUBROUTINE nl_get_auxinput23_begin ( id_id , auxinput23_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput23_begin
  INTEGER id_id
  auxinput23_begin = model_config_rec%auxinput23_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput23_begin
SUBROUTINE nl_get_auxinput23_end_y ( id_id , auxinput23_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput23_end_y
  INTEGER id_id
  auxinput23_end_y = model_config_rec%auxinput23_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput23_end_y
SUBROUTINE nl_get_auxinput23_end_d ( id_id , auxinput23_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput23_end_d
  INTEGER id_id
  auxinput23_end_d = model_config_rec%auxinput23_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput23_end_d
SUBROUTINE nl_get_auxinput23_end_h ( id_id , auxinput23_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput23_end_h
  INTEGER id_id
  auxinput23_end_h = model_config_rec%auxinput23_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput23_end_h
SUBROUTINE nl_get_auxinput23_end_m ( id_id , auxinput23_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput23_end_m
  INTEGER id_id
  auxinput23_end_m = model_config_rec%auxinput23_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput23_end_m
SUBROUTINE nl_get_auxinput23_end_s ( id_id , auxinput23_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput23_end_s
  INTEGER id_id
  auxinput23_end_s = model_config_rec%auxinput23_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput23_end_s
SUBROUTINE nl_get_auxinput23_end ( id_id , auxinput23_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput23_end
  INTEGER id_id
  auxinput23_end = model_config_rec%auxinput23_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput23_end
SUBROUTINE nl_get_io_form_auxinput23 ( id_id , io_form_auxinput23 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput23
  INTEGER id_id
  io_form_auxinput23 = model_config_rec%io_form_auxinput23
  RETURN
END SUBROUTINE nl_get_io_form_auxinput23
SUBROUTINE nl_get_frames_per_auxinput23 ( id_id , frames_per_auxinput23 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput23
  INTEGER id_id
  frames_per_auxinput23 = model_config_rec%frames_per_auxinput23(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput23
SUBROUTINE nl_get_auxinput24_inname ( id_id , auxinput24_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput24_inname
  INTEGER id_id
  auxinput24_inname = trim(model_config_rec%auxinput24_inname)
  RETURN
END SUBROUTINE nl_get_auxinput24_inname
SUBROUTINE nl_get_auxinput24_outname ( id_id , auxinput24_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: auxinput24_outname
  INTEGER id_id
  auxinput24_outname = trim(model_config_rec%auxinput24_outname)
  RETURN
END SUBROUTINE nl_get_auxinput24_outname
SUBROUTINE nl_get_auxinput24_interval_y ( id_id , auxinput24_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput24_interval_y
  INTEGER id_id
  auxinput24_interval_y = model_config_rec%auxinput24_interval_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput24_interval_y
SUBROUTINE nl_get_auxinput24_interval_d ( id_id , auxinput24_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput24_interval_d
  INTEGER id_id
  auxinput24_interval_d = model_config_rec%auxinput24_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput24_interval_d
SUBROUTINE nl_get_auxinput24_interval_h ( id_id , auxinput24_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput24_interval_h
  INTEGER id_id
  auxinput24_interval_h = model_config_rec%auxinput24_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput24_interval_h
SUBROUTINE nl_get_auxinput24_interval_m ( id_id , auxinput24_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput24_interval_m
  INTEGER id_id
  auxinput24_interval_m = model_config_rec%auxinput24_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput24_interval_m
SUBROUTINE nl_get_auxinput24_interval_s ( id_id , auxinput24_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput24_interval_s
  INTEGER id_id
  auxinput24_interval_s = model_config_rec%auxinput24_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput24_interval_s
SUBROUTINE nl_get_auxinput24_interval ( id_id , auxinput24_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput24_interval
  INTEGER id_id
  auxinput24_interval = model_config_rec%auxinput24_interval(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput24_interval
SUBROUTINE nl_get_auxinput24_begin_y ( id_id , auxinput24_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput24_begin_y
  INTEGER id_id
  auxinput24_begin_y = model_config_rec%auxinput24_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput24_begin_y
SUBROUTINE nl_get_auxinput24_begin_d ( id_id , auxinput24_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput24_begin_d
  INTEGER id_id
  auxinput24_begin_d = model_config_rec%auxinput24_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput24_begin_d
SUBROUTINE nl_get_auxinput24_begin_h ( id_id , auxinput24_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput24_begin_h
  INTEGER id_id
  auxinput24_begin_h = model_config_rec%auxinput24_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput24_begin_h
SUBROUTINE nl_get_auxinput24_begin_m ( id_id , auxinput24_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput24_begin_m
  INTEGER id_id
  auxinput24_begin_m = model_config_rec%auxinput24_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput24_begin_m
SUBROUTINE nl_get_auxinput24_begin_s ( id_id , auxinput24_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput24_begin_s
  INTEGER id_id
  auxinput24_begin_s = model_config_rec%auxinput24_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput24_begin_s
SUBROUTINE nl_get_auxinput24_begin ( id_id , auxinput24_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput24_begin
  INTEGER id_id
  auxinput24_begin = model_config_rec%auxinput24_begin(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput24_begin
SUBROUTINE nl_get_auxinput24_end_y ( id_id , auxinput24_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput24_end_y
  INTEGER id_id
  auxinput24_end_y = model_config_rec%auxinput24_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput24_end_y
SUBROUTINE nl_get_auxinput24_end_d ( id_id , auxinput24_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput24_end_d
  INTEGER id_id
  auxinput24_end_d = model_config_rec%auxinput24_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput24_end_d
SUBROUTINE nl_get_auxinput24_end_h ( id_id , auxinput24_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput24_end_h
  INTEGER id_id
  auxinput24_end_h = model_config_rec%auxinput24_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput24_end_h
SUBROUTINE nl_get_auxinput24_end_m ( id_id , auxinput24_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput24_end_m
  INTEGER id_id
  auxinput24_end_m = model_config_rec%auxinput24_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput24_end_m
SUBROUTINE nl_get_auxinput24_end_s ( id_id , auxinput24_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput24_end_s
  INTEGER id_id
  auxinput24_end_s = model_config_rec%auxinput24_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput24_end_s
SUBROUTINE nl_get_auxinput24_end ( id_id , auxinput24_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: auxinput24_end
  INTEGER id_id
  auxinput24_end = model_config_rec%auxinput24_end(id_id)
  RETURN
END SUBROUTINE nl_get_auxinput24_end
SUBROUTINE nl_get_io_form_auxinput24 ( id_id , io_form_auxinput24 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_auxinput24
  INTEGER id_id
  io_form_auxinput24 = model_config_rec%io_form_auxinput24
  RETURN
END SUBROUTINE nl_get_io_form_auxinput24
SUBROUTINE nl_get_frames_per_auxinput24 ( id_id , frames_per_auxinput24 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_auxinput24
  INTEGER id_id
  frames_per_auxinput24 = model_config_rec%frames_per_auxinput24(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_auxinput24
SUBROUTINE nl_get_history_interval ( id_id , history_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_interval
  INTEGER id_id
  history_interval = model_config_rec%history_interval(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval
SUBROUTINE nl_get_frames_per_outfile ( id_id , frames_per_outfile )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: frames_per_outfile
  INTEGER id_id
  frames_per_outfile = model_config_rec%frames_per_outfile(id_id)
  RETURN
END SUBROUTINE nl_get_frames_per_outfile
#endif
#if (NNN == 6)
SUBROUTINE nl_get_restart ( id_id , restart )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: restart
  INTEGER id_id
  restart = model_config_rec%restart
  RETURN
END SUBROUTINE nl_get_restart
SUBROUTINE nl_get_restart_interval ( id_id , restart_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_interval
  INTEGER id_id
  restart_interval = model_config_rec%restart_interval
  RETURN
END SUBROUTINE nl_get_restart_interval
SUBROUTINE nl_get_io_form_input ( id_id , io_form_input )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_input
  INTEGER id_id
  io_form_input = model_config_rec%io_form_input
  RETURN
END SUBROUTINE nl_get_io_form_input
SUBROUTINE nl_get_io_form_history ( id_id , io_form_history )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_history
  INTEGER id_id
  io_form_history = model_config_rec%io_form_history
  RETURN
END SUBROUTINE nl_get_io_form_history
SUBROUTINE nl_get_io_form_restart ( id_id , io_form_restart )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_restart
  INTEGER id_id
  io_form_restart = model_config_rec%io_form_restart
  RETURN
END SUBROUTINE nl_get_io_form_restart
SUBROUTINE nl_get_io_form_boundary ( id_id , io_form_boundary )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: io_form_boundary
  INTEGER id_id
  io_form_boundary = model_config_rec%io_form_boundary
  RETURN
END SUBROUTINE nl_get_io_form_boundary
SUBROUTINE nl_get_debug_level ( id_id , debug_level )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: debug_level
  INTEGER id_id
  debug_level = model_config_rec%debug_level
  RETURN
END SUBROUTINE nl_get_debug_level
SUBROUTINE nl_get_self_test_domain ( id_id , self_test_domain )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: self_test_domain
  INTEGER id_id
  self_test_domain = model_config_rec%self_test_domain
  RETURN
END SUBROUTINE nl_get_self_test_domain
SUBROUTINE nl_get_history_outname ( id_id , history_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: history_outname
  INTEGER id_id
  history_outname = trim(model_config_rec%history_outname)
  RETURN
END SUBROUTINE nl_get_history_outname
SUBROUTINE nl_get_history_inname ( id_id , history_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: history_inname
  INTEGER id_id
  history_inname = trim(model_config_rec%history_inname)
  RETURN
END SUBROUTINE nl_get_history_inname
SUBROUTINE nl_get_use_netcdf_classic ( id_id , use_netcdf_classic )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: use_netcdf_classic
  INTEGER id_id
  use_netcdf_classic = model_config_rec%use_netcdf_classic
  RETURN
END SUBROUTINE nl_get_use_netcdf_classic
SUBROUTINE nl_get_history_interval_d ( id_id , history_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_interval_d
  INTEGER id_id
  history_interval_d = model_config_rec%history_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval_d
SUBROUTINE nl_get_history_interval_h ( id_id , history_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_interval_h
  INTEGER id_id
  history_interval_h = model_config_rec%history_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval_h
SUBROUTINE nl_get_history_interval_m ( id_id , history_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_interval_m
  INTEGER id_id
  history_interval_m = model_config_rec%history_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval_m
SUBROUTINE nl_get_history_interval_s ( id_id , history_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_interval_s
  INTEGER id_id
  history_interval_s = model_config_rec%history_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_history_interval_s
SUBROUTINE nl_get_inputout_interval_d ( id_id , inputout_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_interval_d
  INTEGER id_id
  inputout_interval_d = model_config_rec%inputout_interval_d(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval_d
SUBROUTINE nl_get_inputout_interval_h ( id_id , inputout_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_interval_h
  INTEGER id_id
  inputout_interval_h = model_config_rec%inputout_interval_h(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval_h
SUBROUTINE nl_get_inputout_interval_m ( id_id , inputout_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_interval_m
  INTEGER id_id
  inputout_interval_m = model_config_rec%inputout_interval_m(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval_m
SUBROUTINE nl_get_inputout_interval_s ( id_id , inputout_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_interval_s
  INTEGER id_id
  inputout_interval_s = model_config_rec%inputout_interval_s(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval_s
SUBROUTINE nl_get_inputout_interval ( id_id , inputout_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_interval
  INTEGER id_id
  inputout_interval = model_config_rec%inputout_interval(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_interval
SUBROUTINE nl_get_restart_interval_d ( id_id , restart_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_interval_d
  INTEGER id_id
  restart_interval_d = model_config_rec%restart_interval_d
  RETURN
END SUBROUTINE nl_get_restart_interval_d
SUBROUTINE nl_get_restart_interval_h ( id_id , restart_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_interval_h
  INTEGER id_id
  restart_interval_h = model_config_rec%restart_interval_h
  RETURN
END SUBROUTINE nl_get_restart_interval_h
SUBROUTINE nl_get_restart_interval_m ( id_id , restart_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_interval_m
  INTEGER id_id
  restart_interval_m = model_config_rec%restart_interval_m
  RETURN
END SUBROUTINE nl_get_restart_interval_m
SUBROUTINE nl_get_restart_interval_s ( id_id , restart_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_interval_s
  INTEGER id_id
  restart_interval_s = model_config_rec%restart_interval_s
  RETURN
END SUBROUTINE nl_get_restart_interval_s
SUBROUTINE nl_get_history_begin_y ( id_id , history_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_begin_y
  INTEGER id_id
  history_begin_y = model_config_rec%history_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_y
SUBROUTINE nl_get_history_begin_d ( id_id , history_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_begin_d
  INTEGER id_id
  history_begin_d = model_config_rec%history_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_d
SUBROUTINE nl_get_history_begin_h ( id_id , history_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_begin_h
  INTEGER id_id
  history_begin_h = model_config_rec%history_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_h
SUBROUTINE nl_get_history_begin_m ( id_id , history_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_begin_m
  INTEGER id_id
  history_begin_m = model_config_rec%history_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_m
SUBROUTINE nl_get_history_begin_s ( id_id , history_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_begin_s
  INTEGER id_id
  history_begin_s = model_config_rec%history_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin_s
SUBROUTINE nl_get_history_begin ( id_id , history_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_begin
  INTEGER id_id
  history_begin = model_config_rec%history_begin(id_id)
  RETURN
END SUBROUTINE nl_get_history_begin
SUBROUTINE nl_get_inputout_begin_y ( id_id , inputout_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_begin_y
  INTEGER id_id
  inputout_begin_y = model_config_rec%inputout_begin_y(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_y
SUBROUTINE nl_get_inputout_begin_d ( id_id , inputout_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_begin_d
  INTEGER id_id
  inputout_begin_d = model_config_rec%inputout_begin_d(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_d
SUBROUTINE nl_get_inputout_begin_h ( id_id , inputout_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_begin_h
  INTEGER id_id
  inputout_begin_h = model_config_rec%inputout_begin_h(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_h
SUBROUTINE nl_get_inputout_begin_m ( id_id , inputout_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_begin_m
  INTEGER id_id
  inputout_begin_m = model_config_rec%inputout_begin_m(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_m
SUBROUTINE nl_get_inputout_begin_s ( id_id , inputout_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_begin_s
  INTEGER id_id
  inputout_begin_s = model_config_rec%inputout_begin_s(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_begin_s
SUBROUTINE nl_get_restart_begin_y ( id_id , restart_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_begin_y
  INTEGER id_id
  restart_begin_y = model_config_rec%restart_begin_y
  RETURN
END SUBROUTINE nl_get_restart_begin_y
SUBROUTINE nl_get_restart_begin_d ( id_id , restart_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_begin_d
  INTEGER id_id
  restart_begin_d = model_config_rec%restart_begin_d
  RETURN
END SUBROUTINE nl_get_restart_begin_d
SUBROUTINE nl_get_restart_begin_h ( id_id , restart_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_begin_h
  INTEGER id_id
  restart_begin_h = model_config_rec%restart_begin_h
  RETURN
END SUBROUTINE nl_get_restart_begin_h
SUBROUTINE nl_get_restart_begin_m ( id_id , restart_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_begin_m
  INTEGER id_id
  restart_begin_m = model_config_rec%restart_begin_m
  RETURN
END SUBROUTINE nl_get_restart_begin_m
SUBROUTINE nl_get_restart_begin_s ( id_id , restart_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_begin_s
  INTEGER id_id
  restart_begin_s = model_config_rec%restart_begin_s
  RETURN
END SUBROUTINE nl_get_restart_begin_s
SUBROUTINE nl_get_restart_begin ( id_id , restart_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: restart_begin
  INTEGER id_id
  restart_begin = model_config_rec%restart_begin
  RETURN
END SUBROUTINE nl_get_restart_begin
SUBROUTINE nl_get_history_end_y ( id_id , history_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_end_y
  INTEGER id_id
  history_end_y = model_config_rec%history_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_y
SUBROUTINE nl_get_history_end_d ( id_id , history_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_end_d
  INTEGER id_id
  history_end_d = model_config_rec%history_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_d
SUBROUTINE nl_get_history_end_h ( id_id , history_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_end_h
  INTEGER id_id
  history_end_h = model_config_rec%history_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_h
SUBROUTINE nl_get_history_end_m ( id_id , history_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_end_m
  INTEGER id_id
  history_end_m = model_config_rec%history_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_m
SUBROUTINE nl_get_history_end_s ( id_id , history_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_end_s
  INTEGER id_id
  history_end_s = model_config_rec%history_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_history_end_s
SUBROUTINE nl_get_history_end ( id_id , history_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: history_end
  INTEGER id_id
  history_end = model_config_rec%history_end(id_id)
  RETURN
END SUBROUTINE nl_get_history_end
SUBROUTINE nl_get_inputout_end_y ( id_id , inputout_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_end_y
  INTEGER id_id
  inputout_end_y = model_config_rec%inputout_end_y(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_y
SUBROUTINE nl_get_inputout_end_d ( id_id , inputout_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_end_d
  INTEGER id_id
  inputout_end_d = model_config_rec%inputout_end_d(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_d
SUBROUTINE nl_get_inputout_end_h ( id_id , inputout_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_end_h
  INTEGER id_id
  inputout_end_h = model_config_rec%inputout_end_h(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_h
SUBROUTINE nl_get_inputout_end_m ( id_id , inputout_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_end_m
  INTEGER id_id
  inputout_end_m = model_config_rec%inputout_end_m(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_m
SUBROUTINE nl_get_inputout_end_s ( id_id , inputout_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: inputout_end_s
  INTEGER id_id
  inputout_end_s = model_config_rec%inputout_end_s(id_id)
  RETURN
END SUBROUTINE nl_get_inputout_end_s
SUBROUTINE nl_get_simulation_start_year ( id_id , simulation_start_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: simulation_start_year
  INTEGER id_id
  simulation_start_year = model_config_rec%simulation_start_year
  RETURN
END SUBROUTINE nl_get_simulation_start_year
SUBROUTINE nl_get_simulation_start_month ( id_id , simulation_start_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: simulation_start_month
  INTEGER id_id
  simulation_start_month = model_config_rec%simulation_start_month
  RETURN
END SUBROUTINE nl_get_simulation_start_month
SUBROUTINE nl_get_simulation_start_day ( id_id , simulation_start_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: simulation_start_day
  INTEGER id_id
  simulation_start_day = model_config_rec%simulation_start_day
  RETURN
END SUBROUTINE nl_get_simulation_start_day
SUBROUTINE nl_get_simulation_start_hour ( id_id , simulation_start_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: simulation_start_hour
  INTEGER id_id
  simulation_start_hour = model_config_rec%simulation_start_hour
  RETURN
END SUBROUTINE nl_get_simulation_start_hour
SUBROUTINE nl_get_simulation_start_minute ( id_id , simulation_start_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: simulation_start_minute
  INTEGER id_id
  simulation_start_minute = model_config_rec%simulation_start_minute
  RETURN
END SUBROUTINE nl_get_simulation_start_minute
SUBROUTINE nl_get_simulation_start_second ( id_id , simulation_start_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: simulation_start_second
  INTEGER id_id
  simulation_start_second = model_config_rec%simulation_start_second
  RETURN
END SUBROUTINE nl_get_simulation_start_second
SUBROUTINE nl_get_reset_simulation_start ( id_id , reset_simulation_start )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: reset_simulation_start
  INTEGER id_id
  reset_simulation_start = model_config_rec%reset_simulation_start
  RETURN
END SUBROUTINE nl_get_reset_simulation_start
SUBROUTINE nl_get_sr_x ( id_id , sr_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sr_x
  INTEGER id_id
  sr_x = model_config_rec%sr_x(id_id)
  RETURN
END SUBROUTINE nl_get_sr_x
SUBROUTINE nl_get_sr_y ( id_id , sr_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sr_y
  INTEGER id_id
  sr_y = model_config_rec%sr_y(id_id)
  RETURN
END SUBROUTINE nl_get_sr_y
SUBROUTINE nl_get_iofields_filename ( id_id , iofields_filename )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: iofields_filename
  INTEGER id_id
  iofields_filename = model_config_rec%iofields_filename(id_id)
  RETURN
END SUBROUTINE nl_get_iofields_filename
SUBROUTINE nl_get_ignore_iofields_warning ( id_id , ignore_iofields_warning )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: ignore_iofields_warning
  INTEGER id_id
  ignore_iofields_warning = model_config_rec%ignore_iofields_warning
  RETURN
END SUBROUTINE nl_get_ignore_iofields_warning
SUBROUTINE nl_get_ncd_nofill ( id_id , ncd_nofill )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: ncd_nofill
  INTEGER id_id
  ncd_nofill = model_config_rec%ncd_nofill
  RETURN
END SUBROUTINE nl_get_ncd_nofill
SUBROUTINE nl_get_julyr ( id_id , julyr )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: julyr
  INTEGER id_id
  julyr = model_config_rec%julyr(id_id)
  RETURN
END SUBROUTINE nl_get_julyr
SUBROUTINE nl_get_julday ( id_id , julday )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: julday
  INTEGER id_id
  julday = model_config_rec%julday(id_id)
  RETURN
END SUBROUTINE nl_get_julday
SUBROUTINE nl_get_gmt ( id_id , gmt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: gmt
  INTEGER id_id
  gmt = model_config_rec%gmt(id_id)
  RETURN
END SUBROUTINE nl_get_gmt
SUBROUTINE nl_get_high_freq_outname ( id_id , high_freq_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: high_freq_outname
  INTEGER id_id
  high_freq_outname = trim(model_config_rec%high_freq_outname)
  RETURN
END SUBROUTINE nl_get_high_freq_outname
SUBROUTINE nl_get_partial_atcf_outname ( id_id , partial_atcf_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: partial_atcf_outname
  INTEGER id_id
  partial_atcf_outname = trim(model_config_rec%partial_atcf_outname)
  RETURN
END SUBROUTINE nl_get_partial_atcf_outname
SUBROUTINE nl_get_input_inname ( id_id , input_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: input_inname
  INTEGER id_id
  input_inname = trim(model_config_rec%input_inname)
  RETURN
END SUBROUTINE nl_get_input_inname
SUBROUTINE nl_get_input_outname ( id_id , input_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: input_outname
  INTEGER id_id
  input_outname = trim(model_config_rec%input_outname)
  RETURN
END SUBROUTINE nl_get_input_outname
SUBROUTINE nl_get_bdy_inname ( id_id , bdy_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: bdy_inname
  INTEGER id_id
  bdy_inname = trim(model_config_rec%bdy_inname)
  RETURN
END SUBROUTINE nl_get_bdy_inname
SUBROUTINE nl_get_bdy_outname ( id_id , bdy_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: bdy_outname
  INTEGER id_id
  bdy_outname = trim(model_config_rec%bdy_outname)
  RETURN
END SUBROUTINE nl_get_bdy_outname
SUBROUTINE nl_get_rst_inname ( id_id , rst_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: rst_inname
  INTEGER id_id
  rst_inname = trim(model_config_rec%rst_inname)
  RETURN
END SUBROUTINE nl_get_rst_inname
SUBROUTINE nl_get_rst_outname ( id_id , rst_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: rst_outname
  INTEGER id_id
  rst_outname = trim(model_config_rec%rst_outname)
  RETURN
END SUBROUTINE nl_get_rst_outname
SUBROUTINE nl_get_anl_outname ( id_id , anl_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: anl_outname
  INTEGER id_id
  anl_outname = model_config_rec%anl_outname(id_id)
  RETURN
END SUBROUTINE nl_get_anl_outname
SUBROUTINE nl_get_write_input ( id_id , write_input )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: write_input
  INTEGER id_id
  write_input = model_config_rec%write_input
  RETURN
END SUBROUTINE nl_get_write_input
SUBROUTINE nl_get_write_restart_at_0h ( id_id , write_restart_at_0h )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: write_restart_at_0h
  INTEGER id_id
  write_restart_at_0h = model_config_rec%write_restart_at_0h
  RETURN
END SUBROUTINE nl_get_write_restart_at_0h
SUBROUTINE nl_get_write_hist_at_0h_rst ( id_id , write_hist_at_0h_rst )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: write_hist_at_0h_rst
  INTEGER id_id
  write_hist_at_0h_rst = model_config_rec%write_hist_at_0h_rst
  RETURN
END SUBROUTINE nl_get_write_hist_at_0h_rst
SUBROUTINE nl_get_adjust_output_times ( id_id , adjust_output_times )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: adjust_output_times
  INTEGER id_id
  adjust_output_times = model_config_rec%adjust_output_times
  RETURN
END SUBROUTINE nl_get_adjust_output_times
SUBROUTINE nl_get_adjust_input_times ( id_id , adjust_input_times )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: adjust_input_times
  INTEGER id_id
  adjust_input_times = model_config_rec%adjust_input_times
  RETURN
END SUBROUTINE nl_get_adjust_input_times
SUBROUTINE nl_get_tstart ( id_id , tstart )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: tstart
  INTEGER id_id
  tstart = model_config_rec%tstart(id_id)
  RETURN
END SUBROUTINE nl_get_tstart
SUBROUTINE nl_get_nocolons ( id_id , nocolons )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: nocolons
  INTEGER id_id
  nocolons = model_config_rec%nocolons
  RETURN
END SUBROUTINE nl_get_nocolons
SUBROUTINE nl_get_cycling ( id_id , cycling )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: cycling
  INTEGER id_id
  cycling = model_config_rec%cycling
  RETURN
END SUBROUTINE nl_get_cycling
SUBROUTINE nl_get_output_ready_flag ( id_id , output_ready_flag )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: output_ready_flag
  INTEGER id_id
  output_ready_flag = model_config_rec%output_ready_flag
  RETURN
END SUBROUTINE nl_get_output_ready_flag
SUBROUTINE nl_get_dfi_opt ( id_id , dfi_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_opt
  INTEGER id_id
  dfi_opt = model_config_rec%dfi_opt
  RETURN
END SUBROUTINE nl_get_dfi_opt
SUBROUTINE nl_get_dfi_savehydmeteors ( id_id , dfi_savehydmeteors )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_savehydmeteors
  INTEGER id_id
  dfi_savehydmeteors = model_config_rec%dfi_savehydmeteors
  RETURN
END SUBROUTINE nl_get_dfi_savehydmeteors
SUBROUTINE nl_get_dfi_nfilter ( id_id , dfi_nfilter )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_nfilter
  INTEGER id_id
  dfi_nfilter = model_config_rec%dfi_nfilter
  RETURN
END SUBROUTINE nl_get_dfi_nfilter
SUBROUTINE nl_get_dfi_write_filtered_input ( id_id , dfi_write_filtered_input )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: dfi_write_filtered_input
  INTEGER id_id
  dfi_write_filtered_input = model_config_rec%dfi_write_filtered_input
  RETURN
END SUBROUTINE nl_get_dfi_write_filtered_input
SUBROUTINE nl_get_dfi_write_dfi_history ( id_id , dfi_write_dfi_history )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: dfi_write_dfi_history
  INTEGER id_id
  dfi_write_dfi_history = model_config_rec%dfi_write_dfi_history
  RETURN
END SUBROUTINE nl_get_dfi_write_dfi_history
SUBROUTINE nl_get_dfi_cutoff_seconds ( id_id , dfi_cutoff_seconds )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_cutoff_seconds
  INTEGER id_id
  dfi_cutoff_seconds = model_config_rec%dfi_cutoff_seconds
  RETURN
END SUBROUTINE nl_get_dfi_cutoff_seconds
SUBROUTINE nl_get_dfi_time_dim ( id_id , dfi_time_dim )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_time_dim
  INTEGER id_id
  dfi_time_dim = model_config_rec%dfi_time_dim
  RETURN
END SUBROUTINE nl_get_dfi_time_dim
SUBROUTINE nl_get_dfi_fwdstop_year ( id_id , dfi_fwdstop_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_fwdstop_year
  INTEGER id_id
  dfi_fwdstop_year = model_config_rec%dfi_fwdstop_year
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_year
SUBROUTINE nl_get_dfi_fwdstop_month ( id_id , dfi_fwdstop_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_fwdstop_month
  INTEGER id_id
  dfi_fwdstop_month = model_config_rec%dfi_fwdstop_month
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_month
SUBROUTINE nl_get_dfi_fwdstop_day ( id_id , dfi_fwdstop_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_fwdstop_day
  INTEGER id_id
  dfi_fwdstop_day = model_config_rec%dfi_fwdstop_day
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_day
SUBROUTINE nl_get_dfi_fwdstop_hour ( id_id , dfi_fwdstop_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_fwdstop_hour
  INTEGER id_id
  dfi_fwdstop_hour = model_config_rec%dfi_fwdstop_hour
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_hour
SUBROUTINE nl_get_dfi_fwdstop_minute ( id_id , dfi_fwdstop_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_fwdstop_minute
  INTEGER id_id
  dfi_fwdstop_minute = model_config_rec%dfi_fwdstop_minute
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_minute
SUBROUTINE nl_get_dfi_fwdstop_second ( id_id , dfi_fwdstop_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_fwdstop_second
  INTEGER id_id
  dfi_fwdstop_second = model_config_rec%dfi_fwdstop_second
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_second
SUBROUTINE nl_get_dfi_bckstop_year ( id_id , dfi_bckstop_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_bckstop_year
  INTEGER id_id
  dfi_bckstop_year = model_config_rec%dfi_bckstop_year
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_year
SUBROUTINE nl_get_dfi_bckstop_month ( id_id , dfi_bckstop_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_bckstop_month
  INTEGER id_id
  dfi_bckstop_month = model_config_rec%dfi_bckstop_month
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_month
SUBROUTINE nl_get_dfi_bckstop_day ( id_id , dfi_bckstop_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_bckstop_day
  INTEGER id_id
  dfi_bckstop_day = model_config_rec%dfi_bckstop_day
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_day
SUBROUTINE nl_get_dfi_bckstop_hour ( id_id , dfi_bckstop_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_bckstop_hour
  INTEGER id_id
  dfi_bckstop_hour = model_config_rec%dfi_bckstop_hour
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_hour
SUBROUTINE nl_get_dfi_bckstop_minute ( id_id , dfi_bckstop_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_bckstop_minute
  INTEGER id_id
  dfi_bckstop_minute = model_config_rec%dfi_bckstop_minute
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_minute
SUBROUTINE nl_get_dfi_bckstop_second ( id_id , dfi_bckstop_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_bckstop_second
  INTEGER id_id
  dfi_bckstop_second = model_config_rec%dfi_bckstop_second
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_second
SUBROUTINE nl_get_time_step ( id_id , time_step )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: time_step
  INTEGER id_id
  time_step = model_config_rec%time_step
  RETURN
END SUBROUTINE nl_get_time_step
SUBROUTINE nl_get_time_step_fract_num ( id_id , time_step_fract_num )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: time_step_fract_num
  INTEGER id_id
  time_step_fract_num = model_config_rec%time_step_fract_num
  RETURN
END SUBROUTINE nl_get_time_step_fract_num
SUBROUTINE nl_get_time_step_fract_den ( id_id , time_step_fract_den )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: time_step_fract_den
  INTEGER id_id
  time_step_fract_den = model_config_rec%time_step_fract_den
  RETURN
END SUBROUTINE nl_get_time_step_fract_den
SUBROUTINE nl_get_time_step_dfi ( id_id , time_step_dfi )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: time_step_dfi
  INTEGER id_id
  time_step_dfi = model_config_rec%time_step_dfi
  RETURN
END SUBROUTINE nl_get_time_step_dfi
SUBROUTINE nl_get_max_dom ( id_id , max_dom )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: max_dom
  INTEGER id_id
  max_dom = model_config_rec%max_dom
  RETURN
END SUBROUTINE nl_get_max_dom
SUBROUTINE nl_get_s_we ( id_id , s_we )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: s_we
  INTEGER id_id
  s_we = model_config_rec%s_we(id_id)
  RETURN
END SUBROUTINE nl_get_s_we
SUBROUTINE nl_get_e_we ( id_id , e_we )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: e_we
  INTEGER id_id
  e_we = model_config_rec%e_we(id_id)
  RETURN
END SUBROUTINE nl_get_e_we
SUBROUTINE nl_get_s_sn ( id_id , s_sn )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: s_sn
  INTEGER id_id
  s_sn = model_config_rec%s_sn(id_id)
  RETURN
END SUBROUTINE nl_get_s_sn
SUBROUTINE nl_get_e_sn ( id_id , e_sn )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: e_sn
  INTEGER id_id
  e_sn = model_config_rec%e_sn(id_id)
  RETURN
END SUBROUTINE nl_get_e_sn
SUBROUTINE nl_get_s_vert ( id_id , s_vert )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: s_vert
  INTEGER id_id
  s_vert = model_config_rec%s_vert(id_id)
  RETURN
END SUBROUTINE nl_get_s_vert
SUBROUTINE nl_get_e_vert ( id_id , e_vert )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: e_vert
  INTEGER id_id
  e_vert = model_config_rec%e_vert(id_id)
  RETURN
END SUBROUTINE nl_get_e_vert
SUBROUTINE nl_get_num_metgrid_soil_levels ( id_id , num_metgrid_soil_levels )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_metgrid_soil_levels
  INTEGER id_id
  num_metgrid_soil_levels = model_config_rec%num_metgrid_soil_levels
  RETURN
END SUBROUTINE nl_get_num_metgrid_soil_levels
SUBROUTINE nl_get_dx ( id_id , dx )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: dx
  INTEGER id_id
  dx = model_config_rec%dx(id_id)
  RETURN
END SUBROUTINE nl_get_dx
SUBROUTINE nl_get_dy ( id_id , dy )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: dy
  INTEGER id_id
  dy = model_config_rec%dy(id_id)
  RETURN
END SUBROUTINE nl_get_dy
SUBROUTINE nl_get_grid_id ( id_id , grid_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: grid_id
  INTEGER id_id
  grid_id = model_config_rec%grid_id(id_id)
  RETURN
END SUBROUTINE nl_get_grid_id
SUBROUTINE nl_get_grid_allowed ( id_id , grid_allowed )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: grid_allowed
  INTEGER id_id
  grid_allowed = model_config_rec%grid_allowed(id_id)
  RETURN
END SUBROUTINE nl_get_grid_allowed
SUBROUTINE nl_get_parent_id ( id_id , parent_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: parent_id
  INTEGER id_id
  parent_id = model_config_rec%parent_id(id_id)
  RETURN
END SUBROUTINE nl_get_parent_id
SUBROUTINE nl_get_i_parent_start ( id_id , i_parent_start )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: i_parent_start
  INTEGER id_id
  i_parent_start = model_config_rec%i_parent_start(id_id)
  RETURN
END SUBROUTINE nl_get_i_parent_start
SUBROUTINE nl_get_j_parent_start ( id_id , j_parent_start )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: j_parent_start
  INTEGER id_id
  j_parent_start = model_config_rec%j_parent_start(id_id)
  RETURN
END SUBROUTINE nl_get_j_parent_start
SUBROUTINE nl_get_parent_grid_ratio ( id_id , parent_grid_ratio )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: parent_grid_ratio
  INTEGER id_id
  parent_grid_ratio = model_config_rec%parent_grid_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_parent_grid_ratio
SUBROUTINE nl_get_parent_time_step_ratio ( id_id , parent_time_step_ratio )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: parent_time_step_ratio
  INTEGER id_id
  parent_time_step_ratio = model_config_rec%parent_time_step_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_parent_time_step_ratio
SUBROUTINE nl_get_feedback ( id_id , feedback )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: feedback
  INTEGER id_id
  feedback = model_config_rec%feedback
  RETURN
END SUBROUTINE nl_get_feedback
SUBROUTINE nl_get_smooth_option ( id_id , smooth_option )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: smooth_option
  INTEGER id_id
  smooth_option = model_config_rec%smooth_option
  RETURN
END SUBROUTINE nl_get_smooth_option
SUBROUTINE nl_get_ztop ( id_id , ztop )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: ztop
  INTEGER id_id
  ztop = model_config_rec%ztop(id_id)
  RETURN
END SUBROUTINE nl_get_ztop
SUBROUTINE nl_get_moad_grid_ratio ( id_id , moad_grid_ratio )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: moad_grid_ratio
  INTEGER id_id
  moad_grid_ratio = model_config_rec%moad_grid_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_moad_grid_ratio
SUBROUTINE nl_get_moad_time_step_ratio ( id_id , moad_time_step_ratio )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: moad_time_step_ratio
  INTEGER id_id
  moad_time_step_ratio = model_config_rec%moad_time_step_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_moad_time_step_ratio
SUBROUTINE nl_get_shw ( id_id , shw )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: shw
  INTEGER id_id
  shw = model_config_rec%shw(id_id)
  RETURN
END SUBROUTINE nl_get_shw
SUBROUTINE nl_get_tile_sz_x ( id_id , tile_sz_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: tile_sz_x
  INTEGER id_id
  tile_sz_x = model_config_rec%tile_sz_x
  RETURN
END SUBROUTINE nl_get_tile_sz_x
SUBROUTINE nl_get_tile_sz_y ( id_id , tile_sz_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: tile_sz_y
  INTEGER id_id
  tile_sz_y = model_config_rec%tile_sz_y
  RETURN
END SUBROUTINE nl_get_tile_sz_y
SUBROUTINE nl_get_numtiles ( id_id , numtiles )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: numtiles
  INTEGER id_id
  numtiles = model_config_rec%numtiles
  RETURN
END SUBROUTINE nl_get_numtiles
SUBROUTINE nl_get_numtiles_inc ( id_id , numtiles_inc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: numtiles_inc
  INTEGER id_id
  numtiles_inc = model_config_rec%numtiles_inc
  RETURN
END SUBROUTINE nl_get_numtiles_inc
SUBROUTINE nl_get_numtiles_x ( id_id , numtiles_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: numtiles_x
  INTEGER id_id
  numtiles_x = model_config_rec%numtiles_x
  RETURN
END SUBROUTINE nl_get_numtiles_x
SUBROUTINE nl_get_numtiles_y ( id_id , numtiles_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: numtiles_y
  INTEGER id_id
  numtiles_y = model_config_rec%numtiles_y
  RETURN
END SUBROUTINE nl_get_numtiles_y
SUBROUTINE nl_get_tile_strategy ( id_id , tile_strategy )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: tile_strategy
  INTEGER id_id
  tile_strategy = model_config_rec%tile_strategy
  RETURN
END SUBROUTINE nl_get_tile_strategy
SUBROUTINE nl_get_nproc_x ( id_id , nproc_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nproc_x
  INTEGER id_id
  nproc_x = model_config_rec%nproc_x
  RETURN
END SUBROUTINE nl_get_nproc_x
SUBROUTINE nl_get_nproc_y ( id_id , nproc_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nproc_y
  INTEGER id_id
  nproc_y = model_config_rec%nproc_y
  RETURN
END SUBROUTINE nl_get_nproc_y
SUBROUTINE nl_get_irand ( id_id , irand )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: irand
  INTEGER id_id
  irand = model_config_rec%irand
  RETURN
END SUBROUTINE nl_get_irand
SUBROUTINE nl_get_dt ( id_id , dt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: dt
  INTEGER id_id
  dt = model_config_rec%dt(id_id)
  RETURN
END SUBROUTINE nl_get_dt
SUBROUTINE nl_get_ts_buf_size ( id_id , ts_buf_size )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ts_buf_size
  INTEGER id_id
  ts_buf_size = model_config_rec%ts_buf_size
  RETURN
END SUBROUTINE nl_get_ts_buf_size
SUBROUTINE nl_get_max_ts_locs ( id_id , max_ts_locs )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: max_ts_locs
  INTEGER id_id
  max_ts_locs = model_config_rec%max_ts_locs
  RETURN
END SUBROUTINE nl_get_max_ts_locs
SUBROUTINE nl_get_num_moves ( id_id , num_moves )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_moves
  INTEGER id_id
  num_moves = model_config_rec%num_moves
  RETURN
END SUBROUTINE nl_get_num_moves
SUBROUTINE nl_get_vortex_interval ( id_id , vortex_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: vortex_interval
  INTEGER id_id
  vortex_interval = model_config_rec%vortex_interval(id_id)
  RETURN
END SUBROUTINE nl_get_vortex_interval
SUBROUTINE nl_get_corral_dist ( id_id , corral_dist )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: corral_dist
  INTEGER id_id
  corral_dist = model_config_rec%corral_dist(id_id)
  RETURN
END SUBROUTINE nl_get_corral_dist
SUBROUTINE nl_get_move_id ( id_id , move_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: move_id
  INTEGER id_id
  move_id = model_config_rec%move_id(id_id)
  RETURN
END SUBROUTINE nl_get_move_id
SUBROUTINE nl_get_move_interval ( id_id , move_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: move_interval
  INTEGER id_id
  move_interval = model_config_rec%move_interval(id_id)
  RETURN
END SUBROUTINE nl_get_move_interval
SUBROUTINE nl_get_move_cd_x ( id_id , move_cd_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: move_cd_x
  INTEGER id_id
  move_cd_x = model_config_rec%move_cd_x(id_id)
  RETURN
END SUBROUTINE nl_get_move_cd_x
SUBROUTINE nl_get_move_cd_y ( id_id , move_cd_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: move_cd_y
  INTEGER id_id
  move_cd_y = model_config_rec%move_cd_y(id_id)
  RETURN
END SUBROUTINE nl_get_move_cd_y
SUBROUTINE nl_get_swap_x ( id_id , swap_x )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: swap_x
  INTEGER id_id
  swap_x = model_config_rec%swap_x(id_id)
  RETURN
END SUBROUTINE nl_get_swap_x
SUBROUTINE nl_get_swap_y ( id_id , swap_y )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: swap_y
  INTEGER id_id
  swap_y = model_config_rec%swap_y(id_id)
  RETURN
END SUBROUTINE nl_get_swap_y
SUBROUTINE nl_get_cycle_x ( id_id , cycle_x )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: cycle_x
  INTEGER id_id
  cycle_x = model_config_rec%cycle_x(id_id)
  RETURN
END SUBROUTINE nl_get_cycle_x
SUBROUTINE nl_get_cycle_y ( id_id , cycle_y )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: cycle_y
  INTEGER id_id
  cycle_y = model_config_rec%cycle_y(id_id)
  RETURN
END SUBROUTINE nl_get_cycle_y
SUBROUTINE nl_get_reorder_mesh ( id_id , reorder_mesh )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: reorder_mesh
  INTEGER id_id
  reorder_mesh = model_config_rec%reorder_mesh
  RETURN
END SUBROUTINE nl_get_reorder_mesh
SUBROUTINE nl_get_perturb_input ( id_id , perturb_input )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: perturb_input
  INTEGER id_id
  perturb_input = model_config_rec%perturb_input
  RETURN
END SUBROUTINE nl_get_perturb_input
SUBROUTINE nl_get_eta_levels ( id_id , eta_levels )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: eta_levels
  INTEGER id_id
  eta_levels = model_config_rec%eta_levels(id_id)
  RETURN
END SUBROUTINE nl_get_eta_levels
SUBROUTINE nl_get_ptsgm ( id_id , ptsgm )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: ptsgm
  INTEGER id_id
  ptsgm = model_config_rec%ptsgm
  RETURN
END SUBROUTINE nl_get_ptsgm
SUBROUTINE nl_get_num_metgrid_levels ( id_id , num_metgrid_levels )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_metgrid_levels
  INTEGER id_id
  num_metgrid_levels = model_config_rec%num_metgrid_levels
  RETURN
END SUBROUTINE nl_get_num_metgrid_levels
SUBROUTINE nl_get_p_top_requested ( id_id , p_top_requested )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: p_top_requested
  INTEGER id_id
  p_top_requested = model_config_rec%p_top_requested
  RETURN
END SUBROUTINE nl_get_p_top_requested
SUBROUTINE nl_get_use_prep_hybrid ( id_id , use_prep_hybrid )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: use_prep_hybrid
  INTEGER id_id
  use_prep_hybrid = model_config_rec%use_prep_hybrid
  RETURN
END SUBROUTINE nl_get_use_prep_hybrid
SUBROUTINE nl_get_force_read_thompson ( id_id , force_read_thompson )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: force_read_thompson
  INTEGER id_id
  force_read_thompson = model_config_rec%force_read_thompson
  RETURN
END SUBROUTINE nl_get_force_read_thompson
SUBROUTINE nl_get_write_thompson_tables ( id_id , write_thompson_tables )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: write_thompson_tables
  INTEGER id_id
  write_thompson_tables = model_config_rec%write_thompson_tables
  RETURN
END SUBROUTINE nl_get_write_thompson_tables
SUBROUTINE nl_get_mp_physics ( id_id , mp_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: mp_physics
  INTEGER id_id
  mp_physics = model_config_rec%mp_physics(id_id)
  RETURN
END SUBROUTINE nl_get_mp_physics
SUBROUTINE nl_get_mommix ( id_id , mommix )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: mommix
  INTEGER id_id
  mommix = model_config_rec%mommix(id_id)
  RETURN
END SUBROUTINE nl_get_mommix
SUBROUTINE nl_get_disheat ( id_id , disheat )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: disheat
  INTEGER id_id
  disheat = model_config_rec%disheat(id_id)
  RETURN
END SUBROUTINE nl_get_disheat
SUBROUTINE nl_get_do_radar_ref ( id_id , do_radar_ref )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: do_radar_ref
  INTEGER id_id
  do_radar_ref = model_config_rec%do_radar_ref
  RETURN
END SUBROUTINE nl_get_do_radar_ref
SUBROUTINE nl_get_compute_radar_ref ( id_id , compute_radar_ref )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: compute_radar_ref
  INTEGER id_id
  compute_radar_ref = model_config_rec%compute_radar_ref
  RETURN
END SUBROUTINE nl_get_compute_radar_ref
SUBROUTINE nl_get_ra_lw_physics ( id_id , ra_lw_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ra_lw_physics
  INTEGER id_id
  ra_lw_physics = model_config_rec%ra_lw_physics(id_id)
  RETURN
END SUBROUTINE nl_get_ra_lw_physics
SUBROUTINE nl_get_ra_sw_physics ( id_id , ra_sw_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ra_sw_physics
  INTEGER id_id
  ra_sw_physics = model_config_rec%ra_sw_physics(id_id)
  RETURN
END SUBROUTINE nl_get_ra_sw_physics
SUBROUTINE nl_get_radt ( id_id , radt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: radt
  INTEGER id_id
  radt = model_config_rec%radt(id_id)
  RETURN
END SUBROUTINE nl_get_radt
SUBROUTINE nl_get_sf_sfclay_physics ( id_id , sf_sfclay_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sf_sfclay_physics
  INTEGER id_id
  sf_sfclay_physics = model_config_rec%sf_sfclay_physics(id_id)
  RETURN
END SUBROUTINE nl_get_sf_sfclay_physics
SUBROUTINE nl_get_sf_surface_physics ( id_id , sf_surface_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sf_surface_physics
  INTEGER id_id
  sf_surface_physics = model_config_rec%sf_surface_physics(id_id)
  RETURN
END SUBROUTINE nl_get_sf_surface_physics
SUBROUTINE nl_get_bl_pbl_physics ( id_id , bl_pbl_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: bl_pbl_physics
  INTEGER id_id
  bl_pbl_physics = model_config_rec%bl_pbl_physics(id_id)
  RETURN
END SUBROUTINE nl_get_bl_pbl_physics
SUBROUTINE nl_get_ysu_topdown_pblmix ( id_id , ysu_topdown_pblmix )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ysu_topdown_pblmix
  INTEGER id_id
  ysu_topdown_pblmix = model_config_rec%ysu_topdown_pblmix(id_id)
  RETURN
END SUBROUTINE nl_get_ysu_topdown_pblmix
SUBROUTINE nl_get_shinhong_tke_diag ( id_id , shinhong_tke_diag )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: shinhong_tke_diag
  INTEGER id_id
  shinhong_tke_diag = model_config_rec%shinhong_tke_diag(id_id)
  RETURN
END SUBROUTINE nl_get_shinhong_tke_diag
SUBROUTINE nl_get_windfarm_opt ( id_id , windfarm_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: windfarm_opt
  INTEGER id_id
  windfarm_opt = model_config_rec%windfarm_opt(id_id)
  RETURN
END SUBROUTINE nl_get_windfarm_opt
SUBROUTINE nl_get_windfarm_ij ( id_id , windfarm_ij )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: windfarm_ij
  INTEGER id_id
  windfarm_ij = model_config_rec%windfarm_ij
  RETURN
END SUBROUTINE nl_get_windfarm_ij
SUBROUTINE nl_get_mfshconv ( id_id , mfshconv )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: mfshconv
  INTEGER id_id
  mfshconv = model_config_rec%mfshconv(id_id)
  RETURN
END SUBROUTINE nl_get_mfshconv
SUBROUTINE nl_get_bldt ( id_id , bldt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: bldt
  INTEGER id_id
  bldt = model_config_rec%bldt(id_id)
  RETURN
END SUBROUTINE nl_get_bldt
SUBROUTINE nl_get_cu_physics ( id_id , cu_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: cu_physics
  INTEGER id_id
  cu_physics = model_config_rec%cu_physics(id_id)
  RETURN
END SUBROUTINE nl_get_cu_physics
SUBROUTINE nl_get_shcu_physics ( id_id , shcu_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: shcu_physics
  INTEGER id_id
  shcu_physics = model_config_rec%shcu_physics(id_id)
  RETURN
END SUBROUTINE nl_get_shcu_physics
SUBROUTINE nl_get_cu_diag ( id_id , cu_diag )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: cu_diag
  INTEGER id_id
  cu_diag = model_config_rec%cu_diag(id_id)
  RETURN
END SUBROUTINE nl_get_cu_diag
SUBROUTINE nl_get_gfs_alpha ( id_id , gfs_alpha )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: gfs_alpha
  INTEGER id_id
  gfs_alpha = model_config_rec%gfs_alpha(id_id)
  RETURN
END SUBROUTINE nl_get_gfs_alpha
SUBROUTINE nl_get_cudt ( id_id , cudt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: cudt
  INTEGER id_id
  cudt = model_config_rec%cudt(id_id)
  RETURN
END SUBROUTINE nl_get_cudt
SUBROUTINE nl_get_gsmdt ( id_id , gsmdt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: gsmdt
  INTEGER id_id
  gsmdt = model_config_rec%gsmdt(id_id)
  RETURN
END SUBROUTINE nl_get_gsmdt
SUBROUTINE nl_get_isfflx ( id_id , isfflx )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: isfflx
  INTEGER id_id
  isfflx = model_config_rec%isfflx
  RETURN
END SUBROUTINE nl_get_isfflx
SUBROUTINE nl_get_ideal_xland ( id_id , ideal_xland )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ideal_xland
  INTEGER id_id
  ideal_xland = model_config_rec%ideal_xland
  RETURN
END SUBROUTINE nl_get_ideal_xland
#endif
#if (NNN == 7)
SUBROUTINE nl_get_ifsnow ( id_id , ifsnow )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ifsnow
  INTEGER id_id
  ifsnow = model_config_rec%ifsnow
  RETURN
END SUBROUTINE nl_get_ifsnow
SUBROUTINE nl_get_icloud ( id_id , icloud )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: icloud
  INTEGER id_id
  icloud = model_config_rec%icloud
  RETURN
END SUBROUTINE nl_get_icloud
SUBROUTINE nl_get_swrad_scat ( id_id , swrad_scat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: swrad_scat
  INTEGER id_id
  swrad_scat = model_config_rec%swrad_scat
  RETURN
END SUBROUTINE nl_get_swrad_scat
SUBROUTINE nl_get_surface_input_source ( id_id , surface_input_source )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: surface_input_source
  INTEGER id_id
  surface_input_source = model_config_rec%surface_input_source
  RETURN
END SUBROUTINE nl_get_surface_input_source
SUBROUTINE nl_get_num_soil_layers ( id_id , num_soil_layers )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_soil_layers
  INTEGER id_id
  num_soil_layers = model_config_rec%num_soil_layers
  RETURN
END SUBROUTINE nl_get_num_soil_layers
SUBROUTINE nl_get_num_urban_layers ( id_id , num_urban_layers )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_urban_layers
  INTEGER id_id
  num_urban_layers = model_config_rec%num_urban_layers
  RETURN
END SUBROUTINE nl_get_num_urban_layers
SUBROUTINE nl_get_sf_surface_mosaic ( id_id , sf_surface_mosaic )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sf_surface_mosaic
  INTEGER id_id
  sf_surface_mosaic = model_config_rec%sf_surface_mosaic
  RETURN
END SUBROUTINE nl_get_sf_surface_mosaic
SUBROUTINE nl_get_mosaic_cat ( id_id , mosaic_cat )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: mosaic_cat
  INTEGER id_id
  mosaic_cat = model_config_rec%mosaic_cat
  RETURN
END SUBROUTINE nl_get_mosaic_cat
SUBROUTINE nl_get_mosaic_cat_soil ( id_id , mosaic_cat_soil )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: mosaic_cat_soil
  INTEGER id_id
  mosaic_cat_soil = model_config_rec%mosaic_cat_soil
  RETURN
END SUBROUTINE nl_get_mosaic_cat_soil
SUBROUTINE nl_get_num_urban_hi ( id_id , num_urban_hi )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_urban_hi
  INTEGER id_id
  num_urban_hi = model_config_rec%num_urban_hi
  RETURN
END SUBROUTINE nl_get_num_urban_hi
SUBROUTINE nl_get_mosaic_lu ( id_id , mosaic_lu )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: mosaic_lu
  INTEGER id_id
  mosaic_lu = model_config_rec%mosaic_lu
  RETURN
END SUBROUTINE nl_get_mosaic_lu
SUBROUTINE nl_get_mosaic_soil ( id_id , mosaic_soil )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: mosaic_soil
  INTEGER id_id
  mosaic_soil = model_config_rec%mosaic_soil
  RETURN
END SUBROUTINE nl_get_mosaic_soil
SUBROUTINE nl_get_maxiens ( id_id , maxiens )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: maxiens
  INTEGER id_id
  maxiens = model_config_rec%maxiens
  RETURN
END SUBROUTINE nl_get_maxiens
SUBROUTINE nl_get_maxens ( id_id , maxens )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: maxens
  INTEGER id_id
  maxens = model_config_rec%maxens
  RETURN
END SUBROUTINE nl_get_maxens
SUBROUTINE nl_get_maxens2 ( id_id , maxens2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: maxens2
  INTEGER id_id
  maxens2 = model_config_rec%maxens2
  RETURN
END SUBROUTINE nl_get_maxens2
SUBROUTINE nl_get_maxens3 ( id_id , maxens3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: maxens3
  INTEGER id_id
  maxens3 = model_config_rec%maxens3
  RETURN
END SUBROUTINE nl_get_maxens3
SUBROUTINE nl_get_ensdim ( id_id , ensdim )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ensdim
  INTEGER id_id
  ensdim = model_config_rec%ensdim
  RETURN
END SUBROUTINE nl_get_ensdim
SUBROUTINE nl_get_chem_opt ( id_id , chem_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: chem_opt
  INTEGER id_id
  chem_opt = model_config_rec%chem_opt(id_id)
  RETURN
END SUBROUTINE nl_get_chem_opt
SUBROUTINE nl_get_num_land_cat ( id_id , num_land_cat )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_land_cat
  INTEGER id_id
  num_land_cat = model_config_rec%num_land_cat
  RETURN
END SUBROUTINE nl_get_num_land_cat
SUBROUTINE nl_get_num_soil_cat ( id_id , num_soil_cat )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_soil_cat
  INTEGER id_id
  num_soil_cat = model_config_rec%num_soil_cat
  RETURN
END SUBROUTINE nl_get_num_soil_cat
SUBROUTINE nl_get_topo_wind ( id_id , topo_wind )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: topo_wind
  INTEGER id_id
  topo_wind = model_config_rec%topo_wind(id_id)
  RETURN
END SUBROUTINE nl_get_topo_wind
SUBROUTINE nl_get_mp_zero_out ( id_id , mp_zero_out )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: mp_zero_out
  INTEGER id_id
  mp_zero_out = model_config_rec%mp_zero_out
  RETURN
END SUBROUTINE nl_get_mp_zero_out
SUBROUTINE nl_get_mp_zero_out_thresh ( id_id , mp_zero_out_thresh )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: mp_zero_out_thresh
  INTEGER id_id
  mp_zero_out_thresh = model_config_rec%mp_zero_out_thresh
  RETURN
END SUBROUTINE nl_get_mp_zero_out_thresh
SUBROUTINE nl_get_seaice_threshold ( id_id , seaice_threshold )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: seaice_threshold
  INTEGER id_id
  seaice_threshold = model_config_rec%seaice_threshold
  RETURN
END SUBROUTINE nl_get_seaice_threshold
SUBROUTINE nl_get_fractional_seaice ( id_id , fractional_seaice )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: fractional_seaice
  INTEGER id_id
  fractional_seaice = model_config_rec%fractional_seaice
  RETURN
END SUBROUTINE nl_get_fractional_seaice
SUBROUTINE nl_get_seaice_albedo_opt ( id_id , seaice_albedo_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: seaice_albedo_opt
  INTEGER id_id
  seaice_albedo_opt = model_config_rec%seaice_albedo_opt
  RETURN
END SUBROUTINE nl_get_seaice_albedo_opt
SUBROUTINE nl_get_seaice_albedo_default ( id_id , seaice_albedo_default )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: seaice_albedo_default
  INTEGER id_id
  seaice_albedo_default = model_config_rec%seaice_albedo_default
  RETURN
END SUBROUTINE nl_get_seaice_albedo_default
SUBROUTINE nl_get_seaice_snowdepth_opt ( id_id , seaice_snowdepth_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: seaice_snowdepth_opt
  INTEGER id_id
  seaice_snowdepth_opt = model_config_rec%seaice_snowdepth_opt
  RETURN
END SUBROUTINE nl_get_seaice_snowdepth_opt
SUBROUTINE nl_get_seaice_snowdepth_max ( id_id , seaice_snowdepth_max )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: seaice_snowdepth_max
  INTEGER id_id
  seaice_snowdepth_max = model_config_rec%seaice_snowdepth_max
  RETURN
END SUBROUTINE nl_get_seaice_snowdepth_max
SUBROUTINE nl_get_seaice_snowdepth_min ( id_id , seaice_snowdepth_min )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: seaice_snowdepth_min
  INTEGER id_id
  seaice_snowdepth_min = model_config_rec%seaice_snowdepth_min
  RETURN
END SUBROUTINE nl_get_seaice_snowdepth_min
SUBROUTINE nl_get_seaice_thickness_opt ( id_id , seaice_thickness_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: seaice_thickness_opt
  INTEGER id_id
  seaice_thickness_opt = model_config_rec%seaice_thickness_opt
  RETURN
END SUBROUTINE nl_get_seaice_thickness_opt
SUBROUTINE nl_get_seaice_thickness_default ( id_id , seaice_thickness_default )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: seaice_thickness_default
  INTEGER id_id
  seaice_thickness_default = model_config_rec%seaice_thickness_default
  RETURN
END SUBROUTINE nl_get_seaice_thickness_default
SUBROUTINE nl_get_tice2tsk_if2cold ( id_id , tice2tsk_if2cold )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: tice2tsk_if2cold
  INTEGER id_id
  tice2tsk_if2cold = model_config_rec%tice2tsk_if2cold
  RETURN
END SUBROUTINE nl_get_tice2tsk_if2cold
SUBROUTINE nl_get_sst_update ( id_id , sst_update )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sst_update
  INTEGER id_id
  sst_update = model_config_rec%sst_update
  RETURN
END SUBROUTINE nl_get_sst_update
SUBROUTINE nl_get_sf_urban_physics ( id_id , sf_urban_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sf_urban_physics
  INTEGER id_id
  sf_urban_physics = model_config_rec%sf_urban_physics(id_id)
  RETURN
END SUBROUTINE nl_get_sf_urban_physics
SUBROUTINE nl_get_usemonalb ( id_id , usemonalb )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: usemonalb
  INTEGER id_id
  usemonalb = model_config_rec%usemonalb
  RETURN
END SUBROUTINE nl_get_usemonalb
SUBROUTINE nl_get_rdmaxalb ( id_id , rdmaxalb )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: rdmaxalb
  INTEGER id_id
  rdmaxalb = model_config_rec%rdmaxalb
  RETURN
END SUBROUTINE nl_get_rdmaxalb
SUBROUTINE nl_get_rdlai2d ( id_id , rdlai2d )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: rdlai2d
  INTEGER id_id
  rdlai2d = model_config_rec%rdlai2d
  RETURN
END SUBROUTINE nl_get_rdlai2d
SUBROUTINE nl_get_ua_phys ( id_id , ua_phys )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: ua_phys
  INTEGER id_id
  ua_phys = model_config_rec%ua_phys
  RETURN
END SUBROUTINE nl_get_ua_phys
SUBROUTINE nl_get_gwd_opt ( id_id , gwd_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: gwd_opt
  INTEGER id_id
  gwd_opt = model_config_rec%gwd_opt(id_id)
  RETURN
END SUBROUTINE nl_get_gwd_opt
SUBROUTINE nl_get_iz0tlnd ( id_id , iz0tlnd )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: iz0tlnd
  INTEGER id_id
  iz0tlnd = model_config_rec%iz0tlnd
  RETURN
END SUBROUTINE nl_get_iz0tlnd
SUBROUTINE nl_get_sas_pgcon ( id_id , sas_pgcon )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: sas_pgcon
  INTEGER id_id
  sas_pgcon = model_config_rec%sas_pgcon(id_id)
  RETURN
END SUBROUTINE nl_get_sas_pgcon
SUBROUTINE nl_get_sas_shal_pgcon ( id_id , sas_shal_pgcon )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: sas_shal_pgcon
  INTEGER id_id
  sas_shal_pgcon = model_config_rec%sas_shal_pgcon(id_id)
  RETURN
END SUBROUTINE nl_get_sas_shal_pgcon
SUBROUTINE nl_get_sas_shal_conv ( id_id , sas_shal_conv )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sas_shal_conv
  INTEGER id_id
  sas_shal_conv = model_config_rec%sas_shal_conv(id_id)
  RETURN
END SUBROUTINE nl_get_sas_shal_conv
SUBROUTINE nl_get_sas_mass_flux ( id_id , sas_mass_flux )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: sas_mass_flux
  INTEGER id_id
  sas_mass_flux = model_config_rec%sas_mass_flux(id_id)
  RETURN
END SUBROUTINE nl_get_sas_mass_flux
SUBROUTINE nl_get_var_ric ( id_id , var_ric )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: var_ric
  INTEGER id_id
  var_ric = model_config_rec%var_ric
  RETURN
END SUBROUTINE nl_get_var_ric
SUBROUTINE nl_get_coef_ric_l ( id_id , coef_ric_l )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: coef_ric_l
  INTEGER id_id
  coef_ric_l = model_config_rec%coef_ric_l
  RETURN
END SUBROUTINE nl_get_coef_ric_l
SUBROUTINE nl_get_coef_ric_s ( id_id , coef_ric_s )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: coef_ric_s
  INTEGER id_id
  coef_ric_s = model_config_rec%coef_ric_s
  RETURN
END SUBROUTINE nl_get_coef_ric_s
SUBROUTINE nl_get_random_seed ( id_id , random_seed )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: random_seed
  INTEGER id_id
  random_seed = model_config_rec%random_seed(id_id)
  RETURN
END SUBROUTINE nl_get_random_seed
SUBROUTINE nl_get_icoef_sf ( id_id , icoef_sf )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: icoef_sf
  INTEGER id_id
  icoef_sf = model_config_rec%icoef_sf(id_id)
  RETURN
END SUBROUTINE nl_get_icoef_sf
SUBROUTINE nl_get_lcurr_sf ( id_id , lcurr_sf )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: lcurr_sf
  INTEGER id_id
  lcurr_sf = model_config_rec%lcurr_sf(id_id)
  RETURN
END SUBROUTINE nl_get_lcurr_sf
SUBROUTINE nl_get_ens_random_seed ( id_id , ens_random_seed )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ens_random_seed
  INTEGER id_id
  ens_random_seed = model_config_rec%ens_random_seed(id_id)
  RETURN
END SUBROUTINE nl_get_ens_random_seed
SUBROUTINE nl_get_pert_sas ( id_id , pert_sas )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: pert_sas
  INTEGER id_id
  pert_sas = model_config_rec%pert_sas
  RETURN
END SUBROUTINE nl_get_pert_sas
SUBROUTINE nl_get_pert_pbl ( id_id , pert_pbl )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: pert_pbl
  INTEGER id_id
  pert_pbl = model_config_rec%pert_pbl
  RETURN
END SUBROUTINE nl_get_pert_pbl
SUBROUTINE nl_get_ens_sasamp ( id_id , ens_sasamp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: ens_sasamp
  INTEGER id_id
  ens_sasamp = model_config_rec%ens_sasamp(id_id)
  RETURN
END SUBROUTINE nl_get_ens_sasamp
SUBROUTINE nl_get_ens_pblamp ( id_id , ens_pblamp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: ens_pblamp
  INTEGER id_id
  ens_pblamp = model_config_rec%ens_pblamp(id_id)
  RETURN
END SUBROUTINE nl_get_ens_pblamp
SUBROUTINE nl_get_idtad ( id_id , idtad )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: idtad
  INTEGER id_id
  idtad = model_config_rec%idtad(id_id)
  RETURN
END SUBROUTINE nl_get_idtad
SUBROUTINE nl_get_nsoil ( id_id , nsoil )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nsoil
  INTEGER id_id
  nsoil = model_config_rec%nsoil(id_id)
  RETURN
END SUBROUTINE nl_get_nsoil
SUBROUTINE nl_get_nphs ( id_id , nphs )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nphs
  INTEGER id_id
  nphs = model_config_rec%nphs(id_id)
  RETURN
END SUBROUTINE nl_get_nphs
SUBROUTINE nl_get_ncnvc ( id_id , ncnvc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ncnvc
  INTEGER id_id
  ncnvc = model_config_rec%ncnvc(id_id)
  RETURN
END SUBROUTINE nl_get_ncnvc
SUBROUTINE nl_get_nrand ( id_id , nrand )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nrand
  INTEGER id_id
  nrand = model_config_rec%nrand(id_id)
  RETURN
END SUBROUTINE nl_get_nrand
SUBROUTINE nl_get_nrads ( id_id , nrads )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nrads
  INTEGER id_id
  nrads = model_config_rec%nrads(id_id)
  RETURN
END SUBROUTINE nl_get_nrads
SUBROUTINE nl_get_nradl ( id_id , nradl )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: nradl
  INTEGER id_id
  nradl = model_config_rec%nradl(id_id)
  RETURN
END SUBROUTINE nl_get_nradl
SUBROUTINE nl_get_tprec ( id_id , tprec )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: tprec
  INTEGER id_id
  tprec = model_config_rec%tprec(id_id)
  RETURN
END SUBROUTINE nl_get_tprec
SUBROUTINE nl_get_theat ( id_id , theat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: theat
  INTEGER id_id
  theat = model_config_rec%theat(id_id)
  RETURN
END SUBROUTINE nl_get_theat
SUBROUTINE nl_get_tclod ( id_id , tclod )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: tclod
  INTEGER id_id
  tclod = model_config_rec%tclod(id_id)
  RETURN
END SUBROUTINE nl_get_tclod
SUBROUTINE nl_get_trdsw ( id_id , trdsw )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: trdsw
  INTEGER id_id
  trdsw = model_config_rec%trdsw(id_id)
  RETURN
END SUBROUTINE nl_get_trdsw
SUBROUTINE nl_get_trdlw ( id_id , trdlw )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: trdlw
  INTEGER id_id
  trdlw = model_config_rec%trdlw(id_id)
  RETURN
END SUBROUTINE nl_get_trdlw
SUBROUTINE nl_get_tsrfc ( id_id , tsrfc )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: tsrfc
  INTEGER id_id
  tsrfc = model_config_rec%tsrfc(id_id)
  RETURN
END SUBROUTINE nl_get_tsrfc
SUBROUTINE nl_get_pcpflg ( id_id , pcpflg )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: pcpflg
  INTEGER id_id
  pcpflg = model_config_rec%pcpflg(id_id)
  RETURN
END SUBROUTINE nl_get_pcpflg
SUBROUTINE nl_get_sigma ( id_id , sigma )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sigma
  INTEGER id_id
  sigma = model_config_rec%sigma(id_id)
  RETURN
END SUBROUTINE nl_get_sigma
SUBROUTINE nl_get_sfenth ( id_id , sfenth )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: sfenth
  INTEGER id_id
  sfenth = model_config_rec%sfenth(id_id)
  RETURN
END SUBROUTINE nl_get_sfenth
SUBROUTINE nl_get_co2tf ( id_id , co2tf )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: co2tf
  INTEGER id_id
  co2tf = model_config_rec%co2tf
  RETURN
END SUBROUTINE nl_get_co2tf
SUBROUTINE nl_get_ra_call_offset ( id_id , ra_call_offset )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: ra_call_offset
  INTEGER id_id
  ra_call_offset = model_config_rec%ra_call_offset
  RETURN
END SUBROUTINE nl_get_ra_call_offset
SUBROUTINE nl_get_cam_abs_freq_s ( id_id , cam_abs_freq_s )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: cam_abs_freq_s
  INTEGER id_id
  cam_abs_freq_s = model_config_rec%cam_abs_freq_s
  RETURN
END SUBROUTINE nl_get_cam_abs_freq_s
SUBROUTINE nl_get_levsiz ( id_id , levsiz )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: levsiz
  INTEGER id_id
  levsiz = model_config_rec%levsiz
  RETURN
END SUBROUTINE nl_get_levsiz
SUBROUTINE nl_get_paerlev ( id_id , paerlev )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: paerlev
  INTEGER id_id
  paerlev = model_config_rec%paerlev
  RETURN
END SUBROUTINE nl_get_paerlev
SUBROUTINE nl_get_cam_abs_dim1 ( id_id , cam_abs_dim1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: cam_abs_dim1
  INTEGER id_id
  cam_abs_dim1 = model_config_rec%cam_abs_dim1
  RETURN
END SUBROUTINE nl_get_cam_abs_dim1
SUBROUTINE nl_get_cam_abs_dim2 ( id_id , cam_abs_dim2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: cam_abs_dim2
  INTEGER id_id
  cam_abs_dim2 = model_config_rec%cam_abs_dim2
  RETURN
END SUBROUTINE nl_get_cam_abs_dim2
SUBROUTINE nl_get_no_src_types ( id_id , no_src_types )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: no_src_types
  INTEGER id_id
  no_src_types = model_config_rec%no_src_types
  RETURN
END SUBROUTINE nl_get_no_src_types
SUBROUTINE nl_get_alevsiz ( id_id , alevsiz )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: alevsiz
  INTEGER id_id
  alevsiz = model_config_rec%alevsiz
  RETURN
END SUBROUTINE nl_get_alevsiz
SUBROUTINE nl_get_o3input ( id_id , o3input )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: o3input
  INTEGER id_id
  o3input = model_config_rec%o3input
  RETURN
END SUBROUTINE nl_get_o3input
SUBROUTINE nl_get_aer_opt ( id_id , aer_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: aer_opt
  INTEGER id_id
  aer_opt = model_config_rec%aer_opt
  RETURN
END SUBROUTINE nl_get_aer_opt
SUBROUTINE nl_get_cu_rad_feedback ( id_id , cu_rad_feedback )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: cu_rad_feedback
  INTEGER id_id
  cu_rad_feedback = model_config_rec%cu_rad_feedback(id_id)
  RETURN
END SUBROUTINE nl_get_cu_rad_feedback
SUBROUTINE nl_get_icloud_cu ( id_id , icloud_cu )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: icloud_cu
  INTEGER id_id
  icloud_cu = model_config_rec%icloud_cu
  RETURN
END SUBROUTINE nl_get_icloud_cu
SUBROUTINE nl_get_h_diff ( id_id , h_diff )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: h_diff
  INTEGER id_id
  h_diff = model_config_rec%h_diff(id_id)
  RETURN
END SUBROUTINE nl_get_h_diff
SUBROUTINE nl_get_movemin ( id_id , movemin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: movemin
  INTEGER id_id
  movemin = model_config_rec%movemin(id_id)
  RETURN
END SUBROUTINE nl_get_movemin
SUBROUTINE nl_get_num_snso_layers ( id_id , num_snso_layers )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_snso_layers
  INTEGER id_id
  num_snso_layers = model_config_rec%num_snso_layers
  RETURN
END SUBROUTINE nl_get_num_snso_layers
SUBROUTINE nl_get_num_snow_layers ( id_id , num_snow_layers )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: num_snow_layers
  INTEGER id_id
  num_snow_layers = model_config_rec%num_snow_layers
  RETURN
END SUBROUTINE nl_get_num_snow_layers
SUBROUTINE nl_get_use_aero_icbc ( id_id , use_aero_icbc )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: use_aero_icbc
  INTEGER id_id
  use_aero_icbc = model_config_rec%use_aero_icbc
  RETURN
END SUBROUTINE nl_get_use_aero_icbc
SUBROUTINE nl_get_ccn_conc ( id_id , ccn_conc )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: ccn_conc
  INTEGER id_id
  ccn_conc = model_config_rec%ccn_conc
  RETURN
END SUBROUTINE nl_get_ccn_conc
SUBROUTINE nl_get_hail_opt ( id_id , hail_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: hail_opt
  INTEGER id_id
  hail_opt = model_config_rec%hail_opt
  RETURN
END SUBROUTINE nl_get_hail_opt
SUBROUTINE nl_get_sf_lake_physics ( id_id , sf_lake_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: sf_lake_physics
  INTEGER id_id
  sf_lake_physics = model_config_rec%sf_lake_physics(id_id)
  RETURN
END SUBROUTINE nl_get_sf_lake_physics
SUBROUTINE nl_get_dyn_opt ( id_id , dyn_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dyn_opt
  INTEGER id_id
  dyn_opt = model_config_rec%dyn_opt
  RETURN
END SUBROUTINE nl_get_dyn_opt
SUBROUTINE nl_get_rk_ord ( id_id , rk_ord )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: rk_ord
  INTEGER id_id
  rk_ord = model_config_rec%rk_ord
  RETURN
END SUBROUTINE nl_get_rk_ord
SUBROUTINE nl_get_w_damping ( id_id , w_damping )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: w_damping
  INTEGER id_id
  w_damping = model_config_rec%w_damping
  RETURN
END SUBROUTINE nl_get_w_damping
SUBROUTINE nl_get_diff_opt ( id_id , diff_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: diff_opt
  INTEGER id_id
  diff_opt = model_config_rec%diff_opt(id_id)
  RETURN
END SUBROUTINE nl_get_diff_opt
SUBROUTINE nl_get_km_opt ( id_id , km_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: km_opt
  INTEGER id_id
  km_opt = model_config_rec%km_opt(id_id)
  RETURN
END SUBROUTINE nl_get_km_opt
SUBROUTINE nl_get_damp_opt ( id_id , damp_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: damp_opt
  INTEGER id_id
  damp_opt = model_config_rec%damp_opt
  RETURN
END SUBROUTINE nl_get_damp_opt
SUBROUTINE nl_get_zdamp ( id_id , zdamp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: zdamp
  INTEGER id_id
  zdamp = model_config_rec%zdamp(id_id)
  RETURN
END SUBROUTINE nl_get_zdamp
SUBROUTINE nl_get_base_pres ( id_id , base_pres )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: base_pres
  INTEGER id_id
  base_pres = model_config_rec%base_pres
  RETURN
END SUBROUTINE nl_get_base_pres
SUBROUTINE nl_get_base_temp ( id_id , base_temp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: base_temp
  INTEGER id_id
  base_temp = model_config_rec%base_temp
  RETURN
END SUBROUTINE nl_get_base_temp
SUBROUTINE nl_get_base_lapse ( id_id , base_lapse )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: base_lapse
  INTEGER id_id
  base_lapse = model_config_rec%base_lapse
  RETURN
END SUBROUTINE nl_get_base_lapse
SUBROUTINE nl_get_iso_temp ( id_id , iso_temp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: iso_temp
  INTEGER id_id
  iso_temp = model_config_rec%iso_temp
  RETURN
END SUBROUTINE nl_get_iso_temp
SUBROUTINE nl_get_dampcoef ( id_id , dampcoef )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: dampcoef
  INTEGER id_id
  dampcoef = model_config_rec%dampcoef(id_id)
  RETURN
END SUBROUTINE nl_get_dampcoef
SUBROUTINE nl_get_khdif ( id_id , khdif )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: khdif
  INTEGER id_id
  khdif = model_config_rec%khdif(id_id)
  RETURN
END SUBROUTINE nl_get_khdif
SUBROUTINE nl_get_kvdif ( id_id , kvdif )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: kvdif
  INTEGER id_id
  kvdif = model_config_rec%kvdif(id_id)
  RETURN
END SUBROUTINE nl_get_kvdif
SUBROUTINE nl_get_c_s ( id_id , c_s )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: c_s
  INTEGER id_id
  c_s = model_config_rec%c_s(id_id)
  RETURN
END SUBROUTINE nl_get_c_s
SUBROUTINE nl_get_c_k ( id_id , c_k )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: c_k
  INTEGER id_id
  c_k = model_config_rec%c_k(id_id)
  RETURN
END SUBROUTINE nl_get_c_k
SUBROUTINE nl_get_smdiv ( id_id , smdiv )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: smdiv
  INTEGER id_id
  smdiv = model_config_rec%smdiv(id_id)
  RETURN
END SUBROUTINE nl_get_smdiv
SUBROUTINE nl_get_emdiv ( id_id , emdiv )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: emdiv
  INTEGER id_id
  emdiv = model_config_rec%emdiv(id_id)
  RETURN
END SUBROUTINE nl_get_emdiv
SUBROUTINE nl_get_epssm ( id_id , epssm )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: epssm
  INTEGER id_id
  epssm = model_config_rec%epssm(id_id)
  RETURN
END SUBROUTINE nl_get_epssm
SUBROUTINE nl_get_non_hydrostatic ( id_id , non_hydrostatic )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: non_hydrostatic
  INTEGER id_id
  non_hydrostatic = model_config_rec%non_hydrostatic(id_id)
  RETURN
END SUBROUTINE nl_get_non_hydrostatic
SUBROUTINE nl_get_time_step_sound ( id_id , time_step_sound )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: time_step_sound
  INTEGER id_id
  time_step_sound = model_config_rec%time_step_sound(id_id)
  RETURN
END SUBROUTINE nl_get_time_step_sound
SUBROUTINE nl_get_h_mom_adv_order ( id_id , h_mom_adv_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: h_mom_adv_order
  INTEGER id_id
  h_mom_adv_order = model_config_rec%h_mom_adv_order(id_id)
  RETURN
END SUBROUTINE nl_get_h_mom_adv_order
SUBROUTINE nl_get_v_mom_adv_order ( id_id , v_mom_adv_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: v_mom_adv_order
  INTEGER id_id
  v_mom_adv_order = model_config_rec%v_mom_adv_order(id_id)
  RETURN
END SUBROUTINE nl_get_v_mom_adv_order
SUBROUTINE nl_get_h_sca_adv_order ( id_id , h_sca_adv_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: h_sca_adv_order
  INTEGER id_id
  h_sca_adv_order = model_config_rec%h_sca_adv_order(id_id)
  RETURN
END SUBROUTINE nl_get_h_sca_adv_order
SUBROUTINE nl_get_v_sca_adv_order ( id_id , v_sca_adv_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: v_sca_adv_order
  INTEGER id_id
  v_sca_adv_order = model_config_rec%v_sca_adv_order(id_id)
  RETURN
END SUBROUTINE nl_get_v_sca_adv_order
SUBROUTINE nl_get_top_radiation ( id_id , top_radiation )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: top_radiation
  INTEGER id_id
  top_radiation = model_config_rec%top_radiation(id_id)
  RETURN
END SUBROUTINE nl_get_top_radiation
SUBROUTINE nl_get_tke_upper_bound ( id_id , tke_upper_bound )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: tke_upper_bound
  INTEGER id_id
  tke_upper_bound = model_config_rec%tke_upper_bound(id_id)
  RETURN
END SUBROUTINE nl_get_tke_upper_bound
SUBROUTINE nl_get_tke_drag_coefficient ( id_id , tke_drag_coefficient )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: tke_drag_coefficient
  INTEGER id_id
  tke_drag_coefficient = model_config_rec%tke_drag_coefficient(id_id)
  RETURN
END SUBROUTINE nl_get_tke_drag_coefficient
SUBROUTINE nl_get_tke_heat_flux ( id_id , tke_heat_flux )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: tke_heat_flux
  INTEGER id_id
  tke_heat_flux = model_config_rec%tke_heat_flux(id_id)
  RETURN
END SUBROUTINE nl_get_tke_heat_flux
SUBROUTINE nl_get_pert_coriolis ( id_id , pert_coriolis )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: pert_coriolis
  INTEGER id_id
  pert_coriolis = model_config_rec%pert_coriolis(id_id)
  RETURN
END SUBROUTINE nl_get_pert_coriolis
SUBROUTINE nl_get_euler_adv ( id_id , euler_adv )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: euler_adv
  INTEGER id_id
  euler_adv = model_config_rec%euler_adv
  RETURN
END SUBROUTINE nl_get_euler_adv
SUBROUTINE nl_get_idtadt ( id_id , idtadt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: idtadt
  INTEGER id_id
  idtadt = model_config_rec%idtadt
  RETURN
END SUBROUTINE nl_get_idtadt
SUBROUTINE nl_get_idtadc ( id_id , idtadc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: idtadc
  INTEGER id_id
  idtadc = model_config_rec%idtadc
  RETURN
END SUBROUTINE nl_get_idtadc
SUBROUTINE nl_get_codamp ( id_id , codamp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: codamp
  INTEGER id_id
  codamp = model_config_rec%codamp(id_id)
  RETURN
END SUBROUTINE nl_get_codamp
SUBROUTINE nl_get_coac ( id_id , coac )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: coac
  INTEGER id_id
  coac = model_config_rec%coac(id_id)
  RETURN
END SUBROUTINE nl_get_coac
SUBROUTINE nl_get_slophc ( id_id , slophc )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: slophc
  INTEGER id_id
  slophc = model_config_rec%slophc(id_id)
  RETURN
END SUBROUTINE nl_get_slophc
SUBROUTINE nl_get_wp ( id_id , wp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: wp
  INTEGER id_id
  wp = model_config_rec%wp(id_id)
  RETURN
END SUBROUTINE nl_get_wp
SUBROUTINE nl_get_terrain_smoothing ( id_id , terrain_smoothing )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: terrain_smoothing
  INTEGER id_id
  terrain_smoothing = model_config_rec%terrain_smoothing
  RETURN
END SUBROUTINE nl_get_terrain_smoothing
SUBROUTINE nl_get_spec_bdy_width ( id_id , spec_bdy_width )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: spec_bdy_width
  INTEGER id_id
  spec_bdy_width = model_config_rec%spec_bdy_width
  RETURN
END SUBROUTINE nl_get_spec_bdy_width
SUBROUTINE nl_get_spec_zone ( id_id , spec_zone )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: spec_zone
  INTEGER id_id
  spec_zone = model_config_rec%spec_zone
  RETURN
END SUBROUTINE nl_get_spec_zone
SUBROUTINE nl_get_relax_zone ( id_id , relax_zone )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: relax_zone
  INTEGER id_id
  relax_zone = model_config_rec%relax_zone
  RETURN
END SUBROUTINE nl_get_relax_zone
SUBROUTINE nl_get_specified ( id_id , specified )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: specified
  INTEGER id_id
  specified = model_config_rec%specified(id_id)
  RETURN
END SUBROUTINE nl_get_specified
SUBROUTINE nl_get_periodic_x ( id_id , periodic_x )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: periodic_x
  INTEGER id_id
  periodic_x = model_config_rec%periodic_x(id_id)
  RETURN
END SUBROUTINE nl_get_periodic_x
SUBROUTINE nl_get_symmetric_xs ( id_id , symmetric_xs )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: symmetric_xs
  INTEGER id_id
  symmetric_xs = model_config_rec%symmetric_xs(id_id)
  RETURN
END SUBROUTINE nl_get_symmetric_xs
SUBROUTINE nl_get_symmetric_xe ( id_id , symmetric_xe )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: symmetric_xe
  INTEGER id_id
  symmetric_xe = model_config_rec%symmetric_xe(id_id)
  RETURN
END SUBROUTINE nl_get_symmetric_xe
SUBROUTINE nl_get_open_xs ( id_id , open_xs )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: open_xs
  INTEGER id_id
  open_xs = model_config_rec%open_xs(id_id)
  RETURN
END SUBROUTINE nl_get_open_xs
SUBROUTINE nl_get_open_xe ( id_id , open_xe )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: open_xe
  INTEGER id_id
  open_xe = model_config_rec%open_xe(id_id)
  RETURN
END SUBROUTINE nl_get_open_xe
SUBROUTINE nl_get_periodic_y ( id_id , periodic_y )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: periodic_y
  INTEGER id_id
  periodic_y = model_config_rec%periodic_y(id_id)
  RETURN
END SUBROUTINE nl_get_periodic_y
SUBROUTINE nl_get_symmetric_ys ( id_id , symmetric_ys )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: symmetric_ys
  INTEGER id_id
  symmetric_ys = model_config_rec%symmetric_ys(id_id)
  RETURN
END SUBROUTINE nl_get_symmetric_ys
SUBROUTINE nl_get_symmetric_ye ( id_id , symmetric_ye )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: symmetric_ye
  INTEGER id_id
  symmetric_ye = model_config_rec%symmetric_ye(id_id)
  RETURN
END SUBROUTINE nl_get_symmetric_ye
SUBROUTINE nl_get_open_ys ( id_id , open_ys )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: open_ys
  INTEGER id_id
  open_ys = model_config_rec%open_ys(id_id)
  RETURN
END SUBROUTINE nl_get_open_ys
SUBROUTINE nl_get_open_ye ( id_id , open_ye )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: open_ye
  INTEGER id_id
  open_ye = model_config_rec%open_ye(id_id)
  RETURN
END SUBROUTINE nl_get_open_ye
SUBROUTINE nl_get_polar ( id_id , polar )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: polar
  INTEGER id_id
  polar = model_config_rec%polar(id_id)
  RETURN
END SUBROUTINE nl_get_polar
SUBROUTINE nl_get_nested ( id_id , nested )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(OUT) :: nested
  INTEGER id_id
  nested = model_config_rec%nested(id_id)
  RETURN
END SUBROUTINE nl_get_nested
SUBROUTINE nl_get_real_data_init_type ( id_id , real_data_init_type )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: real_data_init_type
  INTEGER id_id
  real_data_init_type = model_config_rec%real_data_init_type
  RETURN
END SUBROUTINE nl_get_real_data_init_type
SUBROUTINE nl_get_background_proc_id ( id_id , background_proc_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: background_proc_id
  INTEGER id_id
  background_proc_id = model_config_rec%background_proc_id
  RETURN
END SUBROUTINE nl_get_background_proc_id
SUBROUTINE nl_get_forecast_proc_id ( id_id , forecast_proc_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: forecast_proc_id
  INTEGER id_id
  forecast_proc_id = model_config_rec%forecast_proc_id
  RETURN
END SUBROUTINE nl_get_forecast_proc_id
SUBROUTINE nl_get_production_status ( id_id , production_status )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: production_status
  INTEGER id_id
  production_status = model_config_rec%production_status
  RETURN
END SUBROUTINE nl_get_production_status
SUBROUTINE nl_get_compression ( id_id , compression )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: compression
  INTEGER id_id
  compression = model_config_rec%compression
  RETURN
END SUBROUTINE nl_get_compression
SUBROUTINE nl_get_cen_lat ( id_id , cen_lat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: cen_lat
  INTEGER id_id
  cen_lat = model_config_rec%cen_lat(id_id)
  RETURN
END SUBROUTINE nl_get_cen_lat
SUBROUTINE nl_get_cen_lon ( id_id , cen_lon )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: cen_lon
  INTEGER id_id
  cen_lon = model_config_rec%cen_lon(id_id)
  RETURN
END SUBROUTINE nl_get_cen_lon
SUBROUTINE nl_get_truelat1 ( id_id , truelat1 )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: truelat1
  INTEGER id_id
  truelat1 = model_config_rec%truelat1(id_id)
  RETURN
END SUBROUTINE nl_get_truelat1
SUBROUTINE nl_get_truelat2 ( id_id , truelat2 )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: truelat2
  INTEGER id_id
  truelat2 = model_config_rec%truelat2(id_id)
  RETURN
END SUBROUTINE nl_get_truelat2
SUBROUTINE nl_get_moad_cen_lat ( id_id , moad_cen_lat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: moad_cen_lat
  INTEGER id_id
  moad_cen_lat = model_config_rec%moad_cen_lat(id_id)
  RETURN
END SUBROUTINE nl_get_moad_cen_lat
SUBROUTINE nl_get_stand_lon ( id_id , stand_lon )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: stand_lon
  INTEGER id_id
  stand_lon = model_config_rec%stand_lon(id_id)
  RETURN
END SUBROUTINE nl_get_stand_lon
SUBROUTINE nl_get_flag_metgrid ( id_id , flag_metgrid )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_metgrid
  INTEGER id_id
  flag_metgrid = model_config_rec%flag_metgrid
  RETURN
END SUBROUTINE nl_get_flag_metgrid
SUBROUTINE nl_get_flag_snow ( id_id , flag_snow )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_snow
  INTEGER id_id
  flag_snow = model_config_rec%flag_snow
  RETURN
END SUBROUTINE nl_get_flag_snow
SUBROUTINE nl_get_flag_psfc ( id_id , flag_psfc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_psfc
  INTEGER id_id
  flag_psfc = model_config_rec%flag_psfc
  RETURN
END SUBROUTINE nl_get_flag_psfc
SUBROUTINE nl_get_flag_sm000010 ( id_id , flag_sm000010 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_sm000010
  INTEGER id_id
  flag_sm000010 = model_config_rec%flag_sm000010
  RETURN
END SUBROUTINE nl_get_flag_sm000010
SUBROUTINE nl_get_flag_sm010040 ( id_id , flag_sm010040 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_sm010040
  INTEGER id_id
  flag_sm010040 = model_config_rec%flag_sm010040
  RETURN
END SUBROUTINE nl_get_flag_sm010040
SUBROUTINE nl_get_flag_sm040100 ( id_id , flag_sm040100 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_sm040100
  INTEGER id_id
  flag_sm040100 = model_config_rec%flag_sm040100
  RETURN
END SUBROUTINE nl_get_flag_sm040100
SUBROUTINE nl_get_flag_sm100200 ( id_id , flag_sm100200 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_sm100200
  INTEGER id_id
  flag_sm100200 = model_config_rec%flag_sm100200
  RETURN
END SUBROUTINE nl_get_flag_sm100200
SUBROUTINE nl_get_flag_st000010 ( id_id , flag_st000010 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_st000010
  INTEGER id_id
  flag_st000010 = model_config_rec%flag_st000010
  RETURN
END SUBROUTINE nl_get_flag_st000010
SUBROUTINE nl_get_flag_st010040 ( id_id , flag_st010040 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_st010040
  INTEGER id_id
  flag_st010040 = model_config_rec%flag_st010040
  RETURN
END SUBROUTINE nl_get_flag_st010040
SUBROUTINE nl_get_flag_st040100 ( id_id , flag_st040100 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_st040100
  INTEGER id_id
  flag_st040100 = model_config_rec%flag_st040100
  RETURN
END SUBROUTINE nl_get_flag_st040100
SUBROUTINE nl_get_flag_st100200 ( id_id , flag_st100200 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_st100200
  INTEGER id_id
  flag_st100200 = model_config_rec%flag_st100200
  RETURN
END SUBROUTINE nl_get_flag_st100200
SUBROUTINE nl_get_flag_slp ( id_id , flag_slp )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_slp
  INTEGER id_id
  flag_slp = model_config_rec%flag_slp
  RETURN
END SUBROUTINE nl_get_flag_slp
SUBROUTINE nl_get_flag_soilhgt ( id_id , flag_soilhgt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_soilhgt
  INTEGER id_id
  flag_soilhgt = model_config_rec%flag_soilhgt
  RETURN
END SUBROUTINE nl_get_flag_soilhgt
SUBROUTINE nl_get_flag_mf_xy ( id_id , flag_mf_xy )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: flag_mf_xy
  INTEGER id_id
  flag_mf_xy = model_config_rec%flag_mf_xy
  RETURN
END SUBROUTINE nl_get_flag_mf_xy
SUBROUTINE nl_get_bdyfrq ( id_id , bdyfrq )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(OUT) :: bdyfrq
  INTEGER id_id
  bdyfrq = model_config_rec%bdyfrq(id_id)
  RETURN
END SUBROUTINE nl_get_bdyfrq
SUBROUTINE nl_get_mminlu ( id_id , mminlu )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(OUT) :: mminlu
  INTEGER id_id
  mminlu = model_config_rec%mminlu(id_id)
  RETURN
END SUBROUTINE nl_get_mminlu
SUBROUTINE nl_get_iswater ( id_id , iswater )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: iswater
  INTEGER id_id
  iswater = model_config_rec%iswater(id_id)
  RETURN
END SUBROUTINE nl_get_iswater
SUBROUTINE nl_get_islake ( id_id , islake )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: islake
  INTEGER id_id
  islake = model_config_rec%islake(id_id)
  RETURN
END SUBROUTINE nl_get_islake
SUBROUTINE nl_get_isice ( id_id , isice )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: isice
  INTEGER id_id
  isice = model_config_rec%isice(id_id)
  RETURN
END SUBROUTINE nl_get_isice
SUBROUTINE nl_get_isurban ( id_id , isurban )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: isurban
  INTEGER id_id
  isurban = model_config_rec%isurban(id_id)
  RETURN
END SUBROUTINE nl_get_isurban
SUBROUTINE nl_get_isoilwater ( id_id , isoilwater )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: isoilwater
  INTEGER id_id
  isoilwater = model_config_rec%isoilwater(id_id)
  RETURN
END SUBROUTINE nl_get_isoilwater
SUBROUTINE nl_get_map_proj ( id_id , map_proj )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: map_proj
  INTEGER id_id
  map_proj = model_config_rec%map_proj(id_id)
  RETURN
END SUBROUTINE nl_get_map_proj
SUBROUTINE nl_get_dfi_stage ( id_id , dfi_stage )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: dfi_stage
  INTEGER id_id
  dfi_stage = model_config_rec%dfi_stage
  RETURN
END SUBROUTINE nl_get_dfi_stage
SUBROUTINE nl_get_mp_physics_dfi ( id_id , mp_physics_dfi )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: mp_physics_dfi
  INTEGER id_id
  mp_physics_dfi = model_config_rec%mp_physics_dfi(id_id)
  RETURN
END SUBROUTINE nl_get_mp_physics_dfi
SUBROUTINE nl_get_maxpatch ( id_id , maxpatch )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(OUT) :: maxpatch
  INTEGER id_id
  maxpatch = model_config_rec%maxpatch
  RETURN
END SUBROUTINE nl_get_maxpatch
#endif
#endif
#ifdef NL_set_ROUTINES
#if (NNN == 0)
SUBROUTINE nl_set_lakedepth_default ( id_id , lakedepth_default )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: lakedepth_default
  INTEGER id_id
  model_config_rec%lakedepth_default(id_id) = lakedepth_default
  RETURN
END SUBROUTINE nl_set_lakedepth_default
SUBROUTINE nl_set_lake_min_elev ( id_id , lake_min_elev )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: lake_min_elev
  INTEGER id_id
  model_config_rec%lake_min_elev(id_id) = lake_min_elev
  RETURN
END SUBROUTINE nl_set_lake_min_elev
SUBROUTINE nl_set_use_lakedepth ( id_id , use_lakedepth )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: use_lakedepth
  INTEGER id_id
  model_config_rec%use_lakedepth(id_id) = use_lakedepth
  RETURN
END SUBROUTINE nl_set_use_lakedepth
SUBROUTINE nl_set_halo_debug ( id_id , halo_debug )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: halo_debug
  INTEGER id_id
  model_config_rec%halo_debug = halo_debug 
  RETURN
END SUBROUTINE nl_set_halo_debug
SUBROUTINE nl_set_ntracers ( id_id , ntracers )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ntracers
  INTEGER id_id
  model_config_rec%ntracers = ntracers 
  RETURN
END SUBROUTINE nl_set_ntracers
SUBROUTINE nl_set_vortex_tracker ( id_id , vortex_tracker )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: vortex_tracker
  INTEGER id_id
  model_config_rec%vortex_tracker(id_id) = vortex_tracker
  RETURN
END SUBROUTINE nl_set_vortex_tracker
SUBROUTINE nl_set_interest_rad_storm ( id_id , interest_rad_storm )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: interest_rad_storm
  INTEGER id_id
  model_config_rec%interest_rad_storm(id_id) = interest_rad_storm
  RETURN
END SUBROUTINE nl_set_interest_rad_storm
SUBROUTINE nl_set_interest_rad_parent ( id_id , interest_rad_parent )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: interest_rad_parent
  INTEGER id_id
  model_config_rec%interest_rad_parent(id_id) = interest_rad_parent
  RETURN
END SUBROUTINE nl_set_interest_rad_parent
SUBROUTINE nl_set_interest_rad_self ( id_id , interest_rad_self )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: interest_rad_self
  INTEGER id_id
  model_config_rec%interest_rad_self(id_id) = interest_rad_self
  RETURN
END SUBROUTINE nl_set_interest_rad_self
SUBROUTINE nl_set_interest_kids ( id_id , interest_kids )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: interest_kids
  INTEGER id_id
  model_config_rec%interest_kids(id_id) = interest_kids
  RETURN
END SUBROUTINE nl_set_interest_kids
SUBROUTINE nl_set_interest_self ( id_id , interest_self )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: interest_self
  INTEGER id_id
  model_config_rec%interest_self(id_id) = interest_self
  RETURN
END SUBROUTINE nl_set_interest_self
SUBROUTINE nl_set_interest_storms ( id_id , interest_storms )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: interest_storms
  INTEGER id_id
  model_config_rec%interest_storms(id_id) = interest_storms
  RETURN
END SUBROUTINE nl_set_interest_storms
SUBROUTINE nl_set_swath_mode ( id_id , swath_mode )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: swath_mode
  INTEGER id_id
  model_config_rec%swath_mode = swath_mode 
  RETURN
END SUBROUTINE nl_set_swath_mode
SUBROUTINE nl_set_num_old_fixes ( id_id , num_old_fixes )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_old_fixes
  INTEGER id_id
  model_config_rec%num_old_fixes = num_old_fixes 
  RETURN
END SUBROUTINE nl_set_num_old_fixes
SUBROUTINE nl_set_vt4_radius ( id_id , vt4_radius )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: vt4_radius
  INTEGER id_id
  model_config_rec%vt4_radius(id_id) = vt4_radius
  RETURN
END SUBROUTINE nl_set_vt4_radius
SUBROUTINE nl_set_vt4_weightexp ( id_id , vt4_weightexp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: vt4_weightexp
  INTEGER id_id
  model_config_rec%vt4_weightexp(id_id) = vt4_weightexp
  RETURN
END SUBROUTINE nl_set_vt4_weightexp
SUBROUTINE nl_set_vt4_pmax ( id_id , vt4_pmax )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: vt4_pmax
  INTEGER id_id
  model_config_rec%vt4_pmax(id_id) = vt4_pmax
  RETURN
END SUBROUTINE nl_set_vt4_pmax
SUBROUTINE nl_set_vt4_noise_pmax ( id_id , vt4_noise_pmax )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: vt4_noise_pmax
  INTEGER id_id
  model_config_rec%vt4_noise_pmax(id_id) = vt4_noise_pmax
  RETURN
END SUBROUTINE nl_set_vt4_noise_pmax
SUBROUTINE nl_set_vt4_noise_pmin ( id_id , vt4_noise_pmin )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: vt4_noise_pmin
  INTEGER id_id
  model_config_rec%vt4_noise_pmin(id_id) = vt4_noise_pmin
  RETURN
END SUBROUTINE nl_set_vt4_noise_pmin
SUBROUTINE nl_set_vt4_noise_dpdr ( id_id , vt4_noise_dpdr )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: vt4_noise_dpdr
  INTEGER id_id
  model_config_rec%vt4_noise_dpdr(id_id) = vt4_noise_dpdr
  RETURN
END SUBROUTINE nl_set_vt4_noise_dpdr
SUBROUTINE nl_set_vt4_noise_iter ( id_id , vt4_noise_iter )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: vt4_noise_iter
  INTEGER id_id
  model_config_rec%vt4_noise_iter(id_id) = vt4_noise_iter
  RETURN
END SUBROUTINE nl_set_vt4_noise_iter
SUBROUTINE nl_set_nomove_freq ( id_id , nomove_freq )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: nomove_freq
  INTEGER id_id
  model_config_rec%nomove_freq(id_id) = nomove_freq
  RETURN
END SUBROUTINE nl_set_nomove_freq
SUBROUTINE nl_set_coral_x ( id_id , coral_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: coral_x
  INTEGER id_id
  model_config_rec%coral_x(id_id) = coral_x
  RETURN
END SUBROUTINE nl_set_coral_x
SUBROUTINE nl_set_coral_y ( id_id , coral_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: coral_y
  INTEGER id_id
  model_config_rec%coral_y(id_id) = coral_y
  RETURN
END SUBROUTINE nl_set_coral_y
SUBROUTINE nl_set_tg_reset_stream ( id_id , tg_reset_stream )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: tg_reset_stream
  INTEGER id_id
  model_config_rec%tg_reset_stream = tg_reset_stream 
  RETURN
END SUBROUTINE nl_set_tg_reset_stream
SUBROUTINE nl_set_tg_option ( id_id , tg_option )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: tg_option
  INTEGER id_id
  model_config_rec%tg_option = tg_option 
  RETURN
END SUBROUTINE nl_set_tg_option
SUBROUTINE nl_set_ntornado ( id_id , ntornado )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ntornado
  INTEGER id_id
  model_config_rec%ntornado(id_id) = ntornado
  RETURN
END SUBROUTINE nl_set_ntornado
SUBROUTINE nl_set_wbd0 ( id_id , wbd0 )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: wbd0
  INTEGER id_id
  model_config_rec%wbd0(id_id) = wbd0
  RETURN
END SUBROUTINE nl_set_wbd0
SUBROUTINE nl_set_sbd0 ( id_id , sbd0 )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: sbd0
  INTEGER id_id
  model_config_rec%sbd0(id_id) = sbd0
  RETURN
END SUBROUTINE nl_set_sbd0
SUBROUTINE nl_set_analysis ( id_id , analysis )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: analysis
  INTEGER id_id
  model_config_rec%analysis(id_id) = analysis
  RETURN
END SUBROUTINE nl_set_analysis
SUBROUTINE nl_set_write_analysis ( id_id , write_analysis )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: write_analysis
  INTEGER id_id
  model_config_rec%write_analysis(id_id) = write_analysis
  RETURN
END SUBROUTINE nl_set_write_analysis
SUBROUTINE nl_set_io_form_auxinput2 ( id_id , io_form_auxinput2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput2
  INTEGER id_id
  model_config_rec%io_form_auxinput2 = io_form_auxinput2 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput2
SUBROUTINE nl_set_high_freq ( id_id , high_freq )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: high_freq
  INTEGER id_id
  model_config_rec%high_freq = high_freq 
  RETURN
END SUBROUTINE nl_set_high_freq
SUBROUTINE nl_set_high_dom ( id_id , high_dom )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: high_dom
  INTEGER id_id
  model_config_rec%high_dom = high_dom 
  RETURN
END SUBROUTINE nl_set_high_dom
SUBROUTINE nl_set_swint_opt ( id_id , swint_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: swint_opt
  INTEGER id_id
  model_config_rec%swint_opt = swint_opt 
  RETURN
END SUBROUTINE nl_set_swint_opt
SUBROUTINE nl_set_aer_type ( id_id , aer_type )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: aer_type
  INTEGER id_id
  model_config_rec%aer_type(id_id) = aer_type
  RETURN
END SUBROUTINE nl_set_aer_type
SUBROUTINE nl_set_aer_aod550_opt ( id_id , aer_aod550_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: aer_aod550_opt
  INTEGER id_id
  model_config_rec%aer_aod550_opt(id_id) = aer_aod550_opt
  RETURN
END SUBROUTINE nl_set_aer_aod550_opt
SUBROUTINE nl_set_aer_angexp_opt ( id_id , aer_angexp_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: aer_angexp_opt
  INTEGER id_id
  model_config_rec%aer_angexp_opt(id_id) = aer_angexp_opt
  RETURN
END SUBROUTINE nl_set_aer_angexp_opt
SUBROUTINE nl_set_aer_ssa_opt ( id_id , aer_ssa_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: aer_ssa_opt
  INTEGER id_id
  model_config_rec%aer_ssa_opt(id_id) = aer_ssa_opt
  RETURN
END SUBROUTINE nl_set_aer_ssa_opt
SUBROUTINE nl_set_aer_asy_opt ( id_id , aer_asy_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: aer_asy_opt
  INTEGER id_id
  model_config_rec%aer_asy_opt(id_id) = aer_asy_opt
  RETURN
END SUBROUTINE nl_set_aer_asy_opt
SUBROUTINE nl_set_aer_aod550_val ( id_id , aer_aod550_val )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: aer_aod550_val
  INTEGER id_id
  model_config_rec%aer_aod550_val(id_id) = aer_aod550_val
  RETURN
END SUBROUTINE nl_set_aer_aod550_val
SUBROUTINE nl_set_aer_angexp_val ( id_id , aer_angexp_val )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: aer_angexp_val
  INTEGER id_id
  model_config_rec%aer_angexp_val(id_id) = aer_angexp_val
  RETURN
END SUBROUTINE nl_set_aer_angexp_val
SUBROUTINE nl_set_aer_ssa_val ( id_id , aer_ssa_val )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: aer_ssa_val
  INTEGER id_id
  model_config_rec%aer_ssa_val(id_id) = aer_ssa_val
  RETURN
END SUBROUTINE nl_set_aer_ssa_val
SUBROUTINE nl_set_aer_asy_val ( id_id , aer_asy_val )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: aer_asy_val
  INTEGER id_id
  model_config_rec%aer_asy_val(id_id) = aer_asy_val
  RETURN
END SUBROUTINE nl_set_aer_asy_val
SUBROUTINE nl_set_dveg ( id_id , dveg )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dveg
  INTEGER id_id
  model_config_rec%dveg = dveg 
  RETURN
END SUBROUTINE nl_set_dveg
SUBROUTINE nl_set_opt_crs ( id_id , opt_crs )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_crs
  INTEGER id_id
  model_config_rec%opt_crs = opt_crs 
  RETURN
END SUBROUTINE nl_set_opt_crs
SUBROUTINE nl_set_opt_btr ( id_id , opt_btr )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_btr
  INTEGER id_id
  model_config_rec%opt_btr = opt_btr 
  RETURN
END SUBROUTINE nl_set_opt_btr
SUBROUTINE nl_set_opt_run ( id_id , opt_run )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_run
  INTEGER id_id
  model_config_rec%opt_run = opt_run 
  RETURN
END SUBROUTINE nl_set_opt_run
SUBROUTINE nl_set_opt_sfc ( id_id , opt_sfc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_sfc
  INTEGER id_id
  model_config_rec%opt_sfc = opt_sfc 
  RETURN
END SUBROUTINE nl_set_opt_sfc
SUBROUTINE nl_set_opt_frz ( id_id , opt_frz )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_frz
  INTEGER id_id
  model_config_rec%opt_frz = opt_frz 
  RETURN
END SUBROUTINE nl_set_opt_frz
SUBROUTINE nl_set_opt_inf ( id_id , opt_inf )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_inf
  INTEGER id_id
  model_config_rec%opt_inf = opt_inf 
  RETURN
END SUBROUTINE nl_set_opt_inf
SUBROUTINE nl_set_opt_rad ( id_id , opt_rad )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_rad
  INTEGER id_id
  model_config_rec%opt_rad = opt_rad 
  RETURN
END SUBROUTINE nl_set_opt_rad
SUBROUTINE nl_set_opt_alb ( id_id , opt_alb )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_alb
  INTEGER id_id
  model_config_rec%opt_alb = opt_alb 
  RETURN
END SUBROUTINE nl_set_opt_alb
SUBROUTINE nl_set_opt_snf ( id_id , opt_snf )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_snf
  INTEGER id_id
  model_config_rec%opt_snf = opt_snf 
  RETURN
END SUBROUTINE nl_set_opt_snf
SUBROUTINE nl_set_opt_tbot ( id_id , opt_tbot )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_tbot
  INTEGER id_id
  model_config_rec%opt_tbot = opt_tbot 
  RETURN
END SUBROUTINE nl_set_opt_tbot
SUBROUTINE nl_set_opt_stc ( id_id , opt_stc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: opt_stc
  INTEGER id_id
  model_config_rec%opt_stc = opt_stc 
  RETURN
END SUBROUTINE nl_set_opt_stc
SUBROUTINE nl_set_wrf_hydro ( id_id , wrf_hydro )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: wrf_hydro
  INTEGER id_id
  model_config_rec%wrf_hydro = wrf_hydro 
  RETURN
END SUBROUTINE nl_set_wrf_hydro
SUBROUTINE nl_set_run_days ( id_id , run_days )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: run_days
  INTEGER id_id
  model_config_rec%run_days = run_days 
  RETURN
END SUBROUTINE nl_set_run_days
SUBROUTINE nl_set_run_hours ( id_id , run_hours )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: run_hours
  INTEGER id_id
  model_config_rec%run_hours = run_hours 
  RETURN
END SUBROUTINE nl_set_run_hours
SUBROUTINE nl_set_run_minutes ( id_id , run_minutes )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: run_minutes
  INTEGER id_id
  model_config_rec%run_minutes = run_minutes 
  RETURN
END SUBROUTINE nl_set_run_minutes
SUBROUTINE nl_set_run_seconds ( id_id , run_seconds )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: run_seconds
  INTEGER id_id
  model_config_rec%run_seconds = run_seconds 
  RETURN
END SUBROUTINE nl_set_run_seconds
SUBROUTINE nl_set_start_year ( id_id , start_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: start_year
  INTEGER id_id
  model_config_rec%start_year(id_id) = start_year
  RETURN
END SUBROUTINE nl_set_start_year
SUBROUTINE nl_set_start_month ( id_id , start_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: start_month
  INTEGER id_id
  model_config_rec%start_month(id_id) = start_month
  RETURN
END SUBROUTINE nl_set_start_month
SUBROUTINE nl_set_start_day ( id_id , start_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: start_day
  INTEGER id_id
  model_config_rec%start_day(id_id) = start_day
  RETURN
END SUBROUTINE nl_set_start_day
SUBROUTINE nl_set_start_hour ( id_id , start_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: start_hour
  INTEGER id_id
  model_config_rec%start_hour(id_id) = start_hour
  RETURN
END SUBROUTINE nl_set_start_hour
SUBROUTINE nl_set_start_minute ( id_id , start_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: start_minute
  INTEGER id_id
  model_config_rec%start_minute(id_id) = start_minute
  RETURN
END SUBROUTINE nl_set_start_minute
SUBROUTINE nl_set_start_second ( id_id , start_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: start_second
  INTEGER id_id
  model_config_rec%start_second(id_id) = start_second
  RETURN
END SUBROUTINE nl_set_start_second
SUBROUTINE nl_set_end_year ( id_id , end_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: end_year
  INTEGER id_id
  model_config_rec%end_year(id_id) = end_year
  RETURN
END SUBROUTINE nl_set_end_year
SUBROUTINE nl_set_end_month ( id_id , end_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: end_month
  INTEGER id_id
  model_config_rec%end_month(id_id) = end_month
  RETURN
END SUBROUTINE nl_set_end_month
SUBROUTINE nl_set_end_day ( id_id , end_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: end_day
  INTEGER id_id
  model_config_rec%end_day(id_id) = end_day
  RETURN
END SUBROUTINE nl_set_end_day
SUBROUTINE nl_set_end_hour ( id_id , end_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: end_hour
  INTEGER id_id
  model_config_rec%end_hour(id_id) = end_hour
  RETURN
END SUBROUTINE nl_set_end_hour
SUBROUTINE nl_set_end_minute ( id_id , end_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: end_minute
  INTEGER id_id
  model_config_rec%end_minute(id_id) = end_minute
  RETURN
END SUBROUTINE nl_set_end_minute
SUBROUTINE nl_set_end_second ( id_id , end_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: end_second
  INTEGER id_id
  model_config_rec%end_second(id_id) = end_second
  RETURN
END SUBROUTINE nl_set_end_second
SUBROUTINE nl_set_interval_seconds ( id_id , interval_seconds )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: interval_seconds
  INTEGER id_id
  model_config_rec%interval_seconds = interval_seconds 
  RETURN
END SUBROUTINE nl_set_interval_seconds
SUBROUTINE nl_set_input_from_file ( id_id , input_from_file )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: input_from_file
  INTEGER id_id
  model_config_rec%input_from_file(id_id) = input_from_file
  RETURN
END SUBROUTINE nl_set_input_from_file
SUBROUTINE nl_set_fine_input_stream ( id_id , fine_input_stream )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: fine_input_stream
  INTEGER id_id
  model_config_rec%fine_input_stream(id_id) = fine_input_stream
  RETURN
END SUBROUTINE nl_set_fine_input_stream
SUBROUTINE nl_set_auxinput1_inname ( id_id , auxinput1_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput1_inname
  INTEGER id_id
  model_config_rec%auxinput1_inname = trim(auxinput1_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput1_inname
SUBROUTINE nl_set_io_form_auxinput1 ( id_id , io_form_auxinput1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput1
  INTEGER id_id
  model_config_rec%io_form_auxinput1 = io_form_auxinput1 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput1
SUBROUTINE nl_set_override_restart_timers ( id_id , override_restart_timers )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: override_restart_timers
  INTEGER id_id
  model_config_rec%override_restart_timers = override_restart_timers 
  RETURN
END SUBROUTINE nl_set_override_restart_timers
SUBROUTINE nl_set_auxhist1_inname ( id_id , auxhist1_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist1_inname
  INTEGER id_id
  model_config_rec%auxhist1_inname = trim(auxhist1_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist1_inname
SUBROUTINE nl_set_auxhist1_outname ( id_id , auxhist1_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist1_outname
  INTEGER id_id
  model_config_rec%auxhist1_outname = trim(auxhist1_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist1_outname
SUBROUTINE nl_set_auxhist1_interval_y ( id_id , auxhist1_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_interval_y
  INTEGER id_id
  model_config_rec%auxhist1_interval_y(id_id) = auxhist1_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_y
SUBROUTINE nl_set_auxhist1_interval_d ( id_id , auxhist1_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_interval_d
  INTEGER id_id
  model_config_rec%auxhist1_interval_d(id_id) = auxhist1_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_d
SUBROUTINE nl_set_auxhist1_interval_h ( id_id , auxhist1_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_interval_h
  INTEGER id_id
  model_config_rec%auxhist1_interval_h(id_id) = auxhist1_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_h
SUBROUTINE nl_set_auxhist1_interval_m ( id_id , auxhist1_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_interval_m
  INTEGER id_id
  model_config_rec%auxhist1_interval_m(id_id) = auxhist1_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_m
SUBROUTINE nl_set_auxhist1_interval_s ( id_id , auxhist1_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_interval_s
  INTEGER id_id
  model_config_rec%auxhist1_interval_s(id_id) = auxhist1_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist1_interval_s
SUBROUTINE nl_set_auxhist1_interval ( id_id , auxhist1_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_interval
  INTEGER id_id
  model_config_rec%auxhist1_interval(id_id) = auxhist1_interval
  RETURN
END SUBROUTINE nl_set_auxhist1_interval
SUBROUTINE nl_set_auxhist1_begin_y ( id_id , auxhist1_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_begin_y
  INTEGER id_id
  model_config_rec%auxhist1_begin_y(id_id) = auxhist1_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_y
SUBROUTINE nl_set_auxhist1_begin_d ( id_id , auxhist1_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_begin_d
  INTEGER id_id
  model_config_rec%auxhist1_begin_d(id_id) = auxhist1_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_d
SUBROUTINE nl_set_auxhist1_begin_h ( id_id , auxhist1_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_begin_h
  INTEGER id_id
  model_config_rec%auxhist1_begin_h(id_id) = auxhist1_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_h
SUBROUTINE nl_set_auxhist1_begin_m ( id_id , auxhist1_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_begin_m
  INTEGER id_id
  model_config_rec%auxhist1_begin_m(id_id) = auxhist1_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_m
SUBROUTINE nl_set_auxhist1_begin_s ( id_id , auxhist1_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_begin_s
  INTEGER id_id
  model_config_rec%auxhist1_begin_s(id_id) = auxhist1_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist1_begin_s
SUBROUTINE nl_set_auxhist1_begin ( id_id , auxhist1_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_begin
  INTEGER id_id
  model_config_rec%auxhist1_begin(id_id) = auxhist1_begin
  RETURN
END SUBROUTINE nl_set_auxhist1_begin
SUBROUTINE nl_set_auxhist1_end_y ( id_id , auxhist1_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_end_y
  INTEGER id_id
  model_config_rec%auxhist1_end_y(id_id) = auxhist1_end_y
  RETURN
END SUBROUTINE nl_set_auxhist1_end_y
SUBROUTINE nl_set_auxhist1_end_d ( id_id , auxhist1_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_end_d
  INTEGER id_id
  model_config_rec%auxhist1_end_d(id_id) = auxhist1_end_d
  RETURN
END SUBROUTINE nl_set_auxhist1_end_d
SUBROUTINE nl_set_auxhist1_end_h ( id_id , auxhist1_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_end_h
  INTEGER id_id
  model_config_rec%auxhist1_end_h(id_id) = auxhist1_end_h
  RETURN
END SUBROUTINE nl_set_auxhist1_end_h
SUBROUTINE nl_set_auxhist1_end_m ( id_id , auxhist1_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_end_m
  INTEGER id_id
  model_config_rec%auxhist1_end_m(id_id) = auxhist1_end_m
  RETURN
END SUBROUTINE nl_set_auxhist1_end_m
SUBROUTINE nl_set_auxhist1_end_s ( id_id , auxhist1_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_end_s
  INTEGER id_id
  model_config_rec%auxhist1_end_s(id_id) = auxhist1_end_s
  RETURN
END SUBROUTINE nl_set_auxhist1_end_s
SUBROUTINE nl_set_auxhist1_end ( id_id , auxhist1_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist1_end
  INTEGER id_id
  model_config_rec%auxhist1_end(id_id) = auxhist1_end
  RETURN
END SUBROUTINE nl_set_auxhist1_end
SUBROUTINE nl_set_io_form_auxhist1 ( id_id , io_form_auxhist1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist1
  INTEGER id_id
  model_config_rec%io_form_auxhist1 = io_form_auxhist1 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist1
SUBROUTINE nl_set_frames_per_auxhist1 ( id_id , frames_per_auxhist1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist1
  INTEGER id_id
  model_config_rec%frames_per_auxhist1(id_id) = frames_per_auxhist1
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist1
SUBROUTINE nl_set_auxhist2_inname ( id_id , auxhist2_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist2_inname
  INTEGER id_id
  model_config_rec%auxhist2_inname = trim(auxhist2_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist2_inname
SUBROUTINE nl_set_auxhist2_outname ( id_id , auxhist2_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist2_outname
  INTEGER id_id
  model_config_rec%auxhist2_outname = trim(auxhist2_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist2_outname
SUBROUTINE nl_set_auxhist2_interval_y ( id_id , auxhist2_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_interval_y
  INTEGER id_id
  model_config_rec%auxhist2_interval_y(id_id) = auxhist2_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_y
SUBROUTINE nl_set_auxhist2_interval_d ( id_id , auxhist2_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_interval_d
  INTEGER id_id
  model_config_rec%auxhist2_interval_d(id_id) = auxhist2_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_d
SUBROUTINE nl_set_auxhist2_interval_h ( id_id , auxhist2_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_interval_h
  INTEGER id_id
  model_config_rec%auxhist2_interval_h(id_id) = auxhist2_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_h
SUBROUTINE nl_set_auxhist2_interval_m ( id_id , auxhist2_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_interval_m
  INTEGER id_id
  model_config_rec%auxhist2_interval_m(id_id) = auxhist2_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_m
SUBROUTINE nl_set_auxhist2_interval_s ( id_id , auxhist2_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_interval_s
  INTEGER id_id
  model_config_rec%auxhist2_interval_s(id_id) = auxhist2_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist2_interval_s
SUBROUTINE nl_set_auxhist2_interval ( id_id , auxhist2_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_interval
  INTEGER id_id
  model_config_rec%auxhist2_interval(id_id) = auxhist2_interval
  RETURN
END SUBROUTINE nl_set_auxhist2_interval
SUBROUTINE nl_set_auxhist2_begin_y ( id_id , auxhist2_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_begin_y
  INTEGER id_id
  model_config_rec%auxhist2_begin_y(id_id) = auxhist2_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_y
SUBROUTINE nl_set_auxhist2_begin_d ( id_id , auxhist2_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_begin_d
  INTEGER id_id
  model_config_rec%auxhist2_begin_d(id_id) = auxhist2_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_d
SUBROUTINE nl_set_auxhist2_begin_h ( id_id , auxhist2_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_begin_h
  INTEGER id_id
  model_config_rec%auxhist2_begin_h(id_id) = auxhist2_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_h
SUBROUTINE nl_set_auxhist2_begin_m ( id_id , auxhist2_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_begin_m
  INTEGER id_id
  model_config_rec%auxhist2_begin_m(id_id) = auxhist2_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_m
SUBROUTINE nl_set_auxhist2_begin_s ( id_id , auxhist2_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_begin_s
  INTEGER id_id
  model_config_rec%auxhist2_begin_s(id_id) = auxhist2_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist2_begin_s
SUBROUTINE nl_set_auxhist2_begin ( id_id , auxhist2_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_begin
  INTEGER id_id
  model_config_rec%auxhist2_begin(id_id) = auxhist2_begin
  RETURN
END SUBROUTINE nl_set_auxhist2_begin
SUBROUTINE nl_set_auxhist2_end_y ( id_id , auxhist2_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_end_y
  INTEGER id_id
  model_config_rec%auxhist2_end_y(id_id) = auxhist2_end_y
  RETURN
END SUBROUTINE nl_set_auxhist2_end_y
SUBROUTINE nl_set_auxhist2_end_d ( id_id , auxhist2_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_end_d
  INTEGER id_id
  model_config_rec%auxhist2_end_d(id_id) = auxhist2_end_d
  RETURN
END SUBROUTINE nl_set_auxhist2_end_d
SUBROUTINE nl_set_auxhist2_end_h ( id_id , auxhist2_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_end_h
  INTEGER id_id
  model_config_rec%auxhist2_end_h(id_id) = auxhist2_end_h
  RETURN
END SUBROUTINE nl_set_auxhist2_end_h
SUBROUTINE nl_set_auxhist2_end_m ( id_id , auxhist2_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_end_m
  INTEGER id_id
  model_config_rec%auxhist2_end_m(id_id) = auxhist2_end_m
  RETURN
END SUBROUTINE nl_set_auxhist2_end_m
SUBROUTINE nl_set_auxhist2_end_s ( id_id , auxhist2_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_end_s
  INTEGER id_id
  model_config_rec%auxhist2_end_s(id_id) = auxhist2_end_s
  RETURN
END SUBROUTINE nl_set_auxhist2_end_s
SUBROUTINE nl_set_auxhist2_end ( id_id , auxhist2_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist2_end
  INTEGER id_id
  model_config_rec%auxhist2_end(id_id) = auxhist2_end
  RETURN
END SUBROUTINE nl_set_auxhist2_end
SUBROUTINE nl_set_io_form_auxhist2 ( id_id , io_form_auxhist2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist2
  INTEGER id_id
  model_config_rec%io_form_auxhist2 = io_form_auxhist2 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist2
SUBROUTINE nl_set_frames_per_auxhist2 ( id_id , frames_per_auxhist2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist2
  INTEGER id_id
  model_config_rec%frames_per_auxhist2(id_id) = frames_per_auxhist2
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist2
SUBROUTINE nl_set_auxhist3_inname ( id_id , auxhist3_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist3_inname
  INTEGER id_id
  model_config_rec%auxhist3_inname = trim(auxhist3_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist3_inname
SUBROUTINE nl_set_auxhist3_outname ( id_id , auxhist3_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist3_outname
  INTEGER id_id
  model_config_rec%auxhist3_outname = trim(auxhist3_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist3_outname
SUBROUTINE nl_set_auxhist3_interval_y ( id_id , auxhist3_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_interval_y
  INTEGER id_id
  model_config_rec%auxhist3_interval_y(id_id) = auxhist3_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_y
SUBROUTINE nl_set_auxhist3_interval_d ( id_id , auxhist3_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_interval_d
  INTEGER id_id
  model_config_rec%auxhist3_interval_d(id_id) = auxhist3_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_d
SUBROUTINE nl_set_auxhist3_interval_h ( id_id , auxhist3_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_interval_h
  INTEGER id_id
  model_config_rec%auxhist3_interval_h(id_id) = auxhist3_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_h
SUBROUTINE nl_set_auxhist3_interval_m ( id_id , auxhist3_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_interval_m
  INTEGER id_id
  model_config_rec%auxhist3_interval_m(id_id) = auxhist3_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_m
SUBROUTINE nl_set_auxhist3_interval_s ( id_id , auxhist3_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_interval_s
  INTEGER id_id
  model_config_rec%auxhist3_interval_s(id_id) = auxhist3_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist3_interval_s
SUBROUTINE nl_set_auxhist3_interval ( id_id , auxhist3_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_interval
  INTEGER id_id
  model_config_rec%auxhist3_interval(id_id) = auxhist3_interval
  RETURN
END SUBROUTINE nl_set_auxhist3_interval
SUBROUTINE nl_set_auxhist3_begin_y ( id_id , auxhist3_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_begin_y
  INTEGER id_id
  model_config_rec%auxhist3_begin_y(id_id) = auxhist3_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_y
SUBROUTINE nl_set_auxhist3_begin_d ( id_id , auxhist3_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_begin_d
  INTEGER id_id
  model_config_rec%auxhist3_begin_d(id_id) = auxhist3_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_d
SUBROUTINE nl_set_auxhist3_begin_h ( id_id , auxhist3_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_begin_h
  INTEGER id_id
  model_config_rec%auxhist3_begin_h(id_id) = auxhist3_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_h
SUBROUTINE nl_set_auxhist3_begin_m ( id_id , auxhist3_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_begin_m
  INTEGER id_id
  model_config_rec%auxhist3_begin_m(id_id) = auxhist3_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_m
SUBROUTINE nl_set_auxhist3_begin_s ( id_id , auxhist3_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_begin_s
  INTEGER id_id
  model_config_rec%auxhist3_begin_s(id_id) = auxhist3_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist3_begin_s
SUBROUTINE nl_set_auxhist3_begin ( id_id , auxhist3_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_begin
  INTEGER id_id
  model_config_rec%auxhist3_begin(id_id) = auxhist3_begin
  RETURN
END SUBROUTINE nl_set_auxhist3_begin
SUBROUTINE nl_set_auxhist3_end_y ( id_id , auxhist3_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_end_y
  INTEGER id_id
  model_config_rec%auxhist3_end_y(id_id) = auxhist3_end_y
  RETURN
END SUBROUTINE nl_set_auxhist3_end_y
SUBROUTINE nl_set_auxhist3_end_d ( id_id , auxhist3_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_end_d
  INTEGER id_id
  model_config_rec%auxhist3_end_d(id_id) = auxhist3_end_d
  RETURN
END SUBROUTINE nl_set_auxhist3_end_d
SUBROUTINE nl_set_auxhist3_end_h ( id_id , auxhist3_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_end_h
  INTEGER id_id
  model_config_rec%auxhist3_end_h(id_id) = auxhist3_end_h
  RETURN
END SUBROUTINE nl_set_auxhist3_end_h
SUBROUTINE nl_set_auxhist3_end_m ( id_id , auxhist3_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_end_m
  INTEGER id_id
  model_config_rec%auxhist3_end_m(id_id) = auxhist3_end_m
  RETURN
END SUBROUTINE nl_set_auxhist3_end_m
SUBROUTINE nl_set_auxhist3_end_s ( id_id , auxhist3_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_end_s
  INTEGER id_id
  model_config_rec%auxhist3_end_s(id_id) = auxhist3_end_s
  RETURN
END SUBROUTINE nl_set_auxhist3_end_s
SUBROUTINE nl_set_auxhist3_end ( id_id , auxhist3_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist3_end
  INTEGER id_id
  model_config_rec%auxhist3_end(id_id) = auxhist3_end
  RETURN
END SUBROUTINE nl_set_auxhist3_end
SUBROUTINE nl_set_io_form_auxhist3 ( id_id , io_form_auxhist3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist3
  INTEGER id_id
  model_config_rec%io_form_auxhist3 = io_form_auxhist3 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist3
SUBROUTINE nl_set_frames_per_auxhist3 ( id_id , frames_per_auxhist3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist3
  INTEGER id_id
  model_config_rec%frames_per_auxhist3(id_id) = frames_per_auxhist3
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist3
SUBROUTINE nl_set_auxhist4_inname ( id_id , auxhist4_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist4_inname
  INTEGER id_id
  model_config_rec%auxhist4_inname = trim(auxhist4_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist4_inname
SUBROUTINE nl_set_auxhist4_outname ( id_id , auxhist4_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist4_outname
  INTEGER id_id
  model_config_rec%auxhist4_outname = trim(auxhist4_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist4_outname
SUBROUTINE nl_set_auxhist4_interval_y ( id_id , auxhist4_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_interval_y
  INTEGER id_id
  model_config_rec%auxhist4_interval_y(id_id) = auxhist4_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_y
SUBROUTINE nl_set_auxhist4_interval_d ( id_id , auxhist4_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_interval_d
  INTEGER id_id
  model_config_rec%auxhist4_interval_d(id_id) = auxhist4_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_d
SUBROUTINE nl_set_auxhist4_interval_h ( id_id , auxhist4_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_interval_h
  INTEGER id_id
  model_config_rec%auxhist4_interval_h(id_id) = auxhist4_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_h
SUBROUTINE nl_set_auxhist4_interval_m ( id_id , auxhist4_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_interval_m
  INTEGER id_id
  model_config_rec%auxhist4_interval_m(id_id) = auxhist4_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_m
SUBROUTINE nl_set_auxhist4_interval_s ( id_id , auxhist4_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_interval_s
  INTEGER id_id
  model_config_rec%auxhist4_interval_s(id_id) = auxhist4_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist4_interval_s
SUBROUTINE nl_set_auxhist4_interval ( id_id , auxhist4_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_interval
  INTEGER id_id
  model_config_rec%auxhist4_interval(id_id) = auxhist4_interval
  RETURN
END SUBROUTINE nl_set_auxhist4_interval
SUBROUTINE nl_set_auxhist4_begin_y ( id_id , auxhist4_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_begin_y
  INTEGER id_id
  model_config_rec%auxhist4_begin_y(id_id) = auxhist4_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_y
SUBROUTINE nl_set_auxhist4_begin_d ( id_id , auxhist4_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_begin_d
  INTEGER id_id
  model_config_rec%auxhist4_begin_d(id_id) = auxhist4_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_d
SUBROUTINE nl_set_auxhist4_begin_h ( id_id , auxhist4_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_begin_h
  INTEGER id_id
  model_config_rec%auxhist4_begin_h(id_id) = auxhist4_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_h
SUBROUTINE nl_set_auxhist4_begin_m ( id_id , auxhist4_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_begin_m
  INTEGER id_id
  model_config_rec%auxhist4_begin_m(id_id) = auxhist4_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_m
SUBROUTINE nl_set_auxhist4_begin_s ( id_id , auxhist4_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_begin_s
  INTEGER id_id
  model_config_rec%auxhist4_begin_s(id_id) = auxhist4_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist4_begin_s
SUBROUTINE nl_set_auxhist4_begin ( id_id , auxhist4_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_begin
  INTEGER id_id
  model_config_rec%auxhist4_begin(id_id) = auxhist4_begin
  RETURN
END SUBROUTINE nl_set_auxhist4_begin
SUBROUTINE nl_set_auxhist4_end_y ( id_id , auxhist4_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_end_y
  INTEGER id_id
  model_config_rec%auxhist4_end_y(id_id) = auxhist4_end_y
  RETURN
END SUBROUTINE nl_set_auxhist4_end_y
SUBROUTINE nl_set_auxhist4_end_d ( id_id , auxhist4_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_end_d
  INTEGER id_id
  model_config_rec%auxhist4_end_d(id_id) = auxhist4_end_d
  RETURN
END SUBROUTINE nl_set_auxhist4_end_d
SUBROUTINE nl_set_auxhist4_end_h ( id_id , auxhist4_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_end_h
  INTEGER id_id
  model_config_rec%auxhist4_end_h(id_id) = auxhist4_end_h
  RETURN
END SUBROUTINE nl_set_auxhist4_end_h
SUBROUTINE nl_set_auxhist4_end_m ( id_id , auxhist4_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_end_m
  INTEGER id_id
  model_config_rec%auxhist4_end_m(id_id) = auxhist4_end_m
  RETURN
END SUBROUTINE nl_set_auxhist4_end_m
SUBROUTINE nl_set_auxhist4_end_s ( id_id , auxhist4_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_end_s
  INTEGER id_id
  model_config_rec%auxhist4_end_s(id_id) = auxhist4_end_s
  RETURN
END SUBROUTINE nl_set_auxhist4_end_s
SUBROUTINE nl_set_auxhist4_end ( id_id , auxhist4_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist4_end
  INTEGER id_id
  model_config_rec%auxhist4_end(id_id) = auxhist4_end
  RETURN
END SUBROUTINE nl_set_auxhist4_end
SUBROUTINE nl_set_io_form_auxhist4 ( id_id , io_form_auxhist4 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist4
  INTEGER id_id
  model_config_rec%io_form_auxhist4 = io_form_auxhist4 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist4
SUBROUTINE nl_set_frames_per_auxhist4 ( id_id , frames_per_auxhist4 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist4
  INTEGER id_id
  model_config_rec%frames_per_auxhist4(id_id) = frames_per_auxhist4
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist4
SUBROUTINE nl_set_auxhist5_inname ( id_id , auxhist5_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist5_inname
  INTEGER id_id
  model_config_rec%auxhist5_inname = trim(auxhist5_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist5_inname
SUBROUTINE nl_set_auxhist5_outname ( id_id , auxhist5_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist5_outname
  INTEGER id_id
  model_config_rec%auxhist5_outname = trim(auxhist5_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist5_outname
SUBROUTINE nl_set_auxhist5_interval_y ( id_id , auxhist5_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_interval_y
  INTEGER id_id
  model_config_rec%auxhist5_interval_y(id_id) = auxhist5_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist5_interval_y
SUBROUTINE nl_set_auxhist5_interval_d ( id_id , auxhist5_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_interval_d
  INTEGER id_id
  model_config_rec%auxhist5_interval_d(id_id) = auxhist5_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist5_interval_d
SUBROUTINE nl_set_auxhist5_interval_h ( id_id , auxhist5_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_interval_h
  INTEGER id_id
  model_config_rec%auxhist5_interval_h(id_id) = auxhist5_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist5_interval_h
SUBROUTINE nl_set_auxhist5_interval_m ( id_id , auxhist5_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_interval_m
  INTEGER id_id
  model_config_rec%auxhist5_interval_m(id_id) = auxhist5_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist5_interval_m
SUBROUTINE nl_set_auxhist5_interval_s ( id_id , auxhist5_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_interval_s
  INTEGER id_id
  model_config_rec%auxhist5_interval_s(id_id) = auxhist5_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist5_interval_s
SUBROUTINE nl_set_auxhist5_interval ( id_id , auxhist5_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_interval
  INTEGER id_id
  model_config_rec%auxhist5_interval(id_id) = auxhist5_interval
  RETURN
END SUBROUTINE nl_set_auxhist5_interval
SUBROUTINE nl_set_auxhist5_begin_y ( id_id , auxhist5_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_begin_y
  INTEGER id_id
  model_config_rec%auxhist5_begin_y(id_id) = auxhist5_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_y
SUBROUTINE nl_set_auxhist5_begin_d ( id_id , auxhist5_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_begin_d
  INTEGER id_id
  model_config_rec%auxhist5_begin_d(id_id) = auxhist5_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_d
SUBROUTINE nl_set_auxhist5_begin_h ( id_id , auxhist5_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_begin_h
  INTEGER id_id
  model_config_rec%auxhist5_begin_h(id_id) = auxhist5_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_h
SUBROUTINE nl_set_auxhist5_begin_m ( id_id , auxhist5_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_begin_m
  INTEGER id_id
  model_config_rec%auxhist5_begin_m(id_id) = auxhist5_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_m
SUBROUTINE nl_set_auxhist5_begin_s ( id_id , auxhist5_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_begin_s
  INTEGER id_id
  model_config_rec%auxhist5_begin_s(id_id) = auxhist5_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist5_begin_s
SUBROUTINE nl_set_auxhist5_begin ( id_id , auxhist5_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_begin
  INTEGER id_id
  model_config_rec%auxhist5_begin(id_id) = auxhist5_begin
  RETURN
END SUBROUTINE nl_set_auxhist5_begin
SUBROUTINE nl_set_auxhist5_end_y ( id_id , auxhist5_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_end_y
  INTEGER id_id
  model_config_rec%auxhist5_end_y(id_id) = auxhist5_end_y
  RETURN
END SUBROUTINE nl_set_auxhist5_end_y
SUBROUTINE nl_set_auxhist5_end_d ( id_id , auxhist5_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_end_d
  INTEGER id_id
  model_config_rec%auxhist5_end_d(id_id) = auxhist5_end_d
  RETURN
END SUBROUTINE nl_set_auxhist5_end_d
SUBROUTINE nl_set_auxhist5_end_h ( id_id , auxhist5_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_end_h
  INTEGER id_id
  model_config_rec%auxhist5_end_h(id_id) = auxhist5_end_h
  RETURN
END SUBROUTINE nl_set_auxhist5_end_h
SUBROUTINE nl_set_auxhist5_end_m ( id_id , auxhist5_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_end_m
  INTEGER id_id
  model_config_rec%auxhist5_end_m(id_id) = auxhist5_end_m
  RETURN
END SUBROUTINE nl_set_auxhist5_end_m
SUBROUTINE nl_set_auxhist5_end_s ( id_id , auxhist5_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_end_s
  INTEGER id_id
  model_config_rec%auxhist5_end_s(id_id) = auxhist5_end_s
  RETURN
END SUBROUTINE nl_set_auxhist5_end_s
SUBROUTINE nl_set_auxhist5_end ( id_id , auxhist5_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist5_end
  INTEGER id_id
  model_config_rec%auxhist5_end(id_id) = auxhist5_end
  RETURN
END SUBROUTINE nl_set_auxhist5_end
SUBROUTINE nl_set_io_form_auxhist5 ( id_id , io_form_auxhist5 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist5
  INTEGER id_id
  model_config_rec%io_form_auxhist5 = io_form_auxhist5 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist5
SUBROUTINE nl_set_frames_per_auxhist5 ( id_id , frames_per_auxhist5 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist5
  INTEGER id_id
  model_config_rec%frames_per_auxhist5(id_id) = frames_per_auxhist5
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist5
#endif
#if (NNN == 1)
SUBROUTINE nl_set_auxhist6_inname ( id_id , auxhist6_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist6_inname
  INTEGER id_id
  model_config_rec%auxhist6_inname = trim(auxhist6_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist6_inname
SUBROUTINE nl_set_auxhist6_outname ( id_id , auxhist6_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist6_outname
  INTEGER id_id
  model_config_rec%auxhist6_outname = trim(auxhist6_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist6_outname
SUBROUTINE nl_set_auxhist6_interval_y ( id_id , auxhist6_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_interval_y
  INTEGER id_id
  model_config_rec%auxhist6_interval_y(id_id) = auxhist6_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist6_interval_y
SUBROUTINE nl_set_auxhist6_interval_d ( id_id , auxhist6_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_interval_d
  INTEGER id_id
  model_config_rec%auxhist6_interval_d(id_id) = auxhist6_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist6_interval_d
SUBROUTINE nl_set_auxhist6_interval_h ( id_id , auxhist6_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_interval_h
  INTEGER id_id
  model_config_rec%auxhist6_interval_h(id_id) = auxhist6_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist6_interval_h
SUBROUTINE nl_set_auxhist6_interval_m ( id_id , auxhist6_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_interval_m
  INTEGER id_id
  model_config_rec%auxhist6_interval_m(id_id) = auxhist6_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist6_interval_m
SUBROUTINE nl_set_auxhist6_interval_s ( id_id , auxhist6_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_interval_s
  INTEGER id_id
  model_config_rec%auxhist6_interval_s(id_id) = auxhist6_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist6_interval_s
SUBROUTINE nl_set_auxhist6_interval ( id_id , auxhist6_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_interval
  INTEGER id_id
  model_config_rec%auxhist6_interval(id_id) = auxhist6_interval
  RETURN
END SUBROUTINE nl_set_auxhist6_interval
SUBROUTINE nl_set_auxhist6_begin_y ( id_id , auxhist6_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_begin_y
  INTEGER id_id
  model_config_rec%auxhist6_begin_y(id_id) = auxhist6_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist6_begin_y
SUBROUTINE nl_set_auxhist6_begin_d ( id_id , auxhist6_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_begin_d
  INTEGER id_id
  model_config_rec%auxhist6_begin_d(id_id) = auxhist6_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist6_begin_d
SUBROUTINE nl_set_auxhist6_begin_h ( id_id , auxhist6_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_begin_h
  INTEGER id_id
  model_config_rec%auxhist6_begin_h(id_id) = auxhist6_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist6_begin_h
SUBROUTINE nl_set_auxhist6_begin_m ( id_id , auxhist6_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_begin_m
  INTEGER id_id
  model_config_rec%auxhist6_begin_m(id_id) = auxhist6_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist6_begin_m
SUBROUTINE nl_set_auxhist6_begin_s ( id_id , auxhist6_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_begin_s
  INTEGER id_id
  model_config_rec%auxhist6_begin_s(id_id) = auxhist6_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist6_begin_s
SUBROUTINE nl_set_auxhist6_begin ( id_id , auxhist6_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_begin
  INTEGER id_id
  model_config_rec%auxhist6_begin(id_id) = auxhist6_begin
  RETURN
END SUBROUTINE nl_set_auxhist6_begin
SUBROUTINE nl_set_auxhist6_end_y ( id_id , auxhist6_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_end_y
  INTEGER id_id
  model_config_rec%auxhist6_end_y(id_id) = auxhist6_end_y
  RETURN
END SUBROUTINE nl_set_auxhist6_end_y
SUBROUTINE nl_set_auxhist6_end_d ( id_id , auxhist6_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_end_d
  INTEGER id_id
  model_config_rec%auxhist6_end_d(id_id) = auxhist6_end_d
  RETURN
END SUBROUTINE nl_set_auxhist6_end_d
SUBROUTINE nl_set_auxhist6_end_h ( id_id , auxhist6_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_end_h
  INTEGER id_id
  model_config_rec%auxhist6_end_h(id_id) = auxhist6_end_h
  RETURN
END SUBROUTINE nl_set_auxhist6_end_h
SUBROUTINE nl_set_auxhist6_end_m ( id_id , auxhist6_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_end_m
  INTEGER id_id
  model_config_rec%auxhist6_end_m(id_id) = auxhist6_end_m
  RETURN
END SUBROUTINE nl_set_auxhist6_end_m
SUBROUTINE nl_set_auxhist6_end_s ( id_id , auxhist6_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_end_s
  INTEGER id_id
  model_config_rec%auxhist6_end_s(id_id) = auxhist6_end_s
  RETURN
END SUBROUTINE nl_set_auxhist6_end_s
SUBROUTINE nl_set_auxhist6_end ( id_id , auxhist6_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist6_end
  INTEGER id_id
  model_config_rec%auxhist6_end(id_id) = auxhist6_end
  RETURN
END SUBROUTINE nl_set_auxhist6_end
SUBROUTINE nl_set_io_form_auxhist6 ( id_id , io_form_auxhist6 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist6
  INTEGER id_id
  model_config_rec%io_form_auxhist6 = io_form_auxhist6 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist6
SUBROUTINE nl_set_frames_per_auxhist6 ( id_id , frames_per_auxhist6 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist6
  INTEGER id_id
  model_config_rec%frames_per_auxhist6(id_id) = frames_per_auxhist6
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist6
SUBROUTINE nl_set_auxhist7_inname ( id_id , auxhist7_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist7_inname
  INTEGER id_id
  model_config_rec%auxhist7_inname = trim(auxhist7_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist7_inname
SUBROUTINE nl_set_auxhist7_outname ( id_id , auxhist7_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist7_outname
  INTEGER id_id
  model_config_rec%auxhist7_outname = trim(auxhist7_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist7_outname
SUBROUTINE nl_set_auxhist7_interval_y ( id_id , auxhist7_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_interval_y
  INTEGER id_id
  model_config_rec%auxhist7_interval_y(id_id) = auxhist7_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist7_interval_y
SUBROUTINE nl_set_auxhist7_interval_d ( id_id , auxhist7_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_interval_d
  INTEGER id_id
  model_config_rec%auxhist7_interval_d(id_id) = auxhist7_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist7_interval_d
SUBROUTINE nl_set_auxhist7_interval_h ( id_id , auxhist7_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_interval_h
  INTEGER id_id
  model_config_rec%auxhist7_interval_h(id_id) = auxhist7_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist7_interval_h
SUBROUTINE nl_set_auxhist7_interval_m ( id_id , auxhist7_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_interval_m
  INTEGER id_id
  model_config_rec%auxhist7_interval_m(id_id) = auxhist7_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist7_interval_m
SUBROUTINE nl_set_auxhist7_interval_s ( id_id , auxhist7_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_interval_s
  INTEGER id_id
  model_config_rec%auxhist7_interval_s(id_id) = auxhist7_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist7_interval_s
SUBROUTINE nl_set_auxhist7_interval ( id_id , auxhist7_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_interval
  INTEGER id_id
  model_config_rec%auxhist7_interval(id_id) = auxhist7_interval
  RETURN
END SUBROUTINE nl_set_auxhist7_interval
SUBROUTINE nl_set_auxhist7_begin_y ( id_id , auxhist7_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_begin_y
  INTEGER id_id
  model_config_rec%auxhist7_begin_y(id_id) = auxhist7_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist7_begin_y
SUBROUTINE nl_set_auxhist7_begin_d ( id_id , auxhist7_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_begin_d
  INTEGER id_id
  model_config_rec%auxhist7_begin_d(id_id) = auxhist7_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist7_begin_d
SUBROUTINE nl_set_auxhist7_begin_h ( id_id , auxhist7_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_begin_h
  INTEGER id_id
  model_config_rec%auxhist7_begin_h(id_id) = auxhist7_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist7_begin_h
SUBROUTINE nl_set_auxhist7_begin_m ( id_id , auxhist7_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_begin_m
  INTEGER id_id
  model_config_rec%auxhist7_begin_m(id_id) = auxhist7_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist7_begin_m
SUBROUTINE nl_set_auxhist7_begin_s ( id_id , auxhist7_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_begin_s
  INTEGER id_id
  model_config_rec%auxhist7_begin_s(id_id) = auxhist7_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist7_begin_s
SUBROUTINE nl_set_auxhist7_begin ( id_id , auxhist7_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_begin
  INTEGER id_id
  model_config_rec%auxhist7_begin(id_id) = auxhist7_begin
  RETURN
END SUBROUTINE nl_set_auxhist7_begin
SUBROUTINE nl_set_auxhist7_end_y ( id_id , auxhist7_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_end_y
  INTEGER id_id
  model_config_rec%auxhist7_end_y(id_id) = auxhist7_end_y
  RETURN
END SUBROUTINE nl_set_auxhist7_end_y
SUBROUTINE nl_set_auxhist7_end_d ( id_id , auxhist7_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_end_d
  INTEGER id_id
  model_config_rec%auxhist7_end_d(id_id) = auxhist7_end_d
  RETURN
END SUBROUTINE nl_set_auxhist7_end_d
SUBROUTINE nl_set_auxhist7_end_h ( id_id , auxhist7_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_end_h
  INTEGER id_id
  model_config_rec%auxhist7_end_h(id_id) = auxhist7_end_h
  RETURN
END SUBROUTINE nl_set_auxhist7_end_h
SUBROUTINE nl_set_auxhist7_end_m ( id_id , auxhist7_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_end_m
  INTEGER id_id
  model_config_rec%auxhist7_end_m(id_id) = auxhist7_end_m
  RETURN
END SUBROUTINE nl_set_auxhist7_end_m
SUBROUTINE nl_set_auxhist7_end_s ( id_id , auxhist7_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_end_s
  INTEGER id_id
  model_config_rec%auxhist7_end_s(id_id) = auxhist7_end_s
  RETURN
END SUBROUTINE nl_set_auxhist7_end_s
SUBROUTINE nl_set_auxhist7_end ( id_id , auxhist7_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist7_end
  INTEGER id_id
  model_config_rec%auxhist7_end(id_id) = auxhist7_end
  RETURN
END SUBROUTINE nl_set_auxhist7_end
SUBROUTINE nl_set_io_form_auxhist7 ( id_id , io_form_auxhist7 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist7
  INTEGER id_id
  model_config_rec%io_form_auxhist7 = io_form_auxhist7 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist7
SUBROUTINE nl_set_frames_per_auxhist7 ( id_id , frames_per_auxhist7 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist7
  INTEGER id_id
  model_config_rec%frames_per_auxhist7(id_id) = frames_per_auxhist7
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist7
SUBROUTINE nl_set_auxhist8_inname ( id_id , auxhist8_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist8_inname
  INTEGER id_id
  model_config_rec%auxhist8_inname = trim(auxhist8_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist8_inname
SUBROUTINE nl_set_auxhist8_outname ( id_id , auxhist8_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist8_outname
  INTEGER id_id
  model_config_rec%auxhist8_outname = trim(auxhist8_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist8_outname
SUBROUTINE nl_set_auxhist8_interval_y ( id_id , auxhist8_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_interval_y
  INTEGER id_id
  model_config_rec%auxhist8_interval_y(id_id) = auxhist8_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist8_interval_y
SUBROUTINE nl_set_auxhist8_interval_d ( id_id , auxhist8_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_interval_d
  INTEGER id_id
  model_config_rec%auxhist8_interval_d(id_id) = auxhist8_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist8_interval_d
SUBROUTINE nl_set_auxhist8_interval_h ( id_id , auxhist8_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_interval_h
  INTEGER id_id
  model_config_rec%auxhist8_interval_h(id_id) = auxhist8_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist8_interval_h
SUBROUTINE nl_set_auxhist8_interval_m ( id_id , auxhist8_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_interval_m
  INTEGER id_id
  model_config_rec%auxhist8_interval_m(id_id) = auxhist8_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist8_interval_m
SUBROUTINE nl_set_auxhist8_interval_s ( id_id , auxhist8_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_interval_s
  INTEGER id_id
  model_config_rec%auxhist8_interval_s(id_id) = auxhist8_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist8_interval_s
SUBROUTINE nl_set_auxhist8_interval ( id_id , auxhist8_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_interval
  INTEGER id_id
  model_config_rec%auxhist8_interval(id_id) = auxhist8_interval
  RETURN
END SUBROUTINE nl_set_auxhist8_interval
SUBROUTINE nl_set_auxhist8_begin_y ( id_id , auxhist8_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_begin_y
  INTEGER id_id
  model_config_rec%auxhist8_begin_y(id_id) = auxhist8_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist8_begin_y
SUBROUTINE nl_set_auxhist8_begin_d ( id_id , auxhist8_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_begin_d
  INTEGER id_id
  model_config_rec%auxhist8_begin_d(id_id) = auxhist8_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist8_begin_d
SUBROUTINE nl_set_auxhist8_begin_h ( id_id , auxhist8_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_begin_h
  INTEGER id_id
  model_config_rec%auxhist8_begin_h(id_id) = auxhist8_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist8_begin_h
SUBROUTINE nl_set_auxhist8_begin_m ( id_id , auxhist8_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_begin_m
  INTEGER id_id
  model_config_rec%auxhist8_begin_m(id_id) = auxhist8_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist8_begin_m
SUBROUTINE nl_set_auxhist8_begin_s ( id_id , auxhist8_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_begin_s
  INTEGER id_id
  model_config_rec%auxhist8_begin_s(id_id) = auxhist8_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist8_begin_s
SUBROUTINE nl_set_auxhist8_begin ( id_id , auxhist8_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_begin
  INTEGER id_id
  model_config_rec%auxhist8_begin(id_id) = auxhist8_begin
  RETURN
END SUBROUTINE nl_set_auxhist8_begin
SUBROUTINE nl_set_auxhist8_end_y ( id_id , auxhist8_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_end_y
  INTEGER id_id
  model_config_rec%auxhist8_end_y(id_id) = auxhist8_end_y
  RETURN
END SUBROUTINE nl_set_auxhist8_end_y
SUBROUTINE nl_set_auxhist8_end_d ( id_id , auxhist8_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_end_d
  INTEGER id_id
  model_config_rec%auxhist8_end_d(id_id) = auxhist8_end_d
  RETURN
END SUBROUTINE nl_set_auxhist8_end_d
SUBROUTINE nl_set_auxhist8_end_h ( id_id , auxhist8_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_end_h
  INTEGER id_id
  model_config_rec%auxhist8_end_h(id_id) = auxhist8_end_h
  RETURN
END SUBROUTINE nl_set_auxhist8_end_h
SUBROUTINE nl_set_auxhist8_end_m ( id_id , auxhist8_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_end_m
  INTEGER id_id
  model_config_rec%auxhist8_end_m(id_id) = auxhist8_end_m
  RETURN
END SUBROUTINE nl_set_auxhist8_end_m
SUBROUTINE nl_set_auxhist8_end_s ( id_id , auxhist8_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_end_s
  INTEGER id_id
  model_config_rec%auxhist8_end_s(id_id) = auxhist8_end_s
  RETURN
END SUBROUTINE nl_set_auxhist8_end_s
SUBROUTINE nl_set_auxhist8_end ( id_id , auxhist8_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist8_end
  INTEGER id_id
  model_config_rec%auxhist8_end(id_id) = auxhist8_end
  RETURN
END SUBROUTINE nl_set_auxhist8_end
SUBROUTINE nl_set_io_form_auxhist8 ( id_id , io_form_auxhist8 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist8
  INTEGER id_id
  model_config_rec%io_form_auxhist8 = io_form_auxhist8 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist8
SUBROUTINE nl_set_frames_per_auxhist8 ( id_id , frames_per_auxhist8 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist8
  INTEGER id_id
  model_config_rec%frames_per_auxhist8(id_id) = frames_per_auxhist8
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist8
SUBROUTINE nl_set_auxhist9_inname ( id_id , auxhist9_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist9_inname
  INTEGER id_id
  model_config_rec%auxhist9_inname = trim(auxhist9_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist9_inname
SUBROUTINE nl_set_auxhist9_outname ( id_id , auxhist9_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist9_outname
  INTEGER id_id
  model_config_rec%auxhist9_outname = trim(auxhist9_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist9_outname
SUBROUTINE nl_set_auxhist9_interval_y ( id_id , auxhist9_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_interval_y
  INTEGER id_id
  model_config_rec%auxhist9_interval_y(id_id) = auxhist9_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist9_interval_y
SUBROUTINE nl_set_auxhist9_interval_d ( id_id , auxhist9_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_interval_d
  INTEGER id_id
  model_config_rec%auxhist9_interval_d(id_id) = auxhist9_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist9_interval_d
SUBROUTINE nl_set_auxhist9_interval_h ( id_id , auxhist9_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_interval_h
  INTEGER id_id
  model_config_rec%auxhist9_interval_h(id_id) = auxhist9_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist9_interval_h
SUBROUTINE nl_set_auxhist9_interval_m ( id_id , auxhist9_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_interval_m
  INTEGER id_id
  model_config_rec%auxhist9_interval_m(id_id) = auxhist9_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist9_interval_m
SUBROUTINE nl_set_auxhist9_interval_s ( id_id , auxhist9_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_interval_s
  INTEGER id_id
  model_config_rec%auxhist9_interval_s(id_id) = auxhist9_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist9_interval_s
SUBROUTINE nl_set_auxhist9_interval ( id_id , auxhist9_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_interval
  INTEGER id_id
  model_config_rec%auxhist9_interval(id_id) = auxhist9_interval
  RETURN
END SUBROUTINE nl_set_auxhist9_interval
SUBROUTINE nl_set_auxhist9_begin_y ( id_id , auxhist9_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_begin_y
  INTEGER id_id
  model_config_rec%auxhist9_begin_y(id_id) = auxhist9_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist9_begin_y
SUBROUTINE nl_set_auxhist9_begin_d ( id_id , auxhist9_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_begin_d
  INTEGER id_id
  model_config_rec%auxhist9_begin_d(id_id) = auxhist9_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist9_begin_d
SUBROUTINE nl_set_auxhist9_begin_h ( id_id , auxhist9_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_begin_h
  INTEGER id_id
  model_config_rec%auxhist9_begin_h(id_id) = auxhist9_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist9_begin_h
SUBROUTINE nl_set_auxhist9_begin_m ( id_id , auxhist9_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_begin_m
  INTEGER id_id
  model_config_rec%auxhist9_begin_m(id_id) = auxhist9_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist9_begin_m
SUBROUTINE nl_set_auxhist9_begin_s ( id_id , auxhist9_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_begin_s
  INTEGER id_id
  model_config_rec%auxhist9_begin_s(id_id) = auxhist9_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist9_begin_s
SUBROUTINE nl_set_auxhist9_begin ( id_id , auxhist9_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_begin
  INTEGER id_id
  model_config_rec%auxhist9_begin(id_id) = auxhist9_begin
  RETURN
END SUBROUTINE nl_set_auxhist9_begin
SUBROUTINE nl_set_auxhist9_end_y ( id_id , auxhist9_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_end_y
  INTEGER id_id
  model_config_rec%auxhist9_end_y(id_id) = auxhist9_end_y
  RETURN
END SUBROUTINE nl_set_auxhist9_end_y
SUBROUTINE nl_set_auxhist9_end_d ( id_id , auxhist9_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_end_d
  INTEGER id_id
  model_config_rec%auxhist9_end_d(id_id) = auxhist9_end_d
  RETURN
END SUBROUTINE nl_set_auxhist9_end_d
SUBROUTINE nl_set_auxhist9_end_h ( id_id , auxhist9_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_end_h
  INTEGER id_id
  model_config_rec%auxhist9_end_h(id_id) = auxhist9_end_h
  RETURN
END SUBROUTINE nl_set_auxhist9_end_h
SUBROUTINE nl_set_auxhist9_end_m ( id_id , auxhist9_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_end_m
  INTEGER id_id
  model_config_rec%auxhist9_end_m(id_id) = auxhist9_end_m
  RETURN
END SUBROUTINE nl_set_auxhist9_end_m
SUBROUTINE nl_set_auxhist9_end_s ( id_id , auxhist9_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_end_s
  INTEGER id_id
  model_config_rec%auxhist9_end_s(id_id) = auxhist9_end_s
  RETURN
END SUBROUTINE nl_set_auxhist9_end_s
SUBROUTINE nl_set_auxhist9_end ( id_id , auxhist9_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist9_end
  INTEGER id_id
  model_config_rec%auxhist9_end(id_id) = auxhist9_end
  RETURN
END SUBROUTINE nl_set_auxhist9_end
SUBROUTINE nl_set_io_form_auxhist9 ( id_id , io_form_auxhist9 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist9
  INTEGER id_id
  model_config_rec%io_form_auxhist9 = io_form_auxhist9 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist9
SUBROUTINE nl_set_frames_per_auxhist9 ( id_id , frames_per_auxhist9 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist9
  INTEGER id_id
  model_config_rec%frames_per_auxhist9(id_id) = frames_per_auxhist9
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist9
SUBROUTINE nl_set_auxhist10_inname ( id_id , auxhist10_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist10_inname
  INTEGER id_id
  model_config_rec%auxhist10_inname = trim(auxhist10_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist10_inname
SUBROUTINE nl_set_auxhist10_outname ( id_id , auxhist10_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist10_outname
  INTEGER id_id
  model_config_rec%auxhist10_outname = trim(auxhist10_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist10_outname
SUBROUTINE nl_set_auxhist10_interval_y ( id_id , auxhist10_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_interval_y
  INTEGER id_id
  model_config_rec%auxhist10_interval_y(id_id) = auxhist10_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist10_interval_y
SUBROUTINE nl_set_auxhist10_interval_d ( id_id , auxhist10_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_interval_d
  INTEGER id_id
  model_config_rec%auxhist10_interval_d(id_id) = auxhist10_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist10_interval_d
SUBROUTINE nl_set_auxhist10_interval_h ( id_id , auxhist10_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_interval_h
  INTEGER id_id
  model_config_rec%auxhist10_interval_h(id_id) = auxhist10_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist10_interval_h
SUBROUTINE nl_set_auxhist10_interval_m ( id_id , auxhist10_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_interval_m
  INTEGER id_id
  model_config_rec%auxhist10_interval_m(id_id) = auxhist10_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist10_interval_m
SUBROUTINE nl_set_auxhist10_interval_s ( id_id , auxhist10_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_interval_s
  INTEGER id_id
  model_config_rec%auxhist10_interval_s(id_id) = auxhist10_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist10_interval_s
SUBROUTINE nl_set_auxhist10_interval ( id_id , auxhist10_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_interval
  INTEGER id_id
  model_config_rec%auxhist10_interval(id_id) = auxhist10_interval
  RETURN
END SUBROUTINE nl_set_auxhist10_interval
SUBROUTINE nl_set_auxhist10_begin_y ( id_id , auxhist10_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_begin_y
  INTEGER id_id
  model_config_rec%auxhist10_begin_y(id_id) = auxhist10_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist10_begin_y
SUBROUTINE nl_set_auxhist10_begin_d ( id_id , auxhist10_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_begin_d
  INTEGER id_id
  model_config_rec%auxhist10_begin_d(id_id) = auxhist10_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist10_begin_d
SUBROUTINE nl_set_auxhist10_begin_h ( id_id , auxhist10_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_begin_h
  INTEGER id_id
  model_config_rec%auxhist10_begin_h(id_id) = auxhist10_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist10_begin_h
SUBROUTINE nl_set_auxhist10_begin_m ( id_id , auxhist10_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_begin_m
  INTEGER id_id
  model_config_rec%auxhist10_begin_m(id_id) = auxhist10_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist10_begin_m
SUBROUTINE nl_set_auxhist10_begin_s ( id_id , auxhist10_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_begin_s
  INTEGER id_id
  model_config_rec%auxhist10_begin_s(id_id) = auxhist10_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist10_begin_s
SUBROUTINE nl_set_auxhist10_begin ( id_id , auxhist10_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_begin
  INTEGER id_id
  model_config_rec%auxhist10_begin(id_id) = auxhist10_begin
  RETURN
END SUBROUTINE nl_set_auxhist10_begin
SUBROUTINE nl_set_auxhist10_end_y ( id_id , auxhist10_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_end_y
  INTEGER id_id
  model_config_rec%auxhist10_end_y(id_id) = auxhist10_end_y
  RETURN
END SUBROUTINE nl_set_auxhist10_end_y
SUBROUTINE nl_set_auxhist10_end_d ( id_id , auxhist10_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_end_d
  INTEGER id_id
  model_config_rec%auxhist10_end_d(id_id) = auxhist10_end_d
  RETURN
END SUBROUTINE nl_set_auxhist10_end_d
SUBROUTINE nl_set_auxhist10_end_h ( id_id , auxhist10_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_end_h
  INTEGER id_id
  model_config_rec%auxhist10_end_h(id_id) = auxhist10_end_h
  RETURN
END SUBROUTINE nl_set_auxhist10_end_h
SUBROUTINE nl_set_auxhist10_end_m ( id_id , auxhist10_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_end_m
  INTEGER id_id
  model_config_rec%auxhist10_end_m(id_id) = auxhist10_end_m
  RETURN
END SUBROUTINE nl_set_auxhist10_end_m
SUBROUTINE nl_set_auxhist10_end_s ( id_id , auxhist10_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_end_s
  INTEGER id_id
  model_config_rec%auxhist10_end_s(id_id) = auxhist10_end_s
  RETURN
END SUBROUTINE nl_set_auxhist10_end_s
SUBROUTINE nl_set_auxhist10_end ( id_id , auxhist10_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist10_end
  INTEGER id_id
  model_config_rec%auxhist10_end(id_id) = auxhist10_end
  RETURN
END SUBROUTINE nl_set_auxhist10_end
SUBROUTINE nl_set_io_form_auxhist10 ( id_id , io_form_auxhist10 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist10
  INTEGER id_id
  model_config_rec%io_form_auxhist10 = io_form_auxhist10 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist10
SUBROUTINE nl_set_frames_per_auxhist10 ( id_id , frames_per_auxhist10 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist10
  INTEGER id_id
  model_config_rec%frames_per_auxhist10(id_id) = frames_per_auxhist10
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist10
SUBROUTINE nl_set_auxhist11_inname ( id_id , auxhist11_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist11_inname
  INTEGER id_id
  model_config_rec%auxhist11_inname = trim(auxhist11_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist11_inname
SUBROUTINE nl_set_auxhist11_outname ( id_id , auxhist11_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist11_outname
  INTEGER id_id
  model_config_rec%auxhist11_outname = trim(auxhist11_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist11_outname
SUBROUTINE nl_set_auxhist11_interval_y ( id_id , auxhist11_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_interval_y
  INTEGER id_id
  model_config_rec%auxhist11_interval_y(id_id) = auxhist11_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist11_interval_y
SUBROUTINE nl_set_auxhist11_interval_d ( id_id , auxhist11_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_interval_d
  INTEGER id_id
  model_config_rec%auxhist11_interval_d(id_id) = auxhist11_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist11_interval_d
SUBROUTINE nl_set_auxhist11_interval_h ( id_id , auxhist11_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_interval_h
  INTEGER id_id
  model_config_rec%auxhist11_interval_h(id_id) = auxhist11_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist11_interval_h
SUBROUTINE nl_set_auxhist11_interval_m ( id_id , auxhist11_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_interval_m
  INTEGER id_id
  model_config_rec%auxhist11_interval_m(id_id) = auxhist11_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist11_interval_m
SUBROUTINE nl_set_auxhist11_interval_s ( id_id , auxhist11_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_interval_s
  INTEGER id_id
  model_config_rec%auxhist11_interval_s(id_id) = auxhist11_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist11_interval_s
SUBROUTINE nl_set_auxhist11_interval ( id_id , auxhist11_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_interval
  INTEGER id_id
  model_config_rec%auxhist11_interval(id_id) = auxhist11_interval
  RETURN
END SUBROUTINE nl_set_auxhist11_interval
SUBROUTINE nl_set_auxhist11_begin_y ( id_id , auxhist11_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_begin_y
  INTEGER id_id
  model_config_rec%auxhist11_begin_y(id_id) = auxhist11_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist11_begin_y
SUBROUTINE nl_set_auxhist11_begin_d ( id_id , auxhist11_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_begin_d
  INTEGER id_id
  model_config_rec%auxhist11_begin_d(id_id) = auxhist11_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist11_begin_d
SUBROUTINE nl_set_auxhist11_begin_h ( id_id , auxhist11_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_begin_h
  INTEGER id_id
  model_config_rec%auxhist11_begin_h(id_id) = auxhist11_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist11_begin_h
SUBROUTINE nl_set_auxhist11_begin_m ( id_id , auxhist11_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_begin_m
  INTEGER id_id
  model_config_rec%auxhist11_begin_m(id_id) = auxhist11_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist11_begin_m
SUBROUTINE nl_set_auxhist11_begin_s ( id_id , auxhist11_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_begin_s
  INTEGER id_id
  model_config_rec%auxhist11_begin_s(id_id) = auxhist11_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist11_begin_s
SUBROUTINE nl_set_auxhist11_begin ( id_id , auxhist11_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_begin
  INTEGER id_id
  model_config_rec%auxhist11_begin(id_id) = auxhist11_begin
  RETURN
END SUBROUTINE nl_set_auxhist11_begin
SUBROUTINE nl_set_auxhist11_end_y ( id_id , auxhist11_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_end_y
  INTEGER id_id
  model_config_rec%auxhist11_end_y(id_id) = auxhist11_end_y
  RETURN
END SUBROUTINE nl_set_auxhist11_end_y
SUBROUTINE nl_set_auxhist11_end_d ( id_id , auxhist11_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_end_d
  INTEGER id_id
  model_config_rec%auxhist11_end_d(id_id) = auxhist11_end_d
  RETURN
END SUBROUTINE nl_set_auxhist11_end_d
SUBROUTINE nl_set_auxhist11_end_h ( id_id , auxhist11_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_end_h
  INTEGER id_id
  model_config_rec%auxhist11_end_h(id_id) = auxhist11_end_h
  RETURN
END SUBROUTINE nl_set_auxhist11_end_h
SUBROUTINE nl_set_auxhist11_end_m ( id_id , auxhist11_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_end_m
  INTEGER id_id
  model_config_rec%auxhist11_end_m(id_id) = auxhist11_end_m
  RETURN
END SUBROUTINE nl_set_auxhist11_end_m
SUBROUTINE nl_set_auxhist11_end_s ( id_id , auxhist11_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_end_s
  INTEGER id_id
  model_config_rec%auxhist11_end_s(id_id) = auxhist11_end_s
  RETURN
END SUBROUTINE nl_set_auxhist11_end_s
SUBROUTINE nl_set_auxhist11_end ( id_id , auxhist11_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist11_end
  INTEGER id_id
  model_config_rec%auxhist11_end(id_id) = auxhist11_end
  RETURN
END SUBROUTINE nl_set_auxhist11_end
SUBROUTINE nl_set_io_form_auxhist11 ( id_id , io_form_auxhist11 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist11
  INTEGER id_id
  model_config_rec%io_form_auxhist11 = io_form_auxhist11 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist11
SUBROUTINE nl_set_frames_per_auxhist11 ( id_id , frames_per_auxhist11 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist11
  INTEGER id_id
  model_config_rec%frames_per_auxhist11(id_id) = frames_per_auxhist11
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist11
SUBROUTINE nl_set_auxhist12_inname ( id_id , auxhist12_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist12_inname
  INTEGER id_id
  model_config_rec%auxhist12_inname = trim(auxhist12_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist12_inname
SUBROUTINE nl_set_auxhist12_outname ( id_id , auxhist12_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist12_outname
  INTEGER id_id
  model_config_rec%auxhist12_outname = trim(auxhist12_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist12_outname
SUBROUTINE nl_set_auxhist12_interval_y ( id_id , auxhist12_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist12_interval_y
  INTEGER id_id
  model_config_rec%auxhist12_interval_y(id_id) = auxhist12_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist12_interval_y
SUBROUTINE nl_set_auxhist12_interval_d ( id_id , auxhist12_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist12_interval_d
  INTEGER id_id
  model_config_rec%auxhist12_interval_d(id_id) = auxhist12_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist12_interval_d
SUBROUTINE nl_set_auxhist12_interval_h ( id_id , auxhist12_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist12_interval_h
  INTEGER id_id
  model_config_rec%auxhist12_interval_h(id_id) = auxhist12_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist12_interval_h
SUBROUTINE nl_set_auxhist12_interval_m ( id_id , auxhist12_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist12_interval_m
  INTEGER id_id
  model_config_rec%auxhist12_interval_m(id_id) = auxhist12_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist12_interval_m
SUBROUTINE nl_set_auxhist12_interval_s ( id_id , auxhist12_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist12_interval_s
  INTEGER id_id
  model_config_rec%auxhist12_interval_s(id_id) = auxhist12_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist12_interval_s
SUBROUTINE nl_set_auxhist12_interval ( id_id , auxhist12_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist12_interval
  INTEGER id_id
  model_config_rec%auxhist12_interval(id_id) = auxhist12_interval
  RETURN
END SUBROUTINE nl_set_auxhist12_interval
SUBROUTINE nl_set_auxhist12_begin_y ( id_id , auxhist12_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist12_begin_y
  INTEGER id_id
  model_config_rec%auxhist12_begin_y(id_id) = auxhist12_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist12_begin_y
SUBROUTINE nl_set_auxhist12_begin_d ( id_id , auxhist12_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist12_begin_d
  INTEGER id_id
  model_config_rec%auxhist12_begin_d(id_id) = auxhist12_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist12_begin_d
SUBROUTINE nl_set_auxhist12_begin_h ( id_id , auxhist12_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist12_begin_h
  INTEGER id_id
  model_config_rec%auxhist12_begin_h(id_id) = auxhist12_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist12_begin_h
SUBROUTINE nl_set_auxhist12_begin_m ( id_id , auxhist12_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist12_begin_m
  INTEGER id_id
  model_config_rec%auxhist12_begin_m(id_id) = auxhist12_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist12_begin_m
SUBROUTINE nl_set_auxhist12_begin_s ( id_id , auxhist12_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist12_begin_s
  INTEGER id_id
  model_config_rec%auxhist12_begin_s(id_id) = auxhist12_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist12_begin_s
SUBROUTINE nl_set_auxhist12_begin ( id_id , auxhist12_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist12_begin
  INTEGER id_id
  model_config_rec%auxhist12_begin(id_id) = auxhist12_begin
  RETURN
END SUBROUTINE nl_set_auxhist12_begin
SUBROUTINE nl_set_auxhist12_end_y ( id_id , auxhist12_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist12_end_y
  INTEGER id_id
  model_config_rec%auxhist12_end_y(id_id) = auxhist12_end_y
  RETURN
END SUBROUTINE nl_set_auxhist12_end_y
SUBROUTINE nl_set_auxhist12_end_d ( id_id , auxhist12_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist12_end_d
  INTEGER id_id
  model_config_rec%auxhist12_end_d(id_id) = auxhist12_end_d
  RETURN
END SUBROUTINE nl_set_auxhist12_end_d
SUBROUTINE nl_set_auxhist12_end_h ( id_id , auxhist12_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist12_end_h
  INTEGER id_id
  model_config_rec%auxhist12_end_h(id_id) = auxhist12_end_h
  RETURN
END SUBROUTINE nl_set_auxhist12_end_h
SUBROUTINE nl_set_auxhist12_end_m ( id_id , auxhist12_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist12_end_m
  INTEGER id_id
  model_config_rec%auxhist12_end_m(id_id) = auxhist12_end_m
  RETURN
END SUBROUTINE nl_set_auxhist12_end_m
SUBROUTINE nl_set_auxhist12_end_s ( id_id , auxhist12_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist12_end_s
  INTEGER id_id
  model_config_rec%auxhist12_end_s(id_id) = auxhist12_end_s
  RETURN
END SUBROUTINE nl_set_auxhist12_end_s
SUBROUTINE nl_set_auxhist12_end ( id_id , auxhist12_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist12_end
  INTEGER id_id
  model_config_rec%auxhist12_end(id_id) = auxhist12_end
  RETURN
END SUBROUTINE nl_set_auxhist12_end
SUBROUTINE nl_set_io_form_auxhist12 ( id_id , io_form_auxhist12 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist12
  INTEGER id_id
  model_config_rec%io_form_auxhist12 = io_form_auxhist12 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist12
SUBROUTINE nl_set_frames_per_auxhist12 ( id_id , frames_per_auxhist12 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist12
  INTEGER id_id
  model_config_rec%frames_per_auxhist12(id_id) = frames_per_auxhist12
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist12
SUBROUTINE nl_set_auxhist13_inname ( id_id , auxhist13_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist13_inname
  INTEGER id_id
  model_config_rec%auxhist13_inname = trim(auxhist13_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist13_inname
SUBROUTINE nl_set_auxhist13_outname ( id_id , auxhist13_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist13_outname
  INTEGER id_id
  model_config_rec%auxhist13_outname = trim(auxhist13_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist13_outname
SUBROUTINE nl_set_auxhist13_interval_y ( id_id , auxhist13_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist13_interval_y
  INTEGER id_id
  model_config_rec%auxhist13_interval_y(id_id) = auxhist13_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist13_interval_y
SUBROUTINE nl_set_auxhist13_interval_d ( id_id , auxhist13_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist13_interval_d
  INTEGER id_id
  model_config_rec%auxhist13_interval_d(id_id) = auxhist13_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist13_interval_d
SUBROUTINE nl_set_auxhist13_interval_h ( id_id , auxhist13_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist13_interval_h
  INTEGER id_id
  model_config_rec%auxhist13_interval_h(id_id) = auxhist13_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist13_interval_h
SUBROUTINE nl_set_auxhist13_interval_m ( id_id , auxhist13_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist13_interval_m
  INTEGER id_id
  model_config_rec%auxhist13_interval_m(id_id) = auxhist13_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist13_interval_m
SUBROUTINE nl_set_auxhist13_interval_s ( id_id , auxhist13_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist13_interval_s
  INTEGER id_id
  model_config_rec%auxhist13_interval_s(id_id) = auxhist13_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist13_interval_s
SUBROUTINE nl_set_auxhist13_interval ( id_id , auxhist13_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist13_interval
  INTEGER id_id
  model_config_rec%auxhist13_interval(id_id) = auxhist13_interval
  RETURN
END SUBROUTINE nl_set_auxhist13_interval
SUBROUTINE nl_set_auxhist13_begin_y ( id_id , auxhist13_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist13_begin_y
  INTEGER id_id
  model_config_rec%auxhist13_begin_y(id_id) = auxhist13_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist13_begin_y
SUBROUTINE nl_set_auxhist13_begin_d ( id_id , auxhist13_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist13_begin_d
  INTEGER id_id
  model_config_rec%auxhist13_begin_d(id_id) = auxhist13_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist13_begin_d
SUBROUTINE nl_set_auxhist13_begin_h ( id_id , auxhist13_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist13_begin_h
  INTEGER id_id
  model_config_rec%auxhist13_begin_h(id_id) = auxhist13_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist13_begin_h
SUBROUTINE nl_set_auxhist13_begin_m ( id_id , auxhist13_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist13_begin_m
  INTEGER id_id
  model_config_rec%auxhist13_begin_m(id_id) = auxhist13_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist13_begin_m
SUBROUTINE nl_set_auxhist13_begin_s ( id_id , auxhist13_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist13_begin_s
  INTEGER id_id
  model_config_rec%auxhist13_begin_s(id_id) = auxhist13_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist13_begin_s
SUBROUTINE nl_set_auxhist13_begin ( id_id , auxhist13_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist13_begin
  INTEGER id_id
  model_config_rec%auxhist13_begin(id_id) = auxhist13_begin
  RETURN
END SUBROUTINE nl_set_auxhist13_begin
SUBROUTINE nl_set_auxhist13_end_y ( id_id , auxhist13_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist13_end_y
  INTEGER id_id
  model_config_rec%auxhist13_end_y(id_id) = auxhist13_end_y
  RETURN
END SUBROUTINE nl_set_auxhist13_end_y
SUBROUTINE nl_set_auxhist13_end_d ( id_id , auxhist13_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist13_end_d
  INTEGER id_id
  model_config_rec%auxhist13_end_d(id_id) = auxhist13_end_d
  RETURN
END SUBROUTINE nl_set_auxhist13_end_d
SUBROUTINE nl_set_auxhist13_end_h ( id_id , auxhist13_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist13_end_h
  INTEGER id_id
  model_config_rec%auxhist13_end_h(id_id) = auxhist13_end_h
  RETURN
END SUBROUTINE nl_set_auxhist13_end_h
SUBROUTINE nl_set_auxhist13_end_m ( id_id , auxhist13_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist13_end_m
  INTEGER id_id
  model_config_rec%auxhist13_end_m(id_id) = auxhist13_end_m
  RETURN
END SUBROUTINE nl_set_auxhist13_end_m
SUBROUTINE nl_set_auxhist13_end_s ( id_id , auxhist13_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist13_end_s
  INTEGER id_id
  model_config_rec%auxhist13_end_s(id_id) = auxhist13_end_s
  RETURN
END SUBROUTINE nl_set_auxhist13_end_s
SUBROUTINE nl_set_auxhist13_end ( id_id , auxhist13_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist13_end
  INTEGER id_id
  model_config_rec%auxhist13_end(id_id) = auxhist13_end
  RETURN
END SUBROUTINE nl_set_auxhist13_end
SUBROUTINE nl_set_io_form_auxhist13 ( id_id , io_form_auxhist13 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist13
  INTEGER id_id
  model_config_rec%io_form_auxhist13 = io_form_auxhist13 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist13
SUBROUTINE nl_set_frames_per_auxhist13 ( id_id , frames_per_auxhist13 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist13
  INTEGER id_id
  model_config_rec%frames_per_auxhist13(id_id) = frames_per_auxhist13
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist13
SUBROUTINE nl_set_auxhist14_inname ( id_id , auxhist14_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist14_inname
  INTEGER id_id
  model_config_rec%auxhist14_inname = trim(auxhist14_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist14_inname
SUBROUTINE nl_set_auxhist14_outname ( id_id , auxhist14_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist14_outname
  INTEGER id_id
  model_config_rec%auxhist14_outname = trim(auxhist14_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist14_outname
SUBROUTINE nl_set_auxhist14_interval_y ( id_id , auxhist14_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist14_interval_y
  INTEGER id_id
  model_config_rec%auxhist14_interval_y(id_id) = auxhist14_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist14_interval_y
SUBROUTINE nl_set_auxhist14_interval_d ( id_id , auxhist14_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist14_interval_d
  INTEGER id_id
  model_config_rec%auxhist14_interval_d(id_id) = auxhist14_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist14_interval_d
SUBROUTINE nl_set_auxhist14_interval_h ( id_id , auxhist14_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist14_interval_h
  INTEGER id_id
  model_config_rec%auxhist14_interval_h(id_id) = auxhist14_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist14_interval_h
SUBROUTINE nl_set_auxhist14_interval_m ( id_id , auxhist14_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist14_interval_m
  INTEGER id_id
  model_config_rec%auxhist14_interval_m(id_id) = auxhist14_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist14_interval_m
SUBROUTINE nl_set_auxhist14_interval_s ( id_id , auxhist14_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist14_interval_s
  INTEGER id_id
  model_config_rec%auxhist14_interval_s(id_id) = auxhist14_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist14_interval_s
SUBROUTINE nl_set_auxhist14_interval ( id_id , auxhist14_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist14_interval
  INTEGER id_id
  model_config_rec%auxhist14_interval(id_id) = auxhist14_interval
  RETURN
END SUBROUTINE nl_set_auxhist14_interval
SUBROUTINE nl_set_auxhist14_begin_y ( id_id , auxhist14_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist14_begin_y
  INTEGER id_id
  model_config_rec%auxhist14_begin_y(id_id) = auxhist14_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist14_begin_y
SUBROUTINE nl_set_auxhist14_begin_d ( id_id , auxhist14_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist14_begin_d
  INTEGER id_id
  model_config_rec%auxhist14_begin_d(id_id) = auxhist14_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist14_begin_d
SUBROUTINE nl_set_auxhist14_begin_h ( id_id , auxhist14_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist14_begin_h
  INTEGER id_id
  model_config_rec%auxhist14_begin_h(id_id) = auxhist14_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist14_begin_h
SUBROUTINE nl_set_auxhist14_begin_m ( id_id , auxhist14_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist14_begin_m
  INTEGER id_id
  model_config_rec%auxhist14_begin_m(id_id) = auxhist14_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist14_begin_m
SUBROUTINE nl_set_auxhist14_begin_s ( id_id , auxhist14_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist14_begin_s
  INTEGER id_id
  model_config_rec%auxhist14_begin_s(id_id) = auxhist14_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist14_begin_s
#endif
#if (NNN == 2)
SUBROUTINE nl_set_auxhist14_begin ( id_id , auxhist14_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist14_begin
  INTEGER id_id
  model_config_rec%auxhist14_begin(id_id) = auxhist14_begin
  RETURN
END SUBROUTINE nl_set_auxhist14_begin
SUBROUTINE nl_set_auxhist14_end_y ( id_id , auxhist14_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist14_end_y
  INTEGER id_id
  model_config_rec%auxhist14_end_y(id_id) = auxhist14_end_y
  RETURN
END SUBROUTINE nl_set_auxhist14_end_y
SUBROUTINE nl_set_auxhist14_end_d ( id_id , auxhist14_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist14_end_d
  INTEGER id_id
  model_config_rec%auxhist14_end_d(id_id) = auxhist14_end_d
  RETURN
END SUBROUTINE nl_set_auxhist14_end_d
SUBROUTINE nl_set_auxhist14_end_h ( id_id , auxhist14_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist14_end_h
  INTEGER id_id
  model_config_rec%auxhist14_end_h(id_id) = auxhist14_end_h
  RETURN
END SUBROUTINE nl_set_auxhist14_end_h
SUBROUTINE nl_set_auxhist14_end_m ( id_id , auxhist14_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist14_end_m
  INTEGER id_id
  model_config_rec%auxhist14_end_m(id_id) = auxhist14_end_m
  RETURN
END SUBROUTINE nl_set_auxhist14_end_m
SUBROUTINE nl_set_auxhist14_end_s ( id_id , auxhist14_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist14_end_s
  INTEGER id_id
  model_config_rec%auxhist14_end_s(id_id) = auxhist14_end_s
  RETURN
END SUBROUTINE nl_set_auxhist14_end_s
SUBROUTINE nl_set_auxhist14_end ( id_id , auxhist14_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist14_end
  INTEGER id_id
  model_config_rec%auxhist14_end(id_id) = auxhist14_end
  RETURN
END SUBROUTINE nl_set_auxhist14_end
SUBROUTINE nl_set_io_form_auxhist14 ( id_id , io_form_auxhist14 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist14
  INTEGER id_id
  model_config_rec%io_form_auxhist14 = io_form_auxhist14 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist14
SUBROUTINE nl_set_frames_per_auxhist14 ( id_id , frames_per_auxhist14 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist14
  INTEGER id_id
  model_config_rec%frames_per_auxhist14(id_id) = frames_per_auxhist14
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist14
SUBROUTINE nl_set_auxhist15_inname ( id_id , auxhist15_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist15_inname
  INTEGER id_id
  model_config_rec%auxhist15_inname = trim(auxhist15_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist15_inname
SUBROUTINE nl_set_auxhist15_outname ( id_id , auxhist15_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist15_outname
  INTEGER id_id
  model_config_rec%auxhist15_outname = trim(auxhist15_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist15_outname
SUBROUTINE nl_set_auxhist15_interval_y ( id_id , auxhist15_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist15_interval_y
  INTEGER id_id
  model_config_rec%auxhist15_interval_y(id_id) = auxhist15_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist15_interval_y
SUBROUTINE nl_set_auxhist15_interval_d ( id_id , auxhist15_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist15_interval_d
  INTEGER id_id
  model_config_rec%auxhist15_interval_d(id_id) = auxhist15_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist15_interval_d
SUBROUTINE nl_set_auxhist15_interval_h ( id_id , auxhist15_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist15_interval_h
  INTEGER id_id
  model_config_rec%auxhist15_interval_h(id_id) = auxhist15_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist15_interval_h
SUBROUTINE nl_set_auxhist15_interval_m ( id_id , auxhist15_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist15_interval_m
  INTEGER id_id
  model_config_rec%auxhist15_interval_m(id_id) = auxhist15_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist15_interval_m
SUBROUTINE nl_set_auxhist15_interval_s ( id_id , auxhist15_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist15_interval_s
  INTEGER id_id
  model_config_rec%auxhist15_interval_s(id_id) = auxhist15_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist15_interval_s
SUBROUTINE nl_set_auxhist15_interval ( id_id , auxhist15_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist15_interval
  INTEGER id_id
  model_config_rec%auxhist15_interval(id_id) = auxhist15_interval
  RETURN
END SUBROUTINE nl_set_auxhist15_interval
SUBROUTINE nl_set_auxhist15_begin_y ( id_id , auxhist15_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist15_begin_y
  INTEGER id_id
  model_config_rec%auxhist15_begin_y(id_id) = auxhist15_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist15_begin_y
SUBROUTINE nl_set_auxhist15_begin_d ( id_id , auxhist15_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist15_begin_d
  INTEGER id_id
  model_config_rec%auxhist15_begin_d(id_id) = auxhist15_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist15_begin_d
SUBROUTINE nl_set_auxhist15_begin_h ( id_id , auxhist15_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist15_begin_h
  INTEGER id_id
  model_config_rec%auxhist15_begin_h(id_id) = auxhist15_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist15_begin_h
SUBROUTINE nl_set_auxhist15_begin_m ( id_id , auxhist15_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist15_begin_m
  INTEGER id_id
  model_config_rec%auxhist15_begin_m(id_id) = auxhist15_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist15_begin_m
SUBROUTINE nl_set_auxhist15_begin_s ( id_id , auxhist15_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist15_begin_s
  INTEGER id_id
  model_config_rec%auxhist15_begin_s(id_id) = auxhist15_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist15_begin_s
SUBROUTINE nl_set_auxhist15_begin ( id_id , auxhist15_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist15_begin
  INTEGER id_id
  model_config_rec%auxhist15_begin(id_id) = auxhist15_begin
  RETURN
END SUBROUTINE nl_set_auxhist15_begin
SUBROUTINE nl_set_auxhist15_end_y ( id_id , auxhist15_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist15_end_y
  INTEGER id_id
  model_config_rec%auxhist15_end_y(id_id) = auxhist15_end_y
  RETURN
END SUBROUTINE nl_set_auxhist15_end_y
SUBROUTINE nl_set_auxhist15_end_d ( id_id , auxhist15_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist15_end_d
  INTEGER id_id
  model_config_rec%auxhist15_end_d(id_id) = auxhist15_end_d
  RETURN
END SUBROUTINE nl_set_auxhist15_end_d
SUBROUTINE nl_set_auxhist15_end_h ( id_id , auxhist15_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist15_end_h
  INTEGER id_id
  model_config_rec%auxhist15_end_h(id_id) = auxhist15_end_h
  RETURN
END SUBROUTINE nl_set_auxhist15_end_h
SUBROUTINE nl_set_auxhist15_end_m ( id_id , auxhist15_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist15_end_m
  INTEGER id_id
  model_config_rec%auxhist15_end_m(id_id) = auxhist15_end_m
  RETURN
END SUBROUTINE nl_set_auxhist15_end_m
SUBROUTINE nl_set_auxhist15_end_s ( id_id , auxhist15_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist15_end_s
  INTEGER id_id
  model_config_rec%auxhist15_end_s(id_id) = auxhist15_end_s
  RETURN
END SUBROUTINE nl_set_auxhist15_end_s
SUBROUTINE nl_set_auxhist15_end ( id_id , auxhist15_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist15_end
  INTEGER id_id
  model_config_rec%auxhist15_end(id_id) = auxhist15_end
  RETURN
END SUBROUTINE nl_set_auxhist15_end
SUBROUTINE nl_set_io_form_auxhist15 ( id_id , io_form_auxhist15 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist15
  INTEGER id_id
  model_config_rec%io_form_auxhist15 = io_form_auxhist15 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist15
SUBROUTINE nl_set_frames_per_auxhist15 ( id_id , frames_per_auxhist15 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist15
  INTEGER id_id
  model_config_rec%frames_per_auxhist15(id_id) = frames_per_auxhist15
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist15
SUBROUTINE nl_set_auxhist16_inname ( id_id , auxhist16_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist16_inname
  INTEGER id_id
  model_config_rec%auxhist16_inname = trim(auxhist16_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist16_inname
SUBROUTINE nl_set_auxhist16_outname ( id_id , auxhist16_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist16_outname
  INTEGER id_id
  model_config_rec%auxhist16_outname = trim(auxhist16_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist16_outname
SUBROUTINE nl_set_auxhist16_interval_y ( id_id , auxhist16_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist16_interval_y
  INTEGER id_id
  model_config_rec%auxhist16_interval_y(id_id) = auxhist16_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist16_interval_y
SUBROUTINE nl_set_auxhist16_interval_d ( id_id , auxhist16_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist16_interval_d
  INTEGER id_id
  model_config_rec%auxhist16_interval_d(id_id) = auxhist16_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist16_interval_d
SUBROUTINE nl_set_auxhist16_interval_h ( id_id , auxhist16_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist16_interval_h
  INTEGER id_id
  model_config_rec%auxhist16_interval_h(id_id) = auxhist16_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist16_interval_h
SUBROUTINE nl_set_auxhist16_interval_m ( id_id , auxhist16_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist16_interval_m
  INTEGER id_id
  model_config_rec%auxhist16_interval_m(id_id) = auxhist16_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist16_interval_m
SUBROUTINE nl_set_auxhist16_interval_s ( id_id , auxhist16_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist16_interval_s
  INTEGER id_id
  model_config_rec%auxhist16_interval_s(id_id) = auxhist16_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist16_interval_s
SUBROUTINE nl_set_auxhist16_interval ( id_id , auxhist16_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist16_interval
  INTEGER id_id
  model_config_rec%auxhist16_interval(id_id) = auxhist16_interval
  RETURN
END SUBROUTINE nl_set_auxhist16_interval
SUBROUTINE nl_set_auxhist16_begin_y ( id_id , auxhist16_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist16_begin_y
  INTEGER id_id
  model_config_rec%auxhist16_begin_y(id_id) = auxhist16_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist16_begin_y
SUBROUTINE nl_set_auxhist16_begin_d ( id_id , auxhist16_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist16_begin_d
  INTEGER id_id
  model_config_rec%auxhist16_begin_d(id_id) = auxhist16_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist16_begin_d
SUBROUTINE nl_set_auxhist16_begin_h ( id_id , auxhist16_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist16_begin_h
  INTEGER id_id
  model_config_rec%auxhist16_begin_h(id_id) = auxhist16_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist16_begin_h
SUBROUTINE nl_set_auxhist16_begin_m ( id_id , auxhist16_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist16_begin_m
  INTEGER id_id
  model_config_rec%auxhist16_begin_m(id_id) = auxhist16_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist16_begin_m
SUBROUTINE nl_set_auxhist16_begin_s ( id_id , auxhist16_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist16_begin_s
  INTEGER id_id
  model_config_rec%auxhist16_begin_s(id_id) = auxhist16_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist16_begin_s
SUBROUTINE nl_set_auxhist16_begin ( id_id , auxhist16_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist16_begin
  INTEGER id_id
  model_config_rec%auxhist16_begin(id_id) = auxhist16_begin
  RETURN
END SUBROUTINE nl_set_auxhist16_begin
SUBROUTINE nl_set_auxhist16_end_y ( id_id , auxhist16_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist16_end_y
  INTEGER id_id
  model_config_rec%auxhist16_end_y(id_id) = auxhist16_end_y
  RETURN
END SUBROUTINE nl_set_auxhist16_end_y
SUBROUTINE nl_set_auxhist16_end_d ( id_id , auxhist16_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist16_end_d
  INTEGER id_id
  model_config_rec%auxhist16_end_d(id_id) = auxhist16_end_d
  RETURN
END SUBROUTINE nl_set_auxhist16_end_d
SUBROUTINE nl_set_auxhist16_end_h ( id_id , auxhist16_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist16_end_h
  INTEGER id_id
  model_config_rec%auxhist16_end_h(id_id) = auxhist16_end_h
  RETURN
END SUBROUTINE nl_set_auxhist16_end_h
SUBROUTINE nl_set_auxhist16_end_m ( id_id , auxhist16_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist16_end_m
  INTEGER id_id
  model_config_rec%auxhist16_end_m(id_id) = auxhist16_end_m
  RETURN
END SUBROUTINE nl_set_auxhist16_end_m
SUBROUTINE nl_set_auxhist16_end_s ( id_id , auxhist16_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist16_end_s
  INTEGER id_id
  model_config_rec%auxhist16_end_s(id_id) = auxhist16_end_s
  RETURN
END SUBROUTINE nl_set_auxhist16_end_s
SUBROUTINE nl_set_auxhist16_end ( id_id , auxhist16_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist16_end
  INTEGER id_id
  model_config_rec%auxhist16_end(id_id) = auxhist16_end
  RETURN
END SUBROUTINE nl_set_auxhist16_end
SUBROUTINE nl_set_io_form_auxhist16 ( id_id , io_form_auxhist16 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist16
  INTEGER id_id
  model_config_rec%io_form_auxhist16 = io_form_auxhist16 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist16
SUBROUTINE nl_set_frames_per_auxhist16 ( id_id , frames_per_auxhist16 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist16
  INTEGER id_id
  model_config_rec%frames_per_auxhist16(id_id) = frames_per_auxhist16
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist16
SUBROUTINE nl_set_auxhist17_inname ( id_id , auxhist17_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist17_inname
  INTEGER id_id
  model_config_rec%auxhist17_inname = trim(auxhist17_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist17_inname
SUBROUTINE nl_set_auxhist17_outname ( id_id , auxhist17_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist17_outname
  INTEGER id_id
  model_config_rec%auxhist17_outname = trim(auxhist17_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist17_outname
SUBROUTINE nl_set_auxhist17_interval_y ( id_id , auxhist17_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist17_interval_y
  INTEGER id_id
  model_config_rec%auxhist17_interval_y(id_id) = auxhist17_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist17_interval_y
SUBROUTINE nl_set_auxhist17_interval_d ( id_id , auxhist17_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist17_interval_d
  INTEGER id_id
  model_config_rec%auxhist17_interval_d(id_id) = auxhist17_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist17_interval_d
SUBROUTINE nl_set_auxhist17_interval_h ( id_id , auxhist17_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist17_interval_h
  INTEGER id_id
  model_config_rec%auxhist17_interval_h(id_id) = auxhist17_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist17_interval_h
SUBROUTINE nl_set_auxhist17_interval_m ( id_id , auxhist17_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist17_interval_m
  INTEGER id_id
  model_config_rec%auxhist17_interval_m(id_id) = auxhist17_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist17_interval_m
SUBROUTINE nl_set_auxhist17_interval_s ( id_id , auxhist17_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist17_interval_s
  INTEGER id_id
  model_config_rec%auxhist17_interval_s(id_id) = auxhist17_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist17_interval_s
SUBROUTINE nl_set_auxhist17_interval ( id_id , auxhist17_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist17_interval
  INTEGER id_id
  model_config_rec%auxhist17_interval(id_id) = auxhist17_interval
  RETURN
END SUBROUTINE nl_set_auxhist17_interval
SUBROUTINE nl_set_auxhist17_begin_y ( id_id , auxhist17_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist17_begin_y
  INTEGER id_id
  model_config_rec%auxhist17_begin_y(id_id) = auxhist17_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist17_begin_y
SUBROUTINE nl_set_auxhist17_begin_d ( id_id , auxhist17_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist17_begin_d
  INTEGER id_id
  model_config_rec%auxhist17_begin_d(id_id) = auxhist17_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist17_begin_d
SUBROUTINE nl_set_auxhist17_begin_h ( id_id , auxhist17_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist17_begin_h
  INTEGER id_id
  model_config_rec%auxhist17_begin_h(id_id) = auxhist17_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist17_begin_h
SUBROUTINE nl_set_auxhist17_begin_m ( id_id , auxhist17_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist17_begin_m
  INTEGER id_id
  model_config_rec%auxhist17_begin_m(id_id) = auxhist17_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist17_begin_m
SUBROUTINE nl_set_auxhist17_begin_s ( id_id , auxhist17_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist17_begin_s
  INTEGER id_id
  model_config_rec%auxhist17_begin_s(id_id) = auxhist17_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist17_begin_s
SUBROUTINE nl_set_auxhist17_begin ( id_id , auxhist17_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist17_begin
  INTEGER id_id
  model_config_rec%auxhist17_begin(id_id) = auxhist17_begin
  RETURN
END SUBROUTINE nl_set_auxhist17_begin
SUBROUTINE nl_set_auxhist17_end_y ( id_id , auxhist17_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist17_end_y
  INTEGER id_id
  model_config_rec%auxhist17_end_y(id_id) = auxhist17_end_y
  RETURN
END SUBROUTINE nl_set_auxhist17_end_y
SUBROUTINE nl_set_auxhist17_end_d ( id_id , auxhist17_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist17_end_d
  INTEGER id_id
  model_config_rec%auxhist17_end_d(id_id) = auxhist17_end_d
  RETURN
END SUBROUTINE nl_set_auxhist17_end_d
SUBROUTINE nl_set_auxhist17_end_h ( id_id , auxhist17_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist17_end_h
  INTEGER id_id
  model_config_rec%auxhist17_end_h(id_id) = auxhist17_end_h
  RETURN
END SUBROUTINE nl_set_auxhist17_end_h
SUBROUTINE nl_set_auxhist17_end_m ( id_id , auxhist17_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist17_end_m
  INTEGER id_id
  model_config_rec%auxhist17_end_m(id_id) = auxhist17_end_m
  RETURN
END SUBROUTINE nl_set_auxhist17_end_m
SUBROUTINE nl_set_auxhist17_end_s ( id_id , auxhist17_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist17_end_s
  INTEGER id_id
  model_config_rec%auxhist17_end_s(id_id) = auxhist17_end_s
  RETURN
END SUBROUTINE nl_set_auxhist17_end_s
SUBROUTINE nl_set_auxhist17_end ( id_id , auxhist17_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist17_end
  INTEGER id_id
  model_config_rec%auxhist17_end(id_id) = auxhist17_end
  RETURN
END SUBROUTINE nl_set_auxhist17_end
SUBROUTINE nl_set_io_form_auxhist17 ( id_id , io_form_auxhist17 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist17
  INTEGER id_id
  model_config_rec%io_form_auxhist17 = io_form_auxhist17 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist17
SUBROUTINE nl_set_frames_per_auxhist17 ( id_id , frames_per_auxhist17 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist17
  INTEGER id_id
  model_config_rec%frames_per_auxhist17(id_id) = frames_per_auxhist17
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist17
SUBROUTINE nl_set_auxhist18_inname ( id_id , auxhist18_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist18_inname
  INTEGER id_id
  model_config_rec%auxhist18_inname = trim(auxhist18_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist18_inname
SUBROUTINE nl_set_auxhist18_outname ( id_id , auxhist18_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist18_outname
  INTEGER id_id
  model_config_rec%auxhist18_outname = trim(auxhist18_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist18_outname
SUBROUTINE nl_set_auxhist18_interval_y ( id_id , auxhist18_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist18_interval_y
  INTEGER id_id
  model_config_rec%auxhist18_interval_y(id_id) = auxhist18_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist18_interval_y
SUBROUTINE nl_set_auxhist18_interval_d ( id_id , auxhist18_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist18_interval_d
  INTEGER id_id
  model_config_rec%auxhist18_interval_d(id_id) = auxhist18_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist18_interval_d
SUBROUTINE nl_set_auxhist18_interval_h ( id_id , auxhist18_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist18_interval_h
  INTEGER id_id
  model_config_rec%auxhist18_interval_h(id_id) = auxhist18_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist18_interval_h
SUBROUTINE nl_set_auxhist18_interval_m ( id_id , auxhist18_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist18_interval_m
  INTEGER id_id
  model_config_rec%auxhist18_interval_m(id_id) = auxhist18_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist18_interval_m
SUBROUTINE nl_set_auxhist18_interval_s ( id_id , auxhist18_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist18_interval_s
  INTEGER id_id
  model_config_rec%auxhist18_interval_s(id_id) = auxhist18_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist18_interval_s
SUBROUTINE nl_set_auxhist18_interval ( id_id , auxhist18_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist18_interval
  INTEGER id_id
  model_config_rec%auxhist18_interval(id_id) = auxhist18_interval
  RETURN
END SUBROUTINE nl_set_auxhist18_interval
SUBROUTINE nl_set_auxhist18_begin_y ( id_id , auxhist18_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist18_begin_y
  INTEGER id_id
  model_config_rec%auxhist18_begin_y(id_id) = auxhist18_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist18_begin_y
SUBROUTINE nl_set_auxhist18_begin_d ( id_id , auxhist18_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist18_begin_d
  INTEGER id_id
  model_config_rec%auxhist18_begin_d(id_id) = auxhist18_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist18_begin_d
SUBROUTINE nl_set_auxhist18_begin_h ( id_id , auxhist18_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist18_begin_h
  INTEGER id_id
  model_config_rec%auxhist18_begin_h(id_id) = auxhist18_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist18_begin_h
SUBROUTINE nl_set_auxhist18_begin_m ( id_id , auxhist18_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist18_begin_m
  INTEGER id_id
  model_config_rec%auxhist18_begin_m(id_id) = auxhist18_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist18_begin_m
SUBROUTINE nl_set_auxhist18_begin_s ( id_id , auxhist18_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist18_begin_s
  INTEGER id_id
  model_config_rec%auxhist18_begin_s(id_id) = auxhist18_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist18_begin_s
SUBROUTINE nl_set_auxhist18_begin ( id_id , auxhist18_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist18_begin
  INTEGER id_id
  model_config_rec%auxhist18_begin(id_id) = auxhist18_begin
  RETURN
END SUBROUTINE nl_set_auxhist18_begin
SUBROUTINE nl_set_auxhist18_end_y ( id_id , auxhist18_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist18_end_y
  INTEGER id_id
  model_config_rec%auxhist18_end_y(id_id) = auxhist18_end_y
  RETURN
END SUBROUTINE nl_set_auxhist18_end_y
SUBROUTINE nl_set_auxhist18_end_d ( id_id , auxhist18_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist18_end_d
  INTEGER id_id
  model_config_rec%auxhist18_end_d(id_id) = auxhist18_end_d
  RETURN
END SUBROUTINE nl_set_auxhist18_end_d
SUBROUTINE nl_set_auxhist18_end_h ( id_id , auxhist18_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist18_end_h
  INTEGER id_id
  model_config_rec%auxhist18_end_h(id_id) = auxhist18_end_h
  RETURN
END SUBROUTINE nl_set_auxhist18_end_h
SUBROUTINE nl_set_auxhist18_end_m ( id_id , auxhist18_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist18_end_m
  INTEGER id_id
  model_config_rec%auxhist18_end_m(id_id) = auxhist18_end_m
  RETURN
END SUBROUTINE nl_set_auxhist18_end_m
SUBROUTINE nl_set_auxhist18_end_s ( id_id , auxhist18_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist18_end_s
  INTEGER id_id
  model_config_rec%auxhist18_end_s(id_id) = auxhist18_end_s
  RETURN
END SUBROUTINE nl_set_auxhist18_end_s
SUBROUTINE nl_set_auxhist18_end ( id_id , auxhist18_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist18_end
  INTEGER id_id
  model_config_rec%auxhist18_end(id_id) = auxhist18_end
  RETURN
END SUBROUTINE nl_set_auxhist18_end
SUBROUTINE nl_set_io_form_auxhist18 ( id_id , io_form_auxhist18 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist18
  INTEGER id_id
  model_config_rec%io_form_auxhist18 = io_form_auxhist18 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist18
SUBROUTINE nl_set_frames_per_auxhist18 ( id_id , frames_per_auxhist18 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist18
  INTEGER id_id
  model_config_rec%frames_per_auxhist18(id_id) = frames_per_auxhist18
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist18
SUBROUTINE nl_set_auxhist19_inname ( id_id , auxhist19_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist19_inname
  INTEGER id_id
  model_config_rec%auxhist19_inname = trim(auxhist19_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist19_inname
SUBROUTINE nl_set_auxhist19_outname ( id_id , auxhist19_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist19_outname
  INTEGER id_id
  model_config_rec%auxhist19_outname = trim(auxhist19_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist19_outname
SUBROUTINE nl_set_auxhist19_interval_y ( id_id , auxhist19_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist19_interval_y
  INTEGER id_id
  model_config_rec%auxhist19_interval_y(id_id) = auxhist19_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist19_interval_y
SUBROUTINE nl_set_auxhist19_interval_d ( id_id , auxhist19_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist19_interval_d
  INTEGER id_id
  model_config_rec%auxhist19_interval_d(id_id) = auxhist19_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist19_interval_d
SUBROUTINE nl_set_auxhist19_interval_h ( id_id , auxhist19_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist19_interval_h
  INTEGER id_id
  model_config_rec%auxhist19_interval_h(id_id) = auxhist19_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist19_interval_h
SUBROUTINE nl_set_auxhist19_interval_m ( id_id , auxhist19_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist19_interval_m
  INTEGER id_id
  model_config_rec%auxhist19_interval_m(id_id) = auxhist19_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist19_interval_m
SUBROUTINE nl_set_auxhist19_interval_s ( id_id , auxhist19_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist19_interval_s
  INTEGER id_id
  model_config_rec%auxhist19_interval_s(id_id) = auxhist19_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist19_interval_s
SUBROUTINE nl_set_auxhist19_interval ( id_id , auxhist19_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist19_interval
  INTEGER id_id
  model_config_rec%auxhist19_interval(id_id) = auxhist19_interval
  RETURN
END SUBROUTINE nl_set_auxhist19_interval
SUBROUTINE nl_set_auxhist19_begin_y ( id_id , auxhist19_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist19_begin_y
  INTEGER id_id
  model_config_rec%auxhist19_begin_y(id_id) = auxhist19_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist19_begin_y
SUBROUTINE nl_set_auxhist19_begin_d ( id_id , auxhist19_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist19_begin_d
  INTEGER id_id
  model_config_rec%auxhist19_begin_d(id_id) = auxhist19_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist19_begin_d
SUBROUTINE nl_set_auxhist19_begin_h ( id_id , auxhist19_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist19_begin_h
  INTEGER id_id
  model_config_rec%auxhist19_begin_h(id_id) = auxhist19_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist19_begin_h
SUBROUTINE nl_set_auxhist19_begin_m ( id_id , auxhist19_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist19_begin_m
  INTEGER id_id
  model_config_rec%auxhist19_begin_m(id_id) = auxhist19_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist19_begin_m
SUBROUTINE nl_set_auxhist19_begin_s ( id_id , auxhist19_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist19_begin_s
  INTEGER id_id
  model_config_rec%auxhist19_begin_s(id_id) = auxhist19_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist19_begin_s
SUBROUTINE nl_set_auxhist19_begin ( id_id , auxhist19_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist19_begin
  INTEGER id_id
  model_config_rec%auxhist19_begin(id_id) = auxhist19_begin
  RETURN
END SUBROUTINE nl_set_auxhist19_begin
SUBROUTINE nl_set_auxhist19_end_y ( id_id , auxhist19_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist19_end_y
  INTEGER id_id
  model_config_rec%auxhist19_end_y(id_id) = auxhist19_end_y
  RETURN
END SUBROUTINE nl_set_auxhist19_end_y
SUBROUTINE nl_set_auxhist19_end_d ( id_id , auxhist19_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist19_end_d
  INTEGER id_id
  model_config_rec%auxhist19_end_d(id_id) = auxhist19_end_d
  RETURN
END SUBROUTINE nl_set_auxhist19_end_d
SUBROUTINE nl_set_auxhist19_end_h ( id_id , auxhist19_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist19_end_h
  INTEGER id_id
  model_config_rec%auxhist19_end_h(id_id) = auxhist19_end_h
  RETURN
END SUBROUTINE nl_set_auxhist19_end_h
SUBROUTINE nl_set_auxhist19_end_m ( id_id , auxhist19_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist19_end_m
  INTEGER id_id
  model_config_rec%auxhist19_end_m(id_id) = auxhist19_end_m
  RETURN
END SUBROUTINE nl_set_auxhist19_end_m
SUBROUTINE nl_set_auxhist19_end_s ( id_id , auxhist19_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist19_end_s
  INTEGER id_id
  model_config_rec%auxhist19_end_s(id_id) = auxhist19_end_s
  RETURN
END SUBROUTINE nl_set_auxhist19_end_s
SUBROUTINE nl_set_auxhist19_end ( id_id , auxhist19_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist19_end
  INTEGER id_id
  model_config_rec%auxhist19_end(id_id) = auxhist19_end
  RETURN
END SUBROUTINE nl_set_auxhist19_end
SUBROUTINE nl_set_io_form_auxhist19 ( id_id , io_form_auxhist19 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist19
  INTEGER id_id
  model_config_rec%io_form_auxhist19 = io_form_auxhist19 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist19
SUBROUTINE nl_set_frames_per_auxhist19 ( id_id , frames_per_auxhist19 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist19
  INTEGER id_id
  model_config_rec%frames_per_auxhist19(id_id) = frames_per_auxhist19
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist19
SUBROUTINE nl_set_auxhist20_inname ( id_id , auxhist20_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist20_inname
  INTEGER id_id
  model_config_rec%auxhist20_inname = trim(auxhist20_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist20_inname
SUBROUTINE nl_set_auxhist20_outname ( id_id , auxhist20_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist20_outname
  INTEGER id_id
  model_config_rec%auxhist20_outname = trim(auxhist20_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist20_outname
SUBROUTINE nl_set_auxhist20_interval_y ( id_id , auxhist20_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist20_interval_y
  INTEGER id_id
  model_config_rec%auxhist20_interval_y(id_id) = auxhist20_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist20_interval_y
SUBROUTINE nl_set_auxhist20_interval_d ( id_id , auxhist20_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist20_interval_d
  INTEGER id_id
  model_config_rec%auxhist20_interval_d(id_id) = auxhist20_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist20_interval_d
SUBROUTINE nl_set_auxhist20_interval_h ( id_id , auxhist20_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist20_interval_h
  INTEGER id_id
  model_config_rec%auxhist20_interval_h(id_id) = auxhist20_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist20_interval_h
SUBROUTINE nl_set_auxhist20_interval_m ( id_id , auxhist20_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist20_interval_m
  INTEGER id_id
  model_config_rec%auxhist20_interval_m(id_id) = auxhist20_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist20_interval_m
SUBROUTINE nl_set_auxhist20_interval_s ( id_id , auxhist20_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist20_interval_s
  INTEGER id_id
  model_config_rec%auxhist20_interval_s(id_id) = auxhist20_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist20_interval_s
SUBROUTINE nl_set_auxhist20_interval ( id_id , auxhist20_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist20_interval
  INTEGER id_id
  model_config_rec%auxhist20_interval(id_id) = auxhist20_interval
  RETURN
END SUBROUTINE nl_set_auxhist20_interval
SUBROUTINE nl_set_auxhist20_begin_y ( id_id , auxhist20_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist20_begin_y
  INTEGER id_id
  model_config_rec%auxhist20_begin_y(id_id) = auxhist20_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist20_begin_y
SUBROUTINE nl_set_auxhist20_begin_d ( id_id , auxhist20_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist20_begin_d
  INTEGER id_id
  model_config_rec%auxhist20_begin_d(id_id) = auxhist20_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist20_begin_d
SUBROUTINE nl_set_auxhist20_begin_h ( id_id , auxhist20_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist20_begin_h
  INTEGER id_id
  model_config_rec%auxhist20_begin_h(id_id) = auxhist20_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist20_begin_h
SUBROUTINE nl_set_auxhist20_begin_m ( id_id , auxhist20_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist20_begin_m
  INTEGER id_id
  model_config_rec%auxhist20_begin_m(id_id) = auxhist20_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist20_begin_m
SUBROUTINE nl_set_auxhist20_begin_s ( id_id , auxhist20_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist20_begin_s
  INTEGER id_id
  model_config_rec%auxhist20_begin_s(id_id) = auxhist20_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist20_begin_s
SUBROUTINE nl_set_auxhist20_begin ( id_id , auxhist20_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist20_begin
  INTEGER id_id
  model_config_rec%auxhist20_begin(id_id) = auxhist20_begin
  RETURN
END SUBROUTINE nl_set_auxhist20_begin
SUBROUTINE nl_set_auxhist20_end_y ( id_id , auxhist20_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist20_end_y
  INTEGER id_id
  model_config_rec%auxhist20_end_y(id_id) = auxhist20_end_y
  RETURN
END SUBROUTINE nl_set_auxhist20_end_y
SUBROUTINE nl_set_auxhist20_end_d ( id_id , auxhist20_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist20_end_d
  INTEGER id_id
  model_config_rec%auxhist20_end_d(id_id) = auxhist20_end_d
  RETURN
END SUBROUTINE nl_set_auxhist20_end_d
SUBROUTINE nl_set_auxhist20_end_h ( id_id , auxhist20_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist20_end_h
  INTEGER id_id
  model_config_rec%auxhist20_end_h(id_id) = auxhist20_end_h
  RETURN
END SUBROUTINE nl_set_auxhist20_end_h
SUBROUTINE nl_set_auxhist20_end_m ( id_id , auxhist20_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist20_end_m
  INTEGER id_id
  model_config_rec%auxhist20_end_m(id_id) = auxhist20_end_m
  RETURN
END SUBROUTINE nl_set_auxhist20_end_m
SUBROUTINE nl_set_auxhist20_end_s ( id_id , auxhist20_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist20_end_s
  INTEGER id_id
  model_config_rec%auxhist20_end_s(id_id) = auxhist20_end_s
  RETURN
END SUBROUTINE nl_set_auxhist20_end_s
SUBROUTINE nl_set_auxhist20_end ( id_id , auxhist20_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist20_end
  INTEGER id_id
  model_config_rec%auxhist20_end(id_id) = auxhist20_end
  RETURN
END SUBROUTINE nl_set_auxhist20_end
SUBROUTINE nl_set_io_form_auxhist20 ( id_id , io_form_auxhist20 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist20
  INTEGER id_id
  model_config_rec%io_form_auxhist20 = io_form_auxhist20 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist20
SUBROUTINE nl_set_frames_per_auxhist20 ( id_id , frames_per_auxhist20 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist20
  INTEGER id_id
  model_config_rec%frames_per_auxhist20(id_id) = frames_per_auxhist20
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist20
SUBROUTINE nl_set_auxhist21_inname ( id_id , auxhist21_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist21_inname
  INTEGER id_id
  model_config_rec%auxhist21_inname = trim(auxhist21_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist21_inname
SUBROUTINE nl_set_auxhist21_outname ( id_id , auxhist21_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist21_outname
  INTEGER id_id
  model_config_rec%auxhist21_outname = trim(auxhist21_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist21_outname
SUBROUTINE nl_set_auxhist21_interval_y ( id_id , auxhist21_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist21_interval_y
  INTEGER id_id
  model_config_rec%auxhist21_interval_y(id_id) = auxhist21_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist21_interval_y
SUBROUTINE nl_set_auxhist21_interval_d ( id_id , auxhist21_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist21_interval_d
  INTEGER id_id
  model_config_rec%auxhist21_interval_d(id_id) = auxhist21_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist21_interval_d
SUBROUTINE nl_set_auxhist21_interval_h ( id_id , auxhist21_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist21_interval_h
  INTEGER id_id
  model_config_rec%auxhist21_interval_h(id_id) = auxhist21_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist21_interval_h
SUBROUTINE nl_set_auxhist21_interval_m ( id_id , auxhist21_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist21_interval_m
  INTEGER id_id
  model_config_rec%auxhist21_interval_m(id_id) = auxhist21_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist21_interval_m
SUBROUTINE nl_set_auxhist21_interval_s ( id_id , auxhist21_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist21_interval_s
  INTEGER id_id
  model_config_rec%auxhist21_interval_s(id_id) = auxhist21_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist21_interval_s
SUBROUTINE nl_set_auxhist21_interval ( id_id , auxhist21_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist21_interval
  INTEGER id_id
  model_config_rec%auxhist21_interval(id_id) = auxhist21_interval
  RETURN
END SUBROUTINE nl_set_auxhist21_interval
SUBROUTINE nl_set_auxhist21_begin_y ( id_id , auxhist21_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist21_begin_y
  INTEGER id_id
  model_config_rec%auxhist21_begin_y(id_id) = auxhist21_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist21_begin_y
SUBROUTINE nl_set_auxhist21_begin_d ( id_id , auxhist21_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist21_begin_d
  INTEGER id_id
  model_config_rec%auxhist21_begin_d(id_id) = auxhist21_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist21_begin_d
SUBROUTINE nl_set_auxhist21_begin_h ( id_id , auxhist21_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist21_begin_h
  INTEGER id_id
  model_config_rec%auxhist21_begin_h(id_id) = auxhist21_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist21_begin_h
SUBROUTINE nl_set_auxhist21_begin_m ( id_id , auxhist21_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist21_begin_m
  INTEGER id_id
  model_config_rec%auxhist21_begin_m(id_id) = auxhist21_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist21_begin_m
SUBROUTINE nl_set_auxhist21_begin_s ( id_id , auxhist21_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist21_begin_s
  INTEGER id_id
  model_config_rec%auxhist21_begin_s(id_id) = auxhist21_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist21_begin_s
SUBROUTINE nl_set_auxhist21_begin ( id_id , auxhist21_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist21_begin
  INTEGER id_id
  model_config_rec%auxhist21_begin(id_id) = auxhist21_begin
  RETURN
END SUBROUTINE nl_set_auxhist21_begin
SUBROUTINE nl_set_auxhist21_end_y ( id_id , auxhist21_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist21_end_y
  INTEGER id_id
  model_config_rec%auxhist21_end_y(id_id) = auxhist21_end_y
  RETURN
END SUBROUTINE nl_set_auxhist21_end_y
SUBROUTINE nl_set_auxhist21_end_d ( id_id , auxhist21_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist21_end_d
  INTEGER id_id
  model_config_rec%auxhist21_end_d(id_id) = auxhist21_end_d
  RETURN
END SUBROUTINE nl_set_auxhist21_end_d
SUBROUTINE nl_set_auxhist21_end_h ( id_id , auxhist21_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist21_end_h
  INTEGER id_id
  model_config_rec%auxhist21_end_h(id_id) = auxhist21_end_h
  RETURN
END SUBROUTINE nl_set_auxhist21_end_h
SUBROUTINE nl_set_auxhist21_end_m ( id_id , auxhist21_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist21_end_m
  INTEGER id_id
  model_config_rec%auxhist21_end_m(id_id) = auxhist21_end_m
  RETURN
END SUBROUTINE nl_set_auxhist21_end_m
SUBROUTINE nl_set_auxhist21_end_s ( id_id , auxhist21_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist21_end_s
  INTEGER id_id
  model_config_rec%auxhist21_end_s(id_id) = auxhist21_end_s
  RETURN
END SUBROUTINE nl_set_auxhist21_end_s
SUBROUTINE nl_set_auxhist21_end ( id_id , auxhist21_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist21_end
  INTEGER id_id
  model_config_rec%auxhist21_end(id_id) = auxhist21_end
  RETURN
END SUBROUTINE nl_set_auxhist21_end
SUBROUTINE nl_set_io_form_auxhist21 ( id_id , io_form_auxhist21 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist21
  INTEGER id_id
  model_config_rec%io_form_auxhist21 = io_form_auxhist21 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist21
SUBROUTINE nl_set_frames_per_auxhist21 ( id_id , frames_per_auxhist21 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist21
  INTEGER id_id
  model_config_rec%frames_per_auxhist21(id_id) = frames_per_auxhist21
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist21
SUBROUTINE nl_set_auxhist22_inname ( id_id , auxhist22_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist22_inname
  INTEGER id_id
  model_config_rec%auxhist22_inname = trim(auxhist22_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist22_inname
SUBROUTINE nl_set_auxhist22_outname ( id_id , auxhist22_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist22_outname
  INTEGER id_id
  model_config_rec%auxhist22_outname = trim(auxhist22_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist22_outname
SUBROUTINE nl_set_auxhist22_interval_y ( id_id , auxhist22_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist22_interval_y
  INTEGER id_id
  model_config_rec%auxhist22_interval_y(id_id) = auxhist22_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist22_interval_y
SUBROUTINE nl_set_auxhist22_interval_d ( id_id , auxhist22_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist22_interval_d
  INTEGER id_id
  model_config_rec%auxhist22_interval_d(id_id) = auxhist22_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist22_interval_d
SUBROUTINE nl_set_auxhist22_interval_h ( id_id , auxhist22_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist22_interval_h
  INTEGER id_id
  model_config_rec%auxhist22_interval_h(id_id) = auxhist22_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist22_interval_h
SUBROUTINE nl_set_auxhist22_interval_m ( id_id , auxhist22_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist22_interval_m
  INTEGER id_id
  model_config_rec%auxhist22_interval_m(id_id) = auxhist22_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist22_interval_m
SUBROUTINE nl_set_auxhist22_interval_s ( id_id , auxhist22_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist22_interval_s
  INTEGER id_id
  model_config_rec%auxhist22_interval_s(id_id) = auxhist22_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist22_interval_s
SUBROUTINE nl_set_auxhist22_interval ( id_id , auxhist22_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist22_interval
  INTEGER id_id
  model_config_rec%auxhist22_interval(id_id) = auxhist22_interval
  RETURN
END SUBROUTINE nl_set_auxhist22_interval
SUBROUTINE nl_set_auxhist22_begin_y ( id_id , auxhist22_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist22_begin_y
  INTEGER id_id
  model_config_rec%auxhist22_begin_y(id_id) = auxhist22_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist22_begin_y
SUBROUTINE nl_set_auxhist22_begin_d ( id_id , auxhist22_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist22_begin_d
  INTEGER id_id
  model_config_rec%auxhist22_begin_d(id_id) = auxhist22_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist22_begin_d
SUBROUTINE nl_set_auxhist22_begin_h ( id_id , auxhist22_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist22_begin_h
  INTEGER id_id
  model_config_rec%auxhist22_begin_h(id_id) = auxhist22_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist22_begin_h
SUBROUTINE nl_set_auxhist22_begin_m ( id_id , auxhist22_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist22_begin_m
  INTEGER id_id
  model_config_rec%auxhist22_begin_m(id_id) = auxhist22_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist22_begin_m
SUBROUTINE nl_set_auxhist22_begin_s ( id_id , auxhist22_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist22_begin_s
  INTEGER id_id
  model_config_rec%auxhist22_begin_s(id_id) = auxhist22_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist22_begin_s
SUBROUTINE nl_set_auxhist22_begin ( id_id , auxhist22_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist22_begin
  INTEGER id_id
  model_config_rec%auxhist22_begin(id_id) = auxhist22_begin
  RETURN
END SUBROUTINE nl_set_auxhist22_begin
SUBROUTINE nl_set_auxhist22_end_y ( id_id , auxhist22_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist22_end_y
  INTEGER id_id
  model_config_rec%auxhist22_end_y(id_id) = auxhist22_end_y
  RETURN
END SUBROUTINE nl_set_auxhist22_end_y
SUBROUTINE nl_set_auxhist22_end_d ( id_id , auxhist22_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist22_end_d
  INTEGER id_id
  model_config_rec%auxhist22_end_d(id_id) = auxhist22_end_d
  RETURN
END SUBROUTINE nl_set_auxhist22_end_d
SUBROUTINE nl_set_auxhist22_end_h ( id_id , auxhist22_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist22_end_h
  INTEGER id_id
  model_config_rec%auxhist22_end_h(id_id) = auxhist22_end_h
  RETURN
END SUBROUTINE nl_set_auxhist22_end_h
SUBROUTINE nl_set_auxhist22_end_m ( id_id , auxhist22_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist22_end_m
  INTEGER id_id
  model_config_rec%auxhist22_end_m(id_id) = auxhist22_end_m
  RETURN
END SUBROUTINE nl_set_auxhist22_end_m
SUBROUTINE nl_set_auxhist22_end_s ( id_id , auxhist22_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist22_end_s
  INTEGER id_id
  model_config_rec%auxhist22_end_s(id_id) = auxhist22_end_s
  RETURN
END SUBROUTINE nl_set_auxhist22_end_s
SUBROUTINE nl_set_auxhist22_end ( id_id , auxhist22_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist22_end
  INTEGER id_id
  model_config_rec%auxhist22_end(id_id) = auxhist22_end
  RETURN
END SUBROUTINE nl_set_auxhist22_end
SUBROUTINE nl_set_io_form_auxhist22 ( id_id , io_form_auxhist22 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist22
  INTEGER id_id
  model_config_rec%io_form_auxhist22 = io_form_auxhist22 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist22
SUBROUTINE nl_set_frames_per_auxhist22 ( id_id , frames_per_auxhist22 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist22
  INTEGER id_id
  model_config_rec%frames_per_auxhist22(id_id) = frames_per_auxhist22
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist22
SUBROUTINE nl_set_auxhist23_inname ( id_id , auxhist23_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist23_inname
  INTEGER id_id
  model_config_rec%auxhist23_inname = trim(auxhist23_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist23_inname
SUBROUTINE nl_set_auxhist23_outname ( id_id , auxhist23_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist23_outname
  INTEGER id_id
  model_config_rec%auxhist23_outname = trim(auxhist23_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist23_outname
SUBROUTINE nl_set_auxhist23_interval_y ( id_id , auxhist23_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist23_interval_y
  INTEGER id_id
  model_config_rec%auxhist23_interval_y(id_id) = auxhist23_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist23_interval_y
SUBROUTINE nl_set_auxhist23_interval_d ( id_id , auxhist23_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist23_interval_d
  INTEGER id_id
  model_config_rec%auxhist23_interval_d(id_id) = auxhist23_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist23_interval_d
#endif
#if (NNN == 3)
SUBROUTINE nl_set_auxhist23_interval_h ( id_id , auxhist23_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist23_interval_h
  INTEGER id_id
  model_config_rec%auxhist23_interval_h(id_id) = auxhist23_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist23_interval_h
SUBROUTINE nl_set_auxhist23_interval_m ( id_id , auxhist23_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist23_interval_m
  INTEGER id_id
  model_config_rec%auxhist23_interval_m(id_id) = auxhist23_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist23_interval_m
SUBROUTINE nl_set_auxhist23_interval_s ( id_id , auxhist23_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist23_interval_s
  INTEGER id_id
  model_config_rec%auxhist23_interval_s(id_id) = auxhist23_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist23_interval_s
SUBROUTINE nl_set_auxhist23_interval ( id_id , auxhist23_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist23_interval
  INTEGER id_id
  model_config_rec%auxhist23_interval(id_id) = auxhist23_interval
  RETURN
END SUBROUTINE nl_set_auxhist23_interval
SUBROUTINE nl_set_auxhist23_begin_y ( id_id , auxhist23_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist23_begin_y
  INTEGER id_id
  model_config_rec%auxhist23_begin_y(id_id) = auxhist23_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist23_begin_y
SUBROUTINE nl_set_auxhist23_begin_d ( id_id , auxhist23_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist23_begin_d
  INTEGER id_id
  model_config_rec%auxhist23_begin_d(id_id) = auxhist23_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist23_begin_d
SUBROUTINE nl_set_auxhist23_begin_h ( id_id , auxhist23_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist23_begin_h
  INTEGER id_id
  model_config_rec%auxhist23_begin_h(id_id) = auxhist23_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist23_begin_h
SUBROUTINE nl_set_auxhist23_begin_m ( id_id , auxhist23_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist23_begin_m
  INTEGER id_id
  model_config_rec%auxhist23_begin_m(id_id) = auxhist23_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist23_begin_m
SUBROUTINE nl_set_auxhist23_begin_s ( id_id , auxhist23_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist23_begin_s
  INTEGER id_id
  model_config_rec%auxhist23_begin_s(id_id) = auxhist23_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist23_begin_s
SUBROUTINE nl_set_auxhist23_begin ( id_id , auxhist23_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist23_begin
  INTEGER id_id
  model_config_rec%auxhist23_begin(id_id) = auxhist23_begin
  RETURN
END SUBROUTINE nl_set_auxhist23_begin
SUBROUTINE nl_set_auxhist23_end_y ( id_id , auxhist23_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist23_end_y
  INTEGER id_id
  model_config_rec%auxhist23_end_y(id_id) = auxhist23_end_y
  RETURN
END SUBROUTINE nl_set_auxhist23_end_y
SUBROUTINE nl_set_auxhist23_end_d ( id_id , auxhist23_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist23_end_d
  INTEGER id_id
  model_config_rec%auxhist23_end_d(id_id) = auxhist23_end_d
  RETURN
END SUBROUTINE nl_set_auxhist23_end_d
SUBROUTINE nl_set_auxhist23_end_h ( id_id , auxhist23_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist23_end_h
  INTEGER id_id
  model_config_rec%auxhist23_end_h(id_id) = auxhist23_end_h
  RETURN
END SUBROUTINE nl_set_auxhist23_end_h
SUBROUTINE nl_set_auxhist23_end_m ( id_id , auxhist23_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist23_end_m
  INTEGER id_id
  model_config_rec%auxhist23_end_m(id_id) = auxhist23_end_m
  RETURN
END SUBROUTINE nl_set_auxhist23_end_m
SUBROUTINE nl_set_auxhist23_end_s ( id_id , auxhist23_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist23_end_s
  INTEGER id_id
  model_config_rec%auxhist23_end_s(id_id) = auxhist23_end_s
  RETURN
END SUBROUTINE nl_set_auxhist23_end_s
SUBROUTINE nl_set_auxhist23_end ( id_id , auxhist23_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist23_end
  INTEGER id_id
  model_config_rec%auxhist23_end(id_id) = auxhist23_end
  RETURN
END SUBROUTINE nl_set_auxhist23_end
SUBROUTINE nl_set_io_form_auxhist23 ( id_id , io_form_auxhist23 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist23
  INTEGER id_id
  model_config_rec%io_form_auxhist23 = io_form_auxhist23 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist23
SUBROUTINE nl_set_frames_per_auxhist23 ( id_id , frames_per_auxhist23 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist23
  INTEGER id_id
  model_config_rec%frames_per_auxhist23(id_id) = frames_per_auxhist23
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist23
SUBROUTINE nl_set_auxhist24_inname ( id_id , auxhist24_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist24_inname
  INTEGER id_id
  model_config_rec%auxhist24_inname = trim(auxhist24_inname) 
  RETURN
END SUBROUTINE nl_set_auxhist24_inname
SUBROUTINE nl_set_auxhist24_outname ( id_id , auxhist24_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxhist24_outname
  INTEGER id_id
  model_config_rec%auxhist24_outname = trim(auxhist24_outname) 
  RETURN
END SUBROUTINE nl_set_auxhist24_outname
SUBROUTINE nl_set_auxhist24_interval_y ( id_id , auxhist24_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist24_interval_y
  INTEGER id_id
  model_config_rec%auxhist24_interval_y(id_id) = auxhist24_interval_y
  RETURN
END SUBROUTINE nl_set_auxhist24_interval_y
SUBROUTINE nl_set_auxhist24_interval_d ( id_id , auxhist24_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist24_interval_d
  INTEGER id_id
  model_config_rec%auxhist24_interval_d(id_id) = auxhist24_interval_d
  RETURN
END SUBROUTINE nl_set_auxhist24_interval_d
SUBROUTINE nl_set_auxhist24_interval_h ( id_id , auxhist24_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist24_interval_h
  INTEGER id_id
  model_config_rec%auxhist24_interval_h(id_id) = auxhist24_interval_h
  RETURN
END SUBROUTINE nl_set_auxhist24_interval_h
SUBROUTINE nl_set_auxhist24_interval_m ( id_id , auxhist24_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist24_interval_m
  INTEGER id_id
  model_config_rec%auxhist24_interval_m(id_id) = auxhist24_interval_m
  RETURN
END SUBROUTINE nl_set_auxhist24_interval_m
SUBROUTINE nl_set_auxhist24_interval_s ( id_id , auxhist24_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist24_interval_s
  INTEGER id_id
  model_config_rec%auxhist24_interval_s(id_id) = auxhist24_interval_s
  RETURN
END SUBROUTINE nl_set_auxhist24_interval_s
SUBROUTINE nl_set_auxhist24_interval ( id_id , auxhist24_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist24_interval
  INTEGER id_id
  model_config_rec%auxhist24_interval(id_id) = auxhist24_interval
  RETURN
END SUBROUTINE nl_set_auxhist24_interval
SUBROUTINE nl_set_auxhist24_begin_y ( id_id , auxhist24_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist24_begin_y
  INTEGER id_id
  model_config_rec%auxhist24_begin_y(id_id) = auxhist24_begin_y
  RETURN
END SUBROUTINE nl_set_auxhist24_begin_y
SUBROUTINE nl_set_auxhist24_begin_d ( id_id , auxhist24_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist24_begin_d
  INTEGER id_id
  model_config_rec%auxhist24_begin_d(id_id) = auxhist24_begin_d
  RETURN
END SUBROUTINE nl_set_auxhist24_begin_d
SUBROUTINE nl_set_auxhist24_begin_h ( id_id , auxhist24_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist24_begin_h
  INTEGER id_id
  model_config_rec%auxhist24_begin_h(id_id) = auxhist24_begin_h
  RETURN
END SUBROUTINE nl_set_auxhist24_begin_h
SUBROUTINE nl_set_auxhist24_begin_m ( id_id , auxhist24_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist24_begin_m
  INTEGER id_id
  model_config_rec%auxhist24_begin_m(id_id) = auxhist24_begin_m
  RETURN
END SUBROUTINE nl_set_auxhist24_begin_m
SUBROUTINE nl_set_auxhist24_begin_s ( id_id , auxhist24_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist24_begin_s
  INTEGER id_id
  model_config_rec%auxhist24_begin_s(id_id) = auxhist24_begin_s
  RETURN
END SUBROUTINE nl_set_auxhist24_begin_s
SUBROUTINE nl_set_auxhist24_begin ( id_id , auxhist24_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist24_begin
  INTEGER id_id
  model_config_rec%auxhist24_begin(id_id) = auxhist24_begin
  RETURN
END SUBROUTINE nl_set_auxhist24_begin
SUBROUTINE nl_set_auxhist24_end_y ( id_id , auxhist24_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist24_end_y
  INTEGER id_id
  model_config_rec%auxhist24_end_y(id_id) = auxhist24_end_y
  RETURN
END SUBROUTINE nl_set_auxhist24_end_y
SUBROUTINE nl_set_auxhist24_end_d ( id_id , auxhist24_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist24_end_d
  INTEGER id_id
  model_config_rec%auxhist24_end_d(id_id) = auxhist24_end_d
  RETURN
END SUBROUTINE nl_set_auxhist24_end_d
SUBROUTINE nl_set_auxhist24_end_h ( id_id , auxhist24_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist24_end_h
  INTEGER id_id
  model_config_rec%auxhist24_end_h(id_id) = auxhist24_end_h
  RETURN
END SUBROUTINE nl_set_auxhist24_end_h
SUBROUTINE nl_set_auxhist24_end_m ( id_id , auxhist24_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist24_end_m
  INTEGER id_id
  model_config_rec%auxhist24_end_m(id_id) = auxhist24_end_m
  RETURN
END SUBROUTINE nl_set_auxhist24_end_m
SUBROUTINE nl_set_auxhist24_end_s ( id_id , auxhist24_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist24_end_s
  INTEGER id_id
  model_config_rec%auxhist24_end_s(id_id) = auxhist24_end_s
  RETURN
END SUBROUTINE nl_set_auxhist24_end_s
SUBROUTINE nl_set_auxhist24_end ( id_id , auxhist24_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxhist24_end
  INTEGER id_id
  model_config_rec%auxhist24_end(id_id) = auxhist24_end
  RETURN
END SUBROUTINE nl_set_auxhist24_end
SUBROUTINE nl_set_io_form_auxhist24 ( id_id , io_form_auxhist24 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxhist24
  INTEGER id_id
  model_config_rec%io_form_auxhist24 = io_form_auxhist24 
  RETURN
END SUBROUTINE nl_set_io_form_auxhist24
SUBROUTINE nl_set_frames_per_auxhist24 ( id_id , frames_per_auxhist24 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxhist24
  INTEGER id_id
  model_config_rec%frames_per_auxhist24(id_id) = frames_per_auxhist24
  RETURN
END SUBROUTINE nl_set_frames_per_auxhist24
SUBROUTINE nl_set_auxinput1_outname ( id_id , auxinput1_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput1_outname
  INTEGER id_id
  model_config_rec%auxinput1_outname = trim(auxinput1_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput1_outname
SUBROUTINE nl_set_auxinput1_interval_y ( id_id , auxinput1_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_interval_y
  INTEGER id_id
  model_config_rec%auxinput1_interval_y(id_id) = auxinput1_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput1_interval_y
SUBROUTINE nl_set_auxinput1_interval_d ( id_id , auxinput1_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_interval_d
  INTEGER id_id
  model_config_rec%auxinput1_interval_d(id_id) = auxinput1_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput1_interval_d
SUBROUTINE nl_set_auxinput1_interval_h ( id_id , auxinput1_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_interval_h
  INTEGER id_id
  model_config_rec%auxinput1_interval_h(id_id) = auxinput1_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput1_interval_h
SUBROUTINE nl_set_auxinput1_interval_m ( id_id , auxinput1_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_interval_m
  INTEGER id_id
  model_config_rec%auxinput1_interval_m(id_id) = auxinput1_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput1_interval_m
SUBROUTINE nl_set_auxinput1_interval_s ( id_id , auxinput1_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_interval_s
  INTEGER id_id
  model_config_rec%auxinput1_interval_s(id_id) = auxinput1_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput1_interval_s
SUBROUTINE nl_set_auxinput1_interval ( id_id , auxinput1_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_interval
  INTEGER id_id
  model_config_rec%auxinput1_interval(id_id) = auxinput1_interval
  RETURN
END SUBROUTINE nl_set_auxinput1_interval
SUBROUTINE nl_set_auxinput1_begin_y ( id_id , auxinput1_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_begin_y
  INTEGER id_id
  model_config_rec%auxinput1_begin_y(id_id) = auxinput1_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_y
SUBROUTINE nl_set_auxinput1_begin_d ( id_id , auxinput1_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_begin_d
  INTEGER id_id
  model_config_rec%auxinput1_begin_d(id_id) = auxinput1_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_d
SUBROUTINE nl_set_auxinput1_begin_h ( id_id , auxinput1_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_begin_h
  INTEGER id_id
  model_config_rec%auxinput1_begin_h(id_id) = auxinput1_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_h
SUBROUTINE nl_set_auxinput1_begin_m ( id_id , auxinput1_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_begin_m
  INTEGER id_id
  model_config_rec%auxinput1_begin_m(id_id) = auxinput1_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_m
SUBROUTINE nl_set_auxinput1_begin_s ( id_id , auxinput1_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_begin_s
  INTEGER id_id
  model_config_rec%auxinput1_begin_s(id_id) = auxinput1_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput1_begin_s
SUBROUTINE nl_set_auxinput1_begin ( id_id , auxinput1_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_begin
  INTEGER id_id
  model_config_rec%auxinput1_begin(id_id) = auxinput1_begin
  RETURN
END SUBROUTINE nl_set_auxinput1_begin
SUBROUTINE nl_set_auxinput1_end_y ( id_id , auxinput1_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_end_y
  INTEGER id_id
  model_config_rec%auxinput1_end_y(id_id) = auxinput1_end_y
  RETURN
END SUBROUTINE nl_set_auxinput1_end_y
SUBROUTINE nl_set_auxinput1_end_d ( id_id , auxinput1_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_end_d
  INTEGER id_id
  model_config_rec%auxinput1_end_d(id_id) = auxinput1_end_d
  RETURN
END SUBROUTINE nl_set_auxinput1_end_d
SUBROUTINE nl_set_auxinput1_end_h ( id_id , auxinput1_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_end_h
  INTEGER id_id
  model_config_rec%auxinput1_end_h(id_id) = auxinput1_end_h
  RETURN
END SUBROUTINE nl_set_auxinput1_end_h
SUBROUTINE nl_set_auxinput1_end_m ( id_id , auxinput1_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_end_m
  INTEGER id_id
  model_config_rec%auxinput1_end_m(id_id) = auxinput1_end_m
  RETURN
END SUBROUTINE nl_set_auxinput1_end_m
SUBROUTINE nl_set_auxinput1_end_s ( id_id , auxinput1_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_end_s
  INTEGER id_id
  model_config_rec%auxinput1_end_s(id_id) = auxinput1_end_s
  RETURN
END SUBROUTINE nl_set_auxinput1_end_s
SUBROUTINE nl_set_auxinput1_end ( id_id , auxinput1_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput1_end
  INTEGER id_id
  model_config_rec%auxinput1_end(id_id) = auxinput1_end
  RETURN
END SUBROUTINE nl_set_auxinput1_end
SUBROUTINE nl_set_frames_per_auxinput1 ( id_id , frames_per_auxinput1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput1
  INTEGER id_id
  model_config_rec%frames_per_auxinput1(id_id) = frames_per_auxinput1
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput1
SUBROUTINE nl_set_auxinput2_inname ( id_id , auxinput2_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput2_inname
  INTEGER id_id
  model_config_rec%auxinput2_inname = trim(auxinput2_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput2_inname
SUBROUTINE nl_set_auxinput2_outname ( id_id , auxinput2_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput2_outname
  INTEGER id_id
  model_config_rec%auxinput2_outname = trim(auxinput2_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput2_outname
SUBROUTINE nl_set_auxinput2_interval_y ( id_id , auxinput2_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_interval_y
  INTEGER id_id
  model_config_rec%auxinput2_interval_y(id_id) = auxinput2_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput2_interval_y
SUBROUTINE nl_set_auxinput2_interval_d ( id_id , auxinput2_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_interval_d
  INTEGER id_id
  model_config_rec%auxinput2_interval_d(id_id) = auxinput2_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput2_interval_d
SUBROUTINE nl_set_auxinput2_interval_h ( id_id , auxinput2_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_interval_h
  INTEGER id_id
  model_config_rec%auxinput2_interval_h(id_id) = auxinput2_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput2_interval_h
SUBROUTINE nl_set_auxinput2_interval_m ( id_id , auxinput2_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_interval_m
  INTEGER id_id
  model_config_rec%auxinput2_interval_m(id_id) = auxinput2_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput2_interval_m
SUBROUTINE nl_set_auxinput2_interval_s ( id_id , auxinput2_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_interval_s
  INTEGER id_id
  model_config_rec%auxinput2_interval_s(id_id) = auxinput2_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput2_interval_s
SUBROUTINE nl_set_auxinput2_interval ( id_id , auxinput2_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_interval
  INTEGER id_id
  model_config_rec%auxinput2_interval(id_id) = auxinput2_interval
  RETURN
END SUBROUTINE nl_set_auxinput2_interval
SUBROUTINE nl_set_auxinput2_begin_y ( id_id , auxinput2_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_begin_y
  INTEGER id_id
  model_config_rec%auxinput2_begin_y(id_id) = auxinput2_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_y
SUBROUTINE nl_set_auxinput2_begin_d ( id_id , auxinput2_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_begin_d
  INTEGER id_id
  model_config_rec%auxinput2_begin_d(id_id) = auxinput2_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_d
SUBROUTINE nl_set_auxinput2_begin_h ( id_id , auxinput2_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_begin_h
  INTEGER id_id
  model_config_rec%auxinput2_begin_h(id_id) = auxinput2_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_h
SUBROUTINE nl_set_auxinput2_begin_m ( id_id , auxinput2_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_begin_m
  INTEGER id_id
  model_config_rec%auxinput2_begin_m(id_id) = auxinput2_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_m
SUBROUTINE nl_set_auxinput2_begin_s ( id_id , auxinput2_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_begin_s
  INTEGER id_id
  model_config_rec%auxinput2_begin_s(id_id) = auxinput2_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput2_begin_s
SUBROUTINE nl_set_auxinput2_begin ( id_id , auxinput2_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_begin
  INTEGER id_id
  model_config_rec%auxinput2_begin(id_id) = auxinput2_begin
  RETURN
END SUBROUTINE nl_set_auxinput2_begin
SUBROUTINE nl_set_auxinput2_end_y ( id_id , auxinput2_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_end_y
  INTEGER id_id
  model_config_rec%auxinput2_end_y(id_id) = auxinput2_end_y
  RETURN
END SUBROUTINE nl_set_auxinput2_end_y
SUBROUTINE nl_set_auxinput2_end_d ( id_id , auxinput2_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_end_d
  INTEGER id_id
  model_config_rec%auxinput2_end_d(id_id) = auxinput2_end_d
  RETURN
END SUBROUTINE nl_set_auxinput2_end_d
SUBROUTINE nl_set_auxinput2_end_h ( id_id , auxinput2_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_end_h
  INTEGER id_id
  model_config_rec%auxinput2_end_h(id_id) = auxinput2_end_h
  RETURN
END SUBROUTINE nl_set_auxinput2_end_h
SUBROUTINE nl_set_auxinput2_end_m ( id_id , auxinput2_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_end_m
  INTEGER id_id
  model_config_rec%auxinput2_end_m(id_id) = auxinput2_end_m
  RETURN
END SUBROUTINE nl_set_auxinput2_end_m
SUBROUTINE nl_set_auxinput2_end_s ( id_id , auxinput2_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_end_s
  INTEGER id_id
  model_config_rec%auxinput2_end_s(id_id) = auxinput2_end_s
  RETURN
END SUBROUTINE nl_set_auxinput2_end_s
SUBROUTINE nl_set_auxinput2_end ( id_id , auxinput2_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput2_end
  INTEGER id_id
  model_config_rec%auxinput2_end(id_id) = auxinput2_end
  RETURN
END SUBROUTINE nl_set_auxinput2_end
SUBROUTINE nl_set_frames_per_auxinput2 ( id_id , frames_per_auxinput2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput2
  INTEGER id_id
  model_config_rec%frames_per_auxinput2(id_id) = frames_per_auxinput2
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput2
SUBROUTINE nl_set_auxinput3_inname ( id_id , auxinput3_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput3_inname
  INTEGER id_id
  model_config_rec%auxinput3_inname = trim(auxinput3_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput3_inname
SUBROUTINE nl_set_auxinput3_outname ( id_id , auxinput3_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput3_outname
  INTEGER id_id
  model_config_rec%auxinput3_outname = trim(auxinput3_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput3_outname
SUBROUTINE nl_set_auxinput3_interval_y ( id_id , auxinput3_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_interval_y
  INTEGER id_id
  model_config_rec%auxinput3_interval_y(id_id) = auxinput3_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput3_interval_y
SUBROUTINE nl_set_auxinput3_interval_d ( id_id , auxinput3_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_interval_d
  INTEGER id_id
  model_config_rec%auxinput3_interval_d(id_id) = auxinput3_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput3_interval_d
SUBROUTINE nl_set_auxinput3_interval_h ( id_id , auxinput3_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_interval_h
  INTEGER id_id
  model_config_rec%auxinput3_interval_h(id_id) = auxinput3_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput3_interval_h
SUBROUTINE nl_set_auxinput3_interval_m ( id_id , auxinput3_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_interval_m
  INTEGER id_id
  model_config_rec%auxinput3_interval_m(id_id) = auxinput3_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput3_interval_m
SUBROUTINE nl_set_auxinput3_interval_s ( id_id , auxinput3_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_interval_s
  INTEGER id_id
  model_config_rec%auxinput3_interval_s(id_id) = auxinput3_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput3_interval_s
SUBROUTINE nl_set_auxinput3_interval ( id_id , auxinput3_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_interval
  INTEGER id_id
  model_config_rec%auxinput3_interval(id_id) = auxinput3_interval
  RETURN
END SUBROUTINE nl_set_auxinput3_interval
SUBROUTINE nl_set_auxinput3_begin_y ( id_id , auxinput3_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_begin_y
  INTEGER id_id
  model_config_rec%auxinput3_begin_y(id_id) = auxinput3_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_y
SUBROUTINE nl_set_auxinput3_begin_d ( id_id , auxinput3_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_begin_d
  INTEGER id_id
  model_config_rec%auxinput3_begin_d(id_id) = auxinput3_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_d
SUBROUTINE nl_set_auxinput3_begin_h ( id_id , auxinput3_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_begin_h
  INTEGER id_id
  model_config_rec%auxinput3_begin_h(id_id) = auxinput3_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_h
SUBROUTINE nl_set_auxinput3_begin_m ( id_id , auxinput3_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_begin_m
  INTEGER id_id
  model_config_rec%auxinput3_begin_m(id_id) = auxinput3_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_m
SUBROUTINE nl_set_auxinput3_begin_s ( id_id , auxinput3_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_begin_s
  INTEGER id_id
  model_config_rec%auxinput3_begin_s(id_id) = auxinput3_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput3_begin_s
SUBROUTINE nl_set_auxinput3_begin ( id_id , auxinput3_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_begin
  INTEGER id_id
  model_config_rec%auxinput3_begin(id_id) = auxinput3_begin
  RETURN
END SUBROUTINE nl_set_auxinput3_begin
SUBROUTINE nl_set_auxinput3_end_y ( id_id , auxinput3_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_end_y
  INTEGER id_id
  model_config_rec%auxinput3_end_y(id_id) = auxinput3_end_y
  RETURN
END SUBROUTINE nl_set_auxinput3_end_y
SUBROUTINE nl_set_auxinput3_end_d ( id_id , auxinput3_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_end_d
  INTEGER id_id
  model_config_rec%auxinput3_end_d(id_id) = auxinput3_end_d
  RETURN
END SUBROUTINE nl_set_auxinput3_end_d
SUBROUTINE nl_set_auxinput3_end_h ( id_id , auxinput3_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_end_h
  INTEGER id_id
  model_config_rec%auxinput3_end_h(id_id) = auxinput3_end_h
  RETURN
END SUBROUTINE nl_set_auxinput3_end_h
SUBROUTINE nl_set_auxinput3_end_m ( id_id , auxinput3_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_end_m
  INTEGER id_id
  model_config_rec%auxinput3_end_m(id_id) = auxinput3_end_m
  RETURN
END SUBROUTINE nl_set_auxinput3_end_m
SUBROUTINE nl_set_auxinput3_end_s ( id_id , auxinput3_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_end_s
  INTEGER id_id
  model_config_rec%auxinput3_end_s(id_id) = auxinput3_end_s
  RETURN
END SUBROUTINE nl_set_auxinput3_end_s
SUBROUTINE nl_set_auxinput3_end ( id_id , auxinput3_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput3_end
  INTEGER id_id
  model_config_rec%auxinput3_end(id_id) = auxinput3_end
  RETURN
END SUBROUTINE nl_set_auxinput3_end
SUBROUTINE nl_set_io_form_auxinput3 ( id_id , io_form_auxinput3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput3
  INTEGER id_id
  model_config_rec%io_form_auxinput3 = io_form_auxinput3 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput3
SUBROUTINE nl_set_frames_per_auxinput3 ( id_id , frames_per_auxinput3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput3
  INTEGER id_id
  model_config_rec%frames_per_auxinput3(id_id) = frames_per_auxinput3
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput3
SUBROUTINE nl_set_auxinput4_inname ( id_id , auxinput4_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput4_inname
  INTEGER id_id
  model_config_rec%auxinput4_inname = trim(auxinput4_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput4_inname
SUBROUTINE nl_set_auxinput4_outname ( id_id , auxinput4_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput4_outname
  INTEGER id_id
  model_config_rec%auxinput4_outname = trim(auxinput4_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput4_outname
SUBROUTINE nl_set_auxinput4_interval_y ( id_id , auxinput4_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_interval_y
  INTEGER id_id
  model_config_rec%auxinput4_interval_y(id_id) = auxinput4_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput4_interval_y
SUBROUTINE nl_set_auxinput4_interval_d ( id_id , auxinput4_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_interval_d
  INTEGER id_id
  model_config_rec%auxinput4_interval_d(id_id) = auxinput4_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput4_interval_d
SUBROUTINE nl_set_auxinput4_interval_h ( id_id , auxinput4_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_interval_h
  INTEGER id_id
  model_config_rec%auxinput4_interval_h(id_id) = auxinput4_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput4_interval_h
SUBROUTINE nl_set_auxinput4_interval_m ( id_id , auxinput4_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_interval_m
  INTEGER id_id
  model_config_rec%auxinput4_interval_m(id_id) = auxinput4_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput4_interval_m
SUBROUTINE nl_set_auxinput4_interval_s ( id_id , auxinput4_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_interval_s
  INTEGER id_id
  model_config_rec%auxinput4_interval_s(id_id) = auxinput4_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput4_interval_s
SUBROUTINE nl_set_auxinput4_interval ( id_id , auxinput4_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_interval
  INTEGER id_id
  model_config_rec%auxinput4_interval(id_id) = auxinput4_interval
  RETURN
END SUBROUTINE nl_set_auxinput4_interval
SUBROUTINE nl_set_auxinput4_begin_y ( id_id , auxinput4_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_begin_y
  INTEGER id_id
  model_config_rec%auxinput4_begin_y(id_id) = auxinput4_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_y
SUBROUTINE nl_set_auxinput4_begin_d ( id_id , auxinput4_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_begin_d
  INTEGER id_id
  model_config_rec%auxinput4_begin_d(id_id) = auxinput4_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_d
SUBROUTINE nl_set_auxinput4_begin_h ( id_id , auxinput4_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_begin_h
  INTEGER id_id
  model_config_rec%auxinput4_begin_h(id_id) = auxinput4_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_h
SUBROUTINE nl_set_auxinput4_begin_m ( id_id , auxinput4_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_begin_m
  INTEGER id_id
  model_config_rec%auxinput4_begin_m(id_id) = auxinput4_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_m
SUBROUTINE nl_set_auxinput4_begin_s ( id_id , auxinput4_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_begin_s
  INTEGER id_id
  model_config_rec%auxinput4_begin_s(id_id) = auxinput4_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput4_begin_s
SUBROUTINE nl_set_auxinput4_begin ( id_id , auxinput4_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_begin
  INTEGER id_id
  model_config_rec%auxinput4_begin(id_id) = auxinput4_begin
  RETURN
END SUBROUTINE nl_set_auxinput4_begin
SUBROUTINE nl_set_auxinput4_end_y ( id_id , auxinput4_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_end_y
  INTEGER id_id
  model_config_rec%auxinput4_end_y(id_id) = auxinput4_end_y
  RETURN
END SUBROUTINE nl_set_auxinput4_end_y
SUBROUTINE nl_set_auxinput4_end_d ( id_id , auxinput4_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_end_d
  INTEGER id_id
  model_config_rec%auxinput4_end_d(id_id) = auxinput4_end_d
  RETURN
END SUBROUTINE nl_set_auxinput4_end_d
SUBROUTINE nl_set_auxinput4_end_h ( id_id , auxinput4_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_end_h
  INTEGER id_id
  model_config_rec%auxinput4_end_h(id_id) = auxinput4_end_h
  RETURN
END SUBROUTINE nl_set_auxinput4_end_h
SUBROUTINE nl_set_auxinput4_end_m ( id_id , auxinput4_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_end_m
  INTEGER id_id
  model_config_rec%auxinput4_end_m(id_id) = auxinput4_end_m
  RETURN
END SUBROUTINE nl_set_auxinput4_end_m
SUBROUTINE nl_set_auxinput4_end_s ( id_id , auxinput4_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_end_s
  INTEGER id_id
  model_config_rec%auxinput4_end_s(id_id) = auxinput4_end_s
  RETURN
END SUBROUTINE nl_set_auxinput4_end_s
SUBROUTINE nl_set_auxinput4_end ( id_id , auxinput4_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput4_end
  INTEGER id_id
  model_config_rec%auxinput4_end(id_id) = auxinput4_end
  RETURN
END SUBROUTINE nl_set_auxinput4_end
SUBROUTINE nl_set_io_form_auxinput4 ( id_id , io_form_auxinput4 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput4
  INTEGER id_id
  model_config_rec%io_form_auxinput4 = io_form_auxinput4 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput4
SUBROUTINE nl_set_frames_per_auxinput4 ( id_id , frames_per_auxinput4 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput4
  INTEGER id_id
  model_config_rec%frames_per_auxinput4(id_id) = frames_per_auxinput4
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput4
SUBROUTINE nl_set_auxinput5_inname ( id_id , auxinput5_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput5_inname
  INTEGER id_id
  model_config_rec%auxinput5_inname = trim(auxinput5_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput5_inname
SUBROUTINE nl_set_auxinput5_outname ( id_id , auxinput5_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput5_outname
  INTEGER id_id
  model_config_rec%auxinput5_outname = trim(auxinput5_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput5_outname
SUBROUTINE nl_set_auxinput5_interval_y ( id_id , auxinput5_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_interval_y
  INTEGER id_id
  model_config_rec%auxinput5_interval_y(id_id) = auxinput5_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput5_interval_y
SUBROUTINE nl_set_auxinput5_interval_d ( id_id , auxinput5_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_interval_d
  INTEGER id_id
  model_config_rec%auxinput5_interval_d(id_id) = auxinput5_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput5_interval_d
SUBROUTINE nl_set_auxinput5_interval_h ( id_id , auxinput5_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_interval_h
  INTEGER id_id
  model_config_rec%auxinput5_interval_h(id_id) = auxinput5_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput5_interval_h
SUBROUTINE nl_set_auxinput5_interval_m ( id_id , auxinput5_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_interval_m
  INTEGER id_id
  model_config_rec%auxinput5_interval_m(id_id) = auxinput5_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput5_interval_m
SUBROUTINE nl_set_auxinput5_interval_s ( id_id , auxinput5_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_interval_s
  INTEGER id_id
  model_config_rec%auxinput5_interval_s(id_id) = auxinput5_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput5_interval_s
SUBROUTINE nl_set_auxinput5_interval ( id_id , auxinput5_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_interval
  INTEGER id_id
  model_config_rec%auxinput5_interval(id_id) = auxinput5_interval
  RETURN
END SUBROUTINE nl_set_auxinput5_interval
SUBROUTINE nl_set_auxinput5_begin_y ( id_id , auxinput5_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_begin_y
  INTEGER id_id
  model_config_rec%auxinput5_begin_y(id_id) = auxinput5_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_y
SUBROUTINE nl_set_auxinput5_begin_d ( id_id , auxinput5_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_begin_d
  INTEGER id_id
  model_config_rec%auxinput5_begin_d(id_id) = auxinput5_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_d
SUBROUTINE nl_set_auxinput5_begin_h ( id_id , auxinput5_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_begin_h
  INTEGER id_id
  model_config_rec%auxinput5_begin_h(id_id) = auxinput5_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_h
SUBROUTINE nl_set_auxinput5_begin_m ( id_id , auxinput5_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_begin_m
  INTEGER id_id
  model_config_rec%auxinput5_begin_m(id_id) = auxinput5_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_m
SUBROUTINE nl_set_auxinput5_begin_s ( id_id , auxinput5_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_begin_s
  INTEGER id_id
  model_config_rec%auxinput5_begin_s(id_id) = auxinput5_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput5_begin_s
SUBROUTINE nl_set_auxinput5_begin ( id_id , auxinput5_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_begin
  INTEGER id_id
  model_config_rec%auxinput5_begin(id_id) = auxinput5_begin
  RETURN
END SUBROUTINE nl_set_auxinput5_begin
SUBROUTINE nl_set_auxinput5_end_y ( id_id , auxinput5_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_end_y
  INTEGER id_id
  model_config_rec%auxinput5_end_y(id_id) = auxinput5_end_y
  RETURN
END SUBROUTINE nl_set_auxinput5_end_y
SUBROUTINE nl_set_auxinput5_end_d ( id_id , auxinput5_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_end_d
  INTEGER id_id
  model_config_rec%auxinput5_end_d(id_id) = auxinput5_end_d
  RETURN
END SUBROUTINE nl_set_auxinput5_end_d
SUBROUTINE nl_set_auxinput5_end_h ( id_id , auxinput5_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_end_h
  INTEGER id_id
  model_config_rec%auxinput5_end_h(id_id) = auxinput5_end_h
  RETURN
END SUBROUTINE nl_set_auxinput5_end_h
SUBROUTINE nl_set_auxinput5_end_m ( id_id , auxinput5_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_end_m
  INTEGER id_id
  model_config_rec%auxinput5_end_m(id_id) = auxinput5_end_m
  RETURN
END SUBROUTINE nl_set_auxinput5_end_m
SUBROUTINE nl_set_auxinput5_end_s ( id_id , auxinput5_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_end_s
  INTEGER id_id
  model_config_rec%auxinput5_end_s(id_id) = auxinput5_end_s
  RETURN
END SUBROUTINE nl_set_auxinput5_end_s
SUBROUTINE nl_set_auxinput5_end ( id_id , auxinput5_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput5_end
  INTEGER id_id
  model_config_rec%auxinput5_end(id_id) = auxinput5_end
  RETURN
END SUBROUTINE nl_set_auxinput5_end
SUBROUTINE nl_set_io_form_auxinput5 ( id_id , io_form_auxinput5 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput5
  INTEGER id_id
  model_config_rec%io_form_auxinput5 = io_form_auxinput5 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput5
SUBROUTINE nl_set_frames_per_auxinput5 ( id_id , frames_per_auxinput5 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput5
  INTEGER id_id
  model_config_rec%frames_per_auxinput5(id_id) = frames_per_auxinput5
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput5
SUBROUTINE nl_set_auxinput6_inname ( id_id , auxinput6_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput6_inname
  INTEGER id_id
  model_config_rec%auxinput6_inname = trim(auxinput6_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput6_inname
SUBROUTINE nl_set_auxinput6_outname ( id_id , auxinput6_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput6_outname
  INTEGER id_id
  model_config_rec%auxinput6_outname = trim(auxinput6_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput6_outname
SUBROUTINE nl_set_auxinput6_interval_y ( id_id , auxinput6_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_interval_y
  INTEGER id_id
  model_config_rec%auxinput6_interval_y(id_id) = auxinput6_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput6_interval_y
SUBROUTINE nl_set_auxinput6_interval_d ( id_id , auxinput6_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_interval_d
  INTEGER id_id
  model_config_rec%auxinput6_interval_d(id_id) = auxinput6_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput6_interval_d
SUBROUTINE nl_set_auxinput6_interval_h ( id_id , auxinput6_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_interval_h
  INTEGER id_id
  model_config_rec%auxinput6_interval_h(id_id) = auxinput6_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput6_interval_h
SUBROUTINE nl_set_auxinput6_interval_m ( id_id , auxinput6_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_interval_m
  INTEGER id_id
  model_config_rec%auxinput6_interval_m(id_id) = auxinput6_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput6_interval_m
SUBROUTINE nl_set_auxinput6_interval_s ( id_id , auxinput6_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_interval_s
  INTEGER id_id
  model_config_rec%auxinput6_interval_s(id_id) = auxinput6_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput6_interval_s
SUBROUTINE nl_set_auxinput6_interval ( id_id , auxinput6_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_interval
  INTEGER id_id
  model_config_rec%auxinput6_interval(id_id) = auxinput6_interval
  RETURN
END SUBROUTINE nl_set_auxinput6_interval
SUBROUTINE nl_set_auxinput6_begin_y ( id_id , auxinput6_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_begin_y
  INTEGER id_id
  model_config_rec%auxinput6_begin_y(id_id) = auxinput6_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput6_begin_y
SUBROUTINE nl_set_auxinput6_begin_d ( id_id , auxinput6_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_begin_d
  INTEGER id_id
  model_config_rec%auxinput6_begin_d(id_id) = auxinput6_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput6_begin_d
SUBROUTINE nl_set_auxinput6_begin_h ( id_id , auxinput6_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_begin_h
  INTEGER id_id
  model_config_rec%auxinput6_begin_h(id_id) = auxinput6_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput6_begin_h
SUBROUTINE nl_set_auxinput6_begin_m ( id_id , auxinput6_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_begin_m
  INTEGER id_id
  model_config_rec%auxinput6_begin_m(id_id) = auxinput6_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput6_begin_m
SUBROUTINE nl_set_auxinput6_begin_s ( id_id , auxinput6_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_begin_s
  INTEGER id_id
  model_config_rec%auxinput6_begin_s(id_id) = auxinput6_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput6_begin_s
SUBROUTINE nl_set_auxinput6_begin ( id_id , auxinput6_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_begin
  INTEGER id_id
  model_config_rec%auxinput6_begin(id_id) = auxinput6_begin
  RETURN
END SUBROUTINE nl_set_auxinput6_begin
SUBROUTINE nl_set_auxinput6_end_y ( id_id , auxinput6_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_end_y
  INTEGER id_id
  model_config_rec%auxinput6_end_y(id_id) = auxinput6_end_y
  RETURN
END SUBROUTINE nl_set_auxinput6_end_y
SUBROUTINE nl_set_auxinput6_end_d ( id_id , auxinput6_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_end_d
  INTEGER id_id
  model_config_rec%auxinput6_end_d(id_id) = auxinput6_end_d
  RETURN
END SUBROUTINE nl_set_auxinput6_end_d
SUBROUTINE nl_set_auxinput6_end_h ( id_id , auxinput6_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_end_h
  INTEGER id_id
  model_config_rec%auxinput6_end_h(id_id) = auxinput6_end_h
  RETURN
END SUBROUTINE nl_set_auxinput6_end_h
SUBROUTINE nl_set_auxinput6_end_m ( id_id , auxinput6_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_end_m
  INTEGER id_id
  model_config_rec%auxinput6_end_m(id_id) = auxinput6_end_m
  RETURN
END SUBROUTINE nl_set_auxinput6_end_m
SUBROUTINE nl_set_auxinput6_end_s ( id_id , auxinput6_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_end_s
  INTEGER id_id
  model_config_rec%auxinput6_end_s(id_id) = auxinput6_end_s
  RETURN
END SUBROUTINE nl_set_auxinput6_end_s
SUBROUTINE nl_set_auxinput6_end ( id_id , auxinput6_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput6_end
  INTEGER id_id
  model_config_rec%auxinput6_end(id_id) = auxinput6_end
  RETURN
END SUBROUTINE nl_set_auxinput6_end
SUBROUTINE nl_set_io_form_auxinput6 ( id_id , io_form_auxinput6 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput6
  INTEGER id_id
  model_config_rec%io_form_auxinput6 = io_form_auxinput6 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput6
SUBROUTINE nl_set_frames_per_auxinput6 ( id_id , frames_per_auxinput6 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput6
  INTEGER id_id
  model_config_rec%frames_per_auxinput6(id_id) = frames_per_auxinput6
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput6
SUBROUTINE nl_set_auxinput7_inname ( id_id , auxinput7_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput7_inname
  INTEGER id_id
  model_config_rec%auxinput7_inname = trim(auxinput7_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput7_inname
SUBROUTINE nl_set_auxinput7_outname ( id_id , auxinput7_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput7_outname
  INTEGER id_id
  model_config_rec%auxinput7_outname = trim(auxinput7_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput7_outname
SUBROUTINE nl_set_auxinput7_interval_y ( id_id , auxinput7_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_interval_y
  INTEGER id_id
  model_config_rec%auxinput7_interval_y(id_id) = auxinput7_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput7_interval_y
SUBROUTINE nl_set_auxinput7_interval_d ( id_id , auxinput7_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_interval_d
  INTEGER id_id
  model_config_rec%auxinput7_interval_d(id_id) = auxinput7_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput7_interval_d
SUBROUTINE nl_set_auxinput7_interval_h ( id_id , auxinput7_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_interval_h
  INTEGER id_id
  model_config_rec%auxinput7_interval_h(id_id) = auxinput7_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput7_interval_h
SUBROUTINE nl_set_auxinput7_interval_m ( id_id , auxinput7_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_interval_m
  INTEGER id_id
  model_config_rec%auxinput7_interval_m(id_id) = auxinput7_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput7_interval_m
SUBROUTINE nl_set_auxinput7_interval_s ( id_id , auxinput7_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_interval_s
  INTEGER id_id
  model_config_rec%auxinput7_interval_s(id_id) = auxinput7_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput7_interval_s
SUBROUTINE nl_set_auxinput7_interval ( id_id , auxinput7_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_interval
  INTEGER id_id
  model_config_rec%auxinput7_interval(id_id) = auxinput7_interval
  RETURN
END SUBROUTINE nl_set_auxinput7_interval
SUBROUTINE nl_set_auxinput7_begin_y ( id_id , auxinput7_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_begin_y
  INTEGER id_id
  model_config_rec%auxinput7_begin_y(id_id) = auxinput7_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput7_begin_y
SUBROUTINE nl_set_auxinput7_begin_d ( id_id , auxinput7_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_begin_d
  INTEGER id_id
  model_config_rec%auxinput7_begin_d(id_id) = auxinput7_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput7_begin_d
SUBROUTINE nl_set_auxinput7_begin_h ( id_id , auxinput7_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_begin_h
  INTEGER id_id
  model_config_rec%auxinput7_begin_h(id_id) = auxinput7_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput7_begin_h
SUBROUTINE nl_set_auxinput7_begin_m ( id_id , auxinput7_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_begin_m
  INTEGER id_id
  model_config_rec%auxinput7_begin_m(id_id) = auxinput7_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput7_begin_m
SUBROUTINE nl_set_auxinput7_begin_s ( id_id , auxinput7_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_begin_s
  INTEGER id_id
  model_config_rec%auxinput7_begin_s(id_id) = auxinput7_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput7_begin_s
SUBROUTINE nl_set_auxinput7_begin ( id_id , auxinput7_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_begin
  INTEGER id_id
  model_config_rec%auxinput7_begin(id_id) = auxinput7_begin
  RETURN
END SUBROUTINE nl_set_auxinput7_begin
SUBROUTINE nl_set_auxinput7_end_y ( id_id , auxinput7_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_end_y
  INTEGER id_id
  model_config_rec%auxinput7_end_y(id_id) = auxinput7_end_y
  RETURN
END SUBROUTINE nl_set_auxinput7_end_y
SUBROUTINE nl_set_auxinput7_end_d ( id_id , auxinput7_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_end_d
  INTEGER id_id
  model_config_rec%auxinput7_end_d(id_id) = auxinput7_end_d
  RETURN
END SUBROUTINE nl_set_auxinput7_end_d
SUBROUTINE nl_set_auxinput7_end_h ( id_id , auxinput7_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_end_h
  INTEGER id_id
  model_config_rec%auxinput7_end_h(id_id) = auxinput7_end_h
  RETURN
END SUBROUTINE nl_set_auxinput7_end_h
SUBROUTINE nl_set_auxinput7_end_m ( id_id , auxinput7_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_end_m
  INTEGER id_id
  model_config_rec%auxinput7_end_m(id_id) = auxinput7_end_m
  RETURN
END SUBROUTINE nl_set_auxinput7_end_m
SUBROUTINE nl_set_auxinput7_end_s ( id_id , auxinput7_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_end_s
  INTEGER id_id
  model_config_rec%auxinput7_end_s(id_id) = auxinput7_end_s
  RETURN
END SUBROUTINE nl_set_auxinput7_end_s
SUBROUTINE nl_set_auxinput7_end ( id_id , auxinput7_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput7_end
  INTEGER id_id
  model_config_rec%auxinput7_end(id_id) = auxinput7_end
  RETURN
END SUBROUTINE nl_set_auxinput7_end
#endif
#if (NNN == 4)
SUBROUTINE nl_set_io_form_auxinput7 ( id_id , io_form_auxinput7 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput7
  INTEGER id_id
  model_config_rec%io_form_auxinput7 = io_form_auxinput7 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput7
SUBROUTINE nl_set_frames_per_auxinput7 ( id_id , frames_per_auxinput7 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput7
  INTEGER id_id
  model_config_rec%frames_per_auxinput7(id_id) = frames_per_auxinput7
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput7
SUBROUTINE nl_set_auxinput8_inname ( id_id , auxinput8_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput8_inname
  INTEGER id_id
  model_config_rec%auxinput8_inname = trim(auxinput8_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput8_inname
SUBROUTINE nl_set_auxinput8_outname ( id_id , auxinput8_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput8_outname
  INTEGER id_id
  model_config_rec%auxinput8_outname = trim(auxinput8_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput8_outname
SUBROUTINE nl_set_auxinput8_interval_y ( id_id , auxinput8_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_interval_y
  INTEGER id_id
  model_config_rec%auxinput8_interval_y(id_id) = auxinput8_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput8_interval_y
SUBROUTINE nl_set_auxinput8_interval_d ( id_id , auxinput8_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_interval_d
  INTEGER id_id
  model_config_rec%auxinput8_interval_d(id_id) = auxinput8_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput8_interval_d
SUBROUTINE nl_set_auxinput8_interval_h ( id_id , auxinput8_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_interval_h
  INTEGER id_id
  model_config_rec%auxinput8_interval_h(id_id) = auxinput8_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput8_interval_h
SUBROUTINE nl_set_auxinput8_interval_m ( id_id , auxinput8_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_interval_m
  INTEGER id_id
  model_config_rec%auxinput8_interval_m(id_id) = auxinput8_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput8_interval_m
SUBROUTINE nl_set_auxinput8_interval_s ( id_id , auxinput8_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_interval_s
  INTEGER id_id
  model_config_rec%auxinput8_interval_s(id_id) = auxinput8_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput8_interval_s
SUBROUTINE nl_set_auxinput8_interval ( id_id , auxinput8_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_interval
  INTEGER id_id
  model_config_rec%auxinput8_interval(id_id) = auxinput8_interval
  RETURN
END SUBROUTINE nl_set_auxinput8_interval
SUBROUTINE nl_set_auxinput8_begin_y ( id_id , auxinput8_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_begin_y
  INTEGER id_id
  model_config_rec%auxinput8_begin_y(id_id) = auxinput8_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput8_begin_y
SUBROUTINE nl_set_auxinput8_begin_d ( id_id , auxinput8_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_begin_d
  INTEGER id_id
  model_config_rec%auxinput8_begin_d(id_id) = auxinput8_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput8_begin_d
SUBROUTINE nl_set_auxinput8_begin_h ( id_id , auxinput8_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_begin_h
  INTEGER id_id
  model_config_rec%auxinput8_begin_h(id_id) = auxinput8_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput8_begin_h
SUBROUTINE nl_set_auxinput8_begin_m ( id_id , auxinput8_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_begin_m
  INTEGER id_id
  model_config_rec%auxinput8_begin_m(id_id) = auxinput8_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput8_begin_m
SUBROUTINE nl_set_auxinput8_begin_s ( id_id , auxinput8_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_begin_s
  INTEGER id_id
  model_config_rec%auxinput8_begin_s(id_id) = auxinput8_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput8_begin_s
SUBROUTINE nl_set_auxinput8_begin ( id_id , auxinput8_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_begin
  INTEGER id_id
  model_config_rec%auxinput8_begin(id_id) = auxinput8_begin
  RETURN
END SUBROUTINE nl_set_auxinput8_begin
SUBROUTINE nl_set_auxinput8_end_y ( id_id , auxinput8_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_end_y
  INTEGER id_id
  model_config_rec%auxinput8_end_y(id_id) = auxinput8_end_y
  RETURN
END SUBROUTINE nl_set_auxinput8_end_y
SUBROUTINE nl_set_auxinput8_end_d ( id_id , auxinput8_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_end_d
  INTEGER id_id
  model_config_rec%auxinput8_end_d(id_id) = auxinput8_end_d
  RETURN
END SUBROUTINE nl_set_auxinput8_end_d
SUBROUTINE nl_set_auxinput8_end_h ( id_id , auxinput8_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_end_h
  INTEGER id_id
  model_config_rec%auxinput8_end_h(id_id) = auxinput8_end_h
  RETURN
END SUBROUTINE nl_set_auxinput8_end_h
SUBROUTINE nl_set_auxinput8_end_m ( id_id , auxinput8_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_end_m
  INTEGER id_id
  model_config_rec%auxinput8_end_m(id_id) = auxinput8_end_m
  RETURN
END SUBROUTINE nl_set_auxinput8_end_m
SUBROUTINE nl_set_auxinput8_end_s ( id_id , auxinput8_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_end_s
  INTEGER id_id
  model_config_rec%auxinput8_end_s(id_id) = auxinput8_end_s
  RETURN
END SUBROUTINE nl_set_auxinput8_end_s
SUBROUTINE nl_set_auxinput8_end ( id_id , auxinput8_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput8_end
  INTEGER id_id
  model_config_rec%auxinput8_end(id_id) = auxinput8_end
  RETURN
END SUBROUTINE nl_set_auxinput8_end
SUBROUTINE nl_set_io_form_auxinput8 ( id_id , io_form_auxinput8 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput8
  INTEGER id_id
  model_config_rec%io_form_auxinput8 = io_form_auxinput8 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput8
SUBROUTINE nl_set_frames_per_auxinput8 ( id_id , frames_per_auxinput8 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput8
  INTEGER id_id
  model_config_rec%frames_per_auxinput8(id_id) = frames_per_auxinput8
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput8
SUBROUTINE nl_set_auxinput9_inname ( id_id , auxinput9_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput9_inname
  INTEGER id_id
  model_config_rec%auxinput9_inname = trim(auxinput9_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput9_inname
SUBROUTINE nl_set_auxinput9_outname ( id_id , auxinput9_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput9_outname
  INTEGER id_id
  model_config_rec%auxinput9_outname = trim(auxinput9_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput9_outname
SUBROUTINE nl_set_auxinput9_interval_y ( id_id , auxinput9_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput9_interval_y
  INTEGER id_id
  model_config_rec%auxinput9_interval_y(id_id) = auxinput9_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput9_interval_y
SUBROUTINE nl_set_auxinput9_interval_d ( id_id , auxinput9_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput9_interval_d
  INTEGER id_id
  model_config_rec%auxinput9_interval_d(id_id) = auxinput9_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput9_interval_d
SUBROUTINE nl_set_auxinput9_interval_h ( id_id , auxinput9_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput9_interval_h
  INTEGER id_id
  model_config_rec%auxinput9_interval_h(id_id) = auxinput9_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput9_interval_h
SUBROUTINE nl_set_auxinput9_interval_m ( id_id , auxinput9_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput9_interval_m
  INTEGER id_id
  model_config_rec%auxinput9_interval_m(id_id) = auxinput9_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput9_interval_m
SUBROUTINE nl_set_auxinput9_interval_s ( id_id , auxinput9_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput9_interval_s
  INTEGER id_id
  model_config_rec%auxinput9_interval_s(id_id) = auxinput9_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput9_interval_s
SUBROUTINE nl_set_auxinput9_interval ( id_id , auxinput9_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput9_interval
  INTEGER id_id
  model_config_rec%auxinput9_interval(id_id) = auxinput9_interval
  RETURN
END SUBROUTINE nl_set_auxinput9_interval
SUBROUTINE nl_set_auxinput9_begin_y ( id_id , auxinput9_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput9_begin_y
  INTEGER id_id
  model_config_rec%auxinput9_begin_y(id_id) = auxinput9_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput9_begin_y
SUBROUTINE nl_set_auxinput9_begin_d ( id_id , auxinput9_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput9_begin_d
  INTEGER id_id
  model_config_rec%auxinput9_begin_d(id_id) = auxinput9_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput9_begin_d
SUBROUTINE nl_set_auxinput9_begin_h ( id_id , auxinput9_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput9_begin_h
  INTEGER id_id
  model_config_rec%auxinput9_begin_h(id_id) = auxinput9_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput9_begin_h
SUBROUTINE nl_set_auxinput9_begin_m ( id_id , auxinput9_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput9_begin_m
  INTEGER id_id
  model_config_rec%auxinput9_begin_m(id_id) = auxinput9_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput9_begin_m
SUBROUTINE nl_set_auxinput9_begin_s ( id_id , auxinput9_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput9_begin_s
  INTEGER id_id
  model_config_rec%auxinput9_begin_s(id_id) = auxinput9_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput9_begin_s
SUBROUTINE nl_set_auxinput9_begin ( id_id , auxinput9_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput9_begin
  INTEGER id_id
  model_config_rec%auxinput9_begin(id_id) = auxinput9_begin
  RETURN
END SUBROUTINE nl_set_auxinput9_begin
SUBROUTINE nl_set_auxinput9_end_y ( id_id , auxinput9_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput9_end_y
  INTEGER id_id
  model_config_rec%auxinput9_end_y(id_id) = auxinput9_end_y
  RETURN
END SUBROUTINE nl_set_auxinput9_end_y
SUBROUTINE nl_set_auxinput9_end_d ( id_id , auxinput9_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput9_end_d
  INTEGER id_id
  model_config_rec%auxinput9_end_d(id_id) = auxinput9_end_d
  RETURN
END SUBROUTINE nl_set_auxinput9_end_d
SUBROUTINE nl_set_auxinput9_end_h ( id_id , auxinput9_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput9_end_h
  INTEGER id_id
  model_config_rec%auxinput9_end_h(id_id) = auxinput9_end_h
  RETURN
END SUBROUTINE nl_set_auxinput9_end_h
SUBROUTINE nl_set_auxinput9_end_m ( id_id , auxinput9_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput9_end_m
  INTEGER id_id
  model_config_rec%auxinput9_end_m(id_id) = auxinput9_end_m
  RETURN
END SUBROUTINE nl_set_auxinput9_end_m
SUBROUTINE nl_set_auxinput9_end_s ( id_id , auxinput9_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput9_end_s
  INTEGER id_id
  model_config_rec%auxinput9_end_s(id_id) = auxinput9_end_s
  RETURN
END SUBROUTINE nl_set_auxinput9_end_s
SUBROUTINE nl_set_auxinput9_end ( id_id , auxinput9_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput9_end
  INTEGER id_id
  model_config_rec%auxinput9_end(id_id) = auxinput9_end
  RETURN
END SUBROUTINE nl_set_auxinput9_end
SUBROUTINE nl_set_io_form_auxinput9 ( id_id , io_form_auxinput9 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput9
  INTEGER id_id
  model_config_rec%io_form_auxinput9 = io_form_auxinput9 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput9
SUBROUTINE nl_set_frames_per_auxinput9 ( id_id , frames_per_auxinput9 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput9
  INTEGER id_id
  model_config_rec%frames_per_auxinput9(id_id) = frames_per_auxinput9
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput9
SUBROUTINE nl_set_auxinput10_inname ( id_id , auxinput10_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput10_inname
  INTEGER id_id
  model_config_rec%auxinput10_inname = trim(auxinput10_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput10_inname
SUBROUTINE nl_set_auxinput10_outname ( id_id , auxinput10_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput10_outname
  INTEGER id_id
  model_config_rec%auxinput10_outname = trim(auxinput10_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput10_outname
SUBROUTINE nl_set_auxinput10_interval_y ( id_id , auxinput10_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput10_interval_y
  INTEGER id_id
  model_config_rec%auxinput10_interval_y(id_id) = auxinput10_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput10_interval_y
SUBROUTINE nl_set_auxinput10_interval_d ( id_id , auxinput10_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput10_interval_d
  INTEGER id_id
  model_config_rec%auxinput10_interval_d(id_id) = auxinput10_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput10_interval_d
SUBROUTINE nl_set_auxinput10_interval_h ( id_id , auxinput10_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput10_interval_h
  INTEGER id_id
  model_config_rec%auxinput10_interval_h(id_id) = auxinput10_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput10_interval_h
SUBROUTINE nl_set_auxinput10_interval_m ( id_id , auxinput10_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput10_interval_m
  INTEGER id_id
  model_config_rec%auxinput10_interval_m(id_id) = auxinput10_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput10_interval_m
SUBROUTINE nl_set_auxinput10_interval_s ( id_id , auxinput10_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput10_interval_s
  INTEGER id_id
  model_config_rec%auxinput10_interval_s(id_id) = auxinput10_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput10_interval_s
SUBROUTINE nl_set_auxinput10_interval ( id_id , auxinput10_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput10_interval
  INTEGER id_id
  model_config_rec%auxinput10_interval(id_id) = auxinput10_interval
  RETURN
END SUBROUTINE nl_set_auxinput10_interval
SUBROUTINE nl_set_auxinput10_begin_y ( id_id , auxinput10_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput10_begin_y
  INTEGER id_id
  model_config_rec%auxinput10_begin_y(id_id) = auxinput10_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput10_begin_y
SUBROUTINE nl_set_auxinput10_begin_d ( id_id , auxinput10_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput10_begin_d
  INTEGER id_id
  model_config_rec%auxinput10_begin_d(id_id) = auxinput10_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput10_begin_d
SUBROUTINE nl_set_auxinput10_begin_h ( id_id , auxinput10_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput10_begin_h
  INTEGER id_id
  model_config_rec%auxinput10_begin_h(id_id) = auxinput10_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput10_begin_h
SUBROUTINE nl_set_auxinput10_begin_m ( id_id , auxinput10_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput10_begin_m
  INTEGER id_id
  model_config_rec%auxinput10_begin_m(id_id) = auxinput10_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput10_begin_m
SUBROUTINE nl_set_auxinput10_begin_s ( id_id , auxinput10_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput10_begin_s
  INTEGER id_id
  model_config_rec%auxinput10_begin_s(id_id) = auxinput10_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput10_begin_s
SUBROUTINE nl_set_auxinput10_begin ( id_id , auxinput10_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput10_begin
  INTEGER id_id
  model_config_rec%auxinput10_begin(id_id) = auxinput10_begin
  RETURN
END SUBROUTINE nl_set_auxinput10_begin
SUBROUTINE nl_set_auxinput10_end_y ( id_id , auxinput10_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput10_end_y
  INTEGER id_id
  model_config_rec%auxinput10_end_y(id_id) = auxinput10_end_y
  RETURN
END SUBROUTINE nl_set_auxinput10_end_y
SUBROUTINE nl_set_auxinput10_end_d ( id_id , auxinput10_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput10_end_d
  INTEGER id_id
  model_config_rec%auxinput10_end_d(id_id) = auxinput10_end_d
  RETURN
END SUBROUTINE nl_set_auxinput10_end_d
SUBROUTINE nl_set_auxinput10_end_h ( id_id , auxinput10_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput10_end_h
  INTEGER id_id
  model_config_rec%auxinput10_end_h(id_id) = auxinput10_end_h
  RETURN
END SUBROUTINE nl_set_auxinput10_end_h
SUBROUTINE nl_set_auxinput10_end_m ( id_id , auxinput10_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput10_end_m
  INTEGER id_id
  model_config_rec%auxinput10_end_m(id_id) = auxinput10_end_m
  RETURN
END SUBROUTINE nl_set_auxinput10_end_m
SUBROUTINE nl_set_auxinput10_end_s ( id_id , auxinput10_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput10_end_s
  INTEGER id_id
  model_config_rec%auxinput10_end_s(id_id) = auxinput10_end_s
  RETURN
END SUBROUTINE nl_set_auxinput10_end_s
SUBROUTINE nl_set_auxinput10_end ( id_id , auxinput10_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput10_end
  INTEGER id_id
  model_config_rec%auxinput10_end(id_id) = auxinput10_end
  RETURN
END SUBROUTINE nl_set_auxinput10_end
SUBROUTINE nl_set_io_form_auxinput10 ( id_id , io_form_auxinput10 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput10
  INTEGER id_id
  model_config_rec%io_form_auxinput10 = io_form_auxinput10 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput10
SUBROUTINE nl_set_frames_per_auxinput10 ( id_id , frames_per_auxinput10 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput10
  INTEGER id_id
  model_config_rec%frames_per_auxinput10(id_id) = frames_per_auxinput10
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput10
SUBROUTINE nl_set_auxinput11_inname ( id_id , auxinput11_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput11_inname
  INTEGER id_id
  model_config_rec%auxinput11_inname = trim(auxinput11_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput11_inname
SUBROUTINE nl_set_auxinput11_outname ( id_id , auxinput11_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput11_outname
  INTEGER id_id
  model_config_rec%auxinput11_outname = trim(auxinput11_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput11_outname
SUBROUTINE nl_set_auxinput11_interval_y ( id_id , auxinput11_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_interval_y
  INTEGER id_id
  model_config_rec%auxinput11_interval_y(id_id) = auxinput11_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput11_interval_y
SUBROUTINE nl_set_auxinput11_interval_d ( id_id , auxinput11_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_interval_d
  INTEGER id_id
  model_config_rec%auxinput11_interval_d(id_id) = auxinput11_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput11_interval_d
SUBROUTINE nl_set_auxinput11_interval_h ( id_id , auxinput11_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_interval_h
  INTEGER id_id
  model_config_rec%auxinput11_interval_h(id_id) = auxinput11_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput11_interval_h
SUBROUTINE nl_set_auxinput11_interval_m ( id_id , auxinput11_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_interval_m
  INTEGER id_id
  model_config_rec%auxinput11_interval_m(id_id) = auxinput11_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput11_interval_m
SUBROUTINE nl_set_auxinput11_interval_s ( id_id , auxinput11_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_interval_s
  INTEGER id_id
  model_config_rec%auxinput11_interval_s(id_id) = auxinput11_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput11_interval_s
SUBROUTINE nl_set_auxinput11_interval ( id_id , auxinput11_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_interval
  INTEGER id_id
  model_config_rec%auxinput11_interval(id_id) = auxinput11_interval
  RETURN
END SUBROUTINE nl_set_auxinput11_interval
SUBROUTINE nl_set_auxinput11_begin_y ( id_id , auxinput11_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_begin_y
  INTEGER id_id
  model_config_rec%auxinput11_begin_y(id_id) = auxinput11_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput11_begin_y
SUBROUTINE nl_set_auxinput11_begin_d ( id_id , auxinput11_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_begin_d
  INTEGER id_id
  model_config_rec%auxinput11_begin_d(id_id) = auxinput11_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput11_begin_d
SUBROUTINE nl_set_auxinput11_begin_h ( id_id , auxinput11_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_begin_h
  INTEGER id_id
  model_config_rec%auxinput11_begin_h(id_id) = auxinput11_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput11_begin_h
SUBROUTINE nl_set_auxinput11_begin_m ( id_id , auxinput11_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_begin_m
  INTEGER id_id
  model_config_rec%auxinput11_begin_m(id_id) = auxinput11_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput11_begin_m
SUBROUTINE nl_set_auxinput11_begin_s ( id_id , auxinput11_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_begin_s
  INTEGER id_id
  model_config_rec%auxinput11_begin_s(id_id) = auxinput11_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput11_begin_s
SUBROUTINE nl_set_auxinput11_begin ( id_id , auxinput11_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_begin
  INTEGER id_id
  model_config_rec%auxinput11_begin(id_id) = auxinput11_begin
  RETURN
END SUBROUTINE nl_set_auxinput11_begin
SUBROUTINE nl_set_auxinput11_end_y ( id_id , auxinput11_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_end_y
  INTEGER id_id
  model_config_rec%auxinput11_end_y(id_id) = auxinput11_end_y
  RETURN
END SUBROUTINE nl_set_auxinput11_end_y
SUBROUTINE nl_set_auxinput11_end_d ( id_id , auxinput11_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_end_d
  INTEGER id_id
  model_config_rec%auxinput11_end_d(id_id) = auxinput11_end_d
  RETURN
END SUBROUTINE nl_set_auxinput11_end_d
SUBROUTINE nl_set_auxinput11_end_h ( id_id , auxinput11_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_end_h
  INTEGER id_id
  model_config_rec%auxinput11_end_h(id_id) = auxinput11_end_h
  RETURN
END SUBROUTINE nl_set_auxinput11_end_h
SUBROUTINE nl_set_auxinput11_end_m ( id_id , auxinput11_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_end_m
  INTEGER id_id
  model_config_rec%auxinput11_end_m(id_id) = auxinput11_end_m
  RETURN
END SUBROUTINE nl_set_auxinput11_end_m
SUBROUTINE nl_set_auxinput11_end_s ( id_id , auxinput11_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_end_s
  INTEGER id_id
  model_config_rec%auxinput11_end_s(id_id) = auxinput11_end_s
  RETURN
END SUBROUTINE nl_set_auxinput11_end_s
SUBROUTINE nl_set_auxinput11_end ( id_id , auxinput11_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput11_end
  INTEGER id_id
  model_config_rec%auxinput11_end(id_id) = auxinput11_end
  RETURN
END SUBROUTINE nl_set_auxinput11_end
SUBROUTINE nl_set_io_form_auxinput11 ( id_id , io_form_auxinput11 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput11
  INTEGER id_id
  model_config_rec%io_form_auxinput11 = io_form_auxinput11 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput11
SUBROUTINE nl_set_frames_per_auxinput11 ( id_id , frames_per_auxinput11 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput11
  INTEGER id_id
  model_config_rec%frames_per_auxinput11(id_id) = frames_per_auxinput11
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput11
SUBROUTINE nl_set_auxinput12_inname ( id_id , auxinput12_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput12_inname
  INTEGER id_id
  model_config_rec%auxinput12_inname = trim(auxinput12_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput12_inname
SUBROUTINE nl_set_auxinput12_outname ( id_id , auxinput12_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput12_outname
  INTEGER id_id
  model_config_rec%auxinput12_outname = trim(auxinput12_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput12_outname
SUBROUTINE nl_set_auxinput12_interval_y ( id_id , auxinput12_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput12_interval_y
  INTEGER id_id
  model_config_rec%auxinput12_interval_y(id_id) = auxinput12_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput12_interval_y
SUBROUTINE nl_set_auxinput12_interval_d ( id_id , auxinput12_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput12_interval_d
  INTEGER id_id
  model_config_rec%auxinput12_interval_d(id_id) = auxinput12_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput12_interval_d
SUBROUTINE nl_set_auxinput12_interval_h ( id_id , auxinput12_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput12_interval_h
  INTEGER id_id
  model_config_rec%auxinput12_interval_h(id_id) = auxinput12_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput12_interval_h
SUBROUTINE nl_set_auxinput12_interval_m ( id_id , auxinput12_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput12_interval_m
  INTEGER id_id
  model_config_rec%auxinput12_interval_m(id_id) = auxinput12_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput12_interval_m
SUBROUTINE nl_set_auxinput12_interval_s ( id_id , auxinput12_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput12_interval_s
  INTEGER id_id
  model_config_rec%auxinput12_interval_s(id_id) = auxinput12_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput12_interval_s
SUBROUTINE nl_set_auxinput12_interval ( id_id , auxinput12_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput12_interval
  INTEGER id_id
  model_config_rec%auxinput12_interval(id_id) = auxinput12_interval
  RETURN
END SUBROUTINE nl_set_auxinput12_interval
SUBROUTINE nl_set_auxinput12_begin_y ( id_id , auxinput12_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput12_begin_y
  INTEGER id_id
  model_config_rec%auxinput12_begin_y(id_id) = auxinput12_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput12_begin_y
SUBROUTINE nl_set_auxinput12_begin_d ( id_id , auxinput12_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput12_begin_d
  INTEGER id_id
  model_config_rec%auxinput12_begin_d(id_id) = auxinput12_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput12_begin_d
SUBROUTINE nl_set_auxinput12_begin_h ( id_id , auxinput12_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput12_begin_h
  INTEGER id_id
  model_config_rec%auxinput12_begin_h(id_id) = auxinput12_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput12_begin_h
SUBROUTINE nl_set_auxinput12_begin_m ( id_id , auxinput12_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput12_begin_m
  INTEGER id_id
  model_config_rec%auxinput12_begin_m(id_id) = auxinput12_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput12_begin_m
SUBROUTINE nl_set_auxinput12_begin_s ( id_id , auxinput12_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput12_begin_s
  INTEGER id_id
  model_config_rec%auxinput12_begin_s(id_id) = auxinput12_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput12_begin_s
SUBROUTINE nl_set_auxinput12_begin ( id_id , auxinput12_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput12_begin
  INTEGER id_id
  model_config_rec%auxinput12_begin(id_id) = auxinput12_begin
  RETURN
END SUBROUTINE nl_set_auxinput12_begin
SUBROUTINE nl_set_auxinput12_end_y ( id_id , auxinput12_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput12_end_y
  INTEGER id_id
  model_config_rec%auxinput12_end_y(id_id) = auxinput12_end_y
  RETURN
END SUBROUTINE nl_set_auxinput12_end_y
SUBROUTINE nl_set_auxinput12_end_d ( id_id , auxinput12_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput12_end_d
  INTEGER id_id
  model_config_rec%auxinput12_end_d(id_id) = auxinput12_end_d
  RETURN
END SUBROUTINE nl_set_auxinput12_end_d
SUBROUTINE nl_set_auxinput12_end_h ( id_id , auxinput12_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput12_end_h
  INTEGER id_id
  model_config_rec%auxinput12_end_h(id_id) = auxinput12_end_h
  RETURN
END SUBROUTINE nl_set_auxinput12_end_h
SUBROUTINE nl_set_auxinput12_end_m ( id_id , auxinput12_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput12_end_m
  INTEGER id_id
  model_config_rec%auxinput12_end_m(id_id) = auxinput12_end_m
  RETURN
END SUBROUTINE nl_set_auxinput12_end_m
SUBROUTINE nl_set_auxinput12_end_s ( id_id , auxinput12_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput12_end_s
  INTEGER id_id
  model_config_rec%auxinput12_end_s(id_id) = auxinput12_end_s
  RETURN
END SUBROUTINE nl_set_auxinput12_end_s
SUBROUTINE nl_set_auxinput12_end ( id_id , auxinput12_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput12_end
  INTEGER id_id
  model_config_rec%auxinput12_end(id_id) = auxinput12_end
  RETURN
END SUBROUTINE nl_set_auxinput12_end
SUBROUTINE nl_set_io_form_auxinput12 ( id_id , io_form_auxinput12 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput12
  INTEGER id_id
  model_config_rec%io_form_auxinput12 = io_form_auxinput12 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput12
SUBROUTINE nl_set_frames_per_auxinput12 ( id_id , frames_per_auxinput12 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput12
  INTEGER id_id
  model_config_rec%frames_per_auxinput12(id_id) = frames_per_auxinput12
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput12
SUBROUTINE nl_set_auxinput13_inname ( id_id , auxinput13_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput13_inname
  INTEGER id_id
  model_config_rec%auxinput13_inname = trim(auxinput13_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput13_inname
SUBROUTINE nl_set_auxinput13_outname ( id_id , auxinput13_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput13_outname
  INTEGER id_id
  model_config_rec%auxinput13_outname = trim(auxinput13_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput13_outname
SUBROUTINE nl_set_auxinput13_interval_y ( id_id , auxinput13_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput13_interval_y
  INTEGER id_id
  model_config_rec%auxinput13_interval_y(id_id) = auxinput13_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput13_interval_y
SUBROUTINE nl_set_auxinput13_interval_d ( id_id , auxinput13_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput13_interval_d
  INTEGER id_id
  model_config_rec%auxinput13_interval_d(id_id) = auxinput13_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput13_interval_d
SUBROUTINE nl_set_auxinput13_interval_h ( id_id , auxinput13_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput13_interval_h
  INTEGER id_id
  model_config_rec%auxinput13_interval_h(id_id) = auxinput13_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput13_interval_h
SUBROUTINE nl_set_auxinput13_interval_m ( id_id , auxinput13_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput13_interval_m
  INTEGER id_id
  model_config_rec%auxinput13_interval_m(id_id) = auxinput13_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput13_interval_m
SUBROUTINE nl_set_auxinput13_interval_s ( id_id , auxinput13_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput13_interval_s
  INTEGER id_id
  model_config_rec%auxinput13_interval_s(id_id) = auxinput13_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput13_interval_s
SUBROUTINE nl_set_auxinput13_interval ( id_id , auxinput13_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput13_interval
  INTEGER id_id
  model_config_rec%auxinput13_interval(id_id) = auxinput13_interval
  RETURN
END SUBROUTINE nl_set_auxinput13_interval
SUBROUTINE nl_set_auxinput13_begin_y ( id_id , auxinput13_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput13_begin_y
  INTEGER id_id
  model_config_rec%auxinput13_begin_y(id_id) = auxinput13_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput13_begin_y
SUBROUTINE nl_set_auxinput13_begin_d ( id_id , auxinput13_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput13_begin_d
  INTEGER id_id
  model_config_rec%auxinput13_begin_d(id_id) = auxinput13_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput13_begin_d
SUBROUTINE nl_set_auxinput13_begin_h ( id_id , auxinput13_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput13_begin_h
  INTEGER id_id
  model_config_rec%auxinput13_begin_h(id_id) = auxinput13_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput13_begin_h
SUBROUTINE nl_set_auxinput13_begin_m ( id_id , auxinput13_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput13_begin_m
  INTEGER id_id
  model_config_rec%auxinput13_begin_m(id_id) = auxinput13_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput13_begin_m
SUBROUTINE nl_set_auxinput13_begin_s ( id_id , auxinput13_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput13_begin_s
  INTEGER id_id
  model_config_rec%auxinput13_begin_s(id_id) = auxinput13_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput13_begin_s
SUBROUTINE nl_set_auxinput13_begin ( id_id , auxinput13_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput13_begin
  INTEGER id_id
  model_config_rec%auxinput13_begin(id_id) = auxinput13_begin
  RETURN
END SUBROUTINE nl_set_auxinput13_begin
SUBROUTINE nl_set_auxinput13_end_y ( id_id , auxinput13_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput13_end_y
  INTEGER id_id
  model_config_rec%auxinput13_end_y(id_id) = auxinput13_end_y
  RETURN
END SUBROUTINE nl_set_auxinput13_end_y
SUBROUTINE nl_set_auxinput13_end_d ( id_id , auxinput13_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput13_end_d
  INTEGER id_id
  model_config_rec%auxinput13_end_d(id_id) = auxinput13_end_d
  RETURN
END SUBROUTINE nl_set_auxinput13_end_d
SUBROUTINE nl_set_auxinput13_end_h ( id_id , auxinput13_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput13_end_h
  INTEGER id_id
  model_config_rec%auxinput13_end_h(id_id) = auxinput13_end_h
  RETURN
END SUBROUTINE nl_set_auxinput13_end_h
SUBROUTINE nl_set_auxinput13_end_m ( id_id , auxinput13_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput13_end_m
  INTEGER id_id
  model_config_rec%auxinput13_end_m(id_id) = auxinput13_end_m
  RETURN
END SUBROUTINE nl_set_auxinput13_end_m
SUBROUTINE nl_set_auxinput13_end_s ( id_id , auxinput13_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput13_end_s
  INTEGER id_id
  model_config_rec%auxinput13_end_s(id_id) = auxinput13_end_s
  RETURN
END SUBROUTINE nl_set_auxinput13_end_s
SUBROUTINE nl_set_auxinput13_end ( id_id , auxinput13_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput13_end
  INTEGER id_id
  model_config_rec%auxinput13_end(id_id) = auxinput13_end
  RETURN
END SUBROUTINE nl_set_auxinput13_end
SUBROUTINE nl_set_io_form_auxinput13 ( id_id , io_form_auxinput13 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput13
  INTEGER id_id
  model_config_rec%io_form_auxinput13 = io_form_auxinput13 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput13
SUBROUTINE nl_set_frames_per_auxinput13 ( id_id , frames_per_auxinput13 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput13
  INTEGER id_id
  model_config_rec%frames_per_auxinput13(id_id) = frames_per_auxinput13
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput13
SUBROUTINE nl_set_auxinput14_inname ( id_id , auxinput14_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput14_inname
  INTEGER id_id
  model_config_rec%auxinput14_inname = trim(auxinput14_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput14_inname
SUBROUTINE nl_set_auxinput14_outname ( id_id , auxinput14_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput14_outname
  INTEGER id_id
  model_config_rec%auxinput14_outname = trim(auxinput14_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput14_outname
SUBROUTINE nl_set_auxinput14_interval_y ( id_id , auxinput14_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput14_interval_y
  INTEGER id_id
  model_config_rec%auxinput14_interval_y(id_id) = auxinput14_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput14_interval_y
SUBROUTINE nl_set_auxinput14_interval_d ( id_id , auxinput14_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput14_interval_d
  INTEGER id_id
  model_config_rec%auxinput14_interval_d(id_id) = auxinput14_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput14_interval_d
SUBROUTINE nl_set_auxinput14_interval_h ( id_id , auxinput14_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput14_interval_h
  INTEGER id_id
  model_config_rec%auxinput14_interval_h(id_id) = auxinput14_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput14_interval_h
SUBROUTINE nl_set_auxinput14_interval_m ( id_id , auxinput14_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput14_interval_m
  INTEGER id_id
  model_config_rec%auxinput14_interval_m(id_id) = auxinput14_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput14_interval_m
SUBROUTINE nl_set_auxinput14_interval_s ( id_id , auxinput14_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput14_interval_s
  INTEGER id_id
  model_config_rec%auxinput14_interval_s(id_id) = auxinput14_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput14_interval_s
SUBROUTINE nl_set_auxinput14_interval ( id_id , auxinput14_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput14_interval
  INTEGER id_id
  model_config_rec%auxinput14_interval(id_id) = auxinput14_interval
  RETURN
END SUBROUTINE nl_set_auxinput14_interval
SUBROUTINE nl_set_auxinput14_begin_y ( id_id , auxinput14_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput14_begin_y
  INTEGER id_id
  model_config_rec%auxinput14_begin_y(id_id) = auxinput14_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput14_begin_y
SUBROUTINE nl_set_auxinput14_begin_d ( id_id , auxinput14_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput14_begin_d
  INTEGER id_id
  model_config_rec%auxinput14_begin_d(id_id) = auxinput14_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput14_begin_d
SUBROUTINE nl_set_auxinput14_begin_h ( id_id , auxinput14_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput14_begin_h
  INTEGER id_id
  model_config_rec%auxinput14_begin_h(id_id) = auxinput14_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput14_begin_h
SUBROUTINE nl_set_auxinput14_begin_m ( id_id , auxinput14_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput14_begin_m
  INTEGER id_id
  model_config_rec%auxinput14_begin_m(id_id) = auxinput14_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput14_begin_m
SUBROUTINE nl_set_auxinput14_begin_s ( id_id , auxinput14_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput14_begin_s
  INTEGER id_id
  model_config_rec%auxinput14_begin_s(id_id) = auxinput14_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput14_begin_s
SUBROUTINE nl_set_auxinput14_begin ( id_id , auxinput14_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput14_begin
  INTEGER id_id
  model_config_rec%auxinput14_begin(id_id) = auxinput14_begin
  RETURN
END SUBROUTINE nl_set_auxinput14_begin
SUBROUTINE nl_set_auxinput14_end_y ( id_id , auxinput14_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput14_end_y
  INTEGER id_id
  model_config_rec%auxinput14_end_y(id_id) = auxinput14_end_y
  RETURN
END SUBROUTINE nl_set_auxinput14_end_y
SUBROUTINE nl_set_auxinput14_end_d ( id_id , auxinput14_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput14_end_d
  INTEGER id_id
  model_config_rec%auxinput14_end_d(id_id) = auxinput14_end_d
  RETURN
END SUBROUTINE nl_set_auxinput14_end_d
SUBROUTINE nl_set_auxinput14_end_h ( id_id , auxinput14_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput14_end_h
  INTEGER id_id
  model_config_rec%auxinput14_end_h(id_id) = auxinput14_end_h
  RETURN
END SUBROUTINE nl_set_auxinput14_end_h
SUBROUTINE nl_set_auxinput14_end_m ( id_id , auxinput14_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput14_end_m
  INTEGER id_id
  model_config_rec%auxinput14_end_m(id_id) = auxinput14_end_m
  RETURN
END SUBROUTINE nl_set_auxinput14_end_m
SUBROUTINE nl_set_auxinput14_end_s ( id_id , auxinput14_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput14_end_s
  INTEGER id_id
  model_config_rec%auxinput14_end_s(id_id) = auxinput14_end_s
  RETURN
END SUBROUTINE nl_set_auxinput14_end_s
SUBROUTINE nl_set_auxinput14_end ( id_id , auxinput14_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput14_end
  INTEGER id_id
  model_config_rec%auxinput14_end(id_id) = auxinput14_end
  RETURN
END SUBROUTINE nl_set_auxinput14_end
SUBROUTINE nl_set_io_form_auxinput14 ( id_id , io_form_auxinput14 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput14
  INTEGER id_id
  model_config_rec%io_form_auxinput14 = io_form_auxinput14 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput14
SUBROUTINE nl_set_frames_per_auxinput14 ( id_id , frames_per_auxinput14 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput14
  INTEGER id_id
  model_config_rec%frames_per_auxinput14(id_id) = frames_per_auxinput14
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput14
SUBROUTINE nl_set_auxinput15_inname ( id_id , auxinput15_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput15_inname
  INTEGER id_id
  model_config_rec%auxinput15_inname = trim(auxinput15_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput15_inname
SUBROUTINE nl_set_auxinput15_outname ( id_id , auxinput15_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput15_outname
  INTEGER id_id
  model_config_rec%auxinput15_outname = trim(auxinput15_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput15_outname
SUBROUTINE nl_set_auxinput15_interval_y ( id_id , auxinput15_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput15_interval_y
  INTEGER id_id
  model_config_rec%auxinput15_interval_y(id_id) = auxinput15_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput15_interval_y
SUBROUTINE nl_set_auxinput15_interval_d ( id_id , auxinput15_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput15_interval_d
  INTEGER id_id
  model_config_rec%auxinput15_interval_d(id_id) = auxinput15_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput15_interval_d
SUBROUTINE nl_set_auxinput15_interval_h ( id_id , auxinput15_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput15_interval_h
  INTEGER id_id
  model_config_rec%auxinput15_interval_h(id_id) = auxinput15_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput15_interval_h
SUBROUTINE nl_set_auxinput15_interval_m ( id_id , auxinput15_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput15_interval_m
  INTEGER id_id
  model_config_rec%auxinput15_interval_m(id_id) = auxinput15_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput15_interval_m
SUBROUTINE nl_set_auxinput15_interval_s ( id_id , auxinput15_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput15_interval_s
  INTEGER id_id
  model_config_rec%auxinput15_interval_s(id_id) = auxinput15_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput15_interval_s
SUBROUTINE nl_set_auxinput15_interval ( id_id , auxinput15_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput15_interval
  INTEGER id_id
  model_config_rec%auxinput15_interval(id_id) = auxinput15_interval
  RETURN
END SUBROUTINE nl_set_auxinput15_interval
SUBROUTINE nl_set_auxinput15_begin_y ( id_id , auxinput15_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput15_begin_y
  INTEGER id_id
  model_config_rec%auxinput15_begin_y(id_id) = auxinput15_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput15_begin_y
SUBROUTINE nl_set_auxinput15_begin_d ( id_id , auxinput15_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput15_begin_d
  INTEGER id_id
  model_config_rec%auxinput15_begin_d(id_id) = auxinput15_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput15_begin_d
SUBROUTINE nl_set_auxinput15_begin_h ( id_id , auxinput15_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput15_begin_h
  INTEGER id_id
  model_config_rec%auxinput15_begin_h(id_id) = auxinput15_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput15_begin_h
SUBROUTINE nl_set_auxinput15_begin_m ( id_id , auxinput15_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput15_begin_m
  INTEGER id_id
  model_config_rec%auxinput15_begin_m(id_id) = auxinput15_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput15_begin_m
SUBROUTINE nl_set_auxinput15_begin_s ( id_id , auxinput15_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput15_begin_s
  INTEGER id_id
  model_config_rec%auxinput15_begin_s(id_id) = auxinput15_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput15_begin_s
SUBROUTINE nl_set_auxinput15_begin ( id_id , auxinput15_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput15_begin
  INTEGER id_id
  model_config_rec%auxinput15_begin(id_id) = auxinput15_begin
  RETURN
END SUBROUTINE nl_set_auxinput15_begin
SUBROUTINE nl_set_auxinput15_end_y ( id_id , auxinput15_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput15_end_y
  INTEGER id_id
  model_config_rec%auxinput15_end_y(id_id) = auxinput15_end_y
  RETURN
END SUBROUTINE nl_set_auxinput15_end_y
SUBROUTINE nl_set_auxinput15_end_d ( id_id , auxinput15_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput15_end_d
  INTEGER id_id
  model_config_rec%auxinput15_end_d(id_id) = auxinput15_end_d
  RETURN
END SUBROUTINE nl_set_auxinput15_end_d
SUBROUTINE nl_set_auxinput15_end_h ( id_id , auxinput15_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput15_end_h
  INTEGER id_id
  model_config_rec%auxinput15_end_h(id_id) = auxinput15_end_h
  RETURN
END SUBROUTINE nl_set_auxinput15_end_h
SUBROUTINE nl_set_auxinput15_end_m ( id_id , auxinput15_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput15_end_m
  INTEGER id_id
  model_config_rec%auxinput15_end_m(id_id) = auxinput15_end_m
  RETURN
END SUBROUTINE nl_set_auxinput15_end_m
SUBROUTINE nl_set_auxinput15_end_s ( id_id , auxinput15_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput15_end_s
  INTEGER id_id
  model_config_rec%auxinput15_end_s(id_id) = auxinput15_end_s
  RETURN
END SUBROUTINE nl_set_auxinput15_end_s
SUBROUTINE nl_set_auxinput15_end ( id_id , auxinput15_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput15_end
  INTEGER id_id
  model_config_rec%auxinput15_end(id_id) = auxinput15_end
  RETURN
END SUBROUTINE nl_set_auxinput15_end
SUBROUTINE nl_set_io_form_auxinput15 ( id_id , io_form_auxinput15 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput15
  INTEGER id_id
  model_config_rec%io_form_auxinput15 = io_form_auxinput15 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput15
SUBROUTINE nl_set_frames_per_auxinput15 ( id_id , frames_per_auxinput15 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput15
  INTEGER id_id
  model_config_rec%frames_per_auxinput15(id_id) = frames_per_auxinput15
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput15
SUBROUTINE nl_set_auxinput16_inname ( id_id , auxinput16_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput16_inname
  INTEGER id_id
  model_config_rec%auxinput16_inname = trim(auxinput16_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput16_inname
SUBROUTINE nl_set_auxinput16_outname ( id_id , auxinput16_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput16_outname
  INTEGER id_id
  model_config_rec%auxinput16_outname = trim(auxinput16_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput16_outname
SUBROUTINE nl_set_auxinput16_interval_y ( id_id , auxinput16_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput16_interval_y
  INTEGER id_id
  model_config_rec%auxinput16_interval_y(id_id) = auxinput16_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput16_interval_y
SUBROUTINE nl_set_auxinput16_interval_d ( id_id , auxinput16_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput16_interval_d
  INTEGER id_id
  model_config_rec%auxinput16_interval_d(id_id) = auxinput16_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput16_interval_d
SUBROUTINE nl_set_auxinput16_interval_h ( id_id , auxinput16_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput16_interval_h
  INTEGER id_id
  model_config_rec%auxinput16_interval_h(id_id) = auxinput16_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput16_interval_h
SUBROUTINE nl_set_auxinput16_interval_m ( id_id , auxinput16_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput16_interval_m
  INTEGER id_id
  model_config_rec%auxinput16_interval_m(id_id) = auxinput16_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput16_interval_m
SUBROUTINE nl_set_auxinput16_interval_s ( id_id , auxinput16_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput16_interval_s
  INTEGER id_id
  model_config_rec%auxinput16_interval_s(id_id) = auxinput16_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput16_interval_s
SUBROUTINE nl_set_auxinput16_interval ( id_id , auxinput16_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput16_interval
  INTEGER id_id
  model_config_rec%auxinput16_interval(id_id) = auxinput16_interval
  RETURN
END SUBROUTINE nl_set_auxinput16_interval
SUBROUTINE nl_set_auxinput16_begin_y ( id_id , auxinput16_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput16_begin_y
  INTEGER id_id
  model_config_rec%auxinput16_begin_y(id_id) = auxinput16_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput16_begin_y
SUBROUTINE nl_set_auxinput16_begin_d ( id_id , auxinput16_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput16_begin_d
  INTEGER id_id
  model_config_rec%auxinput16_begin_d(id_id) = auxinput16_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput16_begin_d
SUBROUTINE nl_set_auxinput16_begin_h ( id_id , auxinput16_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput16_begin_h
  INTEGER id_id
  model_config_rec%auxinput16_begin_h(id_id) = auxinput16_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput16_begin_h
#endif
#if (NNN == 5)
SUBROUTINE nl_set_auxinput16_begin_m ( id_id , auxinput16_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput16_begin_m
  INTEGER id_id
  model_config_rec%auxinput16_begin_m(id_id) = auxinput16_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput16_begin_m
SUBROUTINE nl_set_auxinput16_begin_s ( id_id , auxinput16_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput16_begin_s
  INTEGER id_id
  model_config_rec%auxinput16_begin_s(id_id) = auxinput16_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput16_begin_s
SUBROUTINE nl_set_auxinput16_begin ( id_id , auxinput16_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput16_begin
  INTEGER id_id
  model_config_rec%auxinput16_begin(id_id) = auxinput16_begin
  RETURN
END SUBROUTINE nl_set_auxinput16_begin
SUBROUTINE nl_set_auxinput16_end_y ( id_id , auxinput16_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput16_end_y
  INTEGER id_id
  model_config_rec%auxinput16_end_y(id_id) = auxinput16_end_y
  RETURN
END SUBROUTINE nl_set_auxinput16_end_y
SUBROUTINE nl_set_auxinput16_end_d ( id_id , auxinput16_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput16_end_d
  INTEGER id_id
  model_config_rec%auxinput16_end_d(id_id) = auxinput16_end_d
  RETURN
END SUBROUTINE nl_set_auxinput16_end_d
SUBROUTINE nl_set_auxinput16_end_h ( id_id , auxinput16_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput16_end_h
  INTEGER id_id
  model_config_rec%auxinput16_end_h(id_id) = auxinput16_end_h
  RETURN
END SUBROUTINE nl_set_auxinput16_end_h
SUBROUTINE nl_set_auxinput16_end_m ( id_id , auxinput16_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput16_end_m
  INTEGER id_id
  model_config_rec%auxinput16_end_m(id_id) = auxinput16_end_m
  RETURN
END SUBROUTINE nl_set_auxinput16_end_m
SUBROUTINE nl_set_auxinput16_end_s ( id_id , auxinput16_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput16_end_s
  INTEGER id_id
  model_config_rec%auxinput16_end_s(id_id) = auxinput16_end_s
  RETURN
END SUBROUTINE nl_set_auxinput16_end_s
SUBROUTINE nl_set_auxinput16_end ( id_id , auxinput16_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput16_end
  INTEGER id_id
  model_config_rec%auxinput16_end(id_id) = auxinput16_end
  RETURN
END SUBROUTINE nl_set_auxinput16_end
SUBROUTINE nl_set_io_form_auxinput16 ( id_id , io_form_auxinput16 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput16
  INTEGER id_id
  model_config_rec%io_form_auxinput16 = io_form_auxinput16 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput16
SUBROUTINE nl_set_frames_per_auxinput16 ( id_id , frames_per_auxinput16 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput16
  INTEGER id_id
  model_config_rec%frames_per_auxinput16(id_id) = frames_per_auxinput16
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput16
SUBROUTINE nl_set_auxinput17_inname ( id_id , auxinput17_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput17_inname
  INTEGER id_id
  model_config_rec%auxinput17_inname = trim(auxinput17_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput17_inname
SUBROUTINE nl_set_auxinput17_outname ( id_id , auxinput17_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput17_outname
  INTEGER id_id
  model_config_rec%auxinput17_outname = trim(auxinput17_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput17_outname
SUBROUTINE nl_set_auxinput17_interval_y ( id_id , auxinput17_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput17_interval_y
  INTEGER id_id
  model_config_rec%auxinput17_interval_y(id_id) = auxinput17_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput17_interval_y
SUBROUTINE nl_set_auxinput17_interval_d ( id_id , auxinput17_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput17_interval_d
  INTEGER id_id
  model_config_rec%auxinput17_interval_d(id_id) = auxinput17_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput17_interval_d
SUBROUTINE nl_set_auxinput17_interval_h ( id_id , auxinput17_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput17_interval_h
  INTEGER id_id
  model_config_rec%auxinput17_interval_h(id_id) = auxinput17_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput17_interval_h
SUBROUTINE nl_set_auxinput17_interval_m ( id_id , auxinput17_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput17_interval_m
  INTEGER id_id
  model_config_rec%auxinput17_interval_m(id_id) = auxinput17_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput17_interval_m
SUBROUTINE nl_set_auxinput17_interval_s ( id_id , auxinput17_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput17_interval_s
  INTEGER id_id
  model_config_rec%auxinput17_interval_s(id_id) = auxinput17_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput17_interval_s
SUBROUTINE nl_set_auxinput17_interval ( id_id , auxinput17_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput17_interval
  INTEGER id_id
  model_config_rec%auxinput17_interval(id_id) = auxinput17_interval
  RETURN
END SUBROUTINE nl_set_auxinput17_interval
SUBROUTINE nl_set_auxinput17_begin_y ( id_id , auxinput17_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput17_begin_y
  INTEGER id_id
  model_config_rec%auxinput17_begin_y(id_id) = auxinput17_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput17_begin_y
SUBROUTINE nl_set_auxinput17_begin_d ( id_id , auxinput17_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput17_begin_d
  INTEGER id_id
  model_config_rec%auxinput17_begin_d(id_id) = auxinput17_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput17_begin_d
SUBROUTINE nl_set_auxinput17_begin_h ( id_id , auxinput17_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput17_begin_h
  INTEGER id_id
  model_config_rec%auxinput17_begin_h(id_id) = auxinput17_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput17_begin_h
SUBROUTINE nl_set_auxinput17_begin_m ( id_id , auxinput17_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput17_begin_m
  INTEGER id_id
  model_config_rec%auxinput17_begin_m(id_id) = auxinput17_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput17_begin_m
SUBROUTINE nl_set_auxinput17_begin_s ( id_id , auxinput17_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput17_begin_s
  INTEGER id_id
  model_config_rec%auxinput17_begin_s(id_id) = auxinput17_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput17_begin_s
SUBROUTINE nl_set_auxinput17_begin ( id_id , auxinput17_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput17_begin
  INTEGER id_id
  model_config_rec%auxinput17_begin(id_id) = auxinput17_begin
  RETURN
END SUBROUTINE nl_set_auxinput17_begin
SUBROUTINE nl_set_auxinput17_end_y ( id_id , auxinput17_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput17_end_y
  INTEGER id_id
  model_config_rec%auxinput17_end_y(id_id) = auxinput17_end_y
  RETURN
END SUBROUTINE nl_set_auxinput17_end_y
SUBROUTINE nl_set_auxinput17_end_d ( id_id , auxinput17_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput17_end_d
  INTEGER id_id
  model_config_rec%auxinput17_end_d(id_id) = auxinput17_end_d
  RETURN
END SUBROUTINE nl_set_auxinput17_end_d
SUBROUTINE nl_set_auxinput17_end_h ( id_id , auxinput17_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput17_end_h
  INTEGER id_id
  model_config_rec%auxinput17_end_h(id_id) = auxinput17_end_h
  RETURN
END SUBROUTINE nl_set_auxinput17_end_h
SUBROUTINE nl_set_auxinput17_end_m ( id_id , auxinput17_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput17_end_m
  INTEGER id_id
  model_config_rec%auxinput17_end_m(id_id) = auxinput17_end_m
  RETURN
END SUBROUTINE nl_set_auxinput17_end_m
SUBROUTINE nl_set_auxinput17_end_s ( id_id , auxinput17_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput17_end_s
  INTEGER id_id
  model_config_rec%auxinput17_end_s(id_id) = auxinput17_end_s
  RETURN
END SUBROUTINE nl_set_auxinput17_end_s
SUBROUTINE nl_set_auxinput17_end ( id_id , auxinput17_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput17_end
  INTEGER id_id
  model_config_rec%auxinput17_end(id_id) = auxinput17_end
  RETURN
END SUBROUTINE nl_set_auxinput17_end
SUBROUTINE nl_set_io_form_auxinput17 ( id_id , io_form_auxinput17 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput17
  INTEGER id_id
  model_config_rec%io_form_auxinput17 = io_form_auxinput17 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput17
SUBROUTINE nl_set_frames_per_auxinput17 ( id_id , frames_per_auxinput17 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput17
  INTEGER id_id
  model_config_rec%frames_per_auxinput17(id_id) = frames_per_auxinput17
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput17
SUBROUTINE nl_set_auxinput18_inname ( id_id , auxinput18_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput18_inname
  INTEGER id_id
  model_config_rec%auxinput18_inname = trim(auxinput18_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput18_inname
SUBROUTINE nl_set_auxinput18_outname ( id_id , auxinput18_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput18_outname
  INTEGER id_id
  model_config_rec%auxinput18_outname = trim(auxinput18_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput18_outname
SUBROUTINE nl_set_auxinput18_interval_y ( id_id , auxinput18_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_interval_y
  INTEGER id_id
  model_config_rec%auxinput18_interval_y(id_id) = auxinput18_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput18_interval_y
SUBROUTINE nl_set_auxinput18_interval_d ( id_id , auxinput18_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_interval_d
  INTEGER id_id
  model_config_rec%auxinput18_interval_d(id_id) = auxinput18_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput18_interval_d
SUBROUTINE nl_set_auxinput18_interval_h ( id_id , auxinput18_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_interval_h
  INTEGER id_id
  model_config_rec%auxinput18_interval_h(id_id) = auxinput18_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput18_interval_h
SUBROUTINE nl_set_auxinput18_interval_m ( id_id , auxinput18_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_interval_m
  INTEGER id_id
  model_config_rec%auxinput18_interval_m(id_id) = auxinput18_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput18_interval_m
SUBROUTINE nl_set_auxinput18_interval_s ( id_id , auxinput18_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_interval_s
  INTEGER id_id
  model_config_rec%auxinput18_interval_s(id_id) = auxinput18_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput18_interval_s
SUBROUTINE nl_set_auxinput18_interval ( id_id , auxinput18_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_interval
  INTEGER id_id
  model_config_rec%auxinput18_interval(id_id) = auxinput18_interval
  RETURN
END SUBROUTINE nl_set_auxinput18_interval
SUBROUTINE nl_set_auxinput18_begin_y ( id_id , auxinput18_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_begin_y
  INTEGER id_id
  model_config_rec%auxinput18_begin_y(id_id) = auxinput18_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput18_begin_y
SUBROUTINE nl_set_auxinput18_begin_d ( id_id , auxinput18_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_begin_d
  INTEGER id_id
  model_config_rec%auxinput18_begin_d(id_id) = auxinput18_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput18_begin_d
SUBROUTINE nl_set_auxinput18_begin_h ( id_id , auxinput18_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_begin_h
  INTEGER id_id
  model_config_rec%auxinput18_begin_h(id_id) = auxinput18_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput18_begin_h
SUBROUTINE nl_set_auxinput18_begin_m ( id_id , auxinput18_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_begin_m
  INTEGER id_id
  model_config_rec%auxinput18_begin_m(id_id) = auxinput18_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput18_begin_m
SUBROUTINE nl_set_auxinput18_begin_s ( id_id , auxinput18_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_begin_s
  INTEGER id_id
  model_config_rec%auxinput18_begin_s(id_id) = auxinput18_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput18_begin_s
SUBROUTINE nl_set_auxinput18_begin ( id_id , auxinput18_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_begin
  INTEGER id_id
  model_config_rec%auxinput18_begin(id_id) = auxinput18_begin
  RETURN
END SUBROUTINE nl_set_auxinput18_begin
SUBROUTINE nl_set_auxinput18_end_y ( id_id , auxinput18_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_end_y
  INTEGER id_id
  model_config_rec%auxinput18_end_y(id_id) = auxinput18_end_y
  RETURN
END SUBROUTINE nl_set_auxinput18_end_y
SUBROUTINE nl_set_auxinput18_end_d ( id_id , auxinput18_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_end_d
  INTEGER id_id
  model_config_rec%auxinput18_end_d(id_id) = auxinput18_end_d
  RETURN
END SUBROUTINE nl_set_auxinput18_end_d
SUBROUTINE nl_set_auxinput18_end_h ( id_id , auxinput18_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_end_h
  INTEGER id_id
  model_config_rec%auxinput18_end_h(id_id) = auxinput18_end_h
  RETURN
END SUBROUTINE nl_set_auxinput18_end_h
SUBROUTINE nl_set_auxinput18_end_m ( id_id , auxinput18_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_end_m
  INTEGER id_id
  model_config_rec%auxinput18_end_m(id_id) = auxinput18_end_m
  RETURN
END SUBROUTINE nl_set_auxinput18_end_m
SUBROUTINE nl_set_auxinput18_end_s ( id_id , auxinput18_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_end_s
  INTEGER id_id
  model_config_rec%auxinput18_end_s(id_id) = auxinput18_end_s
  RETURN
END SUBROUTINE nl_set_auxinput18_end_s
SUBROUTINE nl_set_auxinput18_end ( id_id , auxinput18_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput18_end
  INTEGER id_id
  model_config_rec%auxinput18_end(id_id) = auxinput18_end
  RETURN
END SUBROUTINE nl_set_auxinput18_end
SUBROUTINE nl_set_io_form_auxinput18 ( id_id , io_form_auxinput18 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput18
  INTEGER id_id
  model_config_rec%io_form_auxinput18 = io_form_auxinput18 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput18
SUBROUTINE nl_set_frames_per_auxinput18 ( id_id , frames_per_auxinput18 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput18
  INTEGER id_id
  model_config_rec%frames_per_auxinput18(id_id) = frames_per_auxinput18
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput18
SUBROUTINE nl_set_auxinput19_inname ( id_id , auxinput19_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput19_inname
  INTEGER id_id
  model_config_rec%auxinput19_inname = trim(auxinput19_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput19_inname
SUBROUTINE nl_set_auxinput19_outname ( id_id , auxinput19_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput19_outname
  INTEGER id_id
  model_config_rec%auxinput19_outname = trim(auxinput19_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput19_outname
SUBROUTINE nl_set_auxinput19_interval_y ( id_id , auxinput19_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_interval_y
  INTEGER id_id
  model_config_rec%auxinput19_interval_y(id_id) = auxinput19_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput19_interval_y
SUBROUTINE nl_set_auxinput19_interval_d ( id_id , auxinput19_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_interval_d
  INTEGER id_id
  model_config_rec%auxinput19_interval_d(id_id) = auxinput19_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput19_interval_d
SUBROUTINE nl_set_auxinput19_interval_h ( id_id , auxinput19_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_interval_h
  INTEGER id_id
  model_config_rec%auxinput19_interval_h(id_id) = auxinput19_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput19_interval_h
SUBROUTINE nl_set_auxinput19_interval_m ( id_id , auxinput19_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_interval_m
  INTEGER id_id
  model_config_rec%auxinput19_interval_m(id_id) = auxinput19_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput19_interval_m
SUBROUTINE nl_set_auxinput19_interval_s ( id_id , auxinput19_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_interval_s
  INTEGER id_id
  model_config_rec%auxinput19_interval_s(id_id) = auxinput19_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput19_interval_s
SUBROUTINE nl_set_auxinput19_interval ( id_id , auxinput19_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_interval
  INTEGER id_id
  model_config_rec%auxinput19_interval(id_id) = auxinput19_interval
  RETURN
END SUBROUTINE nl_set_auxinput19_interval
SUBROUTINE nl_set_auxinput19_begin_y ( id_id , auxinput19_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_begin_y
  INTEGER id_id
  model_config_rec%auxinput19_begin_y(id_id) = auxinput19_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput19_begin_y
SUBROUTINE nl_set_auxinput19_begin_d ( id_id , auxinput19_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_begin_d
  INTEGER id_id
  model_config_rec%auxinput19_begin_d(id_id) = auxinput19_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput19_begin_d
SUBROUTINE nl_set_auxinput19_begin_h ( id_id , auxinput19_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_begin_h
  INTEGER id_id
  model_config_rec%auxinput19_begin_h(id_id) = auxinput19_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput19_begin_h
SUBROUTINE nl_set_auxinput19_begin_m ( id_id , auxinput19_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_begin_m
  INTEGER id_id
  model_config_rec%auxinput19_begin_m(id_id) = auxinput19_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput19_begin_m
SUBROUTINE nl_set_auxinput19_begin_s ( id_id , auxinput19_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_begin_s
  INTEGER id_id
  model_config_rec%auxinput19_begin_s(id_id) = auxinput19_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput19_begin_s
SUBROUTINE nl_set_auxinput19_begin ( id_id , auxinput19_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_begin
  INTEGER id_id
  model_config_rec%auxinput19_begin(id_id) = auxinput19_begin
  RETURN
END SUBROUTINE nl_set_auxinput19_begin
SUBROUTINE nl_set_auxinput19_end_y ( id_id , auxinput19_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_end_y
  INTEGER id_id
  model_config_rec%auxinput19_end_y(id_id) = auxinput19_end_y
  RETURN
END SUBROUTINE nl_set_auxinput19_end_y
SUBROUTINE nl_set_auxinput19_end_d ( id_id , auxinput19_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_end_d
  INTEGER id_id
  model_config_rec%auxinput19_end_d(id_id) = auxinput19_end_d
  RETURN
END SUBROUTINE nl_set_auxinput19_end_d
SUBROUTINE nl_set_auxinput19_end_h ( id_id , auxinput19_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_end_h
  INTEGER id_id
  model_config_rec%auxinput19_end_h(id_id) = auxinput19_end_h
  RETURN
END SUBROUTINE nl_set_auxinput19_end_h
SUBROUTINE nl_set_auxinput19_end_m ( id_id , auxinput19_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_end_m
  INTEGER id_id
  model_config_rec%auxinput19_end_m(id_id) = auxinput19_end_m
  RETURN
END SUBROUTINE nl_set_auxinput19_end_m
SUBROUTINE nl_set_auxinput19_end_s ( id_id , auxinput19_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_end_s
  INTEGER id_id
  model_config_rec%auxinput19_end_s(id_id) = auxinput19_end_s
  RETURN
END SUBROUTINE nl_set_auxinput19_end_s
SUBROUTINE nl_set_auxinput19_end ( id_id , auxinput19_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput19_end
  INTEGER id_id
  model_config_rec%auxinput19_end(id_id) = auxinput19_end
  RETURN
END SUBROUTINE nl_set_auxinput19_end
SUBROUTINE nl_set_io_form_auxinput19 ( id_id , io_form_auxinput19 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput19
  INTEGER id_id
  model_config_rec%io_form_auxinput19 = io_form_auxinput19 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput19
SUBROUTINE nl_set_frames_per_auxinput19 ( id_id , frames_per_auxinput19 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput19
  INTEGER id_id
  model_config_rec%frames_per_auxinput19(id_id) = frames_per_auxinput19
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput19
SUBROUTINE nl_set_auxinput20_inname ( id_id , auxinput20_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput20_inname
  INTEGER id_id
  model_config_rec%auxinput20_inname = trim(auxinput20_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput20_inname
SUBROUTINE nl_set_auxinput20_outname ( id_id , auxinput20_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput20_outname
  INTEGER id_id
  model_config_rec%auxinput20_outname = trim(auxinput20_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput20_outname
SUBROUTINE nl_set_auxinput20_interval_y ( id_id , auxinput20_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_interval_y
  INTEGER id_id
  model_config_rec%auxinput20_interval_y(id_id) = auxinput20_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput20_interval_y
SUBROUTINE nl_set_auxinput20_interval_d ( id_id , auxinput20_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_interval_d
  INTEGER id_id
  model_config_rec%auxinput20_interval_d(id_id) = auxinput20_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput20_interval_d
SUBROUTINE nl_set_auxinput20_interval_h ( id_id , auxinput20_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_interval_h
  INTEGER id_id
  model_config_rec%auxinput20_interval_h(id_id) = auxinput20_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput20_interval_h
SUBROUTINE nl_set_auxinput20_interval_m ( id_id , auxinput20_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_interval_m
  INTEGER id_id
  model_config_rec%auxinput20_interval_m(id_id) = auxinput20_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput20_interval_m
SUBROUTINE nl_set_auxinput20_interval_s ( id_id , auxinput20_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_interval_s
  INTEGER id_id
  model_config_rec%auxinput20_interval_s(id_id) = auxinput20_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput20_interval_s
SUBROUTINE nl_set_auxinput20_interval ( id_id , auxinput20_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_interval
  INTEGER id_id
  model_config_rec%auxinput20_interval(id_id) = auxinput20_interval
  RETURN
END SUBROUTINE nl_set_auxinput20_interval
SUBROUTINE nl_set_auxinput20_begin_y ( id_id , auxinput20_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_begin_y
  INTEGER id_id
  model_config_rec%auxinput20_begin_y(id_id) = auxinput20_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput20_begin_y
SUBROUTINE nl_set_auxinput20_begin_d ( id_id , auxinput20_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_begin_d
  INTEGER id_id
  model_config_rec%auxinput20_begin_d(id_id) = auxinput20_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput20_begin_d
SUBROUTINE nl_set_auxinput20_begin_h ( id_id , auxinput20_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_begin_h
  INTEGER id_id
  model_config_rec%auxinput20_begin_h(id_id) = auxinput20_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput20_begin_h
SUBROUTINE nl_set_auxinput20_begin_m ( id_id , auxinput20_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_begin_m
  INTEGER id_id
  model_config_rec%auxinput20_begin_m(id_id) = auxinput20_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput20_begin_m
SUBROUTINE nl_set_auxinput20_begin_s ( id_id , auxinput20_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_begin_s
  INTEGER id_id
  model_config_rec%auxinput20_begin_s(id_id) = auxinput20_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput20_begin_s
SUBROUTINE nl_set_auxinput20_begin ( id_id , auxinput20_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_begin
  INTEGER id_id
  model_config_rec%auxinput20_begin(id_id) = auxinput20_begin
  RETURN
END SUBROUTINE nl_set_auxinput20_begin
SUBROUTINE nl_set_auxinput20_end_y ( id_id , auxinput20_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_end_y
  INTEGER id_id
  model_config_rec%auxinput20_end_y(id_id) = auxinput20_end_y
  RETURN
END SUBROUTINE nl_set_auxinput20_end_y
SUBROUTINE nl_set_auxinput20_end_d ( id_id , auxinput20_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_end_d
  INTEGER id_id
  model_config_rec%auxinput20_end_d(id_id) = auxinput20_end_d
  RETURN
END SUBROUTINE nl_set_auxinput20_end_d
SUBROUTINE nl_set_auxinput20_end_h ( id_id , auxinput20_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_end_h
  INTEGER id_id
  model_config_rec%auxinput20_end_h(id_id) = auxinput20_end_h
  RETURN
END SUBROUTINE nl_set_auxinput20_end_h
SUBROUTINE nl_set_auxinput20_end_m ( id_id , auxinput20_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_end_m
  INTEGER id_id
  model_config_rec%auxinput20_end_m(id_id) = auxinput20_end_m
  RETURN
END SUBROUTINE nl_set_auxinput20_end_m
SUBROUTINE nl_set_auxinput20_end_s ( id_id , auxinput20_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_end_s
  INTEGER id_id
  model_config_rec%auxinput20_end_s(id_id) = auxinput20_end_s
  RETURN
END SUBROUTINE nl_set_auxinput20_end_s
SUBROUTINE nl_set_auxinput20_end ( id_id , auxinput20_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput20_end
  INTEGER id_id
  model_config_rec%auxinput20_end(id_id) = auxinput20_end
  RETURN
END SUBROUTINE nl_set_auxinput20_end
SUBROUTINE nl_set_io_form_auxinput20 ( id_id , io_form_auxinput20 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput20
  INTEGER id_id
  model_config_rec%io_form_auxinput20 = io_form_auxinput20 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput20
SUBROUTINE nl_set_frames_per_auxinput20 ( id_id , frames_per_auxinput20 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput20
  INTEGER id_id
  model_config_rec%frames_per_auxinput20(id_id) = frames_per_auxinput20
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput20
SUBROUTINE nl_set_auxinput21_inname ( id_id , auxinput21_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput21_inname
  INTEGER id_id
  model_config_rec%auxinput21_inname = trim(auxinput21_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput21_inname
SUBROUTINE nl_set_auxinput21_outname ( id_id , auxinput21_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput21_outname
  INTEGER id_id
  model_config_rec%auxinput21_outname = trim(auxinput21_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput21_outname
SUBROUTINE nl_set_auxinput21_interval_y ( id_id , auxinput21_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_interval_y
  INTEGER id_id
  model_config_rec%auxinput21_interval_y(id_id) = auxinput21_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput21_interval_y
SUBROUTINE nl_set_auxinput21_interval_d ( id_id , auxinput21_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_interval_d
  INTEGER id_id
  model_config_rec%auxinput21_interval_d(id_id) = auxinput21_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput21_interval_d
SUBROUTINE nl_set_auxinput21_interval_h ( id_id , auxinput21_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_interval_h
  INTEGER id_id
  model_config_rec%auxinput21_interval_h(id_id) = auxinput21_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput21_interval_h
SUBROUTINE nl_set_auxinput21_interval_m ( id_id , auxinput21_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_interval_m
  INTEGER id_id
  model_config_rec%auxinput21_interval_m(id_id) = auxinput21_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput21_interval_m
SUBROUTINE nl_set_auxinput21_interval_s ( id_id , auxinput21_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_interval_s
  INTEGER id_id
  model_config_rec%auxinput21_interval_s(id_id) = auxinput21_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput21_interval_s
SUBROUTINE nl_set_auxinput21_interval ( id_id , auxinput21_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_interval
  INTEGER id_id
  model_config_rec%auxinput21_interval(id_id) = auxinput21_interval
  RETURN
END SUBROUTINE nl_set_auxinput21_interval
SUBROUTINE nl_set_auxinput21_begin_y ( id_id , auxinput21_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_begin_y
  INTEGER id_id
  model_config_rec%auxinput21_begin_y(id_id) = auxinput21_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput21_begin_y
SUBROUTINE nl_set_auxinput21_begin_d ( id_id , auxinput21_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_begin_d
  INTEGER id_id
  model_config_rec%auxinput21_begin_d(id_id) = auxinput21_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput21_begin_d
SUBROUTINE nl_set_auxinput21_begin_h ( id_id , auxinput21_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_begin_h
  INTEGER id_id
  model_config_rec%auxinput21_begin_h(id_id) = auxinput21_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput21_begin_h
SUBROUTINE nl_set_auxinput21_begin_m ( id_id , auxinput21_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_begin_m
  INTEGER id_id
  model_config_rec%auxinput21_begin_m(id_id) = auxinput21_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput21_begin_m
SUBROUTINE nl_set_auxinput21_begin_s ( id_id , auxinput21_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_begin_s
  INTEGER id_id
  model_config_rec%auxinput21_begin_s(id_id) = auxinput21_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput21_begin_s
SUBROUTINE nl_set_auxinput21_begin ( id_id , auxinput21_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_begin
  INTEGER id_id
  model_config_rec%auxinput21_begin(id_id) = auxinput21_begin
  RETURN
END SUBROUTINE nl_set_auxinput21_begin
SUBROUTINE nl_set_auxinput21_end_y ( id_id , auxinput21_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_end_y
  INTEGER id_id
  model_config_rec%auxinput21_end_y(id_id) = auxinput21_end_y
  RETURN
END SUBROUTINE nl_set_auxinput21_end_y
SUBROUTINE nl_set_auxinput21_end_d ( id_id , auxinput21_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_end_d
  INTEGER id_id
  model_config_rec%auxinput21_end_d(id_id) = auxinput21_end_d
  RETURN
END SUBROUTINE nl_set_auxinput21_end_d
SUBROUTINE nl_set_auxinput21_end_h ( id_id , auxinput21_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_end_h
  INTEGER id_id
  model_config_rec%auxinput21_end_h(id_id) = auxinput21_end_h
  RETURN
END SUBROUTINE nl_set_auxinput21_end_h
SUBROUTINE nl_set_auxinput21_end_m ( id_id , auxinput21_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_end_m
  INTEGER id_id
  model_config_rec%auxinput21_end_m(id_id) = auxinput21_end_m
  RETURN
END SUBROUTINE nl_set_auxinput21_end_m
SUBROUTINE nl_set_auxinput21_end_s ( id_id , auxinput21_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_end_s
  INTEGER id_id
  model_config_rec%auxinput21_end_s(id_id) = auxinput21_end_s
  RETURN
END SUBROUTINE nl_set_auxinput21_end_s
SUBROUTINE nl_set_auxinput21_end ( id_id , auxinput21_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput21_end
  INTEGER id_id
  model_config_rec%auxinput21_end(id_id) = auxinput21_end
  RETURN
END SUBROUTINE nl_set_auxinput21_end
SUBROUTINE nl_set_io_form_auxinput21 ( id_id , io_form_auxinput21 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput21
  INTEGER id_id
  model_config_rec%io_form_auxinput21 = io_form_auxinput21 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput21
SUBROUTINE nl_set_frames_per_auxinput21 ( id_id , frames_per_auxinput21 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput21
  INTEGER id_id
  model_config_rec%frames_per_auxinput21(id_id) = frames_per_auxinput21
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput21
SUBROUTINE nl_set_auxinput22_inname ( id_id , auxinput22_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput22_inname
  INTEGER id_id
  model_config_rec%auxinput22_inname = trim(auxinput22_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput22_inname
SUBROUTINE nl_set_auxinput22_outname ( id_id , auxinput22_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput22_outname
  INTEGER id_id
  model_config_rec%auxinput22_outname = trim(auxinput22_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput22_outname
SUBROUTINE nl_set_auxinput22_interval_y ( id_id , auxinput22_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_interval_y
  INTEGER id_id
  model_config_rec%auxinput22_interval_y(id_id) = auxinput22_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput22_interval_y
SUBROUTINE nl_set_auxinput22_interval_d ( id_id , auxinput22_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_interval_d
  INTEGER id_id
  model_config_rec%auxinput22_interval_d(id_id) = auxinput22_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput22_interval_d
SUBROUTINE nl_set_auxinput22_interval_h ( id_id , auxinput22_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_interval_h
  INTEGER id_id
  model_config_rec%auxinput22_interval_h(id_id) = auxinput22_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput22_interval_h
SUBROUTINE nl_set_auxinput22_interval_m ( id_id , auxinput22_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_interval_m
  INTEGER id_id
  model_config_rec%auxinput22_interval_m(id_id) = auxinput22_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput22_interval_m
SUBROUTINE nl_set_auxinput22_interval_s ( id_id , auxinput22_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_interval_s
  INTEGER id_id
  model_config_rec%auxinput22_interval_s(id_id) = auxinput22_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput22_interval_s
SUBROUTINE nl_set_auxinput22_interval ( id_id , auxinput22_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_interval
  INTEGER id_id
  model_config_rec%auxinput22_interval(id_id) = auxinput22_interval
  RETURN
END SUBROUTINE nl_set_auxinput22_interval
SUBROUTINE nl_set_auxinput22_begin_y ( id_id , auxinput22_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_begin_y
  INTEGER id_id
  model_config_rec%auxinput22_begin_y(id_id) = auxinput22_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput22_begin_y
SUBROUTINE nl_set_auxinput22_begin_d ( id_id , auxinput22_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_begin_d
  INTEGER id_id
  model_config_rec%auxinput22_begin_d(id_id) = auxinput22_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput22_begin_d
SUBROUTINE nl_set_auxinput22_begin_h ( id_id , auxinput22_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_begin_h
  INTEGER id_id
  model_config_rec%auxinput22_begin_h(id_id) = auxinput22_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput22_begin_h
SUBROUTINE nl_set_auxinput22_begin_m ( id_id , auxinput22_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_begin_m
  INTEGER id_id
  model_config_rec%auxinput22_begin_m(id_id) = auxinput22_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput22_begin_m
SUBROUTINE nl_set_auxinput22_begin_s ( id_id , auxinput22_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_begin_s
  INTEGER id_id
  model_config_rec%auxinput22_begin_s(id_id) = auxinput22_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput22_begin_s
SUBROUTINE nl_set_auxinput22_begin ( id_id , auxinput22_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_begin
  INTEGER id_id
  model_config_rec%auxinput22_begin(id_id) = auxinput22_begin
  RETURN
END SUBROUTINE nl_set_auxinput22_begin
SUBROUTINE nl_set_auxinput22_end_y ( id_id , auxinput22_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_end_y
  INTEGER id_id
  model_config_rec%auxinput22_end_y(id_id) = auxinput22_end_y
  RETURN
END SUBROUTINE nl_set_auxinput22_end_y
SUBROUTINE nl_set_auxinput22_end_d ( id_id , auxinput22_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_end_d
  INTEGER id_id
  model_config_rec%auxinput22_end_d(id_id) = auxinput22_end_d
  RETURN
END SUBROUTINE nl_set_auxinput22_end_d
SUBROUTINE nl_set_auxinput22_end_h ( id_id , auxinput22_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_end_h
  INTEGER id_id
  model_config_rec%auxinput22_end_h(id_id) = auxinput22_end_h
  RETURN
END SUBROUTINE nl_set_auxinput22_end_h
SUBROUTINE nl_set_auxinput22_end_m ( id_id , auxinput22_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_end_m
  INTEGER id_id
  model_config_rec%auxinput22_end_m(id_id) = auxinput22_end_m
  RETURN
END SUBROUTINE nl_set_auxinput22_end_m
SUBROUTINE nl_set_auxinput22_end_s ( id_id , auxinput22_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_end_s
  INTEGER id_id
  model_config_rec%auxinput22_end_s(id_id) = auxinput22_end_s
  RETURN
END SUBROUTINE nl_set_auxinput22_end_s
SUBROUTINE nl_set_auxinput22_end ( id_id , auxinput22_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput22_end
  INTEGER id_id
  model_config_rec%auxinput22_end(id_id) = auxinput22_end
  RETURN
END SUBROUTINE nl_set_auxinput22_end
SUBROUTINE nl_set_io_form_auxinput22 ( id_id , io_form_auxinput22 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput22
  INTEGER id_id
  model_config_rec%io_form_auxinput22 = io_form_auxinput22 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput22
SUBROUTINE nl_set_frames_per_auxinput22 ( id_id , frames_per_auxinput22 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput22
  INTEGER id_id
  model_config_rec%frames_per_auxinput22(id_id) = frames_per_auxinput22
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput22
SUBROUTINE nl_set_auxinput23_inname ( id_id , auxinput23_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput23_inname
  INTEGER id_id
  model_config_rec%auxinput23_inname = trim(auxinput23_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput23_inname
SUBROUTINE nl_set_auxinput23_outname ( id_id , auxinput23_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput23_outname
  INTEGER id_id
  model_config_rec%auxinput23_outname = trim(auxinput23_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput23_outname
SUBROUTINE nl_set_auxinput23_interval_y ( id_id , auxinput23_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_interval_y
  INTEGER id_id
  model_config_rec%auxinput23_interval_y(id_id) = auxinput23_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput23_interval_y
SUBROUTINE nl_set_auxinput23_interval_d ( id_id , auxinput23_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_interval_d
  INTEGER id_id
  model_config_rec%auxinput23_interval_d(id_id) = auxinput23_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput23_interval_d
SUBROUTINE nl_set_auxinput23_interval_h ( id_id , auxinput23_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_interval_h
  INTEGER id_id
  model_config_rec%auxinput23_interval_h(id_id) = auxinput23_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput23_interval_h
SUBROUTINE nl_set_auxinput23_interval_m ( id_id , auxinput23_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_interval_m
  INTEGER id_id
  model_config_rec%auxinput23_interval_m(id_id) = auxinput23_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput23_interval_m
SUBROUTINE nl_set_auxinput23_interval_s ( id_id , auxinput23_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_interval_s
  INTEGER id_id
  model_config_rec%auxinput23_interval_s(id_id) = auxinput23_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput23_interval_s
SUBROUTINE nl_set_auxinput23_interval ( id_id , auxinput23_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_interval
  INTEGER id_id
  model_config_rec%auxinput23_interval(id_id) = auxinput23_interval
  RETURN
END SUBROUTINE nl_set_auxinput23_interval
SUBROUTINE nl_set_auxinput23_begin_y ( id_id , auxinput23_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_begin_y
  INTEGER id_id
  model_config_rec%auxinput23_begin_y(id_id) = auxinput23_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput23_begin_y
SUBROUTINE nl_set_auxinput23_begin_d ( id_id , auxinput23_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_begin_d
  INTEGER id_id
  model_config_rec%auxinput23_begin_d(id_id) = auxinput23_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput23_begin_d
SUBROUTINE nl_set_auxinput23_begin_h ( id_id , auxinput23_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_begin_h
  INTEGER id_id
  model_config_rec%auxinput23_begin_h(id_id) = auxinput23_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput23_begin_h
SUBROUTINE nl_set_auxinput23_begin_m ( id_id , auxinput23_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_begin_m
  INTEGER id_id
  model_config_rec%auxinput23_begin_m(id_id) = auxinput23_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput23_begin_m
SUBROUTINE nl_set_auxinput23_begin_s ( id_id , auxinput23_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_begin_s
  INTEGER id_id
  model_config_rec%auxinput23_begin_s(id_id) = auxinput23_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput23_begin_s
SUBROUTINE nl_set_auxinput23_begin ( id_id , auxinput23_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_begin
  INTEGER id_id
  model_config_rec%auxinput23_begin(id_id) = auxinput23_begin
  RETURN
END SUBROUTINE nl_set_auxinput23_begin
SUBROUTINE nl_set_auxinput23_end_y ( id_id , auxinput23_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_end_y
  INTEGER id_id
  model_config_rec%auxinput23_end_y(id_id) = auxinput23_end_y
  RETURN
END SUBROUTINE nl_set_auxinput23_end_y
SUBROUTINE nl_set_auxinput23_end_d ( id_id , auxinput23_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_end_d
  INTEGER id_id
  model_config_rec%auxinput23_end_d(id_id) = auxinput23_end_d
  RETURN
END SUBROUTINE nl_set_auxinput23_end_d
SUBROUTINE nl_set_auxinput23_end_h ( id_id , auxinput23_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_end_h
  INTEGER id_id
  model_config_rec%auxinput23_end_h(id_id) = auxinput23_end_h
  RETURN
END SUBROUTINE nl_set_auxinput23_end_h
SUBROUTINE nl_set_auxinput23_end_m ( id_id , auxinput23_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_end_m
  INTEGER id_id
  model_config_rec%auxinput23_end_m(id_id) = auxinput23_end_m
  RETURN
END SUBROUTINE nl_set_auxinput23_end_m
SUBROUTINE nl_set_auxinput23_end_s ( id_id , auxinput23_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_end_s
  INTEGER id_id
  model_config_rec%auxinput23_end_s(id_id) = auxinput23_end_s
  RETURN
END SUBROUTINE nl_set_auxinput23_end_s
SUBROUTINE nl_set_auxinput23_end ( id_id , auxinput23_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput23_end
  INTEGER id_id
  model_config_rec%auxinput23_end(id_id) = auxinput23_end
  RETURN
END SUBROUTINE nl_set_auxinput23_end
SUBROUTINE nl_set_io_form_auxinput23 ( id_id , io_form_auxinput23 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput23
  INTEGER id_id
  model_config_rec%io_form_auxinput23 = io_form_auxinput23 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput23
SUBROUTINE nl_set_frames_per_auxinput23 ( id_id , frames_per_auxinput23 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput23
  INTEGER id_id
  model_config_rec%frames_per_auxinput23(id_id) = frames_per_auxinput23
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput23
SUBROUTINE nl_set_auxinput24_inname ( id_id , auxinput24_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput24_inname
  INTEGER id_id
  model_config_rec%auxinput24_inname = trim(auxinput24_inname) 
  RETURN
END SUBROUTINE nl_set_auxinput24_inname
SUBROUTINE nl_set_auxinput24_outname ( id_id , auxinput24_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: auxinput24_outname
  INTEGER id_id
  model_config_rec%auxinput24_outname = trim(auxinput24_outname) 
  RETURN
END SUBROUTINE nl_set_auxinput24_outname
SUBROUTINE nl_set_auxinput24_interval_y ( id_id , auxinput24_interval_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_interval_y
  INTEGER id_id
  model_config_rec%auxinput24_interval_y(id_id) = auxinput24_interval_y
  RETURN
END SUBROUTINE nl_set_auxinput24_interval_y
SUBROUTINE nl_set_auxinput24_interval_d ( id_id , auxinput24_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_interval_d
  INTEGER id_id
  model_config_rec%auxinput24_interval_d(id_id) = auxinput24_interval_d
  RETURN
END SUBROUTINE nl_set_auxinput24_interval_d
SUBROUTINE nl_set_auxinput24_interval_h ( id_id , auxinput24_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_interval_h
  INTEGER id_id
  model_config_rec%auxinput24_interval_h(id_id) = auxinput24_interval_h
  RETURN
END SUBROUTINE nl_set_auxinput24_interval_h
SUBROUTINE nl_set_auxinput24_interval_m ( id_id , auxinput24_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_interval_m
  INTEGER id_id
  model_config_rec%auxinput24_interval_m(id_id) = auxinput24_interval_m
  RETURN
END SUBROUTINE nl_set_auxinput24_interval_m
SUBROUTINE nl_set_auxinput24_interval_s ( id_id , auxinput24_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_interval_s
  INTEGER id_id
  model_config_rec%auxinput24_interval_s(id_id) = auxinput24_interval_s
  RETURN
END SUBROUTINE nl_set_auxinput24_interval_s
SUBROUTINE nl_set_auxinput24_interval ( id_id , auxinput24_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_interval
  INTEGER id_id
  model_config_rec%auxinput24_interval(id_id) = auxinput24_interval
  RETURN
END SUBROUTINE nl_set_auxinput24_interval
SUBROUTINE nl_set_auxinput24_begin_y ( id_id , auxinput24_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_begin_y
  INTEGER id_id
  model_config_rec%auxinput24_begin_y(id_id) = auxinput24_begin_y
  RETURN
END SUBROUTINE nl_set_auxinput24_begin_y
SUBROUTINE nl_set_auxinput24_begin_d ( id_id , auxinput24_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_begin_d
  INTEGER id_id
  model_config_rec%auxinput24_begin_d(id_id) = auxinput24_begin_d
  RETURN
END SUBROUTINE nl_set_auxinput24_begin_d
SUBROUTINE nl_set_auxinput24_begin_h ( id_id , auxinput24_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_begin_h
  INTEGER id_id
  model_config_rec%auxinput24_begin_h(id_id) = auxinput24_begin_h
  RETURN
END SUBROUTINE nl_set_auxinput24_begin_h
SUBROUTINE nl_set_auxinput24_begin_m ( id_id , auxinput24_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_begin_m
  INTEGER id_id
  model_config_rec%auxinput24_begin_m(id_id) = auxinput24_begin_m
  RETURN
END SUBROUTINE nl_set_auxinput24_begin_m
SUBROUTINE nl_set_auxinput24_begin_s ( id_id , auxinput24_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_begin_s
  INTEGER id_id
  model_config_rec%auxinput24_begin_s(id_id) = auxinput24_begin_s
  RETURN
END SUBROUTINE nl_set_auxinput24_begin_s
SUBROUTINE nl_set_auxinput24_begin ( id_id , auxinput24_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_begin
  INTEGER id_id
  model_config_rec%auxinput24_begin(id_id) = auxinput24_begin
  RETURN
END SUBROUTINE nl_set_auxinput24_begin
SUBROUTINE nl_set_auxinput24_end_y ( id_id , auxinput24_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_end_y
  INTEGER id_id
  model_config_rec%auxinput24_end_y(id_id) = auxinput24_end_y
  RETURN
END SUBROUTINE nl_set_auxinput24_end_y
SUBROUTINE nl_set_auxinput24_end_d ( id_id , auxinput24_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_end_d
  INTEGER id_id
  model_config_rec%auxinput24_end_d(id_id) = auxinput24_end_d
  RETURN
END SUBROUTINE nl_set_auxinput24_end_d
SUBROUTINE nl_set_auxinput24_end_h ( id_id , auxinput24_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_end_h
  INTEGER id_id
  model_config_rec%auxinput24_end_h(id_id) = auxinput24_end_h
  RETURN
END SUBROUTINE nl_set_auxinput24_end_h
SUBROUTINE nl_set_auxinput24_end_m ( id_id , auxinput24_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_end_m
  INTEGER id_id
  model_config_rec%auxinput24_end_m(id_id) = auxinput24_end_m
  RETURN
END SUBROUTINE nl_set_auxinput24_end_m
SUBROUTINE nl_set_auxinput24_end_s ( id_id , auxinput24_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_end_s
  INTEGER id_id
  model_config_rec%auxinput24_end_s(id_id) = auxinput24_end_s
  RETURN
END SUBROUTINE nl_set_auxinput24_end_s
SUBROUTINE nl_set_auxinput24_end ( id_id , auxinput24_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: auxinput24_end
  INTEGER id_id
  model_config_rec%auxinput24_end(id_id) = auxinput24_end
  RETURN
END SUBROUTINE nl_set_auxinput24_end
SUBROUTINE nl_set_io_form_auxinput24 ( id_id , io_form_auxinput24 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_auxinput24
  INTEGER id_id
  model_config_rec%io_form_auxinput24 = io_form_auxinput24 
  RETURN
END SUBROUTINE nl_set_io_form_auxinput24
SUBROUTINE nl_set_frames_per_auxinput24 ( id_id , frames_per_auxinput24 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_auxinput24
  INTEGER id_id
  model_config_rec%frames_per_auxinput24(id_id) = frames_per_auxinput24
  RETURN
END SUBROUTINE nl_set_frames_per_auxinput24
SUBROUTINE nl_set_history_interval ( id_id , history_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_interval
  INTEGER id_id
  model_config_rec%history_interval(id_id) = history_interval
  RETURN
END SUBROUTINE nl_set_history_interval
SUBROUTINE nl_set_frames_per_outfile ( id_id , frames_per_outfile )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: frames_per_outfile
  INTEGER id_id
  model_config_rec%frames_per_outfile(id_id) = frames_per_outfile
  RETURN
END SUBROUTINE nl_set_frames_per_outfile
#endif
#if (NNN == 6)
SUBROUTINE nl_set_restart ( id_id , restart )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: restart
  INTEGER id_id
  model_config_rec%restart = restart 
  RETURN
END SUBROUTINE nl_set_restart
SUBROUTINE nl_set_restart_interval ( id_id , restart_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_interval
  INTEGER id_id
  model_config_rec%restart_interval = restart_interval 
  RETURN
END SUBROUTINE nl_set_restart_interval
SUBROUTINE nl_set_io_form_input ( id_id , io_form_input )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_input
  INTEGER id_id
  model_config_rec%io_form_input = io_form_input 
  RETURN
END SUBROUTINE nl_set_io_form_input
SUBROUTINE nl_set_io_form_history ( id_id , io_form_history )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_history
  INTEGER id_id
  model_config_rec%io_form_history = io_form_history 
  RETURN
END SUBROUTINE nl_set_io_form_history
SUBROUTINE nl_set_io_form_restart ( id_id , io_form_restart )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_restart
  INTEGER id_id
  model_config_rec%io_form_restart = io_form_restart 
  RETURN
END SUBROUTINE nl_set_io_form_restart
SUBROUTINE nl_set_io_form_boundary ( id_id , io_form_boundary )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: io_form_boundary
  INTEGER id_id
  model_config_rec%io_form_boundary = io_form_boundary 
  RETURN
END SUBROUTINE nl_set_io_form_boundary
SUBROUTINE nl_set_debug_level ( id_id , debug_level )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: debug_level
  INTEGER id_id
  model_config_rec%debug_level = debug_level 
  RETURN
END SUBROUTINE nl_set_debug_level
SUBROUTINE nl_set_self_test_domain ( id_id , self_test_domain )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: self_test_domain
  INTEGER id_id
  model_config_rec%self_test_domain = self_test_domain 
  RETURN
END SUBROUTINE nl_set_self_test_domain
SUBROUTINE nl_set_history_outname ( id_id , history_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: history_outname
  INTEGER id_id
  model_config_rec%history_outname = trim(history_outname) 
  RETURN
END SUBROUTINE nl_set_history_outname
SUBROUTINE nl_set_history_inname ( id_id , history_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: history_inname
  INTEGER id_id
  model_config_rec%history_inname = trim(history_inname) 
  RETURN
END SUBROUTINE nl_set_history_inname
SUBROUTINE nl_set_use_netcdf_classic ( id_id , use_netcdf_classic )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: use_netcdf_classic
  INTEGER id_id
  model_config_rec%use_netcdf_classic = use_netcdf_classic 
  RETURN
END SUBROUTINE nl_set_use_netcdf_classic
SUBROUTINE nl_set_history_interval_d ( id_id , history_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_interval_d
  INTEGER id_id
  model_config_rec%history_interval_d(id_id) = history_interval_d
  RETURN
END SUBROUTINE nl_set_history_interval_d
SUBROUTINE nl_set_history_interval_h ( id_id , history_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_interval_h
  INTEGER id_id
  model_config_rec%history_interval_h(id_id) = history_interval_h
  RETURN
END SUBROUTINE nl_set_history_interval_h
SUBROUTINE nl_set_history_interval_m ( id_id , history_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_interval_m
  INTEGER id_id
  model_config_rec%history_interval_m(id_id) = history_interval_m
  RETURN
END SUBROUTINE nl_set_history_interval_m
SUBROUTINE nl_set_history_interval_s ( id_id , history_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_interval_s
  INTEGER id_id
  model_config_rec%history_interval_s(id_id) = history_interval_s
  RETURN
END SUBROUTINE nl_set_history_interval_s
SUBROUTINE nl_set_inputout_interval_d ( id_id , inputout_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_interval_d
  INTEGER id_id
  model_config_rec%inputout_interval_d(id_id) = inputout_interval_d
  RETURN
END SUBROUTINE nl_set_inputout_interval_d
SUBROUTINE nl_set_inputout_interval_h ( id_id , inputout_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_interval_h
  INTEGER id_id
  model_config_rec%inputout_interval_h(id_id) = inputout_interval_h
  RETURN
END SUBROUTINE nl_set_inputout_interval_h
SUBROUTINE nl_set_inputout_interval_m ( id_id , inputout_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_interval_m
  INTEGER id_id
  model_config_rec%inputout_interval_m(id_id) = inputout_interval_m
  RETURN
END SUBROUTINE nl_set_inputout_interval_m
SUBROUTINE nl_set_inputout_interval_s ( id_id , inputout_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_interval_s
  INTEGER id_id
  model_config_rec%inputout_interval_s(id_id) = inputout_interval_s
  RETURN
END SUBROUTINE nl_set_inputout_interval_s
SUBROUTINE nl_set_inputout_interval ( id_id , inputout_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_interval
  INTEGER id_id
  model_config_rec%inputout_interval(id_id) = inputout_interval
  RETURN
END SUBROUTINE nl_set_inputout_interval
SUBROUTINE nl_set_restart_interval_d ( id_id , restart_interval_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_interval_d
  INTEGER id_id
  model_config_rec%restart_interval_d = restart_interval_d 
  RETURN
END SUBROUTINE nl_set_restart_interval_d
SUBROUTINE nl_set_restart_interval_h ( id_id , restart_interval_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_interval_h
  INTEGER id_id
  model_config_rec%restart_interval_h = restart_interval_h 
  RETURN
END SUBROUTINE nl_set_restart_interval_h
SUBROUTINE nl_set_restart_interval_m ( id_id , restart_interval_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_interval_m
  INTEGER id_id
  model_config_rec%restart_interval_m = restart_interval_m 
  RETURN
END SUBROUTINE nl_set_restart_interval_m
SUBROUTINE nl_set_restart_interval_s ( id_id , restart_interval_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_interval_s
  INTEGER id_id
  model_config_rec%restart_interval_s = restart_interval_s 
  RETURN
END SUBROUTINE nl_set_restart_interval_s
SUBROUTINE nl_set_history_begin_y ( id_id , history_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_begin_y
  INTEGER id_id
  model_config_rec%history_begin_y(id_id) = history_begin_y
  RETURN
END SUBROUTINE nl_set_history_begin_y
SUBROUTINE nl_set_history_begin_d ( id_id , history_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_begin_d
  INTEGER id_id
  model_config_rec%history_begin_d(id_id) = history_begin_d
  RETURN
END SUBROUTINE nl_set_history_begin_d
SUBROUTINE nl_set_history_begin_h ( id_id , history_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_begin_h
  INTEGER id_id
  model_config_rec%history_begin_h(id_id) = history_begin_h
  RETURN
END SUBROUTINE nl_set_history_begin_h
SUBROUTINE nl_set_history_begin_m ( id_id , history_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_begin_m
  INTEGER id_id
  model_config_rec%history_begin_m(id_id) = history_begin_m
  RETURN
END SUBROUTINE nl_set_history_begin_m
SUBROUTINE nl_set_history_begin_s ( id_id , history_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_begin_s
  INTEGER id_id
  model_config_rec%history_begin_s(id_id) = history_begin_s
  RETURN
END SUBROUTINE nl_set_history_begin_s
SUBROUTINE nl_set_history_begin ( id_id , history_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_begin
  INTEGER id_id
  model_config_rec%history_begin(id_id) = history_begin
  RETURN
END SUBROUTINE nl_set_history_begin
SUBROUTINE nl_set_inputout_begin_y ( id_id , inputout_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_begin_y
  INTEGER id_id
  model_config_rec%inputout_begin_y(id_id) = inputout_begin_y
  RETURN
END SUBROUTINE nl_set_inputout_begin_y
SUBROUTINE nl_set_inputout_begin_d ( id_id , inputout_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_begin_d
  INTEGER id_id
  model_config_rec%inputout_begin_d(id_id) = inputout_begin_d
  RETURN
END SUBROUTINE nl_set_inputout_begin_d
SUBROUTINE nl_set_inputout_begin_h ( id_id , inputout_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_begin_h
  INTEGER id_id
  model_config_rec%inputout_begin_h(id_id) = inputout_begin_h
  RETURN
END SUBROUTINE nl_set_inputout_begin_h
SUBROUTINE nl_set_inputout_begin_m ( id_id , inputout_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_begin_m
  INTEGER id_id
  model_config_rec%inputout_begin_m(id_id) = inputout_begin_m
  RETURN
END SUBROUTINE nl_set_inputout_begin_m
SUBROUTINE nl_set_inputout_begin_s ( id_id , inputout_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_begin_s
  INTEGER id_id
  model_config_rec%inputout_begin_s(id_id) = inputout_begin_s
  RETURN
END SUBROUTINE nl_set_inputout_begin_s
SUBROUTINE nl_set_restart_begin_y ( id_id , restart_begin_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_begin_y
  INTEGER id_id
  model_config_rec%restart_begin_y = restart_begin_y 
  RETURN
END SUBROUTINE nl_set_restart_begin_y
SUBROUTINE nl_set_restart_begin_d ( id_id , restart_begin_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_begin_d
  INTEGER id_id
  model_config_rec%restart_begin_d = restart_begin_d 
  RETURN
END SUBROUTINE nl_set_restart_begin_d
SUBROUTINE nl_set_restart_begin_h ( id_id , restart_begin_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_begin_h
  INTEGER id_id
  model_config_rec%restart_begin_h = restart_begin_h 
  RETURN
END SUBROUTINE nl_set_restart_begin_h
SUBROUTINE nl_set_restart_begin_m ( id_id , restart_begin_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_begin_m
  INTEGER id_id
  model_config_rec%restart_begin_m = restart_begin_m 
  RETURN
END SUBROUTINE nl_set_restart_begin_m
SUBROUTINE nl_set_restart_begin_s ( id_id , restart_begin_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_begin_s
  INTEGER id_id
  model_config_rec%restart_begin_s = restart_begin_s 
  RETURN
END SUBROUTINE nl_set_restart_begin_s
SUBROUTINE nl_set_restart_begin ( id_id , restart_begin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: restart_begin
  INTEGER id_id
  model_config_rec%restart_begin = restart_begin 
  RETURN
END SUBROUTINE nl_set_restart_begin
SUBROUTINE nl_set_history_end_y ( id_id , history_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_end_y
  INTEGER id_id
  model_config_rec%history_end_y(id_id) = history_end_y
  RETURN
END SUBROUTINE nl_set_history_end_y
SUBROUTINE nl_set_history_end_d ( id_id , history_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_end_d
  INTEGER id_id
  model_config_rec%history_end_d(id_id) = history_end_d
  RETURN
END SUBROUTINE nl_set_history_end_d
SUBROUTINE nl_set_history_end_h ( id_id , history_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_end_h
  INTEGER id_id
  model_config_rec%history_end_h(id_id) = history_end_h
  RETURN
END SUBROUTINE nl_set_history_end_h
SUBROUTINE nl_set_history_end_m ( id_id , history_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_end_m
  INTEGER id_id
  model_config_rec%history_end_m(id_id) = history_end_m
  RETURN
END SUBROUTINE nl_set_history_end_m
SUBROUTINE nl_set_history_end_s ( id_id , history_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_end_s
  INTEGER id_id
  model_config_rec%history_end_s(id_id) = history_end_s
  RETURN
END SUBROUTINE nl_set_history_end_s
SUBROUTINE nl_set_history_end ( id_id , history_end )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: history_end
  INTEGER id_id
  model_config_rec%history_end(id_id) = history_end
  RETURN
END SUBROUTINE nl_set_history_end
SUBROUTINE nl_set_inputout_end_y ( id_id , inputout_end_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_end_y
  INTEGER id_id
  model_config_rec%inputout_end_y(id_id) = inputout_end_y
  RETURN
END SUBROUTINE nl_set_inputout_end_y
SUBROUTINE nl_set_inputout_end_d ( id_id , inputout_end_d )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_end_d
  INTEGER id_id
  model_config_rec%inputout_end_d(id_id) = inputout_end_d
  RETURN
END SUBROUTINE nl_set_inputout_end_d
SUBROUTINE nl_set_inputout_end_h ( id_id , inputout_end_h )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_end_h
  INTEGER id_id
  model_config_rec%inputout_end_h(id_id) = inputout_end_h
  RETURN
END SUBROUTINE nl_set_inputout_end_h
SUBROUTINE nl_set_inputout_end_m ( id_id , inputout_end_m )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_end_m
  INTEGER id_id
  model_config_rec%inputout_end_m(id_id) = inputout_end_m
  RETURN
END SUBROUTINE nl_set_inputout_end_m
SUBROUTINE nl_set_inputout_end_s ( id_id , inputout_end_s )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: inputout_end_s
  INTEGER id_id
  model_config_rec%inputout_end_s(id_id) = inputout_end_s
  RETURN
END SUBROUTINE nl_set_inputout_end_s
SUBROUTINE nl_set_simulation_start_year ( id_id , simulation_start_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: simulation_start_year
  INTEGER id_id
  model_config_rec%simulation_start_year = simulation_start_year 
  RETURN
END SUBROUTINE nl_set_simulation_start_year
SUBROUTINE nl_set_simulation_start_month ( id_id , simulation_start_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: simulation_start_month
  INTEGER id_id
  model_config_rec%simulation_start_month = simulation_start_month 
  RETURN
END SUBROUTINE nl_set_simulation_start_month
SUBROUTINE nl_set_simulation_start_day ( id_id , simulation_start_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: simulation_start_day
  INTEGER id_id
  model_config_rec%simulation_start_day = simulation_start_day 
  RETURN
END SUBROUTINE nl_set_simulation_start_day
SUBROUTINE nl_set_simulation_start_hour ( id_id , simulation_start_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: simulation_start_hour
  INTEGER id_id
  model_config_rec%simulation_start_hour = simulation_start_hour 
  RETURN
END SUBROUTINE nl_set_simulation_start_hour
SUBROUTINE nl_set_simulation_start_minute ( id_id , simulation_start_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: simulation_start_minute
  INTEGER id_id
  model_config_rec%simulation_start_minute = simulation_start_minute 
  RETURN
END SUBROUTINE nl_set_simulation_start_minute
SUBROUTINE nl_set_simulation_start_second ( id_id , simulation_start_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: simulation_start_second
  INTEGER id_id
  model_config_rec%simulation_start_second = simulation_start_second 
  RETURN
END SUBROUTINE nl_set_simulation_start_second
SUBROUTINE nl_set_reset_simulation_start ( id_id , reset_simulation_start )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: reset_simulation_start
  INTEGER id_id
  model_config_rec%reset_simulation_start = reset_simulation_start 
  RETURN
END SUBROUTINE nl_set_reset_simulation_start
SUBROUTINE nl_set_sr_x ( id_id , sr_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sr_x
  INTEGER id_id
  model_config_rec%sr_x(id_id) = sr_x
  RETURN
END SUBROUTINE nl_set_sr_x
SUBROUTINE nl_set_sr_y ( id_id , sr_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sr_y
  INTEGER id_id
  model_config_rec%sr_y(id_id) = sr_y
  RETURN
END SUBROUTINE nl_set_sr_y
SUBROUTINE nl_set_iofields_filename ( id_id , iofields_filename )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: iofields_filename
  INTEGER id_id
  model_config_rec%iofields_filename(id_id) = iofields_filename
  RETURN
END SUBROUTINE nl_set_iofields_filename
SUBROUTINE nl_set_ignore_iofields_warning ( id_id , ignore_iofields_warning )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: ignore_iofields_warning
  INTEGER id_id
  model_config_rec%ignore_iofields_warning = ignore_iofields_warning 
  RETURN
END SUBROUTINE nl_set_ignore_iofields_warning
SUBROUTINE nl_set_ncd_nofill ( id_id , ncd_nofill )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: ncd_nofill
  INTEGER id_id
  model_config_rec%ncd_nofill = ncd_nofill 
  RETURN
END SUBROUTINE nl_set_ncd_nofill
SUBROUTINE nl_set_julyr ( id_id , julyr )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: julyr
  INTEGER id_id
  model_config_rec%julyr(id_id) = julyr
  RETURN
END SUBROUTINE nl_set_julyr
SUBROUTINE nl_set_julday ( id_id , julday )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: julday
  INTEGER id_id
  model_config_rec%julday(id_id) = julday
  RETURN
END SUBROUTINE nl_set_julday
SUBROUTINE nl_set_gmt ( id_id , gmt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: gmt
  INTEGER id_id
  model_config_rec%gmt(id_id) = gmt
  RETURN
END SUBROUTINE nl_set_gmt
SUBROUTINE nl_set_high_freq_outname ( id_id , high_freq_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: high_freq_outname
  INTEGER id_id
  model_config_rec%high_freq_outname = trim(high_freq_outname) 
  RETURN
END SUBROUTINE nl_set_high_freq_outname
SUBROUTINE nl_set_partial_atcf_outname ( id_id , partial_atcf_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: partial_atcf_outname
  INTEGER id_id
  model_config_rec%partial_atcf_outname = trim(partial_atcf_outname) 
  RETURN
END SUBROUTINE nl_set_partial_atcf_outname
SUBROUTINE nl_set_input_inname ( id_id , input_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: input_inname
  INTEGER id_id
  model_config_rec%input_inname = trim(input_inname) 
  RETURN
END SUBROUTINE nl_set_input_inname
SUBROUTINE nl_set_input_outname ( id_id , input_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: input_outname
  INTEGER id_id
  model_config_rec%input_outname = trim(input_outname) 
  RETURN
END SUBROUTINE nl_set_input_outname
SUBROUTINE nl_set_bdy_inname ( id_id , bdy_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: bdy_inname
  INTEGER id_id
  model_config_rec%bdy_inname = trim(bdy_inname) 
  RETURN
END SUBROUTINE nl_set_bdy_inname
SUBROUTINE nl_set_bdy_outname ( id_id , bdy_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: bdy_outname
  INTEGER id_id
  model_config_rec%bdy_outname = trim(bdy_outname) 
  RETURN
END SUBROUTINE nl_set_bdy_outname
SUBROUTINE nl_set_rst_inname ( id_id , rst_inname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: rst_inname
  INTEGER id_id
  model_config_rec%rst_inname = trim(rst_inname) 
  RETURN
END SUBROUTINE nl_set_rst_inname
SUBROUTINE nl_set_rst_outname ( id_id , rst_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: rst_outname
  INTEGER id_id
  model_config_rec%rst_outname = trim(rst_outname) 
  RETURN
END SUBROUTINE nl_set_rst_outname
SUBROUTINE nl_set_anl_outname ( id_id , anl_outname )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: anl_outname
  INTEGER id_id
  model_config_rec%anl_outname(id_id) = anl_outname
  RETURN
END SUBROUTINE nl_set_anl_outname
SUBROUTINE nl_set_write_input ( id_id , write_input )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: write_input
  INTEGER id_id
  model_config_rec%write_input = write_input 
  RETURN
END SUBROUTINE nl_set_write_input
SUBROUTINE nl_set_write_restart_at_0h ( id_id , write_restart_at_0h )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: write_restart_at_0h
  INTEGER id_id
  model_config_rec%write_restart_at_0h = write_restart_at_0h 
  RETURN
END SUBROUTINE nl_set_write_restart_at_0h
SUBROUTINE nl_set_write_hist_at_0h_rst ( id_id , write_hist_at_0h_rst )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: write_hist_at_0h_rst
  INTEGER id_id
  model_config_rec%write_hist_at_0h_rst = write_hist_at_0h_rst 
  RETURN
END SUBROUTINE nl_set_write_hist_at_0h_rst
SUBROUTINE nl_set_adjust_output_times ( id_id , adjust_output_times )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: adjust_output_times
  INTEGER id_id
  model_config_rec%adjust_output_times = adjust_output_times 
  RETURN
END SUBROUTINE nl_set_adjust_output_times
SUBROUTINE nl_set_adjust_input_times ( id_id , adjust_input_times )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: adjust_input_times
  INTEGER id_id
  model_config_rec%adjust_input_times = adjust_input_times 
  RETURN
END SUBROUTINE nl_set_adjust_input_times
SUBROUTINE nl_set_tstart ( id_id , tstart )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: tstart
  INTEGER id_id
  model_config_rec%tstart(id_id) = tstart
  RETURN
END SUBROUTINE nl_set_tstart
SUBROUTINE nl_set_nocolons ( id_id , nocolons )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: nocolons
  INTEGER id_id
  model_config_rec%nocolons = nocolons 
  RETURN
END SUBROUTINE nl_set_nocolons
SUBROUTINE nl_set_cycling ( id_id , cycling )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: cycling
  INTEGER id_id
  model_config_rec%cycling = cycling 
  RETURN
END SUBROUTINE nl_set_cycling
SUBROUTINE nl_set_output_ready_flag ( id_id , output_ready_flag )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: output_ready_flag
  INTEGER id_id
  model_config_rec%output_ready_flag = output_ready_flag 
  RETURN
END SUBROUTINE nl_set_output_ready_flag
SUBROUTINE nl_set_dfi_opt ( id_id , dfi_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_opt
  INTEGER id_id
  model_config_rec%dfi_opt = dfi_opt 
  RETURN
END SUBROUTINE nl_set_dfi_opt
SUBROUTINE nl_set_dfi_savehydmeteors ( id_id , dfi_savehydmeteors )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_savehydmeteors
  INTEGER id_id
  model_config_rec%dfi_savehydmeteors = dfi_savehydmeteors 
  RETURN
END SUBROUTINE nl_set_dfi_savehydmeteors
SUBROUTINE nl_set_dfi_nfilter ( id_id , dfi_nfilter )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_nfilter
  INTEGER id_id
  model_config_rec%dfi_nfilter = dfi_nfilter 
  RETURN
END SUBROUTINE nl_set_dfi_nfilter
SUBROUTINE nl_set_dfi_write_filtered_input ( id_id , dfi_write_filtered_input )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: dfi_write_filtered_input
  INTEGER id_id
  model_config_rec%dfi_write_filtered_input = dfi_write_filtered_input 
  RETURN
END SUBROUTINE nl_set_dfi_write_filtered_input
SUBROUTINE nl_set_dfi_write_dfi_history ( id_id , dfi_write_dfi_history )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: dfi_write_dfi_history
  INTEGER id_id
  model_config_rec%dfi_write_dfi_history = dfi_write_dfi_history 
  RETURN
END SUBROUTINE nl_set_dfi_write_dfi_history
SUBROUTINE nl_set_dfi_cutoff_seconds ( id_id , dfi_cutoff_seconds )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_cutoff_seconds
  INTEGER id_id
  model_config_rec%dfi_cutoff_seconds = dfi_cutoff_seconds 
  RETURN
END SUBROUTINE nl_set_dfi_cutoff_seconds
SUBROUTINE nl_set_dfi_time_dim ( id_id , dfi_time_dim )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_time_dim
  INTEGER id_id
  model_config_rec%dfi_time_dim = dfi_time_dim 
  RETURN
END SUBROUTINE nl_set_dfi_time_dim
SUBROUTINE nl_set_dfi_fwdstop_year ( id_id , dfi_fwdstop_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_fwdstop_year
  INTEGER id_id
  model_config_rec%dfi_fwdstop_year = dfi_fwdstop_year 
  RETURN
END SUBROUTINE nl_set_dfi_fwdstop_year
SUBROUTINE nl_set_dfi_fwdstop_month ( id_id , dfi_fwdstop_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_fwdstop_month
  INTEGER id_id
  model_config_rec%dfi_fwdstop_month = dfi_fwdstop_month 
  RETURN
END SUBROUTINE nl_set_dfi_fwdstop_month
SUBROUTINE nl_set_dfi_fwdstop_day ( id_id , dfi_fwdstop_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_fwdstop_day
  INTEGER id_id
  model_config_rec%dfi_fwdstop_day = dfi_fwdstop_day 
  RETURN
END SUBROUTINE nl_set_dfi_fwdstop_day
SUBROUTINE nl_set_dfi_fwdstop_hour ( id_id , dfi_fwdstop_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_fwdstop_hour
  INTEGER id_id
  model_config_rec%dfi_fwdstop_hour = dfi_fwdstop_hour 
  RETURN
END SUBROUTINE nl_set_dfi_fwdstop_hour
SUBROUTINE nl_set_dfi_fwdstop_minute ( id_id , dfi_fwdstop_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_fwdstop_minute
  INTEGER id_id
  model_config_rec%dfi_fwdstop_minute = dfi_fwdstop_minute 
  RETURN
END SUBROUTINE nl_set_dfi_fwdstop_minute
SUBROUTINE nl_set_dfi_fwdstop_second ( id_id , dfi_fwdstop_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_fwdstop_second
  INTEGER id_id
  model_config_rec%dfi_fwdstop_second = dfi_fwdstop_second 
  RETURN
END SUBROUTINE nl_set_dfi_fwdstop_second
SUBROUTINE nl_set_dfi_bckstop_year ( id_id , dfi_bckstop_year )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_bckstop_year
  INTEGER id_id
  model_config_rec%dfi_bckstop_year = dfi_bckstop_year 
  RETURN
END SUBROUTINE nl_set_dfi_bckstop_year
SUBROUTINE nl_set_dfi_bckstop_month ( id_id , dfi_bckstop_month )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_bckstop_month
  INTEGER id_id
  model_config_rec%dfi_bckstop_month = dfi_bckstop_month 
  RETURN
END SUBROUTINE nl_set_dfi_bckstop_month
SUBROUTINE nl_set_dfi_bckstop_day ( id_id , dfi_bckstop_day )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_bckstop_day
  INTEGER id_id
  model_config_rec%dfi_bckstop_day = dfi_bckstop_day 
  RETURN
END SUBROUTINE nl_set_dfi_bckstop_day
SUBROUTINE nl_set_dfi_bckstop_hour ( id_id , dfi_bckstop_hour )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_bckstop_hour
  INTEGER id_id
  model_config_rec%dfi_bckstop_hour = dfi_bckstop_hour 
  RETURN
END SUBROUTINE nl_set_dfi_bckstop_hour
SUBROUTINE nl_set_dfi_bckstop_minute ( id_id , dfi_bckstop_minute )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_bckstop_minute
  INTEGER id_id
  model_config_rec%dfi_bckstop_minute = dfi_bckstop_minute 
  RETURN
END SUBROUTINE nl_set_dfi_bckstop_minute
SUBROUTINE nl_set_dfi_bckstop_second ( id_id , dfi_bckstop_second )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_bckstop_second
  INTEGER id_id
  model_config_rec%dfi_bckstop_second = dfi_bckstop_second 
  RETURN
END SUBROUTINE nl_set_dfi_bckstop_second
SUBROUTINE nl_set_time_step ( id_id , time_step )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: time_step
  INTEGER id_id
  model_config_rec%time_step = time_step 
  RETURN
END SUBROUTINE nl_set_time_step
SUBROUTINE nl_set_time_step_fract_num ( id_id , time_step_fract_num )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: time_step_fract_num
  INTEGER id_id
  model_config_rec%time_step_fract_num = time_step_fract_num 
  RETURN
END SUBROUTINE nl_set_time_step_fract_num
SUBROUTINE nl_set_time_step_fract_den ( id_id , time_step_fract_den )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: time_step_fract_den
  INTEGER id_id
  model_config_rec%time_step_fract_den = time_step_fract_den 
  RETURN
END SUBROUTINE nl_set_time_step_fract_den
SUBROUTINE nl_set_time_step_dfi ( id_id , time_step_dfi )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: time_step_dfi
  INTEGER id_id
  model_config_rec%time_step_dfi = time_step_dfi 
  RETURN
END SUBROUTINE nl_set_time_step_dfi
SUBROUTINE nl_set_max_dom ( id_id , max_dom )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: max_dom
  INTEGER id_id
  model_config_rec%max_dom = max_dom 
  RETURN
END SUBROUTINE nl_set_max_dom
SUBROUTINE nl_set_s_we ( id_id , s_we )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: s_we
  INTEGER id_id
  model_config_rec%s_we(id_id) = s_we
  RETURN
END SUBROUTINE nl_set_s_we
SUBROUTINE nl_set_e_we ( id_id , e_we )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: e_we
  INTEGER id_id
  model_config_rec%e_we(id_id) = e_we
  RETURN
END SUBROUTINE nl_set_e_we
SUBROUTINE nl_set_s_sn ( id_id , s_sn )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: s_sn
  INTEGER id_id
  model_config_rec%s_sn(id_id) = s_sn
  RETURN
END SUBROUTINE nl_set_s_sn
SUBROUTINE nl_set_e_sn ( id_id , e_sn )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: e_sn
  INTEGER id_id
  model_config_rec%e_sn(id_id) = e_sn
  RETURN
END SUBROUTINE nl_set_e_sn
SUBROUTINE nl_set_s_vert ( id_id , s_vert )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: s_vert
  INTEGER id_id
  model_config_rec%s_vert(id_id) = s_vert
  RETURN
END SUBROUTINE nl_set_s_vert
SUBROUTINE nl_set_e_vert ( id_id , e_vert )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: e_vert
  INTEGER id_id
  model_config_rec%e_vert(id_id) = e_vert
  RETURN
END SUBROUTINE nl_set_e_vert
SUBROUTINE nl_set_num_metgrid_soil_levels ( id_id , num_metgrid_soil_levels )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_metgrid_soil_levels
  INTEGER id_id
  model_config_rec%num_metgrid_soil_levels = num_metgrid_soil_levels 
  RETURN
END SUBROUTINE nl_set_num_metgrid_soil_levels
SUBROUTINE nl_set_dx ( id_id , dx )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: dx
  INTEGER id_id
  model_config_rec%dx(id_id) = dx
  RETURN
END SUBROUTINE nl_set_dx
SUBROUTINE nl_set_dy ( id_id , dy )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: dy
  INTEGER id_id
  model_config_rec%dy(id_id) = dy
  RETURN
END SUBROUTINE nl_set_dy
SUBROUTINE nl_set_grid_id ( id_id , grid_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: grid_id
  INTEGER id_id
  model_config_rec%grid_id(id_id) = grid_id
  RETURN
END SUBROUTINE nl_set_grid_id
SUBROUTINE nl_set_grid_allowed ( id_id , grid_allowed )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: grid_allowed
  INTEGER id_id
  model_config_rec%grid_allowed(id_id) = grid_allowed
  RETURN
END SUBROUTINE nl_set_grid_allowed
SUBROUTINE nl_set_parent_id ( id_id , parent_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: parent_id
  INTEGER id_id
  model_config_rec%parent_id(id_id) = parent_id
  RETURN
END SUBROUTINE nl_set_parent_id
SUBROUTINE nl_set_i_parent_start ( id_id , i_parent_start )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: i_parent_start
  INTEGER id_id
  model_config_rec%i_parent_start(id_id) = i_parent_start
  RETURN
END SUBROUTINE nl_set_i_parent_start
SUBROUTINE nl_set_j_parent_start ( id_id , j_parent_start )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: j_parent_start
  INTEGER id_id
  model_config_rec%j_parent_start(id_id) = j_parent_start
  RETURN
END SUBROUTINE nl_set_j_parent_start
SUBROUTINE nl_set_parent_grid_ratio ( id_id , parent_grid_ratio )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: parent_grid_ratio
  INTEGER id_id
  model_config_rec%parent_grid_ratio(id_id) = parent_grid_ratio
  RETURN
END SUBROUTINE nl_set_parent_grid_ratio
SUBROUTINE nl_set_parent_time_step_ratio ( id_id , parent_time_step_ratio )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: parent_time_step_ratio
  INTEGER id_id
  model_config_rec%parent_time_step_ratio(id_id) = parent_time_step_ratio
  RETURN
END SUBROUTINE nl_set_parent_time_step_ratio
SUBROUTINE nl_set_feedback ( id_id , feedback )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: feedback
  INTEGER id_id
  model_config_rec%feedback = feedback 
  RETURN
END SUBROUTINE nl_set_feedback
SUBROUTINE nl_set_smooth_option ( id_id , smooth_option )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: smooth_option
  INTEGER id_id
  model_config_rec%smooth_option = smooth_option 
  RETURN
END SUBROUTINE nl_set_smooth_option
SUBROUTINE nl_set_ztop ( id_id , ztop )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: ztop
  INTEGER id_id
  model_config_rec%ztop(id_id) = ztop
  RETURN
END SUBROUTINE nl_set_ztop
SUBROUTINE nl_set_moad_grid_ratio ( id_id , moad_grid_ratio )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: moad_grid_ratio
  INTEGER id_id
  model_config_rec%moad_grid_ratio(id_id) = moad_grid_ratio
  RETURN
END SUBROUTINE nl_set_moad_grid_ratio
SUBROUTINE nl_set_moad_time_step_ratio ( id_id , moad_time_step_ratio )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: moad_time_step_ratio
  INTEGER id_id
  model_config_rec%moad_time_step_ratio(id_id) = moad_time_step_ratio
  RETURN
END SUBROUTINE nl_set_moad_time_step_ratio
SUBROUTINE nl_set_shw ( id_id , shw )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: shw
  INTEGER id_id
  model_config_rec%shw(id_id) = shw
  RETURN
END SUBROUTINE nl_set_shw
SUBROUTINE nl_set_tile_sz_x ( id_id , tile_sz_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: tile_sz_x
  INTEGER id_id
  model_config_rec%tile_sz_x = tile_sz_x 
  RETURN
END SUBROUTINE nl_set_tile_sz_x
SUBROUTINE nl_set_tile_sz_y ( id_id , tile_sz_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: tile_sz_y
  INTEGER id_id
  model_config_rec%tile_sz_y = tile_sz_y 
  RETURN
END SUBROUTINE nl_set_tile_sz_y
SUBROUTINE nl_set_numtiles ( id_id , numtiles )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: numtiles
  INTEGER id_id
  model_config_rec%numtiles = numtiles 
  RETURN
END SUBROUTINE nl_set_numtiles
SUBROUTINE nl_set_numtiles_inc ( id_id , numtiles_inc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: numtiles_inc
  INTEGER id_id
  model_config_rec%numtiles_inc = numtiles_inc 
  RETURN
END SUBROUTINE nl_set_numtiles_inc
SUBROUTINE nl_set_numtiles_x ( id_id , numtiles_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: numtiles_x
  INTEGER id_id
  model_config_rec%numtiles_x = numtiles_x 
  RETURN
END SUBROUTINE nl_set_numtiles_x
SUBROUTINE nl_set_numtiles_y ( id_id , numtiles_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: numtiles_y
  INTEGER id_id
  model_config_rec%numtiles_y = numtiles_y 
  RETURN
END SUBROUTINE nl_set_numtiles_y
SUBROUTINE nl_set_tile_strategy ( id_id , tile_strategy )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: tile_strategy
  INTEGER id_id
  model_config_rec%tile_strategy = tile_strategy 
  RETURN
END SUBROUTINE nl_set_tile_strategy
SUBROUTINE nl_set_nproc_x ( id_id , nproc_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: nproc_x
  INTEGER id_id
  model_config_rec%nproc_x = nproc_x 
  RETURN
END SUBROUTINE nl_set_nproc_x
SUBROUTINE nl_set_nproc_y ( id_id , nproc_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: nproc_y
  INTEGER id_id
  model_config_rec%nproc_y = nproc_y 
  RETURN
END SUBROUTINE nl_set_nproc_y
SUBROUTINE nl_set_irand ( id_id , irand )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: irand
  INTEGER id_id
  model_config_rec%irand = irand 
  RETURN
END SUBROUTINE nl_set_irand
SUBROUTINE nl_set_dt ( id_id , dt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: dt
  INTEGER id_id
  model_config_rec%dt(id_id) = dt
  RETURN
END SUBROUTINE nl_set_dt
SUBROUTINE nl_set_ts_buf_size ( id_id , ts_buf_size )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ts_buf_size
  INTEGER id_id
  model_config_rec%ts_buf_size = ts_buf_size 
  RETURN
END SUBROUTINE nl_set_ts_buf_size
SUBROUTINE nl_set_max_ts_locs ( id_id , max_ts_locs )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: max_ts_locs
  INTEGER id_id
  model_config_rec%max_ts_locs = max_ts_locs 
  RETURN
END SUBROUTINE nl_set_max_ts_locs
SUBROUTINE nl_set_num_moves ( id_id , num_moves )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_moves
  INTEGER id_id
  model_config_rec%num_moves = num_moves 
  RETURN
END SUBROUTINE nl_set_num_moves
SUBROUTINE nl_set_vortex_interval ( id_id , vortex_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: vortex_interval
  INTEGER id_id
  model_config_rec%vortex_interval(id_id) = vortex_interval
  RETURN
END SUBROUTINE nl_set_vortex_interval
SUBROUTINE nl_set_corral_dist ( id_id , corral_dist )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: corral_dist
  INTEGER id_id
  model_config_rec%corral_dist(id_id) = corral_dist
  RETURN
END SUBROUTINE nl_set_corral_dist
SUBROUTINE nl_set_move_id ( id_id , move_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: move_id
  INTEGER id_id
  model_config_rec%move_id(id_id) = move_id
  RETURN
END SUBROUTINE nl_set_move_id
SUBROUTINE nl_set_move_interval ( id_id , move_interval )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: move_interval
  INTEGER id_id
  model_config_rec%move_interval(id_id) = move_interval
  RETURN
END SUBROUTINE nl_set_move_interval
SUBROUTINE nl_set_move_cd_x ( id_id , move_cd_x )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: move_cd_x
  INTEGER id_id
  model_config_rec%move_cd_x(id_id) = move_cd_x
  RETURN
END SUBROUTINE nl_set_move_cd_x
SUBROUTINE nl_set_move_cd_y ( id_id , move_cd_y )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: move_cd_y
  INTEGER id_id
  model_config_rec%move_cd_y(id_id) = move_cd_y
  RETURN
END SUBROUTINE nl_set_move_cd_y
SUBROUTINE nl_set_swap_x ( id_id , swap_x )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: swap_x
  INTEGER id_id
  model_config_rec%swap_x(id_id) = swap_x
  RETURN
END SUBROUTINE nl_set_swap_x
SUBROUTINE nl_set_swap_y ( id_id , swap_y )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: swap_y
  INTEGER id_id
  model_config_rec%swap_y(id_id) = swap_y
  RETURN
END SUBROUTINE nl_set_swap_y
SUBROUTINE nl_set_cycle_x ( id_id , cycle_x )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: cycle_x
  INTEGER id_id
  model_config_rec%cycle_x(id_id) = cycle_x
  RETURN
END SUBROUTINE nl_set_cycle_x
SUBROUTINE nl_set_cycle_y ( id_id , cycle_y )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: cycle_y
  INTEGER id_id
  model_config_rec%cycle_y(id_id) = cycle_y
  RETURN
END SUBROUTINE nl_set_cycle_y
SUBROUTINE nl_set_reorder_mesh ( id_id , reorder_mesh )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: reorder_mesh
  INTEGER id_id
  model_config_rec%reorder_mesh = reorder_mesh 
  RETURN
END SUBROUTINE nl_set_reorder_mesh
SUBROUTINE nl_set_perturb_input ( id_id , perturb_input )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: perturb_input
  INTEGER id_id
  model_config_rec%perturb_input = perturb_input 
  RETURN
END SUBROUTINE nl_set_perturb_input
SUBROUTINE nl_set_eta_levels ( id_id , eta_levels )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: eta_levels
  INTEGER id_id
  model_config_rec%eta_levels(id_id) = eta_levels
  RETURN
END SUBROUTINE nl_set_eta_levels
SUBROUTINE nl_set_ptsgm ( id_id , ptsgm )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: ptsgm
  INTEGER id_id
  model_config_rec%ptsgm = ptsgm 
  RETURN
END SUBROUTINE nl_set_ptsgm
SUBROUTINE nl_set_num_metgrid_levels ( id_id , num_metgrid_levels )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_metgrid_levels
  INTEGER id_id
  model_config_rec%num_metgrid_levels = num_metgrid_levels 
  RETURN
END SUBROUTINE nl_set_num_metgrid_levels
SUBROUTINE nl_set_p_top_requested ( id_id , p_top_requested )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: p_top_requested
  INTEGER id_id
  model_config_rec%p_top_requested = p_top_requested 
  RETURN
END SUBROUTINE nl_set_p_top_requested
SUBROUTINE nl_set_use_prep_hybrid ( id_id , use_prep_hybrid )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: use_prep_hybrid
  INTEGER id_id
  model_config_rec%use_prep_hybrid = use_prep_hybrid 
  RETURN
END SUBROUTINE nl_set_use_prep_hybrid
SUBROUTINE nl_set_force_read_thompson ( id_id , force_read_thompson )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: force_read_thompson
  INTEGER id_id
  model_config_rec%force_read_thompson = force_read_thompson 
  RETURN
END SUBROUTINE nl_set_force_read_thompson
SUBROUTINE nl_set_write_thompson_tables ( id_id , write_thompson_tables )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: write_thompson_tables
  INTEGER id_id
  model_config_rec%write_thompson_tables = write_thompson_tables 
  RETURN
END SUBROUTINE nl_set_write_thompson_tables
SUBROUTINE nl_set_mp_physics ( id_id , mp_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: mp_physics
  INTEGER id_id
  model_config_rec%mp_physics(id_id) = mp_physics
  RETURN
END SUBROUTINE nl_set_mp_physics
SUBROUTINE nl_set_mommix ( id_id , mommix )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: mommix
  INTEGER id_id
  model_config_rec%mommix(id_id) = mommix
  RETURN
END SUBROUTINE nl_set_mommix
SUBROUTINE nl_set_disheat ( id_id , disheat )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: disheat
  INTEGER id_id
  model_config_rec%disheat(id_id) = disheat
  RETURN
END SUBROUTINE nl_set_disheat
SUBROUTINE nl_set_do_radar_ref ( id_id , do_radar_ref )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: do_radar_ref
  INTEGER id_id
  model_config_rec%do_radar_ref = do_radar_ref 
  RETURN
END SUBROUTINE nl_set_do_radar_ref
SUBROUTINE nl_set_compute_radar_ref ( id_id , compute_radar_ref )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: compute_radar_ref
  INTEGER id_id
  model_config_rec%compute_radar_ref = compute_radar_ref 
  RETURN
END SUBROUTINE nl_set_compute_radar_ref
SUBROUTINE nl_set_ra_lw_physics ( id_id , ra_lw_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ra_lw_physics
  INTEGER id_id
  model_config_rec%ra_lw_physics(id_id) = ra_lw_physics
  RETURN
END SUBROUTINE nl_set_ra_lw_physics
SUBROUTINE nl_set_ra_sw_physics ( id_id , ra_sw_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ra_sw_physics
  INTEGER id_id
  model_config_rec%ra_sw_physics(id_id) = ra_sw_physics
  RETURN
END SUBROUTINE nl_set_ra_sw_physics
SUBROUTINE nl_set_radt ( id_id , radt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: radt
  INTEGER id_id
  model_config_rec%radt(id_id) = radt
  RETURN
END SUBROUTINE nl_set_radt
SUBROUTINE nl_set_sf_sfclay_physics ( id_id , sf_sfclay_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sf_sfclay_physics
  INTEGER id_id
  model_config_rec%sf_sfclay_physics(id_id) = sf_sfclay_physics
  RETURN
END SUBROUTINE nl_set_sf_sfclay_physics
SUBROUTINE nl_set_sf_surface_physics ( id_id , sf_surface_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sf_surface_physics
  INTEGER id_id
  model_config_rec%sf_surface_physics(id_id) = sf_surface_physics
  RETURN
END SUBROUTINE nl_set_sf_surface_physics
SUBROUTINE nl_set_bl_pbl_physics ( id_id , bl_pbl_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: bl_pbl_physics
  INTEGER id_id
  model_config_rec%bl_pbl_physics(id_id) = bl_pbl_physics
  RETURN
END SUBROUTINE nl_set_bl_pbl_physics
SUBROUTINE nl_set_ysu_topdown_pblmix ( id_id , ysu_topdown_pblmix )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ysu_topdown_pblmix
  INTEGER id_id
  model_config_rec%ysu_topdown_pblmix(id_id) = ysu_topdown_pblmix
  RETURN
END SUBROUTINE nl_set_ysu_topdown_pblmix
SUBROUTINE nl_set_shinhong_tke_diag ( id_id , shinhong_tke_diag )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: shinhong_tke_diag
  INTEGER id_id
  model_config_rec%shinhong_tke_diag(id_id) = shinhong_tke_diag
  RETURN
END SUBROUTINE nl_set_shinhong_tke_diag
SUBROUTINE nl_set_windfarm_opt ( id_id , windfarm_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: windfarm_opt
  INTEGER id_id
  model_config_rec%windfarm_opt(id_id) = windfarm_opt
  RETURN
END SUBROUTINE nl_set_windfarm_opt
SUBROUTINE nl_set_windfarm_ij ( id_id , windfarm_ij )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: windfarm_ij
  INTEGER id_id
  model_config_rec%windfarm_ij = windfarm_ij 
  RETURN
END SUBROUTINE nl_set_windfarm_ij
SUBROUTINE nl_set_mfshconv ( id_id , mfshconv )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: mfshconv
  INTEGER id_id
  model_config_rec%mfshconv(id_id) = mfshconv
  RETURN
END SUBROUTINE nl_set_mfshconv
SUBROUTINE nl_set_bldt ( id_id , bldt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: bldt
  INTEGER id_id
  model_config_rec%bldt(id_id) = bldt
  RETURN
END SUBROUTINE nl_set_bldt
SUBROUTINE nl_set_cu_physics ( id_id , cu_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: cu_physics
  INTEGER id_id
  model_config_rec%cu_physics(id_id) = cu_physics
  RETURN
END SUBROUTINE nl_set_cu_physics
SUBROUTINE nl_set_shcu_physics ( id_id , shcu_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: shcu_physics
  INTEGER id_id
  model_config_rec%shcu_physics(id_id) = shcu_physics
  RETURN
END SUBROUTINE nl_set_shcu_physics
SUBROUTINE nl_set_cu_diag ( id_id , cu_diag )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: cu_diag
  INTEGER id_id
  model_config_rec%cu_diag(id_id) = cu_diag
  RETURN
END SUBROUTINE nl_set_cu_diag
SUBROUTINE nl_set_gfs_alpha ( id_id , gfs_alpha )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: gfs_alpha
  INTEGER id_id
  model_config_rec%gfs_alpha(id_id) = gfs_alpha
  RETURN
END SUBROUTINE nl_set_gfs_alpha
SUBROUTINE nl_set_cudt ( id_id , cudt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: cudt
  INTEGER id_id
  model_config_rec%cudt(id_id) = cudt
  RETURN
END SUBROUTINE nl_set_cudt
SUBROUTINE nl_set_gsmdt ( id_id , gsmdt )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: gsmdt
  INTEGER id_id
  model_config_rec%gsmdt(id_id) = gsmdt
  RETURN
END SUBROUTINE nl_set_gsmdt
SUBROUTINE nl_set_isfflx ( id_id , isfflx )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: isfflx
  INTEGER id_id
  model_config_rec%isfflx = isfflx 
  RETURN
END SUBROUTINE nl_set_isfflx
SUBROUTINE nl_set_ideal_xland ( id_id , ideal_xland )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ideal_xland
  INTEGER id_id
  model_config_rec%ideal_xland = ideal_xland 
  RETURN
END SUBROUTINE nl_set_ideal_xland
#endif
#if (NNN == 7)
SUBROUTINE nl_set_ifsnow ( id_id , ifsnow )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ifsnow
  INTEGER id_id
  model_config_rec%ifsnow = ifsnow 
  RETURN
END SUBROUTINE nl_set_ifsnow
SUBROUTINE nl_set_icloud ( id_id , icloud )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: icloud
  INTEGER id_id
  model_config_rec%icloud = icloud 
  RETURN
END SUBROUTINE nl_set_icloud
SUBROUTINE nl_set_swrad_scat ( id_id , swrad_scat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: swrad_scat
  INTEGER id_id
  model_config_rec%swrad_scat = swrad_scat 
  RETURN
END SUBROUTINE nl_set_swrad_scat
SUBROUTINE nl_set_surface_input_source ( id_id , surface_input_source )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: surface_input_source
  INTEGER id_id
  model_config_rec%surface_input_source = surface_input_source 
  RETURN
END SUBROUTINE nl_set_surface_input_source
SUBROUTINE nl_set_num_soil_layers ( id_id , num_soil_layers )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_soil_layers
  INTEGER id_id
  model_config_rec%num_soil_layers = num_soil_layers 
  RETURN
END SUBROUTINE nl_set_num_soil_layers
SUBROUTINE nl_set_num_urban_layers ( id_id , num_urban_layers )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_urban_layers
  INTEGER id_id
  model_config_rec%num_urban_layers = num_urban_layers 
  RETURN
END SUBROUTINE nl_set_num_urban_layers
SUBROUTINE nl_set_sf_surface_mosaic ( id_id , sf_surface_mosaic )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sf_surface_mosaic
  INTEGER id_id
  model_config_rec%sf_surface_mosaic = sf_surface_mosaic 
  RETURN
END SUBROUTINE nl_set_sf_surface_mosaic
SUBROUTINE nl_set_mosaic_cat ( id_id , mosaic_cat )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: mosaic_cat
  INTEGER id_id
  model_config_rec%mosaic_cat = mosaic_cat 
  RETURN
END SUBROUTINE nl_set_mosaic_cat
SUBROUTINE nl_set_mosaic_cat_soil ( id_id , mosaic_cat_soil )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: mosaic_cat_soil
  INTEGER id_id
  model_config_rec%mosaic_cat_soil = mosaic_cat_soil 
  RETURN
END SUBROUTINE nl_set_mosaic_cat_soil
SUBROUTINE nl_set_num_urban_hi ( id_id , num_urban_hi )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_urban_hi
  INTEGER id_id
  model_config_rec%num_urban_hi = num_urban_hi 
  RETURN
END SUBROUTINE nl_set_num_urban_hi
SUBROUTINE nl_set_mosaic_lu ( id_id , mosaic_lu )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: mosaic_lu
  INTEGER id_id
  model_config_rec%mosaic_lu = mosaic_lu 
  RETURN
END SUBROUTINE nl_set_mosaic_lu
SUBROUTINE nl_set_mosaic_soil ( id_id , mosaic_soil )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: mosaic_soil
  INTEGER id_id
  model_config_rec%mosaic_soil = mosaic_soil 
  RETURN
END SUBROUTINE nl_set_mosaic_soil
SUBROUTINE nl_set_maxiens ( id_id , maxiens )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: maxiens
  INTEGER id_id
  model_config_rec%maxiens = maxiens 
  RETURN
END SUBROUTINE nl_set_maxiens
SUBROUTINE nl_set_maxens ( id_id , maxens )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: maxens
  INTEGER id_id
  model_config_rec%maxens = maxens 
  RETURN
END SUBROUTINE nl_set_maxens
SUBROUTINE nl_set_maxens2 ( id_id , maxens2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: maxens2
  INTEGER id_id
  model_config_rec%maxens2 = maxens2 
  RETURN
END SUBROUTINE nl_set_maxens2
SUBROUTINE nl_set_maxens3 ( id_id , maxens3 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: maxens3
  INTEGER id_id
  model_config_rec%maxens3 = maxens3 
  RETURN
END SUBROUTINE nl_set_maxens3
SUBROUTINE nl_set_ensdim ( id_id , ensdim )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ensdim
  INTEGER id_id
  model_config_rec%ensdim = ensdim 
  RETURN
END SUBROUTINE nl_set_ensdim
SUBROUTINE nl_set_chem_opt ( id_id , chem_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: chem_opt
  INTEGER id_id
  model_config_rec%chem_opt(id_id) = chem_opt
  RETURN
END SUBROUTINE nl_set_chem_opt
SUBROUTINE nl_set_num_land_cat ( id_id , num_land_cat )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_land_cat
  INTEGER id_id
  model_config_rec%num_land_cat = num_land_cat 
  RETURN
END SUBROUTINE nl_set_num_land_cat
SUBROUTINE nl_set_num_soil_cat ( id_id , num_soil_cat )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_soil_cat
  INTEGER id_id
  model_config_rec%num_soil_cat = num_soil_cat 
  RETURN
END SUBROUTINE nl_set_num_soil_cat
SUBROUTINE nl_set_topo_wind ( id_id , topo_wind )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: topo_wind
  INTEGER id_id
  model_config_rec%topo_wind(id_id) = topo_wind
  RETURN
END SUBROUTINE nl_set_topo_wind
SUBROUTINE nl_set_mp_zero_out ( id_id , mp_zero_out )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: mp_zero_out
  INTEGER id_id
  model_config_rec%mp_zero_out = mp_zero_out 
  RETURN
END SUBROUTINE nl_set_mp_zero_out
SUBROUTINE nl_set_mp_zero_out_thresh ( id_id , mp_zero_out_thresh )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: mp_zero_out_thresh
  INTEGER id_id
  model_config_rec%mp_zero_out_thresh = mp_zero_out_thresh 
  RETURN
END SUBROUTINE nl_set_mp_zero_out_thresh
SUBROUTINE nl_set_seaice_threshold ( id_id , seaice_threshold )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: seaice_threshold
  INTEGER id_id
  model_config_rec%seaice_threshold = seaice_threshold 
  RETURN
END SUBROUTINE nl_set_seaice_threshold
SUBROUTINE nl_set_fractional_seaice ( id_id , fractional_seaice )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: fractional_seaice
  INTEGER id_id
  model_config_rec%fractional_seaice = fractional_seaice 
  RETURN
END SUBROUTINE nl_set_fractional_seaice
SUBROUTINE nl_set_seaice_albedo_opt ( id_id , seaice_albedo_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: seaice_albedo_opt
  INTEGER id_id
  model_config_rec%seaice_albedo_opt = seaice_albedo_opt 
  RETURN
END SUBROUTINE nl_set_seaice_albedo_opt
SUBROUTINE nl_set_seaice_albedo_default ( id_id , seaice_albedo_default )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: seaice_albedo_default
  INTEGER id_id
  model_config_rec%seaice_albedo_default = seaice_albedo_default 
  RETURN
END SUBROUTINE nl_set_seaice_albedo_default
SUBROUTINE nl_set_seaice_snowdepth_opt ( id_id , seaice_snowdepth_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: seaice_snowdepth_opt
  INTEGER id_id
  model_config_rec%seaice_snowdepth_opt = seaice_snowdepth_opt 
  RETURN
END SUBROUTINE nl_set_seaice_snowdepth_opt
SUBROUTINE nl_set_seaice_snowdepth_max ( id_id , seaice_snowdepth_max )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: seaice_snowdepth_max
  INTEGER id_id
  model_config_rec%seaice_snowdepth_max = seaice_snowdepth_max 
  RETURN
END SUBROUTINE nl_set_seaice_snowdepth_max
SUBROUTINE nl_set_seaice_snowdepth_min ( id_id , seaice_snowdepth_min )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: seaice_snowdepth_min
  INTEGER id_id
  model_config_rec%seaice_snowdepth_min = seaice_snowdepth_min 
  RETURN
END SUBROUTINE nl_set_seaice_snowdepth_min
SUBROUTINE nl_set_seaice_thickness_opt ( id_id , seaice_thickness_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: seaice_thickness_opt
  INTEGER id_id
  model_config_rec%seaice_thickness_opt = seaice_thickness_opt 
  RETURN
END SUBROUTINE nl_set_seaice_thickness_opt
SUBROUTINE nl_set_seaice_thickness_default ( id_id , seaice_thickness_default )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: seaice_thickness_default
  INTEGER id_id
  model_config_rec%seaice_thickness_default = seaice_thickness_default 
  RETURN
END SUBROUTINE nl_set_seaice_thickness_default
SUBROUTINE nl_set_tice2tsk_if2cold ( id_id , tice2tsk_if2cold )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: tice2tsk_if2cold
  INTEGER id_id
  model_config_rec%tice2tsk_if2cold = tice2tsk_if2cold 
  RETURN
END SUBROUTINE nl_set_tice2tsk_if2cold
SUBROUTINE nl_set_sst_update ( id_id , sst_update )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sst_update
  INTEGER id_id
  model_config_rec%sst_update = sst_update 
  RETURN
END SUBROUTINE nl_set_sst_update
SUBROUTINE nl_set_sf_urban_physics ( id_id , sf_urban_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sf_urban_physics
  INTEGER id_id
  model_config_rec%sf_urban_physics(id_id) = sf_urban_physics
  RETURN
END SUBROUTINE nl_set_sf_urban_physics
SUBROUTINE nl_set_usemonalb ( id_id , usemonalb )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: usemonalb
  INTEGER id_id
  model_config_rec%usemonalb = usemonalb 
  RETURN
END SUBROUTINE nl_set_usemonalb
SUBROUTINE nl_set_rdmaxalb ( id_id , rdmaxalb )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: rdmaxalb
  INTEGER id_id
  model_config_rec%rdmaxalb = rdmaxalb 
  RETURN
END SUBROUTINE nl_set_rdmaxalb
SUBROUTINE nl_set_rdlai2d ( id_id , rdlai2d )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: rdlai2d
  INTEGER id_id
  model_config_rec%rdlai2d = rdlai2d 
  RETURN
END SUBROUTINE nl_set_rdlai2d
SUBROUTINE nl_set_ua_phys ( id_id , ua_phys )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: ua_phys
  INTEGER id_id
  model_config_rec%ua_phys = ua_phys 
  RETURN
END SUBROUTINE nl_set_ua_phys
SUBROUTINE nl_set_gwd_opt ( id_id , gwd_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: gwd_opt
  INTEGER id_id
  model_config_rec%gwd_opt(id_id) = gwd_opt
  RETURN
END SUBROUTINE nl_set_gwd_opt
SUBROUTINE nl_set_iz0tlnd ( id_id , iz0tlnd )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: iz0tlnd
  INTEGER id_id
  model_config_rec%iz0tlnd = iz0tlnd 
  RETURN
END SUBROUTINE nl_set_iz0tlnd
SUBROUTINE nl_set_sas_pgcon ( id_id , sas_pgcon )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: sas_pgcon
  INTEGER id_id
  model_config_rec%sas_pgcon(id_id) = sas_pgcon
  RETURN
END SUBROUTINE nl_set_sas_pgcon
SUBROUTINE nl_set_sas_shal_pgcon ( id_id , sas_shal_pgcon )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: sas_shal_pgcon
  INTEGER id_id
  model_config_rec%sas_shal_pgcon(id_id) = sas_shal_pgcon
  RETURN
END SUBROUTINE nl_set_sas_shal_pgcon
SUBROUTINE nl_set_sas_shal_conv ( id_id , sas_shal_conv )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sas_shal_conv
  INTEGER id_id
  model_config_rec%sas_shal_conv(id_id) = sas_shal_conv
  RETURN
END SUBROUTINE nl_set_sas_shal_conv
SUBROUTINE nl_set_sas_mass_flux ( id_id , sas_mass_flux )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: sas_mass_flux
  INTEGER id_id
  model_config_rec%sas_mass_flux(id_id) = sas_mass_flux
  RETURN
END SUBROUTINE nl_set_sas_mass_flux
SUBROUTINE nl_set_var_ric ( id_id , var_ric )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: var_ric
  INTEGER id_id
  model_config_rec%var_ric = var_ric 
  RETURN
END SUBROUTINE nl_set_var_ric
SUBROUTINE nl_set_coef_ric_l ( id_id , coef_ric_l )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: coef_ric_l
  INTEGER id_id
  model_config_rec%coef_ric_l = coef_ric_l 
  RETURN
END SUBROUTINE nl_set_coef_ric_l
SUBROUTINE nl_set_coef_ric_s ( id_id , coef_ric_s )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: coef_ric_s
  INTEGER id_id
  model_config_rec%coef_ric_s = coef_ric_s 
  RETURN
END SUBROUTINE nl_set_coef_ric_s
SUBROUTINE nl_set_random_seed ( id_id , random_seed )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: random_seed
  INTEGER id_id
  model_config_rec%random_seed(id_id) = random_seed
  RETURN
END SUBROUTINE nl_set_random_seed
SUBROUTINE nl_set_icoef_sf ( id_id , icoef_sf )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: icoef_sf
  INTEGER id_id
  model_config_rec%icoef_sf(id_id) = icoef_sf
  RETURN
END SUBROUTINE nl_set_icoef_sf
SUBROUTINE nl_set_lcurr_sf ( id_id , lcurr_sf )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: lcurr_sf
  INTEGER id_id
  model_config_rec%lcurr_sf(id_id) = lcurr_sf
  RETURN
END SUBROUTINE nl_set_lcurr_sf
SUBROUTINE nl_set_ens_random_seed ( id_id , ens_random_seed )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ens_random_seed
  INTEGER id_id
  model_config_rec%ens_random_seed(id_id) = ens_random_seed
  RETURN
END SUBROUTINE nl_set_ens_random_seed
SUBROUTINE nl_set_pert_sas ( id_id , pert_sas )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: pert_sas
  INTEGER id_id
  model_config_rec%pert_sas = pert_sas 
  RETURN
END SUBROUTINE nl_set_pert_sas
SUBROUTINE nl_set_pert_pbl ( id_id , pert_pbl )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: pert_pbl
  INTEGER id_id
  model_config_rec%pert_pbl = pert_pbl 
  RETURN
END SUBROUTINE nl_set_pert_pbl
SUBROUTINE nl_set_ens_sasamp ( id_id , ens_sasamp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: ens_sasamp
  INTEGER id_id
  model_config_rec%ens_sasamp(id_id) = ens_sasamp
  RETURN
END SUBROUTINE nl_set_ens_sasamp
SUBROUTINE nl_set_ens_pblamp ( id_id , ens_pblamp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: ens_pblamp
  INTEGER id_id
  model_config_rec%ens_pblamp(id_id) = ens_pblamp
  RETURN
END SUBROUTINE nl_set_ens_pblamp
SUBROUTINE nl_set_idtad ( id_id , idtad )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: idtad
  INTEGER id_id
  model_config_rec%idtad(id_id) = idtad
  RETURN
END SUBROUTINE nl_set_idtad
SUBROUTINE nl_set_nsoil ( id_id , nsoil )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: nsoil
  INTEGER id_id
  model_config_rec%nsoil(id_id) = nsoil
  RETURN
END SUBROUTINE nl_set_nsoil
SUBROUTINE nl_set_nphs ( id_id , nphs )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: nphs
  INTEGER id_id
  model_config_rec%nphs(id_id) = nphs
  RETURN
END SUBROUTINE nl_set_nphs
SUBROUTINE nl_set_ncnvc ( id_id , ncnvc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ncnvc
  INTEGER id_id
  model_config_rec%ncnvc(id_id) = ncnvc
  RETURN
END SUBROUTINE nl_set_ncnvc
SUBROUTINE nl_set_nrand ( id_id , nrand )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: nrand
  INTEGER id_id
  model_config_rec%nrand(id_id) = nrand
  RETURN
END SUBROUTINE nl_set_nrand
SUBROUTINE nl_set_nrads ( id_id , nrads )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: nrads
  INTEGER id_id
  model_config_rec%nrads(id_id) = nrads
  RETURN
END SUBROUTINE nl_set_nrads
SUBROUTINE nl_set_nradl ( id_id , nradl )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: nradl
  INTEGER id_id
  model_config_rec%nradl(id_id) = nradl
  RETURN
END SUBROUTINE nl_set_nradl
SUBROUTINE nl_set_tprec ( id_id , tprec )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: tprec
  INTEGER id_id
  model_config_rec%tprec(id_id) = tprec
  RETURN
END SUBROUTINE nl_set_tprec
SUBROUTINE nl_set_theat ( id_id , theat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: theat
  INTEGER id_id
  model_config_rec%theat(id_id) = theat
  RETURN
END SUBROUTINE nl_set_theat
SUBROUTINE nl_set_tclod ( id_id , tclod )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: tclod
  INTEGER id_id
  model_config_rec%tclod(id_id) = tclod
  RETURN
END SUBROUTINE nl_set_tclod
SUBROUTINE nl_set_trdsw ( id_id , trdsw )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: trdsw
  INTEGER id_id
  model_config_rec%trdsw(id_id) = trdsw
  RETURN
END SUBROUTINE nl_set_trdsw
SUBROUTINE nl_set_trdlw ( id_id , trdlw )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: trdlw
  INTEGER id_id
  model_config_rec%trdlw(id_id) = trdlw
  RETURN
END SUBROUTINE nl_set_trdlw
SUBROUTINE nl_set_tsrfc ( id_id , tsrfc )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: tsrfc
  INTEGER id_id
  model_config_rec%tsrfc(id_id) = tsrfc
  RETURN
END SUBROUTINE nl_set_tsrfc
SUBROUTINE nl_set_pcpflg ( id_id , pcpflg )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: pcpflg
  INTEGER id_id
  model_config_rec%pcpflg(id_id) = pcpflg
  RETURN
END SUBROUTINE nl_set_pcpflg
SUBROUTINE nl_set_sigma ( id_id , sigma )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sigma
  INTEGER id_id
  model_config_rec%sigma(id_id) = sigma
  RETURN
END SUBROUTINE nl_set_sigma
SUBROUTINE nl_set_sfenth ( id_id , sfenth )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: sfenth
  INTEGER id_id
  model_config_rec%sfenth(id_id) = sfenth
  RETURN
END SUBROUTINE nl_set_sfenth
SUBROUTINE nl_set_co2tf ( id_id , co2tf )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: co2tf
  INTEGER id_id
  model_config_rec%co2tf = co2tf 
  RETURN
END SUBROUTINE nl_set_co2tf
SUBROUTINE nl_set_ra_call_offset ( id_id , ra_call_offset )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: ra_call_offset
  INTEGER id_id
  model_config_rec%ra_call_offset = ra_call_offset 
  RETURN
END SUBROUTINE nl_set_ra_call_offset
SUBROUTINE nl_set_cam_abs_freq_s ( id_id , cam_abs_freq_s )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: cam_abs_freq_s
  INTEGER id_id
  model_config_rec%cam_abs_freq_s = cam_abs_freq_s 
  RETURN
END SUBROUTINE nl_set_cam_abs_freq_s
SUBROUTINE nl_set_levsiz ( id_id , levsiz )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: levsiz
  INTEGER id_id
  model_config_rec%levsiz = levsiz 
  RETURN
END SUBROUTINE nl_set_levsiz
SUBROUTINE nl_set_paerlev ( id_id , paerlev )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: paerlev
  INTEGER id_id
  model_config_rec%paerlev = paerlev 
  RETURN
END SUBROUTINE nl_set_paerlev
SUBROUTINE nl_set_cam_abs_dim1 ( id_id , cam_abs_dim1 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: cam_abs_dim1
  INTEGER id_id
  model_config_rec%cam_abs_dim1 = cam_abs_dim1 
  RETURN
END SUBROUTINE nl_set_cam_abs_dim1
SUBROUTINE nl_set_cam_abs_dim2 ( id_id , cam_abs_dim2 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: cam_abs_dim2
  INTEGER id_id
  model_config_rec%cam_abs_dim2 = cam_abs_dim2 
  RETURN
END SUBROUTINE nl_set_cam_abs_dim2
SUBROUTINE nl_set_no_src_types ( id_id , no_src_types )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: no_src_types
  INTEGER id_id
  model_config_rec%no_src_types = no_src_types 
  RETURN
END SUBROUTINE nl_set_no_src_types
SUBROUTINE nl_set_alevsiz ( id_id , alevsiz )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: alevsiz
  INTEGER id_id
  model_config_rec%alevsiz = alevsiz 
  RETURN
END SUBROUTINE nl_set_alevsiz
SUBROUTINE nl_set_o3input ( id_id , o3input )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: o3input
  INTEGER id_id
  model_config_rec%o3input = o3input 
  RETURN
END SUBROUTINE nl_set_o3input
SUBROUTINE nl_set_aer_opt ( id_id , aer_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: aer_opt
  INTEGER id_id
  model_config_rec%aer_opt = aer_opt 
  RETURN
END SUBROUTINE nl_set_aer_opt
SUBROUTINE nl_set_cu_rad_feedback ( id_id , cu_rad_feedback )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: cu_rad_feedback
  INTEGER id_id
  model_config_rec%cu_rad_feedback(id_id) = cu_rad_feedback
  RETURN
END SUBROUTINE nl_set_cu_rad_feedback
SUBROUTINE nl_set_icloud_cu ( id_id , icloud_cu )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: icloud_cu
  INTEGER id_id
  model_config_rec%icloud_cu = icloud_cu 
  RETURN
END SUBROUTINE nl_set_icloud_cu
SUBROUTINE nl_set_h_diff ( id_id , h_diff )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: h_diff
  INTEGER id_id
  model_config_rec%h_diff(id_id) = h_diff
  RETURN
END SUBROUTINE nl_set_h_diff
SUBROUTINE nl_set_movemin ( id_id , movemin )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: movemin
  INTEGER id_id
  model_config_rec%movemin(id_id) = movemin
  RETURN
END SUBROUTINE nl_set_movemin
SUBROUTINE nl_set_num_snso_layers ( id_id , num_snso_layers )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_snso_layers
  INTEGER id_id
  model_config_rec%num_snso_layers = num_snso_layers 
  RETURN
END SUBROUTINE nl_set_num_snso_layers
SUBROUTINE nl_set_num_snow_layers ( id_id , num_snow_layers )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: num_snow_layers
  INTEGER id_id
  model_config_rec%num_snow_layers = num_snow_layers 
  RETURN
END SUBROUTINE nl_set_num_snow_layers
SUBROUTINE nl_set_use_aero_icbc ( id_id , use_aero_icbc )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: use_aero_icbc
  INTEGER id_id
  model_config_rec%use_aero_icbc = use_aero_icbc 
  RETURN
END SUBROUTINE nl_set_use_aero_icbc
SUBROUTINE nl_set_ccn_conc ( id_id , ccn_conc )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: ccn_conc
  INTEGER id_id
  model_config_rec%ccn_conc = ccn_conc 
  RETURN
END SUBROUTINE nl_set_ccn_conc
SUBROUTINE nl_set_hail_opt ( id_id , hail_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: hail_opt
  INTEGER id_id
  model_config_rec%hail_opt = hail_opt 
  RETURN
END SUBROUTINE nl_set_hail_opt
SUBROUTINE nl_set_sf_lake_physics ( id_id , sf_lake_physics )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: sf_lake_physics
  INTEGER id_id
  model_config_rec%sf_lake_physics(id_id) = sf_lake_physics
  RETURN
END SUBROUTINE nl_set_sf_lake_physics
SUBROUTINE nl_set_dyn_opt ( id_id , dyn_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dyn_opt
  INTEGER id_id
  model_config_rec%dyn_opt = dyn_opt 
  RETURN
END SUBROUTINE nl_set_dyn_opt
SUBROUTINE nl_set_rk_ord ( id_id , rk_ord )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: rk_ord
  INTEGER id_id
  model_config_rec%rk_ord = rk_ord 
  RETURN
END SUBROUTINE nl_set_rk_ord
SUBROUTINE nl_set_w_damping ( id_id , w_damping )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: w_damping
  INTEGER id_id
  model_config_rec%w_damping = w_damping 
  RETURN
END SUBROUTINE nl_set_w_damping
SUBROUTINE nl_set_diff_opt ( id_id , diff_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: diff_opt
  INTEGER id_id
  model_config_rec%diff_opt(id_id) = diff_opt
  RETURN
END SUBROUTINE nl_set_diff_opt
SUBROUTINE nl_set_km_opt ( id_id , km_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: km_opt
  INTEGER id_id
  model_config_rec%km_opt(id_id) = km_opt
  RETURN
END SUBROUTINE nl_set_km_opt
SUBROUTINE nl_set_damp_opt ( id_id , damp_opt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: damp_opt
  INTEGER id_id
  model_config_rec%damp_opt = damp_opt 
  RETURN
END SUBROUTINE nl_set_damp_opt
SUBROUTINE nl_set_zdamp ( id_id , zdamp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: zdamp
  INTEGER id_id
  model_config_rec%zdamp(id_id) = zdamp
  RETURN
END SUBROUTINE nl_set_zdamp
SUBROUTINE nl_set_base_pres ( id_id , base_pres )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: base_pres
  INTEGER id_id
  model_config_rec%base_pres = base_pres 
  RETURN
END SUBROUTINE nl_set_base_pres
SUBROUTINE nl_set_base_temp ( id_id , base_temp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: base_temp
  INTEGER id_id
  model_config_rec%base_temp = base_temp 
  RETURN
END SUBROUTINE nl_set_base_temp
SUBROUTINE nl_set_base_lapse ( id_id , base_lapse )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: base_lapse
  INTEGER id_id
  model_config_rec%base_lapse = base_lapse 
  RETURN
END SUBROUTINE nl_set_base_lapse
SUBROUTINE nl_set_iso_temp ( id_id , iso_temp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: iso_temp
  INTEGER id_id
  model_config_rec%iso_temp = iso_temp 
  RETURN
END SUBROUTINE nl_set_iso_temp
SUBROUTINE nl_set_dampcoef ( id_id , dampcoef )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: dampcoef
  INTEGER id_id
  model_config_rec%dampcoef(id_id) = dampcoef
  RETURN
END SUBROUTINE nl_set_dampcoef
SUBROUTINE nl_set_khdif ( id_id , khdif )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: khdif
  INTEGER id_id
  model_config_rec%khdif(id_id) = khdif
  RETURN
END SUBROUTINE nl_set_khdif
SUBROUTINE nl_set_kvdif ( id_id , kvdif )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: kvdif
  INTEGER id_id
  model_config_rec%kvdif(id_id) = kvdif
  RETURN
END SUBROUTINE nl_set_kvdif
SUBROUTINE nl_set_c_s ( id_id , c_s )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: c_s
  INTEGER id_id
  model_config_rec%c_s(id_id) = c_s
  RETURN
END SUBROUTINE nl_set_c_s
SUBROUTINE nl_set_c_k ( id_id , c_k )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: c_k
  INTEGER id_id
  model_config_rec%c_k(id_id) = c_k
  RETURN
END SUBROUTINE nl_set_c_k
SUBROUTINE nl_set_smdiv ( id_id , smdiv )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: smdiv
  INTEGER id_id
  model_config_rec%smdiv(id_id) = smdiv
  RETURN
END SUBROUTINE nl_set_smdiv
SUBROUTINE nl_set_emdiv ( id_id , emdiv )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: emdiv
  INTEGER id_id
  model_config_rec%emdiv(id_id) = emdiv
  RETURN
END SUBROUTINE nl_set_emdiv
SUBROUTINE nl_set_epssm ( id_id , epssm )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: epssm
  INTEGER id_id
  model_config_rec%epssm(id_id) = epssm
  RETURN
END SUBROUTINE nl_set_epssm
SUBROUTINE nl_set_non_hydrostatic ( id_id , non_hydrostatic )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: non_hydrostatic
  INTEGER id_id
  model_config_rec%non_hydrostatic(id_id) = non_hydrostatic
  RETURN
END SUBROUTINE nl_set_non_hydrostatic
SUBROUTINE nl_set_time_step_sound ( id_id , time_step_sound )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: time_step_sound
  INTEGER id_id
  model_config_rec%time_step_sound(id_id) = time_step_sound
  RETURN
END SUBROUTINE nl_set_time_step_sound
SUBROUTINE nl_set_h_mom_adv_order ( id_id , h_mom_adv_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: h_mom_adv_order
  INTEGER id_id
  model_config_rec%h_mom_adv_order(id_id) = h_mom_adv_order
  RETURN
END SUBROUTINE nl_set_h_mom_adv_order
SUBROUTINE nl_set_v_mom_adv_order ( id_id , v_mom_adv_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: v_mom_adv_order
  INTEGER id_id
  model_config_rec%v_mom_adv_order(id_id) = v_mom_adv_order
  RETURN
END SUBROUTINE nl_set_v_mom_adv_order
SUBROUTINE nl_set_h_sca_adv_order ( id_id , h_sca_adv_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: h_sca_adv_order
  INTEGER id_id
  model_config_rec%h_sca_adv_order(id_id) = h_sca_adv_order
  RETURN
END SUBROUTINE nl_set_h_sca_adv_order
SUBROUTINE nl_set_v_sca_adv_order ( id_id , v_sca_adv_order )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: v_sca_adv_order
  INTEGER id_id
  model_config_rec%v_sca_adv_order(id_id) = v_sca_adv_order
  RETURN
END SUBROUTINE nl_set_v_sca_adv_order
SUBROUTINE nl_set_top_radiation ( id_id , top_radiation )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: top_radiation
  INTEGER id_id
  model_config_rec%top_radiation(id_id) = top_radiation
  RETURN
END SUBROUTINE nl_set_top_radiation
SUBROUTINE nl_set_tke_upper_bound ( id_id , tke_upper_bound )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: tke_upper_bound
  INTEGER id_id
  model_config_rec%tke_upper_bound(id_id) = tke_upper_bound
  RETURN
END SUBROUTINE nl_set_tke_upper_bound
SUBROUTINE nl_set_tke_drag_coefficient ( id_id , tke_drag_coefficient )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: tke_drag_coefficient
  INTEGER id_id
  model_config_rec%tke_drag_coefficient(id_id) = tke_drag_coefficient
  RETURN
END SUBROUTINE nl_set_tke_drag_coefficient
SUBROUTINE nl_set_tke_heat_flux ( id_id , tke_heat_flux )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: tke_heat_flux
  INTEGER id_id
  model_config_rec%tke_heat_flux(id_id) = tke_heat_flux
  RETURN
END SUBROUTINE nl_set_tke_heat_flux
SUBROUTINE nl_set_pert_coriolis ( id_id , pert_coriolis )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: pert_coriolis
  INTEGER id_id
  model_config_rec%pert_coriolis(id_id) = pert_coriolis
  RETURN
END SUBROUTINE nl_set_pert_coriolis
SUBROUTINE nl_set_euler_adv ( id_id , euler_adv )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: euler_adv
  INTEGER id_id
  model_config_rec%euler_adv = euler_adv 
  RETURN
END SUBROUTINE nl_set_euler_adv
SUBROUTINE nl_set_idtadt ( id_id , idtadt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: idtadt
  INTEGER id_id
  model_config_rec%idtadt = idtadt 
  RETURN
END SUBROUTINE nl_set_idtadt
SUBROUTINE nl_set_idtadc ( id_id , idtadc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: idtadc
  INTEGER id_id
  model_config_rec%idtadc = idtadc 
  RETURN
END SUBROUTINE nl_set_idtadc
SUBROUTINE nl_set_codamp ( id_id , codamp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: codamp
  INTEGER id_id
  model_config_rec%codamp(id_id) = codamp
  RETURN
END SUBROUTINE nl_set_codamp
SUBROUTINE nl_set_coac ( id_id , coac )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: coac
  INTEGER id_id
  model_config_rec%coac(id_id) = coac
  RETURN
END SUBROUTINE nl_set_coac
SUBROUTINE nl_set_slophc ( id_id , slophc )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: slophc
  INTEGER id_id
  model_config_rec%slophc(id_id) = slophc
  RETURN
END SUBROUTINE nl_set_slophc
SUBROUTINE nl_set_wp ( id_id , wp )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: wp
  INTEGER id_id
  model_config_rec%wp(id_id) = wp
  RETURN
END SUBROUTINE nl_set_wp
SUBROUTINE nl_set_terrain_smoothing ( id_id , terrain_smoothing )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: terrain_smoothing
  INTEGER id_id
  model_config_rec%terrain_smoothing = terrain_smoothing 
  RETURN
END SUBROUTINE nl_set_terrain_smoothing
SUBROUTINE nl_set_spec_bdy_width ( id_id , spec_bdy_width )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: spec_bdy_width
  INTEGER id_id
  model_config_rec%spec_bdy_width = spec_bdy_width 
  RETURN
END SUBROUTINE nl_set_spec_bdy_width
SUBROUTINE nl_set_spec_zone ( id_id , spec_zone )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: spec_zone
  INTEGER id_id
  model_config_rec%spec_zone = spec_zone 
  RETURN
END SUBROUTINE nl_set_spec_zone
SUBROUTINE nl_set_relax_zone ( id_id , relax_zone )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: relax_zone
  INTEGER id_id
  model_config_rec%relax_zone = relax_zone 
  RETURN
END SUBROUTINE nl_set_relax_zone
SUBROUTINE nl_set_specified ( id_id , specified )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: specified
  INTEGER id_id
  model_config_rec%specified(id_id) = specified
  RETURN
END SUBROUTINE nl_set_specified
SUBROUTINE nl_set_periodic_x ( id_id , periodic_x )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: periodic_x
  INTEGER id_id
  model_config_rec%periodic_x(id_id) = periodic_x
  RETURN
END SUBROUTINE nl_set_periodic_x
SUBROUTINE nl_set_symmetric_xs ( id_id , symmetric_xs )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: symmetric_xs
  INTEGER id_id
  model_config_rec%symmetric_xs(id_id) = symmetric_xs
  RETURN
END SUBROUTINE nl_set_symmetric_xs
SUBROUTINE nl_set_symmetric_xe ( id_id , symmetric_xe )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: symmetric_xe
  INTEGER id_id
  model_config_rec%symmetric_xe(id_id) = symmetric_xe
  RETURN
END SUBROUTINE nl_set_symmetric_xe
SUBROUTINE nl_set_open_xs ( id_id , open_xs )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: open_xs
  INTEGER id_id
  model_config_rec%open_xs(id_id) = open_xs
  RETURN
END SUBROUTINE nl_set_open_xs
SUBROUTINE nl_set_open_xe ( id_id , open_xe )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: open_xe
  INTEGER id_id
  model_config_rec%open_xe(id_id) = open_xe
  RETURN
END SUBROUTINE nl_set_open_xe
SUBROUTINE nl_set_periodic_y ( id_id , periodic_y )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: periodic_y
  INTEGER id_id
  model_config_rec%periodic_y(id_id) = periodic_y
  RETURN
END SUBROUTINE nl_set_periodic_y
SUBROUTINE nl_set_symmetric_ys ( id_id , symmetric_ys )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: symmetric_ys
  INTEGER id_id
  model_config_rec%symmetric_ys(id_id) = symmetric_ys
  RETURN
END SUBROUTINE nl_set_symmetric_ys
SUBROUTINE nl_set_symmetric_ye ( id_id , symmetric_ye )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: symmetric_ye
  INTEGER id_id
  model_config_rec%symmetric_ye(id_id) = symmetric_ye
  RETURN
END SUBROUTINE nl_set_symmetric_ye
SUBROUTINE nl_set_open_ys ( id_id , open_ys )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: open_ys
  INTEGER id_id
  model_config_rec%open_ys(id_id) = open_ys
  RETURN
END SUBROUTINE nl_set_open_ys
SUBROUTINE nl_set_open_ye ( id_id , open_ye )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: open_ye
  INTEGER id_id
  model_config_rec%open_ye(id_id) = open_ye
  RETURN
END SUBROUTINE nl_set_open_ye
SUBROUTINE nl_set_polar ( id_id , polar )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: polar
  INTEGER id_id
  model_config_rec%polar(id_id) = polar
  RETURN
END SUBROUTINE nl_set_polar
SUBROUTINE nl_set_nested ( id_id , nested )
  USE module_configure, ONLY : model_config_rec 
  logical , INTENT(IN) :: nested
  INTEGER id_id
  model_config_rec%nested(id_id) = nested
  RETURN
END SUBROUTINE nl_set_nested
SUBROUTINE nl_set_real_data_init_type ( id_id , real_data_init_type )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: real_data_init_type
  INTEGER id_id
  model_config_rec%real_data_init_type = real_data_init_type 
  RETURN
END SUBROUTINE nl_set_real_data_init_type
SUBROUTINE nl_set_background_proc_id ( id_id , background_proc_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: background_proc_id
  INTEGER id_id
  model_config_rec%background_proc_id = background_proc_id 
  RETURN
END SUBROUTINE nl_set_background_proc_id
SUBROUTINE nl_set_forecast_proc_id ( id_id , forecast_proc_id )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: forecast_proc_id
  INTEGER id_id
  model_config_rec%forecast_proc_id = forecast_proc_id 
  RETURN
END SUBROUTINE nl_set_forecast_proc_id
SUBROUTINE nl_set_production_status ( id_id , production_status )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: production_status
  INTEGER id_id
  model_config_rec%production_status = production_status 
  RETURN
END SUBROUTINE nl_set_production_status
SUBROUTINE nl_set_compression ( id_id , compression )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: compression
  INTEGER id_id
  model_config_rec%compression = compression 
  RETURN
END SUBROUTINE nl_set_compression
SUBROUTINE nl_set_cen_lat ( id_id , cen_lat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: cen_lat
  INTEGER id_id
  model_config_rec%cen_lat(id_id) = cen_lat
  RETURN
END SUBROUTINE nl_set_cen_lat
SUBROUTINE nl_set_cen_lon ( id_id , cen_lon )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: cen_lon
  INTEGER id_id
  model_config_rec%cen_lon(id_id) = cen_lon
  RETURN
END SUBROUTINE nl_set_cen_lon
SUBROUTINE nl_set_truelat1 ( id_id , truelat1 )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: truelat1
  INTEGER id_id
  model_config_rec%truelat1(id_id) = truelat1
  RETURN
END SUBROUTINE nl_set_truelat1
SUBROUTINE nl_set_truelat2 ( id_id , truelat2 )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: truelat2
  INTEGER id_id
  model_config_rec%truelat2(id_id) = truelat2
  RETURN
END SUBROUTINE nl_set_truelat2
SUBROUTINE nl_set_moad_cen_lat ( id_id , moad_cen_lat )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: moad_cen_lat
  INTEGER id_id
  model_config_rec%moad_cen_lat(id_id) = moad_cen_lat
  RETURN
END SUBROUTINE nl_set_moad_cen_lat
SUBROUTINE nl_set_stand_lon ( id_id , stand_lon )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: stand_lon
  INTEGER id_id
  model_config_rec%stand_lon(id_id) = stand_lon
  RETURN
END SUBROUTINE nl_set_stand_lon
SUBROUTINE nl_set_flag_metgrid ( id_id , flag_metgrid )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_metgrid
  INTEGER id_id
  model_config_rec%flag_metgrid = flag_metgrid 
  RETURN
END SUBROUTINE nl_set_flag_metgrid
SUBROUTINE nl_set_flag_snow ( id_id , flag_snow )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_snow
  INTEGER id_id
  model_config_rec%flag_snow = flag_snow 
  RETURN
END SUBROUTINE nl_set_flag_snow
SUBROUTINE nl_set_flag_psfc ( id_id , flag_psfc )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_psfc
  INTEGER id_id
  model_config_rec%flag_psfc = flag_psfc 
  RETURN
END SUBROUTINE nl_set_flag_psfc
SUBROUTINE nl_set_flag_sm000010 ( id_id , flag_sm000010 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_sm000010
  INTEGER id_id
  model_config_rec%flag_sm000010 = flag_sm000010 
  RETURN
END SUBROUTINE nl_set_flag_sm000010
SUBROUTINE nl_set_flag_sm010040 ( id_id , flag_sm010040 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_sm010040
  INTEGER id_id
  model_config_rec%flag_sm010040 = flag_sm010040 
  RETURN
END SUBROUTINE nl_set_flag_sm010040
SUBROUTINE nl_set_flag_sm040100 ( id_id , flag_sm040100 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_sm040100
  INTEGER id_id
  model_config_rec%flag_sm040100 = flag_sm040100 
  RETURN
END SUBROUTINE nl_set_flag_sm040100
SUBROUTINE nl_set_flag_sm100200 ( id_id , flag_sm100200 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_sm100200
  INTEGER id_id
  model_config_rec%flag_sm100200 = flag_sm100200 
  RETURN
END SUBROUTINE nl_set_flag_sm100200
SUBROUTINE nl_set_flag_st000010 ( id_id , flag_st000010 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_st000010
  INTEGER id_id
  model_config_rec%flag_st000010 = flag_st000010 
  RETURN
END SUBROUTINE nl_set_flag_st000010
SUBROUTINE nl_set_flag_st010040 ( id_id , flag_st010040 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_st010040
  INTEGER id_id
  model_config_rec%flag_st010040 = flag_st010040 
  RETURN
END SUBROUTINE nl_set_flag_st010040
SUBROUTINE nl_set_flag_st040100 ( id_id , flag_st040100 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_st040100
  INTEGER id_id
  model_config_rec%flag_st040100 = flag_st040100 
  RETURN
END SUBROUTINE nl_set_flag_st040100
SUBROUTINE nl_set_flag_st100200 ( id_id , flag_st100200 )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_st100200
  INTEGER id_id
  model_config_rec%flag_st100200 = flag_st100200 
  RETURN
END SUBROUTINE nl_set_flag_st100200
SUBROUTINE nl_set_flag_slp ( id_id , flag_slp )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_slp
  INTEGER id_id
  model_config_rec%flag_slp = flag_slp 
  RETURN
END SUBROUTINE nl_set_flag_slp
SUBROUTINE nl_set_flag_soilhgt ( id_id , flag_soilhgt )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_soilhgt
  INTEGER id_id
  model_config_rec%flag_soilhgt = flag_soilhgt 
  RETURN
END SUBROUTINE nl_set_flag_soilhgt
SUBROUTINE nl_set_flag_mf_xy ( id_id , flag_mf_xy )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: flag_mf_xy
  INTEGER id_id
  model_config_rec%flag_mf_xy = flag_mf_xy 
  RETURN
END SUBROUTINE nl_set_flag_mf_xy
SUBROUTINE nl_set_bdyfrq ( id_id , bdyfrq )
  USE module_configure, ONLY : model_config_rec 
  real , INTENT(IN) :: bdyfrq
  INTEGER id_id
  model_config_rec%bdyfrq(id_id) = bdyfrq
  RETURN
END SUBROUTINE nl_set_bdyfrq
SUBROUTINE nl_set_mminlu ( id_id , mminlu )
  USE module_configure, ONLY : model_config_rec 
  character*256 , INTENT(IN) :: mminlu
  INTEGER id_id
  model_config_rec%mminlu(id_id) = mminlu
  RETURN
END SUBROUTINE nl_set_mminlu
SUBROUTINE nl_set_iswater ( id_id , iswater )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: iswater
  INTEGER id_id
  model_config_rec%iswater(id_id) = iswater
  RETURN
END SUBROUTINE nl_set_iswater
SUBROUTINE nl_set_islake ( id_id , islake )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: islake
  INTEGER id_id
  model_config_rec%islake(id_id) = islake
  RETURN
END SUBROUTINE nl_set_islake
SUBROUTINE nl_set_isice ( id_id , isice )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: isice
  INTEGER id_id
  model_config_rec%isice(id_id) = isice
  RETURN
END SUBROUTINE nl_set_isice
SUBROUTINE nl_set_isurban ( id_id , isurban )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: isurban
  INTEGER id_id
  model_config_rec%isurban(id_id) = isurban
  RETURN
END SUBROUTINE nl_set_isurban
SUBROUTINE nl_set_isoilwater ( id_id , isoilwater )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: isoilwater
  INTEGER id_id
  model_config_rec%isoilwater(id_id) = isoilwater
  RETURN
END SUBROUTINE nl_set_isoilwater
SUBROUTINE nl_set_map_proj ( id_id , map_proj )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: map_proj
  INTEGER id_id
  model_config_rec%map_proj(id_id) = map_proj
  RETURN
END SUBROUTINE nl_set_map_proj
SUBROUTINE nl_set_dfi_stage ( id_id , dfi_stage )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: dfi_stage
  INTEGER id_id
  model_config_rec%dfi_stage = dfi_stage 
  RETURN
END SUBROUTINE nl_set_dfi_stage
SUBROUTINE nl_set_mp_physics_dfi ( id_id , mp_physics_dfi )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: mp_physics_dfi
  INTEGER id_id
  model_config_rec%mp_physics_dfi(id_id) = mp_physics_dfi
  RETURN
END SUBROUTINE nl_set_mp_physics_dfi
SUBROUTINE nl_set_maxpatch ( id_id , maxpatch )
  USE module_configure, ONLY : model_config_rec 
  integer , INTENT(IN) :: maxpatch
  INTEGER id_id
  model_config_rec%maxpatch = maxpatch 
  RETURN
END SUBROUTINE nl_set_maxpatch
#endif
#endif
!ENDOFREGISTRYGENERATEDINCLUDE
