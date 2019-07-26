!STARTOFREGISTRYGENERATEDINCLUDE 'inc/state_struct.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
integer                                  :: lakeflag
integer                                  :: lake_depth_flag
real                                     :: lakedepth_default
real                                     :: lake_min_elev
integer                                  :: use_lakedepth
integer                                  :: halo_debug
integer                                  :: ntracers
real                                     :: dtc
real                                     :: guessdtc
integer                                  :: vortex_tracker
logical                                  :: update_interest
real                                     :: interest_rad_storm
real                                     :: interest_rad_parent
real                                     :: interest_rad_self
integer                                  :: interest_kids
integer                                  :: interest_self
integer                                  :: interest_storms
integer                                  :: swath_mode
logical                                  :: track_have_guess
real                                     :: track_guess_lat
real                                     :: track_guess_lon
real                                     :: tracker_edge_dist
integer                                  :: track_n_old
real                                     :: track_stderr_m1
real                                     :: track_stderr_m2
real                                     :: track_stderr_m3
integer                                  :: track_last_hour
real                                     :: tracker_fixlon
real                                     :: tracker_fixlat
integer                                  :: tracker_ifix
integer                                  :: tracker_jfix
real                                     :: tracker_rmw
real                                     :: tracker_pmin
real                                     :: tracker_vmax
logical                                  :: tracker_havefix
logical                                  :: tracker_gave_up
real                                     :: vt5searchrad
integer                                  :: pdyn_parent_age
integer                                  :: pdyn_smooth_age
real                                     :: nomove_freq_hr
integer                                  :: move_countdown
integer                                  :: num_old_fixes
real                                     :: vt4_radius
real                                     :: vt4_weightexp
real                                     :: vt4_pmax
real                                     :: vt4_noise_pmax
real                                     :: vt4_noise_pmin
real                                     :: vt4_noise_dpdr
integer                                  :: vt4_noise_iter
real                                     :: nomove_freq
integer                                  :: coral_x
integer                                  :: coral_y
integer                                  :: xloc_1
integer                                  :: xloc_2
integer                                  :: yloc_1
integer                                  :: yloc_2
logical                                  :: mvnest
integer                                  :: julyr_rst
integer                                  :: julday_rst
real                                     :: gmt_rst
integer                                  :: ntime0
logical                                  :: moved
integer                                  :: hifreq_lun
integer                                  :: outatcf_lun
real                                     :: tg_interval_start
real                                     :: tg_interval_end
real                                     :: tg_duration
integer                                  :: tg_want_reset
integer                                  :: tg_reset_stream
integer                                  :: tg_option
integer                                  :: ntornado
real                                     :: wbd0
real                                     :: sbd0
real                                     :: wbd0var
real                                     :: sbd0var
logical                                  :: analysis
logical                                  :: write_analysis
integer                                  :: io_form_auxinput2
logical                                  :: high_freq
integer                                  :: high_dom
integer                                  :: kzmax
integer                                  :: ntsd
integer                                  :: nstart_hour
real                                     :: hcoeff_tot
real                                     :: dy_nmm
real                                     :: cpgfv
real                                     :: en
real                                     :: ent
real                                     :: f4d
real                                     :: f4q
real                                     :: ef4t
logical                                  :: upstrm
real                                     :: dlmd
real                                     :: dphd
real                                     :: pdtop
real                                     :: pt
integer                                  :: has_reqc
integer                                  :: has_reqi
integer                                  :: has_reqs
integer                                  :: swint_opt
integer                                  :: aer_type
integer                                  :: aer_aod550_opt
integer                                  :: aer_angexp_opt
integer                                  :: aer_ssa_opt
integer                                  :: aer_asy_opt
real                                     :: aer_aod550_val
real                                     :: aer_angexp_val
real                                     :: aer_ssa_val
real                                     :: aer_asy_val
logical                                  :: micro_start
logical                                  :: hydro
integer                                  :: nphs0
integer                                  :: ncnvc0
integer                                  :: nprec
integer                                  :: nclod
integer                                  :: nheat
integer                                  :: nrdlw
integer                                  :: nrdsw
integer                                  :: nsrfc
real                                     :: avrain
real                                     :: avcnvc
real                                     :: aratim
real                                     :: acutim
real                                     :: ardlw
real                                     :: ardsw
real                                     :: asrfc
real                                     :: aphtim
integer                                  :: imicrogram
real                                     :: dtbc
integer                                  :: dveg
integer                                  :: opt_crs
integer                                  :: opt_btr
integer                                  :: opt_run
integer                                  :: opt_sfc
integer                                  :: opt_frz
integer                                  :: opt_inf
integer                                  :: opt_rad
integer                                  :: opt_alb
integer                                  :: opt_snf
integer                                  :: opt_tbot
integer                                  :: opt_stc
integer                                  :: wrf_hydro
integer                                  :: landuse_isice
integer                                  :: landuse_lucats
integer                                  :: landuse_luseas
integer                                  :: landuse_isn
integer                                  :: number_at_same_level
integer                                  :: itimestep
real                                     :: xtime
real                                     :: julian
integer                                  :: lbc_fid
logical                                  :: tiled
logical                                  :: patched
real                                     :: xi
real                                     :: xj
real                                     :: vc_i
real                                     :: vc_j
integer                                  :: run_days
integer                                  :: run_hours
integer                                  :: run_minutes
integer                                  :: run_seconds
integer                                  :: start_year
integer                                  :: start_month
integer                                  :: start_day
integer                                  :: start_hour
integer                                  :: start_minute
integer                                  :: start_second
integer                                  :: end_year
integer                                  :: end_month
integer                                  :: end_day
integer                                  :: end_hour
integer                                  :: end_minute
integer                                  :: end_second
integer                                  :: interval_seconds
logical                                  :: input_from_file
integer                                  :: fine_input_stream
character*256                               :: auxinput1_inname
integer                                  :: io_form_auxinput1
logical                                  :: override_restart_timers
integer                                  :: auxhist1_oid
character*256                               :: auxhist1_inname
character*256                               :: auxhist1_outname
integer                                  :: auxhist1_interval_y
integer                                  :: auxhist1_interval_d
integer                                  :: auxhist1_interval_h
integer                                  :: auxhist1_interval_m
integer                                  :: auxhist1_interval_s
integer                                  :: auxhist1_interval
integer                                  :: auxhist1_begin_y
integer                                  :: auxhist1_begin_d
integer                                  :: auxhist1_begin_h
integer                                  :: auxhist1_begin_m
integer                                  :: auxhist1_begin_s
integer                                  :: auxhist1_begin
integer                                  :: auxhist1_end_y
integer                                  :: auxhist1_end_d
integer                                  :: auxhist1_end_h
integer                                  :: auxhist1_end_m
integer                                  :: auxhist1_end_s
integer                                  :: auxhist1_end
integer                                  :: io_form_auxhist1
integer                                  :: frames_per_auxhist1
integer                                  :: auxhist2_oid
character*256                               :: auxhist2_inname
character*256                               :: auxhist2_outname
integer                                  :: auxhist2_interval_y
integer                                  :: auxhist2_interval_d
integer                                  :: auxhist2_interval_h
integer                                  :: auxhist2_interval_m
integer                                  :: auxhist2_interval_s
integer                                  :: auxhist2_interval
integer                                  :: auxhist2_begin_y
integer                                  :: auxhist2_begin_d
integer                                  :: auxhist2_begin_h
integer                                  :: auxhist2_begin_m
integer                                  :: auxhist2_begin_s
integer                                  :: auxhist2_begin
integer                                  :: auxhist2_end_y
integer                                  :: auxhist2_end_d
integer                                  :: auxhist2_end_h
integer                                  :: auxhist2_end_m
integer                                  :: auxhist2_end_s
integer                                  :: auxhist2_end
integer                                  :: io_form_auxhist2
integer                                  :: frames_per_auxhist2
integer                                  :: auxhist3_oid
character*256                               :: auxhist3_inname
character*256                               :: auxhist3_outname
integer                                  :: auxhist3_interval_y
integer                                  :: auxhist3_interval_d
integer                                  :: auxhist3_interval_h
integer                                  :: auxhist3_interval_m
integer                                  :: auxhist3_interval_s
integer                                  :: auxhist3_interval
integer                                  :: auxhist3_begin_y
integer                                  :: auxhist3_begin_d
integer                                  :: auxhist3_begin_h
integer                                  :: auxhist3_begin_m
integer                                  :: auxhist3_begin_s
integer                                  :: auxhist3_begin
integer                                  :: auxhist3_end_y
integer                                  :: auxhist3_end_d
integer                                  :: auxhist3_end_h
integer                                  :: auxhist3_end_m
integer                                  :: auxhist3_end_s
integer                                  :: auxhist3_end
integer                                  :: io_form_auxhist3
integer                                  :: frames_per_auxhist3
integer                                  :: auxhist4_oid
character*256                               :: auxhist4_inname
character*256                               :: auxhist4_outname
integer                                  :: auxhist4_interval_y
integer                                  :: auxhist4_interval_d
integer                                  :: auxhist4_interval_h
integer                                  :: auxhist4_interval_m
integer                                  :: auxhist4_interval_s
integer                                  :: auxhist4_interval
integer                                  :: auxhist4_begin_y
integer                                  :: auxhist4_begin_d
integer                                  :: auxhist4_begin_h
integer                                  :: auxhist4_begin_m
integer                                  :: auxhist4_begin_s
integer                                  :: auxhist4_begin
integer                                  :: auxhist4_end_y
integer                                  :: auxhist4_end_d
integer                                  :: auxhist4_end_h
integer                                  :: auxhist4_end_m
integer                                  :: auxhist4_end_s
integer                                  :: auxhist4_end
integer                                  :: io_form_auxhist4
integer                                  :: frames_per_auxhist4
integer                                  :: auxhist5_oid
character*256                               :: auxhist5_inname
character*256                               :: auxhist5_outname
integer                                  :: auxhist5_interval_y
integer                                  :: auxhist5_interval_d
integer                                  :: auxhist5_interval_h
integer                                  :: auxhist5_interval_m
integer                                  :: auxhist5_interval_s
integer                                  :: auxhist5_interval
integer                                  :: auxhist5_begin_y
integer                                  :: auxhist5_begin_d
integer                                  :: auxhist5_begin_h
integer                                  :: auxhist5_begin_m
integer                                  :: auxhist5_begin_s
integer                                  :: auxhist5_begin
integer                                  :: auxhist5_end_y
integer                                  :: auxhist5_end_d
integer                                  :: auxhist5_end_h
integer                                  :: auxhist5_end_m
integer                                  :: auxhist5_end_s
integer                                  :: auxhist5_end
integer                                  :: io_form_auxhist5
integer                                  :: frames_per_auxhist5
integer                                  :: auxhist6_oid
character*256                               :: auxhist6_inname
character*256                               :: auxhist6_outname
integer                                  :: auxhist6_interval_y
integer                                  :: auxhist6_interval_d
integer                                  :: auxhist6_interval_h
integer                                  :: auxhist6_interval_m
integer                                  :: auxhist6_interval_s
integer                                  :: auxhist6_interval
integer                                  :: auxhist6_begin_y
integer                                  :: auxhist6_begin_d
integer                                  :: auxhist6_begin_h
integer                                  :: auxhist6_begin_m
integer                                  :: auxhist6_begin_s
integer                                  :: auxhist6_begin
integer                                  :: auxhist6_end_y
integer                                  :: auxhist6_end_d
integer                                  :: auxhist6_end_h
integer                                  :: auxhist6_end_m
integer                                  :: auxhist6_end_s
integer                                  :: auxhist6_end
integer                                  :: io_form_auxhist6
integer                                  :: frames_per_auxhist6
integer                                  :: auxhist7_oid
character*256                               :: auxhist7_inname
character*256                               :: auxhist7_outname
integer                                  :: auxhist7_interval_y
integer                                  :: auxhist7_interval_d
integer                                  :: auxhist7_interval_h
integer                                  :: auxhist7_interval_m
integer                                  :: auxhist7_interval_s
integer                                  :: auxhist7_interval
integer                                  :: auxhist7_begin_y
integer                                  :: auxhist7_begin_d
integer                                  :: auxhist7_begin_h
integer                                  :: auxhist7_begin_m
integer                                  :: auxhist7_begin_s
integer                                  :: auxhist7_begin
integer                                  :: auxhist7_end_y
integer                                  :: auxhist7_end_d
integer                                  :: auxhist7_end_h
integer                                  :: auxhist7_end_m
integer                                  :: auxhist7_end_s
integer                                  :: auxhist7_end
integer                                  :: io_form_auxhist7
integer                                  :: frames_per_auxhist7
integer                                  :: auxhist8_oid
character*256                               :: auxhist8_inname
character*256                               :: auxhist8_outname
integer                                  :: auxhist8_interval_y
integer                                  :: auxhist8_interval_d
integer                                  :: auxhist8_interval_h
integer                                  :: auxhist8_interval_m
integer                                  :: auxhist8_interval_s
integer                                  :: auxhist8_interval
integer                                  :: auxhist8_begin_y
integer                                  :: auxhist8_begin_d
integer                                  :: auxhist8_begin_h
integer                                  :: auxhist8_begin_m
integer                                  :: auxhist8_begin_s
integer                                  :: auxhist8_begin
integer                                  :: auxhist8_end_y
integer                                  :: auxhist8_end_d
integer                                  :: auxhist8_end_h
integer                                  :: auxhist8_end_m
integer                                  :: auxhist8_end_s
integer                                  :: auxhist8_end
integer                                  :: io_form_auxhist8
integer                                  :: frames_per_auxhist8
integer                                  :: auxhist9_oid
character*256                               :: auxhist9_inname
character*256                               :: auxhist9_outname
integer                                  :: auxhist9_interval_y
integer                                  :: auxhist9_interval_d
integer                                  :: auxhist9_interval_h
integer                                  :: auxhist9_interval_m
integer                                  :: auxhist9_interval_s
integer                                  :: auxhist9_interval
integer                                  :: auxhist9_begin_y
integer                                  :: auxhist9_begin_d
integer                                  :: auxhist9_begin_h
integer                                  :: auxhist9_begin_m
integer                                  :: auxhist9_begin_s
integer                                  :: auxhist9_begin
integer                                  :: auxhist9_end_y
integer                                  :: auxhist9_end_d
integer                                  :: auxhist9_end_h
integer                                  :: auxhist9_end_m
integer                                  :: auxhist9_end_s
integer                                  :: auxhist9_end
integer                                  :: io_form_auxhist9
integer                                  :: frames_per_auxhist9
integer                                  :: auxhist10_oid
character*256                               :: auxhist10_inname
character*256                               :: auxhist10_outname
integer                                  :: auxhist10_interval_y
integer                                  :: auxhist10_interval_d
integer                                  :: auxhist10_interval_h
integer                                  :: auxhist10_interval_m
integer                                  :: auxhist10_interval_s
integer                                  :: auxhist10_interval
integer                                  :: auxhist10_begin_y
integer                                  :: auxhist10_begin_d
integer                                  :: auxhist10_begin_h
integer                                  :: auxhist10_begin_m
integer                                  :: auxhist10_begin_s
integer                                  :: auxhist10_begin
integer                                  :: auxhist10_end_y
integer                                  :: auxhist10_end_d
integer                                  :: auxhist10_end_h
integer                                  :: auxhist10_end_m
integer                                  :: auxhist10_end_s
integer                                  :: auxhist10_end
integer                                  :: io_form_auxhist10
integer                                  :: frames_per_auxhist10
integer                                  :: auxhist11_oid
character*256                               :: auxhist11_inname
character*256                               :: auxhist11_outname
integer                                  :: auxhist11_interval_y
integer                                  :: auxhist11_interval_d
integer                                  :: auxhist11_interval_h
integer                                  :: auxhist11_interval_m
integer                                  :: auxhist11_interval_s
integer                                  :: auxhist11_interval
integer                                  :: auxhist11_begin_y
integer                                  :: auxhist11_begin_d
integer                                  :: auxhist11_begin_h
integer                                  :: auxhist11_begin_m
integer                                  :: auxhist11_begin_s
integer                                  :: auxhist11_begin
integer                                  :: auxhist11_end_y
integer                                  :: auxhist11_end_d
integer                                  :: auxhist11_end_h
integer                                  :: auxhist11_end_m
integer                                  :: auxhist11_end_s
integer                                  :: auxhist11_end
integer                                  :: io_form_auxhist11
integer                                  :: frames_per_auxhist11
integer                                  :: auxhist12_oid
character*256                               :: auxhist12_inname
character*256                               :: auxhist12_outname
integer                                  :: auxhist12_interval_y
integer                                  :: auxhist12_interval_d
integer                                  :: auxhist12_interval_h
integer                                  :: auxhist12_interval_m
integer                                  :: auxhist12_interval_s
integer                                  :: auxhist12_interval
integer                                  :: auxhist12_begin_y
integer                                  :: auxhist12_begin_d
integer                                  :: auxhist12_begin_h
integer                                  :: auxhist12_begin_m
integer                                  :: auxhist12_begin_s
integer                                  :: auxhist12_begin
integer                                  :: auxhist12_end_y
integer                                  :: auxhist12_end_d
integer                                  :: auxhist12_end_h
integer                                  :: auxhist12_end_m
integer                                  :: auxhist12_end_s
integer                                  :: auxhist12_end
integer                                  :: io_form_auxhist12
integer                                  :: frames_per_auxhist12
integer                                  :: auxhist13_oid
character*256                               :: auxhist13_inname
character*256                               :: auxhist13_outname
integer                                  :: auxhist13_interval_y
integer                                  :: auxhist13_interval_d
integer                                  :: auxhist13_interval_h
integer                                  :: auxhist13_interval_m
integer                                  :: auxhist13_interval_s
integer                                  :: auxhist13_interval
integer                                  :: auxhist13_begin_y
integer                                  :: auxhist13_begin_d
integer                                  :: auxhist13_begin_h
integer                                  :: auxhist13_begin_m
integer                                  :: auxhist13_begin_s
integer                                  :: auxhist13_begin
integer                                  :: auxhist13_end_y
integer                                  :: auxhist13_end_d
integer                                  :: auxhist13_end_h
integer                                  :: auxhist13_end_m
integer                                  :: auxhist13_end_s
integer                                  :: auxhist13_end
integer                                  :: io_form_auxhist13
integer                                  :: frames_per_auxhist13
integer                                  :: auxhist14_oid
character*256                               :: auxhist14_inname
character*256                               :: auxhist14_outname
integer                                  :: auxhist14_interval_y
integer                                  :: auxhist14_interval_d
integer                                  :: auxhist14_interval_h
integer                                  :: auxhist14_interval_m
integer                                  :: auxhist14_interval_s
integer                                  :: auxhist14_interval
integer                                  :: auxhist14_begin_y
integer                                  :: auxhist14_begin_d
integer                                  :: auxhist14_begin_h
integer                                  :: auxhist14_begin_m
integer                                  :: auxhist14_begin_s
integer                                  :: auxhist14_begin
integer                                  :: auxhist14_end_y
integer                                  :: auxhist14_end_d
integer                                  :: auxhist14_end_h
integer                                  :: auxhist14_end_m
integer                                  :: auxhist14_end_s
integer                                  :: auxhist14_end
integer                                  :: io_form_auxhist14
integer                                  :: frames_per_auxhist14
integer                                  :: auxhist15_oid
character*256                               :: auxhist15_inname
character*256                               :: auxhist15_outname
integer                                  :: auxhist15_interval_y
integer                                  :: auxhist15_interval_d
integer                                  :: auxhist15_interval_h
integer                                  :: auxhist15_interval_m
integer                                  :: auxhist15_interval_s
integer                                  :: auxhist15_interval
integer                                  :: auxhist15_begin_y
integer                                  :: auxhist15_begin_d
integer                                  :: auxhist15_begin_h
integer                                  :: auxhist15_begin_m
integer                                  :: auxhist15_begin_s
integer                                  :: auxhist15_begin
integer                                  :: auxhist15_end_y
integer                                  :: auxhist15_end_d
integer                                  :: auxhist15_end_h
integer                                  :: auxhist15_end_m
integer                                  :: auxhist15_end_s
integer                                  :: auxhist15_end
integer                                  :: io_form_auxhist15
integer                                  :: frames_per_auxhist15
integer                                  :: auxhist16_oid
character*256                               :: auxhist16_inname
character*256                               :: auxhist16_outname
integer                                  :: auxhist16_interval_y
integer                                  :: auxhist16_interval_d
integer                                  :: auxhist16_interval_h
integer                                  :: auxhist16_interval_m
integer                                  :: auxhist16_interval_s
integer                                  :: auxhist16_interval
integer                                  :: auxhist16_begin_y
integer                                  :: auxhist16_begin_d
integer                                  :: auxhist16_begin_h
integer                                  :: auxhist16_begin_m
integer                                  :: auxhist16_begin_s
integer                                  :: auxhist16_begin
integer                                  :: auxhist16_end_y
integer                                  :: auxhist16_end_d
integer                                  :: auxhist16_end_h
integer                                  :: auxhist16_end_m
integer                                  :: auxhist16_end_s
integer                                  :: auxhist16_end
integer                                  :: io_form_auxhist16
integer                                  :: frames_per_auxhist16
integer                                  :: auxhist17_oid
character*256                               :: auxhist17_inname
character*256                               :: auxhist17_outname
integer                                  :: auxhist17_interval_y
integer                                  :: auxhist17_interval_d
integer                                  :: auxhist17_interval_h
integer                                  :: auxhist17_interval_m
integer                                  :: auxhist17_interval_s
integer                                  :: auxhist17_interval
integer                                  :: auxhist17_begin_y
integer                                  :: auxhist17_begin_d
integer                                  :: auxhist17_begin_h
integer                                  :: auxhist17_begin_m
integer                                  :: auxhist17_begin_s
integer                                  :: auxhist17_begin
integer                                  :: auxhist17_end_y
integer                                  :: auxhist17_end_d
integer                                  :: auxhist17_end_h
integer                                  :: auxhist17_end_m
integer                                  :: auxhist17_end_s
integer                                  :: auxhist17_end
integer                                  :: io_form_auxhist17
integer                                  :: frames_per_auxhist17
integer                                  :: auxhist18_oid
character*256                               :: auxhist18_inname
character*256                               :: auxhist18_outname
integer                                  :: auxhist18_interval_y
integer                                  :: auxhist18_interval_d
integer                                  :: auxhist18_interval_h
integer                                  :: auxhist18_interval_m
integer                                  :: auxhist18_interval_s
integer                                  :: auxhist18_interval
integer                                  :: auxhist18_begin_y
integer                                  :: auxhist18_begin_d
integer                                  :: auxhist18_begin_h
integer                                  :: auxhist18_begin_m
integer                                  :: auxhist18_begin_s
integer                                  :: auxhist18_begin
integer                                  :: auxhist18_end_y
integer                                  :: auxhist18_end_d
integer                                  :: auxhist18_end_h
integer                                  :: auxhist18_end_m
integer                                  :: auxhist18_end_s
integer                                  :: auxhist18_end
integer                                  :: io_form_auxhist18
integer                                  :: frames_per_auxhist18
integer                                  :: auxhist19_oid
character*256                               :: auxhist19_inname
character*256                               :: auxhist19_outname
integer                                  :: auxhist19_interval_y
integer                                  :: auxhist19_interval_d
integer                                  :: auxhist19_interval_h
integer                                  :: auxhist19_interval_m
integer                                  :: auxhist19_interval_s
integer                                  :: auxhist19_interval
integer                                  :: auxhist19_begin_y
integer                                  :: auxhist19_begin_d
integer                                  :: auxhist19_begin_h
integer                                  :: auxhist19_begin_m
integer                                  :: auxhist19_begin_s
integer                                  :: auxhist19_begin
integer                                  :: auxhist19_end_y
integer                                  :: auxhist19_end_d
integer                                  :: auxhist19_end_h
integer                                  :: auxhist19_end_m
integer                                  :: auxhist19_end_s
integer                                  :: auxhist19_end
integer                                  :: io_form_auxhist19
integer                                  :: frames_per_auxhist19
integer                                  :: auxhist20_oid
character*256                               :: auxhist20_inname
character*256                               :: auxhist20_outname
integer                                  :: auxhist20_interval_y
integer                                  :: auxhist20_interval_d
integer                                  :: auxhist20_interval_h
integer                                  :: auxhist20_interval_m
integer                                  :: auxhist20_interval_s
integer                                  :: auxhist20_interval
integer                                  :: auxhist20_begin_y
integer                                  :: auxhist20_begin_d
integer                                  :: auxhist20_begin_h
integer                                  :: auxhist20_begin_m
integer                                  :: auxhist20_begin_s
integer                                  :: auxhist20_begin
integer                                  :: auxhist20_end_y
integer                                  :: auxhist20_end_d
integer                                  :: auxhist20_end_h
integer                                  :: auxhist20_end_m
integer                                  :: auxhist20_end_s
integer                                  :: auxhist20_end
integer                                  :: io_form_auxhist20
integer                                  :: frames_per_auxhist20
integer                                  :: auxhist21_oid
character*256                               :: auxhist21_inname
character*256                               :: auxhist21_outname
integer                                  :: auxhist21_interval_y
integer                                  :: auxhist21_interval_d
integer                                  :: auxhist21_interval_h
integer                                  :: auxhist21_interval_m
integer                                  :: auxhist21_interval_s
integer                                  :: auxhist21_interval
integer                                  :: auxhist21_begin_y
integer                                  :: auxhist21_begin_d
integer                                  :: auxhist21_begin_h
integer                                  :: auxhist21_begin_m
integer                                  :: auxhist21_begin_s
integer                                  :: auxhist21_begin
integer                                  :: auxhist21_end_y
integer                                  :: auxhist21_end_d
integer                                  :: auxhist21_end_h
integer                                  :: auxhist21_end_m
integer                                  :: auxhist21_end_s
integer                                  :: auxhist21_end
integer                                  :: io_form_auxhist21
integer                                  :: frames_per_auxhist21
integer                                  :: auxhist22_oid
character*256                               :: auxhist22_inname
character*256                               :: auxhist22_outname
integer                                  :: auxhist22_interval_y
integer                                  :: auxhist22_interval_d
integer                                  :: auxhist22_interval_h
integer                                  :: auxhist22_interval_m
integer                                  :: auxhist22_interval_s
integer                                  :: auxhist22_interval
integer                                  :: auxhist22_begin_y
integer                                  :: auxhist22_begin_d
integer                                  :: auxhist22_begin_h
integer                                  :: auxhist22_begin_m
integer                                  :: auxhist22_begin_s
integer                                  :: auxhist22_begin
integer                                  :: auxhist22_end_y
integer                                  :: auxhist22_end_d
integer                                  :: auxhist22_end_h
integer                                  :: auxhist22_end_m
integer                                  :: auxhist22_end_s
integer                                  :: auxhist22_end
integer                                  :: io_form_auxhist22
integer                                  :: frames_per_auxhist22
integer                                  :: auxhist23_oid
character*256                               :: auxhist23_inname
character*256                               :: auxhist23_outname
integer                                  :: auxhist23_interval_y
integer                                  :: auxhist23_interval_d
integer                                  :: auxhist23_interval_h
integer                                  :: auxhist23_interval_m
integer                                  :: auxhist23_interval_s
integer                                  :: auxhist23_interval
integer                                  :: auxhist23_begin_y
integer                                  :: auxhist23_begin_d
integer                                  :: auxhist23_begin_h
integer                                  :: auxhist23_begin_m
integer                                  :: auxhist23_begin_s
integer                                  :: auxhist23_begin
integer                                  :: auxhist23_end_y
integer                                  :: auxhist23_end_d
integer                                  :: auxhist23_end_h
integer                                  :: auxhist23_end_m
integer                                  :: auxhist23_end_s
integer                                  :: auxhist23_end
integer                                  :: io_form_auxhist23
integer                                  :: frames_per_auxhist23
integer                                  :: auxhist24_oid
character*256                               :: auxhist24_inname
character*256                               :: auxhist24_outname
integer                                  :: auxhist24_interval_y
integer                                  :: auxhist24_interval_d
integer                                  :: auxhist24_interval_h
integer                                  :: auxhist24_interval_m
integer                                  :: auxhist24_interval_s
integer                                  :: auxhist24_interval
integer                                  :: auxhist24_begin_y
integer                                  :: auxhist24_begin_d
integer                                  :: auxhist24_begin_h
integer                                  :: auxhist24_begin_m
integer                                  :: auxhist24_begin_s
integer                                  :: auxhist24_begin
integer                                  :: auxhist24_end_y
integer                                  :: auxhist24_end_d
integer                                  :: auxhist24_end_h
integer                                  :: auxhist24_end_m
integer                                  :: auxhist24_end_s
integer                                  :: auxhist24_end
integer                                  :: io_form_auxhist24
integer                                  :: frames_per_auxhist24
integer                                  :: auxinput1_oid
character*256                               :: auxinput1_outname
integer                                  :: auxinput1_interval_y
integer                                  :: auxinput1_interval_d
integer                                  :: auxinput1_interval_h
integer                                  :: auxinput1_interval_m
integer                                  :: auxinput1_interval_s
integer                                  :: auxinput1_interval
integer                                  :: auxinput1_begin_y
integer                                  :: auxinput1_begin_d
integer                                  :: auxinput1_begin_h
integer                                  :: auxinput1_begin_m
integer                                  :: auxinput1_begin_s
integer                                  :: auxinput1_begin
integer                                  :: auxinput1_end_y
integer                                  :: auxinput1_end_d
integer                                  :: auxinput1_end_h
integer                                  :: auxinput1_end_m
integer                                  :: auxinput1_end_s
integer                                  :: auxinput1_end
integer                                  :: frames_per_auxinput1
integer                                  :: auxinput2_oid
character*256                               :: auxinput2_inname
character*256                               :: auxinput2_outname
integer                                  :: auxinput2_interval_y
integer                                  :: auxinput2_interval_d
integer                                  :: auxinput2_interval_h
integer                                  :: auxinput2_interval_m
integer                                  :: auxinput2_interval_s
integer                                  :: auxinput2_interval
integer                                  :: auxinput2_begin_y
integer                                  :: auxinput2_begin_d
integer                                  :: auxinput2_begin_h
integer                                  :: auxinput2_begin_m
integer                                  :: auxinput2_begin_s
integer                                  :: auxinput2_begin
integer                                  :: auxinput2_end_y
integer                                  :: auxinput2_end_d
integer                                  :: auxinput2_end_h
integer                                  :: auxinput2_end_m
integer                                  :: auxinput2_end_s
integer                                  :: auxinput2_end
integer                                  :: frames_per_auxinput2
integer                                  :: auxinput3_oid
character*256                               :: auxinput3_inname
character*256                               :: auxinput3_outname
integer                                  :: auxinput3_interval_y
integer                                  :: auxinput3_interval_d
integer                                  :: auxinput3_interval_h
integer                                  :: auxinput3_interval_m
integer                                  :: auxinput3_interval_s
integer                                  :: auxinput3_interval
integer                                  :: auxinput3_begin_y
integer                                  :: auxinput3_begin_d
integer                                  :: auxinput3_begin_h
integer                                  :: auxinput3_begin_m
integer                                  :: auxinput3_begin_s
integer                                  :: auxinput3_begin
integer                                  :: auxinput3_end_y
integer                                  :: auxinput3_end_d
integer                                  :: auxinput3_end_h
integer                                  :: auxinput3_end_m
integer                                  :: auxinput3_end_s
integer                                  :: auxinput3_end
integer                                  :: io_form_auxinput3
integer                                  :: frames_per_auxinput3
integer                                  :: auxinput4_oid
character*256                               :: auxinput4_inname
character*256                               :: auxinput4_outname
integer                                  :: auxinput4_interval_y
integer                                  :: auxinput4_interval_d
integer                                  :: auxinput4_interval_h
integer                                  :: auxinput4_interval_m
integer                                  :: auxinput4_interval_s
integer                                  :: auxinput4_interval
integer                                  :: auxinput4_begin_y
integer                                  :: auxinput4_begin_d
integer                                  :: auxinput4_begin_h
integer                                  :: auxinput4_begin_m
integer                                  :: auxinput4_begin_s
integer                                  :: auxinput4_begin
integer                                  :: auxinput4_end_y
integer                                  :: auxinput4_end_d
integer                                  :: auxinput4_end_h
integer                                  :: auxinput4_end_m
integer                                  :: auxinput4_end_s
integer                                  :: auxinput4_end
integer                                  :: io_form_auxinput4
integer                                  :: frames_per_auxinput4
integer                                  :: auxinput5_oid
character*256                               :: auxinput5_inname
character*256                               :: auxinput5_outname
integer                                  :: auxinput5_interval_y
integer                                  :: auxinput5_interval_d
integer                                  :: auxinput5_interval_h
integer                                  :: auxinput5_interval_m
integer                                  :: auxinput5_interval_s
integer                                  :: auxinput5_interval
integer                                  :: auxinput5_begin_y
integer                                  :: auxinput5_begin_d
integer                                  :: auxinput5_begin_h
integer                                  :: auxinput5_begin_m
integer                                  :: auxinput5_begin_s
integer                                  :: auxinput5_begin
integer                                  :: auxinput5_end_y
integer                                  :: auxinput5_end_d
integer                                  :: auxinput5_end_h
integer                                  :: auxinput5_end_m
integer                                  :: auxinput5_end_s
integer                                  :: auxinput5_end
integer                                  :: io_form_auxinput5
integer                                  :: frames_per_auxinput5
integer                                  :: auxinput6_oid
character*256                               :: auxinput6_inname
character*256                               :: auxinput6_outname
integer                                  :: auxinput6_interval_y
integer                                  :: auxinput6_interval_d
integer                                  :: auxinput6_interval_h
integer                                  :: auxinput6_interval_m
integer                                  :: auxinput6_interval_s
integer                                  :: auxinput6_interval
integer                                  :: auxinput6_begin_y
integer                                  :: auxinput6_begin_d
integer                                  :: auxinput6_begin_h
integer                                  :: auxinput6_begin_m
integer                                  :: auxinput6_begin_s
integer                                  :: auxinput6_begin
integer                                  :: auxinput6_end_y
integer                                  :: auxinput6_end_d
integer                                  :: auxinput6_end_h
integer                                  :: auxinput6_end_m
integer                                  :: auxinput6_end_s
integer                                  :: auxinput6_end
integer                                  :: io_form_auxinput6
integer                                  :: frames_per_auxinput6
integer                                  :: auxinput7_oid
character*256                               :: auxinput7_inname
character*256                               :: auxinput7_outname
integer                                  :: auxinput7_interval_y
integer                                  :: auxinput7_interval_d
integer                                  :: auxinput7_interval_h
integer                                  :: auxinput7_interval_m
integer                                  :: auxinput7_interval_s
integer                                  :: auxinput7_interval
integer                                  :: auxinput7_begin_y
integer                                  :: auxinput7_begin_d
integer                                  :: auxinput7_begin_h
integer                                  :: auxinput7_begin_m
integer                                  :: auxinput7_begin_s
integer                                  :: auxinput7_begin
integer                                  :: auxinput7_end_y
integer                                  :: auxinput7_end_d
integer                                  :: auxinput7_end_h
integer                                  :: auxinput7_end_m
integer                                  :: auxinput7_end_s
integer                                  :: auxinput7_end
integer                                  :: io_form_auxinput7
integer                                  :: frames_per_auxinput7
integer                                  :: auxinput8_oid
character*256                               :: auxinput8_inname
character*256                               :: auxinput8_outname
integer                                  :: auxinput8_interval_y
integer                                  :: auxinput8_interval_d
integer                                  :: auxinput8_interval_h
integer                                  :: auxinput8_interval_m
integer                                  :: auxinput8_interval_s
integer                                  :: auxinput8_interval
integer                                  :: auxinput8_begin_y
integer                                  :: auxinput8_begin_d
integer                                  :: auxinput8_begin_h
integer                                  :: auxinput8_begin_m
integer                                  :: auxinput8_begin_s
integer                                  :: auxinput8_begin
integer                                  :: auxinput8_end_y
integer                                  :: auxinput8_end_d
integer                                  :: auxinput8_end_h
integer                                  :: auxinput8_end_m
integer                                  :: auxinput8_end_s
integer                                  :: auxinput8_end
integer                                  :: io_form_auxinput8
integer                                  :: frames_per_auxinput8
integer                                  :: auxinput9_oid
character*256                               :: auxinput9_inname
character*256                               :: auxinput9_outname
integer                                  :: auxinput9_interval_y
integer                                  :: auxinput9_interval_d
integer                                  :: auxinput9_interval_h
integer                                  :: auxinput9_interval_m
integer                                  :: auxinput9_interval_s
integer                                  :: auxinput9_interval
integer                                  :: auxinput9_begin_y
integer                                  :: auxinput9_begin_d
integer                                  :: auxinput9_begin_h
integer                                  :: auxinput9_begin_m
integer                                  :: auxinput9_begin_s
integer                                  :: auxinput9_begin
integer                                  :: auxinput9_end_y
integer                                  :: auxinput9_end_d
integer                                  :: auxinput9_end_h
integer                                  :: auxinput9_end_m
integer                                  :: auxinput9_end_s
integer                                  :: auxinput9_end
integer                                  :: io_form_auxinput9
integer                                  :: frames_per_auxinput9
integer                                  :: auxinput10_oid
character*256                               :: auxinput10_inname
character*256                               :: auxinput10_outname
integer                                  :: auxinput10_interval_y
integer                                  :: auxinput10_interval_d
integer                                  :: auxinput10_interval_h
integer                                  :: auxinput10_interval_m
integer                                  :: auxinput10_interval_s
integer                                  :: auxinput10_interval
integer                                  :: auxinput10_begin_y
integer                                  :: auxinput10_begin_d
integer                                  :: auxinput10_begin_h
integer                                  :: auxinput10_begin_m
integer                                  :: auxinput10_begin_s
integer                                  :: auxinput10_begin
integer                                  :: auxinput10_end_y
integer                                  :: auxinput10_end_d
integer                                  :: auxinput10_end_h
integer                                  :: auxinput10_end_m
integer                                  :: auxinput10_end_s
integer                                  :: auxinput10_end
integer                                  :: io_form_auxinput10
integer                                  :: frames_per_auxinput10
integer                                  :: auxinput11_oid
character*256                               :: auxinput11_inname
character*256                               :: auxinput11_outname
integer                                  :: auxinput11_interval_y
integer                                  :: auxinput11_interval_d
integer                                  :: auxinput11_interval_h
integer                                  :: auxinput11_interval_m
integer                                  :: auxinput11_interval_s
integer                                  :: auxinput11_interval
integer                                  :: auxinput11_begin_y
integer                                  :: auxinput11_begin_d
integer                                  :: auxinput11_begin_h
integer                                  :: auxinput11_begin_m
integer                                  :: auxinput11_begin_s
integer                                  :: auxinput11_begin
integer                                  :: auxinput11_end_y
integer                                  :: auxinput11_end_d
integer                                  :: auxinput11_end_h
integer                                  :: auxinput11_end_m
integer                                  :: auxinput11_end_s
integer                                  :: auxinput11_end
integer                                  :: io_form_auxinput11
integer                                  :: frames_per_auxinput11
integer                                  :: auxinput12_oid
character*256                               :: auxinput12_inname
character*256                               :: auxinput12_outname
integer                                  :: auxinput12_interval_y
integer                                  :: auxinput12_interval_d
integer                                  :: auxinput12_interval_h
integer                                  :: auxinput12_interval_m
integer                                  :: auxinput12_interval_s
integer                                  :: auxinput12_interval
integer                                  :: auxinput12_begin_y
integer                                  :: auxinput12_begin_d
integer                                  :: auxinput12_begin_h
integer                                  :: auxinput12_begin_m
integer                                  :: auxinput12_begin_s
integer                                  :: auxinput12_begin
integer                                  :: auxinput12_end_y
integer                                  :: auxinput12_end_d
integer                                  :: auxinput12_end_h
integer                                  :: auxinput12_end_m
integer                                  :: auxinput12_end_s
integer                                  :: auxinput12_end
integer                                  :: io_form_auxinput12
integer                                  :: frames_per_auxinput12
integer                                  :: auxinput13_oid
character*256                               :: auxinput13_inname
character*256                               :: auxinput13_outname
integer                                  :: auxinput13_interval_y
integer                                  :: auxinput13_interval_d
integer                                  :: auxinput13_interval_h
integer                                  :: auxinput13_interval_m
integer                                  :: auxinput13_interval_s
integer                                  :: auxinput13_interval
integer                                  :: auxinput13_begin_y
integer                                  :: auxinput13_begin_d
integer                                  :: auxinput13_begin_h
integer                                  :: auxinput13_begin_m
integer                                  :: auxinput13_begin_s
integer                                  :: auxinput13_begin
integer                                  :: auxinput13_end_y
integer                                  :: auxinput13_end_d
integer                                  :: auxinput13_end_h
integer                                  :: auxinput13_end_m
integer                                  :: auxinput13_end_s
integer                                  :: auxinput13_end
integer                                  :: io_form_auxinput13
integer                                  :: frames_per_auxinput13
integer                                  :: auxinput14_oid
character*256                               :: auxinput14_inname
character*256                               :: auxinput14_outname
integer                                  :: auxinput14_interval_y
integer                                  :: auxinput14_interval_d
integer                                  :: auxinput14_interval_h
integer                                  :: auxinput14_interval_m
integer                                  :: auxinput14_interval_s
integer                                  :: auxinput14_interval
integer                                  :: auxinput14_begin_y
integer                                  :: auxinput14_begin_d
integer                                  :: auxinput14_begin_h
integer                                  :: auxinput14_begin_m
integer                                  :: auxinput14_begin_s
integer                                  :: auxinput14_begin
integer                                  :: auxinput14_end_y
integer                                  :: auxinput14_end_d
integer                                  :: auxinput14_end_h
integer                                  :: auxinput14_end_m
integer                                  :: auxinput14_end_s
integer                                  :: auxinput14_end
integer                                  :: io_form_auxinput14
integer                                  :: frames_per_auxinput14
integer                                  :: auxinput15_oid
character*256                               :: auxinput15_inname
character*256                               :: auxinput15_outname
integer                                  :: auxinput15_interval_y
integer                                  :: auxinput15_interval_d
integer                                  :: auxinput15_interval_h
integer                                  :: auxinput15_interval_m
integer                                  :: auxinput15_interval_s
integer                                  :: auxinput15_interval
integer                                  :: auxinput15_begin_y
integer                                  :: auxinput15_begin_d
integer                                  :: auxinput15_begin_h
integer                                  :: auxinput15_begin_m
integer                                  :: auxinput15_begin_s
integer                                  :: auxinput15_begin
integer                                  :: auxinput15_end_y
integer                                  :: auxinput15_end_d
integer                                  :: auxinput15_end_h
integer                                  :: auxinput15_end_m
integer                                  :: auxinput15_end_s
integer                                  :: auxinput15_end
integer                                  :: io_form_auxinput15
integer                                  :: frames_per_auxinput15
integer                                  :: auxinput16_oid
character*256                               :: auxinput16_inname
character*256                               :: auxinput16_outname
integer                                  :: auxinput16_interval_y
integer                                  :: auxinput16_interval_d
integer                                  :: auxinput16_interval_h
integer                                  :: auxinput16_interval_m
integer                                  :: auxinput16_interval_s
integer                                  :: auxinput16_interval
integer                                  :: auxinput16_begin_y
integer                                  :: auxinput16_begin_d
integer                                  :: auxinput16_begin_h
integer                                  :: auxinput16_begin_m
integer                                  :: auxinput16_begin_s
integer                                  :: auxinput16_begin
integer                                  :: auxinput16_end_y
integer                                  :: auxinput16_end_d
integer                                  :: auxinput16_end_h
integer                                  :: auxinput16_end_m
integer                                  :: auxinput16_end_s
integer                                  :: auxinput16_end
integer                                  :: io_form_auxinput16
integer                                  :: frames_per_auxinput16
integer                                  :: auxinput17_oid
character*256                               :: auxinput17_inname
character*256                               :: auxinput17_outname
integer                                  :: auxinput17_interval_y
integer                                  :: auxinput17_interval_d
integer                                  :: auxinput17_interval_h
integer                                  :: auxinput17_interval_m
integer                                  :: auxinput17_interval_s
integer                                  :: auxinput17_interval
integer                                  :: auxinput17_begin_y
integer                                  :: auxinput17_begin_d
integer                                  :: auxinput17_begin_h
integer                                  :: auxinput17_begin_m
integer                                  :: auxinput17_begin_s
integer                                  :: auxinput17_begin
integer                                  :: auxinput17_end_y
integer                                  :: auxinput17_end_d
integer                                  :: auxinput17_end_h
integer                                  :: auxinput17_end_m
integer                                  :: auxinput17_end_s
integer                                  :: auxinput17_end
integer                                  :: io_form_auxinput17
integer                                  :: frames_per_auxinput17
integer                                  :: auxinput18_oid
character*256                               :: auxinput18_inname
character*256                               :: auxinput18_outname
integer                                  :: auxinput18_interval_y
integer                                  :: auxinput18_interval_d
integer                                  :: auxinput18_interval_h
integer                                  :: auxinput18_interval_m
integer                                  :: auxinput18_interval_s
integer                                  :: auxinput18_interval
integer                                  :: auxinput18_begin_y
integer                                  :: auxinput18_begin_d
integer                                  :: auxinput18_begin_h
integer                                  :: auxinput18_begin_m
integer                                  :: auxinput18_begin_s
integer                                  :: auxinput18_begin
integer                                  :: auxinput18_end_y
integer                                  :: auxinput18_end_d
integer                                  :: auxinput18_end_h
integer                                  :: auxinput18_end_m
integer                                  :: auxinput18_end_s
integer                                  :: auxinput18_end
integer                                  :: io_form_auxinput18
integer                                  :: frames_per_auxinput18
integer                                  :: auxinput19_oid
character*256                               :: auxinput19_inname
character*256                               :: auxinput19_outname
integer                                  :: auxinput19_interval_y
integer                                  :: auxinput19_interval_d
integer                                  :: auxinput19_interval_h
integer                                  :: auxinput19_interval_m
integer                                  :: auxinput19_interval_s
integer                                  :: auxinput19_interval
integer                                  :: auxinput19_begin_y
integer                                  :: auxinput19_begin_d
integer                                  :: auxinput19_begin_h
integer                                  :: auxinput19_begin_m
integer                                  :: auxinput19_begin_s
integer                                  :: auxinput19_begin
integer                                  :: auxinput19_end_y
integer                                  :: auxinput19_end_d
integer                                  :: auxinput19_end_h
integer                                  :: auxinput19_end_m
integer                                  :: auxinput19_end_s
integer                                  :: auxinput19_end
integer                                  :: io_form_auxinput19
integer                                  :: frames_per_auxinput19
integer                                  :: auxinput20_oid
character*256                               :: auxinput20_inname
character*256                               :: auxinput20_outname
integer                                  :: auxinput20_interval_y
integer                                  :: auxinput20_interval_d
integer                                  :: auxinput20_interval_h
integer                                  :: auxinput20_interval_m
integer                                  :: auxinput20_interval_s
integer                                  :: auxinput20_interval
integer                                  :: auxinput20_begin_y
integer                                  :: auxinput20_begin_d
integer                                  :: auxinput20_begin_h
integer                                  :: auxinput20_begin_m
integer                                  :: auxinput20_begin_s
integer                                  :: auxinput20_begin
integer                                  :: auxinput20_end_y
integer                                  :: auxinput20_end_d
integer                                  :: auxinput20_end_h
integer                                  :: auxinput20_end_m
integer                                  :: auxinput20_end_s
integer                                  :: auxinput20_end
integer                                  :: io_form_auxinput20
integer                                  :: frames_per_auxinput20
integer                                  :: auxinput21_oid
character*256                               :: auxinput21_inname
character*256                               :: auxinput21_outname
integer                                  :: auxinput21_interval_y
integer                                  :: auxinput21_interval_d
integer                                  :: auxinput21_interval_h
integer                                  :: auxinput21_interval_m
integer                                  :: auxinput21_interval_s
integer                                  :: auxinput21_interval
integer                                  :: auxinput21_begin_y
integer                                  :: auxinput21_begin_d
integer                                  :: auxinput21_begin_h
integer                                  :: auxinput21_begin_m
integer                                  :: auxinput21_begin_s
integer                                  :: auxinput21_begin
integer                                  :: auxinput21_end_y
integer                                  :: auxinput21_end_d
integer                                  :: auxinput21_end_h
integer                                  :: auxinput21_end_m
integer                                  :: auxinput21_end_s
integer                                  :: auxinput21_end
integer                                  :: io_form_auxinput21
integer                                  :: frames_per_auxinput21
integer                                  :: auxinput22_oid
character*256                               :: auxinput22_inname
character*256                               :: auxinput22_outname
integer                                  :: auxinput22_interval_y
integer                                  :: auxinput22_interval_d
integer                                  :: auxinput22_interval_h
integer                                  :: auxinput22_interval_m
integer                                  :: auxinput22_interval_s
integer                                  :: auxinput22_interval
integer                                  :: auxinput22_begin_y
integer                                  :: auxinput22_begin_d
integer                                  :: auxinput22_begin_h
integer                                  :: auxinput22_begin_m
integer                                  :: auxinput22_begin_s
integer                                  :: auxinput22_begin
integer                                  :: auxinput22_end_y
integer                                  :: auxinput22_end_d
integer                                  :: auxinput22_end_h
integer                                  :: auxinput22_end_m
integer                                  :: auxinput22_end_s
integer                                  :: auxinput22_end
integer                                  :: io_form_auxinput22
integer                                  :: frames_per_auxinput22
integer                                  :: auxinput23_oid
character*256                               :: auxinput23_inname
character*256                               :: auxinput23_outname
integer                                  :: auxinput23_interval_y
integer                                  :: auxinput23_interval_d
integer                                  :: auxinput23_interval_h
integer                                  :: auxinput23_interval_m
integer                                  :: auxinput23_interval_s
integer                                  :: auxinput23_interval
integer                                  :: auxinput23_begin_y
integer                                  :: auxinput23_begin_d
integer                                  :: auxinput23_begin_h
integer                                  :: auxinput23_begin_m
integer                                  :: auxinput23_begin_s
integer                                  :: auxinput23_begin
integer                                  :: auxinput23_end_y
integer                                  :: auxinput23_end_d
integer                                  :: auxinput23_end_h
integer                                  :: auxinput23_end_m
integer                                  :: auxinput23_end_s
integer                                  :: auxinput23_end
integer                                  :: io_form_auxinput23
integer                                  :: frames_per_auxinput23
integer                                  :: auxinput24_oid
character*256                               :: auxinput24_inname
character*256                               :: auxinput24_outname
integer                                  :: auxinput24_interval_y
integer                                  :: auxinput24_interval_d
integer                                  :: auxinput24_interval_h
integer                                  :: auxinput24_interval_m
integer                                  :: auxinput24_interval_s
integer                                  :: auxinput24_interval
integer                                  :: auxinput24_begin_y
integer                                  :: auxinput24_begin_d
integer                                  :: auxinput24_begin_h
integer                                  :: auxinput24_begin_m
integer                                  :: auxinput24_begin_s
integer                                  :: auxinput24_begin
integer                                  :: auxinput24_end_y
integer                                  :: auxinput24_end_d
integer                                  :: auxinput24_end_h
integer                                  :: auxinput24_end_m
integer                                  :: auxinput24_end_s
integer                                  :: auxinput24_end
integer                                  :: io_form_auxinput24
integer                                  :: frames_per_auxinput24
integer                                  :: oid
integer                                  :: history_interval
integer                                  :: frames_per_outfile
logical                                  :: restart
integer                                  :: restart_interval
integer                                  :: io_form_input
integer                                  :: io_form_history
integer                                  :: io_form_restart
integer                                  :: io_form_boundary
integer                                  :: debug_level
logical                                  :: self_test_domain
character*256                               :: history_outname
character*256                               :: history_inname
logical                                  :: use_netcdf_classic
integer                                  :: history_interval_d
integer                                  :: history_interval_h
integer                                  :: history_interval_m
integer                                  :: history_interval_s
integer                                  :: inputout_interval_d
integer                                  :: inputout_interval_h
integer                                  :: inputout_interval_m
integer                                  :: inputout_interval_s
integer                                  :: inputout_interval
integer                                  :: restart_interval_d
integer                                  :: restart_interval_h
integer                                  :: restart_interval_m
integer                                  :: restart_interval_s
integer                                  :: history_begin_y
integer                                  :: history_begin_d
integer                                  :: history_begin_h
integer                                  :: history_begin_m
integer                                  :: history_begin_s
integer                                  :: history_begin
integer                                  :: inputout_begin_y
integer                                  :: inputout_begin_d
integer                                  :: inputout_begin_h
integer                                  :: inputout_begin_m
integer                                  :: inputout_begin_s
integer                                  :: restart_begin_y
integer                                  :: restart_begin_d
integer                                  :: restart_begin_h
integer                                  :: restart_begin_m
integer                                  :: restart_begin_s
integer                                  :: restart_begin
integer                                  :: history_end_y
integer                                  :: history_end_d
integer                                  :: history_end_h
integer                                  :: history_end_m
integer                                  :: history_end_s
integer                                  :: history_end
integer                                  :: inputout_end_y
integer                                  :: inputout_end_d
integer                                  :: inputout_end_h
integer                                  :: inputout_end_m
integer                                  :: inputout_end_s
integer                                  :: simulation_start_year
integer                                  :: simulation_start_month
integer                                  :: simulation_start_day
integer                                  :: simulation_start_hour
integer                                  :: simulation_start_minute
integer                                  :: simulation_start_second
logical                                  :: reset_simulation_start
integer                                  :: sr_x
integer                                  :: sr_y
character*256                               :: iofields_filename
logical                                  :: ignore_iofields_warning
logical                                  :: ncd_nofill
integer                                  :: julyr
integer                                  :: julday
real                                     :: gmt
character*256                               :: high_freq_outname
character*256                               :: partial_atcf_outname
character*256                               :: input_inname
character*256                               :: input_outname
character*256                               :: bdy_inname
character*256                               :: bdy_outname
character*256                               :: rst_inname
character*256                               :: rst_outname
character*256                               :: anl_outname
logical                                  :: write_input
logical                                  :: write_restart_at_0h
logical                                  :: write_hist_at_0h_rst
logical                                  :: adjust_output_times
logical                                  :: adjust_input_times
real                                     :: tstart
logical                                  :: nocolons
logical                                  :: cycling
logical                                  :: output_ready_flag
integer                                  :: dfi_opt
integer                                  :: dfi_savehydmeteors
integer                                  :: dfi_nfilter
logical                                  :: dfi_write_filtered_input
logical                                  :: dfi_write_dfi_history
integer                                  :: dfi_cutoff_seconds
integer                                  :: dfi_time_dim
integer                                  :: dfi_fwdstop_year
integer                                  :: dfi_fwdstop_month
integer                                  :: dfi_fwdstop_day
integer                                  :: dfi_fwdstop_hour
integer                                  :: dfi_fwdstop_minute
integer                                  :: dfi_fwdstop_second
integer                                  :: dfi_bckstop_year
integer                                  :: dfi_bckstop_month
integer                                  :: dfi_bckstop_day
integer                                  :: dfi_bckstop_hour
integer                                  :: dfi_bckstop_minute
integer                                  :: dfi_bckstop_second
integer                                  :: time_step
integer                                  :: time_step_fract_num
integer                                  :: time_step_fract_den
integer                                  :: time_step_dfi
integer                                  :: max_dom
integer                                  :: s_we
integer                                  :: e_we
integer                                  :: s_sn
integer                                  :: e_sn
integer                                  :: s_vert
integer                                  :: e_vert
integer                                  :: num_metgrid_soil_levels
real                                     :: dx
real                                     :: dy
integer                                  :: grid_id
logical                                  :: grid_allowed
integer                                  :: parent_id
integer                                  :: i_parent_start
integer                                  :: j_parent_start
integer                                  :: parent_grid_ratio
integer                                  :: parent_time_step_ratio
integer                                  :: feedback
integer                                  :: smooth_option
real                                     :: ztop
integer                                  :: moad_grid_ratio
integer                                  :: moad_time_step_ratio
integer                                  :: shw
integer                                  :: tile_sz_x
integer                                  :: tile_sz_y
integer                                  :: numtiles
integer                                  :: numtiles_inc
integer                                  :: numtiles_x
integer                                  :: numtiles_y
integer                                  :: tile_strategy
integer                                  :: nproc_x
integer                                  :: nproc_y
integer                                  :: irand
real                                     :: dt
integer                                  :: ts_buf_size
integer                                  :: max_ts_locs
integer                                  :: num_moves
integer                                  :: vortex_interval
integer                                  :: corral_dist
integer                                  :: move_id
integer                                  :: move_interval
integer                                  :: move_cd_x
integer                                  :: move_cd_y
logical                                  :: swap_x
logical                                  :: swap_y
logical                                  :: cycle_x
logical                                  :: cycle_y
logical                                  :: reorder_mesh
logical                                  :: perturb_input
real                                     :: eta_levels
real                                     :: ptsgm
integer                                  :: num_metgrid_levels
real                                     :: p_top_requested
logical                                  :: use_prep_hybrid
logical                                  :: force_read_thompson
logical                                  :: write_thompson_tables
integer                                  :: mp_physics
real                                     :: mommix
logical                                  :: disheat
integer                                  :: do_radar_ref
integer                                  :: compute_radar_ref
integer                                  :: ra_lw_physics
integer                                  :: ra_sw_physics
real                                     :: radt
integer                                  :: sf_sfclay_physics
integer                                  :: sf_surface_physics
integer                                  :: bl_pbl_physics
integer                                  :: ysu_topdown_pblmix
integer                                  :: shinhong_tke_diag
integer                                  :: windfarm_opt
integer                                  :: windfarm_ij
integer                                  :: mfshconv
real                                     :: bldt
integer                                  :: cu_physics
integer                                  :: shcu_physics
integer                                  :: cu_diag
real                                     :: gfs_alpha
real                                     :: cudt
real                                     :: gsmdt
integer                                  :: isfflx
integer                                  :: ideal_xland
integer                                  :: ifsnow
integer                                  :: icloud
real                                     :: swrad_scat
integer                                  :: surface_input_source
integer                                  :: num_soil_layers
integer                                  :: num_urban_layers
integer                                  :: sf_surface_mosaic
integer                                  :: mosaic_cat
integer                                  :: mosaic_cat_soil
integer                                  :: num_urban_hi
integer                                  :: mosaic_lu
integer                                  :: mosaic_soil
integer                                  :: maxiens
integer                                  :: maxens
integer                                  :: maxens2
integer                                  :: maxens3
integer                                  :: ensdim
integer                                  :: chem_opt
integer                                  :: num_land_cat
integer                                  :: num_soil_cat
integer                                  :: topo_wind
integer                                  :: mp_zero_out
real                                     :: mp_zero_out_thresh
real                                     :: seaice_threshold
integer                                  :: fractional_seaice
integer                                  :: seaice_albedo_opt
real                                     :: seaice_albedo_default
integer                                  :: seaice_snowdepth_opt
real                                     :: seaice_snowdepth_max
real                                     :: seaice_snowdepth_min
integer                                  :: seaice_thickness_opt
real                                     :: seaice_thickness_default
logical                                  :: tice2tsk_if2cold
integer                                  :: sst_update
integer                                  :: sf_urban_physics
logical                                  :: usemonalb
logical                                  :: rdmaxalb
logical                                  :: rdlai2d
logical                                  :: ua_phys
integer                                  :: gwd_opt
integer                                  :: iz0tlnd
real                                     :: sas_pgcon
real                                     :: sas_shal_pgcon
integer                                  :: sas_shal_conv
real                                     :: sas_mass_flux
real                                     :: var_ric
real                                     :: coef_ric_l
real                                     :: coef_ric_s
integer                                  :: random_seed
integer                                  :: icoef_sf
logical                                  :: lcurr_sf
integer                                  :: ens_random_seed
logical                                  :: pert_sas
logical                                  :: pert_pbl
real                                     :: ens_sasamp
real                                     :: ens_pblamp
integer                                  :: idtad
integer                                  :: nsoil
integer                                  :: nphs
integer                                  :: ncnvc
integer                                  :: nrand
integer                                  :: nrads
integer                                  :: nradl
real                                     :: tprec
real                                     :: theat
real                                     :: tclod
real                                     :: trdsw
real                                     :: trdlw
real                                     :: tsrfc
logical                                  :: pcpflg
integer                                  :: sigma
real                                     :: sfenth
integer                                  :: co2tf
integer                                  :: ra_call_offset
real                                     :: cam_abs_freq_s
integer                                  :: levsiz
integer                                  :: paerlev
integer                                  :: cam_abs_dim1
integer                                  :: cam_abs_dim2
integer                                  :: no_src_types
integer                                  :: alevsiz
integer                                  :: o3input
integer                                  :: aer_opt
logical                                  :: cu_rad_feedback
integer                                  :: icloud_cu
real                                     :: h_diff
integer                                  :: movemin
integer                                  :: num_snso_layers
integer                                  :: num_snow_layers
logical                                  :: use_aero_icbc
real                                     :: ccn_conc
integer                                  :: hail_opt
integer                                  :: sf_lake_physics
integer                                  :: dyn_opt
integer                                  :: rk_ord
integer                                  :: w_damping
integer                                  :: diff_opt
integer                                  :: km_opt
integer                                  :: damp_opt
real                                     :: zdamp
real                                     :: base_pres
real                                     :: base_temp
real                                     :: base_lapse
real                                     :: iso_temp
real                                     :: dampcoef
real                                     :: khdif
real                                     :: kvdif
real                                     :: c_s
real                                     :: c_k
real                                     :: smdiv
real                                     :: emdiv
real                                     :: epssm
logical                                  :: non_hydrostatic
integer                                  :: time_step_sound
integer                                  :: h_mom_adv_order
integer                                  :: v_mom_adv_order
integer                                  :: h_sca_adv_order
integer                                  :: v_sca_adv_order
logical                                  :: top_radiation
real                                     :: tke_upper_bound
real                                     :: tke_drag_coefficient
real                                     :: tke_heat_flux
logical                                  :: pert_coriolis
logical                                  :: euler_adv
integer                                  :: idtadt
integer                                  :: idtadc
real                                     :: codamp
real                                     :: coac
real                                     :: slophc
real                                     :: wp
integer                                  :: terrain_smoothing
integer                                  :: spec_bdy_width
integer                                  :: spec_zone
integer                                  :: relax_zone
logical                                  :: specified
logical                                  :: periodic_x
logical                                  :: symmetric_xs
logical                                  :: symmetric_xe
logical                                  :: open_xs
logical                                  :: open_xe
logical                                  :: periodic_y
logical                                  :: symmetric_ys
logical                                  :: symmetric_ye
logical                                  :: open_ys
logical                                  :: open_ye
logical                                  :: polar
logical                                  :: nested
integer                                  :: real_data_init_type
integer                                  :: background_proc_id
integer                                  :: forecast_proc_id
integer                                  :: production_status
integer                                  :: compression
real                                     :: cen_lat
real                                     :: cen_lon
real                                     :: truelat1
real                                     :: truelat2
real                                     :: moad_cen_lat
real                                     :: stand_lon
integer                                  :: flag_metgrid
integer                                  :: flag_snow
integer                                  :: flag_psfc
integer                                  :: flag_sm000010
integer                                  :: flag_sm010040
integer                                  :: flag_sm040100
integer                                  :: flag_sm100200
integer                                  :: flag_st000010
integer                                  :: flag_st010040
integer                                  :: flag_st040100
integer                                  :: flag_st100200
integer                                  :: flag_slp
integer                                  :: flag_soilhgt
integer                                  :: flag_mf_xy
real                                     :: bdyfrq
character*256                               :: mminlu
integer                                  :: iswater
integer                                  :: islake
integer                                  :: isice
integer                                  :: isurban
integer                                  :: isoilwater
integer                                  :: map_proj
integer                                  :: dfi_stage
integer                                  :: mp_physics_dfi
integer                                  :: nodyn_dummy
integer                                  :: maxpatch
logical   ,DIMENSION(:,:)     ,POINTER   :: lake2d
real      ,DIMENSION(:,:)     ,POINTER   :: lakedepth2d
real      ,DIMENSION(:,:)     ,POINTER   :: savedtke12d
real      ,DIMENSION(:,:)     ,POINTER   :: snowdp2d
real      ,DIMENSION(:,:)     ,POINTER   :: h2osno2d
real      ,DIMENSION(:,:)     ,POINTER   :: snl2d
real      ,DIMENSION(:,:)     ,POINTER   :: t_grnd2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_lake3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: lake_icefrac3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: z_lake3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: dz_lake3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_soisno3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_ice3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_liq3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: h2osoi_vol3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: z3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: dz3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: zi3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: watsat3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: csol3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: tkmg3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: tkdry3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: tksatu3d
real      ,DIMENSION(:,:,:,:) ,POINTER   :: szj
real      ,DIMENSION(:,:,:,:) ,POINTER   :: s1z
real      ,DIMENSION(:,:,:,:) ,POINTER   :: spz
real      ,DIMENSION(:,:,:,:) ,POINTER   :: tcs
real      ,DIMENSION(:,:)     ,POINTER   :: lu_index
real      ,DIMENSION(:,:)     ,POINTER   :: lu_mask
real      ,DIMENSION(:,:,:)   ,POINTER   :: p_gc
real      ,DIMENSION(:,:)     ,POINTER   :: vegcat
real      ,DIMENSION(:,:)     ,POINTER   :: soilcat
real      ,DIMENSION(:,:)     ,POINTER   :: input_soil_cat
real      ,DIMENSION(:,:)     ,POINTER   :: tsk_gc
real      ,DIMENSION(:,:)     ,POINTER   :: xice_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: ght_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: rh_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_gc
real      ,DIMENSION(:,:)     ,POINTER   :: snoalb
real      ,DIMENSION(:,:,:)   ,POINTER   :: greenfrac_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: albedo12m_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: lai12m_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: soilcbot_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: soilctop_gc
real      ,DIMENSION(:,:)     ,POINTER   :: tmn_gc
real      ,DIMENSION(:,:)     ,POINTER   :: htv_gc
real      ,DIMENSION(:,:)     ,POINTER   :: ht_gc
real      ,DIMENSION(:,:,:)   ,POINTER   :: landusef_gc
real      ,DIMENSION(:,:)     ,POINTER   :: vlon_gc
real      ,DIMENSION(:,:)     ,POINTER   :: vlat_gc
real      ,DIMENSION(:,:)     ,POINTER   :: hlon_gc
real      ,DIMENSION(:,:)     ,POINTER   :: hlat_gc
integer   ,DIMENSION(:)       ,POINTER   :: nrnd1
real      ,DIMENSION(:,:)     ,POINTER   :: relaxwork
integer   ,DIMENSION(:,:)     ,POINTER   :: relaximask
logical   ,DIMENSION(:,:)     ,POINTER   :: relaxmask
integer   ,DIMENSION(:,:)     ,POINTER   :: interesting
real      ,DIMENSION(:,:)     ,POINTER   :: precip_swath
real      ,DIMENSION(:,:)     ,POINTER   :: windsq_swath
real      ,DIMENSION(:,:)     ,POINTER   :: tracker_distsq
real      ,DIMENSION(:,:)     ,POINTER   :: tracker_angle
real      ,DIMENSION(:)       ,POINTER   :: track_old_lon
real      ,DIMENSION(:)       ,POINTER   :: track_old_lat
integer   ,DIMENSION(:)       ,POINTER   :: track_old_ntsd
integer   ,DIMENSION(:,:)     ,POINTER   :: tracker_fixes
real      ,DIMENSION(:,:)     ,POINTER   :: membrane_mslp
real      ,DIMENSION(:,:)     ,POINTER   :: p850rv
real      ,DIMENSION(:,:)     ,POINTER   :: p700rv
real      ,DIMENSION(:,:)     ,POINTER   :: p850wind
real      ,DIMENSION(:,:)     ,POINTER   :: p700wind
real      ,DIMENSION(:,:)     ,POINTER   :: p500u
real      ,DIMENSION(:,:)     ,POINTER   :: p500v
real      ,DIMENSION(:,:)     ,POINTER   :: p700u
real      ,DIMENSION(:,:)     ,POINTER   :: p700v
real      ,DIMENSION(:,:)     ,POINTER   :: p850u
real      ,DIMENSION(:,:)     ,POINTER   :: p850v
real      ,DIMENSION(:,:)     ,POINTER   :: p850z
real      ,DIMENSION(:,:)     ,POINTER   :: p700z
real      ,DIMENSION(:,:)     ,POINTER   :: m10wind
real      ,DIMENSION(:,:)     ,POINTER   :: m10rv
real      ,DIMENSION(:,:)     ,POINTER   :: sp850rv
real      ,DIMENSION(:,:)     ,POINTER   :: sp700rv
real      ,DIMENSION(:,:)     ,POINTER   :: sp850wind
real      ,DIMENSION(:,:)     ,POINTER   :: sp700wind
real      ,DIMENSION(:,:)     ,POINTER   :: sp850z
real      ,DIMENSION(:,:)     ,POINTER   :: sp700z
real      ,DIMENSION(:,:)     ,POINTER   :: sm10wind
real      ,DIMENSION(:,:)     ,POINTER   :: sm10rv
real      ,DIMENSION(:,:)     ,POINTER   :: smslp
real      ,DIMENSION(:,:)     ,POINTER   :: distsq
real      ,DIMENSION(:,:)     ,POINTER   :: weightout
integer   ,DIMENSION(:,:)     ,POINTER   :: mslp_noisy
real      ,DIMENSION(:,:)     ,POINTER   :: pdyn_smooth
real      ,DIMENSION(:,:)     ,POINTER   :: pdyn_parent
real      ,DIMENSION(:,:)     ,POINTER   :: pdyn
real      ,DIMENSION(:,:)     ,POINTER   :: mslp
real      ,DIMENSION(:,:)     ,POINTER   :: best_mslp
real      ,DIMENSION(:,:)     ,POINTER   :: sqws
real      ,DIMENSION(:,:,:)   ,POINTER   :: ducudt
real      ,DIMENSION(:,:,:)   ,POINTER   :: dvcudt
integer   ,DIMENSION(:,:)     ,POINTER   :: randstate1
integer   ,DIMENSION(:,:)     ,POINTER   :: randstate2
integer   ,DIMENSION(:,:)     ,POINTER   :: randstate3
integer   ,DIMENSION(:,:)     ,POINTER   :: randstate4
real      ,DIMENSION(:,:)     ,POINTER   :: random
integer   ,DIMENSION(:,:)     ,POINTER   :: iih
integer   ,DIMENSION(:,:)     ,POINTER   :: jjh
integer   ,DIMENSION(:,:)     ,POINTER   :: iiv
integer   ,DIMENSION(:,:)     ,POINTER   :: jjv
integer   ,DIMENSION(:,:)     ,POINTER   :: hnear_i
integer   ,DIMENSION(:,:)     ,POINTER   :: hnear_j
real      ,DIMENSION(:,:)     ,POINTER   :: hbwgt1
real      ,DIMENSION(:,:)     ,POINTER   :: hbwgt2
real      ,DIMENSION(:,:)     ,POINTER   :: hbwgt3
real      ,DIMENSION(:,:)     ,POINTER   :: hbwgt4
real      ,DIMENSION(:,:)     ,POINTER   :: vbwgt1
real      ,DIMENSION(:,:)     ,POINTER   :: vbwgt2
real      ,DIMENSION(:,:)     ,POINTER   :: vbwgt3
real      ,DIMENSION(:,:)     ,POINTER   :: vbwgt4
real      ,DIMENSION(:,:)     ,POINTER   :: hlon
real      ,DIMENSION(:,:)     ,POINTER   :: hlat
real      ,DIMENSION(:,:)     ,POINTER   :: vlon
real      ,DIMENSION(:,:)     ,POINTER   :: vlat
real      ,DIMENSION(:,:)     ,POINTER   :: tg_max_m10wind
real      ,DIMENSION(:,:)     ,POINTER   :: tg_max_wwind
real      ,DIMENSION(:,:)     ,POINTER   :: tg_min_wwind
real      ,DIMENSION(:,:)     ,POINTER   :: tg_max_zhel_25
real      ,DIMENSION(:,:)     ,POINTER   :: tg_min_zhel_25
real      ,DIMENSION(:,:)     ,POINTER   :: tg_max_zhel_03
real      ,DIMENSION(:,:)     ,POINTER   :: tg_min_zhel_03
real      ,DIMENSION(:,:)     ,POINTER   :: tg_updhel25
real      ,DIMENSION(:,:)     ,POINTER   :: tg_max_updhel25
real      ,DIMENSION(:,:)     ,POINTER   :: tg_updhel03
real      ,DIMENSION(:,:)     ,POINTER   :: tg_max_updhel03
real      ,DIMENSION(:,:)     ,POINTER   :: tg_total_precip
real      ,DIMENSION(:,:)     ,POINTER   :: tlow
real      ,DIMENSION(:,:)     ,POINTER   :: zlow
real      ,DIMENSION(:,:)     ,POINTER   :: rotangle
real      ,DIMENSION(:)       ,POINTER   :: pstd
real      ,DIMENSION(:,:)     ,POINTER   :: hres_fis
real      ,DIMENSION(:,:)     ,POINTER   :: hres_avc
real      ,DIMENSION(:,:)     ,POINTER   :: hres_lnd
real      ,DIMENSION(:,:)     ,POINTER   :: hbm2
real      ,DIMENSION(:,:)     ,POINTER   :: hbm3
real      ,DIMENSION(:,:)     ,POINTER   :: vbm2
real      ,DIMENSION(:,:)     ,POINTER   :: vbm3
real      ,DIMENSION(:,:)     ,POINTER   :: sm
real      ,DIMENSION(:,:)     ,POINTER   :: sice
real      ,DIMENSION(:,:)     ,POINTER   :: pd
real      ,DIMENSION(:,:,:)   ,POINTER   :: pd_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: pd_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: pd_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: pd_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: pd_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: pd_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: pd_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: pd_btye
real      ,DIMENSION(:,:)     ,POINTER   :: fis
real      ,DIMENSION(:,:)     ,POINTER   :: res
real      ,DIMENSION(:,:,:)   ,POINTER   :: t
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_btye
real      ,DIMENSION(:,:,:)   ,POINTER   :: q
real      ,DIMENSION(:,:,:)   ,POINTER   :: q_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: q_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: q_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: q_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: q_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: q_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: q_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: q_btye
real      ,DIMENSION(:,:)     ,POINTER   :: test_vgrid
real      ,DIMENSION(:,:,:)   ,POINTER   :: u
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: u_btye
real      ,DIMENSION(:,:,:)   ,POINTER   :: v
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: v_btye
real      ,DIMENSION(:,:,:)   ,POINTER   :: told
real      ,DIMENSION(:,:,:)   ,POINTER   :: uold
real      ,DIMENSION(:,:,:)   ,POINTER   :: vold
real      ,DIMENSION(:)       ,POINTER   :: hcoeff
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_pd
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_pint
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_dwdt
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_t
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_q
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_u
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_v
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_q2
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_cwm
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_rrw
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_stc
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_smc
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_sh2o
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_snow
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_snowh
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_canwat
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_nmm_tsk
real      ,DIMENSION(:,:)     ,POINTER   :: dfi_snowc
real      ,DIMENSION(:,:)     ,POINTER   :: dx_nmm
real      ,DIMENSION(:,:)     ,POINTER   :: wpdar
real      ,DIMENSION(:,:)     ,POINTER   :: cpgfu
real      ,DIMENSION(:,:)     ,POINTER   :: curv
real      ,DIMENSION(:,:)     ,POINTER   :: fcp
real      ,DIMENSION(:,:)     ,POINTER   :: fdiv
real      ,DIMENSION(:,:)     ,POINTER   :: f
real      ,DIMENSION(:,:)     ,POINTER   :: fad
real      ,DIMENSION(:,:)     ,POINTER   :: ddmpu
real      ,DIMENSION(:,:)     ,POINTER   :: ddmpv
real      ,DIMENSION(:)       ,POINTER   :: deta
real      ,DIMENSION(:)       ,POINTER   :: rdeta
real      ,DIMENSION(:)       ,POINTER   :: aeta
real      ,DIMENSION(:)       ,POINTER   :: f4q2
real      ,DIMENSION(:)       ,POINTER   :: etax
real      ,DIMENSION(:)       ,POINTER   :: dfl
real      ,DIMENSION(:)       ,POINTER   :: deta1
real      ,DIMENSION(:)       ,POINTER   :: aeta1
real      ,DIMENSION(:)       ,POINTER   :: eta1
real      ,DIMENSION(:)       ,POINTER   :: deta2
real      ,DIMENSION(:)       ,POINTER   :: aeta2
real      ,DIMENSION(:)       ,POINTER   :: eta2
real      ,DIMENSION(:)       ,POINTER   :: em
real      ,DIMENSION(:)       ,POINTER   :: emt
real      ,DIMENSION(:,:)     ,POINTER   :: adt
real      ,DIMENSION(:,:)     ,POINTER   :: adu
real      ,DIMENSION(:,:)     ,POINTER   :: adv
real      ,DIMENSION(:)       ,POINTER   :: em_loc
real      ,DIMENSION(:)       ,POINTER   :: emt_loc
real      ,DIMENSION(:,:)     ,POINTER   :: pdsl
real      ,DIMENSION(:,:)     ,POINTER   :: pdslo
real      ,DIMENSION(:,:)     ,POINTER   :: psdt
real      ,DIMENSION(:,:,:)   ,POINTER   :: div
real      ,DIMENSION(:,:,:)   ,POINTER   :: def3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: few
real      ,DIMENSION(:,:,:)   ,POINTER   :: fne
real      ,DIMENSION(:,:,:)   ,POINTER   :: fns
real      ,DIMENSION(:,:,:)   ,POINTER   :: fse
real      ,DIMENSION(:,:,:)   ,POINTER   :: omgalf
real      ,DIMENSION(:,:,:)   ,POINTER   :: petdt
real      ,DIMENSION(:,:,:)   ,POINTER   :: rtop
real      ,DIMENSION(:,:)     ,POINTER   :: pblh
integer   ,DIMENSION(:,:)     ,POINTER   :: lpbl
real      ,DIMENSION(:,:)     ,POINTER   :: mixht
real      ,DIMENSION(:,:)     ,POINTER   :: ustar
real      ,DIMENSION(:,:)     ,POINTER   :: z0
real      ,DIMENSION(:,:)     ,POINTER   :: mz0
real      ,DIMENSION(:,:)     ,POINTER   :: rc2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: dku3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: dkt3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: dubldt
real      ,DIMENSION(:,:,:)   ,POINTER   :: dvbldt
real      ,DIMENSION(:,:,:)   ,POINTER   :: dupbl
real      ,DIMENSION(:,:,:)   ,POINTER   :: dvpbl
real      ,DIMENSION(:,:)     ,POINTER   :: hpbl2d
real      ,DIMENSION(:,:)     ,POINTER   :: heat2d
real      ,DIMENSION(:,:)     ,POINTER   :: evap2d
real      ,DIMENSION(:,:)     ,POINTER   :: z0base
real      ,DIMENSION(:,:)     ,POINTER   :: ths
real      ,DIMENSION(:,:)     ,POINTER   :: mavail
real      ,DIMENSION(:,:)     ,POINTER   :: qsh
real      ,DIMENSION(:,:)     ,POINTER   :: twbs
real      ,DIMENSION(:,:)     ,POINTER   :: qwbs
real      ,DIMENSION(:,:)     ,POINTER   :: taux
real      ,DIMENSION(:,:)     ,POINTER   :: tauy
real      ,DIMENSION(:,:)     ,POINTER   :: prec
real      ,DIMENSION(:,:)     ,POINTER   :: aprec
real      ,DIMENSION(:,:)     ,POINTER   :: acprec
real      ,DIMENSION(:,:)     ,POINTER   :: cuprec
real      ,DIMENSION(:,:)     ,POINTER   :: lspa
real      ,DIMENSION(:,:)     ,POINTER   :: ddata
real      ,DIMENSION(:,:)     ,POINTER   :: accliq
real      ,DIMENSION(:,:)     ,POINTER   :: sno
real      ,DIMENSION(:,:)     ,POINTER   :: si
real      ,DIMENSION(:,:)     ,POINTER   :: cldefi
real      ,DIMENSION(:,:)     ,POINTER   :: deep
real      ,DIMENSION(:,:)     ,POINTER   :: rf
real      ,DIMENSION(:,:)     ,POINTER   :: th10
real      ,DIMENSION(:,:)     ,POINTER   :: q10
real      ,DIMENSION(:,:)     ,POINTER   :: pshltr
real      ,DIMENSION(:,:)     ,POINTER   :: tshltr
real      ,DIMENSION(:,:)     ,POINTER   :: qshltr
real      ,DIMENSION(:,:,:)   ,POINTER   :: q2
real      ,DIMENSION(:,:,:)   ,POINTER   :: q2_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: q2_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: q2_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: q2_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: q2_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: q2_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: q2_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: q2_btye
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_adj
real      ,DIMENSION(:,:,:)   ,POINTER   :: t_old
real      ,DIMENSION(:,:,:)   ,POINTER   :: zero_3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: w0avg
real      ,DIMENSION(:,:)     ,POINTER   :: akhs_out
real      ,DIMENSION(:,:)     ,POINTER   :: akms_out
real      ,DIMENSION(:,:)     ,POINTER   :: lpi
real      ,DIMENSION(:,:)     ,POINTER   :: cd_out
real      ,DIMENSION(:,:)     ,POINTER   :: ch_out
real      ,DIMENSION(:,:)     ,POINTER   :: albase
real      ,DIMENSION(:,:)     ,POINTER   :: albedo
real      ,DIMENSION(:,:)     ,POINTER   :: cnvbot
real      ,DIMENSION(:,:)     ,POINTER   :: cnvtop
real      ,DIMENSION(:,:)     ,POINTER   :: czen
real      ,DIMENSION(:,:)     ,POINTER   :: czmean
real      ,DIMENSION(:,:)     ,POINTER   :: embck
real      ,DIMENSION(:,:)     ,POINTER   :: epsr
real      ,DIMENSION(:,:)     ,POINTER   :: gffc
real      ,DIMENSION(:,:)     ,POINTER   :: glat
real      ,DIMENSION(:,:)     ,POINTER   :: glon
real      ,DIMENSION(:,:)     ,POINTER   :: nmm_tsk
real      ,DIMENSION(:,:)     ,POINTER   :: hdac
real      ,DIMENSION(:,:)     ,POINTER   :: hdacv
real      ,DIMENSION(:,:)     ,POINTER   :: mxsnal
real      ,DIMENSION(:,:)     ,POINTER   :: radin
real      ,DIMENSION(:,:)     ,POINTER   :: radot
real      ,DIMENSION(:,:)     ,POINTER   :: sigt4
real      ,DIMENSION(:,:)     ,POINTER   :: tg
real      ,DIMENSION(:)       ,POINTER   :: dfrlg
integer   ,DIMENSION(:,:)     ,POINTER   :: lvl
integer   ,DIMENSION(:,:)     ,POINTER   :: k22_deep
integer   ,DIMENSION(:,:)     ,POINTER   :: kbcon_deep
integer   ,DIMENSION(:,:)     ,POINTER   :: ktop_deep
real      ,DIMENSION(:,:)     ,POINTER   :: raincv_a
real      ,DIMENSION(:,:)     ,POINTER   :: raincv_b
real      ,DIMENSION(:,:,:)   ,POINTER   :: gd_cloud
real      ,DIMENSION(:,:,:)   ,POINTER   :: gd_cloud2
real      ,DIMENSION(:,:,:)   ,POINTER   :: gd_cloud_a
real      ,DIMENSION(:,:,:)   ,POINTER   :: gd_cloud2_a
real      ,DIMENSION(:,:,:)   ,POINTER   :: qc_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: qi_cu
real      ,DIMENSION(:,:,:)   ,POINTER   :: gd_cldfr
real      ,DIMENSION(:,:)     ,POINTER   :: acswupt
real      ,DIMENSION(:,:)     ,POINTER   :: acswuptc
real      ,DIMENSION(:,:)     ,POINTER   :: acswdnt
real      ,DIMENSION(:,:)     ,POINTER   :: acswdntc
real      ,DIMENSION(:,:)     ,POINTER   :: acswupb
real      ,DIMENSION(:,:)     ,POINTER   :: acswupbc
real      ,DIMENSION(:,:)     ,POINTER   :: acswdnb
real      ,DIMENSION(:,:)     ,POINTER   :: acswdnbc
real      ,DIMENSION(:,:)     ,POINTER   :: aclwupt
real      ,DIMENSION(:,:)     ,POINTER   :: aclwuptc
real      ,DIMENSION(:,:)     ,POINTER   :: aclwdnt
real      ,DIMENSION(:,:)     ,POINTER   :: aclwdntc
real      ,DIMENSION(:,:)     ,POINTER   :: aclwupb
real      ,DIMENSION(:,:)     ,POINTER   :: aclwupbc
real      ,DIMENSION(:,:)     ,POINTER   :: aclwdnb
real      ,DIMENSION(:,:)     ,POINTER   :: aclwdnbc
real      ,DIMENSION(:,:)     ,POINTER   :: swupt
real      ,DIMENSION(:,:)     ,POINTER   :: swuptc
real      ,DIMENSION(:,:)     ,POINTER   :: swdnt
real      ,DIMENSION(:,:)     ,POINTER   :: swdntc
real      ,DIMENSION(:,:)     ,POINTER   :: swupb
real      ,DIMENSION(:,:)     ,POINTER   :: swupbc
real      ,DIMENSION(:,:)     ,POINTER   :: swdnb
real      ,DIMENSION(:,:)     ,POINTER   :: swdnbc
real      ,DIMENSION(:,:)     ,POINTER   :: lwupt
real      ,DIMENSION(:,:)     ,POINTER   :: lwuptc
real      ,DIMENSION(:,:)     ,POINTER   :: lwdnt
real      ,DIMENSION(:,:)     ,POINTER   :: lwdntc
real      ,DIMENSION(:,:)     ,POINTER   :: lwupb
real      ,DIMENSION(:,:)     ,POINTER   :: lwupbc
real      ,DIMENSION(:,:)     ,POINTER   :: lwdnb
real      ,DIMENSION(:,:)     ,POINTER   :: lwdnbc
real      ,DIMENSION(:,:)     ,POINTER   :: swvisdir
real      ,DIMENSION(:,:)     ,POINTER   :: swvisdif
real      ,DIMENSION(:,:)     ,POINTER   :: swnirdir
real      ,DIMENSION(:,:)     ,POINTER   :: swnirdif
real      ,DIMENSION(:,:,:)   ,POINTER   :: refl_10cm
real      ,DIMENSION(:,:)     ,POINTER   :: refd_max
real      ,DIMENSION(:,:)     ,POINTER   :: qnwfa2d
real      ,DIMENSION(:,:,:)   ,POINTER   :: re_cloud
real      ,DIMENSION(:,:,:)   ,POINTER   :: re_ice
real      ,DIMENSION(:,:,:)   ,POINTER   :: re_snow
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_re_cloud
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_re_ice
real      ,DIMENSION(:,:,:)   ,POINTER   :: dfi_re_snow
real      ,DIMENSION(:,:)     ,POINTER   :: swddir
real      ,DIMENSION(:,:)     ,POINTER   :: swddni
real      ,DIMENSION(:,:)     ,POINTER   :: swddif
real      ,DIMENSION(:,:)     ,POINTER   :: gx
real      ,DIMENSION(:,:)     ,POINTER   :: bx
real      ,DIMENSION(:,:)     ,POINTER   :: gg
real      ,DIMENSION(:,:)     ,POINTER   :: bb
real      ,DIMENSION(:,:)     ,POINTER   :: coszen_ref
real      ,DIMENSION(:,:)     ,POINTER   :: coszen
real      ,DIMENSION(:,:)     ,POINTER   :: hrang
real      ,DIMENSION(:,:)     ,POINTER   :: swdown_ref
real      ,DIMENSION(:,:)     ,POINTER   :: swddir_ref
real      ,DIMENSION(:,:,:)   ,POINTER   :: cwm
real      ,DIMENSION(:,:,:)   ,POINTER   :: cwm_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: cwm_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: cwm_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: cwm_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: cwm_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: cwm_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: cwm_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: cwm_btye
real      ,DIMENSION(:,:,:)   ,POINTER   :: rrw
real      ,DIMENSION(:,:,:)   ,POINTER   :: rrw_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: rrw_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: rrw_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: rrw_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: rrw_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: rrw_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: rrw_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: rrw_btye
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_ice
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_rain
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_rimef
real      ,DIMENSION(:,:,:)   ,POINTER   :: cldfra
real      ,DIMENSION(:,:)     ,POINTER   :: sr
real      ,DIMENSION(:,:)     ,POINTER   :: cfrach
real      ,DIMENSION(:,:)     ,POINTER   :: cfracl
real      ,DIMENSION(:,:)     ,POINTER   :: cfracm
integer   ,DIMENSION(:,:)     ,POINTER   :: islope
real      ,DIMENSION(:)       ,POINTER   :: dzsoil
real      ,DIMENSION(:)       ,POINTER   :: rtdpth
real      ,DIMENSION(:)       ,POINTER   :: sldpth
real      ,DIMENSION(:,:)     ,POINTER   :: cmc
real      ,DIMENSION(:,:)     ,POINTER   :: grnflx
real      ,DIMENSION(:,:)     ,POINTER   :: pctsno
real      ,DIMENSION(:,:)     ,POINTER   :: soiltb
real      ,DIMENSION(:,:)     ,POINTER   :: vegfrc
real      ,DIMENSION(:,:)     ,POINTER   :: shdmax
real      ,DIMENSION(:,:)     ,POINTER   :: shdmin
real      ,DIMENSION(:,:,:)   ,POINTER   :: sh2o
real      ,DIMENSION(:,:,:)   ,POINTER   :: smc
real      ,DIMENSION(:,:,:)   ,POINTER   :: stc
real      ,DIMENSION(:,:)     ,POINTER   :: hstdv
real      ,DIMENSION(:,:)     ,POINTER   :: hcnvx
real      ,DIMENSION(:,:)     ,POINTER   :: hasyw
real      ,DIMENSION(:,:)     ,POINTER   :: hasys
real      ,DIMENSION(:,:)     ,POINTER   :: hasysw
real      ,DIMENSION(:,:)     ,POINTER   :: hasynw
real      ,DIMENSION(:,:)     ,POINTER   :: hlenw
real      ,DIMENSION(:,:)     ,POINTER   :: hlens
real      ,DIMENSION(:,:)     ,POINTER   :: hlensw
real      ,DIMENSION(:,:)     ,POINTER   :: hlennw
real      ,DIMENSION(:,:)     ,POINTER   :: hangl
real      ,DIMENSION(:,:)     ,POINTER   :: hanis
real      ,DIMENSION(:,:)     ,POINTER   :: hslop
real      ,DIMENSION(:,:)     ,POINTER   :: hzmax
real      ,DIMENSION(:,:)     ,POINTER   :: crot
real      ,DIMENSION(:,:)     ,POINTER   :: srot
real      ,DIMENSION(:,:)     ,POINTER   :: ugwdsfc
real      ,DIMENSION(:,:)     ,POINTER   :: vgwdsfc
real      ,DIMENSION(:,:)     ,POINTER   :: ctopo
real      ,DIMENSION(:,:)     ,POINTER   :: ctopo2
real      ,DIMENSION(:,:)     ,POINTER   :: dwdtmn
real      ,DIMENSION(:,:)     ,POINTER   :: dwdtmx
real      ,DIMENSION(:,:)     ,POINTER   :: baro
real      ,DIMENSION(:,:,:)   ,POINTER   :: dwdt
real      ,DIMENSION(:,:,:)   ,POINTER   :: pdwdt
real      ,DIMENSION(:,:,:)   ,POINTER   :: pint
real      ,DIMENSION(:,:,:)   ,POINTER   :: w
real      ,DIMENSION(:,:,:)   ,POINTER   :: w_tot
real      ,DIMENSION(:,:,:)   ,POINTER   :: z
real      ,DIMENSION(:,:)     ,POINTER   :: acfrcv
real      ,DIMENSION(:,:)     ,POINTER   :: acfrst
real      ,DIMENSION(:,:)     ,POINTER   :: ssroff
real      ,DIMENSION(:,:)     ,POINTER   :: bgroff
real      ,DIMENSION(:,:)     ,POINTER   :: rlwin
real      ,DIMENSION(:,:)     ,POINTER   :: rlwout
real      ,DIMENSION(:,:)     ,POINTER   :: rlwtoa
real      ,DIMENSION(:,:)     ,POINTER   :: alwin
real      ,DIMENSION(:,:)     ,POINTER   :: alwout
real      ,DIMENSION(:,:)     ,POINTER   :: alwtoa
real      ,DIMENSION(:,:)     ,POINTER   :: rswin
real      ,DIMENSION(:,:)     ,POINTER   :: rswinc
real      ,DIMENSION(:,:)     ,POINTER   :: rswout
real      ,DIMENSION(:,:)     ,POINTER   :: rswtoa
real      ,DIMENSION(:,:)     ,POINTER   :: aswin
real      ,DIMENSION(:,:)     ,POINTER   :: aswout
real      ,DIMENSION(:,:)     ,POINTER   :: aswtoa
real      ,DIMENSION(:,:)     ,POINTER   :: sfcshx
real      ,DIMENSION(:,:)     ,POINTER   :: sfclhx
real      ,DIMENSION(:,:)     ,POINTER   :: subshx
real      ,DIMENSION(:,:)     ,POINTER   :: snopcx
real      ,DIMENSION(:,:)     ,POINTER   :: sfcuvx
real      ,DIMENSION(:,:)     ,POINTER   :: potevp
real      ,DIMENSION(:,:)     ,POINTER   :: potflx
real      ,DIMENSION(:,:)     ,POINTER   :: tlmin
real      ,DIMENSION(:,:)     ,POINTER   :: tlmax
real      ,DIMENSION(:,:)     ,POINTER   :: t02_min
real      ,DIMENSION(:,:)     ,POINTER   :: t02_max
real      ,DIMENSION(:,:)     ,POINTER   :: rh02_min
real      ,DIMENSION(:,:)     ,POINTER   :: rh02_max
real      ,DIMENSION(:,:,:)   ,POINTER   :: rlwtt
real      ,DIMENSION(:,:,:)   ,POINTER   :: rswtt
real      ,DIMENSION(:,:,:)   ,POINTER   :: tcucn
real      ,DIMENSION(:,:,:)   ,POINTER   :: train
integer   ,DIMENSION(:,:)     ,POINTER   :: ncfrcv
integer   ,DIMENSION(:,:)     ,POINTER   :: ncfrst
integer   ,DIMENSION(:)       ,POINTER   :: ihe
integer   ,DIMENSION(:)       ,POINTER   :: ihw
integer   ,DIMENSION(:)       ,POINTER   :: ive
integer   ,DIMENSION(:)       ,POINTER   :: ivw
integer   ,DIMENSION(:)       ,POINTER   :: irad
integer   ,DIMENSION(:)       ,POINTER   :: iheg
integer   ,DIMENSION(:)       ,POINTER   :: ihwg
integer   ,DIMENSION(:)       ,POINTER   :: iveg
integer   ,DIMENSION(:)       ,POINTER   :: ivwg
integer   ,DIMENSION(:)       ,POINTER   :: iradg
integer   ,DIMENSION(:)       ,POINTER   :: n_iup_h
integer   ,DIMENSION(:)       ,POINTER   :: n_iup_v
integer   ,DIMENSION(:)       ,POINTER   :: n_iup_adh
integer   ,DIMENSION(:)       ,POINTER   :: n_iup_adv
integer   ,DIMENSION(:,:)     ,POINTER   :: iup_h
integer   ,DIMENSION(:,:)     ,POINTER   :: iup_v
integer   ,DIMENSION(:,:)     ,POINTER   :: iup_adh
integer   ,DIMENSION(:,:)     ,POINTER   :: iup_adv
real      ,DIMENSION(:,:,:)   ,POINTER   :: winfo
real      ,DIMENSION(:,:,:)   ,POINTER   :: winfo_bxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: winfo_bxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: winfo_bys
real      ,DIMENSION(:,:,:)   ,POINTER   :: winfo_bye
real      ,DIMENSION(:,:,:)   ,POINTER   :: winfo_btxs
real      ,DIMENSION(:,:,:)   ,POINTER   :: winfo_btxe
real      ,DIMENSION(:,:,:)   ,POINTER   :: winfo_btys
real      ,DIMENSION(:,:,:)   ,POINTER   :: winfo_btye
integer   ,DIMENSION(:,:,:)   ,POINTER   :: iinfo
integer   ,DIMENSION(:,:,:)   ,POINTER   :: iinfo_bxs
integer   ,DIMENSION(:,:,:)   ,POINTER   :: iinfo_bxe
integer   ,DIMENSION(:,:,:)   ,POINTER   :: iinfo_bys
integer   ,DIMENSION(:,:,:)   ,POINTER   :: iinfo_bye
integer   ,DIMENSION(:,:,:)   ,POINTER   :: iinfo_btxs
integer   ,DIMENSION(:,:,:)   ,POINTER   :: iinfo_btxe
integer   ,DIMENSION(:,:,:)   ,POINTER   :: iinfo_btys
integer   ,DIMENSION(:,:,:)   ,POINTER   :: iinfo_btye
integer   ,DIMENSION(:,:)     ,POINTER   :: imask_nostag
integer   ,DIMENSION(:,:)     ,POINTER   :: imask_xstag
integer   ,DIMENSION(:,:)     ,POINTER   :: imask_ystag
integer   ,DIMENSION(:,:)     ,POINTER   :: imask_xystag
real      ,DIMENSION(:,:)     ,POINTER   :: sm000007
real      ,DIMENSION(:,:)     ,POINTER   :: sm007028
real      ,DIMENSION(:,:)     ,POINTER   :: sm028100
real      ,DIMENSION(:,:)     ,POINTER   :: sm100255
real      ,DIMENSION(:,:)     ,POINTER   :: st000007
real      ,DIMENSION(:,:)     ,POINTER   :: st007028
real      ,DIMENSION(:,:)     ,POINTER   :: st028100
real      ,DIMENSION(:,:)     ,POINTER   :: st100255
real      ,DIMENSION(:,:)     ,POINTER   :: sm000010
real      ,DIMENSION(:,:)     ,POINTER   :: sm010040
real      ,DIMENSION(:,:)     ,POINTER   :: sm040100
real      ,DIMENSION(:,:)     ,POINTER   :: sm100200
real      ,DIMENSION(:,:)     ,POINTER   :: sm010200
real      ,DIMENSION(:,:)     ,POINTER   :: soilm000
real      ,DIMENSION(:,:)     ,POINTER   :: soilm005
real      ,DIMENSION(:,:)     ,POINTER   :: soilm020
real      ,DIMENSION(:,:)     ,POINTER   :: soilm040
real      ,DIMENSION(:,:)     ,POINTER   :: soilm160
real      ,DIMENSION(:,:)     ,POINTER   :: soilm300
real      ,DIMENSION(:,:)     ,POINTER   :: sw000010
real      ,DIMENSION(:,:)     ,POINTER   :: sw010040
real      ,DIMENSION(:,:)     ,POINTER   :: sw040100
real      ,DIMENSION(:,:)     ,POINTER   :: sw100200
real      ,DIMENSION(:,:)     ,POINTER   :: sw010200
real      ,DIMENSION(:,:)     ,POINTER   :: soilw000
real      ,DIMENSION(:,:)     ,POINTER   :: soilw005
real      ,DIMENSION(:,:)     ,POINTER   :: soilw020
real      ,DIMENSION(:,:)     ,POINTER   :: soilw040
real      ,DIMENSION(:,:)     ,POINTER   :: soilw160
real      ,DIMENSION(:,:)     ,POINTER   :: soilw300
real      ,DIMENSION(:,:)     ,POINTER   :: st000010
real      ,DIMENSION(:,:)     ,POINTER   :: st010040
real      ,DIMENSION(:,:)     ,POINTER   :: st040100
real      ,DIMENSION(:,:)     ,POINTER   :: st100200
real      ,DIMENSION(:,:)     ,POINTER   :: st010200
real      ,DIMENSION(:,:)     ,POINTER   :: soilt000
real      ,DIMENSION(:,:)     ,POINTER   :: soilt005
real      ,DIMENSION(:,:)     ,POINTER   :: soilt020
real      ,DIMENSION(:,:)     ,POINTER   :: soilt040
real      ,DIMENSION(:,:)     ,POINTER   :: soilt160
real      ,DIMENSION(:,:)     ,POINTER   :: soilt300
real      ,DIMENSION(:,:)     ,POINTER   :: landmask
real      ,DIMENSION(:,:)     ,POINTER   :: topostdv
real      ,DIMENSION(:,:)     ,POINTER   :: toposlpx
real      ,DIMENSION(:,:)     ,POINTER   :: toposlpy
real      ,DIMENSION(:,:)     ,POINTER   :: greenmax
real      ,DIMENSION(:,:)     ,POINTER   :: greenmin
real      ,DIMENSION(:,:)     ,POINTER   :: albedomx
real      ,DIMENSION(:,:)     ,POINTER   :: slopecat
real      ,DIMENSION(:,:)     ,POINTER   :: toposoil
real      ,DIMENSION(:,:,:)   ,POINTER   :: landusef
real      ,DIMENSION(:,:,:)   ,POINTER   :: soilctop
real      ,DIMENSION(:,:,:)   ,POINTER   :: soilcbot
real      ,DIMENSION(:,:)     ,POINTER   :: ts_hour
real      ,DIMENSION(:,:)     ,POINTER   :: ts_u
real      ,DIMENSION(:,:)     ,POINTER   :: ts_v
real      ,DIMENSION(:,:)     ,POINTER   :: ts_q
real      ,DIMENSION(:,:)     ,POINTER   :: ts_t
real      ,DIMENSION(:,:)     ,POINTER   :: ts_psfc
real      ,DIMENSION(:,:)     ,POINTER   :: ts_tsk
real      ,DIMENSION(:,:)     ,POINTER   :: ts_tslb
real      ,DIMENSION(:,:)     ,POINTER   :: ts_clw
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist_bxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist_bxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist_bys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist_bye
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist_btxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist_btxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist_btys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: moist_btye
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_moist
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_moist_bxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_moist_bxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_moist_bys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_moist_bye
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_moist_btxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_moist_btxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_moist_btys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_moist_btye
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_bxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_bxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_bys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_bye
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_btxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_btxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_btys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: scalar_btye
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_bxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_bxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_bys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_bye
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_btxs
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_btxe
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_btys
real      ,DIMENSION(:,:,:,:) ,POINTER   :: dfi_scalar_btye
real      ,DIMENSION(:,:,:,:) ,POINTER   :: chem
real      ,DIMENSION(:,:,:)   ,POINTER   :: smois
real      ,DIMENSION(:,:,:)   ,POINTER   :: tslb
real      ,DIMENSION(:,:)     ,POINTER   :: lake_depth
real      ,DIMENSION(:,:)     ,POINTER   :: gsw
real      ,DIMENSION(:,:)     ,POINTER   :: xland
real      ,DIMENSION(:,:)     ,POINTER   :: raincv
real      ,DIMENSION(:,:)     ,POINTER   :: psfc
real      ,DIMENSION(:,:)     ,POINTER   :: th2
real      ,DIMENSION(:,:)     ,POINTER   :: t2
real      ,DIMENSION(:,:)     ,POINTER   :: u10
real      ,DIMENSION(:,:)     ,POINTER   :: v10
real      ,DIMENSION(:,:)     ,POINTER   :: xice
real      ,DIMENSION(:,:)     ,POINTER   :: icedepth
real      ,DIMENSION(:,:)     ,POINTER   :: albsi
real      ,DIMENSION(:,:)     ,POINTER   :: snowsi
real      ,DIMENSION(:,:)     ,POINTER   :: lai
real      ,DIMENSION(:,:)     ,POINTER   :: smstav
real      ,DIMENSION(:,:)     ,POINTER   :: smstot
real      ,DIMENSION(:,:)     ,POINTER   :: soldrain
real      ,DIMENSION(:,:)     ,POINTER   :: sfcheadrt
real      ,DIMENSION(:,:)     ,POINTER   :: infxsrt
real      ,DIMENSION(:,:)     ,POINTER   :: sfcrunoff
real      ,DIMENSION(:,:)     ,POINTER   :: udrunoff
integer   ,DIMENSION(:,:)     ,POINTER   :: ivgtyp
integer   ,DIMENSION(:,:)     ,POINTER   :: isltyp
real      ,DIMENSION(:,:)     ,POINTER   :: vegfra
real      ,DIMENSION(:,:)     ,POINTER   :: sfcevp
real      ,DIMENSION(:,:)     ,POINTER   :: grdflx
real      ,DIMENSION(:,:)     ,POINTER   :: albbck
real      ,DIMENSION(:,:)     ,POINTER   :: sfcexc
real      ,DIMENSION(:,:)     ,POINTER   :: snotime
real      ,DIMENSION(:,:)     ,POINTER   :: acsnow
real      ,DIMENSION(:,:)     ,POINTER   :: acsnom
real      ,DIMENSION(:,:)     ,POINTER   :: rmol
real      ,DIMENSION(:,:)     ,POINTER   :: snow
real      ,DIMENSION(:,:)     ,POINTER   :: canwat
integer   ,DIMENSION(:)       ,POINTER   :: force_sst
real      ,DIMENSION(:,:)     ,POINTER   :: sst
real      ,DIMENSION(:,:)     ,POINTER   :: uoce
real      ,DIMENSION(:,:)     ,POINTER   :: voce
real      ,DIMENSION(:,:)     ,POINTER   :: weasd
real      ,DIMENSION(:,:)     ,POINTER   :: znt
real      ,DIMENSION(:,:)     ,POINTER   :: mol
real      ,DIMENSION(:,:)     ,POINTER   :: noahres
real      ,DIMENSION(:,:,:)   ,POINTER   :: tke_pbl
real      ,DIMENSION(:,:,:)   ,POINTER   :: el_pbl
real      ,DIMENSION(:,:,:)   ,POINTER   :: exch_h
real      ,DIMENSION(:,:,:)   ,POINTER   :: exch_m
real      ,DIMENSION(:,:)     ,POINTER   :: thz0
real      ,DIMENSION(:,:)     ,POINTER   :: qz0
real      ,DIMENSION(:,:)     ,POINTER   :: uz0
real      ,DIMENSION(:,:)     ,POINTER   :: vz0
real      ,DIMENSION(:,:)     ,POINTER   :: flhc
real      ,DIMENSION(:,:)     ,POINTER   :: flqc
real      ,DIMENSION(:,:)     ,POINTER   :: qsg
real      ,DIMENSION(:,:)     ,POINTER   :: qvg
real      ,DIMENSION(:,:)     ,POINTER   :: qcg
real      ,DIMENSION(:,:)     ,POINTER   :: dew
real      ,DIMENSION(:,:)     ,POINTER   :: soilt1
real      ,DIMENSION(:,:)     ,POINTER   :: tsnav
real      ,DIMENSION(:,:)     ,POINTER   :: psfc_out
real      ,DIMENSION(:,:)     ,POINTER   :: uz0h
real      ,DIMENSION(:,:)     ,POINTER   :: vz0h
real      ,DIMENSION(:,:,:)   ,POINTER   :: dudt
real      ,DIMENSION(:,:,:)   ,POINTER   :: dvdt
real      ,DIMENSION(:,:)     ,POINTER   :: qsfc
real      ,DIMENSION(:,:)     ,POINTER   :: akhs
real      ,DIMENSION(:,:)     ,POINTER   :: akms
real      ,DIMENSION(:,:)     ,POINTER   :: htop
real      ,DIMENSION(:,:)     ,POINTER   :: hbot
real      ,DIMENSION(:,:)     ,POINTER   :: htopr
real      ,DIMENSION(:,:)     ,POINTER   :: hbotr
real      ,DIMENSION(:,:)     ,POINTER   :: htopd
real      ,DIMENSION(:,:)     ,POINTER   :: hbotd
real      ,DIMENSION(:,:)     ,POINTER   :: htops
real      ,DIMENSION(:,:)     ,POINTER   :: hbots
real      ,DIMENSION(:,:)     ,POINTER   :: cuppt
real      ,DIMENSION(:,:)     ,POINTER   :: cprate
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_ice_phy
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_rain_phy
real      ,DIMENSION(:,:,:)   ,POINTER   :: f_rimef_phy
real      ,DIMENSION(:,:)     ,POINTER   :: mass_flux
real      ,DIMENSION(:,:)     ,POINTER   :: apr_gr
real      ,DIMENSION(:,:)     ,POINTER   :: apr_w
real      ,DIMENSION(:,:)     ,POINTER   :: apr_mc
real      ,DIMENSION(:,:)     ,POINTER   :: apr_st
real      ,DIMENSION(:,:)     ,POINTER   :: apr_as
real      ,DIMENSION(:,:)     ,POINTER   :: apr_capma
real      ,DIMENSION(:,:)     ,POINTER   :: apr_capme
real      ,DIMENSION(:,:)     ,POINTER   :: apr_capmi
real      ,DIMENSION(:,:,:)   ,POINTER   :: xf_ens
real      ,DIMENSION(:,:,:)   ,POINTER   :: pr_ens
real      ,DIMENSION(:,:,:)   ,POINTER   :: rthften
real      ,DIMENSION(:,:,:)   ,POINTER   :: rqvften
real      ,DIMENSION(:,:)     ,POINTER   :: snowh
real      ,DIMENSION(:,:)     ,POINTER   :: rhosn
real      ,DIMENSION(:,:,:)   ,POINTER   :: smfr3d
real      ,DIMENSION(:,:,:)   ,POINTER   :: keepfr3dflag
real      ,DIMENSION(:,:)     ,POINTER   :: rhosnf
real      ,DIMENSION(:,:)     ,POINTER   :: snowfallac
real      ,DIMENSION(:,:)     ,POINTER   :: precipfr
real      ,DIMENSION(:,:,:)   ,POINTER   :: rc_mf
real      ,DIMENSION(:,:)     ,POINTER   :: flx4
real      ,DIMENSION(:,:)     ,POINTER   :: fvb
real      ,DIMENSION(:,:)     ,POINTER   :: fbur
real      ,DIMENSION(:,:)     ,POINTER   :: fgsn
integer   ,DIMENSION(:,:)     ,POINTER   :: isnowxy
real      ,DIMENSION(:,:)     ,POINTER   :: tvxy
real      ,DIMENSION(:,:)     ,POINTER   :: tgxy
real      ,DIMENSION(:,:)     ,POINTER   :: canicexy
real      ,DIMENSION(:,:)     ,POINTER   :: canliqxy
real      ,DIMENSION(:,:)     ,POINTER   :: eahxy
real      ,DIMENSION(:,:)     ,POINTER   :: tahxy
real      ,DIMENSION(:,:)     ,POINTER   :: cmxy
real      ,DIMENSION(:,:)     ,POINTER   :: chxy
real      ,DIMENSION(:,:)     ,POINTER   :: fwetxy
real      ,DIMENSION(:,:)     ,POINTER   :: sneqvoxy
real      ,DIMENSION(:,:)     ,POINTER   :: alboldxy
real      ,DIMENSION(:,:)     ,POINTER   :: qsnowxy
real      ,DIMENSION(:,:)     ,POINTER   :: wslakexy
real      ,DIMENSION(:,:)     ,POINTER   :: zwtxy
real      ,DIMENSION(:,:)     ,POINTER   :: waxy
real      ,DIMENSION(:,:)     ,POINTER   :: wtxy
real      ,DIMENSION(:,:,:)   ,POINTER   :: tsnoxy
real      ,DIMENSION(:,:,:)   ,POINTER   :: zsnsoxy
real      ,DIMENSION(:,:,:)   ,POINTER   :: snicexy
real      ,DIMENSION(:,:,:)   ,POINTER   :: snliqxy
real      ,DIMENSION(:,:)     ,POINTER   :: lfmassxy
real      ,DIMENSION(:,:)     ,POINTER   :: rtmassxy
real      ,DIMENSION(:,:)     ,POINTER   :: stmassxy
real      ,DIMENSION(:,:)     ,POINTER   :: woodxy
real      ,DIMENSION(:,:)     ,POINTER   :: stblcpxy
real      ,DIMENSION(:,:)     ,POINTER   :: fastcpxy
real      ,DIMENSION(:,:)     ,POINTER   :: xsaixy
real      ,DIMENSION(:,:)     ,POINTER   :: taussxy
real      ,DIMENSION(:,:)     ,POINTER   :: t2mvxy
real      ,DIMENSION(:,:)     ,POINTER   :: t2mbxy
real      ,DIMENSION(:,:)     ,POINTER   :: q2mvxy
real      ,DIMENSION(:,:)     ,POINTER   :: q2mbxy
real      ,DIMENSION(:,:)     ,POINTER   :: tradxy
real      ,DIMENSION(:,:)     ,POINTER   :: neexy
real      ,DIMENSION(:,:)     ,POINTER   :: gppxy
real      ,DIMENSION(:,:)     ,POINTER   :: nppxy
real      ,DIMENSION(:,:)     ,POINTER   :: fvegxy
real      ,DIMENSION(:,:)     ,POINTER   :: qinxy
real      ,DIMENSION(:,:)     ,POINTER   :: runsfxy
real      ,DIMENSION(:,:)     ,POINTER   :: runsbxy
real      ,DIMENSION(:,:)     ,POINTER   :: ecanxy
real      ,DIMENSION(:,:)     ,POINTER   :: edirxy
real      ,DIMENSION(:,:)     ,POINTER   :: etranxy
real      ,DIMENSION(:,:)     ,POINTER   :: fsaxy
real      ,DIMENSION(:,:)     ,POINTER   :: firaxy
real      ,DIMENSION(:,:)     ,POINTER   :: aparxy
real      ,DIMENSION(:,:)     ,POINTER   :: psnxy
real      ,DIMENSION(:,:)     ,POINTER   :: savxy
real      ,DIMENSION(:,:)     ,POINTER   :: sagxy
real      ,DIMENSION(:,:)     ,POINTER   :: rssunxy
real      ,DIMENSION(:,:)     ,POINTER   :: rsshaxy
real      ,DIMENSION(:,:)     ,POINTER   :: bgapxy
real      ,DIMENSION(:,:)     ,POINTER   :: wgapxy
real      ,DIMENSION(:,:)     ,POINTER   :: tgvxy
real      ,DIMENSION(:,:)     ,POINTER   :: tgbxy
real      ,DIMENSION(:,:)     ,POINTER   :: chvxy
real      ,DIMENSION(:,:)     ,POINTER   :: chbxy
real      ,DIMENSION(:,:)     ,POINTER   :: shgxy
real      ,DIMENSION(:,:)     ,POINTER   :: shcxy
real      ,DIMENSION(:,:)     ,POINTER   :: shbxy
real      ,DIMENSION(:,:)     ,POINTER   :: evgxy
real      ,DIMENSION(:,:)     ,POINTER   :: evbxy
real      ,DIMENSION(:,:)     ,POINTER   :: ghvxy
real      ,DIMENSION(:,:)     ,POINTER   :: ghbxy
real      ,DIMENSION(:,:)     ,POINTER   :: irgxy
real      ,DIMENSION(:,:)     ,POINTER   :: ircxy
real      ,DIMENSION(:,:)     ,POINTER   :: irbxy
real      ,DIMENSION(:,:)     ,POINTER   :: trxy
real      ,DIMENSION(:,:)     ,POINTER   :: evcxy
real      ,DIMENSION(:,:)     ,POINTER   :: chleafxy
real      ,DIMENSION(:,:)     ,POINTER   :: chucxy
real      ,DIMENSION(:,:)     ,POINTER   :: chv2xy
real      ,DIMENSION(:,:)     ,POINTER   :: chb2xy
real      ,DIMENSION(:,:)     ,POINTER   :: chstarxy
real      ,DIMENSION(:,:,:)   ,POINTER   :: smoiseq
real      ,DIMENSION(:,:)     ,POINTER   :: smcwtdxy
real      ,DIMENSION(:,:)     ,POINTER   :: rechxy
real      ,DIMENSION(:,:)     ,POINTER   :: deeprechxy
real      ,DIMENSION(:)       ,POINTER   :: mp_restart_state
real      ,DIMENSION(:)       ,POINTER   :: tbpvs_state
real      ,DIMENSION(:)       ,POINTER   :: tbpvs0_state
real      ,DIMENSION(:)       ,POINTER   :: lu_state
real      ,DIMENSION(:,:)     ,POINTER   :: power
real      ,DIMENSION(:,:,:,:) ,POINTER   :: ozmixm
real      ,DIMENSION(:)       ,POINTER   :: pin
real      ,DIMENSION(:,:,:)   ,POINTER   :: o3rad
!ENDOFREGISTRYGENERATEDINCLUDE
