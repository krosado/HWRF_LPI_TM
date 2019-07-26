!STARTOFREGISTRYGENERATEDINCLUDE 'inc/dummy_decl.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
integer                                  :: lakeflag
integer                                  :: lake_depth_flag
real                                     :: dtc
real                                     :: guessdtc
logical                                  :: update_interest
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
real                                     :: wbd0var
real                                     :: sbd0var
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
integer                                  :: auxhist1_oid
integer                                  :: auxhist2_oid
integer                                  :: auxhist3_oid
integer                                  :: auxhist4_oid
integer                                  :: auxhist5_oid
integer                                  :: auxhist6_oid
integer                                  :: auxhist7_oid
integer                                  :: auxhist8_oid
integer                                  :: auxhist9_oid
integer                                  :: auxhist10_oid
integer                                  :: auxhist11_oid
integer                                  :: auxhist12_oid
integer                                  :: auxhist13_oid
integer                                  :: auxhist14_oid
integer                                  :: auxhist15_oid
integer                                  :: auxhist16_oid
integer                                  :: auxhist17_oid
integer                                  :: auxhist18_oid
integer                                  :: auxhist19_oid
integer                                  :: auxhist20_oid
integer                                  :: auxhist21_oid
integer                                  :: auxhist22_oid
integer                                  :: auxhist23_oid
integer                                  :: auxhist24_oid
integer                                  :: auxinput1_oid
integer                                  :: auxinput2_oid
integer                                  :: auxinput3_oid
integer                                  :: auxinput4_oid
integer                                  :: auxinput5_oid
integer                                  :: auxinput6_oid
integer                                  :: auxinput7_oid
integer                                  :: auxinput8_oid
integer                                  :: auxinput9_oid
integer                                  :: auxinput10_oid
integer                                  :: auxinput11_oid
integer                                  :: auxinput12_oid
integer                                  :: auxinput13_oid
integer                                  :: auxinput14_oid
integer                                  :: auxinput15_oid
integer                                  :: auxinput16_oid
integer                                  :: auxinput17_oid
integer                                  :: auxinput18_oid
integer                                  :: auxinput19_oid
integer                                  :: auxinput20_oid
integer                                  :: auxinput21_oid
integer                                  :: auxinput22_oid
integer                                  :: auxinput23_oid
integer                                  :: auxinput24_oid
integer                                  :: oid
integer                                  :: nodyn_dummy
logical   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lake2d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lakedepth2d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: savedtke12d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: snowdp2d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: h2osno2d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: snl2d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: t_grnd2d
real      ,DIMENSION(grid%sm31:grid%em31,1:10,grid%sm32:grid%em32)           :: t_lake3d
real      ,DIMENSION(grid%sm31:grid%em31,1:10,grid%sm32:grid%em32)           :: lake_icefrac3d
real      ,DIMENSION(grid%sm31:grid%em31,1:10,grid%sm32:grid%em32)           :: z_lake3d
real      ,DIMENSION(grid%sm31:grid%em31,1:10,grid%sm32:grid%em32)           :: dz_lake3d
real      ,DIMENSION(grid%sm31:grid%em31,1:15,grid%sm32:grid%em32)           :: t_soisno3d
real      ,DIMENSION(grid%sm31:grid%em31,1:15,grid%sm32:grid%em32)           :: h2osoi_ice3d
real      ,DIMENSION(grid%sm31:grid%em31,1:15,grid%sm32:grid%em32)           :: h2osoi_liq3d
real      ,DIMENSION(grid%sm31:grid%em31,1:15,grid%sm32:grid%em32)           :: h2osoi_vol3d
real      ,DIMENSION(grid%sm31:grid%em31,1:15,grid%sm32:grid%em32)           :: z3d
real      ,DIMENSION(grid%sm31:grid%em31,1:15,grid%sm32:grid%em32)           :: dz3d
real      ,DIMENSION(grid%sm31:grid%em31,1:16,grid%sm32:grid%em32)           :: zi3d
real      ,DIMENSION(grid%sm31:grid%em31,1:10,grid%sm32:grid%em32)           :: watsat3d
real      ,DIMENSION(grid%sm31:grid%em31,1:10,grid%sm32:grid%em32)           :: csol3d
real      ,DIMENSION(grid%sm31:grid%em31,1:10,grid%sm32:grid%em32)           :: tkmg3d
real      ,DIMENSION(grid%sm31:grid%em31,1:10,grid%sm32:grid%em32)           :: tkdry3d
real      ,DIMENSION(grid%sm31:grid%em31,1:10,grid%sm32:grid%em32)           :: tksatu3d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_szj)           :: szj
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_s1z)           :: s1z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_spz)           :: spz
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tcs)           :: tcs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lu_index
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lu_mask
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: p_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vegcat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilcat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: input_soil_cat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tsk_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: xice_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: ght_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: rh_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: v_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: u_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_metgrid_levels)           :: t_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: snoalb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:12)           :: greenfrac_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:12)           :: albedo12m_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:12)           :: lai12m_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_soil_cat)           :: soilcbot_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_soil_cat)           :: soilctop_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tmn_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: htv_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ht_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%num_land_cat)           :: landusef_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vlon_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vlat_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hlon_gc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hlat_gc
integer   ,DIMENSION(grid%sm33:grid%em33)           :: nrnd1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: relaxwork
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: relaximask
logical   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: relaxmask
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: interesting
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: precip_swath
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: windsq_swath
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tracker_distsq
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tracker_angle
real      ,DIMENSION(1:grid%num_old_fixes)           :: track_old_lon
real      ,DIMENSION(1:grid%num_old_fixes)           :: track_old_lat
integer   ,DIMENSION(1:grid%num_old_fixes)           :: track_old_ntsd
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tracker_fixes
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: membrane_mslp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: p850rv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: p700rv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: p850wind
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: p700wind
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: p500u
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: p500v
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: p700u
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: p700v
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: p850u
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: p850v
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: p850z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: p700z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: m10wind
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: m10rv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sp850rv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sp700rv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sp850wind
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sp700wind
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sp850z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sp700z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm10wind
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm10rv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: smslp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: distsq
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: weightout
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: mslp_noisy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: pdyn_smooth
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: pdyn_parent
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: pdyn
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: mslp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: best_mslp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sqws
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: ducudt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dvcudt
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: randstate1
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: randstate2
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: randstate3
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: randstate4
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: random
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: iih
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: jjh
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: iiv
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: jjv
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hnear_i
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hnear_j
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hbwgt1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hbwgt2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hbwgt3
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hbwgt4
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vbwgt1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vbwgt2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vbwgt3
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vbwgt4
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hlon
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hlat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vlon
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vlat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tg_max_m10wind
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tg_max_wwind
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tg_min_wwind
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tg_max_zhel_25
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tg_min_zhel_25
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tg_max_zhel_03
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tg_min_zhel_03
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tg_updhel25
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tg_max_updhel25
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tg_updhel03
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tg_max_updhel03
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tg_total_precip
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tlow
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: zlow
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rotangle
real      ,DIMENSION(grid%sm33:grid%em33)           :: pstd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hres_fis
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hres_avc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hres_lnd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hbm2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hbm3
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vbm2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vbm3
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sice
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: pd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: fis
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: res
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: q
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: test_vgrid
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: u
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: v
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: told
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: uold
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: vold
real      ,DIMENSION(1:grid%dfi_time_dim)           :: hcoeff
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: dfi_pd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dfi_pint
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dfi_dwdt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dfi_t
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dfi_q
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dfi_u
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dfi_v
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dfi_q2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dfi_cwm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dfi_rrw
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: dfi_stc
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: dfi_smc
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: dfi_sh2o
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: dfi_snow
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: dfi_snowh
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: dfi_canwat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: dfi_nmm_tsk
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: dfi_snowc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: dx_nmm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: wpdar
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cpgfu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: curv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: fcp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: fdiv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: f
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: fad
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ddmpu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ddmpv
real      ,DIMENSION(grid%sm33:grid%em33)           :: deta
real      ,DIMENSION(grid%sm33:grid%em33)           :: rdeta
real      ,DIMENSION(grid%sm33:grid%em33)           :: aeta
real      ,DIMENSION(grid%sm33:grid%em33)           :: f4q2
real      ,DIMENSION(grid%sm33:grid%em33)           :: etax
real      ,DIMENSION(grid%sm33:grid%em33)           :: dfl
real      ,DIMENSION(grid%sm33:grid%em33)           :: deta1
real      ,DIMENSION(grid%sm33:grid%em33)           :: aeta1
real      ,DIMENSION(grid%sm33:grid%em33)           :: eta1
real      ,DIMENSION(grid%sm33:grid%em33)           :: deta2
real      ,DIMENSION(grid%sm33:grid%em33)           :: aeta2
real      ,DIMENSION(grid%sm33:grid%em33)           :: eta2
real      ,DIMENSION(1:2600)             :: em
real      ,DIMENSION(1:2600)             :: emt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: adt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: adu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: adv
real      ,DIMENSION(1:2600)             :: em_loc
real      ,DIMENSION(1:2600)             :: emt_loc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: pdsl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: pdslo
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: psdt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: div
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: def3d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: few
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: fne
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: fns
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: fse
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: omgalf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: petdt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rtop
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: pblh
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lpbl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: mixht
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ustar
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: z0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: mz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rc2d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dku3d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dkt3d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dubldt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dvbldt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dupbl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dvpbl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hpbl2d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: heat2d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: evap2d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: z0base
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ths
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: mavail
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qsh
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: twbs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qwbs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: taux
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tauy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: prec
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: aprec
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: acprec
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cuprec
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lspa
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ddata
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: accliq
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sno
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: si
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cldefi
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: deep
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: th10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: q10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: pshltr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tshltr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qshltr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: q2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_adj
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: t_old
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: zero_3d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: w0avg
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: akhs_out
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: akms_out
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lpi
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cd_out
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ch_out
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: albase
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: albedo
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cnvbot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cnvtop
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: czen
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: czmean
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: embck
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: epsr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: gffc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: glat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: glon
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: nmm_tsk
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hdac
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hdacv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: mxsnal
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: radin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: radot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sigt4
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tg
real      ,DIMENSION(grid%sm33:grid%em33)           :: dfrlg
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lvl
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: k22_deep
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: kbcon_deep
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ktop_deep
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: raincv_a
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: raincv_b
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: gd_cloud
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: gd_cloud2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: gd_cloud_a
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: gd_cloud2_a
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: qc_cu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: qi_cu
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: gd_cldfr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: acswupt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: acswuptc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: acswdnt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: acswdntc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: acswupb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: acswupbc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: acswdnb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: acswdnbc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: aclwupt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: aclwuptc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: aclwdnt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: aclwdntc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: aclwupb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: aclwupbc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: aclwdnb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: aclwdnbc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: swupt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: swuptc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: swdnt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: swdntc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: swupb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: swupbc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: swdnb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: swdnbc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lwupt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lwuptc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lwdnt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lwdntc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lwupb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lwupbc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lwdnb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lwdnbc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: swvisdir
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: swvisdif
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: swnirdir
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: swnirdif
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: refl_10cm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: refd_max
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qnwfa2d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: re_cloud
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: re_ice
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: re_snow
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: dfi_re_cloud
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: dfi_re_ice
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: dfi_re_snow
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: swddir
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: swddni
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: swddif
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: gx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: bx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: gg
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: bb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: coszen_ref
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: coszen
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hrang
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: swdown_ref
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: swddir_ref
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: cwm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rrw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: f_ice
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: f_rain
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: f_rimef
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: cldfra
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cfrach
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cfracl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cfracm
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: islope
real      ,DIMENSION(grid%sm33:grid%em33)           :: dzsoil
real      ,DIMENSION(grid%sm33:grid%em33)           :: rtdpth
real      ,DIMENSION(grid%sm33:grid%em33)           :: sldpth
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cmc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: grnflx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: pctsno
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soiltb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vegfrc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: shdmax
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: shdmin
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: sh2o
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: smc
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: stc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hstdv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hcnvx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hasyw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hasys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hasysw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hasynw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hlenw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hlens
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hlensw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hlennw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hangl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hanis
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hslop
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hzmax
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: crot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: srot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ugwdsfc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vgwdsfc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ctopo
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ctopo2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: dwdtmn
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: dwdtmx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: baro
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dwdt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: pdwdt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: pint
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: w_tot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: acfrcv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: acfrst
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ssroff
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: bgroff
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rlwin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rlwout
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rlwtoa
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: alwin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: alwout
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: alwtoa
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rswin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rswinc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rswout
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rswtoa
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: aswin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: aswout
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: aswtoa
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sfcshx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sfclhx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: subshx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: snopcx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sfcuvx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: potevp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: potflx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tlmin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tlmax
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: t02_min
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: t02_max
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rh02_min
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rh02_max
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rlwtt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: rswtt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tcucn
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: train
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ncfrcv
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ncfrst
integer   ,DIMENSION(grid%sm32:grid%em32)           :: ihe
integer   ,DIMENSION(grid%sm32:grid%em32)           :: ihw
integer   ,DIMENSION(grid%sm32:grid%em32)           :: ive
integer   ,DIMENSION(grid%sm32:grid%em32)           :: ivw
integer   ,DIMENSION(grid%sm31:grid%em31)           :: irad
integer   ,DIMENSION(1:2600)             :: iheg
integer   ,DIMENSION(1:2600)             :: ihwg
integer   ,DIMENSION(1:2600)             :: iveg
integer   ,DIMENSION(1:2600)             :: ivwg
integer   ,DIMENSION(1:2000)             :: iradg
integer   ,DIMENSION(grid%sm32:grid%em32)           :: n_iup_h
integer   ,DIMENSION(grid%sm32:grid%em32)           :: n_iup_v
integer   ,DIMENSION(grid%sm32:grid%em32)           :: n_iup_adh
integer   ,DIMENSION(grid%sm32:grid%em32)           :: n_iup_adv
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: iup_h
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: iup_v
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: iup_adh
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: iup_adv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: winfo
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: iinfo
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: imask_nostag
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: imask_xstag
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: imask_ystag
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: imask_xystag
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm000007
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm007028
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm028100
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm100255
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: st000007
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: st007028
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: st028100
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: st100255
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm000010
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm010040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm040100
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm100200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sm010200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilm000
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilm005
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilm020
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilm040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilm160
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilm300
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sw000010
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sw010040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sw040100
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sw100200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sw010200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilw000
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilw005
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilw020
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilw040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilw160
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilw300
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: st000010
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: st010040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: st040100
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: st100200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: st010200
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilt000
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilt005
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilt020
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilt040
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilt160
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilt300
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: landmask
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: topostdv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: toposlpx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: toposlpy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: greenmax
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: greenmin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: albedomx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: slopecat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: toposoil
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_land_cat,grid%sm32:grid%em32)           :: landusef
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_cat,grid%sm32:grid%em32)           :: soilctop
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_cat,grid%sm32:grid%em32)           :: soilcbot
real      ,DIMENSION(1:grid%ts_buf_size,1:grid%max_ts_locs)           :: ts_hour
real      ,DIMENSION(1:grid%ts_buf_size,1:grid%max_ts_locs)           :: ts_u
real      ,DIMENSION(1:grid%ts_buf_size,1:grid%max_ts_locs)           :: ts_v
real      ,DIMENSION(1:grid%ts_buf_size,1:grid%max_ts_locs)           :: ts_q
real      ,DIMENSION(1:grid%ts_buf_size,1:grid%max_ts_locs)           :: ts_t
real      ,DIMENSION(1:grid%ts_buf_size,1:grid%max_ts_locs)           :: ts_psfc
real      ,DIMENSION(1:grid%ts_buf_size,1:grid%max_ts_locs)           :: ts_tsk
real      ,DIMENSION(1:grid%ts_buf_size,1:grid%max_ts_locs)           :: ts_tslb
real      ,DIMENSION(1:grid%ts_buf_size,1:grid%max_ts_locs)           :: ts_clw
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
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: smois
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: tslb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lake_depth
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: gsw
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: xland
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: raincv
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: psfc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: th2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: t2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: u10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: v10
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: xice
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: icedepth
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: albsi
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: snowsi
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lai
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: smstav
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: smstot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soldrain
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sfcheadrt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: infxsrt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sfcrunoff
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: udrunoff
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ivgtyp
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: isltyp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vegfra
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sfcevp
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: grdflx
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: albbck
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sfcexc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: snotime
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: acsnow
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: acsnom
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rmol
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: snow
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: canwat
integer   ,DIMENSION(grid%sm33:grid%em33)           :: force_sst
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sst
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: uoce
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: voce
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: weasd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: znt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: mol
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: noahres
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: tke_pbl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: el_pbl
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: exch_h
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: exch_m
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: thz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: uz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vz0
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: flhc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: flqc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qsg
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qvg
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qcg
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: dew
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: soilt1
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tsnav
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: psfc_out
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: uz0h
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: vz0h
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dudt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33)           :: dvdt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qsfc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: akhs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: akms
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: htop
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hbot
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: htopr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hbotr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: htopd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hbotd
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: htops
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: hbots
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cuppt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cprate
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: f_ice_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: f_rain_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: f_rimef_phy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: mass_flux
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: apr_gr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: apr_w
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: apr_mc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: apr_st
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: apr_as
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: apr_capma
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: apr_capme
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: apr_capmi
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%ensdim)           :: xf_ens
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,1:grid%ensdim)           :: pr_ens
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: rthften
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: rqvften
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: snowh
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rhosn
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: smfr3d
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: keepfr3dflag
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rhosnf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: snowfallac
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: precipfr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: rc_mf
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: flx4
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: fvb
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: fbur
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: fgsn
integer   ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: isnowxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tvxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tgxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: canicexy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: canliqxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: eahxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tahxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: cmxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: chxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: fwetxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sneqvoxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: alboldxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qsnowxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: wslakexy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: zwtxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: waxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: wtxy
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_snow_layers,grid%sm32:grid%em32)           :: tsnoxy
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_snso_layers,grid%sm32:grid%em32)           :: zsnsoxy
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_snow_layers,grid%sm32:grid%em32)           :: snicexy
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_snow_layers,grid%sm32:grid%em32)           :: snliqxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: lfmassxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rtmassxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: stmassxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: woodxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: stblcpxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: fastcpxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: xsaixy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: taussxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: t2mvxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: t2mbxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: q2mvxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: q2mbxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tradxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: neexy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: gppxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: nppxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: fvegxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: qinxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: runsfxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: runsbxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ecanxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: edirxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: etranxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: fsaxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: firaxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: aparxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: psnxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: savxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: sagxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rssunxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rsshaxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: bgapxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: wgapxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tgvxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: tgbxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: chvxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: chbxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: shgxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: shcxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: shbxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: evgxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: evbxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ghvxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ghbxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: irgxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: ircxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: irbxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: trxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: evcxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: chleafxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: chucxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: chv2xy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: chb2xy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: chstarxy
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_soil_layers,grid%sm32:grid%em32)           :: smoiseq
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: smcwtdxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: rechxy
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: deeprechxy
real      ,DIMENSION(1:7501)             :: mp_restart_state
real      ,DIMENSION(1:7501)             :: tbpvs_state
real      ,DIMENSION(1:7501)             :: tbpvs0_state
real      ,DIMENSION(1:7501)             :: lu_state
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32)           :: power
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%levsiz,grid%sm32:grid%em32,num_ozmixm)           :: ozmixm
real      ,DIMENSION(1:grid%levsiz)           :: pin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32)           :: o3rad
#undef COPY_IN
#undef COPY_OUT
!ENDOFREGISTRYGENERATEDINCLUDE
