!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nest_feedbackup_unpack.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL rsl_lite_from_child_info(pig,pjg,retval)
DO while ( retval .eq. 1 )
feedback_flag=cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. )
feedback_flag_v=cd_feedback_mask_v( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. )
IF ( SIZE(grid%lakedepth2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%lakedepth2d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%savedtke12d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%savedtke12d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%snowdp2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%snowdp2d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%h2osno2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%h2osno2d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%snl2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%snl2d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%t_grnd2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%t_grnd2d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%t_lake3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((10)-(1)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = 1,10
NEST_INFLUENCE(grid%t_lake3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%lake_icefrac3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((10)-(1)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = 1,10
NEST_INFLUENCE(grid%lake_icefrac3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%z_lake3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((10)-(1)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = 1,10
NEST_INFLUENCE(grid%z_lake3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%dz_lake3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((10)-(1)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = 1,10
NEST_INFLUENCE(grid%dz_lake3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%t_soisno3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((15)-(1)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = 1,15
NEST_INFLUENCE(grid%t_soisno3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_ice3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((15)-(1)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = 1,15
NEST_INFLUENCE(grid%h2osoi_ice3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_liq3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((15)-(1)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = 1,15
NEST_INFLUENCE(grid%h2osoi_liq3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%h2osoi_vol3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((15)-(1)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = 1,15
NEST_INFLUENCE(grid%h2osoi_vol3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%z3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((15)-(1)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = 1,15
NEST_INFLUENCE(grid%z3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%dz3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((15)-(1)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = 1,15
NEST_INFLUENCE(grid%dz3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%zi3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((16)-(1)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = 1,16
NEST_INFLUENCE(grid%zi3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%watsat3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((10)-(1)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = 1,10
NEST_INFLUENCE(grid%watsat3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%csol3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((10)-(1)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = 1,10
NEST_INFLUENCE(grid%csol3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%tkmg3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((10)-(1)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = 1,10
NEST_INFLUENCE(grid%tkmg3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%tkdry3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((10)-(1)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = 1,10
NEST_INFLUENCE(grid%tkdry3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%tksatu3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((10)-(1)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = 1,10
NEST_INFLUENCE(grid%tksatu3d(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%lu_index) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%lu_index(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%precip_swath) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%precip_swath(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%windsq_swath) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%windsq_swath(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%pdyn_smooth) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%pdyn_smooth(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%pdyn_parent) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%pdyn_parent(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tg_max_m10wind) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%tg_max_m10wind(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tg_max_wwind) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%tg_max_wwind(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tg_min_wwind) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%tg_min_wwind(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tg_max_zhel_25) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%tg_max_zhel_25(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tg_min_zhel_25) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%tg_min_zhel_25(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tg_max_zhel_03) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%tg_max_zhel_03(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tg_min_zhel_03) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%tg_min_zhel_03(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tg_updhel25) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%tg_updhel25(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tg_max_updhel25) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%tg_max_updhel25(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tg_updhel03) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%tg_updhel03(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tg_max_updhel03) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%tg_max_updhel03(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tg_total_precip) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%tg_total_precip(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%hres_fis) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%hres_fis(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%pd) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%pd(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%fis) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%fis(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%t) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%t(pig,pjg,k),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%q) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%q(pig,pjg,k),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%u) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF(feedback_flag_v) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%u(pig,pjg,k),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%v) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF(feedback_flag_v) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%v(pig,pjg,k),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%q2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%q2(pig,pjg,k),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%acswupt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%acswupt(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%acswuptc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%acswuptc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%acswdnt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%acswdnt(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%acswdntc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%acswdntc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%acswupb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%acswupb(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%acswupbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%acswupbc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%acswdnb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%acswdnb(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%acswdnbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%acswdnbc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%aclwupt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%aclwupt(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%aclwuptc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%aclwuptc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%aclwdnt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%aclwdnt(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%aclwdntc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%aclwdntc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%aclwupb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%aclwupb(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%aclwupbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%aclwupbc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%aclwdnb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%aclwdnb(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%aclwdnbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%aclwdnbc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%swupt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%swupt(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%swuptc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%swuptc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%swdnt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%swdnt(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%swdntc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%swdntc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%swupb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%swupb(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%swupbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%swupbc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%swdnb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%swdnb(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%swdnbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%swdnbc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%lwupt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%lwupt(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%lwuptc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%lwuptc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%lwdnt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%lwdnt(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%lwdntc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%lwdntc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%lwupb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%lwupb(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%lwupbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%lwupbc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%lwdnb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%lwdnb(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%lwdnbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%lwdnbc(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%qnwfa2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%qnwfa2d(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%cwm) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%cwm(pig,pjg,k),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%f_ice) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%f_ice(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%f_rain) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%f_rain(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%f_rimef) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(grid%f_rimef(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%ctopo) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%ctopo(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%ctopo2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%ctopo2(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%winfo) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = ckds,ckde
NEST_INFLUENCE(grid%winfo(pig,pjg,k),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%iinfo) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = ckds,ckde
NEST_INFLUENCE(grid%iinfo(pig,pjg,k),xv(k))
ENDDO
ENDIF
ENDIF
if(interp_mp .eqv. .true.) then
DO itrace =  PARAM_FIRST_SCALAR, num_moist
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(moist(pig,pjg,k,itrace),xv(k))
ENDDO
ENDIF
ENDDO
endif
if(interp_mp .eqv. .true.) then
DO itrace =  PARAM_FIRST_SCALAR, num_scalar
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(scalar(pig,pjg,k,itrace),xv(k))
ENDDO
ENDIF
ENDDO
endif
if(interp_mp .eqv. .true.) then
DO itrace =  PARAM_FIRST_SCALAR, num_dfi_scalar
CALL rsl_lite_from_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = ckds,(ckde-1)
NEST_INFLUENCE(dfi_scalar(pig,pjg,k,itrace),xv(k))
ENDDO
ENDIF
ENDDO
endif
IF ( SIZE(grid%isnowxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%isnowxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%tvxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tgxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%tgxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%canicexy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%canicexy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%canliqxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%canliqxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%eahxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%eahxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tahxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%tahxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%cmxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%cmxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%chxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%chxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%fwetxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%fwetxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%sneqvoxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%sneqvoxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%alboldxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%alboldxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%qsnowxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%qsnowxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%wslakexy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%wslakexy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%zwtxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%zwtxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%waxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%waxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%wtxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%wtxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tsnoxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%num_snow_layers)-(1)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = 1,config_flags%num_snow_layers
NEST_INFLUENCE(grid%tsnoxy(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%zsnsoxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%num_snso_layers)-(1)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = 1,config_flags%num_snso_layers
NEST_INFLUENCE(grid%zsnsoxy(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%snicexy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%num_snow_layers)-(1)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = 1,config_flags%num_snow_layers
NEST_INFLUENCE(grid%snicexy(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%snliqxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(((config_flags%num_snow_layers)-(1)+1)*RWORDSIZE,xv) ;
IF(feedback_flag) THEN
DO k = 1,config_flags%num_snow_layers
NEST_INFLUENCE(grid%snliqxy(pig,k,pjg),xv(k))
ENDDO
ENDIF
ENDIF
IF ( SIZE(grid%lfmassxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%lfmassxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%rtmassxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%rtmassxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%stmassxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%stmassxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%woodxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%woodxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%stblcpxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%stblcpxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%fastcpxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%fastcpxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%xsaixy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%xsaixy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%t2mvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%t2mvxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%t2mbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%t2mbxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%q2mvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%q2mvxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%q2mbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%q2mbxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tradxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%tradxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%neexy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%neexy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%gppxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%gppxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%nppxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%nppxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%fvegxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%fvegxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%qinxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%qinxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%runsfxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%runsfxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%runsbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%runsbxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%ecanxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%ecanxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%edirxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%edirxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%etranxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%etranxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%fsaxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%fsaxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%firaxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%firaxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%aparxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%aparxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%psnxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%psnxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%savxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%savxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%sagxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%sagxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%rssunxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%rssunxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%rsshaxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%rsshaxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%bgapxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%bgapxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%wgapxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%wgapxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tgvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%tgvxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%tgbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%tgbxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%chvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%chvxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%chbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%chbxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%shgxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%shgxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%shcxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%shcxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%shbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%shbxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%evgxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%evgxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%evbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%evbxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%ghvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%ghvxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%ghbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%ghbxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%irgxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%irgxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%ircxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%ircxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%irbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%irbxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%trxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%trxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%evcxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%evcxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%chleafxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%chleafxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%chucxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%chucxy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%chv2xy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%chv2xy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%chb2xy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%chb2xy(pig,pjg),xv(1))
ENDIF
ENDIF
IF ( SIZE(grid%chstarxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
CALL rsl_lite_from_child_msg(RWORDSIZE,xv)
IF(feedback_flag) THEN
NEST_INFLUENCE(grid%chstarxy(pig,pjg),xv(1))
ENDIF
ENDIF
CALL rsl_lite_from_child_info(pig,pjg,retval)
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
