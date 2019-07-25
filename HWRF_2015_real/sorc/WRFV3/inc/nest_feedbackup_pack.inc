!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nest_feedbackup_pack.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
msize = (32 + ((num_szj - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_s1z - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_spz - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_tcs - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_chem - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_ozmixm - PARAM_FIRST_SCALAR + 1)) )* nlev + 131
IF(interp_mp .eqv. .true.) then
    msize=msize + (0 + ((num_moist - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_dfi_moist - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_scalar - PARAM_FIRST_SCALAR + 1)) & 
 + ((num_dfi_scalar - PARAM_FIRST_SCALAR + 1)) )*nlev+0
ENDIF
CALL rsl_lite_to_parent_info( local_communicator, msize*RWORDSIZE                               &
                        ,cips,cipe,cjps,cjpe                               &
                        ,nids,nide,njds,njde                               &
                        ,ntasks_x,ntasks_y                                 &
                        ,thisdomain_max_halo_width                                  &
                        ,icoord,jcoord                                     &
                        ,idim_cd,jdim_cd                                   &
                        ,pig,pjg,retval )
DO while ( retval .eq. 1 )
IF ( SIZE(grid%lakedepth2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%lakedepth2d(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%savedtke12d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%savedtke12d(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snowdp2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%snowdp2d(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osno2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%h2osno2d(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snl2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%snl2d(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_grnd2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%t_grnd2d(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_lake3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,10
xv(k)= intermediate_grid%t_lake3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((10)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lake_icefrac3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,10
xv(k)= intermediate_grid%lake_icefrac3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((10)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%z_lake3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,10
xv(k)= intermediate_grid%z_lake3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((10)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%dz_lake3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,10
xv(k)= intermediate_grid%dz_lake3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((10)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t_soisno3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,15
xv(k)= intermediate_grid%t_soisno3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((15)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_ice3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,15
xv(k)= intermediate_grid%h2osoi_ice3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((15)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_liq3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,15
xv(k)= intermediate_grid%h2osoi_liq3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((15)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%h2osoi_vol3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,15
xv(k)= intermediate_grid%h2osoi_vol3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((15)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%z3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,15
xv(k)= intermediate_grid%z3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((15)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%dz3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,15
xv(k)= intermediate_grid%dz3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((15)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%zi3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,16
xv(k)= intermediate_grid%zi3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((16)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%watsat3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,10
xv(k)= intermediate_grid%watsat3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((10)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%csol3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,10
xv(k)= intermediate_grid%csol3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((10)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tkmg3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,10
xv(k)= intermediate_grid%tkmg3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((10)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tkdry3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,10
xv(k)= intermediate_grid%tkdry3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((10)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tksatu3d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,10
xv(k)= intermediate_grid%tksatu3d(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((10)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lu_index) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%lu_index(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%precip_swath) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%precip_swath(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%windsq_swath) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%windsq_swath(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%pdyn_smooth) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%pdyn_smooth(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%pdyn_parent) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%pdyn_parent(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tg_max_m10wind) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%tg_max_m10wind(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tg_max_wwind) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%tg_max_wwind(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tg_min_wwind) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%tg_min_wwind(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tg_max_zhel_25) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%tg_max_zhel_25(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tg_min_zhel_25) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%tg_min_zhel_25(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tg_max_zhel_03) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%tg_max_zhel_03(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tg_min_zhel_03) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%tg_min_zhel_03(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tg_updhel25) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%tg_updhel25(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tg_max_updhel25) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%tg_max_updhel25(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tg_updhel03) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%tg_updhel03(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tg_max_updhel03) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%tg_max_updhel03(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tg_total_precip) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%tg_total_precip(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%hres_fis) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%hres_fis(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%pd) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%pd(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%fis) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%fis(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%t(pig,pjg,k)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%q) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%q(pig,pjg,k)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%u) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%u(pig,pjg,k)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%v) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%v(pig,pjg,k)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%q2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%q2(pig,pjg,k)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%acswupt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%acswupt(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%acswuptc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%acswuptc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%acswdnt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%acswdnt(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%acswdntc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%acswdntc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%acswupb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%acswupb(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%acswupbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%acswupbc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%acswdnb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%acswdnb(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%acswdnbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%acswdnbc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%aclwupt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%aclwupt(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%aclwuptc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%aclwuptc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%aclwdnt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%aclwdnt(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%aclwdntc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%aclwdntc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%aclwupb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%aclwupb(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%aclwupbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%aclwupbc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%aclwdnb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%aclwdnb(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%aclwdnbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%aclwdnbc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swupt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%swupt(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swuptc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%swuptc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swdnt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%swdnt(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swdntc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%swdntc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swupb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%swupb(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swupbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%swupbc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swdnb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%swdnb(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%swdnbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%swdnbc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lwupt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%lwupt(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lwuptc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%lwuptc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lwdnt) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%lwdnt(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lwdntc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%lwdntc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lwupb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%lwupb(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lwupbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%lwupbc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lwdnb) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%lwdnb(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lwdnbc) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%lwdnbc(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%qnwfa2d) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%qnwfa2d(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%cwm) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%cwm(pig,pjg,k)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%f_ice) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%f_ice(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%f_rain) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%f_rain(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%f_rimef) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%f_rimef(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ctopo) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%ctopo(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ctopo2) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%ctopo2(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%winfo) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,ckde
xv(k)= intermediate_grid%winfo(pig,pjg,k)
ENDDO
CALL rsl_lite_to_parent_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%iinfo) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = ckds,ckde
xv(k)= intermediate_grid%iinfo(pig,pjg,k)
ENDDO
CALL rsl_lite_to_parent_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv)
ENDIF
if(interp_mp .eqv. .true.) then
DO itrace =  PARAM_FIRST_SCALAR, num_moist
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%moist(pig,pjg,k,itrace)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDDO
endif
if(interp_mp .eqv. .true.) then
DO itrace =  PARAM_FIRST_SCALAR, num_scalar
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%scalar(pig,pjg,k,itrace)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDDO
endif
if(interp_mp .eqv. .true.) then
DO itrace =  PARAM_FIRST_SCALAR, num_dfi_scalar
DO k = ckds,(ckde-1)
xv(k)= intermediate_grid%dfi_scalar(pig,pjg,k,itrace)
ENDDO
CALL rsl_lite_to_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
ENDDO
endif
IF ( SIZE(grid%isnowxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%isnowxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%tvxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tgxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%tgxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%canicexy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%canicexy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%canliqxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%canliqxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%eahxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%eahxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tahxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%tahxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%cmxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%cmxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%chxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%chxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%fwetxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%fwetxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%sneqvoxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%sneqvoxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%alboldxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%alboldxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%qsnowxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%qsnowxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%wslakexy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%wslakexy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%zwtxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%zwtxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%waxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%waxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%wtxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%wtxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tsnoxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%num_snow_layers
xv(k)= intermediate_grid%tsnoxy(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((config_flags%num_snow_layers)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%zsnsoxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%num_snso_layers
xv(k)= intermediate_grid%zsnsoxy(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((config_flags%num_snso_layers)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snicexy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%num_snow_layers
xv(k)= intermediate_grid%snicexy(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((config_flags%num_snow_layers)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%snliqxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
DO k = 1,config_flags%num_snow_layers
xv(k)= intermediate_grid%snliqxy(pig,k,pjg)
ENDDO
CALL rsl_lite_to_parent_msg(((config_flags%num_snow_layers)-(1)+1)*RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%lfmassxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%lfmassxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%rtmassxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%rtmassxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%stmassxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%stmassxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%woodxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%woodxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%stblcpxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%stblcpxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%fastcpxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%fastcpxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%xsaixy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%xsaixy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t2mvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%t2mvxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%t2mbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%t2mbxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%q2mvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%q2mvxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%q2mbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%q2mbxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tradxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%tradxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%neexy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%neexy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%gppxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%gppxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%nppxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%nppxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%fvegxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%fvegxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%qinxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%qinxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%runsfxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%runsfxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%runsbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%runsbxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ecanxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%ecanxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%edirxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%edirxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%etranxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%etranxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%fsaxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%fsaxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%firaxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%firaxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%aparxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%aparxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%psnxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%psnxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%savxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%savxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%sagxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%sagxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%rssunxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%rssunxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%rsshaxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%rsshaxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%bgapxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%bgapxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%wgapxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%wgapxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tgvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%tgvxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%tgbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%tgbxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%chvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%chvxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%chbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%chbxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%shgxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%shgxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%shcxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%shcxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%shbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%shbxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%evgxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%evgxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%evbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%evbxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ghvxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%ghvxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ghbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%ghbxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%irgxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%irgxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%ircxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%ircxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%irbxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%irbxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%trxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%trxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%evcxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%evcxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%chleafxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%chleafxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%chucxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%chucxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%chv2xy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%chv2xy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%chb2xy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%chb2xy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
IF ( SIZE(grid%chstarxy) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
xv(1)= intermediate_grid%chstarxy(pig,pjg)
CALL rsl_lite_to_parent_msg(RWORDSIZE,xv)
ENDIF
CALL rsl_lite_to_parent_info( local_communicator, msize*RWORDSIZE                               &
                        ,cips,cipe,cjps,cjpe                               &
                        ,nids,nide,njds,njde                               &
                        ,ntasks_x,ntasks_y                                 &
                        ,thisdomain_max_halo_width                                  &
                        ,icoord,jcoord                                     &
                        ,idim_cd,jdim_cd                                   &
                        ,pig,pjg,retval )
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
