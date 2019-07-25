IF ( SIZE(grid%lake2d,1)*SIZE(grid%lake2d,2) .GT. 1 ) THEN
grid%lake2d (ips:min(ide-1,ipe),jms:jme) = grid%lake2d (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lakedepth2d,1)*SIZE(grid%lakedepth2d,2) .GT. 1 ) THEN
grid%lakedepth2d (ips:min(ide-1,ipe),jms:jme) = grid%lakedepth2d (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%savedtke12d,1)*SIZE(grid%savedtke12d,2) .GT. 1 ) THEN
grid%savedtke12d (ips:min(ide-1,ipe),jms:jme) = grid%savedtke12d (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%snowdp2d,1)*SIZE(grid%snowdp2d,2) .GT. 1 ) THEN
grid%snowdp2d (ips:min(ide-1,ipe),jms:jme) = grid%snowdp2d (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%h2osno2d,1)*SIZE(grid%h2osno2d,2) .GT. 1 ) THEN
grid%h2osno2d (ips:min(ide-1,ipe),jms:jme) = grid%h2osno2d (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%snl2d,1)*SIZE(grid%snl2d,2) .GT. 1 ) THEN
grid%snl2d (ips:min(ide-1,ipe),jms:jme) = grid%snl2d (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%t_grnd2d,1)*SIZE(grid%t_grnd2d,2) .GT. 1 ) THEN
grid%t_grnd2d (ips:min(ide-1,ipe),jms:jme) = grid%t_grnd2d (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%t_lake3d,1)*SIZE(grid%t_lake3d,3) .GT. 1 ) THEN
grid%t_lake3d (ips:min(ide-1,ipe),:,jms:jme) = grid%t_lake3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%lake_icefrac3d,1)*SIZE(grid%lake_icefrac3d,3) .GT. 1 ) THEN
grid%lake_icefrac3d (ips:min(ide-1,ipe),:,jms:jme) = grid%lake_icefrac3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%z_lake3d,1)*SIZE(grid%z_lake3d,3) .GT. 1 ) THEN
grid%z_lake3d (ips:min(ide-1,ipe),:,jms:jme) = grid%z_lake3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%dz_lake3d,1)*SIZE(grid%dz_lake3d,3) .GT. 1 ) THEN
grid%dz_lake3d (ips:min(ide-1,ipe),:,jms:jme) = grid%dz_lake3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%t_soisno3d,1)*SIZE(grid%t_soisno3d,3) .GT. 1 ) THEN
grid%t_soisno3d (ips:min(ide-1,ipe),:,jms:jme) = grid%t_soisno3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%h2osoi_ice3d,1)*SIZE(grid%h2osoi_ice3d,3) .GT. 1 ) THEN
grid%h2osoi_ice3d (ips:min(ide-1,ipe),:,jms:jme) = grid%h2osoi_ice3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%h2osoi_liq3d,1)*SIZE(grid%h2osoi_liq3d,3) .GT. 1 ) THEN
grid%h2osoi_liq3d (ips:min(ide-1,ipe),:,jms:jme) = grid%h2osoi_liq3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%h2osoi_vol3d,1)*SIZE(grid%h2osoi_vol3d,3) .GT. 1 ) THEN
grid%h2osoi_vol3d (ips:min(ide-1,ipe),:,jms:jme) = grid%h2osoi_vol3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%z3d,1)*SIZE(grid%z3d,3) .GT. 1 ) THEN
grid%z3d (ips:min(ide-1,ipe),:,jms:jme) = grid%z3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%dz3d,1)*SIZE(grid%dz3d,3) .GT. 1 ) THEN
grid%dz3d (ips:min(ide-1,ipe),:,jms:jme) = grid%dz3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%zi3d,1)*SIZE(grid%zi3d,3) .GT. 1 ) THEN
grid%zi3d (ips:min(ide-1,ipe),:,jms:jme) = grid%zi3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%watsat3d,1)*SIZE(grid%watsat3d,3) .GT. 1 ) THEN
grid%watsat3d (ips:min(ide-1,ipe),:,jms:jme) = grid%watsat3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%csol3d,1)*SIZE(grid%csol3d,3) .GT. 1 ) THEN
grid%csol3d (ips:min(ide-1,ipe),:,jms:jme) = grid%csol3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%tkmg3d,1)*SIZE(grid%tkmg3d,3) .GT. 1 ) THEN
grid%tkmg3d (ips:min(ide-1,ipe),:,jms:jme) = grid%tkmg3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%tkdry3d,1)*SIZE(grid%tkdry3d,3) .GT. 1 ) THEN
grid%tkdry3d (ips:min(ide-1,ipe),:,jms:jme) = grid%tkdry3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%tksatu3d,1)*SIZE(grid%tksatu3d,3) .GT. 1 ) THEN
grid%tksatu3d (ips:min(ide-1,ipe),:,jms:jme) = grid%tksatu3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
  DO itrace = PARAM_FIRST_SCALAR, num_szj
IF ( SIZE(grid%szj,1)*SIZE(grid%szj,2) .GT. 1 ) THEN
grid%szj (ips:min(ide-1,ipe),jms:jme,:,itrace) = grid%szj (ips+px:min(ide-1,ipe)+px,jms:jme,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_s1z
IF ( SIZE(grid%s1z,1)*SIZE(grid%s1z,2) .GT. 1 ) THEN
grid%s1z (ips:min(ide-1,ipe),jms:jme,:,itrace) = grid%s1z (ips+px:min(ide-1,ipe)+px,jms:jme,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_spz
IF ( SIZE(grid%spz,1)*SIZE(grid%spz,2) .GT. 1 ) THEN
grid%spz (ips:min(ide-1,ipe),jms:jme,:,itrace) = grid%spz (ips+px:min(ide-1,ipe)+px,jms:jme,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_tcs
IF ( SIZE(grid%tcs,1)*SIZE(grid%tcs,2) .GT. 1 ) THEN
grid%tcs (ips:min(ide-1,ipe),jms:jme,:,itrace) = grid%tcs (ips+px:min(ide-1,ipe)+px,jms:jme,:,itrace)
ENDIF
  ENDDO
IF ( SIZE(grid%lu_index,1)*SIZE(grid%lu_index,2) .GT. 1 ) THEN
grid%lu_index (ips:min(ide-1,ipe),jms:jme) = grid%lu_index (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lu_mask,1)*SIZE(grid%lu_mask,2) .GT. 1 ) THEN
grid%lu_mask (ips:min(ide-1,ipe),jms:jme) = grid%lu_mask (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%p_gc,1)*SIZE(grid%p_gc,2) .GT. 1 ) THEN
grid%p_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%p_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%vegcat,1)*SIZE(grid%vegcat,2) .GT. 1 ) THEN
grid%vegcat (ips:min(ide-1,ipe),jms:jme) = grid%vegcat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilcat,1)*SIZE(grid%soilcat,2) .GT. 1 ) THEN
grid%soilcat (ips:min(ide-1,ipe),jms:jme) = grid%soilcat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%input_soil_cat,1)*SIZE(grid%input_soil_cat,2) .GT. 1 ) THEN
grid%input_soil_cat (ips:min(ide-1,ipe),jms:jme) = grid%input_soil_cat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tsk_gc,1)*SIZE(grid%tsk_gc,2) .GT. 1 ) THEN
grid%tsk_gc (ips:min(ide-1,ipe),jms:jme) = grid%tsk_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%xice_gc,1)*SIZE(grid%xice_gc,2) .GT. 1 ) THEN
grid%xice_gc (ips:min(ide-1,ipe),jms:jme) = grid%xice_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ght_gc,1)*SIZE(grid%ght_gc,2) .GT. 1 ) THEN
grid%ght_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%ght_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%rh_gc,1)*SIZE(grid%rh_gc,2) .GT. 1 ) THEN
grid%rh_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%rh_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%v_gc,1)*SIZE(grid%v_gc,2) .GT. 1 ) THEN
grid%v_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%v_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%u_gc,1)*SIZE(grid%u_gc,2) .GT. 1 ) THEN
grid%u_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%u_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%t_gc,1)*SIZE(grid%t_gc,2) .GT. 1 ) THEN
grid%t_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%t_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%snoalb,1)*SIZE(grid%snoalb,2) .GT. 1 ) THEN
grid%snoalb (ips:min(ide-1,ipe),jms:jme) = grid%snoalb (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%greenfrac_gc,1)*SIZE(grid%greenfrac_gc,2) .GT. 1 ) THEN
grid%greenfrac_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%greenfrac_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%albedo12m_gc,1)*SIZE(grid%albedo12m_gc,2) .GT. 1 ) THEN
grid%albedo12m_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%albedo12m_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%lai12m_gc,1)*SIZE(grid%lai12m_gc,2) .GT. 1 ) THEN
grid%lai12m_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%lai12m_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%soilcbot_gc,1)*SIZE(grid%soilcbot_gc,2) .GT. 1 ) THEN
grid%soilcbot_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%soilcbot_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%soilctop_gc,1)*SIZE(grid%soilctop_gc,2) .GT. 1 ) THEN
grid%soilctop_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%soilctop_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%tmn_gc,1)*SIZE(grid%tmn_gc,2) .GT. 1 ) THEN
grid%tmn_gc (ips:min(ide-1,ipe),jms:jme) = grid%tmn_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%htv_gc,1)*SIZE(grid%htv_gc,2) .GT. 1 ) THEN
grid%htv_gc (ips:min(ide-1,ipe),jms:jme) = grid%htv_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ht_gc,1)*SIZE(grid%ht_gc,2) .GT. 1 ) THEN
grid%ht_gc (ips:min(ide-1,ipe),jms:jme) = grid%ht_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%landusef_gc,1)*SIZE(grid%landusef_gc,2) .GT. 1 ) THEN
grid%landusef_gc (ips:min(ide-1,ipe),jms:jme,:) = grid%landusef_gc (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%vlon_gc,1)*SIZE(grid%vlon_gc,2) .GT. 1 ) THEN
grid%vlon_gc (ips:min(ide-1,ipe),jms:jme) = grid%vlon_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vlat_gc,1)*SIZE(grid%vlat_gc,2) .GT. 1 ) THEN
grid%vlat_gc (ips:min(ide-1,ipe),jms:jme) = grid%vlat_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hlon_gc,1)*SIZE(grid%hlon_gc,2) .GT. 1 ) THEN
grid%hlon_gc (ips:min(ide-1,ipe),jms:jme) = grid%hlon_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hlat_gc,1)*SIZE(grid%hlat_gc,2) .GT. 1 ) THEN
grid%hlat_gc (ips:min(ide-1,ipe),jms:jme) = grid%hlat_gc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%relaxwork,1)*SIZE(grid%relaxwork,2) .GT. 1 ) THEN
grid%relaxwork (ips:min(ide-1,ipe),jms:jme) = grid%relaxwork (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%relaximask,1)*SIZE(grid%relaximask,2) .GT. 1 ) THEN
grid%relaximask (ips:min(ide-1,ipe),jms:jme) = grid%relaximask (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%relaxmask,1)*SIZE(grid%relaxmask,2) .GT. 1 ) THEN
grid%relaxmask (ips:min(ide-1,ipe),jms:jme) = grid%relaxmask (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%interesting,1)*SIZE(grid%interesting,2) .GT. 1 ) THEN
grid%interesting (ips:min(ide-1,ipe),jms:jme) = grid%interesting (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%precip_swath,1)*SIZE(grid%precip_swath,2) .GT. 1 ) THEN
grid%precip_swath (ips:min(ide-1,ipe),jms:jme) = grid%precip_swath (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%windsq_swath,1)*SIZE(grid%windsq_swath,2) .GT. 1 ) THEN
grid%windsq_swath (ips:min(ide-1,ipe),jms:jme) = grid%windsq_swath (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tracker_distsq,1)*SIZE(grid%tracker_distsq,2) .GT. 1 ) THEN
grid%tracker_distsq (ips:min(ide-1,ipe),jms:jme) = grid%tracker_distsq (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tracker_angle,1)*SIZE(grid%tracker_angle,2) .GT. 1 ) THEN
grid%tracker_angle (ips:min(ide-1,ipe),jms:jme) = grid%tracker_angle (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tracker_fixes,1)*SIZE(grid%tracker_fixes,2) .GT. 1 ) THEN
grid%tracker_fixes (ips:min(ide-1,ipe),jms:jme) = grid%tracker_fixes (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%membrane_mslp,1)*SIZE(grid%membrane_mslp,2) .GT. 1 ) THEN
grid%membrane_mslp (ips:min(ide-1,ipe),jms:jme) = grid%membrane_mslp (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%p850rv,1)*SIZE(grid%p850rv,2) .GT. 1 ) THEN
grid%p850rv (ips:min(ide-1,ipe),jms:jme) = grid%p850rv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%p700rv,1)*SIZE(grid%p700rv,2) .GT. 1 ) THEN
grid%p700rv (ips:min(ide-1,ipe),jms:jme) = grid%p700rv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%p850wind,1)*SIZE(grid%p850wind,2) .GT. 1 ) THEN
grid%p850wind (ips:min(ide-1,ipe),jms:jme) = grid%p850wind (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%p700wind,1)*SIZE(grid%p700wind,2) .GT. 1 ) THEN
grid%p700wind (ips:min(ide-1,ipe),jms:jme) = grid%p700wind (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%p500u,1)*SIZE(grid%p500u,2) .GT. 1 ) THEN
grid%p500u (ips:min(ide-1,ipe),jms:jme) = grid%p500u (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%p500v,1)*SIZE(grid%p500v,2) .GT. 1 ) THEN
grid%p500v (ips:min(ide-1,ipe),jms:jme) = grid%p500v (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%p700u,1)*SIZE(grid%p700u,2) .GT. 1 ) THEN
grid%p700u (ips:min(ide-1,ipe),jms:jme) = grid%p700u (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%p700v,1)*SIZE(grid%p700v,2) .GT. 1 ) THEN
grid%p700v (ips:min(ide-1,ipe),jms:jme) = grid%p700v (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%p850u,1)*SIZE(grid%p850u,2) .GT. 1 ) THEN
grid%p850u (ips:min(ide-1,ipe),jms:jme) = grid%p850u (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%p850v,1)*SIZE(grid%p850v,2) .GT. 1 ) THEN
grid%p850v (ips:min(ide-1,ipe),jms:jme) = grid%p850v (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%p850z,1)*SIZE(grid%p850z,2) .GT. 1 ) THEN
grid%p850z (ips:min(ide-1,ipe),jms:jme) = grid%p850z (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%p700z,1)*SIZE(grid%p700z,2) .GT. 1 ) THEN
grid%p700z (ips:min(ide-1,ipe),jms:jme) = grid%p700z (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%m10wind,1)*SIZE(grid%m10wind,2) .GT. 1 ) THEN
grid%m10wind (ips:min(ide-1,ipe),jms:jme) = grid%m10wind (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%m10rv,1)*SIZE(grid%m10rv,2) .GT. 1 ) THEN
grid%m10rv (ips:min(ide-1,ipe),jms:jme) = grid%m10rv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sp850rv,1)*SIZE(grid%sp850rv,2) .GT. 1 ) THEN
grid%sp850rv (ips:min(ide-1,ipe),jms:jme) = grid%sp850rv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sp700rv,1)*SIZE(grid%sp700rv,2) .GT. 1 ) THEN
grid%sp700rv (ips:min(ide-1,ipe),jms:jme) = grid%sp700rv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sp850wind,1)*SIZE(grid%sp850wind,2) .GT. 1 ) THEN
grid%sp850wind (ips:min(ide-1,ipe),jms:jme) = grid%sp850wind (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sp700wind,1)*SIZE(grid%sp700wind,2) .GT. 1 ) THEN
grid%sp700wind (ips:min(ide-1,ipe),jms:jme) = grid%sp700wind (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sp850z,1)*SIZE(grid%sp850z,2) .GT. 1 ) THEN
grid%sp850z (ips:min(ide-1,ipe),jms:jme) = grid%sp850z (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sp700z,1)*SIZE(grid%sp700z,2) .GT. 1 ) THEN
grid%sp700z (ips:min(ide-1,ipe),jms:jme) = grid%sp700z (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm10wind,1)*SIZE(grid%sm10wind,2) .GT. 1 ) THEN
grid%sm10wind (ips:min(ide-1,ipe),jms:jme) = grid%sm10wind (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm10rv,1)*SIZE(grid%sm10rv,2) .GT. 1 ) THEN
grid%sm10rv (ips:min(ide-1,ipe),jms:jme) = grid%sm10rv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%smslp,1)*SIZE(grid%smslp,2) .GT. 1 ) THEN
grid%smslp (ips:min(ide-1,ipe),jms:jme) = grid%smslp (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%distsq,1)*SIZE(grid%distsq,2) .GT. 1 ) THEN
grid%distsq (ips:min(ide-1,ipe),jms:jme) = grid%distsq (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%weightout,1)*SIZE(grid%weightout,2) .GT. 1 ) THEN
grid%weightout (ips:min(ide-1,ipe),jms:jme) = grid%weightout (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%mslp_noisy,1)*SIZE(grid%mslp_noisy,2) .GT. 1 ) THEN
grid%mslp_noisy (ips:min(ide-1,ipe),jms:jme) = grid%mslp_noisy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%pdyn_smooth,1)*SIZE(grid%pdyn_smooth,2) .GT. 1 ) THEN
grid%pdyn_smooth (ips:min(ide-1,ipe),jms:jme) = grid%pdyn_smooth (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%pdyn_parent,1)*SIZE(grid%pdyn_parent,2) .GT. 1 ) THEN
grid%pdyn_parent (ips:min(ide-1,ipe),jms:jme) = grid%pdyn_parent (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%pdyn,1)*SIZE(grid%pdyn,2) .GT. 1 ) THEN
grid%pdyn (ips:min(ide-1,ipe),jms:jme) = grid%pdyn (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%mslp,1)*SIZE(grid%mslp,2) .GT. 1 ) THEN
grid%mslp (ips:min(ide-1,ipe),jms:jme) = grid%mslp (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%best_mslp,1)*SIZE(grid%best_mslp,2) .GT. 1 ) THEN
grid%best_mslp (ips:min(ide-1,ipe),jms:jme) = grid%best_mslp (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sqws,1)*SIZE(grid%sqws,2) .GT. 1 ) THEN
grid%sqws (ips:min(ide-1,ipe),jms:jme) = grid%sqws (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ducudt,1)*SIZE(grid%ducudt,2) .GT. 1 ) THEN
grid%ducudt (ips:min(ide-1,ipe),jms:jme,:) = grid%ducudt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dvcudt,1)*SIZE(grid%dvcudt,2) .GT. 1 ) THEN
grid%dvcudt (ips:min(ide-1,ipe),jms:jme,:) = grid%dvcudt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%randstate1,1)*SIZE(grid%randstate1,2) .GT. 1 ) THEN
grid%randstate1 (ips:min(ide-1,ipe),jms:jme) = grid%randstate1 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%randstate2,1)*SIZE(grid%randstate2,2) .GT. 1 ) THEN
grid%randstate2 (ips:min(ide-1,ipe),jms:jme) = grid%randstate2 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%randstate3,1)*SIZE(grid%randstate3,2) .GT. 1 ) THEN
grid%randstate3 (ips:min(ide-1,ipe),jms:jme) = grid%randstate3 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%randstate4,1)*SIZE(grid%randstate4,2) .GT. 1 ) THEN
grid%randstate4 (ips:min(ide-1,ipe),jms:jme) = grid%randstate4 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%random,1)*SIZE(grid%random,2) .GT. 1 ) THEN
grid%random (ips:min(ide-1,ipe),jms:jme) = grid%random (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%iih,1)*SIZE(grid%iih,2) .GT. 1 ) THEN
grid%iih (ips:min(ide-1,ipe),jms:jme) = grid%iih (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%jjh,1)*SIZE(grid%jjh,2) .GT. 1 ) THEN
grid%jjh (ips:min(ide-1,ipe),jms:jme) = grid%jjh (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%iiv,1)*SIZE(grid%iiv,2) .GT. 1 ) THEN
grid%iiv (ips:min(ide-1,ipe),jms:jme) = grid%iiv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%jjv,1)*SIZE(grid%jjv,2) .GT. 1 ) THEN
grid%jjv (ips:min(ide-1,ipe),jms:jme) = grid%jjv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hnear_i,1)*SIZE(grid%hnear_i,2) .GT. 1 ) THEN
grid%hnear_i (ips:min(ide-1,ipe),jms:jme) = grid%hnear_i (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hnear_j,1)*SIZE(grid%hnear_j,2) .GT. 1 ) THEN
grid%hnear_j (ips:min(ide-1,ipe),jms:jme) = grid%hnear_j (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbwgt1,1)*SIZE(grid%hbwgt1,2) .GT. 1 ) THEN
grid%hbwgt1 (ips:min(ide-1,ipe),jms:jme) = grid%hbwgt1 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbwgt2,1)*SIZE(grid%hbwgt2,2) .GT. 1 ) THEN
grid%hbwgt2 (ips:min(ide-1,ipe),jms:jme) = grid%hbwgt2 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbwgt3,1)*SIZE(grid%hbwgt3,2) .GT. 1 ) THEN
grid%hbwgt3 (ips:min(ide-1,ipe),jms:jme) = grid%hbwgt3 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbwgt4,1)*SIZE(grid%hbwgt4,2) .GT. 1 ) THEN
grid%hbwgt4 (ips:min(ide-1,ipe),jms:jme) = grid%hbwgt4 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vbwgt1,1)*SIZE(grid%vbwgt1,2) .GT. 1 ) THEN
grid%vbwgt1 (ips:min(ide-1,ipe),jms:jme) = grid%vbwgt1 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vbwgt2,1)*SIZE(grid%vbwgt2,2) .GT. 1 ) THEN
grid%vbwgt2 (ips:min(ide-1,ipe),jms:jme) = grid%vbwgt2 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vbwgt3,1)*SIZE(grid%vbwgt3,2) .GT. 1 ) THEN
grid%vbwgt3 (ips:min(ide-1,ipe),jms:jme) = grid%vbwgt3 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vbwgt4,1)*SIZE(grid%vbwgt4,2) .GT. 1 ) THEN
grid%vbwgt4 (ips:min(ide-1,ipe),jms:jme) = grid%vbwgt4 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hlon,1)*SIZE(grid%hlon,2) .GT. 1 ) THEN
grid%hlon (ips:min(ide-1,ipe),jms:jme) = grid%hlon (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hlat,1)*SIZE(grid%hlat,2) .GT. 1 ) THEN
grid%hlat (ips:min(ide-1,ipe),jms:jme) = grid%hlat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vlon,1)*SIZE(grid%vlon,2) .GT. 1 ) THEN
grid%vlon (ips:min(ide-1,ipe),jms:jme) = grid%vlon (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vlat,1)*SIZE(grid%vlat,2) .GT. 1 ) THEN
grid%vlat (ips:min(ide-1,ipe),jms:jme) = grid%vlat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tg_max_m10wind,1)*SIZE(grid%tg_max_m10wind,2) .GT. 1 ) THEN
grid%tg_max_m10wind (ips:min(ide-1,ipe),jms:jme) = grid%tg_max_m10wind (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tg_max_wwind,1)*SIZE(grid%tg_max_wwind,2) .GT. 1 ) THEN
grid%tg_max_wwind (ips:min(ide-1,ipe),jms:jme) = grid%tg_max_wwind (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tg_min_wwind,1)*SIZE(grid%tg_min_wwind,2) .GT. 1 ) THEN
grid%tg_min_wwind (ips:min(ide-1,ipe),jms:jme) = grid%tg_min_wwind (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tg_max_zhel_25,1)*SIZE(grid%tg_max_zhel_25,2) .GT. 1 ) THEN
grid%tg_max_zhel_25 (ips:min(ide-1,ipe),jms:jme) = grid%tg_max_zhel_25 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tg_min_zhel_25,1)*SIZE(grid%tg_min_zhel_25,2) .GT. 1 ) THEN
grid%tg_min_zhel_25 (ips:min(ide-1,ipe),jms:jme) = grid%tg_min_zhel_25 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tg_max_zhel_03,1)*SIZE(grid%tg_max_zhel_03,2) .GT. 1 ) THEN
grid%tg_max_zhel_03 (ips:min(ide-1,ipe),jms:jme) = grid%tg_max_zhel_03 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tg_min_zhel_03,1)*SIZE(grid%tg_min_zhel_03,2) .GT. 1 ) THEN
grid%tg_min_zhel_03 (ips:min(ide-1,ipe),jms:jme) = grid%tg_min_zhel_03 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tg_updhel25,1)*SIZE(grid%tg_updhel25,2) .GT. 1 ) THEN
grid%tg_updhel25 (ips:min(ide-1,ipe),jms:jme) = grid%tg_updhel25 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tg_max_updhel25,1)*SIZE(grid%tg_max_updhel25,2) .GT. 1 ) THEN
grid%tg_max_updhel25 (ips:min(ide-1,ipe),jms:jme) = grid%tg_max_updhel25 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tg_updhel03,1)*SIZE(grid%tg_updhel03,2) .GT. 1 ) THEN
grid%tg_updhel03 (ips:min(ide-1,ipe),jms:jme) = grid%tg_updhel03 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tg_max_updhel03,1)*SIZE(grid%tg_max_updhel03,2) .GT. 1 ) THEN
grid%tg_max_updhel03 (ips:min(ide-1,ipe),jms:jme) = grid%tg_max_updhel03 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tg_total_precip,1)*SIZE(grid%tg_total_precip,2) .GT. 1 ) THEN
grid%tg_total_precip (ips:min(ide-1,ipe),jms:jme) = grid%tg_total_precip (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tlow,1)*SIZE(grid%tlow,2) .GT. 1 ) THEN
grid%tlow (ips:min(ide-1,ipe),jms:jme) = grid%tlow (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%zlow,1)*SIZE(grid%zlow,2) .GT. 1 ) THEN
grid%zlow (ips:min(ide-1,ipe),jms:jme) = grid%zlow (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rotangle,1)*SIZE(grid%rotangle,2) .GT. 1 ) THEN
grid%rotangle (ips:min(ide-1,ipe),jms:jme) = grid%rotangle (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hres_fis,1)*SIZE(grid%hres_fis,2) .GT. 1 ) THEN
grid%hres_fis (ips:min(ide-1,ipe),jms:jme) = grid%hres_fis (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hres_avc,1)*SIZE(grid%hres_avc,2) .GT. 1 ) THEN
grid%hres_avc (ips:min(ide-1,ipe),jms:jme) = grid%hres_avc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hres_lnd,1)*SIZE(grid%hres_lnd,2) .GT. 1 ) THEN
grid%hres_lnd (ips:min(ide-1,ipe),jms:jme) = grid%hres_lnd (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbm2,1)*SIZE(grid%hbm2,2) .GT. 1 ) THEN
grid%hbm2 (ips:min(ide-1,ipe),jms:jme) = grid%hbm2 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbm3,1)*SIZE(grid%hbm3,2) .GT. 1 ) THEN
grid%hbm3 (ips:min(ide-1,ipe),jms:jme) = grid%hbm3 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vbm2,1)*SIZE(grid%vbm2,2) .GT. 1 ) THEN
grid%vbm2 (ips:min(ide-1,ipe),jms:jme) = grid%vbm2 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vbm3,1)*SIZE(grid%vbm3,2) .GT. 1 ) THEN
grid%vbm3 (ips:min(ide-1,ipe),jms:jme) = grid%vbm3 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm,1)*SIZE(grid%sm,2) .GT. 1 ) THEN
grid%sm (ips:min(ide-1,ipe),jms:jme) = grid%sm (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sice,1)*SIZE(grid%sice,2) .GT. 1 ) THEN
grid%sice (ips:min(ide-1,ipe),jms:jme) = grid%sice (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%pd,1)*SIZE(grid%pd,2) .GT. 1 ) THEN
grid%pd (ips:min(ide-1,ipe),jms:jme) = grid%pd (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%fis,1)*SIZE(grid%fis,2) .GT. 1 ) THEN
grid%fis (ips:min(ide-1,ipe),jms:jme) = grid%fis (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%res,1)*SIZE(grid%res,2) .GT. 1 ) THEN
grid%res (ips:min(ide-1,ipe),jms:jme) = grid%res (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%t,1)*SIZE(grid%t,2) .GT. 1 ) THEN
grid%t (ips:min(ide-1,ipe),jms:jme,:) = grid%t (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%q,1)*SIZE(grid%q,2) .GT. 1 ) THEN
grid%q (ips:min(ide-1,ipe),jms:jme,:) = grid%q (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%test_vgrid,1)*SIZE(grid%test_vgrid,2) .GT. 1 ) THEN
grid%test_vgrid (ips:min(ide-1,ipe),jms:jme) = grid%test_vgrid (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%u,1)*SIZE(grid%u,2) .GT. 1 ) THEN
grid%u (ips:min(ide-1,ipe),jms:jme,:) = grid%u (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%v,1)*SIZE(grid%v,2) .GT. 1 ) THEN
grid%v (ips:min(ide-1,ipe),jms:jme,:) = grid%v (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%told,1)*SIZE(grid%told,2) .GT. 1 ) THEN
grid%told (ips:min(ide-1,ipe),jms:jme,:) = grid%told (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%uold,1)*SIZE(grid%uold,2) .GT. 1 ) THEN
grid%uold (ips:min(ide-1,ipe),jms:jme,:) = grid%uold (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%vold,1)*SIZE(grid%vold,2) .GT. 1 ) THEN
grid%vold (ips:min(ide-1,ipe),jms:jme,:) = grid%vold (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_pd,1)*SIZE(grid%dfi_pd,2) .GT. 1 ) THEN
grid%dfi_pd (ips:min(ide-1,ipe),jms:jme) = grid%dfi_pd (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_pint,1)*SIZE(grid%dfi_pint,2) .GT. 1 ) THEN
grid%dfi_pint (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_pint (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_dwdt,1)*SIZE(grid%dfi_dwdt,2) .GT. 1 ) THEN
grid%dfi_dwdt (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_dwdt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_t,1)*SIZE(grid%dfi_t,2) .GT. 1 ) THEN
grid%dfi_t (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_t (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_q,1)*SIZE(grid%dfi_q,2) .GT. 1 ) THEN
grid%dfi_q (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_q (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_u,1)*SIZE(grid%dfi_u,2) .GT. 1 ) THEN
grid%dfi_u (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_u (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_v,1)*SIZE(grid%dfi_v,2) .GT. 1 ) THEN
grid%dfi_v (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_v (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_q2,1)*SIZE(grid%dfi_q2,2) .GT. 1 ) THEN
grid%dfi_q2 (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_q2 (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_cwm,1)*SIZE(grid%dfi_cwm,2) .GT. 1 ) THEN
grid%dfi_cwm (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_cwm (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_rrw,1)*SIZE(grid%dfi_rrw,2) .GT. 1 ) THEN
grid%dfi_rrw (ips:min(ide-1,ipe),jms:jme,:) = grid%dfi_rrw (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dfi_stc,1)*SIZE(grid%dfi_stc,3) .GT. 1 ) THEN
grid%dfi_stc (ips:min(ide-1,ipe),:,jms:jme) = grid%dfi_stc (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_smc,1)*SIZE(grid%dfi_smc,3) .GT. 1 ) THEN
grid%dfi_smc (ips:min(ide-1,ipe),:,jms:jme) = grid%dfi_smc (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_sh2o,1)*SIZE(grid%dfi_sh2o,3) .GT. 1 ) THEN
grid%dfi_sh2o (ips:min(ide-1,ipe),:,jms:jme) = grid%dfi_sh2o (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_snow,1)*SIZE(grid%dfi_snow,2) .GT. 1 ) THEN
grid%dfi_snow (ips:min(ide-1,ipe),jms:jme) = grid%dfi_snow (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_snowh,1)*SIZE(grid%dfi_snowh,2) .GT. 1 ) THEN
grid%dfi_snowh (ips:min(ide-1,ipe),jms:jme) = grid%dfi_snowh (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_canwat,1)*SIZE(grid%dfi_canwat,2) .GT. 1 ) THEN
grid%dfi_canwat (ips:min(ide-1,ipe),jms:jme) = grid%dfi_canwat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_nmm_tsk,1)*SIZE(grid%dfi_nmm_tsk,2) .GT. 1 ) THEN
grid%dfi_nmm_tsk (ips:min(ide-1,ipe),jms:jme) = grid%dfi_nmm_tsk (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_snowc,1)*SIZE(grid%dfi_snowc,2) .GT. 1 ) THEN
grid%dfi_snowc (ips:min(ide-1,ipe),jms:jme) = grid%dfi_snowc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dx_nmm,1)*SIZE(grid%dx_nmm,2) .GT. 1 ) THEN
grid%dx_nmm (ips:min(ide-1,ipe),jms:jme) = grid%dx_nmm (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%wpdar,1)*SIZE(grid%wpdar,2) .GT. 1 ) THEN
grid%wpdar (ips:min(ide-1,ipe),jms:jme) = grid%wpdar (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cpgfu,1)*SIZE(grid%cpgfu,2) .GT. 1 ) THEN
grid%cpgfu (ips:min(ide-1,ipe),jms:jme) = grid%cpgfu (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%curv,1)*SIZE(grid%curv,2) .GT. 1 ) THEN
grid%curv (ips:min(ide-1,ipe),jms:jme) = grid%curv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%fcp,1)*SIZE(grid%fcp,2) .GT. 1 ) THEN
grid%fcp (ips:min(ide-1,ipe),jms:jme) = grid%fcp (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%fdiv,1)*SIZE(grid%fdiv,2) .GT. 1 ) THEN
grid%fdiv (ips:min(ide-1,ipe),jms:jme) = grid%fdiv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%f,1)*SIZE(grid%f,2) .GT. 1 ) THEN
grid%f (ips:min(ide-1,ipe),jms:jme) = grid%f (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%fad,1)*SIZE(grid%fad,2) .GT. 1 ) THEN
grid%fad (ips:min(ide-1,ipe),jms:jme) = grid%fad (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ddmpu,1)*SIZE(grid%ddmpu,2) .GT. 1 ) THEN
grid%ddmpu (ips:min(ide-1,ipe),jms:jme) = grid%ddmpu (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ddmpv,1)*SIZE(grid%ddmpv,2) .GT. 1 ) THEN
grid%ddmpv (ips:min(ide-1,ipe),jms:jme) = grid%ddmpv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%adt,1)*SIZE(grid%adt,2) .GT. 1 ) THEN
grid%adt (ips:min(ide-1,ipe),jms:jme) = grid%adt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%adu,1)*SIZE(grid%adu,2) .GT. 1 ) THEN
grid%adu (ips:min(ide-1,ipe),jms:jme) = grid%adu (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%adv,1)*SIZE(grid%adv,2) .GT. 1 ) THEN
grid%adv (ips:min(ide-1,ipe),jms:jme) = grid%adv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%pdsl,1)*SIZE(grid%pdsl,2) .GT. 1 ) THEN
grid%pdsl (ips:min(ide-1,ipe),jms:jme) = grid%pdsl (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%pdslo,1)*SIZE(grid%pdslo,2) .GT. 1 ) THEN
grid%pdslo (ips:min(ide-1,ipe),jms:jme) = grid%pdslo (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%psdt,1)*SIZE(grid%psdt,2) .GT. 1 ) THEN
grid%psdt (ips:min(ide-1,ipe),jms:jme) = grid%psdt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%div,1)*SIZE(grid%div,2) .GT. 1 ) THEN
grid%div (ips:min(ide-1,ipe),jms:jme,:) = grid%div (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%def3d,1)*SIZE(grid%def3d,2) .GT. 1 ) THEN
grid%def3d (ips:min(ide-1,ipe),jms:jme,:) = grid%def3d (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%few,1)*SIZE(grid%few,2) .GT. 1 ) THEN
grid%few (ips:min(ide-1,ipe),jms:jme,:) = grid%few (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%fne,1)*SIZE(grid%fne,2) .GT. 1 ) THEN
grid%fne (ips:min(ide-1,ipe),jms:jme,:) = grid%fne (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%fns,1)*SIZE(grid%fns,2) .GT. 1 ) THEN
grid%fns (ips:min(ide-1,ipe),jms:jme,:) = grid%fns (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%fse,1)*SIZE(grid%fse,2) .GT. 1 ) THEN
grid%fse (ips:min(ide-1,ipe),jms:jme,:) = grid%fse (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%omgalf,1)*SIZE(grid%omgalf,2) .GT. 1 ) THEN
grid%omgalf (ips:min(ide-1,ipe),jms:jme,:) = grid%omgalf (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%petdt,1)*SIZE(grid%petdt,2) .GT. 1 ) THEN
grid%petdt (ips:min(ide-1,ipe),jms:jme,:) = grid%petdt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%rtop,1)*SIZE(grid%rtop,2) .GT. 1 ) THEN
grid%rtop (ips:min(ide-1,ipe),jms:jme,:) = grid%rtop (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%pblh,1)*SIZE(grid%pblh,2) .GT. 1 ) THEN
grid%pblh (ips:min(ide-1,ipe),jms:jme) = grid%pblh (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lpbl,1)*SIZE(grid%lpbl,2) .GT. 1 ) THEN
grid%lpbl (ips:min(ide-1,ipe),jms:jme) = grid%lpbl (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%mixht,1)*SIZE(grid%mixht,2) .GT. 1 ) THEN
grid%mixht (ips:min(ide-1,ipe),jms:jme) = grid%mixht (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ustar,1)*SIZE(grid%ustar,2) .GT. 1 ) THEN
grid%ustar (ips:min(ide-1,ipe),jms:jme) = grid%ustar (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%z0,1)*SIZE(grid%z0,2) .GT. 1 ) THEN
grid%z0 (ips:min(ide-1,ipe),jms:jme) = grid%z0 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%mz0,1)*SIZE(grid%mz0,2) .GT. 1 ) THEN
grid%mz0 (ips:min(ide-1,ipe),jms:jme) = grid%mz0 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rc2d,1)*SIZE(grid%rc2d,2) .GT. 1 ) THEN
grid%rc2d (ips:min(ide-1,ipe),jms:jme) = grid%rc2d (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dku3d,1)*SIZE(grid%dku3d,2) .GT. 1 ) THEN
grid%dku3d (ips:min(ide-1,ipe),jms:jme,:) = grid%dku3d (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dkt3d,1)*SIZE(grid%dkt3d,2) .GT. 1 ) THEN
grid%dkt3d (ips:min(ide-1,ipe),jms:jme,:) = grid%dkt3d (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%hpbl2d,1)*SIZE(grid%hpbl2d,2) .GT. 1 ) THEN
grid%hpbl2d (ips:min(ide-1,ipe),jms:jme) = grid%hpbl2d (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%heat2d,1)*SIZE(grid%heat2d,2) .GT. 1 ) THEN
grid%heat2d (ips:min(ide-1,ipe),jms:jme) = grid%heat2d (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%evap2d,1)*SIZE(grid%evap2d,2) .GT. 1 ) THEN
grid%evap2d (ips:min(ide-1,ipe),jms:jme) = grid%evap2d (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%z0base,1)*SIZE(grid%z0base,2) .GT. 1 ) THEN
grid%z0base (ips:min(ide-1,ipe),jms:jme) = grid%z0base (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ths,1)*SIZE(grid%ths,2) .GT. 1 ) THEN
grid%ths (ips:min(ide-1,ipe),jms:jme) = grid%ths (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%mavail,1)*SIZE(grid%mavail,2) .GT. 1 ) THEN
grid%mavail (ips:min(ide-1,ipe),jms:jme) = grid%mavail (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qsh,1)*SIZE(grid%qsh,2) .GT. 1 ) THEN
grid%qsh (ips:min(ide-1,ipe),jms:jme) = grid%qsh (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%twbs,1)*SIZE(grid%twbs,2) .GT. 1 ) THEN
grid%twbs (ips:min(ide-1,ipe),jms:jme) = grid%twbs (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qwbs,1)*SIZE(grid%qwbs,2) .GT. 1 ) THEN
grid%qwbs (ips:min(ide-1,ipe),jms:jme) = grid%qwbs (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%taux,1)*SIZE(grid%taux,2) .GT. 1 ) THEN
grid%taux (ips:min(ide-1,ipe),jms:jme) = grid%taux (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tauy,1)*SIZE(grid%tauy,2) .GT. 1 ) THEN
grid%tauy (ips:min(ide-1,ipe),jms:jme) = grid%tauy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%prec,1)*SIZE(grid%prec,2) .GT. 1 ) THEN
grid%prec (ips:min(ide-1,ipe),jms:jme) = grid%prec (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aprec,1)*SIZE(grid%aprec,2) .GT. 1 ) THEN
grid%aprec (ips:min(ide-1,ipe),jms:jme) = grid%aprec (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%acprec,1)*SIZE(grid%acprec,2) .GT. 1 ) THEN
grid%acprec (ips:min(ide-1,ipe),jms:jme) = grid%acprec (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cuprec,1)*SIZE(grid%cuprec,2) .GT. 1 ) THEN
grid%cuprec (ips:min(ide-1,ipe),jms:jme) = grid%cuprec (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lspa,1)*SIZE(grid%lspa,2) .GT. 1 ) THEN
grid%lspa (ips:min(ide-1,ipe),jms:jme) = grid%lspa (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ddata,1)*SIZE(grid%ddata,2) .GT. 1 ) THEN
grid%ddata (ips:min(ide-1,ipe),jms:jme) = grid%ddata (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%accliq,1)*SIZE(grid%accliq,2) .GT. 1 ) THEN
grid%accliq (ips:min(ide-1,ipe),jms:jme) = grid%accliq (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sno,1)*SIZE(grid%sno,2) .GT. 1 ) THEN
grid%sno (ips:min(ide-1,ipe),jms:jme) = grid%sno (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%si,1)*SIZE(grid%si,2) .GT. 1 ) THEN
grid%si (ips:min(ide-1,ipe),jms:jme) = grid%si (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cldefi,1)*SIZE(grid%cldefi,2) .GT. 1 ) THEN
grid%cldefi (ips:min(ide-1,ipe),jms:jme) = grid%cldefi (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%deep,1)*SIZE(grid%deep,2) .GT. 1 ) THEN
grid%deep (ips:min(ide-1,ipe),jms:jme) = grid%deep (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rf,1)*SIZE(grid%rf,2) .GT. 1 ) THEN
grid%rf (ips:min(ide-1,ipe),jms:jme) = grid%rf (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%th10,1)*SIZE(grid%th10,2) .GT. 1 ) THEN
grid%th10 (ips:min(ide-1,ipe),jms:jme) = grid%th10 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%q10,1)*SIZE(grid%q10,2) .GT. 1 ) THEN
grid%q10 (ips:min(ide-1,ipe),jms:jme) = grid%q10 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%pshltr,1)*SIZE(grid%pshltr,2) .GT. 1 ) THEN
grid%pshltr (ips:min(ide-1,ipe),jms:jme) = grid%pshltr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tshltr,1)*SIZE(grid%tshltr,2) .GT. 1 ) THEN
grid%tshltr (ips:min(ide-1,ipe),jms:jme) = grid%tshltr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qshltr,1)*SIZE(grid%qshltr,2) .GT. 1 ) THEN
grid%qshltr (ips:min(ide-1,ipe),jms:jme) = grid%qshltr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%q2,1)*SIZE(grid%q2,2) .GT. 1 ) THEN
grid%q2 (ips:min(ide-1,ipe),jms:jme,:) = grid%q2 (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%t_adj,1)*SIZE(grid%t_adj,2) .GT. 1 ) THEN
grid%t_adj (ips:min(ide-1,ipe),jms:jme,:) = grid%t_adj (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%t_old,1)*SIZE(grid%t_old,2) .GT. 1 ) THEN
grid%t_old (ips:min(ide-1,ipe),jms:jme,:) = grid%t_old (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%zero_3d,1)*SIZE(grid%zero_3d,2) .GT. 1 ) THEN
grid%zero_3d (ips:min(ide-1,ipe),jms:jme,:) = grid%zero_3d (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%w0avg,1)*SIZE(grid%w0avg,3) .GT. 1 ) THEN
grid%w0avg (ips:min(ide-1,ipe),:,jms:jme) = grid%w0avg (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%akhs_out,1)*SIZE(grid%akhs_out,2) .GT. 1 ) THEN
grid%akhs_out (ips:min(ide-1,ipe),jms:jme) = grid%akhs_out (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%akms_out,1)*SIZE(grid%akms_out,2) .GT. 1 ) THEN
grid%akms_out (ips:min(ide-1,ipe),jms:jme) = grid%akms_out (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lpi,1)*SIZE(grid%lpi,2) .GT. 1 ) THEN
grid%lpi (ips:min(ide-1,ipe),jms:jme) = grid%lpi (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cd_out,1)*SIZE(grid%cd_out,2) .GT. 1 ) THEN
grid%cd_out (ips:min(ide-1,ipe),jms:jme) = grid%cd_out (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ch_out,1)*SIZE(grid%ch_out,2) .GT. 1 ) THEN
grid%ch_out (ips:min(ide-1,ipe),jms:jme) = grid%ch_out (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%albase,1)*SIZE(grid%albase,2) .GT. 1 ) THEN
grid%albase (ips:min(ide-1,ipe),jms:jme) = grid%albase (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%albedo,1)*SIZE(grid%albedo,2) .GT. 1 ) THEN
grid%albedo (ips:min(ide-1,ipe),jms:jme) = grid%albedo (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cnvbot,1)*SIZE(grid%cnvbot,2) .GT. 1 ) THEN
grid%cnvbot (ips:min(ide-1,ipe),jms:jme) = grid%cnvbot (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cnvtop,1)*SIZE(grid%cnvtop,2) .GT. 1 ) THEN
grid%cnvtop (ips:min(ide-1,ipe),jms:jme) = grid%cnvtop (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%czen,1)*SIZE(grid%czen,2) .GT. 1 ) THEN
grid%czen (ips:min(ide-1,ipe),jms:jme) = grid%czen (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%czmean,1)*SIZE(grid%czmean,2) .GT. 1 ) THEN
grid%czmean (ips:min(ide-1,ipe),jms:jme) = grid%czmean (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%embck,1)*SIZE(grid%embck,2) .GT. 1 ) THEN
grid%embck (ips:min(ide-1,ipe),jms:jme) = grid%embck (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%epsr,1)*SIZE(grid%epsr,2) .GT. 1 ) THEN
grid%epsr (ips:min(ide-1,ipe),jms:jme) = grid%epsr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%gffc,1)*SIZE(grid%gffc,2) .GT. 1 ) THEN
grid%gffc (ips:min(ide-1,ipe),jms:jme) = grid%gffc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%glat,1)*SIZE(grid%glat,2) .GT. 1 ) THEN
grid%glat (ips:min(ide-1,ipe),jms:jme) = grid%glat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%glon,1)*SIZE(grid%glon,2) .GT. 1 ) THEN
grid%glon (ips:min(ide-1,ipe),jms:jme) = grid%glon (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%nmm_tsk,1)*SIZE(grid%nmm_tsk,2) .GT. 1 ) THEN
grid%nmm_tsk (ips:min(ide-1,ipe),jms:jme) = grid%nmm_tsk (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hdac,1)*SIZE(grid%hdac,2) .GT. 1 ) THEN
grid%hdac (ips:min(ide-1,ipe),jms:jme) = grid%hdac (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hdacv,1)*SIZE(grid%hdacv,2) .GT. 1 ) THEN
grid%hdacv (ips:min(ide-1,ipe),jms:jme) = grid%hdacv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%mxsnal,1)*SIZE(grid%mxsnal,2) .GT. 1 ) THEN
grid%mxsnal (ips:min(ide-1,ipe),jms:jme) = grid%mxsnal (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%radin,1)*SIZE(grid%radin,2) .GT. 1 ) THEN
grid%radin (ips:min(ide-1,ipe),jms:jme) = grid%radin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%radot,1)*SIZE(grid%radot,2) .GT. 1 ) THEN
grid%radot (ips:min(ide-1,ipe),jms:jme) = grid%radot (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sigt4,1)*SIZE(grid%sigt4,2) .GT. 1 ) THEN
grid%sigt4 (ips:min(ide-1,ipe),jms:jme) = grid%sigt4 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tg,1)*SIZE(grid%tg,2) .GT. 1 ) THEN
grid%tg (ips:min(ide-1,ipe),jms:jme) = grid%tg (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lvl,1)*SIZE(grid%lvl,2) .GT. 1 ) THEN
grid%lvl (ips:min(ide-1,ipe),jms:jme) = grid%lvl (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%k22_deep,1)*SIZE(grid%k22_deep,2) .GT. 1 ) THEN
grid%k22_deep (ips:min(ide-1,ipe),jms:jme) = grid%k22_deep (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%kbcon_deep,1)*SIZE(grid%kbcon_deep,2) .GT. 1 ) THEN
grid%kbcon_deep (ips:min(ide-1,ipe),jms:jme) = grid%kbcon_deep (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ktop_deep,1)*SIZE(grid%ktop_deep,2) .GT. 1 ) THEN
grid%ktop_deep (ips:min(ide-1,ipe),jms:jme) = grid%ktop_deep (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%raincv_a,1)*SIZE(grid%raincv_a,2) .GT. 1 ) THEN
grid%raincv_a (ips:min(ide-1,ipe),jms:jme) = grid%raincv_a (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%raincv_b,1)*SIZE(grid%raincv_b,2) .GT. 1 ) THEN
grid%raincv_b (ips:min(ide-1,ipe),jms:jme) = grid%raincv_b (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%gd_cloud,1)*SIZE(grid%gd_cloud,3) .GT. 1 ) THEN
grid%gd_cloud (ips:min(ide-1,ipe),:,jms:jme) = grid%gd_cloud (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%gd_cloud2,1)*SIZE(grid%gd_cloud2,3) .GT. 1 ) THEN
grid%gd_cloud2 (ips:min(ide-1,ipe),:,jms:jme) = grid%gd_cloud2 (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%gd_cloud_a,1)*SIZE(grid%gd_cloud_a,3) .GT. 1 ) THEN
grid%gd_cloud_a (ips:min(ide-1,ipe),:,jms:jme) = grid%gd_cloud_a (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%gd_cloud2_a,1)*SIZE(grid%gd_cloud2_a,3) .GT. 1 ) THEN
grid%gd_cloud2_a (ips:min(ide-1,ipe),:,jms:jme) = grid%gd_cloud2_a (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%qc_cu,1)*SIZE(grid%qc_cu,3) .GT. 1 ) THEN
grid%qc_cu (ips:min(ide-1,ipe),:,jms:jme) = grid%qc_cu (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%qi_cu,1)*SIZE(grid%qi_cu,3) .GT. 1 ) THEN
grid%qi_cu (ips:min(ide-1,ipe),:,jms:jme) = grid%qi_cu (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%gd_cldfr,1)*SIZE(grid%gd_cldfr,3) .GT. 1 ) THEN
grid%gd_cldfr (ips:min(ide-1,ipe),:,jms:jme) = grid%gd_cldfr (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%acswupt,1)*SIZE(grid%acswupt,2) .GT. 1 ) THEN
grid%acswupt (ips:min(ide-1,ipe),jms:jme) = grid%acswupt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%acswuptc,1)*SIZE(grid%acswuptc,2) .GT. 1 ) THEN
grid%acswuptc (ips:min(ide-1,ipe),jms:jme) = grid%acswuptc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%acswdnt,1)*SIZE(grid%acswdnt,2) .GT. 1 ) THEN
grid%acswdnt (ips:min(ide-1,ipe),jms:jme) = grid%acswdnt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%acswdntc,1)*SIZE(grid%acswdntc,2) .GT. 1 ) THEN
grid%acswdntc (ips:min(ide-1,ipe),jms:jme) = grid%acswdntc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%acswupb,1)*SIZE(grid%acswupb,2) .GT. 1 ) THEN
grid%acswupb (ips:min(ide-1,ipe),jms:jme) = grid%acswupb (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%acswupbc,1)*SIZE(grid%acswupbc,2) .GT. 1 ) THEN
grid%acswupbc (ips:min(ide-1,ipe),jms:jme) = grid%acswupbc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%acswdnb,1)*SIZE(grid%acswdnb,2) .GT. 1 ) THEN
grid%acswdnb (ips:min(ide-1,ipe),jms:jme) = grid%acswdnb (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%acswdnbc,1)*SIZE(grid%acswdnbc,2) .GT. 1 ) THEN
grid%acswdnbc (ips:min(ide-1,ipe),jms:jme) = grid%acswdnbc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aclwupt,1)*SIZE(grid%aclwupt,2) .GT. 1 ) THEN
grid%aclwupt (ips:min(ide-1,ipe),jms:jme) = grid%aclwupt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aclwuptc,1)*SIZE(grid%aclwuptc,2) .GT. 1 ) THEN
grid%aclwuptc (ips:min(ide-1,ipe),jms:jme) = grid%aclwuptc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aclwdnt,1)*SIZE(grid%aclwdnt,2) .GT. 1 ) THEN
grid%aclwdnt (ips:min(ide-1,ipe),jms:jme) = grid%aclwdnt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aclwdntc,1)*SIZE(grid%aclwdntc,2) .GT. 1 ) THEN
grid%aclwdntc (ips:min(ide-1,ipe),jms:jme) = grid%aclwdntc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aclwupb,1)*SIZE(grid%aclwupb,2) .GT. 1 ) THEN
grid%aclwupb (ips:min(ide-1,ipe),jms:jme) = grid%aclwupb (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aclwupbc,1)*SIZE(grid%aclwupbc,2) .GT. 1 ) THEN
grid%aclwupbc (ips:min(ide-1,ipe),jms:jme) = grid%aclwupbc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aclwdnb,1)*SIZE(grid%aclwdnb,2) .GT. 1 ) THEN
grid%aclwdnb (ips:min(ide-1,ipe),jms:jme) = grid%aclwdnb (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aclwdnbc,1)*SIZE(grid%aclwdnbc,2) .GT. 1 ) THEN
grid%aclwdnbc (ips:min(ide-1,ipe),jms:jme) = grid%aclwdnbc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%swupt,1)*SIZE(grid%swupt,2) .GT. 1 ) THEN
grid%swupt (ips:min(ide-1,ipe),jms:jme) = grid%swupt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%swuptc,1)*SIZE(grid%swuptc,2) .GT. 1 ) THEN
grid%swuptc (ips:min(ide-1,ipe),jms:jme) = grid%swuptc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%swdnt,1)*SIZE(grid%swdnt,2) .GT. 1 ) THEN
grid%swdnt (ips:min(ide-1,ipe),jms:jme) = grid%swdnt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%swdntc,1)*SIZE(grid%swdntc,2) .GT. 1 ) THEN
grid%swdntc (ips:min(ide-1,ipe),jms:jme) = grid%swdntc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%swupb,1)*SIZE(grid%swupb,2) .GT. 1 ) THEN
grid%swupb (ips:min(ide-1,ipe),jms:jme) = grid%swupb (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%swupbc,1)*SIZE(grid%swupbc,2) .GT. 1 ) THEN
grid%swupbc (ips:min(ide-1,ipe),jms:jme) = grid%swupbc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%swdnb,1)*SIZE(grid%swdnb,2) .GT. 1 ) THEN
grid%swdnb (ips:min(ide-1,ipe),jms:jme) = grid%swdnb (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%swdnbc,1)*SIZE(grid%swdnbc,2) .GT. 1 ) THEN
grid%swdnbc (ips:min(ide-1,ipe),jms:jme) = grid%swdnbc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lwupt,1)*SIZE(grid%lwupt,2) .GT. 1 ) THEN
grid%lwupt (ips:min(ide-1,ipe),jms:jme) = grid%lwupt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lwuptc,1)*SIZE(grid%lwuptc,2) .GT. 1 ) THEN
grid%lwuptc (ips:min(ide-1,ipe),jms:jme) = grid%lwuptc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lwdnt,1)*SIZE(grid%lwdnt,2) .GT. 1 ) THEN
grid%lwdnt (ips:min(ide-1,ipe),jms:jme) = grid%lwdnt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lwdntc,1)*SIZE(grid%lwdntc,2) .GT. 1 ) THEN
grid%lwdntc (ips:min(ide-1,ipe),jms:jme) = grid%lwdntc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lwupb,1)*SIZE(grid%lwupb,2) .GT. 1 ) THEN
grid%lwupb (ips:min(ide-1,ipe),jms:jme) = grid%lwupb (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lwupbc,1)*SIZE(grid%lwupbc,2) .GT. 1 ) THEN
grid%lwupbc (ips:min(ide-1,ipe),jms:jme) = grid%lwupbc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lwdnb,1)*SIZE(grid%lwdnb,2) .GT. 1 ) THEN
grid%lwdnb (ips:min(ide-1,ipe),jms:jme) = grid%lwdnb (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lwdnbc,1)*SIZE(grid%lwdnbc,2) .GT. 1 ) THEN
grid%lwdnbc (ips:min(ide-1,ipe),jms:jme) = grid%lwdnbc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%swvisdir,1)*SIZE(grid%swvisdir,2) .GT. 1 ) THEN
grid%swvisdir (ips:min(ide-1,ipe),jms:jme) = grid%swvisdir (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%swvisdif,1)*SIZE(grid%swvisdif,2) .GT. 1 ) THEN
grid%swvisdif (ips:min(ide-1,ipe),jms:jme) = grid%swvisdif (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%swnirdir,1)*SIZE(grid%swnirdir,2) .GT. 1 ) THEN
grid%swnirdir (ips:min(ide-1,ipe),jms:jme) = grid%swnirdir (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%swnirdif,1)*SIZE(grid%swnirdif,2) .GT. 1 ) THEN
grid%swnirdif (ips:min(ide-1,ipe),jms:jme) = grid%swnirdif (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%refl_10cm,1)*SIZE(grid%refl_10cm,3) .GT. 1 ) THEN
grid%refl_10cm (ips:min(ide-1,ipe),:,jms:jme) = grid%refl_10cm (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%refd_max,1)*SIZE(grid%refd_max,2) .GT. 1 ) THEN
grid%refd_max (ips:min(ide-1,ipe),jms:jme) = grid%refd_max (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qnwfa2d,1)*SIZE(grid%qnwfa2d,2) .GT. 1 ) THEN
grid%qnwfa2d (ips:min(ide-1,ipe),jms:jme) = grid%qnwfa2d (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%re_cloud,1)*SIZE(grid%re_cloud,3) .GT. 1 ) THEN
grid%re_cloud (ips:min(ide-1,ipe),:,jms:jme) = grid%re_cloud (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%re_ice,1)*SIZE(grid%re_ice,3) .GT. 1 ) THEN
grid%re_ice (ips:min(ide-1,ipe),:,jms:jme) = grid%re_ice (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%re_snow,1)*SIZE(grid%re_snow,3) .GT. 1 ) THEN
grid%re_snow (ips:min(ide-1,ipe),:,jms:jme) = grid%re_snow (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_re_cloud,1)*SIZE(grid%dfi_re_cloud,3) .GT. 1 ) THEN
grid%dfi_re_cloud (ips:min(ide-1,ipe),:,jms:jme) = grid%dfi_re_cloud (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_re_ice,1)*SIZE(grid%dfi_re_ice,3) .GT. 1 ) THEN
grid%dfi_re_ice (ips:min(ide-1,ipe),:,jms:jme) = grid%dfi_re_ice (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%dfi_re_snow,1)*SIZE(grid%dfi_re_snow,3) .GT. 1 ) THEN
grid%dfi_re_snow (ips:min(ide-1,ipe),:,jms:jme) = grid%dfi_re_snow (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%swddir,1)*SIZE(grid%swddir,2) .GT. 1 ) THEN
grid%swddir (ips:min(ide-1,ipe),jms:jme) = grid%swddir (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%swddni,1)*SIZE(grid%swddni,2) .GT. 1 ) THEN
grid%swddni (ips:min(ide-1,ipe),jms:jme) = grid%swddni (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%swddif,1)*SIZE(grid%swddif,2) .GT. 1 ) THEN
grid%swddif (ips:min(ide-1,ipe),jms:jme) = grid%swddif (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%gx,1)*SIZE(grid%gx,2) .GT. 1 ) THEN
grid%gx (ips:min(ide-1,ipe),jms:jme) = grid%gx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%bx,1)*SIZE(grid%bx,2) .GT. 1 ) THEN
grid%bx (ips:min(ide-1,ipe),jms:jme) = grid%bx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%gg,1)*SIZE(grid%gg,2) .GT. 1 ) THEN
grid%gg (ips:min(ide-1,ipe),jms:jme) = grid%gg (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%bb,1)*SIZE(grid%bb,2) .GT. 1 ) THEN
grid%bb (ips:min(ide-1,ipe),jms:jme) = grid%bb (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%coszen_ref,1)*SIZE(grid%coszen_ref,2) .GT. 1 ) THEN
grid%coszen_ref (ips:min(ide-1,ipe),jms:jme) = grid%coszen_ref (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%coszen,1)*SIZE(grid%coszen,2) .GT. 1 ) THEN
grid%coszen (ips:min(ide-1,ipe),jms:jme) = grid%coszen (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hrang,1)*SIZE(grid%hrang,2) .GT. 1 ) THEN
grid%hrang (ips:min(ide-1,ipe),jms:jme) = grid%hrang (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%swdown_ref,1)*SIZE(grid%swdown_ref,2) .GT. 1 ) THEN
grid%swdown_ref (ips:min(ide-1,ipe),jms:jme) = grid%swdown_ref (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%swddir_ref,1)*SIZE(grid%swddir_ref,2) .GT. 1 ) THEN
grid%swddir_ref (ips:min(ide-1,ipe),jms:jme) = grid%swddir_ref (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cwm,1)*SIZE(grid%cwm,2) .GT. 1 ) THEN
grid%cwm (ips:min(ide-1,ipe),jms:jme,:) = grid%cwm (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%rrw,1)*SIZE(grid%rrw,2) .GT. 1 ) THEN
grid%rrw (ips:min(ide-1,ipe),jms:jme,:) = grid%rrw (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%f_ice,1)*SIZE(grid%f_ice,3) .GT. 1 ) THEN
grid%f_ice (ips:min(ide-1,ipe),:,jms:jme) = grid%f_ice (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%f_rain,1)*SIZE(grid%f_rain,3) .GT. 1 ) THEN
grid%f_rain (ips:min(ide-1,ipe),:,jms:jme) = grid%f_rain (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%f_rimef,1)*SIZE(grid%f_rimef,3) .GT. 1 ) THEN
grid%f_rimef (ips:min(ide-1,ipe),:,jms:jme) = grid%f_rimef (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%cldfra,1)*SIZE(grid%cldfra,2) .GT. 1 ) THEN
grid%cldfra (ips:min(ide-1,ipe),jms:jme,:) = grid%cldfra (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%sr,1)*SIZE(grid%sr,2) .GT. 1 ) THEN
grid%sr (ips:min(ide-1,ipe),jms:jme) = grid%sr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cfrach,1)*SIZE(grid%cfrach,2) .GT. 1 ) THEN
grid%cfrach (ips:min(ide-1,ipe),jms:jme) = grid%cfrach (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cfracl,1)*SIZE(grid%cfracl,2) .GT. 1 ) THEN
grid%cfracl (ips:min(ide-1,ipe),jms:jme) = grid%cfracl (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cfracm,1)*SIZE(grid%cfracm,2) .GT. 1 ) THEN
grid%cfracm (ips:min(ide-1,ipe),jms:jme) = grid%cfracm (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%islope,1)*SIZE(grid%islope,2) .GT. 1 ) THEN
grid%islope (ips:min(ide-1,ipe),jms:jme) = grid%islope (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cmc,1)*SIZE(grid%cmc,2) .GT. 1 ) THEN
grid%cmc (ips:min(ide-1,ipe),jms:jme) = grid%cmc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%grnflx,1)*SIZE(grid%grnflx,2) .GT. 1 ) THEN
grid%grnflx (ips:min(ide-1,ipe),jms:jme) = grid%grnflx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%pctsno,1)*SIZE(grid%pctsno,2) .GT. 1 ) THEN
grid%pctsno (ips:min(ide-1,ipe),jms:jme) = grid%pctsno (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soiltb,1)*SIZE(grid%soiltb,2) .GT. 1 ) THEN
grid%soiltb (ips:min(ide-1,ipe),jms:jme) = grid%soiltb (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vegfrc,1)*SIZE(grid%vegfrc,2) .GT. 1 ) THEN
grid%vegfrc (ips:min(ide-1,ipe),jms:jme) = grid%vegfrc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%shdmax,1)*SIZE(grid%shdmax,2) .GT. 1 ) THEN
grid%shdmax (ips:min(ide-1,ipe),jms:jme) = grid%shdmax (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%shdmin,1)*SIZE(grid%shdmin,2) .GT. 1 ) THEN
grid%shdmin (ips:min(ide-1,ipe),jms:jme) = grid%shdmin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sh2o,1)*SIZE(grid%sh2o,3) .GT. 1 ) THEN
grid%sh2o (ips:min(ide-1,ipe),:,jms:jme) = grid%sh2o (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%smc,1)*SIZE(grid%smc,3) .GT. 1 ) THEN
grid%smc (ips:min(ide-1,ipe),:,jms:jme) = grid%smc (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%stc,1)*SIZE(grid%stc,3) .GT. 1 ) THEN
grid%stc (ips:min(ide-1,ipe),:,jms:jme) = grid%stc (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%hstdv,1)*SIZE(grid%hstdv,2) .GT. 1 ) THEN
grid%hstdv (ips:min(ide-1,ipe),jms:jme) = grid%hstdv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hcnvx,1)*SIZE(grid%hcnvx,2) .GT. 1 ) THEN
grid%hcnvx (ips:min(ide-1,ipe),jms:jme) = grid%hcnvx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hasyw,1)*SIZE(grid%hasyw,2) .GT. 1 ) THEN
grid%hasyw (ips:min(ide-1,ipe),jms:jme) = grid%hasyw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hasys,1)*SIZE(grid%hasys,2) .GT. 1 ) THEN
grid%hasys (ips:min(ide-1,ipe),jms:jme) = grid%hasys (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hasysw,1)*SIZE(grid%hasysw,2) .GT. 1 ) THEN
grid%hasysw (ips:min(ide-1,ipe),jms:jme) = grid%hasysw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hasynw,1)*SIZE(grid%hasynw,2) .GT. 1 ) THEN
grid%hasynw (ips:min(ide-1,ipe),jms:jme) = grid%hasynw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hlenw,1)*SIZE(grid%hlenw,2) .GT. 1 ) THEN
grid%hlenw (ips:min(ide-1,ipe),jms:jme) = grid%hlenw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hlens,1)*SIZE(grid%hlens,2) .GT. 1 ) THEN
grid%hlens (ips:min(ide-1,ipe),jms:jme) = grid%hlens (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hlensw,1)*SIZE(grid%hlensw,2) .GT. 1 ) THEN
grid%hlensw (ips:min(ide-1,ipe),jms:jme) = grid%hlensw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hlennw,1)*SIZE(grid%hlennw,2) .GT. 1 ) THEN
grid%hlennw (ips:min(ide-1,ipe),jms:jme) = grid%hlennw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hangl,1)*SIZE(grid%hangl,2) .GT. 1 ) THEN
grid%hangl (ips:min(ide-1,ipe),jms:jme) = grid%hangl (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hanis,1)*SIZE(grid%hanis,2) .GT. 1 ) THEN
grid%hanis (ips:min(ide-1,ipe),jms:jme) = grid%hanis (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hslop,1)*SIZE(grid%hslop,2) .GT. 1 ) THEN
grid%hslop (ips:min(ide-1,ipe),jms:jme) = grid%hslop (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hzmax,1)*SIZE(grid%hzmax,2) .GT. 1 ) THEN
grid%hzmax (ips:min(ide-1,ipe),jms:jme) = grid%hzmax (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%crot,1)*SIZE(grid%crot,2) .GT. 1 ) THEN
grid%crot (ips:min(ide-1,ipe),jms:jme) = grid%crot (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%srot,1)*SIZE(grid%srot,2) .GT. 1 ) THEN
grid%srot (ips:min(ide-1,ipe),jms:jme) = grid%srot (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ugwdsfc,1)*SIZE(grid%ugwdsfc,2) .GT. 1 ) THEN
grid%ugwdsfc (ips:min(ide-1,ipe),jms:jme) = grid%ugwdsfc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vgwdsfc,1)*SIZE(grid%vgwdsfc,2) .GT. 1 ) THEN
grid%vgwdsfc (ips:min(ide-1,ipe),jms:jme) = grid%vgwdsfc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ctopo,1)*SIZE(grid%ctopo,2) .GT. 1 ) THEN
grid%ctopo (ips:min(ide-1,ipe),jms:jme) = grid%ctopo (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ctopo2,1)*SIZE(grid%ctopo2,2) .GT. 1 ) THEN
grid%ctopo2 (ips:min(ide-1,ipe),jms:jme) = grid%ctopo2 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dwdtmn,1)*SIZE(grid%dwdtmn,2) .GT. 1 ) THEN
grid%dwdtmn (ips:min(ide-1,ipe),jms:jme) = grid%dwdtmn (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dwdtmx,1)*SIZE(grid%dwdtmx,2) .GT. 1 ) THEN
grid%dwdtmx (ips:min(ide-1,ipe),jms:jme) = grid%dwdtmx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%baro,1)*SIZE(grid%baro,2) .GT. 1 ) THEN
grid%baro (ips:min(ide-1,ipe),jms:jme) = grid%baro (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dwdt,1)*SIZE(grid%dwdt,2) .GT. 1 ) THEN
grid%dwdt (ips:min(ide-1,ipe),jms:jme,:) = grid%dwdt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%pdwdt,1)*SIZE(grid%pdwdt,2) .GT. 1 ) THEN
grid%pdwdt (ips:min(ide-1,ipe),jms:jme,:) = grid%pdwdt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%pint,1)*SIZE(grid%pint,2) .GT. 1 ) THEN
grid%pint (ips:min(ide-1,ipe),jms:jme,:) = grid%pint (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%w,1)*SIZE(grid%w,2) .GT. 1 ) THEN
grid%w (ips:min(ide-1,ipe),jms:jme,:) = grid%w (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%w_tot,1)*SIZE(grid%w_tot,2) .GT. 1 ) THEN
grid%w_tot (ips:min(ide-1,ipe),jms:jme,:) = grid%w_tot (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%z,1)*SIZE(grid%z,2) .GT. 1 ) THEN
grid%z (ips:min(ide-1,ipe),jms:jme,:) = grid%z (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%acfrcv,1)*SIZE(grid%acfrcv,2) .GT. 1 ) THEN
grid%acfrcv (ips:min(ide-1,ipe),jms:jme) = grid%acfrcv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%acfrst,1)*SIZE(grid%acfrst,2) .GT. 1 ) THEN
grid%acfrst (ips:min(ide-1,ipe),jms:jme) = grid%acfrst (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ssroff,1)*SIZE(grid%ssroff,2) .GT. 1 ) THEN
grid%ssroff (ips:min(ide-1,ipe),jms:jme) = grid%ssroff (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%bgroff,1)*SIZE(grid%bgroff,2) .GT. 1 ) THEN
grid%bgroff (ips:min(ide-1,ipe),jms:jme) = grid%bgroff (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rlwin,1)*SIZE(grid%rlwin,2) .GT. 1 ) THEN
grid%rlwin (ips:min(ide-1,ipe),jms:jme) = grid%rlwin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rlwout,1)*SIZE(grid%rlwout,2) .GT. 1 ) THEN
grid%rlwout (ips:min(ide-1,ipe),jms:jme) = grid%rlwout (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rlwtoa,1)*SIZE(grid%rlwtoa,2) .GT. 1 ) THEN
grid%rlwtoa (ips:min(ide-1,ipe),jms:jme) = grid%rlwtoa (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%alwin,1)*SIZE(grid%alwin,2) .GT. 1 ) THEN
grid%alwin (ips:min(ide-1,ipe),jms:jme) = grid%alwin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%alwout,1)*SIZE(grid%alwout,2) .GT. 1 ) THEN
grid%alwout (ips:min(ide-1,ipe),jms:jme) = grid%alwout (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%alwtoa,1)*SIZE(grid%alwtoa,2) .GT. 1 ) THEN
grid%alwtoa (ips:min(ide-1,ipe),jms:jme) = grid%alwtoa (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rswin,1)*SIZE(grid%rswin,2) .GT. 1 ) THEN
grid%rswin (ips:min(ide-1,ipe),jms:jme) = grid%rswin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rswinc,1)*SIZE(grid%rswinc,2) .GT. 1 ) THEN
grid%rswinc (ips:min(ide-1,ipe),jms:jme) = grid%rswinc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rswout,1)*SIZE(grid%rswout,2) .GT. 1 ) THEN
grid%rswout (ips:min(ide-1,ipe),jms:jme) = grid%rswout (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rswtoa,1)*SIZE(grid%rswtoa,2) .GT. 1 ) THEN
grid%rswtoa (ips:min(ide-1,ipe),jms:jme) = grid%rswtoa (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aswin,1)*SIZE(grid%aswin,2) .GT. 1 ) THEN
grid%aswin (ips:min(ide-1,ipe),jms:jme) = grid%aswin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aswout,1)*SIZE(grid%aswout,2) .GT. 1 ) THEN
grid%aswout (ips:min(ide-1,ipe),jms:jme) = grid%aswout (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aswtoa,1)*SIZE(grid%aswtoa,2) .GT. 1 ) THEN
grid%aswtoa (ips:min(ide-1,ipe),jms:jme) = grid%aswtoa (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sfcshx,1)*SIZE(grid%sfcshx,2) .GT. 1 ) THEN
grid%sfcshx (ips:min(ide-1,ipe),jms:jme) = grid%sfcshx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sfclhx,1)*SIZE(grid%sfclhx,2) .GT. 1 ) THEN
grid%sfclhx (ips:min(ide-1,ipe),jms:jme) = grid%sfclhx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%subshx,1)*SIZE(grid%subshx,2) .GT. 1 ) THEN
grid%subshx (ips:min(ide-1,ipe),jms:jme) = grid%subshx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%snopcx,1)*SIZE(grid%snopcx,2) .GT. 1 ) THEN
grid%snopcx (ips:min(ide-1,ipe),jms:jme) = grid%snopcx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sfcuvx,1)*SIZE(grid%sfcuvx,2) .GT. 1 ) THEN
grid%sfcuvx (ips:min(ide-1,ipe),jms:jme) = grid%sfcuvx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%potevp,1)*SIZE(grid%potevp,2) .GT. 1 ) THEN
grid%potevp (ips:min(ide-1,ipe),jms:jme) = grid%potevp (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%potflx,1)*SIZE(grid%potflx,2) .GT. 1 ) THEN
grid%potflx (ips:min(ide-1,ipe),jms:jme) = grid%potflx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tlmin,1)*SIZE(grid%tlmin,2) .GT. 1 ) THEN
grid%tlmin (ips:min(ide-1,ipe),jms:jme) = grid%tlmin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tlmax,1)*SIZE(grid%tlmax,2) .GT. 1 ) THEN
grid%tlmax (ips:min(ide-1,ipe),jms:jme) = grid%tlmax (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%t02_min,1)*SIZE(grid%t02_min,2) .GT. 1 ) THEN
grid%t02_min (ips:min(ide-1,ipe),jms:jme) = grid%t02_min (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%t02_max,1)*SIZE(grid%t02_max,2) .GT. 1 ) THEN
grid%t02_max (ips:min(ide-1,ipe),jms:jme) = grid%t02_max (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rh02_min,1)*SIZE(grid%rh02_min,2) .GT. 1 ) THEN
grid%rh02_min (ips:min(ide-1,ipe),jms:jme) = grid%rh02_min (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rh02_max,1)*SIZE(grid%rh02_max,2) .GT. 1 ) THEN
grid%rh02_max (ips:min(ide-1,ipe),jms:jme) = grid%rh02_max (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rlwtt,1)*SIZE(grid%rlwtt,2) .GT. 1 ) THEN
grid%rlwtt (ips:min(ide-1,ipe),jms:jme,:) = grid%rlwtt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%rswtt,1)*SIZE(grid%rswtt,2) .GT. 1 ) THEN
grid%rswtt (ips:min(ide-1,ipe),jms:jme,:) = grid%rswtt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%tcucn,1)*SIZE(grid%tcucn,2) .GT. 1 ) THEN
grid%tcucn (ips:min(ide-1,ipe),jms:jme,:) = grid%tcucn (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%train,1)*SIZE(grid%train,2) .GT. 1 ) THEN
grid%train (ips:min(ide-1,ipe),jms:jme,:) = grid%train (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%ncfrcv,1)*SIZE(grid%ncfrcv,2) .GT. 1 ) THEN
grid%ncfrcv (ips:min(ide-1,ipe),jms:jme) = grid%ncfrcv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ncfrst,1)*SIZE(grid%ncfrst,2) .GT. 1 ) THEN
grid%ncfrst (ips:min(ide-1,ipe),jms:jme) = grid%ncfrst (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%iup_h,1)*SIZE(grid%iup_h,2) .GT. 1 ) THEN
grid%iup_h (ips:min(ide-1,ipe),jms:jme) = grid%iup_h (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%iup_v,1)*SIZE(grid%iup_v,2) .GT. 1 ) THEN
grid%iup_v (ips:min(ide-1,ipe),jms:jme) = grid%iup_v (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%iup_adh,1)*SIZE(grid%iup_adh,2) .GT. 1 ) THEN
grid%iup_adh (ips:min(ide-1,ipe),jms:jme) = grid%iup_adh (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%iup_adv,1)*SIZE(grid%iup_adv,2) .GT. 1 ) THEN
grid%iup_adv (ips:min(ide-1,ipe),jms:jme) = grid%iup_adv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%winfo,1)*SIZE(grid%winfo,2) .GT. 1 ) THEN
grid%winfo (ips:min(ide-1,ipe),jms:jme,:) = grid%winfo (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%iinfo,1)*SIZE(grid%iinfo,2) .GT. 1 ) THEN
grid%iinfo (ips:min(ide-1,ipe),jms:jme,:) = grid%iinfo (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%imask_nostag,1)*SIZE(grid%imask_nostag,2) .GT. 1 ) THEN
grid%imask_nostag (ips:min(ide-1,ipe),jms:jme) = grid%imask_nostag (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%imask_xstag,1)*SIZE(grid%imask_xstag,2) .GT. 1 ) THEN
grid%imask_xstag (ips:min(ide-1,ipe),jms:jme) = grid%imask_xstag (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%imask_ystag,1)*SIZE(grid%imask_ystag,2) .GT. 1 ) THEN
grid%imask_ystag (ips:min(ide-1,ipe),jms:jme) = grid%imask_ystag (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%imask_xystag,1)*SIZE(grid%imask_xystag,2) .GT. 1 ) THEN
grid%imask_xystag (ips:min(ide-1,ipe),jms:jme) = grid%imask_xystag (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm000007,1)*SIZE(grid%sm000007,2) .GT. 1 ) THEN
grid%sm000007 (ips:min(ide-1,ipe),jms:jme) = grid%sm000007 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm007028,1)*SIZE(grid%sm007028,2) .GT. 1 ) THEN
grid%sm007028 (ips:min(ide-1,ipe),jms:jme) = grid%sm007028 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm028100,1)*SIZE(grid%sm028100,2) .GT. 1 ) THEN
grid%sm028100 (ips:min(ide-1,ipe),jms:jme) = grid%sm028100 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm100255,1)*SIZE(grid%sm100255,2) .GT. 1 ) THEN
grid%sm100255 (ips:min(ide-1,ipe),jms:jme) = grid%sm100255 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st000007,1)*SIZE(grid%st000007,2) .GT. 1 ) THEN
grid%st000007 (ips:min(ide-1,ipe),jms:jme) = grid%st000007 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st007028,1)*SIZE(grid%st007028,2) .GT. 1 ) THEN
grid%st007028 (ips:min(ide-1,ipe),jms:jme) = grid%st007028 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st028100,1)*SIZE(grid%st028100,2) .GT. 1 ) THEN
grid%st028100 (ips:min(ide-1,ipe),jms:jme) = grid%st028100 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st100255,1)*SIZE(grid%st100255,2) .GT. 1 ) THEN
grid%st100255 (ips:min(ide-1,ipe),jms:jme) = grid%st100255 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm000010,1)*SIZE(grid%sm000010,2) .GT. 1 ) THEN
grid%sm000010 (ips:min(ide-1,ipe),jms:jme) = grid%sm000010 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm010040,1)*SIZE(grid%sm010040,2) .GT. 1 ) THEN
grid%sm010040 (ips:min(ide-1,ipe),jms:jme) = grid%sm010040 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm040100,1)*SIZE(grid%sm040100,2) .GT. 1 ) THEN
grid%sm040100 (ips:min(ide-1,ipe),jms:jme) = grid%sm040100 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm100200,1)*SIZE(grid%sm100200,2) .GT. 1 ) THEN
grid%sm100200 (ips:min(ide-1,ipe),jms:jme) = grid%sm100200 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sm010200,1)*SIZE(grid%sm010200,2) .GT. 1 ) THEN
grid%sm010200 (ips:min(ide-1,ipe),jms:jme) = grid%sm010200 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilm000,1)*SIZE(grid%soilm000,2) .GT. 1 ) THEN
grid%soilm000 (ips:min(ide-1,ipe),jms:jme) = grid%soilm000 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilm005,1)*SIZE(grid%soilm005,2) .GT. 1 ) THEN
grid%soilm005 (ips:min(ide-1,ipe),jms:jme) = grid%soilm005 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilm020,1)*SIZE(grid%soilm020,2) .GT. 1 ) THEN
grid%soilm020 (ips:min(ide-1,ipe),jms:jme) = grid%soilm020 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilm040,1)*SIZE(grid%soilm040,2) .GT. 1 ) THEN
grid%soilm040 (ips:min(ide-1,ipe),jms:jme) = grid%soilm040 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilm160,1)*SIZE(grid%soilm160,2) .GT. 1 ) THEN
grid%soilm160 (ips:min(ide-1,ipe),jms:jme) = grid%soilm160 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilm300,1)*SIZE(grid%soilm300,2) .GT. 1 ) THEN
grid%soilm300 (ips:min(ide-1,ipe),jms:jme) = grid%soilm300 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sw000010,1)*SIZE(grid%sw000010,2) .GT. 1 ) THEN
grid%sw000010 (ips:min(ide-1,ipe),jms:jme) = grid%sw000010 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sw010040,1)*SIZE(grid%sw010040,2) .GT. 1 ) THEN
grid%sw010040 (ips:min(ide-1,ipe),jms:jme) = grid%sw010040 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sw040100,1)*SIZE(grid%sw040100,2) .GT. 1 ) THEN
grid%sw040100 (ips:min(ide-1,ipe),jms:jme) = grid%sw040100 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sw100200,1)*SIZE(grid%sw100200,2) .GT. 1 ) THEN
grid%sw100200 (ips:min(ide-1,ipe),jms:jme) = grid%sw100200 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sw010200,1)*SIZE(grid%sw010200,2) .GT. 1 ) THEN
grid%sw010200 (ips:min(ide-1,ipe),jms:jme) = grid%sw010200 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilw000,1)*SIZE(grid%soilw000,2) .GT. 1 ) THEN
grid%soilw000 (ips:min(ide-1,ipe),jms:jme) = grid%soilw000 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilw005,1)*SIZE(grid%soilw005,2) .GT. 1 ) THEN
grid%soilw005 (ips:min(ide-1,ipe),jms:jme) = grid%soilw005 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilw020,1)*SIZE(grid%soilw020,2) .GT. 1 ) THEN
grid%soilw020 (ips:min(ide-1,ipe),jms:jme) = grid%soilw020 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilw040,1)*SIZE(grid%soilw040,2) .GT. 1 ) THEN
grid%soilw040 (ips:min(ide-1,ipe),jms:jme) = grid%soilw040 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilw160,1)*SIZE(grid%soilw160,2) .GT. 1 ) THEN
grid%soilw160 (ips:min(ide-1,ipe),jms:jme) = grid%soilw160 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilw300,1)*SIZE(grid%soilw300,2) .GT. 1 ) THEN
grid%soilw300 (ips:min(ide-1,ipe),jms:jme) = grid%soilw300 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st000010,1)*SIZE(grid%st000010,2) .GT. 1 ) THEN
grid%st000010 (ips:min(ide-1,ipe),jms:jme) = grid%st000010 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st010040,1)*SIZE(grid%st010040,2) .GT. 1 ) THEN
grid%st010040 (ips:min(ide-1,ipe),jms:jme) = grid%st010040 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st040100,1)*SIZE(grid%st040100,2) .GT. 1 ) THEN
grid%st040100 (ips:min(ide-1,ipe),jms:jme) = grid%st040100 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st100200,1)*SIZE(grid%st100200,2) .GT. 1 ) THEN
grid%st100200 (ips:min(ide-1,ipe),jms:jme) = grid%st100200 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%st010200,1)*SIZE(grid%st010200,2) .GT. 1 ) THEN
grid%st010200 (ips:min(ide-1,ipe),jms:jme) = grid%st010200 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt000,1)*SIZE(grid%soilt000,2) .GT. 1 ) THEN
grid%soilt000 (ips:min(ide-1,ipe),jms:jme) = grid%soilt000 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt005,1)*SIZE(grid%soilt005,2) .GT. 1 ) THEN
grid%soilt005 (ips:min(ide-1,ipe),jms:jme) = grid%soilt005 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt020,1)*SIZE(grid%soilt020,2) .GT. 1 ) THEN
grid%soilt020 (ips:min(ide-1,ipe),jms:jme) = grid%soilt020 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt040,1)*SIZE(grid%soilt040,2) .GT. 1 ) THEN
grid%soilt040 (ips:min(ide-1,ipe),jms:jme) = grid%soilt040 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt160,1)*SIZE(grid%soilt160,2) .GT. 1 ) THEN
grid%soilt160 (ips:min(ide-1,ipe),jms:jme) = grid%soilt160 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt300,1)*SIZE(grid%soilt300,2) .GT. 1 ) THEN
grid%soilt300 (ips:min(ide-1,ipe),jms:jme) = grid%soilt300 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%landmask,1)*SIZE(grid%landmask,2) .GT. 1 ) THEN
grid%landmask (ips:min(ide-1,ipe),jms:jme) = grid%landmask (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%topostdv,1)*SIZE(grid%topostdv,2) .GT. 1 ) THEN
grid%topostdv (ips:min(ide-1,ipe),jms:jme) = grid%topostdv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%toposlpx,1)*SIZE(grid%toposlpx,2) .GT. 1 ) THEN
grid%toposlpx (ips:min(ide-1,ipe),jms:jme) = grid%toposlpx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%toposlpy,1)*SIZE(grid%toposlpy,2) .GT. 1 ) THEN
grid%toposlpy (ips:min(ide-1,ipe),jms:jme) = grid%toposlpy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%greenmax,1)*SIZE(grid%greenmax,2) .GT. 1 ) THEN
grid%greenmax (ips:min(ide-1,ipe),jms:jme) = grid%greenmax (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%greenmin,1)*SIZE(grid%greenmin,2) .GT. 1 ) THEN
grid%greenmin (ips:min(ide-1,ipe),jms:jme) = grid%greenmin (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%albedomx,1)*SIZE(grid%albedomx,2) .GT. 1 ) THEN
grid%albedomx (ips:min(ide-1,ipe),jms:jme) = grid%albedomx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%slopecat,1)*SIZE(grid%slopecat,2) .GT. 1 ) THEN
grid%slopecat (ips:min(ide-1,ipe),jms:jme) = grid%slopecat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%toposoil,1)*SIZE(grid%toposoil,2) .GT. 1 ) THEN
grid%toposoil (ips:min(ide-1,ipe),jms:jme) = grid%toposoil (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%landusef,1)*SIZE(grid%landusef,3) .GT. 1 ) THEN
grid%landusef (ips:min(ide-1,ipe),:,jms:jme) = grid%landusef (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%soilctop,1)*SIZE(grid%soilctop,3) .GT. 1 ) THEN
grid%soilctop (ips:min(ide-1,ipe),:,jms:jme) = grid%soilctop (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%soilcbot,1)*SIZE(grid%soilcbot,3) .GT. 1 ) THEN
grid%soilcbot (ips:min(ide-1,ipe),:,jms:jme) = grid%soilcbot (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
  DO itrace = PARAM_FIRST_SCALAR, num_moist
IF ( SIZE(grid%moist,1)*SIZE(grid%moist,2) .GT. 1 ) THEN
grid%moist (ips:min(ide-1,ipe),jms:jme,:,itrace) = grid%moist (ips+px:min(ide-1,ipe)+px,jms:jme,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_dfi_moist
IF ( SIZE(grid%dfi_moist,1)*SIZE(grid%dfi_moist,2) .GT. 1 ) THEN
grid%dfi_moist (ips:min(ide-1,ipe),jms:jme,:,itrace) = grid%dfi_moist (ips+px:min(ide-1,ipe)+px,jms:jme,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_scalar
IF ( SIZE(grid%scalar,1)*SIZE(grid%scalar,2) .GT. 1 ) THEN
grid%scalar (ips:min(ide-1,ipe),jms:jme,:,itrace) = grid%scalar (ips+px:min(ide-1,ipe)+px,jms:jme,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_dfi_scalar
IF ( SIZE(grid%dfi_scalar,1)*SIZE(grid%dfi_scalar,2) .GT. 1 ) THEN
grid%dfi_scalar (ips:min(ide-1,ipe),jms:jme,:,itrace) = grid%dfi_scalar (ips+px:min(ide-1,ipe)+px,jms:jme,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_chem
IF ( SIZE(grid%chem,1)*SIZE(grid%chem,3) .GT. 1 ) THEN
grid%chem (ips:min(ide-1,ipe),:,jms:jme,itrace) = grid%chem (ips+px:min(ide-1,ipe)+px,:,jms:jme,itrace)
ENDIF
  ENDDO
IF ( SIZE(grid%smois,1)*SIZE(grid%smois,3) .GT. 1 ) THEN
grid%smois (ips:min(ide-1,ipe),:,jms:jme) = grid%smois (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%tslb,1)*SIZE(grid%tslb,3) .GT. 1 ) THEN
grid%tslb (ips:min(ide-1,ipe),:,jms:jme) = grid%tslb (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%lake_depth,1)*SIZE(grid%lake_depth,2) .GT. 1 ) THEN
grid%lake_depth (ips:min(ide-1,ipe),jms:jme) = grid%lake_depth (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%gsw,1)*SIZE(grid%gsw,2) .GT. 1 ) THEN
grid%gsw (ips:min(ide-1,ipe),jms:jme) = grid%gsw (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%xland,1)*SIZE(grid%xland,2) .GT. 1 ) THEN
grid%xland (ips:min(ide-1,ipe),jms:jme) = grid%xland (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%raincv,1)*SIZE(grid%raincv,2) .GT. 1 ) THEN
grid%raincv (ips:min(ide-1,ipe),jms:jme) = grid%raincv (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%psfc,1)*SIZE(grid%psfc,2) .GT. 1 ) THEN
grid%psfc (ips:min(ide-1,ipe),jms:jme) = grid%psfc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%th2,1)*SIZE(grid%th2,2) .GT. 1 ) THEN
grid%th2 (ips:min(ide-1,ipe),jms:jme) = grid%th2 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%t2,1)*SIZE(grid%t2,2) .GT. 1 ) THEN
grid%t2 (ips:min(ide-1,ipe),jms:jme) = grid%t2 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%u10,1)*SIZE(grid%u10,2) .GT. 1 ) THEN
grid%u10 (ips:min(ide-1,ipe),jms:jme) = grid%u10 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%v10,1)*SIZE(grid%v10,2) .GT. 1 ) THEN
grid%v10 (ips:min(ide-1,ipe),jms:jme) = grid%v10 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%xice,1)*SIZE(grid%xice,2) .GT. 1 ) THEN
grid%xice (ips:min(ide-1,ipe),jms:jme) = grid%xice (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%icedepth,1)*SIZE(grid%icedepth,2) .GT. 1 ) THEN
grid%icedepth (ips:min(ide-1,ipe),jms:jme) = grid%icedepth (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%albsi,1)*SIZE(grid%albsi,2) .GT. 1 ) THEN
grid%albsi (ips:min(ide-1,ipe),jms:jme) = grid%albsi (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%snowsi,1)*SIZE(grid%snowsi,2) .GT. 1 ) THEN
grid%snowsi (ips:min(ide-1,ipe),jms:jme) = grid%snowsi (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%lai,1)*SIZE(grid%lai,2) .GT. 1 ) THEN
grid%lai (ips:min(ide-1,ipe),jms:jme) = grid%lai (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%smstav,1)*SIZE(grid%smstav,2) .GT. 1 ) THEN
grid%smstav (ips:min(ide-1,ipe),jms:jme) = grid%smstav (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%smstot,1)*SIZE(grid%smstot,2) .GT. 1 ) THEN
grid%smstot (ips:min(ide-1,ipe),jms:jme) = grid%smstot (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soldrain,1)*SIZE(grid%soldrain,2) .GT. 1 ) THEN
grid%soldrain (ips:min(ide-1,ipe),jms:jme) = grid%soldrain (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sfcheadrt,1)*SIZE(grid%sfcheadrt,2) .GT. 1 ) THEN
grid%sfcheadrt (ips:min(ide-1,ipe),jms:jme) = grid%sfcheadrt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%infxsrt,1)*SIZE(grid%infxsrt,2) .GT. 1 ) THEN
grid%infxsrt (ips:min(ide-1,ipe),jms:jme) = grid%infxsrt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sfcrunoff,1)*SIZE(grid%sfcrunoff,2) .GT. 1 ) THEN
grid%sfcrunoff (ips:min(ide-1,ipe),jms:jme) = grid%sfcrunoff (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%udrunoff,1)*SIZE(grid%udrunoff,2) .GT. 1 ) THEN
grid%udrunoff (ips:min(ide-1,ipe),jms:jme) = grid%udrunoff (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ivgtyp,1)*SIZE(grid%ivgtyp,2) .GT. 1 ) THEN
grid%ivgtyp (ips:min(ide-1,ipe),jms:jme) = grid%ivgtyp (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%isltyp,1)*SIZE(grid%isltyp,2) .GT. 1 ) THEN
grid%isltyp (ips:min(ide-1,ipe),jms:jme) = grid%isltyp (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vegfra,1)*SIZE(grid%vegfra,2) .GT. 1 ) THEN
grid%vegfra (ips:min(ide-1,ipe),jms:jme) = grid%vegfra (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sfcevp,1)*SIZE(grid%sfcevp,2) .GT. 1 ) THEN
grid%sfcevp (ips:min(ide-1,ipe),jms:jme) = grid%sfcevp (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%grdflx,1)*SIZE(grid%grdflx,2) .GT. 1 ) THEN
grid%grdflx (ips:min(ide-1,ipe),jms:jme) = grid%grdflx (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%albbck,1)*SIZE(grid%albbck,2) .GT. 1 ) THEN
grid%albbck (ips:min(ide-1,ipe),jms:jme) = grid%albbck (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sfcexc,1)*SIZE(grid%sfcexc,2) .GT. 1 ) THEN
grid%sfcexc (ips:min(ide-1,ipe),jms:jme) = grid%sfcexc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%snotime,1)*SIZE(grid%snotime,2) .GT. 1 ) THEN
grid%snotime (ips:min(ide-1,ipe),jms:jme) = grid%snotime (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%acsnow,1)*SIZE(grid%acsnow,2) .GT. 1 ) THEN
grid%acsnow (ips:min(ide-1,ipe),jms:jme) = grid%acsnow (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%acsnom,1)*SIZE(grid%acsnom,2) .GT. 1 ) THEN
grid%acsnom (ips:min(ide-1,ipe),jms:jme) = grid%acsnom (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rmol,1)*SIZE(grid%rmol,2) .GT. 1 ) THEN
grid%rmol (ips:min(ide-1,ipe),jms:jme) = grid%rmol (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%snow,1)*SIZE(grid%snow,2) .GT. 1 ) THEN
grid%snow (ips:min(ide-1,ipe),jms:jme) = grid%snow (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%canwat,1)*SIZE(grid%canwat,2) .GT. 1 ) THEN
grid%canwat (ips:min(ide-1,ipe),jms:jme) = grid%canwat (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sst,1)*SIZE(grid%sst,2) .GT. 1 ) THEN
grid%sst (ips:min(ide-1,ipe),jms:jme) = grid%sst (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%uoce,1)*SIZE(grid%uoce,2) .GT. 1 ) THEN
grid%uoce (ips:min(ide-1,ipe),jms:jme) = grid%uoce (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%voce,1)*SIZE(grid%voce,2) .GT. 1 ) THEN
grid%voce (ips:min(ide-1,ipe),jms:jme) = grid%voce (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%weasd,1)*SIZE(grid%weasd,2) .GT. 1 ) THEN
grid%weasd (ips:min(ide-1,ipe),jms:jme) = grid%weasd (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%znt,1)*SIZE(grid%znt,2) .GT. 1 ) THEN
grid%znt (ips:min(ide-1,ipe),jms:jme) = grid%znt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%mol,1)*SIZE(grid%mol,2) .GT. 1 ) THEN
grid%mol (ips:min(ide-1,ipe),jms:jme) = grid%mol (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%noahres,1)*SIZE(grid%noahres,2) .GT. 1 ) THEN
grid%noahres (ips:min(ide-1,ipe),jms:jme) = grid%noahres (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tke_pbl,1)*SIZE(grid%tke_pbl,2) .GT. 1 ) THEN
grid%tke_pbl (ips:min(ide-1,ipe),jms:jme,:) = grid%tke_pbl (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%el_pbl,1)*SIZE(grid%el_pbl,3) .GT. 1 ) THEN
grid%el_pbl (ips:min(ide-1,ipe),:,jms:jme) = grid%el_pbl (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%exch_h,1)*SIZE(grid%exch_h,3) .GT. 1 ) THEN
grid%exch_h (ips:min(ide-1,ipe),:,jms:jme) = grid%exch_h (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%exch_m,1)*SIZE(grid%exch_m,3) .GT. 1 ) THEN
grid%exch_m (ips:min(ide-1,ipe),:,jms:jme) = grid%exch_m (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%thz0,1)*SIZE(grid%thz0,2) .GT. 1 ) THEN
grid%thz0 (ips:min(ide-1,ipe),jms:jme) = grid%thz0 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qz0,1)*SIZE(grid%qz0,2) .GT. 1 ) THEN
grid%qz0 (ips:min(ide-1,ipe),jms:jme) = grid%qz0 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%uz0,1)*SIZE(grid%uz0,2) .GT. 1 ) THEN
grid%uz0 (ips:min(ide-1,ipe),jms:jme) = grid%uz0 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vz0,1)*SIZE(grid%vz0,2) .GT. 1 ) THEN
grid%vz0 (ips:min(ide-1,ipe),jms:jme) = grid%vz0 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%flhc,1)*SIZE(grid%flhc,2) .GT. 1 ) THEN
grid%flhc (ips:min(ide-1,ipe),jms:jme) = grid%flhc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%flqc,1)*SIZE(grid%flqc,2) .GT. 1 ) THEN
grid%flqc (ips:min(ide-1,ipe),jms:jme) = grid%flqc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qsg,1)*SIZE(grid%qsg,2) .GT. 1 ) THEN
grid%qsg (ips:min(ide-1,ipe),jms:jme) = grid%qsg (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qvg,1)*SIZE(grid%qvg,2) .GT. 1 ) THEN
grid%qvg (ips:min(ide-1,ipe),jms:jme) = grid%qvg (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qcg,1)*SIZE(grid%qcg,2) .GT. 1 ) THEN
grid%qcg (ips:min(ide-1,ipe),jms:jme) = grid%qcg (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dew,1)*SIZE(grid%dew,2) .GT. 1 ) THEN
grid%dew (ips:min(ide-1,ipe),jms:jme) = grid%dew (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%soilt1,1)*SIZE(grid%soilt1,2) .GT. 1 ) THEN
grid%soilt1 (ips:min(ide-1,ipe),jms:jme) = grid%soilt1 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tsnav,1)*SIZE(grid%tsnav,2) .GT. 1 ) THEN
grid%tsnav (ips:min(ide-1,ipe),jms:jme) = grid%tsnav (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%psfc_out,1)*SIZE(grid%psfc_out,2) .GT. 1 ) THEN
grid%psfc_out (ips:min(ide-1,ipe),jms:jme) = grid%psfc_out (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%uz0h,1)*SIZE(grid%uz0h,2) .GT. 1 ) THEN
grid%uz0h (ips:min(ide-1,ipe),jms:jme) = grid%uz0h (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%vz0h,1)*SIZE(grid%vz0h,2) .GT. 1 ) THEN
grid%vz0h (ips:min(ide-1,ipe),jms:jme) = grid%vz0h (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%dudt,1)*SIZE(grid%dudt,2) .GT. 1 ) THEN
grid%dudt (ips:min(ide-1,ipe),jms:jme,:) = grid%dudt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%dvdt,1)*SIZE(grid%dvdt,2) .GT. 1 ) THEN
grid%dvdt (ips:min(ide-1,ipe),jms:jme,:) = grid%dvdt (ips+px:min(ide-1,ipe)+px,jms:jme,:)
ENDIF
IF ( SIZE(grid%qsfc,1)*SIZE(grid%qsfc,2) .GT. 1 ) THEN
grid%qsfc (ips:min(ide-1,ipe),jms:jme) = grid%qsfc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%akhs,1)*SIZE(grid%akhs,2) .GT. 1 ) THEN
grid%akhs (ips:min(ide-1,ipe),jms:jme) = grid%akhs (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%akms,1)*SIZE(grid%akms,2) .GT. 1 ) THEN
grid%akms (ips:min(ide-1,ipe),jms:jme) = grid%akms (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%htop,1)*SIZE(grid%htop,2) .GT. 1 ) THEN
grid%htop (ips:min(ide-1,ipe),jms:jme) = grid%htop (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbot,1)*SIZE(grid%hbot,2) .GT. 1 ) THEN
grid%hbot (ips:min(ide-1,ipe),jms:jme) = grid%hbot (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%htopr,1)*SIZE(grid%htopr,2) .GT. 1 ) THEN
grid%htopr (ips:min(ide-1,ipe),jms:jme) = grid%htopr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbotr,1)*SIZE(grid%hbotr,2) .GT. 1 ) THEN
grid%hbotr (ips:min(ide-1,ipe),jms:jme) = grid%hbotr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%htopd,1)*SIZE(grid%htopd,2) .GT. 1 ) THEN
grid%htopd (ips:min(ide-1,ipe),jms:jme) = grid%htopd (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbotd,1)*SIZE(grid%hbotd,2) .GT. 1 ) THEN
grid%hbotd (ips:min(ide-1,ipe),jms:jme) = grid%hbotd (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%htops,1)*SIZE(grid%htops,2) .GT. 1 ) THEN
grid%htops (ips:min(ide-1,ipe),jms:jme) = grid%htops (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%hbots,1)*SIZE(grid%hbots,2) .GT. 1 ) THEN
grid%hbots (ips:min(ide-1,ipe),jms:jme) = grid%hbots (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cuppt,1)*SIZE(grid%cuppt,2) .GT. 1 ) THEN
grid%cuppt (ips:min(ide-1,ipe),jms:jme) = grid%cuppt (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cprate,1)*SIZE(grid%cprate,2) .GT. 1 ) THEN
grid%cprate (ips:min(ide-1,ipe),jms:jme) = grid%cprate (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%f_ice_phy,1)*SIZE(grid%f_ice_phy,3) .GT. 1 ) THEN
grid%f_ice_phy (ips:min(ide-1,ipe),:,jms:jme) = grid%f_ice_phy (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%f_rain_phy,1)*SIZE(grid%f_rain_phy,3) .GT. 1 ) THEN
grid%f_rain_phy (ips:min(ide-1,ipe),:,jms:jme) = grid%f_rain_phy (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%f_rimef_phy,1)*SIZE(grid%f_rimef_phy,3) .GT. 1 ) THEN
grid%f_rimef_phy (ips:min(ide-1,ipe),:,jms:jme) = grid%f_rimef_phy (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%mass_flux,1)*SIZE(grid%mass_flux,2) .GT. 1 ) THEN
grid%mass_flux (ips:min(ide-1,ipe),jms:jme) = grid%mass_flux (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_gr,1)*SIZE(grid%apr_gr,2) .GT. 1 ) THEN
grid%apr_gr (ips:min(ide-1,ipe),jms:jme) = grid%apr_gr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_w,1)*SIZE(grid%apr_w,2) .GT. 1 ) THEN
grid%apr_w (ips:min(ide-1,ipe),jms:jme) = grid%apr_w (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_mc,1)*SIZE(grid%apr_mc,2) .GT. 1 ) THEN
grid%apr_mc (ips:min(ide-1,ipe),jms:jme) = grid%apr_mc (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_st,1)*SIZE(grid%apr_st,2) .GT. 1 ) THEN
grid%apr_st (ips:min(ide-1,ipe),jms:jme) = grid%apr_st (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_as,1)*SIZE(grid%apr_as,2) .GT. 1 ) THEN
grid%apr_as (ips:min(ide-1,ipe),jms:jme) = grid%apr_as (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_capma,1)*SIZE(grid%apr_capma,2) .GT. 1 ) THEN
grid%apr_capma (ips:min(ide-1,ipe),jms:jme) = grid%apr_capma (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_capme,1)*SIZE(grid%apr_capme,2) .GT. 1 ) THEN
grid%apr_capme (ips:min(ide-1,ipe),jms:jme) = grid%apr_capme (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%apr_capmi,1)*SIZE(grid%apr_capmi,2) .GT. 1 ) THEN
grid%apr_capmi (ips:min(ide-1,ipe),jms:jme) = grid%apr_capmi (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rthften,1)*SIZE(grid%rthften,3) .GT. 1 ) THEN
grid%rthften (ips:min(ide-1,ipe),:,jms:jme) = grid%rthften (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%rqvften,1)*SIZE(grid%rqvften,3) .GT. 1 ) THEN
grid%rqvften (ips:min(ide-1,ipe),:,jms:jme) = grid%rqvften (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%snowh,1)*SIZE(grid%snowh,2) .GT. 1 ) THEN
grid%snowh (ips:min(ide-1,ipe),jms:jme) = grid%snowh (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rhosn,1)*SIZE(grid%rhosn,2) .GT. 1 ) THEN
grid%rhosn (ips:min(ide-1,ipe),jms:jme) = grid%rhosn (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%smfr3d,1)*SIZE(grid%smfr3d,3) .GT. 1 ) THEN
grid%smfr3d (ips:min(ide-1,ipe),:,jms:jme) = grid%smfr3d (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%keepfr3dflag,1)*SIZE(grid%keepfr3dflag,3) .GT. 1 ) THEN
grid%keepfr3dflag (ips:min(ide-1,ipe),:,jms:jme) = grid%keepfr3dflag (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%rhosnf,1)*SIZE(grid%rhosnf,2) .GT. 1 ) THEN
grid%rhosnf (ips:min(ide-1,ipe),jms:jme) = grid%rhosnf (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%snowfallac,1)*SIZE(grid%snowfallac,2) .GT. 1 ) THEN
grid%snowfallac (ips:min(ide-1,ipe),jms:jme) = grid%snowfallac (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%precipfr,1)*SIZE(grid%precipfr,2) .GT. 1 ) THEN
grid%precipfr (ips:min(ide-1,ipe),jms:jme) = grid%precipfr (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rc_mf,1)*SIZE(grid%rc_mf,3) .GT. 1 ) THEN
grid%rc_mf (ips:min(ide-1,ipe),:,jms:jme) = grid%rc_mf (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%flx4,1)*SIZE(grid%flx4,2) .GT. 1 ) THEN
grid%flx4 (ips:min(ide-1,ipe),jms:jme) = grid%flx4 (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%fvb,1)*SIZE(grid%fvb,2) .GT. 1 ) THEN
grid%fvb (ips:min(ide-1,ipe),jms:jme) = grid%fvb (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%fbur,1)*SIZE(grid%fbur,2) .GT. 1 ) THEN
grid%fbur (ips:min(ide-1,ipe),jms:jme) = grid%fbur (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%fgsn,1)*SIZE(grid%fgsn,2) .GT. 1 ) THEN
grid%fgsn (ips:min(ide-1,ipe),jms:jme) = grid%fgsn (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%isnowxy,1)*SIZE(grid%isnowxy,2) .GT. 1 ) THEN
grid%isnowxy (ips:min(ide-1,ipe),jms:jme) = grid%isnowxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tvxy,1)*SIZE(grid%tvxy,2) .GT. 1 ) THEN
grid%tvxy (ips:min(ide-1,ipe),jms:jme) = grid%tvxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tgxy,1)*SIZE(grid%tgxy,2) .GT. 1 ) THEN
grid%tgxy (ips:min(ide-1,ipe),jms:jme) = grid%tgxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%canicexy,1)*SIZE(grid%canicexy,2) .GT. 1 ) THEN
grid%canicexy (ips:min(ide-1,ipe),jms:jme) = grid%canicexy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%canliqxy,1)*SIZE(grid%canliqxy,2) .GT. 1 ) THEN
grid%canliqxy (ips:min(ide-1,ipe),jms:jme) = grid%canliqxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%eahxy,1)*SIZE(grid%eahxy,2) .GT. 1 ) THEN
grid%eahxy (ips:min(ide-1,ipe),jms:jme) = grid%eahxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tahxy,1)*SIZE(grid%tahxy,2) .GT. 1 ) THEN
grid%tahxy (ips:min(ide-1,ipe),jms:jme) = grid%tahxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%cmxy,1)*SIZE(grid%cmxy,2) .GT. 1 ) THEN
grid%cmxy (ips:min(ide-1,ipe),jms:jme) = grid%cmxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%chxy,1)*SIZE(grid%chxy,2) .GT. 1 ) THEN
grid%chxy (ips:min(ide-1,ipe),jms:jme) = grid%chxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%fwetxy,1)*SIZE(grid%fwetxy,2) .GT. 1 ) THEN
grid%fwetxy (ips:min(ide-1,ipe),jms:jme) = grid%fwetxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sneqvoxy,1)*SIZE(grid%sneqvoxy,2) .GT. 1 ) THEN
grid%sneqvoxy (ips:min(ide-1,ipe),jms:jme) = grid%sneqvoxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%alboldxy,1)*SIZE(grid%alboldxy,2) .GT. 1 ) THEN
grid%alboldxy (ips:min(ide-1,ipe),jms:jme) = grid%alboldxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qsnowxy,1)*SIZE(grid%qsnowxy,2) .GT. 1 ) THEN
grid%qsnowxy (ips:min(ide-1,ipe),jms:jme) = grid%qsnowxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%wslakexy,1)*SIZE(grid%wslakexy,2) .GT. 1 ) THEN
grid%wslakexy (ips:min(ide-1,ipe),jms:jme) = grid%wslakexy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%zwtxy,1)*SIZE(grid%zwtxy,2) .GT. 1 ) THEN
grid%zwtxy (ips:min(ide-1,ipe),jms:jme) = grid%zwtxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%waxy,1)*SIZE(grid%waxy,2) .GT. 1 ) THEN
grid%waxy (ips:min(ide-1,ipe),jms:jme) = grid%waxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%wtxy,1)*SIZE(grid%wtxy,2) .GT. 1 ) THEN
grid%wtxy (ips:min(ide-1,ipe),jms:jme) = grid%wtxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tsnoxy,1)*SIZE(grid%tsnoxy,3) .GT. 1 ) THEN
grid%tsnoxy (ips:min(ide-1,ipe),:,jms:jme) = grid%tsnoxy (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%zsnsoxy,1)*SIZE(grid%zsnsoxy,3) .GT. 1 ) THEN
grid%zsnsoxy (ips:min(ide-1,ipe),:,jms:jme) = grid%zsnsoxy (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%snicexy,1)*SIZE(grid%snicexy,3) .GT. 1 ) THEN
grid%snicexy (ips:min(ide-1,ipe),:,jms:jme) = grid%snicexy (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%snliqxy,1)*SIZE(grid%snliqxy,3) .GT. 1 ) THEN
grid%snliqxy (ips:min(ide-1,ipe),:,jms:jme) = grid%snliqxy (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%lfmassxy,1)*SIZE(grid%lfmassxy,2) .GT. 1 ) THEN
grid%lfmassxy (ips:min(ide-1,ipe),jms:jme) = grid%lfmassxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rtmassxy,1)*SIZE(grid%rtmassxy,2) .GT. 1 ) THEN
grid%rtmassxy (ips:min(ide-1,ipe),jms:jme) = grid%rtmassxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%stmassxy,1)*SIZE(grid%stmassxy,2) .GT. 1 ) THEN
grid%stmassxy (ips:min(ide-1,ipe),jms:jme) = grid%stmassxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%woodxy,1)*SIZE(grid%woodxy,2) .GT. 1 ) THEN
grid%woodxy (ips:min(ide-1,ipe),jms:jme) = grid%woodxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%stblcpxy,1)*SIZE(grid%stblcpxy,2) .GT. 1 ) THEN
grid%stblcpxy (ips:min(ide-1,ipe),jms:jme) = grid%stblcpxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%fastcpxy,1)*SIZE(grid%fastcpxy,2) .GT. 1 ) THEN
grid%fastcpxy (ips:min(ide-1,ipe),jms:jme) = grid%fastcpxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%xsaixy,1)*SIZE(grid%xsaixy,2) .GT. 1 ) THEN
grid%xsaixy (ips:min(ide-1,ipe),jms:jme) = grid%xsaixy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%taussxy,1)*SIZE(grid%taussxy,2) .GT. 1 ) THEN
grid%taussxy (ips:min(ide-1,ipe),jms:jme) = grid%taussxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%t2mvxy,1)*SIZE(grid%t2mvxy,2) .GT. 1 ) THEN
grid%t2mvxy (ips:min(ide-1,ipe),jms:jme) = grid%t2mvxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%t2mbxy,1)*SIZE(grid%t2mbxy,2) .GT. 1 ) THEN
grid%t2mbxy (ips:min(ide-1,ipe),jms:jme) = grid%t2mbxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%q2mvxy,1)*SIZE(grid%q2mvxy,2) .GT. 1 ) THEN
grid%q2mvxy (ips:min(ide-1,ipe),jms:jme) = grid%q2mvxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%q2mbxy,1)*SIZE(grid%q2mbxy,2) .GT. 1 ) THEN
grid%q2mbxy (ips:min(ide-1,ipe),jms:jme) = grid%q2mbxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tradxy,1)*SIZE(grid%tradxy,2) .GT. 1 ) THEN
grid%tradxy (ips:min(ide-1,ipe),jms:jme) = grid%tradxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%neexy,1)*SIZE(grid%neexy,2) .GT. 1 ) THEN
grid%neexy (ips:min(ide-1,ipe),jms:jme) = grid%neexy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%gppxy,1)*SIZE(grid%gppxy,2) .GT. 1 ) THEN
grid%gppxy (ips:min(ide-1,ipe),jms:jme) = grid%gppxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%nppxy,1)*SIZE(grid%nppxy,2) .GT. 1 ) THEN
grid%nppxy (ips:min(ide-1,ipe),jms:jme) = grid%nppxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%fvegxy,1)*SIZE(grid%fvegxy,2) .GT. 1 ) THEN
grid%fvegxy (ips:min(ide-1,ipe),jms:jme) = grid%fvegxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%qinxy,1)*SIZE(grid%qinxy,2) .GT. 1 ) THEN
grid%qinxy (ips:min(ide-1,ipe),jms:jme) = grid%qinxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%runsfxy,1)*SIZE(grid%runsfxy,2) .GT. 1 ) THEN
grid%runsfxy (ips:min(ide-1,ipe),jms:jme) = grid%runsfxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%runsbxy,1)*SIZE(grid%runsbxy,2) .GT. 1 ) THEN
grid%runsbxy (ips:min(ide-1,ipe),jms:jme) = grid%runsbxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ecanxy,1)*SIZE(grid%ecanxy,2) .GT. 1 ) THEN
grid%ecanxy (ips:min(ide-1,ipe),jms:jme) = grid%ecanxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%edirxy,1)*SIZE(grid%edirxy,2) .GT. 1 ) THEN
grid%edirxy (ips:min(ide-1,ipe),jms:jme) = grid%edirxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%etranxy,1)*SIZE(grid%etranxy,2) .GT. 1 ) THEN
grid%etranxy (ips:min(ide-1,ipe),jms:jme) = grid%etranxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%fsaxy,1)*SIZE(grid%fsaxy,2) .GT. 1 ) THEN
grid%fsaxy (ips:min(ide-1,ipe),jms:jme) = grid%fsaxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%firaxy,1)*SIZE(grid%firaxy,2) .GT. 1 ) THEN
grid%firaxy (ips:min(ide-1,ipe),jms:jme) = grid%firaxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%aparxy,1)*SIZE(grid%aparxy,2) .GT. 1 ) THEN
grid%aparxy (ips:min(ide-1,ipe),jms:jme) = grid%aparxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%psnxy,1)*SIZE(grid%psnxy,2) .GT. 1 ) THEN
grid%psnxy (ips:min(ide-1,ipe),jms:jme) = grid%psnxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%savxy,1)*SIZE(grid%savxy,2) .GT. 1 ) THEN
grid%savxy (ips:min(ide-1,ipe),jms:jme) = grid%savxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%sagxy,1)*SIZE(grid%sagxy,2) .GT. 1 ) THEN
grid%sagxy (ips:min(ide-1,ipe),jms:jme) = grid%sagxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rssunxy,1)*SIZE(grid%rssunxy,2) .GT. 1 ) THEN
grid%rssunxy (ips:min(ide-1,ipe),jms:jme) = grid%rssunxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rsshaxy,1)*SIZE(grid%rsshaxy,2) .GT. 1 ) THEN
grid%rsshaxy (ips:min(ide-1,ipe),jms:jme) = grid%rsshaxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%bgapxy,1)*SIZE(grid%bgapxy,2) .GT. 1 ) THEN
grid%bgapxy (ips:min(ide-1,ipe),jms:jme) = grid%bgapxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%wgapxy,1)*SIZE(grid%wgapxy,2) .GT. 1 ) THEN
grid%wgapxy (ips:min(ide-1,ipe),jms:jme) = grid%wgapxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tgvxy,1)*SIZE(grid%tgvxy,2) .GT. 1 ) THEN
grid%tgvxy (ips:min(ide-1,ipe),jms:jme) = grid%tgvxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%tgbxy,1)*SIZE(grid%tgbxy,2) .GT. 1 ) THEN
grid%tgbxy (ips:min(ide-1,ipe),jms:jme) = grid%tgbxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%chvxy,1)*SIZE(grid%chvxy,2) .GT. 1 ) THEN
grid%chvxy (ips:min(ide-1,ipe),jms:jme) = grid%chvxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%chbxy,1)*SIZE(grid%chbxy,2) .GT. 1 ) THEN
grid%chbxy (ips:min(ide-1,ipe),jms:jme) = grid%chbxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%shgxy,1)*SIZE(grid%shgxy,2) .GT. 1 ) THEN
grid%shgxy (ips:min(ide-1,ipe),jms:jme) = grid%shgxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%shcxy,1)*SIZE(grid%shcxy,2) .GT. 1 ) THEN
grid%shcxy (ips:min(ide-1,ipe),jms:jme) = grid%shcxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%shbxy,1)*SIZE(grid%shbxy,2) .GT. 1 ) THEN
grid%shbxy (ips:min(ide-1,ipe),jms:jme) = grid%shbxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%evgxy,1)*SIZE(grid%evgxy,2) .GT. 1 ) THEN
grid%evgxy (ips:min(ide-1,ipe),jms:jme) = grid%evgxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%evbxy,1)*SIZE(grid%evbxy,2) .GT. 1 ) THEN
grid%evbxy (ips:min(ide-1,ipe),jms:jme) = grid%evbxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ghvxy,1)*SIZE(grid%ghvxy,2) .GT. 1 ) THEN
grid%ghvxy (ips:min(ide-1,ipe),jms:jme) = grid%ghvxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ghbxy,1)*SIZE(grid%ghbxy,2) .GT. 1 ) THEN
grid%ghbxy (ips:min(ide-1,ipe),jms:jme) = grid%ghbxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%irgxy,1)*SIZE(grid%irgxy,2) .GT. 1 ) THEN
grid%irgxy (ips:min(ide-1,ipe),jms:jme) = grid%irgxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%ircxy,1)*SIZE(grid%ircxy,2) .GT. 1 ) THEN
grid%ircxy (ips:min(ide-1,ipe),jms:jme) = grid%ircxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%irbxy,1)*SIZE(grid%irbxy,2) .GT. 1 ) THEN
grid%irbxy (ips:min(ide-1,ipe),jms:jme) = grid%irbxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%trxy,1)*SIZE(grid%trxy,2) .GT. 1 ) THEN
grid%trxy (ips:min(ide-1,ipe),jms:jme) = grid%trxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%evcxy,1)*SIZE(grid%evcxy,2) .GT. 1 ) THEN
grid%evcxy (ips:min(ide-1,ipe),jms:jme) = grid%evcxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%chleafxy,1)*SIZE(grid%chleafxy,2) .GT. 1 ) THEN
grid%chleafxy (ips:min(ide-1,ipe),jms:jme) = grid%chleafxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%chucxy,1)*SIZE(grid%chucxy,2) .GT. 1 ) THEN
grid%chucxy (ips:min(ide-1,ipe),jms:jme) = grid%chucxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%chv2xy,1)*SIZE(grid%chv2xy,2) .GT. 1 ) THEN
grid%chv2xy (ips:min(ide-1,ipe),jms:jme) = grid%chv2xy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%chb2xy,1)*SIZE(grid%chb2xy,2) .GT. 1 ) THEN
grid%chb2xy (ips:min(ide-1,ipe),jms:jme) = grid%chb2xy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%chstarxy,1)*SIZE(grid%chstarxy,2) .GT. 1 ) THEN
grid%chstarxy (ips:min(ide-1,ipe),jms:jme) = grid%chstarxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%smoiseq,1)*SIZE(grid%smoiseq,3) .GT. 1 ) THEN
grid%smoiseq (ips:min(ide-1,ipe),:,jms:jme) = grid%smoiseq (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
IF ( SIZE(grid%smcwtdxy,1)*SIZE(grid%smcwtdxy,2) .GT. 1 ) THEN
grid%smcwtdxy (ips:min(ide-1,ipe),jms:jme) = grid%smcwtdxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%rechxy,1)*SIZE(grid%rechxy,2) .GT. 1 ) THEN
grid%rechxy (ips:min(ide-1,ipe),jms:jme) = grid%rechxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%deeprechxy,1)*SIZE(grid%deeprechxy,2) .GT. 1 ) THEN
grid%deeprechxy (ips:min(ide-1,ipe),jms:jme) = grid%deeprechxy (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
IF ( SIZE(grid%power,1)*SIZE(grid%power,2) .GT. 1 ) THEN
grid%power (ips:min(ide-1,ipe),jms:jme) = grid%power (ips+px:min(ide-1,ipe)+px,jms:jme)
ENDIF
  DO itrace = PARAM_FIRST_SCALAR, num_ozmixm
IF ( SIZE(grid%ozmixm,1)*SIZE(grid%ozmixm,3) .GT. 1 ) THEN
grid%ozmixm (ips:min(ide-1,ipe),:,jms:jme,itrace) = grid%ozmixm (ips+px:min(ide-1,ipe)+px,:,jms:jme,itrace)
ENDIF
  ENDDO
IF ( SIZE(grid%o3rad,1)*SIZE(grid%o3rad,3) .GT. 1 ) THEN
grid%o3rad (ips:min(ide-1,ipe),:,jms:jme) = grid%o3rad (ips+px:min(ide-1,ipe)+px,:,jms:jme)
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
