IF ( SIZE(grid%lake2d,1)*SIZE(grid%lake2d,2) .GT. 1 ) THEN
grid%lake2d (ims:ime,jps:min(jde-1,jpe)) = grid%lake2d (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lakedepth2d,1)*SIZE(grid%lakedepth2d,2) .GT. 1 ) THEN
grid%lakedepth2d (ims:ime,jps:min(jde-1,jpe)) = grid%lakedepth2d (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%savedtke12d,1)*SIZE(grid%savedtke12d,2) .GT. 1 ) THEN
grid%savedtke12d (ims:ime,jps:min(jde-1,jpe)) = grid%savedtke12d (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%snowdp2d,1)*SIZE(grid%snowdp2d,2) .GT. 1 ) THEN
grid%snowdp2d (ims:ime,jps:min(jde-1,jpe)) = grid%snowdp2d (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%h2osno2d,1)*SIZE(grid%h2osno2d,2) .GT. 1 ) THEN
grid%h2osno2d (ims:ime,jps:min(jde-1,jpe)) = grid%h2osno2d (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%snl2d,1)*SIZE(grid%snl2d,2) .GT. 1 ) THEN
grid%snl2d (ims:ime,jps:min(jde-1,jpe)) = grid%snl2d (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%t_grnd2d,1)*SIZE(grid%t_grnd2d,2) .GT. 1 ) THEN
grid%t_grnd2d (ims:ime,jps:min(jde-1,jpe)) = grid%t_grnd2d (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%t_lake3d,1)*SIZE(grid%t_lake3d,3) .GT. 1 ) THEN
grid%t_lake3d (ims:ime,:,jps:min(jde-1,jpe)) = grid%t_lake3d (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lake_icefrac3d,1)*SIZE(grid%lake_icefrac3d,3) .GT. 1 ) THEN
grid%lake_icefrac3d (ims:ime,:,jps:min(jde-1,jpe)) = grid%lake_icefrac3d (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%z_lake3d,1)*SIZE(grid%z_lake3d,3) .GT. 1 ) THEN
grid%z_lake3d (ims:ime,:,jps:min(jde-1,jpe)) = grid%z_lake3d (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dz_lake3d,1)*SIZE(grid%dz_lake3d,3) .GT. 1 ) THEN
grid%dz_lake3d (ims:ime,:,jps:min(jde-1,jpe)) = grid%dz_lake3d (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%t_soisno3d,1)*SIZE(grid%t_soisno3d,3) .GT. 1 ) THEN
grid%t_soisno3d (ims:ime,:,jps:min(jde-1,jpe)) = grid%t_soisno3d (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%h2osoi_ice3d,1)*SIZE(grid%h2osoi_ice3d,3) .GT. 1 ) THEN
grid%h2osoi_ice3d (ims:ime,:,jps:min(jde-1,jpe)) = grid%h2osoi_ice3d (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%h2osoi_liq3d,1)*SIZE(grid%h2osoi_liq3d,3) .GT. 1 ) THEN
grid%h2osoi_liq3d (ims:ime,:,jps:min(jde-1,jpe)) = grid%h2osoi_liq3d (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%h2osoi_vol3d,1)*SIZE(grid%h2osoi_vol3d,3) .GT. 1 ) THEN
grid%h2osoi_vol3d (ims:ime,:,jps:min(jde-1,jpe)) = grid%h2osoi_vol3d (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%z3d,1)*SIZE(grid%z3d,3) .GT. 1 ) THEN
grid%z3d (ims:ime,:,jps:min(jde-1,jpe)) = grid%z3d (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dz3d,1)*SIZE(grid%dz3d,3) .GT. 1 ) THEN
grid%dz3d (ims:ime,:,jps:min(jde-1,jpe)) = grid%dz3d (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%zi3d,1)*SIZE(grid%zi3d,3) .GT. 1 ) THEN
grid%zi3d (ims:ime,:,jps:min(jde-1,jpe)) = grid%zi3d (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%watsat3d,1)*SIZE(grid%watsat3d,3) .GT. 1 ) THEN
grid%watsat3d (ims:ime,:,jps:min(jde-1,jpe)) = grid%watsat3d (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%csol3d,1)*SIZE(grid%csol3d,3) .GT. 1 ) THEN
grid%csol3d (ims:ime,:,jps:min(jde-1,jpe)) = grid%csol3d (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tkmg3d,1)*SIZE(grid%tkmg3d,3) .GT. 1 ) THEN
grid%tkmg3d (ims:ime,:,jps:min(jde-1,jpe)) = grid%tkmg3d (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tkdry3d,1)*SIZE(grid%tkdry3d,3) .GT. 1 ) THEN
grid%tkdry3d (ims:ime,:,jps:min(jde-1,jpe)) = grid%tkdry3d (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tksatu3d,1)*SIZE(grid%tksatu3d,3) .GT. 1 ) THEN
grid%tksatu3d (ims:ime,:,jps:min(jde-1,jpe)) = grid%tksatu3d (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
  DO itrace = PARAM_FIRST_SCALAR, num_szj
IF ( SIZE(grid%szj,1)*SIZE(grid%szj,2) .GT. 1 ) THEN
grid%szj (ims:ime,jps:min(jde-1,jpe),:,itrace) = grid%szj (ims:ime,jps+py:min(jde-1,jpe)+py,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_s1z
IF ( SIZE(grid%s1z,1)*SIZE(grid%s1z,2) .GT. 1 ) THEN
grid%s1z (ims:ime,jps:min(jde-1,jpe),:,itrace) = grid%s1z (ims:ime,jps+py:min(jde-1,jpe)+py,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_spz
IF ( SIZE(grid%spz,1)*SIZE(grid%spz,2) .GT. 1 ) THEN
grid%spz (ims:ime,jps:min(jde-1,jpe),:,itrace) = grid%spz (ims:ime,jps+py:min(jde-1,jpe)+py,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_tcs
IF ( SIZE(grid%tcs,1)*SIZE(grid%tcs,2) .GT. 1 ) THEN
grid%tcs (ims:ime,jps:min(jde-1,jpe),:,itrace) = grid%tcs (ims:ime,jps+py:min(jde-1,jpe)+py,:,itrace)
ENDIF
  ENDDO
IF ( SIZE(grid%lu_index,1)*SIZE(grid%lu_index,2) .GT. 1 ) THEN
grid%lu_index (ims:ime,jps:min(jde-1,jpe)) = grid%lu_index (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lu_mask,1)*SIZE(grid%lu_mask,2) .GT. 1 ) THEN
grid%lu_mask (ims:ime,jps:min(jde-1,jpe)) = grid%lu_mask (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%p_gc,1)*SIZE(grid%p_gc,2) .GT. 1 ) THEN
grid%p_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%p_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%vegcat,1)*SIZE(grid%vegcat,2) .GT. 1 ) THEN
grid%vegcat (ims:ime,jps:min(jde-1,jpe)) = grid%vegcat (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilcat,1)*SIZE(grid%soilcat,2) .GT. 1 ) THEN
grid%soilcat (ims:ime,jps:min(jde-1,jpe)) = grid%soilcat (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%input_soil_cat,1)*SIZE(grid%input_soil_cat,2) .GT. 1 ) THEN
grid%input_soil_cat (ims:ime,jps:min(jde-1,jpe)) = grid%input_soil_cat (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tsk_gc,1)*SIZE(grid%tsk_gc,2) .GT. 1 ) THEN
grid%tsk_gc (ims:ime,jps:min(jde-1,jpe)) = grid%tsk_gc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%xice_gc,1)*SIZE(grid%xice_gc,2) .GT. 1 ) THEN
grid%xice_gc (ims:ime,jps:min(jde-1,jpe)) = grid%xice_gc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ght_gc,1)*SIZE(grid%ght_gc,2) .GT. 1 ) THEN
grid%ght_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%ght_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%rh_gc,1)*SIZE(grid%rh_gc,2) .GT. 1 ) THEN
grid%rh_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%rh_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%v_gc,1)*SIZE(grid%v_gc,2) .GT. 1 ) THEN
grid%v_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%v_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%u_gc,1)*SIZE(grid%u_gc,2) .GT. 1 ) THEN
grid%u_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%u_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%t_gc,1)*SIZE(grid%t_gc,2) .GT. 1 ) THEN
grid%t_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%t_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%snoalb,1)*SIZE(grid%snoalb,2) .GT. 1 ) THEN
grid%snoalb (ims:ime,jps:min(jde-1,jpe)) = grid%snoalb (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%greenfrac_gc,1)*SIZE(grid%greenfrac_gc,2) .GT. 1 ) THEN
grid%greenfrac_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%greenfrac_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%albedo12m_gc,1)*SIZE(grid%albedo12m_gc,2) .GT. 1 ) THEN
grid%albedo12m_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%albedo12m_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%lai12m_gc,1)*SIZE(grid%lai12m_gc,2) .GT. 1 ) THEN
grid%lai12m_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%lai12m_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%soilcbot_gc,1)*SIZE(grid%soilcbot_gc,2) .GT. 1 ) THEN
grid%soilcbot_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%soilcbot_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%soilctop_gc,1)*SIZE(grid%soilctop_gc,2) .GT. 1 ) THEN
grid%soilctop_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%soilctop_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%tmn_gc,1)*SIZE(grid%tmn_gc,2) .GT. 1 ) THEN
grid%tmn_gc (ims:ime,jps:min(jde-1,jpe)) = grid%tmn_gc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%htv_gc,1)*SIZE(grid%htv_gc,2) .GT. 1 ) THEN
grid%htv_gc (ims:ime,jps:min(jde-1,jpe)) = grid%htv_gc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ht_gc,1)*SIZE(grid%ht_gc,2) .GT. 1 ) THEN
grid%ht_gc (ims:ime,jps:min(jde-1,jpe)) = grid%ht_gc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%landusef_gc,1)*SIZE(grid%landusef_gc,2) .GT. 1 ) THEN
grid%landusef_gc (ims:ime,jps:min(jde-1,jpe),:) = grid%landusef_gc (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%vlon_gc,1)*SIZE(grid%vlon_gc,2) .GT. 1 ) THEN
grid%vlon_gc (ims:ime,jps:min(jde-1,jpe)) = grid%vlon_gc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vlat_gc,1)*SIZE(grid%vlat_gc,2) .GT. 1 ) THEN
grid%vlat_gc (ims:ime,jps:min(jde-1,jpe)) = grid%vlat_gc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hlon_gc,1)*SIZE(grid%hlon_gc,2) .GT. 1 ) THEN
grid%hlon_gc (ims:ime,jps:min(jde-1,jpe)) = grid%hlon_gc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hlat_gc,1)*SIZE(grid%hlat_gc,2) .GT. 1 ) THEN
grid%hlat_gc (ims:ime,jps:min(jde-1,jpe)) = grid%hlat_gc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%relaxwork,1)*SIZE(grid%relaxwork,2) .GT. 1 ) THEN
grid%relaxwork (ims:ime,jps:min(jde-1,jpe)) = grid%relaxwork (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%relaximask,1)*SIZE(grid%relaximask,2) .GT. 1 ) THEN
grid%relaximask (ims:ime,jps:min(jde-1,jpe)) = grid%relaximask (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%relaxmask,1)*SIZE(grid%relaxmask,2) .GT. 1 ) THEN
grid%relaxmask (ims:ime,jps:min(jde-1,jpe)) = grid%relaxmask (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%interesting,1)*SIZE(grid%interesting,2) .GT. 1 ) THEN
grid%interesting (ims:ime,jps:min(jde-1,jpe)) = grid%interesting (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%precip_swath,1)*SIZE(grid%precip_swath,2) .GT. 1 ) THEN
grid%precip_swath (ims:ime,jps:min(jde-1,jpe)) = grid%precip_swath (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%windsq_swath,1)*SIZE(grid%windsq_swath,2) .GT. 1 ) THEN
grid%windsq_swath (ims:ime,jps:min(jde-1,jpe)) = grid%windsq_swath (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tracker_distsq,1)*SIZE(grid%tracker_distsq,2) .GT. 1 ) THEN
grid%tracker_distsq (ims:ime,jps:min(jde-1,jpe)) = grid%tracker_distsq (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tracker_angle,1)*SIZE(grid%tracker_angle,2) .GT. 1 ) THEN
grid%tracker_angle (ims:ime,jps:min(jde-1,jpe)) = grid%tracker_angle (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tracker_fixes,1)*SIZE(grid%tracker_fixes,2) .GT. 1 ) THEN
grid%tracker_fixes (ims:ime,jps:min(jde-1,jpe)) = grid%tracker_fixes (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%membrane_mslp,1)*SIZE(grid%membrane_mslp,2) .GT. 1 ) THEN
grid%membrane_mslp (ims:ime,jps:min(jde-1,jpe)) = grid%membrane_mslp (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%p850rv,1)*SIZE(grid%p850rv,2) .GT. 1 ) THEN
grid%p850rv (ims:ime,jps:min(jde-1,jpe)) = grid%p850rv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%p700rv,1)*SIZE(grid%p700rv,2) .GT. 1 ) THEN
grid%p700rv (ims:ime,jps:min(jde-1,jpe)) = grid%p700rv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%p850wind,1)*SIZE(grid%p850wind,2) .GT. 1 ) THEN
grid%p850wind (ims:ime,jps:min(jde-1,jpe)) = grid%p850wind (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%p700wind,1)*SIZE(grid%p700wind,2) .GT. 1 ) THEN
grid%p700wind (ims:ime,jps:min(jde-1,jpe)) = grid%p700wind (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%p500u,1)*SIZE(grid%p500u,2) .GT. 1 ) THEN
grid%p500u (ims:ime,jps:min(jde-1,jpe)) = grid%p500u (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%p500v,1)*SIZE(grid%p500v,2) .GT. 1 ) THEN
grid%p500v (ims:ime,jps:min(jde-1,jpe)) = grid%p500v (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%p700u,1)*SIZE(grid%p700u,2) .GT. 1 ) THEN
grid%p700u (ims:ime,jps:min(jde-1,jpe)) = grid%p700u (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%p700v,1)*SIZE(grid%p700v,2) .GT. 1 ) THEN
grid%p700v (ims:ime,jps:min(jde-1,jpe)) = grid%p700v (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%p850u,1)*SIZE(grid%p850u,2) .GT. 1 ) THEN
grid%p850u (ims:ime,jps:min(jde-1,jpe)) = grid%p850u (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%p850v,1)*SIZE(grid%p850v,2) .GT. 1 ) THEN
grid%p850v (ims:ime,jps:min(jde-1,jpe)) = grid%p850v (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%p850z,1)*SIZE(grid%p850z,2) .GT. 1 ) THEN
grid%p850z (ims:ime,jps:min(jde-1,jpe)) = grid%p850z (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%p700z,1)*SIZE(grid%p700z,2) .GT. 1 ) THEN
grid%p700z (ims:ime,jps:min(jde-1,jpe)) = grid%p700z (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%m10wind,1)*SIZE(grid%m10wind,2) .GT. 1 ) THEN
grid%m10wind (ims:ime,jps:min(jde-1,jpe)) = grid%m10wind (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%m10rv,1)*SIZE(grid%m10rv,2) .GT. 1 ) THEN
grid%m10rv (ims:ime,jps:min(jde-1,jpe)) = grid%m10rv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sp850rv,1)*SIZE(grid%sp850rv,2) .GT. 1 ) THEN
grid%sp850rv (ims:ime,jps:min(jde-1,jpe)) = grid%sp850rv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sp700rv,1)*SIZE(grid%sp700rv,2) .GT. 1 ) THEN
grid%sp700rv (ims:ime,jps:min(jde-1,jpe)) = grid%sp700rv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sp850wind,1)*SIZE(grid%sp850wind,2) .GT. 1 ) THEN
grid%sp850wind (ims:ime,jps:min(jde-1,jpe)) = grid%sp850wind (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sp700wind,1)*SIZE(grid%sp700wind,2) .GT. 1 ) THEN
grid%sp700wind (ims:ime,jps:min(jde-1,jpe)) = grid%sp700wind (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sp850z,1)*SIZE(grid%sp850z,2) .GT. 1 ) THEN
grid%sp850z (ims:ime,jps:min(jde-1,jpe)) = grid%sp850z (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sp700z,1)*SIZE(grid%sp700z,2) .GT. 1 ) THEN
grid%sp700z (ims:ime,jps:min(jde-1,jpe)) = grid%sp700z (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm10wind,1)*SIZE(grid%sm10wind,2) .GT. 1 ) THEN
grid%sm10wind (ims:ime,jps:min(jde-1,jpe)) = grid%sm10wind (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm10rv,1)*SIZE(grid%sm10rv,2) .GT. 1 ) THEN
grid%sm10rv (ims:ime,jps:min(jde-1,jpe)) = grid%sm10rv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%smslp,1)*SIZE(grid%smslp,2) .GT. 1 ) THEN
grid%smslp (ims:ime,jps:min(jde-1,jpe)) = grid%smslp (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%distsq,1)*SIZE(grid%distsq,2) .GT. 1 ) THEN
grid%distsq (ims:ime,jps:min(jde-1,jpe)) = grid%distsq (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%weightout,1)*SIZE(grid%weightout,2) .GT. 1 ) THEN
grid%weightout (ims:ime,jps:min(jde-1,jpe)) = grid%weightout (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%mslp_noisy,1)*SIZE(grid%mslp_noisy,2) .GT. 1 ) THEN
grid%mslp_noisy (ims:ime,jps:min(jde-1,jpe)) = grid%mslp_noisy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%pdyn_smooth,1)*SIZE(grid%pdyn_smooth,2) .GT. 1 ) THEN
grid%pdyn_smooth (ims:ime,jps:min(jde-1,jpe)) = grid%pdyn_smooth (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%pdyn_parent,1)*SIZE(grid%pdyn_parent,2) .GT. 1 ) THEN
grid%pdyn_parent (ims:ime,jps:min(jde-1,jpe)) = grid%pdyn_parent (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%pdyn,1)*SIZE(grid%pdyn,2) .GT. 1 ) THEN
grid%pdyn (ims:ime,jps:min(jde-1,jpe)) = grid%pdyn (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%mslp,1)*SIZE(grid%mslp,2) .GT. 1 ) THEN
grid%mslp (ims:ime,jps:min(jde-1,jpe)) = grid%mslp (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%best_mslp,1)*SIZE(grid%best_mslp,2) .GT. 1 ) THEN
grid%best_mslp (ims:ime,jps:min(jde-1,jpe)) = grid%best_mslp (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sqws,1)*SIZE(grid%sqws,2) .GT. 1 ) THEN
grid%sqws (ims:ime,jps:min(jde-1,jpe)) = grid%sqws (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ducudt,1)*SIZE(grid%ducudt,2) .GT. 1 ) THEN
grid%ducudt (ims:ime,jps:min(jde-1,jpe),:) = grid%ducudt (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dvcudt,1)*SIZE(grid%dvcudt,2) .GT. 1 ) THEN
grid%dvcudt (ims:ime,jps:min(jde-1,jpe),:) = grid%dvcudt (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%randstate1,1)*SIZE(grid%randstate1,2) .GT. 1 ) THEN
grid%randstate1 (ims:ime,jps:min(jde-1,jpe)) = grid%randstate1 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%randstate2,1)*SIZE(grid%randstate2,2) .GT. 1 ) THEN
grid%randstate2 (ims:ime,jps:min(jde-1,jpe)) = grid%randstate2 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%randstate3,1)*SIZE(grid%randstate3,2) .GT. 1 ) THEN
grid%randstate3 (ims:ime,jps:min(jde-1,jpe)) = grid%randstate3 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%randstate4,1)*SIZE(grid%randstate4,2) .GT. 1 ) THEN
grid%randstate4 (ims:ime,jps:min(jde-1,jpe)) = grid%randstate4 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%random,1)*SIZE(grid%random,2) .GT. 1 ) THEN
grid%random (ims:ime,jps:min(jde-1,jpe)) = grid%random (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%iih,1)*SIZE(grid%iih,2) .GT. 1 ) THEN
grid%iih (ims:ime,jps:min(jde-1,jpe)) = grid%iih (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%jjh,1)*SIZE(grid%jjh,2) .GT. 1 ) THEN
grid%jjh (ims:ime,jps:min(jde-1,jpe)) = grid%jjh (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%iiv,1)*SIZE(grid%iiv,2) .GT. 1 ) THEN
grid%iiv (ims:ime,jps:min(jde-1,jpe)) = grid%iiv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%jjv,1)*SIZE(grid%jjv,2) .GT. 1 ) THEN
grid%jjv (ims:ime,jps:min(jde-1,jpe)) = grid%jjv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hnear_i,1)*SIZE(grid%hnear_i,2) .GT. 1 ) THEN
grid%hnear_i (ims:ime,jps:min(jde-1,jpe)) = grid%hnear_i (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hnear_j,1)*SIZE(grid%hnear_j,2) .GT. 1 ) THEN
grid%hnear_j (ims:ime,jps:min(jde-1,jpe)) = grid%hnear_j (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hbwgt1,1)*SIZE(grid%hbwgt1,2) .GT. 1 ) THEN
grid%hbwgt1 (ims:ime,jps:min(jde-1,jpe)) = grid%hbwgt1 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hbwgt2,1)*SIZE(grid%hbwgt2,2) .GT. 1 ) THEN
grid%hbwgt2 (ims:ime,jps:min(jde-1,jpe)) = grid%hbwgt2 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hbwgt3,1)*SIZE(grid%hbwgt3,2) .GT. 1 ) THEN
grid%hbwgt3 (ims:ime,jps:min(jde-1,jpe)) = grid%hbwgt3 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hbwgt4,1)*SIZE(grid%hbwgt4,2) .GT. 1 ) THEN
grid%hbwgt4 (ims:ime,jps:min(jde-1,jpe)) = grid%hbwgt4 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vbwgt1,1)*SIZE(grid%vbwgt1,2) .GT. 1 ) THEN
grid%vbwgt1 (ims:ime,jps:min(jde-1,jpe)) = grid%vbwgt1 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vbwgt2,1)*SIZE(grid%vbwgt2,2) .GT. 1 ) THEN
grid%vbwgt2 (ims:ime,jps:min(jde-1,jpe)) = grid%vbwgt2 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vbwgt3,1)*SIZE(grid%vbwgt3,2) .GT. 1 ) THEN
grid%vbwgt3 (ims:ime,jps:min(jde-1,jpe)) = grid%vbwgt3 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vbwgt4,1)*SIZE(grid%vbwgt4,2) .GT. 1 ) THEN
grid%vbwgt4 (ims:ime,jps:min(jde-1,jpe)) = grid%vbwgt4 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hlon,1)*SIZE(grid%hlon,2) .GT. 1 ) THEN
grid%hlon (ims:ime,jps:min(jde-1,jpe)) = grid%hlon (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hlat,1)*SIZE(grid%hlat,2) .GT. 1 ) THEN
grid%hlat (ims:ime,jps:min(jde-1,jpe)) = grid%hlat (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vlon,1)*SIZE(grid%vlon,2) .GT. 1 ) THEN
grid%vlon (ims:ime,jps:min(jde-1,jpe)) = grid%vlon (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vlat,1)*SIZE(grid%vlat,2) .GT. 1 ) THEN
grid%vlat (ims:ime,jps:min(jde-1,jpe)) = grid%vlat (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tg_max_m10wind,1)*SIZE(grid%tg_max_m10wind,2) .GT. 1 ) THEN
grid%tg_max_m10wind (ims:ime,jps:min(jde-1,jpe)) = grid%tg_max_m10wind (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tg_max_wwind,1)*SIZE(grid%tg_max_wwind,2) .GT. 1 ) THEN
grid%tg_max_wwind (ims:ime,jps:min(jde-1,jpe)) = grid%tg_max_wwind (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tg_min_wwind,1)*SIZE(grid%tg_min_wwind,2) .GT. 1 ) THEN
grid%tg_min_wwind (ims:ime,jps:min(jde-1,jpe)) = grid%tg_min_wwind (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tg_max_zhel_25,1)*SIZE(grid%tg_max_zhel_25,2) .GT. 1 ) THEN
grid%tg_max_zhel_25 (ims:ime,jps:min(jde-1,jpe)) = grid%tg_max_zhel_25 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tg_min_zhel_25,1)*SIZE(grid%tg_min_zhel_25,2) .GT. 1 ) THEN
grid%tg_min_zhel_25 (ims:ime,jps:min(jde-1,jpe)) = grid%tg_min_zhel_25 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tg_max_zhel_03,1)*SIZE(grid%tg_max_zhel_03,2) .GT. 1 ) THEN
grid%tg_max_zhel_03 (ims:ime,jps:min(jde-1,jpe)) = grid%tg_max_zhel_03 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tg_min_zhel_03,1)*SIZE(grid%tg_min_zhel_03,2) .GT. 1 ) THEN
grid%tg_min_zhel_03 (ims:ime,jps:min(jde-1,jpe)) = grid%tg_min_zhel_03 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tg_updhel25,1)*SIZE(grid%tg_updhel25,2) .GT. 1 ) THEN
grid%tg_updhel25 (ims:ime,jps:min(jde-1,jpe)) = grid%tg_updhel25 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tg_max_updhel25,1)*SIZE(grid%tg_max_updhel25,2) .GT. 1 ) THEN
grid%tg_max_updhel25 (ims:ime,jps:min(jde-1,jpe)) = grid%tg_max_updhel25 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tg_updhel03,1)*SIZE(grid%tg_updhel03,2) .GT. 1 ) THEN
grid%tg_updhel03 (ims:ime,jps:min(jde-1,jpe)) = grid%tg_updhel03 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tg_max_updhel03,1)*SIZE(grid%tg_max_updhel03,2) .GT. 1 ) THEN
grid%tg_max_updhel03 (ims:ime,jps:min(jde-1,jpe)) = grid%tg_max_updhel03 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tg_total_precip,1)*SIZE(grid%tg_total_precip,2) .GT. 1 ) THEN
grid%tg_total_precip (ims:ime,jps:min(jde-1,jpe)) = grid%tg_total_precip (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tlow,1)*SIZE(grid%tlow,2) .GT. 1 ) THEN
grid%tlow (ims:ime,jps:min(jde-1,jpe)) = grid%tlow (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%zlow,1)*SIZE(grid%zlow,2) .GT. 1 ) THEN
grid%zlow (ims:ime,jps:min(jde-1,jpe)) = grid%zlow (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rotangle,1)*SIZE(grid%rotangle,2) .GT. 1 ) THEN
grid%rotangle (ims:ime,jps:min(jde-1,jpe)) = grid%rotangle (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hres_fis,1)*SIZE(grid%hres_fis,2) .GT. 1 ) THEN
grid%hres_fis (ims:ime,jps:min(jde-1,jpe)) = grid%hres_fis (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hres_avc,1)*SIZE(grid%hres_avc,2) .GT. 1 ) THEN
grid%hres_avc (ims:ime,jps:min(jde-1,jpe)) = grid%hres_avc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hres_lnd,1)*SIZE(grid%hres_lnd,2) .GT. 1 ) THEN
grid%hres_lnd (ims:ime,jps:min(jde-1,jpe)) = grid%hres_lnd (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hbm2,1)*SIZE(grid%hbm2,2) .GT. 1 ) THEN
grid%hbm2 (ims:ime,jps:min(jde-1,jpe)) = grid%hbm2 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hbm3,1)*SIZE(grid%hbm3,2) .GT. 1 ) THEN
grid%hbm3 (ims:ime,jps:min(jde-1,jpe)) = grid%hbm3 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vbm2,1)*SIZE(grid%vbm2,2) .GT. 1 ) THEN
grid%vbm2 (ims:ime,jps:min(jde-1,jpe)) = grid%vbm2 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vbm3,1)*SIZE(grid%vbm3,2) .GT. 1 ) THEN
grid%vbm3 (ims:ime,jps:min(jde-1,jpe)) = grid%vbm3 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm,1)*SIZE(grid%sm,2) .GT. 1 ) THEN
grid%sm (ims:ime,jps:min(jde-1,jpe)) = grid%sm (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sice,1)*SIZE(grid%sice,2) .GT. 1 ) THEN
grid%sice (ims:ime,jps:min(jde-1,jpe)) = grid%sice (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%pd,1)*SIZE(grid%pd,2) .GT. 1 ) THEN
grid%pd (ims:ime,jps:min(jde-1,jpe)) = grid%pd (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%fis,1)*SIZE(grid%fis,2) .GT. 1 ) THEN
grid%fis (ims:ime,jps:min(jde-1,jpe)) = grid%fis (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%res,1)*SIZE(grid%res,2) .GT. 1 ) THEN
grid%res (ims:ime,jps:min(jde-1,jpe)) = grid%res (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%t,1)*SIZE(grid%t,2) .GT. 1 ) THEN
grid%t (ims:ime,jps:min(jde-1,jpe),:) = grid%t (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%q,1)*SIZE(grid%q,2) .GT. 1 ) THEN
grid%q (ims:ime,jps:min(jde-1,jpe),:) = grid%q (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%test_vgrid,1)*SIZE(grid%test_vgrid,2) .GT. 1 ) THEN
grid%test_vgrid (ims:ime,jps:min(jde-1,jpe)) = grid%test_vgrid (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%u,1)*SIZE(grid%u,2) .GT. 1 ) THEN
grid%u (ims:ime,jps:min(jde-1,jpe),:) = grid%u (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%v,1)*SIZE(grid%v,2) .GT. 1 ) THEN
grid%v (ims:ime,jps:min(jde-1,jpe),:) = grid%v (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%told,1)*SIZE(grid%told,2) .GT. 1 ) THEN
grid%told (ims:ime,jps:min(jde-1,jpe),:) = grid%told (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%uold,1)*SIZE(grid%uold,2) .GT. 1 ) THEN
grid%uold (ims:ime,jps:min(jde-1,jpe),:) = grid%uold (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%vold,1)*SIZE(grid%vold,2) .GT. 1 ) THEN
grid%vold (ims:ime,jps:min(jde-1,jpe),:) = grid%vold (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dfi_pd,1)*SIZE(grid%dfi_pd,2) .GT. 1 ) THEN
grid%dfi_pd (ims:ime,jps:min(jde-1,jpe)) = grid%dfi_pd (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dfi_pint,1)*SIZE(grid%dfi_pint,2) .GT. 1 ) THEN
grid%dfi_pint (ims:ime,jps:min(jde-1,jpe),:) = grid%dfi_pint (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dfi_dwdt,1)*SIZE(grid%dfi_dwdt,2) .GT. 1 ) THEN
grid%dfi_dwdt (ims:ime,jps:min(jde-1,jpe),:) = grid%dfi_dwdt (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dfi_t,1)*SIZE(grid%dfi_t,2) .GT. 1 ) THEN
grid%dfi_t (ims:ime,jps:min(jde-1,jpe),:) = grid%dfi_t (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dfi_q,1)*SIZE(grid%dfi_q,2) .GT. 1 ) THEN
grid%dfi_q (ims:ime,jps:min(jde-1,jpe),:) = grid%dfi_q (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dfi_u,1)*SIZE(grid%dfi_u,2) .GT. 1 ) THEN
grid%dfi_u (ims:ime,jps:min(jde-1,jpe),:) = grid%dfi_u (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dfi_v,1)*SIZE(grid%dfi_v,2) .GT. 1 ) THEN
grid%dfi_v (ims:ime,jps:min(jde-1,jpe),:) = grid%dfi_v (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dfi_q2,1)*SIZE(grid%dfi_q2,2) .GT. 1 ) THEN
grid%dfi_q2 (ims:ime,jps:min(jde-1,jpe),:) = grid%dfi_q2 (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dfi_cwm,1)*SIZE(grid%dfi_cwm,2) .GT. 1 ) THEN
grid%dfi_cwm (ims:ime,jps:min(jde-1,jpe),:) = grid%dfi_cwm (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dfi_rrw,1)*SIZE(grid%dfi_rrw,2) .GT. 1 ) THEN
grid%dfi_rrw (ims:ime,jps:min(jde-1,jpe),:) = grid%dfi_rrw (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dfi_stc,1)*SIZE(grid%dfi_stc,3) .GT. 1 ) THEN
grid%dfi_stc (ims:ime,:,jps:min(jde-1,jpe)) = grid%dfi_stc (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dfi_smc,1)*SIZE(grid%dfi_smc,3) .GT. 1 ) THEN
grid%dfi_smc (ims:ime,:,jps:min(jde-1,jpe)) = grid%dfi_smc (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dfi_sh2o,1)*SIZE(grid%dfi_sh2o,3) .GT. 1 ) THEN
grid%dfi_sh2o (ims:ime,:,jps:min(jde-1,jpe)) = grid%dfi_sh2o (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dfi_snow,1)*SIZE(grid%dfi_snow,2) .GT. 1 ) THEN
grid%dfi_snow (ims:ime,jps:min(jde-1,jpe)) = grid%dfi_snow (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dfi_snowh,1)*SIZE(grid%dfi_snowh,2) .GT. 1 ) THEN
grid%dfi_snowh (ims:ime,jps:min(jde-1,jpe)) = grid%dfi_snowh (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dfi_canwat,1)*SIZE(grid%dfi_canwat,2) .GT. 1 ) THEN
grid%dfi_canwat (ims:ime,jps:min(jde-1,jpe)) = grid%dfi_canwat (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dfi_nmm_tsk,1)*SIZE(grid%dfi_nmm_tsk,2) .GT. 1 ) THEN
grid%dfi_nmm_tsk (ims:ime,jps:min(jde-1,jpe)) = grid%dfi_nmm_tsk (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dfi_snowc,1)*SIZE(grid%dfi_snowc,2) .GT. 1 ) THEN
grid%dfi_snowc (ims:ime,jps:min(jde-1,jpe)) = grid%dfi_snowc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dx_nmm,1)*SIZE(grid%dx_nmm,2) .GT. 1 ) THEN
grid%dx_nmm (ims:ime,jps:min(jde-1,jpe)) = grid%dx_nmm (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%wpdar,1)*SIZE(grid%wpdar,2) .GT. 1 ) THEN
grid%wpdar (ims:ime,jps:min(jde-1,jpe)) = grid%wpdar (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cpgfu,1)*SIZE(grid%cpgfu,2) .GT. 1 ) THEN
grid%cpgfu (ims:ime,jps:min(jde-1,jpe)) = grid%cpgfu (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%curv,1)*SIZE(grid%curv,2) .GT. 1 ) THEN
grid%curv (ims:ime,jps:min(jde-1,jpe)) = grid%curv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%fcp,1)*SIZE(grid%fcp,2) .GT. 1 ) THEN
grid%fcp (ims:ime,jps:min(jde-1,jpe)) = grid%fcp (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%fdiv,1)*SIZE(grid%fdiv,2) .GT. 1 ) THEN
grid%fdiv (ims:ime,jps:min(jde-1,jpe)) = grid%fdiv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%f,1)*SIZE(grid%f,2) .GT. 1 ) THEN
grid%f (ims:ime,jps:min(jde-1,jpe)) = grid%f (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%fad,1)*SIZE(grid%fad,2) .GT. 1 ) THEN
grid%fad (ims:ime,jps:min(jde-1,jpe)) = grid%fad (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ddmpu,1)*SIZE(grid%ddmpu,2) .GT. 1 ) THEN
grid%ddmpu (ims:ime,jps:min(jde-1,jpe)) = grid%ddmpu (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ddmpv,1)*SIZE(grid%ddmpv,2) .GT. 1 ) THEN
grid%ddmpv (ims:ime,jps:min(jde-1,jpe)) = grid%ddmpv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%adt,1)*SIZE(grid%adt,2) .GT. 1 ) THEN
grid%adt (ims:ime,jps:min(jde-1,jpe)) = grid%adt (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%adu,1)*SIZE(grid%adu,2) .GT. 1 ) THEN
grid%adu (ims:ime,jps:min(jde-1,jpe)) = grid%adu (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%adv,1)*SIZE(grid%adv,2) .GT. 1 ) THEN
grid%adv (ims:ime,jps:min(jde-1,jpe)) = grid%adv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%pdsl,1)*SIZE(grid%pdsl,2) .GT. 1 ) THEN
grid%pdsl (ims:ime,jps:min(jde-1,jpe)) = grid%pdsl (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%pdslo,1)*SIZE(grid%pdslo,2) .GT. 1 ) THEN
grid%pdslo (ims:ime,jps:min(jde-1,jpe)) = grid%pdslo (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%psdt,1)*SIZE(grid%psdt,2) .GT. 1 ) THEN
grid%psdt (ims:ime,jps:min(jde-1,jpe)) = grid%psdt (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%div,1)*SIZE(grid%div,2) .GT. 1 ) THEN
grid%div (ims:ime,jps:min(jde-1,jpe),:) = grid%div (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%def3d,1)*SIZE(grid%def3d,2) .GT. 1 ) THEN
grid%def3d (ims:ime,jps:min(jde-1,jpe),:) = grid%def3d (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%few,1)*SIZE(grid%few,2) .GT. 1 ) THEN
grid%few (ims:ime,jps:min(jde-1,jpe),:) = grid%few (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%fne,1)*SIZE(grid%fne,2) .GT. 1 ) THEN
grid%fne (ims:ime,jps:min(jde-1,jpe),:) = grid%fne (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%fns,1)*SIZE(grid%fns,2) .GT. 1 ) THEN
grid%fns (ims:ime,jps:min(jde-1,jpe),:) = grid%fns (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%fse,1)*SIZE(grid%fse,2) .GT. 1 ) THEN
grid%fse (ims:ime,jps:min(jde-1,jpe),:) = grid%fse (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%omgalf,1)*SIZE(grid%omgalf,2) .GT. 1 ) THEN
grid%omgalf (ims:ime,jps:min(jde-1,jpe),:) = grid%omgalf (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%petdt,1)*SIZE(grid%petdt,2) .GT. 1 ) THEN
grid%petdt (ims:ime,jps:min(jde-1,jpe),:) = grid%petdt (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%rtop,1)*SIZE(grid%rtop,2) .GT. 1 ) THEN
grid%rtop (ims:ime,jps:min(jde-1,jpe),:) = grid%rtop (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%pblh,1)*SIZE(grid%pblh,2) .GT. 1 ) THEN
grid%pblh (ims:ime,jps:min(jde-1,jpe)) = grid%pblh (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lpbl,1)*SIZE(grid%lpbl,2) .GT. 1 ) THEN
grid%lpbl (ims:ime,jps:min(jde-1,jpe)) = grid%lpbl (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%mixht,1)*SIZE(grid%mixht,2) .GT. 1 ) THEN
grid%mixht (ims:ime,jps:min(jde-1,jpe)) = grid%mixht (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ustar,1)*SIZE(grid%ustar,2) .GT. 1 ) THEN
grid%ustar (ims:ime,jps:min(jde-1,jpe)) = grid%ustar (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%z0,1)*SIZE(grid%z0,2) .GT. 1 ) THEN
grid%z0 (ims:ime,jps:min(jde-1,jpe)) = grid%z0 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%mz0,1)*SIZE(grid%mz0,2) .GT. 1 ) THEN
grid%mz0 (ims:ime,jps:min(jde-1,jpe)) = grid%mz0 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rc2d,1)*SIZE(grid%rc2d,2) .GT. 1 ) THEN
grid%rc2d (ims:ime,jps:min(jde-1,jpe)) = grid%rc2d (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dku3d,1)*SIZE(grid%dku3d,2) .GT. 1 ) THEN
grid%dku3d (ims:ime,jps:min(jde-1,jpe),:) = grid%dku3d (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dkt3d,1)*SIZE(grid%dkt3d,2) .GT. 1 ) THEN
grid%dkt3d (ims:ime,jps:min(jde-1,jpe),:) = grid%dkt3d (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%hpbl2d,1)*SIZE(grid%hpbl2d,2) .GT. 1 ) THEN
grid%hpbl2d (ims:ime,jps:min(jde-1,jpe)) = grid%hpbl2d (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%heat2d,1)*SIZE(grid%heat2d,2) .GT. 1 ) THEN
grid%heat2d (ims:ime,jps:min(jde-1,jpe)) = grid%heat2d (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%evap2d,1)*SIZE(grid%evap2d,2) .GT. 1 ) THEN
grid%evap2d (ims:ime,jps:min(jde-1,jpe)) = grid%evap2d (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%z0base,1)*SIZE(grid%z0base,2) .GT. 1 ) THEN
grid%z0base (ims:ime,jps:min(jde-1,jpe)) = grid%z0base (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ths,1)*SIZE(grid%ths,2) .GT. 1 ) THEN
grid%ths (ims:ime,jps:min(jde-1,jpe)) = grid%ths (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%mavail,1)*SIZE(grid%mavail,2) .GT. 1 ) THEN
grid%mavail (ims:ime,jps:min(jde-1,jpe)) = grid%mavail (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%qsh,1)*SIZE(grid%qsh,2) .GT. 1 ) THEN
grid%qsh (ims:ime,jps:min(jde-1,jpe)) = grid%qsh (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%twbs,1)*SIZE(grid%twbs,2) .GT. 1 ) THEN
grid%twbs (ims:ime,jps:min(jde-1,jpe)) = grid%twbs (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%qwbs,1)*SIZE(grid%qwbs,2) .GT. 1 ) THEN
grid%qwbs (ims:ime,jps:min(jde-1,jpe)) = grid%qwbs (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%taux,1)*SIZE(grid%taux,2) .GT. 1 ) THEN
grid%taux (ims:ime,jps:min(jde-1,jpe)) = grid%taux (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tauy,1)*SIZE(grid%tauy,2) .GT. 1 ) THEN
grid%tauy (ims:ime,jps:min(jde-1,jpe)) = grid%tauy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%prec,1)*SIZE(grid%prec,2) .GT. 1 ) THEN
grid%prec (ims:ime,jps:min(jde-1,jpe)) = grid%prec (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%aprec,1)*SIZE(grid%aprec,2) .GT. 1 ) THEN
grid%aprec (ims:ime,jps:min(jde-1,jpe)) = grid%aprec (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%acprec,1)*SIZE(grid%acprec,2) .GT. 1 ) THEN
grid%acprec (ims:ime,jps:min(jde-1,jpe)) = grid%acprec (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cuprec,1)*SIZE(grid%cuprec,2) .GT. 1 ) THEN
grid%cuprec (ims:ime,jps:min(jde-1,jpe)) = grid%cuprec (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lspa,1)*SIZE(grid%lspa,2) .GT. 1 ) THEN
grid%lspa (ims:ime,jps:min(jde-1,jpe)) = grid%lspa (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ddata,1)*SIZE(grid%ddata,2) .GT. 1 ) THEN
grid%ddata (ims:ime,jps:min(jde-1,jpe)) = grid%ddata (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%accliq,1)*SIZE(grid%accliq,2) .GT. 1 ) THEN
grid%accliq (ims:ime,jps:min(jde-1,jpe)) = grid%accliq (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sno,1)*SIZE(grid%sno,2) .GT. 1 ) THEN
grid%sno (ims:ime,jps:min(jde-1,jpe)) = grid%sno (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%si,1)*SIZE(grid%si,2) .GT. 1 ) THEN
grid%si (ims:ime,jps:min(jde-1,jpe)) = grid%si (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cldefi,1)*SIZE(grid%cldefi,2) .GT. 1 ) THEN
grid%cldefi (ims:ime,jps:min(jde-1,jpe)) = grid%cldefi (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%deep,1)*SIZE(grid%deep,2) .GT. 1 ) THEN
grid%deep (ims:ime,jps:min(jde-1,jpe)) = grid%deep (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rf,1)*SIZE(grid%rf,2) .GT. 1 ) THEN
grid%rf (ims:ime,jps:min(jde-1,jpe)) = grid%rf (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%th10,1)*SIZE(grid%th10,2) .GT. 1 ) THEN
grid%th10 (ims:ime,jps:min(jde-1,jpe)) = grid%th10 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%q10,1)*SIZE(grid%q10,2) .GT. 1 ) THEN
grid%q10 (ims:ime,jps:min(jde-1,jpe)) = grid%q10 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%pshltr,1)*SIZE(grid%pshltr,2) .GT. 1 ) THEN
grid%pshltr (ims:ime,jps:min(jde-1,jpe)) = grid%pshltr (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tshltr,1)*SIZE(grid%tshltr,2) .GT. 1 ) THEN
grid%tshltr (ims:ime,jps:min(jde-1,jpe)) = grid%tshltr (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%qshltr,1)*SIZE(grid%qshltr,2) .GT. 1 ) THEN
grid%qshltr (ims:ime,jps:min(jde-1,jpe)) = grid%qshltr (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%q2,1)*SIZE(grid%q2,2) .GT. 1 ) THEN
grid%q2 (ims:ime,jps:min(jde-1,jpe),:) = grid%q2 (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%t_adj,1)*SIZE(grid%t_adj,2) .GT. 1 ) THEN
grid%t_adj (ims:ime,jps:min(jde-1,jpe),:) = grid%t_adj (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%t_old,1)*SIZE(grid%t_old,2) .GT. 1 ) THEN
grid%t_old (ims:ime,jps:min(jde-1,jpe),:) = grid%t_old (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%zero_3d,1)*SIZE(grid%zero_3d,2) .GT. 1 ) THEN
grid%zero_3d (ims:ime,jps:min(jde-1,jpe),:) = grid%zero_3d (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%w0avg,1)*SIZE(grid%w0avg,3) .GT. 1 ) THEN
grid%w0avg (ims:ime,:,jps:min(jde-1,jpe)) = grid%w0avg (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%akhs_out,1)*SIZE(grid%akhs_out,2) .GT. 1 ) THEN
grid%akhs_out (ims:ime,jps:min(jde-1,jpe)) = grid%akhs_out (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%akms_out,1)*SIZE(grid%akms_out,2) .GT. 1 ) THEN
grid%akms_out (ims:ime,jps:min(jde-1,jpe)) = grid%akms_out (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lpi,1)*SIZE(grid%lpi,2) .GT. 1 ) THEN
grid%lpi (ims:ime,jps:min(jde-1,jpe)) = grid%lpi (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cd_out,1)*SIZE(grid%cd_out,2) .GT. 1 ) THEN
grid%cd_out (ims:ime,jps:min(jde-1,jpe)) = grid%cd_out (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ch_out,1)*SIZE(grid%ch_out,2) .GT. 1 ) THEN
grid%ch_out (ims:ime,jps:min(jde-1,jpe)) = grid%ch_out (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%albase,1)*SIZE(grid%albase,2) .GT. 1 ) THEN
grid%albase (ims:ime,jps:min(jde-1,jpe)) = grid%albase (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%albedo,1)*SIZE(grid%albedo,2) .GT. 1 ) THEN
grid%albedo (ims:ime,jps:min(jde-1,jpe)) = grid%albedo (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cnvbot,1)*SIZE(grid%cnvbot,2) .GT. 1 ) THEN
grid%cnvbot (ims:ime,jps:min(jde-1,jpe)) = grid%cnvbot (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cnvtop,1)*SIZE(grid%cnvtop,2) .GT. 1 ) THEN
grid%cnvtop (ims:ime,jps:min(jde-1,jpe)) = grid%cnvtop (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%czen,1)*SIZE(grid%czen,2) .GT. 1 ) THEN
grid%czen (ims:ime,jps:min(jde-1,jpe)) = grid%czen (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%czmean,1)*SIZE(grid%czmean,2) .GT. 1 ) THEN
grid%czmean (ims:ime,jps:min(jde-1,jpe)) = grid%czmean (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%embck,1)*SIZE(grid%embck,2) .GT. 1 ) THEN
grid%embck (ims:ime,jps:min(jde-1,jpe)) = grid%embck (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%epsr,1)*SIZE(grid%epsr,2) .GT. 1 ) THEN
grid%epsr (ims:ime,jps:min(jde-1,jpe)) = grid%epsr (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%gffc,1)*SIZE(grid%gffc,2) .GT. 1 ) THEN
grid%gffc (ims:ime,jps:min(jde-1,jpe)) = grid%gffc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%glat,1)*SIZE(grid%glat,2) .GT. 1 ) THEN
grid%glat (ims:ime,jps:min(jde-1,jpe)) = grid%glat (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%glon,1)*SIZE(grid%glon,2) .GT. 1 ) THEN
grid%glon (ims:ime,jps:min(jde-1,jpe)) = grid%glon (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%nmm_tsk,1)*SIZE(grid%nmm_tsk,2) .GT. 1 ) THEN
grid%nmm_tsk (ims:ime,jps:min(jde-1,jpe)) = grid%nmm_tsk (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hdac,1)*SIZE(grid%hdac,2) .GT. 1 ) THEN
grid%hdac (ims:ime,jps:min(jde-1,jpe)) = grid%hdac (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hdacv,1)*SIZE(grid%hdacv,2) .GT. 1 ) THEN
grid%hdacv (ims:ime,jps:min(jde-1,jpe)) = grid%hdacv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%mxsnal,1)*SIZE(grid%mxsnal,2) .GT. 1 ) THEN
grid%mxsnal (ims:ime,jps:min(jde-1,jpe)) = grid%mxsnal (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%radin,1)*SIZE(grid%radin,2) .GT. 1 ) THEN
grid%radin (ims:ime,jps:min(jde-1,jpe)) = grid%radin (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%radot,1)*SIZE(grid%radot,2) .GT. 1 ) THEN
grid%radot (ims:ime,jps:min(jde-1,jpe)) = grid%radot (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sigt4,1)*SIZE(grid%sigt4,2) .GT. 1 ) THEN
grid%sigt4 (ims:ime,jps:min(jde-1,jpe)) = grid%sigt4 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tg,1)*SIZE(grid%tg,2) .GT. 1 ) THEN
grid%tg (ims:ime,jps:min(jde-1,jpe)) = grid%tg (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lvl,1)*SIZE(grid%lvl,2) .GT. 1 ) THEN
grid%lvl (ims:ime,jps:min(jde-1,jpe)) = grid%lvl (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%k22_deep,1)*SIZE(grid%k22_deep,2) .GT. 1 ) THEN
grid%k22_deep (ims:ime,jps:min(jde-1,jpe)) = grid%k22_deep (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%kbcon_deep,1)*SIZE(grid%kbcon_deep,2) .GT. 1 ) THEN
grid%kbcon_deep (ims:ime,jps:min(jde-1,jpe)) = grid%kbcon_deep (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ktop_deep,1)*SIZE(grid%ktop_deep,2) .GT. 1 ) THEN
grid%ktop_deep (ims:ime,jps:min(jde-1,jpe)) = grid%ktop_deep (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%raincv_a,1)*SIZE(grid%raincv_a,2) .GT. 1 ) THEN
grid%raincv_a (ims:ime,jps:min(jde-1,jpe)) = grid%raincv_a (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%raincv_b,1)*SIZE(grid%raincv_b,2) .GT. 1 ) THEN
grid%raincv_b (ims:ime,jps:min(jde-1,jpe)) = grid%raincv_b (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%gd_cloud,1)*SIZE(grid%gd_cloud,3) .GT. 1 ) THEN
grid%gd_cloud (ims:ime,:,jps:min(jde-1,jpe)) = grid%gd_cloud (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%gd_cloud2,1)*SIZE(grid%gd_cloud2,3) .GT. 1 ) THEN
grid%gd_cloud2 (ims:ime,:,jps:min(jde-1,jpe)) = grid%gd_cloud2 (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%gd_cloud_a,1)*SIZE(grid%gd_cloud_a,3) .GT. 1 ) THEN
grid%gd_cloud_a (ims:ime,:,jps:min(jde-1,jpe)) = grid%gd_cloud_a (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%gd_cloud2_a,1)*SIZE(grid%gd_cloud2_a,3) .GT. 1 ) THEN
grid%gd_cloud2_a (ims:ime,:,jps:min(jde-1,jpe)) = grid%gd_cloud2_a (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%qc_cu,1)*SIZE(grid%qc_cu,3) .GT. 1 ) THEN
grid%qc_cu (ims:ime,:,jps:min(jde-1,jpe)) = grid%qc_cu (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%qi_cu,1)*SIZE(grid%qi_cu,3) .GT. 1 ) THEN
grid%qi_cu (ims:ime,:,jps:min(jde-1,jpe)) = grid%qi_cu (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%gd_cldfr,1)*SIZE(grid%gd_cldfr,3) .GT. 1 ) THEN
grid%gd_cldfr (ims:ime,:,jps:min(jde-1,jpe)) = grid%gd_cldfr (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%acswupt,1)*SIZE(grid%acswupt,2) .GT. 1 ) THEN
grid%acswupt (ims:ime,jps:min(jde-1,jpe)) = grid%acswupt (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%acswuptc,1)*SIZE(grid%acswuptc,2) .GT. 1 ) THEN
grid%acswuptc (ims:ime,jps:min(jde-1,jpe)) = grid%acswuptc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%acswdnt,1)*SIZE(grid%acswdnt,2) .GT. 1 ) THEN
grid%acswdnt (ims:ime,jps:min(jde-1,jpe)) = grid%acswdnt (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%acswdntc,1)*SIZE(grid%acswdntc,2) .GT. 1 ) THEN
grid%acswdntc (ims:ime,jps:min(jde-1,jpe)) = grid%acswdntc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%acswupb,1)*SIZE(grid%acswupb,2) .GT. 1 ) THEN
grid%acswupb (ims:ime,jps:min(jde-1,jpe)) = grid%acswupb (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%acswupbc,1)*SIZE(grid%acswupbc,2) .GT. 1 ) THEN
grid%acswupbc (ims:ime,jps:min(jde-1,jpe)) = grid%acswupbc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%acswdnb,1)*SIZE(grid%acswdnb,2) .GT. 1 ) THEN
grid%acswdnb (ims:ime,jps:min(jde-1,jpe)) = grid%acswdnb (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%acswdnbc,1)*SIZE(grid%acswdnbc,2) .GT. 1 ) THEN
grid%acswdnbc (ims:ime,jps:min(jde-1,jpe)) = grid%acswdnbc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%aclwupt,1)*SIZE(grid%aclwupt,2) .GT. 1 ) THEN
grid%aclwupt (ims:ime,jps:min(jde-1,jpe)) = grid%aclwupt (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%aclwuptc,1)*SIZE(grid%aclwuptc,2) .GT. 1 ) THEN
grid%aclwuptc (ims:ime,jps:min(jde-1,jpe)) = grid%aclwuptc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%aclwdnt,1)*SIZE(grid%aclwdnt,2) .GT. 1 ) THEN
grid%aclwdnt (ims:ime,jps:min(jde-1,jpe)) = grid%aclwdnt (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%aclwdntc,1)*SIZE(grid%aclwdntc,2) .GT. 1 ) THEN
grid%aclwdntc (ims:ime,jps:min(jde-1,jpe)) = grid%aclwdntc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%aclwupb,1)*SIZE(grid%aclwupb,2) .GT. 1 ) THEN
grid%aclwupb (ims:ime,jps:min(jde-1,jpe)) = grid%aclwupb (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%aclwupbc,1)*SIZE(grid%aclwupbc,2) .GT. 1 ) THEN
grid%aclwupbc (ims:ime,jps:min(jde-1,jpe)) = grid%aclwupbc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%aclwdnb,1)*SIZE(grid%aclwdnb,2) .GT. 1 ) THEN
grid%aclwdnb (ims:ime,jps:min(jde-1,jpe)) = grid%aclwdnb (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%aclwdnbc,1)*SIZE(grid%aclwdnbc,2) .GT. 1 ) THEN
grid%aclwdnbc (ims:ime,jps:min(jde-1,jpe)) = grid%aclwdnbc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%swupt,1)*SIZE(grid%swupt,2) .GT. 1 ) THEN
grid%swupt (ims:ime,jps:min(jde-1,jpe)) = grid%swupt (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%swuptc,1)*SIZE(grid%swuptc,2) .GT. 1 ) THEN
grid%swuptc (ims:ime,jps:min(jde-1,jpe)) = grid%swuptc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%swdnt,1)*SIZE(grid%swdnt,2) .GT. 1 ) THEN
grid%swdnt (ims:ime,jps:min(jde-1,jpe)) = grid%swdnt (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%swdntc,1)*SIZE(grid%swdntc,2) .GT. 1 ) THEN
grid%swdntc (ims:ime,jps:min(jde-1,jpe)) = grid%swdntc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%swupb,1)*SIZE(grid%swupb,2) .GT. 1 ) THEN
grid%swupb (ims:ime,jps:min(jde-1,jpe)) = grid%swupb (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%swupbc,1)*SIZE(grid%swupbc,2) .GT. 1 ) THEN
grid%swupbc (ims:ime,jps:min(jde-1,jpe)) = grid%swupbc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%swdnb,1)*SIZE(grid%swdnb,2) .GT. 1 ) THEN
grid%swdnb (ims:ime,jps:min(jde-1,jpe)) = grid%swdnb (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%swdnbc,1)*SIZE(grid%swdnbc,2) .GT. 1 ) THEN
grid%swdnbc (ims:ime,jps:min(jde-1,jpe)) = grid%swdnbc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lwupt,1)*SIZE(grid%lwupt,2) .GT. 1 ) THEN
grid%lwupt (ims:ime,jps:min(jde-1,jpe)) = grid%lwupt (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lwuptc,1)*SIZE(grid%lwuptc,2) .GT. 1 ) THEN
grid%lwuptc (ims:ime,jps:min(jde-1,jpe)) = grid%lwuptc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lwdnt,1)*SIZE(grid%lwdnt,2) .GT. 1 ) THEN
grid%lwdnt (ims:ime,jps:min(jde-1,jpe)) = grid%lwdnt (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lwdntc,1)*SIZE(grid%lwdntc,2) .GT. 1 ) THEN
grid%lwdntc (ims:ime,jps:min(jde-1,jpe)) = grid%lwdntc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lwupb,1)*SIZE(grid%lwupb,2) .GT. 1 ) THEN
grid%lwupb (ims:ime,jps:min(jde-1,jpe)) = grid%lwupb (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lwupbc,1)*SIZE(grid%lwupbc,2) .GT. 1 ) THEN
grid%lwupbc (ims:ime,jps:min(jde-1,jpe)) = grid%lwupbc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lwdnb,1)*SIZE(grid%lwdnb,2) .GT. 1 ) THEN
grid%lwdnb (ims:ime,jps:min(jde-1,jpe)) = grid%lwdnb (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lwdnbc,1)*SIZE(grid%lwdnbc,2) .GT. 1 ) THEN
grid%lwdnbc (ims:ime,jps:min(jde-1,jpe)) = grid%lwdnbc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%swvisdir,1)*SIZE(grid%swvisdir,2) .GT. 1 ) THEN
grid%swvisdir (ims:ime,jps:min(jde-1,jpe)) = grid%swvisdir (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%swvisdif,1)*SIZE(grid%swvisdif,2) .GT. 1 ) THEN
grid%swvisdif (ims:ime,jps:min(jde-1,jpe)) = grid%swvisdif (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%swnirdir,1)*SIZE(grid%swnirdir,2) .GT. 1 ) THEN
grid%swnirdir (ims:ime,jps:min(jde-1,jpe)) = grid%swnirdir (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%swnirdif,1)*SIZE(grid%swnirdif,2) .GT. 1 ) THEN
grid%swnirdif (ims:ime,jps:min(jde-1,jpe)) = grid%swnirdif (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%refl_10cm,1)*SIZE(grid%refl_10cm,3) .GT. 1 ) THEN
grid%refl_10cm (ims:ime,:,jps:min(jde-1,jpe)) = grid%refl_10cm (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%refd_max,1)*SIZE(grid%refd_max,2) .GT. 1 ) THEN
grid%refd_max (ims:ime,jps:min(jde-1,jpe)) = grid%refd_max (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%qnwfa2d,1)*SIZE(grid%qnwfa2d,2) .GT. 1 ) THEN
grid%qnwfa2d (ims:ime,jps:min(jde-1,jpe)) = grid%qnwfa2d (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%re_cloud,1)*SIZE(grid%re_cloud,3) .GT. 1 ) THEN
grid%re_cloud (ims:ime,:,jps:min(jde-1,jpe)) = grid%re_cloud (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%re_ice,1)*SIZE(grid%re_ice,3) .GT. 1 ) THEN
grid%re_ice (ims:ime,:,jps:min(jde-1,jpe)) = grid%re_ice (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%re_snow,1)*SIZE(grid%re_snow,3) .GT. 1 ) THEN
grid%re_snow (ims:ime,:,jps:min(jde-1,jpe)) = grid%re_snow (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dfi_re_cloud,1)*SIZE(grid%dfi_re_cloud,3) .GT. 1 ) THEN
grid%dfi_re_cloud (ims:ime,:,jps:min(jde-1,jpe)) = grid%dfi_re_cloud (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dfi_re_ice,1)*SIZE(grid%dfi_re_ice,3) .GT. 1 ) THEN
grid%dfi_re_ice (ims:ime,:,jps:min(jde-1,jpe)) = grid%dfi_re_ice (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dfi_re_snow,1)*SIZE(grid%dfi_re_snow,3) .GT. 1 ) THEN
grid%dfi_re_snow (ims:ime,:,jps:min(jde-1,jpe)) = grid%dfi_re_snow (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%swddir,1)*SIZE(grid%swddir,2) .GT. 1 ) THEN
grid%swddir (ims:ime,jps:min(jde-1,jpe)) = grid%swddir (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%swddni,1)*SIZE(grid%swddni,2) .GT. 1 ) THEN
grid%swddni (ims:ime,jps:min(jde-1,jpe)) = grid%swddni (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%swddif,1)*SIZE(grid%swddif,2) .GT. 1 ) THEN
grid%swddif (ims:ime,jps:min(jde-1,jpe)) = grid%swddif (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%gx,1)*SIZE(grid%gx,2) .GT. 1 ) THEN
grid%gx (ims:ime,jps:min(jde-1,jpe)) = grid%gx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%bx,1)*SIZE(grid%bx,2) .GT. 1 ) THEN
grid%bx (ims:ime,jps:min(jde-1,jpe)) = grid%bx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%gg,1)*SIZE(grid%gg,2) .GT. 1 ) THEN
grid%gg (ims:ime,jps:min(jde-1,jpe)) = grid%gg (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%bb,1)*SIZE(grid%bb,2) .GT. 1 ) THEN
grid%bb (ims:ime,jps:min(jde-1,jpe)) = grid%bb (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%coszen_ref,1)*SIZE(grid%coszen_ref,2) .GT. 1 ) THEN
grid%coszen_ref (ims:ime,jps:min(jde-1,jpe)) = grid%coszen_ref (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%coszen,1)*SIZE(grid%coszen,2) .GT. 1 ) THEN
grid%coszen (ims:ime,jps:min(jde-1,jpe)) = grid%coszen (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hrang,1)*SIZE(grid%hrang,2) .GT. 1 ) THEN
grid%hrang (ims:ime,jps:min(jde-1,jpe)) = grid%hrang (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%swdown_ref,1)*SIZE(grid%swdown_ref,2) .GT. 1 ) THEN
grid%swdown_ref (ims:ime,jps:min(jde-1,jpe)) = grid%swdown_ref (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%swddir_ref,1)*SIZE(grid%swddir_ref,2) .GT. 1 ) THEN
grid%swddir_ref (ims:ime,jps:min(jde-1,jpe)) = grid%swddir_ref (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cwm,1)*SIZE(grid%cwm,2) .GT. 1 ) THEN
grid%cwm (ims:ime,jps:min(jde-1,jpe),:) = grid%cwm (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%rrw,1)*SIZE(grid%rrw,2) .GT. 1 ) THEN
grid%rrw (ims:ime,jps:min(jde-1,jpe),:) = grid%rrw (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%f_ice,1)*SIZE(grid%f_ice,3) .GT. 1 ) THEN
grid%f_ice (ims:ime,:,jps:min(jde-1,jpe)) = grid%f_ice (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%f_rain,1)*SIZE(grid%f_rain,3) .GT. 1 ) THEN
grid%f_rain (ims:ime,:,jps:min(jde-1,jpe)) = grid%f_rain (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%f_rimef,1)*SIZE(grid%f_rimef,3) .GT. 1 ) THEN
grid%f_rimef (ims:ime,:,jps:min(jde-1,jpe)) = grid%f_rimef (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cldfra,1)*SIZE(grid%cldfra,2) .GT. 1 ) THEN
grid%cldfra (ims:ime,jps:min(jde-1,jpe),:) = grid%cldfra (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%sr,1)*SIZE(grid%sr,2) .GT. 1 ) THEN
grid%sr (ims:ime,jps:min(jde-1,jpe)) = grid%sr (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cfrach,1)*SIZE(grid%cfrach,2) .GT. 1 ) THEN
grid%cfrach (ims:ime,jps:min(jde-1,jpe)) = grid%cfrach (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cfracl,1)*SIZE(grid%cfracl,2) .GT. 1 ) THEN
grid%cfracl (ims:ime,jps:min(jde-1,jpe)) = grid%cfracl (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cfracm,1)*SIZE(grid%cfracm,2) .GT. 1 ) THEN
grid%cfracm (ims:ime,jps:min(jde-1,jpe)) = grid%cfracm (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%islope,1)*SIZE(grid%islope,2) .GT. 1 ) THEN
grid%islope (ims:ime,jps:min(jde-1,jpe)) = grid%islope (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cmc,1)*SIZE(grid%cmc,2) .GT. 1 ) THEN
grid%cmc (ims:ime,jps:min(jde-1,jpe)) = grid%cmc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%grnflx,1)*SIZE(grid%grnflx,2) .GT. 1 ) THEN
grid%grnflx (ims:ime,jps:min(jde-1,jpe)) = grid%grnflx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%pctsno,1)*SIZE(grid%pctsno,2) .GT. 1 ) THEN
grid%pctsno (ims:ime,jps:min(jde-1,jpe)) = grid%pctsno (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soiltb,1)*SIZE(grid%soiltb,2) .GT. 1 ) THEN
grid%soiltb (ims:ime,jps:min(jde-1,jpe)) = grid%soiltb (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vegfrc,1)*SIZE(grid%vegfrc,2) .GT. 1 ) THEN
grid%vegfrc (ims:ime,jps:min(jde-1,jpe)) = grid%vegfrc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%shdmax,1)*SIZE(grid%shdmax,2) .GT. 1 ) THEN
grid%shdmax (ims:ime,jps:min(jde-1,jpe)) = grid%shdmax (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%shdmin,1)*SIZE(grid%shdmin,2) .GT. 1 ) THEN
grid%shdmin (ims:ime,jps:min(jde-1,jpe)) = grid%shdmin (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sh2o,1)*SIZE(grid%sh2o,3) .GT. 1 ) THEN
grid%sh2o (ims:ime,:,jps:min(jde-1,jpe)) = grid%sh2o (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%smc,1)*SIZE(grid%smc,3) .GT. 1 ) THEN
grid%smc (ims:ime,:,jps:min(jde-1,jpe)) = grid%smc (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%stc,1)*SIZE(grid%stc,3) .GT. 1 ) THEN
grid%stc (ims:ime,:,jps:min(jde-1,jpe)) = grid%stc (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hstdv,1)*SIZE(grid%hstdv,2) .GT. 1 ) THEN
grid%hstdv (ims:ime,jps:min(jde-1,jpe)) = grid%hstdv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hcnvx,1)*SIZE(grid%hcnvx,2) .GT. 1 ) THEN
grid%hcnvx (ims:ime,jps:min(jde-1,jpe)) = grid%hcnvx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hasyw,1)*SIZE(grid%hasyw,2) .GT. 1 ) THEN
grid%hasyw (ims:ime,jps:min(jde-1,jpe)) = grid%hasyw (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hasys,1)*SIZE(grid%hasys,2) .GT. 1 ) THEN
grid%hasys (ims:ime,jps:min(jde-1,jpe)) = grid%hasys (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hasysw,1)*SIZE(grid%hasysw,2) .GT. 1 ) THEN
grid%hasysw (ims:ime,jps:min(jde-1,jpe)) = grid%hasysw (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hasynw,1)*SIZE(grid%hasynw,2) .GT. 1 ) THEN
grid%hasynw (ims:ime,jps:min(jde-1,jpe)) = grid%hasynw (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hlenw,1)*SIZE(grid%hlenw,2) .GT. 1 ) THEN
grid%hlenw (ims:ime,jps:min(jde-1,jpe)) = grid%hlenw (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hlens,1)*SIZE(grid%hlens,2) .GT. 1 ) THEN
grid%hlens (ims:ime,jps:min(jde-1,jpe)) = grid%hlens (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hlensw,1)*SIZE(grid%hlensw,2) .GT. 1 ) THEN
grid%hlensw (ims:ime,jps:min(jde-1,jpe)) = grid%hlensw (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hlennw,1)*SIZE(grid%hlennw,2) .GT. 1 ) THEN
grid%hlennw (ims:ime,jps:min(jde-1,jpe)) = grid%hlennw (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hangl,1)*SIZE(grid%hangl,2) .GT. 1 ) THEN
grid%hangl (ims:ime,jps:min(jde-1,jpe)) = grid%hangl (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hanis,1)*SIZE(grid%hanis,2) .GT. 1 ) THEN
grid%hanis (ims:ime,jps:min(jde-1,jpe)) = grid%hanis (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hslop,1)*SIZE(grid%hslop,2) .GT. 1 ) THEN
grid%hslop (ims:ime,jps:min(jde-1,jpe)) = grid%hslop (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hzmax,1)*SIZE(grid%hzmax,2) .GT. 1 ) THEN
grid%hzmax (ims:ime,jps:min(jde-1,jpe)) = grid%hzmax (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%crot,1)*SIZE(grid%crot,2) .GT. 1 ) THEN
grid%crot (ims:ime,jps:min(jde-1,jpe)) = grid%crot (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%srot,1)*SIZE(grid%srot,2) .GT. 1 ) THEN
grid%srot (ims:ime,jps:min(jde-1,jpe)) = grid%srot (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ugwdsfc,1)*SIZE(grid%ugwdsfc,2) .GT. 1 ) THEN
grid%ugwdsfc (ims:ime,jps:min(jde-1,jpe)) = grid%ugwdsfc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vgwdsfc,1)*SIZE(grid%vgwdsfc,2) .GT. 1 ) THEN
grid%vgwdsfc (ims:ime,jps:min(jde-1,jpe)) = grid%vgwdsfc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ctopo,1)*SIZE(grid%ctopo,2) .GT. 1 ) THEN
grid%ctopo (ims:ime,jps:min(jde-1,jpe)) = grid%ctopo (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ctopo2,1)*SIZE(grid%ctopo2,2) .GT. 1 ) THEN
grid%ctopo2 (ims:ime,jps:min(jde-1,jpe)) = grid%ctopo2 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dwdtmn,1)*SIZE(grid%dwdtmn,2) .GT. 1 ) THEN
grid%dwdtmn (ims:ime,jps:min(jde-1,jpe)) = grid%dwdtmn (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dwdtmx,1)*SIZE(grid%dwdtmx,2) .GT. 1 ) THEN
grid%dwdtmx (ims:ime,jps:min(jde-1,jpe)) = grid%dwdtmx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%baro,1)*SIZE(grid%baro,2) .GT. 1 ) THEN
grid%baro (ims:ime,jps:min(jde-1,jpe)) = grid%baro (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dwdt,1)*SIZE(grid%dwdt,2) .GT. 1 ) THEN
grid%dwdt (ims:ime,jps:min(jde-1,jpe),:) = grid%dwdt (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%pdwdt,1)*SIZE(grid%pdwdt,2) .GT. 1 ) THEN
grid%pdwdt (ims:ime,jps:min(jde-1,jpe),:) = grid%pdwdt (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%pint,1)*SIZE(grid%pint,2) .GT. 1 ) THEN
grid%pint (ims:ime,jps:min(jde-1,jpe),:) = grid%pint (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%w,1)*SIZE(grid%w,2) .GT. 1 ) THEN
grid%w (ims:ime,jps:min(jde-1,jpe),:) = grid%w (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%w_tot,1)*SIZE(grid%w_tot,2) .GT. 1 ) THEN
grid%w_tot (ims:ime,jps:min(jde-1,jpe),:) = grid%w_tot (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%z,1)*SIZE(grid%z,2) .GT. 1 ) THEN
grid%z (ims:ime,jps:min(jde-1,jpe),:) = grid%z (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%acfrcv,1)*SIZE(grid%acfrcv,2) .GT. 1 ) THEN
grid%acfrcv (ims:ime,jps:min(jde-1,jpe)) = grid%acfrcv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%acfrst,1)*SIZE(grid%acfrst,2) .GT. 1 ) THEN
grid%acfrst (ims:ime,jps:min(jde-1,jpe)) = grid%acfrst (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ssroff,1)*SIZE(grid%ssroff,2) .GT. 1 ) THEN
grid%ssroff (ims:ime,jps:min(jde-1,jpe)) = grid%ssroff (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%bgroff,1)*SIZE(grid%bgroff,2) .GT. 1 ) THEN
grid%bgroff (ims:ime,jps:min(jde-1,jpe)) = grid%bgroff (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rlwin,1)*SIZE(grid%rlwin,2) .GT. 1 ) THEN
grid%rlwin (ims:ime,jps:min(jde-1,jpe)) = grid%rlwin (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rlwout,1)*SIZE(grid%rlwout,2) .GT. 1 ) THEN
grid%rlwout (ims:ime,jps:min(jde-1,jpe)) = grid%rlwout (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rlwtoa,1)*SIZE(grid%rlwtoa,2) .GT. 1 ) THEN
grid%rlwtoa (ims:ime,jps:min(jde-1,jpe)) = grid%rlwtoa (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%alwin,1)*SIZE(grid%alwin,2) .GT. 1 ) THEN
grid%alwin (ims:ime,jps:min(jde-1,jpe)) = grid%alwin (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%alwout,1)*SIZE(grid%alwout,2) .GT. 1 ) THEN
grid%alwout (ims:ime,jps:min(jde-1,jpe)) = grid%alwout (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%alwtoa,1)*SIZE(grid%alwtoa,2) .GT. 1 ) THEN
grid%alwtoa (ims:ime,jps:min(jde-1,jpe)) = grid%alwtoa (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rswin,1)*SIZE(grid%rswin,2) .GT. 1 ) THEN
grid%rswin (ims:ime,jps:min(jde-1,jpe)) = grid%rswin (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rswinc,1)*SIZE(grid%rswinc,2) .GT. 1 ) THEN
grid%rswinc (ims:ime,jps:min(jde-1,jpe)) = grid%rswinc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rswout,1)*SIZE(grid%rswout,2) .GT. 1 ) THEN
grid%rswout (ims:ime,jps:min(jde-1,jpe)) = grid%rswout (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rswtoa,1)*SIZE(grid%rswtoa,2) .GT. 1 ) THEN
grid%rswtoa (ims:ime,jps:min(jde-1,jpe)) = grid%rswtoa (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%aswin,1)*SIZE(grid%aswin,2) .GT. 1 ) THEN
grid%aswin (ims:ime,jps:min(jde-1,jpe)) = grid%aswin (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%aswout,1)*SIZE(grid%aswout,2) .GT. 1 ) THEN
grid%aswout (ims:ime,jps:min(jde-1,jpe)) = grid%aswout (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%aswtoa,1)*SIZE(grid%aswtoa,2) .GT. 1 ) THEN
grid%aswtoa (ims:ime,jps:min(jde-1,jpe)) = grid%aswtoa (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sfcshx,1)*SIZE(grid%sfcshx,2) .GT. 1 ) THEN
grid%sfcshx (ims:ime,jps:min(jde-1,jpe)) = grid%sfcshx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sfclhx,1)*SIZE(grid%sfclhx,2) .GT. 1 ) THEN
grid%sfclhx (ims:ime,jps:min(jde-1,jpe)) = grid%sfclhx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%subshx,1)*SIZE(grid%subshx,2) .GT. 1 ) THEN
grid%subshx (ims:ime,jps:min(jde-1,jpe)) = grid%subshx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%snopcx,1)*SIZE(grid%snopcx,2) .GT. 1 ) THEN
grid%snopcx (ims:ime,jps:min(jde-1,jpe)) = grid%snopcx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sfcuvx,1)*SIZE(grid%sfcuvx,2) .GT. 1 ) THEN
grid%sfcuvx (ims:ime,jps:min(jde-1,jpe)) = grid%sfcuvx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%potevp,1)*SIZE(grid%potevp,2) .GT. 1 ) THEN
grid%potevp (ims:ime,jps:min(jde-1,jpe)) = grid%potevp (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%potflx,1)*SIZE(grid%potflx,2) .GT. 1 ) THEN
grid%potflx (ims:ime,jps:min(jde-1,jpe)) = grid%potflx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tlmin,1)*SIZE(grid%tlmin,2) .GT. 1 ) THEN
grid%tlmin (ims:ime,jps:min(jde-1,jpe)) = grid%tlmin (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tlmax,1)*SIZE(grid%tlmax,2) .GT. 1 ) THEN
grid%tlmax (ims:ime,jps:min(jde-1,jpe)) = grid%tlmax (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%t02_min,1)*SIZE(grid%t02_min,2) .GT. 1 ) THEN
grid%t02_min (ims:ime,jps:min(jde-1,jpe)) = grid%t02_min (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%t02_max,1)*SIZE(grid%t02_max,2) .GT. 1 ) THEN
grid%t02_max (ims:ime,jps:min(jde-1,jpe)) = grid%t02_max (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rh02_min,1)*SIZE(grid%rh02_min,2) .GT. 1 ) THEN
grid%rh02_min (ims:ime,jps:min(jde-1,jpe)) = grid%rh02_min (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rh02_max,1)*SIZE(grid%rh02_max,2) .GT. 1 ) THEN
grid%rh02_max (ims:ime,jps:min(jde-1,jpe)) = grid%rh02_max (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rlwtt,1)*SIZE(grid%rlwtt,2) .GT. 1 ) THEN
grid%rlwtt (ims:ime,jps:min(jde-1,jpe),:) = grid%rlwtt (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%rswtt,1)*SIZE(grid%rswtt,2) .GT. 1 ) THEN
grid%rswtt (ims:ime,jps:min(jde-1,jpe),:) = grid%rswtt (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%tcucn,1)*SIZE(grid%tcucn,2) .GT. 1 ) THEN
grid%tcucn (ims:ime,jps:min(jde-1,jpe),:) = grid%tcucn (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%train,1)*SIZE(grid%train,2) .GT. 1 ) THEN
grid%train (ims:ime,jps:min(jde-1,jpe),:) = grid%train (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%ncfrcv,1)*SIZE(grid%ncfrcv,2) .GT. 1 ) THEN
grid%ncfrcv (ims:ime,jps:min(jde-1,jpe)) = grid%ncfrcv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ncfrst,1)*SIZE(grid%ncfrst,2) .GT. 1 ) THEN
grid%ncfrst (ims:ime,jps:min(jde-1,jpe)) = grid%ncfrst (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%iup_h,1)*SIZE(grid%iup_h,2) .GT. 1 ) THEN
grid%iup_h (ims:ime,jps:min(jde-1,jpe)) = grid%iup_h (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%iup_v,1)*SIZE(grid%iup_v,2) .GT. 1 ) THEN
grid%iup_v (ims:ime,jps:min(jde-1,jpe)) = grid%iup_v (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%iup_adh,1)*SIZE(grid%iup_adh,2) .GT. 1 ) THEN
grid%iup_adh (ims:ime,jps:min(jde-1,jpe)) = grid%iup_adh (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%iup_adv,1)*SIZE(grid%iup_adv,2) .GT. 1 ) THEN
grid%iup_adv (ims:ime,jps:min(jde-1,jpe)) = grid%iup_adv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%winfo,1)*SIZE(grid%winfo,2) .GT. 1 ) THEN
grid%winfo (ims:ime,jps:min(jde-1,jpe),:) = grid%winfo (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%iinfo,1)*SIZE(grid%iinfo,2) .GT. 1 ) THEN
grid%iinfo (ims:ime,jps:min(jde-1,jpe),:) = grid%iinfo (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%imask_nostag,1)*SIZE(grid%imask_nostag,2) .GT. 1 ) THEN
grid%imask_nostag (ims:ime,jps:min(jde-1,jpe)) = grid%imask_nostag (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%imask_xstag,1)*SIZE(grid%imask_xstag,2) .GT. 1 ) THEN
grid%imask_xstag (ims:ime,jps:min(jde-1,jpe)) = grid%imask_xstag (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%imask_ystag,1)*SIZE(grid%imask_ystag,2) .GT. 1 ) THEN
grid%imask_ystag (ims:ime,jps:min(jde-1,jpe)) = grid%imask_ystag (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%imask_xystag,1)*SIZE(grid%imask_xystag,2) .GT. 1 ) THEN
grid%imask_xystag (ims:ime,jps:min(jde-1,jpe)) = grid%imask_xystag (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm000007,1)*SIZE(grid%sm000007,2) .GT. 1 ) THEN
grid%sm000007 (ims:ime,jps:min(jde-1,jpe)) = grid%sm000007 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm007028,1)*SIZE(grid%sm007028,2) .GT. 1 ) THEN
grid%sm007028 (ims:ime,jps:min(jde-1,jpe)) = grid%sm007028 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm028100,1)*SIZE(grid%sm028100,2) .GT. 1 ) THEN
grid%sm028100 (ims:ime,jps:min(jde-1,jpe)) = grid%sm028100 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm100255,1)*SIZE(grid%sm100255,2) .GT. 1 ) THEN
grid%sm100255 (ims:ime,jps:min(jde-1,jpe)) = grid%sm100255 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%st000007,1)*SIZE(grid%st000007,2) .GT. 1 ) THEN
grid%st000007 (ims:ime,jps:min(jde-1,jpe)) = grid%st000007 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%st007028,1)*SIZE(grid%st007028,2) .GT. 1 ) THEN
grid%st007028 (ims:ime,jps:min(jde-1,jpe)) = grid%st007028 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%st028100,1)*SIZE(grid%st028100,2) .GT. 1 ) THEN
grid%st028100 (ims:ime,jps:min(jde-1,jpe)) = grid%st028100 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%st100255,1)*SIZE(grid%st100255,2) .GT. 1 ) THEN
grid%st100255 (ims:ime,jps:min(jde-1,jpe)) = grid%st100255 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm000010,1)*SIZE(grid%sm000010,2) .GT. 1 ) THEN
grid%sm000010 (ims:ime,jps:min(jde-1,jpe)) = grid%sm000010 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm010040,1)*SIZE(grid%sm010040,2) .GT. 1 ) THEN
grid%sm010040 (ims:ime,jps:min(jde-1,jpe)) = grid%sm010040 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm040100,1)*SIZE(grid%sm040100,2) .GT. 1 ) THEN
grid%sm040100 (ims:ime,jps:min(jde-1,jpe)) = grid%sm040100 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm100200,1)*SIZE(grid%sm100200,2) .GT. 1 ) THEN
grid%sm100200 (ims:ime,jps:min(jde-1,jpe)) = grid%sm100200 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sm010200,1)*SIZE(grid%sm010200,2) .GT. 1 ) THEN
grid%sm010200 (ims:ime,jps:min(jde-1,jpe)) = grid%sm010200 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilm000,1)*SIZE(grid%soilm000,2) .GT. 1 ) THEN
grid%soilm000 (ims:ime,jps:min(jde-1,jpe)) = grid%soilm000 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilm005,1)*SIZE(grid%soilm005,2) .GT. 1 ) THEN
grid%soilm005 (ims:ime,jps:min(jde-1,jpe)) = grid%soilm005 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilm020,1)*SIZE(grid%soilm020,2) .GT. 1 ) THEN
grid%soilm020 (ims:ime,jps:min(jde-1,jpe)) = grid%soilm020 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilm040,1)*SIZE(grid%soilm040,2) .GT. 1 ) THEN
grid%soilm040 (ims:ime,jps:min(jde-1,jpe)) = grid%soilm040 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilm160,1)*SIZE(grid%soilm160,2) .GT. 1 ) THEN
grid%soilm160 (ims:ime,jps:min(jde-1,jpe)) = grid%soilm160 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilm300,1)*SIZE(grid%soilm300,2) .GT. 1 ) THEN
grid%soilm300 (ims:ime,jps:min(jde-1,jpe)) = grid%soilm300 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sw000010,1)*SIZE(grid%sw000010,2) .GT. 1 ) THEN
grid%sw000010 (ims:ime,jps:min(jde-1,jpe)) = grid%sw000010 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sw010040,1)*SIZE(grid%sw010040,2) .GT. 1 ) THEN
grid%sw010040 (ims:ime,jps:min(jde-1,jpe)) = grid%sw010040 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sw040100,1)*SIZE(grid%sw040100,2) .GT. 1 ) THEN
grid%sw040100 (ims:ime,jps:min(jde-1,jpe)) = grid%sw040100 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sw100200,1)*SIZE(grid%sw100200,2) .GT. 1 ) THEN
grid%sw100200 (ims:ime,jps:min(jde-1,jpe)) = grid%sw100200 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sw010200,1)*SIZE(grid%sw010200,2) .GT. 1 ) THEN
grid%sw010200 (ims:ime,jps:min(jde-1,jpe)) = grid%sw010200 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilw000,1)*SIZE(grid%soilw000,2) .GT. 1 ) THEN
grid%soilw000 (ims:ime,jps:min(jde-1,jpe)) = grid%soilw000 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilw005,1)*SIZE(grid%soilw005,2) .GT. 1 ) THEN
grid%soilw005 (ims:ime,jps:min(jde-1,jpe)) = grid%soilw005 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilw020,1)*SIZE(grid%soilw020,2) .GT. 1 ) THEN
grid%soilw020 (ims:ime,jps:min(jde-1,jpe)) = grid%soilw020 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilw040,1)*SIZE(grid%soilw040,2) .GT. 1 ) THEN
grid%soilw040 (ims:ime,jps:min(jde-1,jpe)) = grid%soilw040 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilw160,1)*SIZE(grid%soilw160,2) .GT. 1 ) THEN
grid%soilw160 (ims:ime,jps:min(jde-1,jpe)) = grid%soilw160 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilw300,1)*SIZE(grid%soilw300,2) .GT. 1 ) THEN
grid%soilw300 (ims:ime,jps:min(jde-1,jpe)) = grid%soilw300 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%st000010,1)*SIZE(grid%st000010,2) .GT. 1 ) THEN
grid%st000010 (ims:ime,jps:min(jde-1,jpe)) = grid%st000010 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%st010040,1)*SIZE(grid%st010040,2) .GT. 1 ) THEN
grid%st010040 (ims:ime,jps:min(jde-1,jpe)) = grid%st010040 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%st040100,1)*SIZE(grid%st040100,2) .GT. 1 ) THEN
grid%st040100 (ims:ime,jps:min(jde-1,jpe)) = grid%st040100 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%st100200,1)*SIZE(grid%st100200,2) .GT. 1 ) THEN
grid%st100200 (ims:ime,jps:min(jde-1,jpe)) = grid%st100200 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%st010200,1)*SIZE(grid%st010200,2) .GT. 1 ) THEN
grid%st010200 (ims:ime,jps:min(jde-1,jpe)) = grid%st010200 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilt000,1)*SIZE(grid%soilt000,2) .GT. 1 ) THEN
grid%soilt000 (ims:ime,jps:min(jde-1,jpe)) = grid%soilt000 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilt005,1)*SIZE(grid%soilt005,2) .GT. 1 ) THEN
grid%soilt005 (ims:ime,jps:min(jde-1,jpe)) = grid%soilt005 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilt020,1)*SIZE(grid%soilt020,2) .GT. 1 ) THEN
grid%soilt020 (ims:ime,jps:min(jde-1,jpe)) = grid%soilt020 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilt040,1)*SIZE(grid%soilt040,2) .GT. 1 ) THEN
grid%soilt040 (ims:ime,jps:min(jde-1,jpe)) = grid%soilt040 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilt160,1)*SIZE(grid%soilt160,2) .GT. 1 ) THEN
grid%soilt160 (ims:ime,jps:min(jde-1,jpe)) = grid%soilt160 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilt300,1)*SIZE(grid%soilt300,2) .GT. 1 ) THEN
grid%soilt300 (ims:ime,jps:min(jde-1,jpe)) = grid%soilt300 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%landmask,1)*SIZE(grid%landmask,2) .GT. 1 ) THEN
grid%landmask (ims:ime,jps:min(jde-1,jpe)) = grid%landmask (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%topostdv,1)*SIZE(grid%topostdv,2) .GT. 1 ) THEN
grid%topostdv (ims:ime,jps:min(jde-1,jpe)) = grid%topostdv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%toposlpx,1)*SIZE(grid%toposlpx,2) .GT. 1 ) THEN
grid%toposlpx (ims:ime,jps:min(jde-1,jpe)) = grid%toposlpx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%toposlpy,1)*SIZE(grid%toposlpy,2) .GT. 1 ) THEN
grid%toposlpy (ims:ime,jps:min(jde-1,jpe)) = grid%toposlpy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%greenmax,1)*SIZE(grid%greenmax,2) .GT. 1 ) THEN
grid%greenmax (ims:ime,jps:min(jde-1,jpe)) = grid%greenmax (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%greenmin,1)*SIZE(grid%greenmin,2) .GT. 1 ) THEN
grid%greenmin (ims:ime,jps:min(jde-1,jpe)) = grid%greenmin (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%albedomx,1)*SIZE(grid%albedomx,2) .GT. 1 ) THEN
grid%albedomx (ims:ime,jps:min(jde-1,jpe)) = grid%albedomx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%slopecat,1)*SIZE(grid%slopecat,2) .GT. 1 ) THEN
grid%slopecat (ims:ime,jps:min(jde-1,jpe)) = grid%slopecat (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%toposoil,1)*SIZE(grid%toposoil,2) .GT. 1 ) THEN
grid%toposoil (ims:ime,jps:min(jde-1,jpe)) = grid%toposoil (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%landusef,1)*SIZE(grid%landusef,3) .GT. 1 ) THEN
grid%landusef (ims:ime,:,jps:min(jde-1,jpe)) = grid%landusef (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilctop,1)*SIZE(grid%soilctop,3) .GT. 1 ) THEN
grid%soilctop (ims:ime,:,jps:min(jde-1,jpe)) = grid%soilctop (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilcbot,1)*SIZE(grid%soilcbot,3) .GT. 1 ) THEN
grid%soilcbot (ims:ime,:,jps:min(jde-1,jpe)) = grid%soilcbot (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
  DO itrace = PARAM_FIRST_SCALAR, num_moist
IF ( SIZE(grid%moist,1)*SIZE(grid%moist,2) .GT. 1 ) THEN
grid%moist (ims:ime,jps:min(jde-1,jpe),:,itrace) = grid%moist (ims:ime,jps+py:min(jde-1,jpe)+py,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_dfi_moist
IF ( SIZE(grid%dfi_moist,1)*SIZE(grid%dfi_moist,2) .GT. 1 ) THEN
grid%dfi_moist (ims:ime,jps:min(jde-1,jpe),:,itrace) = grid%dfi_moist (ims:ime,jps+py:min(jde-1,jpe)+py,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_scalar
IF ( SIZE(grid%scalar,1)*SIZE(grid%scalar,2) .GT. 1 ) THEN
grid%scalar (ims:ime,jps:min(jde-1,jpe),:,itrace) = grid%scalar (ims:ime,jps+py:min(jde-1,jpe)+py,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_dfi_scalar
IF ( SIZE(grid%dfi_scalar,1)*SIZE(grid%dfi_scalar,2) .GT. 1 ) THEN
grid%dfi_scalar (ims:ime,jps:min(jde-1,jpe),:,itrace) = grid%dfi_scalar (ims:ime,jps+py:min(jde-1,jpe)+py,:,itrace)
ENDIF
  ENDDO
  DO itrace = PARAM_FIRST_SCALAR, num_chem
IF ( SIZE(grid%chem,1)*SIZE(grid%chem,3) .GT. 1 ) THEN
grid%chem (ims:ime,:,jps:min(jde-1,jpe),itrace) = grid%chem (ims:ime,:,jps+py:min(jde-1,jpe)+py,itrace)
ENDIF
  ENDDO
IF ( SIZE(grid%smois,1)*SIZE(grid%smois,3) .GT. 1 ) THEN
grid%smois (ims:ime,:,jps:min(jde-1,jpe)) = grid%smois (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tslb,1)*SIZE(grid%tslb,3) .GT. 1 ) THEN
grid%tslb (ims:ime,:,jps:min(jde-1,jpe)) = grid%tslb (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lake_depth,1)*SIZE(grid%lake_depth,2) .GT. 1 ) THEN
grid%lake_depth (ims:ime,jps:min(jde-1,jpe)) = grid%lake_depth (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%gsw,1)*SIZE(grid%gsw,2) .GT. 1 ) THEN
grid%gsw (ims:ime,jps:min(jde-1,jpe)) = grid%gsw (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%xland,1)*SIZE(grid%xland,2) .GT. 1 ) THEN
grid%xland (ims:ime,jps:min(jde-1,jpe)) = grid%xland (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%raincv,1)*SIZE(grid%raincv,2) .GT. 1 ) THEN
grid%raincv (ims:ime,jps:min(jde-1,jpe)) = grid%raincv (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%psfc,1)*SIZE(grid%psfc,2) .GT. 1 ) THEN
grid%psfc (ims:ime,jps:min(jde-1,jpe)) = grid%psfc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%th2,1)*SIZE(grid%th2,2) .GT. 1 ) THEN
grid%th2 (ims:ime,jps:min(jde-1,jpe)) = grid%th2 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%t2,1)*SIZE(grid%t2,2) .GT. 1 ) THEN
grid%t2 (ims:ime,jps:min(jde-1,jpe)) = grid%t2 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%u10,1)*SIZE(grid%u10,2) .GT. 1 ) THEN
grid%u10 (ims:ime,jps:min(jde-1,jpe)) = grid%u10 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%v10,1)*SIZE(grid%v10,2) .GT. 1 ) THEN
grid%v10 (ims:ime,jps:min(jde-1,jpe)) = grid%v10 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%xice,1)*SIZE(grid%xice,2) .GT. 1 ) THEN
grid%xice (ims:ime,jps:min(jde-1,jpe)) = grid%xice (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%icedepth,1)*SIZE(grid%icedepth,2) .GT. 1 ) THEN
grid%icedepth (ims:ime,jps:min(jde-1,jpe)) = grid%icedepth (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%albsi,1)*SIZE(grid%albsi,2) .GT. 1 ) THEN
grid%albsi (ims:ime,jps:min(jde-1,jpe)) = grid%albsi (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%snowsi,1)*SIZE(grid%snowsi,2) .GT. 1 ) THEN
grid%snowsi (ims:ime,jps:min(jde-1,jpe)) = grid%snowsi (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lai,1)*SIZE(grid%lai,2) .GT. 1 ) THEN
grid%lai (ims:ime,jps:min(jde-1,jpe)) = grid%lai (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%smstav,1)*SIZE(grid%smstav,2) .GT. 1 ) THEN
grid%smstav (ims:ime,jps:min(jde-1,jpe)) = grid%smstav (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%smstot,1)*SIZE(grid%smstot,2) .GT. 1 ) THEN
grid%smstot (ims:ime,jps:min(jde-1,jpe)) = grid%smstot (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soldrain,1)*SIZE(grid%soldrain,2) .GT. 1 ) THEN
grid%soldrain (ims:ime,jps:min(jde-1,jpe)) = grid%soldrain (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sfcheadrt,1)*SIZE(grid%sfcheadrt,2) .GT. 1 ) THEN
grid%sfcheadrt (ims:ime,jps:min(jde-1,jpe)) = grid%sfcheadrt (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%infxsrt,1)*SIZE(grid%infxsrt,2) .GT. 1 ) THEN
grid%infxsrt (ims:ime,jps:min(jde-1,jpe)) = grid%infxsrt (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sfcrunoff,1)*SIZE(grid%sfcrunoff,2) .GT. 1 ) THEN
grid%sfcrunoff (ims:ime,jps:min(jde-1,jpe)) = grid%sfcrunoff (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%udrunoff,1)*SIZE(grid%udrunoff,2) .GT. 1 ) THEN
grid%udrunoff (ims:ime,jps:min(jde-1,jpe)) = grid%udrunoff (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ivgtyp,1)*SIZE(grid%ivgtyp,2) .GT. 1 ) THEN
grid%ivgtyp (ims:ime,jps:min(jde-1,jpe)) = grid%ivgtyp (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%isltyp,1)*SIZE(grid%isltyp,2) .GT. 1 ) THEN
grid%isltyp (ims:ime,jps:min(jde-1,jpe)) = grid%isltyp (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vegfra,1)*SIZE(grid%vegfra,2) .GT. 1 ) THEN
grid%vegfra (ims:ime,jps:min(jde-1,jpe)) = grid%vegfra (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sfcevp,1)*SIZE(grid%sfcevp,2) .GT. 1 ) THEN
grid%sfcevp (ims:ime,jps:min(jde-1,jpe)) = grid%sfcevp (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%grdflx,1)*SIZE(grid%grdflx,2) .GT. 1 ) THEN
grid%grdflx (ims:ime,jps:min(jde-1,jpe)) = grid%grdflx (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%albbck,1)*SIZE(grid%albbck,2) .GT. 1 ) THEN
grid%albbck (ims:ime,jps:min(jde-1,jpe)) = grid%albbck (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sfcexc,1)*SIZE(grid%sfcexc,2) .GT. 1 ) THEN
grid%sfcexc (ims:ime,jps:min(jde-1,jpe)) = grid%sfcexc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%snotime,1)*SIZE(grid%snotime,2) .GT. 1 ) THEN
grid%snotime (ims:ime,jps:min(jde-1,jpe)) = grid%snotime (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%acsnow,1)*SIZE(grid%acsnow,2) .GT. 1 ) THEN
grid%acsnow (ims:ime,jps:min(jde-1,jpe)) = grid%acsnow (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%acsnom,1)*SIZE(grid%acsnom,2) .GT. 1 ) THEN
grid%acsnom (ims:ime,jps:min(jde-1,jpe)) = grid%acsnom (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rmol,1)*SIZE(grid%rmol,2) .GT. 1 ) THEN
grid%rmol (ims:ime,jps:min(jde-1,jpe)) = grid%rmol (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%snow,1)*SIZE(grid%snow,2) .GT. 1 ) THEN
grid%snow (ims:ime,jps:min(jde-1,jpe)) = grid%snow (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%canwat,1)*SIZE(grid%canwat,2) .GT. 1 ) THEN
grid%canwat (ims:ime,jps:min(jde-1,jpe)) = grid%canwat (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sst,1)*SIZE(grid%sst,2) .GT. 1 ) THEN
grid%sst (ims:ime,jps:min(jde-1,jpe)) = grid%sst (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%uoce,1)*SIZE(grid%uoce,2) .GT. 1 ) THEN
grid%uoce (ims:ime,jps:min(jde-1,jpe)) = grid%uoce (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%voce,1)*SIZE(grid%voce,2) .GT. 1 ) THEN
grid%voce (ims:ime,jps:min(jde-1,jpe)) = grid%voce (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%weasd,1)*SIZE(grid%weasd,2) .GT. 1 ) THEN
grid%weasd (ims:ime,jps:min(jde-1,jpe)) = grid%weasd (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%znt,1)*SIZE(grid%znt,2) .GT. 1 ) THEN
grid%znt (ims:ime,jps:min(jde-1,jpe)) = grid%znt (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%mol,1)*SIZE(grid%mol,2) .GT. 1 ) THEN
grid%mol (ims:ime,jps:min(jde-1,jpe)) = grid%mol (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%noahres,1)*SIZE(grid%noahres,2) .GT. 1 ) THEN
grid%noahres (ims:ime,jps:min(jde-1,jpe)) = grid%noahres (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tke_pbl,1)*SIZE(grid%tke_pbl,2) .GT. 1 ) THEN
grid%tke_pbl (ims:ime,jps:min(jde-1,jpe),:) = grid%tke_pbl (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%el_pbl,1)*SIZE(grid%el_pbl,3) .GT. 1 ) THEN
grid%el_pbl (ims:ime,:,jps:min(jde-1,jpe)) = grid%el_pbl (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%exch_h,1)*SIZE(grid%exch_h,3) .GT. 1 ) THEN
grid%exch_h (ims:ime,:,jps:min(jde-1,jpe)) = grid%exch_h (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%exch_m,1)*SIZE(grid%exch_m,3) .GT. 1 ) THEN
grid%exch_m (ims:ime,:,jps:min(jde-1,jpe)) = grid%exch_m (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%thz0,1)*SIZE(grid%thz0,2) .GT. 1 ) THEN
grid%thz0 (ims:ime,jps:min(jde-1,jpe)) = grid%thz0 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%qz0,1)*SIZE(grid%qz0,2) .GT. 1 ) THEN
grid%qz0 (ims:ime,jps:min(jde-1,jpe)) = grid%qz0 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%uz0,1)*SIZE(grid%uz0,2) .GT. 1 ) THEN
grid%uz0 (ims:ime,jps:min(jde-1,jpe)) = grid%uz0 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vz0,1)*SIZE(grid%vz0,2) .GT. 1 ) THEN
grid%vz0 (ims:ime,jps:min(jde-1,jpe)) = grid%vz0 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%flhc,1)*SIZE(grid%flhc,2) .GT. 1 ) THEN
grid%flhc (ims:ime,jps:min(jde-1,jpe)) = grid%flhc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%flqc,1)*SIZE(grid%flqc,2) .GT. 1 ) THEN
grid%flqc (ims:ime,jps:min(jde-1,jpe)) = grid%flqc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%qsg,1)*SIZE(grid%qsg,2) .GT. 1 ) THEN
grid%qsg (ims:ime,jps:min(jde-1,jpe)) = grid%qsg (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%qvg,1)*SIZE(grid%qvg,2) .GT. 1 ) THEN
grid%qvg (ims:ime,jps:min(jde-1,jpe)) = grid%qvg (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%qcg,1)*SIZE(grid%qcg,2) .GT. 1 ) THEN
grid%qcg (ims:ime,jps:min(jde-1,jpe)) = grid%qcg (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dew,1)*SIZE(grid%dew,2) .GT. 1 ) THEN
grid%dew (ims:ime,jps:min(jde-1,jpe)) = grid%dew (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%soilt1,1)*SIZE(grid%soilt1,2) .GT. 1 ) THEN
grid%soilt1 (ims:ime,jps:min(jde-1,jpe)) = grid%soilt1 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tsnav,1)*SIZE(grid%tsnav,2) .GT. 1 ) THEN
grid%tsnav (ims:ime,jps:min(jde-1,jpe)) = grid%tsnav (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%psfc_out,1)*SIZE(grid%psfc_out,2) .GT. 1 ) THEN
grid%psfc_out (ims:ime,jps:min(jde-1,jpe)) = grid%psfc_out (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%uz0h,1)*SIZE(grid%uz0h,2) .GT. 1 ) THEN
grid%uz0h (ims:ime,jps:min(jde-1,jpe)) = grid%uz0h (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%vz0h,1)*SIZE(grid%vz0h,2) .GT. 1 ) THEN
grid%vz0h (ims:ime,jps:min(jde-1,jpe)) = grid%vz0h (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%dudt,1)*SIZE(grid%dudt,2) .GT. 1 ) THEN
grid%dudt (ims:ime,jps:min(jde-1,jpe),:) = grid%dudt (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%dvdt,1)*SIZE(grid%dvdt,2) .GT. 1 ) THEN
grid%dvdt (ims:ime,jps:min(jde-1,jpe),:) = grid%dvdt (ims:ime,jps+py:min(jde-1,jpe)+py,:)
ENDIF
IF ( SIZE(grid%qsfc,1)*SIZE(grid%qsfc,2) .GT. 1 ) THEN
grid%qsfc (ims:ime,jps:min(jde-1,jpe)) = grid%qsfc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%akhs,1)*SIZE(grid%akhs,2) .GT. 1 ) THEN
grid%akhs (ims:ime,jps:min(jde-1,jpe)) = grid%akhs (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%akms,1)*SIZE(grid%akms,2) .GT. 1 ) THEN
grid%akms (ims:ime,jps:min(jde-1,jpe)) = grid%akms (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%htop,1)*SIZE(grid%htop,2) .GT. 1 ) THEN
grid%htop (ims:ime,jps:min(jde-1,jpe)) = grid%htop (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hbot,1)*SIZE(grid%hbot,2) .GT. 1 ) THEN
grid%hbot (ims:ime,jps:min(jde-1,jpe)) = grid%hbot (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%htopr,1)*SIZE(grid%htopr,2) .GT. 1 ) THEN
grid%htopr (ims:ime,jps:min(jde-1,jpe)) = grid%htopr (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hbotr,1)*SIZE(grid%hbotr,2) .GT. 1 ) THEN
grid%hbotr (ims:ime,jps:min(jde-1,jpe)) = grid%hbotr (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%htopd,1)*SIZE(grid%htopd,2) .GT. 1 ) THEN
grid%htopd (ims:ime,jps:min(jde-1,jpe)) = grid%htopd (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hbotd,1)*SIZE(grid%hbotd,2) .GT. 1 ) THEN
grid%hbotd (ims:ime,jps:min(jde-1,jpe)) = grid%hbotd (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%htops,1)*SIZE(grid%htops,2) .GT. 1 ) THEN
grid%htops (ims:ime,jps:min(jde-1,jpe)) = grid%htops (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%hbots,1)*SIZE(grid%hbots,2) .GT. 1 ) THEN
grid%hbots (ims:ime,jps:min(jde-1,jpe)) = grid%hbots (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cuppt,1)*SIZE(grid%cuppt,2) .GT. 1 ) THEN
grid%cuppt (ims:ime,jps:min(jde-1,jpe)) = grid%cuppt (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cprate,1)*SIZE(grid%cprate,2) .GT. 1 ) THEN
grid%cprate (ims:ime,jps:min(jde-1,jpe)) = grid%cprate (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%f_ice_phy,1)*SIZE(grid%f_ice_phy,3) .GT. 1 ) THEN
grid%f_ice_phy (ims:ime,:,jps:min(jde-1,jpe)) = grid%f_ice_phy (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%f_rain_phy,1)*SIZE(grid%f_rain_phy,3) .GT. 1 ) THEN
grid%f_rain_phy (ims:ime,:,jps:min(jde-1,jpe)) = grid%f_rain_phy (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%f_rimef_phy,1)*SIZE(grid%f_rimef_phy,3) .GT. 1 ) THEN
grid%f_rimef_phy (ims:ime,:,jps:min(jde-1,jpe)) = grid%f_rimef_phy (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%mass_flux,1)*SIZE(grid%mass_flux,2) .GT. 1 ) THEN
grid%mass_flux (ims:ime,jps:min(jde-1,jpe)) = grid%mass_flux (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%apr_gr,1)*SIZE(grid%apr_gr,2) .GT. 1 ) THEN
grid%apr_gr (ims:ime,jps:min(jde-1,jpe)) = grid%apr_gr (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%apr_w,1)*SIZE(grid%apr_w,2) .GT. 1 ) THEN
grid%apr_w (ims:ime,jps:min(jde-1,jpe)) = grid%apr_w (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%apr_mc,1)*SIZE(grid%apr_mc,2) .GT. 1 ) THEN
grid%apr_mc (ims:ime,jps:min(jde-1,jpe)) = grid%apr_mc (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%apr_st,1)*SIZE(grid%apr_st,2) .GT. 1 ) THEN
grid%apr_st (ims:ime,jps:min(jde-1,jpe)) = grid%apr_st (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%apr_as,1)*SIZE(grid%apr_as,2) .GT. 1 ) THEN
grid%apr_as (ims:ime,jps:min(jde-1,jpe)) = grid%apr_as (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%apr_capma,1)*SIZE(grid%apr_capma,2) .GT. 1 ) THEN
grid%apr_capma (ims:ime,jps:min(jde-1,jpe)) = grid%apr_capma (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%apr_capme,1)*SIZE(grid%apr_capme,2) .GT. 1 ) THEN
grid%apr_capme (ims:ime,jps:min(jde-1,jpe)) = grid%apr_capme (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%apr_capmi,1)*SIZE(grid%apr_capmi,2) .GT. 1 ) THEN
grid%apr_capmi (ims:ime,jps:min(jde-1,jpe)) = grid%apr_capmi (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rthften,1)*SIZE(grid%rthften,3) .GT. 1 ) THEN
grid%rthften (ims:ime,:,jps:min(jde-1,jpe)) = grid%rthften (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rqvften,1)*SIZE(grid%rqvften,3) .GT. 1 ) THEN
grid%rqvften (ims:ime,:,jps:min(jde-1,jpe)) = grid%rqvften (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%snowh,1)*SIZE(grid%snowh,2) .GT. 1 ) THEN
grid%snowh (ims:ime,jps:min(jde-1,jpe)) = grid%snowh (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rhosn,1)*SIZE(grid%rhosn,2) .GT. 1 ) THEN
grid%rhosn (ims:ime,jps:min(jde-1,jpe)) = grid%rhosn (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%smfr3d,1)*SIZE(grid%smfr3d,3) .GT. 1 ) THEN
grid%smfr3d (ims:ime,:,jps:min(jde-1,jpe)) = grid%smfr3d (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%keepfr3dflag,1)*SIZE(grid%keepfr3dflag,3) .GT. 1 ) THEN
grid%keepfr3dflag (ims:ime,:,jps:min(jde-1,jpe)) = grid%keepfr3dflag (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rhosnf,1)*SIZE(grid%rhosnf,2) .GT. 1 ) THEN
grid%rhosnf (ims:ime,jps:min(jde-1,jpe)) = grid%rhosnf (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%snowfallac,1)*SIZE(grid%snowfallac,2) .GT. 1 ) THEN
grid%snowfallac (ims:ime,jps:min(jde-1,jpe)) = grid%snowfallac (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%precipfr,1)*SIZE(grid%precipfr,2) .GT. 1 ) THEN
grid%precipfr (ims:ime,jps:min(jde-1,jpe)) = grid%precipfr (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rc_mf,1)*SIZE(grid%rc_mf,3) .GT. 1 ) THEN
grid%rc_mf (ims:ime,:,jps:min(jde-1,jpe)) = grid%rc_mf (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%flx4,1)*SIZE(grid%flx4,2) .GT. 1 ) THEN
grid%flx4 (ims:ime,jps:min(jde-1,jpe)) = grid%flx4 (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%fvb,1)*SIZE(grid%fvb,2) .GT. 1 ) THEN
grid%fvb (ims:ime,jps:min(jde-1,jpe)) = grid%fvb (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%fbur,1)*SIZE(grid%fbur,2) .GT. 1 ) THEN
grid%fbur (ims:ime,jps:min(jde-1,jpe)) = grid%fbur (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%fgsn,1)*SIZE(grid%fgsn,2) .GT. 1 ) THEN
grid%fgsn (ims:ime,jps:min(jde-1,jpe)) = grid%fgsn (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%isnowxy,1)*SIZE(grid%isnowxy,2) .GT. 1 ) THEN
grid%isnowxy (ims:ime,jps:min(jde-1,jpe)) = grid%isnowxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tvxy,1)*SIZE(grid%tvxy,2) .GT. 1 ) THEN
grid%tvxy (ims:ime,jps:min(jde-1,jpe)) = grid%tvxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tgxy,1)*SIZE(grid%tgxy,2) .GT. 1 ) THEN
grid%tgxy (ims:ime,jps:min(jde-1,jpe)) = grid%tgxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%canicexy,1)*SIZE(grid%canicexy,2) .GT. 1 ) THEN
grid%canicexy (ims:ime,jps:min(jde-1,jpe)) = grid%canicexy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%canliqxy,1)*SIZE(grid%canliqxy,2) .GT. 1 ) THEN
grid%canliqxy (ims:ime,jps:min(jde-1,jpe)) = grid%canliqxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%eahxy,1)*SIZE(grid%eahxy,2) .GT. 1 ) THEN
grid%eahxy (ims:ime,jps:min(jde-1,jpe)) = grid%eahxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tahxy,1)*SIZE(grid%tahxy,2) .GT. 1 ) THEN
grid%tahxy (ims:ime,jps:min(jde-1,jpe)) = grid%tahxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%cmxy,1)*SIZE(grid%cmxy,2) .GT. 1 ) THEN
grid%cmxy (ims:ime,jps:min(jde-1,jpe)) = grid%cmxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%chxy,1)*SIZE(grid%chxy,2) .GT. 1 ) THEN
grid%chxy (ims:ime,jps:min(jde-1,jpe)) = grid%chxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%fwetxy,1)*SIZE(grid%fwetxy,2) .GT. 1 ) THEN
grid%fwetxy (ims:ime,jps:min(jde-1,jpe)) = grid%fwetxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sneqvoxy,1)*SIZE(grid%sneqvoxy,2) .GT. 1 ) THEN
grid%sneqvoxy (ims:ime,jps:min(jde-1,jpe)) = grid%sneqvoxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%alboldxy,1)*SIZE(grid%alboldxy,2) .GT. 1 ) THEN
grid%alboldxy (ims:ime,jps:min(jde-1,jpe)) = grid%alboldxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%qsnowxy,1)*SIZE(grid%qsnowxy,2) .GT. 1 ) THEN
grid%qsnowxy (ims:ime,jps:min(jde-1,jpe)) = grid%qsnowxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%wslakexy,1)*SIZE(grid%wslakexy,2) .GT. 1 ) THEN
grid%wslakexy (ims:ime,jps:min(jde-1,jpe)) = grid%wslakexy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%zwtxy,1)*SIZE(grid%zwtxy,2) .GT. 1 ) THEN
grid%zwtxy (ims:ime,jps:min(jde-1,jpe)) = grid%zwtxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%waxy,1)*SIZE(grid%waxy,2) .GT. 1 ) THEN
grid%waxy (ims:ime,jps:min(jde-1,jpe)) = grid%waxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%wtxy,1)*SIZE(grid%wtxy,2) .GT. 1 ) THEN
grid%wtxy (ims:ime,jps:min(jde-1,jpe)) = grid%wtxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tsnoxy,1)*SIZE(grid%tsnoxy,3) .GT. 1 ) THEN
grid%tsnoxy (ims:ime,:,jps:min(jde-1,jpe)) = grid%tsnoxy (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%zsnsoxy,1)*SIZE(grid%zsnsoxy,3) .GT. 1 ) THEN
grid%zsnsoxy (ims:ime,:,jps:min(jde-1,jpe)) = grid%zsnsoxy (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%snicexy,1)*SIZE(grid%snicexy,3) .GT. 1 ) THEN
grid%snicexy (ims:ime,:,jps:min(jde-1,jpe)) = grid%snicexy (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%snliqxy,1)*SIZE(grid%snliqxy,3) .GT. 1 ) THEN
grid%snliqxy (ims:ime,:,jps:min(jde-1,jpe)) = grid%snliqxy (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%lfmassxy,1)*SIZE(grid%lfmassxy,2) .GT. 1 ) THEN
grid%lfmassxy (ims:ime,jps:min(jde-1,jpe)) = grid%lfmassxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rtmassxy,1)*SIZE(grid%rtmassxy,2) .GT. 1 ) THEN
grid%rtmassxy (ims:ime,jps:min(jde-1,jpe)) = grid%rtmassxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%stmassxy,1)*SIZE(grid%stmassxy,2) .GT. 1 ) THEN
grid%stmassxy (ims:ime,jps:min(jde-1,jpe)) = grid%stmassxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%woodxy,1)*SIZE(grid%woodxy,2) .GT. 1 ) THEN
grid%woodxy (ims:ime,jps:min(jde-1,jpe)) = grid%woodxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%stblcpxy,1)*SIZE(grid%stblcpxy,2) .GT. 1 ) THEN
grid%stblcpxy (ims:ime,jps:min(jde-1,jpe)) = grid%stblcpxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%fastcpxy,1)*SIZE(grid%fastcpxy,2) .GT. 1 ) THEN
grid%fastcpxy (ims:ime,jps:min(jde-1,jpe)) = grid%fastcpxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%xsaixy,1)*SIZE(grid%xsaixy,2) .GT. 1 ) THEN
grid%xsaixy (ims:ime,jps:min(jde-1,jpe)) = grid%xsaixy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%taussxy,1)*SIZE(grid%taussxy,2) .GT. 1 ) THEN
grid%taussxy (ims:ime,jps:min(jde-1,jpe)) = grid%taussxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%t2mvxy,1)*SIZE(grid%t2mvxy,2) .GT. 1 ) THEN
grid%t2mvxy (ims:ime,jps:min(jde-1,jpe)) = grid%t2mvxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%t2mbxy,1)*SIZE(grid%t2mbxy,2) .GT. 1 ) THEN
grid%t2mbxy (ims:ime,jps:min(jde-1,jpe)) = grid%t2mbxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%q2mvxy,1)*SIZE(grid%q2mvxy,2) .GT. 1 ) THEN
grid%q2mvxy (ims:ime,jps:min(jde-1,jpe)) = grid%q2mvxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%q2mbxy,1)*SIZE(grid%q2mbxy,2) .GT. 1 ) THEN
grid%q2mbxy (ims:ime,jps:min(jde-1,jpe)) = grid%q2mbxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tradxy,1)*SIZE(grid%tradxy,2) .GT. 1 ) THEN
grid%tradxy (ims:ime,jps:min(jde-1,jpe)) = grid%tradxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%neexy,1)*SIZE(grid%neexy,2) .GT. 1 ) THEN
grid%neexy (ims:ime,jps:min(jde-1,jpe)) = grid%neexy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%gppxy,1)*SIZE(grid%gppxy,2) .GT. 1 ) THEN
grid%gppxy (ims:ime,jps:min(jde-1,jpe)) = grid%gppxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%nppxy,1)*SIZE(grid%nppxy,2) .GT. 1 ) THEN
grid%nppxy (ims:ime,jps:min(jde-1,jpe)) = grid%nppxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%fvegxy,1)*SIZE(grid%fvegxy,2) .GT. 1 ) THEN
grid%fvegxy (ims:ime,jps:min(jde-1,jpe)) = grid%fvegxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%qinxy,1)*SIZE(grid%qinxy,2) .GT. 1 ) THEN
grid%qinxy (ims:ime,jps:min(jde-1,jpe)) = grid%qinxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%runsfxy,1)*SIZE(grid%runsfxy,2) .GT. 1 ) THEN
grid%runsfxy (ims:ime,jps:min(jde-1,jpe)) = grid%runsfxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%runsbxy,1)*SIZE(grid%runsbxy,2) .GT. 1 ) THEN
grid%runsbxy (ims:ime,jps:min(jde-1,jpe)) = grid%runsbxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ecanxy,1)*SIZE(grid%ecanxy,2) .GT. 1 ) THEN
grid%ecanxy (ims:ime,jps:min(jde-1,jpe)) = grid%ecanxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%edirxy,1)*SIZE(grid%edirxy,2) .GT. 1 ) THEN
grid%edirxy (ims:ime,jps:min(jde-1,jpe)) = grid%edirxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%etranxy,1)*SIZE(grid%etranxy,2) .GT. 1 ) THEN
grid%etranxy (ims:ime,jps:min(jde-1,jpe)) = grid%etranxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%fsaxy,1)*SIZE(grid%fsaxy,2) .GT. 1 ) THEN
grid%fsaxy (ims:ime,jps:min(jde-1,jpe)) = grid%fsaxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%firaxy,1)*SIZE(grid%firaxy,2) .GT. 1 ) THEN
grid%firaxy (ims:ime,jps:min(jde-1,jpe)) = grid%firaxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%aparxy,1)*SIZE(grid%aparxy,2) .GT. 1 ) THEN
grid%aparxy (ims:ime,jps:min(jde-1,jpe)) = grid%aparxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%psnxy,1)*SIZE(grid%psnxy,2) .GT. 1 ) THEN
grid%psnxy (ims:ime,jps:min(jde-1,jpe)) = grid%psnxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%savxy,1)*SIZE(grid%savxy,2) .GT. 1 ) THEN
grid%savxy (ims:ime,jps:min(jde-1,jpe)) = grid%savxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%sagxy,1)*SIZE(grid%sagxy,2) .GT. 1 ) THEN
grid%sagxy (ims:ime,jps:min(jde-1,jpe)) = grid%sagxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rssunxy,1)*SIZE(grid%rssunxy,2) .GT. 1 ) THEN
grid%rssunxy (ims:ime,jps:min(jde-1,jpe)) = grid%rssunxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rsshaxy,1)*SIZE(grid%rsshaxy,2) .GT. 1 ) THEN
grid%rsshaxy (ims:ime,jps:min(jde-1,jpe)) = grid%rsshaxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%bgapxy,1)*SIZE(grid%bgapxy,2) .GT. 1 ) THEN
grid%bgapxy (ims:ime,jps:min(jde-1,jpe)) = grid%bgapxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%wgapxy,1)*SIZE(grid%wgapxy,2) .GT. 1 ) THEN
grid%wgapxy (ims:ime,jps:min(jde-1,jpe)) = grid%wgapxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tgvxy,1)*SIZE(grid%tgvxy,2) .GT. 1 ) THEN
grid%tgvxy (ims:ime,jps:min(jde-1,jpe)) = grid%tgvxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%tgbxy,1)*SIZE(grid%tgbxy,2) .GT. 1 ) THEN
grid%tgbxy (ims:ime,jps:min(jde-1,jpe)) = grid%tgbxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%chvxy,1)*SIZE(grid%chvxy,2) .GT. 1 ) THEN
grid%chvxy (ims:ime,jps:min(jde-1,jpe)) = grid%chvxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%chbxy,1)*SIZE(grid%chbxy,2) .GT. 1 ) THEN
grid%chbxy (ims:ime,jps:min(jde-1,jpe)) = grid%chbxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%shgxy,1)*SIZE(grid%shgxy,2) .GT. 1 ) THEN
grid%shgxy (ims:ime,jps:min(jde-1,jpe)) = grid%shgxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%shcxy,1)*SIZE(grid%shcxy,2) .GT. 1 ) THEN
grid%shcxy (ims:ime,jps:min(jde-1,jpe)) = grid%shcxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%shbxy,1)*SIZE(grid%shbxy,2) .GT. 1 ) THEN
grid%shbxy (ims:ime,jps:min(jde-1,jpe)) = grid%shbxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%evgxy,1)*SIZE(grid%evgxy,2) .GT. 1 ) THEN
grid%evgxy (ims:ime,jps:min(jde-1,jpe)) = grid%evgxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%evbxy,1)*SIZE(grid%evbxy,2) .GT. 1 ) THEN
grid%evbxy (ims:ime,jps:min(jde-1,jpe)) = grid%evbxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ghvxy,1)*SIZE(grid%ghvxy,2) .GT. 1 ) THEN
grid%ghvxy (ims:ime,jps:min(jde-1,jpe)) = grid%ghvxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ghbxy,1)*SIZE(grid%ghbxy,2) .GT. 1 ) THEN
grid%ghbxy (ims:ime,jps:min(jde-1,jpe)) = grid%ghbxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%irgxy,1)*SIZE(grid%irgxy,2) .GT. 1 ) THEN
grid%irgxy (ims:ime,jps:min(jde-1,jpe)) = grid%irgxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%ircxy,1)*SIZE(grid%ircxy,2) .GT. 1 ) THEN
grid%ircxy (ims:ime,jps:min(jde-1,jpe)) = grid%ircxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%irbxy,1)*SIZE(grid%irbxy,2) .GT. 1 ) THEN
grid%irbxy (ims:ime,jps:min(jde-1,jpe)) = grid%irbxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%trxy,1)*SIZE(grid%trxy,2) .GT. 1 ) THEN
grid%trxy (ims:ime,jps:min(jde-1,jpe)) = grid%trxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%evcxy,1)*SIZE(grid%evcxy,2) .GT. 1 ) THEN
grid%evcxy (ims:ime,jps:min(jde-1,jpe)) = grid%evcxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%chleafxy,1)*SIZE(grid%chleafxy,2) .GT. 1 ) THEN
grid%chleafxy (ims:ime,jps:min(jde-1,jpe)) = grid%chleafxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%chucxy,1)*SIZE(grid%chucxy,2) .GT. 1 ) THEN
grid%chucxy (ims:ime,jps:min(jde-1,jpe)) = grid%chucxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%chv2xy,1)*SIZE(grid%chv2xy,2) .GT. 1 ) THEN
grid%chv2xy (ims:ime,jps:min(jde-1,jpe)) = grid%chv2xy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%chb2xy,1)*SIZE(grid%chb2xy,2) .GT. 1 ) THEN
grid%chb2xy (ims:ime,jps:min(jde-1,jpe)) = grid%chb2xy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%chstarxy,1)*SIZE(grid%chstarxy,2) .GT. 1 ) THEN
grid%chstarxy (ims:ime,jps:min(jde-1,jpe)) = grid%chstarxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%smoiseq,1)*SIZE(grid%smoiseq,3) .GT. 1 ) THEN
grid%smoiseq (ims:ime,:,jps:min(jde-1,jpe)) = grid%smoiseq (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%smcwtdxy,1)*SIZE(grid%smcwtdxy,2) .GT. 1 ) THEN
grid%smcwtdxy (ims:ime,jps:min(jde-1,jpe)) = grid%smcwtdxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%rechxy,1)*SIZE(grid%rechxy,2) .GT. 1 ) THEN
grid%rechxy (ims:ime,jps:min(jde-1,jpe)) = grid%rechxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%deeprechxy,1)*SIZE(grid%deeprechxy,2) .GT. 1 ) THEN
grid%deeprechxy (ims:ime,jps:min(jde-1,jpe)) = grid%deeprechxy (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
IF ( SIZE(grid%power,1)*SIZE(grid%power,2) .GT. 1 ) THEN
grid%power (ims:ime,jps:min(jde-1,jpe)) = grid%power (ims:ime,jps+py:min(jde-1,jpe)+py)
ENDIF
  DO itrace = PARAM_FIRST_SCALAR, num_ozmixm
IF ( SIZE(grid%ozmixm,1)*SIZE(grid%ozmixm,3) .GT. 1 ) THEN
grid%ozmixm (ims:ime,:,jps:min(jde-1,jpe),itrace) = grid%ozmixm (ims:ime,:,jps+py:min(jde-1,jpe)+py,itrace)
ENDIF
  ENDDO
IF ( SIZE(grid%o3rad,1)*SIZE(grid%o3rad,3) .GT. 1 ) THEN
grid%o3rad (ims:ime,:,jps:min(jde-1,jpe)) = grid%o3rad (ims:ime,:,jps+py:min(jde-1,jpe)+py)
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
