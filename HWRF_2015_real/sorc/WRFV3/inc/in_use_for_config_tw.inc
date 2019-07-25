!STARTOFREGISTRYGENERATEDINCLUDE 'inc/in_use_for_config_tw.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
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
!ENDOFREGISTRYGENERATEDINCLUDE
