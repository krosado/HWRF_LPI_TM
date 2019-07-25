!STARTOFREGISTRYGENERATEDINCLUDE 'inc/in_use_for_config_gk.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
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
!ENDOFREGISTRYGENERATEDINCLUDE
