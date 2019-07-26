!STARTOFREGISTRYGENERATEDINCLUDE 'inc/REGISTRY_COMM_DM_subs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
SUBROUTINE SHIFT_HALO_X_HALO_sub ( grid, &
  config_flags, &
  num_szj, &
  szj, &
  num_s1z, &
  s1z, &
  num_spz, &
  spz, &
  num_tcs, &
  tcs, &
  num_moist, &
  moist, &
  num_dfi_moist, &
  dfi_moist, &
  num_scalar, &
  scalar, &
  num_dfi_scalar, &
  dfi_scalar, &
  num_chem, &
  chem, &
  num_ozmixm, &
  ozmixm, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: num_szj
  real, INTENT(INOUT) :: szj ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33 ,num_szj)
  INTEGER, INTENT(IN) :: num_s1z
  real, INTENT(INOUT) :: s1z ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33 ,num_s1z)
  INTEGER, INTENT(IN) :: num_spz
  real, INTENT(INOUT) :: spz ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33 ,num_spz)
  INTEGER, INTENT(IN) :: num_tcs
  real, INTENT(INOUT) :: tcs ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33 ,num_tcs)
  INTEGER, INTENT(IN) :: num_moist
  real, INTENT(INOUT) :: moist ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33 ,num_moist)
  INTEGER, INTENT(IN) :: num_dfi_moist
  real, INTENT(INOUT) :: dfi_moist ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33 ,num_dfi_moist)
  INTEGER, INTENT(IN) :: num_scalar
  real, INTENT(INOUT) :: scalar ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33 ,num_scalar)
  INTEGER, INTENT(IN) :: num_dfi_scalar
  real, INTENT(INOUT) :: dfi_scalar ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33 ,num_dfi_scalar)
  INTEGER, INTENT(IN) :: num_chem
  real, INTENT(INOUT) :: chem ( grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32 ,num_chem)
  INTEGER, INTENT(IN) :: num_ozmixm
  real, INTENT(INOUT) :: ozmixm ( grid%sm31:grid%em31,1:grid%levsiz,grid%sm32:grid%em32 ,num_ozmixm)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  
#ifdef DM_PARALLEL
#include "SHIFT_HALO_X_HALO_inline.inc"
#endif
  
  END SUBROUTINE SHIFT_HALO_X_HALO_sub
!ENDOFREGISTRYGENERATEDINCLUDE
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/REGISTRY_COMM_DM_subs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
SUBROUTINE SHIFT_HALO_Y_HALO_sub ( grid, &
  config_flags, &
  num_szj, &
  szj, &
  num_s1z, &
  s1z, &
  num_spz, &
  spz, &
  num_tcs, &
  tcs, &
  num_moist, &
  moist, &
  num_dfi_moist, &
  dfi_moist, &
  num_scalar, &
  scalar, &
  num_dfi_scalar, &
  dfi_scalar, &
  num_chem, &
  chem, &
  num_ozmixm, &
  ozmixm, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )
  USE module_domain, ONLY:domain
  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config
  USE module_state_description, ONLY:PARAM_FIRST_SCALAR
  USE module_driver_constants
  TYPE(domain) ,               INTENT(IN) :: grid
  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: num_szj
  real, INTENT(INOUT) :: szj ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33 ,num_szj)
  INTEGER, INTENT(IN) :: num_s1z
  real, INTENT(INOUT) :: s1z ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33 ,num_s1z)
  INTEGER, INTENT(IN) :: num_spz
  real, INTENT(INOUT) :: spz ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33 ,num_spz)
  INTEGER, INTENT(IN) :: num_tcs
  real, INTENT(INOUT) :: tcs ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33 ,num_tcs)
  INTEGER, INTENT(IN) :: num_moist
  real, INTENT(INOUT) :: moist ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33 ,num_moist)
  INTEGER, INTENT(IN) :: num_dfi_moist
  real, INTENT(INOUT) :: dfi_moist ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33 ,num_dfi_moist)
  INTEGER, INTENT(IN) :: num_scalar
  real, INTENT(INOUT) :: scalar ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33 ,num_scalar)
  INTEGER, INTENT(IN) :: num_dfi_scalar
  real, INTENT(INOUT) :: dfi_scalar ( grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33 ,num_dfi_scalar)
  INTEGER, INTENT(IN) :: num_chem
  real, INTENT(INOUT) :: chem ( grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32 ,num_chem)
  INTEGER, INTENT(IN) :: num_ozmixm
  real, INTENT(INOUT) :: ozmixm ( grid%sm31:grid%em31,1:grid%levsiz,grid%sm32:grid%em32 ,num_ozmixm)
  INTEGER ,                    INTENT(IN) :: local_communicator
  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y
  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde
  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme
  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: itrace
  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
  LOGICAL, EXTERNAL :: rsl_comm_iter
  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7
  
#ifdef DM_PARALLEL
#include "SHIFT_HALO_Y_HALO_inline.inc"
#endif
  
  END SUBROUTINE SHIFT_HALO_Y_HALO_sub
!ENDOFREGISTRYGENERATEDINCLUDE
