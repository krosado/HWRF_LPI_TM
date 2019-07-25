!SBAO2008-09-11-17:07
!THE following 2 commands are used to compile this diffwrf_3dvar.f90
!not the 3 subroutines at the end of cannot be deleted.
!and this program must sit in this dir: $WRFPATH/external/io_netcdf when you compile it.
!  pgf90 -c -I../ioapi_share -g diffwrf_3dvar.f90
!  pgf90 -o diffwrf_3dvar diffwrf_3dvar.o ../../frame/pack_utils.o 
!../../frame/module_internal_header_util.o ../../frame/module_driver_constants.o ../../frame/module_machine.o 
!../../frame/wrf_debug.o ../../frame/module_wrf_error.o wrf_io.o module_wrfsi_static.o field_routines.o 
!-L$NETCDF/lib -lnetcdf

!??starting from gopal's diffwrf.F90, modify to extract????
!??following variables and write binary file for access ???
!???by 3dvar code: ????????????????????????????????????????
!???                                            ???????????
!???  NHB file used in                          ???????????
!???   gridmod, gsimain, read_files, read_reg_guess ???????
!???                                            ???????????
!??? x   dt (time-step for model, probably no longer needed--used to get fhour
!??? x   pdtop,pt,deta1,eta1,aeta1              ???????????
!??? x            deta2,eta2,aeta2              ???????????
!??? x   dlmd,dphd                              ???????????
!??? x   tlm0d,tph0d                            ???????????
!??? x   wbd,sbd                                ???????????
!??? x    wbd=-(im-1)*dlmd                      ???????????
!??? x    sbd=-((jm-1)/2)*dphd                  ???????????
!???                                            ???????????
!???                                            ???????????
!???     zero=wbd+(im-1)*dlmd                   ???????????
!???      must be at i=1+(im-1)/2               ???????????
!???     zero=sbd+((jm-1)/2)*dphd               ???????????
!???      must be at j=1+(jm-1)/2               ???????????
!???                                            ???????????
!???                                            ???????????
!???                                            ???????????
!???                                            ???????????
!??? x   sm                                     ???????????
!??? x   sice                                   ???????????
!??? x   sst                                    ???????????
!??? x   ivgtyp                                 ???????????
!??? x   isltyp                                 ???????????
!??? x   vegfrac                                ???????????
!???                                            ???????????
!???                                            ???????????
!???                                            ???????????
!???  restart file used in                      ???????????
!???   get_regional_time, gridmod, gsimain, read_reg_guess, wrrega
!???                                            ???????????
!??? x idat,ihrst,ntsd --all used to get idateg,hourg4?????
!???                                            ???????????
!???        idat(1)--month                      ???????????
!???        idat(2)--day                        ???????????
!???        idat(3)--year                       ???????????
!???        ihrst--initial hour                 ???????????
!???        ntsd--number of timesteps, so fcsthr is????????
!???         hourg4=(ntsd-1.)*dt/3600.  (hrs)   ???????????
!???                                            ???????????
!???        idateg(1)-- initial hour            ???????????
!???        idateg(2)-- month                   ???????????
!???        idateg(3)-- day                     ???????????
!???        idateg(4)-- year                    ???????????
!???        hourg4--forecast hour               ???????????
!???                                            ???????????
!???   pd,fis,t,q,u,v,sno,u10,v10,smc,stc       ???????????
!???                                            ???????????
!???                                            ???????????
!???                                            ???????????
!???                                            ???????????
!???                                            ???????????
!???  namelist constants, some or all of which can be replaced with info from nmm netcdf file
!???   nlon_regional  (im)                      ???????????
!???   nlat_regional  (jm)                      ???????????
!???                                            ???????????
!???                                            ???????????
!???                                            ???????????
!???                                            ???????????
!???                                            ???????????
!???                                            ???????????
!??????????????????????????????????????????????????????????
!??????????????????????????????????????????????????????????
!??????????????????????????????????????????????????????????
!???                                            ???????????
!???  DT     constant    (probably not needed)  ???????????
!???        idat,ihrst,ntsd  (")                ???????????
!???  PDTOP  const                              ???????????
!???  PT     const                              ???????????
!???     DETA1                                  ???????????
!???     AETA1                                  ???????????
!???     ETA1                                   ???????????
!???     DETA2                                  ???????????
!???     AETA2                                  ???????????
!???     ETA2                                   ???????????
!???  DLMD   const                              ???????????
!???  DPHD   const                              ???????????
!???  tlm0d,tph0d   (check names)               ???????????
!???      wbd=(im-1)*dlmd                       ???????????
!???      sbd=((jm-1)/2)*dphd                   ???????????
!???  SM      2d                                ???????????
!???  SICE    2d                                ???????????
!???  SST                                       ???????????
!???  IVGTYP                                    ???????????
!???  ISLTYP                                    ???????????
!???  VEGFRC                                    ???????????
!???                                            ???????????
!???  PD      2d                                ???????????
!???  FIS     2d                                ???????????
!???                                            ???????????
!???  T       3d                                ???????????
!???  Q       3d                                ???????????
!???  U       3d                                ???????????
!???  V       3d                                ???????????
!???  SNO     2d                                ???????????
!???  U10     2d                                ???????????
!???  V10     2d                                ???????????
!???  SMC     3d                                ???????????
!???  STC     3d                                ???????????
!???                                            ???????????
!??????????????????????????????????????????????????????????
!??????????????????????????????????????????????????????????
!??????????????????????????????????????????????????????????
!===========================================================================
!
!                Extended diffwrf utility
!===========================================================================
!
!
! 0. unified 3dvar interface with wrf nmm:
! -----------------
! Usage: diffwrf [opcode] [file1] [file2]
!
!           opcode = "3dvar_guess", then
!              file1 = file name of wrf nmm netcdf data
!              file2 = output binary file to be read by unified 3dvar code
!
!           opcode = "3dvar_update", then
!              file1 = file name of wrf nmm netcdf data
!              file2 = input binary file containing analysis from 3dvar code.
!                       file1 is updated with contents of file2.
!
!           opcode = "3dvar_inventory", then
!              file1 = file name of wrf nmm netcdf data
!              stdout contains list if all variables on input netcdf dataset
!
! 1. netcdf output:
! -----------------
! Usage: diffwrf [file0] [file1] [file2] [scale] [variables]
!
! 2. rescaling at restart for producing scaled breeding (Toth and Kalnay, 1997)
! -----------------------------------------------------------------------------
! Usage: diffwrf [file0] [file1] [file2] [scale] [5 vars] breed [var] [limit]
!
! 3. binary output (for binary version of real)
! ----------------------------------------------
! Usage: diffwrf [file0] [file1] [file2] [scale] [5 variables] bin [bin_fname]
!
!------------------------------------------------------------------------------
!
! Description:
! ------------
!
! Further extension of gopal's diffwrf utility to function as 
! interface between nmm netcdf file and unified gridpoint 3dvar
! code.  

! This version of diffwrf.F90 is an extension of John Michalakes 
! version of the diffwrf utility. This extension  is useful for 
! for generating perturbations either to initial or to boundary
! conditions (or both) and may be used for the purpose Ensemble 
! forecasting and breeding method. 
!
! This version reads in 3  NetCDF files of comparable sizes  and
! matching variable names, i.e., files 0, 1 & 2; Uses files 1  &
! 2 to modify a maximum of 5 variables (example:  U or U_2  &/or   
! V or V_2 &/or T or T_2 &/or MU0 or MU_2 etc..) as they  appear 
! in the NetCDF file) in file 0 depending on a scale factor. The
! factor can be preset between 0.1 and 1.0.We suggest a value of 
! 1. for creating perturbations to the boundary conditions (i.e.
! while using  diffwrf on NetCDF SI output files).  However, the
! perturbations to initial conditions (i.e., while using diffwrf 
! diffwrf on restart file) can either be preset or automatically 
! rescaled, depending on additional script-provided input option 
! "breed" or "restart". While using the rescale option the scale
! factor is set to  0.5 if perturbations  are  determined on the 
! basis of differences between two perturbed outputs (say, P and
! N) and to a value of 1.0  if perturbations are determined from
! the differences  between control and say, P. The rescale usage
! option also requires the name of the field (example: T_2 or  T 
! or U_2 or U as it appears in the netcdf output/restart file) & 
! a maximum  value of  perturbation. This  limiting value may be  
! obtained from the regional climatology.
!
! The rescale option  is analogous to the  medium-range ensembles, 
! and may be applied between two 24 hour restart files of positive  
! and the negative  breeding cycles to determine the error growth.
! Depending on the error growth in the previous cycle, the initial
! perturbations in the current cycle are rescaled to a climatologi
! -cal value very similar to procedures followed in medium range
! ensemble forecasting (see Toth and Kalnay, 1997). 
!
! The older version of diffwrf utility may be obtained by setting 
! ensemble to false
!
!--------------------------------------------------------------------
!
! Initial version : Gopal                (sgkrish@noaa.gov)
!
!=============================================================================

!!$module read_util_module
!!$
!!$contains

!!$   subroutine arguments(v2file, lmore)
!!$     implicit none
!!$     character(len=*) :: v2file
!!$     character(len=120) :: harg
!!$     logical :: lmore
!!$   
!!$     integer :: ierr, i, numarg
!!$     integer, external :: iargc
!!$   
!!$     numarg = iargc()
!!$   
!!$     i = 1
!!$     lmore = .false.
!!$   
!!$     do while ( i < numarg) 
!!$        call getarg(i, harg)
!!$        print*, 'harg = ', trim(harg)
!!$   
!!$        if (harg == "-v") then
!!$           i = i + 1
!!$           lmore = .true.
!!$        elseif (harg == "-h") then
!!$           call help
!!$        endif
!!$   
!!$     enddo
!!$   
!!$     call getarg(i,harg)
!!$     v2file = harg
!!$   end subroutine arguments
   
!!$   subroutine help
!!$     implicit none
!!$     character(len=120) :: cmd
!!$     call getarg(0, cmd)
!!$   
!!$     write(*,'(/,"Usage: ", A, " [-v] v2file ")') trim(cmd)
!!$     write(*,'(8x, "-v     : Print extra info")')
!!$     write(*,'(8x, "v3file : MM5v3 file name to read.")')
!!$     write(*,'(8x, "-h     : print this help message and exit.",/)')
!!$     stop
!!$   end subroutine help
!!$end module read_util_module


!============================================================================


!!$module binary_output
!!$
!!$contains
!!$
!!$ subroutine binary_write_get_domain_info(dh,unit,xdim,ydim,zdim)
!!$
!!$   use wrf_data
!!$   use read_util_module
!!$   implicit none
!!$   include 'wrf_status_codes.h'
!!$   include 'netcdf.inc'
!!$
!!$!  i/o
!!$  
!!$   integer          , intent(in)  :: dh,unit
!!$   integer          , intent(out) :: xdim,ydim,zdim  
!!$
!!$!  local
!!$
!!$   integer, parameter        :: idummy=1
!!$
!!$   integer, parameter        :: max_staggers_xy_new = 4
!!$   integer, parameter        :: max_staggers_xy_old = 3
!!$   integer                   :: si_version  
!!$   integer                   :: cross,ierr
!!$   integer                   :: id
!!$   integer                   :: parent_id
!!$   character(len=8)          :: dyn_init_src
!!$   character(len=8)          :: static_init_src
!!$   integer                   :: vt_date
!!$   real                      :: vt_time
!!$   integer                   :: origin_parent_x
!!$   integer                   :: origin_parent_y
!!$   integer                   :: ratio_to_parent
!!$   real                      :: delta_x
!!$   real                      :: delta_y
!!$   real                      :: top_level    ! verify with Dave
!!$   integer                   :: origin_parent_z
!!$   integer                   :: temp(5)
!!$   real                      :: corner_lats_new(4,max_staggers_xy_new)
!!$   real                      :: corner_lons_new(4,max_staggers_xy_new)
!!$   real                      :: corner_lats_old(4,max_staggers_xy_old)
!!$   real                      :: corner_lons_old(4,max_staggers_xy_old)
!!$
!!$!===========================================================================
!!$
!!$      call ext_ncd_get_dom_ti_integer(dh,'si_version',si_version,1,cross,ierr)
!!$      call ext_ncd_get_dom_ti_integer(dh,'WEST-EAST_GRID_DIMENSION', &
!!$                                          xdim,1,cross,ierr)
!!$      call ext_ncd_get_dom_ti_integer(dh,'SOUTH-NORTH_GRID_DIMENSION', &
!!$                                          ydim,1,cross,ierr)
!!$
!!$
!!$!      call ext_ncd_get_dom_ti_integer(dh,'BOTTOM-TOP_GRID_DIMENSION', &
!!$!                                          zdim,1,cross,ierr)
!!$
!!$!      This is a patch for "BOTTOM-TOP_GRID_DIMENSION"
!!$!-------------------------------------------------------------------
!!$       call ext_ncd_get_var_ti_integer(dh,'dim_val','ZNW',   &
!!$                                       temp,5,cross,ierr)
!!$       zdim=temp(1)
!!$!--------------------------------------------------------------------
!!$
!!$      call ext_ncd_get_dom_ti_real(dh,'DX',delta_x,1,cross,ierr)
!!$      call ext_ncd_get_dom_ti_real(dh,'DY',delta_y,1,cross,ierr) 
!!$      call ext_ncd_get_dom_ti_real(dh,'P_TOP',top_level,1,cross,ierr)
!!$
!!$       if(si_version .eq. 1 )then
!!$!         write(unit) id,parent_id,dyn_init_src,                   &
!!$!                   static_init_src, vt_date, vt_time,           &
!!$!                   origin_parent_x, origin_parent_y,            &
!!$!                   ratio_to_parent, delta_x, delta_y,           &
!!$!                   top_level, origin_parent_z,                  &
!!$!                   corner_lats_old,corner_lons_old,xdim,ydim,zdim
!!$       write(*,*)'ERROR: This program does not work for si_version=1'
!!$       stop
!!$       else if (si_version .eq. 2 )then
!!$!         write(unit) id,parent_id,dyn_init_src,                   &
!!$!                   static_init_src, vt_date, vt_time,           &
!!$!                   origin_parent_x, origin_parent_y,            &
!!$!                   ratio_to_parent, delta_x, delta_y,           &
!!$!                   top_level, origin_parent_z,                  &
!!$!                   corner_lats_new, corner_lons_new,xdim,ydim,zdim
!!$
!!$!
!!$!      This is just a patch
!!$!
!!$       corner_lats_new=0.00
!!$       corner_lons_new=0.00
!!$       call ext_ncd_get_var_ti_integer(dh,'vt_date_start','ZNW',      &
!!$                                       vt_date,1,cross,ierr)
!!$       call ext_ncd_get_var_ti_real(dh,'vt_time_start','ZNW',         &
!!$                                     vt_time,1,cross,ierr)
!!$
!!$
!!$         write(unit)idummy,(1-idummy),'SI      ',               &
!!$                   'SI      ', vt_date, vt_time,                &
!!$                   idummy, idummy,                              &
!!$                   idummy, delta_x, delta_y,                    &
!!$                   top_level, idummy,                           &
!!$                   corner_lats_new, corner_lons_new,xdim,ydim,zdim
!!$
!!$       endif
!!$
!!$ end subroutine binary_write_get_domain_info
!!$
!!$!=========================================================================
!!$
!!$ subroutine binary_write_get_var_info(dh,VarName,unit)
!!$
!!$   use wrf_data
!!$   use read_util_module
!!$   implicit none
!!$   include 'wrf_status_codes.h'
!!$   include 'netcdf.inc'
!!$
!!$! input
!!$
!!$   character(len=31), intent(in)  :: VarName
!!$   integer          , intent(in)  :: dh,unit
!!$
!!$!  local
!!$
!!$   integer, parameter        :: max_domains = 10
!!$   integer, parameter        :: var_maxdims = 5
!!$   integer, parameter        :: max_staggers_xy_new = 4
!!$   integer, parameter        :: max_staggers_xy_old = 3
!!$   integer, parameter        :: max_staggers_z = 2
!!$   integer, parameter        :: max_standard_lats = 4
!!$   integer, parameter        :: max_standard_lons = 4
!!$   integer, parameter        :: max_fg_variables = 200
!!$   integer, parameter        :: max_vertical_levels = 2000
!!$
!!$   character (len=8)              :: bin_name
!!$   character (len=16)             :: bin_units
!!$   character (len=80)             :: bin_description
!!$   integer                        :: si_version
!!$   integer                        :: bin_domain_id
!!$   integer                        :: bin_ndim
!!$   integer                        :: bin_dim_val(var_maxdims) 
!!$   character(len=4)               :: bin_dim_desc (var_maxdims)
!!$   integer                        :: bin_start_index(var_maxdims)
!!$   integer                        :: bin_stop_index(var_maxdims)
!!$   integer                        :: bin_h_stagger_index
!!$   integer                        :: bin_v_stagger_index
!!$   character(len=8)               :: bin_array_order
!!$   character(len=4)               :: bin_field_type
!!$   character(len=8)               :: bin_field_source_prog
!!$   character(len=80)              :: bin_source_desc
!!$   character(len=8)               :: bin_field_time_type
!!$   integer                        :: bin_vt_date_start
!!$   real                           :: bin_vt_time_start
!!$   integer                        :: bin_vt_date_stop
!!$   real                           :: bin_vt_time_stop
!!$   character(len=1)               :: bin_stagger
!!$   integer                        :: cross,ierr 
!!$!=========================================================================
!!$
!!$
!!$!    write_binary re-write for all variables (read for unit 13 in DTC mass core)
!!$
!!$       bin_name=''
!!$       bin_units=''
!!$       bin_description=''
!!$       bin_dim_desc='' 
!!$       bin_array_order=''
!!$       bin_field_type=''
!!$       bin_field_source_prog=''
!!$       bin_source_desc=''
!!$       bin_field_time_type=''
!!$
!!$       call ext_ncd_get_var_ti_char(dh,'name',VarName,                 &
!!$                                     bin_name,ierr)
!!$       call ext_ncd_get_var_ti_char(dh,'units',VarName,                &
!!$                                     bin_units,ierr)
!!$       call ext_ncd_get_var_ti_char(dh,'description',VarName,          &
!!$                                     bin_description,ierr)
!!$       call ext_ncd_get_var_ti_char(dh,'dim_desc1',VarName,            &
!!$                                     bin_dim_desc(1),ierr)
!!$       call ext_ncd_get_var_ti_char(dh,'dim_desc2',VarName,            &
!!$                                     bin_dim_desc(2),ierr)
!!$       call ext_ncd_get_var_ti_char(dh,'dim_desc3',VarName,            &
!!$                                     bin_dim_desc(3),ierr)
!!$       call ext_ncd_get_var_ti_char(dh,'dim_desc4',VarName,            &
!!$                                     bin_dim_desc(4),ierr)
!!$       call ext_ncd_get_var_ti_char(dh,'dim_desc5',VarName,            &
!!$                                     bin_dim_desc(5),ierr)
!!$       call ext_ncd_get_var_ti_char(dh,'array_order',VarName,          &
!!$                                     bin_array_order,ierr)
!!$       call ext_ncd_get_var_ti_char(dh,'field_type',VarName,           & 
!!$                                     bin_field_type,ierr)
!!$       call ext_ncd_get_var_ti_char(dh,'field_source_prog',VarName,    &
!!$                                     bin_field_source_prog,ierr) 
!!$       call ext_ncd_get_var_ti_char(dh,'source_desc',VarName,          &
!!$                                     bin_source_desc,ierr)
!!$       call ext_ncd_get_var_ti_char(dh,'field_time_type',VarName,      &
!!$                                     bin_field_time_type,ierr)
!!$
!!$       call ext_ncd_get_var_ti_integer(dh,'domain_id',VarName,         &
!!$                                        bin_domain_id,1,cross,ierr)
!!$       call ext_ncd_get_var_ti_integer(dh,'ndim',VarName,              &
!!$                                        bin_ndim,1,cross,ierr)
!!$       call ext_ncd_get_var_ti_integer(dh,'start_index',VarName,       &
!!$                                        bin_start_index,var_maxdims,cross,ierr)
!!$       call ext_ncd_get_var_ti_integer(dh,'stop_index',VarName,        &
!!$                                        bin_stop_index,var_maxdims,cross,ierr)
!!$       call ext_ncd_get_var_ti_integer(dh,'domain_id',VarName,         &
!!$                                        bin_domain_id,1,cross,ierr)
!!$       call ext_ncd_get_var_ti_integer(dh,'dim_val',VarName,           &
!!$                                        bin_dim_val,var_maxdims,cross,ierr)
!!$       call ext_ncd_get_var_ti_integer(dh,'h_stagger_index',VarName,   &
!!$                                        bin_h_stagger_index,1,cross,ierr)
!!$       call ext_ncd_get_var_ti_integer(dh,'v_stagger_index',VarName,   &
!!$                                        bin_v_stagger_index,1,cross,ierr)
!!$       call ext_ncd_get_var_ti_integer(dh,'vt_date_start',VarName,   &
!!$                                        bin_vt_date_start,1,cross,ierr) 
!!$       call ext_ncd_get_var_ti_integer(dh,'vt_date_stop',VarName,   &
!!$                                        bin_vt_date_stop,1,cross,ierr)
!!$
!!$
!!$       call ext_ncd_get_var_ti_real(dh,'vt_time_start',VarName,   &
!!$                                     bin_vt_time_start,1,cross,ierr)
!!$       call ext_ncd_get_var_ti_real(dh,'vt_time_stop',VarName,   &
!!$                                     bin_vt_time_stop,1,cross,ierr)
!!$
!!$       write(unit)bin_name,bin_units,bin_description,bin_domain_id,       &
!!$                  bin_ndim,bin_dim_val,bin_dim_desc,bin_start_index,      &
!!$                  bin_stop_index,bin_h_stagger_index,bin_v_stagger_index, &
!!$                  bin_array_order,bin_field_type,bin_field_source_prog,   &
!!$                  bin_source_desc,bin_field_time_type,bin_vt_date_start,  &
!!$                  bin_vt_time_start,bin_vt_date_stop,bin_vt_time_stop    
!!$
!!$       write(87,*)bin_name
!!$
!!$end subroutine binary_write_get_var_info 
!!$
!!$end module binary_output

!============================================================================== 
program readv3
  use wrf_data
!  use read_util_module
!  use binary_output
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  character(len=120) :: flnm,flnm0,flnm1,flnm2,flnm3
  character(len=120) :: arg3
  character(len=19)  :: DateStr,DateStr1,DateStr2
  character(len=31)  :: VarName,VarName1,VarName2
  integer            :: dh0,dh1,dh2

  integer :: flag, flag2
  integer :: iunit, iunit2

  integer :: i,j,k
  integer :: levlim
  integer :: cross
  integer :: ndim, ndim1,ndim2
  integer :: WrfType, WrfType1, WrfType2
  real    :: time, time1, time2
  real    :: a, b, old_data, scale
  integer, dimension(4)  :: start_index, end_index, start_index1, end_index1
  integer, dimension(4)  :: start_index2, end_index2
  integer , Dimension(3) :: MemS,MemE,PatS,PatE
  character (len= 4) :: staggering,staggering1,staggering2
  character (len= 3) :: ordering,ordering1,ordering2, ord
  character (len=24) :: start_date,start_date1,start_date2
  character (len=24) :: current_date,current_date1,current_date2
  character (len=31) :: name,name1,name2,name3,name4,name5
  character (len=25) :: units,        units2
  character (len=46) :: description,  description2

  character (len=80), dimension(3)  ::  dimnames
  character (len=80) :: SysDepInfo

  integer :: l, n
  integer :: ikdiffs, ifdiffs

  real, allocatable, dimension(:,:,:,:) :: data,data1,data2

  integer :: ierr, ierr2, ier, ier2, Status, Status_next_time
  integer :: Status_next_time2, Status_next_var, Status_next_var_2

  logical :: ensemble = .FALSE.
  logical :: nmm3dvar = .TRUE.
  logical :: newtime  = .TRUE.
  logical :: justplot, efound

  integer, external :: iargc
  logical, external :: iveceq

  ! binary stuff

  logical :: write_binary = .FALSE.
  integer :: ix,jx,kx
  real, allocatable, dimension(:,:,:,:) :: bin_data 

  ! rmse stuff

  logical :: rescale = .FALSE.
  real:: rms_error, pert_limit
  character (len=31) :: rmse_var 
  real, allocatable, dimension(:,:,:,:) :: rdata1,rdata2

  integer(4) iyear,imonth,iday,ihour,iminute,isecond
  integer(4) nlon_regional,nlat_regional,nsig_regional
  real(4) pt_regional,pdtop_regional
  real(4) dlmd_regional,dphd_regional,CLON1,CLAT1
  real(4),allocatable::field3(:,:,:),field31(:,:,:),field2(:,:),field1(:)
  integer(4),allocatable::ifield2(:,:)
  real(4) rad2deg
  integer(4) istop,loop
  !=========================================================================

  ! storm relocation stuff

  real(4),allocatable::GLON(:,:),GLAT(:,:),HLON(:,:),HLAT(:,:),VLON(:,:),VLAT(:,:)
  real(4),allocatable::T(:,:,:),Q(:,:,:),U(:,:,:),V(:,:,:)
  real(4),allocatable::A101(:,:),B101(:,:),C101(:,:)
  real(4),allocatable::PSFC(:,:),ZINT(:,:,:),TKE(:,:,:),CWM(:,:,:),SLP(:,:)
  real(4),allocatable::PINT(:,:,:),FI(:,:,:),PD(:,:),ETA1(:),ETA2(:)

  REAL(4) DDDX,DDDY ! ,DDDX1(286,636),DDDY1(286,636)
  real(4), allocatable, dimension(:,:) :: DDDX1, DDDY1
  ! end of storm relocation stuff

  call ext_ncd_ioinit(SysDepInfo,Status)
  call set_wrf_debug_level ( 1 )


  if(nmm3dvar) then
     !   ------------------------------------------
     !         extract from guess nmm netcdf file stuff for 3dvar
     !   ------------------------------------------
     call getarg(1,arg3)
     if(trim(arg3).ne.'3dvar_guess'.and.trim(arg3).ne.'3dvar_update'.and.trim(arg3)    &
          .ne.'3dvar_inventory'.and.trim(arg3).ne.'storm_relocate') then
        print *,' for nmm 3dvar 1st argument must be "3dvar_update", "3dvar_guess",  or "3dvar_inventory" '
        stop
     endif
     if(trim(arg3).eq.'3dvar_inventory') then
        call getarg(2,flnm1)
        call ext_ncd_open_for_read( trim(flnm1), 0, 0, "", dh1, Status)
        if ( Status /= 0 )then
           print*,'error opening',flnm1, ' Status = ', Status
           print*
           print*,'USAGE: diffwrf [opcode] [file1] [file2]'
           print*
           stop
        endif

        !-------------  get date info

        call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
        read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
        print *,' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond
        do loop=1,10000
           call ext_ncd_get_next_var(dh1, rmse_var, Status)
           print *,' name, status = ', trim(rmse_var), Status
           if(Status.ne.0) exit
           call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
                start_index,end_index1, WrfType, ierr    )
           print *,' ordering=',ordering
           print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
           print *,' ndim1=',ndim1
           print *,' staggering=',staggering
           print *,' start_index=',start_index
           print *,' end_index1=',end_index1
        end do
        stop
     endif
     if(trim(arg3).eq.'3dvar_guess') then
        call getarg(2,flnm1)
        call getarg(3,flnm2)
        call ext_ncd_open_for_read( trim(flnm1), 0, 0, "", dh1, Status)
        if ( Status /= 0 )then
           print*,'error opening',flnm1, ' Status = ', Status
           print*
           print*,'USAGE: diffwrf [opcode] [file1] [file2]'
           print*
           stop
        endif


        iunit=12357
        open(iunit,file=flnm2,form='unformatted')

        !-------------  get date info

        call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
        read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
        print *,' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond

        !-------------  get grid info
        rmse_var='T'
        call ext_ncd_get_var_info (dh1,rmse_var,ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        nlon_regional=end_index1(1)
        nlat_regional=end_index1(2)
        nsig_regional=end_index1(3)
        print *,' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
        allocate(field2(nlon_regional,nlat_regional),field3(nlon_regional,nlat_regional,nsig_regional))
        allocate(ifield2(nlon_regional,nlat_regional))
        allocate(field1(max(nlon_regional,nlat_regional,nsig_regional)))
        rmse_var='SMC'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1

        rmse_var='DLMD'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             dlmd_regional,WRF_REAL,0,0,0,ordering,          &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' dlmd=',dlmd_regional

        rmse_var='DPHD'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             dphd_regional,WRF_REAL,0,0,0,ordering,          &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' dphd=',dphd_regional

        rmse_var='PT'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             pt_regional,WRF_REAL,0,0,0,ordering,          &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' pt=',pt_regional
        !??????????????????????????????????????????????????
        !????????something wrong with pdtop--or my mistake?
        !??????????????????????????????????????????????????
        rmse_var='PDTOP'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        !    end_index1=start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             pdtop_regional,WRF_REAL,0,0,0,ordering,       &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' pdtop=',pdtop_regional

        write(iunit) iyear,imonth,iday,ihour,iminute,isecond, &
             nlon_regional,nlat_regional,nsig_regional, &
             dlmd_regional,dphd_regional,pt_regional,pdtop_regional
        !?????????????????need to infer tlm0d, tph0d from glat,glon, then recompute glat,glon
        !?????????????????????? in 3dvar as check to see we get same, but don't actually save glat,glon
        !Q??????????????????????? on output file
        rmse_var='DETA1'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field1,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,                    &
             start_index,end_index1,                   & !dom
             start_index,end_index1,                   & !mem
             start_index,end_index1,                   & !pat
             ierr                                 )
        do k=1,nsig_regional
           print *,' k,deta1(k)=',k,field1(k)
        end do
        write(iunit)field1(1:nsig_regional)  ! DETA1

        rmse_var='AETA1'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field1,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,                    &
             start_index,end_index1,                   & !dom
             start_index,end_index1,                   & !mem
             start_index,end_index1,                   & !pat
             ierr                                 )
        do k=1,nsig_regional
           print *,' k,aeta1(k)=',k,field1(k)
        end do
        write(iunit)field1(1:nsig_regional)  ! AETA1

        rmse_var='ETA1'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field1,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,                    &
             start_index,end_index1,                   & !dom
             start_index,end_index1,                   & !mem
             start_index,end_index1,                   & !pat
             ierr                                 )
        do k=1,nsig_regional+1
           print *,' k, eta1(k)=',k,field1(k)
        end do
        write(iunit)field1(1:nsig_regional+1)  !  ETA1

        rmse_var='DETA2'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field1,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,                    &
             start_index,end_index1,                   & !dom
             start_index,end_index1,                   & !mem
             start_index,end_index1,                   & !pat
             ierr                                 )
        do k=1,nsig_regional
           print *,' k,deta2(k)=',k,field1(k)
        end do
        write(iunit)field1(1:nsig_regional)  ! DETA2

        rmse_var='AETA2'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field1,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,                    &
             start_index,end_index1,                   & !dom
             start_index,end_index1,                   & !mem
             start_index,end_index1,                   & !pat
             ierr                                 )
        do k=1,nsig_regional
           print *,' k,aeta2(k)=',k,field1(k)
        end do
        write(iunit)field1(1:nsig_regional)  ! AETA2

        rmse_var='ETA2'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field1,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,                    &
             start_index,end_index1,                   & !dom
             start_index,end_index1,                   & !mem
             start_index,end_index1,                   & !pat
             ierr                                 )
        do k=1,nsig_regional+1
           print *,' k,eta2(k)=',k,field1(k)
        end do
        write(iunit)field1(1:nsig_regional+1)  ! ETA2

        rmse_var='GLAT'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        rad2deg=45./atan(1.)
        print *,' max,min GLAT=',rad2deg*maxval(field2),rad2deg*minval(field2)
        print *,' my guess at tph0d = ',rad2deg*field2(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2)
        write(iunit)field2   !GLAT 

        rmse_var='GLON'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min GLON=',rad2deg*maxval(field2),rad2deg*minval(field2)
        print *,' my guess at tlm0d = ',rad2deg*field2(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2)
        write(iunit)field2   !GLON 

        rmse_var='PD'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min pd=',maxval(field2),minval(field2)
        write(iunit)field2   !PD 

        rmse_var='FIS'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min FIS=',maxval(field2),minval(field2)
        write(iunit)field2   ! FIS

        rmse_var='T'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field3,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        do k=1,nsig_regional
           print *,' k,max,min,mid T=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                field3(nlon_regional/2,nlat_regional/2,k)
           write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! T
        end do

        rmse_var='Q'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field3,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        do k=1,nsig_regional
           print *,' k,max,min,mid Q=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                field3(nlon_regional/2,nlat_regional/2,k)
           write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Q
        end do

        rmse_var='U'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field3,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        do k=1,nsig_regional
           print *,' k,max,min,mid U=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                field3(nlon_regional/2,nlat_regional/2,k)
           write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! U
        end do

        rmse_var='V'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field3,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        do k=1,nsig_regional
           print *,' k,max,min,mid V=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                field3(nlon_regional/2,nlat_regional/2,k)
           write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! V
        end do

        rmse_var='SM'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min sm=',maxval(field2),minval(field2)
        write(iunit)field2   !SM 

        rmse_var='SICE'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min SICE=',maxval(field2),minval(field2)
        write(iunit)field2   !SICE 

        rmse_var='SST'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min SST=',maxval(field2),minval(field2)
        write(iunit)field2   !SST 

        rmse_var='IVGTYP'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             ifield2,WrfType,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min IVGTYP=',maxval(ifield2),minval(ifield2)
        write(iunit)ifield2   !IVGTYP 

        rmse_var='ISLTYP'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             ifield2,WrfType,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min ISLTYP=',maxval(ifield2),minval(ifield2)
        write(iunit)ifield2   !ISLTYP 

        rmse_var='VEGFRC'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min VEGFRC=',maxval(field2),minval(field2)
        write(iunit)field2   !VEGFRC 

        rmse_var='SNO'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min SNO=',maxval(field2),minval(field2)
        write(iunit)field2   !SNO 

        rmse_var='U10'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min U10=',maxval(field2),minval(field2)
        write(iunit)field2   !U10 

        rmse_var='V10'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min V10=',maxval(field2),minval(field2)
        write(iunit)field2   !V10 

        rmse_var='SMC'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field3,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        k=1
        print *,' k,max,min,mid SMC=',k,maxval(field3(:,:,1)),minval(field3(:,:,1)), &
             field3(nlon_regional/2,nlat_regional/2,1)
        write(iunit)((field3(i,j,1),i=1,nlon_regional),j=1,nlat_regional)   ! SMC

        rmse_var='STC'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field3,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        k=1
        print *,' k,max,min,mid STC=',k,maxval(field3(:,:,1)),minval(field3(:,:,1)), &
             field3(nlon_regional/2,nlat_regional/2,1)
        write(iunit)((field3(i,j,1),i=1,nlon_regional),j=1,nlat_regional)   ! STC

        rmse_var='TSK'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min TSK=',maxval(field2),minval(field2)
        write(iunit)field2   !TSK 

        deallocate(field1,field2,ifield2,field3)
        stop
     endif
     if(trim(arg3).eq.'storm_relocate') then

        print*,'storm_relocate'
        call getarg(2,flnm1)
        call getarg(3,flnm2)
        call getarg(4,flnm3)
        call ext_ncd_open_for_read( trim(flnm1), 0, 0, "", dh1, Status)
        if ( Status /= 0 )then
           print*,'error opening',flnm1, ' Status = ', Status
           print*
           print*,'USAGE: diffwrf [opcode] [file1] [file2]'
           print*
           stop
        endif


        iunit=12357
        open(iunit,file=flnm2,form='unformatted')

        !-------------  get date info

        call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
        read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
        print *,' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond

        !-------------  get grid info
        rmse_var='T'
        call ext_ncd_get_var_info (dh1,rmse_var,ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        nlon_regional=end_index1(1)
        nlat_regional=end_index1(2)
        nsig_regional=end_index1(3)
        print *,' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
        allocate(field2(nlon_regional,nlat_regional))
        allocate(field3(nlon_regional,nlat_regional,nsig_regional))
        allocate(ifield2(nlon_regional,nlat_regional))
        allocate(field1(max(nlon_regional,nlat_regional,nsig_regional)))

        allocate(GLON(nlon_regional,nlat_regional),GLAT(nlon_regional,nlat_regional))
        allocate(HLON(nlon_regional,nlat_regional),HLAT(nlon_regional,nlat_regional))
        allocate(VLON(nlon_regional,nlat_regional),VLAT(nlon_regional,nlat_regional))
        allocate(T(nlon_regional,nlat_regional,nsig_regional))
        allocate(Q(nlon_regional,nlat_regional,nsig_regional))
        allocate(U(nlon_regional,nlat_regional,nsig_regional))
        allocate(V(nlon_regional,nlat_regional,nsig_regional))
        allocate(TKE(nlon_regional,nlat_regional,nsig_regional))
        allocate(CWM(nlon_regional,nlat_regional,nsig_regional))
        allocate(PSFC(nlon_regional,nlat_regional))
        allocate(ZINT(nlon_regional,nlat_regional,nsig_regional+1))
        allocate(SLP(nlon_regional,nlat_regional))
        allocate(A101(nlon_regional,nlat_regional))
        allocate(B101(nlon_regional,nlat_regional))
        allocate(C101(nlon_regional,nlat_regional))
        allocate(PINT(nlon_regional,nlat_regional,nsig_regional+1))
        allocate(FI(nlon_regional,nlat_regional,2))
        allocate(PD(nlon_regional,nlat_regional))
        allocate(ETA1(nsig_regional+1))
        allocate(ETA2(nsig_regional+1))


        rmse_var='SMC'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1

        rmse_var='DLMD'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             dlmd_regional,WRF_REAL,0,0,0,ordering,          &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' dlmd=',dlmd_regional

        rmse_var='DPHD'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             dphd_regional,WRF_REAL,0,0,0,ordering,          &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' dphd=',dphd_regional

        rmse_var='PT'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             pt_regional,WRF_REAL,0,0,0,ordering,          &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' pt=',pt_regional


        call ext_ncd_get_dom_ti_real(dh1,'CEN_LAT',clat1,1,cross,ierr) 
        print *,' CLAT1=',CLAT1
        call ext_ncd_get_dom_ti_real(dh1,'CEN_LON',clon1,1,cross,ierr) 
        print *,' CLON1=',CLON1


        !??????????????????????????????????????????????????
        !????????something wrong with pdtop--or my mistake?
        !??????????????????????????????????????????????????
        rmse_var='PDTOP'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        !    end_index1=start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             pdtop_regional,WRF_REAL,0,0,0,ordering,       &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' pdtop=',pdtop_regional

        write(iunit) iyear,imonth,iday,ihour,iminute,isecond, &
             nlon_regional,nlat_regional,nsig_regional, &
             dlmd_regional,dphd_regional,pt_regional,pdtop_regional
        !?????????????????need to infer tlm0d, tph0d from glat,glon, then recompute glat,glon
        !?????????????????????? in 3dvar as check to see we get same, but don't actually save glat,glon
        !Q??????????????????????? on output file
        rmse_var='DETA1'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field1,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,                    &
             start_index,end_index1,                   & !dom
             start_index,end_index1,                   & !mem
             start_index,end_index1,                   & !pat
             ierr                                 )
        do k=1,nsig_regional
           print *,' k,deta1(k)=',k,field1(k)
        end do
        write(iunit)field1(1:nsig_regional)  ! DETA1

        rmse_var='AETA1'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field1,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,                    &
             start_index,end_index1,                   & !dom
             start_index,end_index1,                   & !mem
             start_index,end_index1,                   & !pat
             ierr                                 )
        do k=1,nsig_regional
           print *,' k,aeta1(k)=',k,field1(k)
        end do
        write(iunit)field1(1:nsig_regional)  ! AETA1

        rmse_var='ETA1'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field1,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,                    &
             start_index,end_index1,                   & !dom
             start_index,end_index1,                   & !mem
             start_index,end_index1,                   & !pat
             ierr                                 )
        do k=1,nsig_regional+1
           print *,' k, eta1(k)=',k,field1(k)
        end do
        write(iunit)field1(1:nsig_regional+1)  !  ETA1

        DO K=1,nsig_regional+1
           ETA1(k)=field1(k)
        END DO

        rmse_var='DETA2'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field1,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,                    &
             start_index,end_index1,                   & !dom
             start_index,end_index1,                   & !mem
             start_index,end_index1,                   & !pat
             ierr                                 )
        do k=1,nsig_regional
           print *,' k,deta2(k)=',k,field1(k)
        end do
        write(iunit)field1(1:nsig_regional)  ! DETA2

        rmse_var='AETA2'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field1,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,                    &
             start_index,end_index1,                   & !dom
             start_index,end_index1,                   & !mem
             start_index,end_index1,                   & !pat
             ierr                                 )
        do k=1,nsig_regional
           print *,' k,aeta2(k)=',k,field1(k)
        end do
        write(iunit)field1(1:nsig_regional)  ! AETA2

        rmse_var='ETA2'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field1,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,                    &
             start_index,end_index1,                   & !dom
             start_index,end_index1,                   & !mem
             start_index,end_index1,                   & !pat
             ierr                                 )
        do k=1,nsig_regional+1
           print *,' k,eta2(k)=',k,field1(k)
        end do
        write(iunit)field1(1:nsig_regional+1)  ! ETA2

        do k=1,nsig_regional+1
           ETA2(k)=field1(k)
        end do

        rmse_var='GLAT'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        rad2deg=45./atan(1.)
        print *,' max,min GLAT=',rad2deg*maxval(field2),rad2deg*minval(field2)
        print *,' my guess at tph0d = ',rad2deg*field2(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2)
        write(iunit)field2   !GLAT 

        do j=1,nlat_regional
           do i=1,nlon_regional
              GLAT(i,j)=field2(i,j)
              HLAT(i,j)=field2(i,j)*rad2deg
           end do
        end do

        rmse_var='GLON'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min GLON=',rad2deg*maxval(field2),rad2deg*minval(field2)
        print *,' my guess at tlm0d = ',rad2deg*field2(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2)
        write(iunit)field2   !GLON 

        do j=1,nlat_regional
           do i=1,nlon_regional
              GLON(i,j)=field2(i,j)
              HLON(i,j)=field2(i,j)*rad2deg
           end do
        end do


        rmse_var='VLAT'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )

        do j=1,nlat_regional
           do i=1,nlon_regional
              VLAT(i,j)=field2(i,j)
           end do
        end do

        rmse_var='VLON'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        do j=1,nlat_regional
           do i=1,nlon_regional
              VLON(i,j)=field2(i,j)
           end do
        end do

        DDDX=GLON(1+(nlon_regional-1)/2,(nlat_regional-1)/2)-GLON((nlon_regional-1)/2,(nlat_regional-1)/2)
        DDDY=GLAT((nlon_regional-1)/2,1+(nlat_regional-1)/2)-GLAT((nlon_regional-1)/2,(nlat_regional-1)/2)
           allocate(DDDX1(nlon_regional-1,nlat_regional-1))
           allocate(DDDY1(nlon_regional-1,nlat_regional-1))
        DO J=1,nlat_regional-1
           DO I=1,nlon_regional-1
              DDDX1(I,J)=(GLON(I+1,J)-GLON(I,J))/DDDX
              DDDY1(I,J)=(GLAT(I,J+1)-GLAT(I,J))/DDDY
           END DO
        END DO
        open(76,file='test_data',form='formatted')
        WRITE(76,*)DDDX,DDDY
        WRITE(76,'(15F7.3)')DDDX1
        WRITE(76,*)
        WRITE(76,'(15F7.3)')DDDY1
        close(76)

        rmse_var='PD'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min pd=',maxval(field2),minval(field2)
        !   write(iunit)field2   !PD 

        do j=1,nlat_regional
           do i=1,nlon_regional
              PD(i,j)=field2(i,j)
           end do
        end do

        do j=1,nlat_regional
           do i=1,nlon_regional
              PSFC(i,j)=field2(i,j)+pt_regional+pdtop_regional
           end do
        end do

        rmse_var='FIS'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min FIS=',maxval(field2),minval(field2)
        !   write(iunit)field2   ! FIS

        do j=1,nlat_regional
           do i=1,nlon_regional
              FI(I,J,1)=field2(i,j)
              !     ZINT(i,j,nsig_regional+1)=field2(i,j)/9.8                  ! Surface hight
              ZINT(i,j,1)=field2(i,j)/9.8                  ! Surface hight
           end do
        end do

        rmse_var='T'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field3,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        !   do k=1,nsig_regional
        !             print *,' k,max,min,mid T=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
        !                         field3(nlon_regional/2,nlat_regional/2,k)
        !     write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! T
        !   end do

        do k=1,nsig_regional
           do j=1,nlat_regional
              do i=1,nlon_regional
                 T(i,j,k)=field3(i,j,k)
              end do
           end do
        end do

        WRITE(73)(((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional),k=1,nsig_regional)

        rmse_var='Q'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field3,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        !   do k=1,nsig_regional
        !             print *,' k,max,min,mid Q=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
        !                         field3(nlon_regional/2,nlat_regional/2,k)
        !    write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Q
        !   end do

        do k=1,nsig_regional
           do j=1,nlat_regional
              do i=1,nlon_regional
                 Q(i,j,k)=field3(i,j,k)
              end do
           end do
        end do

        WRITE(73)(((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional),k=1,nsig_regional)

        rmse_var='U'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field3,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        !   do k=1,nsig_regional
        !             print *,' k,max,min,mid U=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
        !                         field3(nlon_regional/2,nlat_regional/2,k)
        !    write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! U
        !   end do

        do k=1,nsig_regional
           do j=1,nlat_regional
              do i=1,nlon_regional
                 U(i,j,k)=field3(i,j,k)
              end do
           end do
           WRITE(73)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)
        end do

        do j=1,nlat_regional,2
           write(50)((field3(i,j,k),i=1,nlon_regional,2),k=1,nsig_regional)
        end do

        !    WRITE(73)(((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional),k=1,nsig_regional)

        rmse_var='V'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL 
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field3,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        !   do k=1,nsig_regional
        !             print *,' k,max,min,mid V=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
        !                         field3(nlon_regional/2,nlat_regional/2,k)
        !    write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! V
        !   end do

        do k=1,nsig_regional
           do j=1,nlat_regional
              do i=1,nlon_regional
                 V(i,j,k)=field3(i,j,k)
              end do
           end do
        end do

        do j=1,nlat_regional,2
           write(50)((field3(i,j,k),i=1,nlon_regional,2),k=1,nsig_regional)
        end do
        do j=1,nlat_regional,2
           write(50)((field3(i,j,k),i=1,nlon_regional,2),k=1,nsig_regional)
        end do
        do j=1,nlat_regional,2
           write(50)((field3(i,j,k),i=1,nlon_regional,2),k=1,nsig_regional)
        end do
        do j=1,nlat_regional,2
           write(50)((field3(i,j,k),i=1,nlon_regional,2),k=1,nsig_regional)
        end do
        WRITE(73)(((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional),k=1,nsig_regional)

        rmse_var='CWM'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field3,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        !   do k=1,nsig_regional
        !             print *,' k,max,min,mid CWM=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
        !                         field3(nlon_regional/2,nlat_regional/2,k)
        !    write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! CWM
        !   end do

        !   CWM=field3

        rmse_var='PINT'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             PINT,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        !   do k=1,nsig_regional+1
        !             print *,' k,max,min,mid PINT=',k,maxval(PINT(:,:,k)),minval(PINT(:,:,k)), &
        !                         PINT(nlon_regional/2,nlat_regional/2,k)
        !    write(iunit)((PINT(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! PINT
        !   end do

        ! Compute Geopotentital height, INTEGRATE HEIGHT HYDROSTATICLY
        !      DO L=nsig_regional,1,-1
      print*,  'in calculating ZINT; nsig_regional,1,nlat_regional, nlon_regional are',nsig_regional,1,nlat_regional,nlon_regional
      print*,  'in calculating ZINT; maxT,maxQ,maxFI,maxPINT are', maxval(T),maxval(Q),maxval(PINT),maxval(FI)
      print*,  'in calculating ZINT; minT,minQ,minFI,minPINT are',minval(T),minval(Q),minval(PINT),minval(FI)
      print*,  'in calculating ZINT; shapeT,shapeQ,shapeFI,shapePINT are',shape(T),shape(Q),shape(PINT),shape(FI)

        DO L=2,nsig_regional
           do j = 1,nlat_regional
              do i = 1, nlon_regional
               if(i.eq.10.and.j.eq.10.and.l.eq.2)then
                  print*,T(i,j,l-1),q(i,j,l-1),pint(i,j,l-1),pint(i,j,l),fi(i,j,1)
               endif

                 FI(I,J,2)=T(I,J,L-1)*(Q(I,J,L-1)*0.608+1.0)*287.04*           &
                      (ALOG(PINT(I,J,L-1))-ALOG(PINT(I,J,L)))+FI(I,J,1)
                 ZINT(I,J,L)=FI(I,J,2)/9.8
                 FI(I,J,1)=FI(I,J,2)
              ENDDO
           ENDDO
           PRINT *,'ZINT  at 10 10 ',l,zint(10,10,l),T(10,10,l-1),q(10,10,l-1),pint(10,10,l-1),pint(10,10,l)
        END DO
      print*,  'in calculating ZINT; nsig_regional,1,nlat_regional, nlon_regional are',nsig_regional,1,nlat_regional,nlon_regional
      print*,  'in calculating ZINT; maxT,maxQ,maxFI,maxPINT are', maxval(T),maxval(Q),maxval(ZINT),maxval(FI)
      print*,  'in calculating ZINT; minT,minQ,minFI,minPINT are',minval(T),minval(Q),minval(ZINT),minval(FI)
      print*,  'in calculating ZINT; shapeT,shapeQ,shapeFI,shapePINT are',shape(T),shape(Q),shape(ZINT),shape(FI)

        print*,'finish deriving geopotential in nmm'
        !

        rmse_var='TKE_MYJ'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field3,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        !   do k=1,nsig_regional
        !             print *,' k,max,min,mid TKE=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
        !                         field3(nlon_regional/2,nlat_regional/2,k)
        !    write(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! TKE
        !   end do

        !   TKE=field3

        !   call storm_relocate(nlon_regional,nlat_regional,nsig_regional,        &
        !                       T,Q,U,V,TKE,CWM,ZINT,GLON,GLAT,PINT,       &
        !                       pt_regional,pdtop_regional,PD,ETA1,ETA2)

        print*,'open77'

        open(77,file=flnm3,form='unformatted')
        WRITE(77)nlon_regional,nlat_regional,nsig_regional
        open(78,file='kknetcdfoutput',form='formatted')
        write(78,*) nlon_regional, nlat_regional, nsig_regional
        WRITE(77)dlmd_regional,dphd_regional,CLON1,CLAT1
        WRITE(78,*)dlmd_regional,dphd_regional,CLON1,CLAT1
        WRITE(77)pt_regional,pdtop_regional
        WRITE(78,*)pt_regional,pdtop_regional
        WRITE(77)T
        write(78,*) maxval(T),minval(T),shape(T)
        WRITE(77)Q
        write(78,*) maxval(Q),minval(Q),shape(Q)
        WRITE(77)U
        write(78,*) maxval(U),minval(U),shape(U)
        WRITE(77)V
        write(78,*) maxval(V),minval(V),shape(V)
        !    WRITE(77)TKE
        !    WRITE(77)CWM
        WRITE(77)ZINT
        write(78,*) maxval(ZINT),minval(ZINT),shape(ZINT)
        !    WRITE(77)GLON,GLAT
        WRITE(77)HLON,HLAT,VLON,VLAT
        WRITE(78,*) maxval(VLON),minval(VLON),maxval(VLAT),minval(VLAT),shape(VLAT),shape(VLON)
        WRITE(77)PINT
        write(78,*)maxval(PINT),minval(PINT),shape(PINT)
        WRITE(77)PD
        write(78,*)maxval(PD),minval(PD),shape(PD)
        WRITE(77)ETA1
        write(78,*)maxval(ETA1),minval(ETA1),shape(ETA1)
        WRITE(77)ETA2
        write(78,*)maxval(ETA2),minval(ETA2),shape(ETA2)


        WRITE(73)(((ZINT(i,j,k),i=1,nlon_regional),j=1,nlat_regional),k=1,nsig_regional)
        WRITE(73)(((PINT(i,j,k),i=1,nlon_regional),j=1,nlat_regional),k=1,nsig_regional)
        WRITE(73)((PD(i,j),i=1,nlon_regional),j=1,nlat_regional)


        do j=10,15
           do i=21,25
              print*,'PSFC(i,j)=',PSFC(i,j),PINT(i,j,1),       &
                   pt_regional+pdtop_regional*ETA1(1)+PD(I,J)*ETA2(1)
           end do
        end do

        ! saving PD and FIS 

        !   do j=1,nlat_regional
        !   do i=1,nlon_regional
        !     field2(i,j)=PSFC(i,j)-pt_regional-pdtop_regional          ! (changed)
        !     field2(i,j)=PINT(i,j,1)                      ! (changed)  ????
        !   end do
        !   end do
        field2=PD
        write(iunit)field2   !PD

        do j=1,nlat_regional
           do i=1,nlon_regional
              field2(i,j)=ZINT(i,j,1)*9.81            ! FIS field (unchanged)
           end do
        end do
        write(iunit)field2   ! FIS


        ! Saving the modified 3D fields

        do k=1,nsig_regional
           print *,' k,max,min,mid T=',k,maxval(T(:,:,k)),minval(T(:,:,k)), &
                T(nlon_regional/2,nlat_regional/2,k)
           write(iunit)((T(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! T
        end do

        do k=1,nsig_regional
           print *,' k,max,min,mid Q=',k,maxval(Q(:,:,k)),minval(Q(:,:,k)), &
                Q(nlon_regional/2,nlat_regional/2,k)
           write(iunit)((Q(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Q 
        end do

        do k=1,nsig_regional
           print *,' k,max,min,mid U=',k,maxval(U(:,:,k)),minval(U(:,:,k)), &
                U(nlon_regional/2,nlat_regional/2,k)
           write(iunit)((U(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! U 
        end do

        do k=1,nsig_regional
           print *,' k,max,min,mid V=',k,maxval(V(:,:,k)),minval(V(:,:,k)), &
                V(nlon_regional/2,nlat_regional/2,k)
           write(iunit)((V(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! V
        end do

        do k=1,nsig_regional
           print *,' k,max,min,mid CWM=',k,maxval(CWM(:,:,k)),minval(CWM(:,:,k)), &
                CWM(nlon_regional/2,nlat_regional/2,k)
           !     write(iunit)((CWM(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! CWM
        end do

        do k=1,nsig_regional+1
           print *,' k,max,min,mid PINT=',k,maxval(PINT(:,:,k)),minval(PINT(:,:,k)), &
                PINT(nlon_regional/2,nlat_regional/2,k)
           !     write(iunit)((PINT(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! PINT (unchanged)
        end do

        do k=1,nsig_regional
           print *,' k,max,min,mid TKE=',k,maxval(TKE(:,:,k)),minval(TKE(:,:,k)), &
                TKE(nlon_regional/2,nlat_regional/2,k)
           !     write(iunit)((TKE(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! TKE
        end do

        ! Complete saving modified 3D fields

        rmse_var='SM'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min sm=',maxval(field2),minval(field2)
        write(iunit)field2   !SM 

        DO J=1,nlat_regional
           DO I=1,nlon_regional
              A101(I,J)=field2(I,J)
           END DO
        END DO

        WRITE(77)A101  

        !   write(52,'(10E12.4)')A101

        rmse_var='SICE'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min SICE=',maxval(field2),minval(field2)
        write(iunit)field2   !SICE 

        rmse_var='SST'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min SST=',maxval(field2),minval(field2)
        write(iunit)field2   !SST 

        rmse_var='IVGTYP'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             ifield2,WrfType,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min IVGTYP=',maxval(ifield2),minval(ifield2)
        write(iunit)ifield2   !IVGTYP 

        rmse_var='ISLTYP'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             ifield2,WrfType,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min ISLTYP=',maxval(ifield2),minval(ifield2)
        write(iunit)ifield2   !ISLTYP 

        rmse_var='VEGFRC'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min VEGFRC=',maxval(field2),minval(field2)
        write(iunit)field2   !VEGFRC 

        rmse_var='SNO'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min SNO=',maxval(field2),minval(field2)
        write(iunit)field2   !SNO 

        rmse_var='U10'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min U10=',maxval(field2),minval(field2)
        write(iunit)field2   !U10 

        DO J=1,nlat_regional
           DO I=1,nlon_regional
              A101(I,J)=field2(I,J)
           END DO
        END DO

        rmse_var='V10'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min V10=',maxval(field2),minval(field2)
        write(iunit)field2   !V10 

        DO J=1,nlat_regional
           DO I=1,nlon_regional
              C101(I,J)=sqrt(field2(I,J)**2+A101(I,J)**2)
           END DO
        END DO

        rmse_var='ZNT'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min ZNT=',maxval(field2),minval(field2)
        !   write(iunit)field2   !ZNT

        DO J=1,nlat_regional
           DO I=1,nlon_regional
              B101(I,J)=field2(I,J)
           END DO
        END DO


        WRITE(77)B101
        WRITE(77)C101

        close(77)
        close(78)

        rmse_var='SMC'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field3,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        k=1
        print *,' k,max,min,mid SMC=',k,maxval(field3(:,:,1)),minval(field3(:,:,1)), &
             field3(nlon_regional/2,nlat_regional/2,1)
        write(iunit)((field3(i,j,1),i=1,nlon_regional),j=1,nlat_regional)   ! SMC

        rmse_var='STC'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field3,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        k=1
        print *,' k,max,min,mid STC=',k,maxval(field3(:,:,1)),minval(field3(:,:,1)), &
             field3(nlon_regional/2,nlat_regional/2,1)
        write(iunit)((field3(i,j,1),i=1,nlon_regional),j=1,nlat_regional)   ! STC

        rmse_var='TSK'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        print *,' max,min TSK=',maxval(field2),minval(field2)
        write(iunit)field2   !TSK 

        deallocate(field1,field2,ifield2,field3)
        deallocate(T,Q,U,V,PSFC,ZINT,TKE,CWM,SLP,PINT,PD,ETA1,ETA2)
        stop
     endif
     if(trim(arg3).eq.'3dvar_update') then
        !
        !           update nmm netcdf file with analysis variables from 3dvar
        !
        call getarg(2,flnm1)
        call getarg(3,flnm2)
        call ext_ncd_open_for_update( trim(flnm1), 0, 0, "", dh1, Status)
        if ( Status /= 0 )then
           print*,'error opening',flnm1, ' Status = ', Status
           print*
           print*,'USAGE: diffwrf [opcode] [file1] [file2]'
           print*
           stop
        endif


        iunit=12357
        open(iunit,file=flnm2,form='unformatted')

        !-------------  get date info

        call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
        read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
        print *,' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond

        !-------------  get grid info
        rmse_var='T'
        call ext_ncd_get_var_info (dh1,rmse_var,ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        nlon_regional=end_index1(1)
        nlat_regional=end_index1(2)
        nsig_regional=end_index1(3)
        print *,' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
        allocate(field2(nlon_regional,nlat_regional),field3(nlon_regional,nlat_regional,nsig_regional))
        allocate(ifield2(nlon_regional,nlat_regional))
        allocate(field1(max(nlon_regional,nlat_regional,nsig_regional)))
        allocate(field31(nlon_regional,nlat_regional,nsig_regional+1))

        read(iunit) ! IX,IY,NZ
        read(iunit) ! DLMD3,DPHD3,CLON3,CLAT3
        read(iunit) ! PT3,PDTOP3

!!$        do k=1,nsig_regional
!!$           read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! T
!!$           print *,' k,max,min,mid T=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
!!$                field3(nlon_regional/2,nlat_regional/2,k)
!!$        end do
        read(iunit) field3
        rmse_var='T'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
             field3,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )

        !_____________________________________________________________________________________
 
!!$        do k=1,nsig_regional
!!$           read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Q
!!$           print *,' k,max,min,mid Q=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
!!$                field3(nlon_regional/2,nlat_regional/2,k)
!!$        end do
        read(iunit) field3
        rmse_var='Q'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
             field3,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        !_____________________________________________________________________________________

!!$        do k=1,nsig_regional
!!$           read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! U
!!$           print *,' k,max,min,mid U=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
!!$                field3(nlon_regional/2,nlat_regional/2,k)
!!$        end do
        read(iunit) field3
        rmse_var='U'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
             field3,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        !_____________________________________________________________________________________

!!$        do k=1,nsig_regional
!!$           read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! V
!!$           print *,' k,max,min,mid V=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
!!$                field3(nlon_regional/2,nlat_regional/2,k)
!!$        end do
        read(iunit) field3
        rmse_var='V'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
             field3,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        !_____________________________________________________________________________________

        read(iunit) ! Z

        read(iunit) ! HLON2,HLAT2

        read(iunit)   field31 ! PINT
        rmse_var='PINT'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var), &
             field31,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )
        !_____________________________________________________________________________________


        read(iunit)   field2   !PD 
        print *,' max,min pd=',maxval(field2),minval(field2)
        rmse_var='PD'
        call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
             start_index,end_index1, WrfType, ierr    )
        print *,' rmse_var=',trim(rmse_var)
        print *,' ordering=',ordering
        print *,' WrfType,WRF_REAL=',WrfType,WRF_REAL
        print *,' ndim1=',ndim1
        print *,' staggering=',staggering
        print *,' start_index=',start_index
        print *,' end_index1=',end_index1
        call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
             field2,WRF_REAL,0,0,0,ordering,           &
             staggering, dimnames ,               &
             start_index,end_index1,               & !dom
             start_index,end_index1,               & !mem
             start_index,end_index1,               & !pat
             ierr                                 )

        read(iunit) ! ETA1
        read(iunit) ! ETA2 

        close(iunit)

        deallocate(field1,field2,ifield2,field3,field31)
        call ext_ncd_ioclose(dh1, Status)
        stop
     endif
  endif
end program readv3

!============================================================================

!!$logical function iveceq( a, b, n )
!!$  implicit none
!!$  integer n
!!$  integer a(n), b(n)
!!$  integer i
!!$  iveceq = .true.
!!$  do i = 1,n
!!$    if ( a(i) .ne. b(i) ) iveceq = .false.
!!$  enddo
!!$  return
!!$end function iveceq

module module_wrf_error_dummy
  INTEGER           :: wrf_debug_level = 0
  CHARACTER*256     :: wrf_err_message

  LOGICAL :: silence=.false.  ! T = this process should not log.
  LOGICAL :: buffered=.false. ! T = messages sent via clog_write
  LOGICAL :: stderrlog=.false.! T = send to write(0,...) if buffered=F

  INTEGER, PARAMETER :: wrf_log_flush=0, wrf_log_set_buffer_size=1, &
                        wrf_log_write=2

  !NOTE: Make sure silence, buffered and stderrlog settings match the
  ! namelist defaults in init_module_wrf_error.

! min_allowed_buffer_size: requested buffer sizes smaller than this
! will simply result in disabling of log file buffering.  This number
! should be larger than any line WRF prints frequently.  If you set it 
! too small, the buffering code will still work.  However, any line 
! that is larger than the buffer will result in two writes: one for 
! the line and one for the end-of-line character at the end.
  integer, parameter :: min_allowed_buffer_size=200
end module module_wrf_error_dummy

! stub for routine called by module_wrf_error (used by netcdf implementation of IO api)
SUBROUTINE wrf_abort
  STOP
END SUBROUTINE wrf_abort
SUBROUTINE get_current_time_string( time_str )
  CHARACTER(LEN=*), INTENT(OUT) :: time_str
  time_str = ''
END SUBROUTINE get_current_time_string

SUBROUTINE get_current_grid_name( grid_str )
  CHARACTER(LEN=*), INTENT(OUT) :: grid_str
  grid_str = ''
END SUBROUTINE get_current_grid_name

SUBROUTINE wrf_error_fatal(s)
  implicit none
  character(*) :: s
  write(0,*) s
  write(6,*) s
  call wrf_abort()
END SUBROUTINE wrf_error_fatal

SUBROUTINE wrf_message(s)
  implicit none
  character(*) :: s
  write(6,*) s
END SUBROUTINE wrf_message

SUBROUTINE wrf_debug(i,s)
  use module_wrf_error_dummy
  implicit none
  integer :: i
  character(*) :: s
  if(i<=wrf_debug_level) write(6,*) s
END SUBROUTINE wrf_debug

SUBROUTINE set_wrf_debug_level(i)
  use module_wrf_error_dummy
  implicit none
  integer i
  wrf_debug_level=i
END SUBROUTINE set_wrf_debug_level
