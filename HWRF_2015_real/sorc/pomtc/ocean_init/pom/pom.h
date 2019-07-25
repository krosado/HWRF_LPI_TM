! pom.h

! contain parameters for the model domain and for the decomposition
! local domain size, and common POM variables

!_______________________________________________________________________
! Grid size parameters
      integer
     $  im_global      ,! number of global grid points in x
     $  jm_global      ,! number of global grid points in y
     $  kb             ,! number of grid points in z
     $  im_local       ,! number of local grid points in x
     $  jm_local       ,! number of local grid points in y
     $  n_proc         ,! number of processors
     $  flagged         ! flagged value !RMY: added flagged

! Correct values for im_local and jm_local are found using
!   n_proc=(im_global-2)/(im_local-2)*(jm_global-2)/(jm_local-2)
! Values higher than necessary will not cause the code to fail, but
! will allocate more memory than is necessary. Value that are too low
! will cause the code to exit

      parameter(
     $  im_global=869  ,
     $  jm_global=449  ,
     $  kb=23          ,
     $  im_local=291   ,
     $  jm_local=151   ,
     $  n_proc=9       ,
     $  flagged=-999   ) !RMY: added flagged

!_______________________________________________________________________
! Efective grid size
      integer
     $  im             ,! number of grid points used in local x domains
     $  imm1           ,! im-1
     $  imm2           ,! im-2
     $  jm             ,! number of grid points used in local y domains
     $  jmm1           ,! jm-1
     $  jmm2           ,! jm-2
     $  kbm1           ,! kb-1
     $  kbm2            ! kb-2

! Note that im and jm may be different between local domains
! im and jm are equal to or lower than im_local and jm_local, depending
! on the use of correct values for im_local and jm_local

      common/blksiz/
     $  im             ,
     $  imm1           ,
     $  imm2           ,
     $  jm             ,
     $  jmm1           ,
     $  jmm2           ,
     $  kbm1           ,
     $  kbm2

!_______________________________________________________________________
! Parallel variables
      integer
     $  my_task        ,! actual parallel processor ID
     $  master_task    ,! master processor ID
     $  pom_comm       ,! POM model MPI group communicator
     $  i_global       ,! global i index for each point in local domain
     $  j_global       ,! global j index for each point in local domain
     $  n_west         ,! western parallel processor ID
     $  n_east         ,! eastern parallel processor ID
     $  n_south        ,! southern parallel processor ID
     $  n_north         ! northern parallel processor ID

      common/blkpar/
     $  my_task        ,
     $  master_task    ,
     $  pom_comm       ,
     $  i_global(im_local),
     $  j_global(jm_local),
     $  n_west         ,
     $  n_east         ,
     $  n_south        ,
     $  n_north

!_______________________________________________________________________
! Scalars
      integer
     $  iint           ,
     $  iprint         ,! interval in iint at which variables are printed
     $  iprint2        ,! like iprint but sfc variables only !RMY: added iprint2
     $  mode           ,! calculation mode
     $  ntp            ,! water type
     $  iend           ,! total internal mode time steps
     $  iext           ,
     $  ifplane        ,! f-plane flag !RMY: added ifplane
     $  igeovel        ,! call geovel flag !RMY: added igeovel
     $  ionedim        ,! 1D vertical physics only flag !RMY added ionedim
     $  ipwave         ,! wave-induced turbulence flag !RMY added ipwave
     $  ismoth         ,! smoothing/desmoothing flag !RMY: added ismoth
     $  ispadv         ,! step interval for updating external advective terms
     $  isplit         ,! dti/dte
     $  iswtch         ,! time step interval to switch from prtd1 to prtd2
     $  nadv           ,! advection scheme
     $  nbct           ,! surface temperature boundary condition
     $  nbcs           ,! surface salinity boundary condition
     $  nitera         ,! number of iterations for Smolarkiewicz scheme
     $  nl             ,! number of z-levels in initial file !RMY: added nl
     $  npg            ,! pressure gradient scheme
     $  nprinto        ,! index to indicate output filename !RMY: added nprinto
     $  nprinto2       ,! index to indicate sfcout filename !RMY: added nprinto2
     $  nread_rst      ,! index to start from restart file
     $  irestart       ,
     $  error_status

      real
     $  alpha          ,! weight for surface slope term in external eq
     $  cnorth_e       ,! constant latitude for f-plane runs !RMY: added cnorth_e
     $  deg2rad        ,! converts degrees to radians !RMY: added deg2rad
     $  dte            ,! external (2-D) time step (s)
     $  dti            ,! internal (3-D) time step (s)
     $  dti2           ,! 2*dti
     $  grav           ,! gravity constant (S.I. units)
     $  hmax           ,! maximum ocean depth !RMY: added hmax
     $  kappa          ,! von Karman's constant
     $  pi             ,! pi
     $  ramp           ,! inertial ramp
     $  rfe            ,! flag for eastern open boundary (see bcond)
     $  rfn            ,! flag for northern open boundary (see bcond)
     $  rfs            ,! flag for southern open boundary (see bcond)
     $  rfw            ,! flag for western open boundary (see bcond)
     $  rhoref         ,! reference density
     $  rho_0          ,! density used for wusurf and wvsurf !RMY: added rho_0
     $  rearth         ,! radius of the earth !RMY: added rearth
     $  sbias          ,! salinity bias
     $  slmax          ,
     $  small          ,! small value
     $  smh            ,! time step frequency for smoothing/desmoothing !RMY: added smh
     $  tbias          ,! temperature bias
     $  time           ,! model time (days)
     $  tnowindd       ,! no prescribed wind after tnowindd (days) !RMY: added tnowindd
     $  tprni          ,! inverse horizontal turbulent Prandtl number
     $  umol           ,! background viscosity
     $  vmaxl          ,! max vaf used to test for model blow-up
     $  write_rst      ,
     $  aam_init       ,! initial value of aam
     $  cbcmax         ,! maximum bottom friction coefficient
     $  cbcmin         ,! minimum bottom friction coefficient
     $  days           ,! run duration in days
     $  dte2           ,! 2*dte
     $  horcon         ,! smagorinsky diffusivity coefficient
     $  ispi           ,! dte/dti
     $  isp2i          ,! dte/(2*dti)
     $  period         ,! inertial period
     $  prtd1          ,! initial print interval (days)
     $  prtd2          ,! final print interval (days)
     $  smoth          ,! constant to prevent solution splitting
     $  sw             ,! smoothing parameter for Smolarkiewicz scheme
     $  swtch          ,! time to switch from prtd1 to prtd2
     $  time0          ,! initial time (days)
     $  z0b             ! bottom roughness

      common/blkcon/
     $  alpha          ,
     $  cnorth_e       , !RMY: added cnorth_e
     $  deg2rad        , !RMY: added deg2rad
     $  dte            ,
     $  dti            ,
     $  dti2           ,
     $  grav           ,
     $  hmax           , !RMY: added hmax
     $  kappa          ,
     $  pi             ,
     $  ramp           ,
     $  rfe            ,
     $  rfn            ,
     $  rfs            ,
     $  rfw            ,
     $  rhoref         ,
     $  rho_0          , !RMY: added rho_0
     $  rearth         , !RMY: added rearth
     $  sbias          ,
     $  slmax          ,
     $  small          ,
     $  smh            , !RMY: added smh
     $  tbias          ,
     $  time           ,
     $  tnowindd       , !RMY: added tnowindd
     $  tprni          ,
     $  umol           ,
     $  vmaxl          ,
     $  write_rst      ,
     $  iint           ,
     $  iprint         ,
     $  iprint2        , !RMY: added iprint2
     $  mode           ,
     $  ntp            ,
     $  aam_init       ,
     $  cbcmax         ,
     $  cbcmin         ,
     $  days           ,
     $  dte2           ,
     $  horcon         ,
     $  ispi           ,
     $  isp2i          ,
     $  period         ,
     $  prtd1          ,
     $  prtd2          ,
     $  smoth          ,
     $  sw             ,
     $  swtch          ,
     $  time0          ,
     $  z0b            ,
     $  iend           ,
     $  iext           ,
     $  ifplane        , !RMY: added ifplane
     $  igeovel        , !RMY: added igeovel
     $  ionedim        , !RMY: added ionedim
     $  ipwave         , !RMY: added ipwave
     $  ismoth         , !RMY: added ismoth
     $  ispadv         ,
     $  isplit         ,
     $  iswtch         ,
     $  nadv           ,
     $  nbct           ,
     $  nbcs           ,
     $  nitera         ,
     $  nl             , !RMY: added nl
     $  npg            ,
     $  nprinto        , !RMY: added nprinto
     $  nprinto2       , !RMY: added nprinto2
     $  nread_rst      ,
     $  irestart       ,
     $  error_status

!_______________________________________________________________________
! 1-D arrays
      real
     $  dz             ,! z(k)-z(k+1)
     $  dzz            ,! zz(k)-zz(k+1)
     $  z              ,! sigma coordinate from z=0 (surface) to z=-1 (bottom)
     $  zz              ! sigma coordinate, intermediate between z

      common/blk1d/
     $  dz(kb)         ,
     $  dzz(kb)        ,
     $  z(kb)          ,
     $  zz(kb)

!_______________________________________________________________________
! 2-D arrays
      integer
     $  irflg           ! storm radius flag used when nbc=5 !RMY: added irflg

      real
     $  aam2d          ,! vertical average of aam
     $  advua          ,! sum of the 2nd, 3rd and 4th terms in eq (18)
     $  advva          ,! sum of the 2nd, 3rd and 4th terms in eq (19)
     $  adx2d          ,! vertical integral of advx
     $  ady2d          ,! vertical integral of advy
     $  art            ,! cell area centered on T grid points
     $  aru            ,! cell area centered on U grid points
     $  arv            ,! cell area centered on V grid points
     $  cbc            ,! bottom friction coefficient
     $  cor            ,! coriolis parameter
     $  d              ,! h+el
     $  drx2d          ,! vertical integral of drhox
     $  dry2d          ,! vertical integral of drhoy
     $  dt             ,! h+et
     $  dum            ,! mask for u velocity
     $  dvm            ,! mask for v velocity
     $  dx             ,! grid spacing in x
     $  dy             ,! grid spacing in y
     $  east_c         ,! horizontal coordinate of cell corner points in x
     $  east_e         ,! horizontal coordinate of elevation points in x
     $  east_u         ,! horizontal coordinate of U points in x
     $  east_v         ,! horizontal coordinate of V points in x
     $  e_atmos        ,! atmospheric pressure
     $  egb            ,! surface elevation use for pressure gradient at time n-1
     $  egf            ,! surface elevation use for pressure gradient at time n+1
     $  el             ,! surface elevation used in the external mode at time n
     $  elb            ,! surface elevation used in the external mode at time n-1
     $  elf            ,! surface elevation used in the external mode at time n+1
     $  et             ,! surface elevation used in the internal mode at time n
     $  etb            ,! surface elevation used in the internal mode at time n-1
     $  etf            ,! surface elevation used in the internal mode at time n+1
     $  fluxua         ,
     $  fluxva         ,
     $  fsm            ,! mask for scalar variables
     $  h              ,! bottom depth
     $  mdp            ,! mean depth for waves !RMY: added mdp
     $  north_c        ,! horizontal coordinate of cell corner points in y
     $  north_e        ,! horizontal coordinate of elevation points in y
     $  north_u        ,! horizontal coordinate of U points in y
     $  north_v        ,! horizontal coordinate of V points in y
     $  psi            ,
     $  rot            ,! rotation angle
     $  ssurf          ,
     $  swrad          ,! short wave radiation incident on the ocean surface
     $  swradb         ,! short wave radiation incident on the ocean surface at time n-1
     $  swradf         ,! short wave radiation incident on the ocean surface at time n+1
     $  taux           ,! x-momentum flux at the surface (wind stress) on depth grid !RMY: added taux
     $  tauxi          ,! x-momentum flux at the surface (wind stress) interp to depth grid !RMY: added tauxi
     $  tauy           ,! y-momentum flux at the surface (wind stress) on depth grid !RMY: added tauy
     $  tauyi          ,! y-momentum flux at the surface (wind stress) interp to depth grid !RMY: added tauyi
     $  tps            ,
     $  tsurf          ,
     $  ua             ,! vertical mean of u at time n
     $  uab            ,! vertical mean of u at time n-1
     $  uaf            ,! vertical mean of u at time n+1
     $  utb            ,! ua time averaged over the interval dti at time n-1
     $  utf            ,! ua time averaged over the interval dti at time n+1
     $  va             ,! vertical mean of v at time n
     $  vab            ,! vertical mean of v at time n-1
     $  vaf            ,! vertical mean of v at time n+1
     $  vfluxb         ,! volume flux through water column surface at time n-1
     $  vfluxf         ,! volume flux through water column surface at time n+1
     $  vtb            ,! va time averaged over the interval dti at time n-1
     $  vtf            ,! va time averaged over the interval dti at time n+1
     $  whs            ,! significant wave height !RMY: added whs
     $  windx          ,! x-component of the surface wind speed on depth grid !RMY: added windx
     $  windy          ,! y-component of the surface wind speed on depth grid !RMY: added windy
     $  wssurf         ,! <ws(0)> salinity flux at the surface
     $  wssurfb        ,! <ws(0)> salinity flux at the surface at time n-1
     $  wssurff        ,! <ws(0)> salinity flux at the surface at time n+1
     $  wtsurf         ,! <wt(0)> temperature flux at the surface
     $  wtsurfb        ,! <wt(0)> temperature flux at the surface at time n-1
     $  wtsurff        ,! <wt(0)> temperature flux at the surface at time n+1
     $  wubot          ,! x-momentum flux at the bottom
     $  wusurf         ,! <wu(0)> momentum flux at the surface
     $  wusurfb        ,! <wu(0)> momentum flux at the surface at time n-1
     $  wusurff        ,! <wu(0)> momentum flux at the surface at time n+1
     $  wvbot          ,! y-momentum flux at the bottom
     $  wvsurf         ,! <wv(0)> momentum flux at the surface
     $  wvsurfb        ,! <wv(0)> momentum flux at the surface at time n-1
     $  wvsurff         ! <wv(0)> momentum flux at the surface at time n+1

      common/blk2d/
     $  aam2d(im_local,jm_local)   ,
     $  advua(im_local,jm_local)   ,
     $  advva(im_local,jm_local)   ,
     $  adx2d(im_local,jm_local)   ,
     $  ady2d(im_local,jm_local)   ,
     $  art(im_local,jm_local)     ,
     $  aru(im_local,jm_local)     ,
     $  arv(im_local,jm_local)     ,
     $  cbc(im_local,jm_local)     ,
     $  cor(im_local,jm_local)     ,
     $  d(im_local,jm_local)       ,
     $  drx2d(im_local,jm_local)   ,
     $  dry2d(im_local,jm_local)   ,
     $  dt(im_local,jm_local)      ,
     $  dum(im_local,jm_local)     ,
     $  dvm(im_local,jm_local)     ,
     $  dx(im_local,jm_local)      ,
     $  dy(im_local,jm_local)      ,
     $  east_c(im_local,jm_local)  ,
     $  east_e(im_local,jm_local)  ,
     $  east_u(im_local,jm_local)  ,
     $  east_v(im_local,jm_local)  ,
     $  e_atmos(im_local,jm_local) ,
     $  egb(im_local,jm_local)     ,
     $  egf(im_local,jm_local)     ,
     $  el(im_local,jm_local)      ,
     $  elb(im_local,jm_local)     ,
     $  elf(im_local,jm_local)     ,
     $  et(im_local,jm_local)      ,
     $  etb(im_local,jm_local)     ,
     $  etf(im_local,jm_local)     ,
     $  fluxua(im_local,jm_local)  ,
     $  fluxva(im_local,jm_local)  ,
     $  fsm(im_local,jm_local)     ,
     $  h(im_local,jm_local)       ,
     $  irflg(im_local,jm_local)   , !RMY: added irflg
     $  mdp(im_local,jm_local)     , !RMY: added mdp
     $  north_c(im_local,jm_local) ,
     $  north_e(im_local,jm_local) ,
     $  north_u(im_local,jm_local) ,
     $  north_v(im_local,jm_local) ,
     $  psi(im_local,jm_local)     ,
     $  rot(im_local,jm_local)     ,
     $  ssurf(im_local,jm_local)   ,
     $  swrad(im_local,jm_local)   ,
     $  swradb(im_local,jm_local)  ,
     $  swradf(im_local,jm_local)  ,
     $  taux(im_local,jm_local)    , !RMY: added taux
     $  tauxi(im_local,jm_local)   , !RMY: added tauxi
     $  tauy(im_local,jm_local)    , !RMY: added tauy
     $  tauyi(im_local,jm_local)   , !RMY: added tauyi
     $  tps(im_local,jm_local)     ,
     $  tsurf(im_local,jm_local)   ,
     $  ua(im_local,jm_local)      ,
     $  uab(im_local,jm_local)     ,
     $  uaf(im_local,jm_local)     ,
     $  utb(im_local,jm_local)     ,
     $  utf(im_local,jm_local)     ,
     $  va(im_local,jm_local)      ,
     $  vab(im_local,jm_local)     ,
     $  vaf(im_local,jm_local)     ,
     $  vfluxb(im_local,jm_local)  ,
     $  vfluxf(im_local,jm_local)  ,
     $  vtb(im_local,jm_local)     ,
     $  vtf(im_local,jm_local)     ,
     $  whs(im_local,jm_local)     , !RMY: added whs
     $  windx(im_local,jm_local)   , !RMY: added windx
     $  windy(im_local,jm_local)   , !RMY: added windy
     $  wssurf(im_local,jm_local)  ,
     $  wssurfb(im_local,jm_local) ,
     $  wssurff(im_local,jm_local) ,
     $  wtsurf(im_local,jm_local)  ,
     $  wtsurfb(im_local,jm_local) ,
     $  wtsurff(im_local,jm_local) ,
     $  wubot(im_local,jm_local)   ,
     $  wusurf(im_local,jm_local)  ,
     $  wusurfb(im_local,jm_local) ,
     $  wusurff(im_local,jm_local) ,
     $  wvbot(im_local,jm_local)   ,
     $  wvsurf(im_local,jm_local)  ,
     $  wvsurfb(im_local,jm_local) ,
     $  wvsurff(im_local,jm_local)

!_______________________________________________________________________
! 3-D arrays
      real
     $  aam            ,! horizontal kinematic viscosity
     $  advx           ,! x-horizontal advection and diffusion terms
     $  advy           ,! y-horizontal advection and diffusion terms
     $  drhox          ,! x-component of the internal baroclinic pressure
     $  drhoy          ,! y-component of the internal baroclinic pressure
     $  dtef           ,
     $  kh             ,! vertical diffusivity
     $  km             ,! vertical kinematic viscosity
     $  kq             ,
     $  l              ,! turbulence length scale
     $  q2b            ,! twice the turbulent kinetic energy at time n-1
     $  q2             ,! twice the turbulent kinetic energy at time n
     $  q2lb           ,! q2 x l at time n-1
     $  q2l            ,! q2 x l at time n
     $  rho            ,! density
     $  rmean          ,! horizontally averaged density
     $  sb             ,! salinity at time n-1
     $  sbin           ,! salinity at time 0 !RMY: added sbin
     $  sclim          ,! horizontally averaged salinity
     $  s              ,! salinity at time n
     $  srstr          ,! restoring salinity
     $  srstrb         ,! restoring salinity at time n-1
     $  srstrf         ,! restoring salinity at time n+1
     $  tb             ,! temperature at time n-1
     $  tbin           ,! temperature at time 0 !RMY: added tbin
     $  tclim          ,! horizontally averaged temperature
     $  t              ,! temperature at time n
     $  trstr          ,! restoring temperature
     $  trstrb         ,! restoring temperature at time n-1
     $  trstrf         ,! restoring temperature at time n+1
     $  taurstr        ,! inv restoring time scale
     $  taurstrb       ,! inv restoring time scale at time n-1
     $  taurstrf       ,! inv restoring time scale at time n+1
     $  ub             ,! horizontal velocity in x at time n-1
     $  uf             ,! horizontal velocity in x at time n+1
     $  u              ,! horizontal velocity in x at time n
     $  vb             ,! horizontal velocity in y at time n-1
     $  vf             ,! horizontal velocity in y at time n+1
     $  v              ,! horizontal velocity in y at time n
     $  w              ,! sigma coordinate vertical velocity
     $  wr             ,! real (z coordinate) vertical velocity
     $  zflux

      common/blk3d/
     $  aam(im_local,jm_local,kb)  ,
     $  advx(im_local,jm_local,kb) ,
     $  advy(im_local,jm_local,kb) ,
     $  drhox(im_local,jm_local,kb),
     $  drhoy(im_local,jm_local,kb),
     $  dtef(im_local,jm_local,kb) ,
     $  kh(im_local,jm_local,kb)   ,
     $  km(im_local,jm_local,kb)   ,
     $  kq(im_local,jm_local,kb)   ,
     $  l(im_local,jm_local,kb)    ,
     $  q2b(im_local,jm_local,kb)  ,
     $  q2(im_local,jm_local,kb)   ,
     $  q2lb(im_local,jm_local,kb) ,
     $  q2l(im_local,jm_local,kb)  ,
     $  rho(im_local,jm_local,kb)  ,
     $  rmean(im_local,jm_local,kb),
     $  sb(im_local,jm_local,kb)   ,
     $  sbin(im_local,jm_local,kb) , !RMY: added sbin
     $  sclim(im_local,jm_local,kb),
     $  s(im_local,jm_local,kb)    ,
     $  srstr(im_local,jm_local,kb),
     $  srstrf(im_local,jm_local,kb),
     $  srstrb(im_local,jm_local,kb),
     $  tb(im_local,jm_local,kb)   ,
     $  tbin(im_local,jm_local,kb) , !RMY: added tbin
     $  tclim(im_local,jm_local,kb),
     $  t(im_local,jm_local,kb)    ,
     $  trstr(im_local,jm_local,kb),
     $  trstrf(im_local,jm_local,kb),
     $  trstrb(im_local,jm_local,kb),
     $  taurstr(im_local,jm_local,kb),
     $  taurstrf(im_local,jm_local,kb),
     $  taurstrb(im_local,jm_local,kb),
     $  ub(im_local,jm_local,kb)   ,
     $  uf(im_local,jm_local,kb)   ,
     $  u(im_local,jm_local,kb)    ,
     $  vb(im_local,jm_local,kb)   ,
     $  vf(im_local,jm_local,kb)   ,
     $  v(im_local,jm_local,kb)    ,
     $  w(im_local,jm_local,kb)    ,
     $  wr(im_local,jm_local,kb)   ,
     $  zflux(im_local,jm_local,kb)

!_______________________________________________________________________
! boundary value arrays
      real
     $  ele            ,! elevation at the eastern open boundary
     $  eln            ,! elevation at the northern open boundary
     $  els            ,! elevation at the southern open boundary
     $  elw            ,! elevation at the western open boundary
     $  sbe            ,! salinity at the eastern open boundary
     $  sbeb           ,! salinity at the eastern open boundary at time n-1
     $  sbef           ,! salinity at the eastern open boundary at time n+1
     $  sbn            ,! salinity at the northern open boundary
     $  sbnb           ,! salinity at the northern open boundary at time n-1
     $  sbnf           ,! salinity at the northern open boundary at time n+1
     $  sbs            ,! salinity at the southern open boundary
     $  sbsb           ,! salinity at the southern open boundary at time n-1
     $  sbsf           ,! salinity at the southern open boundary at time n+1
     $  sbw            ,! salinity at the western open boundary
     $  sbwb           ,! salinity at the western open boundary at time n-1
     $  sbwf           ,! salinity at the western open boundary at time n+1
     $  tbe            ,! temperature at the eastern open boundary
     $  tbeb           ,! temperature at the eastern open boundary at time n-1
     $  tbef           ,! temperature at the eastern open boundary at time n+1
     $  tbn            ,! temperature at the northern open boundary
     $  tbnb           ,! temperature at the northern open boundary at time n-1
     $  tbnf           ,! temperature at the northern open boundary at time n+1
     $  tbs            ,! temperature at the southern open boundary
     $  tbsb           ,! temperature at the southern open boundary at time n-1
     $  tbsf           ,! temperature at the southern open boundary at time n+1
     $  tbw            ,! temperature at the western open boundary
     $  tbwb           ,! temperature at the western open boundary at time n-1
     $  tbwf           ,! temperature at the western open boundary at time n+1
     $  uabe           ,! vertical mean of u at the eastern open boundary
     $  uabeb          ,! vertical mean of u at the eastern open boundary at time n-1
     $  uabef          ,! vertical mean of u at the eastern open boundary at time n+1
     $  uabw           ,! vertical mean of u at the western open boundary
     $  uabwb          ,! vertical mean of u at the western open boundary at time n-1
     $  uabwf          ,! vertical mean of u at the western open boundary at time n+1
     $  ube            ,! u at the eastern open boundary
     $  ubw            ,! u at the western open boundary
     $  vabn           ,! vertical mean of v at the northern open boundary
     $  vabnb          ,! vertical mean of v at the northern open boundary at time n-1
     $  vabnf          ,! vertical mean of v at the northern open boundary at time n+1
     $  vabs           ,! vertical mean of v at the southern open boundary
     $  vabsb          ,! vertical mean of v at the southern open boundary at time n-1
     $  vabsf          ,! vertical mean of v at the southern open boundary at time n+1
     $  vbn            ,! v at the northern open boundary
     $  vbs             ! v at the southern open boundary

      common/bdry/
     $  ele(jm_local)        ,
     $  eln(im_local)        ,
     $  els(im_local)        ,
     $  elw(jm_local)        ,
     $  sbe(jm_local,kb)     ,
     $  sbeb(jm_local,kb)    ,
     $  sbef(jm_local,kb)    ,
     $  sbn(im_local,kb)     ,
     $  sbnb(im_local,kb)    ,
     $  sbnf(im_local,kb)    ,
     $  sbs(im_local,kb)     ,
     $  sbsb(im_local,kb)    ,
     $  sbsf(im_local,kb)    ,
     $  sbw(jm_local,kb)     ,
     $  sbwb(jm_local,kb)    ,
     $  sbwf(jm_local,kb)    ,
     $  tbe(jm_local,kb)     ,
     $  tbeb(jm_local,kb)    ,
     $  tbef(jm_local,kb)    ,
     $  tbn(im_local,kb)     ,
     $  tbnb(im_local,kb)    ,
     $  tbnf(im_local,kb)    ,
     $  tbs(im_local,kb)     ,
     $  tbsb(im_local,kb)    ,
     $  tbsf(im_local,kb)    ,
     $  tbw(jm_local,kb)     ,
     $  tbwb(jm_local,kb)    ,
     $  tbwf(jm_local,kb)    ,
     $  uabe(jm_local)       ,
     $  uabeb(jm_local)      ,
     $  uabef(jm_local)      ,
     $  uabw(jm_local)       ,
     $  uabwb(jm_local)      ,
     $  uabwf(jm_local)      ,
     $  ube(jm_local,kb)     ,
     $  ubw(jm_local,kb)     ,
     $  vabn(im_local)       ,
     $  vabnb(im_local)      ,
     $  vabnf(im_local)      ,
     $  vabs(im_local)       ,
     $  vabsb(im_local)      ,
     $  vabsf(im_local)      ,
     $  vbn(im_local,kb)     ,
     $  vbs(im_local,kb)

!_______________________________________________________________________
! Character variables
      character*26
     $  time_start      ! date and time of start of initial run of model

      character*40
     $  source         ,
     $  title

      character*120
     $  netcdf_file    ,
     $  read_rst_file  ,
     $  write_rst_file

      common/blkchar/
     $  time_start     ,
     $  source         ,
     $  title          ,
     $  netcdf_file    ,
     $  read_rst_file  ,
     $  write_rst_file

!_______________________________________________________________________
! Logical variables
      logical lramp

      common/blklog/ lramp
