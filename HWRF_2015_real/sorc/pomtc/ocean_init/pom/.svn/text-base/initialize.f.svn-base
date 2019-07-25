! initialize.f

! initialize POM: define constant, read initial values, grid and initial
! conditions

!_______________________________________________________________________
      subroutine initialize
! initialize POM
      implicit none
      include 'pom.h'
      integer i,j,k

! initialize the MPI execution environment and create communicator for
!internal POM communications
      call initialize_mpi

! distribute the model domain across processors
      call distribute_mpi

! read input values and define constants
      call read_input

! initialize arrays for safety (may be overwritten)
      call initialize_arrays

! read in grid data
      call read_grid

! read initial and lateral boundary conditions
      call initial_conditions

! update initial conditions and set the remaining conditions
      call update_initial

! calculate the bottom friction coefficient
      call bottom_friction

! read restart data from a previous run !RMY: add tauavr+ for avrtau?
      if(nread_rst.ne.0) call read_restart_pnetcdf

! check for errors
      call sum0d_mpi(error_status,master_task)
      call bcast0d_mpi(error_status,master_task)
      if(error_status.ne.0) then
        if(my_task.eq.master_task) write(*,'(/a)')
     $                                       'POM terminated with error'
        call finalize_mpi
        stop
      end if

! write grid and initial conditions !RMY: output is on sigma levels
      if(netcdf_file.ne.'nonetcdf') call write_output_pnetcdf
      if(netcdf_file.ne.'nonetcdf') call write_output2_pnetcdf !RMY: add
      if(my_task.eq.master_task) write(*,'(/a)') 'End of initialization'

      return
      end

!_______________________________________________________________________
      subroutine read_input
! read input values and defines constants
      implicit none
      include 'pom.h'
      namelist/pom_nml/ title,netcdf_file,mode,nadv,nitera,sw,npg,dte,
     $                  isplit,time_start,nread_rst,read_rst_file,
     $                  write_rst,write_rst_file,days,prtd1,prtd2,swtch,
     $                  nbct,ifplane,igeovel,cnorth_e,ismoth,smh, !RMY
     $                  tnowindd,nl,ionedim,ipwave                !RMY

! read input namelist
      open(73,file='pom.nml',status='old')
      read(73,nml=pom_nml) !RMY: Add imay for mixing option? idamp?
      close(73)

! Input of filenames and constants

! Logical for inertial ramp (.true. if inertial ramp to be applied
! to wind stress and baroclinic forcing, otherwise .false.)
      lramp=.false. !RMY: Same effect as ramp=1.e0 in pomtc

! Reference density (recommended values: 1025 for seawater,
! 1000 for freswater; S.I. units):
      rhoref=1025.e0 !RMY: Is NOT rho_0 used in windmsg and atmos2ocean

! Density used for wusurf and wvsurf !RMY: added rho_0
      rho_0=1024.e0 !RMY: Is rho_0 used in wind and atmos2ocean

! Radius of the earth !RMY: added rearth
      rearth=6371.e3 !RMY: Is rearth used in windmsg and atmos2ocean

! Temperature bias (deg. C)
      tbias=0.e0 !RMY: 0.e0(sbpom),10.(pomtc)

! Salinity bias
      sbias=0.e0 !RMY: 0.e0(sbpom),35.(pomtc)

! gravity constant (S.I. units)
      grav=9.806e0

! maximum ocean depth !RMY: define hmax here instead of bounds_forcing
      hmax=5500.e0 !RMY: 4500.e0(ecoast),x(seamt+stide),5500.e0(pomtc)

! von Karman's constant
      kappa=0.4e0

! Bottom roughness (metres)
      z0b=.01e0 !RMY: .01e0(sbpom+pomtc)

! Minimum bottom friction coeff.
      cbcmin=.0025e0 !RMY: .0025e0(sbpom+pomtc)

! Maximum bottom friction coeff.
      cbcmax=1.e0 !RMY: 1.e0(sbpom),unbounded?(pomtc)

! Smagorinsky diffusivity coeff.
      horcon=0.1e0 !RMY: 0.1e0(ecoast+pomtc),0.2e0(seamt+stide) 

! Inverse horizontal turbulent Prandtl number (ah/am; dimensionless):
! NOTE that tprni=0.e0 yields zero horizontal diffusivity!
      tprni=1.e0 !RMY: .1e0(sbpom),1.(pomtc... via tprnu=1.) <-TEST BOTH

! Background viscosity used in subroutines profq, proft, profu and
! profv (S.I. units):
      umol=2.e-5 !RMY: 1.e-6(ecoast),2.e-5(seamt+stide+pomtc)

! Maximum magnitude of vaf (used in check that essentially tests
! for CFL violation):
      vmaxl=100.e0 !RMY: 100.e0(sbpom+pomtc)

! Maximum allowable value of:
!   <difference of depths>/<sum of depths>
! for two adjacent cells (dimensionless). This is used in subroutine
! slpmax. If >= 1, then slpmax is not applied:
      slmax=2.e0

! Water type, used in subroutine proft.
!    ntp    Jerlov water type
!     1            i
!     2            ia
!     3            ib
!     4            ii
!     5            iii
      ntp=2

! Surface temperature boundary condition, used in subroutine proft:
!    nbct   prescribed    prescribed   short wave
!           temperature      flux      penetration
!     1        no           yes           no
!     2        no           yes           yes
!     3        yes          no            no
!     4        yes          no            yes
!RMY      nbct=1 !RMY: 1(sbpom+pomtcph4),3(pomtcph3) !RMY: read in nbct

! Surface salinity boundary condition, used in subroutine proft:
!    nbcs   prescribed    prescribed
!            salinity      flux
!     1        no           yes
!     3        yes          no
! NOTE that only 1 and 3 are allowed for salinity.
      nbcs=1 !RMY: 1(sbpom+pomtc)

! Step interval during which external (2-D) mode advective terms are
! not updated (dimensionless):
      ispadv=5 !RMY: 1(ecoast),5(seamt+stide+pomtc)

! Constant in temporal filter used to prevent solution splitting
! (dimensionless):
      smoth=0.10e0 !RMY: 0.10e0(sbpom+pomtc)

! Weight used for surface slope term in external (2-D) dynamic
! equation (a value of alpha = 0.e0 is perfectly acceptable, but the
! value, alpha=.225e0 permits a longer time step):
      alpha=0.225e0 !RMY: 0.e0(sbpom),0.225e0(pomtc)

! Initial value of aam:
      aam_init=500.e0 !RMY: 0.e0(ecoast),500.e0(seamt+pomtc),10.e0(stide)

! End of input of constants

! calculate some constants
      small=1.e-8           ! Small value !RMY: 1.e-9(sbpom),vrbl(pomtc)
      pi=atan(1.e0)*4.e0    ! PI

      dti=dte*float(isplit)
      dte2=dte*2
      dti2=dti*2

      iend=max0(nint(days*24.e0*3600.e0/dti),2)
      iprint=nint(prtd1*24.e0*3600.e0/dti)
      iswtch=nint(swtch*24.e0*3600.e0/dti)
      irestart=nint(write_rst*24.e0*3600.e0/dti)
      iprint2=nint(0.0625*24.e0*3600.e0/dti) !RMY: 1.5-hourly sfc output
      nprinto=0 !RMY: initialize nprinto
      nprinto2=0 !RMY: initialize nprinto2

      ispi=1.e0/float(isplit)
      isp2i=1.e0/(2.e0*float(isplit))

! initialise time
      time0=0.e0
      time=0.e0

! print initial summary
      if(my_task.eq.master_task) then
        write(6,'(/'' title      = '',a40)') title
        write(6,'(/'' mode       = '',i10)') mode
        write(6,'('' nadv       = '',i10)') nadv
        write(6,'('' nitera     = '',i10)') nitera
        write(6,'('' sw         = '',f10.4)') sw
        write(6,'('' nread_rst  = '',i10)') nread_rst
        write(6,'('' write_rst  = '',f10.4)') write_rst
        write(6,'('' irestart   = '',i10)') irestart
        write(6,'('' dte        = '',f10.2)') dte
        write(6,'('' dti        = '',f10.1)') dti
        write(6,'('' isplit     = '',i10)') isplit
        write(6,'('' time_start = '',a26)') time_start
        write(6,'('' days       = '',f10.4)') days
        write(6,'('' iend       = '',i10)') iend
        write(6,'('' prtd1      = '',f10.4)') prtd1
        write(6,'('' iprint     = '',i10)') iprint
        write(6,'('' prtd2      = '',f10.4)') prtd2
        write(6,'('' swtch      = '',f10.2)') swtch
        write(6,'('' iswtch     = '',i10)') iswtch
        write(6,'('' iprint2    = '',i10)') iprint2
        write(6,'('' ifplane    = '',i10)') ifplane
        write(6,'('' igeovel    = '',i10)') igeovel
        write(6,'('' cnorth_e   = '',f10.1)') cnorth_e
        write(6,'('' ismoth     = '',i10)') ismoth
        write(6,'('' smh        = '',f10.0)') smh
        write(6,'('' tnowindd   = '',f10.4)') tnowindd
        write(6,'('' nl         = '',i10)') nl
        write(6,'('' ionedim    = '',i10)') ionedim
        write(6,'('' ipwave     = '',i10)') ipwave
        write(6,'('' lramp      = '',l10)') lramp
        write(6,'('' rhoref     = '',f10.3)') rhoref
        write(6,'('' tbias      = '',f10.3)') tbias
        write(6,'('' sbias      = '',f10.3)') sbias
        write(6,'('' grav       = '',f10.4)') grav
        write(6,'('' hmax       = '',f10.0)') hmax
        write(6,'('' kappa      = '',f10.4)') kappa
        write(6,'('' z0b        = '',f10.6)') z0b
        write(6,'('' cbcmin     = '',f10.6)') cbcmin
        write(6,'('' cbcmax     = '',f10.6)') cbcmax
        write(6,'('' horcon     = '',f10.3)') horcon
        write(6,'('' tprni      = '',f10.4)') tprni
        write(6,'('' umol       = '',f10.4)') umol
        write(6,'('' vmaxl      = '',f10.4)') vmaxl
        write(6,'('' slmax      = '',f10.4)') slmax
        write(6,'('' ntp        = '',i10)') ntp
        write(6,'('' nbct       = '',i10)') nbct
        write(6,'('' nbcs       = '',i10)') nbcs
        write(6,'('' ispadv     = '',i10)') ispadv
        write(6,'('' smoth      = '',f10.4)') smoth
        write(6,'('' alpha      = '',f10.4)') alpha
      end if

      return
      end

!_______________________________________________________________________
      subroutine initialize_arrays
! initialize arrays for safety
      implicit none
      include 'pom.h'
      integer i,j,k

! boundary arrays
      do i=1,im
        vabn(i)=0.e0
        vabs(i)=0.e0
        eln(i)=0.e0
        els(i)=0.e0
        do k=1,kb
          vbn(i,k)=0.e0
          vbs(i,k)=0.e0
          tbn(i,k)=0.e0
          tbs(i,k)=0.e0
          sbn(i,k)=0.e0
          sbs(i,k)=0.e0
        end do
      end do

      do j=1,jm
        uabe(j)=0.e0
        uabw(j)=0.e0
        ele(j)=0.e0
        elw(j)=0.e0
        do k=1,kb
          ube(j,k)=0.e0
          ubw(j,k)=0.e0
          tbe(j,k)=0.e0
          tbw(j,k)=0.e0
          sbe(j,k)=0.e0
          sbw(j,k)=0.e0
        end do
      end do

! 2-D and 3-D arrays
      do j=1,jm
        do i=1,im
          uab(i,j)=0.e0
          vab(i,j)=0.e0
          elb(i,j)=0.e0
          etb(i,j)=0.e0
          e_atmos(i,j)=0.e0
          vfluxb(i,j)=0.e0
          vfluxf(i,j)=0.e0
          wusurf(i,j)=0.e0
          wvsurf(i,j)=0.e0
          wtsurf(i,j)=0.e0
          wssurf(i,j)=0.e0
          swrad(i,j)=0.e0
          taux(i,j)=0.e0 !RMY: initialize taux
          tauy(i,j)=0.e0 !RMY: initialize tauy
          windx(i,j)=0.e0 !RMY: initialize windx
          windy(i,j)=0.e0 !RMY: initialize windy
          drx2d(i,j)=0.e0
          dry2d(i,j)=0.e0
          tsurf(i,j)=0.e0 !RMY: initialize tsurf
          ssurf(i,j)=0.e0 !RMY: initialize ssurf
          advua(i,j)=0.e0 !RMY: initialize advua
          advva(i,j)=0.e0 !RMY: initialize advva
          adx2d(i,j)=0.e0 !RMY: initialize adx2d
          ady2d(i,j)=0.e0 !RMY: initialize ady2d
          utb(i,j)=0.e0 !RMY: initialize utb
          vtb(i,j)=0.e0 !RMY: initialize vtb
          mdp(i,j)=0.e0 !RMY: initialize mdp
          whs(i,j)=0.e0 !RMY: initialize whs
          tauxi(i,j)=0.e0 !RMY: initialize tauxi
          tauyi(i,j)=0.e0 !RMY: initialize tauyi
          irflg(i,j)=0 !RMY: initialize irflg
        end do
      end do

      do k=1,kbm1
        do j=1,jm
          do i=1,im
            ub(i,j,k)=0.e0
            vb(i,j,k)=0.e0
            drhox(i,j,k)=0.e0 !RMY: initialize drhox
            drhoy(i,j,k)=0.e0 !RMY: initialize drhoy
          end do
        end do
      end do

      do k=1,kb
        do j=1,jm
          do i=1,im
            advx(i,j,k)=0.e0 !RMY: initialize advx
            advy(i,j,k)=0.e0 !RMY: initialize advy
            w(i,j,k)=0.e0 !RMY: initialize w
            wr(i,j,k)=0.e0 !RMY: initialize wr
            zflux(i,j,k)=0.e0 !RMY: initialize zflux
          end do
        end do
      end do

      return
      end

!_______________________________________________________________________
      subroutine read_grid
! set up vertical and horizontal grid, topography, areas and masks
      implicit none
      include 'pom.h'
      integer i,j,k
!      real deg2rad !RMY: do not define here because added to pom.h

! degrees to radians
      deg2rad=pi/180.

! read grid
      call read_grid_pnetcdf !RMY: z,zz,dx,dy,east_u,east_v,east_e,...
      !RMY: east_c,north_u,north_v,north_e,north_c,rot,h,fsm,dum,dvm
! derived vertical grid variables
      do k=1,kb-1
        dz(k)=z(k)-z(k+1)
        dzz(k)=zz(k)-zz(k+1)
      end do
!RMYflag: Specification of pomtc dz(kb) and dzz(kb) may be bad for sbpom
      dz(kb)=dz(kb-1)   !RMY: 0.(sbpom),dz(kb-1)(pomtc)
      dzz(kb)=dzz(kb-1) !RMY: 0.(sbpom),dzz(kb-1)(pomtc)

! print vertical grid information
      if(my_task.eq.master_task) then
        write(6,'(/2x,a,7x,a,9x,a,9x,a,9x,a)') 'k','z','zz','dz','dzz'
        do k=1,kb
          write(6,'(1x,i5,4f10.3)') k,z(k),zz(k),dz(k),dzz(k)
        end do
      end if

! set up Coriolis parameter !RMY: Need constant cnorth_e for f-plane cor
      if(ifplane.eq.1) then
        do j=1,jm
          do i=1,im
            cor(i,j)=2.*7.29e-5*sin(cnorth_e*deg2rad)     !RMY: f-plane
          end do
        end do
      else
        do j=1,jm
          do i=1,im
            cor(i,j)=2.*7.29e-5*sin(north_e(i,j)*deg2rad) !RMY: Real f
          end do
        end do
      end if

! inertial period for temporal filter
      period=(2.e0*pi)/abs(cor(im/2,jm/2))/86400.e0

! calculate areas of "t" and "s" cells
      do j=1,jm
        do i=1,im
          art(i,j)=dx(i,j)*dy(i,j)
        end do
      end do

! calculate areas of "u" and "v" cells
      if(ionedim.ne.1) then !RMY: added ionedim flag

      do j=2,jm
        do i=2,im
          aru(i,j)=.25e0*(dx(i,j)+dx(i-1,j))*(dy(i,j)+dy(i-1,j))
          arv(i,j)=.25e0*(dx(i,j)+dx(i,j-1))*(dy(i,j)+dy(i,j-1))
        end do
      end do
      call exchange2d_mpi(aru,im,jm)
      call exchange2d_mpi(arv,im,jm)

      if (n_west.eq.-1) then
        do j=1,jm
          aru(1,j)=aru(2,j)
          arv(1,j)=arv(2,j)
        end do
      end if

      if (n_south.eq.-1) then
        do i=1,im
          aru(i,1)=aru(i,2)
          arv(i,1)=arv(i,2)
        end do
      end if

      else

      do j=1,jm
        do i=1,im
          aru(i,j)=art(i,j) !RMY: a-grid
          arv(i,j)=art(i,j) !RMY: a-grid
        end do
      end do

      end if !RMY: end of ionedim-controlled if-statement

      do i=1,im
        do j=1,jm
          d(i,j)=h(i,j)+el(i,j)
          dt(i,j)=h(i,j)+et(i,j)
          end do
      end do

      return
      end

!_______________________________________________________________________
      subroutine initial_conditions
! set up initial and lateral boundary conditions
      implicit none
      include 'pom.h'
      integer nz
!RMY      parameter(nz=33) !RMY: 40(ecoast),33(pomtc); use pom.nml's nl
      integer i,j,k,nn !RMY: nn(ecoast),no_nn(seamt+stide)
      real sum1,sum2 !RMY: as_is(ecoast),elejmid,elwjmid(seamt),x(stide)
      real msk,cmp !RMY: added msk,cmp
      real tb0(im,jm,nl),sb0(im,jm,nl) !RMY: as_is(ecoast),x(seamt+stide)
      real tb2(im,jm,nl),sb2(im,jm,nl) !RMY: use nl in all declarations
      real ub2(im,jm,nl),vb2(im,jm,nl) !RMY: added ub2,vb2
      real elb2(im,jm) !RMY: added elb2
      real z2(nl),taa(nl),saa(nl),area(nl) !RMY: added taa,saa,area
      nz=nl !RMY: set nz=nl locally

! read initial temperature and salinity from ic file
      call read_initial_ts_pnetcdf(nz,z2,tb2,sb2) !RMY: z2,tb2,sb2

!RMY: read initial currents from ic file (already on c grid)
      call read_initial_uv_pnetcdf(nz,z2,ub2,vb2)

!RMY: read initial sea surface elevation from ic file
      call read_initial_el_pnetcdf(elb2)

!RMY: apply land/sea mask and tbias,sbias to tb2,sb2,(ub2,vb2,elb2)
!RMY: could uncomment this section to manually apply mask and biases
!      msk=999.
!      do k=1,nz
!        do j=1,jm
!          do i=1,im
!            tb2(i,j,k)=(tb2(i,j,k)-tbias)*fsm(i,j)
!            sb2(i,j,k)=(sb2(i,j,k)-sbias)*fsm(i,j)
!            if(tb2(i,j,k).gt.msk-tbias) tb2(i,j,k)=0.
!            if(sb2(i,j,k).gt.msk-sbias) sb2(i,j,k)=0.
!          end do
!        end do
!      end do

!RMY: calculate tb0,sb0 by averaging tb2,sb2 - omit read_clim_ts_pnetcdf
!RMYflag: *** THIS TB0,SB0 MAY BE WRONG - LOCAL VS. GLOBAL (A. JORDI) ***
!RMY: could uncomment this section and comment out read_clim_ts_pnetcdf
!      do k=1,nz
!        taa(k)=0.
!        saa(k)=0.
!        area(k)=0.
!        do j=1,jm
!          do i=1,im
!            if(tb2(i,j,k).ne.0.) then !RMY: may exclude real 0. values
!              taa(k)=taa(k)+tb2(i,j,k)
!              saa(k)=saa(k)+sb2(i,j,k)
!              area(k)=area(k)+1.
!            end if
!          end do
!        end do
!        taa(k)=taa(k)/area(k)
!        saa(k)=saa(k)/area(k)
!      end do
!
!      do k=1,nz
!        do j=1,jm
!          do i=1,im
!            tb0(i,j,k)=taa(k)
!            sb0(i,j,k)=saa(k)
!          end do
!        end do
!      end do
      call read_clim_ts_pnetcdf(nz,z2,tb0,sb0) !RMY: z2,tb0,sb0
!RMY: above_line_as_is(ecoast),x(seamt),using_tb2_and_sb2(stide)

! map onto sigma coordinate
      call ztosig(z2,tb2,zz,h,tb,im,jm,nz,kb,
     $                                    n_west,n_east,n_south,n_north)
      call ztosig(z2,sb2,zz,h,sb,im,jm,nz,kb,
     $                                    n_west,n_east,n_south,n_north)

! map onto sigma coordinate !RMY: as_is(ecoast),x(seamt),tb2,sb2(stide)
      call ztosig(z2,tb0,zz,h,tclim,im,jm,nz,kb,
     $                                    n_west,n_east,n_south,n_north)
      call ztosig(z2,sb0,zz,h,sclim,im,jm,nz,kb,
     $                                    n_west,n_east,n_south,n_north)

!RMY: map ub2,vb2 onto sigma coordinate to create ub,vb
      call ztosig(z2,ub2,zz,h,ub,im,jm,nz,kb,
     $                                    n_west,n_east,n_south,n_north)
      call ztosig(z2,vb2,zz,h,vb,im,jm,nz,kb,
     $                                    n_west,n_east,n_south,n_north)

!RMY: calculate elb=elb2 and then etb=elb
      do j=1,jm
        do i=1,im
          elb(i,j)=elb2(i,j)
          etb(i,j)=elb(i,j)
        end do
      end do

!RMY: remove instability in tb if any
!RMYflag: *** IS THIS INSTABILITY REMOVAL NECESSARY??? ***
      do j=1,jm
        do i=1,im
          if(fsm(i,j).eq.1) then
            do k=2,kb
              if(tb(i,j,k).gt.tb(i,j,k-1)) tb(i,j,k)=tb(i,j,k-1)
            end do
          end if
        end do
      end do

! density
      call dens(sb,tb,rho)

! mean density
      call dens(sclim,tclim,rmean) !RMY: as_is(ecoast+stide),x(seamt)
!RMY: empty_as_is(ecoast+stide),separate_loop_for_rmean=rho(seamt)

!RMY: calculate ub,vb using baropg + geovel
      if(igeovel.eq.1) then
        ramp=1.e0
        call baropg
        call geovel
        call exchange3d_mpi(ub(:,:,:),im,jm,kb)     
        call exchange3d_mpi(vb(:,:,:),im,jm,kb)
      end if

!RMY: Calculate uab,vab from ub,vb
      do j=1,jm
        do i=1,im
          uab(i,j)=0.
          vab(i,j)=0.
          cmp=0.
          do k=1,kb-1
            uab(i,j)=uab(i,j)+ub(i,j,k)*dz(k)
            vab(i,j)=vab(i,j)+vb(i,j,k)*dz(k)
            cmp=cmp+dz(k)
          end do
          uab(i,j)=uab(i,j)/cmp
          vab(i,j)=vab(i,j)/cmp
        end do
      end do

!RMY: could modify uab,vab,elb using something like subr. transports

      do k=1,kbm1
        do j=1,jm
          do i=1,im
            tclim(i,j,k)=tb(i,j,k) !RMY: as_is(ecoast+seamt),x(stide)
            sclim(i,j,k)=sb(i,j,k) !RMY: as_is(ecoast+seamt),x(stide)
          end do
        end do
      end do
!RMY: empty_as_is(ecoast+stide),separate_loops_for_uab_and_aam2d(seamt)

!RMY: Make the bottom of the temperature and salinity profiles neutral
      do j=1,jm
        do i=1,im
          tb(i,j,kb)=tb(i,j,kb-1)
          sb(i,j,kb)=sb(i,j,kb-1)
        end do
      end do

!RMY: could convectively adjust tb using something like subr. oadjust

!RMY: calculate tsurf,ssurf for possible use in proft
      do j=1,jm
         do i=1,im
           tsurf(i,j)=tb(i,j,1)
           ssurf(i,j)=sb(i,j,1)
!RMY: could add sstin(i,j)=tb(i,j,1) to use avrsst anomaly from phase3
         end do
      end do

!RMY: calculate tbin,sbin for possible use with bcond and/or smoothing
      do k=1,kb
        do j=1,jm
          do i=1,im
            tbin(i,j,k)=tb(i,j,k)*fsm(i,j)
            sbin(i,j,k)=sb(i,j,k)*fsm(i,j)
          end do
        end do
      end do

! lateral boundary conditions
! boundary conditions are variable (see subroutine lateral_bc)
      rfe=1.e0 !RMY: 1.e0(sbpom),0.1e0(pomtc - effectively)
      rfw=1.e0 !RMY: 1.e0(sbpom),0.1e0(pomtc - effectively)
      rfn=1.e0 !RMY: 1.e0(sbpom),0.1e0(pomtc - effectively)
      rfs=1.e0 !RMY: 1.e0(sbpom),0.1e0(pomtc - effectively)
!RMY: empty_as_is(ecoast),separate_loops_for_uab's_and_el's(seamt+stide)

      do k=1,kbm1
        do j=1,jm
          tbe(j,k)=tb(im,j,k) !RMY: as_is(ecoast+seamt),15.4e0(stide)
          tbw(j,k)=tb(1,j,k)
          sbe(j,k)=sb(im,j,k) !RMY: as_is(ecoast+seamt),32.09e0(stide)
          sbw(j,k)=sb(1,j,k)
        end do
        do i=1,im
          tbn(i,k)=tb(i,jm,k)
          tbs(i,k)=tb(i,1,k)
          sbn(i,k)=sb(i,jm,k)
          sbs(i,k)=sb(i,1,k)
        end do
      end do
      
!RMY: add nonzero boundary conditions for uab,vab,elb
      do j=1,jm
        uabe(j)=uab(im,j)
        uabw(j)=uab(2,j)
        ele(j)=elb(im,j)
        elw(j)=elb(2,j)
      end do
      do i=1,im
        vabn(i)=vab(i,jm)
        vabs(i)=vab(i,2)
        eln(i)=elb(i,jm)
        els(i)=elb(i,2)
      end do

!RMY: reset uab,vab,elb,etb to 0 !RMY: comment if reading in ub,vb,elb?
      do j=1,jm
        do i=1,im
          uab(i,j)=0.e0
          vab(i,j)=0.e0
!          elb(i,j)=0.e0 !RMY: do not reset elb if reading elb
!          etb(i,j)=0.e0 !RMY: do not reset etb if reading elb
        end do
      end do

      return
      end

!_______________________________________________________________________
      subroutine update_initial
! update the initial conditions and set the remaining initial conditions
      implicit none
      include 'pom.h'
      integer i,j,k

      do i=1,im
        do j=1,jm
          ua(i,j)=uab(i,j)
          va(i,j)=vab(i,j)
          el(i,j)=elb(i,j)
          et(i,j)=etb(i,j)
          etf(i,j)=et(i,j)
          d(i,j)=h(i,j)+el(i,j)
          dt(i,j)=h(i,j)+et(i,j)
          w(i,j,1)=vfluxf(i,j)
        end do
      end do

!RMY: initialize l,q2b,q2lb,kh,km,kq similar to pomtc, not sbpom <- NO!
      do k=1,kb
        do j=1,jm
          do i=1,im
            l(i,j,k)=0.1*dt(i,j)
            q2b(i,j,k)=small
            q2lb(i,j,k)=l(i,j,k)*q2b(i,j,k)
            kh(i,j,k)=l(i,j,k)*sqrt(q2b(i,j,k))
            km(i,j,k)=kh(i,j,k)
            kq(i,j,k)=kh(i,j,k)
            aam(i,j,k)=aam_init
          end do
        end do
      end do

!      do j=1,jm
!        do i=1,im
!          do k=1,3
!            l(i,j,k)=1.e0
!            q2b(i,j,k)=small
!            q2lb(i,j,k)=l(i,j,k)*q2b(i,j,k)
!            kh(i,j,k)=2.e-2
!            km(i,j,k)=kh(i,j,k)
!            kq(i,j,k)=0.2e0*l(i,j,k)*sqrt(q2b(i,j,k))
!            aam(i,j,k)=aam_init
!          end do
!          do k=4,kb
!            l(i,j,k)=1.e0
!            q2b(i,j,k)=small
!            q2lb(i,j,k)=l(i,j,k)*q2b(i,j,k)
!            kh(i,j,k)=2.e-5
!            km(i,j,k)=kh(i,j,k)
!            kq(i,j,k)=0.2e0*l(i,j,k)*sqrt(q2b(i,j,k))
!            aam(i,j,k)=aam_init
!          end do
!        end do
!      end do
!RMY: end of l,q2b,q2lb,kh,km,kq initialization

      do k=1,kbm1
        do i=1,im
          do j=1,jm
            q2(i,j,k)=q2b(i,j,k)
            q2l(i,j,k)=q2lb(i,j,k)
            t(i,j,k)=tb(i,j,k)
            s(i,j,k)=sb(i,j,k)
            u(i,j,k)=ub(i,j,k)
            v(i,j,k)=vb(i,j,k)
          end do
        end do
      end do

      call baropg !RMY: as_is(ecoast+stide),npg=2_for_baropg_mcc(seamt)

      do k=1,kbm1
        do j=1,jm
          do i=1,im
            drx2d(i,j)=drx2d(i,j)+drhox(i,j,k)*dz(k)
            dry2d(i,j)=dry2d(i,j)+drhoy(i,j,k)*dz(k)
          end do
        end do
      end do

      return
      end

!_______________________________________________________________________
      subroutine bottom_friction
! calculate the bottom friction coefficient
      implicit none
      include 'pom.h'
      integer i,j
! calculate bottom friction !RMY: why no fsm control on cbc?
      do i=1,im
        do j=1,jm
          cbc(i,j)=(kappa/log((1.+zz(kbm1))*h(i,j)/z0b))**2
          cbc(i,j)=max(cbcmin,cbc(i,j))
! if the following is invoked, then it is probable that the wrong
! choice of z0b or vertical spacing has been made:
          cbc(i,j)=min(cbcmax,cbc(i,j))
        end do
      end do
      return
      end

!_______________________________________________________________________
      subroutine ztosig(zs,tb,zz,h,t,im,jm,ks,kb,
     $                                    n_west,n_east,n_south,n_north)
! interpolate vertically
      implicit none
      integer im,jm,ks,kb
      real zs(ks),tb(im,jm,ks),zz(kb),h(im,jm),t(im,jm,kb),tin(ks),
     $                                                  tout(kb),zzh(kb)
      integer n_west,n_east,n_south,n_north
      real tmax
      integer i,j,k

      do i=2,im-1
      do j=2,jm-1
        if (h(i,j).gt.1.0) then
! special interp on z-lev for cases of no data because h smoothing
          do k=1,ks
            tin(k)=tb(i,j,k)
            if (zs(k).le.h(i,j) .and. tin(k).lt.0.01) then
              tmax=amax1(tb(i-1,j,k),tb(i+1,j,k),
     $                   tb(i,j-1,k),tb(i,j+1,k))
              tin(k)=tmax
            endif
            if (tin(k).lt.0.01 .and. k.ne.1) tin(k)=tin(k-1)
          end do

          do k=1,kb
            zzh(k)=-zz(k)*h(i,j)
          end do

! vertical spline interp
          call splinc(zs,tin,ks,2.e30,2.e30,zzh,tout,kb)

          do k=1,kb
              t(i,j,k)=tout(k)
          end do

        end if
      end do
      end do
      call exchange3d_mpi(t,im,jm,kb)

! boundaries
      do k=1,kb
        do j=1,jm
          if(n_west.eq.-1) t(1,j,k)=t(2,j,k)
          if(n_east.eq.-1) t(im,j,k)=t(im-1,j,k)
        end do
        do i=1,im
          if(n_south.eq.-1) t(i,1,k)=t(i,2,k)
          if(n_north.eq.-1) t(i,jm,k)=t(i,jm-1,k)
        end do
      end do

      return
      end

!_______________________________________________________________________
      subroutine splinc(x,y,n,yp1,ypn,xnew,ynew,m)
! interpolate using splines
      parameter (nmax=300)
      dimension x(n),y(n),y2(nmax),u(nmax),xnew(m),ynew(m)

      if (yp1.gt..99e30) then
        y2(1)=0.
        u(1)=0.
      else
        y2(1)=-0.5
        u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
      endif

      do i=2,n-1
        sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
        p=sig*y2(i-1)+2.
        y2(i)=(sig-1.)/p
        u(i)=(6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))
     $      /(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
      end do

      if (ypn.gt..99e30) then
        qn=0.
        un=0.
      else
        qn=0.5
        un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      endif

      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
      do k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k)
      end do

      do i=1,m
        call splint(x,y,y2,n,xnew(i),ynew(i))
      end do

      return
      end

!_______________________________________________________________________
      subroutine splint(xa,ya,y2a,n,x,y)
      dimension xa(n),ya(n),y2a(n)

      klo=1
      khi=n
1     if (khi-klo.gt.1) then
        k=(khi+klo)/2
        if(xa(k).gt.x)then
          khi=k
        else
          klo=k
        endif
        goto 1
      endif
      h=xa(khi)-xa(klo)
      if (h.eq.0.)  then
        error_staus=1
        write(6,'(/a)') 'Error: bad xa input in splint'
      end if
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+
     $      ((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
      return
      end
