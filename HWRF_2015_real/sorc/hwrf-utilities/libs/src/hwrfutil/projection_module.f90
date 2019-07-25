! projection_module.f90 - contains multiple classes in various
! submodules of the projection_module.  These modules are able to
! translate between various projections of the earth, providing wind
! rotation angles and gridpoint location information.  The base class
! is Projection, in proj_base_module, which allows you to point to a
! projection without knowing which projection it is.  Subclasses in
! other modules handle specific projections.


! ######################################################################
! PROJ_BASE_MODULE - Contains the base class of all projections; is
! not an actual projection.  See below for specific projections in
! other modules.
! ######################################################################

module proj_base_module
  use sysutil_module, only: fail, warn
  use constants_module, only: default_Requator=>Requator,                     &
       default_Rearth=>Rearth, default_pi=>pi, default_flattening=>flattening
  implicit none
  private
  public :: projection, init_projection
  public :: bdy_cyclic, bdy_polar, bdy_edge
  public :: default_Requator, default_flattening, default_Rearth, default_pi

  integer, parameter, public :: badpoint = -2147483648

  integer, parameter :: bdy_cyclic = 1  ! longitude boundary
  integer, parameter :: bdy_polar  = 2  ! latitude polar boundary
  integer, parameter :: bdy_edge   = 0  ! hard edge (regional grid)

  type projection
     ! This abstract base class represents any projection
     integer :: xbdy1, xbdy2, ybdy1, ybdy2 ! treatment of boundaries
     logical :: earth_winds ! .true. = winds are N/S and E/W

     ! FIXME: ADD CONSTANTS FROM CONSTANTS_MODULE
   contains
     procedure free
     procedure projinfo
     procedure locate
     procedure near
     procedure print
  end type projection

contains

  subroutine init_projection(this,xbdy1,xbdy2,ybdy1,ybdy2,earth_winds)
    class(projection) :: this
    integer, intent(in), optional :: xbdy1,xbdy2,ybdy1,ybdy2
    logical, intent(in), optional :: earth_winds

    this%earth_winds=.false.
    if(present(earth_winds)) this%earth_winds=earth_winds

    ! Default boundary: regional
    this%xbdy1=bdy_edge
    this%ybdy1=bdy_edge
    this%xbdy2=bdy_edge
    this%ybdy2=bdy_edge

    ! Use given boundary conditions if present:
    if(present(xbdy1)) this%xbdy1=xbdy1
    if(present(ybdy1)) this%ybdy1=ybdy1
    if(present(xbdy2)) this%xbdy2=xbdy2
    if(present(ybdy2)) this%ybdy2=ybdy2

    ! If one edge of the domain is cyclic, the opposite must be too:
    if(this%xbdy1==bdy_cyclic) this%xbdy2=bdy_cyclic
    if(this%xbdy2==bdy_cyclic) this%xbdy1=bdy_cyclic
    if(this%ybdy1==bdy_cyclic) this%ybdy2=bdy_cyclic
    if(this%ybdy2==bdy_cyclic) this%ybdy1=bdy_cyclic
  end subroutine init_projection

  subroutine free(this)
    class(projection), intent(inout) :: this
  end subroutine free

  subroutine print(this,str,unit)
    class(projection), intent(in) :: this
    integer, intent(in), optional :: unit
    character*(*), intent(out), optional :: str
    str=' '
    call fail('INTERNAL ERROR: a projection subclass did not implement "print" member function')
  end subroutine print

  subroutine near(this,xpoints,ypoints,irad, &
                  inear,jnear,weight, &
                  ids,ide,jds,jde,kds,kde, &
                  ims,ime,jms,jme,kms,kme, &
                  ips,ipe,jps,jpe,kps,kpe,    nnear)
    class(projection), intent(in) :: this
    integer, intent(in) :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe
    integer, intent(in) :: irad,nnear
    integer, intent(out) :: inear(nnear,ims:ime,jms:jme,kms:kme), jnear(nnear,ims:ime,jms:jme,kms:kme)
    real, intent(in) :: ypoints(ims:ime,jms:jme,kms:kme),xpoints(ims:ime,jms:jme,kms:kme)
    real, intent(out) :: weight(nnear,ims:ime,jms:jme,kms:kme)
    call fail('INTERNAL ERROR: a projection subclass did not implement "near" member function')
    inear=0
    jnear=0
    weight=0
  end subroutine near

  subroutine locate(this,lat,lon, &
                    ids,ide,jds,jde,kds,kde, &
                    ims,ime,jms,jme,kms,kme, &
                    ips,ipe,jps,jpe,kps,kpe, &
                    xpoints,ypoints,crot,srot,dx,dy,nnear,irad,rot45)
    integer, intent(in) :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe
    class(projection), intent(in) :: this
    real, intent(out), optional, dimension(ims:ime,jds:jde,kds:kde) :: &
         xpoints,ypoints,crot,srot,dx,dy
    real, intent(in), dimension(ims:ime,jms:jme,kms:kme) :: lat,lon
    integer, intent(out), optional :: nnear
    integer, intent(in), optional :: irad
    logical, intent(out), optional :: rot45
    call fail('INTERNAL ERROR: a projection subclass did not implement "locate" member function')
    if(present(xpoints)) xpoints=badpoint
    if(present(ypoints)) ypoints=badpoint
    if(present(crot)) crot=1
    if(present(srot)) srot=0
    if(present(dx)) dx=badpoint
    if(present(dy)) dy=badpoint
    if(present(nnear)) nnear=0
    if(present(rot45)) rot45=.false.
  end subroutine locate

  subroutine projinfo(this, &
                    ids,ide,jds,jde,kds,kde, &
                    ims,ime,jms,jme,kms,kme, &
                    ips,ipe,jps,jpe,kps,kpe, &
                    lat,lon,crot,srot,dx,dy)
    ! In subclasses, this generates projection information on the full
    ! 3D grid.
    !   lat,lon = latitudes and longitudes
    !   crot,srot = cosine and sine of wind rotation angle
    !   dx,dy = approximate local grid cell size
    class(projection), intent(in) :: this
    real, optional, intent(out), dimension(ims:ime,jms:jme,kms:kme) :: &
         lat,lon,crot,srot,dx,dy
    integer, intent(in) :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe
    call fail('INTERNAL ERROR: a projection subclass did not implement "projinfo" member function')
    if(present(lat)) lat=badpoint
    if(present(lon)) lon=badpoint
    if(present(crot)) crot=1
    if(present(srot)) srot=0
    if(present(dx)) dx=badpoint
    if(present(dy)) dy=badpoint
  end subroutine projinfo
end module proj_base_module

! ######################################################################
! PROJ_LATLON_MODULE - Regular Latitude-Longitude Grids
! ######################################################################

module proj_latlon_module
  use sysutil_module, only: fail, warn
  use proj_base_module
  implicit none
  private
  public :: proj_latlon, init_proj_latlon

  type, extends(projection) :: proj_latlon
     real :: lat1,lon1      ! start loc
     real :: dlat,dlon      ! increment (positive or negative)
     integer :: nx,ny       ! number of points (must be > 0)
     logical :: lon_inner   ! true = nx is lon.  false = ny is lon
     real :: Rearth0, flattening

     ! Global boundary handling:
     logical :: cyclic_lon  ! true = all longitudes included
     logical :: north_polar ! true = north pole included
     logical :: south_polar ! true = south pole included
   contains
     procedure projinfo
     procedure print
     procedure locate
     procedure near
  end type proj_latlon

contains

  subroutine init_proj_latlon(this,lat1,lon1,dlat,dlon,nx,ny, &
       lon_inner,cyclic_lon,north_polar,south_polar, &
       Rearth0,flattening)
    ! Sets the projection information.  See proj_latlon for details.
    ! The lon_inner is optional, and will be assumed .true. if missing
    class(proj_latlon) :: this
    real, intent(in) :: lat1,lon1, dlat,dlon
    real, optional, intent(in) :: Rearth0,flattening
    logical, optional, intent(in) :: lon_inner,cyclic_lon,north_polar,south_polar
    integer, intent(in) :: nx,ny
    integer :: xbdy1,ybdy1,ybdy2,swapper
    logical :: isspolar,isnpolar,iscyclic,islonin

    isspolar=.false.
    isnpolar=.false.
    iscyclic=.false.
    islonin = .true.
    if(present(south_polar)) isspolar=south_polar
    if(present(north_polar)) isnpolar=north_polar
    if(present(cyclic_lon))  iscyclic=cyclic_lon
    if(present(lon_inner))   islonin =lon_inner

    ! Default boundary conditions: regional
    xbdy1=bdy_edge
    ybdy1=bdy_edge
    ybdy2=bdy_edge

    ! Handle different conditions, pretending dx>0, dy>0 and lon_inner:
    if(isspolar) ybdy1=bdy_polar
    if(isnpolar) ybdy2=bdy_polar
    if(iscyclic) xbdy1=bdy_cyclic

    ! Swap latitude boundary based on sign of dy
    if(dlat<0) then
       swapper=ybdy1
       ybdy1=ybdy2
       ybdy2=swapper
    endif

    ! Call parent class constructor based on lon_inner
    if(islonin) then
       call init_projection(this,xbdy1=xbdy1,ybdy1=ybdy1,ybdy2=ybdy2,earth_winds=.true.)
    else
       call init_projection(this,ybdy1=xbdy1,xbdy1=ybdy1,xbdy2=ybdy2,earth_winds=.true.)
    endif

    ! 
    this%Rearth0=default_Rearth
    this%flattening=0.
    this%lon_inner=.true.
    this%north_polar=isnpolar
    this%south_polar=isspolar
    this%cyclic_lon=iscyclic
    this%lon_inner=islonin

    if(present(Rearth0))        this%Rearth0=Rearth0
    if(present(flattening))     this%flattening=flattening

    this%nx=nx
    this%ny=ny
    this%lat1=lat1
    this%lon1=lon1
    this%dlat=dlat
    this%dlon=dlon
  end subroutine init_proj_latlon

  subroutine print(this,str,unit)
    class(proj_latlon), intent(in) :: this
    integer, intent(in), optional :: unit
    character*(*), intent(out), optional :: str
    integer :: nlat,nlon
33  format('regular lat-lon: lat1=',F0.5,' dlat=',F0.5,' nlat=',F0.5, &
           ' lon1=',F0.5,' dlon=',F0.5,' nlon=',F0.5)
    if(this%lon_inner) then
       nlat=this%ny
       nlon=this%nx
    else
       nlat=this%nx
       nlon=this%ny
    endif
    if(present(str)) then
       write(str,33)  this%lat1, this%dlat, nlat, &
                      this%lon1, this%dlon, nlon
    endif
    if(present(unit)) then
       write(unit,33) this%lat1, this%dlat, nlat, &
                      this%lon1, this%dlon, nlon
    endif
  end subroutine print

  subroutine projinfo(this, &
                    ids,ide,jds,jde,kds,kde, &
                    ims,ime,jms,jme,kms,kme, &
                    ips,ipe,jps,jpe,kps,kpe, &
                    lat,lon,crot,srot,dx,dy)
    class(proj_latlon), intent(in) :: this
    real, intent(out), dimension(ims:ime,jms:jme,kms:kme), optional :: &
         lat,lon,crot,srot,dx,dy
    integer, intent(in) :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe
    integer :: i,j,k
    real :: rlat, locstore(ips:ipe), radstore(ips:ipe), lenstore(ips:ipe)
    real :: Rearth
    real :: DEGRAD
    character*255 message
    DEGRAD=default_pi/180
    if(present(crot)) then
       !$OMP PARALLEL DO PRIVATE(i,j)
       do j=jps,jpe
       enddo
    endif
    if(present(srot) .and. present(crot)) then
       do k=kps,kpe
          !$OMP PARALLEL DO PRIVATE(i,j)
          do j=jps,jpe
             srot(ips:ipe,j,k)=0
             crot(ips:ipe,j,k)=1
          enddo
       enddo
    elseif(present(srot) .neqv. present(crot)) then
       call fail('ABORT: when calling proj_latlon_module subroutine "projinfo", you must specify both srot and crot or neither')
    endif
    if(present(dx) .neqv. present(dy)) then
83     format('ABORT: when calling proj_latlon_module subroutine "projinfo", you must specify both dx and dy or neither',/,' dx present?',I0,/,' dy present?',I0)
       write(message,83) merge(1,0,present(dx)),merge(1,0,present(dy))
       call fail(message)
    endif
    if(present(lat) .neqv. present(lon)) then
       call fail('ABORT: when calling proj_latlon_module subroutine "projinfo", you must specify both lat and lon or neither')
    endif
    bigk: do k=kps,kpe
    if(this%lon_inner) then
       if(present(lon)) then
          !$OMP PARALLEL DO PRIVATE(i)
          do i=ips,ipe
             locstore(i)=this%lon1+this%dlon*(i-1)
          enddo
       endif
       !$OMP PARALLEL DO PRIVATE(i,j,rlat,Rearth)
       do j=jps,jpe
          rlat=this%lat1+this%dlat*(j-jds)
          if(present(dx)) then
             if(abs(this%flattening)<1e-6) then
                Rearth=this%Rearth0
             else
                Rearth=this%Rearth0*(1-sin(rlat*DEGRAD)**2*this%flattening)
             endif
             dy(ips:ipe,j,k)=Rearth*DEGRAD*this%dlat
             dx(ips:ipe,j,k)=Rearth*DEGRAD*this%dlon*cos(rlat*DEGRAD)
          endif
          if(present(lon)) then
             lon(ips:ipe,j,k)=locstore(ips:ipe)
             lat(ips:ipe,j,k)=rlat
          endif
       enddo
    else
       !$OMP PARALLEL DO PRIVATE(i,rlat,Rearth)
       do i=ips,ipe
          rlat=this%lat1+this%dlat*(i-ids)
          if(present(dx)) then
             if(abs(this%flattening)<1e-6) then
                Rearth=this%Rearth0
             else
                Rearth=this%Rearth0*(1-sin(rlat*DEGRAD)**2*this%flattening)
             endif
             radstore(i)=Rearth
             lenstore(i)=Rearth*DEGRAD*this%dlon*cos(rlat*DEGRAD)
          endif
          if(present(lat)) locstore(i)=rlat
       enddo
       if(present(lat)) then
          !$OMP PARALLEL DO PRIVATE(j)
          do j=jps,jpe
             lat(ips:ipe,j,k)=locstore(ips:ipe)
             lon(ips:ipe,j,k)=this%lon1+this%dlon*(j-jds)
          enddo
       endif
       if(present(dx)) then
          !$OMP PARALLEL DO PRIVATE(j)
          do j=jps,jpe
             dx(ips:ipe,j,k)=radstore(ips:ipe)*DEGRAD*this%dlat
             dy(ips:ipe,j,k)=lenstore(ips:ipe)
          enddo
       endif
    endif
    enddo bigk
  end subroutine projinfo

  subroutine near(this,xpoints,ypoints,irad, &
                  inear,jnear,weight, &
                  ids,ide,jds,jde,kds,kde, &
                  ims,ime,jms,jme,kms,kme, &
                  ips,ipe,jps,jpe,kps,kpe, nnear)
    class(proj_latlon), intent(in) :: this
    integer, intent(in) :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe
    integer, intent(in) :: irad,nnear
    integer, intent(out) :: inear(nnear,ims:ime,jms:jme,kms:kme), jnear(nnear,ims:ime,jms:jme,kms:kme)
    real, intent(in) :: ypoints(ims:ime,jms:jme,kms:kme),xpoints(ims:ime,jms:jme,kms:kme)
    real, intent(out) :: weight(nnear,ims:ime,jms:jme,kms:kme)
    logical :: x1ok, x2ok, y1ok, y2ok, nxe, nye
    integer :: i,j,o,x1,x2,y1,y2, nx,ny, s1,s2, k
    real :: x,y,xw,yw
    real, parameter :: repsilon = 0.9999, epsilon=0.0001
    integer :: nx10,ny10,sx,sy,c

    if(irad/=1) then
       call fail('ABORT: only irad=1 (linear interpolation) is supported in proj_latlon_module subroutine "near"')
    endif

    nx10=10*this%nx
    ny10=10*this%ny

    nx = this%nx
    ny = this%ny

    sx  = int(sign(1.,this%dlon))
    sy  = int(sign(1.,this%dlat))
    if(this%lon_inner) then
       i=sx
       sx=sy
       sy=i
    endif

    if(nnear<4) then
       call fail('ABORT: in proj_latlon_module "near" subroutine, argument nnear<4')
    endif

    bigk: do k=kps,kpe
    !$OMP PARALLEL DO PRIVATE(x,y,x1,x2,xw,y1,y2,yw,x1ok,x2ok,y1ok,y2ok,c,o,i,j)
    ixy: do j=jps,jpe
       jxy: do i=ips,ipe
          x=xpoints(i,j,k)
          y=ypoints(i,j,k)

          x1=floor(x)
          x2=ceiling(x)
          xw=abs(x-x1)

          y1=floor(y)
          y2=ceiling(y)
          yw=abs(y-y1)

          if(this%lon_inner) then
             if(this%xbdy1==bdy_cyclic) then
                x1=mod(x1-1+nx10,this%nx)+1
                x2=mod(x2-1+nx10,this%nx)+1
             endif
          else
             if(this%ybdy1==bdy_cyclic) then
                y1=mod(y1-1+ny10,this%ny)+1
                y2=mod(y2-1+ny10,this%ny)+1
             endif
          endif

          if(xw>repsilon) x1=x2
          if(xw< epsilon) x2=x1
          if(x1==x2) xw=0.5

          if(yw>repsilon) y1=y2
          if(yw< epsilon) y2=y1
          if(y1==y2) yw=0.5

          x1ok = (x1>=1 .and. x1<=nx)
          x2ok = (x2>=1 .and. x2<=nx)
          y1ok = (y1>=1 .and. y1<=ny)
          y2ok = (y2>=1 .and. y2<=ny)

          o=1 ! next index to store interp data
          
          ! Store polar boundary interpolation information
          lonin:if(this%lon_inner) then
             y1polar:if(this%ybdy1==bdy_polar .and. y1<1) then
                c=(nx-1)/2+mod(nx,2)
                if(x1ok) then
                   weight(o,i,j,k)=(1-xw)*(1-yw)
                   inear (o,i,j,k)=mod(x1-1-c+nx10,nx)+1
                   jnear (o,i,j,k)=1
                   o=o+1
                endif
                if(x2ok) then
                   weight(o,i,j,k)=   xw *(1-yw)
                   inear (o,i,j,k)=mod(x2-1+c+nx10,nx)+1
                   jnear (o,i,j,k)=1
                   o=o+1
                endif
             endif y1polar
             y2polar: if(this%ybdy2==bdy_polar .and. y2>ny) then
                c=(nx-1)/2+mod(nx,2)
                if(x1ok) then
                   weight(o,i,j,k)=(1-xw)*   yw 
                   inear (o,i,j,k)=mod(x1-1-c+nx10,this%nx)+1
                   jnear (o,i,j,k)=ny-(y2-ny-1)
                   o=o+1
                endif
                if(x2ok) then
                   weight(o,i,j,k)=   xw *   yw 
                   inear (o,i,j,k)=mod(x2-1+c+nx10,this%nx)+1
                   jnear (o,i,j,k)=ny-(y2-ny-1)
                   o=o+1
                endif
             endif y2polar
          else
             x1polar: if(this%xbdy1==bdy_polar .and. x1<1) then
                c=(ny-1)/2+mod(ny,2)
                if(y1ok) then
                   weight(o,i,j,k)=(1-xw)*(1-yw)
                   inear (o,i,j,k)=1
                   jnear (o,i,j,k)=mod(y1-1-c+ny10,this%ny)+1
                   o=o+1
                endif
                if(y2ok) then
                   weight(o,i,j,k)=(1-xw)*   yw 
                   inear (o,i,j,k)=1
                   jnear (o,i,j,k)=mod(y2-1+c+ny10,this%ny)+1
                   o=o+1
                endif
             endif x1polar
             x2polar: if(this%xbdy2==bdy_polar .and. x2>nx) then
                c=(ny-1)/2+mod(ny,2)
                if(y1ok) then
                   weight(o,i,j,k)=   xw *(1-yw)
                   inear (o,i,j,k)=nx
                   jnear (o,i,j,k)=mod(y1-1-c+ny10,this%ny)+1
                   o=o+1
                endif
                if(x2ok) then
                   weight(o,i,j,k)=   xw *   yw 
                   jnear (o,i,j,k)=mod(y2-1+c+ny10,this%ny)+1
                   inear (o,i,j,k)=nx
                   o=o+1
                endif
             endif x2polar
          endif lonin

          ! Store non-polar boundaries:
          x1check: if(x1ok) then
             if(y1ok) then
                weight(o,i,j,k)  = (1-xw)*(1-yw)
                inear (o,i,j,k)  = x1
                jnear (o,i,j,k)  = y1
                o=o+1
             endif
             if(y2ok) then
                weight(o,i,j,k)  = (1-xw)*   yw
                inear (o,i,j,k)  = x1
                jnear (o,i,j,k)  = y2
                o=o+1
             endif
          endif x1check
          x2check: if(x2ok) then
             if(y1ok) then
                weight(o,i,j,k)  =    xw *(1-yw)
                inear (o,i,j,k)  = x2
                jnear (o,i,j,k)  = y1
                o=o+1
             endif
             if(y2ok) then
                weight(o,i,j,k)  =    xw *   yw
                inear(o,i,j,k)   = x2
                jnear(o,i,j,k)   = y2
                o=o+1
             endif
          endif x2check

          ! Fill remaining boundary info with zero:
          ofill: do while(o<=nnear)
             weight(o,i,j,k) = 0
             inear (o,i,j,k) = badpoint
             jnear (o,i,j,k) = badpoint
             o=o+1
          enddo ofill
       enddo jxy
    enddo ixy
    !$OMP END PARALLEL DO
    enddo bigk
  end subroutine near

  subroutine locate(this,lat,lon, &
                    ids,ide,jds,jde,kds,kde, &
                    ims,ime,jms,jme,kms,kme, &
                    ips,ipe,jps,jpe,kps,kpe, &
                    xpoints,ypoints,crot,srot,dx,dy,nnear,irad,rot45)
    class(proj_latlon), intent(in) :: this
    integer, intent(in) :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe
    real, intent(out), optional, dimension(ims:ime,jms:jme,kms:kme) :: &
         xpoints,ypoints,crot,srot,dx,dy
    real, intent(in), dimension(ims:ime,jms:jme,kms:kme) :: lat,lon
    integer, intent(out), optional :: nnear
    integer, intent(in), optional :: irad
    integer :: i,j,k
    real :: x,y, dlat,dlon, rlat,Rearth, DEGRAD, xw,yw, xw1,yw1, rlon
    integer :: x1,y1,x2,y2
    logical, intent(out), optional :: rot45

    DEGRAD=default_pi/180

    if(present(rot45)) rot45=.false.
    if(present(nnear)) then
       nnear=4
       if(present(irad)) then
          nnear=2*irad * 2*irad
          if(nnear<0) nnear=0
       endif
    endif

    if(present(crot) .and. present(srot)) then
       do k=kps,kpe
       !$OMP PARALLEL DO PRIVATE(i,j)
       do j=jps,jpe
          do i=ips,ipe
             crot(i,j,k)=1
             srot(i,j,k)=0
          enddo
       enddo
       !$OMP END PARALLEL DO
       enddo
    elseif(present(crot) .neqv. present(srot)) then
       call fail('ABORT: proj_latlon_module subroutine "locate", you must provide both crot and srot or neither.')
    endif

    xy: if(present(xpoints) .and. present(ypoints)) then
       do k=kps,kpe
       !$OMP PARALLEL DO PRIVATE(i,j,dlat,dlon,x,y,rlon)
       jxy: do j=jps,jpe
          ixy: do i=ips,ipe
             dlat=(lat(i,j,k)-this%lat1)/this%dlat+1
             dlon=(lon(i,j,k)-this%lon1)/this%dlon+1
             !dlon=mod(lon(i,j,k)-this%lon1+3600,360.)/this%dlon + 1

             ! Handle dimension ordering:
             if(this%lon_inner) then
                x=dlon ; y=dlat
             else
                x=dlat ; y=dlon
             endif

             xpoints(i,j,k)=x
             ypoints(i,j,k)=y
          enddo ixy
       enddo jxy
       !$OMP END PARALLEL DO
       enddo
    elseif(present(xpoints) .neqv. present(ypoints)) then
       call fail('ABORT: in proj_latlon_module subroutine "locate", you must provide both xpoints and ypoints or neither.')
    endif xy

    dxdy: if(present(dx) .and. present(dy)) then
       do k=kps,kpe
       !$OMP PARALLEL DO PRIVATE(i,j,rlat,dlat,dlon,Rearth)
       jdxdy: do j=jps,jpe
          idxdy: do i=ips,ipe
             rlat=lat(i,j,k)*DEGRAD
             if(abs(this%flattening)<1e-6) then
                Rearth=this%Rearth0
             else
                Rearth=this%Rearth0*(1-sin(rlat)**2*this%flattening)
             endif
             dlat=Rearth*DEGRAD*this%dlat
             dlon=Rearth*DEGRAD*this%dlon*cos(rlat)
             if(this%lon_inner) then
                dx(i,j,k)=dlon
                dy(i,j,k)=dlat
             else
                dx(i,j,k)=dlat
                dy(i,j,k)=dlon
             endif
          enddo idxdy
       enddo jdxdy
       !$OMP END PARALLEL DO
       enddo
    elseif(present(dx) .neqv. present(dy)) then
       call fail('ABORT: in proj_latlon_module subroutine "locate", you must provide both dx and dy or neither.')
    endif dxdy
  end subroutine locate

end module proj_latlon_module

! ######################################################################
!   PROJ_NMME_MODULE - Regional WRF-NMM E Grid
! ######################################################################

module proj_nmme_module
  use sysutil_module, only: fail, warn
  use proj_latlon_module, only: init_proj_latlon, proj_latlon
  use proj_base_module, only: default_pi, badpoint
  implicit none
  private

  public :: proj_nmme, init_proj_nmme
  
  type, extends(proj_latlon) :: proj_nmme
     double precision :: lat0,lon0 ! projection center

     ! Cached calculations:
     double precision, private :: clat0,slat0,hi,hj,x0,y0,slat1,clat1,hs0,clon1,slatr,clatr
     double precision, private :: clonr,rlatlr,rlatcr,rlonlr,rloncr, clon0
     integer, private :: is1, iscan,jscan,kscan
   contains
     procedure projinfo
     procedure locate
     procedure near
     procedure print
     procedure latlon_bounds
     procedure proj_cache   ! recompute cached calculations
     procedure, private :: rotate_bound ! rotate lat1,lon1 to geo coordinates
  end type proj_nmme

  interface init_proj_nmme
     module procedure new_proj_nmme
     module procedure copy_proj_nmme
  end interface

contains

  subroutine rotate_bound(this)
    class(proj_nmme), intent(inout) :: this
    double precision :: clonr,slatr,clatr,slat,clat,hs,hi, clon
    double precision :: DEGRAD, RADDEG
    !$OMP MASTER
    RADDEG=180/default_pi
    DEGRAD=default_pi/180
    HI=(-1.)**this%ISCAN
    clonr = cos(this%lon1*DEGRAD)
    slatr = sin(this%lat1*DEGRAD)
    clatr = cos(this%lat1*DEGRAD)
    slat  = this%clat0*slatr + this%slat0*clatr*clonr
    if(slat<=-1) then
       clat=0
       clon=this%clon0
       this%lon1=0
       this%lat1=-90
    elseif(slat>=1) then
       clat=0
       clon=this%clon0
       this%lon1=0
       this%lat1=90
    else
       clat=sqrt(1-slat**2)
       clon=min(1.,max(-1.,(this%clat0*clatr*clonr - this%slat0*slatr)/clat))
       hs=hi*sign(1.,this%x0+1-this%is1)
       if(abs(hs)/=1) hs=1
       this%lon1=mod(this%lon0+hs*RADDEG*acos(min(1.,max(-1.,clon)))+3600,360.)
       this%lat1=asin(max(-1.,min(1.,slat)))*RADDEG
    endif
    !$OMP END MASTER
  end subroutine rotate_bound

  subroutine copy_proj_nmme(new,old,rotated1,vgrid,earth_winds)
    class(proj_nmme), intent(in) :: old
    type(proj_nmme), intent(inout) :: new
    logical, intent(in), optional :: eartH_winds, vgrid, rotated1
    logical :: rotateme

    rotateme=.false.
    if(present(rotated1)) rotateme=rotated1
    new%kscan=old%kscan
    if(present(vgrid)) then
       if(vgrid) then
          new%kscan=1
       else
          new%kscan=0
       endif
    endif
    new%iscan=old%iscan
    new%jscan=old%iscan
    call init_proj_latlon(new,old%lat1,old%lon1, old%dlat,old%dlon, &
         old%nx,old%ny, lon_inner=.true., &
         cyclic_lon=.false.,north_polar=.false.,south_polar=.false.)
    new%earth_winds=old%earth_winds
    if(present(earth_winds)) new%earth_winds=earth_winds
    new%lat0=old%lat0
    new%lon0=old%lon0

    call new%proj_cache(rotateme)
  end subroutine copy_proj_nmme

  subroutine new_proj_nmme(this,cenlat,cenlon,nx,ny, dlat,dlon, &
                   lat1,lon1,rotated1,vgrid,earth_winds)
    class(proj_nmme), intent(inout) :: this
    real, intent(in) :: lat1,lon1, dlat,dlon, cenlat,cenlon
    integer, intent(in) :: nx,ny
    logical, optional, intent(in) :: rotated1, vgrid, earth_winds
    logical :: rotateme

    rotateme=.false.
    if(present(rotated1)) rotateme=rotated1
    this%kscan=0
    if(present(vgrid)) then
       if(vgrid) this%kscan=1
    endif
    this%iscan=0
    this%jscan=1
    call init_proj_latlon(this,lat1,lon1,dlat,dlon,nx,ny,lon_inner=.true., &
           cyclic_lon=.false.,north_polar=.false.,south_polar=.false.)
    this%earth_winds=.false. ! winds are grid-relative in WRF-NMM
    this%lat0=cenlat
    this%lon0=cenlon

    if(present(earth_winds)) this%earth_winds=earth_winds

    call this%proj_cache(rotateme)
  end subroutine new_proj_nmme

  subroutine proj_cache(this,rotateme)
    class(proj_nmme), intent(inout) :: this
    logical, intent(in) :: rotateme
    double precision :: DEGRAD, RADDEG, rlatlr, rlonlr

    !$OMP MASTER
    DEGRAD=default_pi/180
    RADDEG=180/default_pi

    ! Cache some calculations needed for projecting:
    
    if(this%kscan==0) then
       this%is1=(this%ny+1)/2
    else
       this%is1=this%ny/2
    endif

    this%clat0=cos(this%lat0*DEGRAD)
    this%slat0=sin(this%lat0*DEGRAD)
    this%HI=(-1.)**this%ISCAN
    this%HJ=(-1.)**(1-this%JSCAN)
    this%x0 = (this%ny-1)/2. + ( (this%nx-1)/2. - this%IS1 )
    this%y0 = (this%ny-1)/2. - ( (this%nx-1)/2. - this%IS1 ) + this%KSCAN

    ! print *,'lon0 before rotation is ',this%lon0
    ! print *,'lat0 before rotation is ',this%lat0

    if(rotateme) then
       ! print *,'lon1 before rotation is ',this%lon1
       ! print *,'lat1 before rotation is ',this%lat1
       this%rlatlr=this%lat1
       this%rlonlr=this%lon1
       call this%rotate_bound()
       ! print *,'lon1 after rotation is ',this%lon1
       ! print *,'lat1 after rotation is ',this%lat1
    endif

    this%slat1=sin(this%lat1*DEGRAD)
    this%clat1=cos(this%lat1*DEGRAD)

    this%HS0=sign(1.,mod(this%lon1-this%lon0+180+3600,360.)-180)

    this%clon1=cos((this%lon1-this%lon0)*DEGRAD)
    this%slatr=this%clat0*this%slat1-this%slat0*this%clat1*this%clon1
    this%clatr=sqrt(1-this%slatr**2)
    this%clonr=(this%clat0*this%clat1*this%clon1 + &
         this%slat0*this%slat1)/this%clatr
    if(.not. rotateme) this%rlatlr=RADDEG*asin(max(-1.,min(1.,this%slatr)))
    this%rlatcr=this%rlatlr+(this%ny-1)/2.*this%dlat
    if(.not. rotateme) this%rlonlr=this%hs0*RADDEG*acos(max(-1.,min(1.,this%clonr)))
    this%rloncr=this%rlonlr+(this%nx-1)/2.*this%dlon
    ! print *,'rlonlr is ',this%rlonlr
    ! print *,'rlatlr is ',this%rlatlr
    ! print *,'rloncr is ',this%rloncr
    ! print *,'rlatcr is ',this%rlatcr
    !$OMP END MASTER
  end subroutine proj_cache

  subroutine print(this,str,unit)
    class(proj_nmme), intent(in) :: this
    integer, intent(in), optional :: unit
    character*(*), intent(out), optional :: str
33  format('NMM E grid lat-lon: cenlat=',F0.5,&
         ' lat1=',F0.5,' dlat=',F0.5,' nlat=',I0, &
         ' cenlon=',F0.5,' lon0=',F0.5,' dlon=',F0.5,' nlon=',I0)
    if(present(str)) then
       write(str,33) this%lat0, this%lat1, this%dlat, this%ny, &
                     this%lon0, this%lon1, this%dlon, this%nx
    endif
    if(present(unit)) then
       write(unit,33) this%lat0, this%lat1, this%dlat, this%ny, &
                     this%lon0, this%lon1, this%dlon, this%nx
    endif
  end subroutine print

  subroutine locate(this,lat,lon, &
                    ids,ide,jds,jde,kds,kde, &
                    ims,ime,jms,jme,kms,kme, &
                    ips,ipe,jps,jpe,kps,kpe, &
                    xpoints,ypoints,crot,srot,dx,dy,nnear,irad,rot45)
    logical, intent(out), optional :: rot45
    class(proj_nmme), intent(in) :: this
    integer, intent(in) :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe
    real, intent(out), optional, dimension(ims:ime,jms:jme,kms:kme) :: &
         xpoints,ypoints,crot,srot,dx,dy
    real, intent(in), dimension(ims:ime,jms:jme,kms:kme) :: lat,lon
    integer, intent(out), optional :: nnear
    integer, intent(in), optional :: irad
    integer*8 :: badpoints
    integer :: i,j,k,is1, im,jm
    double precision :: DEGRAD, RADDEG, valt,vlon, clat,clon,slat, slatr,clatr
    double precision :: xmin,xmax,ymin,ymax, epsilon
    double precision :: rlonr,rlatr, xptf,yptf, hs,vlat, cenlon, clonr, slon, dlats,dlons
    logical :: sentrange

    epsilon=.01 ! gridpoints
    jm=this%ny
    im=this%nx*2-1
    xmin=-epsilon
    xmax=im+1+epsilon
    if(im==nint(360/abs(this%dlon))) xmax=im+2+epsilon
    ymin=-epsilon
    ymax=jm+1+epsilon

    sentrange=.false.

    DEGRAD=default_pi/180
    RADDEG=180/default_pi

    if(present(rot45)) rot45=.true.
    if(present(nnear)) then
       nnear=4
       if(present(irad)) then
          nnear=2*irad * 2*irad
          if(nnear<0) nnear=0
       endif
    endif
    if(present(dx)) then
       do k=kps,kpe
          !$OMP PARALLEL DO PRIVATE(i,j)
          do j=jps,jpe
             do i=ips,ipe
                dx(i,j,k)=0
                dy(i,j,k)=0
             enddo
          enddo
          !$OMP END PARALLEL DO
       enddo
    endif
    badpoints=0
    findpoint: if(  present(xpoints) .or. present(ypoints) .or. &
         present(crot) .or. present(srot)) then
       cenlon=DEGRAD*this%lon0
       !dlats=DEGRAD*this%dlat
       !dlons=DEGRAD*this%dlon
       dlats=this%dlat
       dlons=this%dlon
       is1=this%is1
       bigk: do k=kps,kpe
          !$OMP PARALLEL DO DEFAULT(SHARED) &
          !$OMP PRIVATE(i,j,vlat,vlon,hs,clon,slat,clat,slatr,clatr,rlonr,rlatr,xptf,yptf,clonr,slon) &
          !$OMP REDUCTION(+:badpoints)
          bigj: do j=jps,jpe
             bigi: do i=ips,ipe
                vlat=lat(i,j,k)*DEGRAD
                vlon=lon(i,j,k)*DEGRAD
                hs=sign(1.,mod(lon(i,j,k)-this%lon0+180+3600,360.)-180.)
                clon=cos(vlon-cenlon)
                slat=sin(vlat)
                clat=cos(vlat)
                slatr=this%clat0*slat-this%slat0*clat*clon
                ispole: if(slatr<=-1) then
                   clatr=0
                   rlonr=0
                   rlatr=-90
                elseif(slatr>=1) then
                   clatr=0
                   rlonr=0
                   rlatr=90
                else
                   clatr=sqrt(1-slatr**2)
                   clonr=min(1.,max(-1.,(this%clat0*clat*clon+this%slat0*slat)/clatr))
                   rlonr=RADDEG*hs*acos(min(1.,max(-1.,clonr)))
                   rlatr=RADDEG*   asin(min(1.,max(-1.,slatr)))
                endif ispole
33              format('bad point at ',I0,',',I0,' rlonr=',F0.5,' rlatr=',F0.5,' rlonlr=',F0.5,' rlatlr=',F0.5,' x,y=',F0.5,',',F0.5)
44              format('    ok range: x=',F0.5,'...',F0.5,' y=',F0.5,'...',F0.5)
                rot: if(present(crot)) then
                   if_grid_winds: if(.not. this%earth_winds) then
                      rotpole: if(clatr<=0) then
                         crot(i,j,k)=-sign(1.,slatr*this%slat0)
                         srot(i,j,k)=0
                      else
                         slon=sin(vlon-cenlon)
                         crot(i,j,k)=(this%clat0*clat + this%slat0*slat*clon)/clatr
                         srot(i,j,k)= this%slat0*slon/clatr
                      endif rotpole
                   else
                      crot(i,j,k)=1
                      srot(i,j,k)=0
                   endif if_grid_winds
                endif rot
                xy: if(present(xpoints)) then
                   xptf=(rlonr-this%rlonlr)/dlons+ids
                   yptf=(rlatr-this%rlatlr)/dlats+jds
                   okpoint: if(xptf>=xmin .and. xptf<=xmax .and. &
                               yptf>=ymin .and. yptf<=ymax) then
                      xpoints(i,j,k)=is1+(xptf-(yptf-this%kscan))/2
                      ypoints(i,j,k)=    (xptf+(yptf-this%kscan))/2
                   else
                      ! !$OMP CRITICAL
                      ! if(.not.sentrange) then
                      !    print 44, xmin,xmax,ymin,ymax
                      !    sentrange=.true.
                      ! endif
                      ! !$OMP END CRITICAL
                      !print 33, i,j, rlonr,rlatr, this%rlonlr,this%rlatlr, xptf,yptf
                      badpoints=badpoints+1
                      xpoints(i,j,k)=badpoint
                      ypoints(i,j,k)=badpoint
                   endif okpoint
                endif xy
             enddo bigi
          enddo bigj
          !$OMP END PARALLEL DO
       enddo bigk
401    format('  Total ',I0,' of ',I0,' points outside domain in proj_nmme%locate.')
       if(badpoints>0) then
          print 401,badpoints, &
               (ipe-ips+1)*(jpe-jps+1)*(kpe-kps+1)
       endif
    end if findpoint
  end subroutine locate

  subroutine latlon_bounds(this,lat1,latmid,lat2,lon1,lonmid,lon2)
    ! Calculate a bounding rectangle in lat-lon space, and a center
    ! point in lat-lon space.  All longitudes are within +/- 180
    ! degrees of lon0 (projection center).
    class(proj_nmme), intent(inout) :: this
    real, intent(inout) :: lat1,latmid,lat2,lon1,lonmid,lon2

    double precision :: DEGRAD, RADDEG, dlm,dph,phi0,cos_phi0,sin_phi0, &
         xwb,xsb,mid,lmbd0,tlm01,tlm02,tph1,tph2,stph1,ctph1,stph2,     &
         ctph2,tlm1,tlm2,sph1,sph2,outlat1,outlat2,clm1,clm2,lm1,lm2,   &
         outlon1,outlon2,tlm0,tph,stph,ctph,tlm,sph,outlat,outlon,lm,   &
         clm
    INTEGER :: hgrid,i,j, jds,jde,ids,ide

    ids=1
    jds=1
    ide=this%nx
    jde=this%ny

    DEGRAD=default_pi/180
    RADDEG=180/default_pi

    lon1=1e19
    lat1=1e19
    lat2=-1e19
    lon2=-1e19

    dlm=this%dlon*DEGRAD
    dph=this%dlat*DEGRAD
    phi0       = this%lat0*DEGRAD
    sin_phi0   = sin(phi0)
    cos_phi0   = cos(phi0)
    hgrid      = 1-this%kscan
    !xwb        = -dlm*(this%nx)  
    xwb        = (mod(3600+180+this%rlonlr,360.)-180)*DEGRAD
    !xsb        = -dph*(this%ny/2.)
    xsb        = this%rlatlr*DEGRAD
    mid        = this%lon0          ! fit lons within mid +/- 360
    lmbd0      = this%lon0*DEGRAD

    ! ------------------------------------------------------------------
    !  Process Y start and end boundaries

    tlm01=xwb-2*dlm+mod(jds+hgrid,2)*dlm
    tlm02=xwb-2*dlm+mod(jde+hgrid,2)*dlm

    tph1=xsb
    tph2=xsb+(jde-jds)*dph
    stph1=sin(tph1)
    ctph1=cos(tph1)
    stph2=sin(tph2)
    ctph2=cos(tph2)

    !$OMP PARALLEL DO                                                                     &
    !$OMP PRIVATE(i,tlm1,tlm2,sph1,sph2,outlat1,outlat2,clm1,clm2,lm1,lm2,outlon1,outlon2)&
    !$OMP REDUCTION(min:lat1) REDUCTION(min:lon1) REDUCTION(max:lat2) REDUCTION(max:lon2)
    do i=ids,ide
       ! 1 = Y start, 2 = Y end
       tlm1=tlm01+i*2*dlm
       tlm2=tlm02+i*2*dlm

       sph1=cos_phi0*stph1+sin_phi0*ctph1*cos(tlm1)
       sph2=cos_phi0*stph2+sin_phi0*ctph2*cos(tlm2)

       outlat1=asin(sph1)
       outlat2=asin(sph2)

       clm1=max(-1.0,min(1.0,ctph1*cos(tlm1)/(cos(outlat1)*cos_phi0)-tan(outlat1)*tan(phi0)))
       clm2=max(-1.0,min(1.0,ctph2*cos(tlm2)/(cos(outlat2)*cos_phi0)-tan(outlat2)*tan(phi0)))

       lm1=acos(clm1)
       lm2=acos(clm2)

       if(tlm1>0.) lm1=-lm1
       if(tlm2>0.) lm2=-lm2

       outlon1 = mod(3600-this%lon0+180 + RADDEG*(lmbd0 - lm1), 360.) + this%lon0-180
       outlon2 = mod(3600-this%lon0+180 + RADDEG*(lmbd0 - lm2), 360.) + this%lon0-180
       outlat1 = outlat1 * RADDEG
       outlat2 = outlat2 * RADDEG

       lat1=min(lat1,min(outlat1,outlat2))
       lon1=min(lon1,min(outlon1,outlon2))
       lat2=max(lat2,max(outlat1,outlat2))
       lon2=max(lon2,max(outlon1,outlon2))
    enddo

    ! ------------------------------------------------------------------
    !  Process X start and end boundaries

    !$OMP PARALLEL DO                                                    &
    !$OMP PRIVATE(i,j,tlm0,tph,stph,tlm1,tlm2,sph1,sph2,outlat1,outlat2, &
    !$OMP         clm1,clm2,lm1,lm2,outlon1,outlon2,tlm,sph,clm,lm,      &
    !$OMP         ctph,outlat,outlon)                                    &
    !$OMP REDUCTION(min:lat1) REDUCTION(min:lon1) REDUCTION(max:lat2)    &
    !$OMP REDUCTION(max:lon2)
    do j=jds,jde
       tlm0=xwb-2*dlm+mod(j+hgrid,2)*dlm
       tph=xsb+(j-1)*dph
       stph=sin(tph)
       ctph=cos(tph)

       tlm1=tlm0+ids*2*dlm
       tlm2=tlm0+ide*2*dlm

       sph1=cos_phi0*stph+sin_phi0*ctph*cos(tlm1)
       sph2=cos_phi0*stph+sin_phi0*ctph*cos(tlm2)

       outlat1=asin(sph1)
       outlat2=asin(sph2)

       clm1=max(-1.0,min(1.0,ctph*cos(tlm1)/(cos(outlat1)*cos_phi0)-tan(outlat1)*tan(phi0)))
       clm2=max(-1.0,min(1.0,ctph*cos(tlm2)/(cos(outlat2)*cos_phi0)-tan(outlat2)*tan(phi0)))

       lm1=acos(clm1)
       lm2=acos(clm2)

       if(tlm1>0.) lm1=-lm1
       if(tlm2>0.) lm2=-lm2

       outlon1 = mod(3600-this%lon0+180 + RADDEG*(lmbd0 - lm1), 360.) + this%lon0-180
       outlon2 = mod(3600-this%lon0+180 + RADDEG*(lmbd0 - lm2), 360.) + this%lon0-180
       outlat1 = outlat1 * RADDEG
       outlat2 = outlat2 * RADDEG

       lat1=min(lat1,min(outlat1,outlat2))
       lon1=min(lon1,min(outlon1,outlon2))
       lat2=max(lat2,max(outlat1,outlat2))
       lon2=max(lon2,max(outlon1,outlon2))

       if(j==(jds+jde)/2) then
          ! Store domain midpoint
          i=(ids+ide)/2
          tlm=tlm0+i*2*dlm
          sph=cos_phi0*stph+sin_phi0*ctph*cos(tlm)
          outlat=asin(sph)

          clm=max(-1.,min(1.,ctph*cos(tlm)/(cos(outlat)*cos_phi0)-tan(outlat)*tan(phi0)))
          lm=acos(clm)
          if(tlm>0.) lm=-lm
          outlon = mod(3600-this%lon0+180 + RADDEG*(lmbd0 - lm), 360.) + this%lon0-180
          outlat = outlat*RADDEG

          ! Only one thread should ever get to the middle j index, so
          ! no locking is necessary before these two assignments:
          latmid=outlat
          lonmid=outlon
       endif
    enddo
  end subroutine latlon_bounds

  subroutine projinfo(this, &
                    ids,ide,jds,jde,kds,kde, &
                    ims,ime,jms,jme,kms,kme, &
                    ips,ipe,jps,jpe,kps,kpe, &
                    lat,lon,crot,srot,dx,dy)
    ! Note use of double precision.  This is needed to avoid huge
    ! errors near the domain center due to division by cosine (or
    ! adding result of a tangent).
    class(proj_nmme), intent(in) :: this
    real, intent(out), dimension(ims:ime,jms:jme,kms:kme), optional :: &
         lat,lon,crot,srot,dx,dy
    integer, intent(in) :: ids,ide,jds,jde,kds,kde, &
                           ims,ime,jms,jme,kms,kme, &
                           ips,ipe,jps,jpe,kps,kpe

    double precision :: dlm,dph,phi0,sin_phi0,cos_phi0,xwb,xsb,mid,lmbd0
    double precision :: tlm0,tph,stph,ctph,outlat,sph,clm,lm,outlon
    double precision :: big_denom,relm,sin_alpha,cos_alpha,tlm
    double precision :: DEGRAD, RADDEG
    integer :: hgrid,i,j,k

    DEGRAD=default_pi/180
    RADDEG=180/default_pi

    dlm=this%dlon*DEGRAD
    dph=this%dlat*DEGRAD
    phi0       = this%lat0*DEGRAD
    sin_phi0   = sin(phi0)
    cos_phi0   = cos(phi0)
    hgrid      = 1-this%kscan
    !xwb        = -dlm*(this%nx)  
    xwb        = (mod(3600+180+this%rlonlr,360.)-180)*DEGRAD
    !xsb        = -dph*(this%ny/2.)
    xsb        = this%rlatlr*DEGRAD
    mid        = this%lon0          ! fit lons within mid +/- 360
    lmbd0      = this%lon0*DEGRAD
    ! print *,'sb in deg is ',xsb*RADDEG
    ! print *,'wb in deg is ',xwb*RADDEG
    ! print *,'lmbd0 in deg ',lmbd0*RADDEG
    ! print *,'phi0 in deg  ',phi0*RADDEG 
    ! print *,'dlm in deg   ',dlm*RADDEG
    ! print *,'dph in deg   ',dph*RADDEG
    ! print *,'lmbd0=',lmbd0
    if(present(dx)) then
       do k=kps,kpe
          !$OMP PARALLEL DO PRIVATE(i,j)
          do j=jps,jpe
             do i=ips,ipe
                dx(i,j,k)=0
                dy(i,j,k)=0
             enddo
          enddo
          !$OMP END PARALLEL DO
       enddo
    endif
   if(present(lat) .or. present(lon) .or. present(crot) .or. present(srot)) then
       !$OMP PARALLEL DO DEFAULT(SHARED) SCHEDULE(STATIC)           &
       !$OMP PRIVATE(i,j,tlm0,tph,stph,ctph,tlm,sph,outlat,clm,lm,outlon,relm,big_denom,sin_alpha,cos_alpha)
       do j=jps,jpe
          ! Note the only h-vs-v grid difference is in mod(j+hgrid,2)
          tlm0=xwb-2*dlm+mod(j+hgrid,2)*dlm
          tph=xsb+(j-1)*dph
          stph=sin(tph)
          ctph=cos(tph)
          do i=ips,ipe
             tlm=tlm0+i*2*dlm
             sph=cos_phi0*stph+sin_phi0*ctph*cos(tlm)
             outlat=asin(sph)
             !clm=max(-1.0,min(1.0, &
             !     (ctph*cos(tlm) - sin(outlat)*sin_phi0)/(cos(outlat)*cos_phi0)))
             clm=max(-1.0,min(1.0, &
                  ctph*cos(tlm)/(cos(outlat)*cos_phi0) - &
                  tan(outlat)*tan(phi0)))
             lm=acos(clm)
             if(tlm>0.) lm=-lm
             outlon = lmbd0 + -lm ! lmbd0-lm ! -(-lmbd0+lm)

             if(present(lat)) then
                ! Store the longitude, fitting it between +/- 180 degrees of mid:
                lon(i,j,kps)=outlon*RADDEG ! mod(3600+outlon*RADDEG,360.)
                
                ! Store latitude unmodified:
                lat(i,j,kps)=outlat*RADDEG
             endif
             if(present(crot)) then
                grid_wind: if(.not.this%earth_winds) then
                   relm=outlon-lmbd0
                   big_denom=sqrt(1. - (cos_phi0*sin(outlat) - sin_phi0*cos(outlat)*cos(relm))**2)
                   sin_alpha=sin_phi0*sin(relm)/big_denom
                   cos_alpha=(cos_phi0*cos(outlat)+sin_phi0*sin(outlat)*cos(relm))/big_denom
                   crot(i,j,kps)=cos_alpha
                   srot(i,j,kps)=sin_alpha
                else
                   crot(i,j,kps)=1
                   srot(i,j,kps)=0
                endif grid_wind
             endif
          enddo
       enddo
       !$OMP END PARALLEL DO
    end if
       
    if(present(crot)) then
       kcopy: do k=kps+1,kpe
          !$OMP PARALLEL DO PRIVATE(i,j)
          jcopy: do j=jps,jpe
             icopy: do i=ips,ipe
                crot(i,j,k) = crot(i,j,kps)
                srot(i,j,k) = srot(i,j,kps)
             enddo icopy
          enddo jcopy
          !$OMP END PARALLEL DO
       enddo kcopy
    endif

    if(present(lat)) then
       kcopy2: do k=kps+1,kpe
          !$OMP PARALLEL DO PRIVATE(i,j)
          jcopy2: do j=jps,jpe
             icopy2: do i=ips,ipe
                lat(i,j,k) = lat(i,j,kps)
                lon(i,j,k) = lon(i,j,kps)
             enddo icopy2
          enddo jcopy2
          !$OMP END PARALLEL DO
       enddo kcopy2
    endif
  end subroutine projinfo

  subroutine near(this,xpoints,ypoints,irad, &
                  inear,jnear,weight, &
                  ids,ide,jds,jde,kds,kde, &
                  ims,ime,jms,jme,kms,kme, &
                  ips,ipe,jps,jpe,kps,kpe, nnear)
    class(proj_nmme), intent(in) :: this
    integer, intent(in) :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe
    integer, intent(in) :: irad,nnear
    integer, intent(out) :: inear(nnear,ims:ime,jms:jme,kms:kme), jnear(nnear,ims:ime,jms:jme,kms:kme)
    real, intent(in) :: ypoints(ims:ime,jms:jme,kms:kme),xpoints(ims:ime,jms:jme,kms:kme)
    real, intent(out) :: weight(nnear,ims:ime,jms:jme,kms:kme)
    real, parameter :: repsilon = 0.999, epsilon=0.001
    real :: x,y, xw, yw
    integer :: x1,x2, y1,y2, i,j,k, o, is1, kscan, xt,yt
    logical :: x1ok,x2ok, y1ok,y2ok

    if(irad/=1) then
       call fail('ABORT: only irad=1 (linear interpolation) is supported in proj_nmme_module subroutine "near"')
    endif

    if(nnear<4) then
       call fail('ABORT: in proj_nmme_module "near" subroutine, argument nnear<4')
    endif

    is1=this%is1
    !print *,'is1 is ',is1
    kscan=this%kscan
10  format('At ',I0,',',I0,' unrot ',F0.3,',',F0.3,' to ',I0,',',I0,' for ',A)    
15  format('At ',I0,',',I0,' ',A,' x1=',I0,' x2=',I0,' xw=',F0.3)
16  format('At ',I0,',',I0,' ',A,' y1=',I0,' y2=',I0,' yw=',F0.3)
22  format('At ',I0,',',I0,' input is x=',F0.6,' y=',F0.6)
    do k=kps,kpe
       !$OMP PARALLEL DO PRIVATE(i,j,x,y,x1,x2,xw,y1,y2,yw,x1ok,x2ok,y1ok,y2ok,o,xt,yt)
       do j=jps,jpe
          do i=ips,ipe
             x=xpoints(i,j,k)
             y=ypoints(i,j,k)
             !print 22,i,j,x,y

             x1=floor(x)
             x2=ceiling(x)
             xw=abs(x-x1)

             y1=floor(y)
             y2=ceiling(y)
             yw=abs(y-y1)

             !print 15, i,j, 'start', x1,x2,xw
             !print 16, i,j, 'start', y1,y2,yw

             if(xw>repsilon) x1=x2
             if(xw< epsilon) x2=x1
             if(x1==x2) xw=0.5

             if(yw>repsilon) y1=y2
             if(yw< epsilon) y2=y1
             if(y1==y2) yw=0.5

             !print 15, i,j, 'after', x1,x2,xw
             !print 16, i,j, 'after', y1,y2,yw

             x1ok = (x1>=1 .and. x1<=this%nx)
             x2ok = (x2>=1 .and. x2<=this%nx)
             y1ok = (y1>=1 .and. y1<=this%ny)
             y2ok = (y2>=1 .and. y2<=this%ny)

             o=1

             xt=(y1+(x1-is1)+1)/2
             yt=y1-(x1-is1)+kscan
             !print 10,i,j,x,y,xt,yt,'x1y1'
             if(xt>=1 .and. xt<=this%nx .and. yt>=1 .and. yt<=this%ny) then
                weight(o,i,j,k)  = (1-xw)*(1-yw)
                inear (o,i,j,k)  = xt
                jnear (o,i,j,k)  = yt
                o=o+1
             endif

             xt=(y2+(x1-is1)+1)/2
             yt=y2-(x1-is1)+kscan
             !print 10,i,j,x,y,xt,yt,'x1y2'
             if(xt>=1 .and. xt<=this%nx .and. yt>=1 .and. yt<=this%ny) then
                weight(o,i,j,k)  = (1-xw)*   yw 
                inear (o,i,j,k)  = xt
                jnear (o,i,j,k)  = yt
                o=o+1
             endif

             xt=(y1+(x2-is1)+1)/2
             yt=y1-(x2-is1)+kscan
             !print 10,i,j,x,y,xt,yt,'x2y1'
             if(xt>=1 .and. xt<=this%nx .and. yt>=1 .and. yt<=this%ny) then
                weight(o,i,j,k)  =    xw *(1-yw)
                inear (o,i,j,k)  = xt
                jnear (o,i,j,k)  = yt
                o=o+1
             endif

             xt=(y2+(x2-is1)+1)/2
             yt=y2-(x2-is1)+kscan
             !print 10,i,j,x,y,xt,yt,'x2y2'
             if(xt>=1 .and. xt<=this%nx .and. yt>=1 .and. yt<=this%ny) then
                weight(o,i,j,k)  =    xw *   yw 
                inear (o,i,j,k)  = xt
                jnear (o,i,j,k)  = yt
                o=o+1
             endif

             ! Fill unused points
             if(o<=nnear) then
99              format('At ',I0,',',I0,' fill ',I0,' points with badval')
                !print 99,i,j,nnear-o+1
             endif
             do while(o<=nnear)
                weight(o,i,j,k) = 0
                inear (o,i,j,k) = badpoint
                jnear (o,i,j,k) = badpoint
                o=o+1
             enddo
          enddo
       enddo
       !$OMP END PARALLEL DO
    enddo
  end subroutine near

end module proj_nmme_module

! ######################################################################
! PROJECTION_MODULE - convenience module that includes all projections
! in one module.
! ######################################################################

module projection_module
  use proj_base_module
  use proj_latlon_module
  use proj_nmme_module
end module projection_module
