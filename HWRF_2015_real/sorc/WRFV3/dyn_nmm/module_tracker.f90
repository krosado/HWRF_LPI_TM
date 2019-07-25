module module_tracker
  implicit none
  private

  public :: ncep_tracker_center, ncep_tracker_init, update_tracker_post_move

  real, parameter :: invE=0.36787944117 

  
  real,parameter :: searchrad_6=250.0 
  real,parameter :: searchrad_7=200.0 
  integer, parameter :: maxtp=11 
  real, parameter :: uverrmax = 225.0  
  real, parameter :: ecircum = 40030.2  
  
  real, parameter :: rads_vmag=120.0 
  real, parameter :: err_reg_init=300.0 
  real, parameter :: err_reg_max=225.0 

  real, parameter :: errpmax=485.0 
  real, parameter :: errpgro=1.25 

  real, parameter :: max_wind_search_radius=searchrad_7 
  real, parameter :: min_mlsp_search_radius=searchrad_7 

  
  real, parameter :: km2nmi = 0.539957, kn2mps=0.514444, mps2kn=1./kn2mps, pi180=0.01745329251
contains

  
  
  
  

  character(1) function get_lat_ns(lat)
    
    implicit none ; real lat
    if(lat>=0) then
       get_lat_ns='N'
    else
       get_lat_ns='S'
    endif
  end function get_lat_ns
  character(1) function get_lon_ew(lon)
    
    implicit none ; real lon
    if(lon>=0) then
       get_lon_ew='E'
    else
       get_lon_ew='W'
    endif
  end function get_lon_ew

  subroutine ncep_tracker_init(grid)
    
    use module_domain, only: domain
    implicit none
    type(domain), intent(inout) :: grid
    call wrf_message('ncep_tracker_init')
    grid%track_stderr_m1=-99.9
    grid%track_stderr_m2=-99.9
    grid%track_stderr_m3=-99.9
    grid%track_n_old=0
    grid%track_old_lon=0
    grid%track_old_lat=0
    grid%track_old_ntsd=0

    grid%tracker_angle=0
    grid%tracker_fixlon=-999.0
    grid%tracker_fixlat=-999.0
    grid%tracker_ifix=-99
    grid%tracker_jfix=-99
    grid%tracker_havefix=.false.
    grid%tracker_gave_up=.false.
    grid%tracker_pmin=-99999.
    grid%tracker_vmax=-99.
    grid%tracker_rmw=-99.

    grid%track_have_guess=.false.
    grid%track_guess_lat=-999.0
    grid%track_guess_lon=-999.0
  end subroutine ncep_tracker_init

  subroutine ncep_tracker_center(grid)
    
    
    
    

    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    implicit none
    type(domain), intent(inout) :: grid
    character*255 :: message

    integer :: IDS,IDE,JDS,JDE,KDS,KDE
    integer :: IMS,IME,JMS,JME,KMS,KME
    integer :: IPS,IPE,JPS,JPE,KPS,KPE

    CALL get_ijk_from_grid (  grid ,      &
         ids, ide, jds, jde, kds, kde,    &
         ims, ime, jms, jme, kms, kme,    &
         ips, ipe, jps, jpe, kps, kpe    )

    call ntc_impl(grid,                &
         ids, ide, jds, jde, kds, kde,    &
         ims, ime, jms, jme, kms, kme,    &
         ips, ipe, jps, jpe, kps, kpe    )
  end subroutine ncep_tracker_center

  subroutine ntc_impl(grid, &
       IDS,IDE,JDS,JDE,KDS,KDE, &
       IMS,IME,JMS,JME,KMS,KME, &
       IPS,IPE,JPS,JPE,KPS,KPE)
    
    

    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid

    use module_dm, only: wrf_dm_sum_real

    implicit none
    logical, external :: wrf_dm_on_monitor
    type(domain), intent(inout) :: grid
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE

    real :: dxdymean, sum
    integer :: i,j, iweights,ip


    integer :: iguess, jguess 
    real :: latguess, longuess 

    integer :: iuvguess, juvguess 
    real :: srsq
    integer :: ifinal,jfinal
    real :: latfinal,lonfinal
    integer :: ierr
    integer :: icen(maxtp), jcen(maxtp) 
    real :: loncen(maxtp), latcen(maxtp) 
    logical :: calcparm(maxtp) 
    real :: max_wind,min_pres 
    real :: rcen(maxtp) 
    character*255 :: message
    logical :: north_hemi 
    logical :: have_guess 
    real :: guessdist,guessdeg 
    real :: latnear, lonnear 

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    call wrf_message('ncep_tracker_center')

    
    icen=-99
    jcen=-99
    latcen=9e9
    loncen=9e9
    rcen=9e9
    calcparm=.false.
    if(grid%vortex_tracker==6) then
       srsq=searchrad_6*searchrad_6*1e6
    else
       srsq=searchrad_7*searchrad_7*1e6
    endif

    
    have_guess=grid%track_have_guess
    if(have_guess) then
       
       longuess=grid%track_guess_lon
       latguess=grid%track_guess_lat
       call get_nearest_lonlat(grid,iguess,jguess,ierr,longuess,latguess, &
            ids,ide, jds,jde, kds,kde, &
            ims,ime, jms,jme, kms,kme, &
            ips,ipe, jps,jpe, kps,kpe,     lonnear, latnear)
       if(ierr==0) then
          call calcdist(longuess,latguess, lonnear,latnear, guessdist,guessdeg)
          if(guessdist*1e3>3*grid%dy) then
108          format('WARNING: guess lon=',F0.3,',lat=',F0.3, &
                  ' too far (',F0.3,'km) from nearest point lon=',F0.3,',lat=',F0.3, &
                  '.  Will use domain center as first guess.')
             write(message,108) grid%track_guess_lon,grid%track_guess_lat, &
                  guessdist,lonnear,latnear
             call wrf_message(message)
             have_guess=.false. 
          else
             latguess=latnear
             longuess=lonnear
          endif
       else
          have_guess=.false. 
109       format('WARNING: guess lon=',F0.3,',lat=',F0.3, &
                  ' does not exist in this domain.  Will use domain center as first guess.')
          write(message,109) grid%track_guess_lon,grid%track_guess_lat
          call wrf_message(message)
       endif
   endif

   
   
    if(grid%vortex_tracker==6 .or. .not.have_guess) then
       
       
       
       
       iguess=ide/2
       jguess=jde/2
       if(grid%vortex_tracker==7) then
          call wrf_message('Using domain center as first guess since no valid first guess is available.')
       endif
       call get_lonlat(grid,iguess,jguess,longuess,latguess,ierr, &
            ids,ide, jds,jde, kds,kde, &
            ims,ime, jms,jme, kms,kme, &
            ips,ipe, jps,jpe, kps,kpe)
       if(ierr/=0) then
          call wrf_error_fatal3("<stdin>",234,&
"ERROR: center of domain is not inside the domain")
       endif
       have_guess=.true.
    endif

    if(.not.have_guess) then
       call wrf_error_fatal3("<stdin>",241,&
"INTERNAL ERROR: No first guess is available (should never happen).")
    endif

    north_hemi = latguess>0.0

    
    sum=0
    do j=jps,min(jde-1,jpe)
       do i=ips,min(ide-1,ipe)
          sum=sum+grid%dx_nmm(i,j)
       enddo
    enddo

    sum=wrf_dm_sum_real(sum)

    dxdymean=0.5*(grid%dy_nmm + sum/( (ide-ids) * (jde-jds) ))/1000.0
33  format ('dxdymean=',F0.3,' dx=',F0.3,' dy=',F0.3,' sum=',F0.3,' count=',I0)
    
    

    
    call find_center(grid,grid%p850rv,grid%sp850rv,srsq, &
         icen(1),jcen(1),rcen(1),calcparm(1),loncen(1),latcen(1),dxdymean,'zeta', &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE, north_hemi=north_hemi)
    call find_center(grid,grid%p700rv,grid%sp700rv,srsq, &
         icen(2),jcen(2),rcen(2),calcparm(2),loncen(2),latcen(2),dxdymean,'zeta', &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE, north_hemi=north_hemi)
    call find_center(grid,grid%p850z,grid%sp850z,srsq, &
         icen(7),jcen(7),rcen(7),calcparm(7),loncen(7),latcen(7),dxdymean,'hgt', &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)
    call find_center(grid,grid%p700z,grid%sp700z,srsq, &
         icen(8),jcen(8),rcen(8),calcparm(8),loncen(8),latcen(8),dxdymean,'hgt', &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)
    call find_center(grid,grid%membrane_mslp,grid%smslp,srsq, &
         icen(9),jcen(9),rcen(9),calcparm(9),loncen(9),latcen(9),dxdymean,'slp', &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)
    call find_center(grid,grid%m10rv,grid%sm10rv,srsq, &
         icen(11),jcen(11),rcen(11),calcparm(11),loncen(11),latcen(11),dxdymean,'zeta', &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE, north_hemi=north_hemi)

    
    call get_uv_guess(grid,icen,jcen,loncen,latcen,calcparm, &
         iguess,jguess,longuess,latguess,iuvguess,juvguess, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)

    
    windmin: if(grid%vortex_tracker==6) then
       call find_center(grid,grid%p850wind,grid%sp850wind,srsq, &
            icen(3),jcen(3),rcen(3),calcparm(3),loncen(3),latcen(3),dxdymean,'wind', &
            IDS,IDE,JDS,JDE,KDS,KDE, &
            IMS,IME,JMS,JME,KMS,KME, &
            IPS,IPE,JPS,JPE,KPS,KPE, &
            iuvguess=iuvguess, juvguess=juvguess)
       call find_center(grid,grid%p700wind,grid%sp700wind,srsq, &
            icen(5),jcen(5),rcen(5),calcparm(5),loncen(5),latcen(5),dxdymean,'wind', &
            IDS,IDE,JDS,JDE,KDS,KDE, &
            IMS,IME,JMS,JME,KMS,KME, &
            IPS,IPE,JPS,JPE,KPS,KPE, &
            iuvguess=iuvguess, juvguess=juvguess)
       call find_center(grid,grid%m10wind,grid%sm10wind,srsq, &
            icen(10),jcen(10),rcen(10),calcparm(10),loncen(10),latcen(10),dxdymean,'wind', &
            IDS,IDE,JDS,JDE,KDS,KDE, &
            IMS,IME,JMS,JME,KMS,KME, &
            IPS,IPE,JPS,JPE,KPS,KPE, &
            iuvguess=iuvguess, juvguess=juvguess)
    else
       call get_uv_center(grid,grid%p850wind, &
            icen(3),jcen(3),rcen(3),calcparm(3),loncen(3),latcen(3),dxdymean,'wind', &
            IDS,IDE,JDS,JDE,KDS,KDE, &
            IMS,IME,JMS,JME,KMS,KME, &
            IPS,IPE,JPS,JPE,KPS,KPE, &
            iuvguess=iuvguess, juvguess=juvguess)
       call get_uv_center(grid,grid%p700wind, &
            icen(5),jcen(5),rcen(5),calcparm(5),loncen(5),latcen(5),dxdymean,'wind', &
            IDS,IDE,JDS,JDE,KDS,KDE, &
            IMS,IME,JMS,JME,KMS,KME, &
            IPS,IPE,JPS,JPE,KPS,KPE, &
            iuvguess=iuvguess, juvguess=juvguess)
       call get_uv_center(grid,grid%m10wind, &
            icen(10),jcen(10),rcen(10),calcparm(10),loncen(10),latcen(10),dxdymean,'wind', &
            IDS,IDE,JDS,JDE,KDS,KDE, &
            IMS,IME,JMS,JME,KMS,KME, &
            IPS,IPE,JPS,JPE,KPS,KPE, &
            iuvguess=iuvguess, juvguess=juvguess)
    endif windmin

    
    call fixcenter(grid,icen,jcen,calcparm,loncen,latcen, &
         iguess,jguess,longuess,latguess, &
         ifinal,jfinal,lonfinal,latfinal, &
         north_hemi, &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         ips,ipe, jps,jpe, kps,kpe)

    grid%tracker_fixes=0
    do ip=1,maxtp
       if(calcparm(ip)) then
300       format('Parameter ',I0,': i=',I0,' j=',I0,' lon=',F0.2,' lat=',F0.2)
          
          if(icen(ip)>=ips .and. icen(ip)<=ipe &
               .and. jcen(ip)>=jps .and. jcen(ip)<=jpe) then
             grid%tracker_fixes(icen(ip),jcen(ip))=ip
          endif
       else
301       format('Parameter ',I0,' invalid')
          
       endif
    enddo

    if(iguess>=ips .and. iguess<=ipe .and. jguess>=jps .and. jguess<=jpe) then
       grid%tracker_fixes(iguess,jguess)=-1
201    format('First guess: i=',I0,' j=',I0,' lon=',F0.2,' lat=',F0.2)
       
    endif

    if(iuvguess>=ips .and. iuvguess<=ipe .and. juvguess>=jps .and. juvguess<=jpe) then
       grid%tracker_fixes(iuvguess,juvguess)=-2
202    format('UV guess: i=',I0,' j=',I0)
       
    endif

1000 format('Back with final lat/lon at i=',I0,' j=',I0,' lon=',F0.3,' lat=',F0.3)
    

    if(ifinal>=ips .and. ifinal<=ipe .and. jfinal>=jps .and. jfinal<=jpe) then
       grid%tracker_fixes(ifinal,jfinal)=-3
203    format('Final fix: i=',I0,' j=',I0,' lon=',F0.2,' lat=',F0.2)
       
    endif

    call get_tracker_distsq(grid, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)

    call get_wind_pres_intensity(grid, &
         grid%tracker_pmin,grid%tracker_vmax,grid%tracker_rmw, &
         max_wind_search_radius, min_mlsp_search_radius, &
         lonfinal,latfinal, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)


    if(wrf_dm_on_monitor()) then

       call output_partial_atcfunix(grid, &
            IDS,IDE,JDS,JDE,KDS,KDE, &
            IMS,IME,JMS,JME,KMS,KME, &
            IPS,IPE,JPS,JPE,KPS,KPE)

    endif







       














  end subroutine ntc_impl

  subroutine get_first_ges(grid,  &
         iguess,jguess,longuess,latguess, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)
    
    
    
    
    
    
    
    
    
    
    
    
    


    use module_dm, only: wrf_dm_maxval_real

    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    use module_wrf_error, only: wrf_at_debug_level
    implicit none
    type(domain), intent(inout) :: grid
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE
    integer, intent(out) :: iguess,jguess
    real, intent(out) :: longuess,latguess

    character*255 message
    integer :: iold, inew, jold, jnew
    integer :: ifix,jfix,jrot,irot,ierr, pinky,brain, n, tsum, ntsd_plus_1, i, told
    real :: motion_grideast, motion_gridnorth, fixdx
    real :: dxeast,dynorth, xeast, ynorth
    real :: dxrot, dyrot, tracker_dt, xsum, ysum, ytsum, xtsum, xxsum, yysum, ttsum
    real :: mx, my, bx, by 
    real :: xrot,yrot
    logical :: have_motion_guess, have_line_guess

    have_motion_guess=.false.
    have_line_guess=.false.

    if(grid%tracker_havefix) then
       ifix=grid%tracker_ifix
       jfix=grid%tracker_jfix

       call mean_motion(grid, motion_grideast, motion_gridnorth, &
            IDS,IDE,JDS,JDE,KDS,KDE, &
            IMS,IME,JMS,JME,KMS,KME, &
            IPS,IPE,JPS,JPE,KPS,KPE)


       fixdx=0
       if(ifix>=ips .and. ifix<=ipe .and. jfix>=jps .and. jfix<=jpe) then
          fixdx=grid%dx_nmm(ifix,jfix)
       endif
       pinky=2 ; brain=308
       call wrf_dm_maxval_real(fixdx,pinky,brain)




       
       tracker_dt=grid%dt*grid%nphs*grid%movemin
       dxeast = motion_grideast * tracker_dt / fixdx
       dynorth = motion_gridnorth * tracker_dt / grid%dy_nmm

       
       
       
       xeast=ifix*2
       ynorth=jfix
       if(mod(jfix,2)==0) xeast=xeast+1
       jrot=nint((xeast+ynorth)/2 + (dxeast+dynorth)/2)
       irot=nint((ynorth-xeast)/2 + ((jde-1)/2) + (dynorth-dxeast)/2)

       
       iguess=irot-jrot+((jde-1)/2)
       jguess=irot+jrot-((jde-1)/2)
       if(mod(jguess,2)==0) then
          iguess=(iguess-1)/2
       else
          iguess=iguess/2
       endif

       
       have_motion_guess = .not.(iguess<ide/4 .or. iguess>ide*3/4 .or. jguess<jde/4 .or. jguess>jde*3/4)

       
    endif

    if(.not.have_motion_guess) then
       
       
       iguess=ide/2
       jguess=jde/2
       
    endif

    if(grid%track_n_old>0) then
       
       n=1
       call to_rot45_grid(grid%tracker_ifix,grid%tracker_jfix,jde,xrot,yrot)
       xsum=xrot
       ysum=yrot
       tsum=grid%ntsd
       xtsum=xsum*tsum
       xxsum=xsum*xsum
       yysum=ysum*ysum
       ytsum=ysum*tsum
       ttsum=tsum*tsum
       
       do i=1,grid%track_n_old
          call get_nearest_lonlat(grid,iold,jold,ierr, &
               grid%track_old_lon(i),grid%track_old_lat(i), &
               ids,ide, jds,jde, kds,kde, &
               ims,ime, jms,jme, kms,kme, &
               ips,ipe, jps,jpe, kps,kpe)
          if(ierr==0) then
             
             call to_rot45_grid(iold,jold,jde,xrot,yrot)
             n=n+1
             xsum=xsum+xrot
             ysum=ysum+yrot
             told=grid%track_old_ntsd(i)
             tsum=tsum+told
             xtsum=xtsum+xrot*told
             xxsum=xxsum+xrot*xrot
             ytsum=ytsum+yrot*told
             yysum=xxsum+yrot*yrot
             ttsum=ttsum+told*told
          endif
       enddo
       

       if(n>1) then
          ntsd_plus_1 = grid%ntsd + grid%movemin*grid%nphs
          mx=(xtsum-(xsum*tsum)/real(n))/(ttsum-(tsum*tsum)/real(n))
          my=(ytsum-(ysum*tsum)/real(n))/(ttsum-(tsum*tsum)/real(n))
          bx=(xsum-mx*tsum)/real(n)
          by=(ysum-my*tsum)/real(n)
          
          xrot=nint(mx*ntsd_plus_1+bx)
          yrot=nint(my*ntsd_plus_1+by)
          call from_rot45_grid(inew,jnew,jde,xrot,yrot)
          
          have_line_guess=.not.(inew<ide/4 .or. inew>ide*3/4 &
               .or. jnew<jde/4 .or. jnew>jde*3/4)
       else
          have_line_guess=.false.
       endif
    endif

    print_locs: if(wrf_at_debug_level(2)) then
       call get_lonlat(grid,iguess,jguess,longuess,latguess,ierr, &
            ids,ide, jds,jde, kds,kde, &
            ims,ime, jms,jme, kms,kme, &
            ips,ipe, jps,jpe, kps,kpe)
       if(ierr==0) then
          if(have_motion_guess) then
3088         format('Motion Guess: lon=',F0.3,' lat=',F0.3)
             write(message,3088) longuess,latguess
             call wrf_debug(2,message)
          else
3089         format('Motion Guess failed; use domain center: lon=',F0.3,' lat=',F0.3)
             write(message,3089) longuess,latguess
             call wrf_debug(2,message)
          endif
       else
3090      format('Motion guess ierr=',I0)
          write(message,3090) ierr
          call wrf_debug(2,message)
       endif
       if(have_line_guess) then
          call get_lonlat(grid,inew,jnew,longuess,latguess,ierr, &
               ids,ide, jds,jde, kds,kde, &
               ims,ime, jms,jme, kms,kme, &
               ips,ipe, jps,jpe, kps,kpe)
          if(ierr==0) then
3091         format('Line guess: lon=',F0.3,' lat=',F0.3)
             write(message,3091) longuess,latguess
             call wrf_debug(2,message)
          else
3092         format('Line guess ierr=',I0)
             write(message,3092) ierr
             call wrf_debug(2,message)
          endif
       endif
    endif print_locs

    if(have_line_guess) then
       if(have_motion_guess) then
          call wrf_debug(1,'get_first_ges: have MOTION and LINE guesses')
          iguess=(iguess+inew)/2
          jguess=(jguess+jnew)/2
       else
          call wrf_debug(1,'get_first_ges: have LINE guess only')
          iguess=inew
          jguess=jnew
       endif
    elseif(have_motion_guess) then
       call wrf_debug(1,'get_first_ges: have MOTION guess only')
    else
       call wrf_debug(1,'get_first_ges: have no guesses; will use domain center')
    endif

    
    call get_lonlat(grid,iguess,jguess,longuess,latguess,ierr, &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         ips,ipe, jps,jpe, kps,kpe)
    if(ierr/=0) then
       
       call wrf_error_fatal3("<stdin>",649,&
"ERROR: domain is not inside the domain in get_first_ges (!?)")
    endif

38  format('First guess: i=',I0,' j=',I0,' lat=',F8.3,' lon=',F8.3)
    write(message,38) iguess,jguess,latguess,longuess
    call wrf_message(message)
  end subroutine get_first_ges

  subroutine store_old_fixes(grid,  &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)
    
    
    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    implicit none
    type(domain), intent(inout) :: grid
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE
    integer i
    if(grid%tracker_havefix) then
       
       if(grid%track_n_old>0) then
          
          do i=1,grid%num_old_fixes-1
             grid%track_old_lon(i+1)=grid%track_old_lon(i)
             grid%track_old_lat(i+1)=grid%track_old_lat(i)
             grid%track_old_ntsd(i+1)=grid%track_old_ntsd(i)
          enddo
       endif
       grid%track_old_lon(1)=grid%tracker_fixlon
       grid%track_old_lat(1)=grid%tracker_fixlat
       grid%track_old_ntsd(1)=grid%ntsd
       grid%track_n_old=min(grid%num_old_fixes,grid%track_n_old+1)
       
    endif
  end subroutine store_old_fixes

  subroutine to_rot45_grid(i,j,jde,x,y)
    implicit none
    integer, intent(in) :: i,j,jde
    real, intent(inout) :: x,y
    real :: a,b
    a=i*2
    b=j
    if(mod(j,2)==0) a=a+1
    x=(a+b)/2
    y=(b-a)/2+((jde-1)/2)
  end subroutine to_rot45_grid

  subroutine from_rot45_grid(i,j,jde,x,y)
    implicit none
    integer, intent(inout) :: i,j
    integer, intent(in) :: jde
    real, intent(in) :: x,y
    i=x-y+((jde-1)/2)
    j=x+y-((jde-1)/2)
    if(mod(j,2)==0) then
       i=(i-1)/2
    else
       i=i/2
    endif
  end subroutine from_rot45_grid

  subroutine get_nearest_lonlat(grid,iloc,jloc,ierr,lon,lat, &
               ids,ide, jds,jde, kds,kde, &
               ims,ime, jms,jme, kms,kme, &
               ips,ipe, jps,jpe, kps,kpe, &
               lonnear, latnear)
    
    

    use module_dm, only: wrf_dm_minloc_real

    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    implicit none
    type(domain), intent(inout) :: grid
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE
    integer, intent(out) :: iloc,jloc,ierr
    real, intent(in) :: lon,lat
    real :: dx,dy,d,dmin, zdummy, latmin,lonmin
    integer :: i,j,imin,jmin
    real, intent(out), optional :: latnear, lonnear

    zdummy=42
    dmin=9e9
    imin=-99
    jmin=-99
    latmin=9e9
    lonmin=9e9
    ierr=0
    do j=jps,min(jde-1,jpe)
       do i=ips,min(ide-1,ipe)
          dy=abs(lat-grid%hlat(i,j))
          dx=abs(mod(3600.+180.+(lon-grid%hlon(i,j)),360.)-180.)
          d=dx*dx+dy*dy
          if(d<dmin) then
             dmin=d
             imin=i
             jmin=j
             latmin=grid%hlat(i,j)
             lonmin=grid%hlon(i,j)
          endif
       enddo
    enddo


    call wrf_dm_minloc_real(dmin,latmin,lonmin,zdummy,imin,jmin)

    if(imin<0 .or. jmin<0) then
       ierr=-99
    else
       iloc=imin ; jloc=jmin
    endif
    if(present(latnear)) latnear=latmin
    if(present(lonnear)) lonnear=lonmin
  end subroutine get_nearest_lonlat

  subroutine output_partial_atcfunix(grid, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         ITS,ITE,JTS,JTE,KTS,KTE)
    
    
    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    implicit none
    type(domain), intent(inout) :: grid
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: ITS,ITE,JTS,JTE,KTS,KTE
    character*255 message
313 format(F11.2,", ",                                  &
           "W10 = ",F7.3," kn, PMIN = ",F8.3," mbar, ", &
           "LAT = ",F6.3,A1,", LON = ",F7.3,A1,", ",    &
           "RMW = ",F7.3," nmi")
    write(grid%outatcf_lun,313) grid%dt*grid%ntsd,                 &
         grid%tracker_vmax*mps2kn,grid%tracker_pmin/100.,          &
         abs(grid%tracker_fixlat),get_lat_ns(grid%tracker_fixlat), &
         abs(grid%tracker_fixlon),get_lon_ew(grid%tracker_fixlon), &
         grid%tracker_rmw*km2nmi
    
    
    
    
    
    
  end subroutine output_partial_atcfunix

  subroutine get_wind_pres_intensity(grid, &
       min_mslp,max_wind,rmw, &
       max_wind_search_radius, min_mlsp_search_radius, clon,clat, &
       IDS,IDE,JDS,JDE,KDS,KDE, &
       IMS,IME,JMS,JME,KMS,KME, &
       ITS,ITE,JTS,JTE,KTS,KTE)
    
    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid

    use module_dm, only: wrf_dm_maxval_real,wrf_dm_minval_real

    implicit none
    type(domain), intent(inout) :: grid
    real, intent(out) :: min_mslp,max_wind,rmw
    real, intent(in) :: max_wind_search_radius, min_mlsp_search_radius,clon,clat
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: ITS,ITE,JTS,JTE,KTS,KTE

    real :: localextreme,globalextreme, sdistsq,windsq
    real :: globallat,globallon,degrees
    integer :: locali,localj,globali,globalj,ierr,i,j

    
    
    localextreme=9e9
    locali=-1
    localj=-1
    sdistsq=min_mlsp_search_radius*min_mlsp_search_radius*1e6
    do j=jts,min(jte,jde-1)
       do i=its,min(ite,ide-1)
          if(grid%membrane_mslp(i,j)<localextreme .and. &
               grid%tracker_distsq(i,j)<sdistsq) then
             localextreme=grid%membrane_mslp(i,j)
             locali=i
             localj=j
          endif
       enddo
    enddo

    globalextreme=localextreme
    globali=locali
    globalj=localj

    call wrf_dm_minval_real(globalextreme,globali,globalj)

    min_mslp=globalextreme
    if(globali<0 .or. globalj<0) then
       call wrf_message("WARNING: No membrane mslp values found that were less than 9*10^9.")
       min_mslp=-999
    endif

    
    
    localextreme=-9e9
    locali=-1
    localj=-1
    sdistsq=max_wind_search_radius*max_wind_search_radius*1e6
    do j=jts,min(jte,jde-1)
       do i=its,min(ite,ide-1)
          if(grid%tracker_distsq(i,j)<sdistsq) then
             windsq=grid%u10(i,j)*grid%u10(i,j) + &
                    grid%v10(i,j)*grid%v10(i,j)
             if(windsq>localextreme) then
                localextreme=windsq
                locali=i
                localj=j
             endif
          endif
       enddo
    enddo
    if(localextreme>0) localextreme=sqrt(localextreme)

    globalextreme=localextreme
    globali=locali
    globalj=localj

    call wrf_dm_maxval_real(globalextreme,globali,globalj)


    call get_lonlat(grid,globali,globalj,globallon,globallat,ierr, &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         its,ite, jts,jte, kts,kte)
    if(ierr/=0) then
       call wrf_message("WARNING: Unable to find location of wind maximum.")
       rmw=-99
    else
       call calcdist(clon,clat,globallon,globallat,rmw,degrees)
    end if

    
    max_wind=globalextreme
    if(globali<0 .or. globalj<0) then
       call wrf_message("WARNING: No wind values found that were greater than -9*10^9.")
       min_mslp=-999
    endif

  end subroutine get_wind_pres_intensity

  subroutine mean_motion(grid,motion_grideast,motion_gridnorth, &
       ids,ide, jds,jde, kds,kde, &
       ims,ime, jms,jme, kms,kme, &
       its,ite, jts,jte, kts,kte)
    
    


    use module_dm, only: wrf_dm_sum_real8, wrf_dm_sum_integer

    use module_wrf_error
    USE MODULE_DOMAIN, ONLY : domain, domain_clock_get
    implicit none
    integer, intent(in) :: &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         its,ite, jts,jte, kts,kte
    type(domain), intent(in) :: grid
    real, intent(out) :: motion_grideast,motion_gridnorth
    integer :: count,i,j,ierr
    real :: distsq, dist
    real*8 :: e,n

    e=0 ; n=0 ; count=0 

    dist = min(grid%tracker_edge_dist, max(50e3, 3e3*grid%tracker_rmw))
    distsq = dist * dist

    
    
    
    

    do j=jts,min(jte,jde-1)
       do i=its,min(ite,ide-1)
          if(grid%tracker_distsq(i,j)<distsq) then
             count = count + 3
             e = e + (grid%p500u(i,j) + grid%p700u(i,j) + grid%p850u(i,j))
             n = n + (grid%p500v(i,j) + grid%p700v(i,j) + grid%p850v(i,j))
          endif
       enddo
    enddo


    e=wrf_dm_sum_real8(e)
    n=wrf_dm_sum_real8(n)
    count=wrf_dm_sum_integer(count)


    motion_grideast=e/count
    motion_gridnorth=n/count

    
    
  end subroutine mean_motion

  subroutine fixcenter(grid,icen,jcen,calcparm,loncen,latcen, &
       iguess,jguess,longuess,latguess, &
       ifinal,jfinal,lonfinal,latfinal, &
       north_hemi, &
       ids,ide, jds,jde, kds,kde, &
       ims,ime, jms,jme, kms,kme, &
       ips,ipe, jps,jpe, kps,kpe)
    
    
    
    
    
    
    
    
    
    
    
    

    
    
    
    
    
    
    

    
    
    

    
    

    use module_wrf_error
    USE MODULE_DOMAIN, ONLY : domain, domain_clock_get
    implicit none
    integer, intent(in) :: &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         ips,ipe, jps,jpe, kps,kpe
    type(domain), intent(inout) :: grid
    integer, intent(in) :: icen(maxtp), jcen(maxtp)
    real, intent(in) :: loncen(maxtp), latcen(maxtp)
    logical, intent(inout) :: calcparm(maxtp)

    integer, intent(in) :: iguess,jguess
    real, intent(in) :: latguess,longuess

    integer, intent(inout) :: ifinal,jfinal
    real, intent(inout) :: lonfinal,latfinal

    logical, intent(in), optional :: north_hemi

    character*255 :: message
    real :: errdist(maxtp),avgerr,errmax,errinit,xavg_stderr
    real :: dist,degrees, total
    real :: minutes,hours,trkerr_avg, dist_from_mean(maxtp),wsum
    integer :: ip,itot4next,iclose,count,ifound,ierr
    integer(kind=8) :: isum,jsum
    real :: irsum,jrsum,errtmp,devia,wtpos
    real :: xmn_dist_from_mean, stderr_close
    logical use4next(maxtp)

    
    call domain_clock_get(grid,minutesSinceSimulationStart=minutes)
    hours=minutes/60.

    
    if(hours<0.5) then
       errmax=err_reg_init
       errinit=err_reg_init
    else
       errmax=err_reg_max
       errinit=err_reg_max
    endif

    if(hours>4.) then
       xavg_stderr = ( grid%track_stderr_m1 + &
            grid%track_stderr_m2 + grid%track_stderr_m3 ) / 3.0
    elseif(hours>3.) then
       xavg_stderr = ( grid%track_stderr_m1 + grid%track_stderr_m2 ) / 2.0
    elseif(hours>2.) then
       xavg_stderr = grid%track_stderr_m1
    endif

    if(hours>2.) then
       errtmp = 3.0*xavg_stderr*errpgro
       errmax = max(errtmp,errinit)
       errtmp = errpmax
       errmax = min(errmax,errtmp)
    endif

    
    errdist=0.0
    use4next=.false.
    trkerr_avg=0
    itot4next=0
    iclose=0
    isum=0
    jsum=0
    ifound=0

    
    

500 format('Parm ip=',I0,' dist=',F0.3)
501 format('  too far, but discard')
    do ip=1,maxtp
       if(ip==4 .or. ip==6) then
          calcparm(ip)=.false.
          cycle
       elseif(calcparm(ip)) then
          ifound=ifound+1
          call calcdist(longuess,latguess,loncen(ip),latcen(ip),dist,degrees)
          errdist(ip)=dist
          
          if(dist<=errpmax) then
             if(ip==3 .or. ip==5 .or. ip==10) then
                use4next(ip)=.false.
                
             else
                
                use4next(ip)=.true.
                trkerr_avg=trkerr_avg+dist
                itot4next=itot4next+1
             endif
          endif
          if(dist<=errmax) then
502          format('  apply i=',I0,' j=',I0)
             
             iclose=iclose+1
             isum=isum+icen(ip)
             jsum=jsum+jcen(ip)
503          format(' added things isum=',I0,' jsum=',I0,' iclose=',I0)
             
          else
             
             calcparm(ip)=.false.
          endif
       endif
    enddo

    if(ifound<=0) then
       call wrf_message('The tracker could not find the centers for any parameters.  Thus,')
       call wrf_message('a center position could not be obtained for this storm.')
       goto 999
    endif

    if(iclose<=0) then
200    format('No storms are within errmax=',F0.1,'km of the parameters')
       
       call wrf_message(message)
       goto 999
    endif

    ifinal=real(isum)/real(iclose)
    jfinal=real(jsum)/real(iclose)

504 format(' calculated ifinal, jfinal: ifinal=',I0,' jfinal=',I0,' isum=',I0,' jsum=',I0,' iclose=',I0)
    

    call get_lonlat(grid,ifinal,jfinal,lonfinal,latfinal,ierr, &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         ips,ipe, jps,jpe, kps,kpe)
    if(ierr/=0) then
       call wrf_message('Gave up on finding the storm location due to error in get_lonlat (1).')
       goto 999
    endif

    count=0
    dist_from_mean=0.0
    total=0.0
    do ip=1,maxtp
       if(calcparm(ip)) then
          call calcdist(lonfinal,latfinal,loncen(ip),latcen(ip),dist,degrees)
          dist_from_mean(ip)=dist
          total=total+dist
          count=count+1
       endif
    enddo
    xmn_dist_from_mean=total/real(count)

    do ip=1,maxtp
       if(calcparm(ip)) then
          total=total+(xmn_dist_from_mean-dist_from_mean(ip))**2
       endif
    enddo
    if(count<2) then
       stderr_close=0.0
    else
       stderr_close=max(1.0,sqrt(1./(count-1) * total))
    endif

    if(calcparm(1) .or. calcparm(2) .or. calcparm(7) .or. &
         calcparm(8) .or. calcparm(9) .or. calcparm(11)) then
       continue
    else
       
       call wrf_message('In fixcenter, STOPPING PROCESSING for this storm.  The reason is that')
       call wrf_message('none of the fix locations for parms z850, z700, zeta 850, zeta 700')
       call wrf_message('MSLP or sfc zeta were within a reasonable distance of the guess location.')
       goto 999
    endif

    
    if(stderr_close<5.0) then
       
       stderr_close=5.0
    endif
    irsum=0
    jrsum=0
    wsum=0
    do ip=1,maxtp
       if(calcparm(ip)) then
          devia=max(1.0,dist_from_mean(ip)/stderr_close)
          wtpos=exp(-devia/3.)
          irsum=icen(ip)*wtpos+irsum
          jrsum=jcen(ip)*wtpos+jrsum
          wsum=wtpos+wsum
1100      format(' Adding parm: devia=',F0.3,' wtpos=',F0.3,' irsum=',F0.3,' jrsum=',F0.3,' wsum=',F0.3)
          
       endif
    enddo
    ifinal=nint(real(irsum)/real(wsum))
    jfinal=nint(real(jrsum)/real(wsum))
    call get_lonlat(grid,ifinal,jfinal,lonfinal,latfinal,ierr, &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         ips,ipe, jps,jpe, kps,kpe)
    if(ierr/=0) then
       call wrf_message('Gave up on finding the storm location due to error in get_lonlat (2).')
       goto 999
    endif

    
    grid%tracker_fixlon=lonfinal
    grid%tracker_fixlat=latfinal
    grid%tracker_ifix=ifinal
    grid%tracker_jfix=jfinal
    grid%tracker_havefix=.true.

1000 format('Stored lat/lon at i=',I0,' j=',I0,' lon=',F0.3,' lat=',F0.3)
    
    

    if(nint(hours) > grid%track_last_hour ) then
       
       count=0
       dist_from_mean=0.0
       total=0.0
       do ip=1,maxtp
          if(calcparm(ip)) then
             call calcdist(lonfinal,latfinal,loncen(ip),loncen(ip),dist,degrees)
             dist_from_mean(ip)=dist
             total=total+dist
             count=count+1
          endif
       enddo
       xmn_dist_from_mean=total/real(count)

       do ip=1,maxtp
          if(calcparm(ip)) then
             total=total+(xmn_dist_from_mean-dist_from_mean(ip))**2
          endif
       enddo
       if(count<2) then
          stderr_close=0.0
       else
          stderr_close=max(1.0,sqrt(1./(count-1) * total))
       endif

       grid%track_stderr_m3=grid%track_stderr_m2
       grid%track_stderr_m2=grid%track_stderr_m1
       grid%track_stderr_m1=stderr_close
       grid%track_last_hour=nint(hours)
    endif

    
    return

    
999 continue
    
    grid%tracker_ifix=ide/2
    grid%tracker_jfix=jde/2
    grid%tracker_havefix=.false.
    grid%tracker_gave_up=.true.
    call get_lonlat(grid,ifinal,jfinal,lonfinal,latfinal,ierr, &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         ips,ipe, jps,jpe, kps,kpe)
    if(ierr/=0) then
       call wrf_error_fatal3("<stdin>",1252,&
'Center of domain is not in domain (!?)')
       goto 999
    endif

    grid%tracker_fixlon=-999.0
    grid%tracker_fixlat=-999.0
    
  end subroutine fixcenter

  subroutine get_uv_guess(grid,icen,jcen,loncen,latcen,calcparm, &
       iguess,jguess,longuess,latguess,iout,jout, &
       IDS,IDE,JDS,JDE,KDS,KDE, &
       IMS,IME,JMS,JME,KMS,KME, &
       ITS,ITE,JTS,JTE,KTS,KTE)
    
    
    
    
    
    
    
    
    
    

    
    
    
    
    
    
    

    
    

    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid

    use module_dm, only: wrf_dm_sum_real

    implicit none
    type(domain), intent(inout) :: grid
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: ITS,ITE,JTS,JTE,KTS,KTE

    integer, intent(in) :: icen(maxtp), jcen(maxtp)
    real, intent(in) :: loncen(maxtp), latcen(maxtp)
    logical, intent(in) :: calcparm(maxtp)

    integer, intent(in) :: iguess,jguess
    real, intent(in) :: latguess,longuess

    integer, intent(inout) :: iout,jout
    real :: degrees,dist
    integer :: ip,ict
    integer(kind=8) :: isum,jsum

    ict=2
    isum=2*iguess
    jsum=2*jguess

    
    do ip=1,maxtp
       if ((ip > 2 .and. ip < 7) .or. ip == 10) then
          cycle   
          
       elseif(calcparm(ip)) then
          call calcdist (longuess,latguess,loncen(ip),latcen(ip),dist,degrees)
          if(dist<uverrmax) then
             if(ip==1 .or. ip==2 .or. ip==11) then
                isum=isum+2*icen(ip)
                jsum=jsum+2*jcen(ip)
                ict=ict+2
             else
                isum=isum+icen(ip)
                jsum=jsum+jcen(ip)
                ict=ict+1
             endif
          endif
       endif
    enddo

    iout=nint(real(isum)/real(ict))
    jout=nint(real(jsum)/real(ict))
  end subroutine get_uv_guess

  subroutine get_uv_center(grid,orig, &
       iout,jout,rout,calcparm,lonout,latout, &
       dxdymean,cparm, &
       IDS,IDE,JDS,JDE,KDS,KDE, &
       IMS,IME,JMS,JME,KMS,KME, &
       IPS,IPE,JPS,JPE,KPS,KPE, &
       iuvguess,juvguess) 

    use module_wrf_error

    use module_dm, only: wrf_dm_minval_real, wrf_dm_maxval_real, wrf_dm_sum_real

    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    use module_relax
    implicit none

    integer, intent(in) :: iuvguess,juvguess
    type(domain), intent(inout) :: grid
    character*(*), intent(in) :: cparm
    real, intent(in) :: dxdymean
    real, intent(inout) :: rout
    integer, intent(inout) :: iout,jout
    logical, intent(inout) :: calcparm
    real, intent(inout) :: latout,lonout
    real, intent(in) :: orig(ims:ime,jms:jme)

    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE

    integer :: icen,jcen, i,j, istart,istop, jstart,jstop, ierr
    real :: rcen, srsq

    
    istart=max(ids+1,ips)
    istop=min(ide-2,ipe)
    jstart=max(jds+2,jps)
    jstop=min(jde-3,jpe)

    
    istart=max(istart,iuvguess-nint(rads_vmag/(2.*dxdymean)))
    istop=min(istop,iuvguess+nint(rads_vmag/(2.*dxdymean)))
    jstart=max(jstart,juvguess-nint(rads_vmag/(2.*dxdymean)))
    jstop=min(jstop,juvguess+nint(rads_vmag/(2.*dxdymean)))
    
    srsq=rads_vmag*rads_vmag*1e6

    icen=-99
    jcen=-99
    rcen=9e9
    do j=jstart,jstop
       do i=istart,istop
          if(orig(i,j)<rcen .and. grid%distsq(i,j)<srsq) then
             rcen=orig(i,j)
             icen=i
             jcen=j
          endif
       enddo
    enddo


       call wrf_dm_minval_real(rcen,icen,jcen)
       


    
    resultif: if(icen==-99 .or. jcen==-99) then
       
       calcparm=.false.
       
    else
       iout=icen
       jout=jcen
       rout=rcen
       calcparm=.true.
       call get_lonlat(grid,iout,jout,lonout,latout,ierr, &
            ids,ide, jds,jde, kds,kde, &
            ims,ime, jms,jme, kms,kme, &
            ips,ipe, jps,jpe, kps,kpe) 
       if(ierr/=0) then
          
          calcparm=.false.
          return
       endif
       
    endif resultif
  end subroutine get_uv_center

  subroutine find_center(grid,orig,smooth,srsq, &
       iout,jout,rout,calcparm,lonout,latout, &
       dxdymean,cparm, &
       IDS,IDE,JDS,JDE,KDS,KDE, &
       IMS,IME,JMS,JME,KMS,KME, &
       IPS,IPE,JPS,JPE,KPS,KPE, &
       iuvguess,juvguess,north_hemi)
    
    

    
    
    
    

    

    
    
    
    
    
    
    
    
    
    
    

    

    
    
    

    use module_wrf_error

    use module_dm, only: wrf_dm_minval_real, wrf_dm_maxval_real, wrf_dm_sum_real

    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    use module_relax
    implicit none

    integer, intent(in), optional :: iuvguess,juvguess
    type(domain), intent(inout) :: grid
    character*(*), intent(in) :: cparm
    real, intent(in) :: dxdymean, srsq
    real, intent(inout) :: rout
    integer, intent(inout) :: iout,jout
    logical, intent(inout) :: calcparm
    real, intent(inout) :: latout,lonout
    real, intent(in) :: orig(ims:ime,jms:jme)
    real, intent(out) :: smooth(ims:ime,jms:jme)
    character*255 :: message
    logical, optional :: north_hemi

    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE

    integer :: icen,jcen,i,j,ismooth,ierr
    real :: rcen, here, sum, mean, cendist, heredist

    integer :: istart,istop, jstart,jstop,itemp

    logical :: findmin

    
    grid%relaxmask=.false.
    do j=max(jds+2,jps),min(jde-3,jpe)
       do i=max(ids+2,ips),min(ide-3,ipe)
          grid%relaxmask(i,j)=.true.
       enddo
    enddo
    do j=jps,min(jde-1,jpe)
       do i=ips,min(ide-1,ipe)
          grid%relaxwork(i,j)=orig(i,j)
       enddo
    enddo

    
    
    if(trim(cparm)=='wind') then
       itemp=nint(1.2*111./(dxdymean*sqrt(2.)))
       ismooth=min(30,max(2,itemp))
       
    elseif(grid%vortex_tracker==6) then
       itemp=nint(0.3*111./(dxdymean*sqrt(2.)))
       ismooth=min(15,max(1,itemp))
       
    else
       itemp=nint(150./(dxdymean*sqrt(2.)))
       ismooth=min(50,max(2,itemp))
    endif

    
    istart=max(ids+1,ips)
    istop=min(ide-2,ipe)
    jstart=max(jds+2,jps)
    jstop=min(jde-3,jpe)

    
    if(present(iuvguess)) then
       istart=max(istart,iuvguess-nint(rads_vmag/(2.*dxdymean)))
       istop=min(istop,iuvguess+nint(rads_vmag/(2.*dxdymean)))
    endif
    if(present(juvguess)) then
       jstart=max(jstart,juvguess-nint(rads_vmag/(2.*dxdymean)))
       jstop=min(jstop,juvguess+nint(rads_vmag/(2.*dxdymean)))
    endif

    
    
    call relax4e(grid,real(0.59539032480831),ismooth,0, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)
    
    
    do j=jps,min(jde-1,jpe)
       do i=ips,min(ide-1,ipe)
          smooth(i,j)=grid%relaxwork(i,j)
       enddo
    enddo
       
    
    ifmin: if(trim(cparm)=='zeta') then
       if(.not.present(north_hemi)) then
          call wrf_error_fatal3("<stdin>",1556,&
'When calling module_tracker find_center for zeta, you must specify the hemisphere parameter.')
       endif
       findmin=.not.north_hemi
    elseif(trim(cparm)=='hgt') then
       findmin=.true.
    elseif(trim(cparm)=='slp') then
       findmin=.true.
    elseif(trim(cparm)=='wind') then
       findmin=.true.
    else
100    format('Invalid parameter cparm="',A,'" in module_tracker find_center')
       
       call wrf_error_fatal3("<stdin>",1569,&
message)
    endif ifmin

3011 format('ips=',I0,' ipe=',I0,' istart=',I0,' istop=',I0)
3012 format('jps=',I0,' jpe=',I0,' jstart=',I0,' jstop=',I0)
    
    

    
    icen=-99
    jcen=-99
    findminmax: if(findmin) then 
       rcen=9e9
       do j=jstart,jstop
          do i=istart,istop
             if(grid%relaxwork(i,j)<rcen .and. grid%distsq(i,j)<srsq) then
                rcen=grid%relaxwork(i,j)
                icen=i
                jcen=j
             endif
          enddo
       enddo
3013   format(A,' minval i=',I0,' j=',I0,' r=',F0.3)
       

       call wrf_dm_minval_real(rcen,icen,jcen)
       

    else 
3014   format(A,' maxval i=',I0,' j=',I0,' r=',F0.3)
       rcen=-9e9
       do j=jstart,jstop
          do i=istart,istop
             if(grid%relaxwork(i,j)>rcen .and. grid%distsq(i,j)<srsq) then
                rcen=grid%relaxwork(i,j)
                icen=i
                jcen=j
             endif
          enddo
       enddo
       

       call wrf_dm_maxval_real(rcen,icen,jcen)
       

    endif findminmax

    
    resultif: if(icen==-99 .or. jcen==-99) then
       
       calcparm=.false.
       
    else
       iout=icen
       jout=jcen
       rout=rcen
       calcparm=.true.
       call get_lonlat(grid,iout,jout,lonout,latout,ierr, &
            ids,ide, jds,jde, kds,kde, &
            ims,ime, jms,jme, kms,kme, &
            ips,ipe, jps,jpe, kps,kpe) 
       if(ierr/=0) then
          
          calcparm=.false.
          return
       endif
       
    endif resultif
  end subroutine find_center

  subroutine get_tracker_distsq(grid, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         ITS,ITE,JTS,JTE,KTS,KTE)
    
    
    
    
    
    
    

    use module_dm, only: wrf_dm_minval_real

    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    implicit none
    type(domain), intent(inout) :: grid
    character*255 message
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: ITS,ITE,JTS,JTE,KTS,KTE
    integer i,j,cx,cy,ierr
    integer wilbur,harvey 
    real xfar,yfar,far,xshift,max_edge_distsq,clatr,clonr
    real ylat1,ylat2,xlon1,xlon2,mindistsq

    cx=grid%tracker_ifix
    cy=grid%tracker_jfix

    call get_lonlat(grid,cx,cy,clonr,clatr,ierr, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         ITS,ITE,JTS,JTE,KTS,KTE)
    if(ierr/=0) then
       call wrf_error_fatal3("<stdin>",1674,&
'Tracker fix location is not inside domain.')
    end if

    do j=jts,min(jte,jde-1)
       if(mod(j,2)==1) then
          xshift=1.
       else
          xshift=-1.
       endif
       do i=its,min(ite,ide-1)
          xfar=(i-cx)*grid%dx_nmm(i,j)*2
          yfar=(j-cy)*grid%dy_nmm
          if(mod(cy-j,2) /= 0) then
             xfar=xfar + grid%dx_nmm(i,j)*xshift
          endif
          far = xfar*xfar + yfar*yfar
          GRID%tracker_distsq(i,j)=far
       enddo
    enddo

    
    
    
    xlon1=clonr ; ylat1=clatr
    call clean_lon_lat(xlon1,ylat1)
    xlon1=xlon1*pi180
    ylat1=ylat1*pi180
    do j=jts,min(jte,jde-1)
       do i=its,min(ite,ide-1)
          xlon2=grid%glon(i,j)
          ylat2=grid%glat(i,j)
          call clean_lon_lat(xlon2,ylat2)
          xlon2=xlon2*pi180
          ylat2=ylat2*pi180
          grid%tracker_angle(i,j)=atan2(xlon2-xlon1,ylat2-ylat1)
       enddo
    enddo

    
    
    mindistsq=9e19
    if(jts==jds) then
       mindistsq=min(mindistsq,minval(grid%tracker_distsq(its:min(ite,ide-1),jds)))
    endif
    if(jte==jde) then
       mindistsq=min(mindistsq,minval(grid%tracker_distsq(its:min(ite,ide-1),jde-1)))
    endif
    if(its==ids) then
       mindistsq=min(mindistsq,minval(grid%tracker_distsq(ids,jts:min(jte,jde-1))))
    endif
    if(ite==ide) then
       mindistsq=min(mindistsq,minval(grid%tracker_distsq(ide-1,jts:min(jte,jde-1))))
    endif

    wilbur=1
    harvey=2
    call wrf_dm_minval_real(mindistsq,wilbur,harvey)


    grid%tracker_edge_dist=sqrt(mindistsq)

17  format('Min distance from lon=',F9.3,', lat=',F9.3,' to center is ',F19.3)
    
    write(message,17) clonr, clatr, grid%tracker_edge_dist
    call wrf_message(message)
  end subroutine get_tracker_distsq

  subroutine clean_lon_lat(xlon1,ylat1)
    real, intent(inout) :: xlon1,ylat1
    
    
    
    
    xlon1=(mod(xlon1+3600.+180.,360.)-180.)
    ylat1=(mod(ylat1+3600.+180.,360.)-180.)
    if(ylat1>90.) then
       ylat1=180.-ylat1
       xlon1=mod(xlon1+360.,360.)-180.
    elseif(ylat1<-90.) then
       ylat1=-180. - ylat1
       xlon1=mod(xlon1+360.,360.)-180.
    endif
  end subroutine clean_lon_lat

  subroutine calcdist(rlonb,rlatb,rlonc,rlatc,xdist,degrees)
    
    
    
    
    
    
    
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    implicit none

    real, intent(inout) :: degrees
    real, intent(out) :: xdist
    real, intent(in) :: rlonb,rlatb,rlonc,rlatc
    real, parameter :: dtr = 0.0174532925199433
    real :: distlatb,distlatc,pole,difflon,cosanga,circ_fract
    
    if (rlatb < 0.0 .or. rlatc < 0.0) then
       pole = -90.
    else
       pole = 90.
    endif
    
    distlatb = (pole - rlatb) * dtr
    distlatc = (pole - rlatc) * dtr
    difflon  = abs( (rlonb - rlonc)*dtr )
    
    cosanga = ( cos(distlatb) * cos(distlatc) + &
         sin(distlatb) * sin(distlatc) * cos(difflon))

    
    
    
    

    if (cosanga > 1.0) then
       cosanga = 1.0
    endif

    degrees    = acos(cosanga) / dtr
    circ_fract = degrees / 360.
    xdist      = circ_fract * ecircum
    
    
    
    
    
    
    return
  end subroutine calcdist

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  


  subroutine get_lonlat(grid,iguess,jguess,longuess,latguess,ierr, &
       ids,ide, jds,jde, kds,kde, &
       ims,ime, jms,jme, kms,kme, &
       ips,ipe, jps,jpe, kps,kpe)
    
    
    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    USE MODULE_DM, ONLY: wrf_dm_maxloc_real
    implicit none
    integer, intent(in) :: &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         ips,ipe, jps,jpe, kps,kpe
    integer, intent(out) :: ierr
    type(domain), intent(inout) :: grid
    integer, intent(in) :: iguess,jguess
    real, intent(inout) :: longuess,latguess
    real :: weight,zjunk
    integer :: itemp,jtemp

    ierr=0
    zjunk=1
    if(iguess>=ips .and. iguess<=ipe .and. jguess>=jps .and. jguess<=jpe) then
       weight=1
       longuess=grid%hlon(iguess,jguess)
       latguess=grid%hlat(iguess,jguess)
       itemp=iguess
       jtemp=jguess
    else
       weight=0
       longuess=-999.9
       latguess=-999.9
       itemp=-99
       jtemp=-99
    endif


    call wrf_dm_maxloc_real(weight,latguess,longuess,zjunk,itemp,jtemp)


    if(itemp==-99 .and. jtemp==-99) then
       ierr=95
    endif
  end subroutine get_lonlat

  subroutine update_tracker_post_move(grid)
    
    
    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    type(domain), intent(inout) :: grid
    integer :: ierr, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE

    
    CALL get_ijk_from_grid (  grid ,      &
         ids, ide, jds, jde, kds, kde,    &
         ims, ime, jms, jme, kms, kme,    &
         ips, ipe, jps, jpe, kps, kpe    )

    
    ierr=0
    call get_nearest_lonlat(grid,grid%tracker_ifix,grid%tracker_jfix, &
         ierr,grid%tracker_fixlon,grid%tracker_fixlat, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)

    
    
    if(ierr==0) &
         call get_tracker_distsq(grid, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)
  end subroutine update_tracker_post_move

end module module_tracker


subroutine nmm_med_tracker_post_move(grid)
  
  
  use module_tracker, only: update_tracker_post_move
  use module_domain, only : domain
  type(domain), intent(inout) :: grid
  call update_tracker_post_move(grid)
end subroutine nmm_med_tracker_post_move
