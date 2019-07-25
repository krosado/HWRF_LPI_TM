module module_stats_for_move
  implicit none
  private

  public :: stats_for_move, vorttrak_init

  

  
  real, parameter :: vt5_max_radius=250000.0
  real, parameter :: vt5_min_radius=100000.0
  real, parameter :: vt5_start_radius=vt5_max_radius

  
  
  real, parameter :: vt5_move_factor=1.1
  real, parameter :: vt5_nomove_factor=0.8

contains

  SUBROUTINE VORTTRAK_INIT(grid,config_flags,init,  &
                           IDS,IDE,JDS,JDE,KDS,KDE, &
                           IMS,IME,JMS,JME,KMS,KME, &
                           ITS,ITE,JTS,JTE,KTS,KTE)
    USE MODULE_CONFIGURE, ONLY : grid_config_rec_type
    USE MODULE_DOMAIN, ONLY : domain

    USE module_tracker, only: ncep_tracker_init

    IMPLICIT NONE
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: ITS,ITE,JTS,JTE,KTS,KTE
    type(domain), intent(inout) :: grid
    type(grid_config_rec_type), intent(in) :: config_flags
    integer :: vortex_tracker
    character*255 :: message
    logical, intent(in) :: init 

    
    integer :: cx,cy 
    real :: xfar,yfar,far 
    integer :: xshift 
    integer :: i,j
    

    vortex_tracker=grid%vortex_tracker
    if(vortex_tracker<1 .or. vortex_tracker>7) then
31     format('Domain ',I0,' has invalid value ',I0,' for vortex_tracker: it must be an integer from 1-7')
       write(message,31) grid%id,vortex_tracker
       call wrf_error_fatal3("<stdin>",51,&
message)
    endif

    if(grid%swath_mode==1) then
      
      
       if(grid%interest_storms/=0 .and. vortex_tracker/=6 .and. vortex_tracker/=7) then
          grid%interest_storms=0
          if(vortex_tracker==2) then
             if(grid%interest_kids/=0) then
                
                
                
                grid%interest_kids=1
39              format('Grid ',I0,' switching from interest_storms to interest_kids due to vortex_tracker==2 (nest following)')
                write(message,39) grid%id
                call wrf_message(message)
             else
                
                
                
                
                
37              format('Grid ',I0,' using nest area of interest (already enabled) instead of storm area of interest due to vortex_tracker==2 (nest following).')
                write(message,37) grid%id
                call wrf_debug(2,message)
             endif
          elseif(grid%interest_self==0) then
             
             
             
             
             if(grid%num_nests<1) then
                grid%interest_self=1
                grid%interest_rad_self=grid%interest_rad_storm
38              format('Grid ',I0,' switching from interest_storm to interest_self due to lack of vortex information.  You must use vortex tracker 6 or 7 to get a storm area of interest.')
                write(message,38) grid%id
                call wrf_message(message)
             else
                grid%interest_kids=1
35              format('Grid ',I0,' switching from interest_storm to interest_kids due to lack of vortex information, and presence of nests.  You must use vortex tracker 6 and 7 to get a storm area of interest.')
                write(message,35) grid%id
                call wrf_message(message)
             endif
          endif
       endif
    endif

    
    
    grid%pdyn_parent_age=0
    grid%pdyn_smooth_age=0

    if(size(grid%pdyn_smooth)>1) then
       
       CALL UPDATE_PDYN_MSLP(grid,config_flags, &
                            IDS,IDE,JDS,JDE,KDS,KDE, &
                            IMS,IME,JMS,JME,KMS,KME, &
                            ITS,ITE,JTS,JTE,KTS,KTE )
       
    end if

    is_vt45: if(vortex_tracker==4 .or. vortex_tracker==5) then
       call wrf_message('in VORTTRAK_INIT for vortex tracker 4 or 5')
       if(vortex_tracker==4) then
          if(.not.(grid%vt4_pmax<0.0)) then
             if(grid%vt4_pmax<100000.0 .or. grid%vt4_pmax>107000) then
                write(message,'("vt4_pmax bad: vt4_pmax must be either <0 or within [1e5,1.07e5], but it is ",F15.5,".  We recommend -1.")') grid%vt4_pmax
                call wrf_error_fatal3("<stdin>",120,&
message)
             endif
          endif
       endif

       if(vortex_tracker==5) then
          if(init) then 
             grid%vt5searchrad=vt5_start_radius
13011        format("Search radius now ",F0.3,"km for domain ",I0)
             
             
          endif
       endif
    endif is_vt45

    distsq: if(size(grid%distsq)>1) then
       cx=ide/2
       cy=jde/2
       
       jdo: do j = jts, min(jte,jde)
          if(mod(j,2)==1) then
             xshift=1.
          else
             xshift=-1.
          endif
          do i = its, min(ite,ide)
             xfar=(i-cx)*grid%dx_nmm(i,j)*2
             yfar=(j-cy)*grid%dy_nmm
             if(mod(cy-j,2) /= 0) then
                xfar=xfar + grid%dx_nmm(i,j)*xshift
             endif
             far = xfar*xfar + yfar*yfar
             GRID%distsq(i,j)=far
          enddo
       enddo jdo
    endif distsq



    if(init .and. (vortex_tracker==6 .or. vortex_tracker==7) ) then
       call ncep_tracker_init(grid)
    endif


  END SUBROUTINE VORTTRAK_INIT


  
  
  SUBROUTINE UPDATE_PDYN_MSLP(grid,config_flags, &
                            IDS,IDE,JDS,JDE,KDS,KDE, &
                            IMS,IME,JMS,JME,KMS,KME, &
                            IPS,IPE,JPS,JPE,KPS,KPE )
    USE MODULE_CONFIGURE, ONLY : grid_config_rec_type
    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid

    USE MODULE_COMM_DM, ONLY : HALO_NMM_TRACK_sub
    USE MODULE_DM, ONLY: ntasks_x, ntasks_y, mytask, ntasks, local_communicator

    use module_membrane_mslp
    implicit none
    type(domain), intent(inout) :: grid
    type(grid_config_rec_type), intent(in) :: config_flags
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE
    integer :: i,j
    logical bad

    
    CALL STATS_MAKE_MSLP (grid%PDYN,grid%membrane_MSLP,grid%MSLP,grid%SQWS,         &
                          grid%PINT,grid%T,grid%Q,grid%U,grid%V, &
                          grid%FIS,grid%PD,grid%SM,              &
                          grid%PDTOP,grid%PT,                    &
                          grid%DETA1,grid%DETA2,grid%ETA2,       &
                          IDS,IDE,JDS,JDE,KDS,KDE,               &
                          IMS,IME,JMS,JME,KMS,KME,               &
                          IPS,IPE,JPS,JPE,KPS,KPE)

    
    
    if(size(grid%pdyn_smooth)>1) then
       if(grid%id==1) then
          
          
          do j=max(jds,jps),min(jpe,jde-1)
             do i=max(ids,ips),min(ipe,ide-1)
                grid%pdyn_smooth(i,j)=grid%pdyn(i,j)
             enddo
          enddo
       else
          
          do j=max(jds,jps),min(jpe,jde-1)
             do i=max(ids,ips),min(ipe,ide-1)
                grid%pdyn_smooth(i,j) = &
                     0.5*grid%pdyn(i,j) + &
                     0.5*grid%pdyn_parent(i,j)
             enddo
          enddo
       endif

       grid%pdyn_smooth_age=max(1,grid%pdyn_smooth_age+1)
    else
       call wrf_error_fatal3("<stdin>",224,&
'pdyn_smooth not allocated')
    endif







CALL HALO_NMM_TRACK_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


  END SUBROUTINE UPDATE_PDYN_MSLP


  
  
  SUBROUTINE STATS_FOR_MOVE(grid,config_flags, &
                            IDS,IDE,JDS,JDE,KDS,KDE, &
                            IMS,IME,JMS,JME,KMS,KME, &
                            IPS,IPE,JPS,JPE,KPS,KPE, &
                            ITS,ITE,JTS,JTE,KTS,KTE )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    USE module_tracker, only: ncep_tracker_center
    USE MODULE_CONFIGURE, ONLY : grid_config_rec_type
    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    use module_membrane_mslp
    USE MODULE_COMM_DM, ONLY : HALO_NMM_VT4_NOISE_sub, HALO_NMM_VT4_MSLP_sub
    USE MODULE_DM, ONLY: ntasks_x, ntasks_y, mytask, ntasks, local_communicator
    IMPLICIT NONE
    type(domain), intent(inout) :: grid
    type(grid_config_rec_type), intent(in) :: config_flags
    integer :: i,movefreq
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE
    integer, intent(in) :: ITS,ITE,JTS,JTE,KTS,KTE
    integer :: vortex_tracker,j
    character*255 :: message
    logical :: skip_nest_motion

    

    MOVEFREQ=grid%movemin*grid%nphs
    vortex_tracker=grid%vortex_tracker



    skip_nest_motion=.false.
    IF(MOD(grid%NTSD+1,MOVEFREQ)/=0 .or. grid%id==1)THEN

       IF(grid%MOVED .and. grid%id/=1) then
          grid%NTIME0=grid%NTSD             
       ENDIF

       grid%MVNEST=.FALSE.
       skip_nest_motion=.true.
    ENDIF


    if(skip_nest_motion .and. ( grid%pdyn_smooth_age/=0 .or. size(grid%pdyn_smooth)<=1)) then
       
       
       
       return
    else
       
       
       
    endif

    
    call make_membrane_mslp(grid)


    
    CALL UPDATE_PDYN_MSLP(grid,config_flags, &
                          IDS,IDE,JDS,JDE,KDS,KDE, &
                          IMS,IME,JMS,JME,KMS,KME, &
                          IPS,IPE,JPS,JPE,KPS,KPE)

    if(skip_nest_motion) then
       
       
       return
    endif


    
    oldmove: if(vortex_tracker<4) then
       
       
       CALL STATS_FOR_MOVE_123 (grid%XLOC_2,grid%YLOC_2                   &

                                ,grid%MSLP                                &



            ,grid%sm							  &

            ,GRID%RESTART,grid%NTIME0                                     &
            ,GRID%MOVED,grid%MVNEST,grid%ntsd,GRID%NPHS,GRID%MOVEMIN      &



            ,GRID%VORTEX_TRACKER                                          &
            ,IDS,IDE-1,JDS,JDE-1,KDS,KDE                                  &
            ,IMS,IME,JMS,JME,KMS,KME                                      &
            ,ITS,ITE,JTS,JTE,KTS,KTE)
       RETURN

    elseif(vortex_tracker==6 .or. vortex_tracker==7) then
       
       call ncep_tracker_center(grid)
       call vt67_move(grid%tracker_ifix,grid%tracker_jfix,  &
                     grid%tracker_gave_up,grid%tracker_havefix, &
                     grid%xloc_2,grid%yloc_2, grid%id,     &
                     grid%xloc_1,grid%yloc_1, grid%mvnest, &
                     IDS,IDE,JDS,JDE,KDS,KDE,              &
                     IMS,IME,JMS,JME,KMS,KME,              &
                     ITS,ITE,JTS,JTE,KTS,KTE)

       
       
       if(grid%interest_storms/=0) then
38        format('grid ',I2,' updating area of interest due to storm motion')
          write(message,38) grid%id
          call wrf_message(trim(message))
          grid%update_interest=.true.
       else
39        format('grid ',I2,' not updating area of interest after storm motion because grid%interest_storms is 0')
          write(message,39) grid%id
          call wrf_message(trim(message))
       endif
    elseif(vortex_tracker==5) then
       
       call vt5_move(grid%pdyn_parent,grid%distsq,grid%vt5searchrad, &
                     grid%xloc_2,grid%yloc_2, grid%id,     &
                     grid%xloc_1,grid%yloc_1, grid%mvnest, &
                     IDS,IDE,JDS,JDE,KDS,KDE,              &
                     IMS,IME,JMS,JME,KMS,KME,              &
                     ITS,ITE,JTS,JTE,KTS,KTE)

       
       
       if(grid%mvnest) then
          grid%vt5searchrad=max(vt5_min_radius,min(vt5_max_radius, &
               grid%vt5searchrad*vt5_move_factor))
       else
          grid%vt5searchrad=max(vt5_min_radius,min(vt5_max_radius, &
               grid%vt5searchrad*vt5_nomove_factor))
       endif
13011  format("Search radius now ",F0.3,"km for domain ",I0)
       write(message,13011) grid%vt5searchrad/1000.0,grid%id
       call wrf_debug(1,message)
    else
    

       
       if(config_flags%vt4_noise_iter<0) then
          grid%mslp_noisy=0
       else






CALL HALO_NMM_VT4_MSLP_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

          call vt4_noise_detect(grid%mslp,grid%mslp_noisy,   &
                                config_flags%vt4_noise_pmax, &
                                config_flags%vt4_noise_pmin, &
                                config_flags%vt4_noise_dpdr, &
                                grid%dx_nmm,grid%dy_nmm,     &
                                IDS,IDE,JDS,JDE,KDS,KDE,     &
                                IMS,IME,JMS,JME,KMS,KME,     &
                                ITS,ITE,JTS,JTE,KTS,KTE)
          do i=1,config_flags%vt4_noise_iter          






CALL HALO_NMM_VT4_NOISE_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

             call vt4_noise_iter(grid%mslp_noisy,             &
                                 IDS,IDE,JDS,JDE,KDS,KDE,     &
                                 IMS,IME,JMS,JME,KMS,KME,     &
                                 ITS,ITE,JTS,JTE,KTS,KTE)
          enddo
       endif
       call vt4_move(grid%mslp,grid%weightout,grid%distsq, &
                     grid%mslp_noisy, grid%dx_nmm,         &
                     grid%dy_nmm,grid%xloc_2,grid%yloc_2,  &
                     grid%xloc_1,grid%yloc_1,grid%mvnest,  &
                     config_flags%vt4_radius,              &
                     config_flags%vt4_weightexp,           &
                     config_flags%vt4_pmax,                &
                     IDS,IDE,JDS,JDE,KDS,KDE,              &
                     IMS,IME,JMS,JME,KMS,KME,              &
                     IPS,IPE,JPS,JPE,KPS,KPE,              &
                     ITS,ITE,JTS,JTE,KTS,KTE)
    endif oldmove
  END SUBROUTINE STATS_FOR_MOVE

  SUBROUTINE vt4_noise_iter(NOISY,                   &
                            IDS,IDE,JDS,JDE,KDS,KDE, &
                            IMS,IME,JMS,JME,KMS,KME, &
                            ITS,ITE,JTS,JTE,KTS,KTE)
    
    
    
    
    
    
    
    
    
    
    
    IMPLICIT NONE
    integer,dimension(ims:ime,jms:jme), intent(inout) :: noisy
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE, &
                           IMS,IME,JMS,JME,KMS,KME, &
                           ITS,ITE,JTS,JTE,KTS,KTE
    integer :: i,j,iadd
    
    do j = max(jds+1,jts), min(jte,jde-2)
       iadd=mod(j,2)-1
       do i = max(ids+1,its), min(ite,ide-2)
          if(       noisy(i+1+iadd,j+1)/=0 &
               .or. noisy(i+1+iadd,j-1)/=0 &
               .or. noisy(i+iadd,j+1)/=0 &
               .or. noisy(i+iadd,j-1)/=0) then
             noisy(i,j)=1
          endif
       enddo
    enddo
  END SUBROUTINE vt4_noise_iter
  SUBROUTINE vt4_noise_detect(MSLP,NOISY,PMAX,PMIN,DPDR, &
                              DX_NMM, DY_NMM,            &
                              IDS,IDE,JDS,JDE,KDS,KDE,   &
                              IMS,IME,JMS,JME,KMS,KME,   &
                              ITS,ITE,JTS,JTE,KTS,KTE)
    
    
    
    
    
    
    
    
    
    
    
    
    
    IMPLICIT NONE
    real,dimension(ims:ime,jms:jme), intent(in) :: mslp,dx_nmm
    real, intent(in) :: dy_nmm,pmax,pmin,dpdr
    integer,dimension(ims:ime,jms:jme), intent(out) :: noisy
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE, &
                           IMS,IME,JMS,JME,KMS,KME, &
                           ITS,ITE,JTS,JTE,KTS,KTE
    real :: dp2,dy2,dx2,pdiff,dp2max
    integer :: i,j
    integer :: iadd

    
    dy2=dy_nmm*dy_nmm*4
    dp2max=dpdr*dpdr


    do j=jts,min(jte,jde-1)
       iadd=mod(j,2)-1
       do i=its,min(ite,ide-1)
          noisetype: if(mslp(i,j)>pmax) then
             noisy(i,j)=1
          elseif(mslp(i,j)<pmin) then







             noisy(i,j)=1
          else
             dx2=dx_nmm(i,j)*dx_nmm(i,j)*4
             
             notbdy: if(i>ids .and. i<ide-1 .and. j>jds .and. j<jde-1) then
                
                pdiff=mslp(i+1+iadd,j+1)-mslp(i+iadd,j-1)
                dp2=pdiff*pdiff/(dx2+dy2)
                if(dp2>dp2max) then
                   noisy(i,j)=1
                   cycle
                endif

                
                pdiff=mslp(i+1+iadd,j-1)-mslp(i+iadd,j+1)
                dp2=pdiff*pdiff/(dx2+dy2)
                if(dp2>dp2max) then
                   noisy(i,j)=1
                   cycle
                endif
             endif notbdy

             
             
             noisy(i,j)=0
          endif noisetype
       enddo
    enddo
  END SUBROUTINE vt4_noise_detect

  SUBROUTINE vt5_move(PDYN,distsq,searchrad,xloc,yloc,gridid,cx,cy,mvnest, &
                      IDS,IDE,JDS,JDE,KDS,KDE,                   &
                      IMS,IME,JMS,JME,KMS,KME,                   &
                      ITS,ITE,JTS,JTE,KTS,KTE)
    use module_dm, only: wrf_dm_minval_real
    implicit none
    real, intent(in) :: PDYN(ims:ime,jms:jme)
    real, intent(in) :: distsq(ims:ime,jms:jme)
    real, intent(in) :: searchrad
    integer, intent(inout) :: xloc,yloc
    integer, intent(in) :: cx,cy,gridid
    logical, intent(out) :: mvnest 
    integer :: &
                      IDS,IDE,JDS,JDE,KDS,KDE,                   &
                      IMS,IME,JMS,JME,KMS,KME,                   &
                      ITS,ITE,JTS,JTE,KTS,KTE
   
    real, parameter :: big_pdyn=999999.9
    integer i,j,iloc,jloc
    real pdynloc,xdiff,ydiff,radsq
    character*255 message

    if(gridid==1) then 
       
       mvnest=.false.
       xloc=cx
       yloc=cy
       return
    endif

201 format("Search for minimum PDYN (<",F10.2,") within searchrad=",F0.3,"km of domain center cx=",I0," cy=",I0)
    

    radsq=searchrad*searchrad
    iloc=-1
    jloc=-1
    pdynloc=big_pdyn
    do j=jts,min(jde-1,jte)
       do i=its,min(ide-1,ite)
          if(distsq(i,j)<radsq .and. PDYN(i,j)<pdynloc) then
             pdynloc=PDYN(i,j)
             iloc=i
             jloc=j
          endif
       enddo
    enddo

303 format("local loc: pdyn=",F0.3," i=",I0," j=",I0)
    

    call wrf_dm_minval_real(pdynloc,iloc,jloc)

304 format("global loc: pdyn=",F0.3," i=",I0," j=",I0)
    

    if(iloc==-1) then
       call wrf_error_fatal3("<stdin>",654,&
'ERROR: cannot find min PDYN.  Either distsq or searchrad are erroneous, or your resolution is worse than 100km.')
    endif

    xdiff=abs(iloc-real(cx))/3.0
    ydiff=abs(jloc-real(cy))/6.0
    if(xdiff>=1. .or. ydiff>=1.) then
       
       
       mvnest=.true.
       xloc=iloc
       yloc=jloc
       call wrf_debug(1,'Moving: PDYN minimum left nest center')
    else
       mvnest=.false.
       xloc=cx
       yloc=cy
       call wrf_debug(1,'Not moving: PDYN minimum is near nest center')
    endif
  END SUBROUTINE vt5_move
  SUBROUTINE vt67_move(ifix,jfix,gaveup,havefix, &
                      xloc,yloc,gridid,cx,cy,mvnest, &
                      IDS,IDE,JDS,JDE,KDS,KDE,                   &
                      IMS,IME,JMS,JME,KMS,KME,                   &
                      ITS,ITE,JTS,JTE,KTS,KTE)
    
    
    use module_dm, only: wrf_dm_minval_real
    implicit none
    integer, intent(in) :: ifix,jfix
    logical, intent(in) :: gaveup,havefix
    integer, intent(inout) :: xloc,yloc
    integer, intent(in) :: cx,cy,gridid
    logical, intent(out) :: mvnest 
    integer :: &
                      IDS,IDE,JDS,JDE,KDS,KDE,                   &
                      IMS,IME,JMS,JME,KMS,KME,                   &
                      ITS,ITE,JTS,JTE,KTS,KTE
   
    real, parameter :: big_pdyn=999999.9
    integer i,j,iloc,jloc
    real pdynloc,xdiff,ydiff,radsq
    character*255 message

    mvnest=.false.
    xloc=cx
    yloc=cy
    
    if(gridid==1) then 
       return
    endif

    if(gaveup) then
       call wrf_debug(1,'Not moving: tracker decided the storm dissapated')
       return
    endif

    if(.not.havefix) then
       call wrf_debug(1,'Not moving: tracker did not find a storm')
       return
    endif

    xdiff=abs(ifix-real(cx))/3.0
    ydiff=abs(jfix-real(cy))/6.0
    if(xdiff>=1. .or. ydiff>=1.) then
       
       
       mvnest=.true.
       xloc=ifix
       yloc=jfix
       call wrf_debug(1,'Moving: tracker center left nest center')
    else
       mvnest=.false.
       xloc=cx
       yloc=cy
       call wrf_debug(1,'Not moving: tracker center is near nest center')
    endif
  END SUBROUTINE vt67_move
  SUBROUTINE vt4_move(MSLP,WEIGHTOUT,DISTSQ,NOISY,DX_NMM,DY_NMM, &
                      xloc,yloc,cx,cy,mvnest,                    &
                      searchrad,searchpow,searchpmax,            &
                      IDS,IDE,JDS,JDE,KDS,KDE,                   &
                      IMS,IME,JMS,JME,KMS,KME,                   &
                      IPS,IPE,JPS,JPE,KPS,KPE,                   &
                      ITS,ITE,JTS,JTE,KTS,KTE)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    USE module_dm, only: wrf_dm_maxval_real, wrf_dm_sum_real, &
         local_communicator_y, local_communicator_x
    IMPLICIT NONE
    INCLUDE 'mpif.h'
    character*512 :: message
    integer, intent(out) :: xloc,yloc
    integer, intent(in) :: cx,cy
    real,dimension(ims:ime,jms:jme), intent(in) :: mslp,distsq,dx_nmm
    real, intent(in) :: dy_nmm,searchrad,searchpow,searchpmax
    real,dimension(ims:ime,jms:jme), intent(out) :: weightout
    integer,dimension(ims:ime,jms:jme), intent(inout) :: noisy
    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE, &
                           IMS,IME,JMS,JME,KMS,KME, &
                           IPS,IPE,JPS,JPE,KPS,KPE, &
                           ITS,ITE,JTS,JTE,KTS,KTE
    logical, intent(out) :: mvnest
    real :: pmax,sr2,weight,xsum,ysum,xwsum,ywsum
    real :: centroid_x, centroid_y, xdiff, ydiff
    real :: xweight,yweight,maxweight
    integer :: i,j,idum,jdum,ierr,ctx,cty
    integer :: xcount(jps:jpe),ycount(ips:ipe)
    integer :: myxcount(jps:jpe),myycount(ips:ipe)


    
    
    
    if(searchrad<dy_nmm*4) then
       call wrf_error_fatal3("<stdin>",788,&
'increase searchrad: searchrad<dy_nmm*4')
    endif
    sr2=searchrad*searchrad

    
    
    
    
    
    
    
    
    
    
    
    

    do j=jts,min(jte,jde-1)
       if(j<jds+10 .or. j>jde-11) then
          noisy(its:ite,j)=3
       else
          do i=its,min(ite,ide-1)
             if(i<ids+7 .or. i>ide-8) then
                noisy(i,j)=3
             elseif(distsq(i,j)>sr2 .and. noisy(i,j)/=1) then
                noisy(i,j)=2
             endif
          enddo
       endif
    enddo

    
    
    myxcount=0
    myycount=0
    do j=jps,min(jpe,jde-1)
       do i=ips,min(ipe,ide-1)
          if(noisy(i,j)==0) then
             myxcount(j)=myxcount(j)+1
          endif
       enddo
    enddo
    do j=jps,min(jpe,jde-1)
       do i=ips,min(ipe,ide-1)
          if(noisy(i,j)==0) then
             myycount(i)=myycount(i)+1
          endif
       enddo
    enddo
    call MPI_Allreduce(myxcount,xcount,jpe-jps+1,MPI_INTEGER,MPI_SUM,local_communicator_x,ierr)
    call MPI_Allreduce(myycount,ycount,ipe-ips+1,MPI_INTEGER,MPI_SUM,local_communicator_y,ierr)

    
    findpmax: if(searchpmax<0.0) then
       pmax=-9e9
       do j = max(jds+2,jts), min(jte,jde-3)
          do i = max(ids+1,its), min(ite,ide-2)
             if(noisy(i,j)==0 .and. mslp(i,j)>pmax) then
                pmax=mslp(i,j)
             endif
          enddo
       enddo

       idum=-99 ; jdum=-99 
       call wrf_dm_maxval_real(pmax,idum,jdum)
       pmax=min(105000.0,pmax)
    else
       pmax=searchpmax
    endif findpmax
    

    xsum=0.0 ; ysum=0.0 ; xwsum=0.0 ; ywsum=0.0
    maxweight=-99.

    if_pow_1: if(abs(searchpow-1.0)<1e-5) then
       
       
       do j = max(jds+2,jts), min(jte,jde-3)
          do i = max(ids+1,its), min(ite,ide-2)
             if(noisy(i,j)==0) then
                weight = pmax - mslp(i,j)
                if(weight>0) then
                   if(weight>maxweight) maxweight=weight
                   weight=weight/pmax
                   weightout(i,j)=weight
                   xweight=weight/ycount(i)
                   yweight=weight/xcount(j)
                   xsum=xsum + i*xweight
                   ysum=ysum + j*yweight
                   xwsum=xwsum + xweight
                   ywsum=ywsum + yweight
                else
                   weightout(i,j)=0
                end if
             else
                weightout(i,j)=0
             endif
          enddo
       enddo
    else
       if_sqrt: if(abs(searchpow-0.5)<1e-5) then
          
          do j = max(jds+2,jts), min(jte,jde-3)
             do i = max(ids+1,its), min(ite,ide-2)
                if(noisy(i,j)==0) then
                   weight = pmax - mslp(i,j)
                   if(weight>0) then
                      if(weight>maxweight) maxweight=weight
                      weight=sqrt(weight/pmax)
                      weightout(i,j)=weight
                      xweight=weight/ycount(i)
                      yweight=weight/xcount(j)
                      xsum=xsum + i*xweight
                      ysum=ysum + j*yweight
                      xwsum=xwsum + xweight
                      ywsum=ywsum + yweight
                   else
                      weightout(i,j)=0
                   end if
                else
                   weightout(i,j)=0
                endif
             enddo
          enddo
       else
          
          do j = max(jds+2,jts), min(jte,jde-3)
             do i = max(ids+1,its), min(ite,ide-2)
                if(noisy(i,j)==0) then
                   weight = pmax - mslp(i,j)
                   if(weight>0) then
                      if(weight>maxweight) maxweight=weight
                      weight=(weight/pmax)**searchpow
                      weightout(i,j)=weight
                      xweight=weight/ycount(i)
                      yweight=weight/xcount(j)
                      xsum=xsum + i*xweight
                      ysum=ysum + j*yweight
                      xwsum=xwsum + xweight
                      ywsum=ywsum + yweight
                   else
                      weightout(i,j)=0
                   end if
                else
                   weightout(i,j)=0
                endif
             enddo
          enddo
       endif if_sqrt
    endif if_pow_1

    if(maxweight<=0) then
       
    else
       
       
    endif

    xwsum = wrf_dm_sum_real(xwsum)
    
    no_vortex: if(xwsum <= 0) then
       
       
       centroid_x=cx
       centroid_y=cy

       write(message,*) 'Lost the storm.  Search rad,pmax,pow = ', &
            searchrad,pmax,searchpow
       call wrf_message(message)
       mvnest=.false.
       return
    else 
       ywsum = wrf_dm_sum_real(ywsum)
       xsum = wrf_dm_sum_real(xsum)
       ysum = wrf_dm_sum_real(ysum)
       centroid_x = xsum/xwsum
       centroid_y = ysum/ywsum

 383   format("XSUM=",F0.3," YSUM=",F0.3," XWSUM=",F0.3," YWSUM=",F0.3, &
       " Center=(",I0,",",I0,")", &
       " Centroid=(",F0.3,",",F0.3,")")
       write(message,383) xsum,ysum,xwsum,ywsum,cx,cy,centroid_x,centroid_y
       call wrf_message(message)
    endif no_vortex

    
    ctx=nint(centroid_x)
    cty=nint(centroid_y)
    do j=cty-1,cty+1
       do i=ctx-4,ctx+4
          if(i>=its .and. i<=ite .and. j>=jts .and. j<=jte) then
             noisy(i,j)=noisy(i,j)-6
          endif
       enddo
    enddo
    do j=cty-4,cty+4
       do i=ctx-1,ctx+1
          if(i>=its .and. i<=ite .and. j>=jts .and. j<=jte) then
             noisy(i,j)=noisy(i,j)-6
          endif
       enddo
    enddo

    xdiff=abs(centroid_x-real(cx))/3.0
    ydiff=abs(centroid_y-real(cy))/6.0
    if(xdiff>=1. .or. ydiff>=1.) then
       
       
       mvnest=.true.
       xloc=nint(centroid_x)
       yloc=nint(centroid_y)
       call wrf_debug(1,'Centroid is far enough from nest center to trigger a move.')
    else
       mvnest=.false.
       xloc=cx
       yloc=cy
       call wrf_debug(1,'Nest has not moved far enough yet.')
    endif
  END SUBROUTINE vt4_move

  SUBROUTINE STATS_MAKE_MSLP(PDYN,MEMBRANE_MSLP,MSLP,SQWS          &
                          ,PINT,T,Q,U,V              &
                          ,FIS,PD,SM,PDTOP,PTOP      &
                          ,DETA1,DETA2,ETA2          &
                          ,IDS,IDE,JDS,JDE,KDS,KDE   &
                          ,IMS,IME,JMS,JME,KMS,KME   &
                          ,ITS,ITE,JTS,JTE,KTS,KTE)
    
    
    
    
    
    
    
    
    
    
    
    USE MODULE_MODEL_CONSTANTS
    IMPLICIT NONE
    INTEGER,INTENT(IN)  :: IDS,IDE,JDS,JDE,KDS,KDE   &
                          ,IMS,IME,JMS,JME,KMS,KME   &
                          ,ITS,ITE,JTS,JTE,KTS,KTE

    REAL, DIMENSION(KMS:KME),                 INTENT(IN)  :: DETA1,DETA2,ETA2
    REAL,                                     INTENT(IN)  :: PDTOP,PTOP
    REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(IN)  :: FIS,PD,SM
    REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME), INTENT(IN)  :: PINT,T,Q,U,V
    REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(OUT) :: PDYN,MSLP,SQWS
    REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(IN)  :: MEMBRANE_MSLP

    INTEGER :: ITF,JTF, i,j,k
    REAL :: DZ,RTOPP,APELP,A,TSFC

    REAL, PARAMETER :: LAPSR=6.5E-3, GI=1./G,D608=0.608
    REAL, PARAMETER :: COEF3=287.05*GI*LAPSR, COEF2=-1./COEF3
    REAL, PARAMETER :: TRG=2.0*R_D*GI,LAPSI=1.0/LAPSR

    REAL, DIMENSION(IMS:IME,JMS:JME,KMS:KME)              :: Z

    REAL :: densum,presum,density,pdyntemp,vpres

    ITF=MIN(ITE,IDE-1)
    JTF=MIN(JTE,JDE-1)

    

    DO J = JTS, MIN(JTE,JDE-1)
       DO I = ITS, MIN(ITE,IDE-1)
          Z(I,J,1)=FIS(I,J)*GI
       ENDDO
    ENDDO
    
    
    DO K = KTS,min(kde-1,KTE)
       DO J = JTS, MIN(JTE,JDE-1)
          DO I = ITS, MIN(ITE,IDE-1)
             APELP      = (PINT(I,J,K+1)+PINT(I,J,K))
             RTOPP      = TRG*T(I,J,K)*(1.0+Q(I,J,K)*P608)/APELP
             DZ         = RTOPP*(DETA1(K)*PDTOP+DETA2(K)*PD(I,J))
             Z(I,J,K+1) = Z(I,J,K) + DZ
          ENDDO
       ENDDO
    ENDDO

    
    
    

    DO J = JTS, MIN(JTE,JDE-1)
       DO I = ITS, MIN(ITE,IDE-1)
          TSFC      = T(I,J,1)*(1.+D608*Q(I,J,1)) + LAPSR*(Z(I,J,1)+Z(I,J,2))*0.5
          A         = LAPSR*Z(I,J,1)/TSFC
          MSLP(I,J) = PINT(I,J,1)*(1-A)**COEF2

          SQWS(I,J) =  (U(I,J,9)*U(I,J,9) + V(I,J,9)*V(I,J,9)           &
              +   U(I,J,10)*U(I,J,10) + V(I,J,10)*V(I,J,10)       &
              +   U(I,J,11)*U(I,J,11) + V(I,J,11)*V(I,J,11))/3.0

          PDYN(I,J) = 1.1*SQWS(I,J)/2.0 + MEMBRANE_MSLP(I,J)
       ENDDO
    ENDDO

  END SUBROUTINE STATS_MAKE_MSLP

  
  
  SUBROUTINE STATS_FOR_MOVE_123 (XLOC,YLOC,PRES,SM &
       ,RESTART,NTIME0                        & 
       ,MOVED,MVNEST,NTSD,NPHS,CFREQ          & 
       ,vortex_tracker                        &
       ,IDS,IDE,JDS,JDE,KDS,KDE               &
       ,IMS,IME,JMS,JME,KMS,KME               &
       ,ITS,ITE,JTS,JTE,KTS,KTE               )

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    USE MODULE_MODEL_CONSTANTS
    USE MODULE_DM
    USE MODULE_WRF_ERROR

    IMPLICIT NONE
    
    LOGICAL,INTENT(INOUT)                                 :: MVNEST  
    LOGICAL,INTENT(IN)                                    :: MOVED
    INTEGER,INTENT(IN)                                    :: vortex_tracker
    INTEGER,INTENT(IN)                                    :: IDS,IDE,JDS,JDE,KDS,KDE   &
         ,IMS,IME,JMS,JME,KMS,KME   &
         ,ITS,ITE,JTS,JTE,KTS,KTE   &
    ,NTSD,NPHS,CFREQ
    
    INTEGER, INTENT(OUT)                                  :: XLOC,YLOC
    INTEGER                                               :: NXLOC,NYLOC
    REAL                                                  :: NSUM1,NSUM2,NSUM3
    REAL, DIMENSION(IMS:IME,JMS:JME),         INTENT(IN)  :: SM,PRES

    
    

    character*256                                         :: message
    
    INTEGER,INTENT(INOUT)                                 :: NTIME0
    LOGICAL,INTENT(IN)                                    :: RESTART
    REAL :: far,weight,sr2,pmax,xfar,yfar,xshift,yshift
    integer :: cx,cy
    INTEGER                                               :: IM,JM,IP,JP
    INTEGER                                               :: I,K,J,XR,YR,DTMOVE,IDUM,JDUM,ITF,JTF
    REAL                                                  :: STMP0,STMP1
    REAL                                                  :: SMSUM,SMOUT,XDIFF,YDIFF,PCUT,PGR
    REAL                                                  :: MINGBL_PRES,MAXGBL_PRES,MAXGBL_SQWS
    REAL                                                  :: MINGBL_MIJ
    REAL, DIMENSION(IMS:IME,JMS:JME)                      :: MIJ

    
    
    
    ITF=min(ITE,IDE-1)
    JTF=min(JTE,JDE-1)

    
    
    MAXGBL_PRES=MAXVAL(PRES(ITS:ITF,JTS:JTF))
    CALL WRF_DM_MAXVAL(MAXGBL_PRES,IDUM,JDUM)
    MINGBL_PRES=MINVAL(PRES(ITS:ITF,JTS:JTF))
    CALL WRF_DM_MINVAL(MINGBL_PRES,IDUM,JDUM)
    PCUT = 0.5*(MAXGBL_PRES + MINGBL_PRES)
    
    IM=IDE/2 - IDE/6
    IP=IDE/2 + IDE/6
    JM=JDE/2 - JDE/4
    JP=JDE/2 + JDE/4
    
    DO J = JTS, MIN(JTE,JDE)
       DO I = ITS, MIN(ITE,IDE)
          IF(I .GE. IM .AND. I .LE. IP .AND. J .GE. JM .AND. J .LE. JP  &
               .AND. PCUT .GT. PRES(I,J))THEN
             MIJ(I,J) = PRES(I,J)
          ELSE
             MIJ(I,J) = 105000.0
          ENDIF
       ENDDO
    ENDDO

    
    old_tracker_1: if(vortex_tracker == 1) then
       
       
       
       STMP0=MAXGBL_PRES*100.                 
       MINGBL_MIJ=MINVAL(MIJ(ITS:ITF,JTS:JTF))
       DO J = JTS, MIN(JTE,JDE)
          DO I = ITS, MIN(ITE,IDE)
             IF(MIJ(I,J) .EQ. MINGBL_MIJ)THEN
                XLOC=I
                YLOC=J
                STMP0=PRES(I,J)
             ENDIF
          ENDDO
       ENDDO

       CALL WRF_DM_MINVAL(MINGBL_MIJ,XLOC,YLOC)
       CALL WRF_DM_MINVAL(STMP0,IDUM,JDUM)
    endif old_tracker_1
    


    
    hwrfx_tracker: if(vortex_tracker == 3) then
       

       NSUM1=0.0
       NSUM2=0.0
       NSUM3=0.0
       DO J = JTS, MIN(JTE,JDE)
          DO I = ITS, MIN(ITE,IDE)
             IF(I .GE. IM .AND. I .LE. IP .AND. J .GE. JM .AND. J .LE. JP )THEN
                
                NSUM1 = NSUM1 + I*(105000.1 - MIJ(I,J))
                NSUM2 = NSUM2 + J*(105000.1 - MIJ(I,J))
                NSUM3 = NSUM3 + (105000.1 - MIJ(I,J))
                
             ENDIF
          ENDDO
       ENDDO
       NSUM1 = WRF_DM_SUM_REAL(NSUM1)
       NSUM2 = WRF_DM_SUM_REAL(NSUM2)
       NSUM3 = WRF_DM_SUM_REAL(NSUM3)
       NXLOC = NINT(NSUM1/NSUM3)
       NYLOC = NINT(NSUM2/NSUM3)

       XLOC = NXLOC
       YLOC = NYLOC

       
       
       
       

    endif hwrfx_tracker
    

    
    
    old_tracker_2: if ( vortex_tracker == 1 ) then

       STMP1=0.0
       DO J = JTS, MIN(JTE,JDE)
          DO I = ITS, MIN(ITE,IDE)
             IF(I .EQ. XLOC+18)THEN
                XR=I
                YR=J
                STMP1=MIJ(I,J)
             ENDIF
          ENDDO
       ENDDO

       CALL WRF_DM_MAXVAL(STMP1,XR,YR)

       
       
       

       SMSUM = 0.0
       DO J = JTS, MIN(JTE,JDE)
          DO I = ITS, MIN(ITE,IDE)
             SMSUM = SMSUM + SM(I,J)
          ENDDO
       ENDDO

       SMOUT=WRF_DM_SUM_REAL(SMSUM)/(IDE*JDE)

       
       

       PGR=STMP1-STMP0
    endif old_tracker_2
    


    XDIFF=ABS(XLOC - IDE/2)
    YDIFF=ABS(YLOC - JDE/2)
    
    IF((.NOT.RESTART .AND. NTSD==0) .OR. MOVED)NTIME0=NTSD
    DTMOVE=NTSD-NTIME0                    
    
    
    
    if(vortex_tracker == 3) then
       
       IF(XDIFF .GE. 1 .OR. YDIFF .GE. 2) THEN
          MVNEST=.TRUE.
          NTIME0=NTSD
       ELSE
          
          MVNEST=.FALSE.
       ENDIF
    elseif(vortex_tracker==2) then
       
       
       MVNEST=.TRUE.
    elseif(vortex_tracker==1) then
       
       IF(DTMOVE .LE. 45 .OR. PGR .LE. 200.)THEN
          WRITE(message,*)'SUSPEND MOTION: SMALL DTMOVE OR WEAK PGF:','DTMOVE=',DTMOVE,'PGR=',PGR
          call wrf_debug(1,message)
          MVNEST=.FALSE.                               
       ELSE IF(STMP0 .GE. STMP1)THEN
          WRITE(message,*)'SUSPEND MOTION: THERE IS NO VORTEX IN THE DOMAIN:','STMP0=',STMP0,'STMP1=',STMP1
          call wrf_debug(1,message)
          MVNEST=.FALSE.
       ELSE IF(XDIFF .GT. 24 .OR. YDIFF .GT. 24)THEN
          WRITE(message,*)'SUSPEND MOTION: LOST VORTEX ','DTMOVE=',DTMOVE,'XDIFF=',XDIFF,'YDIFF=',YDIFF
          call wrf_debug(1,message)
          MVNEST=.FALSE.
       ELSE IF(SMOUT .LE. 0.2 .AND. XDIFF .GT. 12 .AND. YDIFF .GT. 12)THEN
          WRITE(message,*)'SUSPEND MOTION: VORTEX LOST OVER LAND ','DTMOVE=',DTMOVE,'XDIFF=',XDIFF,'YDIFF=',YDIFF
          call wrf_debug(1,message)
          MVNEST=.FALSE.
       ELSE IF(SMOUT .LE. 0.2 .AND. PGR .LE. 400.)THEN
          WRITE(message,*)'SUSPEND MOTION: VORTEX WEAK OVER LAND ','SMOUT=',SMOUT,'PGR=',PGR
          call wrf_debug(1,message)
          MVNEST=.FALSE.
       ELSE IF(SMOUT .LE. 0.2 .AND. DTMOVE .GE. 1500)THEN
          WRITE(message,*)'SUSPEND MOTION: STOP MOTION  OVER LAND','SMOUT=',SMOUT,'DTMOVE=',DTMOVE
          call wrf_debug(1,message)
          MVNEST=.FALSE.
       ELSE
          MVNEST=.TRUE.
       ENDIF
    else
       
       MVNEST=.false.
    endif

    RETURN

  END SUBROUTINE STATS_FOR_MOVE_123



END module module_stats_for_move
