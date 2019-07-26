module module_membrane_mslp
  implicit none
  private



  public :: make_membrane_mslp

  integer, parameter :: npres = 33
  real, parameter :: badheight=-9e9

  
  
  
  real, parameter :: post_stdpres(npres) = (/ 20000.,          &
       22500., 25000., 27500., 30000., 32500., 35000., 37500., 40000., &
       42500., 45000., 47500., 50000., 52500., 55000., 57500., 60000., &
       62500., 65000., 67500., 70000., 72500., 75000., 77500., 80000., &
       82500., 85000., 87500., 90000., 92500., 95000., 97500.,100000./)

  
  
  integer, parameter :: k850 = 27, k700=21, k500=13

  
  
  
  
  real, parameter :: post_istdpres(npres+1) = (/ 18750., &
       21250., 23750., 26250., 28750., 31250., 33750., 36250., 38750., &
       41250., 43750., 46250., 48750., 51250., 53750., 56250., 58750., &
       61250., 63750., 66250., 68750., 71250., 73750., 76250., 78750., &
       81250., 83750., 86250., 88750., 91250., 93750., 96250., 98750., &
       101250./)

  
  
  real, parameter :: post_H1=1.0
  real, parameter :: post_PQ0=379.90516
  real, parameter :: post_A2=17.2693882
  real, parameter :: post_A3=273.16
  real, parameter :: post_A4=35.86
  real, parameter :: post_D608=0.608
  real, parameter :: post_RD=287.04
  real, parameter :: post_G=9.81
  real, parameter :: post_GAMMA=6.5E-3
  real, parameter :: post_RGAMOG=post_RD*post_GAMMA/post_G
  real, parameter :: post_RHmin=1.0E-6     
  real, parameter :: post_smallQ=1.E-12

  real, parameter :: post_slope=-6.6e-4 

  REAL, PARAMETER :: old_COEF3=post_RD*post_SLOPE
  REAL, PARAMETER :: old_COEF2=-1./old_COEF3

contains

  subroutine make_membrane_mslp(grid)
    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
    implicit none
    type(domain), intent(inout) :: grid
    character*255 :: message

    integer :: IDS,IDE,JDS,JDE,KDS,KDE
    integer :: IMS,IME,JMS,JME,KMS,KME
    integer :: IPS,IPE,JPS,JPE,KPS,KPE

    
100 format('In module_membrane_mslp, post_stdpres(',A,')=',F0.3,' but should be ',F0.3)
    if(abs(post_stdpres(k850)-85000.)>1) then
       write(message,100) 'k850',post_stdpres(k850),85000.
       call wrf_error_fatal3("<stdin>",72,&
message)
    endif
    if(abs(post_stdpres(k700)-70000.)>1) then
       write(message,100) 'k850',post_stdpres(k700),70000.
       call wrf_error_fatal3("<stdin>",77,&
message)
    endif

    CALL get_ijk_from_grid (  grid ,      &
         ids, ide, jds, jde, kds, kde,    &
         ims, ime, jms, jme, kms, kme,    &
         ips, ipe, jps, jpe, kps, kpe    )

    call membrane_mslp_impl(grid,         &
         ids, ide, jds, jde, kds, kde,    &
         ims, ime, jms, jme, kms, kme,    &
         ips, ipe, jps, jpe, kps, kpe    )

  end subroutine make_membrane_mslp

  
  
  


  
  
  subroutine membrane_mslp_impl(grid, &
       IDS,IDE,JDS,JDE,KDS,KDE, &
       IMS,IME,JMS,JME,KMS,KME, &
       IPS,IPE,JPS,JPE,KPS,KPE)
    USE MODULE_DOMAIN, ONLY : domain
    USE MODULE_RELAX

    USE MODULE_COMM_DM, ONLY : HALO_NMM_MEMBRANE_INTERP_sub
    USE MODULE_DM, ONLY: ntasks_x, ntasks_y, mytask, ntasks, local_communicator
    use module_dm, only: wrf_dm_minval_real, wrf_dm_maxval_integer


    implicit none

    type(domain), intent(inout) :: grid

    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE

    real :: presTv(ips:ipe,jps:jpe,npres), Pmsl(ips:ipe,jps:jpe)
    real :: presZ(ips:ipe,jps:jpe,npres)

    real :: interP(ips:ipe,jps:jpe,npres+1), interZ(ips:ipe,jps:jpe,npres+1)

    logical :: ground_mask(ips:ipe,jps:jpe,npres)
    integer :: ground_level(ips:ipe,jps:jpe)
    integer :: ipres,i,j,mpres,imin,jmin,k,need_to_relax,imax,jmax
    real :: pmin
    character*255 :: message

    if(size(grid%p700rv)>1) then
       






CALL HALO_NMM_MEMBRANE_INTERP_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

    endif

    

    
    
    
    
    pmin=9e9
    imin=-99
    jmin=-99
    do j=max(jps,jds),min(jpe,jde-1)
       do i=max(ips,ids),min(ipe,ide-1)
          pmin=min(pmin,grid%pint(i,j,1))
          imin=i
          jmin=j
       enddo
    enddo
    call wrf_dm_minval_real(pmin,imin,jmin)

    
    

    
    
    call calculate_3D(grid,presTv,presZ,ground_mask,ground_level, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)

    

    
    
    
    
    

    
    
    
    
    
    
    
    
    
    
    
    
    

    grid%relaxmask=.true.

    
    do ipres=npres,1,-1
       
       
       need_to_relax=0
       do j=max(jps,jds+1),min(jde-2,jpe)
          do i=max(ips,ids+1),min(ide-2,ipe)
             grid%relaxmask(i,j)=ground_mask(i,j,ipres)
             if(grid%relaxmask(i,j)) need_to_relax=1
          enddo
       enddo

       
       call wrf_dm_maxval_integer(need_to_relax,imax,jmax)
       if(need_to_relax==0) then
 38       format('end mslp relax loop at ',I0)
          write(message,38) ipres
          call wrf_debug(1,message)
          exit
       endif

       
       do j=jps,min(jde-1,jpe)
          do i=ips,min(ide-1,ipe)
             grid%relaxwork(i,j)=presTv(i,j,ipres)
          enddo
       enddo

       
       call relax4e(grid,0.7,100,2, &
            IDS,IDE,JDS,JDE,KDS,KDE, &
            IMS,IME,JMS,JME,KMS,KME, &
            IPS,IPE,JPS,JPE,KPS,KPE)

       
       do j=jps,min(jde-1,jpe)
          do i=ips,min(ide-1,ipe)
             ground_mask(i,j,ipres)=grid%relaxmask(i,j)
          enddo
       enddo

       
       do j=jps,min(jde-1,jpe)
          do i=ips,min(ide-1,ipe)
             presTv(i,j,ipres)=grid%relaxwork(i,j)
          enddo
       enddo
    end do

    
    
    
    call calculate_interP(presTv,presZ,grid%Z,Pmsl,grid%PINT, &
         grid%T(:,:,1), grid%Q(:,:,1), &
         ground_level, ground_mask,grid%fis, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)

    
    do j=jps,min(jde-1,jpe)
       do i=ips,min(ide-1,ipe)
          grid%membrane_MSLP(i,j)=Pmsl(i,j)
       enddo
    enddo

    
    call smoothMSLP(grid,1, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)

    if(size(grid%p850z)>1) then
       
       do j=max(jds,jps),min(jde-1,jpe)
          do i=max(ids,ips),min(ide-1,ipe)
             grid%p850z(i,j)=presZ(i,j,k850)
             grid%p700z(i,j)=presZ(i,j,k700)
          enddo
       enddo
    endif

  end subroutine membrane_mslp_impl

  subroutine calculate_3D(grid,presTv,presZ,ground_mask,ground_level, &
       IDS,IDE,JDS,JDE,KDS,KDE, &
       IMS,IME,JMS,JME,KMS,KME, &
       IPS,IPE,JPS,JPE,KPS,KPE)
    USE MODULE_DOMAIN, ONLY : domain
    USE MODULE_DM, ONLY: ntasks_x, ntasks_y, mytask, ntasks, local_communicator
    USE MODULE_COMM_DM, ONLY : HALO_NMM_MEMBRANE_INTERP_sub
    use module_dm, only: wrf_dm_maxval_integer
    implicit none

    type(domain), intent(inout) :: grid

    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE

    real, intent(inout) :: presTv(ips:ipe,jps:jpe,npres)
    real, intent(inout) :: presZ(ips:ipe,jps:jpe,npres)

    logical, intent(inout) :: ground_mask(ips:ipe,jps:jpe,npres)
    integer, intent(inout) :: ground_level(ips:ipe,jps:jpe)

    integer :: Tkdest(ips:ipe,jps:jpe), Zkdest(ips:ipe,jps:jpe), Zbottom(ips:ipe,jps:jpe)
    integer :: i,j,ks,a,kd,k
    real :: weight, TL,QL,PL, tempT, RHL, TVRL, TVRBLO, TBLO,QBLO

    integer,target, dimension(ips:ipe,jps:jpe) :: ks850,ks700,ks500
    real, target,dimension(ips:ipe,jps:jpe) :: dummy1,dummy2
    integer, pointer, dimension(:,:) :: ksX
    integer :: nanfound
    real, pointer, dimension(:,:) :: preswind,presrv,presu,presv

    real :: Pmass(ips:ipe,jps:jpe,kds:kde)
    real :: numsum,densum,modelP1,modelP2,pdiff,presQ,presT,ZL,QSAT, U1, V1, U2, V2, dudy1,dvdx1, dudy2,dvdx2
    character*255 :: message
    logical :: wantuv
    






CALL HALO_NMM_MEMBRANE_INTERP_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


    
    
    ground_level=0
    ground_mask=.false.
    Zkdest=1
    Tkdest=1
    Zbottom=0

    ks850=0
    ks700=0
    ks500=0

    
    
    
    do ks=kde-1,kds,-1
       do j=jps,min(jde-1,jpe)
          iZ: do i=ips,min(ide-1,ipe)
             Pmass(i,j,ks)=sqrt(grid%PINT(i,j,ks)*grid%PINT(i,j,ks+1))
          enddo iZ
       enddo
    enddo

    
    
    do ks=kde-1,kds+1,-1
       do j=jps,min(jde-1,jpe)
          iTQ: do i=ips,min(ide-1,ipe)
             kd=Tkdest(i,j)
             if(kd<=npres) then
                innerTQ: do while(kd<=npres)
                   if(.not.(post_stdpres(kd)<=Pmass(i,j,ks-1) &
                        .and. post_stdpres(kd)>=Pmass(i,j,ks))) then
                      cycle iTQ
                   endif
                   weight=log(post_stdpres(kd)/Pmass(i,j,ks))/log(Pmass(i,j,ks-1)/Pmass(i,j,ks))

                   presZ(i,j,kd)=weight*grid%Z(i,j,ks-1) + (1.-weight)*grid%Z(i,j,ks)

                   presT=weight*grid%T(i,j,ks-1) + (1.-weight)*grid%T(i,j,ks)
                   presQ=weight*grid%Q(i,j,ks-1) + (1.-weight)*grid%Q(i,j,ks)
                   presTv(i,j,kd)=presT*(1.+post_D608*presQ)

                   if(kd==k850) then
                      ks850(i,j)=ks
                   elseif(kd==k700) then
                      ks700(i,j)=ks
                   elseif(kd==k500) then
                      ks500(i,j)=ks
                   endif

103                format('interp ks=',I0,' kd=',I0,' presT(i=',I0,',j=',I0,',kd)=',F0.3, &
                        ' between T(i,j,ks-1)=',F0.3,' and T(i,j,ks)=', &
                        F0.3,' using weight=',F0.3)
                   
104                format(' Pmass(i,j,ks)=',F0.3,' Pmass(i,j,ks-1)=',F0.3,' post_stdpres(kd)=',F0.3)
                   
                   if(weight<0 .or. weight>1) then
                      write(0,*) 'Bad weight: ',weight
                      call wrf_error_fatal3("<stdin>",394,&
'bad weight')
                   endif
                   kd=kd+1
                   Tkdest(i,j)=kd
                   Zkdest(i,j)=kd
                   Zbottom(i,j)=ks
                end do innerTQ
             end if
          end do iTQ
       end do
    end do

   
   
   do j=jps,min(jde-1,jpe)
      iTQ2: do i=ips,min(ide-1,ipe)
         kd=Zkdest(i,j)
         if(kd<=npres) then
            do while(kd<=npres)
               if(.not.(post_stdpres(kd)<=grid%PINT(i,j,kds) &
                    .and. post_stdpres(kd)>=Pmass(i,j,kds))) then
                  cycle iTQ2
               endif

               presT=grid%T(i,j,1)
               presQ=grid%Q(i,j,1)
               presTv(i,j,kd)=presT*(1.+post_D608*presQ)

               weight=log(post_stdpres(kd)/Pmass(i,j,kds))/log(grid%PINT(i,j,kds)/Pmass(i,j,kds))
               presZ(i,j,kd)=(1.-weight)*grid%Z(i,j,1)+weight*grid%fis(i,j)/post_g

               kd=kd+1
               Tkdest(i,j)=kd
               Zkdest(i,j)=kd
               Zbottom(i,j)=ks
            end do
         end if
      end do iTQ2
   end do

1234 format('grid ',I0,': size(',A,') = ',I0)
   write(message,1234) grid%id,'grid%p700rv',size(grid%p700rv)
   call wrf_message(trim(message))
   write(message,1234) grid%id,'grid%p700u',size(grid%p700u)
   call wrf_message(trim(message))

   wantuv=(grid%vortex_tracker == 7) 

   ifwind: if(size(grid%p700rv)>1 .or. size(grid%p700u)>1) then
    
    
    
    
    nullify(presu)
    nullify(presv)
    windloop: do k=0,2
       if(k==0) then
          
          kd=k500
          ksX=>ks500
          preswind=>dummy1
          presrv=>dummy2
          if(wantuv) then
             presu=>grid%p500u
             presv=>grid%p500v
          endif
       elseif(k==1) then
          ksX=>ks700
          preswind=>grid%p700wind
          presrv=>grid%p700rv
          kd=k700
          if(wantuv) then
             presu=>grid%p700u
             presv=>grid%p700v
          endif
       elseif(k==2) then
          ksX=>ks850
          kd=k850
          preswind=>grid%p850wind
          presrv=>grid%p850rv 
          if(wantuv) then
             presu=>grid%p850u
             presv=>grid%p850v
          endif
       endif

      
      if(jps<=jds) then
         do i=ips,min(ide-1,ipe)
            preswind(i,jds)=0
            presrv(i,jds)=0
         enddo
         if(wantuv) then
            do i=ips,min(ide-1,ipe)
               presu(i,jds)=0
               presv(i,jds)=0
            enddo
         endif
      endif
      if(jpe>=jde-1) then
         do i=ips,min(ide-1,ipe)
            preswind(i,jde-1)=0
            presrv(i,jde-1)=0
         enddo
         if(wantuv) then
            do i=ips,min(ide-1,ipe)
               presu(i,jde-1)=0
               presv(i,jde-1)=0
            enddo
         endif
      endif
      if(ips<=ids) then
         do j=jps,min(jde-1,jpe)
            preswind(ids,j)=0
            presrv(ids,j)=0
         enddo
         if(wantuv) then
            do j=jps,min(jde-1,jpe)
               presu(ids,j)=0
               presv(ids,j)=0
            enddo
         endif
      endif
      if(ipe>=ide-1) then
         do j=jps,min(jde-1,jpe)
            preswind(ide-1,j)=0
            presrv(ide-1,j)=0
         enddo
         if(wantuv) then
            do j=jps,min(jde-1,jpe)
               presu(ide-1,j)=0
               presv(ide-1,j)=0
            enddo
         endif
      endif

      
      do j=max(jps,jds+2),min(jde-2,jpe)
         a=mod(j,2)
         do i=max(ips,ids+2),min(ide-2,ipe)
            ks=ksX(i,j)
            if(ks>1) then
               
               weight=log(post_stdpres(kd)/Pmass(i,j,ks))/log(Pmass(i,j,ks-1)/Pmass(i,j,ks))

               U1=0.25*(grid%u(i,j-1,ks) + grid%u(i,j+1,ks) + grid%u(i-a,j,ks) + grid%u(i+1-a,j,ks))
               V1=0.25*(grid%v(i,j-1,ks) + grid%v(i,j+1,ks) + grid%v(i-a,j,ks) + grid%v(i+1-a,j,ks))
               U2=0.25*(grid%u(i,j-1,ks-1) + grid%u(i,j+1,ks-1) + grid%u(i-a,j,ks-1) + grid%u(i+1-a,j,ks-1))
               V2=0.25*(grid%v(i,j-1,ks-1) + grid%v(i,j+1,ks-1) + grid%v(i-a,j,ks-1) + grid%v(i+1-a,j,ks-1))
               
               dvdx1 = (grid%v(i+1-a,j,ks)-grid%v(i-a,j,ks))/(2.*grid%dx_nmm(i,j))
               dudy1 = (grid%u(i,j+1,ks)-grid%u(i,j-1,ks))/(2.*grid%dy_nmm)
               dvdx2 = (grid%v(i+1-a,j,ks-1)-grid%v(i-a,j,ks-1))/(2.*grid%dx_nmm(i,j))
               dudy2 = (grid%u(i,j+1,ks-1)-grid%u(i,j-1,ks-1))/(2.*grid%dy_nmm)

               if(wantuv) then
                  presu(i,j)=weight*u2+(1.-weight)*u1
                  presv(i,j)=weight*v2+(1.-weight)*v1
               endif
               preswind(i,j)=weight*sqrt(u2*u2+v2*v2) + (1.-weight)*sqrt(u1*u1+v1*v1)
               presrv(i,j)=(dvdx2-dudy2)*weight + (dvdx1-dudy1)*(1.-weight)
            elseif(post_stdpres(kd)>=Pmass(i,j,kds)) then
               
               ks=1
               U1=0.25*(grid%u(i,j-1,ks) + grid%u(i,j+1,ks) + grid%u(i-a,j,ks) + grid%u(i+1-a,j,ks))
               V1=0.25*(grid%v(i,j-1,ks) + grid%v(i,j+1,ks) + grid%v(i-a,j,ks) + grid%v(i+1-a,j,ks))
               
               dvdx1 = (grid%v(i+1-a,j,ks)-grid%v(i-a,j,ks))/(2.*grid%dx_nmm(i,j))
               dudy1 = (grid%u(i,j+1,ks)-grid%u(i,j-1,ks))/(2.*grid%dy_nmm)

               preswind(i,j)=sqrt(u1*u1 + v1*v1)
               presrv(i,j)=dvdx1-dudy1
               if(wantuv) then
                  presu(i,j)=u1
                  presv(i,j)=v1
               endif
            endif
         end do
      end do
   enddo windloop

   
   
   nanfound=0
   do j=max(jps,jds+1),min(jpe,jde-2)
      a=mod(j,2)
      do i=max(ips,ids+1),min(ipe,ide-2)
         grid%m10wind(i,j)=sqrt(grid%u10(i,j)*grid%u10(i,j) + grid%v10(i,j)*grid%v10(i,j))
         dvdx1 = 0.5*(grid%v10(i-a+1,j+1)-grid%v10(i-a,j+1) + &
                     grid%v10(i-a+1,j-1)-grid%v10(i-a,j-1)) / (2*grid%dx_nmm(i,j))
         dudy1 = 0.5*(grid%u10(i-a,j+1)-grid%u10(i-a,j-1) + &
                     grid%u10(i-a+1,j+1)-grid%u10(i-a+1,j-1)) / (2*grid%dy_nmm)
         grid%m10rv(i,j) = dvdx1 - dudy1
         if(grid%m10rv(i,j) == grid%m10rv(i,j)) then
            call wrf_debug(1000,'FIXME: REMOVE THIS CHECK')
         else
3088        format('NaN m10rv at i=',I0,' j=',I0,': a=',I0,' dx=',F0.3,' dy=',F0.3)
            write(message,3088) i,j,a,grid%dx_nmm(i,j),grid%dy_nmm
            call wrf_message2(trim(message))
3089        format('NaN m10rv at i=',I0,' j=',I0,': dvdx1=',F0.5,' dudy=',F0.5)
            write(message,3089) i,j,dvdx1,dudy1
            call wrf_message2(trim(message))
            nanfound=1
         endif
      enddo
   enddo
   call wrf_dm_maxval_integer(nanfound,i,j)
   if(nanfound/=0) then
      call wrf_error_fatal3("<stdin>",603,&
'ERROR: NaN m10rv seen; aborting.')
   endif
  elseif(grid%id==3) then
     call wrf_error_fatal3("<stdin>",607,&
'ERROR: NOT INTERPOLATING WIND')
  endif ifwind

    do j=jps,min(jde-1,jpe)
       do i=ips,min(ide-1,ipe)
          ground_level(i,j)=min(Zkdest(i,j),Tkdest(i,j))
       enddo
    enddo

    do kd=1,npres
       do j=jps,min(jde-1,jpe)
          do i=ips,min(ide-1,ipe)
             ground_mask(i,j,kd) = (kd>=ground_level(i,j))
          enddo
       enddo
    enddo

    
    
    jloop2: do j=jps,min(jde-1,jpe)
       iloop2: do i=ips,min(ide-1,ipe)
          if(ground_level(i,j)>npres) then
301          format('Extrap: i=',I0,' j=',I0,' NO EXTRAP: ground at ',I0)
             
             cycle iloop2
          else
302          format('Extrap: i=',I0,' j=',I0,' extrap from ',F0.3,' ground at ',I0)
             
          endif
          kloop2: do kd=ground_level(i,j),npres
             
             
             
             PL=grid%PINT(I,J,2)
             ZL=0.5*(grid%Z(I,J,2)+grid%Z(I,J,1))
             TL=0.5*(grid%T(I,J,2)+grid%T(I,J,1))
             QL=0.5*(grid%Q(I,J,2)+grid%Q(I,J,1))
             QSAT=post_PQ0/PL*EXP(post_A2*(TL-post_A3)/(TL-post_A4))
             
             RHL=QL/QSAT
             
             IF(RHL.GT.1.)THEN
                RHL=1.
                QL =RHL*QSAT
             ENDIF
             
             IF(RHL.LT.post_RHmin)THEN
                RHL=post_RHmin
                QL =RHL*QSAT
             ENDIF
             
             TVRL  =TL*(1.+post_D608*QL)
             TVRBLO=TVRL*(post_stdpres(kd)/PL)**post_RGAMOG
             TBLO  =TVRBLO/(1.+post_D608*QL)

             

             
             

             presTv(i,j,kd)=TBLO

             
             

             
             

             
             
             presZ(i,j,kd)=badheight

303          format('Extrap i=',I0,' j=',I0,' kd=',I0,' presTv=',F0.3,' presZ=',F0.3)
304          format('   TL=',F0.3,' QL=',F0.3,' ZL=',F0.3,' QSAT=',F0.3)
305          format('   TVRL=',F0.3,' TVRBLO=',F0.3,' TBLO=',F0.3,' RHL=',F0.3)
             
             
             
          enddo kloop2
       enddo iloop2
    enddo jloop2
  end subroutine calculate_3D

  subroutine calculate_interP( &
       presTv,presZ,modelZ,Pmsl,PINT,T1,Q1, &
       ground_level,ground_mask,fis, &
       IDS,IDE,JDS,JDE,KDS,KDE, &
       IMS,IME,JMS,JME,KMS,KME, &
       IPS,IPE,JPS,JPE,KPS,KPE)
    USE MODULE_DOMAIN, ONLY : domain

    implicit none

    integer, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE
    integer, intent(in) :: IMS,IME,JMS,JME,KMS,KME
    integer, intent(in) :: IPS,IPE,JPS,JPE,KPS,KPE

    real, intent(in) :: PINT(ims:ime,jms:jme,kms:kme), modelZ(ims:ime,jms:jme,kms:kme)
    real, intent(in) :: T1(ims:ime,jms:jme,1)
    real, intent(in) :: Q1(ims:ime,jms:jme,1)

    real, intent(in) :: fis(ims:ime,jms:jme)
    real, intent(out) :: Pmsl(ips:ipe,jps:jpe)
    real, intent(inout) :: presTv(ips:ipe,jps:jpe,npres)
    real, intent(inout) :: presZ(ips:ipe,jps:jpe,npres)

    logical, intent(inout) :: ground_mask(ips:ipe,jps:jpe,npres)
    integer, intent(inout) :: ground_level(ips:ipe,jps:jpe)

    real :: Z,midTv,dZ,newZ,P,newP,TVRT,TLYR,DIS,oa,slope
    integer :: kp,ip,i,j

    

    
    
    

    
    


    
    do j=jps,min(jde-1,jpe)
       iloop: do i=ips,min(ide-1,ipe)
          
          
          
          
          if(ground_level(i,j)<npres+1) then
             kp=ground_level(i,j)-1
101          format('i=',I0,' j=',I0,' kp=',I0,' ground level =',I0)
             
             if(kp<1) then
                call wrf_error_fatal3("<stdin>",742,&
"Lowest model surface pressure is lower than second lowest standard pressure level." )
             endif
             
             
             
             newZ=fis(i,j)/post_G
             newP=pint(i,j,1)
             do ip=kp,npres-1
                P=newP
                Z=newZ
                
                midTv=presTv(i,j,ip+1)
                newP=post_stdpres(ip+1)
                dZ=post_Rd*midTv*alog(P/newP)/post_g
102             format('  make some Z at ip=',I0,': P=',F0.3,' newP=',F0.3)
1021            format('  Z=',F0.3,' midTv=',F0.3,' dZ=',F0.3)
                
                
                if(dZ>=0.) then
                   call wrf_error_fatal3("<stdin>",762,&
"dZ>=0.")
                endif
                newZ=Z+dZ
                presZ(i,j,ip+1)=newZ
                if(newZ<=0) then
                   
1022               format('  extrap using ',F0.3,'/exp(-',F0.3,'*',F0.3,'/(',F0.3,'*',F0.3,'))')
                   


                   
                   Pmsl(i,j)=(Z*newP-newZ*P)/(-dZ)
10221              format('  result: ',F0.3)
                   

                   cycle iloop
                endif
             enddo
          endif
          
          

          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
1025      format('  use npres: TLYR=',F0.3,' Pmsl=',F0.3)
1026      format('     result: ',F0.3,'/EXP(-',F0.3,'*',F0.3,'/(',F0.3,'*',F0.3,'))')
          TLYR=presTv(I,J,npres)-presZ(I,J,npres)*post_SLOPE*post_G*0.5
          Pmsl(I,J)=post_stdpres(npres)/EXP(-presZ(I,J,npres)*post_G/(post_RD*TLYR))
          
          
          
          

          
          
       enddo iloop
    enddo
  end subroutine calculate_interP

  subroutine smoothMSLP(grid,iterations,  &
       IDS,IDE,JDS,JDE,KDS,KDE, &
       IMS,IME,JMS,JME,KMS,KME, &
       IPS,IPE,JPS,JPE,KPS,KPE)
    use module_relax
    USE MODULE_DOMAIN, ONLY : domain
    implicit none
    type(domain), intent(inout) :: grid
    integer, intent(in) :: iterations

    integer :: IDS,IDE,JDS,JDE,KDS,KDE
    integer :: IMS,IME,JMS,JME,KMS,KME
    integer :: IPS,IPE,JPS,JPE,KPS,KPE
    integer :: i,j

    do j=jps,min(jde-1,jpe)
       do i=ips,min(ide-1,ipe)
          grid%relaxmask(i,j)=.true.
          grid%relaxwork(i,j)=grid%membrane_mslp(i,j)
       enddo
    enddo

    call relax4e(grid,0.5,iterations,0, &
         IDS,IDE,JDS,JDE,KDS,KDE, &
         IMS,IME,JMS,JME,KMS,KME, &
         IPS,IPE,JPS,JPE,KPS,KPE)

    do j=jps,min(jde-1,jpe)
       do i=ips,min(ide-1,ipe)
          grid%membrane_mslp(i,j)=grid%relaxwork(i,j)
       enddo
    enddo

  end subroutine smoothMSLP

end module module_membrane_mslp
