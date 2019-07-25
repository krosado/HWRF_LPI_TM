! advance.f

! advance POM

!_______________________________________________________________________
      subroutine advance(icoupling) !RMY: added icoupling control
! advance POM 1 step in time
      implicit none
      include 'pom.h'
      integer icoupling !RMY: added icoupling
      integer i,j !RMY: added i,j

! get time
      call get_time

! set time dependent surface boundary conditions
      if(icoupling.ne.1) call surface_forcing !RMY: added icoupling flag

      if(ionedim.ne.1) then !RMY: added calculation of tauxi and tauyi
        do j=1,jmm1
          do i=1,imm1
            tauxi(i,j)=-rho_0*.5e0*(wusurf(i,j)+wusurf(i+1,j))
            tauyi(i,j)=-rho_0*.5e0*(wvsurf(i,j)+wvsurf(i,j+1))
          end do
        end do
        call exchange2d_mpi(tauxi,im,jm)
        call exchange2d_mpi(tauyi,im,jm)
      else
        tauxi(i,j)=-rho_0*wusurf(i,j)
        tauyi(i,j)=-rho_0*wvsurf(i,j)
      end if

! set time dependent lateral boundary conditions
!RMY      call lateral_bc !RMY: as_is(ecoast),x(seamt+stide)

! set lateral viscosity
      if(ionedim.ne.1) call lateral_viscosity !RMY: added ionedim flag

! form vertical averages of 3-D fields for use in external (2-D) mode
      if(ionedim.ne.1) call mode_interaction !RMY: added ionedim flag

! external (2-D) mode calculation
      if(ionedim.ne.1) then !RMY: added ionedim flag
        do iext=1,isplit
          call mode_external
        end do
      end if !RMY: end of ionedim-controlled if-statement

! internal (3-D) mode calculation
      call mode_internal

! print section
      call print_section

! write output !RMY: output is on sigma levels - use m-files in pomviz
      if(netcdf_file.ne.'nonetcdf' .and. mod(iint,iprint).eq.0)
     $                                         call write_output_pnetcdf
      if(netcdf_file.ne.'nonetcdf' .and. mod(iint,iprint2).eq.0)
     $                              call write_output2_pnetcdf !RMY: new

! write restart !RMY: add tauavr+ for avrtau?
      if(mod(iint,irestart).eq.0) call write_restart_pnetcdf

! check CFL condition
      call check_velocity

      return
      end

!_______________________________________________________________________
      subroutine get_time
! return the model time
      implicit none
      include 'pom.h'
      time=dti*float(iint)/86400.e0+time0
      if(iint.gt.iswtch) iprint=nint(prtd2*24.e0*3600.e0/dti)
      if(lramp) then
        ramp=time/period
        if(ramp.gt.1.e0) ramp=1.e0
      else
        ramp=1.e0
      endif
      return
      end

!_______________________________________________________________________
      subroutine surface_forcing
! set time dependent surface boundary conditions
      implicit none
      include 'pom.h'
      integer i,j
      real tatm,satm

!RMY: reset fluxes to zero when not coupling to atmosphere
        do j=2,jmm1
          do i=2,imm1
            wtsurf(i,j)=0.e0
            swrad(i,j)=0.e0
          end do
        end do

        do j=1,jm
          do i=1,im
            taux(i,j)=0.e0
            tauy(i,j)=0.e0
            wusurf(i,j)=0.e0
            wvsurf(i,j)=0.e0
            mdp(i,j)=0.e0 !RMY: use test value if not coupling to waves
            whs(i,j)=2.e0 !RMY: use test value if not coupling to waves
          enddo
        enddo

!RMY: added tnowindd to determine if prescribed wind forcing is applied
        if(time.lt.tnowindd) then
          call windmsg !RMY: call windmsg instead of wind
        end if

        do j=1,jm
          do i=1,im
            if(taux(i,j).gt.0.1.and.nbct.eq.3) nbct=1
          end do
        end do

! wind forcing is suplied in subroutine wind
!RMY      call wind !RMY: as_is(ecoast),x(seamt+stide)
! heat fluxes are suplied in subroutine heat
!      call heat !RMY: as_is(ecoast),x(seamt+stide)
! water fluxes are suplied in subroutine water
!      call water !RMY: as_is(ecoast),x(seamt+stide)
      return !RMY: 1st_calc_w<x>surf,e_atmos,vfluxf,w,swrad(seamt+stide)
      end

!_______________________________________________________________________
      subroutine lateral_viscosity
! set the lateral viscosity
      implicit none
      include 'pom.h'
      integer i,j,k
! if mode=2 then initial values of aam2d are used. If one wishes
! to use Smagorinsky lateral viscosity and diffusion for an
! external (2-D) mode calculation, then appropiate code can be
! adapted from that below and installed just before the end of the
! "if(mode.eq.2)" loop in subroutine advave

! calculate Smagorinsky lateral viscosity:
! ( hor visc = horcon*dx*dy*sqrt((du/dx)**2+(dv/dy)**2
!                                +.5*(du/dy+dv/dx)**2) )
      if(mode.ne.2) then
        call advct

        if (npg.eq.1) then
          call baropg
        else if (npg.eq.2) then
          call baropg_mcc
        else
          error_status=1
          write(6,'(/''Error: invalid value for npg'')')
        end if

        do k=1,kbm1
          do j=2,jmm1
            do i=2,imm1
              aam(i,j,k)=horcon*dx(i,j)*dy(i,j)
     $                    *sqrt( ((u(i+1,j,k)-u(i,j,k))/dx(i,j))**2
     $                          +((v(i,j+1,k)-v(i,j,k))/dy(i,j))**2
     $                    +.5e0*(.25e0*(u(i,j+1,k)+u(i+1,j+1,k)
     $                                 -u(i,j-1,k)-u(i+1,j-1,k))
     $                    /dy(i,j)
     $                    +.25e0*(v(i+1,j,k)+v(i+1,j+1,k)
     $                           -v(i-1,j,k)-v(i-1,j+1,k))
     $                    /dx(i,j)) **2)
            end do
          end do
        end do !RMY: as_is(ecoast+stide),create_aam_sponge_zones(seamt)
        call exchange3d_mpi(aam(:,:,1:kbm1),im,jm,kbm1)
      end if

      return
      end

!_______________________________________________________________________
      subroutine mode_interaction
! form vertical averages of 3-D fields for use in external (2-D) mode
      implicit none
      include 'pom.h'
      integer i,j,k

      if(mode.ne.2) then

        do j=1,jm
          do i=1,im
            adx2d(i,j)=0.e0
            ady2d(i,j)=0.e0
            drx2d(i,j)=0.e0
            dry2d(i,j)=0.e0
            aam2d(i,j)=0.e0
          end do
        end do

        do k=1,kbm1
          do j=1,jm
            do i=1,im
              adx2d(i,j)=adx2d(i,j)+advx(i,j,k)*dz(k)
              ady2d(i,j)=ady2d(i,j)+advy(i,j,k)*dz(k)
              drx2d(i,j)=drx2d(i,j)+drhox(i,j,k)*dz(k)
              dry2d(i,j)=dry2d(i,j)+drhoy(i,j,k)*dz(k)
              aam2d(i,j)=aam2d(i,j)+aam(i,j,k)*dz(k)
            end do
          end do
        end do

        call advave

        call exchange2d_mpi(advua,im,jm) !RMY: add advua exchange here
        call exchange2d_mpi(advva,im,jm) !RMY: add advva exchange here

        do j=1,jm
          do i=1,im
            adx2d(i,j)=adx2d(i,j)-advua(i,j)
            ady2d(i,j)=ady2d(i,j)-advva(i,j)
          end do
        end do

        call exchange2d_mpi(adx2d,im,jm) !RMY: add adx2d exchange here
        call exchange2d_mpi(ady2d,im,jm) !RMY: add ady2d exchange here

      end if

      do j=1,jm
        do i=1,im
          egf(i,j)=el(i,j)*ispi
        end do
      end do

      do j=1,jm
        do i=2,im
          utf(i,j)=ua(i,j)*(d(i,j)+d(i-1,j))*isp2i
        end do
      end do
      do j=2,jm
        do i=1,im
          vtf(i,j)=va(i,j)*(d(i,j)+d(i,j-1))*isp2i
        end do
      end do

      call exchange2d_mpi(utf,im,jm) !RMY: add utf exchange here
      call exchange2d_mpi(vtf,im,jm) !RMY: add vtf exchange here

      return
      end

!_______________________________________________________________________
      subroutine mode_external
! calculate the external (2-D) mode
      implicit none
      include 'pom.h'
      integer i,j

      do j=2,jm
        do i=2,im
          fluxua(i,j)=.25e0*(d(i,j)+d(i-1,j))
     $                 *(dy(i,j)+dy(i-1,j))*ua(i,j)
          fluxva(i,j)=.25e0*(d(i,j)+d(i,j-1))
     $                 *(dx(i,j)+dx(i,j-1))*va(i,j)
        end do
      end do

      call exchange2d_mpi(fluxua,im,jm) !RMY: add fluxua exchange here
      call exchange2d_mpi(fluxva,im,jm) !RMY: add fluxva exchange here

! NOTE addition of surface freshwater flux, w(i,j,1)=vflux, compared
! with pom98.f. See also modifications to subroutine vertvl
      do j=2,jmm1
        do i=2,imm1
          elf(i,j)=elb(i,j)
     $              +dte2*(-(fluxua(i+1,j)-fluxua(i,j)
     $                      +fluxva(i,j+1)-fluxva(i,j))/art(i,j)
     $                      -vfluxf(i,j))
        end do
      end do

      call bcond(1)

      call exchange2d_mpi(elf,im,jm)

      if(mod(iext,ispadv).eq.0) then !RMY: modify if-statement for exch
        call advave
        call exchange2d_mpi(advua,im,jm) !RMY: add advua exchange here
        call exchange2d_mpi(advva,im,jm) !RMY: add advva exchange here
      end if

      do j=2,jmm1
        do i=2,im
          uaf(i,j)=adx2d(i,j)+advua(i,j)
     $              -aru(i,j)*.25e0
     $                *(cor(i,j)*d(i,j)*(va(i,j+1)+va(i,j))
     $                 +cor(i-1,j)*d(i-1,j)*(va(i-1,j+1)+va(i-1,j)))
     $              +.25e0*grav*(dy(i,j)+dy(i-1,j))
     $                *(d(i,j)+d(i-1,j))
     $                *((1.e0-2.e0*alpha)
     $                   *(el(i,j)-el(i-1,j))
     $                  +alpha*(elb(i,j)-elb(i-1,j)
     $                         +elf(i,j)-elf(i-1,j))
     $                  +e_atmos(i,j)-e_atmos(i-1,j))
     $              +drx2d(i,j)+aru(i,j)*(wusurf(i,j)-wubot(i,j))
        end do
      end do

      do j=2,jmm1
        do i=2,im
          uaf(i,j)=((h(i,j)+elb(i,j)+h(i-1,j)+elb(i-1,j))
     $                *aru(i,j)*uab(i,j)
     $              -4.e0*dte*uaf(i,j))
     $             /((h(i,j)+elf(i,j)+h(i-1,j)+elf(i-1,j))
     $                 *aru(i,j))
        end do
      end do

      do j=2,jm
        do i=2,imm1
          vaf(i,j)=ady2d(i,j)+advva(i,j)
     $              +arv(i,j)*.25e0
     $                *(cor(i,j)*d(i,j)*(ua(i+1,j)+ua(i,j))
     $               +cor(i,j-1)*d(i,j-1)*(ua(i+1,j-1)+ua(i,j-1)))
     $              +.25e0*grav*(dx(i,j)+dx(i,j-1))
     $                *(d(i,j)+d(i,j-1))
     $                *((1.e0-2.e0*alpha)*(el(i,j)-el(i,j-1))
     $                  +alpha*(elb(i,j)-elb(i,j-1)
     $                         +elf(i,j)-elf(i,j-1))
     $                  +e_atmos(i,j)-e_atmos(i,j-1))
     $              +dry2d(i,j)+arv(i,j)*(wvsurf(i,j)-wvbot(i,j))
        end do
      end do

      do j=2,jm
        do i=2,imm1
          vaf(i,j)=((h(i,j)+elb(i,j)+h(i,j-1)+elb(i,j-1))
     $                *arv(i,j)*vab(i,j)
     $              -4.e0*dte*vaf(i,j))
     $             /((h(i,j)+elf(i,j)+h(i,j-1)+elf(i,j-1))
     $                 *arv(i,j))
        end do
      end do

      call bcond(2)

      call exchange2d_mpi(uaf,im,jm)
      call exchange2d_mpi(vaf,im,jm)

      if(iext.eq.(isplit-2))then
        do j=1,jm
          do i=1,im
            etf(i,j)=.25e0*smoth*elf(i,j)
          end do
        end do

      else if(iext.eq.(isplit-1)) then

        do j=1,jm
          do i=1,im
            etf(i,j)=etf(i,j)+.5e0*(1.-.5e0*smoth)*elf(i,j)
          end do
        end do

      else if(iext.eq.isplit) then

        do j=1,jm
          do i=1,im
            etf(i,j)=(etf(i,j)+.5e0*elf(i,j))*fsm(i,j)
          end do
        end do

      end if

! apply filter to remove time split
      do j=1,jm
        do i=1,im
          ua(i,j)=ua(i,j)+.5e0*smoth*(uab(i,j)-2.e0*ua(i,j)+uaf(i,j))
          va(i,j)=va(i,j)+.5e0*smoth*(vab(i,j)-2.e0*va(i,j)+vaf(i,j))
          el(i,j)=el(i,j)+.5e0*smoth*(elb(i,j)-2.e0*el(i,j)+elf(i,j))
          elb(i,j)=el(i,j)
          el(i,j)=elf(i,j)
          d(i,j)=h(i,j)+el(i,j)
          uab(i,j)=ua(i,j)
          ua(i,j)=uaf(i,j)
          vab(i,j)=va(i,j)
          va(i,j)=vaf(i,j)
        end do
      end do

      if(iext.ne.isplit) then
        do j=1,jm
          do i=1,im
            egf(i,j)=egf(i,j)+el(i,j)*ispi
          end do
        end do
        do j=1,jm
          do i=2,im
            utf(i,j)=utf(i,j)+ua(i,j)*(d(i,j)+d(i-1,j))*isp2i
          end do
        end do
        do j=2,jm
          do i=1,im
            vtf(i,j)=vtf(i,j)+va(i,j)*(d(i,j)+d(i,j-1))*isp2i
          end do
        end do
        call exchange2d_mpi(utf,im,jm) !RMY: add utf exchange here
        call exchange2d_mpi(vtf,im,jm) !RMY: add vtf exchange here
      end if

      return
      end

!_______________________________________________________________________
      subroutine mode_internal
! calculate the internal (3-D) mode
      implicit none
      include 'pom.h'
      integer i,j,k

      if((iint.ne.1.or.time0.ne.0.e0).and.mode.ne.2) then

       if(ionedim.ne.1) then !RMY: added ionedim flag
! adjust u(z) and v(z) such that depth average of (u,v) = (ua,va)
        do j=1,jm
          do i=1,im
            tps(i,j)=0.e0
          end do
        end do

        do k=1,kbm1
          do j=1,jm
            do i=1,im
              tps(i,j)=tps(i,j)+u(i,j,k)*dz(k)
            end do
          end do
        end do

        do k=1,kbm1
          do j=1,jm
            do i=2,im
              u(i,j,k)=(u(i,j,k)-tps(i,j))+
     $                 (utb(i,j)+utf(i,j))/(dt(i,j)+dt(i-1,j))
            end do
          end do
        end do

        do j=1,jm
          do i=1,im
            tps(i,j)=0.e0
          end do
        end do

        do k=1,kbm1
          do j=1,jm
            do i=1,im
              tps(i,j)=tps(i,j)+v(i,j,k)*dz(k)
            end do
          end do
        end do

        do k=1,kbm1
          do j=2,jm
            do i=1,im
              v(i,j,k)=(v(i,j,k)-tps(i,j))+
     $                 (vtb(i,j)+vtf(i,j))/(dt(i,j)+dt(i,j-1))
            end do
          end do
        end do

        call exchange3d_mpi(u(:,:,1:kbm1),im,jm,kbm1) !RMY: add u exch
        call exchange3d_mpi(v(:,:,1:kbm1),im,jm,kbm1) !RMY: add v exch

! calculate w from u, v, dt (h+et), etf and etb
        call vertvl

        call bcond(5) !RMY: bcondorl(5)(ecoast),bcond(5)(seamt+stide)

        call exchange3d_mpi(w,im,jm,kb)
       end if !RMY: end of ionedim-controlled if-statement

! set uf and vf to zero
        do k=1,kb
          do j=1,jm
            do i=1,im
              uf(i,j,k)=0.e0
              vf(i,j,k)=0.e0
            end do
          end do
        end do

!RMY: could introduce imay functionality for something other than M-Y
! calculate q2f and q2lf using uf, vf, a and c as temporary variables
       if(ionedim.ne.1) then !RMY: added ionedim flag
        call advq(q2b,q2,uf)
        call advq(q2lb,q2l,vf)
       else !RMY: step forward in time as if advq was used
        do k=2,kbm1
          do j=2,jmm1
            do i=2,imm1
              uf(i,j,k)=((h(i,j)+etb(i,j))*art(i,j)
     $                   *q2b(i,j,k)-dti2*uf(i,j,k))
     $                  /((h(i,j)+etf(i,j))*art(i,j))
              vf(i,j,k)=((h(i,j)+etb(i,j))*art(i,j)
     $                   *q2lb(i,j,k)-dti2*vf(i,j,k))
     $                  /((h(i,j)+etf(i,j))*art(i,j))
            end do
          end do
        end do
       end if !RMY: end of ionedim-controlled if-statement
        call profq

        call bcond(6)

        call exchange3d_mpi(uf(:,:,2:kbm1),im,jm,kbm2)
        call exchange3d_mpi(vf(:,:,2:kbm1),im,jm,kbm2)

        do k=1,kb
          do j=1,jm
            do i=1,im
              q2(i,j,k)=q2(i,j,k)
     $                   +.5e0*smoth*(uf(i,j,k)+q2b(i,j,k)
     $                                -2.e0*q2(i,j,k))
              q2l(i,j,k)=q2l(i,j,k)
     $                   +.5e0*smoth*(vf(i,j,k)+q2lb(i,j,k)
     $                                -2.e0*q2l(i,j,k))
              q2b(i,j,k)=q2(i,j,k)
              q2(i,j,k)=uf(i,j,k)
              q2lb(i,j,k)=q2l(i,j,k)
              q2l(i,j,k)=vf(i,j,k)
            end do
          end do
        end do
!RMY: end of section that could include imay functionality

!RMY: reset uf and vf to zero
        do k=1,kb
          do j=1,jm
            do i=1,im
              uf(i,j,k)=0.e0
              vf(i,j,k)=0.e0
            end do
          end do
        end do !RMY: end of new section to reset uf and vf to zero

! calculate tf and sf using uf, vf, a and c as temporary variables
        if(mode.ne.4) then
         if(ionedim.ne.1) then !RMY: added ionedim flag
          if(nadv.eq.1) then
            call advt1(tb,t,tclim,uf)
            call advt1(sb,s,sclim,vf)
          else if(nadv.eq.2) then
            call advt2(tb,t,tclim,uf)
            call advt2(sb,s,sclim,vf)
          else
            error_status=1
            write(6,'(/''Error: invalid value for nadv'')')
          end if
         else !RMY: step forward in time as if advt1 was used
          do j=1,jm
            do i=1,im
               t(i,j,kb)=t(i,j,kbm1)
               tb(i,j,kb)=tb(i,j,kbm1)
               s(i,j,kb)=s(i,j,kbm1)
               sb(i,j,kb)=sb(i,j,kbm1)
            end do
          end do
          do j=2,jmm1
            do i=2,imm1
              do k=1,kbm1
                uf(i,j,k)=(tb(i,j,k)*dble((h(i,j)+etb(i,j))*art(i,j))
     $                 -dti2*uf(i,j,k))/dble((h(i,j)+etf(i,j))*art(i,j))
                vf(i,j,k)=(sb(i,j,k)*dble((h(i,j)+etb(i,j))*art(i,j))
     $                 -dti2*vf(i,j,k))/dble((h(i,j)+etf(i,j))*art(i,j))
              end do
            end do
          end do
         end if !RMY: end of ionedim-controlled if-statement

          call exchange3d_mpi(uf(:,:,1:kbm1),im,jm,kbm1)
          call exchange3d_mpi(vf(:,:,1:kbm1),im,jm,kbm1)

          call proft(uf,wtsurf,tsurf,nbct)
          call proft(vf,wssurf,ssurf,nbcs)

         if(ionedim.ne.1) then !RMY: added ionedim flag
          call bcond(4)

!RMY: adding pomtc code here for smoothing/desmoothing; is mpi ok?
          if((amod(float(iint),smh).eq.0.0).and.(ismoth.eq.1)) then

            do k=1,kbm1
              do j=1,jm
                do i=1,im
                  uf(i,j,k)=uf(i,j,k)-tbin(i,j,k)
                end do
              end do
            end do

            do k=1,kbm1
              call smoothing(uf(1,1,k),im,jm,0.25,fsm,
     $                       n_west,n_east,n_south,n_north)
              call smoothing(uf(1,1,k),im,jm,-0.27,fsm,
     $                       n_west,n_east,n_south,n_north)
              call smoothing(vf(1,1,k),im,jm,0.25,fsm,
     $                       n_west,n_east,n_south,n_north)
              call smoothing(vf(1,1,k),im,jm,-0.27,fsm,
     $                       n_west,n_east,n_south,n_north)
            end do

            do k=1,kbm1
              do j=1,jm
                do i=1,im
                  uf(i,j,k)=uf(i,j,k)+tbin(i,j,k)
                end do
              end do
            end do

          endif
!RMY: end of pomtc code for smoothing/desmoothing

!RMY: adding pomtc code here for eliminating T (uf) growth
          do j=2,jm-1
            do i=2,im-1
              if(uf(i,j,1).gt.t(i,j,1)) then
                if(uf(i,j,1).ge.t(i-1,j,1).and.
     1             uf(i,j,1).ge.t(i+1,j,1).and.
     2             uf(i,j,1).ge.t(i,j-1,1).and.
     3             uf(i,j,1).ge.t(i,j+1,1).and.
     4             uf(i,j,1).ge.t(i,j,2)) uf(i,j,1)=t(i,j,1)
              end if
            end do
          end do

          do k=2,kb-1
            do j=2,jm-1
              do i=2,im-1
                if(uf(i,j,k).gt.t(i,j,k)) then
                  if(uf(i,j,k).ge.t(i,j,k-1).and.
     1               uf(i,j,k).ge.t(i-1,j,k).and.
     2               uf(i,j,k).ge.t(i+1,j,k).and.
     3               uf(i,j,k).ge.t(i,j-1,k).and.
     4               uf(i,j,k).ge.t(i,j+1,k).and.
     5               uf(i,j,k).ge.t(i,j,k+1)) uf(i,j,k)=t(i,j,k)
                end if
              end do
            end do 
          end do

          do j=2,jm-1
            do i=2,im-1
              if(uf(i,j,kb).gt.t(i,j,kb)) then
                if(uf(i,j,kb).ge.t(i-1,j,kb).and.
     1             uf(i,j,kb).ge.t(i+1,j,kb).and.
     2             uf(i,j,kb).ge.t(i,j-1,kb).and.
     3             uf(i,j,kb).ge.t(i,j+1,kb).and.
     4             uf(i,j,kb).ge.t(i,j,kb-1)) uf(i,j,kb)=t(i,j,kb)
              end if
            end do
          end do

          call exchange3d_mpi(uf(:,:,1:kb),im,jm,kb)
!RMY: end of pomtc code for eliminating T (uf) growth

         else
          call bcond(7) !RMY: a-grid
         end if !RMY: end of ionedim-controlled if-statement

          do k=1,kb
            do j=1,jm
              do i=1,im
                t(i,j,k)=t(i,j,k)
     $                    +.5e0*smoth*(uf(i,j,k)+tb(i,j,k)
     $                                 -2.e0*t(i,j,k))
                s(i,j,k)=s(i,j,k)
     $                    +.5e0*smoth*(vf(i,j,k)+sb(i,j,k)
     $                                 -2.e0*s(i,j,k))
                tb(i,j,k)=t(i,j,k)
                t(i,j,k)=uf(i,j,k)
                sb(i,j,k)=s(i,j,k)
                s(i,j,k)=vf(i,j,k)
              end do
            end do
          end do

          ! restore temperature and salinity
!RMY          call restore_interior !RMY: as_is(ecoast),x(seamt+stide)

          call dens(s,t,rho)

        end if

! calculate uf and vf
       if(ionedim.ne.1) then !RMY: added ionedim flag
        call advu
        call advv
       else !RMY: step forward in time as if advu and advv were used
        do k=1,kb
          do j=1,jm
            do i=1,im
              uf(i,j,k)=0.e0
              vf(i,j,k)=0.e0
            end do
          end do
        end do
        do k=1,kbm1
          do j=2,jmm1
            do i=2,imm1
              uf(i,j,k)=-aru(i,j)*cor(i,j)*dt(i,j)*v(i,j,k) !RMY: a-grid
              vf(i,j,k)=arv(i,j)*cor(i,j)*dt(i,j)*u(i,j,k)  !RMY: a-grid
            end do
          end do
        end do
        do k=1,kbm1
          do j=2,jmm1
            do i=2,imm1
              uf(i,j,k)=((h(i,j)+etb(i,j))*aru(i,j)*ub(i,j,k)
     $                   -dti2*uf(i,j,k))
     $                  /((h(i,j)+etf(i,j))*aru(i,j))       !RMY: a-grid
              vf(i,j,k)=((h(i,j)+etb(i,j))*arv(i,j)*vb(i,j,k)
     $                   -dti2*vf(i,j,k))
     $                  /((h(i,j)+etf(i,j))*arv(i,j))       !RMY: a-grid
            end do
          end do
        end do
       end if !RMY: end of ionedim-controlled if-statement
        call profu
        call profv

       if(ionedim.ne.1) then !RMY: added ionedim flag
        call bcond(3) !RMY: bcondorl(3)(ecoast),bcond(3)(seamt+stide)

        call exchange3d_mpi(uf(:,:,1:kbm1),im,jm,kbm1)
        call exchange3d_mpi(vf(:,:,1:kbm1),im,jm,kbm1)

!RMY: adding pomtc code here for smoothing/desmoothing; is mpi ok?
!RMY: not sure if smoothing/desmoothing code goes before/after exchange
        if((amod(float(iint),smh*3.).eq.0.0).and.(ismoth.eq.1)) then
          do k=1,kbm1
            call smoothing(uf(1,1,k),im,jm,0.25,fsm,
     $                       n_west,n_east,n_south,n_north)
            call smoothing(uf(1,1,k),im,jm,-0.25,fsm,
     $                       n_west,n_east,n_south,n_north)
            call smoothing(vf(1,1,k),im,jm,0.25,fsm,
     $                       n_west,n_east,n_south,n_north)
            call smoothing(vf(1,1,k),im,jm,-0.25,fsm,
     $                       n_west,n_east,n_south,n_north)
          enddo
        endif
!RMY: end of pomtc code for smoothing/desmoothing

       else
        call bcond(7) !RMY: a-grid

        call exchange3d_mpi(uf(:,:,1:kbm1),im,jm,kbm1)
        call exchange3d_mpi(vf(:,:,1:kbm1),im,jm,kbm1)
       end if !RMY: end of ionedim-controlled if-statement

        do j=1,jm
          do i=1,im
            tps(i,j)=0.e0
          end do
        end do

        do k=1,kbm1
          do j=1,jm
            do i=1,im
              tps(i,j)=tps(i,j)
     $                  +(uf(i,j,k)+ub(i,j,k)-2.e0*u(i,j,k))*dz(k)
            end do
          end do
        end do

        do k=1,kbm1
          do j=1,jm
            do i=1,im
              u(i,j,k)=u(i,j,k)
     $                  +.5e0*smoth*(uf(i,j,k)+ub(i,j,k)
     $                               -2.e0*u(i,j,k)-tps(i,j))
            end do
          end do
        end do

        do j=1,jm
          do i=1,im
            tps(i,j)=0.e0
          end do
        end do

        do k=1,kbm1
          do j=1,jm
            do i=1,im
              tps(i,j)=tps(i,j)
     $                  +(vf(i,j,k)+vb(i,j,k)-2.e0*v(i,j,k))*dz(k)
            end do
          end do
        end do

        do k=1,kbm1
          do j=1,jm
            do i=1,im
              v(i,j,k)=v(i,j,k)
     $                  +.5e0*smoth*(vf(i,j,k)+vb(i,j,k)
     $                               -2.e0*v(i,j,k)-tps(i,j))
            end do
          end do
        end do

        do k=1,kb
          do j=1,jm
            do i=1,im
              ub(i,j,k)=u(i,j,k)
              u(i,j,k)=uf(i,j,k)
              vb(i,j,k)=v(i,j,k)
              v(i,j,k)=vf(i,j,k)
            end do
          end do
        end do

      end if

      do j=1,jm
        do i=1,im
          egb(i,j)=egf(i,j)
          etb(i,j)=et(i,j)
          et(i,j)=etf(i,j)
          dt(i,j)=h(i,j)+et(i,j)
          utb(i,j)=utf(i,j)
          vtb(i,j)=vtf(i,j)
          vfluxb(i,j)=vfluxf(i,j)
        end do
      end do

! calculate real w as wr
      if(ionedim.ne.1) call realvertvl !RMY: added ionedim flag

!RMY: may need to add pomtc code to pass sst to atmosphere (icoupling=1)

      return
      end

!_______________________________________________________________________
      subroutine print_section
! print output
      implicit none
      include 'pom.h'
      real atot,darea,dvol,eaver,saver,taver,vtot,tsalt
      integer i,j,k

      if(mod(iint,iprint).eq.0) then !RMY: comment to print all times

! print time
        if(my_task.eq.master_task) write(6,'(/
     $    ''**********************************************************''
     $    /''time ='',f9.4,'', iint ='',i8,'', iext ='',i8,
     $    '', iprint ='',i8)') time,iint,iext,iprint

! check for errors
        call sum0d_mpi(error_status,master_task)
        call bcast0d_mpi(error_status,master_task)
        if(error_status.ne.0) then
          if(my_task.eq.master_task) write(*,'(/a)')
     $                                       'POM terminated with error'
          call finalize_mpi
          stop
        end if

! local averages
        vtot=0.e0
        atot=0.e0
        taver=0.e0
        saver=0.e0
        eaver=0.e0
        do k=1,kbm1
          do j=1,jm
            do i=1,im
              darea=dx(i,j)*dy(i,j)*fsm(i,j)
              dvol=darea*dt(i,j)*dz(k)
              vtot=vtot+dvol
              taver=taver+tb(i,j,k)*dvol
              saver=saver+sb(i,j,k)*dvol
            end do
          end do
        end do

        do j=1,jm
          do i=1,im
            darea=dx(i,j)*dy(i,j)*fsm(i,j)
            atot=atot+darea
            eaver=eaver+et(i,j)*darea
          end do
        end do

        taver=taver/vtot
        saver=saver/vtot
        eaver=eaver/atot
        tsalt=(saver+sbias)*vtot

! print averages
! global averages requiere to transfer high amounts of data between
! processor - therefore, only local average for master_task is printed
        if(my_task.eq.master_task) write(6,'(/''vtot = '',e16.7,
     $    ''   atot = '',e16.7,''  eaver ='',e16.7/''taver ='',e16.7,
     $    ''   saver ='',e16.7,''  tsalt ='',e16.7)')
     $    vtot,atot,eaver,taver,saver,tsalt

!RMY: print u,v,w,t,s,rho at (i,j,k)=(5,5,1:kb) for diagnostics
!        if(my_task.eq.master_task) then
!          write(6,'(/)')
!          do k=1,kb
!            write(6,'(''u5 ='',e16.7,'' v5 ='',e16.7,'' w5 ='',e16.7)')
!     $        u(5,5,k),v(5,5,k),w(5,5,k)
!          end do
!          write(6,'(/)')
!          do k=1,kb
!            write(6,'(''t5 ='',e16.7,'' s5 ='',e16.7,'' ro5 ='',e16.7)')
!     $        t(5,5,k),s(5,5,k),rho(5,5,k)
!          end do
!        end if

      end if

      return
      end

!_______________________________________________________________________
      subroutine check_velocity
! check if velocity condition is violated
      use ieee_arithmetic
      implicit none
      include 'pom.h'
      real vamax,atot,darea,dvol,eaver,saver,taver,vtot,tsalt
      integer i,j,k
      integer imax,jmax
      integer ismax,jsmax
      integer isstflg

      vamax=0.e0
      isstflg=0 !RMY: added logic to kick out when sst > 50 deg C or NaN

      do j=1,jm
        do i=1,im
          if(abs(vaf(i,j)).ge.vamax) then
            vamax=abs(vaf(i,j))
            imax=i
            jmax=j
          end if
          if(t(i,j,1).ge.50 .or. ieee_is_nan(t(i,j,1))) then
            isstflg=1
            ismax=i
            jsmax=j
          end if
        end do
      end do

      if(vamax.gt.vmaxl) then
        if(my_task.eq.master_task.and.error_status.eq.0) write(6,'(/
     $    ''Error: velocity condition violated''/''time ='',f9.4,
     $    '', iint ='',i8,'', iext ='',i8,'', iprint ='',i8,/
     $    ''vamax ='',e12.3,''   imax,jmax ='',2i5)')
     $    time,iint,iext,iprint,vamax,imax,jmax
        error_status=1
      end if

      if(isstflg.eq.1) then
        if(my_task.eq.master_task.and.error_status.eq.0) write(6,'(/
     $    ''Error: SST exceeds 50 C''/''time ='',f9.4,
     $    '', iint ='',i8,'', iext ='',i8,'', iprint ='',i8,/
     $    ''t(ismax,jsmax,1) ='',e12.3,''   ismax,jsmax ='',2i5)')
     $    time,iint,iext,iprint,t(ismax,jsmax,1),ismax,jsmax
        error_status=1
      end if

      return
      end
