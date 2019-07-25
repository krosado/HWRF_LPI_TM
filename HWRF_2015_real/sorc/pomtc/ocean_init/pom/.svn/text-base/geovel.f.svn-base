      subroutine geovel
c---------------------------------------------------------------
c ---  INSTEAD OF USING THE INITIAL VELOCITY FIELDS DIRECTLY ---
c      This subroutine calculates initial geostrophic velocity 
c      (but not transports) from the initial density gradient.
c---------------------------------------------------------------
      implicit none
      include 'pom.h'
      integer i,j,k
      real cmp
c
      do k=1,kbm1 !RMY: changed all el to elb here to use non-zero elb
         do j=2,jm
            do i=2,im
               drhox(i,j,k)=drhox(i,j,k)+.25E0*grav*(dy(i,j)+dy(i-1,j))
     1         *(d(i,j)+d(i-1,j))*(elb(i,j)-elb(i-1,j))*dum(i,j)
               drhoy(i,j,k)=drhoy(i,j,k)+.25E0*grav*(dx(i,j)+dx(i,j-1))
     1         *(d(i,j)+d(i,j-1))*(elb(i,j)-elb(i,j-1))*dvm(i,j)
            end do
         end do
         do j=2,jm
            if(n_west.eq.-1) then !RMY: ensure b.c. is truly an edge
               drhox(1,j,k)=drhox(2,j,k)
               drhoy(1,j,k)=drhoy(2,j,k)
            end if
         end do
         do i=2,im 
            if(n_south.eq.-1) then !RMY: ensure b.c. is truly an edge
               drhox(i,1,k)=drhox(i,2,k)
               drhoy(i,1,k)=drhoy(i,2,k)
            end if
         end do
         if(n_west.eq.-1.and.n_south.eq.-1) then !RMY: corner b.c.
            drhox(1,1,k)=0.5*(drhox(1,2,k)+drhox(2,1,k))
            drhoy(1,1,k)=0.5*(drhoy(1,2,k)+drhoy(2,1,k))
         end if
      end do

      call exchange3d_mpi(drhox(:,:,1:kbm1),im,jm,kbm1) !RMY: add exch
      call exchange3d_mpi(drhoy(:,:,1:kbm1),im,jm,kbm1) !RMY: add exch

cRMY      small=1.e-15 !RMY: small is already defined as small=1.e-9
      do k=1,kbm1
         do j=2,jm-1
            do i=2,im-1
               cmp=dum(i+1,j)+dum(i,j)+dum(i,j-1)+dum(i+1,j-1)+small
               vb(i,j,k)= 1/aru(i,j)*( drhox(i+1,j,k)+drhox(i,j,k)+
     1         drhox(i,j-1,k)+drhox(i+1,j-1,k) )/cmp/d(i,j)/cor(i,j)
               cmp=dvm(i-1,j)+dvm(i,j)+dvm(i,j+1)+dvm(i-1,j+1)+small
               ub(i,j,k)=-1/arv(i,j)*( drhoy(i-1,j,k)+drhoy(i,j,k)+
     1         drhoy(i,j+1,k)+drhoy(i-1,j+1,k) )/cmp/d(i,j)/cor(i,j)
            end do
         end do
         do j=2,jm-1
            if(n_east.eq.-1) then !RMY: ensure b.c. is truly and edge
               cmp=dvm(im-1,j)+dvm(im,j)+dvm(im,j+1)+dvm(im-1,j+1)+small
               ub(im,j,k)=-1/arv(im,j)*( drhoy(im-1,j,k)+drhoy(im,j,k)+
     1         drhoy(im,j+1,k)+drhoy(im-1,j+1,k) )/cmp/d(im,j)/cor(im,j)
               cmp=dum(im,j)+dum(im,j-1)+small
               vb(im,j,k)= 1/aru(im,j)*( drhox(im,j,k)+
     1         drhox(im,j-1,k) )/cmp/d(im,j)/cor(im,j)
            end if
         end do
         do i=2,im-1
            if(n_north.eq.-1) then !RMY: ensure b.c. is truly and edge
               cmp=dum(i+1,jm)+dum(i,jm)+dum(i,jm-1)+dum(i+1,jm-1)+small
               vb(i,jm,k)= 1/aru(i,jm)*( drhox(i+1,jm,k)+drhox(i,jm,k)+
     1         drhox(i,jm-1,k)+drhox(i+1,jm-1,k) )/cmp/d(i,jm)/cor(i,jm)
               cmp=dvm(i-1,jm)+dvm(i,jm)+small
               ub(i,jm,k)=-1/arv(i,jm)*( drhoy(i-1,jm,k)+
     1         drhoy(i,jm,k) )/cmp/d(i,jm)/cor(i,jm)
            end if
         end do
         if(n_west.eq.-1.and.n_south.eq.-1) then !RMY: corner b.c.
            ub(1,1,k)=0.5*(ub(1,2,k)+ub(2,1,k))
            vb(1,1,k)=0.5*(vb(1,2,k)+vb(2,1,k))
         end if
         if(n_west.eq.-1.and.n_north.eq.-1) then !RMY: corner b.c.
            ub(1,jm,k)=0.5*(ub(1,jm-1,k)+ub(2,jm,k))
            vb(1,jm,k)=0.5*(vb(1,jm-1,k)+vb(2,jm,k))
         end if
         if(n_east.eq.-1.and.n_south.eq.-1) then !RMY: corner b.c.
            ub(im,1,k)=0.5*(ub(im,2,k)+ub(im-1,1,k))
            vb(im,1,k)=0.5*(vb(im,2,k)+vb(im-1,1,k))
         end if
         if(n_east.eq.-1.and.n_north.eq.-1) then !RMY: corner b.c.
           ub(im,jm,k)=0.5*(ub(im,jm-1,k)+ub(im-1,jm,k))
           vb(im,jm,k)=0.5*(vb(im,jm-1,k)+vb(im-1,jm,k))
         end if
         do j=1,jm
            do i=1,im
               ub(i,j,k)=ub(i,j,k)*dum(i,j)
               vb(i,j,k)=vb(i,j,k)*dvm(i,j)
            end do
         end do
      end do

      call exchange3d_mpi(ub(:,:,1:kbm1),im,jm,kbm1) !RMY: add exch
      call exchange3d_mpi(vb(:,:,1:kbm1),im,jm,kbm1) !RMY: add exch

c----------- falk 09/28/00 correct u,v that u=v=0 at lev kb-1
c --- LUO: the following is necessary for the pacific region.
c --- don't know why it is not for the atlantic --- 01.18.2008 
       do k=1,kbm1
          do j=1,jm
             do i=1,im
                ub(i,j,k)=ub(i,j,k)-ub(i,j,kbm1)
                vb(i,j,k)=vb(i,j,k)-vb(i,j,kbm1)
             end do
          end do
       end do
c---------- 01-23-02 do (not?) use calculation of u,v below 2000m
cc
cc      do i=1,im
cc      do j=1,jm
cc       if(d(i,j).gt.2000.) then
cc        do k=1,kb
cc         if(-d(i,j)*zz(k).gt.2000.) then
cc          k2000=k-1
cc          go to 987
cc         end if
cc        end do
cc 987    continue
cc        do k=k2000+1,kb
cc         ub(i,j,k)=ub(i,j,k2000)
cc         vb(i,j,k)=vb(i,j,k2000)
cc        end do
cc       end if
cc      end do
cc      end do
c
c-------------------------------------------------------------------
c
      return
      end
