!-----------------------------------------------------------------------
! Transform of the coordinate
! The TC center is relocated to the North Pole.
!
!     clon & clat: TC center
!                                               JUL 05 2012 In-Hyuk Kwon
!-----------------------------------------------------------------------
      subroutine trans2Polar(tp,xlon,ylat,ib,jb,clon,clat,cyl,mt,nt,dnt)
!-----------------------------------------------------------------------
      implicit none
      real(kind=8),dimension(ib,jb) :: tp,xlon,ylat
      real(kind=8),dimension(0:nt-1,0:mt-1) :: x0,y0,z0,cyl
      real(kind=8),dimension(0:nt-1,0:mt-1) :: plon,plat
      real(kind=8),dimension(0:mt-1)    :: axtp,cthet
      real(kind=8),dimension(0:nt-1)    ::      clamb
      real(kind=8) :: clat,clon,dnt
      real(kind=8) :: radius,pi,pi2,pi180,alph,beta,dthet,dlamb,r
      real(kind=8) :: x1,y1,z1,tmp1,tmp2,lambp,s,t,x,y,sum
      real(kind=8) :: bi0,bi1,bj0,bj1
      integer :: ib,jb,mt,nt,ilo,ihi,jlo,jhi
      integer :: i,j,m,n,ism,ntm
! -----------------------------------------------
       radius= 6.37100D+06
       pi= 3.141592653589793238D0
       pi2= pi *2.D0
       pi180= pi/180.D0

! --------------------------
!      print*,tp(1,1),xlon(1,1),ylat(1,1)
!      print*,ib,jb,clon,clat,mt,nt,dnt

      do m= 0,mt-1
        do n= 0,nt-1
           clamb(n)=             n *pi2 / nt
           cthet(m)= 0.5D0 *pi - m *dnt / radius
           x0(n,m)= radius *dcos(clamb(n)) *dcos(cthet(m))
           y0(n,m)= radius *dsin(clamb(n)) *dcos(cthet(m))
           z0(n,m)= radius *dsin(cthet(m))
        enddo
      enddo

      do m= 0,mt-1
        do n= 0,nt-1
           r= dsqrt( y0(n,m)**2. +z0(n,m)**2.)
          if(y0(n,m) == 0.D0 .and. z0(n,m) == 0.D0 )then
             beta= pi *0.5D0
          else
             beta= datan2(z0(n,m) ,y0(n,m))
          endif
           alph= (90.D0 -clat)  *pi180
           x1= x0(n,m)
           y1= r *dcos(alph +beta)
           z1= r *dsin(alph +beta)

           tmp1= z1 /radius
          if(tmp1 > 1.D0) tmp1=  1.D0
           dthet= dasin(tmp1)
           tmp2= y1 /(radius *dcos(dthet))
          if(tmp2 > 1.D0) tmp2=  1.D0
          if(tmp2 <-1.D0) tmp2= -1.D0
           dlamb= dasin(tmp2) + 90.D0 *pi180 + clon *pi180
          if(x1   <-0.D0) dlamb= -dlamb + pi2 + clon *pi180 *2.D0
          if(dlamb > pi2) dlamb=  dlamb - pi2

           plat(n,m)= dthet /pi180
           plon(n,m)= dlamb /pi180

         enddo
       enddo

!     write(*,'(a,2f5.1)') 'Central longitude and latitude:',clon,clat
!     write(*,'(a)')
!    & 'r= 0deg lambda=0deg  lambda=90deg lambda=180deg lambda=270deg'
!     write(*,'(a,4f14.4)') ' lon:',
!    & plon(0,0),plon(25,0),plon(50,0),plon(75,0)
!     write(*,'(a,4f14.4)') ' lat:',
!    & plat(0,0),plat(25,0),plat(50,0),plat(75,0)
!     write(*,'(a)')
!    & 'r= 1deg lambda=0deg  lambda=90deg lambda=180deg lambda=270deg'
!     write(*,'(a,4f14.4)') ' lon:',
!    & plon(0,1),plon(25,1),plon(50,1),plon(75,1)
!     write(*,'(a,4f14.4)') ' lat:',
!    & plat(0,1),plat(25,1),plat(50,1),plat(75,1)
!     write(*,'(a)')
!    & 'r= 2deg lambda=0deg  lambda=90deg lambda=180deg lambda=270deg'
!     write(*,'(a,4f14.4)') ' lon:',
!    & plon(0,2),plon(25,2),plon(50,2),plon(75,2)
!     write(*,'(a,4f14.4)') ' lat:',
!    & plat(0,2),plat(25,2),plat(50,2),plat(75,2)
!            stop

! --------------------------  Linear Interpolation
!        print*,'Linear Interpolation'

         bi0= xlon(1 ,1)
         bi1= xlon(ib,1)
         bj0= ylat(1 ,1)
         bj1= ylat(1,jb)
!        print*,bi0,bi1,bj0,bj1
      do m= 0,mt-1
      do n= 0,nt-1
           x= plon(n,m)
           y= plat(n,m)
           if(x > 180.) x= x-360.
        if(x < bi0 .or. x > bi1 .or. y < bj0 .or. y > bj1)then
           cyl(n,m)=  9.999E+20
           goto 10
        endif
           jlo=( y-ylat(1,1) ) *(jb-1) /( ylat(1,jb)-ylat(1,1) )
           ilo=( x-xlon(1,1) ) *(ib-1) /( xlon(ib,1)-xlon(1,1) )
      call hunt(xlon(:,1),ib,x,ilo)
      call hunt(ylat(1,:),jb,y,jlo)
           ihi= ilo +1
           jhi= jlo +1
        if(tp(ilo,jhi) > 1.D15 .or. tp(ihi,jhi) > 1.D15 .or.
     &     tp(ilo,jlo) > 1.D15 .or. tp(ihi,jhi) > 1.D15      )then
           cyl(n,m)=  9.999E+20
        else
           s= (x -xlon(ilo,jlo)) /(xlon(ihi,jlo) -xlon(ilo,jlo))
           t= (y -ylat(ilo,jlo)) /(ylat(ilo,jhi) -ylat(ilo,jlo))
           cyl(n,m)=
     &      (1-s)*   t *tp(ilo,jhi) +  s *   t *tp(ihi,jhi)
     &     +(1-s)*(1-t)*tp(ilo,jlo) +  s *(1-t)*tp(ihi,jlo)
        endif
   10   continue
      enddo
      enddo

! -----------------------------------------------------------------
      end subroutine trans2Polar
!------------------------------------------------------------------
