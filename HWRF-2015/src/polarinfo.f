! Polar coordinate information of corresponding grid point
! Origin of polar coordinate is identical to the strom center (clon,clat)
!
!     plon: rotated longitude
!     plat: rotated latitude
!                                               JUL 05 2012 In-Hyuk Kwon
!-----------------------------------------------------------------------
      subroutine polarinfo(xlon,ylat,ib,jb,clon,clat,plon,plat)
!-----------------------------------------------------------------------
      implicit none
      real(kind=8),dimension(ib,jb) :: xlon,ylat,plon,plat
      real(kind=8),dimension(ib,jb) :: x0,y0,z0
      real(kind=8) :: clat,clon
      real(kind=8) :: radius,pi,pi2,pi180,alph,beta,dthet,dlamb,r
      real(kind=8) :: x1,y1,z1,tmp1,tmp2,cthet,clamb,thre
      integer :: i,j,ib,jb
! -----------------------------------------------
       radius= 6.37100D+06
       pi= 3.141592653589793238D0
       pi2= pi *2.D0
       pi180= pi/180.D0
       thre =1.D0 - 1.D-8

! ------------------------------------------------------- Sphere rotation
! A specific point (clon,clat) is transformed into the north pole (0, 90N)

!                          lat and lon --> Cartesian coordinate
      do j= 1,jb
        do i=1,ib
           cthet  = ylat(i,j) *pi180
           clamb  =(xlon(i,j) -clon -90.D0) *pi180
           x0(i,j)= radius *dcos(clamb) *dcos(cthet)
           y0(i,j)= radius *dsin(clamb) *dcos(cthet)
           z0(i,j)= radius *dsin(cthet)
        enddo
      enddo

      do j= 1,jb
        do i= 1,ib
           r= dsqrt( y0(i,j)**2. +z0(i,j)**2.)
          if(r == 0.D0 )then
             beta= pi *0.5D0
          else
             beta= datan2(z0(i,j), -y0(i,j))
          endif
             alph= (90.D0 -clat)  *pi180

!                                                      Rotation
           x1= x0(i,j)
           y1= r *dcos(alph +beta) *(-1.)
           z1= r *dsin(alph +beta)

!                          Cartesian coordinate --> lat and lon
           tmp1= z1 /radius
          if(tmp1 >  1.D0)  tmp1=  1.D0
           dthet= Dasin(tmp1)
           tmp2= x1 /(radius *Dsin(dthet+ pi*0.5))
          if(tmp2 >  1.D0)  tmp2=  1.D0
          if(tmp2 < -1.D0)  tmp2= -1.D0
             dlamb= Dacos(tmp2)
          if(y1 < 0.)then
             dlamb= -dlamb
          endif

           plat(i,j)= dthet /pi180
           plon(i,j)= dlamb /pi180

        enddo
      enddo

!-----------------------------------------------------------------------
      end subroutine polarinfo
!-----------------------------------------------------------------------
