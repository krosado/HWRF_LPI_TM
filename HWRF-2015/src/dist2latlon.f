!-----------------------------------------------------------------------
!                                               22 JUL 2010 In-Hyuk Kwon
!-----------------------------------------------------------------------
      subroutine dist2latlon(dist,ang,clon,clat,wlon,wlat)
!-----------------------------------------------------------------------
      implicit none
      real :: dist,ang,clat,clon,wlon,wlat
      real(kind=8) :: x0,y0,z0,x1,y1,z1
      real(kind=8) :: radius,pi,pi2,pi180,alph,beta,dthet,dlamb,r
      real(kind=8) :: clamb,cthet,tmp1,tmp2
! -----------------------------------------------
       radius= 6.37100D+06
       pi= 3.141592653589793238
       pi2= pi *2.
       pi180= pi/180.

! --------------------------
       clamb= ang *pi180
       cthet= 0.5 *pi -dist /radius

       x0= radius *cos(clamb) *cos(cthet)
       y0= radius *sin(clamb) *cos(cthet)
       z0= radius *sin(cthet)

       r = sqrt( y0**2. +z0**2.)

      if(r == 0. )then
         beta= pi *0.5
      else
         beta= atan2(z0 ,y0)
      endif

       alph= (90. -clat)  *pi180
       x1= x0
       y1= r *cos(alph +beta)
       z1= r *sin(alph +beta)

       tmp1= z1 /radius
      if(tmp1 > 1.D0) tmp1=  1.D0
       dthet= asin(tmp1)
       tmp2= y1 /(radius *cos(dthet))
      if(tmp2 > 1.D0) tmp2=  1.D0
      if(tmp2 <-1.D0) tmp2= -1.D0
       dlamb= asin(tmp2) + 90. *pi180 + clon *pi180
      if(x1   <-0.) dlamb= -dlamb + pi2 + clon *pi180 *2.
      if(dlamb > pi ) dlamb= dlamb - pi2

       wlat= dthet /pi180
       wlon= dlamb /pi180

! -----------------------------------------------------------------
      end subroutine dist2latlon
!------------------------------------------------------------------
