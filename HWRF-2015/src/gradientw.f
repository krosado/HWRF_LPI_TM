!-----------------------------------------------------------------------
      subroutine gradientw(axhgt,grwnd,mt,dmt,clat)
!-----------------------------------------------------------------------
      implicit none
      real(kind=8),dimension(0:mt-1) :: axhgt,grwnd,phiS,difp
      real(kind=8) :: dmt,clat
      real(kind=8) :: Omega,gravi,PI,PI180,fc,y1,y2,r,r2f2,sqtot
      integer ::  m,mt,mt1

       Omega= 7.292D-05
       gravi=9.8D0
       PI   = 3.141592653589793238D0
       PI180= PI /180.D0
       fc   = 2.D0 *Omega *DSIN(clat*PI180)

       phiS(:)= axhgt(:) *gravi
       grwnd(:)= 1.D+20
       mt1= mt-1

      do m= 1, mt-2
         y1   = phiS(m-1)
         y2   = phiS(m+1)
         difp(m) = (y2 - y1) / (dmt *2.D0)
         r    = m * dmt
         r2f2 =(fc**2. * r**2.D0) / 4.D0
         sqtot= r * difp(m) + r2f2
        if(y1 > 1.e10 .or. y2 > 1.e10)then
           grwnd(m)= 1.D+20
        elseif(sqtot <  0.) then
           grwnd(m)= 1.D+20
        else
           grwnd(m)= Dsqrt(sqtot) -(fc *r) /2.D0
        endif
      enddo
           grwnd(0)= 0.D0

!-----------------------------------------------------------------------
      end subroutine gradientw
!-----------------------------------------------------------------------
