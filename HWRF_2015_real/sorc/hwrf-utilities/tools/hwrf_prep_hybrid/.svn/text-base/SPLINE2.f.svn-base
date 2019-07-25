        SUBROUTINE SPLINE2(KK,nold,xold,yold,y2,nnew,xnew,ynew,p,
     &                                                    qspline)
!
!     ******************************************************************
!     *                                                                *
!     *  this is a one-dimensional cubic spline fitting routine        *
!     *  programed for a small scalar machine.                         *
!     *                                                                *
!     *  programer: z. janjic                                          *
!     *                                                                *
!     *  nold - number of given values of the function.  Must be ge 3. *
!     *  xold - locations of the points at which the values of the     *
!     *         function are given.  Must be in ascending order.       *
!     *  yold - the given values of the function at the points xold.   *
!     *  y2   - the second derivatives at the points xold.  If natural *
!     *         spline is fitted y2(1)=0. And y2(nold)=0. Must be      *
!     *         specified.                                             *
!     *  nnew - number of values of the function to be calculated.     *
!     *  xnew - locations of the points at which the values of the     *
!     *         function are calculated.  Xnew(k) must be ge xold(1)   *
!     *         and le xold(nold).                                     *
!     *  ynew - the values of the function to be calculated.           *
!     *  p, q - auxiliary vectors of the length nold-2.                *
!     *                                                                *
!     ******************************************************************
!
        INTEGER:: nold,noldm1,nnew,i,j,ii,jj,k,k2,kold,k1,l,KK
        REAL::  xold(nold),yold(nold),y2(nold),p(nold),qspline(nold)
        REAL::  xnew(nnew),ynew(nnew)
        REAL:: dxl,dxr,dydxl,dydxr,rtdxc,y2k,y2kp1,dxc
        REAL:: dx,rdx,ak,bk,ck,x,xsq,xk, den

	KKK=37970
!
!mp      if(nnew.eq.1)then
       if(KK .eq. KKK)then
        print*,'DEBUG in SPLINE2:HSO= ',xnew
        do l=1,nold
         print*,'DEBUG in SPLINE2:L,ZETAI,PINTI= ' 
     &                      ,L,yold(L),xold(L)
        end do
       end if
!mp      end if
      noldm1=nold-1
!
      dxl=xold(2)-xold(1)
      dxr=xold(3)-xold(2)
      dydxl=(yold(2)-yold(1))/dxl
      dydxr=(yold(3)-yold(2))/dxr
      rtdxc=.5/(dxl+dxr)
!
      p(1)= rtdxc*(6.*(dydxr-dydxl)-dxl*y2(1))
      qspline(1)=-rtdxc*dxr
!
      if(nold.eq.3) go to 700
!-----------------------------------------------------------------------
      k=3
!
 100  dxl=dxr
      dydxl=dydxr
      dxr=xold(k+1)-xold(k)
      dydxr=(yold(k+1)-yold(k))/dxr
      dxc=dxl+dxr
      den=1./(dxl*qspline(k-2)+dxc+dxc)
!
      p(k-1)= den*(6.*(dydxr-dydxl)-dxl*p(k-2))
      qspline(k-1)=-den*dxr
!
      k=k+1
      if(k.lt.nold) go to 100
!-----------------------------------------------------------
 700  k=noldm1
!
 200  y2(k)=p(k-1)+qspline(k-1)*y2(k+1)
!
      k=k-1
      if(k.gt.1) go to 200
!-----------------------------------------------------------------------
      k1=1
!
 300  xk=xnew(k1)
!
      do 400 k2=2,nold
      if(xold(k2).le.xk) go to 400
      kold=k2-1
      go to 450
 400  continue
      ynew(k1)=yold(nold)
      go to 600
!
 450  if(k1.eq.1)   go to 500
      if(k.eq.kold) go to 550
!
 500  k=kold
!
      y2k=y2(k)
      y2kp1=y2(k+1)
      dx=xold(k+1)-xold(k)
      rdx=1./dx
!
      ak=.1666667*rdx*(y2kp1-y2k)
      bk=.5*y2k
      ck=rdx*(yold(k+1)-yold(k))-.1666667*dx*(y2kp1+y2k+y2k)
!
 550  x=xk-xold(k)
      xsq=x*x
!
      ynew(k1)=ak*xsq*x+bk*xsq+ck*x+yold(k)

       if(KK.eq.KKK)then
        write(0,*) 'DEBUG:: k1,ynew(k1)Z,xnew(k1)P: ', 
     &    k1,ynew(k1),xnew(k1)
        endif
!
 600   k1=k1+1
        if(k1.le.nnew) go to 300
        END SUBROUTINE SPLINE2
