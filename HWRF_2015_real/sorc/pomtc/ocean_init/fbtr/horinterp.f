      subroutine horinterp(mskval,im,jm,im1,jm1,x,y,xi,yi,f,fi)
      real x1,x2,y1,y2,area,mskval
      real f(im1,jm1),fi(im,jm)
      dimension x(im1),y(jm1),xi(im),yi(jm),zt(4),zts(4)
      integer ii,jj
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  first interpolating horizontally along each layer
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if(xi(im).gt.x(im1).or.xi(1).lt.x(1)) then
      print*,'x = ',x,'; xi = ',xi
      print*,'there is no data for this vicinity'
      stop
      else
      if(yi(jm).gt.y(jm1).or.yi(1).lt.y(1)) then
      print*,'y = ',y,'; yi = ',yi  
      print*,'there is no data for this vicinity'
      stop
      end if
      end if

      do j1=1,jm
         do i1=1,im
c
c******** looking for closest points in x ****************
c
      ii=1
      do while (ii.le.im1.and.(x(ii)-xi(i1)).lt.0)
      ii=ii+1
      end do
      if(ii.eq.1) ii=ii+1 
c
c******** looking for closest points in y ****************
c
      jj=1
      do while (jj.le.jm1.and.(y(jj)-yi(j1)).lt.0)
      jj=jj+1
      end do
      if(jj.eq.1) jj=jj+1 
c
c*****  calculating parameters for formula  **************
c
      x1=x(ii-1)
      x2=x(ii)
      y1=y(jj-1)
      y2=y(jj)
      zt(1)=f(ii-1,jj-1)
      zt(2)=f(ii,jj-1)
      zt(3)=f(ii,jj)
      zt(4)=f(ii-1,jj)
c
c******* calculating formula *****************************
c
      do i=1,4
         if(zt(i).eq.mskval) then
      zts(i)=0.
         else
      zts(i)=1.
         end if
      end do
c
      fi(i1,j1)=zts(1)*zt(1)*(x2-xi(i1))*(y2-yi(j1))
     1         +zts(2)*zt(2)*(xi(i1)-x1)*(y2-yi(j1))
     2         +zts(3)*zt(3)*(xi(i1)-x1)*(yi(j1)-y1)
     3         +zts(4)*zt(4)*(x2-xi(i1))*(yi(j1)-y1)
      area=zts(1)*abs((x2-xi(i1))*(y2-yi(j1)))
     1         +zts(2)*abs((xi(i1)-x1)*(y2-yi(j1)))
     2         +zts(3)*abs((xi(i1)-x1)*(yi(j1)-y1))
     3         +zts(4)*abs((x2-xi(i1))*(yi(j1)-y1))
c
c------------ falk 04-19-02 check more carefully
c     if(area.eq.0.) then
      if(area.lt.(0.1*abs(x2-x1)*abs(y2-y1))) then
      fi(i1,j1)=mskval
      else
      fi(i1,j1)=fi(i1,j1)/area
      end if
c
      end do
         end do
c
      return
      end
