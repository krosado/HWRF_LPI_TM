      subroutine serftempr(im1,jm1,stm,fsm,alon,alat,im,jm)
c changes are made on 11/01/00 to read avn global sst
c biju thomas
c changes are made on 04/17/14 to set sst(x=180) = sst(x=-180)
c richard yablonsky

      dimension xi(im),yi(jm)
      dimension xavn(im1),yavn(jm1)
      dimension tem(im1,jm1),tb1(im,jm),zt(4),zts(4)
      dimension stm(im,jm),fsm(im,jm),alon(im,jm),alat(im,jm)
      dimension xavn2(im1+1),tem2(im1+1,jm1)
      real msk(im1,jm1)
      real cmp
c
c-------------- falk 03-26-02 
      print *,'serftempr: dim avn sst data im1,jm1=',im1,jm1
      cmp=0.
      read(21) tem
      read(22) msk
      rewind(21)
      rewind(22)
      read(23,124)im3,jm3
      read(23,123)xavn,yavn
 123  format(1x,10e10.4)
 124  format(1x,2i5)
      rewind(23)
c
c*********************************************************
c  extrapolating original data to fill missing data points
c*********************************************************
      do n1=1,10
      do 9 j=2,jm1-1
      do 9 i=2,im1-1
         if(msk(i,j).eq.1) then
      tem(i,j)=0
      cmp=0
      zt(1)=tem(i+1,j)
      zts(1)=1-msk(i+1,j)
      zt(2)=tem(i-1,j)
      zts(2)=1-msk(i-1,j)
      zt(3)=tem(i,j+1)
      zts(3)=1-msk(i,j+1)
      zt(4)=tem(i,j-1)
      zts(4)=1-msk(i,j-1)
        do n=1,4
        tem(i,j)=tem(i,j)+zts(n)*zt(n)
        cmp=cmp+zts(n)
        end do
      if(cmp.eq.0) then
        tem(i,j)=0.0
      else
        tem(i,j)=tem(i,j)/cmp
        msk(i,j)=0.0
      end if
         end if
   9  continue
      end do
c
c************************************************************
cRMY: extrapolating original data so sst(x=180) = sst(x=-180)
c************************************************************
      do i=1,im1
         xavn2(i)=xavn(i)
      end do
      xavn2(im1+1)=180.     

      do j=1,jm1
         do i=1,im1
         tem2(i,j)=tem(i,j)
         end do
         tem2(im1+1,j)=tem(1,j)
      end do
c
c----------- interpolating horizontally along each layer ---------
c
      do j=1,jm
         do i=1,im
         xi(i)=alon(i,1)
         yi(j)=alat(1,j)
         end do
      end do
c
      call horinterp(0.0,im,jm,im1+1,jm1,xavn2,yavn,xi,yi,tem2,tb1)
c
      do j=1,jm
         do i=1,im
         stm(i,j)=tb1(i,j)-273.19
         stm(i,j)=stm(i,j)*fsm(i,j)
         end do
      end do
c
      return
      end
