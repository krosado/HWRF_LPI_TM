      program transatl06prep
c Purpose: Create merged transatlantic T/S with ~1/6-deg spacing
c          from sharpened united GDEM and unsharpened eastatl GDEM
c Author: Richard M. Yablonsky, URI/GSO, 12/5/12
c
      parameter(IM=254,JM=225,nl=33,IM1=171,JM1=101,IM2=435,JM2=225)
      DIMENSION TB1(IM,JM,nl),SB1(IM,JM,nl)
      DIMENSION T(IM2,JM2,nl),S(IM2,JM2,nl)
      DIMENSION TB2(IM2,JM2,nl),SB2(IM2,JM2,nl)
      dimension X(IM1),Y(JM1),XI(IM),YI(JM),XI2(IM2),YI2(JM2)
      DIMENSION TEM(IM1,JM1),TEMI2(IM2,JM2)
      dimension fin(IM1,JM1,nl),fin1(IM1,JM1,nl)
      real LATMIN,LATMAX,LONGMIN,LONGMAX
      real LATMIN2,LATMAX2,LONGMIN2,LONGMAX2
      parameter(nmos=12)
      dimension ddm12(nmos),f2in(IM1,JM1,nl),f2in1(IM1,JM1,nl)
      dimension wm6(13),wm5(13),wm4(13),wm3(13),wm2(13),wm1(13),w00(13)
      dimension wp1(13),wp2(13),wp3(13),wp4(13),wp5(13),wp6(13)
      DATA ddm12/31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31./
      DATA wm6/1.,1.,1.,1.,1.,1.,1.,0.,0.,0.,0.,0.,0./
      DATA wm5/0.,2.,2.,2.,2.,2.,2.,0.,0.,0.,0.,0.,0./
      DATA wm4/0.,1.,3.,3.,3.,3.,3.,1.,0.,0.,0.,0.,0./
      DATA wm3/0.,0.,2.,4.,4.,4.,4.,2.,0.,0.,0.,0.,0./
      DATA wm2/0.,0.,1.,3.,5.,5.,5.,3.,1.,0.,0.,0.,0./
      DATA wm1/0.,0.,0.,2.,4.,6.,6.,4.,2.,0.,0.,0.,0./
      DATA w00/0.,0.,0.,1.,3.,5.,7.,5.,3.,1.,0.,0.,0./
      DATA wp1/0.,0.,0.,0.,2.,4.,6.,6.,4.,2.,0.,0.,0./
      DATA wp2/0.,0.,0.,0.,1.,3.,5.,5.,5.,3.,1.,0.,0./
      DATA wp3/0.,0.,0.,0.,0.,2.,4.,4.,4.,4.,2.,0.,0./
      DATA wp4/0.,0.,0.,0.,0.,1.,3.,3.,3.,3.,3.,1.,0./
      DATA wp5/0.,0.,0.,0.,0.,0.,2.,2.,2.,2.,2.,2.,0./
      DATA wp6/0.,0.,0.,0.,0.,0.,1.,1.,1.,1.,1.,1.,1./
c
c define sharpened united GDEM region
      LATMIN=10.
      LATMAX=47.5
      LONGMIN=-98.5
      LONGMAX=-50.
c
      dlon=(LONGMAX-LONGMIN)/float(im-1)
      dlat=(LATMAX-LATMIN)/float(jm-1)
      do i=1,im
       XI(i)=LONGMIN+dlon*float(i-1)
      end do
      do j=1,jm
       YI(j)=LATMIN+dlat*float(j-1)
      end do
c
c define raw GDEM region
      xmin=-100.
      xmax=-15.
      ymin=5.
      ymax=55.
c---------------
      RESNX=(xmax-xmin)/float(IM1-1)
      RESNY=(ymax-ymin)/float(JM1-1)
      do i=1,im1
       X(i)=xmin+RESNX*float(i-1)
      end do
      do j=1,jm1
       Y(j)=ymin+RESNY*float(j-1)
      end do
c
c define transatlantic ocean region
      LATMIN2=10.
      LATMAX2=47.5
      LONGMIN2=-98.5
      LONGMAX2=-15.3
c
      dlon2=(LONGMAX2-LONGMIN2)/float(im2-1)
      dlat2=(LATMAX2-LATMIN2)/float(jm2-1)
      do i=1,im2
       XI2(i)=LONGMIN2+dlon2*float(i-1)
      end do
      do j=1,jm2
       YI2(j)=LATMIN2+dlat2*float(j-1)
      end do
c
c read raw GDEM from fort.8 and fort.90 and interpolate between months
      read (8,11) fin,fin1
 11   format(10f6.2)
      read (90,11) f2in,f2in1
      read(91,12) mmm
 12   format(i2)
      read(91,912) ddm
 912  format(f2.0)
      if(ddm.le.15.) then
        fac2=(15.-ddm)/ddm12(mmm-1)
      else
        fac2=(ddm-15.)/ddm12(mmm)
      end if
      fac1=1.-fac2
      do k=1,nl
        do j=1,jm1
          do i=1,im1
            fin(i,j,k)=fac1*fin(i,j,k)+fac2*f2in(i,j,k)
            fin1(i,j,k)=fac1*fin1(i,j,k)+fac2*f2in1(i,j,k)
          end do
        end do
      end do
c
c horiz. interpolate from GDEM grid to transatlantic ocean grid
      do n=1,2
        do k=1,nl
          do j=1,jm1
            do i=1,im1
              if(n.eq.1) then
                TEM(i,j)=fin(i,j,k)
              else
                TEM(i,j)=fin1(i,j,k)
              end if
            end do
          end do
c-------------
          call horint(TEM,TEMI2,X,Y,XI2,YI2,IM1,JM1,IM2,JM2)
c-------------
          do j=1,jm2
            do i=1,im2
              if(n.eq.1) then
                T(i,j,k)=temi2(i,j)
              else
                S(i,j,k)=temi2(i,j)
              end if
            end do
          end do
        end do
      end do
c
c read sharpened united GDEM T/S
      read(13) TB1
      read(13) SB1
c
c replace raw GDEM with sharpened GDEM in united region
      do k=1,nl
        do j=1,jm
          do i=1,im
            T(i,j,k)=TB1(i,j,k)
            S(i,j,k)=SB1(i,j,k)
          end do
        end do
      end do
c
c blend raw GDEM and sharpened GDEM around 50W longitude
      TB2=T
      SB2=S
      do k=1,nl
        do j=1,jm2
          do n=1,13
            wtd=wm6(n)+wm5(n)+wm4(n)+wm3(n)+wm2(n)+wm1(n)+w00(n)+
     1          wp1(n)+wp2(n)+wp3(n)+wp4(n)+wp5(n)+wp6(n)
            nwt=n-7
            TB2(im+nwt,j,k)=(wm6(n)*T(im-6,j,k)+wm5(n)*T(im-5,j,k)+
     1    wm4(n)*T(im-4,j,k)+wm3(n)*T(im-3,j,k)+wm2(n)*T(im-2,j,k)+
     2    wm1(n)*T(im-1,j,k)+w00(n)*T(im+0,j,k)+wp1(n)*T(im+1,j,k)+
     3    wp2(n)*T(im+2,j,k)+wp3(n)*T(im+3,j,k)+wp4(n)*T(im+4,j,k)+
     4    wp5(n)*T(im+5,j,k)+wp6(n)*T(im+6,j,k))/wtd
            SB2(im+nwt,j,k)=(wm6(n)*S(im-6,j,k)+wm5(n)*S(im-5,j,k)+
     1    wm4(n)*S(im-4,j,k)+wm3(n)*S(im-3,j,k)+wm2(n)*S(im-2,j,k)+
     2    wm1(n)*S(im-1,j,k)+w00(n)*S(im+0,j,k)+wp1(n)*S(im+1,j,k)+
     3    wp2(n)*S(im+2,j,k)+wp3(n)*S(im+3,j,k)+wp4(n)*S(im+4,j,k)+
     4    wp5(n)*S(im+5,j,k)+wp6(n)*S(im+6,j,k))/wtd
          end do
        end do
      end do
c
c write TB2/SB2 to fort.113
      write(113) TB2
      write(113) SB2
c
c end program
      stop
      end
c
      subroutine horint(F,FI,X,Y,XI,YI,IG,JG,IM,JM)
      dimension F(IG,JG),FI(im,jm),X(IG),Y(JG),XI(im),YI(jm)
c---------------  Subr. interpolates F(X,Y) to FI(XI,YI)
c---------------   X,Y have even steps RESNX,RESNY
c---------------  There is no mask for files F and FI
c---------------  For Western Hemisphere X=X-360., XI=XI-360.
c---------------
      print *,' begin horint: IG,JG,IM,JM=',
     *                 IG,JG,IM,JM
c
      RESNX=(X(IG)-X(1))/float(IG-1)
      RESNY=(Y(JG)-Y(1))/float(JG-1)
      write(6,201) RESNX,RESNY
 201  format(' RESNX,RESNY=',2f12.6)
      do i=1,im
      do j=1,jm
c--------- find left low corner
       i1=INT((XI(i)-X(1))/RESNX)+1
       j1=INT((YI(j)-Y(1))/RESNY)+1
c-------------------
       if(i1.lt.1.or.i1.ge.IG) then
        print *,' i1 out of region'
        write(6,101) i,j,i1,j1,XI(i),YI(j),X(1),Y(1)
 101    format('i,j,i1,j1,XI(i),YI(j),X(1),Y(1)=',
     *         4i7,4f10.3)
        stop
       end if
       if(j1.lt.1.or.j1.ge.JG) then
        print *,' j1 out of region'
        write(6,101) i,j,i1,j1,XI(i),YI(j),X(1),Y(1)
        stop
       end if
c-------------------
c
       x1=XI(i)-X(i1)
       x2=X(i1+1)-XI(i)
       y1=YI(j)-Y(j1)
       y2=Y(j1+1)-YI(j)
c-------------------
c      if(x1.lt.0..or.x2.lt.0.) then
c       print *,'x1 or x2 out of interval'
c       write(6,103) i,j,i1,j1,XI(i),YI(j),X(i1),Y(j1)
 103    format('i,j,i1,j1,XI(i),YI(j),X(i1),Y(j1)=',
     *         4i7,4f10.3)
c       write(6,102) x1,x2,y1,y2
 102    format(' x1,x2,y1,y2=',4f10.4)
c       stop
c      end if
c      if(y1.lt.0..or.y2.lt.0.) then
c       print *,'y1 or y2 out of interval'
c       write(6,103) i,j,i1,j1,XI(i),YI(j),X(i1),Y(j1)
c       write(6,102) x1,x2,y1,y2
c       stop
c      end if
c-------------------
       a1=F(i1,j1)
       a2=F(i1+1,j1)
       a3=F(i1+1,j1+1)
       a4=F(i1,j1+1)
c
       FI(i,j)=(a1*x2*y2+a2*x1*y2+a3*x1*y1+a4*x2*y1)/
     *           ((x1+x2)*(y1+y2))
c---------------
       if(i.eq.108.and.j.eq.90) then
        print *,' check bilin i,j,i1,j1=',i,j,i1,j1
        write(6,301) a1,a2,a3,a4,FI(i,j)
 301    format('a1,a2,a3,a4,FI(i,j)=',5f9.3)
        write(6,302) x1,x2,y1,y2
 302    format('x1,x2,y1,y2=',4f9.3)
        write(6,303) X(i1),X(i1+1),XI(i)
 303    format('X(i1),X(i1+1),XI(i)=',3f9.3)
        write(6,304) Y(j1),Y(j1+1),YI(j)
 304    format('Y(j1),Y(j1+1),YI(j=',3f9.3)
       end if
c---------------
      end do
      end do
      print *,' end horint'
      return
      end
