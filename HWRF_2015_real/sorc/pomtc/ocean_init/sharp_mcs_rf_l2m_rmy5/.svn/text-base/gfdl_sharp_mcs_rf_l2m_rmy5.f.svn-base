      program sharp_mcs_rf_l2m
c---------------  reads gdem 0.5 deg resolution t,s 
c---------------        defined over land and ocean;
c---------------  reads gdem3 0.25 deg reolution t,s
c---------------        defined with -9. under bottom;
c---------------  interpolates gdem data to United region;
c---------------  makes mixture of gdem3 and gdem and 
c---------------        smoothes them (made from mixLevGDEM 
c---------------        in subnavygs.f)
c---------------  reads H_FSM_FSMA, BAYu,FSgsu,SGYREu after
c---------------        mask_topog_path.f in SCRIPTS/workc
c---------------  reads monthly climate GS from 75W to 50W
c---------------        and creates the entire GS path
c---------------  sharpens t,s along all trajectories
c---------------  uses ynp=26.6 for BAY or from fort.30
c
      parameter(IM=254,JM=225,nl=33,mmx=500,IM1=171,JM1=101)
      DIMENSION H(IM,JM),TB1(IM,JM,nl),SB1(IM,JM,nl),
     *          FSM(IM,JM),Zlev(nl)
      dimension t(im,jm,nl),s(im,jm,nl)
      dimension t12N(nl),t12S(nl)
      dimension X(IM1),Y(JM1),XI(IM),YI(JM)
      DIMENSION TEM(IM1,JM1),TEMI(IM,JM)
      dimension fin(IM1,JM1,nl),fin1(IM1,JM1,nl)
      real LATMIN,LATMAX,LONGMIN,LONGMAX
      COMMON/sphere/REARTH,LATMIN,LATMAX,LONGMIN,LONGMAX
      CHARACTER*60 FTLEVIT,FTLEV_GDEM
c---------------- falk 08-15-05 add file aladj
      dimension aladj(nl)
cRMY 11/01/06 define variables for GDEM interpolation between months
      parameter(nmos=12)
      dimension ddm12(nmos),f2in(IM1,JM1,nl),f2in1(IM1,JM1,nl)
      DATA ddm12/31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31./
cRMY 11/01/06 end 1 of 2--------------------------------------------
c
c------------- read Levitus data
c-------------   replace /emcsrc to /emc1
c--------- 03-02-04 read Levitus monthly data from fort.24
c     FTLEVIT='/emc1/wx20af/gfdl/fix/levitint.dat'
c     OPEN(12,FILE=FTLEVIT,STATUS='unknown',form='formatted')
c------------ write mixed LEVITUS-GDEM data
c     FTLEV_GDEM='/nfsuser/g01/wx20af/gfdl/gradu/data/LEV_GDEM'
c     OPEN(82,FILE=FTLEV_GDEM,STATUS='unknown',form='formatted')
c
c----------- falk 05-05-05  use Zlev as real data
      DATA Zlev/
     1      0.,  10.,  20.,  30.,  50.,  75., 100., 125., 150., 200.
     2  , 250., 300., 400., 500., 600., 700., 800., 900.,1000.,1100.
     3  ,1200.,1300.,1400.,1500.,1750.,2000.,2500.,3000.,3500.,4000.
     4  ,4500.,5000.,5500./
c---------------- falk 08-15-05 add file aladj
      DATA aladj/
     *    0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.07, 0.15, 0.30, 0.45
     *   ,0.60, 0.75, 1.00, 0.90, 0.80, 0.65, 0.50, 0.35, 0.25, 0.20
     *   ,0.10, 0.05, 0.02, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00
     *   ,0.00, 0.00, 0.00/
c     DATA aladj/
c    *    0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.07, 0.15, 0.30, 0.45
c    *   ,0.60, 0.75, 1.00, 1.00, 1.00, 0.85, 0.70, 0.55, 0.35, 0.20
c    *   ,0.10, 0.05, 0.02, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00
c    *   ,0.00, 0.00, 0.00/
c--------- for united region 
      LATMIN=10.
      LATMAX=47.5
      LONGMIN=-98.5
      LONGMAX=-50.
c
      write(6,301) LATMIN,LATMAX,LONGMIN,LONGMAX
 301  format('   LATMIN,LATMAX,LONGMIN,LONGMAX',4f7.2)
c---------------
      print *,' Zlev(nl)   nl=',nl
      write(6,102) Zlev
 102  format(10f7.0)
c--------------- read H, FSM after mask_topog_path from workc
      rewind(66)
      read(66,102) H
      read(66,102) FSM
      close(66)
c---------------
      dlon=(LONGMAX-LONGMIN)/float(im-1)
      dlat=(LATMAX-LATMIN)/float(jm-1)
      do i=1,im
       XI(i)=LONGMIN+dlon*float(i-1)
      end do
      do j=1,jm
       YI(j)=LATMIN+dlat*float(j-1)
      end do
c---------------
      print *,' ocean model grid im,jm=',im,jm
      write(6,503) XI(1),XI(im)
 503  format(' XI(1),XI(im)=',2f9.3)
      write(6,504) YI(1),YI(jm)
 504  format(' YI(1),YI(jm)=',2f9.3)
c------------ use for debug in ocean model grid
       ip=60
       jp=97
c------------
c
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
c---------------
      print *,' GDEM grid im1,jm1=',im1,jm1
      write(6,501) X(1),X(im1)
 501  format(' X(1),X(im1)=',2f9.3)
      write(6,502) Y(1),Y(jm1)
 502  format(' Y(1),Y(jm1)=',2f9.3)
c---------------
c     rewind(8)
      read (8,11) fin,fin1
 11   format(10f6.2)
c     close(8)
cRMY 11/01/06 interpolate GDEM climatology between months
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
c Code does not currently interpolate gdem3 or Levitus
c Doing this requires code mods in subroutine mixLevGDEMm
cRMY 11/01/06 end 2 of 2---------------------------------
c-------------- horiz. interpolate from GDEM grid to ocean grid 
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
          call horint(TEM,TEMI,X,Y,XI,YI,IM1,JM1,IM,JM)
c-------------
          do j=1,jm
            do i=1,im
              if(n.eq.1) then
                TB1(i,j,k)=temi(i,j)
              else
                SB1(i,j,k)=temi(i,j)
              end if
            end do
          end do
        end do
      end do
c---------------------
c------ check TB1
      print *,' check  TB1(ip,jp,k),k=1,nl GDEM data'
      write(6,303) (TB1(ip,jp,k),k=1,nl)
      print *,' check  SB1(ip,jp,k),k=1,nl'
      write(6,303) (SB1(ip,jp,k),k=1,nl)
 303  format(10f7.2)
c
c--------- read gdem3 monthly climate data with 0.25deg resolution
c--------- these data have -9. under bottom gdem3 topography
c
c------------ 10-21-05 do not read gdem3 here; use mixLevGDEMm
c     read(24,104) t
c     read(24,104) s
 104  format(10f7.3)
c
c------ check t,s
c     print *,' check  t(ip,jp,k),k=1,nl gdem3 data'
c     write(6,303) (t(ip,jp,k),k=1,nl)
c     print *,' check  s(ip,jp,k),k=1,nl gdem3 data'
c     write(6,303) (s(ip,jp,k),k=1,nl)
c
c----------  send GDEM data where gdem t=-9.
c----------       and smoothed mixed data
c
      call mixLevGDEMm(TB1,SB1,XI,YI,Zlev,H,FSM,
     *                      t,s,im,jm,nl,mmx)
c
      call stabil(t,Zlev,im,im,jm,nl)
c
c-------- 03-20-03 check t,s after mixing GDEM and gdem3 data
c
      print *,'check t after mixing GDEM and gdem3 data'
      call checkGDEM(t,FSM,H,Zlev,'   t')
      print *,'check s after mixing GDEM and gdem3 data'
      call checkGDEM(s,FSM,H,Zlev,'   s')
c
c------------- save t,s
c
      do l=1,nl
       do j=1,jm
        do i=1,im
         TB1(i,j,l)=t(i,j,l)
         SB1(i,j,l)=s(i,j,l)
        end do
       end do
      end do
c
      print *,' check after stabil  TB1(ip,jp,k),k=1,nl'
      write(6,303) (TB1(ip,jp,k),k=1,nl)
      print *,' check after stabil  SB1(ip,jp,k),k=1,nl'
      write(6,303) (SB1(ip,jp,k),k=1,nl)
c------------ 01-28-03 add print to fort.82
c     write(82) t
c     write(82) s
c
c------------- check H
c
      Hmax=0.
      ishore=0
      ishallow=0
      ishallow1=0
      do i=1,im
      do j=i,jm
       if(FSM(i,j).lt.0.5.and.H(i,j).gt.1.1) ishore=ishore+1
       if(FSM(i,j).gt.0.5.and.H(i,j).lt.10.) ishallow=ishallow+1
       if(H(i,j).gt.1..and.H(i,j).le.10.) ishallow1=ishallow1+1
       if(H(i,j).gt.Hmax) Hmax=H(i,j)
      end do
      end do
c
      print *,'check H'
      print *,'ishore,ishallow,ishallow1=',ishore,ishallow,ishallow1
      print *,'     Hmax=',Hmax
c
c----------- 12-30-02 check Mona Passage before pathCURR
c
      print *,'Mona Passage before pathCURR'
      call pritem3c(t,XI,YI,im,jm,nl,-72.05,1,15.00,22.05,
     *      't l=15 z=600  ',-1,15,cos30,-1)
      call pritem3c(t,XI,YI,im,jm,nl,-70.13,1,15.00,22.05,
     *      't l=15 z=600  ',-1,15,cos30,-1)
      call pritem3c(t,XI,YI,im,jm,nl,-68.02,1,15.00,22.05,
     *      't l=15 z=600  ',-1,15,cos30,-1)
      call pritem3c(t,XI,YI,im,jm,nl,-66.10,1,15.00,22.05,
     *      't l=15 z=600  ',-1,15,cos30,-1)
c
c-----------  sharp t for TR1-TR4 and BAY
c-----------  for each traj. take TB1, send -9. below H
c-----------  before sharpening and send results after 
c-----------  sharpening to t,s.
c-----------  For BAY also correct traj. and temp. before
c-----------  sharpening
c
c---------------- falk 08-15-05 add file aladj
      call pathCURR(TB1,SB1,XI,YI,Zlev,H,FSM,t,s,
     *                   t12N,t12S,aladj)
c
c----------- 12-30-02 check Mona Passage after pathCURR
c
      print *,'Mona Passage after pathCURR'
      call pritem3c(t,XI,YI,im,jm,nl,-72.05,1,15.00,22.05,
     *      't l=15 z=600  ',-1,15,cos30,-1)
      call pritem3c(t,XI,YI,im,jm,nl,-70.13,1,15.00,22.05,
     *      't l=15 z=600  ',-1,15,cos30,-1)
      call pritem3c(t,XI,YI,im,jm,nl,-68.02,1,15.00,22.05,
     *      't l=15 z=600  ',-1,15,cos30,-1)
      call pritem3c(t,XI,YI,im,jm,nl,-66.10,1,15.00,22.05,
     *      't l=15 z=600  ',-1,15,cos30,-1)
c
c---------- 01-31-03 call pathGS after pathCURR to use t12S
cn
c------------   GS path ------------------
c
c------ read specified GS path in Fl. Str., read GS climate data
c------  from URI for each month, use sklclim to connect them
c------   and interpolate connected path to even steps,
c------    increase radius of curvature to Rmin, calculate perp.
c------     lines to GS path and sharp t,s (call sharpgs)
c------      send t12 along GS path (t12N,t12S,t12NB)
c
c---------------- falk 08-15-05 add file aladj
      call pathGS(TB1,SB1,XI,YI,Zlev,H,FSM,t,s,
     *                   t12N,t12S,aladj)
c
c-------- write XI,YI,Zlev,H,FSM,T,S after sharp
c
c     rewind(28)
c     write(28,101) IM,JM,nl
c     write(28,100) XI
c     write(28,100) YI
c     write(28,102) Zlev
c     write(28,102) H
c     write(28,103) FSM
c     write(28,103) t
c     write(28,103) s
c     close(28)
 101  format(3i7)
 100  format(10f8.3)
 103  format(10f7.2)
c
c---------- send t,s to TB1,SB1 after sharpening
c---------- do not send -9. under bottom 
c
      do i=1,im
       do j=1,jm
        do l=1,nl
         if(t(i,j,l).gt.-8.) TB1(i,j,l)=t(i,j,l)
         if(s(i,j,l).gt.-8.) SB1(i,j,l)=s(i,j,l)
        end do
       end do
      end do
c--------- 04-17-02 remove instability after last changes
      call stabil(TB1,Zlev,im,im,jm,nl)
c
c-------- 03-20-03 check t,s after sharpening
c
      print *,'check TB1,SB1 after sharpening along all currents'
      call checkGDEM(TB1,FSM,H,Zlev,' TB1')
      call checkGDEM(SB1,FSM,H,Zlev,' SB1')
c
      print *,'               END sharp_mcs'
c---------------- write initdata
      write(13) TB1
      write(13) SB1
c----------------
      stop
      end
c
c-----------
c
C  *** SPLINE INTERP. BOUND. COND.:SECOND DERIVATIVES IS EQUEL TO 0
C  ***GIVEN Y=Y(X),X(K),K=1,KK CALCULATE R(Z) IN POINTS Z(J),
C  ***J=1,JJ  R1=DR/DZ: KK and JJ number of points processed
C  ***                  KKM<500, KKM is a size of X and Y
C  ***                  JJM<500, JJM is a size of R,R1,Z
C
      SUBROUTINE SP(X,Y,R,R1,Z,KK,JJ,KKM,JJM)
      DIMENSION X(KKM),Y(KKM),R(JJM),R1(JJM),Z(JJM),
     *P(500),PM(500),H(500),A(500),B(500),C(500),D(500),
     *V(500),Q(500)
      DO 200 K=2,KK
  200 H(K)=X(K)-X(K-1)
      PM(1)=0.
      PM(KK)=0.
      K1=KK-1
      DO 201 K=2,K1
      C1=H(K)
      C2=H(K+1)
      B(K)=2.
      C(K)=C2/(C1+C2)
      A(K)=1-C(K)
  201 D(K)=6.*((Y(K+1)-Y(K))/C2-(Y(K)-Y(K-1))/C1)/(C1+C2)
C
      Q(1)=0.
      V(1)=0.
      DO 202 K=2,K1
      P(K)=A(K)*Q(K-1)+B(K)
      Q(K)=-C(K)/P(K)
      V(K)=(D(K)-A(K)*V(K-1))/P(K)
  202 CONTINUE
C
      DO 203 K=2,K1
      I=KK+1-K
  203 PM(I)=Q(I)*PM(I+1)+V(I)
      K=2
C
      DO 204 J=1,JJ
      E=Z(J)
      F=ABS(E-X(K-1))
      G=ABS(X(K)-X(K-1))*1.E-6
      IF(F.LT.G) GOTO 207
  205 IF((E-X(K-1))*(E-X(K))) 207,207,206
  206 IF(K.EQ.KK) GOTO 207
      K=K+1
      GOTO 205
C
  207 A1=PM(K-1)
      A2=PM(K)
      B1=Y(K-1)
      B2=Y(K)
      C1=X(K)-E
      C2=E-X(K-1)
      C3=H(K)
C
      R(J)=(A1*(C1**3)+A2*(C2**3)+(B1*6.-A1*C3*C3)
     **C1+(B2*6.-A2*C3*C3)*C2)/(6.*C3)
      R1(J)=(-A1*C1*C1*0.5+A2*C2*C2*0.5+B2-B1)/C3-
     *(A2-A1)*C3/6.
  204 CONTINUE
      RETURN
      END
c
c---------
c
      subroutine filtr1D(t,nn,n1,n2,kf)
      dimension t(nn)
      dimension pk(11),mf(11),wt(500),wtt(500)
      data mf/2,3,4,2,5,6,7,2,8,9,2/
c------- pk koeff for filtr from Kurihara et al., 1993
c------- kf=1      removes sinusoidal waves 2h
c------- kf=2      removes sinusoidal waves 3h
c------- kf=3 or 4 removes sinusoidal waves 4h
c------- kf=5      removes sinusoidal waves 5h
c------- kf=6      removes sinusoidal waves 6h
c------- kf=7      removes sinusoidal waves 7h
c------- kf=9      removes sinusoidal waves 8h
c------- kf=10 or 11 removes sinusoidal waves 9h
c
c----------- filtering 1D array t(nn) from n1 to n2
c
c-------------        nn=< 500 -------
c
       PI=3.141592
       do j=1,kf
        pk(j)=0.5/(1.-cos(2.*PI/float(mf(j))))
       end do
c
       do n=n1,n2
        wt(n)=t(n)
       end do
c
       do j=1,kf
        do n=n1+1,n2-1
         wtt(n)=wt(n)+pk(j)*(wt(n-1)+wt(n+1)-2.*wt(n))
        end do
        do n=n1+1,n2-1
         wt(n)=wtt(n)
        end do
       end do
c
       do n=n1+1,n2-1
        t(n)=wt(n)
       end do
      return
      end
c
c-------------------
c
c---------------- falk 02/15/01 include migw mige
c------   if migw=1 gs path include west bnd
c------   if mige=1 gs path include east bnd
c
      subroutine perp(xlongs,ylatgs,xlon2,ylat2,nn,stepn,mrgs,mmx, 
     *                cos30,migw,mige)
      dimension xlongs(mmx),ylatgs(mmx),xlon2(mmx,nn),ylat2(mmx,nn)
      dimension sinal(mmx),cosal(mmx),np(20)
c
      F(x1,y1,x,y)=sqrt((y1-y)**2+(x1-x)**2)
c
c------ subr. calculates points along perpendicular to GS path
c       input   xlongs,xlatgs, mrgs-length of these files
c       output  xlon2,ylat2, size of these files (mrgs,nn)
c------ along perpendicular number increases from warm to cold
c
       write(6,101) nn,mrgs,stepn
 101   format(/10x,' SUBR. PERP',
     *        /10x,'nn,mrgs,stepn',
     *        /10x,2i7,f7.2)
       print *,'migw,mige=',migw,mige
c
      m0=86
c
      do m=1,mrgs
        x0=xlongs(m)
        y0=ylatgs(m)
       if(m.ge.3.and.m.le.(mrgs-2)) then
c-------------- change 08/25/00  as in curvchange
c       x1=(xlongs(m-1)+xlongs(m-2))*0.5
c       y1=(ylatgs(m-1)+ylatgs(m-2))*0.5
c       x2=(xlongs(m+1)+xlongs(m+2))*0.5
c       y2=(ylatgs(m+1)+ylatgs(m+2))*0.5
        x1=xlongs(m-1)
        y1=ylatgs(m-1)
        x2=xlongs(m+1)
        y2=ylatgs(m+1)
       else
        if(m.eq.1) then
         x1=xlongs(m)
         y1=ylatgs(m)
         x2=xlongs(m+1)
         y2=ylatgs(m+1)
         if(migw.eq.1) y2=y1
        end if
        if(m.eq.2.or.m.eq.(mrgs-1)) then
         x1=xlongs(m-1)
         y1=ylatgs(m-1)
         x2=xlongs(m+1)
         y2=ylatgs(m+1)
        end if
        if(m.eq.mrgs) then
         x1=xlongs(m-1)
         y1=ylatgs(m-1)
         x2=xlongs(m)
         y2=ylatgs(m)
         if(mige.eq.1) y2=y1
        end if
       end if
       A=-(y2-y1)
       B=x2-x1
c---     calculate sin and cos GS path angle with axis x
       C=sqrt(A*A+B*B)
       sinal(m)=-A/C
       cosal(m)=B/C
c-------------
       if(m.eq.m0) then
        write(6,301) m0,A,B,C,sinal(m),cosal(m),x0,y0,x1,y1,x2,y2
 301    format(/10x,' sub perp',
     *    /10x,'m0,A,B,C,sinal(m),cosal(m),x0,y0,x1,y1,x2,y2',
     *    /10x,i7,5f7.2,/10x,6f7.2)
       end if
c-------------
c---calculate points along perpendicular to the line 1-2
       do k=1,nn
        n=k-(nn+1)/2
        if(abs(B).gt.abs(A)) then
         d=A/B
         y=y0+n*stepn*B/sqrt(A*A+B*B)
         x=x0+d*(y-y0)
        else
         d=B/A
         d2=d*d
         x=x0+n*stepn*A/(sqrt(A*A+B*B))
         y=y0+d*(x-x0)
        end if
         xlon2(m,k)=x
         ylat2(m,k)=y
       end do
      end do
c--------- falk 02/15/01 change for migw=1 or mige=1
      if(migw.eq.1) then
       do n=1,nn
        xlon2(2,n)=0.5*(xlon2(1,n)+xlon2(3,n))
        ylat2(2,n)=0.5*(ylat2(1,n)+ylat2(3,n))
       end do
      end if
      if(mige.eq.1) then
       do n=1,nn
        xlon2(mrgs-1,n)=0.5*(xlon2(mrgs,n)+xlon2(mrgs-2,n))
        ylat2(mrgs-1,n)=0.5*(ylat2(mrgs,n)+ylat2(mrgs-2,n))
       end do
      end if
c--------
c--------falk 02/15/01 change n1,n2 
      nc=(nn+1)/2
      n1=nc-5
      n2=nc+5
      ii=n2-n1+1
      do i=1,ii
       np(i)=n1+(i-1)
      end do
      write(6,202)
 202  format(/10x,' xlongs(m)/cos30,m=1,mrgs')
      write(6,102) (xlongs(m)/cos30,m=1,mrgs)
 102  format(10f7.2)
      write(6,203)
 203  format(/10x,' ylatgs(m),m=1,mrgs')
      write(6,102) (ylatgs(m),m=1,mrgs)
c
      write(6,204) n1,n2
 204  format(/10x,' xlon2(m,n)/cos30,n=n1,n2 m=1,mrgs,20,n1,n2=',2i7)
      write(6,105) (np(i),i=1,ii)
 105  format(7x,11i7)
c     do m=1,mrgs
      do m=1,mrgs,10
       write(6,104) m,(xlon2(m,n)/cos30,n=n1,n2)
 104   format(i7,11f7.2)
      end do
      write(6,205)
 205  format(/10x,' ylat2(m,n),n=n1,n2 m=1,mrgs,20')
      write(6,105) (np(i),i=1,ii)
c     do m=1,mrgs
      do m=1,mrgs,10
       write(6,104) m,(ylat2(m,n),n=n1,n2)
      end do
c
      return
      end
c
c------------
c
       subroutine model2perp(MSKVAL,u,uu,mm,nn,ll,xlon2,ylat2,char,
     *                 mmgs,nngs,xlon1,ylat1,dlon,dlat,mmx,cos30)
       dimension u(mm,nn,ll),uu(mmx,nngs,ll),
     *           xlon2(mmx,nngs),ylat2(mmx,nngs),ZT(4),ZTS(4)
       character*5 char
       real MSKVAL
c
       nc=(nngs+1)/2
       print *,'model2perp: mm,nn,ll,mmgs,nngs',mm,nn,ll,mmgs,nngs
       write(6,102) xlon1/cos30,ylat1,dlon/cos30,dlat
 102   format(/10x,'xlon1/cos30,ylat1,dlon/cos30,dlat=',2f7.2,2f7.4)
c
       m0=47
       n0=18
c
       do m=1,mmgs
       do n=1,nngs
        x0=xlon2(m,n)
        y0=ylat2(m,n)
c---         find left low corner from model grid around (x0,y0)
        ml=(x0-xlon1)/dlon+1.0001
        nb=(y0-ylat1)/dlat+1.0001
c--------------- falk 03-20-01 change check bnd
        if(ml.lt.1) then
         ml=1
         x0=xlon1
        end if
        if(ml.gt.mm-1) then
         ml=mm-1
         x0=xlon1+dlon*float(mm-1)
        end if
        if(nb.lt.1) then
         nb=1
         y0=ylat1
        end if
        if(nb.gt.nn-1) then
         nb=nn-1
         y0=ylat1+dlat*float(nn-1)
        end if
c----           check boundaries
c       if(ml.eq.0.and.abs(x0-xlon1).lt.dlon/5.) then
c         ml=1
c         x0=xlon1
c       end if
c       xe=xlon1+dlon*float(ml-1)
c       if(ml.eq.mm.and.abs(x0-xe).lt.dlon/5.) then
c         ml=mm-1
c         x0=xe
c       end if
c       if(nb.eq.0.and.abs(y0-ylat1).lt.dlat/5.) then
c         nb=1
c         y0=ylat1
c       end if
c       ye=ylat1+dlat*float(nb-1)
c       if(nb.eq.nn.and.abs(y0-ye).lt.dlat/5.) then
c         nb=nn-1
c         y0=ye
c       end if
c
c       if(ml.lt.1.or.ml.gt.mm-1) go to 100
c       if(nb.lt.1.or.nb.gt.nn-1) go to 100
c-------
        x1=xlon1+dlon*float(ml-1)
        x2=x1+dlon
        y1=ylat1+dlat*float(nb-1)
        y2=y1+dlat
c
c----------
c       if(m.eq.m0.and.n.eq.n0) then
c        write(6,301) m0,n0,ml,nb,x1,x2,y1,y2,x0,y0
 301     format(/10x,' sub. model2perp',
     *          /10x,' m0,n0,ml,nb,x1,x2,y1,y2,x0,y0',
     *          /1x,4i7,6f7.2)
c       end if
c----------
c
        do l=1,ll
c
         ZT(1)=u(ml,nb,l)
         ZT(2)=u(ml+1,nb,l)
         ZT(3)=u(ml+1,nb+1,l)
         ZT(4)=u(ml,nb+1,l)
C
C******* CALCULATING FORMULA *****************************
C
         DO I=1,4
          IF(ZT(I).EQ.MSKVAL) THEN
            ZTS(I)=0.
          ELSE
           ZTS(I)=1.
          END IF
         END DO
C
         a=   ZTS(1)*ZT(1)*(X2-X0)*(Y2-Y0)
     1       +ZTS(2)*ZT(2)*(X0-X1)*(Y2-Y0)
     2       +ZTS(3)*ZT(3)*(X0-X1)*(Y0-Y1)
     3       +ZTS(4)*ZT(4)*(X2-X0)*(Y0-Y1)
         AREA=ZTS(1)*ABS((X2-X0)*(Y2-Y0))
     1       +ZTS(2)*ABS((X0-X1)*(Y2-Y0))
     2       +ZTS(3)*ABS((X0-X1)*(Y0-Y1))
     3       +ZTS(4)*ABS((X2-X0)*(Y0-Y1))
C
c------------ 04-19-02 check more carefully
         IF(AREA.LT.(0.1*ABS(X2-X1)*ABS(Y2-Y1))) THEN
c        IF(AREA.EQ.0.) THEN
          a=MSKVAL
         ELSE
          a=a/AREA
         END IF
c
         uu(m,n,l)=a
c
c---------
c        if(m.eq.m0.and.n.eq.n0) then
c         write(6,302) l,ZT,uu(m,n,l)
 302      format(/1x,'l,ZT,uu(m,n,l)=',i7,5f11.2)
c        end if
c---------
c
        end do
c-------------   end loop in l
c
 100    continue
c
       end do
       end do
c-------------- end loops in n and m
c
      n1=nc-5
      n2=nc+5
c     write(6,209) char,n1,n2
 209  format(/10x,A5,' l=1   m=1,20,n=n1,n2=',2i7)
c     write(6,404) (n,n=n1,n2)
c     do m=1,20
c      write(6,104) m,(uu(m,n,1),n=n1,n2)
c     end do
 109   format(i7,11f8.2)
c
      m1=mmgs-20
      m2=mmgs
c     write(6,210) char,m1,m2,n1,n2
 210  format(/10x,A5,' l=1     m=m1,m2,n=n1,n2=',4i7)
c     write(6,404) (n,n=n1,n2)
c     do mr=m1,m2
c      m=m2-mr+m1
c      write(6,104) m,(uu(m,n,1),n=n1,n2)
c     end do
c     
      write(6,204) char,n1,n2
 204  format(/10x,A5,' l=1   n=n1,n2 m=1,mmgs,20',2i7)
      write(6,404) (n,n=n1,n2)
 404  format(6x,11i8)
c     do m=1,mmgs
      do m=1,mmgs,10
       write(6,104) m,(uu(m,n,1),n=n1,n2)
 104   format(i7,11f8.2)
      end do
c
      go to 55
      if(ll.eq.1) go to 55
c
      write(6,206) char,n1,n2
 206  format(/10x,A5,'  z=200m l=10,n=n1,n2 m=1,mmgs,10',2i7)
      write(6,404) (n,n=n1,n2)
c     do m=1,mmgs
      do m=1,mmgs,10
       write(6,104) m,(uu(m,n,10),n=n1,n2)
      end do
c
      write(6,205) char,n1,n2
 205  format(/10x,A5,'  z=400m l=13,n=n1,n2 m=1,mmgs,10',2i7)
      write(6,404) (n,n=n1,n2)
c     do m=1,mmgs
      do m=1,mmgs,10
       write(6,104) m,(uu(m,n,13),n=n1,n2)
      end do
c
      write(6,215) char,n1,n2
 215  format(/10x,A5,'  z=400m l=13,n=n1,n2 m=1,40',2i7)
      write(6,404) (n,n=n1,n2)
      do m=1,40
       write(6,104) m,(uu(m,n,13),n=n1,n2)
      end do
c
      write(6,207) char,n1,n2
 207  format(/10x,A5,'  z=600m l=15,n=n1,n2 m=1,mmgs,10',2i7)
      write(6,404) (n,n=n1,n2)
c     do m=1,mmgs
      do m=1,mmgs,10
       write(6,104) m,(uu(m,n,15),n=n1,n2)
      end do
c
      write(6,208) char,n1,n2
 208  format(/10x,A5,'  z=400m l=13,n=n1,n2 m=mmgs-30,mmgs',2i7)
      write(6,404) (n,n=n1,n2)
      do m=mmgs-30,mmgs
c     do m=1,mmgs,20
       write(6,104) m,(uu(m,n,13),n=n1,n2)
      end do
c
      print *,' l=10(200m), l=13(400m),l=19(1000m)' 
      write(6,220) char
 220  format(/1x,A5,' lon/cos30  lat  l=1 l=10 l=13 l=19 l=ll')
      do m=1,mmgs,5
       write(6,221) m,xlon2(m,nc)/cos30,ylat2(m,nc),uu(m,nc,1),
     *       uu(m,nc,10),uu(m,nc,13),uu(m,nc,19),uu(m,nc,ll)
 221   format(1x,i7,2f8.2,5f7.2)
      end do
 55   continue
      print *,'   END model2perp'
      return
      end
c
c-------------
c
      subroutine t12perp(tt,xlon2,ylat2,mmx,nngs,mrgs,nl,tgs,lgs)
      dimension tt(mmx,nngs,nl),xlon2(mmx,nngs),ylat2(mmx,nngs)
      dimension ncnew(500),xlonnew(500),ylatnew(500)
      do m=1,mrgs
       ncnew(m)=0
       do n=1,nngs-1
        if(tt(m,n,lgs).ge.tgs.and.tt(m,n+1,lgs).le.tgs) then
         ncnew(m)=n
c        a=tt(m,n,lgs)
c        b=tt(m,n+1,lgs)
        end if
       end do
      end do
      print *,' t12perp:  (ncnew(m),m=1,mrgs)'
      write(6,101) (ncnew(m),m=1,mrgs)
 101  format(10i7)
      return
      end
c
c------------
c
      subroutine t12(t,XI,YI,xb12,xe12,yb12,ye12,im,jm,nl,tgs,lgs,
     *               xlon12,ylat12,cos30,ii12)
      parameter(mmx=500)
      dimension t(im,jm,nl),XI(im),YI(jm),xlon12(mmx),ylat12(mmx)
c
c
c-------   xb12, xe12 the first and last longitudes of path t=12
c          ib12 and ie12 the closest points of model grid 
c-------     to xb12 and xe12
c
c-------   find t=12 between yb12 and ye12 longitudes
c
c-------   for GS and TAIL yb12=30N and path goes from west to east
c-------   for SGYRE yb12=11N  and path goes from east to west
c
      print *,' subr. t12: im,jm,nl,mmx=',im,jm,nl,mmx
      write(6,201) xb12,xe12,yb12,ye12
 201  format('xb12,xe12,yb12,ye12=',4f7.2)
c
      call findi(XI,im,xb12,ib12)
      call findi(XI,im,xe12,ie12)
      call findj(YI,jm,yb12,jb12)
      call findj(YI,jm,ye12,je12)
c-------------
      print *,'the first longitudes of path t=12: xb12=',xb12/cos30
      print *,'the last  longitudes of path t=12: xe12=',xe12/cos30
      print *,'the closest point of model grid to xb12: ib12=',ib12
      print *,'the closest point of model grid to xe12: ie12=',ie12
c-------------
      if(yb12.gt.20.) then 
       mig=1
      else 
       mig=-1
      end if
c------------- mig=1 path goes to east, mig=-1 to west
      ii12=abs(ie12-ib12)+1
c
      do i=1,ii12
       k=ib12+mig*(i-1)
       xlon12(i)=XI(k)
c--------- 06-06-02 include check if there is 12deg
       ylat12(i)=0.    
       do j=jb12,je12
        if(t(k,j,lgs).ge.tgs.and.t(k,j+mig,lgs).le.tgs) then
         a=t(k,j,lgs)
         b=t(k,j+mig,lgs)
         c=YI(j)
         d=YI(j+mig)
         if (abs(b-a).gt.e-6) then
          ylat12(i)=c+(tgs-a)*(d-c)/(b-a)
         else
          ylat12(i)=c
         end if
         go to 200        
        end if
       end do
 200   continue
c--------- 06-06-02 include check if there is 12deg
c---------   assume that for i=1 ylat12(1) is not zero
       if(i.gt.1) then
        dylat=abs(ylat12(i)-ylat12(i-1))
        dxlon=2.*abs(xlon12(i)-xlon12(i-1))
        if(dylat.gt.dxlon) ylat12(i)=ylat12(i-1)
       end if
      end do
      print *,' number of points along longitudes ii12=',ii12
      print *,' t12:  (xlon12(i)/cos30,i=1,ii12)'
      write(6,103) (xlon12(i)/cos30,i=1,ii12)
      print *,' t12:  (ylat12(i),i=1,ii12)'
      write(6,103) (ylat12(i),i=1,ii12)
 103  format(10f7.2)
      return
      end
c
c----- 04-26-01 make outflow (not to change t,s)
c
      subroutine outflow(alatgs12,mmgs,mout,yout,moutc)
      parameter(mmx=500)
      dimension alatgs12(mmx),alatnew(mmx)
c
c------  develops ouflow perp to east bnd on OTIS grid and
c------  creates sponge area to the west from const latitude
c------                           of outflow
c------  mout is a number of points for sponge area
c------  moutc is a number of points where ylatgs=yout
c------       these numbers for step dlat=0.2
c------       mout=10,  moutc=8
c
      mb=mmgs-mout-moutc
      me=mmgs-moutc
c     am=0.
c     do m=me,mmgs
c      am=am+alatgs12(m)
c     end do
c     yout=am/(moutc+1)
c------------------
      print *,'                               outflow'
      write(6,101) mmgs,mout,moutc,mb,me,yout
 101  format(/10x,'mmgs,mout,moutc,mb,me,yout=',5i7,f7.2)
c------------------
c
      do m=mb,me
       al=float(m-mb)/float(mout)
       alatnew(m)=(1.-al)*alatgs12(mb)+al*yout
      end do
      do m=me,mmgs
       alatnew(m)=yout
      end do
c-----------    smooth alatnew 3 times
      alatnew(mb-1)=alatgs12(mb-1)
      do it=1,3
      do m=mb,me
       alatnew(m)=(alatnew(m-1)+alatnew(m+1))*0.25+alatnew(m)*0.5
      end do
      end do
c------------------
      write(6,113) mb,me,mmgs
 113  format(/10x,'(old alatgs12(m),m=mb,mmgs),mb,me,mmgs=',3i7)
      write(6,112) (alatgs12(m),m=mb,mmgs)
 112  format(10f7.2)
      print *,' (alatnew(m),m=mb,mmgs)'
      write(6,112) (alatnew(m),m=mb,mmgs)
c------------------
      do m=mb,mmgs
       alatgs12(m)=alatnew(m)
      end do
      return
      end
c
c-----------
c
      subroutine rad(xlongs,ylatgs,mmx,mrgs)
      dimension xlongs(mmx),ylatgs(mmx),Radk(500)
      dimension x(500),y(500),mb(100),me(100)
      dimension delx(100),dely(100)
cRMY      real*16 A,H
cRMY      real*16 O,Z
      real A,H
      real O,Z
      x(1)=xlongs(1)
      y(1)=ylatgs(1)
      do m=2,mrgs
c      x(m)=(xlongs(m)+xlongs(m-1))*0.5
c      y(m)=(ylatgs(m)+ylatgs(m-1))*0.5
       x(m)=xlongs(m)
       y(m)=ylatgs(m)
      end do
      delx(1)=0.
      dely(1)=0.
      do i=2,40
       delx(i)=x(i+1)-x(i-1)
       dely(i)=y(i+1)-y(i-1)
      end do
      print *,' (x(m),m=1,40)'
      write(6,107) (x(m),m=1,40) 
      print *,' (y(m),m=1,40)'
      write(6,107) (y(m),m=1,40) 
 107  format(10f7.3)
      print *,' (delx(m),m=1,40)'
      write(6,107) (delx(m),m=1,40) 
      print *,' (dely(m),m=1,40)'
      write(6,107) (dely(m),m=1,40) 
c
      Radk(1)=100.
      Radk(mrgs)=100.
      it=0
 100  continue
      do m=2,mrgs-1
       ax=x(m+1)
       bx=x(m-1)
       cx=x(m)
       ay=y(m+1)
       by=y(m-1)
       cy=y(m)
       xp1=(ax-bx)/0.2
       yp1=(ay-by)/0.2
       xp2=(ax-2.*cx+bx)/0.01
       yp2=(ay-2.*cy+by)/0.01
       a=xp1*xp1+yp1*yp1
       b=sqrt(a)
       d=a*a*a
       e=xp1*yp2-xp2*yp1
       if(abs(e).lt.1.E-3) then
        Radk(m)=100.
       else
        Radk(m)=d/e
       end if
c-----------
       if(m.eq.14) then
        write(6,105) xp1,yp1,xp2,yp2,a,b,d,e,Radk(m)
 105    format(/1x,'xp1,yp1,xp2,yp2,a,b,d,e,Radk(m)',
     *        /1x,9f7.4)
       end if
c-----------
      end do
c-----------
      print *,' rad: (Radk(m),m=1,mrgs) mrgs=',mrgs
      print *,'                             it=',it
      write(6,101) (Radk(m),m=1,40)
 101  format(10f7.2)
c------------
      do k=1,100
       mb(k)=0
       me(k)=0
      end do
      rmin=2.
      mig=1
      k=1
      do m=4,mrgs-3
       if(abs(Radk(m)).lt.rmin.and.mig.eq.1) then
        mb(k)=m-3
        mig=2
       end if
       if(mb(k).gt.0.and.me(k).eq.0) then
        if(abs(Radk(m)).ge.rmin.and.mig.eq.2) then
         me(k)=m+3
         mig=1
         k=k+1
        end if
       end if
      end do
      kk=k-1
      if(mb(kk).gt.0.and.me(kk).eq.0) me(kk)=mrgs-1
c---------------
      print *,' mb(k),k=1,kk     kk=',kk
      write(6,104) (mb(k),k=1,kk)
      print *,' me(k),k=1,kk     kk=',kk
      write(6,104) (me(k),k=1,kk)
 104  format(10i7)
c---------------
      do k=1,kk
       m=mb(k)
       dx=x(m+1)-x(m-1)
       dy=y(m+1)-y(m-1)
       if(abs(dx).gt.abs(dy)) then
        mig=1
       else
        mig=2
       end if
       do m=mb(k),me(k)
        dx=x(m+1)-x(m-1)
        dy=y(m+1)-y(m-1)
        if(mig.eq.1.and.abs(dx).le.abs(dy)) then
         mc=m
         goto 200
        end if
        if(mig.eq.2.and.abs(dx).gt.abs(dy)) then
         mc=m
         goto 200
        end if
       end do
 200   continue
       print *,' mig,mc=',mig,mc
       if(mig.eq.1) then
        call smth(y,mb(k),mc,500,0.5)
        call smth(x,mc,me(k),500,0.5)
       else
        call smth(x,mb(k),mc,500,0.5)
        call smth(y,mc,me(k),500,0.5)
       end if
      end do
      it=it+1
      if(it.le.3) go to 100
      return
      end
c
c-------------
c
      subroutine smth(f,i1,i2,ii,al)
      dimension f(ii),wr(500)
      do i=i1+1,i2-1
       wr(i)=f(i)+al*(f(i+1)-2.*f(i)+f(i-1))
      end do
      do i=i1+1,i2-1
       f(i)=wr(i)
      end do
      return
      end
c
c-------------
c
      subroutine curvchange(olongs,olatgs,mmx,mrgs,oRmin)
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
      dimension xlongs(500),ylatgs(500),Radk(500),oRadk(500)
      dimension olongs(mmx),olatgs(mmx),olondel(500),olatdel(500)
      dimension oradmin(500)
      F(x1,y1,x2,y2)=sqrt((y1-y2)**2+(x1-x2)**2)
c-------------- falk 02/15/01 change Rmin for nngs=51
c     Rmin=2.15
c     Rmin=1.5
c----- falk 12-26-01
      Rmin=oRmin
      Radk(1)=5.
      Radk(mrgs)=5.
      do m=1,mrgs
       xlongs(m)=olongs(m)
       ylatgs(m)=olatgs(m)
      end do
      it=1
 300  continue
      do m=2,mrgs-1
       x0=xlongs(m)
       y0=ylatgs(m)
       x1=xlongs(m-1)
       y1=ylatgs(m-1)
       x2=xlongs(m+1)
       y2=ylatgs(m+1)
c-----    find r radius of curvature in point x0,y0
c--------- falk 07-12-02 include calc. sign of r rsign
       call radius(x0,y0,x1,y1,x2,y2,r,m,rsign)
       Radk(m)=r
      end do
      oradmin(it)=1000.
      do m=1,mrgs
       oRadk(m)=Radk(m)
c--------- falk 07-12-02 use r with sign
       if(oradmin(it).gt.abs(oRadk(m))) oradmin(it)=abs(oRadk(m))
      end do
c-----------
      if(it.eq.1.or.mod(it,20).eq.0) then
       print *,' curvchange: (Radk(m),m=1,mrgs) mrgs=',mrgs
       print *,'                             it=',it
       write(6,201) (oRadk(m),m=1,mrgs)
      end if
 201  format(10f7.2)
c------------
c     go to 200
c
      do m=2,mrgs-1
c--------- falk 07-12-02 use r with sign
       if(dabs(Radk(m)).gt.Rmin) go to 200
c
       x0=xlongs(m)
       y0=ylatgs(m)
       x1=xlongs(m-1)
       y1=ylatgs(m-1)
       x2=xlongs(m+1)
       y2=ylatgs(m+1)
c-----     change coord of x0,y0 that r=Rmin
       xm=(x1+x2)*0.5
       ym=(y1+y2)*0.5
       d1m=F(x1,y1,xm,ym)
       rd=dsqrt(Rmin*Rmin-d1m*d1m)
       rm=Rmin-rd
c---------------
c      if(m.eq.14.or.m.eq.19) then
c       print *,'        curvchange, m=',m
c       ox0=x0
c       oy0=y0
c       ox1=x1
c       oy1=y1
c       ox2=x2
c       oy2=y2
c       write(6,101) ox0,oy0,ox1,oy1,ox2,oy2
 101    format(/1x,'x0,y0,x1,y1,x2,y2=',6f7.3)
c       oxm=xm
c       oym=ym
c       od1m=d1m
c       oRmin=Rmin
c       ord=rd
c       orm=rm
c       write(6,102) oxm,oym,od1m,oRmin,ord,orm
 102    format(/1x,'xm,ym,d1m,Rmin,rd,rm=',6f7.3)
c      end if
c---------------
       A=-(y2-y1)
       B=x2-x1
c---     calculate sin and cos GS path angle with axis x
       C=dsqrt(A*A+B*B)
       sinal=-A/C
       cosal=B/C
       if(dabs(B).gt.dabs(A)) then
         d=A/B
         yc1=ym+rd*cosal
         xc1=xm+d*(yc1-ym)
         yc2=ym-rd*cosal
         xc2=xm+d*(yc2-ym)
         d1=F(xc1,yc1,x0,y0)
         d2=F(xc2,yc2,x0,y0)
         if(d1.gt.d2) then
          y0=ym-rm*cosal
          x0=xm+d*(y0-ym)
          xc=xc1
          yc=yc1
         else
          y0=ym+rm*cosal
          x0=xm+d*(y0-ym)
          xc=xc2
          yc=yc2
         end if
       else
         d=B/A
         xc1=xm-rd*sinal
         yc1=ym+d*(xc1-xm)
         xc2=xm+rd*sinal
         yc2=ym+d*(xc2-xm)
         d1=F(xc1,yc1,x0,y0)
         d2=F(xc2,yc2,x0,y0)
         if(d1.gt.d2) then
          x0=xm+rm*sinal
          y0=ym+d*(x0-xm)
          xc=xc1
          yc=yc1
         else
          x0=xm-rm*sinal
          y0=ym+d*(x0-xm)
          xc=xc2
          yc=yc2
         end if
       end if
c---------------
c      if(m.eq.14.or.m.eq.19) then
c       print *,'        curvchange, m=',m
c       oA=A
c       oB=B
c       oC=C
c       osinal=sinal
c       ocosal=cosal
c       od=d
c       write(6,103) oA,oB,oC,osinal,ocosal,od
 103    format(/1x,'A,B,C,sinal,cosal,d=',6f7.3)
c       oxc1=xc1
c       oyc1=yc1
c       oxc2=xc2
c       oyc2=yc2
c       od1=d1
c       od2=d2
c       ox0=x0
c       oy0=y0
c       write(6,104) oxc1,oyc1,oxc2,oyc2,od1,od2,ox0,oy0
 104    format(/1x,'xc1,yc1,xc2,yc2,d1,d2,x0,y0=',
     *         /1x,8f7.3)
c      end if
c---------------
       dd0=F(xc,yc,x0,y0)
       dd1=F(xc,yc,x1,y1)
       dd2=F(xc,yc,x2,y2)
c---------------
c      if(m.eq.14.or.m.eq.19) then
c       print *,'        curvchange, m=',m
c       oxc=xc
c       oyc=yc
c       odd0=dd0
c       odd1=dd1
c       odd2=dd2
c       write(6,105) oxc,oyc,odd0,odd1,odd2
 105    format(/1x,'xc,yc,dd0,dd1,dd2=',5f7.3)
c       print *,'  check radius   m=',m
c       call radius(x0,y0,x1,y1,x2,y2,r,m)
c      end if
c---------- send new x0,y0
       xlongs(m)=x0
       ylatgs(m)=y0
 200   continue
      end do
c
      it=it+1
      ora=Rmin
c     ora=ora*0.91
c------- use 201 iterations --------
      if(it.lt.201.and.ora.gt.oradmin(it-1)) 
     *             go to 300
      do m=1,mrgs
       olondel(m)=xlongs(m)
       olondel(m)=olondel(m)-olongs(m)
       olatdel(m)=ylatgs(m)
       olatdel(m)=olatdel(m)-olatgs(m)
c
       olongs(m)=xlongs(m)
       olatgs(m)=ylatgs(m)
      end do
      print *,'(olondel(m),m=1,mrgs)'
      write(6,106) (olondel(m),m=1,mrgs)
      print *,'(olatdel(m),m=1,mrgs)'
      write(6,106) (olatdel(m),m=1,mrgs)
 106  format(10f7.3)
      print *,' (oradmin(i),i=1,it-1)  it=',it
      write(6,107) (oradmin(i),i=1,it-1)
 107  format(10f7.2)
      return
      end
c
c------------
c
       subroutine chdist(xlon2,ylat2,mmx,nngs,mrgs)
       dimension xlon2(mmx,nngs),ylat2(mmx,nngs)
       dimension ddmax(100),ddmin(100),dl(2)
c
c----------- all longitudes were multiplied by cos30
c      F(x1,y1,x2,y2)=sqrt(((x1-x2)*cos30)**2+(y1-y2)**2)
       F(x1,y1,x2,y2)=sqrt((x1-x2)**2+(y1-y2)**2)
c      PI=3.1415927
c      cos30=cos(PI*30./180.)
        dmax=0.
        dmin=1000.
        do m=2,mrgs
        do n=1,nngs
         x1=xlon2(m-1,n)
         y1=ylat2(m-1,n)
         x2=xlon2(m,n)
         y2=ylat2(m,n)
         d=F(x1,y1,x2,y2)
         if(d.gt.dmax) then
          dmax=d
          mmax=m
          nmax=n
         end if
         if(d.lt.dmin) then
          dmin=d
          mmin=m
          nmin=n
         end if
        end do
        end do
        print *,' chdist: max and min dist along lines paral to GS'
        print *,'  mmx,nngs,mrgs=',mmx,nngs,mrgs 
        write(6,101) mmax,nmax,dmax
 101    format(/10x,'mmax,nmax,dmax=',2i7,f10.3)
        write(6,102) mmin,nmin,dmin
 102    format(/10x,'mmin,nmin,dmin=',2i7,f10.3)
        m=mmax
        do n=1,nngs
         x1=xlon2(m-1,n)
         y1=ylat2(m-1,n)
         x2=xlon2(m,n)
         y2=ylat2(m,n)
         ddmax(n)=F(x1,y1,x2,y2)
        end do
        print *,'  ddmax(n),n=1,nngs, mmax=',mmax
        write(6,103) (ddmax(n),n=1,nngs)
 103    format(10f7.3)       
        l=mmax-1
        do i=1,2
         if(i.eq.2) l=mmax
         x1=xlon2(l,1)
         y1=ylat2(l,1)
         x2=xlon2(l,nngs)
         y2=ylat2(l,nngs)
         dl(i)=F(x1,y1,x2,y2)
c        write(6,104) l,mmax,x1,y1,x2,y2
 104     format(/1x,'l,mmax,x1,y1,x2,y2=',2i7,4f7.3)
        end do
        print *,' length in deg along perp line for mmax-1 and mmax'
        write(6,108) dl
 108    format(/10x,2f7.4)
        m=mmin
        do n=1,nngs
         x1=xlon2(m-1,n)
         y1=ylat2(m-1,n)
         x2=xlon2(m,n)
         y2=ylat2(m,n)
         ddmin(n)=F(x1,y1,x2,y2)
        end do
        print *,'  ddmin(n),n=1,nngs, mmin=',mmin
        write(6,103) (ddmin(n),n=1,nngs)
        l=mmin-1
        do i=1,2
         if(i.eq.2) l=mmin
         x1=xlon2(l,1)
         y1=ylat2(l,1)
         x2=xlon2(l,nngs)
         y2=ylat2(l,nngs)
         dl(i)=F(x1,y1,x2,y2)
        end do
        print *,' length in deg along perp line for mmin-1 and mmin'
        write(6,108) dl
       return
       end
c
c------------
c
      subroutine skip(PLOND,PLATD,mmx,IAE,step,yfs,cos30)
      parameter(ii=500)
      dimension PLOND(mmx),PLATD(mmx)
      dimension PLONW(ii),PLATW(ii)
      logical pl1,pl2
      F(x1,y1,x2,y2)=sqrt((y1-y2)**2+(x1-x2)**2)
c-------skip data if step along GS < 0.4*step or
c        if PLOND(i)-PLOND(i-1)<0.2*step for lat>35N
        write(6,201) mmx,IAE,step,yfs
 201    format(' skip: mmx,IAE,step,yfs==',2i7,2f7.2)
c
        PLONW(1)=PLOND(1)
        PLATW(1)=PLATD(1)
        j=1
        i=2
        i0=1
c
 200    continue
        x1=PLOND(i)
        y1=PLATD(i)
        x2=PLOND(i0)
        y2=PLATD(i0)
        d=F(x1,y1,x2,y2)
        pl1=d.lt.(step*0.4)
c       pl2=y1.gt.yfs.and.abs(x2-x1).lt.(0.2*step)
        pl2=y1.gt.yfs.and.(x1-x2).lt.(0.2*step)
        if(pl1.or.pl2) then
          i=i+1
          if(i.gt.IAE) go to 300
          go to 200
        else
          j=j+1
          PLONW(j)=PLOND(i)
          PLATW(j)=PLATD(i)
          i=i+1
          i0=i-1
          if(i.gt.IAE) then
           go to 300
          else
           go to 200
          end if
        end if
 300    continue
        IAW=j
c
      print *,'    (PLOND(i)/cos30,i=1,IAE), IAE=',IAE
      write(6,101) (PLOND(i)/cos30,i=1,IAE)
      print *,'    (PLATD(i),i=1,IAE), IAE=',IAE
      write(6,101) (PLATD(i),i=1,IAE)
 101  format(10f7.2)
c
      print *,'    (PLONW(i)/cos30,i=1,IAW), IAW=',IAW
      write(6,101) (PLONW(i)/cos30,i=1,IAW)
      print *,'    (PLATW(i),i=1,IAW), IAW=',IAW
      write(6,101) (PLATW(i),i=1,IAW)
c
      do i=1,IAW
       PLOND(i)=PLONW(i)
       PLATD(i)=PLATW(i)
      end do
      IAE=IAW
      return
      end
c
c-------------
c
      subroutine sklnew(xl1,yl1,xl2,yl2,dlon,
     *                  il1,il2,xskl,dskl,cos30)
      parameter(mmx=500)
      dimension xl1(mmx),yl1(mmx),xl2(mmx),yl2(mmx),R1(mmx)
      dimension rlon(mmx),rlat(mmx),rlon1(mmx),rlat1(mmx)
      dimension rlon2(mmx),rlat2(mmx),rlat3(mmx),rlat4(mmx)
c------- sponge two trajectories in an interval xskl+dskl
c-------   use even step dlon along longitude
c-------     and create united path xl,yl
c------- paths go to east (for GS and TAIL)
c-------  dskl>0  a sponge area to east from xskl
c
      xb=xskl
      xe=xskl+dskl
c
      write(6,101) xskl/cos30,dskl/cos30,xb/cos30,xe/cos30
 101  format('sklnew: xskl/cos30,dskl/cos30,xb/cos30,xe/cos30=',4f7.2)
c
c----   find the first and last point for 1 curve in sponge area
c
      do i=1,il1
       if(xl1(i).ge.xb) then
        kb1=i
        go to 100
       end if
      end do
 100  continue
      do i=1,il1
       if(xl1(i).gt.xe) then
        ke1=i-1
        go to 200
       end if
      end do
 200  continue
      if(xl1(il1).le.xe) ke1=il1
c
c----   find the first and last point for 2 curve in sponge area
c
      do i=1,il2
       if(xl2(i).ge.xb) then
        kb2=i
        go to 300
       end if
      end do
 300  continue
      do i=1,il2
       if(xl2(i).gt.xe) then
        ke2=i-1
        go to 400
       end if
      end do
 400  continue
      if(xl2(il2).le.xe) ke2=il2
c------ find boundaries for interpolation
c
      xbb=max(xl1(kb1),xl2(kb2))
      xee=min(xl1(ke1),xl2(ke2))
c
c---- find number of points with even step for interpolation
      rlon(1)=xbb
      k=2
 500  rlon(k)=rlon(k-1)+dlon
      if(rlon(k).le.xee)  then
       k=k+1
       go to 500
      end if
      kk=k-1
c-----------
      print *,'      kb1,ke1,il1=',kb1,ke1,il1
      print *,'      kb2,ke2,il2=',kb2,ke2,il2
      write(6,102) xbb/cos30,xee/cos30
 102  format('xbb=max(xl1(kb1),xl2(kb2)), xee=min(xl1(ke1),xl2(ke2))',
     *        2f7.2)
      write(6,104) xl1(kb1-1)/cos30,xl1(kb1)/cos30,xl2(kb2)/cos30
 104  format('xl1(kb1-1)/cos30,xl1(kb1)/cos30,xl2(kb2)/cos30=',3f7.2)
      write(6,105) xl1(ke1)/cos30,xl2(ke2)/cos30,xl2(ke2+1)/cos30
 105  format('xl1(ke1)/cos30,xl2(ke2)/cos30,xl2(ke2+1)/cos30=',3f7.2)
c
      print *,' old  xl1(i)/cos30, i=1,il1  il1=',il1
      write(6,103) (xl1(i)/cos30,i=1,il1)
      print *,' old  yl1(i), i=1,il1  il1=',il1
      write(6,103) (yl1(i),i=1,il1)
 103  format(10f7.2)
      print *,' old  xl2(i)/cos30, i=1,il2  il2=',il2
      write(6,103) (xl2(i)/cos30,i=1,il2)
      print *,' old  yl2(i), i=1,il2  il2=',il2
      write(6,103) (yl2(i),i=1,il2)
c--------------
c-----------  send for interpolation
      k1=ke1-kb1+1
      do i=1,k1
       k=kb1+i-1
       rlon1(i)=xl1(k)
       rlat1(i)=yl1(k)
      end do
      k2=ke2-kb2+1
      do i=1,k2
       k=kb2+i-1
       rlon2(i)=xl2(k)
       rlat2(i)=yl2(k)
      end do
c--------------
      print *,' numbers of points in sponge k1,k2,kk=',k1,k2,kk
      print *,'  sponge area    rlon1(i)/cos30, i=1,k1  k1=',k1
      write(6,103) (rlon1(i)/cos30,i=1,k1)
      print *,'  sponge area    rlat1(i), i=1,k1  k1=',k1
      write(6,103) (rlat1(i),i=1,k1)
      print *,'  sponge area    rlon2(i)/cos30, i=1,k2  k2=',k2
      write(6,103) (rlon2(i)/cos30,i=1,k2)
      print *,'  sponge area    rlat2(i), i=1,k2  k2=',k2
      write(6,103) (rlat2(i),i=1,k2)
c--------------
c
      call SP(rlon1,rlat1,rlat3,R1,rlon,k1,kk,mmx,mmx)
      call SP(rlon2,rlat2,rlat4,R1,rlon,k2,kk,mmx,mmx)
c
c--------------
      print *,'  sponge area    rlon(i)/cos30, i=1,kk  kk=',kk
      write(6,103) (rlon(i)/cos30,i=1,kk)
      print *,'  sponge after SP 1st curve  rlat3(i), i=1,kk  kk=',kk
      write(6,103) (rlat3(i),i=1,kk)
      print *,'  sponge after SP 2nd curve  rlat4(i), i=1,kk  kk=',kk
      write(6,103) (rlat4(i),i=1,kk)
c--------------
      do k=1,kk
       al=float(kk-k)/float(kk-1)
       rlat(k)=al*rlat3(k)+(1.-al)*rlat4(k)
      end do
c--------------
      print *,'  sponge results:  rlat(i), i=1,kk  kk=',kk
      write(6,103) (rlat(i),i=1,kk)
c--------------
c------- send united path in xl1,yl1
      do k=1,kk
       i=kb1+k-1
       xl1(i)=rlon(k)
       yl1(i)=rlat(k)
      end do
c
      kbb1=kb1+kk
      kbb2=ke2+1
      print *,'first points after sponge kbb1,kbb2=',kbb1,kbb2
      do k=kbb2,il2
       i=kbb1-kbb2+k
       xl1(i)=xl2(k)
       yl1(i)=yl2(k)
      end do
c----------- new il1
      il1=kbb1-kbb2+il2
      print *,' new     xl1(i)/cos30, i=1,il1  il1=',il1
      write(6,103) (xl1(i)/cos30,i=1,il1)
      print *,' new     yl1(i), i=1,il1  il1=',il1
      write(6,103) (yl1(i),i=1,il1)
      return
      end
c
c-------------
c
      subroutine sklnewb(xl1,yl1,xl2,yl2,dlon,
     *                  il1,il2,xskl,dskl,cos30)
      parameter(mmx=500)
      dimension xl1(mmx),yl1(mmx),xl2(mmx),yl2(mmx),R1(mmx)
      dimension rlon(mmx),rlat(mmx),rlon1(mmx),rlat1(mmx)
      dimension rlon2(mmx),rlat2(mmx),rlat3(mmx),rlat4(mmx)
c------- sponge two trajectories in an interval xskl+-dskl
c-------   use even step dlon along longitude
c-------     and create united path xl,yl
c------- paths go to west
c------- dskl<0 a sponge area to west from xskl
c
      xb=xskl
      xe=xskl+dskl
c
c------- xb>xe
      write(6,101) xskl/cos30,dskl/cos30,xb/cos30,xe/cos30,mig
 101  format('sklnew: xskl,dskl,xb,xe//cos30,mig=',4f7.2,i7)
c
c----   find the first and last point for 1 curve in sponge area
c
      do i=1,il1
       if(xl1(i).le.xb) then
        kb1=i
        go to 100
       end if
      end do
 100  continue
      do i=1,il1
       if(xl1(i).lt.xe) then
        ke1=i-1
        go to 200
       end if
      end do
 200  continue
      if(xl1(il1).ge.xe) ke1=il1
c
c----   find the first and last point for 2 curve in sponge area
c
      do i=1,il2
       if(xl2(i).le.xb) then
        kb2=i
        go to 300
       end if
      end do
 300  continue
      do i=1,il2
       if(xl2(i).lt.xe) then
        ke2=i-1
        go to 400
       end if
      end do
 400  continue
      if(xl2(il2).ge.xe) ke2=il2
c------ find boundaries for interpolation
c
      xbb=min(xl1(kb1),xl2(kb2))
      xee=max(xl1(ke1),xl2(ke2))
c
c---- find number of points with even step for interpolation
      rlon(1)=xbb
      k=2
c------------------ 12-23-01 include nig for SGYRE
c--------      when paths go to west
 500  rlon(k)=rlon(k-1)-dlon
      if(rlon(k).ge.xee)  then
       k=k+1
       go to 500
      end if
      kk=k-1
c-----------
      print *,'      kb1,ke1,il1=',kb1,ke1,il1
      print *,'      kb2,ke2,il2=',kb2,ke2,il2
      write(6,102) xbb/cos30,xee/cos30
 102  format('xbb=min(xl1(kb1),xl2(kb2)), xee=max(xl1(ke1),xl2(ke2))',
     *        2f7.2)
      write(6,104) xl1(kb1)/cos30,xl2(kb2)/cos30,xb/cos30
 104  format('xl1(kb1)/cos30,xl2(kb2)/cos30,xb/cos30=',3f7.2)
      write(6,105) xl1(ke1)/cos30,xl2(ke2)/cos30,xe/cos30
 105  format('xl1(ke1)/cos30,xl2(ke2)/cos30,xe/cos30=',3f7.2)
c
      print *,' old  xl1(i)/cos30, i=1,il1  il1=',il1
      write(6,103) (xl1(i)/cos30,i=1,il1)
      print *,' old  yl1(i), i=1,il1  il1=',il1
      write(6,103) (yl1(i),i=1,il1)
 103  format(10f7.2)
      print *,' old  xl2(i)/cos30, i=1,il2  il2=',il2
      write(6,103) (xl2(i)/cos30,i=1,il2)
      print *,' old  yl2(i), i=1,il2  il2=',il2
      write(6,103) (yl2(i),i=1,il2)
c--------------
c-----------  send for interpolation
      k1=ke1-kb1+1
      do i=1,k1
       k=kb1+i-1
       rlon1(i)=xl1(k)
       rlat1(i)=yl1(k)
      end do
      k2=ke2-kb2+1
      do i=1,k2
       k=kb2+i-1
       rlon2(i)=xl2(k)
       rlat2(i)=yl2(k)
      end do
c--------------
      print *,' numbers of points in sponge k1,k2,kk=',k1,k2,kk
      print *,'  sponge area    rlon1(i)/cos30, i=1,k1  k1=',k1
      write(6,103) (rlon1(i)/cos30,i=1,k1)
      print *,'  sponge area    rlat1(i), i=1,k1  k1=',k1
      write(6,103) (rlat1(i),i=1,k1)
      print *,'  sponge area    rlon2(i)/cos30, i=1,k2  k2=',k2
      write(6,103) (rlon2(i)/cos30,i=1,k2)
      print *,'  sponge area    rlat2(i), i=1,k2  k2=',k2
      write(6,103) (rlat2(i),i=1,k2)
c--------------
c
      call SP(rlon1,rlat1,rlat3,R1,rlon,k1,kk,mmx,mmx)
      call SP(rlon2,rlat2,rlat4,R1,rlon,k2,kk,mmx,mmx)
c
c--------------
      print *,'  sponge area    rlon(i)/cos30, i=1,kk  kk=',kk
      write(6,103) (rlon(i)/cos30,i=1,kk)
      print *,'  sponge after SP 1st curve  rlat3(i), i=1,kk  kk=',kk
      write(6,103) (rlat3(i),i=1,kk)
      print *,'  sponge after SP 2nd curve  rlat4(i), i=1,kk  kk=',kk
      write(6,103) (rlat4(i),i=1,kk)
c--------------
      do k=1,kk
       al=float(kk-k)/float(kk-1)
       rlat(k)=al*rlat3(k)+(1.-al)*rlat4(k)
      end do
c--------------
      print *,'  sponge results:  rlat(i), i=1,kk  kk=',kk
      write(6,103) (rlat(i),i=1,kk)
c--------------
c------- send united path in xl1,yl1
      do k=1,kk
       i=kb1+k-1
       xl1(i)=rlon(k)
       yl1(i)=rlat(k)
      end do
c
      kbb1=kb1+kk
      kbb2=ke2+1
      print *,'first points after sponge kbb1,kbb2=',kbb1,kbb2
      do k=kbb2,il2
       i=kbb1-kbb2+k
       xl1(i)=xl2(k)
       yl1(i)=yl2(k)
      end do
c----------- new il1
      il1=kbb1-kbb2+il2
      print *,' new     xl1(i)/cos30, i=1,il1  il1=',il1
      write(6,103) (xl1(i)/cos30,i=1,il1)
      print *,' new     yl1(i), i=1,il1  il1=',il1
      write(6,103) (yl1(i),i=1,il1)
      return
      end
c
c-------------
c
      subroutine sklnew1(xl1,yl1,xl2,yl2,dlon,
     *                  il1,il2,dskl,cos30)
      parameter(mmx=500)
      dimension xl1(mmx),yl1(mmx),xl2(mmx),yl2(mmx)
      dimension rlon(mmx),rlat(mmx)
c------- connect two trajectories in an interval xb+dskl,
c-------     xb is the last point of the first trajectory,
c-------     create united path in xl1,yl1
c
c------- calculate long. of beginning and end connection area
      xb=xl1(il1)
      xe=xb+dskl
c
      write(6,101) dskl/cos30,xb/cos30,xe/cos30
 101  format('sklnew1: dskl/cos30,xb/cos30,xe/cos30=',3f7.2)
c
c----   find the first and last point for 2 curve in sponge area
c
      do i=1,il2
       if(xl2(i).ge.xb) then
        kb2=i
        go to 300
       end if
      end do
 300  continue
c------------ remove a very small step in united trajectory
      if(xl2(kb2)-xb.lt.dlon/3.) kb2=kb2+1
c
      do i=1,il2
       if(xl2(i).gt.xe) then
        ke2=i-1
        go to 400
       end if
      end do
 400  continue
      if(xl2(il2).le.xe) ke2=il2
c-----------
      print *,'      kb2,ke2,il2=',kb2,ke2,il2
      write(6,104) xl1(il1)/cos30,xb/cos30,xl2(kb2)/cos30
 104  format('xl1(il1)/cos30,xb/cos30,xl2(kb2)/cos30=',3f7.2)
      write(6,105) yl1(il1),yl2(kb2),yl2(ke2)
 105  format('yl1(il1),yl2(kb2),yl2(ke2)=',3f7.2)
c
c--------- send the last yl1 to ye and interpolate 
c
      ye=yl1(il1)
c-------           number of points in sponge area
      kk=ke2-kb2+1
c
      do k=1,kk
       i=kb2+k-1
       al=(xl2(i)-xb)/(xl2(ke2)-xb)
       rlon(k)=xl2(i)
       rlat(k)=al*yl2(i)+(1.-al)*ye
      end do
c--------------
      print *,'  sponge results:  rlat(k), k=1,kk   kk=',kk
      write(6,103) (rlat(k),k=1,kk)
      print *,'  sponge results:  rlon(k)/cos30, k=1,kk'
      write(6,103) (rlon(k)/cos30,k=1,kk)
 103  format(10f7.2)
c--------------
c------- send united path in xl1,yl1
      do k=1,kk
       i=il1+k
       xl1(i)=rlon(k)
       yl1(i)=rlat(k)
      end do
c
      kbb1=il1+kk+1
      kbb2=ke2+1
      print *,'first point after sponge for 1 trajectory kbb1=',kbb1
      print *,'first point after sponge for 2 trajectory kbb2=',kbb2
c
c---send points of 2 trajectory to 1 trajectory after sponge area
c
      do k=kbb2,il2
       i=kbb1-kbb2+k
       xl1(i)=xl2(k)
       yl1(i)=yl2(k)
      end do
c----------- new il1
      il1=kbb1-kbb2+il2
      print *,' number of points in united trajectory new il1=',il1
      print *,' new     xl1(i)/cos30, i=1,il1  il1=',il1
      write(6,103) (xl1(i)/cos30,i=1,il1)
      print *,' new     yl1(i), i=1,il1  il1=',il1
      write(6,103) (yl1(i),i=1,il1)
      return
      end
c
c-----------------
c
c--------- falk 07-12-02 include calc. sign of r rsign
c---------            (+ cyclone,- anticycl rotation)
c
      subroutine radius(x0,y0,x1,y1,x2,y2,r,m,rsign)
      IMPLICIT DOUBLE PRECISION (A-H,P-Z)
c------- find rad of curvature in point 0 between 1 and 2 
      F(x1,y1,x2,y2)=sqrt((y1-y2)**2+(x1-x2)**2)
      PI=3.141592
       xm1=(x1+x0)*0.5
       ym1=(y1+y0)*0.5
       xm2=(x2+x0)*0.5
       ym2=(y2+y0)*0.5
c--------------- 02-14-03 comment print
c      if(m.eq.7.or.m.eq.8) then
c       print *,'          radius:, m=',m
c       ox0=x0
c       oy0=y0
c       ox1=x1
c       oy1=y1
c       ox2=x2
c       oy2=y2
c       write(6,101) ox0,oy0,ox1,oy1,ox2,oy2
 101    format(/1x,'x0,y0,x1,y1,x2,y2=',6f7.3)
c      end if
c---------------
       A1=x0-x1
       B1=(y0-y1)
       A2=x2-x0
       B2=(y2-y0)
       C1=-B1*ym1-A1*xm1
       C2=-B2*ym2-A2*xm2
c--------------- 02-14-03 comment print
c      if(m.eq.7.or.m.eq.8) then
c       print *,'          radius:, m=',m
c       oA1=A1
c       oB1=B1
c       oC1=C1
c       write(6,103) oA1,oB1,oC1
 103    format(/1x,'A1,B1,C1=',3f7.3)
c       oA2=A2
c       oB2=B2
c       oC2=C2
c       write(6,203) oA2,oB2,oC2
 203    format(/1x,'A2,B2,C2=',3f7.3)
c       oxm1=xm1
c       oym1=ym1
c       oxm2=xm2
c       oym2=ym2
c       write(6,204) oxm1,oym1,oxm2,oym2
 204    format(/1x,'xm1,ym1,xm2,ym2=',4f7.3)
c      end if
c---------------
c-----   calculate point of intersection of two perp lines
        d=A1*B2-A2*B1
c------  abs of discriminant has to be more than E-4
        if(dabs(d).gt.1.D-4) then
         d2=B1*C2-B2*C1
         d3=C1*A2-C2*A1
         x=d2/d
         y=d3/d
c----    check distances two 3 points        
         dd0=F(x,y,x0,y0)
         dd1=F(x,y,x1,y1)
         dd2=F(x,y,x2,y2)
         r=dd0
        else
c--------- falk 07-12-02 use 10. not 100.
         r=10.D0
        end if 
c
c--------------- 02-14-03 comment print
c      if(m.eq.7.or.m.eq.8) then
c       print *,'          radius:, m=',m
c       od=d
c       od2=d2
c       od3=d3
c       write(6,104) od,od2,od3
 104    format(/1x,' d,d2,d3',
     *         /1x,3f7.4)
c       ox=x
c       oy=y
c       odd0=dd0
c       odd1=dd1
c       odd2=dd2
c       write(6,105) ox,oy,odd0,odd1,odd2
 105    format(/1x,'x,y,dd0,dd1,dd2=',5f7.3)
c      end if
c--------- falk 07-12-02 include calc. sign of r
       abs1=dsqrt(A1*A1+B1*B1)
       abs2=dsqrt(A2*A2+B2*B2)
       sinal1=B1/abs1
       sinal2=B2/abs2
       cosal1=A1/abs1
       cosal2=A2/abs2
       if(sinal1.ge.0.) then
        al1=dacos(cosal1)
       else
        al1=2.D0*PI-dacos(cosal1)
       end if
       if(sinal2.ge.0.) then
        al2=dacos(cosal2)
       else
        al2=2.D0*PI-dacos(cosal2)
       end if
       if(al2.gt.al1) then
        rsign=1.
       else
        rsign=-1.
       end if
c--------------- check if vectors are on different sides of x axis
       if(al1.lt.PI*0.5D0.and.al2.gt.PI*1.5D0) rsign=-1.
       if(al2.lt.PI*0.5D0.and.al1.gt.PI*1.5D0) rsign=1.
c--------------- 02-14-03 comment print
c      if(m.eq.7.or.m.eq.8) then
c       print *,'          radius:, m=',m
c       oabs1=abs1
c       oabs2=abs2
c       osinal1=sinal1
c       osinal2=sinal2
c       ocosal1=cosal1
c       ocosal2=cosal2
c       oal1=al1
c       oal2=al2
c       write(6,106) oabs1,oabs2,osinal1,osinal2,ocosal1,ocosal2,
c    *               oal1,oal2
 106    format('abs1,abs2,sinal1,sinal2,cosal1,cosal2,',
     *         'al1,al2',/8f10.5)
c      end if
c---------------
c--------- falk 07-12-02  use r with sign
       r=r*rsign
      return
      end
c
c------------
c
c--------------- 01-31-03 use lu,ld instead of lc1,lc2
c---------------- falk 08-15-05 add file aladj
c----------------      12-15-05 use lu=5
      subroutine pathGS(TB1,SB1,XI,YI,Zlev,H,FSM,t,s,
     *                   t12N,t12S,aladj)
      parameter(im=254,jm=225,nl=33,
     *          nngs=51,mmx=500,stepgs=0.1,stepn=0.1,
     *          lgs=13,tgs=12.,xskl=-75,dskl=2.,
     *          moutspng=6,moutcnst=5,yout=40.4,aldist=2.5,lu=5,
     *          ld=27,alonavyp=-73.,xb12p=-75.,xe12p=-50.,
     *          yb12=30.,ye12=47.5,Rmin=1.5,ncd=8,xoutp=-50.)
      real XI(IM),YI(JM)
      DIMENSION H(IM,JM),TB1(IM,JM,nl),SB1(IM,JM,nl),
     *          FSM(IM,JM),Zlev(nl)
      dimension t(im,jm,nl),s(im,jm,nl)
      dimension xlongs(mmx),ylatgs(mmx)
      dimension xnew(mmx),ynew(mmx)
      dimension xlon2(mmx,nngs),ylat2(mmx,nngs)
      dimension xlon12(mmx),ylat12(mmx)
      dimension tt(mmx,nngs,nl),ss(mmx,nngs,nl),HPE(mmx,nngs)
      dimension xgsclim(mmx),ygsclim(mmx)
      dimension t12N(nl),t12S(nl),t12NB(nl)
      real LATMIN,LATMAX,LONGMIN,LONGMAX
      COMMON/sphere/REARTH,LATMIN,LATMAX,LONGMIN,LONGMAX
      CHARACTER*55 FTGSFlSt,FTGSPATH,DATAGS,FTGSNAVY
c---------------- 04-08-03 use changetcur2 instead of sharpgs
      common/tdif/tdifbs(99),tdifes(99)
      dimension nbndN(mmx),nbndS(mmx)
      dimension tm(mmx,nngs,nl),sm(mmx,nngs,nl)
      dimension dgs(im,jm)
c---------------- falk 08-15-05 add file aladj
      dimension aladj(nl)
c
c     FTGSPATH='gspath'
c     OPEN(64,FILE=FTGSPATH,STATUS='unknown',form='formatted')
c
c---------- 03-08-01  for reading fixed GS Fl. Str. from topogpath.f
c
c     FTGSFlSt='/migr/data/wd20af/forecast_system/ocean/FSgs'
c     OPEN(65,FILE=FTGSFlSt,STATUS='unknown',form='formatted')
c
c----------- 
c
      PI=3.1415927
c-------------- 03-04-02 use for GS alat=30.
      alat=30.
      cos30=cos(PI*alat/180.)
c
c-------   step in degrees in model grid
c
      dlon=(LONGMAX-LONGMIN)/float(IM-1)
      dlat=(LATMAX-LATMIN)/float(JM-1)
c
      nc=(nngs+1)/2
c
c
c----------- multiply all longitudes on cos30
c
      LONGMIN=LONGMIN*cos30
      LONGMAX=LONGMAX*cos30
      dlon=dlon*cos30
      xout=xoutp*cos30
      do i=1,IM
       XI(i)=XI(i)*cos30
      end do
      xsklc=xskl*cos30
      dsklc=dskl*cos30
      xb12=xb12p*cos30
      xe12=xe12p*cos30
      alonavy=alonavyp*cos30
c
c---------- 03-08-01 read fixed GS path Fl. Str.  from topath_u.f
c
      read(65,106) mmnew
      read(65,105) (xlongs(m),m=1,mmnew)
      read(65,105) (ylatgs(m),m=1,mmnew)
 105  format(10f7.3)
 106  format(i7)
c--------------- xlongs was multiplied to cos30 in topath_u.f
c
      print *,'   BEGIN pathGS'
      print *,' alat,cos30=',alat,cos30
c---------------- falk 08-15-05 add check aladj
      print *, 'pathGS: aladj(nl)'
      write(6,103) aladj
c
      print *,'number points along even step FSgs:  mmnew=',mmnew
c
c------- 05-29-02 use GS climate data 
c-------     read them and interpolate to even step along GS line
c
      step=stepgs
c
      call readclgs(xgsclim,ygsclim,iiclim,step,
     *                          moutspng,moutcnst,yout)
c
c-------------- decrease radius of GS curvature to Rmin
c
      call curvchange(xgsclim,ygsclim,mmx,iiclim,Rmin)
c
c---------   calculate perp lines to a GS path
c
      call perp(xgsclim,ygsclim,xlon2,ylat2,nngs,step,iiclim,mmx,
     *                cos30,0,1)
      print *,'      iiclim=',iiclim
c
c------------- check dist between lines paral to GS
      call chdist(xlon2,ylat2,mmx,nngs,iiclim)
c---------------
c
c---------   interpolate H from model grid to perp lines
c
      call model2perp(1.,H,HPE,IM,JM,1,xlon2,ylat2,'  HPE',
     *           iiclim,nngs,LONGMIN,LATMIN,dlon,dlat,mmx,cos30)
c
      print *,'depth along GS path (0.2deg from thermal north wall)'
      print *,'      (HPE(m,nc-2),m=1,iiclim) nc,iiclim=',nc,iiclim
      write(6,102) (HPE(m,nc-2),m=1,iiclim)
 102  format(10f7.0)
c
C---------  Offset between thermal north wall and G.S. axis.
c
      do i=1,iiclim
       xgsclim(i)=xlon2(i,nc-2)
       ygsclim(i)=ylat2(i,nc-2)
      end do
      print *,' after offset between thermal nortn wall and GS axis'
      print *,'      xgsclim(i)/cos30, i=1,iiclim  iiclim=',iiclim
      write(6,103) (xgsclim(i)/cos30,i=1,iiclim)
      print *,'      ygsclim(i), i=1,iiclim  iiclim=',iiclim
      write(6,103) (ygsclim(i),i=1,iiclim)
 103  format(10f7.2)
c
c----------- skl Fl. Str. with climate from URI
c
      call sklclim(xlongs,ylatgs,xgsclim,ygsclim,
     *                  mmnew,iiclim,xsklc,dsklc,cos30)
c
c------ interpolate gs path after all sponges to even stepstepgs
c
      mrgs=mmnew
c
      print *,' # points along GS after the sponge mrgs=',mrgs
c
c------------- 04-07-03 use evenstep not evenstepgs
      call evenstep(xlongs,ylatgs,mmx,mrgs,step,cos30)
c
c----- decrease radius of GS curvature to Rmin
c             for gs path with even step=stepgs
c
      call curvchange(xlongs,ylatgs,mmx,mrgs,Rmin)
c
c-- 04-26-01 do even steps as after curvchange steps become not even
c
c------------- 04-07-03 use evenstep not evenstepgs
      call evenstep(xlongs,ylatgs,mmx,mrgs,step,cos30)
c
c---------   calculate perp lines to a GS path
c
c---------- 01-16-02 put mige=1 for pathGS for united (east bnd -50)
c
      call perp(xlongs,ylatgs,xlon2,ylat2,nngs,stepn,mrgs,mmx,
     *                cos30,0,1)
      print *,'      mrgs=',mrgs
c
c------------- check dist between lines paral to GS
c
      call chdist(xlon2,ylat2,mmx,nngs,mrgs)
c
c---------   interpolate H from model grid to perp lines
c
      call model2perp(1.,H,HPE,IM,JM,1,xlon2,ylat2,'  HPE',
     *           mrgs,nngs,LONGMIN,LATMIN,dlon,dlat,mmx,cos30)
c
      print *,'      (HPE(m,nc),m=1,mrgs) nc,mrgs=',nc,mrgs
      write(6,102) (HPE(m,nc),m=1,mrgs)
c
c---------   interpolate TB1,SB1 from model grid to perp lines
c
      call model2perp(-999.,TB1,tt,IM,JM,nl,xlon2,ylat2,'   tt',
     *            mrgs,nngs,LONGMIN,LATMIN,dlon,dlat,mmx,cos30)
c
      call model2perp(-999.,SB1,ss,IM,JM,nl,xlon2,ylat2,'   ss',
     *             mrgs,nngs,LONGMIN,LATMIN,dlon,dlat,mmx,cos30)
c
c-------- falk 03-13-01  send in the center of GS
c           temperature from mlonavy for m=1,...,mlonavy
c-----        find mlonavy a first point to east from alonavy
      do m=1,mrgs
       if(xlongs(m).gt.alonavy) then
        mlonavy=m
        go to 1002
       end if
      end do
 1002 continue
c---------------
c---------------  falk 08-15-05 do not use t12N, t12S
      go to 299
c---------------
      print *,' alonavy/cos30=',alonavy/cos30
      print *,'mlonavy a first point to east from alonavy=',mlonavy
      print *,'the first level sent from mlonavy lu=',lu
      print *,'  the last level in point mlonavy ld=',ld
c---------------
c---------------  falk 08-12-05 check closest points to
c---------------                tgs at lgs along traj  m=1,mrgs
      print *,'traj: m,n12min,tt(n12min),tt(nc) at 0,100,200,400m'
      do m=1,mrgs
       d12min=1000.
       do n=1,nngs
        if(abs(tt(m,n,lgs)-tgs).lt.d12min) then
         d12min=abs(tt(m,n,lgs)-tgs)
         n12min=n
        end if
       end do
       n=n12min
       write(6,710) m,n12min,tt(m,n,1),tt(m,nc,1),tt(m,n,7),
     *  tt(m,nc,7),tt(m,n,10),tt(m,nc,10),tt(m,n,13),tt(m,nc,13)
 710   format(2i7,4(2f7.2,1x))
      end do
      print *,'traj: m,n12min,tt(n12min)-tt(nc) at 0,100,200,400,'
      print *,'                                    600,800,1000,1500m'
      do m=1,mrgs
       d12min=1000.
       do n=1,nngs
        if(abs(tt(m,n,lgs)-tgs).lt.d12min) then
         d12min=abs(tt(m,n,lgs)-tgs)
         n12min=n
        end if
       end do
       n=n12min
       aa1=tt(m,n,1)-tt(m,nc,1)
       aa2=tt(m,n,7)-tt(m,nc,7)
       aa3=tt(m,n,10)-tt(m,nc,10)
       aa4=tt(m,n,13)-tt(m,nc,13)
       aa5=tt(m,n,15)-tt(m,nc,15)
       aa6=tt(m,n,17)-tt(m,nc,17)
       aa7=tt(m,n,19)-tt(m,nc,19)
       aa8=tt(m,n,20)-tt(m,nc,20)
c      write(6,710) m,n12min,tt(m,n,15),tt(m,nc,15),tt(m,n,17),
c    *  tt(m,nc,17),tt(m,n,19),tt(m,nc,19),tt(m,n,24),tt(m,nc,24)
       write(6,710) m,n12min,aa1,aa2,aa3,aa4,aa5,aa6,aa7,aa8
      end do
c--------------- end changes
c-------------  send to the center of GS t=12 at z=400
c-------------  find t12 beginning from m=mrgs
c
c-------------- 04-07-03 find m12N where t=tgs between n=1 and n=nngs
      do m=mrgs,mlonavy,-1
       do n=2,nngs
        if(tt(m,n-1,lgs).ge.tgs.and.tt(m,n,lgs).lt.tgs) then
         adif=tt(m,n-1,lgs)-tt(m,n,lgs)
         if(adif.lt.0.01) then
          alint=1.
         else
          alint=(tt(m,n-1,lgs)-tgs)/adif
         end if
         do l=1,nl
          t12NB(l)=alint*tt(m,n,l)+(1.-alint)*tt(m,n-1,l)
         end do
         m12N=m
         go to 200
        end if
       end do
      end do
 200  continue
c----------- 04-07-03 send t12NB(l) to current center for m>m12N
      do m=m12N,mrgs
       do l=lu,ld
        tt(m,nc,l)=t12NB(l)
       end do
      end do
c-----------
      print *,' GS: t12NB(l) l=1,nl'
      write(6,103) t12NB
c-----------  send t=tgs for m12N > m >= mlonavy
      do m=m12N-1,mlonavy,-1
       do n=2,nngs
        if(tt(m,n-1,lgs).ge.tgs.and.tt(m,n,lgs).lt.tgs) then
         adif=tt(m,n-1,lgs)-tt(m,n,lgs)
         if(adif.lt.0.01) then
          alint=1.
         else
          alint=(tt(m,n-1,lgs)-tgs)/adif
         end if
         do l=lu,ld
          tt(m,nc,l)=alint*tt(m,n,l)+(1.-alint)*tt(m,n-1,l)
         end do
         go to 250
        end if
       end do
c-------------- 01-31-03 if there is no t=tgs, send it from m+1
       do l=lu,ld
        tt(m,nc,l)=tt(m+1,nc,l)
       end do
 250   continue
      end do
c
c------------- send t12N from mlonavy
c
      do l=1,nl
       t12N(l)=tt(mlonavy,nc,l)
      end do
c--------------
      print *,' GS: t12N(l) l=1,nl'
      write(6,103) t12N
      print *,' GS: t12S(l) l=1,nl calculated in BAY'
      print *,' t12S calculated in BAY for GS'
      write(6,103) t12S
c--------------
c------- 01-31-03 send linear interpolated t12N and t12S 
c
      do m=1,mlonavy
       alint=float(m-1)/float(mlonavy-1)
       do l=lu,ld
        tt(m,nc,l)=alint*t12N(l)+(1.-alint)*t12S(l)
       end do
      end do
c----------------
      print *,' GS  after send t12N'
c---------------  falk 08-15-05  use aladj for all traj
 299  continue
c--------------
      print *,' GS  before correction with aladj'
      print *,'l=7(100m), l=10(200), l=13(400), l=15(600)' 
      print *,'                      l=17(800), l=19(1000)' 
      print *,'lon/cos30, lat, tt at  l=7 l=10 l=13 l=15 l=17 l=19'
c--------------
      do m=1,mrgs,5
       write(6,221) m,xlon2(m,nc)/cos30,ylat2(m,nc),tt(m,nc,7),
     *       tt(m,nc,10),tt(m,nc,13),tt(m,nc,15),tt(m,nc,17),
     *       tt(m,nc,19)
 221   format(1x,i7,2f7.2,2x,6f7.2)
      end do
c--------------
      do m=1,mrgs
       dtt=tt(m,nc,lgs)-tgs
       do l=1,nl
        tt(m,nc,l)=tt(m,nc,l)-aladj(l)*dtt
       end do
      end do
c--------------
c     print *,' l=10(200m), l=13(400m),l=19(1000m)' 
c     print *,'tt  lon/cos30  lat  l=1 l=10 l=13 l=19 l=nl'
c     do m=1,mrgs,5
c      write(6,221) m,xlon2(m,nc)/cos30,ylat2(m,nc),tt(m,nc,1),
c    *       tt(m,nc,10),tt(m,nc,13),tt(m,nc,19),tt(m,nc,nl)
c221   format(1x,i7,2f8.2,5f7.2)
c     end do
c--------------
      print *,' GS  after correction with aladj'
      print *,'l=7(100m), l=10(200), l=13(400), l=15(600)' 
      print *,'                      l=17(800), l=19(1000)' 
      print *,'lon/cos30, lat, tt at  l=7 l=10 l=13 l=15 l=17 l=19'
c--------------
      do m=1,mrgs,5
       write(6,221) m,xlon2(m,nc)/cos30,ylat2(m,nc),tt(m,nc,7),
     *       tt(m,nc,10),tt(m,nc,13),tt(m,nc,15),tt(m,nc,17),
     *       tt(m,nc,19)
      end do
c--------------
c------------- 06-17-04 put ntrj=5 for GS before to use it
       ntrj=5
c-------------
       if(ntrj.ne.1) then
        print *,' GS use tdif from BAY: mrgs,nl,ntrj=',mrgs,nl,ntrj
        print *,' (tdifbs(l),l=1,nl) at the last point mrgs, l=1,nl'
        write(6,103) (tdifbs(l),l=1,nl) 
        print *,' (tdifes(l),l=1,nl) at the last point mrgs, l=1,nl'
        write(6,103) (tdifes(l),l=1,nl) 
       end if
c------------- send nbndN and nbndS to create asymmetric current
c------------- 02-02-05 do not use asymmetric current
c------------- 02-03-05 combine symmetric and asymmetric current
       ylatb=33.
       ylate=35.
       do m=1,mrgs
        if(ylat2(m,nc).lt.ylatb) masymb=m
        if(ylat2(m,nc).lt.ylate) masyme=m
       end do
       print *,' masymb,ylatb,ylat2(masymb,nc)=',
     *           masymb,ylatb,ylat2(masymb,nc)
       print *,' masyme,ylate,ylat2(masyme,nc)=',
     *           masyme,ylate,ylat2(masyme,nc)
       do m=1,mrgs
        nbndN(m)=1
c       nbndS(m)=nngs-10
        if(m.lt.masymb) nbndS(m)=nngs
        if(m.ge.masymb.and.m.lt.masyme) 
     *   nbndS(m)=nngs-INT(10*(m-masymb)/(masyme-masymb))
        if(m.ge.masyme) nbndS(m)=nngs-10
       end do
c--------- 05-17-05 decrease current in GS near east bnd
c--------- 11-18-05 do not decrease current in GS near east bnd
       go to 333
       mdec=mrgs-50
       ndec=10
       nbndSe=nbndS(mdec)
       do m=mdec,mrgs
        nbndN(m)=1+INT((nc-1-ndec)*(m-mdec)/(mrgs-mdec))
        nbndS(m)=nbndSe+INT((nc+ndec-nbndSe)*(m-mdec)/(mrgs-mdec))
       end do
       print *, 'mdec,ndec,nbndN(mdec),nbndN(mrgs)=',
     *           mdec,ndec,nbndN(mdec),nbndN(mrgs)
       print *, 'mdec,ndec,nbndS(mdec),nbndS(mrgs)=',
     *           mdec,ndec,nbndS(mdec),nbndS(mrgs)
       print *,'nbndS(masymb),nbndS(masyme),nbndS(mrgs)=',
     *          nbndS(masymb),nbndS(masyme),nbndS(mrgs)
c--------- 11-18-05 do not decrease current in GS near east bnd
 333   continue
c---------
       print *,'(nbndS(m),m=masymb-5,masyme+5)'
       write(6,222) (nbndS(m),m=masymb-5,masyme+5)
 222   format(10i7)
c---------------- falk 10-12-05 use nsm=8, msm=5
       nsm=8
c      msm=5
c      nsm=15
c---------------- falk 10-18-05 use  msm=10
       msm=10
       mpr=21
c
       print *,'  use in GS changetcur2'
c-------- send tt=-9. for HPE < zlev(l)
c-------- 08-31-00 save data for the first level under bottom
c-------- to use Frolov's vertical interpolation to sigma-levels
c
       print *,'before sending -9.'
       print *,'mrgs,nngs,nl=',mrgs,nngs,nl
       print *,'zlev(nl)  nl=',nl
       write(6,102) zlev
c----------------
       do m=1,mrgs
       do n=1,nngs
        do l=1,nl-1
         if(zlev(l).gt.HPE(m,n)) then
          tt(m,n,l+1)=-9.
          ss(m,n,l+1)=-9.
         end if
        end do
c
        if(HPE(m,n).eq.1.) then
         do l=1,nl
          tt(m,n,l)=-9.
          ss(m,n,l)=-9.
         end do
        end if
c
       end do
       end do
c
       call parsm(xlon2,ylat2,tt,mmx,nngs,mrgs,nl,'tt',
     *                    nbndN,nbndS,lu,nsm,msm)
c------------- 04-08-03 use changetcur2 instead of sharpgs
c
       call changetcur2(tt,tm,nbndN,nbndS,mmx,mrgs,nngs,nl,
     *                 lu,ld,ncd,ntrj,mpr)
c
       call parsm(xlon2,ylat2,tm,mmx,nngs,mrgs,nl,'tm',
     *                    nbndN,nbndS,lu,nsm,msm)
c
       call stabil(tm,Zlev,mmx,mrgs,nngs,nl)
c
c--------- 02-10-03 use new subr to charp salinity in interval
c---------              nc-ncd <= n <= nc+ncd
       call changesal(ss,sm,mmx,nngs,nl,mrgs,nc,ncd,ld)
c---------------
       lpr=13
       print *,' tm in GS after parsm and stabil'
       print *,'(tm(m,n,lpr),n=nc-5,nc+5) ntrj,mrgs,nc,lpr=',
     *          ntrj,mrgs,nc,lpr
       write(6,109) (n,n=nc-5,nc+5)
  109  format(5x,11i7)
       do m=1,mrgs,10
        write(6,110) m,(tm(m,n,lpr),n=nc-5,nc+5)
  110   format(i7,11f7.2)
       end do
c---------------
c--------------- 02-01-05 check noise in GS near 28-31N at 75m
       lpr=6
       print *,' tt in GS at 28-31N m=58,88 lev=6 (75m)'
       print *,'ylat2(58,nc),ylat2(88,nc)=',ylat2(58,nc),ylat2(88,nc)
       print *,'(tt(m,n,lpr),n=nc+5,nc+15) ntrj,mrgs,nc,lpr=',
     *          ntrj,mrgs,nc,lpr
       write(6,109) (n,n=nc+5,nc+15)
       do m=58,88
        write(6,110) m,(tt(m,n,lpr),n=nc+5,nc+15)
       end do
c
       print *,' tm in GS at 28-31N m=58,88 lev=6 (75m)'
       print *,'ylat2(58,nc),ylat2(88,nc)=',ylat2(58,nc),ylat2(88,nc)
       print *,'(tm(m,n,lpr),n=nc+5,nc+15) ntrj,mrgs,nc,lpr=',
     *          ntrj,mrgs,nc,lpr
       write(6,109) (n,n=nc+5,nc+15)
       do m=58,88
        write(6,110) m,(tm(m,n,lpr),n=nc+5,nc+15)
       end do
c-------------
       print *,'(tt(m,n,lpr),n=nngs-10,nngs) ntrj,mrgs,nc,lpr=',
     *          ntrj,mrgs,nc,lpr
       write(6,109) (n,n=nngs-10,nngs)
       do m=58,88
        write(6,110) m,(tt(m,n,lpr),n=nngs-10,nngs)
       end do
c
       print *,'(tm(m,n,lpr),n=nngs-10,nngs) ntrj,mrgs,nc,lpr=',
     *          ntrj,mrgs,nc,lpr
       write(6,109) (n,n=nngs-10,nngs)
       do m=58,88
        write(6,110) m,(tm(m,n,lpr),n=nngs-10,nngs)
       end do
c---------
       print *,' tt in GS at 26-27N m=40,55 lev=6 (75m)'
       print *,'ylat2(40,nc),ylat2(55,nc)=',ylat2(40,nc),ylat2(55,nc)
       print *,'(tt(m,n,lpr),n=1,11) ntrj,mrgs,nc,lpr=',
     *          ntrj,mrgs,nc,lpr
       write(6,109) (n,n=1,11)
       do m=40,55
        write(6,110) m,(tt(m,n,lpr),n=1,11)
       end do
c
       print *,'(tm(m,n,lpr),n=1,11) ntrj,mrgs,nc,lpr=',
     *          ntrj,mrgs,nc,lpr
       write(6,109) (n,n=1,11)
       do m=40,55
        write(6,110) m,(tm(m,n,lpr),n=1,11)
       end do
c-------------
       print *,'(tt(m,n,lpr),n=11,21) ntrj,mrgs,nc,lpr=',
     *          ntrj,mrgs,nc,lpr
       write(6,109) (n,n=11,21)
       do m=40,55
        write(6,110) m,(tt(m,n,lpr),n=11,21)
       end do
c
       print *,'(tm(m,n,lpr),n=11,21) ntrj,mrgs,nc,lpr=',
     *          ntrj,mrgs,nc,lpr
       write(6,109) (n,n=11,21)
       do m=40,55
        write(6,110) m,(tm(m,n,lpr),n=11,21)
       end do
c--------------- 02-02-05 check noise in the beginning of GS 
       lpr=6
       print *,'check noise in the beginning of GS'
       print *,' tt in GS at 82-83W m=1,30 lev=6 (75m)'
       print *,'ylat2(1,nc),ylat2(30,nc)=',ylat2(1,nc),ylat2(30,nc)
       print *,'(tt(m,n,lpr),n=nc+5,nc+15) ntrj,mrgs,nc,lpr=',
     *          ntrj,mrgs,nc,lpr
       write(6,109) (n,n=nc+5,nc+15)
       do m=1,30
        write(6,110) m,(tt(m,n,lpr),n=nc+5,nc+15)
       end do
c
       print *,' tm in GS at 82-83W m=1,30 lev=6 (75m)'
       print *,'ylat2(1,nc),ylat2(30,nc)=',ylat2(1,nc),ylat2(30,nc)
       print *,'(tm(m,n,lpr),n=nc+5,nc+15) ntrj,mrgs,nc,lpr=',
     *          ntrj,mrgs,nc,lpr
       write(6,109) (n,n=nc+5,nc+15)
       do m=1,30
        write(6,110) m,(tm(m,n,lpr),n=nc+5,nc+15)
       end do
c-------------
       print *,'(tt(m,n,lpr),n=nngs-10,nngs) ntrj,mrgs,nc,lpr=',
     *          ntrj,mrgs,nc,lpr
       write(6,109) (n,n=nngs-10,nngs)
       do m=1,30
        write(6,110) m,(tt(m,n,lpr),n=nngs-10,nngs)
       end do
c
       print *,'(tm(m,n,lpr),n=nngs-10,nngs) ntrj,mrgs,nc,lpr=',
     *          ntrj,mrgs,nc,lpr
       write(6,109) (n,n=nngs-10,nngs)
       do m=1,30
        write(6,110) m,(tm(m,n,lpr),n=nngs-10,nngs)
       end do
c-------------
c---------
c--------- interpolate tm from perp lines to t (model grid)
c
       migw=0
       mige=1
       mb=141
       me=151
       call tootis(t,tm,im,jm,xlon2,ylat2,mmx,nngs,nl,lu,
     *     dgs,LONGMIN,LATMIN,dlon,dlat,mrgs,mb,me,nc,xout,cos30,
     *        migw,mige,nbndN,nbndS)
c
c------------- 04-10-03 change salinity
c
       call tootis(s,sm,im,jm,xlon2,ylat2,mmx,nngs,nl,lu,
     *     dgs,LONGMIN,LATMIN,dlon,dlat,mrgs,mb,me,nc,xout,cos30,
     *        migw,mige,nbndN,nbndS)
c
c--------------- send -9 to t after tootis as in subsharpgs.f
c
       do i=1,im
       do j=1,jm
        do l=1,nl-1
         if(zlev(l).gt.H(i,j)) then
          t(i,j,l+1)=-9.
          s(i,j,l+1)=-9.
         end if
        end do
        if(H(i,j).eq.1.) then
         do l=1,nl
          t(i,j,l)=-9.
          s(i,j,l)=-9.
         end do
        end if
       end do
       end do
c
c
c------------- 04-08-03 end changes
c
c---------------   write GS path
c
c     rewind(64)
c     write(64,140) mrgs
 140  format(i7)
c     write(64,141) (xlongs(m),m=1,mrgs)
c     write(64,141) (ylatgs(m),m=1,mrgs)
c     write(64,141) ((xlon2(m,n),m=1,mrgs),n=1,nngs)
c     write(64,141) ((ylat2(m,n),m=1,mrgs),n=1,nngs)
c     write(64,142) ((HPE(m,n),m=1,mrgs),n=1,nngs)
 141  format(10f7.3)
 142  format(10f7.1)
c
c--------- recover LONGMIN, LONGMAX, XI
c
      LONGMIN=LONGMIN/cos30
      LONGMAX=LONGMAX/cos30
      do i=1,IM
       XI(i)=XI(i)/cos30
      end do
c
c--------------- 04-08-03 do not use sharpgs
      go to 1200
C This code has been bipassed, that is why the call to sharpgs at line 
C 2683 is commented out.
c
c------------------- prescribe cincr for GS path
c
c----- 02-04-03 increase and decrease intensity of GS 
c----- 03-21-03 decrease intensity of GS cincr1 from 1.1 to 1.0
      cincr1=1.0
      cincr2=0.7
      cincrlat1=32.
      cincrlat2=39.
      cincrlon1=0.
      cincrlon2=0.
      migw=0
      mige=1
      icurr=1
c
c-- use in sharpgs: icurr=1 for GS, 2 for TAIL, 3 for SGYRE, 4 for BAY
c
c----------
c
c
      print *,'GS: t before sharp'
c
c     call pritem3(t,im,jm,nl,96,1,11,80,120,'t l=13 z=400  ',-1,13)
c
      call pritem3c(t,XI,YI,im,jm,nl,-80.5,1,23.0,30.0,
     *      't l=13 z=400  ',-1,13,cos30,-1)
      call pritem3c(t,XI,YI,im,jm,nl,-60.0,1,38.0,41.0,
     *      't l=13 z=400  ',-1,13,cos30,-1)
c Call to sharpgs no longer used
C     call sharpgs(t,s,XI,YI,Zlev,H,FSM,mrgs,xlongs,ylatgs,
C    *                   xlon2,ylat2,HPE,tt,ss,
C    *                   cincr1,cincr2,cincrlat1,cincrlat2,
C    *                   cincrlon1,cincrlon2,migw,mige,icurr)
c
      print *,'GS: t after sharp'
 1200 continue
      print *,'GS: t after changetcur2'
c
      call pritem3c(t,XI,YI,im,jm,nl,-80.5,1,23.0,30.0,
     *      't l=13 z=400  ',-1,13,cos30,-1)
      call pritem3c(t,XI,YI,im,jm,nl,-60.0,1,38.0,41.0,
     *      't l=13 z=400  ',-1,13,cos30,-1)
c
c     call pritem3(t,im,jm,nl,96,1,11,80,120,'t l=13 z=400  ',-1,13)
c
      print *,  '  ----------    END pathGS ----------'
c
      return
      end
c
c-----------------
c
      subroutine findj(YI,jm,y,j)
c-------- find the closest j for y
      dimension YI(jm)
      dlat=(YI(jm)-YI(1))/(jm-1)
      j=(y-YI(1))/dlat+1.0001
      if(j.ge.jm) then
       print *,'findj: j>=jm send j=jm'
       j=jm
       go to 100
      end if
      if(j.lt.1) then
       print *,'findj: j<1 send j=1'
       j=1
       go to 100
      end if
      if(abs(y-YI(j)).gt.abs(y-YI(j+1))) j=j+1
 100  continue
      write(6,101) y,j
 101  format('findj: y,j',f7.2,i7)
      return
      end
c
c------------
c
      subroutine findi(XI,im,x,i)
c-------- find the closest i for x
      dimension XI(im)
      dlon=(XI(im)-XI(1))/float(im-1)
      i=(x-XI(1))/dlon+1.0001
      if(i.ge.im) then
       print *,'findi: i>=im send i=im'
       i=im
       go to 100
      end if
      if(i.lt.1) then
       print *,'findi: i<1 send i=1'
       i=1
       go to 100
      end if
      if(abs(x-XI(i)).gt.abs(x-XI(i+1))) i=i+1
 100  continue
      write(6,101) x,i
 101  format('findi: x,i',f7.2,i7)
      return
      end
c
c------------
c
       subroutine pritop(H,XI,YI,im,jm,ibeg,iend,j1,j2,char,mig)
       dimension H(im,jm),XI(im),YI(jm),jH(11)
       character*25 char
        write(6,100) char
 100    format(/10x,a25)
        write(6,105) im,jm,ibeg,iend,j1,j2
 105    format(/1x,' PRITOP',/5x,'im,jm,ibeg,iend,j1,j2=',
     *         /5x,6i6)
        write(6,103) (i,i=ibeg,iend)
        write(6,102) (XI(i),i=ibeg,iend)
c102    format(/14x,11f7.2,/)
 102    format(/14x,11f6.2,/)
        do j=j1,j2
         if(mig.eq.-1) then
          jc=j2-j+j1
         else
          jc=j
         endif
         do i=ibeg,iend
          i1=i-ibeg+1
          jH(i1)=H(i,jc)
         end do
         ii=iend-ibeg+1
c        write(6,101) jc,YI(jc),(H(i,jc),i=ibeg,iend)
         write(6,101) jc,YI(jc),(jH(i),i=1,ii)
        end do
c 101   format(i7,f7.2,11f7.0)
  101   format(i7,f7.2,11i6)
c 103   format(14x,11i7)
  103   format(14x,11i6)
        return
        end
c
c-----------------
c
      subroutine checkGDEM(t,FSM,H,Zlev,char)
      parameter(im=254,jm=225,nl=33)
      dimension t(im,jm,nl),FSM(im,jm),H(im,jm),Zlev(nl)
      dimension difmaxt(nl),idifmaxt(nl),jdifmaxt(nl)
      character*4 char
      print *,' checkGDEM char=',char
      do l=1,nl
       difmaxt(l)=0.
       do j=2,jm-1
        do i=2,im-1
         if(FSM(i,j).gt.0.5.and.Zlev(l).le.H(i,j)) then
          a1=0.
          a2=0.
          a3=0.
          a4=0.
          if(FSM(i+1,j).gt.0.5.and.Zlev(l).le.H(i+1,j))
     *                      a1=abs(t(i+1,j,l)-t(i,j,l))
          if(FSM(i,j+1).gt.0.5.and.Zlev(l).le.H(i,j+1))
     *                      a2=abs(t(i,j+1,l)-t(i,j,l))
          if(FSM(i-1,j).gt.0.5.and.Zlev(l).le.H(i-1,j))
     *                      a3=abs(t(i-1,j,l)-t(i,j,l))
          if(FSM(i,j-1).gt.0.5.and.Zlev(l).le.H(i,j-1))
     8                      a4=abs(t(i,j-1,l)-t(i,j,l))
          dift=max(a1,a2,a3,a4)
          if(dift.gt.difmaxt(l)) then
           difmaxt(l)=dift
           idifmaxt(l)=i
           jdifmaxt(l)=j
          end if
         end if
        end do
       end do
      end do
c
c------------- 
c
      print *,'l,idifmaxt(l),jdifmaxt(l),difmaxt(l)'
      print *,'           t(idifmaxt(l),jdifmaxt(l),l)'
      do l=1,nl
       write(6,201) l,idifmaxt(l),jdifmaxt(l),difmaxt(l),
     *              t(idifmaxt(l),jdifmaxt(l),l)
      end do
 201  format(i7,2x,2i7,2x,f7.2,2x,f7.2)
c      
      return
      end
c
      subroutine readclgs(xgsclim,ygsclim,iiclim,step,
     *                          moutspng,moutcnst,yout)
      parameter(mmx=500,kk=83,k50=76)
      dimension xgsclim(mmx),ygsclim(mmx),xlon(kk),ylat(kk)
      character*55 FTCLIM
      FTCLIM='/nfsuser/g01/wx20af/gfdl/gradu/data/gspath.09'
c     OPEN(50,file=FTCLIM,form='formatted')
c
      print *,'readclgs: kk,k50,moutspng,moutcnst,yout=',
     *                   kk,k50,moutspng,moutcnst,yout
      PI=3.1415927
      cos30=cos(PI*30./180.)
c------- read GS climate data from -75 to -50 (76 points)
      rewind(50)
      do k=1,k50
       read(50,101) ylat(k),xlon(k)
      end do
 101  format(2f10.4)
      close(50)
c----------  convert longitudes
      do k=1,k50
       xlon(k)=(-360.+xlon(k))*cos30
      end do
c
c----------- sponge at east bnd  with yout
c
      mb=k50-moutspng-moutcnst
      me=k50-moutcnst
      do m=mb,me
       al=float(m-mb)/float(moutspng)
       ylat(m)=al*yout+(1.-al)*ylat(m)
      end do
      do m=me,k50
       ylat(m)=yout
      end do
c
c---------- interpolate with even step along curve
c---------- 04-07-03 use evennstep not evenstepgs
      iiclim=k50
      do m=1,iiclim
       xgsclim(m)=xlon(m)
       ygsclim(m)=ylat(m)
      end do
      call evenstep(xgsclim,ygsclim,mmx,iiclim,step,cos30)
      return
      end
c
c-----------------
c
      subroutine sklclim(xl1,yl1,xl2,yl2,
     *                  il1,il2,xskl,dskl,cos30)
      parameter(mmx=500)
      dimension xl1(mmx),yl1(mmx),xl2(mmx),yl2(mmx)
      dimension rlat(mmx)
c------- sponge two climate trajectories in an interval
c-------  (xskl-dskl,xskl);   xl2(1) is very close to 
c-------     the right bound. of the interval xskl
c-------      thus only the first trajectory is changed 
c-------       
c------- 
c
      print *,'   sklclim'
      xe=xl2(1)
      xb=xe-dskl
      ye2=yl2(1)
c
      write(6,101) xskl/cos30,dskl/cos30,xb/cos30,xe/cos30
 101  format('sklnew: xskl/cos30,dskl/cos30,xb/cos30,xe/cos30=',4f7.2)
c
c----   find the first and last point for 1 curve in sponge area
c
      do i=1,il1
       if(xl1(i).ge.xb) then
        kb=i
        go to 100
       end if
      end do
 100  continue
      do i=1,il1
       if(xl1(i).gt.xe) then
        ke=i-1
        go to 200
       end if
      end do
 200  continue
c
c------ calculaste y for the first traj in the point xe
      ye1=yl1(ke)+(yl1(ke+1)-yl1(ke))*(xe-xl1(ke))/(xl1(ke+1)-xl1(ke))
c
      if(xl1(il1).le.xe) ke=il1
c--------- check if the distance xl2(1)-xl1(ke) is not very small
c--------- xe=xl2(1)>xskl>xl1(ke) and xl1(ke)>xl1(ke-1)
      if(abs(xe-xl1(ke)).lt.(xl1(ke)-xl1(ke-1))/4.) ke=ke-1
c
      print *,'      kb,ke,il1,il2=',kb,ke,il1,il2
      write(6,102) xl1(ke)/cos30,xl1(ke+1)/cos30,xe/cos30,xl2(1)/cos30
 102  format('xl1(ke)/cos30,xl1(ke+1)/cos30,xe/cos30,xl2(1)/cos30',
     *        4f7.3)
c
      print *,' old  xl1(i)/cos30, i=kb,ke  '
      write(6,103) (xl1(i)/cos30,i=kb,ke)
      print *,' old  yl1(i),i=kb,ke  '
      write(6,103) (yl1(i),i=kb,ke)
 103  format(10f7.2)
      print *,' old  xl2(i)/cos30, i=1,10  il2=',il2
      write(6,103) (xl2(i)/cos30,i=1,10)
      print *,' old  yl2(i), i=1,10  il2=',il2
      write(6,103) (yl2(i),i=1,10)
c--------------
      dye=ye2-ye1
      do k=kb,ke
       al=(xl1(k)-xl1(kb))/(xe-xl1(kb))
       rlat(k)=yl1(k)+al*dye
      end do
c--------------
      print *,'  sponge results: (rlat(k),k=kb,ke)   kb,ke=',kb,ke
      write(6,103) (rlat(k),k=kb,ke)
c--------------
c------- send united path in xl1,yl1
      do k=kb,ke
       yl1(k)=rlat(k)
      end do
c
c----------- new il1 and united path
c
      il1=ke+il2
      do k=ke+1,il1
       xl1(k)=xl2(k-ke)
       yl1(k)=yl2(k-ke)
      end do
c------------
      print *,' new     xl1(i)/cos30, i=1,il1  il1=',il1
      write(6,103) (xl1(i)/cos30,i=1,il1)
      print *,' new     yl1(i), i=1,il1  il1=',il1
      write(6,103) (yl1(i),i=1,il1)
c------------
      return
      end
c
c--------------
c
c------------- 03-20-03 use for TR4 nngs4=61
c------------- 04-15-03 do not use xlon2D1-ylat2D4,
c-------------          tt,tm,HPE,dgs;
c-------------          use nngs1-nngs4,nngs7
c------------- 04-30-03 use nngs7=51 (it was =61)
c------------- 05-02-03 use nngs7=61 again
c---------------- falk 08-15-05 add file aladj
      subroutine pathCURR(TB1,SB1,XI,YI,Zlev,H,FSM,t,s,
     *                   t12N,t12S,aladj)
      parameter(im=254,jm=225,nl=33,mmx=500,
     *          nngs1=51,nngs2=51,nngs3=51,nngs4=61,nngs7=61,
     *          stepgs=0.1,stepn=0.1)
      real XI(IM),YI(JM)
      DIMENSION H(IM,JM),TB1(IM,JM,nl),SB1(IM,JM,nl),
     *          FSM(IM,JM),Zlev(nl)
      dimension t(im,jm,nl),s(im,jm,nl)
      dimension xlongs1(mmx),ylatgs1(mmx),xlongs2(mmx),ylatgs2(mmx)
      dimension xlongs3(mmx),ylatgs3(mmx),xlongs4(mmx),ylatgs4(mmx)
c------------- 12-31-02 use for TR4 nngs4=71
c------------- 03-20-03 use for TR4 nngs4=61
c
      dimension nbndN1(mmx),nbndS1(mmx),nbndN2(mmx),nbndS2(mmx)
      dimension nbndN3(mmx),nbndS3(mmx),nbndN4(mmx),nbndS4(mmx)
      dimension t12N(nl),t12S(nl)
      real LATMIN,LATMAX,LONGMIN,LONGMAX
      COMMON/sphere/REARTH,LATMIN,LATMAX,LONGMIN,LONGMAX
      CHARACTER*55  FTSGYRE,FTBAY
c---------------- falk 08-15-05 add file aladj
      dimension aladj(nl)
c-------------- 05-18-06 add cold rings
c--------------   lring=1 for warm core, lring=-1 for cold core ring
c--------------   wwr is weight for twc in a warm core ring
c--------------   dtc is a temperature difference in a cold core ring
      common /ringp/ lring,wwr,dtc
c
      FTSGYRE='/nfsuser/g01/wx20af/gfdl/scripts/workl/SGYREu'
c     OPEN(75,FILE=FTSGYRE,form='formatted')
      FTBAY='/nfsuser/g01/wx20af/gfdl/scripts/workl/BAYu'
c     OPEN(55,FILE=FTBAY,form='formatted')
c
      print *,' BEGIN pathCURR: im,jm,nl,mmx=',
     *          im,jm,nl,mmx 
      print *,'nngs1,nngs2,nngs3,nngs4,nngs7=',
     *         nngs1,nngs2,nngs3,nngs4,nngs7
c-----------
      PI=3.1415927
c--------  03-04-02   use for SGYRE average alat=16N
      alat=16.
      cos30=cos(PI*alat/180.)
c-------- for subr. tootis 
      xout=-50.
      mb=1
      me=11
c----------------
      write(6,204) stepgs,stepn,alat,cos30,xout,mb,me
 204  format('stepgs,stepn,alat,cos30,xout,mb,me=',
     *        5f10.4,2i7) 
c----------------
c
      dlon=(LONGMAX-LONGMIN)/float(IM-1)
      dlat=(LATMAX-LATMIN)/float(JM-1)
c
c---------------
      write(6,201) dlon,dlat
 201  format('   dlon,dlat=',2f10.4)
      write(6,202) XI(1),XI(im)
 202  format(' XI(1),XI(im)=',2f10.4)
      write(6,203) YI(1),YI(jm)
 203  format(' YI(1),YI(jm)=',2f10.4)
 102  format(10f7.2)
c---------------- falk 08-15-05 add check aladj
      print *,' pathCURR: aladj(nl)'
      write(6,102) aladj
c---------------
c
c----------- multiply all longitudes on cos30
c
      LONGMIN=LONGMIN*cos30
      LONGMAX=LONGMAX*cos30
      dlon=dlon*cos30
      xout=xout*cos30
      do i=1,IM
       XI(i)=XI(i)*cos30
      end do
c
      read(75,106) mmnew1
      read(75,105) (xlongs1(m),m=1,mmnew1)
      read(75,105) (ylatgs1(m),m=1,mmnew1)
      read(75,107) (nbndN1(m),m=1,mmnew1)
      read(75,107) (nbndS1(m),m=1,mmnew1)
c
      read(75,106) mmnew2
      read(75,105) (xlongs2(m),m=1,mmnew2)
      read(75,105) (ylatgs2(m),m=1,mmnew2)
      read(75,107) (nbndN2(m),m=1,mmnew2)
      read(75,107) (nbndS2(m),m=1,mmnew2)
c
      read(75,106) mmnew3
      read(75,105) (xlongs3(m),m=1,mmnew3)
      read(75,105) (ylatgs3(m),m=1,mmnew3)
      read(75,107) (nbndN3(m),m=1,mmnew3)
      read(75,107) (nbndS3(m),m=1,mmnew3)
c
      read(75,106) mmnew4
      read(75,105) (xlongs4(m),m=1,mmnew4)
      read(75,105) (ylatgs4(m),m=1,mmnew4)
      read(75,107) (nbndN4(m),m=1,mmnew4)
      read(75,107) (nbndS4(m),m=1,mmnew4)
c------------- 12-31-02 use for TR4 nngs4=71
c------------- 01-03-03 use for TR4 nngs4=81
c------------- 03-20-03 use for TR4 nngs4=61
      do m=1,mmnew4
       nbndS4(m)=nbndS4(m)+5
      end do
c
 105  format(10f7.3)
 106  format(i7)
 107  format(10i7)
c
c-------------- 11-28-05 correct nbnd
c-------------- 12-22-05 do not correct nbnd
      go to 200
c------------------     TR1
      print *,'nbndN1 before correction mmnew1=',mmnew1
      call pri10I1(nbndN1,mmx,mmnew1,1,mmnew1,'  nbndN1  ')
      do m=2,mmnew1
       if(nbndN1(m).lt.nbndN1(m-1)) nbndN1(m)=nbndN1(m-1)
      end do
      print *,'nbndN1 after correction mmnew1=',mmnew1
      call pri10I1(nbndN1,mmx,mmnew1,1,mmnew1,'  nbndN1  ')
c---------
      print *,'nbndS1 before correction mmnew1=',mmnew1
      call pri10I1(nbndS1,mmx,mmnew1,1,mmnew1,'  nbndS1  ')
      do m=2,mmnew1
       if(nbndS1(m).gt.nbndS1(m-1)) nbndS1(m)=nbndS1(m-1)
      end do
      print *,'nbndS1 after correction mmnew1=',mmnew1
      call pri10I1(nbndS1,mmx,mmnew1,1,mmnew1,'  nbndS1  ')
c------------------     TR2
      print *,'nbndN2 before correction mmnew2=',mmnew2
      call pri10I1(nbndN2,mmx,mmnew2,1,mmnew2,'  nbndN2  ')
      do m=2,mmnew2
       if(nbndN2(m-1).ne.(nngs2+1)/2) then
        if(nbndN2(m).lt.nbndN2(m-1)) nbndN2(m)=nbndN2(m-1)
       end if
      end do
      print *,'nbndN2 after correction mmnew2=',mmnew2
      call pri10I1(nbndN2,mmx,mmnew2,1,mmnew2,'  nbndN2  ')
c---------
      print *,'nbndS2 before correction mmnew2=',mmnew2
      call pri10I1(nbndS2,mmx,mmnew2,1,mmnew2,'  nbndS2  ')
      do m=2,mmnew2
       if(nbndS2(m-1).ne.(nngs2+1)/2) then
        if(nbndS2(m).gt.nbndS2(m-1)) nbndS2(m)=nbndS2(m-1)
       else
        if(nbndS2(m).gt.nbndS2(m-1)) nbndS2(m)=nbndS1(mmnew1)
       end if
      end do
      print *,'nbndS2 after correction mmnew2=',mmnew2
      call pri10I1(nbndS2,mmx,mmnew2,1,mmnew2,'  nbndS2  ')
c------------------     TR3
      print *,'nbndN3 before correction mmnew3=',mmnew3
      call pri10I1(nbndN3,mmx,mmnew3,1,mmnew3,'  nbndN3  ')
      do m=2,mmnew3
       if(nbndN3(m).lt.nbndN3(m-1)) nbndN3(m)=nbndN3(m-1)
      end do
      print *,'nbndN3 after correction mmnew3=',mmnew3
      call pri10I1(nbndN3,mmx,mmnew3,1,mmnew3,'  nbndN3  ')
c---------
      print *,'nbndS3 before correction mmnew3=',mmnew3
      call pri10I1(nbndS3,mmx,mmnew3,1,mmnew3,'  nbndS3  ')
      do m=2,mmnew3
       if(nbndS3(m).gt.nbndS3(m-1)) nbndS3(m)=nbndS3(m-1)
      end do
      print *,'nbndS3 after correction mmnew3=',mmnew3
      call pri10I1(nbndS3,mmx,mmnew3,1,mmnew3,'  nbndS3  ')
c------------------     TR4
      print *,'nbndS4 before correction mmnew4=',mmnew4
      call pri10I1(nbndS4,mmx,mmnew4,1,mmnew4,'  nbndS4  ')
      do m=2,mmnew4
       if(nbndS4(m).gt.nbndS4(m-1)) nbndS4(m)=nbndS4(m-1)
      end do
      print *,'nbndS4 after correction mmnew4=',mmnew4
      call pri10I1(nbndS4,mmx,mmnew4,1,mmnew4,'  nbndS4  ')
c---------
c--------- 11-28-05  end correction 
 200  continue
c--------------
      print *,' mmnew1,mmnew2,mmnew3,mmnew4=',
     *          mmnew1,mmnew2,mmnew3,mmnew4
c
      print *,' process TR1'
      migw=1
      mige=0
      mrgs=mmnew1
      ntrj=1
      mpr=161
c---------- 12-31-02 use nsm=6 as before
c---------------- falk 10-12-05 use nsm=8, msm=5
      nsm=8
c     msm=5
c---------------- falk 10-18-05 use  msm=10
      msm=10
c     nsm=6
c     nsm=15
c---------- 03-20-03 include msm in proctr as parameter
c
c---------------- falk 08-15-05 add file aladj
      call proctr(TB1,SB1,XI,YI,Zlev,H,FSM,t,s,im,jm,nl,
     *   xlongs1,ylatgs1,'TR1       ',
     *   nbndN1,nbndS1,t12N,t12S,aladj,
     *   mmx,nngs1,mrgs,stepgs,stepn,cos30,migw,mige,
     *   dlon,dlat,mb,me,xout,ntrj,mpr,nsm,msm)
c
      print *,' process TR2'
      migw=1
      mige=0
      mrgs=mmnew2
      ntrj=2
      mpr=184
c---------- 12-31-02 use nsm=6 as before
c     nsm=6
c---------------- falk 10-12-05 use nsm=8, msm=5
      nsm=8
c     msm=5
c---------------- falk 10-18-05 use  msm=10
      msm=10
c     nsm=15
c---------- 03-20-03 include msm in proctr as parameter
c
c---------------- falk 08-15-05 add file aladj
      call proctr(TB1,SB1,XI,YI,Zlev,H,FSM,t,s,im,jm,nl,
     *   xlongs2,ylatgs2,'TR2       ',
     *   nbndN2,nbndS2,t12N,t12S,aladj,
     *   mmx,nngs2,mrgs,stepgs,stepn,cos30,migw,mige,
     *   dlon,dlat,mb,me,xout,ntrj,mpr,nsm,msm)
c
      print *,' process TR3'
      migw=0
      mige=0
      mrgs=mmnew3
      ntrj=3
      mpr=111
c---------- 12-31-02 use nsm=6 as before
c---------------- falk 10-12-05 use nsm=8, msm=5
      nsm=8
c---------------- falk 10-18-05 use  msm=15
c---------------- falk 10-19-05 use  msm=20
      msm=20
c
c---------------- falk 08-15-05 add file aladj
      call proctr(TB1,SB1,XI,YI,Zlev,H,FSM,t,s,im,jm,nl,
     *   xlongs3,ylatgs3,'TR3       ',
     *   nbndN3,nbndS3,t12N,t12S,aladj,
     *   mmx,nngs3,mrgs,stepgs,stepn,cos30,migw,mige,
     *   dlon,dlat,mb,me,xout,ntrj,mpr,nsm,msm)
c
      print *,' process TR4'
      migw=0
      mige=0
      mrgs=mmnew4
      ntrj=4
c     mpr=201
      mpr=181
c---------------- falk 10-18-05 use nsm=10 msm=15
      nsm=10
c---------------- falk 10-19-05 use nsm=10 msm=20
      msm=20
c------------- 12-31-02 use for TR4 nngs4=71 and nc4=36,
c-------------           tt4,tm4,HPE4
c------------- 03-20-03 use for TR4 nngs4=61 and nc4=31
c---------------- falk 08-15-05 add file aladj
      call proctr(TB1,SB1,XI,YI,Zlev,H,FSM,t,s,im,jm,nl,
     *   xlongs4,ylatgs4,'TR4       ',
     *   nbndN4,nbndS4,t12N,t12S,aladj,
     *   mmx,nngs4,mrgs,stepgs,stepn,cos30,migw,mige,
     *   dlon,dlat,mb,me,xout,ntrj,mpr,nsm,msm)
c
c----------  process BAY
c
      cos30p=cos30
c--------    use for BAY average alat=20N
      alat=20.
      cos30=cos(PI*alat/180.)
c------- recalculate for a new alat
      LONGMIN=LONGMIN*cos30/cos30p
      LONGMAX=LONGMAX*cos30/cos30p
      dlon=dlon*cos30/cos30p
      xout=xout*cos30/cos30p
      do i=1,IM
       XI(i)=XI(i)*cos30/cos30p
      end do
c
      read(55,106) mmnew1
      read(55,105) (xlongs1(m),m=1,mmnew1)
      read(55,105) (ylatgs1(m),m=1,mmnew1)
      read(55,107) (nbndN1(m),m=1,mmnew1)
c
      print *,'  mmnew for BAY=',mmnew1
c----------  for BAY send to south bnd nngs
      do m=1,mmnew1
       nbndS1(m)=nngs
      end do
c
      print *,' process BAY'
      migw=0
      mige=0
      mrgs=mmnew1
      ntrj=7
      mpr=154
c     nsm=6
c     nsm=15
c--------------- 04-02-03 use new subr BAYtr 
c--------------- 05-05-03 use ynp=25.0 instead of 26.4
c--------------- 05-05-03a use ynp=27.5 instead of 26.4
c--------------- 05-15-03 use again ynp=26.4
c     ynp=27.5
c--------------- 06-17-04 read ynp from fort.30
c     read(31,601) ynp
 601  format(f7.1)
c     write(6,602) ynp
 602  format(' read ynp=',f7.2)
c---------------
c     ynp=26.4
      call BAYtr(xlongs1,ylatgs1,mmx,mrgs,stepgs,stepn,ynp,cos30,
     *                 nbndN1,nbndS1,nngs7,migw,mige)
c
c---------- 03-20-03 include msm in proctr as parameter
c     msm=10
c---------------- falk 10-12-05 use nsm=8, msm=5
      nsm=8
c---------------- falk 10-18-05 use  msm=10
c     msm=5
      msm=10
c---------------- falk 08-15-05 add file aladj
      call proctr(TB1,SB1,XI,YI,Zlev,H,FSM,t,s,im,jm,nl,
     *   xlongs1,ylatgs1,'BAY       ',
     *   nbndN1,nbndS1,t12N,t12S,aladj,
     *   mmx,nngs7,mrgs,stepgs,stepn,cos30,migw,mige,
     *   dlon,dlat,mb,me,xout,ntrj,mpr,nsm,msm)
c----------------
c---------------- falk 02-06-06 add ring
c
      read(32,701) iring
 701  format(i2)
c----------------
c---------------- 05-17-06 use several warm rings
      if(iring.ne.0) then
      do irg=1,iring
c----------------
       ntrj=8
       print *,' process RING ntrj=',ntrj
       migw=0
       mige=0
       nngs8=41
       mpr=40
c---------------- 05-18-06 add cold rings
       read(32,701) lring
       print *,' lring=',lring
       if(lring.eq.1) then
        read(32,702) wwr
        print *,' weight for warm core ring: wwr='
        write(6,702) wwr
       else
        read(32,702) dtc
        print *,' temperature difference for cold core ring: dtc='
        write(6,702) dtc
       end if
 702   format(f3.1)
c----------------
c---------------- 05-17-06 include par. irg into ringtr
       call ringtr(xlongs2,ylatgs2,nbndN2,nbndS2,mmx,nngs8,
     *              mrgs,cos30,stepgs,stepn,irg)
       nsm=5
       msm=10
       call proctr(TB1,SB1,XI,YI,Zlev,H,FSM,t,s,im,jm,nl,
     *   xlongs2,ylatgs2,'RING      ',
     *   nbndN2,nbndS2,t12N,t12S,aladj,
     *   mmx,nngs8,mrgs,stepgs,stepn,cos30,migw,mige,
     *   dlon,dlat,mb,me,xout,ntrj,mpr,nsm,msm)
      end do
      end if
c----------------
c
c--------- recover LONGMIN, LONGMAX, XI
c
      LONGMIN=LONGMIN/cos30
      LONGMAX=LONGMAX/cos30
      do i=1,IM
       XI(i)=XI(i)/cos30
      end do
c-------------------------
      print *,' topogr in BAY'
      call pritop(H,XI,YI,im,jm,77,87,71,91,
     *            '  H(i,j),i=77,87,j=71,91 ',-1)
      call pritop(H,XI,YI,im,jm,66,76,71,91,
     *            '  H(i,j),i=66,76,j=71,91 ',-1)
      call pritop(H,XI,YI,im,jm,61,71,51,81,
     *            '   H(i,j),i=61,71,j=51,81',-1)
      call pritop(H,XI,YI,im,jm,77,87,37,67,
     *            '   H(i,j),i=77,87,j=37,67',-1)
c-------------------------
c     stop
      return
      end
c
c-----------------
c
c------------- 12-26-02 include param nsm for paralgs
c------------- 03-20-03 include param msm for parsm
c---------------- falk 08-15-05 add file aladj
c----------------      12-15-05 use lu=5
      subroutine proctr(TB1,SB1,XI,YI,Zlev,H,FSM,t,s,im,jm,nl,
     *   xlongs,ylatgs,char,
     *   nbndN,nbndS,t12N,t12S,aladj,
     *   mmx,nngs,mrgs,step,stepn,cos30,migw,mige,
     *   dlon,dlat,mb,me,xout,ntrj,mpr,nsm,msm)
c---------01-31-03 include lsend,tsend,lgs,tgs,fiorg for LC
      parameter(lu=5,ld=27,ncd=8,Rmin=1.0,lsend=13,tsend=15.,
     *          lgs=13,tgs=12.,fiorg=19.)
c
      real XI(IM),YI(JM)
      DIMENSION H(IM,JM),TB1(IM,JM,nl),SB1(IM,JM,nl),
     *          FSM(IM,JM),Zlev(nl)
      dimension t(im,jm,nl),s(im,jm,nl),dgs(im,jm)
      dimension xlongs(mmx),ylatgs(mmx)
      dimension xlon2(mmx,nngs),ylat2(mmx,nngs)
      dimension tt(mmx,nngs,nl),tm(mmx,nngs,nl),HPE(mmx,nngs)
      dimension nbndN(mmx),nbndS(mmx)
c------------- 01-28-03 add var and arrays for looptr (BAY)
      real xconw,yconw,xcone,ycone
c------- 01-31-03 include mlow,mup to send t=15 in LC
      integer mlow,mup
c------- 01-31-03 include t12N,t12S 
      dimension t12N(nl),t12S(nl)
      dimension xnew(mmx),ynew(mmx),xbnd(mmx),ybnd(mmx)
      character char*10
      real LATMIN,LATMAX,LONGMIN,LONGMAX
      COMMON/sphere/REARTH,LATMIN,LATMAX,LONGMIN,LONGMAX
c---------------- falk 08-15-05 add file aladj
      dimension aladj(nl)
c
c---------------------
      nc=(nngs+1)/2
      print *,' subr. PROCTR: char=',char
      print *,'im,jm,nl,mmx,nngs,mrgs,mb,me=',
     *         im,jm,nl,mmx,nngs,mrgs,mb,me
      print *,'    nsm=',nsm
      print *,'zlev(nl)  nl=',nl
      write(6,102) zlev
      write(6,301) LATMIN,LATMAX,LONGMIN/cos30,LONGMAX/cos30
 301  format('LATMIN,LATMAX,LONGMIN/cos30,LONGMAX/cos30',4f7.2)
c
      print *,'(nbndN(m),m=1,mrgs)'
      write(6,104) (nbndN(m),m=1,mrgs)
      print *,'(nbndS(m),m=1,mrgs)'
      write(6,104) (nbndS(m),m=1,mrgs)
 104  format(10i7)
c---------------------
c------------- 02-10-06 add output for LC and GS
      if(ntrj.eq.7) then
       print *,'begin proctr: ntrj=',ntrj
       print *,'before sending warm tt and changetcur'
       call pritem3c(t,XI,YI,im,jm,nl,-83.00,1,22.00,25.00,
     *      't l=7  z=100  ',-1,7,cos30,1)
      end if
c------------- 02-08-06 add output for RING
      if(ntrj.eq.8) then
       print *,'before changetcur'
       call pritem3c(t,XI,YI,im,jm,nl,-90.00,1,25.00,27.00,
     *      't l=1  z=0    ',-1,1,cos30,1)
      end if
c
c
      call perp(xlongs,ylatgs,xlon2,ylat2,nngs,stepn,mrgs,mmx,
     *                cos30,migw,mige)
c
c--------------------
      print *,' after perp for char, mrgs=',char,mrgs
c     print *,'(ylat2(m,nngs),m=1,mrgs)'
c     write(6,103) (ylat2(m,nngs),m=1,mrgs) 
 103  format(10f7.2)
c--------------------
c
c------------- check dist between lines paral to GS
c
      call chdist(xlon2,ylat2,mmx,nngs,mrgs)
c
c---------   interpolate H from model grid to perp lines
c
      call model2perp(1.,H,HPE,IM,JM,1,xlon2,ylat2,'  HPE',
     *           mrgs,nngs,LONGMIN,LATMIN,dlon,dlat,mmx,cos30)
c--------------------
      print *,' after model2perp: mrgs=',mrgs
c     print *,'(HPE(m,nngs),m=1,mrgs)'
c     write(6,103) (HPE(m,nngs),m=1,mrgs) 
c--------------------
c
      print *,'      (HPE(m,nc),m=1,mrgs) nc,mrgs=',nc,mrgs
      write(6,102) (HPE(m,nc),m=1,mrgs)
 102  format(10f7.0)
c
c---------   interpolate TB1,SB1 from model grid to perp lines
c
      call model2perp(-999.,TB1,tt,IM,JM,nl,xlon2,ylat2,'   tt',
     *            mrgs,nngs,LONGMIN,LATMIN,dlon,dlat,mmx,cos30)
c
c--------------------
      print *,' after model2perp: mrgs=',mrgs
      m0=15
      n0=10
      print *,'l,(tt(m0,n,l),n=n0-5,n0+5) m0,n0=',m0,n0
      write(6,602) (n,n=n0-5,n0+5)
 602  format(6x,11i7)
      do l=1,nl
       write(6,603) l,(tt(m0,n,l),n=n0-5,n0+5)
      end do
 603  format(i7,11f7.2)
      print *,'l,(tt(m,n0,l),m=m0-5,m0+5) m0,n0=',m0,n0
      write(6,602) (m,m=m0-5,m0+5)
      do l=1,nl
       write(6,603) l,(tt(m,n0,l),m=m0-5,m0+5)
      end do
c
c----------- 01-31-03 send t=tsend=15 at z=400 betw. 2 LC brancches
c
      if(ntrj.eq.7) then
c---------------- falk 08-15-05 add file aladj
       call BAYprep(tt,nbndN,xlongs,ylatgs,t12S,Zlev,mmx,nngs,nc,
     *              HPE,xlon2,ylat2,nl,lu,ld,mrgs,cos30,aladj)
      end if
c---------------- falk 02-06-06 add RINGprep
      if(ntrj.eq.8) then
c------------------- 05-18-06 add nbndS for cold rings
       call RINGprep(tt,nbndN,nbndS,xlongs,ylatgs,Zlev,mmx,
     *               nngs,nc,nl,lu,ld,mrgs,cos30,aladj)
      end if
c       
c     print *,'(tt(m,nngs,1),m=1,mrgs)'
c     write(6,103) (tt(m,nngs,1),m=1,mrgs) 
c--------------------
c
c-------------- 12-27-02 do not send -9.
c      go to 199
c
c-------- send tt=-9. for H < zlev(l)
c-------- 08-31-00 save data for the first level under bottom
c-------- to use Frolov's vertical interpolation to sigma-levels
c
       print *,'before sending -9.'
       print *,'mrgs,nngs,nl=',mrgs,nngs,nl
       print *,'zlev(nl)  nl=',nl
       write(6,102) zlev
c----------------
       do m=1,mrgs
       do n=1,nngs
        do l=1,nl-1
         if(zlev(l).gt.HPE(m,n)) then
          tt(m,n,l+1)=-9.
         end if
        end do
c
        if(HPE(m,n).eq.1.) then
         do l=1,nl
          tt(m,n,l)=-9.
         end do
        end if
c
       end do
       end do
c
c----------- 04-16-03 send t=-9. after call tootis as in sharp
c
c-------------- 12-27-02 do not send -9.
  199  continue
c
c------------------
       print *,' before changetcur'
c      print *,'(nbndN(m),m=1,mrgs)'
c      write(6,104) (nbndN(m),m=1,mrgs)
c      print *,'(nbndS(m),m=1,mrgs)'
c      write(6,104) (nbndS(m),m=1,mrgs)
c------------------
c
       call parsm(xlon2,ylat2,tt,mmx,nngs,mrgs,nl,'tt',
     *                    nbndN,nbndS,lu,nsm,msm)
       print *,'after parsm tt in PROCTR'
c
c----------- 03-20-03 use changetcur2; remove special treatment 
c-----------          TR1 and TR4
       call changetcur2(tt,tm,nbndN,nbndS,mmx,mrgs,nngs,nl,
     *                 lu,ld,ncd,ntrj,mpr)
c
c----------- include msm in proctr as parameter
c
       call parsm(xlon2,ylat2,tm,mmx,nngs,mrgs,nl,'tm',
     *                    nbndN,nbndS,lu,nsm,msm)
c
       call stabil(tm,Zlev,mmx,mrgs,nngs,nl)
c----------------
       print *,'after changetcur: m,(tt(m,n,13),n=nc-5,nc+5)'
       write(6,106) (n,n=nc-5,nc+5)
 106   format(7x,11i7)
       do m=1,mrgs,10
        write(6,105) m,(tt(m,n,13),n=nc-5,nc+5)
       end do
       print *,'after changetcur: m,(tm(m,n,13),n=nc-5,nc+5)'
       write(6,106) (n,n=nc-5,nc+5)
       do m=1,mrgs,10
        write(6,105) m,(tm(m,n,13),n=nc-5,nc+5)
       end do
 105   format(i7,11f7.2)
c---------------- 02-01-05 check noise at 15-18N,82-85W
       print *,' check the last 25 points at lev=6 (75m)'
       print *,'            ntrj=',ntrj
       print *,' ylat2(mrgs-25,nc),ylat2(mrgs,nc) =',
     *           ylat2(mrgs-25,nc),ylat2(mrgs,nc)
       print *,'m,(tt(m,n,6),n=nc+5,nc+15)'
       write(6,106) (n,n=nc+5,nc+15)
       do m=mrgs-25,mrgs
        write(6,105) m,(tt(m,n,6),n=nc+5,nc+15)
       end do
c
       print *,'            ntrj=',ntrj
       print *,'m,(tt(m,n,6),n=nngs-10,nngs)'
       write(6,106) (n,n=nngs-10,nngs)
       do m=mrgs-25,mrgs
        write(6,105) m,(tt(m,n,6),n=nngs-10,nngs)
       end do
c
       print *,'m,(ylat2(m,n),n=nngs-10,nngs)'
       write(6,106) (n,n=nngs-10,nngs)
       do m=mrgs-25,mrgs
        write(6,105) m,(ylat2(m,n),n=nngs-10,nngs)
       end do
c
       print *,'m,(xlon2(m,n)/cos30,n=nngs-10,nngs)'
       write(6,106) (n,n=nngs-10,nngs)
       do m=mrgs-25,mrgs
        write(6,105) m,(xlon2(m,n)/cos30,n=nngs-10,nngs)
       end do
c-----------
       print *,'m,(tm(m,n,13),n=nc+5,nc+15)'
       write(6,106) (n,n=nc+5,nc+15)
       do m=mrgs-25,mrgs
        write(6,105) m,(tm(m,n,13),n=nc+5,nc+15)
       end do
c
       print *,'m,(tm(m,n,6),n=nngs-10,nngs)'
       write(6,106) (n,n=nngs-10,nngs)
       do m=mrgs-25,mrgs
        write(6,105) m,(tm(m,n,6),n=nngs-10,nngs)
       end do
c--------------- 02-10-06 check before smoothing
       if(ntrj.eq.7) then
        mup=149
        nup=7
        print *,' proctr: check tt and tm: mup,nup=',mup,nup
        print *,'(tt(m,nup,7),m=mup-10,mrgs)'
        write(6,103) (tt(m,nup,7),m=mup-10,mrgs)
        print *,'(tm(m,nup,7),m=mup-10,mrgs)'
        write(6,103) (tm(m,nup,7),m=mup-10,mrgs)
        print *,'(tt(m,nc-10,7),m=mup-10,mrgs)'
        write(6,103) (tt(m,nc-10,7),m=mup-10,mrgs)
        print *,'(tm(m,nc-10,7),m=mup-10,mrgs)'
        write(6,103) (tm(m,nc-10,7),m=mup-10,mrgs)
        print *,'(tt(mrgs-5,n,7),n=1,nc)' 
        write(6,103) (tt(mrgs-5,n,7),n=1,nc)
        print *,'(tm(mrgs-5,n,7),n=1,nc)' 
        write(6,103) (tm(mrgs-5,n,7),n=1,nc)
       end if
c----------------
c
c--------- interpolate t from perp lines to model grid
c
c---------  12-13-05 use for BAY lu=1
c     if(ntrj.eq.7) then
c---------  02-06-06 use for RING lu=1
      if(ntrj.eq.7.or.ntrj.eq.8) then
       call tootis(t,tm,im,jm,xlon2,ylat2,mmx,nngs,nl,1,
     *     dgs,LONGMIN,LATMIN,dlon,dlat,mrgs,mb,me,nc,xout,cos30,
     *        migw,mige,nbndN,nbndS)
      else
       call tootis(t,tm,im,jm,xlon2,ylat2,mmx,nngs,nl,lu,
     *     dgs,LONGMIN,LATMIN,dlon,dlat,mrgs,mb,me,nc,xout,cos30,
     *        migw,mige,nbndN,nbndS)
      end if
c------------- 02-10-06 add output for LC and GS
      if(ntrj.eq.7) then
       print *,'end proctr: ntrj=',ntrj
       print *,'after sending warm tt and changetcur'
       call pritem3c(t,XI,YI,im,jm,nl,-83.00,1,22.00,25.00,
     *      't l=7  z=100  ',-1,7,cos30,1)
      end if
c------------- 02-08-06 add output for RING
      if(ntrj.eq.8) then
       print *,'after changetcur and tootis'
       call pritem3c(t,XI,YI,im,jm,nl,-90.00,1,25.00,27.00,
     *      't l=1  z=0    ',-1,1,cos30,1)
      end if
c
c
c------------- 04-16-03 send t=-9. after call tootis as in sharp
c
       do i=1,im
       do j=1,jm
        do l=1,nl-1
         if(zlev(l).gt.H(i,j)) then
          t(i,j,l+1)=-9.
         end if
        end do
        if(H(i,j).eq.1.) then
         do l=1,nl
          t(i,j,l)=-9.
         end do
        end if
       end do
       end do
c
       print *,' END PROCTR'
c
       return
       end
c
c-------------
c
c---------- 02-14-03 send new xloop,yloop to initial xlongs,ylatgs
c
      subroutine looptr(xlongs,ylatgs,mmx,mrgs,step,ynp,cos30,
     *                  xconw,yconw,xcone,ycone,xnpc,ynpc)
c------------ 04-29-03 change path in BAY and xwp,...yep
c     parameter(xwp=-86.54,ywp=23.48,xep=-84.77,yep=24.46)
c------------ 10-18-05 adjust xep,yep to nc_3=nc-3
c     parameter(xwp=-86.54,ywp=23.49,xep=-84.77,yep=24.77,
      parameter(xwp=-86.54,ywp=23.49,xep=-84.50,yep=24.65,
c------------ 06-17-04 add ynpmax
     *          ynpmax=27.5)
      dimension xloop(mmx),yloop(mmx),xlongs(mmx),ylatgs(mmx),
     *   xt1(mmx),yt1(mmx),xt2(mmx),yt2(mmx),xt3(mmx),yt3(mmx),
     *   xbnd(mmx),ybnd(mmx)
      real xconw,yconw,xcone,ycone,xnpc,ynpc
c
c------- xlongs,ylatgs the BAY TR obtained for Levitus climate
c------- mw the last fixed point of a west branch of the BAY TR 
c------- me the  first fixed point of an east branch of the BAY TR 
c------- ynp the northern point of Loop Current from satellite data
c------- subr. calculates new BAY TR through p. mw, me simmetric
c-------       about perp line to line connected p. mw, me and
c-------       goes through ynp
c
c------- mw= 26 (23.48,-86.54)
c------- me=100 (24.46,-84.80)
c------- filow=22.0
      F(x1,y1,x2,y2)=sqrt((x1-x2)**2+(y1-y2)**2)
      PI=3.141592
c---------- specify filow to compare TR before and after smoothing
      filow=22.0
c-------------
      print *,'         '
      print *,'              LOOPTR: mmx,mrgs=',mmx,mrgs
      write(6,99) ynp
 99   format('ynp=',f7.2)
c---------------- 06-17-04 check ynp
      if(ynp.lt.yep) then
        ynp=yep
        write(6,301) ynp,yep,ynpmax
 301    format('correct ynp: ynp,yep,ynpmax=',3f7.2) 
      end if
      if(ynp.gt.ynpmax) then
        ynp=ynpmax
        write(6,301) ynp,yep,ynpmax
      end if
c----------------        
      call pri10D1(xlongs,mmx,mrgs,1,mrgs,cos30,' xlongs   ')
      call pri10D1(ylatgs,mmx,mrgs,1,mrgs,1.,' ylatgs   ')
c-------------
c-------- 01-21-03 find mw,me closest to (xwp,ywp) and (xep,yep)
      xw=xwp*cos30
      yw=ywp
      xe=xep*cos30
      ye=yep
      dw=1000.
      de=1000.
      do m=1,mrgs
       d=F(xw,yw,xlongs(m),ylatgs(m))
       if(d.lt.dw) then
        dw=d
        mw=m
       end if
       dd=F(xe,ye,xlongs(m),ylatgs(m))
       if(dd.lt.de) then
        de=dd
        me=m
       end if
      end do
c------------
      write(6,201) xwp,ywp,xep,yep,mw,me
 201  format('   xwp,ywp,xep,yep,mw,me=',4f7.2,2i7)
c------------
      x1=xlongs(mw)
      y1=ylatgs(mw)
      x2=xlongs(me)
      y2=ylatgs(me)
c------- find middle point between mw and me
      x0=(x1+x2)*0.5
      y0=(y1+y2)*0.5
c------- find param. for line (1,2) connected points 1 and 2 
      pk=(y2-y1)/(x2-x1)
      pk2=pk*pk
c------- find on perp. line to (1,2) through p. (x0,y0) p. (xnp,ynp)
      xnp=x0-pk*(ynp-y0)
      r=F(x1,y1,x0,y0)
      d=F(xnp,ynp,x0,y0)
c-----------
      write(6,101) x1/cos30,x2/cos30,x0/cos30,y1,y2,y0
 101  format('x1/cos30,x2/cos30,x0/cos30,y1,y2,y0=',
     *        6f7.2)
      write(6,102) xnp/cos30,ynp,pk,pk2,r,d
 102  format('xnp/cos30,ynp,pk,pk2,r,d=',2f7.2,2f8.3,2f7.2)
c-----------
      if(d.lt.r) then
       xnpc=x0-r/sqrt(1.+1./pk2)
       ynpc=y0-1./pk*(xnpc-x0)
       mig=1
      else
       xnpc=xnp
       ynpc=ynp
       mig=0
      end if
c------- find p. (xc,yc) on perp line at distance r from (xnp,ynp)
      xc=xnpc+r/sqrt(1.+1./pk2)
      yc=ynpc-1./pk*(xc-xnpc)
c------- find 2 p. on line through (xc,yc) || to (1,2) at dist. r
      xc1=xc-r/sqrt(1.+pk2)
      yc1=yc+pk*(xc1-xc)
      xc2=xc+r/sqrt(1.+pk2)
      yc2=yc+pk*(xc2-xc)
c-----------
      write(6,103) xc1/cos30,xc2/cos30,xc/cos30,yc1,yc2,yc
 103  format('xc1/cos30,xc2/cos30,xc/cos30,yc1,yc2,yc=',
     *        6f7.2)
c---------- check distances
      r1=F(xc1,yc1,xc,yc)
      r2=F(xc2,yc2,xc,yc)
      r3=F(xc,yc,xnpc,ynpc)
      write(6,106) xnp/cos30,xnpc/cos30,ynp,ynpc,r1,r2,r3
 106  format('xnp/cos30,xnpc/cos30,ynp,ynpc,r1,r2,r3=',
     *       4f7.2,3f8.4)
c-----------
      m1=0
      m2=0
      if(mig.eq.0) then
       d1=F(xc1,yc1,x1,y1)
       m1=d1/step
       step1=d1/float(m1)
       xt1(1)=x1
       yt1(1)=y1
       do m=1,m1
        xt1(m+1)=xt1(m)-step1/sqrt(1.+1./pk2)
        yt1(m+1)=yt1(m)-1./pk*(xt1(m+1)-xt1(m))
       end do
       d2=F(xc2,yc2,x2,y2)
       m2=d2/step
       step2=d2/float(m2)
       xt2(1)=xc2
       yt2(1)=yc2
       do m=1,m2
        xt2(m+1)=xt2(m)+step2/sqrt(1.+1./pk2)
        yt2(m+1)=yt2(m)-1./pk*(xt2(m+1)-xt2(m))
       end do
c-----------
       write(6,104) m1,d1,step,step1,m2,d2,step2
 104   format('m1,d1,step,step1,m2,d2,step2=',
     *         i7,f7.2,2f8.4,i7,f7.2,f8.4)
       call pri10D1(xt1,mmx,m1+1,1,m1+1,cos30,'       xt1')
       call pri10D1(yt1,mmx,m1+1,1,m1+1,1.,'       yt1')
       call pri10D1(xt2,mmx,m2+1,1,m2+1,cos30,'       xt2')
       call pri10D1(yt2,mmx,m2+1,1,m2+1,1.,'       yt2')
c-----------
      end if 
c
      d3=PI*r
      m3=d3/step
      al=PI/float(m3)
      al0=PI+atan(pk)
      xt3(1)=x1
      yt3(1)=y1
      do m=1,m3+1
       alc=al0-al*(m-1)
       xt3(m)=xc+r*cos(alc)
       yt3(m)=yc+r*sin(alc)
      end do
c-----------
      write(6,105) m3,d3,al,al0
 105  format('m3,d3,al,al0=',i7,f7.2,2f8.4)
      call pri10D1(xt3,mmx,m3+1,1,m3+1,cos30,'       xt3')
      call pri10D1(yt3,mmx,m3+1,1,m3+1,1.,'       yt3')
c-----------
      do m=1,mw-1
       xloop(m)=xlongs(m)
       yloop(m)=ylatgs(m)
      end do
      if(mig.eq.1) then
       me1=mw
       go to 200
      end if
      me1=mw+m1
      do m=mw,mw+m1-1
       k=m-mw+1
       xloop(m)=xt1(k)
       yloop(m)=yt1(k)
      end do
 200  continue
          print *,'mig,me1=',mig,me1
      me2=me1+m3
      do m=me1,me1+m3-1
       k=m-me1+1
       xloop(m)=xt3(k)
       yloop(m)=yt3(k)
      end do
      if(mig.eq.1) then
       me3=me2
       go to 300
      end if
      do m=me2,me2+m2-1
       k=m-me2+1
       xloop(m)=xt2(k)
       yloop(m)=yt2(k)
      end do
      me3=me2+m2
 300  continue
         print *,'mig,me3=',mig,me3
      me4=mrgs-me
      do m=me3,me3+me4
       k=m-me3+me
       xloop(m)=xlongs(k)
       yloop(m)=ylatgs(k)
      end do
      mloop=me3+me4
      mconw=me1
      mcone=me2
c----------- 01-21-03 remember xconw,yconw,xcone,ycone
      xconw=xloop(mconw)
      yconw=yloop(mconw)
      xcone=xloop(mcone)
      ycone=yloop(mcone)
      write(6,202) mconw,mcone,xconw/cos30,yconw,xcone/cos30,ycone
 202  format(' mconw,mcone,xconw/cos30,yconw,xcone/cos30,ycone=',
     *         2i7,4f7.2)
c-----------
      print *,'mw,m1,me1,m2,me2,m3,me3,me4,mrgs,me=',
     *         mw,m1,me1,m2,me2,m3,me3,me4,mrgs,me
      call pri10D1(xloop,mmx,mloop,1,mloop,cos30,'     xloop')
      call pri10D1(yloop,mmx,mloop,1,mloop,1.,'     yloop')
c-----------
      mrgs=mloop
      do m=1,mloop
       xlongs(m)=xloop(m)
       ylatgs(m)=yloop(m)
      end do
c-----------
      write(6,110) xconw/cos30,yconw,xcone/cos30,ycone,xnpc/cos30,ynpc
 110  format('xconw/cos30,yconw,xcone/cos30,ycone,xnpc/cos30,ynpc=',
     *       6f7.2)
      print *,' END LOOPTR'
c-----------
      return
      end
c
c--------------------
c
      subroutine pri10D1(f,mmx,mm,m1,m2,cos30,char)
      dimension f(mmx),r(10)
      character*10 char
      print *,'  PRI10: mmx,mm,m1,m2=',mmx,mm,m1,m2
      write(6,100) char
 100  format(/10x,a25)
      write(6,101) (i,i=1,10)
 101  format(5x,10i7)
      nn=mm/10
      do n=1,nn
       do k=1,10
        m=10*(n-1)+k
        r(k)=f(m)/cos30
       end do
       write(6,102) 10*(n-1)+1,(r(k),k=1,10),10*n
 102   format(i7,10f7.2,i7)
      end do
      if(mm.gt.nn*10) then
       write(6,103) 10*nn+1,(f(m)/cos30,m=nn*10+1,mm)
 103   format(i7,10f7.2)
      end if
      print *,'     '
      return
      end
c
c-------------------- 11-28-05 add pri10I1 to print nbnd
c
      subroutine pri10I1(jf,mmx,mm,m1,m2,char)
      dimension jf(mmx),jr(10)
      character*10 char
      print *,'  PRI10I1: mmx,mm,m1,m2=',mmx,mm,m1,m2
      write(6,100) char
 100  format(/10x,a10)
      write(6,101) (i,i=1,10)
 101  format(7x,10i7)
      print *,'     '
      nn=mm/10
      do n=1,nn
       do k=1,10
        m=10*(n-1)+k
        jr(k)=jf(m)
       end do
       write(6,102) 10*(n-1)+1,(jr(k),k=1,10),10*n
 102   format(i7,10i7,i7)
      end do
      if(mm.gt.nn*10) then
       write(6,103) 10*nn+1,(jf(m),m=nn*10+1,mm)
 103   format(i7,10i7)
      end if
      print *,'     '
      return
      end
c
c--------------
c
       subroutine parsm(xlon2,ylat2,tt,mmx,nngs,mrgs,nl,char,
     *                    nbndN,nbndS,lu,nsm,msm)
       dimension xlon2(mmx,nngs),ylat2(mmx,nngs),tt(mmx,nngs,nl)
       dimension nbndN(mmx),nbndS(mmx),tr(mmx),mbr(mmx),mer(mmx)
c----------- 03-21-03 add msmc
       integer msmc(nngs)
       character*2 char
       logical ch1,ch2,ch3,ch4,ch5,ch6,ch7
c
c------------  this subr finds intervals on perp lines to a GS path
c                with data (over the ocean bottom) 
c                  and between nbndN and nbndS
c------------  smooths tt in the interval (nc-nsm,nc+nsm) 
c                 with a slide smoothing procedure along m
c                   in the interval (m-msm,m+msm)
c
       nc=(nngs+1)/2
c
      print *,' parsm: mmx,nngs,mrgs,nl,nc=',mmx,nngs,mrgs,nl,nc
      print *,' char,lu,nsm,msm=',char,lu,nsm,msm
c
c     print *,'  (xlon2(1,n),n=1,nngs)'
c     write(6,104) (xlon2(1,n),n=1,nngs)
c     print *,'  (tt(1,n,1),n=1,nngs)'
c     write(6,104) (tt(1,n,1),n=1,nngs)
 104  format(10f7.2)
c     print *,'  (xlon2(mrgs,n),n=1,nngs)'
c     write(6,104) (xlon2(mrgs,n),n=1,nngs)
c     print *,'  (tt(mrgs,n,1),n=1,nngs)'
c     write(6,104) (tt(mrgs,n,1),n=1,nngs)
c
c     print *,'surface temp along GS center (tt(m,nc,1),m=1,mrgs)'
c     write(6,104) (tt(m,nc,1),m=1,mrgs)
c     print *,'temp at z=200m along GS center (tt(m,nc,10),m=1,mrgs)'
c     write(6,104) (tt(m,nc,10),m=1,mrgs)
c     print *,'temp at z=400m along GS center (tt(m,nc,13),m=1,mrgs)'
c     write(6,104) (tt(m,nc,13),m=1,mrgs)
c
       n0=nc-5
       l0=13
c
       n1=nc-nsm
       n2=nc+nsm
c----------- 03-21-03 add msmc 
c-------   msmc=msm in the interval n=nc+-ncon and decrease
c-------            to 1 linearly  at n=nc+-nsm;   ncon<nsm
       ncon=4
       nb=nc-ncon
       ne=nc+ncon
       do n=n1,nb
        ap=float(msm-1)*float(n-n1)/float(nb-n1)
        msmc(n)=1+INT(ap)
        if(msmc(n).lt.1) msmc(n)=1
       end do
       do n=nb,ne
        msmc(n)=msm
       end do
       do n=ne+1,n2
        ap=float(msm-1)*float(n2-n)/float(n2-ne) 
        msmc(n)=1+INT(ap)
        if(msmc(n).lt.1) msmc(n)=1
       end do
       print *,'(msmc(n),n=n1,n2)',(msmc(n),n=n1,n2)
c-------
       do l=lu,nl
        do n=n1,n2
         do m=1,mrgs
          tr(m)=tt(m,n,l)
         end do
c----------      find intervals with data for each n and l
c----------      mbr(k)<=m<= mer(k)  k-interval kk is a number of 
c                                              intervals
         mig=1
         k=1
         do m=1,mrgs
          ch1=mig.eq.1
          ch2=mig.eq.2
          ch3=tr(m).gt.-8.
          ch4=tr(m).lt.-8.
          ch5=m.eq.mrgs
          ch6=nbndN(m).le.n.and.n.le.nbndS(m)
          ch7=n.lt.nbndN(m).or.n.gt.nbndS(m)
          if(ch1.and.ch3.and.ch6) then
            mbr(k)=m
            mig=2
            go to 100
          end if
          if(ch2.and.(ch4.or.ch7)) then
            mer(k)=m-1
            k=k+1
            mig=1
            go to 100
          end if
          if(ch2.and.ch5) then
            mer(k)=m
            k=k+1
          end if
 100      continue
         end do                      ! end loop in m
c
         kk=k-1
         if(kk.lt.1) go to 200
c-----------------
         if(n.eq.n0.and.l.eq.l0) then
          print *,' parsm  n0,l0,kk=',n0,l0,kk
          print *,'           (mbr(k),k=1,kk)'
          write(6,101) (mbr(k),k=1,kk)
          print *,'           (mer(k),k=1,kk)'
          write(6,101) (mer(k),k=1,kk)
 101      format(10i7)
         end if
c-----------------
         do k=1,kk
          m1=mbr(k)
          m2=mer(k)
          if((m2-m1).lt.2) go to 300
c-----------------
          if(n.eq.n0.and.l.eq.l0) then
           print *,' before slidesm'
           print *,'(tr(m),m=m1,m2)   k,m1=mbr(k),m2=mer(k)',
     *              k,m1,m2
           write(6,104) (tr(m),m=m1,m2)
          end if
c-----------------
c----------- 03-21-03 add msmc(n) 
          call slidesm(tr,mmx,m1,m2,msmc(n))
c-----------------
          if(n.eq.n0.and.l.eq.l0) then
           print *,' after slidesm'
           print *,'(tr(m),m=m1,m2)   k,m1=mbr(k),m2=mer(k)',
     *              k,m1,m2
           write(6,104) (tr(m),m=m1,m2)
          end if
c-----------------
 300      continue
         end do      !    loop in k 
         do m=1,mrgs
          tt(m,n,l)=tr(m)
         end do
 200     continue
        end do       !    loop in n 
       end do        !    loop in l 
       return
       end
c
c------------- 11-18-05 rewrite slidesm : add w(mmx) ksm1,ksm2
c
       subroutine slidesm(f,mmx,m1,m2,msm)
c---------------- (m2-m1).ge.2
       dimension f(mmx),w(mmx)
       do m=m1,m2
        sum=0.
c       ksm=min(msm,m-m1,m2-m)
        ksm1=min(msm,m-m1)
        ksm2=min(msm,m2-m)
        k1=m-ksm1
        k2=m+ksm2
        do k=k1,k2
         sum=sum+f(k)
        end do
        kk=k2-k1+1
        w(m)=sum/kk
       end do
c---------------
       do m=m1,m2
        f(m)=w(m)
       end do
       return
       end
c
c-------------- 02-14-03 make new subr
c
      subroutine evenstep(x,y,mmx,mrgs,step,cos30)
      dimension x(mmx),y(mmx),dalgs(mmx),R1(mmx)
      dimension xnew(mmx),ynew(mmx),dalgsnew(mmx)
c----
c----   stepm is a specified step along GS path
c----   stepnew is even step along curve (very close to stepm)
c----   mm is a number of points along GS after skipping points
c----         with a distance less than 0.4*stepm
c----   mmnew is a number of points along GS with even step
c----   subr. transforms a curve (x,y) with mrgs points and not even 
c----         distances between points to
c----         a curve (x,y) with even distances along curve with
c----         a new number of points mrgs
c
c-------------
      F(x1,y1,x2,y2)=sqrt((y1-y2)**2+(x1-x2)**2)
c-------------
      print *,'          ' 
      print *,'           EVENSTEP(x,y,mmx,mrgs,step,cos30)'
      print *,'   mmx,mrgs,step,cos30=',mmx,mrgs,step,cos30
      print *,'old x/cos30 with not even steps along GS path: ',mrgs
      write(6,103) (x(m)/cos30,m=1,mrgs)
      print *,'old y with not even steps along GS path: mrgs=',mrgs
      write(6,103) (y(m),m=1,mrgs)
c
c------------- 02-13-03 skip points with a distance less 0.4*step
c
      dalgs(1)=0.
      dalgsmax=0.
      dalgsmin=1000.
      x1=x(1)
      y1=y(1)
      xnew(1)=x1
      ynew(1)=y1
      k=1
      do m=2,mrgs
       x2=x(m)
       y2=y(m)
       d=F(x1,y1,x2,y2)
       if(d.gt.0.4*step) then
        k=k+1
        xnew(k)=x2
        ynew(k)=y2
c------------   07-19-05 change m to k in the next line
        dalgs(k)=dalgs(k-1)+d
        x1=x2
        y1=y2
c---------- find max and min distance
        if(dalgsmax.lt.d) then
         dalgsmax=d
         mdalgsmax=k
        end if
        if(dalgsmin.gt.d) then
         dalgsmin=d
         mdalgsmin=k
        end if
c------------
c------------ 07-19-05 add else to check
       else
        write(6,201) m,k,d
 201    format('d.lt.0.4*step   m,k,d=',2i7,f10.3)
       end if
c-----------
       if(m.eq.mrgs) print *,' m,mrgs,k=',m,mrgs,k
      end do
      mm=k
c-------------------
      write(6,101) mmx,mm,dalgs(mm),dalgsmax,dalgsmin,
     *             mdalgsmax,mdalgsmin
 101  format(/10x,'evenstep: mmx,mm,dalgs(mm),dalgsmax,dalgsmin',
     *       /10x,2i7,3f8.4,/10x,'mdalgsmax,mdalgsmin=',2i7)
c-------------------
c------------- find new mrgs with even step
      adal=dalgs(mm)
      mmnew=dalgs(mm)/step+1.001
c
      if(mmnew.gt.mmx) mmnew=mmx
c
      stepnew=dalgs(mm)/float(mmnew-1)
      do m=1,mmnew
       dalgsnew(m)=stepnew*float(m-1)
      end do
c------------
      write(6,104) step,stepnew,dalgs(mm),dalgsnew(mmnew)
 104  format('step,stepnew,dalgs(mm),dalgsnew(mmnew)=',4f12.6)
c------  remove computation error
      dalgs(mm)=dalgsnew(mmnew)
c-------------
      print *,' mrgs is old with not even step  ',mrgs
      print *,' mm is after skipping            ',mm
      print *,' mmnew is new with even step     ',mmnew
      write(6,102) step,stepnew,mrgs,mm,mmnew
 102  format(/5x,'step,stepnew,mrgs,mm,mmnew=',2f7.4,3i7)
c
      call SP(dalgs,xnew,x,R1,dalgsnew,mm,mmnew,mmx,mmx)
c
      call SP(dalgs,ynew,y,R1,dalgsnew,mm,mmnew,mmx,mmx)
c
      mrgs=mmnew
c-------------
      print *,'x(mmnew)/cos30=',x(mmnew)/cos30
      print *,'y(mmnew)=',y(mmnew)
c-------------
      print *,'x/cos30 with even steps along GS path: mmnew=',mmnew
      write(6,103) (x(m)/cos30,m=1,mmnew)
      print *,'ynew with even steps along GS path: mmnew=',mmnew
      write(6,103) (y(m),m=1,mmnew)
 103  format(10f7.2)
c-------------
      return
      end
c
c-------- 03-20-03 use changetcur2; remove special treatment 
c--------          TR1 and TR4
      subroutine changetcur2(tt,tm,nbndN,nbndS,mmx,mrgs,nngs,ll,
     *                 lu,ld,ncd,ntrj,mpr)
c
      dimension tt(mmx,nngs,ll),tm(mmx,nngs,ll),wtem(10),wt(nngs)
      dimension nbndN(mmx),nbndS(mmx)
c----------- 02-01-05 change infb(ll),infe(ll)
      dimension infb(mmx,ll),infe(mmx,ll),tdifb(ll),tdife(ll)
c
      data wtem/1.5,1.3,0.8,0.35,0.30,0.25,0.20,0.15,0.10,0.05/
c---------------- 04-08-03 check common
      common/tdif/tdifbs(99),tdifes(99)
c----------- 02-13-06 include tdifbmax to restrict tdifb and tdife
      dimension tdifbmax(ll)
c
c---- subr. sharps gradient of temperature along current path,
c---- finds for levels from (lu to ld) interval (nb,ne) where t>-9. 
c---- finds tmax in interval  (nb,nc-1) and tmin in (nc+1,ne),
c---- sharps gradient of t in the interval (nc-ncd,nc+ncd)
c---- using weights wtem.
c
c---- tt and tm are temp along perp lines to the current;
c---- tt before sharpening, tm after;
c---- nbndN,nbndS are north and south boundaries for current;
c----  nbndN=<nc, nbndS=>nc
c---- current goes from east to west, n=1,nngs goes from north 
c---- (warm temp) to south (cold temp).
c---- infb(m,l)=1 if there is land from n=nbndN to n=nc
c---- infe(m,l)=1 if there is land from n=nc to n=nbndS
c
c---------------- 04-25-03 increase in BAY tdife linearly from 
c                          m=75 (23N) to m=99 (25N)
       mincr1=75
       mincr2=99
c---------------- 02-13-06 specify tdifbmax
       do l=1,ll
        if(l.lt.5) tdifbmax(l)=0.5
        if(l.ge.5.and.l.lt.9) tdifbmax(l)=(1.5*l-1.5)/4.
        if(l.ge.9.and.l.lt.13) tdifbmax(l)=(l+3.)/4.
        if(l.ge.13.and.l.lt.15) tdifbmax(l)=4.
        if(l.ge.15.and.l.lt.20) tdifbmax(l)=(-3.*l+65.)/5.
        if(l.ge.20.and.l.lt.27) tdifbmax(l)=(-0.9*l+25.)/7.
        if(l.ge.27) tdifbmax(l)=0.1
       end do
c----------------
       print *,'changetcur2: tdifbmax(l), l=1,ll  ll=',ll
       write(6,130) tdifbmax
 130   format(10f7.3)
c----------------
c
       nc=(1+nngs)/2
c---------------- 04-08-03 check common
       if(ntrj.ne.1) then
        print *,' check common mrgs,ll,ntrj=',mrgs,ll,ntrj
        print *,' (tdifbs(l),l=1,ll) at the last point mrgs, l=1,ll'
        write(6,102) (tdifbs(l),l=1,ll) 
        print *,' (tdifes(l),l=1,ll) at the last point mrgs, l=1,ll'
        write(6,102) (tdifes(l),l=1,ll) 
       end if
c---------------- 
c---------------- 04-08-03 use changetcur2 also for GS ntrj=5
c----------------          save tdifb,tdife from BAY for GS
c----------------          GS is calculated after BAY
       if(ntrj.eq.5) then
        do l=1,ll
         tdifb(l)=tdifbs(l)
         tdife(l)=tdifes(l)
        end do
       else
        do l=1,ll
         tdifb(l)=0.
         tdife(l)=0.
        end do
       end if
c
c----------  specify ncd number of points where gradient of temp. 
c----------    will be sharpened
c      ncd=8
c
c----------------
c     print *,' begin CHANGETCUR1'
c     print *,'(nbndN(m),m=1,mrgs)'
c     write(6,103) (nbndN(m),m=1,mrgs)
c     print *,'(nbndS(m),m=1,mrgs)'
c     write(6,103) (nbndS(m),m=1,mrgs)
c----------------
       do n=1,nngs
        wt(n)=0.
       end do
       aa=0.
       do i=1,ncd
        aa=aa+wtem(i)
       end do
c
c---------- 11-14-02 not *0.5, use weights for each side
       do n1=nc-ncd,nc-1
        n=(nc-1)-n1+(nc-ncd)
        i=nc-n
c       wt(n)=wtem(i)/aa*0.5
        wt(n)=wtem(i)/aa
       end do
       do n=nc+1,nc+ncd
        i=n-nc
c       wt(n)=wtem(i)/aa*0.5
        wt(n)=wtem(i)/aa
       end do
c----------------
       print *,'  CHANGETCUR1: ntrj,mpr=',ntrj,mpr
       print *,'  mmx,mrgs,nngs,ncd,nc,ll,lu,ld=',
     *            mmx,mrgs,nngs,ncd,nc,ll,lu,ld
       print *,'     (wtem(i),i=1,10)'
       write(6,142) (wtem(i),i=1,10)
 142   format(10f7.2)
       print *,'          (wt(n),n=nc-ncd,nc+ncd)'
       write(6,141) (wt(n),n=nc-ncd,nc+ncd)
 141   format(11f7.2)
c----------------
c
c----- send tt to tm,  tt =-9. in the points without data
c
       do m=1,mrgs
       do n=1,nngs
       do l=1,ll
        tm(m,n,l)=tt(m,n,l)
       end do
       end do
       end do
c
c----------------
c     print *,'                          before m loop'
c     print *,'        (nbndN(m),m=1,mrgs)'
c     write(6,103) (nbndN(m),m=1,mrgs)
c     print *,'            (nbndS(m),m=1,mrgs)'
c     write(6,103) (nbndS(m),m=1,mrgs)
 103  format(10i7)
c     print *,'                  (tm(1,n,lu),n=1,nngs)'
c     write(6,142) (tm(1,n,lu),n=1,nngs)
c----------------
c
c----------- 02-01-05 change 1D infb(ll),infe(ll) to 2D
c----------- and calculate them before in additional loops in m,l
      do m=1,mrgs
       do l=lu,ld
        if(tm(m,nc,l).lt.-8.) then
         infb(m,l)=1
         infe(m,l)=1
        else
         infb(m,l)=0
         nbb=nbndN(m)
         nb=nbb
         if(nbb.eq.nc) go to 121
         do n=nc,nbb,-1
          if(tm(m,n,l).lt.-8.) then
           nb=n+1
           infb(m,l)=1
           go to 121
          end if
         end do
 121     continue
         infe(m,l)=0
         nee=nbndS(m)
         ne=nee
         if(nee.eq.nc) go to 122
         do n=nc,nee
          if(tm(m,n,l).lt.-8.) then
           ne=n-1
           infe(m,l)=1
           go to 122
          end if
         end do
 122     continue
        end if
c-------------- 02-02-05 add output for debug
        if(ntrj.eq.7.and.l.eq.6.and.m.eq.192) then
         print *,' debug TR7'
         print *,'ntrj,l,m,infe(m,l),infe(m-1,l)=',
     *          ntrj,l,m,infe(m,l),infe(m-1,l)
         print *,'nbndS(m),ne,nee,nc=',
     *            nbndS(m),ne,nee,nc
        end if
        if(ntrj.eq.5.and.l.eq.6.and.m.eq.44) then
         print *,' debug GS'
         print *,'ntrj,l,m,infb(m,l),infb(m-1,l)=',
     *          ntrj,l,m,infb(m,l),infb(m-1,l)
         print *,'nbndN(m),nb,nbb,nc=',
     *            nbndN(m),nb,nbb,nc
        end if
c-------------- 
       end do
      end do
c-------------- 02-02-05 add output for debug
      print *,'check infb at lev=6 (75m)' 
      write(6,124) (infb(m,6),m=1,mrgs)
 124  format(10i7)
c--------------    loop in m -------
c
c------ remove infb,infe calculation from  next loop 
      do m=1,mrgs
       do l=lu,ld
        if(tm(m,nc,l).lt.-8.) go to 300
        tcen=tm(m,nc,l)
        nbb=nbndN(m)
        nb=nbb
        if(nbb.eq.nc) go to 120
        do n=nc,nbb,-1
         if(tm(m,n,l).lt.-8.) then
          nb=n+1
          go to 120
         end if
        end do
 120    continue
c------------ 01-02-03
c       if(ntrj.eq.4.and.l.eq.10.and.m.eq.mpr) then
c        print *,'ntrj,l,m=',ntrj,l,m
c        print *,'nb,nbb,nc,infb(m,l),tcen=',nb,nbb,nc,infb(m,l),tcen
c       end if
c----------- 12-19-02 use max and min points
        nmax=nb
        nbc=nb
        nchb=nb
        difb=0.
        tmax=0.
        if(nb.ge.nc-1) go to 100
        do n=nb,nc-1
         if(tm(m,n,l).gt.tmax) then
           tmax=tm(m,n,l)
           nmax=n
         end if
        end do
        difb=tmax-tcen
c----------- 02-01-05 calculate weights
        if(infb(m,l).eq.1) then 
         k1=max(1,m-10)
         k2=min(m+10,mrgs)
         mbef=k1
         do k=k1,m
          if(infb(k,l).eq.0) mbef=k
         end do
         maft=k2
         do k=k2,m,-1
          if(infb(k,l).eq.0) maft=k
         end do
         ialph=min(m-mbef,maft-m)
         if(m-5.le.0) ialph=maft-m
         if(m+5.ge.mrgs) ialph=m-mbef
         alph=0.1*float(ialph)
c----------- 02-01-05 use weights
         if(difb.lt.0.) then
          difb=alph*tdifb(l)
         else
          difb=max(difb,alph*tdifb(l)+(1.-alph)*difb)
         end if
         nbc=nb
        else
         nbc=nmax
         if(nmax.ge.nc-1) go to 100
        end if
c-------------- 02-02-05 add output for debug
       if(ntrj.eq.7.and.l.eq.6.and.m.eq.192) then
        print *,' debug end of BAY for difb ntrj=',ntrj
        print *,'m,k1,k2,mbef,maft,ialph,alph=',
     *           m,k1,k2,mbef,maft,ialph,alph
        print *,' tdifb(6),difb=',tdifb(6),difb
       end if
c-------------- 
c------------ 01-02-03
c       if(ntrj.eq.4.and.l.eq.10.and.m.eq.mpr) then
c        print *,'ntrj,l,m=',ntrj,l,m
c        print *,'nbc,nmax,tcen,tmax,difb=',nbc,nmax,tcen,tmax,difb
c       end if
c------------
        if(difb.le.0.) go to 100
        if(nbc.lt.nc-ncd) then
         nchb=nc-ncd
         do n=nbc,nc-ncd-1
          tm(m,n,l)=tcen+difb
         end do
        else
         nchb=nbc
        end if
        cc=0.
        do n=nc-1,nchb,-1
         cc=cc+wt(n)
        end do
        do n=nc-1,nchb,-1
         tm(m,n,l)=tm(m,n+1,l)+difb*wt(n)/cc
        end do  
cRMY 07-14-06 begin!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cRMY DON'T DO 01-23-06 decrease!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c------------ 01-23-06 decrease changes at lu,lu+1,lu+2 in LC
c------------                               and in GS
cRMY        if(ntrj.eq.7.or.ntrj.eq.5) then
cRMY         do n=nbc,nc-1
cRMY          if(l.eq.lu) tm(m,n,l)=0.25*tm(m,n,l)+0.75*tt(m,n,l)
cRMY          if(l.eq.lu+1) tm(m,n,l)=0.5*tm(m,n,l)+0.5*tt(m,n,l)
cRMY          if(l.eq.lu+2) tm(m,n,l)=0.75*tm(m,n,l)+0.25*tt(m,n,l)
cRMY         end do
cRMY        end if
c------------
cRMY 07-14-06 end  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
c------------ 02-03-05
        if(ntrj.eq.7.and.l.eq.6.and.m.eq.192) then
          print *,'  end of BAY'
          print *,'ntrj,l,m=',ntrj,l,m
          print *,'nc,nchb,cc,difb=',nc,nchb,cc,difb
        end if
c-----------
 100    continue
        tdifb(l)=max(difb,tdifb(l))
c--------------- 02-13-06 include tdifbmax
        if(tdifb(l).gt.tdifbmax(l)) tdifb(l)=tdifbmax(l)
c
        tnb=tm(m,nb,l)
        tnbc=tm(m,nbc,l)
c--------------- 02-03-05
        if(ntrj.eq.7.and.l.eq.6.and.m.eq.192) then
         print *,'ntrj,l,m=',ntrj,l,m
         print *,'    l,nb,nbc,nmax,nchb,nc=',l,nb,nbc,nmax,nchb,nc
         write(6,104) l,tnb,tnbc,tcen,tnb-tnbc,tnbc-tcen
        end if
c---------------
c
        nee=nbndS(m)
        ne=nee
        if(nee.eq.nc) go to 220
        do n=nc,nee
         if(tm(m,n,l).lt.-8.) then
          ne=n-1
          go to 220
         end if
        end do
 220    continue
c--------------
        if(m.eq.mpr) then
         print *,' CHANGETCUR1: ntrj,mpr,l,nb,ne=',ntrj,mpr,l,nb,ne
         print *,'nbndN(m),nbndS(m)=',nbndN(m),nbndS(m)
         print *,'m,l,infb(m,l)=',m,l,infb(m,l)
         print *,'m,l,infe(m,l)=',m,l,infe(m,l)
        end if
c--------------- 01-02-03 check TR4
c       if(ntrj.eq.4.and.l.eq.10) then
c        print *,' CHANGETCUR1: ntrj,m,l,nb,ne=',ntrj,m,l,nb,ne
c        print *,'nbndN(m),nbndS(m)=',nbndN(m),nbndS(m)
c        print *,'m,l,infb(m,l)=',m,l,infb(m,l)
c        print *,'m,l,infe(m,l)=',m,l,infe(m,l)
c        print *,'(tt(m,n,l),n=1,nngs)'
c        write(6,102) (tt(m,n,l),n=1,nngs)
c       end if
c--------------
c----------- 12-19-02 use max and min points
        nmin=ne
        nec=ne
        nche=ne
        dife=0.
        tmin=1000.
        if(ne.le.nc+1) go to 200
        do n=nc+1,ne
         if(tm(m,n,l).lt.tmin) then
           tmin=tm(m,n,l)
           nmin=n
         end if
        end do
        dife=tcen-tmin
c----------- 02-01-05 calculate weights
        if(infe(m,l).eq.1) then 
         k1=max(1,m-10)
         k2=min(m+10,mrgs)
         mbef=k1
         do k=k1,m
          if(infe(k,l).eq.0) mbef=k
         end do
         maft=k2
         do k=k2,m,-1
          if(infe(k,l).eq.0) maft=k
         end do
         ialph=min(m-mbef,maft-m)
         if(m-5.le.0) ialph=maft-m
         if(m+5.ge.mrgs) ialph=m-mbef
         alph=0.1*float(ialph)
c----------- 02-01-05 use weights
         if(dife.lt.0.) then
          dife=alph*tdife(l)
         else 
          dife=max(dife,alph*tdife(l)+(1.-alph)*dife)
         end if
         nec=ne
        else
         nec=nmin
        end if
c-------------- 02-02-05 add output for debug
      if(ntrj.eq.7.and.l.eq.6.and.m.eq.192) then
       print *,' debug end of BAY for dife'
       print *,'m,k1,k2,mbef,maft,ialph,alph=',
     *          m,k1,k2,mbef,maft,ialph,alph
       print *,'tdife(6),dife=',tdife(6),dife
      end if
c-------------- 
        if(dife.le.0.) go to 200
c-------------- 01-23-06 remove bug 
c       if(ne.gt.nc+ncd) then
        if(nec.gt.(nc+ncd)) then
         nche=nc+ncd 
         do n=nc+ncd+1,nec
          tm(m,n,l)=tcen-dife
         end do
        else
         nche=nec
        end if
        cc=0.
        do n=nc+1,nche
        cc=cc+wt(n)
        end do
        do n=nc+1,nche
         tm(m,n,l)=tm(m,n-1,l)-dife*wt(n)/cc
        end do
cRMY 07-14-06 begin!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cRMY DON'T DO 01-23-06 decrease!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c------------ 01-23-06 decrease changes at lu,lu+1,lu+2 in LC
c------------                               and in GS
cRMY        if(ntrj.eq.7.or.ntrj.eq.5) then
cRMY         do n=nc+1,nec
cRMY          if(l.eq.lu) tm(m,n,l)=0.25*tm(m,n,l)+0.75*tt(m,n,l)
cRMY          if(l.eq.lu+1) tm(m,n,l)=0.5*tm(m,n,l)+0.5*tt(m,n,l)
cRMY          if(l.eq.lu+2) tm(m,n,l)=0.75*tm(m,n,l)+0.25*tt(m,n,l)
cRMY         end do
cRMY        end if
c------------ 
cRMY 07-14-06 end  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 200    continue
        tdife(l)=max(dife,tdife(l))
c--------------- 02-13-06 include tdifbmax
        if(tdife(l).gt.tdifbmax(l)) tdife(l)=tdifbmax(l)
        tne=tm(m,ne,l)
        tnec=tm(m,nec,l)
c------------ 02-03-05
        if(ntrj.eq.7.and.l.eq.6.and.m.eq.192) then
          print *,'  end of BAY'
          print *,'ntrj,l,m=',ntrj,l,m
          print *,'nc,nche,cc,dife=',nc,nche,cc,dife
        end if
c------------
        if(m.eq.mpr) then
         write(6,201) m,l,difb,tdifb(l),dife,tdife(l)
 201     format('m,l,difb,tdifb(l),dife,tdife(l)=',2i7,4f7.2)
         print *,'    l,nb,nbc,nmax,nchb,nc=',l,nb,nbc,nmax,nchb,nc
         write(6,104) l,tnb,tnbc,tcen,tnb-tnbc,tnbc-tcen
 104     format('l,tnb,tnbc,tcen,tnb-tnbc,tnbc-tcen=',i7,5f7.2)
         print *,'    l,ne,nec,nmin,nche,nc=',l,ne,nec,nmin,nche,nc
         write(6,101) l,tne,tnec,tcen,tne-tnec,tnec-tcen
 101     format('l,tne,tnec,tcen,tne-tnec,tnec-tcen=',i7,5f7.2)
c
         print *,'(tt(m,n,l),n=1,nngs)'
         write(6,102) (tt(m,n,l),n=1,nngs)
         print *,'(tm(m,n,l),n=1,nngs)'
         write(6,102) (tm(m,n,l),n=1,nngs)
 102     format(10f7.2)
        end if
c---------------- 04-11-03 increase tdife in BAY
c---------------- 04-14-03 do not use this change 
c---------------- 04-21-03 use this changes for m>120
c----------------          (the northern point of LC)
c---------------- 04-25-03 increase tdife in BAY linearly from 
c                          m=75 (23N) to m=99 (25N)
c       mincr1=75
c       mincr2=99
c--------------- 04-30-03 increase tdife in BAY 
c---------------          from m=1
c       if(ntrj.eq.7) tdife(l)=max(tdifb(l),tdife(l)) 
c----------- 05-02-03 use again mincr1,mincr2
c
c--------------- 01-23-06 do not increase intensity LC
        if(ntrj.eq.17) then 
c       if(ntrj.eq.7) then 
         if(m.le.mincr1) almin=0.
         if(m.gt.mincr1.and.m.lt.mincr2) 
     *    almin=float(m-mincr1)/float(mincr2-mincr1)
         if(m.ge.mincr2) almin=1.
c----------- falk 05-05-05 decrease intensity of LC at low levels
c-----------                multiply tdifb(l) *0.5
         tdife(l)=(1.-almin)*tdife(l)+almin*max(0.5*tdifb(l),tdife(l))
        end if
c
c---------------- 04-30-03 increase tdife in GS 
c----------- falk 05-05-05 decrease intensity of GS at low levels
c-----------                multiply tdifb(l) *0.5
c
c--------------- 01-23-06 do not increase intensity GS
c       if(ntrj.eq.5) tdife(l)=max(0.5*tdifb(l),tdife(l)) 
c
       end do                 ! end loop in l
 300   continue
      end do         ! end loop in m
c
c--------------- 12-19-05 smooth tm at l=lu=5 (50m)
c---------------          between levels 30 and 75m
c--------------- 01-23-06 do not smooth 30 and 50m
      go to 321
      do m=1,mrgs
       do n=nbndN(m),nc+ncd
        if(tm(m,n,lu+1).gt.-8.) then
          tm(m,n,lu)=0.25*tm(m,n,lu)+0.375*(tm(m,n,lu-1)+
     *                              tm(m,n,lu+1))
        end if
c--------------- 12-20-05 smooth tm at l=lu-1=4 (30m)
c---------------          between levels 20 and 50m
        if(tm(m,n,lu).gt.-8.) then
          tm(m,n,lu-1)=0.5*tm(m,n,lu-1)+2./6.*tm(m,n,lu-2)+
     *                                  1./6.*tm(m,n,lu)
        end if
       end do
      end do
c--------------- 01-23-06 end 
 321  continue
c--------------- 04-08-03 add print tdifb,tdife
      print *,' mrgs,ll,ntrj=',mrgs,ll,ntrj
      print *,' (tdifb(l),l=1,ll) at the last point m=mrgs, l=1,ll'
      write(6,102) tdifb 
      print *,' (tdife(l),l=1,ll) at the last point m=mrgs, l=1,ll'
      write(6,102) tdife 
c-------------- 02-06-06 do not send for ring (ntrj=8)
      if(ntrj.ne.8) then
       do l=1,ll
        tdifbs(l)=tdifb(l)
        tdifes(l)=tdife(l)
       end do
      end if
c----------------
      lpr=13
      print *,'(tt(m,n,lpr),n=nc-5,nc+5) ntrj,mrgs,nc,lpr=',
     *          ntrj,mrgs,nc,lpr
      write(6,109) (n,n=nc-5,nc+5)
 109  format(5x,11i7)
      do m=1,mrgs,10
       write(6,110) m,(tt(m,n,lpr),n=nc-5,nc+5)
 110   format(i7,11f7.2)
      end do
      print *,'(tm(m,n,lpr),n=nc-5,nc+5) ntrj,mrgs,nc,lpr=',
     *          ntrj,mrgs,nc,lpr
      write(6,109) (n,n=nc-5,nc+5)
      do m=1,mrgs,10
       write(6,110) m,(tm(m,n,lpr),n=nc-5,nc+5)
      end do
c
c---------------- 02-04-03 print 11 last point
c
      print *,'(tt(m,n,lpr),n=nc-5,nc+5) ntrj,mrgs,nc,lpr=',
     *          ntrj,mrgs,nc,lpr
      print *,' last 11 points m'
      write(6,109) (n,n=nc-5,nc+5)
      do m=mrgs-10,mrgs
       write(6,110) m,(tt(m,n,lpr),n=nc-5,nc+5)
      end do
      print *,' last 11 points m'
      print *,'(tm(m,n,lpr),n=nc-5,nc+5) ntrj,mrgs,nc,lpr=',
     *          ntrj,mrgs,nc,lpr
      write(6,109) (n,n=nc-5,nc+5)
      do m=mrgs-10,mrgs
       write(6,110) m,(tm(m,n,lpr),n=nc-5,nc+5)
      end do
c--------------
      return
      end
c
c---------------
c
      subroutine BAYtr(xlongs,ylatgs,mmx,mrgs,step,stepn,ynp,cos30,
     *                 nbndN,nbndS,nngs,migw,mige)
      parameter(Rmin=1.0,filow=21.0)
      dimension xlongs(mmx),ylatgs(mmx),xbnd(mmx),ybnd(mmx)
      dimension nbndN(mmx),nbndS(mmx),xlon2(mmx,nngs),ylat2(mmx,nngs)
      real xconw,yconw,xcone,ycone,xnpc,ynpc
      nc=(nngs+1)/2
c----------
c---------- subr. calculate a new BAY trajectory and nbndN,nbndS
c---------- 
      print *,'BAYtr begin: migw,mige=',migw,mige
      write(6,101) mmx,mrgs,nngs,nc,step,stepn,ynp,cos30
 101  format('mmx,mrgs,nngs,nc,step,stepn,ynp,cos30',4i7,4f7.2)
c---------- 
c---------- use ynp to calculate new TR in BAY and boundary
c----------     between 2 branches xbnd,ybnd
c
c---------- 02-28-06 use looptr2
      call looptr2(xlongs,ylatgs,mmx,mrgs,step,cos30,
     *                  xconw,yconw,xcone,ycone,xnpc,ynpc)
c     call looptr(xlongs,ylatgs,mmx,mrgs,step,ynp,cos30,
c    *            xconw,yconw,xcone,ycone,xnpc,ynpc)
c
      call evenstep(xlongs,ylatgs,mmx,mrgs,step,cos30)
c
      call curvchange(xlongs,ylatgs,mmx,mrgs,Rmin)
c
      call evenstep(xlongs,ylatgs,mmx,mrgs,step,cos30)
c
      call perp(xlongs,ylatgs,xlon2,ylat2,nngs,stepn,mrgs,mmx,
     *                cos30,migw,mige)
      print *,'    new  mrgs=',mrgs
c
c------------- check dist between lines paral to GS
      call chdist(xlon2,ylat2,mmx,nngs,mrgs)
c---------- calculate nbndN; filowb crosses Cuba 
c---------- nbndN=1 for m=1,mlow and m=mup,mrgs
c
      call loopnbnd(xlon2,ylat2,mmx,mrgs,nngs,nc,nbndN,
     *           xbnd,ybnd,lbnd,filow,step,cos30,
     *           xconw,yconw,xcone,ycone,xnpc,ynpc)
c---------- send nbndS=1
      do m=1,mrgs
       nbndS(m)=nngs
      end do
      print *,' after LOOPBND'
      print *,'(nbndN(m),m=1,mrgs)'
      write(6,104) (nbndN(m),m=1,mrgs)
      print *,'(nbndS(m),m=1,mrgs)'
      write(6,104) (nbndS(m),m=1,mrgs)
 104  format(10i7)
      print *,'END of BAYtr'
      return
      end
c
c--------------
c
c---------------- falk 08-15-05 add file aladj
c----------------      12-09-05 add in subr. HPE,xlon2,ylat2
      subroutine BAYprep(tt,nbndN,xlongs,ylatgs,t12S,Zlev,mmx,nngs,nc,
     *                   HPE,xlon2,ylat2,nl,lu,ld,mrgs,cos30,aladj)
      parameter(fiorg=19.,lgs=13,tgs=12.,lsend=13,tsend=15.4,
     *          filow=21.,plonup=-83.0)
c---------------- falk 08-15-05 add file aladj
      dimension tt(mmx,nngs,nl),nbndN(mmx),xlongs(mmx),ylatgs(mmx),
     *          t12S(nl),aladj(nl),Zlev(nl)
c----------------      12-09-05 add in subr. HPE,xlon2,ylat2
      dimension HPE(mmx,nngs),xlon2(mmx,nngs),ylat2(mmx,nngs)
c-------------   02-06-06 add twc for RING
      common /warmcore/ twc(50)
c------------- 04-04-03 find new mlow,mup using filow,plonup
       plonupc=plonup*cos30
       m=1
       do while(ylatgs(m).lt.filow.and.m.le.mrgs)
        m=m+1
       end do
       mlow=m
       m=mrgs
       do while(xlongs(m).gt.plonupc.and.m.ge.1)
        m=m-1
       end do
       mup=m
       print *,'BAYprep begins: mmx,nngs,nl,mrgs,mlow,mup,lu,ld=',
     *                         mmx,nngs,nl,mrgs,mlow,mup,lu,ld
       write(6,101) nc,lgs,lsend,tgs,tsend,fiorg
 101  format('nc,lgs,lsend,tgs,tsend,fiorg=',3i7,3f7.2)
c-------------
c---------------- falk 08-15-05 add check aladj
       print *,' BAYprep: aladj(nl)'
       write(6,103) aladj
c----------------  12-09-05 uncomment print write and add 104 format
       print *,'BAY check tt(m,nbndN(m),lsend)  mlow,mup=',mlow,mup
       print *,'(nbndN(m),m=mlow,mup)'
       write(6,104) (nbndN(m),m=mlow,mup)
 104   format(10i7)
       print *,'(tt(m,nbndN(m),lsend),m=mlow,mup)'
       write(6,103) (tt(m,nbndN(m),lsend),m=mlow,mup)
c----------------  12-09-05 add write xlon2,ylat2,HPE
       print *,'(xlon2(m,nbndN(m))/cos30,m=mlow,mup)'
       write(6,103) (xlon2(m,nbndN(m))/cos30,m=mlow,mup)
       print *,'(ylat2(m,nbndN(m)),m=mlow,mup)'
       write(6,103) (ylat2(m,nbndN(m)),m=mlow,mup)
       print *,'(HPE(m,nbndN(m)),m=mlow,mup)'
       write(6,105) (HPE(m,nbndN(m)),m=mlow,mup)
 105   format(10f7.0)
c------------- 
       msend=mlow
       do m=mlow+1,mup
        ptm=tt(m,nbndN(m),lsend)
        ptm1=tt(m-1,nbndN(m-1),lsend)
        if(ptm.le.tsend.and.ptm1.gt.tsend) then
         msend=m-1
         go to 200
        end if
       end do
 200   continue
       print *,'msend=',msend
c
c----------------  12-09-05 add print 
       print *,'msend,nbndN(msend)=',msend,nbndN(msend)
       print *,'tt(msend,nbndN(msend),l)'
       write(6,103) (tt(msend,nbndN(msend),l),l=1,nl)
c----------------  12-12-05 add print 
       m=92
       print *,'center of circle: m,nbndN(m)=',m,nbndN(m)
       print *,'tt(m,nbndN(m),l)'
       write(6,103) (tt(m,nbndN(m),l),l=1,nl)
c-----------
       n=nc
       do while(HPE(mup,n).gt.5..and.n.ge.1)
        n=n-1
       end do
       nup=n+1
c-----------
       print *,'(HPE(mup,n),n=1,nc)'
       write(6,105) (HPE(mup,n),n=1,nc)
c-----------
       print *,'1-st ocean p. to the north from Cuba: mup,nup=',
     *          mup,nup
       print *,'tt(mup,nup,l)'
       write(6,103) (tt(mup,nup,l),l=1,nl)
c-----------
       print *,'bound. p. for  mup,nbndN(mup)=',mup,nbndN(mup)
       print *,'tt(mup,nbndN(mup),l)'
       write(6,103) (tt(mup,nbndN(mup),l),l=1,nl)
c----------------
       go to 400
c---------------- 02-10-06 add output for tt at lev=7 (100m)
       print *,'begin BAYprep: mup,mrgs=',mup,mrgs
       print *,'     n,(tt(m,n,7), m=mup-10,mup)'
       write(6,106) (m,m=mup-10,mup)
 106   format(5x,11i7)
       do k=1,nc
        n=nc-k+1
        write(6,107) n,(tt(m,n,7),m=mup-10,mup)
 107    format(i7,11f7.2)
       end do
c----------------
       print *,'begin BAYprep: mup,mrgs=',mup,mrgs
       print *,'     n,(tt(m,n,7),m=mup,mup+10)'
       write(6,106) (m,m=mup,mup+10)
       do k=1,nc
        n=nc-k+1
        write(6,107) n,(tt(m,n,7),m=mup,mup+10)
       end do
 400   continue
c----------------
c-------------   05-31-06 add using real data for warm core in LC
       read(31,311) idt
 311   format(i1)
       print *,'if idt=1 use real data for warm core in LC: idt=',idt
cRMY 10/25/06 add ability to use fraction of twc if not using real data 
       read(31,313) wlc
 313   format(f3.1)
       print*, 'weight for LC if not using real data: wlc='
       write(6,313) wlc
cRMY 10/25/06 end 1 of 2-----------------------------------------------
       if(idt.eq.1) then
cRMY 12/28/06 allow partial use of real data for LC profile
cRMY        read(31,312) (twc(l),l=1,nl)
        read(31,314) nlax
 314    format(i2)
        read(31,312) (twc(l),l=1,nlax)
 312    format(10f7.2)
        do l=nlax+1,nl
         twc(l)=tt(msend,nbndN(msend),l)
        end do
cRMY 12/28/06 end 1 of 4-----------------------------------
c-------------  05-31-06 add printing twc
        print *,' twc(l), l=1,nl'
        write(6,312) (twc(l),l=1,nl)
       else
c-------------   02-06-06 add twc for RING
        do l=1,nl
         twc(l)=tt(msend,nbndN(msend),l)
        end do
       end if
c----------------
       do m=msend,mup
        n=nbndN(m)
c----------------  12-12-05 send from l=1,ld (it was l=lu,ld)
        do l=1,ld
c----------------  12-14-05 send from nbndN(m) to nc
c----------------  05-31-06 use twc
c        ttt=tt(msend,nbndN(msend),l)
         ttt=twc(l)
c-------------   02-06-06 smooth for idt=1 between twc tt(msend,..)
         if(idt.eq.1.and.m.lt.msend+10) then
          alidt=float(m-msend)/10.
          ttt=alidt*twc(l)+(1.-alidt)*tt(msend,nbndN(msend),l)
         end if
c----------------  coef. decreases from 1 to 0 when k=n,nc
         do k=n,nc
cRMY 10/25/06 use wlc like Falkovich used wwr on 5/18/06 in RINGprep
cRMY          tt(m,k,l)=(float(nc-k)*ttt+
cRMY     *               float(k-n)*tt(m,k,l))/float(nc-n)
          allc=wlc*float(nc-k)/float(nc-n)
          tt(m,k,l)=allc*ttt+(1.-allc)*tt(m,k,l)
cRMY 10/25/06 end 2 of 2--------------------------------------------
         end do
        end do
       end do
c-------------  05-31-06 add printing twc
       print *,' (twc(l), l=1,nl)'
       write(6,312) (twc(l), l=1,nl)
c----------------  12-15-05 smooth between msend-10,msend
c----------------  03-13-06 make smoothing along n=const
       nms=nbndN(msend)
       ms10=max(msend-10,1)
       do m=ms10,msend-1
        n=nbndN(m)
        do l=1,ld
         if(n.ge.nms) then
          do k=n,nc
           ttt=tt(msend,k,l)
           alf=float(m-ms10)/float(msend-ms10)
           tt(m,k,l)=alf*ttt+(1.-alf)*tt(m,k,l)
          end do
         else
          do k=nms,nc
           ttt=tt(msend,k,l)
           alf=float(m-ms10)/float(msend-ms10)
           tt(m,k,l)=alf*ttt+(1.-alf)*tt(m,k,l)
          end do
          do k=n,nms-1
           alf1=float(k-n)/float(nms-n)
           tt(m,k,l)=alf1*tt(m,nms,l)+(1.-alf1)*tt(m,k,l)
          end do
         end if
        end do
       end do
c----------------  12-15-05 smooth between mup,mup+10
c----------------  03-13-06 make smoothing along n=const
       mup10=min(mup+10,mrgs)
       if((mup10-mup).gt.1) then
c---------------
       nup=nbndN(mup)
       do m=mup+1,mup10
        n=nbndN(m)
        do l=1,ld
         if(n.ge.nup) then
          do k=n,nc
           ttt=tt(mup,k,l)
           alf=float(mup10-m)/float(mup10-mup)
           tt(m,k,l)=alf*ttt+(1.-alf)*tt(m,k,l)
          end do
         else
          do k=nup,nc
           ttt=tt(mup,k,l)
           alf=float(mup10-m)/float(mup10-mup)
           tt(m,k,l)=alf*ttt+(1.-alf)*tt(m,k,l)
          end do
          do k=n,nup-1
           alf1=float(k-n)/float(nup-n)
           tt(m,k,l)=alf1*tt(m,nup,l)+(1.-alf1)*tt(m,k,l)
          end do
         end if
        end do
       end do
c---------------
       end if
c--------------- 02-10-06 check smoothing
       print *,' check after smoothing: mup,nup=',mup,nup
       print *,'(tt(m,nup,7),m=mup-10,mrgs)'
       write(6,108) (tt(m,nup,7),m=mup-10,mrgs)
       print *,'(tt(m,nc-10,7),m=mup-10,mrgs)'
       write(6,108) (tt(m,nc-10,7),m=mup-10,mrgs)
       print *,'(tt(mrgs-5,n,7),n=1,nc)' 
       write(6,108) (tt(mrgs-5,n,7),n=1,nc)
 108   format(10f7.2)
c
c----------- falk 08-15-05 do not use t12S
       go to 299
c-----------
c
c----------- 01-31-03 send t=12 at z=400 along LC
c
c-------------- find m=m12S where tgs close to t(m,nc,lgs)
       do m=mrgs,mlow,-1
        do n=2,nngs
         ptn=tt(m,n,lgs)
         ptn1=tt(m,n-1,lgs)
         if(ptn.lt.tgs.and.ptn1.ge.tgs) then
          adif=tt(m,n-1,lgs)-tt(m,n,lgs)
          if(adif.lt.0.01) then
           alint=1.
          else
           alint=(tt(m,n-1,lgs)-tgs)/adif
          end if
          do l=1,nl
           t12S(l)=alint*tt(m,n,l)+(1.-alint)*tt(m,n-1,l)
          end do
          m12S=m
          go to 300
         end if
        end do
       end do
 300   continue
c
c----------- send t12S to current center for m>m12S
c------------ 01-31-03 use lu,ld instead of lc1,lc2
       do m=m12S,mrgs
        do l=lu,ld
         tt(m,nc,l)=t12S(l)
        end do
       end do
c-------- find and send t=12 to points mlow<m<m12S
c---------- 04-04-03 do not use fiorg; send t12 up to m=1
       do m=m12S-1,1,-1
        do n=2,nngs
         ptn=tt(m,n,lgs)
         ptn1=tt(m,n-1,lgs)
         if(ptn.lt.tgs.and.ptn1.ge.tgs) then
          adif=tt(m,n-1,lgs)-tt(m,n,lgs)
          if(adif.lt.0.01) then
           alint=1.
          else
           alint=(tt(m,n-1,lgs)-tgs)/adif
          end if
c------------ 01-31-03 use lu,ld instead of lc1,lc2
          do l=lu,ld
           tt(m,nc,l)=alint*tt(m,n,l)+(1.-alint)*tt(m,n-1,l)
          end do
          go to 250
         end if
        end do
c------- if there is no t=12 send from previous point m+1
c------------ 01-31-03 use lu,ld instead of lc1,lc2
c------------ 04-04-03 change in tt(m+1,n,l) n to nc
        do l=lu,ld
         tt(m,nc,l)=tt(m+1,nc,l)
        end do
 250    continue
       end do
c-----------
c----------- falk 08-15-05 do not use t12S, use aladj to send t=12
 299   continue
c-----------
c--------------
      print *,' LC  before correction with aladj'
      print *,'l=7(100m), l=10(200), l=13(400), l=15(600)' 
      print *,'                      l=17(800), l=19(1000)' 
      print *,'lon/cos30, lat, tt at  l=7 l=10 l=13 l=15 l=17 l=19'
c--------------
      do m=1,mrgs,5
       write(6,221) m,xlongs(m)/cos30,ylatgs(m),tt(m,nc,7),
     *       tt(m,nc,10),tt(m,nc,13),tt(m,nc,15),tt(m,nc,17),
     *       tt(m,nc,19)
 221   format(1x,i7,2f7.2,2x,6f7.2)
      end do
c-------------- falk 08-15-05 to connect with TR2 use madj
      madj=20
      do m=1,mrgs
       if(m.gt.madj) then
        alint=1.
       else
        alint=float(m-1)/float(madj-1)
       end if
       dtt=tt(m,nc,lgs)-tgs
       do l=1,nl
        tt(m,nc,l)=tt(m,nc,l)-aladj(l)*dtt*alint
       end do
      end do
c--------------
      print *,' LC  after correction with aladj'
      print *,'l=7(100m), l=10(200), l=13(400), l=15(600)' 
      print *,'                      l=17(800), l=19(1000)' 
      print *,'lon/cos30, lat, tt at  l=7 l=10 l=13 l=15 l=17 l=19'
c--------------
      do m=1,mrgs,5
       write(6,221) m,xlongs(m)/cos30,ylatgs(m),tt(m,nc,7),
     *       tt(m,nc,10),tt(m,nc,13),tt(m,nc,15),tt(m,nc,17),
     *       tt(m,nc,19)
      end do
c--------------
c--------------
       print *,'mlow,msend,mup,mrgs,lsend=',mlow,msend,mup,mrgs,lsend
       write(6,609) tt(mlow,nbndN(mlow),lsend),tsend
 609   format('tt(mlow,nbndN(mlow),lsend),tsend=',2f7.2)
       write(6,610) tt(msend,nbndN(msend),lsend),tsend
 610   format('tt(msend,nbndN(msend),lsend),tsend=',2f7.2)
c      print *,' t12S(l)     m12S=',m12S
c      write(6,103) t12S
 103   format(10f7.2)
c
c      print *,' BAY after send t=12'
c      print *,' l=10(200m), l=13(400m),l=19(1000m)' 
c      print *,'tt  lon/cos30  lat  l=1 l=10 l=13 l=19 l=nl'
c      do m=1,mrgs,5
c       write(6,221) m,xlongs(m)/cos30,ylatgs(m),tt(m,nc,1),
c    *        tt(m,nc,10),tt(m,nc,13),tt(m,nc,19),tt(m,nc,nl)
c221    format(1x,i7,2f8.2,5f7.2)
c      end do
c---------------- 02-10-06 add output for tt at lev=7 (100m)
       go to 401
       print *,'end BAYprep: mup,mrgs=',mup,mrgs
       print *,'     n,(tt(m,n,7), m=mup-10,mup)'
       write(6,106) (m,m=mup-10,mup)
       do k=1,nc
        n=nc-k+1
        write(6,107) n,(tt(m,n,7), m=mup-10,mup)
       end do
c--------------
       print *,'end BAYprep: mup,mrgs=',mup,mrgs
       print *,'     n,(tt(m,n,7),m=mup,mup+10)'
       write(6,106) (m,m=mup,mup+10)
       do k=1,nc
        n=nc-k+1
        write(6,107) n,(tt(m,n,7),m=mup,mup+10)
       end do
 401   continue
c----------------
c------------
       call stabil(tt,Zlev,mmx,mrgs,nngs,nl)
      return
      end
c
c------------
c
      subroutine mixLevGDEM(TB1,SB1,XI,YI,Zlev,H,FSM,
     *                      t,s,im,jm,nl,mmx)
c-------- create mixture from Levitus and GDEM data
c-------- Levitus data t,s have -9 under bottom Levitus topography
c-------- H is a model topography,FSM is a model mask
c-------- GDEM data defined everywhere
c
      real XI(IM),YI(JM)
      DIMENSION H(IM,JM),TB1(IM,JM,nl),SB1(IM,JM,nl),
     *          FSM(IM,JM),Zlev(nl)
      dimension t(im,jm,nl),s(im,jm,nl)
      dimension tr(mmx),sr(mmx)
c
c---------- 06-05-03 use only GDEM data; send t,s=GDEM and go to 100
c
      do l=1,nl
       do j=1,jm
        do i=1,im
         t(i,j,l)=TB1(i,j,l)
         s(i,j,l)=SB1(i,j,l)
        end do
       end do
      end do
      go to 100
c---------- 03-20-03 remove large grad in Levitus data
c----------          between 67--63W near south bnd j0=10 (11.51N)
      j0=10
      do l=1,nl
       do j=1,j0
        do i=164,187
         t(i,j,l)=t(i,j0,l)
         s(i,j,l)=s(i,j0,l)
        end do
       end do
      end do
c
c---------- send over land and under bottom GDEM data
c
      do l=1,nl
       do j=1,jm
        do i=1,im
         if(t(i,j,l).lt.-8.) t(i,j,l)=TB1(i,j,l)
         if(s(i,j,l).lt.-8.) s(i,j,l)=SB1(i,j,l)
        end do
       end do
      end do
c
c------------ 04-17-03 do not use GDEM data
c             04-17-03a       use GDEM again
c     go to 100
c
c------------ 03-04-03 use GDEM data only for lat>23
c------------          and lon>-82 and sponge area 2 and 5 deg
c------------          
      ymix1=23.0
      ymix2=28.0
      xmix1=-82.0     
      xmix2=-80.0     
      call findi(XI,im,xmix1,imix1)
      call findi(XI,im,xmix2,imix2)
      call findj(YI,jm,ymix1,jmix1)
      call findj(YI,jm,ymix2,jmix2)
      print *,' mixLevGDEM'
      print *,' use GDEM data for >imix2, j>jmix2'
      write(6,296) xmix1,xmix2,imix1,imix2,im
      write(6,295) ymix1,ymix2,jmix1,jmix2,jm
 296  format('bnd of mix data xmix1,xmix2,imix1,imix2,im=',
     *        2f7.2,3i7)
 295  format('bnd of mix data ymix1,ymix2,jmix1,jmix2,jm=',
     *        2f7.2,3i7)
      do i=1,im
       gamix=float(i-imix1)/(imix2-imix1)
       if(i.lt.imix1) gamix=0.
       if(i.gt.imix2) gamix=1.
       do j=1,jm
        betmix=float(j-jmix1)/float(jmix2-jmix1)
        if(j.lt.jmix1) betmix=0.
        if(j.gt.jmix2) betmix=1.
        delmix=min(betmix,gamix)
        do l=1,nl
         t(i,j,l)=(1.-delmix)*t(i,j,l)+delmix*TB1(i,j,l)
         s(i,j,l)=(1.-delmix)*s(i,j,l)+delmix*SB1(i,j,l)
        end do
       end do
      end do
c
 100  continue
c
c------- 03-24-03 remove short waves from t,s to the south from 15N
c-------     j0=31 (15.02N)
      j0=31
      do l=1,nl
       do j=1,j0
        do i=1,im
         tr(i)=t(i,j,l)
         sr(i)=s(i,j,l)
        end do
        call filtr1D(tr,mmx,1,im,11)
        call filtr1D(sr,mmx,1,im,11)
        al=float(j0-j)/float(j0-1)
        do i=1,im
         t(i,j,l)=al*tr(i)+(1.-al)*t(i,j,l)
         s(i,j,l)=al*sr(i)+(1.-al)*s(i,j,l)
        end do
       end do
       do i=1,im
        do j=1,j0
         tr(j)=t(i,j,l)
         sr(j)=s(i,j,l)
        end do
        call filtr1D(tr,mmx,1,j0,3)
        call filtr1D(sr,mmx,1,j0,3)
        do j=1,j0
         t(i,j,l)=tr(j)
         s(i,j,l)=sr(j)
        end do
       end do
      end do
c
c------------ 03-04-03 smooth mixed fields
c                       smooth along boundaries
      do l=1,nl
        j0=1
        do i=1,im
         tr(i)=t(i,j0,l)
         sr(i)=s(i,j0,l)
        end do
        call filtr1D(tr,mmx,1,im,11)
        call filtr1D(sr,mmx,1,im,11)
        do i=1,im
         t(i,j0,l)=tr(i)
         s(i,j0,l)=sr(i)
        end do
        j0=jm
        do i=1,im
         tr(i)=t(i,j0,l)
         sr(i)=s(i,j0,l)
        end do
        call filtr1D(tr,mmx,1,im,11)
        call filtr1D(sr,mmx,1,im,11)
        do i=1,im
         t(i,j0,l)=tr(i)
         s(i,j0,l)=sr(i)
        end do
        i0=1
        do j=1,jm
         tr(j)=t(i0,j,l)
         sr(j)=s(i0,j,l)
        end do
        call filtr1D(tr,mmx,1,jm,11)
        call filtr1D(sr,mmx,1,jm,11)
        do j=1,jm
         t(i0,j,l)=tr(j)
         s(i0,j,l)=sr(j)
        end do
        i0=im
        do j=1,jm
         tr(j)=t(i0,j,l)
         sr(j)=s(i0,j,l)
        end do
        call filtr1D(tr,mmx,1,jm,11)
        call filtr1D(sr,mmx,1,jm,11)
        do j=1,jm
         t(i0,j,l)=tr(j)
         s(i0,j,l)=sr(j)
        end do
      end do
c
c--------- 04-25-03 smooth  fields near north shore of BAY
c---------          i0=87 (82W) j0=103 (27N) j1=115 (29N)
      xsm0=-82.
      ysm0=27.
      ysm1=29.
c     i0=87
c     j0=103
c     j1=115
      call findi(XI,im,xsm0,i0)
      call findj(YI,jm,ysm0,j0)
      call findj(YI,jm,ysm1,j1)
      print *,' smooth  fields near north shore of BAY'
      print *,'i0,j0,j1=',i0,j0,j1
      do l=1,nl
       do j=j0,jm
        do i=1,i0
         tr(i)=t(i,j,l)
         sr(i)=s(i,j,l)
        end do
        call filtr1D(tr,mmx,1,i0,11)
        call filtr1D(sr,mmx,1,i0,11)
        al=1.
        if(j.ge.j0.and.j.le.j1) al=float(j-j0)/float(j1-j0)
        do i=1,i0
         t(i,j,l)=al*tr(i)+(1.-al)*t(i,j,l)
         s(i,j,l)=al*sr(i)+(1.-al)*s(i,j,l)
        end do
       end do
       do i=1,i0
        do j=j0,jm
         tr(j)=t(i,j,l)
         sr(j)=s(i,j,l)
        end do
        call filtr1D(tr,mmx,j0,jm,11)
        call filtr1D(sr,mmx,j0,jm,11)
        do j=j0,jm
         t(i,j,l)=tr(j)
         s(i,j,l)=sr(j)
        end do
       end do
      end do
c--------- 04-28-03 smooth  fields to south from Yucatan
c---------    i0=77 (84W) i1=87 (82W) j0=49 (18N) j1=61 (20N)
      xsm0=-84.
      xsm1=-82.
      ysm0=18.
      ysm1=20.
c     i0=77
c     i1=87
c     j0=61
c     j1=49
      call findi(XI,im,xsm0,i0)
      call findi(XI,im,xsm1,i1)
      call findj(YI,jm,ysm0,j0)
      call findj(YI,jm,ysm1,j1)
      print *,' smooth  fields to south from Yucatan'
      print *,'i0,i1,j0,j1=',i0,i1,j0,j1
      do l=1,nl
       do j=1,j1
        do i=1,i1
         tr(i)=t(i,j,l)
         sr(i)=s(i,j,l)
        end do
        call filtr1D(tr,mmx,1,i1,11)
        call filtr1D(sr,mmx,1,i1,11)
        al=1.
        if(j.ge.j0.and.j.le.j1) al=float(j1-j)/float(j1-j0)
        do i=1,i1
         t(i,j,l)=al*tr(i)+(1.-al)*t(i,j,l)
         s(i,j,l)=al*sr(i)+(1.-al)*s(i,j,l)
        end do
       end do
       do i=1,i1
        do j=1,j1
         tr(j)=t(i,j,l)
         sr(j)=s(i,j,l)
        end do
        call filtr1D(tr,mmx,1,j1,11)
        call filtr1D(sr,mmx,1,j1,11)
        al=1.
        if(i.ge.i0.and.i.le.i1) al=float(i1-i)/float(i1-i0)
        do j=1,j1
         t(i,j,l)=al*tr(j)+(1.-al)*t(i,j,l)
         s(i,j,l)=al*sr(j)+(1.-al)*s(i,j,l)
        end do
       end do
      end do
c
c--------- 03-04-03 smooth  fields over the entire region
c
      do l=1,nl
       do j=1,jm
        do i=1,im
         tr(i)=t(i,j,l)
         sr(i)=s(i,j,l)
        end do
        call filtr1D(tr,mmx,1,im,11)
        call filtr1D(sr,mmx,1,im,11)
        do i=1,im
         t(i,j,l)=tr(i)
         s(i,j,l)=sr(i)
        end do
       end do
       do i=1,im
        do j=1,jm
         tr(j)=t(i,j,l)
         sr(j)=s(i,j,l)
        end do
        call filtr1D(tr,mmx,1,jm,11)
        call filtr1D(sr,mmx,1,jm,11)
        do j=1,jm
         t(i,j,l)=tr(j)
         s(i,j,l)=sr(j)
        end do
       end do
      end do
      return
      end
c
c------- 06-05-03 use GDEM in MIXED layer over entire region
c
      subroutine mixLevGDEMm(TB1,SB1,XI,YI,Zlev,H,FSM,
     *                      t,s,im,jm,nl,mmx)
c-------- create mixture from Levitus and GDEM data
c-------- Levitus data t,s have -9 under bottom Levitus topography
c-------- H is a model topography,FSM is a model mask
c-------- GDEM data defined everywhere
c
      real XI(IM),YI(JM)
      DIMENSION H(IM,JM),TB1(IM,JM,nl),SB1(IM,JM,nl),
     *          FSM(IM,JM),Zlev(nl)
      dimension t(im,jm,nl),s(im,jm,nl)
      dimension tr(mmx),sr(mmx)
      dimension told(nl),tnew(nl)
c-------- falk 08-22-05 read input (mig_data)
c--------               and use needed climate
      read(5,100) mig_data
 100  format(i1)
      print *,'mix_data:   mig_data=',mig_data
c
c--------- read gdem3 monthly climate data with 0.25deg resolution
c--------- these data have -9. under bottom gdem3 topography
c
      if(mig_data.eq.2) then
       read(82,104) t
       read(82,104) s
 104   format(10f7.3)
      end if
c
c--------- read Levitus monthly climate data with 0.25deg resolution
c--------- these data have -9. under bottom Levitus topography
c
      if(mig_data.eq.3) then
       read(24,104) t
       read(24,104) s
      end if
c
c---------- 
c
      do l=1,nl
       do j=1,jm
        do i=1,im
         if(mig_data.eq.1) then
          t(i,j,l)=TB1(i,j,l)
          s(i,j,l)=SB1(i,j,l)
         else
c---------- send over land and under bottom GDEM data
          if(t(i,j,l).lt.-8.) t(i,j,l)=TB1(i,j,l)
          if(s(i,j,l).lt.-8.) s(i,j,l)=SB1(i,j,l)
         end if
        end do
       end do
      end do
c
c------------ Levitus climate has unstable temp
c
      call stabil(t,Zlev,im,im,jm,nl)
c
c---------- 06-05-03 use GDEM in mixed layer and relax to Levitus
c---------- 03-02-04 do not use GDEM data in mixed layer
c
      go to 300
c-------- specify lm=5 (50m) to use GDEM mixed layer
      lm=5
      ip=249
      jp=195
c
      do j=1,jm
       do i=1,im
        do l=1,nl
         told(l)=t(i,j,l)
         tnew(l)=t(i,j,l)
        end do
        DTS=TB1(i,j,lm)-t(i,j,lm)
        tmold=told(lm)
        tm=TB1(i,j,lm)
        do l=1,lm
         tnew(l)=TB1(i,j,l)
        end do
        n=lm
        if(DTS.gt.0.) then
         DM=Zlev(lm)+DTS*25.
         do while(Zlev(n).le.DM.and.n.lt.nl-1)
          n=n+1
         end do
         tb=told(n)
         dtb=max(0.1,tmold-tb)
         constgr=(tm-tb)/dtb
         do l=lm,n-1
          dtgr=told(l+1)-told(l)
          tnew(l+1)=tnew(l)+constgr*dtgr
         end do
        end if
        do l=1,n
         t(i,j,l)=tnew(l) 
        end do
c----------------
        if(i.eq.ip.and.j.eq.jp) then
         print *,'mixLevGDEMm: im,jm,nl,lm,n=',im,jm,nl,lm,n
         write(6,101) ip,jp,Zlev(lm),XI(ip),YI(jp)
 101     format('ip,jp,Zlev(lm),XI(ip),YI(jp)',2i7,f7.0,2f7.2)
         print *,'   told Levitus temp'
         write(6,102) told
 102     format(10f7.2)
         print *,'tnew mixed GDEM (in mixed layer) and Levitus lower' 
         write(6,102) tnew
         print *,'  GDEM temp (TB1(ip,jp,l),l=1,nl)'
         write(6,102) (TB1(ip,jp,l),l=1,nl)
         write(6,103) DTS,DM,tm,tmold,tb,dtb,constgr
 103     format('DTS,DM,tm,tmold,tb,dtb,constgr',f7.2,f7.0,5f7.2)
        end if
c----------------
       end do
      end do
c---------- 03-02-04 end changes
c
 300  continue
c
c---------- 03-20-03 remove large grad in Levitus data
c----------          between 67--63W near south bnd j0=10 (11.51N)
      j0=10
      do l=1,nl
       do j=1,j0
        do i=164,187
         t(i,j,l)=t(i,j0,l)
         s(i,j,l)=s(i,j0,l)
        end do
       end do
      end do
c
c------- 03-24-03 remove short waves from t,s to the south from 15N
c-------     j0=31 (15.02N)
      j0=31
      do l=1,nl
       do j=1,j0
        do i=1,im
         tr(i)=t(i,j,l)
         sr(i)=s(i,j,l)
        end do
        call filtr1D(tr,mmx,1,im,11)
        call filtr1D(sr,mmx,1,im,11)
        al=float(j0-j)/float(j0-1)
        do i=1,im
         t(i,j,l)=al*tr(i)+(1.-al)*t(i,j,l)
         s(i,j,l)=al*sr(i)+(1.-al)*s(i,j,l)
        end do
       end do
       do i=1,im
        do j=1,j0
         tr(j)=t(i,j,l)
         sr(j)=s(i,j,l)
        end do
        call filtr1D(tr,mmx,1,j0,3)
        call filtr1D(sr,mmx,1,j0,3)
        do j=1,j0
         t(i,j,l)=tr(j)
         s(i,j,l)=sr(j)
        end do
       end do
      end do
c
c------------ 03-04-03 smooth mixed fields
c                       smooth along boundaries
      do l=1,nl
        j0=1
        do i=1,im
         tr(i)=t(i,j0,l)
         sr(i)=s(i,j0,l)
        end do
        call filtr1D(tr,mmx,1,im,11)
        call filtr1D(sr,mmx,1,im,11)
        do i=1,im
         t(i,j0,l)=tr(i)
         s(i,j0,l)=sr(i)
        end do
        j0=jm
        do i=1,im
         tr(i)=t(i,j0,l)
         sr(i)=s(i,j0,l)
        end do
        call filtr1D(tr,mmx,1,im,11)
        call filtr1D(sr,mmx,1,im,11)
        do i=1,im
         t(i,j0,l)=tr(i)
         s(i,j0,l)=sr(i)
        end do
        i0=1
        do j=1,jm
         tr(j)=t(i0,j,l)
         sr(j)=s(i0,j,l)
        end do
        call filtr1D(tr,mmx,1,jm,11)
        call filtr1D(sr,mmx,1,jm,11)
        do j=1,jm
         t(i0,j,l)=tr(j)
         s(i0,j,l)=sr(j)
        end do
        i0=im
        do j=1,jm
         tr(j)=t(i0,j,l)
         sr(j)=s(i0,j,l)
        end do
        call filtr1D(tr,mmx,1,jm,11)
        call filtr1D(sr,mmx,1,jm,11)
        do j=1,jm
         t(i0,j,l)=tr(j)
         s(i0,j,l)=sr(j)
        end do
      end do
c
c--------- 04-25-03 smooth  fields near north shore of BAY
c---------          i0=87 (82W) j0=103 (27N) j1=115 (29N)
      xsm0=-82.
      ysm0=27.
      ysm1=29.
c     i0=87
c     j0=103
c     j1=115
      call findi(XI,im,xsm0,i0)
      call findj(YI,jm,ysm0,j0)
      call findj(YI,jm,ysm1,j1)
      print *,' smooth  fields near north shore of BAY'
      print *,'i0,j0,j1=',i0,j0,j1
      do l=1,nl
       do j=j0,jm
        do i=1,i0
         tr(i)=t(i,j,l)
         sr(i)=s(i,j,l)
        end do
        call filtr1D(tr,mmx,1,i0,11)
        call filtr1D(sr,mmx,1,i0,11)
        al=1.
        if(j.ge.j0.and.j.le.j1) al=float(j-j0)/float(j1-j0)
        do i=1,i0
         t(i,j,l)=al*tr(i)+(1.-al)*t(i,j,l)
         s(i,j,l)=al*sr(i)+(1.-al)*s(i,j,l)
        end do
       end do
       do i=1,i0
        do j=j0,jm
         tr(j)=t(i,j,l)
         sr(j)=s(i,j,l)
        end do
        call filtr1D(tr,mmx,j0,jm,11)
        call filtr1D(sr,mmx,j0,jm,11)
        do j=j0,jm
         t(i,j,l)=tr(j)
         s(i,j,l)=sr(j)
        end do
       end do
      end do
c--------- 04-28-03 smooth  fields to south from Yucatan
c---------    i0=77 (84W) i1=87 (82W) j0=49 (18N) j1=61 (20N)
      xsm0=-84.
      xsm1=-82.
      ysm0=18.
      ysm1=20.
c     i0=77
c     i1=87
c     j0=61
c     j1=49
      call findi(XI,im,xsm0,i0)
      call findi(XI,im,xsm1,i1)
      call findj(YI,jm,ysm0,j0)
      call findj(YI,jm,ysm1,j1)
      print *,' smooth  fields to south from Yucatan'
      print *,'i0,i1,j0,j1=',i0,i1,j0,j1
      do l=1,nl
       do j=1,j1
        do i=1,i1
         tr(i)=t(i,j,l)
         sr(i)=s(i,j,l)
        end do
        call filtr1D(tr,mmx,1,i1,11)
        call filtr1D(sr,mmx,1,i1,11)
        al=1.
        if(j.ge.j0.and.j.le.j1) al=float(j1-j)/float(j1-j0)
        do i=1,i1
         t(i,j,l)=al*tr(i)+(1.-al)*t(i,j,l)
         s(i,j,l)=al*sr(i)+(1.-al)*s(i,j,l)
        end do
       end do
       do i=1,i1
        do j=1,j1
         tr(j)=t(i,j,l)
         sr(j)=s(i,j,l)
        end do
        call filtr1D(tr,mmx,1,j1,11)
        call filtr1D(sr,mmx,1,j1,11)
        al=1.
        if(i.ge.i0.and.i.le.i1) al=float(i1-i)/float(i1-i0)
        do j=1,j1
         t(i,j,l)=al*tr(j)+(1.-al)*t(i,j,l)
         s(i,j,l)=al*sr(j)+(1.-al)*s(i,j,l)
        end do
       end do
      end do
c
c--------- 03-04-03 smooth  fields over the entire region
c
      do l=1,nl
       do j=1,jm
        do i=1,im
         tr(i)=t(i,j,l)
         sr(i)=s(i,j,l)
        end do
        call filtr1D(tr,mmx,1,im,11)
        call filtr1D(sr,mmx,1,im,11)
        do i=1,im
         t(i,j,l)=tr(i)
         s(i,j,l)=sr(i)
        end do
       end do
       do i=1,im
        do j=1,jm
         tr(j)=t(i,j,l)
         sr(j)=s(i,j,l)
        end do
        call filtr1D(tr,mmx,1,jm,11)
        call filtr1D(sr,mmx,1,jm,11)
        do j=1,jm
         t(i,j,l)=tr(j)
         s(i,j,l)=sr(j)
        end do
       end do
      end do
      return
      end
c
c---------------------------
c
      subroutine changesal(ss,sm,mmx,nngs,nl,mrgs,nc,ncd,ld)
      dimension ss(mmx,nngs,nl),sm(mmx,nngs,nl)
      dimension wsal(10),ws(60)
      data wsal/1.6,1.4,0.9,0.35,0.30,0.20,0.15,0.10,0.00,0.00/
c-------------
      print *,'changesal: mmx,nngs,nl,mrgs,nc,ncd,ld=',
     *                    mmx,nngs,nl,mrgs,nc,ncd,ld
c-------------
c
      m0=100
      bb=0.
      do i=1,ncd
       bb=bb+wsal(i)
      end do
      do n1=nc-ncd,nc-1
       n=(nc-1)-n1+(nc-ncd)
       i=nc-n
       ws(n)=wsal(i)/bb*0.5
      end do
      do n=nc+1,nc+ncd
       i=n-nc
       ws(n)=wsal(i)/bb*0.5
      end do
      ws(nc)=0.
c-------------
      write(6,100) wsal
 100  format(' changesal:  wsal=',/5x,10f7.2)
      print *,' (ws(n),n=nc-ncd,nc+ncd)'
      write(6,141) (ws(n),n=nc-ncd,nc+ncd)
 141  format(11f7.2)
c-------------
c------------- send ss to sm
       do m=1,mrgs
       do n=1,nngs
       do l=1,nl
        sm(m,n,l)=ss(m,n,l)
       end do
       end do
       end do
c
      do m=1,mrgs
       do l=1,ld
        if(ss(m,nc,l).lt.-8.) go to 200
        nb=1
        n=nc
        do while(ss(m,n,l).gt.-8..and.n.gt.1)
         n=n-1
        end do
        nb=n+1
        ne=nngs
        n=nc
        do while(ss(m,n,l).gt.-8..and.n.lt.nngs)
         n=n+1
        end do
        ne=n-1
        if(nb.eq.ne) go to 200
        nmax=max(nb,nc-ncd)
        nmin=min(ne,nc+ncd)
c------------  find max of salinity for nmax <= n <= nc-1
c------------  nmaxs can be equal nc or nmins can be equal nc
        nmaxs=nc
        smax=0.
        if(nmax.eq.nc) go to 201
        do n=nmax,nc-1
         if(ss(m,n,l).gt.smax) then
          nmaxs=n
          smax=ss(m,n,l)
         end if
        end do
 201    continue
c
c------------  find min of salinity for nmin => n => nc+1
c
        nmins=nc
        smin=10000.
        if(nmin.eq.nc) go to 202
        do n=nc+1,nmin
         if(ss(m,n,l).lt.smin) then
          nmins=n
          smin=ss(m,n,l)
         end if
        end do
 202    continue
c
        smax=ss(m,nmaxs,l)
        smin=ss(m,nmins,l)
        smnc=ss(m,nc,l)
        sdif=smax-smin
c--------------------
        if(m.eq.m0) then
         print *,'m0,l,nb,ne,nmax,nmin=',m0,l,nb,ne,nmax,nmin
         print *,'l,nmaxs,nmins,smax,smin,smnc,sdif'
         write(6,106) l,nmaxs,nmins,smax,smin,smnc,sdif
 106     format(3i7,4f7.2)
        end if
c--------------------
        if(sdif.le.0) go to 300
        cc=0.
        if(nmaxs.eq.nc) go to 102
        do n=nmaxs+1,nc
         cc=cc+ws(n-1)
        end do
 102    continue
        if(nmins.eq.nc) go to 103
        do n=nc+1,nmins
         cc=cc+ws(n)
        end do
 103    continue
        if(cc.eq.0.) go to 300
        if(nmaxs.eq.nc) go to 302
        do n=nmaxs+1,nc
         sm(m,n,l)=sm(m,n-1,l)-sdif*ws(n-1)/cc
        end do
 302    continue
        if(nmins.eq.nc) go to 300
        do n=nc+1,nmins
         sm(m,n,l)=sm(m,n-1,l)-sdif*ws(n)/cc
        end do
 300    continue
       end do              ! end loop in l
 200   continue
      end do          ! end loop in m
c-------------------
      print *,'l,(ss(m0,n,l),n=nc-5,nc+5)   m0=',m0
      write(6,107) (n,n=nc-5,nc+5)
 107  format(5x,11i7)
      do l=1,nl
       write(6,105) l,(ss(m0,n,l),n=nc-5,nc+5)
 105   format(i7,11f7.2)
      end do
c
      print *,'l,(sm(m0,n,l),n=nc-5,nc+5)   m0=',m0
      write(6,107) (n,n=nc-5,nc+5)
      do l=1,nl
       write(6,105) l,(sm(m0,n,l),n=nc-5,nc+5)
      end do
c-------------------
      return
      end
c
c-------------------
c
        subroutine stabil(t,zlev,imx,im,jm,ll)
        dimension t(imx,jm,ll),zlev(ll)
c
c-------- check and remove all unstable points
c                            in temperature t
          do i=1,im
          do j=1,jm
c
          do l=1,ll-1
           if(t(i,j,l+1).lt.-8.or.t(i,j,l).lt.-8.) go to 125
c
           if((t(i,j,l+1)-t(i,j,l)).gt.0.) then
            t(i,j,l+1)=t(i,j,l)
           end if
          end do
c---------------------end loop in l
 125      continue
          end do
          end do
        return
        end
c      
c------
c
       subroutine pritem3c(t,XI,YI,im,jm,ll,xbeg,istep,ybeg,yend,
     *                     char,mig,l,cos30,ncos)
       dimension t(im,jm,ll),inum(40),XI(im),YI(jm)
       character*14 char
        write(6,100) char
 100    format(/10x,a14)
        write(6,105) im,jm,xbeg,istep,ybeg,yend
 105    format(/1x,'use PRITEM3c',/5x,'im,jm,xbeg,istep,ybeg,yend=',
     *         /5x,2i6,f7.2,i6,2f7.2)
c--------------- find ibeg
        if(ncos.eq.1) then
         ibeg=(xbeg*cos30-XI(1))/(XI(im)-XI(1))*float(im-1)+1.0001
        else
         ibeg=(xbeg-XI(1))/(XI(im)-XI(1))*float(im-1)+1.0001
        end if
c--------------- find j1, j2
        j1=(ybeg-YI(1))/(YI(jm)-YI(1))*float(jm-1)+1.0001
        j2=(yend-YI(1))/(YI(jm)-YI(1))*float(jm-1)+1.0001
c---------- use 11 as a number of points on line
        iline=11
        ilinec=iline
        print *,' ibeg,istep,j1,j2,iline=',ibeg,istep,j1,j2,iline
        if(ncos.eq.1) then
         write(6,104) xbeg,XI(1)/cos30,XI(im)/cos30,
     *               ybeg,yend,YI(1),YI(jm)
 104     format('xbeg,XI(1)/cos30,XI(im)/cos30,ybeg,yend,YI(1),YI(jm)',
     *          /7f7.2)
        else
         write(6,204) xbeg,XI(1),XI(im),
     *               ybeg,yend,YI(1),YI(jm)
 204     format('xbeg,XI(1),XI(im),ybeg,yend,YI(1),YI(jm)',
     *          /7f7.2)
        end if
        do i=1,iline
         inum(i)=ibeg+istep*(i-1)
         if(inum(i).gt.im) then
          ilinec=i-1
          go to 200
         end if
        end do
 200    continue
c
        write(6,102) (inum(i),i=1,ilinec)
 102    format(/9x,11i7,/)
c
        if(ncos.eq.1) then
         write(6,103) (XI(inum(i))/cos30,i=1,ilinec) 
 103     format(11x,11f7.2,/)
        else
         write(6,103) (XI(inum(i)),i=1,ilinec) 
        end if
c
        do j=j1,j2
         if(mig.eq.-1) then
          jc=j2-j+j1
         else
          jc=j
         endif
         write(6,101) jc,YI(jc),(t(inum(i),jc,l),i=1,ilinec)
        end do
  101   format(i5,f6.2,11f7.2)
        return
        end
c
c-------------------
c
c-------- 11-15-02 interpolate back only for nbndN < n < nbndS
      subroutine tootis(H,HH,mm,nn,xlon2,ylat2,mmx,nngs,ll,lu,
     *        dgs,xlon1,ylat1,dlon,dlat,mmgs,mb,me,nc,xout,cos30,
     *        migw,mige,nbndN,nbndS)
      dimension xlon2(mmx,nngs),ylat2(mmx,nngs),x(4),y(4),al(4),
     *          dists(4)
      dimension H(mm,nn,ll),HH(mmx,nngs,ll),dgs(mm,nn),
     *          nbndN(mmx),nbndS(mmx)
c
c--------- interpolate HH from irregular grid xlon2,ylat2(mmx,nngs)
c--------- to H on latlon grid with origin in (xlon1,ylat1) with 
c--------- steps dlon,  dlat  for m=1,mmgs
c
c---------------- all longitudes were multiplied by cos30
c
       F(x1,y1,x2,y2)=sqrt((x1-x2)**2+(y1-y2)**2)
       PI=3.1415927
c----   for debug
       i0=95
       j0=87
      print *,'tootis: mm,nn,mmx,nngs,ll,lu=',mm,nn,mmx,nngs,ll,lu
      print *,'tootis: mmgs,mb,me,nc,migw,mige=',
     *                 mmgs,mb,me,nc,migw,mige
      write(6,1201) xlon1/cos30,ylat1,dlon/cos30,dlat,xout/cos30
 1201 format('xlon1/cos30,ylat1,dlon/cos30,dlat,xout/cos30=',5f7.2)
c
c----------- find mgs74
c
c      do mgs=1,mmgs
c       if(xlon2(mgs,nc).gt.-74.) then
c        mgs74=mgs-1
c        go to 1001
c       end if
c      end do
c1001  continue
c      print *,' tootis: mgs74=',mgs74
c      write(6,115) xlon2(mgs74,nc)/cos30
c115   format('xlon2(mgs74,nc)/cos30=',f7.2)
c      
c-----     send distance to GS -9.
      do m=1,mm
      do n=1,nn
       dgs(m,n)=-9.
      end do
      end do
       do m=1,mmgs-1
c      do m=mb,me
c-------- 11-15-02 interpolate back only for nbndN < n < nbndS
c      do n=1,nngs-1
c-------  11-25-02 use min anan max values for points m, m+1
       nbN=max(nbndN(m),nbndN(m+1))
       nbS=min(nbndS(m),nbndS(m+1))
       do n=nbN,nbS-1
c------ check if two perp lines cross each other
c------ if so change points 3 and 4
        u1=xlon2(m+1,n)-xlon2(m,n)
        v1=ylat2(m+1,n)-ylat2(m,n)
        u2=xlon2(m+1,n+1)-xlon2(m,n+1)
        v2=ylat2(m+1,n+1)-ylat2(m,n+1)
        asc=u1*u2+v1*v2
         x(1)=xlon2(m,n)
         x(2)=xlon2(m+1,n)
         y(1)=ylat2(m,n)
         y(2)=ylat2(m+1,n)
        if(asc.ge.0.) then
         x(3)=xlon2(m+1,n+1)
         x(4)=xlon2(m,n+1)
         y(3)=ylat2(m+1,n+1)
         y(4)=ylat2(m,n+1)
         iflag=1
        else
         x(3)=xlon2(m,n+1)
         x(4)=xlon2(m+1,n+1)
         y(3)=ylat2(m,n+1)
         y(4)=ylat2(m+1,n+1)
         iflag=-1
c        write(6,104) m,n,u1,v1,u2,v2,asc
 104     format(/10x,'tootis iflag=-1 m,n,u1,v1,u2,v2,asc',
     *          /1x,2i7,5f8.5)
c        write(6,105) x,y
 105     format(/1x,'x,y',/8f7.3)
        end if
        xmin=min(x(1),x(2),x(3),x(4))
        xmax=max(x(1),x(2),x(3),x(4))
        ymin=min(y(1),y(2),y(3),y(4))
        ymax=max(y(1),y(2),y(3),y(4))
        ib=(xmin-xlon1)/dlon+1.0001
        ie=(xmax-xlon1)/dlon+1.0001
        jb=(ymin-ylat1)/dlat+1.0001
        je=(ymax-ylat1)/dlat+1.0001
c---------- falk 11-13-02 check ib,ie,jb,je
        if(ib.lt.1.or.ie.gt.mm) go to 1000
        if(jb.lt.1.or.je.gt.nn) go to 1000
c------   find latlon grid points inside 4-angle of irreg grid
        do i=ib,ie
        do j=jb,je
         x0=xlon1+dlon*(i-1)
         y0=ylat1+dlat*(j-1)
c
c-----    for each side of 4-angle k check if sum of angles is 
c             equal to 2PI. It means that  (x0,y0) is in 4-angle
c-----    here it is not important that distances in x < than in y
c
          icorner=0
          do k=1,4
           x1=x(k)
           y1=y(k)
           if(k.eq.4) then
            x2=x(1)
            y2=y(1)
           else
            x2=x(k+1)
            y2=y(k+1)
           end if
           a=F(x1,y1,x2,y2)
           b=F(x0,y0,x1,y1)
           c=F(x0,y0,x2,y2)
           call distoline(x1,y1,x2,y2,x0,y0,dists(k))
           if(abs(b).lt.1.E-3.or.abs(c).lt.1.E-3) then
            icorner=1
            go to 300
           end if
           cosal=(b*b+c*c-a*a)/(2.*b*c)
           if(abs(cosal).gt.1.) cosal=0.99999*cosal
           al(k)=acos(cosal)
 300       continue
c---------------
c           if(i.eq.i0.and.j.eq.j0) then
c            print *,'tootis, i0,j0=',i0,j0
c            write(6,101) m,n,k,ib,ie,jb,je
 101         format(/1x,' m,n on perp line,k,ib,ie,jb,je',
     *              /1x,7i7)
c            write(6,102) x0,y0,x1,y1,x2,y2
 102         format(/1x,'x0,y0 on OTIS,x1,y1,x2,y2 on perp linesl',
     *              /1x,6f7.2)
c            write(6,103) a,b,c,cosal,al(k)
 103         format(/10x,'a,b,c,cosal,al(k)',5f7.2)
c           end if
c---------------
          end do 
          if(icorner.eq.1) go to 100
c
          alsum=0.
c
          do k=1,4
           alsum=alsum+al(k)
          end do
c---------------
c         if(i.eq.i0.and.j.eq.j0) then
c          write(6,107) x,y
 107       format(' x(k),y(k) on perp lines,k=1,4',
     *            /8f7.2)
c          write(6,108) al,alsum
 108       format('al(k),k=1,4; alsum',5f7.3)
c         end if
c---------------
c
          if(abs(alsum-2.*PI).lt.1.E-2) go to 100
          go to 200
 100      continue
c---for dist to GS it is important that distances in x < than in y
          call distgs(d,x0,y0,xlon2,ylat2,mmx,nngs,nc,m)
          if(dgs(i,j).gt.d.or.dgs(i,j).lt.-8.) then
           dgs(i,j)=d 
           call bilinz1(H,HH,dists,mm,nn,ll,mmx,nngs,i,j,m,n,iflag,lu)
          end if
 200      continue
c---------------
c         if(i.eq.i0.and.j.eq.j0) then
c          print *,' icorner=',icorner
c          write(6,110) dists,d,dgs(i,j)
 110       format(/'dists,d,dgs(i,j)',6f7.3)
c          print *,' (H(i,j,l),l=1,ll)'
c          write(6,111) (H(i,j,l),l=1,ll)
 111       format(10f7.2)
c         end if
c---------------
        end do     ! end loop in i
        end do        ! end loop in j
c---------- falk 11-13-02 check ib,ie,jb,je
 1000   continue
c
       end do            ! end loop in n
       end do                ! end loop in m
c
c--------     interpolate on west bnd
c
c------------- 03-27-03 migw=1 is used for TR1-TR4 which go
c                       from east to west
       if(migw.eq.1) then
c
       mend=(xout-xlon1)/dlon+1.001
       m=mend
       print *,'west bnd: m,xout/cos30, xlon1/cos30, dlon/cos30=',
     *          m,xout/cos30, xlon1/cos30, dlon/cos30
c----------------
       do n=1,nngs-1
        y1=ylat2(1,n)
        y2=ylat2(1,n+1)
        jb=(y1-ylat1)/dlat+1.0001
        je=(y2-ylat1)/dlat+1.0001
        do j=jb,je
         y0=ylat1+dlat*(j-1)
         if(y0.ge.y1.and.y0.lt.y2) then
          alb=(y0-y1)/(y2-y1)
c------------ falk 01/08/01 begin interp from l=lu
c         do l=1,ll
          do l=lu,ll
c--------------- 03-27-03 change HH(mmgs,...  to HH(1,...
           a=HH(1,n,l)
           b=HH(1,n+1,l)
           if(a.gt.-8..and.b.gt.-8.) then
            H(m,j,l)=a*(1.-alb)+b*alb
           else
            if(a.gt.-8..and.alb.lt.0.5) H(m,j,l)=a
            if(b.gt.-8..and.alb.gt.0.5) H(m,j,l)=b
           end if
          end do
         end if
        end do
       end do
c
       end if
c
c--------     interpolate on east bnd
c
c------------- 03-27-03 mige=1 is used for GS only
       if(mige.eq.1) then
c
       mend=(xout-xlon1)/dlon+1.001
       m=mend
       print *,'east bnd: mend,xout/cos30,xlon1/cos30,dlon/cos30=',
     *          mend,xout/cos30,xlon1/cos30,dlon/cos30
c----------------
       do n=1,nngs-1
        y1=ylat2(mmgs,n)
        y2=ylat2(mmgs,n+1)
        jb=(y1-ylat1)/dlat+1.0001
        je=(y2-ylat1)/dlat+1.0001
        do j=jb,je
         y0=ylat1+dlat*(j-1)
         if(y0.ge.y1.and.y0.lt.y2) then
          alb=(y0-y1)/(y2-y1)
c------------ falk 01/08/01 begin interp from l=lu
c         do l=1,ll
          do l=lu,ll
           a=HH(mmgs,n,l)
           b=HH(mmgs,n+1,l)
           if(a.gt.-8..and.b.gt.-8.) then
            H(m,j,l)=a*(1.-alb)+b*alb
           else
            if(a.gt.-8..and.alb.lt.0.5) H(m,j,l)=a
            if(b.gt.-8..and.alb.gt.0.5) H(m,j,l)=b
           end if
          end do
         end if
        end do
       end do
c
       end if
c----------------
c      print *,' after interpolation (H(m,n,1),n=1,nn) m=',mend
c      write(6,106) (H(m,n,1),n=1,nn)
 106   format(10f8.2)
c      print *,'(dgs(i,j),i=86,96) j=85,95'
c      do j=85,95
c       write(6,109) (dgs(i,j),i=86,96)
c      end do
 109   format(11f7.2)
c----------------
c
      return
      end
c
c-------------------
c
      subroutine distoline(x1,y1,x2,y2,x0,y0,dist)
c
c----- Subr. finds distance from a point x0,y0 to a strait line
c-----       which goes through points 1 and 2
c
      A1=-(y2-y1)
      B1=x2-x1
      C1=(y2-y1)*x1-(x2-x1)*y1
      D1=sqrt(A1*A1+B1*B1)
      if(D1.lt.1.E-3) then
       dist=sqrt((x0-x1)**2+(y0-y1)**2)
       go to 100
      end if
      if(C1.gt.0.) D1=-D1
      A=A1/D1
      B=B1/D1
      C=C1/D1
      dist=abs(A*x0+B*y0+C)
 100  continue
      return
      end
c
c----------
c
      subroutine distgs(dmin,x0,y0,xlon2,ylat2,mmx,nngs,nc,m)
      dimension xlon2(mmx,nngs),ylat2(mmx,nngs)
c
c---------------         all longitudes were multiplied by cos30
c
c     F(x1,y1,x2,y2)=sqrt(((x1-x2)*cos30)**2+(y1-y2)**2)
      F(x1,y1,x2,y2)=sqrt((x1-x2)**2+(y1-y2)**2)
c     PI=3.1415927
c     cos30=cos(PI*30./180.)
      dmin=1000.
      do i=m,m+1
       x=xlon2(i,nc)
       y=ylat2(i,nc)
       d=F(x0,y0,x,y)
       if(d.lt.dmin) dmin=d
      end do
      return
      end
c
c-------------------
c
c---------  falk 01/08/01 begin interp from l=lu
       subroutine bilinz1(H,HH,dists,mm,nn,ll,mmx,nngs,
     *                    i,j,m,n,iflag,lu)
       dimension H(mm,nn,ll),HH(mmx,nngs,ll),dists(4),f(4),ZTS(4)
c
       bet1=dists(2)*dists(3)
       bet2=dists(4)*dists(3)
       bet3=dists(1)*dists(4)
       bet4=dists(1)*dists(2)
c---------  falk 01/08/01 begin interp from l=lu
c      do l=1,ll
       do l=lu,ll
        f(1)=HH(m,n,l)
        f(2)=HH(m+1,n,l)
        if(iflag.eq.1) then
         f(3)=HH(m+1,n+1,l)
         f(4)=HH(m,n+1,l)
        else
         f(3)=HH(m,n+1,l)
         f(4)=HH(m+1,n+1,l)
        end if
        do k=1,4
         if(f(k).gt.-8.) then
          ZTS(k)=1.
         else
          ZTS(k)=0.
         end if
        end do
        a=    ZTS(1)*f(1)*bet1+ZTS(2)*f(2)*bet2
     *       +ZTS(3)*f(3)*bet3+ZTS(4)*f(4)*bet4
        area= ZTS(1)*bet1+ZTS(2)*bet2
     *       +ZTS(3)*bet3+ZTS(4)*bet4
c--------- falk 11-13-02 
c       if(area.eq.0.) then
        if(area.lt.(0.1*(bet1+bet2+bet3+bet4))) then
         a=-9.
        else
         a=a/area
        end if
        H(i,j,l)=a
       end do
       return
       end
c
c----------------
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
c
c----------------
c
c---------------- 05-17-06 include par. irg into ringtr
      subroutine ringtr(xlonr,ylatr,nbndN,nbndS,mmx,nngs,mrgs,
     *                  cos30,stepgs,stepn,irg)
c-------------  make trajectory along ring
c-------------  read coord. of west and east
c-------------        points along big axis of ring, It has to be
c-------------        (xwr < xer) and if(xwr=xer) then ywr < yer),
c-------------         rsm for small axis
c-------------  ring consists from 2 half circles and cylinder
c-------------                between them
c     parameter(xwr=-92.,ywr=26.0,xer=-88.,yer=28.0,rsm=1.0)
c     parameter(xwr=-92.,ywr=28.0,xer=-88.,yer=26.0,rsm=1.0)
c     parameter(xwr=-90.,ywr=24.0,xer=-88.,yer=28.0,rsm=1.0)
c     parameter(xwr=-90.,ywr=28.0,xer=-88.,yer=24.0,rsm=1.0)
c     parameter(xwr=-90.,ywr=26.0,xer=-88.,yer=24.0,rsm=4.0)
c     parameter(mmx=500,step=0.1)
c------------
      dimension xlonr(mmx),ylatr(mmx),xt1(mmx),yt1(mmx),
     *   xt2(mmx),yt2(mmx),xt3(mmx),yt3(mmx),xt4(mmx),yt4(mmx)
      dimension nbndN(mmx),nbndS(mmx)
c-------------- 05-18-06 add cold rings
c--------------   lring=1 for warm core, lring=-1 for cold core ring
c--------------   wwr is weight for twc in a warm core ring
c--------------   dtc is a temperature difference in a cold core ring
      common /ringp/ lring,wwr,dtc
c------------
      F(x1,y1,x2,y2)=sqrt((x1-x2)**2+(y1-y2)**2)
      PI=3.141592
c------------ read ring parameters
      read(32,200) xwr
      read(32,200) ywr
      read(32,200) xer
      read(32,200) yer
      read(32,200) rsm
 200  format(f7.1)
c------------ 02-06-06 use the same cos30 as for LC
c     PHI=(ywr+yer)*0.5
c     cos30=cos(PHI*PI/180.)
c     write(6,100) PHI,cos30
 100  format('PHI,cos30=',f7.2,f8.3)
c-------------
c---------------- 05-17-06 include par. irg into ringtr
      print *,'ring number: irg=',irg
      write(6,101) xwr,ywr,xer,yer,rsm
 101  format('xwr,ywr,xer,yer,rsm=',5f8.3)
c-------------
c     cos30=1.
      x1=xwr*cos30
      y1=ywr
      x2=xer*cos30
      y2=yer
      dwe=F(x1,y1,x2,y2)
c-------------  mig=1 there is no cylider (ring is a circle)
      if(dwe.lt.2.*rsm) then
       mig=1
       r=dwe*0.5
      else
       mig=0
       r=rsm
      end if
      print *,'mig,rsm,r=',mig,rsm,r
c------------- 
      sinal=(y2-y1)/dwe
      cosal=(x2-x1)/dwe
      xz1=0.
      yz1=0.
      x2t=x2-x1
      y2t=y2-y1
      xz2=x2t*cosal+y2t*sinal
      yz2=-x2t*sinal+y2t*cosal
c-----------------
      write(6,110) x2t,y2t
 110  format('x2t,y2t=',2f8.3)
      write(6,111) xz2,yz2,sinal,cosal
 111  format('xz2,yz2,sinal,cosal=',4f8.3)
c-----------------
      xz01=r
      yz01=0.
      xzc1=r
      yzc1=r
      xzd1=r
      yzd1=-r
c-----------------
      xz02=xz2-r
      yz02=0.
      xzc2=xz2-r
      yzc2=r
      xzd2=xz2-r
      yzd2=-r
c-----------------
      ald1=3./2.*PI
      alc2=1./2.*PI
c-----------------
      write(6,104) xz01,yx01,xzc1,yzc1,xzd1,yzd1
 104  format('xz01,yx01,xzc1,yzc1,xzd1,yzd1=',6f8.3)
      write(6,105) xz02,yx02,xzc2,yzc2,xzd2,yzd2
 105  format('xz02,yx02,xzc2,yzc2,xzd2,yzd2=',6f8.3)
      write(6,108) ald1/PI*180.,alc2/PI*180.
 108  format('ald1/PI*180.,alc2/PI*180.   =',2f8.2)
c----------------- from d1 to c1
      dst1=PI*r
      m1=dst1/stepgs
      al=PI/float(m1)
      xt1(1)=xzd1
      yt1(1)=yzd1
      do m=1,m1+1
       alc=ald1-al*float(m-1)
       xt1(m)=xz01+r*cos(alc)
       yt1(m)=yz01+r*sin(alc)
      end do
c----------------- from c2 to d2
      dstr2=PI*r
      m2=dstr2/stepgs
      al=PI/float(m2)
      xt2(1)=xzc2
      yt2(1)=yzc2
      do m=1,m2+1
       alc=alc2-al*float(m-1)
       xt2(m)=xz02+r*cos(alc)
       yt2(m)=yz02+r*sin(alc)
      end do
c----------------
      print *,'(xt1(m),m=1,m1+1)  m1=',m1
      write(6,201) (xt1(m),m=1,m1+1)
      print *,'(yt1(m),m=1,m1+1)  m1=',m1
      write(6,201) (yt1(m),m=1,m1+1)
c----------------
      print *,'(xt2(m),m=1,m2+1)  m2=',m2
      write(6,201) (xt2(m),m=1,m2+1)
      print *,'(yt2(m),m=1,m2+1)  m2=',m2
      write(6,201) (yt2(m),m=1,m2+1)
 201  format(10f7.2)
c---------------------------- 
      if(mig.eq.0) then
c---------------- from c1 to c2
       dstr3=F(xzc1,yzc1,xzc2,yzc2)
       m3=dstr3/stepgs
       step1=dstr3/float(m3)
       xt3(1)=xzc1
       yt3(1)=yzc1
       do m=2,m3
        xt3(m)=xt3(m-1)+step1
        yt3(m)=r
       end do
c---------------- from d2 to d1
       dstr4=F(xzd1,yzd1,xzd2,yzd2)
       m4=dstr4/stepgs
       step2=dstr4/float(m4)
       xt4(1)=xzd2
       yt4(1)=yzd2
       do m=2,m4
        xt4(m)=xt4(m-1)-step2
        yt4(m)=-r
       end do
c----------------
       write(6,202) stepgs,step1,step2
 202   format('stepgs,step1,step2=',3f8.3)
c----------------
       print *,'(xt3(m),m=1,m3)  m3=',m3
       write(6,201) (xt3(m),m=1,m3)
       print *,'(yt3(m),m=1,m3)  m3=',m3
       write(6,201) (yt3(m),m=1,m3)
c----------------
       print *,'(xt4(m),m=1,m4)  m4=',m4
       write(6,201) (xt4(m),m=1,m4)
       print *,'(yt4(m),m=1,m4)  m4=',m4
       write(6,201) (yt4(m),m=1,m4)
c----------------
      end if
c----------------
      do n=1,m1
       xlonr(n)=xt1(n)
       ylatr(n)=yt1(n)
      end do
      n1=m1
c----------------
      if(mig.eq.0) then
       do m=1,m3
        n=m+n1
        xlonr(n)=xt3(m)
        ylatr(n)=yt3(m)
       end do
       n2=m1+m3 
      else
       n2=n1
      end if
c----------------
      do m=1,m2
       n=m+n2
       xlonr(n)=xt2(m)
       ylatr(n)=yt2(m)
      end do
c----------------
      if(mig.eq.0) then
       n3=m1+m3+m2 
       do m=1,m4
        n=m+n3
        xlonr(n)=xt4(m)
        ylatr(n)=yt4(m)
       end do
       nn=m1+m3+m2+m4 
      else
       nn=n2+m2
      end if
c----------------- use for tootis additional point nn+1
      nn=nn+1
      xlonr(nn)=xlonr(1)
      ylatr(nn)=ylatr(1)
c-----------------
      print *,'(xlonr(n),n=1,nn)   nn=',nn
      write(6,201) (xlonr(n),n=1,nn)
      print *,'(ylatr(n),n=1,nn)   nn=',nn
      write(6,201) (ylatr(n),n=1,nn)
c-----------------
      x2t=xz2*cosal-yz2*sinal
      y2t=xz2*sinal+yz2*cosal
c-----------------
      write(6,110) x2t,y2t
c-----------------
      do n=1,nn
       aa=xlonr(n)*cosal-ylatr(n)*sinal+x1
       bb=xlonr(n)*sinal+ylatr(n)*cosal+y1
       xlonr(n)=aa
       ylatr(n)=bb
      end do
c-----------------
      mrgs=nn
c----------------- specify nbndN,nbmdS
      k=r/stepn+1.00001
      dd=stepn*float(k)
      nc=(nngs+1)/2
c-----------------
      write(6,109) lring,k,nc,nngs,dd,r
 109  format(' lring,k,nc,nngs,dd,r=',4i7,2f8.3)
c-----------------
c----------------- 05-18-06 add cold rings
      if(lring.eq.1) then
       do n=1,nn
        nbndN(n)=nc-k
cRMY 07-14-06 begin!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c         nbndS(n)=nc+8
         nbndS(n)=nc+1
cRMY 07-14-06 end  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       end do 
      else
c----------------- change direction of trajectory for cold ring
       do n=1,nn
        nbndS(n)=nc+k
cRMY 07-14-06 begin!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c         nbndN(n)=nc-8
         nbndN(n)=nc-1
cRMY 07-14-06 end  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       end do 
       do n=1,nn
        k=nn-n+1
        xt1(k)=xlonr(n)
        yt1(k)=ylatr(n)
       end do
       do n=1,nn
        xlonr(n)=xt1(n)
        ylatr(n)=yt1(n)
       end do
      end if
c-----------------
      print *,' lring,nbndN(1),nbndS(1)=',lring,nbndN(1),nbndS(1)
      print *,'return to lat-lon'
      print *,'(xlonr(n)/cos30,n=1,nn)   nn=',nn
      write(6,201) (xlonr(n)/cos30,n=1,nn)
      print *,'(ylatr(n),n=1,nn)   nn=',nn
      write(6,201) (ylatr(n),n=1,nn)
c-----------------
      return
      end
c
c----------------- falk 02-06-06 add RING
c
c----------------- 05-18-06 add nbndS for cold rings 
      subroutine RINGprep(tt,nbndN,nbndS,xlongs,ylatgs,Zlev,mmx,
     *                    nngs,nc,nl,lu,ld,mrgs,cos30,aladj)
      parameter(lgs=13,tgs=12.)
      dimension tt(mmx,nngs,nl),nbndN(mmx),nbndS(mmx),
     *          xlongs(mmx),ylatgs(mmx),aladj(nl),Zlev(nl)
      common /warmcore/ twc(50)
c-------------- 05-18-06 add cold rings
c--------------   lring=1 for warm core, lring=-1 for cold core ring
c--------------   wwr is weight for twc in a warm core ring
c--------------   dtc is a temperature difference in a cold core ring
      common /ringp/ lring,wwr,dtc
c--------------
      print *,' RINGprep: aladj(nl)'
      write(6,103) aladj
 103  format(10f7.2)
c-------------- 05-31-06 add using real temp. profile in ring core
      read(32,321) idt
 321  format(i1)
      print *,' idt=1 for real data, else idt=0   idt=',idt
      if(idt.eq.1) then
cRMY 12/28/06 allow partial use of real data for RING profile
cRMY       read(32,322) (twc(l),l=1,nl)
       read(32,324) nlax
 324   format(i2)
       read(32,322) (twc(l),l=1,nlax)
 322   format(10f7.2)
cRMY       print *,'(twc(l),l=1,nl) for real profile'
cRMY       write(6,103) (twc(l),l=1,nl)
       print *,'(twc(l),l=1,nlax) for real profile'
       write(6,103) (twc(l),l=1,nlax)
cRMY 12/28/06 end 2 of 4-------------------------------------
      end if
c-------------- 05-18-06 add cold rings
      print *,'lring=1 (warm), lring=-1 (cold); lring=', lring
      if(lring.eq.1) then
c-------------- 02-08-06 add output for twc
       print *,'(twc(l),l=1,nl) for warm core'
       write(6,103) (twc(l),l=1,nl)
cRMY 12/28/06:       if(idt.eq.1) wwr=1.
       write(6,703) wwr
 703   format('weight for warm core wwr=',f3.1)
      end if
c-------------- 02-08-06 add output for m=1 before sending twc
      print *,'(tt(1,k,l), k=n,n+10) n=nbndN(1) before sending twc'
      n=nbndN(1)
      write(6,100) (k,k=n,n+10)
 100  format(5x,11i7)
      do l=1,nl
       write(6,101) l,(tt(1,k,l), k=n,n+10)
      end do
 101  format(i7,11f7.2)
c-------------- 05-18-06 add cold rings
      if(lring.eq.1) then
c-------------- send twc with weight from 1 to 0 for k from n to nc
       do m=1,mrgs
        n=nbndN(m)
cRMY 12/28/06 make provision to allow wwr < 1 under nlax 
        if(idt.eq.0) then
        do l=1,ld
         ttt=twc(l)
         do k=n,nc-1
c-------------- 02-08-06 decrease intensity in ring
c-------------- send twc with weight from 0.8 to 0 for k from n to nc
c         tt(m,k,l)=(float(nc-k)*ttt+
c    *               float(k-n)*tt(m,k,l))/float(nc-n)
c-------------- 05-18-06 use wwr
c         al=0.8*float(nc-k)/float(nc-n)
          al=wwr*float(nc-k)/float(nc-n)
          tt(m,k,l)=al*ttt+(1.-al)*tt(m,k,l)
         end do
        end do
        else
        do l=1,nlax
         ttt=twc(l)
         do k=n,nc-1
          al=1.0*float(nc-k)/float(nc-n)
          tt(m,k,l)=al*ttt+(1.-al)*tt(m,k,l)
         end do
        end do
        do l=nlax+1,ld
         ttt=twc(l)
         do k=n,nc-1
          al=wwr*float(nc-k)/float(nc-n)
          tt(m,k,l)=al*ttt+(1.-al)*tt(m,k,l)
         end do
        end do
        end if
cRMY 12/28/06 end 3 of 4--------------------------------
       end do
      end if
c-------------- 05-18-06 add cold rings
      if(lring.eq.-1) then
       do m=1,mrgs
        n=nbndS(m)
cRMY 12/28/06 make special provision for cold core ring
cRMY        do l=1,ld
c-------------- 05-31-06 use real temp. profile for cold core ring
cRMY         if(idt.eq.0) then
cRMY          tt(m,n,l)=tt(m,n,l)-dtc*aladj(l)
cRMY         else
cRMY          tt(m,n,l)=twc(l)
cRMY         end if
cRMY        end do
        if(idt.eq.0) then
         do l=1,ld
          tt(m,n,l)=tt(m,n,l)-dtc*aladj(l)
         end do
        else
         do l=1,nlax
          tt(m,n,l)=twc(l)
         end do
         do l=nlax+1,ld
          tt(m,n,l)=tt(m,n,l)-dtc*aladj(l)
         end do
        end if
cRMY 12/28/06 end 4 of 4-------------------------------
       end do
       print *,'temp. profile in the cold ring core'
       write(6,322) (tt(1,nbndS(1),l),l=1,nl)
      end if
c-------------- 05-18-06 add cold rings
      if(lring.eq.1) then
c-------------- 02-08-06 add output for m=1 after sending twc
      print *,'(tt(1,k,l), k=n,n+10) n=nbndN(1) after sending twc'
      n=nbndN(1)
      write(6,100) (k,k=n,n+10)
      do l=1,nl
       write(6,101) l,(tt(1,k,l), k=n,n+10)
      end do
c--------------
      print *,' RING  before correction with aladj'
      print *,'l=7(100m), l=10(200), l=13(400), l=15(600)' 
      print *,'                      l=17(800), l=19(1000)' 
      print *,'lon/cos30, lat, tt at  l=7 l=10 l=13 l=15 l=17 l=19'
c--------------
      do m=1,mrgs,5
       write(6,221) m,xlongs(m)/cos30,ylatgs(m),tt(m,nc,7),
     *       tt(m,nc,10),tt(m,nc,13),tt(m,nc,15),tt(m,nc,17),
     *       tt(m,nc,19)
 221   format(1x,i7,2f7.2,2x,6f7.2)
      end do
c-------------- adjust tt at n=nc with aladj
      do m=1,mrgs
       dtt=tt(m,nc,lgs)-tgs
       do l=1,nl
        tt(m,nc,l)=tt(m,nc,l)-aladj(l)*dtt
       end do
      end do
c--------------
      print *,' RING  after correction with aladj'
      print *,'l=7(100m), l=10(200), l=13(400), l=15(600)'
      print *,'                      l=17(800), l=19(1000)'
      print *,'lon/cos30, lat, tt at  l=7 l=10 l=13 l=15 l=17 l=19'
c--------------
      do m=1,mrgs,5
       write(6,221) m,xlongs(m)/cos30,ylatgs(m),tt(m,nc,7),
     *       tt(m,nc,10),tt(m,nc,13),tt(m,nc,15),tt(m,nc,17),
     *       tt(m,nc,19)
      end do
c-------------- 05-18-06 add cold rings
      end if
c--------------
      call stabil(tt,Zlev,mmx,mrgs,nngs,nl)
      return
      end
c
c----------------
c
      subroutine looptr2(xlongs,ylatgs,mmx,mrgs,step,cos30,
     *                  xconw,yconw,xcone,ycone,xnpc,ynpc)
c-----------
      parameter(xwp=-86.55,ywp=23.46,xep=-84.55,yep=24.62)
c-----------
      dimension xloop(mmx),yloop(mmx),xlongs(mmx),ylatgs(mmx),
     *   xt1(mmx),yt1(mmx),xt2(mmx),yt2(mmx),xt3(mmx),yt3(mmx),
     *   xt4(mmx),yt4(mmx),xt5(mmx),yt5(mmx),
     *   xbnd(mmx),ybnd(mmx)
      real xconw,yconw,xcone,ycone,xnpc,ynpc
c     CHARACTER*70 FT_BAY,FT_lc2
c-----------
      F(x1,y1,x2,y2)=sqrt((x1-x2)**2+(y1-y2)**2)
c-----------
c     FT_BAY='/nfsuser/g01/wx20af/gfdl/scripts/workf/BAYuf'
c     OPEN(55,FILE=FT_BAY,form='formatted')
c     FT_lc2='/nfsuser/g01/wx20af/gfdl/scripts/run_sharp/lc2'
c     OPEN(31,FILE=FT_lc2,form='formatted')
c------------ 
      PI=3.141592
c------------ 
c     phi=20.
c     cos30=cos(phi*PI/180.)
c     cos30=1.
c------------ 
c------------ read climate LC path
c     read(55,106) mrgs
c     read(55,105) (xlongs(m),m=1,mrgs)
c     read(55,105) (ylatgs(m),m=1,mrgs)
 105  format(10f7.3)
 106  format(i7)
 107  format(10i7)
c------------ 
      print *,'         '
      print *,'              LOOPTR2: mmx,mrgs=',mmx,mrgs
c------------ read parameters of new LC path
      rewind(31)
      read(31,600) ilc
 600  format(i1)
      read(31,601) xcp
      read(31,601) ycp
c------------ 04-21-06 add reading xwp1,ywp1,xep1,yep1
      read(31,601) xwp1
      read(31,601) ywp1
      read(31,601) xep1
      read(31,601) yep1
 601  format(f7.1)
c------------  04-21-06 add writing xwp1,ywp1,xep1,yep1
      write(6,223) xwp1,ywp1,xep1,yep1
 223  format('specif. points:xwp1,ywp1,xep1,yep1=',4f7.2)
c------------ 
      write(6,222) xwp,ywp,xep,yep
 222  format('     fixed points: xwp,ywp,xep,yep=',4f7.2)
c------------ 
      write(6,121) ilc,xcp,ycp
 121  format('ilc,xcp,ycp=',i7,2f7.2)
c------------ 
c------------ 05-31-06 use ilc=3 for specification of curves
c------------          between west (east) points 1 and 2
      if(ilc.ge.2) then
       read(31,601) xwp2
       read(31,601) ywp2
       read(31,601) xep2
       read(31,601) yep2
c------------ 
       write(6,122) xwp2,ywp2,xep2,yep2
 122   format('xwp2,ywp2,xep2,yep2=',4f7.2)
c------------ 
      end if
c
c------- xlongs,ylatgs the BAY TR obtained for Levitus climate
c------- mw the last fixed point of a west branch of the BAY TR 
c------- me the  first fixed point of an east branch of the BAY TR 
c------- ynp the northern point of Loop Current from satellite data
c------- subr. calculates new BAY TR through p. mw, me simmetric
c-------       about perp line to line connected p. mw, me and
c-------       goes through ynp
c
c------- mw= 67 (23.46,-86.55)
c------- me=139 (24.62,-84.55)
c------- 
c-------------
      call pri10D1(xlongs,mmx,mrgs,1,mrgs,cos30,' xlongs   ')
      call pri10D1(ylatgs,mmx,mrgs,1,mrgs,1.,' ylatgs   ')
c---------------- 
      xw=xwp*cos30
      yw=ywp
      xe=xep*cos30
      ye=yep
c---------------- 04-21-06 use specified points (not climate)
      xw1=xwp1*cos30
      yw1=ywp1
      xe1=xep1*cos30
      ye1=yep1
c------ 04-21-06 check dist. between climate and specified points
      ddw=F(xw,yw,xw1,yw1)
      dde=F(xe,ye,xe1,ye1)
      if(ddw.gt.1.5) then
       xw1=xw
       yw1=yw
      end if
      if(dde.gt.1.5) then
       xe1=xe
       ye1=ye
      end if
c---------------- 
      write(6,224) ddw,dde
 224  format('dist. betw. clim. and specif. ddw,dde=',2f7.3)
c---------------- 04-21-06 find closest points to specified 
      dw=1000.
      de=1000.
      mw=1
      me=1
      do m=1,mrgs
       d=F(xw1,yw1,xlongs(m),ylatgs(m))
       if(d.lt.dw) then
        dw=d
c---------------- 04-21-06 add calculation dxw1,dyw1
        dxw1=xw1-xlongs(m)
        dyw1=yw1-ylatgs(m)
        mw=m
       end if
       dd=F(xe1,ye1,xlongs(m),ylatgs(m))
       if(dd.lt.de) then
        de=dd
c---------------- 04-21-06 add calculation dxe1,dye1
        dxe1=xe1-xlongs(m)
        dye1=ye1-ylatgs(m)
        me=m
       end if
      end do
c------------
      write(6,201) xw1/cos30,yw1,xe1/cos30,ye1,mw,me
 201  format('xw1/cos30,yw1,xe1/cos30,ye1,mw,me=',4f7.2,2i7)
c---------------- 04-21-06 add writing dxw1,dyw1,dxe1,dye1
      write(6,241) dxw1,dyw1,dxe1,dye1
 241  format('dxw1,dyw1,dxe1,dye1=',4f7.3)
c------------
c------------ 04-21-06 use specified points (not climate)
c     xw1=xlongs(mw)
c     yw1=ylatgs(mw)
c     xe1=xlongs(me)
c     ye1=ylatgs(me)
c------------
      xc=xcp*cos30
      yc=ycp
c------------
c------------ 05-31-06 use ilc=3 for specification of curves
c------------          between west points 1 and 2
c------------     and  between east points 2 and 1
      if(ilc.ge.2) then
       xw2=xwp2*cos30
       yw2=ywp2
       xe2=xep2*cos30
       ye2=yep2
        if(ilc.eq.2) then
c------------ find line connected (xw1,yw1) with (xw2,yw2)
       write(6,301) xw1/cos30,yw1,xw2/cos30,yw2
 301   format('xw1/cos30,yw1,xw2/cos30,yw2=',4f7.2) 
       call line(xt1,yt1,mmx,xw1,yw1,xw2,yw2,step,m1)
       print *,'               m1=',m1
c------------ find line connected (xe2,ye2) with (xe1,ye1)
       write(6,302) xe2/cos30,ye2,xe1/cos30,ye1
 302   format('xe2/cos30,ye2,xe1/cos30,ye1=',4f7.2) 
       call line(xt2,yt2,mmx,xe2,ye2,xe1,ye1,step,m2)
       print *,'               m2=',m2
        else
         read(31,331) num_w
         do n=1,num_w
          read(31,332) xt1(n),yt1(n)
          xt1(n)=xt1(n)*cos30
         end do
         read(31,331) num_e
         do n=1,num_e
          read(31,332) xt2(n),yt2(n)
          xt2(n)=xt2(n)*cos30
         end do
         m1=num_w-1
         m2=num_e-1
        end if
 331    format(i3)
 332    format(2f7.1)
c------------
       call pri10D1(xt1,mmx,m1+1,1,m1+1,cos30,'       xt1')
       call pri10D1(yt1,mmx,m1+1,1,m1+1,1.,'       yt1')
       call pri10D1(xt2,mmx,m2+1,1,m2+1,cos30,'       xt2')
       call pri10D1(yt2,mmx,m2+1,1,m2+1,1.,'       yt2')
c------- find middle point between (xw2,yw2) and (xe2,ye2)
       x0=(xw2+xe2)*0.5
       y0=(yw2+ye2)*0.5
       xpw=xw2
       ypw=yw2
       xpe=xe2
       ype=ye2
      else
c------- find middle point between (xw1,yw1) and (xe1,ye1)
       x0=(xw1+xe1)*0.5
       y0=(yw1+ye1)*0.5
       xpw=xw1
       ypw=yw1
       xpe=xe1
       ype=ye1
      end if
c------------
      write(6,203) x0/cos30,y0,xpw/cos30,ypw,xpe/cos30,ype
 203  format('x0/cos30,y0,xpw/cos30,ypw,xpe/cos30,ype=',6f7.2)
c------------
c------- make transfer and rotation
      dcp0=F(x0,y0,xc,yc)
      sinal=(yc-y0)/dcp0
      cosal=(xc-x0)/dcp0
      xz1=0.
      yz1=0.
      x2t=xc-x0
      y2t=yc-y0
      xz2=x2t*cosal+y2t*sinal
      yz2=-x2t*sinal+y2t*cosal
      xwt=xpw-x0
      ywt=ypw-y0
      xet=xpe-x0
      yet=ype-y0
      xwz=xwt*cosal+ywt*sinal
      ywz=-xwt*sinal+ywt*cosal
      xez=xet*cosal+yet*sinal
      yez=-xet*sinal+yet*cosal
c-----------------
      write(6,110) x2t,y2t
 110  format('x2t,y2t=',2f8.3)
      write(6,111) xz2,yz2,sinal,cosal,dcp0
 111  format('xz2,yz2,sinal,cosal,dcp0=',5f8.3)
      write(6,112) xwt,ywt,xet,yet
 112  format('xwt,ywt,xet,yet=',4f8.3)
      write(6,113) xwz,ywz,xez,yez
 113  format('xwz,ywz,xez,yez=',4f8.3)
c-----------------
      r=ywz
      dw=xz2-xwz
      de=xz2-xez
      alc3=PI*0.5
      ywzc=ywz
      yezc=yez
      if(dw.gt.r.and.de.gt.r) then
       mig=0
       xwzc=xz2-r
      else
       if(xwz.gt.xez) then
        mig=1
        xwzc=xwz
       else
        mig=2
        xwzc=xez
       end if
      end if
      xezc=xwzc
c---------------- correct xz2
      xz2=xwzc+r
c----------------
      print *,'mig=',mig
      write(6,114) r,dw,de,alc3/PI*180.,xwzc,xz2
 114  format('r,dw,de,alc3/PI*180.,xwzc,xz2=',6f7.2)
c----------------
      dstr=PI*r
      m3=dstr/step
      al=PI/float(m3)
c----------------
      write(6,115) m3,step,dstr,al*180./PI
 115  format('m3,step,dstr,al*180./PI  =',i7,3f7.2)
       print *,'               m3=',m3
c----------------
      do m=1,m3+1
       alc=alc3-al*float(m-1)
       xt3(m)=xwzc+r*cos(alc)
       yt3(m)=r*sin(alc)
      end do
c----------------
      print *,'after rotation'
      call pri10D1(xt3,mmx,m3+1,1,m3+1,cos30,'       xt3')
      call pri10D1(yt3,mmx,m3+1,1,m3+1,1.,'       yt3')
c----------------
      do m=1,m3+1
       aa=xt3(m)*cosal-yt3(m)*sinal+x0
       bb=xt3(m)*sinal+yt3(m)*cosal+y0
       xt3(m)=aa
       yt3(m)=bb
      end do
c----------------
      print *,'after rotation back'
      call pri10D1(xt3,mmx,m3+1,1,m3+1,cos30,'       xt3')
      call pri10D1(yt3,mmx,m3+1,1,m3+1,1.,'       yt3')
c---------------- rotate back
      xwc=xwzc*cosal-ywzc*sinal+x0
      ywc=xwzc*sinal+ywzc*cosal+y0
      xec=xezc*cosal-yezc*sinal+x0
      yec=xezc*sinal+yezc*cosal+y0
c---------------- rotate back (xz2,yz2) to (xnpc,ynpc)
      xnpc=xz2*cosal-yz2*sinal+x0
      ynpc=xz2*sinal+yz2*cosal+y0
c----------------
      if(mig.ne.1) then
c------------ find line connected (xpw,ypw) with (xwc,ywc)
       write(6,303) xpw/cos30,ypw,xwc/cos30,ywc,xnpc/cos30,ynpc
 303   format('xpw/cos30,ypw,xwc/cos30,ywc,xnpc/cos30,ynpc=',6f7.2) 
       call line(xt4,yt4,mmx,xpw,ypw,xwc,ywc,step,m4)
       print *,'               m4=',m4
       call pri10D1(xt4,mmx,m4+1,1,m4+1,cos30,'       xt4')
       call pri10D1(yt4,mmx,m4+1,1,m4+1,1.,'       yt4')
      end if
      if(mig.ne.2) then
c------------ find line connected (xec,yec) with (xpe,ype)
       write(6,304) xec/cos30,yec,xpe/cos30,ype
 304   format('xec/cos30,yec,xpe/cos30,ype=',4f7.2) 
       call line(xt5,yt5,mmx,xec,yec,xpe,ype,step,m5)
       print *,'               m5=',m5
       call pri10D1(xt5,mmx,m5+1,1,m5+1,cos30,'       xt5')
       call pri10D1(yt5,mmx,m5+1,1,m5+1,1.,'       yt5')
      end if
c------------
c------------ 04-21-06 use sponge area (15 points) to connect 
c------------        climate path with specified point (xwp1,ywp1)
      mspg=15
      mspb=max(1,mw-mspg)
c------------
      print *,'mw,mspg,mspb=',mw,mspg,mspb
c------------
      do m=1,mspb
       xloop(m)=xlongs(m)
       yloop(m)=ylatgs(m)
      end do
      do m=mspb,mw
       alsp=float(m-mspb)/float(mw-mspb)
       xloop(m)=alsp*dxw1+xlongs(m)
       yloop(m)=alsp*dyw1+ylatgs(m)
      end do
      if(ilc.eq.1) then
       me1=mw
      else
       me1=mw+m1
       do m=mw,me1-1
        k=m-mw+1
        xloop(m)=xt1(k)
        yloop(m)=yt1(k)
       end do
      end if
        print *,'ilc,me1=',ilc,me1
      if(mig.eq.1) then
       me4=me1
      else
       me4=me1+m4
       do m=me1,me4-1
        k=m-me1+1
        xloop(m)=xt4(k)
        yloop(m)=yt4(k)
       end do
      end if
        print *,'ilc,mig,m4,me1,me4=',ilc,mig,m4,me1,me4
      me3=me4+m3
      do m=me4,me3-1
       k=m-me4+1
       xloop(m)=xt3(k)
       yloop(m)=yt3(k)
      end do
        print *,'m3,me4,me3=',m3,me4,me3
      if(mig.eq.2) then
       me5=me3
      else
       me5=me3+m5
       do m=me3,me5-1
        k=m-me3+1
        xloop(m)=xt5(k)
        yloop(m)=yt5(k)
       end do
      end if
        print *,'ilc,mig,m5,me3,me5=',ilc,mig,m5,me3,me5
      if(ilc.eq.1) then
       me2=me5
      else
       me2=me5+m2
       do m=me5,me2-1
        k=m-me5+1
        xloop(m)=xt2(k)
        yloop(m)=yt2(k)
       end do
      end if
        print *,'ilc,m2,me5,me2=',ilc,m2,me5,me2
      m6=mrgs-me
      me6=me2+m6
c------------ 04-21-06 use sponge area (15 points) to connect 
c------------        climate path with specified point (xep1,yep1)
      kspg=15
      kspe=min(me+kspg,mrgs)
c------------
      print *,'me,kspg,kspe,mrgs=',me,kspg,kspe,mrgs
c------------
      do m=me2,me6
       k=m-me2+me
       if(k.ge.kspe) then
        xloop(m)=xlongs(k)
        yloop(m)=ylatgs(k)
       else
        alsp=float(kspe-k)/float(kspe-me)
        xloop(m)=xlongs(k)+alsp*dxe1
        yloop(m)=ylatgs(k)+alsp*dye1
       end if
      end do
        print *,'me,mrgs,m6,me2,me6=',me,mrgs,m6,me2,me6
      mloop=me6
      mconw=me4
      mcone=me3
       print *,'mloop=',mloop
c----------- 01-21-03 remember xconw,yconw,xcone,ycone
      xconw=xloop(mconw)
      yconw=yloop(mconw)
      xcone=xloop(mcone)
      ycone=yloop(mcone)
      write(6,202) mconw,mcone,xconw/cos30,yconw,xcone/cos30,ycone
 202  format(' mconw,mcone,xconw/cos30,yconw,xcone/cos30,ycone=',
     *         2i7,4f7.2)
c-----------
      call pri10D1(xloop,mmx,mloop,1,mloop,cos30,'     xloop')
      call pri10D1(yloop,mmx,mloop,1,mloop,1.,'     yloop')
c-----------
      mrgs=mloop
      do m=1,mloop
       xlongs(m)=xloop(m)
       ylatgs(m)=yloop(m)
      end do
c-----------
      write(6,109) xconw/cos30,yconw,xcone/cos30,ycone,xc/cos30,yc
 109  format('xconw/cos30,yconw,xcone/cos30,ycone,xc/cos30,yc=',
     *       6f7.2)
      print *,' END LOOPTR2'
c-----------
c     stop
      return
      end
c
c-----------
c
      subroutine line(xt,yt,mmx,x1,y1,x2,y2,step,mm)
      dimension xt(mmx),yt(mmx)
c------------  find points with step close to 'step' along 
c------------  line connected points (x1,y1) with (x2,y2)
c------------
      F(x1,y1,x2,y2)=sqrt((x1-x2)**2+(y1-y2)**2)
c------------
       d1=F(x2,y2,x1,y1)
       mm=d1/step
       step1=d1/float(mm)
       xt(1)=x1
       yt(1)=y1
       if(abs(x2-x1).gt.abs(y2-y1)) then
        pk=(y2-y1)/(x2-x1)
        pk2=pk*pk
        do m=1,mm
         if(x2.gt.x1) then
          xt(m+1)=xt(m)+step1/sqrt(1.+pk2)
         else
          xt(m+1)=xt(m)-step1/sqrt(1.+pk2)
         end if
         yt(m+1)=yt(m)+pk*(xt(m+1)-xt(m))
        end do
       else
        pk=(x2-x1)/(y2-y1)
        pk2=pk*pk
        do m=1,mm
         if(y2.gt.y1) then
          yt(m+1)=yt(m)+step1/sqrt(1.+pk2)
         else
          yt(m+1)=yt(m)-step1/sqrt(1.+pk2)
         end if
         xt(m+1)=xt(m)+pk*(yt(m+1)-yt(m))
        end do
       end if
c-------------
       print *,'subr. line: mmx=',mmx
       write(6,101) mm,d1,step,step1
 101   format('mm,d1,step,step1=',i7,f7.2,2f8.4)
      return
      end
c
c-------------
c
      subroutine loopnbnd(xlon2,ylat2,mmx,mrgs,nngs,nc,nbnd,
     *           xbnd,ybnd,lbnd,filow,step,cos30,
     *           xconw,yconw,xcone,ycone,xnpc,ynpc)
c----------- subr. finds nbnd for a new BAY TR 
c-----------    finds mlow and mup for 2 closest p. on TR to filow
c-----------          on (xbnd(m),ybnd(m));
c-----------    finds mconw, mcone closest p. to (xconw,yconw) and
c-----------        (xcone,ycone); between these p. TR is a circle 
c-----------    (xnpc,ynpc) is a central point on this cirle;
c-----------    finds (xbnd(m),ybnd(m)) l=1,lbnd line between 2 TR 
c-----------      branches, lines begins from p. (xnpc,ynpc)
c----------- filow=21.0N
c
      dimension xlon2(mmx,nngs),ylat2(mmx,nngs),xbnd(mmx),ybnd(mmx),
     *          nbnd(mmx)
      real xconw,yconw,xcone,ycone,xnpc,ynpc
      F(x1,y1,x2,y2)=sqrt((x1-x2)**2+(y1-y2)**2)
c---------------
      print *,'LOOPNBND:mmx,mrgs,nngs,nc=',mmx,mrgs,nngs,nc
      write(6,203) xconw/cos30,yconw,xcone/cos30,ycone,filow
 203  format('xconw/cos30,yconw,xcone/cos30,ycone,filow=',5f7.2)
c--------- 04-03-03 calculate new bnd between 2 traj. branches 
c     xnpc=xbnd(1)
c     ynpc=ybnd(1)
      write(6,204) xnpc/cos30,ynpc,step,cos30
 204  format('xnpc/cos30,ynpc,step,cos30=',2f7.2,2f10.3)
      dw=1000.
      de=1000.
      do m=1,mrgs
       d=F(xconw,yconw,xlon2(m,nc),ylat2(m,nc))
       if(d.lt.dw) then
        dw=d
        mconw=m
        xw=xlon2(m,nc)
        yw=ylat2(m,nc)
       end if
       dd=F(xcone,ycone,xlon2(m,nc),ylat2(m,nc))
       if(dd.lt.de) then
        de=dd
        mcone=m
        xe=xlon2(m,nc)
        ye=ylat2(m,nc)
       end if
      end do
c------------
      print *,'mconw,mcone=',mconw,mcone
c------------
      x0=(xw+xe)*0.5
      y0=(yw+ye)*0.5
c----------- 03-02-06 use subr. line between p. (xnpc,ynpc) 
c-----------          and (x0,y0) to begin line (xbnd,ybnd)
      call line(xbnd,ybnd,mmx,xnpc,ynpc,x0,y0,step,lc)
c------------
      call pri10D1(xbnd,mmx,lc,1,lc,cos30,' xbnd     ')
      call pri10D1(ybnd,mmx,lc,1,lc,1.,' ybnd     ')
c------------
      print *,'  lc=',lc
c------------
      mw=mconw
      me=mcone
c------------------------------------------------
      do l=lc+1,mmx
c------------
       itr=1
c------------
       x1=xbnd(l-1)
       y1=ybnd(l-1)
       x2=xbnd(l-2)
       y2=ybnd(l-2)
c----------- rotate line (1)-(2) to x-axis
       d21=F(x2,y2,x1,y1)
       sinal=(y2-y1)/d21
       cosal=(x2-x1)/d21
c----------- put  coord. (0,0) to (1)
       x2t=x2-x1
       y2t=y2-y1
       xz2=x2t*cosal+y2t*sinal
       yz2=-x2t*sinal+y2t*cosal
c------ make step along line (1)-(2) in direction from (2) to (1)
       xz=-step
       yz=yz2
c----------- rotate back
       x=xz*cosal-yz*sinal+x1
       y=xz*sinal+yz*cosal+y1
c------------
       if(l.eq.19) then
        write(6,401) l,d21,sinal,cosal,x1/cos30,y1,x2/cos30,y2
 401    format('bnd line: l,d21,sinal,cosal,x1/cos30,y1,x2/cos30,y2',
     *          i7,7f7.2)
        write(6,403) xz2/cos30,yz2,xz/cos30,yz,x/cos30,y
 403    format('rotation: xz2/cos30,yz2,xz/cos30,yz,x/cos30,y=',
     *          6f7.2)
       end if
c---------------------------
 400   continue
c------------ find closest point on  west LC branch (mwc) to (x,y)
c------------ in interval (mw-10,mw+10)  
       dw=1000
       k1=max(1,mw-10)
       k2=min(mw+10,mrgs)
       do k=k1,k2
        xw=xlon2(k,nc)
        yw=ylat2(k,nc)
        dd=F(x,y,xw,yw)
        if(dd.lt.dw) then
         dw=dd
         mwc=k
        end if
c----------------
        if(l.eq.19) then
         write(6,407) l,k,dd,dw
 407     format('l,k,dd,dw=',2i7,2f7.2)
        end if
c----------------
       end do
       xw=xlon2(mwc,nc)
       yw=ylat2(mwc,nc)
c------------
       de=1000
       k1=max(1,me-10)
       k2=min(me+10,mrgs)
       do k=k1,k2
        xe=xlon2(k,nc)
        ye=ylat2(k,nc)
        dd=F(x,y,xe,ye)
        if(dd.lt.de) then
         de=dd
         mec=k
        end if
       end do
       xe=xlon2(mec,nc)
       ye=ylat2(mec,nc)
c------------
       if(l.eq.19) then
        write(6,402) mwc,dw,xw/cos30,yw,mec,de,xe/cos30,ye
 402    format('mwc,dw,xw/cos30,yw,mec,de,xe/cos30,ye=',
     *  i7,3f7.2,i7,3f7.2)
       end if
c------------
       mw=mwc
       me=mec
c-----------------------------
       if(abs(dw-de).lt.0.5*step) go to 300
       dm=(dw+de)*0.5
c------------ rotate line (x,y)-(xw,yw)
       sinal=(yw-y)/dw
       cosal=(xw-x)/dw
c----------- put  coord. (0,0) to (x,y)
       x2t=xw-x
       y2t=yw-y
       xz2=x2t*cosal+y2t*sinal
       yz2=-x2t*sinal+y2t*cosal
c------ find point at dist dm from p. xz2
       x3z=xz2-dm
       y3z=yz2
c----------- rotate back
       x3=x3z*cosal-y3z*sinal+x
       y3=x3z*sinal+y3z*cosal+y
c------------ rotate line (x,y)-(xe,ye)
       sinal=(ye-y)/de
       cosal=(xe-x)/de
c----------- put  coord. (0,0) to (x,y)
       x2t=xe-x
       y2t=ye-y
       xz2=x2t*cosal+y2t*sinal
       yz2=-x2t*sinal+y2t*cosal
c------ find point at dist dm from p. xz2
       x4z=xz2-dm
       y4z=yz2
c----------- rotate back
       x4=x4z*cosal-y4z*sinal+x
       y4=x4z*sinal+y4z*cosal+y
c----------- find new point (x,y)
       x=(x3+x4)*0.5
       y=(y3+y4)*0.5
c------------
       if(l.eq.19) then
        write(6,404) dm,x3/cos30,y3,x4/cos30,y4,x/cos30,y
 404    format('dm,x3/cos30,y3,x4/cos30,y4,x/cos30,y=',
     *          7f7.2)
       end if
c---------------
       if(l.eq.19) then
        print *,'l,itr,mwc,mec=',l,itr,mwc,mec
        write(6,110) xlon2(mwc,nc)/cos30,ylat2(mwc,nc),
     *               xlon2(mec,nc)/cos30,ylat2(mec,nc)
 110     format('xgs(mwc),ygs(mwc),xgs(mec),ygs(mec)',
     *    4f7.2)
       end if
c-----------------------------
       itr=itr+1
       if(itr.lt.5) go to 400
 300   continue
c-----------------------------
       xbnd(l)=x
       ybnd(l)=y
c---------------
       print *,'l,itr,mw,me,dw,de=',l,itr,mw,me,dw,de
c---------------
       if(x.ge.xlon2(mrgs,nc)) then
        lbnd=l
        go to 500
       end if
      end do
 500  continue
c-----------------
      print *,'lbnd=',lbnd
      call pri10D1(xbnd,mmx,lbnd,1,lbnd,cos30,'     xbnd ')
      call pri10D1(ybnd,mmx,lbnd,1,lbnd,1.,'     ybnd ')
c---------------
c--------- 01-21-03 find llob close to filow p. on (xbnd,ybnd)
      l=1
      do while (ybnd(l).gt.filow.and.l.le.lbnd) 
       l=l+1
      end do
      llob=l
      print *,' llob,ybnd(llob),filow=',
     *         llob,ybnd(llob),filow
c--------   find m's along two branches of Loop Cur.
c--------   closest to p. (xbnd(llob),ybnd(llob))
      dcw=1000.
      do m=1,mconw
       d1=F(xbnd(llob),ybnd(llob),xlon2(m,nc),ylat2(m,nc))
       if(d1.lt.dcw) then
        dcw=d1
        mlow=m
       end if
      end do
      dce=1000.
      do m=mcone,mrgs
       d2=F(xbnd(llob),ybnd(llob),xlon2(m,nc),ylat2(m,nc))
       if(d2.lt.dce) then
        dce=d2
        mup=m
       end if
      end do
c
      write(6,405) xbnd(llob)/cos30,ybnd(llob),dcw,dce
 405  format('xbnd(llob)/cos30,ybnd(llob),dcw,dce=',4f7.2)
      print *,'mlow,mconw,mcone,mup,mrgs=',mlow,mconw,mcone,mup,mrgs
c
      do m=1,mrgs
       nbnd(m)=1.
      end do
c-------------- 03-08-06 change finding nbnd
c--------------          begin from m=mconw
      ddw=1000.
      dde=1000.
      mw=mconw
      me=mcone
      do n=nc-1,1,-1
       dd=F(xbnd(lc),ybnd(lc),xlon2(mw,n),ylat2(mw,n))
       if(dd.lt.ddw) then
        ddw=dd
        nbnd(mw)=n
       end if
       dd=F(xbnd(lc),ybnd(lc),xlon2(me,n),ylat2(me,n))
       if(dd.lt.dde) then
        dde=dd
        nbnd(me)=n
       end if
      end do
      write(6,408) mw,me,lc,nbnd(mw),nbnd(me),ddw,dde
 408  format('mw,me,lc,nbnd(mw),nbnd(me),ddw,dde=',5i7,2f7.2)
c-------------- 
      lb=lc
      do m=mw-1,1,-1
       l1=max(1,lb-5)
       l2=min(lb+5,lbnd)
       ddw=1000.
       do l=l1,l2
        do n=nc-1,1,-1
         dd=F(xbnd(l),ybnd(l),xlon2(m,n),ylat2(m,n))
         if(dd.lt.ddw) then
          ddw=dd
          nbnd(m)=n
          lb=l
         end if
        end do
       end do
      end do
      do m=mw+1,me
       nbnd(m)=nbnd(mw)
      end do
c---------------
      lb=lc
      do m=me+1,mrgs 
       l1=max(1,lb-5)
       l2=min(lb+5,lbnd)
       dde=1000.
       do l=l1,l2
        do n=nc-1,1,-1
         dd=F(xbnd(l),ybnd(l),xlon2(m,n),ylat2(m,n))
         if(dd.lt.dde) then
          dde=dd
          nbnd(m)=n
          lb=l
         end if
        end do
       end do
      end do
c---------------
      do m=1,mrgs
       if(nbnd(m).gt.1) nbnd(m)=nbnd(m)-1
      end do
c---------------
      print *,' (nbnd(m),m=1,mrgs)  mrgs=',mrgs
      write(6,103) (nbnd(m),m=1,mrgs)
 103  format(10i7)
c-----------------
      return
      end
