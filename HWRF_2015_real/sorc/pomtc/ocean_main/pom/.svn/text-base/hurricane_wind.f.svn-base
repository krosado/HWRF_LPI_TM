! hurricane_wind.f

! specify synthetic winds based on NHC best track data
!_______________________________________________________________________
      subroutine windmsg
        use ieee_arithmetic
        include 'pom.h'
        integer pln
        parameter(PLN=100)

        integer hour, lat, lon, mx, rmw
        integer readunit
        integer*4 startdate,date
        integer ni
        integer day,month,year,end_flag
        integer garb(5),Rd1(4),Rd2(4)
        character*19 name
        character*8 model_date

        real,DIMENSION(PLN)   :: X,Y,TM,PRES,PRES0,RMAXa,WSPMAX,RADCLS
        real,DIMENSION(PLN,4) :: R18v,R26v
        real,DIMENSION(5) :: Rref18v,Rref26v,alphv
        real,DIMENSION(14):: RAD,WS,RADM,WSM,ANGL
        real,dimension(1) :: timev

        REAL CMP,T1,T2,F0,F1,L0,L1,R,A7,B,E,DELP,x0,y0 !REARTH in pom.h
        REAL LATMIN,LATMAX,lonMIN,lonMAX
        real wtan,wrad,wx,wy,alphaw
        REAL DELTAX,DELTAX1,DELTAY,DELTAY1,DXDY,julday
        COMMON/sphere/LATMIN,LATMAX,lonMIN,lonMAX !REARTH in pom.h

!        DATA RHO_0/1024.E0/ !RHO_0 defined in initialize.f
        DATA RAD/0.,.4,.7,.8,.95,1.,1.35,
     $           2.7,4.05,5.4,6.75,8.1,10.8,13.5/
        DATA ANGL/0.,2.,4.,6.,7.,7.,14.,23.,24.,22.,21.,21.,21.,21./

        WIND_SCALE=1
        ROA=1.28
        RMAX=50.E3
!        REARTH=6371.E3 !REARTH defined in initialize.f
        PI=3.1415927
        E=exp(1.)
        windx=0
        windy=0
        taux=0
        tauy=0
        timev(1)=time

        model_date=time_start(3:4)//time_start(6:7)//
     $                           time_start(9:10)//time_start(12:13)
        read(model_date,'(I8)') startdate

        readunit = 15

      !----------------------- Reading message file --------------------
17      format(A19,I6,1X,I4,1X,I3,1X,I4,1X,I3,1X,I3,3I5,
     $     1X,I2,1X,I3,1X,I4,1X,I4,1X,I4,1X,I4,1X,I4,1X,I4,1X,I4,1X,I4)
        open(readunit,file="track",status='old')
        end_flag=0.
        I=0
        do while(end_flag.eq.0)
          read(readunit,17) name,date,hour,lat,lon,garb,mx,rmw,rd1,rd2
      !----------- falk 09-12-05 change output
      !  if(MOD(IINT,24).EQ.0) then
      !   print*,'reading file ',filename
      !   write(6,17) name,date,hour,lat,lon,garb,mx,rmw,Rd1,Rd2
      !  end if
!RMY: Uncomment the following code block for debug only
!          if(my_task.eq.master_task) then
!            write(*,*) 'name = ',name
!            write(*,*) 'date = ',date
!            write(*,*) 'hour = ',hour
!            write(*,*) 'lat = ',lat
!            write(*,*) 'lon = ',lon
!            write(*,*) 'garb = ',garb
!            write(*,*) 'mx = ',mx
!            write(*,*) 'rmw = ',rmw
!            write(*,*) 'rd1 = ',rd1
!            write(*,*) 'rd2 = ',rd2
!          end if
          
          if(date.eq.0) then
            end_flag = 1
            close(readunit)
          else
           I=I+1

           date=date*100+hour/100
           call date2day(year,julday,date)

           TM(i)=julday
           X(i)=lon/10.                       !RMY: Eastern Hemsiphere
           Y(i)=lat/10.                       !RMY: Northern Hemisphere
           if (east_e(1,1).lt.0.) X(i)=-X(i)  !RMY: Western Hemisphere
           if (north_e(1,1).lt.0.) Y(i)=-Y(i) !RMY: Southern Hemisphere
           PRES(i)=float(garb(3))
           PRES0(i)=float(garb(4))
           RADCLS(i)=float(garb(5))*1000
           WSPMAX(i)=float(mx)
           RMAXa(i)=float(rmw)
           do ni=1,4
             n1=ni+(1-mod(ni,2))*sign(2,3-ni)
             R18v(i,ni)=Rd1(n1)
             R26v(i,ni)=Rd2(n1)
             if(wspmax(i).le.26.or.R26v(i,ni).le.RMAXa(i))
     $          R26v(i,ni)=-999
             if(wspmax(i).le.18.or.R18v(i,ni).le.RMAXa(i))
     $          R18v(i,ni)=-999
             if(R26v(i,ni).gt.R18v(i,ni)) R26v(i,ni)=-999
           end do
         end if
        end do
        ipmax=I
        close(readunit)

      !--------------------- Calculating starting day -----------------

        call date2day(year,julday,startdate)
        do i=1,ipmax
          TM(i)=TM(i)-julday
        end do
!++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  INTERPOLATION OF HURRICANE PATH TO DETERMINE THE CURRENT POSITION

        CMP=TM(ipmax)
        if(cmp.lt.time.or.time.lt.tm(1)) return

        call verinterp(ipmax,1,TM,X,TIMEv,F0)
        call verinterp(ipmax,1,TM,Y,TIMEv,L0)
        xcen=F0
        ycen=L0
        call verinterp(ipmax,1,TM,PRES,TIMEv,PRES1)
        call verinterp(ipmax,1,TM,PRES0,TIMEv,PRES2)
        call verinterp(ipmax,1,TM,RADCLS,TIMEv,RCLS)
        DELP=(PRES2-PRES1)*100.
        call verinterp(ipmax,1,TM,WSPMAX,TIMEv,WSMAX)
        prsmin=PRES1
        wndmax=WSMAX
        WSMAX=WSMAX*WIND_SCALE
        WS18=18*WIND_SCALE
        WS26=26*WIND_SCALE
        call verinterp(ipmax,1,TM,RMAXa,TIMEv,RMAX)
        RMAX=RMAX*1.e3
        do ni=1,4
          call interp1d(-999.0,ipmax,1,TM,R18v(1,ni),TIMEv,Rref18v(ni))
          if(Rref18v(ni).ne.-999) Rref18v(ni) = Rref18v(ni)*1.e3
          call interp1d(-999.0,ipmax,1,TM,R26v(1,ni),TIMEv,Rref26v(ni))
          if(Rref26v(ni).ne.-999) Rref26v(ni) = Rref26v(ni)*1.e3
          alphv(ni) = (ni-1)*pi/2
        end do
        do ni=2,6
          n1=mod(ni-1,4)+1
          nm1=mod(ni-2,4)+1
          np1=mod(ni,4)+1
          if(Rref18v(n1).eq.-999) then
            if(Rref18v(nm1).ne.-999) then
              if(Rref18v(np1).ne.-999) then
                Rref18v(n1)=0.5*(Rref18v(nm1)+Rref18v(np1))
              else
                Rref18v(n1)=Rref18v(nm1)
              end if
            else
              if(Rref18v(np1).ne.-999) then
                Rref18v(n1)=Rref18v(np1)
              else
                Rref18v(n1)=-999
              end if
            end if
          end if
          if(Rref26v(n1).eq.-999) then
            if(Rref26v(nm1).ne.-999) then
              if(Rref26v(np1).ne.-999) then
                Rref26v(n1)=0.5*(Rref26v(nm1)+Rref26v(np1))
              else
                Rref26v(n1)=Rref26v(nm1)
              end if
            else
              if(Rref26v(np1).ne.-999) then
                Rref26v(n1)=Rref26v(np1)
              else
                Rref26v(n1)=-999
              end if
            end if
          end if
        end do
  
        Rref18v(5) = Rref18v(1)
        Rref26v(5) = Rref26v(1)
        alphv(5) = alphv(4)+pi/2
  
      !----------- falk 09-12-05 change output
        !if(MOD(IINT,24).EQ.0) then
!       print*,'Time=',Time
!       print*,'Current hurricane position (x,y): ',f0,l0
!       print*,'WSMAX=',WSMAX,'; DELP=',DELP,'; RMAX=',RMAX
!       print*,'Rref18v=',Rref18v
!       print*,'Rref26v=',Rref26v
!       end if

        x0=f0
        y0=l0
        F0=F0*2.*PI/360
        L0=L0*2.*PI/360
        CL0=cnorth_e*2.*PI/360
      !--- F0,L0 ARE THE CARRENT (lonITUDE,LATTITUDE) COORDINATES OF THE HURRICANE
  
      !--- CALCULATING UTX AND UTY (HURRICANE SPEED)
  
        cmp=tm(ipmax)
        do i=1,ipmax
          if(abs(tm(i)-time).le.cmp) then
            cmp=abs(tm(i)-time)
            ii=i
          end if
        end do
      !----------- falk 01-05-04 not to go out bnd
        if((tm(ii)-time).le.0.and.ii.ne.ipmax) then
          t1=tm(ii)
          t2=tm(ii+1)
          x1=x(ii)
          x2=x(ii+1)
          y1=y(ii)
          y2=y(ii+1)
        else
          t2=tm(ii)
          t1=tm(ii-1)
          x2=x(ii)
          x1=x(ii-1)
          y2=y(ii)
          y1=y(ii-1)
        end if
        if(ifplane.eq.1) then
          deltax1=rearth*cos(CL0)*(x2-x1)*2.*pi/360
        else
          deltax1=rearth*cos(L0)*(x2-x1)*2.*pi/360
        end if
        deltay1=rearth*(y2-y1)*2.*pi/360
        utx=deltax1/((t2-t1)*24.*3600.)
        uty=deltay1/((t2-t1)*24.*3600.)
  
     !--- CALCULATING PARAMETERS FOR WIND PROFILE FORMULA
        B=WSMAX**2*E*ROA/DELP
        A7=RMAX**B
  
        do i=1,14
          RADM(I)=RMAX*RAD(I)
        end do
  
        wusurf=1E-6
        wvsurf=1E-6
        do i=1,im
          do j=1,jm
            if(ieee_is_nan(east_u(i,j)).or.
     $        ieee_is_nan(north_u(i,j)).or.
     $        ieee_is_nan(east_v(i,j)) .or.
     $        ieee_is_nan(north_v(i,j)).or.
     $        F0.eq.0.or.L0.eq.0) then
              wusurf(i,j)=0
              wvsurf(i,j)=0
              windx(i,j)=0
              windy(i,j)=0
              taux(i,j)=0
              tauy(i,j)=0
            else

!       CALCULATING U-WIND STRESS FOR I,J POINT OF U-VELOCITY GRID
              F1=east_u(i,j)*2.*PI/360
              L1=north_u(i,j)*2.*PI/360
              if(ifplane.eq.1) then
                DELTAX=REARTH*COS(CL0)*(F1-F0)
              else
                DELTAX=REARTH*COS(L0)*(F1-F0)
              end if
              if(abs(DELTAX).lt.1.e-8) DELTAX=sign(1.e-8,deltax)
              DELTAY=REARTH*(L1-L0)
              if(abs(DELTAy).lt.1.e-8) DELTAY=sign(1.e-8,deltay)
              DXDY=DELTAX*DELTAY
              R=SQRT(DELTAX**2+DELTAY**2)
              alphaw=atan(abs(DELTAY/DELTAX))*sign(1.,DXDY) +
     $              (1-sign(1.,DXDY))*pi/2+(1-sign(1.,DELTAY))*pi/2
              if(alphaw.ge.pi/4) then
                alphaw=alphaw-pi/4
              else
                alphaw=alphaw-pi/4+2*pi
              end if
              call verinterp(5,1,alphv,Rref18v,alphaw,Rref18)
              call verinterp(5,1,alphv,Rref26v,alphaw,Rref26)
              call verinterp(14,1,radm,angl,R,RANGL)

!       CALCULATING WIND SPEED 
              if(r.ge.rcls*1.5) then
                wnd=0
                UTXa=0
                UTYa=0
                windx(i,j)=0
                windy(i,j)=0
                taux(i,j)=0
                tauy(i,j)=0
              else
                if(Rref18.le.0.and.Rref26.le.0) then
                  WND=SQRT(A7*B*DELP*EXP(-A7/R**B)/(ROA*R**B)+R**2*
     $               COR(I,J)**2/4.)-R*COR(I,J)/2.
                  UTXa=UTX/2.
                  UTYa=UTY/2.
                else
                  WND=EXPWND(R,RMAX,Rref18,Rref26,WSMAX,WS18,WS26)
                  UTXa=0.
                  UTYa=0.
                end if
!RMY    ELECT TO MAKE WIND IN F-PLANE RUNS PERFECTLY AXISYMMETRIC
                if(ifplane.eq.1) then
                  UTXa=0.
                  UTYa=0.
                end if
               
                RANGL=RANGL*PI/180.
                Wtan=WND*COS(RANGL)
                Wrad=-WND*SIN(RANGL)
                WX=Wrad*(DELTAX)/R-Wtan*(DELTAY)/R+UTXa
                WY=Wtan*(DELTAX)/R+Wrad*(DELTAY)/R+UTYa
                WM=SQRT(WX**2+WY**2)
                 
!RMY USE NEW STRESS PARAMETERIZATION: CD3 FROM TUNG'S THESIS
!RMY                if(WM.LT.10.) then
!RMY                  CD=1.14*1.E-3
!RMY                else 
!RMY                  CD=(0.49+.065*WM)*1.E-3
!RMY                end if
!RMY                 
!RMY                if(WM.le.35.) then
!RMY                  WUSURF(I,J)=WUSURF(I,J)-CD*ROA*WM*WX/RHO_0
!RMY                else
!RMY                  WUSURF(I,J)=WUSURF(I,J)-(3.3368+
!RMY     $                         (WM-34.0449)**0.3)*WX/(WM*RHO_0)
!RMY                end if
      IF(WM.LT.12.5) then
        Z0W=0.0185/GRAV*(0.001*WM**2.+0.028*WM)**2.
        CD=0.4**2./(log(10./Z0W))**2.
      else
        CD1=3.58e-9*WM**3.-9.88e-7*WM**2.+7.81e-5*WM+7.9107e-4
        DCDL=1.12e-7*WM**2.+2.49e-5*WM-3.32e-4
        CD=CD1-DCDL
      end if
      WUSURF(I,J)=WUSURF(I,J)-CD*ROA*WM*WX/RHO_0
              end if

!  CALCULATING V-WIND STRESS FOR I,J POINT OF V-VELOCITY GRID
              F1=east_v(i,j)*2.*PI/360
              L1=north_v(i,j)*2.*PI/360
              if(ifplane.eq.1) then
                DELTAX=REARTH*COS(CL0)*(F1-F0)
              else
                DELTAX=REARTH*COS(L0)*(F1-F0)
              end if
              if(abs(DELTAX).lt.1.e-8) DELTAX=sign(1.e-8,deltax)
              DELTAY=REARTH*(L1-L0)
              if(abs(DELTAy).lt.1.e-8) DELTAy=sign(1.e-8,deltay)
              DXDY=DELTAX*DELTAY
              R=SQRT(DELTAX**2+DELTAY**2)
              alphaw=atan(abs(DELTAY/DELTAX))*sign(1.,DXDY) +
     $              (1-sign(1.,DXDY))*pi/2+(1-sign(1.,DELTAY))*pi/2
              if(alphaw.ge.pi/4) then
                alphaw=alphaw-pi/4
              else
                alphaw=alphaw-pi/4+2*pi
              end if
              call verinterp(5,1,alphv,Rref18v,alphaw,Rref18)
              call verinterp(5,1,alphv,Rref26v,alphaw,Rref26)
              call verinterp(14,1,radm,angl,R,RANGL)

!    CALCULATING WIND SPEED 

              if(r.ge.rcls*1.5) then
                wnd=0
                UTXa=0
                UTYa=0
                windx(i,j)=0
                windy(i,j)=0
                taux(i,j)=0
                tauy(i,j)=0
              else
                if(Rref18.le.0.and.Rref26.le.0) then
                  WND=SQRT(A7*B*DELP*EXP(-A7/R**B)/(ROA*R**B)+R**2*
     $                COR(I,J)**2/4.)-R*COR(I,J)/2.
                  UTXa=UTX/2.
                  UTYa=UTY/2.
                else
                  WND=EXPWND(R,RMAX,Rref18,Rref26,WSMAX,WS18,WS26)
                  UTXa=0.
                  UTYa=0.
                end if
!RMY    ELECT TO MAKE WIND IN F-PLANE RUNS PERFECTLY AXISYMMETRIC
                if(ifplane.eq.1) then
                  UTXa=0.
                  UTYa=0.
                end if

                RANGL=RANGL*PI/180.
                Wtan=WND*COS(RANGL)
                Wrad=-WND*SIN(RANGL)
                WX=Wrad*(DELTAX)/R-Wtan*(DELTAY)/R+UTXa
                WY=Wtan*(DELTAX)/R+Wrad*(DELTAY)/R+UTYa
                WM=SQRT(WX**2+WY**2)

!RMY USE NEW STRESS PARAMETERIZATION: CD3 FROM TUNG'S THESIS
!RMY                if(WM.LT.10.) then
!RMY                  CD=1.14*1.E-3
!RMY                else
!RMY                  CD=(0.49+.065*WM)*1.E-3
!RMY                end if
!RMY        
!RMY                if(WM.le.35.) then
!RMY                  WVSURF(I,J)=WVSURF(I,J)-CD*ROA*WM*WY/RHO_0
!RMY                else
!RMY                  WVSURF(I,J)=WVSURF(I,J)-(3.3368+
!RMY     $                        (WM-34.0449)**0.3)*WY/(WM*RHO_0)
!RMY                end if
      IF(WM.LT.12.5) then
        Z0W=0.0185/GRAV*(0.001*WM**2.+0.028*WM)**2.
        CD=0.4**2./(log(10./Z0W))**2.
      else
        CD1=3.58e-9*WM**3.-9.88e-7*WM**2.+7.81e-5*WM+7.9107e-4
        DCDL=1.12e-7*WM**2.+2.49e-5*WM-3.32e-4
        CD=CD1-DCDL
      end if
      WVSURF(I,J)=WVSURF(I,J)-CD*ROA*WM*WY/RHO_0
              end if

!       CALCULATING WIND STRESS FOR I,J POINT OF DEPTH GRID
              F1=east_e(i,j)*2.*PI/360
              L1=north_e(i,j)*2.*PI/360
              if(ifplane.eq.1) then
                DELTAX=REARTH*COS(CL0)*(F1-F0)
              else
                DELTAX=REARTH*COS(L0)*(F1-F0)
              end if
              if(abs(DELTAX).lt.1.e-8) DELTAX=sign(1.e-8,deltax)
              DELTAY=REARTH*(L1-L0)
              if(abs(DELTAy).lt.1.e-8) DELTAY=sign(1.e-8,deltay)
              DXDY=DELTAX*DELTAY
              R=SQRT(DELTAX**2+DELTAY**2)
              alphaw=atan(abs(DELTAY/DELTAX))*sign(1.,DXDY) +
     $              (1-sign(1.,DXDY))*pi/2+(1-sign(1.,DELTAY))*pi/2
              if(alphaw.ge.pi/4) then
                alphaw=alphaw-pi/4
              else
                alphaw=alphaw-pi/4+2*pi
              end if
              call verinterp(5,1,alphv,Rref18v,alphaw,Rref18)
              call verinterp(5,1,alphv,Rref26v,alphaw,Rref26)
              call verinterp(14,1,radm,angl,R,RANGL)

!       CALCULATING WIND SPEED 

              if(r.ge.rcls*1.5) then
                wnd=0
                UTXa=0
                UTYa=0
                windx(i,j)=0
                windy(i,j)=0
                taux(i,j)=0
                tauy(i,j)=0
              else
                if(Rref18.le.0.and.Rref26.le.0) then
                  WND=SQRT(A7*B*DELP*EXP(-A7/R**B)/(ROA*R**B)+R**2*
     $               COR(I,J)**2/4.)-R*COR(I,J)/2.
                  UTXa=UTX/2.
                  UTYa=UTY/2.
                else
                  WND=EXPWND(R,RMAX,Rref18,Rref26,WSMAX,WS18,WS26)
                  UTXa=0.
                  UTYa=0.
                end if
!RMY    ELECT TO MAKE WIND IN F-PLANE RUNS PERFECTLY AXISYMMETRIC
                if(ifplane.eq.1) then
                  UTXa=0.
                  UTYa=0.
                end if
               
                RANGL=RANGL*PI/180.
                Wtan=WND*COS(RANGL)
                Wrad=-WND*SIN(RANGL)
                WX=Wrad*(DELTAX)/R-Wtan*(DELTAY)/R+UTXa
                WY=Wtan*(DELTAX)/R+Wrad*(DELTAY)/R+UTYa
                WM=SQRT(WX**2+WY**2)
                windx(i,j)=wx
                windy(i,j)=wy
                irflg(i,j)=1
                 
!RMY USE NEW STRESS PARAMETERIZATION: CD3 FROM TUNG'S THESIS
!RMY                if(WM.LT.10.) then
!RMY                  CD=1.14*1.E-3
!RMY                else 
!RMY                  CD=(0.49+.065*WM)*1.E-3
!RMY                end if
!RMY                 
!RMY                if(WM.le.35.) then
!RMY                  taux(I,J)=taux(I,J)-CD*ROA*WM*WX
!RMY                  tauy(I,J)=tauy(I,J)-CD*ROA*WM*WY
!RMY                else
!RMY                  taux(I,J)=taux(I,J)-(3.3368+
!RMY     $                        (WM-34.0449)**0.3)*WX/WM
!RMY                  tauy(I,J)=tauy(I,J)-(3.3368+
!RMY     $                        (WM-34.0449)**0.3)*WY/WM
!RMY                end if
      IF(WM.LT.12.5) then
        Z0W=0.0185/GRAV*(0.001*WM**2.+0.028*WM)**2.
        CD=0.4**2./(log(10./Z0W))**2.
      else
        CD1=3.58e-9*WM**3.-9.88e-7*WM**2.+7.81e-5*WM+7.9107e-4
        DCDL=1.12e-7*WM**2.+2.49e-5*WM-3.32e-4
        CD=CD1-DCDL
      end if
      TAUY(I,J)=TAUY(I,J)+CD*ROA*WM*WY
      TAUX(I,J)=TAUX(I,J)+CD*ROA*WM*WX
              end if
            end if
          end do
        end do
!  ------------------------- falk 01-05-04 add call avrtau
!RMY        call avrtau(x0,y0,0)
        RETURN
      end 
!_______________________________________________________________________
      SUBROUTINE DATE2DAY(year,julday,date)
        integer*4 date
        integer dat2day(12),dat2dayl(12),day,month,year,hour
        real julday
        real*8 tmp
        data dat2day/31,28,31,30,31,30,31,31,30,31,30,31/
        data dat2dayl/31,29,31,30,31,30,31,31,30,31,30,31/

        year=int(date/1000000.)
        month=nint(100*(date/1000000.-int(date/1000000.)))
        julday=0
        if(mod(year,4).eq.0) then
          do ni=1,month-1
            julday=julday+dat2dayl(ni)
          end do
        else
          do ni=1,month-1
            julday=julday+dat2day(ni)
          end do
        end if
        julday=julday+nint(100*(date/10000.-int(date/10000.)))
        hour=date-nint(date/100.)*100
        julday=julday+float(hour)/24.

        return
      end
!_______________________________________________________________________
      subroutine VERINTERP(nv,nvi,xv,yv,xvi,yvi)
!--------------------------------------------------------------
!   This subroutine determines ni values yi at the points xi 
!   interpolating between the n values y at the points x
!--------------------------------------------------------------
        real xv(nv),yv(nv),xvi(nvi),yvi(nvi),tmpv(500)
        integer n,nvi,ivi
        integer iv,jv

        do iv=1,nvi
          if((xvi(iv).gt.xv(nv).and.xvi(iv).gt.xv(1)).or.
     $      (xvi(iv).lt.xv(1).and.xvi(iv).lt.xv(nv))) then
            if(xvi(iv).gt.xv(nv).and.xvi(iv).gt.xv(1)) then
              if(xv(nv).gt.xv(1)) then
                yvi(iv)=yv(nv)
              else
                yvi(iv)=yv(1)
              end if
            else
              if(xv(nv).gt.xv(1)) then
                yvi(iv)=yv(1)
              else
                yvi(iv)=yv(nv)
              end if
            end if
          else
            do jv=1,nv-1
              tmpv(jv)=(xvi(iv)-xv(jv))*(xvi(iv)-xv(jv+1))
            end do
            do jv=1,nv-1
              if(tmpv(jv).le.0) then
                ivi=jv
              end if
            end do
            yvi(iv)=(yv(ivi)*abs(xv(ivi+1)-xvi(iv))+yv(ivi+1)*
     $              abs(xvi(iv)-xv(ivi)))/abs(xv(ivi+1)-xv(ivi))
          end if
        end do

      return
      end
!_______________________________________________________________________
      subroutine INTERP1d(maskv,nv,nvi,xv,yv,xvi,yvi)
!--------------------------------------------------------------
!   This subroutine determines ni values yi at the points xi 
!   interpolating between the n values y at the points x
!   values equal to mask are ignored
!--------------------------------------------------------------
        real xv(nv),yv(nv),xvi(nvi),yvi(nvi),tmpv(500)
        real maskv
        integer nv,nvi,ivi
        integer iv,jv

        do iv=1,nvi
          if((xvi(iv).gt.xv(nv).and.xvi(iv).gt.xv(1)).or.
     $      (xvi(iv).lt.xv(1).and.xvi(iv).lt.xv(nv))) then
            if(xvi(iv).gt.xv(nv).and.xvi(iv).gt.xv(1)) then
              if(xv(nv).gt.xv(1)) then
                yvi(iv)=yv(nv)
              else
                yvi(iv)=yv(1)
              end if
            else
              if(xv(nv).gt.xv(1)) then
                yvi(iv)=yv(1)
              else
                yvi(iv)=yv(nv)
              end if
            end if
          else
            do jv=1,nv-1
              tmpv(jv)=(xvi(iv)-xv(jv))*(xvi(iv)-xv(jv+1))
            end do
            do jv=1,nv-1
              if(tmpv(jv).le.0) ivi=jv
            end do
            if(yv(ivi).eq.maskv.or.yv(ivi+1).eq.maskv) then
              yvi(iv)=maskv
            else
            yvi(iv)=(yv(ivi)*abs(xv(ivi+1)-xvi(iv))+yv(ivi+1)*
     $              abs(xvi(iv)-xv(ivi)))/abs(xv(ivi+1)-xv(ivi))
            end if
          end if
        end do

        return
      end
!_______________________________________________________________________
      subroutine avrtau(xln,ylt,mig)
!--------- This subr. is called from WIND in phase4 (mig=0)
!--------- and from atmos2ocean.f (mig=1) in coupled run
        include 'pom.h'
        parameter(timesm4=12.,timesm=12.,RAVR=100.e3)
        REAL LATMIN,LATMAX,lonMIN,lonMAX
        real xln,ylt,tavr,counter,x1,y1,x0,y0,r,deltax,deltay
        real xlnc,yltc,tauavrp,taumaxp
        real tauavr,taumax,awucon,bwucon
        real RAVR,RRCT,xrct,yrct
        integer irct,jrct
        integer migtau
        integer itau,jtau


!------------- in pom.h included common/tau/
!------------- also the next 5 parameters incuded in RST file
!     common/tau/ tauavr,taumax,awucon,bwucon,migtau
!-------------
!     if(mig.eq.1) then
!      print *,'begin avrtau: coupled model'
!     else
!      print *,'begin avrtau: phase4'
!     end if
!-------------
!
!     write(6,201) LATMIN,LATMAX,lonMIN,lonMAX
201     format('avrtau: LATMIN,LATMAX,lonMIN,lonMAX=',4f7.2)

        pi=3.1415927
!------- save previous tauavr, taumax
        tauavrp=tauavr
        taumaxp=taumax
!-------  for coupled run use TC position from TVARY.h
!-------  for phase4 run use TC position: (xln,ylt)
        if(mig.eq.1) then
          xlnc=poslon
          yltc=poslat
        else
          xlnc=xln
          yltc=ylt
        end if

        tavr=0.0
        counter=0.0
        taumax=0.0
        RRCT=1.e8
        irct=1000
        jrct=1000
        do jtau=1,jm
          do itau=1,im
            x1=(lonMIN+float(Itau-1)*(lonMAX-lonMIN)/float(IM-1))*pi/180.
            y1=(LATMIN+float(Jtau-1)*(LATMAX-LATMIN)/float(JM-1))*pi/180.
            x0=xlnc*pi/180.
            y0=yltc*pi/180.
            cy0=cnorth_e*pi/180.
            if(ifplane.eq.1) then
              DELTAX=REARTH*COS(cy0)*(x1-x0)
            else
              DELTAX=REARTH*COS(y0)*(x1-x0)
            end if
            DELTAY=REARTH*(y1-y0)
            r=SQRT(DELTAX**2+DELTAY**2)
            if(r.lt.RAVR) then
              tauabs=sqrt(wusurf(itau,jtau)**2+wvsurf(itau,jtau)**2)
              if(tauabs*fsm(itau,jtau).gt.taumax) taumax=tauabs
              tavr=tavr+tauabs*fsm(itau,jtau)
              counter=counter+fsm(itau,jtau)
            end if
            if(r.lt.RRCT) then
             RRCT=r
             irct=itau
             jrct=jtau
             xrct=x1*180./pi
             yrct=y1*180./pi
            end if
          end do
        end do
        if(counter.gt.0.) then
          tauavr=tavr/counter
        else
          tauavr=0.0
        end if

        if(mig.eq.1.and.migtau.eq.0) then
!--------- falk 08-19-03 use taumax instead of tauavr
!       if(tauavr.gt.tauavrp) then
!       awucon=tauavrp/tauavr
!       bwucon=(tauavr-tauavrp)/tauavr
          if(taumax.gt.taumaxp) then
            awucon=taumaxp/taumax
            bwucon=(taumax-taumaxp)/taumax
          else
            awucon=1.
            bwucon=0.
          end if

          !print *,' avrtau: first step in coupled model'
          !print *,'migtau,mig=',migtau,mig
          !write(6,101) tauavrp,tauavr,awucon,bwucon
101       format(' tauavrp,tauavr,awucon,bwucon=',4(1PE10.2))
          migtau=1
        end if

        if(mig.eq.1) then
          wucon=awucon+SIN(time*24./timesm*pi*0.5)*bwucon
          if(time*24..gt.timesm) wucon=1.
        else
          wucon=SIN(time*24./timesm4*pi*0.5)
          if(time*24..gt.timesm4) wucon=1.
        end if

        do jtau=1,jm
          do itau=1,im
            wusurf(itau,jtau)=wusurf(itau,jtau)*wucon
            wvsurf(itau,jtau)=wvsurf(itau,jtau)*wucon
            taux(itau,jtau)=taux(itau,jtau)*wucon
            tauy(itau,jtau)=tauy(itau,jtau)*wucon
          end do
        end do

!----------- falk 09-12-05 change output
        if(MOD(IINT,24).EQ.0) then
          if(mig.eq.1) then
            !print *,'avrtau: coupled model'
          else
            !print *,'avrtau: phase4'
          end if

          !write(6,102) time*24,xlnc,yltc
102       format('time*24,xlnc,yltc=',3f7.2)
          !print *,'closest point to the center'
          !write(6,204) xrct,yrct,RRCT,irct,jrct
204       format('xrct,yrct,RRCT,irct,jrct=',2f7.2,f10.0,2i7)
          !write(6,103) tauavrp,tauavr,taumaxp,taumax
103       format('tauavrp,tauavr,taumaxp,taumax=',4(1PE10.2))
          !write(6,202) awucon,bwucon,wucon
202       format('  awucon,bwucon,wucon=',3f10.4)
!         write(6,203) timesm4,timesm
203       format(   'timesm4,timesm=',2f7.2)
        end if

        return
      end

      function expwnd(R,RMAX,Rref18,Rref26,WSMAX,WS18,WS26)
        real Rref18,Rref26,R,RMAX,WSMAX,WS18,WS26
        real b,expwnd
 
        r1=0.5*(Rref18+Rref26)
        WS=0.5*(WS18+WS26)

        if(Rref18.le.0.) then
          r1=Rref26
          WS=WS26
        end if

        if(Rref26.le.0.) then
          r1=Rref18
          WS=WS18
        end if

        if(R.GE.RMAX) then
          b=(RMAX-r1)/log(WS/WSMAX)                     
          expwnd=WSMAX*exp((RMAX-R)/b)
        else
          expwnd=R*WSMAX/RMAX
        end if

        return
      end
