module sACOS_mod
contains
 real(kind=SELECTED_REAL_KIND(13)) function sACOS(V)
   ! safer aCOS that handles near 1.0 and near -1.0 values
   implicit none
   real(kind=SELECTED_REAL_KIND(13)), intent(in) :: V
   real(kind=SELECTED_REAL_KIND(13)) :: av
   real(kind=SELECTED_REAL_KIND(13)), parameter :: near1=1.0001

   av=V

   if(av>-near1 .and. av<-1.0) av=-1
   if(av< near1 .and. av> 1.0) av=1
   sACOS=acos(av)
 end function sACOS
end module sACOS_mod
SUBROUTINE EARTH_LATLON ( HLAT,HLON,VLAT,VLON,     & !Earth lat,lon at H and V points
                          DLMD1,DPHD1,WBD1,SBD1,   & !input res,west & south boundaries,
                          CENTRAL_LAT,CENTRAL_LON, & ! central lat,lon, all in degrees,
                          IDS,IDE,JDS,JDE,KDS,KDE, &  
                          IMS,IME,JMS,JME,KMS,KME, &
                          ITS,ITE,JTS,JTE,KTS,KTE  )
!============================================================================
!
  use sACOS_mod
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME 
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE  
 REAL(4),       INTENT(IN   )                         :: DLMD1,DPHD1,WBD1,SBD1
 REAL(4),       INTENT(IN   )                         :: CENTRAL_LAT,CENTRAL_LON
 REAL(4), DIMENSION(IMS:IME,JMS:JME), INTENT(OUT)     :: HLAT,HLON,VLAT,VLON

! local

 INTEGER,PARAMETER                           :: KNUM=SELECTED_REAL_KIND(13) 
 INTEGER                                     :: I,J
 REAL(KIND=KNUM)                             :: WB,SB,DLM,DPH,TPH0,STPH0,CTPH0
 REAL(KIND=KNUM)                             :: TDLM,TDPH,TLMH,TLMV,TLMH0,TLMV0,TPHH,TPHV,DTR
 REAL(KIND=KNUM)                             :: STPH,CTPH,STPV,CTPV,PI_2
 REAL(KIND=KNUM)                             :: SPHH,CLMH,FACTH,SPHV,CLMV,FACTV
 REAL(KIND=KNUM), DIMENSION(IMS:IME,JMS:JME) :: GLATH,GLONH,GLATV,GLONV
!-------------------------------------------------------------------------

!
      PI_2 = ACOS(0.)
      DTR  = PI_2/90.
      WB   = WBD1 * DTR                 ! WB:   western boundary in radians
      SB   = SBD1 * DTR                 ! SB:   southern boundary in radians
      DLM  = DLMD1 * DTR                ! DLM:  dlamda in radians 
      DPH  = DPHD1 * DTR                ! DPH:  dphi   in radians
      TDLM = DLM + DLM                 ! TDLM: 2.0*dlamda 
      TDPH = DPH + DPH                 ! TDPH: 2.0*DPH 

!     For earth lat lon only

      TPH0  = CENTRAL_LAT*DTR                ! TPH0: central lat in radians 
      STPH0 = SIN(TPH0)
      CTPH0 = COS(TPH0)

      WRITE(0,*) 'WB,SB,DLM,DPH,DTR: ',WBD1,SBD1,DLM,DPH,DTR    
      WRITE(0,*) 'IMS,IME,JMS,JME,KMS,KME',IMS,IME,JMS,JME,KMS,KME 
      WRITE(0,*) 'IDS,IDE,JDS,JDE,KDS,KDE',IDS,IDE,JDS,JDE,KDS,KDE
      WRITE(0,*) 'ITS,ITE,JTS,JTE,KTS,KTE',ITS,ITE,JTS,JTE,KTS,KTE 

                                                !    .H
      DO J = JTS,MIN(JTE,JDE-1)                 ! H./    This loop takes care of zig-zag 
!                                               !   \.H  starting points along j  
         TLMH0 = WB - TDLM + MOD(J+1,2) * DLM   !  ./    TLMH (rotated lats at H points)
         TLMV0 = WB - TDLM + MOD(J,2) * DLM     !  H     (//ly for V points) 
         TPHH = SB + (J-1)*DPH                  !   TPHH (rotated lons at H points) are simple trans.
         TPHV = TPHH                            !   TPHV (rotated lons at V points) are simple trans.
         STPH = SIN(TPHH)
         CTPH = COS(TPHH)
         STPV = SIN(TPHV)
         CTPV = COS(TPHV)

                                                              !   .H
         DO I = ITS,MIN(ITE,IDE-1)                            !  / 
           TLMH = TLMH0 + I*TDLM                              !  \.H   .U   .H 
!                                                             !H./ ----><----
           SPHH = CTPH0 * STPH + STPH0 * CTPH * COS(TLMH)     !     DLM + DLM
           GLATH(I,J)=ASIN(SPHH)                              ! GLATH: Earth Lat in radians 
           CLMH = CTPH*COS(TLMH)/(COS(GLATH(I,J))*CTPH0) &
                - TAN(GLATH(I,J))*TAN(TPH0)
           IF(CLMH .GT. 1.) CLMH = 1.0
           IF(CLMH .LT. -1.) CLMH = -1.0
           FACTH = 1.
           IF(TLMH .GT. 0.) FACTH = -1.
           GLONH(I,J) = -CENTRAL_LON*DTR + FACTH*ACOS(CLMH)

         ENDDO                                    

         DO I = ITS,MIN(ITE,IDE-1)
           TLMV = TLMV0 + I*TDLM
           SPHV = CTPH0 * STPV + STPH0 * CTPV * COS(TLMV)
           GLATV(I,J) = ASIN(SPHV)
           CLMV = CTPV*COS(TLMV)/(COS(GLATV(I,J))*CTPH0) &
                - TAN(GLATV(I,J))*TAN(TPH0)
           IF(CLMV .GT. 1.) CLMV = 1.
           IF(CLMV .LT. -1.) CLMV = -1.
           FACTV = 1.
           IF(TLMV .GT. 0.) FACTV = -1.
           GLONV(I,J) = -CENTRAL_LON*DTR + FACTV*ACOS(CLMV)

         ENDDO

      ENDDO

!     Conversion to degrees (may not be required, eventually)

      DO J = JTS, MIN(JTE,JDE-1)
       DO I = ITS, MIN(ITE,IDE-1)
          HLAT(I,J) = GLATH(I,J) / DTR
          HLON(I,J)= -GLONH(I,J)/DTR
          IF(HLON(I,J) .GT. 180.) HLON(I,J) = HLON(I,J)  - 360.
          IF(HLON(I,J) .LT. -180.) HLON(I,J) = HLON(I,J) + 360.
          VLAT(I,J) = GLATV(I,J) / DTR
          VLON(I,J) = -GLONV(I,J) / DTR
          IF(VLON(I,J) .GT. 180.) VLON(I,J) = VLON(I,J)  - 360.
          IF(VLON(I,J) .LT. -180.) VLON(I,J) = VLON(I,J) + 360.
       ENDDO
      ENDDO

END SUBROUTINE EARTH_LATLON

 SUBROUTINE G2T2H_egrid( IIH,JJH,                            & ! output grid index and weights 
                         HBWGT,                              & ! output weights in terms of parent grid
                         CLAT1, CLON1,                       & ! input source domain center
                         CURRENT_DOMAIN_ID,                  & ! input source domain ID
                         DX,DY,                              & ! input source domain resolution
                         HLAT1,HLON1,                        & ! input source point regular lat and lon 
                         HLAT2,HLON2,                        & ! input target point regular lat and lon 
                         IDS,IDE,JDS,JDE,                    & ! input source outmost domain dimensions
                         ISS,ISE,JSS,JSE,                    & ! source dimensions
                         ITS,ITE,JTS,JTE              )        ! target dimensions
!
!*** XUEJIN ZHANG --- Initial version (07/14/2011)
!*** Qingfu Liu   --- Modified (07/14/2011,04/23/2013)
!Function: Bilnear interpolation weight and indexing for E-grid
!          from (HLAT1,HLON1) to (HLAT2,HLON2)
!
!************************************************************* 
!
!*** FIGURE 1. ARRANGEMENT OF 4 VERTEXES FROM SOURCE DOMAIN
!***
!***                  4
!***
!***                  h
!***             1         2
!***
!***
!***                  3 
!
!points 1,2,3,4 are source points
!point h is target point 
!************************************************************* 
!
!Variables:
!IIH---------I index in term of 4 source points (output)
!JJH---------J index in term of 4 source points (output)
!HBWGT-------weight in term of source point 1 (output)
!HLAT1-------source regular latitude  (input)
!HLON1-------source regular longitude (input)
!HLAT2-------target regular latitude  (input)
!HLON2-------target regular longitude (input)
!CLAT1-------source domain center regular latitude  (input)
!CLON1-------source domain center regular longitude (input) 
!CLAT2-------target domain center regular latitude  (input)
!CLON2-------target domain center regular longitude (input)
!MAX_DOM-----source maximum domain number (input)
!CURRENT_DOMAIN_ID--source domain ID (input)
!I_PARENT_START-----source nest start I in parent domain (input)
!J_PARENT_START-----source nest start I in parent domain (input) 
!DX,DY-------source outmost domain resolution (input)
!RATIO-------source parent-nest domain ratio  (always set = 3)
!PARENT_ID---source PARENT ID (input)
!GRID_ID-----source GRID ID   (input)
!
!IDS---------target outmost domain I-dimension start (input)
!IDE---------target outmost domain I-dimension end   (input)
!JDS---------target outmost domain J-dimension start (input)
!JDE---------target outmost domain J-dimension end   (input)
!ISS---------source array I-dimension start (input)
!ISE---------source array I-dimension end   (input)
!JSS---------source array J-dimension start (input)
!JSE---------source array J-dimension end   (input)
!ITS---------target array I-dimension start (input)
!ITE---------target array I-dimension end   (input)
!JTS---------target array J-dimension start (input)
!JTE---------target array J-dimension end   (input)
!
!************************************************************* 
!     DECLARE VARIABLES
 IMPLICIT NONE
 integer                                :: IDS,IDE,JDS,JDE
 integer                                :: ISS,ISE,JSS,JSE
 integer                                :: ITS,ITE,JTS,JTE
 integer(4), dimension(ITS:ITE,JTS:JTE,4)  :: IIH,JJH 
 real,    dimension(ITS:ITE,JTS:JTE,4)  :: HBWGT
 real                                   :: CLAT1, CLON1
 real                                   :: CLAT2, CLON2
 integer                                :: CURRENT_DOMAIN_ID
 real                                   :: DX,DY
 real,    dimension(ISS:ISE,JSS:JSE)    :: HLAT1,HLON1 
 real,    dimension(ITS:ITE,JTS:JTE)    :: HLAT2,HLON2 
!
! Local variables
 integer                                :: I,J,K,N,II,JJ
 real                                   :: wb, sb, wb0, sb0
 real,    dimension(ISS:ISE,JSS:JSE)    :: HLAT_R,HLON_R 
 real                                   :: slat, slon, rlat, rlon, olat, olon
 real,    dimension(ISS:ISE,JSS:JSE)    :: HLAT1_R,HLON1_R 
 real,    dimension(ITS:ITE,JTS:JTE)    :: HLAT2_R,HLON2_R 
 real,    dimension(ITS:ITE,JTS:JTE)    :: HLAT2_RT,HLON2_RT 
 integer, dimension(ITS:ITE,JTS:JTE)    :: IIH_RT,JJH_RT 
 real                                   :: hlat2_tmp, hlon2_tmp 
 integer                                :: iih_t,jjh_t,iih1,jjh1
 real                                   :: pi,aa,bb,alpha,beta
 real                                   :: res,dis,dis1,dis2
 real                                   :: cint1,cint2,cint3,cint4,cint5
 real                                   :: check_sum
 logical                                :: found
!******************************************
!   I. Create source domain coordinator   *
!******************************************
!
!***************************************************************
!   I.1 Calculate west and south boundary of the source domain *
!***************************************************************

 print*,'inside g2t2h_egrid'
 print*,'dx,dy',dx,dy
 print *, 'ids,ide', ids,ide

 pi=atan(1.0)*4.0
 res=sqrt(dx*dx+dy*dy)
 cint1=-pi/4.
 cint2=pi/4.
 cint3=3.*pi/4.
 cint4=5.*pi/4.
 cint5=7.*pi/4.+0.0001

!*********************************************************************************
!   I.2 Define hlat and hlon of the source domain on rotated lat-lon coordinator *
!*********************************************************************************
 olat = clat1; olon = clon1
 call Convert_LatLon(1, (ISE-ISS+1), (JSE-JSS+1), hlon1, hlat1, hlon1_r, hlat1_r, olon, olat)

! Check hlat_r, hlon_r
! print *, 'hlon_r = ', hlon1_r(iss,jss), hlon1_r(ise,jse)
! print *, 'hlat_r = ', hlat1_r(iss,jss), hlat1_r(ise,jse)
 rlat = hlat1_r(iss,jss); rlon = hlon1_r(iss,jss)
 call Convert_LatLon(2, 1, 1, slon, slat, rlon, rlat, olon, olat)
! print *, 'Check the overlap between read-in (hlat1,hlon1) and (slat, slon) at SW point of source domain'
! print *, 'Defined by g2t2H-egrid, SW hlat1 = ', slat, 'SW hlon1 = ', slon
! print *, 'Read-in from wrfout_d*, SW hlat1 = ', hlat1(iss,jss), 'SW hlon1 = ', hlon1(iss,jss)
 rlat = hlat1_r(ise,jse); rlon = hlon1_r(ise,jse)
 call Convert_LatLon(2, 1, 1, slon, slat, rlon, rlat, olon, olat)
! print *, 'Check the overlap between read-in (hlat1,hlon1) and (slat, slon) at NE point of source domain'
! print *, 'Defined by g2t2H-egrid, NE hlat1 = ', slat, 'NE hlon1 = ', slon
! print *, 'Read-in from wrfout_d*, NE hlat1 = ', hlat1(ise,jse), 'NE hlon1 = ', hlon1(ise,jse)
!**********************************************************************************
!   II. Convert hlat and hlon of the target domain onto source domain             *
!                    rotated lat-lon coordinator                                  *
!**********************************************************************************
 call Convert_LatLon(1, (ITE-ITS+1), (JTE-JTS+1), hlon2, hlat2, hlon2_r, hlat2_r, olon, olat)
! print*,'hlon2_r(1,1),hlat2_r(1,1)=',hlon2_r(1,1),hlat2_r(1,1)
! print*,'hlon1_r(1,1),hlat1_r(1,1)=',hlon1_r(1,1),hlat1_r(1,1)
! print*,'hlon2_r(ite,jte),hlat2_r(ite,jte)=',hlon2_r(ite,jte),hlat2_r(ite,jte)
! print*,'hlon1_r(ise,jse),hlat1_r(ise,jse)=',hlon1_r(ise,jse),hlat1_r(ise,jse)

!**********************************************************************************
!   III. Find index of (hlat2, hlon2) of the target domain on source domain     *
!**********************************************************************************
!
!Function: Find the VERTEX 1 in FIGURE 1
!
 ii=0
!
! Counting the number of points within the source domain
! Purpose: eliminate loops outside of the source domain to reduce the computing time 
!
 iih_rt = 0; jjh_rt = 0
 if(current_domain_id.eq.1)then
   do j = jts,jte
   do i = its,ite
      hlat2_rt(i,j) = hlat2_r(i,j)
      hlon2_rt(i,j) = hlon2_r(i,j)
      iih_rt(i,j)   = i
      jjh_rt(i,j)   = j
      ii = ii + 1
   end do
   end do
 else
   do j = jts,jte
   do i = its,ite
     if(hlat2_r(i,j).ge.hlat1_r(iss,jss) .and.       &
       hlat2_r(i,j).lt.hlat1_r(ise,jse)) then
        if(hlon2_r(i,j).ge.hlon1_r(iss,jss) .and.       &
           hlon2_r(i,j).lt.hlon1_r(ise,jse)) then
           hlat2_rt(i,j) = hlat2_r(i,j)
           hlon2_rt(i,j) = hlon2_r(i,j)
           iih_rt(i,j)   = i
           jjh_rt(i,j)   = j
           ii = ii + 1
        endif
     endif
   enddo
   enddo
 end if
 print *,' total target points within source domain, ', ii
!
! Start inside loops
!
!      write(81,'(4A5,4A12)') 'iih','jjh','iih_t','jjh_t','hlat2_tmp','hlon2_tmp','hlat1_r','hlon1_r'
!      write(82,'(4A5,4A12)') 'iih','jjh','iih_t','jjh_t','hlat2_tmp','hlon2_tmp','hlat1_r','hlon1_r'
!      write(83,'(4A5,4A12)') 'iih','jjh','iih_t','jjh_t','hlat2_tmp','hlon2_tmp','hlat1_r','hlon1_r'
!      write(84,'(4A5,4A12)') 'iih','jjh','iih_t','jjh_t','hlat2_tmp','hlon2_tmp','hlat1_r','hlon1_r'
!      write(85,'(4A5,4A12)') 'iih','jjh','iih_t','jjh_t','hlat2_tmp','hlon2_tmp','hlat1_r','hlon1_r'
!      write(86,'(4A5,4A12)') 'iih','jjh','iih_t','jjh_t','hlat2_tmp','hlon2_tmp','hlat1_r','hlon1_r'
    
 n=0
 do i = its, ite
 do j = jts, jte
!    if(i.eq.1.and.j.eq.1)print *, 'qq test=',iih_rt(i,j),jjh_rt(i,j),hlat2_rt(i,j),hlon2_rt(i,j)
    found=.false.
    if(iih_rt(i,j).gt.0.and.jjh_rt(i,j).gt.0) then
       hlat2_tmp = hlat2_rt(iih_rt(i,j),jjh_rt(i,j))
       hlon2_tmp = hlon2_rt(iih_rt(i,j),jjh_rt(i,j))
       iih_t = int((hlon2_tmp-hlon1_r(iss,jss))/(2*dx))+1
       jjh_t = 2*int((hlat2_tmp-hlat1_r(iss,jss))/(2*dy))+1
       if(iih_t.ge.iss.and.iih_t.lt.ise.and.jjh_t.gt.jss.and.jjh_t.lt.jse)then
        
       aa=hlon2_tmp-hlon1_r(iih_t,jjh_t+1)
       bb=hlat2_tmp-hlat1_r(iih_t,jjh_t+1)
       dis=sqrt(aa*aa+bb*bb)/res
       if(dis.gt.1.005)then
         print*,'hlat2_tmp,hlon2_tmp=',hlat2_tmp,hlon2_tmp
         print*,'hlon1_r,hlat1_r(+1)=',hlon1_r(iih_t,jjh_t+1),hlat1_r(iih_t,jjh_t+1)
         print*,'hlon1_r(iss,jss),hlat1_r(iss,jss)=',hlon1_r(iss,jss),hlat1_r(iss,jss)
         print*,'iih_t,jjh_t=',iih_t,jjh_t
         print*,'aa,bb,dx,dy=',aa,bb,dx,dy,res,dis
         stop
       end if
       if(dis.gt.1..and.dis.lt.1.005)dis=1.0
       if(dis.lt.0.005)then
          iih1=iih_t
          jjh1=jjh_t+1
          hbwgt(i,j,1) = 1.0
          hbwgt(i,j,2) = 0.0
          hbwgt(i,j,3) = 0.0
          hbwgt(i,j,4) = 0.0
          found=.true.
       else
          alpha=atan2(bb,aa)
          if(alpha.lt.(-pi/4))alpha=alpha+2.*pi
          if(alpha.ge.cint1.and.alpha.lt.cint2)then
            found=.true.
            iih1=iih_t
            jjh1=jjh_t+1
            beta=cint2-alpha
            dis1=dis*sin(beta)
            dis2=dis*cos(beta)
            if(dis1.lt.(-0.001).or.dis1.gt.1.001.or.      &
               dis2.lt.(-0.001).or.dis2.gt.1.001)then
              print*,'qliu test wrong1=',i,j,dis,dis1,dis2,beta
              stop
            end if
            hbwgt(i,j,1) = (1.-dis1)*(1.-dis2)
            hbwgt(i,j,2) = dis1*dis2
            hbwgt(i,j,3) = dis1*(1.-dis2)
            hbwgt(i,j,4) = (1.-dis1)*dis2
          else if(alpha.ge.cint2.and.alpha.lt.cint3)then
            found=.true.
            iih1=iih_t
            jjh1=jjh_t+2
            beta=cint3-alpha
            dis1=dis*sin(beta)
            dis2=dis*cos(beta)
            if(dis1.lt.(-0.001).or.dis1.gt.1.001.or.      &
               dis2.lt.(-0.001).or.dis2.gt.1.001)then
              print*,'qliu test wrong2=',i,j,dis,dis1,dis2,beta
              stop
            end if
            hbwgt(i,j,1) = (1.-dis1)*dis2
            hbwgt(i,j,2) = dis1*(1.-dis2)
            hbwgt(i,j,3) = (1.-dis1)*(1.-dis2)
            hbwgt(i,j,4) = dis1*dis2
          else if(alpha.ge.cint3.and.alpha.lt.cint4)then
            found=.true.
            iih1=iih_t-1
            jjh1=jjh_t+1
            beta=cint4-alpha
            dis1=dis*sin(beta)
            dis2=dis*cos(beta)
            if(dis1.lt.(-0.001).or.dis1.gt.1.001.or.      &
               dis2.lt.(-0.001).or.dis2.gt.1.001)then
              print*,'qliu test wrong3=',i,j,dis,dis1,dis2,beta
              stop
            end if
            hbwgt(i,j,1) = dis1*dis2
            hbwgt(i,j,2) = (1.-dis1)*(1.-dis2)
            hbwgt(i,j,3) = (1.-dis1)*dis2
            hbwgt(i,j,4) = dis1*(1.-dis2)
          else if(alpha.ge.cint4.and.alpha.le.cint5)then
            found=.true.
            iih1=iih_t
            jjh1=jjh_t
            beta=cint5-alpha
            dis1=dis*sin(beta)
            dis2=dis*cos(beta)
            if(dis1.lt.(-0.001).or.dis1.gt.1.001.or.      &
               dis2.lt.(-0.001).or.dis2.gt.1.001)then
              print*,'qliu test wrong4=',i,j,dis,dis1,dis2,beta
              stop
            end if
            hbwgt(i,j,1) = dis1*(1.-dis2)
            hbwgt(i,j,2) = (1.-dis1)*dis2
            hbwgt(i,j,3) = dis1*dis2
            hbwgt(i,j,4) = (1.-dis1)*(1.-dis2)
          end if
       endif
       endif
    endif
  
    check_sum=abs(hbwgt(i,j,1))+abs(hbwgt(i,j,2))+abs(hbwgt(i,j,3))+abs(hbwgt(i,j,4))
    if(.not. found .or. check_sum>1.02 .or. check_sum<0.98) then
!    print*, 'qingfu test test',alpha,iih_t,jjh_t,iss,ise,jss,jse,i,j
!    print*,iih(i,j,1),jjh(i,j,1),hbwgt(i,j,1),hbwgt(i,j,2),hbwgt(i,j,3),hbwgt(i,j,4)
!    print *, hlon2_tmp,hlon1_r(iih_t,jjh_t),hlon1_r(iih_t+1,jjh_t)
!    print *, hlon2_tmp,hlon1_r(iih_t,jjh_t+2),hlon1_r(iih_t+1,jjh_t+2)
!    print *, hlat2_tmp,hlat1_r(iih_t,jjh_t),hlat1_r(iih_t+1,jjh_t)
!    print *, hlat2_tmp,hlat1_r(iih_t,jjh_t+2),hlat1_r(iih_t+1,jjh_t+2)
!  write(91,'(4I6,4F10.6)') iih(i,j,1),jjh(i,j,1),i,j,hbwgt(i,j,1),hbwgt(i,j,2),hbwgt(i,j,3),hbwgt(i,j,4)
      hbwgt(i,j,:)=0.0
      iih(i,j,:)=0
      jjh(i,j,:)=0
      n=n+1
    elseif(mod(jjh1,2).ne.0)then
      iih(i,j,1)=iih1
      jjh(i,j,1)=jjh1
      iih(i,j,2)=iih1+1
      jjh(i,j,2)=jjh1
      iih(i,j,3)=iih1
      jjh(i,j,3)=jjh1-1
      iih(i,j,4)=iih1
      jjh(i,j,4)=jjh1+1
    else
      iih(i,j,1)=iih1
      jjh(i,j,1)=jjh1
      iih(i,j,2)=iih1+1
      jjh(i,j,2)=jjh1
      iih(i,j,3)=iih1+1
      jjh(i,j,3)=jjh1-1
      iih(i,j,4)=iih1+1
      jjh(i,j,4)=jjh1+1
    end if  
 end do
 end do

 print*,'total grid outside the interpolation =', N

!**********************************************************************************
!   IV. Find weight of (hlat2, hlon2) of the target domain on source domain     *
!**********************************************************************************


! End of g2t2h_egrid

 end subroutine g2t2h_egrid 
 SUBROUTINE G2T2V_egrid( IIV,JJV,                            & ! output grid index and weights 
                         VBWGT,                              & ! output weights in terms of parent grid
                         CLAT1, CLON1,                       & ! input source domain center(-6h)
                         CURRENT_DOMAIN_ID,                  & ! input source domain ID
                         DX,DY,                              & ! input source outmost domain resolution
                         VLAT1,VLON1,                        & ! input source point regular lat and lon 
                         VLAT2,VLON2,                        & ! input target point regular lat and lon 
                         IDS,IDE,JDS,JDE,                    & ! input target outmost domain dimensions
                         ISS,ISE,JSS,JSE,                    & ! source dimensions
                         ITS,ITE,JTS,JTE              )        ! target dimensions
!
!*** XUEJIN ZHANG --- Initial version (07/14/2011)
!Function: Bilnear interpolation weight and indexing for E-grid
!          from (VLAT1,HLON1) to (VLAT2,HLON2)
!
!************************************************************* 
!
!*** FIGURE 1. ARRANGEMENT OF 4 VERTEXES FROM SOURCE DOMAIN
!***
!***                  4
!***
!***                  v
!***             1         2
!***
!***
!***                  3 
!
!points 1,2,3,4 are source points
!point h is target point 
!************************************************************* 
!
!Variables:
!IIV---------I index in term of 4 source points (output)
!JJV---------J index in term of 4 source points (output)
!VBWGT-------weight in term of source point 1 (output)
!VLAT1-------source regular latitude  (input)
!VLON1-------source regular longitude (input)
!VLAT2-------target regular latitude  (input)
!VLON2-------target regular longitude (input)
!CLAT1-------source domain center regular latitude  (input)
!CLON1-------source domain center regular longitude (input) 
!CLAT2-------target domain center regular latitude  (input)
!CLON2-------target domain center regular longitude (input)
!MAX_DOM-----source maximum domain number (input)
!CURRENT_DOMAIN_ID--source domain ID (input)
!I_PARENT_START-----source nest start I in parent domain (input)
!J_PARENT_START-----source nest start I in parent domain (input) 
!DX,DY-------source outmost domain resolution (input)
!RATIO-------source parent-nest domain ratio  (always set = 3)
!PARENT_ID---source PARENT ID (input)
!GRID_ID-----source GRID ID   (input)
!
!IDS---------target outmost domain I-dimension start (input)
!IDE---------target outmost domain I-dimension end   (input)
!JDS---------target outmost domain J-dimension start (input)
!JDE---------target outmost domain J-dimension end   (input)
!ISS---------source array I-dimension start (input)
!ISE---------source array I-dimension end   (input)
!JSS---------source array J-dimension start (input)
!JSE---------source array J-dimension end   (input)
!ITS---------target array I-dimension start (input)
!ITE---------target array I-dimension end   (input)
!JTS---------target array J-dimension start (input)
!JTE---------target array J-dimension end   (input)
!
!************************************************************* 
!     DECLARE VARIABLES
 IMPLICIT NONE
 integer                                :: IDS,IDE,JDS,JDE
 integer                                :: ISS,ISE,JSS,JSE
 integer                                :: ITS,ITE,JTS,JTE
 integer(4), dimension(ITS:ITE,JTS:JTE,4)  :: IIV,JJV 
 real,    dimension(ITS:ITE,JTS:JTE,4)  :: VBWGT
 real                                   :: CLAT1, CLON1
 real                                   :: CLAT2, CLON2
 integer                                :: CURRENT_DOMAIN_ID
 real                                   :: DX,DY
 real,    dimension(ISS:ISE,JSS:JSE)    :: VLAT1,VLON1 
 real,    dimension(ITS:ITE,JTS:JTE)    :: VLAT2,VLON2 
!
! Local variables
 integer                                :: I,J,K,N,ii,jj
 real                                   :: wb, sb, wb0, sb0
 real,    dimension(ISS:ISE,JSS:JSE)    :: VLAT_R,VLON_R 
 real                                   :: slat, slon, rlat, rlon, olat, olon
 real,    dimension(ISS:ISE,JSS:JSE)    :: VLAT1_R,VLON1_R 
 real,    dimension(ITS:ITE,JTS:JTE)    :: VLAT2_R,VLON2_R 
 real,    dimension(ITS:ITE,JTS:JTE)    :: VLAT2_RT,VLON2_RT 
 integer, dimension(ITS:ITE,JTS:JTE)    :: IIV_RT,JJV_RT
 real                                   :: vlat2_tmp, vlon2_tmp
 integer                                :: iiv_t,jjv_t,iiv1,jjv1
 real                                   :: pi,aa,bb,alpha,beta
 real                                   :: res,dis,dis1,dis2
 real                                   :: cint1,cint2,cint3,cint4,cint5
 real                                   :: check_sum
 logical                                :: found
!******************************************
!   I. Create source domain coordinator   *
!******************************************
!
!***************************************************************
!   I.1 Calculate west and south boundary of the source domain *
!***************************************************************
 print*,'inside g2t2v_egrid'
 print*,'dx,dy',dx,dy
 print *, 'ids,ide', ids,ide

 pi=atan(1.0)*4.0
 res=sqrt(dx*dx+dy*dy)
 cint1=-pi/4.
 cint2=pi/4.
 cint3=3.*pi/4.
 cint4=5.*pi/4.
 cint5=7.*pi/4.+0.0001

!*********************************************************************************
!   I.2 Define vlat and vlon of the source domain on rotated lat-lon coordinator *
!*********************************************************************************
 olat = clat1; olon = clon1
 call Convert_LatLon(1, (ISE-ISS+1), (JSE-JSS+1), vlon1, vlat1, vlon1_r, vlat1_r, olon, olat)

! Check vlat_r, vlon_r
! print *, 'vlon_r = ', vlon1_r(iss,jss), vlon1_r(ise,jse)
! print *, 'vlat_r = ', vlat1_r(iss,jss), vlat1_r(ise,jse)
 rlat = vlat1_r(iss,jss); rlon = vlon1_r(iss,jss)
 call Convert_LatLon(2, 1, 1, slon, slat, rlon, rlat, olon, olat)
! print *, 'Check the overlap between read-in (vlat1,vlon1) and (slat, slon) at SW point of source domain'
! print *, 'Defined by g2t2V-egrid, SW vlat1 = ', slat, 'SW vlon1 = ', slon
! print *, 'Read-in from wrfout_d*, SW vlat1 = ', vlat1(iss,jss), 'SW vlon1 = ', vlon1(iss,jss)
 rlat = vlat1_r(ise,jse); rlon = vlon1_r(ise,jse)
 call Convert_LatLon(2, 1, 1, slon, slat, rlon, rlat, olon, olat)
! print *, 'Check the overlap between read-in (vlat1,vlon1) and (slat, slon) at NE point of source domain'
! print *, 'Defined by g2t2V-egrid, NE vlat1 = ', slat, 'NE vlon1 = ', slon
! print *, 'Read-in from wrfout_d*, NE vlat1 = ', vlat1(ise,jse), 'NE vlon1 = ', vlon1(ise,jse)
!**********************************************************************************
!   II. Convert vlat and vlon of the target domain onto source domain             *
!                    rotated lat-lon coordinator                                  *
!**********************************************************************************
 call Convert_LatLon(1, (ITE-ITS+1), (JTE-JTS+1), vlon2, vlat2, vlon2_r, vlat2_r, olon, olat)
! print*,'vlon2_r(1,1),vlat2_r(1,1)=',vlon2_r(1,1),vlat2_r(1,1)
! print*,'vlon1_r(1,1),vlat1_r(1,1)=',vlon1_r(1,1),vlat1_r(1,1)
! print*,'vlon2_r(ite,jte),vlat2_r(ite,jte)=',vlon2_r(ite,jte),vlat2_r(ite,jte)
! print*,'vlon1_r(ise,jse),vlat1_r(ise,jse)=',vlon1_r(ise,jse),vlat1_r(ise,jse)

!**********************************************************************************
!   III. Find index of (hlat2, hlon2) of the target domain on source domain     *
!**********************************************************************************
!
!Function: Find the VERTEX 1 in FIGURE 1
!
 ii=0
!
! Counting the number of points within the source domain
! Purpose: eliminate loops outside of the source domain to reduce the computing time 
!
 iiv_rt = 0; jjv_rt = 0
 if(current_domain_id.eq.1)then
   do j = jts,jte
   do i = its,ite
      vlat2_rt(i,j) = vlat2_r(i,j)
      vlon2_rt(i,j) = vlon2_r(i,j)
      iiv_rt(i,j)   = i
      jjv_rt(i,j)   = j
      ii = ii + 1
   end do
   end do
 else
   ii=0
   do j = jts,jte
   do i = its,ite
     if(vlat2_r(i,j).ge.vlat1_r(iss,jss) .and.       &
        vlat2_r(i,j).lt.vlat1_r(ise,jse)) then
        if(vlon2_r(i,j).ge.vlon1_r(iss,jss) .and.       &
           vlon2_r(i,j).lt.vlon1_r(ise,jse)) then
           vlat2_rt(i,j) = vlat2_r(i,j)
           vlon2_rt(i,j) = vlon2_r(i,j)
           iiv_rt(i,j)   = i
           jjv_rt(i,j)   = j
           ii = ii + 1
        endif
     endif
   enddo
   enddo
 end if
 print *,' total target points within source domain, ', ii
 n=0
 do i = its, ite
 do j = jts, jte
    found=.false.
    if(iiv_rt(i,j).gt.0.and.jjv_rt(i,j).gt.0) then
       vlat2_tmp = vlat2_rt(iiv_rt(i,j),jjv_rt(i,j))
       vlon2_tmp = vlon2_rt(iiv_rt(i,j),jjv_rt(i,j))
       iiv_t = int((vlon2_tmp-vlon1_r(iss,jss))/(2*dx))+1
       jjv_t = 2*int((vlat2_tmp-vlat1_r(iss,jss))/(2*dy))+1
       if(iiv_t.ge.iss.and.iiv_t.lt.ise.and.jjv_t.gt.jss.and.jjv_t.lt.jse)then

       aa=vlon2_tmp-vlon1_r(iiv_t+1,jjv_t+1)
       bb=vlat2_tmp-vlat1_r(iiv_t+1,jjv_t+1)
       dis=sqrt(aa*aa+bb*bb)/res
       if(dis.gt.1..and.dis.lt.1.005)dis=1.0
       if(dis.lt.0.005)then
          found=.true.
          iiv1=iiv_t+1
          jjv1=jjv_t+1
          vbwgt(i,j,1) = 1.0
          vbwgt(i,j,2) = 0.0
          vbwgt(i,j,3) = 0.0
          vbwgt(i,j,4) = 0.0
       else
          alpha=atan2(bb,aa)
          if(alpha.lt.(-pi/4))alpha=alpha+2.*pi
          if(alpha.ge.cint1.and.alpha.lt.cint2)then
            found=.true.
            iiv1=iiv_t+1
            jjv1=jjv_t+1
            beta=cint2-alpha
            dis1=dis*sin(beta)
            dis2=dis*cos(beta)
            if(dis1.lt.(-0.001).or.dis1.gt.1.001.or.      &
               dis2.lt.(-0.001).or.dis2.gt.1.001)then
              print*,'qliu test wrong1=',i,j,dis,dis1,dis2,beta
              stop
            end if
            vbwgt(i,j,1) = (1.-dis1)*(1.-dis2)
            vbwgt(i,j,2) = dis1*dis2
            vbwgt(i,j,3) = dis1*(1.-dis2)
            vbwgt(i,j,4) = (1.-dis1)*dis2
          else if(alpha.ge.cint2.and.alpha.lt.cint3)then
            found=.true.
            iiv1=iiv_t
            jjv1=jjv_t+2
            beta=cint3-alpha
            dis1=dis*sin(beta)
            dis2=dis*cos(beta)
            if(dis1.lt.(-0.001).or.dis1.gt.1.001.or.      &
               dis2.lt.(-0.001).or.dis2.gt.1.001)then
              print*,'qliu test wrong2=',i,j,dis,dis1,dis2,beta
              stop
            end if
            vbwgt(i,j,1) = (1.-dis1)*dis2
            vbwgt(i,j,2) = dis1*(1.-dis2)
            vbwgt(i,j,3) = (1.-dis1)*(1.-dis2)
            vbwgt(i,j,4) = dis1*dis2
          else if(alpha.ge.cint3.and.alpha.lt.cint4)then
            found=.true.
            iiv1=iiv_t
            jjv1=jjv_t+1
            beta=cint4-alpha
            dis1=dis*sin(beta)
            dis2=dis*cos(beta)
            if(dis1.lt.(-0.001).or.dis1.gt.1.001.or.      &
               dis2.lt.(-0.001).or.dis2.gt.1.001)then
              print*,'qliu test wrong3=',i,j,dis,dis1,dis2,beta
              stop
            end if
            vbwgt(i,j,1) = dis1*dis2
            vbwgt(i,j,2) = (1.-dis1)*(1.-dis2)
            vbwgt(i,j,3) = (1.-dis1)*dis2
            vbwgt(i,j,4) = dis1*(1.-dis2)
          else if(alpha.ge.cint4.and.alpha.le.cint5)then
            found=.true.
            iiv1=iiv_t
            jjv1=jjv_t
            beta=cint5-alpha
            dis1=dis*sin(beta)
            dis2=dis*cos(beta)
            if(dis1.lt.(-0.001).or.dis1.gt.1.001.or.      &
               dis2.lt.(-0.001).or.dis2.gt.1.001)then
              print*,'qliu test wrong4=',i,j,dis,dis1,dis2,beta
              stop
            end if
            vbwgt(i,j,1) = dis1*(1.-dis2)
            vbwgt(i,j,2) = (1.-dis1)*dis2
            vbwgt(i,j,3) = dis1*dis2
            vbwgt(i,j,4) = (1.-dis1)*(1.-dis2)
          end if
       endif
       endif
    endif
    check_sum=abs(vbwgt(i,j,1))+abs(vbwgt(i,j,2))+abs(vbwgt(i,j,3))+abs(vbwgt(i,j,4))
    if(.not.found .or. check_sum>1.02 .or. check_sum<0.98) then
      n=n+1
      iiv(i,j,:)=0
      jjv(i,j,:)=0
      vbwgt(i,j,:)=0.
!    print*, 'qingfu test test',alpha
!    print*,iiv(i,j,1),jjv(i,j,1),vbwgt(i,j,1),vbwgt(i,j,2),vbwgt(i,j,3),vbwgt(i,j,4)
!    print *, vlon2_tmp,vlon1_r(iiv_t,jjv_t),vlon1_r(iiv_t+1,jjv_t)
!    print *, vlon2_tmp,vlon1_r(iiv_t,jjv_t+2),vlon1_r(iiv_t+1,jjv_t+2)
!    print *, vlat2_tmp,vlat1_r(iiv_t,jjv_t),vlat1_r(iiv_t+1,jjv_t)
!    print *, vlat2_tmp,vlat1_r(iiv_t,jjv_t+2),vlat1_r(iiv_t+1,jjv_t+2)
!    stop
    elseif(mod(jjv1,2).ne.0)then
      iiv(i,j,1)=iiv1
      jjv(i,j,1)=jjv1
      iiv(i,j,2)=iiv1+1
      jjv(i,j,2)=jjv1
      iiv(i,j,3)=iiv1+1
      jjv(i,j,3)=jjv1-1
      iiv(i,j,4)=iiv1+1
      jjv(i,j,4)=jjv1+1
    else
      iiv(i,j,1)=iiv1
      jjv(i,j,1)=jjv1
      iiv(i,j,2)=iiv1+1
      jjv(i,j,2)=jjv1
      iiv(i,j,3)=iiv1
      jjv(i,j,3)=jjv1-1
      iiv(i,j,4)=iiv1
      jjv(i,j,4)=jjv1+1
    end if  
 end do
 end do

 print*,'total grid outside the interpolation =', N

!**********************************************************************************
!   IV. Find weight of (vlat2, vlon2) of the target domain on source domain     *
!**********************************************************************************

! End of g2t2v_egrid

 end subroutine g2t2v_egrid 
!==============================================================================

   SUBROUTINE Convert_LatLon(mode, im, jm, slon, slat, rlon, rlat, olon, olat)
!******************************************************************************
! 
!  Convert between standard & rotated latlon coordinates 
!  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
!  This subroutine converts latlon coordinates between two coordinate systems, 
!  the standard geographic system {S} & the rotated spherical coord system {R} 
!  obtained by rotating the origin of {S} to (olat,olon) as the new origin: 
!  (0,0) -> (olat,olon) = (0,0)'. The poles are not allowed for conversion. 
!  
!  mode = 1 : convert standard coord (alat,alon) to rotated  coord (blat,blon) 
!  mode = 2 : convert rotated  coord (blat,blon) to standard coord (alat,alon) 
!  Author: Kao-San Yeh (AOML/HRD)
!______________________________________________________________________________
   Implicit None 
!  -------------------------------------------------------------------- [input] 
   Integer  mode, im, jm      !* mode= 1/2: Std -> Rot / Rot -> Std coord 
   Real(4)  olon, olat        !* origin of rotated grid in standard coord 
!  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - [in/out] 
   Real(4)  slon(im,jm), slat(im,jm) !* standard latlon coordinates (deg) 
   Real(4)  rlon(im,jm), rlat(im,jm) !* rotated  latlon coordinates (deg) 
!  -------------------------------------------------------------------- [local] 
   Real(8)  alon(im,jm), alat(im,jm) !* standard latlon coordinates (deg) 
   Real(8)  blon(im,jm), blat(im,jm) !* rotated  latlon coordinates (deg) 
   Real(8)  pi_cnst, deg2rad, eps1, sqrt2_, xcos, xsin, ycos, ysin 
   Real(8)  olamda, otheta, alamda, atheta, blamda, btheta 
   Real(8)  aaa, bbb, ccc, ddd, eee, xxx, yyy, zzz 
   Integer   i, j 
   
!           *     *     *     *     *     *     *     *     *     * 
   
   pi_cnst = DAcos(-1.D0) ; deg2rad = DAcos(-1.D0)/180. 
     eps1  = 1. - 1.D-6 ;  sqrt2_ = 1./sqrt(2.) 
   
!* Enhance precision for trigonometric functions 
!* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
   alon(:,:) = slon(:,:) ; alat(:,:) = slat(:,:) 
   blon(:,:) = rlon(:,:) ; blat(:,:) = rlat(:,:) 
   olamda = olon*deg2rad ; otheta = olat*deg2rad 
      aaa = Dcos(otheta) ; bbb = Dsin(otheta) 
   
!* Convert standard Earth latlon to rotated Model latlon 
!* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
   IF ( mode == 1 ) THEN  !* - - - - - - - - - - - - - - - - - - - - 
   
   DO j = 1, jm 
   DO i = 1, im 
      yyy = alat(i,j)*deg2rad 
      xxx = alon(i,j)*deg2rad - olamda 
     xcos = Dcos(xxx) ; xsin = Dsin(xxx) 
     ycos = Dcos(yyy) ; ysin = Dsin(yyy) 
!    - - - - - - - - - - - - - - - - - - 
      zzz = aaa*ysin - bbb*ycos*xcos 
      zzz = max(-eps1, min(zzz, eps1)) !* Poles undefined 
     btheta = DAsin(zzz) ; eee = 1./Dcos(btheta) 
      ccc = ( bbb*ysin + aaa*ycos*xcos )*eee 
      ddd = (                ycos*xsin )*eee 
      IF (  ccc > sqrt2_ ) THEN 
         blamda = DAsin(ddd) 
      ELSEIF (  ccc  < -sqrt2_ ) THEN 
         blamda = pi_cnst*Sign(1.D+0,ddd) - DAsin(ddd) 
      ELSE ! ( |ccc| <= sqrt2_ ) ! 
         blamda = DAcos(ccc)*Sign(1.D+0,ddd) 
      ENDIF 
      blat(i,j) = btheta/deg2rad 
      blon(i,j) = blamda/deg2rad 
   ENDDO 
   ENDDO 
   
!* Convert rotated Model latlon to standard Earth latlon
!* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
   ELSE !* ( mode == 2 ) - - - - - - - - - - - - - - - - - - - - - - 
   
   DO j = 1, jm 
   DO i = 1, im 
      yyy = blat(i,j)*deg2rad 
      xxx = blon(i,j)*deg2rad 
     xcos = Dcos(xxx) ; xsin = Dsin(xxx) 
     ycos = Dcos(yyy) ; ysin = Dsin(yyy) 
!    - - - - - - - - - - - - - - - - - - 
      zzz = aaa*ysin + bbb*ycos*xcos 
      zzz = max(-eps1, min(zzz, eps1)) !* Poles undefined 
     atheta = DAsin(zzz) ; eee = 1./Dcos(atheta) 
      ccc = (-bbb*ysin + aaa*ycos*xcos )*eee 
      ddd = (                ycos*xsin )*eee 
      IF (  ccc > sqrt2_ ) THEN 
         alamda = DAsin(ddd) + olamda 
      ELSEIF (  ccc  < -sqrt2_ ) THEN 
         alamda = pi_cnst*Sign(1.D+0,ddd) - DAsin(ddd) + olamda 
      ELSE ! ( |ccc| <= sqrt2_ ) ! 
         alamda = DAcos(ccc)*Sign(1.D+0,ddd) + olamda 
      ENDIF 
      alat(i,j) = atheta/deg2rad 
      alon(i,j) = alamda/deg2rad 
   ENDDO 
   ENDDO 
   
   ENDIF  !* - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
   
!* Recover single precision for output 
!* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
   slon(:,:) = alon(:,:) ; slat(:,:) = alat(:,:) 
   rlon(:,:) = blon(:,:) ; rlat(:,:) = blat(:,:) 
   
   END SUBROUTINE Convert_LatLon
 
