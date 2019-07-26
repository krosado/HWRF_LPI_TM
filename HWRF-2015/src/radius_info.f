!                         radius_info_cent_v1.1
!
! This program calculates 10 m maximum wind speed and its radius and azimuth.
! The radii of 50 kt and 34 kt wind are also calculated.
! The center is determined as the smallest wind speed or maximum of total
! tangential wind
!
! Output(text):
!     Maximum Wind, radius of MW, and azimuth of MW
!     mean radii of 50 kt for four quadrants (NE, NW, SW and SE)
!     mean radii of 34 kt for four quadrants (NE, NW, SW and SE)
!
! Ver 1.0, JAN 06 2012, Coded by IH Kwon
! Ver 1.1, JUN 12 2011, Updated center search for the wind speed below 17m/s

!-----------------------------------------------------------------------
      program radius_info
!-----------------------------------------------------------------------
      implicit none
      integer,parameter :: mt=400,nt=80,nt1=nt-1
      real(kind=8),allocatable,dimension(:,:) :: xlon,ylat
      real(kind=8),allocatable,dimension(:,:) :: plon,plat,wndgr8
      real(kind=4),allocatable,dimension(:,:) :: wmlon,wmlat,ugrd,vgrd
      real(kind=4),allocatable,dimension(:,:) :: wndgr,twsum
      real(kind=4),allocatable,dimension(:,:) :: utmp,vtmp
      real(kind=4),allocatable,dimension(:,:) :: rork1,rork2,vtgr,vrgr
      real(kind=4),allocatable,dimension(:)   :: xl1d,yl1d
      real(kind=8),dimension(0:nt1,mt) :: wndpo8
      real(kind=4),dimension(0:nt1,mt) :: wndpo
      real(kind=4),dimension(0:nt1) :: amax,rmax,r50,r34
      real(kind=8) :: cenx8,ceny8,dmt
      real(kind=4) :: cenx,ceny,wdist,wang,adis
      real(kind=4) :: clat,clon,slat,slon,dlat,dlon,dclat,dclon
      real(kind=4) :: undef,radius,pi,pi180,angle,dang,rea
      real(kind=4) :: Vmax,w50,w34,sum,int
      real(kind=4) :: cmin,rmw,ttmp,rend
      real(kind=4) :: srchrad=0.21        ! radius for the center searching
      real(kind=4) :: tanwrad=0.5         ! radius for the Vt calculation
      real(kind=4) :: wthrhold=15.

      integer,dimension(0:nt1) :: maxr,m50,m34
      integer,dimension(4 ) :: ns,ne,radi50,radi34
      integer :: ib,jb,kb
      integer :: i,ic,j,jiv,l,m,n,mx,my,mthr,ibc,jbc
      integer :: itc,imx,is,ie,il
      integer :: jtc,jmx,js,je,jl
      integer :: msta,mend,mlimt,mradius,mradiex
      namelist /idata/ clat,clon,slat,slon,dlat,dlon,ib,jb,kb

! -----------------------------------------------
       dmt  = 1.D3          ! radius interval [m] (total radius= mt*dmt)
       undef= 9.999E+20
       radius= 6.37100E+06                     ! [m]
       pi   = 3.141592653589793238             ! [radial]
       pi180= pi/180.                          ! [radial]
       dang = 360. /nt                         ! [degree]
       mthr = nt /8
       w50= 50. / 1.943844                     ! [m/s]
       w34= 34. / 1.943844                     ! [m/s]
       
      if(slon < 0.) slon=slon+360.
      if(clon < 0.) clon=clon+360.

       read(11,NML=idata)

      allocate( xlon(ib,jb),ylat(ib,jb),plon(ib,jb),plat(ib,jb) )
      allocate( wmlon(ib,jb),wmlat(ib,jb),ugrd(ib,jb),vgrd(ib,jb) )
      allocate( rork1(ib,jb),rork2(ib,jb),vtgr(ib,jb),vrgr(ib,jb) )
      allocate( wndgr8(ib,jb) )

      do i= 1,ib
         xlon(i,:)= slon +(i-1) *dlon
      enddo
      do j= 1,jb
         ylat(:,j)= slat +(j-1) *dlat
      enddo

      dclon=xlon(ib/2+1,1)
      dclat=ylat(1,jb/2+1)
 
      print*,'slon,slat,dlon,dlat'
      print*, slon,slat,dlon,dlat
      print*,'Storm center',clon,clat
      print*,'domain center',dclon,dclat

! -------------------

       open(14,file='ugrd10m.bin',recl=ib*jb*4,
     &      form='unformatted',access='direct')
       open(15,file='vgrd10m.bin',recl=ib*jb*4,
     &      form='unformatted',access='direct')
       open(21,file='wind_po.bin',recl=mt*nt*4,
     &      form='unformatted',access='direct')
       open(22,file='radius.bin',recl=nt*4,
     &      form='unformatted',access='direct')
       open(31,file='vtgrd10m.bin',recl=ib*jb*4,
     &      form='unformatted',access='direct')
       open(32,file='vrgrd10m.bin',recl=ib*jb*4,
     &      form='unformatted',access='direct')
       open(72,file='radinfo.txt')

       adis= sqrt( (dclat-clat)**2. +(dclon-clon)**2. )
      print*,'distance from the domain center to the storm center',adis

      if(adis > 2.7)then
         print*,'Storm center is far away from the domain center'
         wndpo= undef
         rmax = undef
         r50  = undef
         r34  = undef
         write(21,rec=1) wndpo
         write(22,rec=1) rmax
         write(22,rec=2) r50
         write(22,rec=3) r34
         goto 998
       endif

       read(14,rec=1) rork1
       ugrd(:,:)= rork1(:,:)

       read(15,rec=1) rork1
       vgrd(:,:)= rork1(:,:)

!--------------------------
!.. Read TC center in case exists
      print*,'reading storm center...'
       open(17,file='stoCenter.txt')
       read(17,err=999,fmt='(2F10.3)') clat,clon
      close(17)
      print*,'Storm center from stoCenter.txt',clat,clon

       cenx= clon
       ceny= clat

       itc= (ib-1) *(clon-xlon(1,1)) /(xlon(ib,1)-xlon(1,1)) +1
       jtc= (jb-1) *(clat-ylat(1,1)) /(ylat(1,jb)-ylat(1,1)) +1

       rend= tanwrad *2.5

       is = itc -rend/dlat
       ie = itc +rend/dlat
       js = jtc -rend/dlat
       je = jtc +rend/dlat
       imx= ie -is +1
       jmx= je -js +1
 
      print*,'srchrad,tanwrad,rend,dlat:',srchrad,tanwrad,rend,dlat
      print*,'is,ie,js,je,imx,jmx:',is,ie,js,je,imx,jmx

      allocate( wndgr(imx,jmx) )
      allocate( xl1d(imx),yl1d(jmx) )

      do j =1,jmx
      do i =1,imx
         il= is+i-1
         jl= js+j-1
         wndgr(i,j)= sqrt( ugrd(il,jl)**2. +vgrd(il,jl)**2. )
      enddo
      enddo

!.. Maximum wind speed
         Vmax= 0.
      do j =1,jmx
      do i =1,imx
        if(wndgr(i,j) > Vmax .and. wndgr(i,j) < 1.e10)then
           Vmax= wndgr(i,j)
        endif
      enddo
      enddo

      print*,'Maximum wind speed:',Vmax

      do i =1,imx
         xl1d(i)= xlon(is+i-1,1)
      enddo
      do j =1,jmx
         yl1d(j)= ylat(1,js+j-1)
      enddo


      if(Vmax > wthrhold)then     ! Storm center based on the wind speed if Vmax > 15 m/s

        call findCenter
     &  (wndgr,xl1d,yl1d,clon,clat,cenx,ceny,imx,jmx,srchrad,-1)
!                                                                    -1: find the minimum

      else                   ! Storm center based on total tangential wind

         mradius= tanwrad/dlat
         mradiex= 0.1/dlat
        print*,'mradius,mradiex:',mradius,mradiex

        allocate( utmp(imx,jmx),vtmp(imx,jmx),twsum(imx,jmx) )

        do j=1,jmx
        do i=1,imx
           il= is+i-1
           jl= js+j-1
!          ttmp= sqrt(ugrd(il,jl)**2.+vgrd(il,jl)**2.)
!          utmp(i,j)= ugrd(il,jl) /ttmp
!          vtmp(i,j)= vgrd(il,jl) /ttmp
           utmp(i,j)= ugrd(il,jl)
           vtmp(i,j)= vgrd(il,jl)
        enddo
        enddo

        call tWindSum(utmp,vtmp,twsum,imx,jmx,mradius,mradiex)
        call findCenter
     &  (twsum,xl1d,yl1d,clon,clat,cenx,ceny,imx,jmx,srchrad,1)
!                                                                    1: find the maximum 
        deallocate( utmp,vtmp,twsum )

      endif


      write(*,*) 'Storm center at surface: ',cenx,ceny

  999 continue

       cenx8= cenx
       ceny8= ceny

! ------------------------ Wind speed in polar coordinate

      do j=1,jb
      do i=1,ib
         wndgr8(i,j)= sqrt(ugrd(i,j)**2.+vgrd(i,j)**2.)
      enddo
      enddo

! --- Transform the field into polar coordinate
      call trans2Polar
     &(wndgr8,xlon,ylat,ib,jb,cenx8,ceny8,wndpo8,mt,nt,dmt)

      do m= 1,mt
      do n= 0,nt1
        if(wndpo8(n,m) > 1.e10)then
           wndpo(n,m) = undef
        else
           wndpo(n,m) = wndpo8(n,m)
        endif
      enddo
      enddo
      write(21,rec=1) wndpo

! ------------------------ Calculate RMW

         amax(:)= undef
         rmax(:)= undef
         maxr(:)= 9999
         msta =  10.e3 /dmt         ! Minimum of RMW
         mend = 300.e3 /dmt         ! Maximum of RMW
         mlimt= 800.e3 /dmt         ! Maximum of r34

      do n= 0, nt1
         amax(n)= 0.
        do m= msta, mend
          if(wndpo(n,m) > amax(n) .and. wndpo(n,m) < 1.e10)then
             amax(n)= wndpo(n,m)
             maxr(n)= m
          endif
        enddo
        if(maxr(n)== mend)then
           rmax(n)= undef
           maxr(n)= 9999
        else
           rmax(n)= (maxr(n)-1) *dmt *1.e-3    ! [km]
        endif
      enddo

!.. azimuthal average

         sum= 0.
       ic= 0
      do n= 0, nt1
        if(maxr(n) .ne. 9999)then
           ic = ic +1
           sum= sum + rmax(n)
        endif
      enddo

       rmw= sum /ic                    ! [km]
      print*,'RMW=',rmw

   
! ------------------------ radius of 50 kt and radius of 34 kt

      do n= 0, nt1
        if(amax(n) >= w50 .and. maxr(n) /= 9999 )then
             m= maxr(n)
          do while( wndpo(n,m) >= w50 .and. wndpo(n,m) < 1.e10 )
             m= m +1
          enddo
             m50(n)= m
             r50(n)= (m-1) *dmt *1.e-3         ! [km]

          do while( wndpo(n,m) >= w34 .and. wndpo(n,m) < 1.e10 )
             m= m +1
          enddo
             m34(n)= m
             r34(n)= (m-1) *dmt *1.e-3         ! [km]

        elseif(amax(n) >= w34 .and. maxr(n) /= 9999 )then
             m50(n)= 9999
             r50(n)= undef
             m= maxr(n)
          do while( wndpo(n,m) >= w34 .and. wndpo(n,m) <1.e10)
             m= m +1
          enddo
             m34(n)= m
             r34(n)= (m-1) *dmt *1.e-3         ! [km]

        else
             m50(n)= 9999
             m34(n)= 9999
             r50(n)= undef
             r34(n)= undef
        endif
      enddo

!.. RMW for each direction

      write(22,rec=1) rmax

!.. 50 kt radius for each direction

         ic= 0
      do n= 0, nt1
         m= m50(n)
        if(m == 9999 .or. wndpo(n,m) > 1.e10)then
           r50(n) = undef
        else
           ic= 1
        endif
      enddo

      if(ic == 0)then
         print*,'No R50 information'
         r50(0)= 0.
      endif

      write(22,rec=2) r50

!.. 34 kt radius for each direction

         ic= 0
      do n= 0, nt1
         m= m34(n)
        if(m == 9999 .or. wndpo(n,m) > 1.e10)then
           r34(n) = undef
        else
           ic= 1
        endif
      enddo

      if(ic == 0)then
         print*,'No R34 information'
         r34(0)= 0.
      endif

      write(22,rec=3) r34


!.. Calculate RMW (azimuthal average)

         sum= 0.
         ic= 0
        do n= 0, nt1
          if(maxr(n) .ne. 9999)then
             ic = ic +1
             sum= sum + rmax(n)
          endif
        enddo

        if(ic >= mthr)then
           rmw= sum /ic                    ! [km]
        else
           rmw= 9999.
        endif

         ns(1)= 1
         ne(4)= nt1

!.. Calculate maximum wind

           Vmax= 0.
        do n= 0, nt1
          if(amax(n) > Vmax .and. maxr(n) .ne. 9999)then
             Vmax= amax(n)
             wang= n *360 /nt
          endif
        enddo

!.. 34 kt & 50 kt radius for each quadrant

      do l= 2, 4
         rea= nt *(l-1) /4.
         int= nt *(l-1) /4
         ne(l-1)= int
         ns(l  )= int +1
        if(rea == int) ne(l-1)= ne(l-1) -1
      enddo
       
      do l= 1, 4

         sum= 0.
         ic= 0
        do n= ns(l), ne(l)
          if(m50(n) .ne. 9999)then
             ic = ic +1
             sum= sum + m50(n) 
          endif
        enddo
 
        if(ic >= mthr)then
           radi50(l)= sum /ic *dmt *1.e-3         ! [km]
        else
           radi50(l)= 9999
        endif

         sum= 0.
         ic= 0
        do n= ns(l), ne(l)
          if(m34(n) .ne. 9999)then
             ic = ic +1
             sum= sum + m34(n)
          endif
        enddo

        if(ic >= mthr)then
           radi34(l)= sum /ic *dmt *1.e-3         ! [km]
        else
           radi34(l)= 9999
        endif

      enddo

      write(72,'(3F7.1,8I6)')
     &      Vmax,rmw,wang,(radi34(l),l=1,4),(radi50(l),l=1,4)

! ------------------- Tangential & radial wind

! --- Polar coordinate information of corresponding grid point
!     call polarinfo
!    &(xlon,ylat,ib,jb,cenx8,ceny8,plon,plat)

!                                             vtgr: tangential wind
!                                             vrgr: radial wind
!     do j=1,jb
!     do i=1,ib
!       if(ugrd(i,j) > 1.e10 .or. vgrd(i,j) > 1.e10)then
!          vtgr(i,j)= undef
!          vrgr(i,j)= undef
!       else
!          angle = plon(i,j) *pi180
!          vtgr(i,j)= -ugrd(i,j)*sin(angle) +vgrd(i,j)*cos(angle)
!          vrgr(i,j)=  ugrd(i,j)*cos(angle) +vgrd(i,j)*sin(angle)
!       endif
!     enddo
!     enddo

!     write(31,rec=1) vtgr
!     write(32,rec=1) vrgr

  998 continue
      close(14)
      close(15)
      close(21)
      close(22)
      close(31)
      close(32)
      close(72)

      print*,'== Surface wind diagnosis is complete =='
!-----------------------------------------------------------------------
      end program radius_info
!-----------------------------------------------------------------------
