program swath
  !
  !
  !        parameter (im=59,jm=99,ntimes=1000)
  !        parameter (im=153,jm=271,ntimes=1000)
  parameter ( ntimes=1000 )
  integer :: im,jm
  REAL, DIMENSION(:,:,:),  ALLOCATABLE :: HBWGT,VBWGT
  REAL, DIMENSION(:),  ALLOCATABLE :: xlat,xlon
  REAL, DIMENSION(:,:),  ALLOCATABLE :: ncount
  REAL, DIMENSION(:,:,:),  ALLOCATABLE :: wno,idn
  REAL, DIMENSION(:,:),  ALLOCATABLE :: xwind10,xacprec
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: IIH,JJH,IIV,JJV
  real, allocatable, dimension(:,:) :: hlatm,hlonm,u10,v10,accpre
  !       dimension hlatm(im,jm),hlonm(im,jm),u10(im,jm),v10(im,jm)
  !       dimension accpre(im,jm)
  dimension stormlat(ntimes),stormlon(ntimes)



  real, dimension(:)   ,  allocatable :: htvar,xglon,yglat

  real, dimension(:,:), allocatable :: hlatb,hlonb,wind10b,acpreb
  real, dimension(:,:), allocatable :: vlatb,vlonb
  real, dimension(:,:), allocatable :: xdiff,ibgmsk

  !
  character*3 fname
  !
  !      
  input=75
  nwsw=76
  npsw=77
  !      hourly interval in atcf & stats files
  nunint=6
  nunint=3  !!  track stat, every 3 hours
  !      number of auxiliary files per hour
  nauxphr=1
  !      read lon & lats for fine grid over entire parent domain
  ! DLMD=DLMD/3./3. ! 2.
  ! DPHD=DPHD/3./3. ! 2.
  IDE=160
  IDE= 647                   !3x215+2
  IDE= 1937
  JDE=310
  JDE= 1294                  !3x433+1
  JDE= 3880

  im=-99
  jm=-99

  ALLOCATE ( hlatb(IDE,JDE)  )
  ALLOCATE ( hlonb(IDE,JDE)  )
  ALLOCATE ( vlatb(IDE,JDE)  )
  ALLOCATE ( vlonb(IDE,JDE)  )
  ALLOCATE ( xdiff(IDE,JDE)  )
  ALLOCATE ( ibgmsk(IDE,JDE)  )
  ALLOCATE ( wind10b(IDE,JDE)  )
  ALLOCATE ( acpreb (IDE,JDE)  )

  ALLOCATE ( htvar (IDE*JDE)  )
  ALLOCATE ( xglon (IDE*JDE)  )
  ALLOCATE ( yglat (IDE*JDE)  )


  do j=1,jde
     do i=1,ide
        read(65,*) irow,jrow,hlonb(i,j),hlatb(i,j)
        ibgmsk  (i,j) = 0
        wind10b (i,j) = 0.0
        acpreb   (i,j) = 0.0
     enddo
  enddo

  print *, 'hlonb(i,j),hlatb(i,j) corner'
  print *, hlonb(1,1),hlatb(1,1),hlonb(ide,jde),hlatb(ide,jde)

  !      read unix file to get storm positions and ending hour... ihour
  do nt=1,ntimes
     read(12,4049,end=105) hour,stormlon(nt),stormlat(nt)
     nthour=nt-1
     ihour=nint(hour)
4049 format(5x,F5.1,7X,F8.2,6X,F7.2)
  enddo
105 continue

  !      calculate no of time levels of auxilary files to read
  ntmax=nunint*nthour
  !      ntmax=10
  print *, 'number of storm positions & aux files to read ', nthour,ntmax


  !      we now have fine grid coordinates everywhere
  !      read through all times to load data 
  !
  windbig=0.0
  timeloop: do nt=1,ntmax
     hour=float(nt)/float(nauxphr)
     !      read filename for each time level
     read(66,'(a3)',end=100) fname
     open(input,file=trim(fname),form='unformatted')
     print *, 'processing file ', fname
     read(input) imfile,jmfile
     print *, 'im,jm on file = ' , imfile,jmfile
     if(im/=imfile .or. jm/=jmfile) then
        if(allocated(hlatm)) deallocate(hlatm)
        if(allocated(hlonm)) deallocate(hlonm)
        if(allocated(u10)) deallocate(u10)
        if(allocated(v10)) deallocate(v10)
        if(allocated(accpre)) deallocate(accpre)
        im=imfile
        jm=jmfile
        allocate(hlatm(im,jm),hlonm(im,jm),u10(im,jm),v10(im,jm))
        allocate(accpre(im,jm))
     endif

     read(input) u10
     read(input) v10
     read(input) hlatm
     read(input) hlonm
     read(input) accpre

     ! open(99,file='look'//trim(fname))
     !  write(99,*)im,jm
     !  write(99,*)hlatm
     !  write(99,*)hlonm
     ! close(99)
     ! print *,'printing ','look'//trim(fname),'nt=',nt
303  format(I0,',',I0,' mesh pt: hlat=',F0.3,' hlon=',F0.3,' u10=',F0.3,' v10=',F0.3,' acpre=',F0.3)
     print 303,im/2,jm/2,hlatm(im/2,jm/2),hlonm(im/2,jm/2),u10(im/2,jm/2),v10(im/2,jm/2),accpre(im/2,jm/2)
     !      
     hlatsw=hlatm(1,1)
     hlonsw=hlonm(1,1)
     print *, 'hlatsw,hlonsw ', hlatsw,hlonsw
     ! calculate diff between nest and parent fine grid
     do j=1,jde
        do i=1,ide
           !      xdiff(i,j) = abs(hlonb(i,j)-hlonsw)
           xdiff(i,j) = (hlonb(i,j)-hlonsw)**2 + (hlatb(i,j)-hlatsw)**2
        enddo
     enddo
     !      determine index of 1st nest point into parent-fine mesh array...
     !
     !      i1dex=minval(xdiff,ide*jde)+1
     i1dex=ismin (ide*jde,xdiff,1)  
     !      idex=mod(i1dex,ide) + 1
     idex=mod(i1dex,ide)
     jdex=i1dex/ide + 1

     print *, 'i...j index into big parent 1d-idnex ', idex,jdex, i1dex
     print *, 'lat of big and mesh array ', hlatb(idex,jdex), hlatsw
     print *, 'lon of big and mesh array ', hlonb(idex,jdex), hlonsw
     !
     !
     !      now load mesh array data into big parent-fine mesh array.

     wind10mx=0.0
     do j=1,jm
        do i=1,im
           wind10=u10(i,j)*u10(i,j) + v10(i,j)*v10(i,j)
           wind10m=sqrt(wind10)
           wind10mx=amax1(wind10m,wind10mx)
           wind10b(idex-1+i,jdex-1+j) =  amax1(wind10m,wind10b(idex-1+i,jdex-1+j))     
           acpreb (idex-1+i,jdex-1+j) =  amax1(accpre(i,j), acpreb (idex-1+i,jdex-1+j))
           ibgmsk (idex-1+i,jdex-1+j) =  1
           windbig=amax1(wind10b(idex-1+i,jdex-1+j),windbig)
        enddo
     enddo
     wind10mx=1.944*wind10mx
     print *, 'wind at corner of mesh..index', wind10m, wind10b(idex,jdex), idex,jdex
     print *, 'nt max 10m wind on mesh & on big grid up to now ', nt, wind10mx, windbig
     !      write out hourly windmax at 10 meters
     write(78,2900)  hour, wind10mx
2900 format(1x,'HOUR: ', f5.1,5x, 'MAX SURF WIND (KNOTS): ', f5.1)
     !      end of time loop
  enddo timeloop
  !
100 continue
  print *, 'finished reading aux files ntimes= ', ntmax

  windmax=0.0
  windmin=999.
  do j=1,jde
     do i=1,ide
        windmax=amax1(wind10b(i,j),windmax)
        windmin=amin1(wind10b(i,j),windmin)
     enddo
  enddo
  print *, 'max and min on entire big domain ',  windmax, windmin

  !
  !      we now have max values in parent-fine mesh arrays of rotated e-grid
  !      use the routine G2T2H to interpolate to regular lat-lon grid  instead....

  !     now determine output domain of lat-lon grid
  slat=+999.
  elon=-999.
  xnlat=-999.
  wlon=+999.
  nobigs=0
  !
  do j=1,jde
     do i=1,ide

        if(ibgmsk(i,j).eq.1)  then
           nobigs=nobigs+1
           slat=amin1(hlatb(i,j),slat)
           xnlat=amax1(hlatb(i,j),xnlat)
           wlon=amin1(hlonb(i,j),wlon)
           elon=amax1(hlonb(i,j),elon)
        endif

     enddo
  enddo

  reslatlon=0.1
  ! NOTE: If you change reslatlon, you MUST change nsearch in interadsp.f

  !      add 5 deg to surrounding moving mesh swath
  slat=float(nint(slat))-3.0
  xnlat=float(nint(xnlat))+3.0
  wlon=float(nint(wlon))-3.0
  elon=float(nint(elon))+3.0
  !
  imax=nint((elon-wlon)/reslatlon)+1
  jmax=nint((xnlat-slat)/reslatlon)+1

  print *, 'wlon,elon,slat,xnlat,imax,jmax', wlon,elon,slat,xnlat,imax,jmax

  ALLOCATE ( xlat(jmax))
  ALLOCATE ( xlon(imax))
  ALLOCATE ( xwind10(imax,jmax))
  ALLOCATE ( xacprec(imax,jmax))

  ALLOCATE ( ncount(imax,jmax))
  ALLOCATE ( wno( 5,imax,jmax)) 
  ALLOCATE ( idn( 5,imax,jmax))
  !
  !      calculate lat-lon coordinates
  do j=1,jmax
     xlat(j)= slat + float(j-1)*reslatlon
  enddo
  do i=1,imax
     xlon(i)= wlon + float(i-1)*reslatlon
  enddo
  print *, 'xlat(1)..xlon(imax) ',xlat(1),xlon(1),xlat(jmax),xlon(imax)

  !
  !      now calculate weights to interpolate from fine-resolution parent domain
  !      to regular lat-lon
  !
  !
  np=0
  do jrot=1,jde
     do irot=1,ide
        !
        if(ibgmsk(irot,jrot).eq.1)  then
           np=np+1 
           xglon(np)=hlonb(irot,jrot)
           yglat(np)=hlatb(irot,jrot)
           !    convert to knots from m/s
           htvar(np)=1.944*wind10b(irot,jrot)
        endif
        !
     enddo
  enddo
  npts=np
  md=1
  print *, 'ide*jde , nobigs,npts ', ide*jde, nobigs,npts
  call interad(md,npts,xglon,yglat,htvar,                               &
       imax,jmax,imax,xlon,xlat,xwind10,wno,idn,ncount,                  &
       slat,wlon,reslatlon)

  print *, 'ide*jde , npts ', ide*jde, npts

  !    now interpolate prec

  np=0
  do jrot=1,jde
     do irot=1,ide
        !
        if(ibgmsk(irot,jrot).eq.1)  then
           np=np+1
           !    convert from meters to inches   ...(100cm/m)* (in/2.54cm)
           htvar(np)=acpreb(irot,jrot)*100./2.54
        endif
        !
     enddo
  enddo
  npts=np
  md=3
  call interad(md,npts,xglon,yglat,htvar,                               &
       imax,jmax,imax,xlon,xlat,xacprec,wno,idn,ncount,                  &
       slat,wlon,reslatlon)


  !     now write data out to text file for NHC
  !     make the ascii files for Michelle's color plots
  write(nwsw,3332)xlon(1),xlon(imax),xlat(1),xlat(jmax),reslatlon,imax,jmax
  write(npsw,3332)xlon(1),xlon(imax),xlat(1),xlat(jmax),reslatlon,imax,jmax
3332 format(5f9.2,2i5)
  do  j = 1 , jmax
     do  i = 1 , imax
        write(nwsw,3333) xlat(j),xlon(i),xwind10(i,j)
        write(npsw,3333) xlat(j),xlon(i),xacprec(i,j)
3333    format(3f9.2)
     enddo
  enddo
  !     output stream data for grads..
  !
  open(88,form='unformatted',ACCESS='DIRECT',RECL=(imax*jmax*4),STATUS='UNKNOWN')
  write(88,rec=1) xwind10
  write(88,rec=2) xacprec
  !
  !     output CTL file
  print *, 'STRAT output GRADS ctl file, swath.ctl'
  open(91,file='swath.ctl',form='formatted',status='UNKNOWN')
  write(91,*)'DSET ^fort.88'
  write(91,*)'TITLE 10m max wind and rain'
  write(91,*)'UNDEF -999'
  write(91,*)'OPTIONS byteswapped'
  write(91,9100)imax,xlon(1),reslatlon
9100 format(' XDEF ',I5,' LINEAR ',F7.2,1x,F6.3)
  write(91,9101)jmax,xlat(1),reslatlon
9101 format(' YDEF ',I5,' LINEAR ',F7.2,1x,F6.3)
  write(91,*)'ZDEF 1 LEVELS 0'
  write(91,*)'tdef 1 linear 00Z01JAN2011 1mon'
  write(91,*)'VARS 2'
  write(91,*)'wind10 0 99 10-m max wind (kt)'
  write(91,*)'rain   0 99 accumulated precipitation(IN)'
  write(91,*)'ENDVARS'
  close(91)
  !
  tmns = -99.0
  write(nwsw,3333)tmns,tmns,tmns
  write(npsw,3333)tmns,tmns,tmns
  !     include duration & track of storm  
  write(nwsw,3335)ihour
  write(npsw,3335)ihour
3335 format(i5)
  do i = 1 , nthour
     write(nwsw,3334) stormlat(i),stormlon(i)
     write(npsw,3334) stormlat(i),stormlon(i)
3334 format(2f8.2)
  enddo

  stop
end program swath
