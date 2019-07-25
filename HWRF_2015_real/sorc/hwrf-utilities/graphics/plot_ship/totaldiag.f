      program totaldiag
c**********************************************************************
c     This program takes the information about the diagnostics, 
c       pressure levels, and the individual time parameter files to
c       produce the text diagnostic file.
c     Last Modified: 12/31/2012, version 2.1
c        -added DELTAT to diagnostic file printout
c        -added custom variable TGRD to diagnostic file printout
c        -added comments with averaging radii information to 
c         diagnostic file printout
c     Modified:      03/02/2012, version 2.0
c**********************************************************************
c 
      USE diag_util
      IMPLICIT NONE
c 
      !begin variable declaration
      integer, parameter :: imiss = -9999, imissd = 9999
      real, parameter :: rmiss = -999.9
      integer, parameter :: nvar = 16, nrad = 7, ncust = 1
      integer :: nlevs
      integer, allocatable, dimension(:) :: iplevs
      integer :: inestuse, iprintradii
      integer :: luin, lupl, lupa, lupr, lura, luou, ierrc, istat
      integer :: itmax, itint, m, n, ntimes, itpar
      character(len=20) :: fnin, fnpl, fnpa, fnpr, fnra, fnou
      character(len=40) :: dash
      character(len=2) :: cbasin, csbasin, csnum
      character(len=4) :: cmodel, csmodel
      character(len=10) :: csdtg, cname, csname
      character(len=13) :: cfmtstri, cfmtstra
      character(len=31) :: cfmtstrp
c      character(len=10) :: cendt, cendr, cendp, cendu, cendv, cendz
c      character(len=4) :: csurf
      character(len=4) :: cplev
      character(len=16) :: ctlabt, ctlabr, ctlabp
      character(len=16) :: ctlabu, ctlabv, ctlabz
c 
      !parameter variables
      integer :: numparams
c      integer, parameter :: numparams = nvar+5*(nsnd+1)
c      character(len=16), dimension(numparams) :: paramlab
      character(len=16), dimension(nvar) :: paramlab
      integer, allocatable, dimension(:,:) :: iparams
      integer, allocatable, dimension(:) :: itimes
      real, allocatable, dimension(:) :: rtimes, rlat, rlon
      real, allocatable, dimension(:) :: rxt, ryt, rmagt, rspd, rhdg
      character(len=6), allocatable, dimension(:) :: clon, clat
c 
      !custom variable section
c      integer :: ncust
      character(len=16), dimension(ncust) :: custlab
c 
      !comment section
c      integer :: nrad
      integer, dimension(nrad) :: iradi, irado
      character(len=50) :: combegin, comend
c 
      !end variable declaration
c 
      dash='----------------------------------------'
c 
      fnin='diaginfo.txt'
      luin=30
c 
      fnpl='input.plvls'
      lupl=31
c 
      lupa=32
c 
      fnpr='printradiiflag.txt'  !holds radii flag for comment section
      lupr=33
c 
      fnra='lsdiagradii.txt' !holds radii values for comment section
      lura=34
c 
      fnou='diag.txt'
      luou=39
c 
      open(unit=luin,file=fnin,form='formatted',status='old',err=900)
      open(unit=lupl,file=fnpl,form='formatted',status='old',err=900)
      open(unit=lupr,file=fnpr,form='formatted',status='old',err=900)
      open(unit=luou,file=fnou,form='formatted',status='replace',
     +     err=900)
      !read in model run information
      read(luin,*) nlevs
      read(luin,*) inestuse
      read(luin,*) itmax
      read(luin,*) itint
      read(luin,'(a10)') csdtg
      read(luin,'(a4)') csmodel
      read(luin,'(a2)') csbasin
      read(luin,'(a2)') csnum
      read(luin,'(a10)') csname
      call upcase(csname,10)
      call upcase(csmodel,4)
      call upcase(csbasin,2)
c 
      !allocate iplevs and read from input.plvls
      allocate(iplevs(nlevs),STAT=istat)
      iplevs=imiss
c 
      !read in iplevs array from input.plvls
      do n=1,nlevs
         read(lupl,*) iplevs(n)
      enddo
      close(lupl)
c 
      !read in radii flag
      read(lupr,*) iprintradii
      close(lupr)
c 
c      !calculate the number of stored parameters
      numparams=nvar+5*(nlevs+1)+ncust
c      write(*,*)'numparams=', numparams
c 
      ntimes=int(float(itmax)/float(itint)) + 1
      allocate(itimes(ntimes),STAT=istat)
      allocate(rtimes(ntimes),STAT=istat)
      allocate(rlat(ntimes),STAT=istat)
      allocate(rlon(ntimes),STAT=istat)
      allocate(rxt(ntimes),STAT=istat)
      allocate(ryt(ntimes),STAT=istat)
      allocate(rmagt(ntimes),STAT=istat)
      allocate(rspd(ntimes),STAT=istat)
      allocate(rhdg(ntimes),STAT=istat)
      allocate(clon(ntimes),STAT=istat)
      allocate(clat(ntimes),STAT=istat)
      allocate(iparams(numparams,ntimes),STAT=istat)
c 
      paramlab( 1) = 'LAT     (DEG)   '
      paramlab( 2) = 'LON     (DEG)   '
      paramlab( 3) = 'MAXWIND (KT)    '
      paramlab( 4) = 'RMW     (KM)    '
      paramlab( 5) = 'MIN_SLP (MB)    '
      paramlab( 6) = 'SHR_MAG (KT)    '
      paramlab( 7) = 'SHR_HDG (DEG)   '
      paramlab( 8) = 'STM_SPD (KT)    '
      paramlab( 9) = 'STM_HDG (DEG)   '
      paramlab(10) = 'SST     (10C)   '
      paramlab(11) = 'OHC     (KJ/CM2)'
      paramlab(12) = 'TPW     (MM)    '
      paramlab(13) = 'LAND    (KM)    '
      paramlab(14) = '850TANG (10M/S) '
      paramlab(15) = '850VORT (/S)    '
      paramlab(16) = '200DVRG (/S)    '
c 
      custlab(1) =   'TGRD (10^7C/M)  '
c 
      ctlabt(1:16) = 'T_SURF  (10C)   '
      ctlabr(1:16) = 'R_SURF  (%)     '
      ctlabp(1:16) = 'P_SURF  (MB)    '
      ctlabu(1:16) = 'U_SURF  (10KT)  '
      ctlabv(1:16) = 'V_SURF  (10KT)  '
      ctlabz(1:16) = 'Z_SURF  (DM)    '
c 
      do n=1,ntimes
         itimes(n) = (n-1)*itint
         rtimes(n) = float(itimes(n))
         read(luin,'(a20)') fnpa
         open(unit=lupa,file=fnpa,form='formatted',status='old',
     +        err=900)
         do m=1,numparams
            read(lupa,'(i6)') iparams(m,n)
         enddo
         close(lupa)
         !set up latitude and longitude
         if (iparams(1,n) .eq. imissd) then
            rlat(n) = rmiss
            write(clat(n),'(i6)') imissd
         else
            rlat(n) = float(iparams(1,n))/10.0
            write(clat(n),'(f6.1)') rlat(n)
         endif
         if (iparams(2,n) .eq. imissd) then
            rlon(n) = rmiss
            write(clon(n),'(i6)') imissd
         else
            rlon(n) = float(iparams(2,n))/10.0
            write(clon(n),'(f6.1)') rlon(n)
         endif
      enddo
c 
      !add in storm speed and heading calculations (diagvar 8 and 9)
      call tspdcal(rlat,rlon,rtimes,ntimes,rmiss,rxt,ryt,rmagt)
      do n=1,ntimes
         if ((rlat(n) .le. rmiss) .or. (rlon(n) .le. rmiss)) then
            iparams(8,n) = imissd
            iparams(9,n) = imissd
         elseif (rmagt(n) .le. rmiss) then
            iparams(8,n) = imissd
            iparams(9,n) = imissd
         else
            call ctorh(rxt(n),ryt(n),rspd(n),rhdg(n))
            iparams(8,n) = nint(rspd(n))
            iparams(9,n) = nint(rhdg(n))
         endif
      enddo
c 
      !specify the format for almost all params (except lat/lon)
      cfmtstri(1:5) = '(a16,'
      write(cfmtstri(6:8),'(i3.3)') ntimes
      cfmtstri(9:13) = '(i6))'
c 
      cfmtstra=cfmtstri
      cfmtstra(10:10)='a'
c 
      cfmtstrp(1:31) = '(a4,1x,i3.3,1x,a4,000(1x,i4.4))'
      write(cfmtstrp(19:21),'(i3.3)') nlevs
c 
      !write out the diagnostic file
      write(*,*)'Processing diagnostic file'
      write(luou,101)'*',csmodel,csdtg,'*'
  101 format(15x,a1,3x,a4,2x,a10,3x,a1)
      write(luou,102)'*',csbasin,csnum,csname,'*'
  102 format(15x,a1,3x,2(a2),2x,a10,3x,a1)
      write(luou,*)''
      write(luou,103)dash,dash(1:14),'STORM DATA',dash,dash(1:18)
  103 format(16x,a40,a14,5x,a10,5x,a40,a18)
      write(luou,*)''
      write(luou,108) 'NTIME', ntimes, 'DELTAT', itint
  108 format(a5,1x,i3.3,3x,a6,1x,i3.3)
      write(luou,cfmtstri) 'TIME    (HR)    ', itimes
      write(luou,cfmtstra) paramlab(1), clat
      write(luou,cfmtstra) paramlab(2), clon
      do n=3,nvar
         write(luou,cfmtstri) paramlab(n), iparams(n,:)
      enddo
      write(luou,*)''
c 
      !begin sounding data section
      write(luou,104)dash,dash(1:14),'SOUNDING DATA',dash,dash(1:15)
  104 format(16x,a40,a14,5x,a13,5x,a40,a15)
      write(luou,*)''
c      write(luou,105) 'NLEV', nlevs, 'SURF', iplevs
c  105 format(a4,1x,i3.3,1x,a4,21(1x,i4.4))
      write(luou,cfmtstrp) 'NLEV', nlevs+1, 'SURF', iplevs
      write(luou,cfmtstri) 'TIME    (HR)    ', itimes
      itpar=nvar+1
      write(luou,cfmtstri) ctlabt(1:16), iparams(itpar,:)
      itpar=itpar+1
      write(luou,cfmtstri) ctlabr(1:16), iparams(itpar,:)
      itpar=itpar+1
      write(luou,cfmtstri) ctlabp(1:16), iparams(itpar,:)
      itpar=itpar+1
      write(luou,cfmtstri) ctlabu(1:16), iparams(itpar,:)
      itpar=itpar+1
      write(luou,cfmtstri) ctlabv(1:16), iparams(itpar,:)
      itpar=itpar+1
      do n=1,nlevs
         itpar=nvar+(5*n)+1
         write(cplev(1:4),'(i4.4)') iplevs(n)
         ctlabt(3:6) = cplev(1:4)
         write(luou,cfmtstri) ctlabt(1:16), iparams(itpar,:)
         itpar=itpar+1
         ctlabr(3:6) = cplev(1:4)
         write(luou,cfmtstri) ctlabr(1:16), iparams(itpar,:)
         itpar=itpar+1
         ctlabz(3:6) = cplev(1:4)
         write(luou,cfmtstri) ctlabz(1:16), iparams(itpar,:)
         itpar=itpar+1
         ctlabu(3:6) = cplev(1:4)
         write(luou,cfmtstri) ctlabu(1:16), iparams(itpar,:)
         itpar=itpar+1
         ctlabv(3:6) = cplev(1:4)
         write(luou,cfmtstri) ctlabv(1:16), iparams(itpar,:)
         itpar=itpar+1
      enddo
      write(luou,*)''
c 
      !begin custom data section
      write(luou,106)dash,dash(1:14),'CUSTOM DATA',dash,dash(1:17)
  106 format(16x,a40,a14,5x,a11,5x,a40,a17)
      write(luou,*)''
      write(luou,'(a4,1x,i3.3)') 'NVAR', ncust
      do n=1,ncust
         itpar=nvar+(5*(nlevs+1))+n
         write(luou,cfmtstri) custlab(1), iparams(itpar,:)
      enddo
      write(luou,*)''
c 
      !begin comment section
      write(luou,107)dash,dash(1:14),'COMMENTS',dash,dash(1:20)
  107 format(16x,a40,a14,5x,a8,5x,a40,a20)
      write(luou,*)''
      if (iprintradii .eq. 1) then
         open(unit=lura,file=fnra,form='formatted',status='old',err=900)
         do n=1,nrad
            read(lura,*) iradi(n)
            read(lura,*) irado(n)
            select case (n)
            case (1)
               combegin='SST, OHC averaged from '
               comend=' km around storm center [x10 C, kJ/cm2]'
               write(luou,111) '*', combegin(1:23), iradi(n), 
     +                         '-', irado(n), comend(1:39), '*'
  111          format(15x,a1,3x,a23,i4,a1,i4,a39,3x,a1)
            case (2)
               combegin='RMW within '
               comend=' km around storm center [km]'
               write(luou,112) '*', combegin(1:11), iradi(n), 
     +                         '-', irado(n), comend(1:28), '*'
  112          format(15x,a1,3x,a11,i4,a1,i4,a28,3x,a1)
            case (3)
               combegin='U, V, SHR averaged from '
               comend=' km around storm center [x10 kt, x10 kt, kt]'
               write(luou,113) '*', combegin(1:24), iradi(n), 
     +                         '-', irado(n), comend(1:44), '*'
  113          format(15x,a1,3x,a24,i4,a1,i4,a44,3x,a1)
            case (4)
               combegin='850VORT, 200DVRG averaged from '
               comend=' km around storm center [x10^7 /s, x10^7 /s]'
               write(luou,114) '*', combegin(1:31), iradi(n), 
     +                         '-', irado(n), comend(1:44), '*'
  114          format(15x,a1,3x,a31,i4,a1,i4,a44,3x,a1)
            case (5)
               combegin='850TANG averaged from '
               comend=' km around storm center [x10 m/s]'
               write(luou,115) '*', combegin(1:22), iradi(n), 
     +                         '-', irado(n), comend(1:33), '*'
  115          format(15x,a1,3x,a22,i4,a1,i4,a33,3x,a1)
            case (6)
               combegin='T, R, Z, P averaged from '
               comend=' km around storm center [x10 C, %, dm, mb]'
               write(luou,116) '*', combegin(1:25), iradi(n), 
     +                         '-', irado(n), comend(1:42), '*'
  116          format(15x,a1,3x,a25,i4,a1,i4,a42,3x,a1)
            case (7)
               combegin='TPW averaged from '
               comend=' km around storm center [mm]'
               write(luou,117) '*', combegin(1:18), iradi(n), 
     +                         '-', irado(n), comend(1:28), '*'
  117          format(15x,a1,3x,a18,i4,a1,i4,a28,3x,a1)
            end select
         enddo
         close(lura)
      endif
c 
      close(luin)
      close(luou)
c 
      deallocate(iplevs,STAT=istat)
      deallocate(itimes,STAT=istat)
      deallocate(rtimes,STAT=istat)
      deallocate(rlat,STAT=istat)
      deallocate(rlon,STAT=istat)
      deallocate(rxt,STAT=istat)
      deallocate(ryt,STAT=istat)
      deallocate(rmagt,STAT=istat)
      deallocate(rspd,STAT=istat)
      deallocate(rhdg,STAT=istat)
      deallocate(clon,STAT=istat)
      deallocate(clat,STAT=istat)
      deallocate(iparams,STAT=istat)
c 
c      return
      goto 950
c 
  900 continue
      stop 'Error during file open for totaldiag'
c 
  950 continue
      end
