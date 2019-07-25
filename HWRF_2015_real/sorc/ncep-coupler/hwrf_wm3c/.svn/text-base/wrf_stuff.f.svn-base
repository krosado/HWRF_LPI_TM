!
!***********************************************************************
!
      SUBROUTINE GRID_CELL_a(kg,nx,ny,ia,ja,ib,jb,ic,jc)

C        This is WRF specific

      implicit none

      integer kg,nx,ny
      integer,dimension(nx,ny):: ia,ja,ib,jb,ic,jc

      integer i,j
      logical j_odd
C

CB  The following is needed for both interpolation and
C   its initialization

      do j=1,ny
        j_odd=j.ne.(j/2)*2
        do i=1,nx
          if (kg.eq.0 .eqv. j_odd) then
            ia(i,j)=i
            ic(i,j)=i
          else
            ia(i,j)=i+1
            ic(i,j)=i+1
          end if
          ja(i,j)=j-1
          ib(i,j)=i+1
          jb(i,j)=j
          jc(i,j)=j+1
        end do
      end do
CE
      RETURN
      END
!
!***********************************************************************
!
      SUBROUTINE DOMAIN_BOUNDARY_a(kg,nx,ny,km,ibnd,jbnd)

C        This is WRF specific

      implicit none

      integer kg,nx,ny
      integer km
      integer,dimension(0:nx*ny):: ibnd,jbnd

      integer i,j,k
      logical j_odd
C

CB  The following is not needed for interpolation but is needed for
C   its initialization

      if (kg.eq.0) then
        km=2*(2*nx+ny-7) ! =2*(nx-2+nx-1+ny-4)
        do k=1,2*nx-3
          ibnd(k-1)=k/2+1
          jbnd(k-1)=1+k-2*(k/2)
          ibnd(2*nx+ny-8+k)=nx-(k+1)/2
          jbnd(2*nx+ny-8+k)=ny-k+2*(k/2)
        end do
        do k=3,ny-2
          ibnd(2*nx-6+k)=nx-1+k-2*(k/2)
          jbnd(2*nx-6+k)=k
          ibnd(4*nx+ny-13+k)=1
          jbnd(4*nx+ny-13+k)=ny-k+1
        end do
      else if (kg.eq.1) then
        km=2*(2*nx+ny-5) ! =2*(nx-1+nx-2+ny-2)
        do k=1,2*nx-1
          ibnd(k-1)=(k-1)/2+1
          jbnd(k-1)=1+k-2*(k/2)
          ibnd(2*nx+ny-6+k)=nx-k/2
          jbnd(2*nx+ny-6+k)=ny-k+2*(k/2)
        end do
        do k=3,ny-2
          ibnd(2*nx-4+k)=nx-k+2*(k/2)
          jbnd(2*nx-4+k)=k
          ibnd(4*nx+ny-9+k)=1
          jbnd(4*nx+ny-9+k)=ny-k+1
        end do
      else
        call GLOB_ABORT(kg,'kg is neither 0 nor 1 in GRID_a which '//
     >  'is a WRF specific subroutine. ABORTED',1)
      end if

      print*,'C: DOMAIN_BOUNDARY_a: km: ',km

      ibnd(km)=ibnd(0)
      jbnd(km)=jbnd(0)
CE

      RETURN
      END
!
!***********************************************************************
!
      SUBROUTINE MAKE_BOUNDARY_MASK_LAND_a(nx,ny,MASK)

C        This is WRF specific

      implicit none

      integer nx,ny
      integer,dimension(nx,ny):: MASK
C

      MASK(:,1:2)=1
      MASK(:,ny-1:ny)=1
      MASK(1,:)=1
      MASK(nx-1:nx,:)=1

      RETURN
      END
!
!***********************************************************************
!
      SUBROUTINE ADJUST_VMASK_a(nx,ny,HMASK,VMASK)

! Make those V-points which have a neighboring land H-point land points.
C        This is WRF specific

      implicit none

      integer nx,ny
      integer,dimension(nx,ny):: HMASK,VMASK

      integer i,j
      logical j_odd
C

      do j=1,ny
        j_odd=j.ne.(j/2)*2
        do i=1,nx
        if (VMASK(i,j).eq.0) then
          if (HMASK(i,j).ne.0) then
            VMASK(i,j)=1
            cycle
          end if
          if (i.lt.nx .and. j_odd) then
            if (HMASK(i+1,j).ne.0) then
              VMASK(i,j)=1
              cycle
            end if
          end if
          if (i.gt.1 .and. .not. j_odd) then
            if (HMASK(i-1,j).ne.0) then
              VMASK(i,j)=1
              cycle
            end if
          end if
          if (j.lt.ny) then
            if (HMASK(i,j+1).ne.0) then
              VMASK(i,j)=1
              cycle
            end if
          end if
          if (j.gt.1) then
            if (HMASK(i,j-1).ne.0) then
              VMASK(i,j)=1
              cycle
            end if
          end if
        end if
        end do
      end do

      RETURN
      END
!
!***********************************************************************
!
      subroutine coor_mask_a(NIDE,NJDE,HLON,HLAT,VLON,VLAT,HSM,VSM)

C        This is WRF specific

      IMPLICIT NONE

      INTEGER                          :: NIDE,NJDE,IDE,JDE,I,J,II,JJ
      real,DIMENSION(NIDE,NJDE):: HLAT,HLON,VLAT,VLON,HSM,VSM

      REAL(kind=4),DIMENSION(NIDE,NJDE)::NHLAT,NHLON,NVLAT,NVLON,HRES_SM

      integer imin,jmin,imax,jmax
      real amin,amax
!

c     IDE=216   ! ---------------------> input
c     JDE=432   ! ---------------------> input
!
!
C         NIDE=3*(IDE-1)-2
C         NJDE=3*(JDE-1)-2 

      OPEN(65, file='fort.65',status='old',form='UNFORMATTED')

      READ(65)NHLAT
      READ(65)NHLON
      READ(65)NVLAT
      READ(65)NVLON
      READ(65)HRES_SM

      HLAT=NHLAT
      HLON=NHLON
      VLAT=NVLAT
      VLON=NVLON
      HSM=HRES_SM
      VSM=HRES_SM

      call minmax(NIDE,NJDE,HLON,imin,jmin,amin,imax,jmax,amax)
      print*,'C: coor_mask_a: HLON min, max: ',
     >imin,jmin,amin,imax,jmax,amax
      call minmax(NIDE,NJDE,HLAT,imin,jmin,amin,imax,jmax,amax)
      print*,'C: coor_mask_a: HLAT min, max: ',
     >imin,jmin,amin,imax,jmax,amax
      call minmax(NIDE,NJDE,VLON,imin,jmin,amin,imax,jmax,amax)
      print*,'C: coor_mask_a: VLON min, max: ',
     >imin,jmin,amin,imax,jmax,amax
      call minmax(NIDE,NJDE,VLAT,imin,jmin,amin,imax,jmax,amax)
      print*,'C: coor_mask_a: VLAT min, max: ',
     >imin,jmin,amin,imax,jmax,amax

      close(65)

      return
      END
!
!***********************************************************************
!
      SUBROUTINE gridsizes_a(PIDE,PJDE,NIDE,NJDE,RATIO,NDOM)

C        This is WRF specific

!-----------------------------------------------------------------------
!     Adopted from Tom's routine in ./frame/module_io_quilt.F
!-----------------------------------------------------------------------
! modified for call from Coupler; name of subr. changed from read_namelist - D.
!***********************************************************************
      implicit none
!-----------------------------------------------------------------------
      CHARACTER*80 :: filename
      integer, intent(out)       :: PIDE,PJDE,NIDE,NJDE,RATIO,NDOM
c     real,    intent(out)       :: DLMD,DPHD
      real DLMD,DPHD

      integer, parameter :: MD=20             ! maximum domains

      integer :: i_parent_start(md),j_parent_start(md)
      integer :: time_step,time_step_fract_num,time_step_fract_den,
     >max_dom,s_we(MD),e_we(MD),s_sn(MD),e_sn(MD),s_vert(MD),e_vert(MD),
     >grid_id(MD),tile_sz_x,tile_sz_y,numtiles,nproc_x,
     >nproc_y,level,parent_id(MD),
     >parent_grid_ratio(MD),parent_time_step_ratio(MD),feedback,
     >smooth_option,num_moves,move_id,move_interval,
     >move_cd_x,move_cd_y

      real   ::  dx(MD),dy(MD),ztop
!zhang add additional variables in to namelist file
      integer :: num_metgrid_levels,coral_x(MD),coral_y(MD)
      real, dimension(3000) :: eta_levels
      logical :: use_prep_hybrid
      integer :: num_metgrid_soil_levels, halo_debug
      real :: p_top_requested, ptsgm

      namelist /domains/
     >time_step,time_step_fract_num,time_step_fract_den,
     >max_dom,s_we,e_we,s_sn,e_sn,s_vert,e_vert,dx,dy,
     >ztop,grid_id,tile_sz_x,tile_sz_y,numtiles,nproc_x,
     >nproc_y,level,parent_id,i_parent_start,j_parent_start,
     >parent_grid_ratio,parent_time_step_ratio,feedback,
     >smooth_option,num_moves,move_id,move_interval,
     >move_cd_x,move_cd_y,eta_levels,p_top_requested, ptsgm,
     >num_metgrid_levels, use_prep_hybrid,num_metgrid_soil_levels,
     >halo_debug,coral_x,coral_y
!-----------------------------------------------------------------------

      filename='namelist.input'

      open(unit=99,file=trim(filename),form="formatted",status="old") ! uncommented; unit made 99 - D.

      read(99,domains) ! unit made 99 - D.

        PIDE = e_we(1)-1  ! -1 added - D.
        PJDE = e_sn(1)-1  ! -1 added - D.
        NIDE = e_we(2)-1  ! -1 added - D.
        NJDE = e_sn(2)-1  ! -1 added - D.
        DLMD = dx(1)
        DPHD = dy(1)
        RATIO= parent_grid_ratio(2) 

        NDOM=max_dom
        if (max_dom.gt.2) NDOM=2
        print*,'C: gridsizes_a: max_dom, NDOM: ',max_dom, NDOM

        close(99)
!-----------------------------------------------------------------------
      END
!-----------------------------------------------------------------------
!
!***********************************************************************
!
      subroutine CHECK_FSG_against_CSG(im1,jm1,im0,jm0,rat,
     >hlon1,hlat1,hmask1,vlon1,vlat1,vmask1,
     >hlon0,hlat0,hmask0,vlon0,vlat0,vmask0,err)

      implicit none

      integer im1,jm1,im0,jm0,rat
      real,dimension(im1,jm1):: hlon1,hlat1,vlon1,vlat1
      integer,dimension(im1,jm1):: hmask1,vmask1
      real,dimension(im0,jm0):: hlon0,hlat0,vlon0,vlat0
      integer,dimension(im0,jm0):: hmask0,vmask0
c     logical fix
      integer err

      real,dimension(im0,jm0):: hlon2,vlon2,hlat2,vlat2
      integer,dimension(im0,jm0):: mask
      integer ierr,err1
      real eps /1.E-3/
C

      err=0
      err1=0

      if (im0.ne.rat*(im1-1)+1 .or. jm0.ne.rat*(jm1-1)+1 .or.
     >    jm0.eq.2*(jm0/2) .or. jm1.eq.2*(jm1/2)) then
        err=-10
        print*,'CHECK_FSG_against_CSG: wrong im0 and/or jm0 and/or rat',
     >  im1,jm1,im0,jm0,rat
        return
      end if

      call coor(hlon1,vlon1,hlon2,vlon2)
      call coor(hlat1,vlat1,hlat2,vlat2)
      call geocmp(0,hlat0,hlon0,hlat2,hlon2,eps,10,ierr)
      call fu('# of H LAT/LON failures = ',-5,1)
      call geocmp(0,vlat0,vlon0,vlat2,vlon2,eps,10,ierr)
      call fu('# of V LAT/LON failures = ',-5,1)

      call hmsk(hmask1,mask,ierr)
      call fu('this will not be printed anyway',-11,0)
      call cmpi(0,mask,hmask0,10,ierr)
      call fu('# of HMASK failures = ',-5,1)

      if (ANY(vmask0.ne.hmask0) .or. ANY(vmask1.ne.hmask1)) then
        err=err+2
        if (ANY(vmask0.ne.hmask0))
     >  print*,'CHECK_FSG_against_CSG: FSG V-mask differs from '//
     >  'H-mask which is contrary to original algorithm'
        if (ANY(vmask1.ne.hmask1))
     >  print*,'CHECK_FSG_against_CSG: CSG V-mask differs from '//
     >  'H-mask which is contrary to original algorithm'
      else
        print*,'CHECK_FSG_against_CSG: FSG/CSG V-mask = FSG/CSG '//
     >  'H-mask according to original algorithm. No further '//
     >  'V-mask check performed'
      end if
        
c     call hmsk(vmask1,mask,ierr)
c     call fu('this will not be printed anyway',-12,0)
c     call cmpi(0,mask,vmask0,10,ierr)
c     call fu('# of VMASK failures = ',-6,2)

      if (err1.lt.0) err=err1

      return

!==
      CONTAINS
!==

        subroutine coor(H1,V1,H0,V0)

        implicit none

        real,dimension(im1,jm1):: H1,V1
        real,dimension(im0,jm0):: H0,V0

        integer i0,j0,i1,j1,is,js,ie,je,im12,im02
        real,dimension(2*im0+rat-1,jm0):: F0
        real,dimension(2*im1,jm1):: F1
C

        im12=2*im1
        im02=2*im0

        F1(1:im12:2,1:jm1:2)=H1(1:im1,1:jm1:2)
        F1(2:im12:2,1:jm1:2)=V1(1:im1,1:jm1:2)
        F1(1:im12:2,2:jm1:2)=V1(1:im1,2:jm1:2)
        F1(2:im12:2,2:jm1:2)=H1(1:im1,2:jm1:2)

        do i1=1,im12-1
          is=(i1-1)*rat+1
          ie=i1*rat
          do i0=is,ie
            F0(i0,1:jm0:rat)=F1(i1,:)+
     >      real(i0-is)/rat*(F1(i1+1,:)-F1(i1,:))
          end do
        end do
!       F0(im02+rat-1,1:jm0:rat)=F1(im12,:) ! <- correct but not needed
        do j1=1,jm1-1
          js=(j1-1)*rat+2
          je=j1*rat
          do j0=js,je
            F0(:,j0)=F0(:,js-1)+
     >      real(j0-js+1)/rat*(F0(:,je+1)-F0(:,js-1))
          end do
        end do

        H0(1:im0,1:jm0:2)=F0(1:im02:2,1:jm0:2)
        V0(1:im0,1:jm0:2)=F0(2:im02:2,1:jm0:2)
        V0(1:im0,2:jm0:2)=F0(1:im02:2,2:jm0:2)
        H0(1:im0,2:jm0:2)=F0(2:im02:2,2:jm0:2)
          
        return
        end subroutine coor

!==

        subroutine hmsk(H1,H0,ierr)

        implicit none

        integer,dimension(im1,jm1),intent(in):: H1
        integer,dimension(im0,jm0),intent(out):: H0
        integer,intent(out):: ierr

        integer i,j,i1,j1,m,n,m1,n1
        real rm1,rn1
C

        ierr=0

        do j=1,jm0
          do i=1,im0
            m=i+j/2
            n=(j+1)/2-i+im0
            rm1=real(m-1)/rat+1
            m1=rm1
            if (rm1-m1.ge.0.5) m1=m1+1
            rn1=real(n-1)/rat+1
            n1=rn1
            if (rn1-n1.ge.0.5) n1=n1+1
            j1=m1+n1-im1
            i1=m1-j1/2
            if (i1.lt.1 .or. i1.gt.im1 .or.j1.lt.1 .or. j1.gt.jm1) then
              print*,'CHECK_FSG_against_CSG: hmsk: ERROR in algorithm',
     >        i,j,i1,j1
              ierr=-1
              return
            end if
            H0(i,j)=H1(i1,j1)
          end do
        end do

        return
        end subroutine hmsk

!==

        subroutine geocmp(hv,lat1,lon1,lat2,lon2,eps,n,ierr)
        implicit none
        real, dimension(:,:), intent(in) :: lat1,lon1,lat2,lon2
        integer, intent(in) :: hv, n
        real, intent(in) :: eps
        integer, intent(inout) :: ierr
        integer imin,imax,jmin,jmax,i,j,k
        real, parameter :: Requator   = 6378137.0000
        real, parameter :: pi      = 3.141592653589793238
        real, parameter :: flattening = 1/298.257223563
        real, parameter :: DEGRAD = pi/180
        real :: rlat1,rlon1, rlat2,rlon2
        real :: Rearth1,Rearth2,epsdist,dist
        real, parameter :: flattening_inv=1/flattening
C
        imin=LBOUND(lat1,1)
        imax=UBOUND(lat1,1)
        jmin=LBOUND(lat1,2)
        jmax=UBOUND(lat1,2)

        if(imin/=LBOUND(lon1,1) .or. imax/=UBOUND(lon1,1) .or.
     >     jmin/=LBOUND(lon1,2) .or. jmax/=UBOUND(lon1,2)) then
          print*,'CHECK_FSG_against_CSG: cmp: wrong lon1 boundaries ',
     >    imin,LBOUND(lon1,1),imax,UBOUND(lon1,1),
     >    jmin,LBOUND(lon1,2),jmax,UBOUND(lon1,2)
          ierr=-99
        endif

        if(imin/=LBOUND(lat2,1) .or. imax/=UBOUND(lat2,1) .or.
     >     jmin/=LBOUND(lat2,2) .or. jmax/=UBOUND(lat2,2)) then
          print*,'CHECK_FSG_against_CSG: cmp: wrong lat2 boundaries ',
     >    imin,LBOUND(lat2,1),imax,UBOUND(lat2,1),
     >    jmin,LBOUND(lat2,2),jmax,UBOUND(lat2,2)
          ierr=-99
        endif

        if(imin/=LBOUND(lon2,1) .or. imax/=UBOUND(lon2,1) .or.
     >     jmin/=LBOUND(lon2,2) .or. jmax/=UBOUND(lon2,2)) then
          print*,'CHECK_FSG_against_CSG: cmp: wrong lon2 boundaries ',
     >    imin,LBOUND(lon2,1),imax,UBOUND(lon2,1),
     >    jmin,LBOUND(lon2,2),jmax,UBOUND(lon2,2)
          ierr=-99
        endif

        if (hv.ne.0 .and. hv.ne.1) then
          print*,'CHECK_FSG_against_CSG: cmp: wrong hv value ',hv
          ierr=-98
          return
        end if

        epsdist=Requator*eps*DEGRAD
 44     format('Using epsdist=',F0.7,' from eps=',F0.7)
        print 44,epsdist,eps

        ierr=0
        k=hv
        do j=jmin,jmax
          do i=imin,imax-k
             rlat1=lat1(i,j)*DEGRAD
             rlon1=lon1(i,j)*DEGRAD
             rlon2=lon2(i,j)*DEGRAD
             rlat2=lat2(i,j)*DEGRAD

             Rearth1=Requator*(1-sin(rlat1)**2/flattening_inv)
             Rearth2=Requator*(1-sin(rlat2)**2/flattening_inv)
            
             dist=(Rearth1+Rearth2)*asin(min(1.0,sqrt(                  &
     &           sin((rlat1-rlat2)/2)**2+                               &
     &           cos(rlat1)*cos(rlat2)*sin((rlon1-rlon2)/2)**2)))
             
             if(dist>epsdist) then
               ierr=ierr+1
 48            format('CHECK_FSG_against_CSG: cmp: dist=',F0.7,'m ',    &
     &              ' FSG: lat1,lon1=',F0.7,',',F0.7,                   &
     &              ' CSG interped to FSG: lat2,lon2=',F0.7,',',F0.7)
               if(ierr<n) then
                  print 48,dist,lat1(i,j),lon1(i,j),lat2(i,j),lon2(i,j)
               endif
             end if
          end do
          k=1-k
        end do

        end subroutine geocmp
!==

        subroutine cmp(hv,x,y,eps,n,ierr)

        implicit none

        integer,intent(in):: hv
        real,dimension(:,:),intent(in):: x,y
        real,intent(in):: eps
        integer,intent(in):: n
        integer,intent(out):: ierr

        integer imin,imax,jmin,jmax,i,j,k
C

        imin=LBOUND(x,1)
        imax=UBOUND(x,1)
        jmin=LBOUND(x,2)
        jmax=UBOUND(x,2)

        if (imin.ne.LBOUND(y,1) .or. imax.ne.UBOUND(y,1) .or.
     >      jmin.ne.LBOUND(y,2) .or. jmax.ne. UBOUND(y,2) .or.
     >      imin.ne.1 .or. jmin.ne.1) then
          print*,'CHECK_FSG_against_CSG: cmp: wrong array boundaries ',
     >    imin,LBOUND(y,1),imax,UBOUND(y,1),
     >    jmin,LBOUND(y,2),jmax,UBOUND(y,2)
          ierr=-99
          return
        end if
        if (hv.ne.0 .and. hv.ne.1) then
          print*,'CHECK_FSG_against_CSG: cmp: wrong hv value ',hv
          ierr=-98
          return
        end if

        ierr=0
        k=hv
        do j=jmin,jmax
          do i=imin,imax-k
            if (abs(x(i,j)-y(i,j)).ge.eps) then
              ierr=ierr+1
              if (ierr.le.n) print*,'CHECK_FSG_against_CSG: cmp: ',
     >                       i,j,x(i,j),y(i,j)
            end if
          end do
          k=1-k
        end do
          
        return
        end subroutine cmp

!==

        subroutine cmpi(hv,x,y,n,ierr)

        implicit none

        integer,intent(in):: hv
        integer,dimension(:,:),intent(in):: x,y
        integer,intent(in):: n
        integer,intent(out):: ierr

        integer imin,imax,jmin,jmax,i,j,k
C

        imin=LBOUND(x,1)
        imax=UBOUND(x,1)
        jmin=LBOUND(x,2)
        jmax=UBOUND(x,2)

        if (imin.ne.LBOUND(y,1) .or. imax.ne.UBOUND(y,1) .or.
     >      jmin.ne.LBOUND(y,2) .or. jmax.ne. UBOUND(y,2) .or.
     >      imin.ne.1 .or. jmin.ne.1) then
          print*,'CHECK_FSG_against_CSG: cmpi: wrong array boundaries ',
     >    imin,LBOUND(y,1),imax,UBOUND(y,1),
     >    jmin,LBOUND(y,2),jmax,UBOUND(y,2)
          ierr=-99
          return
        end if
        if (hv.ne.0 .and. hv.ne.1) then
          print*,'CHECK_FSG_against_CSG: cmpi: wrong hv value ',hv
          ierr=-98
          return
        end if

        ierr=0
        k=hv
        do j=jmin,jmax
          do i=imin,imax-k
            if (x(i,j).ne.y(i,j)) then
              ierr=ierr+1
              if (ierr.le.n) print*,'CHECK_FSG_against_CSG: cmpi: ',
     >                       i,j,x(i,j),y(i,j)
            end if
          end do
          k=1-k
        end do
          
        return
        end subroutine cmpi

!==
        
        subroutine fu(s,e1,e2)

        implicit none

        character*(*) s
        integer e1,e2
C

        if (ierr.lt.0) then
          err1=e1
          print*,'CHECK_FSG_against_CSG: SEVERE failure, ierr=',ierr
        else if (ierr.ne.0) then
          err=err+e2
          print*,'CHECK_FSG_against_CSG: '//s,ierr
     >    ,'  ratio=',real(ierr)/(im0*jm0)
        end if
        
        return
        end subroutine fu

!==
        
      END
!
!***********************************************************************
!
      SUBROUTINE minmax(IM,JM,a,imin,jmin,amin,imax,jmax,amax)

      implicit none

      integer,intent(in):: IM,JM
      real,dimension(IM,JM),intent(in):: a
      integer,intent(out):: imin,jmin,imax,jmax
      real,intent(out):: amin,amax

      integer i,j
C

      amin=1.E30
      amax=-1.E30

      do j=1,JM
      do i=1,IM
        if (a(i,j).lt.amin) then
          imin=i
          jmin=j
          amin=a(i,j)
        end if
        if (a(i,j).gt.amax) then
          imax=i
          jmax=j
          amax=a(i,j)
        end if
      end do
      end do

      return
      END
C
C***********************************************************************
C
      subroutine get_subgrid(hv,IM,JM,IM1,JM1,rat,F,F1,SPVAL)
                             !
                             !<- 0: H-points, 1: V-points

      implicit none

      integer hv,IM,JM,IM1,JM1,rat
      real,dimension(IM,JM):: F
      real,dimension(IM1,JM1):: F1
      real SPVAL

      integer i,j,i1,j1
C

      do i1=1,IM1
        do j1=1,JM1
          if ((j1/2)*2.ne.j1 .eqv. hv.eq.0) then
            i=(i1-1)*rat+1
          else
            i=i1*rat-1
          end if
          j=(j1-1)*rat+1
          if (i.le.IM .and. j.le.JM) then
            F1(i1,j1)=F(i,j)
          else
            F1(i1,j1)=SPVAL
          end if
        end do
      end do

      return
      END
C
C***********************************************************************
C
      subroutine get_subgrid4(hv,IM,JM,IM1,JM1,rat,F,F1)
                             !
                             !<- 0: H-points, 1: V-points

      implicit none

      integer hv,IM,JM,IM1,JM1,rat
      real(kind=4),dimension(IM,JM):: F
      real(kind=4),dimension(IM1,JM1):: F1

      integer i,j,i1,j1
C

      do i1=1,IM1
        do j1=1,JM1
          if ((j1/2)*2.ne.j1 .eqv. hv.eq.0) then
            i=(i1-1)*rat+1
          else
            i=i1*rat-1
          end if
          j=(j1-1)*rat+1
          if (i.le.IM .and. j.le.JM) then
            F1(i1,j1)=F(i,j)
          else
            F1(i1,j1)=0.
          end if
        end do
      end do

      return
      END
C
C***********************************************************************
C
      subroutine put_subgrid4(hv,IM,JM,IM1,JM1,rat,F,F1)
                             !
                             !<- 0: H-points, 1: V-points

      implicit none

      integer hv,IM,JM,IM1,JM1,rat
      real(kind=4),dimension(IM,JM):: F
      real(kind=4),dimension(IM1,JM1):: F1

      integer i,j,i1,j1
C

      do i1=1,IM1
        do j1=1,JM1
          if ((j1/2)*2.ne.j1 .eqv. hv.eq.0) then
            i=(i1-1)*rat+1
          else
            i=i1*rat-1
          end if
          j=(j1-1)*rat+1
          if (i.le.IM .and. j.le.JM) then
            F(i,j)=F1(i1,j1)
          end if
        end do
      end do

      return
      END
C
C***********************************************************************
C
