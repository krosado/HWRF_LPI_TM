!
!=======================================================================
!
      program Coupler
C
      USE CPL_COMM
      USE mod_interp

      implicit none

C
      integer,parameter:: Atmos_L=1, Ocean_L=2, Waves_L=3
      integer Atmos_id /Atmos_L/,   ! this is components' ids, used to
     >        Ocean_id /Ocean_L/,   ! establish/verify their identities,
     >        Waves_id /Waves_L/,
     >        Atmos_spec /0/, ! these define what specific model
     >        Ocean_spec /0/, ! each component is
     >        Waves_spec /0/  !
!zz      character*2,dimension(max_component_id) :: cm /'AM','OM','WM'/
      character*2,dimension(max_component_id) :: cm 
      data cm /'AM','OM','WM'/

C
      integer NSF_a     ! number of surf. fields received from AM
      integer NSF_a2o   ! number of surf. fields passed from AM to OM
      integer NSF_a2w   ! number of surf. fields passed from AM to WM
      integer NSSF_a    ! number of scalar surf. fields received from AM

      character*4,dimension(:),allocatable::
     >sfn_a,sfn_a2o,sfn_a2w     ! (NSF_a,NSF_a2o,NSF_a2w resp.)
      character*15,dimension(Ocean_L:max_component_id):: vm_a

      real :: reflon = -9e9

      TYPE ARRAY2
        real(kind=kind_REAL),dimension(:,:),pointer:: ar
      END TYPE ARRAY2
      TYPE IARRAY1
        integer,dimension(:),pointer:: ar
      END TYPE IARRAY1
      TYPE IARRAY2
        integer,dimension(:,:),pointer:: ar
      END TYPE IARRAY2

      TYPE PARR2
        real(kind=kind_REAL),dimension(:,:),pointer:: gar
        real(kind=kind_REAL),dimension(:,:),pointer:: lar
        logical:: lar_pointed=.false.
      END TYPE PARR2
      TYPE IPARR2
        integer,dimension(:,:),pointer:: gar
        integer,dimension(:,:),pointer:: lar
        logical:: lar_pointed=.false.
      END TYPE IPARR2

      real(kind=kind_REAL),dimension(:),allocatable::
     >lakeval_o,lakeval_w !(NSF_a2o,NSF_a2w resp., for the time being;
                          ! later with surf. filelds other than AM
                          ! fields and OM SST transmitted it could be
                          ! something like NSF_o_recv, NSF_w_recv resp.)

! Data on AM grid
      integer,parameter:: NG_a=2, ND_a=2
      integer agridrat
      integer istart1,jstart1,iend1,jend1,istart0,jstart0,iend0,jend0
      integer,dimension(0:ND_a):: nxa,nya,nxnya
      integer,dimension(0:ND_a):: nxa_l,nya_l,nxnya_l,nxnya_l1,
     >ia_l_f,ia_l_l,ja_l_f,ja_l_l,ija_l_f,ija_l_l
      integer nga /NG_a/, nda /ND_a/, kdaf/0/
C         nga is NG_a and nda is ND_a when AM is present
! ***  nda is ND_a unless there is only 1 domain in AM, then nda=1
! ***  kdaf is   0 unless there is only 1 domain in AM, then kdaf=1
! (see assignments for nda and kdaf below)
      character*1,dimension(:),allocatable:: gn_a  ! (0:nga-1)
      integer,dimension(:,:),allocatable:: km_a    ! (0:nga-1,0:1)
      integer,dimension(:,:),allocatable:: it
      character*3 dn_a(0:ND_a) /'FSG','CSG','FMG'/
      TYPE (PARR2), dimension(:,:), allocatable::
     >ALON_a,ALAT_a,SLM_a                          ! (0:nga-1,0:nda)
      TYPE (IARRAY2), dimension(:), allocatable:: force_extrap_a
                                                   ! (0:nga-1)
      TYPE (IARRAY2), dimension(:,:), allocatable::
     >ia_a,ja_a,ib_a,jb_a,ic_a,jc_a                ! (0:nga-1,0:nda)
      TYPE (IARRAY1), dimension(:,:), allocatable::
     >ibnd_a,jbnd_a                                ! (0:nga-1,0:1)
      TYPE (PARR2), dimension(:,:), allocatable::
     >SF_a,                                        ! (NSF_a,nda)
     >SF_a2o_a,                                    ! (NSF_a2o,nda)
     >SF_a2w_a                                     ! (NSF_a2w,nda)
      TYPE (PARR2), dimension(:), allocatable::
     >SST_a,SST_a0,dQdT_a                    ! (nda)
      TYPE (PARR2) SST_a_r
      TYPE (IPARR2), dimension(:,:), allocatable::
     >MASK_a                                       ! (0:nga-1,0:nda)
      TYPE (IPARR2), dimension(:), allocatable::
     >MASK_a2                                      ! (0:nga-1)
      integer,dimension(:,:),pointer:: MASK_a_fluxes
      real(kind=kind_REAL),dimension(:,:),allocatable:: bf_a
      TYPE (PARR2) buf_a

! Data on OM grid
      integer nxo,nyo,nxnyo
      integer nxo_l,nyo_l,nxnyo_l,nxnyo_l1,
     >io_l_f,io_l_l,jo_l_f,jo_l_l,ijo_l_f,ijo_l_l
      integer,parameter:: NG_o=3
      integer ngo /NG_o/
      character*1,allocatable:: gn_o(:)
      TYPE (PARR2), allocatable, dimension(:)::
     >ALON_o,ALAT_o,SLM_o,                                ! (0:ngo-1)
     >SF_a2o_o                                            ! (NSF_a2o)
      TYPE (IARRAY2), dimension(:), allocatable:: force_extrap_o
                                                   ! (0:ngo-1)
      TYPE (PARR2) SST_o,buf_o
      real(kind=kind_REAL),dimension(:,:),allocatable::
     >SST_o0,SST_o_i,dQdT_o
      real(kind=kind_REAL),dimension(:,:),allocatable::
     >buf1_o,buf2_o
      TYPE (IPARR2), dimension(:), allocatable::
     >MASK_o                                              ! (0:ngo-1)

! Data on WM grid
      integer NSFs_w       ! number of surf. fields sent to WM
      integer nxw,nyw,nxnyw
      integer nxw_l,nyw_l,nxnyw_l,nxnyw_l1,
     >iw_l_f,iw_l_l,jw_l_f,jw_l_l,ijw_l_f,ijw_l_l
      integer,parameter:: NG_w=1
      integer ngw /NG_w/
      character*1,allocatable:: gn_w(:)
      TYPE (PARR2), allocatable, dimension(:)::
     >ALON_w,ALAT_w,SLM_w,                                ! (0:ngw-1)
     >SF_a2w_w                                            ! (NSF_a2w)
      TYPE (IARRAY2), dimension(:), allocatable:: force_extrap_w
                                                   ! (0:ngw-1)
      TYPE (PARR2) buf_w
      real(kind=kind_REAL),dimension(:,:),allocatable::
     >buf1_w,buf2_w
      TYPE (IPARR2), dimension(:), allocatable::
     >MASK_w,ICE_w                                        ! (0:ngw-1)

C Mutual configuration of the grids
   !    On AM grid(s)
      TYPE (IPARR2), dimension(:,:,:), allocatable::
     >Iog_a,Jog_a                         ! (0:nga-1,0:nda,0:ngo-1)
   !    On OM grid(s)
      TYPE (IPARR2), dimension(:,:,:), allocatable::
     >Iag_o,Jag_o                         ! (0:ngo-1,0:nga-1,0:nda)
      TYPE (PARR2), dimension(:,:), allocatable::
     >cosag_o,sinag_o                     ! (0:ngo-1,0:nga-1)
   !    On WM grid
      TYPE (IPARR2), dimension(:,:,:), allocatable::
     >Iag_w,Jag_w                         ! (0:ngw-1,0:nga-1,0:nda)
      TYPE (PARR2), dimension(:,:), allocatable::
     >cosag_w,sinag_w                     ! (0:ngw-1,0:nga-1)
      logical a2w_samegrid /.false./

C Kinds
!zz      integer,dimension(-max_component_id:max_component_id)::
      integer,dimension(-max_component_id:max_component_id):: knd_R,
     &         knd_sfcflux,knd_SST,knd_SLM,knd_lonlat
      data
     >                               knd_R       /0,0,0,0,4,4,4/
     >                             , knd_sfcflux /0,0,0,0,4,4,4/
     >                             , knd_SST     /0,0,0,0,4,4,4/
     >                             , knd_SLM     /0,0,0,0,4,4,4/
     >                             , knd_lonlat  /0,0,0,0,4,4,8/
                      ! elements with nonpositive indices are dummies,
                      ! for the case when the component model is absent

C Misc. data
      real dinr
      parameter(dinr=180./3.1415926535897932)
      real TZERO,TFREEZ,JCAL,HF_CONV_A2O
      integer itime_sec
      parameter(TZERO=273.16)
      parameter(TFREEZ=273.16-1.79)
      parameter(JCAL=4.1855,HF_CONV_A2O=1.E-4/JCAL)
      character*4 argument1
      logical prep,noii,iiwrt !if prep then interpolation initialization
                           ! is performed (in preprocessing) and its
                           ! output written to a file;
                           ! if noii then interpolation initialization
                           ! is not performed and INTP_INI output is
                           ! read from the file
      character*8 date
      character*10 time
      integer*4 iargc
      integer narg
      integer,parameter:: NSFmax=99
      integer,dimension(max_component_id):: nunit_restart

C type of external functions
      real cpl_sum
      integer cpl_isum

C Settings

      integer a_sea/1/,o_sea/1/,w_sea/1/ ! <-This is about initial masks
          ! Below it is assumed that SLM_a is float(a_sea)
          ! on sea and float(1-a_sea) on land, same about SLM_o, SLM_w
          ! with o_sea instead of a_sea
      real,parameter:: too_low=-1.E20, ! <- this
                     ! (1) must be .GE. very_large_negative in
                     ! interp. routine, currently -1.E30
                     ! (2) must be .GE. unrealistically_low_SF in
                     ! AM communications, currently -1.E30
                     ! (3) must be unrealistically low, i.e. less than
                     ! any real surface flux value
     >too_low10=10.*too_low

C Namelist data

      logical restart
      integer cstepmax

      real dt_c /0./
      logical,dimension(NSFmax,max_component_id):: SHT_SF
C       mask_tolerance_level             - accessed via USE CPL_COMM
      logical FORCE_INTP_INIT /.true./
c     integer FORCE_INTP_INIT /0/  ! 0: INTP_INIT calculations are not
                                   ! forced, the data are read from file
                                   ! intp_init instead. If it does not
                                   ! exist, the the data are calculated
                                   ! and written to the file
                                   ! 1: INTP_INIT calculations are
                                   ! forced, the data are calculated
                                   ! and neither read nor written
                                   ! 2: INTP_INIT calculations are
                                   ! forced, the data are calculated
                                   ! and written to file intp_init
                                   ! 3: same as 0 but if input data
                                   ! from the file fail to be the same
                                   ! as input arguments, routine does
                                   ! not abort but calculations are
                                   ! forced and the file is overwritten
                           !<- With use of INTP_INI for interpolation
                           ! initialization, integer FORCE_INTP_INIT
                           ! is disabled

      integer max_extrap /5/
      logical no_dq_at_all          /.true./,
     >        no_action_under_AM_SI /.true./,
     >        no_AM_SST             /.true./,
     >        extrap_SF             /.true./
      real OM_ul_thickn   /50./,
     >     efoldingtime   /864000./
C        nprint                          - accessed via USE CPL_COMM
C        VerbLev                         - accessed via USE CPL_COMM
      integer nprper/0/,
     >        npr1st/0/

      real RING_Iw /0.25/  ! parameter for computing WM ice mask (deg)

      SAVE

C Misc. variables
      integer i,i0,i1,i2,j,j0,j1,j2,k,k0,k1,k2,m,m0,m1,m2,n,n0,n1,n2,
     >ios,ios1,itm,imin,jmin,imax,jmax,kd,m10,m20,ia,ja,ib,jb,ic,jc,L
      integer cstep,iiunit,extrap_vrbs/1/
      integer (kind=kind_INTEGER) ibuf(2)
      integer nmin,nmax
      real(kind=kind_REAL) buf(1)
      real z,z1,z2,y1,y2,y3,fmean,fsigma,fmin,fmax
      real(kind=kind_REAL),dimension(:),allocatable:: cnt,cnt1
      real,dimension(:),allocatable:: fr,fr1
      character*20 cs
      character*80 s,s1,s2,modeltime
      logical MDout,MDout_sea

      NAMELIST /CPL_SETTINGS/
     >  restart,         !<- no default value, must be specified
     >  cstepmax,        !<- no default value, must be specified
     >  dt_c,
     >  SHT_SF,
     >  mask_tolerance_level,
     >  FORCE_INTP_INIT,
     >  max_extrap,
     >  extrap_SF,
     >  no_dq_at_all,no_action_under_AM_SI,no_AM_SST,
     >  OM_ul_thickn,efoldingtime,
     >  nprint,
     >  VerbLev,nprper,npr1st,
     >  RING_Iw
C     ------------------------------------------------------------------

CB
C  Introduction. Executable's arguments

!     nprint=7 ! assign 7 for printout files to be without date/time
               ! default is 6 (see module CPL_COMM)
C     ------------------------------------------------------------------

      print*,'This is Coupler code with generalized (diffe'//
     >'rent data types) communication procedures and generalized '//
     >'(for irregular grid cells) interpolation procedures'
      print*,'This Coupler is 3-way and is meant for AM stationary '//
     >'domain/grid and optional AM moving subdomain/grid (1);'//
     >' OM stationary domain/grid (2); and WM grid (3)'
      print*,'wrf_stuff.f corrected 01/26/2010'
      print*,
     >'prep option as of 02/01/2010 1:45pm (as in file wm3c_prep.f)'
      print*,'interp.f corrected 07/16/2010'
      print*,'- plus HYCOM+GFSfluxes adjustment'//
     >' (see comments in text) 08/03/2010'
      print*,
     >'This Coupler is multiprocessor with MPI assembling/disassembling'

      print*,'Analysis separated in draft 06/10/2013 4:00pm'
      print*,
!    >'Coupler main in wk since last modified 08/02/2012 5:00pm'! 1pr coupler
     >'Coupler main in wk since last modified 12/17/2013 4:00pm'

      narg=iargc()
      if (narg.gt.0) then
        CALL getarg(1,argument1)
      else
        argument1='null'
      end if

      print*,'argument1='//argument1

      prep=trim(argument1).eq.'prep'
      noii=trim(argument1).eq.'noii'
      iiwrt=trim(argument1).eq.'iiwrt'

!     if (prep) nprint=7

      if (prep) print*,'prep mode: to do interp. init.'
CE
C
CB0
C Initialize MPI communications
C
      call MPI_INIT(ierr)

      call CPL_ANNOUNCE('communication initialization to begin',1)
      CALL GLOB_ABORT(ierr,'C: ABORTED upon CALL MPI_INIT',1)
      call CPL_ANNOUNCE('back from MPI_INIT',1)
C
      call CPL_INIT
      call CPL_ANNOUNCE('back from CPL_INIT',1)
C
      call CPL_INTRO(Atmos_id)
      write(s,'(i2)') Atmos_id
      call CPL_ANNOUNCE(
     >'back from CPL_INTRO(Atmos_id), Atmos_id='//s,0)
C
      call CPL_INTRO(Ocean_id)
      write(s,'(i2)') Ocean_id
      call CPL_ANNOUNCE(
     >'back from CPL_INTRO(Ocean_id), Ocean_id='//s,0)
C
      call CPL_INTRO(Waves_id)
      write(s,'(i2)') Waves_id
      call CPL_ANNOUNCE(
     >'back from CPL_INTRO(Waves_id), Waves_id='//s,0)
C
      call CPL_ANNOUNCE('Communication initialization completed',1)
C
CE0
C  Define some controls
C  Get initial misc. technical info
!
      IF (MASTER) THEN
!
      open(14,file='cpl_nml',status='old',iostat=ios)
      call GLOB_ABORT(ios,'Error opening file cpl_nml. C TERMINATED',1)
      SHT_SF=.false.
      read(14,NML=CPL_SETTINGS,iostat=ios)
      write(nprint,NML=CPL_SETTINGS)
      call GLOB_ABORT(ios,'Error reading file cpl_nml. C TERMINATED',1)

      if (nprint.eq.7) then
        open(nprint,file='C_printout',form='formatted',status='unknown')
      else if (nprint.ne.6) then
        call date_and_time(date,time)
        open(nprint,file='C_printout.'//
     >  date(5:6)//'.'//date(7:8)//'.'//date(3:4)//'_'//
     >  time(1:2)//'.'//time(3:4)//'.'//time(5:6),
     >  form='formatted',status='unknown')
      end if
C
C     ------------------------------------------------------------------
C
      write(s,'(i10)') cstepmax
      call CPL_ANNOUNCE('cstepmax='//s,0)
      if (nprper.eq.0) then
        nprper=cstepmax
        call CPL_ANNOUNCE('nprper set =cstepmax='//s,0)
      end if
!
      END IF
!
      call CPL_LOGICAL_BC(restart,1)
      call CPL_INTEGER_BC(cstepmax,1)
      call CPL_BC(dt_c,1)
      call CPL_LOGICAL_BC(SHT_SF,NSFmax*max_component_id)
      call CPL_INTEGER_BC(mask_tolerance_level,1)
      call CPL_LOGICAL_BC(FORCE_INTP_INIT,1)
      call CPL_INTEGER_BC(max_extrap,1)
      call CPL_LOGICAL_BC(extrap_SF,1)
      call CPL_LOGICAL_BC(no_dq_at_all,1)
      call CPL_LOGICAL_BC(no_action_under_AM_SI,1)
      call CPL_LOGICAL_BC(no_AM_SST,1)
      call CPL_BC(OM_ul_thickn,1)
      call CPL_BC(efoldingtime,1)
      call CPL_INTEGER_BC(nprint,1)
      call CPL_INTEGER_BC(VerbLev,1)
      call CPL_INTEGER_BC(nprper,1)
      call CPL_INTEGER_BC(npr1st,1)
      call CPL_BC(RING_Iw,1)
      call CPL_BC(reflon,1)

      if(reflon<-8e8) then
         print *,'No reference longitude in namelist.'
         print *,'Will decide a reference longitude automatically.'
      else
 33      format('Reference longitude = ',F0.3)
         print 33,reflon
      endif

C
CB  OM identification: POM or HYCOM or none; OM model dependent stuff;
C   WM presence: WM or no WM; allocation of some small arrays.
C     Added: AM identification: dummy HWRF version or real HWRF
C
C
      call CPL_INTEGER_RECV(Atmos_spec,1,Atmos_id)

      if (Atmos_spec.eq.3) then
        call CPL_ANNOUNCE(
     >  'AM is *dummy* HWRF with MG, grid is WM grid',1)
      else if (Atmos_spec.eq.2) then
        call CPL_ANNOUNCE(
     >  'AM is *dummy* HWRF with MG, grid is HWRF grid',1)
      else if (Atmos_spec.eq.1) then
        if (nda.gt.1) then
          call CPL_ANNOUNCE('AM is (real) HWRF with MG',1)
        else
          call CPL_ANNOUNCE('AM is (real) HWRF, 1 dom.',1)
        end if
      else
        call GLOB_ABORT(Atmos_spec,'C: wrong Atmos_spec, ABORTED',1)
      end if

      call CPL_INTEGER_RECV(Ocean_spec,1,Ocean_id)

      call CPL_INTEGER_SEND(Ocean_spec,1,Atmos_id)

      if (Ocean_spec.eq.1) then

        call CPL_ANNOUNCE('OM is POM',1)

        NSF_a2o=4
        allocate(sfn_a2o(NSF_a2o))
        sfn_a2o=(/'HF  ','SWR ','TAUx','TAUy'/)

        knd_R(Ocean_id)=4

      else if (Ocean_spec.eq.2) then

        call CPL_ANNOUNCE('OM is Hurricane HYCOM',1)

        NSF_a2o=8
        allocate(sfn_a2o(NSF_a2o))
        sfn_a2o=(/'Prcp','SHF ','Evap','Ps  ','Rad ','SWR ',
     >  'TAUx','TAUy'/)

        knd_R(Ocean_id)=8

      else

        if (Ocean_id.gt.0 .or. Ocean_spec.ne.0) then
          call GLOB_ABORT(Ocean_spec,'C: wrong Ocean_spec, ABORTED',1)
        end if

        call CPL_ANNOUNCE('No OM (OM component inactive)',1)

        NSF_a2o=0
        allocate(sfn_a2o(NSF_a2o)) !just in case, for allocation status

      end if

      if (Ocean_id.gt.0) vm_a(Ocean_id)=' |('//sfn_a2o(NSF_a2o-1)//','//
     >sfn_a2o(NSF_a2o)//')| '

      call CPL_INTEGER_SEND(Waves_id,1,Atmos_id) 
                             ! requires proper provision in AM

      if (Waves_id.gt.0) then
        NSF_a2w=2
        allocate(sfn_a2w(NSF_a2w))
        sfn_a2w=(/'Uwnd','Vwnd'/)
        vm_a(Waves_id)=' |('//sfn_a2w(NSF_a2w-1)//','//
     >  sfn_a2w(NSF_a2w)//')| '
      else
        NSF_a2w=0
      end if

CE
C
C Continue to define 3-way coupling parameters, allocate small arrays:

      knd_lonlat(Ocean_id)=knd_R(Ocean_id)
      knd_sfcflux(Ocean_id)=knd_R(Ocean_id)
      knd_SST(Ocean_id)=knd_R(Ocean_id)
      knd_SLM(Ocean_id)=knd_R(Ocean_id)

      NSF_a=NSF_a2o+NSF_a2w
             ! this is just because currently, no surface fields are
             ! transmitted from AM to both OM and WM
      NSSF_a=NSF_a-min(NSF_a2o,2)-min(NSF_a2w,2)
             ! currently, 2 vector fields (2 pairs of vector coordinate
             ! fields) are received from AM: stress for OM and wind for
             ! WM. [It is assumed that if no vector field is received
             ! for OM (WM) then OM (WM) is inactive and NSF_a2o=0
             ! (NSF_a2w=0)]

c     NSF=NSF_a   <- not needed

             ! currently, (1) no surface fields are supplied by
             ! WM and (2) the only field supplied by OM is SST
             ! which is handled separately and not included in
             ! the list of surface fields. (1) is certainly
             ! temporary, (2) may or may not be advisable to keep

!
      if (Atmos_spec.ne.3) then
!
        call CPL_ANNOUNCE(
     >  'to get AMG sizes, n. of dom. from AM namelist',2)

        IF (MASTER)
     >    call gridsizes_a(nxa(1),nya(1),nxa(2),nya(2),agridrat,nda)
        call CPL_INTEGER_BC(nxa(1),2)
        call CPL_INTEGER_BC(nya(1),2)
        call CPL_INTEGER_BC(agridrat,1)
        call CPL_INTEGER_BC(nda,1)

        write(s,'(6i6)') nxa(1),nya(1),nxa(2),nya(2),agridrat,nda
        call CPL_ANNOUNCE(
     >  'AMG sizes, n. of dom. from AM namelist got: '//s,2)
!
      end if
!

      if (nda.eq.1) then
        kdaf=1
      end if
      write(s,'(i2)') nda
      call CPL_ANNOUNCE('Number of AM domains nda='//s,1)
      L=kdaf

      write(s,'(5i4)') NSFmax,NSF_a,NSF_a2o,NSF_a2w
      call CPL_ANNOUNCE('NSFmax, NSF_a, NSF_a2o, NSF_a2w: '//s,1)
      if (NSF_a.gt.NSFmax) then
        call GLOB_ABORT(NSF_a,'C: NSF_a>NSFmax, ABORTED',1)
      end if

      allocate(sfn_a(NSF_a))

      if (Ocean_id.gt.0) sfn_a(1:NSF_a2o)=sfn_a2o
      if (Waves_id.gt.0) sfn_a(NSF_a-NSF_a2w+1:NSF_a)=sfn_a2w
               ! see comment above to line NSF_a=NSF_a2o+NSF_a2w

      allocate(SF_a2o_a(NSF_a2o,nda),SF_a2o_o(NSF_a2o),
     >SF_a2w_a(NSF_a2w,nda),SF_a2w_w(NSF_a2w),
     >SF_a(NSF_a,nda))

      allocate(lakeval_o(NSF_a2o),lakeval_w(NSF_a2w))
      lakeval_o=0.
      lakeval_w=0.

      allocate(gn_a(0:nga-1),km_a(0:nga-1,L:1))
      allocate(
     >ALON_a(0:nga-1,L:nda),ALAT_a(0:nga-1,L:nda),SLM_a(0:nga-1,L:nda),
     >ia_a(0:nga-1,L:nda),ja_a(0:nga-1,L:nda),ib_a(0:nga-1,L:nda),
     >jb_a(0:nga-1,L:nda),ic_a(0:nga-1,L:nda),jc_a(0:nga-1,L:nda),
     >ibnd_a(0:nga-1,L:1),jbnd_a(0:nga-1,L:1),
     >SST_a(nda),SST_a0(nda),dQdT_a(nda),
     >MASK_a(0:nga-1,L:nda),
     >MASK_a2(0:nga-1),
     >force_extrap_a(0:nga-1)
     >)

      allocate(cnt(0:nga-1),cnt1(0:nga-1),fr(0:nga-1),fr1(0:nga-1))

      allocate(gn_o(0:ngo-1))
      allocate(ALON_o(0:ngo-1),ALAT_o(0:ngo-1),
     >SLM_o(0:ngo-1),MASK_o(0:ngo-1),
     >force_extrap_o(0:ngo-1))

      allocate(gn_w(0:ngw-1))
      allocate(ALON_w(0:ngw-1),ALAT_w(0:ngw-1),
     >SLM_w(0:ngw-1),MASK_w(0:ngw-1),ICE_w(0:ngw-1),
     >force_extrap_w(0:ngw-1))

      gn_a(0)='T'
      if (nga.eq.2) then
        gn_a(1)='V'
      else if (nga.eq.3) then
        gn_a(1)='u'
        gn_a(2)='v'
      else if (nga.ne.1) then
        print*,'C: nga=',nga
        call GLOB_ABORT(1,'nga is not 1, 2, or 3, aborted',1)
      end if
         print*,'gn_a:',gn_a

      gn_o(0)='T'
      if (ngo.eq.2) then
        gn_o(1)='V'
      else if (ngo.eq.3) then
        gn_o(1)='u'
        gn_o(2)='v'
      else if (ngo.ne.1) then
        print*,'C: ngo=',ngo
        call GLOB_ABORT(1,'ngo is not 1, 2, or 3, aborted',1)
      end if
         print*,'gn_o:',gn_o

      gn_w(0)='1'
      if (ngw.ne.1) then
        print*,'C: ngw=',ngw
        call GLOB_ABORT(1,'ngw is not 1, aborted',1)
      end if

      allocate(
     >  Iag_o(0:ngo-1,0:nga-1,L:nda), Jag_o(0:ngo-1,0:nga-1,L:nda),
     >  Iag_w(0:ngw-1,0:nga-1,L:nda), Jag_w(0:ngw-1,0:nga-1,L:nda),
     >  Iog_a(0:nga-1,L:nda,0:ngo-1), Jog_a(0:nga-1,L:nda,0:ngo-1),
     >  cosag_o(0:ngo-1,0:nga-1),sinag_o(0:ngo-1,0:nga-1),
     >  cosag_w(0:ngw-1,0:nga-1),sinag_w(0:ngw-1,0:nga-1))

C
C
CB1
C Receive AM, OM, WM grid dimensions
C and OM time step; send coupling period to AM, OM, WM;
C allocate arrays, receive AM, OM, WM grids
 
C Receive OM time step; send coupling period to OM and AM

      IF (Ocean_id.gt.0) THEN

      call CPL_ANNOUNCE('to receive OM time step',1)
      call CPL_R(buf,1,Ocean_id,knd_R(Ocean_id))
      write(s,'(1pe20.12)') buf
      call CPL_ANNOUNCE('OM time step='//trim(s)//' received',1)

      ELSE

      buf(1)=0.
      call CPL_ANNOUNCE('OM time step set =0. (no OM)',1)

      END IF

      if (dt_c.eq.0.) then
        if (Ocean_id.le.0) call GLOB_ABORT(1,
     >  'No value for coupling period dt_c',1)
        write(s,'(1pe20.12)') buf
        call CPL_ANNOUNCE('No input value for coupling period dt_c. '//
     >  'Default assumed: dt_c = OM time step ='//s,0)
      else
        write(s,'(1pe20.12)') dt_c
        call CPL_ANNOUNCE('Coupling period dt_c (input value) ='//s,0)
        if (buf(1).ne.0.) then
          z=dt_c/buf(1)
          if (abs(nint(z)-z).gt.1.e-7) CALL GLOB_ABORT(1,
     >    'C: ABORTED: dt_c must be a multiple of OM time step '//s,1)
        end if
        buf(1)=dt_c
      end if

      call CPL_ANNOUNCE('to send coupling period to AM',1)
      call CPL_S(buf,1,Atmos_id,knd_R(Atmos_id))
      call CPL_ANNOUNCE('Coupling period sent to AM',1)
            
      if (Ocean_id.gt.0) then
        call CPL_ANNOUNCE('to send coupling period to OM',1)
        call CPL_S(buf,1,Ocean_id,knd_R(Ocean_id))
        call CPL_ANNOUNCE('Coupling period sent to OM',1)
      end if

      if (Waves_id.gt.0) then
        call CPL_ANNOUNCE('to send coupling period to WM',1)
        call CPL_S(buf,1,Waves_id,knd_R(Waves_id))
        call CPL_ANNOUNCE('Coupling period sent to WM',1)
      end if

C Receive AM, OM, WM grid dimensions

      if (Atmos_id.le.0) call GLOB_ABORT(1,
     >'Atmos_id.le.0 but this Coupler version requires an active AM',1)

      call CPL_ANNOUNCE('to receive AMG dimensions, '//dn_a(1),3)
      call CPL_INTEGER_RECV(ibuf,2,Atmos_id)
      write(s,'(2i6)') ibuf
      call CPL_ANNOUNCE(
     >'AM '//dn_a(1)//' dimensions received: '//s,2)
!
      if (Atmos_spec.ne.3) then ! real HWRF or dummy HWRF with HWRF grid
!
        call GLOB_ABORT(abs(nxa(1)-ibuf(1))+abs(nya(1)-ibuf(2)),
     >  'Grid sizes received differ from those read from namelist',1)
!
      else                      ! dummy HWRF with WM grid
!
        nxa(1)=ibuf(1)
        nya(1)=ibuf(2)
        agridrat=1
        call CPL_ANNOUNCE('to receive AMG dimensions, '//dn_a(2),3)
        call CPL_INTEGER_RECV(ibuf,2,Atmos_id)
        write(s,'(2i6)') ibuf
        call CPL_ANNOUNCE(
     >  'AM '//dn_a(2)//' dimensions received: '//s,2)
        nxa(2)=ibuf(1)
        nya(2)=ibuf(2)
!
      end if
!

      IF (nda.gt.1) THEN

      nxa(0)=agridrat*(nxa(1)-1)+1
      nya(0)=agridrat*(nya(1)-1)+1

      write(s,'(i3,2i6,4x,2i6,4x,2i6)') Atmos_id,
     >(nxa(kd),nya(kd),kd=L,nda)
      call CPL_ANNOUNCE('Atmos_id, nxa, nya: '//s,1)

      END IF

      call CPL_ANNOUNCE('to receive OMG dimensions',3)
      ibuf=0    !<- for the case of no OM
      call CPL_INTEGER_RECV(ibuf,2,Ocean_id)
      write(s,'(2i6)') ibuf
      call CPL_ANNOUNCE('OMG dimensions received: '//s,2)
      nxo=ibuf(1)
      nyo=ibuf(2)

      call CPL_ANNOUNCE('to receive WMG dimensions',3)
      ibuf=0    !<- for the case of no WM
      call CPL_INTEGER_RECV(ibuf,2,Waves_id)
      write(s,'(2i6)') ibuf
      call CPL_ANNOUNCE('WMG dimensions received: '//s,2)
      nxw=ibuf(1)
      nyw=ibuf(2)

      write(s,'(i3,2i6)') Ocean_id,nxo,nyo
      call CPL_ANNOUNCE('Ocean_id, nxo, nyo: '//s,1)

      write(s,'(i3,2i6)') Waves_id,nxw,nyw
      call CPL_ANNOUNCE('Waves_id, nxw, nyw: '//s,1)
        
      nxnya=nxa*nya
      nxnyo=nxo*nyo
      nxnyw=nxw*nyw
             ! the above are numbers of the components' (2D) grid points
!
      IF (MASTER) THEN
!
C Open additional output file (for FMG status records):

      IF (nda.gt.1) THEN

        if (Ocean_id.gt.0) then
          open(16,file='MDstatus',form='formatted',status='unknown')
          MDout=.false.
          MDout_sea=.false.
        end if

      END IF

C  Open file for diagnostics (duplicates part of stdout, disposable):
        open(17,file='C_flux_momenta',
     >  form='unformatted',status='unknown')
!
      END IF
!
C Set local (=per-process) dimensions:

!   this Coupler version requires an active AM (see above), thus no IF:
      do kd=L,nda  ! <- correction 11/27/2012, 12/12/2012: L replacing 1
        call SET_LOCDIM(
     >  nxa(kd),nya(kd),nxa_l(kd),nya_l(kd),nxnya_l(kd),nxnya_l1(kd),
     >  ia_l_f(kd),ia_l_l(kd),ja_l_f(kd),ja_l_l(kd),
     >  ija_l_f(kd),ija_l_l(kd))
        print*,'SET_LOCDIM args, A, kd=',kd,':',
     >  nxa(kd),nya(kd),nxa_l(kd),nya_l(kd),nxnya_l(kd),nxnya_l1(kd),
     >  ia_l_f(kd),ia_l_l(kd),ja_l_f(kd),ja_l_l(kd),
     >  ija_l_f(kd),ija_l_l(kd)
      end do

!     IF (Ocean_id.gt.0) THEN
        call SET_LOCDIM(nxo,nyo,nxo_l,nyo_l,nxnyo_l,nxnyo_l1,
     >  io_l_f,io_l_l,jo_l_f,jo_l_l,ijo_l_f,ijo_l_l)
        print*,'SET_LOCDIM args, O:',
     >  nxo,nyo,nxo_l,nyo_l,nxnyo_l,nxnyo_l1,
     >  io_l_f,io_l_l,jo_l_f,jo_l_l,ijo_l_f,ijo_l_l
!     END IF

!     IF (Waves_id.gt.0) THEN
        call SET_LOCDIM(nxw,nyw,nxw_l,nyw_l,nxnyw_l,nxnyw_l1,
     >  iw_l_f,iw_l_l,jw_l_f,jw_l_l,ijw_l_f,ijw_l_l)
        print*,'SET_LOCDIM args, W:',
     >  nxw,nyw,nxw_l,nyw_l,nxnyw_l,nxnyw_l1,
     >  iw_l_f,iw_l_l,jw_l_f,jw_l_l,ijw_l_f,ijw_l_l
!     END IF

C Allocate global (=entire-grid) and local (=tile) grid arrays:

      allocate(SST_a_r%gar(nxa(1),nya(1)),it(nxa(1),nya(1)),
     >bf_a(nxa(1),nya(1)))
      call pointlar(SST_a_r,Atmos_id,kd=1)  ! only for fast analysis
      if (nda.gt.1) then
        allocate(buf_a%gar(nxa(2),nya(2)))
        call pointlar(buf_a,Atmos_id,kd=2)
      end if
      do k=0,nga-1
        do kd=L,nda
          allocate(ALON_a(k,kd)%gar(nxa(kd),nya(kd)),
     >             ALAT_a(k,kd)%gar(nxa(kd),nya(kd)),
     >              SLM_a(k,kd)%gar(nxa(kd),nya(kd)),
     >               ia_a(k,kd)%ar(nxa(kd),nya(kd)),
     >               ja_a(k,kd)%ar(nxa(kd),nya(kd)),
     >               ib_a(k,kd)%ar(nxa(kd),nya(kd)),
     >               jb_a(k,kd)%ar(nxa(kd),nya(kd)),
     >               ic_a(k,kd)%ar(nxa(kd),nya(kd)),
     >               jc_a(k,kd)%ar(nxa(kd),nya(kd)),
     >             MASK_a(k,kd)%gar(nxa(kd),nya(kd)))
          call pointlar(ALON_a(k,kd),Atmos_id,kd)
          call pointlar(ALAT_a(k,kd),Atmos_id,kd)
          call pointlar(SLM_a(k,kd),Atmos_id,kd) !probably not needed
          call ipointlar(MASK_a(k,kd),Atmos_id,kd)
        end do
        do kd=L,1
          allocate(ibnd_a(k,kd)%ar(0:nxnya(kd)),
     >             jbnd_a(k,kd)%ar(0:nxnya(kd)))
        end do
        IF (nda.gt.1) allocate(MASK_a2(k)%gar(nxa(2),nya(2)))
        allocate(force_extrap_a(k)%ar(nxa(1),nya(1)))
        force_extrap_a(k)%ar=0
      end do
      do kd=1,nda
        allocate(SST_a(kd)%gar(nxa(kd),nya(kd)),
     >          SST_a0(kd)%gar(nxa(kd),nya(kd)),
     >          dQdT_a(kd)%gar(nxa(kd),nya(kd)))
        call pointlar(SST_a(kd),Atmos_id,kd) !could be just allocated:
!       allocate(SST_a(kd)%lar(nxa_l(kd),nya_l(kd)))
        do n=1,NSF_a
          allocate(SF_a(n,kd)%gar(nxa(kd),nya(kd))) ! gar allocation:
          call pointlar(SF_a(n,kd),Atmos_id,kd)     ! for analysis only
        end do
        SF_a2o_a(1:NSF_a2o,kd)=SF_a(1:NSF_a2o,kd)
        SF_a2w_a(1:NSF_a2w,kd)=SF_a(NSF_a-NSF_a2w+1:NSF_a,kd)
               ! see comment above to line NSF_a=NSF_a2o+NSF_a2w
      end do

      do k=0,ngo-1
        allocate(ALON_o(k)%gar(nxo,nyo),
     >           ALAT_o(k)%gar(nxo,nyo),
     >            SLM_o(k)%gar(nxo,nyo),
     >           MASK_o(k)%gar(nxo,nyo))
        call pointlar(ALON_o(k),Ocean_id)
        call pointlar(ALAT_o(k),Ocean_id)
        call pointlar(SLM_o(k),Ocean_id)  ! probably not needed
        call ipointlar(MASK_o(k),Ocean_id)
        allocate(force_extrap_o(k)%ar(nxo,nyo))
        force_extrap_o(k)%ar=0
      end do
      do n=1,NSF_a2o
        allocate(SF_a2o_o(n)%gar(nxo,nyo))
        call pointlar(SF_a2o_o(n),Ocean_id)
      end do
      allocate(SST_o%gar(nxo,nyo),
     >buf_o%gar(nxo,nyo),
     >SST_o_i(nxo,nyo),dQdT_o(nxo,nyo))
      buf_o%gar=too_low10
      call pointlar(buf_o,Ocean_id)
      call pointlar(SST_o,Ocean_id)  ! only for fast analysis

      allocate(buf1_o(nxo_l,nyo_l),buf2_o(nxo_l,nyo_l))

      do k=0,ngw-1
        allocate(ALON_w(k)%gar(nxw,nyw),
     >           ALAT_w(k)%gar(nxw,nyw),
     >            SLM_w(k)%gar(nxw,nyw),
     >           MASK_w(k)%gar(nxw,nyw),
     >            ICE_w(k)%gar(nxw,nyw))
        call pointlar(ALON_w(k),Waves_id)
        call pointlar(ALAT_w(k),Waves_id)
        call pointlar(SLM_w(k),Waves_id)  ! probably not needed
        call ipointlar(MASK_w(k),Waves_id)
        call ipointlar(ICE_w(k),Waves_id)
        allocate(force_extrap_w(k)%ar(nxw,nyw))
        force_extrap_w(k)%ar=0
      end do
      do n=1,NSF_a2w
        allocate(SF_a2w_w(n)%gar(nxw,nyw))
        call pointlar(SF_a2w_w(n),Waves_id)
      end do
      allocate(buf_w%gar(nxw,nyw))
      buf_w%gar=too_low10
      call pointlar(buf_w,Waves_id)

      allocate(buf1_w(nxw_l,nyw_l),buf2_w(nxw_l,nyw_l))

      call CPL_ANNOUNCE('global and local grid arrays allocated',2)

C Receive grids:
! [for AM domain 0 (Fine Stationary Grid), also sea/land mask is
! read, not MPI-received]

      IF (Atmos_id.gt.0) THEN

      call CPL_ANNOUNCE(
     >'to receive AM '//dn_a(1)//' arrays (NG_a*2 MPI calls)',3)

      call RECV_COOR(Atmos_id,nga,nxa(1),nya(1),nxnya(1),
     >ALON_a(:,1),ALAT_a(:,1),gn_a)
!
      if (Atmos_spec.ne.3) then
!
        IF (nda.gt.1) THEN

        !*
        IF (MASTER) THEN
        !*
        call CPL_ANNOUNCE('to get '//dn_a(0)//' arrays from a file',2)
        call GLOB_ABORT(NG_a-2,'There must be exactly 2 types of grid ('
     >  //gn_a(0)//', '//gn_a(1)//
     >  ') in AM to call the WRF-specific routine coor_mask_a',1)
        call coor_mask_a(nxa(0),nya(0),
     >  ALON_a(0,0)%gar,ALAT_a(0,0)%gar,
     >  ALON_a(1,0)%gar,ALAT_a(1,0)%gar,
     >  SLM_a(0,0)%gar,SLM_a(1,0)%gar)

        call RECV_COOR(Atmos_id,nga,nxa(0),nya(0),nxnya(0),
     >  ALON_a(:,0),ALAT_a(:,0),gn_a,RECV=.false.)
        !*
        END IF
        !*
C         because RECV=.false. in last call RECV_COOR:
        do k=0,nga-1
          call CPL_BC(ALON_a(k,0)%gar,nxnya(0))
          call CPL_BC(ALAT_a(k,0)%gar,nxnya(0))
          call CPL_BC(SLM_a(k,0)%gar,nxnya(0))
        end do

        END IF
!
      else
!
        ALON_a(:,0)=ALON_a(:,1)
        ALAT_a(:,0)=ALAT_a(:,1)
        SLM_a(:,0)=SLM_a(:,1)

        a2w_samegrid=.true.
!
      end if
!
      ELSE
                    !
      call GLOB_ABORT(1,
     >'Atmos_id=<0 is not allowed in this Coupler: AM must be active',1)
                    !
      END IF

      call RECV_COOR(Ocean_id,ngo,nxo,nyo,nxnyo,ALON_o,ALAT_o,gn_o)

      call RECV_COOR(Waves_id,ngw,nxw,nyw,nxnyw,ALON_w,ALAT_w,gn_w)
C
CE1

C     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C  to interpolate surface fields, the coupler needs all sea/land masks
CB2

C Receive sea/land mask from AM:
!>aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

      do k=0,NG_a-1
        call CPL_ANNOUNCE(
     >  'to receive AM '//dn_a(1)//' sea/land mask, '//gn_a(k)//
     >  '-points',3)
        call CPL_R(SLM_a(k,1)%gar,nxnya(1),Atmos_id,knd_SLM(Atmos_id))
        call CPL_ANNOUNCE(
     >  'AM '//dn_a(1)//' sea/land mask received, '//gn_a(k)//
     >  '-points',1)
      end do

!<aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
C Receive sea/land mask from OM:
!>oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

      IF (Ocean_id.gt.0) THEN

      do k=0,NG_o-1
        call CPL_ANNOUNCE(
     >  'to receive OMG sea/land mask, '//gn_o(k)//'-points',3)
        call CPL_R(SLM_o(k)%gar,nxnyo,Ocean_id,knd_SLM(Ocean_id))
        call CPL_ANNOUNCE(
     >  'OMG sea/land mask received from OM, '//gn_o(k)//'-points',1)
      end do

      END IF

!<oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

C Receive sea/land mask from WM:
!>wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

      IF (Waves_id.gt.0) THEN

      do k=0,ngw-1
        call CPL_ANNOUNCE(
     >  'to receive WMG sea/land mask, '//gn_w(k)//'-points',3)
        call CPL_R(SLM_w(k)%gar,nxnyw,Waves_id,knd_SLM(Waves_id))
        call CPL_ANNOUNCE(
     >  'WMG sea/land mask received from WM, '//gn_w(k)//'-points',1)
      end do

      END IF

!<wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

C Invert indexation if necessary:
C - with general (not lat.-lon.) grid this is presumably pointless    
C
C   Define integer surface mask arrays for AM, OM, WM
C   [So far only sea (value 0) and land (value 1) are distinguished
C   on existent gridpoints; -1 is assigned at nonexistent gridpoints,
C   the latter being ones for which there are no valid coordinate
C   values.]:->
C
      do kd=L,1
        do k=0,nga-1
          do j=1,nya(kd)
          do i=1,nxa(kd)
            if (SLM_a(k,kd)%gar(i,j).gt.1.E-4 .and.
     >          abs(SLM_a(k,kd)%gar(i,j)-1.).gt.1.E-4) then
              print*,'C: ',i,j,k,kd,SLM_a(k,kd)%gar(i,j)
              CALL GLOB_ABORT(1,'C: Bad AM sea/land mask',1)
            end if
            if (SLM_a(k,kd)%gar(i,j).lt.-1.E-4) then ! no such points
                                                   ! expected; see above
              MASK_a(k,kd)%gar(i,j)=-1             ! if (abs(SLM_a...
            else
              MASK_a(k,kd)%gar(i,j)=SLM_a(k,kd)%gar(i,j)*(1-a_sea)+
     >        (1.-SLM_a(k,kd)%gar(i,j))*a_sea+0.01
            end if
          end do
          end do
        end do
      end do

      do k=0,ngo-1
        do j=1,nyo
        do i=1,nxo
          if (SLM_o(k)%gar(i,j).gt.1.E-4 .and.
     >        abs(SLM_o(k)%gar(i,j)-1.).gt.1.E-4) then
            print*,'C: ',i,j,k,SLM_o(k)%gar(i,j)
            CALL GLOB_ABORT(1,'C: Bad OM sea/land mask',1)
          end if
          if (SLM_o(k)%gar(i,j).lt.-1.E-4) then
            MASK_o(k)%gar(i,j)=-1
          else
            MASK_o(k)%gar(i,j)=SLM_o(k)%gar(i,j)*(1-o_sea)+
     >      (1.-SLM_o(k)%gar(i,j))*o_sea+0.01
          end if
        end do
        end do
      end do

      do k=0,ngw-1
        do j=1,nyw
        do i=1,nxw
          if (SLM_w(k)%gar(i,j).gt.1.E-4 .and.
     >        abs(SLM_w(k)%gar(i,j)-1.).gt.1.E-4) then
            print*,'C: ',i,j,k,SLM_w(k)%gar(i,j)
            CALL GLOB_ABORT(1,'C: Bad WM sea/land mask',1)
          end if
          if (SLM_w(k)%gar(i,j).lt.-1.E-4) then
            MASK_w(k)%gar(i,j)=-1
          else
            MASK_w(k)%gar(i,j)=SLM_w(k)%gar(i,j)*(1-w_sea)+
     >      (1.-SLM_w(k)%gar(i,j))*w_sea+0.01
          end if
        end do
        end do
      end do
C
C   Check the masks' consistency and terminate if necessary :->
C
C    - deleted as not applicable to general grids. To be rewritten/added
C      if necessary

C
C   Convert values of integer surface mask arrays to meet requirements
C   of interpolation procedures :->
C
C    - no conversion is currently required

C Begin WRF specific

        n=0

        do k=0,nga-1
!
          if (Atmos_spec.eq.3) then
!
            km_a(k,:)=2*(nxa+nya)-4
            ibnd_a(k,1)%ar(0:nxa(1)-1)=(/(i,i=1,nxa(1))/)
            jbnd_a(k,1)%ar(0:nxa(1)-1)=1
            ibnd_a(k,1)%ar(nxa(1):nxa(1)+nya(1)-2)=nxa(1)
            jbnd_a(k,1)%ar(nxa(1):nxa(1)+nya(1)-2)=(/(j,j=2,nya(1))/)
            ibnd_a(k,1)%ar(nxa(1)+nya(1)-1:2*nxa(1)+nya(1)-3)=
     >      (/(i,i=nxa(1)-1,1,-1)/)
            jbnd_a(k,1)%ar(nxa(1)+nya(1)-1:2*nxa(1)+nya(1)-3)=nya(1)
            ibnd_a(k,1)%ar(2*nxa(1)+nya(1)-2:km_a(k,1))=1
            jbnd_a(k,1)%ar(2*nxa(1)+nya(1)-2:km_a(k,1))=
     >      (/(j,j=nya(1)-1,1,-1)/)
            ibnd_a(k,L)%ar=ibnd_a(k,1)%ar
            jbnd_a(k,L)%ar=jbnd_a(k,1)%ar
            do kd=L,nda                   !
              do j=1,nya(kd)              !
              do i=1,nxa(kd)              !
                ia_a(k,kd)%ar(i,j)=i+1    !
                ja_a(k,kd)%ar(i,j)=j      !
                ib_a(k,kd)%ar(i,j)=i+1    !
                jb_a(k,kd)%ar(i,j)=j+1    !
                ic_a(k,kd)%ar(i,j)=i      !
                jc_a(k,kd)%ar(i,j)=j+1    !
              end do                      !
              end do                      !
            end do                        !

            CYCLE
!
          end if
!

          IF (nda.gt.1 .and. MASTER) THEN

          call get_subgrid(k,nxa(0),nya(0),nxa(1),nya(1),agridrat,
     >    ALON_a(k,0)%gar,bf_a,999.)
          z=fraction(abs(ALON_a(k,1)%gar-bf_a).ge.0.01 .and.
     >    bf_a.ne.999.)
          if (z.ge.1.E-8) then
            n=n+1
            print*,'C: shitty AM SG '//gn_a(k)//
     >      ' longitudes, shit ratio =',z
            call print_offenders(10,abs(ALON_a(k,1)%gar-bf_a).ge.0.01
     >      .and. bf_a.ne.999.,ALON_a(k,1)%gar,bf_a)
          end if
          call get_subgrid(k,nxa(0),nya(0),nxa(1),nya(1),agridrat,
     >    ALAT_a(k,0)%gar,bf_a,999.)
          z=fraction(abs(ALAT_a(k,1)%gar-bf_a).ge.0.01 .and.
     >    bf_a.ne.999.)
          if (z.ge.1.E-8) then
            n=n+1
            print*,'C: shitty AM SG '//gn_a(k)//
     >      ' latitudes, shit ratio =',z
            call print_offenders(10,abs(ALAT_a(k,1)%gar-bf_a).ge.0.01
     >      .and. bf_a.ne.999.,ALAT_a(k,1)%gar,bf_a)
          end if
          call get_subgrid(k,nxa(0),nya(0),nxa(1),nya(1),agridrat,
     >    SLM_a(k,0)%gar,bf_a,999.)
                              ! mask values at these gridpoints are
          bf_a(1,:)=999.      ! inconsequential; whereas, as of tests
          bf_a(nxa(1),:)=999. ! of 2/19/07, there are discrepancies
          bf_a(:,1)=999.      ! betw. the CSG mask sent by AM and that
          bf_a(:,nya(1))=999. ! obtained as partial array from FSG
                              ! mask at i=nxa(1)
          z=fraction(abs(SLM_a(k,1)%gar-bf_a).ge.0.01 .and.
     >    bf_a.ne.999.)
          if (z.ge.1.E-8) then

            ! This check's abort (n=n+1) is disabled because the check
            ! relies on a false assumption about the relationship between
            ! the 27km and 9km WRF landmasks.  The assumption is that the
            ! (lat,lon) point in the 9km landmask that is closest to a 
            ! 27km point has the same LSM value.  WRF generates the 9km
            ! landmask from the 27km landmask using an interpolation 
            ! routine that does not make that guarantee.

            ! n=n+1

            print*,'C: shitty AM SG '//gn_a(k)//
     >      ' SLM, shit ratio =',z
            call print_offenders(10,abs(SLM_a(k,1)%gar-bf_a).ge.0.01
     >      .and. bf_a.ne.999.,SLM_a(k,1)%gar,bf_a)
          end if

          END IF

          do kd=L,nda
            call GRID_CELL_a(k,nxa(kd),nya(kd),
     >      ia_a(k,kd)%ar,ja_a(k,kd)%ar,
     >      ib_a(k,kd)%ar,jb_a(k,kd)%ar,
     >      ic_a(k,kd)%ar,jc_a(k,kd)%ar)
          end do
          do kd=L,1
            call DOMAIN_BOUNDARY_a(k,nxa(kd),nya(kd),
     >      km_a(k,kd),ibnd_a(k,kd)%ar,jbnd_a(k,kd)%ar)
          end do

          IF (nda.gt.1) THEN

          call IPRI2D(km_a(k,0)+1,1,ibnd_a(k,0)%ar,
     >    'AM FSG boundary i-s, '//gn_a(k)//':',5)
          call IPRI2D(km_a(k,0)+1,1,jbnd_a(k,0)%ar,
     >    'AM FSG boundary j-s, '//gn_a(k)//':',5)
          if (VerbLev.ge.2) then
            call PRI2D(km_a(k,0)+1,1,
     >      ALON_a(k,0)%gar(ibnd_a(k,0)%ar(0:km_a(k,0)),
     >                     jbnd_a(k,0)%ar(0:km_a(k,0))),
     >      'AM FSG boundary longitudes, '//gn_a(k)//':')
            call PRI2D(km_a(k,0)+1,1,
     >      ALAT_a(k,0)%gar(ibnd_a(k,0)%ar(0:km_a(k,0)),
     >                     jbnd_a(k,0)%ar(0:km_a(k,0))),
     >      'AM FSG boundary latitudes, '//gn_a(k)//':')
          end if

          END IF

        end do

        IF (nda.gt.1 .and. MASTER) THEN

        call GLOB_ABORT(n,'CSG is not a subset of FSG',1)

        call CHECK_FSG_against_CSG(nxa(1),nya(1),nxa(0),nya(0),agridrat,
     >  ALON_a(0,1)%gar,ALAT_a(0,1)%gar,MASK_a(0,1)%gar,
     >  ALON_a(1,1)%gar,ALAT_a(1,1)%gar,MASK_a(1,1)%gar,
     >  ALON_a(0,0)%gar,ALAT_a(0,0)%gar,MASK_a(0,0)%gar,
     >  ALON_a(1,0)%gar,ALAT_a(1,0)%gar,MASK_a(1,0)%gar,ierr)
        print*,'C: CHECK_FSG_against_CSG called, ierr=',ierr
        if (ierr.lt.0 .and. Atmos_spec.ne.3) call GLOB_ABORT(ierr,
     >  'SEVERE FAILURE when checking FSG against CSG',1)
        if (ierr.ge.0) then
          i=ierr-10*(ierr/10)
          if (i.ge.1 .and. i.le.3) then
            if (i.ne.2) print*,'C: *!* FSG H-mask is NOT that obtained'
     >      //' from CSG H-mask by nearest neighbor method'
            if (i.ge.2) print*,'C: *!* SG V-mask is NOT that obtained'//
     >      ' with the original method i.e. not equal to H-mask'
            if (i.eq.ierr) ierr=0
          end if
          call GLOB_ABORT(ierr,
     >    'FSG coordinates FAILED checking against CSG coordinates',1)
        end if

        END IF

        if (Atmos_spec.ne.3) then
          do k=0,nga-1
            call MAKE_BOUNDARY_MASK_LAND_a(nxa(1),nya(1),
     >      MASK_a(k,1)%gar)
          end do
        end if

C End WRF specific

      do k=0,nga-1
        call IPRI2D(nxa(1),nya(1),MASK_a(k,1)%gar,
     >  'AM '//dn_a(1)//' MASK, '//gn_a(k)//':',1)
      end do

      do k=0,ngo-1
        call IPRI2D(nxo,nyo,MASK_o(k)%gar,'OM MASK, '//gn_o(k)//':',1)
      end do

      do k=0,ngw-1
        call IPRI2D(nxw,nyw,MASK_w(k)%gar,'WM MASK, '//gn_w(k)//':',1)
      end do

C  Receiving and processing AM, OM, WM mask arrays above may be
C  advisable to make 3 calls to one (internal) subroutine

CE2
C

CB3  Initialization of interpolation procedures
C
C
      FORCE_INTP_INIT=FORCE_INTP_INIT.and..not.noii

      if (FORCE_INTP_INIT) then
!       if (VerbLev.gt.1 .or. prep) then ! as of 4/29/2013
        if (prep.or.iiwrt) then          !
          iiunit=15
        else
          iiunit=0
        end if
      else
        iiunit=-15
      end if

      if (iiunit.ne.0) then
        write(s,'(i0)') process_rank_local
        open(abs(iiunit),file='intp_init'//trim(s),
     >       form='unformatted',status='unknown')
      end if

      ierr=0

      call CPL_BARR  ! is it required?

!===
      DO kd=L,1
!===
!O<->A
      IF (Ocean_id.gt.0) THEN
!O<->A
      call CPL_ANNOUNCE('to initialize O->A '//dn_a(kd)//
     >' interpolation, '//gn_o(0)//'->'//gn_a(0),1)
      call PRDT(1)
      if (kd.eq.0) then                            !+ this merely
        allocate(Iog_a(0,0,0)%gar(nxa(0),nya(0)),  !+ saves a little
     >           Jog_a(0,0,0)%gar(nxa(0),nya(0)))  !+ memory
        call ipointlar(Iog_a(0,0,0),Atmos_id,kd=0) !+ (see below: !-)
        call ipointlar(Jog_a(0,0,0),Atmos_id,kd=0) !+
      else                                         !+
        allocate(Iog_a(0,kd,0)%lar(nxa_l(kd),nya_l(kd)),
     >           Jog_a(0,kd,0)%lar(nxa_l(kd),nya_l(kd)))
      end if                                       !+

C          The following called routine must be compiled with forced
C          kind_REAL for REAL and kind_INTEGER for INTEGER (see module
C          CPL_COMM for the values)
C                                   - currently, 8 and 4 respectively,
C          and if these change, adjustment will have to made to the subr

C        O->A: h-p.->H-p. (normally 0->0):
      call INTP_INI(nxo,nyo,nxa_l(kd),nya_l(kd),
     >ALON_o(0)%gar,ALAT_o(0)%gar,ALON_a(0,kd)%lar,ALAT_a(0,kd)%lar,
     >MASK_o(0)%gar,MASK_a(0,kd)%lar,
     >Iog_a(0,kd,0)%lar,Jog_a(0,kd,0)%lar,ierr,
     >nvrbs=VerbLev-1,iiunit=-iiunit)
      call PRDT(1)
      CALL GLOB_ABORT(ierr,'C: INTP_INI for O->A '//dn_a(kd)//' '//
     >gn_o(0)//'->'//gn_a(0)//' failed, ABORTED',1)
      if (kd.eq.0) then
!-      allocate(Iog_a(0,0,0)%gar(nxa(0),nya(0)),   !-  (ee above: !+)
!-   >           Jog_a(0,0,0)%gar(nxa(0),nya(0)))   !-
        call IGATHM(Iog_a(0,kd,0),nxnya_l(kd),nxnya_l1(kd))
        call IGATHM(Jog_a(0,kd,0),nxnya_l(kd),nxnya_l1(kd))
        call CPL_INTEGER_BC(Iog_a(0,kd,0)%gar,nxnya(kd))
        call CPL_INTEGER_BC(Jog_a(0,kd,0)%gar,nxnya(kd))
      end if

      if (ngo.gt.1 .or. nga.gt.1) then
        call CPL_ANNOUNCE('to initialize A '//dn_a(kd)//
     >  '->O interpolation, '//gn_a(0)//'->'//gn_o(0),1)
        call PRDT(1)
        allocate(Iag_o(0,0,kd)%lar(nxo_l,nyo_l),
     >           Jag_o(0,0,kd)%lar(nxo_l,nyo_l))
        if (nda.gt.1 .and. kd.eq.0)
     >    allocate(Iag_o(0,0,2)%lar(nxo_l,nyo_l),
     >             Jag_o(0,0,2)%lar(nxo_l,nyo_l))
C        A->O: H-p.->h-p. (0->0):
        call INTP_INI(nxa(kd),nya(kd),nxo_l,nyo_l,
     >  ALON_a(0,kd)%gar,ALAT_a(0,kd)%gar,ALON_o(0)%lar,ALAT_o(0)%lar,
     >  MASK_a(0,kd)%gar,MASK_o(0)%lar,
     >  Iag_o(0,0,kd)%lar,Jag_o(0,0,kd)%lar,ierr,
     >  ia=ia_a(0,kd)%ar,ja=ja_a(0,kd)%ar,
     >  ib=ib_a(0,kd)%ar,jb=jb_a(0,kd)%ar,
     >  ic=ic_a(0,kd)%ar,jc=jc_a(0,kd)%ar,
     >  KM1=km_a(0,kd),ibndr1=ibnd_a(0,kd)%ar,jbndr1=jbnd_a(0,kd)%ar,
     >  nvrbs=VerbLev-1,iiunit=-iiunit)
        call PRDT(1)
        CALL GLOB_ABORT(ierr,'C: INTP_INI for A '//dn_a(kd)//'->O '//
     >  gn_a(0)//'->'//gn_o(0)//' failed, ABORTED',1)
        if (VerbLev.ge.3) then
          if (kd.eq.0) then
!           write (s1,'("FSG Iag_o%lar, m=",i1,", k=",i1)') 0,0 !dbfork
!           call IPRI2D(nxo,nyo,Iag_o(0,0,0)%lar,s1,5)          !dbfork
!           write (s1,'("FSG Jag_o%lar, m=",i1,", k=",i1)') 0,0 !dbfork
!           call IPRI2D(nxo,nyo,Jag_o(0,0,0)%lar,s1,5)          !dbfork
            allocate(Iag_o(0,0,0)%gar(nxo,nyo),
     >               Jag_o(0,0,0)%gar(nxo,nyo))
            call IGATHM(Iag_o(0,0,0),nxnyo_l,nxnyo_l1)
            call IGATHM(Jag_o(0,0,0),nxnyo_l,nxnyo_l1)
            write (s1,'("FSG Iag_o, m=",i1,", k=",i1)') 0,0
            call IPRI2D(nxo,nyo,Iag_o(0,0,0)%gar,s1,5)
            write (s1,'("FSG Jag_o, m=",i1,", k=",i1)') 0,0
            call IPRI2D(nxo,nyo,Jag_o(0,0,0)%gar,s1,5)
          end if
        end if
      end if
      do m=min(1,ngo-1),ngo-1
      do k=min(1,nga-1),nga-1
        call CPL_ANNOUNCE('to initialize A '//dn_a(kd)//
     >  '->O interpolation, '//gn_a(k)//'->'//gn_o(m),1)
        call PRDT(1)
        allocate(Iag_o(m,k,kd)%lar(nxo_l,nyo_l),
     >           Jag_o(m,k,kd)%lar(nxo_l,nyo_l))
        if (nda.gt.1 .and. kd.eq.0)
     >    allocate(Iag_o(m,k,2)%lar(nxo_l,nyo_l),
     >             Jag_o(m,k,2)%lar(nxo_l,nyo_l))
        if (kd.eq.L)
     >    allocate(cosag_o(m,k)%lar(nxo_l,nyo_l),
     >             sinag_o(m,k)%lar(nxo_l,nyo_l))

!     below V-p. - AM gridpoints for velocity components; currently same
!     for both components (grid 1) but may be different (in which case
!     cosag, sinag stuff may need revision);
!           u-p., v-p. - OM gridpoints for velocity components;
!     currently different for the two components, grids 1,2, but may
!     be same (in which case cosag, sinag stuff may need revision)
C        A->O: V-p.->u-p.,v-p. (normally 1->1,2):
        if (kd.eq.L) then ! include COSANG, SINANG
          call INTP_INI(nxa(kd),nya(kd),nxo_l,nyo_l,
     >    ALON_a(k,kd)%gar,ALAT_a(k,kd)%gar,ALON_o(m)%lar,ALAT_o(m)%lar,
     >    MASK_a(k,kd)%gar,MASK_o(m)%lar,
     >    Iag_o(m,k,kd)%lar,Jag_o(m,k,kd)%lar,ierr,
     >    ia=ia_a(k,kd)%ar,ja=ja_a(k,kd)%ar,
     >    ib=ib_a(k,kd)%ar,jb=jb_a(k,kd)%ar,
     >    ic=ic_a(k,kd)%ar,jc=jc_a(k,kd)%ar,
     >    KM1=km_a(k,kd),ibndr1=ibnd_a(k,kd)%ar,jbndr1=jbnd_a(k,kd)%ar,
     >    nvrbs=VerbLev-1,iiunit=-iiunit,
     >    COSANG=cosag_o(m,k)%lar,SINANG=sinag_o(m,k)%lar)
        else ! do not iclude COSANG, SINANG
          call INTP_INI(nxa(kd),nya(kd),nxo_l,nyo_l,
     >    ALON_a(k,kd)%gar,ALAT_a(k,kd)%gar,ALON_o(m)%lar,ALAT_o(m)%lar,
     >    MASK_a(k,kd)%gar,MASK_o(m)%lar,
     >    Iag_o(m,k,kd)%lar,Jag_o(m,k,kd)%lar,ierr,
     >    ia=ia_a(k,kd)%ar,ja=ja_a(k,kd)%ar,
     >    ib=ib_a(k,kd)%ar,jb=jb_a(k,kd)%ar,
     >    ic=ic_a(k,kd)%ar,jc=jc_a(k,kd)%ar,
     >    KM1=km_a(k,kd),ibndr1=ibnd_a(k,kd)%ar,jbndr1=jbnd_a(k,kd)%ar,
     >    nvrbs=VerbLev-1,iiunit=-iiunit)
        end if
        call PRDT(1)
        CALL GLOB_ABORT(ierr,'C: INTP_INI for A '//dn_a(kd)//'->O '//
     >  gn_a(k)//'->'//gn_o(m)//' failed, ABORTED',1)
      end do
      end do
      call PRDT(1)
!O<->A
      END IF
!O<->A

! Initialization of interpolation for O<->A (above) and W<-A (below; or
! W<->A in the future) are to be made two calls of an internal
! interpolation initialization subroutine

!W<- A
      IF (Waves_id.gt.0) THEN
!W<- A
      if (ngw.gt.1 .or. nga.gt.1) then
        call CPL_ANNOUNCE('to initialize A '//dn_a(kd)//
     >  '->W interpolation, '//gn_a(0)//'->'//gn_w(0),1)
        call PRDT(1)
        allocate(Iag_w(0,0,kd)%lar(nxw_l,nyw_l),
     >           Jag_w(0,0,kd)%lar(nxw_l,nyw_l))
        if (nda.gt.1 .and. kd.eq.0)
     >    allocate(Iag_w(0,0,2)%lar(nxw_l,nyw_l),
     >             Jag_w(0,0,2)%lar(nxw_l,nyw_l))
C        A->W: H-p.->W (normally 0->0):
        call INTP_INI(nxa(kd),nya(kd),nxw_l,nyw_l,
     >  ALON_a(0,kd)%gar,ALAT_a(0,kd)%gar,ALON_w(0)%lar,ALAT_w(0)%lar,
     >  MASK_a(0,kd)%gar,MASK_w(0)%lar,
     >  Iag_w(0,0,kd)%lar,Jag_w(0,0,kd)%lar,ierr,
     >  ia=ia_a(0,kd)%ar,ja=ja_a(0,kd)%ar,
     >  ib=ib_a(0,kd)%ar,jb=jb_a(0,kd)%ar,
     >  ic=ic_a(0,kd)%ar,jc=jc_a(0,kd)%ar,
     >  KM1=km_a(0,kd),ibndr1=ibnd_a(0,kd)%ar,jbndr1=jbnd_a(0,kd)%ar,
     >  nvrbs=VerbLev-1,iiunit=-iiunit,same_grid=a2w_samegrid)
        call PRDT(1)
        CALL GLOB_ABORT(ierr,'C: INTP_INI for A '//dn_a(kd)//'->W '//
     >  gn_a(0)//'->'//gn_w(0)//' failed, ABORTED',1)
        if (VerbLev.ge.3) then
          allocate(Iag_w(0,0,0)%gar(nxw,nyw),Jag_w(0,0,0)%gar(nxw,nyw))
          call IGATHM(Iag_w(0,0,0),nxnyw_l,nxnyw_l1)
          call IGATHM(Jag_w(0,0,0),nxnyw_l,nxnyw_l1)
          if (kd.eq.0) then
            write (s1,'("FSG Iag_w, m=",i1,", k=",i1)') 0,0
            call IPRI2D(nxw,nyw,Iag_w(0,0,0)%gar,s1,5)
            write (s1,'("FSG Jag_w, m=",i1,", k=",i1)') 0,0
            call IPRI2D(nxw,nyw,Jag_w(0,0,0)%gar,s1,5)
          end if
        end if
      end if
!     below u-p., v-p. - WM gridpoints for velocity components;
!     currently same for both components, grid 0, but may
!     differ (in which case cosag, sinag stuff may need revision)
C        A->W: V-p.->W (as u-p.=v-p.=h-p. here, normally 1->0):
      do m=min(1,ngw-1),ngw-1
      do k=min(1,nga-1),nga-1
        call CPL_ANNOUNCE('to initialize A '//dn_a(kd)//
     >  '->W interpolation, '//gn_a(k)//'->'//gn_w(m),1)
        call PRDT(1)
        allocate(Iag_w(m,k,kd)%lar(nxw_l,nyw_l),
     >           Jag_w(m,k,kd)%lar(nxw_l,nyw_l))
        if (nda.gt.1 .and. kd.eq.0)
     >    allocate(Iag_w(m,k,2)%lar(nxw_l,nyw_l),
     >             Jag_w(m,k,2)%lar(nxw_l,nyw_l))
        if (kd.eq.L)
     >    allocate(cosag_w(m,k)%lar(nxw_l,nyw_l),
     >             sinag_w(m,k)%lar(nxw_l,nyw_l))

        if (kd.eq.L) then ! include COSANG, SINANG
          call INTP_INI(nxa(kd),nya(kd),nxw_l,nyw_l,
     >    ALON_a(k,kd)%gar,ALAT_a(k,kd)%gar,ALON_w(m)%lar,ALAT_w(m)%lar,
     >    MASK_a(k,kd)%gar,MASK_w(m)%lar,
     >    Iag_w(m,k,kd)%lar,Jag_w(m,k,kd)%lar,ierr,
     >    ia=ia_a(k,kd)%ar,ja=ja_a(k,kd)%ar,
     >    ib=ib_a(k,kd)%ar,jb=jb_a(k,kd)%ar,
     >    ic=ic_a(k,kd)%ar,jc=jc_a(k,kd)%ar,
     >    KM1=km_a(k,kd),ibndr1=ibnd_a(k,kd)%ar,jbndr1=jbnd_a(k,kd)%ar,
     >    nvrbs=VerbLev-1,iiunit=-iiunit,
     >    COSANG=cosag_w(m,k)%lar,SINANG=sinag_w(m,k)%lar,
     >    same_grid=a2w_samegrid)
        else ! do not iclude COSANG, SINANG
          call INTP_INI(nxa(kd),nya(kd),nxw_l,nyw_l,
     >    ALON_a(k,kd)%gar,ALAT_a(k,kd)%gar,ALON_w(m)%lar,ALAT_w(m)%lar,
     >    MASK_a(k,kd)%gar,MASK_w(m)%lar,
     >    Iag_w(m,k,kd)%lar,Jag_w(m,k,kd)%lar,ierr,
     >    ia=ia_a(k,kd)%ar,ja=ja_a(k,kd)%ar,
     >    ib=ib_a(k,kd)%ar,jb=jb_a(k,kd)%ar,
     >    ic=ic_a(k,kd)%ar,jc=jc_a(k,kd)%ar,
     >    KM1=km_a(k,kd),ibndr1=ibnd_a(k,kd)%ar,jbndr1=jbnd_a(k,kd)%ar,
     >    nvrbs=VerbLev-1,iiunit=-iiunit,same_grid=a2w_samegrid)
        end if
        call PRDT(1)
        CALL GLOB_ABORT(ierr,'C: INTP_INI for A '//dn_a(kd)//'->W '//
     >  gn_a(k)//'->'//gn_w(m)//' failed, ABORTED',1)
      end do
      end do
      call PRDT(1)
!W<- A
      END IF
!W<- A

!===
      END DO
!===

      call CPL_ANNOUNCE(
     >'initialization of interpolation routines completed',1)

C
CE3
C
C
      nunit_restart(Ocean_L)=10
      nunit_restart(Waves_L)=11

      IF (MASTER) THEN

      if (restart) then
        if (Ocean_id.gt.0) then
          OPEN(nunit_restart(Ocean_id),file='fluxes_for_OM',
     >    form='unformatted',status='old',iostat=ios)
          CALL GLOB_ABORT(ios,'C: no file fluxes_for_OM : terminated',1)
        end if
        if (Waves_id.gt.0) then
          OPEN(nunit_restart(Waves_id),file='fluxes_for_WM',
     >    form='unformatted',status='old',iostat=ios)
          CALL GLOB_ABORT(ios,'C: no file fluxes_for_WM : terminated',1)
        end if
      else
        if (Ocean_id.gt.0) then
          OPEN(nunit_restart(Ocean_id),file='fluxes_for_OM',
     >    form='unformatted',status='unknown',iostat=ios)
        end if
        if (Waves_id.gt.0) then
          OPEN(nunit_restart(Waves_id),file='fluxes_for_WM',
     >    form='unformatted',status='unknown',iostat=ios)
        end if
! debug inserion:->
        open(49,file='fluxes_from_AM',form='unformatted',
     >  status='unknown')
!<-:debug inserion
      end if

      END IF

C     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C     Time steps
C

      call PRDT(1)

Cccccccccccccccccccccccccccccccc       
      DO cstep=1,cstepmax
Cccccccccccccccccccccccccccccccc       

      if (cstep.gt.1 .and. prep) then
!zz        call flush_(iiunit)
        flush(iiunit)
        call GLOB_ABORT(1,
     >  ' prep mode: 1 c. time step completed; ABORTING WITH RC=0',0)
      end if

      call CPL_BARR  ! is it required?

      write(cs,'(i0)') cstep
      write(modeltime,'(f8.0," s =",f8.4," h =",f10.6," d")')
     >cstep*dt_c,cstep*dt_c/3600.,cstep*dt_c/86400.
      call CPL_ANNOUNCE('      ENTERING STEP '//trim(cs)//
     >', model time: '//trim(modeltime),2)

      itime_sec=cstep*dt_c

      if (cstep.gt.1) extrap_vrbs=0

CB4
C Receive SST from OM
!>oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

      IF (Ocean_id.gt.0) THEN
        CALL RECV_SST
      END IF

!<oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
      
CE4
C

C
CB5
C   Define OM open-sea/sea-ice/land mask basing on predefined
C   OM sea/land mask and OM SST
!       Nothing is done since there is no sea ice in OM.
CE5
C
      call GET_INIT_SF(Ocean_id,NSF_a2o,SF_a2o_o,nxnyo)
      call GET_INIT_SF(Waves_id,NSF_a2w,SF_a2w_w,nxnyw)
C
CB6
C   Interpolate OMG SST to AM, stationary AM grid
C   Update open-sea/sea-ice mask for AM by interpolation from OM
C   (currently nothing is done since there is no sea ice in OM)
C
      if (cstep.eq.1) call RECV_REF_AM_SST
      CALL INTERPOLATE_SST_A(kda=1)
CE6
C
CB7
C Send interpolated SST to AM, stationary AM grid
!>aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

      CALL SEND_SST_A(kda=1)

      CALL ANALYZE_SST_O

      CALL ANALYZE_SST_A(kda=1)

!<aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
C   Here, the updated open-sea/sea-ice mask for AM should also be
C   sent to AM 
!   [not done as there is no sea ice in OM]
CE7
C
CB10
C Send Surface Boundary Conditions (SBC, i.e. surface fluxes/stresses
C and possibly others) to OM (on ocean grid). Send WM SBC to WM
C
!>oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

      CALL SEND_SF(Ocean_id,NSF_a2o,ngo)

!<oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!>wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

      CALL SEND_SF(Waves_id,NSF_a2w,ngw)

!<wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
!>oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

      CALL ANALYZE_INTERPOLATED_SF(Ocean_id,NSF_a2o,ngo)

!<oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!>wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

      CALL ANALYZE_INTERPOLATED_SF(Waves_id,NSF_a2w,ngw)

!<wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
CE10
C

CB  Obtain location of AM domain 2 in AM domains 1 and 0;
C   Define AM FMG grid parameter arrays;
C   Calculate WM "ice mask" and send it to WM

!>aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      IF (nda.gt.1) THEN

        call CPL_ANNOUNCE('to receive istart1, jstart1 which define '//
     >  'lower left corner of AM '//dn_a(2),2)

        call CPL_INTEGER_RECV(ibuf,2,Atmos_id)

        istart1=ibuf(1)
        jstart1=ibuf(2)
        if ((jstart1/2)*2.eq.jstart1) call GLOB_ABORT(jstart1+1,
     >    'even values of jstart1 must be forbidden in WRF',1)
        istart0=agridrat*(istart1-1)+1
        jstart0=agridrat*(jstart1-1)+1
        iend0=istart0+nxa(2)-1
        jend0=jstart0+nya(2)-1
        iend1=(iend0-1)/agridrat+1
!            =(istart0+nxa(2)-2)/agridrat+1
!            =istart1+(nxa(2)-1)/agridrat
        jend1=(jend0-1)/agridrat+1

!>wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
        IF (Waves_id.gt.0) THEN
        do k=0,ngw-1
          if (Atmos_spec.ne.3) then
            call COMPUTE_ICE_w
          else
            call READ_ICE_w(ICE_w(k)%gar)
          end if
          call CPL_INTEGER_SEND(ICE_w(k)%gar,nxnyw,Waves_id)
          call  CPL_ANNOUNCE('ICE_w(k)%gar sent to WM',2)
        end do
        END IF
!<wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

        write(s,'(4i5)') istart1,jstart1,istart0,jstart0
        call CPL_ANNOUNCE('istart1,jstart1,istart0,jstart0: '//
     >  trim(s)//'; time step='//trim(cs),2)
        s1=' '
        do k=0,nga-1
!         ALON_a(k,2)%ar=ALON_a(k,0)%ar(istart0:iend0,jstart0:jend0)
!         ALAT_a(k,2)%ar=ALAT_a(k,0)%ar(istart0:iend0,jstart0:jend0)
!         MASK_a(k,2)%ar=MASK_a(k,0)%ar(istart0:iend0,jstart0:jend0)
C either the above 3 lines, with %ar replaced by %gar,
C or the 3 lines below must be uncommented. Originally (in 1-process
C Coupler), the above was used. If the below is used the allocations
C of the arrays in LHS can be disposed of; but pointer assignments
C for *_a(k,2)%lar will have to be done AFTER the pointer
C assignments below.
          ALON_a(k,2)%gar=>ALON_a(k,0)%gar(istart0:iend0,jstart0:jend0)
          ALAT_a(k,2)%gar=>ALAT_a(k,0)%gar(istart0:iend0,jstart0:jend0)
          MASK_a(k,2)%gar=>MASK_a(k,0)%gar(istart0:iend0,jstart0:jend0)

          MASK_a2(k)%gar=MASK_a(k,2)%gar

          call pointlar(ALON_a(k,2),Atmos_id,kd=2)
          call pointlar(ALAT_a(k,2),Atmos_id,kd=2)
          call ipointlar(MASK_a(k,2),Atmos_id,kd=2)
          call ipointlar(MASK_a2(k),Atmos_id,kd=2)

          call MAKE_BOUNDARY_MASK_LAND_a(nxa(2),nya(2),MASK_a2(k)%gar)
          if (VerbLev.ge.2) then
            cnt(k)=gcount(MASK_a(k,2)%lar.eq.0)
            cnt1(k)=gcount(MASK_a2(k)%lar.eq.0)
            fr(k)=cnt(k)/nxnya(2)
            fr1(k)=cnt1(k)/nxnya(2)
            s1=trim(s1)//' '//gn_a(k)
          end if
        end do

        if (VerbLev.ge.2) then
          write(s,'(1p,2e10.2," / ",1p,2e10.2)') fr,fr1
          call CPL_ANNOUNCE('Sea fractions for '//dn_a(2)//':'//
     >    trim(s1)//' before/after bound. adj.: '//trim(s),2)
        end if

        if (Ocean_id.gt.0) then
!         Iog_a(0,2,0)%ar=Iog_a(0,0,0)%ar(istart0:iend0,jstart0:jend0)
!         Jog_a(0,2,0)%ar=Jog_a(0,0,0)%ar(istart0:iend0,jstart0:jend0)
C either the above 2 lines, with %ar replaced by %gar,
C or the 2 lines below must be uncommented. Originally (in 1-process
C Coupler), the above was used. If the below is used the allocations
C of the arrays in LHS can be disposed of; but pointer assignments
C for *_a(0,2,0)%lar will have to be done AFTER the pointer
C assignments below.
          Iog_a(0,2,0)%gar=>
     >    Iog_a(0,0,0)%gar(istart0:iend0,jstart0:jend0)
          Jog_a(0,2,0)%gar=>
     >    Jog_a(0,0,0)%gar(istart0:iend0,jstart0:jend0)

          call ipointlar(Iog_a(0,2,0),Atmos_id,kd=2)
          call ipointlar(Jog_a(0,2,0),Atmos_id,kd=2)
        end if ! (Ocean_id.gt.0)

C        here is the place for corresponding assignments with
C        Iwg_a, Jwg_a (now absent since there is no interpolation
C        from WMG to AMG)

        if (Ocean_id.gt.0) then
          i=gcount(MASK_a2(0)%lar.eq.0 .and. Iog_a(0,2,0)%lar.gt.0)
          IF (MASTER) THEN
            z1=float(i)/nxnya(2)
            write(s,'(1p,e10.2)') z1
            call CPL_ANNOUNCE('OM-sea fraction for '//dn_a(2)//
     >      ' after bound. adj.: '//trim(s),2)
            if (i.gt.0 .eqv. MDout) then
              MDout=.not.MDout
              write (16,'("'//trim(modeltime)//', step '//trim(cs)//
     >        ': AM Moving Domain out of OM Domain =",L1)') MDout
            end if
            if ( (i.gt.0 .or. fr(0).lt.1.E-8) .eqv. MDout_sea) then
              MDout_sea=.not.MDout_sea
              write (16,'("'//trim(modeltime)//', step '//trim(cs)//
     >        ': AM MD not dry but out of OM D =",L1)') MDout_sea
            end if
          END IF
        end if ! (Ocean_id.gt.0)

CE

        if (cstep.eq.1) then

CB Receive AM FMG coordinate and mask arrays and check that they
C  are sections of the corresponding AM FSG ("big") arrays read
C  previously from file (this is a safety feature)

          call CPL_ANNOUNCE('(for double-checking only) '//
     >    'to receive AMG dimensions, '//dn_a(2),3)
          call CPL_INTEGER_RECV(ibuf,2,Atmos_id)
          write(s,'(2i6)') ibuf
          call CPL_ANNOUNCE('(for double-checking only) '//
     >    'AM '//dn_a(2)//' dimensions received: '//s,1)
          call GLOB_ABORT(abs(nxa(2)-ibuf(1))+abs(nya(2)-ibuf(2)),
     >    'FMG grid sizes received differ from those read from namelist'
     >    ,1)
          call CPL_ANNOUNCE('(for double-checking only) '//
     >    'to receive AM '//dn_a(2)//' arrays (NG_a*2 MPI calls)',3)
          do k=0,NG_a-1
            call CPL_R(buf_a%gar,nxnya(2),Atmos_id,
     >                 knd_lonlat(Atmos_id))
            z1=min(gfraction(abs(buf_a%lar-ALON_a(k,2)%lar).gt.5.E-3),
     >         gfraction(abs(buf_a%lar+360.-ALON_a(k,2)%lar).gt.5.E-3))
            call CPL_R(buf_a%gar,nxnya(2),Atmos_id,
     >                 knd_lonlat(Atmos_id))
            z2=gfraction(abs(buf_a%lar-ALAT_a(k,2)%lar).gt.5.E-3)
            write(s,'(1p2e11.3)') z1,z2
            call CPL_ANNOUNCE('Fractions of '//dn_a(2)//
     >      ' inconsistencies for '//gn_a(k)//' lon, lat: '//trim(s),2)
C           if (z1.ge.1.E-8 .or. z2.ge.1.E-8) call GLOB_ABORT(1,
C    >      dn_a(2)//' lon/lat double checking FAILED',1)
          end do
          call CPL_ANNOUNCE('(for double-checking only) '//
     >    'to receive AM '//dn_a(2)//' sea/land mask',3)
          do k=0,NG_a-1
            SLM_a(k,2)%gar=>SLM_a(k,0)%gar(istart0:iend0,jstart0:jend0)
            call pointlar(SLM_a(k,2),Atmos_id,kd=2)
            call CPL_R(buf_a%gar,nxnya(2),Atmos_id,knd_SLM(Atmos_id))
            z1=gfraction(abs(buf_a%lar-SLM_a(k,2)%lar).gt.5.E-3)
            write(s,'(1p2e11.3)') z1
            call CPL_ANNOUNCE('Fractions of '//dn_a(2)//
     >      ' inconsistencies for '//gn_a(k)//' SLM: '//trim(s),2)
            if (z1.ge.1.E-8) call GLOB_ABORT(1,
     >      dn_a(2)//' SLM double checking FAILED',1)
          end do
          call CPL_ANNOUNCE(dn_a(2)//' lon/lat/SLM doublechecking OK',1)

CE

        end if
C
CB6-2
C   Interpolate OMG SST to AM, fine moving AM grid
C   Update open-sea/sea-ice mask for AM by interpolation from OM
C   (currently nothing is done since there is no sea ice in OM)
C

        CALL INTERPOLATE_SST_A(kda=2)

CE6-2
C
CB7-2
C Send interpolated SST to AM, moving AM grid

        CALL SEND_SST_A(kda=2)

C   Here, the updated open-sea/sea-ice mask for AM should also be
C   sent to AM 
!   [not done as there is no sea ice in OM]
CE7-2

        CALL ANALYZE_SST_A(kda=2)

      END IF ! (nda.gt.1)
!<aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

C
CB8
C   
C    Receive SBC from AM
C [This may or may not include: (1) d(net surface heat flux)/d(SST)
C from AM (and d(E-P)/dSST ?); (2) AM SST from AM (for auxiliary
C technical purposes, if a different time level is used which is
C unavailable in Coupler)
C - currently neither is included]
!>aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

      DO kd=1,nda

      CALL RECV_SF(kda=kd)
      call ANALYZE_RECVD_SF(kda=kd)

      END DO

!   [In the (current) case of no SI in OM, the info on OS/SI mask is
!   supposed to have arrived in the flux array SF_a. Namely, its values
!   at SI AMGPs (i,j) are supposed to be .le. the too_low negative,
!   currently -1.E20 ]
!<aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
!
CE8
C
CB9
C   Interpolate SBC from AM to OM & WM grids
C [This may or may not include:
C interpolation of d(surface fluxes)/d(SST) and of AM SST;
C calculation of corrected fluxes on OMG
C - currently none is included]

      do kd=1,nda

        CALL INTERPOLATE_SF(kda=kd, id=Ocean_id, NSF=NSF_a2o, ng=ngo,
     >  buf1=buf1_o, buf2=buf2_o)

        CALL INTERPOLATE_SF(kda=kd, id=Waves_id, NSF=NSF_a2w, ng=ngw,
     >  buf1=buf1_w, buf2=buf2_w)

      end do

C   
CE9
C
C
! the following for no-SI OM version ("old" SST is needed to stably
! calculate substitute fluxes (nudging to TFREEZ) under AM SI)
C
!<- an MLCFS feature currently not enabled
C

      if ((itime_sec/(6*3600))*6*3600.eq.itime_sec) then
        call WRITE_RESTART(Ocean_id,NSF_a2o,SF_a2o_o)
        call WRITE_RESTART(Waves_id,NSF_a2w,SF_a2w_w)
      end if

Cccccccccccccccccccccccccccccccc       
      END DO ! cstep
Cccccccccccccccccccccccccccccccc       

      call PRDT(1)

      call WRITE_RESTART(Ocean_id,NSF_a2o,SF_a2o_o)
      call WRITE_RESTART(Waves_id,NSF_a2w,SF_a2w_w)

! debug inserion:->
!     write (49) (SF_a(n,1)%gar,n=1,NSF_a) ! for 1-process
!<-:debug inserion

!     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C

! Finalize MPI
      write(nprint,*)'C: to call MPI_FINALIZE'
      call MPI_FINALIZE(ierr)
      write(nprint,*)'C: back from MPI_FINALIZE,ierr=',ierr

!     ------------------------------------------------------------------
C
c     write(nprint,*)'C: to call system(...'
c     call system(<a command or shell script to be run in the end>)
C
      write(nprint,*)'C: done'
      STOP
C
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      CONTAINS
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!
!-----------------------------------------------------------------------
!
      SUBROUTINE pointlar(F,id,kd)

      implicit none

      TYPE (PARR2) F
      integer id
      integer,optional:: kd
C

      if (id.le.0) then
!       call GLOB_ABORT(id-1,'pointlar: id.le.0 (=printed val.+1)',1)
        RETURN
      end if

      if (id.eq.Atmos_id .neqv. PRESENT(kd))
     > call GLOB_ABORT(id,'pointlar: id.eq.Atmos_id.neqv.PRESENT(kd)',1)

      if (id.eq.Atmos_id) then
        if (kd.lt.0 .or. kd.gt.2) call GLOB_ABORT(kd,
     >  'pointlar: wrong kd value - must be 0 or 1 or 2',1)
        F%lar=>F%gar(ia_l_f(kd):ia_l_l(kd),ja_l_f(kd):ja_l_l(kd))
      else if (id.eq.Ocean_id) then
        F%lar=>F%gar(io_l_f:io_l_l,jo_l_f:jo_l_l)
      else if (id.eq.Waves_id) then
        F%lar=>F%gar(iw_l_f:iw_l_l,jw_l_f:jw_l_l)
      else
        call GLOB_ABORT(id,'pointlar: wrong id',1)
      end if

      F%lar_pointed=.true.

      RETURN
      END SUBROUTINE pointlar
!
!-----------------------------------------------------------------------
!
      SUBROUTINE ipointlar(F,id,kd)

      implicit none

      TYPE (IPARR2) F
      integer id
      integer,optional:: kd
C

      if (id.le.0) then
!       call GLOB_ABORT(id-1,'ipointlar: id.le.0 (=printed val.+1)',1)
        RETURN
      end if

      if (id.eq.Atmos_id .neqv. PRESENT(kd))
     >call GLOB_ABORT(id,'ipointlar:id.eq.Atmos_id.neqv.PRESENT(kd)',1)

      if (id.eq.Atmos_id) then
        if (kd.lt.0 .or. kd.gt.2) call GLOB_ABORT(kd,
     >  'ipointlar: wrong kd value - must be 0 or 1 or 2',1)
        F%lar=>F%gar(ia_l_f(kd):ia_l_l(kd),ja_l_f(kd):ja_l_l(kd))
      else if (id.eq.Ocean_id) then
        F%lar=>F%gar(io_l_f:io_l_l,jo_l_f:jo_l_l)
      else if (id.eq.Waves_id) then
        F%lar=>F%gar(iw_l_f:iw_l_l,jw_l_f:jw_l_l)
      else
        call GLOB_ABORT(id,'ipointlar: wrong id',1)
      end if

      F%lar_pointed=.true.

      RETURN
      END SUBROUTINE ipointlar
!
!-----------------------------------------------------------------------
!
      SUBROUTINE RECV_COOR(id,ng,nx,ny,nxny,ALON,ALAT,gn,RECV)

      implicit none

      integer id,ng,nx,ny,nxny
      TYPE (PARR2), dimension(0:ng-1):: ALON,ALAT
      character*1, dimension(0:ng-1):: gn
      logical,optional:: RECV
      character*400 :: message
      logical R
      integer ix,iy ! Sam
C

      IF (id.le.0) RETURN

      R=.true.
      if (PRESENT(RECV)) R=RECV

      if (id.ne.Ocean_id .and. id.ne.Waves_id .and. id.ne.Atmos_id) then
        call GLOB_ABORT(1,'id in RECV_COOR is neither Ocean_id nor '//
     >  'Waves_id nor Atmos_id, aborted',1)
      end if
      
      IF (R) THEN
        call CPL_ANNOUNCE('to receive '//cm(id)//' coor. arrays',3)
        do k=0,ng-1
          call CPL_R(ALON(k)%gar,nxny,id,knd_lonlat(id))
          call CPL_R(ALAT(k)%gar,nxny,id,knd_lonlat(id))
        end do
        call CPL_ANNOUNCE(cm(id)//' coor. arrays received',1)
      END IF

C          Convert rads to degs if required
      if (maxval(abs(ALON(0)%gar)).lt.7) then
        call CPL_ANNOUNCE(
     >  'to convert '//cm(id)//' coordinates from rads to degs',2)
        do k=0,ng-1
          ALON(k)%gar=ALON(k)%gar*dinr
          ALAT(k)%gar=ALAT(k)%gar*dinr
        end do
      end if

!     Begin Sam's new amendment ----------------------------------------

      if(ng>0 .and. ny>0 .and. nx>0) then
!        If no reference longitude was given, automatically determine
!        the reference longitude from the middle longitude of the first
!        component's grid 0.
         if(reflon<-8e8) then
            reflon=ALON(0)%gar(max(1,nx/2),max(1,ny/2))
 48         format('Reference longitude autoset to ',F0.7)
            write(message,48) reflon
            call CPL_ANNOUNCE(trim(message),1)
         endif
!        Force all longitudes to be within +/- 180 of the reference
!        longitude.
         do k=0,ng-1
            do iy=1,ny
               do ix=1,nx
                  ALON(k)%gar(ix,iy) = reflon-180 +                     &
     &                 mod(3600+180+ALON(k)%gar(ix,iy)-reflon,360.)
               enddo
            enddo
         enddo
      endif

!     End Sam's new amendment ------------------------------------------

      if (minval(ALON(0)%gar).lt.-10.) then
        call CPL_ANNOUNCE('to add 360. to '//cm(id)//' longitudes',1)
        do k=0,ng-1
          ALON(k)%gar=ALON(k)%gar+360.
        end do
      end if

      if (VerbLev.ge.2) then
      do k=0,ng-1
        call PRI2D(nx,ny,
     >  ALON(k)%gar,cm(id)//'G '//gn(k)//' longitudes:')
        call PRI2D(nx,ny,ALAT(k)%gar,cm(id)//'G '//gn(k)//' latitudes:')
      end do
      end if

      return
      END SUBROUTINE RECV_COOR
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GET_INIT_SF(id,NSF,SF,nxny)

      implicit none

      integer id,NSF,nxny
      TYPE (PARR2), dimension(NSF):: SF
C

      IF (cstep.gt.1) RETURN

      IF (id.le.0) RETURN

      IF (id.ne.Ocean_id .and. id.ne.Waves_id) call GLOB_ABORT(id,
     >'GET_INIT_SF: id must be Ocean_id or Waves_id',1)
!
      IF (MASTER) THEN
!
      print*,'C: GET_INIT_SF: to read unit ',nunit_restart(id),
     >' id=',id 
      call PRDT(1) 

      read (nunit_restart(id),iostat=ios) (SF(n)%gar,n=1,NSF)

      if (restart) then
        call GLOB_ABORT(ios,'C: FATAL: restart=T but init. '//cm(id)//
     >  ' SF could not be read',1)
        call CPL_ANNOUNCE(
     >  'This is a restart. Init. '//cm(id)//' SF read',0)
        if (id.eq.Ocean_id) then

          read (nunit_restart(id),iostat=ios1) SST_a_r%gar

          call GLOB_ABORT(ios1,
     >    'C: FATAL: restart=T but AM ref. SST could not be read',1)
          call CPL_ANNOUNCE(
     >    'This is a restart. Init. AM ref. SST read',0)
        else if (id.eq.Waves_id) then

          read (nunit_restart(id),iostat=ios1) (ICE_w(k)%gar,k=0,ngw-1)

          call GLOB_ABORT(ios1,
     >    'C: FATAL: restart=T but WM "ice" could not be read',1)
          call CPL_ANNOUNCE(
     >    'This is a restart. Init. WM "ice" read',0)
        end if
      else
        if (ios.eq.0) then
          call CPL_ANNOUNCE('WARNING: init. '//cm(id)//
     >    ' SF successfully read but restart=F. These fluxes will '//
     >   'not be used. The procedure will be cold start',0)
        else
          call CPL_ANNOUNCE('This is a cold start. Init. '//cm(id)//
     >    ' SF cannot be read',0)
        end if
        do n=1,NSF
          SF(n)%gar=0. ! this is the current version of init. SF
        end do
        if (id.eq.Waves_id) then ! placing "ice" initialization inside
                                 ! IF statement avoids its duplication
          do k=0,ngw-1
            ICE_w(k)%gar=1.  ! total "ice" is the current version of
          end do             ! init. "ice"
        end if
                        !
!         y1=2.*too_low !
!         SF_o=y1       !
                        ! this is the MLCFS version of init. fluxes
                        ! for cold start. It is supposed to tell OM
                        ! to use its own fluxes.
                        ! (Syntax is wrong in the context.)
      end if
!
      END IF
!
      if (id.eq.Ocean_id .and. restart) then
        call CPL_BC(SST_a_r%gar,nxnya(1))
      else if (id.eq.Waves_id) then
        do k=0,ngw-1
          call CPL_INTEGER_BC(ICE_w(k)%gar,nxnyw)
        end do
      end if

      do n=1,NSF
        call CPL_BC(SF(n)%gar,nxny)
      end do

      print*,'C: GET_INIT_SF: exiting, id=',id
      call PRDT(1)

      return
      END SUBROUTINE GET_INIT_SF
!
!-----------------------------------------------------------------------
!
      SUBROUTINE RECV_SST

      implicit none

      logical convert2K, first/.true./
      save convert2K, first
C

      write(s,'(2i10)') nxnyo,cstep
      call CPL_ANNOUNCE('to receive OMG SST from OM; nxnyo, cstep: '//
     >s,3)
      call CPL_R(SST_o%gar,nxnyo,Ocean_id,knd_SST(Ocean_id))
      write(s1,'(1p4e20.13)') SST_o%gar(1:2,1:2)
      call CPL_ANNOUNCE('OMG SST received from OM '//s1,2)
C
C Invert indexation if necessary
C
! - for general grids, this is nearly pointless

C
C Convert to Kelvin degrees if necessary
C

!first
      if (first) then
!first

      SST_o_i=SST_o%gar ! SST_o_i is used here just as a free array, and
                        ! it is for a debugging purpose
      i1=0
      i2=0
      do j=1,nyo
      do i=1,nxo
        if (Mask_o(0)%gar(i,j).eq.0) then ! i.e. if it is sea (not land)
          i1=i1+1
          if (SST_o%gar(i,j).lt.100.) then
            i2=i2+1
            SST_o%gar(i,j)=SST_o%gar(i,j)+TZERO
          end if
        end if
      end do
      end do
      if (i2.ne.i1 .and. i2.ne.0) then
        j1=0
        j2=0
        do j=1,nyo
        do i=1,nxo
        if (Mask_o(0)%gar(i,j).eq.0) then
          if (SST_o_i(i,j).ge.100.) then
            j1=j1+1
            if (j1.le.10)
     >        write(nprint,*) 'C: i,j,MASK_o,SST_o: ',
     >                        i,j,MASK_o(0)%gar(i,j),SST_o_i(i,j)
          else
            j2=j2+1
            if (j2.le.10)
     >        write(nprint,*) 'C: i,j,MASK_o,SST_o: ',
     >                        i,j,MASK_o(0)%gar(i,j),SST_o_i(i,j)
          end if
        end if
        end do
        end do
        CALL GLOB_ABORT(i2,'C: strange SST_o array, ABORTED',i1)
      end if

      convert2K=i2.gt.0
      first=VerbLev.ge.4

!first
      else if (convert2K) then
!first

      where (Mask_o(0)%gar.eq.0)
        SST_o%gar=SST_o%gar+TZERO
      end where

!first
      end if
!first

      RETURN
      END SUBROUTINE RECV_SST
!
!-----------------------------------------------------------------------
!
      SUBROUTINE ANALYZE_SST_O

      implicit none

      real cpl_sum
      integer cpl_isum
C

      if (VerbLev.ge.4) then
        z1=1.E20
        z2=-1.E20
        do j=1,nyo_l
        do i=1,nxo_l
          if (Mask_o(0)%lar(i,j).eq.0) then! ie if it is sea (not land)
            if (SST_o%lar(i,j).lt.z1) then
              z1=SST_o%lar(i,j)
              i1=i
              j1=j
            end if
            if (SST_o%lar(i,j).gt.z2) then
              z2=SST_o%lar(i,j)
              i2=i
              j2=j
            end if
          end if
        end do
        end do
        call cpl_min(z1,z1,i)
        call cpl_max(z2,z2,j)
        call cpl_integer2master(i1,i)
        call cpl_integer2master(j1,i)
        call cpl_integer2master(i2,j)
        call cpl_integer2master(j2,j)
        write (s1,'(i7,2(0p,2i4,2f7.2,1pe14.6))') cstep,
     >  i1,j1,ALON_o(0)%gar(i1,j1),ALAT_o(0)%gar(i1,j1),z1,
     >  i2,j2,ALON_o(0)%gar(i2,j2),ALAT_o(0)%gar(i2,j2),z2
        call CPL_ANNOUNCE('OMG SST min, max at step'//s1,2)
      end if

!---> Debugging insertion
      if (VerbLev.ge.3) then
        write(s2,'(1p4e20.13)') SST_o%gar(1:2,1:2)
        call CPL_ANNOUNCE(
     >  'OMG SST left upper corner:'//s2,2)
      end if
      if (VerbLev.ge.2) then
        y1=0.
        y2=0.
        i1=0
        do j=1,nyo_l
        do i=1,nxo_l
          if (MASK_o(0)%lar(i,j).eq.0) then
            i1=i1+1
            y1=y1+SST_o%lar(i,j)
          end if
        end do
        end do
        i1=cpl_isum(i1)
        y1=cpl_sum(y1)
        if (MASTER .and. i1.gt.0) y1=y1/i1
        call CPL_BC(y1,1)
        do j=1,nyo_l
        do i=1,nxo_l
          if (MASK_o(0)%lar(i,j).eq.0)
     >    y2=y2+abs(SST_o%lar(i,j)-y1)
        end do
        end do
        y2=cpl_sum(y2)
        if (MASTER .and. i1.gt.0) y2=y2/i1
        write(s1,'(1p2e17.10,i9)') y1,y2,i1
        call CPL_ANNOUNCE('SST_o: mean, mean abs. dev.; n.of sea GPs: '
     >  //s1,2)
      end if

      if (VerbLev.ge.2) then
        IF (cstep.le.npr1st .or. (cstep/nprper)*nprper.eq.cstep)
     >  call PRI2D(nxo,nyo,SST_o%gar,'SST_o')
      end if
!<--- Debugging insertion

      if (VerbLev.ge.2) then
        call MOMENTA(nxo_l,nyo_l,MASK_o(0)%lar.eq.0,SST_o%lar,
     >  fmean,fsigma,imin,jmin,fmin,nmin,imax,jmax,fmax,nmax)
        call PRMOMENTA('OM SST',Ocean_id,0)
        do kd=1,nda
          if (kd.eq.2 .and. cstep.eq.1) RETURN
          call MOMENTA(nxo_l,nyo_l,
     >    MASK_o(0)%lar.eq.0 .and. Iag_o(0,0,kd)%lar.gt.0,
     >    SST_o%lar,
     >    fmean,fsigma,imin,jmin,fmin,nmin,imax,jmax,fmax,nmax)
          call PRMOMENTA('OM SST in AM '//dn_a(kd)//' domain',
     >    Ocean_id,0)
        end do
      end if

      RETURN
      END SUBROUTINE ANALYZE_SST_O
!
!-----------------------------------------------------------------------
!
      SUBROUTINE RECV_REF_AM_SST

      implicit none
C

      write(s,'(2i10)') nxnya(1),cstep
      call CPL_ANNOUNCE('to receive AMG ref. SST from AM; nxnya(1), '//
     >'cstep: '//s,3)
      call CPL_R(bf_a,nxnya(1),Atmos_id,knd_SST(Atmos_id))
      if (.not.restart) SST_a_r%gar=bf_a
      write(s1,'(1p4e20.13)') SST_a_r%gar(1:2,1:2)
      call CPL_ANNOUNCE('SST_a_r '//s1,2)
      if (VerbLev.ge.2) then
        call MOMENTA(nxa_l(1),nya_l(1),MASK_a(0,1)%lar.eq.0,SST_a_r%lar,
     >  fmean,fsigma,imin,jmin,fmin,nmin,imax,jmax,fmax,nmax)
        call PRMOMENTA('AM ref. SST',Atmos_id,0,kd=1)
        if (Ocean_id.gt.0) then
          call MOMENTA(nxa_l(1),nya_l(1),
     >    MASK_a(0,1)%lar.eq.0 .and. Iog_a(0,1,0)%lar.gt.0,
     >    SST_a_r%lar,
     >    fmean,fsigma,imin,jmin,fmin,nmin,imax,jmax,fmax,nmax)
          call PRMOMENTA('AM ref. SST in OM domain',Atmos_id,0,kd=1)
        end if
      end if
      RETURN
      END SUBROUTINE RECV_REF_AM_SST
!
!-----------------------------------------------------------------------
!
      SUBROUTINE INTERPOLATE_SST_A(kda)

      implicit none

      integer,intent(in):: kda

      character*80 ss(2) /'SST inter/extrapolation from OMG to AM',
     >                    'SST interpolation from OMG to AM'/
C

      if (kda.gt.nda) RETURN

      if (Ocean_id.le.0) then
        SST_a(kda)%gar=too_low10
        RETURN
      end if

      call CPL_ANNOUNCE(
     >'to interpolate SST from OMG to AM '//dn_a(kda)//', cstep='//cs,3)
C    The following called routine must be compiled with forced
C    kind_REAL for REAL and kind_INTEGER for INTEGER (see module
C    CPL_COMM for the values)
C                             - currently, 8 and 4 respectively,
C    and if these change, adjustment will have to made to the subr.
      call INTERP(nxo,nyo,nxa_l(kda),nya_l(kda),
     >ALON_o(0)%gar,ALAT_o(0)%gar,ALON_a(0,kda)%lar,ALAT_a(0,kda)%lar,
     >MASK_o(0)%gar,MASK_a(0,kda)%lar,
     >Iog_a(0,kda,0)%lar,Jog_a(0,kda,0)%lar,
     >SST_o%gar,SST_a(kda)%lar,ierr)
      CALL GLOB_ABORT(ierr,
     >'C: ABORTED on abnormal return from INTERP',1)

      call GATHM(SST_a(kda),nxnya_l(kda),nxnya_l1(kda))

      IF (.not.MASTER) RETURN

      if (kda.eq.1) then
        call CPL_ANNOUNCE(
     >  'to expand SST from AM '//dn_a(1)//' to self'//', '//cs,3)
        CALL EXTRAP(nxa(1),nya(1),SST_a(1)%gar,MASK_a(0,1)%gar.eq.0,
     >  SST_a(1)%gar.gt.too_low,ierr,
     >  itmaxmax=max_extrap,
     >  itmax=itm,
     >  it=it,
     >  f_r=SST_a_r%gar,
!    >  lakeval=too_low10,
     >  ivrbs=extrap_vrbs,
     >  force=force_extrap_a(0)%ar)
        if (ierr.lt.0) then
          write(s,'(i0)') -ierr
          call CPL_ANNOUNCE('EXTRAP detected '//trim(s)//
     >    ' lake points on AMG, filled with ref. AMG SST values',2)
!    >    ' lake points on AMG, filled with lakeval=too_low10',2)
          ierr=0
        end if
        CALL GLOB_ABORT(ierr,'C: ABORTED upon failure to expand SST',1)
        write(s,'(i0)') itm
        call CPL_ANNOUNCE('EXTRAP finished for SST, itm='//
     >  trim(s)//'  t.s.='//cs,3)
      end if

      call CPL_ANNOUNCE(trim(ss(kda))//' '//dn_a(kda)//
     >' completed, cstep='//cs,2)

!*    IF (cstep.le.npr1st .or. (cstep/nprper)*nprper.eq.cstep .or.
!*   >Atmos_id.lt.0 .and. Ocean_id.eq.-2)
!*   >call PRI2D(nxa(kda),nya(kda),SST_a(kda)%gar,'AM '//dn_a(kda)//
!*   >' SST, cstep='//cs)
!*
!*    y1=0.
!*    y2=0.
!*    i1=0
!*    i2=0
!*    k=0
!*    do j=1,nya(kda)
!*    do i=1,nxa(kda)
!*      if (MASK_a(0,kda)%gar(i,j).eq.0) then
!*      k=k+1
!*      if (SST_a(kda)%gar(i,j).gt.too_low) then
!*        i1=i1+1
!*        y1=y1+SST_a(kda)%gar(i,j)
!*        if (i2.lt.10 .and. Ocean_id.eq.-2 .and. Atmos_id.lt.0 .and.
!*   >    abs(SST_a(kda)%gar(i,j)-TZERO).gt.0.01) then
!*          i2=i2+1
!*          print '("C: xxxx",4i6,1pe15.7)',
!*   >      cstep,i,j,MASK_a(0,kda)%gar(i,j),SST_a(kda)%gar(i,j)
!*        end if
!*      end if
!*      end if
!*    end do
!*    end do
!*    if (i1.ne.0) y1=y1/i1
!*    do j=1,nya(kda)
!*    do i=1,nxa(kda)
!*      if (MASK_a(0,kda)%gar(i,j).eq.0 .and.
!*   >      SST_a(kda)%gar(i,j).gt.too_low)
!*   >    y2=y2+abs(SST_a(kda)%gar(i,j)-y1)
!*    end do
!*    end do
!*    if (i1.ne.0) y2=y2/i1
!*    write(s2,'(1p2e20.12,0p,2i10)') y1,y2,i1,k
!*    call CPL_ANNOUNCE('AM '//dn_a(kda)//
!*   >' SST: mean, mean abs. dev.: '//s2,2)
!*<-this is moved, in a split version, to ANALYZE_SST_A
      RETURN
      END SUBROUTINE INTERPOLATE_SST_A
!
!-----------------------------------------------------------------------
!
      SUBROUTINE SEND_SST_A(kda)

      implicit none

      integer kda

      character*11 s
C

      if (kda.gt.nda) RETURN

      call CPL_S(SST_a(kda)%gar,nxnya(kda),Atmos_id,knd_SST(Atmos_id))
      write(s,'(" AM grid ",i1,1x)') kda
      call CPL_ANNOUNCE(
     >'back from CPL_S(SST_a...),'//s//cs,3)

!!    if (VerbLev.ge.2 .and. Ocean_id.gt.0) then
!!      if (kda.eq.1) call CPL_BC(SST_a(1)%gar,nxnya(1)) ! otherwise,
!            for processes other than master process,
!            values of SST_a(1)%lar in call MOMENTA below are
!            interpolated but not subsequently extrapolated SST values
!!      call MOMENTA(nxa_l(kda),nya_l(kda),
!!   >  MASK_a(0,kda)%lar.eq.0 .and. SST_a(kda)%lar.gt.too_low,
!!   >  SST_a(kda)%lar,
!!   >  fmean,fsigma,imin,jmin,fmin,nmin,imax,jmax,fmax,nmax)
!!      call PRMOMENTA('AM '//dn_a(kda)//' SST',Atmos_id,0,kd=kda)
!!      if (kda.eq.2 .and. MASTER) then
!!        c_key=.true.
!!        call MOMENTA(iend1-istart1+1,jend1-jstart1+1,
!!   >    MASK_a(0,1)%gar(istart1:iend1,jstart1:jend1).eq.0 .and.
!!   >    SST_a(1)%gar(istart1:iend1,jstart1:jend1).gt.too_low,
!!   >    SST_a(1)%gar(istart1:iend1,jstart1:jend1),
!!   >    fmean,fsigma,imin,jmin,fmin,nmin,imax,jmax,fmax,nmax)
!!        c_key=.false.
!!        call PRMOMENTA('AM '//dn_a(1)//' SST in '//dn_a(2)//' domain',
!!   >    Atmos_id,0,kd=1,is=istart1,ie=iend1,js=jstart1,je=jend1)
!!      end if
!!      call MOMENTA(nxa_l(kda),nya_l(kda),
!!   >  MASK_a(0,kda)%lar.eq.0 .and. SST_a(kda)%lar.gt.too_low
!!   >  .and. Iog_a(0,kda,0)%lar.gt.0,
!!   >  SST_a(kda)%lar,
!!   >  fmean,fsigma,imin,jmin,fmin,nmin,imax,jmax,fmax,nmax)
!!      call PRMOMENTA('AM '//dn_a(kda)//' SST in OM domain',
!!   >                 Atmos_id,0,kd=kda)
!!    end if
!!<-this is moved, in a split version, to ANALYZE_SST_A
      RETURN
      END SUBROUTINE SEND_SST_A
!
!-----------------------------------------------------------------------
!
      SUBROUTINE ANALYZE_SST_A(kda)

      implicit none

      integer kda

      character*11 s

      real cpl_sum
      integer cpl_isum
C

      if (kda.gt.nda) RETURN

      IF (VerbLev.lt.2) RETURN

      IF (cstep.le.npr1st .or. (cstep/nprper)*nprper.eq.cstep)
     >call PRI2D(nxa(kda),nya(kda),SST_a(kda)%gar,'AM '//dn_a(kda)//
     >' SST, cstep='//cs)

      if (kda.eq.1) call CPL_BC(SST_a(1)%gar,nxnya(1)) ! otherwise,
!            for processes other than master process,
!            values of SST_a(1)%lar in call MOMENTA below are
!            interpolated but not subsequently extrapolated SST values
      y1=0.
      y2=0.
      i1=0
      i2=0
      k=0
      do j=1,nya_l(kda)
      do i=1,nxa_l(kda)
        if (MASK_a(0,kda)%lar(i,j).eq.0) then
        k=k+1
        if (SST_a(kda)%lar(i,j).gt.too_low) then
          i1=i1+1
          y1=y1+SST_a(kda)%lar(i,j)
        end if
        end if
      end do
      end do

      y1=cpl_sum(y1)
      i1=cpl_isum(i1)
      if (MASTER .and. i1.ne.0) y1=y1/i1
      call CPL_BC(y1,1)
      k=cpl_isum(k)

      do j=1,nya_l(kda)
      do i=1,nxa_l(kda)
        if (MASK_a(0,kda)%lar(i,j).eq.0 .and.
     >      SST_a(kda)%lar(i,j).gt.too_low)
     >    y2=y2+abs(SST_a(kda)%lar(i,j)-y1)
      end do
      end do

      y2=cpl_sum(y2)
      if (MASTER .and. i1.ne.0) y2=y2/i1
      write(s2,'(1p2e20.12,0p,2i10)') y1,y2,i1,k
      call CPL_ANNOUNCE('AM '//dn_a(kda)//
     >' SST: mean, mean abs. dev.: '//s2,2)

      if (Ocean_id.lt.0) RETURN

!     if (VerbLev.ge.2 .and. Ocean_id.gt.0) then
        call MOMENTA(nxa_l(kda),nya_l(kda),
     >  MASK_a(0,kda)%lar.eq.0 .and. SST_a(kda)%lar.gt.too_low,
     >  SST_a(kda)%lar,
     >  fmean,fsigma,imin,jmin,fmin,nmin,imax,jmax,fmax,nmax)
        call PRMOMENTA('AM '//dn_a(kda)//' SST',Atmos_id,0,kd=kda)
        if (kda.eq.2 .and. MASTER) then
          c_key=.true.
          call MOMENTA(iend1-istart1+1,jend1-jstart1+1,
     >    MASK_a(0,1)%gar(istart1:iend1,jstart1:jend1).eq.0 .and.
     >    SST_a(1)%gar(istart1:iend1,jstart1:jend1).gt.too_low,
     >    SST_a(1)%gar(istart1:iend1,jstart1:jend1),
     >    fmean,fsigma,imin,jmin,fmin,nmin,imax,jmax,fmax,nmax)
          c_key=.false.
          call PRMOMENTA('AM '//dn_a(1)//' SST in '//dn_a(2)//' domain',
     >    Atmos_id,0,kd=1,is=istart1,ie=iend1,js=jstart1,je=jend1)
        end if
        call MOMENTA(nxa_l(kda),nya_l(kda),
     >  MASK_a(0,kda)%lar.eq.0 .and. SST_a(kda)%lar.gt.too_low
     >  .and. Iog_a(0,kda,0)%lar.gt.0,
     >  SST_a(kda)%lar,
     >  fmean,fsigma,imin,jmin,fmin,nmin,imax,jmax,fmax,nmax)
        call PRMOMENTA('AM '//dn_a(kda)//' SST in OM domain',
     >                 Atmos_id,0,kd=kda)
!     end if

      RETURN
      END SUBROUTINE ANALYZE_SST_A
!
!-----------------------------------------------------------------------
!
      SUBROUTINE RECV_SF(kda)

      implicit none

      integer kda
C

      if (kda.gt.nda) RETURN

      do n=1,NSF_a
        call CPL_ANNOUNCE('to receive '//sfn_a(n)//' from AM '//
     >  dn_a(kda)//', '//cs,3)
        call CPL_R(SF_a(n,kda)%gar,nxnya(kda),Atmos_id,
     >  knd_sfcflux(Atmos_id))
      end do

      RETURN
      END SUBROUTINE RECV_SF
!
!-----------------------------------------------------------------------
!
      SUBROUTINE ANALYZE_RECVD_SF(kda)

      implicit none

      integer kda

      character*11 s
      integer,dimension(2):: id
      TYPE (IPARR2), dimension(2):: Ig_a
C

      if (VerbLev.lt.2) RETURN

      id(1)=Ocean_id
      Ig_a(1)=Iog_a(0,kda,0)
      id(2)=Waves_id
!     id(2)=0
!     Ig_a(2)=Iwg_a(0,kda,0)  !<- for future: so far no interpolation
                              ! from WMG to AMG and so no Iwg_a

      do n=1,NSF_a
        k1=min(nga-1,1)   ! AM u-points
        k2=nga-1          ! AM v-points
        if (n.le.NSSF_a) then
          k=0
        else if (MOD(n-NSSF_a,2).eq.1) then
          k=k1
        else
          k=k2
        end if
        call MOMENTA(nxa_l(kda),nya_l(kda),
     >  MASK_a(k,kda)%lar.eq.0, SF_a(n,kda)%lar,
     >  fmean,fsigma,imin,jmin,fmin,nmin,imax,jmax,fmax,nmax)
        call PRMOMENTA('AM '//dn_a(kda)//' '//sfn_a(n),
     >                 Atmos_id,k,kd=kda,nunit=17)
        if (kda.eq.2 .and. MASTER) then
          c_key=.true.
          call MOMENTA(iend1-istart1+1,jend1-jstart1+1,
     >    MASK_a(k,1)%gar(istart1:iend1,jstart1:jend1).eq.0,
     >    SF_a(n,1)%gar(istart1:iend1,jstart1:jend1),
     >    fmean,fsigma,imin,jmin,fmin,nmin,imax,jmax,fmax,nmax)
          c_key=.false.
          call PRMOMENTA('AM '//dn_a(1)//' '//sfn_a(n)//
     >                   ' in '//dn_a(2)//' domain',
     >    Atmos_id,k,kd=1,is=istart1,ie=iend1,js=jstart1,je=jend1,
     >                   nunit=17)
        end if
        do i=1,2
          if (i.eq.2) CYCLE                ! this is because there is
                   ! no Ig_a(2)=Iwg_a(0,kda,0) currently, since there
                   ! is no interpolation from WM to AM and it would be
                   ! inexpedient to compute Iwg_a just for the sake of
                   ! this analysis. To be corrected as soon as Iwg_a
                   ! is available
          if (id(i).le.0) CYCLE
          call MOMENTA(nxa_l(kda),nya_l(kda),
     >    MASK_a(k,kda)%lar.eq.0 .and. Ig_a(i)%lar.gt.0,
     >    SF_a(n,kda)%lar,
     >    fmean,fsigma,imin,jmin,fmin,nmin,imax,jmax,fmax,nmax)
          call PRMOMENTA('AM '//dn_a(kda)//' '//sfn_a(n)//' in '//
     >                   cm(id(i))//' domain',
     >                   Atmos_id,k,kd=kda)
        end do
      end do
      do i=1,2
        if (id(i).le.0) CYCLE
        i2=min(NSSF_a+2*i,NSF_a)
        i1=i2-1
        write (s,'(i0)') kda
        call MOMENTA(nxa_l(kda),nya_l(kda),
     >  MASK_a(k1,kda)%lar.eq.0 .and. MASK_a(k2,kda)%lar.eq.0,
     >  sqrt(SF_a(i1,kda)%lar**2+SF_a(i2,kda)%lar**2),
     >  fmean,fsigma,imin,jmin,fmin,nmin,imax,jmax,fmax,nmax)
        call PRMOMENTA('AM '//dn_a(kda)//vm_a(id(i)),Atmos_id,0,kd=kda,
     >  LON=0.5*(ALON_a(k1,kda)%gar+ALON_a(k2,kda)%gar),
     >  LAT=0.5*(ALAT_a(k1,kda)%gar+ALAT_a(k2,kda)%gar),nunit=17)
        if (kda.eq.2) then
          if (i.eq.2) CYCLE
          IF (MASTER) THEN
          c_key=.true.
          call MOMENTA(iend1-istart1+1,jend1-jstart1+1,
     >    MASK_a(k1,1)%gar(istart1:iend1,jstart1:jend1).eq.0 .and.
     >    MASK_a(k2,1)%gar(istart1:iend1,jstart1:jend1).eq.0,
     >    sqrt(SF_a(i1,1)%gar(istart1:iend1,jstart1:jend1)**2+
     >    SF_a(i2,1)%gar(istart1:iend1,jstart1:jend1)**2),
     >    fmean,fsigma,imin,jmin,fmin,nmin,imax,jmax,fmax,nmax)
          c_key=.false.
          call PRMOMENTA('AM '//dn_a(1)//vm_a(id(i))//
     >         ' in '//dn_a(2)//' domain',            Atmos_id,0,kd=1,
     >         LON=0.5*(ALON_a(k1,1)%gar(istart1:iend1,jstart1:jend1)+
     >                  ALON_a(k2,1)%gar(istart1:iend1,jstart1:jend1)),
     >         LAT=0.5*(ALAT_a(k1,1)%gar(istart1:iend1,jstart1:jend1)+
     >                  ALAT_a(k2,1)%gar(istart1:iend1,jstart1:jend1)),
     >                  nunit=17)
          END IF ! (MASTER)
          call MOMENTA(nxa_l(kda),nya_l(kda),
     >    MASK_a(k1,kda)%lar.eq.0 .and. MASK_a(k2,kda)%lar.eq.0
     >    .and. Ig_a(i)%lar.gt.0,
     >    sqrt(SF_a(i1,kda)%lar**2+SF_a(i2,kda)%lar**2),
     >    fmean,fsigma,imin,jmin,fmin,nmin,imax,jmax,fmax,nmax)
          call PRMOMENTA('AM '//dn_a(kda)//vm_a(id(i))//
     >         ' in '//cm(id(i))//' domain', Atmos_id,0,kd=kda,
     >         LON=0.5*(ALON_a(k1,kda)%gar+ALON_a(k2,kda)%gar),
     >         LAT=0.5*(ALAT_a(k1,kda)%gar+ALAT_a(k2,kda)%gar))
        end if
      end do

      call CPL_ANNOUNCE('ANALYZE_RECVD_SF: exiting, kda='//trim(s)//
     >', cstep='//cs,3)

      RETURN
      END SUBROUTINE ANALYZE_RECVD_SF
!
!-----------------------------------------------------------------------
!
      SUBROUTINE INTERPOLATE_SF(kda,id,NSF,ng,buf1,buf2)

      implicit none

      integer kda,id,NSF,ng
      real(kind=kind_REAL),dimension(:,:):: buf1,buf2

      integer nx,ny,nx_l,ny_l,nxny_l1
      real(kind=kind_REAL),dimension(NSF):: lakeval
      character*4,dimension(NSF):: sfn
      character*1,dimension(0:ng-1):: gn
      TYPE (PARR2), dimension(NSF,nda):: SFa
      TYPE (PARR2), dimension(NSF):: SF
      TYPE (PARR2), dimension(0:ng-1):: ALON,ALAT
      TYPE (IPARR2), dimension(0:ng-1):: MASK
      TYPE (IPARR2), dimension(0:ng-1,0:nga-1,L:nda):: Iag,Jag
      TYPE (PARR2), dimension(0:ng-1,0:nga-1):: cosag,sinag
      TYPE (IARRAY2), dimension(0:ng-1):: force_extrap

      logical nospval(2) /.false.,.true./
      integer kda0/0/,cstep0/0/,id0/0/
      character*100 s,s1
      SAVE
C

c     if (id.le.0) RETURN
c
      if (id.eq.Ocean_L) then
        nx=nxo
        ny=nyo
        nx_l=nxo_l
        ny_l=nyo_l
        nxny_l1=nxnyo_l1
        lakeval=lakeval_o
        sfn=sfn_a2o
        gn=gn_o
        SFa=SF_a2o_a
        SF=SF_a2o_o
        ALON=ALON_o
        ALAT=ALAT_o
        MASK=MASK_o
        Iag=Iag_o
        Jag=Jag_o
        cosag=cosag_o
        sinag=sinag_o
        force_extrap=force_extrap_o
      else if (id.eq.Waves_L) then
        nx=nxw
        ny=nyw
        nx_l=nxw_l
        ny_l=nyw_l
        nxny_l1=nxnyw_l1
        lakeval=lakeval_w
        sfn=sfn_a2w
        gn=gn_w
        SFa=SF_a2w_a
        SF=SF_a2w_w
        ALON=ALON_w
        ALAT=ALAT_w
        MASK=MASK_w
        Iag=Iag_w
        Jag=Jag_w
        cosag=cosag_w
        sinag=sinag_w
        force_extrap=force_extrap_w
      else
        RETURN
      end if

      if (VerbLev.ge.2) then
        write (s,'(i0)') kda
        write (s1,'(i0)') id
        call CPL_ANNOUNCE('INTERPOLATE_SF: entering, kda='//
     >  trim(s)//', id='//trim(s1)//', cstep='//cs,3)
      end if

      if (kda.gt.nda) RETURN

      if (kda.le.kda0 .and. cstep.eq.cstep0
     >    .and. id.eq.id0)  ! just for bug control
     >call GLOB_ABORT(1,'wrong order of calls of INTERPOLATE_SF',1)
      cstep0=cstep
      kda0=kda
      id0=id

      if (kda.eq.1) then
        do n=1,NSF
          SF(n)%lar=too_low10 ! should not be necessary, just to
                              ! mark those values which are not assigned
                              ! in INTERP, i.e. land gridpoint values
        end do
      end if

      n1=NSF-1   ! x component of AM vector field
      n2=NSF     ! y component of AM vector field

      k1=min(nga-1,1)   ! AM u-points (WRF: = V-points)
      k2=nga-1          ! AM v-points (WRF: = V-points)

      k0=-1
      m10=-1
      m20=-1

C   Interpolate SBC from AM to *M grid

      do n=1,NSF

C        n<n1=NSF-1: interpolate from AM T-points to *M T-points
C                 (everything except stresses)
C        n=n1=NSF-1: interpolate AM u-stress from AM u-points
C                    (which is V-points in WRF)
C        n=n2=NSF:   interpolate AM v-stress from AM v-points
C                    (which is V-points in WRF)

        if (n.lt.n1) then
          k=0               ! AM T-points
          m1=0              ! *M T-points
          m2=0              ! *M T-points
        else
          m1=min(ng-1,1)   ! *M u-points
          m2=ng-1          ! *M v-points
          if (n.eq.n1) then
            k=k1            ! AM u-points (WRF: = V-points)
          else if (n.eq.n2) then
            k=k2            ! AM v-points (WRF: = V-points)
          else
            call GLOB_ABORT(1,'this is preposterous',1)
          end if
        end if

        if (kda.eq.2) then   !  moving grid
          if (k.ne.k0 .or. m1.ne.m10 .or. m2.ne.m20) then

            do m=m1,m2
              do j=1,ny_l
              do i=1,nx_l
                if (Iag(m,k,0)%lar(i,j).gt.0) then
                  i0=Iag(m,k,0)%lar(i,j)-istart0+1
                  j0=Jag(m,k,0)%lar(i,j)-jstart0+1
                  if ( i0.lt.1 .or. i0.gt.nxa(2) .or.
     >                 j0.lt.1 .or. j0.gt.nya(2) ) then
                    Iag(m,k,2)%lar(i,j)=-2       ! see INTP_INI
                  else
                    ia=ia_a(k,2)%ar(i0,j0)
                    ja=ja_a(k,2)%ar(i0,j0)
                    ib=ib_a(k,2)%ar(i0,j0)
                    jb=jb_a(k,2)%ar(i0,j0)
                    ic=ic_a(k,2)%ar(i0,j0)
                    jc=jc_a(k,2)%ar(i0,j0)
                    if ( ia.lt.1 .or. ia.gt.nxa(2) .or.
     >                   ja.lt.1 .or. ja.gt.nya(2) .or.
     >                   ib.lt.1 .or. ib.gt.nxa(2) .or.
     >                   jb.lt.1 .or. jb.gt.nya(2) .or.
     >                   ic.lt.1 .or. ic.gt.nxa(2) .or.
     >                   jc.lt.1 .or. jc.gt.nya(2) ) then
                      Iag(m,k,2)%lar(i,j)=-2       ! see INTP_INI
                    else
                      Iag(m,k,2)%lar(i,j)=i0
                      Jag(m,k,2)%lar(i,j)=j0
                    end if
                  end if
                else
                  Iag(m,k,2)%lar(i,j)=Iag(m,k,0)%lar(i,j)
                ! thus *MG points that are land or surrounded by AMG
                ! land points (Iag(m,k,0)%lar(i,j) = 0 or -1) will not
                ! receive -2 value even if they lie outside AM domain 2.
                ! This is contrary to INTP_INI policies but should not
                ! affect results, since interpolation to such points is
                ! blocked anyway. Actually, the outer IF may be removed;
                ! then, all non-interpolatable *MG points will receive
                ! -2 value regardless of whether or not they lie inside
                ! AM domain 2.
                end if
              end do
              end do

              if (VerbLev.ge.3) then
              if (cstep.eq.1) then
                allocate(Iag(m,k,2)%gar(nx,ny),Jag(m,k,2)%gar(nx,ny))
                call IGATHM(Iag(m,k,2),nx_l*ny_l,nxny_l1)
                write (s1,'("FMG Iag for '//cm(id)//
     >          ', m=",i1,", k=",i1)') m,k
                call IPRI2D(nx,ny,Iag(m,k,2)%gar,s1,4)
                call IGATHM(Jag(m,k,2),nx_l*ny_l,nxny_l1)
                write (s1,'("FMG Jag for '//cm(id)//
     >          ', m=",i1,", k=",i1)') m,k
                call IPRI2D(nx,ny,Jag(m,k,2)%gar,s1,4)
              end if
              end if

            end do

          end if
        end if

        do m=m1,m2

          call CPL_ANNOUNCE('to interpolate '//sfn(n)//' from AM '//
     >    dn_a(kda)//' '//gn_a(k)//'-points to '//cm(id)//' '//
     >    gn(m)//'-points, '//cs,3)

          if (kda.le.1) then
            MASK_a_fluxes=>MASK_a(k,kda)%gar
          else
            MASK_a_fluxes=>MASK_a2(k)%gar
            if (m.lt.m2) then
              if (n.eq.n1) then
                SF(n)%lar=buf1
              else if (n.eq.n2) then
                SF(n)%lar=buf2
              else
                call GLOB_ABORT(1,'this is not supposed to occur',1)
              end if
            end if
          end if

          CALL INTERP(nxa(kda),nya(kda),nx_l,ny_l,
     >    ALON_a(k,kda)%gar,ALAT_a(k,kda)%gar,ALON(m)%lar,ALAT(m)%lar,
     >    MASK_a_fluxes,                    MASK(m)%lar,
     >    Iag(m,k,kda)%lar,Jag(m,k,kda)%lar,
     >    SFa(n,kda)%gar,
     >    SF(n)%lar,
     >    ierr,
     >    ia=ia_a(k,kda)%ar,ja=ja_a(k,kda)%ar,
     >    ib=ib_a(k,kda)%ar,jb=jb_a(k,kda)%ar,
     >    ic=ic_a(k,kda)%ar,jc=jc_a(k,kda)%ar,
     >    NOSPVAL=nospval(kda),
     >    same_grid=a2w_samegrid.and.id.eq.Waves_id)

          CALL GLOB_ABORT(ierr,'C: ABORTED upon failure to interpolate '
     >    //sfn(n)//' from AM '//dn_a(kda)//' '//gn_a(k)//
     >    '-points to '//cm(id)//' '//gn(m)//'-points, '//cs,1)

          write(s,'(1p4e20.12)') SF(n)%lar(1:2,1:2)
          call CPL_ANNOUNCE('done interp. '//sfn(n)//' from AM '//
     >    dn_a(kda)//' '//gn_a(k)//'-points to '//cm(id)//' '//
     >    gn(m)//'-points, '//trim(cs)//' '//trim(s),3)

c         if (m.lt.m2) then
          if (m.eq.m1) then
            if (n.eq.n1) then
              buf1=SF(n)%lar
            else if (n.eq.n2) then
              buf2=SF(n)%lar
c           else
c             call GLOB_ABORT(1,'THIS IS NOT SUPPOSED TO OCCUR',1)
            end if
          end if

        end do

        k0=k
        m10=m1
        m20=m2

      end do

      if (kda.lt.nda) RETURN  ! i.e. only do rotation and extrapolation
                              ! once after interpolation from AM
                              ! domains 1...nda

      write (s1,'(6i3)') n1,n2,m1,m2,k1,k2
      call CPL_ANNOUNCE('to rotate vector field on '//cm(id)//
     >'G, n1,n2,m1,m2,k1,k2: '//trim(s1),3)

! rotation: obtain y vector component at *M u-points
      where (SF(n1)%lar.gt.too_low .and. SF(n2)%lar.gt.too_low)
        SF(n2)%lar=SF(n2)%lar*cosag(m2,k2)%lar+
     >              SF(n1)%lar*sinag(m2,k1)%lar
      elsewhere
        SF(n2)%lar=too_low10
      end where
! rotation: obtain x vector component at *M u-points
      where (buf1.gt.too_low .and. buf2.gt.too_low)
        SF(n1)%lar=buf1*cosag(m1,k1)%lar-
     >              buf2*sinag(m1,k2)%lar
      elsewhere
        SF(n1)%lar=too_low10
      end where

      write(s,'(1p4e10.2,3x,1p4e10.2)')
     >SF(n1)%lar(1:2,1:2),SF(n2)%lar(1:2,1:2)
      call CPL_ANNOUNCE('done rotating vector field on '//cm(id)//
     >'G '//trim(s),3)

      do n=1,NSF
        call GATHM(SF(n),nx_l*ny_l,nxny_l1)
      end do

      if (.not.extrap_SF) RETURN

! HYCOM+GFSfluxes adjustment:-> 
      if (Ocean_spec.eq.2 .and. id.eq.Ocean_id) then
        call CPL_ANNOUNCE('interpolation from AMG to '//cm(id)//'G (no '
     >  //'subsequent extrapolation for HYCOM) done, '//cs,2)
        call CPL_ANNOUNCE('INTERPOLATE_FLUXES: exiting, kda='//trim(s)//
     >  ', cstep='//cs,3)

        RETURN  ! HYCOM (Ocean_spec=2) prefers no extrapolation
                ! (using GFS fluxes instead)
      end if
!<-:HYCOM+GFSfluxes adjustment

! Uncomment RETURN below to suppress extrapolation:->
C       Options: (1) suppress forever, (2) extrapolate tile
C       (to itself), (3) assemble and extrapolate global array
C       in MASTER process. Uncommented RETURN <-> option (1), RETURN
C       commented out <-> option (3)
!     RETURN
! <-: Uncomment RETURN above to suppress extrapolation

C   Extrapolate SBC from *M grid to itself

      IF (.not. MASTER) RETURN

      do n=1,NSF

        if (n.lt.n1) then
          m=0                 ! *M T-points
        else if (n.eq.n1) then
          m=min(ng-1,1)      ! *M u-points
        else if (n.eq.n2) then
          m=ng-1             ! *M v-points
        else
          call GLOB_ABORT(1,'this is preposterous',1)
        end if

        call CPL_ANNOUNCE(
     >  'to expand '//sfn(n)//' from '//cm(id)//'G to itself, '//cs,3)

        CALL EXTRAP(nx,ny,SF(n)%gar,MASK(m)%gar.eq.0,
     >  SF(n)%gar.gt.too_low,ierr,
     >  itmax=itm,
     >  lakeval=lakeval(n),
     >  ivrbs=extrap_vrbs,
     >  force=force_extrap(m)%ar)

        if (ierr.lt.0) then
          write(s,'(i0)') -ierr
          write(s1,'(1pe12.4)') lakeval(n)
          call CPL_ANNOUNCE('EXTRAP detected '//trim(s)//
     >    ' lake points on '//cm(id)//'G, filled with lake value '//
     >    trim(s1),2)
          ierr=0
        end if

        CALL GLOB_ABORT(ierr,
     >  'C: ABORTED upon failure to expand '//sfn(n),1)

        write(s,'(i0)') itm
        call CPL_ANNOUNCE('EXTRAP finished for '//sfn(n)//', itm='//
     >  trim(s)//'  t.s.='//cs,3)

      end do

      call CPL_ANNOUNCE('interpolation from AMG to '//cm(id)//'G (plus '
     >//'subsequent extrapolation) done, '//cs,2)

C   Calculate corrected fluxes on OMG (including dealing with those
C        under AM SI in the current case of no SI in OM);
C   make surface stress 0 in the current case of no SI in OM
! - This part is currently excluded. See MLCFS Coupler for contents

      call CPL_ANNOUNCE('INTERPOLATE_FLUXES: exiting, kda='//trim(s)//
     >', cstep='//cs,3)

      RETURN
      END SUBROUTINE INTERPOLATE_SF
!
!-----------------------------------------------------------------------
!
      SUBROUTINE SEND_SF(id,NSF,ng)

C For each surface flux,
C  *  if required, fill it with invalid values
C                            to force OM to use its own data
C  *  invert indexation if necessary <- not required with general grids
C  *  change sign / convert units if necessary,
C  *  send it.

      implicit none

      integer id,NSF,ng

      integer nx,ny,nx_l,ny_l
      character*4,dimension(NSF):: sfn
      TYPE (PARR2), dimension(NSF):: SF
      TYPE (PARR2), dimension(0:ng-1):: ALON,ALAT
      TYPE (IPARR2), dimension(0:ng-1):: MASK
      TYPE (IPARR2), dimension(0:ng-1,0:nga-1,L:nda):: Iag
      TYPE (PARR2):: buf

      integer nxny
C

      if (id.eq.Ocean_L) then
        nx=nxo
        ny=nyo
        nx_l=nxo_l
        ny_l=nyo_l
        sfn=sfn_a2o
        SF=SF_a2o_o
        ALON=ALON_o
        ALAT=ALAT_o
        MASK=MASK_o
        Iag=Iag_o
      else if (id.eq.Waves_L) then
        nx=nxw
        ny=nyw
        nx_l=nxw_l
        ny_l=nyw_l
        sfn=sfn_a2w
        SF=SF_a2w_w
        ALON=ALON_w
        ALAT=ALAT_w
        MASK=MASK_w
        Iag=Iag_w
      else
        RETURN
      end if

      nxny=nx*ny

C Change sign / convert units if necessary
C
      do n=1,NSF
        if (SHT_SF(n,id)) then
          if (id.eq.Ocean_L) then
            buf=buf_o
          else if (id.eq.Waves_L) then
            buf=buf_w
          else
            call GLOB_ABORT(id-Ocean_L,'SEND_SF: error',1)
          end if
        else
          buf=SF(n)
   ! unit conversions, if required, must be inserted here. [This
   ! also pertains to change of sign]
           ! [ E.g. in MLCFS heat fluxes changed sign and were converted
           ! from W/m**2 to cal/(cm**2*s) by
           ! call CONVF(Q_o,-HF_CONV_A2O,too_low);
           ! E-P was converted to sal. flux units]
        end if
        call CPL_S(buf%gar,nxny,id,knd_sfcflux(id))
        write(s,'(1p4e20.12)') buf%gar(1:2,1:2)
        call CPL_ANNOUNCE(sfn(n)//' sent to '//cm(id)//' '//cs//s,2)
      end do

      return
      END SUBROUTINE SEND_SF
!
!-----------------------------------------------------------------------
!
      SUBROUTINE ANALYZE_INTERPOLATED_SF(id,NSF,ng)

      implicit none

      integer id,NSF,ng

      integer nx,ny,nx_l,ny_l
      character*4,dimension(NSF):: sfn
      TYPE (PARR2), dimension(NSF):: SF
      TYPE (PARR2), dimension(0:ng-1):: ALON,ALAT
      TYPE (IPARR2), dimension(0:ng-1):: MASK
      TYPE (IPARR2), dimension(0:ng-1,0:nga-1,L:nda):: Iag

      integer nxny
C

      if (VerbLev.lt.2) RETURN

      if (id.eq.Ocean_L) then
        nx=nxo
        ny=nyo
        nx_l=nxo_l
        ny_l=nyo_l
        sfn=sfn_a2o
        SF=SF_a2o_o
        ALON=ALON_o
        ALAT=ALAT_o
        MASK=MASK_o
        Iag=Iag_o
      else if (id.eq.Waves_L) then
        nx=nxw
        ny=nyw
        nx_l=nxw_l
        ny_l=nyw_l
        sfn=sfn_a2w
        SF=SF_a2w_w
        ALON=ALON_w
        ALAT=ALAT_w
        MASK=MASK_w
        Iag=Iag_w
      else
        RETURN
      end if

      nxny=nx*ny

      do n=1,NSF
        if (extrap_SF) call CPL_BC(SF(n)%gar,nxny)
!            for processes other than master process,
!            values of buf%lar in call MOMENTA below are
!            interpolated but not subsequently extrapolated SF values
        k1=min(nga-1,1)   ! AM u-points
        k2=nga-1          ! AM v-points
        m1=min(ng-1,1)    !  M u-points
        m2=ng-1           !  M v-points
        if (n.le.NSF-2) then
          k=0
          m=0
          k0=0
        else if (n.eq.NSF-1) then
          k=k1
          m=m1
          k0=k2
        else
          k=k2
          m=m2
          k0=k1
        end if
        call MOMENTA(nx_l,ny_l,MASK(m)%lar.eq.0,SF(n)%lar,fmean,fsigma,
     >  imin,jmin,fmin,nmin,imax,jmax,fmax,nmax)
        call PRMOMENTA(cm(id)//' '//sfn(n), id,m)
        do kd=1,nda
          call MOMENTA(nx_l,ny_l,
     >    MASK(m)%lar.eq.0 .and.
     >    Iag(m,k,kd)%lar.gt.0 .and. Iag(m,k0,kd)%lar.gt.0,
     >    SF(n)%lar,
     >    fmean,fsigma,imin,jmin,fmin,nmin,imax,jmax,fmax,nmax)
          call PRMOMENTA(cm(id)//' '//sfn(n)//' in AM '//dn_a(kd)//
     >    ' domain', id,m)
          if (cstep.eq.1) exit
        end do
      end do

      call MOMENTA(nx_l,ny_l,
     >MASK(m1)%lar.eq.0 .and. MASK(m2)%lar.eq.0,
     >sqrt(SF(NSF-1)%lar**2+SF(NSF)%lar**2),fmean,fsigma,
     >imin,jmin,fmin,nmin,imax,jmax,fmax,nmax)
      call PRMOMENTA(cm(id)//' |vector field|',     id,0,
     >               LON=0.5*(ALON(m1)%gar+ALON(m2)%gar),
     >               LAT=0.5*(ALAT(m1)%gar+ALAT(m2)%gar))

      do kd=1,nda
        call MOMENTA(nx_l,ny_l,
     >  MASK(m1)%lar.eq.0 .and. MASK(m2)%lar.eq.0 .and.
     >  Iag(m1,k1,kd)%lar.gt.0 .and. Iag(m2,k2,kd)%lar.gt.0 .and.
     >  Iag(m2,k1,kd)%lar.gt.0 .and. Iag(m1,k2,kd)%lar.gt.0,
     >  sqrt(SF(NSF-1)%lar**2+SF(NSF)%lar**2),
     >  fmean,fsigma,imin,jmin,fmin,nmin,imax,jmax,fmax,nmax)
        call PRMOMENTA(
     >  cm(id)//' |vector field| in AM '//dn_a(kd)//' domain', id,0,
     >                 LON=0.5*(ALON(m1)%gar+ALON(m2)%gar),
     >                 LAT=0.5*(ALAT(m1)%gar+ALAT(m2)%gar))
        if (cstep.eq.1) exit
      end do

      return
      END SUBROUTINE ANALYZE_INTERPOLATED_SF
!
!-----------------------------------------------------------------------
!
      SUBROUTINE COMPUTE_ICE_w

      implicit none

      integer i0,j0,i1,j1,i2,j2,i3,j3,i4,j4,k
      real x0,y0,x1,y1,x2,y2,x3,y3,x4,y4,r2
C
      if (cstep.eq.1) RETURN

      i0=(istart1+iend1)/2
      j0=(jstart1+jend1)/2
      x0=ALON_a(0,1)%gar(i0,j0)
      y0=ALAT_a(0,1)%gar(i0,j0)
      x1=ALON_a(0,1)%gar(istart1,jstart1)
      y1=ALAT_a(0,1)%gar(istart1,jstart1)
      x2=ALON_a(0,1)%gar(iend1,jstart1)
      y2=ALAT_a(0,1)%gar(iend1,jstart1)
      x3=ALON_a(0,1)%gar(iend1,jend1)
      y3=ALAT_a(0,1)%gar(iend1,jend1)
      x4=ALON_a(0,1)%gar(istart1,jend1)
      y4=ALAT_a(0,1)%gar(istart1,jend1)
      r2=MAX((x1-x0)**2+(y1-y0)**2,(x2-x0)**2+(y2-y0)**2,
     >       (x3-x0)**2+(y3-y0)**2,(x4-x0)**2+(y4-y0)**2)
      r2=(sqrt(r2)+RING_Iw)**2

!     do k=0,ngw-1
!       where ((ALON_w(k)%gar-x0)**2+(ALAT_w(k)%gar-y0)**2.gt.r2)
!         ICE_w(k)%gar=1
!       elsewhere
!         ICE_w(k)%gar=0
!       end where
!     end do

C      the follwing is the split version. Either the previous loop, or
C      the following loop must be uncommented
      do k=0,ngw-1
        where ((ALON_w(k)%lar-x0)**2+(ALAT_w(k)%lar-y0)**2.gt.r2)
          ICE_w(k)%lar=1
        elsewhere
          ICE_w(k)%lar=0
        end where
        call IGATHM(ICE_w(k),nxnyw_l,nxnyw_l1)
      end do

      return
      END SUBROUTINE COMPUTE_ICE_w
!
!-----------------------------------------------------------------------
!
      SUBROUTINE READ_ICE_w(ice)
C        for work with dummy AM with Atmos_spec=3
      implicit none
      integer,dimension(nxw,nyw):: ice

      real*4,dimension(nxw,nyw):: rice
      logical first/.true./
      integer ios,icetime(2)
      save first,ios
C
      IF (.not.MASTER) RETURN

      if (first) then
        open(9752,file='writeout_ice',form='unformatted',
     >  status='old',iostat=ios)
        first=.false.
      end if
      if (ios.ne.0) then
        ice=0
        call CPL_ANNOUNCE(
     >  'READ_ICE_w: file cannot be opened, ice array filled with 0s',2)
        RETURN
      end if
      read(9752) icetime,rice
      ice=rice
      print*,'C: READ_ICE_w: cstep=',cstep,' icetime=',icetime
      RETURN
      END SUBROUTINE READ_ICE_w
!
!-----------------------------------------------------------------------
!
      SUBROUTINE CONVF(F,C,too_low_to_convert)

      implicit none

      real F(nxo,nyo),C,too_low_to_convert

      integer i,j
C
      do j=1,nyo
      do i=1,nxo
        if (F(i,j).gt.too_low_to_convert) F(i,j)=C*F(i,j)
      end do
      end do

      RETURN
      END SUBROUTINE CONVF
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GRID_a_is_o

      implicit none

      integer i,j,k
C

      do k=0,nga-1
        do j=1,nya(1)
          do i=1,nxa(1)
            ia_a(k,1)%ar(i,j)=i+1
            ja_a(k,1)%ar(i,j)=j
            ib_a(k,1)%ar(i,j)=i+1
            jb_a(k,1)%ar(i,j)=j+1
            ic_a(k,1)%ar(i,j)=i
            jc_a(k,1)%ar(i,j)=j+1
          end do
        end do
      end do

      km_a=0

      RETURN
      END SUBROUTINE GRID_a_is_o
!
!-----------------------------------------------------------------------
!
      SUBROUTINE PRDT(VL)
      integer VL
C
      IF (VerbLev.lt.VL) RETURN
      IF (.not.MASTER) RETURN
      call DATE_AND_TIME(date,time)
      call CPL_ANNOUNCE('date, time: '//date//'  '//time,VL)

      return
      END SUBROUTINE PRDT
!
!-----------------------------------------------------------------------
!
      SUBROUTINE PRMOMENTA(s,id,kg,kd,is,ie,js,je,LON,LAT,nunit)

      implicit none

      character*(*) s
      integer id,kg
      integer,optional:: kd,is,ie,js,je,nunit
      real(kind=kind_REAL),dimension(:,:),target,optional:: LAT,LON

      real(kind=kind_REAL),dimension(:,:),pointer:: X,Y
      character*100 s1
C

      IF (.not.MASTER) RETURN

      if (id.le.0) then
!       call GLOB_ABORT(id-1,'PRMOMENTA: id.le.0 (=printed val.+1)',1)
        RETURN
      end if

      if (id.eq.Atmos_id .neqv. PRESENT(kd))
     >call GLOB_ABORT(id,'PRMOMENTA: id.eq.Atmos_id.neqv.PRESENT(kd)',1)

      if ((PRESENT(is).neqv.PRESENT(ie)).or.
     >    (PRESENT(is).neqv.PRESENT(js)).or.
     >    (PRESENT(is).neqv.PRESENT(je)))
     >call GLOB_ABORT(1,'PRMOMENTA: is,ie,js,je partly PRESENT',1)

      if (PRESENT(LON).neqv.PRESENT(LAT))
     >call GLOB_ABORT(id,'PRMOMENTA: PRESENT(LON).neqv.PRESENT(LAT)',1)

      if (PRESENT(nunit)) then
        write (nunit) cstep,real(fmean,4),real(fsigma,4),
     >                      real(fmin,4),real(fmax,4)
      end if

      if (fmin.gt.fmax+0.1) then
        call CPL_ANNOUNCE(s//': no values',1)
        return
      end if

      if (id.eq.Atmos_id) then
        if (kd.lt.0 .or. kd.gt.2) call GLOB_ABORT(kd,
     >  'PRMOMENTA: wrong kd value - must be 0 or 1 or 2',1)
        if (nmin.gt.0)
     >    jmin=ja_l_l(kd)+(nmin-1)*(nxnya_l1(kd)/nxa(kd))+jmin
        if (nmax.gt.0)
     >    jmax=ja_l_l(kd)+(nmax-1)*(nxnya_l1(kd)/nxa(kd))+jmax
        if (PRESENT(is)) then
          if (PRESENT(LON))
     >      call GLOB_ABORT(1,'PRMOMENTA: both is, LON PRESENT',1)
          X=>ALON_a(kg,kd)%gar(is:ie,js:je)
          Y=>ALAT_a(kg,kd)%gar(is:ie,js:je)
        else if (PRESENT(LON)) then  ! kg, kd are not used
                                     ! (though must be present)
          X=>LON
          Y=>LAT
        else
          X=>ALON_a(kg,kd)%gar
          Y=>ALAT_a(kg,kd)%gar
        end if
      else if (id.eq.Ocean_id) then
        if (nmin.gt.0) jmin=jo_l_l+(nmin-1)*(nxnyo_l1/nxo)+jmin
        if (nmax.gt.0) jmax=jo_l_l+(nmax-1)*(nxnyo_l1/nxo)+jmax
        if (PRESENT(LON)) then ! kg is not used (though must be present)
          X=>LON
          Y=>LAT
        else
          X=>ALON_o(kg)%gar
          Y=>ALAT_o(kg)%gar
        end if
      else if (id.eq.Waves_id) then
        if (nmin.gt.0) jmin=jw_l_l+(nmin-1)*(nxnyw_l1/nxw)+jmin
        if (nmin.gt.0) jmax=jw_l_l+(nmax-1)*(nxnyw_l1/nxw)+jmax
        if (PRESENT(LON)) then ! kg is not used (though must be present)
          X=>LON
          Y=>LAT
        else
          X=>ALON_w(kg)%gar
          Y=>ALAT_w(kg)%gar
        end if
      else
        call GLOB_ABORT(id,'PRMOMENTA: wrong id',1)
      end if

      write(s1,'(1p,2e16.8,0p,2i6,1p,e16.8,0p,2i6,1p,e16.8)')
     >fmean,fsigma,imin,jmin,fmin,imax,jmax,fmax
      call CPL_ANNOUNCE(s//
     >' mean,sigma,imin,jmin,min,imax,jmax,max: '//trim(s1),1)
      write(s1,'(1p,2e13.5,4x,2e13.5)')
     >X(imin,jmin),Y(imin,jmin),X(imax,jmax),Y(imax,jmax)
      call CPL_ANNOUNCE(
     >' lon,lat of min; lon,lat of max for '//s//': '//trim(s1),1)

      return
      END SUBROUTINE PRMOMENTA
!
!-----------------------------------------------------------------------
!
      integer function count(m)

      implicit none

      logical,dimension(:,:):: m

      integer i,j,k
C

      k=0.

      do j=LBOUND(m,2),UBOUND(m,2)
      do i=LBOUND(m,1),UBOUND(m,1)
        if (m(i,j)) k=k+1.
      end do
      end do

      count=k

      return
      END function count
!
!-----------------------------------------------------------------------
!
      integer function gcount(m)

      implicit none

      logical,dimension(:,:):: m

      integer i,j

      integer cpl_isum
C

      i=count(m)
      j=0
      j=cpl_isum(i)

      gcount=j

      return
      END function gcount
!
!-----------------------------------------------------------------------
!
      real function fraction(m)

      implicit none

      logical,dimension(:,:):: m

      real f
C

      f=count(m)

      fraction=f/size(m)

      return
      END function fraction
!
!-----------------------------------------------------------------------
!
      real function gfraction(m)

      implicit none

      logical,dimension(:,:):: m

      integer i,j
      real f

      integer cpl_isum
C

      f=gcount(m)

      i=size(m)
      j=cpl_isum(i)

      IF (MASTER) THEN
        gfraction=f/j
      ELSE
        gfraction=0.
      END IF

      return
      END function gfraction
!
!-----------------------------------------------------------------------
!
      subroutine print_offenders(n,offense,a1,a2)

      implicit none

      integer n
      logical,dimension(:,:):: offense
      real,dimension(:,:):: a1
      real,dimension(:,:),optional:: a2

      integer i,j,k
      real f
C

      k=0

      do j=LBOUND(offense,2),UBOUND(offense,2)
      do i=LBOUND(offense,1),UBOUND(offense,1)
        if (offense(i,j)) then
          k=k+1
          if (k.gt.n) return
          if (PRESENT(a2)) then
            print '("C: print_offenders, i,j,a1,a2: ",2i5,1p2e15.7)',
     >      i,j,a1(i,j),a2(i,j)
          else
            print '("C: print_offenders, i,j,a1: ",2i5,1pe15.7)',
     >      i,j,a1(i,j)
          end if
        end if
      end do
      end do

      return
      END subroutine print_offenders
!
!-----------------------------------------------------------------------
!
      subroutine WRITE_RESTART(id,NSF,SF)

      implicit none

      integer id,NSF
      TYPE (PARR2), dimension(NSF):: SF

      character*20 s
C

      if (id.le.0) RETURN
      
      if (.not. MASTER) RETURN

      s=cm(id)

      rewind nunit_restart(id)
      write (nunit_restart(id)) (SF(n)%gar,n=1,NSF)

      if (id.eq.Ocean_id) then
        write (nunit_restart(id)) SST_a_r%gar
        s=s//' & AM ref. SST'
      else if (id.eq.Waves_id) then
        write (nunit_restart(id)) (ICE_w(k)%gar,k=0,ngw-1)
        s=s//' & "ice"'
      end if

      call CPL_ANNOUNCE('at model time = '//trim(modeltime)//
     >'  SBC for '//trim(s)//' written to restart file',0)

      return
      END subroutine WRITE_RESTART
!
!-----------------------------------------------------------------------
!
      SUBROUTINE SET_LOCDIM(nx,ny,nx_l,ny_l,nxny_l,nxny_l1,
     >i_l_f,i_l_l,j_l_f,j_l_l,ij_l_f,ij_l_l)

!       Modify this subr if Coupler_master_rank_local in not 0

      implicit none

      integer,intent(in):: nx,ny
      integer,intent(out):: nx_l,ny_l,nxny_l,nxny_l1,
     >i_l_f,i_l_l,j_l_f,j_l_l,ij_l_f,ij_l_l

      integer ny_l1,ny_l0
C

      call GLOB_ABORT(Coupler_master_rank_local,
     >'SET_LOCDIM: Coupler_master_rank_local must be 0 but is not',1)

      nx_l=nx
      ny_l1=ny/Coupler_nprocs
      if (ny_l1*Coupler_nprocs.ne.ny) ny_l1=ny_l1+1
      ny_l0=ny-(Coupler_nprocs-1)*ny_l1
      if (MASTER) then
        ny_l=ny_l0
      else
        ny_l=ny_l1
      end if
      nxny_l=nx_l*ny_l
      nxny_l1=nx_l*ny_l1

      i_l_f=1
      i_l_l=nx
      j_l_l=ny_l0+process_rank_local*ny_l1
      j_l_f=j_l_l-ny_l+1
      ij_l_l=nx_l*j_l_l
      ij_l_f=ij_l_l-nxny_l+1

      print*,'SET_LOCDIM ',process_rank_local,
     >': nx,ny,nx_l,ny_l: ',nx,ny,nx_l,ny_l
        
      return
      END subroutine SET_LOCDIM
!
!-----------------------------------------------------------------------
!
      SUBROUTINE GATHM(F,N,N1)

      implicit none

      TYPE (PARR2) F
      integer N,N1
C

      call CPL_GATHM(F%lar,F%gar,N,N1,.not.F%lar_pointed)

      return
      END subroutine GATHM
!
!-----------------------------------------------------------------------
!
      SUBROUTINE IGATHM(F,N,N1)

      implicit none

      TYPE (IPARR2) F
      integer N,N1
C

      call CPL_INTEGER_GATHM(F%lar,F%gar,N,N1,.not.F%lar_pointed)

      return
      END subroutine IGATHM
!
!-----------------------------------------------------------------------
!
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      END
!=======================================================================

!
!***********************************************************************
!
      SUBROUTINE PRI2D(IM,JM,F,s)
      USE CPL_COMM, ONLY: nprint,MASTER

      implicit none

      integer IM,JM
      REAL F(IM,JM)
      character*(*) s

      integer n,i,j
      character*9 sn
      character*6 fr
      character*2 sc
c     integer nprint /6/
C

      IF (.not. MASTER) RETURN

      write(nprint,*)' '
      n=10

      if (s(1:3).eq.'SST') then
        fr='f12.7)'
        sc=' '
      else if (s(2:3).eq.'MG') then
        fr='f7.2)'
        sc=' '
        n=20
      else
        fr='e12.4)'
        sc='1p'
      end if
      
      IF (IM.eq.1) THEN
        write(nprint,'("C: '//trim(s)//', j from")')
        do j=1,JM,n
          write(sn,'(i0)') min(j+n-1,JM)-j+1
          write(nprint,'("C:",i4," to",i4,'//sc//trim(sn)//fr)
     >    j,min(j+n-1,JM),F(1,j:min(j+n-1,JM))
        end do
      ELSE IF (JM.eq.1) THEN
        write(nprint,'("C: '//trim(s)//', i from")')
        do i=1,IM,n
          write(sn,'(i0)') min(i+n-1,IM)-i+1
          write(nprint,'("C:",i4," to",i4,'//sc//trim(sn)//fr)
     >    i,min(i+n-1,IM),F(i:min(i+n-1,IM),1)
        end do
      ELSE
        do i=1,IM,n
          write(nprint,'("C: '//trim(s)//', i from",i4," to",i4)')
     >    i,min(i+n-1,IM)
          write(sn,'(i0)') min(i+n-1,IM)-i+1
          do j=1,JM
            write (nprint,'("C:",2i4,'//sc//trim(sn)//fr)
     >      i,j,F(i:min(i+n-1,IM),j)
          end do
        end do
      END IF

      return
      END
!
!***********************************************************************
!
      SUBROUTINE IPRI2D(IM,JM,F,s,L)
      USE CPL_COMM, ONLY: nprint,MASTER

      implicit none

      integer IM,JM,L
      INTEGER F(IM,JM)
      character*(*) s

      integer n,i,j
      character*9 sn,sl
C

      IF (.not. MASTER) RETURN

      write(sl,'(i0)') L
      write(nprint,*)' '
      n=120/L
      IF (IM.eq.1) THEN
        write(nprint,'("C: '//trim(s)//', j from")')
        do j=1,JM,n
          write(sn,'(i0)') min(j+n-1,JM)-j+1
          write(nprint,
     >    '("C:",i4," to",i4,2x,'//trim(sn)//'i'//trim(sl)//')') 
     >    j,min(j+n-1,JM),F(1,j:min(j+n-1,JM))
        end do
      ELSE IF (JM.eq.1) THEN
        write(nprint,'("C: '//trim(s)//', i from")')
        do i=1,IM,n
          write(sn,'(i0)') min(i+n-1,IM)-i+1
          write(nprint,
     >    '("C:",i4," to",i4,2x,'//trim(sn)//'i'//trim(sl)//')') 
     >    i,min(i+n-1,IM),F(i:min(i+n-1,IM),1)
        end do
      ELSE
        do i=1,IM,n
          write(nprint,'("C: '//trim(s)//', i from",i4," to",i4)')
     >    i,min(i+n-1,IM)
          write(sn,'(i0)') min(i+n-1,IM)-i+1
          do j=1,JM
            write(nprint,'("C:",2i4,2x,'//trim(sn)//'i'//trim(sl)//')') 
     >      i,j,F(i:min(i+n-1,IM),j)
          end do
        end do
      END IF

      return
      END
!
!***********************************************************************
!
      SUBROUTINE MOMENTA(IM,JM,mask,F,Fmean,Fsigma,
     >imin,jmin,Fmin,nmin,imax,jmax,Fmax,nmax)

      USE CPL_COMM               ! differs from 1-proc. version

      implicit none

      integer IM,JM,imin,jmin,imax,jmax,nmin,nmax
      logical mask(IM,JM)
      real(kind=kind_REAL) F(IM,JM),Fmean,Fsigma,Fmin,Fmax

      integer i,j
      real(kind=kind_REAL) z0,z,z2

      real cpl_sum
      integer cpl_isum
C
      z0=0.
      z=0.
      z2=0.
      Fmin=1.E30
      Fmax=-1.E30

      do j=1,JM
      do i=1,IM
      if (mask(i,j)) then
        z0=z0+1.
        z=z+F(i,j)
        z2=z2+F(i,j)**2
        if (F(i,j).lt.Fmin) then
          imin=i
          jmin=j
          Fmin=F(i,j)
        end if
        if (F(i,j).gt.Fmax) then
          imax=i
          jmax=j
          Fmax=F(i,j)
        end if
      end if
      end do
      end do

      if (c_key) then
        nmin=0
        nmax=0
      else                 ! if there is more than 1 process,
                           ! c_key here must be .true. if and only if
        z0=cpl_sum(z0)     ! this routine is not called by all the
        z=cpl_sum(z)       ! processes (then it must be called by the
        z2=cpl_sum(z2)     ! root aka master process only)
        call cpl_min(Fmin,Fmin,nmin)
        call cpl_max(Fmax,Fmax,nmax)
        call cpl_integer2master(imin,nmin)
        call cpl_integer2master(jmin,nmin)
        call cpl_integer2master(imax,nmax)
        call cpl_integer2master(jmax,nmax)
      end if

      if (.not.MASTER) RETURN

      if (z0.gt.0.) then
        Fmean=z/z0
        Fsigma=sqrt(max(0.,z2/z0-Fmean**2))
      else
        Fmean=0.
        Fsigma=0.
      end if

      return
      END
