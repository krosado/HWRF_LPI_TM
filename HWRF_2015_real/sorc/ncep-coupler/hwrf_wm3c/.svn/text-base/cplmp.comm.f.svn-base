      MODULE CPL_COMM
C
      implicit none
        
! MPI variables
      include 'mpif.h'

C
      integer Coupler_id /0/, Coupler_master_rank_global,
     >process_rank_global,nprocs,Coupler_nprocs,COMM_Coupler,
     >process_rank_local

      integer,parameter:: kind_REAL=8,kind_INTEGER=4,
     >kind_alt_REAL=12-kind_REAL
      integer,parameter:: Coupler_master_rank_local=0,
     >                    tagloc=613
      integer MPI_kind_REAL,MPI_kind_alt_REAL
C       kind_INTEGER must be number of bytes equal to number of bytes
C     implied by MPI_INTEGER MPI constant; all integers sent/received
C     are of this kind. No value other than 4 is anticipated as of now

      integer,parameter:: max_component_id=3

      integer ierr,status(MPI_STATUS_SIZE)

      integer
     >component_master_rank(max_component_id),
     >component_FlexLev(max_component_id)

      integer,allocatable:: ids(:)

      logical MASTER,  ! whether this is the local root process
     >C_BC,            ! wheteher to broadcast on receipt
     >c_key/.false./   ! for unspecified miscellaneous use

CB Controls:
      integer VerbLev /3/
c     integer VerbLev /2/
      integer nprint /6/   ! assign 7 for printout files to be without
                           ! date/time suffices (see program Coupler)
      integer mask_tolerance_level /2/
      logical SNDSHT(max_component_id)       ! for debugging only. To
      data SNDSHT /max_component_id*.false./ ! send actual data, values
                                             ! must be .false.
CE

      save

      END MODULE CPL_COMM
C
C***********************************************************************
C
      SUBROUTINE CPL_INIT
 
      USE CPL_COMM
 
      implicit none

      integer color,key,i,tag
      integer,parameter:: ibuffer_size=10
      integer 
c test removal     > (kind=kind_INTEGER)
     >ibuffer(ibuffer_size)
      character*10 s
C

C        Determine MPI send/receive types according to prescribed
C        types for arrays to be communicated

      if (kind_REAL.eq.8) then
        MPI_kind_REAL=MPI_REAL8
        MPI_kind_alt_REAL=MPI_REAL4
      else if (kind_REAL.eq.4) then
        MPI_kind_REAL=MPI_REAL4
        MPI_kind_alt_REAL=MPI_REAL8
      else
        write(s,'(i0)') kind_REAL
        call GLOB_ABORT(1,
     >  'CPL_INIT: illegal value of kind_REAL='//s,1)
      end if
      if (MPI_INTEGER.ne.MPI_INTEGER4) then
        write(s,'(2i5)') MPI_INTEGER,MPI_INTEGER4
        call CPL_ANNOUNCE('MPI_INTEGER and MPI_INTEGER4 differ: '//s//
     >  ' (hope it is OK)',0)
c       call GLOB_ABORT(1,'MPI_INTEGER and MPI_INTEGER4 differ',1)
      end if
      if (kind_INTEGER.ne.4) then
        write(s,'(i0)') kind_INTEGER
        call GLOB_ABORT(1,
     >  'CPL_INIT: illegal value of kind_INTEGER='//s,1)
      end if

C     1. The following is to match calls of MPI_COMM_SPLIT by the
C     other processes (in CMP_INIT).
C
      call CPL_ANNOUNCE('CPL_INIT: entered',3)

      color=Coupler_id
      key=1
      write(s,'(i5)') color
      call CPL_ANNOUNCE('CPL_INIT: to call MPI_COMM_SPLIT, color='//s,2)
      call MPI_COMM_SPLIT(MPI_COMM_WORLD,color,key,COMM_Coupler,ierr)
      call GLOB_ABORT(ierr,'CPL_INIT: error in MPI_COMM_SPLIT',1)

C     2. Start handshaking with components: 
C        find the Coupler master process (global) rank
C        Coupler_master_rank_global and send it to all other processes
C        (to be received in CMP_INIT). Also, find and store the global
C        rank of this process
C        
      call MPI_COMM_RANK(COMM_Coupler,process_rank_local,ierr)
      call GLOB_ABORT(ierr,'CPL_INIT: error in MPI_COMM_RANK call 1',1)
      call CPL_ANNOUNCE('CPL_INIT: to call MPI_COMM_RANK, call 2',3)
      call MPI_COMM_RANK(MPI_COMM_WORLD,process_rank_global,ierr)
      call GLOB_ABORT(ierr,'CPL_INIT: error in MPI_COMM_RANK call 2',1)

      MASTER=process_rank_local.eq.Coupler_master_rank_local

      if (MASTER) then
        Coupler_master_rank_global=process_rank_global
        write(s,'(i5)') Coupler_master_rank_global
        call CPL_ANNOUNCE('CPL_INIT: Coupler_master_rank_global='//s,2)
      end if
      call MPI_BCAST(Coupler_master_rank_global,1,MPI_INTEGER,
     >Coupler_master_rank_local,COMM_Coupler,ierr)
      call GLOB_ABORT(ierr,'CPL_INIT: error in MPI_BCAST',1)
 
      call MPI_COMM_SIZE(COMM_Coupler,Coupler_nprocs,ierr)
      call GLOB_ABORT(ierr,'CPL_INIT: error in MPI_COMM_SIZE,call 1',1)
      C_BC=Coupler_nprocs.gt.1 ! but don't use C_BC instead of RHS of
                               ! this assignment, since it may be
                               ! changed externally (this is just
                               ! initialization)
      call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs,ierr)
      call GLOB_ABORT(ierr,'CPL_INIT: error in MPI_COMM_SIZE,call 2',1)
      allocate(ids(nprocs))
      ibuffer(2)=Coupler_master_rank_global
      ibuffer(1)=Coupler_id
      ibuffer(3)=ibuffer_size
      ibuffer(4)=VerbLev
      tag=Coupler_id+23456
      do i=Coupler_nprocs,nprocs-1
        if (i.eq.process_rank_global) call GLOB_ABORT(i,
     >  'CPL_INIT: this number must not be a Coupler process rank',i)
        if (.not.MASTER) cycle

        call MPI_SEND(ibuffer,ibuffer_size,MPI_INTEGER,i,tag,
     >  MPI_COMM_WORLD,ierr)
        write(s,'(i5)') i
        if (ierr.ne.0) then
          call CPL_ANNOUNCE(
     >    'CPL_INIT: stopped, error in MPI_SEND to proc. #'//s,0)
          CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)
        end if
        call CPL_ANNOUNCE(
     >  'CPL_INIT: back from MPI_SEND to proc. #'//s,2)
c             print*,'CPL_INIT: back from MPI_SEND to proc. # ',i
      end do

C     3. Receive (and store) the active components' ids
C
      call MPI_GATHER(Coupler_id,1,MPI_INTEGER,ids,1,MPI_INTEGER,
     >Coupler_master_rank_global,MPI_COMM_WORLD,ierr)

C MPI_BCAST ids, to supply ids' values to Coupler nonmaster processes
C (do they need it?... suppose they do; so is assumed in CPL_INTRO):

      call MPI_BCAST(ids,nprocs,MPI_INTEGER,
     >Coupler_master_rank_local,COMM_Coupler,ierr)
      call GLOB_ABORT(ierr,'CPL_INIT: error in MPI_BCAST(ids,...',1)

c           print*,'CPL_INIT: returning'
      return
      END
C
C***********************************************************************
C
      SUBROUTINE CPL_INTRO(component_id)
C
C  This must be called once for each Component's id.
C
      USE CPL_COMM
 
      implicit none

      integer component_id

      integer tag,i
      integer
c test removal     > (kind=kind_INTEGER)
     > ibuf(3)
      logical component_inactive
C

            print*,'CPL_INTRO: entered, component_id=',component_id
            print*,'CPL_INTRO on entry: component_master_rank array: ',
     >      component_master_rank
      if (component_id.le.0) then
        call GLOB_ABORT(1,
     >  'C: CPL_INTRO: component_id nonpositive, terminated',1)
      end if

C  Find out if the Component is active (it then have sent its id to
C  CPL_INIT from CMP_INIT). If it is inactive, make its id negative
C  and return
C
      component_inactive=.true.
      do i=1,nprocs
        if (ids(i).eq.component_id) then
          component_inactive=.false.
          exit
        end if
      end do
            print*,'CPL_INTRO: component_id=',component_id
            print*,'CPL_INTRO: component_master_rank array: ',
     >      component_master_rank
      if (component_inactive) then
        component_id=-component_id
        print*,'C: CPL_INTRO: component with id=',-component_id,
     >  ' INACTIVE; id made =',component_id
        RETURN
      end if
      print*,'C: CPL_INTRO: component with id=',component_id,' ACTIVE'
            print*,'CPL_INTRO: component_id=',component_id
            print*,'CPL_INTRO: component_master_rank array: ',
     >      component_master_rank
!
      IF (MASTER) THEN       ! **
!
C  Complete handshaking with the Component: "register" it, i.e.
C  receive its master process global rank. Also, receive its
C  requested "flexibility level" (on which see cmp.comm.f)
C   ** Note: these data are supplied to Coupler master process ONLY
!
      tag=component_id+54321
      write(nprint,*)'CPL_INTRO: to call MPI_RECV ',ibuf,3,
     >      MPI_INTEGER,MPI_ANY_SOURCE,tag,MPI_COMM_WORLD,status,ierr
      call MPI_RECV(ibuf,3,MPI_INTEGER,MPI_ANY_SOURCE,tag,
     >MPI_COMM_WORLD,status,ierr)
c           print*,'CPL_INTRO: back from MPI_RECV ',ibuf,3,
c    >      MPI_INTEGER,MPI_ANY_SOURCE,tag,MPI_COMM_WORLD,status,ierr
c           print*,'CPL_INTRO: component_id=',component_id
c           print*,'CPL_INTRO: component_master_rank array: ',
c    >      component_master_rank
      call GLOB_ABORT(ierr,'CPL_INTRO: error in MPI_RECV',1)
c           print*,'CPL_INTRO: component_id=',component_id
c           print*,'CPL_INTRO: component_master_rank array: ',
c    >      component_master_rank
      component_master_rank(component_id)=ibuf(2)
c           print*,'CPL_INTRO: component_id=',component_id
c           print*,'CPL_INTRO: component_master_rank array: ',
c    >      component_master_rank
      component_FlexLev(component_id)=ibuf(3)
c           print*,'CPL_INTRO: component_id=',component_id
c           print*,'CPL_INTRO: component_master_rank array: ',
c    >      component_master_rank
      write(nprint,*)
     >'C: CPL_INTRO: back from MPI_RECV, component_master_rank=',
     >component_master_rank(component_id)
c           write(nprint,*)
c    >      'CPL_INTRO: back from MPI_RECV, component_master_rank=',
c    >      component_master_rank(component_id),' component_id=',
c    >      component_id,' component_FlexLev(component_id)=',
c    >      component_FlexLev(component_id)
      if (ibuf(1).ne.component_id) then
        write(nprint,
     >  '("C: CPL_INTRO: stopped, received component id value",i9/
     >  "not equal to component_id argument",i9)')
     >  ibuf(1),component_id
        call GLOB_ABORT(1,'CPL_INTRO: error',2)
      end if
C
C
c           print*,'CPL_INTRO to return: component_id=',component_id
c           print*,'CPL_INTRO to return: component_master_rank array: ',
c    >      component_master_rank

!
      END IF
!
      call MPI_BARRIER(COMM_Coupler,ierr)
!   <- this may or may not be necessary, depending on Coupler Main
!      details (it's harmless anyway)

      return
      END
C
C***********************************************************************
C
      SUBROUTINE CPL_RECV(F,N,component_id)
 
      USE CPL_COMM
 
      implicit none
 
      integer N,component_id,tag,source
      real (kind=kind_REAL) F(N)
C

c           print*,'CPL_RECV on entry: component_id: ',component_id
c           print*,'CPL_RECV on entry: '//
c    >      'component_master_rank array: ',component_master_rank

C  If Component is inactive, return:

      if (component_id.lt.0) then
        return
      end if

!
      IF (MASTER) THEN
         ! ** Only Coupler master proc. receives data from components
!

      if (component_FlexLev(component_id).le.1) then
        source=component_master_rank(component_id)
      else
        source=MPI_ANY_SOURCE
      end if
      tag=component_id     ! this may be bad if FlexLev(component_id)>1
                           ! (somehow a unique tag needs to be found)
cdbg  print*,'CPL_RECV: MPI_kind_REAL,source,tag: ',
cdbg >MPI_kind_REAL,source,tag
      call MPI_RECV(F,N,MPI_kind_REAL,source,tag,MPI_COMM_WORLD,
     >status,ierr)
      call GLOB_ABORT(ierr,'CPL_RECV: error in MPI_RECV',1)
cdbg  print*,'CPL_RECV: exiting'
!
      END IF
!
      if (C_BC) then
        call MPI_BCAST(F,N,MPI_kind_REAL,
     >  Coupler_master_rank_local,COMM_Coupler,ierr)
      else
        call MPI_BARRIER(COMM_Coupler,ierr)
!   <-  this is not necessary if the call of this subr. is followed by
!       a disassembling or broadcasting of the received data or another
!       blocking procedure (but it's harmless anyway)
      end if

c           print*,'CPL_RECV to return: '//
c    >      'component_master_rank array: ',component_master_rank
      return
      END
C
C***********************************************************************
C
      SUBROUTINE CPL_alt_RECV(F,N,component_id)
 
      USE CPL_COMM
 
      implicit none

      integer N,component_id
      real (kind=kind_alt_REAL) F(N)

      integer tag,source
C

C  If Component is inactive, return:

      if (component_id.lt.0) then
        return
      end if
!
      IF (MASTER) THEN
         ! ** Only Coupler master proc. receives data from components
!

      if (component_FlexLev(component_id).le.1) then
        source=component_master_rank(component_id)
      else
        source=MPI_ANY_SOURCE
      end if
      tag=component_id     ! this may be bad if FlexLev(component_id)>1
                           ! (somehow a unique tag needs to be found)
      call MPI_RECV(F,N,MPI_kind_alt_REAL,source,tag,MPI_COMM_WORLD,
     >status,ierr)
      call GLOB_ABORT(ierr,'CPL_alt_RECV: error in MPI_RECV',1)
!
      END IF
!
      if (C_BC) then
        call MPI_BCAST(F,N,MPI_kind_alt_REAL,
     >  Coupler_master_rank_local,COMM_Coupler,ierr)
      else
        call MPI_BARRIER(COMM_Coupler,ierr)
!   <-  this is not necessary if the call of this subr. is followed by
!       a disassembling or broadcasting of the received data or another
!       blocking procedure (but it's harmless anyway)
      end if

      return
      END
C
C***********************************************************************
C
      SUBROUTINE CPL_INTEGER_RECV(F,N,component_id)
 
      USE CPL_COMM
 
      implicit none
 
      integer N,component_id
      integer
c test removal     > (kind=kind_INTEGER)
     > F(N)

      integer tag,source
C

c           print*,'CPL_INTEGER_RECV: entered, N=',N,
c    >      ' component_id=',component_id,' F=',F
c           print*,'CPL_INTEGER_RECV on entry: '//
c    >      'component_master_rank array: ',component_master_rank
 
C  If Component is inactive, return:

      if (component_id.lt.0) then
        return
      end if
!
      IF (MASTER) THEN
         ! ** Only Coupler master proc. receives data from components
!

      if (component_FlexLev(component_id).le.1) then
        source=component_master_rank(component_id)
            print*,'CPL_INTEGER_RECV: source=component_master_rank('//
     >      'component_id)=',source
      else
        source=MPI_ANY_SOURCE
            print*,'CPL_INTEGER_RECV: source=MPI_ANY_SOURCE=',source
      end if
      tag=component_id     ! this may be bad if FlexLev(component_id)>1
                           ! (somehow a unique tag needs to be found)
            print*,'CPL_INTEGER_RECV: to call MPI_RECV, source=',
     >      source,' tag=',tag,' status=',status
      call MPI_RECV(F,N,MPI_INTEGER,source,tag,MPI_COMM_WORLD,
     >status,ierr)
            print*,'CPL_INTEGER_RECV: back from MPI_RECV, source=',
     >      source,' tag=',tag,' status=',status,' F=',F
      call GLOB_ABORT(ierr,'CPL_INTEGER_RECV: error in MPI_RECV',1)
!
      END IF
!
      if (C_BC) then
        call MPI_BCAST(F,N,MPI_INTEGER,
     >  Coupler_master_rank_local,COMM_Coupler,ierr)
      else
        call MPI_BARRIER(COMM_Coupler,ierr)
!   <-  this is not necessary if the call of this subr. is followed by
!       a disassembling or broadcasting of the received data or another
!       blocking procedure (but it's harmless anyway)
      end if

c           print*,'CPL_INTEGER_RECV to return: '//
c    >      'component_master_rank array: ',component_master_rank
      return
      END
C
C***********************************************************************
C
      SUBROUTINE CPL_R(F,N,component_id,knd)
 
      USE CPL_COMM
 
      implicit none
 
      integer N,component_id,knd
      real (kind=kind_REAL) F(N)

      integer tag,source
      integer MPI_knd
      real (kind=kind_alt_REAL) Fa(N)
C

c           print*,'CPL_R on entry: component_id, knd: ',component_id,knd
c           print*,'CPL_R on entry: '//
c    >      'component_master_rank array: ',component_master_rank

C  If Component is inactive, return:
      if (component_id.lt.0) then
        return
      end if

      if (knd.eq.kind_alt_REAL) then
        MPI_knd=MPI_kind_alt_REAL
      else if (knd.eq.kind_REAL) then
        MPI_knd=MPI_kind_REAL
      else
        print*,'C: CPL_R: **FATAL**: knd=',knd
        call GLOB_ABORT(1,
     >  'CPL_R: knd is neither kind_REAL nor kind_alt_REAL, aborted',1)
      end if
!
      IF (MASTER) THEN
         ! ** Only Coupler master proc. receives data from components
!
      if (component_FlexLev(component_id).le.1) then
        source=component_master_rank(component_id)
      else
        source=MPI_ANY_SOURCE
      end if

      tag=component_id     ! this may be bad if FlexLev(component_id)>1
                           ! (somehow a unique tag needs to be found)

      if (knd.eq.kind_alt_REAL) then
        call MPI_RECV(Fa,N,MPI_kind_alt_REAL,source,tag,MPI_COMM_WORLD,
     >  status,ierr)
        if (.not.C_BC) F=Fa
      else
        call MPI_RECV(F,N,MPI_kind_REAL,source,tag,MPI_COMM_WORLD,
     >  status,ierr)
      end if

      call GLOB_ABORT(ierr,'CPL_RECV: error in MPI_RECV',1)
!
      END IF
!
      if (C_BC) then
        if (knd.eq.kind_alt_REAL) then
          if (kind_alt_real.lt.kind_real) then
            call MPI_BCAST(Fa,N,MPI_kind_alt_REAL,
     >      Coupler_master_rank_local,COMM_Coupler,ierr)
            F=Fa
          else
            F=Fa
            call MPI_BCAST(F,N,MPI_kind_REAL,
     >      Coupler_master_rank_local,COMM_Coupler,ierr)
          end if
        else
            call MPI_BCAST(F,N,MPI_kind_REAL,
     >      Coupler_master_rank_local,COMM_Coupler,ierr)
        end if
      else
        call MPI_BARRIER(COMM_Coupler,ierr)
!   <-  this is not necessary if the call of this subr. is followed by
!       a disassembling or broadcasting of the received data or another
!       blocking procedure (but it's harmless anyway)
      end if

c           print*,'CPL_R to return: '//
c    >      'component_master_rank array: ',component_master_rank
      return
      END
C
C***********************************************************************
C
      SUBROUTINE CPL_SEND(F,N,component_id)
 
      USE CPL_COMM
 
      implicit none
 
      integer N,component_id
      real (kind=kind_REAL) F(N)

      integer tag,destination
      integer
c test removal     > (kind=kind_INTEGER)
     > ibuf(3)
      real (kind=kind_REAL) F1(N)               !<- for debugging only,
                                                ! see comments in
                                                ! module CPL_COMM
C

c           print*,'CPL_SEND on entry: component_id: ',component_id
c           print*,'CPL_SEND on entry: '//
c    >      'component_master_rank array: ',component_master_rank
 
C  If Component is inactive, return

      if (component_id.lt.0) return

!
      IF (MASTER) THEN
         ! ** Only Coupler master proc. sends data to components
!
      if (component_FlexLev(component_id).le.2) then
        destination=component_master_rank(component_id)
      else
        call MPI_RECV(ibuf,2,MPI_INTEGER,MPI_ANY_SOURCE,
     >  tag,MPI_COMM_WORLD,status,ierr)
        call GLOB_ABORT(ierr,'CPL_SEND: error in MPI_RECV',1)
        if (ibuf(1).ne.component_id) then
          write(nprint,
     >    '("C: CPL_SEND: received component id value",i9/
     >    "not equal to component_id argument",i9)')
     >    ibuf(1),component_id
          call GLOB_ABORT(ierr,'CPL_SEND: error',2)
        end if
        destination=ibuf(2)
      end if
      tag=component_id     ! this may be bad if FlexLev(component_id)>1
                           ! (somehow a unique tag needs to be found)
      IF (SNDSHT(component_id)) THEN            ! for debugging only,
        F1=-1.E30                               ! see comments in
        call MPI_SEND(F1,N,MPI_kind_REAL,       ! module CPL_COMM
     >  destination,tag,MPI_COMM_WORLD,ierr)    ! ------------------
      ELSE                                      ! ------------------
      call MPI_SEND(F,N,MPI_kind_REAL,destination,tag,
     >MPI_COMM_WORLD,ierr)
      END IF                                    ! ------------------
      call GLOB_ABORT(ierr,'CPL_SEND: error in MPI_SEND',1)
!
      END IF
!
      call MPI_BARRIER(COMM_Coupler,ierr)
!   <- this does not seem to be necessary but it's harmless anyway

c           print*,'CPL_SEND to return: '//
c    >      'component_master_rank array: ',component_master_rank
      return
      END
C
C***********************************************************************
C
      SUBROUTINE CPL_INTEGER_SEND(F,N,component_id)
 
      USE CPL_COMM
 
      implicit none
 
      integer N,component_id
      integer F(N)

      integer tag,destination
      integer
c test removal     > (kind=kind_INTEGER)
     > ibuf(3)
C
 
C  If Component is inactive, return

      if (component_id.lt.0) return

!
      IF (MASTER) THEN
         ! ** Only Coupler master proc. sends data to components
!
      if (component_FlexLev(component_id).le.2) then
        destination=component_master_rank(component_id)
      else
        call MPI_RECV(ibuf,2,MPI_INTEGER,MPI_ANY_SOURCE,
     >  tag,MPI_COMM_WORLD,status,ierr)
        call GLOB_ABORT(ierr,'CPL_INTEGER_SEND: error in MPI_RECV',1)
        if (ibuf(1).ne.component_id) then
          write(nprint,
     >    '("C: CPL_INTEGER_SEND: received component id value",i9/
     >    "not equal to component_id argument",i9)')
     >    ibuf(1),component_id
          call GLOB_ABORT(ierr,'CPL_INTEGER_SEND: error',2)
        end if
        destination=ibuf(2)
      end if
      tag=component_id     ! this may be bad if FlexLev(component_id)>1
                           ! (somehow a unique tag needs to be found)
      call MPI_SEND(F,N,MPI_INTEGER,destination,tag,
     >MPI_COMM_WORLD,ierr)
      call GLOB_ABORT(ierr,'CPL_INTEGER_SEND: error in MPI_SEND',1)
!
      END IF
!
      call MPI_BARRIER(COMM_Coupler,ierr)
!   <- this does not seem to be necessary but it's harmless anyway

      return
      END
C
C***********************************************************************
C
      SUBROUTINE CPL_alt_SEND(F,N,component_id)
 
      USE CPL_COMM
 
      implicit none
 
      integer N,component_id
      real (kind=kind_alt_REAL) F(N)

      integer tag,destination
      integer
c test removal     > (kind=kind_INTEGER)
     > ibuf(3)
      real (kind=kind_alt_REAL) F1(N)           !<- for debugging only,
                                                ! see comments in
                                                ! module CPL_COMM
C

c           print*,'CPL_alt_SEND on entry: component_id: ',component_id
c           print*,'CPL_alt_SEND on entry: '//
c    >      'component_master_rank array: ',component_master_rank
 
C  If Component is inactive, return

      if (component_id.lt.0) return

!
      IF (MASTER) THEN
         ! ** Only Coupler master proc. sends data to components
!
      if (component_FlexLev(component_id).le.2) then
        destination=component_master_rank(component_id)
      else
        call MPI_RECV(ibuf,2,MPI_INTEGER,MPI_ANY_SOURCE,
     >  tag,MPI_COMM_WORLD,status,ierr)
        call GLOB_ABORT(ierr,'CPL_SEND: error in MPI_RECV',1)
        if (ibuf(1).ne.component_id) then
          write(nprint,
     >    '("C: CPL_SEND: received component id value",i9/
     >    "not equal to component_id argument",i9)')
     >    ibuf(1),component_id
          call GLOB_ABORT(ierr,'CPL_SEND: error',2)
        end if
        destination=ibuf(2)
      end if
      tag=component_id     ! this may be bad if FlexLev(component_id)>1
                           ! (somehow a unique tag needs to be found)
      IF (SNDSHT(component_id)) THEN            ! for debugging only,
        F1=-1.E30                               ! see comments in
        call MPI_SEND(F1,N,MPI_kind_alt_REAL,   ! module CPL_COMM
     >  destination,tag,MPI_COMM_WORLD,ierr)    ! ------------------
      ELSE                                      ! ------------------
      call MPI_SEND(F,N,MPI_kind_alt_REAL,destination,tag,
     >MPI_COMM_WORLD,ierr)
      END IF                                    ! ------------------
      call GLOB_ABORT(ierr,'CPL_SEND: error in MPI_SEND',1)
!
      END IF
!
      call MPI_BARRIER(COMM_Coupler,ierr)
!   <- this does not seem to be necessary but it's harmless anyway

c           print*,'CPL_alt_SEND to return: '//
c    >      'component_master_rank array: ',component_master_rank
      return
      END
C
C***********************************************************************
C
      SUBROUTINE CPL_S(F,N,component_id,knd)
 
      USE CPL_COMM
 
      implicit none
 
      integer N,component_id,knd
      real (kind=kind_REAL) F(N)

      integer tag,destination
      integer
c test removal     > (kind=kind_INTEGER)
     > ibuf(3)
      real (kind=kind_alt_REAL) Fa(N)
      real (kind=kind_REAL) F1(N)               !<- for debugging only,
                                                ! see comments in
                                                ! module CPL_COMM
C

c           print*,'CPL_S on entry: component_id: ',component_id
c           print*,'CPL_S on entry: '//
c    >      'component_master_rank array: ',component_master_rank
 
C  If Component is inactive, return

      if (component_id.lt.0) return

!
      IF (MASTER) THEN
         ! ** Only Coupler master proc. sends data to components
!
      if (component_FlexLev(component_id).le.2) then
        destination=component_master_rank(component_id)
      else
        call MPI_RECV(ibuf,2,MPI_INTEGER,MPI_ANY_SOURCE,
     >  tag,MPI_COMM_WORLD,status,ierr)
        call GLOB_ABORT(ierr,'CPL_SEND: error in MPI_RECV',1)
        if (ibuf(1).ne.component_id) then
          write(nprint,
     >    '("C: CPL_SEND: received component id value",i9/
     >    "not equal to component_id argument",i9)')
     >    ibuf(1),component_id
          call GLOB_ABORT(ierr,'CPL_SEND: error',2)
        end if
        destination=ibuf(2)
      end if
      tag=component_id     ! this may be bad if FlexLev(component_id)>1
                           ! (somehow a unique tag needs to be found)

      if (knd.eq.kind_alt_REAL) then
        IF (SNDSHT(component_id)) THEN            ! for debugging only,
          Fa=-1.E30                               ! see comments in
        ELSE                                      ! module CPL_COMM
          Fa=F
        END IF                                    ! ------------------
        call MPI_SEND(Fa,N,MPI_kind_alt_REAL,
     >  destination,tag,MPI_COMM_WORLD,ierr)
      else if (knd.eq.kind_REAL) then
        IF (SNDSHT(component_id)) THEN            ! for debugging only,
          F1=-1.E30                               ! see comments in
          call MPI_SEND(F1,N,MPI_kind_REAL,       ! module CPL_COMM
     >    destination,tag,MPI_COMM_WORLD,ierr)    ! ------------------
        ELSE                                      ! ------------------
          call MPI_SEND(F,N,MPI_kind_REAL,destination,tag,
     >    MPI_COMM_WORLD,ierr)
        END IF                                    ! ------------------
      else
        print*,'C: CPL_S: **FATAL**: knd=',knd
        call GLOB_ABORT(1,
     >  'CPL_S: knd is neither kind_REAL nor kind_alt_REAL, aborted',1)
      end if

      call GLOB_ABORT(ierr,'CPL_SEND: error in MPI_SEND',1)
!
      END IF
!
      call MPI_BARRIER(COMM_Coupler,ierr)
!   <- this does not seem to be necessary but it's harmless anyway

c           print*,'CPL_S to return: '//
c    >      'component_master_rank array: ',component_master_rank
      return
      END
C
C***********************************************************************
C
      SUBROUTINE CPL_BC(F,N)
 
      USE CPL_COMM
 
      implicit none
 
      integer N
      real (kind=kind_REAL) F(N)
C
      if (Coupler_nprocs.eq.1) return

      call MPI_BCAST(F,N,MPI_kind_REAL,
     >Coupler_master_rank_local,COMM_Coupler,ierr)

      call GLOB_ABORT(ierr,'CPL_BC: error in MPI_BCAST',1)

      return
      END
C
C***********************************************************************
C
      SUBROUTINE CPL_INTEGER_BC(F,N)
 
      USE CPL_COMM
 
      implicit none
 
      integer N
      integer F(N)
C
      if (Coupler_nprocs.eq.1) return

      call MPI_BCAST(F,N,MPI_INTEGER,
     >Coupler_master_rank_local,COMM_Coupler,ierr)

      call GLOB_ABORT(ierr,'CPL_INTEGER_BC: error in MPI_BCAST',1)

      return
      END
C
C***********************************************************************
C
      SUBROUTINE CPL_LOGICAL_BC(F,N)
 
      USE CPL_COMM
 
      implicit none
 
      integer N
      logical F(N)
C
      if (Coupler_nprocs.eq.1) return

      call MPI_BCAST(F,N,MPI_LOGICAL,
     >Coupler_master_rank_local,COMM_Coupler,ierr)

      call GLOB_ABORT(ierr,'CPL_INTEGER_BC: error in MPI_BCAST',1)

      return
      END
C
C***********************************************************************
C
      SUBROUTINE CPL_GATHM(F,FM,N,N1,need)
 
      USE CPL_COMM
 
      implicit none
 
      integer N,N1
      real (kind=kind_REAL) F(1),FM(1)
      logical need
C
!     if (Coupler_nprocs.eq.1) return ! <- ?

      IF (MASTER) THEN  ! here N may differ from N1
        if (need) FM(1:N)=F(1:N) ! this assignment MAY be superfluous
!error! call MPI_GATHER(MPI_IN_PLACE,0,0,FM(NM+1),N,MPI_kind_REAL,
        call MPI_GATHER(MPI_IN_PLACE,0,0,FM(N-N1+1),N1,MPI_kind_REAL,
     >  Coupler_master_rank_local,COMM_Coupler,ierr)
      ELSE              ! here N must be =N1
        call MPI_GATHER(F,N,MPI_kind_REAL,FM,0,0,
     >  Coupler_master_rank_local,COMM_Coupler,ierr)
      END IF

      call GLOB_ABORT(ierr,'CPL_GATHM: error in MPI_GATHER',1)

      return
      END
C
C***********************************************************************
C
      SUBROUTINE CPL_INTEGER_GATHM(F,FM,N,N1,need)
 
      USE CPL_COMM
 
      implicit none
 
      integer N,N1
      integer F(1),FM(1)
      logical need
C
!     if (Coupler_nprocs.eq.1) return ! <- ?

      IF (MASTER) THEN  ! here N may differ from N1
        if (need) FM(1:N)=F(1:N) ! this assignment MAY be superfluous
        call MPI_GATHER(MPI_IN_PLACE,0,0,FM(N-N1+1),N1,MPI_INTEGER,
     >  Coupler_master_rank_local,COMM_Coupler,ierr)
           print*,'CPL_INTEGER_GATHM, FM(1):',FM(1)
      ELSE              ! here N must be =N1
        call MPI_GATHER(F,N,MPI_INTEGER,FM,0,0,
     >  Coupler_master_rank_local,COMM_Coupler,ierr)
      END IF

      call GLOB_ABORT(ierr,'CPL_GATHM: error in MPI_GATHER',1)

      return
      END
C
C***********************************************************************
C
      real function cpl_sum(x)
 
      USE CPL_COMM

      implicit none

      real x  

      integer i
      real s  
      real buf(Coupler_nprocs)
C

      call MPI_GATHER(x,1,MPI_kind_REAL,buf,1,MPI_kind_REAL,
     >Coupler_master_rank_local,COMM_Coupler,ierr)
      s=0.
      if (MASTER) then
        do i=1,Coupler_nprocs
          s=s+buf(i)
        end do  
      end if  

      cpl_sum=s

!     call CPL_BARR  ! doubtfully, this may be needed for usages such
                     ! as: x=cpl_sum(x)

      return  
      END
C
C***********************************************************************
C
      integer function cpl_isum(x)
 
      USE CPL_COMM

      implicit none

      integer x  

      integer i,s
      integer buf(Coupler_nprocs)
C

      call MPI_GATHER(x,1,MPI_INTEGER,buf,1,MPI_INTEGER,
     >Coupler_master_rank_local,COMM_Coupler,ierr)
      s=0.
      if (MASTER) then
        do i=1,Coupler_nprocs
          s=s+buf(i)
        end do  
      end if  

      cpl_isum=s

!     call CPL_BARR  ! doubtfully, this may be needed for usages such
                     ! as: x=cpl_isum(x)

      return  
      END
C
C***********************************************************************
C
      subroutine cpl_max(x,xm,n)
 
      USE CPL_COMM

      implicit none

      real x,xm
      integer n

      integer k
      real s
      real buf(Coupler_nprocs)
C

      if (Coupler_nprocs.eq.1) then
        xm=x
        n=0
        RETURN
      end if

      call MPI_GATHER(x,1,MPI_kind_REAL,buf,1,MPI_kind_REAL,
     >Coupler_master_rank_local,COMM_Coupler,ierr)
      if (MASTER) then
        xm=buf(1)
        n=0
        do k=2,Coupler_nprocs
          if (buf(k).gt.xm) then
            xm=buf(k)
            n=k-1
          end if
        end do  
      end if  

      return  
      END
C
C***********************************************************************
C
      subroutine cpl_min(x,xm,n)
 
      USE CPL_COMM

      implicit none

      real x,xm
      integer n

      integer k
      real s
      real buf(Coupler_nprocs)
C

      if (Coupler_nprocs.eq.1) then
        xm=x
        n=0
        RETURN
      end if

      call MPI_GATHER(x,1,MPI_kind_REAL,buf,1,MPI_kind_REAL,
     >Coupler_master_rank_local,COMM_Coupler,ierr)
      if (MASTER) then
        xm=buf(1)
        n=0
        do k=2,Coupler_nprocs
          if (buf(k).lt.xm) then
            xm=buf(k)
            n=k-1
          end if
        end do  
      end if  

      return  
      END
C
C***********************************************************************
C
      subroutine cpl_integer2master(i,n)
 
      USE CPL_COMM

      implicit none

      integer i   ! integer to be sent from proc. n to master proc.
      integer n   ! must have the right value in master proc.

      integer m
C

      if (Coupler_nprocs.eq.1) RETURN

      m=n
      call CPL_INTEGER_BC(m,1)
      if (m.eq.Coupler_master_rank_local) RETURN
      if (process_rank_local.eq.m) then
        call MPI_SEND(i,1,MPI_INTEGER,Coupler_master_rank_local,tagloc,
     >  COMM_Coupler,ierr)
      else if (m.ge.Coupler_nprocs) then
        call GLOB_ABORT(m,'cpl_integer2master: n.ge.Coupler_nprocs',1)
      end if
      if (MASTER) then
        call MPI_RECV(i,1,MPI_INTEGER,m,tagloc,COMM_Coupler,
     >  status,ierr)
      end if

      return  
      END
C
C***********************************************************************
C
      SUBROUTINE CPL_BARR
 
      USE CPL_COMM
 
      implicit none
C
      if (Coupler_nprocs.eq.1) return

      call MPI_BARRIER(COMM_Coupler,ierr)

      call GLOB_ABORT(ierr,'CPL_BARR: error in MPI_BARRIER',1)

      return
      END
 
C
C***********************************************************************
C
      SUBROUTINE CPL_ANNOUNCE(s,DbgLev)

      USE CPL_COMM, ONLY: VerbLev,nprint,MASTER

      implicit none

      character*(*) s
      integer DbgLev
C
      if (DbgLev.le.VerbLev .and. MASTER)
     >  write(nprint,*) 'C: '//s

      return
      END
