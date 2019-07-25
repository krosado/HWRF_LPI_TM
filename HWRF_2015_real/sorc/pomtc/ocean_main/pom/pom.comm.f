      MODULE OC_cc

      USE CMP_COMM, ONLY:

     >   MPI_COMM_Ocean => COMM_local,

     >   Coupler_id,
     >   component_master_rank_local,
     >   process_rank_local,
     >   component_nprocs,
     >   ibuffer,

     >   MPI_INTEGER,MPI_STATUS_SIZE,

     >   kind_REAL,kind_alt_REAL,MPI_kind_REAL,MPI_kind_alt_REAL

      implicit none

      real dtc            !<- Coupling period

      integer i_dtc2dto /100/,  !<- Coupling period / OM time step
     >        n_ts /-1/   !<- number of time step - 1
                          ! (increments/used in OC_SENDSST)

      integer IM,JM,NX,NY,NLEV,nt,NGP,rc

      integer,parameter:: kind_R=kind_alt_REAL

      integer,parameter:: kind_sfcflux=kind_R,
     >                    kind_SST=kind_R,
     >                    kind_SLM=kind_R,
     >                    kind_lonlat=kind_R
      integer MPI_kind_R,
     >MPI_kind_sfcflux,MPI_kind_SST,MPI_kind_SLM,MPI_kind_lonlat

      integer MPI_DATATYPE_sfcflux

      integer, parameter :: Ocean_spec=1  ! identifies the OM as POM
      integer, parameter :: nc=1  ! # of copies of each flux

      real,allocatable:: ALONt(:,:),ALATt(:,:),
     >                   ALONu(:,:),ALATu(:,:),
     >                   ALONv(:,:),ALATv(:,:)

c     real(kind=kind_SLM),allocatable:: SLMt(:,:),SLMu(:,:),SLMv(:,:)
!<- these are not needed other than for printout purposes. Uncomment
!if the latter require (note same indication elsewhere)

Controls:
      integer nunit_announce /6/, VerbLev /3/

      SAVE

      END MODULE OC_cc
C
C***********************************************************************
C
      MODULE SFLUX_cc

      USE OC_cc, ONLY: kind_sfcflux

      implicit none

      integer num_sflx /4/  ! # of surf. fluxes to be dealt with

      real(kind=kind_sfcflux),allocatable:: sflx(:,:,:)

      SAVE

      END MODULE SFLUX_cc
C
C***********************************************************************
C
      SUBROUTINE OC_CMP_START

      USE OC_cc, ONLY:
     >  process_rank_local,VerbLev,ibuffer,Coupler_id,Ocean_spec

      implicit none

      integer Ocean_id /2/, Ocean_master_rank_local /0/
      integer ibuf(1)
      character*20 s
C

                      !<-id of OM as a component of the coupled system
      call CMP_INIT(Ocean_id,1)
                             !<-"flexibility level"
      if (Coupler_id.ge.0) VerbLev=min(VerbLev,ibuffer(4))

      if (process_rank_local.eq.Ocean_master_rank_local) then
        print*,'**OM: back from CMP_INIT, to call CMP_INTRO'
      end if

      call CMP_INTRO(Ocean_master_rank_local)

      write(s,'(i2)') VerbLev
      call OC_ANNOUNCE('back from CMP_INTRO, VerbLev='//s,2)

      ibuf(1)=Ocean_spec

      call CMP_INTEGER_SEND(ibuf,1)

      write(s,'(i2)') ibuf(1)
      call OC_ANNOUNCE('OC_CMP_START: returning, sent Ocean_spec='//s,2)

      return
      END
C
C***********************************************************************
C
      SUBROUTINE OC_INIT(im_,jm_)

      USE OC_cc

      USE SFLUX_cc

      implicit none

      integer im_,jm_
C

      call OC_ANNOUNCE('OC_INIT: entered',2)

      IM=im_
      JM=jm_

      NX=IM
      NY=JM

      NGP=NY*NX

      allocate(ALONt(NX,NY),ALATt(NX,NY),
     >ALONu(NX,NY),ALATu(NX,NY),
     >ALONv(NX,NY),ALATv(NX,NY))

c       allocate(SLMt(NX,NY),
c    >  SLMu(NX,NY),SLMv(NX,NY))
!<- these are not needed other than for printout purposes. Uncomment
!if the latter require (note same indication elsewhere)

      allocate(sflx(NX,NY,num_sflx))

      if (kind_R.eq.kind_REAL) then
        MPI_kind_R=MPI_kind_REAL
      else if (kind_R.eq.kind_alt_REAL) then
        MPI_kind_R=MPI_kind_alt_REAL
      else
        call GLOB_ABORT(1,
     >  'kind_R is neither kind_REAL nor kind_alt_REAL',1)
      end if

      if (kind_sfcflux.eq.kind_REAL) then
        MPI_kind_sfcflux=MPI_kind_REAL
      else 
        MPI_kind_sfcflux=MPI_kind_alt_REAL
      end if
      if (kind_SST.eq.kind_REAL) then
        MPI_kind_SST=MPI_kind_REAL
      else 
        MPI_kind_SST=MPI_kind_alt_REAL
      end if
      if (kind_SLM.eq.kind_REAL) then
        MPI_kind_SLM=MPI_kind_REAL
      else 
        MPI_kind_SLM=MPI_kind_alt_REAL
      end if
      if (kind_lonlat.eq.kind_REAL) then
        MPI_kind_lonlat=MPI_kind_REAL
      else
        MPI_kind_lonlat=MPI_kind_alt_REAL
      end if

      if (VerbLev.ge.3) print*,'OC_INIT: returning ',NX,NY

      return
      END
C
C***********************************************************************
C
      SUBROUTINE OC_SENDGRIDS(xt,yt,xu,yu,xv,yv,dt)

      USE OC_cc

      implicit none

      real(kind=kind_lonlat), dimension(NX,NY):: xt,xu,xv
      real(kind=kind_lonlat), dimension(NX,NY):: yt,yu,yv
      real(kind=kind_R) dt
      
      integer ibuf(2),i,j
      real(kind=kind_R) buf(1)
      character*20 s
C

      if (Coupler_id.lt.0) return    !   <- standalone mode

      buf(1)=dt   ! <- OM time step
      write(s,'(1pe20.12)') dt
      call OC_ANNOUNCE('to send time step='//s,2)
      call CMP_gnr_SEND(buf,1,MPI_kind_R)
      call OC_ANNOUNCE('time step='//s//' sent',1)

      IF (component_master_rank_local.eq.process_rank_local) THEN

        call CMP_gnr_RECV(buf,1,MPI_kind_R)
        dtc=buf(1)
        write(s,'(1pe20.12)') dtc
        call OC_ANNOUNCE('coupling period received: '//s,1)
        i_dtc2dto=nint(dtc/dt)
        if (abs(i_dtc2dto-dtc/dt).gt.1.E-7) call GLOB_ABORT(1,
     >  'OM: ABORTED: dtc is not a multiple of dt',1)

        ibuf(1)=NX
        ibuf(2)=NY
        call OC_ANNOUNCE('to send grid dimensions',2)
        call CMP_INTEGER_SEND(ibuf,2)
        call OC_ANNOUNCE('grid dimensions sent',1)

        ibuf(1)=i_dtc2dto

      END IF

      call MPI_BCAST(ibuf,1,MPI_INTEGER,component_master_rank_local,
     >MPI_COMM_Ocean,rc)
      i_dtc2dto=ibuf(1)

      call OC_ANNOUNCE('i_dtc2dto broadcast OK',2)

!      do j=1,NY
        ALONt(:,:)=xt(:,:)
        ALONu(:,:)=xu(:,:)
        ALONv(:,:)=xv(:,:)
        ALATt(:,:)=yt(:,:)
        ALATu(:,:)=yu(:,:)
        ALATv(:,:)=yv(:,:)
!      end do
       

         print*,'OM: OC_SENDGRIDS: ALONt(1:3,1:3): ',ALONt(1:3,1:3)
         print*,'OM: OC_SENDGRIDS: ALONu(1:3,1:3): ',ALONu(1:3,1:3)
         print*,'OM: OC_SENDGRIDS: ALONv(1:3,1:3): ',ALONv(1:3,1:3)

      call OC_ANNOUNCE('(BP) to send grid arrays (6 MPI calls)',2)

      print*,'Biju in 01'
      call CMP_gnr_SEND(ALONt,NGP,MPI_kind_lonlat)
       print*,'Biju out 01'
      call CMP_gnr_SEND(ALATt,NGP,MPI_kind_lonlat)
      call CMP_gnr_SEND(ALONu,NGP,MPI_kind_lonlat)
      call CMP_gnr_SEND(ALATu,NGP,MPI_kind_lonlat)
      call CMP_gnr_SEND(ALONv,NGP,MPI_kind_lonlat)
      call CMP_gnr_SEND(ALATv,NGP,MPI_kind_lonlat)

      call OC_ANNOUNCE('the 6 grid arrays sent',1)

      call OC_ANNOUNCE('(BP) OC_SENDGRIDS: returning',2)

      return
      END
C
C***********************************************************************
C
      SUBROUTINE OC_ANNOUNCE(s,DbgLev)

      USE OC_cc, ONLY: nunit_announce,VerbLev,MPI_COMM_Ocean

      implicit none

      character*(*) s
      integer DbgLev

      integer ierr
C
      if (DbgLev.le.VerbLev) then
        if (s(1:5).eq.'(BP) ') then
          call MPI_BARRIER(MPI_COMM_Ocean,ierr)
        end if
        CALL CMP_ANNOUNCE(nunit_announce,'OM: '//s)
      end if

      return
      END
C
C***********************************************************************
C
      SUBROUTINE OC_SENDSLM(mt,mu,mv)

      USE OC_cc

      implicit none

      real(kind=kind_SLM), dimension(NX,NY):: mt,mu,mv

C

      if (Coupler_id.lt.0) return    !   <- standalone mode

C       mt, mu, mv are supposed to be 1 at sea and 0 on land

      call CMP_gnr_SEND(mt,NGP,MPI_kind_SLM)
      call CMP_gnr_SEND(mu,NGP,MPI_kind_SLM)
      call CMP_gnr_SEND(mv,NGP,MPI_kind_SLM)

      return
      END
C
C***********************************************************************
C
      SUBROUTINE OC_SENDSST(SST)

      USE OC_cc

      implicit none

      real(kind=kind_SST) SST(NX,NY)

      character*20 s
C

      n_ts=n_ts+1

      if (VerbLev.ge.3) write(s,'(2i10)') n_ts,i_dtc2dto
      call OC_ANNOUNCE('OC_SENDSST: n_ts, i_dtc2dto'//s,3)

      if (Coupler_id.lt.0) RETURN  !   <- standalone mode

      IF ((n_ts/i_dtc2dto)*i_dtc2dto.ne.n_ts) RETURN

      if (NX.ne.IM .or. NY.ne.JM) then
        call GLOB_ABORT(1,
     >  '** OC_SENDSST: NX.ne.IM .or. NY.ne.JM',1)
      end if
             ! NX, NY are sizes of array for which interpolated
             ! values are computed by C. IM, JM are the sizes of whole
             ! grid array in POM. If the two pairs are not the same
             ! (normally, it can be that IM>NX and/or JM>NY
             ! because of boundaries), a new array SST1(IM,JM) needs to
             ! be used [SST=SST1(x:x+NX-1,y:y+NY-1)]

      write(92,*)SST(240:244,200:204)
      call CMP_gnr_SEND(SST,NGP,MPI_kind_SST)

      call OC_ANNOUNCE('OC_SENDSST: to return',3)

      return
      END
C
C***********************************************************************
C
      SUBROUTINE OC_RECV_SBC(SF,fctr)

      USE OC_cc

      USE SFLUX_cc

      implicit none

      real(kind=kind_sfcflux) SF(NX,NY)
      real(kind=kind_R) fctr
                                      ! declared as "real" in POM
                                      ! (comblk.h)
      
      real(kind=kind_sfcflux) F(NX,NY)
      real too_low /-1.E20/ ! must be >= very_large_negative in
                            ! interp. routine, currently -1.E30
      integer i,j,m,n
      integer nts,n_ts_old/-100/
      logical SENDSSTcomesFIRST/.true./
      save too_low,n_ts_old,SENDSSTcomesFIRST,n
C

      call OC_ANNOUNCE('OC_RECV_SBC entered',3)

      if (NX.ne.IM .or. NY.ne.JM) then
        call GLOB_ABORT(1,'** OC_RECVSBC: NX.ne.IM.or.NY.ne.JM',1)
      end if
             ! NX, NY are sizes of array for which interpolated
             ! values are computed by C. IM, JM are sizes of whole
             ! grid array in HYCOM. If the two pairs are not the same
             ! (normally, it can be that IM>NX and/or JM>NY
             ! because of boundaries) SF must be declared SF(IM,JM)
             ! and only its section assigned F(1:NX,1:NY)

      if (n_ts.eq.-1) SENDSSTcomesFIRST=.false.
      if (SENDSSTcomesFIRST) then
        nts=n_ts
      else
        nts=n_ts+1
      end if

      if (n_ts.eq.n_ts_old) then
        n=n+1
      else
        n=1
        n_ts_old=n_ts
      end if

      call GLOB_ABORT(max(n-num_sflx,0),
     >'number of surf. fluxes exceeds num_sflx in OC_RECVSBC',1)

      if (Coupler_id.lt.0) return     !   <- standalone mode

      IF ((nts/i_dtc2dto)*i_dtc2dto.eq.nts) THEN
        call CMP_gnr_RECV(F,NGP,MPI_kind_sfcflux)
        where (F.gt.too_low)
          sflx(:,:,n)=F*fctr
        elsewhere
          sflx(:,:,n)=F
        end where
      END IF
                                   
      WHERE (sflx(:,:,n).gt.too_low)
        SF=sflx(:,:,n)
      END WHERE

      call OC_ANNOUNCE('OC_RECV_SBC to return',3)

      return
      END
C
C***********************************************************************
C
      SUBROUTINE OC_DO_SBC(HF,SWR,nbc)

      USE OC_cc, ONLY: kind_sfcflux,NX,NY

      implicit none

      real(kind=kind_sfcflux),dimension(NX,NY):: HF,SWR
      integer,dimension(NX,NY):: nbc
C

      where (nbc.eq.1)
        HF=HF+SWR
      end where

      return
      END
C
C***********************************************************************
C
      SUBROUTINE OC_FINALIZE

      implicit none

      logical izd
      integer ierr
C

      call CMP_FINALIZE(izd,ierr)

      call GLOB_ABORT(ierr,'OC_FINALIZE: *** error ***',1)

      if (izd) then
        call OC_ANNOUNCE('OC_FINALIZE finished normally',2)
      else
        call OC_ANNOUNCE('OC_FINALIZE: wants to MPI_FINILIZE '//
     >  'but MPI is not initialized',1)
      end if

      return
      END
C
C***********************************************************************
C
