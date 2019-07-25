!@PROCESS NOEXTCHK
      SUBROUTINE MPI_FIRST
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    MPI_FIRST   SET UP MESSGAE PASSING INFO
C   PRGRMMR: TUCCILLO        ORG: IBM
C
C ABSTRACT:
C     SETS UP MESSAGE PASSING INFO
C   .
C
C PROGRAM HISTORY LOG:
C   00-01-06  TUCCILLO - ORIGINAL
C   01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
C   02-06-19  MIKE BALDWIN - WRF VERSION
C
C USAGE:    CALL MPI_FIRST
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST:
C
C   OUTPUT FILES:
C     STDOUT  - RUN TIME STANDARD OUT.
C
C   SUBPROGRAMS CALLED:
C       PARA_RANGE
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON - CTLBLK.comm
C
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : IBM RS/6000 SP
C$$$
c
      use vrbls3d
      use vrbls2d
      use soil
      use masks
c
!      include "parmeta"
      include "CTLBLK.comm"
      include "params"
!      include "parmsoil"
      include 'mpif.h'
c

c
!      PARAMETER (IMJM=IM*JM-JM/2)
c
      integer ierr
c
      if ( me .eq. 0 ) then
c        print *, ' NUM_PROCS = ',num_procs
      end if

      if ( num_procs .gt. 1024 ) then
         print *, ' too many MPI tasks, max is 1024, stopping'
         call mpi_abort(MPI_COMM_WORLD,1,ierr)
         stop
      end if
c
c     error check
c
      if ( num_procs .gt. JM/2 ) then
         print *, ' too many MPI tasks, max is ',jm/2,' stopping'
         call mpi_abort(MPI_COMM_WORLD,1,ierr)
         stop
      end if
c
c     global loop ranges
c
      call para_range(1,jm,num_procs,me,jsta,jend)
      jsta_m  = jsta
      jsta_m2 = jsta
      jend_m  = jend
      jend_m2 = jend
      if ( me .eq. 0 ) then
         jsta_m  = 2
         jsta_m2 = 3
      end if
      if ( me .eq. num_procs - 1 ) then
         jend_m  = jm - 1
         jend_m2 = jm - 2
      end if
c
c     neighbors
c
      iup = me + 1
      idn = me - 1
      if ( me .eq. 0 ) then
         idn = MPI_PROC_NULL
      end if
      if ( me .eq. num_procs - 1 ) then
         iup = MPI_PROC_NULL
      end if
C
c     print *, ' ME, NUM_PROCS = ',me,num_procs
c     print *, ' ME, JSTA, JSTA_M, JSTA_M2 = ',me,jsta,jsta_m,jsta_m2
c     print *, ' ME, JEND, JEND_M, JEND_M2 = ',me,jend,jend_m,jend_m2
c     print *, ' ME, IUP, IDN = ',me,iup,idn
c
c     counts, disps for gatherv and scatterv
c
      do i = 0, num_procs - 1
         call para_range(1,jm,num_procs,i,jsx,jex) 
         icnt(i) = (jex-jsx+1)*im
         idsp(i) = (jsx-1)*im
         if ( me .eq. 0 ) then
c           print *, ' i, icnt(i),idsp(i) = ',i,icnt(i),idsp(i)
         end if
      end do
c
c     extraction limits -- set to two rows    
c
      jsta_2l = max(jsta - 2,  1 )
      jend_2u = min(jend + 2, jm )
! special for c-grid v
      jvend_2u = min(jend + 2, jm+1 )
! special for c-grid v
c     print *, ' me, jsta_2l, jend_2u = ',me,jsta_2l, jend_2u
c     print *, ' me, jvend_2u = ',me,jvend_2u
c
c     allocate arrays
c
C
C     FROM VRBLS3D
C
      allocate(u(im,jsta_2l:jend_2u,lm))
      allocate(v(im,jsta_2l:jend_2u,lm))
      allocate(t(im,jsta_2l:jend_2u,lm))
! CHUANG ADD POTENTIAL TEMP BECAUSE WRF OUTPUT THETA
      allocate(th(im,jsta_2l:jend_2u,lm))   
      allocate(q(im,jsta_2l:jend_2u,lm))
!      allocate(w(im,jsta_2l:jend_2u,lp1))
      allocate(uh(im,jsta_2l:jend_2u,lm))
      allocate(vh(im,jsta_2l:jend_2u,lm))
      allocate(wh(im,jsta_2l:jend_2u,lm))
      allocate(pmid(im,jsta_2l:jend_2u,lm))
      allocate(pmidv(im,jsta_2l:jend_2u,lm))
      allocate(pint(im,jsta_2l:jend_2u,lp1))
      allocate(alpint(im,jsta_2l:jend_2u,lp1))
      allocate(zmid(im,jsta_2l:jend_2u,lm))
      allocate(zint(im,jsta_2l:jend_2u,lp1))
!      allocate(rainw(im,jsta_2l:jend_2u,lm))
      allocate(q2(im,jsta_2l:jend_2u,lm))
      allocate(omga(im,jsta_2l:jend_2u,lm))
      allocate(T_ADJ(im,jsta_2l:jend_2u,lm))
      allocate(ttnd(im,jsta_2l:jend_2u,lm))
      allocate(rswtt(im,jsta_2l:jend_2u,lm))
      allocate(rlwtt(im,jsta_2l:jend_2u,lm))
      allocate(exch_h(im,jsta_2l:jend_2u,lm)) 
      allocate(train(im,jsta_2l:jend_2u,lm))
      allocate(tcucn(im,jsta_2l:jend_2u,lm))
      allocate(el_myj(im,jsta_2l:jend_2u,lm))
C     MP FIELD   
      allocate(cwm(im,jsta_2l:jend_2u,lm))
      allocate(F_ice(im,jsta_2l:jend_2u,lm))
      allocate(F_rain(im,jsta_2l:jend_2u,lm))
      allocate(F_RimeF(im,jsta_2l:jend_2u,lm))
      allocate(QQW(im,jsta_2l:jend_2u,lm))
      allocate(QQI(im,jsta_2l:jend_2u,lm))
      allocate(QQR(im,jsta_2l:jend_2u,lm))
      allocate(QQS(im,jsta_2l:jend_2u,lm))
      allocate(QQG(im,jsta_2l:jend_2u,lm))
      allocate(CFR(im,jsta_2l:jend_2u,lm))
      allocate(DBZ(im,jsta_2l:jend_2u,lm))
      allocate(DBZR(im,jsta_2l:jend_2u,lm))
      allocate(DBZI(im,jsta_2l:jend_2u,lm))
      allocate(DBZC(im,jsta_2l:jend_2u,lm))
C
C     FROM SOIL
C
      allocate(smc(im,jsta_2l:jend_2u,nsoil))
      allocate(stc(im,jsta_2l:jend_2u,nsoil))
      allocate(sh2o(im,jsta_2l:jend_2u,nsoil))
      allocate(SLDPTH(NSOIL))
      allocate(RTDPTH(NSOIL))
C
C     FROM VRBLS2D
C
      allocate(u10(im,jsta_2l:jend_2u))
      allocate(v10(im,jsta_2l:jend_2u))
      allocate(tshltr(im,jsta_2l:jend_2u))
      allocate(qshltr(im,jsta_2l:jend_2u))
      allocate(smstav(im,jsta_2l:jend_2u))
      allocate(ssroff(im,jsta_2l:jend_2u))
      allocate(bgroff(im,jsta_2l:jend_2u))
      allocate(vegfrc(im,jsta_2l:jend_2u))
      allocate(acsnow(im,jsta_2l:jend_2u))
      allocate(acsnom(im,jsta_2l:jend_2u))
      allocate(cmc(im,jsta_2l:jend_2u))
      allocate(sst(im,jsta_2l:jend_2u))
      allocate(qz0(im,jsta_2l:jend_2u))
      allocate(thz0(im,jsta_2l:jend_2u))
      allocate(uz0(im,jsta_2l:jend_2u))
      allocate(vz0(im,jsta_2l:jend_2u))
      allocate(qs(im,jsta_2l:jend_2u))
      allocate(ths(im,jsta_2l:jend_2u))
      allocate(sno(im,jsta_2l:jend_2u))
      allocate(akms(im,jsta_2l:jend_2u))
      allocate(akhs(im,jsta_2l:jend_2u))
      allocate(cuprec(im,jsta_2l:jend_2u))
      allocate(acprec(im,jsta_2l:jend_2u))
      allocate(ancprc(im,jsta_2l:jend_2u))
      allocate(cuppt(im,jsta_2l:jend_2u))
      allocate(rswin(im,jsta_2l:jend_2u))
      allocate(rlwin(im,jsta_2l:jend_2u))
      allocate(rlwtoa(im,jsta_2l:jend_2u))
      allocate(tg(im,jsta_2l:jend_2u))
      allocate(sfcshx(im,jsta_2l:jend_2u))
      allocate(sfclhx(im,jsta_2l:jend_2u))
      allocate(fis(im,jsta_2l:jend_2u))
      allocate(fi(im,jsta_2l:jend_2u,2))
      allocate(t500(im,jsta_2l:jend_2u))
      allocate(cfracl(im,jsta_2l:jend_2u))
      allocate(cfracm(im,jsta_2l:jend_2u))
      allocate(cfrach(im,jsta_2l:jend_2u))
      allocate(acfrst(im,jsta_2l:jend_2u))
      allocate(acfrcv(im,jsta_2l:jend_2u))
      allocate(hbot(im,jsta_2l:jend_2u))
      allocate(htop(im,jsta_2l:jend_2u))
      allocate(aswin(im,jsta_2l:jend_2u))
      allocate(alwin(im,jsta_2l:jend_2u))
      allocate(aswout(im,jsta_2l:jend_2u))
      allocate(alwout(im,jsta_2l:jend_2u))
      allocate(aswtoa(im,jsta_2l:jend_2u))
      allocate(alwtoa(im,jsta_2l:jend_2u))
      allocate(czen(im,jsta_2l:jend_2u))
      allocate(czmean(im,jsta_2l:jend_2u))
      allocate(sigt4(im,jsta_2l:jend_2u))
      allocate(rswout(im,jsta_2l:jend_2u))
      allocate(radot(im,jsta_2l:jend_2u))
      allocate(ncfrst(im,jsta_2l:jend_2u))  ! real
      allocate(ncfrcv(im,jsta_2l:jend_2u))  ! real
      allocate(smstot(im,jsta_2l:jend_2u))
      allocate(pctsno(im,jsta_2l:jend_2u))
      allocate(pshltr(im,jsta_2l:jend_2u))
      allocate(th10(im,jsta_2l:jend_2u))
      allocate(q10(im,jsta_2l:jend_2u))
      allocate(sr(im,jsta_2l:jend_2u))
      allocate(prec(im,jsta_2l:jend_2u))
      allocate(subshx(im,jsta_2l:jend_2u))
      allocate(snopcx(im,jsta_2l:jend_2u))
      allocate(sfcuvx(im,jsta_2l:jend_2u))
      allocate(sfcevp(im,jsta_2l:jend_2u))
      allocate(potevp(im,jsta_2l:jend_2u))
      allocate(z0(im,jsta_2l:jend_2u))
      allocate(ustar(im,jsta_2l:jend_2u))
      allocate(pblh(im,jsta_2l:jend_2u))
      allocate(twbs(im,jsta_2l:jend_2u))
      allocate(qwbs(im,jsta_2l:jend_2u))
      allocate(sfcexc(im,jsta_2l:jend_2u))
      allocate(grnflx(im,jsta_2l:jend_2u))
      allocate(soiltb(im,jsta_2l:jend_2u))
      allocate(z1000(im,jsta_2l:jend_2u))
      allocate(slp(im,jsta_2l:jend_2u))
      allocate(pslp(im,jsta_2l:jend_2u))
      allocate(f(im,jsta_2l:jend_2u))
      allocate(albedo(im,jsta_2l:jend_2u))
      allocate(albase(im,jsta_2l:jend_2u))
      allocate(cldfra(im,jsta_2l:jend_2u))
      allocate(cprate(im,jsta_2l:jend_2u))
      allocate(cnvcfr(im,jsta_2l:jend_2u))
      allocate(ivgtyp(im,jsta_2l:jend_2u))
      allocate(isltyp(im,jsta_2l:jend_2u))
      allocate(hbotd(im,jsta_2l:jend_2u))
      allocate(htopd(im,jsta_2l:jend_2u))
      allocate(hbots(im,jsta_2l:jend_2u))
      allocate(htops(im,jsta_2l:jend_2u))
      allocate(cldefi(im,jsta_2l:jend_2u))
      allocate(islope(im,jsta_2l:jend_2u))
      allocate(si(im,jsta_2l:jend_2u))
      allocate(lspa(im,jsta_2l:jend_2u))
      allocate(rswinc(im,jsta_2l:jend_2u))
      allocate(vis(im,jsta_2l:jend_2u))
      allocate(pd(im,jsta_2l:jend_2u))
      allocate(mxsnal(im,jsta_2l:jend_2u))
C
C     FROM MASKS
C
      allocate(hbm2(im,jsta_2l:jend_2u))
      allocate(sm(im,jsta_2l:jend_2u))
      allocate(sice(im,jsta_2l:jend_2u))
      allocate(lmh(im,jsta_2l:jend_2u))  ! real
      allocate(lmv(im,jsta_2l:jend_2u))  ! real
      allocate(gdlat(im,jsta_2l:jend_2u))
      allocate(gdlon(im,jsta_2l:jend_2u))
      allocate(dx(im,jsta_2l:jend_2u))
      allocate(dy(im,jsta_2l:jend_2u))
      allocate(htm(im,jsta_2l:jend_2u,lm))
      allocate(vtm(im,jsta_2l:jend_2u,lm))
      allocate(vdlat(im,jsta_2l:jend_2u))
      allocate(vdlon(im,jsta_2l:jend_2u))
      allocate(znt1(im,jsta_2l:jend_2u))
C
      end
