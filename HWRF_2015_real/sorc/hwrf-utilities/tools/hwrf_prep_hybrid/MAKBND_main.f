      PROGRAM MAKBND_main

      INTEGER :: IM, JM, LM, KB, KBETA, IMJM, JMP1, IMM1, IMJMM1,
     & KHL00,KHH00,KNE,KNW,KSW,KSE,KSL,KSLM1,narg,IMAX,JMAX
      character(len=200) :: arg
      real DLMD,DPHD
      INTEGER :: nthreads

C set default parameters
      IM=216
      JM=432
      LM=43
      DLMD=0.18
      DPHD=0.18
      IMAX=1440
      JMAX=721

C command line arguments over-ride defaults
      narg=0
      narg = iargc()
      if (narg .eq. 7) then
          call getarg(1,arg)
          read(arg,'(i5)') IM
          call getarg(2,arg)
          read(arg,'(i5)') JM
          call getarg(3,arg)
          read(arg,'(i5)') LM
          call getarg(4,arg)
          read(arg,'(f10.6)') DLMD
          call getarg(5,arg)
          read(arg,'(f10.6)') DPHD
          call getarg(6,arg)
          read(arg,'(i5)') IMAX
          call getarg(7,arg)
          read(arg,'(i5)') JMAX
      endif

      IM=IM-1
      JM=JM-1
      LM=LM-1
      print*, "Input domain size",im,jm,lm
      print*, "Input Resolution"
      print'(f10.6)', DLMD,DPHD
      print*, "Intermediate resolution",imax,jmax
      KB=JM+2*IM-3
      KBETA=JM+2*IM-3
      IMJM=IM*JM-JM/2
      JMP1=JM+1
      IMM1=IM-1
      IMJMM1=IMJM-1
      KHL00=1
      KHH00=IM*JM-JM/2
      KNE=IM
      KNW=IM-1
      KSW=-IM
      KSE=-IM+1
      KSL=IMJM+KSE
      KSLM1=KSL-1

      LMM1=LM-1
      LMP1=LM+1
      IMT=2*IM-1
      JMT=JM/2+1
      call MAKBND(IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,
     & KHL00,KHH00,KNE,KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT
     & ,JMT,DLMD,DPHD,IMAX,JMAX,narg)
      stop
      end
