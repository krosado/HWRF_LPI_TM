      SUBROUTINE etaPTS(ZLATB,ZLONB,NHPT,
     &                 ULATB,ULONB,NUPT,
     &                 VLATB,VLONB,NVPT,
     &                 HLAT,HLON,VLAT,VLON,
     & IM,JM,LM,KB,KBETA,IMJM,JMP1,IMM1,IMJMM1,
     & KHL00,KHH00,KNE,
     & KNW,KSW,KSE,KSL,KSLM1,LMM1,LMP1,IMT,JMT,DLMD,DPHD) !Zhang
      implicit none
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM: etaPTS        FINDS LAT/LONG OF ETA BOUNDARY POINTS
C
C   PRGMMR: E. ROGERS       ORG: W/NMC22    DATE: 90-06-19
C
C ABSTRACT: COMPUTES THE EARTH LATITUDE AND LONGITUDE OF ETA GRID
C   POINTS (BOTH H AND V POINTS) AND STRIPS OFF THE BOUNDARY POINTS
C
C PROGRAM HISTORY LOG:
C   90-06-19  E.ROGERS
C
C USAGE:    CALL etaPTS(ZLATB,ZLONB,NHPT,ULATB,ULONB,NUPT,VLATB,VLONB,
C                      NVPT)
C   INPUT ARGUMENT LIST:
C     NONE
C
C   OUTPUT ARGUMENT LIST:
C     ZLATB    - LATITUDE OF H BOUNDARY POINTS IN degrees (NEG=S)
C     ZLONB    - LONGITUDE OF H BOUNDARY POINTS IN degrees (W)
C     NHPT     - NUMBER OF H BOUNDARY POINTS
C     ULATB    - LATITUDE OF U BOUNDARY POINTS IN degrees (NEG=S)
C     ULONB    - LONGITUDE OF U BOUNDARY POINTS IN degrees (W)
C     NUPT     - NUMBER OF U BOUNDARY POINTS
C     VLATB    - LATITUDE OF V BOUNDARY POINTS IN degrees (NEG=S)
C     VLONB    - LONGITUDE OF V BOUNDARY POINTS IN degrees (W)
C     NVPT     - NUMBER OF V BOUNDARY POINTS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
      integer,parameter::real_32=selected_real_kind(6,30)
      include "parmlbc"
C-----------------------------------------------------------------------
      integer ::  KHL0  (JM),KHH0  (JM), KVL0 (JM), KVH0 (JM)
C
      real :: HLAT (IMJM),HLON (IMJM),VLAT (IMJM),VLON (IMJM)
      real :: ZLATB(KB), ZLONB(KB), ULATB(KB), ULONB(KB)
      real :: VLATB(KB), VLONB(KB)
      integer :: i,j,k, khl,khh,kvl,kvh,n,KHSTWB,KHENWB,KHICWB
      integer :: KHSTEB,KHENEB,KHICEB,jj,kk,nupt,nvpt,nxpt,nhpt
      integer :: KVSTWB, KVENWB, KVICWB, KVSTEB, KVENEB, KVICEB
      double precision ::GLATH(IMJM),GLONH(IMJM),GLATV(IMJM),GLONV(IMJM)
      double precision :: DTR, WBD, SBD, TPH0, WB, SB, DLM, DPH, TDLM
      double precision :: TDPH, STPH0, CTPH0, TLMH0, TPHH, TPHV, TLMV0
      double precision :: STPH, CTPH, STPV, CTPV, TLMH, SPHH, CPHH
      double precision :: CLMH, FACTH, TPH0D,TLM0D, PI, TLMV, SPHV, CPHV
      double precision :: FACTV, CLMV
      real DLMD,DPHD
C
C       REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::GLATH,GLONH,GLATV,GLONV
C       REAL(REAL_32),ALLOCATABLE,DIMENSION(:)::HLAT,HLON,VLAT,VLON
C
c                             D A T A
C     &  LIST  /06/
C--------------HORIZONTAL GRID CONSTANTS - STANDARD RESOLUTION----------
C--------------HORIZONTAL GRID CONSTANTS - come from parameter ---------
C                            D A T A
C    & TLM0D/-100.0/,TPH0D/50.0/,WBD/-37.5/,SBD/-35.0/
C    &,DLMD/.576923077/,DPHD/.538461539/
C--------------HORIZONTAL GRID CONSTANTS - DOUBLE RESOLUTION------------
C                            D A T A
C    & TLM0D/-100.0/,TPH0D/52.5/,WBD/-37.5/,SBD/-35./
C    &,DLMD/.288461538/,DPHD/.269230769/
C--------------UNIVERSAL CONSTANTS--------------------------------------
                             D A T A
     & PI/3.141592654/
C-----------------------------------------------------------------------
 1000 FORMAT(' K=',I4,' GLAT=',E12.5,' GLON=',E12.5,' ELAT=',E12.5
     &,' ELON=',E12.5,' WLON = ',E12.5)
C----------------------------------------------------------------------
C--------------DERIVED GEOMETRICAL CONSTANTS----------------------------
C----------------------------------------------------------------------
      REWIND 45
      READ(45,*) TPH0D
      READ(45,*) TLM0D

      DTR = PI / 180.0
      WBD=-(((IM-1))*DLMD)       !KWON FROM module_initialize_real_bin.F IM-1 instead of (IM-1)-1
      SBD=-(((JM-1)/2)*DPHD)     !KWON FROM module_initialize_real_bin.F

      TPH0 = TPH0D * DTR
      WB = WBD * DTR
      SB = SBD * DTR
      DLM = DLMD * DTR
      DPH = DPHD * DTR
      TDLM = DLM + DLM
      TDPH = DPH + DPH
C
      STPH0 = SIN(TPH0)
      CTPH0 = COS(TPH0)
C
      DO 100 J = 1, JM
        KHL0(J) = IM * (J-1) - (J-1)/2 + 1
        KHH0(J) = IM * J - J / 2
        IF(MOD(J,2) .EQ. 0) THEN
          KVL0(J) = KHL0(J) - 1
          KVH0(J) = KHH0(J)
        ELSE
          KVL0(J) = KHL0(J)
          KVH0(J) = KHH0(J) - 1
        END IF
C       WRITE(LIST,20) J,KHL0(J),KHH0(J),KVL0(J),KVH0(J)
C  20 FORMAT(2X,5I10)
  100 CONTINUE
C-----------------------------------------------------------------------
C---COMPUTE GEOGRAPHIC LAT AND LONG OF ETA GRID POINTS (H & V POINTS)---
C-----------------------------------------------------------------------
C      ALLOCATE(GLATH(IMJM))
C      ALLOCATE(GLONH(IMJM))
C      ALLOCATE(HLAT(IMJM))
C      ALLOCATE(HLON(IMJM))
C      ALLOCATE(GLATV(IMJM))
C      ALLOCATE(GLONV(IMJM))
C      ALLOCATE(VLAT(IMJM))
C      ALLOCATE(VLON(IMJM))
C
      TPHH = SB - DPH
      DO 200 J = 1, JM
         KHL = KHL0(J)
         KHH = KHH0(J)
         KVL = KVL0(J)
         KVH = KVH0(J)
C
         TLMH0 = WB - TDLM + MOD(J+1,2) * DLM
         TPHH = SB + (J-1)*DPH
         TLMV0 = WB - TDLM + MOD(J,2) * DLM
         TPHV = TPHH
         STPH = SIN(TPHH)
         CTPH = COS(TPHH)
         STPV = SIN(TPHV)
         CTPV = COS(TPHV)
C----------------------------------------------------------------------
C---------- COMPUTE EARTH LATITUDE/LONGITUDE OF H POINTS --------------
C----------------------------------------------------------------------
         DO 201 K = KHL, KHH
           I=K-KHL+1
           TLMH = TLMH0 + I*TDLM
           SPHH = CTPH0 * STPH + STPH0 * CTPH * COS(TLMH)
           CPHH = sqrt(1-SPHH**2)
           GLATH(K) = ASIN(SPHH)
           CLMH = (CTPH*COS(TLMH)-SPHH*STPH0) / (CPHH*CTPH0)

           IF(CLMH .GT. 1.) CLMH = 1.0
           IF(CLMH .LT. -1.) CLMH = -1.0
           FACTH = 1.
           IF(TLMH .GT. 0.) FACTH = -1.
           GLONH(K) = -TLM0D * DTR + FACTH * ACOS(CLMH)
C          IF(K .EQ. 1. OR. MOD(K,50). EQ. 0) THEN
C           WRITE(LIST,99995) J,K,GLATH(K),GLONH(K)
C9995       FORMAT(2X,2(I6,1X),2(E12.5,1X))
C          END IF
C         w a s:
C    CONVERT INTO DEGREES AND EAST LONGITUDE
C     n o w:  we leave in degrees positive WEST!
C
           HLAT(K) = GLATH(K) / DTR
c east     HLON(K) = 360.0 - GLONH(K) / DTR
           HLON(K) =         GLONH(K) / DTR
           IF(HLON(K) .GT. 360.) HLON(K) = HLON(K) - 360.
           IF(HLON(K) .LT.  0. ) HLON(K) = HLON(K) + 360.
  201    CONTINUE
C----------------------------------------------------------------------
C---------- COMPUTE EARTH LATITUDE/LONGITUDE OF V POINTS --------------
C----------------------------------------------------------------------
         DO 202 K = KVL, KVH
           I=K-KVL+1
           TLMV = TLMV0 + I*TDLM
           SPHV = CTPH0 * STPV + STPH0 * CTPV * COS(TLMV)
           CPHV = sqrt(1-SPHV**2)
           GLATV(K) = ASIN(SPHV)
           CLMV = (CTPV*COS(TLMV)-SPHV*STPH0) / (CPHV*CTPH0)
           IF(CLMV .GT. 1.) CLMV = 1.
           IF(CLMV .LT. -1.) CLMV = -1.
           FACTV = 1.
           IF(TLMV .GT. 0.) FACTV = -1.
           GLONV(K) = -TLM0D * DTR + FACTV * ACOS(CLMV)
C          IF(K .EQ. 1. OR. MOD(K,50). EQ. 0) THEN
C           WRITE(LIST,99995) J,K,GLATV(K),GLONV(K)
C          END IF
C         w a s:
C    CONVERT INTO DEGREES AND EAST LONGITUDE
C     n o w:  we leave in degrees positive WEST!
C
           VLAT(K) = GLATV(K) / DTR
c east     VLON(K) = 360.0 - GLONV(K) / DTR
           VLON(K) =         GLONV(K) / DTR
           IF(VLON(K) .GT. 360.) VLON(K) = VLON(K) - 360.
           IF(VLON(K) .LT.  0. ) VLON(K) = VLON(K) + 360.
  202    CONTINUE
  200 CONTINUE
C
C      DEALLOCATE(GLATH)
C      DEALLOCATE(GLONH)
C      DEALLOCATE(GLATV)
C      DEALLOCATE(GLONV)
C
C     DO 210 K = KHL00, KHH00
C      IF(K .LT. 100 .OR. MOD(K,100) .EQ. 0) THEN
C        WRITE(LIST,88888) K,HLAT(K),HLON(K),VLAT(K),VLON(K)
C8888    FORMAT(2X,I5,1X,4(E12.5,1X))
C      END IF
C 210 CONTINUE
C
C------------- THE SEPARATION OF THE BOUNDARY VALUES -------------------
C
C       SOUTH BOUNDARY - H POINTS
C
      N = 1
      DO 541 K = 1, IM
         ZLATB(N) = HLAT(K)
         ZLONB(N) = HLON(K)
         N = N + 1
 541  CONTINUE
C
C       NORTH BOUNDARY - H POINTS
C
      DO 543 K = KSL, IMJM
         ZLATB(N) = HLAT(K)
         ZLONB(N) = HLON(K)
         N = N + 1
 543  CONTINUE
C
C       WEST BOUNDARY - H POINTS
C
      KHSTWB= 2 * KNE
      KHENWB= KSL+KSW+KSE
      KHICWB= KNW+KNE
      DO 545 K = KHSTWB,KHENWB,KHICWB
         ZLATB(N) = HLAT(K)
         ZLONB(N) = HLON(K)
      N = N + 1
 545  CONTINUE
C
C       EAST BOUNDARY - H POINTS
C 
      KHSTEB= 2*KNE + KNW
      KHENEB= IMJM + KSW +KSE
      KHICEB= KNW+KNE
      DO 547 K = KHSTEB,KHENEB,KHICEB
         ZLATB(N) = HLAT(K)
         ZLONB(N) = HLON(K)
          N = N + 1
 547  CONTINUE
      NHPT = N - 1
C
C       SOUTH BOUNDARY - V POINTS
C
      N = 1
      DO 549 K = 1, IMM1
            ULATB(N) = VLAT(K)
            ULONB(N) = VLON(K)
            VLATB(N) = VLAT(K)
            VLONB(N) = VLON(K)
         N = N + 1
 549  CONTINUE
C
C       NORTH BOUNDARY - V POINTS
C
      DO 551 K = KSL, IMJMM1
            ULATB(N) = VLAT(K)
            ULONB(N) = VLON(K)
            VLATB(N) = VLAT(K)
            VLONB(N) = VLON(K)
 550     CONTINUE
         N = N + 1
 551  CONTINUE
C
C       WEST BOUNDARY - V POINTS
C 
      KVSTWB= IM
      KVENWB= KSL + KSW
      KVICWB= KNE + KNW
      DO 553 K = KVSTWB,KVENWB,KVICWB
            ULATB(N) = VLAT(K)
            ULONB(N) = VLON(K)
            VLATB(N) = VLAT(K)
            VLONB(N) = VLON(K)
         N = N + 1
 553  CONTINUE
C
C       EAST BOUNDARY - V POINTS
C 
      KVSTEB= KNE + KNW
      KVENEB= KSLM1 
      KVICEB= KNE + KNW
      DO 555 K = KVSTEB,KVENEB,KVICEB
            ULATB(N) = VLAT(K)
            ULONB(N) = VLON(K)
            VLATB(N) = VLAT(K)
            VLONB(N) = VLON(K)
         N = N + 1
 555  CONTINUE
      NUPT = N - 1
      NVPT = NUPT
C
C      DEALLOCATE(HLAT)
C      DEALLOCATE(HLON)
C      DEALLOCATE(VLAT)
C      DEALLOCATE(VLON)
C
      WRITE(6,*) 'NHPT NUPT NVPT at ETAPTS ',NHPT,NUPT,NVPT
C
C.. WRITE ZLONB ZLATB FOR COMPARISON   !YC KWON
C 
      OPEN(987,FILE='zlatb-zlonb-etapts',STATUS='UNKNOWN')
      WRITE(987,988) KSL,IMJM,KB,WBD,SBD
988   FORMAT(1X,'KSL = ',I6,3X,'IMJM = ',I6,3X,'KB = ',I4,2F10.4)
      DO JJ = 1,KB
      WRITE(987,989) JJ,ZLATB(JJ),ZLONB(JJ)
989   FORMAT(1X,I4,2X,F10.4,2X,F10.4)
      ENDDO
C
C.. WRITE HLAT HLON FOR COMPARISON   !YCKWON
C
      OPEN(876,FILE='hlat-hlon-etapts',STATUS='UNKNOWN')
      DO KK = 1,IMJM
      WRITE(876,877) KK,HLAT(KK),HLON(KK)
877   FORMAT(1X,I5,1X,2(F10.4,1X))
      ENDDO
C
C.. COPY VLON (IMJM-1) TO VLON(IMJM) 
C.. COPY VLAT (IMJM-1) TO VLAT(IMJM) FOR CONSISTENCY
C..              YC KWON
C
      VLON(IMJM) = VLON(IMJM-1)
      VLAT(IMJM) = VLAT(IMJM-1)
C
C     diagnostic print
      NXPT = max(NHPT,NUPT) 
      NXPT = max(NXPT,NVPT) 
      WRITE(6,77776) NHPT, NUPT, NVPT
77776 FORMAT(2X,3(I6,1X))
      DO 77777 N = 1, NXPT, 4
       WRITE(6,77778) N,ZLATB(N),ZLONB(N),ULATB(N),ULONB(N)
77778  FORMAT(I5,4(1X,F11.4))
77777 CONTINUE
         r  e  t  u  r  n
      END
