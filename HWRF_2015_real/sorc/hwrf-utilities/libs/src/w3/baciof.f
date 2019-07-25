C-----------------------------------------------------------------------
      MODULE BACIO_MODULE
C$$$  F90-MODULE DOCUMENTATION BLOCK
C
C F90-MODULE: BACIO_MODULE   BYTE-ADDRESSABLE I/O MODULE
C   PRGMMR: IREDELL          ORG: NP23        DATE: 98-06-04
C
C ABSTRACT: MODULE TO SHARE FILE DESCRIPTORS
C   IN THE BYTE-ADDESSABLE I/O PACKAGE.
C
C PROGRAM HISTORY LOG:
C   98-06-04  IREDELL
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      INTEGER,EXTERNAL:: BACIO
      INTEGER,DIMENSION(999),SAVE:: FD=999*0
      INTEGER,DIMENSION(20),SAVE:: BAOPTS=0
      INCLUDE 'baciof.h'
      END
C-----------------------------------------------------------------------
      SUBROUTINE BASETO(NOPT,VOPT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BASETO         BYTE-ADDRESSABLE SET OPTIONS
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: SET OPTIONS FOR BYTE-ADDRESSABLE I/O.
C   ALL OPTIONS DEFAULT TO 0.
C   OPTION 1: BLOCKED READING OPTION
C             IF THE OPTION VALUE IS 1, THEN THE READING IS BLOCKED
C             INTO FOUR 4096-BYTE BUFFERS.  THIS MAY BE EFFICIENT IF
C             THE READS WILL BE REQUESTED IN MUCH SMALLER CHUNKS.
C             OTHERWISE, EACH CALL TO BAREAD INITIATES A PHYSICAL READ.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BASETO(NOPT,VOPT)
C   INPUT ARGUMENTS:
C     NOPT         INTEGER OPTION NUMBER
C     VOPT         INTEGER OPTION VALUE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      INTEGER NOPT,VOPT
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(NOPT.GE.1.AND.NOPT.LE.20) BAOPTS(NOPT)=VOPT
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE BAOPEN(LU,CFN,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAOPEN         BYTE-ADDRESSABLE OPEN
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BAOPEN(LU,CFN,IRET)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO OPEN
C     CFN          CHARACTER FILENAME TO OPEN
C                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
C   OUTPUT ARGUMENTS:
C     IRET         INTEGER RETURN CODE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      CHARACTER CFN*(*)
      CHARACTER(80) CMSG
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.999) THEN
        IRET=6
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIO(BACIO_OPENRW,IB,JB,1,NB,KA,FD(LU),CFN//CHAR(0),A)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE BAOPENR(LU,CFN,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAOPENR        BYTE-ADDRESSABLE OPEN
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE FOR READ ONLY.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BAOPENR(LU,CFN,IRET)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO OPEN
C     CFN          CHARACTER FILENAME TO OPEN
C                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
C   OUTPUT ARGUMENTS:
C     IRET         INTEGER RETURN CODE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      CHARACTER CFN*(*)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.999) THEN
        IRET=6
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIO(BACIO_OPENR,IB,JB,1,NB,KA,FD(LU),CFN//CHAR(0),A)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE BAOPENW(LU,CFN,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAOPENW        BYTE-ADDRESSABLE OPEN
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE FOR WRITE ONLY.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BAOPENW(LU,CFN,IRET)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO OPEN
C     CFN          CHARACTER FILENAME TO OPEN
C                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
C   OUTPUT ARGUMENTS:
C     IRET         INTEGER RETURN CODE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      CHARACTER CFN*(*)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.999) THEN
        IRET=6
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIO(BACIO_OPENW,IB,JB,1,NB,KA,FD(LU),CFN//CHAR(0),A)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE BAOPENWT(LU,CFN,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAOPENWT       BYTE-ADDRESSABLE OPEN
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE FOR WRITE ONLY WITH TRUNCATION.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BAOPENWT(LU,CFN,IRET)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO OPEN
C     CFN          CHARACTER FILENAME TO OPEN
C                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
C   OUTPUT ARGUMENTS:
C     IRET         INTEGER RETURN CODE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      CHARACTER CFN*(*)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.999) THEN
        IRET=6
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIO(BACIO_OPENWT,IB,JB,1,NB,KA,FD(LU),CFN//CHAR(0),A)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE BAOPENWA(LU,CFN,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAOPENWA       BYTE-ADDRESSABLE OPEN
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE FOR WRITE ONLY WITH APPEND.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BAOPENWA(LU,CFN,IRET)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO OPEN
C     CFN          CHARACTER FILENAME TO OPEN
C                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
C   OUTPUT ARGUMENTS:
C     IRET         INTEGER RETURN CODE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      CHARACTER CFN*(*)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.999) THEN
        IRET=6
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIO(BACIO_OPENWA,IB,JB,1,NB,KA,FD(LU),CFN//CHAR(0),A)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE BACLOSE(LU,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BACLOSE        BYTE-ADDRESSABLE CLOSE
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: CLOSE A BYTE-ADDRESSABLE FILE.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BACLOSE(LU,IRET)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO CLOSE
C   OUTPUT ARGUMENTS:
C     IRET         INTEGER RETURN CODE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.999) THEN
        IRET=6
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIO(BACIO_CLOSE,IB,JB,1,NB,KA,FD(LU),CHAR(0),A)
      IF(IRET.EQ.0) FD(LU)=0
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE BAREAD(LU,IB,NB,KA,A)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAREAD         BYTE-ADDRESSABLE READ
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: READ A GIVEN NUMBER OF BYTES FROM AN UNBLOCKED FILE,
C   SKIPPING A GIVEN NUMBER OF BYTES.
C   THE PHYSICAL I/O IS BLOCKED INTO FOUR 4096-BYTE BUFFERS
C   IF THE BYTE-ADDRESSABLE OPTION 1 HAS BEEN SET TO 1 BY BASETO.
C   THIS BUFFERED READING IS INCOMPATIBLE WITH NO-SEEK READING.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BAREAD(LU,IB,NB,KA,A)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO READ
C     IB           INTEGER NUMBER OF BYTES TO SKIP
C                  (IF IB<0, THEN THE FILE IS ACCESSED WITH NO SEEKING)
C     NB           INTEGER NUMBER OF BYTES TO READ
C   OUTPUT ARGUMENTS:
C     KA           INTEGER NUMBER OF BYTES ACTUALLY READ
C     A            CHARACTER*1 (NB) DATA READ
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      CHARACTER A(NB)
      CHARACTER CFN
      PARAMETER(NY=4096,MY=4)
      INTEGER NS(MY),NN(MY)
      CHARACTER Y(NY,MY)
      DATA LUX/0/
      SAVE JY,NS,NN,Y,LUX
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(FD(LU).LE.0) THEN
        KA=0
        RETURN
      ENDIF
      IF(IB.LT.0.AND.BAOPTS(1).EQ.1) THEN
        KA=0
        RETURN
      ENDIF
      IF(NB.LE.0) THEN
        KA=0
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  UNBUFFERED I/O
      IF(BAOPTS(1).NE.1) THEN
        IF(IB.GE.0) THEN
          IRET=BACIO(BACIO_READ,IB,JB,1,NB,KA,FD(LU),CFN//CHAR(0),A)
        ELSE
          IRET=BACIO(BACIO_READ+BACIO_NOSEEK,0,JB,1,NB,
     &               KA,FD(LU),CFN//CHAR(0),A)
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  BUFFERED I/O
C  GET DATA FROM PREVIOUS CALL IF POSSIBLE
      ELSE
        KA=0
        IF(LUX.NE.LU) THEN
          JY=0
          NS=0
          NN=0
        ELSE
          DO I=1,MY
            IY=MOD(JY+I-1,MY)+1
            KY=IB+KA-NS(IY)
            IF(KA.LT.NB.AND.KY.GE.0.AND.KY.LT.NN(IY)) THEN
              K=MIN(NB-KA,NN(IY)-KY)
              A(KA+1:KA+K)=Y(KY+1:KY+K,IY)
              KA=KA+K
            ENDIF
          ENDDO
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SET POSITION AND READ BUFFER AND GET DATA
        IF(KA.LT.NB) THEN
          LUX=ABS(LU)
          JY=MOD(JY,MY)+1
          NS(JY)=IB+KA
          IRET=BACIO(BACIO_READ,NS(JY),JB,1,NY,NN(JY),
     &               FD(LUX),CFN//CHAR(0),Y(1,JY))
          IF(NN(JY).GT.0) THEN
            K=MIN(NB-KA,NN(JY))
            A(KA+1:KA+K)=Y(1:K,JY)
            KA=KA+K
          ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CONTINUE TO READ BUFFER AND GET DATA
          DOWHILE(NN(JY).EQ.NY.AND.KA.LT.NB)
            JY=MOD(JY,MY)+1
            NS(JY)=NS(JY)+NN(JY)
            IRET=BACIO(BACIO_READ+BACIO_NOSEEK,NS(JY),JB,1,NY,NN(JY),
     &                 FD(LUX),CFN//CHAR(0),Y(1,JY))
            IF(NN(JY).GT.0) THEN
              K=MIN(NB-KA,NN(JY))
              A(KA+1:KA+K)=Y(1:K,JY)
              KA=KA+K
            ENDIF
          ENDDO
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE BAWRITE(LU,IB,NB,KA,A)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAWRITE        BYTE-ADDRESSABLE WRITE
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: WRITE A GIVEN NUMBER OF BYTES TO AN UNBLOCKED FILE,
C   SKIPPING A GIVEN NUMBER OF BYTES.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BAWRITE(LU,IB,NB,KA,A)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO WRITE
C     IB           INTEGER NUMBER OF BYTES TO SKIP
C                  (IF IB<0, THEN THE FILE IS ACCESSED WITH NO SEEKING)
C     NB           INTEGER NUMBER OF BYTES TO WRITE
C     A            CHARACTER*1 (NB) DATA TO WRITE
C   OUTPUT ARGUMENTS:
C     KA           INTEGER NUMBER OF BYTES ACTUALLY WRITTEN
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      CHARACTER A(NB)
      CHARACTER CFN
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(FD(LU).LE.0) THEN
        KA=0
        RETURN
      ENDIF
      IF(NB.LE.0) THEN
        KA=0
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(IB.GE.0) THEN
        IRET=BACIO(BACIO_WRITE,IB,JB,1,NB,KA,FD(LU),CHAR(0),A)
      ELSE
        IRET=BACIO(BACIO_WRITE+BACIO_NOSEEK,0,JB,1,NB,KA,
     &             FD(LU),CHAR(0),A)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE WRYTE(LU,NB,A)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: WRYTE          WRITE DATA OUT BY BYTES
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: WRITE A GIVEN NUMBER OF BYTES TO AN UNBLOCKED FILE.
C
C PROGRAM HISTORY LOG:
C   92-10-31  IREDELL
C   95-10-31  IREDELL     WORKSTATION VERSION
C   1998-06-04  IREDELL   BACIO VERSION
C
C USAGE:    CALL WRYTE(LU,NB,A)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO WHICH TO WRITE
C     NB           INTEGER NUMBER OF BYTES TO WRITE
C     A            CHARACTER*1 (NB) DATA TO WRITE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      CHARACTER A(NB)
      CHARACTER CFN
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(FD(LU).LE.0) THEN
        RETURN
      ENDIF
      IF(NB.LE.0) THEN
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIO(BACIO_WRITE+BACIO_NOSEEK,0,JB,1,NB,KA,FD(LU),
     &           CHAR(0),A)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
