	
	
      subroutine avnglatlon(iomax,jomax,jmaxd2)
      real XO(IOMAX),XON(IOMAX),DLAM(IOMAX)
      real THETO(jomax),YO(jomax),COLAT(jmaxd2),
     &     COSCL(jmaxd2),SINCL(jmaxd2),DUMWT(jmaxd2)

C
C ABSTRACT: This program calculate AVN gaussian latitude and longitude.
C
C
C PROGRAM HISTORY LOG:
C   01-27-02  Biju Thomas original writing
C

C Initialise local variables
      XO = 0.0
      XON = 0.0
      DLAM = 0.0
      THETO = 0.0
      YO = 0.0
      COLAT = 0.0
      COSCL = 0.0
      DUMWT = 0.0
      SINCL = 0.0
      COLAT = 0.0

      PI=4.*ATAN(1.)
      PI180=PI/180.
      PI2=PI/2.
      TLAP=6.7E-5
      GGG=980.6
C     COMPUTE GAUSSIAN LATS-LONGS   *******
      DELXO=360./FLOAT(IOMAX)
      IMXXX=IOMAX
      DO 30 I=1,IMXXX
      XO(I)=FLOAT(I-1)*DELXO
      DLAM(I)=DELXO*PI180
   30 CONTINUE
      
       DO I=1,IOMAX/2
         XON(I)=XO(IOMAX/2+I)-360.0
       ENDDO
       DO  I=1+IOMAX/2,IOMAX
         XON(I)=XO(I-IOMAX/2)
       ENDDO

c      PRINT 45,XO,DLAM
C     DETERMINE LATITUDES
      CALL GAUSSG(JMAXD2,COSCL,DUMWT,SINCL,COLAT)
      DO 40 J=1,JMAXD2
      THETO(JOMAX+1-J)=PI2-COLAT(J)
      THETO(J)=-(PI2-COLAT(J))
      YO(JOMAX+1-J)=THETO(JOMAX+1-J)/PI180
   40 YO(J)=THETO(J)/PI180
c      PRINT 45,XO,YO
c   45 FORMAT(10X,'OLD LONGS,OLD LATS(DEG)=',/(1X,10E10.4))
c     Open unit 23 with a soft link (ln -s) in the script....
C      OPEN(23,FILE='lonlat_avn')
      WRITE(23,124)iomax,jomax
      WRITE(23,123)XON,YO
 124  FORMAT(1X,2I5)
 123  FORMAT(1X,10E10.4)


      return
      end
