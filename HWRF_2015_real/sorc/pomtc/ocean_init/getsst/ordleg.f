      SUBROUTINE ORDLEG(SX,COA,IR)
C                                                                        0002000
      IRPP = IR + 1
      IRPPM = IRPP - 1
      DELTA = ACOS(COA)
      SQR2=SQRT(2.0)
C                                                                        0008000
      THETA=DELTA
      C1=SQR2
      DO 20 N=1,IRPPM
      FN=N
      FN2=FN+FN
      FN2SQ=FN2*FN2
      C1=C1*SQRT(1.0-1.0/FN2SQ)
   20 CONTINUE
C                                                                        0017000
      N=IRPPM
      ANG=FN*THETA
      S1=0.0
      C4=1.0
      A=-1.0
      B=0.0
      N1=N+1
      DO 27 KK=1,N1,2
      K=KK-1
      IF (K.EQ.N) C4=0.5*C4
      S1=S1+C4*COS(ANG)
      A=A+2.0
      B=B+1.0
      FK=K
      ANG=THETA*(FN-FK-2.0)
      C4=(A*(FN-B+1.0)/(B*(FN2-A)))*C4
   27 CONTINUE
      SX=S1*C1
C                                                                        0036000
      RETURN
      END
