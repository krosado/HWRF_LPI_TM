      SUBROUTINE GAUSSG(NZERO,F,WT,SIA,RAD)
C                                                                        
C     THIS ROUTINE CALCULATES THE COSINE OF THE COLATITUDES 'F' AND     
C     THE WEIGHTS 'WT' , FOR THE GAUSSIAN QUADRATURE WITH 'NZERO' POINTS
C     BETWEEN POLE AND EQUATOR                                          
C                                                                        
      USE setparms
c
      DIMENSION F(NZERO),WT(NZERO),SIA(NZERO),RAD(NZERO)
C                                                                      

   50 if (kind(XLIM) == real_single) then
        XLIM=1.E-6
      else if (kind(XLIM) == real_double) then
        XLIM=1.E-12
      endif

      print *,'in GAUSSG,  XLIM = ',XLIM,'  NZERO = ',NZERO
C      print *,'F(1)   = ',F(1)
C      print *,'WT(1)  = ',WT(1)
C      print *,'SIA(1) = ',SIA(1)
C      print *,'RAD(1) = ',RAD(1)

      IR = NZERO+NZERO
      FI=IR
      FI1=FI+1.0
      PIOV2 = 3.1415926535898*0.5
      FN=PIOV2/NZERO

      do I=1,NZERO
C        print *,'In GAUSSG I loop, I = ',I
        WT(I)=I-0.5
      enddo

      do I=1,NZERO
        F(I)=SIN(WT(I)*FN+PIOV2)
      enddo

      DN = FI/ SQRT(4.0 * FI*FI-1.0  )
      DN1=FI1/ SQRT(4.0 * FI1*FI1-1.0  )
      A = DN1*FI
      B = DN*FI1
      IRP = IR + 1
      IRM = IR -1

      do I=1,NZERO
    5   CALL ORDLEG(G,F(I),IR)
        CALL ORDLEG(GM,F(I),IRM)
        CALL ORDLEG(GP,F(I),IRP)
        GT = (F(I)*F(I)-1.0) / (A*GP-B*GM)
        FTEMP = F(I) - G*GT
        GTEMP = F(I) - FTEMP
        F(I) = FTEMP
        IF( ABS(GTEMP).GT.XLIM) GO TO 5
      enddo   

      do I=1,NZERO
        A=2.0  *(1.0  -F(I)*F(I))
        CALL ORDLEG(B,F(I),IRM)
        B = B*B*FI*FI
        WT(I)=A*(FI-0.5  )/B
        RAD(I) = ACOS(F(I))
        SIA(I) = SIN(RAD(I))
      enddo
c
      RETURN
      END
