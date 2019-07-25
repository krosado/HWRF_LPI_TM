
      SUBROUTINE CORT_N(inx,jnx,kmx,u_wrf,v_wrf,t_wrf,q_wrf,ps_wrf,  &
                   TENV,TH1,RP,psfc_obs1,vobs,HLON,HLAT,VLON,VLAT,   &
                   CLON_NEW,CLAT_NEW,beta,ics,VRMAX,PRMAX,PW,RIJ)

      parameter (IR=200)
      parameter (IR_1=IR-1)

      real(4)   u_wrf(inx,jnx,kmx),v_wrf(inx,jnx,kmx)
      real(4)   t_wrf(inx,jnx,kmx),q_wrf(inx,jnx,kmx)
      real(4)   TENV(inx,jnx,kmx)
      real(4)   ps_wrf(inx,jnx)
      REAL(4)   HLON(inx,jnx),HLAT(inx,jnx)
      REAL(4)   VLON(inx,jnx),VLAT(inx,jnx)

      REAL(4)   PW(kmx),th1(ir),rp(ir),dt_ps(ir)

      REAL(8)   CLON_NEW,CLAT_NEW

      REAL(4)    rij(inx,jnx)


       pi=4.*atan(1.)
       pi180=pi/180.
       pi_deg=180./pi
       DST1=6.371E6*pi180

       cost=cos(clat_new*pi180)

       print*,'inside cort'
       print*,'pi= ',pi

      print*,'clon_new,clat_new=',clon_new,clat_new

      print*,'beta=',beta

       ptmin=1.E20
       do j=1,jnx
       do i=1,inx
          if(ptmin.gt.ps_wrf(i,j))then
            ptmin=ps_wrf(i,j)
            i_m=i
            j_m=j
          end if
        end do
        end do

      idc=inx/2
      jdc=jnx/2

      DDS=(HLAT(idc,jdc+1)-HLAT(idc,jdc))**2*1.4

      pt_c=0.
      sum1=0.
      do j=1,jnx
      do i=1,inx
        dist=(((HLON(i,j)-CLON_NEW)*cost)**2+             &
              (HLAT(i,j)-CLAT_NEW)**2)
        rij(i,j)=sqrt(dist)*DST1
        if(dist.lt.DDS)THEN
          dist1=1./dist
          sum1=sum1+dist1
          pt_c=pt_c+ps_wrf(i,j)*dist1
        end if
      end do
      end do

      ptmin_c=pt_c/sum1 


      ps_diff_obs=psfc_obs1-ptmin_c    

!      ps_diff_mod=ptmin_c*(beta*beta-1.)

!      ps_diff=0.5*(ps_diff_obs+ps_diff_mod)

      ps_diff=ptmin_c*beta
     
      prmax15=1.5*PRMAX

      print*,'ptmin,ptmin_c,ps_diff=',ptmin,ptmin_c,ps_diff

      beta1=min(beta,1.)
      beta2=min(beta*beta,1.)
      beta5=min(sqrt(beta),1.)

      ps_diff_climate=-100.*(vobs*1.944/11.5)**2
!      ps_diff=min(ps_diff_obs,ps_diff_climate)

!      print*,'ps_mod,ps_climate,ps_obs=',ps_diff_mod,ps_diff_climate,ps_diff_obs

      dt_ps=0.

      sum_vt=0.
      do i=ir_1,1,-1
        th_m=0.5*(th1(i+1)+th1(i))
        sum_vt=sum_vt+th_m**2*(ALOG(rp(i)/rp(i+1)))
        dt_ps(i)=beta*(beta-1.)*1.1765*sum_vt     ! density=1.1765
        print*,'i,dt_ps(i)=',i,dt_ps(i)
      end do

      do i=1,ir_1
        dt_ps(i)=dt_ps(i)            ! use 75%
      end do

!      dt_ps=0.

      if(ics.eq.3)then
!      if(ics.eq.1)then

        do j=1,jnx
        do i=1,inx
          ps_wrf(i,j)=ps_wrf(i,j)*beta
        end do
        end do

      else

      do j=1,jnx
      do i=1,inx
        d_ps=0.
        DO N=1,ir
          DIF=rp(N)-RIJ(I,J)  
          IF(DIF.GT.0.)THEN
            IF(N.GT.1)THEN
              W1=(RIJ(I,J)-rp(N-1))/(rp(N)-rp(N-1))
              d_ps=W1*dt_ps(N)+(1.-W1)*dt_ps(N-1)
            ELSE IF(N.EQ.1)THEN
              d_ps=dt_ps(1)
            END IF
            GO TO 243
          END IF
        END DO
 243    CONTINUE
!        ps_wrf(i,j)=ps_wrf(i,j)+
!     &           ps_diff*(1.-exp(-VRMAX/(rij(i,j)+1.E-20)))
!     &           *sqrt(max(0.,(prmax15-rij(i,j))/prmax15))
!        ps_wrf(i,j)=ps_wrf(i,j)*ps_diff/ptmin_c
        ps_wrf(i,j)=ps_wrf(i,j)*beta + d_ps
      end do
      end do

      end if

      wind_c=min(1.,(vobs/21.)**4)      ! remove warm core
      wind_c1=min(1.,(vobs/41.)**2)      ! remove warm core
!      wind_c1=1.0
      if(ics.eq.1)then                                  ! analysis vobs < vmax
        do j=1,jnx
        do i=1,inx
        do k=1,kmx
          u_wrf(i,j,k)=u_wrf(i,j,k)*beta
          v_wrf(i,j,k)=v_wrf(i,j,k)*beta
          t_wrf(i,j,k)=t_wrf(i,j,k)*beta1*beta5*wind_c1
          q_wrf(i,j,k)=q_wrf(i,j,k)*beta1*beta5*wind_c1           ! does not use (is reset late)
        end do
        end do
        end do
      else if(ics.eq.0)then                             ! vobs > vmax
        do j=1,jnx
        do i=1,inx
        do k=1,kmx
          u_wrf(i,j,k)=u_wrf(i,j,k)*beta*PW(k)
          v_wrf(i,j,k)=v_wrf(i,j,k)*beta*PW(k)
          t_wrf(i,j,k)=t_wrf(i,j,k)*beta1*beta5*wind_c*PW(k)
          q_wrf(i,j,k)=q_wrf(i,j,k)*beta1*beta5*wind_c*PW(k)     ! does not use (is reset late)
        end do
        end do
        end do
      else if(ics.eq.2)then                             ! bogus
        do j=1,jnx
        do i=1,inx
        do k=1,kmx
          u_wrf(i,j,k)=u_wrf(i,j,k)*beta*PW(k)
          v_wrf(i,j,k)=v_wrf(i,j,k)*beta*PW(k)
          t_wrf(i,j,k)=t_wrf(i,j,k)*beta2*wind_c*PW(k)   ! remove more temp
          q_wrf(i,j,k)=q_wrf(i,j,k)*beta2*wind_c*PW(k)   ! remove more moisture  (is reset late)
        end do
        end do
        end do
      end if
 
      if(ics.eq.10)then

        FC2=2.*7.292E-5*SIN(CLAT_NEW*pi180)

        dt_ps=0.

        sum_vt=0.
        do i=ir_1,1,-1
          th_m=0.5*(th1(i+1)+th1(i))
          sum_vt=sum_vt+th_m*(rp(i)-rp(i+1))
          dt_ps(i)=dt_ps(i)+FC2*1.1765*sum_vt
        end do

        sum_vt=0.
        do i=ir_1,1,-1
          th_m=0.5*(th1(i+1)+th1(i))
          sum_vt=sum_vt+th_m**2*(ALOG(rp(i)/rp(i+1)))
          dt_ps(i)=dt_ps(i)+1.1765*sum_vt     ! density=1.1765
        end do

      do j=1,jnx
      do i=1,inx
        d_ps=0.
        DO N=1,ir
          DIF=rp(N)-RIJ(I,J)
          IF(DIF.GT.0.)THEN
            IF(N.GT.1)THEN
              W1=(RIJ(I,J)-rp(N-1))/(rp(N)-rp(N-1))
              d_ps=W1*dt_ps(N)+(1.-W1)*dt_ps(N-1)
            ELSE IF(N.EQ.1)THEN
              d_ps=dt_ps(1)
            END IF
            GO TO 24
          END IF
        END DO
 24    CONTINUE
        ps_wrf(i,j)=d_ps

        end do
        end do

      end if

      return    
      END
