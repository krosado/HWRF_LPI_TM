



















      SUBROUTINE SOLVE_NMM(GRID,CONFIG_FLAGS                            &







,szj,s1z,spz,tcs,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist, &
dfi_moist_bxs,dfi_moist_bxe,dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar, &
scalar_bxs,scalar_bxe,scalar_bys,scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs, &
dfi_scalar_bxe,dfi_scalar_bys,dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,chem,ozmixm &


     &           )

        use module_timing
      USE MODULE_DOMAIN,                ONLY : DOMAIN, GET_IJK_FROM_GRID &
                                              ,domain_clock_get,is_alarm_tstep_nphs
      USE MODULE_CONFIGURE,             ONLY : GRID_CONFIG_REC_TYPE
      USE MODULE_MODEL_CONSTANTS
      USE MODULE_STATE_DESCRIPTION
      USE MODULE_CTLBLK
      use MODULE_RANDOM,                ONLY : rand_grid_r4
      USE MODULE_DM,                    ONLY : LOCAL_COMMUNICATOR       &
                                              ,MYTASK,NTASKS,NTASKS_X   &
                                              ,NTASKS_Y
      USE MODULE_COMM_DM
      USE MODULE_SWATH,                 ONLY : UPDATE_INTEREST
      USE MODULE_HIFREQ,                ONLY: HIFREQ_WRITE, HIFREQ_OPEN
      USE MODULE_TORNADO_GENESIS,       ONLY: CALC_TORNADO_GENESIS, RESET_TORNADO_GENESIS
      USE MODULE_IGWAVE_ADJUST,         ONLY: PDTE,PFDHT,DDAMP,VTOA
      USE MODULE_ADVECTION,             ONLY: ADVE,VAD2,HAD2            &
                                             ,ADV2,MONO                 &
                                             ,VAD2_SCAL,HAD2_SCAL
      USE MODULE_NONHY_DYNAM,           ONLY: EPS,VADZ,HADZ
      USE MODULE_DIFFUSION_NMM,         ONLY: HDIFF
      USE MODULE_BNDRY_COND,            ONLY: &
           BOCOV, MASS_BOUNDARY, MP_BULK_BOUNDARY, MP_SPECIES_BDY
      USE MODULE_PHYSICS_CALLS
      USE MODULE_EXT_INTERNAL
      USE MODULE_PRECIP_ADJUST
      USE MODULE_NEST_UTIL     
      USE MODULE_STATS_FOR_MOVE,        ONLY: STATS_FOR_MOVE
      USE MODULE_DIAG_REFL




      IMPLICIT NONE







      TYPE(DOMAIN),TARGET :: GRID

















real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_szj)           :: szj
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_s1z)           :: s1z
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_spz)           :: spz
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tcs)           :: tcs
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_moist)           :: moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)           :: dfi_moist
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_scalar)           :: scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)           :: dfi_scalar
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bye
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxs
real      ,DIMENSION(grid%sm32:grid%em32,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33,grid%sm32:grid%em32,num_chem)           :: chem
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%levsiz,grid%sm32:grid%em32,num_ozmixm)           :: ozmixm





      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS







      INTEGER :: IDS,IDE,JDS,JDE,KDS,KDE                                &
     &          ,IMS,IME,JMS,JME,KMS,KME                                & 
     &          ,IPS,IPE,JPS,JPE,KPS,KPE                                &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE

      LOGICAL :: advect_q2
      INTEGER :: I,ICLTEND,IDF,IRTN,J,JC,JDF,K,KDF,LB,N_MOIST &
     &          ,NTSD_current,L

      INTEGER,SAVE :: NTSD_restart1,NTSD_restart2,NTSD_restart3
      integer :: ierr,nrand,idt
      INTEGER,SAVE :: NTSD_restart

      INTEGER :: MYPROC,imid,jmid
      INTEGER :: KVH,NTSD_rad,RC
      INTEGER :: NUM_AEROSOLC

      REAL :: DT_INV,FICE,FRAIN,GPS,QI,QR,QW,WC,WP

      LOGICAL :: LAST_TIME,OPERATIONAL_PHYSICS,ETAMP_PHYSICS

      CHARACTER(80) :: MESSAGE


      INTEGER :: ISTAT,DOM,one
      LOGICAL :: HF
      REAL,ALLOCATABLE,SAVE,DIMENSION(:,:,:) :: PPTDAT





      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: TTEN,QTEN
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: RTHRATEN,RTHBLTEN,RQVBLTEN



      LOGICAL wrf_dm_on_monitor
      EXTERNAL wrf_dm_on_monitor




      real,save :: solve_tim,exch_tim,pdte_tim,adve_tim,vtoa_tim        &
     &,            vadz_tim,hadz_tim,eps_tim,vad2_tim,had2_tim          &
     &,            radiation_tim,rdtemp_tim,turbl_tim,cltend_tim        &
     &,            cucnvc_tim,gsmdrive_tim,hdiff_tim,bocoh_tim          &
     &,            pfdht_tim,ddamp_tim,bocov_tim,uv_htov_tim,sum_tim    &
     &,            sst_tim,flux_tim,hifreq_tim                          &
     &,            diag_tim,adjppt_tim,tornado_tim                      


      LOGICAL                        :: diag_flag

      real,save :: exch_tim_max
      real :: ttim,btimx
      real :: et_max,this_tim
      integer :: n_print_time














      LOGICAL :: EULER
      INTEGER :: IDTADT 
      INTEGER :: IDTADC
      INTEGER :: KS                                                        

      REAL,SAVE :: SUMDRRW














































      ttim=now_time() 
      CALL DOMAIN_CLOCK_GET(GRID,ADVANCEcOUNT=NTSD_current)

      IF(NTSD_current==0)THEN
        IF(GRID%RESTART.AND.GRID%TSTART>0.)THEN

          if( grid%id .eq. 1 ) NTSD_restart1=INT(grid%TSTART*3600./GRID%DT+0.5)
          if( grid%id .eq. 2 ) NTSD_restart2=INT(grid%TSTART*3600./GRID%DT+0.5)
          if( grid%id .eq. 3 ) NTSD_restart3=INT(grid%TSTART*3600./GRID%DT+0.5)
          IHRST=grid%nstart_hour
          NTSD_restart=grid%ntsd
        ELSE
          IHRST=GRID%GMT
          grid%nstart_hour=IHRST

          NTSD_restart1=0
          NTSD_restart2=0
          NTSD_restart3=0
        ENDIF
      ENDIF

      if( grid%id .eq. 1 ) grid%ntsd=NTSD_restart1+NTSD_current
      if( grid%id .eq. 2 ) grid%ntsd=NTSD_restart2+NTSD_current
      if( grid%id .eq. 3 ) grid%ntsd=NTSD_restart3+NTSD_current
      LAST_TIME=domain_last_time_step(GRID)





      diag_flag = &
           is_alarm_tstep_nphs(grid%domain_clock, grid%alarms(HISTORY_ALARM), grid%nphs) &
           .or. &
           is_alarm_tstep_nphs(grid%domain_clock, grid%alarms(AUXHIST1_ALARM), grid%nphs) &
           .or. &
           is_alarm_tstep_nphs(grid%domain_clock, grid%alarms(AUXHIST2_ALARM), grid%nphs) &
           .or. &
           is_alarm_tstep_nphs(grid%domain_clock, grid%alarms(AUXHIST3_ALARM), grid%nphs)





        WRITE(MESSAGE,125)grid%ntsd,grid%ntsd*GRID%DT/3600.
  125   FORMAT(' SOLVE_NMM: TIMESTEP IS ',I5,'   TIME IS ',F7.3,' HOURS')
        CALL WRF_MESSAGE(TRIM(MESSAGE))




      EULER=model_config_rec%EULER_ADV
      IDTADT=model_config_rec%IDTADT
      IDTADC=model_config_rec%IDTADC
      WP=model_config_rec%WP(grid%id)


      CALL WRF_GET_DM_COMMUNICATOR(MPI_COMM_COMP)
      CALL WRF_GET_NPROC(NPES)
      CALL WRF_GET_MYPROC(MYPROC)
      MYPE=MYPROC




      CALL GET_IJK_FROM_GRID(GRID                                       &
     &                      ,IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                      ,IMS,IME,JMS,JME,KMS,KME                    &
     &                      ,IPS,IPE,JPS,JPE,KPS,KPE )






      CALL SET_TILES(GRID,IDS,IDE,JDS,JDE,IPS,IPE,JPS,JPE)





      ETAMP_PHYSICS=.FALSE.

      IF (CONFIG_FLAGS%MP_PHYSICS == ETAMPNEW .OR.                 &

     &    CONFIG_FLAGS%MP_PHYSICS == FER_MP_HIRES .OR.                 &
     &    CONFIG_FLAGS%MP_PHYSICS == ETAMP_HWRF ) THEN

         ETAMP_PHYSICS=.TRUE.

      ENDIF

      ADVECT_Q2=.TRUE.
      if(CONFIG_FLAGS%BL_PBL_PHYSICS == GFSSCHEME .OR. &
         CONFIG_FLAGS%BL_PBL_PHYSICS == GFS2011SCHEME) THEN
         ADVECT_Q2=.FALSE.
      endif









      OPERATIONAL_PHYSICS=.FALSE.

      IF(CONFIG_FLAGS%RA_SW_PHYSICS    ==GFDLSWSCHEME.AND.              &
     &   CONFIG_FLAGS%RA_LW_PHYSICS    ==GFDLLWSCHEME.AND.              &
     &   CONFIG_FLAGS%SF_SFCLAY_PHYSICS==MYJSFCSCHEME.AND.              &
     &   CONFIG_FLAGS%BL_PBL_PHYSICS   ==MYJPBLSCHEME.AND.              &
     &   CONFIG_FLAGS%CU_PHYSICS       ==BMJSCHEME.AND.                 &
     &   ETAMP_PHYSICS ) THEN


        OPERATIONAL_PHYSICS=.TRUE.

      ENDIF





      ALLOCATE(TTEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
      ALLOCATE(QTEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
      ALLOCATE(RTHBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
      ALLOCATE(RQVBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
      ALLOCATE(RTHRATEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)

        IF(CONFIG_FLAGS%CU_PHYSICS==GDSCHEME.OR.                        &
     &  CONFIG_FLAGS%CU_PHYSICS==TIEDTKESCHEME.OR.                      &
     &  CONFIG_FLAGS%CU_PHYSICS==KFETASCHEME)THEN

        DO J=JMS,JME
        DO K=KMS,KME
        DO I=IMS,IME
          TTEN(I,K,J)=grid%t(I,J,K)
          QTEN(I,K,J)=grid%q(I,J,K)
        ENDDO
        ENDDO
        ENDDO
      ENDIF

      GRID%SIGMA=1 

      IF (config_flags%non_hydrostatic) THEN
        grid%hydro=.FALSE.
      ELSE
        grid%hydro=.TRUE.
      ENDIF

      IDF=IDE-1
      JDF=JDE-1
      KDF=KDE-1






      ITS=IPS
      ITE=MIN(IPE,IDF)
      JTS=JPS
      JTE=MIN(JPE,JDF)
      KTS=KPS
      KTE=MIN(KPE,KDF)


      if(grid%ntsd==0)then
        write(message,*)' its=',its,' ite=',ite
        call wrf_message(trim(message))
        write(message,*)' jts=',jts,' jte=',jte
        call wrf_message(trim(message))
        write(message,*)' kts=',kts,' kte=',kte
        call wrf_message(trim(message))


      endif



      if(grid%ntsd==0)then
        sum_tim=0.
        solve_tim=0.
        exch_tim=0.
        pdte_tim=0.
        adve_tim=0.
        vtoa_tim=0.
        vadz_tim=0.
        hadz_tim=0.
        eps_tim=0.
        vad2_tim=0.
        had2_tim=0.
        radiation_tim=0.
        rdtemp_tim=0.
        turbl_tim=0.
        cltend_tim=0.
        cucnvc_tim=0.
        gsmdrive_tim=0.
        hdiff_tim=0.
        bocoh_tim=0.
        pfdht_tim=0.
        ddamp_tim=0.
        bocov_tim=0.
        uv_htov_tim=0.
        exch_tim_max=0.
        adjppt_tim=0.
        diag_tim=0.
        tornado_tim=0.
        sst_tim=0.
        flux_tim=0.
        hifreq_tim=0.
      endif

      N_MOIST=NUM_MOIST



      DO J=max(jds+( 0 ),jts-( 4  )),min(jde-( 0 ),jte+( 4  ))
        grid%iheg(J)=MOD(J+1,2)
        grid%ihwg(J)=grid%iheg(J)-1
        grid%iveg(J)=MOD(J,2)
        grid%ivwg(J)=grid%iveg(J)-1
      ENDDO

      DO J=max(jds+( 0 ),jts-( 4  )),min(jde-( 0 ),jte+( 4  ))
        grid%ivw(J)=grid%ivwg(J)
        grid%ive(J)=grid%iveg(J)
        grid%ihe(J)=grid%iheg(J)
        grid%ihw(J)=grid%ihwg(J)
      ENDDO



      LB=2*(IDF-IDS+1)+(JDF-JDS+1)-3



      JC=jps+(jpe-jps)/2
      GPS=SQRT(grid%dx_nmm(ips,JC)**2+grid%dy_nmm**2)



      TSPH=3600./GRID%DT

      n_print_time=nint(3600./grid%dt)   


      NBOCO=0








      CALL wrf_debug ( 100 , 'nmm: in patch' )



      btimx=now_time()




      IF(GRID%ID/=1)THEN



        IF(GRID%ID/=1.AND.MOD(grid%ntsd,1)==0.AND.GRID%NUM_MOVES==-99)THEN
          grid%XLOC_1=(IDE-1)/2     
          grid%YLOC_1=(JDE-1)/2     
        ENDIF


      ENDIF






      IF(GRID%PCPFLG.AND..NOT.ALLOCATED(PPTDAT))THEN
        ALLOCATE(PPTDAT(IMS:IME,JMS:JME,3),STAT=ISTAT)
      ENDIF













      IF (grid%ntsd==0) THEN
        IF (GRID%PCPFLG) THEN
          CALL READPCP(PPTDAT,grid%ddata,grid%lspa                                &
     &      ,IDS,IDE,JDS,JDE,KDS,KDE                                    &
     &      ,IMS,IME,JMS,JME,KMS,KME                                    &
     &      ,ITS,ITE,JTS,JTE,KTS,KTE)
        ENDIF
      ENDIF







      randif: IF(in_use_for_config(grid%id,'random')) THEN

         nrand=config_flags%nrand
         if(nrand==0) nrand=grid%ncnvc
         if(nrand==0) nrand=1
         IDT=MOD(grid%NTSD,nrand)
 
         IF(IDT.EQ.0 .OR. grid%NTSD .EQ. 0)THEN
            call start_timing
            call wrf_message('Update random numbers...')
            one=1

            imid=(its+ite)/2   ;   jmid=(jts+jte)/2

            write(message,'(A,": random(",I0,",",I0,") = ",E15.10)') 'before call',imid,jmid,grid%random(imid,jmid)
            call wrf_debug(3,message)

            call rand_grid_r4(grid%randstate1,grid%randstate2, &
                              grid%randstate3,grid%randstate4, &
                              grid%random, &
                              IDS,IDE,JDS,JDE,one,one, &
                              IMS,IME,JMS,JME,one,one, &
                              ITS,ITE,JTS,JTE,one,one)

            write(message,'(A,": random(",I0,",",I0,") = ",E15.10)') 'after call',imid,jmid,grid%random(imid,jmid)
            call wrf_debug(3,message)

            call end_timing('Updating random numbers')
         ENDIF    
      ENDIF randif



     IF(grid%tg_want_reset/=0) THEN
        btimx=now_time()
        CALL RESET_TORNADO_GENESIS(GRID,CONFIG_FLAGS)
        tornado_tim=tornado_tim+now_time()-btimx
     ENDIF




     if(size(grid%precip_swath)>1 .and. grid%update_interest) then
        call update_interest(grid,config_flags)
        grid%update_interest=.false.
     endif





      CALL BUCKETS(grid%ntsd,grid%nprec,grid%nsrfc,grid%nrdsw,grid%nrdlw                         &
     &            ,GRID%RESTART,GRID%TSTART                             &
     &            ,grid%nclod,grid%nheat,GRID%NPHS,TSPH                           &
     &            ,grid%acprec,grid%cuprec,grid%acsnow,grid%acsnom,grid%ssroff,grid%bgroff            &
     &            ,grid%sfcevp,grid%potevp,grid%sfcshx,grid%sfclhx,grid%subshx,grid%snopcx            &
     &            ,grid%sfcuvx,grid%potflx                                        &
     &            ,grid%ardsw,grid%aswin,grid%aswout,grid%aswtoa                            &
     &            ,grid%ardlw,grid%alwin,grid%alwout,grid%alwtoa                            &
     &            ,grid%acfrst,grid%ncfrst,grid%acfrcv,grid%ncfrcv                          &
     &            ,grid%avcnvc,grid%avrain,grid%tcucn,grid%train                            &
     &            ,grid%asrfc                                                &
     &            ,grid%t,grid%tlmax,grid%tlmin,grid%tshltr,grid%pshltr,grid%qshltr                   &
     &            ,grid%t02_max,grid%t02_min,grid%rh02_max,grid%rh02_min                    &
     &            ,IDS,IDE,JDS,JDE,KDS,KDE                              &
     &            ,IMS,IME,JMS,JME,KMS,KME                              &
     &            ,ITS,ITE,JTS,JTE,KTS,KTE)






      IF(grid%ntsd==0)THEN

        FIRST=.TRUE.




        btimx=now_time()

        grid%mommix=amin1(grid%mommix,1.0)






      IF(EULER) THEN
        SUMDRRW=0.

        DO K=KTS,KTE
          DO J=JMS,JME
            DO I=IMS,IME
              grid%rrw(I,J,K)=0.

              IF(I>=IDE/2-6.AND.I<=IDE/2+6.AND.   &
                 J>=JDE/2-6.AND.J<=JDE/2+6     ) THEN
                grid%rrw(I,J,K)=10.0 

              ENDIF

            ENDDO
          ENDDO
        ENDDO

        DO KS=PARAM_FIRST_SCALAR,NUM_SZJ
          DO K=KMS,KME
            DO J=JMS,JME
              DO I=IMS,IME
                SZJ(I,J,K,KS)=0.
                S1Z(I,J,K,KS)=0.
                SPZ(I,J,K,KS)=0.
                TCS(I,J,K,KS)=0.
              ENDDO
            ENDDO
          ENDDO
        ENDDO

      ENDIF

















      IF(EULER) THEN

        DO K=KTS,KTE
          DO J=JMS,JME
            DO I=IMS,IME
              SPZ(I,J,K,P_SPZ1)=SQRT(MAX(grid%q  (I,J,K),EPSQ))
              SPZ(I,J,K,P_SPZ2)=SQRT(MAX(grid%cwm(I,J,K),EPSQ))
              SPZ(I,J,K,P_SPZ4)=SQRT(MAX(grid%rrw(I,J,K),0.  ))
            ENDDO
          ENDDO
        ENDDO

        DO J=JMS,JME
          DO I=IMS,IME
            SPZ(I,J,KTE,P_SPZ3)=SQRT(MAX((grid%q2(I,J,KTE)+EPSQ2)*0.5,EPSQ2))
          ENDDO
        ENDDO

        DO K=KTE-1,KTS,-1
          DO J=JMS,JME
            DO I=IMS,IME
              SPZ(I,J,K,P_SPZ3)=SQRT(MAX((grid%q2(I,J,K)+grid%q2(I,J,K+1))*0.5,EPSQ2))
            ENDDO
          ENDDO
        ENDDO

      ENDIF




        exch_tim=exch_tim+now_time()-btimx











       if(GRID%RESTART) then
       FIRST=.FALSE.
       else
       GO TO 2003
       endif




      ENDIF



 2000 CONTINUE








      btimx=now_time()
      call ATM_TSTEP_INIT(NTSD_current,grid%NPHS,GRID%ID,grid%NPHS*grid%dt, &
      ids,idf,jds,jdf,its,ite,jts,jte,ims,ime,jms,jme,           &
                       kds,kde,kts,kte,kms,kme,                  &
                       grid%HLON,grid%HLAT,grid%VLON,grid%VLAT,grid%sm,                   &
                       grid%i_parent_start,grid%j_parent_start,  &
                       grid%guessdtc,grid%dtc)
      sst_tim=sst_tim+now_time()-btimx











      btimx=now_time()






      exch_tim=exch_tim+now_time()-btimx








      btimx=now_time()

      call check_grid(grid,config_flags,'before PDTE', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
      CALL PDTE(                                                        &

     &            GRID,MYPE,MPI_COMM_COMP,                              &

     &            grid%ntsd,GRID%DT,grid%pt,grid%eta2,grid%res,grid%hydro,grid%hbm2                   &
     &           ,grid%pd,grid%pdsl,grid%pdslo                                         &
     &           ,grid%petdt,grid%div,grid%psdt                                        &
     &           ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                       &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &           ,IMS,IME,JMS,JME,KMS,KME                               &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)


      pdte_tim=pdte_tim+now_time()-btimx








      btimx=now_time()







CALL HALO_NMM_F_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )







CALL HALO_NMM_F1_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


      exch_tim=exch_tim+now_time()-btimx




      btimx=now_time()

      call check_grid(grid,config_flags,'before ADVE', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
      CALL ADVE(grid%ntsd,GRID%DT,grid%deta1,grid%deta2,grid%pdtop                          &
     &         ,grid%curv,grid%f,grid%fad,grid%f4d,grid%em_loc,grid%emt_loc,grid%en,grid%ent,grid%dx_nmm,grid%dy_nmm      &
     &         ,grid%hbm2,grid%vbm2                                               &
     &         ,grid%t,grid%u,grid%v,grid%pdslo,grid%told,grid%uold,grid%vold                              &
     &         ,grid%petdt,grid%upstrm                                            &
     &         ,grid%few,grid%fns,grid%fne,grid%fse                                         &
     &         ,grid%adt,grid%adu,grid%adv                                             & 
     &         ,grid%n_iup_h,grid%n_iup_v                                         &
     &         ,grid%n_iup_adh,grid%n_iup_adv                                     &
     &         ,grid%iup_h,grid%iup_v,grid%iup_adh,grid%iup_adv                             &
     &         ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                         &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)

      adve_tim=adve_tim+now_time()-btimx





      eulerian: IF(EULER) THEN    




        IF(.NOT.ETAMP_PHYSICS.and.CONFIG_FLAGS%MP_PHYSICS/=0) THEN
          WRITE( wrf_err_message , * ) 'EULER advection works only with ETAMPNEW microphysics.'
          CALL wrf_error_fatal3("<stdin>",923,&
wrf_err_message )
        ENDIF


        idtadt_block: IF(MOD(grid%ntsd,IDTADT)==0) THEN

          btimx=now_time()



          exch_tim=exch_tim+now_time()-btimx

          btimx=now_time()

          DO K=KTS,KTE
            DO J=JMS,JME
              DO I=IMS,IME
                SZJ(I,J,K,P_SPZ1)=MAX(grid%q  (I,J,K),EPSQ)
                SZJ(I,J,K,P_SPZ2)=MAX(grid%cwm(I,J,K),EPSQ)
                SZJ(I,J,K,P_SPZ4)=MAX(grid%rrw(I,J,K),0.  )
              ENDDO
            ENDDO
          ENDDO

          DO J=JMS,JME
            DO I=IMS,IME
              SZJ(I,J,KTE,P_SPZ3)=MAX((grid%q2 (I,J,KTE)+EPSQ2)*0.5,EPSQ2)
            ENDDO
          ENDDO

          DO K=KTE-1,KTS,-1
            DO J=JMS,JME
              DO I=IMS,IME
                SZJ(I,J,K,P_SPZ3)=MAX((grid%q2 (I,J,K)+grid%q2 (I,J,K+1))*0.5,EPSQ2)
              ENDDO
            ENDDO
          ENDDO







CALL HALO_TRACERS_sub ( grid, &
  num_szj, &
  szj, &
  num_s1z, &
  s1z, &
  num_spz, &
  spz, &
  num_tcs, &
  tcs, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

      call check_grid(grid,config_flags,'before ADV2', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
          CALL ADV2                &
          (grid%upstrm                  &
          ,MYPE,PARAM_FIRST_SCALAR,NUM_SZJ &
          ,IDS,IDE,JDS,JDE,KDS,KDE &
          ,IMS,IME,JMS,JME,KMS,KME &
          ,ITS,ITE,JTS,JTE,KTS,KTE &
          ,grid%n_iup_h                 &
          ,grid%n_iup_adh               &
          ,grid%iup_h,grid%iup_adh           &
          ,grid%ent                     &
          ,IDTADT                  &
          ,grid%DT,grid%pdtop           &
          ,grid%ihe,grid%ihw,grid%ive,grid%ivw         &
          ,grid%deta1,grid%deta2             &
          ,grid%emt_loc                 &
          ,grid%fad,grid%hbm2,grid%pdsl,grid%pdslo     &
          ,grid%petdt                   &
          ,grid%uold,grid%vold               &
          ,SZJ,SPZ                 &
          
          ,grid%fne,grid%fse,grid%few,grid%fns,S1Z,TCS)







CALL HALO_TRACERS_sub ( grid, &
  num_szj, &
  szj, &
  num_s1z, &
  s1z, &
  num_spz, &
  spz, &
  num_tcs, &
  tcs, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

      call check_grid(grid,config_flags,'before MONO', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
          CALL MONO &
          (         &
           GRID%DOMDESC, &
           MYPE,grid%ntsd,grid%ntsd*GRID%DT/3600.,PARAM_FIRST_SCALAR,NUM_SZJ &
          ,IDS,IDE,JDS,JDE,KDS,KDE              &
          ,IMS,IME,JMS,JME,KMS,KME              &
          ,ITS,ITE,JTS,JTE,KTS,KTE              &
          ,IDTADT                               &
          ,grid%dy_nmm,grid%pdtop                         &
          ,SUMDRRW                              &
          ,grid%ihe,grid%ihw                              &
          ,grid%deta1,grid%deta2                          &
          ,grid%dx_nmm,grid%hbm2,grid%pdsl                     &
          ,SZJ                                  &
          
          ,S1Z,TCS)

          DO KS=PARAM_FIRST_SCALAR,NUM_SZJ 
            DO K=KTS,KTE
              DO J=max(jds+( 2 ),jts-( 0  )),min(jde-( 2 ),jte+( 0  ))
                DO I=max(ids+( 1 ),its-( 0  )),min(ide-( 1 ),ite+( 0  ))
                  SZJ(I,J,K,KS)=SZJ(I,J,K,KS)+TCS(I,J,K,KS)
                ENDDO
              ENDDO
            ENDDO
          ENDDO         

          DO K=KTS,KTE
            DO J=max(jds+( 2 ),jts-( 0  )),min(jde-( 2 ),jte+( 0  ))
              DO I=max(ids+( 1 ),its-( 0  )),min(ide-( 1 ),ite+( 0  ))
                grid%q  (I,J,K)=SZJ(I,J,K,P_SZJ1)
                grid%cwm(I,J,K)=SZJ(I,J,K,P_SZJ2)
                grid%rrw(I,J,K)=SZJ(I,J,K,P_SZJ4)
              ENDDO
            ENDDO
          ENDDO

          DO J=max(jds+( 2 ),jts-( 0  )),min(jde-( 2 ),jte+( 0  ))
            DO I=max(ids+( 1 ),its-( 0  )),min(ide-( 1 ),ite+( 0  ))
              grid%q2(I,J,KTE)=MAX(SZJ(I,J,KTE,P_SZJ3)+SZJ(I,J,KTE,P_SZJ3)-EPSQ2 &
                             ,EPSQ2)
            ENDDO
          ENDDO

          DO K=KTE-1,KTS+1,-1
            DO J=max(jds+( 2 ),jts-( 0  )),min(jde-( 2 ),jte+( 0  ))
              DO I=max(ids+( 1 ),its-( 0  )),min(ide-( 1 ),ite+( 0  ))
                IF(K>KTS)THEN
                  grid%q2(I,J,K)=MAX(SZJ(I,J,K,P_SZJ3)+SZJ(I,J,K,P_SZJ3)-grid%q2(I,J,K+1) &
                               ,EPSQ2)
                ELSE
                  grid%q2(I,J,K)=grid%q2(I,J,K+1)
                ENDIF
              ENDDO
            ENDDO
          ENDDO










        IF(.NOT.OPERATIONAL_PHYSICS)THEN
          DO K=KTS,KTE
            DO J=max(jds+( 0 ),jts-( 0  )),min(jde-( 0 ),jte+( 0  ))
              DO I=max(ids+( 0 ),its-( 0  )),min(ide-( 0 ),ite+( 0  ))
                MOIST(I,J,K,P_QV)=grid%q(I,J,K)/(1.-grid%q(I,J,K))
                WC = grid%cwm(I,J,K)
                QI = 0.
                QR = 0.
                QW = 0.
                FICE=grid%f_ice(I,K,J)
                FRAIN=grid%f_rain(I,K,J)

                IF(FICE>=1.)THEN
                  QI=WC
                ELSEIF(FICE<=0.)THEN
                  QW=WC
                ELSE
                  QI=FICE*WC
                  QW=WC-QI
                ENDIF

                IF(QW>0..AND.FRAIN>0.)THEN
                  IF(FRAIN>=1.)THEN
                    QR=QW
                    QW=0.
                  ELSE
                    QR=FRAIN*QW
                    QW=QW-QR
                  ENDIF
                ENDIF

                MOIST(I,J,K,P_QC)=QW
                MOIST(I,J,K,P_QR)=QR
                MOIST(I,J,K,P_QI)=0.
                MOIST(I,J,K,P_QS)=QI
                MOIST(I,J,K,P_QG)=0.
              ENDDO
            ENDDO
          ENDDO
        ENDIF

        had2_tim=had2_tim+now_time()-btimx


        ENDIF idtadt_block



      ENDIF eulerian  








      btimx=now_time()

      call check_grid(grid,config_flags,'before VTOA', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
      CALL VTOA(                                                        &
     &          grid%ntsd,GRID%DT,grid%pt,grid%eta2                                    &
     &         ,grid%hbm2,grid%ef4t                                               &
     &         ,grid%t,grid%dwdt,grid%rtop,grid%omgalf                                      &
     &         ,grid%pint,grid%div,grid%psdt,grid%res                                       &
     &         ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                         &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)

      vtoa_tim=vtoa_tim+now_time()-btimx





      btimx=now_time()




      call check_grid(grid,config_flags,'before VADZ', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
      CALL VADZ(grid%ntsd,GRID%DT,grid%fis,GRID%SIGMA,grid%dfl,grid%hbm2                    &
     &         ,grid%deta1,grid%deta2,grid%pdtop                                       &
     &         ,grid%pint,grid%pdsl,grid%pdslo,grid%petdt                                   &
     &         ,grid%rtop,grid%t,grid%q,grid%cwm,grid%z,grid%w,grid%dwdt,grid%pdwdt                             &
     &         ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                         &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)

      vadz_tim=vadz_tim+now_time()-btimx





      btimx=now_time()







CALL HALO_NMM_G_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


      exch_tim=exch_tim+now_time()-btimx





      btimx=now_time()

      call check_grid(grid,config_flags,'before HADZ', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
      CALL HADZ(grid%ntsd,GRID%DT,grid%hydro,grid%hbm2,grid%deta1,grid%deta2,grid%pdtop               &
     &         ,grid%dx_nmm,grid%dy_nmm,grid%fad                                       &
     &         ,grid%few,grid%fns,grid%fne,grid%fse                                         &
     &         ,grid%pdsl,grid%u,grid%v,grid%w,grid%z,WP,grid%BARO                          &
     &         ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                         &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)

      hadz_tim=hadz_tim+now_time()-btimx





      btimx=now_time()







CALL HALO_NMM_H_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


      exch_tim=exch_tim+now_time()-btimx





      btimx=now_time()

      call check_grid(grid,config_flags,'before EPS', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)

      CALL EPS(grid%ntsd,GRID%DT,grid%hydro,grid%dx_nmm,grid%dy_nmm,grid%fad                     &
     &        ,grid%aeta1,grid%deta1,grid%deta2,grid%pdtop,grid%pt                                     &
     &        ,grid%hbm2,grid%hbm3                                                &
     &        ,grid%pdsl,grid%pdslo,grid%pint,grid%rtop,grid%petdt,grid%pdwdt                         &
     &        ,grid%dwdt,grid%dwdtmn,grid%dwdtmx                                       &
     &        ,grid%fns,grid%few,grid%fne,grid%fse                                          &
     &        ,grid%t,grid%u,grid%v,grid%w,grid%w_tot,grid%q,grid%cwm                                            &
     &        ,grid%def3d,grid%hdac,grid%baro                                              &
     &        ,WP                                                 &
     &        ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                          &
     &        ,IDS,IDF,JDS,JDF,KDS,KDE                                  &
     &        ,IMS,IME,JMS,JME,KMS,KME                                  &
     &        ,ITS,ITE,JTS,JTE,KTS,KTE)

      eps_tim=eps_tim+now_time()-btimx



      not_euler: IF(.NOT.EULER) THEN 





      call check_grid(grid,config_flags,'before VAD2', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
      IF(MOD(grid%ntsd,GRID%IDTAD)==0)THEN
        btimx=now_time()

        vad2_micro_check: IF (ETAMP_PHYSICS) THEN
          CALL VAD2(grid%ntsd,GRID%DT,GRID%IDTAD,grid%dx_nmm,grid%dy_nmm               &
     &             ,grid%aeta1,grid%aeta2,grid%deta1,grid%deta2,grid%pdsl,grid%pdtop,grid%hbm2             &
     &             ,grid%q,grid%q2,grid%cwm,grid%petdt                                      &
     &             ,grid%n_iup_h,grid%n_iup_v                                     &
     &             ,grid%n_iup_adh,grid%n_iup_adv                                 &
     &             ,grid%iup_h,grid%iup_v,grid%iup_adh,grid%iup_adv                         &
     &             ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                     &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                             &
     &             ,IMS,IME,JMS,JME,KMS,KME                             &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)

        ELSE vad2_micro_check
          CALL VAD2_SCAL(grid%ntsd,GRID%DT,GRID%IDTAD,grid%dx_nmm,grid%dy_nmm          &
     &                  ,grid%aeta1,grid%aeta2,grid%deta1,grid%deta2,grid%pdsl,grid%pdtop             &
     &                  ,grid%hbm2                                           &
     &                  ,grid%q2,grid%petdt                                       &
     &                  ,grid%n_iup_h,grid%n_iup_v                                &
     &                  ,grid%n_iup_adh,grid%n_iup_adv                            &
     &                  ,grid%iup_h,grid%iup_v,grid%iup_adh,grid%iup_adv                    &
     &                  ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                &
     &                  ,1,1                                            &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)                              
     
          CALL VAD2_SCAL(grid%ntsd,GRID%DT,GRID%IDTAD,grid%dx_nmm,grid%dy_nmm          &
     &                  ,grid%aeta1,grid%aeta2,grid%deta1,grid%deta2,grid%pdsl,grid%pdtop             &
     &                  ,grid%hbm2                                           &
     &                  ,MOIST,grid%petdt                                    &
     &                  ,grid%n_iup_h,grid%n_iup_v                                &
     &                  ,grid%n_iup_adh,grid%n_iup_adv                            &
     &                  ,grid%iup_h,grid%iup_v,grid%iup_adh,grid%iup_adv                    &
     &                  ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                &
     &                  ,NUM_MOIST,2                                    &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)

          CALL VAD2_SCAL(grid%ntsd,GRID%DT,GRID%IDTAD,grid%dx_nmm,grid%dy_nmm          &
     &                  ,grid%aeta1,grid%aeta2,grid%deta1,grid%deta2,grid%pdsl,grid%pdtop             &
     &                  ,grid%hbm2                                           &
     &                  ,SCALAR,grid%petdt                                   &
     &                  ,grid%n_iup_h,grid%n_iup_v                                &
     &                  ,grid%n_iup_adh,grid%n_iup_adv                            &
     &                  ,grid%iup_h,grid%iup_v,grid%iup_adh,grid%iup_adv                    &
     &                  ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                &
     &                  ,NUM_SCALAR,2                                   &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)


          DO K=KTS,KTE
          DO J=max(jds+( 0 ),jts-( 0  )),min(jde-( 0 ),jte+( 0  ))
          DO I=max(ids+( 0 ),its-( 0  )),min(ide-( 0 ),ite+( 0  ))
            grid%q(I,J,K)=MOIST(I,J,K,P_QV)/(1.+MOIST(I,J,K,P_QV))
          ENDDO
          ENDDO   
          ENDDO   

        ENDIF vad2_micro_check

        vad2_tim=vad2_tim+now_time()-btimx

      ENDIF





      idtad_block: IF(MOD(grid%ntsd,GRID%IDTAD)==0)THEN
        btimx=now_time()







CALL HALO_NMM_I_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        IF (.NOT.ETAMP_PHYSICS) THEN






CALL HALO_NMM_I_3_sub ( grid, &
  num_moist, &
  moist, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

        ENDIF


        exch_tim=exch_tim+now_time()-btimx





        btimx=now_time()


      call check_grid(grid,config_flags,'before HAD2', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
        had2_micro_check: IF (ETAMP_PHYSICS) THEN


          CALL HAD2(                                                   &
     &              GRID%DOMDESC,                                      &
     &              grid%ntsd,GRID%DT,GRID%IDTAD,grid%dx_nmm,grid%dy_nmm              &
     &             ,grid%aeta1,grid%aeta2,grid%deta1,grid%deta2,grid%pdsl,grid%pdtop                 &
     &             ,grid%hbm2,grid%hbm3                                          &
     &             ,grid%q,grid%q2,grid%cwm,grid%u,grid%v,grid%z,grid%hydro                               &
     &             ,grid%n_iup_h,grid%n_iup_v                                    &
     &             ,grid%n_iup_adh,grid%n_iup_adv                                &
     &             ,grid%iup_h,grid%iup_v,grid%iup_adh,grid%iup_adv                        &
     &             ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                    &
     &             ,advect_Q2 &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)








          IF(.NOT.OPERATIONAL_PHYSICS)THEN
            DO K=KTS,KTE
            DO J=max(jds+( 0 ),jts-( 0  )),min(jde-( 0 ),jte+( 0  ))
            DO I=max(ids+( 0 ),its-( 0  )),min(ide-( 0 ),ite+( 0  ))
              MOIST(I,J,K,P_QV)=grid%q(I,J,K)/(1.-grid%q(I,J,K))
              WC = grid%cwm(I,J,K)
              QI = 0.
              QR = 0.
              QW = 0.
              FICE=grid%f_ice(I,K,J)
              FRAIN=grid%f_rain(I,K,J)

              IF(FICE>=1.)THEN
                QI=WC
              ELSEIF(FICE<=0.)THEN
                QW=WC
              ELSE
                QI=FICE*WC
                QW=WC-QI
              ENDIF

              IF(QW>0..AND.FRAIN>0.)THEN
                IF(FRAIN>=1.)THEN
                  QR=QW
                  QW=0.
                ELSE
                  QR=FRAIN*QW
                  QW=QW-QR
                ENDIF
              ENDIF

              MOIST(I,J,K,P_QC)=QW
              MOIST(I,J,K,P_QR)=QR

              IF (ETAMP_PHYSICS) THEN
                 MOIST(I,J,K,P_QI)=QI
                 MOIST(I,J,K,P_QS)=0.
              endif
              MOIST(I,J,K,P_QG)=0.
            ENDDO
            ENDDO
            ENDDO
          ENDIF


        ELSE had2_micro_check


          CALL HAD2_SCAL(                                              &
     &                   GRID%DOMDESC,                                 &
     &                   grid%ntsd,GRID%DT,GRID%IDTAD,grid%dx_nmm,grid%dy_nmm         &
     &                  ,grid%aeta1,grid%aeta2,grid%deta1,grid%deta2,grid%pdsl,grid%pdtop            &
     &                  ,grid%hbm2,grid%hbm3                                     &
     &                  ,grid%q2,grid%u,grid%v,grid%z,grid%hydro                                &
     &                  ,grid%n_iup_h,grid%n_iup_v                               &
     &                  ,grid%n_iup_adh,grid%n_iup_adv                           &
     &                  ,grid%iup_h,grid%iup_v,grid%iup_adh,grid%iup_adv                   &
     &                  ,grid%ihe,grid%ihw,grid%ive,grid%ivw                               &
     &                  ,1,1                                           &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                       &
     &                  ,IMS,IME,JMS,JME,KMS,KME                       &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)

          CALL HAD2_SCAL(                                              &
     &                   GRID%DOMDESC,                                 &
     &                   grid%ntsd,GRID%DT,GRID%IDTAD,grid%dx_nmm,grid%dy_nmm         &
     &                  ,grid%aeta1,grid%aeta2,grid%deta1,grid%deta2,grid%pdsl,grid%pdtop            &
     &                  ,grid%hbm2,grid%hbm3                                     &
     &                  ,MOIST,grid%u,grid%v,grid%z,grid%hydro                             &
     &                  ,grid%n_iup_h,grid%n_iup_v                               &
     &                  ,grid%n_iup_adh,grid%n_iup_adv                           &
     &                  ,grid%iup_h,grid%iup_v,grid%iup_adh,grid%iup_adv                   &
     &                  ,grid%ihe,grid%ihw,grid%ive,grid%ivw                               &
     &                  ,NUM_MOIST,2                                   &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                       &
     &                  ,IMS,IME,JMS,JME,KMS,KME                       &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)

          CALL HAD2_SCAL(                                              &
     &                   GRID%DOMDESC,                                 &
     &                   grid%ntsd,GRID%DT,GRID%IDTAD,grid%dx_nmm,grid%dy_nmm         &
     &                  ,grid%aeta1,grid%aeta2,grid%deta1,grid%deta2,grid%pdsl,grid%pdtop            &
     &                  ,grid%hbm2,grid%hbm3                                     &
     &                  ,SCALAR,grid%u,grid%v,grid%z,grid%hydro                            &
     &                  ,grid%n_iup_h,grid%n_iup_v                               &
     &                  ,grid%n_iup_adh,grid%n_iup_adv                           &
     &                  ,grid%iup_h,grid%iup_v,grid%iup_adh,grid%iup_adv                   &
     &                  ,grid%ihe,grid%ihw,grid%ive,grid%ivw                               &
     &                  ,NUM_SCALAR,2                                  &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                       &
     &                  ,IMS,IME,JMS,JME,KMS,KME                       &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)                             

          DO K=KTS,KTE 
          DO J=max(jds+( 0 ),jts-( 0  )),min(jde-( 0 ),jte+( 0  ))
          DO I=max(ids+( 0 ),its-( 0  )),min(ide-( 0 ),ite+( 0  ))
            grid%q(I,J,K)=MOIST(I,J,K,P_QV)/(1.+MOIST(I,J,K,P_QV))           
          ENDDO
          ENDDO    
          ENDDO   


        ENDIF had2_micro_check

        had2_tim=had2_tim+now_time()-btimx


      ENDIF idtad_block



      ENDIF not_euler  








      NUM_AEROSOLC=1

      IF(grid%ntsd<=0)THEN
        NTSD_rad=grid%ntsd
      ELSE




        NTSD_rad=grid%ntsd+1
      ENDIF






      call check_grid(grid,config_flags,'before RADIATION', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
      IF(MOD(NTSD_rad,GRID%NRADS)==0.OR.                               &
     &   MOD(NTSD_rad,GRID%NRADL)==0)THEN

        btimx=now_time()
        IF(OPERATIONAL_PHYSICS)THEN
          CALL UPDATE_MOIST(MOIST,grid%q,grid%cwm,grid%f_ice,grid%f_rain,N_MOIST           &
     &                     ,IDS,IDF,JDS,JDF,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE)
        ENDIF

        CALL RADIATION(NTSD_rad,GRID%DT,GRID%JULDAY,GRID%JULYR         &
     &                ,GRID%XTIME,GRID%JULIAN                          &
     &                ,IHRST,GRID%NPHS                                 &
     &                ,grid%glat,grid%glon,GRID%NRADS,GRID%NRADL                 &
     &                ,grid%deta1,grid%deta2,grid%aeta1,grid%aeta2,grid%eta1,grid%eta2,grid%pdtop,grid%pt      &
     &                ,grid%pd,grid%res,grid%pint,grid%t,grid%q,MOIST,grid%ths,grid%albedo,grid%epsr           &
     &                ,grid%f_ice,grid%f_rain                                    &
     &                ,grid%GD_CLOUD,grid%GD_CLOUD2                              &
     &                ,grid%sm,grid%hbm2,grid%cldfra,N_MOIST,RESTRT                   &
     &                ,grid%rlwtt,grid%rswtt,grid%rlwin,grid%rswin,grid%rswinc,grid%rswout           &
     &                ,grid%rlwtoa,grid%rswtoa,grid%czmean                            &
     &                ,grid%cfracl,grid%cfracm,grid%cfrach,grid%sigt4                      &
     &                ,grid%acfrst,grid%ncfrst,grid%acfrcv,grid%ncfrcv                     &
     &                ,grid%cuppt,grid%vegfrc,grid%sno,grid%htop,grid%hbot                      &
     &                ,grid%z,grid%sice,NUM_AEROSOLC,NUM_OZMIXM        &
     &                ,OZMIXM,grid%PIN,grid%LEVSIZ                     &
     &                ,GRID,CONFIG_FLAGS                               &
     &                ,RTHRATEN                                        &  
     &                ,grid%re_cloud,grid%re_ice,grid%re_snow          &  
     &                ,grid%has_reqc,grid%has_reqi,grid%has_reqs       &  
     &                ,grid%SWUPT,grid%SWUPTC,grid%SWDNT,grid%SWDNTC   &
     &                ,grid%SWUPB,grid%SWUPBC,grid%SWDNB,grid%SWDNBC   &
     &                ,grid%LWUPT,grid%LWUPTC,grid%LWDNT,grid%LWDNTC   &
     &                ,grid%LWUPB,grid%LWUPBC,grid%LWDNB,grid%LWDNBC   &
     &                ,grid%ACSWUPT,grid%ACSWUPTC,grid%ACSWDNT,grid%ACSWDNTC   &
     &                ,grid%ACSWUPB,grid%ACSWUPBC,grid%ACSWDNB,grid%ACSWDNBC   &
     &                ,grid%ACLWUPT,grid%ACLWUPTC,grid%ACLWDNT,grid%ACLWDNTC   &
     &                ,grid%ACLWUPB,grid%ACLWUPBC,grid%ACLWDNB,grid%ACLWDNBC   &
     &                 ,grid%swvisdir ,grid%swvisdif &  
     &                 ,grid%swnirdir ,grid%swnirdif &  
     &                ,IDS,IDF,JDS,JDF,KDS,KDE                         &
     &                ,IMS,IME,JMS,JME,KMS,KME                         &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)

        DO J=jts,min(jde-1,jte)
        DO I=its,min(ide-1,ite)
          grid%gsw(I,J)=grid%rswin(I,J)-grid%rswout(I,J)
        ENDDO
        ENDDO
      btimx=now_time()







CALL HALO_NMM_RAD_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


      exch_tim=exch_tim+now_time()-btimx







        radiation_tim=radiation_tim+now_time()-btimx
      ENDIF





      btimx=now_time()





      call check_grid(grid,config_flags,'before RDTEMP', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
      CALL RDTEMP(grid%ntsd,GRID%DT,GRID%JULDAY,GRID%JULYR                  &
     &           ,GRID%XTIME,IHRST,grid%glat,grid%glon                           &
     &           ,grid%czen,grid%czmean,grid%t,grid%rswtt,grid%rlwtt,grid%hbm2                       &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                              &
     &           ,IMS,IME,JMS,JME,KMS,KME                              &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)

      rdtemp_tim=rdtemp_tim+now_time()-btimx







        btimx=now_time()
        CALL ATM_GETSST(grid%sst,grid%sm)
        sst_tim=sst_tim+now_time()-btimx





      IF(MOD(grid%ntsd,GRID%NPHS)==0)THEN

        btimx=now_time()

        IF(OPERATIONAL_PHYSICS                                         &
     &    .AND.MOD(NTSD_rad,GRID%NRADS)/=0                             &
     &    .AND.MOD(NTSD_rad,GRID%NRADL)/=0)THEN
          CALL UPDATE_MOIST(MOIST,grid%q,grid%cwm,grid%f_ice,grid%f_rain,N_MOIST           &
     &                     ,IDS,IDF,JDS,JDF,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE)
        ENDIF

        call check_grid(grid,config_flags,'before TURBL', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
        CALL TURBL(grid%ntsd,GRID%DT,GRID%NPHS,RESTRT                       &
     &            ,N_MOIST,GRID%NUM_SOIL_LAYERS,grid%sldpth,grid%dzsoil          &
     &            ,grid%deta1,grid%deta2,grid%aeta1,grid%aeta2,grid%eta1,grid%eta2,grid%pdtop,grid%pt          &
     &            ,grid%sm,grid%hbm2,grid%vbm2,grid%dx_nmm,grid%dfrlg                           &
     &            ,grid%czen,grid%czmean,grid%sigt4,grid%rlwin,grid%rswin,grid%radot                 &
     &            ,grid%pd,grid%res,grid%pint,grid%t,grid%q,grid%cwm,grid%f_ice,grid%f_rain,grid%sr                 &
     &            ,grid%q2,grid%u,grid%v,grid%ths,grid%nmm_tsk,grid%sst,grid%prec,grid%sno                     &
     &            ,grid%fis,grid%z0,grid%mz0,grid%z0base,grid%ustar,grid%mixht,grid%pblh,grid%lpbl,grid%el_pbl    &   
     &            ,MOIST,grid%rmol,grid%mol                                      &
     &            ,grid%exch_h,grid%exch_m,grid%f,grid%akhs,grid%akms,grid%akhs_out,grid%akms_out         &
     &            ,grid%thz0,grid%qz0,grid%uz0,grid%vz0,grid%qsh,grid%mavail                         &
     &            ,grid%stc,grid%smc,grid%cmc,grid%smstav,grid%smstot,grid%ssroff,grid%bgroff             &
     &            ,grid%ivgtyp,grid%isltyp,grid%vegfrc,grid%shdmin,grid%shdmax,grid%grnflx           &
     &            ,grid%snotime                                             &
     &            ,grid%sfcexc,grid%acsnow,grid%acsnom,grid%snopcx,grid%sice,grid%tg,grid%soiltb          &
     &            ,grid%albsi,grid%icedepth,grid%snowsi                                                   &
     &            ,grid%albase,grid%mxsnal,grid%albedo,grid%sh2o,grid%si,grid%epsr,grid%embck             &
     &            ,grid%u10,grid%v10,grid%uoce,grid%voce,grid%th10,grid%q10,grid%tshltr,grid%qshltr,grid%pshltr               &
     &            ,grid%t2,grid%qsg,grid%qvg,grid%qcg,grid%soilt1,grid%tsnav,grid%smfr3d,grid%keepfr3dflag     &
     &            ,grid%twbs,grid%qwbs,grid%taux,grid%tauy,grid%sfcshx,grid%sfclhx,grid%sfcevp,RTHRATEN                      &
     &            ,grid%potevp,grid%potflx,grid%subshx                                &
     &            ,grid%aphtim,grid%ardsw,grid%ardlw,grid%asrfc                            &
     &            ,grid%rswout,grid%rswtoa,grid%rlwtoa                                &
     &            ,grid%aswin,grid%aswout,grid%aswtoa,grid%alwin,grid%alwout,grid%alwtoa             &
     &            ,grid%uz0h,grid%vz0h,grid%dudt,grid%dvdt,grid%ugwdsfc,grid%vgwdsfc,grid%sfenth          & 
     &            ,RTHBLTEN,RQVBLTEN                                   & 
     &            ,GRID%PCPFLG,grid%ddata                                   &
     &            ,grid%hstdv,grid%hcnvx,grid%hasyw,grid%hasys,grid%hasysw,grid%hasynw,grid%hlenw,grid%hlens   & 
     &            ,grid%hlensw,grid%hlennw,grid%hangl,grid%hanis,grid%hslop,grid%hzmax,grid%crot,grid%srot     & 
     &            ,grid%dew                                            & 
     &            ,grid%rc_mf                                          & 
     &            ,GRID,CONFIG_FLAGS                                   &
     &            ,grid%ihe,grid%ihw,grid%ive,grid%ivw                 &
     &            ,GRID%DISHEAT,GRID%DKU3D,GRID%DKT3D                          &
     &            ,GRID%HPBL2D, GRID%EVAP2D, GRID%HEAT2D,GRID%RC2D               &  
     &            ,GRID%SFCHEADRT,GRID%INFXSRT,GRID%SOLDRAIN           &  
     &            ,grid%cd_out,grid%ch_out                             &
     &            ,IDS,IDF,JDS,JDF,KDS,KDE                             &
     &            ,IMS,IME,JMS,JME,KMS,KME                             &
     &            ,IPS,IPE,JPS,JPE,KPS,KPE                             &
     &            ,ITS,ITE,JTS,JTE,KTS,KTE)





        turbl_tim=turbl_tim+now_time()-btimx





















      btimx=now_time()
      call ATM_DOFLUXES(grid%twbs,grid%qwbs,grid%rlwin,grid%rswin,grid%radot,grid%rswout, &
      grid%taux,grid%tauy,grid%pint,grid%prec,grid%u10,grid%v10)
      flux_tim=flux_tim+now_time()-btimx



      IF(GRID%ID .EQ. 1 .AND. MOD(grid%ntsd,grid%NPHS)==0)THEN
        btimx=now_time()
        flux_tim=flux_tim+now_time()-btimx
      ENDIF


        btimx=now_time()







CALL HALO_NMM_TURBL_A_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )








CALL HALO_NMM_TURBL_B_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        exch_tim=exch_tim+now_time()-btimx







        btimx=now_time()
        CALL UV_H_TO_V(grid%ntsd,GRID%DT,GRID%NPHS,grid%uz0h,grid%vz0h,grid%uz0,grid%vz0         &
     &                ,grid%dudt,grid%dvdt,grid%u,grid%v,grid%hbm2,grid%ive,grid%ivw                       &
     &                ,IDS,IDF,JDS,JDF,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
        uv_htov_tim=uv_htov_tim+now_time()-btimx





        btimx=now_time()







CALL HALO_NMM_J_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        IF (.NOT.ETAMP_PHYSICS) THEN






CALL HALO_NMM_J_3_sub ( grid, &
  num_moist, &
  moist, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

        ENDIF


        exch_tim=exch_tim+now_time()-btimx





        ICLTEND=-1
        btimx=now_time()

        call check_grid(grid,config_flags,'before CLTEND', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
        CALL CLTEND(ICLTEND,GRID%NPHS,grid%t,grid%t_old,grid%t_adj                    &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)

        cltend_tim=cltend_tim+now_time()-btimx
     ENDIF





      IF(MOD(grid%ntsd,GRID%NCNVC)==0.AND.                              &
     &   (CONFIG_FLAGS%CU_PHYSICS.eq.KFETASCHEME .or.                     &
     &   CONFIG_FLAGS%CU_PHYSICS.eq.OSASSCHEME .or.                       &
     &   CONFIG_FLAGS%CU_PHYSICS.eq.NSASSCHEME .or.                       &
     &   CONFIG_FLAGS%CU_PHYSICS.eq.MESO_SAS .or.                         &   
     &   CONFIG_FLAGS%CU_PHYSICS.eq.SASSCHEME))THEN                       

        btimx=now_time()







CALL HALO_NMM_C_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        exch_tim=exch_tim+now_time()-btimx




     ENDIF

      convection: IF(CONFIG_FLAGS%CU_PHYSICS/=0)THEN
        btimx=now_time()



        IF(CONFIG_FLAGS%CU_PHYSICS==GDSCHEME.OR.                        &
     &  CONFIG_FLAGS%CU_PHYSICS==TIEDTKESCHEME.OR.                      &
     &  CONFIG_FLAGS%CU_PHYSICS==KFETASCHEME)THEN
          DT_INV=1./GRID%DT
          DO J=JMS,JME
          DO K=KMS,KME
          DO I=IMS,IME
            TTEN(I,K,J)=(grid%t(I,J,K)-TTEN(I,K,J))*DT_INV
            QTEN(I,K,J)=(grid%q(I,J,K)-QTEN(I,K,J))*DT_INV
          ENDDO
          ENDDO
          ENDDO
        ENDIF



        IF(OPERATIONAL_PHYSICS                                         &
     &    .AND.MOD(NTSD_rad,GRID%NRADS)/=0                             &
     &    .AND.MOD(NTSD_rad,GRID%NRADL)/=0                             &
     &    .AND.MOD(grid%ntsd,GRID%NPHS)/=0)THEN
          CALL UPDATE_MOIST(MOIST,grid%q,grid%cwm,grid%f_ice,grid%f_rain,N_MOIST           &
     &                     ,IDS,IDF,JDS,JDF,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE)
       ENDIF


       call wrf_message('call cucnvc')
       call start_timing
        call check_grid(grid,config_flags,'before CUCNVC', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
        CALL CUCNVC(grid%ntsd,GRID%DT,GRID%NCNVC,GRID%NRADS,GRID%NRADL      &
     &             ,GPS,RESTRT,grid%hydro,grid%cldefi,N_MOIST,GRID%ENSDIM        &
     &             ,MOIST                                              &
     &             ,grid%deta1,grid%deta2,grid%aeta1,grid%aeta2,grid%eta1,grid%eta2                  &
     &             ,grid%f_ice,grid%f_rain                                       &

     &             ,grid%apr_gr,grid%apr_w,grid%apr_mc,TTEN,QTEN                      &
     &             ,grid%apr_st,grid%apr_as,grid%apr_capma                            &
     &             ,grid%apr_capme,grid%apr_capmi                                &
     &             ,grid%mass_flux,grid%xf_ens                                   &
     &             ,grid%pr_ens,grid%gsw                                         &
     &             ,grid%GD_CLOUD,grid%GD_CLOUD2,grid%ktop_deep                  &
     &             ,grid%pdtop,grid%pt,grid%pd,grid%res,grid%pint,grid%t,grid%q,grid%cwm,grid%tcucn                 &
     &             ,grid%omgalf,grid%u,grid%v,grid%w,grid%z,grid%fis,grid%w0avg                           &
     &             ,grid%prec,grid%acprec,grid%cuprec,grid%cuppt,grid%cprate                    &
     &             ,grid%sm,grid%hbm2,grid%pblh,grid%lpbl,grid%cnvbot,grid%cnvtop                         &
     &             ,grid%htop,grid%hbot,grid%htopd,grid%hbotd,grid%htops,grid%hbots                  &
     &             ,RTHBLTEN,RQVBLTEN,RTHRATEN                         & 
     &             ,grid%twbs,grid%qwbs                                &
     &                 ,grid%DUCUDT, grid%DVCUDT, GRID%MOMMIX, grid%random             &
     &             ,grid%hpbl2d,grid%evap2d,grid%heat2d                &
     &             ,grid%avcnvc,grid%acutim,grid%ihe,grid%ihw          &
     &             ,GRID,CONFIG_FLAGS                                  &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,IPS,IPE,JPS,JPE,KPS,KPE                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)
        call check_grid(grid,config_flags,'after CUCNVC', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
        call end_timing('cucnvc')


        cucnvc_tim=cucnvc_tim+now_time()-btimx











        IF(MOD(grid%ntsd, GRID%NCNVC).eq.0.and.                 &
     &    (CONFIG_FLAGS%CU_PHYSICS.eq.OSASSCHEME.or.            &
     &     CONFIG_FLAGS%CU_PHYSICS.eq.NSASSCHEME.or.            &
     &     CONFIG_FLAGS%CU_PHYSICS.eq.MESO_SAS.or.              & 
     &     CONFIG_FLAGS%CU_PHYSICS.eq.SASSCHEME))THEN 


        btimx=now_time()







CALL HALO_NMM_SAS_A_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )








CALL HALO_NMM_SAS_B_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


        exch_tim=exch_tim+now_time()-btimx




        btimx=now_time()


        CALL UV_H_TO_V(grid%NTSD,GRID%DT,GRID%NCNVC,grid%UZ0H,grid%VZ0H,grid%UZ0,grid%VZ0         &
     &                ,grid%DUCUDT,grid%DVCUDT,grid%U,grid%V,grid%HBM2,grid%IVE,grid%IVW                   &
     &                ,IDS,IDF,JDS,JDF,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
        uv_htov_tim=uv_htov_tim+now_time()-btimx


     ENDIF 



  ENDIF convection





      IF(MOD(grid%ntsd,GRID%NPHS)==0)THEN
        btimx=now_time()

        call check_grid(grid,config_flags,'before GSMDRIVE', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
        CALL GSMDRIVE(grid%ntsd,GRID%DT,GRID%NPHS,N_MOIST                   &
     &               ,grid%dx_nmm(ITS,JC),GRID%DY,GRID%LPI,grid%sm,grid%hbm2,grid%fis               &
     &               ,grid%deta1,grid%deta2,grid%aeta1,grid%aeta2,grid%eta1,grid%eta2                &
     &               ,grid%pdtop,grid%pt,grid%pd,grid%res,grid%pint,grid%t,grid%q,grid%cwm,grid%train               &
     &               ,MOIST,SCALAR,NUM_SCALAR                          &
     &               ,grid%f_ice,grid%f_rain,grid%f_rimef,grid%sr                          &
     &               ,grid%prec,grid%acprec,grid%avrain                               &
     &               ,grid%mp_restart_state                                 &
     &               ,grid%tbpvs_state                                      &
     &               ,grid%tbpvs0_state                                     &
     &               ,GRID,CONFIG_FLAGS                                &
     &               ,grid%re_cloud,grid%re_ice,grid%re_snow           &  
     &               ,grid%has_reqc,grid%has_reqi,grid%has_reqs        &  
     &               ,diag_flag                                        &  
     &               ,IDS,IDF,JDS,JDF,KDS,KDE                          &
     &               ,IMS,IME,JMS,JME,KMS,KME                          &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)
        call check_grid(grid,config_flags,'after GSMDRIVE', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)

        gsmdrive_tim=gsmdrive_tim+now_time()-btimx





        IF (GRID%PCPFLG) THEN
          btimx=now_time()

          CALL CHKSNOW(grid%ntsd,GRID%DT,GRID%NPHS,grid%sr,PPTDAT                 &
     &      ,IDS,IDE,JDS,JDE,KDS,KDE                                    &
     &      ,IMS,IME,JMS,JME,KMS,KME                                    &
     &      ,ITS,ITE,JTS,JTE,KTS,KTE)
          CALL ADJPPT(grid%ntsd,GRID%DT,GRID%NPHS,grid%prec,grid%lspa,PPTDAT,grid%ddata     &
     &      ,IDS,IDE,JDS,JDE,KDS,KDE                                    &
     &      ,IMS,IME,JMS,JME,KMS,KME                                    &
     &      ,ITS,ITE,JTS,JTE,KTS,KTE)

          adjppt_tim=adjppt_tim+now_time()-btimx
        ENDIF





        ICLTEND=0
        btimx=now_time()

        CALL CLTEND(ICLTEND,GRID%NPHS,grid%t,grid%t_old,grid%t_adj                    &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)

        cltend_tim=cltend_tim+now_time()-btimx
      ENDIF





      ICLTEND=1
      btimx=now_time()

      CALL CLTEND(ICLTEND,GRID%NPHS,grid%t,grid%t_old,grid%t_adj                      &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                              &
     &           ,IMS,IME,JMS,JME,KMS,KME                              &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)

      cltend_tim=cltend_tim+now_time()-btimx





      btimx=now_time()







CALL HALO_NMM_K_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


      exch_tim=exch_tim+now_time()-btimx





      btimx=now_time()

      CALL HDIFF(grid%ntsd,GRID%DT,grid%fis,grid%dy_nmm,grid%hdac,grid%hdacv         &
     &          ,grid%hbm2,grid%deta1,GRID%SIGMA                       &
     &          ,grid%t,grid%q,grid%u,grid%v,grid%q2,grid%z,grid%w,grid%sm,grid%sice,grid%h_diff    &
     &          ,grid%def3d                                              &
     &          ,grid%ihe,grid%ihw,grid%ive,grid%ivw                   &
     &          ,CONFIG_FLAGS                                          &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)

        call check_grid(grid,config_flags,'after HDIFF', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
      IF(.NOT.OPERATIONAL_PHYSICS)THEN
        DO K=KTS,KTE
        DO J=max(jds+( 0 ),jts-( 0  )),min(jde-( 0 ),jte+( 0  ))
        DO I=max(ids+( 0 ),its-( 0  )),min(ide-( 0 ),ite+( 0  ))

          MOIST(I,J,K,P_QV)=grid%q(I,J,K)/(1.-grid%q(I,J,K))           
        ENDDO
        ENDDO
        ENDDO
      ENDIF

      hdiff_tim=hdiff_tim+now_time()-btimx





      btimx=now_time()







CALL HALO_NMM_L_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )








CALL HALO_NMM_L_3_sub ( grid, &
  num_moist, &
  moist, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )



      exch_tim=exch_tim+now_time()-btimx





      btimx=now_time()

        call check_grid(grid,config_flags,'before mass_boundary', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
      CALL MASS_BOUNDARY(GRID%ID,grid%ntsd,GRID%DT,NEST,NUNIT_NBC,NBOCO,LAST_TIME,TSPH &
     &          ,LB,grid%eta1,grid%eta2,grid%pdtop,grid%pt,grid%res                              &
     &          ,grid%PD_BXS,grid%PD_BXE,grid%PD_BYS,grid%PD_BYE,grid%T_BXS,grid%T_BXE,grid%T_BYS,grid%T_BYE    &
     &          ,grid%Q_BXS,grid%Q_BXE,grid%Q_BYS,grid%Q_BYE,grid%U_BXS,grid%U_BXE,grid%U_BYS,grid%U_BYE,grid%V_BXS  &
     &          ,grid%V_BXE,grid%V_BYS,grid%V_BYE,grid%Q2_BXS,grid%Q2_BXE,grid%Q2_BYS,grid%Q2_BYE  &
     &          ,grid%PD_BTXS,grid%PD_BTXE,grid%PD_BTYS        &
     &          ,grid%PD_BTYE,grid%T_BTXS,grid%T_BTXE,grid%T_BTYS,grid%T_BTYE,grid%Q_BTXS,grid%Q_BTXE      &
     &          ,grid%Q_BTYS,grid%Q_BTYE,grid%U_BTXS,grid%U_BTXE,grid%U_BTYS,grid%U_BTYE,grid%V_BTXS       &
     &          ,grid%V_BTXE,grid%V_BTYS,grid%V_BTYE,grid%Q2_BTXS,grid%Q2_BTXE,grid%Q2_BTYS,grid%Q2_BTYE   &
     &          ,grid%pd,grid%t,grid%q,grid%q2,grid%pint &
     &          ,GRID%SPEC_BDY_WIDTH,grid%z                                  &
     &          ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                        &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                                &
     &          ,IMS,IME,JMS,JME,KMS,KME                                &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
        call check_grid(grid,config_flags,'after MASS_BOUNDARY', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
      if(ETAMP_PHYSICS) then

      CALL MP_BULK_BOUNDARY(GRID%ID,grid%ntsd,GRID%DT &
     &          ,LB,grid%eta1,grid%eta2,grid%pdtop,grid%pt              &
     &          ,grid%CWM_BXS,grid%CWM_BXE,grid%CWM_BYS,grid%CWM_BYE    &
     &          ,grid%CWM_BTXS,grid%CWM_BTXE,grid%CWM_BTYS,grid%CWM_BTYE&
     &          ,grid%q,grid%cwm                                        &
     &          ,MOIST,N_MOIST,SCALAR,NUM_SCALAR                        &
     &          ,GRID%SPEC_BDY_WIDTH                                    &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                                &
     &          ,IMS,IME,JMS,JME,KMS,KME                                &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
      else
         CALL MP_SPECIES_BDY(grid%id,1,grid%dt,               &
              grid%CWM,grid%Q,                                &
              MOIST,N_MOIST,                                  &
              MOIST_bxs,MOIST_bxe,MOIST_bys,MOIST_bye,        &
              MOIST_btxs,MOIST_btxe,MOIST_btys,MOIST_btye,    &
              SCALAR,NUM_SCALAR,                              &
              SCALAR_bxs,SCALAR_bxe,SCALAR_bys,SCALAR_bye,    &
              SCALAR_btxs,SCALAR_btxe,SCALAR_btys,SCALAR_btye,&
              ids,idf,jds,jdf,kds,kde,                        &
              ims,ime,jms,jme,kms,kme,                        &
              its,ite,jts,jte,kts,kte)
      endif
        call check_grid(grid,config_flags,'after boundaries', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)

      bocoh_tim=bocoh_tim+now_time()-btimx











 2003 CONTINUE





      btimx=now_time()







CALL HALO_NMM_A_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


      IF (ETAMP_PHYSICS) THEN






CALL HALO_NMM_A_3_sub ( grid, &
  num_moist, &
  moist, &
  num_scalar, &
  scalar, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

      ENDIF


      exch_tim=exch_tim+now_time()-btimx





      btimx=now_time()

      call check_grid(grid,config_flags,'before PFDHT', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
      CALL PFDHT(grid%ntsd,LAST_TIME,grid%pt,grid%deta1,grid%deta2,grid%pdtop,grid%res,grid%fis           &
     &          ,grid%hydro,GRID%SIGMA,FIRST,grid%dx_nmm,grid%dy_nmm                  &
     &          ,grid%hbm2,grid%vbm2,grid%vbm3                                        &
     &          ,grid%fdiv,grid%fcp,grid%wpdar,grid%dfl,grid%cpgfu,grid%cpgfv                        &
     &          ,grid%pd,grid%pdsl,grid%t,grid%q,grid%u,grid%v,grid%cwm,grid%omgalf,grid%pint,grid%dwdt                  &
     &          ,grid%rtop,grid%div,grid%few,grid%fns,grid%fne,grid%fse                              &
     &          ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                       &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
      call check_grid(grid,config_flags,'after PFDHT', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)


      pfdht_tim=pfdht_tim+now_time()-btimx





      btimx=now_time()







CALL HALO_NMM_B_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


      exch_tim=exch_tim+now_time()-btimx





      btimx=now_time()

      call check_grid(grid,config_flags,'before DDAMP', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
      CALL DDAMP(grid%ntsd,GRID%DT,grid%deta1,grid%deta2,grid%pdsl      &
     &          ,grid%pdtop,grid%div,grid%hbm2                          &
     &          ,grid%t,grid%u,grid%v,grid%ddmpu,grid%ddmpv             &
     &          ,grid%ihe,grid%ihw,grid%ive,grid%ivw                    &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                                &
     &          ,IMS,IME,JMS,JME,KMS,KME                                &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
      call check_grid(grid,config_flags,'after DDAMP', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)

      ddamp_tim=ddamp_tim+now_time()-btimx




      IF(FIRST.AND.grid%ntsd==0)THEN
        FIRST=.FALSE.
        btimx=now_time()





        exch_tim=exch_tim+now_time()-btimx





        GO TO 2000
      ENDIF





      btimx=now_time()







CALL HALO_NMM_C_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )


      exch_tim=exch_tim+now_time()-btimx





      btimx=now_time()

      CALL BOCOV(GRID%ID,grid%ntsd,GRID%DT,LB,grid%U_BXS,grid%U_BXE,grid%U_BYS,grid%U_BYE,grid%V_BXS &
     &          ,grid%V_BXE,grid%V_BYS,grid%V_BYE,grid%U_BTXS,grid%U_BTXE,grid%U_BTYS,grid%U_BTYE,grid%V_BTXS  &
     &          ,grid%V_BTXE,grid%V_BTYS,grid%V_BTYE,grid%u,grid%v                              &
     &          ,GRID%SPEC_BDY_WIDTH                                   &
     &          ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                       &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE )
 

      bocov_tim=bocov_tim+now_time()-btimx





      DO K=KTS,KTE
      DO J=JTS,JTE
      DO I=ITS,ITE
        grid%tke_pbl(I,J,K)=0.5*grid%q2(I,J,K) 
      ENDDO
      ENDDO
      ENDDO



   IF ( config_flags%compute_radar_ref .EQ. 1 ) THEN
   CALL wrf_debug ( 200 , ' call diagnostic_driver' )

   call check_grid(grid,config_flags,'before diag o c r', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
   CALL diagnostic_output_calc_refl(                                   &
      &              DIAGFLAG=diag_flag                                &
      &             ,REFD_MAX=grid%refd_max                            &
      &             ,refl_10cm=grid%refl_10cm                          &
      &             ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
      &             ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
      &             ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
      &                                                          )
   call check_grid(grid,config_flags,'after diag o c r', &
            ids,ide,jds,jde,kds,kde, &
            ims,ime,jms,jme,kms,kme, &
            its,ite,jts,jte,kts,kte)
   END IF




      IF(LAST_TIME.AND.ALLOCATED(PPTDAT))THEN
        DEALLOCATE(PPTDAT,STAT=ISTAT)
      ENDIF



      solve_tim=solve_tim+now_time()-ttim
      ttim=now_time()





      sum_tim=pdte_tim+adve_tim+vtoa_tim+vadz_tim+hadz_tim+eps_tim     &
     &       +vad2_tim+had2_tim+radiation_tim+rdtemp_tim+turbl_tim     &
     &       +cltend_tim+cucnvc_tim+gsmdrive_tim+hdiff_tim             &
     &       +bocoh_tim+pfdht_tim+ddamp_tim+bocov_tim+uv_htov_tim      &
     &       +exch_tim+adjppt_tim

      if(mod(grid%ntsd,n_print_time)==0)then
        sum_tim = adjppt_tim + exch_tim + pdte_tim + adve_tim + vtoa_tim + &
             vadz_tim + hadz_tim + eps_tim + vad2_tim + had2_tim +         &
             radiation_tim + rdtemp_tim + turbl_tim + cltend_tim +         &
             cucnvc_tim + gsmdrive_tim + hdiff_tim + bocoh_tim +           &
             pfdht_tim + ddamp_tim + bocov_tim + uv_htov_tim + diag_tim +  &
             tornado_tim
        sum_tim = sum_tim + sst_tim + flux_tim + hifreq_tim

17      format(A16,F13.6,A5,F7.3,'%')
        write(message,*)' grid%ntsd=',grid%ntsd,' solve_tim=',solve_tim    &
     &           ,' sum_tim=',sum_tim
        call wrf_message(trim(message))
        write(message,17)' pdte_tim=',pdte_tim,' pct=',pdte_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' adve_tim=',adve_tim,' pct=',adve_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' vtoa_tim=',vtoa_tim,' pct=',vtoa_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' vadz_tim=',vadz_tim,' pct=',vadz_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' hadz_tim=',hadz_tim,' pct=',hadz_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' eps_tim=',eps_tim,' pct=',eps_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' vad2_tim=',vad2_tim,' pct=',vad2_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' had2_tim=',had2_tim,' pct=',had2_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' radiation_tim=',radiation_tim,' pct=',radiation_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' rdtemp_tim=',rdtemp_tim,' pct=',rdtemp_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' turbl_tim=',turbl_tim,' pct=',turbl_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' cltend_tim=',cltend_tim,' pct=',cltend_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' cucnvc_tim=',cucnvc_tim,' pct=',cucnvc_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' gsmdrive_tim=',gsmdrive_tim,' pct=',gsmdrive_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' adjppt_tim=',adjppt_tim,' pct=',adjppt_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' hdiff_tim=',hdiff_tim,' pct=',hdiff_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' bocoh_tim=',bocoh_tim,' pct=',bocoh_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' pfdht_tim=',pfdht_tim,' pct=',pfdht_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' ddamp_tim=',ddamp_tim,' pct=',ddamp_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' bocov_tim=',bocov_tim,' pct=',bocov_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' uv_h_to_v_tim=',uv_htov_tim,' pct=',uv_htov_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' exch_tim=',exch_tim,' pct=',exch_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' diag_tim=',diag_tim,' pct=',diag_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' tornado_tim=',tornado_tim,' pct=',tornado_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' sst_tim=',sst_tim,' pct=',sst_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' flux_tim=',flux_tim,' pct=',flux_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,17)' hifreq_tim=',hifreq_tim,' pct=',hifreq_tim/sum_tim*100.
        call wrf_message(trim(message))





        call field_stats(grid%t,mype,mpi_comm_comp                          &
     &                  ,ids,ide,jds,jde,kds,kde                       &
     &                  ,ims,ime,jms,jme,kms,kme                       &
     &                  ,its,ite,jts,jte,kts,kte)
      endif


      DEALLOCATE(TTEN,STAT=ISTAT)
      DEALLOCATE(QTEN,STAT=ISTAT)
      DEALLOCATE(RTHRATEN,STAT=ISTAT)
      DEALLOCATE(RTHBLTEN,STAT=ISTAT)
      DEALLOCATE(RQVBLTEN,STAT=ISTAT)










   IF ( grid%num_moves.EQ.-99 ) THEN
     btimx=now_time()
      call start_timing()
      call stats_for_move(grid,config_flags &
                         ,IDS,IDE,JDS,JDE,KDS,KDE &
                         ,IMS,IME,JMS,JME,KMS,KME &
                         ,IPS,IPE,JPS,JPE,KPS,KPE &
                         ,ITS,ITE,JTS,JTE,KTS,KTE)
3303  format('stats_for_move on domain ',I0)
      write(message,3303) grid%id
      call end_timing(message)
   CALL wrf_debug ( 100 , 'nmm stats_for_move: after advection' )
     diag_tim=diag_tim+now_time()-btimx
   ENDIF
   hwrfx_mlsp: if(grid%vortex_tracker /= 1) then
     btimx=now_time()




     IF(grid%id .EQ. 1 .AND. MOD(grid%NTSD,n_print_time)==0)THEN
      call wrf_debug(1,'COMPUTING MSLP OVER THE PARENT DOMAIN')

      CALL MSLP_DIAG (grid%MSLP,grid%PINT,grid%T,grid%Q                &
                     ,grid%FIS,grid%PD,grid%DETA1,grid%DETA2,grid%PDTOP     &
                     ,IDS,IDF,JDS,JDF,KDS,KDE      &
                     ,IMS,IME,JMS,JME,KMS,KME      &
                     ,ITS,ITE,JTS,JTE,KTS,KTE      )
     ENDIF

     diag_tim=diag_tim+now_time()-btimx
   endif hwrfx_mlsp










      btimx=now_time()
      call ATM_SENDFLUXES
      flux_tim=flux_tim+now_time()-btimx














     IF(mod(grid%NTSD,grid%ntornado)==0) then
        btimx=now_time()
        CALL CALC_TORNADO_GENESIS(GRID,CONFIG_FLAGS)
        tornado_tim=tornado_tim+now_time()-btimx
     ENDIF

     IF(mod(grid%NTSD,grid%ntornado)==0) then
      have_best: if(size(grid%best_mslp)>1) then
         have_membrane: if(size(grid%membrane_mslp)>1) then
            call CALC_BEST_MSLP(grid%best_mslp,grid%mslp, &
                 grid%membrane_mslp,grid%fis, &
                 IDS,IDE,JDS,JDE,KDS,KDE, &
                 IMS,IME,JMS,JME,KMS,KME, &
                 ITS,ITE,JTS,JTE,KTS,KTE)
         else
            
            
            do j=jps,min(jde-1,jpe)
               do i=ips,min(ide-1,ipe)
                  grid%best_mslp(i,j)=grid%mslp(i,j)
               enddo
            enddo
         endif have_membrane
      endif have_best
     ENDIF

     IF(grid%hifreq_lun /= 0) THEN
        btimx=now_time()
        CALL HIFREQ_WRITE(grid%hifreq_lun,GRID%NTSD,GRID%DT,GRID%HLAT,GRID%HLON    &
                      ,GRID%U10,GRID%V10,grid%pint,grid%t,grid%q                   &
                      ,grid%fis,grid%pd,grid%pdtop,grid%deta1,grid%deta2           &
                      ,IDS,IDF,JDS,JDF,KDS,KDE                                     &
                      ,IMS,IME,JMS,JME,KMS,KME                                     &
                      ,ITS,ITE,JTS,JTE,KTS,KTE   )
        hifreq_tim=hifreq_tim+now_time()-btimx
     ENDIF


 

      solve_tim=solve_tim+now_time()-ttim
      Return







      END SUBROUTINE SOLVE_NMM



      SUBROUTINE TWR(ARRAY,KK,FIELD,ntsd,MYPE,NPES,MPI_COMM_COMP       &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &              ,IMS,IME,JMS,JME,KMS,KME                           &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)


      USE MODULE_EXT_INTERNAL

      IMPLICIT NONE
      INCLUDE "mpif.h"


      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                    &
     &                     ,KK,MPI_COMM_COMP,MYPE,NPES,ntsd

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME+KK),INTENT(IN) :: ARRAY

      CHARACTER(*),INTENT(IN) :: FIELD



      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
      INTEGER,DIMENSION(2) :: IM_REM,JM_REM,IT_REM,JT_REM

      INTEGER :: I,IENDX,IER,IPE,IRECV,IRTN,ISEND,IUNIT                &
     &          ,J,K,N,NLEN,NSIZE
      INTEGER :: ITS_REM,ITE_REM,JTS_REM,JTE_REM

      REAL,DIMENSION(IDS:IDE,JDS:JDE) :: TWRITE
      REAL,ALLOCATABLE,DIMENSION(:) :: VALUES
      CHARACTER(5) :: TIMESTEP
      CHARACTER(6) :: FMT
      CHARACTER(12) :: FILENAME




      IF(ntsd<=9)THEN
        FMT='(I1.1)'
        NLEN=1
      ELSEIF(ntsd<=99)THEN
        FMT='(I2.2)'
        NLEN=2
      ELSEIF(ntsd<=999)THEN
        FMT='(I3.3)'
        NLEN=3
      ELSEIF(ntsd<=9999)THEN
        FMT='(I4.4)'
        NLEN=4
      ELSEIF(ntsd<=99999)THEN
        FMT='(I5.5)'
        NLEN=5
      ENDIF
      WRITE(TIMESTEP,FMT)ntsd
      FILENAME=FIELD//'_'//TIMESTEP(1:NLEN)

      IF(MYPE==0)THEN
        CALL INT_GET_FRESH_HANDLE(IUNIT)
        CLOSE(IUNIT)
        OPEN(UNIT=IUNIT,FILE=FILENAME,FORM='UNFORMATTED',IOSTAT=IER)
      ENDIF




      DO 500 K=KDE-1,KDS,-1   


      IF(MYPE==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          TWRITE(I,J)=ARRAY(I,J,K)
        ENDDO
        ENDDO

        DO IPE=1,NPES-1
          CALL MPI_RECV(IT_REM,2,MPI_INTEGER,IPE,IPE                    &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
          CALL MPI_RECV(JT_REM,2,MPI_INTEGER,IPE,IPE                    &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)

          ITS_REM=IT_REM(1)
          ITE_REM=IT_REM(2)
          JTS_REM=JT_REM(1)
          JTE_REM=JT_REM(2)

          NSIZE=(ITE_REM-ITS_REM+1)*(JTE_REM-JTS_REM+1)
          ALLOCATE(VALUES(1:NSIZE))

          CALL MPI_RECV(VALUES,NSIZE,MPI_REAL,IPE,IPE                   &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
          N=0
          DO J=JTS_REM,JTE_REM
            DO I=ITS_REM,ITE_REM
              N=N+1
              TWRITE(I,J)=VALUES(N)
            ENDDO
          ENDDO

          DEALLOCATE(VALUES)

        ENDDO


      ELSE
        NSIZE=(ITE-ITS+1)*(JTE-JTS+1)
        ALLOCATE(VALUES(1:NSIZE))

        N=0
        DO J=JTS,JTE
        DO I=ITS,ITE
          N=N+1
          VALUES(N)=ARRAY(I,J,K)
        ENDDO
        ENDDO

        IT_REM(1)=ITS
        IT_REM(2)=ITE
        JT_REM(1)=JTS
        JT_REM(2)=JTE

        CALL MPI_SEND(IT_REM,2,MPI_INTEGER,0,MYPE                       &
     &               ,MPI_COMM_COMP,ISEND)
        CALL MPI_SEND(JT_REM,2,MPI_INTEGER,0,MYPE                       &
     &               ,MPI_COMM_COMP,ISEND)

        CALL MPI_SEND(VALUES,NSIZE,MPI_REAL,0,MYPE                      &
     &               ,MPI_COMM_COMP,ISEND)

        DEALLOCATE(VALUES)

      ENDIF


      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)

      IF(MYPE==0)THEN

        DO J=JDS,JDE-1
          IENDX=IDE-1
          IF(MOD(J,2)==0)IENDX=IENDX-1
          WRITE(IUNIT)(TWRITE(I,J),I=1,IENDX)
        ENDDO

      ENDIF



  500 CONTINUE

      IF(MYPE==0)CLOSE(IUNIT)


      END SUBROUTINE TWR



      SUBROUTINE VWR(ARRAY,KK,FIELD,ntsd,MYPE,NPES,MPI_COMM_COMP       &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &              ,IMS,IME,JMS,JME,KMS,KME                           &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)


      USE MODULE_EXT_INTERNAL

      IMPLICIT NONE
      INCLUDE "mpif.h"


      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                    &
     &                     ,KK,MPI_COMM_COMP,MYPE,NPES,ntsd

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME+KK),INTENT(IN) :: ARRAY

      CHARACTER(*),INTENT(IN) :: FIELD



      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
      INTEGER,DIMENSION(2) :: IM_REM,JM_REM,IT_REM,JT_REM

      INTEGER :: I,IENDX,IER,IPE,IRECV,IRTN,ISEND,IUNIT                &
     &          ,J,K,L,N,NLEN,NSIZE
      INTEGER :: ITS_REM,ITE_REM,JTS_REM,JTE_REM

      REAL,DIMENSION(IDS:IDE,JDS:JDE) :: TWRITE
      REAL,ALLOCATABLE,DIMENSION(:) :: VALUES
      CHARACTER(5) :: TIMESTEP
      CHARACTER(6) :: FMT
      CHARACTER(12) :: FILENAME
      LOGICAL :: OPENED




      IF(ntsd<=9)THEN
        FMT='(I1.1)'
        NLEN=1
      ELSEIF(ntsd<=99)THEN
        FMT='(I2.2)'
        NLEN=2
      ELSEIF(ntsd<=999)THEN
        FMT='(I3.3)'
        NLEN=3
      ELSEIF(ntsd<=9999)THEN
        FMT='(I4.4)'
        NLEN=4
      ELSEIF(ntsd<=99999)THEN
        FMT='(I5.5)'
        NLEN=5
      ENDIF
      WRITE(TIMESTEP,FMT)ntsd
      FILENAME=FIELD//'_'//TIMESTEP(1:NLEN)

      IF(MYPE==0)THEN
        CALL INT_GET_FRESH_HANDLE(IUNIT)
        CLOSE(IUNIT)
        OPEN(UNIT=IUNIT,FILE=FILENAME,FORM='UNFORMATTED',IOSTAT=IER)
      ENDIF



















      DO 500 K=KDE-1,KDS,-1   


      IF(MYPE==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          TWRITE(I,J)=ARRAY(I,J,K)
        ENDDO
        ENDDO

        DO IPE=1,NPES-1
          CALL MPI_RECV(IT_REM,2,MPI_INTEGER,IPE,IPE                    &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
          CALL MPI_RECV(JT_REM,2,MPI_INTEGER,IPE,IPE                    &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)

          ITS_REM=IT_REM(1)
          ITE_REM=IT_REM(2)
          JTS_REM=JT_REM(1)
          JTE_REM=JT_REM(2)

          NSIZE=(ITE_REM-ITS_REM+1)*(JTE_REM-JTS_REM+1)
          ALLOCATE(VALUES(1:NSIZE))

          CALL MPI_RECV(VALUES,NSIZE,MPI_REAL,IPE,IPE                   &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
          N=0
          DO J=JTS_REM,JTE_REM
            DO I=ITS_REM,ITE_REM
              N=N+1
              TWRITE(I,J)=VALUES(N)
            ENDDO
          ENDDO

          DEALLOCATE(VALUES)

        ENDDO


      ELSE
        NSIZE=(ITE-ITS+1)*(JTE-JTS+1)
        ALLOCATE(VALUES(1:NSIZE))

        N=0
        DO J=JTS,JTE
        DO I=ITS,ITE
          N=N+1
          VALUES(N)=ARRAY(I,J,K)
        ENDDO
        ENDDO

        IT_REM(1)=ITS
        IT_REM(2)=ITE
        JT_REM(1)=JTS
        JT_REM(2)=JTE

        CALL MPI_SEND(IT_REM,2,MPI_INTEGER,0,MYPE                       &
     &               ,MPI_COMM_COMP,ISEND)
        CALL MPI_SEND(JT_REM,2,MPI_INTEGER,0,MYPE                       &
     &               ,MPI_COMM_COMP,ISEND)

        CALL MPI_SEND(VALUES,NSIZE,MPI_REAL,0,MYPE                      &
     &               ,MPI_COMM_COMP,ISEND)

        DEALLOCATE(VALUES)

      ENDIF


      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)

      IF(MYPE==0)THEN

        DO J=JDS,JDE-1
          IENDX=IDE-1
          IF(MOD(J,2)==1)IENDX=IENDX-1
          WRITE(IUNIT)(TWRITE(I,J),I=1,IENDX)
        ENDDO

      ENDIF


  500 CONTINUE

      IF(MYPE==0)CLOSE(IUNIT)


      END SUBROUTINE VWR



      SUBROUTINE TIME_STATS(TIME_LCL_IN,NAME,ntsd,MYPE,NPES,MPI_COMM_COMP)


      USE MODULE_EXT_INTERNAL


      IMPLICIT NONE

      INCLUDE "mpif.h"

      INTEGER,INTENT(IN) :: MPI_COMM_COMP,MYPE,NPES,ntsd
      REAL,INTENT(IN) :: TIME_LCL_IN

      CHARACTER(*),INTENT(IN) :: NAME



      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
      INTEGER,ALLOCATABLE,DIMENSION(:) :: ID_PE,IPE_SORT

      INTEGER :: IPE,IPE_MAX,IPE_MEDIAN,IPE_MIN,IRECV,IRTN,ISEND       &
     &          ,N,N_MEDIAN,NLEN

      REAL,ALLOCATABLE,DIMENSION(:) :: TIME,SORT_TIME
      REAL,DIMENSION(2) :: REMOTE
      REAL :: TIME_MAX,TIME_MEAN,TIME_MEDIAN,TIME_MIN,TIME_LCL

      CHARACTER(5) :: TIMESTEP
      CHARACTER(6) :: FMT
      CHARACTER(25) :: TITLE
      CHARACTER(LEN=256) :: message




      TIME_LCL=TIME_LCL_IN*1000.
      IF(ntsd<=9)THEN
        FMT='(I1.1)'
        NLEN=1
      ELSEIF(ntsd<=99)THEN
        FMT='(I2.2)'
        NLEN=2
      ELSEIF(ntsd<=999)THEN
        FMT='(I3.3)'
        NLEN=3
      ELSEIF(ntsd<=9999)THEN
        FMT='(I4.4)'
        NLEN=4
      ELSEIF(ntsd<=99999)THEN
        FMT='(I5.5)'
        NLEN=5
      ENDIF
      WRITE(TIMESTEP,FMT)ntsd
      TITLE=NAME//'_'//TIMESTEP(1:NLEN)



      IF(MYPE==0)THEN
        ALLOCATE(TIME(1:NPES))
        ALLOCATE(SORT_TIME(1:NPES))
        ALLOCATE(ID_PE(1:NPES))
        ALLOCATE(IPE_SORT(1:NPES))

        TIME(1)=TIME_LCL
        ID_PE(1)=MYPE



        DO IPE=1,NPES-1
          CALL MPI_RECV(REMOTE,2,MPI_REAL,IPE,IPE                      &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)

          TIME(IPE+1)=REMOTE(1)
          ID_PE(IPE+1)=NINT(REMOTE(2))
        ENDDO




        TIME_MEAN=0.
        TIME_MAX=-1.
        TIME_MIN=1.E10
        IPE_MAX=-1
        IPE_MIN=-1

        DO N=1,NPES
          TIME_MEAN=TIME_MEAN+TIME(N)

          IF(TIME(N)>TIME_MAX)THEN
            TIME_MAX=TIME(N)
            IPE_MAX=ID_PE(N)
          ENDIF

          IF(TIME(N)<TIME_MIN)THEN
            TIME_MIN=TIME(N)
            IPE_MIN=ID_PE(N)
          ENDIF

        ENDDO

        TIME_MAX=TIME_MAX*1.E-3
        TIME_MIN=TIME_MIN*1.E-3
        TIME_MEAN=TIME_MEAN*1.E-3/REAL(NPES)



        CALL SORT(TIME,NPES,SORT_TIME,IPE_SORT)
        N_MEDIAN=(NPES+1)/2
        TIME_MEDIAN=SORT_TIME(N_MEDIAN)*1.E-3
        IPE_MEDIAN=IPE_SORT(N_MEDIAN)


      ELSE



        REMOTE(1)=TIME_LCL
        REMOTE(2)=REAL(MYPE)

        CALL MPI_SEND(REMOTE,2,MPI_REAL,0,MYPE,MPI_COMM_COMP,ISEND)

      ENDIF


      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)



      IF(MYPE==0)THEN
        WRITE(message,100)TITLE
        CALL wrf_message(trim(message))
        WRITE(message,105)TIME_MAX,IPE_MAX
        CALL wrf_message(trim(message))
        WRITE(message,110)TIME_MIN,IPE_MIN
        CALL wrf_message(trim(message))
        WRITE(message,115)TIME_MEDIAN,IPE_MEDIAN
        CALL wrf_message(trim(message))
        WRITE(message,120)TIME_MEAN
        CALL wrf_message(trim(message))
  100   FORMAT(' Time for ',A)
  105   FORMAT(' Maximum=',G11.5,' for PE ',I2.2)
  110   FORMAT(' Minimum=',G11.5,' for PE ',I2.2)
  115   FORMAT(' Median =',G11.5,' for PE ',I2.2)
  120   FORMAT(' Mean   =',G11.5)
      ENDIF


      END SUBROUTINE TIME_STATS




      SUBROUTINE SORT(DATA,NPES,DATA_SORTED,IPE_SORTED)






      IMPLICIT NONE

      INTEGER,INTENT(IN) :: NPES
      REAL,DIMENSION(NPES),INTENT(IN) :: DATA

      INTEGER,DIMENSION(NPES),INTENT(OUT) :: IPE_SORTED
      REAL,DIMENSION(NPES),INTENT(OUT) :: DATA_SORTED

      TYPE :: DATA_LINK
        REAL :: VALUE
        INTEGER :: IPE
        TYPE(DATA_LINK),POINTER :: NEXT_VALUE
      END TYPE





      INTEGER :: ISTAT,N

      TYPE(DATA_LINK),POINTER :: HEAD,TAIL  
      TYPE(DATA_LINK),POINTER :: PTR_NEW    
      TYPE(DATA_LINK),POINTER :: PTR1,PTR2  



      pe_loop: DO N=1,NPES
        ALLOCATE(PTR_NEW,STAT=ISTAT)  
        PTR_NEW%VALUE=DATA(N)
        PTR_NEW%IPE=N-1







        main: IF(N==1)THEN
          HEAD=>PTR_NEW
          TAIL=>HEAD
          NULLIFY(PTR_NEW%NEXT_VALUE)





        ELSE
          check: IF(PTR_NEW%VALUE<HEAD%VALUE)THEN
            PTR_NEW%NEXT_VALUE=>HEAD
            HEAD=>PTR_NEW





          ELSEIF(PTR_NEW%VALUE>=TAIL%VALUE)THEN
            TAIL%NEXT_VALUE=>PTR_NEW  
                                      
                                      
            TAIL=>PTR_NEW
            NULLIFY(TAIL%NEXT_VALUE)





          ELSE
            PTR1=>HEAD
            PTR2=>PTR1%NEXT_VALUE

            search: DO
              IF((PTR_NEW%VALUE>=PTR1%VALUE).AND.                      &
     &           (PTR_NEW%VALUE<PTR2%VALUE))THEN
                PTR_NEW%NEXT_VALUE=>PTR2
                PTR1%NEXT_VALUE=>PTR_NEW
                EXIT search
              ENDIF

              PTR1=>PTR2
              PTR2=>PTR2%NEXT_VALUE
            ENDDO search

          ENDIF check

        ENDIF main

      ENDDO pe_loop





      PTR1=>HEAD

      DO N=1,NPES

        DATA_SORTED(N)=PTR1%VALUE
        IPE_SORTED(N)=PTR1%IPE
        PTR1=>PTR1%NEXT_VALUE
      ENDDO

      DEALLOCATE(PTR_NEW)
      NULLIFY (HEAD,TAIL,PTR1,PTR2)

      END SUBROUTINE SORT



      SUBROUTINE FIELD_STATS(FIELD,MYPE,MPI_COMM_COMP                  &
     &                      ,IDS,IDE,JDS,JDE,KDS,KDE                   &
     &                      ,IMS,IME,JMS,JME,KMS,KME                   &
     &                      ,ITS,ITE,JTS,JTE,KTS,KTE)





      IMPLICIT NONE

      INCLUDE "mpif.h"


      INTEGER,INTENT(IN) :: MPI_COMM_COMP,MYPE
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE

      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: FIELD





      INTEGER,PARAMETER :: DOUBLE=SELECTED_REAL_KIND(15,300)

      INTEGER :: I,IEND,IRTN,I_BY_J,J,K,KFLIP

      REAL :: FIKJ,FMAXK,FMINK
      REAL(KIND=DOUBLE) :: F_MEAN,POINTS,RMS,ST_DEV,SUMFK,SUMF2K
      REAL,DIMENSION(KTS:KTE) :: FMAX,FMAX_0,FMIN,FMIN_0
      REAL(KIND=DOUBLE),DIMENSION(KTS:KTE) :: SUMF,SUMF_0,SUMF2,SUMF2_0
 
      CHARACTER(LEN=256) :: message


      I_BY_J=(IDE-IDS)*(JDE-JDS)-(JDE-JDS-1)/2  
                                                
                                                
                                                

      layer_loop:  DO K=KTS,KTE

        FMAXK=-1.E10
        FMINK=1.E10
        SUMFK=0.
        SUMF2K=0.

        DO J=JTS,JTE
          IEND=MIN(ITE,IDE-1)
          IF(MOD(J,2)==0.AND.ITE==IDE-1)IEND=IEND-1
          DO I=ITS,IEND
            FIKJ=FIELD(I,J,K)
            FMAXK=MAX(FMAXK,FIKJ)
            FMINK=MIN(FMINK,FIKJ)
            SUMFK=SUMFK+FIKJ
            SUMF2K=SUMF2K+FIKJ*FIKJ
          ENDDO
        ENDDO

        FMAX(K)=FMAXK
        FMIN(K)=FMINK
        SUMF(K)=SUMFK
        SUMF2(K)=SUMF2K

      ENDDO layer_loop





      CALL MPI_REDUCE(SUMF,SUMF_0,KTE,MPI_REAL8,MPI_SUM,0              &
     &               ,MPI_COMM_COMP,IRTN)
      CALL MPI_REDUCE(SUMF2,SUMF2_0,KTE,MPI_REAL8,MPI_SUM,0            &
     &               ,MPI_COMM_COMP,IRTN)
      CALL MPI_REDUCE(FMAX,FMAX_0,KTE,MPI_REAL,MPI_MAX,0               &
     &               ,MPI_COMM_COMP,IRTN)
      CALL MPI_REDUCE(FMIN,FMIN_0,KTE,MPI_REAL,MPI_MIN,0               &
     &               ,MPI_COMM_COMP,IRTN)

      IF(MYPE==0)THEN
        POINTS=I_BY_J
        DO K=KTE,KTS,-1
          F_MEAN=SUMF_0(K)/POINTS
          ST_DEV=SQRT((POINTS*SUMF2_0(K)-SUMF_0(K)*SUMF_0(K))/         &
     &                (POINTS*(POINTS-1)))
          RMS=SQRT(SUMF2_0(K)/POINTS)
          KFLIP=KTE-K+1
          WRITE(message,101)KFLIP,FMAX_0(K),FMIN_0(K)
          CALL wrf_message(trim(message))
          WRITE(message,102)F_MEAN,ST_DEV,RMS
          CALL wrf_message(trim(message))
  101     FORMAT(' LAYER=',I2,' MAX=',E13.6,' MIN=',E13.6)
  102     FORMAT(9X,' MEAN=',E13.6,' STDEV=',E13.6,' RMS=',E13.6)
        ENDDO
      ENDIF

      END SUBROUTINE FIELD_STATS

      SUBROUTINE check_grid(grid,config_flags,where, &
                            IDS,IDE,JDS,JDE,KDS,KDE, &
                            IMS,IME,JMS,JME,KMS,KME, &
                            ITS,ITE,JTS,JTE,KTS,KTE)
        use module_domain, only : domain
        use module_configure, only : grid_config_rec_type
        use, intrinsic :: ieee_arithmetic
        implicit none
        LOGICAL ISNAN, EXTERNAL           
        character*(*), intent(in) :: where
        type(grid_config_rec_type),intent(in) :: config_flags
        type(domain), intent(in) :: grid
        INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                  &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
        
        character(len=255) :: message
        integer :: i,j,k

        if(config_flags%halo_debug/=2 .and. config_flags%halo_debug/=3) then
           return
        endif
        call wrf_debug(2,'Check for NaN')

        do k=kts,kte-1
           do j=jts,jte
              do i=its,ite
                 if(ieee_is_nan(grid%w(i,j,k))) then
                    write(message,303) where,'W',i,j,k
                    call wrf_error_fatal3("<stdin>",3515,&
message)
                 endif
                 if(ieee_is_nan(grid%u(i,j,k))) then
                    write(message,303) where,'U',i,j,k
                    call wrf_error_fatal3("<stdin>",3520,&
message)
                 endif
                 if(ieee_is_nan(grid%v(i,j,k))) then
                    write(message,303) where,'V',i,j,k
                    call wrf_error_fatal3("<stdin>",3525,&
message)
                 endif
                 if(ieee_is_nan(grid%t(i,j,k))) then
                    write(message,303) where,'T',i,j,k
                    call wrf_error_fatal3("<stdin>",3530,&
message)
                 endif
                 if(ieee_is_nan(grid%q(i,j,k))) then
                    write(message,303) where,'Q',i,j,k
                    call wrf_error_fatal3("<stdin>",3535,&
message)
                 endif
                 if(ieee_is_nan(grid%cwm(i,j,k))) then
                    write(message,303) where,'CWM',i,j,k
                    call wrf_error_fatal3("<stdin>",3540,&
message)
                 endif
303              format('check_grid(...,"',A,'",...): NaN at ',A,'(',I0,',',I0,',',I0,')')
              enddo
           enddo
        enddo
      END SUBROUTINE check_grid

