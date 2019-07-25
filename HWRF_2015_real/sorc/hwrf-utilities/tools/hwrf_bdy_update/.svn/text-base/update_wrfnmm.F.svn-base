       SUBROUTINE update_wrfnmm (wrfbdyfile,gsifilename)

     USE module_domain
     USE module_io_domain
     USE module_configure
     USE module_dm
     USE module_date_time
     USE module_utility

!!   USE module_bc
   IMPLICIT NONE



   INTEGER, PARAMETER :: CONFIG_BUF_LEN=32768  ! careful, may be IBM specific (a configure.wrf file item)
   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4*CONFIG_BUF_LEN
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor

  integer :: i,j,k
  integer :: ids,ide, jds,jde, kds,kde
  integer :: ims,ime, jms,jme, kms,kme
  integer :: its,ite, jts,jte, kts,kte
  integer :: ips,ipe, jps,jpe, kps,kpe


        character (len=19) :: temp19
        character (len=24) :: temp24 , temp24b

	character(len=20) :: wrfbdyfile, wrfbdyfile_update
        character(len=20) :: gsifilename
        CHARACTER(LEN=19) :: start_date_char , end_date_char , &
                        current_date_char , next_date_char
	character(len=255) :: message

        INTEGER :: time_loop_max , loop

        integer, save :: id,id2, ierr
        INTEGER :: switch, fid, rc, idum1,idum2, debug_level
        LOGICAL, parameter:: replace=.true.


  REAL :: TBT,UBT,VBT,QBT,CWMBT
  REAL :: PDBT

  REAL,ALLOCATABLE,DIMENSION(:,:)::TBA,UBA,VBA,QBA,CWMBA
  REAL,ALLOCATABLE,DIMENSION(:)::PDBA

  INTEGER :: KB, KB_V, LM, IM, JM, iunit_gfs, N, NSTART, NSTART_V, ioerror
  INTEGER :: jstart, jend, inc_h, inc_v
    LOGICAL :: alloc_ph_arrays


        TYPE(domain) , POINTER :: grid
        TYPE(domain) , POINTER :: null_domain

        CHARACTER*40            :: this_datestr, next_datestr
    Type(WRFU_Time) time, currtime, currentTime


   TYPE (grid_config_rec_type)  :: config_flags


#include "version_decl"

   INTERFACE
     SUBROUTINE Setup_Timekeeping( grid )
      USE module_domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Setup_Timekeeping
   END INTERFACE

   !  Define the name of this program (program_name defined in module_domain)

   program_name = "REAL_NMM " // TRIM(release_version) // " PREPROCESSOR"

	write(0,*) 'getting into WRF stuff'

!#ifdef DM_PARALLEL
   CALL disable_quilting
!#endif

        CALL init_modules(1)   ! Phase 1 returns after MPI_INIT() (if it is called)
!3.3        CALL WRFU_Initialize( defaultCalendar=WRFU_CAL_GREGORIAN, rc=rc )
        CALL WRFU_Initialize( defaultCalKind=WRFU_CAL_GREGORIAN, rc=rc )
        CALL init_modules(2)   ! Phase 2 resumes after MPI_INIT() (if it is called)

! #ifdef DM_PARALLEL
 if (wrf_dm_on_monitor() ) then
   CALL initial_config
 endif
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_dm_initialize

! #else
!    CALL initial_config
! #endif


   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

   NULLIFY( null_domain )
   CALL alloc_and_configure_domain ( domain_id  = 1           , &
                                     grid       = head_grid   , &
                                     parent     = null_domain , &
                                     kid        = -1            )

   grid => head_grid


   CALL Setup_Timekeeping ( grid )
   CALL domain_clock_set( grid, &
                          time_step_seconds=model_config_rec%interval_seconds )

   CALL set_scalar_indices_from_config ( grid%id , idum1, idum2 )

   CALL     wrf_debug ( 100 , 'real_nmm: calling model_to_grid_config_rec ' )

  !  Initialize the WRF IO: open files, init file handles, etc.

       CALL       wrf_debug ( 100 , 'real_nmm: calling init_wrfio' )

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   CALL compute_start_and_end ( model_config_rec%start_year  (grid%id) , &
                                   model_config_rec%start_month (grid%id) , &
                                   model_config_rec%start_day   (grid%id) , &
                                   model_config_rec%start_hour  (grid%id) , &
                                   model_config_rec%start_minute(grid%id) , &
                                   model_config_rec%start_second(grid%id) , &
                                   model_config_rec%  end_year  (grid%id) , &
                                   model_config_rec%  end_month (grid%id) , &
                                   model_config_rec%  end_day   (grid%id) , &
                                   model_config_rec%  end_hour  (grid%id) , &
                                   model_config_rec%  end_minute(grid%id) , &
                                   model_config_rec%  end_second(grid%id) , &
                                   model_config_rec%interval_seconds      , &
                                   start_date_char , end_date_char , time_loop_max )

   !  Here we define the initial time to process, for later use by the code.

   current_date_char = start_date_char
   start_date = start_date_char
   current_date = start_date

	write(0,*) 'model_config_rec%interval_seconds: ', model_config_rec%interval_seconds
   CALL nl_set_bdyfrq ( grid%id , REAL(model_config_rec%interval_seconds) )
	config_flags%bdyfrq=model_config_rec%interval_seconds

   !  Loop over each time period to process.

   write(0,*) 'time_loop_max: ', time_loop_max

!no	call wrf_error_fatal("was core generated yet")
!    DO loop = 1 , time_loop_max


	write(0,*) 'config_flags%num_land_cat: ', config_flags%num_land_cat

        CALL init_wrfio
	write(0,*) 'now config_flags%num_land_cat: ', config_flags%num_land_cat

	if (config_flags%num_land_cat .ne. 24)  then
	write(0,*) 'in trouble'
	call wrf_error_fatal("didnt get num_land cat")
        else	
	write(0,*) 'looking good'
	endif


   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )


	write(0,*) 'attempt to open for read : ', trim(wrfbdyfile)

      CALL open_r_dataset ( id, TRIM(wrfbdyfile) , grid , config_flags , &
                              "DATASET=BOUNDARY", ierr )

              ids = grid%sd31 ; ide = grid%ed31 ;
              jds = grid%sd32 ; jde = grid%ed32 ;
              kds = grid%sd33 ; kde = grid%ed33 ;

              ims = grid%sm31 ; ime = grid%em31 ;
              jms = grid%sm32 ; jme = grid%em32 ;
              kms = grid%sm33 ; kme = grid%em33 ;

              its = grid%sp31 ; ite = grid%ep31 ; ! tile is entire patch
              jts = grid%sp32 ; jte = grid%ep32 ; ! tile is entire patch
              kts = grid%sp33 ; kte = grid%ep33 ; ! tile is entire patch

   ips = grid%sp31
   ipe = grid%ep31-1 ! 030730tst
   jps = grid%sp32
   jpe = grid%ep32-1 ! 030730tst
   kps = grid%sp33
   kpe = grid%ep33-1 ! 030730tst

      DO loop = 1 , time_loop_max-1

         CALL input_boundary ( id, grid , config_flags , ierr )

	if (ips .eq. 1 .and. jps .eq. 1) then
	write(0,*) 'grid%pd_bxs(1,1,1): ', grid%pd_bxs(1,1,1)
!	write(0,*) 'grid%pd_btxs(1,1,1): ', grid%pd_btxs(1,1,1)
	write(0,*) 'target pd_bxs value: ', grid%pd_bxs(1,1,1)+config_flags%bdyfrq*grid%pd_btxs(1,1,1)
	endif

      first_replace:	if (loop .eq. 1 .and. replace) then

        KB = 2*(IDE-1) + (JDE-1) - 3
        KB_V=KB
        LM = KDE-1

        ALLOCATE(TBA(KB,LM))
        ALLOCATE(QBA(KB,LM))
        ALLOCATE(CWMBA(KB,LM))
        ALLOCATE(UBA(KB,LM))
        ALLOCATE(VBA(KB,LM))
        ALLOCATE(PDBA(KB))

	write(0,*) 'trim(gsifilename) is: ', trim(gsifilename)

         call readgsifile( trim(gsifilename),KB,KB_V,LM,PDBA,TBA,QBA,CWMBA,UBA,VBA)

	write(0,*) 'return KB, LM: ', KB, LM


! **** Overwrite the bdy values, and recompute the tendencies based on the new initial value.

   SOUTH:     IF(JPS.EQ.JDS)THEN  ! ****************************************
           J=1

           DO k = kps , MIN(kde,kpe)
              N=1
              DO i = ips , MIN(ide,ipe)
                 TBT=grid%t_bys(i,k,1)+config_flags%bdyfrq*grid%t_btys(i,k,1)
                 grid%t_bys(i,k,1) =   TBA(N,k)
                 grid%t_btys(i,k,1) = (TBT-TBA(N,K))/config_flags%bdyfrq
                 
                 QBT=grid%q_bys(i,k,1)+config_flags%bdyfrq*grid%q_btys(i,k,1)
                 grid%q_bys(i,k,1) =   QBA(N,k)
                 grid%q_btys(i,k,1) = (QBT-QBA(N,K))/config_flags%bdyfrq

                 CWMBT=grid%cwm_bys(i,k,1)+config_flags%bdyfrq*grid%cwm_btys(i,k,1)
                 grid%cwm_bys(i,k,1) =   CWMBA(N,k)
                 grid%cwm_btys(i,k,1) = (CWMBT-CWMBA(N,K))/config_flags%bdyfrq

                 grid%q2_bys(i,k,1) =  0.0        !KWON
                 grid%q2_btys(i,k,1) =  0.0        !KWON

                 N=N+1
              END DO
           END DO
           DO k = kps , MIN(kde,kpe)
              N=1
              DO i = ips , MIN(ide-2,ipe)
                 UBT=grid%u_bys(i,k,1)+config_flags%bdyfrq*grid%u_btys(i,k,1)
                 grid%u_bys(i,k,1) = UBA(N,k)
                 grid%u_btys(i,k,1)= (UBT-UBA(N,k))/config_flags%bdyfrq

                 VBT=grid%v_bys(i,k,1)+config_flags%bdyfrq*grid%v_btys(i,k,1)
                 grid%v_bys(i,k,1) = VBA(N,k)
                 grid%v_btys(i,k,1)= (VBT-VBA(N,k))/config_flags%bdyfrq

                 N=N+1
              ENDDO
           END DO
           NSTART_V=N

           N=1
           DO i = ips , MIN(ide,ipe)
              PDBT=grid%pd_bys(i,1,1)+config_flags%bdyfrq*grid%pd_btys(i,1,1)
              grid%pd_btys(i,1,1) = (PDBT-PDBA(N))/config_flags%bdyfrq
              grid%pd_bys(i,1,1) = PDBA(N)

              N=N+1
           END DO


!! all tasks need to know NSTART and NSTART_V
!!
!! or is this better done on a single processor?  Incrementing on N is problematic.  
!! go the HWRF route instead?  But even doesn't seem general enough for a M X N decomp

        ENDIF SOUTH

   NORTH:     IF(JPE.GE.JDE-1)THEN ! **************************************************


	write(0,*) 'ips, min(IDE,IPE): ', ips, min(IDE,IPE)
           NSTART=N
	write(0,*) 'NSTART: ', NSTART
           DO k = kps , MIN(kde,kpe)
              N=NSTART
              DO i = ips , MIN(ide,ipe)
                 TBT=grid%t_bye(i,k,1)+config_flags%bdyfrq*grid%t_btye(i,k,1)
                 grid%t_bye(i,k,1) =   TBA(N,k)
                 grid%t_btye(i,k,1) = (TBT-TBA(N,K))/config_flags%bdyfrq

                 QBT=grid%q_bye(i,k,1)+config_flags%bdyfrq*grid%q_btye(i,k,1)
                 grid%q_bye(i,k,1) =   QBA(N,k)
                 grid%q_btye(i,k,1) = (QBT-QBA(N,K))/config_flags%bdyfrq

                 CWMBT=grid%cwm_bye(i,k,1)+config_flags%bdyfrq*grid%cwm_btye(i,k,1)
                 grid%cwm_bye(i,k,1) =   CWMBA(N,k)
                 grid%cwm_btye(i,k,1) = (CWMBT-CWMBA(N,K))/config_flags%bdyfrq

                 grid%q2_bye(i,k,1) =  0.0        !KWON
                 grid%q2_btye(i,k,1) =  0.0        !KWON

                 N=N+1
              END DO
           END DO
	write(0,*) 'start north V at: ', NSTART_V
           DO k = kps , MIN(kde,kpe)
              N=NSTART_V
              DO i = ips , MIN(ide-2,ipe)
                 UBT=grid%u_bye(i,k,1)+config_flags%bdyfrq*grid%u_btye(i,k,1)
                 grid%u_bye(i,k,1) = UBA(N,k)
                 grid%u_btye(i,k,1) = (UBT-UBA(N,K))/config_flags%bdyfrq

                 VBT=grid%v_bye(i,k,1)+config_flags%bdyfrq*grid%v_btye(i,k,1)
                 grid%v_bye(i,k,1) = VBA(N,k)
                 grid%v_btye(i,k,1) = (VBT-VBA(N,K))/config_flags%bdyfrq

                 N=N+1
              ENDDO
           END DO
           NSTART_V=N

           N=NSTART
           DO i = ips , MIN(ide,ipe)
              PDBT=grid%pd_bye(i,1,1)+config_flags%bdyfrq*grid%pd_btye(i,1,1)
              grid%pd_bye(i,1,1) = PDBA(N)
              grid%pd_btye(i,1,1) = (PDBT-PDBA(N))/config_flags%bdyfrq

              N=N+1
           END DO

        ENDIF NORTH


   WEST:     IF(IPS.EQ.IDS)THEN ! ********************************************************

	write(0,*) 'size(t_bxs): ', size(grid%t_bxs,dim=1),  size(grid%t_bxs,dim=2), size(grid%t_bxs,dim=3)
           NSTART=N
              inc_h=mod(jps+1,2)
              inc_v=mod(jps,2)
              jstart=jps+inc_h
              jend=min(jde,jpe)
	write(0,*) 'start west H at: ', NSTART
	write(0,*) 'west jstart, jend (H): ', jstart, jend

           DO k = kps , MIN(kde,kpe)
              N=NSTART
              DO j = jstart , jend, 2
              if (J .ge. 3 .and. J .le. JDE-2 .and. mod(J,2) .eq. 1) then

                 TBT=grid%t_bxs(j,k,1)+config_flags%bdyfrq*grid%t_btxs(j,k,1)
                 grid%t_bxs(j,k,1) =   TBA(N,k)
                 grid%t_btxs(j,k,1) = (TBT-TBA(N,k))/config_flags%bdyfrq

                 QBT=grid%q_bxs(j,k,1)+config_flags%bdyfrq*grid%q_btxs(j,k,1)
                 grid%q_bxs(j,k,1) =   QBA(N,k)
                 grid%q_btxs(j,k,1) = (QBT-QBA(N,k))/config_flags%bdyfrq

                 CWMBT=grid%cwm_bxs(j,k,1)+config_flags%bdyfrq*grid%cwm_btxs(j,k,1)
                 grid%cwm_bxs(j,k,1) =   CWMBA(N,k)
                 grid%cwm_btxs(j,k,1) = (CWMBT-CWMBA(N,k))/config_flags%bdyfrq

                 grid%q2_bxs(j,k,1) =  0.0        !KWON
                 grid%q2_btxs(j,k,1) =  0.0        !KWON

                 N=N+1
              endif
              END DO
           END DO
       
              jstart=jps+inc_v
	write(0,*) 'start west V at: ', NSTART_V

           DO k = kps , MIN(kde,kpe)
              N=NSTART_V
              DO j = jstart , jend, 2
                if (J .ge. 2 .and. J .le. JDE-1 .and. mod(J,2) .eq. 0) then
                 UBT=grid%u_bxs(j,k,1)+config_flags%bdyfrq*grid%u_btxs(j,k,1)
                 grid%u_bxs(j,k,1) = UBA(N,k)
                 grid%u_btxs(j,k,1) = (UBT-UBA(N,k))/config_flags%bdyfrq

                 VBT=grid%v_bxs(j,k,1)+config_flags%bdyfrq*grid%v_btxs(j,k,1)
                 grid%v_bxs(j,k,1) = VBA(N,k)
                 grid%v_btxs(j,k,1) = (VBT-VBA(N,k))/config_flags%bdyfrq

                 N=N+1
                endif
              ENDDO
           END DO

              NSTART_V=N

              jstart=jps+inc_h
           N=NSTART
           DO j = jstart , jend, 2
             if (J .ge. 3 .and. J .le. JDE-2 .and. mod(J,2) .eq. 1) then

              PDBT=grid%pd_bxs(j,1,1)+config_flags%bdyfrq*grid%pd_btxs(j,1,1)
              grid%pd_bxs(j,1,1) = PDBA(N)
              grid%pd_btxs(j,1,1) = (PDBT-PDBA(N))/config_flags%bdyfrq
              N=N+1
             endif
           END DO

        ENDIF WEST

   EAST:     IF(IPE.GE.IDE-1)THEN ! **************************************************

              inc_h=mod(jps+1,2)
              inc_v=mod(jps,2)
              jstart=jps+inc_h
              jend=min(jde,jpe)

           NSTART=N

	write(0,*) 'start east H at: ', NSTART
           DO k = kps , MIN(kde,kpe)
              N=NSTART
              DO j = jstart , jend, 2
                 if (J .ge. 3 .and. J .le. JDE-2 .and. mod(J,2) .eq. 1) then

                 TBT=grid%t_bxe(j,k,1)+config_flags%bdyfrq*grid%t_btxe(j,k,1)
                 grid%t_bxe(j,k,1) =   TBA(N,k)
                 grid%t_btxe(j,k,1) = (TBT-TBA(N,k))/config_flags%bdyfrq

                 QBT=grid%q_bxe(j,k,1)+config_flags%bdyfrq*grid%q_btxe(j,k,1)
                 grid%q_bxe(j,k,1) =   QBA(N,k)
                 grid%q_btxe(j,k,1) = (QBT-QBA(N,k))/config_flags%bdyfrq

                 CWMBT=grid%cwm_bxe(j,k,1)+config_flags%bdyfrq*grid%cwm_btxe(j,k,1)
                 grid%cwm_bxe(j,k,1) =   CWMBA(N,k)
                 grid%cwm_btxe(j,k,1) = (CWMBT-CWMBA(N,k))/config_flags%bdyfrq

                 grid%q2_bxe(j,k,1) =  0.0        !KWON
                 grid%q2_btxe(j,k,1) =  0.0        !KWON

                 N=N+1
                 endif
              END DO
           END DO

	write(0,*) 'start east V at: ', NSTART_V
              jstart=jps+inc_v
           DO k = kps , MIN(kde,kpe)
              N=NSTART_V
              DO j = jstart , jend, 2
                if (J .ge. 2 .and. J .le. JDE-1 .and. mod(J,2) .eq. 0) then
                 UBT=grid%u_bxe(j,k,1)+config_flags%bdyfrq*grid%u_btxe(j,k,1)
                 grid%u_bxe(j,k,1) = UBA(N,k)
                 grid%u_btxe(j,k,1) = (UBT-UBA(N,k))/config_flags%bdyfrq

                 VBT=grid%v_bxe(j,k,1)+config_flags%bdyfrq*grid%v_btxe(j,k,1)
                 grid%v_bxe(j,k,1) = VBA(N,k)
                 grid%v_btxe(j,k,1) = (VBT-VBA(N,k))/config_flags%bdyfrq
                 N=N+1
                endif
              ENDDO
           END DO

              jstart=jps+inc_h
           N=NSTART
           DO j = jstart , jend, 2
              if (J .ge. 3 .and. J .le. JDE-2 .and. mod(J,2) .eq. 1) then
              PDBT=grid%pd_bxe(j,1,1)+config_flags%bdyfrq*grid%pd_btxe(j,1,1)
              grid%pd_bxe(j,1,1) = PDBA(N)
              grid%pd_btxe(j,1,1) = (PDBT-PDBA(N))/config_flags%bdyfrq
              N=N+1
              endif
           END DO

        ENDIF EAST

	endif first_replace

	wrfbdyfile_update='wrfbdy_d01_update'
	
	if (loop .eq. 1) then
      CALL open_w_dataset ( id2, TRIM(wrfbdyfile_update) , grid , config_flags , &
                              output_boundary,"DATASET=BOUNDARY", ierr )
	endif

	write(0,*) 'current_date_char: ', current_date_char
	write(0,*) 'start_date_char: ', start_date_char
	
      CALL geth_newdate ( current_date_char , start_date_char , &
                          loop * model_config_rec%interval_seconds )

      current_date =  current_date_char // '.0000'
	write(0,*) 'current_date defined as: ', current_date(1:19)

        CALL domain_clock_set( grid, current_timestr=current_date(1:19) )

      temp24=current_date
      temp24b=start_date
      start_date = current_date
      CALL geth_newdate ( temp19 , temp24b(1:19) , &
                         (loop-1) * model_config_rec%interval_seconds )
      current_date = temp19 //  '.0000'
       CALL domain_clock_set( grid, current_date(1:19) )
      write(0,*) 'LBC valid between these times ',current_date, ' ',start_date

         CALL output_boundary ( id2, grid , config_flags , ierr )
      current_date = temp24
      start_date = temp24b

	enddo  ! loop

       END SUBROUTINE update_wrfnmm
