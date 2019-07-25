      PROGRAM hwrf_bin_read_write
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C MAIN PROGRAM: WRFPOST
C   PRGMMR: BALDWIN          ORG: NSSL/SPC    DATE: 2002-06-18
C     
C ABSTRACT:  
C     THIS PROGRAM DRIVES THE EXTERNAL WRF POST PROCESSOR.
C     
C PROGRAM HISTORY LOG:
C   92-12-24  RUSS TREADON - CODED ETAPOST AS STAND ALONE CODE
C   98-05-29  BLACK - CONVERSION OF POST CODE FROM 1-D TO 2-D
C   00-02-04  JIM TUCCILLO - PARALLEL VERSION VIA MPI
C   01-02-15  JIM TUCCILLO - MANY COMMON BLOCKS REPLACED WITH MODULES
C             TO SUPPORT FORTRAN "ALLOCATE"s FOR THE EXACT SIZE OF THE 
C             ARRAYS NEEDED BASED ON THE NUMBER OF MPI TASKS.
C             THIS WAS DONE TO REDUCE THE ADDRESS SPACE THAT THE LOADER SEES.
C             THESE CHANGES WERE NECESSARY FOR RUNNING LARGER DOMAINS SUCH AS
C             12 KMS
C   01-06-15  JIM TUCCILLO - ADDED ASYNCRONOUS I/O CAPABILITY. IF THERE ARE MORE
C             THAN ONE MPI TASK, THE IO WILL BE DONE AYNCHRONOUSLY BY THE LAST
C             MPI TASK.
C   02-06-17  MIKE BALDWIN - CONVERT ETAPOST TO WRFPOST.  INCLUDE WRF I/O API
C             FOR INPUT OF MODEL DATA.  MODIFY CODE TO DEAL WITH C-GRID
C             DATA.  STREAMLINE OUTPUT TO A CALL OF ONE SUBROUTINE INSTEAD OF THREE.
C             REPLACE COMMON BLOCKS WITH A LIMITED NUMBER OF MODULES.
C   04-01-01  H CHUANG - ADDED NMM IO MODULE AND BINARY OPTIONS
C   05-07-08  Binbin Zhou: Aadded RSM model
C   05-12-05  H CHUANG - ADDED CAPABILITY TO OUTPUT OFF-HOUR FORECAST WHICH HAS
c               NO IMPACTS ON ON-HOUR FORECAST
C   06-02-20  CHUANG, BLACK, AND ROGERS - FINALIZED COMPLETE LIST OF NAM
C             OPERATIONAL PRODUCTS FROM WRF
C   06-02-27  H CHUANG - MODIFIED TO POST MULTIPLE
C             FORECAST HOURS IN ONE EXECUTION
C   06-03-03  H CHUANG - ADDED PARRISH'S MPI BINARY IO TO READ BINARY
C             WRF FILE AS RANDOM ASSCESS SO THAT VARIABLES IN WRF OUTPUT
C             DON'T HAVE TO BE READ IN IN SPECIFIC ORDER 
C   06-12-05  Y KWON - MODIFIED FOR WRITING AND REPLACING 
C             BINARY HWRF DATA FOR 3DVAR 
C  
C USAGE:    WRFPOST
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON - CTLBLK
C                RQSTFLD
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : IBM RS/6000 SP
C$$$  
C
C
C============================================================================================================
C
C     This is an MPI code. All array indexing is with respect to the global indices. Loop indices 
C     look as follows for N MPI tasks.
C
C
C
C  Original                                            New
C  Index                                               Index
C
C   JM ----------------------------------------------- JEND
C JM-1 -                                             - JEND_M
C JM-2 -               MPI TASK N-1                  - JEND_M2
C      -                                             -
C      -                                             -
C      ----------------------------------------------- JSTA, JSTA_M, JSTA_M2
C      ----------------------------------------------- JEND, JEND_M, JEND_M2
C      -                                             -
C      -               MPI TASK N-2                  -
C      -                                             -
C      -                                             -
C      ----------------------------------------------- JSTA, JSTA_M, JSTA_M2
C
C                           .
C                           .
C                           .
C
C      ----------------------------------------------- JEND, JEND_M, JEND_M2
C      -                                             -
C      -               MPI TASK 1                    -
C      -                                             -
C      -                                             -
C      ----------------------------------------------- JSTA, JSTA_M, JSTA_M2
C      ----------------------------------------------- JEND, JEND_M, JEND_M2
C      -                                             - 
C      -               MPI TASK 0                    - 
C    3 -                                             - JSTA_M2
C    2 -                                             - JSTA_M
C    1 ----------------------------------------------- JSTA
C
C     1                                              IM               
C
C
C     Jim Tuccillo
C     Jan 2000
C
C     README - Jim Tuccillo Feb 2001
C 
C     Many common blocks have been replaced by modules to support Fortran
C     "allocate" commands. Many of the 3-D arrays are now allocated to be the
C     exact size required based on the number of MPI tasks. The dimensioning will be 
C        x ( im,jsta_2l:jend_2u,lm)
C     Most 2-D arrays continue to be dimensioned (im,jm). This is fine but please be aware 
C     that the EXCH routine for arrays dimensioned (im,jm) is different than arrays dimensioned
C     (im,jsta_2l:jend_2u). Also, be careful about passing any arrays dimensioned
C     (im,jst_2l:jend_2u,lm). See examples in the code as to the correct calling sequence and
C     EXCH routine to use.
C
C
C     ASYNCHRONOUS I/O HAS BEEN ADDED. THE LAST MPI TASK DOES THE I/O. IF THERE IS
C     ONLY ONE MPI TASK THN TASK ) DOES THE I/O.
C     THE CODE HAS GOTTEN A LITTLE KLUDGY. BASICLY, IM, IMX and IMOUT MUST BE EQUAL
C     AND REPRESENT THE VALUE USED IN THE MODEL. THE SAME HOLDS FOR JM, JMX and JMOUT.
C
C     Jim Tuccillo June 2001
C
C
C===========================================================================================
C
C     INCLUDE ARRAY DIMENSIONS.
!      INCLUDE "parmeta"
!      INCLUDE "parmout"
      INCLUDE "mpif.h"
C
C     INCLUDE COMMON BLOCKS.
      INCLUDE "CTLBLK.comm"
C
C     DECLARE VARIABLES.
C     
C     SET HEADER WRITER FLAGS TO TRUE.
c
      common/tim_info/ETAFLD2_tim,ETA2P_tim,SURFCE2_tim, CLDRAD_tim,
     *                MISCLN_tim,FIXED_tim,MDL2SIGMA_tim
C
      common/jjt/time_output, time_e2out
!      real(8) time_output, time_e2out, time_initpost, rtc, ist
      real rinc(5)
chc      integer jdate(8),idate(8)
      integer iii
C      integer INAV
      integer :: Status
      character startdate*19,SysDepInfo*80,IOWRFNAME*3
C 
      character(len=20) :: IOFORM
!      character(len=4) :: MODELNAME 

!! YC KWON  :: ADD NAMELIST IN CASE NEEDE

      integer time_step,max_dom,s_we(3),e_we(3),
     1    s_sn(3),e_sn(3),
     2    s_vert(3),e_vert(3),grid_id(3),tile_sz_x,tile_sz_y,
     3    numtiles,nproc_x,nproc_y,level,parent_id(3),
     4    parent_grid_ratio(3),
     5    parent_time_step_ratio(3),i_parent_start(3),
     6    j_parent_start(3), feedback, num_moves,
     7    p_top_requested, ptsgm, num_metgrid_soil_levels
      real dx(3),dy(3), eta_levels(500)
      logical use_prep_hybrid

      NAMELIST /domains/time_step,max_dom,s_we,e_we,s_sn,e_sn,
     1    s_vert,e_vert,dx,dy,grid_id,tile_sz_x,tile_sz_y,
     2    numtiles,nproc_x,nproc_y,level,parent_id,parent_grid_ratio,
     3    parent_time_step_ratio,i_parent_start,j_parent_start,
     4    feedback,num_moves,num_metgrid_levels,p_top_requested,
     5    time_step_fract_num,time_step_fract_den,ptsgm,eta_levels,
     6    use_prep_hybrid,num_metgrid_soil_levels

C
C     START HERE
C
      call start()
C
C     INITIALIZE MPI
      
      CALL SETUP_SERVERS(ME,
     *                   NUM_PROCS,
     *                   NUM_SERVERS,
     *                   MPI_COMM_COMP,
     *                   MPI_COMM_INTER)
C
C     ME IS THE RANK
C     NUM_PROCS IS THE NUMBER OF TASKS DOING POSTING
C     NUM_SERVERS IS ONE IF THERE ARE MORE THAN ONE TOTAL MPI TASKS, OTHERWISE ZERO
C     MPI_COMM_COMP IS THE INTRACOMMUNICATOR
C     MPI_COMM_INTER IS THE INTERCOMMUNICATOR FOR COMMUNCATION BETWEEN TASK 0 OF THE
C        TASKS DOING THE POSTING AND THE I/O SERVER
C
C
C     IF WE HAVE MORE THAN 1 MPI TASK THEN WE WILL FIRE UP THE IO SERVER
C     THE LAST TASK ( IN THE CONTEXT OF MPI_COMM_WORLD ) IS THE I/O SERVER
C
      print*,'ME,NUM_PROCS,NUM_SERVERS=',ME,NUM_PROCS,NUM_SERVERS
      if ( me .ge. num_procs ) then
C
         call server
C
      else
C
      time_output = 0.
      time_e2out = 0.
      time_initpost = 0.
C
      INITPOST_tim = 0.
      ETAFLD2_tim = 0.0
      ETA2P_tim = 0.0
      MDL2SIGMA_tim = 0.0
      SURFCE2_tim = 0.0
      CLDRAD_tim = 0.0
      MISCLN_tim =0.0
      FIXED_tim =  0.0
!      bbtim = timef()
C**************************************************************************
C
C     START PROGRAM WRFPOST.
C
         read(5,111,end=1000) directory
         read(5,111,end=1000) infile
 111  format(a160)

      fileName = trim(directory)//'/'//trim(infile)
      print *,'fileName ',fileName

      IF(trim(infile).EQ.'wrfinput_d01'.
     &    or.trim(infile).EQ.'wrfout_d01') THEN   
                                                       !!BECAUSE OF DIM INDISCREPENCY
       call ext_int_ioinit(SysDepInfo,Status)
       print*,'called ioinit', Status
       call ext_int_open_for_read( trim(fileName), 0, 0, " ", 
     &   DataHandle, Status)
       print*,'called open for read', Status
       if ( Status /= 0 ) then
         print*,'error opening ',fileName, ' Status = ', Status ; stop
       endif

       call ext_int_get_dom_ti_integer(DataHandle
     1 ,'WEST-EAST_GRID_DIMENSION',iim,1,ioutcount, status )
       if ( Status /= 0 ) then
         print*,'error getting grid dim '; stop
       endif
       im=iim-1
       call ext_int_get_dom_ti_integer(DataHandle
     1 ,'SOUTH-NORTH_GRID_DIMENSION',jjm,1,ioutcount, status )
       jm=jjm-1
       call ext_int_get_dom_ti_integer(DataHandle
     1 ,'BOTTOM-TOP_GRID_DIMENSION',llm,1,ioutcount, status )
       lm=llm-1
       LP1=LM+1
       LM1=LM-1
       IM_JM=IM*JM
       print*,'im jm lm from wrfout= ',im,jm,lm

       ELSEIF(trim(infile).EQ.'wrfinput_d02'.
     &    or.trim(infile).EQ.'wrfout_d02') THEN

       OPEN(22,FILE=trim(directory)//'/namelist.input',
     1      STATUS='OLD',FORM = 'FORMATTED')
       READ(22,NML=domains)

       im = e_we(2) - 1
       jm = e_sn(2) - 1
       lm = e_vert(2) - 1
       LP1=LM+1
       LM1=LM-1
       IM_JM=IM*JM
       print*,'im jm lm from namelist= ',im,jm,lm

!       ELSEIF(trim(infile).EQ.'wrfinput_d03'.
!     &    or.trim(infile).EQ.'wrfout_d03') THENS
!
       ELSE

       OPEN(22,FILE=trim(directory)//'/namelist.input',
     1      STATUS='OLD',FORM = 'FORMATTED')
       READ(22,NML=domains)

       im = e_we(3) - 1
       jm = e_sn(3) - 1
       lm = e_vert(3) - 1
       LP1=LM+1
       LM1=LM-1
       IM_JM=IM*JM
       print*,'im jm lm from namelist= ',im,jm,lm

       ENDIF                                         !! KWON


        call ext_int_get_dom_ti_integer(DataHandle
     1    ,'SF_SURFACE_PHYSICS',itmp,1,ioutcount, status )
! set NSOIL to 4 as default for NOAH but change if using other 
! SFC scheme
        NSOIL=4
        IF(itmp.eq.1)then !thermal diffusion scheme
         NSOIL=5
        ELSE IF(itmp.eq.3)then ! RUC LSM
         NSOIL=6
        END IF
       END IF	
       print*,'NSOIL from wrfout= ',NSOIL
       call ext_int_ioclose ( DataHandle, Status )

      CALL MPI_FIRST
      print*,'jsta,jend,jsta_m,jend_m,jsta_2l,jend_2u=',jsta,jend
     1,jsta_m,jend_m,jsta_2l,jend_2u
     
!
!--- Initialize a few constants for new cloud fields (Ferrier, Feb '02)
!
      CALL MICROINIT
!
C EXP. initialize netcdf here instead
cexp      call ext_ncd_ioinit(Status)
cexp      call ext_ncd_open_for_read( trim(fileName), 0, 0, " ",
cexp     &  DataHandle, Status)
cexp       if ( Status /= 0 ) then
cexp         print*,'error opening ',fileName, ' Status = ', Status ; stop
cexp       endif
C Exp
!      ist = rtc()
        print*,'CALLING INITPOST_NMM_BIN_MPIIO TO 
     +  PROCESS NMM BINARY OUTPUT'
        CALL INITPOST_NMM_BIN_MPIIO
C
      call summary()
      CALL MPI_FINALIZE(IERR)
1000  CONTINUE
      STOP0
      END

      module module_wrf_error_dummy
        INTEGER           :: wrf_debug_level = 0
        CHARACTER*256     :: wrf_err_message
      
        LOGICAL :: silence=.false.  ! T = this process should not log.
        LOGICAL :: buffered=.false. ! T = messages sent via clog_write
        LOGICAL :: stderrlog=.false.! T = send to write(0,...) if buffered=F
      
        INTEGER, PARAMETER :: wrf_log_flush=0
        INTEGER, PARAMETER :: wrf_log_set_buffer_size=1
        INTEGER, PARAMETER :: wrf_log_write=2
      
        !NOTE: Make sure silence, buffered and stderrlog settings match the
        ! namelist defaults in init_module_wrf_error.
      
      ! min_allowed_buffer_size: requested buffer sizes smaller than this
      ! will simply result in disabling of log file buffering.  This number
      ! should be larger than any line WRF prints frequently.  If you set it 
      ! too small, the buffering code will still work.  However, any line 
      ! that is larger than the buffer will result in two writes: one for 
      ! the line and one for the end-of-line character at the end.
        integer, parameter :: min_allowed_buffer_size=200
      end module module_wrf_error_dummy
      
      SUBROUTINE wrf_abort
        STOP
      END SUBROUTINE wrf_abort
      SUBROUTINE get_current_time_string( time_str )
        CHARACTER(LEN=*), INTENT(OUT) :: time_str
        time_str = ''
      END SUBROUTINE get_current_time_string
      
      SUBROUTINE get_current_grid_name( grid_str )
        CHARACTER(LEN=*), INTENT(OUT) :: grid_str
        grid_str = ''
      END SUBROUTINE get_current_grid_name
      
      SUBROUTINE wrf_error_fatal(s)
        implicit none
        character(*) :: s
        write(0,*) s
        write(6,*) s
        call wrf_abort()
      END SUBROUTINE wrf_error_fatal
      
      SUBROUTINE wrf_message(s)
        implicit none
        character(*) :: s
        write(6,*) s
      END SUBROUTINE wrf_message
      
      SUBROUTINE wrf_debug(i,s)
        use module_wrf_error_dummy
        implicit none
        integer :: i
        character(*) :: s
        if(i<=wrf_debug_level) write(6,*) s
      END SUBROUTINE wrf_debug
      
      SUBROUTINE set_wrf_debug_level(i)
        use module_wrf_error_dummy
        implicit none
        integer i
        wrf_debug_level=i
      END SUBROUTINE set_wrf_debug_level
