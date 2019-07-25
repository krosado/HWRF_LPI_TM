          PROGRAM bdyupdate

!   Program is intended to allow for updating the lateral boundary conditions
!   to match analysis updates of the model fields for the WRF-NMM and NEMS/NMMB.

!   Initial Coding - Feb 2012 - M Pyle

   USE module_dm
   USE module_configure
   USE module_timing
   USE module_bc_time_utilities
   USE module_date_time
   USE module_utility

         character(len=10):: core
         character(len=20):: bdyfilename,gsifilename
         integer :: ierr, rc

!   read in file to select model LBCs being updated, and other details

         read(5,*) core

         if (core(1:4) == 'NMMB' .or. core(1:4) == 'nmmb') then
            write(0,*) 'enter NMMB branch'
!            call update_nmmb ( ) 
         elseif (core(1:3) == 'NMM' .or. core(1:3) == 'nmm') then
!
! binary/netcdf
! where define the names of files used in update?
!
! file from GSI (updated LBC info)
! existing wrfbdy file (old LBC info to be modified)
!
          bdyfilename='wrfbdy_d01'
          gsifilename='wrf_nmm_bnd'
            call update_wrfnmm ( bdyfilename, gsifilename ) 

         else
            write(0,*) 'unrecognized core : ', core
            STOP
         endif

	write(0,*) 'just before end program'

    CALL wrf_dm_shutdown
	write(0,*) 'past wrf_dm_shutdown'
!    CALL WRFU_Finalize( rc=rc )
!	write(0,*) 'past WRFU_Finalize'

         END PROGRAM bdyupdate
