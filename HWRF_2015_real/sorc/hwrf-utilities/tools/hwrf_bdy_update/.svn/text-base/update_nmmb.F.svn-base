       SUBROUTINE update_wrfnmm (bocofile,gsifilename)


!!   USE module_bc
   IMPLICIT NONE




  integer :: i,j,k
  integer :: ids,ide, jds,jde, kds,kde
  integer :: ims,ime, jms,jme, kms,kme
  integer :: its,ite, jts,jte, kts,kte
  integer :: ips,ipe, jps,jpe, kps,kpe


	character(len=20) :: bocofile, bocofile_update
        character(len=20) :: gsifilename
        CHARACTER(LEN=19) :: start_date_char , end_date_char , &
                        current_date_char , next_date_char
	character(len=255) :: message

        INTEGER :: time_loop_max , loop

        integer, save :: id,id2, ierr
        INTEGER :: switch, fid, rc, idum1,idum2, debug_level
        LOGICAL, parameter:: replace=.true.

         REAL :: tboco


  REAL :: TBT,UBT,VBT,QBT,CWMBT
  REAL :: PDBT

  REAL,ALLOCATABLE,DIMENSION(:,:)::TBA,UBA,VBA,QBA,CWMBA
  REAL,ALLOCATABLE,DIMENSION(:)::PDBA

  INTEGER :: KB, LM, IM, JM, iunit, N, NSTART, NSTART_V, ioerror
  INTEGER :: jstart, jend, inc_h, inc_v

        CHARACTER*40            :: this_datestr, next_datestr



	loop=1


	write(0,*) 'attempt to open for read : ', trim(bocofile)


!      DO loop = 1 , time_loop_max-1
!         CALL input_boundary ( id, grid , config_flags , ierr )


      first_replace:	if (loop .eq. 1 .and. replace) then

!         KB = 2*(IDE-1) + (JDE-1) - 3
          KB = 2*IM+2*JM
          KB_V = KB-4
         
          IDS=1
          JDS=1
      
        LM = KDE-1

        ALLOCATE(TBA(KB,LM))
        ALLOCATE(QBA(KB,LM))
        ALLOCATE(CWMBA(KB,LM))
        ALLOCATE(UBA(KB_V,LM))
        ALLOCATE(VBA(KB_V,LM))
        ALLOCATE(PDBA(KB))

        ALLOCATE(pdb_n_g(IDS:IEND,1:LNSH,2))
        ALLOCATE(pdb_s_g(IDS:IEND,1:LNSH,2))
        ALLOCATE(pdb_w_g(1:LNSH,JDS:JEND,2))
        ALLOCATE(pdb_e_g(1:LNSH,JDS:JEND,2))

        ALLOCATE(tb_n_g(IDS:IEND,1:LNSH,LM,2))
        ALLOCATE(tb_s_g(IDS:IEND,1:LNSH,LM,2))
        ALLOCATE(tb_w_g(1:LNSH,JDS:JEND,LM,2))
        ALLOCATE(tb_e_g(1:LNSH,JDS:JEND,LM,2))

        ALLOCATE(qb_n_g(IDS:IEND,1:LNSH,LM,2))
        ALLOCATE(qb_s_g(IDS:IEND,1:LNSH,LM,2))
        ALLOCATE(qb_w_g(1:LNSH,JDS:JEND,LM,2))
        ALLOCATE(qb_e_g(1:LNSH,JDS:JEND,LM,2))

        ALLOCATE(cwmb_n_g(IDS:IEND,1:LNSH,LM,2))
        ALLOCATE(cwmb_s_g(IDS:IEND,1:LNSH,LM,2))
        ALLOCATE(cwmb_w_g(1:LNSH,JDS:JEND,LM,2))
        ALLOCATE(cwmb_e_g(1:LNSH,JDS:JEND,LM,2))

        ALLOCATE(ub_n_g(IDS:IEND,1:LNSV,LM,2))
        ALLOCATE(ub_s_g(IDS:IEND,1:LNSV,LM,2))
        ALLOCATE(ub_w_g(1:LNSV,JDS:JEND,LM,2))
        ALLOCATE(ub_e_g(1:LNSV,JDS:JEND,LM,2))

        ALLOCATE(vb_n_g(IDS:IEND,1:LNSV,LM,2))
        ALLOCATE(vb_s_g(IDS:IEND,1:LNSV,LM,2))
        ALLOCATE(vb_w_g(1:LNSV,JDS:JEND,LM,2))
        ALLOCATE(vb_e_g(1:LNSV,JDS:JEND,LM,2))

	open (unit=iunit, file=trim(bocofile_update), form='unformatted')

!        read(iunit) runbc, idatbc, IHRSTBC, tboco
!        read(iunit) pdb_s_g, pdb_n_g, pdb_w_g, pdb_e_g
!        read(iunit) tb_s_g, tb_n_g, tb_w_g, tb_e_g
!        read(iunit) qb_s_g, qb_n_g, qb_w_g, qb_e_g
!        read(iunit) cwmb_s_g, cwmb_n_g, cwmb_w_g, cwmb_e_g
!        read(iunit) ub_s_g, ub_n_g, ub_w_g, ub_e_g
!        readiiunit) vb_s_g, vb_n_g, vb_w_g, vb_e_g
         close(iunit)


	write(0,*) 'trim(gsifilename) is: ', trim(gsifilename)

! need nmmb version of readgsifile (or switchable in code)
         call readgsifile( trim(gsifilename),KB,KB_V,LM,PDBA,TBA,QBA,CWMBA,UBA,VBA)

	write(0,*) 'return KB, LM: ', KB, LM

! **** Overwrite the bdy values, and recompute the tendencies based on the new initial value.

   SOUTH:     IF(JPS.EQ.JDS)THEN  ! **************************************************************
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

        ENDIF SOUTH

   NORTH:     IF(JPE.GE.JDE-1)THEN ! **********************************************************************


           NSTART=N
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


   WEST:     IF(IPS.EQ.IDS)THEN ! ****************************************************************************

	write(0,*) 'size(t_bxs): ', size(grid%t_bxs,dim=1),  size(grid%t_bxs,dim=2), size(grid%t_bxs,dim=3)
           NSTART=N
              inc_h=mod(jps+1,2)
              inc_v=mod(jps,2)
              jstart=jps+inc_h
              jend=min(jde,jpe)
	write(0,*) 'start west H at: ', NSTART

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

   EAST:     IF(IPE.GE.IDE-1)THEN ! **********************************************************************

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

! ********************************************************************
	endif first_replace
! ********************************************************************

	bocofile_update='boco.0000_update'
	

!	write(0,*) 'current_date_char: ', current_date_char
!	write(0,*) 'start_date_char: ', start_date_char
	
!      CALL geth_newdate ( current_date_char , start_date_char , &
!                          loop * model_config_rec%interval_seconds )
!
!      current_date =  current_date_char // '.0000'
!	write(0,*) 'current_date defined as: ', current_date(1:19)
!
!        CALL domain_clock_set( grid, current_timestr=current_date(1:19) )

!      temp24=current_date
!      temp24b=start_date
!      start_date = current_date
!      CALL geth_newdate ( temp19 , temp24b(1:19) , &
!                         (loop-1) * model_config_rec%interval_seconds )
!      current_date = temp19 //  '.0000'
!!       CALL domain_clock_set( grid, current_date(1:19) )
!      write(0,*) 'LBC valid between these times ',current_date, ' ',start_date

!         CALL output_boundary ( id2, grid , config_flags , ierr )



	open (unit=iunit, file=trim(bocofile_update), form='unformatted')

!        write(iunit) runbc, idatbc, IHRSTBC, tboco
!        write(iunit) bdy%pdb_s_g, bdy%pdb_n_g, bdy%pdb_w_g, bdy%pdb_e_g
!        write(iunit) bdy%tb_s_g, bdy%tb_n_g, bdy%tb_w_g, bdy%tb_e_g
!        write(iunit) bdy%qb_s_g, bdy%qb_n_g, bdy%qb_w_g, bdy%qb_e_g
!        write(iunit) bdy%cwmb_s_g, bdy%cwmb_n_g, bdy%cwmb_w_g, bdy%cwmb_e_g
!        write(iunit) bdy%ub_s_g, bdy%ub_n_g, bdy%ub_w_g, bdy%ub_e_g
!        writeiiunit) bdy%vb_s_g, bdy%vb_n_g, bdy%vb_w_g, bdy%vb_e_g
         close(iunit)


!	enddo  ! loop

       END SUBROUTINE update_wrfnmm
