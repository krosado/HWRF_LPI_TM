SUBROUTINE shift_domain_nmm ( grid , disp_x, disp_y &







,szj,s1z,spz,tcs,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist, &
dfi_moist_bxs,dfi_moist_bxe,dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar, &
scalar_bxs,scalar_bxe,scalar_bys,scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs, &
dfi_scalar_bxe,dfi_scalar_bys,dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,chem,ozmixm &


                           )
   USE module_domain
   USE module_timing
   USE module_configure
   USE module_dm
   USE module_comm_dm
   USE module_timing
   IMPLICIT NONE
  
   INTEGER disp_x, disp_y       
   TYPE(domain) , POINTER                     :: grid

  
   INTEGER  :: i, j, ii, ipf, jpf
   INTEGER  :: px, py       
   INTEGER  :: ids , ide , jds , jde , kds , kde , &
               ims , ime , jms , jme , kms , kme , &
               ips , ipe , jps , jpe , kps , kpe
   TYPE (grid_config_rec_type)  :: config_flags
   TYPE( fieldlist ), POINTER :: p

   LOGICAL :: E_BDY,N_BDY,S_BDY,W_BDY

   CHARACTER(LEN=255) :: message

   






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













   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   CALL get_ijk_from_grid (  grid ,                           &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe     )


   S_BDY=(JPS==JDS)
   N_BDY=(JPE==JDE)
   W_BDY=(IPS==IDS)
   E_BDY=(IPE==IDE)

   write(message,*)' S_BDY,N_BDY,W_BDY,E_BDY ', S_BDY,N_BDY,W_BDY,E_BDY
   CALL wrf_message(trim(message))

   grid%imask_nostag=0
   IF ( disp_x > 0 ) THEN
      IF ( E_BDY ) THEN 
         DO J=jps,min(jde-1,jpe)
         DO I=ips,min(ide-1,ipe-2-mod(j+1,2)) 
            grid%imask_nostag(i,j) = 1
         END DO
         END DO
      ELSE
         DO J=jps,min(jde-1,jpe)
         DO I=ips,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 1
         END DO
         END DO
      END IF

   IF ( disp_y > 0 ) THEN
      IF ( N_BDY ) THEN
         DO J=min(jde-1,jpe-2),max(jde-1,jpe)
         DO I=ips,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 0
         END DO
         END DO
      ENDIF
   ELSEIF ( disp_y < 0 ) THEN
      IF ( S_BDY ) THEN
         DO J=jps,jps+1
         DO I=ips,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 0
         END DO
         END DO
      ENDIF
   ENDIF      

   ELSEIF ( disp_x < 0 ) THEN
      IF ( W_BDY ) THEN 
         DO J=jps,min(jde-1,jpe)
         DO I=ips+1,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 1
         END DO
         END DO
      ELSE
         DO J=jps,min(jde-1,jpe)
         DO I=ips,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 1
         END DO
         END DO
      END IF

   IF ( disp_y > 0 ) THEN
      IF ( N_BDY ) THEN
         DO J=min(jde-1,jpe-2),max(jde-1,jpe)
         DO I=ips,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 0
         END DO
         END DO
      ENDIF
   ELSEIF ( disp_y < 0 ) THEN
      IF ( S_BDY ) THEN
         DO J=jps,jps+1
         DO I=ips,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 0
         END DO
         END DO
      ENDIF
   ENDIF      

   ELSE                            

   IF ( disp_y > 0 ) THEN
      IF ( N_BDY ) THEN 
         DO J=jps,min(jde-1,jpe-3)
         DO I=ips,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 1
         END DO
         END DO
      ELSE
         DO J=jps,min(jde-1,jpe)
         DO I=ips,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 1
         END DO
         END DO
      END IF
   END IF
   IF ( disp_y < 0 ) THEN
      IF ( S_BDY ) THEN
         DO J=jps+2,min(jde-1,jpe)
         DO I=ips,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 1
         END DO
         END DO
      ELSE
         DO J=jps,min(jde-1,jpe)
         DO I=ips,min(ide-1,ipe)
            grid%imask_nostag(i,j) = 1
         END DO
         END DO
      END IF
   END IF

   END IF



   px = isign(grid%parent_grid_ratio,disp_x)
   py = isign(grid%parent_grid_ratio,disp_y)


   do ii = 1,abs(disp_x)






CALL SHIFT_HALO_X_HALO_sub ( grid, &
  config_flags, &
  num_szj, &
  szj, &
  num_s1z, &
  s1z, &
  num_spz, &
  spz, &
  num_tcs, &
  tcs, &
  num_moist, &
  moist, &
  num_dfi_moist, &
  dfi_moist, &
  num_scalar, &
  scalar, &
  num_dfi_scalar, &
  dfi_scalar, &
  num_chem, &
  chem, &
  num_ozmixm, &
  ozmixm, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

      p => grid%head_statevars%next
      DO WHILE ( ASSOCIATED( p ) ) 
        IF ( p%ProcOrient .NE. 'X' .AND. p%ProcOrient .NE. 'Y' ) THEN
          IF ( INDEX(TRIM(p%Stagger),'X') .GT. 0 ) THEN
            ipf = MIN(ipe,ide)
          ELSE
            ipf = MIN(ipe,ide-1)
          ENDIF
          IF ( p%Ndim .EQ. 2 ) THEN
            IF (      p%MemoryOrder(1:1) .EQ. 'X' .AND.  p%MemoryOrder(2:2) .EQ.  'Y' ) THEN
              IF      ( p%Type .EQ. 'r' ) THEN
                IF ( SIZE(p%rfield_2d,1)*SIZE(p%rfield_2d,2) .GT. 1 ) THEN
                  p%rfield_2d(ips:ipf,jms:jme) = p%rfield_2d(ips+px:ipf+px,jms:jme)
                ENDIF
              ELSE IF ( p%Type .EQ. 'd' ) THEN
                IF ( SIZE(p%dfield_2d,1)*SIZE(p%dfield_2d,2) .GT. 1 ) THEN
                  p%dfield_2d(ips:ipf,jms:jme) = p%dfield_2d(ips+px:ipf+px,jms:jme)
                ENDIF
              ELSE IF ( TRIM(p%Type) .EQ. 'i' ) THEN
                IF ( SIZE(p%ifield_2d,1)*SIZE(p%ifield_2d,2) .GT. 1 ) THEN
                  p%ifield_2d(ips:ipf,jms:jme) = p%ifield_2d(ips+px:ipf+px,jms:jme)
                ENDIF
              ELSE IF ( p%Type .EQ. 'l' ) THEN
                IF ( SIZE(p%lfield_2d,1)*SIZE(p%lfield_2d,2) .GT. 1 ) THEN
                  p%lfield_2d(ips:ipf,jms:jme) = p%lfield_2d(ips+px:ipf+px,jms:jme)
                ENDIF
              ENDIF
            ENDIF
          ELSE IF ( p%Ndim .EQ. 3 ) THEN
            IF (      p%MemoryOrder(1:1) .EQ. 'X' .AND.  p%MemoryOrder(3:3) .EQ.  'Y' ) THEN
              IF      ( p%Type .EQ. 'r' ) THEN
                IF ( SIZE(p%rfield_3d,1)*SIZE(p%rfield_3d,3) .GT. 1 ) THEN
                  p%rfield_3d(ips:ipf,:,jms:jme) = p%rfield_3d(ips+px:ipf+px,:,jms:jme)
                ENDIF
              ELSE IF ( p%Type .EQ. 'd' ) THEN
                IF ( SIZE(p%dfield_3d,1)*SIZE(p%dfield_3d,3) .GT. 1 ) THEN
                  p%dfield_3d(ips:ipf,:,jms:jme) = p%dfield_3d(ips+px:ipf+px,:,jms:jme)
                ENDIF
              ELSE IF ( p%Type .EQ. 'i' ) THEN
                IF ( SIZE(p%ifield_3d,1)*SIZE(p%ifield_3d,3) .GT. 1 ) THEN
                  p%ifield_3d(ips:ipf,:,jms:jme) = p%ifield_3d(ips+px:ipf+px,:,jms:jme)
                ENDIF
              ELSE IF ( p%Type .EQ. 'l' ) THEN
                CALL wrf_error_fatal3("<stdin>",304,&
'3D logical arrays cannot be shifted for moving nests' )
              ENDIF
            ELSE IF (  p%MemoryOrder(1:2) .EQ. 'XY' ) THEN
              IF      ( p%Type .EQ. 'r' ) THEN
                IF ( SIZE(p%rfield_3d,1)*SIZE(p%rfield_3d,2) .GT. 1 ) THEN
                  p%rfield_3d(ips:ipf,jms:jme,:) = p%rfield_3d(ips+px:ipf+px,jms:jme,:)
                ENDIF
              ELSE IF ( p%Type .EQ. 'd' ) THEN
                IF ( SIZE(p%dfield_3d,1)*SIZE(p%dfield_3d,2) .GT. 1 ) THEN
                  p%dfield_3d(ips:ipf,jms:jme,:) = p%dfield_3d(ips+px:ipf+px,jms:jme,:)
                ENDIF
              ELSE IF ( p%Type .EQ. 'i' ) THEN
                IF ( SIZE(p%ifield_3d,1)*SIZE(p%ifield_3d,2) .GT. 1 ) THEN
                  p%ifield_3d(ips:ipf,jms:jme,:) = p%ifield_3d(ips+px:ipf+px,jms:jme,:)
                ENDIF
              ELSE IF ( p%Type .EQ. 'l' ) THEN
                CALL wrf_error_fatal3("<stdin>",321,&
'3D logical arrays cannot be shifted for moving nests' )
              ENDIF
            ENDIF
          ELSE IF ( p%Ndim .EQ. 4 ) THEN
            IF (      p%MemoryOrder(1:1) .EQ. 'X' .AND.  p%MemoryOrder(3:3) .EQ.  'Y' ) THEN
              IF      ( p%Type .EQ. 'r' ) THEN
                IF ( SIZE(p%rfield_4d,1)*SIZE(p%rfield_4d,3) .GT. 1 ) THEN
                  DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                    p%rfield_4d(ips:ipf,:,jms:jme,itrace) = p%rfield_4d(ips+px:ipf+px,:,jms:jme,itrace)
                  ENDDO
                ENDIF
              ELSE IF ( p%Type .EQ. 'd' ) THEN
                IF ( SIZE(p%dfield_4d,1)*SIZE(p%dfield_4d,3) .GT. 1 ) THEN
                  DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                    p%dfield_4d(ips:ipf,:,jms:jme,itrace) = p%dfield_4d(ips+px:ipf+px,:,jms:jme,itrace)
                  ENDDO
                ENDIF
              ELSE IF ( p%Type .EQ. 'i' ) THEN
                IF ( SIZE(p%ifield_4d,1)*SIZE(p%ifield_4d,3) .GT. 1 ) THEN
                  DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                    p%ifield_4d(ips:ipf,:,jms:jme,itrace) = p%ifield_4d(ips+px:ipf+px,:,jms:jme,itrace)
                  ENDDO
                ENDIF
              ELSE IF ( p%Type .EQ. 'l' ) THEN
                CALL wrf_error_fatal3("<stdin>",346,&
'4D logical arrays cannot be shifted for moving nests' )
              ENDIF
            ELSE IF (  p%MemoryOrder(1:2) .EQ. 'XY' ) THEN
              IF      ( p%Type .EQ. 'r' ) THEN
                IF ( SIZE(p%rfield_4d,1)*SIZE(p%rfield_4d,2) .GT. 1 ) THEN
                  DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                    p%rfield_4d(ips:ipf,jms:jme,:,itrace) = p%rfield_4d(ips+px:ipf+px,jms:jme,:,itrace)
                  ENDDO
                ENDIF
              ELSE IF ( p%Type .EQ. 'd' ) THEN
                IF ( SIZE(p%dfield_4d,1)*SIZE(p%dfield_4d,2) .GT. 1 ) THEN
                  DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                    p%dfield_4d(ips:ipf,jms:jme,:,itrace) = p%dfield_4d(ips+px:ipf+px,jms:jme,:,itrace)
                  ENDDO
                ENDIF
              ELSE IF ( p%Type .EQ. 'i' ) THEN
                IF ( SIZE(p%ifield_4d,1)*SIZE(p%ifield_4d,2) .GT. 1 ) THEN
                  DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                    p%ifield_4d(ips:ipf,jms:jme,:,itrace) = p%ifield_4d(ips+px:ipf+px,jms:jme,:,itrace)
                  ENDDO
                ENDIF
              ELSE IF ( p%Type .EQ. 'l' ) THEN
                CALL wrf_error_fatal3("<stdin>",369,&
'4D logical arrays cannot be shifted for moving nests' )
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        p => p%next
      ENDDO
   enddo


   do ii = 1,abs(disp_y)






CALL SHIFT_HALO_Y_HALO_sub ( grid, &
  config_flags, &
  num_szj, &
  szj, &
  num_s1z, &
  s1z, &
  num_spz, &
  spz, &
  num_tcs, &
  tcs, &
  num_moist, &
  moist, &
  num_dfi_moist, &
  dfi_moist, &
  num_scalar, &
  scalar, &
  num_dfi_scalar, &
  dfi_scalar, &
  num_chem, &
  chem, &
  num_ozmixm, &
  ozmixm, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

      p => grid%head_statevars%next
      DO WHILE ( ASSOCIATED( p ) ) 
        IF ( p%ProcOrient .NE. 'X' .AND. p%ProcOrient .NE. 'Y' ) THEN
          IF ( INDEX(TRIM(p%Stagger),'Y') .GT. 0 ) THEN
            jpf = MIN(jpe,jde)
          ELSE
            jpf = MIN(jpe,jde-1)
          ENDIF
          IF ( p%Ndim .EQ. 2 ) THEN
            IF (      p%MemoryOrder(1:1) .EQ. 'X' .AND.  p%MemoryOrder(2:2) .EQ.  'Y' ) THEN
              IF      ( p%Type .EQ. 'r' ) THEN
                IF ( SIZE(p%rfield_2d,1)*SIZE(p%rfield_2d,2) .GT. 1 ) THEN
                  p%rfield_2d(ims:ime,jps:jpf) = p%rfield_2d(ims:ime,jps+py:jpf+py)
                ENDIF
              ELSE IF ( p%Type .EQ. 'd' ) THEN
                IF ( SIZE(p%dfield_2d,1)*SIZE(p%dfield_2d,2) .GT. 1 ) THEN
                  p%dfield_2d(ims:ime,jps:jpf) = p%dfield_2d(ims:ime,jps+py:jpf+py)
                ENDIF
              ELSE IF ( p%Type .EQ. 'i' ) THEN
                IF ( SIZE(p%ifield_2d,1)*SIZE(p%ifield_2d,2) .GT. 1 ) THEN
                  p%ifield_2d(ims:ime,jps:jpf) = p%ifield_2d(ims:ime,jps+py:jpf+py)
                ENDIF
              ELSE IF ( p%Type .EQ. 'l' ) THEN
                IF ( SIZE(p%lfield_2d,1)*SIZE(p%lfield_2d,2) .GT. 1 ) THEN
                  p%lfield_2d(ims:ime,jps:jpf) = p%lfield_2d(ims:ime,jps+py:jpf+py)
                ENDIF
              ENDIF
            ENDIF
          ELSE IF ( p%Ndim .EQ. 3 ) THEN
            IF (      p%MemoryOrder(1:1) .EQ. 'X' .AND.  p%MemoryOrder(3:3) .EQ.  'Y' ) THEN
              IF      ( p%Type .EQ. 'r' ) THEN
                IF ( SIZE(p%rfield_3d,1)*SIZE(p%rfield_3d,3) .GT. 1 ) THEN
                 p%rfield_3d(ims:ime,:,jps:jpf) = p%rfield_3d(ims:ime,:,jps+py:jpf+py)

                ENDIF
              ELSE IF ( p%Type .EQ. 'd' ) THEN
                IF ( SIZE(p%dfield_3d,1)*SIZE(p%dfield_3d,3) .GT. 1 ) THEN
                  p%dfield_3d(ims:ime,:,jps:jpf) = p%dfield_3d(ims:ime,:,jps+py:jpf+py)
                ENDIF
              ELSE IF ( p%Type .EQ. 'i' ) THEN
                IF ( SIZE(p%ifield_3d,1)*SIZE(p%ifield_3d,3) .GT. 1 ) THEN
                  p%ifield_3d(ims:ime,:,jps:jpf) = p%ifield_3d(ims:ime,:,jps+py:jpf+py)
                ENDIF
              ELSE IF ( p%Type .EQ. 'l' ) THEN
                CALL wrf_error_fatal3("<stdin>",459,&
'3D logical arrays cannot be shifted for moving nests' )
              ENDIF
            ELSE IF (  p%MemoryOrder(1:2) .EQ. 'XY' ) THEN
              IF      ( p%Type .EQ. 'r' ) THEN
                IF ( SIZE(p%rfield_3d,1)*SIZE(p%rfield_3d,2) .GT. 1 ) THEN
                  p%rfield_3d(ims:ime,jps:jpf,:) = p%rfield_3d(ims:ime,jps+py:jpf+py,:)
                ENDIF
              ELSE IF ( p%Type .EQ. 'd' ) THEN
                IF ( SIZE(p%dfield_3d,1)*SIZE(p%dfield_3d,2) .GT. 1 ) THEN
                  p%dfield_3d(ims:ime,jps:jpf,:) = p%dfield_3d(ims:ime,jps+py:jpf+py,:)
                ENDIF
              ELSE IF ( p%Type .EQ. 'i' ) THEN
                IF ( SIZE(p%ifield_3d,1)*SIZE(p%ifield_3d,2) .GT. 1 ) THEN
                  p%ifield_3d(ims:ime,jps:jpf,:) = p%ifield_3d(ims:ime,jps+py:jpf+py,:)
                ENDIF
              ELSE IF ( p%Type .EQ. 'l' ) THEN
                CALL wrf_error_fatal3("<stdin>",476,&
'3D logical arrays cannot be shifted for moving nests' )
              ENDIF
            ENDIF
          ELSE IF ( p%Ndim .EQ. 4 ) THEN
            IF (      p%MemoryOrder(1:1) .EQ. 'X' .AND.  p%MemoryOrder(3:3) .EQ.  'Y' ) THEN
              IF      ( p%Type .EQ. 'r' ) THEN
                IF ( SIZE(p%rfield_4d,1)*SIZE(p%rfield_4d,3) .GT. 1 ) THEN
                  DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                    p%rfield_4d(ims:ime,:,jps:jpf,itrace) = p%rfield_4d(ims:ime,:,jps+py:jpf+py,itrace)
                  ENDDO
                ENDIF
              ELSE IF ( p%Type .EQ. 'd' ) THEN
                IF ( SIZE(p%dfield_4d,1)*SIZE(p%dfield_4d,3) .GT. 1 ) THEN
                  DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                    p%dfield_4d(ims:ime,:,jps:jpf,itrace) = p%dfield_4d(ims:ime,:,jps+py:jpf+py,itrace)
                  ENDDO
                ENDIF
              ELSE IF ( p%Type .EQ. 'i' ) THEN
                IF ( SIZE(p%ifield_4d,1)*SIZE(p%ifield_4d,3) .GT. 1 ) THEN
                  DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                    p%ifield_4d(ims:ime,:,jps:jpf,itrace) = p%ifield_4d(ims:ime,:,jps+py:jpf+py,itrace)
                  ENDDO
                ENDIF
              ELSE IF ( p%Type .EQ. 'l' ) THEN
                CALL wrf_error_fatal3("<stdin>",501,&
'4D logical arrays cannot be shifted for moving nests' )
              ENDIF
            ELSE IF (  p%MemoryOrder(1:2) .EQ. 'XY' ) THEN
              IF      ( p%Type .EQ. 'r' ) THEN
                IF ( SIZE(p%rfield_4d,1)*SIZE(p%rfield_4d,2) .GT. 1 ) THEN
                  DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                    p%rfield_4d(ims:ime,jps:jpf,:,itrace) = p%rfield_4d(ims:ime,jps+py:jpf+py,:,itrace)
                  ENDDO
                ENDIF
              ELSE IF ( p%Type .EQ. 'd' ) THEN
                IF ( SIZE(p%dfield_4d,1)*SIZE(p%dfield_4d,2) .GT. 1 ) THEN
                  DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                    p%dfield_4d(ims:ime,jps:jpf,:,itrace) = p%dfield_4d(ims:ime,jps+py:jpf+py,:,itrace)
                  ENDDO
                ENDIF
              ELSE IF ( p%Type .EQ. 'i' ) THEN
                IF ( SIZE(p%ifield_4d,1)*SIZE(p%ifield_4d,2) .GT. 1 ) THEN
                  DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                    p%ifield_4d(ims:ime,jps:jpf,:,itrace) = p%ifield_4d(ims:ime,jps+py:jpf+py,:,itrace)
                  ENDDO
                ENDIF
              ELSE IF ( p%Type .EQ. 'l' ) THEN
                CALL wrf_error_fatal3("<stdin>",524,&
'4D logical arrays cannot be shifted for moving nests' )
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        p => p%next
      ENDDO
   enddo




END SUBROUTINE shift_domain_nmm
