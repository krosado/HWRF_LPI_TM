!STARTOFREGISTRYGENERATEDINCLUDE 'inc/scalar_indices.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
  P_szj1 = 1 ; F_szj1 = .FALSE. 
  P_szj2 = 1 ; F_szj2 = .FALSE. 
  P_szj3 = 1 ; F_szj3 = .FALSE. 
  P_szj4 = 1 ; F_szj4 = .FALSE. 
  P_s1z1 = 1 ; F_s1z1 = .FALSE. 
  P_s1z2 = 1 ; F_s1z2 = .FALSE. 
  P_s1z3 = 1 ; F_s1z3 = .FALSE. 
  P_s1z4 = 1 ; F_s1z4 = .FALSE. 
  P_spz1 = 1 ; F_spz1 = .FALSE. 
  P_spz2 = 1 ; F_spz2 = .FALSE. 
  P_spz3 = 1 ; F_spz3 = .FALSE. 
  P_spz4 = 1 ; F_spz4 = .FALSE. 
  P_tcs1 = 1 ; F_tcs1 = .FALSE. 
  P_tcs2 = 1 ; F_tcs2 = .FALSE. 
  P_tcs3 = 1 ; F_tcs3 = .FALSE. 
  P_tcs4 = 1 ; F_tcs4 = .FALSE. 
  P_qv = 1 ; F_qv = .FALSE. 
  P_qc = 1 ; F_qc = .FALSE. 
  P_qr = 1 ; F_qr = .FALSE. 
  P_qi = 1 ; F_qi = .FALSE. 
  P_qs = 1 ; F_qs = .FALSE. 
  P_qg = 1 ; F_qg = .FALSE. 
  P_qh = 1 ; F_qh = .FALSE. 
  P_dfi_qv = 1 ; F_dfi_qv = .FALSE. 
  P_dfi_qc = 1 ; F_dfi_qc = .FALSE. 
  P_dfi_qr = 1 ; F_dfi_qr = .FALSE. 
  P_dfi_qi = 1 ; F_dfi_qi = .FALSE. 
  P_dfi_qs = 1 ; F_dfi_qs = .FALSE. 
  P_dfi_qg = 1 ; F_dfi_qg = .FALSE. 
  P_dfi_qh = 1 ; F_dfi_qh = .FALSE. 
  P_qni = 1 ; F_qni = .FALSE. 
  P_qt = 1 ; F_qt = .FALSE. 
  P_qns = 1 ; F_qns = .FALSE. 
  P_qnr = 1 ; F_qnr = .FALSE. 
  P_qng = 1 ; F_qng = .FALSE. 
  P_qnh = 1 ; F_qnh = .FALSE. 
  P_qnn = 1 ; F_qnn = .FALSE. 
  P_qnc = 1 ; F_qnc = .FALSE. 
  P_qvolg = 1 ; F_qvolg = .FALSE. 
  P_qnwfa = 1 ; F_qnwfa = .FALSE. 
  P_qnifa = 1 ; F_qnifa = .FALSE. 
  P_qndrop = 1 ; F_qndrop = .FALSE. 
  P_qrimef = 1 ; F_qrimef = .FALSE. 
  P_qvolh = 1 ; F_qvolh = .FALSE. 
  P_dfi_qndrop = 1 ; F_dfi_qndrop = .FALSE. 
  P_dfi_qni = 1 ; F_dfi_qni = .FALSE. 
  P_dfi_qt = 1 ; F_dfi_qt = .FALSE. 
  P_dfi_qns = 1 ; F_dfi_qns = .FALSE. 
  P_dfi_qnr = 1 ; F_dfi_qnr = .FALSE. 
  P_dfi_qng = 1 ; F_dfi_qng = .FALSE. 
  P_dfi_qnh = 1 ; F_dfi_qnh = .FALSE. 
  P_dfi_qnn = 1 ; F_dfi_qnn = .FALSE. 
  P_dfi_qnc = 1 ; F_dfi_qnc = .FALSE. 
  P_dfi_qnwfa = 1 ; F_dfi_qnwfa = .FALSE. 
  P_dfi_qnifa = 1 ; F_dfi_qnifa = .FALSE. 
  P_mth01 = 1 ; F_mth01 = .FALSE. 
  P_mth02 = 1 ; F_mth02 = .FALSE. 
  P_mth03 = 1 ; F_mth03 = .FALSE. 
  P_mth04 = 1 ; F_mth04 = .FALSE. 
  P_mth05 = 1 ; F_mth05 = .FALSE. 
  P_mth06 = 1 ; F_mth06 = .FALSE. 
  P_mth07 = 1 ; F_mth07 = .FALSE. 
  P_mth08 = 1 ; F_mth08 = .FALSE. 
  P_mth09 = 1 ; F_mth09 = .FALSE. 
  P_mth10 = 1 ; F_mth10 = .FALSE. 
  P_mth11 = 1 ; F_mth11 = .FALSE. 
  P_mth12 = 1 ; F_mth12 = .FALSE. 
  IF (model_config_rec%sf_lake_physics(idomain)==0)THEN
  END IF
  IF (model_config_rec%sf_lake_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%ntracers==4)THEN
   IF ( szj_index_table( PARAM_szj1 , idomain ) .lt. 1 ) THEN
     szj_num_table(idomain) = szj_num_table(idomain) + 1
     P_szj1 = szj_num_table(idomain)
     szj_index_table( PARAM_szj1 , idomain ) = P_szj1
   ELSE
     P_szj1 = szj_index_table( PARAM_szj1 , idomain )
   END IF
   szj_boundary_table( idomain, P_szj1 ) = .FALSE.
   szj_dname_table( idomain, P_szj1 ) = 'szj1'
   szj_desc_table( idomain, P_szj1 ) = 'szj'
   szj_units_table( idomain, P_szj1 ) = 'units'
   szj_streams_table( idomain, P_szj1 )%stream(1) = 0 ! 00000000 
   szj_streams_table( idomain, P_szj1 )%stream(2) = 2097152 ! 00200000 
   F_szj1 = .TRUE.
   IF ( szj_index_table( PARAM_szj2 , idomain ) .lt. 1 ) THEN
     szj_num_table(idomain) = szj_num_table(idomain) + 1
     P_szj2 = szj_num_table(idomain)
     szj_index_table( PARAM_szj2 , idomain ) = P_szj2
   ELSE
     P_szj2 = szj_index_table( PARAM_szj2 , idomain )
   END IF
   szj_boundary_table( idomain, P_szj2 ) = .FALSE.
   szj_dname_table( idomain, P_szj2 ) = 'szj2'
   szj_desc_table( idomain, P_szj2 ) = 'szj'
   szj_units_table( idomain, P_szj2 ) = 'units'
   szj_streams_table( idomain, P_szj2 )%stream(1) = 0 ! 00000000 
   szj_streams_table( idomain, P_szj2 )%stream(2) = 2097152 ! 00200000 
   F_szj2 = .TRUE.
   IF ( szj_index_table( PARAM_szj3 , idomain ) .lt. 1 ) THEN
     szj_num_table(idomain) = szj_num_table(idomain) + 1
     P_szj3 = szj_num_table(idomain)
     szj_index_table( PARAM_szj3 , idomain ) = P_szj3
   ELSE
     P_szj3 = szj_index_table( PARAM_szj3 , idomain )
   END IF
   szj_boundary_table( idomain, P_szj3 ) = .FALSE.
   szj_dname_table( idomain, P_szj3 ) = 'szj3'
   szj_desc_table( idomain, P_szj3 ) = 'szj'
   szj_units_table( idomain, P_szj3 ) = 'units'
   szj_streams_table( idomain, P_szj3 )%stream(1) = 0 ! 00000000 
   szj_streams_table( idomain, P_szj3 )%stream(2) = 2097152 ! 00200000 
   F_szj3 = .TRUE.
   IF ( szj_index_table( PARAM_szj4 , idomain ) .lt. 1 ) THEN
     szj_num_table(idomain) = szj_num_table(idomain) + 1
     P_szj4 = szj_num_table(idomain)
     szj_index_table( PARAM_szj4 , idomain ) = P_szj4
   ELSE
     P_szj4 = szj_index_table( PARAM_szj4 , idomain )
   END IF
   szj_boundary_table( idomain, P_szj4 ) = .FALSE.
   szj_dname_table( idomain, P_szj4 ) = 'szj4'
   szj_desc_table( idomain, P_szj4 ) = 'szj'
   szj_units_table( idomain, P_szj4 ) = 'units'
   szj_streams_table( idomain, P_szj4 )%stream(1) = 0 ! 00000000 
   szj_streams_table( idomain, P_szj4 )%stream(2) = 2097152 ! 00200000 
   F_szj4 = .TRUE.
   IF ( s1z_index_table( PARAM_s1z1 , idomain ) .lt. 1 ) THEN
     s1z_num_table(idomain) = s1z_num_table(idomain) + 1
     P_s1z1 = s1z_num_table(idomain)
     s1z_index_table( PARAM_s1z1 , idomain ) = P_s1z1
   ELSE
     P_s1z1 = s1z_index_table( PARAM_s1z1 , idomain )
   END IF
   s1z_boundary_table( idomain, P_s1z1 ) = .FALSE.
   s1z_dname_table( idomain, P_s1z1 ) = 's1z1'
   s1z_desc_table( idomain, P_s1z1 ) = 's1z'
   s1z_units_table( idomain, P_s1z1 ) = 'units'
   s1z_streams_table( idomain, P_s1z1 )%stream(1) = 0 ! 00000000 
   s1z_streams_table( idomain, P_s1z1 )%stream(2) = 2097152 ! 00200000 
   F_s1z1 = .TRUE.
   IF ( s1z_index_table( PARAM_s1z2 , idomain ) .lt. 1 ) THEN
     s1z_num_table(idomain) = s1z_num_table(idomain) + 1
     P_s1z2 = s1z_num_table(idomain)
     s1z_index_table( PARAM_s1z2 , idomain ) = P_s1z2
   ELSE
     P_s1z2 = s1z_index_table( PARAM_s1z2 , idomain )
   END IF
   s1z_boundary_table( idomain, P_s1z2 ) = .FALSE.
   s1z_dname_table( idomain, P_s1z2 ) = 's1z2'
   s1z_desc_table( idomain, P_s1z2 ) = 's1z'
   s1z_units_table( idomain, P_s1z2 ) = 'units'
   s1z_streams_table( idomain, P_s1z2 )%stream(1) = 0 ! 00000000 
   s1z_streams_table( idomain, P_s1z2 )%stream(2) = 2097152 ! 00200000 
   F_s1z2 = .TRUE.
   IF ( s1z_index_table( PARAM_s1z3 , idomain ) .lt. 1 ) THEN
     s1z_num_table(idomain) = s1z_num_table(idomain) + 1
     P_s1z3 = s1z_num_table(idomain)
     s1z_index_table( PARAM_s1z3 , idomain ) = P_s1z3
   ELSE
     P_s1z3 = s1z_index_table( PARAM_s1z3 , idomain )
   END IF
   s1z_boundary_table( idomain, P_s1z3 ) = .FALSE.
   s1z_dname_table( idomain, P_s1z3 ) = 's1z3'
   s1z_desc_table( idomain, P_s1z3 ) = 's1z'
   s1z_units_table( idomain, P_s1z3 ) = 'units'
   s1z_streams_table( idomain, P_s1z3 )%stream(1) = 0 ! 00000000 
   s1z_streams_table( idomain, P_s1z3 )%stream(2) = 2097152 ! 00200000 
   F_s1z3 = .TRUE.
   IF ( s1z_index_table( PARAM_s1z4 , idomain ) .lt. 1 ) THEN
     s1z_num_table(idomain) = s1z_num_table(idomain) + 1
     P_s1z4 = s1z_num_table(idomain)
     s1z_index_table( PARAM_s1z4 , idomain ) = P_s1z4
   ELSE
     P_s1z4 = s1z_index_table( PARAM_s1z4 , idomain )
   END IF
   s1z_boundary_table( idomain, P_s1z4 ) = .FALSE.
   s1z_dname_table( idomain, P_s1z4 ) = 's1z4'
   s1z_desc_table( idomain, P_s1z4 ) = 's1z'
   s1z_units_table( idomain, P_s1z4 ) = 'units'
   s1z_streams_table( idomain, P_s1z4 )%stream(1) = 0 ! 00000000 
   s1z_streams_table( idomain, P_s1z4 )%stream(2) = 2097152 ! 00200000 
   F_s1z4 = .TRUE.
   IF ( spz_index_table( PARAM_spz1 , idomain ) .lt. 1 ) THEN
     spz_num_table(idomain) = spz_num_table(idomain) + 1
     P_spz1 = spz_num_table(idomain)
     spz_index_table( PARAM_spz1 , idomain ) = P_spz1
   ELSE
     P_spz1 = spz_index_table( PARAM_spz1 , idomain )
   END IF
   spz_boundary_table( idomain, P_spz1 ) = .FALSE.
   spz_dname_table( idomain, P_spz1 ) = 'spz1'
   spz_desc_table( idomain, P_spz1 ) = 'spz'
   spz_units_table( idomain, P_spz1 ) = 'units'
   spz_streams_table( idomain, P_spz1 )%stream(1) = 0 ! 00000000 
   spz_streams_table( idomain, P_spz1 )%stream(2) = 2097152 ! 00200000 
   F_spz1 = .TRUE.
   IF ( spz_index_table( PARAM_spz2 , idomain ) .lt. 1 ) THEN
     spz_num_table(idomain) = spz_num_table(idomain) + 1
     P_spz2 = spz_num_table(idomain)
     spz_index_table( PARAM_spz2 , idomain ) = P_spz2
   ELSE
     P_spz2 = spz_index_table( PARAM_spz2 , idomain )
   END IF
   spz_boundary_table( idomain, P_spz2 ) = .FALSE.
   spz_dname_table( idomain, P_spz2 ) = 'spz2'
   spz_desc_table( idomain, P_spz2 ) = 'spz'
   spz_units_table( idomain, P_spz2 ) = 'units'
   spz_streams_table( idomain, P_spz2 )%stream(1) = 0 ! 00000000 
   spz_streams_table( idomain, P_spz2 )%stream(2) = 2097152 ! 00200000 
   F_spz2 = .TRUE.
   IF ( spz_index_table( PARAM_spz3 , idomain ) .lt. 1 ) THEN
     spz_num_table(idomain) = spz_num_table(idomain) + 1
     P_spz3 = spz_num_table(idomain)
     spz_index_table( PARAM_spz3 , idomain ) = P_spz3
   ELSE
     P_spz3 = spz_index_table( PARAM_spz3 , idomain )
   END IF
   spz_boundary_table( idomain, P_spz3 ) = .FALSE.
   spz_dname_table( idomain, P_spz3 ) = 'spz3'
   spz_desc_table( idomain, P_spz3 ) = 'spz'
   spz_units_table( idomain, P_spz3 ) = 'units'
   spz_streams_table( idomain, P_spz3 )%stream(1) = 0 ! 00000000 
   spz_streams_table( idomain, P_spz3 )%stream(2) = 2097152 ! 00200000 
   F_spz3 = .TRUE.
   IF ( spz_index_table( PARAM_spz4 , idomain ) .lt. 1 ) THEN
     spz_num_table(idomain) = spz_num_table(idomain) + 1
     P_spz4 = spz_num_table(idomain)
     spz_index_table( PARAM_spz4 , idomain ) = P_spz4
   ELSE
     P_spz4 = spz_index_table( PARAM_spz4 , idomain )
   END IF
   spz_boundary_table( idomain, P_spz4 ) = .FALSE.
   spz_dname_table( idomain, P_spz4 ) = 'spz4'
   spz_desc_table( idomain, P_spz4 ) = 'spz'
   spz_units_table( idomain, P_spz4 ) = 'units'
   spz_streams_table( idomain, P_spz4 )%stream(1) = 0 ! 00000000 
   spz_streams_table( idomain, P_spz4 )%stream(2) = 2097152 ! 00200000 
   F_spz4 = .TRUE.
   IF ( tcs_index_table( PARAM_tcs1 , idomain ) .lt. 1 ) THEN
     tcs_num_table(idomain) = tcs_num_table(idomain) + 1
     P_tcs1 = tcs_num_table(idomain)
     tcs_index_table( PARAM_tcs1 , idomain ) = P_tcs1
   ELSE
     P_tcs1 = tcs_index_table( PARAM_tcs1 , idomain )
   END IF
   tcs_boundary_table( idomain, P_tcs1 ) = .FALSE.
   tcs_dname_table( idomain, P_tcs1 ) = 'tcs1'
   tcs_desc_table( idomain, P_tcs1 ) = 'tcs'
   tcs_units_table( idomain, P_tcs1 ) = 'units'
   tcs_streams_table( idomain, P_tcs1 )%stream(1) = 0 ! 00000000 
   tcs_streams_table( idomain, P_tcs1 )%stream(2) = 2097152 ! 00200000 
   F_tcs1 = .TRUE.
   IF ( tcs_index_table( PARAM_tcs2 , idomain ) .lt. 1 ) THEN
     tcs_num_table(idomain) = tcs_num_table(idomain) + 1
     P_tcs2 = tcs_num_table(idomain)
     tcs_index_table( PARAM_tcs2 , idomain ) = P_tcs2
   ELSE
     P_tcs2 = tcs_index_table( PARAM_tcs2 , idomain )
   END IF
   tcs_boundary_table( idomain, P_tcs2 ) = .FALSE.
   tcs_dname_table( idomain, P_tcs2 ) = 'tcs2'
   tcs_desc_table( idomain, P_tcs2 ) = 'tcs'
   tcs_units_table( idomain, P_tcs2 ) = 'units'
   tcs_streams_table( idomain, P_tcs2 )%stream(1) = 0 ! 00000000 
   tcs_streams_table( idomain, P_tcs2 )%stream(2) = 2097152 ! 00200000 
   F_tcs2 = .TRUE.
   IF ( tcs_index_table( PARAM_tcs3 , idomain ) .lt. 1 ) THEN
     tcs_num_table(idomain) = tcs_num_table(idomain) + 1
     P_tcs3 = tcs_num_table(idomain)
     tcs_index_table( PARAM_tcs3 , idomain ) = P_tcs3
   ELSE
     P_tcs3 = tcs_index_table( PARAM_tcs3 , idomain )
   END IF
   tcs_boundary_table( idomain, P_tcs3 ) = .FALSE.
   tcs_dname_table( idomain, P_tcs3 ) = 'tcs3'
   tcs_desc_table( idomain, P_tcs3 ) = 'tcs'
   tcs_units_table( idomain, P_tcs3 ) = 'units'
   tcs_streams_table( idomain, P_tcs3 )%stream(1) = 0 ! 00000000 
   tcs_streams_table( idomain, P_tcs3 )%stream(2) = 2097152 ! 00200000 
   F_tcs3 = .TRUE.
   IF ( tcs_index_table( PARAM_tcs4 , idomain ) .lt. 1 ) THEN
     tcs_num_table(idomain) = tcs_num_table(idomain) + 1
     P_tcs4 = tcs_num_table(idomain)
     tcs_index_table( PARAM_tcs4 , idomain ) = P_tcs4
   ELSE
     P_tcs4 = tcs_index_table( PARAM_tcs4 , idomain )
   END IF
   tcs_boundary_table( idomain, P_tcs4 ) = .FALSE.
   tcs_dname_table( idomain, P_tcs4 ) = 'tcs4'
   tcs_desc_table( idomain, P_tcs4 ) = 'tcs'
   tcs_units_table( idomain, P_tcs4 ) = 'units'
   tcs_streams_table( idomain, P_tcs4 )%stream(1) = 0 ! 00000000 
   tcs_streams_table( idomain, P_tcs4 )%stream(2) = 2097152 ! 00200000 
   F_tcs4 = .TRUE.
  END IF
  IF (model_config_rec%swath_mode==1)THEN
  END IF
  IF (model_config_rec%vortex_tracker(idomain)==1)THEN
  END IF
  IF (model_config_rec%vortex_tracker(idomain)==2)THEN
  END IF
  IF (model_config_rec%vortex_tracker(idomain)==3)THEN
  END IF
  IF (model_config_rec%vortex_tracker(idomain)==4)THEN
  END IF
  IF (model_config_rec%vortex_tracker(idomain)==5)THEN
  END IF
  IF (model_config_rec%vortex_tracker(idomain)==6)THEN
  END IF
  IF (model_config_rec%vortex_tracker(idomain)==7)THEN
  END IF
  IF (model_config_rec%tg_option==0)THEN
  END IF
  IF (model_config_rec%tg_option==1)THEN
  END IF
  IF (model_config_rec%dyn_opt==5)THEN
  END IF
  IF (model_config_rec%chem_opt(idomain)==0)THEN
  END IF
  IF (model_config_rec%mp_physics(idomain)==0)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==1)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==2)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 ! 00200000 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 ! 00200000 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 ! 00200000 
   F_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==3)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==4)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 ! 00200000 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 ! 00200000 
   F_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==5)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 ! 00200000 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 ! 00200000 
   F_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==15)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 ! 00200000 
   F_qi = .TRUE.
   IF ( scalar_index_table( PARAM_qrimef , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qrimef = scalar_num_table(idomain)
     scalar_index_table( PARAM_qrimef , idomain ) = P_qrimef
   ELSE
     P_qrimef = scalar_index_table( PARAM_qrimef , idomain )
   END IF
   scalar_boundary_table( idomain, P_qrimef ) = .TRUE.
   scalar_dname_table( idomain, P_qrimef ) = 'QRIMEF'
   scalar_desc_table( idomain, P_qrimef ) = 'rime factor * qi'
   scalar_units_table( idomain, P_qrimef ) = 'kg kg-1'
   scalar_streams_table( idomain, P_qrimef )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qrimef )%stream(2) = 2097152 ! 00200000 
   F_qrimef = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==6)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 ! 00200000 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 ! 00200000 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 ! 00200000 
   F_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==7)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 ! 00200000 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 ! 00200000 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 ! 00200000 
   F_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==8)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 ! 00200000 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 ! 00200000 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 ! 00200000 
   F_qg = .TRUE.
   IF ( scalar_index_table( PARAM_qni , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qni = scalar_num_table(idomain)
     scalar_index_table( PARAM_qni , idomain ) = P_qni
   ELSE
     P_qni = scalar_index_table( PARAM_qni , idomain )
   END IF
   scalar_boundary_table( idomain, P_qni ) = .TRUE.
   scalar_dname_table( idomain, P_qni ) = 'QNICE'
   scalar_desc_table( idomain, P_qni ) = 'Ice Number concentration'
   scalar_units_table( idomain, P_qni ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qni )%stream(1) = 100663309 ! 0600000d 
   scalar_streams_table( idomain, P_qni )%stream(2) = 2097152 ! 00200000 
   F_qni = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnr ) = .TRUE.
   scalar_dname_table( idomain, P_qnr ) = 'QNRAIN'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnr )%stream(1) = 100663309 ! 0600000d 
   scalar_streams_table( idomain, P_qnr )%stream(2) = 2097152 ! 00200000 
   F_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==28)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 ! 00200000 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 ! 00200000 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 ! 00200000 
   F_qg = .TRUE.
   IF ( scalar_index_table( PARAM_qni , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qni = scalar_num_table(idomain)
     scalar_index_table( PARAM_qni , idomain ) = P_qni
   ELSE
     P_qni = scalar_index_table( PARAM_qni , idomain )
   END IF
   scalar_boundary_table( idomain, P_qni ) = .TRUE.
   scalar_dname_table( idomain, P_qni ) = 'QNICE'
   scalar_desc_table( idomain, P_qni ) = 'Ice Number concentration'
   scalar_units_table( idomain, P_qni ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qni )%stream(1) = 100663309 ! 0600000d 
   scalar_streams_table( idomain, P_qni )%stream(2) = 2097152 ! 00200000 
   F_qni = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnr ) = .TRUE.
   scalar_dname_table( idomain, P_qnr ) = 'QNRAIN'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnr )%stream(1) = 100663309 ! 0600000d 
   scalar_streams_table( idomain, P_qnr )%stream(2) = 2097152 ! 00200000 
   F_qnr = .TRUE.
   IF ( scalar_index_table( PARAM_qnc , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnc = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnc , idomain ) = P_qnc
   ELSE
     P_qnc = scalar_index_table( PARAM_qnc , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnc ) = .TRUE.
   scalar_dname_table( idomain, P_qnc ) = 'QNCLOUD'
   scalar_desc_table( idomain, P_qnc ) = 'cloud water Number concentration'
   scalar_units_table( idomain, P_qnc ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnc )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qnc )%stream(2) = 2097152 ! 00200000 
   F_qnc = .TRUE.
   IF ( scalar_index_table( PARAM_qnwfa , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnwfa = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnwfa , idomain ) = P_qnwfa
   ELSE
     P_qnwfa = scalar_index_table( PARAM_qnwfa , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnwfa ) = .TRUE.
   scalar_dname_table( idomain, P_qnwfa ) = 'QNWFA'
   scalar_desc_table( idomain, P_qnwfa ) = 'water-friendly aerosol number con'
   scalar_units_table( idomain, P_qnwfa ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnwfa )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qnwfa )%stream(2) = 2097152 ! 00200000 
   F_qnwfa = .TRUE.
   IF ( scalar_index_table( PARAM_qnifa , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnifa = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnifa , idomain ) = P_qnifa
   ELSE
     P_qnifa = scalar_index_table( PARAM_qnifa , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnifa ) = .TRUE.
   scalar_dname_table( idomain, P_qnifa ) = 'QNIFA'
   scalar_desc_table( idomain, P_qnifa ) = 'ice-friendly aerosol number con'
   scalar_units_table( idomain, P_qnifa ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnifa )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qnifa )%stream(2) = 2097152 ! 00200000 
   F_qnifa = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==9)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 ! 00200000 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 ! 00200000 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 ! 00200000 
   F_qg = .TRUE.
   IF ( moist_index_table( PARAM_qh , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qh = moist_num_table(idomain)
     moist_index_table( PARAM_qh , idomain ) = P_qh
   ELSE
     P_qh = moist_index_table( PARAM_qh , idomain )
   END IF
   moist_boundary_table( idomain, P_qh ) = .TRUE.
   moist_dname_table( idomain, P_qh ) = 'QHAIL'
   moist_desc_table( idomain, P_qh ) = 'Hail mixing ratio'
   moist_units_table( idomain, P_qh ) = 'kg kg-1'
   moist_streams_table( idomain, P_qh )%stream(1) = 1 ! 00000001 
   moist_streams_table( idomain, P_qh )%stream(2) = 2097152 ! 00200000 
   F_qh = .TRUE.
   IF ( scalar_index_table( PARAM_qnc , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnc = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnc , idomain ) = P_qnc
   ELSE
     P_qnc = scalar_index_table( PARAM_qnc , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnc ) = .TRUE.
   scalar_dname_table( idomain, P_qnc ) = 'QNCLOUD'
   scalar_desc_table( idomain, P_qnc ) = 'cloud water Number concentration'
   scalar_units_table( idomain, P_qnc ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnc )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qnc )%stream(2) = 2097152 ! 00200000 
   F_qnc = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnr ) = .TRUE.
   scalar_dname_table( idomain, P_qnr ) = 'QNRAIN'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnr )%stream(1) = 100663309 ! 0600000d 
   scalar_streams_table( idomain, P_qnr )%stream(2) = 2097152 ! 00200000 
   F_qnr = .TRUE.
   IF ( scalar_index_table( PARAM_qni , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qni = scalar_num_table(idomain)
     scalar_index_table( PARAM_qni , idomain ) = P_qni
   ELSE
     P_qni = scalar_index_table( PARAM_qni , idomain )
   END IF
   scalar_boundary_table( idomain, P_qni ) = .TRUE.
   scalar_dname_table( idomain, P_qni ) = 'QNICE'
   scalar_desc_table( idomain, P_qni ) = 'Ice Number concentration'
   scalar_units_table( idomain, P_qni ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qni )%stream(1) = 100663309 ! 0600000d 
   scalar_streams_table( idomain, P_qni )%stream(2) = 2097152 ! 00200000 
   F_qni = .TRUE.
   IF ( scalar_index_table( PARAM_qns , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qns = scalar_num_table(idomain)
     scalar_index_table( PARAM_qns , idomain ) = P_qns
   ELSE
     P_qns = scalar_index_table( PARAM_qns , idomain )
   END IF
   scalar_boundary_table( idomain, P_qns ) = .TRUE.
   scalar_dname_table( idomain, P_qns ) = 'QNSNOW'
   scalar_desc_table( idomain, P_qns ) = 'Snow Number concentration'
   scalar_units_table( idomain, P_qns ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qns )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qns )%stream(2) = 2097152 ! 00200000 
   F_qns = .TRUE.
   IF ( scalar_index_table( PARAM_qng , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qng = scalar_num_table(idomain)
     scalar_index_table( PARAM_qng , idomain ) = P_qng
   ELSE
     P_qng = scalar_index_table( PARAM_qng , idomain )
   END IF
   scalar_boundary_table( idomain, P_qng ) = .TRUE.
   scalar_dname_table( idomain, P_qng ) = 'QNGRAUP'
   scalar_desc_table( idomain, P_qng ) = 'Graupel Number concentration'
   scalar_units_table( idomain, P_qng ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qng )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qng )%stream(2) = 2097152 ! 00200000 
   F_qng = .TRUE.
   IF ( scalar_index_table( PARAM_qnh , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnh = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnh , idomain ) = P_qnh
   ELSE
     P_qnh = scalar_index_table( PARAM_qnh , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnh ) = .TRUE.
   scalar_dname_table( idomain, P_qnh ) = 'QNHAIL'
   scalar_desc_table( idomain, P_qnh ) = 'Hail Number concentration'
   scalar_units_table( idomain, P_qnh ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnh )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qnh )%stream(2) = 2097152 ! 00200000 
   F_qnh = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==10)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 ! 00200000 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 ! 00200000 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 ! 00200000 
   F_qg = .TRUE.
   IF ( scalar_index_table( PARAM_qni , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qni = scalar_num_table(idomain)
     scalar_index_table( PARAM_qni , idomain ) = P_qni
   ELSE
     P_qni = scalar_index_table( PARAM_qni , idomain )
   END IF
   scalar_boundary_table( idomain, P_qni ) = .TRUE.
   scalar_dname_table( idomain, P_qni ) = 'QNICE'
   scalar_desc_table( idomain, P_qni ) = 'Ice Number concentration'
   scalar_units_table( idomain, P_qni ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qni )%stream(1) = 100663309 ! 0600000d 
   scalar_streams_table( idomain, P_qni )%stream(2) = 2097152 ! 00200000 
   F_qni = .TRUE.
   IF ( scalar_index_table( PARAM_qns , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qns = scalar_num_table(idomain)
     scalar_index_table( PARAM_qns , idomain ) = P_qns
   ELSE
     P_qns = scalar_index_table( PARAM_qns , idomain )
   END IF
   scalar_boundary_table( idomain, P_qns ) = .TRUE.
   scalar_dname_table( idomain, P_qns ) = 'QNSNOW'
   scalar_desc_table( idomain, P_qns ) = 'Snow Number concentration'
   scalar_units_table( idomain, P_qns ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qns )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qns )%stream(2) = 2097152 ! 00200000 
   F_qns = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnr ) = .TRUE.
   scalar_dname_table( idomain, P_qnr ) = 'QNRAIN'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnr )%stream(1) = 100663309 ! 0600000d 
   scalar_streams_table( idomain, P_qnr )%stream(2) = 2097152 ! 00200000 
   F_qnr = .TRUE.
   IF ( scalar_index_table( PARAM_qng , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qng = scalar_num_table(idomain)
     scalar_index_table( PARAM_qng , idomain ) = P_qng
   ELSE
     P_qng = scalar_index_table( PARAM_qng , idomain )
   END IF
   scalar_boundary_table( idomain, P_qng ) = .TRUE.
   scalar_dname_table( idomain, P_qng ) = 'QNGRAUP'
   scalar_desc_table( idomain, P_qng ) = 'Graupel Number concentration'
   scalar_units_table( idomain, P_qng ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qng )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qng )%stream(2) = 2097152 ! 00200000 
   F_qng = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==13)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 ! 00200000 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 ! 00200000 
   F_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==14)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 ! 00200000 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 ! 00200000 
   F_qs = .TRUE.
   IF ( scalar_index_table( PARAM_qnn , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnn = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnn , idomain ) = P_qnn
   ELSE
     P_qnn = scalar_index_table( PARAM_qnn , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnn ) = .TRUE.
   scalar_dname_table( idomain, P_qnn ) = 'QNCCN'
   scalar_desc_table( idomain, P_qnn ) = 'CCN Number concentration'
   scalar_units_table( idomain, P_qnn ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnn )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qnn )%stream(2) = 2097152 ! 00200000 
   F_qnn = .TRUE.
   IF ( scalar_index_table( PARAM_qnc , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnc = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnc , idomain ) = P_qnc
   ELSE
     P_qnc = scalar_index_table( PARAM_qnc , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnc ) = .TRUE.
   scalar_dname_table( idomain, P_qnc ) = 'QNCLOUD'
   scalar_desc_table( idomain, P_qnc ) = 'cloud water Number concentration'
   scalar_units_table( idomain, P_qnc ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnc )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qnc )%stream(2) = 2097152 ! 00200000 
   F_qnc = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnr ) = .TRUE.
   scalar_dname_table( idomain, P_qnr ) = 'QNRAIN'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnr )%stream(1) = 100663309 ! 0600000d 
   scalar_streams_table( idomain, P_qnr )%stream(2) = 2097152 ! 00200000 
   F_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==16)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 ! 00200000 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 ! 00200000 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 ! 00200000 
   F_qg = .TRUE.
   IF ( scalar_index_table( PARAM_qnn , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnn = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnn , idomain ) = P_qnn
   ELSE
     P_qnn = scalar_index_table( PARAM_qnn , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnn ) = .TRUE.
   scalar_dname_table( idomain, P_qnn ) = 'QNCCN'
   scalar_desc_table( idomain, P_qnn ) = 'CCN Number concentration'
   scalar_units_table( idomain, P_qnn ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnn )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qnn )%stream(2) = 2097152 ! 00200000 
   F_qnn = .TRUE.
   IF ( scalar_index_table( PARAM_qnc , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnc = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnc , idomain ) = P_qnc
   ELSE
     P_qnc = scalar_index_table( PARAM_qnc , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnc ) = .TRUE.
   scalar_dname_table( idomain, P_qnc ) = 'QNCLOUD'
   scalar_desc_table( idomain, P_qnc ) = 'cloud water Number concentration'
   scalar_units_table( idomain, P_qnc ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnc )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qnc )%stream(2) = 2097152 ! 00200000 
   F_qnc = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnr ) = .TRUE.
   scalar_dname_table( idomain, P_qnr ) = 'QNRAIN'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnr )%stream(1) = 100663309 ! 0600000d 
   scalar_streams_table( idomain, P_qnr )%stream(2) = 2097152 ! 00200000 
   F_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==17)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 ! 00200000 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 ! 00200000 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 ! 00200000 
   F_qg = .TRUE.
   IF ( moist_index_table( PARAM_qh , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qh = moist_num_table(idomain)
     moist_index_table( PARAM_qh , idomain ) = P_qh
   ELSE
     P_qh = moist_index_table( PARAM_qh , idomain )
   END IF
   moist_boundary_table( idomain, P_qh ) = .TRUE.
   moist_dname_table( idomain, P_qh ) = 'QHAIL'
   moist_desc_table( idomain, P_qh ) = 'Hail mixing ratio'
   moist_units_table( idomain, P_qh ) = 'kg kg-1'
   moist_streams_table( idomain, P_qh )%stream(1) = 1 ! 00000001 
   moist_streams_table( idomain, P_qh )%stream(2) = 2097152 ! 00200000 
   F_qh = .TRUE.
   IF ( scalar_index_table( PARAM_qndrop , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qndrop = scalar_num_table(idomain)
     scalar_index_table( PARAM_qndrop , idomain ) = P_qndrop
   ELSE
     P_qndrop = scalar_index_table( PARAM_qndrop , idomain )
   END IF
   scalar_boundary_table( idomain, P_qndrop ) = .TRUE.
   scalar_dname_table( idomain, P_qndrop ) = 'QNDROP'
   scalar_desc_table( idomain, P_qndrop ) = 'Droplet number mixing ratio'
   scalar_units_table( idomain, P_qndrop ) = '  kg-1'
   scalar_streams_table( idomain, P_qndrop )%stream(1) = 100663309 ! 0600000d 
   scalar_streams_table( idomain, P_qndrop )%stream(2) = 2097152 ! 00200000 
   F_qndrop = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnr ) = .TRUE.
   scalar_dname_table( idomain, P_qnr ) = 'QNRAIN'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnr )%stream(1) = 100663309 ! 0600000d 
   scalar_streams_table( idomain, P_qnr )%stream(2) = 2097152 ! 00200000 
   F_qnr = .TRUE.
   IF ( scalar_index_table( PARAM_qni , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qni = scalar_num_table(idomain)
     scalar_index_table( PARAM_qni , idomain ) = P_qni
   ELSE
     P_qni = scalar_index_table( PARAM_qni , idomain )
   END IF
   scalar_boundary_table( idomain, P_qni ) = .TRUE.
   scalar_dname_table( idomain, P_qni ) = 'QNICE'
   scalar_desc_table( idomain, P_qni ) = 'Ice Number concentration'
   scalar_units_table( idomain, P_qni ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qni )%stream(1) = 100663309 ! 0600000d 
   scalar_streams_table( idomain, P_qni )%stream(2) = 2097152 ! 00200000 
   F_qni = .TRUE.
   IF ( scalar_index_table( PARAM_qns , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qns = scalar_num_table(idomain)
     scalar_index_table( PARAM_qns , idomain ) = P_qns
   ELSE
     P_qns = scalar_index_table( PARAM_qns , idomain )
   END IF
   scalar_boundary_table( idomain, P_qns ) = .TRUE.
   scalar_dname_table( idomain, P_qns ) = 'QNSNOW'
   scalar_desc_table( idomain, P_qns ) = 'Snow Number concentration'
   scalar_units_table( idomain, P_qns ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qns )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qns )%stream(2) = 2097152 ! 00200000 
   F_qns = .TRUE.
   IF ( scalar_index_table( PARAM_qng , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qng = scalar_num_table(idomain)
     scalar_index_table( PARAM_qng , idomain ) = P_qng
   ELSE
     P_qng = scalar_index_table( PARAM_qng , idomain )
   END IF
   scalar_boundary_table( idomain, P_qng ) = .TRUE.
   scalar_dname_table( idomain, P_qng ) = 'QNGRAUP'
   scalar_desc_table( idomain, P_qng ) = 'Graupel Number concentration'
   scalar_units_table( idomain, P_qng ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qng )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qng )%stream(2) = 2097152 ! 00200000 
   F_qng = .TRUE.
   IF ( scalar_index_table( PARAM_qnh , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnh = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnh , idomain ) = P_qnh
   ELSE
     P_qnh = scalar_index_table( PARAM_qnh , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnh ) = .TRUE.
   scalar_dname_table( idomain, P_qnh ) = 'QNHAIL'
   scalar_desc_table( idomain, P_qnh ) = 'Hail Number concentration'
   scalar_units_table( idomain, P_qnh ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnh )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qnh )%stream(2) = 2097152 ! 00200000 
   F_qnh = .TRUE.
   IF ( scalar_index_table( PARAM_qvolg , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qvolg = scalar_num_table(idomain)
     scalar_index_table( PARAM_qvolg , idomain ) = P_qvolg
   ELSE
     P_qvolg = scalar_index_table( PARAM_qvolg , idomain )
   END IF
   scalar_boundary_table( idomain, P_qvolg ) = .TRUE.
   scalar_dname_table( idomain, P_qvolg ) = 'QVGRAUPEL'
   scalar_desc_table( idomain, P_qvolg ) = 'Graupel Particle Volume'
   scalar_units_table( idomain, P_qvolg ) = 'm(3) kg(-1)'
   scalar_streams_table( idomain, P_qvolg )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qvolg )%stream(2) = 2097152 ! 00200000 
   F_qvolg = .TRUE.
   IF ( scalar_index_table( PARAM_qvolh , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qvolh = scalar_num_table(idomain)
     scalar_index_table( PARAM_qvolh , idomain ) = P_qvolh
   ELSE
     P_qvolh = scalar_index_table( PARAM_qvolh , idomain )
   END IF
   scalar_boundary_table( idomain, P_qvolh ) = .TRUE.
   scalar_dname_table( idomain, P_qvolh ) = '0)'
   scalar_desc_table( idomain, P_qvolh ) = 'QVHAIL'
   scalar_units_table( idomain, P_qvolh ) = 'Hail Particle Volume'
   scalar_streams_table( idomain, P_qvolh )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qvolh )%stream(2) = 2097152 ! 00200000 
   F_qvolh = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==18)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 ! 00200000 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 ! 00200000 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 ! 00200000 
   F_qg = .TRUE.
   IF ( moist_index_table( PARAM_qh , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qh = moist_num_table(idomain)
     moist_index_table( PARAM_qh , idomain ) = P_qh
   ELSE
     P_qh = moist_index_table( PARAM_qh , idomain )
   END IF
   moist_boundary_table( idomain, P_qh ) = .TRUE.
   moist_dname_table( idomain, P_qh ) = 'QHAIL'
   moist_desc_table( idomain, P_qh ) = 'Hail mixing ratio'
   moist_units_table( idomain, P_qh ) = 'kg kg-1'
   moist_streams_table( idomain, P_qh )%stream(1) = 1 ! 00000001 
   moist_streams_table( idomain, P_qh )%stream(2) = 2097152 ! 00200000 
   F_qh = .TRUE.
   IF ( scalar_index_table( PARAM_qnn , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnn = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnn , idomain ) = P_qnn
   ELSE
     P_qnn = scalar_index_table( PARAM_qnn , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnn ) = .TRUE.
   scalar_dname_table( idomain, P_qnn ) = 'QNCCN'
   scalar_desc_table( idomain, P_qnn ) = 'CCN Number concentration'
   scalar_units_table( idomain, P_qnn ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnn )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qnn )%stream(2) = 2097152 ! 00200000 
   F_qnn = .TRUE.
   IF ( scalar_index_table( PARAM_qndrop , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qndrop = scalar_num_table(idomain)
     scalar_index_table( PARAM_qndrop , idomain ) = P_qndrop
   ELSE
     P_qndrop = scalar_index_table( PARAM_qndrop , idomain )
   END IF
   scalar_boundary_table( idomain, P_qndrop ) = .TRUE.
   scalar_dname_table( idomain, P_qndrop ) = 'QNDROP'
   scalar_desc_table( idomain, P_qndrop ) = 'Droplet number mixing ratio'
   scalar_units_table( idomain, P_qndrop ) = '  kg-1'
   scalar_streams_table( idomain, P_qndrop )%stream(1) = 100663309 ! 0600000d 
   scalar_streams_table( idomain, P_qndrop )%stream(2) = 2097152 ! 00200000 
   F_qndrop = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnr ) = .TRUE.
   scalar_dname_table( idomain, P_qnr ) = 'QNRAIN'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnr )%stream(1) = 100663309 ! 0600000d 
   scalar_streams_table( idomain, P_qnr )%stream(2) = 2097152 ! 00200000 
   F_qnr = .TRUE.
   IF ( scalar_index_table( PARAM_qni , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qni = scalar_num_table(idomain)
     scalar_index_table( PARAM_qni , idomain ) = P_qni
   ELSE
     P_qni = scalar_index_table( PARAM_qni , idomain )
   END IF
   scalar_boundary_table( idomain, P_qni ) = .TRUE.
   scalar_dname_table( idomain, P_qni ) = 'QNICE'
   scalar_desc_table( idomain, P_qni ) = 'Ice Number concentration'
   scalar_units_table( idomain, P_qni ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qni )%stream(1) = 100663309 ! 0600000d 
   scalar_streams_table( idomain, P_qni )%stream(2) = 2097152 ! 00200000 
   F_qni = .TRUE.
   IF ( scalar_index_table( PARAM_qns , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qns = scalar_num_table(idomain)
     scalar_index_table( PARAM_qns , idomain ) = P_qns
   ELSE
     P_qns = scalar_index_table( PARAM_qns , idomain )
   END IF
   scalar_boundary_table( idomain, P_qns ) = .TRUE.
   scalar_dname_table( idomain, P_qns ) = 'QNSNOW'
   scalar_desc_table( idomain, P_qns ) = 'Snow Number concentration'
   scalar_units_table( idomain, P_qns ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qns )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qns )%stream(2) = 2097152 ! 00200000 
   F_qns = .TRUE.
   IF ( scalar_index_table( PARAM_qng , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qng = scalar_num_table(idomain)
     scalar_index_table( PARAM_qng , idomain ) = P_qng
   ELSE
     P_qng = scalar_index_table( PARAM_qng , idomain )
   END IF
   scalar_boundary_table( idomain, P_qng ) = .TRUE.
   scalar_dname_table( idomain, P_qng ) = 'QNGRAUP'
   scalar_desc_table( idomain, P_qng ) = 'Graupel Number concentration'
   scalar_units_table( idomain, P_qng ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qng )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qng )%stream(2) = 2097152 ! 00200000 
   F_qng = .TRUE.
   IF ( scalar_index_table( PARAM_qnh , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnh = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnh , idomain ) = P_qnh
   ELSE
     P_qnh = scalar_index_table( PARAM_qnh , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnh ) = .TRUE.
   scalar_dname_table( idomain, P_qnh ) = 'QNHAIL'
   scalar_desc_table( idomain, P_qnh ) = 'Hail Number concentration'
   scalar_units_table( idomain, P_qnh ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnh )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qnh )%stream(2) = 2097152 ! 00200000 
   F_qnh = .TRUE.
   IF ( scalar_index_table( PARAM_qvolg , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qvolg = scalar_num_table(idomain)
     scalar_index_table( PARAM_qvolg , idomain ) = P_qvolg
   ELSE
     P_qvolg = scalar_index_table( PARAM_qvolg , idomain )
   END IF
   scalar_boundary_table( idomain, P_qvolg ) = .TRUE.
   scalar_dname_table( idomain, P_qvolg ) = 'QVGRAUPEL'
   scalar_desc_table( idomain, P_qvolg ) = 'Graupel Particle Volume'
   scalar_units_table( idomain, P_qvolg ) = 'm(3) kg(-1)'
   scalar_streams_table( idomain, P_qvolg )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qvolg )%stream(2) = 2097152 ! 00200000 
   F_qvolg = .TRUE.
   IF ( scalar_index_table( PARAM_qvolh , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qvolh = scalar_num_table(idomain)
     scalar_index_table( PARAM_qvolh , idomain ) = P_qvolh
   ELSE
     P_qvolh = scalar_index_table( PARAM_qvolh , idomain )
   END IF
   scalar_boundary_table( idomain, P_qvolh ) = .TRUE.
   scalar_dname_table( idomain, P_qvolh ) = '0)'
   scalar_desc_table( idomain, P_qvolh ) = 'QVHAIL'
   scalar_units_table( idomain, P_qvolh ) = 'Hail Particle Volume'
   scalar_streams_table( idomain, P_qvolh )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qvolh )%stream(2) = 2097152 ! 00200000 
   F_qvolh = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==19)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 ! 00200000 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 ! 00200000 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 ! 00200000 
   F_qg = .TRUE.
   IF ( moist_index_table( PARAM_qh , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qh = moist_num_table(idomain)
     moist_index_table( PARAM_qh , idomain ) = P_qh
   ELSE
     P_qh = moist_index_table( PARAM_qh , idomain )
   END IF
   moist_boundary_table( idomain, P_qh ) = .TRUE.
   moist_dname_table( idomain, P_qh ) = 'QHAIL'
   moist_desc_table( idomain, P_qh ) = 'Hail mixing ratio'
   moist_units_table( idomain, P_qh ) = 'kg kg-1'
   moist_streams_table( idomain, P_qh )%stream(1) = 1 ! 00000001 
   moist_streams_table( idomain, P_qh )%stream(2) = 2097152 ! 00200000 
   F_qh = .TRUE.
   IF ( scalar_index_table( PARAM_qvolg , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qvolg = scalar_num_table(idomain)
     scalar_index_table( PARAM_qvolg , idomain ) = P_qvolg
   ELSE
     P_qvolg = scalar_index_table( PARAM_qvolg , idomain )
   END IF
   scalar_boundary_table( idomain, P_qvolg ) = .TRUE.
   scalar_dname_table( idomain, P_qvolg ) = 'QVGRAUPEL'
   scalar_desc_table( idomain, P_qvolg ) = 'Graupel Particle Volume'
   scalar_units_table( idomain, P_qvolg ) = 'm(3) kg(-1)'
   scalar_streams_table( idomain, P_qvolg )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qvolg )%stream(2) = 2097152 ! 00200000 
   F_qvolg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==21)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 ! 00200000 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 ! 00200000 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 ! 00200000 
   F_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==22)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 ! 00200000 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 ! 00200000 
   F_qs = .TRUE.
   IF ( moist_index_table( PARAM_qg , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qg = moist_num_table(idomain)
     moist_index_table( PARAM_qg , idomain ) = P_qg
   ELSE
     P_qg = moist_index_table( PARAM_qg , idomain )
   END IF
   moist_boundary_table( idomain, P_qg ) = .TRUE.
   moist_dname_table( idomain, P_qg ) = 'QGRAUP'
   moist_desc_table( idomain, P_qg ) = 'Graupel mixing ratio'
   moist_units_table( idomain, P_qg ) = 'kg kg-1'
   moist_streams_table( idomain, P_qg )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qg )%stream(2) = 2097152 ! 00200000 
   F_qg = .TRUE.
   IF ( scalar_index_table( PARAM_qndrop , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qndrop = scalar_num_table(idomain)
     scalar_index_table( PARAM_qndrop , idomain ) = P_qndrop
   ELSE
     P_qndrop = scalar_index_table( PARAM_qndrop , idomain )
   END IF
   scalar_boundary_table( idomain, P_qndrop ) = .TRUE.
   scalar_dname_table( idomain, P_qndrop ) = 'QNDROP'
   scalar_desc_table( idomain, P_qndrop ) = 'Droplet number mixing ratio'
   scalar_units_table( idomain, P_qndrop ) = '  kg-1'
   scalar_streams_table( idomain, P_qndrop )%stream(1) = 100663309 ! 0600000d 
   scalar_streams_table( idomain, P_qndrop )%stream(2) = 2097152 ! 00200000 
   F_qndrop = .TRUE.
   IF ( scalar_index_table( PARAM_qnr , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qnr = scalar_num_table(idomain)
     scalar_index_table( PARAM_qnr , idomain ) = P_qnr
   ELSE
     P_qnr = scalar_index_table( PARAM_qnr , idomain )
   END IF
   scalar_boundary_table( idomain, P_qnr ) = .TRUE.
   scalar_dname_table( idomain, P_qnr ) = 'QNRAIN'
   scalar_desc_table( idomain, P_qnr ) = 'Rain Number concentration'
   scalar_units_table( idomain, P_qnr ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qnr )%stream(1) = 100663309 ! 0600000d 
   scalar_streams_table( idomain, P_qnr )%stream(2) = 2097152 ! 00200000 
   F_qnr = .TRUE.
   IF ( scalar_index_table( PARAM_qni , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qni = scalar_num_table(idomain)
     scalar_index_table( PARAM_qni , idomain ) = P_qni
   ELSE
     P_qni = scalar_index_table( PARAM_qni , idomain )
   END IF
   scalar_boundary_table( idomain, P_qni ) = .TRUE.
   scalar_dname_table( idomain, P_qni ) = 'QNICE'
   scalar_desc_table( idomain, P_qni ) = 'Ice Number concentration'
   scalar_units_table( idomain, P_qni ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qni )%stream(1) = 100663309 ! 0600000d 
   scalar_streams_table( idomain, P_qni )%stream(2) = 2097152 ! 00200000 
   F_qni = .TRUE.
   IF ( scalar_index_table( PARAM_qns , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qns = scalar_num_table(idomain)
     scalar_index_table( PARAM_qns , idomain ) = P_qns
   ELSE
     P_qns = scalar_index_table( PARAM_qns , idomain )
   END IF
   scalar_boundary_table( idomain, P_qns ) = .TRUE.
   scalar_dname_table( idomain, P_qns ) = 'QNSNOW'
   scalar_desc_table( idomain, P_qns ) = 'Snow Number concentration'
   scalar_units_table( idomain, P_qns ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qns )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qns )%stream(2) = 2097152 ! 00200000 
   F_qns = .TRUE.
   IF ( scalar_index_table( PARAM_qng , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qng = scalar_num_table(idomain)
     scalar_index_table( PARAM_qng , idomain ) = P_qng
   ELSE
     P_qng = scalar_index_table( PARAM_qng , idomain )
   END IF
   scalar_boundary_table( idomain, P_qng ) = .TRUE.
   scalar_dname_table( idomain, P_qng ) = 'QNGRAUP'
   scalar_desc_table( idomain, P_qng ) = 'Graupel Number concentration'
   scalar_units_table( idomain, P_qng ) = '  kg(-1)'
   scalar_streams_table( idomain, P_qng )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qng )%stream(2) = 2097152 ! 00200000 
   F_qng = .TRUE.
   IF ( scalar_index_table( PARAM_qvolg , idomain ) .lt. 1 ) THEN
     scalar_num_table(idomain) = scalar_num_table(idomain) + 1
     P_qvolg = scalar_num_table(idomain)
     scalar_index_table( PARAM_qvolg , idomain ) = P_qvolg
   ELSE
     P_qvolg = scalar_index_table( PARAM_qvolg , idomain )
   END IF
   scalar_boundary_table( idomain, P_qvolg ) = .TRUE.
   scalar_dname_table( idomain, P_qvolg ) = 'QVGRAUPEL'
   scalar_desc_table( idomain, P_qvolg ) = 'Graupel Particle Volume'
   scalar_units_table( idomain, P_qvolg ) = 'm(3) kg(-1)'
   scalar_streams_table( idomain, P_qvolg )%stream(1) = 100663297 ! 06000001 
   scalar_streams_table( idomain, P_qvolg )%stream(2) = 2097152 ! 00200000 
   F_qvolg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==85)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qi , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qi = moist_num_table(idomain)
     moist_index_table( PARAM_qi , idomain ) = P_qi
   ELSE
     P_qi = moist_index_table( PARAM_qi , idomain )
   END IF
   moist_boundary_table( idomain, P_qi ) = .TRUE.
   moist_dname_table( idomain, P_qi ) = 'QICE'
   moist_desc_table( idomain, P_qi ) = 'Ice mixing ratio'
   moist_units_table( idomain, P_qi ) = 'kg kg-1'
   moist_streams_table( idomain, P_qi )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qi )%stream(2) = 2097152 ! 00200000 
   F_qi = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 ! 00200000 
   F_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==95)THEN
   IF ( moist_index_table( PARAM_qv , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qv = moist_num_table(idomain)
     moist_index_table( PARAM_qv , idomain ) = P_qv
   ELSE
     P_qv = moist_index_table( PARAM_qv , idomain )
   END IF
   moist_boundary_table( idomain, P_qv ) = .TRUE.
   moist_dname_table( idomain, P_qv ) = 'QVAPOR'
   moist_desc_table( idomain, P_qv ) = 'Water vapor mixing ratio'
   moist_units_table( idomain, P_qv ) = 'kg kg-1'
   moist_streams_table( idomain, P_qv )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qv )%stream(2) = 2097152 ! 00200000 
   F_qv = .TRUE.
   IF ( moist_index_table( PARAM_qc , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qc = moist_num_table(idomain)
     moist_index_table( PARAM_qc , idomain ) = P_qc
   ELSE
     P_qc = moist_index_table( PARAM_qc , idomain )
   END IF
   moist_boundary_table( idomain, P_qc ) = .TRUE.
   moist_dname_table( idomain, P_qc ) = 'QCLOUD'
   moist_desc_table( idomain, P_qc ) = 'Cloud water mixing ratio'
   moist_units_table( idomain, P_qc ) = 'kg kg-1'
   moist_streams_table( idomain, P_qc )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qc )%stream(2) = 2097152 ! 00200000 
   F_qc = .TRUE.
   IF ( moist_index_table( PARAM_qr , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qr = moist_num_table(idomain)
     moist_index_table( PARAM_qr , idomain ) = P_qr
   ELSE
     P_qr = moist_index_table( PARAM_qr , idomain )
   END IF
   moist_boundary_table( idomain, P_qr ) = .TRUE.
   moist_dname_table( idomain, P_qr ) = 'QRAIN'
   moist_desc_table( idomain, P_qr ) = 'Rain water mixing ratio'
   moist_units_table( idomain, P_qr ) = 'kg kg-1'
   moist_streams_table( idomain, P_qr )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qr )%stream(2) = 2097152 ! 00200000 
   F_qr = .TRUE.
   IF ( moist_index_table( PARAM_qs , idomain ) .lt. 1 ) THEN
     moist_num_table(idomain) = moist_num_table(idomain) + 1
     P_qs = moist_num_table(idomain)
     moist_index_table( PARAM_qs , idomain ) = P_qs
   ELSE
     P_qs = moist_index_table( PARAM_qs , idomain )
   END IF
   moist_boundary_table( idomain, P_qs ) = .TRUE.
   moist_dname_table( idomain, P_qs ) = 'QSNOW'
   moist_desc_table( idomain, P_qs ) = 'Snow mixing ratio'
   moist_units_table( idomain, P_qs ) = 'kg kg-1'
   moist_streams_table( idomain, P_qs )%stream(1) = 13 ! 0000000d 
   moist_streams_table( idomain, P_qs )%stream(2) = 2097152 ! 00200000 
   F_qs = .TRUE.
  END IF
  IF (model_config_rec%compute_radar_ref==1)THEN
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==-1)THEN
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==0)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 ! 00200000 
   F_dfi_qv = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==1)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 ! 00200000 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 ! 00200000 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==2)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 ! 00200000 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 ! 00200000 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 ! 00200000 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 ! 00200000 
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qg ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(2) = 2097152 ! 00200000 
   F_dfi_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==3)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 ! 00200000 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 ! 00200000 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==4)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 ! 00200000 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 ! 00200000 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 ! 00200000 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 ! 00200000 
   F_dfi_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==5)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 ! 00200000 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 ! 00200000 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 ! 00200000 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 ! 00200000 
   F_dfi_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==6)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 ! 00200000 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 ! 00200000 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 ! 00200000 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 ! 00200000 
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qg ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(2) = 2097152 ! 00200000 
   F_dfi_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==7)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 ! 00200000 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 ! 00200000 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 ! 00200000 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 ! 00200000 
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qg ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(2) = 2097152 ! 00200000 
   F_dfi_qg = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==8)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 ! 00200000 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 ! 00200000 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 ! 00200000 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 ! 00200000 
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qg ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(2) = 2097152 ! 00200000 
   F_dfi_qg = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qni , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qni = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qni , idomain ) = P_dfi_qni
   ELSE
     P_dfi_qni = dfi_scalar_index_table( PARAM_dfi_qni , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qni ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qni ) = 'DFI_QNICE'
   dfi_scalar_desc_table( idomain, P_dfi_qni ) = 'DFI Ice Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qni ) = '  kg-1'
   dfi_scalar_streams_table( idomain, P_dfi_qni )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qni )%stream(2) = 2097152 ! 00200000 
   F_dfi_qni = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnr = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) = P_dfi_qnr
   ELSE
     P_dfi_qnr = dfi_scalar_index_table( PARAM_dfi_qnr , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnr ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnr ) = 'DFI_QNRAIN'
   dfi_scalar_desc_table( idomain, P_dfi_qnr ) = 'DFI Rain Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnr ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==28)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 ! 00200000 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 ! 00200000 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 ! 00200000 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 ! 00200000 
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qg ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(2) = 2097152 ! 00200000 
   F_dfi_qg = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qni , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qni = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qni , idomain ) = P_dfi_qni
   ELSE
     P_dfi_qni = dfi_scalar_index_table( PARAM_dfi_qni , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qni ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qni ) = 'DFI_QNICE'
   dfi_scalar_desc_table( idomain, P_dfi_qni ) = 'DFI Ice Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qni ) = '  kg-1'
   dfi_scalar_streams_table( idomain, P_dfi_qni )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qni )%stream(2) = 2097152 ! 00200000 
   F_dfi_qni = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnr = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) = P_dfi_qnr
   ELSE
     P_dfi_qnr = dfi_scalar_index_table( PARAM_dfi_qnr , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnr ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnr ) = 'DFI_QNRAIN'
   dfi_scalar_desc_table( idomain, P_dfi_qnr ) = 'DFI Rain Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnr ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qnr = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnc = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) = P_dfi_qnc
   ELSE
     P_dfi_qnc = dfi_scalar_index_table( PARAM_dfi_qnc , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnc ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnc ) = 'DFI_QNCLOUD'
   dfi_scalar_desc_table( idomain, P_dfi_qnc ) = 'DFI Cloud Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnc ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnc )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qnc )%stream(2) = 2097152 ! 00200000 
   F_dfi_qnc = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnwfa , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnwfa = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnwfa , idomain ) = P_dfi_qnwfa
   ELSE
     P_dfi_qnwfa = dfi_scalar_index_table( PARAM_dfi_qnwfa , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnwfa ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnwfa ) = 'DFI_QNWFA'
   dfi_scalar_desc_table( idomain, P_dfi_qnwfa ) = 'DFI water-friendly aerosol number con'
   dfi_scalar_units_table( idomain, P_dfi_qnwfa ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnwfa )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qnwfa )%stream(2) = 2097152 ! 00200000 
   F_dfi_qnwfa = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnifa , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnifa = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnifa , idomain ) = P_dfi_qnifa
   ELSE
     P_dfi_qnifa = dfi_scalar_index_table( PARAM_dfi_qnifa , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnifa ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnifa ) = 'DFI_QNIFA'
   dfi_scalar_desc_table( idomain, P_dfi_qnifa ) = 'DFI ice-friendly aerosol number con'
   dfi_scalar_units_table( idomain, P_dfi_qnifa ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnifa )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qnifa )%stream(2) = 2097152 ! 00200000 
   F_dfi_qnifa = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==9)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 ! 00200000 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 ! 00200000 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 ! 00200000 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 ! 00200000 
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qg ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(2) = 2097152 ! 00200000 
   F_dfi_qg = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qh , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qh = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qh , idomain ) = P_dfi_qh
   ELSE
     P_dfi_qh = dfi_moist_index_table( PARAM_dfi_qh , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qh ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qh ) = 'QHAIL'
   dfi_moist_desc_table( idomain, P_dfi_qh ) = 'Hail mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qh ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qh )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qh )%stream(2) = 2097152 ! 00200000 
   F_dfi_qh = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnc = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) = P_dfi_qnc
   ELSE
     P_dfi_qnc = dfi_scalar_index_table( PARAM_dfi_qnc , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnc ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnc ) = 'DFI_QNCLOUD'
   dfi_scalar_desc_table( idomain, P_dfi_qnc ) = 'DFI Cloud Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnc ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnc )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qnc )%stream(2) = 2097152 ! 00200000 
   F_dfi_qnc = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnr = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) = P_dfi_qnr
   ELSE
     P_dfi_qnr = dfi_scalar_index_table( PARAM_dfi_qnr , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnr ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnr ) = 'DFI_QNRAIN'
   dfi_scalar_desc_table( idomain, P_dfi_qnr ) = 'DFI Rain Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnr ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qnr = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qni , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qni = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qni , idomain ) = P_dfi_qni
   ELSE
     P_dfi_qni = dfi_scalar_index_table( PARAM_dfi_qni , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qni ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qni ) = 'DFI_QNICE'
   dfi_scalar_desc_table( idomain, P_dfi_qni ) = 'DFI Ice Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qni ) = '  kg-1'
   dfi_scalar_streams_table( idomain, P_dfi_qni )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qni )%stream(2) = 2097152 ! 00200000 
   F_dfi_qni = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qns , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qns = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qns , idomain ) = P_dfi_qns
   ELSE
     P_dfi_qns = dfi_scalar_index_table( PARAM_dfi_qns , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qns ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qns ) = 'DFI_QNSNOW'
   dfi_scalar_desc_table( idomain, P_dfi_qns ) = 'DFI Snow Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qns ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qns )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qns )%stream(2) = 2097152 ! 00200000 
   F_dfi_qns = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qng , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qng = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qng , idomain ) = P_dfi_qng
   ELSE
     P_dfi_qng = dfi_scalar_index_table( PARAM_dfi_qng , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qng ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qng ) = 'DFI_QNGRAUPEL'
   dfi_scalar_desc_table( idomain, P_dfi_qng ) = 'DFI Graupel Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qng ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qng )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qng )%stream(2) = 2097152 ! 00200000 
   F_dfi_qng = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnh , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnh = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnh , idomain ) = P_dfi_qnh
   ELSE
     P_dfi_qnh = dfi_scalar_index_table( PARAM_dfi_qnh , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnh ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnh ) = 'QNHAIL'
   dfi_scalar_desc_table( idomain, P_dfi_qnh ) = 'Hail Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnh ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnh )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qnh )%stream(2) = 2097152 ! 00200000 
   F_dfi_qnh = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==10)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 ! 00200000 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 ! 00200000 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 ! 00200000 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 ! 00200000 
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qg ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(2) = 2097152 ! 00200000 
   F_dfi_qg = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qni , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qni = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qni , idomain ) = P_dfi_qni
   ELSE
     P_dfi_qni = dfi_scalar_index_table( PARAM_dfi_qni , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qni ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qni ) = 'DFI_QNICE'
   dfi_scalar_desc_table( idomain, P_dfi_qni ) = 'DFI Ice Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qni ) = '  kg-1'
   dfi_scalar_streams_table( idomain, P_dfi_qni )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qni )%stream(2) = 2097152 ! 00200000 
   F_dfi_qni = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qns , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qns = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qns , idomain ) = P_dfi_qns
   ELSE
     P_dfi_qns = dfi_scalar_index_table( PARAM_dfi_qns , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qns ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qns ) = 'DFI_QNSNOW'
   dfi_scalar_desc_table( idomain, P_dfi_qns ) = 'DFI Snow Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qns ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qns )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qns )%stream(2) = 2097152 ! 00200000 
   F_dfi_qns = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnr = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) = P_dfi_qnr
   ELSE
     P_dfi_qnr = dfi_scalar_index_table( PARAM_dfi_qnr , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnr ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnr ) = 'DFI_QNRAIN'
   dfi_scalar_desc_table( idomain, P_dfi_qnr ) = 'DFI Rain Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnr ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qnr = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qng , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qng = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qng , idomain ) = P_dfi_qng
   ELSE
     P_dfi_qng = dfi_scalar_index_table( PARAM_dfi_qng , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qng ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qng ) = 'DFI_QNGRAUPEL'
   dfi_scalar_desc_table( idomain, P_dfi_qng ) = 'DFI Graupel Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qng ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qng )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qng )%stream(2) = 2097152 ! 00200000 
   F_dfi_qng = .TRUE.
  END IF
  IF (model_config_rec%mp_physics(idomain)==13)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 ! 00200000 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 ! 00200000 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 ! 00200000 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 ! 00200000 
   F_dfi_qs = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==14)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 ! 00200000 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 ! 00200000 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 ! 00200000 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 ! 00200000 
   F_dfi_qs = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnn , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnn = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnn , idomain ) = P_dfi_qnn
   ELSE
     P_dfi_qnn = dfi_scalar_index_table( PARAM_dfi_qnn , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnn ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnn ) = 'DFI_QNCC'
   dfi_scalar_desc_table( idomain, P_dfi_qnn ) = 'DFI CNN Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnn ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnn )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qnn )%stream(2) = 2097152 ! 00200000 
   F_dfi_qnn = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnc = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) = P_dfi_qnc
   ELSE
     P_dfi_qnc = dfi_scalar_index_table( PARAM_dfi_qnc , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnc ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnc ) = 'DFI_QNCLOUD'
   dfi_scalar_desc_table( idomain, P_dfi_qnc ) = 'DFI Cloud Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnc ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnc )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qnc )%stream(2) = 2097152 ! 00200000 
   F_dfi_qnc = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnr = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) = P_dfi_qnr
   ELSE
     P_dfi_qnr = dfi_scalar_index_table( PARAM_dfi_qnr , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnr ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnr ) = 'DFI_QNRAIN'
   dfi_scalar_desc_table( idomain, P_dfi_qnr ) = 'DFI Rain Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnr ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==16)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 ! 00200000 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 ! 00200000 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qi , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qi = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qi , idomain ) = P_dfi_qi
   ELSE
     P_dfi_qi = dfi_moist_index_table( PARAM_dfi_qi , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qi ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qi ) = 'QICE'
   dfi_moist_desc_table( idomain, P_dfi_qi ) = 'Ice mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qi ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qi )%stream(2) = 2097152 ! 00200000 
   F_dfi_qi = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 ! 00200000 
   F_dfi_qs = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qg , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qg = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qg , idomain ) = P_dfi_qg
   ELSE
     P_dfi_qg = dfi_moist_index_table( PARAM_dfi_qg , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qg ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qg ) = 'QGRAUP'
   dfi_moist_desc_table( idomain, P_dfi_qg ) = 'Graupel mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qg ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qg )%stream(2) = 2097152 ! 00200000 
   F_dfi_qg = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnn , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnn = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnn , idomain ) = P_dfi_qnn
   ELSE
     P_dfi_qnn = dfi_scalar_index_table( PARAM_dfi_qnn , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnn ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnn ) = 'DFI_QNCC'
   dfi_scalar_desc_table( idomain, P_dfi_qnn ) = 'DFI CNN Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnn ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnn )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qnn )%stream(2) = 2097152 ! 00200000 
   F_dfi_qnn = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnc = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnc , idomain ) = P_dfi_qnc
   ELSE
     P_dfi_qnc = dfi_scalar_index_table( PARAM_dfi_qnc , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnc ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnc ) = 'DFI_QNCLOUD'
   dfi_scalar_desc_table( idomain, P_dfi_qnc ) = 'DFI Cloud Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnc ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnc )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qnc )%stream(2) = 2097152 ! 00200000 
   F_dfi_qnc = .TRUE.
   IF ( dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) .lt. 1 ) THEN
     dfi_scalar_num_table(idomain) = dfi_scalar_num_table(idomain) + 1
     P_dfi_qnr = dfi_scalar_num_table(idomain)
     dfi_scalar_index_table( PARAM_dfi_qnr , idomain ) = P_dfi_qnr
   ELSE
     P_dfi_qnr = dfi_scalar_index_table( PARAM_dfi_qnr , idomain )
   END IF
   dfi_scalar_boundary_table( idomain, P_dfi_qnr ) = .TRUE.
   dfi_scalar_dname_table( idomain, P_dfi_qnr ) = 'DFI_QNRAIN'
   dfi_scalar_desc_table( idomain, P_dfi_qnr ) = 'DFI Rain Number concentration'
   dfi_scalar_units_table( idomain, P_dfi_qnr ) = '  kg(-1)'
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(1) = 0 ! 00000000 
   dfi_scalar_streams_table( idomain, P_dfi_qnr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qnr = .TRUE.
  END IF
  IF (model_config_rec%mp_physics_dfi(idomain)==95)THEN
   IF ( dfi_moist_index_table( PARAM_dfi_qv , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qv = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qv , idomain ) = P_dfi_qv
   ELSE
     P_dfi_qv = dfi_moist_index_table( PARAM_dfi_qv , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qv ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qv ) = 'QVAPOR'
   dfi_moist_desc_table( idomain, P_dfi_qv ) = 'Water vapor mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qv ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qv )%stream(2) = 2097152 ! 00200000 
   F_dfi_qv = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qc , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qc = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qc , idomain ) = P_dfi_qc
   ELSE
     P_dfi_qc = dfi_moist_index_table( PARAM_dfi_qc , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qc ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qc ) = 'QCLOUD'
   dfi_moist_desc_table( idomain, P_dfi_qc ) = 'Cloud water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qc ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qc )%stream(2) = 2097152 ! 00200000 
   F_dfi_qc = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qr , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qr = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qr , idomain ) = P_dfi_qr
   ELSE
     P_dfi_qr = dfi_moist_index_table( PARAM_dfi_qr , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qr ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qr ) = 'QRAIN'
   dfi_moist_desc_table( idomain, P_dfi_qr ) = 'Rain water mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qr ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qr )%stream(2) = 2097152 ! 00200000 
   F_dfi_qr = .TRUE.
   IF ( dfi_moist_index_table( PARAM_dfi_qs , idomain ) .lt. 1 ) THEN
     dfi_moist_num_table(idomain) = dfi_moist_num_table(idomain) + 1
     P_dfi_qs = dfi_moist_num_table(idomain)
     dfi_moist_index_table( PARAM_dfi_qs , idomain ) = P_dfi_qs
   ELSE
     P_dfi_qs = dfi_moist_index_table( PARAM_dfi_qs , idomain )
   END IF
   dfi_moist_boundary_table( idomain, P_dfi_qs ) = .TRUE.
   dfi_moist_dname_table( idomain, P_dfi_qs ) = 'QSNOW'
   dfi_moist_desc_table( idomain, P_dfi_qs ) = 'Snow mixing ratio'
   dfi_moist_units_table( idomain, P_dfi_qs ) = 'kg kg-1'
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(1) = 0 ! 00000000 
   dfi_moist_streams_table( idomain, P_dfi_qs )%stream(2) = 2097152 ! 00200000 
   F_dfi_qs = .TRUE.
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==3)THEN
   IF ( ozmixm_index_table( PARAM_mth01 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth01 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth01 , idomain ) = P_mth01
   ELSE
     P_mth01 = ozmixm_index_table( PARAM_mth01 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth01 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth01 ) = 'OZMIXMTH01'
   ozmixm_desc_table( idomain, P_mth01 ) = 'Month 1 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth01 ) = '-'
   ozmixm_streams_table( idomain, P_mth01 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth01 )%stream(2) = 0 ! 00000000 
   F_mth01 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth02 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth02 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth02 , idomain ) = P_mth02
   ELSE
     P_mth02 = ozmixm_index_table( PARAM_mth02 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth02 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth02 ) = 'OZMIXMTH02'
   ozmixm_desc_table( idomain, P_mth02 ) = 'Month 2 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth02 ) = '-'
   ozmixm_streams_table( idomain, P_mth02 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth02 )%stream(2) = 0 ! 00000000 
   F_mth02 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth03 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth03 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth03 , idomain ) = P_mth03
   ELSE
     P_mth03 = ozmixm_index_table( PARAM_mth03 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth03 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth03 ) = 'OZMIXMTH03'
   ozmixm_desc_table( idomain, P_mth03 ) = 'Month 3 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth03 ) = '-'
   ozmixm_streams_table( idomain, P_mth03 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth03 )%stream(2) = 0 ! 00000000 
   F_mth03 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth04 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth04 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth04 , idomain ) = P_mth04
   ELSE
     P_mth04 = ozmixm_index_table( PARAM_mth04 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth04 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth04 ) = 'OZMIXMTH04'
   ozmixm_desc_table( idomain, P_mth04 ) = 'Month 4 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth04 ) = '-'
   ozmixm_streams_table( idomain, P_mth04 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth04 )%stream(2) = 0 ! 00000000 
   F_mth04 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth05 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth05 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth05 , idomain ) = P_mth05
   ELSE
     P_mth05 = ozmixm_index_table( PARAM_mth05 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth05 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth05 ) = 'OZMIXMTH05'
   ozmixm_desc_table( idomain, P_mth05 ) = 'Month 5 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth05 ) = '-'
   ozmixm_streams_table( idomain, P_mth05 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth05 )%stream(2) = 0 ! 00000000 
   F_mth05 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth06 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth06 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth06 , idomain ) = P_mth06
   ELSE
     P_mth06 = ozmixm_index_table( PARAM_mth06 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth06 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth06 ) = 'OZMIXMTH06'
   ozmixm_desc_table( idomain, P_mth06 ) = 'Month 6 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth06 ) = '-'
   ozmixm_streams_table( idomain, P_mth06 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth06 )%stream(2) = 0 ! 00000000 
   F_mth06 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth07 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth07 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth07 , idomain ) = P_mth07
   ELSE
     P_mth07 = ozmixm_index_table( PARAM_mth07 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth07 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth07 ) = 'OZMIXMTH07'
   ozmixm_desc_table( idomain, P_mth07 ) = 'Month 7 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth07 ) = '-'
   ozmixm_streams_table( idomain, P_mth07 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth07 )%stream(2) = 0 ! 00000000 
   F_mth07 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth08 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth08 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth08 , idomain ) = P_mth08
   ELSE
     P_mth08 = ozmixm_index_table( PARAM_mth08 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth08 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth08 ) = 'OZMIXMTH08'
   ozmixm_desc_table( idomain, P_mth08 ) = 'Month 8 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth08 ) = '-'
   ozmixm_streams_table( idomain, P_mth08 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth08 )%stream(2) = 0 ! 00000000 
   F_mth08 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth09 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth09 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth09 , idomain ) = P_mth09
   ELSE
     P_mth09 = ozmixm_index_table( PARAM_mth09 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth09 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth09 ) = 'OZMIXMTH09'
   ozmixm_desc_table( idomain, P_mth09 ) = 'Month 9 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth09 ) = '-'
   ozmixm_streams_table( idomain, P_mth09 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth09 )%stream(2) = 0 ! 00000000 
   F_mth09 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth10 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth10 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth10 , idomain ) = P_mth10
   ELSE
     P_mth10 = ozmixm_index_table( PARAM_mth10 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth10 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth10 ) = 'OZMIXMTH10'
   ozmixm_desc_table( idomain, P_mth10 ) = 'Month 10 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth10 ) = '-'
   ozmixm_streams_table( idomain, P_mth10 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth10 )%stream(2) = 0 ! 00000000 
   F_mth10 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth11 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth11 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth11 , idomain ) = P_mth11
   ELSE
     P_mth11 = ozmixm_index_table( PARAM_mth11 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth11 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth11 ) = 'OZMIXMTH11'
   ozmixm_desc_table( idomain, P_mth11 ) = 'Month 11 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth11 ) = '-'
   ozmixm_streams_table( idomain, P_mth11 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth11 )%stream(2) = 0 ! 00000000 
   F_mth11 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth12 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth12 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth12 , idomain ) = P_mth12
   ELSE
     P_mth12 = ozmixm_index_table( PARAM_mth12 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth12 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth12 ) = 'OZMIXMTH12'
   ozmixm_desc_table( idomain, P_mth12 ) = 'Month 12 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth12 ) = '-'
   ozmixm_streams_table( idomain, P_mth12 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth12 )%stream(2) = 0 ! 00000000 
   F_mth12 = .TRUE.
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==4)THEN
   IF ( ozmixm_index_table( PARAM_mth01 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth01 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth01 , idomain ) = P_mth01
   ELSE
     P_mth01 = ozmixm_index_table( PARAM_mth01 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth01 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth01 ) = 'OZMIXMTH01'
   ozmixm_desc_table( idomain, P_mth01 ) = 'Month 1 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth01 ) = '-'
   ozmixm_streams_table( idomain, P_mth01 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth01 )%stream(2) = 0 ! 00000000 
   F_mth01 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth02 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth02 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth02 , idomain ) = P_mth02
   ELSE
     P_mth02 = ozmixm_index_table( PARAM_mth02 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth02 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth02 ) = 'OZMIXMTH02'
   ozmixm_desc_table( idomain, P_mth02 ) = 'Month 2 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth02 ) = '-'
   ozmixm_streams_table( idomain, P_mth02 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth02 )%stream(2) = 0 ! 00000000 
   F_mth02 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth03 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth03 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth03 , idomain ) = P_mth03
   ELSE
     P_mth03 = ozmixm_index_table( PARAM_mth03 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth03 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth03 ) = 'OZMIXMTH03'
   ozmixm_desc_table( idomain, P_mth03 ) = 'Month 3 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth03 ) = '-'
   ozmixm_streams_table( idomain, P_mth03 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth03 )%stream(2) = 0 ! 00000000 
   F_mth03 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth04 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth04 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth04 , idomain ) = P_mth04
   ELSE
     P_mth04 = ozmixm_index_table( PARAM_mth04 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth04 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth04 ) = 'OZMIXMTH04'
   ozmixm_desc_table( idomain, P_mth04 ) = 'Month 4 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth04 ) = '-'
   ozmixm_streams_table( idomain, P_mth04 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth04 )%stream(2) = 0 ! 00000000 
   F_mth04 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth05 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth05 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth05 , idomain ) = P_mth05
   ELSE
     P_mth05 = ozmixm_index_table( PARAM_mth05 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth05 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth05 ) = 'OZMIXMTH05'
   ozmixm_desc_table( idomain, P_mth05 ) = 'Month 5 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth05 ) = '-'
   ozmixm_streams_table( idomain, P_mth05 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth05 )%stream(2) = 0 ! 00000000 
   F_mth05 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth06 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth06 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth06 , idomain ) = P_mth06
   ELSE
     P_mth06 = ozmixm_index_table( PARAM_mth06 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth06 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth06 ) = 'OZMIXMTH06'
   ozmixm_desc_table( idomain, P_mth06 ) = 'Month 6 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth06 ) = '-'
   ozmixm_streams_table( idomain, P_mth06 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth06 )%stream(2) = 0 ! 00000000 
   F_mth06 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth07 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth07 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth07 , idomain ) = P_mth07
   ELSE
     P_mth07 = ozmixm_index_table( PARAM_mth07 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth07 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth07 ) = 'OZMIXMTH07'
   ozmixm_desc_table( idomain, P_mth07 ) = 'Month 7 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth07 ) = '-'
   ozmixm_streams_table( idomain, P_mth07 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth07 )%stream(2) = 0 ! 00000000 
   F_mth07 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth08 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth08 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth08 , idomain ) = P_mth08
   ELSE
     P_mth08 = ozmixm_index_table( PARAM_mth08 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth08 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth08 ) = 'OZMIXMTH08'
   ozmixm_desc_table( idomain, P_mth08 ) = 'Month 8 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth08 ) = '-'
   ozmixm_streams_table( idomain, P_mth08 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth08 )%stream(2) = 0 ! 00000000 
   F_mth08 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth09 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth09 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth09 , idomain ) = P_mth09
   ELSE
     P_mth09 = ozmixm_index_table( PARAM_mth09 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth09 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth09 ) = 'OZMIXMTH09'
   ozmixm_desc_table( idomain, P_mth09 ) = 'Month 9 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth09 ) = '-'
   ozmixm_streams_table( idomain, P_mth09 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth09 )%stream(2) = 0 ! 00000000 
   F_mth09 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth10 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth10 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth10 , idomain ) = P_mth10
   ELSE
     P_mth10 = ozmixm_index_table( PARAM_mth10 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth10 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth10 ) = 'OZMIXMTH10'
   ozmixm_desc_table( idomain, P_mth10 ) = 'Month 10 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth10 ) = '-'
   ozmixm_streams_table( idomain, P_mth10 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth10 )%stream(2) = 0 ! 00000000 
   F_mth10 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth11 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth11 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth11 , idomain ) = P_mth11
   ELSE
     P_mth11 = ozmixm_index_table( PARAM_mth11 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth11 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth11 ) = 'OZMIXMTH11'
   ozmixm_desc_table( idomain, P_mth11 ) = 'Month 11 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth11 ) = '-'
   ozmixm_streams_table( idomain, P_mth11 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth11 )%stream(2) = 0 ! 00000000 
   F_mth11 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth12 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth12 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth12 , idomain ) = P_mth12
   ELSE
     P_mth12 = ozmixm_index_table( PARAM_mth12 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth12 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth12 ) = 'OZMIXMTH12'
   ozmixm_desc_table( idomain, P_mth12 ) = 'Month 12 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth12 ) = '-'
   ozmixm_streams_table( idomain, P_mth12 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth12 )%stream(2) = 0 ! 00000000 
   F_mth12 = .TRUE.
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==24)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==5)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==7)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==98)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==3)THEN
   IF ( ozmixm_index_table( PARAM_mth01 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth01 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth01 , idomain ) = P_mth01
   ELSE
     P_mth01 = ozmixm_index_table( PARAM_mth01 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth01 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth01 ) = 'OZMIXMTH01'
   ozmixm_desc_table( idomain, P_mth01 ) = 'Month 1 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth01 ) = '-'
   ozmixm_streams_table( idomain, P_mth01 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth01 )%stream(2) = 0 ! 00000000 
   F_mth01 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth02 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth02 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth02 , idomain ) = P_mth02
   ELSE
     P_mth02 = ozmixm_index_table( PARAM_mth02 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth02 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth02 ) = 'OZMIXMTH02'
   ozmixm_desc_table( idomain, P_mth02 ) = 'Month 2 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth02 ) = '-'
   ozmixm_streams_table( idomain, P_mth02 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth02 )%stream(2) = 0 ! 00000000 
   F_mth02 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth03 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth03 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth03 , idomain ) = P_mth03
   ELSE
     P_mth03 = ozmixm_index_table( PARAM_mth03 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth03 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth03 ) = 'OZMIXMTH03'
   ozmixm_desc_table( idomain, P_mth03 ) = 'Month 3 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth03 ) = '-'
   ozmixm_streams_table( idomain, P_mth03 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth03 )%stream(2) = 0 ! 00000000 
   F_mth03 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth04 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth04 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth04 , idomain ) = P_mth04
   ELSE
     P_mth04 = ozmixm_index_table( PARAM_mth04 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth04 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth04 ) = 'OZMIXMTH04'
   ozmixm_desc_table( idomain, P_mth04 ) = 'Month 4 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth04 ) = '-'
   ozmixm_streams_table( idomain, P_mth04 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth04 )%stream(2) = 0 ! 00000000 
   F_mth04 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth05 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth05 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth05 , idomain ) = P_mth05
   ELSE
     P_mth05 = ozmixm_index_table( PARAM_mth05 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth05 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth05 ) = 'OZMIXMTH05'
   ozmixm_desc_table( idomain, P_mth05 ) = 'Month 5 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth05 ) = '-'
   ozmixm_streams_table( idomain, P_mth05 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth05 )%stream(2) = 0 ! 00000000 
   F_mth05 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth06 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth06 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth06 , idomain ) = P_mth06
   ELSE
     P_mth06 = ozmixm_index_table( PARAM_mth06 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth06 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth06 ) = 'OZMIXMTH06'
   ozmixm_desc_table( idomain, P_mth06 ) = 'Month 6 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth06 ) = '-'
   ozmixm_streams_table( idomain, P_mth06 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth06 )%stream(2) = 0 ! 00000000 
   F_mth06 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth07 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth07 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth07 , idomain ) = P_mth07
   ELSE
     P_mth07 = ozmixm_index_table( PARAM_mth07 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth07 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth07 ) = 'OZMIXMTH07'
   ozmixm_desc_table( idomain, P_mth07 ) = 'Month 7 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth07 ) = '-'
   ozmixm_streams_table( idomain, P_mth07 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth07 )%stream(2) = 0 ! 00000000 
   F_mth07 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth08 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth08 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth08 , idomain ) = P_mth08
   ELSE
     P_mth08 = ozmixm_index_table( PARAM_mth08 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth08 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth08 ) = 'OZMIXMTH08'
   ozmixm_desc_table( idomain, P_mth08 ) = 'Month 8 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth08 ) = '-'
   ozmixm_streams_table( idomain, P_mth08 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth08 )%stream(2) = 0 ! 00000000 
   F_mth08 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth09 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth09 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth09 , idomain ) = P_mth09
   ELSE
     P_mth09 = ozmixm_index_table( PARAM_mth09 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth09 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth09 ) = 'OZMIXMTH09'
   ozmixm_desc_table( idomain, P_mth09 ) = 'Month 9 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth09 ) = '-'
   ozmixm_streams_table( idomain, P_mth09 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth09 )%stream(2) = 0 ! 00000000 
   F_mth09 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth10 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth10 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth10 , idomain ) = P_mth10
   ELSE
     P_mth10 = ozmixm_index_table( PARAM_mth10 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth10 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth10 ) = 'OZMIXMTH10'
   ozmixm_desc_table( idomain, P_mth10 ) = 'Month 10 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth10 ) = '-'
   ozmixm_streams_table( idomain, P_mth10 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth10 )%stream(2) = 0 ! 00000000 
   F_mth10 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth11 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth11 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth11 , idomain ) = P_mth11
   ELSE
     P_mth11 = ozmixm_index_table( PARAM_mth11 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth11 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth11 ) = 'OZMIXMTH11'
   ozmixm_desc_table( idomain, P_mth11 ) = 'Month 11 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth11 ) = '-'
   ozmixm_streams_table( idomain, P_mth11 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth11 )%stream(2) = 0 ! 00000000 
   F_mth11 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth12 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth12 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth12 , idomain ) = P_mth12
   ELSE
     P_mth12 = ozmixm_index_table( PARAM_mth12 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth12 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth12 ) = 'OZMIXMTH12'
   ozmixm_desc_table( idomain, P_mth12 ) = 'Month 12 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth12 ) = '-'
   ozmixm_streams_table( idomain, P_mth12 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth12 )%stream(2) = 0 ! 00000000 
   F_mth12 = .TRUE.
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==4)THEN
   IF ( ozmixm_index_table( PARAM_mth01 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth01 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth01 , idomain ) = P_mth01
   ELSE
     P_mth01 = ozmixm_index_table( PARAM_mth01 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth01 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth01 ) = 'OZMIXMTH01'
   ozmixm_desc_table( idomain, P_mth01 ) = 'Month 1 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth01 ) = '-'
   ozmixm_streams_table( idomain, P_mth01 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth01 )%stream(2) = 0 ! 00000000 
   F_mth01 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth02 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth02 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth02 , idomain ) = P_mth02
   ELSE
     P_mth02 = ozmixm_index_table( PARAM_mth02 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth02 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth02 ) = 'OZMIXMTH02'
   ozmixm_desc_table( idomain, P_mth02 ) = 'Month 2 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth02 ) = '-'
   ozmixm_streams_table( idomain, P_mth02 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth02 )%stream(2) = 0 ! 00000000 
   F_mth02 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth03 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth03 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth03 , idomain ) = P_mth03
   ELSE
     P_mth03 = ozmixm_index_table( PARAM_mth03 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth03 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth03 ) = 'OZMIXMTH03'
   ozmixm_desc_table( idomain, P_mth03 ) = 'Month 3 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth03 ) = '-'
   ozmixm_streams_table( idomain, P_mth03 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth03 )%stream(2) = 0 ! 00000000 
   F_mth03 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth04 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth04 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth04 , idomain ) = P_mth04
   ELSE
     P_mth04 = ozmixm_index_table( PARAM_mth04 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth04 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth04 ) = 'OZMIXMTH04'
   ozmixm_desc_table( idomain, P_mth04 ) = 'Month 4 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth04 ) = '-'
   ozmixm_streams_table( idomain, P_mth04 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth04 )%stream(2) = 0 ! 00000000 
   F_mth04 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth05 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth05 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth05 , idomain ) = P_mth05
   ELSE
     P_mth05 = ozmixm_index_table( PARAM_mth05 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth05 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth05 ) = 'OZMIXMTH05'
   ozmixm_desc_table( idomain, P_mth05 ) = 'Month 5 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth05 ) = '-'
   ozmixm_streams_table( idomain, P_mth05 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth05 )%stream(2) = 0 ! 00000000 
   F_mth05 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth06 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth06 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth06 , idomain ) = P_mth06
   ELSE
     P_mth06 = ozmixm_index_table( PARAM_mth06 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth06 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth06 ) = 'OZMIXMTH06'
   ozmixm_desc_table( idomain, P_mth06 ) = 'Month 6 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth06 ) = '-'
   ozmixm_streams_table( idomain, P_mth06 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth06 )%stream(2) = 0 ! 00000000 
   F_mth06 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth07 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth07 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth07 , idomain ) = P_mth07
   ELSE
     P_mth07 = ozmixm_index_table( PARAM_mth07 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth07 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth07 ) = 'OZMIXMTH07'
   ozmixm_desc_table( idomain, P_mth07 ) = 'Month 7 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth07 ) = '-'
   ozmixm_streams_table( idomain, P_mth07 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth07 )%stream(2) = 0 ! 00000000 
   F_mth07 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth08 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth08 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth08 , idomain ) = P_mth08
   ELSE
     P_mth08 = ozmixm_index_table( PARAM_mth08 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth08 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth08 ) = 'OZMIXMTH08'
   ozmixm_desc_table( idomain, P_mth08 ) = 'Month 8 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth08 ) = '-'
   ozmixm_streams_table( idomain, P_mth08 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth08 )%stream(2) = 0 ! 00000000 
   F_mth08 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth09 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth09 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth09 , idomain ) = P_mth09
   ELSE
     P_mth09 = ozmixm_index_table( PARAM_mth09 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth09 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth09 ) = 'OZMIXMTH09'
   ozmixm_desc_table( idomain, P_mth09 ) = 'Month 9 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth09 ) = '-'
   ozmixm_streams_table( idomain, P_mth09 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth09 )%stream(2) = 0 ! 00000000 
   F_mth09 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth10 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth10 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth10 , idomain ) = P_mth10
   ELSE
     P_mth10 = ozmixm_index_table( PARAM_mth10 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth10 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth10 ) = 'OZMIXMTH10'
   ozmixm_desc_table( idomain, P_mth10 ) = 'Month 10 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth10 ) = '-'
   ozmixm_streams_table( idomain, P_mth10 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth10 )%stream(2) = 0 ! 00000000 
   F_mth10 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth11 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth11 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth11 , idomain ) = P_mth11
   ELSE
     P_mth11 = ozmixm_index_table( PARAM_mth11 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth11 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth11 ) = 'OZMIXMTH11'
   ozmixm_desc_table( idomain, P_mth11 ) = 'Month 11 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth11 ) = '-'
   ozmixm_streams_table( idomain, P_mth11 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth11 )%stream(2) = 0 ! 00000000 
   F_mth11 = .TRUE.
   IF ( ozmixm_index_table( PARAM_mth12 , idomain ) .lt. 1 ) THEN
     ozmixm_num_table(idomain) = ozmixm_num_table(idomain) + 1
     P_mth12 = ozmixm_num_table(idomain)
     ozmixm_index_table( PARAM_mth12 , idomain ) = P_mth12
   ELSE
     P_mth12 = ozmixm_index_table( PARAM_mth12 , idomain )
   END IF
   ozmixm_boundary_table( idomain, P_mth12 ) = .FALSE.
   ozmixm_dname_table( idomain, P_mth12 ) = 'OZMIXMTH12'
   ozmixm_desc_table( idomain, P_mth12 ) = 'Month 12 CAM ozone mixing ratio'
   ozmixm_units_table( idomain, P_mth12 ) = '-'
   ozmixm_streams_table( idomain, P_mth12 )%stream(1) = 0 ! 00000000 
   ozmixm_streams_table( idomain, P_mth12 )%stream(2) = 0 ! 00000000 
   F_mth12 = .TRUE.
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==24)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==5)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==7)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%ra_sw_physics(idomain)==98)THEN
  END IF
  IF (model_config_rec%ra_lw_physics(idomain)==31)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==91)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==88)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==4)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==7)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==10)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==89)THEN
  END IF
  IF (model_config_rec%sf_sfclay_physics(idomain)==12)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==4)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==5)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==88)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==7)THEN
  END IF
  IF (model_config_rec%sf_surface_physics(idomain)==8)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==83)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==93)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==4)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==7)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==8)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==9)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==10)THEN
  END IF
  IF (model_config_rec%bl_pbl_physics(idomain)==11)THEN
  END IF
  IF (model_config_rec%windfarm_opt(idomain)==1)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==93)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==84)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==85)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==4)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==5)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==7)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==10)THEN
  END IF
  IF (model_config_rec%cu_diag(idomain)==1)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==6)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==16)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==14)THEN
  END IF
  IF (model_config_rec%cu_physics(idomain)==99)THEN
  END IF
  IF (model_config_rec%shcu_physics(idomain)==1)THEN
  END IF
  IF (model_config_rec%shcu_physics(idomain)==2)THEN
  END IF
  IF (model_config_rec%shcu_physics(idomain)==3)THEN
  END IF
  IF (model_config_rec%dfi_stage==0)THEN
  END IF
  IF (model_config_rec%dfi_stage==1)THEN
  END IF
  IF (model_config_rec%dfi_stage==2)THEN
  END IF
  IF (model_config_rec%dfi_stage==3)THEN
  END IF
  IF (model_config_rec%dfi_stage==4)THEN
  END IF
  IF (model_config_rec%dfi_stage==5)THEN
  END IF
  IF (model_config_rec%dfi_opt==0)THEN
  END IF
  IF (model_config_rec%dfi_opt==1)THEN
  END IF
  IF (model_config_rec%dfi_opt==2)THEN
  END IF
  IF (model_config_rec%dfi_opt==3)THEN
  END IF
  IF (model_config_rec%seaice_albedo_opt==0)THEN
  END IF
  IF (model_config_rec%seaice_albedo_opt==1)THEN
  END IF
  IF (model_config_rec%seaice_albedo_opt==2)THEN
  END IF
  IF (model_config_rec%seaice_snowdepth_opt==0)THEN
  END IF
  IF (model_config_rec%seaice_snowdepth_opt==1)THEN
  END IF
  IF (model_config_rec%seaice_thickness_opt==0)THEN
  END IF
  IF (model_config_rec%seaice_thickness_opt==1)THEN
  END IF
  IF (model_config_rec%io_form_restart==1)THEN
  END IF
  IF (model_config_rec%io_form_restart==2)THEN
  END IF
  IF (model_config_rec%io_form_restart==3)THEN
  END IF
  IF (model_config_rec%io_form_restart==4)THEN
  END IF
  IF (model_config_rec%io_form_restart==5)THEN
  END IF
  IF (model_config_rec%io_form_restart==6)THEN
  END IF
  IF (model_config_rec%io_form_restart==7)THEN
  END IF
  IF (model_config_rec%io_form_restart==8)THEN
  END IF
  IF (model_config_rec%io_form_restart==9)THEN
  END IF
  IF (model_config_rec%io_form_restart==10)THEN
  END IF
  IF (model_config_rec%io_form_restart==11)THEN
  END IF
  IF (model_config_rec%io_form_restart==12)THEN
  END IF
  IF (model_config_rec%wrf_hydro==0)THEN
  END IF
  IF (model_config_rec%wrf_hydro==1)THEN
  END IF
!ENDOFREGISTRYGENERATEDINCLUDE
