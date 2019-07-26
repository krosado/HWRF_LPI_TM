!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nest_forcedown_interp_vert.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF ( SIZE( grid%t, 1 ) * SIZE( grid%t, 2 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%t, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%q, 1 ) * SIZE( grid%q, 2 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%q, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%u, 1 ) * SIZE( grid%u, 2 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%u, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%v, 1 ) * SIZE( grid%v, 2 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%v, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%q2, 1 ) * SIZE( grid%q2, 2 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%q2, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%cwm, 1 ) * SIZE( grid%cwm, 2 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  grid%cwm, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
IF ( SIZE( grid%pint, 1 ) * SIZE( grid%pint, 2 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting_w( &
                                  grid%pint, & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  alt_w_c, alt_w_n ) !coordinates for parent and nest
ENDIF
DO itrace = PARAM_FIRST_SCALAR, num_moist
IF ( SIZE( moist, 1 ) * SIZE( moist, 2 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  moist(grid%sm31,grid%sm32,grid%sm33,itrace), & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_scalar
IF ( SIZE( scalar, 1 ) * SIZE( scalar, 2 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  scalar(grid%sm31,grid%sm32,grid%sm33,itrace), & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
ENDDO
DO itrace = PARAM_FIRST_SCALAR, num_dfi_scalar
IF ( SIZE( dfi_scalar, 1 ) * SIZE( dfi_scalar, 2 ) .GT. 1 ) THEN 
    CALL vert_interp_vert_nesting( &
                                  dfi_scalar(grid%sm31,grid%sm32,grid%sm33,itrace), & !CD field
                                  ids, ide, kds, kde, jds, jde, & !CD dims
                                  ims, ime, kms, kme, jms, jme, & !CD dims
                                  ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                  pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                  pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                  alt_u_c, alt_u_n ) !coordinates for parent and nest
ENDIF
ENDDO
!ENDOFREGISTRYGENERATEDINCLUDE
