
PROGRAM idelprep

!$$$  main program documentation block
!
! main program: idelprep
!   PRGMMR: RICHARD YABLONSKY, URI/GSO, 2014-10-07
!   LANG: FORTRAN 90
! Abstract: Read idealized data (ascii) and prepare the
! initial conditions for Princeton Ocean Model (MPIPOM-TC). 
! 
!
! program history log:
!   

USE netcdf
IMPLICIT NONE

 INTERFACE
  SUBROUTINE check(index)
  USE netcdf
  IMPLICIT NONE
    INTEGER, INTENT(IN)             ::  index
  END SUBROUTINE
 END INTERFACE

 INTEGER, PARAMETER                 :: nx = 869, ny = 449, nl = 33
 INTEGER, PARAMETER                 :: im = 869, jm = 449, kb = 23
 INTEGER                            :: ionedim
 INTEGER                            :: i, j, k, fid, igfssst, ifplane
 INTEGER                            :: im1, jm1
 INTEGER                            :: x_dimid, y_dimid, z_dimid
 INTEGER                            :: z_varid, zz_varid
 INTEGER                            :: dx_varid, dy_varid
 INTEGER                            :: east_u_varid, east_v_varid
 INTEGER                            :: east_e_varid, east_c_varid
 INTEGER                            :: north_u_varid, north_v_varid
 INTEGER                            :: north_e_varid, north_c_varid
 INTEGER                            :: rot_varid, h_varid, fsm_varid
 INTEGER                            :: dum_varid, dvm_varid
 INTEGER                            :: t_varid, s_varid
 INTEGER                            :: u_varid, v_varid, el_varid
 REAL                               :: d2r, rearth, blon, blat, cnorth_e
 REAL, DIMENSION(kb)                :: z1, z, zz
 REAL, DIMENSION(nl)                :: taa, saa, area, Zp, Tp, Sp
 REAL, DIMENSION(im,jm)             :: alon, alat, el
 REAL, DIMENSION(im,jm)             :: dx, dy, rot, h, fsm, dum, dvm
 REAL, DIMENSION(im,jm)             :: east_e, north_e, east_c, north_c
 REAL, DIMENSION(im,jm)             :: east_u, north_u, east_v, north_v
 REAL, DIMENSION(im,jm)             :: stm
 REAL, DIMENSION(im,jm,nl)          :: t, s, uin, vin
 REAL, DIMENSION(im,jm,nl)          :: u, v, tclim, sclim
 CHARACTER(LEN=120)                 :: runid, gridfile, tsclimfile
 CHARACTER(LEN=120)                 :: tsinfile, uvinfile, elinfile
 CHARACTER(LEN=4)                   :: odata

 READ(5,*) odata ! Region-specific information
 WRITE(6,*) odata

! MPIPOM-TC idealized grid for selected region

 IF (odata == 'idtr') THEN
   blon =  -98.5
   blat =   10.0
 ELSE IF (odata == 'idep') THEN
   blon = -167.5
   blat =    5.0
 ELSE IF (odata == 'idwp') THEN
   blon =   96.6
   blat =    5.0
 ELSE IF (odata == 'idni') THEN
   blon =   32.3
   blat =    2.5
 ELSE IF (odata == 'idsi') THEN
   blon =   32.3
   blat =  -40.0
 ELSE IF (odata == 'idsw') THEN
   blon =   96.6
   blat =  -40.0
 ELSE IF (odata == 'idse') THEN
   blon = -179.8
   blat =  -40.0
 ELSE IF (odata == 'idsa') THEN
   blon =  -83.5
   blat =  -40.0
 ELSE
   WRITE(6,*) 'Bad region selection... stopping now.'
   STOP
 END IF

! MPIPOM-TC z-levels (z1)

 z1 = (/0.,  10.,  20.,  30.,  40.,  50.,  60.,  70., & 
       85., 100., 120., 150., 200., 300., 450., 650., &
      900.,1300.,1800.,2400.,3200.,4200.,5500./)

! MPIPOM-TC full (z) and half (zz) sigma levels

 DO k = 1,kb
   z(k) = -z1(k)/z1(kb)
 END DO

 DO k = 1,kb-1
   zz(k) = 0.5*(z(k)+z(k+1))
 END DO
 zz(kb) = z(kb)

! Define alon and alat (and cnorth_e for f-plane)

 DO j = 1,jm
   DO i = 1,im
     alon(i,j) = blon+(i-1)*83.2/(im-1)
     alat(i,j) = blat+(j-1)*37.5/(jm-1)
   END DO
 END DO

 READ(5,*) ifplane
 READ(5,*) cnorth_e
 WRITE(6,*) 'Use f-plane (0=no; 1=yes)? ',ifplane
 WRITE(6,*) 'Latitude for f-plane (if used): ',cnorth_e

! Read idealized z, t, and s data (ascii)

 DO k = 1,nl
   read(13,*) Zp(k),Tp(k),Sp(k) ! ensure TS_ideal.dat linked to fort.13
 END DO

 DO k = 1,nl
   DO j = 1,jm
     DO i = 1,im
       t(i,j,k) = Tp(k)
       s(i,j,k) = Sp(k)
     END DO
   END DO
 END DO

! Set uin, vin, and el to 0

 DO k = 1,nl
   DO j = 1,jm
     DO i = 1,im
       uin(i,j,k) = 0. ! calculated as geostrophic in old POM-TC
       vin(i,j,k) = 0. ! calculated as geostrophic in old POM-TC
     END DO
   END DO
 END DO

 DO j = 1,jm
   DO i = 1,im
     el(i,j) = 0.
   END DO
 END DO

! MPIPOM-TC el points

 DO j = 1,jm
   DO i = 1,im
     east_e(i,j) = alon(i,j)
     north_e(i,j) = alat(i,j)
   END DO
 END DO

! MPIPOM-TC grid spacing

 d2r = 3.1416/180.
 rearth = 6371.e3

 IF (ifplane.eq.1) THEN

   DO j = 1,jm
     DO i = 2,im-1
       dx(i,j) = 0.5*d2r*rearth*sqrt(((east_e(i+1,j)-east_e(i-1,j))* &
         cos(cnorth_e*d2r))**2.+(north_e(i+1,j)-north_e(i-1,j))**2.)
     END DO ! *does* use f-plane
     dx(1,j) = dx(2,j)
     dx(im,j) = dx(im-1,j)
   END DO

   DO i = 1,im
     DO j = 2,jm-1
       dy(i,j) = 0.5*d2r*rearth*sqrt(((east_e(i,j+1)-east_e(i,j-1))* &
         cos(cnorth_e*d2r))**2.+(north_e(i,j+1)-north_e(i,j-1))**2.)
     END DO ! *does* use f-plane
     dy(i,1) = dy(i,2)
     dy(i,jm) = dy(i,jm-1)
   END DO

 ELSE

   DO j = 1,jm
     DO i = 2,im-1
       dx(i,j) = 0.5*d2r*rearth*sqrt(((east_e(i+1,j)-east_e(i-1,j))* &
         cos(north_e(i,j)*d2r))**2.+(north_e(i+1,j)-north_e(i-1,j))**2.)
     END DO ! *does not* use f-plane
     dx(1,j) = dx(2,j)
     dx(im,j) = dx(im-1,j)
   END DO

   DO i = 1,im
     DO j = 2,jm-1
       dy(i,j) = 0.5*d2r*rearth*sqrt(((east_e(i,j+1)-east_e(i,j-1))* &
         cos(north_e(i,j)*d2r))**2.+(north_e(i,j+1)-north_e(i,j-1))**2.)
     END DO ! *does not* use f-plane
     dy(i,1) = dy(i,2)
     dy(i,jm) = dy(i,jm-1)
   END DO

 END IF

! MPIPOM-TC cell corners

 DO j = 2,jm
   DO i = 2,im
     east_c(i,j) = 0.25*(east_e(i,j)+east_e(i,j-1)+ &
                         east_e(i-1,j)+east_e(i-1,j-1))
     north_c(i,j) = 0.25*(north_e(i,j)+north_e(i,j-1)+ &
                          north_e(i-1,j)+north_e(i-1,j-1))
   END DO
 END DO
 DO i = 2,im
   east_c(i,1) = 2.*east_c(i,2)-east_c(i,3)
   north_c(i,1) = 2.*north_c(i,2)-north_c(i,3)
 END DO
 east_c(1,1) = 2.*east_c(2,1)-east_c(3,1)
 DO j = 2,jm
   east_c(1,j) = 2.*east_c(2,j)-east_c(3,j)
   north_c(1,j) = 2.*north_c(2,j)-north_c(3,j)
 END DO
 north_c(1,1) = 2.*north_c(1,2)-north_c(1,3)

  READ(5,*) ionedim
  IF (ionedim.eq.1) THEN

! MPIPOM-TC u points

 DO i = 1,im
   DO j = 1,jm
     east_u(i,j) = east_e(i,j)
     north_u(i,j) = north_e(i,j)
   END DO
 END DO

! MPIPOM-TC v points

 DO j = 1,jm
   DO i = 1,im
     east_v(i,j) = east_e(i,j)
     north_v(i,j) = north_e(i,j)
   END DO
 END DO

  ELSE

! MPIPOM-TC u points

 DO i = 1,im
   DO j = 1,jm-1
     east_u(i,j) = 0.5*(east_c(i,j)+east_c(i,j+1))
     north_u(i,j) = 0.5*(north_c(i,j)+north_c(i,j+1))
   END DO
   east_u(i,jm) = 0.5*(east_c(i,jm)*3.-east_c(i,jm-1))
   north_u(i,jm) = 0.5*(north_c(i,jm)*3.-north_c(i,jm-1))
 END DO

! MPIPOM-TC v points

 DO j = 1,jm
   DO i = 1,im-1
     east_v(i,j) = 0.5*(east_c(i,j)+east_c(i+1,j))
     north_v(i,j) = 0.5*(north_c(i,j)+north_c(i+1,j))
   END DO
   east_v(im,j) = 0.5*(east_c(im,j)*3.-east_c(im-1,j))
   north_v(im,j) = 0.5*(north_c(im,j)*3.-north_c(im-1,j))
 END DO

  END IF

! MPIPOM-TC rotation angle

 DO j = 1,jm
   DO i = 1,im-1
     rot(i,j) = atan2(north_e(i+1,j)-north_e(i,j), &
                      east_e(i+1,j)-east_e(i,j))
   END DO
   rot(im,j) = rot(im-1,j)
 END DO

! MPIPOM-TC bottom topography (h) and mask (fsm)

 DO j = 1,jm
   DO i = 1,im
     h(i,j) = 2500.0
     fsm(i,j) = 1.
     dum(i,j) = 1.
     dvm(i,j) = 1.
   END DO
 END DO

! Read and assimilate gfs sst into temperature field
 READ(5,*) igfssst
 WRITE(6,*) 'SST assimilation (0=no; 1=yes)? ',igfssst
 IF (igfssst.eq.1) THEN
     print*, 'Preparing to read gfs sst...'
     read(23,224) im1,jm1 ! ensure lonlat.gfs   is linked to fort.23
     rewind(23)           ! ensure sst.gfs.dat  is linked to fort.21
 224 format(1x,2i5)       ! ensure mask.gfs.dat is linked to fort.22
     call serftempr(im1,jm1,stm,fsm,alon,alat,im,jm)
     print*, '... serftempr read gfs sst: im1,jm = ',im1,jm1

     print*, 'Preparing to assimilate gfs sst...'
     call mixsstz(t,stm,h,fsm,Zp,im,jm,nl,0)
     print*, '... mixsstz assimilated gfs sst'
 END IF

! MPIPOM-TC u and v modification

 DO k = 1,nl
  IF (ionedim.eq.1) THEN
   DO j = 1,jm
     DO i = 1,im
       u(i,j,k) = uin(i,j,k)*dum(i,j)
       v(i,j,k) = vin(i,j,k)*dvm(i,j)
     END DO
   END DO
  ELSE
   DO j = 1,jm
     u(1,j,k) = uin(1,j,k)*dum(1,j)
     DO i = 2,im
       u(i,j,k) = 0.5*(uin(i-1,j,k)+uin(i,j,k))*dum(i,j)
     END DO
   END DO
   DO i = 1,im
     v(i,1,k) = vin(i,1,k)*dvm(i,1)
     DO j = 2,jm
       v(i,j,k) = 0.5*(vin(i,j-1,k)+vin(i,j,k))*dvm(i,j)
     END DO
   END DO
  END IF
 END DO

! MPIPOM-TC el modification

 DO j = 1,jm
   DO i = 1,im
     el(i,j) = el(i,j)*fsm(i,j)
   END DO
 END DO

! MPIPOM-TC tclim and sclim calcuation

  DO k = 1,nl
    taa(k) = 0.
    saa(k) = 0.
    area(k) = 0.
    DO j = 1,jm
      DO i = 1,im
        IF (t(i,j,k) /= 0.) THEN 
          taa(k) = taa(k)+t(i,j,k)
          saa(k) = saa(k)+s(i,j,k)
          area(k) = area(k)+1.
        END IF
      END DO
    END DO
    taa(k) = taa(k)/area(k)
    saa(k) = saa(k)/area(k)
  END DO

  DO k = 1,nl
    DO j = 1,jm
      DO i = 1,im
          tclim(i,j,k) = taa(k)
          sclim(i,j,k) = saa(k)
      END DO
    END DO
  END DO

! Prepare to create MPIPOM-TC input files

 READ(5,*) runid
 WRITE(6,*) TRIM(ADJUSTL(runid))
 WRITE(gridfile,'(a,''.grid.nc'')')       TRIM(ADJUSTL(runid))
 WRITE(tsinfile,'(a,''.ts_initial.nc'')') TRIM(ADJUSTL(runid)) 
 WRITE(uvinfile,'(a,''.uv_initial.nc'')') TRIM(ADJUSTL(runid))
 WRITE(elinfile,'(a,''.el_initial.nc'')') TRIM(ADJUSTL(runid))
 WRITE(tsclimfile,'(a,''.ts_clim.nc'')')  TRIM(ADJUSTL(runid))

! Create MPIPOM-TC input grid file

 CALL check(NF90_CREATE(gridfile,NF90_CLOBBER,fid))
 CALL check(NF90_PUT_ATT(fid,NF90_GLOBAL,"title",TRIM(ADJUSTL(runid))))
 CALL check(NF90_PUT_ATT(fid,NF90_GLOBAL,"description", &
                                         "POM grid file"))
 CALL check(NF90_DEF_DIM(fid,"x",im,x_dimid))
 CALL check(NF90_DEF_DIM(fid,"y",jm,y_dimid))
 CALL check(NF90_DEF_DIM(fid,"z",kb,z_dimid))
 CALL check(NF90_DEF_VAR(fid,"z",NF90_DOUBLE,z_dimid,z_varid))
 CALL check(NF90_PUT_ATT(fid,z_varid,"long_name", &
                                     "sigma of cell face"))
 CALL check(NF90_PUT_ATT(fid,z_varid,"units","sigma_level"))
 CALL check(NF90_PUT_ATT(fid,z_varid,"standard_name", &
                                     "ocean_sigma_coordinate"))
 CALL check(NF90_PUT_ATT(fid,z_varid,"formula_terms", &
                                     "sigma: z eta: elb depth: h"))
 CALL check(NF90_DEF_VAR(fid,"zz",NF90_DOUBLE,z_dimid,zz_varid))
 CALL check(NF90_PUT_ATT(fid,zz_varid,"long_name", &
                                      "sigma of cell centre"))
 CALL check(NF90_PUT_ATT(fid,zz_varid,"units","sigma_level"))
 CALL check(NF90_PUT_ATT(fid,zz_varid,"standard_name", &
                                      "ocean_sigma_coordinate"))
 CALL check(NF90_PUT_ATT(fid,zz_varid,"formula_terms", &
                                      "sigma: zz eta: elb depth: h"))
 CALL check(NF90_DEF_VAR(fid,"dx",NF90_DOUBLE,(/x_dimid,y_dimid/), &
                                                       dx_varid))
 CALL check(NF90_PUT_ATT(fid,dx_varid,"long_name", &
                                      "grid increment in x"))
 CALL check(NF90_PUT_ATT(fid,dx_varid,"units","metre"))
 CALL check(NF90_PUT_ATT(fid,dx_varid,"coords","east_e north_e"))
 CALL check(NF90_DEF_VAR(fid,"dy",NF90_DOUBLE,(/x_dimid,y_dimid/), &
                                                       dy_varid))
 CALL check(NF90_PUT_ATT(fid,dy_varid,"long_name", &
                                      "grid increment in y"))
 CALL check(NF90_PUT_ATT(fid,dy_varid,"units","metre"))
 CALL check(NF90_PUT_ATT(fid,dy_varid,"coords","east_e north_e"))
 CALL check(NF90_DEF_VAR(fid,"east_u",NF90_DOUBLE, &
                         (/x_dimid,y_dimid/),east_u_varid))
 CALL check(NF90_PUT_ATT(fid,east_u_varid,"long_name", &
                                          "easting of u-points"))
 CALL check(NF90_PUT_ATT(fid,east_u_varid,"units","degree"))
 CALL check(NF90_PUT_ATT(fid,east_u_varid,"coords","east_u north_u"))
 CALL check(NF90_DEF_VAR(fid,"east_v",NF90_DOUBLE, &
                         (/x_dimid,y_dimid/),east_v_varid))
 CALL check(NF90_PUT_ATT(fid,east_v_varid,"long_name", &
                                          "easting of v-points"))
 CALL check(NF90_PUT_ATT(fid,east_v_varid,"units","degree"))
 CALL check(NF90_PUT_ATT(fid,east_v_varid,"coords","east_v north_v"))
 CALL check(NF90_DEF_VAR(fid,"east_e",NF90_DOUBLE, &
                         (/x_dimid,y_dimid/),east_e_varid))
 CALL check(NF90_PUT_ATT(fid,east_e_varid,"long_name", &
                             "easting of elevation points"))
 CALL check(NF90_PUT_ATT(fid,east_e_varid,"units","degree"))
 CALL check(NF90_PUT_ATT(fid,east_e_varid,"coords","east_e north_e"))
 CALL check(NF90_DEF_VAR(fid,"east_c",NF90_DOUBLE, &
                         (/x_dimid,y_dimid/),east_c_varid))
 CALL check(NF90_PUT_ATT(fid,east_c_varid,"long_name", &
                             "easting of cell corners"))
 CALL check(NF90_PUT_ATT(fid,east_c_varid,"units","degree"))
 CALL check(NF90_PUT_ATT(fid,east_c_varid,"coords","east_c north_c"))
 CALL check(NF90_DEF_VAR(fid,"north_u",NF90_DOUBLE, &
                         (/x_dimid,y_dimid/),north_u_varid))
 CALL check(NF90_PUT_ATT(fid,north_u_varid,"long_name", &
                                           "northing of u-points"))
 CALL check(NF90_PUT_ATT(fid,north_u_varid,"units","degree"))
 CALL check(NF90_PUT_ATT(fid,north_u_varid,"coords","east_u north_u"))
 CALL check(NF90_DEF_VAR(fid,"north_v",NF90_DOUBLE, &
                         (/x_dimid,y_dimid/),north_v_varid))
 CALL check(NF90_PUT_ATT(fid,north_v_varid,"long_name", &
                                           "northing of v-points"))
 CALL check(NF90_PUT_ATT(fid,north_v_varid,"units","degree"))
 CALL check(NF90_PUT_ATT(fid,north_v_varid,"coords","east_v north_v"))
 CALL check(NF90_DEF_VAR(fid,"north_e",NF90_DOUBLE, &
                         (/x_dimid,y_dimid/),north_e_varid))
 CALL check(NF90_PUT_ATT(fid,north_e_varid,"long_name", &
                             "northing of elevation points"))
 CALL check(NF90_PUT_ATT(fid,north_e_varid,"units","degree"))
 CALL check(NF90_PUT_ATT(fid,north_e_varid,"coords","east_e north_e"))
 CALL check(NF90_DEF_VAR(fid,"north_c",NF90_DOUBLE, &
                         (/x_dimid,y_dimid/),north_c_varid))
 CALL check(NF90_PUT_ATT(fid,north_c_varid,"long_name", &
                             "northing of cell corners"))
 CALL check(NF90_PUT_ATT(fid,north_c_varid,"units","degree"))
 CALL check(NF90_PUT_ATT(fid,north_c_varid,"coords","east_c north_c"))
 CALL check(NF90_DEF_VAR(fid,"rot",NF90_DOUBLE, &
                         (/x_dimid,y_dimid/),rot_varid))
 CALL check(NF90_PUT_ATT(fid,rot_varid,"long_name", &
                             "rotation angle of x-axis wrt. east"))
 CALL check(NF90_PUT_ATT(fid,rot_varid,"units","degree"))
 CALL check(NF90_PUT_ATT(fid,rot_varid,"coords","east_e north_e"))
 CALL check(NF90_DEF_VAR(fid,"h",NF90_DOUBLE, &
                         (/x_dimid,y_dimid/),h_varid))
 CALL check(NF90_PUT_ATT(fid,h_varid,"long_name", &
                             "undisturbed water depth"))
 CALL check(NF90_PUT_ATT(fid,h_varid,"units","metre"))
 CALL check(NF90_PUT_ATT(fid,h_varid,"coords","east_e north_e"))
 CALL check(NF90_DEF_VAR(fid,"fsm",NF90_DOUBLE, &
                         (/x_dimid,y_dimid/),fsm_varid))
 CALL check(NF90_PUT_ATT(fid,fsm_varid,"long_name", &
                             "free surface mask"))
 CALL check(NF90_PUT_ATT(fid,fsm_varid,"units","dimensionless"))
 CALL check(NF90_PUT_ATT(fid,fsm_varid,"coords","east_e north_e"))
 CALL check(NF90_DEF_VAR(fid,"dum",NF90_DOUBLE, &
                         (/x_dimid,y_dimid/),dum_varid))
 CALL check(NF90_PUT_ATT(fid,dum_varid,"long_name", &
                             "u-velocity mask"))
 CALL check(NF90_PUT_ATT(fid,dum_varid,"units","dimensionless"))
 CALL check(NF90_PUT_ATT(fid,dum_varid,"coords","east_u north_u"))
 CALL check(NF90_DEF_VAR(fid,"dvm",NF90_DOUBLE, &
                         (/x_dimid,y_dimid/),dvm_varid))
 CALL check(NF90_PUT_ATT(fid,dvm_varid,"long_name", &
                             "v-velocity mask"))
 CALL check(NF90_PUT_ATT(fid,dvm_varid,"units","dimensionless"))
 CALL check(NF90_PUT_ATT(fid,dvm_varid,"coords","east_v north_v"))
 CALL check(NF90_ENDDEF(fid))
 CALL check(NF90_PUT_VAR(fid,z_varid,z))
 CALL check(NF90_PUT_VAR(fid,zz_varid,zz))
 CALL check(NF90_PUT_VAR(fid,dx_varid,dx))
 CALL check(NF90_PUT_VAR(fid,dy_varid,dy))
 CALL check(NF90_PUT_VAR(fid,east_u_varid,east_u))
 CALL check(NF90_PUT_VAR(fid,east_v_varid,east_v))
 CALL check(NF90_PUT_VAR(fid,east_c_varid,east_c))
 CALL check(NF90_PUT_VAR(fid,east_e_varid,east_e))
 CALL check(NF90_PUT_VAR(fid,north_u_varid,north_u))
 CALL check(NF90_PUT_VAR(fid,north_v_varid,north_v))
 CALL check(NF90_PUT_VAR(fid,north_c_varid,north_c))
 CALL check(NF90_PUT_VAR(fid,north_e_varid,north_e))
 CALL check(NF90_PUT_VAR(fid,rot_varid,rot))
 CALL check(NF90_PUT_VAR(fid,h_varid,h))
 CALL check(NF90_PUT_VAR(fid,fsm_varid,fsm))
 CALL check(NF90_PUT_VAR(fid,dum_varid,dum))
 CALL check(NF90_PUT_VAR(fid,dvm_varid,dvm))
 CALL check(NF90_CLOSE(fid))

! Create MPIPOM-TC input ts_initial file

 CALL check(NF90_CREATE(tsinfile,NF90_CLOBBER,fid))
 CALL check(NF90_PUT_ATT(fid,NF90_GLOBAL,"title",TRIM(ADJUSTL(runid))))
 CALL check(NF90_PUT_ATT(fid,NF90_GLOBAL,"description", &
                                         "POM initial condition file"))
 CALL check(NF90_DEF_DIM(fid,"x",im,x_dimid))
 CALL check(NF90_DEF_DIM(fid,"y",jm,y_dimid))
 CALL check(NF90_DEF_DIM(fid,"z",nl,z_dimid))
 CALL check(NF90_DEF_VAR(fid,"z",NF90_DOUBLE,z_dimid,z_varid))
 CALL check(NF90_PUT_ATT(fid,z_varid,"long_name","depth level"))
 CALL check(NF90_PUT_ATT(fid,z_varid,"units","m"))
 CALL check(NF90_DEF_VAR(fid,"t",NF90_DOUBLE, &
                         (/x_dimid,y_dimid,z_dimid/),t_varid))
 CALL check(NF90_PUT_ATT(fid,t_varid,"long_name", &
                                     "potential temperature"))
 CALL check(NF90_PUT_ATT(fid,t_varid,"units","K"))
 CALL check(NF90_DEF_VAR(fid,"s",NF90_DOUBLE, &
                         (/x_dimid,y_dimid,z_dimid/),s_varid))
 CALL check(NF90_PUT_ATT(fid,s_varid,"long_name","salinity"))
 CALL check(NF90_PUT_ATT(fid,s_varid,"units","PSS"))
 CALL check(NF90_ENDDEF(fid))
 CALL check(NF90_PUT_VAR(fid,z_varid,Zp))
 CALL check(NF90_PUT_VAR(fid,t_varid,t))
 CALL check(NF90_PUT_VAR(fid,s_varid,s))
 CALL check(NF90_CLOSE(fid))

! Create MPIPOM-TC input ts_clim file

 CALL check(NF90_CREATE(tsclimfile,NF90_CLOBBER,fid))
 CALL check(NF90_PUT_ATT(fid,NF90_GLOBAL,"title",TRIM(ADJUSTL(runid))))
 CALL check(NF90_PUT_ATT(fid,NF90_GLOBAL,"description", &
                                         "POM initial condition file"))
 CALL check(NF90_DEF_DIM(fid,"x",im,x_dimid))
 CALL check(NF90_DEF_DIM(fid,"y",jm,y_dimid))
 CALL check(NF90_DEF_DIM(fid,"z",nl,z_dimid))
 CALL check(NF90_DEF_VAR(fid,"z",NF90_DOUBLE,z_dimid,z_varid))
 CALL check(NF90_PUT_ATT(fid,z_varid,"long_name","depth level"))
 CALL check(NF90_PUT_ATT(fid,z_varid,"units","m"))
 CALL check(NF90_DEF_VAR(fid,"t",NF90_DOUBLE, &
                         (/x_dimid,y_dimid,z_dimid/),t_varid))
 CALL check(NF90_PUT_ATT(fid,t_varid,"long_name", &
                                     "potential temperature"))
 CALL check(NF90_PUT_ATT(fid,t_varid,"units","K"))
 CALL check(NF90_DEF_VAR(fid,"s",NF90_DOUBLE, &
                         (/x_dimid,y_dimid,z_dimid/),s_varid))
 CALL check(NF90_PUT_ATT(fid,s_varid,"long_name","salinity"))
 CALL check(NF90_PUT_ATT(fid,s_varid,"units","PSS"))
 CALL check(NF90_ENDDEF(fid))
 CALL check(NF90_PUT_VAR(fid,z_varid,Zp))
 CALL check(NF90_PUT_VAR(fid,t_varid,tclim))
 CALL check(NF90_PUT_VAR(fid,s_varid,sclim))
 CALL check(NF90_CLOSE(fid))

! Create MPIPOM-TC input uv_initial file

 CALL check(NF90_CREATE(uvinfile,NF90_CLOBBER,fid))
 CALL check(NF90_PUT_ATT(fid,NF90_GLOBAL,"title",TRIM(ADJUSTL(runid))))
 CALL check(NF90_PUT_ATT(fid,NF90_GLOBAL,"description", &
                                         "POM initial condition file"))
 CALL check(NF90_DEF_DIM(fid,"x",im,x_dimid))
 CALL check(NF90_DEF_DIM(fid,"y",jm,y_dimid))
 CALL check(NF90_DEF_DIM(fid,"z",nl,z_dimid))
 CALL check(NF90_DEF_VAR(fid,"z",NF90_DOUBLE,z_dimid,z_varid))
 CALL check(NF90_PUT_ATT(fid,z_varid,"long_name","depth level"))
 CALL check(NF90_PUT_ATT(fid,z_varid,"units","m"))
 CALL check(NF90_DEF_VAR(fid,"u",NF90_DOUBLE, &
                         (/x_dimid,y_dimid,z_dimid/),u_varid))
 CALL check(NF90_PUT_ATT(fid,u_varid,"long_name","uvel"))
 CALL check(NF90_PUT_ATT(fid,u_varid,"units","m/s"))
 CALL check(NF90_DEF_VAR(fid,"v",NF90_DOUBLE, &
                         (/x_dimid,y_dimid,z_dimid/),v_varid))
 CALL check(NF90_PUT_ATT(fid,v_varid,"long_name","vvel"))
 CALL check(NF90_PUT_ATT(fid,v_varid,"units","m/s"))
 CALL check(NF90_ENDDEF(fid))
 CALL check(NF90_PUT_VAR(fid,z_varid,Zp))
 CALL check(NF90_PUT_VAR(fid,u_varid,u))
 CALL check(NF90_PUT_VAR(fid,v_varid,v))
 CALL check(NF90_CLOSE(fid))

! Create MPIPOM-TC input el_initial file

 CALL check(NF90_CREATE(elinfile,NF90_CLOBBER,fid))
 CALL check(NF90_PUT_ATT(fid,NF90_GLOBAL,"title",TRIM(ADJUSTL(runid))))
 CALL check(NF90_PUT_ATT(fid,NF90_GLOBAL,"description", &
                                         "POM initial condition file"))
 CALL check(NF90_DEF_DIM(fid,"x",im,x_dimid))
 CALL check(NF90_DEF_DIM(fid,"y",jm,y_dimid))
 CALL check(NF90_DEF_VAR(fid,"el",NF90_DOUBLE, &
                         (/x_dimid,y_dimid/),el_varid))
 CALL check(NF90_PUT_ATT(fid,el_varid,"long_name","ssh"))
 CALL check(NF90_PUT_ATT(fid,el_varid,"units","m"))
 CALL check(NF90_ENDDEF(fid))
 CALL check(NF90_PUT_VAR(fid,el_varid,el))
 CALL check(NF90_CLOSE(fid))

END PROGRAM

SUBROUTINE check(index)
USE netcdf
IMPLICIT NONE
 INTEGER, INTENT(IN)                 ::  index
 IF( index /= nf90_noerr) THEN
  PRINT*, nf90_strerror(index)
  STOP
 ENDIF
END SUBROUTINE
