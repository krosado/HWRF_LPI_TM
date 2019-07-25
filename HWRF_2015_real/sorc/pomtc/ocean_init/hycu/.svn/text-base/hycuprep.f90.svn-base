
PROGRAM hycuprep

!$$$  main program documentation block
!
! main program: hycuprep
!   PRGMMR: RICHARD YABLONSKY, URI/GSO, 2014-11-13
!   LANG: FORTRAN 90
! Abstract: Read Global Hycom data (NetCDF) and prepare the
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

 INTEGER, PARAMETER                 :: nx = 1042, ny = 471, nl = 40
 INTEGER, PARAMETER                 :: im = 869, jm = 449, kb = 23
 INTEGER, PARAMETER                 :: ih = 435, jh = 225
 INTEGER                            :: ionedim
 INTEGER                            :: i, j, k, fid, nflg, igfssst
 INTEGER                            :: im1, jm1
 INTEGER                            :: ilonid, ilatid, idepid, isshid
 INTEGER                            :: itmpid, isalid, iuvlid, ivvlid
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
 INTEGER                            :: fval
 INTEGER, DIMENSION(1)              :: ncxstart, ncxcount
 INTEGER, DIMENSION(1)              :: ncystart, ncycount
 INTEGER, DIMENSION(1)              :: nczstart, nczcount
 INTEGER, DIMENSION(2)              :: nc2start, nc2count
 INTEGER, DIMENSION(3)              :: nc3start, nc3count
 REAL                               :: d2r, rearth, blon, blat
 REAL                               :: sfac, ofst
 REAL, DIMENSION(im)                :: xi
 REAL, DIMENSION(jm)                :: yi
 REAL, DIMENSION(kb)                :: z1, z, zz
 REAL, DIMENSION(nx)                :: x0
 REAL, DIMENSION(ny)                :: y0
 REAL, DIMENSION(nl)                :: zhycom, taa, saa, area
 REAL, DIMENSION(ih)                :: xh
 REAL, DIMENSION(jh)                :: yh
 REAL, DIMENSION(im,jm)             :: alon, alat, el
 REAL, DIMENSION(im,jm)             :: dx, dy, rot, h, fsm, dum, dvm
 REAL, DIMENSION(im,jm)             :: east_e, north_e, east_c, north_c
 REAL, DIMENSION(im,jm)             :: east_u, north_u, east_v, north_v
 REAL, DIMENSION(im,jm)             :: stm, yfi, h2
 REAL, DIMENSION(nx,ny)             :: yf!, dpth
 INTEGER, DIMENSION(nx,ny)          :: elev
 REAL, DIMENSION(ih,jh)             :: dpth
 REAL, DIMENSION(im,jm,nl)          :: t, s, uin, vin
 REAL, DIMENSION(im,jm,nl)          :: u, v, tclim, sclim
 INTEGER, DIMENSION(nx,ny,nl)       :: temp, salt, uvel, vvel
 CHARACTER(LEN=120)                 :: runid, gridfile, tsclimfile
 CHARACTER(LEN=120)                 :: tsinfile, uvinfile, elinfile
 CHARACTER(LEN=4)                   :: odata

 READ(5,*) odata ! Region-specific information
 WRITE(6,*) odata

! MPIPOM-TC subset of Global Hycom grid for selected region

 IF (odata == 'hytr') THEN
   ncxstart = (/3269/)
   ncystart = (/1126/)
   nc2start = (/3269, 1126/)
   nc3start = (/3269, 1126, 1/)
   blon =  -98.5
   blat =   10.0
 ELSE IF (odata == 'hyep') THEN
   ncxstart = (/2407/)
   ncystart = (/1063/)
   nc2start = (/2407, 1063/)
   nc3start = (/2407, 1063, 1/)
   blon = -167.5
   blat =    5.0
 ELSE IF (odata == 'hywp') THEN
   ncxstart = (/1208/)
   ncystart = (/1063/)
   nc2start = (/1208, 1063/)
   nc3start = (/1208, 1063, 1/)
   blon =   96.6
   blat =    5.0
 ELSE IF (odata == 'hyni') THEN
   ncxstart = (/404/)
   ncystart = (/1032/)
   nc2start = (/404, 1032/)
   nc3start = (/404, 1032, 1/)
   blon =   32.3
   blat =    2.5
 ELSE IF (odata == 'hysi') THEN
   ncxstart = (/404/)
   ncystart = (/501/)
   nc2start = (/404, 501/)
   nc3start = (/404, 501, 1/)
   blon =   32.3
   blat =  -40.0
 ELSE IF (odata == 'hysw') THEN
   ncxstart = (/1208/)
   ncystart = (/501/)
   nc2start = (/1208, 501/)
   nc3start = (/1208, 501, 1/)
   blon =   96.6
   blat =  -40.0
 ELSE IF (odata == 'hyse') THEN
   ncxstart = (/2253/)
   ncystart = (/501/)
   nc2start = (/2253, 501/)
   nc3start = (/2253, 501, 1/)
   blon = -179.8
   blat =  -40.0
 ELSE IF (odata == 'hysa') THEN
   ncxstart = (/3457/)
   ncystart = (/501/)
   nc2start = (/3457, 501/)
   nc3start = (/3457, 501, 1/)
   blon =  -83.5
   blat =  -40.0
 ELSE
   WRITE(6,*) 'Bad region selection... stopping now.'
   STOP
 END IF
 ncxcount = (/1042/)
 ncycount = (/471/)
 nczstart = (/1/)
 nczcount = (/40/)
 nc2count = (/1042, 471/)
 nc3count = (/1042, 471, 40/)

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

! Define alon and alat

 DO j = 1,jm
   DO i = 1,im
     alon(i,j) = blon+(i-1)*83.2/(im-1)
     alat(i,j) = blat+(j-1)*37.5/(jm-1)
   END DO
 END DO

! Read Global Hycom data (NetCDF)

 CALL check(NF90_OPEN('tin.nc',NF90_NOWRITE,fid))
 CALL check(NF90_INQ_VARID(fid,"lon",ilonid))
 CALL check(NF90_GET_VAR(fid,ilonid,x0,ncxstart,ncxcount))
 CALL check(NF90_INQ_VARID(fid,"lat",ilatid))
 CALL check(NF90_GET_VAR(fid,ilatid,y0,ncystart,ncycount))
 CALL check(NF90_INQ_VARID(fid,"depth",idepid))
 CALL check(NF90_GET_VAR(fid,idepid,zhycom,nczstart,nczcount))
 CALL check(NF90_INQ_VARID(fid,"water_temp",itmpid))
 CALL check(NF90_GET_VAR(fid,itmpid,temp,nc3start,nc3count))

 CALL check(NF90_OPEN('sin.nc',NF90_NOWRITE,fid))
 CALL check(NF90_INQ_VARID(fid,"salinity",isalid))
 CALL check(NF90_GET_VAR(fid,isalid,salt,nc3start,nc3count))

 CALL check(NF90_OPEN('uin.nc',NF90_NOWRITE,fid))
 CALL check(NF90_INQ_VARID(fid,"water_u",iuvlid))
 CALL check(NF90_GET_VAR(fid,iuvlid,uvel,nc3start,nc3count))

 CALL check(NF90_OPEN('vin.nc',NF90_NOWRITE,fid))
 CALL check(NF90_INQ_VARID(fid,"water_v",ivvlid))
 CALL check(NF90_GET_VAR(fid,ivvlid,vvel,nc3start,nc3count))

 CALL check(NF90_OPEN('elin.nc',NF90_NOWRITE,fid))
 CALL check(NF90_INQ_VARID(fid,"surf_el",isshid))
 CALL check(NF90_GET_VAR(fid,isshid,elev,nc2start,nc2count))

! Print points where temp, salt, uvel, or vvel fill points differ

 DO k = 1,nl
   DO j = 1,ny
     DO i = 1,nx
       IF (ABS(temp(i,j,k)-salt(i,j,k)) > 50000 .OR. &
           ABS(temp(i,j,k)-uvel(i,j,k)) > 50000 .OR. &
           ABS(temp(i,j,k)-vvel(i,j,k)) > 50000) THEN
         PRINT*, 'i=',i,', j=',j,', k=',k
         PRINT*, 't0=',temp(i,j,k),', s0=',salt(i,j,k)
         PRINT*, 'u0=',uvel(i,j,k),', v0=',vvel(i,j,k)
         STOP
       END IF
     END DO
   END DO
 END DO

! Set scale factor, offset, and fill value based on Global Hycom data

 sfac=0.001
 ofst=20.
 fval=-30000

! Interpolate t, s, u, v, and el onto 1/12-deg MPIPOM-TC grid

 DO i = 1,im
   xi(i) = alon(i,1)
 END DO

 DO j = 1,jm
   yi(j) = alat(1,j)
 END DO

  IF (odata == 'hytr' .OR. odata == 'hyep' .OR. &
      odata == 'hyse' .OR. odata == 'hysa') THEN
 DO i = 1,nx
   x0(i) = x0(i)-360.
 END DO
  END IF

 DO k = 1,nl
   DO j = 1,ny
     DO i = 1,nx
       IF (temp(i,j,k) == fval) THEN
         yf(i,j) = temp(i,j,k)
       ELSE
         yf(i,j) = temp(i,j,k)*sfac+ofst
       END IF
       IF (yf(i,j) < -9. .AND. yf(i,j) /= float(fval)) THEN
         PRINT*, 'yf(',i,',',j,') = ',yf(i,j) 
         STOP
       END IF
     END DO
   END DO
   call horinterp(float(fval),im,jm,nx,ny,x0,y0,xi,yi,yf,yfi)
   DO j = 1,jm
     DO i = 1,im
       IF (yfi(i,j) < -9. .AND. yfi(i,j) /= float(fval)) THEN
         PRINT*, 'yfi(',i,',',j,') = ',yfi(i,j)
         STOP
       END IF
       t(i,j,k) = yfi(i,j)
     END DO
   END DO
 END DO

 DO k = 1,nl
   DO j = 1,ny
     DO i = 1,nx
       IF (salt(i,j,k) == fval) THEN
         yf(i,j) = salt(i,j,k)
       ELSE
         yf(i,j) = salt(i,j,k)*sfac+ofst
       END IF
       IF (yf(i,j) < -9. .AND. yf(i,j) /= float(fval)) THEN
         PRINT*, 'yf(',i,',',j,') = ',yf(i,j)
         STOP
       END IF
     END DO
   END DO
   call horinterp(float(fval),im,jm,nx,ny,x0,y0,xi,yi,yf,yfi)
   DO j = 1,jm
     DO i = 1,im
       IF (yfi(i,j) < -9. .AND. yfi(i,j) /= float(fval)) THEN
         PRINT*, 'yfi(',i,',',j,') = ',yfi(i,j)
         STOP
       END IF
       s(i,j,k) = yfi(i,j)
     END DO
   END DO
 END DO

 DO k = 1,nl
   DO j = 1,ny
     DO i = 1,nx
       IF (uvel(i,j,k) == fval) THEN
         yf(i,j) = uvel(i,j,k)
       ELSE
         yf(i,j) = uvel(i,j,k)*sfac
       END IF
       IF (yf(i,j) < -9. .AND. yf(i,j) /= float(fval)) THEN
         PRINT*, 'yf(',i,',',j,') = ',yf(i,j)
         STOP
       END IF
     END DO
   END DO
   call horinterp(float(fval),im,jm,nx,ny,x0,y0,xi,yi,yf,yfi)
   DO j = 1,jm
     DO i = 1,im
       IF (yfi(i,j) < -9. .AND. yfi(i,j) /= float(fval)) THEN
         PRINT*, 'yfi(',i,',',j,') = ',yfi(i,j)
         STOP
       END IF
       uin(i,j,k) = yfi(i,j)
     END DO
   END DO
 END DO

 DO k = 1,nl
   DO j = 1,ny
     DO i = 1,nx
       IF (vvel(i,j,k) == fval) THEN
         yf(i,j) = vvel(i,j,k)
       ELSE
         yf(i,j) = vvel(i,j,k)*sfac
       END IF
       IF (yf(i,j) < -9. .AND. yf(i,j) /= float(fval)) THEN
         PRINT*, 'yf(',i,',',j,') = ',yf(i,j)
         STOP
       END IF
     END DO
   END DO
   call horinterp(float(fval),im,jm,nx,ny,x0,y0,xi,yi,yf,yfi)
   DO j = 1,jm
     DO i = 1,im
       IF (yfi(i,j) < -9. .AND. yfi(i,j) /= float(fval)) THEN
         PRINT*, 'yfi(',i,',',j,') = ',yfi(i,j)
         STOP
       END IF
       vin(i,j,k) = yfi(i,j)
     END DO
   END DO
 END DO

   DO j = 1,ny
     DO i = 1,nx
       IF (elev(i,j) == fval) THEN
         yf(i,j) = elev(i,j)
       ELSE
         yf(i,j) = elev(i,j)*sfac
       END IF
       IF (yf(i,j) < -9. .AND. yf(i,j) /= float(fval)) THEN
         PRINT*, 'yf(',i,',',j,') = ',yf(i,j)
         STOP
       END IF
     END DO
   END DO
   call horinterp(float(fval),im,jm,nx,ny,x0,y0,xi,yi,yf,yfi)
   DO j = 1,jm
     DO i = 1,im
       IF (yfi(i,j) < -9. .AND. yfi(i,j) /= float(fval)) THEN
         PRINT*, 'yfi(',i,',',j,') = ',yfi(i,j)
         STOP
       END IF
       el(i,j) = yfi(i,j)
     END DO
   END DO

! Print points where t, s, uin, or vin fill points differ

 DO k = 1,nl
   DO j = 1,jm
     DO i = 1,im
       IF (ABS(t(i,j,k)-s(i,j,k)) > 50. .OR. &
           ABS(t(i,j,k)-uin(i,j,k)) > 50. .OR. &
           ABS(t(i,j,k)-vin(i,j,k)) > 50. .OR. &
           ABS(t(i,j,1)-el(i,j)) > 50.) THEN
         PRINT*, 'i=',i,', j=',j,', k=',k
         PRINT*, 'ti=',t(i,j,k),', si=',s(i,j,k)
         PRINT*, 'ui=',uin(i,j,k),', vi=',vin(i,j,k)
         PRINT*, 'eli=',el(i,j)
         STOP
       END IF
     END DO
   END DO
 END DO

! Set uin, vin, and el to 0
!
! DO k = 1,nl
!   DO j = 1,jm
!     DO i = 1,im
!       uin(i,j,k) = 0. ! calculated as geostrophic in old POM-TC
!       vin(i,j,k) = 0. ! calculated as geostrophic in old POM-TC
!     END DO
!   END DO
! END DO
!
! DO j = 1,jm
!   DO i = 1,im
!     el(i,j) = 0.
!   END DO
! END DO

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

 DO j = 1,jm
   DO i = 2,im-1
     dx(i,j) = 0.5*d2r*rearth*sqrt(((east_e(i+1,j)-east_e(i-1,j))* &
       cos(north_e(i,j)*d2r))**2.+(north_e(i+1,j)-north_e(i-1,j))**2.)
   END DO
   dx(1,j) = dx(2,j)
   dx(im,j) = dx(im-1,j)
 END DO

 DO i = 1,im
   DO j = 2,jm-1
     dy(i,j) = 0.5*d2r*rearth*sqrt(((east_e(i,j+1)-east_e(i,j-1))* &
       cos(north_e(i,j)*d2r))**2.+(north_e(i,j+1)-north_e(i,j-1))**2.)
   END DO
   dy(i,1) = dy(i,2)
   dy(i,jm) = dy(i,jm-1)
 END DO

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

! MPIPOM-TC bottom topography based on hycom

 DO j = 1,jm
   DO i = 1,im
     DO k = 2,nl
       IF (t(i,j,k) < -9. .OR. s(i,j,k) < -9.) THEN
         h(i,j) = zhycom(k-1)
         EXIT
       ELSE
         h(i,j) = zhycom(k)
       END IF
     END DO
   END DO
 END DO

! Remove hycom-based bottom topography via neutral profile generation

 DO j = 1,jm
   DO i = 1,im
     nflg = 0
     DO k = 2,nl
       IF (t(i,j,k) < -9. .OR. s(i,j,k) < -9. .OR. nflg .EQ. 1) THEN
         t(i,j,k) = t(i,j,k-1)
         s(i,j,k) = s(i,j,k-1)
         uin(i,j,k) = uin(i,j,k-1)
         vin(i,j,k) = vin(i,j,k-1)
         nflg = 1
       END IF
     END DO
   END DO
 END DO

! Read MPIPOM-TC bottom topography based on etopo5

 read(66) dpth

! Interpolate etopo5-based topography onto 1/12-deg MPIPOM-TC grid

 DO i = 1,ih
   xh(i) = blon+(i-1)*83.2/(ih-1)
 END DO
 DO j = 1,jh
   yh(j) = blat+(j-1)*37.5/(jh-1)
 END DO

 call horinterp(0.0,im,jm,ih,jh,xh,yh,xi,yi,dpth,h2)

! Define bottom topography based on shallower of the two options?
! No, redefine bottom topography based on etopo5-based topography

 DO j = 1,jm
   DO i = 1,im
!     IF (h2(i,j) < h(i,j)) h(i,j) = h2(i,j)
     h(i,j) = h2(i,j)
   END DO
 END DO

! Restrict bottom topography to 5000-m depth

 DO j = 1,jm
   DO i = 1,im
     IF (h(i,j) > 5000.) h(i,j) = 5000.
   END DO
 END DO

! Restrict shallowest topography to 11-m depth

 DO j = 1,jm
   DO i = 1,im
     IF (h(i,j) < 11.) h(i,j) = 1.
   END DO
 END DO

! MPIPOM-TC land/sea mask

 DO j = 1,jm
   DO i = 1,im
     IF (h(i,j) > 1. .AND. t(i,j,1) > -9.) THEN
       fsm(i,j) = 1.
       dum(i,j) = 1.
       dvm(i,j) = 1.
     ELSE
       fsm(i,j) = 0.
       dum(i,j) = 0.
       dvm(i,j) = 0.
       h(i,j) = 1.
     END IF
   END DO
 END DO

  IF (odata == 'hyep') THEN

! MPIPOM-TC Caribbean and Gulf of Mexico removal

 DO j = 120,jm
   DO i = 810,im
     fsm(i,j) = 0.
     dum(i,j) = 0.
     dvm(i,j) = 0.
     h(i,j) = 1.
   END DO
 END DO

 DO j = 145,jm
   DO i = 715,im
     fsm(i,j) = 0.
     dum(i,j) = 0.
     dvm(i,j) = 0.
     h(i,j) = 1.
   END DO
 END DO

  END IF

  IF (odata == 'hysa') THEN

! MPIPOM-TC Southeast Pacific removal

 DO j = 1,jm
   DO i = 1,194
     fsm(i,j) = 0.
     dum(i,j) = 0.
     dvm(i,j) = 0.
     h(i,j) = 1.
   END DO
 END DO

  END IF

  IF (ionedim.ne.1) THEN

! MPIPOM-TC land/sea mask modification

 DO j = 1,jm-1
   DO i = 1,im
     IF (fsm(i,j) == 0. .AND. fsm(i,j+1) /= 0.) dvm(i,j+1) = 0.
   END DO
 END DO

 DO j = 1,jm
   DO i = 1,im-1
     IF (fsm(i,j) == 0. .AND. fsm(i+1,j) /= 0.) dum(i+1,j) = 0. 
   END DO
 END DO

  END IF

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
     call mixsstz(t,stm,h,fsm,zhycom,im,jm,nl,0)
     print*, '... mixsstz assimilated gfs sst'
 END IF

! MPIPOM-TC temperature and salinity modification

 DO k = 1,nl
   DO j = 1,jm
     DO i = 1,im
       t(i,j,k) = t(i,j,k)*fsm(i,j)
       s(i,j,k) = s(i,j,k)*fsm(i,j)
       IF (t(i,j,k) < -9. .OR. s(i,j,k) < -9.) THEN
          t(i,j,k) = 0.
          s(i,j,k) = 0.
          uin(i,j,k) = 0.
          vin(i,j,k) = 0.
          PRINT*, 'Bathymetry problem at i=',i,', j=',j,', k=',k
       END IF
     END DO
   END DO
 END DO

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
 CALL check(NF90_PUT_VAR(fid,z_varid,zhycom))
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
 CALL check(NF90_PUT_VAR(fid,z_varid,zhycom))
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
 CALL check(NF90_PUT_VAR(fid,z_varid,zhycom))
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
