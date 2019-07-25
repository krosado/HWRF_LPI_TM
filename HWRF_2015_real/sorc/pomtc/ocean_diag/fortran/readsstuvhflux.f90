
PROGRAM sstuvhflux

!$$$  main program documentation block
!
! main program: sstuvhflux
!   PRGMMR: RICHARD YABLONSKY, URI/GSO, 2014-12-02
!   LANG: FORTRAN 90
! Abstract: Read 1.5-hourly SST from MPIPOM-TC output (NetCDF) and
! calculate average, minimum, and maximum SST within 60, 100, 150, and
! 200 km radius around the storm center, as well as the SST difference
! from both phase 1 and the end of phase 2 (start of coupled forecast).
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

 INTEGER, PARAMETER                 :: im = 869, jm = 449
 INTEGER                            :: i, j, k, fid, n, nf, nprinto2
 INTEGER                            :: ilonid, ilatid, isstid
 INTEGER                            :: wrad, fhr1, fhr2
 REAL                               :: d2r, rearth, tavr, counter
 REAL                               :: tavrd1, tavrd2
 REAL                               :: avgsst, avgsstd1, avgsstd2
 REAL                               :: minsst, minsstd1, minsstd2
 REAL                               :: maxsst, maxsstd1, maxsstd2
 REAL                               :: xcen, ycen, fhr, dx, dy, r
 REAL                               :: xcen1, ycen1, xcen2, ycen2
 REAL, DIMENSION(4)                 :: dist
 REAL, DIMENSION(im,jm)             :: alon, alat, sst, sst0ph1, sst0cpl
 CHARACTER(LEN=18)                  :: sstinfile
 CHARACTER(LEN=12)                  :: sstoutfile
 CHARACTER(LEN=29)                  :: junk1
 CHARACTER(LEN=1)                   :: ns, ew, junk2, junk3
 CHARACTER(LEN=16)                  :: junk4
 CHARACTER(LEN=190)                 :: junk5

 dist(1) = 60000.
 dist(2) = 100000.
 dist(3) = 150000.
 dist(4) = 200000.

 DO n = 1,4

! Open file for writing table

 WRITE(sstoutfile,'(''sst'',i3.3,''km.dat'')') int(dist(n)/1000.)
 OPEN(9,FILE=sstoutfile)
 
! Write header information to the table
 WRITE(9,100) "%FHOUR     LON    LAT AVGSST MINSST MAXSST &
 AVGSSTD1 MINSSTD1 MAXSSTD1 AVGSSTD2 MINSSTD2 MAXSSTD2"
 100 FORMAT(a96)

! Read SST data from phase 1 hour 0 MPIPOM-TC output file (NetCDF)

 WRITE(sstinfile,'(''../pom/output/OCEAN/PHASE1/sstuvhflux.'', &
                   i4.4,''.nc'')') 0
 CALL check(NF90_OPEN(sstinfile,NF90_NOWRITE,fid))
 CALL check(NF90_INQ_VARID(fid,"sst",isstid))
 CALL check(NF90_GET_VAR(fid,isstid,sst0ph1))
 CALL check(NF90_CLOSE(fid))

! Read SST data from coupled hour 0 MPIPOM-TC output file (NetCDF)

 WRITE(sstinfile,'(''sstuvhflux.'',i4.4,''.nc'')') 0
 CALL check(NF90_OPEN(sstinfile,NF90_NOWRITE,fid))
 CALL check(NF90_INQ_VARID(fid,"sst",isstid))
 CALL check(NF90_GET_VAR(fid,isstid,sst0cpl))
 CALL check(NF90_CLOSE(fid))

 OPEN(10)
 DO nf = 1,85

! Read SST data from 1.5-hourly MPIPOM-TC output file (NetCDF)

 nprinto2 = nf-1
 WRITE(sstinfile,'(''sstuvhflux.'',i4.4,''.nc'')') nprinto2
 CALL check(NF90_OPEN(sstinfile,NF90_NOWRITE,fid))
 CALL check(NF90_INQ_VARID(fid,"sst",isstid))
 CALL check(NF90_GET_VAR(fid,isstid,sst))
 CALL check(NF90_INQ_VARID(fid,"east_e",ilonid))
 CALL check(NF90_GET_VAR(fid,ilonid,alon))
 CALL check(NF90_INQ_VARID(fid,"north_e",ilatid))
 CALL check(NF90_GET_VAR(fid,ilatid,alat))
 CALL check(NF90_CLOSE(fid))

! Read storm center lat/lon from 3-hourly track file (fort.10)

 101 FORMAT(a29,i4,a1,f4.0,a1,a1,f5.0,a1,a16,i4,a190)

 IF(nprinto2.eq.0) THEN

 wrad = 0
 fhr1 = 1
 DO WHILE(wrad.ne.34.or.MOD(fhr1,3).ne.0)
 READ(10,101) junk1,fhr1,junk2,ycen1,ns,junk3,xcen1,ew,junk4,wrad,junk5
 xcen1 = xcen1/10.
 ycen1 = ycen1/10.
 IF(ew.eq.'W') xcen1 = -xcen1
 IF(ns.eq.'S') ycen1 = -ycen1
 END DO

 wrad = 0
 fhr2 = 1
 DO WHILE(wrad.ne.34.or.MOD(fhr2,3).ne.0)
 READ(10,101) junk1,fhr2,junk2,ycen2,ns,junk3,xcen2,ew,junk4,wrad,junk5
 xcen2 = xcen2/10.
 ycen2 = ycen2/10.
 IF(ew.eq.'W') xcen2 = -xcen2
 IF(ns.eq.'S') ycen2 = -ycen2
 END DO

 fhr = fhr1
 xcen = xcen1
 ycen = ycen1

 ELSE IF(MOD(nprinto2,2).eq.0) THEN

 fhr1 = fhr2
 xcen1 = xcen2
 ycen1 = ycen2

 wrad = 0
 fhr2 = 1
 DO WHILE((wrad.ne.34.or.MOD(fhr2,3).ne.0).and.fhr1.lt.126)
 READ(10,101) junk1,fhr2,junk2,ycen2,ns,junk3,xcen2,ew,junk4,wrad,junk5
 xcen2 = xcen2/10.
 ycen2 = ycen2/10.
 IF(ew.eq.'W') xcen2 = -xcen2
 IF(ns.eq.'S') ycen2 = -ycen2
 END DO

 fhr = fhr1
 xcen = xcen1
 ycen = ycen1

 ELSE

 fhr = 0.5*(fhr1+fhr2)
 xcen = 0.5*(xcen1+xcen2)
 ycen = 0.5*(ycen1+ycen2)

 END IF

! Write data to the table

 rearth = 6371000.
 d2r = 3.141592654/180.
 tavr = 0.
 tavrd1 = 0.
 tavrd2 = 0.
 maxsst = 0.
 maxsstd1 = 0.
 maxsstd2 = 0.
 minsst = 100.
 minsstd1 = 0.
 minsstd2 = 0.
 counter = 0.
 DO j = 1,jm
   DO i = 1,im
     dx = rearth*(alon(i,j)-xcen)*d2r*cos(ycen*d2r)
     dy = rearth*(alat(i,j)-ycen)*d2r
     r = sqrt(dx**2.+dy**2.)
     IF(r.lt.dist(n).and.sst(i,j).ne.0.) THEN
       tavr = tavr+sst(i,j)
       tavrd1 = tavrd1+sst(i,j)-sst0ph1(i,j)
       tavrd2 = tavrd2+sst(i,j)-sst0cpl(i,j)
       maxsst = max(sst(i,j),maxsst)
       maxsstd1 = max(sst(i,j)-sst0ph1(i,j),maxsstd1)
       maxsstd2 = max(sst(i,j)-sst0cpl(i,j),maxsstd2)
       minsst = min(sst(i,j),minsst)
       minsstd1 = min(sst(i,j)-sst0ph1(i,j),minsstd1)
       minsstd2 = min(sst(i,j)-sst0cpl(i,j),minsstd2)
       counter = counter+1.
     END IF
   END DO
 END DO
 IF(counter.gt.0.) THEN
   avgsst = tavr/counter
   avgsstd1 = tavrd1/counter
   avgsstd2 = tavrd2/counter
 ELSE
   avgsst = 0.
   avgsstd1 = 0.
   avgsstd2 = 0.
 END IF

 WRITE(9,102) fhr,xcen,ycen,avgsst,minsst,maxsst, &
              avgsstd1,minsstd1,maxsstd1,avgsstd2,minsstd2,maxsstd2
 102 FORMAT(f6.1,f8.2,f7.2,3f7.2,6f9.2)

 END DO
 CLOSE(10)

 END DO

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

