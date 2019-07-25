      SUBROUTINE INITPOST_NMM_BIN_MPIIO
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    INITPOST    INITIALIZE POST FOR RUN
C   PRGRMMR: RUSS TREADON    ORG: W/NP2      DATE: 93-11-10
C     
C ABSTRACT:  THIS ROUTINE INITIALIZES CONSTANTS AND
C   VARIABLES AT THE START OF AN ETA MODEL OR POST 
C   PROCESSOR RUN.
C
C   THIS ROUTINE ASSUMES THAT INTEGERS AND REALS ARE THE SAME SIZE
C   .     
C     
C PROGRAM HISTORY LOG:
C   93-11-10  RUSS TREADON - ADDED DOCBLOC
C   98-05-29  BLACK - CONVERSION OF POST CODE FROM 1-D TO 2-D
C   99-01 20  TUCCILLO - MPI VERSION
C   01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
C   02-06-19  MIKE BALDWIN - WRF VERSION
C   02-08-15  H CHUANG - UNIT CORRECTION AND GENERALIZE PROJECTION OPTIONS
C   02-10-31  H CHUANG - MODIFY TO READ WRF BINARY OUTPUT
C   05-12-05  H CHUANG - ADD CAPABILITY TO OUTPUT OFF-HOUR FORECAST WHICH HAS
c               NO INPACTS ON ON-HOUR FORECAST
C   06-12-07  Y KWON  - MODIFY TO READ, WRITE, AND UPDATE BINARY HWRF DATA FOR 3DVAR
C   07-29-10  M TONG  - MODIFIDED TO READ AETA1 AND AETA2 FOR POST BALANCE
C   08-31-10  M TONG  - MODIFIDED TO OUTPUT UPDATED PINT
C     
C USAGE:    CALL INIT
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - CTLBLK
C                  LOOKUP
C                  SOILDEPTH
C
C    
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
      use vrbls3d
      use vrbls2d
      use soil
      use masks
      use kinds, only             : i_llong
      use module_io_int_idx, only: io_int_index, io_int_loc, r_info
      use mpiio_rw, only: read_data, write_data
C
C     INCLUDE/SET PARAMETERS.
C     
      include 'wrf_io_flags.h'
      INCLUDE "params"
      INCLUDE "parm.tbl"
      INCLUDE "mpif.h"
! This version of INITPOST shows how to initialize, open, read from, and
! close a NetCDF dataset. In order to change it to read an internal (binary)
! dataset, do a global replacement of _ncd_ with _int_. 

      character(len=31) :: VarName
      integer :: Status
      character startdate*19,SysDepInfo*80,cgar*1,inout*160,outfile*160
      character startdate2(19)*4,cfile*160
C 
C     NOTE: SOME INTEGER VARIABLES ARE READ INTO DUMMY ( A REAL ). THIS IS OK
C     AS LONG AS REALS AND INTEGERS ARE THE SAME SIZE.
C
C     ALSO, EXTRACT IS CALLED WITH DUMMY ( A REAL ) EVEN WHEN THE NUMBERS ARE
C     INTEGERS - THIS IS OK AS LONG AS INTEGERS AND REALS ARE THE SAME SIZE.
      LOGICAL RUN,RUNB,RESTRT,SINGLRST
     1,       SIGMA,SUBPOST,NEST,HYDRO
      LOGICAL IOOMG,IOALL
      CHARACTER*32 LABEL
      CHARACTER*40 CONTRL,FILALL,FILMST,FILTMP,FILTKE,FILUNV
     &, FILCLD,FILRAD,FILSFC
      CHARACTER*4 RESTHR
      CHARACTER FNAME*160,ENVAR*50,BLANK*4
      INTEGER IDATB(3),IDATE(8),JDATE(8)
C     
C     INCLUDE COMMON BLOCKS.
C
      INCLUDE "LOOKUP.comm"
      INCLUDE "CTLBLK.comm"
      INCLUDE "GRIDSPEC.comm"
C
C     DECLARE VARIABLES.
C    
      real(4) rad2deg
 
      REAL SLDPTH2(NSOIL)
      REAL RINC(5)
      REAL ETA1(LM), ETA2(LM), AETA1(LM), AETA2(LM)
      REAL NETA1(LM+1), NETA2(LM+1)
      REAL DUM1D (LM+1)
      REAL DUMMY ( IM, JM )
      REAL DUMMY2 ( IM, JM )
      REAL CLON,CLAT
      INTEGER IDUMMY ( IM, JM )
      INTEGER CENLAT,CENLON,TRUELAT1,TRUELAT2
      INTEGER LATSTART,LONSTART,LATLAST,LONLAST
      INTEGER DXVAL, DYVAL

      integer ibuf(im,jsta_2l:jend_2u)
      real buf(im,jsta_2l:jend_2u),bufsoil(im,nsoil,jsta_2l:jend_2u)
     +  ,buf3d(im,jsta_2l:jend_2u,lm),buf3d2(im,jsta_2l:jend_2u,lp1)
      real(4),allocatable::field3(:,:,:),field2(:,:),
     +   field1(:),field31(:,:,:)

      type(r_info), pointer         :: r(:) => NULL()
      integer(kind=mpi_offset_kind) :: pos
      integer                       :: n

      integer nlat_regional, nlon_regional, nsig_regional
C
      DATA BLANK/'    '/
C
C***********************************************************************
C     START INIT HERE.
C
      WRITE(6,*)'INITPOST:  ENTER INITPOST'
      READ(5,111) inout
      READ(5,111) outfile
111   FORMAT(a160)
C     

      

      IF(trim(inout).eq.'relocate' .or. trim(inout).eq.'balance') THEN      !!WRITE BINARY VARIABLE FOR 3DVAR
C     
C     STEP 1.  READ MODEL OUTPUT FILE
C
C
C***
!
! LMH always = LM for sigma-type vert coord
! LMV always = LM for sigma-type vert coord

       do j = jsta_2l, jend_2u
        do i = 1, im
            LMV ( i, j ) = lm
            LMH ( i, j ) = lm
        end do
       end do


! HTM VTM all 1 for sigma-type vert coord

      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            HTM ( i, j, l ) = 1.0
            VTM ( i, j, l ) = 1.0
        end do
       end do
      end do
!
!  The end j row is going to be jend_2u for all variables except for V.
      JS=JSTA_2L
      JE=JEND_2U
      IF (JEND_2U.EQ.JM) THEN
       JEV=JEND_2U+1
      ELSE
       JEV=JEND_2U
      ENDIF
C
c start calling mpi io

!!  YC KWON

      call io_int_index(filename, r, ierr)
      if (ierr /= 0) then
       print*,'Error generating index of: ', trim(filename)
       stop
      end if

      call mpi_file_open(mpi_comm_world, filename
     + , mpi_mode_rdonly,mpi_info_null, iunit, ierr)
      if (ierr /= 0) then
       print*,"Error opening file with mpi io"
       stop
      end if

      call read_data(iunit, r, 'START_DATE', dst=startdate, ierr=ierr)

      jdate=0
      idate=0
      read(startdate,15)iyear,imn,iday,ihrst,imin
 15   format(i4,1x,i2,1x,i2,1x,i2,1x,i2)
      print*,'start yr mo day hr min =',iyear,imn,iday,ihrst,imin
      print*,'processing yr mo day hr min='
     +,idat(3),idat(1),idat(2),idat(4),idat(5)
      idate(1)=iyear
      idate(2)=imn
      idate(3)=iday
      idate(5)=ihrst
      idate(6)=imin
      SDAT(1)=imn
      SDAT(2)=iday
      SDAT(3)=iyear
      jdate(1)=idat(3)
      jdate(2)=idat(1)
      jdate(3)=idat(2)
      jdate(5)=idat(4)
      jdate(6)=idat(5)

      CALL W3DIFDAT(JDATE,IDATE,0,RINC)
      ifhr=nint(rinc(2)+rinc(1)*24.)
      ifmin=nint(rinc(3))
      print*,' in INITPOST ifhr ifmin fileName=',ifhr,ifmin,fileName

      nlon_regional=IM
      nlat_regional=JM
      nsig_regional=LM

C Getting tstart
      tstart=0.
      call read_data(iunit, r, 'TSTART', dst=tstart, ierr=ierr)
      print*,'tstart= ',tstart

C Getiing restart

      IF(tstart .GT. 1.0E-2)THEN
       ifhr=ifhr+NINT(tstart)
       rinc=0
       idate=0
       rinc(2)=-1.0*ifhr
       call w3movdat(rinc,jdate,idate)
       SDAT(1)=idate(2)
       SDAT(2)=idate(3)
       SDAT(3)=idate(1)
       IHRST=idate(5)
       print*,'new forecast hours for restrt run= ',ifhr
       print*,'new start yr mo day hr min =',sdat(3),sdat(1)
     +       ,sdat(2),ihrst,imin
      END IF 

      VarName='SM'      
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        SM=SPVAL
      else        
        pos=pos+(jsta_2l-1)*4*im
	n=im*(jend_2u-jsta_2l+1)
	print*,'SM R offset,length= ',pos,n 
        call read_data(iunit, r, VarName, pos, n, SM, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          SM=SPVAL
        end if 
      end if


      VarName='PD'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        PD=SPVAL
      else
        pos=pos+(jsta_2l-1)*4*im
	n=im*(jend_2u-jsta_2l+1)
	print*,'PD R offset,length= ',pos,n 
        call read_data(iunit, r, VarName, pos, n, PD, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          PD=SPVAL
        end if
      end if
      VarName='T'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        T=SPVAL
      else
        pos=pos+(jsta_2l-1)*4*im*lm
	n=im*(jend_2u-jsta_2l+1)*lm
	print*,'T R offset,length= ',pos,n 
        call read_data(iunit, r, VarName, pos, n, buf3d, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          T=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
!            T ( i, j, l ) = buf3d ( i, ll, j )
             T ( i, j, l ) = buf3d ( i, j,l )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample T= ',
     +         i,j,l,T ( i, j, l )	     
            end do
           end do
          end do 
	end if 
      end if	 
	  

      VarName='Q'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        Q=SPVAL
      else
        pos=pos+(jsta_2l-1)*4*im*lm
	n=im*(jend_2u-jsta_2l+1)*lm
	print*,'Q R offset,length= ',pos,n 
        call read_data(iunit, r, VarName, pos, n, buf3d, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          Q=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
!            Q ( i, j, l ) = buf3d ( i, ll, j )
             Q ( i, j, l ) = buf3d ( i,  j, l )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample Q= ',
     +         i,j,l,Q ( i, j, l )	     
            end do
           end do
          end do 
	end if 
      end if
      

      VarName='U'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        U=SPVAL
      else
        pos=pos+(jsta_2l-1)*4*im*lm
	n=im*(jend_2u-jsta_2l+1)*lm
	print*,'U R offset,length= ',pos,n 
        call read_data(iunit, r, VarName, pos, n, buf3d, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          U=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
!            U ( i, j, l ) = buf3d ( i, ll, j )
             U ( i, j, l ) = buf3d ( i, j, l )
	     UH( i, j, l ) = U( i, j, l )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample U= ',
     +         i,j,l,U ( i, j, l )	     
            end do
           end do
          end do 
	end if 
      end if

      VarName='V'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        V=SPVAL
      else
        pos=pos+(jsta_2l-1)*4*im*lm
	n=im*(jend_2u-jsta_2l+1)*lm
	print*,'V R offset,length= ',pos,n 
        call read_data(iunit, r, VarName, pos, n, buf3d, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          V=SPVAL
        else
	  do l = 1, lm
	   ll=lm-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
!            V ( i, j, l ) = buf3d ( i, ll, j )
             V ( i, j, l ) = buf3d ( i, j, l )
	     VH( i, j, l ) = V( i, j, l )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample V= ',
     +         i,j,l,V ( i, j, l )	     
            end do
           end do
          end do 
	end if 
      end if
      

      varname='AETA1'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        AETA1=SPVAL
      else
        n = lm
        call read_data(iunit, r, VarName, pos, n, AETA1, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          AETA1=SPVAL
        end if
      end if


      varname='ETA1'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ETA1=SPVAL
      else
        n = lm
        call read_data(iunit, r, VarName, pos, n, ETA1, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ETA1=SPVAL
        end if
      end if


      varname='AETA2'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        AETA2=SPVAL
      else
        n = lm
        call read_data(iunit, r, VarName, pos, n, AETA2, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          AETA2=SPVAL
        end if
      end if


      varname='ETA2'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ETA2=SPVAL
      else
        n = lm
        call read_data(iunit, r, VarName, pos, n, ETA2, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ETA2=SPVAL
        end if
      end if
      
      varname='PDTOP'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        PDTOP=SPVAL
      else
        call read_data(iunit, r, VarName, pos, 1, pdtop, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          PDTOP=SPVAL
        end if
      end if

        varname='PT'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        PT=SPVAL
      else
        call read_data(iunit, r, VarName, pos, 1, pt, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          PT=SPVAL
        end if
      end if
      
      print*,'PT, PDTOP= ',PT,PDTOP

      varname='DLMD'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        DLMD=SPVAL
      else
        call read_data(iunit, r, VarName, pos, 1, dlmd, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          DLMD=SPVAL
        end if
      end if

      varname='DPHD'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        DPHD=SPVAL
      else
        call read_data(iunit, r, VarName, pos, 1, dphd, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          DPHD=SPVAL
        end if
      end if

      print*,'DLMD, DPHD= ',DLMD,DPHD

      rad2deg=45./atan(1.)
	
      varname='GLAT'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        GDLAT=SPVAL
      else
        pos=pos+(jsta_2l-1)*4*im
	n=im*(jend_2u-jsta_2l+1)
	print*,'GLAT R offset,length= ',pos,n 
        call read_data(iunit, r, VarName, pos, n, buf, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          GDLAT=SPVAL
        else
          do j = jsta_2l, jend_2u
           do i = 1, im
!             GDLAT(I,J)=buf(I,J)*RTD
             GDLAT(I,J)=buf(I,J)*rad2deg
	     
           enddo
          enddo
        end if
      end if
      
      varname='GLON'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        GDLON=SPVAL
      else
        pos=pos+(jsta_2l-1)*4*im
	n=im*(jend_2u-jsta_2l+1)
	print*,'GLON R offset,length= ',pos,n 
        call read_data(iunit, r, VarName, pos, n, buf, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          GDLON=SPVAL
        else
          do j = jsta_2l, jend_2u
           do i = 1, im
!             GDLON(I,J)=buf(I,J)*RTD
             GDLON(I,J)=buf(I,J)*rad2deg
	     if(i.le.5.and.j.le.5)print*,'GDLAT GDLON in HWRF_BIN='
     +	     ,i,j,GDLAT(I,J),GDLON(I,J)
           enddo
          enddo
        end if
      end if

      IF(trim(infile).ne.'wrfinput_d01')then
      varname='VLAT'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        VDLAT=SPVAL
      else
        pos=pos+(jsta_2l-1)*4*im
        n=im*(jend_2u-jsta_2l+1)
	print*,'VLAT R offset,length= ',pos,n 
        call read_data(iunit, r, VarName, pos, n, buf, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          VDLAT=SPVAL
        else
          do j = jsta_2l, jend_2u
           do i = 1, im
!             VDLAT(I,J)=buf(I,J)*RTD
             VDLAT(I,J)=buf(I,J)
           enddo
          enddo
        end if
      end if

      varname='VLON'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        VDLON=SPVAL
      else
        pos=pos+(jsta_2l-1)*4*im
        n=im*(jend_2u-jsta_2l+1)
	print*,'VLON R offset,length= ',pos,n 
        call read_data(iunit, r, VarName, pos, n, buf, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          VDLON=SPVAL
        else
          do j = jsta_2l, jend_2u
           do i = 1, im
!             VDLON(I,J)=buf(I,J)*RTD
             VDLON(I,J)=buf(I,J)
             if(i.eq.409.and.j.eq.835)print*,'VDLAT VDLON in INITPOST='
     +       ,i,j,VDLAT(I,J),VDLON(I,J)
           enddo
          enddo
        end if
      end if

      endif

      varname='U10'
      print *,'varname ',varname
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        u10=SPVAL
      else
        pos=pos+(jsta_2l-1)*4*im
        n=im*(jend_2u-jsta_2l+1)
	print*,'u10 R offset,length= ',pos,n 
        print *,'index jsta_2l jend_2u  ',index jsta_2l,jend_2u
        call read_data(iunit, r, VarName, pos, n, buf, ierr)
        print *,'after call mpi_file_read_at ',ierr
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          u10=SPVAL
        else
          print *,'u10 = BUF'
          do j = jsta_2l, jend_2u
           do i = 1, im
             u10(I,J)=buf(I,J)
           enddo
          enddo
        end if
      end if

      varname='V10'
      print *,'varname ',varname
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        v10=SPVAL
      else
        pos=pos+(jsta_2l-1)*4*im
        n=im*(jend_2u-jsta_2l+1)
	print*,'v10 R offset,length= ',pos,n 
        print *,'index jsta_2l jend_2u  ',index jsta_2l,jend_2u
        call read_data(iunit, r, VarName, pos, n, buf, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          v10=SPVAL
        else
          print *,'ZNT1 = BUF'
          do j = jsta_2l, jend_2u
           do i = 1, im
             v10(I,J)=sqrt(buf(I,J)**2+u10(I,J)**2)
           enddo
          enddo
        end if
      end if

      varname='ZNT'
      print *,'varname ',varname
      call io_int_loc(VarName, r, pos, n, iret)
      print *,'after retrieve_index for ZNT ',index,iret
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        ZNT1=SPVAL
      else
        pos=pos+(jsta_2l-1)*4*im
        n=im*(jend_2u-jsta_2l+1)
	print*,'ZNT R offset,length= ',pos,n 
        print *,'index jsta_2l jend_2u  ',index jsta_2l,jend_2u
        call read_data(iunit, r, VarName, pos, n, buf, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          ZNT1=SPVAL
        else
          print *,'ZNT1 = BUF'
          do j = jsta_2l, jend_2u
           do i = 1, im
             ZNT1(I,J)=buf(I,J)
           enddo
          enddo
        end if
      end if

      varname='FIS'
      print *,'varname ',varname
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        FIS=SPVAL
      else
        pos=pos+(jsta_2l-1)*4*im
        n=im*(jend_2u-jsta_2l+1)
	print*,'FIS R offset,length= ',pos,n 
       print*, 'FIS debug'
        call read_data(iunit, r, VarName, pos, n, buf, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          FIS=SPVAL
        else
          do j = jsta_2l, jend_2u
           do i = 1, im
             FIS(I,J)=buf(I,J)
             ZINT(I,J,1) = FIS(I,J)/9.8
             FI(I,J,1) = buf(I,J)
           enddo
          enddo
        end if
      end if
      
       if(jsta.le.594.and.jend.ge.594)print*,'gdlon(120,594)= ',
     + gdlon(120,594)
       if(jsta.le.594.and.jend.ge.594)print*,'fis(120,594)= ',
     + znt1(120,594)

      VarName='PINT'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        PINT=SPVAL
      else
        pos=pos+(jsta_2l-1)*4*im*lp1
	n=im*(jend_2u-jsta_2l+1)*lp1
	print*,'PINT R offset,length= ',pos,n 
        call read_data(iunit, r, VarName, pos, n, buf3d2, ierr)
        if (ierr /= 0) then
          print*,"Error reading ", VarName,"Assigned missing values"
          PINT=SPVAL
        else
	  do l = 1, lp1
	   ll=lp1-l+1
           do j = jsta_2l, jend_2u
            do i = 1, im
!            PINT ( i, j, l ) = buf3d2 ( i, ll, j )	
             PINT ( i, j, l ) = buf3d2 ( i, j,l )	
             ALPINT(I,J,L)=ALOG(PINT(I,J,L))     
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'PINT= ',
     +         i,j,l,PINT ( i, j, l )
            end do
           end do
          end do 
	end if 
      end if

!! CALCULATE GEOPOTENTIAL HEIGHT 


          do l = 2, lp1
           do j = jsta_2l, jend_2u
            do i = 1, im
             FI(I,J,2)=T(I,J,L-1)*(Q(I,J,L-1)*0.608+1.0)*287.04*        
     1         (ALOG(PINT(I,J,L-1))-ALOG(PINT(I,J,L)))+FI(I,J,1)
             ZINT(I,J,L) = FI(I,J,2)/9.8
             FI(I,J,1) = FI(I,J,2)
            end do
           end do
          end do

!! CALCULATE CLON CLAT 

!!      CLON=GDLON(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2)
!!      CLAT=GDLAT(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2) 
!!
!! READ domain.center

      read(5,111) cfile
      open(44,FILE=trim(cfile),STATUS='OLD')
      read(44,*) clat
      read(44,*) clon
      close(44)

      print *,'CLON CLAT  ',CLON,CLAT
  
!! MAKE ETA1 ETA2 to LM+1 FOR QL

      DO LL = 1,LM
      NETA1(LL) = ETA1(LL)
      NETA2(LL) = ETA2(LL)
      ENDDO

      NETA1(LM+1) = 0.
      NETA2(LM+1) = 0.

!! WRITE DATA FOR 3DVAR
      open(77,file=trim(outfile),form='unformatted')
      WRITE(77)nlon_regional,nlat_regional,nsig_regional
      WRITE(77)dlmd,dphd,CLON,CLAT
      WRITE(77)pt,pdtop
      WRITE(77)T
      WRITE(77)Q
      WRITE(77)U
      WRITE(77)V
      WRITE(77)ZINT
      IF(trim(infile).eq.'wrfinput_d01')then
        WRITE(77)GDLON,GDLAT
      ELSE
        WRITE(77)GDLON,GDLAT,VDLON,VDLAT
      ENDIF
      WRITE(77)PINT
      WRITE(77)PD
      IF( trim(inout).eq.'balance' )THEN
        WRITE(77)AETA1
      ENDIF
      WRITE(77)NETA1
      IF( trim(inout).eq.'balance' )THEN
        WRITE(77)AETA2
      ENDIF
      WRITE(77)NETA2
      WRITE(77)SM
      WRITE(77)ZNT1
      WRITE(77)v10
      PRINT *,'before close 77'
      CLOSE(77)
      close(78)

       ELSEIF(trim(inout).eq.'update') THEN            !UPDATE HWRF BINARY DATA USING 3DVAR OUPUT
C     
C     STEP 1.  READ MODEL OUTPUT FILE
C
C
C***
!
! LMH always = LM for sigma-type vert coord
! LMV always = LM for sigma-type vert coord

       do j = jsta_2l, jend_2u
        do i = 1, im
            LMV ( i, j ) = lm
            LMH ( i, j ) = lm
        end do
       end do


! HTM VTM all 1 for sigma-type vert coord

      do l = 1, lm
       do j = jsta_2l, jend_2u
        do i = 1, im
            HTM ( i, j, l ) = 1.0
            VTM ( i, j, l ) = 1.0
        end do
       end do
      end do
!
!  The end j row is going to be jend_2u for all variables except for V.
      JS=JSTA_2L
      JE=JEND_2U
      IF (JEND_2U.EQ.JM) THEN
       JEV=JEND_2U+1
      ELSE
       JEV=JEND_2U
      ENDIF
C
c start calling mpi io
      ! Get an index of the file
      call io_int_index(filename, r, ierr)
      if (ierr /= 0) then
       print*,'Error obtinaing index of: ', trim(filename)
       stop
      end if

!!  YC KWON

      call mpi_file_open(mpi_comm_world, filename
     + , mpi_mode_rdwr,mpi_info_null, iunit, ierr)
      if (ierr /= 0) then
       print*,"Error opening file with mpi io"
       stop
      end if

      call read_data(iunit, r, 'START_DATE', dst=startdate, ierr=ierr)
      if (ierr /= 0) then
        print*,"Error reading START_DATE using MPIIO"
      else
        print*,'START_DATE from MPIIO READ= ', startdate
      end if

      jdate=0
      idate=0
      read(startdate,15)iyear,imn,iday,ihrst,imin       
      print*,'start yr mo day hr min =',iyear,imn,iday,ihrst,imin
      print*,'processing yr mo day hr min='
     +,idat(3),idat(1),idat(2),idat(4),idat(5)
      idate(1)=iyear
      idate(2)=imn
      idate(3)=iday
      idate(5)=ihrst
      idate(6)=imin
      SDAT(1)=imn
      SDAT(2)=iday
      SDAT(3)=iyear
      jdate(1)=idat(3)
      jdate(2)=idat(1)
      jdate(3)=idat(2)
      jdate(5)=idat(4)
      jdate(6)=idat(5)

      CALL W3DIFDAT(JDATE,IDATE,0,RINC)
      ifhr=nint(rinc(2)+rinc(1)*24.)
      ifmin=nint(rinc(3))
      print*,' in INITPOST ifhr ifmin fileName=',ifhr,ifmin,fileName

      call io_int_loc('T', r, pos, n, iret)
      if(iret.ne.0) stop

      nlon_regional=IM
      nlat_regional=JM
      nsig_regional=LM

C Getting tstart
      tstart=0.
      call read_data(iunit, r, 'TSTART', dst=tstart, ierr=ierr)
      print*,'tstart= ',tstart
      
C Getiing restart
      
            
      IF(tstart .GT. 1.0E-2)THEN
       ifhr=ifhr+NINT(tstart)
       rinc=0
       idate=0
       rinc(2)=-1.0*ifhr
       call w3movdat(rinc,jdate,idate)
       SDAT(1)=idate(2)
       SDAT(2)=idate(3)
       SDAT(3)=idate(1)
       IHRST=idate(5)       
       print*,'new forecast hours for restrt run= ',ifhr
       print*,'new start yr mo day hr min =',sdat(3),sdat(1)
     +       ,sdat(2),ihrst,imin
      END IF 


      allocate(field2(nlon_regional,nlat_regional))
      allocate(field3(nlon_regional,nlat_regional,nsig_regional))
      allocate(field31(nlon_regional,nlat_regional,nsig_regional+1))

      open(77,file=trim(outfile),form='unformatted')
      READ(77) ! IX,IY,NZ
      READ(77) ! DLMD3,DPHD3
      READ(77) ! PT3,PDTOP3

      READ(77)field3     !T

     
          do l = 1, lm
           do j = jsta_2l, jend_2u
            do i = 1, im
             buf3d ( i, j, l ) = field3 ( i, j, l )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample T= ',
     +         i,j,l,field3 ( i, j, l )	     
            end do
           end do
          end do

      VarName='T'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        buf3d=SPVAL
      else
        pos=pos+(jsta_2l-1)*4*im*lm
        n=im*(jend_2u-jsta_2l+1)*lm
	print*,'T W offset,length= ',pos,n 
        call write_data(iunit, r, VarName, pos, n, buf3d, ierr)
        if (ierr /= 0) then
          print*,"Error writing ", VarName,"Assigned missing values"
        end if
      end if

       read(77)field3   ! Q

          do l = 1, lm
           do j = jsta_2l, jend_2u
            do i = 1, im
             buf3d ( i, j, l ) = field3 ( i, j, l )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample Q= ',
     +         i,j,l,field3 ( i, j, l )	     
            end do
           end do
          end do

      VarName='Q'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        buf3d=SPVAL
      else
        pos=pos+(jsta_2l-1)*4*im*lm
        n=im*(jend_2u-jsta_2l+1)*lm
	print*,'Q W offset,length= ',pos,n 
        call write_data(iunit, r, VarName, pos, n, buf3d, ierr)
        if (ierr /= 0) then
          print*,"Error writing ", VarName,"Assigned missing values"
        end if
      end if
      
       read(77)field3   ! U
       
          do l = 1, lm
           do j = jsta_2l, jend_2u
            do i = 1, im
             buf3d ( i, j, l ) = field3 ( i, j, l )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample U= ',
     +         i,j,l,field3 ( i, j, l )	     
            end do
           end do
          end do

      VarName='U'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        buf3d=SPVAL
      else
        pos=pos+(jsta_2l-1)*4*im*lm
        n=im*(jend_2u-jsta_2l+1)*lm
	print*,'U W offset,length= ',pos,n 
        call write_data(iunit, r, VarName, pos, n, buf3d, ierr)
        if (ierr /= 0) then
          print*,"Error writing ", VarName,"Assigned missing values"
          buf3d=SPVAL
        end if
      end if
        
       read(77)field3   ! V

          do l = 1, lm
           do j = jsta_2l, jend_2u
            do i = 1, im
             buf3d ( i, j, l ) = field3 ( i, j, l )
	     if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample V= ',
     +         i,j,l,field3 ( i, j, l )	     
            end do
           end do
          end do

      VarName='V'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        buf3d=SPVAL
      else
        pos=pos+(jsta_2l-1)*4*im*lm
        n=im*(jend_2u-jsta_2l+1)*lm
	print*,'V W offset,length= ',pos,n 
        call write_data(iunit, r, VarName, pos, n, buf3d, ierr)
        if (ierr /= 0) then
          print*,"Error writing ", VarName,"Assigned missing values"
          buf3d=SPVAL
        end if
      end if

      READ(77)            !Z
      READ(77)            !HLON2,HLAT2
      READ(77) field31    !PINT


          do l = 1, lp1
           do j = jsta_2l, jend_2u
            do i = 1, im
             buf3d2 ( i, j, l ) = field31 ( i, j, l )
             if(i.eq.im/2.and.j.eq.(jsta+jend)/2)print*,'sample PINT= ',
     +         i,j,l,field31 ( i, j, l )
            end do
           end do
          end do


      VarName='PINT'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        buf3d2=SPVAL
      else
        pos=pos+(jsta_2l-1)*4*im*lp1
        n=im*(jend_2u-jsta_2l+1)*lp1
        print*,'PINT W offset,length= ',pos,n
        call write_data(iunit, r, VarName, pos, n, buf3d2, ierr)
        if (ierr /= 0) then
          print*,"Error writing ", VarName,"Assigned missing values"
          buf3d2=SPVAL
        end if
      end if


      READ(77) field2     !PD
      VarName='PD'
      call io_int_loc(VarName, r, pos, n, iret)
      if (iret /= 0) then
        print*,VarName," not found in file-Assigned missing values"
        field2=SPVAL
      else
        pos=pos+(jsta_2l-1)*4*im
        n=im*(jend_2u-jsta_2l+1)
	print*,'PD W offset,length= ',pos,n 
        call write_data(iunit, r, VarName, pos, n, field2, ierr)
        if (ierr /= 0) then
          print*,"Error writing ", VarName,"Assigned missing values"
        end if
      end if

      READ(77)     !ETA1
      READ(77)     !ETA2
      
      close(77)
      print*, '77 closed when updating'
      ENDIF     ! END IF LOOP FOR RELOCATE OR UPDATE
c
      call mpi_file_close(iunit,ierr)
      
      RETURN
      END
