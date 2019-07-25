module gradsmod
!$$$   module documentation block
! This module contains routines and information useful for plotting grads maps
!   of the guess and analysis on the analysis grid.
!
! Subroutines Included:
!   sub init_grads - initialize internal variables prior to opening output
!                      files for grads output
!   sub open_grads - open output files and initialize for writing grads output
!   sub out_grads  - write next output grads record
!   sub close_grads- close grads output files
!
! Variable Definitions:
!   def make_maps - if = .true., then generate grads output of guess and
!                            analysis on analysis grid
!$$$ end documentation block

  implicit none

  logical make_maps
  character(80) label_dat(20)
  integer unit_dat(20)
  integer num_open

contains

  subroutine init_grads

    integer i

    make_maps=.false.
    do i=1,20
     label_dat(i)='xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
     unit_dat(i)=0
    end do
    num_open=0

  end subroutine init_grads

  subroutine open_grads(label,nlon,nlat,nsig)

    character(*) label
    integer nlon,nlat,nsig
    integer i,ithis,k,ntime
    real(4) undef
    character(1) blank
    character(80) datdes(31)
    integer unit_des
    character(80) label_des
    real(4) startx,starty,startp,xinc,yinc,pinc

    blank=' '
    undef=-9.99e33

    ithis=num_open+1

!    create names of grads control and data files

    write(label_des,'(a,".ctl")')trim(label)
    write(label_dat(ithis),'(a,".dat")')trim(label)

!    find unused unit number

    unit_des=60
    unit_dat(ithis)=60+ithis

!    initialize counters for this set of output fields

    startx=1. ; xinc=1.
    starty=1. ; yinc=1.
    startp=1. ; pinc=1.
    ntime=1
    do i=1,15
     write(datdes(i),'(80a1)')(blank,k=1,80)
    end do
    write(datdes(1),'("DSET ^",a)')trim(label_dat(ithis))
    write(datdes(2),'("options big_endian sequential")')
    write(datdes(3),'("TITLE ",a)')trim(label)
    write(datdes(4),'("UNDEF ",e11.2)')undef
    write(datdes(5),'("XDEF ",i5," LINEAR ",f7.2,f7.2)')nlon,startx,xinc
    write(datdes(6),'("YDEF ",i5," LINEAR ",f7.2,f7.2)')nlat,starty,yinc
    write(datdes(7),'("ZDEF ",i5," LINEAR ",f7.2,f7.2)')nsig,startp,pinc
    write(datdes(8),'("TDEF ",i5," LINEAR 0Z23may1992 24hr")')ntime
    write(datdes(9),'("VARS 21")')
    write(datdes(10),'("pint ",i5," 99 pint ")')nsig+1
    write(datdes(11),'("pd   ",i5," 99 pd   ")')1
    write(datdes(12),'("ps   ",i5," 99 ps   ")')1
    write(datdes(13),'("fis  ",i5," 99 fis  ")')1
    write(datdes(14),'("t    ",i5," 99 t    ")')nsig
    write(datdes(15),'("q    ",i5," 99 q    ")')nsig
    write(datdes(16),'("geoph",i5," 99 geoph")')nsig
    write(datdes(17),'("u    ",i5," 99 u    ")')nsig
    write(datdes(18),'("v    ",i5," 99 v    ")')nsig
    write(datdes(19),'("w    ",i5," 99 w    ")')nsig+1
    write(datdes(20),'("sm   ",i5," 99 sm   ")')1
    write(datdes(21),'("sice ",i5," 99 sice ")')1
    write(datdes(22),'("sst  ",i5," 99 sst  ")')1
    write(datdes(23),'("vgty ",i5," 99 vgty ")')1
    write(datdes(24),'("slty ",i5," 99 slty ")')1
    write(datdes(25),'("vgfr ",i5," 99 vgfr ")')1
    write(datdes(26),'("sno  ",i5," 99 sno  ")')1
    write(datdes(27),'("u10  ",i5," 99 u10  ")')1
    write(datdes(28),'("v10  ",i5," 99 v10  ")')1
    write(datdes(29),'("smc  ",i5," 99 smc  ")')1
    write(datdes(30),'("stc  ",i5," 99 stc  ")')1
!   write(datdes(31),'("tsk  ",i5," 99 tsk  ")')1
    write(datdes(31),'("ENDVARS")')

!   write out datdes

    write(*,'(a80)')datdes(24)

    open(unit_des,file=label_des,form='formatted')
    rewind unit_des
    write(unit_des,'(a80)')datdes
    close(unit_des)

!   open dat file

    open(unit_dat(ithis),file=label_dat(ithis),form='unformatted')
    rewind unit_dat(ithis)

!    advance num_open

    num_open=num_open+1

  end subroutine open_grads

  subroutine out_grads(tempa,label,nlon,nlat)

    integer nlon,nlat
    real(4) tempa(nlon,nlat)
    character(*) label
    character(80) label_test
    integer i,ithis


!  find output unit number

    write(label_test,'(a,".dat")')trim(label)
    ithis=0
    if(num_open.gt.0) then
     do i=1,num_open
      if(trim(label_dat(i)).eq.trim(label_test)) then
       ithis=i
       exit
      end if
     end do
    end if
    if(ithis.gt.0) write(unit_dat(ithis))tempa

  end subroutine out_grads

  subroutine close_grads(label)

    character(*) label
    character(80) label_test
    integer i,ithis

!   close dat file

    write(label_test,'(a,".dat")')trim(label)
    ithis=0
    if(num_open.gt.0) then
     do i=1,num_open
      if(trim(label_dat(i)).eq.trim(label_test)) then
       ithis=i
       exit
      end if
     end do
    end if
    if(ithis.gt.0) then
     close(unit_dat(ithis))
     label_dat(ithis)='xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
    end if

  end subroutine close_grads

end module gradsmod

program reg_gsi_grads

!   create grads files from regional mode input/output of unified gsi

  use gradsmod
  use wrf_data
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'

  character(len=19)  :: DateStr1
  character(len=31)  :: VarName
  integer            :: dh1

  integer :: ndim1
  integer :: WrfType
  integer, dimension(4)  :: start_index, end_index, start_index1, end_index1
  character (len=31) :: rmse_var
  character (len= 4) :: staggering
  character (len= 3) :: ordering
  character (len=24) :: start_date,start_date1

  character (len=80), dimension(3)  ::  dimnames
  character (len=80) :: SysDepInfo
  integer :: ierr, Status, Status_next_time

  integer regional_time(6)
  integer nlon_regional,nlat_regional,nsig
  real(4) dlmd,dphd,pt,pdtop
  real(4),allocatable:: deta1(:),aeta1(:),eta1(:),deta2(:),aeta2(:),eta2(:)
  real(4),allocatable:: glat(:,:),glon(:,:)
  character(120) input_file,outname
  real(4),allocatable::temp1(:,:),tempb(:,:),temp3(:,:,:),temp4(:,:,:),psfc_this(:,:)
  integer,allocatable::itemp1(:,:)
  integer im,jm,igtypeh,igtypev,i,j,k
  integer iyear,imonth,iday,ihour,iminute,isecond
  integer jyear,jmonth,jday,jhour,jminute,jsecond
  integer nlon,nlat,nsoil
  integer im_tempb
  real,parameter ::  r0_01 = 0.01, one_tenth = 0.10, ten=10.0, one=1.0
  real,parameter ::  grav=9.81, rd=287.04, rv=4.6150e+2
  real :: dz, h, rdog, fv, pdtop_ll, pt_ll
  real,allocatable:: aeta1_ll(:),eta1_ll(:),aeta2_ll(:),eta2_ll(:)
  real,allocatable:: height(:)
  real,allocatable:: gz(:,:)
  real,allocatable:: tsen(:,:,:),tv(:,:,:),prsl(:,:,:),prsi(:,:,:),geop_hgti(:,:,:)

  
  fv=rv/rd-1.0
  igtypeh=1
  igtypev=2
  call init_grads
  
!   get file name from input argument

  call getarg(1,input_file)
  call getarg(2,outname)

  call ext_ncd_ioinit(SysDepInfo,status)
  call set_wrf_debug_level ( 1 )

  call ext_ncd_open_for_read( trim(input_file), 0, 0, "", dh1, Status)
  if ( Status /= 0 )then
       print*,'error opening',trim(input_file), ' Status = ', Status
       print*
       print*,'USAGE: diffwrf [opcode] [file1] [file2]'
       print*
       stop
  endif

!-------------  get date info

  call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
  read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)')iyear,imonth,iday,ihour,iminute,isecond
!        write(6,*)'convert_netcdf_nmm_ens:
!        iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond

!-------------  get grid info
  rmse_var='T'
  call ext_ncd_get_var_info (dh1,rmse_var,ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       nlon=end_index1(1)
       nlat=end_index1(2)
       nsig=end_index1(3)

  rmse_var='DLMD'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
      write(6,*)' rmse_var=',trim(rmse_var)
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       dlmd,WRF_REAL,0,0,0,ordering,          &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  print *,' dlmd=',dlmd

  rmse_var='DPHD'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
      write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       dphd,WRF_REAL,0,0,0,ordering,          &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  print *,' dphd=',dphd

  rmse_var='PT'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
      write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       pt,WRF_REAL,0,0,0,ordering,          &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  print *,' pt=',pt

  rmse_var='PDTOP'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
      write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            pdtop,WRF_REAL,0,0,0,ordering,       &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
  print *,' pdtop=',pdtop

  im=nlon
  jm=nlat
  allocate(aeta1(nsig),eta1(nsig+1),aeta2(nsig),eta2(nsig+1))
  allocate(glat(im,jm),glon(im,jm))
  glat=999.0
  glon=999.0

  rmse_var='AETA1'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
      write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            aeta1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,                    &
            start_index,end_index1,                   & !dom
            start_index,end_index1,                   & !mem
            start_index,end_index1,                   & !pat
            ierr                                 )

  rmse_var='ETA1'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
      write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            eta1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,                    &
            start_index,end_index1,                   & !dom
            start_index,end_index1,                   & !mem
            start_index,end_index1,                   & !pat
            ierr                                 )


  rmse_var='AETA2'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
      write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            aeta2,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,                    &
            start_index,end_index1,                   & !dom
            start_index,end_index1,                   & !mem
            start_index,end_index1,                   & !pat
            ierr                                 )

  rmse_var='ETA2'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            eta2,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,                    &
            start_index,end_index1,                   & !dom
            start_index,end_index1,                   & !mem
            start_index,end_index1,                   & !pat
            ierr                                 )

  rmse_var='GLAT'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            glat,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )

  rmse_var='GLON'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            glon,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
  

  call open_grads(outname,2*im-1,jm,nsig)

  im_tempb = 2*im-1
  allocate(temp1(im,jm),tempb(im_tempb,jm),itemp1(im,jm),temp3(im,jm,nsig),psfc_this(im,jm))
  allocate(temp4(im,jm,nsig+1))
  allocate(height(nsig+1))
  allocate(gz(im,jm))

  print*,'maxval of glat:', maxval(glat*180.0/3.1415926),maxloc(glat)
  print*,'minval of glat:', minval(glat*180.0/3.1415926),minloc(glat)
  print*,'maxval of glon:', maxval(glon*180.0/3.1415926),maxloc(glon)
  print*,'minval of glon:', minval(glon*180.0/3.1415926),minloc(glon)


  !write(600)tempb
  print*,'maxval of glat:', maxval(tempb*180.0/3.1415926)
  print*,'minval of glat:', minval(tempb*180.0/3.1415926)
  tempb=tempb*180.0/3.1415926
  do j=1,jm,10
     write(*,*)'lat, lon::',j,tempb(1,j),tempb(im_tempb,j)
  end do
  call fill_nmm_gridg(glon,im,jm,tempb,igtypeh)
  !write(600)tempb
  print*,'maxval of glon:', maxval(tempb*180.0/3.1415926)
  print*,'minval of glon:', minval(tempb*180.0/3.1415926)
  tempb=tempb*180.0/3.1415926
  do j=1,im_tempb
     write(*,*)'lat, lon::',j,tempb(j,jm),tempb(j,1)
  end do 

  deallocate(glat,glon)

!                    pint 
  temp4=0.0
  rmse_var='PINT'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  !    write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            temp4,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )

  do k=1,nsig+1
    do j=1,jm
       do i=1,im
          temp1(i,j)=temp4(i,j,k)
       end do
    end do
    call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
    call out_grads(tempb,outname,2*im-1,jm)
!   print*,'maxval of PINT:', maxval(temp1)
!   print*,'PINT-average in the  ',k, 'lowest level:', sum(tempb)
  end do

!                    pd
  rmse_var='PD'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            temp1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )

  do j=1,jm
    do i=1,im
       psfc_this(i,j)=(temp1(i,j)+pdtop+pt)*r0_01
    end do
  end do
  call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
  call out_grads(tempb,outname,2*im-1,jm)

!                    ps
  call fill_nmm_gridg(psfc_this,im,jm,tempb,igtypeh)
  call out_grads(tempb,outname,2*im-1,jm)

!                    fis
  rmse_var='FIS'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            temp1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )

  do j=1,jm
    do i=1,im
       gz(i,j)=temp1(i,j)/grav
    end do
  end do
  call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
  call out_grads(tempb,outname,2*im-1,jm)

  allocate(tsen(im,jm,nsig))
!                      T
  temp3=0.0 
  rmse_var='T'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       write(6,*)' ierr,rmse_var=',ierr,trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            temp3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
  do k=1,nsig
   do j=1,jm
    do i=1,im
     temp1(i,j)=temp3(i,j,k)
     tsen(i,j,k)=temp1(i,j)
    end do
   end do
   call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
   call out_grads(tempb,outname,2*im-1,jm)
  end do

  allocate(tv(im,jm,nsig))
!                      q
  temp3=0.0
  rmse_var='Q'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            temp3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
  do k=1,nsig
   do j=1,jm
    do i=1,im
     temp1(i,j)=temp3(i,j,k)
     tv(i,j,k)=tsen(i,j,k)* (one+fv*temp1(i,j))
    end do
   end do
   call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
   call out_grads(tempb,outname,2*im-1,jm)
  end do
  
  deallocate(tsen)

  allocate(aeta1_ll(nsig),eta1_ll(nsig+1),aeta2_ll(nsig),eta2_ll(nsig+1))
  allocate(prsl(im,jm,nsig),prsi(im,jm,nsig+1))
  eta1_ll=eta1
  aeta1_ll=aeta1
  eta2_ll=eta2
  aeta2_ll=aeta2
  pt_ll=r0_01*pt
  pdtop_ll=r0_01*pdtop
  do k=1,nsig+1
     do j=1,jm
        do i=1,im
           prsi(i,j,k)=one_tenth* &
              (eta1_ll(k)*pdtop_ll + &
               eta2_ll(k)*(psfc_this(i,j)-pdtop_ll-pt_ll) + &
               pt_ll)
        end do
     end do
  end do
  
  do k=1,nsig
     do j=1,jm
        do i=1,im
           prsl(i,j,k)=one_tenth* &
                 (aeta1_ll(k)*pdtop_ll + &
                  aeta2_ll(k)*(psfc_this(i,j)-pdtop_ll-pt_ll) + &
                  pt_ll)
        end do
     end do
  end do
  
  deallocate(eta1_ll,aeta1_ll,eta2,aeta2_ll)

  allocate(geop_hgti(im,jm,nsig+1))
  geop_hgti=0.0
  rdog = rd/grav
  do j=1,jm
     do i=1,im
        k=1
        height(k) = gz(i,j)
  
        do k=2,nsig
           h  = rdog * tv(i,j,k-1)
           dz = h * log(prsi(i,j,k-1)/prsi(i,j,k))
           height(k) = height(k-1) + dz
        end do
  
        k=nsig+1
        h = rdog * tv(i,j,k-1)
        dz = h * log(prsi(i,j,k-1)/prsl(i,j,k-1))
        height(k) = height(k-1) + dz
  
        do k=1,nsig+1
           geop_hgti(i,j,k)=height(k) - gz(i,j)
        end do
     end do
  end do
  
  deallocate(prsi,prsl,height,tv,gz)

!                     geop_hgti
  do k=2,nsig+1
   do j=1,jm
    do i=1,im
     temp1(i,j)=geop_hgti(i,j,k)
    end do
   end do
   call fill_nmm_gridg(temp1,im,jm,tempb,igtypev)
   call out_grads(tempb,outname,2*im-1,jm)
  end do

  deallocate(geop_hgti)

!                      u
  temp3=0.0
  rmse_var='U'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            temp3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
  
  do k=1,nsig
   do j=1,jm
    do i=1,im
     temp1(i,j)=temp3(i,j,k)
    end do
   end do
   call fill_nmm_gridg(temp1,im,jm,tempb,igtypev)
   call out_grads(tempb,outname,2*im-1,jm)
! print*,'U-average in the  ',k, 'lowest level:', sum(tempb)
  end do

!                      v
  temp3=0.0
  rmse_var='V'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            temp3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
  
  do k=1,nsig
   do j=1,jm
    do i=1,im
     temp1(i,j)=temp3(i,j,k)
    end do
   end do
   call fill_nmm_gridg(temp1,im,jm,tempb,igtypev)
   call out_grads(tempb,outname,2*im-1,jm)
  end do
!  deallocate(temp3)
  
!                      w
  temp4=0.0
  rmse_var='W'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            temp4,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )

  do k=1,nsig+1
    do j=1,jm
      do i=1,im
         temp1(i,j)=temp4(i,j,k)
      end do
    end do
    call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
    call out_grads(tempb,outname,2*im-1,jm)
 !  print*,'maxval of W:', maxval(temp1)
    print*,'W-average in the  ',k, 'lowest level:', sum(tempb)
  end do
  deallocate(temp4)

!                      sm
  rmse_var='SM'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            temp1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
  call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
  call out_grads(tempb,outname,2*im-1,jm)

!                      sice
  rmse_var='SICE'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            temp1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
  call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
  call out_grads(tempb,outname,2*im-1,jm)

!                      sst
  rmse_var='SST'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            temp1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
  call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
  call out_grads(tempb,outname,2*im-1,jm)
  
!                      ivgtyp
  rmse_var='IVGTYP'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
    start_index,end_index1, WrfType, ierr    )
     write(6,*)' rmse_var=',trim(rmse_var)
     call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
          itemp1,WrfType,0,0,0,ordering,           &
          staggering, dimnames ,               &
          start_index,end_index1,               & !dom
          start_index,end_index1,               & !mem
          start_index,end_index1,               & !pat
          ierr                                 )
  do j=1,jm
   do i=1,im
    temp1(i,j)=itemp1(i,j)
   end do
  end do
  call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
  call out_grads(tempb,outname,2*im-1,jm)

!                      isltyp
  rmse_var='ISLTYP'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            itemp1,WrfType,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
  do j=1,jm
   do i=1,im
    temp1(i,j)=itemp1(i,j)
   end do
  end do
  call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
  call out_grads(tempb,outname,2*im-1,jm)
  
  deallocate(itemp1)

!                      vegfrac
  rmse_var='VEGFRC'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            temp1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 ) 
  call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
  call out_grads(tempb,outname,2*im-1,jm)

!                      sno
  rmse_var='SNO'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            temp1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 ) 
  call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
  call out_grads(tempb,outname,2*im-1,jm)

!                      u10   --  special note:  u10,v10 on mass grid
  rmse_var='U10'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            temp1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
  call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
  call out_grads(tempb,outname,2*im-1,jm)

!                      v10
  rmse_var='V10'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            temp1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )  
  call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
  call out_grads(tempb,outname,2*im-1,jm)
  
!                      smc
       rmse_var='SMC'
       call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
            start_index,end_index1, WrfType, ierr    )
       write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            temp3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
  do j=1,jm
   do i=1,im
    temp1(i,j)=temp3(i,j,1)
   end do
  end do
  call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
  call out_grads(tempb,outname,2*im-1,jm)
  
!                      stc
  rmse_var='STC'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            temp3,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
  do j=1,jm
   do i=1,im
    temp1(i,j)=temp3(i,j,1)
   end do
  end do
  call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
  call out_grads(tempb,outname,2*im-1,jm)

!                      tsk
  rmse_var='TSK'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
       write(6,*)' rmse_var=',trim(rmse_var)
       call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
            temp1,WRF_REAL,0,0,0,ordering,           &
            staggering, dimnames ,               &
            start_index,end_index1,               & !dom
            start_index,end_index1,               & !mem
            start_index,end_index1,               & !pat
            ierr                                 )
  call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
  call out_grads(tempb,outname,2*im-1,jm)
  
  deallocate(temp1,tempb,temp3)
  
  call close_grads(outname)
  call ext_ncd_ioclose(dh1, Status)
  
end program reg_gsi_grads

subroutine fill_nmm_gridg(gin,nx,ny,b,igtype)

!  convert input staggered grid to output filled grid

!   --> gin:   input staggered grid
!   --> nx,ny: input grid dimensions
!  <--  b:     output filled grid
!   --> igtype: =1, then (1,1) on staggered grid is at corner of grid
!               =2, then (1,1) is staggered

implicit none

integer nx,ny,igtype
real(4) gin(nx,ny),b(2*nx-1,ny)

integer i,im,ip,j,jm,jp
real(4) fill,test

fill=.95*huge(fill) ; test=.95*fill
do j=1,ny
 do i=1,2*nx-1
  b(i,j)=fill
 end do
end do

!  first transfer all staggered points to appropriate
!   points on filled output grid

if(igtype.eq.1) then
 do j=1,ny,2
  do i=1,nx
   b(2*i-1,j)=gin(i,j)
  end do
 end do
 do j=2,ny,2
  do i=1,nx-1
   b(2*i,j)=gin(i,j)
  end do
 end do
else
 do j=1,ny,2
  do i=1,nx-1
   b(2*i,j)=gin(i,j)
  end do
 end do
 do j=2,ny,2
  do i=1,nx
   b(2*i-1,j)=gin(i,j)
  end do
 end do
end if

!  now fill in holes

! top and bottom rows:

do j=1,ny,ny-1
 do i=1,2*nx-1
  if(b(i,j).gt.test) then
   ip=i+1 ; if(ip.gt.2*nx-1) ip=i-1
   im=i-1 ; if(im.lt.1) im=i+1
   b(i,j)=.5*(b(im,j)+b(ip,j))
  end if
 end do
end do

! left and right rows:

do j=1,ny
 jp=j+1 ; if(jp.gt.ny) jp=j-1
 jm=j-1 ; if(jm.lt.1) jm=j+1
 do i=1,2*nx-1,2*nx-2
  if(b(i,j).gt.test) b(i,j)=.5*(b(i,jm)+b(i,jp))
 end do
end do

! interior points

do j=1,ny
 jp=j+1 ; if(jp.gt.ny) jp=j-1
 jm=j-1 ; if(jm.lt.1) jm=j+1
 do i=1,2*nx-1
  if(b(i,j).gt.test) then
   ip=i+1 ; if(ip.gt.2*nx-1) ip=i-1
   im=i-1 ; if(im.lt.1) im=i+1
   b(i,j)=.25*(b(ip,j)+b(im,j)+b(i,jp)+b(i,jm))
  end if
 end do
end do

end subroutine fill_nmm_gridg

MODULE module_wrf_error_dummy
  INTEGER           :: wrf_debug_level = 0
  CHARACTER*256     :: wrf_err_message

  LOGICAL :: silence=.false.  ! T = this process should not log.
  LOGICAL :: buffered=.false. ! T = messages sent via clog_write
  LOGICAL :: stderrlog=.false.! T = send to write(0,...) if buffered=F

  INTEGER, PARAMETER :: wrf_log_flush=0, wrf_log_set_buffer_size=1, &
                        wrf_log_write=2

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

! stub for routine called by module_wrf_error (used by netcdf implementation of
! IO api)
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

