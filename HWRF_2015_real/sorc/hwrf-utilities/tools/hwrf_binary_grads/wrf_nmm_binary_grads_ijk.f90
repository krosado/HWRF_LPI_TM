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
    write(datdes(1),'("DSET ",a)')trim(label_dat(ithis))
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
    write(datdes(19),'("w    ",i5," 99 w    ")')nsig
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
implicit none

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

call retrieve_binary_nmm_constants(input_file, &
                      iyear,imonth,iday,ihour,iminute,isecond, &
                      jyear,jmonth,jday,jhour,jminute,jsecond, &
                      nlon,nlat,nsig,nsoil,dlmd,dphd,pt,pdtop)
im=nlon
jm=nlat
!write(6,*)'nlon,nlat::',nlon,nlat
allocate(deta1(nsig),aeta1(nsig),eta1(nsig+1),deta2(nsig),aeta2(nsig),eta2(nsig+1))
allocate(glat(im,jm),glon(im,jm))
glat=999.0
glon=999.0
call retrieve_binary_nmm_field('DETA1',deta1,input_file,nsig)
call retrieve_binary_nmm_field('AETA1',aeta1,input_file,nsig)
call retrieve_binary_nmm_field('ETA1',eta1,input_file,nsig+1)
call retrieve_binary_nmm_field('DETA2',deta2,input_file,nsig)
call retrieve_binary_nmm_field('AETA2',aeta2,input_file,nsig)
call retrieve_binary_nmm_field('ETA2',eta2,input_file,nsig+1)
call retrieve_binary_nmm_field('GLAT',glat,input_file,im*jm)
call retrieve_binary_nmm_field('GLON',glat,input_file,im*jm)


call open_grads(outname,2*im-1,jm,nsig)

allocate(temp1(im,jm),tempb(2*im-1,jm),itemp1(im,jm),temp3(im,jm,nsig),psfc_this(im,jm))
allocate(temp4(im,jm,nsig+1))
allocate(height(nsig+1))
allocate(gz(im,jm))

print*,'maxval of glat:', maxval(glat*180.0/3.1415926),maxloc(glat)
print*,'minval of glat:', minval(glat*180.0/3.1415926),minloc(glat)
print*,'maxval of glon:', maxval(glon*180.0/3.1415926),maxloc(glon)
print*,'minval of glon:', minval(glon*180.0/3.1415926),minloc(glon)

call fill_nmm_gridg(glat,im,jm,tempb,igtypeh)
write(600)tempb
print*,'maxval of glat:', maxval(tempb*180.0/3.1415926)
print*,'minval of glat:', minval(tempb*180.0/3.1415926)
tempb=tempb*180.0/3.1415926
!  do j=1,jm,10
!    write(*,*)'lat, lon::',j,tempb(1,j),tempb(650,j)
!  end do
call fill_nmm_gridg(glon,im,jm,tempb,igtypeh)
write(600)tempb
print*,'maxval of glon:', maxval(tempb*180.0/3.1415926)
print*,'minval of glon:', minval(tempb*180.0/3.1415926)
tempb=tempb*180.0/3.1415926
! do j=1,2*im-1
!    write(*,*)'lat, lon::',j,tempb(j,600),tempb(j,1)
! end do 
!stop

!                    pint 
temp4=0.0
call retrieve_binary_nmm_field('PINT',temp4,input_file,im*jm*(nsig+1))
do k=1,nsig+1
 do j=1,jm
   do i=1,im
      temp1(i,j)=temp4(i,j,k)
   end do
 end do
   call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
   call out_grads(tempb,outname,2*im-1,jm)
!  print*,'maxval of PINT:', maxval(temp1)
!  print*,'PINT-average in the  ',k, 'lowest level:', sum(tempb)
end do

!                    pd
call retrieve_binary_nmm_field('PD',temp1,input_file,im*jm)
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
call retrieve_binary_nmm_field('FIS',temp1,input_file,im*jm)
do j=1,jm
  do i=1,im
     gz(i,j)=temp1(i,j)/grav
  end do
end do
call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,2*im-1,jm)

allocate(tsen(im,jm,nsig))
!                      T
call retrieve_binary_nmm_field('T',temp3,input_file,im*jm*nsig)
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
call retrieve_binary_nmm_field('Q',temp3,input_file,im*jm*nsig)
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

deallocate(prsi,prsl,height,tv)

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
call retrieve_binary_nmm_field('U',temp3,input_file,im*jm*nsig)
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
call retrieve_binary_nmm_field('V',temp3,input_file,im*jm*nsig)
do k=1,nsig
 do j=1,jm
  do i=1,im
   temp1(i,j)=temp3(i,j,k)
  end do
 end do
 call fill_nmm_gridg(temp1,im,jm,tempb,igtypev)
 call out_grads(tempb,outname,2*im-1,jm)
end do
deallocate(temp3)

!                      w
temp3=0.0
call retrieve_binary_nmm_field('W',temp3,input_file,im*jm*(nsig+1))
do k=1,nsig
 do j=1,jm
   do i=1,im
      temp1(i,j)=temp3(i,j,k)
   end do
 end do
   call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
   call out_grads(tempb,outname,2*im-1,jm)
!  print*,'maxval of W:', maxval(temp1)
!  print*,'W-average in the  ',k, 'lowest level:', sum(tempb)
end do
           deallocate(temp4)


!                      sm
call retrieve_binary_nmm_field('SM',temp1,input_file,im*jm)
call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,2*im-1,jm)

!                      sice
call retrieve_binary_nmm_field('SICE',temp1,input_file,im*jm)
call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,2*im-1,jm)

!                      sst
call retrieve_binary_nmm_field('SST',temp1,input_file,im*jm)
call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,2*im-1,jm)

!                      ivgtyp
call retrieve_binary_nmm_ifield('IVGTYP',itemp1,input_file,im*jm)
do j=1,jm
 do i=1,im
  temp1(i,j)=itemp1(i,j)
 end do
end do
call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,2*im-1,jm)

!                      isltyp
call retrieve_binary_nmm_ifield('ISLTYP',itemp1,input_file,im*jm)
do j=1,jm
 do i=1,im
  temp1(i,j)=itemp1(i,j)
 end do
end do
call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,2*im-1,jm)

!                      vegfrac
call retrieve_binary_nmm_field('VEGFRAC',temp1,input_file,im*jm)
call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,2*im-1,jm)

!                      sno
call retrieve_binary_nmm_field('SNO',temp1,input_file,im*jm)
call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,2*im-1,jm)

!                      u10   --  special note:  u10,v10 on mass grid
call retrieve_binary_nmm_field('U10',temp1,input_file,im*jm)
call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,2*im-1,jm)

!                      v10
call retrieve_binary_nmm_field('V10',temp1,input_file,im*jm)
call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,2*im-1,jm)

allocate(temp3(im,nsoil,jm))
!                      smc
call retrieve_binary_nmm_field('SMC',temp3,input_file,im*jm*nsoil)
do j=1,jm
 do i=1,im
  temp1(i,j)=temp3(i,1,j)
 end do
end do
call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,2*im-1,jm)

!                      stc
call retrieve_binary_nmm_field('STC',temp3,input_file,im*jm*nsoil)
do j=1,jm
 do i=1,im
  temp1(i,j)=temp3(i,1,j)
 end do
end do
call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,2*im-1,jm)
deallocate(temp3)

!                      tsk
call retrieve_binary_nmm_field('TSK',temp1,input_file,im*jm)
call fill_nmm_gridg(temp1,im,jm,tempb,igtypeh)
call out_grads(tempb,outname,2*im-1,jm)

call close_grads(outname)

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

subroutine retrieve_binary_nmm_constants(wrf_ges_filename, &
                      iyear,imonth,iday,ihour,iminute,isecond, &
                      jyear,jmonth,jday,jhour,jminute,jsecond, &
                      nlon,nlat,nsig,nsoil,dlmd,dphd,pt,pdtop)

  implicit none

  character(*),intent(in):: wrf_ges_filename
  integer,intent(out):: iyear,imonth,iday,ihour,iminute,isecond  !  start time
  integer,intent(out):: jyear,jmonth,jday,jhour,jminute,jsecond  !  forecast time
  integer,intent(out):: nlon,nlat,nsig,nsoil
  real(4),intent(out):: dlmd,dphd,pt,pdtop

  integer,parameter:: int_field       =       530
  integer,parameter:: int_dom_ti_char =       220

  integer in_unit,status,status_hdr,code
  integer hdrbuf(512)

  integer itypesize,rtypesize,typesize
  integer hdrbufsize
  integer inttypesize,realtypesize
  integer datahandle
  character(132) datestr,varname
  real(4) dummy_field(1)
  integer fieldtype,comm,iocomm
  integer domaindesc
  character(132) memoryorder,stagger,dimnames(3)
  integer domainstart(3),domainend(3)
  integer memorystart(3),memoryend(3)
  integer patchstart(3),patchend(3)
  integer irec
  character(128) element,strdata,dumstr
  integer loccode,loccount

  call wrf_sizeof_integer(itypesize)
  call wrf_sizeof_real(rtypesize)
  in_unit=10
  open(in_unit,file=trim(wrf_ges_filename),form='unformatted')
! write(6,*)' retrieve_binary_nmm_constants: in_unit=',in_unit
  inttypesize=itypesize
  realtypesize=rtypesize

! Check for valid input file
  read(in_unit,iostat=status_hdr)hdrbuf
  if(status_hdr /= 0) then
     write(6,*)'retrieve_binary_nmm_constants:  problem with wrf_ges_filename = ',&
          trim(wrf_ges_filename),', Status = ',status_hdr
     stop
  endif

!   start with 1st record, which has various constants

!                   y,m,d,h,m,s
  rewind in_unit
  do
     read(in_unit,iostat=status_hdr)hdrbuf
     if(status_hdr /= 0) exit
     if(hdrbuf(2) == int_dom_ti_char) then
        call int_get_ti_header_char(hdrbuf,hdrbufsize,inttypesize, &
             datahandle,element,dumstr,strdata,loccode)
        if(trim(element) == 'START_DATE') then
           read(strdata,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') &
                iyear,imonth,iday,ihour,iminute,isecond
           write(6,*)' retrieve_binary_nmm_constants: START_DATE =',&
                iyear,imonth,iday,ihour,iminute,isecond
           exit
        end if
     end if
  end do

!                  nlon, nlat, nsig
  rewind in_unit
  do
     read(in_unit,iostat=status_hdr)hdrbuf
     if(status_hdr /= 0) exit
     code=hdrbuf(2)
     if(code == int_field) then
        call int_get_write_field_header(hdrbuf,hdrbufsize,inttypesize,typesize, &
             datahandle,datestr,varname,dummy_field,fieldtype,comm,iocomm, &
             domaindesc,memoryorder,stagger,dimnames, &
             domainstart,domainend,memorystart,memoryend,patchstart,patchend)
        if(trim(varname) == 'T') then
           nlon=domainend(1)
           nlat=domainend(2)
           nsig=domainend(3)
           read(datestr,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') &
                jyear,jmonth,jday,jhour,jminute,jsecond
           write(6,*)' retrieve_binary_nmm_constants: iy,m,d,h,m,s=',&
                iyear,imonth,iday,ihour,iminute,isecond
           write(6,*)' retrieve_binary_nmm_constants: nlon,lat,sig=',&
                nlon,nlat,nsig
           exit
        end if
        read(in_unit,iostat=status)
        if(status /= 0) exit
     end if
  end do

!                  dlmd
  rewind in_unit
  do
     read(in_unit,iostat=status_hdr)hdrbuf
     if(status_hdr /= 0) exit
     code=hdrbuf(2)
     if(code == int_field) then
        call int_get_write_field_header(hdrbuf,hdrbufsize,inttypesize,typesize, &
             datahandle,datestr,varname,dummy_field,fieldtype,comm,iocomm, &
             domaindesc,memoryorder,stagger,dimnames, &
             domainstart,domainend,memorystart,memoryend,patchstart,patchend)
        if(trim(varname) == 'DLMD') then
           read(in_unit,iostat=status)dlmd
           write(6,*)' retrieve_binary_nmm_constants: dlmd=',dlmd
           exit
        end if
        read(in_unit,iostat=status)
        if(status /= 0) exit
     end if
  end do

!                  dphd
  rewind in_unit
  do
     read(in_unit,iostat=status_hdr)hdrbuf
     if(status_hdr /= 0) exit
     code=hdrbuf(2)
     if(code == int_field) then
        call int_get_write_field_header(hdrbuf,hdrbufsize,inttypesize,typesize, &
             datahandle,datestr,varname,dummy_field,fieldtype,comm,iocomm, &
             domaindesc,memoryorder,stagger,dimnames, &
             domainstart,domainend,memorystart,memoryend,patchstart,patchend)
        if(trim(varname) == 'DPHD') then
           read(in_unit,iostat=status)dphd
           write(6,*)' retrieve_binary_nmm_constants: dphd=',dphd
           exit
        end if
        read(in_unit,iostat=status)
        if(status /= 0) exit
     end if
  end do

!                  pt
  rewind in_unit
  do
     read(in_unit,iostat=status_hdr)hdrbuf
     if(status_hdr /= 0) exit
     code=hdrbuf(2)
     if(code == int_field) then
        call int_get_write_field_header(hdrbuf,hdrbufsize,inttypesize,typesize, &
             datahandle,datestr,varname,dummy_field,fieldtype,comm,iocomm, &
             domaindesc,memoryorder,stagger,dimnames, &
             domainstart,domainend,memorystart,memoryend,patchstart,patchend)
        if(trim(varname) == 'PT') then
           read(in_unit,iostat=status)pt
           write(6,*)' retrieve_binary_nmm_constants: pt=',pt
           exit
        end if
        read(in_unit,iostat=status)
        if(status /= 0) exit
     end if
  end do

!                  pdtop
  rewind in_unit
  do
     read(in_unit,iostat=status_hdr)hdrbuf
     if(status_hdr /= 0) exit
     code=hdrbuf(2)
     if(code == int_field) then
        call int_get_write_field_header(hdrbuf,hdrbufsize,inttypesize,typesize, &
             datahandle,datestr,varname,dummy_field,fieldtype,comm,iocomm, &
             domaindesc,memoryorder,stagger,dimnames, &
             domainstart,domainend,memorystart,memoryend,patchstart,patchend)
        if(trim(varname) == 'PDTOP') then
           read(in_unit,iostat=status)pdtop
           write(6,*)' retrieve_binary_nmm_constants: pdtop=',pdtop
           exit
        end if
        read(in_unit,iostat=status)
        if(status /= 0) exit
     end if
  end do

!                   SMC
  rewind in_unit
  do
     read(in_unit,iostat=status_hdr)hdrbuf
     if(status_hdr /= 0) exit
     code=hdrbuf(2)
     if(code == int_field) then
        call int_get_write_field_header(hdrbuf,hdrbufsize,inttypesize,typesize, &
             datahandle,datestr,varname,dummy_field,fieldtype,comm,iocomm, &
             domaindesc,memoryorder,stagger,dimnames, &
             domainstart,domainend,memorystart,memoryend,patchstart,patchend)
        if(trim(varname) == 'SMC') then
           nsoil=domainend(2)
           write(6,*)' convert_binary_nmm: nsoil=',nsoil
           exit
        end if
        read(in_unit,iostat=status)
        if(status /= 0) exit
     end if
  end do

  close(in_unit)

end subroutine retrieve_binary_nmm_constants

subroutine retrieve_binary_nmm_field(desired_varname,outbuf,wrf_ges_filename,lenbuf)

  implicit none

  character(*),intent(in)::desired_varname
  integer,intent(in):: lenbuf
  character(*),intent(in)::wrf_ges_filename
  real(4),intent(out)::outbuf(lenbuf)

  integer,parameter:: int_field       =       530
  integer,parameter:: int_dom_ti_char =       220

  integer in_unit,status,status_hdr,code
  integer hdrbuf(512)

  integer itypesize,rtypesize,typesize
  integer hdrbufsize
  integer inttypesize,realtypesize
  integer datahandle
  character(132) datestr,varname
  real(4) dummy_field(1)
  integer fieldtype,comm,iocomm
  integer domaindesc
  character(132) memoryorder,stagger,dimnames(3)
  integer domainstart(3),domainend(3)
  integer memorystart(3),memoryend(3)
  integer patchstart(3),patchend(3)
  integer irec
  character(128) element,strdata,dumstr
  integer loccode,loccount

  call wrf_sizeof_integer(itypesize)
  call wrf_sizeof_real(rtypesize)
  in_unit=10
  open(in_unit,file=trim(wrf_ges_filename),form='unformatted')
!  write(6,*)' retrieve_binary_nmm_constants: in_unit=',in_unit
  inttypesize=itypesize
  realtypesize=rtypesize

  rewind in_unit
  do
     read(in_unit,iostat=status_hdr)hdrbuf
     if(status_hdr /= 0) exit
     code=hdrbuf(2)
     if(code == int_field) then
        call int_get_write_field_header(hdrbuf,hdrbufsize,inttypesize,typesize, &
             datahandle,datestr,varname,dummy_field,fieldtype,comm,iocomm, &
             domaindesc,memoryorder,stagger,dimnames, &
             domainstart,domainend,memorystart,memoryend,patchstart,patchend)
        if(trim(varname) == trim(desired_varname)) then
           read(in_unit,iostat=status)outbuf
           exit
        end if
        read(in_unit,iostat=status)
        if(status /= 0) exit
     end if
  end do

  close(in_unit)

end subroutine retrieve_binary_nmm_field

subroutine retrieve_binary_nmm_ifield(desired_varname,outbuf,wrf_ges_filename,lenbuf)

  implicit none

  character(*),intent(in)::desired_varname
  integer,intent(in):: lenbuf
  character(*),intent(in)::wrf_ges_filename
  integer(4),intent(out)::outbuf(lenbuf)

  integer,parameter:: int_field       =       530
  integer,parameter:: int_dom_ti_char =       220

  integer in_unit,status,status_hdr,code
  integer hdrbuf(512)

  integer itypesize,rtypesize,typesize
  integer hdrbufsize
  integer inttypesize,realtypesize
  integer datahandle
  character(132) datestr,varname
  real(4) dummy_field(1)
  integer fieldtype,comm,iocomm
  integer domaindesc
  character(132) memoryorder,stagger,dimnames(3)
  integer domainstart(3),domainend(3)
  integer memorystart(3),memoryend(3)
  integer patchstart(3),patchend(3)
  integer irec
  character(128) element,strdata,dumstr
  integer loccode,loccount

  call wrf_sizeof_integer(itypesize)
  call wrf_sizeof_real(rtypesize)
  in_unit=10
  open(in_unit,file=trim(wrf_ges_filename),form='unformatted')
!  write(6,*)' retrieve_binary_nmm_constants: in_unit=',in_unit
  inttypesize=itypesize
  realtypesize=rtypesize

  rewind in_unit
  do
     read(in_unit,iostat=status_hdr)hdrbuf
     if(status_hdr /= 0) exit
     code=hdrbuf(2)
     if(code == int_field) then
        call int_get_write_field_header(hdrbuf,hdrbufsize,inttypesize,typesize, &
             datahandle,datestr,varname,dummy_field,fieldtype,comm,iocomm, &
             domaindesc,memoryorder,stagger,dimnames, &
             domainstart,domainend,memorystart,memoryend,patchstart,patchend)
        if(trim(varname) == trim(desired_varname)) then
           read(in_unit,iostat=status)outbuf
           exit
        end if
        read(in_unit,iostat=status)
        if(status /= 0) exit
     end if
  end do

  close(in_unit)

end subroutine retrieve_binary_nmm_ifield
subroutine mpi_abort

  stop

end subroutine mpi_abort


SUBROUTINE int_get_ti_header_char( hdrbuf, hdrbufsize, itypesize, &
                              DataHandle, Element, VarName, Data, code )
!<DESCRIPTION>
!<PRE>
! Same as int_gen_ti_header_char except that Data is read from 
! the file.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
!!  INCLUDE 'intio_tags.h'
  INTEGER, INTENT(INOUT)       ::  hdrbuf(*)
  INTEGER, INTENT(OUT)         ::  hdrbufsize
  INTEGER, INTENT(IN)          ::  itypesize
  CHARACTER*(*), INTENT(INOUT) ::  Element, Data, VarName
  INTEGER, INTENT(OUT)         ::  DataHandle, code
!Local
  INTEGER i, n, DummyCount, typesize
  CHARACTER * 132  dummyData
  logical, external :: debug_foo
!
  CALL int_get_ti_header_c ( hdrbuf, hdrbufsize, n, itypesize, typesize, &
                           DataHandle, dummyData, DummyCount, code )
  i = n/itypesize+1 ;
  CALL int_unpack_string ( Element, hdrbuf( i ), n ) ; i = i + n
  CALL int_unpack_string ( Data   , hdrbuf( i ), n ) ; i = i + n
  CALL int_unpack_string ( VarName  , hdrbuf( i ), n ) ; i = i + n
  hdrbufsize = hdrbuf(1)

  RETURN
END SUBROUTINE int_get_ti_header_char
SUBROUTINE int_get_write_field_header ( hdrbuf, hdrbufsize, itypesize, ftypesize, &
                                        DataHandle , DateStr , VarName , Dummy , FieldType , Comm , IOComm,  &
                                        DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                        DomainStart , DomainEnd ,                                    &
                                        MemoryStart , MemoryEnd ,                                    &
                                        PatchStart , PatchEnd )
!<DESCRIPTION>
!<PRE>
! See documentation block in int_gen_write_field_header() for 
! a description of a "write field" header.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE

!!  INCLUDE 'intio_tags.h'
  integer,parameter:: int_field       =       530
  INTEGER,       INTENT(INOUT)  ::  hdrbuf(*)
  INTEGER,       INTENT(OUT)    ::  hdrbufsize
  INTEGER,       INTENT(INOUT)  ::  itypesize, ftypesize
  INTEGER ,      INTENT(OUT)    :: DataHandle
  CHARACTER*(*), INTENT(INOUT)  :: DateStr
  CHARACTER*(*), INTENT(INOUT)  :: VarName
  REAL, DIMENSION(*)            :: Dummy
  INTEGER                                       :: FieldType
  INTEGER                                       :: Comm
  INTEGER                                       :: IOComm
  INTEGER                                       :: DomainDesc
  CHARACTER*(*)                                 :: MemoryOrder
  CHARACTER*(*)                                 :: Stagger
  CHARACTER*(*) , dimension (*)                 :: DimNames
  INTEGER ,dimension(*)                         :: DomainStart, DomainEnd
  INTEGER ,dimension(*)                         :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)                         :: PatchStart,  PatchEnd
!Local
  CHARACTER*132 mess
  INTEGER i, n

  hdrbufsize = hdrbuf(1)
  IF ( hdrbuf(2) .NE. int_field ) THEN
    write(mess,*)'int_get_write_field_header: hdrbuf(2) ne int_field ',hdrbuf(2),int_field
    CALL wrf_error_fatal3 ( "module_internal_header_util.b" , 220 ,  mess )
  ENDIF
  ftypesize = hdrbuf(3)

   i = 4
   DataHandle = hdrbuf(i)     ; i = i+1
  call int_unpack_string( DateStr, hdrbuf(i), n )     ; i = i+n
  call int_unpack_string( VarName, hdrbuf(i), n )     ; i = i+n
   FieldType = hdrbuf(i)      ; i = i+1
  call int_unpack_string( MemoryOrder, hdrbuf(i), n ) ; i = i+n
  call int_unpack_string( Stagger, hdrbuf(i), n )     ; i = i+n
  call int_unpack_string( DimNames(1), hdrbuf(i), n ) ; i = i+n
  call int_unpack_string( DimNames(2), hdrbuf(i), n ) ; i = i+n
  call int_unpack_string( DimNames(3), hdrbuf(i), n ) ; i = i+n
   DomainStart(1) = hdrbuf(i)    ; i = i+1
   DomainStart(2) = hdrbuf(i)    ; i = i+1
   DomainStart(3) = hdrbuf(i)    ; i = i+1
   DomainEnd(1) = hdrbuf(i)       ; i = i+1
   DomainEnd(2) = hdrbuf(i)       ; i = i+1
   DomainEnd(3) = hdrbuf(i)       ; i = i+1
   PatchStart(1) = hdrbuf(i)     ; i = i+1
   PatchStart(2) = hdrbuf(i)     ; i = i+1
   PatchStart(3) = hdrbuf(i)     ; i = i+1
   PatchEnd(1) = hdrbuf(i)       ; i = i+1
   PatchEnd(2) = hdrbuf(i)       ; i = i+1
   PatchEnd(3) = hdrbuf(i)       ; i = i+1
   DomainDesc = hdrbuf(i)       ; i = i+1

  RETURN
END SUBROUTINE int_get_write_field_header
SUBROUTINE int_unpack_string ( str, buf, n )
  IMPLICIT NONE
!<DESCRIPTION>
!<PRE>
! This routine is used to extract a string from a sequence of integers.  
! The first integer is the string length.  
!</PRE>
!</DESCRIPTION>
  CHARACTER*(*), INTENT(OUT)        :: str
  INTEGER, INTENT(OUT)              :: n       ! on return, N is the number of ints copied from buf
  INTEGER, INTENT(IN), DIMENSION(*) :: buf
!Local
  INTEGER i
  INTEGER strlen

  strlen = buf(1)
  str = ""
  DO i = 1, strlen
    str(i:i) = char(buf(i+1))
  ENDDO
  n = strlen + 1
END SUBROUTINE int_unpack_string

SUBROUTINE wrf_sizeof_integer( retval )
  IMPLICIT NONE
  integer(4),parameter:: i_kind=4
  INTEGER retval
! IWORDSIZE is defined by CPP
  retval = i_kind
  RETURN
END SUBROUTINE wrf_sizeof_integer

SUBROUTINE wrf_sizeof_real( retval )
  IMPLICIT NONE
  integer(4),parameter:: r_kind=4
  INTEGER retval
! RWORDSIZE is defined by CPP
  retval = r_kind
  RETURN
END SUBROUTINE wrf_sizeof_real

!WRF:DRIVER_LAYER:UTIL
!

MODULE module_wrf_error
  INTEGER           :: wrf_debug_level = 0
  CHARACTER*256     :: wrf_err_message
CONTAINS

  LOGICAL FUNCTION wrf_at_debug_level ( level )
    IMPLICIT NONE
    INTEGER , INTENT(IN) :: level
    wrf_at_debug_level = ( level .LE. wrf_debug_level )
    RETURN
  END FUNCTION wrf_at_debug_level

  SUBROUTINE init_module_wrf_error
  END SUBROUTINE init_module_wrf_error

END MODULE module_wrf_error

  SUBROUTINE set_wrf_debug_level ( level )
    USE module_wrf_error
    IMPLICIT NONE
    INTEGER , INTENT(IN) :: level
    wrf_debug_level = level
    RETURN
  END SUBROUTINE set_wrf_debug_level

  SUBROUTINE get_wrf_debug_level ( level )
    USE module_wrf_error
    IMPLICIT NONE
    INTEGER , INTENT(OUT) :: level
    level = wrf_debug_level
    RETURN
  END SUBROUTINE get_wrf_debug_level


SUBROUTINE wrf_debug( level , str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) str
  INTEGER , INTENT (IN) :: level
  INTEGER               :: debug_level
  CHARACTER (LEN=256) :: time_str
  CHARACTER (LEN=256) :: grid_str
  CHARACTER (LEN=512) :: out_str
  CALL get_wrf_debug_level( debug_level )
  IF ( level .LE. debug_level ) THEN
    ! old behavior
      CALL wrf_message( str )
  ENDIF
  RETURN
END SUBROUTINE wrf_debug

SUBROUTINE wrf_message( str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) str
  write(0,*) TRIM(str)
  print*, TRIM(str)
!TBH:  Calls to logwrite cut off str in ESMF 2.2.0.
!TBH:  Restore this call once this ESMF bug is fixed.
!TBH#ifdef USE_LOGERR
!TBH  IF ( WRFU_IsInitialized() ) THEN
!TBH    CALL WRFU_LogWrite( TRIM(str), WRFU_LOG_INFO )
!TBH  ENDIF
!TBH#endif
END SUBROUTINE wrf_message

! intentionally write to stderr only
SUBROUTINE wrf_message2( str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) str
  write(0,*) str
!TBH:  Calls to logwrite cut off str in ESMF 2.2.0.
!TBH:  Restore this call once this ESMF bug is fixed.
!TBH#ifdef USE_LOGERR
!TBH  IF ( WRFU_IsInitialized() ) THEN
!TBH    CALL WRFU_LogWrite( str, WRFU_LOG_INFO )
!TBH  ENDIF
!TBH#endif
END SUBROUTINE wrf_message2

SUBROUTINE wrf_error_fatal3( file_str, line, str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) file_str
  INTEGER , INTENT (IN) :: line  ! only print file and line if line > 0
  CHARACTER*(*) str
  CHARACTER*256 :: line_str

  write(line_str,'(i6)') line
  CALL wrf_message( '-------------- FATAL CALLED ---------------' )
  ! only print file and line if line is positive
  IF ( line > 0 ) THEN
    CALL wrf_message( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
  CALL wrf_message( str )
  CALL wrf_message( '-------------------------------------------' )
! CALL wrf_abort
  stop
END SUBROUTINE wrf_error_fatal3

SUBROUTINE wrf_error_fatal( str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) str
  CALL wrf_error_fatal3 ( ' ', 0, str )
END SUBROUTINE wrf_error_fatal

! Check to see if expected value == actual value
! If not, print message and exit.  
SUBROUTINE wrf_check_error( expected, actual, str, file_str, line )
  USE module_wrf_error
  IMPLICIT NONE
  INTEGER , INTENT (IN) :: expected
  INTEGER , INTENT (IN) :: actual
  CHARACTER*(*) str
  CHARACTER*(*) file_str
  INTEGER , INTENT (IN) :: line
  CHARACTER (LEN=512)   :: rc_str
  CHARACTER (LEN=512)   :: str_with_rc

  IF ( expected .ne. actual ) THEN
    WRITE (rc_str,*) '  Routine returned error code = ',actual
    str_with_rc = TRIM(str // rc_str)
    CALL wrf_error_fatal3 ( file_str, line, str_with_rc )
  ENDIF
END SUBROUTINE wrf_check_error
