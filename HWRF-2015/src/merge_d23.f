!
!     Merging domain 2 and 3 with a resolution of domain 3
!
!-----------------------------------------------------------------------
      program Interpb_d23
!-----------------------------------------------------------------------
      real,allocatable,dimension(:,:,:) :: hgt1,tmp1,ugrd1,vgrd1
      real,allocatable,dimension(:,:,:) :: vvel1,spfh1,rh1
      real,allocatable,dimension(:,:,:) :: hgt2,tmp2,ugrd2,vgrd2
      real,allocatable,dimension(:,:,:) :: vvel2,spfh2,rh2
      real,allocatable,dimension(:,:,:) :: hgt,tmp,ugrd,vgrd
      real,allocatable,dimension(:,:,:) :: vvel,spfh,rh
      real,allocatable,dimension(:,:,:) :: work1,work2
      real,allocatable,dimension(:,:) :: ugrd10m1,vgrd10m1
      real,allocatable,dimension(:,:) :: tmpsfc1,lhtfl1,prmsl1
      real,allocatable,dimension(:,:) :: ugrd10m2,vgrd10m2
      real,allocatable,dimension(:,:) :: tmpsfc2,lhtfl2,prmsl2
      real,allocatable,dimension(:,:) :: ugrd10m,vgrd10m
      real,allocatable,dimension(:,:) :: tmpsfc,lhtfl,prmsl
      real,allocatable,dimension(:)   :: rlon1,rlat1,rlon2,rlat2
      real,allocatable,dimension(:)   :: tlon,tlat
      real    :: clat,clon
      real    :: slat1,slon1,dlat1,dlon1
      real    :: slat2,slon2,dlat2,dlon2
      real    :: elon1,elon2,elat1,elat2
      real    :: x,y,s,t,alph1,alph2,alph3,alph4,undef
      integer :: ib1,jb1,ib2,jb2,ib,jb,kb
      integer :: is,ie,js,je
      integer :: i,j,ilo,jlo,ihi,jhi,jiv
      namelist /idata/ clat,clon,slat1,slon1,dlat1,dlon1,ib1,jb1
     &                          ,slat2,slon2,dlat2,dlon2,ib2,jb2,kb
! -----------------------------------------------
       undef= 9.999E+20

       read(11,NML=idata)

      print*,'ib1,jb1,ib2,jb2,kb',ib1,jb1,ib2,jb2,kb
      allocate( hgt1(ib1,jb1,kb),tmp1(ib1,jb1,kb),ugrd1(ib1,jb1,kb) )
      allocate( vgrd1(ib1,jb1,kb),vvel1(ib1,jb1,kb),spfh1(ib1,jb1,kb) )
      allocate( rh1(ib1,jb1,kb),ugrd10m1(ib1,jb1),vgrd10m1(ib1,jb1) )
      allocate( tmpsfc1(ib1,jb1),lhtfl1(ib1,jb1),prmsl1(ib1,jb1) )
      allocate( work1(ib1,jb1,kb),rlon1(ib1),rlat1(jb1) )
      allocate( hgt2(ib2,jb2,kb),tmp2(ib2,jb2,kb),ugrd2(ib2,jb2,kb) )
      allocate( vgrd2(ib2,jb2,kb),vvel2(ib2,jb2,kb),spfh2(ib2,jb2,kb) )
      allocate( rh2(ib2,jb2,kb),ugrd10m2(ib2,jb2),vgrd10m2(ib2,jb2) )
      allocate( tmpsfc2(ib2,jb2),lhtfl2(ib2,jb2),prmsl2(ib2,jb2) )
      allocate( work2(ib2,jb2,kb),rlon2(ib2),rlat2(jb2) )
      print*,'clat,clon',clat,clon

       elon1= slon1 +(ib1-1)*dlon1
       elon2= slon2 +(ib2-1)*dlon2
       elat1= slat1 +(jb1-1)*dlat1
       elat2= slat2 +(jb2-1)*dlat2
   
       is= (slon2-slon1)/dlon2
       ie= (elon1-elon2)/dlon2
       js= (slat2-slat1)/dlat2
       je= (elat1-elat2)/dlat2
       ib= is +ib2 +ie
       jb= js +jb2 +je

       slon= slon2 -is *dlon2
       slat= slat2 -js *dlat2

      allocate( hgt(ib,jb,kb),tmp(ib,jb,kb),ugrd(ib,jb,kb) )
      allocate( vgrd(ib,jb,kb),vvel(ib,jb,kb),spfh(ib,jb,kb) )
      allocate( rh(ib,jb,kb),ugrd10m(ib,jb),vgrd10m(ib,jb) )
      allocate( tmpsfc(ib,jb),lhtfl(ib,jb),prmsl(ib,jb) )
      allocate( tlon(ib),tlat(jb)    )

      do i= 1,ib1
         rlon1(i) = slon1 +(i-1) *dlon1
      enddo
      do j= 1,jb1
         rlat1(j) = slat1 +(j-1) *dlat1
      enddo
      do i= 1,ib2
         rlon2(i) = slon2 +(i-1) *dlon2
      enddo
      do j= 1,jb2
         rlat2(j) = slat2 +(j-1) *dlat2
      enddo
      do i= 1,ib 
         tlon(i)  = slon  +(i-1) *dlon2
      enddo
      do j= 1,jb 
         tlat(j)  = slat  +(j-1) *dlat2
      enddo

      print*,'slat1,slon1,dlat1,dlon1,ib1,jb1'
      print*, slat1,slon1,dlat1,dlon1,ib1,jb1
      print*,'slat2,slon2,dlat2,dlon2,ib2,jb2'
      print*, slat2,slon2,dlat2,dlon2,ib2,jb2
      print*,'elat1,elon1,elat2,elon2'
      print*, elat1,elon1,elat2,elon2
      print*,'is,ie,js,je'
      print*, is,ie,js,je
      print*,'ib,jb,slat,slon,elat,elon'
      print*, ib,jb,slat,slon,tlat(jb),tlon(ib)

       open(31,file='d23info.txt')
      write(31,'(2F9.3,2I9)') slat,slon,jb,ib
      close(31)
! -------------------

       open(12,file='HGT1.bin',recl=ib1*jb1*kb*4,
     &      form='unformatted',access='direct')
       open(13,file='TMP1.bin',recl=ib1*jb1*kb*4,
     &      form='unformatted',access='direct')
       open(14,file='UGRD1.bin',recl=ib1*jb1*kb*4,
     &      form='unformatted',access='direct')
       open(15,file='VGRD1.bin',recl=ib1*jb1*kb*4,
     &      form='unformatted',access='direct')
       open(16,file='SPFH1.bin',recl=ib1*jb1*kb*4,
     &      form='unformatted',access='direct')
       open(17,file='VVEL1.bin',recl=ib1*jb1*kb*4,
     &      form='unformatted',access='direct')
       open(18,file='RH1.bin',recl=ib1*jb1*kb*4,
     &      form='unformatted',access='direct')
       open(19,file='ugrd10m1.bin',recl=ib1*jb1*4,
     &      form='unformatted',access='direct')
       open(20,file='vgrd10m1.bin',recl=ib1*jb1*4,
     &      form='unformatted',access='direct')
       open(21,file='tmpsfc1.bin',recl=ib1*jb1*4,
     &      form='unformatted',access='direct')
       open(22,file='lhtfl1.bin',recl=ib1*jb1*4,
     &      form='unformatted',access='direct')
       open(23,file='prmsl1.bin',recl=ib1*jb1*4,
     &      form='unformatted',access='direct')

       open(32,file='HGT2.bin',recl=ib2*jb2*kb*4,
     &      form='unformatted',access='direct')
       open(33,file='TMP2.bin',recl=ib2*jb2*kb*4,
     &      form='unformatted',access='direct')
       open(34,file='UGRD2.bin',recl=ib2*jb2*kb*4,
     &      form='unformatted',access='direct')
       open(35,file='VGRD2.bin',recl=ib2*jb2*kb*4,
     &      form='unformatted',access='direct')
       open(36,file='SPFH2.bin',recl=ib2*jb2*kb*4,
     &      form='unformatted',access='direct')
       open(37,file='VVEL2.bin',recl=ib2*jb2*kb*4,
     &      form='unformatted',access='direct')
       open(38,file='RH2.bin',recl=ib2*jb2*kb*4,
     &      form='unformatted',access='direct')
       open(39,file='ugrd10m2.bin',recl=ib2*jb2*4,
     &      form='unformatted',access='direct')
       open(40,file='vgrd10m2.bin',recl=ib2*jb2*4,
     &      form='unformatted',access='direct')
       open(41,file='tmpsfc2.bin',recl=ib2*jb2*4,
     &      form='unformatted',access='direct')
       open(42,file='lhtfl2.bin',recl=ib2*jb2*4,
     &      form='unformatted',access='direct')
       open(43,file='prmsl2.bin',recl=ib2*jb2*4,
     &      form='unformatted',access='direct')

       read(12,rec=1) work1
      do k= 1,kb
         kiv= kb-k+1
      do j= 1,jb1
         jiv= jb1-j+1
         hgt1(:,j,k) = work1(:,jiv,kiv)  !! because of the yrev
      enddo
      enddo
       read(13,rec=1) work1
      do k= 1,kb
         kiv= kb-k+1
      do j= 1,jb1
         jiv= jb1-j+1
         tmp1(:,j,k) = work1(:,jiv,kiv)
      enddo
      enddo
       read(14,rec=1) work1
      do k= 1,kb
         kiv= kb-k+1
      do j= 1,jb1
         jiv= jb1-j+1
         ugrd1(:,j,k) = work1(:,jiv,kiv)
      enddo
      enddo
       read(15,rec=1) work1
      do k= 1,kb
         kiv= kb-k+1
      do j= 1,jb1
         jiv= jb1-j+1
         vgrd1(:,j,k) = work1(:,jiv,kiv)
      enddo
      enddo
       read(16,rec=1) work1
      do k= 1,kb
         kiv= kb-k+1
      do j= 1,jb1
         jiv= jb1-j+1
         spfh1(:,j,k) = work1(:,jiv,kiv)
      enddo
      enddo
       read(17,rec=1) work1
      do k= 1,kb
         kiv= kb-k+1
      do j= 1,jb1
         jiv= jb1-j+1
         vvel1(:,j,k) = work1(:,jiv,kiv)
      enddo
      enddo
       read(18,rec=1) work1
      do k= 1,kb
         kiv= kb-k+1
      do j= 1,jb1
         jiv= jb1-j+1
         rh1(:,j,k) = work1(:,jiv,kiv)
      enddo
      enddo
       read(19,rec=1) work1(:,:,1)
      do j= 1,jb1
         jiv= jb1-j+1
         ugrd10m1(:,j) = work1(:,jiv,1)
      enddo
       read(20,rec=1) work1(:,:,1)
      do j= 1,jb1
         jiv= jb1-j+1
         vgrd10m1(:,j) = work1(:,jiv,1)
      enddo
       read(21,rec=1) work1(:,:,1)
      do j= 1,jb1
         jiv= jb1-j+1
         tmpsfc1(:,j) = work1(:,jiv,1)
      enddo
       read(22,rec=1) work1(:,:,1)
      do j= 1,jb1
         jiv= jb1-j+1
         lhtfl1(:,j) = work1(:,jiv,1)
      enddo
       read(23,rec=1) work1(:,:,1)
      do j= 1,jb1
         jiv= jb1-j+1
         prmsl1(:,j) = work1(:,jiv,1)
      enddo


       read(32,rec=1) work2
      do k= 1,kb
         kiv= kb-k+1
      do j= 1,jb2
         jiv= jb2-j+1
         hgt2(:,j,k) = work2(:,jiv,kiv)
      enddo
      enddo
       read(33,rec=1) work2
      do k= 1,kb
         kiv= kb-k+1
      do j= 1,jb2
         jiv= jb2-j+1
         tmp2(:,j,k) = work2(:,jiv,kiv)
      enddo
      enddo
       read(34,rec=1) work2
      do k= 1,kb
         kiv= kb-k+1
      do j= 1,jb2
         jiv= jb2-j+1
         ugrd2(:,j,k) = work2(:,jiv,kiv)
      enddo
      enddo
       read(35,rec=1) work2
      do k= 1,kb
         kiv= kb-k+1
      do j= 1,jb2
         jiv= jb2-j+1
         vgrd2(:,j,k) = work2(:,jiv,kiv)
      enddo
      enddo
       read(36,rec=1) work2
      do k= 1,kb
         kiv= kb-k+1
      do j= 1,jb2
         jiv= jb2-j+1
         spfh2(:,j,k) = work2(:,jiv,kiv)
      enddo
      enddo
       read(37,rec=1) work2
      do k= 1,kb
         kiv= kb-k+1
      do j= 1,jb2
         jiv= jb2-j+1
         vvel2(:,j,k) = work2(:,jiv,kiv)
      enddo
      enddo
       read(38,rec=1) work2
      do k= 1,kb
         kiv= kb-k+1
      do j= 1,jb2
         jiv= jb2-j+1
         rh2(:,j,k) = work2(:,jiv,kiv)
      enddo
      enddo
       read(39,rec=1) work2(:,:,1)
      do j= 1,jb2
         jiv= jb2-j+1
         ugrd10m2(:,j) = work2(:,jiv,1)
      enddo
       read(40,rec=1) work2(:,:,1)
      do j= 1,jb2
         jiv= jb2-j+1
         vgrd10m2(:,j) = work2(:,jiv,1)
      enddo
       read(41,rec=1) work2(:,:,1)
      do j= 1,jb2
         jiv= jb2-j+1
         tmpsfc2(:,j) = work2(:,jiv,1)
      enddo
       read(42,rec=1) work2(:,:,1)
      do j= 1,jb2
         jiv= jb2-j+1
         lhtfl2(:,j) = work2(:,jiv,1)
      enddo
       read(43,rec=1) work2(:,:,1)
      do j= 1,jb2
         jiv= jb2-j+1
         prmsl2(:,j) = work2(:,jiv,1)
      enddo
       
      close(12)
      close(13)
      close(14)
      close(15)
      close(16)
      close(17)
      close(18)
      close(19)
      close(20)
      close(21)
      close(22)
      close(23)
      close(32)
      close(33)
      close(34)
      close(35)
      close(36)
      close(37)
      close(38)
      close(39)
      close(40)
      close(41)
      close(42)
      close(43)


! Interpolation from domain 2

       hgt(:,:,:)= undef
       tmp(:,:,:)= undef
       ugrd(:,:,:)= undef
       vgrd(:,:,:)= undef
       spfh(:,:,:)= undef
       vvel(:,:,:)= undef
       rh(:,:,:)= undef
       ugrd10m(:,:)= undef
       vgrd10m(:,:)= undef
       tmpsfc(:,:)= undef
       lhtfl(:,:)= undef
       prmsl(:,:)= undef

      do j=1,jb
         y=tlat(j)
      do i=1,ib
         x=tlon(i)

         ilo=(x -slon1) /dlon1 +1
         jlo=(y -slat1) /dlat1 +1

         if(ilo >= ib1) ilo= ib1-1
         if(ilo <= 0  ) ilo= 1
         if(jlo >= jb1) jlo= jb1-1
         if(jlo <= 0  ) jlo= 1

         ihi= ilo +1
         jhi= jlo +1



        if(hgt1(ilo-1,jhi+1,1)>1.e9 .or. hgt1(ihi+1,jhi+1,1)>1.e9 .or.
     &     hgt1(ilo-1,jlo-1,1)>1.e9 .or. hgt1(ihi+1,jlo-1,1)>1.e9)then
           goto 997
        endif

        if(hgt1(ilo ,jhi,1)<1.e9 .and. hgt1(ihi ,jhi,1)<1.e9 .and.
     &     hgt1(ilo ,jlo,1)<1.e9 .and. hgt1(ihi ,jlo,1)<1.e9 .and.
     &     vgrd1(ilo,jhi,1)<1.e9 .and. vgrd1(ihi,jhi,1)<1.e9 .and.
     &     vgrd1(ilo,jlo,1)<1.e9 .and. vgrd1(ihi,jlo,1)<1.e9)then

           s= (x -rlon1(ilo)) /(rlon1(ihi) -rlon1(ilo))
           t= (y -rlat1(jlo)) /(rlat1(jhi) -rlat1(jlo))
           alph1= (1-s)*   t
           alph2=    s *   t
           alph3= (1-s)*(1-t)
           alph4=    s *(1-t)

          do k=1,kb
             hgt(i,j,k)=
     &        alph1 *hgt1(ilo,jhi,k) +alph2 *hgt1(ihi,jhi,k)
     &       +alph3 *hgt1(ilo,jlo,k) +alph4 *hgt1(ihi,jlo,k)
             tmp(i,j,k)=
     &        alph1 *tmp1(ilo,jhi,k) +alph2 *tmp1(ihi,jhi,k)
     &       +alph3 *tmp1(ilo,jlo,k) +alph4 *tmp1(ihi,jlo,k)
             ugrd(i,j,k)=
     &        alph1 *ugrd1(ilo,jhi,k) +alph2 *ugrd1(ihi,jhi,k)
     &       +alph3 *ugrd1(ilo,jlo,k) +alph4 *ugrd1(ihi,jlo,k)
             vgrd(i,j,k)=
     &        alph1 *vgrd1(ilo,jhi,k) +alph2 *vgrd1(ihi,jhi,k)
     &       +alph3 *vgrd1(ilo,jlo,k) +alph4 *vgrd1(ihi,jlo,k)
             spfh(i,j,k)=
     &        alph1 *spfh1(ilo,jhi,k) +alph2 *spfh1(ihi,jhi,k)
     &       +alph3 *spfh1(ilo,jlo,k) +alph4 *spfh1(ihi,jlo,k)
             vvel(i,j,k)=
     &        alph1 *vvel1(ilo,jhi,k) +alph2 *vvel1(ihi,jhi,k)
     &       +alph3 *vvel1(ilo,jlo,k) +alph4 *vvel1(ihi,jlo,k)
             rh(i,j,k)=
     &        alph1 *rh1(ilo,jhi,k) +alph2 *rh1(ihi,jhi,k)
     &       +alph3 *rh1(ilo,jlo,k) +alph4 *rh1(ihi,jlo,k)
          enddo
             ugrd10m(i,j)=
     &        alph1 *ugrd10m1(ilo,jhi) +alph2 *ugrd10m1(ihi,jhi)
     &       +alph3 *ugrd10m1(ilo,jlo) +alph4 *ugrd10m1(ihi,jlo)
             vgrd10m(i,j)=
     &        alph1 *vgrd10m1(ilo,jhi) +alph2 *vgrd10m1(ihi,jhi)
     &       +alph3 *vgrd10m1(ilo,jlo) +alph4 *vgrd10m1(ihi,jlo)
             tmpsfc(i,j)=
     &        alph1 *tmpsfc1(ilo,jhi) +alph2 *tmpsfc1(ihi,jhi)
     &       +alph3 *tmpsfc1(ilo,jlo) +alph4 *tmpsfc1(ihi,jlo)
             lhtfl(i,j)=
     &        alph1 *lhtfl1(ilo,jhi) +alph2 *lhtfl1(ihi,jhi)
     &       +alph3 *lhtfl1(ilo,jlo) +alph4 *lhtfl1(ihi,jlo)
             prmsl(i,j)=
     &        alph1 *prmsl1(ilo,jhi) +alph2 *prmsl1(ihi,jhi)
     &       +alph3 *prmsl1(ilo,jlo) +alph4 *prmsl1(ihi,jlo)

        endif

  997 continue

      enddo
      enddo

!     goto 998

! Interpolation from domain 3

      do j=1,jb
         y=tlat(j)
      do i=1,ib
         x=tlon(i)

        if(x<=slon2 .or. x>=elon2 .or. y<=slat2 .or. y>=elat2)then
           goto 999
        endif

         ilo=(x -slon2) /dlon2 +1
         jlo=(y -slat2) /dlat2 +1

         if(ilo >= ib2) ilo= ib2-1
         if(ilo <= 0  ) ilo= 1
         if(jlo >= jb2) jlo= jb2-1
         if(jlo <= 0  ) jlo= 1

         ihi= ilo +1
         jhi= jlo +1

        if(hgt2(ilo-1,jhi+1,1)>1.e9 .or. hgt2(ihi+1,jhi+1,1)>1.e9 .or.
     &     hgt2(ilo-1,jlo-1,1)>1.e9 .or. hgt2(ihi+1,jlo-1,1)>1.e9)then
           goto 999
        endif

        if(hgt2(ilo ,jhi,1)<1.e9 .and. hgt2(ihi ,jhi,1)<1.e9 .and.
     &     hgt2(ilo ,jlo,1)<1.e9 .and. hgt2(ihi ,jlo,1)<1.e9 .and.
     &     vgrd2(ilo,jhi,1)<1.e9 .and. vgrd2(ihi,jhi,1)<1.e9 .and.
     &     vgrd2(ilo,jlo,1)<1.e9 .and. vgrd2(ihi,jlo,1)<1.e9)then

           s= (x -rlon2(ilo)) /(rlon2(ihi) -rlon2(ilo))
           t= (y -rlat2(jlo)) /(rlat2(jhi) -rlat2(jlo))

           alph1= (1-s)*   t
           alph2=    s *   t
           alph3= (1-s)*(1-t)
           alph4=    s *(1-t)

          do k=1,kb
             hgt(i,j,k)=
     &        alph1 *hgt2(ilo,jhi,k) +alph2 *hgt2(ihi,jhi,k)
     &       +alph3 *hgt2(ilo,jlo,k) +alph4 *hgt2(ihi,jlo,k)
             tmp(i,j,k)=
     &        alph1 *tmp2(ilo,jhi,k) +alph2 *tmp2(ihi,jhi,k)
     &       +alph3 *tmp2(ilo,jlo,k) +alph4 *tmp2(ihi,jlo,k)
             ugrd(i,j,k)=
     &        alph1 *ugrd2(ilo,jhi,k) +alph2 *ugrd2(ihi,jhi,k)
     &       +alph3 *ugrd2(ilo,jlo,k) +alph4 *ugrd2(ihi,jlo,k)
             vgrd(i,j,k)=
     &        alph1 *vgrd2(ilo,jhi,k) +alph2 *vgrd2(ihi,jhi,k)
     &       +alph3 *vgrd2(ilo,jlo,k) +alph4 *vgrd2(ihi,jlo,k)
             spfh(i,j,k)=
     &        alph1 *spfh2(ilo,jhi,k) +alph2 *spfh2(ihi,jhi,k)
     &       +alph3 *spfh2(ilo,jlo,k) +alph4 *spfh2(ihi,jlo,k)
             vvel(i,j,k)=
     &        alph1 *vvel2(ilo,jhi,k) +alph2 *vvel2(ihi,jhi,k)
     &       +alph3 *vvel2(ilo,jlo,k) +alph4 *vvel2(ihi,jlo,k)
             rh(i,j,k)=
     &        alph1 *rh2(ilo,jhi,k) +alph2 *rh2(ihi,jhi,k)
     &       +alph3 *rh2(ilo,jlo,k) +alph4 *rh2(ihi,jlo,k)
          enddo
             ugrd10m(i,j)=
     &        alph1 *ugrd10m2(ilo,jhi) +alph2 *ugrd10m2(ihi,jhi)
     &       +alph3 *ugrd10m2(ilo,jlo) +alph4 *ugrd10m2(ihi,jlo)
             vgrd10m(i,j)=
     &        alph1 *vgrd10m2(ilo,jhi) +alph2 *vgrd10m2(ihi,jhi)
     &       +alph3 *vgrd10m2(ilo,jlo) +alph4 *vgrd10m2(ihi,jlo)
             tmpsfc(i,j)=
     &        alph1 *tmpsfc2(ilo,jhi) +alph2 *tmpsfc2(ihi,jhi)
     &       +alph3 *tmpsfc2(ilo,jlo) +alph4 *tmpsfc2(ihi,jlo)
             lhtfl(i,j)=
     &        alph1 *lhtfl2(ilo,jhi) +alph2 *lhtfl2(ihi,jhi)
     &       +alph3 *lhtfl2(ilo,jlo) +alph4 *lhtfl2(ihi,jlo)
             prmsl(i,j)=
     &        alph1 *prmsl2(ilo,jhi) +alph2 *prmsl2(ihi,jhi)
     &       +alph3 *prmsl2(ilo,jlo) +alph4 *prmsl2(ihi,jlo)


        endif
  999 continue

      enddo
      enddo

  998 continue

       open(52,file='HGT.bin',recl=ib*jb*kb*4,
     &      form='unformatted',access='direct')
       open(53,file='TMP.bin',recl=ib*jb*kb*4,
     &      form='unformatted',access='direct')
       open(54,file='UGRD.bin',recl=ib*jb*kb*4,
     &      form='unformatted',access='direct')
       open(55,file='VGRD.bin',recl=ib*jb*kb*4,
     &      form='unformatted',access='direct')
       open(56,file='SPFH.bin',recl=ib*jb*kb*4,
     &      form='unformatted',access='direct')
       open(57,file='VVEL.bin',recl=ib*jb*kb*4,
     &      form='unformatted',access='direct')
       open(58,file='RH.bin',recl=ib*jb*kb*4,
     &      form='unformatted',access='direct')
       open(59,file='ugrd10m.bin',recl=ib*jb*4,
     &      form='unformatted',access='direct')
       open(60,file='vgrd10m.bin',recl=ib*jb*4,
     &      form='unformatted',access='direct')
       open(61,file='tmpsfc.bin',recl=ib*jb*4,
     &      form='unformatted',access='direct')
       open(62,file='lhtfl.bin',recl=ib*jb*4,
     &      form='unformatted',access='direct')
       open(63,file='prmsl.bin',recl=ib*jb*4,
     &      form='unformatted',access='direct')
       open(71,file='domain23.bin',recl=ib*jb*4,
     &      form='unformatted',access='direct')

      write(52,rec=1) hgt
      write(53,rec=1) tmp
      write(54,rec=1) ugrd
      write(55,rec=1) vgrd
      write(56,rec=1) spfh
      write(57,rec=1) vvel
      write(58,rec=1) rh
      write(59,rec=1) ugrd10m
      write(60,rec=1) vgrd10m
      write(61,rec=1) tmpsfc
      write(62,rec=1) lhtfl
      write(63,rec=1) prmsl

      do k=1,kb
        write(71,rec=k)      hgt(:,:,k)
        write(71,rec=k+kb*1) tmp(:,:,k)
        write(71,rec=k+kb*2) ugrd(:,:,k)
        write(71,rec=k+kb*3) vgrd(:,:,k)
        write(71,rec=k+kb*4) spfh(:,:,k)
        write(71,rec=k+kb*5) vvel(:,:,k)
        write(71,rec=k+kb*6) rh(:,:,k)
      enddo
        write(71,rec=1+kb*7) ugrd10m
        write(71,rec=2+kb*7) vgrd10m
        write(71,rec=3+kb*7) tmpsfc
        write(71,rec=4+kb*7) lhtfl
        write(71,rec=5+kb*7) prmsl

      close(52)
      close(53)
      close(54)
      close(55)
      close(56)
      close(57)
      close(58)
      close(59)
      close(60)
      close(61)
      close(62)
      close(63)
      close(71)

      print*,'== Merging domain 2 and 3 is completed =='
!-----------------------------------------------------------------------
      end program Interpb_d23
!-----------------------------------------------------------------------
