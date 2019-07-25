      subroutine getsm(idrt,im,jm,jcap,nc)
c
c Biju Thomas on 10/30/00
c
      USE setparms; USE sfcio_module
      type(sfcio_head):: head
      type(sfcio_data):: data

      real s0(nc)
      real(4), allocatable :: s0_4byte(:)
      real gz(im*jm)
c      real gtst(im*jm),glsm(im*jm)
      real sst(im,jm),msk(im,jm),sst1(im,jm),msk1(im,jm)
      integer, parameter :: iosfc=11,iosig=12

      tlap=6.7e-5
c

      print *,'Beginning of getsm....'
      print *,'idrt im jm jcap = ',idrt,im,jm,jcap
      print *,'nc = ',nc

      allocate (s0_4byte(nc),stat=istat)
      if (istat /= 0) then
        print *,'!!! ERROR ALLOCATING s0_4byte in getsm....'
        return
      endif
c
c     do sptez for zstar.  Read the variables into
c     the 4-byte arrays, then copy over to 8-byte arrays...

      read(iosig) s0_4byte
      s0 = s0_4byte
      print *,'Just before sptez call, '
      print *,'idrt im jm jcap = ',idrt,im,jm,jcap
      call sptez(0,jcap,idrt,im,jm,s0,gz,1)
      deallocate (s0_4byte)
c     change height units...

      do ij=1,im*jm
        gz (ij)=100.*gz (ij)
      enddo

c     Next, get sfc temp & land-sea mask from bges file

c      call rdbges(11,im*jm,gtst,glsm)
      call sfcio_srohdc (iosfc,'for11',head,data,iret)
      if (iret == 0) then
        sst = data%tsea
        msk = data%slmsk
      else
        print *,' '
        print *,'ERROR reading sst & land mask from sfc file.'
        print *,'ERROR code = ',iret,'  ....EXITING....'
        print *,' '
        stop 94
      endif


c     one level fields... sfc temp, land-seamask, wetness(dum),zstar,pstar

      ij=1
      do j=1,jm
        do i=1,im
          sst(i,j)=sst(i,j)+tlap*gz (ij)
          if ( msk(i,j).gt.1 ) then
            msk(i,j) = 1.
          end if
          ij=ij+1
        enddo
      enddo
     
      do j=1,jm
       jj = jm-j+1
       do i=1,im/2
         sst1(i,j)=sst(im/2+i,jj)
         msk1(i,j)=msk(im/2+i,jj)
       enddo
       do i=1+im/2,im
         sst1(i,j)=sst(i-im/2,jj)
         msk1(i,j)=msk(i-im/2,jj)
       enddo
      enddo
      
      write(74)sst1
      write(77)msk1

      return
      end
