!----------------------------------------------------------------------
!                            tWindSum_v1.1.f
!
! Calulate tangential wind and summation of the tangential wind
! Ver 1.0, DEC 02 2011, Coded by IH Kwon
! Ver 1.1, JUN 13 2012, Add mrex
!
!----------------------------------------------------------------------
      subroutine tWindSum(ugrd,vgrd,vtsum,ib,jb,mradi,mrex)
!----------------------------------------------------------------------
      implicit none
      real,dimension(ib,jb),intent(in   ) :: ugrd,vgrd
      integer              ,intent(in   ) :: ib,jb,mradi,mrex
      real,dimension(ib,jb),intent(  out) :: vtsum

      real    :: sum,dx,dy,angl,vtgr
      integer :: i,j,m,n,is,ie,js,je,mrex1
! --------------------------------------------------

       mrex1= mrex +1
     
      print*,'mrex,mrex1=',mrex,mrex1
      print*,'mradius=',mradi

       is=   mradi +1
       ie=ib-mradi -1
       js=   mradi +1
       je=jb-mradi -1

       vtsum(:,:)= 0.

      do j=js,je
      do i=is,ie
!                              444444333
!                              444444333
!                              444444333
!                              111   333
!                              111   333
!                              111   333
!                              111222222
!                              111222222
!                              111222222
           sum= 0.
        do n=j-mradi,j+mrex                             !! 1
        do m=i-mradi,i-mrex1
           dx= m -i
           dy= n -j
           angl= atan2(dy,dx)
           vtgr=-ugrd(m,n)*sin(angl) +vgrd(m,n)*cos(angl)
           sum= sum +vtgr
        enddo
        enddo
        do n=j-mradi,j-mrex1                            !! 2
        do m=i-mrex,i+mradi
           dx= m -i
           dy= n -j
           angl= atan2(dy,dx)
           vtgr=-ugrd(m,n)*sin(angl) +vgrd(m,n)*cos(angl)
           sum= sum +vtgr
        enddo
        enddo
        do n=j-mrex,j+mradi                             !! 3
        do m=i+mrex1,i+mradi
           dx= m -i
           dy= n -j
           angl= atan2(dy,dx)
           vtgr=-ugrd(m,n)*sin(angl) +vgrd(m,n)*cos(angl)
           sum= sum +vtgr
        enddo
        enddo
        do n=j+mrex1,j+mradi                            !! 4
        do m=i-mradi,i+mrex
           dx= m -i
           dy= n -j
           angl= atan2(dy,dx)
           vtgr=-ugrd(m,n)*sin(angl) +vgrd(m,n)*cos(angl)
           sum= sum +vtgr
        enddo
        enddo
         vtsum(i,j)= sum
      enddo
      enddo

       open(91,file='tanwind.bin',recl=ib*jb*4,
     &     form='unformatted',access='direct')
      write(91,rec=1) vtsum
      close(91)

!----------------------------------------------------------------------
      end subroutine tWindSum
!----------------------------------------------------------------------
