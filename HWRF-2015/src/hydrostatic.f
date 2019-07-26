!-----------------------------------------------------------------------
      subroutine hydrostat(hgtcr,thydro,mt,kb,plev)
!-----------------------------------------------------------------------
      implicit none
      real,intent(in),dimension(mt,kb)  :: hgtcr
      real,intent(out),dimension(mt,kb) :: thydro
      real,intent(in),dimension(kb) :: plev
      integer,intent(in) :: mt,kb
      real :: r,pdif,gravi,gp1,gp2
      integer ::  m,k,lint1,lint2

       r = 287.05
       gravi=9.8
       thydro(:,:)= 1.e20
       
      do k= 2, kb-1
         lint1= plev(k-1) -plev(k)
         lint2= plev(k) -plev(k+1)
        if(lint1 == lint2)then
          do m= 1, mt
             gp1= hgtcr(m,k-1) *gravi
             gp2= hgtcr(m,k+1) *gravi
            if(gp1 < 1.e10 .and. gp2 < 1.e10)then
               pdif= (gp1-gp2) /(plev(k-1)-plev(k+1))
               thydro(m,k)= - pdif *plev(k) /r
            endif
          enddo
        endif
      enddo

!-----------------------------------------------------------------------
      end subroutine hydrostat
!-----------------------------------------------------------------------
