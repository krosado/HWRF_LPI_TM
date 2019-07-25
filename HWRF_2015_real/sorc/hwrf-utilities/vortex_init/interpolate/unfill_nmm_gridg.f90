subroutine unfill_nmm_gridg(gin,nx,ny,gout,igtype)

!  convert input staggered grid to output filled grid

!   --> gin:   input filled grid
!   --> nx,ny: input grid dimensions
!  <--  gout:  output staggered grid
!   --> igtype: =1, then (1,1) on staggered grid is at corner of grid
!               =2, then (1,1) is staggered

implicit none

  integer nx,ny,igtype
  real(4) gin(2*nx-1,ny),gout(nx,ny)
  
  integer i,j

  if(igtype==1) then
     do j=1,ny,2
        do i=1,nx
           gout(i,j)=gin(2*i-1,j)
        end do
     end do
     do j=2,ny,2
        do i=1,nx-1
           gout(i,j)=gin(2*i,j)
        end do
     end do
  else
     do j=1,ny,2
        do i=1,nx-1
           gout(i,j)=gin(2*i,j)
        end do
     end do
     do j=2,ny,2
        do i=1,nx
           gout(i,j)=gin(2*i-1,j)
        end do
     end do
  end if

end subroutine unfill_nmm_gridg

