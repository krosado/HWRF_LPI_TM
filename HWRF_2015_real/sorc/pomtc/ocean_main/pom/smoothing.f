      subroutine smoothing(f,ni,nj,sm,smk,n_west,n_east,n_south,n_north)
c*****************************************
c smoothing(sm>0) and de-smoothing(sm<0).
c this code was developed by dr. ginis,
c rewritten by erxuan fu. 12/1/94,gso,uri.
c rewritten by r. yablonsky. 6/15/12.
c*****************************************
      implicit none
      integer i,j,ni,nj
      integer n_west,n_east,n_south,n_north !RMY: add this line
      real sm
      real f(ni,nj),f1(ni,nj),smk(ni,nj)
c
c**********************************************
c x-smoothing
c**********************************************
      do j=1,nj   !RMY: 1,nj(biju);2,nj-1(orig)
        do i=1,ni !RMY: 1,ni(biju);2,ni-1(orig)
          f1(i,j)=f(i,j)
        end do
      end do
c
      do j=1,nj   !RMY: 1,nj(biju);2,nj-1(orig)
        do i=2,ni-1
          if(smk(i,j).ne.0.) then
            if(smk(i-1,j).eq.0.or.(n_west.eq.-1.and.i.eq.2)) then      !RMY: add n_west
              if(smk(i+1,j).eq.0.or.(n_east.eq.-1.and.i.eq.ni-1)) then !RMY: add n_east
                f1(i,j)=f(i,j)
              else
                f1(i,j)=f(i,j)+sm*(f(i+1,j)+f(i,j)-2.*f(i,j))
              end if
            else
              if(smk(i+1,j).eq.0.or.(n_east.eq.-1.and.i.eq.ni-1)) then !RMY: add n_east
                f1(i,j)=f(i,j)+sm*(f(i,j)+f(i-1,j)-2.*f(i,j))
              else
                f1(i,j)=f(i,j)+sm*(f(i+1,j)+f(i-1,j)-2.*f(i,j))
              end if
            end if
          else
            f1(i,j)=f(i,j)
          end if
        end do
      end do
c
      do i=2,ni-1
        do j=1,nj !RMY: 1,nj(biju);2,nj-1(orig)
          f(i,j)=f1(i,j)
        end do
      end do

      call exchange2d_mpi(f,ni,nj)
c
c**********************************************
c y-smoothing
c**********************************************
      do i=1,ni   !RMY: 1,ni(biju);2,ni-1(orig)
        do j=2,nj-1
          if(smk(i,j).ne.0.) then
            if(smk(i,j-1).eq.0.or.(n_south.eq.-1.and.j.eq.2)) then      !RMY: add n_south
              if(smk(i,j+1).eq.0.or.(n_north.eq.-1.and.j.eq.nj-1)) then !RMY: add n_north
                f1(i,j)=f(i,j)
              else
                f1(i,j)=f(i,j)+sm*(f(i,j+1)+f(i,j)-2.*f(i,j))
              end if
            else
              if(smk(i,j+1).eq.0.or.(n_north.eq.-1.and.j.eq.nj-1)) then !RMY: add n_north
                f1(i,j)=f(i,j)+sm*(f(i,j)+f(i,j-1)-2.*f(i,j))
              else
                f1(i,j)=f(i,j)+sm*(f(i,j+1)+f(i,j-1)-2.*f(i,j))
              end if
            end if
          else
            f1(i,j)=f(i,j)
          end if
        end do
      end do
c 
      do j=2,nj-1
        do i=1,ni !RMY: 1,ni(biju);2,ni-1(orig)
          f(i,j)=f1(i,j)
        end do
      end do

      call exchange2d_mpi(f,ni,nj)
c
      return
      end
