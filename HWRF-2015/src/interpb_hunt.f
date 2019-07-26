!---------------------------------------------------------------------
!                        interpb_hunt_v1.2.f
!  Ordered table for a linear interpolation
!
!  Given an array xx(n), and given a value x, returns a value jlo that
!  x is between xx(jlo) and xx(jlo+1).
!  jlo on inut is taken as the initial guess for jlo on output
!  Reference: Numerical recipes in Fortran 77

!  Ver 1.0, JUL 12 1999, IH Kwon  jlo varies from 0 to n
!  Ver 1.1, APR 29 2011, IH Kwon, jlo varies from 1 to n-1
!  Ver 1.1d,JUN 03 2011, IH Kwon, Double Precision of x & xx
!  Ver 1.2, NOV 29 2011, IH Kwon, bug fixed

!---------------------------------------------------------------------
      subroutine hunt(xx,n,x,jlo)
! --------------------------------------------------------------------
      implicit none
      real(kind=8),intent(in) :: x, xx(n)
      integer,intent(inout) :: jlo
      integer,intent(in)    :: n
      integer :: inc,jhi,jm
      logical :: ascnd
! --------------------------------

      ascnd= xx(n) .ge. xx(1)           ! True if ascending order of table,
                                        !   false otherwise
      if(jlo <= 0 .or. jlo >= n)then    ! Not useful input guess.
         jlo= 0                         ! Go immediately to bisection.
         jhi= n +1
         goto 3
      endif

         inc= 1
      if(x >= xx(jlo) .eqv. ascnd) then  ! Hunt up:
    1    jhi= jlo +inc
        if(jhi > n)then                  ! Done hunting, since off end of table
           jhi= n +1
        else if(x >= xx(jhi) .eqv. ascnd)then    ! Not done hunting
           jlo= jhi
           inc= inc +inc
           goto 1                        ! Try again
        endif
      else                               ! Hunt down:
         jhi= jlo
    2    jlo= jhi -inc
        if(jlo < 1)then                  ! Done hunting, since off end of table
           jlo= 0
        else if(x < xx(jlo) .eqv. ascnd)then     ! Not done hunting
           jhi= jlo
           inc= inc +inc
           goto 2                        ! Try again
        endif                            ! Done hunting, value bracketed
      endif

    3 if(jhi-jlo == 1)then
        if(x >= xx(n) .eqv. ascnd) jlo= n -1
        if(x <= xx(1) .eqv. ascnd) jlo= 1
        if(x <= xx(n) .neqv. ascnd) jlo= n -1
        if(x >= xx(1) .neqv. ascnd) jlo= 1
        goto 4
      endif

         jm= (jhi +jlo) /2                 ! bisection
      if(x >= xx(jm) .eqv. ascnd)then
         jlo= jm
      else
         jhi= jm
      endif
         goto 3

    4 continue

! ------------------------------------------------------------------
      end subroutine hunt
!-------------------------------------------------------------------
