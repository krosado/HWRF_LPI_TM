subroutine var_summary(name,grid,im,jm,diag,what)
  implicit none
  integer, intent(in) :: im,jm
  character(*), intent(in) :: name, what
  real, intent(in) :: grid(im,jm)
  logical, intent(in) :: diag

  real(kind=8) :: accum,meanval,accumsq,rim,rjm,var,std,rms,rcount
  integer(kind=8) :: count,numnonzero,bigim,bigjm
  real :: minval,maxval
  integer :: i,j
  integer, parameter :: nx=8,ny=8

  print '(A,"(",I0,",",I0,") : ",A)',trim(name),im,jm,trim(what)
  print '("Value at middle of grid (",I0,",",I0,") = ",F10.5)', &
       nint(im/2.0),nint(jm/2.0),grid(nint(im/2.0),nint(jm/2.0))

  if ( .not. diag ) then
     ! Not doing diagnostic output, so don't do the expensive calculations.
     return
  end if

  bigim=im
  bigjm=jm
  rim=im
  rjm=jm

  accum=0
  accumsq=0
  numnonzero=0
  count=bigim*bigjm
  minval=grid(1,1)
  maxval=grid(1,1)
  do j=1,jm
     do i=1,im
        if(grid(i,j) > maxval) maxval=grid(i,j)
        if(grid(i,j) < minval) minval=grid(i,j)
        if(grid(i,j) /= 0) numnonzero=numnonzero+1
        accum=accum+grid(i,j)
        accumsq=accumsq+grid(i,j)*grid(i,j)
     enddo
  enddo

  rcount=count
  meanval=accum / rcount
  var=1/(rcount-1) * ( accumsq - 2*meanval*accum + rcount*meanval*meanval )
  std=sqrt(var)
  rms=sqrt(accumsq/rcount)

  if(count>1) then
     print '(A,"(1:",I0,",1:",I0,"):")',trim(name),IM,JM
     print '("    min =",F10.5,", max =",F10.5,", mean =",F10.5,", total =",F13.5)',minval,maxval,meanval,accum
     print '("    number of non-zero = ",I0," =",F7.3,"%")',numnonzero,(100.0*numnonzero)/count
     print '("    unbiased est.: RMS=",F10.5,", stdev=",F10.5,", variance=",F20.10)',rms,std,var
  end if

  print '(A," sample values:")',trim(name)

  print '(7(" ",F10.5))',grid( &
      1+nint((rim-1)*(/ 0.0, 1.0/6.0, 2.0/6.0, 3.0/6.0, 4.0/6.0, 5.0/6.0, 1.0 /)), &
      1+nint((rjm-1)*(/ 0.0, 1.0/10.0, 2.0/10.0, 3.0/10.0, 4.0/10.0, 5.0/10.0, 6.0/10.0, 7.0/10.0, 8.0/10.0, 9.0/10.0, 1.0 /)))

end subroutine var_summary
