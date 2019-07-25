integer function ismin(n,x,incx)
  integer, intent(in) :: n, incx
  real, intent(in) :: x(n)

  integer j ! current location being searched
  integer k ! index of minimum element found so far

  if(incx>0) then
     k=1
     do j=2,n
        if( x(j) < x(k) ) then
           k=j
        endif
     enddo
     ismin=k
  else
     k=n
     do j=n-1,1,-1
        if( x(j) < x(k) ) then
           k=j
        endif
     enddo
     ismin=n-k+1
  end if

end function ismin
