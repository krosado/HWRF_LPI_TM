!----------------------------------------------------------------------
!                        findcenter_v1.3.f
!
! Subroutine to determine the center of TC
! The TC center is calculated in subgrid using the parabolic function.
!
! Ver 1.0, APR 27 2010, coded by IH Kwon
! Ver 1.1, JAN 21 2011, Boundary treatment
! Ver 1.2, JUN 20 2011, Add a search radius and switch
! Ver 1.3, JUN 13 2012, Bug fixed
!
!  iswch= +1 : searching a position of the maximum 
!  iswch= -1 : searching a position of the minimum 
!
!----------------------------------------------------------------------
      subroutine findCenter
     &(work,xlon,ylat,clon,clat,elon,elat,ib,jb,radi,iswch)
!----------------------------------------------------------------------
      implicit none
      real,dimension(ib,jb),intent(in   ) :: work
      real,dimension(ib)   ,intent(in   ) :: xlon
      real,dimension(jb)   ,intent(in   ) :: ylat
      real                 ,intent(in   ) :: clat,clon,radi
      integer              ,intent(in   ) :: ib,jb,iswch
      real                 ,intent(  out) :: elat,elon

      real  :: cmin,cmax,pbe,pce,paf,vbe,vce,vaf,tmp1,tmp2,ddeg
      integer :: i,j,it,jt,icen,jcen,mrad
! --------------------------------------------------

      ddeg= ylat(2)-ylat(1)
      mrad= radi /ddeg
      mrad= max(mrad,1)

!     print*,'radi,ddeg,mrad in fc:',radi,ddeg,mrad

! find the nearest grid point to the clon & clat

       jt=( clat -ylat(1) ) *(jb-1) / ( ylat(jb) -ylat(1) ) +1
       it=( clon -xlon(1) ) *(ib-1) / ( xlon(ib) -xlon(1) ) +1

       tmp1= abs(clon -xlon(it))
       tmp2= abs(clon -xlon(it+1))
      if(tmp2 < tmp1) it=it+1

       tmp1= abs(clat -ylat(jt))
       tmp2= abs(clat -ylat(jt+1))
      if(tmp2 < tmp1) jt=jt+1

!     print*,'it,jt:',it,jt

! --------------------------
      if(iswch == 1)then
! --------------------------

! find the grid point having the maximum value
         cmax= -10.e8
      do j= jt-mrad,jt+mrad
      do i= it-mrad,it+mrad
        if(work(i,j) > cmax)then
           cmax= work(i,j)
           icen= i
           jcen= j
        endif
      enddo 
      enddo 

! Calculate the peak point position in subgrid

       pbe= xlon(icen-1)
       pce= xlon(icen  )
       paf= xlon(icen+1)
       vbe= work(icen-1,jcen)
       vce= work(icen  ,jcen)
       vaf= work(icen+1,jcen)

      if(vbe > vce)then
         elon= pbe
      elseif(vaf > vce)then
         elon= paf
      else
         elon= exactc(pbe,pce,paf,vbe,vce,vaf)
      endif

       pbe= ylat(jcen-1)
       pce= ylat(jcen  )
       paf= ylat(jcen+1)
       vbe= work(icen,jcen-1)
       vce= work(icen,jcen  )
       vaf= work(icen,jcen+1)

      if(vbe > vce)then
         elat= pbe
      elseif(vaf > vce)then
         elat= paf
      else
         elat= exactc(pbe,pce,paf,vbe,vce,vaf)
      endif

! ---------------------------
      elseif(iswch == -1)then
! ---------------------------

! find the grid point having the minimun value
         cmin= 10.e8
      do j= jt-mrad,jt+mrad
      do i= it-mrad,it+mrad
        if(work(i,j) < cmin)then
           cmin= work(i,j)
           icen= i
           jcen= j
        endif
      enddo
      enddo

! Calculate the peak point position in subgrid

       pbe= xlon(icen-1)
       pce= xlon(icen  )
       paf= xlon(icen+1)
       vbe= work(icen-1,jcen)
       vce= work(icen  ,jcen)
       vaf= work(icen+1,jcen)

      if(vbe < vce)then
         elon= pbe
      elseif(vaf < vce)then
         elon= paf
      else
         elon= exactc(pbe,pce,paf,vbe,vce,vaf)
      endif

       pbe= ylat(jcen-1)
       pce= ylat(jcen  )
       paf= ylat(jcen+1)
       vbe= work(icen,jcen-1)
       vce= work(icen,jcen  )
       vaf= work(icen,jcen+1)

      if(vbe < vce)then
         elat= pbe
      elseif(vaf < vce)then
         elat= paf
      else
         elat= exactc(pbe,pce,paf,vbe,vce,vaf)
      endif

! ---------------------------
      else
! ---------------------------

         stop 'invalid switch'

! ---------------------------
      endif
! ---------------------------

! --------------------------------------------------
      Contains
      REAl FUNCTION exactc(ak1,ak0,ak2,y1,y0,y2)
      IMPLICIT NONE
      REAL :: ak1,ak0,ak2,x1,x0,x2,y1,y0,y2,u,d
       x1 = ak0 - ak1
       x0 = 0
       x2 = ak2 - ak0
       u   =(x1**2. *y2 - x2**2. *y1 - (x1**2. - x2**2.) *y0) *(-1)
       d   =(x1     *y2 + x2     *y1 - (x1     + x2    ) *y0) *(2.)
       exactc = ak0 + u / d
      END FUNCTION exactc
!----------------------------------------------------------------------
      end subroutine findCenter
!----------------------------------------------------------------------
