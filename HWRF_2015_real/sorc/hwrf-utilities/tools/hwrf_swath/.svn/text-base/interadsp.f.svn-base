      subroutine interad(mode,ndp        ,xin,yin,datain,
     a     nxpts,nypts,imx,xg,yg,value,wno,idn,ncount,
     a     slat,wlon,reslatlon)
      parameter (nobmx=5,nsearch=18,nnear=(nsearch*2+1)*(nsearch*2+1))
      dimension wno(nobmx,imx,nypts)         
      dimension idn(nobmx,imx,nypts)
      dimension ncount(imx,nypts)          
      dimension xin(ndp),yin(ndp),xg(imx),yg(nypts)
      dimension datain(ndp),value(imx,nypts)

      real :: multidistsq(nobmx,imx,nypts) ,distsq,dist
      real, intent(in) :: slat,wlon,reslatlon
      integer :: inear,jnear,idist,imove

      spcval=-999.
      ncrit=2
      rinf=1.5

      !NOTE: if you change rinf, you must change nsearch.

      if(nint(rinf/reslatlon)+2>nsearch) then
 1       format('ASSERTION FAILURE: nint(rinf/reslatlon+2) = ',I0,' < ',&
     &          'nsearch = ',I0)
         write(0,1) nint(rinf/reslatlon)+2,nsearch
         write(6,1) nint(rinf/reslatlon)+2,nsearch
         stop 3
      endif

      rinfsq=rinf*rinf
      pi=4.*atan(1.)
      pid180=pi/180.
      a=1. 
      b=5./(rinf*rinf)
c       print*,'ndp,nxpts,nypts,nobmx',ndp,nxpts,nypts,nobmx

c     Don't recalculate interpolation information after first pass:
      if (mode.gt.1) goto 210

c     Initialize interpolation arrays:
      ncount=0
      wno=0.0
      idn=0
      multidistsq=0.0

c     Loop over all source points:
      do nd=1,ndp
c        Determine nearby location in lat-lon grid
         inear=(xin(nd)-wlon)/reslatlon+1
         jnear=(yin(nd)-slat)/reslatlon+1
         
         idist=1
         do jn=max(1,jnear-nsearch),min(nypts,jnear+nsearch)
            do in=max(1,inear-nsearch),min(nxpts,inear+nsearch)
               distsq=(cos(pid180*yg(jn))*(xin(nd)-xg(in)))**2 +
     a              (yin(nd)-yg(jn))**2
               if(distsq<=rinfsq) then
                  ! Add this "observation" to the list of points near
                  ! this lat-lon point.
                  if(ncount(in,jn)==0) then
                     ! No "observations" are yet recorded, so add this
                     ! as the only one:
                     nc=1
                     ncount(in,jn)=nc
                     wno(nc,in,jn)=a*exp(-(b)*distsq)
                     idn(nc,in,jn)=nd
                     multidistsq(nc,in,jn)=distsq
                  else
                     ! There are already "observations" present for this
                     ! lat-lon point, so we need to see where to add this
                     ! in the list.  The list is sorted from closest to
                     ! farthest, with a maximum of nobmx points.

                     nc=ncount(in,jn)
                     do nct=nc,1,-1
                        if(distsq<multidistsq(nct,in,jn)) then
                           ! This point is closer than the point at index
                           ! nct, so we insert here.  If there are already
                           ! nobmx "observations" then the last one is
                           ! discarded.
                           nc=min(nc+1,nobmx)
                           do imove=nct+1,nc
                              multidistsq(imove,in,jn)=
     a                             multidistsq(imove-1,in,jn)
                              wno(imove,in,jn)=wno(imove-1,in,jn)
                              idn(imove,in,jn)=idn(imove-1,in,jn)
                           enddo
                           ncount(in,jn)=nc
                           wno(nct,in,jn)=a*exp(-(b)*distsq)
                           idn(nct,in,jn)=nd
                           multidistsq(nct,in,jn)=distsq
                        endif
                     enddo
                  endif
               endif
            enddo
         enddo
      enddo

c     print *, 'ncount ', ncount
c     now do interpolation ...........
  210 do 300 jn=1,nypts
      do 300 in=1,nxpts
      vsum=0
      sum=0
       do 250 ic=1,ncount(in,jn)
        vsum=vsum+datain(idn(ic,in,jn))*wno(ic,in,jn)
        sum=sum+wno(ic,in,jn)
c	 print*,'sum=',sum,'  vsum=',vsum
  250  continue
      if(ncount(in,jn).ge.ncrit)  then
      value(in,jn)=vsum/sum                                    
             else
             value(in,jn)=spcval
      endif
c     print*,'value',value(in,jn)
  300 continue
      return
      end
