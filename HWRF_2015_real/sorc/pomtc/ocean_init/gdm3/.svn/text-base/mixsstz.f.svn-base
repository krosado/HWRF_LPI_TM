      subroutine mixsstz(f,stm,h,fsm,zlev,im,jm,nl,tbias)
      parameter(kk=29,jj=31,step=5.)
c---------------------------------------------------------------------
c----- this subr. was rewritten 06/17/05 by a.falkovich
c----- to assimilate sst real data in gdem monthly t data for z-levels
c---------------------------------------------------------------------
cRMY modified 03/11/13 by r.yablonsky for gdem3: kk=29 instead of 9 !!
c
c---- change temperature in the layer 0-125m.
c---- use spline to interpolate from gdem z-levels to even step 10m
c---- here temperature is specified at all levels 
c----                                 (under land and under bottom)
c---- mixed layer depth is the last level n where t(1)-t(k)<0.5deg
c---- dts=stm-t(1);  tnew(k)=t(k)+dts for k=1,...,n
c---- if abs(dts)>5deg correct stm
c---- if stm<t(150m) correct stm
c
      real stm(im,jm),h(im,jm),fsm(im,jm)
      real f(im,jm,nl),zlev(nl)
      real dts,t(jj),z(jj),tt(kk),zz(kk),tc(jj),te(kk),dt(kk)
      logical plg1,plg2,plg3
c
c--------- stm is used after correction for tbias,
c--------- f temperature before correction for tbias
c
c
c---------------
      print *,' mixsstz: im,jm,nl=',im,jm,nl
      print *,' tbias=',tbias
      print *,'kk,jj=',kk,jj
      print *,'  zlev, k=1,nl'
c---------------
      write(6,202) (zlev(k),k=1,nl)
 202  format(10f7.0)
c--------------- for debug
       i0=124
       j0=92
c--------------- for debug
       i1=124
       j1=88
       j2=98
c---------------
       print *,' before sst assim: i1,j1,j2=',i1,j1,j2
       print *,'     f(i1,j,k), j=j1,j2   nl=',nl
       write(6,301) (j,j=j1,j2)
       do k=1,kk
        write(6,302) zlev(k),(f(i1,j,k),j=j1,j2)
       end do
 301   format(5x,11i7)
 302   format(f8.0,11f7.2)
c---------------
c-----  remove correction for tbias
      do j=1,jm
      do i=1,im
         if(fsm(i,j).eq.1.) stm(i,j)=stm(i,j)+tbias
      end do
      end do
c----------------
      write(6,401) i0,j0,stm(i0,j0),h(i0,j0)
 401  format('i0,j0,stm(i0,j0),h(i0,j0)=',
     *            /2i7,f7.2,f7.0)
c---------------
      print *,'     stm(i1,j), j=j1,j2 '
      write(6,303) (stm(i1,j),j=j1,j2)
 303  format(8x,11f7.2)
c
c------------------ check difference between stm and f(i,j,1)
c
      difmax=0.
      difmin=0.
      do i=1,im
      do j=1,jm
       if(fsm(i,j).gt.0.5) then
        difp=stm(i,j)-f(i,j,1)
        difm=-difp
        if(difp.gt.difmax) then
         imax=i
         jmax=j
         difmax=difp
        end if
        if(difm.gt.difmin) then
         imin=i
         jmin=j
         difmin=difm
        end if
       end if
      end do
      end do
c------------------
      print *,' mixsstz: max(stm-f(1)),i,j'
      print *,' difmax,idifmax,jdifmax=',difmax,imax,jmax
      print *,' mixsstz: max(f(1)-stm),i,j'
      print *,' difmin,imin,jmin=',difmin,imin,jmin
c------------------ specify z and zz
      do j=1,jj
       z(j)=step*float(j-1)
      end do
      do k=1,kk
       zz(k)=zlev(k)
      end do
c------------------
c-----------------------------   loops in i,j
      do j=1,jm
      do i=1,im
c--------------- for debug
c      plg1=i.eq.i0.and.j.eq.j0
       plg1=i.eq.i1.and.j.ge.j1.and.j.le.j2
       plg2=i.eq.imax.and.j.eq.jmax
       plg3=i.eq.imin.and.j.eq.jmin
c---------------
       if(fsm(i,j).eq.0.) go to 1000
c------------         send tt
       do k=1,kk
        tt(k)=f(i,j,k)
       end do
c------------         send sst
       sst=stm(i,j)
c------------         sp interpolate  to even step z
       call sp(zz,tt,t,z,kk,jj,kk,jj)
c------------
c      if(i.eq.i0.and.j.eq.j0) then
       if(plg1.or.plg2.or.plg3) then
c------------
        print *,'i,j,sst=',i,j,sst
        print *,'initial depth zz'
        write(6,101) zz
 101    format(10f7.0)
c------------
        print *,'initial temperature tt'
        write(6,102) tt
 102    format(10f7.2)
c------------
        print *,' depth for interpolation'
        write(6,101) z
c------------
        print *,'temperature after spline interpolation'
        write(6,102) t
c------------
       end if
c
c------------- find n for the depth of mixed layer
       n=1
       do while ((t(1)-t(n+1)).le.0.5.and.n.lt.(jj))
        n=n+1
       end do
c------------- if mixed layer more than 80m send 80m (n=17)
       if(n.gt.17) n=17
c---------------------- correct sst if it deviates too much from t(1)
       if(sst-t(1).gt.5.)  sst=t(1)+5.
       if(sst-t(1).lt.-5.) sst=t(1)-5.
c---------------------- correct sst if sst<t(jj)
       if(sst.lt.t(jj)) sst=t(jj)
c----------------------
       dts=sst-t(1)
c----------------------
       do k=1,n
          tc(k)=t(k)+dts
       end do
c----------------------
       m=jj-n
c----------------------
       do k=n+1,jj
        tc(k)=t(k)+dts*float(jj-k)/float(m)
       end do
c----------------------
c      if(i.eq.i0.and.j.eq.j0) then
c      if(plg2.or.plg3) then
       if(plg1.or.plg2.or.plg3) then
         print *,'i,j,sst=',i,j,sst
         print *,'mix: n,m=',n,m
         write(6,201) sst,t(1),dts
 201     format('sst,t(1),dts=',3f7.2)
c------------
         print *,' depth for interpolation'
         write(6,101) z
c------------
         print *,'temperature in mix before checking'
         write(6,102) tc
       end if
c------------- check if(tc(k).lt.t(jj))
       do k=2,jj-1
        if(tc(k).le.t(jj)) then
          n=k-1
          m=jj-n
          dts=t(jj)-tc(n)
c         write(6,104) n,m,dts
 104      format('n,m,dts=',2i7,f7.2)
          go to 100
        end if
       end do
       go to 200
 100   continue
       do k=n+1,jj
         tc(k)=t(jj)-dts*float(jj-k)/float(m)
       end do
c------------
 200   continue
c------------- check stability
       do k=2,jj
         if(tc(k-1).lt.tc(k)) tc(k)=tc(k-1)
       end do
c------------ interpolate back to gdem z-levels
       call sp(z,tc,te,zz,jj,kk,jj,kk)
c------------
       do k=1,kk
         dt(k)=te(k)-tt(k)
       end do
c------------
c      if(i.eq.i0.and.j.eq.j0) then
c      if(plg2.or.plg3) then
       if(plg1.or.plg2.or.plg3) then
         print *,'i,j,sst=',i,j,sst
         print *,'temperature in mix after checking'
         write(6,102) tc
c------------
         print *,'initial depth zz'
         write(6,101) zz
c------------
         print *,'initial temperature tt'
         write(6,102) tt
c------------
         print *,'temperature after back interpolation'
         write(6,102) te
c------------
         print *,'temperature differences after mix'
         write(6,102) dt
       end if
c---------------------- send back adjusted temperature
       do k=1,kk
         f(i,j,k)=te(k)
       end do
c------------
 1000  continue
c--------------------- end loops in i,j
      end do
      end do
c----------------------
       print *,' after sst assim: i1,j1,j2=',i1,j1,j2
       print *,'     f(i1,j,k), j=j1,j2   nl=',nl
       write(6,301) (j,j=j1,j2)
       do k=1,kk
        write(6,302) zlev(k),(f(i1,j,k),j=j1,j2)
       end do
c----------------------
      return
      end
