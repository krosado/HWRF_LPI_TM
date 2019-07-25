      subroutine rdbges(iobges,nptsgs,sst,slmskf)
      character*32 washin
      real(4) fhour_4byte
      real    fhour
      real    xdat(10)
      real(4), allocatable :: sst_4byte(:),slmskf_4byte(:)
      dimension sst(nptsgs),slmskf(nptsgs)
      dimension idate(4)
c
c     read sea surface temperature field.
c
c     read washington record !!!!!!!!!!!!!!!!!!
      read(iobges) washin
      read(iobges) fhour_4byte,idate,it1,jt1,iver

      print *,'in rdbges, washin= ',washin
      print *,'in rdbges, fhour_4byte= ',fhour_4byte
      print *,'in rdbges, idate= ',idate
      print *,'in rdbges, it1= ',it1
      print *,'in rdbges, jt1= ',jt1
      print *,'in rdbges, iver= ',iver

      fhour = fhour_4byte
      write(6,1003)    fhour,idate
      nr=3

      allocate (sst_4byte(nptsgs),stat=istat)
      if (istat /= 0) then
        print *,'!!! ERROR ALLOCATING sst_4byte in rdbges....'
        return
      endif

      read(iobges) sst_4byte
      sst = sst_4byte
      deallocate (sst_4byte)

c     soil moisture(4th) & soil temperature(6th) record-twice as big
c     include sea-land-ice(0-1-2) mask from nmc  ....nr=13(was 14th)

      allocate (slmskf_4byte(nptsgs),stat=istat)
      if (istat /= 0) then
        print *,'!!! ERROR ALLOCATING sst_4byte in rdbges....'
        return
      endif

      do nr=4,13
        read(iobges) slmskf_4byte
      enddo

      slmskf = slmskf_4byte
      deallocate (slmskf_4byte)

   10 continue
c
 1003 format(10x,f10.3,4i10/(10x,10f10.7) )
      return
      end
