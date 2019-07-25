      program readsst
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: GFDC_GETSST
C   PRGMMR: MARCHOK          ORG: NP22        DATE: 2001-03-30
C
C ABSTRACT: This program extracts the sst and land-sea mask data from
C the AVN sfcanl and siganl files, and then writes that data back out 
C into a regular flat file, to be used in a later executable.
C
C PROGRAM HISTORY LOG:
C   01-03-20  Biju Thomas original writing
C
C INPUT FILES:
C   UNIT   11    AVN sfcanl file
C   UNIT   12    AVN siganl file
C
C OUTPUT FILES:
C   UNIT   74    Output file containing AVN's sst data
C   UNIT   77    Output file containing AVN's land-sea mask  data
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
c
      use sfcio_module
      type(sfcio_head):: head
      type(sfcio_data):: data

      integer idate(4), ixdate(4)
      integer, parameter :: iosfc=11,iosig=12
      real ext(245)
      real(4) fhour_4byte,ext_4byte(245)
      real xhour
      data idrt/4/

c
      call w3tagb('GFDC_GETSST',2001,0089,0074,'NP22')

c      read(11)
c      read(11) xhour, ixdate, im, jm, iver
c      print *,'From gfdlgg sfcanl read, xhour= ',xhour
c      print *,'From gfdlgg sfcanl read, ixdate= ',ixdate
c      print *,'From gfdlgg sfcanl read, im= ',im,' jm= ',jm
c      print *,'From gfdlgg sfcanl read, iver= ',iver
c      rewind(11)

      call sfcio_srohdc (iosfc,'for11',head,data,iret)

      if (iret /= 0) then
        print *,' ' 
        print *,'ERROR in getsst reading sfcanl header, ' 
        print *,' error code = ',iret 
        print *,' ' 
        stop
      endif

      print *,'From gfdlgg sfcanl read, xhour= ',head%fhour
      print *,'From gfdlgg sfcanl read, ixdate= ',head%idate
      print *,'From gfdlgg sfcanl read, im= ',head%lonb,' jm= '
     &       ,head%latb
      print *,'From gfdlgg sfcanl read, iver= ',head%ivs

      im = head%lonb
      jm = head%latb
      idate = head%idate

      read(iosig)
      read(iosig) fhour_4byte,idate,ext_4byte
      fhour = fhour_4byte
      ext   = ext_4byte
      jcap=ext(202)
      nc=(jcap+1)*(jcap+2)
      print *, 'idate,idrt,im,jm',  idate,idrt,im,jm
      print *, 'jcap,nc', jcap,nc
      print 10,  fhour,idate,im,jm,ext
   10 format(1x,'from spec file fhour,idate,si,sl ',
     a       F10.3,6I10/(10X,10F11.7) )

      call getsm(idrt,im,jm,jcap,nc)
      
      call w3tage('GFDC_GETSST')
c
      jm1=jm/2
      call avnglatlon(im,jm,jm1)
      stop 
      end
