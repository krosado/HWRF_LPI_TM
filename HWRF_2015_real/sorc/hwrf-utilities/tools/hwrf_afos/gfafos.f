      PROGRAM gfafos                                                    
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK                                  
C                .      .    .                                       .  
C MAIN PROGRAM: GFDLAFOS     Produces AFOS Message from GFDL Model           
C   PRGMMR: LORD             ORG: NP22        DATE: 95-05-05  
C                                                                       
C ABSTRACT: Initiates GFDL Model by checking available storms, 
C           assigning priority and order in which storms should be
C           run.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   95-04-13  S. J. LORD (763-8005) 
c   00-07-20  Marchok - Updated the read of unit 12 so that it is 
c                  not fixed to read only 13 time levels.  This way,
c                  we can handle either 78h or 126h forecasts.
c   01-03-20  Marchok - File header now indicates if forecast was
c                  run coupled or uncoupled.
c   01-07-11  Marchok - Afos msg now uses the 0-hour info from the 
c                  forecast, as opposed to just copying it from 
c                  tc vitals.  This gives initial intensity of the 
c                  model storm. 
C                                                                      
C USAGE:                                                                
C   INPUT FILES:                                                        
C     unit 2   - Text file to indicate coupled or uncoupled forecast
C     unit 5   - standard input
C     fort.11  - storm message file (input)  
C     fort.12  - gfdl storm track file (input)  
C                                                                       
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)                            
C     fort.51  - afos message for a single storm  
C     fort.6   - standard output                                          
C                                                                       
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)        
C     UNIQUE: 
C
C     LIBRARY:                                                          
C       W3LIB    -                                   
C       COMMON   -                                                 
C                                                                       
C   EXIT STATES:                                                        
C     COND = 0   - SUCCESSFUL RUN.               
C          =20   - MAJOR ERROR IN CODE.  CALL S. LORD (763-8005 OR      
C                - 249-7713 (HOME))                                     
C                                                                       
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION             
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM/SP
C                                                                       
C$$$                                                                    

      PARAMETER (MAXSTM=1)
      PARAMETER (MAXFIL=99)                                             

      CHARACTER FILNAM*128
      character line*80,header*1,head1*72,head2*72,head3*72,
     1          head4*72,head5*72,head6*72,stmtyp*19,mon*3,monz*3,
     1          date*10,slsh*1,headcoup*72,headnocoup*72
                                                    
      DIMENSION FILNAM(0:MAXFIL),header(6,72),stmtyp(3),mon(12),
     1          iscolt(3),iecolt(3)

      
C          IUNTVI: UNIT NUMBER FOR storm message                
C          IUNTGF: UNIT NUMBER FOR gfdl track message                

      DATA IUNTOP/3/,IUNTVI/11/,IUNTGF/12/,iprnt/1/,iopt/0/
      data head1/'ATTENTION...NATIONAL HURRICANE CENTER                 
     1        '/,
     1     head2/'NCEP HURRICANE MODEL..HWRF ATM...FORECAST MADE FOR      
     1      '/,
     2     head3/'INITIAL TIME                                          
     1        '/,
     3     head4/'FORECAST STORM POSITION                               
     1        '/,
     4     head5/'HOUR        LATITUDE        LONGITUDE        HEADING/S
     5PEED(KT)'/,
     6     head6/'STORM DISSIPATED AT     HRS AT THE ABOVE PSN.         
     1        '/
      data headcoup/'NCEP COUPLED HWRF HURRICANE MODEL FORECAST MADE FOR
     &      '/
      data headnocoup/'NCEP UNCOUPLED HWRF HURRICANE MODEL FORECAST MADE 
     & FOR  '/
      data stmtyp/'TROPICAL DEPRESSION','TROPICAL STORM','HURRICANE'/
      data iscolt/1,1,1/,iecolt/19,14,9/
      data mon/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',
     1         'OCT','NOV','DEC'/
      data slsh/'/'/
      
      include 'vitfmtn.inc'                                              
      include 'stmvitn.inc'                                              
      include 'vitdatn.inc'

      CALL W3TAGB('HWRF_AFOS',0095,0125,0051,'NP22')
                                                    
      filnam(0)='fildef.afos'                                            
c     CALL OFILE0(IUNTOP,MAXFIL,NFTOT,FILNAM) 

      read(2,901) IOCEAN
901   format(i5)

                                
      write(6,1)
    1 format('  ',//,7x,'   HWRF AFOS MESSAGE GENERATOR')

      line=' '
      line=head1
      write(6,9)  line
      write(51,9)  line
    9 format(a)
      line=' '
      write(6,9)  line
      write(51,9)  line

      if (iocean == 1) then
        line=headcoup
      else
        line=headnocoup
      endif

      write(6,9)  line
      write(51,9)  line
   
      read(IUNTVI,11,err=900)  bufinz
   11 format(a)
c      write(6,9)  stmidz
c      write(6,9)  stmnmz

c     current position
      ivar=3
      call decvar(istvar(ivar),ienvar(ivar),ival,ierdec,fmtvit(ivar),
     1            bufinz)
      stmlat(1)=ival*vitfac(ivar)
      ivar=4
      call decvar(istvar(ivar),ienvar(ivar),ival,ierdec,fmtvit(ivar),
     1            bufinz)
      stmlon(1)=ival*vitfac(ivar)
      istfc=0

c     Pull the storm direction and heading off of the TC Vitals 
c     record and use it for reporting the heading/speed for the 
c     0-hour position.

      ivar=5     ! Get the storm heading first
      call decvar(istvar(ivar),ienvar(ivar),ival,ierdec,fmtvit(ivar),
     1            bufinz)
      stormdir_init = ival

      ivar=6     ! Get the storm speed next
      call decvar(istvar(ivar),ienvar(ivar),ival,ierdec,fmtvit(ivar),
     1            bufinz)
      stormspeed_init = ival
      
c     storm category
      
      ivar=10
      call decvar(istvar(ivar),ienvar(ivar),ival,ierdec,fmtvit(ivar),
     1            bufinz)
      vmax(1)=ival*vitfac(ivar)*1.94
c      write(6,29)  vmax(1)
c   29 format(' ...vmax=',f8.1)
      if(vmax(1) .le. 34.0)  then
        istt=1
      else if(vmax(1) .le. 66.0)  then
        istt=2
      else
        istt=3
      endif
      line=' '
      write(6,9)  line
      write(51,9)  line
      line(iscolt(istt):iecolt(istt))=stmtyp(istt)
      line(iecolt(istt)+2:iecolt(istt)+10)=stmnmz
      line(iecolt(istt)+12:iecolt(istt)+14)=stmidz
      write(6,9)  line
      write(51,9)  line

c     date/time

      do 20 iv=1,2
      call decvar(istvar(iv),ienvar(iv),ivtvar(iv),ierdec,
     1            fmtvit(iv),bufinz)
   20 continue
c      write(6,21)  idatez,iutcz
c   21 format(' ...idatez,iutcz=',i8,i5)
      call ztime(idatez,iutcz,iyr,imo,ida,ihr,imin)
      write(6,234)  imo
      write(6,234)  ida
      write(6,234)  ihr
  234 format(i5)
      date='          '
      write(date(1:2),23)  ihr
      write(date(9:10),23)  ida
   23 format(i2)
      monz=mon(imo)
      date(3:3)='Z'
      date(5:7)=monz
      write(6,25)  date
   25 format('  ...date=',a)
      line=' '
      write(6,9)  line
      write(51,9)  line
      line=' '
      line=head3
      line(15:24)=date
      write(6,9)  line
      write(51,9)  line
      line=' '
      write(6,9)  line
      write(51,9)  line
      line=head4
      write(6,9)  line
      write(51,9)  line
      line=' '
      write(6,9)  line
      write(51,9)  line
      
c     column header
      line=' '
      line=head5
      write(6,9)  line
      write(51,9)  line
      line=' '
      write(6,9)  line
      write(51,9)  line

c     now the forecast positions

c      write(6,4050) istfc,stmlat(1),stmlon(1)
c      write(51,4050) istfc,stmlat(1),stmlon(1)

      oldlat=stmlat(1)
      oldlon=-stmlon(1)
      oldhr=0.0

C     This read loop reads the stats file.  If the forecast ran to
c     completion (either 78h or 126h), then the stats file just has
c     those positions in there, with one line for each new time.
c     If instead the storm dissipated before completion, there will
c     be a line of -99's in the stats file, which is why in the 
c     readloop below there is a check to see if the hour is less
c     than 0.

      readloop: do while (.true.)

        read(12,4049,end=105) hour,poslon,poslat                         
4049    format(5x,F5.1,7X,F8.2,6X,F7.2)                

        if (poslon > 0.0) poslon = -1 * (360.0 - poslon) 

        if (hour < 0.0) then
          ilast = nint(oldhr)
          oldhr = hour
          exit readloop
        endif
c
        if (hour == 0.0) then
          ! Just use the storm motion speed and direction right 
          ! off of the TC vitals card for hour 0.
          direct = stormdir_init
          speed  = (stormspeed_init/10.0) * 1.94
        else
          dist=distsp(poslat,poslon,oldlat,oldlon)
          dt=(hour-oldhr)*3600.0
          speed=1.94*dist/dt
          dlon=poslon-oldlon
          dlat=poslat-oldlat

c          write(6,1293)  poslat,oldlat,poslon,oldlon,dt,speed,dlon,dlat,
c     1                   dist
c 1293     format(' ...poslat,oldlat,poslon,oldlon,dt,speed,dlon,dlat',/,
c     1           6x,8f13.4)      

          direct=atan2d(dlon,dlat)
          if(direct .lt. 0.0)  direct=360.+direct
        endif

        write(6,4050) ifix(hour),poslat,-poslon,direct,slsh,speed
        write(51,4050) ifix(hour),poslat,-poslon,direct,slsh,speed
4050    format(1x,i3,9X,F7.1,10X,F7.1,10x,f5.0,a,f4.1)

        oldlat=poslat
        oldlon=poslon
        oldhr=hour

      enddo readloop

105   continue

c     final header

      ihour=nint(oldhr)
C      if(ihour .ne. 78)  then
      if(ihour < 0)  then
        line=' '
        write(6,9)  line
        write(51,9) line
        line=' '
        line=head6
        write(line(21:23),4060) ilast
4060    format(i3)
        write(6,9)  line
        write(51,9)  line
      endif
      icond=0
      go to 1000
   
  900 write(6,19) 
   19 format('   ******THERE ARE NO STORMS. ABORT')
      icond=20
      go to 1000


 1000 continue
      CALL W3TAGE('HWRF_AFOS')                                           
      stop
      end
