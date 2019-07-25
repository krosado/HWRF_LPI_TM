      program day_to_date
!===  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: GFDC_DAY2DATE
!   PRGMMR: MARCHOK          ORG: NP22        DATE: 2001-03-29
!
! ABSTRACT: this program converts year and julian date input to date 
!   in YYMMDDHH format date integer
!
! PROGRAM HISTORY LOG:
!   98-06-02  frolov - original implementation
!   00-03-21  rowe   - reorganized
!
! INPUT FILES:
!   UNIT    5    standart input
!
! OUTPUT FILES:
!   UNIT    6    standart output
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!
!===

!     convert year and julian date input to date in YYMMDDHH format
!
!     dail - March 21, 2000

      implicit none

      integer*8 date
      integer dat2day(12),dat2dayl(12),day,month,year,year1,hour,n
      real julday,julday1

!     number of days in months of leap and regular years
      data dat2day/31,28,31,30,31,30,31,31,30,31,30,31/
      data dat2dayl/31,29,31,30,31,30,31,31,30,31,30,31/

      call w3tagb('GFDC_DAY2DATE',2001,0088,0086,'NP22')                  

      read(5,*) year,julday

      if(int(julday).gt.365+int(1./(mod(year,4)*100.+1.))) then
         julday1=julday-365-int(1./(mod(year,4)*100.+1.))
         year1=year+1
      else
         julday1=julday
         year1=year
      end if

      day=0
      n=1
      if(mod(year1,4).eq.0) then  
         do while(day+dat2dayl(n).lt.int(julday1))
            day=day+dat2dayl(n)
            n=n+1   
         end do
      else
         do while(day+dat2day(n).lt.int(julday1))
            day=day+dat2day(n)
            n=n+1   
         end do
      end if

      month=n
      day=int(julday1-day)
      hour=int((julday1-int(julday1))*24.)
      date=year1*1000000+month*10000+day*100+hour

      write(61,*) date
!
      call w3tage('GFDC_DAY2DATE')
!
      stop
      end
