      program date_to_day
!===  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: GFDC_DATE2DAY
!   PRGMMR: MARCHOK          ORG: NP22        DATE: 2001-03-29
!
! ABSTRACT: this program converts a date integer in YYMMDDHH format to
!   julian day in year YY
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

!     this program converts a date integer in YYMMDDHH format to 
!     julian day in year YY
!
!     dail - march 21, 2000

      implicit none

      integer*8 date
      integer dat2day(12),dat2dayl(12),day,month,year,hour,n
      real julday

! number of days in each month in regular and leap years
      data dat2day/31,28,31,30,31,30,31,31,30,31,30,31/
      data dat2dayl/31,29,31,30,31,30,31,31,30,31,30,31/

      call w3tagb('GFDC_DATE2DAY',2001,0088,0084,'NP22') 
      read(5,*) date

! get year and month
      year=int(date/1000000.)
      month=nint(100*(date/1000000.-int(date/1000000.)))

! calculate julian day in year.  account for leap years.
      julday=0
      if(mod(year,4).eq.0) then
         do n=1,month-1
            julday=julday+dat2dayl(n)
         end do
      else
         do n=1,month-1
            julday=julday+dat2day(n)
         end do
      end if

      julday=julday+nint(100*(date/10000.-int(date/10000.)))
      hour=date-nint(date/100.)*100
      julday=julday+float(hour)/24.

      write(61,*) julday, year

      call w3tage('GFDC_DATE2DAY')

      stop
      end
