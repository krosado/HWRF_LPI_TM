      PROGRAM NHOUR
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: NHOUR        COMPUTE FORECAST HOUR
C   PRGMMR: IREDELL          ORG: NP23        DATE: 1998-08-18
C
C ABSTRACT: PROGRAM TO COMPUTE FORECAST HOUR
C   GIVEN THE VERIFYING DATE AND THE INITIAL DATE.
C
C PROGRAM HISTORY LOG:
C   95-02-28  IREDELL
C   97-09-22  IREDELL  4-DIGIT YEAR ALLOWED; 2-DIGIT YEAR STANDARDIZED
C   98-03-25  IREDELL  4-DIGIT YEAR FOR ALL DATES.  A 2-DIGIT YEAR WILL
C                      BE INTERPRETED AS A YEAR IN THE FIRST CENTURY
C                      WHICH SHOULD BE ALL RIGHT BEFORE THE YEAR 2000.
C                      STANDARD ERROR WARNINGS WILL BE GIVEN FOR SUCH
C                      DATES UNTIL 1 SEPT 1998 AFTER WHICH NHOUR ABORTS.
C                      THE NEW Y2K-COMPLIANT W3LIB PACKAGE IS USED.
C 1998-08-17  IREDELL  DROP-DEAD DATE RESET TO 1 SEPT 1999
C 1999-04-22  Gilbert  Changed subroutine EXIT(N) to ERREXIT(N) so that
C                      error return values are passed back to the shell
C                      properly.
C 1999-09-02  IREDELL  STANDARDIZED 4-DIGIT YEAR AS IN NDATE
C
C USAGE:      nhour vdate [idate]
C   INPUT ARGUMENT LIST:
C     VDATE    - VERIFYING DATE IN YYYYMMDDHH FORMAT.
C     IDATE    - INITIAL DATE IN YYYYMMDDHH FORMAT.
C                IDATE DEFAULTS TO THE UTC DATE AND HOUR.
C   OUTPUT ARGUMENT LIST:
C     NHOUR    - FORECAST HOUR
C                LEADING ZEROES ADDED TO MAKE IT AT LEAST TWO DIGITS.
C                LEADING MINUS SIGN ADDED IF IDATE COMES AFTER VDATE.
C   EXIT STATES:
C     0      - SUCCESS
C     1      - FAILURE; INVALID ARGUMENT
C     2      - FAILURE; INCORRECT NUMBER OF ARGUMENTS
C
C SUBPROGRAMS CALLED:
C   IARGC           GET NUMBER OF ARGUMENTS
C   GETARG          GET ARGUMENT
C   W3DIFDAT        RETURN A TIME INTERVAL BETWEEN TWO DATES
C   W3PRADAT        FORMAT A DATE AND TIME INTO CHARACTERS
C   W3UTCDAT        RETURN THE UTC DATE AND TIME
C   ERRMSG          WRITE A MESSAGE TO STDERR
C   ERREXIT         EXIT PROGRAM
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      CHARACTER*256 CARG,CFMT
      INTEGER IDAT(8),JDAT(8)
      REAL RINC(5)
      LOGICAL W3VALDAT
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CHECK NUMBER OF ARGUMENTS
      NARG=IARGC()
      IF(NARG.LT.1.OR.NARG.GT.2) THEN
        CALL ERRMSG('nhour: Incorrect number of arguments')
        CALL EUSAGE
        CALL ERREXIT(2)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET AND CHECK FIRST ARGUMENT (VERIFYING DATE)
      CALL GETARG(1,CARG)
      NCARG=LEN_TRIM(CARG)
      WRITE(CFMT,'("(I",I2,",3I2)")') NCARG-6
      JDAT=0
      READ(CARG,CFMT,IOSTAT=IRET) JDAT(1),JDAT(2),JDAT(3),JDAT(5)
      IF(IRET.NE.0.OR..NOT.W3VALDAT(JDAT)) THEN
        CALL ERRMSG('nhour: Invalid date '//CARG(1:NCARG))
        CALL EUSAGE
        CALL ERREXIT(1)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET AND CHECK SECOND ARGUMENT (INITIAL DATE)
      IF(NARG.GE.2) THEN
        CALL GETARG(2,CARG)
        NCARG=LEN_TRIM(CARG)
        WRITE(CFMT,'("(I",I2,",3I2)")') NCARG-6
        IDAT=0
        READ(CARG,CFMT,IOSTAT=IRET) IDAT(1),IDAT(2),IDAT(3),IDAT(5)
        IF(IRET.NE.0.OR..NOT.W3VALDAT(IDAT)) THEN
          CALL ERRMSG('nhour: Invalid date '//CARG(1:NCARG))
          CALL EUSAGE
          CALL ERREXIT(1)
        ENDIF
      ELSE
        CALL W3UTCDAT(IDAT)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE AND PRINT HOUR DIFFERENCE
      CALL W3DIFDAT(JDAT,IDAT,2,RINC)
      IHOUR=NINT(RINC(2))
      NDIG=LOG10(ABS(IHOUR)+0.5)+1
      NDIG=MAX(NDIG,2)
      IF(IHOUR.LT.0) NDIG=NDIG+1
      WRITE(CFMT,'("(I",I2,".2)")') NDIG
      PRINT CFMT,IHOUR
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CONTAINS
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  WRITE USAGE
      SUBROUTINE EUSAGE
      CALL ERRMSG('Usage: nhour vdate [idate]')
      ENDSUBROUTINE
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ENDPROGRAM
!-----------------------------------------------------------------------
      subroutine w3difdat(jdat,idat,it,rinc)
!$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
!
! SUBPROGRAM: W3DIFDAT       RETURN A TIME INTERVAL BETWEEN TWO DATES
!   AUTHOR: MARK IREDELL     ORG: WP23       DATE: 98-01-05
!
! ABSTRACT: THIS SUBPROGRAM RETURNS THE ELAPSED TIME INTERVAL FROM
!   AN NCEP ABSOLUTE DATE AND TIME GIVEN IN THE SECOND ARGUMENT UNTIL
!   AN NCEP ABSOLUTE DATE AND TIME GIVEN IN THE FIRST ARGUMENT.
!   THE OUTPUT TIME INTERVAL IS IN ONE OF SEVEN CANONICAL FORMS
!   OF THE NCEP RELATIVE TIME INTERVAL DATA STRUCTURE.
!
! PROGRAM HISTORY LOG:
!   98-01-05  MARK IREDELL
!
! USAGE:  CALL W3DIFDAT(JDAT,IDAT,IT,RINC)
!
!   INPUT VARIABLES:
!     JDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
!     IDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
!     IT         INTEGER RELATIVE TIME INTERVAL FORMAT TYPE
!                (-1 FOR FIRST REDUCED TYPE (HOURS ALWAYS POSITIVE),
!                 0 FOR SECOND REDUCED TYPE (HOURS CAN BE NEGATIVE),
!                 1 FOR DAYS ONLY, 2 FOR HOURS ONLY, 3 FOR MINUTES ONLY,
!                 4 FOR SECONDS ONLY, 5 FOR MILLISECONDS ONLY)
!
!   OUTPUT VARIABLES:
!     RINC       REAL (5) NCEP RELATIVE TIME INTERVAL
!                (DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS)
!                (TIME INTERVAL IS POSITIVE IF JDAT IS LATER THAN IDAT.)
!
! SUBPROGRAMS CALLED:
!     IW3JDN         COMPUTE JULIAN DAY NUMBER     
!     W3REDDAT       REDUCE A TIME INTERVAL TO A CANONICAL FORM
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      integer jdat(8),idat(8)
      real rinc(5)
      real rinc1(5)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  difference the days and time and put into canonical form
      rinc1(1)=iw3jdn(jdat(1),jdat(2),jdat(3))-
     &         iw3jdn(idat(1),idat(2),idat(3))
      rinc1(2:5)=jdat(5:8)-idat(5:8)
      call w3reddat(it,rinc1,rinc)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
       FUNCTION IW3JDN(IYEAR,MONTH,IDAY)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: IW3JDN         COMPUTE JULIAN DAY NUMBER
C   AUTHOR: JONES,R.E.       ORG: W342       DATE: 87-03-29
C
C ABSTRACT: COMPUTES JULIAN DAY NUMBER FROM YEAR (4 DIGITS), MONTH,
C   AND DAY. IW3JDN IS VALID FOR YEARS 1583 A.D. TO 3300 A.D.
C   JULIAN DAY NUMBER CAN BE USED TO COMPUTE DAY OF WEEK, DAY OF
C   YEAR, RECORD NUMBERS IN AN ARCHIVE, REPLACE DAY OF CENTURY,
C   FIND THE NUMBER OF DAYS BETWEEN TWO DATES.
C
C PROGRAM HISTORY LOG:
C   87-03-29  R.E.JONES
C   89-10-25  R.E.JONES   CONVERT TO CRAY CFT77 FORTRAN
C
C USAGE:   II = IW3JDN(IYEAR,MONTH,IDAY)
C
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     IYEAR  ARG LIST  INTEGER   YEAR           ( 4 DIGITS)
C     MONTH  ARG LIST  INTEGER   MONTH OF YEAR   (1 - 12)
C     IDAY   ARG LIST  INTEGER   DAY OF MONTH    (1 - 31)
C
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     IW3JDN FUNTION   INTEGER   JULIAN DAY NUMBER
C                      JAN. 1,1960 IS JULIAN DAY NUMBER 2436935
C                      JAN. 1,1987 IS JULIAN DAY NUMBER 2446797
C
C   REMARKS: JULIAN PERIOD WAS DEVISED BY JOSEPH SCALIGER IN 1582.
C     JULIAN DAY NUMBER #1 STARTED ON JAN. 1,4713 B.C. THREE MAJOR
C     CHRONOLOGICAL CYCLES BEGIN ON THE SAME DAY. A 28-YEAR SOLAR
C     CYCLE, A 19-YEAR LUNER CYCLE, A 15-YEAR INDICTION CYCLE, USED
C     IN ANCIENT ROME TO REGULATE TAXES. IT WILL TAKE 7980 YEARS
C     TO COMPLETE THE PERIOD, THE PRODUCT OF 28, 19, AND 15.
C     SCALIGER NAMED THE PERIOD, DATE, AND NUMBER AFTER HIS FATHER
C     JULIUS (NOT AFTER THE JULIAN CALENDAR). THIS SEEMS TO HAVE
C     CAUSED A LOT OF CONFUSION IN TEXT BOOKS. SCALIGER NAME IS
C     SPELLED THREE DIFFERENT WAYS. JULIAN DATE AND JULIAN DAY
C     NUMBER ARE INTERCHANGED. A JULIAN DATE IS USED BY ASTRONOMERS
C     TO COMPUTE ACCURATE TIME, IT HAS A FRACTION. WHEN TRUNCATED TO
C     AN INTEGER IT IS CALLED AN JULIAN DAY NUMBER. THIS FUNCTION
C     WAS IN A LETTER TO THE EDITOR OF THE COMMUNICATIONS OF THE ACM
C     VOLUME 11 / NUMBER 10 / OCTOBER 1968. THE JULIAN DAY NUMBER
C     CAN BE CONVERTED TO A YEAR, MONTH, DAY, DAY OF WEEK, DAY OF
C     YEAR BY CALLING SUBROUTINE W3FS26.
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY Y-MP8/864, CRAY Y-MP EL2/256
C
C$$$
C
       IW3JDN  =    IDAY - 32075
     &            + 1461 * (IYEAR + 4800 + (MONTH - 14) / 12) / 4
     &            + 367 * (MONTH - 2 - (MONTH -14) / 12 * 12) / 12
     &            - 3 * ((IYEAR + 4900 + (MONTH - 14) / 12) / 100) / 4
       RETURN
       END
      subroutine w3reddat(it,rinc,dinc)
!$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
!
! SUBPROGRAM: W3REDDAT       REDUCE A TIME INTERVAL TO A CANONICAL FORM
!   AUTHOR: MARK IREDELL     ORG: WP23       DATE: 98-01-05
!
! ABSTRACT: THIS SUBPROGRAM REDUCES AN NCEP RELATIVE TIME INTERVAL
!   INTO ONE OF SEVEN CANONICAL FORMS, DEPENDING ON THE INPUT IT VALUE.
!
!   First reduced format type (IT=-1):
!        RINC(1) is an arbitrary integer.
!        RINC(2) is an integer between 00 and 23, inclusive.
!        RINC(3) is an integer between 00 and 59, inclusive.
!        RINC(4) is an integer between 00 and 59, inclusive.
!        RINC(5) is an integer between 000 and 999, inclusive.
!      If RINC(1) is negative, then the time interval is negative.
!    
!   Second reduced format type (IT=0):
!      If the time interval is not negative, then the format is:
!        RINC(1) is zero or a positive integer. 
!        RINC(2) is an integer between 00 and 23, inclusive.
!        RINC(3) is an integer between 00 and 59, inclusive.
!        RINC(4) is an integer between 00 and 59, inclusive.
!        RINC(5) is an integer between 000 and 999, inclusive.
!      Otherwise if the time interval is negative, then the format is:
!        RINC(1) is zero or a negative integer. 
!        RINC(2) is an integer between 00 and -23, inclusive.
!        RINC(3) is an integer between 00 and -59, inclusive.
!        RINC(4) is an integer between 00 and -59, inclusive.
!        RINC(5) is an integer between 000 and -999, inclusive.
!    
!   Days format type (IT=1):
!        RINC(1) is arbitrary.
!        RINC(2) is zero.
!        RINC(3) is zero.
!        RINC(4) is zero.
!        RINC(5) is zero.
!    
!   Hours format type (IT=2):
!        RINC(1) is zero.
!        RINC(2) is arbitrary.
!        RINC(3) is zero.
!        RINC(4) is zero.
!        RINC(5) is zero.
!      (This format should not express time intervals longer than 300 years.)
!    
!   Minutes format type (IT=3):
!        RINC(1) is zero.
!        RINC(2) is zero.
!        RINC(3) is arbitrary.
!        RINC(4) is zero.
!        RINC(5) is zero.
!      (This format should not express time intervals longer than five years.)
!    
!   Seconds format type (IT=4):
!        RINC(1) is zero.
!        RINC(2) is zero.
!        RINC(3) is zero.
!        RINC(4) is arbitrary.
!        RINC(5) is zero.
!      (This format should not express time intervals longer than one month.)
!    
!   Milliseconds format type (IT=5):
!        RINC(1) is zero.
!        RINC(2) is zero.
!        RINC(3) is zero.
!        RINC(4) is zero.
!        RINC(5) is arbitrary.
!     (This format should not express time intervals longer than one hour.)
!
! PROGRAM HISTORY LOG:
!   98-01-05  MARK IREDELL
!
! USAGE:  CALL W3REDDAT(IT,RINC,DINC)
!
!   INPUT VARIABLES:
!     IT         INTEGER RELATIVE TIME INTERVAL FORMAT TYPE
!                (-1 FOR FIRST REDUCED TYPE (HOURS ALWAYS POSITIVE),
!                 0 FOR SECOND REDUCED TYPE (HOURS CAN BE NEGATIVE),
!                 1 FOR DAYS ONLY, 2 FOR HOURS ONLY, 3 FOR MINUTES ONLY,
!                 4 FOR SECONDS ONLY, 5 FOR MILLISECONDS ONLY)
!     RINC       REAL (5) NCEP RELATIVE TIME INTERVAL
!                (DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS)
!
!   OUTPUT VARIABLES:
!     DINC       REAL (5) NCEP RELATIVE TIME INTERVAL
!                (DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS)
!
! SUBPROGRAMS CALLED:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      real rinc(5),dinc(5)
!  parameters for number of units in a day
!  and number of milliseconds in a unit
!  and number of next smaller units in a unit, respectively
      integer,dimension(5),parameter:: itd=(/1,24,1440,86400,86400000/),
     &                                 itm=itd(5)/itd
c      integer,dimension(4),parameter:: itn=itd(2:5)/itd(1:4)
      integer itn(4)
      integer,parameter:: np=16
      integer iinc(4),jinc(5),kinc(5)

      itn(1) = itd(2)/itd(1)
      itn(2) = itd(3)/itd(2)
      itn(3) = itd(4)/itd(3)
      itn(4) = itd(5)/itd(4)

c      print *,'itn(1)= ',itn(1)
c      print *,'itn(2)= ',itn(2)
c      print *,'itn(3)= ',itn(3)
c      print *,'itn(4)= ',itn(4)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  first reduce to the first reduced form
      iinc=floor(rinc(1:4))
!  convert all positive fractional parts to milliseconds
!  and determine canonical milliseconds
      jinc(5)=nint(dot_product(rinc(1:4)-iinc,real(itm(1:4)))+rinc(5))
      kinc(5)=modulo(jinc(5),itn(4))
!  convert remainder to seconds and determine canonical seconds
      jinc(4)=iinc(4)+(jinc(5)-kinc(5))/itn(4)
      kinc(4)=modulo(jinc(4),itn(3))
!  convert remainder to minutes and determine canonical minutes
      jinc(3)=iinc(3)+(jinc(4)-kinc(4))/itn(3)
      kinc(3)=modulo(jinc(3),itn(2))
!  convert remainder to hours and determine canonical hours
      jinc(2)=iinc(2)+(jinc(3)-kinc(3))/itn(2)
      kinc(2)=modulo(jinc(2),itn(1))
!  convert remainder to days and compute milliseconds of the day
      kinc(1)=iinc(1)+(jinc(2)-kinc(2))/itn(1)
      ms=dot_product(kinc(2:5),itm(2:5))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  next reduce to either single value canonical form
!  or to one of the two reduced forms
      if(it.ge.1.and.it.le.5) then
!  ensure that exact multiples of 1./np are expressed exactly
!  (other fractions may have precision errors)
        rp=(np*ms)/itm(it)+mod(np*ms,itm(it))/real(itm(it))
        dinc=0
        dinc(it)=real(kinc(1))*itd(it)+rp/np
      else
!  the reduced form is done except the second reduced form is modified
!  for negative time intervals with fractional days
        dinc=kinc
        if(it.eq.0.and.kinc(1).lt.0.and.ms.gt.0) then
          dinc(1)=dinc(1)+1
          dinc(2:5)=mod(ms-itm(1),itm(1:4))/itm(2:5)
        endif
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
!-----------------------------------------------------------------------
      logical function w3valdat(idat)
!$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
!
! SUBPROGRAM: W3VALDAT       DETERMINE THE VALIDITY OF A DATE AND TIME
!   AUTHOR: MARK IREDELL     ORG: WP23       DATE: 98-01-05
!
! ABSTRACT: THIS LOGICAL FUNCTION RETURNS TRUE IF THE INPUT IS A VALID
!   NCEP ABSOLUTE DATE AND TIME.
!
! PROGRAM HISTORY LOG:
!   98-01-05  MARK IREDELL
!
! USAGE:  ...=W3VALDAT(IDAT)
!
!   INPUT VARIABLES:
!     IDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
!
!   OUTPUT VARIABLES:
!     W3VALDAT   LOGICAL TRUE IF IDAT IS A VALID NCEP DATE AND TIME
!
! SUBPROGRAMS CALLED:
!     IW3JDN         COMPUTE JULIAN DAY NUMBER     
!     W3FS26         YEAR, MONTH, DAY FROM JULIAN DAY NUMBER
!     W3REDDAT       REDUCE A TIME INTERVAL TO A CANONICAL FORM
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      integer idat(8)
      real rinc1(5),rinc2(5)
      integer jdat(8)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  essentially move the date and time by a zero time interval
!  and see if the same date and time is returned
      rinc1(1)=0
      rinc1(2:5)=idat(5:8)
      call w3reddat(-1,rinc1,rinc2)
      jldayn=iw3jdn(idat(1),idat(2),idat(3))+nint(rinc2(1))
      call w3fs26(jldayn,jdat(1),jdat(2),jdat(3),jdow,jdoy)
!  the time zone is valid if it is in signed hhmm format
!  with hh between -23 and 23 and mm equal to 00 or 30
      jdat(4)=mod(idat(4)/100,24)*100+mod(mod(idat(4),100),60)/30*30
      jdat(5:8)=nint(rinc2(2:5))
      w3valdat=all(idat.eq.jdat)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
!-----------------------------------------------------------------------
      subroutine w3utcdat(idat)
!$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
!
! SUBPROGRAM: W3UTCDAT       RETURN THE UTC DATE AND TIME
!   AUTHOR: MARK IREDELL     ORG: WP23       DATE: 98-01-05
!
! ABSTRACT: THIS SUBPROGRAM RETURNS THE UTC (GREENWICH) DATE AND TIME
!   IN THE NCEP ABSOLUTE DATE AND TIME DATA STRUCTURE.
!
! PROGRAM HISTORY LOG:
!   98-01-05  MARK IREDELL
! 1999-04-28  Gilbert         - added a patch to check for the proper
!                               UTC offset.  Needed until the IBM bug
!                               in date_and_time is fixed.  The patch
!                               can then be removed.  See comments in
!                               the section blocked with "&&&&&&&&&&&".
! 1999-08-12  Gilbert         - Changed so that czone variable is saved
!                               and the system call is only done for
!                               first invocation of this routine.
!
! USAGE:  CALL W3UTCDAT(IDAT)
!
!   OUTPUT VARIABLES:
!     IDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
!
! SUBPROGRAMS CALLED:
!     DATE_AND_TIME  FORTRAN 90 SYSTEM DATE INTRINSIC
!     IW3JDN         COMPUTE JULIAN DAY NUMBER     
!     W3FS26         YEAR, MONTH, DAY FROM JULIAN DAY NUMBER
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      integer idat(8)
      character cdate*8,ctime*10,czone*5
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  get local date and time but use the character time zone
      call date_and_time(cdate,ctime,czone,idat)
      read(czone,'(i5)') idat(4)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  convert to hours and minutes to UTC time
!  and possibly adjust the date as well
      idat(6)=idat(6)-mod(idat(4),100)
      idat(5)=idat(5)-idat(4)/100
      idat(4)=0
      if(idat(6).lt.00) then
        idat(6)=idat(6)+60
        idat(5)=idat(5)-1
      elseif(idat(6).ge.60) then
        idat(6)=idat(6)-60
        idat(5)=idat(5)+1
      endif
      if(idat(5).lt.00) then
        idat(5)=idat(5)+24
        jldayn=iw3jdn(idat(1),idat(2),idat(3))-1
        call w3fs26(jldayn,idat(1),idat(2),idat(3),idaywk,idayyr)
      elseif(idat(5).ge.24) then
        idat(5)=idat(5)-24
        jldayn=iw3jdn(idat(1),idat(2),idat(3))+1
        call w3fs26(jldayn,idat(1),idat(2),idat(3),idaywk,idayyr)
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
       SUBROUTINE W3FS26(JLDAYN,IYEAR,MONTH,IDAY,IDAYWK,IDAYYR)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: W3FS26         YEAR, MONTH, DAY FROM JULIAN DAY NUMBER
C   AUTHOR: JONES,R.E.       ORG: W342       DATE: 87-03-29
C
C ABSTRACT: COMPUTES YEAR (4 DIGITS), MONTH, DAY, DAY OF WEEK, DAY
C   OF YEAR FROM JULIAN DAY NUMBER. THIS SUBROUTINE WILL WORK
C   FROM 1583 A.D. TO 3300 A.D.
C
C PROGRAM HISTORY LOG:
C   87-03-29  R.E.JONES
C   89-10-25  R.E.JONES   CONVERT TO CRAY CFT77 FORTRAN
C
C USAGE:  CALL W3FS26(JLDAYN,IYEAR,MONTH,IDAY,IDAYWK,IDAYYR)
C
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     JLDAYN ARG LIST  INTEGER   JULIAN DAY NUMBER
C
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     IYEAR  ARG LIST  INTEGER   YEAR  (4 DIGITS)
C     MONTH  ARG LIST  INTEGER   MONTH
C     IDAY   ARG LIST  INTEGER   DAY
C     IDAYWK ARG LIST  INTEGER   DAY OF WEEK (1 IS SUNDAY, 7 IS SAT)
C     IDAYYR ARG LIST  INTEGER   DAY OF YEAR (1 TO 366)
C
C   REMARKS: A JULIAN DAY NUMBER CAN BE COMPUTED BY USING ONE OF THE
C     FOLLOWING STATEMENT FUNCTIONS. A DAY OF WEEK CAN BE COMPUTED
C     FROM THE JULIAN DAY NUMBER. A DAY OF YEAR CAN BE COMPUTED FROM
C     A JULIAN DAY NUMBER AND YEAR.
C
C      IYEAR (4 DIGITS)
C
C      JDN(IYEAR,MONTH,IDAY) = IDAY - 32075
C    &            + 1461 * (IYEAR + 4800 + (MONTH - 14) / 12) / 4
C    &            + 367 * (MONTH - 2 - (MONTH -14) / 12 * 12) / 12
C    &            - 3 * ((IYEAR + 4900 + (MONTH - 14) / 12) / 100) / 4
C
C      IYR (4 DIGITS) , IDYR(1-366) DAY OF YEAR
C
C      JULIAN(IYR,IDYR) = -31739 + 1461 * (IYR + 4799) / 4
C    &                    -3 * ((IYR + 4899) / 100) / 4 + IDYR
C
C      DAY OF WEEK FROM JULIAN DAY NUMBER, 1 IS SUNDAY, 7 IS SATURDAY.
C
C      JDAYWK(JLDAYN) = MOD((JLDAYN + 1),7) + 1
C
C      DAY OF YEAR FROM JULIAN DAY NUMBER AND 4 DIGIT YEAR.
C
C      JDAYYR(JLDAYN,IYEAR) = JLDAYN -
C     &  (-31739+1461*(IYEAR+4799)/4-3*((IYEAR+4899)/100)/4)
C
C      THE FIRST FUNCTION WAS IN A LETTER TO THE EDITOR COMMUNICATIONS
C      OF THE ACM  VOLUME 11 / NUMBER 10 / OCTOBER, 1968. THE 2ND
C      FUNCTION WAS DERIVED FROM THE FIRST. THIS SUBROUTINE WAS ALSO
C      INCLUDED IN THE SAME LETTER. JULIAN DAY NUMBER 1 IS
C      JAN 1,4713 B.C. A JULIAN DAY NUMBER CAN BE USED TO REPLACE A
C      DAY OF CENTURY, THIS WILL TAKE CARE OF THE DATE PROBLEM IN
C      THE YEAR 2000, OR REDUCE PROGRAM CHANGES TO ONE LINE CHANGE
C      OF 1900 TO 2000. JULIAN DAY NUMBERS CAN BE USED FOR FINDING
C      RECORD NUMBERS IN AN ARCHIVE OR DAY OF WEEK, OR DAY OF YEAR.
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY Y-MP8/864
C
C$$$
C
       L      = JLDAYN + 68569
       N      = 4 * L / 146097
       L      = L - (146097 * N + 3) / 4
       I      = 4000 * (L + 1) / 1461001
       L      = L - 1461 * I / 4 + 31
       J      = 80 * L / 2447
       IDAY   = L - 2447 * J / 80
       L      = J / 11
       MONTH  = J + 2 - 12 * L
       IYEAR  = 100 * (N - 49) + I + L
       IDAYWK = MOD((JLDAYN + 1),7) + 1
       IDAYYR = JLDAYN -
     &  (-31739 +1461 * (IYEAR+4799) / 4 - 3 * ((IYEAR+4899)/100)/4)
       RETURN
       END
C-----------------------------------------------------------------------
      SUBROUTINE ERRMSG(CMSG)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: ERRMSG         WRITE A MESSAGE TO STDERR
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 95-10-31
C
C ABSTRACT: WRITE A MESSAGE TO STDERR.
C
C PROGRAM HISTORY LOG:
C   95-10-31  IREDELL
C
C USAGE:    CALL ERRMSG(CMSG)
C   INPUT ARGUMENTS:
C     CMSG         CHARACTER*(*) MESSAGE TO WRITE
C
C REMARKS: THIS IS A MACHINE-DEPENDENT SUBPROGRAM.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C   MACHINE:  CRAY
C
C$$$
      CHARACTER*(*) CMSG
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      WRITE(0,'(A)') CMSG
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
      SUBROUTINE ERREXIT(IRET)
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK
C                .      .    .                                       .
C SUBPROGRAM:  ERREXIT       EXIT WITH A RETURN CODE
C   PRGMMR: IREDELL          ORG: NP23        DATE:1998-06-04
C
C ABSTRACT: EXIT WITH A RETURN CODE
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C   1999-01-26  Gilbert     - changed to use XLF utility routine exit_(n)
C                             instead of exit(n).  exit_(n) will return
C                             the proper value ( n must be 4 byte int )
C                             to the sh/ksh shell status variable $?
C                             ( $status for csh ) on the IBM SP.
C
C USAGE:    CALL ERREXIT(IRET)
C   INPUT ARGUMENT LIST:
C     IRET     - INTEGER RETURN CODE
C
C SUBPROGRAMS CALLED:
C   EXIT_      - EXITS FROM A FORTRAN PROGRAM
C
C ATTRIBUTES:
C   LANGUAGE: XLF FORTRAN 90
C   MACHINE: IBM SP
C
C$$$
      INTEGER IRET
      INTEGER(4) JRET
      JRET=IRET
      stop 95
      END
