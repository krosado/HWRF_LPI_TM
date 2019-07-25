#include <time.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <stdint.h>

/* c_etime_init: switches the timezone to UTC, needed by c_etime */

void c_etimeinit(int32_t *err) {
  *err=setenv("TZ","UTC",1);
  tzset();
}

/* c_etime: convert a YMDH number to a unix epoch time */

void c_etime(int32_t *ymdh, int64_t *epochtime) {
  struct tm t;
  int year=*ymdh/1000000;
  int month=(*ymdh/10000)%100;
  int day=(*ymdh/100)%100;
  int hour=(*ymdh)%100;
  time_t result;

  t.tm_sec=0;
  t.tm_min=0;
  t.tm_hour=hour;
  t.tm_mday=day;
  t.tm_mon=month-1;
  t.tm_year=year-1900;
  t.tm_wday=0;
  t.tm_yday=0;
  t.tm_isdst=0;
  
  result=mktime(&t);
  *epochtime=(int64_t)result;
}

/* c_wrf2epoch: convert a WRF time string to epoch time */
void c_wrf2epoch(const char wrftime[19],int64_t *epochtime,int32_t *ierr) {
  /* WRF time strings have the following format.  For the non-existant
     date (due to Gregorian Calendar time shift) of September 3, 1752,
     forty-five and a half minutes past four in the afternoon (tea
     time), WRF would spit out this time string:

     1752-09-03 16:45:30 
     0123456789012345678  ! C byte positions

     Various other characters can replace "-" and "_".  We copy that
     into another string, add a null byte at the end and replace
     non-time data with spaces:

     1752 09 03 16 45 30  // The last byte is actually a null byte,
     01234567890123456789

     Then strptime does the magic of parsing this string, and mktime
     converts to a unix epoch time.

     Arguments:
     wrftime = input: the WRF time string
     epochtime = output: the UNIX epoch time
     ierr = output: error code, or 0 on success
  */
  char wrftime0[20];
  struct tm result;
  *ierr=0;
  memcpy(wrftime0,wrftime,19);
  wrftime0[19]='\0';
  wrftime0[4]=wrftime0[7]=wrftime0[10]=wrftime0[13]=wrftime0[16]=' ';
  if( !strptime(wrftime0,"%Y %m %d %H %M %S",&result) ) {
    fprintf(stderr,"%s: cannot parse time: %s\n",wrftime0,strerror(errno));
    *ierr=errno;
    if(!*ierr) *ierr=1;
    return; /* return on error */
  }
  *epochtime=mktime(&result);
}

/* c_ytime: convert an epoch time to YMDH */
void c_ytime(int64_t *epochtime, int32_t *ymdh) {
  time_t tt=*epochtime;
  struct tm *t=gmtime(&tt);
  *ymdh=  1000000*(1900 + t->tm_year) +
            10000*(1    + t->tm_mon) +
              100*(       t->tm_mday) +
                          t->tm_hour;
}

/* Synonyms for common fortran name mangling schemes */

void c_etimeinit_(int32_t*i) { c_etimeinit(i); }
void c_etimeinit__(int32_t*i) { c_etimeinit(i); }
void C_ETIMEINIT(int32_t*i) { c_etimeinit(i); }
void C_ETIMEINIT_(int32_t*i) { c_etimeinit(i); }
void C_ETIMEINIT__(int32_t*i) { c_etimeinit(i); }

void c_etime_(int32_t *a, int64_t *b) { c_etime(a,b); }
void c_etime__(int32_t *a, int64_t *b) { c_etime(a,b); }
void C_ETIME(int32_t *a, int64_t *b) { c_etime(a,b); }
void C_ETIME_(int32_t *a, int64_t *b) { c_etime(a,b); }
void C_ETIME__(int32_t *a, int64_t *b) { c_etime(a,b); }

void c_wrf2epoch_(const char w[19],int64_t *e,int32_t *i) { c_wrf2epoch(w,e,i); }
void c_wrf2epoch__(const char w[19],int64_t *e,int32_t *i) { c_wrf2epoch(w,e,i); }
void C_WRF2EPOCH(const char w[19],int64_t *e,int32_t *i) { c_wrf2epoch(w,e,i); }
void C_WRF2EPOCH_(const char w[19],int64_t *e,int32_t *i) { c_wrf2epoch(w,e,i); }
void C_WRF2EPOCH__(const char w[19],int64_t *e,int32_t *i) { c_wrf2epoch(w,e,i); }

void c_ytime_(int64_t *e,int32_t *y) { c_ytime(e,y); }
void c_ytime__(int64_t *e,int32_t *y) { c_ytime(e,y); }
void C_YTIME(int64_t *e,int32_t *y) { c_ytime(e,y); }
void C_YTIME_(int64_t *e,int32_t *y) { c_ytime(e,y); }
void C_YTIME__(int64_t *e,int32_t *y) { c_ytime(e,y); }
