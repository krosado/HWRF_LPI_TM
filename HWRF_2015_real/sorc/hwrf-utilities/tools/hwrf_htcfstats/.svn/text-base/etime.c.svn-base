#include <time.h>
#include <stdlib.h>
#include <stdint.h>

/* etime_init: switches the timezone to UTC, needed by etime */

void etimeinit(int *err) {
  *err=setenv("TZ","UTC",1);
  tzset();
}

/* etime: convert a YMDH number to a unix epoch time */

void etime(int *ymdh, int64_t *epochtime) {
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

/* ytime: convert an epoch time to YMDH */
void ytime(int64_t *epochtime, int *ymdh) {
  time_t tt=*epochtime;
  struct tm *t=gmtime(&tt);
  *ymdh=  1000000*(1900 + t->tm_year) +
            10000*(1    + t->tm_mon) +
              100*(       t->tm_mday) +
                          t->tm_hour;
}

/* Synonyms for common fortran name mangling schemes */

void etimeinit_(int*i) { etimeinit(i); }
void etimeinit__(int*i) { etimeinit(i); }
void ETIMEINIT(int*i) { etimeinit(i); }
void ETIMEINIT_(int*i) { etimeinit(i); }
void ETIMEINIT__(int*i) { etimeinit(i); }

void etime_(int *a, int64_t *b) { etime(a,b); }
void etime__(int *a, int64_t *b) { etime(a,b); }
void ETIME(int *a, int64_t *b) { etime(a,b); }
void ETIME_(int *a, int64_t *b) { etime(a,b); }
void ETIME__(int *a, int64_t *b) { etime(a,b); }

void ytime_(int64_t *e,int *y) { ytime(e,y); }
void ytime__(int64_t *e,int *y) { ytime(e,y); }
void YTIME(int64_t *e,int *y) { ytime(e,y); }
void YTIME_(int64_t *e,int *y) { ytime(e,y); }
void YTIME__(int64_t *e,int *y) { ytime(e,y); }
