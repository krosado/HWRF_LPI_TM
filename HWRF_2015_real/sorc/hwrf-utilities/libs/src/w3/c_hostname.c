#include <unistd.h>

/* Get the hostname. */
void c_hostname(char *name, int *nsize) {
  gethostname(name,*nsize);
}

/* Handle various name mangling schemes */

void c_hostname_(char *c,int *s)    { c_hostname(c,s); }
void c_hostname__(char *c,int *s)   { c_hostname(c,s); }
void C_HOSTNAME(char *c,int *s)     { c_hostname(c,s); }
void C_HOSTNAME_(char *c,int *s)    { c_hostname(c,s); }
void C_HOSTNAME__(char *c,int *s)   { c_hostname(c,s); }
