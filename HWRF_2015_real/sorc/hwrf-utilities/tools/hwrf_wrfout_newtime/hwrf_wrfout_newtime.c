#include <stdio.h>
#include <stdlib.h>
#include <netcdf.h>
#include <stdarg.h>
#include <string.h>

void die(const char *format,...) {
  va_list ap;
  va_start(ap,format);
  vfprintf(stderr,format,ap);
  va_end(ap);
  exit(2);
}

void usage(const char *format,...) {
  va_list ap;
  fprintf(stderr,"Format: hwrf_wrfout_newtime filename datestamp\n"
                 "  where datestamp has the format \"2011-08-28_12:01:30\"\n"
                 "  and the file is an HWRF NetCDF wrfout file.\n");
  if(format!=NULL) {
    va_start(ap,format);
    vfprintf(stderr,format,ap);
    va_end(ap);
  }
  exit(1);
}

int main(int argc,char **argv) {
  int ret,fid,vid;
  const char *filename,*value;
  const char *varname="Times";
  const int timesize=19;

  char name[NC_MAX_NAME+1],dimname[NC_MAX_NAME+1];
  nc_type xtype;
  int ndims;
  int dimids[NC_MAX_VAR_DIMS];
  size_t dimsizes[NC_MAX_VAR_DIMS],dimsize,datasize;
  int natts,i;

  void * data;

  if(argc!=3)
    usage("Exactly two arguments must be supplied.\n");
  
  filename=argv[1];
  value=argv[2];

  if(strlen(value)!=timesize)
    usage("Second argument must be exactly %d characters long.\n",timesize);
  
  if(NC_NOERR!=(ret=nc_open(filename,NC_WRITE,&fid)))
    die("%s: cannot open.  NetCDF function nc_open failed with status %d (%s).\n",
        filename,nc_strerror(ret));
  
  if(NC_NOERR!=(ret=nc_inq_varid(fid,varname,&vid)))
    die("%s: cannot find variable \"%s\" in file.  NetCDF function nc_inq_varid returned status %d (%s).\n",
        filename,varname,ret,nc_strerror(ret));

  if(NC_NOERR!=(ret=nc_inq_var(fid,vid,name,&xtype,&ndims,dimids,&natts)))
    die("%s: unable to inquire information about variable \"%s\" with varid %d.  NetCDF function nc_inq_var returned status %d (%s).\n",
        filename,varname,vid,ret,nc_strerror(ret));

  if(strcmp(name,varname))
    die("%s: NetCDF nc_inq_var returned name \"%s\" for variable \"%s\" with varid %d.  Is there bug in nc_inq_varid or nc_inq_var?\n",
        filename,name,varname);

  if(ndims!=2)
    die("%s: variable \"%s\" should be two-dimensional but instead it has %d dimensions.\n",
        filename,varname,ndims);

  for(i=0;i<ndims;i++)
    if(NC_NOERR!=(ret=nc_inq_dim(fid,dimids[i],dimname,dimsizes+i)))
      die("%s: NetCDF nc_inq_dim is unable to provide information about dimension %d, the %dth dimension of variable %s; function returned status %d (%s).\n",
          filename,dimids[i],i,varname,ret,nc_strerror(ret));
  
  printf("ndims=%d dimsizes(0:2)=(%llu %llu %llu)\n",
         ndims,(unsigned long long)dimsizes[0],
         (unsigned long long)dimsizes[1],
         (unsigned long long)dimsizes[2]);

  if(dimsizes[1]!=timesize)
    die("%s: variable %s second dimension has length %llu instead of %d.\n",
        filename,varname,(unsigned long long)dimsizes[1],timesize);

  if(dimsizes[0]<1)
    die("%s: variable %s first dimension has length less than 1 (is %llu).\n",
        filename,varname,(unsigned long long)dimsizes[0]);

  datasize=sizeof(unsigned char)*dimsizes[0]*dimsizes[1];

  if(!(data=malloc(datasize)))
    die("Unable to allocate %llu bytes.\n",(unsigned long long)datasize);

  if(NC_NOERR!=(ret=nc_get_var_text(fid,vid,data)))
    die("%s: error reading variable %s: NetCDF function nc_get_var_text returned status %d (%s).\n",
        filename,varname,ret,nc_strerror(ret));

  memcpy(data,value,timesize);

  if(NC_NOERR!=(ret=nc_put_var_text(fid,vid,data)))
    die("%s: error writing variable %s: NetCDF function nc_put_var_text returned status %d (%s).\n",
        filename,varname,ret,nc_strerror(ret));

  if(NC_NOERR!=(ret=nc_close(fid)))
    die("%s: error closing file.  Some data may not have been written.  NetCDF function nc_close returned %d (%s).\n",
        filename,ret,nc_strerror(ret));

  return 0;
}
