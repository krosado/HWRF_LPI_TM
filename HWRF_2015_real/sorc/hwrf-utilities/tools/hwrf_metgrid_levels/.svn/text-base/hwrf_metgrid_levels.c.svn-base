#include <stdio.h>
#include <netcdf.h>

int main(int argc,char **argv) {
  int fid, did,ierr;
  size_t len;
  const char * varname="num_metgrid_levels";

  if(argc==3) {
    varname=argv[2];
  } else if(argc!=2) {
    fprintf(stderr,"ERROR: Format: hwrf_metgrid_levels metfile.nc\n"
            "Prints to stdout the length of the num_metgrid_levels dimension.\n"
            "Exits with status 0 on success, and non-zero on failure.\n"
            "A special exit status of 2 means the NetCDF library returned\n"
            "an error.\n");
    return 2;
  }
  if( NC_NOERR != (ierr=nc_open(argv[1],NC_SHARE,&fid)) ) {
    fprintf(stderr,"%s: cannot open: %s\n",argv[1],nc_strerror(ierr));
    return 2;
  }
  if( NC_NOERR != (ierr=nc_inq_dimid(fid,varname,&did)) ) {
    fprintf(stderr,"%s: cannot find %s domain: %s\n",
            argv[1],varname,nc_strerror(ierr));
    return 2;
  }
  if( NC_NOERR != (ierr=nc_inq_dimlen(fid,did,&len)) ) {
    fprintf(stderr,"%s: cannot get dim len: %s\n",argv[1],nc_strerror(ierr));
    return 2;
  }
  if( NC_NOERR != (ierr=nc_close(fid)) ) {
    fprintf(stderr,"%s: error closing: %s\n",argv[1],nc_strerror(ierr));
    return 2;
  }

  ierr=(int)printf("%llu\n",(unsigned long long)len);

  return 0;
}
