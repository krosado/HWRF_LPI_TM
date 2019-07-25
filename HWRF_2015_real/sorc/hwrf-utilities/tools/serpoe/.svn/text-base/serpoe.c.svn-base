#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>
#include <sys/wait.h>

static const int max_command_len=1048576; /* Limit command length to avoid buffer overflow attacks */

static int mpi_inited=0;

void die(const char *format,...) {
  va_list ap;

  va_start(ap,format);
  vfprintf(stderr,format,ap);
  va_end(ap);

  if(mpi_inited)
    MPI_Abort(MPI_COMM_WORLD,2);
  exit(2);
  abort();
}

void mpicall(int ret,const char *name) {
  char error[MPI_MAX_ERROR_STRING+1]="";
  int len=-1,err2;
  if(ret!=MPI_SUCCESS) {
    if((err2=MPI_Error_string(ret,error,&len))==MPI_SUCCESS) {
      error[len]='\0';
      die("MPI error %d when %s: %s\n",ret,name,error);
    } else {
      die("MPI error %d when %s, and MPI_Error_string could not process that error number (it said %d) so I don't even know what went wrong!\n",
          ret,name,err2);
    }
  }
}

int is_spmd(const int argc,const int rank) {
  int spmd,mpmd;
  char *pgmmodel;

  if(rank==0) {
    spmd=(argc>1);
    mpmd=(!spmd);
    pgmmodel=getenv("MP_PGMMODEL");
    if(pgmmodel) {
      if(!strcasecmp(pgmmodel,"MPMD")) {
        spmd=0 ; mpmd=1;
      } else if(!strcasecmp(pgmmodel,"SPMD")) {
        spmd=1 ; mpmd=0;
      } else {
        die("Invalid value \"%s\" for $MP_PGMMMODEL\n",pgmmodel);
      }
    }
  }
  mpicall(MPI_Bcast(&spmd,1,MPI_INTEGER,0,MPI_COMM_WORLD),"calling MPI_Bcast");
  return spmd;
}

char *append_args(const int argc,const char **argv) {
  char *buf;
  int iarg;
  size_t len,len1,buflen;

  len=0;
  for(iarg=1;iarg<argc;iarg++)
    len+=1+strlen(argv[iarg]);
  if(!(buf=(char*)malloc(len)))
    die("Unable to allocate %llu bytes.\n",(unsigned long long)len);

  buflen=len;
  len=0;
  for(iarg=1;iarg<argc;iarg++) {
    len1=strlen(argv[iarg]);
    memcpy(buf+len,argv[iarg],len1);
    len+=len1;

    if(iarg==argc-1)
      buf[len]='\0';
    else
      buf[len]=' ';
    len++;
  }
  if(len!=buflen)
    die("ASSERTION FAILURE: %llu!=%llu in %s:%d\n",
        (unsigned long long)len,
        (unsigned long long)buflen,
        __FILE__,__LINE__);

  return buf;
}

unsigned int run(const char *command) {
  unsigned int rc;
  int ret;
  ret=system(command);
  if(ret==-1)
    die("Unable to run system(\"%s\"): %s\n",command,strerror(errno));
  else if(WIFEXITED(ret))
    rc=(unsigned int)WEXITSTATUS(ret);
  else if(WIFSIGNALED(ret))
    rc=(unsigned int)(128+WTERMSIG(ret));
  else if(WIFSTOPPED(ret))
    rc=(unsigned int)(128+WSTOPSIG(ret));
  else
    rc=255;

  return rc;
}

unsigned int spmd_run(const int argc,const char **argv,const int rank) {
  char *command=append_args(argc,argv);
  unsigned int rc=run(command);
  free(command);
  return rc;
}

unsigned int mpmd_run(const int argc,const char **argv,const int rank) {
  char *file,*command;
  size_t len,cmdlen;
  unsigned int icmdlen;
  const int lentag=3013, strtag=3014;
  int ilen;
  unsigned int rc;

  /* Determine name of command file */
  if(rank==0) {
    file=getenv("MP_CMDFILE");
    if(!file)
      die("When using MPMD mode, you must specify $MP_CMDFILE\n");
    len=strlen(file);
    if(!len)
      die("When using MPMD mode, you must specify $MP_CMDFILE\n");
    ilen=len;
  }

  /* Read command file on root */
  if(rank==0) {
    FILE *f=fopen(file,"rt");
    char buf[max_command_len],*cret,*point;
    size_t buflen=0;
    int lastline,blankline;
    int irank,commsize;

    if(!f)
      die("Unable to open command file $MP_CMDFILE=\"%s\": %s\n",
          file,strerror(errno));

    mpicall(MPI_Comm_size(MPI_COMM_WORLD,&commsize),"getting size of MPI_COMM_WORLD");

    lastline=0;
    for(irank=0;irank<commsize;irank++) {
      blankline=1;

      /* Skip over blank lines and comment lines */
      while(blankline && !lastline) {
        cret=fgets(buf,max_command_len,f);
        if(cret!=buf) {
          if(feof(f))
            lastline=1;
          else
            die("I/O error reading command file \"%s\": %s\n",file,strerror(errno));
        }

        for(point=buf;*point;point++) {
          if(*point==' ' || *point=='\t')
            continue;
          else if(*point=='\n' || *point=='\r')
            break; /* end of line */
          else if(*point=='#')
            break;
          else {
            blankline=0;
            break;
          }
        }
      }

      /* Handle command files that are too short */
      if(lastline && irank!=commsize-1)
        die("Not enough ranks in $MP_CMDFILE=\"%s\" for %d tasks (only %d present)\n",
            file,commsize,irank+1);

      if(blankline)
        die("Not enough ranks in $MP_CMDFILE=\"%s\" for %d tasks (only %d present)\n",
            file,commsize,irank);

      for(point=buf;*point;point++) {
        if(*point=='\r' || *point=='\n') {
          *point='\0';
          break;
        }
      }

      cmdlen=point-buf+1;
      icmdlen=cmdlen;
      if(cmdlen!=icmdlen) {
        die("Command has length %llu which is too long to represent the length with a %d-byte integer.\n",cmdlen,sizeof(int));
      }

      if(irank==0) {
        command=(char*)malloc(cmdlen);
        memcpy(command,buf,cmdlen);
      } else {
        mpicall(MPI_Send(&icmdlen,1,MPI_INTEGER,irank,lentag,MPI_COMM_WORLD),
                "sending command length");
        mpicall(MPI_Send(buf,cmdlen,MPI_CHARACTER,irank,strtag,MPI_COMM_WORLD),
                "sending command");
      }
    }
  } else {
    mpicall(MPI_Recv(&cmdlen,1,MPI_INTEGER,0,lentag,MPI_COMM_WORLD,MPI_STATUS_IGNORE),
            "recieving command length from root");
    command=malloc(cmdlen);
    if(!command)
      die("Unable to allocate %llu bytes for rank %d command\n",
          (unsigned long long)cmdlen,rank);
    mpicall(MPI_Recv(command,cmdlen,MPI_CHARACTER,0,strtag,
                     MPI_COMM_WORLD,MPI_STATUS_IGNORE),
            "receiving command from root");
    command[cmdlen-1]='\0';
  }

  rc=run(command);
  free(command);
  return rc;
}

int main(int argc,char **argv) {
  int err,rank;
  unsigned int rc;
  int maxcode,mycode,spmd,mpmd;

  mpicall(MPI_Init( &argc, &argv ),"calling MPI_Init");
  mpi_inited=1;

  mpicall(MPI_Errhandler_set(MPI_COMM_WORLD, MPI_ERRORS_RETURN),"initializing mpi error handler");
  mpicall(MPI_Comm_rank(MPI_COMM_WORLD,&rank),"determining mpi rank");

  /* Determine the mode: SPMD or MPMD */
  spmd=is_spmd(argc,rank);
  mpmd=!spmd;

  if(spmd) {
    if(argc<2)
      die("Incorrect format for SPMD mode.  Format: %s command to run\nWill run the command on all MPI ranks and exit with highest error status.\n",argv[0]);
    rc=spmd_run(argc,argv,rank);
  } else {
    if(argc>1)
      die("ERROR.  Do not specify arguments in MPMD mode.\n");
    rc=mpmd_run(argc,argv,rank);
  }
 
  /* Determine the maximum return code */
  mycode=rc;
  maxcode=255;
  mpicall(MPI_Allreduce(&mycode,&maxcode,1,MPI_INTEGER,MPI_MAX,MPI_COMM_WORLD),"using an mpi_allreduce to get the maximum return code");

  mpicall(MPI_Finalize(),"finalizing MPI library");
  return maxcode;
}
