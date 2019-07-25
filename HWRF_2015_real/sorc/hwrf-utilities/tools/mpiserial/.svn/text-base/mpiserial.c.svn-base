/**********************************************************************/
/*  PROGRAM MPISERIAL

PURPOSE: This program is a simple MPI wrapper that runs a serial
program on each MPI rank.  It has been tested on GAEA, Cirrus,
Stratus, Tide, Gyre, Zeus and Jet.  Each MPI rank has two variables
set: SCR_COMM_RANK and SCR_COMM_SIZE containing the MPI rank and
communicator size.  

CAVEATS: This program is not a substitute for a vendor MPMD interface
that provides support for MPI in the commands, in that it can only
handle purely serial programs.  The script that is executed cannot run
any MPI programs, or most MPI implementations (including those on
Zeus, GAEA, Tide and Gyre) will fail in generally unpredictable ways.
The coupled model case is an example of what must be handled by the
vendor's MPMD method.

AUTHORSHIP: 

 circa 2010-2012 - various implementations made by Sam
   Trahan, George Vandenberghe and Hendrik Tollman.  

 late 2012 - Sam Trahan made a version that included best features of
   all versions

 early 2013 - wrong version (old wave model version) given to NCO,
   lacking error handling and SPMD support

 2013 March - "Merged" version tested on Tide, Gyre, GAEA, Zeus, Jet,
   Cirrus and Stratus and finalized.

---------

CALLING CONVENTION: This program can be called in one of two different
ways:

SINGLE PROGRAM MULTIPLE DATA (SPMD)

  mpiexec mpiserial 'command to run'

Called like that, the program will run the specified command on all
MPI ranks using /bin/sh.  This is intended to be used to execute
simple commands like hostname or sync.  However, more complex programs
can differentiate between tasks using $SCR_COMM_RANK.

Multiple arguments can be specified in SPMD mode, but they will simply
be appended to one another with a space between each.  That behavior
may be changed eventually to correctly handle arguments via the execvp
family of functions.


MULTIPLE PROGRAM MULTIPLE DATA (MPMD)

  cp /my/command/file cmdfile
  mpiexec mpiserial

OR

  export SCR_CMDFILE=/my/command/file
  mpiexec mpiserial

The program will read the first N lines of the command file, where N
is the size of MPI_COMM_WORLD.  Each rank M runs the command on line
M+1 (keeping in mind that MPI ranks are zero-based).  If the command
file is too short, extra ranks will run "/bin/true".

---------

EXIT STATUS: The return status from mpiserial is 0 if the serial
programs all exited with status 0.  If a command could not be
executed, or if an MPI error is encountered, mpiserial will return a
non-zero status.

 */
/**********************************************************************/

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

static const int max_command_len=1048576; /* Limit command length to avoid buffer overflow attacks */

static int mpi_inited=0, mpi_rank=-99;
static const char * const  default_cmdfile="cmdfile"; /* default cmdfile name */
static const char * const  special_non_command="***"; /* don't run a command, exit 0 */

void die(const char *format,...) {
  /* Error handling function.  Exits program with a message, calling
     MPI_Abort if needed.  On some MPI implementations it is critical
     to call MPI_Abort or the program may hang. */
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
  /* This is the MPI error handling function.  It is a simple wrapper
     around "die" that generates an intelligible error message */
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

int scr_immediate_exit(void) {
  char *ie;
  ie=getenv("SCR_IMMEDIATE_EXIT");
  if(!ie) return 0;
  if(!strcmp(ie,"YES"))
    return 1;
  return 0;
}

int is_spmd(const int argc,const int rank) {
  /* Decides if the program is called in SPMD mode.  Returns 1 for
     SPMD, 0 for MPMD and aborts if unsure. */
  int spmd,mpmd;
  char *pgmmodel;

  if(rank==0) {
    spmd=(argc>1);
    mpmd=(!spmd);
    pgmmodel=getenv("SCR_PGMMODEL");
    if(pgmmodel) {
      if(!strcasecmp(pgmmodel,"MPMD")) {
        spmd=0 ; mpmd=1;
      } else if(!strcasecmp(pgmmodel,"SPMD")) {
        spmd=1 ; mpmd=0;
      } else {
        die("Invalid value \"%s\" for $SCR_PGMMMODEL\n",pgmmodel);
      }
    }
  }
  mpicall(MPI_Bcast(&spmd,1,MPI_INTEGER,0,MPI_COMM_WORLD),"calling MPI_Bcast");
  return spmd;
}

char *append_args(const int argc,const char **argv) {
  /* Appends contents of argv together with spaces in between,
     allocating memory as needed.  Returns the resulting string. */
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
  /* Runs the specified command, and returns an exit status which will
     be in the range 0-255.  This correctly handles killed or stopped
     jobs, returning 128 plus the signal number. */
  unsigned int rc=99;
  int ret;

  if(!strcmp(command,special_non_command))
    return 0;

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

  if(rc>255) rc=255;

  return rc;
}

unsigned int spmd_run(const int argc,const char **argv,const int rank) {
  /* Executes the specified command using the "run" function,
     returning the resulting error code */
  char *command=append_args(argc,argv);
  unsigned int rc;
  rc=run(command);
  free(command);
  return rc;
}

void hydra_workaround(void) {
  struct stat s;
  if(!fstat(0,&s) && S_ISDIR(s.st_mode)) {
    fprintf(stderr,"warning: stdin is a directory; reopening /dev/null.  This is a bug in the Hydra MPI.  Submit a ticket to your helpdesk.\n");
    int x=open("/dev/null",O_RDONLY);
    if(x>0)
      dup2(x,0);
    else
      fprintf(stderr,"error: could not open /dev/null: %s\n",strerror(errno));
  }
}

void set_comm_size_rank(int commsize,int rank) {
  /* Sets the SCR_COMM_COMM and SCR_COMM_RANK variables to the
     specified values */
  char buffer[300];
  sprintf(buffer,"%d",rank);
  setenv("SCR_COMM_RANK",buffer,1);
  sprintf(buffer,"%d",commsize);
  setenv("SCR_COMM_SIZE",buffer,1);
}

unsigned int mpmd_run(const int argc,const char **argv,const int rank) {
  /* Executes the specified command using the "run" function,
     returning the resulting error code */

  char *file,*command;
  size_t len,cmdlen;
  unsigned int icmdlen;
  const int lentag=3013, strtag=3014;
  int ilen;
  unsigned int rc;

  /* Determine name of command file */
  if(rank==0) {
    if( (file=getenv("SCR_CMDFILE")) && (len=strlen(file)) ) {
      /* We have a non-zero-length command file name in $SCR_CMDFILE */
      ilen=len;
    } else {
      /* We do not have a valid command file name so use the default */
      size_t slen=strlen(default_cmdfile)+1;
      if( !(file=malloc(slen)) )
        die("Unable to allocate %llu bytes: %s\n",
            (unsigned long long)slen,strerror(errno));
      strcpy(file,default_cmdfile);
      ilen=strlen(default_cmdfile);
    }
  }

  /* Read command file on root */
  if(rank==0) {
    FILE *f=fopen(file,"rt");
    char buf[max_command_len],*cret,*point;
    size_t buflen=0;
    int lastline,blankline;
    int irank,commsize;

    if(!f)
      die("Unable to open command file $SCR_CMDFILE=\"%s\": %s\n",
          file,strerror(errno));

    mpicall(MPI_Comm_size(MPI_COMM_WORLD,&commsize),"getting size of MPI_COMM_WORLD");

    lastline=0;
    for(irank=0;irank<commsize;irank++) {
      blankline=1;

      /* Skip over blank lines and comment lines */
      while(blankline && !lastline) {
        cret=fgets(buf,max_command_len,f);
        if(cret!=buf) {
          if(feof(f)) {
            lastline=1;
            strcpy(buf,special_non_command);
          } else
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
        die("Not enough ranks in $SCR_CMDFILE=\"%s\" for %d tasks (only %d present)\n",
            file,commsize,irank+1);

      if(blankline)
        die("Not enough ranks in $SCR_CMDFILE=\"%s\" for %d tasks (only %d present)\n",
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
    mpicall(MPI_Recv(&icmdlen,1,MPI_INTEGER,0,lentag,MPI_COMM_WORLD,MPI_STATUS_IGNORE),
            "receiving command length from root");
    cmdlen=icmdlen;
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
  int maxcode,mycode,spmd,mpmd,commsize;

  mpicall(MPI_Init( &argc, &argv ),"calling MPI_Init");
  mpi_inited=1; /* indicate to "die" that MPI_Init was called */

  hydra_workaround(); /* make sure stdin is not a directory */

  /* Enable MPI error handling so that we can give intelligent error messages */
  mpicall(MPI_Errhandler_set(MPI_COMM_WORLD, MPI_ERRORS_RETURN),"initializing mpi error handler");

  /* Determine the rank and size so we can forward that to the child processes */
  mpicall(MPI_Comm_rank(MPI_COMM_WORLD,&rank),"determining mpi rank in MPI_COMM_WORLD");
  mpicall(MPI_Comm_size(MPI_COMM_WORLD,&commsize),"determining MPI_COMM_WORLD size");

  mpi_rank=rank; /* avoid duplicate calls by storing rank */

  /* Determine the mode: SPMD or MPMD */
  spmd=is_spmd(argc,rank);
  mpmd=!spmd;

  /* Set the $SCR_COMM_RANK and $SCR_COMM_SIZE environment variables */
  set_comm_size_rank(commsize,rank);

  /* Run the command */
  if(spmd) {
    if(argc<2)
      die("Incorrect format for SPMD mode.  Format: %s command to run\nWill run the command on all MPI ranks and exit with highest error status.\n",argv[0]);
    rc=spmd_run(argc,(const char**)argv,rank);
  } else {
    if(argc>1)
      die("ERROR.  Do not specify arguments in MPMD mode.\n");
    rc=mpmd_run(argc,(const char**)argv,rank);
  }

  /* Exit immediately if non-zero exit status and
     SCR_IMMEDIATE_EXIT=YES */
  if(rc && scr_immediate_exit()) {
    fprintf(stderr,"%s: exit status was %d and SCR_IMMEDIATE_EXIT is enabled.  Calling MPI_Abort.\n",argv[0],rc);
    mpicall(MPI_Abort(MPI_COMM_WORLD,255),"calling MPI_Abort");
    return 255;
  }
 
  /* Determine the maximum return code */
  mycode=rc;
  maxcode=255;
  mpicall(MPI_Allreduce(&mycode,&maxcode,1,MPI_INTEGER,MPI_MAX,MPI_COMM_WORLD),"using an mpi_allreduce to get the maximum return code");

#ifdef MPI_ABORT_FOR_NONZERO_EXIT
  if(mpi_rank==0 && maxcode!=0)  
    fprintf(stderr,"%s: maximum exit status was %d.  Calling MPI_Abort instead of exiting to work around a problem in your MPI implementation.\n",argv[0],maxcode);
#endif /* MPI_ABORT_FOR_NONZERO_EXIT */
  
  /* This final MPI_Barrier is necessary on some platforms: */
  mpicall(MPI_Barrier(MPI_COMM_WORLD),"during final MPI_Barrier");

#ifdef MPI_ABORT_FOR_NONZERO_EXIT
  if(maxcode!=0) {
    /* Have to MPI_Abort in order to ensure a non-zero exit status due
       to SGI MPT bug. */
    mpicall(MPI_Abort(MPI_COMM_WORLD,maxcode),"calling MPI_Abort");
  } else
#endif /* MPI_ABORT_FOR_NONZERO_EXIT */

    mpicall(MPI_Finalize(),"finalizing MPI library");

  /* Should never reach this line */
  return maxcode;
}
