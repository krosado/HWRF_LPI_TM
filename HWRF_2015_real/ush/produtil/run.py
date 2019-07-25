"""This module implements a shell-like syntax for launching MPI and
non-MPI programs from Python.  It recognizes three types of
executables: mpi, "small serial" (safe for running on a batch node)
and "big serial" (which should be run via aprun if applicable).  There
is no difference between "small serial" and "big serial" programs
except on certain architectures (like Cray) where the job script runs
on a heavily-loaded batch node and has compute nodes assigned for
running other programs.

PROGRAM TYPES

There are three types of programs: mpi, serial and "big non-MPI."  A
"big" executable is one that is either OpenMP, or is a serial program
that cannot safely be run on heavily loaded batch nodes.  On Cray
architecture machines, the job script runs on a heavily-populated
"batch" node, with some compute nodes assigned for "large" programs.
In such environments, the "big" executables are run on compute nodes
and the small ones on the batch node.  

mpi('exename') = an executable "exename" that calls MPI_Init and
    MPI_Finalize exactly once each, in that order.

exe('exename') = a small non-MPI program safe to run on a batch node

bigexe('exename') = a big non-MPI program that must be run on a
    compute node it may or may not use other forms of parallelism

You can also make reusable aliases to avoid having to call those
functions over and over (more on that later).  Examples:

Python:   wrf=mpi('./wrf.exe')
Python:   lsl=alias(exe('/bin/ls')['-l'].env(LANG='C',LS_COLORS='never'))

Those can then be reused later on as if the code is pasted in, similar
to a shell alias.

SERIAL EXECUTION SYNTAX

Select your serial programs by exe('name') for small serial programs
and bigexe('name') for big serial programs.  The return value of those
functions can then be used with a shell-like syntax to specify
redirection and piping.  Example:

  shell version: ls -l / | wc -l
  Python version: run(exe('ls')['-l','/'] | exe('wc')['-l'])

Redirection syntax similar to the shell (< > and << operators):
  run( ( exe('myprogram')['arg1','arg2','...'] < 'infile' ) > 'outfile')

Note the extra set of parentheses: you cannot do exe('prog') < infile
> outfile because of the order of precedence of Python operators

Append also works:
  run(exe('myprogram')['arg1','arg2','...'] >> 'appendfile')

You can also send strings as input with <<
  run(exe('myprogram')['arg1','arg2','...'] << 'some input string')

One difference from shells is that < and << always modify the
beginning of the pipeline:

  shell: cat < infile | wc -l
  Python #1: ( exe('cat') < 'infile' ) | exe('wc')['-l']
  Python #2: exe('cat') | ( exe('wc')['-l'] < 'infile' )

Note that the last second one, equivalent to `cat|wc -l<infile`, would
NOT work in a shell since you would be giving wc -l two inputs.  

PARALLEL EXECUTION SYNTAX

Use mpi('exename') to select your executable, use [] to set arguments,
use multiplication to set the number of ranks and use addition to
combine different executables together into a multiple program
multiple data (MPMD) MPI program.

Run ten copies of ls -l:
  run(mpi('ls')['-l'] * 10)

Run HyCOM coupled HWRF: one wm3c.exe, 30 hycom.exe and 204 wrf.exe:
  run(mpi('wm3c.exe') + mpi('hycom.exe')*30 + mpi('wrf.exe')*204)

You can set environment variables, pipe MPI output and handle
redirection using the mpirun() function, which converts MPI programs
into an bigexe()-style object (Runner):

  shell version:
    result=$( mpirun -n 30 hostname | sort -u | wc -l )
  Python version:
    result=runstr( mpirun(mpi('hostname')*30) | exe['sort']['-u'] | exe['wc']['-l'] )

ALIASES:

If you find yourself frequently needing the same command, then you may
want to define an alias.  Let's say you want "long output" format
Japanese language "ls" output:

  exe('ls')['-l','/path/to/dir'].env(LANG='JP')

but you find yourself running that on many different directories.
Then you may want to make an alias:

  jplsl=alias(exe('ls')['-l'].env(LANG='JP'))

The return value jplsl can be treated as an exe()-like return value
since it was from exe() originally, but any new arguments will be
appended to the original set:

  jplsl['/path/to/dir']

This also works with pipelines, but only the last command in the
pipeline is modified.  
"""

import time
import produtil.mpi_impl as mpiimpl
import produtil.sigsafety
import produtil.prog as prog
import produtil.mpiprog as mpiprog
import produtil.pipeline as pipeline

__all__=['alias','exe','run','runstr','mpi','mpiserial','mpirun',
         'runbg','prog','mpiprog','mpiimpl','waitprocs',
         'InvalidRunArgument','ExitStatusException','checkrun']

class InvalidRunArgument(prog.ProgSyntaxError):
    """Raised to indicate that an invalid argument was sent into one of the run module functions."""
class ExitStatusException(Exception):
    """Raised to indicate that a program generated an invalid return
    code.  Examine the "returncode" member variable for the returncode
    value.  Negative values indicate the program was terminated by a
    signal while zero and positive values indicate the program exited.
    The highest exit status of the pipeline is returned when a
    pipeline is used.

    For MPI programs, the exit status is generally unreliable due to
    implementation-dependent issues, but this package attempts to
    return the highest exit status seen.  Generally, you can count on
    MPI implementations to return zero if you call MPI_Finalize() and
    exit normally, and non-zero if you call MPI_Abort with a non-zero
    argument.  Any other situation will produce unpredictable results."""
    def __init__(self,message,status):
        self.message=message
        self.returncode=status
    @property
    def status(self):
        """An alias for self.returncode: the exit status."""
        return self.returncode
    def __str__(self):
        return '%s (returncode=%d)'%(str(self.message),int(self.returncode))
    def __repr__(self):
        return 'NonZeroExit(%s,%s)'%(repr(self.message),repr(self.returncode))

def alias(arg):
    """Attempts to generate an unmodifiable "copy on write" version of the
argument.  The returned copy will generate a modifiable duplicate of
itself if you attempt to change it."""
    if isinstance(arg,prog.Runner):
        return prog.ImmutableRunner(arg)
    elif isinstance(arg,mpiprog.MPIRanksBase):
        arg.make_runners_immutable()
        return arg
    else:
        raise InvalidRunArgument('Arguments to alias() must be Runner objects (such as from exe()) or MPIRanksBase objects (such as from mpi() or mpiserial()).  Got: %s'%(repr(arg),))

def exe(name,**kwargs):
    """Returns a prog.ImmutableRunner object that represents a small
    serial program that can be safely run on a busy batch node."""
    return prog.ImmutableRunner([str(name)],**kwargs)

def bigexe(name,**kwargs):
    """Returns a prog.ImmutableRunner object that represents a large
    serial program that must be run on a compute node."""
    return prog.ImmutableRunner(mpiimpl.bigexe_prepend(name)+[str(name)],**kwargs)

def mpirun(arg,**kwargs):
    """Returns a prog.Runner object for the specified
    mpiprog.MPIRanksBase object."""
    return mpiimpl.mpirunner(arg,**kwargs)

def make_pipeline(arg,capture,**kwargs):
    """This internal implementation function generates a
    prog.PopenCommand object for the specified input, which may be a
    prog.Runner or mpiprog.MPIRanksBase."""
    if isinstance(arg,prog.Runner):
        runner=arg
    elif isinstance(arg, mpiprog.MPIRanksBase):
        runner=mpiimpl.mpirunner(arg,**kwargs)
    else:
        raise InvalidRunArgument(
            'Can only run a Runner object (such as from exe()) or an '
            'MPIRanksBase object (such as from mpi() or mpiserial()).  '
            'Got: %s'%(repr(arg),))
    logger=None
    if 'logger' in kwargs: logger=kwargs['logger']
    if logger is not None:
        logger.info('Starting: %s'%(repr(arg),))
        if capture: logger.info('  - and will capture output.')
    pl=pipeline.Pipeline(runner,capture=capture,logger=logger)
    if logger is not None:
        logger.debug('Pipeline is %s'%(repr(pl),))
    return pl

def runbg(arg,capture=False,**kwargs):
    """Runs the specified process in the background.  Specify
    capture=True to capture the command's output.  Returns a
    produtil.prog.PopenCommand.  Call poll() to determine process
    completion, and use the stdout_data property to get the output
    after completion, if capture=True was specified."""
    p=make_pipeline(arg,capture,**kwargs)
    p.background()
    return p

def waitprocs(procs,logger=None,timeout=None,usleep=1000):
    """Waits for one or more backgrounded processes to complete.  Logs
    to the specified logger while doing so.  If a timeout is
    specified, returns False after the given time if some processes
    have not returned.  The usleep argument is the number of
    microseconds to sleep between checks (can be a fraction).  The
    first argument, procs specifies the processes to check.  It must
    be a PopenCommand (return value from runbg) or an iterable (list
    or tuple) of such."""
    p=set()
    if isinstance(procs,produtil.prog.PopenCommand):
        p.add(procs)
    else:
        for pp in procs:
            p.add(pp)
    if logger is not None: logger.info("Wait for: %s",repr(p))
    while p: # keep looping as long as there are unfinished processes
        p2=set()
        for proc in p:
            ret=proc.poll()
            if ret is not None:
                if logger is not None:
                    logger.info("%s returned %s"%(repr(proc),repr(ret)))
            elif logger is not None and usleep>4.99999e6:
                # babble about running processes if the sleep time is long.
                logger.info("%s is still running"%(repr(proc),))
                p2.add(proc)
        p=p2

        if not p: break # done! no need to sleep...

        if usleep>4.99999e6 and logger is not None:
            # babble about sleeping if the sleep time is 5sec or longer:
            logger.info("... sleep %f ..."%(float(usleep/1.e6),))
        time.sleep(usleep/1.e6)
    return False if(p) else True

def run(arg,logger=None,sleeptime=None,**kwargs):
    """Executes the specified program and attempts to return its exit
    status.  In the case of a pipeline, the highest exit status seen
    is returned.  For MPI programs, exit statuses are unreliable and
    generally implementation-dependent, but it is usually safe to
    assume that a program that runs MPI_Finalize() and exits normally
    will return 0, and anything that runs MPI_Abort(MPI_COMM_WORLD)
    will return non-zero."""
    p=make_pipeline(arg,False,logger=logger)
    p.communicate(sleeptime=sleeptime)
    result=p.poll()
    if logger is not None:
        logger.info('  - exit status %d'%(int(result),))
    return result

def checkrun(arg,logger=None,**kwargs):
    """This is a simple wrapper round run that raises
    ExitStatusException if the program exit status is non-zero.  The
    optional run=[] argument can provide a different list of
    acceptable exit statuses.  Calling conventions and return value
    are otherwise the same as for run()"""
    r=run(arg,logger=logger)
    if kwargs is not None and 'ret' in kwargs:
        if not r in kwargs['ret']:
            raise ExitStatusException('%s: unexpected exit status'%(repr(arg),),r)
    elif not r==0:
        raise ExitStatusException('%s: non-zero exit status'%(repr(arg),),r)
    return r

def openmp(arg,threads=None):
    """Sets the number of OpenMP threads for the specified program.
    The "arg" argument must be from mpiserial, mpi, exe or bigexe.
    The optional "threads" argument is an integer number of threads.
    If it is not specified, the maximum possible number of threads
    will be used.  Note that using threads=None with
    mpirun(...,allranks=True) will generally not work unless the batch
    system has already configured the environment correctly for an
    MPI+OpenMP task with default maximum threads and ranks."""
    return mpiimpl.openmp(arg,threads)
    
def runstr(arg,logger=None,**kwargs):
    """Executes the specified program or pipeline, capturing its
    stdout and returning that as a string.  If the exit status is
    non-zero, then NonZeroExit is thrown.  You can specify an optional
    list or tuple "ret" that contains an alternative list of valid
    return codes.  All return codes are zero or positive: negative
    values represent signal-terminated programs (ie.: SIGTERM produces
    -15, SIGKILL produces -9, etc.)

    Example:
      runstr(exe('false'),ret=(1))
    succeeds if "false" returns 1, and raises ExitStatusError otherwise."""
    p=make_pipeline(arg,True,logger=logger)
    s=p.to_string()
    r=p.poll()
    if kwargs is not None and 'ret' in kwargs:
        if not r in kwargs['ret']:
            raise ExitStatusException('%s: unexpected exit status'%(repr(arg),),r)
    elif not r==0:
        raise ExitStatusException('%s: non-zero exit status'%(repr(arg),),r)
    return s

def mpi(arg,**kwargs):
    """Returns an MPIRank object that represents the specified MPI executable."""
    return mpiprog.MPIRank(arg,**kwargs)

def mpiserial(arg,**kwargs):
    """Generates an mpiprog.MPISerial object that represents an MPI
    rank that executes a serial (non-MPI) program.  The given value
    MUST be from bigexe() or exe(), NOT from mpi()."""
    return mpiprog.MPISerial(arg,**kwargs)
