"""Do not load this module directly.  It is meant to be loaded only by
the produtil.run module.  This module is part of the implementation of
a shell-like syntax for running programs.  The rest of the
implementation is in the produtil.run module.

This module implements a shell-like syntax of running shell programs
from Python.  This module should not be used directly: the
produtil.run implements critical parts of the functionality.
Specifically, this module implements the Runner, ImmutableRunner and
Pipeline classes.  

Runner - This class represents a process that could be run.  It keeps
track of all possible aspects of running a process, including the
command, arguments, environment variables, stdout stream, stderr
stream, stdin stream, and a list of functions or callable objects to
run before executing the problem.  Provides public functions to modify
the Runner.

ImmutableRunner - A Runner that cannot be changed: when modifying the
Runner, it returns a new object.  This is to implement shell aliases.
For example, one could make an ImmutableRunner for program to index
GRIB2 files.  All the user would have to do is add the GRIB2 file as
an argument, and capture the output.

Pipeline - Executes a list of Runners, piped from one's stdout to the
next's stdin.  Optionally captures the stdout of the pipeline.

Note that the actual work of creating the Runner or ImmutableRunner,
or turning them into Pipeline objects done by the produtil.run module.
Turning MPI programs into Runner objects is done by the
produtil.mpiprog module and produtil.mpi_impl package, with the public
interface in produtil.run.  Hence, nobody would ever load this module
directly, except for type checking (ie.: to see if your argument is a
Runner before passing it to produtil.run.checkrun).."""

import produtil.sigsafety
import StringIO,select,io,re,time,fcntl,os,logging,signal

import produtil.mpi_impl
from produtil.pipeline import launch, manage, PIPE, ERR2OUT

class ProgSyntaxError(Exception): 
    """Base class of exceptions raised when a Runner is given
    arguments that make no sense."""
class OverspecifiedStream(ProgSyntaxError): 
    """Raised when one tries to specify the stdout, stderr or stdin to
    go to, or come from, more than one location"""
class MultipleStdin(OverspecifiedStream): 
    """Raised when the caller specifies more than one source for the
    stdin of a Runner"""
class MultipleStdout(OverspecifiedStream): 
    """Raised when the caller specifies more than one destination for
    a Runner's stdout"""
class MultipleStderr(OverspecifiedStream):
    """Raised when the caller specifies more than one destination for
    a Runner's stderr"""
class InvalidPipeline(ProgSyntaxError): 
    """Raised when the caller specifies an invalid input or output
    when piping a Runner into or out of another object."""

class NotValidPosixSh(Exception): 
    """Base class of exceptions that are raised when converting a
    Runner or pipeline of Runners to a POSIX sh command, if the Runner
    cannot be expressed as POSIX sh."""
class NoSuchRedirection(NotValidPosixSh): 
    """Raised when trying to convert a pipeline of Runners to a POSIX
    sh string, if a redirection in the pipeline cannot be expressed in
    POSIX sh."""
class NotValidPosixShString(Exception): 
    """Raised when converting a Runner or pipeline of Runners to a
    POSIX sh string.  If a string is sent to a program's stdin, this
    is raised when that string cannot be expressed in POSIX sh."""
class EqualInExecutable(Exception): 
    """Raised when converting a Runner or pipeline of Runners to a
    posix sh string if a Runner's executable contains an equal ("=")
    sign."""
class EqualInEnv(Exception): 
    """Raised when converting a Runner or pipeline of Runners to a
    POSIX sh string if there is an equal ("=") sign in an environment
    variable name."""

def shvarok(s):
    """Returns True if the specified environment variable name is a
    valid POSIX sh variable name, and False otherwise."""
    if re.search(r'\A[A-Za-z][A-Za-z0-9_]*\z',s):
        return True
    else:
        return False

def shstrok(s):
    """Returns True if the specified string can be expressed as a
    POSIX sh string, and false otherwise."""
    # Only allow non-whitespace ASCII and space (chr(32)-chr(126)):
    if re.search(r'\A[a-zA-Z0-9 !"#$%&?()*+,./:;<=>?@^_`{|}~\\\]\[\'-]*\Z',s):
        return True
    else:
        return False

def shbackslash(s):
    """Given a Python str, returns a backslashed POSIX sh string, or
    raises NotValidPosixShString if that cannot be done."""
    if not shstrok(s):
        raise NotValidPosixShString('String is not expressable in POSIX sh: %s'%(repr(s),))
    if re.search(r'(?ms)[^a-zA-Z0-9_+.,/-]',s):
        return '"' + re.sub(r'(["\\\\$])',r"\\\1",s) + '"'
    return s

########################################################################

class StreamGenerator(object):
    """This is part of the internal implementation of Runner, and is
    used to convert it to a Popen for execution.  This is an abstract
    class whose subclasses create the Popen's stdout, stdin and
    stderr."""  
    def for_input(self):
        """Has no effect.  This exists only for debugging."""
        return '<unexpected:%s>'%(repr(self),)
    def for_output(self):
        """Has no effect.  This exists only for debugging."""
        return '<unexpected:%s>'%(repr(self),)
    def repr_for_err(self):
        """Returns the stderr value.  The default implementation
        returns repr_for_out(), causing stderr to receive whatever
        stdout receives."""
        return self.repr_for_out()

class FileOpener(StreamGenerator):
    """This is part of the internal implementation of Runner, used to
    convert it to a Popen for execution.  It represents stdin, stdout
    or stderr being connected to an open file.  It instructs the
    Runner to open the file before starting the process."""
    def __init__(self,filename,mode,err=False):
        self.filename=filename
        self.mode=mode
        self.err=err
    def copy(self):
        """Creates a shallow copy of this object."""
        return FileOpener(self.filename,self.mode,self.err)
    def to_shell(self):
        """Creates a POSIX sh representation of the part of the
        command that requests redirection."""
        more=''
        if self.err:
            more='2'
        if   self.mode=='ab': return '%s>> %s'%(more,shbackslash(self.filename))
        elif self.mode=='wb': return '%s> %s'%(more,shbackslash(self.filename))
        elif self.mode=='rb': return 'cat %s | '%(shbackslash(self.filename),)
        raise NoSuchRedirection('Cannot convert file open mode %s to a '
                                'POSIX sh redirection'%(self.mode,))
    @property 
    def intmode(self):
        """Returns an integer version of mode suitable for os.open"""
        intmode=None
        assert('r' in 'rb')
        if 'r' in self.mode:
            intmode=os.O_RDONLY
        elif 'w' in self.mode:
            intmode=os.O_WRONLY|os.O_CREAT|os.O_TRUNC
        elif 'a' in self.mode:
            intmode=os.O_WRONLY|os.O_CREAT|os.O_APPEND
        assert(intmode is not None)
        return intmode
    def _gen_stream(self):
        """Returns a tuple (None,stream,None,True) where "stream" is
        the opened file object."""
        return (None,os.open(self.filename,self.intmode),None,True)
    def __repr__(self):
        """Returns a string representation of this object as valid
        Python code."""
        return 'FileOpener(%s,%s)'%(repr(self.filename),repr(self.mode))
    def repr_for_in(self):
        """Part of the implementation of Runner.__repr__, this returns
        the filename and ",string=False"."""
        return repr(self.filename)+',string=False'
    def repr_for_out(self):
        """Part of the implementation of Runner.__repr__, this returns
        the filename and ",string=False".  It also appends ",append=X"
        where X is the true/false flag for appending to the file."""
        return '%s,append=%s'%(repr(self.filename),repr(self.mode=='ab'))
    def repr_for_err(self): 
        """Same as repr_for_out."""
        return self.repr_for_out()

class StringInput(StreamGenerator):
    """Represents sending a string to a process's stdin."""
    def __init__(self,obj):
        """Creates a StringInput that sends the specified object to
        stdin."""
        self.obj=obj
    def copy(self):
        """Returns a shallow copy of this object."""
        return StringInput(self.obj)
    def _gen_stream(self):
        """Returns a tuple containing (O,None,None,None) where O was
        the object sent to the StringInput constructor."""
        return (self.obj,None,None,None)
    def __repr__(self):
        """Returns a string representation of this object as valid
        Python code."""
        return 'StringInput(%s)'%(repr(self.obj),)
    def to_shell(self): 
        """Converts this object, if possible, to an echo command
        followed by a pipe ("|")."""
        return 'echo %s | '%(shbackslash(self.obj))
    def repr_for_in(self):
        """Part of the implementation of Runner.__repr__.  If
        possible, this creates valid Python code to represent
        specifying sending the given string to the stdin of a Runner.
        If the string is too long, it is abbreviated."""
        if len(self.obj)>40:
            return "%s...,string=True"%(repr(self.obj[0:37]+'...'),)
        else:
            return '%s,string=True'%(repr(self.obj),)

class StreamReuser(StreamGenerator):
    """Arranges for a stream-like object to be sent to the stdout,
    stderr or stdin of a Runner."""
    def __init__(self,obj):
        """Creates a StreamReuser for the specified stream-like object."""
        self.obj=obj
    def copy(self):
        """Returns a shallow copy of this object.  Note that means
        that the underlying stream object is not copied."""
        return StreamReuser(self.obj)
    def to_shell(self):
        """Raises NotValidPosixSh to indicate that the stream cannot
        be represented as POSIX sh."""
        raise NotValidPosixSh('Python streams cannot be passed to '
                              'remote POSIX sh processes.')
    def _gen_stream(self):
        """Returns a tuple (None,None,obj,False) where obj is the
        provided stream-like object."""
        return (None,None,self.obj,False)
    def repr_for_in(self):
        """Returns repr(obj) where obj is the given stream-like
        object."""
        return repr(self.obj)
    def repr_for_out(self):
        """Returns repr(obj) where obj is the given stream-like
        object."""
        return repr(self.obj)

class OutIsError(StreamGenerator):
    """Instructs a Runner to send stderr to stdout"""
    def __init__(self):      pass
    def copy(self):
        """Returns a new OutIsError object."""
        return OutIsError()
    def to_shell(self):      
        """Returns 2>&1"""
        return '2>&1'
    def _gen_stream(self):
        """Returns a tuple containing (None,None,pipeline.ERR2OUT,False)"""
        return (None,None,ERR2OUT,False)
    def repr_for_in(self):   
        """This should never be called.  It returns ".err2out()"."""
        return '.err2out()'
    def repr_for_out(self):  
        """Part of the representation of Runner.__repr__.  Returns
        ".err2out()" which instructs a Runner to send stderr to
        stdout."""
        return '.err2out()'
    def __eq__(self,other):  return isinstance(other,OutIsError)

########################################################################

class Runner(object):
    """This is a linked list class used to store information about a
    program or pipeline of programs to be run.  It has the capability
    of converting itself to a Pipeline object (run(Runner)), or
    converting itself to a POSIX sh command (Runner.to_shell()).  Note
    that some commands cannot be represented in POSIX sh, such as
    commands with non-ASCII characters or commands that have Python
    streams as their stdout or stdin.  Those commands can still be run
    with a Pipeline, but trying to convert them to a POSIX sh command
    will throw NotValidPosixSh or a subclass thereof."""
    def __init__(self,args,**kwargs):
        """Creates a new Runner.  The only non-keyword argument can be
        one of three things:

          1. A Runner to copy.  Every aspect of the Runner that can be
             copied will be.  Note that if a stream-like object is
             connected to stdin, stdout or stderr, it will NOT be
             copied.

          2. A list of strings.  This will be used as the command
             path, and arguments.

        Many options can be set via keyword arguments:

          clearenv=True - the envronment should be cleared before
            running this command.  Any arguments set by the env=
            keyword or the .env(...) member function ignore this.
            Also, PATH, USER, LOGNAME and HOME are retained since
            most programs cannot run without them.

          env=dict(var=value,...) - a dict of environment variables to
            set before running the Runner.  Does NOT affect this
            parent's process, only the child process.

          in=filename - a file to send to stdin.

          instr=str - a string to send to stdin

          out=filename - a file to connect to stdout.  Will truncate the file.

          outa=filename - same as "out=filename," but appends to the file.

          err2out - redirects stderr to stdout

          err=filename - a file to connect to stderr.  Will truncate the file.

          erra=filename - same as "err=filename," but appends to the file.

          prerun=[obj,anotherobj,...] - sent to self.prerun, this is a
            list of functions or callable objects to run before
            executing the process.  The objects are not called until
            execution is requested via self._gen."""

        self._stdin=self._stdout=self._stderr=self._prev=self._env=None
        self._prerun=self._cd=None

        if isinstance(args,Runner):
            r=args # other runner to copy
            self._args=list(r._args)
            if(r._stdin is not None): self._stdin=r._stdin.copy()
            if(r._stdout is not None): self._stdout=r._stdout.copy()
            if(r._stderr is not None): self._stderr=r._stderr.copy()
            if(r._prev is not None): self._prev=r._prev.copy()
            if(r._env is not None): self._env=dict(r._env)
            if(r._prerun is not None): self._prerun=list(r._prerun)
            self._copy_env=r._copy_env
        else:
            assert(isinstance(args,list))
            assert(isinstance(args[0],basestring))
            self._args=args
            self._copy_env=True

        # Initialize environment if requested:
        if 'clearenv' in kwargs and kwargs['clearenv']:   self.clearenv()
        if 'env' in kwargs:                               self._env=dict(kwargs['env'])

        # Initialize input/output/error if requested:
        if 'in' in kwargs:           self<kwargs['in']
        if 'instr' in kwargs:        self<<str(kwargs['instr'])
        if 'out' in kwargs:          self>kwargs['out']
        if 'outa' in kwargs:         self>>kwargs['outa']
        if 'err2out' in kwargs:      self.err2out()
        if 'err' in kwargs:          self.err(kwargs['err'],append=False)
        if 'erra' in kwargs:         self.err(kwargs['erra'],append=True)
        if 'cd' in kwargs:           self.cd(kwargs['cd'])
        
        # Allow a list of "prerun" callables that will be called at
        # the beginning of self._gen:
        if 'prerun' in kwargs:       self.prerun(kwargs['prerun'])
    def prerun(self,arg):
        """Adds a function or callable object to be called before
        running the program.  The callables should be very fast
        operations, and are executed by self._gen when creating the
        Pipeline.  They take, as an argument, the Runner and an
        optional "logger" keyword argument that is either None, or a
        logging.Logger to use to log messages."""
        if self._prerun is None:
            self._prerun=[arg]
        else:
            self._prerun.append(arg)
    def _stringify_arg(self,arg):
        """Returns a string representation of the given argument to
        convert it to an input to Popen:
          float - converted via %g
          int - converted via %d
          basestring - no conversion; used directly
          all others - str(arg)"""
        if isinstance(arg,float):
            return '%g'%(arg,)
        elif isinstance(arg,int):
            return '%d'%(arg,)
        elif isinstance(arg,basestring):
            return arg
        else:
            return str(arg)
    def __getitem__(self,args):
        """Can ONLY accept strings, ints, floats or iterables (tuple,
        list).  Strings, ints and floats are sent to _stringify_args,
        and the result is added to the end of the list of arguments to
        the command to run.  For iterables (tuple, list), adds all
        elements to the list of arguments, passing each through
        _stringify_args."""
        if isinstance(args,basestring) or isinstance(args,float) \
                or isinstance(args,int):
            self._args.append(self._stringify_arg(args))
        else:
            self._args.extend([self._stringify_arg(x) for x in args])
        return self
    def __str__(self): return self.__repr__()
    def __repr__(self):
        """Attempts to produce valid Python code to represent this
        Runnable.  Generally, that can be done, unless an input string
        is too long, or a stream is connected to a Python object.  In
        those cases, human-readable representations are given, which
        are not exactly Python code."""
        if self._prev is not None:
            s='%s | '%(repr(self._prev),)
        else:
            s=''
        if len(self._args)==0:
            s+='exe(<empty>)'
        else:
            s+='exe(%s)'%(repr(self._args[0]))
        if len(self._args)>1:
            s+='['+','.join([repr(x) for x in self._args[1:]])+']'
        if self._stdin is not None:
            s+='.in(%s)'%(self._stdin.repr_for_in(),)
        if self._stdout is not None:
            s+='.out(%s)'%(self._stdout.repr_for_out(),)
        if self._stderr is not None:
            if isinstance(self._stderr,OutIsError):
                s+='.err2out()'
            else:
                s+='.err(%s)'%(self._stderr.repr_for_err(),)
        if not self._copy_env:
            s+='.clearenv()'
        if self._env is not None:
            s+='.env('+(', '.join(['%s=%s'%(k,v) 
                                   for k,v in self._env.iteritems()]))+')'
        if self._prerun is not None:
            s+=''.join(['.prerun(%s)'%(repr(x),) for x in self._prerun])
        if self._cd is not None:
            s+=".cd("+repr(self._cd)+")"
        return s
    
    def __eq__(self,other):
        """Returns True if the other object is a Runner that is equal
        to this one, and False otherwise."""
        if isinstance(other,Runner):
            return self._args==other._args and \
                self._copy_env==other._copy_env and \
                self._stdin==other._stdin and \
                self._stdout==other._stdout and \
                self._stderr==other._stderr and \
                self._env==other._env and \
                self._prerun==other._prerun
        else:
            return NotImplemented

    def isplainexe(self):
        """Returns true if this is simply an executable with arguments
        (no redirection, no prerun objects, no environment
        modification, no piping), and False otherwise."""
        return self._stdin is None and self._stdout is None and \
            self._prerun is None and self._stderr is None and \
            self._env is None and self._prev is None

    def cd(self,dirpath):
        """Requests that this process run in the specified directory.
        The directory must already exist before the program starts."""
        self._cd=dirpath
        return self
    def __lt__(self,stdin):
        """Connects the given object to stdin, via inp(stdin,string=False)."""
        return self.inp(stdin,string=False)
    def __gt__(self,stdout): 
        """Connects the given object to stdout, truncating it if it is
        a file.  Same as out(stdout,append=False)."""
        return self.out(stdout,append=False)
    def __lshift__(self,stdin): 
        """Sends the specified string into stdin.  Same as
        inp(stdin,string=True)."""
        return self.inp(stdin,string=True)
    def __rshift__(self,stdout): 
        """Appends stdout to the specified file.  Same as
        out(stdout,append=True)."""
        return self.out(stdout,append=True)
    def __pos__(self): 
        """Sends stderr to stdout.  Same as err2out()."""
        return self.err2out()
    def __ge__(self,outerr): 
        """Redirects stderr and stdout to the specified file, truncating it.  
        Same as err2out().out(filename,append=False)"""
        return self.err2out().out(outerr,append=False)
    def __or__(self,other): 
        """Pipes this Runner to the other Runner.  Same as pipeto(other)."""
        return self.pipeto(other)

    def args(self):
        """Iterates over the executable and arguments of this command"""
        for arg in self._args:
            yield arg

    def copy(self,typeobj=None):
        """Returns a deep copy of this object, almost.  If stdin,
        stdout or stderr are connected to streams instead of files or
        strings, then the streams are not copied.  Instead, the exact
        same stream objects are connected to the same unit in the new
        Runner."""
        if typeobj is None: typeobj=Runner
        assert(typeobj is not None)
        r=typeobj(list(self._args))
        r._copy_env=self._copy_env
        if self._stdin  is not None: r._stdin =self._stdin .copy()
        if self._stdout is not None: r._stdout=self._stdout.copy()
        if self._stderr is not None: r._stderr=self._stderr.copy()
        if self._env is not None: r._env=dict(self._env)
        if self._prev is not None: r._prev=self._prev.copy()
        assert(r is not None)
        assert(isinstance(r,typeobj))
        return r

    def copyenv(self):
        """Instructs this command to duplicate the parent process
        environment (the default)."""
        self._copy_env=True
        return self

    def clearenv(self):
        """Instructs this command to start with an empty environment
        except for certain critical variables without which most
        programs cannot run.  (Retains PATH, USER, LOGNAME and HOME.)"""
        self._copy_env=False
        self._env={}
        return self

    def _impl_make_env(self):
        """This internal function generates information about the
        environment variables to be input to this process.  If the
        parent environment is to be passed unmodified, None is
        returned.  Otherwise, this routine returns dict of environment
        variables calculated from os.environ and internal settings."""

        if self._env is None and self._copy_env:
            return None # copy parent process environment verbatim
        env={}
        if self._copy_env:
            env=dict(os.environ)
        else:
            env={}
            for key in ('PATH','USER','LOGNAME','HOME'):
                if key in os.environ:
                    env[key]=os.environ[key]
        if self._env is not None:
            for key in self._env:
                env[key]=self._env[key]
        return env

    def env(self,**kwargs):
        """Sets environment variables for this Runner.  The variables
        should be specified as keyword arguments."""
        if self._env is None:
            self._env={}
        for key in kwargs:
            self._env[str(key)]=str(kwargs[key])
        return self

    def to_shell(self):
        """Returns a string that expresses this object as a POSIX sh
        shell command if possible, or raises a subclass of
        NotValidPosixSh if not."""
        if self._prev is not None:
            s=self._prev.to_shell()+' | '
        elif self._stdin is not None:
            s=self._stdin.to_shell()
        else:
            s=''
        if self._cd is not None:
            s+="( set -e ; cd "+shbackslash(self._cd)+" ; exec "
        if self._env is not None or not self._copy_env:
            if(re.search('=',self._args[0])):
                raise EqualInExecutable(
                    '%s: cannot have an "=" in the executable name when '
                    'modifying the environment.'%(self._args[0],))
            s+='env'
            if not self._copy_env:
                s+=' -i'
            for key in self._env:
                if(re.search('=',key)):
                    raise EqualInEnv('%s: variable name contains an "="'
                                     %(key,))
                s+=' '+shbackslash("%s=%s"%(key,self._env[key]))
            if not self._copy_env:
                for key in ('PATH','USER','LOGNAME','HOME'):
                    if not key in self._env:
                        s+=' "%s=$%s"'%(key,key)
            s+=' '
        s+=' '.join([shbackslash(x) for x in self._args])
        if self._stdout is not None: s+=' '+self._stdout.to_shell()
        if self._stderr is not None: s+=' '+self._stderr.to_shell()
        if self._cd is not None:
            s+=" )"
        return s
    def runner(self):
        """Returns self if self is modifiable, otherwise returns a
        modifiable copy of self.  This is intended to be used to
        implement unmodifiable subclasses of Runner"""
        return self
    def pipeto(self,other):
        """Specifies that this Runner will send its stdout to the
        other runner's stdin.  This will raise MultipleStdout if this
        Runner's stdout target is already specified, or MultipleStdin
        if the other's stdin is already specified."""
        if not isinstance(other,Runner):
            raise InvalidPipeline(
                'Attempting to pipe a Runner into something that is not '
                'a Runner (likely a syntax error).')
        if other._prev is not None:
            raise MultipleStdin('Attempted to pipe more than one process '
                                'into stdin of the same process')
        if self._stdout is not None:
            raise MultipleStdout('More than one stdout is detected in '
                                 'prog|prog')
        if other._stdin is not None and self._stdin is not None:
            raise MultipleStdin('More than one stdin is detected in '
                                'prog|prog')

        # Get a modifiable copy of the other Runner and pipe to it:
        rother=other.runner()
        rother._prev=self
        if rother._stdin is not None:
            self.inp(rother._stdin)
            rother._stdin=None

        # Return the other object since it is later in the pipeline
        # (this is needed for syntactic reasons):
        return rother

    def inp(self,stdin,string=False):
        """Specifies that the first Runner in this pipeline takes
        input from the given file or string specified by stdin.  If
        string=True, then stdin is converted to a string via str(),
        otherwise it must be a filename or a stream.  Raises
        MultipleStdin if the stdin source is already specified."""
        if self._prev is not None:
            self._prev.inp(stdin,string)
            return self
        # ---- to get past here, we have to be the beginning of the pipeline ----
        if self._stdin is not None:
            raise MultipleStdin('More than one stdin detected in Runner.inp')
        if isinstance(stdin,StringInput) or isinstance(stdin,FileOpener) or\
                isinstance(stdin,StreamReuser):
            self._stdin=stdin
        elif(string):
            self._stdin=StringInput(str(stdin))
        else:
            if isinstance(stdin,basestring):
                self._stdin=FileOpener(str(stdin),'rb')
            else:
                self._stdin=StreamReuser(stdin)
        return self

    def out(self,stdout,append=False):
        """Specifies that this process sends output from its stdout
        stream to the given file or stream.  The stdout object must be
        a string filename, or a stream.  If append=False, and the
        stdout is a filename, the file will be truncated, if
        append=True then it is appended.  Raises MultipleStdout if the
        stdout location is already specified"""
        if self._stdout is not None:
            raise MultipleStdout('More than one stdout detected in call '
                                 'to Runner.out')
        if isinstance(stdout,basestring):
            if append:
                self._stdout=FileOpener(str(stdout),'ab')
            else:
                self._stdout=FileOpener(str(stdout),'wb')
        else:
            self._stdout=StreamReuser(stdout)
        return self

    def err2out(self):
        """Sends stderr to stdout"""
        if self._stderr is not None:
            raise MultipleStderr(
                'More than one stderr detected in call to Runner.err')
        self._stderr=OutIsError()
        return self

    def err(self,stderr,append=False):
        """Specifies that this process sends output from its stderr
        stream to the given file or stream.  The stderr object must be
        a string filename, or a stream.  If append=False, and the
        stderr is a filename, the file will be truncated, if
        append=True then it is appended.  Raises MultipleStderr if the
        stderr location is already specified."""
        if self._stderr is not None:
            raise MultipleStderr(
                'More than one stderr detected in call to Runner.err')
        if isinstance(stderr,basestring):
            if append:
                self._stderr=FileOpener(str(stderr),'ab',True)
            else:
                self._stderr=FileOpener(str(stderr),'wb',True)
        else:
            self._stderr=StreamReuser(stderr)
        return self

    def _gen(self,pipeline,logger=None,next=None):
        """Populates a Pipeline object with information from this
        Runner.  This is a recursive function that starts at the last
        element of the pipeline (output element) and walks back to the
        first (input element).  The "next" parameter points to the
        next (output-direction) element of the pipeline.  The optional
        logger parameter is where to send log messages."""

        if self._prev is not None:
            self._prev._gen(pipeline,logger=logger,next=self)
        elif logger is not None:
            logger.debug('GEN %s: recurse to %s'%(
                    repr(self),repr(self._prev)))
        if self._prerun is not None:
            for prerun in self._prerun:
                prerun(self,logger=logger)
        if logger is not None:
            logger.debug('GEN %s: gen to %s with cmd=%s'%(
                    repr(self),repr(pipeline),str(self._args[0])))

        kwargs={}

        if self._stdin is not None:
            (string,stream,send,close)=self._stdin._gen_stream()
            if string is not None: kwargs['instring']=string
            if stream is not None: kwargs['stdin']=stream
            if send is not None: kwargs['sendin']=send
            if close is not None: kwargs['closein']=close
        if self._stdout is not None:
            (string,stream,send,close)=self._stdout._gen_stream()
            assert(string is None)
            if stream is not None: kwargs['stdout']=stream
            if send is not None: kwargs['sendout']=send
            if close is not None: kwargs['closeout']=close
        if self._stderr is not None:
            (string,stream,send,close)=self._stderr._gen_stream()
            assert(string is None)
            if stream is not None: kwargs['stderr']=stream
            if send is not None: kwargs['senderr']=send
            if close is not None: kwargs['closeerr']=close
        if self._env is not None:
            kwargs['env']=self._impl_make_env()
        if logger is not None:
            kwargs['logger']=logger
        pipeline._impl_add(self._args,(next is None),cd=self._cd,**kwargs)

########################################################################

class ImmutableRunner(Runner):
    """This subclass of Runner is unmodifiable.  It is meant to be
    used for re-usable exe()-like objects.  For example, if one wants
    an object lsl that runs exe('ls')['-l'] with optional extra
    arguments, one could do:

      lsl=ImmutableRunner(Runner('ls')['-l'])

    and then every time one does run(lsl[argument list]), it generates
    a new object without modifying the original lsl, ensuring later
    calls to lsl will have the same effect:

      lsl['/']
      lsl['~']
      lsl['/']  # prints the same as the first

    This is implemented by a copy-on-write method: if a modification
    is requested, a Runner is returned with the requested
    modifications."""
    def __init__(self,args,**kwargs):
        """Creates a new ImmutableRunner.  All arguments to this
        constructor have the same meanings as the Runner
        constructor."""
        try:
            self._init=True
            Runner.__init__(self,args,**kwargs)
            if self._prev is not None:
                self._prev=ImmutableRunner(self._prev)
        finally:
            self._init=False

    def copy(self,typeobj=None):
        """Creates a deep copy of this runner, except if stream
        objects are connected to stdin, stdout or stderr.  In that
        case, those same stream objects are still connected."""
        if typeobj is None: typeobj=ImmutableRunner
        return Runner.copy(self,typeobj)

    def runner(self):
        """Returns a modifiable version of this object (as a Runner)."""
        return self.copy(Runner)
    def _init_runner(self):
        """Do not call this function: it is an internal implementation
        function.  It returns self if self.__init__ is still being
        run, otherwise it returns self.runner()."""
        if self._init:
            x=self
        else:
            x=self.runner()
        assert(x is not None)
        assert(isinstance(x,Runner))
        return x
    def copyenv(self): return self._init_runner().copyenv()
    def clearenv(self): return self._init_runner().clearenv()
    def cd(self,cd): return self._init_runner().cd(cd)
    def env(self,**kwargs): return self._init_runner().env(**kwargs)
    def pipeto(self,other): return self.runner().pipeto(other)
    def inp(self,stdin,string=False): 
        return self._init_runner().inp(stdin,string)
    def out(self,stdout,append=False):
        return self._init_runner().out(stdout,append)
    def err(self,stderr,append=False): 
        return self._init_runner().err(stderr,append)
    def err2out(self): return self._init_runner().err2out()
    def prerun(self,arg): return self._init_runner().prerun(arg)
    def __getitem__(self,args): 
        return Runner.__getitem__(self._init_runner(),args)
    def _gen(self,pipeline,logger=None,next=None):
        """Creates a Runner object that is a duplicate of this
        ImmutableRunner, and calls its _gen function."""
        return self.runner()._gen(pipeline,logger,next)
