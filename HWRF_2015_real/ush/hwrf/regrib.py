"""This module provides an object-oriented interface to the low-level
GRIB manipulation routines in produtil.gribop.  It also sits on top of
the produtil.taskflow module, providing a means to grab GRIB files
from Task and Product objects, processing them upon availability."""

import os,stat,errno,os.path,copy,threading, collections, logging
import produtil.datastore, produtil.run

from hwrf.exceptions import RegribError,GRIBInputError,Subsetless,\
    InvalidRegribResult,RegribProductError,NoProductError, \
    ProductAmbiguityError,RegribManyError, \
    RegribKeyError,RegribGridError,GridlessError,GridMismatchError,\
    NoIndexError

from produtil.run import exe,alias,run,runstr
from produtil.datastore import UpstreamFile,FileProduct

__all__=['GRIBOp','GRIB2Op','GRIB1Op','GRIBGrid','GRIBSubsetter',
         'GRIB1Merge','Regrib','GRIB1','GRIB2','GRIB1File',
         'GRIB2File','UpstreamGRIB1','GRIB2Product','quarterDegree']

GRIB1=1
GRIB2=2

########################################################################

class RegribBase(object):
    """This is the abstract base class of Regrib and RegribMany and
    should not be instantiated directly.  It exists only to reduce
    code duplication."""
    def __init__(self,logger=None,prodargs=None,prodkwargs=None,_base=None,
                 cnvgrib_g12=None,cnvgrib_g21=None,wgrib=None,copygb=None):
        """Creates a RegribBase.  

          _base -- another RegribBase.  If this is specified, all other
            arguments are ignored, and the _base is copied.  This is the
            implementation of self.copy()

          logger -- the logging.Logger that should be used when regribbing
          prodargs -- a list of additional arguments to send to "products"
            iterators.
          prodkwargs -- a dict of additional keyword arguments to send to
            "products" iterators.

        Aliases.  These are used when regribbing, to run various
        programs.  They should all be produtil.prog.ImmutableRunner
        objects, from produtil.run.alias.

          cnvgrib_g12 -- cnvgrib -p32 -g12
            The -p32 is the GRIB2 compression type.  Another compression
            type is acceptable, but will not match what is used in the
            operational HWRF.
          cnvgrib_g21 -- cnvgrib -g21
          wgrib -- the wgrib program, with no arguments
          copygb -- the copygb program, with no arguments"""
        super(RegribBase,self).__init__()
        if _base is not None:
            self._copygb=_base.copygb
            self._wgrib=_base.wgrib
            self._cnvgrib_g12=_base.cnvgrib_g12
            self._cnvgrib_g21=_base.cnvgrib_g21
            self._logger=_base.logger
            self._prodargs=list(_base._prodargs)
            self._prodkwargs=dict(_base._prodkwargs)
        else:
            assert(copygb is not None)
            if copygb is None: copygb=alias(exe('copygb'))
            if wgrib is None:  wgrib=alias(exe('wgrib'))
            if cnvgrib_g12 is None: cnvgrib_g12=alias(exe('cnvgrib')['-g12','-p32'])
            if cnvgrib_g21 is None: cnvgrib_g21=alias(exe('cnvgrib')['-g21'])
            
            self._copygb=copygb
            self._cnvgrib_g12=cnvgrib_g12
            self._cnvgrib_g21=cnvgrib_g21
            self._wgrib=wgrib

            if prodargs is None:
                self._prodargs=[]
            else:
                self._prodargs=list(prodargs)
            if prodkwargs is None:
                self._prodkwargs={}
            else:
                self._prodkwargs=dict(prodkwargs)
            self._logger=logger

    def getcopygb(self):
        """Returns the copygb ImmutableRunner"""
        return self._copygb
    def getcnvgrib_g12(self): 
        """Returns the cnvgrib -p32 -g12 command, which should be an
        ImmutableRunner"""
        return self._cnvgrib_g12
    def getcnvgrib_g21(self): 
        """Returns the cnvgrib -g21 command, which should be an
        ImmutableRunner."""
        return self._cnvgrib_g21
    def getwgrib(self):
        """Returns the wgrib command, which should be an
        ImmutableRunner."""
        return self._wgrib
    def getlog(self):      
        """Returns the logger.Logging object to use for logging
        messages, or None if no logger was provided to the
        constructor."""
        return self._logger
    def setlog(self,val):
        """Sets the logger.Logging object to use for logging
        messages."""
        self._logger=val
        return self._logger
    def getprodargs(self):
        """Returns the array of arguments to send to the products
        iterators."""
        return self._prodargs
    def getprodkwargs(self):  
        """Returns a dict of keyword arguments to send to the products
        iterators."""
        return self._prodkwargs
    
    logger=property(getlog,setlog,None,
        """Either None or the logging.Logger object for this Regrib.""")
    prodargs=property(getprodargs,None,None,
        """A list of additional positional arguments to send to Task.products.""")
    prodkwargs=property(getprodkwargs,None,None,
        """A dict of additional keyword arguments to send to Task.products.""")
    wgrib=property(getwgrib,None,None,
        """Returns None or a wgrib object suitable for produtils.run.run""")
    copygb=property(getcopygb,None,None,
        """Returns None or a copygb object suitable for produtils.run.run""")
    cnvgrib_g21=property(getcnvgrib_g21,None,None,
        """Returns None or a cnvgrib -g21 object suitable for produtils.run.run""")
    cnvgrib_g12=property(getcnvgrib_g12,None,None,
        """Returns None or a cnvgrib -g12 object suitable for produtils.run.run""")

########################################################################

class Regrib(RegribBase):
    """This is a helper class indended to be used internally by
    RegribMany.  It performs a small number of GRIB manipulations
    intended to produce the result of a single GRIB expression."""
    def __init__(self,result,op,workprefix=None,**kwargs):
        """Creates a Regrib object whose result has the given name (in
        "result") and produces the output of the specified operation
        "op", prepending the given prefix "workprefix" to temporary
        and output filenames."""
        super(Regrib,self).__init__(**kwargs)
        (self._result,self._op,self._step,self._workprefix)=(result,op,0,None)
        self.set_workprefix(workprefix)
    def set_workprefix(self,workprefix=None):
        """Sets the work prefix to the given value, or sets it to a
        reasonable default if workprefix is None or missing.  The work
        prefix is not necesarily a directory: it can include
        additional text to prepend to the filename."""
        if workprefix is None:
            self._workprefix=os.path.join('./',os.path.basename(result)+'.step')
        else:
            self._workprefix=workprefix
    def reset(self,result,op,workprefix=None):
        """Discards all temporary data, and re-initializes this object
        for the specified result name, operator and workprefix.  This
        is used by RegribMany to re-use a single Regrib object over
        and over."""
        self.set_workprefix(workprefix)
        self._result=result
        self._op=op
        self._step=0
    def getop(self):
        """Returns the GRIBBase object that performs this Regrib's work."""
        return self._op
    op=property(getop,None,None,
                """Returns the GRIBBase object that performs this Regrib's work""")
    def is_ready(self,**kwargs):
        """Returns True if this Regrib object's operator is "ready"
        (according to is_ready) and False otherwise."""
        return self.op.is_ready(self,**kwargs)
    def input_valid(self,**kwargs):
        """Returns True if the specified kwargs are valid for this
        Regrib object's operator and False otherwise."""
        return self.op.input_valid(self,**kwargs)
    def make(self,**kwargs):
        """Runs this Regrib object's operator's make function with the
        given keyword arguments, returning the results."""
        result=self.op.make(self,**kwargs)
        assert(result is not None)
        if not (isinstance(result,GRIBBase) or isinstance(result,GRIBGrid)):
            logging.critical(
                'Invalid result for type=%s repr=%s which came from running '
                'self.op.make(**kwargs) on an op=%s and kwargs=%s'%
                (type(result).__name__,repr(result),repr(self.op),
                 repr(kwargs)))
        return result
    def indextemp(self,grib):
        """Generates a temporary filename to use for the specified
        GRIB1/2 file's index file."""
        if not isinstance(grib,basestring):
            raise TypeError('Regrib.indextemp requires a string argument, not %s'
                            %(type(grib).__name__))
        if(grib.startswith(self._workprefix)):
            return grib+'.wgrib_s'
        else:
            self._step+=1
            out="%s%02d.%s"%(self._workprefix,self._step,
                             os.path.basename(grib))
            return out
    def gribtemp(self,what=None):
        """Returns a tuple (grib,index) containing a suggested GRIB1/2
        filename and index filename.  The optional argument "what"
        must contain no shell metacharacters and is incorporated into
        the file name."""
        self._step+=1
        if what is None:
            what='tmp'
        out="%s%02d.%s"%(self._workprefix,self._step,str(what))
        return (out,out+'.wgrib_s')
    def tempfile(self,what=None):
        """Creates a name of a temporary file.  The "what" argument is
        a terse explanation of the purpose of the file (such as
        "merge" or "to-grib2") to include in the filename."""
        self._step+=1
        if what is None:
            what='tmp'
        out="%s%02d.%s"%(self._workprefix,self._step,str(what))
        return out

########################################################################

class RegribMany(RegribBase):
    """This class keeps track of a set of named regribbing operations
    and target grids.  Each operation may connect to another by name,
    allowing the output of one operation to be the input to one or
    more other operations.  The RegribMany.make can then produce the
    output of an operation, on the condition that its inputs from
    earlier operations are ready.  

    The RegribMany is not intended to be used alone.  Instead, the
    GRIBTask cass in hwrf.gribtask should be used to execute
    RegribMany tasks.  You can run multiple GRIBTasks at the same
    time, on the same RegribMany outputs, and the GRIBTasks will
    automatically work together via file locking and a database to
    produce the outputs in parallel, as fast as possible.

    Here is a simple example of the use of a RegribMany object:

    # This code assumes a PostManyWRF object is available as "mypost",
    # and HWRFConfig as "conf", and two WRFDomain are available as
    # "d01" and "d02".  We also assume the post has already been run.

    r = RegribMany()
    getd01 = igrb1(mypost,domain=d01)
    getd02 = igrb1(mypost,domain=d02)
    r.add('cgrid',clatlon(getd02,res=[0.05,0.05],size=[30.,30.],
          scan=136,n=[600,600]))
    r.add('cGRIB1',getd01*r.grid('cgrid')+getd02*r.grid('cgrid'))
    r.add('cGRIB2',r.GRIB('cGRIB1') * GRIB2)
    r.to_intercom('output_f{fahr:03d}.grib2','cGRIB2')
    gribber = GribTask(conf.datastore,conf,'mygrib',r,conf.cycle,
                       126*3600, 6*3600)
    gribber.run()

    Now lets step through that and explain what it does.  

      r = RegribMany() - creates a RegribMany.
    First, we create the RegribMany object.  We're actually doing this
    in a sloppy way: it will search the $PATH for cnvgrib, wgrib, and
    other programs.  It is better to specify the exact location.  That
    can be done by passing arguments to the RegribMany constructor.
    See the RegribBase instructor for a list.

      getd01 = igrb1(mypost,domain=d01)
      getd02 = igrb1(mypost,domain=d02)
    This creates a pair of GRIB1Selectors, operations recognized by
    RegribMany.  A GRIB1Selector takes a Task, in this case mypost,
    and calls its "products" function to get a Product as input to the
    next step in a regribbing operation.  The domain=d01 is passed
    into products, as is the time=X for each time X the RegribMany
    processes.

      r.add('cgrid',clatlon(getd02,res=[0.05,0.05],size=[30.,30.],
            scan=136,n=[600,600]))
    This instructs the RegribMany to add an operation to its
    dictonary.  The operation is called "cgrid", and consists of a
    GRIBGridCompute created by the clatlon function, defined later in
    this module.  The clatlon takes our getd02, and creates a 30x30
    degree, 600x600 gridpoint, 0.05 degree grid, centered on that
    domain at each time.  If domain d02 moves, this clatlon output
    will be a moving domain.

      r.add('cGRIB1',getd01*r.grid('cgrid')+getd02*r.grid('cgrid'))
    This adds another operation to the RegribMany's dictionary, this
    time a grib output operation called cGRIB1.  The r.grid('cgrid')
    looks up the cgrid operation we just inserted, connecting that to
    the cGRIB1.  Recall that getd01 and getd02 are also operations,
    and produce a GRIB1 file.  When we multiply that by a grid, it
    creates a new operation that will use copygb to output a new GRIB1
    file on that grid.  Hence, we have created two temporary files:

      getd01*r.grid('cgrid') = domain 1's post output on the cgrid
      getd02*r.grid('cgrid') = domain 2's post output on the cgrid

    When we add those two together, it pastes the second one on top of
    the first via a copygb merge operation.  That result is then
    stored as cGRIB1.

      r.add('cGRIB2',r.GRIB('cGRIB1') * GRIB2)
    This adds a third operation to the RegribMany, called cGRIB2.  The
    r.GRIB('cGRIB1') looks up the operation that creates cGRIB1,
    connecting that to the new cGRIB2.  When we multiply by the
    special constant GRIB2, it creates a new operation to convert that
    file to GRIB2.

      r.to_com('output_0p05_degree_f{fahr:03d}.grib2','cGRIB2')
    This sets the delivery location for the cGRIB2 outputs.  It will 
    create files with names like:
      output_0p05_degree_048.grib2   # 48hr output file
    Without that command, the output will go to automatically-generated
    locations somewhere in intercom, which is likely not what you want.

      gribber = GRIBTask(conf.datastore,conf,'mygrib',r,conf.cycle,
                         126*3600, 6*3600)
    This creates a GRIBTask that will execute the regribber we just made.

    The GRIBTask is configured to be able to run our RegribMany for
    all forecast times starting at the analysis time (conf.cycle) and
    running to 126 hours afterwards (126*3600), every six hours
    (6*3600).  The GRIBTask will set the time= keyword argument to
    mypost.products, allowing each RegribMany operation to run on the
    correct input time.

      gribber.run()

    This tells the gribber to run to completion, generating our output
    files."""
    def __init__(self,_copy=None,workprefix=None,**kwargs):
        """Creats a new RegribMany.  The workprefix, if specified,
        sets the location in which to run.  The _copy is used by the
        self.copy() for copying.  All other arguments are passed to
        the superclass (RegribBase) constructor."""
        assert('time' not in kwargs)
        self._name_deliver=collections.defaultdict(list)
        if _copy is not None:
            RegribBase.__init__(self,_base=_copy)
            self._workprefix=_copy._workprefix
            self._gens=dict(_copy._gens)
            self._order=list(_copy._order)
            self._kwargs=dict(_copy._kwargs)
            self._delivery=copy.copy(_copy._delivery)
            for f in self._delivery:
                self._name_deliver[f[2]]=f
        else:
            assert( not ( not 'copygb' in kwargs or kwargs['copygb'] is None ) )
            RegribBase.__init__(self,**kwargs)
            if workprefix is None:
                workprefix='./'
            self._workprefix=workprefix
            self._delivery=list()
            self._gens=dict()
            self._kwargs=dict()
            self._order=list()
        self._data=None
        self._lock=threading.RLock()
    def __enter__(self):
        """Has no effect.  This is for forward compatibility to a
        later thread-safe implementation."""
        pass # self._lock.acquire()
    def __exit__(self,etype,evalue,traceback):
        """Has no effect.  This is for forward compatibility to a
        later thread-safe implementation."""
        pass # self._lock.release()
    def has_name(self,name):
        """Returns True if an operation by the given name exists."""
        with self:
            return name in self._gens
    def reset(self):
        """Erases all cached results."""
        with self:
            self._data=None
    def copy(self):
        """Returns a copy of self with no cached results."""
        with self:
            return RegribMany(_copy=self)
    def GRIB(self,name):
        """Returns a GRIB1Fetcher object that will grab the GRIB1 data
        with the given name."""
        self._gens[name]
        return GRIB1Fetcher(name)
    def nonGRIBOps(self):
        """Iterates over all non-GRIBOp operations yielding tuples
        containing the name and operation."""
        for k,v in self._gens.iteritems():
            if not isinstance(v,GRIBOp):
                yield k,v
    def GRIBOps(self):
        """Iterates over all GRIBOp operations yielding tuples
        containing the name and operation."""
        for k,v in self._gens.iteritems():
            if isinstance(v,GRIBOp):
                yield k,v
    def is_grid(self,name): 
        """Returns True if the name corresponds to a grid""" 
        return isinstance(self._gens.get(name,None),GRIBGridBase) 
    def grid(self,name):
        """Returns a GRIBGridFetcher object that will grab the grid
        structure with the given name."""
        self._gens[name]
        return GRIBGridFetcher(name)
    def subset(self,name):
        """Returns a GRIBSubsetterFetcher object that will grab the
        GRIB subsetter with the given name."""
        self._gens[name]
        return GRIBSubsetterFetcher(name)
    def add(self,name,arg,**kwargs):
        """Adds the given generator "arg" under the specified name (in
        "name") to this object."""
        with self:
            assert name not in self._gens
            self._gens[name]=arg
            self._order.append(name)
            self._kwargs[name]=dict(kwargs)
    def to_com(self,location,name,category=None,prodname=None,keep=True):
        """Requests delivery to the com directory in file "location"
        of operation "name".  The category and prodname are used to
        create the FileProduct object, while keep is sent to the final
        deliver_file operation.  It should be left as True."""
        assert(isinstance(location,basestring))
        assert(isinstance(name,basestring))
        assert(category is None or isinstance(category,basestring))
        assert(prodname is None or isinstance(prodname,basestring))
        assert(isinstance(keep,bool))
        with self:
            if not name in self._gens:
                raise RegribKeyError('%s: no such key'%(name,))
            deldat=['com',name,location,category,prodname,keep]
            self._delivery.append(deldat)
            self._name_deliver[name].append(deldat)
    def to_intercom(self,location,name,category=None,prodname=None,keep=True):
        """Requests delivery to the intercom directory in file
        "location" of operation "name".  The category and prodname are
        used to create the FileProduct object, while keep is sent to
        the final deliver_file operation.  It should be left as True."""
        assert(isinstance(location,basestring))
        assert(isinstance(name,basestring))
        assert(category is None or isinstance(category,basestring))
        assert(prodname is None or isinstance(prodname,basestring))
        assert(isinstance(keep,bool))
        with self:
            if not name in self._gens:
                raise RegribKeyError('%s: no such key'%(name,))
            deldat=['intercom',name,location,category,prodname,keep]
            self._delivery.append(deldat)
            self._name_deliver[name].append(deldat)
    def deliveries(self,name=None):
        """Iterates over all deliveries yielding a six element tuple:
          1. "com" or "intercom"
          2. operation name
          3. location within target directory
          4. requested category or None
          5. requested prodname or None
          6. keep flag from the to_com or to_intercom"""
        for (where,iname,loc,cat,prod,keep) in self._delivery:
            if name is None or name==iname:
                yield where,iname,loc,cat,prod,keep
    def has_deliveries(self):
        """Returns True if there are any requested deliveries and
        False otherwise."""
        with self:
            return len(self._delivery)>0
    def has_data(self,name,**kwargs):
        """Returns True if there is a cached result for the specified
        operation.  The keyword arguments are passed on to the
        get_data routine for the caching object (usually a GRIBTask)."""
        if self._data is None:
            return self.is_ready(name,**kwargs)
        args=dict(self._kwargs[name])
        args.update(kwargs)
        args['regribmany']=self
        with self:
            with self._data:
                return self._data.get_data(name,**args) is not None
    def is_ready(self,name,**kwargs):
        """Determines if the specified operation is ready.  Obtains
        the operation by the specified name, and calls its is_ready
        routine, passing the given keyword arguments to it."""
        with self:
            op=self._gens[name]
        if 'regribmany' in kwargs:
            return op.is_ready(**kwargs)
        else:
            args=dict(kwargs)
            args['regribmany']=self
            return op.is_ready(**args)
    def input_valid(self,name,**kwargs):
        """Determines if it is possible to run the specified operation
        with the given keyword arguments.  Obtains the operation by
        the specified name, and calls its input_valid routine with the
        given keyword arguments."""
        with self:
            op=self._gens[name]
        if 'regribmany' in kwargs:
            return op.input_valid(**kwargs)
        else:
            args=dict(kwargs)
            args['regribmany']=self
            return op.input_valid(**args)
    def make(self,name,**kwargs):
        """Executes the operation previously stored in
        self.add(name,...), passing it the given keyword arguments,
        returning the result.  If a cache is available (usually a
        GRIBTask), stores the result in that cache."""
        args=dict(self._kwargs[name])
        args.update(kwargs)
        args['regribmany']=self
        with self:
            if self._data is not None:
                with self._data:
                    got=self._data.get_data(name,**args)
                    if got is not None: return got
            op=self._gens[name]
        r=Regrib(name,op,workprefix=os.path.join(
                self._workprefix,name+'.step'),_base=self)
        result=r.make(**args)
        if not (isinstance(result,GRIBBase) or isinstance(op,GRIBGrid)):
            raise InvalidRegribResult(
                'Invalid result for %s: type=%s repr=%s which came from '
                'running r.make(**args) on an r=%s and args=%s'%
                (str(name),type(result).__name__,repr(result),repr(r),
                 repr(args)))
        with self:
            if self._data is not None:
                self._data.set_data(name,result,**args)
        return result
    def names(self):
        """Iterates over the names provided to self.add(name,...) in
        the order they were added."""
        for name in self._order:
            yield name

########################################################################

# These functions are a mediation layer that sit between the
# produtils.gridop low-level functions and the objects in this module.
# Their primary job is to get filenames and other information from a
# Regrib and various GRIBOp objects and send them to the
# produtil.gribop functions.  All return either a GRIB1File or a
# GRIB2File except "checkgrib" which is a bool function that checks to
# see if a GRIB1/2 file can plausably contain data, without opening
# the file..

GRIB_FILE_MIN_SIZE=8
"""Eight bytes: the minimum required size of a non-empty GRIB1 or
GRIB2 file.  This size is chosen because it is not possible for a file
to be smaller than this and still meet the GRIB standard.
Realistically, the file should be much larger."""

def checkgrib(regrib,filename):
    """This is a low-level implementation function.  It should not be
    called directly.  This checks a file to make sure it exists, is a
    regular file (or symlink to a regular file) and is large enough to
    plausibly contain some GRIB1 or GRIB2 data.  Returns True for such
    plausible files and False otherwise.  Does not open the file, and
    performs only a single stat.  Uses errno ENOENT (no such file or
    directory) and ELOOP (too many symbolic links) to detect
    non-existance.  Other exceptions will be passed to the caller."""
    if filename is None:
        if regrib.logger is not None:
            regrib.logger.warning(
                'checkgrib: filename=None.  Probable code error.')
        return False
    try:
        s=os.stat(filename)
        sizeok=s.st_size>GRIB_FILE_MIN_SIZE
        if not sizeok:
            if regrib.logger is not None:
                regrib.logger.warning('%s: size=%d < 8 bytes'
                                      %(str(filename),s.st_size))
            return False
        typeok=stat.S_ISREG(s.st_mode)
        if not typeok:
            if regrib.logger is not None:
                regrib.logger.warning('%s: not a regular file'
                                      %(str(filename),s.st_size))
            return False
    except EnvironmentError as e:
        if e.errno == errno.ENOENT:
            if regrib.logger is not None:
                regrib.logger.warning('%s: input GRIB1/2 file does not exist: %s'%(str(filename),os.strerror(e.errno)))
            return False
        elif e.errno == errno.ELOOP:
            if regrib.logger is not None:
                regrib.logger.warning('%s: input GRIB1/2 file path results in too many symlinks (likely a symbolic link loop): %s'%(str(filename),os.strerror(e.errno)))
            return False
        # We get here on genuine errors
        raise
    return True

def action_grib1to2(op,regrib,infile,*args,**kwargs):
    """This is a low-level implementation function.  It should not be
    called directly.  This function is sent as a parameter to GRIB1Op
    to produce a GRIB2 version of a GRIB1 file.  It uses
    regrib.cnvgrib_g12 to perform the conversion."""
    (outfile,outindex)=regrib.gribtemp('grb2')
    cmd=regrib.cnvgrib_g12[infile.grib1file,outfile]
    if regrib.logger is not None:
        regrib.logger.info(repr(cmd))
    ex=run(cmd)
    if ex!=0:
        raise RegribError('ERROR: regrib.cnvgrib_g12 command failed with '
                          'exit status %d'%(ex,))
    return GRIB2File(outfile,None,None)

def action_grib2to1(op,regrib,infile,*args,**kwargs):
    """This is a low-level implementation function.  It should not be
    called directly.  This function is sent as a parameter to GRIB2Op
    to produce a GRIB1 version of a GRIB2 file.  It uses
    regrib.cnvgrib_g21 to do the conversion."""
    (outfile,outindex)=regrib.gribtemp('grb1')
    produtil.gribop.grib2to1(outfile,infile.grib1file,regrib.cnvgrib_g21,
                             regrib.logger)
    produtil.gribop.grib1wgrib_s(outfile,outindex,regrib.wgrib,regrib.logger)
    return GRIB1File(outfile,outindex,None)

def action_grib1regridmerge(op,regrib,grid,in1,in2,**kwargs):
    """This is a low-level implementation function.  It should not be
    called directly.  This function is sent as a parameter to GRIB1Op
    to do a copygb regrid AND merge in a single operation.  The grid
    argument is the target grid, and the in1 and in2 arguments are the
    input GRIB1 files."""
    grid=grid.grib1grid
    if grid is None:
        raise GridlessError('No GRIB1 grid in input grid object'
                            ' (grid.grib1grid is None).')

    file1=in1.grib1file
    file2=in2.grib1file

    ok1=checkgrib(regrib,file1)
    ok2=checkgrib(regrib,file2)

    if ok1 and ok2:
        (outfile,outindex)=regrib.gribtemp('merge.grb1')
        cmd=regrib.copygb['-xg'+grid,'-M',file1,file2,outfile]
        if regrib.logger is not None:
            regrib.logger.info(repr(cmd))
        ex=run(cmd)
        if ex!=0:
            message='%s + %s: GRIB1 merge failed: exit status %d'%(
                file1,file2,ex)
            if regrib.logger is not None:
                regrib.logger.error(message)
            raise RegribError(message)
        return GRIB1File(outfile,None,grid)
    elif ok1:
        run(exe('ls')['-l'])
        run(exe('pwd'))
        raise GRIBInputError(file2,': failed sanity checks')
        return in1
    elif ok2:
        run(exe('ls')['-l'])
        run(exe('pwd'))
        raise GRIBInputError(file1,': failed sanity checks')
        return in2
    else:
        raise GRIBInputError(
            '%s, %s: both inputs to GRIB1 regribmerge are empty or '
            'missing.'%(file1,file2))

def action_grbindex(op,regrib,ingrib,task=None,**kwargs):
    """This is a low-level implementation function.  It should not be
    called directly.  This function adds a grbindex output to the
    specified input file's product, unless one is already available.
    Unlike most action_* functions, this one does not create a new
    GRIBFile or GRIBProduct, instead it modifies the existing one to
    add the location of the grbindex output."""
    assert(task is not None)
    assert(ingrib is not None)

    infile=ingrib.grib1file
    inindex=ingrib.grib1grbindex
    logger=regrib.logger

    if inindex is None:
        if logger is not None:
            logger.info('%s: make the grbindex'%(infile,))
        inindex=infile+'.grbindex'
        grbindex=alias(exe(task.getexe('grbindex')))
        produtil.run.checkrun(grbindex[infile,inindex],logger=logger)
        ingrib.grib1grbindex=inindex
    elif logger is not None:
        logger.info('%s: already has grbindex %s'%(infile,inindex))
    return ingrib

def action_grib1merge(op,regrib,in1,in2,**kwargs):
    """This is a low-level implementation function.  It should not be
    called directly.  This function is sent as a parameter to GRIB1Op
    to do a copygb merge (-xM) of two GRIB1 files to produce a single
    GRIB1 file.  It requires that at least one of the files have a
    known grid, and if both have a known grid then their grids must
    mach."""
    grid1=in1.grib1grid
    grid2=in2.grib1grid
    if grid1 is None and grid2 is None:
        raise GridlessError(
            '%s: unable to guess the grid for this GRIB merge'
            %(outfile,))
    elif grid1 is None:
        grid=grid2
    elif grid2 is None:
        grid=grid1
    elif grid1!=grid2:
        raise GridMismatchError(
            'input grids to not match in GRIB merge: (%s) != (%s)'
            %(grid1,grid2))
    else:
        grid=grid1

    file1=in1.grib1file
    file2=in2.grib1file

    ok1=checkgrib(regrib,file1)
    ok2=checkgrib(regrib,file2)

    if ok1 and ok2:
        (outfile,outindex)=regrib.gribtemp('merge.grb1')
        cmd=regrib.copygb['-g'+grid,'-xM',file1,file2,outfile]
        if regrib.logger is not None:
            regrib.logger.info(repr(cmd))
        ex=run(cmd)
        if ex!=0:
            message='%s + %s: GRIB1 merge failed: exit status %d'%(
                file1,file2,ex)
            if regrib.logger is not None:
                regrib.logger.error(message)
            raise RegribError(message)
        return GRIB1File(outfile,None,grid)
    elif ok1: 
        run(exe('ls')['-l'])
        run(exe('pwd'))
        raise GRIBInputError(file2,': not ok (empty or missing)') # FIXME: REMOVE
        return in1
    elif ok2: 
        run(exe('ls')['-l'])
        run(exe('pwd'))
        raise GRIBInputError(file1,': not ok (empty or missing)') # FIXME: REMOVE
        return in2
    else:
        raise GRIBInputError('%s, %s: both inputs to GRIB1 merge are empty or missing.'%(file1,file2))

def action_grib1regrid(op,regrib,infile,ingrid,*args,**kwargs):
    """This is a low-level implementation function.  It should not be
    called directly.  This is sent as a parameter to GRIB1Op to
    convert a GRIB1 file from one grid to another.  The output has a
    known grid, so it can be sent into an action_grib1merge."""
    (outfile,outindex) = regrib.gribtemp('to_'+ingrid.gridname)
    grid=ingrid.grib1grid
    if grid is None:
        raise GridlessError(
            'Tried to regrid a GRIB1 file, but the GRIBGrid did not have '
            'GRIB1 grid information.')
    cmd=regrib.copygb['-g'+ingrid.grib1grid,'-x',infile.grib1file,outfile]
    if regrib.logger is not None:
        regrib.logger.info(repr(cmd))
    ex=run(cmd)
    if ex!=0:
        raise RegribError('ERROR: copygb regrid command failed with '
                          'exit status %d'%(ex,))
    checkgrib(regrib,outfile)
    return GRIB1File(outfile,None,ingrid.grib1grid)

def action_grib1subset(op,regrib,infile,subsetter,*args,**kwargs):
    """This is a low-level implementation function.  It should not be
    called directly. It subsets a GRIB1 file infile using the given
    subsetter.  The subsetter should have a list of strings accessible
    via subsetter.grib1sub.  It should also have a name accessible
    through subsetter.subsetname.  The wgrib -s output of infile is
    scanned.  Any line in wgrib -s's output that contains the entirety
    of any line in the subsetter.grib1sub is retained. The subsetting
    is done via wgrib -i -grib (infile) -o (outfile)."""
    (outfile,outindex)=regrib.gribtemp('sub_'+subsetter.subsetname)
    if subsetter.grib1sub is None:
        raise SubsetlessError('%s: cannot create: no GRIB1 subsetter '
                              'for subset %s'%(outfile,subname))
    
    index=infile.wgrib_s(regrib)
    subindex=[line for line in subsetter.grib1sub(index)]
    cmd=regrib.wgrib['-i','-grib',infile.grib1file,'-o',outfile] \
        << '\n'.join(subindex)
    if regrib.logger is not None:
        regrib.logger.info(repr(cmd))
    ex=run(cmd,logger=regrib.logger)
    if ex!=0:
        raise RegribError('ERROR: wgrib subset command failed with '
                          'exit status %d'%(ex,))
    return GRIB1File(outfile,None,infile.grib1grid,
                     infile.nscenter,infile.ewcenter)

def action_clatlon(op,regrib,name,center,nsres,ewres,nssize,ewsize,
                   nscount,ewcount,scan,**kwargs):
    assert (regrib.logger is not None)
    regrib.logger.info('IN CLATLON')
    nscen=float(center.nscenter)
    ewcen=float(center.ewcenter)
    if regrib.logger is not None:
        regrib.logger.info(
            'CLATLON: nsres=%s ewres=%s nssize=%s ewsize=%s nscount=%s '
            'ewcount=%s nscen=%f ewcen=%f'%
            (repr(nsres),repr(ewres),repr(nssize),repr(ewsize),
             repr(nscount),repr(ewcount),nscen,ewcen))
    iewres = int(ewres*1000)
    insres = int(nsres*1000)
    iscan = int(scan)
    inscount = int(nscount)
    iewcount = int(ewcount)
    inlat = int(nscen*1000+1000*nssize/2)
    islat = int(nscen*1000-1000*nssize/2)
    ielon = int(ewcen*1000+1000*ewsize/2)
    iwlon = int(ewcen*1000-1000*ewsize/2)
    if 0!=(int(scan)&128):
        if regrib.logger is not None:
            regrib.logger.info('CLATLON scan=%d, swap EW'%(int(scan),))
        (ielon,iwlon)=(iwlon,ielon)
    if 0!=(int(scan)&64):
        if regrib.logger is not None:
            regrib.logger.info('CLATLON scan=%d, swap NS'%(int(scan),))
        (inlat,islat)=(islat,inlat)
    if 0!=(int(scan)&32):
        if regrib.logger is not None:
            regrib.logger.info('CLATLON scan=%d, swap NS-EW sizes'
                               %(int(scan),))
        (ewcount,nscount)=(nscount,ewcount)
    assert(insres>0)
    assert(iewres>0)
    s = '255 0 %d %d %d %d %d %d %d %d %d 0' % \
        ( inscount, iewcount, inlat, ielon, iscan, islat, 
          iwlon, insres, iewres )
    return GRIBGrid(name,s,None)

########################################################################

class GRIBBase(object):
    """This is the abstract base class for all GRIB files, operators,
    subsetters and grids.  Everything that is representable as an
    Regrib operation is a subclass of GRIBBase.  This class should
    never be instantiated directly."""
    def is_ready(self,*args,**kwargs):
        """Returns True if this object and its subobjects are all
        ready for a call to make, and False otherwise."""
        return True
    def input_valid(self,**kwargs):
        """Returns True if the specified kwargs are valid and False
        otherwise."""
        return True
    def make(self,regrib,**kwargs):
        """Runs the action this object should perform and returns
        another GRIBBase object."""
        return self

class GRIBGridBase(GRIBBase):
    """This abstract base class represents something that is able to
    produce a GRIBGrid object when make() is called."""

class GRIBGrid(GRIBGridBase):
    """This class represents a GRIB1 or GRIB2 grid specification."""
    def __init__(self,gridname,grib1grid=None,grib2grid=None):
        """Creates a GRIBGrid with a given name, a copygb -g style
        GRIB1 grid specification and a GRIB2 grid specification that
        is currently unused.  The gridname is mandatory and will be
        used to generate temporary filenames."""
        super(GRIBGrid,self).__init__()
        self._gridname=gridname
        self._grib1grid=grib1grid
        self._grib2grid=grib2grid
    def is_ready(self,*args,**kwargs): 
        """Returns True.  The grid information is always ready."""
        return True
    def input_valid(self,**kwargs): 
        """Returns True.  The grid information is always available,
        regardless of the kwargs."""
        return True
    def make(self,*args,**kwargs): 
        """Returns self.  This grid is the output grid of this grid."""
        return self
    def getgridname(self): 
        """Returns the name of this grid, as provided to the
        constructor."""
        return self._gridname
    def getgrib1grid(self):
        """Returns the GRIB1 (wgrib) style grid information, as sent
        to the constructor's grib1grid argument."""
        return self._grib1grid
    def getgrib2grid(self): 
        """This is a placeholder to return the GRIB2-style grid
        information.  At present, it returns whatever was sent to the
        constructor's grib2grid argument"""
        return self._grib2grid
    gridname=property(getgridname,None,None,
        """The name of this grid.  This is used to generate output filenames when regridding.""")
    grib1grid=property(getgrib1grid,None,None,
        """A string to send to copygb's -g parameter for interpolating to this grid.""")
    grib2grid=property(getgrib2grid,None,None,
       """GRIB2 regridding information, currently unused.""")

quarterDegree=GRIBGrid('Quarter_Degree_Grid_193','193',None)

class GRIBGridFetcher(GRIBGridBase):
    """This class represents a GRIBGrid that is stored in a RegribMany
    object.  It is able to check for readiness via RegribMany.is_ready
    and produce a result via RegribMany.make."""
    def __init__(self,name):
        self._name=name
    def is_ready(self,*args,**kwargs):
        """Calls kwargs['regribmany'].has_data to see if the specified
        operation has produced its grid."""
        source=kwargs['regribmany']
        assert(source)
        return source.has_data(self._name,*args,**kwargs)
    def input_valid(self,**kwargs):
        """Calls kwargs['regribmany'].input_valid to see if the
        specified kwargs make a valid input to the operation."""
        source=kwargs['regribmany']
        assert(source)
        return source.input_valid(self._name,**kwargs)
    def make(self,*args,**kwargs):
        """Calls kwargs['regribmany'].make to produce the grid."""
        source=kwargs['regribmany']
        assert(source)
        return source.make(self._name,**kwargs)

class GRIBGridCompute(GRIBGridBase):
    """This class represents a GRIBGrid that must be computed from
    some other input, usually a GRIB file.  This is used to create
    storm-centered or E-grid GRIB-centered grids for the HWRF."""
    def __init__(self,name,action,*args):
        """Creates a new GRIBGridCompute with the given name and
        action.  The action is a function or callable object that
        takes this object, the regrib, the name and the contents of
        args.  Also, any keyword arguments given to self.make are
        passed on as well."""
        self._name=name
        self._action=action
        self._args=list(args)
    def is_ready(self,*args,**kwargs):
        """Returns the logical "or" of is_ready(*args,**kwargs) called
        on all subobjects.  Subobjects that are not instances of
        GRIBBase are assumed to be ready."""
        for arg in self._args:
            if isinstance(arg,GRIBBase) and not arg.is_ready(*args,**kwargs):
                return False
        return True
    def input_valid(self,*args,**kwargs):
        """Returns the logical "or" of input_valid(**kwargs) called on
        all subobjects.  Subobjects that are not instances of GRIBBase
        are assumed to be ready."""
        for arg in self._args:
            if isinstance(arg,GRIBBase) and not arg.input_valid(**kwargs):
                return False
        return True
    def make(self,regrib,**kwargs):
        """Runs the action specified in the constructor, providing as
        arguments regrib, and the result of running make on all of the
        other arguments sent to the constructor."""
        args=[]
        for arg in self._args:
            if isinstance(arg,GRIBBase):
                args.append(arg.make(regrib,**kwargs))
            else:
                args.append(arg)
        return self._action(self,regrib,self._name,*args,**kwargs)

class FixedLocation(GRIBBase):
    """Represents a specific location on the earth as a latitude,
    longitude pair."""
    def __init__(self,lat,lon):
        self.__lat=float(lat)
        self.__lon=float(lon)
    def getlat(self): 
        """Returns the point's latitude"""
        return self.__lat
    def getlon(self): 
        """Returns the point's longitude."""
        return self.__lon
    def setlat(self,val): 
        """Sets the point's latitude.""" 
        self.__lat=val
    def setlon(self,val): 
        """Sets the point's longitude."""
        self.__lon=val
    nscenter=property(getlat,setlat,None,
                      """The latitude of this fixed location.""")
    ewcenter=property(getlon,setlon,None,
                      """The longitude of this fixed location.""")

def clatlon(center,res,size,n,scan=136,name=None):
    """Returns a GRIBGridCompute that will produce a lat-lon grid
    centered on the given object.  The provided object must have
    nscenter and ewcenter properties that produce real lats and lons
    that are positive North and positive East.  The res parameter is a
    two element array containing the resolution in degrees.  The size
    parameter is a two-element array containing the extent in degrees.
    The n parameter contains the number of gridpoints.  All three,
    res, size and n, are two element arrays with the N-S value first
    and the E-W value second.  The "scan" parameter is the scanning
    mode parameter: 128 means E-W are reversed, 64 means N-S are
    reversed, and 32 means the data is transposed."""
    assert(len(n)==2 and n[0]>0 and n[1]>0)
    assert(len(size)==2 and size[0]>0 and size[1]>0)
    assert(scan>=0 and scan<=255)
    assert(len(res)==2 and res[0]>0 and res[1]>0)
    if name is None:
        name='clatlon%dx%dp'%(int(n[0]),int(n[1]))
    return GRIBGridCompute(name,action_clatlon,center,res[0],res[1],size[0],
                           size[1],n[0],n[1],scan)

class GRIBSubsetter(GRIBBase):
    def __init__(self,subsetname,grib1sub=None,grib2sub=None):
        """Creates a GRIBSubsetter with a given name, a callable
        object grib1sub that will be sent to
        produtil.gribop.grib1subset, and an unused grib2sub that will
        be used for GRIB2 subsetting one day.  The gridname is
        mandatory and will be used to generate temporary filenames."""
        super(GRIBSubsetter,self).__init__()
        self._subsetname=subsetname
        self._grib1sub=grib1sub
        self._grib2sub=grib2sub
    def is_ready(self,*args,**kwargs): 
        """Returns True.  The subsetter is its own product, so it is
        always "ready." """
        return True
    def input_valid(self,**kwargs):
        """Returns True.  The subsetter's product (itself) does not
        vary with the kwargs, so any input is valid."""
        return True
    def make(self,*args,**kwargs): 
        """Returns self.  The subsetter is its own product."""
        return self
    def getsubsetname(self):
        """Returns a name of this subset for use in filenames."""
        return self._subsetname
    def getgrib1sub(self): 
        """Returns an iterator that takes as its only argument an
        array of lines of wgrib -s output."""
        return self._grib1sub
    def getgrib2sub(self): 
        """This is a placeholder for future GRIB2 subsetting
        support."""
        return self._grib2sub
    subsetname=property(getsubsetname,None,None,
       """The name of this GRIB1/2 subset.  This is used to generate 
       output filenames.""")
    grib1sub=property(getgrib1sub,None,None,
       """A callable object to send to produtil.gribop.grib1subset's
       subsetter argument.""")
    grib2sub=property(getgrib2sub,None,None,
       """GRIB2 subsetting information, currently unused.""")

class GRIBSubsetterFetcher(GRIBSubsetter):
    """This class grabs a GRIBSubsetter from a RegribMany object."""
    def __init__(self,name):
        self._name=name
    def input_valid(self,**kwargs):
        """Calls kwargs['regribmany'].input_valid to see if the kwargs
        make a valid input."""
        source=kwargs['regribmany']
        assert(source)
        return source.input_valid(self._name,**kwargs)
    def is_ready(self,*args,**kwargs):
        """Calls kwargs['regribmany'].has_data to see if the operation
        has produced its subsetter yet."""
        source=kwargs['regribmany']
        assert(source)
        return source.has_data(self._name,*args,**kwargs)
    def make(self,*args,**kwargs):
        """Calls kwargs['regribmany'].make to produce the subsetter."""
        source=kwargs['regribmany']
        assert(source)
        return source.make(self._name,**kwargs)

########################################################################

class GRIBOp(GRIBBase):
    """This is the abstract base class for all GRIB1 and GRIB2 files
    and operators.  It should never be instantiated directly."""
    def __init__(self,action,*args):
        """Creates a GRIBOp that has a number of child GRIBBase
        objects, with a specified action to perform in the GRIBOp.make
        method.  The action should be a callable object that takes as
        input a Regrib, the contents of args and any keyword arguments
        sent to make."""
        self._action=action
        self._args=list(args)
    def getaction(self): 
        """Returns the action, a function or callable object."""
        return self._action
    def args(self): 
        """Iterates over all child GRIBBase objects."""
        for arg in self._args:
            yield arg
    action=property(getaction,None,None,
        """Returns the callable object that performs this object's work""")
    def is_ready(self,*args,**kwargs):
        """Returns the logical "and" of is_ready(*args,**kwargs)
        called on all subobjects."""
        for arg in self.args():
            if not arg.is_ready(*args,**kwargs):
                return False
        return True
    def input_valid(self,**kwargs):
        """Returns the logical "and" of input_valid(**kwargs) called
        on all subobjects."""
        for arg in self.args():
            if not arg.input_valid(**kwargs):
                return False
        return True
    def make(self,regrib,**kwargs):
        """Runs the action specified in the constructor, providing as
        arguments regrib, and the result of running make on all of the
        other arguments sent to the constructor."""
        args=[]
        for arg in self.args():
            result=arg.make(regrib,**kwargs)
            assert(result is not None)
            assert(isinstance(result,GRIBBase))
            args.append(result)
        return self._action(self,regrib,*args,**kwargs)
    def __repr__(self):
        return '<%s %s %s>'%(type(self).__name__,repr(self._action),
                             ','.join([repr(x) for x in self.args()]))

class GRIB2Op(GRIBOp):
    """This subclass of GRIBOp produces GRIB2 files, and can be
    converted to GRIB1 via obj*GRIB1 or obj.to_grib1().  Calls to
    obj.to_grib2() or obj*GRIB2 simply return self."""
    def __init__(self,action,*args):
        super(GRIB2Op,self).__init__(action,*args)
    def to_grib1(self):
        return GRIB2Op(action_grib2to1,self)
    def to_grib2(self):
        return self
    def __mul__(self,grid):
        if grid is GRIB1:
            return self.to_grib1()
        elif grid is GRIB2:
            return self
        return NotImplemented

class GRIB1Op(GRIBOp):
    """This subclass of GRIBOp produces GRIB1 files.  It introduces a
    noumber of convenience calls:

      file1 + file2 + file3 = a copygb merge of the three files
      file1*grid = copygb interpolate file1 to the specified GRIBGrid
      file1*GRIB2 = convert to GRIB2
      file1*GRIB1 = no-op (returns self)
      file1/subsetter = subsets the GRIB1 file using the given GRIBSubsetter

    These can be combined, of course, and follow the usual Python
    order of precedence:

      (file1*grid + file2*grid + file3/subsetter*grid)*GRIB1

    As with the GRIB2Op, there are also to_grib1() and to_grib2()
    methods which return self, and a GRIB2 conversion of self,
    respectively."""
    def __init__(self,action,*args):
        """Creates a new GRIB1Op, passing all arguments to the
        superclass constructor."""
        super(GRIB1Op,self).__init__(action,*args)
    def to_grib1(self): 
        """Returns self.  The GRIB1Op already produces a GRIB1 file."""
        return self
    def to_grib2(self):
        """Returns a GRIB2Op that will make a GRIB2 version of this
        operation's output."""
        return GRIB2Op(action_grib1to2,self)
    def regridmerge(self,grid,other):
        """Returns a GRIB1Op that will ask copygb to merge the other
        grid on top of this one."""
        if not isinstance(grid,GRIBGridBase):
            raise TypeError(
                'First argument to GRIB1Op.regridmerge must be a '
                'GRIBGridBase subclass, not a %s.'
                %(type(grid).__name__,))
        return GRIB1Op(action_grib1regridmerge,grid,self,other.to_grib1())
    def make_grbindex(self):
        """Returns a GRIB1Op that will run grbindex and save the
        results, unless that has already been done."""
        return GRIB1Op(action_grbindex,self)
    def __div__(self,subsetter):
        """Returns a GRIB1Op that will subset this one using the
        specified subsetter."""
        if isinstance(subsetter,GRIBSubsetter):
            return GRIB1Op(action_grib1subset,self,subsetter)
        return NotImplemented
    def __mul__(self,grid):
        """If the grid is a GRIBGridBase, returns a GRIB1Op that will
        regrid this GRIB1Op's output to that grid.  If grid is the
        special constant GRIB2, returns self.to_grib2.  If it is the
        constant GRIB1, returns self.to_grib1.  Otherwise, returns
        NotImplemented."""
        if isinstance(grid,GRIBGridBase):
            return GRIB1Op(action_grib1regrid,self,grid)
        elif grid is GRIB1:
            return self.to_grib1()
        elif grid is GRIB2:
            return self.to_grib2()
        return NotImplemented
    def __add__(self,other):
        """If the argument is a GRIBOp, returns a GRIB1Op that will
        paste a GRIB1 version of that operations output onto this
        operation's output.  For any other argument type, returns
        NotImplemented."""
        if isinstance(other,GRIBOp):
            return GRIB1Op(action_grib1merge,self,other.to_grib1())
        return NotImplemented

########################################################################

class GRIB1Fetcher(GRIB1Op):
    """The GRIB1Fetcher is a GRIB1Op subclass that fetches a GRIB1
    file from a RegribMany."""
    def __init__(self,name):
        """Creates a GRIB1Fetcher for the specified operation name."""
        self._name=name
    def input_valid(self,**kwargs):
        """Calls kwargs['regribmany'].input_valid to determine if the
        specified keyword arguments make a valid input to the
        operation."""
        source=kwargs['regribmany']
        assert(source)
        return source.input_valid(self._name,**kwargs)
    def is_ready(self,*args,**kwargs):
        """Calls kwargs['regribmany'].has_data to see if there is
        cached output from the operation."""
        source=kwargs['regribmany']
        assert(source)
        return source.has_data(self._name,*args,**kwargs)
    def make(self,*args,**kwargs):
        """Calls kwargs['regribmany'].make to produce the operation's
        output."""
        source=kwargs['regribmany']
        assert(source)
        return source.make(self._name,**kwargs)

class GRIB1File(GRIB1Op):
    """This subclass of GRIB1Op represents a GRIB1 file on disk that
    is ALREADY PRESENT.  Its is_ready function is overridden to assume
    the file is always ready, and its "make" method simply returns
    self."""
    def __init__(self,grib1file,grib1index,grib1grid,
                 nscenter=None,ewcenter=None,grib1grbindex=None):
        super(GRIB1File,self).__init__(None)
        self._grib1file=grib1file
        self._grib1index=grib1index
        self._grib1grid=grib1grid
        self._grib1grbindex=grib1grbindex
        if not (ewcenter is None or float(ewcenter) is not None):
            raise TypeError('ewcenter must be None, or an int or float, not a %s (ewcenter=%s)'
                            %(ewcenter.__class__.__name__,repr(ewcenter)))
        if not (nscenter is None or float(nscenter) is not None):
            raise TypeError('nscenter must be None, or an int or float, not a %s (nscenter=%s)'
                            %(nscenter.__class__.__name__,repr(nscenter)))
        self._ewcenter=ewcenter
        self._nscenter=nscenter
        self._indexdata=None
    def input_valid(self,**kwargs):
        """Returns True.  The file is already present, so any kwargs
        make a valid input."""
        return True
    def is_ready(self,*args,**kwargs):
        """Returns True.  The operation is ready to run because it has
        nothing to do: the file is already present."""
        return True
    def make(self,*args,**kwargs):
        """Returns self.  This class represents a GRIB1 file, so it is
        its own output."""
        return self
    def getnscenter(self):
        """Returns the domain center latitude."""
        return self._nscenter
    def getewcenter(self): 
        """Returns the domain center longitude."""
        return self._ewcenter
    def getgrib1file(self): 
        """Returns the GRIB1 file's location."""
        return self._grib1file
    def getgrib1index(self): 
        """Returns the location of the file that stores the output of
        wgrib -s."""
        return self._grib1index
    def getgrib1grbindex(self):
        """Returns the location of the file that stores the output of
        the grbindex program run on the GRIB1 file."""
        return self._grib1grbindex
    def setgrib1grbindex(self,here):
        """Sets the location of the file that stores the output of
        the grbindex program run on the GRIB1 file."""
        self._grib1grbindex=here
    def getgrib1grid(self): 
        """Returns the GRIB1 grid information as a string."""
        return self._grib1grid
    def wgrib_s(self,regrib):
        """Returns the output of wgrib -s run on this file, which may
        be from cached data."""
        if self._indexdata is not None:
            return self._indexdata
        indexfile=self.grib1index
        if indexfile is None or not os.path.exists(indexfile):
            indexfile=regrib.indextemp(self.grib1file)
            index=runstr(regrib.wgrib['-s',self.grib1file])
            with open(indexfile,'wt') as f:
                f.write(index)
        else:
            with open(indexfile,'rt') as f:
                index=f.readall()
        self._indexdata=index.splitlines()
        return self._indexdata
    resultfile=property(getgrib1file,None,None,
        'The path to the result file, in this case the GRIB1 file.')
    grib1file=property(getgrib1file,None,None,
        'The path to the GRIB1 file which is assumed to already exist.')
    grib1index=property(getgrib1index,None,None,
        'Either None or the wgrib -s index file for the GRIB1 file.')
    grib1grbindex=property(getgrib1grbindex,setgrib1grbindex,None,
        'Either None or the grbindex binary index file for the GRIB1 file.')
    grib1grid=property(getgrib1grid,None,None,
        "Either None or the copygb -g argument data for the GRIB1 file's grid")
    nscenter=property(getnscenter,None,None,
        'Returns None or the center latitude of this GRIB1 file.')
    ewcenter=property(getewcenter,None,None,
        'Returns None or the center longitude of this GRIB1 file.')

class GRIB2File(GRIB2Op):
    """This subclass of GRIB2Op represents a GRIB2 file on disk that
    is ALREADY PRESENT.  Its is_ready function is overridden to assume
    the file is always ready, and its "make" method simply returns
    self."""
    def __init__(self,grib2file,grib2index,grib2grid):
        super(GRIB2File,self).__init__(None)
        self._grib2file=grib2file
        self._grib2index=grib2index
        self._grib2grid=grib2grid
    def is_ready(self,*args,**kwargs):
        """Returns True.  The operation is ready to run because it has
        nothing to do: the file is already present."""
        return True
    def input_valid(self,**kwargs):         
        """Returns True.  The file is already present, so any kwargs
        make a valid input."""
        return True
    def make(self,*args,**kwargs):         
        """Returns self.  This class represents a GRIB2 file, so it is
        its own output."""
        return self
    def getgrib2file(self): 
        """Returns the GRIB2 file's location."""
        return self._grib2file
    def getgrib2index(self):
        """This is a placeholder for future development.  It returns
        the grib2index argument from the constructor."""
        return self._grib2index
    def getgrib2grid(self): 
        """This is a placeholder for future development.  It returns
        the grib2grid argument from the constructor."""
        return self._grib2grid
    resultfile=property(getgrib2file,None,None,
        'The path to the result file, in this case the GRIB2 file.')
    grib2file=property(getgrib2file,None,None,
        'The path to the GRIB2 file which is assumed to already exist.')
    grib2index=property(getgrib2index,None,None,
        'This is a placeholder for future GRIB2 subsetting support')
    grib2grid=property(getgrib2grid,None,None,
        "This is a placeholder for future GRIB2 regridding support")

class UpstreamGRIB1(UpstreamFile,GRIB1Op):
    """This subclass of GRIB1Op and UpstreamFile represents a GRIB1
    file that is produced by an upstream workflow.  The is_ready
    subroutine calls self.check to check the modification time and
    file size.  The make subroutine copies the file to a local
    location."""
    def __init__(self,dstore,*args,**kwargs):
        UpstreamFile.__init__(self,dstore,*args,**kwargs)
        GRIB1Op.__init__(self,None)
    def is_ready(self,*args,**kwargs):
        """Runs the check function on the upstream product, and
        returns its results.  If the frominfo argument is in kwargs,
        it is passed as the first positional parameter to check."""
        if 'frominfo' in kwargs:
            return self.check(kwargs['frominfo'])
        else:
            return self.check()
    def input_valid(self,**kwargs):
        """Returns True."""
        return True
    def make(self,regrib,*args,**kwargs):
        """Calls deliver_file to copy the file to a temporary location
        decided by regrib.gribtemp.  Returns a GRIB1File for that
        file."""
        loc=self.location
        (filename,index)=regrib.gribtemp('upstream.'+os.path.basename(loc))
        produtil.fileop.deliver_file(loc,filename,logger=regrib.logger)
        return GRIB1File(filename,None,None)
    resultfile=property(UpstreamFile.getlocation,None,None,
        'The path to the result file, in this case self.location.')

class GRIB1Selector(GRIB1Op):
    """This object produces a GRIB1Op from a call to a Task object's
    Task.product.  This is the class that underlies the implementation
    of igrb1."""
    def __init__(self,task,**kwargs):
        """Creates a GRIB1Selector for the specified task.  The kwargs
        will be passed to task.products."""
        self._task=task
        self._kwargs=dict(kwargs)
    def is_ready(self,**kwargs):
        """Returns True if the task's product is available."""
        product=self.select(kwargs)
        return product.is_available()
    def input_valid(self,**kwargs):
        """Returns True if self.select is able to find a product and
        False if NoProductError is raised.  Passes the kwargs to
        self.select."""
        try:
            product=self.select(kwargs)
            return True
        except NoProductError:
            return False
    def make(self,regrib,**kwargs):
        """Calls self.select(kwargs) to get the product, and calls
        make on the result."""
        product=self.select(kwargs)
        if not isinstance(product,GRIB1Op):
            raise GRIBInputError('Selected product is not a GRIB product.')
        return product.make(regrib,**kwargs)
    def select(self,kwargs):
        """Attempts to attain a Product from the supplied Task.  The
        kwargs is a dict of keyword arguments to pass to
        task.products.  Those will override any keyword arguments that
        were sent to the constructor.  If more than one Product is
        yielded, then ProductAmbiguityError is raised.  If no Product
        is yielded, NoProductError is raised.  If the returned Product
        is not a GRIBBase, TypeError is raised.  Otherwise, the result
        is returned."""
        arg=dict(self._kwargs)
        arg.update(kwargs) 
        products=[p for p in self._task.products(**arg)]
        if len(products)>1:
            raise ProductAmbiguityError('Specified arguments produced more than one product.')
        elif len(products)==0:
            raise NoProductError('Specified arguments produced no product.')
        else:
            p=products[0]
            if not isinstance(p,GRIBBase):
                raise TypeError(
                    'ERROR: in GRIB1Selector, a Task.products call '
                    'returned something that was not a GRIBBase object.  '
                    'It returned a %s %s.'%(type(p).__name__,repr(p)))
            return p
    def __repr__(self):
        return '%s(%s)'%(self.__class__.__name__,self._task.__class__.__name__)

def igrb1(task,**kwargs):
    """This is a convenient alias for the GRIB1Selector constructor."""
    return GRIB1Selector(task,**kwargs)

########################################################################

class GRIB2Product(FileProduct,GRIB2File):
    """This class represents a GRIB2 file that is produced by this
    workflow.  It is a direct subclass of both GRIB2File and
    FileProduct."""
    def __init__(self,dstore,*args,**kwargs):
        """Creates a new GRIB2Product.  The dstore is the
        produtil.datastore.Datastore.  All other arguments are passed
        on to FileProduct's constructor.  The GRIB2File constructor is
        initialized with None for the file, index and grid."""
        assert('location' in kwargs and kwargs['location']!='' )
        FileProduct.__init__(self,dstore,*args,**kwargs)
        GRIB2File.__init__(self,None,None,None)
        self._indexdata=None
        assert(self.location is not None and self.location!='')
    def input_valid(self,**kwargs):
        """Returns True.  Since this object is already its product,
        any kwargs make a valid input."""
        return True
    def is_ready(self,*args,**kwargs):
        """Calls self.check to see if the product is available.  If
        frominfo is in the kwargs, it is passed as the first
        positional argument to check."""
        if 'frominfo' in kwargs:
            return self.check(kwrargs['frominfo'])
        else:
            return self.check()
    def make(self,regrib,*args,**kwargs):
        """Calls deliver_file to copy the file to a new temporary
        location from regrib.gribtemp.  Returns a GRIB2File for that
        temporary file."""
        loc=self.location
        (filename,index)=regrib.gribtemp('prod.'+os.path.basename(loc))
        produtil.fileop.deliver_file(loc,filename,logger=regrib.logger)
        return GRIB2File(filename,None,None)
    def setgrib2grid(self,grid):
        """Sets the grib2 grid information."""
        if  grid is None or  grid=='' or grid=='None':  return
        self['grib2grid']=grid
    def getgrib2grid(self):        
        """Returns the grib2 grid information."""
        return self.get('grib2grid',None)
    def delgrib2grid(self):        
        """Clears the grib2 grid information."""
        del self['grib2grid']
    grib2grid=property(getgrib2grid,setgrib2grid,delgrib2grid,
                       """The GRIB2 grid information if known.""")

    def setgrib2index(self,index):
        """Sets the grib2 index information."""
        if index is None or index=='' or index=='None': return
        self['grib2index']=index
    def getgrib2index(self):       
        """Gets the grib2 index information, or returns None if unknown."""
        return self.get('grib2index',None)
    def delgrib2index(self):
        """Cleares the stored information about the index location.
        Does NOT delete the index file."""
        del self['grib2index']
    grib2index=property(getgrib2index,setgrib2index,delgrib2index,
        """The disk location of the GRIB2 index file.""")

    def getgrib2file(self):   
        """Returns the location of the GRIB2 file from self.location."""
        return self.location
    def setgrib2file(self,val): 
        """Sets the location of the GRIB2 file by setting self.location."""
        self.location=val
    grib2file=property(getgrib2file,setgrib2file,None,
                       """Synonym for self.location""")
    resultfile=property(getgrib2file,setgrib2file,None,
                       """Synonym for self.location""")

    def deliver(self,location=None,index=None,grbindex=None,frominfo=None,
                keep=True,logger=None):
        """Delivers a GRIB2 file to its destination.  May also deliver
        other files associated with that GRIB2 file:
          """
        if isinstance(frominfo,basestring):
            # Delivering from a specified file.  Deliver as a FileProduct:
            FileProduct.deliver(self,location=location,frominfo=frominfo,
                                keep=keep)
            return
        elif not isinstance(frominfo,GRIB2Op):
            raise TypeError(
                'GRIB2Product.deliver requires a frominfo that is either '
                'a string filename or a GRIB2Op.')

        # Get from/to information in a transaction for speed:
        with self.dstore.transaction():
            loc=location if(location is not None) else self.location
            if loc is None:
                raise produtil.datastore.UnknownLocation(
                    '%s: no location known when delivering product.  '
                    'Specify a location to deliver().'%(self.did))
            (fromindex,toindex)=(None,None)
            try:
                fromindex=frominfo.grib2index
            except AttributeError, KeyError: pass 
            try:
                fromloc=frominfo.grib2file
            except AttributeError, KeyError:
                fromloc=frominfo.resultfile
        toindex=str(index) if (index is not None) else '%s.wgrib_s'%(loc,)

        # Deliver the GRIB2 file and the text and binary index files:
        assert(fromloc is not None and fromloc!='')
        assert(loc is not None and loc!='')
        produtil.fileop.deliver_file(fromloc,loc,keep=keep)

        haveindex=False
        if fromindex is not None and toindex is not None and \
                produtil.fileop.isnonempty(fromindex):
            produtil.fileop.deliver_file(fromindex,toindex,keep=keep)
            haveindex=True

        # Store grid, index location, etc. in a transaction for speed
        # and also to ensure all records are updated or none are:
        with self.dstore.transaction():
            self.location=loc
            if haveindex: self.grib2index=toindex
            try:
                self.grib2grid=frominfo.grib2grid
            except AttributeError,KeyError: pass
            self.available=True
        self.call_callbacks(logger=logger)

    def undeliver(self):
        """Deletes the delivered GRIB2 file and discards the index and
        grid information."""
        loc=self.location
        if loc is not None and loc!='':
            produtil.fileop.remove_file(loc)
        index=self.grib2index
        with self.dstore.transaction():
            del self.grib2index
            del self.grib2grid
            self.available=False

        
########################################################################
class GRIB1Product(FileProduct,GRIB1File):
    """This class represents a GRIB1 file produced by this workflow.
    It stores grid information, if available, and will also deliver an
    index file if one exists.  It is a direct subclass of both
    FileProduct and GRIB1File."""
    def __init__(self,dstore,*args,**kwargs):
        """Creates a new GRIB1Product.  The dstore is the
        produtil.datastore.Datastore.  The kwargs must specify the
        file location as a non-empty string.  All arguments are passed
        to the FileProduct constructor.  The GRIB1File is initialized
        with the various indices and grid missing (None)."""
        assert('location' in kwargs and kwargs['location']!='' )
        FileProduct.__init__(self,dstore,*args,**kwargs)
        GRIB1File.__init__(self,None,None,None)
        self._indexdata=None
        assert(self.location is not None and self.location!='')
    def input_valid(self,**kwargs):
        """Returns True.  This object is its own product, so any
        kwargs are valid."""
        return True
    def is_ready(self,*args,**kwargs):
        """Runs self.check and returns the result.  If frominfo is in
        kwargs, it is sent as the first positional argument to check."""
        if 'frominfo' in kwargs:
            return self.check(kwargs['frominfo'])
        else:
            return self.check()
    def make(self,regrib,*args,**kwargs):
        """Gets a temporary filename from gribtemp, and copies the
        GRIB1 file there.  Returns a new GRIB1File object for that
        file."""
        loc=self.location
        (filename,index)=regrib.gribtemp('prod.'+os.path.basename(loc))
        produtil.fileop.deliver_file(loc,filename,logger=regrib.logger)
        return GRIB1File(filename,None,None)

    def setgrib1grid(self,grid):
        """Sets the GRIB1 grid information; modifies the grib1grid
        metadata in the datastore."""
        self['grib1grid']=grid
    def getgrib1grid(self):        
        """Returns the GRIB1 grid information from the grib1grid
        metadata in the datastore."""
        return self.get('grib1grid',None)
    def delgrib1grid(self):        
        """Discards GRIB1 grid information by removing the grib1grid
        metadata from the datastore."""
        del self['grib1grid']
    grib1grid=property(getgrib1grid,setgrib1grid,delgrib1grid,
                       """The GRIB1 grid (GDS) information if known.""")

    def setgrib1grbindex(self,index): 
        """Sets the output location of grbindex to the given location,
        and sets the grib1grbindex metadata value."""
        self['grib1grbindex']=index
    def getgrib1grbindex(self):       
        """Returns the output location of grbindex from the
        grib1grbindex metadata value."""
        return self.get('grib1grbindex',None)
    def delgrib1grbindex(self):
        """Cleares the stored information about the grbindex binary
        index file location for this GRIB1 product by deleting the
        grib1grbindex metadata value.  Does NOT delete the grbindex
        file."""
        del self['grib1grbindex']
    grib1grbindex=property(getgrib1grbindex,setgrib1grbindex,delgrib1grbindex,
        """The disk location of the GRIB1 grbindex binary index file.""")

    def setgrib1index(self,index): 
        """Sets the output location of wgrib -s and sets the
        grib1index metadata value."""
        self['grib1index']=index
    def getgrib1index(self):       
        """Returns the output location of wgrib -s from the grib1index
        metadata value."""
        return self.get('grib1index',None)
    def delgrib1index(self):
        """Cleares the stored information about the index location.
        Does NOT delete the index file."""
        del self['grib1index']
    grib1index=property(getgrib1index,setgrib1index,delgrib1index,
        """The disk location of the GRIB1 index file, from wgrib -s.""")

    def getgrib1file(self):     
        """Returns the GRIB1 file location from self.location."""
        return self.location
    def setgrib1file(self,val): 
        """Sets the GRIB1 file location.  Same as setting self.location."""
        self.location=val
    grib1file=property(getgrib1file,setgrib1file,None,
                       """Synonym for self.location""")
    resultfile=property(getgrib1file,setgrib1file,None,
                       """Synonym for self.location""")

    def deliver(self,location=None,index=None,grbindex=None,
                frominfo=None,keep=True,logger=None):
        """Delivers the GRIB1 data, and possibly the various index
        files as well.  If frominfo is supplied, then that file is
        delivered, and no other action is performed.  Otherwise, the
        file is delivered from self.location to the supplied location,
        the grib1index is delivered to the same location with an added
        ".wgrib_s" extension and the grib1grbindex is delivered with a
        ".grbindex" extension."""
        if isinstance(frominfo,basestring):
            # Delivering from a specified file.  Deliver as a FileProduct:
            FileProduct.deliver(
                self,location=location,frominfo=frominfo,keep=keep)
            return
        elif not isinstance(frominfo,GRIB1Op):
            raise TypeError('GRIB1Product.deliver requires a frominfo '
                            'that is either a string filename or a GRIB1Op.')

        # Get from/to information in a transaction for speed:
        with self.dstore.transaction():
            loc=location if(location is not None) else self.location
            if loc is None:
                raise produtil.datastore.UnknownLocation(
                    '%s: no location known when delivering '
                    'product.  Specify a location to deliver().'
                    %(self.did))
            (fromindex,toindex)=(None,None)
            try:
                fromindex=frominfo.grib1index
            except AttributeError, KeyError: pass 
            try:
                fromloc=frominfo.grib1file
            except AttributeError, KeyError:
                fromloc=frominfo.resultfile
            try:
                fromgrbindex=frominfo.grib1grbindex
            except AttributeError, KeyError:
                if fromloc.find('hwrftrk')>=0:
                    raise NoIndexError('hwrf track input has no grib1grbindex')
            if fromgrbindex is None and fromloc.find('hwrftrk')>=0:
                raise NoIndexError('hwrf track input has a grib1grbindex=None')
        toindex=str(index) if (index is not None) else '%s.wgrib_s'%(loc,)
        togrbindex=str(grbindex) if (grbindex is not None) \
            else '%s.grbindex'%(loc,)

        # Deliver the GRIB1 file and the text and binary index files:
        assert(fromloc is not None and fromloc!='')
        assert(loc is not None and loc!='')
        produtil.fileop.deliver_file(fromloc,loc,keep=keep,logger=logger)

        haveindex=False
        if fromindex is not None and toindex is not None and \
                produtil.fileop.isnonempty(fromindex):
            produtil.fileop.deliver_file(
                fromindex,toindex,keep=keep,logger=logger)
            haveindex=True

        havegrbindex=False
        if fromgrbindex is not None and togrbindex is not None and \
                produtil.fileop.isnonempty(fromgrbindex):
            produtil.fileop.deliver_file(
                fromgrbindex,togrbindex,keep=keep,logger=logger)
            havegrbindex=True

        if fromloc.find('hwrftrk')>=0:
            # print 'fromgrbindex = %s'%(repr(fromgrbindex),)
            # print 'togrbindex = %s'%(repr(togrbindex),)
            # if fromgrbindex is not None:
            #     print 'isnonempty = %s'%(repr(produtil.fileop.isnonempty(fromgrbindex),))
            assert(havegrbindex)

        # Store grid, index location, etc. in a transaction for speed
        # and also to ensure all records are updated or none are:
        with self.dstore.transaction():
            self.location=loc
            if haveindex: self.grib1index=toindex
            if havegrbindex: self.grib1grbindex=togrbindex
            try:
                self.grib1grid=frominfo.grib1grid
            except AttributeError,KeyError: pass
            self.available=True
        self.call_callbacks(logger=logger)

    def undeliver(self):
        loc=self.location
        if loc is not None and loc!='':
            produtil.fileop.remove_file(loc)
        index=self.grib1index
        if index: produtil.fileop.remove_file(index)
        grbindex=self.grib1grbindex
        if grbindex: produtil.fileop.remove_file(index)
        with self.dstore.transaction():
            del self.grib1grbindex
            del self.grib1index
            del self.grib1grid
            self.available=False
