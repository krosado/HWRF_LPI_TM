"""This module contains the GRIBTask, which is an HWRFTask that runs
the regribbing described in an hwrf.regrib.RegribMany object."""

__all__=['GRIBTask']

import os, os.path, time, socket, re, collections

import produtil.datastore, produtil.cd, produtil.fileop
import hwrf.post, hwrf.exceptions
import hwrf.regrib, hwrf.hwrftask, hwrf.config
import hwrf.storminfo, hwrf.numerics

from hwrf.regrib import GRIBBase, GRIBOp, RegribMany, GRIB2Op
from hwrf.numerics import to_datetime
from hwrf.exceptions import GribberError
from produtil.cd import TempDir, NamedDir
from produtil.datastore import Product, COMPLETED, UNSTARTED

from produtil.run import run,exe

def nonemaker():
    """Returns None."""
    return None
def nonedict():
    """Creates a collections.defaultdict that has a default value of
    None for any key that has no value."""
    return collections.defaultdict(nonemaker)

class LockDummy(object):
    """A Python "with" block construct that does nothing."""
    def __enter__(self): pass
    def __exit__(self,etype,evalue,traceback): pass

SHOULD_BE_MADE=object()
"""A constant used internally in GRIBTask to indicate a product should
be made."""

class GRIBTask(hwrf.hwrftask.HWRFTask):
    """An HWRFTask that runs regribbing for a list of input/output
    times.  The regribbing operations are specified in an
    hwrf.regrib.RegribMany object.  It is possible for multiple jobs
    to run the same GRIBTask at the same time: the task uses lock
    files and a produtil.datastore.Datastore to communicate between
    them."""
    def __init__(self,dstore,conf,section,regribmany,start,end,step,
                 atime=None,**kwargs):
        """Creates a new GRIBTask:
          dstore - the produtil.datastore.Datastore to use
          conf - the HWRFConfig to use for configuration options.
             This conf is passed down to the RegribMany during
             regribbing operations.
          section - the config section to use.
          start, end, step - the start and end times and timestep for
            the list of times to regrib.
          atime - the analysis time of the simulation.  This is needed 
            by some of the regribbing operations.  Note that this
            analysis time may differ from the conf.cycle, especially for
            data assimilation first guess jobs.
        Other keyword arguments are passed to the HWRFTask constructor"""

        assert(regribmany.has_deliveries())
        super(GRIBTask,self).__init__(dstore,conf,section,**kwargs)
        self._regribstore=hwrf.numerics.TimeArray(start,end,step,nonedict)
        self.start=self._regribstore.start
        self.end=self._regribstore.end
        self.timestep=self._regribstore.timestep
        n = len(self._regribstore)
        self._deliveries=collections.defaultdict(self.make_time_array_list)
        self._all_products=set()
        self._regribnames=list()
        self._subtasks=hwrf.numerics.TimeArray(start,end,step)
        logger=self.log()
        for time in self._subtasks.times():
            (ihr,imin) = hwrf.numerics.fcst_hr_min(time,self.start)
            taskname='%s.f%02dh%02dm' % ( self.taskname, ihr, imin )
            self._subtasks[time] = produtil.datastore.Task(self.dstore,
                taskname=taskname,logger=logger)

        if atime is not None: self.atime=self.start
        self._rm=regribmany
        self.make_products()
        regribmany.logger=self.log()
        self.workerdesc=None

        

    # --------------------------------------------------------------------------

    def make_time_array(self):
        """Returns a new hwrf.numerics.TimeArray for teh list of times
        to be processed."""
        return hwrf.numerics.TimeArray(self.start,self.end,self.timestep)
    def make_time_array_list(self):
        """Returns a new hwrf.numerics.TimeArray for the list of times
        to be processed.  Unlike make_time_array, this TimeArray will
        assume any times that don't have data assigned have an empty
        list()."""
        return hwrf.numerics.TimeArray(self.start,self.end,self.timestep,
                                       list)
    def get_data(self,name,time=None,update=False,**kwargs):
        """Used by RegribMany to retrieve intermediate results.  The
        extra update flag is not used by RegribMany, but rather used
        by GRIBTask.run() to force a recheck of the database to see if
        another GRIBTask has completed a regribbing operation by
        sending update=True."""
        assert(time is not None)
        if time is None: return None
        if name not in self._regribstore[time]:
            return None
        got=self._regribstore[time][name]
        if got is SHOULD_BE_MADE:
            return None
        if isinstance(got,Product):
            av=got.available
            if not av and update: 
                got.update()
                av=got.available
            if not av: return None
        strtime=time.strftime('%Y%m%d.%H%M%S')
        self.log().info('recalling result - %s %s'%(strtime,name))
        return got
    def set_data(self,name,data,time=None,logger=None,**kwargs):
        """Used by RegribMany to store intermediate results.  Will
        also deliver any output products."""
        assert(time is not None)
        if logger is None: logger=self.log()
        strtime=time.strftime('%Y%m%d.%H%M%S')
        if time is None: return
        logger.info('result obtained - %s %s'%(strtime,name))
        assert(name in self._regribstore[time])
        mydata=self._regribstore[time][name]
        if isinstance(mydata,produtil.datastore.Product):
            keep = name in self._deliveries
            logger.warning(
                'result obtained - %s %s: deliver (keep=%s) '
                'destionation = %s frominfo = %s'
                %(name,strtime,repr(keep),repr(mydata.location),repr(data)))
            mydata.deliver(frominfo=data,keep=keep)
            if name in self._deliveries:
                products=self._deliveries[name][time]
                for product in products:
                    if product!=mydata:
                        logger.warning('%s %s: also deliver to %s'%
                                        (name,strtime,product.location))
                        product.deliver(frominfo=data)
        else:
            logger.info('result obtained - %s %s: non-product '
                             'result.'%(strtime,name))
            self._regribstore[time][name]=data

    # --------------------------------------------------------------------------

    def make_products(self):
        """Creates Product objects for all output products.  Note that
        this is potentially an expensive function if the gribtask has
        a large number of products."""
        category=self.taskname
        deliveries=collections.defaultdict(list)
        grib2s=set()
        grib1s=set()
        alls=set()
        nongrib=set()
        logger=self.log()
        # Divide up into GRIB1 and GRIB2 products:
        for name,op in self._rm.GRIBOps():
            alls.add(name)
            if isinstance(op,GRIB2Op):
                grib2s.add(name)
            else: # assume anything that isn't GRIB2 but is a GRIBOp
                  # is a GRIB1
                grib1s.add(name)

        for name,op in self._rm.nonGRIBOps():
            nongrib.add(name)

        # Get the list of deliveries to make:
        regribset=set()
        for (where,name,loc,cat,prod,keep) in self._rm.deliveries():
            deliveries[name].append([where,loc,cat,prod,keep])
            if name not in regribset:
                regribset.add(name)
                self._regribnames.append(name)

        # Now loop over all output times and make Product objects:
        taskname=self.taskname
        make_product=self._make_product
        for time in self._regribstore.times():
            fcststr=time.strftime('%Y%m%d.%H%M%S')
            for name in nongrib:
                if self._rm.input_valid(name,time=time,task=self):
                    logger.debug('%s: make non-file %s at this time'
                                 %(fcststr,name))
                    self._regribstore[time][name]=SHOULD_BE_MADE
            for name in alls:
                if not self._rm.input_valid(name,time=time,task=self):
                    continue
                logger.debug('%s: make file %s at this time'%(fcststr,name))
                if name in grib2s:
                    ptype=hwrf.regrib.GRIB2Product
                else:
                    ptype=hwrf.regrib.GRIB1Product
                dlist=None
                if name in deliveries:
                    dlist=deliveries[name]
                if dlist is None or len(dlist)!=1:
                    # Zero deliveries or multiple deliveries so we
                    # need an intermediate file.
                    product=make_product(ptype,name,fcststr,time,
                                         cat=taskname)
                    self._all_products.add(product)
                    self._regribstore[time][name]=product
                    if dlist is not None:
                        for where,loc,cat,prod,keep in dlist:
                            product=make_product(ptype,name,fcststr,time,
                                                 where,loc,cat,prod)
                            self._all_products.add(product)
                            self._deliveries[name][time].append(product)
                else: # one delivery, so use that product as the
                      # target location
                    (where,loc,cat,prod,keep)=dlist[0]
                    product=make_product(ptype,name,fcststr,time,
                       where=where,loc=loc,cat=cat,prod=prod)
                    self._all_products.add(product)
                    self._regribstore[time][name]=product
            for name in self._regribstore[time].iterkeys():
                logger.debug('%s: final list contains: %s'%(fcststr,name))

    def products(self,name=None,time=None,yieldtime=False):
        """Iterates over products:

          name - specify the name of the products (same name as in the
            RegribMany).

          time - the forecast time of the product

          yieldtime - if True, instead of iterating over products, this
            will iterate over tuples containing the time and product."""

        if time is not None:
            if yieldtime or name is not None:
                neartime=self._regribstore.neartime(time)
            if name is not None:
                if name not in self._regribstore[neartime]: return
                found=self._regribstore[neartime][name]
                if found is None: return
                if yieldtime:
                    yield neartime,found
                else:
                    yield found
            else:
                for (name,product) in self._regribstore[time].iteritems():
                    if isinstance(product,produtil.datastore.Product):
                        if yieldtime:
                            yield neartime,product
                        else:
                            yield product
        else:
            for rtime in self._regribstore.datatimes():
                if name is not None:
                    if name not in self._regribstore[rtime]: return
                    product=self._regribstore[rtime].get(name,None)
                    if isinstance(product,produtil.datastore.Product):
                        if yieldtime:
                            yield rtime,product
                        else:
                            yield product
                else:
                    for (name,product) in self._regribstore[rtime].iteritems():
                        if isinstance(product,produtil.datastore.Product):
                            if yieldtime:
                                yield rtime,product
                            else:
                                yield product

    def _make_product(self,cls,name,fcststr,time,where=None,loc=None,
                      cat=None,prod=None):
        """The underlying implementation of make_products, this makes
        one product.
          cls - the class to create.  This is a constructor which will 
                be called like so:
            cls(self.dstore,category=cat,prodname=prod,location=loc)
          name - the name of the product from the RegribMany
          fcststr - a string representation of the forecast time, used
            for logging and product location generation
          time - the forecast time
          where - "com" or "intercom".  Default: "intercom"
          loc - the product location.  A reasonable default is chosen
            if this is unspecified
          cat - the product category.  Default: self.taskname
          prod - the product's prodname.  Default: name."""
        if where is None: where='intercom'
        if cat is None:
            cat=self.taskname
        elif '{' in cat:
            cat=self.conftimestrinterp(cat,time)
        if loc is None:
            assert(where=='intercom') # only auto-gen locations in temporary areas?
            loc='%s/%s.%s'%(self.taskname,name,fcststr)
        elif '{' in loc:
            loc=self.conftimestrinterp(loc,time)
        if prod is None:
            prod='%s/%s'%(where,loc)
        elif '{' in prod:
            prod=self.conftimestrinterp(prod,time)
        loc=os.path.join(self.conf.getdir(where),loc)
        assert(loc is not None and loc!='')
        assert(cat is not None and cat!='')
        assert(prod is not None and prod!='')
        return cls(self.dstore,category=cat,prodname=prod,location=loc)

    # --------------------------------------------------------------------------

    def uncomplete(self):
        """Marks this task and all subtasks as incomplete, ensuring
        that the next call to run or runpart will attempt to produce
        any products that are not delivered.  All products that are
        "available" but are not on disk are marked as unavailable."""
        logger=self.log()
        with self.dstore.transaction():
            for task in self._subtasks:
                if not isinstance(task,produtil.datastore.Task):
                    logger.critical('Somehow ended up with a non-Task '
                                    'in the array of tasks: %s'
                                    %(repr(task),))
                assert(isinstance(task,produtil.datastore.Task))
                task.state=UNSTARTED
            for product in self._all_products:
                if not product.available: 
                    logger.info('%s: not available'%(product.did,))
                    continue
                loc=product.location
                if loc!='':
                    if not produtil.fileop.isnonempty(loc):
                        logger.warning('%s: no file at %s'%(
                                product.did,loc))
                        product.undeliver()
                    else:
                        logger.info('%s: already at %s'%(product.did,loc))
                else:
                    logger.warning('%s: no location, but set to available'
                                   %(product.did,))
                    product.available=False
            self.state=UNSTARTED

    def call_completed_callbacks(self):
        """Calls the Product.call_callback function for all completed
        and delivered products."""
        logger=self.log()
        logger.warning('Calling all callbacks for products that have '
                       'already been completed.')
        ncomplete=0
        ntotal=0
        ncalled=0
        for product in self._all_products:
            ntotal+=1
            if product.available:
                ncomplete+=1
                if product.location:
                    if product.has_callbacks():
                        
                        logger.info('%s: Calling all callbacks for this '
                                    'product.  Product location is %s'
                                    %(product.did, product.location))
                        product.call_callbacks()
                    else:
                        logger.info('%s: Already completed, but has no '
                                    'callbacks.  Product location is %s'
                                    %(product.did, product.location))
                else:
                    logger.info('%s: Product has no location.  I will not '
                                'call its callbacks.'%(product.did,))
        logger.warning('Done calling callbacks for already completed '
                       'products.')
        logger.warning('Called %d callbacks out of %d completed products, '
                       'of %d total products for this job.'
                       %(ncalled,ncomplete,ntotal))

    def unrun(self):
        """Deletes all output products, and marks this task and all
        subtasks as incomplete."""
        with self.dstore.transaction():
            for product in self._all_products:
                assert(product.location != '')
                product.undeliver()
                assert(product.location != '')
#                produtil.datastore.force_unlock(product)
            for task in self._subtasks:
                task.unrun()
            self.state=UNSTARTED

    def run_helper(self,one=False,worker=None,raiseall=False,now=False):
        """Performs the actual work of run() and runpart()

          one = True for runpart(), false for run().  If True, exits
            after one product is processed.
          worker = ignored
          raiseall = If true, nearly all exceptions are raised.
          now = If true, the function will not sleep or wait."""
        if self.is_completed():
            return
        logger=self.log()
        # waitsleep: sleep time if some inputs were not ready
        waitsleep=self.confint('waitsleep',10) 
        # readysleep: sleep time if all products were ready
        readysleep=self.confint('readysleep',2) 

        # worker: now defunct; produtil.datastore.Worker is gone:
        #if worker is None:
        #    workerdesc='host %s process %d'%(socket.gethostname(),os.getpid())
        #    worker=produtil.datastore.Worker(self.dstore,workerdesc,logger=logger)

        produtil.fileop.makedirs(os.path.join(self.getdir('intercom'),self.taskname))
        lockdir=os.path.join(self.getdir('lockdir'),self.taskname)
        produtil.fileop.makedirs(lockdir,logger=logger)
        produtil.fileop.makedirs(self.workdir,logger=logger)

        fail_counts=collections.defaultdict(lambda: 0)
        all_fails=0

        for rtime in self._regribstore.times():
            strtime=rtime.strftime('%Y%m%d.%H%M%S')
            logger.info('%s: will process time %s'%(self.taskname,strtime))

        first_n=8
        first_time_through=True
        notready=True

        dummy=LockDummy()
        attempted_something=True
        while True:
            # I have no idea what this was supposed to do, but
            # it does not do what it claims to do and it
            # causes random failures of the products job:
            #
            # if not attempted_something and not notready:
            #     raise hwrf.exceptions.PostFailed(
            #         'Gave up: too many remaining tasks failed.')
            attempted_something=False
            if all_fails>100:
                raise hwrf.exceptions.PostFailed(
                    'Gave up: more than 100 regribbing operations failed.')

            if not first_time_through:
                sleeptime = waitsleep if notready else readysleep
                loggify = logger.warning if notready else logger.info 
                loggify('sleep %d...'%(sleeptime,))
                time.sleep(sleeptime)
                loggify('done sleeping.')
            first_time_through=False
            nincomplete=list()
            # Find the first five incomplete tasks:
            with self.dstore.transaction():
                for rtime in self._regribstore.times():
                    subtask=self._subtasks[rtime]
                    if not subtask.is_completed():
                        subtask.update()
                        if not subtask.is_completed():
                            nincomplete.append( (subtask,rtime) )
                    if len(nincomplete)>=first_n: break             
            logger.warning('Considering these times: %s'%
                ', '.join([ t.strftime('%Y%m%d-%H%M%S') \
                                for (s,t) in nincomplete ]))
            if len(nincomplete)<=0:
                logger.warning('No subtasks incomplete.  I think I am '
                               'done running.  Will exit regribber now.')
                break
            for (subtask,rtime) in nincomplete:
                strtime=rtime.strftime('%Y%m%d.%H%M%S')
                logger.info('%s: examine this time...'%(strtime,))
                if subtask.is_completed():
                    logger.info('%s: already done.'%(strtime,))
                    continue
                regribber=self._rm.copy()
                regribber._data=self
                thisdone=True
                notready=False
                for name in self._rm.names():
                    failkey=(name,strtime)
                    if failkey in fail_counts:
                        if fail_counts[failkey]>=3:
                            logger.debug('%s %s: skip: failed too many times.'
                                         %(strtime,name))
                            continue
                        else:
                            logger.info('%s %s: fail count is %d'
                                        %(strtime,name,fail_counts[failkey]))
                    rst=self._regribstore[rtime]
                    logger.info('%s: consider product %s'%(strtime,name))
                    if name not in rst:
                        logger.info('%s: no %s product at this time.'
                                    %(strtime,name))
                        continue
                    try:
                        if self.get_data(name,rtime) is not None:
                            logger.info('%s %s: already done'%(name,strtime))
                            continue
                        if not regribber.is_ready(name,time=rtime,task=self):
                            msg='%s: %s: not ready yet.'%(strtime,name)
                            logger.info(msg)
                            thisdone=False
                            notready=True
                            if now and raiseall: raise GribberError(msg)
                            continue
                        if name in self._regribnames:
                            logger.info('%s %s: will need lock'
                                        %(name,strtime))
                            lockfile=os.path.join(lockdir,'%s.%s'
                                                  %(strtime,name))
                            locker=produtil.locking.LockFile(
                                filename=lockfile,logger=logger,max_tries=1,
                                giveup_quiet=True)
                        else:
                            (product,locker)=(None,dummy)
                        
                        attempted_something=True
                        with locker:
                            if self.get_data(name,rtime,update=True) \
                                    is not None:
                                logger.info('%s %s: already done'
                                            %(name,strtime))
                                continue
                            with TempDir(prefix='%s/%s.%s.'
                                         %(self.taskname,strtime,name),
                                         dir=self.workdir,keep=False,
                                         keep_on_error=False):
                                if regribber.is_grid(name): 
                                    logger.info(
                                        '%s: %s: calculate this grid.' 
                                        %(strtime,name)) 
                                else: 
                                    logger.warning(
                                        '%s: %s: process this grib1/2 '
                                        'product.'%(strtime,name))
                                if regribber.is_ready(name,time=rtime,
                                                      task=self):
                                    regribber.make(name,time=rtime,task=self)
                                if self.get_data(name,rtime) is None:
                                    msg='%s %s: somehow regribber.make '\
                                      'did not deliver anything'%(name,strtime)
                                    logger.warning(msg)
                                    thisdone=False
                                    if raiseall:  raise GribberError(msg)
                    except hwrf.exceptions.NoProductError as npe:
                        logger.info('%s %s: internal error: product does '
                                    'not exist at this time.'%(name,strtime))
                        raise
                    except produtil.locking.LockHeld as lh:
                        logger.info('%s %s: lock held, move on.'
                                    %(name,strtime))
                        thisdone=False
                    except Exception as e:
                        logger.warning('%s %s: failed with exception %s'
                                       %(name,strtime,str(e)),exc_info=True)
                        if raiseall: raise
                        all_fails+=1
                        fail_counts[failkey] = fail_counts[failkey]+1
                        if fail_counts[failkey]>=3:
                            logger.error('%s %s: failed too many times'
                                         %(strtime,name))
                        thisdone=False
                if thisdone:
                    logger.info('%s: done.'%(strtime,))
                    subtask.state=COMPLETED
                    if one: return
                else:
                    logger.info('%s: not done.'%(strtime,))
                logger.debug('%s: on to next time....'%(strtime,))
        self.state=COMPLETED
    def run(self,**kwargs):
        """Runs all regribbing.  Does not return until all regribbing
        is complete, or a fatal error happens.  It is safe to run this
        in multiple threads at the same time.  Through file locking
        and database usage, the jobs will work together."""
        with NamedDir(self.workdir):
            self.run_helper(False,**kwargs)
    def runpart(self,**kwargs):   
        """Performs a small amount of work and returns."""
        with NamedDir(self.workdir):
            self.run_helper(True,**kwargs)
