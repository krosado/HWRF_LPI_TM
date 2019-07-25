"""This module, contrary to its name, implements HWRFTask classes to
run the real_nmm and WRF, including tasks to make the the wrfanl and
ghost files.  The uncoupled forecast is found in the WRFAtmos class.
The coupled forecast is not in this module: it is hwrf.mpipomtc
instead."""

__all__=['FcstTask']

import os, collections, glob, re,math, fractions, shutil, sys, time,logging
import produtil.cd, produtil.fileop, produtil.datastore, produtil.run
import hwrf.hwrftask, hwrf.exceptions, hwrf.numerics
import hwrf.wrf, hwrf.numerics, hwrf.wrfbase

from produtil.cd import NamedDir
from produtil.fileop import realcwd, deliver_file, make_symlink, \
    check_last_lines, isnonempty, remove_file
from produtil.datastore import FileProduct, UpstreamFile, UNSTARTED
from produtil.run import checkrun, mpi, mpirun, bigexe, ExitStatusException,\
    run
from hwrf.exceptions import WRFGeogridMissing, WRFMetgridMissing, \
    WRFPrepMissing, WRFInputMissing, WRFBdyMissing, WRFAnlMissing, \
    ForecastInputError, SetNestFailed, RealNMMError
from hwrf.numerics import to_fraction, within_dt_epsilon, to_datetime_rel, \
    to_timedelta
from hwrf.wrf import ExternalWRFTask

########################################################################

class Input2Fcst(object):
    """This is the abstract base class of anything that gets, or
    creates, input files for a WRF simulation without running another
    Task.  For example, something that copies the geogrid output would
    be a subclass of Input2Fcst"""
    def __init__(self,src): self.src=src
    def get_inputs(self,just_check=False,**kwargs): 
        """In subclasses, this function copies or links the input
        files.  This default implementation does nothing."""
        return True
    def link_product(self,product,excclass,logger,target=None,
                     just_check=False):
        """Links the file from the given product to the target
        location (basename(product.location) if no target is
        provided).  If the product is not yet available or has no
        location, then the given exception class excclass is
        raised."""
        assert(logger is not None or not just_check)
        assert(isinstance(product,produtil.datastore.Product))
        (L,A)=(product.location,product.available)
        if L and A:
            if target is None: target=os.path.basename(L)
            if just_check:
                if isnonempty(L):
                    return True
                elif logger is not None:
                    msg='%s: file is empty or non-existent'%(L,)
                    logger.warning(msg)
                return False
            make_symlink(L, target, logger=logger,force=True)
            return True
        msg='%s: unavailable (available=%s location=%s)'%(
            str(product.did),repr(A),repr(L))
        if logger is not None: logger.warning(msg)
        if just_check: return False
        raise excclass(msg)

class Geog2WRF(Input2Fcst):
    """Links Geogrid data to the current directory for a wrf or
    real_nmm run."""
    def get_inputs(self,logger,domain,just_check=False,**kwargs):
        if self.src is not None and domain is not None:
            p=self.src.geodat(domain)
            if p: return self.link_product(p,WRFGeogridMissing,logger,
                                           just_check=just_check)
            return False

class Met2WRF(Input2Fcst):
    """Links Metgrid data to the current directory for a wrf or
    real_nmm run."""
    def get_inputs(self,logger,ftime,just_check=False,**kwargs):
        if self.src is not None and ftime is not None:
            p=self.src.met_at_time(ftime)
            if p: return self.link_product(p,WRFMetgridMissing,logger,
                                           just_check=just_check)
            return False

class WRFInput2WRF(Input2Fcst):
    """Links real_nmm wrfinput_d01 files the current directory for a
    wrf run."""
    def get_inputs(self,logger,atime,domain,just_check=False,**kwargs):
        if self.src is not None and atime is not None and domain is not None:
            p=self.src.wrfinput_at_time(atime,domain)
            if p:
                return self.link_product(p,WRFInputMissing,logger,
                                         target='wrfinput_d01',
                                         just_check=just_check)
            return False

class Fort652WRF(Input2Fcst):
    """Links real_nmm fort.65 files the current directory for a wrf run."""
    def get_inputs(self,logger,atime,domain,just_check=False,**kwargs):
        if self.src is not None and atime is not None and domain is not None:
            p=self.src.fort65_at_time(atime,domain)
            if p: 
                return self.link_product(p,WRFInputMissing,logger,
                                         target='fort.65',
                                         just_check=just_check)
            return False
class WRFAnl2WRF(Input2Fcst):
    """Links wrfanl or ghost files the current directory for a wrf run."""
    def __init__(self,src,domain):
        super(WRFAnl2WRF,self).__init__(src)
        self.domain=domain
    def get_inputs(self,logger,atime,domain,wrfanl='wrfanl',
                   just_check=False,**kwargs):
        if self.src is not None and atime is not None and domain is not None\
                and domain==self.domain: # yay for run-on sentences!
            p=self.src.wrfanl_at_time(atime,domain)
            if p:
                localname=hwrf.wrfbase.parse_wrf_outname(
                    str(wrfanl)+'_d<domain>_<date>',domain.get_grid_id(),
                    atime,domain.nocolons)
                return self.link_product(
                    p,WRFAnlMissing,logger,target=localname,
                    just_check=just_check)
            return False

class WRFBdy2WRF(Input2Fcst):
    """Links real_nmm wrfbdy_d01 files the current directory for a wrf
    run."""
    def get_inputs(self,logger,atime,domain,just_check=False,**kwargs):
        if self.src is not None and atime is not None and domain is not None:
            p=self.src.wrfbdy_at_time(atime,domain)
            if p:
                return self.link_product(p,WRFBdyMissing,logger,
                                         target='wrfbdy_d01',
                                         just_check=just_check)
            return False
class Prep2WRF(Input2Fcst):
    """Links prep_hybrid files to the current directory for a real_nmm
    run."""
    def get_inputs(self,logger,times,just_check=False,**kwargs):
        if self.src is not None and times:
            for i in xrange(len(times)):
                logger.info('Look for prep at time '+str(i))
                t=times[i]
                what = 'init' if(i==0) else 'bdy'
                prod=[p for p in self.src.products(time=t,name=what)]
                if not prod:
                    if just_check: return False
                    raise WRFPrepMissing('No prep %s data for t=%s'%(
                            what,t.strftime('%Y%m%d-%H%M%S')))
                prod=prod[0]
                if not self.link_product(prod,WRFPrepMissing,logger,
                                         'fort.%03d'%(i+900),
                                         just_check=just_check):
                    if just_check: return False
        else:
            logger.warning(
                'When looking for prep data, src is none or times is false:'
                'src=%s times=%s'%(repr(src),repr(times)))
        if just_check: return False
        return True

########################################################################
class FcstTask(hwrf.hwrftask.HWRFTask):
    """Abstract base class of anything that runs a forecast model, or
    prepares input to a forecast model.  This should not be
    instantiated directly."""
    def __init__(self,dstore,conf,section,outdir=None,taskname=None,
                 **kwargs):
        assert(taskname is not None)
        if taskname is None: taskname=section
        super(FcstTask,self).__init__(dstore,conf,section,taskname=taskname,
                                      outdir=outdir,**kwargs)
        self.inputs=collections.defaultdict(list)
    def has_input(self,typename):
        """Returns True if there is at least one input source for the
        specified input type.  That is, if someone called
        add_input(typename,(something)).  Returns False otherwise."""
        return self.inputs.has_key(typename)
    def add_input(self,typename,inobj):
        """Adds an input of the given type typename that should be
        provided by the given object.  The object should be a subclass
        of Input2Fcst."""
        self.inputs[typename].append(inobj)
        return self
    def check_input(self,typenames,**kwargs):
        """Checks all inputs of the given typenames to make sure
        link_input would work if called with the same parameters.
        Returns True if link_input should succeed, and False if it
        would fail.  This is a simple wrapper around link_input with
        just_check=True.  However, subclasses may override this to
        perform additional checks, such as for a coupled model."""
        return self.link_input(typenames,just_check=True,**kwargs)
    def link_input(self,typenames,just_check=False,**kwargs):
        """Links all inputs of types given in typenames (an iterable)
        by calling obj.get_inputs on anything sent to self.add_input.
        If multiple input sources are available for a given input
        type, then only the first one that has input is used.

        Do not use the just_check option: it is part of the internal
        implementation of check_input; if you need to just check the
        inputs, use check_input instead.  If just_check=True, then
        nothing is linked.  Instead, the routine just checks to see if
        the inputs are availabe.  That is the same as calling
        check_input.  However, subclasses may override check_input to
        check additional inputs as part of a coupled model."""
        if isinstance(typenames,basestring): typenames=( typenames, )
        logger=self.log()
        for typename in typenames:
            logger.info('Look for input of type %s with kwargs=%s'
                        %(typename,repr(kwargs)))
            if typename in self.inputs:
                thelist=self.inputs[typename]
                found=False
                itry=0
                for inputter in thelist:
                    logger.info('Check %s for input of type %s'
                                %(inputter,typename))
                    itry+=1
                    try:
                        found=(inputter.get_inputs(
                                logger,just_check=just_check,**kwargs)
                               is True )
                        if found:
                            logger.info(
                                'Found input type %s in inputter #%d (%s)'
                                %(repr(typename),itry,repr(inputter)))
                            break
                        else:
                            logger.warning(
                                'Could not get input type %s in inputter'
                                ' #%d (%s)'
                                %(repr(typename),itry,repr(inputter)))
                    except (ForecastInputError,KeyError) as e:
                        logger.warning(
                            'cannot get %s files due to exception: %s'
                            %(typename,str(e)),exc_info=True)
                    if found: break
                if not found:
                    msg='%s: could not find input files of this type.  '\
                        'Giving up.'%(typename,)
                    if just_check:
                        logger.warning(msg)
                        return False
                    else:
                        logger.error(msg)
                        raise ForecastInputError(msg)
        return True

########################################################################
class WRFTaskBase(FcstTask):
    """This is the abstract base class of tasks that run real or WRF.
    The purpose of this class is simply to reduce code duplication."""
    def __init__(self,dstore,conf,section,wrf,keeprun=True,**kwargs):
        """Creates a WRFTaskBase for the specified datastore, conf and
        section.  The wrf argument is a WRFSimulation.  It will not be
        used directly: instead the wrf is copied, and the copy is
        used.  If keeprun=True, then the output of the simulation,
        after running it, will not be scrubbed.  Other keyword
        arguments are passed to the superclass FcstTask."""
        super(WRFTaskBase,self).__init__(dstore,conf,section,**kwargs)
        self.__wrf=self.make_wrf(wrf)
        self.make_products()
        self.dt_epsilon=wrf.bdyepsilon()
        self.keeprun=keeprun
    def _set_wrf_proc_config(self,wrf,logger=None):
        """This is a protected member function meant to be used by the
        make_wrf implementation in subclasses.  This class does not
        use it.  It sets the WRF nproc_x and nproc_y, and sets I/O
        server settings if desired based on config options:

          nproc_x, nproc_y = compute grid dimensions
          nio_groups = number of I/O server groups
          nio_tasks_per_group = number of servers per group
          poll_servers = True to poll I/O servers.  This generally 
            decreases the number of I/O server groups needed for a
            given WRF run."""
        if logger is None: logger=self.log()
        assert(wrf is not None)
        nio_groups=max(0,self.confint('nio_groups',1))
        nio_tasks_per_group=max(0,self.confint('nio_tasks_per_group',0))
        poll_servers=self.confbool('poll_servers',True)
        nproc_x=self.confint('nproc_x',-999)
        nproc_y=self.confint('nproc_y',-999)

        if nio_groups*nio_tasks_per_group > 0:
            logger.debug(
                'Using %d groups of %d io tasks with poll_servers=%s'%(
                    nio_groups,nio_tasks_per_group,repr(poll_servers)))
            wrf.set_io_servers(nio_tasks_per_group,nio_groups,poll_servers)
        else:
            logger.debug('Not setting io_server settings.')
            
        if (nproc_x>0 and nproc_y>0) or (nproc_x==-1 and nproc_y==-1):
            logger.debug('Setting nproc_x=%d nproc_y=%d'%(nproc_x,nproc_y))
            wrf.set_nprocs(nproc_x,nproc_y)
        else:
            logger.debug('Not setting nproc_x or nproc_y (%s,%s)'%(
                    repr(nproc_x),repr(nproc_y)))

    def make_wrf(self,wrf): 
        """Returns a WRFSimulation object for this class."""

        return wrf.copy()
    def add_geogrid(self,g):
        """Adds an input source (via self.add_input) that will provide
        the output of WPS geogrid.exe.  The given object must have a
        geodad member function which takes a WRFDomain as its argument
        and returns a Product to link.  Returns self."""
        return self.add_input('geogrid',Geog2WRF(g))
    def add_metgrid(self,m):
        """Adds an input source (via self.add_input) that will provide
        the output of WPS metgrid.exe.  The given object must have a
        met_at_time function that returns a Product to link for a
        specified forecast time.  Returns self."""
        return self.add_input('metgrid',Met2WRF(m))
    def add_prep_hybrid(self,p):
        """Adds an input source (via self.add_input) that will provide
        the output of the prep_hybrid program.  The given object must
        have a products function that iterates over products for a
        given name='bdy' or name='init' and a time=F for a given
        forecast time F.  Returns self."""
        return self.add_input('prep',Prep2WRF(p))
    @property
    def use_prep_hybrid(self):
        """Returns True if prep_hybrid was requested, and False
        otherwise."""
        return self.has_input('prep')
    def add_wrfinput(self,r):
        """Adds an input source (via self.add_input) that will provide
        the wrfinput output file from real_nmm.  The given object must
        have a wrfinput_at_time(atime,domain) function that returns a
        Product for a given analysis time and WRFDomain object.
        Returns self."""
        return self.add_input('wrfinput',WRFInput2WRF(r))
    def add_wrfbdy(self,r):
        """Adds an input source (via self.add_input) that will provide
        the wrfbdy output of a real_nmm run.  The given object must
        have a wrfbdy_at_time(atime,domain) function that returns a
        Product to link for a specified analysis time and WRFDomain
        object.  Returns self."""
        return self.add_input('wrfbdy',WRFBdy2WRF(r))
    def add_fort65(self,r):
        """Adds an input source (via self.add_input) that will provide
        the fort.65 output from real_nmm. given object must have a
        fort65_at_time(atime,domain) function that returns a Product
        to link for a specified analysis time and WRFDomain object.
        Returns self."""
        return self.add_input('fort65',Fort652WRF(r))
    def add_real(self,r):
        """This is a convenience function that simply passes its
        argument to self.add_fort65, add_wrfinput and add_wrfbdy in
        that order.  Returns self."""
        self.add_fort65(r)
        self.add_wrfinput(r)
        return self.add_wrfbdy(r)
    def add_wrfanl(self,r,domain):
        """Adds an input source (via self.add_input) that will provide
        the wrfanl output file from a prior run of wrf.exe.  The given
        object must have a wrfanl_at_time function that returns a
        Product to link for a specified analysis time and domain.
        Returns self."""
        name='wrfanl-%s'%(domain.name,)
        return self.add_input(name,WRFAnl2WRF(r,domain))
    def make_products(self):
        """This is called from the WRFTaskBase constructor.
        Subclasses should re-implement this method to generate
        internal information about what products this class can
        provide.  The default implementation does nothing."""
    def link_fix(self):
        """Links or copies fix files needed by WRF.  Will copy if the
        link_wrf_fix=no in this task's config section.  Otherwise, the
        files are linked."""
        link_files=self.confbool('link_wrf_fix',True)
        act='link' if link_files else 'copy'
        for confitem in ('fix.eta_lookup', 'fix.track', 'fix.wrf_other'):
            pattern=self.confstr(confitem,'')
            logger=self.log()
            if not pattern: 
                logger.warning(
                    '%s: no conf entry for this fix file or set of fix files'
                    %(confitem,))
                continue
            logger.info('Will %s WRF fix files from %s to current directory.'
                        %(act,pattern))
            for src in glob.glob(pattern):
                tgt=re.sub('^hwrf_','',os.path.basename(src))
                if link_files or os.path.isdir(src):
                    make_symlink(src,tgt,logger=logger,force=True)
                else:
                    deliver_file(src,tgt,logger=logger)
    def check_all_inputs(self):
        """Checks to see if all needed input is available."""
        return self.link_all_inputs(just_check=True)
    def link_all_inputs(self,just_check=False):
        """Links all inputs provided by the various add_(whatever)
        member functions."""
        use_prep = ('prep' in self.inputs)
        okay=True
        if use_prep:
            okay=okay and self.link_input(
                'prep',times=[t for t in self.__wrf.bdytimes()],
                just_check=just_check)
        for domain in self.__wrf:
            okay=okay and self.link_input(
                'geogrid',domain=domain,just_check=just_check)
        for ftime in self.__wrf.bdytimes():
            okay=okay and self.link_input(
                'metgrid',ftime=ftime,just_check=just_check)
            if not self.need_all_metgrid() or use_prep: break
        if 'wrfinput' in self.inputs:
            okay=okay and self.link_input(
                'wrfinput',domain=self.__wrf.get_moad(),
                atime=self.__wrf.simstart(),just_check=just_check)
        if 'wrfbdy' in self.inputs:
            okay=okay and self.link_input(
                'wrfbdy',domain=self.__wrf.get_moad(),
                atime=self.__wrf.simstart(),just_check=just_check)
        if 'fort65' in self.inputs:
            okay=okay and self.link_input(
                'fort65',domain=self.__wrf.get_moad(),
                atime=self.__wrf.simstart(),just_check=just_check)
        MOAD=self.__wrf.get_moad()
        for domain in self.__wrf:
            if domain!=MOAD:
                name='wrfanl-%s'%(domain.name,)
                if name in self.inputs:
                    okay=okay and self.link_input(
                        name,domain=domain,atime=self.__wrf.simstart(),
                        just_check=just_check)
        return okay
    def need_all_metgrid(self): 
        """Returns True if all metgrid files are needed as input to
        this Task"""
        return False
    def run_exe(self,exename='wrf',not_allranks=False,runner=None,
                sleeptime=None):
        """Runs the executable this task is responsible for running.
        Determines if the program ran correctly.  The exename is the
        name of the argument in the [exe] section of the
        HWRFConfig.  Options:

          not_allranks=True - by default, all ranks are used to run
          the executable.  Pass not_allranks=True to run on only one
          ranks.

          runner=Runner - pass a produtil.prog.Runner object if
          desired.  This overrides any decision of what to run: the
          exename and not_allranks will be ignored, and whatever is
          supplied in runner is simply passed to produtil.run.run.

          sleeptime - passed to produtil.run.run to determine how
          often to check the child process.  By default, the sleeptime
          option in this task's config section is used, or if that is
          absent, 30 seconds."""
        if sleeptime is None:
            sleeptime=self.conffloat('sleeptime',30)
        logger=self.log()
        if runner is None:
            exe=self.getexe(exename)
            if not_allranks:
                runner=mpirun(mpi(exe)) # run in one rank
            else:
                runner=mpirun(mpi(exe),allranks=True)
        stat=run(runner,logger=logger,sleeptime=sleeptime)
        logger.info('%s: exit status %d'%(exename,stat))
        if not check_last_lines('rsl.out.0000','SUCCESS COMPLETE',
                                logger=logger):
            msg='%s: did not see SUCCESS COMPLETE in rsl.out.0000'%(exename,)
            logger.error(msg)
            raise RealNMMError(msg)
        else:
            logger.info('%s: SUCCESS COMPLETE in rsl.out.0000'%(exename,))
    def final_prerun(self): 
        """Called by self.run() just before calling run_exe.  The
        default implementation does nothing.  This is intended to be
        overridden by subclasses."""
    def initial_prerun(self): 
        """Called by self.run() after CDing to the new directory, but
        before doing anything else.  The default implementation does
        nothing.  This is intended to be overridden by subclasses."""
    def run(self):
        """Performs all work needed to run the program.  Creates the
        work directory, CD's to it, runs the initial_prerun, link_fix,
        link_all_inputs, make_namelist, final_prerun, run_exe, postrun
        and deliver_products."""
        logger=self.log()
        runhere=self.workdir
        if os.path.exists(runhere):
            logger.warning('%s: directory exists; will delete'%(runhere,))
            assert(not os.path.samefile(self.getdir('WORKhwrf'),runhere))
            shutil.rmtree(runhere)
        atime=self.__wrf.simstart()
        with NamedDir(runhere,keep=self.keeprun,logger=logger,
                      keep_on_error=True) as rundir:
            try:
                logger.info('%s running in directory %s'%(
                        self.taskname,realcwd()))
                self.location=runhere
                self.initial_prerun()
                self.link_fix()
                self.link_all_inputs()
                self.make_namelist()
                self.final_prerun()
                self.run_exe()
                self.postrun()
                self.deliver_products()
            except Exception as e:
                logger.critical('%s failed: %s'%(self.taskname,str(e)),
                                exc_info=True)
                raise
        self.postmsg('%s: completed'%(self.taskname,))
    def postrun(self): 
        """Called by self.run() just after run_exe returns
        successfully.  The default implementation does nothing; this
        is intended to be overridden in subclasses."""
    def deliver_products(self):
        """Called by self.run() after postrun(), just before CDing out
        of the work directory.  This should deliver products to their
        destinations.  The default implementation raises
        NotImplementedError.  This MUST be overridden in
        subclasses."""
        raise NotImplementedError('A WRFTaskBase subclass did not '
                                  'implement deliver_products')
    def make_namelist(self,filename='namelist.input',logger=None):
        """Runs set_ij_start (swcorner_dynamic) to generate the i & j
        start locations for domain 2, then generates the namelist."""
        if logger is None: logger=self.log()
        domlat=self.conf.getfloat('config','domlat')
        domlon=self.conf.getfloat('config','domlon')
        s=self.wrf().swcorner_dynamic(self.getexe('swcorner_dynamic'),
                                      self.storminfo, domlat,domlon,logger)
        with open(filename,'wt') as nlin:
            nlin.write(s)
    def wrf(self): 
        """Returns the WRFSimulation object used by this task."""
        return self.__wrf
    @property
    def sim(self): 
        """Returns the WRFSimulation object used by this task.  Has
        the same effect as self.wrf(), but this is a property
        instead."""
        return self.__wrf

########################################################################
class RealNMM(WRFTaskBase):
    """This subclass of WRFTaskBase runs the real_nmm to generate
    inputs for the provided WRFSimulaton."""
    def make_products(self):
        """Generates FileProduct objects for the files that should be
        delivered in deliver_products."""
        self.prod_wrfinput=FileProduct(self.dstore,'wrfinput_d01',
            self.taskname,location=os.path.join(self.outdir,'wrfinput_d01'))
        self.prod_wrfbdy=FileProduct(self.dstore,'wrfbdy_d01',self.taskname,
            location=os.path.join(self.outdir,'wrfbdy_d01'))
        self.prod_log=FileProduct(self.dstore,'rsl.out.0000',self.taskname,
            location=os.path.join(self.outdir,'rsl.out.0000'))
        self.prod_fort65=FileProduct(self.dstore,'fort.65',self.taskname,
            location=os.path.join(self.outdir,'fort.65'))
    def add_prep_hybrid(self,p):
        ret=super(RealNMM,self).add_prep_hybrid(p)
        self.sim.nl.nl_set('domains','use_prep_hybrid',True)
        return ret
    def make_wrf(self,wrf):
        """Creates a copy of the specified WRFSimulation wrf.  The
        copy has a simulation length of one minute or one physics
        timestep, whichever is larger.  Also, it produces wrfanl files
        for all domains."""
        wrf=wrf.copy()
        wrf.nl.nl_set('domains','use_prep_hybrid',self.use_prep_hybrid)
        return wrf
    def need_all_metgrid(self): 
        """Returns True if all metgrid files are needed as input to
        this Task"""
        return True
    def initial_prerun(self):
        """Deletes the wrfinput_d01 and wrfbdy_d01."""
        logger=self.log()
        for f in ['wrfinput_d01','wrfbdy_d01']:
            try:
                remove_file(f,logger)
            except(EnvironmentError) as e:
                logger.warning('%s: cannot remove, but continuing anyway.  '
                               'Error was: %s'%(f,str(e)),exc_info=True)
    def run_exe(self,exename='real_nmm',sleeptime=30):
        """Tries several times to run the real_nmm.  This is a
        workaround for problems encountered on Zeus when running the
        experimental 9:3:1 HWRF.  The real_nmm would exit for no
        apparent reason about 50% of the time.  Eventually that was
        tracked down to a memory error caused by NetCDF, which forced
        us to use intio for some files, NetCDF for others, and PNetCDF
        for yet more files."""
        logger=self.log()
        logger.info('real section is '+repr(self.section))
        maxtries=self.confint('max_tries',-999)
        logger.info('real max_tries is '+repr(maxtries))
        maxtries=max(1,maxtries)
        logger.info('after max(1,...), real max_tries is '+str(maxtries))
        
        logger.info('Will try to run real %d times'%(maxtries,))
        for trynum in xrange(maxtries):
            try:
                super(RealNMM,self).run_exe(exename,'prep' in 
                                            self.inputs,sleeptime=sleeptime)
                logger.info('Real succeeded.  Hurrah!')
                break
            except(EnvironmentError,RealNMMError,ExitStatusException) as e:
                if(trynum+1<maxtries):
                    logger.warning(
                        'Real failed %d time(s).  Will retry after %d '
                        'second sleep.  Error: %s'%(
                            trynum+1,sleeptime,str(e)),exc_info=True)
                    time.sleep(sleeptime)
                    logger.info(
                        'Returned from sleeping.  Will now retry real.')
                else:
                    logger.error(
                        'Real failed %d time(s).  Aborting.  Error: %s'
                        %(maxtries,str(e)),exc_info=True)
                    raise
    def get_wrfinput(self):
        """Returns the wrfinput file regardless of the time or
        domain"""
        return self.prod_wrfinput
    def wrfinput_at_time(self,atime=None,domain=None):
        """Returns the wrfinput file for the specified time and
        domain.  Returns None if that time and domain are not
        valid."""
        if domain and domain!=self.wrf().get_moad(): return None
        if atime is not None and  \
                within_dt_epsilon(atime,self.wrf().simstart(),self.
                                  dt_epsilon):
            return self.prod_wrfinput
        return None
    def wrfbdy_at_time(self,atime,domain=None):
        """Returns the wrfbdy file for the specified time and domain."""
        if domain and domain!=self.wrf().get_moad(): return None
        if atime is not None and  \
                within_dt_epsilon(atime,self.wrf().simstart(),
                                  self.dt_epsilon):
            return self.prod_wrfbdy
        return None
    def fort65_at_time(self,atime,domain=None):
        """Returns the fort.65 file for the specified time and domain."""
        if domain and domain!=self.wrf().get_moad(): return None
        if atime is not None and  \
                within_dt_epsilon(atime,self.wrf().simstart(),
                                  self.dt_epsilon):
            return self.prod_fort65
        return None
    def deliver_products(self):
        """Delivers products (FileProduct objects) that were
        identified by the make_products."""
        produtil.fileop.makedirs(os.path.dirname(
                self.prod_wrfinput.location))
        self.prod_wrfinput.deliver(frominfo='wrfinput_d01',keep=False)
        self.prod_wrfbdy.deliver(frominfo='wrfbdy_d01',keep=False)
        self.prod_log.deliver(frominfo='rsl.out.0000',keep=False)
        self.prod_fort65.deliver(frominfo='fort.65',keep=False)
    def products(self):
        """Iterates over all of this Task's products (FileProduct
        objects created by make_products."""
        yield self.prod_wrfinput
        yield self.prod_wrfbdy
        yield self.prod_log
        yield self.prod_fort65
    def make_namelist(self,filename='namelist.input',logger=None):
        """Writes the namelist for real.  This does the same as its
        parent class's implementation, except that the
        num_metgrid_levels is also overridden to match whatever WPS
        actually created (which may not match the original
        namelist)."""
        if logger is None: logger=self.log()
        exepath=self.getexe('hwrf_metgrid_levels')
        metfile=None
        for x in glob.glob('met_nmm.d01*.nc'):
            metfile=x
            self.wrf().set_metgrid_levels_from(exepath,metfile,logger)
            break
        if metfile is None:
            raise RealNMMError(
                'Could not find a met_nmm.d01*.nc file for running '
                'hwrf_metgrid_levels.')
        super(RealNMM,self).make_namelist(filename,logger)

########################################################################
anl_fudge_factor=fractions.Fraction(2,3) # for tricking WRF into
                                         # outputting child domains'
                                         # data
class WRFAnl(WRFTaskBase):
    """This class runs a short WRF simulation to generate wrfanl files"""
    def make_wrf(self,wrf):
        """Creates a copy of the specified WRFSimulation wrf.  The
        copy has a simulation length of one minute or one physics
        timestep, whichever is larger.  Calls the _set_wrf_proc_config
        to set I/O server and compute grid dimensions based on this
        HWRFTask's config section.  Configures the WRF to produce
        wrfanl files for all domains."""
        wrf=wrf.copy()
        self._set_wrf_proc_config(wrf)

        # Determine the length of a physics timestep:
        MOAD=wrf.get_moad()
        endtime=to_fraction(MOAD.nl.nl_get('physics','nphs')) * MOAD.dt
        floored=max(60.0,float(math.floor(endtime*anl_fudge_factor)))
        assert(floored>0)
        # Set the simulation end time and history output frequency to
        # the physics timestep:
        wrf.analysis_out()
        wrf.set_timing(end=floored)
        assert(wrf.simstart()<wrf.simend())
        wrf.add_output('history',step=floored)
        return wrf
    @property
    def anltime(self):
        """The time for which analysis files are generated."""
        return self.wrf().simstart()
    def make_products(self):
        """Creates FileProduct objects for all wrfanl files."""
        self._products=dict()
        MOAD=self.wrf().get_moad()
        for domain in self.wrf():
            if domain==MOAD: continue
            pname=self.wrf().analysis_name(domain)
            loc=os.path.join(self.outdir,pname)
            self._products[domain]=FileProduct(self.dstore,pname,
                self.taskname,location=loc)
    def get_wrfanl(self,domain):
        """Returns the wrfanl file for the specified domain,
        regardless of analysis time.""" 
        if domain is None: return None
        if not domain in self.sim: return None
        domain=self.sim[domain]
        if domain.is_moad(): return None
        return self._products[domain]
    def wrfanl_at_time(self,atime,domain):
        """Returns the wrfanl file for the specified domain and
        analysis time, if one exists, and otherwise, None."""
        if atime!=self.anltime: return None
        if domain not in self.wrf(): return None
        d=self.wrf()[domain]
        if d.is_moad() :return None
        return self._products[d]
    def deliver_products(self):
        """Delivers the products (FileProduct objects) that were
        identified by make_products."""
        logger=self.log()
        logger.info('%s: make directory'%(self.outdir,))
        first=True
        for domain in self.wrf():
            if domain not in self._products: continue # skip MOAD
            p=self._products[domain]
            if first:
                first=False
                produtil.fileop.makedirs(os.path.dirname(p.location))
            p.deliver(frominfo=os.path.basename(p.location),logger=logger,
                      keep=False)
    def products(self,domain=None):
        """Iterates over all Products (FileProduct objects) that were
        identified by make_products."""
        if domain:
            if self._products.has_key(domain):
                yield self._products[domain]
        else:
            for domain in self.wrf():
                if self._products.has_key(domain):
                    yield self._products[domain]

########################################################################
class WRFGhost(WRFAnl):
    """This class runs a short WRF simulation to generate wrfanl
    files, but names them "ghost" instead of "wrfanl".  Also, history
    output is disabled."""
    def make_wrf(self,wrf):
        """Creates a copy of the WRFSimulation object wrf.  This first
        calls the WRFAnl.make_wrf, and then disables the history
        stream."""
        wrf=super(WRFGhost,self).make_wrf(wrf)
        wrf.set_wrfanl_outname('ghost_d<domain>')
        wrf.add_output('history',step=3600*3,end=9*3600,start=3*3600)
        return wrf
    def get_ghost(self,domain):
        """Same as get_wrfanl."""
        if domain is None: return None
        if not domain in self.sim: return None
        domain=self.sim[domain]
        if domain.is_moad(): return None
        return self._products[domain]

########################################################################
JUST_MOAD=object()
ALL_DOMS=object()
class WRFAnl4Trak(WRFAnl):
    """This subtask of WRFAnl modifies the time of the 1 minute
    forecast wrfout file from the outer domain to have the analysis
    time instead so that the tracker can be run to get the initial
    storm location."""
    def __init__(self,dstore,conf,section,wrf,trakdoms=JUST_MOAD,
                 trakname='trankin_d<domain>',**kwargs):
        self._trakdoms=trakdoms
        self._trackin_name_pattern=trakname
        super(WRFAnl4Trak,self).__init__(dstore,conf,section,wrf,**kwargs)

    def track_domains(self):
        if self._trakdoms is JUST_MOAD:
            yield self.sim.get_moad()
        elif self._trakdoms is ALL_DOMS:
            for domain in self.sim: yield domain
        else:
            for domain in self._trakdoms: yield self.sim[domain]

    def make_products(self):
        """This make_products adds all products produced by
        WRFAnl.make_products, but adds a product for the outer domain
        one minute forecast history stream (wrfout) file called
        "trackin_d<domain>".  That file is intended to be used to
        generate the parent domain vortex information."""
        WRFAnl.make_products(self)
        
        sim=self.sim
        self.trackin_name=dict()
        self.trackin_prod=dict()
        trackin_name_pattern=self._trackin_name_pattern
        for domain in self.track_domains():
            idom=sim[domain].get_grid_id()
            name=hwrf.wrfbase.parse_wrf_outname(
                trackin_name_pattern,idom,
                sim.simstart(),sim.get_nocolons())
            self.trackin_name[domain]=name
            loc=os.path.join(self.outdir,name)
            prod=FileProduct(self.dstore,name,self.taskname,location=loc)
            prod.location=loc
            prod['stream']='history'
            self.trackin_prod[domain]=prod

    def postrun(self):
        """Calls retime_wrfout for all domains whose trackin_d0X files
        are requested.  This produces the modified 1 minute forecast
        wrfout files that lie and say they're from the analysis
        time."""
        for domain in self.track_domains():
            self.retime_wrfout(domain)

    def retime_wrfout(self,domain):
        """If possible, modifies the stated output time in the one
        minute forecast trackin_d<domain> file to the analysis time.
        Does this for one domain.  For intio files, it does not modify
        the file, but instead simply renames it.  That is done
        because, at last test, the post does not actually look in an
        intio wrfout file for the time, so no modification is
        necessary."""
        stream='history'
        logger=self.log()
        sim=self.wrf()
        dom=sim[domain]
        name=self.trackin_name[domain]
        logger.info('simend = '+repr(self.wrf().simend())+' = '+str(self.wrf().simend()))
        infile=dom.get_output(stream,self.wrf().simend(),logger=logger)
        logger.info('infile = '+str(infile.path()))
        io_form=sim.io_form_for(stream)%100
        if io_form == 1:
            logger.warning('%s: renaming instead of running wrfout_newtime '
                           'since file is (probably) not NetCDF: '
                           'io_form=%d'%(infile,io_form))
            os.rename(infile.path(),name)
            return
        try:
            shutil.copy2(infile.path(),name)
            checkrun(bigexe(self.getexe('wrfout_newtime'))[name,
                sim.simstart().strftime('%Y-%m-%d_%H:%M:%S')])
        except Exception as e:
            logger.error('%s (from %s): unable to modify time in NetCDF '
                         'file: %s'%(infile.path(), name, str(e)))
            raise

    def deliver_products(self):
        """Delivers all products (FileProduct objects) created by
        make_products, including the new trackin_d<domain> added by
        this subclass, and all products added by the superclass
        WRFAnl."""
        super(WRFAnl4Trak,self).deliver_products()
        for d,p in self.trackin_prod.iteritems():
            p.deliver(frominfo=self.trackin_name[d],
                      logger=self.log(),keep=False)

    def get_trackin(self,domain=None):
        """Returns the trackin file.  If a domain is specified,
        returns the trackin file for that domain."""
        if domain is None: 
            domain=self.sim.get_moad()
        if domain in self.trackin_prod:
            return self.trackin_prod[domain]
        return None

    def products(self,domain=None,domains=None,time=None,stream=None):
        """Iterates over all products from this task.  This class adds
        the trackin_d0* files."""
        if not domains and not time and not stream:
            for p in WRFAnl.products(self,domain=domain): yield p

        if stream and stream!='history': return
        if time and time!=self.wrf().simstart(): return

        for d,p in self.trackin_prod.iteritems():
            simd=self.sim[d]
            if domains and simd not in domains: continue
            if domain is not None and domain!=simd: continue
            yield p

########################################################################
class WRFGhostForPost(WRFAnl4Trak):
    """This class generates wrfghost files, and wrfout files, for each
    domain.  The wrfout files happen at the end of a roughly one
    minute forecast so that they can be correctly post-processed.
    However, their internal timestamp has been changed to be for the
    analysis time.  This class derives from WRFAnl4Trak instead of
    WRFGhost to reuse the wrfout renamer functionality, but it may be
    used in place of a WRFGhost object."""
    def __init__(self,dstore,conf,section,wrf,trakdoms=ALL_DOMS,
                 trakname='ghout_d<domain>',**kwargs):
        super(WRFGhostForPost,self).__init__(
            dstore,conf,section,wrf,trakdoms,trakname,**kwargs)
    def make_wrf(self,wrf):
        """Creates a WRFSimulation that calls its wrfanl files "ghost"
        files instead."""
        wrf=super(WRFGhostForPost,self).make_wrf(wrf)
        wrf.set_wrfanl_outname('ghost_d<domain>')
        return wrf

    def get_ghout(self,domain):
        """Returns the ghost wrfout file for the specified domain."""
        assert(domain is not None)
        return self.get_trackin(domain)

    def get_ghost(self,domain):
        """Same as get_wrfanl."""
        if domain is None: return None
        if not domain in self.sim: return None
        domain=self.sim[domain]
        if domain.is_moad(): return None
        return self._products[domain]

########################################################################
class WRFAtmos(WRFTaskBase):
    """This class runs an atmosphere-only WRF run, using wrfanl files
    from a prior WRFAnl simulation.  It encapsulates an
    ExternalWRFTask, which performs the actual tracking of products.
    This allows the post-processors and wrf copy tasks to keep track
    of the model's output while the model is running.  That subtask
    shows up as ".mon" (for "monitor") relative to this task (so if
    this task is wrfatmos, then the external task is wrfatmos.mon)."""
    def __init__(self,dstore,conf,section,wrf,keeprun=True,
                 wrfdiag_stream='auxhist1',**kwargs):
        """Creates a new WRFAtmos:
           dstore - the produtil.datastore.Datastore object
           conf - the HWRFConfig object for configuration information
           section - the section name within that HWRFConfig object
           wrf - the WRFSimulation that is to be executed
           keeprun - True means the output directory should NOT be deleted
           wrfdiag_stream - stream that generates wrfdiag files
           kwargs - passed to the parent class constructor."""
        self._wrfdiag_stream=wrfdiag_stream
        # Create the WRFTaskBase first since it creates a copy of the
        # wrf, modifying the namelist in various ways:
        super(WRFAtmos,self).__init__(dstore,conf,section,wrf,
                                      keeprun=keeprun,**kwargs)
        # Create an ExternalWRFTask object to handle actual product
        # generation:
        self.__ex=hwrf.wrf.ExternalWRFTask(dstore=self.dstore,conf=self.conf,
            section=self.section,wrf=self.wrf(),
            taskname=self.taskname+'.mon',
            location=self.workdir,outdir=self.workdir)
        self.__ex['outdir']=self.workdir # make sure it is set even on rerun
    def make_wrf(self,wrf):
        """Runs the superclass WRFAtmos.make_wrf.  Instructs the
        resulting WRFSimulation to take analysis files as input, and
        calls the _set_wrf_proc_config to set I/O server and compute
        grid dimensions."""
        wrf=super(WRFAtmos,self).make_wrf(wrf)
        wrf.analysis_in()
        logger=self.log()
        self._set_wrf_proc_config(wrf,logger)
        return wrf
    def unrun(self): 
        """Deletes output files.  See the
        hwrf.wrf.ExternalWRFSimulation.unrun for details."""
        self.__ex.unrun()
    def run(self):
        """Runs the WRF."""
        self.state=UNSTARTED
        self.__ex.state=UNSTARTED
        super(WRFAtmos,self).run()
    def products(self,**kwargs): 
        """Iterates over all WRF products.  See the
        hwrf.wrf.ExternalWRFSimulation.products for details."""
        for p in self.__ex.products(**kwargs): 
            yield p
    def exproducts(self,**kwargs): 
        """Synonym for products."""
        for p in self.__ex.products(**kwargs): 
            yield p
    def wrf_check(self,**kwargs): 
        """Checks the status of the WRF simulation.  Should only be
        used while the simulation is running.  This is intended to be
        run by jobs other than the WRF job, such as the
        post-processors, to monitor the WRF simulation as it
        progresses."""
        self.__ex.wrf_check(**kwargs)
    def run_exe(self,exename='wrf',not_allranks=False,runner=None,
                sleeptime=None):
        """Runs the WRF simulation.  The default sleeptime, if none is
        specified is 60 seconds rather than the usual 30.  Other
        options have the same meaning as the parent class."""
        if sleeptime is None:
            sleeptime=self.conffloat('sleeptime',60)
        super(WRFAtmos,self).run_exe(
            exename=exename,not_allranks=not_allranks,
            runner=runner,sleeptime=sleeptime)
    def update_state(self): 
        """Checks the state of the WRF simulation and copies that
        information to self.state in the produtil.datastore.Datastore.
        See hwrf.wrf.ExternalWRFTask for details."""
        with self.dstore.transaction() as t:
            self.__ex.update_state()
            self.state=self.__ex.state
    def deliver_products(self,*args,**kwargs): 
        """Has no effect.  This is present only because it is a
        requirement of the parent class.  No delivery is required
        because the products are all UpstreamFile objects, so the
        delivery state is set by the post-processors when calling the
        "check" function of each product."""

########################################################################
class AnalysisCycle(WRFGhost):
    """This class is similar to a WRFGhost run, except that it runs a
    longer simulation (typically 1-6 hours), and provides wrfghost and
    wrfinput files at the end of it.  It also requires wrfghost and
    wrfinput files as input.  Note that this implementation relies on
    the fact that wrfghost, wrfanl and restart files are all exactly
    the same file format (just different times and domains)."""
    def __init__(self,dstore,conf,section,wrf,simlen=None,
                 keeprun=False,**kwargs):
        if simlen is not None:
            simlen=to_timedelta(simlen)
        self.__simlen=simlen
        super(AnalysisCycle,self).__init__(dstore,conf,section,wrf,
                                           keeprun,**kwargs)

    def make_products(self):
        self._products=dict()
        MOAD=self.wrf().get_moad()
        logger=self.log()

        for domain in self.wrf():
            pname='wrfinput_d%02d'%(domain.get_grid_id(),)
            loc=os.path.join(self.outdir,pname)
            if domain==MOAD:
                self.prod_wrfinput=FileProduct(
                    self.dstore,pname,self.taskname,location=loc)
            else:
                self._products[domain]=FileProduct(
                    self.dstore,pname,self.taskname,location=loc)

    @property
    def anlouttime(self):
        """The time at end of the analysis cycle, at which the output
        wrfanl and wrfinput files are available."""
        return self.wrf().simend()

    @property
    def anlintime(self):
        """The time at beginning of the analysis cycle, at which the
        input wrfanl and wrfinput files must be provided."""
        return self.wrf().simstart()

    @property
    def simlen(self):
        """The requested length of the simulation.  Note that this may
        not be the same as the actual simulation length
        (self.anlouttime-self.anlintime) due to the model timestep."""
        if self.__simlen is None:
            self.__simlen=to_timedelta(self.confstr('simlen'))
        return self.__simlen

    def make_wrf(self,wrf):
        """Called from the constructor.  Generates and returns a new
        WRFSimulation object that meets these requirements:
        1. Read in wrfanl and wrfinput files
        2. Output a restart and wrfinput file after self.simlen time
        3. Disable history and auxhist 1-3."""

        wrf=wrf.copy()

        # Read in analysis files:
        wrf.analysis_in()

        # Get the simulation length that was requested in either the
        # constructor or conf section:
        simlen=to_timedelta(self.simlen)

        # Change the simulation end time to the specified time:
        wrf.set_timing(end=simlen)

        # Output a restart file at the end of the simulation:
        wrf.add_output('restart',step=simlen,start=simlen,end=simlen)

        # Output an input file at the end of the simulation.  The rest
        # of this code assumes the file is written only once, with
        # only one output frame.
        wrf.add_output('inputout',step=simlen,start=simlen,end=simlen,
                       outname='wrfinputout_d<domain>')
        input_outname=wrf.nl.nl_get('time_control','input_outname')
        assert(input_outname=='wrfinputout_d<domain>')

        for domain in wrf:
            assert(domain.has_output('inputout'))
            assert(domain.has_output('restart'))

        # Make the domain not move by setting movemin to a huge value
        for domain in wrf:
            domain.nl.nl_set('physics','movemin',1000)

        # Disable output for four streams by moving the stream output
        # start time to after the end of the simulation.  This is to
        # work around buggy code that wants the WRF history stream
        # frequency to decide how to accumulate certain variables.
        logger=self.log()
        if not wrf.has_output('history'):
            wrf.add_output('history',start=simlen*2,step=simlen,end=simlen*3)
        for stream in [ 'auxhist1', 'auxhist2', 'auxhist3', 'history' ]:
            io_form=wrf.io_form_for(stream)
            for domain in wrf:
                if domain.has_output(stream):
                    domain.hide_output(stream)

        return wrf
    def wrfinput_at_time(self,atime=None,domain=None):
        """Returns the wrfinput file for the specified time and
        domain.  Returns None if that time and domain are not
        valid."""
        if domain and domain!=self.wrf().get_moad(): return None
        if atime is not None and  \
                within_dt_epsilon(atime,self.anltime,self.dt_epsilon):
            return self.prod_wrfinput
        return None
    def get_wrfinput(self):
        """Returns the output wrfinput file regardless of the time or
        domain"""
        return self.prod_wrfinput
    def deliver_products(self):
        MOAD=self.wrf().get_moad()
        logger=self.log()
        when=self.anlouttime

        logger.info('Deliver products: rst and wrfinput at time %s'%(repr(when),))
        produtil.fileop.makedirs(self.outdir,logger=logger)

        for domain in self.wrf():
            pname='wrfinput_d%02d'%(domain.get_grid_id(),)
            loc=os.path.join(self.outdir,pname)
            if domain==MOAD:
                fromloc=domain.get_output('inputout',when,logger=logger).path()
                logger.info('Deliver moad %s from %s'%(str(domain),fromloc))
                self.prod_wrfinput.deliver(
                    frominfo=fromloc,logger=logger,keep=False)
            else:
                fromloc=domain.get_output('restart',when,logger=logger).path()
                logger.info('Deliver nest %s from %s'%(str(domain),fromloc))
                self._products[domain].deliver(
                    frominfo=fromloc,logger=logger,keep=False)
    def products(self,domain=None):
        if domain is None:
            yield self.prod_wrfinput
        elif domain==self.wrf().get_moad():
            yield self.prod_wrfinput
            return
        elif domain is None:
            yield self.prod_wrfinput
        for p in super(AnalysisCycle,self).products(domain):
            yield p
