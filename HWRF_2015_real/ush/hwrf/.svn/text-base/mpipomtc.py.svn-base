"""This module handles the POM-coupled WRF simulation.  It contains
two critical pieces:

  POMInit -- an HWRFTask that is a wrapper around the Python pom package.
  WRFCoupledPOM - a subclass of hwrf.fcsttask.WRFAtmos that runs the
    WRF-POM two-way coupled system based on the output of the POMInit."""

__all__ = ['POMInit', 'WRFCoupledPOM']

import os, shutil, math
import produtil.datastore, produtil.fileop, produtil.cd, produtil.run
import produtil.rusage
import hwrf.hwrftask, hwrf.numerics, hwrf.exceptions, hwrf.fcsttask
import pom.master, pom.exceptions

from produtil.rusage import setrlimit, rusage, getrlimit
from produtil.datastore import UpstreamFile, wait_for_products, \
    COMPLETED, RUNNING, FAILED
from produtil.fileop import isnonempty, make_symlink, deliver_file
from produtil.cd import NamedDir
from produtil.run import mpirun, mpi
from hwrf.numerics import to_datetime, to_datetime_rel, to_fraction
from hwrf.exceptions import OceanInitFailed
from pom.exceptions import POMInputError

prodnames={ 'grid':           ( '{oceandir}/{vit[stormname]}.grid.nc',  
                                '{outdir}/{out_prefix}.pom.grid.nc'),
            'ts_initial':     ( '{oceandir}/{vit[stormname]}.ts_initial.nc', 
                                '{outdir}/{out_prefix}.pom.ts_initial.nc' ),
            'ts_clim':        ( '{oceandir}/{vit[stormname]}.ts_clim.nc',    
                                '{outdir}/{out_prefix}.pom.ts_clim.nc' ),
            'uv_initial':     ( '{oceandir}/{vit[stormname]}.uv_initial.nc', 
                                '{outdir}/{out_prefix}.pom.uv_initial.nc' ),
            'el_initial':     ( '{oceandir}/{vit[stormname]}.el_initial.nc', 
                                '{outdir}/{out_prefix}.pom.el_initial.nc' ),
            'restart.phase2': ( '{oceandir}/restart.phase2.nc',         
                                '{outdir}/{out_prefix}.pom.restart.phse2.nc'
                                ),
            'pom.nml':        ( '{nmldir}/pom.nml', 
                                '{outdir}/{out_prefix}.pom.nml' ) }
"""A mapping from product name to a two-element tuple.  The tuple
contains the path to the file in the local directory structure of the
pom package, and the destination file within the HWRF system.  Both
should be sent through string interpolation (strinterp or
timestrinterp) before use."""

class POMInit(hwrf.hwrftask.HWRFTask):
    """This HWRFTask subclass is a wrapper around the pom package.  It
    runs the POM initialization, and generates the POM namelist for
    the forecast run."""
    def __init__(self,dstore,conf,section,taskname=None,vitfile=None,
                 fcstlen=None,outstep=86400,**kwargs):
        """Creates a POMInit.
          dstore - the produtil.datastore.Datastore to use
          conf - the HWRFConfig to use
          section - the section name for this task
          taskname - the task name.  Default: section
          vitfile - the vitals file with tcvitals for all times this
            storm has existed.  Default:
            self.icstr('{WORKhwrf}/{stormlabel}.vitals')
        Other keyword arguments are passed to the superclass constructor."""
        if 'outdir' not in kwargs: outdir=conf.getdir('com')
        super(POMInit,self).__init__(dstore,conf,section,taskname=taskname,
                                     **kwargs)
        self._sfc_dataset = str(kwargs.get('sfc_dataset',self.confstr(
                    'sfc_dataset','hwrfdata')))
        self._loop_dataset = str(kwargs.get('loop_dataset',self.confstr(
                    'loop_dataset','hwrfdata')))
        self._sfcanl_item = str(kwargs.get('sfcanl_item',self.confstr(
                    'sfcanl_item','gfs_sfcanl')))
        self._sanl_item = str(kwargs.get('sanl_item',self.confstr(
                    'sanl_item','gfs_sanl')))
        self._loop_item = str(kwargs.get('loop_item',self.confstr(
                    'loop_item','gfdl_loop')))
        self._wc_ring_item = str(kwargs.get('wc_ring_item',self.confstr(
                    'wc_ring_item','gfdl_wc_ring')))
        self._atime=to_datetime(conf.cycle)
        self.__fcstlen=fcstlen
        self.__outstep=int(outstep)
        if self.__outstep<30: self.__outstep=86400
        if vitfile is None:
            vitfile=self.icstr('{WORKhwrf}/{stormlabel}.vitals')
        self._vitfile=vitfile
        if 'catalog' in kwargs and isinstance(kwargs['catalog'],
                                              hwrf.input.DataCatalog):
            self._catalog=kwargs['catalog']
        else:
            incat = str(kwargs.get('catalog',self.confstr(
                        'catalog','hwrfdata')))
            self._catalog=hwrf.input.DataCatalog(conf,incat,conf.cycle)

        self._products=dict()

        rundir=self.workdir
        outputdir=os.path.join(rundir,'output')
        self._make_products(outputdir)

    def run(self):
        """Runs the POM initialization and copies the results to their
        destinations within the HWRF work area."""
        try:
            self.state=RUNNING
            logger=self.log()
            rundir=self.workdir
            assert(rundir)
            inputdir=os.path.join(rundir,'input')
            outputdir=os.path.join(rundir,'output')
            if os.path.exists(rundir):
                shutil.rmtree(rundir)
            with NamedDir(rundir,keep=True,logger=logger) as d:
                with NamedDir(inputdir,keep=True,logger=logger) as d:
                    self.get_inputs()
                with NamedDir(outputdir,keep=True,logger=logger) as d:
                    self.run_init(inputdir,outputdir)
                    self.deliver_products(os.path.join(outputdir,'OCEAN'),
                                          outputdir)
            self.state=COMPLETED
        except pom.exceptions.POMUnsupportedBasin as ue:
            logger.info('Basin is unsupported.')
            self.state=COMPLETED
            raise # caller needs to handle this
        except Exception as e:
            logger.error('Unhandled exception in ocean init: %s'
                         %(str(e),),exc_info=True)
            self.state=FAILED
            raise

    def _make_products(self,outdir):
        """Creates FileProduct objects for all output files.  The
        outdir is the directory to which the pom package output its
        final files."""
        atime=self._atime
        oceandir=os.path.join(outdir,'OCEAN')
        with self.dstore.transaction():
            for prodname,filepaths in prodnames.iteritems():
                (localpath,compath)=filepaths
                prod=produtil.datastore.FileProduct(
                    self.dstore,prodname,self.taskname)
                prod.location=self.timestr(compath,atime,atime,
                                           outdir=self.outdir)
                slocalpath=self.timestr(localpath,atime,atime,
                                        oceandir=oceandir,nmldir=outdir)
                prod['localpath']=slocalpath
                self._products[prodname]=( prod,slocalpath )

    def deliver_products(self,oceandir,nmldir,redeliver=False):
        """Copies results to their destinations within the HWRF work areas
          oceandir - the OCEAN directory created by the pom package in run()
          nmldir - the directory in which the forecast namelist was made
          redeliver - if True, products are re-copied even if
            available=True"""
        assert(self._products)
        logger=self.log()
        good=True
        baddies=list()
        atime=self._atime
        produtil.fileop.makedirs(self.outdir,logger=logger)
        for prodname,stuff in self._products.iteritems():
            assert(isinstance(stuff,tuple))
            ( prod,localpath ) = stuff
            if prod.available and not redeliver:
                logger.info('%s: already available and redeliver=False, so '
                            'skipping this (available=%s location=%s)'%(
                        prod.did,repr(prod.available),repr(prod.location)))
                continue
            if not os.path.exists(localpath):
                logger.warning(
                    localpath+": expected output file does not exist.")
                good=False
                baddies.append(localpath+' (missing)')
                continue
            elif not isnonempty(localpath):
                logger.warning(localpath+": is empty.  Will deliver anyway."
                               +"  Beware of impending failures.")
                baddies.append(localpath+' (empty)')
            prod.deliver(frominfo=localpath,keep=False,logger=logger)
        if len(baddies)>0:
            msg='Some ocean outputs were empty or missing: '+\
                (', '.join(baddies))
            logger.warning(msg)
            if not good:
                logger.critical('Ocean init failed: '+msg)
                raise OceanInitFailed(msg)

    def run_init(self,inputdir,outputdir):
        """This internal implemenentation function passes control to
        the pom package.  This is part of the implementation of
        run().  Do not call directly except for debugging."""
        CENTERID=self.storminfo.center.upper()
        EXEChwrf=self.getdir('EXEChwrf')
        PARMhwrf=self.getdir('PARMhwrf')
        FIXhwrf=os.path.join(self.getdir('FIXhwrf'),'hwrf-pom')
        VITDIR=inputdir
        GFSDIR=inputdir
        LCDIR=inputdir
        CSTREAM=outputdir
        COMIN=self.getdir('com')
        STARTDATE=self._atime.strftime('%Y%m%d%H')
        STORMID=self.storminfo.stormid3.upper()
        STORMNAME=self.storminfo.stormname.upper()
        kwargs=dict(logger=self.log(), conf=self.conf)
        method=self.confstr('method','')
        if method: kwargs['input_method']=method.upper()
        assert(GFSDIR.find('pom/output')<0)
        logger=self.log()
        setrlimit(logger=logger,stack=6e9,ignore=True)
        getrlimit(logger=logger)
        pom.master.run_init(STORMNAME,STORMID,STARTDATE,EXEChwrf,PARMhwrf,
                            FIXhwrf,VITDIR,GFSDIR,LCDIR,CSTREAM,COMIN,
                            fcstlen=self.__fcstlen,outstep=self.__outstep,
                            **kwargs)

    def products(self,name=None):
        """Iterates over Product objects for all of the files that
        need to be copied to the forecast directory to run POM.  The
        products will all have a "localname" metadata value telling
        the local filename they should have in the forecast
        directory."""
        if name is None:
            for p in self._products.itervalues(): yield p[0]
        else:
            if name in self._products: yield self._products[name][0]

    def inputiter(self):
        yield dict(dataset=self._sfc_dataset,item=self._sanl_item,
                   atime=self._atime)
        yield dict(dataset=self._sfc_dataset,item=self._sfcanl_item,
                   atime=self._atime)
        # Get the basin as it will be passed to the pom package.  This
        # pair of odd lines exactly reproduces the process that is
        # used:
        STORMID=self.storminfo.stormid3.upper()
        BASIN=STORMID[2].upper()        
        if BASIN=='L':
            yield dict(dataset=self._loop_dataset,item=self._loop_item,
                       atime=self._atime)
            yield dict(dataset=self._loop_dataset,item=self._wc_ring_item,
                       atime=self._atime)

    def get_inputs(self):
        """Copies all inputs to locations expected by the pom package.
        Copies the GFS sanl and sfcanl, waiting for them if needed.
        Makes a new tcvitals file by parsing the old one, and
        generating a new one, discarding lines containing "INVEST"."""
        logger=self.log()
        atime=self._atime

        # Copy GFS sanl and sfcanl files (required)
        with self.dstore.transaction() as t:
            sanlx=self._catalog.locate(self._sfc_dataset,self._sanl_item,
                                       atime=atime,logger=logger)
            sfcanlx=self._catalog.locate(self._sfc_dataset,self._sfcanl_item,
                                         atime=atime,logger=logger)
           
            sanl=UpstreamFile(self.dstore,'input_sanl',self.taskname,
                              minsize=30000)
            sanl.location=sanlx
            sanl.available=False
            sfcanl=UpstreamFile(self.dstore,'input_sfcanl',self.taskname,
                                minsize=30000)
            sfcanl.location=sfcanlx
            sfcanl.available=False

        names={ sanl:self.timestr('gfs.t{aHH}z.sanl',0,atime=self._atime),
                sfcanl:self.timestr('gfs.t{aHH}z.sfcanl',0,
                                    atime=self._atime) }
        def namer(p,logger,*a): return names[p]
        def actor(p,name,logger,*a): make_symlink(p.location,name,
                                                  force=True,logger=logger)
        wait_for_products([sanl,sfcanl],logger,namer,actor)

        # Copy loop current (optional)
        maxback=max(1,self.confint('loop_current_max_days_back',30))
        bad=True
        for idelta in xrange(maxback):
            hdelta=idelta*24.0
            looptime=to_datetime_rel(hdelta,atime)
            stime=looptime.strftime('%Y%m%d%H')
            loop=self._catalog.locate(self._loop_dataset,self._loop_item,
                                      atime=looptime,logger=logger)
            wc_ring=self._catalog.locate(
                self._loop_dataset,self._wc_ring_item,atime=looptime,
                logger=logger)
            bad=False
            if not isnonempty(loop):
                bad=True
                logger.warning('%s (loop at time %s): is empty or '
                               'non-existant'%(str(loop),stime))
            if not isnonempty(wc_ring):
                bad=True
                logger.warning('%s (loop wc_ring at time %s): is empty or '
                               'non-existant'%(str(wc_ring),stime))
            if not bad: break
        if not bad:
            looploc=self.timestr('hwrf_gfdl_loop_current_rmy5.dat.{aYMD}',
                                 0,atime=self._atime)
            make_symlink(loop,looploc,logger=logger)
            wc_ringloc=self.timestr(
                'hwrf_gfdl_loop_current_wc_ring_rmy5.dat.{aYMD}',
                0,atime=self._atime)
            make_symlink(wc_ring,wc_ringloc,logger=logger)
        else:
            logger.critical('No loop current available.  Checked %d day(s) '
                            'for loop current for %s'
                            %(maxback,atime.strftime('%Y%m%d')))
    
        # Create tcvitals file, excluding INVEST lines
        vitdest='syndat_tcvitals.%04d'%(self.storminfo.when.year,)
        logger.info('Copy vitals %s to %s'%(self._vitfile,vitdest))
        with open(vitdest,'wt') as outf:
            with open(self._vitfile,'rt') as inf:
                for line in inf:
                    if line.find('INVEST')>=0:
                        continue
                    outf.write(line)

########################################################################        
class WRFCoupledPOM(hwrf.fcsttask.WRFAtmos):
    """Runs a WRF-POM coupled simulation.  Most of the work of this
    class is done by the superclass, WRFAtmos.  This class adds code
    to copy the inputs needed by POM and the coupler.  There are three
    critical new config section values:

        wm3c_ranks = number of coupler ranks.  Default: 1
        pom_ranks = number of POM ranks.  Default: 9
        wrf_ranks = nubmer of WRF ranks.  No default.  This one is
          mandatory."""
    def __init__(self,dstore,conf,section,wrf,keeprun=True,
                 wrfdiag_stream='auxhist1',pominit=None,**kwargs):
        assert(pominit is not None)
        super(WRFCoupledPOM,self).__init__(dstore,conf,section,wrf,keeprun,
                                           wrfdiag_stream,**kwargs)
        self._pominit=pominit

    def check_all_inputs(self,coupled=None):
        """Returns True if all inputs needed to run the forecast are
        present, and false otherwise.  If coupled=True (the default),
        then POM inputs are also checked, otherwise they are
        ignored."""
        self.__coupled=coupled
        logger=self.log()
        okay=super(WRFCoupledPOM,self).check_all_inputs()
        return okay

    def link_all_inputs(self,just_check=False,coupled=None):
        """Copies inputs required by all components of the coupled
        POM-WRF simulation"""
        logger=self.log()
        okay=super(WRFCoupledPOM,self).link_all_inputs(
            just_check=just_check)
        if just_check:
            if okay:
                logger.info('WRF inputs are present.')
            else:
                logger.error('FAIL: WRF inputs are missing.')
        if coupled is None: coupled=self.__coupled
        if not coupled:
            self.log().info('Not copying pom and coupler inputs because an '
                            'uncoupled simulation was requested.')
            return okay
        if not just_check:
            self.make_coupler_namelist('cpl_nml')
        if not self.copy_pom_inputs(just_check=just_check):
            logger.error('POM inputs are missing.')
            return False
        else:
            logger.info('POM inputs are all present.')
        return okay

    def copy_pom_inputs(self,just_check=False):
        """Copies the inputs required by the POM model.  If
        just_check=True, the inputs are not copied; instead, the
        routine just checks for them.  Do not use just_check: call
        check_inputs instead."""
        logger=self.log()
        logger.info('Copying POM inputs from POMInit task %s'
                    %(self._pominit.taskname,))
        n_copied=0
        for prod in self._pominit.products():
            assert(isinstance(prod,produtil.datastore.Product))
            if not prod.available: prod.check(logger=logger)
            localname=prod.meta('localpath','')
            avail=prod.available 
            loc=prod.location
            if not localname:
                msg='POM product %s (available=%s location=%s) has no '\
                    'localname.'%(prod.did,repr(avail),repr(loc))
                if just_check: 
                    logger.warning(msg)
                else:
                    logger.error(msg)
                    raise POMInputError(msg)
            if not avail:
                msg='POM product %s (available=%s location=%s localname=%s)'\
                    ' is not available.'\
                    %(prod.did,repr(avail),repr(loc),repr(localname))
                if just_check:
                    logger.warning(msg)
                    return False
                else:
                    logger.error(msg)
                    raise POMInputError(msg)
            if not loc:
                msg='POM product %s (available=%s location=%s localname=%s)'\
                    ' has no location.'\
                    %(prod.did,repr(avail),repr(loc),repr(localname))
                if just_check:
                    logger.warning(msg)
                    return False
                else:
                    logger.error(msg)
                    raise POMInputError(msg)
            if not just_check:
                deliver_file(loc,os.path.basename(localname),keep=True,
                             logger=logger)
            n_copied+=1
        if n_copied<1:
            msg='No outputs reported by POM initialization.'\
                '  Did you forget to run the ocean init?'
            if just_check:
                logger.warning(msg)
                return False
            else:
                logger.error(msg)
                raise POMInputError(msg)
        logger.info('Copied %d POM inputs'%(n_copied,))
        return True

    def products(self,domains=None,stream=None,time=None,**kwargs):
        """Iterates over all products, both atmosphere and ocean.
        Select ocean products by stream='ocean'."""
        if stream is None or stream=='ocean':
            if time is None:
                for prod in self._products:
                    yield prod
            else:
                try:
                    yield self._products[time]
                except (hwrf.exceptions.NoNearbyValues,KeyError) as e:
                    pass # no product at that time
        if stream is None or stream!='ocean':
            for p in self.exproducts(domains=domains,stream=stream,
                                     time=time,**kwargs):
                yield p

    def make_coupler_namelist(self,filename='cpl_nml'):
        """Makes the namelist for the NCEP Coupler"""
        logger=self.log()
        logger.info('section is %s'%(self.section,))
        
        cplsec=self.confstr('cpl_nml')
        dt_c=self.confint('dt_c')
        f_dt_c=to_fraction(dt_c)
        simlen=to_fraction(self.sim.simend()-self.sim.simstart())
        cstepmax=int(math.ceil(float(simlen/f_dt_c)))
        morevars=dict(cstepmax=cstepmax, dt_c=dt_c)
        
        logger.info('dt_c=%s f_dt_c=%s simlen=%s cstepmax=%s'%(
                repr(dt_c),repr(f_dt_c),repr(simlen),repr(cstepmax)))
        
        cplnml=hwrf.namelist.Conf2Namelist(
            conf=self.conf,section=cplsec,morevars=morevars)
        with open(filename,'wt') as cn:
            nmlstr=cplnml.make_namelist(morevars=morevars)
            cn.write(nmlstr)

    def run(self,coupled=True):
        """Runs the WRF-POM coupled simulation.  Sets the internal
        coupled vs. uncoupled flag to the specified value.  Default:
        True (coupled)."""
        self.__coupled=coupled
        super(WRFCoupledPOM,self).run()

    def run_exe(self):
        """Runs the MPI command for the coupled WRF-POM simulation.
        Do not call this directly: call self.run instead."""
        logger=self.log()
        setrlimit(logger=logger,stack=6e9,ignore=True)
        if self.__coupled:
            logger.info('Starting the coupled wrf simulation')
            wrfexe=self.getexe('wrf')
            pomexe=self.getexe('hwrf_ocean_fcst')
            cplexe=self.getexe('hwrf_wm3c')
            cplranks=self.confint('wm3c_ranks',1)
            pomranks=self.confint('pom_ranks',9)
            wrfranks=self.confint('wrf_ranks')
            cmd = mpirun( mpi(cplexe)*cplranks + 
                          mpi(pomexe)*pomranks +
                          mpi(wrfexe)*wrfranks ) \
                          > self.confstrinterp('{WORKhwrf}/cpl.out')
            assert(isnonempty('pom.nml'))
            assert(isnonempty('cpl_nml'))
            super(WRFCoupledPOM,self).run_exe('wrf',None,cmd)
        else:
            logger.info('Starting the uncoupled wrf simulation')
            super(WRFCoupledPOM,self).run_exe('wrf',False,None)

########################################################################

def unset_ocstatus(conf,logger=None):
    """Delete the ocean coupling status file.  If the logger is not
    specified, the ocstatus subdomain of the conf default logging
    domain is used."""
    if logger is None: logger=conf.log('ocstatus')
    for ocstat in ( 'ocstatus', 'ocstatus2' ):
        ocstat=conf.get('config',ocstat)
        ocstatfile=os.path.join(conf.getdir('com'),ocstat)
        produtil.fileop.remove_file(ocstatfile,info=True,logger=logger)

def set_ocstatus(conf,coupled_flag,logger=None):
    """Set RUN_COUPLED=YES (true) or =NO (false) depending on the
    value of coupled_flag.  If the logger is not specified, the
    ocstatus subdomain of the conf default logging domain is used."""
    if logger is None: logger=conf.log('ocstatus')
    coupled_flag=bool(coupled_flag)
    for onetwo in ( 'ocstatus', 'ocstatus2' ):
        ocstat=conf.get('config',onetwo)
        ocstatfile=os.path.join(conf.getdir('com'),ocstat)
        strflag='YES' if coupled_flag else 'NO'
        logger.info('Setting RUN_COUPLED=%s in ocean status file %s'%(
                strflag,ocstatfile))
        with open(ocstatfile,'wt') as f:
            f.write('RUN_COUPLED='+strflag+"\n")

def get_ocstatus(conf,logger=None):
    """Checks the ocean status file.  If the file does not exist or
    cannot be opened or read, then False is returned.  Otherwise, the
    file is scanned for RUN_COUPLED=YES or RUN_COUPLED=NO (case
    insensitive).  The last of those RUN_COUPLED lines is used:
    NO=return False, YES=return True.  If the logger is not specified,
    the ocstatus subdomain of the conf default logging domain is
    used."""

    if logger is None: logger=conf.log('ocstatus')
    ocstat=conf.get('config','ocstatus')
    ocstatfile=os.path.join(conf.getdir('com'),ocstat)
    ocean_success=None

    logger.info('%s: scan ocean status file for RUN_COUPLED=YES or NO'%(
            ocstatfile))

    try:
        with open(ocstatfile,'rt') as f:
            for line in f:
                if line.upper().find('RUN_COUPLED=YES')>=0:
                    ocean_success=True
                    logger.info(
                        'Ocean status file says: RUN_COUPLED=YES')
                elif line.upper().find('RUN_COUPLED=NO')>=0:
                    ocean_success=False
                    logger.warning(
                        'Ocean status file says: RUN_COUPLED=NO')
    except EnvironmentError as e:
        logger.error(
            'Error checking ocean status file: %s'
            %(str(e),),exc_info=True)
    except Exception as ee:
        logger.error(
            'Unhandled exception while checking ocean status file: %s'
            %(str(ee),),exc_info=True)
        raise

    if ocean_success is None:
        logger.warning('Could not scan ocean status file for RUN_COUPLED=YES'
                       ' or NO.  Assuming RUN_COUPLED=NO.')
        ocean_success=False
    elif ocean_success:
        logger.info(
            'RUN_COUPLED=YES: ocean status file says ocean init succeeded.')
    else:
        logger.warning(
            'RUN_COUPLED=NO: ocean status file says ocean init failed.')

    return ocean_success
    
