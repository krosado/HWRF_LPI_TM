#! /bin/env python

"""This script implements data delivery functionality specific to the
NCEP HWRF system.  It is run after everything except archiving, and it
copies the output of some prior jobs to the COM directory under
different names.  In the operational HWRF, this job is also supposed
to email the NOAA Senior Duty Meterologist the HWRF AFOS file.
However, that functionality is intentionally left out at the moment.

This job is written specifically for the 2014 and 2013 operational
HWRF systems.  It does recognize the run_gsi, run_ocean and
run_relocation options, but it will need to be modified to handle
other configurations.  However, this job is optional.  If you do not
run this job, the later archiving and graphics jobs will still run
correctly."""

import os, glob, sys, logging, math
import produtil.fileop, produtil.setup

import produtil.log, produtil.setup, produtil.datastore
from produtil.log import jlogger
import hwrf.mpipomtc, hwrf.tracker
from hwrf.ensda import read_ensda_flag_file
import hwrf_alerts

copier=None
"""The copier is a function that returns either None, or a callable
object suitable for passing into the deliver_file to compress while
copying.  It is actually just a pointer to hwrf_expt.wrfcopier.copier,
created in the "main" function after initializing the hwrf_expt
module."""

def deliver_multi(sourceglob,target,logger):
    count=0
    if sourceglob.find('*')>=0 or sourceglob.find('?')>=0:
        logger.info('Globbing for: %s'%(sourceglob,))
        for source in glob.glob(sourceglob):
            produtil.fileop.deliver_file(
                source,target,keep=True,
                logger=logger,copier=copier(source))
            count+=1
    else:
        produtil.fileop.deliver_file(
            sourceglob,target,keep=True,
            logger=logger,copier=copier(sourceglob))
        count=1
    return count

class Deliverer(object):
    def __init__(self,logger,conf):
        """Creates a new Deliverer with the specified logging.Logger
        and HWRFConfig."""
        self.__logger=logger
        self.__conf=conf
        self.__failures=0 # Count of the number of failed deliveries
        self.__morevars=dict() # dict of string format substitutions
    def log(self,sublog=None):
        """Gets the logging.Logger for this Deliverer.  If "sublog" is
        provided, a new logging.Logger is created for that subdomain
        of this Deliverer's logging domain."""
        if sublog is not None:
            return logging.getLogger(self.__logger.name+'.'+sublog)
        return self.__logger
    def __setitem__(self,key,value):
        """Sets a key in an internal dict of values used for string
        formatting."""
        self.__morevars[key]=value
    def __getitem__(self,key):
        """Gets a key from the hash of internal values used for string
        formatting."""
        return self.__morevars[key]
    @property
    def failures(self):
        """The number of failed deliveries."""
        return self.__failures
    def reset(self):
        """Resets the number of failures to zero."""
        self.__failures=0
    @property
    def conf(self):
        """The HWRFConfig object for this Deliverer."""
        return self.__conf
    def deliver_file(self,workfile,comfile=None,from_com=False,
                     optional=False,**kwargs):
        """Delivers the specified work area file to the specified com
        location.  OR, if from_com=True, it does the opposite: deliver
        from com to the workfile.  If the com location is not
        specified, a suitable default will be chosen.  If
        optional=True, deliver_file will ignore the file if it is
        missing.  Otherwise, a missing file counts as a failure in the
        internal failure counter.  Additional keyword arguments are
        sent to conf.strinterp.  If the workfile is a relative path,
        it is relative to the WORKhwrf directory.

        When delivering to COM (from_com=False, the default), the
        workfile can instead be a Product, in which case the
        Product.location and .available are checked for availability
        information.  When delivering from COM (from_com=True), the
        workfile must be a string path."""
        conf=self.conf
        logger=self.log()
        if kwargs:
            morevars=dict(self.__morevars)
            morevars.update(kwargs)
        else:
            morevars=self.__morevars
        if not from_com and isinstance(workfile,produtil.datastore.Product):
            with workfile.dstore.transaction() as t:
                loc=workfile.location
                av=workfile.available
                if loc is None or loc=='':
                    if not optional:
                        logger.error('%s: no location'%(workfile.did,))
                        self.__failures+=1
                        return False
                    else:
                        logger.warning('%s: no location'%(workfile.did,))
                        return
                elif not av:
                    if not optional:
                        logger.error('%s: not available (location=%s)'%(
                                workfile.did,workfile.location))
                        self.__failures+=1
                        return False
                    else:
                        logger.warning('%s: not available (location=%s)'%(
                                workfile.did,workfile.location))
                        return                        
                else:
                    workpath=workfile.location
        elif from_com:
            assert(isinstance(workfile,basestring))
            workpath=conf.strinterp('config',workfile,**morevars)
            logger.info('from_com workpath='+workpath)
        else:
            assert(isinstance(workfile,basestring))
            workpath=os.path.join(
                conf.getdir('WORKhwrf'),
                conf.strinterp('config',workfile,**morevars) )
            
        if comfile is None:
            if from_com:
                raise AssertionError('When copying from com, the comfile '
                                     'must be specified.')
            compath=os.path.join(
                conf.getdir('com'),
                conf.strinterp('config','{out_prefix}.{workbasename}',
                               workbasename=os.path.basename(workpath)))
        else:
            compath=os.path.join(
                conf.getdir('com'),
                conf.strinterp('config',comfile,**morevars) )
        logger.info('compath is '+compath)
        if from_com and optional and not os.path.exists(compath):
            logger.info('Optional file does not exist: '+repr(compath))
            return
        elif not from_com and optional and not os.path.exists(workpath):
            logger.info('Optional file does not exist: '+repr(workpath))
            return
        try:
            if from_com:
                frompath=compath
                topath=workpath
            else:
                frompath=workpath
                topath=compath
            logger.info('deliver %s => %s'%(frompath,topath))
            count=deliver_multi(frompath,topath,logger=logger)
        except EnvironmentError as e:
            if optional:
                logger.warning('%s: cannot deliver: %s'%(workpath,str(e)))
            else:
                logger.error('%s: cannot deliver: %s'%(workpath,str(e)),
                             exc_info=True)
                self.__failures+=1
            return False
        if count==0:
            if optional:
                logger.warning('%s: no files matched this glob.  This '
                               'file was optional, so I will continue.'
                               %(frompath,))
                return False
            else:
                logger.error('%s: no files matched this glob.'%(frompath,))
                self.__failures+=1
                return False
        return True

########################################################################

def main():
    import hwrf_expt
    hwrf_expt.init_module(make_ensemble_da=True)
    # Make sure DBN alerts and other such things are triggered:
    hwrf_alerts.add_nhc_alerts()
    hwrf_alerts.add_regrib_alerts()
    hwrf_alerts.add_wave_alerts()

    global copier
    copier=hwrf_expt.wrfcopier.compression_copier

    if 'NO' == os.environ.get('PARAFLAG','YES'):
        jlogger.info('Calling email_afos_to_sdm from output job to email the track.')
        afos=hwrf_expt.nhcp.product('afos')
        hwrf_alerts.email_afos_to_sdm(afos)
        jlogger.info('Done with email_afos_to_sdm.  Will now celebrate by delivering many things to COM.')

    conf=hwrf_expt.conf
    relocation=conf.getbool('config','run_relocation',True)
    coupled=conf.getbool('config','run_ocean',True)
    GSI=conf.getbool('config','run_gsi') 
    run_ensemble_da=conf.getbool('config','run_ensemble_da',False)
    extra_trackers=conf.getbool('config','extra_trackers',False) 
    fcstlen=conf.getint('config','forecast_length',126)

    logger=conf.log('output')

    if coupled and not hwrf.mpipomtc.get_ocstatus(conf,logger):
        coupled=False

    hwrf_expt.wrfcopier.run()

    D=Deliverer(logger,conf)
    D['wrfdir']=hwrf_expt.runwrf.workdir
    D.deliver_file('{WORKhwrf}/tmpvit','{out_prefix}.storm_vit')

    if GSI:
        D['gsi_d02']=hwrf_expt.gsi_d02.outdir
        if hwrf_expt.gsid03_flag:
            D['gsi_d03']=hwrf_expt.gsi_d03.outdir

    logger.info('WRF run directory is %s'%(repr(D['wrfdir']),))

    D.deliver_file('{WORKhwrf}/jlogfile',optional=True)

    d01=hwrf_expt.moad
    d02=hwrf_expt.storm1outer
    d03=hwrf_expt.storm1inner

    if coupled:
        D.deliver_file('{wrfdir}/MDstatus',optional=True)
        for ocrest in ('el_initial.nc', 'grid.nc', 'ts_clim.nc', 
                        'ts_initial.nc', 'uv_initial.nc' ):
            D.deliver_file('{wrfdir}/{vit[stormname]}.{ocrest}',
                           '{out_prefix}.pom.{ocrest}', ocrest=ocrest)

        for iday in xrange(int(math.floor(fcstlen/24.0+0.01))):
            ocrest="%04d.nc"%iday
            D.deliver_file('{wrfdir}/{vit[stormname]}.{ocrest}',
                           '{out_prefix}.pom.{ocrest}', ocrest=ocrest)

    logcount=0
    for ext in ('log','out','err'):
        globme=conf.getdir('WORKhwrf')+'/*.'+ext
        logger.info('Globbing for %s log files'%(globme,))
        for log in glob.glob(globme):
            logcount+=1
            D.deliver_file(log)
    logger.info('Found %d log file(s)'%(logcount,))

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    # Deliver GSI stuff next.
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    gsi_flag=conf.getbool('config','run_gsi') 
    gsiop=True
    if gsi_flag:
        gsiop=((not hwrf_expt.gsi_d02.completed) or
               (not hwrf_expt.gsi_d03.completed))
        if gsiop:
            logger.warning('GSI failed, so all GSI products are optional.')
        else:
            logger.info('GSI ran, so its products are mandatory.')
    if GSI:
        # Copy the original wrfinput file before DA:
        org_d01=hwrf_expt.gfs_init.realinit.wrfinput_at_time(
            hwrf_expt.cycle,d01)
        D.deliver_file(org_d01,'{out_prefix}.wrforg_d01',optional=gsiop)

    if GSI:
        # Get the FGAT initialization at the analysis time:
        ceninit=hwrf_expt.fgat_init.init_at_time(hwrf_expt.conf.cycle)

        # Copy the original wrfanl files before relocation:
        org_d02=ceninit.runwrfanl.wrfanl_at_time(hwrf_expt.conf.cycle,d02)
        org_d03=ceninit.runwrfanl.wrfanl_at_time(hwrf_expt.conf.cycle,d03)
        D.deliver_file(org_d02,'{out_prefix}.wrforg_d02',optional=gsiop)
        D.deliver_file(org_d03,'{out_prefix}.wrforg_d03',optional=gsiop)

        if relocation:
            # Copy the wrfanl files after relocation, but before GSI:
            ges_d02=ceninit.rstage3.wrfanl_at_time(hwrf_expt.conf.cycle,d02)
            ges_d03=ceninit.rstage3.wrfanl_at_time(hwrf_expt.conf.cycle,d03)
            D.deliver_file(ges_d02,'{out_prefix}.wrfges_d02',optional=gsiop)
            D.deliver_file(ges_d03,'{out_prefix}.wrfges_d03',optional=gsiop)

    # for domain in hwrf_expt.gfs_init.runwrfanl.sim:
    #     if not domain.is_moad():
    #         org_prod=hwrf_expt.gfs_init.runwrfanl.wrfanl_at_time(
    #             hwrf_expt.cycle,domain)
    #         D.deliver_file(org_prod,'{out_prefix}.wrfanl_d{gid:02d}_org',
    #                      gid=int(domain.get_grid_id()))

    if GSI:
        D.deliver_file('{gsi_d02}/satbias_out',
                       '{out_prefix}.gsi_cvs2.biascr',optional=gsiop)
        if hwrf_expt.gsid03_flag:
            D.deliver_file('{gsi_d03}/satbias_out',
                       '{out_prefix}.gsi_cvs3.biascr',optional=gsiop)

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    # Lastly, deliver the diag files
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    jlogger.info('Delivering wrfdiag files to com.')
    hwrf_expt.nhcp.deliver_wrfdiag()

    if D.failures>0:
        jlogger.critical('HWRF: unable to deliver %d non-optional products to com.'%int(D.failures))
        sys.exit(1)

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    # Deliver things to noscrub for non-NCO runs  # # # # # # # # # # # 
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    if conf.getbool('config','PARAFLAG'):
        logger.info('You are not NCO, so I will deliver files to noscrub.')
    else:
        logger.info('You are NCO so I will skip NOSCRUB deliveries.')

    D.reset()
    def fromcom(workpath,compath,optional=False):
        D.deliver_file(workpath,compath,from_com=True,optional=optional)

    def havedir(sdir):
        there=conf.get('dir',sdir,'NOPE')
        if there=='NOPE':
            return False
        produtil.fileop.makedirs(there)
        return True

    if havedir('outatcf'):
        fromcom('{outatcf}','{out_prefix}.trak.hwrf.atcfunix')
    if havedir('outdiag'):
        fromcom('{outdiag}','{out_prefix}.trak.hwrf.3hourly*')
        fromcom('{outdiag}','{out_prefix}*resolution',True)
        fromcom('{outdiag}','{out_prefix}*htcf*stats',True)
        fromcom('{outdiag}','{out_prefix}*htcf',True)
        fromcom('{outdiag}','a*.dat')
        fromcom('{outdiag}','{out_prefix}.stats.tpc',optional=True)
        if extra_trackers:
            fromcom('{outdiag}','{com}/{out_prefix}.trak.hwrfd01.atcfunix')
            fromcom('{outdiag}','{com}/{out_prefix}.trak.hwrfd02.atcfunix')
    if havedir('outships'):
        fromcom('{outships}','figures/*.txt',optional=True)
    if havedir('outstatus'):
        fromcom('{outstatus}','{WORKhwrf}/submit.out',optional=True)
        timings=conf.strinterp('config','{outstatus}/{out_prefix}.timings')
        inout=conf.strinterp('config','{WORKhwrf}/hwrf_*.out')
        with open(timings,'wt') as outf:
            for inoutfile in glob.glob(inout):
                if not os.path.exists(inoutfile):
                    logger.warning('%s: file does not exist; skipping'
                                   %(inoutfile,))
                with open(inoutfile,'rt') as inf:
                    for line in inf:
                        if line.find('TIMING')>=0:
                            print>>outf,line.rstrip()
    if havedir('outatcfcorrected'):
        inatcf=conf.strinterp('config',
                              '{com}/{out_prefix}.trak.hwrf.atcfunix')
        outatcf=conf.strinterp(
            'config','{outatcfcorrected}/{out_prefix}.trak.hwrf.atcfunix')
        hwrf.tracker.jtwc_rewrite(inatcf,outatcf,logger)

    ####################################################################

    # Create the "done file" if ensda is entirely disabled.  This is
    # used by the workflow layer to know when the cycle is entirely
    # complete, and can be deleted.

    # NOTE FOR FUTURE DEVELOPMENT: When the graphics are added to the
    # workflow, we will need to move the creation of this "done file"
    # to a later step, after the graphics.  The logical candidate
    # would be a new job whose purpose is to check the cycle's entire
    # workflow to make sure it is finished.
    make_done=True
    if run_ensemble_da:
        flag_file=conf.strinterp('tdrcheck','{tdr_flag_file}')
        try:
            ensda_flag=hwrf.ensda.read_ensda_flag_file(flag_file)
        except (EnvironmentError) as e:
            logger.error('%s: unable to get ensda_flag; assume False: %s'%(
                    flag_file,str(e)),exc_info=True)
            ensda_flag=False

        if ensda_flag:
            jlogger.info(
                'Not creating donefile: ensda_output will do it instead.')
            make_done=not ensda_flag
        else:
            jlogger.info('ensda disabled: make donefile now')

    if make_done:
        donefile=os.path.join(conf.strinterp(
                'config','{com}/{stormlabel}.done'))
        with open(donefile,'wt') as f:
            f.write('Cycle is complete.')

########################################################################

if __name__=='__main__':
    try:
        produtil.setup.setup()
        jlogger.info('hwrf_output is starting')
        main()
        jlogger.info('hwrf_output has completed')
    except Exception as e:
        jlogger.critical('hwrf_output is aborting: '+str(e),exc_info=True)
        sys.exit(2)
