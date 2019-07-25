"""This module contains utilities for doing various forms of
ensemble-based data assimilation."""

__all__=['DAEnsemble','FromPriorCycle','FromGFSENKF',
         'write_ensda_flag_file','read_ensda_flag_file',
         'CycleTDRCheck','AlwaysRunENSDA','enada_pre_object_for']

import os
import produtil.datastore, produtil.cd, produtil.fileop, produtil.log

import hwrf.hwrftask, hwrf.numerics, hwrf.prep, hwrf.fcsttask
import hwrf.input

from produtil.log import jlogger
from produtil.datastore import COMPLETED,UpstreamFile
from produtil.fileop import isnonempty, wait_for_files, make_symlink
from hwrf.numerics import to_datetime_rel, to_timedelta, TimeArray, to_datetime
from produtil.cd import NamedDir

class DAEnsemble(hwrf.hwrftask.HWRFTask):
    def __init__(self,dstore,conf,section,anlintime=None,
                 taskname=None,**kwargs):
        super(DAEnsemble,self).__init__(dstore,conf,section,taskname,
                                            **kwargs)

        if anlintime is None:
            anlintime=conf.cycle
        anlintime=to_datetime_rel(anlintime,conf.cycle)
        self.__anlintime=anlintime
        self.__memberids=set()

        cycling_interval=conf.getfloat('config','cycling_interval')*3600.0

        self.__tstep=to_timedelta(self.confstr('da_cycle',cycling_interval))
        assert(self.__tstep>to_timedelta(1800))
        endtime=to_datetime_rel(cycling_interval,conf.cycle)

        self.__members=TimeArray(conf.cycle,endtime-self.__tstep,
                                 self.__tstep,dict)
        self.__anlouttime=self.__members.lasttime

    @property
    def anlintime(self):
        """The time at the beginning of the first ensemble step."""
        return self.__anlintime

    @property
    def anlouttime(self):
        """The time at the end of the last ensemble step."""
        return self.__anlouttime

    @property
    def nmembers(self):
        """The number of members of the ensemble."""
        return len(self.__memberids)

    @property
    def nsteps(self):
        """The number of ensemble DA time steps."""
        return len(self.__members)

    @property
    def anlintimes(self):
        """Iterates over all ensemble analysis input times."""
        for t in self.__members.times():
            yield t

    @property
    def anlouttimes(self):
        """Iterates over all ensemble analysis output times."""
        first=True
        for t in self.__members.times():
            if first: 
                first=False
            else:
                yield t
        yield self.__anlouttime

    def member_ids(self):
        """Iterates over all member ids."""
        for memberid in self.__memberids:
            yield memberid

    def set_member(self,atime,enkfmem,task):
        """Tells member enkfmem to use the specified task to produce
        output whose input analysis time is atime"""
        self.__memberids.add(enkfmem)
        self.__members[atime][enkfmem]=task

    def members_at_time(self,atime):
        """Iterates over all members at the specified anlintime,
        yielding (id,member) tuples."""
        time=to_datetime_rel(atime,self.__anlintime)
        for (enkfmem,memstep) in self.__members[time].iteritems():
            yield (enkfmem,memstep)

    def members_at_anlouttime(self):
        """Iterates over all members at the final analysis time,
        yielding (id,member) tuples."""
        for (e,m) in self.members_at_time(self.__members.lasttime):
            yield e,m

    def steps_for_member(self,enkfmem):
        """Iterates over (time,EnsembleDAMemberStep) pairs for the
        specified member."""
        for (t,a) in self.__members.iteritems():
            yield t,a[enkfmem]

    def member(self,atime,enkfmem):
        """Returns the da cycle for member enkfmem at analysis input
        time atime."""
        return self.__members[atime][enkfmem]

    def inputiter(self):
        """Calls inputiter for all steps of all ENKF members."""
        for t,e in self.__members.iteritems():
            for m in e.itervalues():
                for d in m.inputiter():
                    yield d

    def dump(self):
        for (time,stuff) in self.__members.iteritems():
            for (enkfid,memberstep) in stuff.iteritems():
                print "self.__members[%s][%s]=%s"%(
                    repr(time),repr(enkfid),repr(memberstep))
        t=self.__members.lasttime
        print 'last time t is %s'%(repr(t),)
        for (enkfid,memberstep) in self.members_at_time(t):
            print 'self.__members[t][%s]=%s'%(
                repr(enkfid),repr(memberstep))

########################################################################

class FromPriorCycle(hwrf.hwrftask.HWRFTask):
    def __init__(self,dstore,conf,section,domains,enkfmem,anlouttime,
                 **kwargs):
        super(FromPriorCycle,self).__init__(dstore,conf,section,**kwargs)
        self.__domains=[ d for d in domains ]
        self.__enkfmem=int(enkfmem)
        self.__anlouttime=hwrf.numerics.to_datetime(anlouttime)
    def products(self,domains=None,**kwargs):
        if False: yield None # ensures this is an iterator
        if domains is None: domains=self.__domains
        assert(domains)
        for domain in domains:
            assert(domain in self.__domains)
            if domain in self.__domains:
                if domain.is_moad():
                    yield self.get_wrfinput(domain,self.__anlouttime)
                else:
                    yield self.get_wrfanl(domain,self.__anlouttime)
    def get_wrfinput(self,domain,atime):
        return self.get_product(domain,atime)
    def get_wrfanl(self,domain,atime):
        return self.get_product(domain,atime)
    def get_product(self,domain,atime):
        logger=self.log()
        atime=hwrf.numerics.to_datetime(atime)
        if not atime==self.__anlouttime:
            logger.info('Wrong atime: %s vs %s'
                        %(str(atime),str(self.__anlouttime)))
            return None
        if not domain in self.__domains:
            logger.info('Invalid domain: %s not in %s'%(
                    str(domain), ', '.join([str(x) for x in self.__domains])))
            return None
        if domain.is_moad():
            logger.info('Not returning MOAD domains.')
            return None
        loc=self.confstrinterp(
            '{oldcom}/{oldvit[stormnamelc]}{oldvit[stormid3lc]}.{oldvit[YMDH]}.'
            'ensda_{enkfid:03d}.wrfinput_d{domid:02d}',enkfid=self.__enkfmem,
            domid=int(domain.get_grid_id()))
        logger.info('Domain %s atime %s enkfmem %s loc %s'%(
                str(domain),str(atime),repr(self.__enkfmem),repr(loc)))
        uf=UpstreamFile(self.dstore,category=self.taskname,
                        prodname=os.path.basename(loc),
                        location=loc)
        uf.check()
        return uf
########################################################################

class FromGFSENKF(hwrf.hwrftask.HWRFTask):
    def __init__(self,dstore,conf,section,detinit,enkfmem,sim,
                 taskname=None,**kwargs):
        super(FromGFSENKF,self).__init__(dstore,conf,section,taskname,
                                         **kwargs)
        assert(isinstance(sim,hwrf.wrf.WRFSimulation))
        self.enkfmem=enkfmem
        self.make_wrf(detinit,sim)
        self.make_init(detinit)
        self.make_fcst(detinit)

    @property
    def anlintime(self):
        return self.__wrf.simstart()

    def make_wrf(self,detinit,sim):
        self.fcst=hwrf.fcsttask.AnalysisCycle(
            self.dstore,self.conf,self.confstr('fcsttask'),sim.copy(),
            taskname=self.taskname+'.fcst',outdir=self.outdir+'/fcst',
            workdir=self.workdir+'/fcst')
        self.__wrf=sim
        # WCOSS workaround:
        self.fcst.sim.set_active_io_form_to(2)
        
    def make_fcst(self,delinit):
        self.fcst\
            .add_geogrid(self.geogrid) \
            .add_metgrid(self.metgrid) \
            .add_fort65(self.realinit) \
            .add_wrfinput(self.realinit) \
            .add_wrfbdy(self.realinit)
        for domain in self.sim:
            if domain.is_moad(): continue
            self.fcst.add_wrfanl(self.wrfanl,domain)

    @property
    def sim(self):
        return self.__wrf

    def inputiter(self):
        for d in self.prep.inputiter(): yield d

    def make_init(self,detinit):

        self.geogrid=detinit.geogrid
        #self.realbdy=detinit.realinit
        self.metgrid=detinit.metgrid
        wrf=self.sim
        moad=wrf.get_moad()
        geodat=self.geogrid.geodat(moad)

        realsection=self.confstr('realinit')
        prepsection=self.confstr('prep_hybrid')

        self.prep=hwrf.prep.PrepHybrid(
            self.dstore,self.conf,prepsection,self.sim,geodat,
            atime=self.anlintime,taskname=self.taskname+'.prep',
            workdir=self.workdir+'/prep',outdir=self.outdir+'/prep')

        self.realinit=hwrf.fcsttask.RealNMM(
            self.dstore,self.conf,realsection,self.sim,
            taskname=self.taskname+'.real',keeprun=False,
            outdir=self.outdir+'/real',workdir=self.workdir+'/real') \
            .add_geogrid(self.geogrid)\
            .add_metgrid(self.metgrid)\
            .add_prep_hybrid(self.prep)
        # WCOSS workaround:
        self.realinit.sim.set_active_io_form_to(2)
            
        self.prep.tvset('enkfmem',self.enkfmem)

        # We use a WRFGhost instead of a WRFAnl since WRFGhost
        # disables the history stream.
        self.wrfanl=hwrf.fcsttask.WRFGhost(
            self.dstore,self.conf,realsection,self.sim,False,
            atime=self.anlintime,taskname=self.taskname+'.wrfanl',
            workdir=self.workdir+'/wrfanl',outdir=self.outdir+'/wrfanl')\
            .add_geogrid(self.geogrid) \
            .add_metgrid(self.metgrid) \
            .add_fort65(self.realinit) \
            .add_wrfinput(self.realinit) \
            .add_wrfbdy(self.realinit)

    def products(self,**kwargs):
        for p in self.fcst.products(**kwargs):
            yield p

    def run(self):
        where=self.workdir
        logger=self.log()
        logger.info('Run ensemble member in %s'%(where,))
        with produtil.cd.NamedDir(where,keep=False,keep_on_error=True,
                                  logger=logger):
            self.prep.run()
            self.realinit.run()
            self.wrfanl.run()
            self.fcst.run()
            self.state=COMPLETED

########################################################################
def write_ensda_flag_file(flag_file,run_ensda):
    """Writes the stormX.run_ensda flag file.  It contains a single
    line: RUN_ENSDA=YES or RUN_ENSDA=NO.  Will also log a message to
    the jlogfile at INFO level telling which was written."""
    if run_ensda is True:
        with open(flag_file,'wt') as f:
            f.write('RUN_ENSDA=YES\n')
        produtil.log.jlogger.info('Will run HWRF ENSDA for this cycle.')
    else: # None, False, anything else:
        with open(flag_file,'wt') as f:
            f.write('RUN_ENSDA=NO\n')
        produtil.log.jlogger.info('Disabled HWRF ENSDA for this cycle.')

def read_ensda_flag_file(flag_file):
    """Reads the stormX.run_ensda flag file, searching for a single
    line RUN_ENSDA=YES or RUN_ENSDA=NO.  Returns True for YES or False
    for No, based on the last such line seen.  Returns None otherwise."""
    run_ensda=None
    with open(flag_file,'rt') as f:
        for line in f:
            if line.find('RUN_ENSDA=YES')>=0:
                run_ensda=True
            elif line.find('RUN_ENSDA=NO')>=0:
                run_ensda=False
    return run_ensda

########################################################################
class CycleTDRCheck(hwrf.hwrftask.HWRFTask):
    """This class checks to see if a specified cycle has Tail Doppler
    Radar data available.  This is the condition used to decide
    whether to run the DA ensemble."""
    def __init__(self,dstore,conf,section,cycle_rel,**kwargs):
        """Create a new CycleTDRCheck which will look for TDR for the
        specified cycle.  The cycle_rel is anything accepted by
        to_datetime_rel's second argument."""
        super(CycleTDRCheck,self).__init__(dstore,conf,section,**kwargs)
        incat_name=self.confstr('catalog')
        self.__ensda_flag_file=self.confstr('tdr_flag_file')
        self.__run_ensda=None
        self.tgtcycle=to_datetime_rel(cycle_rel,conf.cycle)
        self.__in_catalog=hwrf.input.DataCatalog(
            self.conf,incat_name,self.tgtcycle)
        dataset=self.confstr('dataset','tdr')
        item=self.confstr('item','gdas1_bufr')
        obstype=self.confstr('obstype','tldplr')
        self.__tdrdict=dict(self.taskvars,dataset=dataset,item=item,
            obstype=obstype,atime=self.tgtcycle,ftime=self.tgtcycle,
            optional=True)
        self._dirname=self.workdir
        self._stormid='999'

    def should_run_ensda(self):
        """Should ENSDA be run?  If self.run was called in this
        process, returns the cached result of that.  Otherwise, reads
        the run_ensda flag file from COM."""
        if self.__run_ensda is None:
            self.__run_ensda=read_ensda_flag_file(self.__ensda_flag_file)
        return self.__run_ensda

    def inputiter(self):
        """This iterator's purpose is to provide "all files external
        to this workflow" that are needed by the task.  In this case,
        of course, it is only the TDR bufr_d file.  Hence, there is a
        single "yield" statement that requests the TDR."""
        yield self.__tdrdict

    def run(self):
        """Creates the storm1.run_ensda flag file with RUN_ENSDA=YES
        if the TDR data is available, and RUN_ENSDA=NO otherwise."""
        
        run_ensda=False

        try:
            if self._actually_run():
                run_ensda=True
        finally:
            self.write_flag_file(run_ensda)

    def write_flag_file(self,run_ensda):
        write_ensda_flag_file(self.__ensda_flag_file,run_ensda)

    def tdr_this_cycle(self):
        """check if TDR data is available for this cycle"""
        logger=self.log()
        atime=to_datetime(self.conf.cycle)
        self._in_catalog=hwrf.input.DataCatalog(
                         self.conf,self.confstr('catalog'),atime)
        item=self.conf.get('tdr_new_obstype','item')
        ds=os.path.join(self.getdir('intercom'),'bufrprep')
        it=self._in_catalog.parse(item,atime=atime,
                            logger=logger,obstype='tldplr')
        there=os.path.join(ds,it)
        logger.info('there is %s'%there)
        if isnonempty(there):
            return True
        else:
            return False 

    def _actually_run(self):
        """Do not call this routine directly; it is an internal
        implementation routine.  This routine contains the code that
        actually determines if TDR is present or not.  The "run"
        routine calls _actually_run in a try-finally block to ensure
        the run_ensda flag file is created no matter what."""
        logger=self.log()

        if self.realtime and os.path.isdir('/dcom/us007003'):
            if self.tdr_this_cycle():
                logger.info('TDR data is available for current cycle %s!'
                    'Enabling ENSDA.'%self.conf.cycle.strftime('%Y%m%d%H'))
                return True
            elif self.read_trigger_file():
                logger.info('There will be TDR data for cycle %s!'
                    'Enabling ENSDA.'%self.tgtcycle.strftime('%Y%m%d%H'))
                return True
            else:
                logger.warning('ensda trigger file is not found. '
                               'TDR data is not available for current cycle'
                               'Will continue without ENSDA.')
                return False
        else:
            ic=self.__in_catalog
            there_it_is=ic.locate(**self.__tdrdict)
            if there_it_is is None:
                logger.error(
                    'Configuration error: DataCatalog %s does not know how '
                    'to find TDR data.  Will continue without ENSDA.'
                    %(repr(ic),))
                return False
            elif not isnonempty(there_it_is):
                logger.warning(
                    '%s: %s Tail Doppler Radar bufr_d file is empty or '
                    'non-existant.  Will continue without ENSDA.'
                    %(there_it_is,self.tgtcycle.strftime('%Y%m%d%H')))
                return False
            else:
                logger.info('%s: TDR data found for cycle %s!  Enabling ENSDA.'
                             %(there_it_is,self.tgtcycle.strftime('%Y%m%d%H')))
                return True

    def read_trigger_file(self):
        """Read TDR trigger file for operational run"""
        logger=self.log()
        atime=to_datetime(self.tgtcycle)
        ymdh=atime.strftime('%Y%m%d%H')
        basin=self.storminfo.pubbasin2
        if int(ymdh[0:4]) <= 2013:
            self._stormid=self.storminfo.stnum
        elif basin.upper()=='AL':
            self._stormid='%s%02d' % ('1',self.storminfo.stnum)
        elif basin.upper()=='EP':
            self._stormid='%s%02d' % ('2',self.storminfo.stnum)
        elif basin.upper()=='CP':
            self._stormid='%s%02d' % ('3',self.storminfo.stnum)
        else:
            self._stormid='999'

        input_catalog=self.conf.get('config','fcst_catalog')
        dcom=self.conf.get(input_catalog,'dcom','/dcom/us007003')
        if os.path.isdir(dcom):
            btime=to_datetime_rel(-24*3600,atime)
            tank1=os.path.join(dcom,atime.strftime("%Y%m%d"),'b006/xx070')
            tank2=os.path.join(dcom,btime.strftime("%Y%m%d"),'b006/xx070')
            logger.info('tank1 is %s tank2 is %s'%(tank1,tank2)) 
            with NamedDir(self._dirname,keep=not self.scrub,logger=logger): 
                numtry=self.confint('numofcheck',4)
                timeinv=self.confint('checksecinv',600)
                stime=timeinv/2
                n=1
                while n<=numtry:
                    if isnonempty(tank1):
                        logger.info('tank1 exist')
                        make_symlink(tank1,'tldplrbufr',force=True,logger=logger)
                        self.readensdatrigger(self._stormid,ymdh)
                    if not isnonempty('runensda') and ymdh[8:10]=='00' \
                        and isnonempty(tank2):
                        make_symlink(tank2,'tldplrbufr',force=True,logger=logger)
                        self.readensdatrigger(self._stormid,ymdh) 
                    if n<numtry:
                        if wait_for_files(
                            'runensda',logger,
                            maxwait=timeinv,sleeptime=stime,min_size=1,
                            min_mtime_age=5,min_atime_age=None,
                            min_ctime_age=None,min_fraction=1.0):
                            logger.info('found trigger file')
                            n=numtry+1
                    n+=1                        
                if isnonempty('runensda'):    
                    return True
                else:
                    totalwait=timeinv*(numtry-1)/60.0
                    logger.info('waited for %s minunites, ensda trigger'
                                ' is not found'%str(totalwait))
                    return False
        else:
            logger.warning('%s does not exist. This is not wcoss.'
                        'real-time ensda trigger can only be run on wcoss'%dcom)
            return False

    def readensdatrigger(self,stmidin,tgtcycle):
        """Runs the hwrf_readtdrtrigger program."""
        self.log().info('readensdatrigger')
        logger=self.log()
        fprog = 'hwrf_readtdrtrigger'
        prog  = self.getexe(fprog)
        cmd = produtil.run.exe(prog) << stmidin+" "+tgtcycle 
        produtil.run.checkrun(cmd,logger=logger)

########################################################################
class AlwaysRunENSDA(CycleTDRCheck):
    """This subclass of CycleTDRCheck instructs ENSDA to run whether
    TDR is available or not."""
    def should_run_ensda(self):
        """Always returns True, indicating that ENSDA should always be
        run."""
        return True
    def _actually_run(self):
        """Calls the superclass _actually_run, so that the TDR check
        is done, and then returns True regardless of the TDR
        availability."""
        if not super(AlwaysRunENSDA,self)._actually_run():
            msg="OVERRIDE: Will run ENSDA anyway due to "\
                "configuration settings."
            self.log().warning(msg)
            jlogger.warning(msg)
        return True

########################################################################
def enada_pre_object_for(ds,conf,section,next_cycle):
    ensda_when=conf.getstr('config','ensda_when','tdr_next_cycle').lower()
    if ensda_when=='tdr_next_cycle':
        return hwrf.ensda.CycleTDRCheck(
            ds,conf,'tdrcheck',next_cycle)
    elif ensda_when=='always':
        return hwrf.ensda.AlwaysRunENSDA(
            ds,conf,'tdrcheck',next_cycle)
    else:
        raise ValueError('The ensda_when option must be set to tdr_next_cycle or always (case-insensitive) not %s.'%(repr(ensda_when),))
    
