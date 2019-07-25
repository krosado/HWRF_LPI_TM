"""This module is used to create the initial HWRF conf file in the
first HWRF job via the hwrf.launcher.launch.  The hwrf.launcher.load
then reloads that configuration.  The launch() function does more than
just create the conf file though.  It parses the tcvitals, creates
several initial files and directories and runs a sanity check on the
whole setup."""

__all__=['load','launch','HWRFLauncher','parse_launch_args']

import os, re, sys, collections, random
import produtil.fileop, produtil.run, produtil.log
import hwrf.revital, hwrf.storminfo, hwrf.numerics, hwrf.config, hwrf.input

from random import Random
from produtil.fileop import isnonempty
from produtil.run import run, exe
from produtil.log import jlogger
from hwrf.numerics import to_datetime_rel, to_datetime
from hwrf.config import HWRFConfig
from hwrf.exceptions import HWRFDirInsane,HWRFStormInsane,HWRFCycleInsane, \
    HWRFVariableInsane,HWRFInputInsane,HWRFScriptInsane,HWRFExecutableInsane,\
    HWRFFixInsane,HWRFArchiveInsane,HWRFConfigInsane

def parse_launch_args(args,logger,usage,PARMhwrf=None):
    """This is the argument parser for the exhwrf_launch.py and
    hwrf_driver.py scripts.  It parses the storm ID and later
    arguments (in args).  Earlier arguments are parsed by the scripts
    themselves.  If something goes wrong, this function calls
    sys.exit(1) or sys.exit(2).

    The arguments depend on if PARMhwrf=None or not.  

    If PARMhwrf is None:
      StormID CASE_ROOT /path/to/parm [options]
    Otherwise:
      StormID CASE_ROOT [options]

    StormID - three character storm identifier (ie.: 12L for Katrina)
    CASE_ROOT - HISTORY or FORECAST
    /path/to/parm - path to the parm directory, which contains the
      default conf files.

    Options:
    section.variable=value  -- set this value in this section, no matter what
    /path/to/file.conf -- read this conf file after the default conf files.

    Later conf files override earlier ones.  The conf files read in
    are:
      parm/hwrf_input.conf
      parm/hwrf.conf
      parm/hwrf_holdvars.conf
      parm/hwrf_basic.conf
      parm/system.conf """
    if len(args)<2 or ( PARMhwrf is None and len(args)<3):
        usage(logger=logger)
        sys.exit(2)

    # Get the storm ID:
    stid=args[0].upper()
    if not re.match('^[0-9][0-9][ABCELPQSW]$',stid):
        logger.error('%s: invalid storm id.  Must be a three character '
                     'storm ID such as 90L or 13W'%(stid,))
        sys.exit(2)

    # Get the case root (real-time vs. retrospective):
    case_root=args[1].upper()
    if case_root=='HISTORY':
        real_time=False
    elif case_root=='FORECAST':
        real_time=True
    else:
        logger.error('%s: invalid case root.  Must be HISTORY for '
                     'retrospective runs or FORECAST for real-time runs.'
                     %(case_root,))
        sys.exit(2)
    logger.info('Case root is '+repr(case_root))

    # Find the parm directory
    if PARMhwrf is None:
        parm=args[2]
        if not os.path.exists(parm):
            logger.error(parm+': parm directory does not exist')
            sys.exit(2)
        elif not os.path.isdir(parm):
            logger.error(parm+': parm directory is not a directory')
            sys.exit(2)
        logger.info('Scan %d optional arguments.'%(len(args)-2))
        args=args[3:]
    else:
        parm=PARMhwrf
        logger.info('Scan %d optional arguments.'%(len(args)-1))
        args=args[2:]
    parm=os.path.realpath(parm)

    # Standard conf files:
    infiles=[ os.path.join(parm,'hwrf_input.conf'),
              os.path.join(parm,'hwrf.conf'), 
              os.path.join(parm,'hwrf_holdvars.conf'), 
              os.path.join(parm,'hwrf_basic.conf'),
              os.path.join(parm,'system.conf')
              ]
    
    # Now look for any option and conf file arguments:
    bad=False
    moreopt=collections.defaultdict(dict)
    for iarg in xrange(len(args)):
        logger.info(args[iarg])
        m=re.match('''(?x)
          (?P<section>[a-zA-Z][a-zA-Z0-9_]*)
           \.(?P<option>[^=]+)
           =(?P<value>.*)$''',args[iarg])
        if m:
            logger.info('Set [%s] %s = %s'%(
                    m.group('section'),m.group('option'),
                    repr(m.group('value'))))
            moreopt[m.group('section')][m.group('option')]=m.group('value')
        elif os.path.exists(args[iarg]):
            logger.info('%s: read this conf file'%(args[iarg],))
            infiles.append(args[iarg])
        else:
            bad=True
            logger.error('%s: invalid argument.  Not an config option '
                         '(a.b=c) nor a conf file.'%(args[iarg],))
    if bad:
        sys.exit(2)

    for file in infiles:
        if not os.path.exists(file):
            logger.error(file+': conf file does not exist.')
            sys.exit(2)
        elif not os.path.isfile(file):
            logger.error(file+': conf file is not a regular file.')
            sys.exit(2)
        elif not produtil.fileop.isnonempty(file):
            logger.warning(
                    file+': conf file is empty.  Will continue anyway.')
        logger.info('Conf input: '+repr(file))
    return (case_root,parm,infiles,stid,moreopt)

def load(filename):
    """Creates an HWRFConfig object for an HWRF workflow that was
    previously initialized by hwrf.launcher.launch.  The only argument
    is the name of the config file produced by the launch command."""
    conf=HWRFLauncher()
    conf.read(filename)
    logger=conf.log()

    cycle=conf.cycle
    assert(cycle is not None)
    strcycle=cycle.strftime('%Y%m%d%H')
    logger.info('Running cycle: '+cycle.strftime('%Y%m%d%H'))
    
    WORKhwrf=conf.getdir('WORKhwrf')

    tmpvit=os.path.join(WORKhwrf,'tmpvit')
    logger.info(tmpvit+': read vitals for current cycle')
    with open(tmpvit,'rt') as f:
        syndat=hwrf.storminfo.parse_tcvitals(f,logger,raise_all=True)
        syndat=syndat[0]
    logger.info('Current cycle vitals: '+syndat.as_tcvitals())

    oldvit=os.path.join(WORKhwrf,'oldvit')
    logger.info(oldvit+': read vitals for prior cycle')
    with open(oldvit,'rt') as f:
        oldsyndat=hwrf.storminfo.parse_tcvitals(f,logger,raise_all=True)
        oldsyndat=oldsyndat[0]
    logger.info('Prior cycle vitals: '+oldsyndat.as_tcvitals())

    conf.set_storm(syndat,oldsyndat)

    return conf

def launch(file_list,cycle,stid,moreopt,case_root,init_dirs=True,
           prelaunch=None):
    """Initializes the directory structure for a new HWRF workflow.
    Returns the full path to the conf file that is created as a
    result.  That conf file should be passed in to the
    hwrf.launcher.load at the top of every job.

    file_list - a list of conf files to read
    cycle - the cycle to run; anything accepted by to_datetime
    stid - the three character storm identifier for the storm to run.
      For example, stid=11L is the eleventh storm of the season in the
      Atlantic basin.  Although this argument is optional, the single
      storm HWRF workflow will fail if stid is not provided.
    case_root - HISTORY for retrospective mode, FORECAST for real-time
    moreopt - a dict of dicts with additional options to set.  This
      maps section name to option to value.
    prelaunch - a function to call on the configuration before
      writing it to disk.  Takes as arguments: conf,logger,cycle
      Note that the logger or cycle may be None.  The conf is the
      configuration object that will be written.

    You can run this function in a special mode that just reads the
    conf file, without specifying a cycle, or making directories.  To
    do that, send cycle=None and init_dirs=False.  That mode is used
    by the script that prepares the rocoto XML file for a multi-cycle
    workflow.
    """
    for filename in file_list:
        if not isinstance(filename,basestring):
            raise TypeError('First input to hwrf.config.for_initial_job '
                            'must be a list of strings.')
    conf=HWRFLauncher()
    logger=conf.log()
    if cycle is not None:
        conf.cycle=to_datetime(cycle)
        logger.info('Caller wants to launch a %s run of cycle %s storm %s.'
                    %(case_root,conf.cycle.strftime('%Y%m%d%H'),stid))
    else:
        logger.info('Caller wants to launch a %s run of storm %s.'
                    %(case_root,stid))
    conf.add_section('holdvars')
    conf.set('holdvars','CASE_ROOT',case_root)
    conf.set('config','case_root',case_root)
    if case_root=='HISTORY':
        conf.set('config','fcsthist','hist')
        conf.set('config','realtime','false')
        hist=True
    else:
        conf.set('config','fcsthist','fcst')
        conf.set('config','realtime','true')
        hist=False

    for filename in file_list:
        logger.info("%s: parse this file"%(filename,))
        conf.read(filename)

    if not hist:
        input_catalog=conf.get('config','input_catalog','hwrfdata')
        if input_catalog=='hwrfdata':
            fcst_catalog=conf.get('config','fcst_catalog')
            conf.set('config','input_catalog',fcst_catalog)
            jlogger.info("FORECAST mode, so changing input_catalog to %s"
                         %(repr(fcst_catalog),))

    if moreopt is not None:
        for section,options in moreopt.iteritems():
            if not conf.has_section(section):
                conf.add_section(section)
            for option,value in options.iteritems():
                logger.info('Override: %s.%s=%s'
                            %(section,option,repr(value)))
                conf.set(section,option,value)
    conf.guess_default_values()
    cycling_interval=conf.getfloat('config','cycling_interval',6.0)
    cycling_interval=-abs(cycling_interval*3600.0)
    if cycle is not None:
        other_cycle=to_datetime_rel(cycling_interval,conf.cycle)

    if stid is not None and cycle is not None:
        revit=conf.read_tcvitals_and_messages(other_cycle=other_cycle)
        conf.gen_vitals(stid,cycling_interval,revit)

    if not init_dirs:
        return conf

    produtil.fileop.makedirs(conf.getdir('com'),logger=logger)
    produtil.fileop.makedirs(conf.getdir('WORKhwrf'),logger=logger)

    logger.info('Expand certain [dir] values to ensure availability '
                'before vitals parsing.')
    for var in ( 'WORKhwrf', 'HOMEhwrf', 'com' ):
        expand=conf.getstr('dir',var)
        logger.info('Replace [dir] %s with %s'%(var,expand))
        conf.set('dir',var,expand)

    dtcgsi=os.path.join(conf.getdir('HOMEhwrf'),'sorc/GSI')
    if os.path.exists(dtcgsi):
        if os.path.isdir(dtcgsi):
            logger.info('%s: community GSI is checked out, use  '
                        'community gsi fix files '%(dtcgsi,))
            conf.set('dir','FIXgsi',os.path.join(dtcgsi,'fix'))
          
    if stid is not None:
        conf.decide_domain_center()
        loc=conf.getdir('domlocfile')
        logger.info('%s: Writing domain center.'%(loc,))
        with open(loc,'wt') as f:
            f.write("%g\n%g\n"%(
                    conf.getfloat('config','domlat'),
                    conf.getfloat('config','domlon')))

    if prelaunch is not None:
        prelaunch(conf,logger,cycle)

    confloc=conf.getloc('CONFhwrf')
    logger.info('%s: write hwrf.conf here'%(confloc,))
    with open(confloc,'wt') as f:
        conf.write(f)

    with open(os.path.join(conf.getdir('WORKhwrf'),'PDY'),'wt') as f:
        f.write(conf.strinterp(
                'config','cyc={HH}\nPDY={YMD}\nYMDH={YMDH}\n'))

    return conf

class HWRFLauncher(HWRFConfig):
    def __init__(self,conf=None):
        super(HWRFLauncher,self).__init__(conf)
        self._cycle=None
    def storm_for_stormnum(self):
        """Returns the one letter basin, numeric storm ID and year for
        the specified storm number (1-10)."""

    def decide_domain_center(self,logger=None):
        """If the domain center is not already set in the [config]
        section domlat and domlon variables, decides the domain center
        using the hwrf.storminfo.StormInfo.hwrf_domain_center
        routine."""
        if logger is None: logger=self.log()
        if self.has_option('config','domlat') and \
                self.has_option('config','domlon'):
            cenla=self.getfloat('config','domlat')
            cenlo=self.getfloat('config','domlon')
            logger.info('Domain center is already set to lat=%g lon=%g'
                        %(cenla,cenlo))
            return
        (cenlo, cenla) = self.syndat.hwrf_domain_center(logger)
        self.set('config','domlat',cenla)
        self.set('config','domlon',cenlo)
        logger.info('Decided on domain center lat=%g lon=%g'%(cenla,cenlo))

    def choose_vitbase(self,storm_num=None):
        """Decides the location of the vitfile that should be read in
        by read_precleaned_vitfile.  Optionally, you can specify the
        storm number (1-10) of the storm whose vitals should be read
        in.  Otherwise, a reasonable guess will be made."""
        if storm_num is not None:
            storm_num=int(storm_num)
            vitfile=os.path.join(self.getdir('WORKhwrf'),
                'storm%d.vitals'%(storm_num,))
        else:
            stormlabel=self.getstr('config','stormlabel','storm1')
            vitfile=os.path.join(self.getdir('WORKhwrf'),
                '%s.vitals'%(stormlabel,))
        return vitfile

    def read_tcvitals_and_messages(self,vitdir=None,vitpattern=None,
            include_messages=True,other_cycle=None):
        """Reads in the tcvitals files for the current cycle and
        optionally another cycle, which may be in the same file.  Also
        reads in message files if requested.  Cleans the result up and
        returns it as an hwrf.revital.Revital object.

           vitdir - optional: the directory in which to find the tcvitals.
              Default: [dir] section syndat variable.

           vitpattern - optional: passed into strftime to generate the
              name of the vitals file within vitdir. Default: [conf]
              section vitpattern variable, or syndat_tcvitals.%Y if
              missing.

           include_messages - optional flag: if True, attempts to find
              the hurricane message files, and includes them in the
              list of files to read in.  Default: True.

           other_cycle - optional: another cycle whose vitals file
              should also be parsed.  This can be anything accepted by
              to_datetime_rel(...,self.cycle).  This is intended to
              allow year-crossing cycling, such as a January 1, 00:00
              UTC cycle that is a warm start off of a prior December
              31, 18:00 UTC cycle.  If the other_cycle's vitals file
              is the same as the one from self.cycle, then the file is
              only read once."""
        ENV=os.environ
        logger=self.log()
        inputs=list()
        if vitdir is None:        
            vitdir=self.getdir('syndat')
        if vitpattern is None:    
            vitpattern=self.getstr('config','vitpattern',
                                   'syndat_tcvitals.%Y')
        file1=os.path.join(vitdir,self._cycle.strftime(vitpattern))
        inputs.append(file1)
        if other_cycle is not None:
            other_cycle=to_datetime_rel(other_cycle,self._cycle)
            file2=os.path.join(vitdir,other_cycle.strftime(vitpattern))
            if file2!=file1: 
                inputs.append(file2)

        if include_messages:
            # Try to guess the location of the message files:
            mdir=self.getdir('mesagdir','')
            if mdir is None or mdir=='':
                if 'mesagdir' in ENV:
                    mdir=ENV['mesagdir']
                elif 'envir' in ENV:
                    mdir='/com/hur/'+ENV['envir']+'/inpdata'
                else:
                    mdir='/com/hur/prod/inpdata'

            # Add the messages to the input files:
            nstorms_filename=os.path.join(mdir,'nstorms')
            nstorms=7
            try:
                with open(nstorms_filename,'rt') as nstorms_file:
                    dat=nstorms_file.readline()
                    nstorms=int(dat)
            except (EnvironmentError,ValueError,TypeError) as e:
                logger.error('%s: error reading: %s.  Will read all storms.'%(
                        nstorms_filename,str(e)),exc_info=True)
            for imessage in xrange(nstorms):
                file=os.path.join(mdir,'message%d'%(imessage+1,))
                if os.path.exists(file):
                    inputs.append(file)

        self.log().info('read vitals from: '+','.join(inputs))
        revital=hwrf.revital.Revital(logger=logger)
        revital.readfiles(inputs,raise_all=False)
        return revital

    def set_storm(self,syndat,oldsyndat):
        """Sets the syndat and oldsyndat member variables, and several
        related options in the [config] section, to the storm in the
        provided tcvitals or message file data.  

          config.STID -- The three character storm id (ie.: 12L) of
            the storm to run.

          config.stnum -- the numeric part of config.STID
          config.basin1 -- the basin part of STID (ie.: the L in 12L)
          config.basin1lc -- the lower-case version of config.basin1
          """
        assert(isinstance(syndat,hwrf.storminfo.StormInfo))
        if oldsyndat is not None:
            assert(isinstance(oldsyndat,hwrf.storminfo.StormInfo))
        self.set_options('config',STID=syndat.stormid3,stnum=syndat.stnum,
                         basin1=syndat.basin1,basin1lc=syndat.basin1lc)
        self.__dict__['syndat']=syndat.copy()
        if oldsyndat is not None:
            self.__dict__['oldsyndat']=oldsyndat.copy()

    def tcautoseed(self,loud=True):
        """Automatically decides a random seed for the tcvitals
        perturbation, based on the storm number, basin and cycle.  The
        number and basin used are before the invest renumbering
        (self.syndat.old()).  

        If loud=True (the default), then a message is sent to the
        jlogfile via postmsg with the seed, and information about the
        calculation that went into it."""
        si=self.syndat.old() # storminfo before renumbering
        icycle=int(self.cycle.strftime('%Y%m%d%H'))
        istnum=int(si.stnum)
        cbasin=str(si.basin1).upper()
        ibasin=ord(cbasin)
        seed=icycle ^ istnum ^ ibasin # ^ is bitwise exclusive or (XOR)
        if loud:
            produtil.log.postmsg(
                'Automatic perturbation seed calculation: '
                '%d %d%s => seed = %d^%d^ord("%s") = %d^%d^%d = %d'%(
                    icycle,istnum,cbasin,
                    icycle,istnum,cbasin,
                    icycle,istnum,ibasin,
                    seed))
        return seed

    def gen_vitals(self,STID,cycling_interval,revital,storm_num=None):
        """Given an hwrf.revital.Revital object, preferably from
        read_precleaned_vitfile or read_tcvitals_and_messages,
        searches for the specified storm's vitals.  Creates the files
        that are expected to exist in the WORKhwrf directory.  The
        filenames are based off of the vitbase variable, but with
        various suffixes appended.  This function should only be
        called once per workflow, per storm."""
        logger=self.log()
        stnum=int(STID[0:2],10)
        STID=STID.upper()
        strcycle=self._cycle.strftime('%Y%m%d%H')
        syndat=None
        oldsyndat=None

        if cycling_interval<0:
            cycling_interval=-cycling_interval
        cycling_interval=cycling_interval/3600.0
        prior=hwrf.numerics.to_datetime_rel(-cycling_interval*3600.,self._cycle)
        strprior=prior.strftime('%Y%m%d%H')
        logger.info('gen_vitals: cycle=%s interval=%s prior=%s STID=%s'%(
                repr(self.cycle),repr(cycling_interval),repr(prior),
                repr(STID)))

        def keep_condition(vit):
            return vit.stormid3.upper()==STID or \
                ( 'old_stormid3' in vit.__dict__ and 
                  vit.old_stormid3.upper()==STID )

        if stnum>=50:
            logger.info('%s: Not renumbering invests because %d>=50.'
                        %(STID,stnum))
            unrenumbered=revital.copy()
            unrenumbered.discard_except(keep_condition)
            unrenumbered.clean_up_vitals()
            renumbered=unrenumbered
        else:
            logger.info('%s: Renumber and unrenumber invests.'%(STID,))
            unrenumbered=revital.copy()
            unrenumbered.renumber(unrenumber=True)
            unrenumbered.discard_except(keep_condition)
            unrenumbered.clean_up_vitals()
            renumbered=unrenumbered.copy()
            renumbered.swap_numbers()
            renumbered.clean_up_vitals()
            unrenumbered.mirror_renumbered_vitals()
            unrenumbered.clean_up_vitals()

        # Find the current cycle's vitals:
        for vit in renumbered.each(STID):
            if vit.when==self._cycle:
                syndat=vit

        if syndat is None:
            raise hwrf.storminfo.NoSuchVitals(
                'Error: cannot find %s cycle %s'%(STID,strcycle))
        logger.info('syndat='+syndat.as_tcvitals())
        self.set_storm(syndat,None)

        # Perturb the current cycle's vitals if requested.
        ens=self.getint('config','ENS',99)
        ensize=self.getint('ensemble','ensize',20)
        if ens>0 and ens<99 and ens<=ensize:
            seedmethod=self.getstr('ensemble','tcvitals_seed')
            if seedmethod=='auto':
                seed=self.tcautoseed()
            else:
                seed=self.getint('ensemble','tcvitals_seed')
            vmax_pert=self.getint('ensemble','vmax_pert')
            if vmax_pert>0:
                rand=Random()
                rand.seed(seed)
                vperts=hwrf.numerics.randint_zeromean(ensize,vmax_pert,rand)
                vpert=vperts[ens-1]
                logger.info('ENS perturbations: %s sum %s'%(
                        ( ', '.join([repr(s) for s in vperts]) ), 
                        repr(sum(vperts)) ))
                produtil.log.postmsg(
                    'ENS %d (of %d) wind perturbation %d m/s'
                    %(ens,ensize,vpert))
                vit.wmax+=vpert
            else:
                produtil.log.postmsg(
                    'ENS %d (of %d) wind perturbation disabled'%(ens,ensize))
        else:
            produtil.log.postmsg(
                'ENS %d (of %d) is not a perturbed ensemble member; '
                'not perturbing wind.'%(ens,ensize))

        # Find the prior cycle's vitals.  First pass: look for a cycle
        # whose data that actually exists on disk.
        nodatasyndat=None
        for vit in unrenumbered.each(STID,old=True):
            if vit.when!=prior: continue # wrong cycle
            if oldsyndat is not None and oldsyndat.stnum<50:
                logger.info('%s %s: not checking these vitals for data on '
                            'disk since I found a non-invest number %s '
                            'already with data on disk'
                            %(str(vit.stormid3),str(vit.YMDH),
                              str(oldsyndat.stormid3)))
            else:
                checkfile=self.timestrinterp(
                    'config','{HISTCHECK}',atime=prior,ftime=prior,
                    oldvit=vit.__dict__,vit=syndat.__dict__)
                if os.path.exists(checkfile):
                    logger.info('%s: exists'%(checkfile,))
                    logger.info('%s %s: prior is %s %s and has data on disk'%
                              (STID,strcycle,vit.stormid3,strprior))
                    oldsyndat=vit
                else:
                    logger.info('%s: does not exist'%(checkfile,))
                    logger.info('%s %s: prior could be %s %s but there is '
                                'no data on disk'%
                                (STID,strcycle,vit.stormid3,strprior))
            if oldsyndat is None:
                if nodatasyndat is not None and nodatasyndat.stnum<50:
                    logger.info('%s %s: not using as backup since I found a '
                                'non-invest number %s already'
                                %(str(vit.stormid3),str(vit.YMDH),
                                  str(nodatasyndat.stormid3)))
                else:
                    nodatasyndat=vit
                        
        self.set('config','expect_cold_start','no')
        if oldsyndat is None:
            logger.info('%s %s: no storm IDs for prior cycle have data '
                        'on disk.'%(STID,strcycle))
            if nodatasyndat is not None:
                oldsyndat=nodatasyndat
                logger.info('%s %s: will use %s %s as prior cycle storm.'
                          %(STID,strcycle,oldsyndat.stormid3,strprior))
                logger.info('prior vitals: '+oldsyndat.as_tcvitals())
            else:
                logger.warning('No prior syndat available.  This is a cold '
                               'start.  I will extrapolate vitals.')
                oldsyndat=syndat-cycling_interval # extrapolate vitals
                logger.warning('extrapolated vitals: %s'
                               %(oldsyndat.as_tcvitals()))
            self.set('config','expect_cold_start','yes')
        else:
            logger.info('%s %s prior cycle on disk for %s %s'
                        %(STID,strcycle,oldsyndat.stormid3,strprior))
            logger.info('prior cycle on disk: '+oldsyndat.as_tcvitals())

        self.set_storm(syndat,oldsyndat)
        vitbase=self.choose_vitbase(storm_num)

        vitbasedir=os.path.dirname(vitbase)
        produtil.fileop.makedirs(vitbasedir,logger=logger)

        logger.info('Reformat vitals...')
        filename=vitbase+'.allids'
        logger.info(
            filename+': write unrenumbered vitals with all storm IDs')
        with open(filename,'wt') as vitalsout:
            for vit in unrenumbered.each(stormid=STID,old=True):
                print>>vitalsout, vit.as_tcvitals()
        filename=vitbase+'.renumberlog'
        logger.info(filename+': write renumberlog with my storm ID')
        logger.info(vitbase+': write renumbered vitals')
        with open(filename,'wt') as renumberlog:
            with open(vitbase,'wt') as vitalsout:
                renumbered.print_vitals(vitalsout,renumberlog=renumberlog,
                                     stormid=STID,format='tcvitals')
        filename=vitbase+'.oldid'
        logger.info(filename+': write vitals with original ID')
        with open(filename,'wt') as vitalsout:
            for vit in renumbered.each(stormid=STID):
                print>>vitalsout, vit.old().as_tcvitals()

        filename=os.path.join(self.getdir('WORKhwrf'),'tmpvit')
        logger.info(filename+': write current cycle vitals here')
        with open(filename,'wt') as tmpvit:
            print>>tmpvit, self.syndat.as_tcvitals()

        filename=os.path.join(self.getdir('WORKhwrf'),'oldvit')
        logger.info(filename+': write prior cycle vitals here')
        with open(filename,'wt') as tmpvit:
            print>>tmpvit, self.oldsyndat.as_tcvitals()

    def sanity_check_ensemble(self,enset,logger=None):
        """Runs a sanity check on the ensemble configurations.  Makes
        sure that:

        1. If the GEFS-based forecast ensemble is in use, a valid
        ensemble ID is chosen.

        2. If a valid ensemble ID is chosen, the GEFS-based forecast
        ensemble is in use.

        3. The user does not enable both the GEFS-based forecast
        ensemble and the GFS-based DA ensemble.

        4. If the GFS-based DA ensemble is in use, at least thirty
        members are chosen, and no more than eighty."""

        has_gefs_members=False
        has_deterministic=False
        has_invalid=False
        for ens in enset:
            iens=int(ens,10)
            if iens>=0 and iens<=20:
                has_gefs_members=True
            elif iens==99:
                has_deterministic=True
            else:
                raise HWRFConfigInsane(
                    "Invalid ensemble ID %s: must be 00-20 or 99"
                    %(repr(ens),))

        if has_deterministic and has_gefs_members:
            raise HWRFConfigInsane(
                "You cannot run the GFS-based deterministic HWRF (ENS=99) "
                "and GEFS-based hwrf (ENS=00 through 20) in the same "
                "workflow.")
        
        is_fcst_ens=self.getbool('config','is_forecast_ensemble',False)
        fcst_ens=has_gefs_members
        da_ens=self.getbool('config','run_ensemble_da')

        if (fcst_ens or is_fcst_ens) and da_ens:
            raise HWRFConfigInsane(
                """
You cannot run both the GFS-based DA ensemble (ENS=99
run_ensemble_da=yes) and GEFS-based forecast ensemble (ENS=00 through
20, run_ensemble_da=no).  Turn one of them off.

To run the GEFS-based HWRF ensemble with no data assimilation, you
must set the ensemble ID to one or more numbers from 00-20 and specify
the hwrf_ensemble_$YYYY override file:

  ./run_hwrf.py 01-20 2015 03W FORECAST ../parm/hwrf_ensemble_2014.conf

To run the deterministic HWRF with ensemble covariances from six hour
forecasts of HWRF off of the GFS ENKF, do this:

  ./run_hwrf.py 2015 03W FORECAST config.run_ensemble_da=yes

You cannot do both.""")

        if is_fcst_ens!=fcst_ens:
            raise HWRFConfigInsane(
                """
When running the GEFS-based HWRF ensemble, you must set the ensemble
ID to one or more numbers from 00-20 and specify the
hwrf_ensemble_$YYYY override file:

  ./run_hwrf.py 01-20 2015 03W FORECAST ../parm/hwrf_ensemble_2014.conf

To run the deterministic HWRF, do neither:

  ./run_hwrf.py 2015 03W FORECAST
""")

        if da_ens:
            ensda_size=self.getint('hwrf_da_ens','ensda_size',0)
            if(ensda_size<30):
                raise HWRFConfigInsane(
                    "You must use at least 30 members when running the GFS "
                    "ENKF based HWRF DA ensemble.  You only requested %d."
                    %ensda_size)
            if(ensda_size>80):
                raise HWRFConfigInsane(
                    "You cannot use more than 80 members when running the GFS"
                    " ENKF based HWRF DA ensemble.  You requested %d."
                    %ensda_size)

    def sanity_check_archive(self,logger=None):
        """Runs a sanity check on the archiving settings."""
        if not self.getbool('sanity','check_archive',True): return
        archive=self.getloc('archive','NONE')
        if archive.lower()=='none':
            if logger is not None: 
                logger.info('Archiving is disabled: archive=none')
            return
        
        adir=os.path.dirname(archive[5:])
        missing=False
        if archive[0:5]=='hpss:' or archive[0:5]=='hpsz:':
            logger.info('Cannot hsi -P ls / so skipping archive check.')
            # hsils1=exe(self.getexe('hsi'))['-P','ls','/']
            # if 0!=run(hsils1,logger=logger):
            #     logger.info('Cannot hsi -P ls / so skipping archive check.')
            #     produtil.log.postmsg(
            #         'Skipping archive sanity check: hsi -P ls / failed')
            #     return

            # hsils2=exe(self.getexe('hsi'))['-P','ls',adir]
            # if 0==run(hsils2,logger=logger):
            #     logger.info('%s: HPSS archive directory exists.'%(adir,))
            # else:
            #     missing=True
            #     logger.warning('%s: HPSS archive directory is missing.'
            #                    %(adir,))
        elif archive[0:5]=='disk:':
            if os.path.exists(adir):
                if os.path.isdir(adir):
                    logger.info('%s: disk archive directory exists and is a '
                                'directory.'%(adir,))
                else:
                    msg='%s: disk archive directory is not a '\
                        'directory '%(adir,)
                    logger.warning(msg)
                    raise HWRFArchiveInsane(msg)
            else:
                logger.info('%s: disk archive directory does not exist'
                            %(adir,))
                missing=True
        else:
            msg='%s: Invalid archive method %s'%(archive,archive[0:4])
            logger.error(msg)
            raise HWRFArchiveInsane(msg)
        if missing:
            if not self.getbool('archive','mkdir',False):
                msg='%s: archive directory is missing and [archive] mkdir '\
                    'is disabled.  Archive job would fail.  Set [config] '\
                    'archive=none to disable archiving OR set [archive] '\
                    'mkdir=yes to make archive directory or disable the '\
                    'archive sanity check with [sanity] check_archive=no'\
                    %(archive,)
                logger.warning(msg)
                raise HWRFArchiveInsane(msg)

    def sanity_check_config_files(self,logger=None):
        """Runs sanity checks related to config files.  For example, some config files are incompatible with others, and some must bre laoded in a specific order."""

        if self.getbool('prelaunch','hwrf_43lev_conf',False) and \
           self.getbool('prelaunch','hwrf_3km_conf',False) and \
           self.getstr('prelaunch','last_of_43lev_3km','OOO')=='43lev':
            msg="When using 43lev and 3km configurations together, you "\
                "must load hwrf_43lev.conf BEFORE hwrf_3km.conf.  "\
                "Otherwise, the model will use the wrong timestep."
            if logger is not None: logger.error(msg)
            raise hwrf.exceptions.HWRFConfigFileOrder(msg)

    def timeless_sanity_check(self,enset=None,logger=None):
        """Runs any sanity checks that are possible without knowing
        the cycle that is to be run.  This is intended to be used by
        the workflow automation system (rocoto, ecflow, etc.) to make
        sure everything is functional before starting any jobs."""

        for dirvar in ( 'HOMEhwrf', 'EXEChwrf', 'EXhwrf', 'USHhwrf', 
                        'FIXhwrf', 'PARMhwrf', 'utilexec' ):
            logger.debug('%s: check this dir variable'%(dirvar,))
            thedir=self.getdir(dirvar)
            self.sanity_check_directory(thedir,dirvar,False,logger)

        # Make sure the hwrf.launcher exists, and is the same as this
        # one.
        checkme=os.path.join(self.getdir('USHhwrf'),'hwrf','launcher.py')
        myfile=os.path.realpath(__file__)
        if myfile[-4:]=='.pyc': myfile=myfile[0:-1]
        if not produtil.fileop.isnonempty(checkme):
            raise HWRFScriptInsane(
                '%s: The ush/hwrf/launcher.py does not exist, which is '
                'impossible because it is running now.  Check your paths '
                'and EXPT.'%(checkme,))
        if not os.path.samefile(checkme,myfile):
            raise HWRFScriptInsane(
                '%s: not the same as the launcher.py that is running now '
                '(%s) -- check your paths and EXPT.'%(checkme,myfile))
        self.sanity_check_forecast_length(logger)
        self.sanity_check_executables(logger)
        self.sanity_check_fix_files(logger)
        self.sanity_check_config_files(logger)
        if enset is not None:
            self.sanity_check_ensemble(enset,logger)

    def sanity_check_forecast_length(self,logger=None):
        """Ensures the forecast length is valid."""
        iflen=self.getint('config','forecast_length',126)
        if iflen<12:
            raise HWRFConfigInsane("The forecast length must be at least "
                                   "12hrs (you specified %dhrs)"%iflen)
        if iflen%6 != 0:
            raise HWRFConfigInsane("The forecast length must divisible by "
                                   "6hrs (you specified %dhrs)"%iflen)

    def sanity_check_directory(self,thedir,dirvar,writable=True,logger=None):
        """Checks to make sure the specified directory exists and can
        be read and executed.  If writable=True, also checks to see if
        it can be written.  The dirvar is an explanation of what the
        directory relates to, for example HOMEhwrf."""
        if logger is None: logger=self.log('sanity.checker')
        logger.info('%s: check directory %s'%(dirvar,thedir))
        if not os.path.exists(thedir):
            raise HWRFDirInsane('%s: directory does not exist: %s'
                                 %(dirvar,thedir),thedir)
        if writable:
            if not os.access(thedir,os.W_OK):
                raise HWRFDirInsane('%s: cannot write directory: %s'
                                    %(dirvar,thedir),thedir)
        if not os.access(thedir,os.R_OK):
            raise HWRFDirInsane('%s: cannot read directory: %s'
                                 %(dirvar,thedir),thedir)
        if not os.access(thedir,os.X_OK):
            raise HWRFDirInsane('%s: cannot execute directory: %s'
                                 %(dirvar,thedir),thedir)

    def sanity_check_sanity_check(self,logger=None):
        """Checks to see if the sanity checks can be run.  In essence,
        this is a sanity check of the sanity check routines."""
        if not self.has_section('sanity'):
            raise HWRFConfigInsane(
                'The [sanity] section is missing from the HWRF conf files.')
        # Checking the fix_version is a further check of the [sanity]
        # section:
        self.sanity_get_fix_version(logger)

    def sanity_get_fix_version(self,logger=None):
        """Gets the expected fix file version from [sanity]
        fix_version.  Raises HWRFConfigInsane if there is an error
        while getting it."""

        fix_version=self.getstr('sanity','fix_version','nope')
        if fix_version=='nope':
            raise HWRFConfigInsane(
                'The [sanity] section fix_version is not set.')
        try:
            fix_version=int(fix_version)
        except (ValueError, TypeError) as e:
            raise HWRFConfigInsane(
                'The [sanity] section fix_version is not a number.')
        if fix_version > 20991231:
            raise HWRFConfigInsane(
                'The [sanity] section fix_version has an implausable value '
                '%d (>20991231)'%fix_version)
        if fix_version < 20040131:
            raise HWRFConfigInsane(
                'The [sanity] section fix_version has an implausable value '
                '%d (<20140131)'%fix_version)
        return fix_version

    def sanity_check_fix_files(self,logger=None):
        """Checks to see if the fix files are available and match the
        expected fix file version."""

        if not self.getbool('sanity','check_fix',True):
            if logger is not None:
                logger.info(
                    'Skipping fix file check: [sanity] check_fix=no')
                return

        fix_version=self.sanity_get_fix_version(logger)
        if logger is not None:
            logger.info('Want fix file version %d'%fix_version)
            datestamp=os.path.join(self.getdir('FIXhwrf'),'hwrf_fix_datestamp')
            logger.info('check fix version: '+datestamp)
        def complain(msg):
            if logger is not None: logger.error(msg)
            raise HWRFFixInsane(msg)
        try:
            with open(datestamp,'rt') as f:
                line=f.readline()
                line=line.rstrip()
                version=int(line)
                if version>20991231:
                    complain('%s: The fix file version datestamp %d is '
                             'implausible (>201991231)'%(datestamp,version))
                elif version<20040131:
                    complain('%s: The fix file version datestamp %d is '
                             'implausible (<20140131)'%(datestamp,version))
                elif version<fix_version:
                    complain('%s: The fix file version is too old.  Expected '
                             '%d, got %d'%(datestamp,fix_version,version))
                elif version!=fix_version:
                    msg=(
                        '%s: The fix file version (%d) does not match '
                        'the expected version (%d).  It is a newer '
                        'version, so I will try to run.'%(
                            datestamp,version,fix_version))
                    if logger is not None: logger.warning(msg)
                    produtil.log.jlogger.warning(msg)
                else:
                    logger.info('fix version %d matches'%version)

        except (KeyError,TypeError,EnvironmentError,ValueError) as e:
            complain('%s: fix files failed a sanity check: %s'%(
                datestamp,str(e)))

    def sanity_check_executables(self,logger=None):
        """Checks to see if a few of the executables are available.
        This is not an exhaustive check: most executables are not
        checked.  This check is just to see if the user forgot to
        install executables entirely."""

        if not self.getbool('sanity','check_exec',True):
            if logger is not None:
                logger.info(
                    'Skipping executable check: [sanity] check_exec=no')
                return

        loc=None
        exe=None

        def complain(why):
            # msg="wrf: /path/to/wrf.exe: executable is empty"
            msg='%s: %s: %s'%(exe,loc,why) 
            if logger is not None:
                if exe=='gsi' and os.environ.get('PARAFLAG','YES')!='NO':
                    logger.critical(
                        '''GSI EXECUTABLE IS MISSING:

If you are not NCO, and you are on Jet, Zeus or WCOSS, the latest
developmental version of the HWRF GSI, maintained by Mingjing Tong,
can be found at these locations:

    WCOSS: /hwrf/save/emc.hurpara/EMCGSI/hwrf_gsi
    Zeus: /scratch1/portfolios/NCEPDEV/hwrf/save/hurpara/EMCGSI/hwrf_gsi
    Jet: /mnt/pan2/projects/hwrfv3/hurrun/EMCGSI/hwrf_gsi

Just link or copy the src/global_gsi executable to exec/hwrf_gsi
in your installation directory:

    ln -s /path/to/GSI_HWRF/src/global_gsi %s

If you are on another machine, you will need to check out and build
GSI from either the EMC or DTC repositories, then build and install
it.

Sincerely,
 /                   \\
 \\O\\ THE HWRF TEAM /O/
   /               \\  '''%(loc,))
                else:
                    logger.critical(msg)
            raise HWRFExecutableInsane(msg)

        checkme=[ 'wrf', 'gettrk', 'post', 'real_nmm', 'mpiserial' ,
                  'hwrf_geogrid', 'tar', 'hwrf_nhc_products', 
                  'cnvgrib' ]

        run_gsi=self.getbool('config','run_gsi',True)
        run_ocean=self.getbool('config','run_ocean',True)
        run_relocation=self.getbool('config','run_relocation',True)
        
        if run_relocation: checkme.append('hwrf_wrf_split')
        if run_gsi: checkme.append('gsi')
        if run_ocean: checkme.append('hwrf_ocean_fcst')

        for exe in checkme:
            loc=self.getexe(exe)
            if loc.find('/')<0:
                # No path, so we need to search $PATH
                path=produtil.fileop.find_exe(loc,raise_missing=False)
                if path is None:
                    complain('cannot find in $PATH')
                loc=path
            if not os.path.exists(loc): complain('executable does not exist')
            if os.path.getsize(loc)<=0: complain('executable is empty')
            if not os.path.isfile(loc): complain('executable is not a file')
            if not os.access(loc,os.X_OK): complain('cannot execute')

    def sanity_check(self):
        """Runs simple sanity checks on the HWRF installation
        directory and configuration to make sure everything looks
        okay.  May throw a wide variety of exceptions if sanity checks
        fail."""
        logger=self.log('sanity.checker')
        for dirvar in ( 'WORKhwrf', 'com' ):
            logger.info('%s: check this dir variable'%(dirvar,))
            thedir=self.getdir(dirvar)
            self.sanity_check_directory(thedir,dirvar,True,logger)

        enset=set()
        enset.add(self.get('config','ENS','99'))

        self.timeless_sanity_check(enset,logger)

        CONFhwrf=self.getdir('CONFhwrf')
        logger.info('Try to load configuration file %s'%(CONFhwrf,))
        redo=load(CONFhwrf)

        logger.info('Compare new and old vitals')
        if 'syndat' in self.__dict__ and self.syndat.stormid3 != \
                redo.syndat.stormid3:
            raise HWRFStormInsane(
                "New directory has the wrong stormid: correct=%s conf=%s"
                %(self.syndat.stormid3,redo.syndat.stormid3))
        if self.cycle!=redo.cycle:
            raise HWRFCycleInsane(
                'New directory has the wrong cycle: correct=%s conf=%s'
                %(self.cycle.strftime('%Y%m%d%H'),
                  redo.cycle.strftime('%Y%m%d%H')))

        case_root=redo.getstr('config','case_root').upper()
        input_catalog=redo.getstr('config','input_catalog')
        logger.info('Case root is %s and input catalog is %s'
                    %(repr(case_root),repr(input_catalog)))

        if case_root=='HISTORY':
            if not self.getbool('sanity','check_input',True):
                logger.info(
                    'Input check is disabled: [sanity] check_input=False.  '
                    'Skipping input checks.')
            elif self.get('config','input_catalog')=='hwrfdata':
                logger.info(
                    '[config] input_catalog=hwrfdata  --  skipping input '
                    'check: will fetch input instead.')
            else:
                in_item=self.getstr('sanity','input_item','gfs')
                in_dataset=self.getstr('sanity','input_dataset','gfs_sfcanl')
                logger.info('Retrospective mode.  Check for %s %s file.'%(
                        in_item,in_dataset))
                ic=hwrf.input.DataCatalog(redo,input_catalog,redo.cycle)
                there=ic.locate(in_dataset,in_item,redo.cycle)
                if there is None:
                    raise HWRFInputInsane('Could not locate %s %s file.'
                                          %(in_dataset,in_item))
                if not os.path.exists(there):
                    raise HWRFInputInsane(
                        '%s %s file does not exist: %s'%(
                            in_dataset,in_item,there))
                if not isnonempty(there):
                    raise HWRFInputInsane('%s %s file is empty: %s'%(
                            in_dataset,in_item,there))
        elif case_root=='FORECAST':
            logger.info('Real-time mode.  Will skip data checks.')
        else:
            raise HWRFVariableInsane(
                'config.case_root must be HISTORY or FORECAST not %s'
                %(repr(case_root),))

        self.sanity_check_archive(logger)
        self.sanity_check_expt(logger)

    def sanity_check_expt(self,logger):
        """Loads the hwrf_expt module, runs its init_module routine,
        and then runts its sanity_check, passing the specified
        logger."""

        if not self.getbool('sanity','check_expt',True):
            if logger is not None:
                logger.info(
                    'Skipping hwrf_expt check: [sanity] check_expt=no')
                return

        logger.info('Export [config] CONFhwrf to the environment '
                    'variable $CONFhwrf.')
        os.environ['CONFhwrf'] = self.getstr('config','CONFhwrf')
        logger.info('Attempt to load hwrf_expt module.')
        import hwrf_expt
        logger.info('Attempt to initialize hwrf_expt module.')
        hwrf_expt.init_module()
        logger.info("Run the hwrf_expt module's own sanity_check routine.")
        hwrf_expt.sanity_check(logger)

    def guess_default_values(self):
        """Tries to set default values for some mandatory conf
        options.  The default values come from either other options or
        from environment variables.  If no suitable default can be
        found, execution will continue, but later jobs may fail.

        Config options are as follows.  If $VAR appears, that refers
        to ENV["VAR"]:

          config.cycle -- the cycle to run as a ten digit date (2014091418)
            Taken from cycle if present, otherwise $YMDH

          config.storm_num -- the storm number as a priority 1 to 5.
            Taken from $storm_num or uses the default of 1.

          config.stormlabel -- "storm" with the storm number appended
            (ie.: storm5 if storm_num=5).  

          dir.HOMEhwrf -- set to HOMEhwrf or $HOMEhwrf

          dir.WORKhwrf -- set to WORKhwrf or $WORKhwrf

          dir.syndat -- tcvitals directory.  Default: $COMINARCH

          config.input_catalog -- input catalog (conf section) name.
            Default: $INPUT_CATALOG or "hwrfdata"

          config.PARAFLAG -- NO if you are NCEP Central Operations
            (NCO), and YES otherwise.  This is used to turn on or off
            DBNet alerts and other NCO-specific options. 

        In addition, the following directories are added to the [dir]
        section:

          USHhwrf -- the location of ush scripts and the parent
            directory of the hwrf, pom and produtil packages
          FIXhwrf -- the location of the HWRF fix directory
          JOBhwrf -- the location of the HWRF jobs directory.  This is
            not needed by normal users.
          EXhwrf -- the location of the HWRF scripts directory
          PARMhwrf -- the location of the HWRF parm/ directory
          EXEChwrf -- the location of the HWRF exec/ directory
          utilexec -- the location of the HWRF nwport/exec or
            /nwprod/exec directory

        If set, these variables will be copied to the [config] section:
          EXPT -- optional: the experiment identifier, which must be
            alphanumeric, and can contain underscores.  Default: HWRF
          SUBEXPT -- optional: the subexperiment identifier, which
            must be alphanumeric, and can contain underscores.
            Default: set to value of EXPT
        """
        ENV=os.environ
        logger=self.log()
        PARAFLAG=( ENV.get('PARAFLAG','YES')!='NO' )

        def set_default(section,option,default,env1=None,env2=None):
            if not self.has_option(section,option):
                if env1 is not None and env1 in ENV:
                    self.set(section,option,ENV[env1])
                elif env2 is not None and env2 in ENV:
                    self.set(section,option,ENV[env2])
                elif default is not None:
                    self.set(section,option,str(default))
                else:
                    logger.error(
                        'Cannot find suitable default for [%s] option %s'%(
                            section,option))

        set_default('config','case_root','HISTORY','CASE_ROOT')
        set_default('config','EXPT','HWRF','EXPT')
        set_default('config','SUBEXPT','{EXPT}','SUBEXPT')
        set_default('dir','HOMEhwrf',None,'HOMEhwrf')
        set_default('dir','WORKhwrf',None,'WORKhwrf','DATA')
        set_default('config','datastore','{WORKhwrf}/hwrf_state.sqlite3')
        set_default('config','storm_num','1','storm_num')
        set_default('config','stormlabel','storm{storm_num}')
        set_default('config','input_catalog','hwrfdata','INPUT_CATALOG')
        set_default('dir','syndat',None,'COMINARCH')
        set_default('dir','com',None,'COMOUT')
        set_default('config','PARAFLAG','YES','PARAFLAG')
       
        if not self.has_option('config','cycle'):
            if 'YMDH' in ENV:
                self.cycle=ENV['YMDH']
        #cycle=self.cycle

        if 'NWPROD' in ENV:
            NWPROD='NWPROD',ENV['NWPROD']
        elif 'envir' in ENV and os.path.exists('/nw'+ENV['envir']):
            NWPROD='/nw'+ENV['envir']
        else:
            NWPROD='{HOMEhwrf}/nwport'

        def dirset(evar,deff,parent='{HOMEhwrf}'):
            if evar in ENV:
                self._conf.set('dir',evar,ENV[evar])
            elif not self._conf.has_option('dir',evar):
                self._conf.set('dir',evar,parent+'/'+deff.lower())

        dirset('FIXhwrf','fix')
        dirset('USHhwrf','ush')
        dirset('EXhwrf','scripts')
        dirset('EXEChwrf','exec')
        dirset('JOBhwrf','jobs')
        dirset('PARMhwrf','parm')
        dirset('utilexec','util/exec',NWPROD)

    def make_holdvars(self,part1='{PARMhwrf}/hwrf_holdvars.txt',part2=None):
        """Creates the storm*.holdvars.txt file needed by the old
        ksh-based scripts.  This is done for backward compatibility
        only.  The two arguments (part1 and part2) are two files to
        pass through self.strinterp and then into the holdvars file.
        Part 1 is mandatory, but part2 is optional.  It also fills in
        a few custom derived variables:

          cap_run_gsi -- capitalized version of [config] section run_gsi
          cap_run_relocation -- capitalized version of [config]
            section run_relocation
          holdvars_model -- "COUPLED" if [config] section run_ocean is
            true, and "ATMOS" if it is false."""
        assert(isinstance(part1,basestring))
        out=list()
        logger=self.log()
        gsi_flag=self.getbool('config','run_gsi')
        self.set('holdvars','cap_run_gsi',('YES' if gsi_flag else 'NO'))

        reloc_flag=self.getbool('config','run_relocation')
        self.set('holdvars','cap_run_relocation',
                 ('YES' if reloc_flag else 'NO'))

        ocean_flag=self.getbool('config','run_ocean')
        self.set('holdvars','holdvars_model',
                 ('COUPLED' if ocean_flag else 'ATMOS'))

        io_pergrp=self.getint('runwrf','nio_tasks_per_group',0)
        io_groups=self.getint('runwrf','nio_groups',0)
        io_servers = (io_pergrp*io_groups)>0

        self.set('holdvars','IO_SERVERS',
                 ('YES' if io_servers else 'NO'))
        self.set('holdvars','IOSRV_PERGRP','%d'%io_pergrp)
        self.set('holdvars','IOSRV_GROUPS','%d'%io_groups)

        with open(self.strinterp('dir',part1),'rt') as f:
            for line in f: 
                out.append(self.strinterp('holdvars',line.rstrip()))
        if part2 is not None:
            with open(self.strinterp(part2),'rt') as f:
                for line in f: 
                    out.append(self.strinterp(line.rstrip()))
        return '\n'.join(out) + '\n'
