#! /usr/bin/env python

import os, sys, re, logging, collections, StringIO
from os.path import realpath, normpath, dirname

def ask(question):
    sys.stdout.write(question)
    itry=0
    itrytoohard=100
    go=True
    while go:
        itry+=1
        x=sys.stdin.readline()
        if x.lower()=='y\n':
            return True
        elif x.lower()=='n\n':
            return False
        elif itry>=itrytoohard:
            sys.stderr.write('Giving up after %d failed responses.'%itry)
            sys.exit(2)
        else:
            sys.stdout.write('Please answer y or n.')

def usage(message=None,logger=None):
    print>>sys.stderr, '''
Usage: run_hwrf.py [options] [ensids and cycles] 95E case_root [conf]

Mandatory arguments: 
  95E -- the storm to run
  case_root -- FORECAST = real-time mode, HISTORY = retrospective mod

Workflow options:
  -f -- Tells the run_hwrf.py that you already ran it once for this
        storm, cycle list and experiment.  It will use any existing
        *.xml or *.db file without asking for permission.  Critical 
        in crontabs.
  -w workflow-file.xml -- use this as the output XML file to send 
        into rocotorun (rocotorun's -w option)
  -d workflow-db.db -- use this as the SQLite3 database file for
        Rocoto (rocotorun's -d option)

Specifying a site:
  -s site-file -- path to a custom-made site file, rather than using
        one automatically chosen from sites/*.ent.  Do not include any
        shell or XML metacharacters in the name.

PATHS:
  This script should be run from the rocoto subdirectory of the HWRF
  installation location so it can guess the ush/ and parm/ locations
  (../ush and ../parm).  You can override those guesses by providing
  the paths in the $USHhwrf and $PARMhwrf environment variables.

ENSEMBLE OPTIONS:
  [ensids] -- one or more ensemble id specification:
    03 - run member 03
    07-13 - run members 07, 08, 09, 10, 11, 12 and 13
    00-20 - run members 00, 01, 02, 03, ..., 20
  You can give multiple specifications, so this:
    03-05 06
  Is the same as giving all four members explicitly:
    03 04 05 06

SPECIFYING CYCLES:

  -c N -- number of hours between cycles.  This ONLY affects cycle
   specifications after the -c option.

  [cycles] -- one or more cycle specifications:
    2014091312-2014091712     - run this range of cycles
    2014091312                - run this cycle
    2014                      - all cycles from 0Z January 1, 2014 to 
                                the end of that year.
    2014091312-2014091712 2014091800 - run cycles from 2014091312
                               through 2014091712 AND run 2014091800

    H214:2012 - run whatever cycles H214 ran for this 2012 storm

  -t -- include cycles even if they are not in the tcvitals.  This
        option is turned on automatically when H214 cycle lists are
        requested.

  -n -- disable renumbering of invests to non-invests.  This is done
        automatically when an invest is requested.

  -W N -- discard invests weaker than N meters per second before
        renumbering.  Default: -W 14 if a non-invest storm is 
        requested, and -W 0 (don't discard) if an invest is requested.

Configuration ([conf]):
section.option=value -- override conf options on the command line
/path/to/file.conf -- additional conf files to parse'''
    if message is not None:
        print>>sys.stderr,str(message).rstrip()+'\n'
    sys.exit(2)

########################################################################
# Try to guess $USHhwrf and $PARMhwrf.  The $HOMEhwrf or current
# working directory are used if $USHhwrf and $PARMhwrf are not set in
# the environment.  We also add the $USHhwrf to the Python library
# path.

USHhwrf=None
HOMEhwrf=None
PARMhwrf=None

if os.environ.get('USHhwrf',''):   USHhwrf=os.environ['USHhwrf']
if os.environ.get('PARMhwrf',''):  PARMhwrf=os.environ['PARMhwrf']
if os.environ.get('HOMEhwrf',''):  HOMEhwrf=os.environ['HOMEhwrf']

if HOMEhwrf is None and (USHhwrf is None or PARMhwrf is None):
    HOMEhwrf=dirname(os.getcwd())
    USHguess=os.path.join(HOMEhwrf,'ush')
    PARMguess=os.path.join(HOMEhwrf,'parm')
    if os.path.isdir(USHguess) and os.path.isdir(PARMguess):
        if USHhwrf is None: USHhwrf=USHguess
        if PARMhwrf is None: PARMhwrf=PARMguess

if HOMEhwrf is not None:
    if USHhwrf is None:            USHhwrf=os.path.join(HOMEhwrf,'ush')
    if PARMhwrf is None:           PARMhwrf=os.path.join(HOMEhwrf,'parm')

if USHhwrf is None: 
    print>>sys.stderr, "Cannot guess $USHhwrf.  Please set $HOMEhwrf or " \
        "$USHhwrf in environment."
    sys.exit(2)

if PARMhwrf is None: 
    print>>sys.stderr, "Cannot guess $PARMhwrf.  Please set $HOMEhwrf or " \
        "$PARMhwrf in environment."
    sys.exit(2)

if HOMEhwrf is None:
    print>>sys.stderr, "Cannot guess $HOMEhwrf.  Please set $HOMEhwrf " \
        "in the environment."
    sys.exit(2)

sys.path.append(USHhwrf)

########################################################################
# Load and set up the produtil package.

import produtil.setup, produtil.atparse, produtil.run, produtil.prog, \
    produtil.fileop, produtil.batchsystem, produtil.cluster
import hwrf.launcher, hwrf.numerics, hwrf.rocoto
import hwrf_expt

from hwrf.numerics import to_datetime, to_timedelta
from hwrf.rocoto import entity_quote

from produtil.fileop import remove_file, isnonempty
from produtil.run import run, exe
from produtil.prog import shbackslash

produtil.batchsystem.set_jobname('run_hwrf')
produtil.setup.setup(send_dbn=False)

########################################################################
# Global variables and constants

logger=logging.getLogger('run_hwrf')

epsilon=to_timedelta(5)  # five seconds
six_hours=to_timedelta(6*3600)
cycling_interval=six_hours
cycleset=set()
enset=set()
benchmarkset=None
parse_tcvitals=True
renumber=True
force=False
site_file=''

def okpath(path):
    return produtil.fileop.norm_expand_path(path,fullnorm=True)

########################################################################
# Parse arguments.

outxml=None
outdb=None
args=sys.argv[1:]
dateargs=list()
firstarg=0
iarg=0
weak_invest=None
while iarg<len(args):
    arg=args[iarg]
    if arg=='-c':
        logger.info('-c N -- cycling interval')
        # -c = set the cycling interval
        if iarg+1>=len(args):
            usage('-c requires an argument.')
        intarg=int(args[iarg+1])
        cycling_interval=to_datetime(intarg*3600)
        iarg+=1
    elif arg=='-t':
        logger.info('-t -- parse_tcvitals=False')
        parse_tcvitals=False
    elif arg=='-W':
        logger.info('-W N -- discard weak invests')
        if iarg+1>=len(args):
            usage("-W requires an argument.")
        weak_invest=int(args[iarg+1])
        iarg+=1
    elif arg=='-s':
        if iarg+1>=len(args):
            usage('-s requires an argument.')
        logger.info('-s %s -- specify site file'%(repr(args[iarg+1]),))
        site_file=str(args[iarg+1])
        iarg+=1
    elif arg=='-w':
        if iarg+1>=len(args): usage('-w requires an argument')
        logger.info('-w %s -- use this workflow XML file'%(repr(args[iarg+1]),))
        outxml=args[iarg+1]
        iarg+=1
        if outxml[-3:]=='.db':
            usage('When using the -d option, the Rocoto XML filename must '
                  'not end with ".db".')
    elif arg=='-d':
        if iarg+1>=len(args): usage('-d requires an argument')
        logger.info('-d %s -- use this workflow db file'
                    %(repr(args[iarg+1]),))
        outdb=args[iarg+1]
        iarg+=1
        if outdb[-4:]=='.xml':
            usage('When using the -d option, the database filename must '
                  'not end with ".xml".')
    elif arg=='-n':
        logger.info('-n -- renumber=False')
        renumber=False
    elif arg=='-f':
        logger.info('-f -- force use of existing db and xml files')
        force=True
    elif re.match('\A\d\d\Z',arg):
        logger.info('ensemble id')
        # Single ensemble ID
        enset.add('%02d'%int(arg,10))
    elif re.match('\A\d\d-\d\d\Z',arg):
        logger.info('list of ensemble ids')
        # List of ensemble IDs
        en1=int(arg[0:2],10)
        en2=int(arg[3:],10)
        enset.update([ "%02d"%(x+en1) for x in range(en2-en1+1) ])
    elif re.match('\A\d{10}\Z',arg):
        logger.info('single date/time')
        # Single date/time
        cycleset.add(arg)
        dateargs.append(arg)
    elif re.match('\A\d{4}\Z',arg):
        logger.info('year')
        # Year
        start=to_datetime(arg+'01010000')
        end=to_datetime(arg+'12312359')
        now=start
        while now<end+epsilon:
            cycleset.add(now.strftime('%Y%m%d%H'))
            now+=cycling_interval
        dateargs.append(arg)
    elif re.match('\A\d{10}-\d{10}\Z',arg):
        logger.info('range of cycles')
        # Range of date/times
        start=to_datetime(arg[0:10])
        end=to_datetime(arg[11:])
        now=start
        while now<end+epsilon:
            cycleset.add(now.strftime('%Y%m%d%H'))
            now+=cycling_interval
        dateargs.append(arg)
    elif re.match('\A\d\d[A-Z]\Z',arg.upper()):
        logger.info('storm id')
        # Storm ID.  This ends our argument parsing.  We pass the
        # remaining arguments on to parse_launch_args.
        firstarg=iarg
        if renumber:
            if arg[0]=='9':
                logger.info('Disabling renumbering for invest storm '+arg)
                renumber=False
            elif arg[0]=='8':
                logger.info('Disabling renumbering for test storm '+arg)
                renumber=False
        break
    elif re.match('\AH214:\d\d\d\d\Z',arg.upper()):
        # H214 cycle requested
        logger.info('H214 - use the H214 benchmark cycles')
        parse_tcvitals=False
        benchmarkset=arg.upper()
    else:
        usage('SCRIPT IS ABORTING DUE TO UNRECOGNIZED ARGUMENT "%s"'%(arg,))
    iarg+=1

if benchmarkset and cycleset:
    usage('SCRIPT IS ABORTING: YOU CANNOT SPECIFY CYCLES AND '
          'USE A BENCHMARK SET')

if enset==set(['99']):
    enset=set()

# Now parse the rest of the arguments the same way as exhwrf_launch:
print 'firstarg',firstarg
print 'argsfirstarg..',args[firstarg:]
def fullify(s):
    m=re.match('''(?x)(?P<section>[a-zA-Z][a-zA-Z0-9_]*)\.(?P<option>[^=]+)=(?P<value>.*)$''',s)
    if not m:
        return os.path.abspath(s)
    else:
        return s

# Turn any conf files specified in arguments into fully-qualified
# paths.  This is needed because run_hwrf will generally be started
# from a different directory than the exhwrf_launch.py.
if firstarg+2<len(args):
    confargs=args[(firstarg+2):]
    more_launch_vars=' '.join(
        entity_quote(shbackslash(fullify(str(x))))
        for x in confargs)
else:
    confargs=list()
    more_launch_vars=''
logger.info('MORE_LAUNCH_VARS='+repr(more_launch_vars))

# Tell the hwrf.launcher to parse the remaining arguments so we can
# make the conf information:
(case_root,parm,infiles,stid,moreopt)=hwrf.launcher.parse_launch_args(
    args[firstarg:],logger,usage,PARMhwrf=PARMhwrf)

logger=logging.getLogger('run_hwrf_'+str(stid))

if(weak_invest is None):
    if(str(stid)[0]=='9'):
        logger.info('Invest requested, and no -w given.  Not discarding '
                    'weak Invests.')
        weak_invest=0
    else:
        logger.info('Non-Invest requested, and no -w given.  Will start '
                    'cycling off of last Invest <14m/s.')
        weak_invest=14
        # Note: this weak_invest default must match the value in
        # ush/hwrf/relocate.py's Stage1.run function.

# Generate the conf file and run the hwrf.launcher's sanity checks
# that do not require a cycle:
conf=hwrf.launcher.launch(infiles,None,stid,moreopt,case_root,
                          init_dirs=False,prelaunch=hwrf_expt.prelaunch)
logger.info('Run sanity checks.')
try:
    conf.timeless_sanity_check(enset,logger)
except Exception as e:
    hwrf.rocoto.sanity_check_failed(logger,e)
    sys.exit(1)
logger.info("I think I'm sane.")

# Try to connect to the jlogfile:
loghere=conf.getloc('jlogfile','')
if not loghere:
    try:
        loghere=os.path.join(
            conf.getloc('CDSCRUB'),conf.getstr('config','SUBEXPT'),
            'log','jlogfile')
    except KeyError as ke:
        loghere=None
if loghere:
    print 'Sending jlogfile messages to %s'%(loghere,)
    produtil.log.set_jlogfile(loghere)

########################################################################
# Parse the tcvitals

def check_test_vitals(vl):
    """This is a replacement for hwrf.storminfo.name_number_okay for
    use with TEST storms and internal stormids.  It allows through
    only the storm numbers matching stormnum, regardless of the 
    storm name (usually TEST and UNKNOWN would be dropped)."""
    logger.info('Keeping only storm number %d in vitals'%(stid,))
    for vital in vl:
        if vital.stnum.upper()==stid.upper():
            yield vital

if parse_tcvitals:
    logger.info('Getting list of tcvitals files.')
    syndatdir=conf.getdir('syndat')
    vitpattern=conf.getstr('config','vitpattern','syndat_tcvitals.%Y')
    fileset=set()
    for cycle in cycleset:
        when=to_datetime(cycle)
        vitfile=os.path.join(syndatdir,when.strftime(vitpattern))
        fileset.add(vitfile)
    revit=hwrf.revital.Revital(logger=logger)
    logger.info('List of files to scan: '+(','.join(fileset)))
    revit.readfiles(fileset,raise_all=False)
    if renumber:
        logger.info('Renumber invest cycles.')
        if weak_invest:
            revit.renumber(threshold=int(weak_invest))
        else:
            revit.renumber()
    elif stid[0]=='8':
        logger.info('Fake stormid requested.  Running limited clean-up.')
        revit.clean_up_vitals(name_number_checker=check_test_vitals)
    else:
        logger.info('Not renumbering invest cycles.  Will just clean.')
        revit.clean_up_vitals()
    tcvset=set([ vit.when.strftime('%Y%m%d%H') for vit in revit.each(stid) ])
    notok = cycleset - tcvset
    okset = cycleset - notok
    cycleset=okset

    listed=list(notok)
    listed.sort()
    logger.debug('NOTOK = '+( ','.join(listed) ))

    listed=list(cycleset)
    listed.sort()

    if not listed:
        produtil.log.jlogger.info(
            '%s %s: no cycles to run.  Exiting.'
            %(str(stid),' '.join(dateargs)))
        sys.exit(0)
    else:
        logger.info('I will ONLY run these cycles, since they have vitals:'
                    +(','.join(listed)))
    

########################################################################
# Create the list of variables to send to the ATParser

VARS=dict(os.environ)
if cycleset:
    VARS['CYCLE_LIST']=hwrf.rocoto.cycles_as_entity(cycleset)
    for line in VARS['CYCLE_LIST'].splitlines():
        logger.info('Rocoto cycles: %s'%(line.rstrip(),))
    cyclelist=list(cycleset)
    cyclelist.sort()
    firstcycle=to_datetime(cyclelist[0])
    cycledesc=firstcycle.strftime('%Y%m%d%H')
else:
    assert(isinstance(benchmarkset,basestring))
    year=int(benchmarkset[5:])
    number=sid[0:2]
    basin1=sid[2].upper()
    (ibasin2,basin2,basin1,longinfo) = \
        hwrf.storminfo.expand_basin(basin1)
    cycledesc='&%s%s%s;'%(basin2,number,year)
    VARS['CYCLE_LIST']=cycledesc

fgatstr=conf.getint('fgat','FGATSTR',-3)
fgatend=conf.getint('fgat','FGATEND',3)
fgatinv=conf.getint('fgat','FGATINV',3)

if fgatend > fgatstr:
    fhrlist=' '.join([ '%d'%(i+6) for i in xrange(fgatstr,fgatend+1,fgatinv) ]) 
    initmodel=' '.join([ 'GDAS1' for i in xrange(fgatstr,fgatend+1,fgatinv) ])
    initparts=' '.join([ '3DVAR' for i in xrange(fgatstr,fgatend+1,fgatinv) ]) 
    VARS.update(INIT_FHR=fhrlist)
    VARS.update(INIT_MODEL=initmodel)
    VARS.update(INIT_PARTS=initparts)

if enset:
    enlist=list(enset)
    enlist.sort()
    VARS.update(ENSIDS=' '.join(enlist),
                ENSEMBLE='YES')
else:
    VARS.update(ENSIDS='99',ENSEMBLE='NO')

if conf.getbool('config','run_ensemble_da'):
    VARS.update(DA_ENSEMBLE='YES',ENSDA_ENS_MEMBER='99')
    esize=conf.getint('hwrf_da_ens','ensda_size',40)
    assert(esize>=1)
    ensdalist=' '.join([ '%03d'%(i+1) for i in xrange(esize) ])
    VARS.update(ENSDA_IDS=ensdalist)
else:
    VARS.update(DA_ENSEMBLE='NO',ENSDA_ENS_MEMBER='99',ENSDA_IDS='001')

try:
    stormlabel=conf.get('config','stormlabel','storm1')
except KeyError:
    stormlabel='storm1'

def yesno(b):
    return 'YES' if(b) else 'NO'

VARS.update(SID=stid.upper(),  stormlabel=str(stormlabel),
            WHERE_AM_I=conf.get('holdvars','WHERE_AM_I'),
            WHICH_JET=conf.get('holdvars','WHICH_JET','none'),
            MORE_LAUNCH_VARS=more_launch_vars,
            CASE_ROOT=case_root,
            SITE_FILE=site_file,
            FETCH_INPUT=yesno(conf.get('config','input_catalog')=='hwrfdata'),
            ARCHIVE_WRFOUT=yesno(conf.has_option('archive','wrfout'))
            )

for (key,val) in conf.items('rocotostr'):
    VARS[key]=str(val)
for (key,val) in conf.items('rocotobool'):
    VARS[key]=yesno(conf.getbool('rocotobool',key))


bad=False
for k,v in VARS.iteritems():
    if not isinstance(v,basestring):
        logger.error('%s: value is not a string.  '
                     'It is type %s with value %s'%(
                str(k),type(v).__name__,repr(v)))
        bad=True
if bad: sys.exit(1)

########################################################################
# Order the ATParser to create the XML file.

rocotoxml=StringIO.StringIO()
parser=produtil.atparse.ATParser(rocotoxml,varhash=VARS,logger=logger)
parser.parse_file('hwrf_workflow.xml.in')

outbase='hwrf-%s-%s-%s'%(
    conf.get('config','SUBEXPT'),
    stid.upper(),
    cycledesc)

if outxml is None: outxml=okpath(outbase+'.xml')
if outdb is None: outdb=okpath(outbase+'.db')
havexml=isnonempty(outxml)

if havexml:
    if not force and \
          not ask('ALERT! %s: XML file exists.  Overwrite (y/n)?'%(outxml,)):
        logger.error('%s: file exists, user does not want to overwrite.'
                     %(outxml,))
        sys.exit(1)
    else:
        logger.warning('%s: overwriting pre-existing XML file.'%(outxml,))

havedb=isnonempty(outdb)
deletedb=False
if havedb:
    if force or ask(
          'ALERT! %s: database for old configuration exists.  Use it (y/n)?'
          %(outdb,)):
        logger.warning('%s: not deleting database for old configuration.'
                       %(outdb,))
    elif ask('%s: Delete database for old configuration (y/n)?'%(outdb,)):
        logger.warning('%s: deleting database for old configuration.'
                       %(outdb,))
        remove_file(outdb)
    else:
        logger.error('%s: database exists, user does not want to delete '
                     'or use it.  Aborting.')
        sys.exit(2)

with open(outxml,'wt') as outf:
    outf.write(rocotoxml.getvalue())

########################################################################
# Run rocotorun

clustername=produtil.cluster.name()

if clustername in ('tide','gyre'):
    WHERE_AM_I='wcoss2'
else:
    WHERE_AM_I=clustername

cmd = exe('sh') [
    '--login', '-c', '. %s/hwrf_pre_job.sh.inc ; rocotorun --verbose=5 -d %s -w %s'
    %( shbackslash(USHhwrf), shbackslash(outdb), 
       shbackslash(outxml) ) ] .env(QUIET_PRE_JOB='YES',
                                    HOMEhwrf=HOMEhwrf,
                                    WHERE_AM_I=WHERE_AM_I) \
                      < '/dev/null'
result=run(cmd,logger=logger)
if result:
    sys.exit(result)
    produtil.jlogger.critical('rocotorun failed')

produtil.log.postmsg('Successfully ran rocotorun for %s.'%(outbase,))
bakdb=outdb+'.bak'
logger.info('Making a backup copy of .db file here: %s'%(bakdb,))
produtil.fileop.deliver_file(outdb,bakdb)
logger.info('Success. Rejoice: hurrah!')
