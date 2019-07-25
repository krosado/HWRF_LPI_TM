#! /usr/bin/env python

import logging, sys, os, getopt
import produtil.sigsafety, produtil.fileop
import hwrf.revital

# Install SIGTERM, SIGINT, etc. handlers.  Needed to handle batch job
# kills correctly.
produtil.sigsafety.install_handlers()

# Set up logging.  We customize things to look nice.
handler=logging.StreamHandler(sys.stderr)
handler.setFormatter(logging.Formatter(
        'hwrf_revital:%(levelname)s:%(asctime)s: %(message)s',
        '%Y%m%d.%H%M%S'))
logger=logging.getLogger()
logger.setLevel(logging.INFO)
logger.addHandler(handler)

forecast=False

inputs=[]
tcvlocs=[]
messagedir=[]

PARA = ( 'YES' == os.environ.get('PARAFLAG','YES') )

########################################################################
# THIS SECTION NEEDS HARD-CODED PATHS FOR EMC ##########################
# HARD-CODED PATHS ARE PROTECTED BY PARAFLAG=YES BLOCK BELOW ###########
def set_para_paths():
    global tcvlocs, messagedir, inputs
    tcvlocs=[
        "/scratch1/portfolios/NCEPDEV/hwrf/noscrub/input/SYNDAT-PLUS",
        "/scratch1/portfolios/NCEPDEV/hwrf/noscrub/input/SYNDAT",
        "/lfs3/projects/hwrf-data/hwrf-input/SYNDAT-PLUS",
        "/lfs3/projects/hwrf-data/hwrf-input/SYNDAT",
        "/hwrf/noscrub/input/SYNDAT-PLUS",
        "/hwrf/noscrub/input/SYNDAT"
        ]
    messagedir=[
        "/scratch1/portfolios/NCEPDEV/hwrf/noscrub/input/MESSAGES",
        "/lfs1/projects/hwrf-vd/hwrf-input/MESSAGES",
        "/com/hur/prod/inpdata"
        ]
    if 'CASE_ROOT' in os.environ and os.environ['CASE_ROOT']=='FORECAST':
        tcvlocs=['/com/arch/prod/syndat']
    else:
        tcvlocs.append('/com/arch/prod/syndat')
# END OF SECTION WITH HARD-CODED PATHS #################################
########################################################################

# PARSE ARGUMENTS

renumber=True
format='tcvitals'
threshold=14
try:
    (optlist,args) = getopt.getopt(sys.argv[1:],'HvnW')
    for opt,val in optlist:
        if   opt=='-v': 
            logger.setLevel(logging.DEBUG)
            logger.debug('Verbosity enabled.')
        elif   opt=='-W':
            threshold=int(val)
            logger.debug('Weak storm threshold is now %d'%(threshold,))
        elif opt=='-n': 
            renumber=False
            logger.info('Disabling renumbering due to -n')
        elif opt=='-H': format='HHS'
        else:
            logger.error('Invalid option %s'%(opt,))
            sys.exit(1)
except (getopt.GetoptError,ValueError,TypeError) as e:
    usage('Error parsing arguments')
    sys.exit(1)
    
########################################################################
# DECIDE VITALS LOCATIONS
# THIS PARA BLOCK PROTECTS AGAINST NCO REACHING HARD-CODED PATHS:
if PARA:
    set_para_paths()
else:
    # Find tcvitals:
    if 'COMINARCH' in os.environ:
        tcvlocs = [ os.environ['COMINARCH'], ]
    elif 'envir' in os.environ:
        tcvlocs = [ '/com/arch/'+envir+'/syndat', ]
    else:
        fail('ERROR: Both $COMINARCH and $envir are unset in '
             '$PARAFLAG=NO mode.  Cannot find tcvitals.')
    # Find message files
    if 'mesagdir' in os.environ:
        messagedir = [ os.environ['mesagdir'], ]
    elif 'envir' in os.environ:
        messagedir = [ '/com/hur/'+envir+'/inpdata', ]
    else:
        fail('ERROR: Both $mesagdir and $envir are unset in '
             '$PARAFLAG=NO mode.  Cannot find tcvitals.')
########################################################################

if 'CASE_ROOT' in os.environ and os.environ['CASE_ROOT']=='FORECAST':
    for d in messagedir:
        if os.path.isdir(d):
            inputs.extend([os.path.join(d,'message%d'%(1+x,)) \
                               for x in xrange(5)])
            break

if len(args)<2:
    print>>sys.stderr,'ERROR: Script requires at least two '\
        'arguments: stormid and year'
    sys.exit(1)

stormid=str(args[0]).upper()
stormnum=int(stormid[0:2])
tcvyears_in=[ int(x) for x in str(args[1]).split()]
tcvyears=list()
xset=set()

def check_test_vitals(vl):
    """This is a replacement for hwrf.storminfo.name_number_okay for
    use with TEST storms and internal stormids.  It allows through
    only the storm numbers matching stormnum, regardless of the 
    storm name (usually TEST and UNKNOWN would be dropped)."""
    logger.info('Keeping only storm number %d in vitals'%(stormnum,))
    for vital in vl:
        if vital.stnum==stormnum:
            yield vital

for tcvyear in tcvyears_in:
    if tcvyear not in xset:
        xset.add(tcvyear)
        tcvyears.append(tcvyear)

if len(args)>2 and args[2]!='':
    renumberlog=open(str(sys.argv[3]),'wt')
else:
    renumberlog=None

if len(args)>3:
    for tcvyear in tcvyears:
        tcvfile=os.path.join(str(args[3]),'syndat_tcvitals.%04d'%(tcvyear,))
        if not os.path.isdir(tcvfile):
            logger.error('%s: syndat file does not exist'%(tcvfile,))
            sys.exit(1)
        inputs.append(tcvfile)
else:
    for tcvyear in tcvyears:        
        for thatdir in tcvlocs:
            thatfile=os.path.join(thatdir,'syndat_tcvitals.%04d'%(tcvyear,))
            if produtil.fileop.isnonempty(thatfile):
                inputs.append(thatfile)
                break
            else:
                logger.debug('%s: empty or non-existent'%(thatfile,))

try:
    revital=hwrf.revital.Revital(logger=logger)
    logger.info('List of input files: %s'%( repr(inputs), ))
    logger.info('Read input files...')
    revital.readfiles(inputs,raise_all=False)
    if not renumber:
        logger.info(
            'Not renumbering because renumbering is disabled via -n')
        logger.info('Cleaning up vitals instead.')
        revital.clean_up_vitals()
    elif stormnum<50:
        logger.info('Renumber invests with weak storm threshold %d...'
                    %(threshold,))
        revital.renumber(threshold=threshold)
    elif stormnum>=90:
        logger.info('Not renumbering invests when storm of '
                    'interest is 90-99.')
        logger.info('Cleaning up vitals instead.')
        revital.clean_up_vitals()
    else:
        logger.info('Fake stormid requested.  Running limited clean-up.')
        revital.clean_up_vitals(name_number_checker=check_test_vitals)
    logger.info('Reformat vitals...')
    revital.print_vitals(sys.stdout,renumberlog=renumberlog,
                         stormid=stormid,format=format)
except Exception as e:
    logger.info(str(e),exc_info=True)
    logger.critical('ERROR: %s'%(str(e),))
