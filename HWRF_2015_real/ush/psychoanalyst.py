#! /bin/env python

"""This script runs all HWRF sanity checks that do not require a
specific cyle to run.  These are the same sanity checks run by the
Rocoto automation system in rocoto/run_hwrf.py, except that no cycles
are started."""

import os, logging,sys

if os.path.isdir('ush'):
    sys.path.append(os.path.realpath('ush'))

import produtil.setup
import hwrf.launcher

def usage(message=None,logger=None):
    print>>sys.stderr, '''Sanity check failed: incorrect arguments:
    psychoanalyst.py 18L HISTORY [conf files] [option arguments]

[conf files] - paths to *.conf files to parse after standard ones
[option arguments] - section.option=value arguments to set configuration
    options in individual sections'''
    if message is not None:
        logger.error('parse_launch_args says this: %s'%(str(message),))

produtil.setup.setup(send_dbn=False)

logger=logging.getLogger('hwrf.psychoanalyst')
logger.info('HWRF Psychoanalyst: running HWRF sanity checks.')

if os.path.isdir('../parm'):
    PARMhwrf=os.path.realpath('../parm')
    logger.info('Assuming PARMhwrf = ../parm = %s'%(PARMhwrf,))
elif os.path.isdir('parm'):
    PARMhwrf=os.path.realpath('parm')
    logger.info('Assuming PARMhwrf = parm = %s'%(PARMhwrf,))
else:
    logger.error('Cannot guess PARMhwrf path.  You should run this program from the ush directory or the directory above it.')

logger.info('Telling the hwrf.launcher to parse %s'%(' '.join(sys.argv[1:])))

(case_root,parm,infiles,stid,moreopt)=\
    hwrf.launcher.parse_launch_args(
    sys.argv[1:],logger,usage,PARMhwrf=PARMhwrf)

logger.info('Calling hwrf.launcher.launch...')
conf=hwrf.launcher.launch(infiles,None,stid,moreopt,case_root,init_dirs=False)

logger.info('Run sanity checks.')
try:
    conf.timeless_sanity_check(logger=logger)
except Exception as e:
    logger.info(str(e),exc_info=True)
    print '\n\n'
    logger.info('''Sanity check failed.  

Error message is: %s

See earlier messages for details.  Checklist for common errors:

1. Did you link the fix files?
2. Does your config.EXPT value match the installation directory name?
3. Did you run "make" and "make install" in the sorc directory?
4. Did you build and install, or link, the exec/hwrf_gsi?

The HWRF Psychoanalyist pronounces you insane.  Fix problems and try again.'''
                %(str(e),))
    sys.exit(1)

logger.info('Done.  The HWRF Psychoanalyst pronounces you sane.')
