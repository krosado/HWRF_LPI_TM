#! /usr/bin/env python

import os, sys, re, logging, collections

if 'USHhwrf' in os.environ:
    sys.path.append(os.environ['USHhwrf'])
elif 'HOMEhwrf' in os.environ:
    sys.path.append(os.path.join(os.environ['HOMEhwrf'],'ush'))
else:
    guess_HOMEhwrf=os.path.dirname(os.path.dirname(
            os.path.realpath(__file__)))
    guess_USHhwrf=os.path.join(guess_HOMEhwrf,'ush')
    sys.path.append(guess_USHhwrf)

import produtil.setup, produtil.log, produtil.dbnalert
import hwrf.launcher
import hwrf_expt
from hwrf.numerics import to_datetime

logger=None
startdata="""
# Holdvars file with ksh variables:
holdvars="{holdvars}"

# Main conf file:
CONFhwrf="{CONFhwrf}"

# Cycle being run:
cycle={YMDH}

# Three character storm ID -- just number and basin letter:
stormid3="{vit[stormid3]}"

# Long storm ID:
longstormid="{vit[longstormid]}"
"""  # Don't forget the end of line before the """

def usage(logger):
    logger.critical('Invalid arguments to exhwrf_launch.py.  Aborting.')
    print '''
Usage: exhwrf_launch.py 2014062400 95E case_root /path/to/parm [options]

Mandatory arguments: 
  2014062400 -- the cycle to run
  95E -- the storm to run
  case_root -- FORECAST = real-time mode, HISTORY = retrospective mod
  /path/to/parm -- location of parm directory where standard conf files
      reside

Optional arguments:
section.option=value -- override conf options on the command line
/path/to/file.conf -- additional conf files to parse

Aborting due to incorrect arguments.'''
    sys.exit(2)

def main():
    logger=logging.getLogger('exhwrf_launch')
    PARAFLAG = ( 'YES' == os.environ.get('PARAFLAG','YES') )
    logger.info('Top of exhwrf_launch.')

    args=sys.argv[1:]

    if len(args)<4: usage(logger)

    # Find cycle:
    cycle=to_datetime(args[0])

    (case_root,parm,infiles,stid,moreopt) = \
        hwrf.launcher.parse_launch_args(args[1:],logger,usage)

    logger.info('Requested storm %s cycle %s case root %s'
                %(stid,cycle.strftime('%Y%m%d%H'),case_root))

    conf=hwrf.launcher.launch(infiles,cycle,stid,moreopt,case_root,
                              prelaunch=hwrf_expt.prelaunch)
    conf.sanity_check()

    if 'NO'==os.environ.get('PARAFLAG','YES'):
        message=conf.strinterp('wcoss_fcst_nco','{messages}/message{storm_num}')
        if os.path.exists(message):
            alert=produtil.dbnalert.DBNAlert(['MODEL','HWRF_MESSAGE','{job}',
                                              message])
            alert()
            
    holdvars=conf.strinterp('dir','{com}/{stormlabel}.holdvars.txt')
    logger.info(holdvars+': write holdvars here')
    with open(holdvars,'wt') as f:
        f.write(conf.make_holdvars())

    if conf.has_option('config','startfile'):
        startfile=conf.getstr('config','startfile')
        logger.info(startfile+': Write holdvars and conf location here.')
        startcontents=conf.strinterp('config',startdata,holdvars=holdvars)
        with open(startfile,'wt') as f:
            f.write(startcontents)

if __name__ == '__main__': 
    try:
        produtil.setup.setup()
        produtil.log.postmsg('exhwrf_launch is starting')
        main()
        produtil.log.postmsg('exhwrf_launch completed')
    except Exception as e:
        produtil.log.jlogger.critical(
            'exhwrf_launch failed: %s'%(str(e),),exc_info=True)
        sys.exit(2)
