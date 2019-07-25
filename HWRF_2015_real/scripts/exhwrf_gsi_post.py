#! /bin/env python
import os, sys
import produtil.setup, produtil.log, produtil.cd
import hwrf_expt
from produtil.log import jlogger

def main():
    hwrf_expt.init_module()
    logger=hwrf_expt.conf.log('exhwrf_gsi_post')

    if not hwrf_expt.conf.getbool('config','run_gsi'):
        jlogger.info('GSI is disabled.  This job need not be run.')
        sys.exit(0)

    produtil.fileop.chdir(hwrf_expt.conf.getdir('WORKhwrf'),logger=logger)

    logger.info('Unrun GSI post and gribber')
    hwrf_expt.gsipost.unrun()
    hwrf_expt.gsigribber.unrun()

    logger.info('Run GSI post')
    hwrf_expt.gsipost.run()

    logger.info('Run GSI gribber, and deliver to com.')
    hwrf_expt.gsigribber.run(now=True)

if __name__=='__main__':
    try:
        produtil.setup.setup()
        jlogger.info("HWRF GSI post job starting")
        main()
        jlogger.info("HWRF GSI post job completed")
    except Exception as e:
        jlogger.critical('HWRF GSI post is aborting: '+str(e),
                         exc_info=True)
        sys.exit(2)
