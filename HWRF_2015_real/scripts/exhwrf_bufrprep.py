#! /bin/env python

import os, sys, produtil.log, produtil.setup, produtil.cluster
from produtil.log import jlogger
import hwrf_wcoss,hwrf.gsi
from hwrf.gsi import unset_gsistatus, set_gsistatus

def fail(msg):
    jlogger.error(msg)
    sys.exit(2)

def main():
    
    import hwrf_expt
    hwrf_expt.init_module()
    conf=hwrf_expt.conf
    logger=conf.log('exhwrf_bufrprep')
    unset_gsistatus(conf,logger)

    if hwrf_expt.conf.getbool('config','run_gsi'):
        hwrf_expt.bufrprep.run()
    else:
        jlogger.info('GSI is disabled.  This job need not be run.')

    set_gsistatus(conf,logger)

if __name__=='__main__':
    try:
        produtil.setup.setup()
        main()
    except Exception as e:
        jlogger.critical('HWRF bufrprep is aborting: '+str(e),exc_info=True)
        sys.exit(2)
