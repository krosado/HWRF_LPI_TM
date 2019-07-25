#! /bin/env python

import os, sys
import produtil.log, produtil.setup
import pom.exceptions
import hwrf.mpipomtc
from produtil.log import jlogger

def main():
    import hwrf_expt
    hwrf_expt.init_module()
    conf=hwrf_expt.conf
    logger=conf.log('exhwrf_ocean_init')
    hwrf.mpipomtc.unset_ocstatus(conf,logger)
    try:
        if not conf.getbool('config','run_ocean'):
            jlogger.info('Ocean is disabled.  This job need not be run.')
            hwrf.mpipomtc.set_ocstatus(conf,False,logger)
            return
        hwrf_expt.pominit.run()
        hwrf.mpipomtc.set_ocstatus(conf,True,logger)
    except pom.exceptions.POMUnsupportedBasin as e:
        produtil.log.postmsg('Unsupported basin: will run without ocean.')
        hwrf.mpipomtc.set_ocstatus(conf,False,logger)
        return
    except Exception as e:
        if conf.getbool('config','allow_fallbacks',False):
            logger.error('Could not run ocean init: will run without ocean.'
                         '  Unhandled exception: '+str(e),exc_info=True)
            hwrf.mpipomtc.set_ocstatus(conf,False,logger)
            return
        raise

if __name__=='__main__':
    try:
        produtil.setup.setup()
        main()
    except Exception as e:
        jlogger.critical('HWRF POM ocean init is aborting: '
                         +str(e),exc_info=True)
        sys.exit(2)
