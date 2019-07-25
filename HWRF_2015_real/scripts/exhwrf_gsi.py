#! /bin/env python

import logging
import os, sys, produtil.log, produtil.setup, produtil.cluster
from produtil.log import jlogger
import hwrf_wcoss
import hwrf.gsi

def fail(msg):
    jlogger.error(msg)
    sys.exit(2)

def main():
    ENV=os.environ
    gsi_domain=ENV['GSI_DOMAIN'].lower()
    if gsi_domain!='d02' and gsi_domain!='d03':
        fail('Aborting: gsi_domain="%s" must be "d02" or "d03"'%(gsi_domain,))
    
    import hwrf_expt
    hwrf_expt.init_module()
    logger=hwrf_expt.conf.log('exhwrf_gsi')

    if not hwrf_expt.conf.getbool('config','run_gsi'):
        jlogger.info('GSI is disabled.  This job need not be run.')
        sys.exit(0)

    if produtil.cluster.name() in ['gyre','tide']:
        hwrf_wcoss.set_vars_for_gsi(logger)
    else:
        logger.info('Not on WCOSS, so not setting WCOSS-specific vars.')

    if not hwrf.gsi.get_gsistatus(hwrf_expt.conf,'gsi_'+gsi_domain,logger):
        jlogger.info('GSI is disabled for %s.  This job need not be run.'
                     %(gsi_domain,))
        sys.exit(0)
    else:
        logger.info('GSI is enabled for %s.'%(gsi_domain,))

    if gsi_domain=='d02':
        hwrf_expt.gsi_d02.run()
    else:
        hwrf_expt.gsi_d03.run()

if __name__=='__main__':
    try:
        produtil.setup.setup(thread_logger=True,eloglevel=logging.INFO)
        main()
    except Exception as e:
        jlogger.critical('HWRF GSI is aborting: '+str(e),exc_info=True)
        sys.exit(2)
