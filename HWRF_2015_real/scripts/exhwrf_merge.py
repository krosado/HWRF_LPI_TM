#! /bin/env python

import os, sys, produtil.log, produtil.setup
from produtil.log import jlogger
import hwrf.gsi

def fail(msg):
    jlogger.error(msg)
    sys.exit(2)

def main():
    ENV=os.environ
    init_model=ENV.get('INIT_MODEL','GDAS1').lower()
    if init_model!='gfs' and init_model!='gdas1':
        fail('Aborting: init_model="%s" must be "gfs" or "gdas1"'
             %(init_model,))
    
    import hwrf_expt
    hwrf_expt.init_module()
    conf=hwrf_expt.conf
    logger=conf.log('exhwrf_merge')
    if init_model=='gfs':
        jlogger.info('MERGE does not need to be run for INIT_MODEL=GFS')
        hwrf_expt.gfs_merge.run()
    elif not hwrf_expt.conf.getbool('config','run_gsi'):
        jlogger.info('GSI is disabled via configuration settings.  '
                     'This job need not be run.')
        sys.exit(0)
    elif not hwrf.gsi.get_gsistatus(conf,'gsi_d02',logger) and \
         not hwrf.gsi.get_gsistatus(conf,'gsi_d03',logger):
        jlogger.info('GSI status file claims GSI is disabled for both '
                     'domains.  This job need not be run.')
        sys.exit(0)
    else:
        hwrf_expt.gdas_merge.run()

if __name__=='__main__':
    try:
        produtil.setup.setup()
        jlogger.info('exhwrf_merge is starting')
        main()
        jlogger.info('exhwrf_merge has completed')
    except Exception as e:
        jlogger.critical('HWRF merge is aborting: '+str(e),exc_info=True)
        sys.exit(2)
