#! /bin/env python

import os, sys, produtil.log, produtil.setup
from produtil.log import jlogger

def fail(msg):
    jlogger.error(msg)
    sys.exit(2)

def main():
    ENV=os.environ
    init_model=ENV['INIT_MODEL'].lower()
    init_fhr=int(ENV.get('INIT_FHR','0'))
    if init_model!='gfs' and init_model!='gdas1':
        fail('Aborting: init_model="%s" must be "gfs" or "gdas1"'%(init_model,))
    if init_model=='gdas1' and init_fhr<1:
        fail('Aborting: when init_model=gdas1, init_fhr must be >= 1 (init_fhr=%d)'%(init_fhr,))
    if init_model=='gfs': init_fhr=0
    
    import hwrf_expt
    hwrf_expt.init_module()
    if init_model=='gfs':
        jlogger.info('HWRF relocation for GFS fhr starting')
        init=hwrf_expt.gfs_init.run_relocate()
        jlogger.info('HWRF relocation for GFS fhr completed')
    elif not hwrf_expt.conf.getbool('config','run_gsi'):
        jlogger.info('GSI is disabled.  This job need not be run.')
        sys.exit(0)
    else:
        init=None
        logger=hwrf_expt.fgat_init.log()
        logger.info('search for fgat hour %d'%(init_fhr,))
        for fhr,init in hwrf_expt.fgat_init.fhr_and_init():
            if abs(fhr-init_fhr)<0.01:
                logger.info('fhr %d is init_fhr %d'%(fhr,init_fhr))
                jlogger.info('HWRF relocation for GDAS1 fhr %d starting'%fhr)
                init.run_relocate()
                jlogger.info('HWRF relocation for GDAS1 fhr %d completed'%fhr)
                break
            else:
                logger.info('fhr %d is not init_fhr %d'%(fhr,init_fhr))
        assert(init is not None)

if __name__=='__main__':
    try:
        produtil.setup.setup()
        main()
    except Exception as e:
        jlogger.critical('HWRF relocation is aborting: '+str(e),exc_info=True)
        sys.exit(2)
