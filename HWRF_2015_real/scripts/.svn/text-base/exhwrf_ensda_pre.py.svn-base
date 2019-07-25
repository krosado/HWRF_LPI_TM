#! /bin/env python

"""This script determines whether the ENSDA needs to be run for this
cycle."""

import sys, os
import produtil.setup, produtil.log
from produtil.log import jlogger

def main():
    import hwrf_expt
    hwrf_expt.init_module(make_ensemble_da=True)

    conf=hwrf_expt.conf
    run_ensemble_da=conf.getbool('config','run_ensemble_da')
    if not run_ensemble_da:
        jlogger.info('ENSDA is disabled for this configuration.  '
                     'This job need not be run.')
        hwrf_expt.ensda_pre.write_flag_file(False)
        return
    hwrf_expt.ensda_pre.run()

if __name__=='__main__':
    try:
        produtil.setup.setup()
        jlogger.info('ensda_pre is starting')
        main()
        jlogger.info('ensda_pre is completed')
    except Exception as e:
        jlogger.critical('ensda_pre is aborting: '+str(e),exc_info=True)
        sys.exit(2)

