#! /usr/bin/env python

# This script marks all post-processor products as having not been
# created.  It exists solely for the convenience of the operators: it
# allows an NCO SPA (operator) to click a button to rerun the entire
# post-processing system from the beginning.

import produtil.setup, produtil.datastore, produtil.log
import hwrf_expt
from produtil.datastore import *
def main():
    produtil.setup.setup()
    hwrf_expt.init_module()
    jlogger=produtil.log.jlogger
    jlogger.info('unpost starting')
    with hwrf_expt.ds.transaction():
        hwrf_expt.runwrf.state=UNSTARTED
        hwrf_expt.runwrf.unrun()
        hwrf_expt.wrfcopier.unrun()
        hwrf_expt.satpost.unrun()
        hwrf_expt.nonsatpost.unrun()
        hwrf_expt.gribber.unrun()
    jlogger.info('unpost completed')

if __name__ == '__main__': main()

