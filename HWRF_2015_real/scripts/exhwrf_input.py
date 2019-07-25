#! /bin/env python

import logging, os, sys
import produtil.log, produtil.setup, produtil.run, produtil.fileop
from produtil.log import jlogger
from produtil.run import exe
import hwrf.input

def main():
    import hwrf_expt
    hwrf_expt.init_module(make_ensemble_da=True)

    conf=hwrf_expt.conf
    cycle=hwrf_expt.conf.cycle
    input_catalog=conf.get('config','input_catalog')
    input_sources=conf.get('config','input_sources')
    logger=conf.log('exhwrf_input')
    WORKhwrf=conf.getdir('WORKhwrf')

    if input_catalog!='hwrfdata':
        jlogger.info("Input catalog is %s, not \"hwrfdata\" so data should "
                     "be staged on disk already.  I have nothing to do, so "
                     "I'll just exit.  This is not an error.")
        sys.exit(0)

    # Make sure we're in the cycle's work directory, otherwise we might
    # pull archives and other big things to $HOME.
    produtil.fileop.chdir(WORKhwrf,logger=logger)

    # Figure out how to run htar:
    htar=exe(conf.getexe('htar'))

    # Figure out how to run hsi:
    hsi=exe(conf.getexe('hsi'))

    # Get the list of data to pull:
    data=list(d for d in hwrf_expt.inputiter())

    # Decide where to put the data:
    cat=hwrf.input.DataCatalog(conf,"hwrfdata",cycle)

    # Now pull the data:
    getem=hwrf.input.InputSource(conf,input_sources,conf.cycle,
                                 htar=htar,hsi=hsi,logger=logger)
    bad=not getem.get(data,cat)
    if bad:
        jlogger.error('Missing data in exhwrf_input.  Workflow may fail.')
        sys.exit(1)

if __name__=='__main__':
    try:
        produtil.setup.setup(thread_logger=True,eloglevel=logging.INFO)
        jlogger.info("HWRF input job starting")
        main()
        jlogger.info("HWRF input job completed")
    except Exception as e:
        jlogger.critical('HWRF input is aborting: '+str(e),exc_info=True)
        sys.exit(2)
