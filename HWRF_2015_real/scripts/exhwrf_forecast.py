#! /bin/env python

import os, sys
import produtil.setup, produtil.log, produtil.cluster
import hwrf.mpipomtc, hwrf_wcoss

def set_vars(coupled,logger,wrf_ranks):
    if produtil.cluster.name() in ['gyre','tide']:
        hwrf_wcoss.set_vars_for_coupled_hwrf(logger,coupled,wrf_ranks)
    else:
        logger.info('Not on WCOSS, so not setting WCOSS-specific vars.')

def doit():
    produtil.setup.setup()
    import hwrf_expt
    hwrf_expt.init_module()
    conf=hwrf_expt.conf
    logger=hwrf_expt.conf.log('exhwrf_forecast')

    ocean_flag=conf.getbool('config','run_ocean')
    fallbacks_flag=conf.getbool('config','allow_fallbacks')
    wrf_ranks=conf.getint('runwrf','wrf_ranks')

    if ocean_flag:
        ocean_success=hwrf.mpipomtc.get_ocstatus(conf,logger)

        if not hwrf_expt.pominit.is_completed():
            logger.warning('The pom_init completion flag is off.  '
                           'Ocean init failed.')
            ocean_success=False

        if not ocean_success:
            basin1=conf.syndat.basin1
            if basin1 in hwrf_expt.non_ocean_basins:
                produtil.log.postmsg(
                    'Cannot run ocean in this basin- run uncoupled.')
                set_vars(False,logger,wrf_ranks)
                hwrf_expt.runwrf.run(coupled=False)
                return
            elif fallbacks_flag:
                logger.critical(
                    'CRITICAL FAILURE: HWRF ocean init failed, but '
                    'fallbacks are enabled.  Running uncoupled.')
                set_vars(False,logger,wrf_ranks)
                hwrf_expt.runwrf.run(coupled=False)
                return
            else:
                logger.critical(
                    'CRITICAL FAILURE: HWRF ocean init failed, and '
                    'fallbacks are disabled.  Aborting.')
                sys.exit(1)
        else:
            produtil.log.postmsg('Ocean init succeeded.  Running coupled.')
            set_vars(True,logger,wrf_ranks)
    else:
        produtil.log.postmsg('Ocean is disabled.  Running uncoupled.')
        set_vars(False,logger,wrf_ranks)

    hwrf_expt.runwrf.run()
    produtil.log.postmsg('Forecast complete.')

if __name__=='__main__': 
    doit()
