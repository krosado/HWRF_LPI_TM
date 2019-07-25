#! /bin/env python

import os, sys
import produtil.setup, produtil.log
import hwrf.mpipomtc
import hwrf_expt
from produtil.log import jlogger

def check_ocean_init(logger):
    conf=hwrf_expt.conf
    if not hwrf.mpipomtc.get_ocstatus(conf,logger):
        logger.warning('The ocean status file says ocean is disabled.')
        return False

    if not hwrf_expt.pominit.is_completed():
        logger.warning('The pominit completion flag is off.  '
                       'Ocean init failed.')
        return False

    return True

def check_gsi(logger):
    okay=True
    conf=hwrf_expt.conf
    if hwrf.gsi.get_gsistatus(conf,'gsi_d02',logger) and not hwrf_expt.gsi_d02.is_completed():
        logger.warning(
            'GSI gsi_d02 Task state is not COMPLETED.  Domain 2 GSI failed.')
        okay=False

    if hwrf.gsi.get_gsistatus(conf,'gsi_d03',logger) and not hwrf_expt.gsi_d03.is_completed():
        logger.warning(
            'GSI gsi_d03 Task state is not COMPLETED.  Domain 3 GSI failed.')
        okay=False
    return okay

def check_relocate(conf,who,init,relocate,logger,when):
    i=init
    r=relocate
    okay=True
    if not r.completed:
        logger.warning('%s: FAIL: relocation job state is not COMPLETE (state %s)'
                       %(who,r.strstate))
        okay=False
    for domain in i.wrfghost:
        # Get the product for this domain and time:
        if domain.is_moad():
            p=r.wrfinput_at_time(when,domain)
        else:
            p=r.get_ghost(domain)
            
        # Is the product available, and does it have a valid location?
        if not p.available:
            logger.warning('%s: FAIL: relocation output product %s '
                           'not available'%(who,p.did))
            okay=False
        if p.location is None or p.location=='':
            logger.warning('%s: FAIL: relocation output product %s has '
                           'no location'%(who,p.did))
            okay=False
        elif not produtil.fileop.isnonempty(p.location):
            logger.warning(
                '%s: FAIL: relocation output %s is empty or does not exist at '
                '%s'%(who,p.did,p.location))
            okay=False

        # Get the cold/warm start info and such:
        try:
            rinfo=r.rinfo
            logger.info('%s: relocation info: %s'%(who,rinfo))
            logger.info('%s:   from file: %s'%(who,rinfo.from_file))
            if rinfo.warm_cold_flag is None:
                logger.warning(
                    '%s: FAIL: warm/cold state is unknown (None).'%(who,))
                okay=False
            elif rinfo.warm_cold_flag is not hwrf.relocate.WARM:
                if rinfo.cold_ok:
                    logger.info('%s: cold start, but cold_ok=True'%(who,))
                elif conf.getbool('config','expect_cold_start',False):
                    logger.warning('%s: cold start: no prior cycle found'%(who,))
                else:
                    logger.error('%s: FAIL: unexpected cold start'%(who,))
                    okay=False
            else:
                logger.info('%s: warm start'%(who,))
        except Exception as e:
            logger.error('%s: error determining cold vs. warm start: %s'
                         %(who,str(e)),exc_info=True)
            okay=False
    return okay

def check_fgat_relocate(conf,logger):
    okay=True
    reloc=hwrf_expt.fgat_init.get_relocates()
    for t,r in reloc.iteritems():
        i=hwrf_expt.fgat_init.init_at_time(t)
        who="FGAT time "+t.strftime('%Y%m%d%H')
        if not check_relocate(conf,who,i,r,logger,t):
            okay=False
            logger.error(who+': failed.')
    return okay

def check_gfs_relocate(conf,logger):
    if not check_relocate(conf,'GFS relocate',hwrf_expt.gfs_init,
                          hwrf_expt.gfs_init.rstage3,logger,
                          hwrf_expt.conf.cycle):
        logger.error('GFS relocate failed.')
        return False
    return True

def main():
    produtil.setup.setup()
    hwrf_expt.init_module()
    conf=hwrf_expt.conf
    logger=conf.log("check_init")

    ocean_flag=conf.getbool('config','run_ocean')
    gsi_flag=conf.getbool('config','run_gsi') 
    reloc_flag=conf.getbool('config','run_relocation')

    okay=True

    if ocean_flag:
        if check_ocean_init(logger):
            logger.info('Ocean init succeeded.')
        elif conf.syndat.basin1 in hwrf_expt.non_ocean_basins:
            logger.info('Ocean init aborted, but basin is not supported.')
        else:
            logger.error('Ocean init failed.')
            okay=False
    else:
        logger.info('Ocean is disabled.  Skipping ocean checks.')

    if gsi_flag:
        if not check_gsi(logger):
            logger.error('GSI failed.')
            okay=False
        if not check_fgat_relocate(conf,logger):
            logger.error('FGAT relocate failed.')
            okay=False
    elif reloc_flag:
        logger.info('GSI is disabled.  Skipping GSI and FGAT '
                    'relocation checks.')
        if not check_gfs_relocate(conf,logger):
            logger.error('GFS relocate failed.')
    else:
        logger.info('Relocation and GSI are disabled.  '
                    'Skipping relocation checks.')

    logger.info('Asking the forecast object to check if all input files '
                'are available.')
    print type(hwrf_expt.runwrf).__name__
    have_input=hwrf_expt.runwrf.check_all_inputs()
    if not have_input:
        okay=False
        logger.error('FAILURE: WRF or POM inputs are missing')
    if not have_input:
        logger.critical('FORECAST INPUTS ARE MISSING!!')
        sys.exit(1)
    elif not okay:
        logger.critical('INIT JOBS DID NOT SUCCEED!!')
        sys.exit(1)

if __name__=='__main__':
    try:
        main()
    except Exception as e:
        jlogger.critical('HWRF check init is aborting: '+str(e),exc_info=True)
        sys.exit(2)

