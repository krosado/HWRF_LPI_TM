#! /bin/env python

"""This script exists only for dependency tracking with Rocoto.  It
creates the storm*.done file for a cycle in the case where the DA
ensemble is enabled.  If the DA ensemble is disabled, the
exhwrf_output job would do that instead."""

import sys, os
import produtil.setup, produtil.log
from produtil.log import jlogger
from hwrf.ensda import read_ensda_flag_file

def main():
    import hwrf_expt
    hwrf_expt.init_module(make_ensemble_da=True)

    conf=hwrf_expt.conf
    run_ensemble_da=conf.getbool('config','run_ensemble_da')
    ensda_flag_file=conf.getstr('tdrcheck','tdr_flag_file')
    run_ensda=read_ensda_flag_file(ensda_flag_file)
    if run_ensemble_da and run_ensda:
        ensda_size=conf.getint('hwrf_da_ens','ensda_size')
    else:
        jlogger.info('ENSDA was not run.')
        ensda_size=0

    logger=conf.log('output')
    
    bad=False
    for ens in xrange(ensda_size):
        imemb=ens+1
        omemb=hwrf_expt.ensda.member(hwrf_expt.conf.cycle,imemb)
        for prod in omemb.products():
            if not prod.location:
                logger.warning('ensda %03d: No product: %s'%(imemb,prod.did,))
                bad=True
            elif not prod.available:
                logger.warning(
                    'ensda %03d: product %s not available (location %s)'%(
                        imemb,repr(prod.did),repr(prod.location)))
                bad=True
            else:
                dest='%s/%s.ensda_%03d.%s'%(
                    hwrf_expt.conf.getdir('com'),
                    hwrf_expt.conf.getstr('config','out_prefix'),
                    imemb,os.path.basename(prod.location))
                if not os.path.exists(dest):
                    logger.warning('ensda %03d: %s: does not exist'
                                   %(imemb,dest,))
                    bad=True
                elif os.path.getsize(dest)<1:
                    logger.warning('ensda %03d: %s: is empty'%(imemb,dest))
                    bad=True
                else:
                    logger.info('ensda %03d: %s exists and is non-empty.'
                                %(imemb,dest))

    if bad:
        logger.critical(
            'HWRF data assimilation ensemble products are missing.')
        sys.exit(1)

    jlogger.info('Creating donefile.')
    donefile=os.path.join(conf.strinterp('config','{com}/{stormlabel}.done'))
    with open(donefile,'wt') as f:
        f.write('Cycle is complete.')

if __name__=='__main__':
    try:
        produtil.setup.setup()
        jlogger.info('ensda_output is starting')
        main()
        jlogger.info('ensda_output is completed')
    except Exception as e:
        jlogger.critical('ensda_output is aborting: '+str(e),exc_info=True)
        sys.exit(2)
