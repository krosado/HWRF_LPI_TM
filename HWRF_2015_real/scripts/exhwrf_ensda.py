#! /bin/env python

import logging, os, sys, produtil.log, produtil.setup
from produtil.log import jlogger
import hwrf_wcoss

def fail(msg):
    jlogger.error(msg)
    sys.exit(2)

def set_vars(logger=None):
    if produtil.cluster.name() in ['gyre','tide']:
        hwrf_wcoss.set_vars_for_ensda_hwrf(logger)
    else:
        logger.info('Not on WCOSS, so not setting WCOSS-specific vars.')

def main():
    logger=logging.getLogger('exhwrf_ensda')
    ENV=os.environ
    memb=ENV.get('ENSDA_MEMB','NOPE').lower()
    if memb=='nope':
        fail('Aborting: you must specify ENSDA_MEMB')
    imemb=int(memb,10)
    jlogger.info('HWRF ensda member %03d starting'%imemb)

    set_vars(logger)

    import hwrf_expt
    hwrf_expt.init_module(make_ensemble_da=True)
    omemb=hwrf_expt.ensda.member(hwrf_expt.conf.cycle,imemb)
    omemb.run()
    for prod in omemb.products():
        if not prod.location:
            logger.error('No product: %s'%(prod.did,))
        elif not prod.available:
            logger.error('Product %s not available (location %s)'%(
                    repr(prod.did),repr(prod.location)))
        else:
            dest='%s/%s.ensda_%03d.%s'%(
                hwrf_expt.conf.getdir('com'),
                hwrf_expt.conf.getstr('config','out_prefix'),
                imemb,os.path.basename(prod.location))
            logger.info('%s %s: send to %s'%(
                    str(prod.did),repr(imemb),str(dest)))
            assert(os.path.isabs(dest))
            copier=hwrf_expt.wrfcopier.compression_copier(prod.location)
            if copier is None:
                logger.error('%s %s: not a NetCDF 3 file.'%(
                        str(prod.did),str(prod.location)))
                sys.exit(1)
            produtil.fileop.deliver_file(
                prod.location,dest,logger=logger,
                copier=copier)

    jlogger.info('HWRF ensda member %03d has completed'%imemb)

if __name__=='__main__':
    try:
        produtil.setup.setup()
        main()
    except Exception as e:
        jlogger.critical('HWRF ensda is aborting: '+str(e),exc_info=True)
        sys.exit(2)
