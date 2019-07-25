"""This module contains utility functions for the
hwrf.launcher.launch() prelaunch argument.  These functions edit the
configuration of an individual cycle before the cycle starts."""

import os
import hwrf.numerics
import produtil.fileop

__all__=['prelaunch_ungrib','prelaunch_rsmc','prelaunch_basin']

def prelaunch_ensid(conf,logger):
    ens_overrides=conf.getbool('prelaunch','ensid_overrides')
    if not ens_overrides:
        logger.info('Ensemble ID overrides are disabled.')
        return
    ens=conf.getint('config','ENS',99)
    item=conf.get('ungrib','item_E%02d'%ens,'')
    item2=conf.get('ungrib','item2_E%02d'%ens,'')
    if item:
        logger.info('Overriding [ungrib] item=item_E%02d=%s'%(ens,item))
        conf.set('ungrib','item',item)
    if item2:
        logger.info('Overriding [ungrib] item2=item2_E%02d=%s'%(ens,item2))
        conf.set('ungrib','item2',item2)

def prelaunch_ungrib(conf,logger,cycle):
    ungrib_overrides=conf.getbool('prelaunch','ungrib_overrides') 
    if not ungrib_overrides:
        logger.info('Ungrib overrides are disabled.')
        return

    # We're in an exhwrf_launch job, not in run_hwrf.py
    cyc=hwrf.numerics.to_datetime(cycle)   # the cycle as a datetime
   
    # Replace [ungrib] tbl with per-year data
    tblYEARname=cyc.strftime("tbl%Y")  # ie.: tbl2011 for 2011090418
    tblYEARvalue=conf.get("ungrib",tblYEARname,'')
    if tblYEARvalue:
        conf.set("ungrib",'tbl',tblYEARvalue)

def prelaunch_rsmc(conf,logger,cycle):
    rsmc_overrides=conf.getbool('prelaunch','rsmc_overrides')
    if not rsmc_overrides:
        logger.info('RSMC overrides are disabled.')
        return

    vit=conf.syndat
    rsmc=str(vit.center).upper()
    rfile=conf.strinterp('prelaunch','{rsmc_conf}',RSMC=rsmc)
    if not produtil.fileop.isnonempty(rfile):
        logger.warning('%s: RSMC override file is empty or non-existant'
                       %(rfile,))
    conf.read(rfile)

def prelaunch_basin(conf,logger,cycle):
    basin_overrides=conf.getbool('prelaunch','basin_overrides')
    if not basin_overrides:
        logger.info('Basin overrides are disabled.')
        return

    vit=conf.syndat
    if vit is None:
        logger.warning('Cannot use basin overrides - conf.syndat is None')
        return
    bfile=conf.strinterp('prelaunch','{basin_conf}',vit=vit)
    nfile=conf.strinterp('prelaunch','{no_basin_conf}',vit=vit)

    if os.path.exists(bfile):
        logger.warning('%s: reading basin override file'%(bfile,))
        conf.read(bfile)
    elif os.path.exists(nfile):
        logger.warning('%s: basin override enabled, but file is '
                       'missing or empty; will read %s instead.'
                       %(bfile,nfile))
        conf.read(nfile)
    else:
        logger.warning('%s: basin override enabled, and no "no_basin_file"'
                       'is available at %s; will not override defaults.'
                       %(bfile,nfile))

