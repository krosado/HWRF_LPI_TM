__all__=[] # Ensure that accidental "from hwrf_expt import *" does nothing

import os,os.path

import pdb
import produtil.datastore, produtil.run

import hwrf.config, hwrf.wrf, hwrf.post, hwrf.numerics, hwrf.launcher
import hwrf.regrib, hwrf.gribtask, hwrf.tracker, hwrf.storminfo
import hwrf.wps, hwrf.nhc_products, hwrf.copywrf, hwrf.fcsttask, hwrf.ensda
import hwrf.relocate, hwrf.init, hwrf.prep, hwrf.gsi, hwrf.mpipomtc
import hwrf.bufrprep, hwrf.gsipost, hwrf.prelaunch

import hwrf.hwrfsystem

from produtil.run import exe,alias
from produtil.fileop import isnonempty
from hwrf.wrf import WRFDomain,WRFSimulation,ExternalWRFTask
from hwrf.post import PostManyWRF
from hwrf.regrib import RegribMany,igrb1,clatlon,GRIB2

def prelaunch(conf,logger,cycle):
    """This function makes per-cycle modifications to the
    configuration file storm1.conf.  This is called in exhwrf_launch
    and run_hwrf.py by hwrf.launcher.launch on the configuration
    object (HWRFLauncher, a subclass of HWRFConfig), before the
    per-cycle storm1.conf configuration file is written.  Any changes
    made to the conf object will be stored in storm1.conf file and
    used in later jobs.  This allows modifications to the
    configuration on a per-cycle basis.  Note that cycle=None and
    conf.cycle is unavailable when run_hwrf.py calls prelaunch."""

    # Prelaunch that happens even when running run_hwrf.py:
    hwrf.prelaunch.prelaunch_ensid(conf,logger)

    if cycle is None: return

    # Prelaunch that only happens in exhwrf_launch, not run_hwrf.py:
    hwrf.prelaunch.prelaunch_ungrib(conf,logger,cycle)
    hwrf.prelaunch.prelaunch_rsmc(conf,logger,cycle)
    hwrf.prelaunch.prelaunch_basin(conf,logger,cycle)

def sanity_check(logger):
    """Runs a sanity check on this module's contents.  This should be
    called after init_module."""
    logger.info('The runwrf object = %s'%(repr(runwrf),))
    logger.info('The gribber object = %s'%(repr(gribber),))
    logger.info('The datastore (ds) object = %s'%(repr(ds),))
    logger.info('The wrf object = %s'%(repr(wrf),))

    # Make sure the mandatory configuration variables are specified:
    gsi_flag=conf.getbool('config','run_gsi') 
    ocean_flag=conf.getbool('config','run_ocean')
    reloc_flag=conf.getbool('config','run_relocation')
    spectral_flag=conf.getbool('config','use_spectral')
    spectral_bdy=conf.getbool('config','spectral_bdy') and spectral_flag
    fallbacks_flag=conf.getbool('config','allow_fallbacks')
    extra_trackers=conf.getbool('config','extra_trackers',False)
    wrf_output_step=conf.getint('forecast_products','wrf_output_step',10800)
    if ocean_flag:
        logger.info('The pominit object = %s'%(repr(pominit),))

    if wrf_output_step<1:
        raise hwrf.exceptions.PrecisionTooHigh(
            'The wrf_output_step must be at least 1 second')
    elif wrf_output_step!=10800 and 0 != (3600%wrf_output_step):
        raise hwrf.exceptions.TimestepModularityError(
            'One hour (3600 seconds) must be divisable by the WRF '
            'output timestep.  You specified %d.'%(wrf_output_step,))

    # Make sure the configuration makes sense and is supported:
    if gsi_flag and not reloc_flag:
        logger.error("Cannot use GSI without relocation.")
        raise hwrf.exceptions.HWRFConfigUnsupported(
            "Cannot use GSI without relocation.")

def inputiter():
    """Iterates over all inputs required by this configuration.  Can
    be passed into the "data" argument of hwrf.input.InputSource.get.
    Iterates over dicts that contain the following:
      dataset - string name of the dataset (gfs, gdas1, gefs,
        enkf, etc.)
      item - string name of the object (ie.: gfs_sf, gfs_sfcanl, bufr)
      atime - self.conf.cycle
      ftime - only present when relevant: the forecast time, in a 
        format accepted by to_datetime_rel
      enkfmem - only present when relevant: the ENKF member ID
      obstype - only present when relevant: the bufr data type."""  
    gsi_flag=conf.getbool('config','run_gsi') 
    run_ensemble_da=conf.getbool('config','run_ensemble_da',False)
    spectral_flag=conf.getbool('config','use_spectral')
    spectral_bdy=conf.getbool('config','spectral_bdy') and spectral_flag
    ocean_flag=conf.getbool('config','run_ocean')
    conditional_gsid03=conf.getbool('config','conditional_gsid03',False) 
    tdrflagfile=conf.strinterp('dir','{com}/{stormlabel}.tdr')
    realtime=conf.getbool('config','realtime')
    if not conditional_gsid03 or (conditional_gsid03 and isnonempty(tdrflagfile)):
        gsid03_flag=True
    else:
        gsid03_flag=False

    if ocean_flag:
        for d in pominit.inputiter(): yield d

    for d in gfs_init.inputiter(): yield d
    if gsi_flag:
        for d in gsi_d02.inputiter(): yield d
        if gsid03_flag:
            for d in gsi_d03.inputiter(): yield d
        for d in fgat_init.inputiter(): yield d
        if run_ensemble_da:
            for d in ensda.inputiter(): yield d
            if not realtime or not os.path.isdir('/dcom/us007003'): 
                for d in ensda_pre.inputiter(): yield d

def init_module(CONFhwrf=None,make_ensemble_da=True,make_post=True):
    global conf,ds,moad,storm1outer,storm1inner,wrf, anl4trak
    global runwrf,nonsatpost,satpost,hwrfsub,stormloc,domloc, gfs_init
    global gribber, tracker, WRFOUThwrf, coupled, pominit
    global nhcp, wrfcopier, real12, real126, WORKhwrf, HOMEhwrf
    global rel_stage1, rel_stage2, rel_stage3, fgat_init, non_ocean_basins
    global bufrprep, gsi_d02, gsi_d03, gsid03_flag, gdas_merge, cycle, prior_ensda
    global ensda, entest, da_ensemble_size, trackerd01, trackerd02
    global gsigribber, gsipost, ensda_pre

    # Figure out where to find the config file if it was not supplied
    # as an argument:
    if CONFhwrf is None:
        CONFhwrf=os.environ['CONFhwrf']
    
    # Generate the HWRFConfig object, which stores experiment
    # configuration, processor counts and so on.  It also knows how to
    # automatically generate other critical objects like the Datastore:
    conf=hwrf.launcher.load(CONFhwrf)
    conf.log().info('Initializing hwrf_expt module...')

    # Convenience variables:
    cycle=conf.cycle
    WORKhwrf=conf.getdir('WORKhwrf')
    HOMEhwrf=conf.getdir('HOMEhwrf')

    # Major configuration variables:
    gsi_flag=conf.getbool('config','run_gsi') 
    run_ensemble_da=conf.getbool('config','run_ensemble_da',False)
    make_ensemble_da=make_ensemble_da and run_ensemble_da
    satpost_flag=conf.getbool('config','run_satpost',True)
    ocean_flag=conf.getbool('config','run_ocean')
    reloc_flag=conf.getbool('config','run_relocation')
    spectral_flag=conf.getbool('config','use_spectral')
    spectral_bdy=conf.getbool('config','spectral_bdy') and spectral_flag
    fallbacks_flag=conf.getbool('config','allow_fallbacks')
    extra_trackers=conf.getbool('config','extra_trackers',False)
    conditional_gsid03=conf.getbool('config','conditional_gsid03',False)
    conditional_gsid02=conf.getbool('config','conditional_gsid02',False)
    fcstlen=conf.getint('config','forecast_length',126)
    fgatstr=conf.getint('fgat','FGATSTR',-3)
    fgatend=conf.getint('fgat','FGATEND',3)
    fgatinv=conf.getint('fgat','FGATINV',3)
    tdrflagfile=conf.strinterp('dir','{com}/{stormlabel}.tdr')
    if not conditional_gsid03 or (conditional_gsid03 and isnonempty(tdrflagfile)):
        gsid03_flag=True
    else:
        gsid03_flag=False

    # ----------------------------------------------------------------------
    # Define the HWRF workflow

    # Create and obtain the Datastore object, which stores the HWRF state
    # information:
    ds=conf.datastore

    # Create the objects in this module in a single transaction since that
    # speeds things up by a factor of 100:
    with ds.transaction():
        # Known types of domains:
        moad=WRFDomain(conf,'moad')
        storm1outer=WRFDomain(conf,'storm1outer')
        storm1inner=WRFDomain(conf,'storm1inner')
        storm1ghost_parent=WRFDomain(conf,'storm1ghost_parent')
        storm1ghost=WRFDomain(conf,'storm1ghost')
        storm1ghost_parent_big=WRFDomain(conf,'storm1ghost_parent_big')
        storm1ghost_big=WRFDomain(conf,'storm1ghost_big')
        postdoms=[moad,storm1outer,storm1inner]

        wrf=WRFSimulation(conf,'wrf',moad,conf.cycle,
                          conf.cycle+hwrf.numerics.to_timedelta(fcstlen*3600))
        wrf.add(storm1outer,moad)
        wrf.add(storm1inner,storm1outer)
        wrf.analysis_in(None)
        
        wrf_output_step=conf.getint('forecast_products','wrf_output_step',10800)
        pom_output_step=conf.getint('forecast_products','pom_output_step',86400)
        if wrf_output_step<10800:
            # Just output the history stream for smaller timesteps.
            wrf.add_output('history',step=wrf_output_step)
        else:
            # For the special case of three-hourly output, we still
            # need hourly for nine hours.
            wrf.add_output('history',step=3600*3,end=9*3600)
            wrf.add_output('auxhist2',step=3600*3,end=9*3600,
                           outname='wrfout_d<domain>_<date>')
            wrf.add_output('auxhist3',step=3600*3,
                           outname='wrfout_d<domain>_<date>')
        wrf.add_output('auxhist1',step=3600,outname='wrfdiag_d<domain>', \
                           frames_per_outfile=999,io_form=202)
        #wrftask=ExternalWRFTask(ds,conf,'wrf',wrf,location=WRFOUThwrf)

        # Set up the wrfdoms.  This is the same as postdoms, but with
        # information filled in about grid IDs, output times, etc.:
        wrfdoms=[d for d in wrf]

        # ------------------------------------------------------------------
        # ATMOSPHERE PRE-PROCESSING

        wrfghost=WRFSimulation(conf,'wrf',moad,conf.cycle,
                            conf.cycle+hwrf.numerics.to_timedelta(6*3600))
        wrfghost.add(storm1ghost_parent,moad)
        wrfghost.add(storm1ghost,storm1ghost_parent)

        wrfghost_big=WRFSimulation(conf,'wrf',moad,conf.cycle,
                            conf.cycle+hwrf.numerics.to_timedelta(6*3600))
        wrfghost_big.add(storm1ghost_parent_big,moad)
        wrfghost_big.add(storm1ghost_big,storm1ghost_parent_big)

        gfs_init=hwrf.init.HWRFInit(
            ds,conf,'gfsinit',wrf,6*3600,fcstlen*3600,ibdystep=6*3600,
            wrfghost=wrfghost_big,prep=spectral_flag,track=True,
            realfcst=True,relocate=reloc_flag,prepfcst=spectral_bdy)

        next_cycle=conf.cycle+hwrf.numerics.to_timedelta(6*3600)
        ensda_pre = hwrf.ensda.enada_pre_object_for(
            ds,conf,'tdrcheck',next_cycle)
        if gsi_flag:
            if make_ensemble_da:
                ##################################################
                # ENSEMBLE DA

                # NOTE: If you change this section, you must also
                # change the run_hwrf.py and hwrf_workflow.xml.in to
                # match.
                ensdadom=WRFDomain(conf,'ensdadom')
                ensdawrf=WRFSimulation(
                    conf,'enswrf',moad,conf.cycle,next_cycle)
                ensdawrf.add(ensdadom,moad)
                ensdawrf.add_output('history',step=3600*3,end=9*3600)
                ensdadoms=[ ensdawrf[moad], ensdawrf[ensdadom] ]
                for dom in ensdadoms:
                    assert(dom.get_grid_id() is not None)
            
                def makememb(clazz,ienkf,topname='ensda'):
                    assert(isinstance(topname,basestring))
                    assert(isinstance(ienkf,int))
                    return clazz(
                        ds,conf,"hwrf_da_ens",gfs_init,ienkf,ensdawrf,
                        "%s.%03d"%(topname,ienkf),
                        workdir=conf.getdir('WORKhwrf')+'/%s/%03d'%(
                            topname,ienkf),
                        outdir=conf.getdir('intercom')+'/%s/%03d'%(
                            topname,ienkf))

                entest=makememb(hwrf.ensda.FromGFSENKF,30,'entest')

                prior_ensda=hwrf.ensda.DAEnsemble(
                    ds,conf,'hwrf_da_ens',conf.cycle,'prior_ensda')
                da_ensemble_size=prior_ensda.confint('ensda_size',40)
                for i in xrange(da_ensemble_size):
                    prior_ensda.set_member(
                        conf.cycle,i+1,hwrf.ensda.FromPriorCycle(
                            ds,conf,"hwrf_da_ens",ensdadoms,
                            i+1,conf.cycle,taskname='%s.%03d'%(
                                'prior_ensda',i+1)))
                
                ensda=hwrf.ensda.DAEnsemble(
                    ds,conf,'hwrf_da_ens',conf.cycle,'ensda')
                for i in xrange(da_ensemble_size):
                    ensda.set_member(conf.cycle,i+1,makememb(
                            hwrf.ensda.FromGFSENKF,i+1,'ensda'))
                ##################################################
             

            if gsid03_flag:
                gsid03=True
            else:
                gsid03=None
            fgat_init=hwrf.init.FGATInit(
                ds,conf,'fgat',wrf,3*3600,fcstlen*3600,wrfghost=wrfghost,
                prep=spectral_flag,track=True,realfcst=False,ibdystep=3*3600,
                in_dataset='gdas1',relocate=reloc_flag,gsi_d02=True,
                gsi_d03=gsid03,prepfcst=spectral_bdy)
            ceninit=fgat_init.init_at_time(conf.cycle)

            bufrprep=hwrf.bufrprep.Bufrprep(
                ds,conf,'bufrprep',taskname='bufrprep')

            ingsi_d02=fgat_init.get_relocates(storm1ghost_parent)
            gsi_d02=hwrf.gsi.FGATGSI(
                ds,conf,'gsi_d02',storm1ghost_parent,
                ceninit.rstage3.get_ghost(storm1ghost_parent),
                ingsi_d02,wrfghost)

            if gsid03_flag:
                ingsi_d03=fgat_init.get_relocates(storm1ghost)
                gsi_d03=hwrf.gsi.FGATGSI(
                    ds,conf,'gsi_d03',storm1ghost,
                    ceninit.rstage3.get_ghost(storm1ghost),
                    ingsi_d03,wrfghost)
            else:
                gsi_d03=None


            if make_ensemble_da:
                gsi_d02.set_ensda(prior_ensda,ensdadoms)
                if gsid03_flag:
                    gsi_d03.set_ensda(prior_ensda,ensdadoms)

            gdas_merge=hwrf.relocate.Merge(
                ds,conf,'merge',ceninit.rstage3,
                wrfinput=gfs_init.realinit,
                wrfanl=gfs_init.runwrfanl,
                taskname='gdas_merge',
                gsi_d02=gsi_d02, gsi_d03=gsi_d03)
                

            ges_d02=ceninit.rstage3.get_ghost(storm1ghost_parent)
            if gsid03_flag:
                ges_d03=ceninit.rstage3.get_ghost(storm1ghost)
            else:
                ges_d03=ceninit.rstage3.get_wrfanl(storm1inner)
            gdas_merge.set_ges(ges_d02,ges_d03)

            # Create the GSI post.  It will post-process the ghost
            # files from the init.wrfghost, relocation and GSI for
            # each of d02 and d03:
            hgpp=hwrf.hwrfsystem.HWRFGSIPostProcessing(
                ds,conf,'gsi_products')
            (gsipost,gsigribber)=hgpp.make_gsi_post(
                gsi_d02,gsi_d03,storm1ghost,storm1ghost_parent,ceninit,
                gsid03_flag)

        # ------------------------------------------------------------------
        # OCEAN PRE-PROCESSING
        
        # Basins where we cannot run the chosen ocean model:
        non_ocean_basins='CABPSQ'

        # Ocean init job:    
        if ocean_flag:
            pominit=hwrf.mpipomtc.POMInit(ds,conf,'pom',fcstlen=fcstlen,
               outstep=pom_output_step)

        # ------------------------------------------------------------------
        # FORECAST 

        if ocean_flag:
            runwrf=hwrf.mpipomtc.WRFCoupledPOM(
                ds,conf,'runwrf',wrf,taskname='runwrf',pominit=pominit)
        else:
            runwrf=hwrf.fcsttask.WRFAtmos(
                ds,conf,'runwrf',wrf,taskname='runwrf')

        runwrf.add_metgrid(gfs_init.metgrid) \
              .add_geogrid(gfs_init.geogrid) \
              .add_fort65(gfs_init.realinit) \
              .add_wrfbdy(gfs_init.realfcst)

        # Add the merge output for the initial conditions and realfcst
        # for the boundary:
        if gsi_flag:
            runwrf.add_wrfinput(gdas_merge) \
                .add_wrfanl(gdas_merge,storm1outer) \
                .add_wrfanl(gdas_merge,storm1inner)

        if not gsi_flag or fallbacks_flag \
                 or (conditional_gsid03 and \
                     conditional_gsid02 and gsi_flag):
            if reloc_flag or (conditional_gsid03 and \
                              conditional_gsid02):
                runwrf.add_wrfinput(gfs_init.rstage3) \
                    .add_wrfanl(gfs_init.rstage3,storm1outer) \
                    .add_wrfanl(gfs_init.rstage3,storm1inner)
            else:
                # Do not fall back to "no relocation" even if
                # fallbacks are enabled.  This is because the
                # forecasting skill is useless without vortex
                # relocation.
                runwrf.add_wrfinput(gfs_init.realinit) \
                    .add_wrfanl(gfs_init.runwrfanl,storm1outer) \
                    .add_wrfanl(gfs_init.runwrfanl,storm1inner)

        # Force a location of the runwrf task to ensure the later post
        # and products are able to find files.
        runwrf.location=os.path.join(WORKhwrf,'runwrf')

        ################################################################

        #  RETURN HERE IF WE ARE NOT MAKING THE POST

        if not make_post: return

        ################################################################


        # ------------------------------------------------------------------
        # POST-PROCESSING

        hfpp=hwrf.hwrfsystem.HWRFForecastPostProcessing(
            ds,conf,'forecast_products',runwrf,wrf,postdoms,
            wrfdoms,moad,storm1inner,storm1outer)
        nonsatpost=hfpp.make_nonsatpost()
        satpost=hfpp.make_satpost()
        wrfcopier=hfpp.make_wrfcopier()
        assert(wrfcopier is not None)
        (gribber,tracker,track,nhcp)=\
            hfpp.make_gribber_tracker(extra_trackers,satpost_flag)
        if extra_trackers:
            (trackerd01,trackerd02)=\
                hfpp.make_extra_trackers()

    conf.log().info('Done in hwrf_expt module.')
