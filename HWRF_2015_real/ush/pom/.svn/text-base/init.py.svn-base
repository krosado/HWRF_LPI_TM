#!/usr/bin/env python

"""
Written By Biju Thomas, GSO, University of Rhode Island on 6/13/14. This module
contains classes and methods for ocean  prepossessing, ocean spin up: Phase1 and 
Phase2 (also known as Phase3 and Phase4). The prognostic ocean spin up uses wind
stress forcing that is based on NHC vitals. I use a simple "get_vitals" function to 
extract vitals  from NHC vital file (syndat_tcvitals.${yyyy}). 
Please report bugs/questions/comments to bijuthomas@mail.uri.edu.
"""

__all__=[ 'Hwrf', 'Oceanini', 'fbtr', 'g3', 'na', 'phase', 'pget', 'prun', 'psend' ]

import os
import logging
import shutil
import os.path
import produtil.run, produtil.fileop, produtil.cd, produtil.rusage

from os.path import join as jn2r
from produtil.cd import NamedDir
from produtil.fileop import deliver_file, makedirs, make_symlink, rmall, isnonempty
from produtil.run import mpi, mpirun, checkrun, run, exe , openmp
from util import ysplitter, inpfile, logi2int
from exceptions import *
from check_lcfiles import Isthis_LCuseful
              
class Hwrf(object):
    def __init__(self,STORMNAME,STORMID,STARTDATE,EXEChwrf,PARMhwrf, 
                      FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN): 
        self.STORMNAME = STORMNAME
        self.STORMID = STORMID
        self.STARTDATE = STARTDATE
        self.EXEChwrf = EXEChwrf
        self.PARMhwrf = PARMhwrf
        self.LCDIR = LCDIR
        self.GFSDIR = GFSDIR
        self.FIXhwrf = FIXhwrf
        assert(self.GFSDIR.find('pom/output')<0)
        self.CSTREAM = CSTREAM
        self.COMIN = COMIN
class Oceanini(Hwrf):
    def __init__(self,ODIR,DOMAIN,ODATA,SSTASIM,STORMNAME,STORMID,STARTDATE,EXEChwrf,PARMhwrf, 
                      FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN, **kwargs):
        assert(GFSDIR.find('pom/output')<0)
        super(Oceanini,self).__init__(STORMNAME,STORMID,STARTDATE,EXEChwrf,PARMhwrf, 
                      FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN)
        self.ODIR = ODIR
        self.DOMAIN = DOMAIN
        self.ODATA = ODATA 
        self.SSTASIM = SSTASIM
        self.conf = kwargs.pop('conf', None)

    def setpath(self,SUBDIR=""):
        RUNDIR = self.CSTREAM +"/"+self.ODIR
        try: 
            if SUBDIR != "" and os.path.exists(RUNDIR) and   \
                                not os.path.exists(RUNDIR+"/"+SUBDIR):
                makedirs(RUNDIR+"/"+SUBDIR)
            if not os.path.exists(RUNDIR):
                makedirs(RUNDIR)
            return (RUNDIR,RUNDIR+"/"+SUBDIR)   
        except:
          raise IOError('CSTREAM does NOT exist.') 
     
class fbtr(Oceanini):
    def __init__(self,*args,**kargs):
        super(fbtr,self).__init__(*args,**kargs)
    def getinp(self, DEST,logger=None):               
        if logger is None: logger=logging.getLogger('pom')
        try:
            (y4,mm,mm1,dd,hh) = ysplitter(self.STARTDATE)
            make_symlink(jn2r(self.GFSDIR, "gfs.t"+hh+"z.sfcanl"),jn2r(DEST,"fort.11"),True,logger=logger)
            make_symlink(jn2r(self.GFSDIR, "gfs.t"+hh+"z.sfcanl"),jn2r(DEST,"for11"),True,logger=logger) 
            make_symlink(jn2r(self.GFSDIR, "gfs.t"+hh+"z.sanl"),jn2r(DEST,"fort.12"),True,logger=logger)     
            make_symlink(jn2r(self.FIXhwrf,"gfdl_ocean_topo_and_mask.united"),
                         jn2r(DEST,"fort.66"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"gfdl_gdem."+mm+".ascii"),
                         jn2r(DEST,"fort.8"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"gfdl_gdem."+mm1+".ascii"),
                         jn2r(DEST,"fort.90"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"gfdl_ocean_readu.dat."+mm),
                         jn2r(DEST,"fort.24"),True,logger=logger)          
            make_symlink(jn2r(self.FIXhwrf,"gfdl_ocean_spinup_gdem3.dat."+mm),
                         jn2r(DEST,"fort.82"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"gfdl_ocean_spinup_gspath."+mm),
                         jn2r(DEST,"fort.50"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"gfdl_ocean_spinup.BAYuf"),
                         jn2r(DEST,"fort.55"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"gfdl_ocean_spinup.FSgsuf"),
                         jn2r(DEST,"fort.65"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"gfdl_ocean_spinup.SGYREuf"),
                         jn2r(DEST,"fort.75"),True,logger=logger)
            lfile = jn2r(self.LCDIR,"hwrf_gfdl_loop_current_rmy5.dat."+y4+mm+dd)
            rfile = jn2r(self.LCDIR,"hwrf_gfdl_loop_current_wc_ring_rmy5.dat."+y4+mm+dd)
            if Isthis_LCuseful(self.STARTDATE, lfile, rfile,logger=logger):
                make_symlink(lfile,jn2r(DEST,"fort.31"),True,logger=logger)
                make_symlink(rfile,jn2r(DEST,"fort.32"),True,logger=logger) 
            else:
                make_symlink(jn2r(self.PARMhwrf,"hwrf_gfdl_loop_current_rmy5.dat"),
                         jn2r(DEST,"fort.31"),True,logger=logger)
                make_symlink(jn2r(self.PARMhwrf,"hwrf_gfdl_loop_current_wc_ring_rmy5.dat"),
                         jn2r(DEST,"fort.32"),True,logger=logger)
            inpfile(jn2r(DEST,"fort.91"), [mm, dd],logger=logger)
            inpfile(jn2r(DEST,"input_sharp"),["1"],logger=logger)
            inpfile(jn2r(DEST,"input"),
                    [self.ODATA, "0", str(logi2int(self.SSTASIM)),self.STORMNAME],logger=logger)
        except Exception as e:
            msg='Cannot find input: '+str(e)
            logger.warning(msg,exc_info=True)
            raise POMInputError(msg)
    def setrun(self, DEST,logger=None):
      with NamedDir(DEST) as d:
        if logger is None: logger=logging.getLogger('pom')
        (y4,mm,mm1,dd,hh) = ysplitter(self.STARTDATE)
        rmall(jn2r(DEST,"fort.23"),jn2r(DEST,"fort.74"),jn2r(DEST,"fort.77"),logger=logger)
        rmall(jn2r(DEST,"lonlat.gfs"),jn2r(DEST,"sst.gfs.dat"),jn2r(DEST,"mask.gfs.dat"),logger=logger)
        if os.path.exists(jn2r(DEST,"fort.11")) and os.path.exists(jn2r(DEST,"fort.12")):
            with open('listing','wb') as f:
                listing=str(produtil.listing.Listing())
                f.write(listing)
            produtil.rusage.getrlimit(logger)
            # Raise stack limit to 6000M
            produtil.rusage.setrlimit(ignore=True,logger=logger,stack=6e9)
            xc = self.conf.getexe('hwrf_getsst')
            log = jn2r(DEST,"getsst.out")
            with produtil.rusage.rusage(logger=logger):
                retcode = run(openmp(exe(xc),threads=1).env(OMP_STACKSIZE='128M')
                              > log,logger=logger)
            logger.info("hwrf_getsst: err = %s" %(retcode))
            if retcode == 0 and os.path.exists(jn2r(DEST,"fort.74")):
                deliver_file(jn2r(DEST,"fort.74"),jn2r(DEST,"sst.gfs.dat"),False)
                deliver_file(jn2r(DEST,"fort.77"),jn2r(DEST,"mask.gfs.dat"),False)
                logger.info("GFS SST extracted: {0}".format(DEST))
            else:
                msg="hwrf_getsst Failed: NO GFS SST extracted: {0}".format(DEST)
                logger.warning(msg)
                raise POMSSTError(msg)
        else: 
            msg="GFS Spectral Files NOT Found in %s:" %(DEST)
            logger.error(msg)
            raise POMInputError(msg)

        if isnonempty('fort.31') and isnonempty('fort.32'):
            rmall(jn2r(DEST,"fort.13"),jn2r(DEST,"gfdl_initdata.united."+mm),logger=logger)
            xc = self.conf.getexe('hwrf_sharp_mcs_rf_l2m_rmy5')
            inp = jn2r(DEST,"input_sharp")
            log = jn2r(DEST,"sharp_mcs_r_l2b.out")
            retcode = run((exe(xc) < inp) >= log,logger=logger)
            logger.info("sharp_mcs_rf: err = %s" %(retcode))
            if retcode == 0 and os.path.exists(jn2r(DEST,"fort.13")):
                deliver_file(jn2r(DEST,"fort.13"),jn2r(DEST,"gfdl_initdata.united."+mm),False,logger=logger)
                logger.info("Specified LC penetration: {0}".format(retcode))
            else:
                deliver_file(jn2r(self.FIXhwrf,"gfdl_initdata.gdem.united."+mm), 
                             jn2r(DEST,"gfdl_initdata.united."+mm),logger=logger)
                logger.warning("sharp_mcs_rf failed: Climate LC penetration %s" %(retcode))

        else:
            deliver_file(jn2r(self.FIXhwrf,"gfdl_initdata.gdem.united."+mm),    
                         jn2r(DEST,"gfdl_initdata.united."+mm),logger=logger)
            # Log to highest log level so it is in the jlogfile:
            logger.critical("LC data NOT available: Climate LC penetration %s" %(retcode))
        if os.path.exists(jn2r(DEST,"gfdl_initdata.united."+mm)):
            make_symlink(DEST+"/gfdl_initdata.united."+mm, DEST+"/fort.13",True,logger=logger)
            xc = self.conf.getexe('hwrf_ocean_transatl06prep')
            log = jn2r(DEST,"transatl06prep.out")
            retcode = run(exe(xc)  >= log, logger=logger)
            if retcode == 0 and os.path.exists(jn2r(DEST,"fort.113")):
                deliver_file(jn2r(DEST,"fort.113"),jn2r(DEST,"gfdl_initdata."+self.DOMAIN+"."+mm),False,logger=logger)
                logger.info("transatl06prep: Success {0}".format(retcode))
            else:
                msg="transatl06prep: Failed {0}".format(retcode)
                logger.warning(msg)
                raise POMPrepError(msg)
        else:
            logger.info("NOT Found in %s:" %("gfdl_initdata.united."+mm)) 
        if os.path.exists(jn2r(DEST,"gfdl_initdata."+self.DOMAIN+"."+mm)):
             deliver_file(jn2r(DEST,"sst.gfs.dat"),jn2r(DEST,"fort.21"),False,logger=logger)
             deliver_file(jn2r(DEST,"mask.gfs.dat"),jn2r(DEST,"fort.22"),False,logger=logger)
             rmall(jn2r(DEST,"fort.13"),logger=logger)
             make_symlink(jn2r(DEST,"gfdl_initdata."+self.DOMAIN+"."+mm),jn2r(DEST,"fort.13"),True,logger=logger)
             make_symlink(jn2r(self.FIXhwrf,"gfdl_ocean_topo_and_mask."+self.DOMAIN+ 
                          ".lores"), jn2r(DEST,"fort.66"),True,logger=logger)
             xc = self.conf.getexe("hwrf_ocean_pomprep_"+self.ODATA[0:2])
             inp = jn2r(DEST,"input")
             log = jn2r(DEST,"ocean_pomprep.out")
             retcode = run((exe(xc) < inp ) >= log,logger=logger)
             if retcode == 0 and os.path.exists(jn2r(DEST,self.STORMNAME+".grid.nc")):
                 logger.info("ocean_pomprep: Success %s:" %(self.ODATA))
                 return True
             else:
                 msg="ocean_pomprep: Failed %s" %(self.ODATA)
                 logger.warning(msg)
                 raise POMInitFailed(msg)
        else:
            msg="NOT Found in %s:" %(''.join(["gfdl_initdata.",self.DOMAIN,".",mm]))
            logger.warning(msg)
            raise POMInitFailed(msg)
          
class g3(Oceanini):
    def __init__(self, *args,**kargs):
        super(g3,self).__init__(*args,**kargs)
    def getinp(self, DEST,logger=None):
        if logger is None: logger=logging.getLogger('pom')
        (y4,mm,mm1,dd,hh) = ysplitter(self.STARTDATE)
        try:
            make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.sfcanl"),jn2r(DEST,"fort.11"),True,logger=logger)
            make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.sfcanl"),jn2r(DEST,"for11"),True,logger=logger)
            make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.sanl"),jn2r(DEST,"fort.12"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"gfdl_ocean_topo_and_mask."+
                         self.DOMAIN+".lores"),jn2r(DEST,"fort.66"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"tgdemv3s"+mm+".nc"),
                         jn2r(DEST,"tin.nc"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"sgdemv3s"+mm+".nc"),
                         jn2r(DEST,"sin.nc"),True,logger=logger)
            inpfile(jn2r(DEST,"input"),
                 [self.ODATA, "0", str(logi2int(self.SSTASIM)),self.STORMNAME],logger=logger)
        except Exception as e:
            msg='Input Data does NOT exist: %s'%(str(e),)
            logger.error(msg,exc_info=True)
            raise POMInputError(msg)
    def setrun(self, DEST,logger=None):
      with NamedDir(DEST):
        if logger is None: logger=logging.getLogger('pom')
        (y4,mm,mm1,dd,hh) = ysplitter(self.STARTDATE)
        rmall(jn2r(DEST,"fort.23"),jn2r(DEST,"fort.74"),jn2r(DEST,"fort.77"),logger=logger)
        rmall(jn2r(DEST,"lonlat.gfs"),jn2r(DEST,"sst.gfs.dat"),jn2r(DEST,"mask.gfs.dat"),logger=logger)
        if os.path.exists(jn2r(DEST,"fort.11")) and os.path.exists(jn2r(DEST,"fort.12")):
            produtil.rusage.getrlimit(logger)
            # Raise stack limit to 6000M
            produtil.rusage.setrlimit(ignore=True,logger=logger,stack=6e9)
            xc = self.conf.getexe('hwrf_getsst')
            log = jn2r(DEST,"getsst.out")
            retcode = run(exe(xc).env(OMP_STACKSIZE='128M')
                          > log,logger=logger)
            logger.info("hwrf_getsst: err = %s" %(retcode))
            if retcode == 0 and os.path.exists(jn2r(DEST,"fort.74")):
                deliver_file(jn2r(DEST,"fort.74"),jn2r(DEST,"sst.gfs.dat"),False,logger=logger)
                deliver_file(jn2r(DEST,"fort.77"),jn2r(DEST,"mask.gfs.dat"),False,logger=logger)
                logger.info("GFS SST extracted: {0}".format(retcode))
            else:
                msg="hwrf_getsst Failed: NO GFS SST extracted %s" %(retcode)
                logger.warning(msg)
                raise POMSSTError(msg)
        else:
            msg="GFS Spectral Files NOT Found in %s:" %(DEST)
            logger.warning(msg)
            raise POMInputError(msg)
        deliver_file(jn2r(DEST,"sst.gfs.dat"),jn2r(DEST,"fort.21"),False,logger=logger)
        deliver_file(jn2r(DEST,"mask.gfs.dat"),jn2r(DEST,"fort.22"),False,logger=logger)
        xc = self.conf.getexe("hwrf_ocean_pomprep_"+self.ODATA[0:2])
        inp = jn2r(DEST,"input")
        log = jn2r(DEST,"ocean_pomprep.out")
        retcode = run((exe(xc) < inp) > log,logger=logger)
        if retcode == 0 and os.path.exists(jn2r(DEST,self.STORMNAME+".grid.nc")):
            logging.info("ocean_pomprep: Success %s:" %(self.ODATA))
            return True
        else:
            msg="ocean_pomprep: Failed %s:" %(self.ODATA)
            logger.warning(msg)
            raise POMInitFailed(msg)
        
class na(Oceanini):
    def __init__(self, *args,**kargs):
        super(na,self).__init__(*args,**kargs)
    def getinp(self, DEST, logger=None):
        if logger is None: logger=logging.getLogger('pom')
        (y4,mm,mm1,dd,hh) = ysplitter(self.STARTDATE)
        try:
            # RMY: For now, define local variable "NCDADIR" to point to the NCODA input data #
            make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.sfcanl"),jn2r(DEST,"fort.11"),True,logger=logger)
            make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.sfcanl"),jn2r(DEST,"for11"),True,logger=logger)
            make_symlink(jn2r(self.GFSDIR,"gfs.t"+hh+"z.sanl"),jn2r(DEST,"fort.12"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"gfdl_ocean_topo_and_mask."+
                         self.DOMAIN+".lores"),jn2r(DEST,"fort.66"),True,logger=logger)
            NCDADIR="/lfs2/projects/hwrf-vd/hwrf-input/NCODA/"+y4+"/"+y4+mm+dd+"/"
            make_symlink(jn2r(NCDADIR,"seatmp_pre_000000_005000_1o2161x1051_"+
                         y4+mm+dd+"00_00000000_analfld"),jn2r(DEST,"fort.48"),True,logger=logger)
            make_symlink(jn2r(NCDADIR,"salint_pre_000000_005000_1o2161x1051_"+
                         y4+mm+dd+"00_00000000_analfld"),jn2r(DEST,"fort.49"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"depths_sfc_000000_000000_1o2161x1051_datafld"),
                         jn2r(DEST,"fort.68"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"grdlon_sfc_000000_000000_1o2161x1051_datafld"),
                         jn2r(DEST,"fort.78"),True,logger=logger)
            make_symlink(jn2r(self.FIXhwrf,"grdlat_sfc_000000_000000_1o2161x1051_datafld"),
                         jn2r(DEST,"fort.79"),True,logger=logger)
            ##################################################################################
            inpfile(jn2r(DEST,"input"), 
                         [self.ODATA, "0", str(logi2int(self.SSTASIM)),self.STORMNAME],logger=logger)
        except Exception as e:
            msg='Input data does NOT exist: %s'%(str(e),)
            logger.error(msg,exc_info=True)
            raise POMInputError(msg)
    def setrun(self, DEST,logger=None):
      with NamedDir(DEST):
        if logger is None: logger=logging.getLogger('pom')
        (y4,mm,mm1,dd,hh) = ysplitter(self.STARTDATE)
        rmall(jn2r(DEST,"fort.23"),jn2r(DEST,"fort.74"),jn2r(DEST,"fort.77"),logger=logger)
        rmall(jn2r(DEST,"lonlat.gfs"),jn2r(DEST,"sst.gfs.dat"),jn2r(DEST,"mask.gfs.dat"),logger=logger)
        if os.path.exists(jn2r(DEST,"fort.11")) and os.path.exists(jn2r(DEST,"fort.12")):
            produtil.rusage.getrlimit(logger)
            # Raise stack limit to 6000M
            produtil.rusage.setrlimit(ignore=True,logger=logger,stack=6e9)
            xc = self.conf.getexe('hwrf_getsst')
            log = jn2r(DEST,"getsst.out")
            retcode = run(exe(xc).env(OMP_STACKSIZE='128M')
                          > log,logger=logger)
            logging.info("hwrf_getsst: err = %s" %(retcode))
            if retcode == 0 and os.path.exists(jn2r(DEST,"fort.74")):
                deliver_file(jn2r(DEST,"fort.74"),jn2r(DEST,"sst.gfs.dat"),False,logger=logger)
                deliver_file(jn2r(DEST,"fort.77"),jn2r(DEST,"mask.gfs.dat"),False,logger=logger)
                logger.info("GFS SST extracted: {0}".format(retcode))
            else:
                msg="hwrf_getsst Failed: NO GFS SST extracted %s"%(retcode)
                logger.warning(msg)
                raise POMSSTError(msg)
        else:
            msg="GFS Spectral Files NOT Found in %s:" %(DEST)
            logger.warning(msg)
            raise POMInputError(msg)
        deliver_file(jn2r(DEST,"sst.gfs.dat"),jn2r(DEST,"fort.21"),False,logger=logger)
        deliver_file(jn2r(DEST,"mask.gfs.dat"),jn2r(DEST,"fort.22"),False,logger=logger)
        xc = self.conf.getexe("hwrf_ocean_pomprep_"+self.ODATA[0:2])
        inp = jn2r(DEST,"input")
        log = jn2r(DEST,"ocean_pomprep.out")
        retcode = run((exe(xc) < inp) >log,logger=logger)
        if retcode == 0 and os.path.exists(jn2r(DEST,self.STORMNAME+".grid.nc")):
            logging.info("ocean_pomprep: Success %s:" %(self.ODATA))
            return True
        else:
            msg="ocean_pomprep: Failed %s:" %(self.ODATA)
            logger.warning(msg)
            raise POMInitFailed(msg)

class phase(Oceanini):
    def __init__(self,*args,**kargs):
        super(phase,self).__init__(*args,**kargs)
    def getinp(self, DEST, logger=None):
        if logger is None: logger=logging.getLogger('pom')
        try:
            (y4,mm,mm1,dd,hh) = ysplitter(self.STARTDATE)
            SRC = jn2r(self.CSTREAM, self.ODIR)
            make_symlink(jn2r(SRC,self.STORMNAME+".grid.nc"),
                         jn2r(DEST,self.STORMNAME+".grid.nc"),True,logger=logger)
            make_symlink(jn2r(SRC,self.STORMNAME+".ts_initial.nc"),
                         jn2r(DEST,self.STORMNAME+".ts_initial.nc"),True,logger=logger)
            make_symlink(jn2r(SRC,self.STORMNAME+".ts_clim.nc"),
                         jn2r(DEST,self.STORMNAME+".ts_clim.nc"),True,logger=logger)
            make_symlink(jn2r(SRC,self.STORMNAME+".uv_initial.nc"),
                         jn2r(DEST,self.STORMNAME+".uv_initial.nc"),True,logger=logger) 
            make_symlink(jn2r(SRC,self.STORMNAME+".el_initial.nc"),
                         jn2r(DEST,self.STORMNAME+".el_initial.nc"),True,logger=logger)
        except Exception as e:
            msg='Input data does not exist: %s'%(str(e),)
            logger.warning(msg,exc_info=True)
            raise POMInputError(msg)

    def setrun(self, DEST,logger=None):
      with NamedDir(DEST):
        if logger is None: logger=logging.getLogger('pom')
        (y4,mm,mm1,dd,hh) = ysplitter(self.STARTDATE)
        if os.path.exists(jn2r(DEST,self.STORMNAME+".grid.nc")) and \
           os.path.exists(jn2r(DEST,"pom.nml")):
            exe = self.conf.getexe("hwrf_ocean_init")
            log = jn2r(DEST,"ocean_init.out")
            retcode=run(mpirun(mpi(exe)*9) >= log ,logger=logger)
            logger.info("hwrf_ocean_init: err = %s" %( retcode))
            if retcode == 0 and os.path.exists(jn2r(DEST,"restart.0001.nc")):
                logger.info("Phase completed HERE:  %s" %(DEST))
                return True
            else:
                msg="Phase FAILED HERE:  %s" %(DEST)
                logger.error(msg)
                return False
        else:
            msg="Input Files  NOT exists %s:" %(DEST)
            logger.warning(msg)
            raise POMInputError(msg)
    def sendout(self, SRC, infile, DEST, outfile,logger=None):
        if logger is None: logger=logging.getLogger('pom')
        if os.path.exists(jn2r(SRC, infile)): 
            deliver_file(jn2r(SRC,infile),jn2r(DEST,outfile),False,logger=logger)
            return True
        else:
            msg="Restart file %s does NOT exist:" %(infile)
            logger.warning(msg)
            raise POMInitFailed(msg)

class pget:
    def getinp(self, this, DEST,logger=None):
        if logger is None: logger=logging.getLogger('pom')
        logger.info('get input this=%s DEST=%s'%(repr(this),repr(DEST)))
        this.getinp(DEST,logger=logger)
class prun:
    def setrun(self, this, DEST,logger=None):
        if logger is None: logger=logging.getLogger('pom')
        return this.setrun(DEST, logger=logger)
class psend:
    def sendout(self, this, SRC, infile, DEST, outfile, logger=None):
        if logger is None: logger=logging.getLogger('pom')
        return this.sendout(SRC, infile, DEST, outfile, logger=logger)

