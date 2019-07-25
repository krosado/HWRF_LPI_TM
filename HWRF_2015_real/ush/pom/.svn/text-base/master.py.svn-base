#!/usr/bin/env python

""" 
Written By Biju Thomas, GSO, University of Rhode Island on 6/12/14, main script to 
for running ocean spin up: Phase1 and Phase2 (also known as Phase3 and Phase4).
It assumes that all parent HWRF directory tree (CSTREAM, COMIN), exists and the 
necessary inputs sets are availabe in  PARMhwrf, FIXhwrf, VITDIR , GFSDIR, LCDIR 
directories. I assume that python based HWRF directory structure is same as in the
unix/linux based one. Otherwise, it can be changed based on the python version.
Please report bugs/questions/comments to bijuthomas@mail.uri.edu.

Modified by Richard Yablonsky, GSO, University of Rhode Island on 10/1/14 to
allow for domains other than transatl, eastpac, and westpac.
Modified by Richard Yablonsky, GSO, University of Rhode Island on 12/1/14 to
allow for NCODA initialization as an alternative to the GDEM/F-B initialization.
Please report bugs/questions/comments to rmyablon@mail.uri.edu.
"""
import logging
from init import Oceanini, fbtr, g3, na, pget, prun, phase, psend
from util import dateplushours, ysplitter, logi2int, jn2r
from nml import nml
from track import get_vitals, track_shorten
from exceptions import *

# To use default initialization in a given basin, enter "GDEM"  after the : below
# To use NCODA   initialization in a given basin, enter "NCODA" after the : below
oinit_d = {"transatl":"GDEM", "eastpac":"GDEM", "westpac":"GDEM",
           "northind":"GDEM", "southind":"GDEM", "swpac":"GDEM",
           "sepac":"GDEM", "southatl":"GDEM"}

odata_d = {"L":("fbtr","natr","idtr"), "E":("g3ep","naep","idep"),
           "W":("g3wp","nawp","idwp"), "A":("g3ni","nani","idni"),
           "S":("g3si","nasi","idsi"), "P":("g3sw","nasw","idsw"),
           "X":("g3se","nase","idse"), "C":("g3ep","naep","idep"),
           "O":("g3wp","nawp","idwp"), "B":("g3ni","nani","idni"),
           "U":("g3sw","nasw","idsw"), "T":("g3wp","nawp","idwp"),
           "Q":("g3sa","nasa","idsa")}
domain_d = {"L":"transatl", "E":"eastpac", "W":"westpac",
            "A":"northind", "S":"southind", "P":"swpac",
            "X":"sepac", "C":"eastpac", "O":"westpac",
            "B":"northind", "U":"swpac", "T":"westpac",
            "Q":"southatl"}

def run_init(STORMNAME,STORMID,STARTDATE,
             EXEChwrf,PARMhwrf,FIXhwrf,VITDIR,GFSDIR,
             LCDIR,CSTREAM,COMIN,init_method=None,logger=None, 
             fcstlen=None,outstep=None,**kwargs):
    assert(GFSDIR.find('pom/output')<0)
    if logger is None: logger=logging.getLogger('pom')
    if fcstlen is None: fcstlen=126 # forecast length in hours
    if outstep is None: outstep=86400
    if outstep<540: outstep=540
    prtd1=outstep/86400.0

    (y4,mm,mm1,dd,hh)=ysplitter(STARTDATE)
    BASIN=STORMID[2].upper()
    if  BASIN == "L":
        CENTERID = "NHC"
        DOMAIN = domain_d[BASIN] 
        if init_method is None: init_method=oinit_d[domain_d[BASIN]]
        if init_method == 'GDEM':
            GEOVEL = True
            SSTASIM = True
            NL = 33
            ODATA = odata_d[BASIN][0]
            prep = fbtr("OCEAN",DOMAIN,ODATA, SSTASIM,STORMNAME,STORMID,STARTDATE,
                         EXEChwrf,PARMhwrf,FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN,
                         **kwargs)
        elif init_method == 'NCODA':
            GEOVEL = True
            SSTASIM = False
            NL = 34
            ODATA = odata_d[BASIN][1]
            prep = na("OCEAN",DOMAIN,ODATA,SSTASIM,STORMNAME,STORMID,STARTDATE,
                       EXEChwrf,PARMhwrf,FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN,
                       **kwargs)
        else:
            msg=("%s:ODATA is NOT Supported" %(oinit_d[domain_d[BASIN]]))
            logger.critical(msg)
            raise POMConfigError(msg)
        (DATA,DATA1) = prep.setpath()
        ymdh = STARTDATE
        infiles = pget()
        infiles.getinp(prep,DATA)
        runexes = prun()
        if not runexes.setrun(prep,DATA):
            msg='setrun failed for L domain prep'
            logger.warning(msg)
            raise POMInitFailed(msg)
    elif BASIN == "E" or BASIN == "C":
        CENTERID = "NHC"
        DOMAIN = domain_d[BASIN]
        if init_method is None: init_method=oinit_d[domain_d[BASIN]]
        if init_method == 'GDEM':
            GEOVEL = True
            SSTASIM = True
            NL = 73
            ODATA =  odata_d[BASIN][0]
            prep = g3("OCEAN",DOMAIN,ODATA,SSTASIM,STORMNAME,STORMID,STARTDATE,
                       EXEChwrf,PARMhwrf,FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN,
                       **kwargs)
        elif init_method == 'NCODA':
            GEOVEL = True
            SSTASIM = False
            NL = 34
            ODATA = odata_d[BASIN][1]
            prep = na("OCEAN",DOMAIN,ODATA,SSTASIM,STORMNAME,STORMID,STARTDATE,
                   EXEChwrf,PARMhwrf,FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN,
                   **kwargs)
        else:
            msg=("%s:ODATA is NOT Supported" %(oinit_d[domain_d[BASIN]]))
            logger.critical(msg)
            raise POMConfigError(msg)
        (DATA,DATA1) = prep.setpath()
        ymdh = STARTDATE
        infiles = pget()
        infiles.getinp(prep,DATA)
        runexes = prun()
        if not runexes.setrun(prep,DATA):
            msg='setrun failed for E domain prep'
            logger.warning(msg)
            raise POMInitFailed(msg)
    elif BASIN in ("W", "A", "S", "P", "X", "O", "B", "U", "T", "Q"):
        CENTERID = "JTWC"
        DOMAIN = domain_d[BASIN]
        if init_method is None: init_method=oinit_d[domain_d[BASIN]]
        if init_method == 'GDEM':
            GEOVEL = False
            SSTASIM = True
            NL = 73
            ODATA = odata_d[BASIN][0]
            prep = g3("OCEAN",DOMAIN,ODATA,SSTASIM,STORMNAME,STORMID,STARTDATE,
                       EXEChwrf,PARMhwrf,FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN,
                       **kwargs)
        elif init_method == 'NCODA':
            GEOVEL = False
            SSTASIM = False
            NL = 34
            ODATA = odata_d[BASIN][1]
            prep = na("OCEAN",DOMAIN,ODATA,SSTASIM,STORMNAME,STORMID,STARTDATE,
                   EXEChwrf,PARMhwrf,FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN,
                   **kwargs)
        else:
            msg=("%s:ODATA is NOT Supported" %(oinit_d[domain_d[BASIN]]))
            logger.critical(msg)
            raise POMConfigError(msg)
        (DATA,DATA1) = prep.setpath()
        ymdh = STARTDATE
        infiles = pget()
        infiles.getinp(prep,DATA)
        runexes = prun()
        if not runexes.setrun(prep,DATA):
            msg='setrun failed for W/A/S/P/X/O/B/U/T/Q domain prep'
            logger.warning(msg)
            raise POMUnsupportedBasin(msg)
    else:
        msg=("%s : Domain is NOT Supported" %(STORMID))
        logger.critical(msg)
        raise POMUnsupportedBasin(msg)

    diag = phase("OCEAN",DOMAIN,ODATA,SSTASIM,STORMNAME,STORMID,STARTDATE,
                 EXEChwrf,PARMhwrf,FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN,
                 **kwargs)
    (DATA,DATA1) = diag.setpath("PHASE1")
    namelst=nml()
    namelst("'MPIPOM-TC:"+STORMNAME+STORMID+"'", STORMNAME,
            "'"+y4+"-"+mm+"-"+dd+" "+hh+":00:00 +00:00'",0, 
            "'restart.phase0.nc'",2.0,2.0,prtd1,3,0.,logi2int(GEOVEL),NL)
    namelst.make(DATA1)
    infiles.getinp(diag,DATA1)
    if not  runexes.setrun(diag,DATA1):
        msg='setrun failed for diag'
        logger.warning(msg)
        raise POMInitFailed(msg)
    outfiles = psend()
    prog = phase("OCEAN",DOMAIN,ODATA,SSTASIM,STORMNAME,STORMID,STARTDATE,
                 EXEChwrf,PARMhwrf,FIXhwrf,LCDIR,GFSDIR,CSTREAM,COMIN,
                 **kwargs)
    ymdh = dateplushours(ymdh, -72)
    (DATA,DATA2) = prog.setpath("PHASE2")
    (y4,mm,mm1,dd,hh)=ysplitter(ymdh)
    namelst("'MPIPOM-TC:"+STORMNAME+STORMID+"'", STORMNAME,
            "'"+y4+"-"+mm+"-"+dd+" "+hh+":00:00 +00:00'",1, 
            "'restart.phase1.nc'",3.0,3.0,prtd1,1,9999.,logi2int(GEOVEL),NL)
    namelst.make(DATA2) 
    if outfiles.sendout(diag,DATA1,"restart.0001.nc",DATA2,"restart.phase1.nc"):
        vitfile = jn2r(VITDIR,"syndat_tcvitals." + y4)
        trackfile = jn2r(DATA2,"track.full")
        trackshort = jn2r(DATA2,"track") 
        nvit = get_vitals(vitfile, CENTERID, STORMID, y4,trackfile)
        nvit = track_shorten(trackfile,trackshort,STARTDATE)
        if nvit >= 2:
            infiles.getinp(prog,DATA2)
            if runexes.setrun(prog,DATA2):
                outfiles.sendout(prog,DATA2,"restart.0001.nc",DATA,"restart.phase2.nc")
            else:
                outfiles.sendout(prog,DATA2,"restart.phase1.nc",DATA,"restart.phase2.nc")
        else:
            outfiles.sendout(prog,DATA2,"restart.phase1.nc",DATA,"restart.phase2.nc")
    else:
        msg='sendout failed'
        logger.warning(msg)
        raise POMInitFailed(msg)

    # Make a namelist for the forecast model as pom.nml in the top-level directory:
    namelst=nml()
    (y4,mm,mm1,dd,hh)=ysplitter(STARTDATE)
    namelst("'MPIPOM-TC:"+STORMNAME+STORMID+"'", STORMNAME,
            "'"+y4+"-"+mm+"-"+dd+" "+hh+":00:00 +00:00'",1, 
            "'restart.phase2.nc'",9999.,fcstlen/24.,prtd1,1,9999.,logi2int(GEOVEL),NL)
    namelst.make('.')
    return True

def main(STORMNAME, STORMID, STARTDATE,fcstlen=None):
    EXEChwrf = "/mnt/pan2/projects/hur-uri/Biju.Thomas/trunk/exec/"
    PARMhwrf ="/mnt/pan2/projects/hur-uri/Biju.Thomas/parm/"
    FIXhwrf = "/mnt/pan2/projects/hur-uri/Biju.Thomas/fix/"
#    FIXhwrf = "/mnt/lfs1/projects/hwrf-vd/fix-files/hwrf-20140312-fix/fix/hwrf-pom/"
    VITDIR = "/mnt/lfs1/projects/hwrf-vd/hwrf-input/SYNDAT/"
    GFSDIR = "/pan2/projects/hur-uri/Biju.Thomas/ptmp/Biju.Thomas/./DATA/OCEAN/gfs."+STARTDATE+'/'+STARTDATE
    LCDIR = "/mnt/lfs1/projects/hwrf-vd/hwrf-input/LOOP-CURRENT/"
    LCDIR = "/lfs1/projects/hwrf-vd/fix-files/hwrf-20130917-fix/fix/loop_curr/"
    CSTREAM = "/mnt/pan2/projects/hur-uri/Biju.Thomas/ptmp/Biju.Thomas/hwrf"+\
               "/"+STARTDATE+"/"+STORMID.lower()+"/"+STORMNAME.upper()+\
               "."+STARTDATE+"/"
    COMIN = "/mnt/pan2/projects/hur-uri/Biju.Thomas/ptmp/Biju.Thomas/hwrf/com/"+\
             STARTDATE+"/"+STORMID.lower()+"/"
    return run_init(STORMNAME,STORMID,STARTDATE,
                    EXEChwrf,PARMhwrf,FIXhwrf,VITDIR,GFSDIR,
                    LCDIR,CSTREAM,COMIN,fcstlen)

