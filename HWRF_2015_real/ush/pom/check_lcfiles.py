#!/usr/bin/env python

"""
Written By Biju Thomas, GSO, University of Rhode Island on 8/5/14. This module
checks the datestamp in the Loop Current files and compare with the forecast 
start date. If the datestmp is 90 days older than forecast start date, it tells the
ocean init scripts to not use them. Also note that if the datestamp in the Loop 
Current files later than forecast date, it tells the ocean init scripts to not
not use them.

"""

from os.path import exists, getsize
from datetime import datetime
import logging

def Isthis_LCuseful(startdate, lfile, rfile, logger=None):
    if logger is None: logger=logging.getLogger('pom')
    if exists(lfile) and getsize(lfile) > 0 and  \
        exists(rfile) and getsize(rfile) > 0:
        logger.info('LC: %s and %s exists' %(lfile,rfile))
        try:
            ldate = int(get_endline(lfile))
        except ValueError as e:
            logger.error("datestamp can not be accessed from %s" %(lfile))
            logger.error("Error: %s" %(e))
            return False
        try:
            rdate = int(get_endline(rfile))
        except ValueError as e:
            logger.error("datestamp can not be accessed from %s" %(rfile))
            logger.error("Error: %s" %(e)) 
            return False
        if ldate == rdate:
            if isinstance(startdate, str):
                ymdh = startdate
                try: 
                    t1 = datetime(int(ymdh[0:4]),int(ymdh[4:6]),int(ymdh[6:8]),int(ymdh[8:10]))
                except ValueError as e:
                    logger.error("Invalid startdate format %s" %(startdate))
                    logger.error("Error: %s" %(e))
                    return False
                ymdh = str(ldate)
                try:
                    t2 = datetime(int(ymdh[0:4]),int(ymdh[4:6]),int(ymdh[6:8]),int(ymdh[8:10]))
                except ValueError as e:
                    logger.error("Invalid LC datestamp format in %s" %(str(ldate)))
                    logger.error("Error: %s " %(e))
                    return False
                days = (t1 - t2).days
                if days >= 0 and  days <= 90:
                    logger.info("LC files are created by %i days ago from %s" %(days, startdate))
                    logger.info("%s and %s  files can be used for sharpening" %(lfile,rfile))
                    return True
                else:
                    logger.warn("LC files are created by %i days ago from %s" %(days,startdate))
                    logger.warn("%s and %s are OLD and Not using for sharpening" %(lfile,rfile))
                    return False
            else:
                logger.error("InValid startdate(in Isthis_LCuseful) %s", startdate)
                return False  
        else:
            logger.error("datestamps in %s and %s do not match" %(lfile,rfile))
            return False
    else:
        logger.error("%s %s files do not exists" %(lfile,rfile))
        return False

def get_endline(file):
    with open(file) as fid:
        data = fid.read() 
    return data.split()[len(data.split())-1]

if __name__ == '__main__':    
    LCdir = '/mnt/lfs2/projects/hwrf-data/fix-files/hwrf-20140617-fix/fix/loop_curr'
    lfile = LCdir+'/hwrf_gfdl_loop_current_rmy5.dat.20131001'
    rfile = LCdir+'/hwrf_gfdl_loop_current_wc_ring_rmy5.dat.20131001'
    startdate = '2013101123'
    print(Isthis_LCuseful(startdate, lfile, rfile))
