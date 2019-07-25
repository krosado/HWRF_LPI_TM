#! /bin/env python

import os, sys, StringIO, logging
import produtil.setup, produtil.atparse
from os.path import join,dirname

produtil.setup.setup()
logger=logging.getLogger('hwrf_make_jobs')

def job(JJOBNAME,JOBMORE='',EXNAME=None,**kwargs):
    """Makes a dict to pass to the AtParser to generate a job with the
    given specifications.
       JJOBNAME - the JJOB's name: the part after "JHWRF_"
       JOBMORE - unused.  Sets the JOBMORE variable
       EXNAME - ex-script name (the part between exhwrf_ and .py).
                Set automatically from the JJOBNAME if absent.
       **kwargs - inserted into the resulting dict via "update" """
    if EXNAME is None: EXNAME=JJOBNAME.lower()
    out=dict(JJOBNAME=str(JJOBNAME),
             JOBMORE=str(JOBMORE),
             EXNAME=str(EXNAME))
    out.update(PARQ='devmax2',SHAREQ='devmax2_shared')
    out.update(kwargs)
    return out

# List of jobs to create:
jobs = [ job('GSI'), job('BUFRPREP'), job('ENSDA'), job('ENSDA_OUTPUT'),
         job('ENSDA_PRE'), job('FORECAST'), job('GSI_POST'),
         job('INIT'), job('LAUNCH'), job('OUTPUT'), job('UNPOST'), 
         job('MERGE'), job('RELOCATE'), job('OCEAN_INIT'), job('POST'), 
         job('PRODUCTS') ]

# Read the JHWRF.in file:
hwrf_make_jobs_py=os.path.realpath(__file__)
HOMEhwrf=dirname(dirname(hwrf_make_jobs_py))
JOBhwrf=join(HOMEhwrf,'jobs')
JHWRF_in_path=join(JOBhwrf,'JHWRF.in')
try:
    with open(JHWRF_in_path,'rt') as jhwrf_in_file:
        jhwrf_in=jhwrf_in_file.readlines()
except EnvironmentError as e:
    logger.error('%s: %s'%(JHWRF_in_path,str(e)),exc_info=True)
    sys.exit(1)

def make_job(jd,lines,logger):
    """Makes one J-Job by parsing the given lines using an atparser
      jd - a dict to pass to the atparser
      lines - an array of lines from JHWRF.in
      logger - where to send errors"""
    sio=StringIO.StringIO()
    ap=produtil.atparse.ATParser(sio,jd,logger)
    i=0
    for line in lines:
        i+=1
        ap.parse_line(line,'JHWRF.in',i)
    out=sio.getvalue()
    sio.close()
    return out

# Make the jobs:
for jd in jobs:
    filename=os.path.join(JOBhwrf,'JHWRF_'+jd['JJOBNAME'].upper())
    if 'JOBMORE' in jd and jd['JOBMORE']:
        # job run in multiple different ways:
        filename+='.mode.'+jd['JOBMORE']
    contents=make_job(jd,jhwrf_in,logger)
    logger.info('%s: write file'%(filename,))
    with open(filename,'wt') as outf:
        outf.write(contents)
