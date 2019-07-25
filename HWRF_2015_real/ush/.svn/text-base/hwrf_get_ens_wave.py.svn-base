#! /usr/bin/env python

import tempfile
import subprocess
import os
import re
import logging
import sys
import getopt

import produtil.cd
import hwrf.archive

basedir='/NCEPDEV/hpssuser/g01/hurpara/hwrf_trunk_2013_ens_GRIB2'
verbosity=logging.INFO
namere=re.compile('([a-zA-Z0-9_-]+)([0-9][0-9])([a-zA-Z])\.([0-9]{10})')

########################################################################
# Utility functions

def guess_wave_filenames(path,maxdom=3):
    arbase=os.path.basename(path)
    # Get the storm name, cycle, ID and basin:
    m=namere.search(arbase)
    if m is None:
        return []
    base="%s%s%s.%s.out4wave" % m.group(1,2,3,4) # Get beginning of all out4wave filenames
    return ["%s_d%02d" % (base,1+i) for i in xrange(maxdom) ] # append domain number and return

def initlogging():
    logging.basicConfig(level=verbosity,format='%(asctime)s %(levelname)8s - %(message)s')

def usage(why=None):
    logging.critical("""FORMAT: hwrf_get_ens_wave.py [options] stormprefix cycle
Example: hwrf_get_ens_wave.py humberto09l 2013091306

Will grab the data for humberto09l cycle 2013091306 in a subdirectory
humberto09l.2013091306.out4wave of the current directory.  For a list
of possible storms and cycles, see:

  hsi ls %s/20

where 20 refers to ensemble member 20.  Generally all ensemble members
have the same storms and cycles run for them.

Options:

  -h /directory
       Overrides the directory to go for ensemble data
       Default: %s
  -v
       Verbose

"""%(basedir,basedir))
    if why is None:
        logging.critical('SCRIPT IS EXITING DUE TO INCORRECT ARGUMENTS.')
    else:
        logging.critical(why)
    sys.exit(2)

########################################################################
# Parse arguments

try:
    opts,args = getopt.gnu_getopt(sys.argv,'h:v')
except getopt.GetoptError as err:
    usage(str(err))
for opt,arg in opts:
    if opt=='-v':
        verbosity=logging.DEBUG
    elif opt=='-h':
        basedir=str(arg)

if(len(sys.argv)!=3):
    usage('SYNTAX ERROR: Specify exactly two non-option arguments: storm and cycle')
storm=sys.argv[1]    #'humberto09l'
cycle=sys.argv[2]    #'2013091306'

initlogging()

########################################################################
# Create output directory, check for user stupidity:

outdir='%s.%s.out4wave'%(storm,cycle)
if os.path.exists(outdir):
    if not os.path.isdir(outdir):
        logging.error('%s: I need to make this directory but there is already a non-directory file there'%outdir)
        sys.exit(2)
else:
    try:
        logging.info('Making output directory %s'%outdir)
        os.mkdir(outdir)
    except(IOError,OSError) as e:
        logging.info('%s: cannot make directory: %s'%(outdir,repr(e)))
        sys.exit(2)

########################################################################
# Grab some data:

with produtil.cd.TempDir('.tmpdir','archive.',os.getcwd()+'/.') as t:
    for ens in range(1,21):
        archivename="hpss:%s/%02d/%s.%s.tar" % (basedir,ens,storm,cycle)
        logging.info('Archive name for ensemble id %02d: %s'%(ens,archivename))
        try:
            a=hwrf.archive.hwrfarchive(archivename)
            if(ens<2):
                a.sane()
            logging.debug('archive: '+repr(a.name()))
            if not a.exists():
                logging.error('%s: member %d archive does not appear to exist'%(archivename,ens))
            wavefiles=guess_wave_filenames(archivename)
            logging.debug('wave guess: ['+','.join(wavefiles)+']')
            a.get_files(wavefiles)
            for wfile in wavefiles:
                if os.path.exists(wfile):
                    newname='../%s/ens-%02d-%s'%(outdir,ens,os.path.basename(wfile))
                    logging.info('%s: move to %s'%(wfile,newname))
                    os.rename(wfile,newname)
                else:
                    logging.warning('%s: missing'%wfile)
        except(IOError,OSError) as e:
            logging.error('ens %02d: uncaught exception: %s'%(ens,repr(e)))


logging.info('Success.')
