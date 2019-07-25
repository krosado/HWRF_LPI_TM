#! /bin/env python

import sys, os, glob
import produtil.setup, produtil.log, produtil.run, produtil.cd
import hwrf_expt
from produtil.log import postmsg, jlogger
from produtil.run import exe, checkrun, run
from produtil.cd import NamedDir

def main_disk():
    postmsg('hwrf_archive disk step starting')
    conf=hwrf_expt.conf
    logger=conf.log('archive.disk')
    if conf.has_section('archive'):
        makethedir=conf.getbool('archive','mkdir',False)
    else:
        makethedir=False
    archive=conf.getloc('archive','NONE')
    if archive=='NONE':
        logger.info('No archive location specified.  Exiting.')
        postmsg('hwrf_archive disk step has nothing to do when archiving is '
                'disabled.')
        return
    with NamedDir(conf.getdir('com')):
        flist=[ filename+'\n' for filename in glob.glob('*') ]
        flist.sort()
        files=''.join(flist)
        assert(len(files)>0)
        if archive.lower()=='none':
            postmsg('Archiving is disabled: archive=none')
            return
        elif archive[0:5]=='disk:':
            path=archive[5:]
            if makethedir:
                adir=os.path.dirname(path)
                if not os.path.exists(adir):
                    produtil.fileop.makedirs(adir,logger=logger)
            flags='-cvp'
            if path[-3:]=='.gz' or path[-4:]=='.tgz':
                flags+='z'
            cmd=exe(conf.getexe('tar'))[flags+'f',path,'-T','-'] << files
        elif archive[0:5]=='hpss:':
            logger.info('HPSS archiving enabled.')
            logger.info('Nothing to do in the disk archiving step.')
            logger.info('Returning successfully after doing nothing.')
            postmsg('hwrf_archive disk step does nothing when using htar '
                    'archives.')
            return
        elif archive[0:5]=='hpsz:':
            path=conf.strinterp('config','{WORKhwrf}/stage-archive.tar.gz')
            cmd=exe(conf.getexe('tar'))['-cvpf',path,'-T','-'] << files
        else:
            jlogger.error('Ignoring invalid archive method %s in %s'
                          %(archive[0:4],archive))
            return
        checkrun(cmd,logger=logger)
    postmsg('hwrf_archive disk step completed')

def main_tape():
    postmsg('hwrf_archive tape step starting')
    conf=hwrf_expt.conf
    logger=conf.log('archive.disk')
    archive=conf.getloc('archive','NONE')
    if conf.has_section('archive'):
        makethedir=conf.getbool('archive','mkdir',False)
    else:
        makethedir=False
    if archive=='NONE':
        logger.info('No archive location specified.  Exiting.')
        postmsg('hwrf_archive disk step has nothing to do when archiving is '
                'disabled.')
        return
    with NamedDir(conf.getdir('com')):
        flist=[ filename+'\n' for filename in glob.glob('*') ]
        flist.sort()
        files=''.join(flist)
        assert(len(files)>0)

        if archive.lower()=='none':
            postmsg('Archiving is disabled: archive=none')
            return
        elif archive[0:5]=='disk:':
            logger.info('Disk archiving enabled.')
            logger.info('Nothing to do in the HPSS archiving step.')
            logger.info('Returning successfully after doing nothing.')
            postmsg('hwrf_archive tape step does nothing when using disk '
                    'archives.')
            return
        elif archive[0:5]!='hpss:' and archive[0:5]!='hpsz:':
            jlogger.error('Ignoring invalid archive method %s in %s'
                          %(archive[0:4],archive))
            return

        if makethedir:
            adir=os.path.dirname(archive[5:])
            logger.info('%s: make this HPSS directory, even if it exists'%(adir,))
            mcmd=exe(conf.getexe('hsi'))['-P','mkdir','-p',adir]
            run(mcmd,logger=logger)

        if archive[0:5]=='hpss:':
            path=archive[5:]
            flags='-cvp'
            cmd=exe(conf.getexe('htar'))[flags+'f',path,'-L','-'] << files
        elif archive[0:5]=='hpsz:':
            
            topath=archive[5:]
            frompath=conf.strinterp('config',
                                    '{WORKhwrf}/stage-archive.tar.gz')
            cmd=exe(conf.getexe('hsi'))['put',frompath,':',topath]
        checkrun(cmd,logger=logger)
    postmsg('hwrf_archive tape step completed')

if __name__=='__main__':
    try:
        acase=os.environ.get('ARCHIVE_STEP','BOTH').upper()
        produtil.setup.setup()
        hwrf_expt.init_module()
        if acase == 'DISK':
            main_disk()
        elif acase == 'TAPE':
            main_tape()
        elif acase == 'BOTH':
            main_disk()
            main_tape()
        else:
            postmsg('INVALID JHWRF_ARCHIVE STEP %s!! ABORTING!'
                    %(repr(acase),))
    except Exception as e:
        jlogger.critical('hwrf_archive is aborting: '+str(e),exc_info=True)
        sys.exit(2)
