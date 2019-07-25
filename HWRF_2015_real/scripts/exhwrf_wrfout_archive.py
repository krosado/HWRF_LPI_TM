#! /bin/env python
import os, sys
import produtil.setup, produtil.log, produtil.run, produtil.cd
import hwrf.numerics
import hwrf_expt

from produtil.log import jlogger
from produtil.run import exe, checkrun, run

def main():
    hwrf_expt.init_module()
    conf=hwrf_expt.conf
    if not conf.has_option('archive','wrfout'):
        jlogger.info('No wrfout option in [archive] section.  Will not make wrfout archive.')
        sys.exit(0)

    logger=conf.log()

    files=list()
    dt=hwrf.numerics.to_timedelta('6:00:00')
    t0=conf.cycle
    wrf=hwrf_expt.runwrf.wrf()
    with produtil.cd.NamedDir(hwrf_expt.runwrf.location):
        for i in xrange(22):
            for dom in wrf:
                t=t0+dt*i
                out=dom.get_output('auxhist3',t)
                if out is None:
                    out=dom.get_output('history',t)
                if out is None:
                    out=dom.get_output('auxhist2',t)
                if out is None:
                    logger.error('%s: could not determine wrfout for '
                                 'domain %s'%(t.strftime('%Y%m%d%H'),
                                              str(dom)))
                if not os.path.exists(out.path()):
                    logger.error('%s: does not exist'%(out.path(),))
                if not produtil.fileop.isnonempty(out.path(),):
                    logger.error('%s: is empty'%(out.path(),))
                files.append(out.path())

        thearchive=conf.timestrinterp('archive','{wrfout}',0)
        if thearchive[0:5]!='hpss:':
            logger.error('The wrfout archive path must begin with "hpss:": '+
                         thearchive)
            sys.exit(1)
        thearchive=thearchive[5:]
        adir=os.path.dirname(thearchive)
        mkdir=exe(conf.getexe('hsi'))['-P','mkdir','-p',adir]
        run(mkdir,logger=logger)
        cmd=exe(conf.getexe('htar'))['-cpf',thearchive][files]
        checkrun(cmd,logger=logger)

if __name__=='__main__':
    try:
        produtil.setup.setup()
        jlogger.info("HWRF rundir archive job starting")
        main()
        jlogger.info("HWRF rundir archive job completed")
    except Exception as e:
        jlogger.critical('HWRF rundir archive is aborting: '+str(e),
                         exc_info=True)
        sys.exit(2)
