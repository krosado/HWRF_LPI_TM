#! /bin/env python

import logging, os
import produtil.sigsafety, produtil.cd, produtil.run, produtil.setup
import produtil.log
import hwrf_expt
import hwrf_alerts
import hwrf_wcoss

from produtil.log import jlogger
from produtil.cd import NamedDir
from produtil.run import mpi, mpirun, checkrun

def gribber():
    jlogger.info(hwrf_expt.conf.strinterp('config',
            '{stormlabel}: starting regribbing job for {out_prefix}'))
    with NamedDir(hwrf_expt.WORKhwrf,logger=logging.getLogger()) as t:
        hwrf_expt.gribber.uncomplete()
        #hwrf_expt.gribber.unrun()
        hwrf_expt.gribber.run()
    jlogger.info(hwrf_expt.conf.strinterp('config',
            '{stormlabel}: completed regribbing job for {out_prefix}'))

def tracker(n):
    jlogger.info(hwrf_expt.conf.strinterp('config',
            '{stormlabel}: starting domain {dom} tracker job for {out_prefix}',
            dom=n))
    with NamedDir(hwrf_expt.WORKhwrf,logger=logging.getLogger()) as t:
        if n==3:
            hwrf_expt.tracker.run()
        elif n==2:
            hwrf_expt.trackerd02.run()
        elif n==1:
            hwrf_expt.trackerd01.run()
    jlogger.info(hwrf_expt.conf.strinterp(
            'config','{stormlabel}: completed domain {dom} tracker job '
            'for {out_prefix}',dom=n))

def copier():
    post_runs_copier=hwrf_expt.conf.getbool(
        'config','post_runs_wrfcopier',False)
    if not post_runs_copier:
        jlogger.info(hwrf_expt.conf.strinterp('config',
            '{stormlabel}: starting wrfcopier job for {out_prefix}'))
        with NamedDir(hwrf_expt.WORKhwrf,logger=logging.getLogger()) as t:
            hwrf_expt.wrfcopier.run(check_all=True)
        jlogger.info(hwrf_expt.conf.strinterp('config',
            '{stormlabel}: completed wrfcopier job for {out_prefix}'))
    else:
        jlogger.info('Products job will not run wrfcopier, post will do it.')
    gribber()

def products():
    jlogger.info(hwrf_expt.conf.strinterp('config',
            '{stormlabel}: starting nhc_products job for {out_prefix}'))
    with NamedDir(hwrf_expt.WORKhwrf,logger=logging.getLogger()) as t:
        hwrf_expt.nhcp.run()
    jlogger.info(hwrf_expt.conf.strinterp('config',
            '{stormlabel}: completed nhc_products job for {out_prefix}'))

def starter(dryrun):
    conf=hwrf_expt.conf
    myrank=int(os.environ['SCR_COMM_RANK'])
    count=int(os.environ['SCR_COMM_SIZE'])
    logger=conf.log('exhwrf_products')
    extra_trackers=conf.getbool('config','extra_trackers',False)
    ngribbers=0
    ncopiers=0
    run=None
    for rank in xrange(count):
        if rank==0: 
            logger.info('Rank %d runs d03 tracker'%rank)
            if rank==myrank:
                run=lambda: tracker(3)
                whoami='tracker'
        elif rank==1 and extra_trackers:
            logger.info('Rank %d runs d02 tracker'%rank)
            if rank==myrank:
                run=lambda: tracker(2)
                whoami='d02tracker'
        elif rank==2 and extra_trackers:
            logger.info('Rank %d runs d01 tracker'%rank)
            if rank==myrank:
                run=lambda: tracker(1)
                whoami='d01tracker'
        elif rank==count-1:
            logger.info('Rank %d runs wrfcopier'%rank)
            ncopiers+=1
            if rank==myrank:
                run=lambda: copier()
                whoami='copier%d'%ncopiers
        else:
            logger.info('Rank %d runs gribber'%rank)
            ngribbers+=1
            if rank==myrank: 
                run=lambda: gribber()
                whoami='gribber%d'%ngribbers
    if ncopiers<1 or ngribbers<2:
        need=2+1+1
        if extra_trackers: need+=2
        msg='Cannot run products job with %d processors with these settings.'\
            ' I require at least %d.'%(count,need)
        logger.critical(msg)
        sys.exit(2)
    if dryrun:
        return whoami
    else:
        run()

def slave_main():
    """This is run multiple times in parallel, once in each subprocess."""
    rank=int(os.environ['SCR_COMM_RANK'])
    count=int(os.environ['SCR_COMM_SIZE'])
    print 'MPI communicator: rank=%d size=%d'%(rank,count)
    hwrf_expt.init_module()
    hwrf_alerts.add_regrib_alerts()
    hwrf_alerts.add_tracker_alerts()
    subdict={ 'RANK':rank, 'COUNT':count, 'WHO':'regribber', 
              'jobid':produtil.batchsystem.jobid(), 
              'WORKhwrf':hwrf_expt.conf.getdir('WORKhwrf') }

    whoami=starter(dryrun=True)
    subdict['THREAD_WHOAMI']=whoami

    if whoami.find('tracker')>=0:
        # Redirect stdout and stderr to one stream for tracker job:
        if 'TRACKER_LOGS' in os.environ:
            r=os.environ.get('TRACKER_LOGS')
        else:
            r=hwrf_expt.conf.strinterp(
                'config','%(WORKhwrf)s/%(jobid)s-%(THREAD_WHOAMI)s.log')
        rstdout=r % dict(subdict, WHO='tracker', STREAM='out')
        rstderr=r % dict(subdict, WHO='tracker', STREAM='err')
        produtil.log.mpi_redirect(stdoutfile=rstdout,stderrfile=None,
                                  threadname='tracker')
    else:
        # Regribber and copier have one file per stream (out, err).
        if 'REGRIBBER_LOGS' in os.environ:
            r=os.environ['REGRIBBER_LOGS']
        else:
            r=hwrf_expt.conf.strinterp(
                'config',
                '%(WORKhwrf)s/%(jobid)s-%(THREAD_WHOAMI)s.%(STREAM)s',
                threadwhoami=whoami)
        rstdout=r % dict(subdict, WHO='regribber', STREAM='out')
        rstderr=r % dict(subdict, WHO='regribber', STREAM='err')
        logging.getLogger('hwrf').warning(
            'Redirecting regribber %d to: stderr=%s stdout=%s'%
            ( rank, rstderr, rstdout ))
        produtil.log.mpi_redirect(stdoutfile=rstdout,stderrfile=rstderr,
                                  threadname='regrib%d'%(rank,))
    whoami=starter(dryrun=False)

def launchself():
    # Instruct further processes not to re-launch scripts via mpirun:
    os.environ['LAUNCH_SELF']='no'
    # Launch four copies of myself.  We must use mpiserial for this
    # because we need the SCR_COMM_RANK variable:
    checkrun(mpirun(mpi(hwrf_expt.conf.getexe('mpiserial','mpiserial'))
                    [os.path.realpath(__file__)],allranks=True))
    # Calling checkrun ensures the program exits abnormally if
    # mpirun.lsf (or whatever you use) exits abnormally.

def doit(): 
    produtil.setup.setup()
    if 'SCR_COMM_RANK' not in os.environ \
            and os.environ.get('LAUNCH_SELF','yes')=='yes':
        # This is the top level of the job: we are NOT inside an
        # mpi_serial call.

        # Initialize the hwrf_expt and re-call any callbacks for completed products:
        hwrf_expt.init_module()
        logger=logging.getLogger('exhwrf_products')
        hwrf_wcoss.set_vars_for_products(logger)
        logger.info('Ensure incomplete products are marked as such...')
        hwrf_expt.gribber.uncomplete()
        logger.info('Add alerts and delveries...')
        hwrf_alerts.add_nhc_alerts()
        hwrf_alerts.add_regrib_alerts()
        hwrf_alerts.add_wave_alerts()
        logger.warning('''Rerunning dbn_alert for prior jobs' posted files.''')
        hwrf_expt.gribber.call_completed_callbacks()

        # We're in the top-level job.  Launch copies of ourself to run the
        # gribber and tracker:
        logger.warning('---------------------------------------------------')
        logger.warning('LAUNCH PARALLEL PORTION OF SCRIPT------------------')
        logger.warning('---------------------------------------------------')
        launchself()
        logger.warning('---------------------------------------------------')
        logger.warning('PARALLEL PORTION OF SCRIPT HAS ENDED---------------')
        logger.warning('---------------------------------------------------')

        # Gribber and tracker succeeded.  Run the products job:
        products()
    else:
        # We're in a subprocess.  Just run the gribber and tracker and return:
        slave_main()

#cProfile.run('doit()')
doit()
