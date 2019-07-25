#! /bin/env python

import os, produtil.cd, produtil.sigsafety, cProfile, logging, time
import produtil.datastore, produtil.setup, produtil.log
from produtil.log import jlogger
from produtil.cd import NamedDir
from produtil.datastore import COMPLETED, FAILED, UNSTARTED

def done(task):
    state=task.getstate()
    return state == COMPLETED or state==FAILED

def post():
    produtil.setup.setup()
    jlogger.info('starting post')
    import hwrf_expt
    hwrf_expt.init_module()

    run_copier=hwrf_expt.conf.getbool('config','post_runs_wrfcopier',False)
    run_satpost=hwrf_expt.conf.getbool('config','run_satpost',True)

    # Make sure we check all tasks to see if they're posted:
    hwrf_expt.nonsatpost.state=UNSTARTED
    hwrf_expt.satpost.state=UNSTARTED

    if run_copier:
        hwrf_expt.wrfcopier.state=UNSTARTED

    logger=logging.getLogger('exhwrf_post')

    # Change to a temp directory to run the  post:
    with NamedDir(hwrf_expt.WORKhwrf,logger=logger) as t:
        #hwrf_expt.ds.dump() # dump entire database state to stdout
        alldone=False
        while not alldone:
            before=int(time.time())
            if run_copier: 
                if not done(hwrf_expt.wrfcopier):    
                    hwrf_expt.wrfcopier.runpart()
            if not done(hwrf_expt.nonsatpost):   hwrf_expt.nonsatpost.runpart()
            if not done(hwrf_expt.nonsatpost):   hwrf_expt.nonsatpost.runpart()
            if run_satpost:
                if not done(hwrf_expt.satpost):  hwrf_expt.satpost.runpart()
            if not done(hwrf_expt.nonsatpost):   hwrf_expt.nonsatpost.runpart()
            alldone = ( done(hwrf_expt.satpost) or not run_satpost ) \
                and done(hwrf_expt.nonsatpost) \
                and ( not run_copier or done(hwrf_expt.wrfcopier) )
            after=int(time.time())
            took=after-before
            threshold=5
            sleeptime=20
            if took < threshold:
                logger.info(
                    'Post loop iteration took only %d seconds, which is '
                    'less than the threshold of %d seconds.  Will sleep '
                    '%d seconds.'%(took,threshold,sleeptime))
                time.sleep(sleeptime)
            else:
                logger.info('Post loop iteration took %d seconds, '
                            'which is above the threshold of %d.  '
                            'Sleeping only one second.'%(took,threshold))
                time.sleep(1) # avoid thrash loop in case of logic error
            logger.info('Done sleeping.')

    jlogger.info('completed post')

post()
#cProfile.run('post()') # use instead of post() to get profiling info
