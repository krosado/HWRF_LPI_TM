__all__=['retry_io']

import time
import logging
import random

def retry_io(max_tries,sleep_time,operation,opargs=[],logger=None,
             fail=None,failargs=[],giveup=None,giveupargs=[],randsleep=True,
             backoff=1.3,first_warn=0,giveup_quiet=False):
    """This function automates retrying an unreliable operation
    several times until it succeeds.  This subroutine will retry the
    operation up to a maximum number of times.  If the operation fails
    too many times, then the last exception thrown by the operation is
    passed on (raised) to the caller.

        max_tries  = maximum number of times to attempt the operation (mandatory)
        sleep_time = time to sleep between tries
        operation  = a function or callable object that may thrown an Exception
        opargs     = a list containing arguments to the operation
        logger     = a logging.Logger object to use for logging, or None
                     to disable logging.
        fail       = a string to print, or a function to call, when 
                     the operation fails but more retries are possible
        failargs   = optional: a list of arguments to fail, or None to disable
        giveup     = a string to print, or a function to call when the
                     operation fails too many times, causing retry_io
                     to give up.  Default: same as fail
        giveupargs = optional: a list of arguments to giveup, or None to disable
        randsleep  = set to True (default) to enable an exponential backoff
                     algorithm, which will increase the sleep time between tries
        backoff    = the exponent for the exponential backoff algorithm
        first_warn = the first failure at which to warn via the logger
        giveup_quiet = if True, a WARNING-level message is sent to the logger
                     if the operation fails more than max_tries times.

    If fail or giveup are functions, they are passed the contents of
    failargs (default: opargs) or giveupargs (default: failargs or
    opargs) with several additional arguments appended.  Those
    arguments are the exception that was caught, the number of
    attempts so far, the max_tries, the sleep_time, and then a boolean
    that is true iff the operation is about to be retried.

    Return value: the return value of the operation."""
        
    # Handle default arguments:
    if fail is not None:
        if failargs is None: failargs=opargs
        if giveup is None: giveup=fail
    if giveup is not None:
        if giveupargs is None: giveupargs=failargs

    if sleep_time is None:
        sleep_time=0.1
    sleepme=sleep_time

    for ntry in xrange(max_tries):
        try:
            if logger is not None:
                logger.debug('%s(%s)'%(repr(operation),repr(opargs)))
            return operation(*opargs)
        except(Exception) as e:
            if(ntry<max_tries-1):
                # Failed but have not given up yet.
                if logger is not None:
                    logger.debug('Failed but have not given up: %s'%(str(e),),
                                 exc_info=True)
                if sleep_time is not None:
                    if randsleep:
                        sleepmax=min(sleep_time,0.05) * \
                            min(max(1.0,backoff**ntry),50.0)
                        sleepme=random.uniform(sleepmax/2.0,sleepmax)
                if isinstance(fail,basestring):
                    if logger is not None and ntry>=first_warn:
                        logger.info("%s (try %d/%d; sleep %.3f and retry): %s"%\
                                           (fail,ntry+1,max_tries,sleepme,repr(e)))
                elif fail is not None:
                    arglist=failargs[:]
                    arglist.extend([e,ntry+1,max_tries,sleep_time,True])
                    if logger is not None:
                        logger.debug('arglist to fail (1): '+repr(arglist))
                    fail(*arglist)
                time.sleep(max(0.05,sleepme))
            else:
                # Gave up.
                if logger is not None:
                    logger.debug('Failed and gave up: %s'%(str(e),),
                                 exc_info=True)
                if isinstance(giveup,basestring) and not giveup_quiet:
                    if logger is not None:
                        logger.warning("%s (giving up after %d tries): %s"%\
                                          (giveup,ntry+1,repr(e)),exc_info=True)
                elif giveup is not None:
                    arglist=giveupargs[:]
                    arglist.extend([e,ntry+1,max_tries,sleep_time,False])
                    if logger is not None:
                        logger.debug('arglist to fail (2): '+repr(arglist))
                    if isinstance(fail,basestring):
                        if giveup_quiet:
                            logger.warning(fail)
                    else:
                        fail(*arglist)
                raise
