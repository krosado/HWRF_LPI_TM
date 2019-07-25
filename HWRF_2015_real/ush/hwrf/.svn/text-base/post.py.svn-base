"""Runs the Unified Post Processor on outputs from the WRF-NMM,
producing E grid GRIB files as EGRIB1Product objects."""

__all__=['PostOneWRF','PostManyWRF','EGRIBProduct','check_post',
         'link_post_fix']

import shutil,glob,os,os.path,stat,time,math, re


import produtil.cd,produtil.fileop,produtil.run, produtil.datastore
import produtil.locking
import hwrf.hwrftask,hwrf.numerics, hwrf.regrib, hwrf.exceptions

from hwrf.exceptions import PostFailed
from hwrf.regrib import GRIB1File,UpstreamGRIB1

from produtil.datastore import FileProduct,COMPLETED,FAILED,RUNNING,UNSTARTED
from produtil.cd import TempDir
from produtil.fileop import *
from produtil.run import *

from hwrf.hwrftask import HWRFTask
from hwrf.numerics import *

########################################################################

def check_post(retval,what,logger):
    """Checks the current working directory and the specified return
    value retval from the post to determine if the post succeeded.
    Returns a four-element tuple (ok,cenla,cenlo,filename) where "ok"
    is True if the post succeded, (cenla,cenlo) is the domain center
    and filename is the name of the post output file."""
    if retval!=0:
        # The post return value is not reliable, so verify the
            # simulation failed by checking for the post's completion
            # message.
        logger.warning('%s:Non-zero exit status %d from post.'%(what,retval))
            # Check the last 100kb or so for "ALL GRIDS PROCESSED"
        with open('vpost.log','rb') as f:
            f.seek(0,os.SEEK_END)
            filesize=f.tell()
            readsize=min(102400,filesize)
            f.seek(-readsize,os.SEEK_END)
            vpostdat=f.read(readsize)
            if(-1 == vpostdat.find('ALL GRIDS PROCESSED')):
                logger.warning(
                    '%s: Did not find "ALL GRIDS PROCESSED" in last %d '
                    'bytes of vpost.log (file size %d).  Post probably '
                    'failed'%(what,readsize,filesize))
                lins=vpostdat.splitlines()
                for lin in lins[-50:]:
                    logger.warning('vpost.log: %s'%(lin,))
                return (False,None,None,None)
    if os.path.exists('vpost.log'):
        with open('vpost.log','rt') as f:
            pass
    # At this point, it seems likely that the post THINKS it succeded.
    # Get the cenla/cenlo from the first 100kB of
    # this-domain-center.ksh.inc
    cenla=None
    cenlo=None
    sentexc=False
    with open('this-domain-center.ksh.inc','rt') as f:
        for line in f.readlines(102400):
            try:
                m=re.search('(clat|clon)\s*=\s*([+-]?[.0-9]+)',line)
                if m:
                    (varr,where)=m.groups()
                    if varr=='clat': cenla=float(where)
                    if varr=='clon': cenlo=float(where)
            except(KeyError,ValueError,TypeError,AttributeError) as e:
                if not sentexc:
                    sentexc=True
                    logger.warning(
                        '%s: Exception while parsing this-domain-'
                        'center.ksh.inc'%(what,),exc_info=True)
    if cenla is None:
        logger.warning('%s: Could not get clat from this-domain-'
                       'center.ksh.in'%(what,))
    if cenlo is None:
            logger.warning('%s: Could not get clon from this-domain-'
                           'center.ksh.in'%(what,))
    ok=cenla is not None and cenlo is not None
    # Now we need to find the post output file.
    for filename in glob.glob('WRFPRS*'):
        logger.info('%s: success: cenla=%s cenlo=%s file=%s.'
                    %(what,cenla,cenlo,filename))
        return (ok,cenla,cenlo,filename)
    logger.warning('%s: No WRFPRS* files found.'%(what,))
    run(exe('ls')['-ltr'])
    run(exe('tail')['-20','vpost.log'])
    return(False,None,None,None)

########################################################################

def link_post_fix(fixd,needcrtm,logger=None):
    """Links or copies all fix files for the post to the current
    working directory."""
    
    if logger is not None:
        logger.info('%s: link post fix files here.  Needcrtm=%s'%(
                str(os.getcwd()),repr(needcrtm)))

    # Copy the microphysics lookup table to many different names:
    #old: shutil.copy(fixd+'/hwrf_eta_micro_lookup.dat','./eta_micro_lookup.dat')
    for tgt in [ 'eta_micro_lookup.dat', 'nam_micro_lookup.dat', 
               'hires_micro_lookup.dat' ]:
        # Note about micro lookups: the code opens these for
        # read/write, not read, so we cannot link.  We have to create
        # a local copy.  The preserve_perms=False prevents us from
        # turning off write access if the fix file copy is read-only.
        src=os.path.join(fixd,'hwrf-wrf','ETAMPNEW_DATA.expanded_rain')
        produtil.fileop.deliver_file(src,tgt,logger=logger,keep=True,
                                     preserve_perms=False)

    # CRTM coefficient files if needed:
    if needcrtm:
        crtmd=os.path.join(fixd,'hwrf-crtm-2.0.6')
        links=[]
        for src in ( "amsre_aqua", "imgr_g11", "imgr_g12", "imgr_g13", 
                     "imgr_g15", "imgr_mt1r", "imgr_mt2", "seviri_m10",
                     "ssmi_f13", "ssmi_f14", "ssmi_f15", "ssmis_f16",
                     "ssmis_f17", "ssmis_f18", "ssmis_f19", "ssmis_f20",
                     "tmi_trmm", "v.seviri_m10", "imgr_insat3d" ):
            links.append('%s/SpcCoeff/Big_Endian/%s.SpcCoeff.bin'%(crtmd,src))
            links.append('%s/TauCoeff/Big_Endian/%s.TauCoeff.bin'%(crtmd,src))
        for src in ( 'Aerosol','Emis','Cloud' ):
            links.append('%s/%sCoeff/Big_Endian/%sCoeff.bin'%(crtmd,src,src))
        make_symlinks_in(links,'.',logger=logger,force=True)
        
########################################################################

wrf_hr_min = fcst_hr_min

class EGRIB1Product(hwrf.regrib.UpstreamGRIB1):
    """This represents an E grid WRF-NMM GRIB1 file, and stores two
    metadata values: CENLA and CENLO which contain the domain center
    location."""
    def deliver(self,location,fileinfo,logger=None):
        """Copies the file to its destination, and sets the CENLA and
        CENLO metadata values to the domain center."""
        assert(location is not None)
        dirname=os.path.dirname(location)
        if not os.path.exists(dirname):
            os.makedirs(dirname)
        deliver_file(fileinfo['fromloc'],location,keep=False,logger=logger)
        cenla=fileinfo['CENLA']
        cenlo=fileinfo['CENLO']
        assert(cenla is not None)
        assert(cenlo is not None)
        with self.dstore.transaction() as t:
            self['CENLA']=cenla
            self['CENLO']=cenlo
            self.set_loc_avail(location,True)
            self.update()
            assert('CENLA' in self)
            assert('CENLO' in self)
        self.call_callbacks(logger=logger)
    def make_location(self,outdir):
        """Decides a filename which is of the format
        "outdir/category/prodname"."""
        return os.path.join(os.path.join(outdir,self.category),self.prodname)
    def make(self,regrib,*args,**kwargs):
        loc=self.location
        (filename,index)=regrib.gribtemp('prod.'+os.path.basename(loc))
        produtil.fileop.deliver_file(loc,filename,logger=regrib.logger)
        return GRIB1File(filename,None,None,self['CENLA'],self['CENLO'])
    def getnscenter(self): return self['CENLA']
    def getewcenter(self): return self['CENLO']
    nscenter=property(getnscenter,None,None,
        'Returns None or the center latitude of this GRIB1 file.')
    ewcenter=property(getewcenter,None,None,
        'Returns None or the center longitude of this GRIB1 file.')

########################################################################

class PostOneWRF(HWRFTask):
    """This is an HWRFTask that post-processes output data for a
    single WRF stream, from several WRF domains at a a given time."""
    def __init__(self,wrf,domains,conf,section,time,stream='history',
                 needcrtm=True,grib=1,faketime=None,taskname=None,**kwargs):
        super(PostOneWRF,self).__init__(wrf.dstore,wrf.conf,section,
                                        taskname=taskname,**kwargs)
        self.__needcrtm=needcrtm
        self.__wrf=wrf
        self.__time=to_datetime_rel(time,wrf.wrf().simstart())
        self.__domains=domains
        self.__stream=stream
        self.__wrfproducts={}
        self.__myproducts={}
        self.__grib2 = (grib==2)
        assert(domains)
        prodext = ( 'egrb2' if(grib==2) else 'egrb' )
        if grib==2:
            raise NotImplementedError(
                'GRIB2 support not yet implemented in hwrf.post')
        added=False
        for domain in self.__domains:
            first=True
            assert(stream is  not None)
            assert(time is not None)
            for product in wrf.products(domains=[domain],stream=stream,
                                        time=self.__time):
                added=True
                assert(first)
                first=False
                self.__wrfproducts[domain]=product
                self.log().debug('%s added %s => %s'%(self.taskname, 
                  repr(domain),repr(product)))
                prodname=self.product_name(domain)
                self.__myproducts[domain]=\
                    EGRIB1Product(self.dstore,category=self.taskname,prodname=prodname)
        assert(added)
    def product_name(self,domain):
        """Returns a human readable string representation of the
        product name for the given domain.  This is used for filenames
        and product ids."""
        ext = ('egrb2' if(self.__grib2) else 'egrb')
        if domain is None: 
            result='%s.%s'%(self.taskname,ext)                # nonsatpost.egrb1
        else:
            result='%s-%s.%s'%(self.taskname,domain.name,ext) # nonsatpost-moad.egrb1
        result=re.sub('[^a-zA-Z0-9_.-]','_',result)
        return result
    def wrf(self):
        """Returns the WRFSimulation object."""
        return self.__wrf.wrf()
    def wrftask(self):
        """Returns the Task that represents the running WRF simulation."""
        return self.__wrf
    def products(self,*args,**kwargs):
        if 'domains' in kwargs:
            for domain in kwargs['domains']:
                if domain in self.__myproducts:
                    yield self.__myproducts[domain]
        elif 'domain' in kwargs and kwargs['domain'] in self.__myproducts:
            yield self.__myproducts[kwargs['domain']]
        else:
            for domain in self.__myproducts:
                yield self.__myproducts[domain]
    def make_control(self,stream):
        # Figure out what control file to use from the conf section
        # for this task.  We look for a stream-specific one first
        # (ie.: "auxhist2_control").  If that is missing we use a
        # default control file from the "control" variable:
        Missing='**MISSING**'
        control=self.confget('%s_control'%(stream,),Missing)
        if control is Missing:   control=self.confstr('control')
        self.log().debug('Use control file %s'%(control,))
        shutil.copy(control,'./fort.14')

    def link_fix(self):
        fixd=self.getdir('FIXhwrf')
        needcrtm=self.__needcrtm
        link_post_fix(fixd,needcrtm,logger=self.log())

    def requested_time(self):
        """Returns the forecast time that is being processed."""
        return self.__time
    def domains(self):
        """Iterates over all domains that will be processed."""
        for domain in self.__domains: yield domain
    def domprod(self):
        """Iterates over tuples (domain,wrfproduct,myproduct) where
        domain is the WRF domain, wrfproduct is the WRF output Product
        from that domain for the chosen time and myproduct is the
        output EGRIBProduct for that time."""
        for domain in self.__domains:
            yield domain,self.__wrfproducts[domain],self.__myproducts[domain]
    def del_post_output(self):
        """Deletes any post output files from the current working
        directory."""
        for filename in glob.glob('./WRFPRS*'):
            os.unlink(filename)
        if os.path.exists('this-domain-center.ksh.inc'):
            os.unlink('this-domain-center.ksh.inc')
    def check_post(self,retval,what):
        return check_post(retval,what,self.log())
    def can_run(self):
        for domain,wrfprod,myprod in self.domprod():
            if myprod.available: continue
            if not wrfprod.available:
                wrfprod.check()
                if not wrfprod.available:
                    self.log().debug(
                        '%s: cannot run: %s not available (loc=%s avail=%s)'%(
                            self.taskname, wrfprod.did, 
                            repr(wrfprod.location), repr(wrfprod.available)))
                    return False
        return True
    def run(self, nosleep=False, raiseall=False):
        state=self.getstate()
        logger=self.log()
        lastsync=0
        if state is COMPLETED or state is FAILED:
            return
        with TempDir(prefix='post.%s.'%(self.product_name(None),),dir='.',
                     suffix='.work',logger=self.log()) as tempdir:
            self.log().info('cwd: '+os.getcwd())
            assert(not re.match('\A/tmp',os.getcwd()))
            self.link_fix()
            done=set()   # set of my products I already know are delivered
            gaveup=set() # set of my products that I gave up on producing
            tries=dict()
            count=len(self.__domains) # total number of products
            outdir=self.outdir
            while len(done)+len(gaveup)<count:
                message='status: '
                for domain,wrfproduct,myproduct in self.domprod():
                    what=domain.name
                    if domain in done or myproduct.is_available():
                        done.add(domain)
                        message+='[%s: done] '%(domain.name,)
                    elif not wrfproduct.is_available():
                        now=int(time.time())
                        if(now>lastsync+30):
                            logger.info('Calling sync...')
                            run(exe('sync'),logger=logger)
                            logger.info('Returned from sync.')
                            lastsync=int(time.time())
                        wrfproduct.check()

                    if domain in gaveup:
                        pass
                    elif domain in tries and tries[domain]>=5:
                        gaveup.add(domain)
                    elif wrfproduct.is_available():
                        try:
                            if domain in tries:
                                tries[domain]+=1
                            else:
                                tries[domain]=1
                            self.del_post_output()
                            make_symlink(wrfproduct.location,'./INFILE',
                                         force=True)
                            self.make_control(wrfproduct['stream'])
                            wrf_base=os.path.basename(wrfproduct.location)
                            match=re.search(
                                '(\d\d\d\d.\d\d.\d\d.\d\d.\d\d.\d\d)',
                                wrf_base)
                            if match and len(match.groups())>0:
                                #print 'match group = ',repr(match.groups())
                                datestamp=match.groups()[0]
                            else:
                                datestamp=self.__time.strftime(
                                    '%Y-%m-%d_%H:%M:%S')
                                # NOTE: raiseall=True should not raise
                                # here since this is a normal,
                                # expected, condition when running the
                                # post on the 1 minute forecast in the
                                # JHWRF_INIT jobs.
                                logger.warning(
                                    '%s: %s: cannot get datestamp from this '
                                    'name; will guess %s'%\
                                        (what,wrf_base,datestamp))
                            # Workaround needed for pnetcdf support:
                            datestamp = datestamp[0:4]+'-'+datestamp[5:7]+\
                                '-'+datestamp[8:10]+'_'+datestamp[11:13]+\
                                ':'+datestamp[14:16]+':'+datestamp[17:19]
                            cmd = mpirun(mpi(self.getexe('post')),
                                         allranks=True) > 'vpost.log'
                            logger.info('Post command: %s'%(repr(cmd),))
                            #cmd=exe('false')
                            with open('itag','wt') as itag:
                                itag.write("""INFILE
netcdf
%s
NMM NEST
""" % (datestamp,))
                            ret = run(cmd)
                            (ok,cenla,cenlo,filename) = self.check_post(
                                ret,what)
                            if ok:
                                logger.info('%s: deliver'%(what,))
                                myproduct.deliver(myproduct.make_location(
                                        outdir),{'CENLA':cenla, 'CENLO':cenlo,
                                         'fromloc':filename},logger=logger)
                                done.add(domain)
                                message+='[%s: just posted] '%(domain.name,)
                            elif raiseall:
                                msg='%s: failed'%(what,)
                                logger.warning(msg)
                                raise PostFailed(msg)
                            else:
                                logger.info('%s: failed'%(what,))
                                message+='[%s: post failed %d times] '%(
                                    domain.name,tries[domain])
                        except Exception as e:
                            logger.warning('%s: Exception caught in post: %s'%(
                                    what,str(e)),exc_info=True)
                            message+='[%s: exception] '%(domain.name,)
                            raise
                    elif wrfproduct.location:
                        message+='[%s (%s) unavailable] '%(
                            str(wrfproduct.location),domain.name)   
                        if raiseall:
                            raise PostHasNoInput(
                                "%s: %s: not available (should be at %s but "
                                "available=False)"%(domain.name,wrfproduct.
                                 did,wrfproduct.location))
                    else:
                        message+='[%s: unavailable] '%(domain.name,)
                        if raiseall:
                            raise PostHasNoInput(
                                "%s: %s: not available, and location=None"%(
                                    domain.name,wrfproduct.did))
                #print message
                logger.info(message)
                if len(done)+len(gaveup)<count:
                    logger.info('Sleep 30...')
                    time.sleep(30)
                    logger.info('Done sleeping.')
                    if nosleep: return
                else:
                    if len(gaveup)>0:
                        logger.critical('state=FAILED')
                        self.setstate(FAILED)
                    else:
                        logger.info('state=COMPLETED')
                        self.setstate(COMPLETED)

########################################################################

class PostManyWRF(HWRFTask):
    def __init__(self,wrf,domains,conf,section,step,postclass=PostOneWRF,
                 start=None,end=None,streams=['history'],
                 needcrtm=True,grib=1,taskname=None,**kwargs):
        super(PostManyWRF,self).__init__(wrf.dstore,wrf.conf,
            section,taskname=taskname,**kwargs)
        self._needcrtm=needcrtm
        self.__wrf=wrf
        self._subtasks=[]
        self.__done=set()
        self._postclass=postclass

        #print 'process streams'
        # Get the earliest start time and latest end time of the output:
        (istart,iend,iinterval)=self.wrf()[domains[0]].get_output_range(
            streams[0])
        keepstreams=list()
        for stream in streams:
            try:
                for domain in domains:
                    (jstart,jend,jinterval)=self.wrf()[domain].\
                        get_output_range(stream)
                    if jstart<istart: istart=jstart
                    if jend>iend: iend=jend
                keepstreams.append(stream)
            except hwrf.exceptions.OutputStreamDisabled:
                pass
        streams=keepstreams
        #print repr(streams)
        #print istart,iend

        # Figure out the start and end time for data processing.
        # We'll use the istart, iend that we just computed for start
        # and end if they're missing:
        start = istart if start is None else start
        end = iend if end is None else iend
        interval = step

        # Convert start and end to datetime objects:
        start=to_datetime_rel(start,istart) # start relative to istart
        end=to_datetime_rel(end,start) # end is relative to start
        interval=to_timedelta(interval)

        #print repr(start),repr(end),repr(interval)

        self._step=interval

        # Get the WRF object's version of the domains since it will be
        # filled with additional information we need about file
        # locations, etc.:
        self.__mydomains=[self.wrf()[domain] for domain in domains]

        # Now generate the subtasks:
        self._add_subtasks(streams,start,end)
    def _add_subtasks(self,streams,start,end):
        """Fills the self._subtasks array.  It figures out which times
        have data for all domains and creates a PostOneWRF object for
        each of those times.  The subtasks are created in temporal
        order."""
        interval=self._step
        when=start
        epsilon=to_fraction(interval)/10
        last=dict()
        logger=self.log()
        ende=to_datetime_rel(epsilon,end)
        while when<ende:
            this=dict()
            for stream in streams:
                logger.debug('  check stream %s'%(stream))
                ok=False
                for domain in self.domains():
                    key=(domain,stream)
                    this[key]=domain.get_output(stream,when,logger=self.log())
                    if this[key] is None:
                        continue
                    logger.debug('  domain %s stream %s time %s result %s'% \
                        (repr(domain),repr(stream),repr(when),repr(this[key])))
                    attime=this[key].validtime()
                    dt=abs(to_fraction(attime-when,negok=True))
                    if dt<epsilon:
                        logger.debug('  dt=%s-%s=%s > epsilon=%s'%(
                                repr(attime),repr(when),repr(dt),
                                repr(epsilon)))
                        okstream=stream
                        ok=True
                        break
                if ok: break
            if ok:
                logger.debug('Adding post %s with stream %s time %s'%(
                        self.taskname_for(when),repr(stream),repr(when)))
                #print 'use stream ',stream,' for time ',repr(when)
                self._subtasks.append( (when,
                    self._postclass(self.wrftask(),[
                                x for x in self.domains()],self._conf,
                               self._section,when,stream=okstream,
                                    needcrtm=self._needcrtm,
                                    taskname=self.taskname_for(when)) ) )
                last=this
            else:
                logger.debug('%s: ignoring duplicate output time due to WRF '
                             'output frequency'%(when.strftime(
                            '%Y-%m-%d %H:%M:%S'),))
            when+=interval
        self.log().debug('len(self._subtasks)=%s'%(repr(len(self._subtasks))))
    def domains(self):
        """Iterates over all WRFDomain objects."""
        for x in self.__mydomains:
            yield x
    def subtasks(self):
        """Iterator that loops over all subtasks yielding a tuple:
          (itask,rtime,subtask)
        Where:
          itask = task index from 0 to ntasks-1
          rtime = output time this task processes
          subtask = the Task object"""
        for x in xrange(len(self._subtasks)):
            yield (x,self._subtasks[x][0],self._subtasks[x][1])
    def unrun(self):
        """Calls uncomplete, and then deletes all products."""
        self.uncomplete()
        for product in self.products(): product.undeliver()
    def uncomplete(self):
        """Marks this task and all subtasks as incomplete so that all
        post-processing will be rerun.  Does not undeliver any
        delivered products."""
        self.state=UNSTARTED
        for itask,rtime,subtask in self.subtasks():
            subtask.state=UNSTARTED
    def taskname_for(self,time):
        """Returns a human-readable taskname for the given subtask
        time."""
        (ihours,iminutes)=wrf_hr_min(time,self.wrf().simstart())
        return '%s-f%02dh%02dm'%(self.taskname,ihours,iminutes)
    def subtask_count(self):
        """Returns the number of subtasks."""
        return len(self._subtasks)
    def starttime(self):
        """Returns the first time to be processed"""
        return self._subtasks[0][1]
    def endtime(self):
        """Returns the last time to be processed"""
        return self._subtasks[len(self._subtasks)-1][1]
    def wrf(self):
        """Returns the WRF object being posted"""
        return self.__wrf.wrf()
    def wrftask(self):
        """Returns the Task that ran WRF"""
        return self.__wrf
    def _run_helper(self,one):
        """Internal implementation function: this implements run and
        runone.  Do not call directly."""
        logger=self.log()
        if self.getstate()==COMPLETED:
            return
        completed=0
        failed=0
        count=self.subtask_count()
        break_outer=False
        lockdir=os.path.join(self.getdir('lockdir'),self.taskname)
        produtil.fileop.makedirs(lockdir)
        while not break_outer and completed+failed<count:
            # Check subtasks to se what we can run
            completed=0 # reset completed task count
            failed=0 # reset failed task count
            n_unrunable=0 # number seen so far that are not failed,
                          # and cannot start
            max_unrunable=5 # number of subtasks to look ahead
            for (itask,rtime,subtask) in self.subtasks():
                state=subtask.getstate()
                if state==COMPLETED:
                    #logger.debug('subtask %s is completed.'%(subtask.taskname,))
                    completed+=1
                    continue
                elif state==FAILED:
                    #logger.debug('subtask %s is failed.'%(subtask.taskname,))
                    failed+=1
                    continue
                elif not subtask.can_run():
                    n_unrunable+=1
                    logger.debug('substask %s cannot run yet.'%(
                            subtask.taskname,))
                    if n_unrunable>max_unrunable:
                        if one: break_outer=True
                        break
                    continue
                lockfile=os.path.join(lockdir,'%s.task%d'%
                    ( rtime.strftime('%Y%m%d.%H%M%S'),itask ))
                locker=produtil.locking.LockFile(
                    filename=lockfile,max_tries=1)
                try:
                    with locker:
                        logger.info('run subtask %s'%(subtask.taskname,))
                        subtask.run(nosleep=True)
                        if subtask.is_completed() and one:
                            break_outer=True
                            break
                except produtil.locking.LockHeld as lh:
                    logger.info('subtask %s: lock held, moving on.'
                                    %(subtask.taskname,))
                except Exception as e:
                    if not self.confbool('ignore_errors',False):
                        logger.error(
                            'aborting: %s raised unexpected exception: %s'%
                            (subtask.taskname,str(e)),exc_info=True)
                        raise
                    else:
                        logger.warning(
                            '%s raised unexpected exception: %s'%
                            (subtask.taskname,str(e)),exc_info=True)
            if not break_outer and completed+failed<count:
                logger.info('Sleep 20...')
                time.sleep(20)
                logger.info('done sleeping.')
        if completed==count:
            self.setstate(COMPLETED)
            self.postmsg('All %d of %d subtasks completed.'%(completed,count))
        elif failed==count:
            self.setstate(FAILED)
            logger.critical('MULTI-TASK WORKSTREAM FAILED.')
    def run(self): self._run_helper(False)
    def runpart(self): self._run_helper(True)
    def products(self,time=None,**kwargs):
        if time is None:
            for (itask,xtime,subtask) in self.subtasks():
                for product in subtask.products(**kwargs):
                    yield product
        else:
            reltime=to_datetime_rel(time,self.wrf().simstart())
            epsilon=to_fraction(self._step/10)
            for (itask,xtime,subtask) in self.subtasks():
                dt=abs(to_fraction(xtime-reltime,negok=True))
                if xtime>=reltime and dt<epsilon:
                    for product in subtask.products(**kwargs):
                        yield product
                    break
