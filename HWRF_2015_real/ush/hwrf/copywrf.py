__all__ = ['WRFCopyTask']

import os, re, time
import produtil.datastore, produtil.locking, produtil.fileop
import hwrf.hwrftask, hwrf.wrf

from produtil.run import checkrun, run, exe, bigexe, mpi, mpirun
from produtil.datastore import COMPLETED,RUNNING,UNSTARTED,Product,\
    FileProduct,UpstreamFile
from hwrf.hwrftask import HWRFTask

class WRFCopyTask(HWRFTask):
    """This is a Task that copies WRF input and output files from the
    WRF run directory to the COM directory."""
    def __init__(self,dstore,conf,section,wrftask,out_prefix,**kwargs):
        if 'outdir' not in kwargs:
            kwargs['outdir']=conf.getdir('com')
        super(WRFCopyTask,self).__init__(dstore,conf,section,**kwargs)
        self._wrftask=wrftask
        self.out_prefix=out_prefix
        self._initial=list()
        self._wrfprod=list()
        self._final=list()
        self._ncks_path=False

    @property 
    def ncks_path(self):
        """Returns the path to ncks.  Returns None if ncks cannot be
        found.  This function will only search for ncks once, and will
        cache the result.  Set self._ncks_path=False to force a
        recheck."""
        if self._ncks_path is False:
            ncks=self.getexe('ncks','')
            if not self._ncks_path:
                ncks=produtil.fileop.find_exe('ncks',raise_missing=False)
            assert(ncks is None or 
                   (isinstance(ncks,basestring) and ncks!=''))
            self._ncks_path=ncks
        return self._ncks_path

    def compression_copier(self,src,vsubset=None):
        """Returns the object that should be sent as the "copier"
        argument to deliver_file to copy the given source file. This
        is either None, or a function that calls ncks to compress
        NetCDF files.  If a vsubset argument is present, the file is
        subsetted, retaining only the variables vsubset (a comma
        separated list)."""
        ncks=self.ncks_path
        if ncks is None:
            return None # don't have ncks, so cannot deliver

        if produtil.fileop.netcdfver(src) is None: 
            return None # is not NetCDF, so just copy the file bit-by-bit

        # Source file IS NetCDF, or possibly non-NetCDF HDF5, but
        # we'll overlook that.  Use ncks to compress, raise an
        # exception if ncks returns non-zero.
        logger=self.log()
        def copy(s,t,x):
            produtil.fileop.remove_file(t,logger=logger)
            checkrun(bigexe(ncks)['-4','-L','6',s,t]<'/dev/null',
                     logger=logger)
        return copy

    def decompression_copier(self,src):
        """Returns an object that has the reverse effect of
        self.copier.  This will uncompress files that copier would
        compress.  NetCDF files will all be converted to 64-bit
        indexing NetCDF 3 files."""
        ncks=self.ncks_path
        if ncks is None:
            return None # don't have ncks, so cannot deliver

        if produtil.fileop.netcdfver(src) is None: 
            return None # is not NetCDF, so just copy the file bit-by-bit

        # Source file IS NetCDF, or possibly non-NetCDF HDF5, but
        # we'll overlook that.  Use ncks to decompress, raise an
        # exception if ncks returns non-zero.
        logger=self.log()
        def copy(s,t,x):
            produtil.fileop.remove_file(t,logger=logger)
            checkrun(bigexe(ncks)['-6',s,t]<'/dev/null',logger=logger)
        return copy

    def comfile(self,orig,destname=None):
        """Generates a full path to the delivery location of the
        specified source file.  Returns the full path and the basename
        in a tuple."""
        if(isinstance(orig,Product)):
            bn=os.path.basename(str(orig.location))
        else:
            bn=os.path.basename(str(orig))
        # The bn_colon is the same as bn, but uses : to separate time
        # components, - to separate date components and an _ between
        # the date and time.  This matches the syntax expected by most
        # programs external to this workflow.
        bn_colon=re.sub(
            '([0-9][0-9][0-9][0-9])[_.-]([0-9][0-9])[_.-]([0-9][0-9])'
            '[_.-]([0-9][0-9])[_.-]([0-9][0-9])[_.-]([0-9][0-9])[^/]*$',
            r'\1-\2-\3_\4:\5:\6',bn)
        bn_colon_s00=re.sub(
            '([0-9][0-9][0-9][0-9])[_.-]([0-9][0-9])[_.-]([0-9][0-9])'
            '[_.-]([0-9][0-9])[_.-]([0-9][0-9])[_.-]([0-9][0-9])[^/]*$',
            r'\1-\2-\3_\4:\5:00',bn)
        if destname is None:
            fullbn='%s.%s'%(self.out_prefix,bn_colon)
        else:
            fullbn=self.confstrinterp(destname,inname=bn,
                                      inname_colon=bn_colon,
                                      inname_colon_s00=bn_colon_s00)
        return ( os.path.join(self.outdir,fullbn), fullbn )

    def d_initial(self,inprod,check=None,destname=None):
        """Requests delivery of a file that is created before the
        wrf.exe invocation.  The "inprod" may be a Product or a
        filename relative to the WRF run directory.  The optional
        "check" argument enables calling the potentially expensive
        "check" subroutine on the upstream product every time it is
        considered for delivery.  If the input is a Product, and
        check=False, then only the "available" subroutine is called,
        which will not be updated unless another Task marks the
        product as available.  The default is check=False for Products
        and check=True for filenames."""
        return self._deliver_to_group(self._initial,inprod,check,destname)

    def d_final(self,inprod,check=None,destname=None):
        """Requests delivery of a file created by WRF that is not
        complete until the WRF exits.  Examples of this are the
        wrfdiag, hifreq and patcf files.  These files will be
        delivered when the underlying WRF Task has a state of
        COMPLETED.  The optional "check" argument enables calling the
        potentially expensive "check" subroutine on the upstream
        product every time it is considered for delivery.  If the
        input is a Product, and check=False, then only the "available"
        subroutine is called, which will not be updated unless another
        Task marks the product as available.  The default is
        check=False for Products and check=True for filenames."""
        return self._deliver_to_group(self._final,inprod,check,destname)

    def _deliver_to_group(self,group,inprod,check=None,destname=None):
        """Do not call this function directly.  It is the internal
        implementation of d_initial and d_final.  Call those functions
        instead."""
        (comfile,combn)=self.comfile(inprod,destname=destname)
        if(isinstance(inprod,Product)):
            upstream=inprod
            if check is None: check=False
        else:
            # Make an internal UpstreamFile to check for the file:
            wrffile=os.path.join(self._wrftask.location,inprod)
            upstream=UpstreamFile(dstore=self.dstore,prodname=combn,
                category="%s-upstream"%(self.taskname,),location=wrffile)
            upstream.location=wrffile
            # Nobody else can see this Product so we must check it:
            check=True
        product=FileProduct(
            dstore=self.dstore,prodname=combn,category=self.taskname,
            location=comfile)
        group.append( (upstream,product,bool(check)) )
        return self
    def d_wrfprod(self,product,check=False,destname=None):
        """Requests delivery of a WRF I/O subsystem output file.  The
        "product" argument must be a Product object.  The optional
        argument "check" enables calling the potentially expensive
        "check" subroutine on the product every time it is considered
        for delivery.  If check=False, then only the "available"
        subroutine is called, which will not be updated unless another
        Task marks the product as available."""
        (comfile,combn)=self.comfile(product,destname=destname)
        outproduct=FileProduct(dstore=self.dstore,prodname=combn,
                               category=self.taskname,location=comfile)
        outproduct.location=comfile
        self._wrfprod.append( (product,outproduct,bool(check)) )
        return self

    def run(self,check_all=False):
        """Keeps watching for WRF files to show up, copying them when
        they do.  This is just a simple wrapper around self.runpart,
        and does not return until runpart sets the state to something
        other than RUNNING."""
        self.state=RUNNING
        logger=self.log()
        while self.state==RUNNING:
            if self.run_helper(False,check_all):
                logger.info('Sleep 5...')
                time.sleep(5)
                logger.info('       ...done sleeping.')

    def deliver_group(self,group,check_all=False):
        """Takes a list of tuples containing an upstream product, a
        downstream product to deliver, and a boolean telling whether
        to check() the upstream product.  Delivers available products.
        Returns a tuple containing two booleans: the first is True iff
        something was delivered, and the second is true iff something
        is left in the group that has not been delivered yet.  The
        check_all argument can be used to force a check on all
        products by setting check_all=True."""
        did_something=False
        more_to_do=False
        logger=self.log()
        lockdir=os.path.join(self.getdir('lockdir'),self.taskname)
        for inprod,outprod,check in group:
            logger.debug(
                'COPYWRF ITEM: inprod=%s outprod=%s check=%s check_all=%s'%(
                    repr(inprod),repr(outprod),repr(check),repr(check_all)))
            messagemore=''
            if not check and not check_all:
                messagemore=' or post has not posted it yet'
            try:
                if not outprod.available:
                    available=inprod.available
                    if not available and ( check or check_all ):
                        inprod.check()
                        available=inprod.available
                    if available:
                        logger.info('%s: delivering.'%(
                                str(outprod.location),))
                        lockfile=os.path.join(
                            lockdir,os.path.basename(inprod.location))
                        locker=produtil.locking.LockFile(
                            filename=lockfile,max_tries=1)
                        try:
                            with locker:
                                ifrom=inprod.location
                                copier=self.compression_copier(ifrom)
                                outprod.deliver(frominfo=ifrom,copier=copier)
                                did_something=True
                        except produtil.locking.LockHeld as lh:
                            logger.info(
                                ' Nope.  Another process is delivering this '
                                'file right now.  Moving on.')
                            more_to_do=True
                    else:
                        logger.info('%s: not yet available%s.'
                                    %(str(inprod.location),messagemore))
                        more_to_do=True
            except Exception as e:
                more_to_do=True
                logger.warning('%s: trouble delivering: %s\n'%(
                        repr(outprod.location),str(e)),exc_info=True)
        return ( did_something, more_to_do )
    def unrun(self):
        """Calls the undeliver function on all products, deleting them
        from the destination."""
        for inprod,outprod,check in self._initial:
            outprod.undeliver()
        for inprod,outprod,check in self._wrfprod:
            outprod.undeliver()
    def runpart(self,check_all=False):
        """Delivers one output file and returns.  Sets the state to
        COMPLETED if all files are delivered.  The optional check_all
        argument forces a call to check() on all undelivered products,
        even if those products are not checked by default."""
        self.run_helper(True,check_all)
    def run_helper(self,runpart,check_all=False):
        """This is the internal implementation of run and runpart.  It
        delivers files, and returns False if all files are delivered.
        If runpart=True, it will return immediately after delivering
        one file.  The optional check_all argument forces a call to
        check() on all undelivered products, even if those products
        are not checked by default."""
        self._wrftask.update_state()
        state=self._wrftask.state
        started = (state==RUNNING or state==COMPLETED)
        completed = ( state == COMPLETED )

        initial_complete=False # done copying wrf inputs
        parallel_complete=False # done copying wrf outputs
        logger=self.log()
        logger.info('wrf task state=%d so started=%s and completed=%s'
                    %(self._wrftask.state,repr(started),repr(completed)))
        more_init=False
        more_para=False
        more_final=False
        if started:
            (did_something,more_init)=self.deliver_group(
                self._initial,check_all)
            if did_something and runpart: 
                # This is runpart, and we just delivered some initial
                # products, so return.
                return True
        # Now deliver any parallel products:
        (did_something,more_para)=self.deliver_group(self._wrfprod,check_all)
        if did_something and runpart:
            return True
        # And the "final state" products:
        if completed:
            (did_something,more_final)=self.deliver_group(
                self._final,check_all)
            if did_something and runpart: 
                # This is runpart, and we just delivered some initial
                # products, so return.
                return True
        if not more_init and not more_para and not more_final:
            # Nothing remains to be delivered, so we're done.
            self.state=COMPLETED
            logger.info('nothing left to deliver')
            return False
        else:
            return True
