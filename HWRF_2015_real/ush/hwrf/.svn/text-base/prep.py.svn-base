__all__=['PrepHybrid']

import os
import produtil.datastore, produtil.cd, produtil.fileop, produtil.prog
import hwrf.numerics, hwrf.hwrftask, hwrf.wrf, hwrf.namelist, hwrf.exceptions
import hwrf.input
import produtil.rusage

from produtil.rusage import setrlimit, rusage, getrlimit
from hwrf.exceptions import NoGeogData
from hwrf.numerics import to_datetime, to_datetime_rel, to_timedelta, \
    to_fraction, timedelta_epsilon, TimeMapping, partial_ordering, \
    to_datetime_rel
from produtil.fileop import deliver_file, make_symlink, fortlink, realcwd, \
    wait_for_files
from produtil.run import alias, bigexe, checkrun, openmp
from produtil.datastore import FileProduct

class PrepHybrid(hwrf.hwrftask.HWRFTask):
    def __init__(self,dstore,conf,section,wrf,geogdata,atime=None,
                 ftime=None,taskname=None,times=None,inputs=None,
                 in_item=None,in_dataset=None,one_time=False,
                 in_anl_item=None,**kwargs):
        if taskname is None: taskname=section

        super(PrepHybrid,self).__init__(dstore,conf,section,taskname,**kwargs)

        if times is None:
            self.times=[ x for x in wrf.bdytimes() ]
        else:
            self.times=sorted(times) 

        if inputs is None:
            hd=self.confstr('catalog','hwrfdata')
            inputs=hwrf.input.DataCatalog(self.conf,hd,wrf.simstart())
        self._epsilon=timedelta_epsilon(
            self.times,default=to_fraction(wrf.bdystep())/10)
        if in_dataset is None:     in_dataset=self.confstr('dataset')
        self.in_dataset=in_dataset
        if in_item is None:        in_item=self.confstr('item')
        if in_anl_item is None:
            in_anl_item=self.confstr('anl_item','')
        if in_anl_item=='':        
            in_anl_item=in_item
        self.in_item=in_item
        self.in_anl_item=in_anl_item
        if atime is None: atime=wrf.simstart()
        if ftime is None: ftime=atime
        self.one_time=one_time
        self.in_atime=to_datetime(atime)
        self.in_ftime=to_datetime_rel(ftime,atime)
        self.geogdata=geogdata
        self.inputs=inputs
        self.bdyprod=TimeMapping(self.times)
        self.logprod=TimeMapping(self.times)
        self.initprod=None
        self.wrf=wrf

        self.make_products()
        self.nl=hwrf.namelist.Conf2Namelist(conf, self.confstr('namelist'))
        self.init_namelist()

        self._prep=None
    def inputiter(self):
        atime=self.in_atime
        for t in self.times:
            if hwrf.numerics.within_dt_epsilon(t,atime,5):
                in_item=self.in_anl_item
            else:
                in_item=self.in_item
            yield dict(self.taskvars,dataset=self.in_dataset,
                       item=in_item,ftime=t,atime=atime)
            if self.one_time: break
    def input_at(self,when):
        logger=self.log()
        ftime=to_datetime_rel(self.in_ftime,self.in_atime)
        if self.one_time:
            when=ftime
        else:
            when=to_datetime_rel(when,ftime)
        atime=self.in_atime
        if hwrf.numerics.within_dt_epsilon(when,atime,5):
            logger.info('Within dt epsilon: %s %s'%(when,atime))
            in_item=self.in_anl_item
        else:
            logger.info('NOT within dt epsilon: %s %s'%(when,atime))
            in_item=self.in_item

        self.log().info(
            "Check for dataset=%s item=%s ftime=%s atime=%s in %s"
            %(str(self.in_dataset), str(in_item), 
              when.strftime("%Y%m%d%H"),
              atime.strftime("%Y%m%d%H"), repr(self.inputs) ))
        return self.inputs.locate(self.in_dataset,in_item,ftime=when,
                                  atime=self.in_atime,**self.taskvars)
    def io_form_geogrid(self):
        """Guesses the WRF I/O form that was used to run geogrid"""
        return self.wrf.io_form_for('auxinput2')
    def init_namelist(self):
        # Alias some functions for convenience:
        siu=self.nl.nl_set_if_unset  # set my namelist var if not yet set
        wg=self.wrf.nl.nl_get        # get WRF namelist variable
        wtg=self.wrf.nl.trait_get    # get WRF trait
        # Set or override several parameters:
        siu('prmfld','ntimes',1)
        siu('domain','levels',wg('domains','eta_levels'))
        siu('domain','ptsgm',float(wg('domains','ptsgm')))
        siu('domain','p_top_requested',float(wtg('ptop'))/100.0) # Pa->hPa
    def make_products(self):
        first=True
        outdir=self.outdir
        ds=self.dstore
        cat=self.outdir
        self.initprod=FileProduct(ds,'hwrfinit_0',cat,
                                  location=os.path.join(outdir,'hwrfinit_0'))
        for itime in xrange(len(self.times)):
            time=self.times[itime]
            assert(time is not None)
            self.bdyprod[time]=FileProduct(ds,'hwrfbcs_%d'%(itime,),cat,
                    location=os.path.join(outdir,'hwrfbcs_%d'%(itime,)))
            self.logprod[time]=os.path.join(outdir,'prep_%d.log'%(itime,))
    def prep_init_file(self):
        return self.initprod
    def prep_bdy_files(self):
        for prod in self.bdyprod.itervalues():
            yield prod
    def products(self,time=None,name=None):
        if name is None or name=='init':
            dt=to_timedelta(abs(to_fraction(time-self.wrf.simstart())))
            if time is None or dt<self._epsilon:
                yield self.initprod
        if name is None or name=='bdy':
            bdys=self.bdyprod
            if time is None:
                for (time,prod) in bdys.iteritems(): yield prod
            else:
                when=bdys.neartime(time,self._epsilon)
                if when in bdys: yield bdys[when]
    def unrun(self):
        for p in self.products():
            p.available=0
    def clean(self):        self.rmtree(os.path.join(self.workdir))
    def run(self,keep=True):
        """Preps all times, even if they've already been prepped."""
        logger=self.log()
        self.postmsg('%s: prep is starting'%(self.taskname,))
        for now in self.times:
            logger.info('time %s: prep %s'%(now.strftime('%Y%m%d-%H%M%S'),
                                            self.input_at(now)))
        for ipiece in xrange(len(self.times)):
            self.run_ipiece(ipiece,keep=keep)
        self.postmsg('%s: prep is completed'%(self.taskname,))
    def runpart(self,keep=True):
        """Preps one time that has not yet been prepped, and returns."""
        logger=self.log()
        for ipiece in xrange(len(self.times)):
            now=self.times[ipiece]
            if not self.bdyprod[now].available or ipiece==0 and \
                    not self.initprod.available:
                logger.info(
                    'time %s (piece %d): not done; process this one'
                    %(now.strftime('%Y%m%d-%H%M%S'),ipiece))
                try:
                    self.run_ipiece(ipiece,keep=keep)
                    return
                except Exception as e:
                    logger.warning('time %s (piece %d): %s'%(
                            now.strftime('%Y%m%d-%H%M%S'),ipiece,str(e)),
                                   exc_info=True)
                    raise
            else:
                self.log().info('time %s (piece %d): already complete'
                                %(now.strftime('%Y%m%d-%H%M%S'),ipiece))
    def run_ipiece(self,ipiece,keep=True):
        """This is the internal helper function that runs the
        prep_hybrid for self.run and self.runpart."""
        logger=self.log() # avoid many function calls
        cenla=self.conf.getfloat('config','domlat')
        cenlo=self.conf.getfloat('config','domlon')
        now=self.times[ipiece]
        prepme=self.input_at(now)
        assert(isinstance(prepme,basestring))
        if self.realtime and not wait_for_files(
            prepme,logger,
            maxwait=self.confint('max_spectral_wait',1800),
            sleeptime=self.confint('spectral_sleep_time',20),
            min_size=self.confint('min_spectral_size',int(4e7)),
            min_mtime_age=self.confint('min_spectral_age',30),
            min_atime_age=None,
            min_ctime_age=None,
            min_fraction=1.0):
            msg='Some input spectral files do not exist.  Giving up.'
            logger.error(msg)
            raise hwrf.exceptions.NoSpectralData(msg)

        stime=now.strftime('%Y%m%d-%H%M%S')
        geoloc=self.geogdata.location
        if not self.geogdata.available:
            raise NoGeogData('%s product: WPS geogrid data is not yet '
                             'available. (loc=%s)'%(
                    str(self.geogdata.did), repr(self.geogdata.location)))
        moad=self.wrf.get_moad()
        # Decide where to run:
        there=self.conftimestrinterp('{workdir}/{fYMDH}',
                                     ftime=now,workdir=self.workdir)
        with produtil.cd.NamedDir(there,keep=keep,logger=logger):
            logger.info('%s: prep in directory %s',stime,realcwd())
            make_symlink(self.geogdata.location,'geogrid.out',
                         logger=logger,force=True)
            with open('dloc','wt')  as f:  f.write("%f\n%f\n"%(cenla,cenlo))
            with open('itime','wt') as f:  f.write("%d\n"%(ipiece,))
            bdyfile='../hwrfbcs00_%d'%(ipiece,)
            fortlink({52:bdyfile,
                      11:prepme,
                      44:'itime',
                      45:'dloc',           },logger=logger,force=True)
            if ipiece==0:
                initfile='../hwrfinit_%d'%(ipiece,)
                fortlink({53:initfile},logger=logger,force=True)
            with open('hwrf_na12_mkbnd.parm','wt') as f: 
                f.write(self.nl.make_namelist(section_sorter=partial_ordering(
                    ['rgrid','prmfld','domain'])))
            imax=self.confint('imax',1440)
            jmax=self.confint('jmax',721)
            iof=self.io_form_geogrid()%100
            if iof==11: iof=2
            ex=( bigexe(self.getexe('hwrf_prep')).env(
                    IO_FORM=iof,
                    OMP_STACKSIZE="128M",
                    MKL_NUM_THREADS='1'   )[
                    moad.nl.nl_get('domains','e_we'), 
                    moad.nl.nl_get('domains','e_sn'),
                    moad.nl.nl_get('domains','e_vert'),
                    moad.nl.nl_get('domains','dx'),
                    moad.nl.nl_get('domains','dy'),
                    imax, jmax]
                 < 'hwrf_na12_mkbnd.parm' ) >= 'prep.log'
            threads=self.confint('threads',0)
            # Try to change stack limit to 6GB:
            setrlimit(logger=logger,stack=6e9,ignore=True)
            # Print all resource limits:
            getrlimit(logger=logger)
            # AND collect resource usage during prep:
            with rusage(logger=logger):
                if threads<1:
                    logger.info('Use automatic thread count.')
                    checkrun(openmp(ex),logger=logger)  #use default threads
                else:
                    logger.info('Use %d threads'%(threads,))
                    checkrun(openmp(ex,threads=threads),logger=logger)
            if ipiece==0:
                produtil.fileop.makedirs(os.path.dirname(
                        self.initprod.location))
                self.initprod.deliver(
                    frominfo=initfile,logger=logger,keep=False)
            self.bdyprod[now].deliver(
                frominfo=bdyfile,logger=logger,keep=False)
            deliver_file('prep.log',self.logprod[now],logger=logger,
                         keep=False)
