__all__ = ["GSIPost",'F_ORG','F_GES','F_ANL','F_ALL']

import os, glob, shutil
import produtil.fileop, produtil.run, produtil.cd
import hwrf.gsi, hwrf.hwrftask, hwrf.post, hwrf.exceptions

from produtil.run import run, checkrun, alias, mpi, mpirun, exe, bigexe
from produtil.cd import NamedDir
from produtil.fileop import deliver_file, make_symlink, remove_file
from hwrf.post import EGRIB1Product, check_post, link_post_fix

F_ORG=1
"""Flag to indicate the original, downscaled parent data is desired."""

F_GES=2
"""Flag to indicate the post-relocation data (first guess) is
desired."""

F_ANL=4
"""Flag to indicate the final WRF input (after GSI and merge) is
desired."""

F_ALL=F_ORG|F_GES|F_ANL
"""A combination of F_ORG, F_GES and F_ANL, to indicate all three
initialization steps are requested."""

class GSIPost(hwrf.hwrftask.HWRFTask):
    def __init__(self,dstore,conf,section,**kwargs):
        super(GSIPost,self).__init__(dstore,conf,section,**kwargs)
        self._domains=list()
        self._wrfout=dict()

        self._wrforg=dict()
        self._wrfges=dict()
        self._wrfanl=dict()
        self._org_products=dict()
        self._ges_products=dict()
        self._anl_products=dict()

    def add_case(self,domain,wrfout,org=None,ges=None,anl=None):
        """Adds another case to the list of those to post-process:
        domain = a WRFDomain to be processed
        wrfout = a wrfout file for internal use.  It will be copied
          and its fields will e replaced with the ges and/or analysis
          file as needed
        org = original down-scaled parent domain data
        ges = a first guess wrfinput or restart file, generally after
          vortex relocation
        anl = an analysis (GSI output) wrfinput or restart file
        Note that at least one of org, ges or anl must be given."""
        assert(not (org is None and ges is None and anl is None))
        assert(wrfout is not None)
        assert(domain is not None)

        self._domains.append(domain)
        #self._domsize[domain]=[int(mdeglat),int(mdeglon),int(mdegres)]
        self._wrfout[domain]=wrfout

        if org is not None:
            self._wrforg[domain]=org
            self._org_products[domain]=EGRIB1Product(
                self.dstore,category=self.taskname,
                prodname=domain.name+'_org_'+org.prodname)

        if ges is not None:
            self._wrfges[domain]=ges
            self._ges_products[domain]=EGRIB1Product(
                self.dstore,category=self.taskname,
                prodname=domain.name+'_ges_'+ges.prodname)

        if anl is not None:
            self._wrfanl[domain]=anl
            self._anl_products[domain]=EGRIB1Product(
                self.dstore,category=self.taskname,
                prodname=domain.name+'_anl_'+anl.prodname)

    def run(self):
        """Executes the GSI post for all cases on all domains."""
        logger=self.log()
        good=True
        for domain in self._domains:
            logger.info('%s: process this domain'%(str(domain),))
            wrfout=self._wrfout[domain]
            if domain in self._org_products:
                logger.info('%s: process org'%(str(domain),))
                self._process(self._wrforg[domain],
                    domain,'org',wrfout,self._org_products[domain])
            else:
                logger.info('%s: has no org data'%(str(domain),))

            if domain in self._ges_products:
                logger.info('%s: process ges'%(str(domain),))
                self._process(self._wrfges[domain],
                    domain,'ges',wrfout,self._ges_products[domain])
            else:
                logger.info('%s: has no ges data'%(str(domain),))
                
            if domain in self._anl_products:
                logger.info('%s: process anl'%(str(domain),))
                self._process(self._wrfanl[domain],
                    domain,'anl',wrfout,self._anl_products[domain])
            else:
                logger.info('%s: has no anl data'%(str(domain),))
        logger.info('Done processing domains.')
        
    def _process(self,inprod,domain,why,wrfout,outprod):
        """Do not call directly.  This is the implementation of
        self.run: it runs the post for one case, on one domain."""
        assert(inprod is not None)
        assert(outprod is not None)
        assert(wrfout is not None)
        assert(domain is not None)
        logger=self.log()
        #assert(outprod.location) # should already be set in constructor
        assert(wrfout.location)
        wrfthere=wrfout.location
        assert(wrfthere)
        if not produtil.fileop.isnonempty(wrfthere):
            raise hwrf.exceptions.PostHasNoInput(
                '%s: is empty or nonexistant'%(wrfthere,))
        if not inprod.location:
            logger.info('%s: not available (location unknown)'%(
                    inprod.did,))
        elif not inprod.available:
            logger.info('%s (%s): not available according to database'%(
                    inprod.did, inprod.location))
        shortname=domain.name+'_'+why+'_'+inprod.prodname
        workdir=os.path.join(self.workdir,shortname)
        if os.path.exists(workdir):
            logger.info('%s: exists; will delete'%(workdir,))
            shutil.rmtree(workdir)
        with NamedDir(workdir,keep=not self.scrub,logger=logger):
            diffwrf=alias(bigexe(self.getexe('hwrf_3dvar')))
            post=alias(mpi(self.getexe('post')))

            # Copy the fields into the wrfout file:
            deliver_file(wrfthere,'postinput',keep=True)
            make_symlink(inprod.location,'ghost',force=True)
            cmd=diffwrf['storm_relocate','ghost','flnm3','new_ght']
            checkrun(cmd >= 'storm_relocate.log',logger=logger)
            cmd=diffwrf['3dvar_update','postinput','new_ght']
            checkrun(cmd >= '3dvar_update.log',logger=logger)

            # Delete any stray fort.* files:
            for filename in glob.glob('fort.*'):
                produtil.fileop.remove_file(filename,logger=logger)

            # Run the post:
            datestamp=self.conf.cycle.strftime('%Y-%m-%d_%H:%M:%S')
            with open('itag','wt') as itag:
                itag.write("""postinput
netcdf
%s
NMM NEST
""" % (datestamp,))
            cmd=mpirun(post,allranks=True) >= 'vpost.log'
            needcrtm=self.confbool('needcrtm',False)
            logger.info('link post fix files')
            link_post_fix(self.getdir('FIXhwrf'),needcrtm,logger=logger)
            fort14=self.confstr('control')
            logger.info('%s: use this control file for gsi post'%(fort14,))
            produtil.fileop.make_symlink(fort14,'fort.14',force=True,
                                         logger=logger)
            logger.info('Run post, log to vpost.log.')
            ret=run(cmd,logger=logger)

            # Check to see if the post succeeded and get the center
            # lat & lon of the domain for post-processing:
            (ok,cenla,cenlo,filename)=check_post(ret,shortname,logger)
            if not ok:
                raise hwrf.exceptions.PostFailed('GSI post on '+shortname)
            #toloc=outprod.location
            toloc=os.path.join(self.outdir,shortname)
            outprod.deliver(toloc,{'CENLA':cenla,'CENLO':cenlo,
                                   'fromloc':filename},logger=logger)
        return True

    def products(self,domains=None,domain=None,which_step=F_ALL,**kwargs):
        """Iterates over EGRIB1Product objects produced by this task:
          domains = a list of WRFDomain objects.  Only these will be iterated.
          domain = a single WRFDomain; only its products will be iterated
          ges = iterate over first guess products?
          anl = iterate over analysis (GSI output) products?
        Note that a wrfinput file in this context is also an
        "analysis" file."""
        if domains is None and domain is not None:
            domains=[domain]
        elif domains is None:
            domains=self._domains
        for d in domains:
            if which_step&F_ORG and d in self._org_products:
                yield self._org_products[d]
            if which_step&F_ANL and d in self._anl_products:
                yield self._anl_products[d]
            if which_step&F_GES and d in self._ges_products:
                yield self._ges_products[d]


