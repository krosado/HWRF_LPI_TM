__all__=['HWRFGSIPostProcessing','HWRFForecastPostProcessing']

import os,sys

import produtil.datastore, produtil.run

import hwrf.config, hwrf.wrf, hwrf.post, hwrf.numerics, hwrf.launcher
import hwrf.regrib, hwrf.gribtask, hwrf.tracker, hwrf.storminfo
import hwrf.wps, hwrf.nhc_products, hwrf.copywrf, hwrf.fcsttask, hwrf.ensda
import hwrf.relocate, hwrf.init, hwrf.prep, hwrf.gsi, hwrf.mpipomtc
import hwrf.bufrprep, hwrf.hwrftask

from produtil.run import exe,alias
from hwrf.wrf import WRFDomain,WRFSimulation,ExternalWRFTask
from hwrf.post import PostManyWRF
from hwrf.regrib import RegribMany,igrb1,clatlon,GRIB2


def add_clatlon_grid(task,r,name,rel):
    contents=task.confstr(name+"_grid")
    split=contents.rsplit(",")
    assert(len(split)==7)
    res1=float(split[0])
    res2=float(split[1])
    size1=float(split[2])
    size2=float(split[3])
    scan=int(split[4])
    n1=int(split[5])
    n2=int(split[6])
    r.add(name,clatlon(rel,res=[res1,res2],size=[size1,size2],
                       scan=scan,n=[n1,n2]))

def add_expandlatlon_grid(task,r,name,rel):
    contents=task.confstr(name+"_expand")
    split=contents.rsplit(",")
    assert(len(split)==9)
    west=float(split[0])
    east=float(split[1])
    north=float(split[2])
    south=float(split[3])
    n1=int(split[4])
    n2=int(split[5])
    scan=int(split[6])
    res1=float(split[7])
    res2=float(split[8])
    r.add(name,hwrf.tracker.expandlatlon \
              (r.GRIB(rel),west=west,east=east,north=north,south=south,
               n=[n1,n2],scan=scan,res=[res1,res2]))

def to_com_intercom(task,what,r):
    """Utility function for deliveries.  Given what="hwrftrk" this
    will look for option "hwrftrk%com" and "hwrftrk%intercom".  If
    the option is present and non-empty, the corresponding
    delivery is enabled and the option's value is used as the
    delivery location."""
    comvar=what+'%com'
    intercomvar=what+'%intercom'
    comloc=task.confraw(comvar,'')
    intercomloc=task.confraw(intercomvar,'')
    if comloc: 
        sys.stderr.write('to_com(%s,%s) (from %s)\n'%(
                repr(comloc),repr(what),repr(comvar)))
        r.to_com(comloc,what)
    if intercomloc: 
        sys.stderr.write('to_intercom(%s,%s) (from %s)\n'%(
                repr(intercomloc),repr(what),repr(intercomvar)))
        r.to_intercom(intercomloc,what)


class HWRFGSIPostProcessing(hwrf.hwrftask.HWRFTask):
    def __init__(self,ds,conf,section,**kwargs):
        super(HWRFGSIPostProcessing,self).__init__(
            ds,conf,section,**kwargs)
    def make_gsi_post(self,gsi_d02,gsi_d03,storm1ghost,storm1ghost_parent,
                      ceninit,gsid03_flag,gsipost_name='gsipost',
                      gsigribber_name='gsigribber'):
        ds=self.dstore
        conf=self.conf
        # Create the GSI post.  It will post-process the ghost
        # files from the init.wrfghost, relocation and GSI for
        # each of d02 and d03:
        gsipost = hwrf.gsipost.GSIPost(ds,conf,gsipost_name)
        if gsid03_flag:
            gsipost.add_case(
                storm1ghost,
                ceninit.runghost.get_ghout(storm1ghost),
                org=ceninit.runghost.get_ghost(storm1ghost),
                ges=ceninit.rstage3.get_ghost(storm1ghost),
                anl=gsi_d03.get_ghost(storm1ghost)  )
        gsipost.add_case(
            storm1ghost_parent,
            ceninit.runghost.get_ghout(storm1ghost_parent),
            org=ceninit.runghost.get_ghost(storm1ghost_parent),
            ges=ceninit.rstage3.get_ghost(storm1ghost_parent),
            anl=gsi_d02.get_ghost(storm1ghost_parent)  )
        
        g2p='%d'%(self.confint('grib2_compression',32),)
        g=RegribMany(
            copygb=alias(exe(conf.getexe('copygb'))),
            wgrib=alias(exe(conf.getexe('wgrib'))),
            cnvgrib_g12=alias(exe(conf.getexe('cnvgrib'))
                              ['-g12','-p'+g2p]))
        
        storm1ghost_grib=igrb1(
            gsipost,domain=storm1ghost,which_step=hwrf.gsipost.F_ORG)
        storm1ghost_parent_grib=igrb1(
            gsipost,domain=storm1ghost_parent,which_step=hwrf.gsipost.F_ORG)
        add_clatlon_grid(self,g,'d3',storm1ghost_grib)
        add_clatlon_grid(self,g,'d2',storm1ghost_parent_grib)

        g.add('hwrforg_n',igrb1(gsipost,domain=storm1ghost,
            which_step=hwrf.gsipost.F_ORG)*g.grid('d3')*GRIB2)
        g.add('hwrforg_i',igrb1(gsipost,domain=storm1ghost_parent,
            which_step=hwrf.gsipost.F_ORG)*g.grid('d2')*GRIB2)

        g.add('hwrfges_n',igrb1(gsipost,domain=storm1ghost,
            which_step=hwrf.gsipost.F_GES)*g.grid('d3')*GRIB2)
        g.add('hwrfges_i',igrb1(gsipost,domain=storm1ghost_parent,
            which_step=hwrf.gsipost.F_GES)*g.grid('d2')*GRIB2)

        g.add('hwrfanl_n',igrb1(gsipost,domain=storm1ghost,
            which_step=hwrf.gsipost.F_ANL)*g.grid('d3')*GRIB2)
        g.add('hwrfanl_i',igrb1(gsipost,domain=storm1ghost_parent,
            which_step=hwrf.gsipost.F_ANL)*g.grid('d2')*GRIB2)

        for w in ( 'org', 'ges', 'anl' ):
            for gr in 'in':
                base='hwrf%s_%s'%(w,gr)
                to_com_intercom(self,base,g)
        
        gsigribber=hwrf.gribtask.GRIBTask(
            ds,conf,gsigribber_name,g,start=conf.cycle,step=3600,end=0)
        self.gsipost=gsipost
        self.gsigribber=gsigribber
        return (gsipost,gsigribber)

class HWRFForecastPostProcessing(hwrf.hwrftask.HWRFTask):
    """This class encapsulates the object instantiations for the HWRF
    forecast post-processing in a single class.  The purpose is to
    simplify the hwrf_expt."""
    def __init__(self,ds,conf,section,runwrf,wrf,postdoms,wrfdoms,
                 moad,storm1inner,storm1outer,**kwargs):
        super(HWRFForecastPostProcessing,self).__init__(
            ds,conf,section,**kwargs)
        self.runwrf=runwrf
        self.wrf=wrf
        self.postdoms=postdoms
        self.wrfdoms=wrfdoms
        self.moad=moad
        self.storm1inner=storm1inner
        self.storm1outer=storm1outer

    def make_nonsatpost(self,default_step=1.):
        step=self.conffloat('nonsatpost_step',default_step)*3600
        self.nonsatpost = PostManyWRF(
            self.runwrf,self.postdoms,self.conf,'nonsatpost',step,
            needcrtm=False,streams=['history','auxhist2','auxhist3'])
        return self.nonsatpost

    def make_satpost(self,default_step=6):
        # Post-processors:
        step=self.conffloat('satpost_step',default_step)*3600
        self.satpost = PostManyWRF(
            self.runwrf,self.postdoms,self.conf,'satpost',step,
            needcrtm=True,streams=['history','auxhist2','auxhist3'])
        return self.satpost

    def make_wrfcopier(self,wrfcopier_name='copywrf'):
        """Generates the WRF input/output copier task."""
        copystart=self.conffloat('wrfcopier_start',0)
        copyend=self.conffloat('wrfcopier_end',9)
        copystep=self.conffloat('wrfcopier_step',3)
        assert(copystep>0)

        runwrf=self.runwrf
        wrf=self.wrf
        wrfcopier = hwrf.copywrf.WRFCopyTask(
            self.dstore,self.conf,wrfcopier_name,runwrf,
            self.conf.getstr('config','out_prefix'))
        wrfcopier.d_initial('namelist.input')
        wrfcopier.d_initial('wrfinput_d01')
        wrfcopier.d_initial('wrfbdy_d01')
        wrfcopier.d_initial('fort.65')
        wrfcopier.d_initial(wrf.analysis_name(self.storm1outer),
                            destname=self.confraw('anl_outer'))
        wrfcopier.d_initial(wrf.analysis_name(self.storm1inner),
                            destname=self.confraw('anl_inner'))
        copied=False
        fhr=copystart
        while fhr-0.001<copyend:
            prodstream='history' if fhr % 3 == 0 else 'auxhist2'
            for product in runwrf.products(time=3600*fhr,stream=prodstream):
                wrfcopier.d_wrfprod(product,check=False,
                  destname='{vit[stnum]:02d}{vit[basin1lc]}.'
                                    '{inname_colon_s00}')
                copied=True
            fhr2=fhr+copystep
            assert(fhr2>fhr)
            fhr=fhr2
        assert(copied)
        wrfcopier.d_final('track_d03.patcf')
        self.wrfcopier=wrfcopier
        return wrfcopier

    def make_gribber_tracker(self,extra_trackers=False,satpost_flag=True):
        """Generate the regribbing objects and the main tracker."""
        # Aliases to simplify below code:
        (nonsatpost,satpost)=(self.nonsatpost,self.satpost)
        (moad,storm1inner,storm1outer) = \
            (self.moad,self.storm1inner,self.storm1outer)
        conf=self.conf
        fcstlen=self.confint('forecast_length',126)
        combinetrack_fhr=self.conffloat('combinetrack_fhr',12.)

        # Domain selection.  These objects will grab Post output for the
        # specified domain for any arbitrary time when inserted into a
        # RegribMany object:
        grid3=igrb1(nonsatpost,domain=storm1inner)  # 3km non-satellite post
        grid2=igrb1(nonsatpost,domain=storm1outer)  # 9km non-satellite post
        grid1=igrb1(nonsatpost,domain=moad)         # 27km non-satellite post
        satE3=igrb1(satpost,domain=storm1inner)     # 3km satellite post
        satE2=igrb1(satpost,domain=storm1outer)     # 9km satellite post
        satE1=igrb1(satpost,domain=moad)            # 27km satellite post
        
        # The hwrfsub, when used in a RegribMany, selects the
        # hwrfprs_c and hwrfsat_c GRIB1/2 subset:
        hwrfsub=hwrf.regrib.GRIBSubsetter(
            'part',hwrf.tracker.hwrf_combine_subset,None)

        # The ghwrfsub, when used in a RegribMany, selects the
        # hwrfprs_g and hwrfsat_g GRIB1/2 subset:
        ghwrfsub=hwrf.regrib.GRIBSubsetter(
            'part',hwrf.tracker.hwrf_global_subset,None)

        # In a RegribMany, trksub selects the subset needed by the tracker:
        trksub=hwrf.regrib.GRIBSubsetter(
            'part',hwrf.tracker.tracker_subset,None)

        # The domloc is the location of the HWRF domain center:
        domloc=hwrf.regrib.FixedLocation(lat=conf['config','domlat'],
                                         lon=conf['config','domlon'])
        basin=conf.syndat.pubbasin2
        if ((basin=='AL' or basin=='EP') and domloc.ewcenter<360.0):
            domloc.ewcenter+=360.0 # to match 2013 HWRF behavior
       
        # Create the RegribMany object.  This object describes the GRIB
        # processing to do after the post completes:
        g2p='%d'%(self.confint('grib2_compression',32),)
        r=RegribMany(copygb=alias(exe(conf.getexe('copygb'))),
                     wgrib=alias(exe(conf.getexe('wgrib'))),
                     cnvgrib_g12=alias(exe(conf.getexe('cnvgrib'))
                                       ['-g12','-p'+g2p]))
       
        # Define some grids that we'll reuse later:
        add_clatlon_grid(self,r,'d23',grid2)
        add_clatlon_grid(self,r,'d123low',domloc)
        add_clatlon_grid(self,r,'d123high',domloc)
        add_clatlon_grid(self,r,'d2',grid2)
        add_clatlon_grid(self,r,'d3',grid3)
        qd=hwrf.regrib.quarterDegree # GFS quarter degree grid (grid 193)

        if extra_trackers:
            # Grids for extra trackers:
            add_clatlon_grid(self,r,'d2t',grid2)
            add_clatlon_grid(self,r,'d1t',grid2)

        r.add('hwrfprs_m',grid2*r.grid('d23')+grid3*r.grid('d23'))
        r.add('parenthigh',grid1/hwrfsub*r.grid('d123high'))

        # Regular three domain tracker input:
        add_expandlatlon_grid(self,r,'trk','hwrfprs_m')
        r.add('trkbasic',(r.GRIB('parenthigh')/trksub) \
                  .regridmerge(r.grid('trk'),r.GRIB('hwrfprs_m')/trksub))
        r.add('hwrftrk',hwrf.tracker.vinttave(r.GRIB('trkbasic')))

        r.add('hwrfprs_n',grid3*r.grid('d3'))
        r.add('hwrfprs_i',grid2*r.grid('d2'))
        r.add('hwrfprs_p',grid1*r.grid('d123low'))
        r.add('hwrfprs_c',grid1/hwrfsub*r.grid('d123high') +
              (grid2/hwrfsub*r.grid('d123high') +
               grid3/hwrfsub*r.grid('d123high')))
        r.add('hwrfprs_g',grid1/ghwrfsub*qd +
              (grid2/ghwrfsub*qd + grid3/ghwrfsub*qd))
        
        # Extra input for d02 and d01 trackers:
        if extra_trackers:
            r.add('hwrftrkd01',((grid1/trksub)*r.grid('d1t')).make_grbindex())
            r.add('hwrftrkd02',((grid1/trksub)*r.grid('d2t')
                  +(grid2/trksub)*r.grid('d2t')).make_grbindex())
            #r.add('hwrftrk27km',hwrf.tracker.vinttave(r.GRIB('trk27km')))
            #r.add('hwrftrk9km',hwrf.tracker.vinttave(r.GRIB('trk9km')))
        
        if satpost_flag:
            r.add('hwrfsat_m',satE2*r.grid('d23')+satE3*r.grid('d23'))
            r.add('hwrfsat_n',satE3*r.grid('d3'))
            r.add('hwrfsat_i',satE2*r.grid('d2'))
            r.add('hwrfsat_p',satE1*r.grid('d123low'))
            r.add('hwrfsat_c',satE1/hwrfsub*r.grid('d123high') +
                  satE2/hwrfsub*r.grid('d123high')) # sat_c only includes
                                                    # d01 and d02
            r.add('hwrfsat_g',satE1/hwrfsub*qd +
                  (satE2/hwrfsub*qd + satE3/hwrfsub*qd))

        # Add GRIB2 converted versions of all GRIB1 products.  GRIB2,
        # unlike GRIB1, delivers the massive hwrfprs_m file.
        if satpost_flag:
            do_me=('prs','sat')
        else:
            do_me=('prs',)
        for T in do_me:
            for D in 'mnipcg': # iterates over those five letters
                inx='hwrf%s_%s'%(T,D)    # looks like: hwrfprs_m
                outx='hwrf2%s_%s'%(T,D)  # looks like: hwrf2prs_m
                r.add(outx,r.GRIB(inx)*GRIB2)

        splist=[ 'prs' ]
        if satpost_flag: splist.append('sat')
        for v in ( '', '2' ):
            for sp in splist:
                for g in 'gmnipc':
                    base='hwrf'+v+sp+'_'+g  # hwrf2prs_g
                    to_com_intercom(self,base,r)
        to_com_intercom(self,'hwrftrk',r)
                    
        # Create the GRIBTask which will actually do the re-gribbing:
        gribber=hwrf.gribtask.GRIBTask(
            self.dstore,conf,'regribber',r,start=conf.cycle,
            step=3600,end=fcstlen*3600)

        # Run the GFDL vortex tracker in multi-file moveable grid
        # mode off of the hwrftrk files from gribber:
        tracker=hwrf.tracker.TrackerTask(self.dstore,conf,'tracker',
            start=conf.cycle, step=self.conffloat('tracker_step',1)*3600,
                                         end=fcstlen*3600)
        tracker.add_moving_grid(conf.syndat,gribber,'hwrftrk')
        tracker.send_raw_atcfunix('rawatcfunix',
            conf.strinterp('dir','{com}/{out_prefix}.trak.hwrf.raw'))
        track=tracker.send_atcfunix('cleanatcfunix',
            conf.strinterp('dir','{com}/{out_prefix}.trak.hwrf.atcfunix'))
        tracker.send_atcfunix_subset('atcf3hourly',freq=3,
            location=conf.strinterp('dir','{com}/{out_prefix}.trak.hwrf.3hourly'))
        tracker.send_atcfunix_subset('atcfshort6hr',freq=6,cut=112,
            location=conf.strinterp('dir','{com}/{out_prefix}.trak.hwrf.short6hr'))
        tracker.send_atcfunix_subset('combinetrack',fhr=12,
            location=conf.strinterp(
                'dir','{com}/{vit[stnum]:02d}{basin1lc}.trak.hwrf.'
                'atcfunix.{YMDH}.combine'))

        # Run the hwrf_nhc_products program to generate the swath,
        # afos file and other custom outputs for NHC:
        nhcp=hwrf.nhc_products.NHCProducts(
            self.dstore,conf,'nhc_products',self.runwrf,track,
            self.wrfdoms,self.conf.syndat)

        self.nhcp=nhcp
        self.track=track
        self.tracker=tracker
        self.gribber=gribber
        return (gribber,tracker,track,nhcp)

    def make_extra_trackers(self):
        ds=self.dstore
        conf=self.conf
        gribber=self.gribber
        fcstlen=conf.getint('config','forecast_length',126)
        tracker_step=self.conffloat('tracker_step')*3600

        # d01+d02 tracker:
        trackerd02=hwrf.tracker.TrackerTask(ds,conf,'trackerd02',
            start=conf.cycle, step=tracker_step, end=fcstlen*3600)
        trackerd02.add_moving_grid(conf.syndat,gribber,'hwrftrkd02')
        trackerd02.send_raw_atcfunix('rawatcfunix',
            conf.strinterp('dir','{com}/{out_prefix}.trak.hwrfd02.raw'))
        trackerd02.send_atcfunix('cleanatcfunix',
            conf.strinterp('dir','{com}/{out_prefix}.trak.hwrfd02.atcfunix'))
    
        # d01 tracker:
        trackerd01=hwrf.tracker.TrackerTask(ds,conf,'trackerd01',
            start=conf.cycle, step=tracker_step, end=fcstlen*3600)
        trackerd01.add_moving_grid(conf.syndat,gribber,'hwrftrkd01')
        trackerd01.send_raw_atcfunix('rawatcfunix',
            conf.strinterp('dir','{com}/{out_prefix}.trak.hwrfd01.raw'))
        trackerd01.send_atcfunix('cleanatcfunix',
            conf.strinterp('dir','{com}/{out_prefix}.trak.hwrfd01.atcfunix'))
        self.trackerd02=trackerd02
        self.trackerd01=trackerd01
        return (trackerd01,trackerd02)
