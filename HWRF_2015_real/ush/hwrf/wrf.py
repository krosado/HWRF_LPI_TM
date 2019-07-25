
import fractions,math,re,datetime,os
import produtil.fileop

from produtil.datastore import COMPLETED,UpstreamFile,UNSTARTED,RUNNING,FAILED
from produtil.fileop import isnonempty
from produtil.run import bigexe, checkrun, mpirun, mpi, runstr

from hwrf.hwrftask import HWRFTask
from hwrf.numerics import *
from hwrf.namelist import *
from hwrf.exceptions import *
from hwrf.wrfbase import *

__all__=['default_wrf_outname','WRFDomain','WRFSimulation','ExternalWRFTask']

########################################################################

def default_wrf_outname(stream):
    """Generate a reasonable default wrf outname input value for the
    specified stream.  Presently, these match the WRF defaults.  These
    do not have to match the WRF defaults since we always specify the
    outname for all streams."""
    if stream=='history':
        return 'wrfout_d<domain>_<date>'
    elif stream=='anl':
        return 'wrf%s_d<domain>_<date>' % (stream,)
    elif stream=='restart':
        return 'wrfrst_d<domain>_<date>'
    elif stream=='bdy' or stream=='input':
        return 'wrf%s_d<domain>' % (stream,)
    elif stream=='geo_nmm':
        return '%s_d<domain>' % (stream,)
    elif stream=='inputout':
        return 'wrfinput_d<domain>_<date>'
    elif stream=='bdyout':
        return 'wrfbdy_d<domain>'
    else:
        return '%s_d<domain>_<date>'%(stream,)

########################################################################
# Default ordering of WRF namelist sections and namelist variables
# when calling WRF.namelist.  Note that this can be overridden if
# needed.  The defaults are set to produce the same order as the
# pre-python EMC HWRF.

_wrf_namelist_order=partial_ordering(['time_control','fdda','domains','physics','dynamics','bdy_control','namelist_quilt','logging'],6)
_wrf_nl_var_order={
    'time_control': partial_ordering([
        'start_year','start_month','start_day','start_hour','start_minute',
        'start_second','end_year','end_month','end_day','end_hour',
        'end_minute','end_second','interval_seconds','history_interval',
        'auxhist1_interval','auxhist2_interval',
        'auxhist3_interval','history_end','auxhist2_end','auxhist1_outname',
        'auxhist2_outname','auxhist3_outname','frames_per_outfile',
        'frames_per_auxhist1','frames_per_auxhist2','frames_per_auxhist3',
        'analysis','anl_outname','restart','restart_interval',
        'reset_simulation_start','io_form_input','io_form_history',
        'io_form_restart','io_form_boundary','io_form_auxinput1',
        'io_form_auxhist1','io_form_auxhist2','io_form_auxhist3',
        'auxinput1_inname','debug_level','tg_reset_stream',
        'override_restart_timers']),
    'domains': partial_ordering([
            'time_step','time_step_fract_num','time_step_fract_den',
            'max_dom','s_we','e_we','s_sn','e_sn','s_vert','e_vert','dx',
            'dy','grid_id','tile_sz_x','tile_sz_y','numtiles','nproc_x',
            'nproc_y','parent_id','parent_grid_ratio',
            'parent_time_step_ratio','i_parent_start','j_parent_start',
            'feedback','num_moves','num_metgrid_levels','p_top_requested',
            'ptsgm','eta_levels','use_prep_hybrid',
            'num_metgrid_soil_levels']),
    'physics': partial_ordering([
            'num_soil_layers','mp_physics','ra_lw_physics','ra_sw_physics',
            'sf_sfclay_physics','sf_surface_physics','bl_pbl_physics',
            'cu_physics','mommix','var_ric','coef_ric_l','coef_ric_s',
            'h_diff','gwd_opt',
            'sfenth','nrads','nradl','nphs','ncnvc','movemin','gfs_alpha',
            'sas_pgcon','sas_mass_flux','co2tf','vortex_tracker',
            'nomove_freq','tg_option','ntornado']),
    'dynamics': partial_ordering(
        ['non_hydrostatic','euler_adv','wp','coac','codamp',
         'terrain_smoothing']),
    'bdy_control': partial_ordering(['spec_bdy_width','specified']),
    'namelist_quilt': partial_ordering(
        ['poll_servers','nio_tasks_per_group','nio_groups']),
    'logging': partial_ordering(
        ['compute_slaves_silent','io_servers_silent','stderr_logging'])
    }

########################################################################

class WRFDomain(WRFDomainBase):
    def __init__(self,conf,section,name=None,copy=None):
        """Creates a new WRFDomain based on the information in a
        section of the HWRFConfig object conf.  The domain's name is
        in "name."  The "copy" argument should never be specified: it
        is used by self.copy for deep copies of a WRFDomain."""
        self.nestlevel=None
        self.parent=None
        self.nocolons=True
        self._start=None
        self._end=None
        self._dt=None
        self._output={}
        self.dx=None
        self.dy=None
        if copy is not None:
            self.nl=copy.nl.copy()
            self.name=str(copy.name)
            (  self._start,self._end,self._dt,self.nestlevel,self.parent ) =\
              (copy._start,copy._end,copy._dt,copy.nestlevel,copy.parent )
            (  self.dx,self.dy,self.nocolons) = \
              (copy.dx,copy.dy,copy.nocolons)
            for (n,o) in copy._output.iteritems():
                self._output[n]=dict(o)
        else:
            self.nl=Conf2Namelist(conf,section)
            self.name=str(section)
    def moad_ratio(self):
        """Returns the nesting ratio between this domain and the MOAD."""
        if self.parent is None:
            return 1
        else:
            return self.parent.moad_ratio()*self.nl.nl_get(
                'domains','parent_grid_ratio')
    def getdt(self): 
        """Returns the timestep for this domain."""
        return self._dt
    dt=property(getdt,None,None,'The timestep for this domain.')
    def __repr__(self): return '<WRFDomain name=%s>'%(str(self.name),)
    def _nl_subsetter(self,s,v):
        """Returns True.  This will be used in the future to ensure
        users do not specify namelist options that should be
        automatically configured."""
	#if s=='time_control':
        #    return not re.search('\A(?:start_.*|end_.*|.*_interval|frames_per_.*|analysis|restart.*|io_form.*|reset_simulation_start|.*_inname|override_restart_timers|history_outname|aux.*outname)\Z',s)
        #elif s=='domains':
        #    return not re.search('\A(?:time_step.*|[se]_.*|dx|dy|grid_id|parent_.*|num_moves|numtiles|tile_sz_x|tile_sz_y)\Z',s)
        return True
    def copy(self):
        """Returns a copy of this object.  The copy has its own data
        structures, so modifying the copy will not modify the
        original."""
        return WRFDomain(None,None,copy=self)
    def set_timing(self,start,end,timestep):
        """Sets this WRFDomain's idea of the simulation start and end
        times and timestep."""
        WRFDomainBase.set_timing(self,start,end,timestep)
        (ts,te,dt)=self._validate_timespan(start,end,timestep)
        # Set the start/end:
        for n in ('year','month','day','hour','minute','second'):
            self.nl.nl_set('time_control','start_'+n,getattr(ts,n))
            self.nl.nl_set('time_control','end_'+n,getattr(te,n))
    def get_grid_id(self):
        """Returns the WRF grid id."""
        return self.nl.nl_get('domains','grid_id')
    def is_moad(self):
        """Returns True if this is the WRF Mother of All Domains
        (MOAD) and False otherwise.  The MOAD is the outermost
        domain."""
        gid=self.get_grid_id()
        gid=int(gid)
        return gid==1
    @property
    def nx(self):
        """The number of gridcells the X direction."""
        return self.nl.nl_get('domains','e_we')
    @property
    def ny(self):
        """The number of gridcells the Y direction."""
        return self.nl.nl_get('domains','e_sn')
    @property
    def nz(self):
        """The number of gridcells the Z direction."""
        return self.nl.nl_get('domains','e_vert')
    def init_domain(self,grid_id):
        """Initializes this domain's variables that are unrelated to
        nesting."""
        s=self.nl.nl_set
        t=self.nl.trait_get
        s('domains','s_we',1)
        s('domains','s_sn',1)
        s('domains','s_vert',1)
        s('domains','e_we',t('nx'))
        s('domains','e_sn',t('ny'))
        s('domains','grid_id',int(grid_id))
    def init_as_moad(self,simstart,simend,simdt,eta_levels):
        """Initializes this domain's variables so it knows to act as
        the Mother Of All Domains (MOAD)"""
        WRFDomainBase.init_as_moad(self,simstart,simend,simdt,eta_levels)
        s=self.nl.nl_set
        t=self.nl.trait_get
        (i,n,d)=split_fraction(simdt)
        nz=t('nz',len(eta_levels))
        if len(eta_levels)!=nz:
            raise WRFError('nz and len(eta_levels) mismatch: %d vs %d'
                           %(int(nz),len(eta_levels)))
        s('domains','e_vert',nz)
        self.dx=t('dx')
        s('domains','dx',self.dx)
        self.dy=t('dy')
        s('domains','dy',self.dy)
        s('domains','parent_id',0)
        s('domains','parent_grid_ratio',1)
        s('domains','parent_time_step_ratio',1)
        s('domains','i_parent_start',0)
        s('domains','j_parent_start',0)
        assert(self.dy is not None)
    def init_as_nest(self,parent,grid_id,start,end):
        """Initializes this domain's variables to make it a nest
        within the given parent WRFDomain"""
        WRFDomainBase.init_as_nest(self,parent,grid_id,start,end)
        if not is_at_timestep(parent._start,start,parent._dt):
            raise StartNotAtParentTimestep(
                'Start time %s for domain %d is not at parent %d timestep.'
                %(parent._start,grid_id,parent.grid_id))
        s=self.nl.nl_set
        t=self.nl.trait_get
        p=parent.nl.nl_get
        # for n in [ 'restart_begin', 'restart_begin_m',
        #            'restart_begin_s', 'restart_begin_h',
        #            'restart_begin_d', 'restart_interval',
        #            'restart_interval_s', 'restart_interval_m',
        #            'restart_interval_h', 'restart_interval_d' ]:
        #     if self.nl.nl_have('time_control',n):
        #         self.nl.nl_del('time_control',n)
        s('domains','parent_id',p('domains','grid_id'))
        s('domains','parent_time_step_ratio',3)
        s('domains','parent_grid_ratio',3)
        self.dx=parent.dx/3.
        self.dy=parent.dy/3.
        s('domains','dx',p('domains','dx')/3)
        s('domains','dy',p('domains','dy')/3)
        s('domains','e_vert',p('domains','e_vert'))
        start=str(t('start','auto')).lower()
        if start=='fixed':
            s('domains','i_parent_start',int(t('istart')))
            s('domains','j_parent_start',int(t('jstart')))
        elif start=='centered':
            s('domains','i_parent_start','**CENTERED**')
            s('domains','j_parent_start','**CENTERED**')
        elif start=='auto':
            s('domains','i_parent_start','**AUTO**')
            s('domains','j_parent_start','**AUTO**')
        else:
            raise InvalidDomainStart(
                '%s: Invalid value for start.  It must be "fixed" "centered" or "auto",'
                ' but you gave %s'%(self.name,repr(start)),self.name)
        assert(self.dy is not None)
    def make_namelist(self):
        """Creates the WRF namelist contents and returns it as a string."""
        return self.nl.make_namelist()
    def has_output(self,stream):
        """Returns True if the domain will output to the specified
        stream."""
        return stream in self._output
    def no_output(self,stream):
        """Disables output for the specified stream."""
        self.add_output(self,stream,start=sim_start+5)
        del self._output[stream]
    def get_output_range(self,stream):
        """Returns a tuple containing the first output time, last
        output time and output interval for this domain and the
        specified stream."""
        if not self.has_output(stream):
            raise OutputStreamDisabled(
                'Stream %s is disabled for domain %s'%(stream,repr(self)))
        start=self._start
        if 'start' in self._output[stream] and \
                self._output[stream]['start'] is not None:
            start=to_datetime_rel(self._output[stream]['start'],start)
        end=self._end
        if 'end' in self._output[stream] and \
                self._output[stream]['end'] is not None:
            end=to_datetime_rel(self._output[stream]['end'],start)
        interval=to_timedelta(self._output[stream]['step'])
        return (start,end,interval)
    def hifreq_file(self):
        """Returns the hifreq filename for this domain."""
        return parse_wrf_outname('hifreq_d<domain>.htcf',self.get_grid_id(),
                               self._start,True)
    def _get_output_time(self,when):
        """This is an internal implementation function.  You should
        not call it directly.  It returns the nearest
        datetime.datetime to the specified time that lies on a model
        timestep, without going over."""
        return nearest_datetime(self._start,when,self._dt)
    def _get_output(self,stream,outname,when,actual_time=None,logger=None):
        """This is an internal implementation function.  You should
        not call it directly.  It returns a WRFOutput object for the
        specified output time (when) and stream.  The outname is the
        WRF output filename syntax, with <domain> and <date> in it.
        If actual_time is specified, it is used as the output time,
        otherwise, "when" is passed into self._get_output_time to get
        the actual time WRF will output it."""
        if actual_time is None:
            actual_time=self._get_output_time(when)
        path=parse_wrf_outname(outname,self.get_grid_id(),actual_time,
                               self.nocolons)
        assert(actual_time is not None)
        return WRFOutput(self.get_anl_time(),stream,self,path,
                         validtime=actual_time)
    def get_all_outputs(self,time=None):
        """Iterates over all output files as WRFOutput objects.  If a
        time is specified, then only outputs for that time are
        yielded."""
        for stream in self._output:
            if time is None:
                for obj in self.get_outputs(stream):
                    yield obj
            else:
                yield self.get_output(stream,to_datetime_rel(time,self._start))
    def get_outputs(self,stream):
        """Iterates over all output files for the specified stream, as
        WRFOutput objects."""
        (start,end,interval)=self.get_output_range(stream)
        epsilon=to_timedelta('0+1/500')
        when=start
        firstwhen=start
        prevwhen=None
        outname=self._output[stream]['outname']
        while when<end+epsilon:
            actual_time=self._get_output_time(when)
            if prevwhen is None or prevwhen!=actual_time:
                obj=self._get_output(stream,outname,when,actual_time)
                prevwhen=actual_time
                yield obj
            when=when+interval
            if when==firstwhen:
                raise InvalidTimespan(
                    'Zero output interval %s: somehow %s+%s=%s'%\
                        (repr(interval),repr(when),repr(interval),repr(when)))
    def get_output(self,stream,time,logger=None):
        """Returns a WRFOutput object for the output file for the
        specified stream and time, or None if no such file exists.
        Will return the first output time not before the given
        time."""
        (start,end,interval)=self.get_output_range(stream)
        time=to_datetime_rel(time,self._start)
        outname=self._output[stream]['outname']
        near=nearest_datetime(start,time,interval)
        if(time>end):
            return None
        # if(logger is not None):
        #     logger.debug('get_output %s %s %s %s %s %s %s' % (
        #         repr(start),repr(end),repr(interval),repr(outname),
        #         repr(stream),repr(near),repr(time)))
        result=self._get_output(stream,outname,near,logger=logger)
        # if(logger is not None):
        #     logger.debug('  got output %s'%(repr(result),))
        return result
    def has_output(self,stream):
        """Returns True if the specified stream has output for this
        domain and False otherwise."""
        return stream in self._output
    def no_output(self,stream):
        """Disables output for the specified stream."""
        del self._output[stream]
    def hide_output(self,stream):
        """If output is enabled for the specified stream, moves the
        output start time to after the end of the simulation.  That
        way any WRF code relying on the output frequency will still
        work, but no output will be generated.  Will not work for
        restart stream since that is not controlled on a per-domain
        basis.  Note that code that queries the output times will
        break if this is called."""
        if not self._output[stream]: return
        forever=999999 # far in the future, in minutes
        s=self.nl.nl_set
        d=self.nl.nl_del
        if stream=='restart' and not self.is_moad():
            return
        self._output[stream]['start']=forever
        self._output[stream]['end']=forever+1
        self._output[stream]['step']=forever
        if stream=='inputout':
            s('time_control','%s_begin_m'%(stream,),forever)
            s('time_control','%s_end_m'%(stream,),forever+1)
        else:
            s('time_control','%s_begin'%(stream,),forever)
            s('time_control','%s_end'%(stream,),forever+1)
        d('time_control','%s_begin_s'%(stream,))
        d('time_control','%s_end_s'%(stream,))
        del self._output[stream]
    def add_output(self,stream,start=None,end=None,step=None,outname=None, 
                   frames_per_outfile=None,io_form=None,simstart=None):
        """Adds output to the specified stream.  Other arguments are
        optional:

          start - first time to output.  Anything accepted by
            to_datetime_rel, relative to the simulation start time.

          simstart - simulation start time.  

          end - last time to output.  Anything accepted by
            to_datetime_rel, relative to the output start time.

          step - output timestep, passed through to_timedelta

          outname - output filename format, as used by WRF.  The
            <domain> and <date> should be specified, if relevant.

          frames_per_outfile - same meaning as in the WRF namelist

          io_form - same meaning as in the WRF namelist.  Only 1
            (intio), 2 (netcdf), and 11 (pnetcdf) are recognized,
            though one can add 100 or 200 to achieve the usual effect."""
        if io_form is None:
            iof=self.nl.trait_get('io_form','missing')
            if iof=='missing':
                io_form=2
            else:
                io_form=int(iof)
        assert(isinstance(io_form,int))
        #print 'add output for stream ',stream
        s=self.nl.nl_set
        # Check input, convert to usable objects:
        if outname is None:
            outname=default_wrf_outname(stream)
        (dstart,dend,fstep)=(None,None,None)
        if ( start is not None or end is not None ) and simstart is None:
            raise TypeError('WRFDomain.add_output: simstart must be provided '
                            'if start or end times are given')
        if start is not None:       
            start=to_datetime_rel(start,simstart)
            dstart=start-simstart
            (smin,ssec,srest) = minutes_seconds_rest(dstart)
            if srest!=0: 
                raise PrecisionTooHigh(
                    'Output start time must be an integer multiple of a '
                    'second after simulation start time.')
        if end is not None:
            startrel=self._start if (start is None) else start
            dend=to_datetime_rel(end,simstart)
            fend=dend-simstart
            (emin,esec,erest) = minutes_seconds_rest(fend)
            if erest!=0:
                raise PrecisionTooHigh(
                    'Output end time must be an integer multiple of a second '
                    'after simulation start time.')

        if step is not None:        fstep=to_fraction(step)
        if fstep is None:           fstep=to_fraction(3600*6) # six hours
        (minutes,seconds,rest)=minutes_seconds_rest(fstep)
        if rest!=0:
            raise PrecisionTooHigh(
                'Output frequency must be an integer multiple of a second.')

        frames_per='frames_per_outfile' 
        if stream!='history':
            frames_per='frames_per_%s'%(stream,)

        # Set namelist values:
        # no, bad: s('time_control','%s_outname'%(stream,),outname)
        if stream=='inputout':
            s('time_control','%s_interval_m'%(stream,),minutes)
        else:
            s('time_control','%s_interval'%(stream,),minutes)
        if seconds!=0:
            s('time_control','%s_interval_s'%(stream,),seconds)
        if stream=='restart' and not self.is_moad():
            pass # print 'NO TIME CONTROL FOR NON-MOAD RESTART'
        elif start is not None:
            if stream=='inputout':
                s('time_control','%s_begin_m'%(stream,),smin)
            else:
                s('time_control','%s_begin'%(stream,),smin)
            if ssec!=0:
                s('time_control','%s_begin_s'%(stream,),ssec)
        if end is not None and stream!='restart':
            if stream=='inputout':
                s('time_control','%s_end_m'%(stream,),emin)
            else:
                s('time_control','%s_end'%(stream,),emin)
            if esec!=0:
                s('time_control','%s_end_s'%(stream,),esec)
        #no, bad: s('time_control','%s_outname'%(stream,),outname)
        if stream!='inputout' and stream!='restart':
            if frames_per_outfile is not None:
                s('time_control',frames_per,int(frames_per_outfile))
            else:
                s('time_control',frames_per,1)
        #no, bad: s('time_control','io_form_%s'%(stream,),io_form)
        # Store data needed to generate outputs:
        self._output[stream]={'start':dstart, 'end':dend, 
                              'step':fstep, 'outname':outname}
        self.nl.nl_set_if_unset('time_control','nocolons',True)
    def interval_for(self,stream):
        """Returns the output interval for the specified stream, or
        None if the stream is disabled."""
        if stream in self._output:
            return self._output[stream]['step']
        return None

########################################################################

class WRFSimulation(WRFDomains):
    def copy(self):
        return WRFSimulation(None,None,None,None,None,None,self)
    def __init__(self,conf,section,moad,simstart,simend,timestep=None,dup=None):
        if dup is not None:
            # copy constructor
            WRFDomains.__init__(self,None,None,None,None,None,None,dup=dup)
            self._wps=dup._wps
            self._tiling=dup._tiling
            for domain in self: pass
            return

        WRFDomains.__init__(self,conf,section,moad,simstart,simend,timestep)

        self._wps=None

        s=self.nl.nl_set
        sh=self.nl.nl_have
        siu=self.nl.nl_set_if_unset
        t=self.nl.trait_get
        th=self.nl.trait_have

        # Make sure all namelists that we know are required, are present:
        self.nl.nl_section(
            'time_control','fdda','domains','physics','dynamics',
            'bdy_control','namelist_quilt','logging')

        dt=to_fraction(t('dt'))
        (i,n,d)=split_fraction(dt)
        s('domains','time_step',i)
        s('domains','time_step_fract_num',n)
        s('domains','time_step_fract_den',d)

        s('time_control','interval_seconds',t('bdystep'))
        s('domains','ptsgm',float(t('ptsgm')))
        s('domains','p_top_requested',float(t('ptop')))
        s('domains','use_prep_hybrid',bool(t('prep_hybrid')))
        s('domains','num_metgrid_soil_levels',int(t('metgrid_soil_levels',4)))
        s('domains','num_metgrid_levels',int(t('metgrid_levels',4)))

        siu('namelist_quilt','nio_tasks_per_group',0)
        siu('namelist_quilt','nio_groups',1)
        for stream in ['input','boundary','auxinput1','auxhist1','auxhist2',
                       'auxhist3','auxhist4','auxhist5','history','auxhist6',
                       'auxinput2']:
            n='io_form_'+stream
            if not sh('time_control',n):
                if th(n):
                    s('time_control',n,t(n))
                else:
                    s('time_control',n,self.io_form)
        siu('time_control','auxinput1_inname',"met_nmm.d<domain>.<date>")
        siu('domains','nproc_x',-1)
        siu('domains','nproc_y',-1)

        nio=self.nl.nl_get('namelist_quilt','nio_tasks_per_group') * \
            self.nl.nl_get('namelist_quilt','nio_groups')

        siu('namelist_quilt','poll_servers',nio > 0)

        self._tiling=None
        s('domains','numtiles',1)
        s('domains','tile_sz_x',0)
        s('domains','tile_sz_y',0)
    def set_nprocs(self,nproc_x=-1,nproc_y=-1):
        """Sets the WRF namelist values of nproc_x and nproc_y, which
        configure task geometry.  Default values are -1, which tells
        WRF to automatically decide the task geometry."""
        nproc_x=int(nproc_x)
        nproc_y=int(nproc_y)
        s=self.nl.nl_set
        s('domains','nproc_x',nproc_x)
        s('domains','nproc_y',nproc_y)
        return self
    def has_output(self,stream):
        for domain in self:
            if domain.has_output(stream):
                return True
        return False
    def set_io_servers(self,tasks_per_group,groups,poll_servers=True):
        """Sets the I/O server configuration in WRF."""
        tasks_per_group=int(tasks_per_group)
        groups=int(groups)
        poll_servers=bool(poll_servers)
        s=self.nl.nl_set
        s('namelist_quilt','nio_tasks_per_group',tasks_per_group)
        s('namelist_quilt','nio_groups',groups)
        s('namelist_quilt','poll_servers',poll_servers)
        return self
    @property
    def nio_tasks_per_group(self):
        """The number of I/O server tasks per group"""
        iopg=self.nl.nl_get('namelist_quilt','nio_tasks_per_group','1')
        iopg=int(iopg)
        return iopg
    @property
    def nio_groups(self):
        """The number of I/O server groups"""
        ngroups=self.nl.nl_get('namelist_quilt','nio_groups','0')
        ngroups=int(ngroups)
        return ngroups
    def set_bdystep(self,step):
        """Sets the interval at which this WRF simulation expects
        boundary conditions.  Accepts anything that can be passed to
        to_timedelta."""
        step=to_timedelta(step)
        step=to_fraction(step)
        step=int(float(round(step)))
        self.nl.trait_set('bdystep',step)
        self.nl.nl_set('time_control','interval_seconds',step)
    def bdystep(self):
        """Computes the interval at which this WRF simulation expects
        boundary conditions as a datetime.timedelta."""
        return to_timedelta(self.nl.trait_get('bdystep'))
    def bdyepsilon(self):
        """Returns the largest difference between two times such that
        they are considered identical.  This is used in the context of
        WRF boundary input times."""
        return to_fraction(self.nl.trait_get('bdystep'))/10
    def bdytimes(self):
        """Iterates over times at which this WRF simulation expects
        boundary conditions.  Yields datetime objects for each
        time."""
        dt=self.bdystep()
        now=self.simstart()
        end=self.simend() + to_timedelta(to_fraction(dt)/10)
        while now<end:
            yield now
            now+=dt
    def num_tiles(self):
        """Gets the number of WRF tiles in each WRF patch, returning 1
        if tiling is not in use."""
        if self._tiling is None:
            return 1
        else:
            return self._tiling[0]*self._tiling[1]
    def set_tiling(self,x,y):
        """Sets the number of WRF OpenMP tiles in each WRF MPI patch
        to x by y.  Don't use this: it is not supported by WRF-NMM"""
        self._tiling=[int(x),int(y)]
        s('domains','numtiles',int(x)*int(y))
        s('domains','tile_sz_x',int(x))
        s('domains','tile_sz_y',int(y))
    def wrf_namelist(self,section_sorter=None,var_sorters=None):
        """Generates a Conf2Namelist object for the namelist that
        should be input to wrf.exe"""
        nl=self.nl.copy()
        domain_nl_list=[d.nl for d in self]
        domain_nl=domain_nl_list[0].join(domain_nl_list[1:])
        return domain_nl.copy(other=nl).remove_traits().set_sorters(
            _wrf_namelist_order,_wrf_nl_var_order)
    def _analysis_setup(self,io_form=None):
        self._domains_done=True
        if io_form is None:
            io_form=self.nl.trait_get(
                'io_form_restart',self.nl.trait_get('io_form',
                                                    self._io_form))
        self.nl.nl_set('time_control','io_form_restart',io_form)
        self.nl.nl_set('time_control','override_restart_timers',True)
        self.nl.nl_set('time_control','restart',False)
        self.nl.nl_set_if_unset('time_control','restart_interval',36000)
        self.nl.nl_set('time_control','reset_simulation_start',False)
    def analysis_out(self,io_form=None):
        """Requests that this WRF simulation write analysis files."""
        for domain in self:
            #print 'domain %s analysis = %s'%(repr(domain),repr(False))
            domain.nl.nl_set('time_control','analysis',False)
        self._analysis_setup(io_form)
        return self
    def analysis_in(self,io_form=None):
        """Requests that this WRF simulation read an analysis file."""
        self._analysis_setup(io_form)
        moad=self.get_moad()
        for domain in self:
            val = domain!=moad
            #print 'domain %s analysis = %s'%(repr(domain),repr(val))
            domain.nl.nl_set('time_control','analysis',val)
        self._analysis_setup(io_form)
        return self
    def set_wrfanl_outname(self,pattern):
        """Sets the WRF wrfanl output file pattern for all domains."""
        for domain in self:
            domain.nl.nl_set('time_control','anl_outname',
                             'wrfanl_d<domain>_<date>')
    def analysis_name(self,domain):
        """Produces an analysis filename for the specified domain.
        NOTE: this function assumes all domains have the same wrfanl
        filename format"""
        domain=self.get(domain)
        domid=self.get(domain).get_grid_id()
        pattern=domain.nl.nl_get('time_control','anl_outname',
                                 'wrfanl_d<domain>_<date>')
        return parse_wrf_outname(pattern,domid,self.simstart(),
                                 self.get_nocolons())
    def restart_in(self,restartsource):
        """Requests that this WRF simulation read a restart file.
        This is not implemented since the restart capability is broken
        in HWRF."""
        raise NotImplementedError(
            'Restart capability is presently broken in HWRF.')
        return self
    # def restart_out(self,interval=3600*6,start=None,stop=None,io_form=None):
    #     """Requests that this WRF simulation write restart files.
    #     This is not implemented since the restart capability is broken
    #     in HWRF.
    #       interval - how many seconds between restart files
    #       start - first restar file output time
    #       stop - last restart file output time
    #       io_form - the io_form to use"""
    #     interval=hwrf.numerics.to_timedelta(interval)
    #     if io_form is None: io_form=self.io_form_for(stream)
    #     self.nl.nl_set('time_control','io_form_restart',io_form)
    #     self.nl.nl_set('time_control','restart_interval',interval)
        
    #     return self
    def set_active_io_form_to(self,io_form):
        """Sets the io_form for all active streams."""
        io_form=int(io_form)
        forms=set()
        for var,value in self.nl.nl_each('time_control'):
            if var.find('io_form')>=0:
                forms.add(var)
        for var in forms:
            self.nl.nl_set('time_control',var,io_form)
    def set_io_form(self,stream,io_form=None):
        """Sets the io_form for the given stream"""
        io_form_stream='io_form_%s'%(stream,)
        s=self.nl.nl_set
        ts=self.nl.trait_set
        t=self.nl.trait_get
        if io_form is not None:
            s('time_control',io_form_stream,int(io_form))
            ts(io_form_stream,int(io_form))
        else:
            s('time_control',io_form_stream,self.io_form_for(stream))
            ts(io_form_stream,self.io_form_for(stream))
    def set_timing(self,start=None,end=None,timestep=None):
        """Sets the simulation start and end times, and timestep.  The
        start may be anything accepted by to_datetime.  The end is
        passed through to_datetime_rel, relative to start.  The
        timestep must be accepted by to_fraction."""
        if start is None: start=self._simstart
        if end is None: end=self._simend
        if timestep is None: timestep=self._timestep
        start=to_datetime(start)
        end=to_datetime_rel(end,start)
        timestep=to_fraction(timestep)
        for domain in self:
            domain.set_timing(start,end,timestep/domain.moad_ratio())
        self._simstart=start
        self._simend=end
        self._timestep=timestep
    def set_metgrid_levels_from(self,exepath,metgrid_out_file,logger=None):
        """Overrides the num_metgrid_levels in &domains to equal the
        value in the specified metgrid file.
          exepath - path to the hwrf_metgrid_levels program
          metgrid_out_file - path to the metgrid out file to read
          logger - a logging.Logger to use for logging (optional)"""
        strexe=str(exepath)
        strmet=str(metgrid_out_file)
        nummet=runstr(bigexe(strexe)[strmet],logger=logger)
        numsm=runstr(bigexe(strexe)[strmet,'num_sm_levels'],logger=logger)
        numst=runstr(bigexe(strexe)[strmet,'num_st_levels'],logger=logger)
        nummet=int(nummet)
        numsm=int(numsm)
        numst=int(numst)
        numsoil=min(numsm,numst)
        if logger is not None:
            logger.info('Have %d levels, %d soil levels (m=%d t=%d).'
                        %(nummet,numsoil,numsm,numst))
        self.nl.nl_set('domains','num_metgrid_levels',nummet)
        self.nl.nl_set('domains','num_metgrid_soil_levels',numsoil) 
    def swcorner_dynamic(self,exepath,storminfo,domlat,domlon,logger=None):
        """Runs the swcorner_dynamic program to fill in this
        WRFSimulation's domain start locations.  Returns the resulting
        namelist as a multi-line string.

        Inputs:
          exepath = full path to the hwrf_swcorner_dynamic

          storminfo = an hwrf.storminfo.StormInfo object for the storm
            of interest

          domlat, domlon = the outermost domain center lat & lon,
            which is also the projection center

          logger = optional: a logger.Logger object for logging"""
        # Create a copy of this WRF so we can fill in junk values for
        # the I & J starts:
        junkwrf=self.copy()
        junkwrf.fill_auto_starts(7) # set auto-start locations to sevens
        junknml=junkwrf.wrf_namelist().make_namelist()
        with open('fort.12','wt') as f:
            f.write(junknml)
        # sys.stdout.write(junknml)
        with open('domain.center','wt') as f:
            f.write('%f\n%f\n'%(float(domlat),float(domlon)))
            #f.write(self.confstrinterp("{domlat}\n{domlon}\n"))
        with open('storm.center','wt') as f:
            f.write('%f\n%f\n'%(storminfo.lat,storminfo.lon))
            #f.write(self.confstrinterp("{vit[lat]:f}\n{vit[lon]:f}\n"))
        strexe=str(exepath)
        checkrun(bigexe(strexe) << str(storminfo.hwrfbasin2),logger=logger)
        if not isnonempty('set_nest'):
            raise SetNestFailed('%s could not find the nest south-west '
                                'corner point.'%(strexe,))
        try:
            (istart, jstart) = (-99,-99)
            with open('set_nest','rt') as f:
                for line in f:
                    line=line.upper().rstrip()
                    m=re.search('([IJ])START=([0-9.+-]+)',line)
                    if m:
                        (ij,val)=m.groups()
                        if ij=='I': istart=int(val)
                        if ij=='J': jstart=int(val)
                    elif line and logger:
                        logger.warning('%s: ignoring unrecognized line %s'
                                       %(strexe,line.rstrip()))
            if not (istart>5 and jstart>5):
                raise SetNestFailed(
                    '%s: could not find the south-west corner point '
                    '(istart=%d jstart=%d)'%(strexe,istart,jstart))
            self.fill_auto_starts(istart,jstart)
            return self.wrf_namelist().make_namelist()
        except Exception as e:
            if logger:
                logger.warning('%s unexpected exception: %s'
                               %(strexe,str(e)),exc_info=True)
            raise
        

########################################################################

class ExternalWRFTask(HWRFTask):
    """This class represents a WRF simulation that is running in an
    external workflow.  It reads the WRF configuration to internally
    generate namelist information, as if it was going to run the WRF
    itself.  It then monitors the running or completed WRF simulation
    for simulation output, making it available as Product objects.
    All WRF outputs are available as UpstreamProduct objects.

    The WRFSimulation object is available as the public "wrf" member
    variable and is initialized by the ExternalWRFTask constructor
    using arguments similar to the WRFSimulation constructor.  The
    simulation start, end and timestep, if unspecified, are taken from
    the specified conf section's variables by the same name."""
    def __init__(self,dstore,conf,section,wrf,relocate=False,**kwargs):
        """Creates an ExternalWRFTask, as a wrapper around a
        WRFSimulation.  The conf, section, moad, simstart, simend and
        timestep are passed on to the WRFSimulation.  If simstart,
        simend, or timestep are None or missing, then they are taken
        from the configuration section for this task."""
        self.__prodcache=dict()
        self.__wrf=wrf
        if 'skip_parent' not in kwargs or not kwargs['skip_parent']:
            HWRFTask.__init__(self,dstore,conf,section,**kwargs)
        if 'outdir' not in self:
            if 'outdir' not in kwargs:
                self['outdir']=os.path.join(self.getdir('WORKhwrf'),
                                            self.taskname)
            else:
                self['outdir']=str(kwargs['outdir'])
        with self.dstore.transaction() as t:
            for p in self.products(relocate=relocate): pass
    def wrf(self):
        """Returns the underlying WRFSimulation object that describes
        the simulation that is being run."""
        return self.__wrf
    def unrun(self):
        """Marks all products as not having been delivered.  Does not
        delete anything."""
        for product in self.products():
            product.undeliver()
    def clear_cached_products(self):
        """Clears the cache of WRFOutput -> Product mappings, used to
        speed up as_product.  Calling this will ensure that any later
        calls to as_product will generate new Product objects."""
        self.__prodcache=dict()
    def as_product(self,wrfout,relocate=False):
        """Converts a WRFOutput to a Product."""
        if self.__prodcache.has_key(wrfout):
            return self.__prodcache[wrfout]
        rel=wrfout.path()
        #outdir=os.path.join(self['outdir'],rel)
        outdir=self['outdir']
        assert(outdir is not None)
        loc=os.path.join(outdir,os.path.basename(wrfout.path()))
        with self.dstore.transaction() as t:
            uf=UpstreamFile(self.dstore,category=self.taskname,
                            prodname=rel,location=loc)
            uf['stream']=wrfout.stream()
            uf['location']=loc
        if relocate:    uf.location=loc
        self.__prodcache[wrfout]=uf
        return uf
    def products(self,domains=None,stream=None,time=None,relocate=False):
        """Iterates over all Products subject to the given
        constraints, or all Products if no constraints are given:

          domains - only these WRFDomains
          stream - only these streams (strings)
          time - only these times.  

        For time specification, the earliest output that is not before
        the target time is yielded.
        """
        if domains is None:
            domlist=[d for d in self.__wrf]
        elif isinstance(domains,WRFDomainBase) or isinstance(domains,int) \
                or isinstance(domains,basestring):
            domlist=[self.__wrf.get(domain)]
        else:
            domlist=[self.__wrf.get(d) for d in domains]
        for domain in domlist:
            if stream is None:
                for out in domain.get_all_outputs(time):
                    yield self.as_product(out,relocate=relocate)
            elif time is None:
                for out in domain.get_outputs(stream):
                    yield self.as_product(out,relocate=relocate)
            else:
                yield self.as_product(domain.get_output(stream,time),
                                      relocate=relocate)
    def wrf_check(self,product=None,stream=None,time=None):
        """Calls Product.check on all products that match the given
        constraints.  The constraints are passed on to the
        self.products function to iterate over selected products."""
        if product is not None:
            product.check()
        else:
            for prod in self.products(stream,time):
                product.check()
    def update_state(self):
        """Scans the rsl.out.0000 file to automatically determine the
        state of the WRF simulation.  Looks for "SUCCESS COMPLETE" and
        "FATAL CALLED" to detect successful completion, or calling of
        wrf_error_fatal.  Sets self.state to COMPLETED, FAILED,
        RUNNING or UNSTARTED based on the contents of rsl.out.0000"""
        logger=self.log()
        logger.info('Check on status of WRF...')
        rsl0=os.path.join(self.location,'rsl.out.0000')
        if not produtil.fileop.isnonempty(rsl0):
            logger.info('No RSL file here: '+repr(rsl0))
            self.state=UNSTARTED
        try:
            with open(rsl0,'rt') as f:
                try:
                    f.seek(-10000,os.SEEK_END) # seek to 10000 bytes from end
                except EnvironmentError as e:
                    logger.info(
                        'Cannot seek -10000 bytes.  Will read whole file.')
                    pass # if we cannot seek, then just read from
                         # start of file
                for line in f:
                    if re.search('SUCCESS COMPLETE',line):
                        logger.info('WRF is complete: %s'%(line.rstrip(),))
                        self.state=COMPLETED
                        return
                    elif re.search('FATAL CALLED',line):
                        logger.info('WRF failed: %s'%(line.rstrip(),))
                        self.state=FAILED
                        return
        except EnvironmentError as e:
            logger.warning('Unexpected error checking WRF: %s'%(str(e),),
                           exc_info=True)
            self.state=UNSTARTED
        self.state=RUNNING
        
