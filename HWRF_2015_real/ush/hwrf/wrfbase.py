"""This module contains low-level classes that underlie the
implementation of the hwrf.wrf module.  It also contains the WRFOutput
class, which is used to describe a WRF output file, and the
parse_wrf_outname, which turns WRF-style filename formats
(wrfout_d<domain>_<date>) into filenames."""

import fractions,math,re,datetime, pdb, logging

from hwrf.numerics import *
from hwrf.namelist import *
from hwrf.exceptions import *

__all__=['parse_wrf_outname','WRFOutput','WRFDomainBase','WRFDomains']

########################################################################

def parse_wrf_outname(outname,grid_id,date,nocolons):
    """Takes a wrf outname, a grid_id and a date and produces the
    final string outname.  The mandatory boolean argument nocolons is
    set to the namelist &time_control nocolons value."""
    assert(isinstance(outname,basestring))
    out=outname
    if re.search('(?i)<domain>',out):
        out=re.sub('(?i)<domain>','%02d'%(int(grid_id),),out)
    if re.search('(?i)<date>',out):
        out=re.sub('(?i)<date>',to_datetime(date).\
                       strftime('%Y-%m-%d_%H:%M:%S'),out)
    if nocolons:
        out=out[0:2]+re.sub(':','_',out[2:])
    return out

########################################################################

class WRFOutput:
    def __init__(self,anltime,stream,domain,path,validtime=None,
                 fcstdelta=None):
        """Creates a WRFOutput object that knows the path to its file
        (self.path()), the output time as a datetime
        (self.validtime()), the simulation start time as a datetime
        (self.anltime()), the output forecast second as a timedelta
        (self.fcstdelta()), the name of the WRF stream
        (self.stream()), and the WRF domain object (self.domain()).
        Do not modify the domain object or many things may break.

        You must specify exactly one of validtime or fcstdelta.  If
        you specify both, OverspecifiedOutputTime will be raised.  If
        you specify neither, NoOutputTime will be raised."""
        assert(isinstance(stream,basestring))
        assert(isinstance(path,basestring))
        self._anltime=to_datetime(anltime)
        if validtime is None and delta is None:
            raise NoOutputTime('In WRFOutput.__init__, both validtime and '
                               'fcstdelta were None')
        elif validtime is not None:
            self._validtime=to_datetime(validtime)
            self._fcstdelta=self._validtime-self._anltime
        elif fcstdelta is not None:
            self._fcstdelta=to_timedelta(fcstdelta)
            self._validtime=self._anltime + self._fcstdelta
        else:
            raise OverspecifiedOutputTime(
                'In WRFOutput.__init__, both validtime and fcstdelta were '
                'specified.  You must specify exactly one.')
        self._path=path
        self._stream=str(stream)
        assert(domain is None or isinstance(domain,WRFDomainBase))
        self._domain=domain
    def __hash__(self):
        return hash(self._domain) ^ hash(self._stream) ^ hash(self._path) ^ \
            hash(self._validtime) ^ hash(self._anltime)
    def __eq__(self,other):
        """Returns True if the other WRFOutput object is identical to
        this one, and False if it is not.  For anything other than a
        WRFOutput, returns NotImplemented."""
        if not isinstance(other,WRFOutput):
            return NotImplemented
        if not self._domain==other._domain:         return False
        if not self._stream==other._stream:         return False
        if not self._path==other._path:             return False
        if not self._validtime==other._validtime:   return False
        if not self._anltime==other._anltime:       return False
        return True
    def __str__(self):
        return '%s output stream=%s path=%s' % \
            (repr(self.domain()),str(self.stream()),str(self.path()))
    def __repr__(self):
        return 'WRFOutput(%s,%s,%s,%s,validtime=%s)' % \
            (repr(self.anltime()),repr(self.stream()),repr(self.domain()),
             repr(self.path()),repr(self.validtime()))
    def path(self): 
        """Returns the full path to the output file."""
        return self._path
    def stream(self): 
        """Returns the lower-case name of the WRF stream."""
        return self._stream
    def domain(self): 
        """Returns the domain object for this output file's WRF
        domain.  Do not modify the returned object or many things will
        break."""
        return self._domain
    def fcstdelta(self):
        """Returns the time difference between the valid (output) time
        and the analysis (simulation start) time."""
        return self._fcstdelta
    def validtime(self): 
        """Returns the output time as a datetime.datetime."""
        return self._validtime
    def anltime(self): 
        """Returns the analysis time as a datetime.datetime."""
        return self._anltime

########################################################################

class WRFDomainBase(object):
    """This is the superclass of WRFDomain and it should not be
    instantiated directly."""
    def __init__(self,conf,section,name=None,copy=None):
        """Creates a new WRFDomainBase.
          conf - an HWRFConfig with configuration information
          section - the section to read for information about this domain
          name - the name of the domain.  Default: section name.
          copy - do not specify this.  It is used by self.copy()
            to copy a WRFDomainBase."""
        self.nestlevel=None
        self.parent=None
        self.nocolons=True
        self._start=None
        self._end=None
        self._dt=None
        self._output={}
        if copy is not None:
            self.nl=copy.nl.copy()
            self.name=str(copy.name)
            (  self._start,self._end,self._dt,self.nestlevel,self.parent,
               self.nocolons) = \
              (copy._start,copy._end,copy._dt,copy.nestlevel,copy.parent,
               copy.nocolons)
        else:
            self.nl=Conf2Namelist(conf,section)
            self.name=str(section)
    def __hash__(self): return hash(self.name)
    def __repr__(self): return '<WRFDomain name=%s>'%(str(self.name),)
    def __cmp__(self,other): return cmp(self.name,other.name)
    def __str__(self): return repr(self) 
    def get_anl_time(self):
        """Returns the analysis time"""
        if self.parent is not None:
            return self.parent._start
        else:
            return self._start
    def get_grid_id(self):
        """Raises NotImplementedError.  The WRFDomain overrides this
        to return the grid id."""
        raise NotImplementedError(
            'WRFDomainBase does not implement get_grid_id')
    def remove_forbidden(self):
        """Removes all namelist entries that the conf files are
        forbidden from specifying.  Generally this is anything related
        to domain start, end, size, I/O or timesteps."""
        self.nl=self.nl.copy(var_subset=self._nl_subsetter)
    def _nl_subsetter(self,s,v):
        """Returns True."""
        return True
    def copy(self):
        """Returns a copy of this object.  The copy has its own data
        structures, so modifying the copy will not modify the
        original."""
        return WRFDomainBase(None,None,copy=self)
    def _validate_timespan(self,start,end,timestep):
        """Analyzes a potential WRF simulation start, end and timestep
        to make sure it matches assumptions made within WRF, and
        within the hwrf package.
          1. There must be no timezone (UTC only)
          2. The start time must not contain microseconds.
          3. The end time must be at or after the start time"""
        ts=to_datetime(start)
        te=to_datetime(end)
        dt=to_fraction(timestep)
        if te.tzinfo is not None or ts.tzinfo is not None:
            raise TimezoneProvided(
                'WRF start and end times must not contain timezones.')
        if te.microsecond!=0 or ts.microsecond!=0:
            raise PrecisionTooHigh(
                'WRF start and end times must lie exactly on a second.')
        if(te<ts):
            raise EndBeforeStart(
                'Invalid domain start/end times: end is before start.',ts,te)
        #if not is_at_timestep(ts,te,dt):
        #    raise EndNotTimestep('Domain end time does not lie on a '
        #                         'domain timestep.',ts,te)
        return (ts,te,dt)
    def set_timing(self,start,end,timestep):
        """Sets the start and end times of this domain, and the domain
        timestep."""
        (ts,te,dt)=self._validate_timespan(start,end,timestep)
        self._start=ts
        self._end=te
        self._dt=dt
    def init_domain(self,grid_id):
        """Initializes this domain's variables that are unrelated to
        nesting."""
    def init_as_moad(self,simstart,simend,simdt,eta_levels):
        """Initializes this domain's variables so it knows to act as
        the Mother Of All Domains (MOAD)"""
        (self._simstart,self._simend,self._simdt)=(simstart,simend,simdt)
        self.set_timing(simstart,simend,simdt)
        self.init_domain(1)
        self.parent=None
    def init_as_nest(self,parent,grid_id,start,end):
        """Initializes this domain's variables to make it a nest
        within the given parent WRFDomain"""
        self.init_domain(grid_id)
        self.parent=parent
        (self._simstart,self._simend,self._simdt) = \
            (parent._simstart,parent._simend,parent._simdt)
        if not is_at_timestep(parent._start,start,parent._dt):
            raise StartNotAtParentTimestep(
                'Start time %s for domain %d is not at parent %d timestep.'
                %(parent._start,grid_id,parent.grid_id))
        self.set_timing(parent._simstart,parent._simend,parent._simdt/3)
    def make_namelist(self):
        """Creates the wrf namelist as a string, and returns it."""
        return self.nl.make_namelist()

########################################################################

class WRFDomains(object):
    """This is the abstract base class of WRFSimulation.  You should
    not instantiate it directly.  Its purpose is to combine
    information about multiple WRFDomainBase objects (or WRFDomain
    objects) to make a single namelist.  It also has the ability to
    make aggregate changes to those objects, or obtain aggregate
    information about them."""
    def copy(self):
        """Returns a copy of this object, but with its own data
        structures so modifying the copy will not modify the
        original."""
        return WRFDomains(None,None,None,None,None,None,dup=self)
    def __init__(self,conf,section,moad,simstart,simend,timestep=None,dup=None):
        """Creates a new WRFDomains object:
          conf - the HWRFConfig to provide configuration information
          section - the section to use in that config object
          moad - the Mother of All Domains, as a WRFDomain 
          simstart, simend - start and end times
          timestep - the simulation timestep
          dup - do not use.  This is used by the self.copy() do do a deep
            copy of a WRFDomains."""
        if dup is not None:
            # copy constructor
            self.nl=dup.nl.copy()
            ( self._simstart,self._simend,self._timestep,self._io_form ) = \
             ( dup._simstart, dup._simend, dup._timestep, dup._io_form )
            self._nextid=dup._nextid
            self._domains_done=dup._domains_done
            self._grid=dict()
            self._to_id=dict()
            self._to_name=dict()
            self._parent=dict()
            for (name,domain) in dup._grid.iteritems():
                gid=dup._to_id[name]
                self._grid[name]=domain.copy()
                self._to_id[name]=gid
                self._to_name[gid]=name
            for (a,b) in dup._parent.iteritems(): self._parent[a]=b
            return
        self.nl=Conf2Namelist(conf,str(section))
        eta_levels=self.nl.nl_get('domains','eta_levels')
        self._simstart=to_datetime(simstart)
        self._simend=to_datetime(simend)
        if timestep is None:
            timestep=self.nl.trait_get('dt')
        self._timestep=to_fraction(timestep)
        iof=self.nl.trait_get('io_form','missing')
        if iof=='missing':
            self._io_form=2 # default IO form
        else:
            self._io_form=int(iof)

        nc=self.get_nocolons()
        mymoad=moad.copy()
        mymoad.remove_forbidden()
        mymoad.init_as_moad(self._simstart,self._simend,self._timestep,
                            eta_levels)
        mymoad.nocolons=nc

        self._grid={mymoad.name:mymoad}
        self._to_id={mymoad.name:1}
        self._to_name={1:mymoad.name}
        self._parent={} # mapping of child name to parent name
        self._nextid=2 # next unused grid ID

        self._domains_done=False # True if we can no longer add domains
    def fill_auto_starts(self,ivalue,jvalue=None,fillthis='**AUTO**',centerthis='**CENTERED**'):
        """For domains whose i_parent_start or j_parent_start are set
        automatically (start=fillthis), this fills those domains
        starts with the provided values.  Domains that should be
        centered in their parent (start=centerthis) are centered
        relative to their parent if they have one."""
        if jvalue is None: jvalue=ivalue

        # Make sure that the start J is odd if it is set.  Mimics the
        # ksh calculation.
        jvalue=(int(jvalue)+1)/2*2-1
        def intify(bob):
            if isinstance(bob,int): return bob
            return int(bob,10)

        for domain in self:
            got=domain.nl.nl_get('domains','i_parent_start')
            if isinstance(got,basestring):
                if got==fillthis:
                    domain.nl.nl_set('domains','i_parent_start',ivalue)
                elif got==centerthis:
                    if domain.parent is None:
                        raise InvalidDomainStart(
                            '%s: invalid value for start.  Only nests can '
                            'have start=centered'%(domain.name,))
                    dxp=intify(domain.parent.nl.nl_get('domains','e_we'))
                    dxd=intify(domain.nl.nl_get('domains','e_we'))
                    rat=intify(domain.nl.nl_get('domains','parent_grid_ratio',3))
                    icen=(dxp-dxd//rat)//2
                    domain.nl.nl_set('domains','i_parent_start',icen)
                else:
                    raise InvalidDomainStart(
                        '%s: invalid value for start.  Must be %s, %s or an '
                        'integer'%(got,fillthis,centerthis))
            got=domain.nl.nl_get('domains','j_parent_start')
            if isinstance(got,basestring):
                if got==fillthis:
                    domain.nl.nl_set('domains','j_parent_start',jvalue)
                elif got==centerthis:
                    if domain.parent is None:
                        raise InvalidDomainStart(
                            '%s: invalid value for start.  Only nests can '
                            'have start=centered'%(domain.name,))
                    dyp=intify(domain.parent.nl.nl_get('domains','e_sn'))
                    dyd=intify(domain.nl.nl_get('domains','e_sn'))
                    rat=intify(domain.nl.nl_get('domains','parent_grid_ratio',3))
                    jcen=(dyp-dyd//rat)//2
                    jcen=(int(jcen)+1)//2*2-1
                    domain.nl.nl_set('domains','j_parent_start',jcen)
                else:
                    raise InvalidDomainStart(
                        '%s: invalid value for start.  Must be %s, %s or an '
                        'integer'%(got,fillthis,centerthis))
        return self
    def simstart(self): 
        """Returns the simulation start time as a datetime.datetime."""
        return self._simstart
    def simend(self):  
        """Returns the simulation end time as a datetime.datetime."""
        return self._simend
    def timestep(self): 
        """Returns the simulation timestep as a datetime.time_delta."""
        return self._timestep

    @property
    def nocolons(self):
        """Should colons be omitted from filenames?  The return value
        may be cached.  To ensure the value is recomputed instead, you
        can call get_nocolons."""
        if '_nocolons_cache' not in self.__dict__:
            return self.get_nocolons()
        return self._nocolons_cache

    def get_nocolons(self):
        """This method guesses the nocolons setting from the
        WRF/WPS/Real namelist.  The result of this calculation is
        never cached: all namelist options are re-scanned and the flag
        is recomputed each time.  To get the cached value, use the
        nocolons property instead."""
        nc=self.nl.trait_get('nocolons',True)
        if not nc:
            for var,value in self.nl.nl_each('time_control'):
                if var.find('io_form')>=0:
                    iof=int(value)%100
                    if iof==1 or iof==11:
                        nc=True
                        break
        if not nc:
            for var,value in self.nl.trait_each():
                if var.find('io_form')>=0:
                    iof=int(value)%100
                    if iof==1 or iof==11: 
                        nc=True
                        break
        self.nl.nl_set('time_control','nocolons',nc)
        self.__dict__['_nocolons_cache']=nc
        return nc
    def get_io_form(self): 
        """Gets the default io_form."""
        return self._io_form
    def set_io_form(self,i): 
        """Sets the default io_form."""
        self._io_form=i
    io_form=property(get_io_form,set_io_form,None,
                     "The default I/O form for this WRF")
    def io_form_for(self,stream):
        """Returns the io_form for the specified stream."""
        if stream=='history': stream='output'
        iof=self.nl.trait_get('io_form_%s'%(stream),int(self._io_form))
        return iof
    def get_moad(self): 
        """Returns the MOAD as a WRFDomain."""
        return self._grid[self._to_name[1]]
    def get_last(self):
        """Returns the last domain added to this WRF."""
        return self._grid[self._to_name[self._nextid-1]]
    def add_output(self,stream='history',domain=None,io_form=None,
                   start=None,end=None,step=None,frames_per_outfile=None,
                   outname=None):
        """Requests that one or all WRF domains output the specified
        stream.  The stream should be "history" or "auxhistN" for an
        integer N>0.  Optional "domain" specifies the domain that
        should output this stream, otherwise all domains will output
        it.

        Other arguments: in all of these, replace $stream with the
        stream:

          io_form = WRF IO form.  Simply calls self.set_io_form(
            stream,io_form)
          step = output interval, sent into to_out_interval().  
            Default: trait $stream_interval or 6hrs
          outname = output name or array of output names (one per domain).
            Can only specify for all domains, not for only one.
            Default: leave unspecified, and let WRF use its defaults.
          start = output start time (anything accepted by to_datetime)
            Default: simulation start time.
          end = output end time (anything accepted by to_datetime.
            Default: simulation end time.

        NOTE: You cannot add any more domains after calling this
        routine."""
        self._domains_done=True
        if io_form is None:
            io_form=self.io_form_for(stream)
        else:
            self.set_io_form(stream,io_form)
        if stream=='inputout':
            if outname is None: outname='wrfinputout_d<domain>'
            self.nl.nl_set('time_control','write_input',True)
        if outname is not None: self.set_outname(stream,outname)
        if start is not None: start=to_datetime_rel(start,self._simstart)
        nc=self.get_nocolons()
        for mydom in self:
            mydom.nocolons=nc
            if domain is not None and domain!=mydom and \
                    not mydom.has_output(stream):
                mydom.no_output(stream)
            else:
                mydom.add_output(
                    stream,start,end,step,outname,frames_per_outfile,
                    io_form,simstart=self._simstart)
        if stream=='restart':
            self.make_restart_time_scalar()

        return self
    def make_restart_time_scalar(self):
        for domain in self:
            for n in [ 'restart_begin', 'restart_begin_s', 'restart_begin_m',
                       'restart_begin_h', 'restart_begin_d',
                       'restart_interval', 'restart_interval_s',
                       'restart_interval_m', 'restart_interval_h',
                       'restart_interval_d' ]:
                if domain.nl.nl_have('time_control',n):
                    v=domain.nl.nl_get('time_control',n)
                    domain.nl.nl_del('time_control',n)
                    self.nl.nl_set('time_control',n,v)
    def set_outname(self,stream,outname):
        """Sets the WRF output filename format for the specified
        stream to the given name.  The name should contain <domain>
        and <time> if appropriate."""
        if stream=='inputout': stream='input'
        self.nl.nl_set('time_control','%s_outname'
                       %(str(stream),),str(outname))
    def set_io_form(self,stream,io_form):
        """Sets the io_form for the specified stream."""
        if stream=='history': stream='output'
        self.nl.nl_set('time_control','io_form_%s'%(stream),int(io_form))
        self.nl.trait_set('io_form_%s'%(stream),int(io_form))
    def get_io_suffix(self,stream='history'):
        """Gets the suggested output suffix for the specified stream.
        Returns "int" for 1 and "nc" for 2 or 11."""
        iof=self.io_form_for(stream)%100
        if iof == 1:
            ios = "int"
        elif iof == 2 or iof==11:
            ios = "nc"
        else:
            raise NameError("Unsupported IO form %d" %self_.io_form)
        return ios
    def get(self,what):
        """Return the specified domain.  If the parameter is a string
        or WRFDomain, then the domain with that name is returned.  If
        the paremeter is an integer, the domain with that ID is
        returned.  This method may raise IndexError or KeyError if the
        domain does not exist."""
        if isinstance(what,WRFDomainBase): return self._grid[str(what.name)]
        if isinstance(what,basestring): return self._grid[str(what)]
        if isinstance(what,int): return self._grid[self._to_name[what]]
        raise KeyError('In WRF.get, the key must be a basestring, '
                       'WRFDomain or an int (or subclass thereof).  You '
                       'provided %s.'%(repr(what),))
    def maxdom(self):
        """Returns the highest domain number, which is also the number
        of domains."""
        return self._nextid-1
    def can_add(self):
        """Returns true if this WRF can accept new domains.  Certain
        operations, such as requesting output files, end the ability
        to add domains."""
        return not self._domans_done
    def add(self,child,parent=None):
        """Adds the child WRFDomain to this WRF, with the specified
        parent.  If the parent is not specified, the last added domain
        is used as the parent.  If specified, the parent may be
        anything accepted by self.get.  The return value is the new
        WRFDomain."""

        if self._domains_done:
            raise DomainsDone(
                'You cannot add any domains after setting up I/O for a '
                'WRF object.')

        # Get the settings for the new domain, plus some related inputs:
        newid=self._nextid
        newname=str(child.name)
        newparent=self.get_last() if parent is None else self.get(parent)
        parentname=str(newparent.name)
        moad=self.get_moad()
        if newname in self._grid:
            raise DomainExists('%s: domain already exists'%(newname,),newname)

        # Initialize the new domain:
        mine=child.copy()
        mine.nl.trait_set('id',newid)
        mine.nocolons=self.get_nocolons()
        mine.remove_forbidden()
        mine.init_as_nest(newparent,newid,self._simstart,self._simend)

        # Add the new domain.  These assignments must not fail or the
        # object will be corrupted, but it should not be possible to
        # get a failure here:
        self._grid[newname]=mine
        self._to_id[newname]=newid
        self._to_name[newid]=newname
        self._parent[newname]=parentname
        self._nextid=newid+1

        self.nl.nl_set('domains','max_dom',int(self.maxdom()))

        return self
    # Special methods to allow wrf[domain], len(wrf), domain in wrf,
    # and for domain in wrf
    def __getitem__(self,what): 
        """Same as self.get(what)"""
        return self.get(what)
    def __len__(self,what): 
        """Same as self.maxdom()."""
        return self.maxdom()
    def __contains__(self,what):
        """Returns True if self.get(what) succeeds, and False if it
        raises KeyError.  Any other exceptions are passed to the
        caller."""
        try:
            self.get(what)
            return True
        except KeyError: pass
        return False
    def __iter__(self):
        """Iterates over all WRFDomain objects in this WRFDomains."""
        for grid_id in xrange(1,self._nextid):
            yield self._grid[self._to_name[grid_id]]
