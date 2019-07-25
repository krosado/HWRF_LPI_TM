"""This module contains the HWRFTask class, a subclass of
produtil.datastore.Task intended to be the base class of any HWRF
task.  It provides logging services, easy access to a config file,
access to tcvitals, and extends the hwrf.config.HWRFConfig string
interpolation to include vitals information.  It also provides a
standard way of setting and querying the work and output directories
of a task, and whether the task should scrub its output."""

import re, os
import hwrf.numerics
import produtil.log
from produtil.datastore import Task

__all__=['HWRFTask']

UNSPECIFIED=object()
class HWRFTask(Task):
    def __init__(self,dstore,conf,section,taskname=None,workdir=None,
                 outdir=None,storminfo=UNSPECIFIED,taskvars=UNSPECIFIED,
                 **kwargs):
        """Creates an HWRF task.  Arguments are the same as for
        produtil.datastore.Task, except:

        conf - an HWRFConf object with configuration options for this Task
        section - the name of the section in conf to get options for
          this Task
        storminfo - either None, or a hwrf.StormInfo object containing
          information about the storm this Task is processing.  If
          None, then the internal storminfo data will be unset.  If
          unspecified, then conf.vitals will be used
        workdir - directory in which to run this task.  The value in
          the database will override this value.

        If taskname and datumid are both unspecified, then the section
        is used as the taskname."""
        if taskname is None:
            taskname=section
        conf.register_hwrf_task(taskname)
        self.__taskvars=dict()
        if taskvars is not UNSPECIFIED:
            for k,v in taskvars.iteritems():
                self.tvset(k,v)
        self._conf=conf
        self._section=str(section)
        self._storminfo=storminfo
        if taskname is not None and not isinstance(taskname,basestring):
            raise TypeError('The taskname must be None or a basestring '
                            'subclass')
        if not isinstance(section,basestring):
            raise TypeError('The section be a basestring subclass')
        if workdir is None:
            workdir=self.confstr('workdir','')
            if workdir is None or workdir=='':
                workdir=os.path.join(self.getdir('WORKhwrf'),taskname)
        if outdir is None:
            outdir=self.confstr('workdir','')
            if outdir is None or outdir=='':
                outdir=os.path.join(self.getdir('intercom'),taskname)
        with dstore.transaction():
            super(HWRFTask,self).__init__(dstore,taskname=taskname,
                                          logger=conf.log(taskname),**kwargs)
            mworkdir=self.meta('workdir','')
            moutdir=self.meta('outdir','')
            if mworkdir: 
                workdir=mworkdir
            else:
                self['workdir']=workdir
            if moutdir: 
                outdir=moutdir
            else:
                self['outdir']=outdir
        if storminfo is UNSPECIFIED:
            if hasattr(conf,'syndat'):
                self.storminfo=conf.syndat
        elif storminfo is not None:
            if isinstance(storminfo,basestring):
                self.storminfo=hwrf.storminfo.StormInfo()
                self.storminfo.update(conf.items('config'))
                self.storminfo.parse_vitals(storminfo)
            elif isinstance(storminfo,hwrf.storminfo.StormInfo):
                self.storminfo=storminfo.copy()
            else:
                raise TypeError("The storminfo argument to HWRFTask() must "
                                'be None, a string or '
                                'hwrf.storminfo.StormInfo')
    def get_workdir(self):
        """Returns the directory the class should work in, as set by
        the "workdir" metadata value."""
        workdir=self.meta('workdir','')
        if not workdir:
            workdir=os.path.join(self.getdir('WORKhwrf'),self.taskname)
        assert(workdir!='/')
        return workdir
    def set_workdir(self,val):
        """Sets the directory the class should work in.  This sets the
        "workdir" metadata value."""
        self['workdir']=str(val)
    workdir=property(get_workdir,set_workdir,None,
        """The directory in which this task should be run.""")
    
    def get_outdir(self):
        """Gets the directory that should receive output data.  This
        is in the "outdir" metadata value."""
        outdir=self.meta('outdir','')
        if not outdir:
            outdir=os.path.join(self.getdir('intercom'),self.taskname)
        assert(outdir!='/')
        return outdir
    def set_outdir(self,val):
        """Sets the directory that should receive output data.  Sets
        the "outdir" metadata value."""
        self['outdir']=str(val)
    outdir=property(get_outdir,set_outdir,None,
        """The directory to which this task should deliver its final output.""")

    @property
    def realtime(self):
        """Is this job a real-time forecast job?  This is different
        than self.conf.realtime: it allows this job's config section
        to locally override the value."""
        return self.confbool('realtime',True)

    @property
    def redirect(self):
        """Should subprograms' outputs be redirected to separate files?"""
        return self.confbool('redirect',False)

    @property
    def scrub(self):
        """Should temporary files be deleted as soon as they are not
        needed?"""
        return self.confbool('scrub',True)

    def tvset(self,opt,val):
        """Sets an object-local value for option "opt" to value "val".
        This will override config settings from the HWRFConfig object.
        These are sent into the taskvars= parameter to the various
        HWRFConfig member functions (hence the "tv" in "tvset")."""
        sopt=str(opt)
        if sopt[0:2]=='__':
            raise hwrf.exceptions.InvalidConfigOptName(
                '%s: invalid option name.  Cannot begin with __'%(sopt,))
        self.__taskvars[sopt]=val

    def tvdel(self,opt):
        """Deletes an object-local value set by tvset."""
        sopt=str(opt)
        del self.__taskvars[sopt]

    def tvget(self,opt):
        """Returns the value of an object-local option set by tvset."""
        sopt=str(opt)
        return self.__taskvars[sopt]

    def tvhave(self,opt=UNSPECIFIED):
        """If an option is specified, determines if the given option
        has an object-local value.  If no option is specified, returns
        True if ANY object-local values exist for any options."""
        if opt is UNSPECIFIED:
            return len(self.__taskvars)>0
        sopt=str(opt)
        return sopt in self.__taskvars

    @property
    def taskvars(self):
        return self.__taskvars

    def confint(self,opt,default=None,badtypeok=False,section=None,
                morevars=None):
        """Alias for self.conf.getint for section self.section."""
        if(section is None): section=self._section
        return self._conf.getint(section,opt,default,badtypeok,
                                 morevars=morevars,taskvars=self.__taskvars)
    def confstr(self,opt,default=None,badtypeok=False,section=None,
                morevars=None):
        """Alias for self.conf.getstr for section self.section."""
        if(section is None): section=self._section
        return self._conf.getstr(section,opt,default,badtypeok,
                                 morevars=morevars,taskvars=self.__taskvars)
    def conffloat(self,opt,default=None,badtypeok=False,section=None,
                  morevars=None):
        """Alias for self.conf.getfloat for section self.section."""
        if(section is None): section=self._section
        return self._conf.getfloat(section,opt,default,badtypeok,
                                   morevars=morevars,taskvars=self.__taskvars)
    def confbool(self,opt,default=None,badtypeok=False,section=None,
                 morevars=None):
        """Alias for self.conf.getbool for section self.section."""
        if(section is None): section=self._section
        return self._conf.getbool(section,opt,default,badtypeok,
                                  morevars=morevars,taskvars=self.__taskvars)
    def confget(self,opt,default=None,badtypeok=False,section=None,
                morevars=None):
        """Alias for self.conf.get for section self.section."""
        if(section is None): section=self._section
        return self._conf.get(section,opt,default,badtypeok,
                              morevars=morevars,taskvars=self.__taskvars)
    def confitems(self,section=None,morevars=None):
        """Alias for self.conf.items for section self.section."""
        if(section is None): section=self._section
        return self._conf.items(section,morevars=morevars,taskvars=self.__taskvars)

    def confstrinterp(self,string,section=None,**kwargs):
        """Alias for self.icstr for backward compatibility"""
        return self.icstr(string,section=section,**kwargs)

    def conftimestrinterp(self,string,ftime,atime=None,section=None,
                          **kwargs):
        """Alias for self.timestr for backward comaptibility"""
        return self.timestr(string,ftime,atime=atime,section=section,
                            taskvars=self.__taskvars,**kwargs)
    def confraw(self,opt,default=None,section=None):
        """Returns the raw, uninterpolated value for the specified
        option, raising an exception if that option is unset.  Will
        not search other sections, and will not search the taskvars,
        unlike other conf accessors."""
        if section is None: section=self.section
        return self._conf.getraw(section,opt,default)

    def icstr(self,string,section=None,**kwargs):
        """Expands a string in the given conf section.  Makes this
        objects tcvitals, if any, available via the "vit" variable
        while interpolating strings."""
        if(section is None): section=self._section
        if self.storminfo and 'vit' not in kwargs: 
            kwargs['vit']=self.storminfo.__dict__
        return self._conf.strinterp(section,string,taskvars=self.__taskvars,
                                    **kwargs)

    def timestr(self,string,ftime,atime=None,section=None,**kwargs):
        """Expands a string in the given conf section (default:
        self.section), and includes forecast and analysis time
        (default: conf.cycle) information in the variables that can be
        expanded.  The mandatory ftime argument is the forecast time
        which will be used to expand values such as fHH, fYMDH, etc.
        The optional atime will be used to expand aHH, aYMDH, etc.,
        and the two will be used together for forecast minus analysis
        fields like fahr.  See hwrf.config.timestrinterp for details

        As with self.icstr, this class's vitals are available via the
        "vit" variable while interpolating strings."""
        if(section is None): section=self._section
        if self.storminfo and 'vit' not in kwargs: 
            kwargs['vit']=self.storminfo.__dict__
        if 'taskvars' in kwargs:
            return self._conf.timestrinterp(section,string,ftime,atime,**kwargs)
        else:
            return self._conf.timestrinterp(section,string,ftime,atime,
                                            taskvars=self.__taskvars,**kwargs)

    def getdir(self,opt,default=None,morevars=None):
        """Alias for self.conf.get for the "dir" section."""
        return self._conf.get('dir',opt,default,morevars=morevars,
                              taskvars=self.__taskvars)
    def getexe(self,opt,default=None,morevars=None):
        """Alias for self.conf.get for the "exe" section."""
        return self._conf.get('exe',opt,default,morevars=morevars,
                              taskvars=self.__taskvars)
    def getconf(self):
        """Returns this HWRFTask's HWRFConfig object."""
        return self._conf
    conf=property(getconf,None,None,
                  """the HWRFConfig for this HWRFTask (read-only)""")
    def getsection(self):
        """Returns this HWRFTask's section name in the HWRFConfig."""
        return self._section
    section=property(getsection,None,None,
        """the confsection in self.section for this HWRFTask (read-only)""")
    def log(self,subdom=None):
        """Creates or returns a logging.Logger.  If subdom is None or
        unspecified, returns a cached logger for this task's logging
        domain.  Otherwise, returns a logger for the specified
        subdomain of this task's logging domain."""
        if subdom is None:
            return self._logger
        return self._conf.log(self.taskname+'.'+str(subdom))
    def inputiter(self):
        """Iterates over all inputs required by this task.  Iterates
        over dict-like objects suitable for input to
        hwrf.input.InputSource.get.  Each object contains the
        following keywords:
          dataset - string name of the dataset (gfs, gdas1, gefs,
            enkf, etc.)
          item - string name of the object (ie.: gfs_sf, gfs_sfcanl, bufr)
          atime - self.conf.cycle
          ftime - only present when relevant: the forecast time, in a 
            format accepted by to_datetime_rel
          enkfmem - only present when relevant: the ENKF member ID
          obstype - only present when relevant: the bufr data type.
        Other keywords may be present if needed.  They will be passed on
        by InputSource for string replacement."""  
        return
        yield {} # ensures this is an iterator
        
