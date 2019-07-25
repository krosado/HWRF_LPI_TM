__all__=['Cluster','where','longname','name','group_quotas','acl_support',
         'no_access_control','use_acl_for_rstdata','ncepprod',
         'NOAAJet','NOAAGAEA','NOAAZeus','NOAAWCOSS']

import time, socket, os

DO_NOT_SET=object()

class Cluster(object):
    """Stores information about a computer cluster.  The following
    attributes or public member variables are available:

    group_quotas - True if group membership is used to manage disk
    quotas.  If this is True, then the scripts should never copy the
    group ID when copying files.

    acl_support - True if the system uses Access Control Lists (ACLs)
    to control access to files.  

    use_acl_for_rstdata - True if the scripts should use ACLs to
    protect restricted access data.  If this is True, the scripts
    should copy ACLs when copying files.  The produtil.acl supplies a
    way to do that on some Linux machines.

    production - True if this system is production (real-time
    forecasting) environment, and False otherwise.  Most systems
    should set this to False.  

    name - a short name of this cluster.  Must be a valid Python
    identifier string.

    longname - a long name of this cluster."""
    def __init__(self,group_quotas,acl_support,production,name,longname,
                 use_acl_for_rstdata=None):
        """Sets all public member variables.  All are mandatory except
        use_acl_for_rstdata.  The default for use_acl_for_Rstdata is
        the logical AND of group_quotas and acl_support."""
        self.group_quotas=bool(group_quotas)
        self.acl_support=bool(acl_support)
        if production is not DO_NOT_SET:
            self.production=bool(production)
        self.name=str(name)
        self.longname=str(longname)
        if use_acl_for_rstdata is None:
            use_acl_for_rstdata=self.group_quotas and self.acl_support
        self.use_acl_for_rstdata=use_acl_for_rstdata

here=None

def set_cluster(there):
    """Sets the current cluster (module-level "here" variable) to the
    given value.  Bad things may happen if this is not a subclass of
    Cluster."""
    global here
    here=there

def where():
    """Attempts to guess what cluster the program is running on, and
    if it cannot, returns a cluster named "noname" with reasonable
    defaults.  The result is stored in the module scope "here"
    variable."""
    global here
    if here is None:
        if os.path.exists('/pan2'):
            here=NOAAJet()
        elif os.path.exists('/glade'):
            here=UCARYellowstone()
        elif os.path.exists('/scratch3'):
            theia=False
            with open('/proc/cpuinfo','rt') as f:
                for line in f.readlines(1000):
                    if line.find('E5-2690')>=0:
                        theia=True
                        break
            if theia:
                here=NOAATheia()
            else:
                here=NOAAZeus()
        elif os.path.exists('/ptmpd2'):
            here=NOAAWCOSS()
        else:
            here=Cluster(False,False,False,'noname','noname')
    return here

def longname():
    """Synonym for here.longname.  Will call the "where()" function if
    "here" is uninitialized."""
    if here is None: where()
    return here.longname

def name():
    """Synonym for here.name.  Will call the "where()" function if
    "here" is uninitialized."""
    if here is None: where()
    return here.name

def group_quotas():
    """Synonym for here.group_quotas.  Will call the "where()" function if
    "here" is uninitialized."""
    if here is None: where()
    return here.group_quotas

def acl_support():
    """Synonym for here.acl_support.  Will call the "where()" function if
    "here" is uninitialized."""
    if here is None: where()
    return here.acl_support

def no_access_control():
    """True if the cluster provides no means to control access to
    files.  This is true if the cluster uses group ids for quotas, and
    provides no access control list support."""
    if here is None: where()
    return here.group_quotas and not here.use_acl_for_rstdata

def use_acl_for_rstdata():
    """Synonym for here.use_acl_for_rstdata.  Will call the "where()"
    function if "here" is uninitialized."""
    if here is None: where()
    return here.use_acl_for_rstdata

def ncepprod():
    """Returns True if the present machine is the NCEP production
    machine.  Note that this function may read a text file when it is
    called, and the return value may change during the execution of
    the program if the program is running during a production
    switch."""
    if here is None: where()
    return here.production and (  here.name=='tide' or here.name=='gyre'  )

class NOAAJet(Cluster):
    """Represents the NOAA Jet cluster, which has non-functional ACL
    support.  Will report that ACLs are supported, but should not be
    used.  Also, group quotas are in use.  That means that there is no
    means by which to restrict access control, so no_access_control()
    will return True."""
    def __init__(self):
        super(NOAAJet,self).__init__(True,True,False,'jet',
                                     'jet.rdhpcs.noaa.gov',False)

class NOAAGAEA(Cluster):
    """Represents the NOAA GAEA cluster.  Allows ACLs to be used for
    restricted data, and specifies that group quotas are in use."""
    def __init__(self):
        super(NOAAGAEA,self).__init__(True,True,False,'gaea',
                                      'gaea.rdhpcs.noaa.gov')

class NOAAZeus(Cluster):
    """Represents the NOAA Zeus cluster.  Allows ACLs to be used for
    restricted data, and specifies that group quotas are in use."""
    def __init__(self):
        super(NOAAZeus,self).__init__(True,True,False,'zeus',
                                      'zeus.rdhpcs.noaa.gov')

class UCARYellowstone(Cluster):
    """Represents the Yellowstone cluster.  Does not allow ACLs,
    assumes group quotas."""
    def __init__(self):
        super(UCARYellowstone,self).__init__(
            True,False,False,'yellowstone','yellowstone.ucar.edu')

class NOAATheia(Cluster):
    """Represents the NOAA Theia cluster.  Does not allow ACLs,
    assumes no group quotas (fileset quotas instead)."""
    def __init__(self):
        super(NOAATheia,self).__init__(
            False,False,False,'theia','theia.rdhpcs.noaa.gov')

class NOAAWCOSS(Cluster):
    """Represents the NOAA WCOSS clusters, Tide, Gyre and the test
    system Eddy.  Automatically determines which one the program is on
    based on the first letter of socket.gethostname().  Will report no
    ACL support, and no group quotas.  Hence, the cluster should use
    group IDs for access control.  

    The production accessor is no longer a public member variable: it
    is now a property, which may open the /etc/prod file.  The result
    of the self.production property is cached for up to
    prod_cache_time seconds.  That time can be specified in the
    constructor, and defaults to 30 seconds."""
    def __init__(self,prod_cache_time=30):
        """Creates a NOAAWCOSS object, and optionally specifies the
        time for which the result of self.production should be cached.
        Default: 30 seconds."""
        host1=socket.gethostname()[0:1]
        if host1=='t':          name='tide'
        elif host1=='g':        name='gyre'
        else:                   name='eddy'
        super(NOAAWCOSS,self).__init__(False,False,DO_NOT_SET,name,
                                       name+'.ncep.noaa.gov')
        self._production=None
        self._lastprod=0
        self._prod_cache_time=int(prod_cache_time)
    def uncache(self):
        """Clears the cached value of self.production so the next call
        will return up-to-date information."""
        self._production=None
        self._lastprod=0
    @property
    def production(self):
        """Is this the WCOSS production machine?  The return value may
        change during the execution of this program if a production
        switch happened.  In addition, the check requires opening and
        parsing the /etc/prod file, so the runtime is likely several
        milliseconds when the cache times out.  To force a refresh of
        this information, call self.uncache() first."""
        now=int(time.time())
        if self._production is None or now-self._lastprod>self._prod_cache_time:
            tide = self.name=='tide'
            gyre = self.name=='gyre'
            prod=False
            with open('/etc/prod','/rt') as f:
                for line in f:
                    if line.find('tide')>=0: prod=tide
                    if line.find('gyre')>=0: prod=gyre
            self._production=prod
            self._lastprod=int(time.time())
            return prod
        else:
            return self._production

