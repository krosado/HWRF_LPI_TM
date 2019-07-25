"""This module implements various simple numerical algorithms, such as
partial sorting, time manipulation or fraction-to-date conversions.
It also contains two array-like classes that take datetime objects as
indices."""

__all__ = [ 'partial_ordering','fcst_hr_min','split_fraction','to_fraction',
            'to_datetime_rel','to_datetime','to_timedelta','TimeArray',
            'minutes_seconds_rest','nearest_datetime','is_at_timestep',
            'great_arc_dist', 'timedelta_epsilon', 'TimeMapping',
            'within_dt_epsilon', 'randint_zeromean']

import fractions,math,datetime,re,random
import hwrf.exceptions

########################################################################

class partial_ordering(object):
    """This class is a drop-in replacement for cmp in sorting
    routines.  It represents a partial ordering of objects by
    specifying the order of a known subset of those objects, and
    inserting all unknown objects in a specified location in the that
    list (at the end, by default) in an order determined by cmp(a,b).
    Example:

    p=partial_ordering([3,2,1]) # list is ordered as [3,2,1] with
                                #  everything else after that

    sorted([0,1,2,3,6,4,5],p)  # = [3, 2, 1, 0, 4, 5, 6]
    p(1,-99)  # = -1, so -99 goes after 1 since -99 is not
              #   in the partial ordering
    p(1,3)    # = 1, so 3 goes before 1 since 3 is before 1
              #   in the partial ordering
    p(5,10)   # = -1 since cmp(5,10)=-1"""
    def __init__(self,ordering,unordered=None,backupcmp=cmp):
        """Creates a partial ordering.  The subset that is ordered is
        specified by the ordered iterable "ordered" while the index at
        which to place unordered values is optionally specified by
        "unordered", which can be anything that can be cmp()'ed to an
        int.  If "unordered" is missing, then all objects not in
        "ordered" will be placed at the end of any list.  To place at
        the beginning of the list, give unordered=0.  To insert
        between the first and second elements, specify 1, between
        second and third elements: specify unordered=2, and so on.
        Specify another tiebreaker "cmp" function with "backupcmp"
        (default: cmp)."""
        self.order=dict()
        self.backupcmp=backupcmp
        if unordered is None:
            self.unordered=float('inf') # index of the location to
                                        # store unordered elements
        else:
            self.unordered=unordered
        #  self.unordered must NOT cmp() compare to 0 against any
        #  possible index
        i=0
        for obj in ordering:
            while cmp(i,self.unordered)==0:
                i+=1
            self.order[obj]=i
            i+=1
    def __call__(self,a,b):
        ia = self.order[a] if a in self.order else self.unordered
        ib = self.order[b] if b in self.order else self.unordered
        c=cmp(ia,ib)
        if c==0:
            c=self.backupcmp(a,b)
        return c

########################################################################

def great_arc_dist(xlon1,ylat1, xlon2,ylat2):
    """Calculates the great arc distance in meters between two points
    using the Haversine method.  Uses the local Earth radius at the
    latiude half-way between the two points."""
    deg2rad=math.pi/180.0
    Requator=6378137.0
    flattening_inv=298.247

    rlat1=float(ylat1)*deg2rad
    rlon1=float(xlon1)*deg2rad
    rlat2=float(ylat2)*deg2rad
    rlon2=float(xlon2)*deg2rad

    Rearth1=Requator*(1.0-math.sin(rlat1)**2.0/flattening_inv)
    Rearth2=Requator*(1.0-math.sin(rlat2)**2.0/flattening_inv)

    return (Rearth1+Rearth2)*math.asin(min(1.0,math.sqrt( \
        math.sin((rlat1-rlat2)/2.0)**2.0 + \
        math.cos(rlat1)*math.cos(rlat2)*math.sin((rlon1-rlon2)/2.0)**2.0)))

########################################################################

def fcst_hr_min(time,start):
    """Given a forecast datetime.datetime and an analysis
    datetime.datetime, this returns a tuple containing the forecast
    hour and minute, rounded to the nearest integer minute."""
    dt=time - start # forecast time
        # Convert to hours and minutes, round to nearest minute:
    fhours=dt.days*24 + dt.seconds/3600 + dt.microseconds/3600e6
    (fpart,ihours)=math.modf(fhours)
    if fpart>1.:
        assert(fpart<1.)
    iminutes=round(fpart*60)
    return (ihours,iminutes)

########################################################################

def randint_zeromean(count,imax,randomizer=None):
    """Generates "count" numbers uniformly distributed between -imax
    and imax, inclusive, with a mean of zero."""
    imax=abs(int(imax))
    count=int(count)
    if randomizer is None:
        randint=random.randint
    else:
        randint=randomizer.randint
    if imax!=0 and not ( -imax < imax):
        # Integer overflow
        raise OverflowError(
            'In randint_zeromean, imax=%d cannot be negated and fit '
            'within a Python int.'%imax)
    rand=[ randint(-imax,imax) for x in xrange(count) ]
    cen=sum(rand)
    while cen!=0:
        if cen>0:
            while True:
                icen=randint(0,count-1)
                if rand[icen]>-imax:
                    rand[icen]-=1
                    break
        elif cen<0:
            while True:
                icen=randint(0,count-1)
                if rand[icen]<imax:
                    rand[icen]+=1
                    break
        cen=sum(rand)
    assert(sum(rand)==0)
    return rand

########################################################################

def split_fraction(f):
    """Splits a fraction.Fraction into integer, numerator and
    denominator parts.  For example, split_fraction(Fraction(13,7))
    will return (1,6,7) since 1+6/7=13/7."""
    i=int(f)
    f2=f-i
    return (i,int(f2.numerator),int(f2.denominator))

########################################################################

def within_dt_epsilon(time1,time2,epsilon):
    """Returns True if time1 is within epsilon of time2, and False
    otherwise."""
    if not isinstance(time2,datetime.datetime):
        dt=to_fraction(time2)
    else:
        if not isinstance(time1,datetime.datetime): 
            time1=to_datetime(time1)
        dt=time2-time1
        dt=fractions.Fraction(dt.microseconds,1000000)+\
            dt.seconds+24*3600*dt.days
    if not isinstance(epsilon,fractions.Fraction):
        epsilon=to_fraction(epsilon)
    return abs(dt) < abs(epsilon)

########################################################################

def timedelta_epsilon(times,rel=None,default=None,sort=False,numerator=10):
    """Given an iterable of datetime objects (or anything accepted by
    to_datetime_rel), computes the minimum time difference between any
    two adjacent times and divides it by "numerator" (default: 10).
    The "rel" argument is the relative time when calling
    to_datetime_rel.  If unspecified, it will be the first time seen
    (in which case that time must be acceptable to to_datetime).  The
    "default" is the return value when all elements in "times" are
    identical, or when there is only one element.  If the default is
    unspecified and it is needed, then NoTimespan is raised.  If sort
    is specified and True, then the times will be sorted before
    anything is done (it is False by default)."""
    if sort: times=sorted(times)
    if rel is not None: rel=to_timedelta(rel)
    mindiff=None
    prior=None
    numerator=int(numerator)
    for time in times:
        if rel is None:
            now=to_datetime(time)
            rel=now
        else:
            now=to_datetime_rel(time,rel)
        if prior is not None:
            diff=abs(to_fraction(now-prior,negok=True) / numerator)
            if mindiff is None or diff<mindiff and diff>0: mindiff=diff
        prior=now
    if mindiff is None:
        if default is None: 
            raise hwrf.exceptions.NoTimespan(
                'Too few non-identical times in input, and no default '
                'specified.  Cannot compute timespan in timedelta_epsilon.',
                start=rel,end=None)
        return to_timedelta(default)
    return to_timedelta(mindiff)
   
########################################################################

def to_fraction(a,b=None,negok=False):
    """This routine is a wrapper around fraction.Fraction() which
    accepts additional inputs.  Fractions are needed to provide higher
    precision in time and resolution calculations (and also to match
    the WRF behavior).  The arguments are the same as for the
    fractions.Fraction constructor, but additional calling conventions
    are accepted: a single float argument, a single datetime.timedelta
    object, or a string containing an integer and a fraction.  If a
    float is provided, its denominator is restricted to be no more
    than 1000000.  If the resulting fraction is not larger than 0,
    then InvalidTimestep is thrown unless negok=True.

    Examples:
        to_fraction(0.855)            # 0.855 seconds
        to_fraction(33)               # 33 seconds
        to_fraction('12/5')           # 2.4 seconds
        to_fraction('7+1/2')          # 7.5 seconds
        to_fraction(0.0000001) # ERROR! Value of the float() is less
                               # than 1e-6
        to_fraction(1,10000000) # Valid as a fraction, even though value
                                # is less than 1e-6"""
    result=None
    if b is not None:
        result=fractions.Fraction(a,b)
    elif isinstance(a,datetime.timedelta):
        result=fractions.Fraction(a.microseconds,1000000)+\
            a.seconds+24*3600*a.days
    elif isinstance(a,basestring): # Catch the 1+3/7 syntax:
        m=re.match('\s*(?P<ipart>[+-]?\d+)\s*(?P<num>[+-]\d+)\s*/\s*(?P<den>\d+)',a)
        if(m):
            (i,n,d)=m.groups(['ipart','num','den'])
            result=fractions.Fraction(int(i)*int(d)+int(n),int(d))
    elif isinstance(a,float):
        result=fractions.Fraction.from_float(a).limit_denominator(1000000)
    if result is None:
        result=fractions.Fraction(a)
    if not negok and not result>=0: # avoid < in case of NaN
        raise hwrf.exceptions.InvalidTimestep \
            ('to_fraction(%s,%s) resulted in a negative timestep' \
                 % (repr(a),repr(b)))
    return result

########################################################################

def to_datetime_rel(d,rel):
    """Given a datetime object "rel", converts "d" to a datetime.
    Object "d" can be anything accepted by to_datetime, or anything
    accepted as a single argument by to_timedelta.  If it is a
    timedelta, it is added to "rel" to get the final time."""
    if not isinstance(rel,datetime.datetime):
        rel=to_datetime(rel)
    if isinstance(d,datetime.datetime):
        return d
    elif isinstance(d,datetime.timedelta):
        return rel+d
    elif isinstance(d,basestring):
        if(re.match('\A(?:\d\d\d\d-\d\d-\d\d \d\d:\d\d:\d\d|\d{10}|\d{12})\Z',d)):
            if   len(d)==10:
                return datetime.datetime.strptime(d,'%Y%m%d%H')
            elif len(d)==12:
                return datetime.datetime.strptime(d,'%Y%m%d%H%M')
            else:
                return datetime.datetime.strptime(d,'%Y-%m-%d %H:%M:%S')
    return rel+to_timedelta(d)

########################################################################

def to_datetime(d):
    """Converts the argument to a datetime.  If the argument is
    already a datetime, it is simply returned.  Otherwise it must be a
    string of the format YYYYMMDDHH, YYYYMMDDHHMM, or YYYY-MM-DD
    HH:MM:SS."""
    if isinstance(d,int): d=str(d)
    if isinstance(d,datetime.datetime):
        return d
    elif isinstance(d,basestring):
        if len(d)==10:
            return datetime.datetime.strptime(d,'%Y%m%d%H')
        elif len(d)==12:
            return datetime.datetime.strptime(d,'%Y%m%d%H%M')
        else:
            return datetime.datetime.strptime(d,'%Y-%m-%d %H:%M:%S')
    else:
        raise ValueError('Invalid value %s for to_datetime.  Parameter '
                         'must be a datetime or a string that matches '
                         'YYYY-MM-DD HH:MM:SS.'%(repr(d),))

########################################################################

def to_timedelta(a,b=None,negok=True):
    """Returns a datetime.timedelta object.  If "a" is a timedelta,
    then that value is returned.  If "a" is a string of the format
    08:13 or 08:13:12, optionally preceeded by a "-" then it is
    interpeted as HH:MM or HH:MM:SS, respectively (where a "-"
    negates).  Otherwise, the arguments are sent to to_fraction, and
    the result is converted into a timedelta."""
    if isinstance(a,datetime.timedelta): return a
    if isinstance(a,basestring) and b is None:
        # 03:14 = three hours
        try:
            m=re.search('''(?ix) \A \s* (?P<negative>-)? 0* (?P<hours>\d+)
                :0*(?P<minutes>\d+)
                  (?: :0*(?P<seconds>\d+(?:\.\d*)?) )?
                \s*''', a)
            if m:
                (hours,minutes,seconds)=(0.,0.,0.)
                mdict=m.groupdict()
                if 'hours' in mdict and mdict['hours'] is not None:
                    hours=float(mdict['hours'])
                if 'minutes' in mdict and mdict['minutes'] is not None:
                    minutes=float(mdict['minutes'])
                if 'seconds' in  mdict and mdict['seconds'] is not None:
                    seconds=float(mdict['seconds'])
                dt=datetime.timedelta(hours=hours,minutes=minutes,
                                      seconds=seconds)
                if 'negative' in mdict and mdict['negative'] is not None \
                        and mdict['negative']=='-':
                    return -dt
                return dt
        except(TypeError,ValueError,AttributeError) as e:
            # Fall back to to_fraction
            raise #FIXME: remove this line?  Why is this here?  It
                  #conflicts with the description.
            pass
    f=to_fraction(a,b,negok=negok)
    (fpart,ipart)=math.modf(f)
    return datetime.timedelta(seconds=ipart,
                              microseconds=int(round(fpart*1e6)))

########################################################################

def minutes_seconds_rest(fraction):
    """Splits the given fractions.Fraction of seconds into integer
    minutes, seconds and fractional remainder <=0, returning them as a
    three-element tuple"""
    fraction=to_fraction(fraction)
    minutes=int(fraction/60)
    seconds=int(fraction-minutes*60)
    rest=fraction-minutes*60-seconds
    return (minutes,seconds,rest)

########################################################################

def nearest_datetime(start,target,timestep):
    """Given a start time, a target time and a timestep, determine the
    nearest time not earlier than the target that lies exactly on a
    timestep.  Input start and target can be anything understood by
    to_datetime, and the timestep can be anything understood by
    to_fraction.  Return value is a datetime.datetime object."""
    dstart=to_datetime(start)
    dtarget=to_datetime(target)
    dt=to_fraction(timestep)
    how_many_dt=int(math.ceil(to_fraction(dtarget-dstart)/dt))
    return to_datetime_rel(math.floor(how_many_dt*dt),dstart)

########################################################################

def is_at_timestep(start,target,timestep):
    """Returns True if the target time lies exactly on a timestep, and
    False otherwise."""
    span=to_fraction(to_datetime(target)-to_datetime(start))
    timesteps=span / to_fraction(timestep)
    return timesteps-int(timesteps)<1e-5

########################################################################

def str_timedelta(dt):
    """Converts a timedelta to a string of the format DD:HH:MM:SS+num/den
       DD - number of days
       HH - number of hours
       MM - minutes
       SS - seconds
       num/den - fractional part
    The to_fraction is used to get the fractional part."""
    fdt=to_fraction(to_timedelta(dt))
    neg=fdt<0
    if neg: fdt=-fdt
    (i,n,d) = split_fraction(fdt)
    seconds=int(i)%60
    minutes=(int(i)/60)%60
    hours=int(i)/3600
    out='%02d:%02d:%02d'%(hours,minutes,seconds)
    if n!=0:
        out='%s+%d/%d'%(out,n,d)
    return out

########################################################################

class TimeContainer(object):
    """This is the abstract base class of any class that represents a
    mapping from a set of times to a set of data.  It provides the
    underlying implementation of TimeArray and TimeMapping.  This is
    not exported by "from hwrf.numerics import *" to prevent
    accidental use."""
    # Implementation notes:
    def __init__(self,times,init=None):
        """Initializes the internal arrays for the given list of
        times, which must not be empty.  Note that strange,
        potentially bad things will happen if there are duplicate
        times.

        Implementation notes:

          self._times[N]: a list of times
          self._data[N]: the data for each time
          self._assigned[N]: True if there is data for this time,
            False otherwise

        where N is the length of the input times array.  The _data
        will be filled with init() and _assigned filled with True if
        init is present and not None, otherwise _assigned will be
        False."""
        self._times=times
        N=len(self._times)
        if init is None:
            self._assigned=[False]*N
            self._data=[None]*N
        else:
            self._assigned=[True]*N
            self._data=[init() for x in xrange(N)]
    def at_index(self,index):
        """Returns the data at the given index, or raises KeyError if
        no data exists."""
        if not self._assigned[index]:         raise KeyError(index)
        return self._data[index]
    def index_of(self,when):
        """This virtual function should be implemented in a subclass.
        It should return the index of the time to use for the given
        time."""
        raise NotImplementedError(
            'TimeContainer.index_of is not implemented.')
    @property
    def lasttime(self):
        """Returns the last time in the array or list of times, even
        if it has no data."""
        return self._times[-1]
    @property
    def firsttime(self):
        """Returns the first time in the array or list of times, even
        if it has no data."""
        return self._times[0]
    def __getitem__(self,when):
        """Returns the item at the latest time that is not later than
        "when".  If there is no data assigned at that time, then
        KeyError is raised."""
        (iwhen,index)=self.index_of(when)
        if not self._assigned[index]:
            raise KeyError(when)
        return self._data[index]
    def neartime(self,when,epsilon=None):
        """Returns a tuple containing the time nearest to "when"
        without going over, and the index of that time.  If epsilon
        is specified, raise NoNearbyValues if no times are near that
        value."""
        (then,index)=self.index_of(when)
        if epsilon is not None:
            if abs(to_fraction(then-when,negok=True)) \
                    <= to_fraction(epsilon):
                return then
            else:
                raise hwrf.exceptions.NoNearbyValues(
                    '%s: nearest value not after is %s, which is not '
                    'within %s seconds.'%(str(when),str(then),
                                          str(to_fraction(epsilon))))
        else:
            return then
    def get(self,when,default):
        """Returns the item at the latest time that is not later than
        "when."  If there is no data assigned at that time then the
        given default is returned."""
        (when,index)=self.index_of(to_datetime(when))
        if not self._assigned[index]:
            return default
        return self._data[index]
    def __setitem__(self,when,val):
        """Finds the latest time that is not later than "when" in
        self._times.  Assigns "val" to that time."""
        (when,index)=self.index_of(when)
        try:
            return self._data.__setitem__(index,val)
        finally:
            self._assigned[index]=True
    def __iter__(self):
        """Iterates over all data."""
        for i in xrange(len(self._times)):
            if self._assigned[i]:
                yield self._data[i]
    def itervalues(self):
        """Iterates over all known times that have data."""
        for i in xrange(len(self._times)):
            if self._assigned[i]:
                yield self._data[i]
    def iterkeys(self):
        """Iterates over all times that have data."""
        for i in xrange(len(self._times)):
            if self._assigned[i]:
                yield self._times[i]
    def iteritems(self):
        """Iterates over all known times that have data, returning a
        tuple containg the time and the data at that time."""
        for i in xrange(len(self._times)):
            if self._assigned[i]:
                yield self._times[i],self._data[i]
    def __reversed__(self):
        """Iterates over all known times that have data, in reverse
        order."""
        n=len(self._times)
        for i in xrange(n):
            j=n-i-1
            if self._assigned[j]:
                yield self._data[j]
    def __delitem__(self,when):
        """Finds the latest time that is not later than "when" and
        removes the data it is mapped to.  Later calls to __getitem__,
        get, __iter__ and so on, will not return any data for this
        time."""
        (when,index)=self.index_of(when)
        self._assigned[index]=False
        self._data[index]=None
    def __contains__(self,when):
        """Finds the latest time that is not later than "when" in
        self._times.  Returns True if there is data mapped to that
        time and False otherwise."""
        try:
            (iwhen,index)=self.index_of(when)
            return self._assigned[index]
        except hwrf.exceptions.NotInTimespan,KeyError:
            return False
    def __len__(self):
        """Returns the number of times that have data."""
        return len(self._data)
    def times(self):
        """Iterates over all times in this TimeContainer"""
        for t in self._times:
            yield t
    def datatimes(self):
        """Iterates over all times in this TimeContainer that map to data."""
        for i in xrange(len(self._times)):
            if self._assigned[i]:
                yield self._times[i]
    def __str__(self):
        def idxstr(i): 
            t=self._times[i]
            st=t.strftime("%Y%m%d-%H%M%S")
            if self._assigned[i]:
                return "%s:%s"%(st,repr(self._data[i]))
            else:
                return "%s:(unassigned)"%(st,)
        contents=", ".join([ idxstr(i) for i in xrange(len(self._times)) ])
        return "%s(%s)"% ( self.__class__.__name__, contents )
    def datatimes_reversed(self):
        """Iterates in reverse order over all times in this
        TimeContainer that map to data."""
        N=len(self._times)
        for i in xrange(N):
            j=N-i-1
            if self._assigned[j]:
                yield self._times[j]

########################################################################

class TimeArray(TimeContainer):
    """This implements an array-like object that uses time as an
    index.  It recognizes only discrete times as specified by a start,
    an end and a timestep.  The end is only used as a guideline: if it
    does not lie exactly on a timestep, the next timestep end is used.
    Note that all methods in this class have the same meaning as the
    built-in list class, but accept times as input, except if
    specified otherwise.  In all cases, the time "index" is anything
    accepted by to_datetime_rel(...,self.start)."""
    def __init__(self,start,end,timestep,init=None):
        self._start=to_datetime(start)
        self._end=to_datetime_rel(end,self._start)
        dt=to_fraction(timestep)
        self._timestep=dt
        n=int(to_fraction(self._end-self._start)/self._timestep)+1
        TimeContainer.__init__(self,[ self._start+to_timedelta(i*dt) for i in xrange(n) ],init=init)
        # Public accessors.  Presently these are just data values, not properties:
        self.start=self._start
        self.end=self._end
        self.timestep=dt
    def index_of(self,when):
        """Returns the index of the specified time in the internal
        storage arrays or raises NotInTimespan if the time is not in
        the timespan.  The argument can be anything accepted by
        to_datetime_rel(when,self._start)"""
        when=to_datetime_rel(when,self._start)
        index=int(to_fraction(when-self._start)/self._timestep)
        if index<0 or index>=len(self._data):
            raise hwrf.exceptions.NotInTimespan \
                ('%s: not in range [%s,%s]' % (when.ctime(),
                    self._start.ctime(),self._end.ctime()) )
        return (self._times[index],index)
    # def __iter__(self):
    #     """Iterates over all known times."""
    #     for i in xrange(len(self._times)):
    #         yield self._times[i]

########################################################################

class TimeMapping(TimeContainer):
    """A TimeMapping is a mapping from an ordered list of times to a
    set of data.  The full set of possible times is given in the
    constructor: it is not possible to add new times to the mapping
    after creation of the TimeMapping.  A TimeMapping is more
    expensive than a TimeArray since every [] lookup (__getitem__) has
    to do a binary search.  However, it is more general since the
    times do not have to be exactly at an interval.

    Note that a TimeArray can replace a TimeMapping if a small enough
    timestep (the greatest common factor) is used since most of the
    TimeContainer methods only work on the indices that have data.
    However, if some timespans have dense timesteps and the rest are
    sparse, there may be significant memory and CPU savings in using a
    TimeMapping."""
    def __init__(self,times,init=None):
        times=sorted([ to_datetime(x) for x in set(times)])
        TimeContainer.__init__(self,times,init=init)
    def index_of(self,when):
        when=to_datetime(when)
        N=len(self._times)
        if N==0:
            raise hwrf.exceptions.NotInTimespan(
                'TimeMapping is empty: no data is in an empty timespan.')

        # Get the bounding times:
        iearly=0
        early=self._times[iearly]
        ilate=N-1
        late=self._times[ilate]

        # See if we're outside the bounding times:
        if when<early:
            raise hwrf.exceptions.NotInTimespan(
                'Time %s is not earlier than first time (%s) in TimeMapping'
                % ( str(when),str(early) ) )
        elif when>=late:
            return ( self._times[ilate], ilate )

        # Binary search for the bounding indices:
        while ilate>iearly+1:
            imiddle=(iearly+ilate)/2
            middle=self._times[imiddle]
            if when<middle:
                (ilate,late)=(imiddle,middle)
            else:
                (iearly,early)=(imiddle,middle)
        return ( self._times[iearly], iearly )
