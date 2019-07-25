"""This module handles reading and manipulating individual entries of
TCVitals files or CARQ entries of aid ("A deck") files.  It provides
each line as a StormInfo object with all available information
contained within."""

__all__=[ 'current_century', 'StormInfoError', 'InvalidBasinError', 
          'InvalidStormInfoLine', 'InvalidVitals', 'CenturyError', 
          'InvalidATCF', 'NoSuchVitals', 'name_number_okay',
          'basin_center_okay', 'vit_cmp_by_storm', 'vitcmp', 'storm_key',
          'clean_up_vitals', 'floatlatlon', 'quadrantinfo', 
          'parse_tcvitals', 'find_tcvitals_for', 'parse_carq', 'StormInfo',
          'expand_basin' ]

import re, datetime, math, fractions, logging, copy
import hwrf.numerics, hwrf.constants, hwrf.exceptions
from hwrf.exceptions import HWRFError
import pdb

current_century=int(datetime.datetime.now().year)/100
"""The first two digits of the year: the thousands and hundreds
digits.  This is used to convert message files to tcvitals."""

class StormInfoError(HWRFError):
    """This is the base class of all exceptions raised when errors are
    found in the tcvitals, Best Track, Aid Deck or other storm
    information databases."""

class InvalidBasinError(StormInfoError):
    """This exception is raised when an invalid Tropical Cyclone basin
    is found.  The invalid basin is available as self.basin, and the
    subbasin is self.subbasin (which might be None)."""
    def __init__(self,basin,subbasin=None):
        self.basin=basin
        self.subbasin=subbasin
    def __str__(self):
        if self.subbasin is None:
            return 'Invalid basin identifier %s'%(repr(self.basin),)
        else:
            return 'Invalid basin identifier %s (subbasin %s)' \
                %(repr(self.basin),repr(self.subbasin))
    def __repr__(self):
        return '%s(%s,%s)'%(type(self).__name__,repr(self.basin),
                            repr(self.subbasin))

class InvalidStormInfoLine(StormInfoError):
    """This exception is raised when the StormInfo class receives an
    invalid tcvitals line or ATCF line that it cannot parse.  The line
    is available as self.badline."""
    def __init__(self,message,badline):
        self.badline=badline
        super(InvalidStormInfoLine,self).__init__(message)
                            
class InvalidVitals(InvalidStormInfoLine): 
    """Raised when a syntax error is found in the tcvitals, and the
    code cannot guess what the operator intended."""
class CenturyError(InvalidStormInfoLine): 
    """Raised when an implausable century is found."""
class InvalidATCF(InvalidStormInfoLine): 
    """Raised when invalid ATCF data is found."""
class NoSuchVitals(StormInfoError):
    """This should be raised when the user requests a specific storm
    or cycle of a storm and no such vitals exists.  This module never
    raises this exception: this is meant to be raised by calling
    modules."""

def name_number_okay(vl):
    """Given an array of StormInfo objects, iterate over those that
    have valid names and numbers.  Discards TEST, UNKNOWN, and numbers
    50-89"""
    for vital in vl:
        if vital.stormname=='TEST' or vital.stormname=='UNKNOWN' or \
                (vital.stnum>50 and vital.stnum<90): 
            continue
        yield vital

def basin_center_okay(vl):
    """Given a list of StormInfo objects, iterates over those that
    have the right basins for the right centers.  A, B, W, S and P are
    allowed for JTWC, and E, L, C and Q are allowed for NHC.  Other
    entries are ignored.  Also discards North hemispheric basin storms
    that are in the south hemisphere and vice versa"""
    okay={ 'JTWC': 'ABWSPECQ', 'NHC': 'ELCQ' }
    for vital in vl:
        center=vital.center
        if not (center in okay and vital.basin1 in okay[center]): continue
        if vital.basin1 in 'SPQU':
            if vital.lat>0: continue  # should be in S hemisphere but is not
        else:
            if vital.lat<0: continue  # should be in N hemisphere but is not
        yield vital

def vit_cmp_by_storm(a,b):
    """A cmp comparison for StormInfo objects intended to be used with
    sorted().  This is intended to be used on cleaned vitals returned
    by clean_up_vitals.  For other purposes, use vitcmp.

    Uses the following method:
      1. Sort numerically by when.year
      2. Break ties by sorting lexically by stormid3
      3. Break ties by sorting by date/time
      4. Break ties by retaining original order ("stable sort")."""
    c=cmp(a.when.year,b.when.year)
    if c==0: c=cmp(a.longstormid,b.longstormid)
    if c==0: c=cmp(a.when,b.when)
    return c

def vitcmp(a,b):
    """A cmp comparison for StormInfo objects intended to be used with
    sorted().  Uses the following method:

      1. Sort numerically by date/time.
      2. Break ties by a reverse sort by stormid.  This places Invest 
         (90s) first.
      3. Break ties by ASCII lexical sort by center (ie.: JTWC first, 
         NHC second)
      4. Break ties by placing vitals WITH 34kt wind radii after those 
         without.
      5. Break ties by placing vitals with a full line (through 64kt 
         radii) last
      6. Break ties by retaining original order ("stable sort")."""
    c=cmp(a.when,b.when)
    if c==0: c=-cmp(a.stormid3,b.stormid3)
    if c==0: c=cmp(a.center,b.center)
    if c==0: c=cmp(a.have34kt,b.have34kt)
    return c

def storm_key(vit):
    """Lines are considered to be for the same database entry if
    they're from the same forecasting center, have the same basin,
    date/time and storm number.  Note that the public two-letter basin
    (AL, EP, CP, WP, IO, SH, SL) is used here (stormid4) so that a/b
    and s/p conflicts are handled by keeping the last entry in the
    file."""
    return (vit.center, vit.stormid4, vit.when)
                            
def clean_up_vitals(vitals,name_number_checker=None,basin_center_checker=None,vitals_cmp=None):
    """Given a list of StormInfo, sorts using the vitcmp comparison,
    discards suspect storm names and numbers as per name_number_okay,
    and discards invalid basin/center combinations as per
    basin_center_okay.  Lastly, loops over all lines keeping only the
    last line for each storm and analysis time.  The optional
    name_number_checker is a function or callable object that takes a
    StormInfo as an argument, and returns True if the name and number
    match some internal requirements.  By default, the
    name_number_okay function is used."""

    if name_number_checker is None:  
        name_number_checker=name_number_okay 
    if basin_center_checker is None:
        basin_center_checker=basin_center_okay
    if vitals_cmp is None:
        vitals_cmp=vitcmp

    # Sort vitals using above described method:
    sortvitals=sorted(vitals,cmp=vitals_cmp)
    vitals=sortvitals

    # Discard suspect storm names and numbers:
    nn_ok_vitals=[v for v in name_number_checker(vitals)]
    vitals=nn_ok_vitals

    # Discard wrong center/basin lines (ie.: NHC W basin, JTWC L basin)
    keepvitals=[ v for v in basin_center_checker(vitals) ]
    vitals=keepvitals

    # Remove duplicate entries keeping the last:
    revuniqvitals=list()
    seen=set()
    for vital in reversed(vitals):
        key=storm_key(vital)
        if key in seen: continue
        seen.add(key)
        revuniqvitals.append(vital)
    uniqvitals=[x for x in reversed(revuniqvitals)]
    return uniqvitals

def floatlatlon(string,fact=10.0):
    """floatlatlon(string="311N",fact=10.0) = float(31.1) in degrees
    North.  Note we do not allow negative numbers.  That means the
    tcvitals "badval" -999 or -99 or -9999 will result in a None
    return value."""
    m=re.search('\A(?P<num>0*\d+)(?:(?P<positive>[NnEe ])|(?P<negative>[SsWw]))\Z',string)
    if m:
        mdict=m.groupdict()
        latlon=float(mdict['num'])/fact
        if 'negative' in mdict and mdict['negative'] is not None: latlon=-latlon
        return latlon
    return None

def quadrantinfo(data,qset,irad,qcode,qdata,what='',conversion=1.0):
    """This is part of the internal implementation of StormInfo: it
    deals with parsing wind or sea quadrant information."""
    #print(repr(qdata))
    assert(len(qdata)==4)
    quadrant=['NNQ', 'NEQ', 'EEQ', 'SEQ', 'SSQ', 'SWQ', 'WWQ', 'NWQ']
    irad=int(irad)
    fqdata=[None]*4
    maxdata=0.
    for i in xrange(4):
        if qdata[i] is not None and qdata[i]!='':
            fqdata[i]=float(qdata[i])
            if fqdata[i]<0:
                fqdata[i]=-999
            else:
                fqdata[i]*=conversion
                maxdata=max(maxdata,fqdata[i])
    if qcode=='AAA':
        if count>0:
            var='%sCIRCLE%d'%(what,irad)
            data[var]=maxdata
            qset.add(var)
    elif qcode=='':
        return
    else:
        iquad=quadrant.index(qcode)
        for i in xrange(4):
            var='%s%s%d'%(what,quadrant[(iquad+2*i)%len(quadrant)][0:2],irad)
            data[var]=fqdata[i]
            qset.add(var)

def parse_tcvitals(fd,logger=None,raise_all=False):
    """This reads line by line from the given file object fd, parsing
    tcvitals.  It returns a list of StormInfo objects, one per line.
    If raise_all=True, exceptions will be raised immediately.
    Otherwise, any StormInfoError or ValueError will be logged or
    ignored, and parsing will continue.  The logger is a
    logging.Logger object in which to log messages."""
    out=list()
    for line in fd:
        try:
            out.append(StormInfo('tcvitals',line.rstrip('\n')))
        except (StormInfoError,ValueError) as e:
            if logger is not None:
                logger.warning(str(e))
            if raise_all: raise           
    return out

def find_tcvitals_for(fd,logger=None,raise_all=False,when=None,
                      stnum=None,basin1=None):
    """A fast method of finding tcvitals in a file: instead of parsing
    each line into a StormInfo, it simply scans the characters of the
    line trying to find the right storm and time.  Returns a list of
    matching vitals as StormInfo objects.

      fd - the stream-like object to read from
      logger - the logging.Logger to log to, or None
      raise_all - if True, exceptions are raised immediately instead 
        of just being logged.
      when - the date to look for, or None
      stnum - the storm number (ie.: 09 in 09L)
      basin1 - the basin letter (ie.: L in 09L)"""
    if(isinstance(stnum,basestring)): stnum=int(stnum)
    assert(not isinstance(stnum,basestring))
    if when is not None:
        strwhen=hwrf.numerics.to_datetime(when).strftime('%Y%m%d %H%M')
        abcd='abcd'
        assert(strwhen is not None)
        if(logger is not None):
            logger.debug('VITALS: search for when=%s'%(strwhen,))
    if stnum is not None:
        strstnum="%02d" % (stnum,)
        if(logger is not None):
            logger.debug('VITALS: search for stnum=%s'%(strstnum,))
    if basin1 is not None:
        strbasin1=str(basin1)[0].upper()
        if(logger is not None): 
            logger.debug('VITALS: search for basin1=%s'%(basin1,))
    #---------------------------------
    # JTWC 05P FREDA     20130101 0000
    # NHC  99L ABCDEFGHI 20991234 1200
    #---------------------------------
    # 00000000001111111111222222222233
    # 01234567890123456789012345678901
    #---------------------------------
    count=0
    for line in fd:
        if stnum is not None and line[5:7]!=strstnum: continue
        if when is not None and line[19:32]!=strwhen: continue
        if basin1 is not None and line[7]!=strbasin1: continue
        count+=1
        yield StormInfo('tcvitals',line.rstrip('\n'))
    if(logger is not None): 
        logger.debug('VITALS: yielded %d matches'%(count,))

def parse_carq(fd,logger=None,raise_all=True):
    """Scans an A deck file connected to stream-like object fd,
    reading it into a list of StormInfo objects.  Returns the list.
      logger - a logging.Logger to log to.
      raise_all - if False, log and ignore errors instead of raising them.
    """
    out=list()
    when=None
    store=list()
    for line in fd:
        if len(line)<40:  # blank or error line
            if logger is not None:
                logger.warning('Ignoring short line (<40 chars): %s'%(line,))
            continue
        elif 'CARQ' in line:
            when2=line[0:40].split(',')[2].strip()
            if when is None or when==when2:
                when=when2
                store.append(line)
                continue
        elif when is None:
            continue
        try:
            out.append(StormInfo(linetype='carq',inputs=store,logger=logger,
                                 raise_all=raise_all))
            store=list()
            when=None
        except(StormInfoError,ValueError) as e:
            if raise_all: raise
    return out

class StormInfo(object):
    """Represents all information about a one storm at one time as a
    Python object.  It can read a single line of a tcvitals file, one
    time from a Best Track file, or CARQ entries from an Aid Deck file
    at a single time.  It will scan multiple lines from a Best Track
    or CARQ group to get the last forecast hour (up to a maximum of
    72hrs) and all possible radii for one time.

    This class is meant for complex manipulations of a small amount of
    data, not for manipulations of whole databases.  It can be used
    for manipulating the entire TCVitals database, or multiple ATCF
    deck files, but will generally be slower than other libraries --
    operations will take on the order of ten times as long.

    TODO: write a separate class for simple manipulations of a whole
    ATCF database.  This could be done efficiently using an in-memory
    sqlite3 database."""
    def __init__(self,linetype,inputs,carq='CARQ',logger=None,raise_all=True):
        if logger is not None and not isinstance(logger,logging.Logger):
            raise TypeError(
                'In StormInfo constructor, logger must be a '
                'logging.Logger, but instead it is a %s'
                %(type(logger).__name__))
        super(StormInfo,self).__init__()
        self._cenlo=None
        self._cenla=None
        self.format=linetype
        self.has_old_stnum=False
        if   linetype=='tcvitals':
            self.line=str(inputs)
            self._parse_tcvitals_line(self.line)
        elif linetype=='message':  
            self.line=str(inputs)
            self._parse_message_line(self.line)
        elif linetype=='carq':   
            self.lines=copy.copy(inputs)
            self._parse_carq(lines=inputs,tech=str(carq),logger=logger,
                             raise_all=bool(raise_all))
        elif linetype=='old' or linetype=='copy':
            old=linetype=='old'
            def checktype(var):
                for t in ( basestring, int, float, datetime.datetime, 
                           datetime.timedelta ):
                    if isinstance(var,t): return True
                return False
            if not isinstance(inputs,StormInfo):
                raise TypeError(
                    'In StormInfo constructor, when linetype=="old", '
                    'inputs must be a StormInfo object, not a %s.'
                    %(type(inputs).__name__))
            for k,v in inputs.__dict__.iteritems():
                if k[0]=='_': continue
                if k[0:4]=='old_' and old: continue
                if not checktype(v): continue
                self.__dict__[k]=v
            if old:
                for k,v in inputs.__dict__.iteritems():
                    if not checktype(v): continue
                    if k[0:4]=='old_': self.__dict__[k[4:]]=v
        else:
            raise InvalidStormInfoFormat(
                'Unknown storm info format %s: only know "tcvitals" '
                'and "message".'%(repr(linetype),))
    def old(self):
        """Returns a copy of this StormInfo, but with the last
        renumbering or renaming of the vitals undone."""
        return StormInfo('old',self)
    def copy(self):
        """Returns a copy if this object."""
        return StormInfo('copy',self)
    def __sub__(self,amount): 
        """Same as self + (-amount)"""
        return self+ (-amount)
    def __add__(self,amount):
        """Returns a copy of this object, with the vitals extrapolated
        forward "amount" hours.  Only the location is changed."""
        copy=self.copy()
        dtamount=hwrf.numerics.to_timedelta(amount*3600)
        vmag=max(0,copy.stormspeed)*10.0
        pi180=math.pi/180.
        Rearth=hwrf.constants.Rearth
        dt=hwrf.numerics.to_fraction(dtamount,negok=True)
        dx=vmag*math.sin(copy.stormdir*pi180)
        dy=vmag*math.cos(copy.stormdir*pi180)
        dlat=float(dy*dt/Rearth)*2*math.pi
        dlon=float(dx*dt*math.cos(copy.lat*pi180)/Rearth)*2*math.pi
        copy.lat=round( float(copy.lat+dlat)*10 )/10.0
        copy.lon=round( float(copy.lon+dlon)*10 )/10.0
        copy.when=hwrf.numerics.to_datetime_rel(amount*3600,copy.when)
        copy.YMDH=copy.when.strftime('%Y%m%d%H')
        for v in ('flat','flon','fhr'):
            if v in copy.__dict__: del(copy.__dict__[v])
        copy.havefcstloc=False
        #logging.debug('vmag=%s dt=%s dx=%s dy=%s dlat=%s dlon=%s'%(
        #        repr(vmag),repr(dt),repr(dx),repr(dy),repr(dlat),repr(dlon)))
        return copy
    def hwrf_domain_center(self,logger=None):
        """Uses the 2013 operational HWRF method of deciding the
        domain center based on the storm location, basin, and, if
        available, the 72hr forecast location.  Returns a tuple
        containing a pair of floats (cenlo, cenla) which are the
        domain center longitude and latitude, respectively.  Results
        are cached internally so future calls will not have to
        recompute the center location."""

        if self._cenlo is not None and self._cenla is not None:
            return (self._cenlo,self._cenla)

        # This strange-looking sequence of comparisons and coercion to
        # irrelevant types mimics the weird combination of awk, bc and
        # ksh calculations used in the 2013 HWRF.  The latitude and
        # longitude cutoffs prevent crashes of the HWRF relocation
        # system when the storm approaches lateral boundaries, and
        # beyond that, they have also had a positive impact on track
        # forecasting skill.  

        storm_lon=self.lon
        assert(storm_lon is not None)
        storm_lat=self.lat
        if self.havefcstloc:
            assert(self.flon is not None)
            avglon=self.flon
        else:
            avglon=storm_lon-20.0
        assert(avglon is not None)

        # Decide center latitude.  
        cenla=storm_lat
        if storm_lat<0: cenla=-cenla
        ilat=math.floor(cenla)
        if ilat <  15: cenla=15.0
        if ilat >  25: cenla=25.0
        if ilat >= 35: cenla=30.0
        if ilat >= 40: cenla=35.0
        if ilat >= 44: cenla=40.0
        if ilat >= 50: cenla=45.0
        if ilat >= 55: cenla=50.0
        if storm_lat<0: cenla=-cenla

        # Decide the center longitude.
        if logger is not None:
            logger.info('Averaging storm_lon=%f and avglon=%f'%(storm_lon,avglon))
        diff=storm_lon-avglon
        if(diff> 360.):  storm_lon -= 360.0
        if(diff<-360.):  avglon -= 360.0
        result=int((10.0*storm_lon + 10.0*avglon)/2.0)/10.0
        if(result >  180.0): result-=360.0
        if(result < -180.0): result+=360.0
        cenlo=result
        if logger is not None:
            logger.info('Decided cenlo=%f cenla=%f'%(cenlo,cenla))
            logger.info('Storm is at lon=%f lat=%f'%(storm_lon,storm_lat))
        # Lastly, some sanity checks to avoid outer domain centers too
        # far from storm centers:
        moved=False
        if(int(cenlo)>int(storm_lon)+5):
            cenlo=storm_lon+5.0
            if logger is not None:
                logger.info(
                    'Center is too far east of storm.  Moving it to %f'
                    %(cenlo,))
            moved=True
        if(int(cenlo)<int(storm_lon)-5):
            cenlo=storm_lon-5.0
            if logger is not None:
                logger.info(
                    'Center is too far west of storm.  Moving it to %f'
                    %(cenlo,))
            moved=True
        if logger is not None and not moved:
            logger.info('Center is within +/- 5 degrees longitude of storm.')
        logger.info('Final outer domain center is lon=%f lat=%f'
                    %(cenlo,cenla))
        # Return results as a tuple:
        ( self._cenlo, self._cenla ) = ( cenlo, cenla )
        return ( cenlo, cenla )

    def _parse_carq(self,lines,tech="CARQ",logger=None,raise_all=True):
        """Given an array of lines from a CARQ entry in an ATCF Aid
        Deck file, parses the data and adds it to this StormInfo
        object most of the work is done in other subroutines."""
        d=self.__dict__
        d['center']=tech
        # STEP 1: Split all lines at commas, trim white space from
        # each entry, and make sure everything is for the same storm
        # and time.
        (split,izeros,ibig,fhrbig) = self._split_carq(
            lines,tech,logger,raise_all)
        if not izeros:
            raise InvalidATCF('ATCF CARQ data must contain at least '
                              'one line with forecast hour 0 data.',
                              lines[0])
        first=True
        for izero in izeros:
            if first:
                self._parse_atcf_meat(lines[izero],split[izero],
                                      logger=logger,raise_all=raise_all)
                first=False
            self._parse_atcf_radii_seas(lines[izero],split[izero],
                                        logger=logger,raise_all=raise_all)
        if ibig is not None:
            try:
                d['flat']=floatlatlon(split[ibig][6])
                d['flon']=floatlatlon(split[ibig][7])
                d['havefcstloc']=True
                d['fhr']=fhrbig
            except(StormInfoError,KeyError,ValueError,TypeError,
                   AttributeError) as e:
                if logger is not None:
                    logger.warning('could not location: %s line: %s'%
                                   (str(e),lines[ibig]),exc_info=True)
                if raise_all: raise

        nameless=set(('NAMELESS','UNKNOWN','NONAME',''))
        if 'stormname' not in d or str(d['stormname']).upper() in nameless:
            d['stormname']='NAMELESS'

        # Check for mandatory variables:
        require=set(('basin1','stormname','lat','lon','stnum'))
        for var in require:
            if not var in self.__dict__:
                raise InvalidVitals(
                    'Could not get mandatory field %s from input.  '
                    'First line: %s'%(var,lines[0]),lines[0])
        d['stormnamelc']=d['stormname'].lower()

    def _split_carq(self,lines,tech,logger=None,raise_all=True):
        """Do not call this: it is an internal implementation
        function.  It parses an array of CARQ lines from an Aid Deck
        file.  Returns a four-element tuple containing:
          1. A list of lists (one list per line).  The inner lists
            contain one string per comma separated entry in the line.
          2. A list of indices of lines that are for forecast hour 0
          3. The index of the last line that has the latest forecast
            hour that is not later than 72hrs (needed for generating
            tcvitals forecast locations).
          4. The forecast hour for that line."""
        split=[ line.split(',') for line in lines ]
        izeros=list()
        ibig=None
        fhrbig=None
        for i in xrange(len(split)):
            split[i]=[ x.strip() for x in split[i] ]
            if len(split[i])<8:
                raise InvalidATCF(
                    'CARQ entries in deck files must have at least '
                    'eight fields (everything through lat & lon).  '
                    'Cannot parse this: %s'%(lines[i],),lines[i])
            if any([ split[i][j]!=split[0][j] for j in [0,1,2,4] ]):
                raise InvalidATCF(
                    'Basin, storm number, YMDH and technique must '
                    'match for ALL LINES when parsing CARQ data in '
                    '_parse_carq.',lines[i])
            myfhr=int(split[i][5])
            if myfhr==0:
                if not izeros:
                    self._parse_atcf_time(split[i],tech,
                                          logger=logger,raise_all=raise_all)
                izeros.append(i)
            if (ibig is None or myfhr>fhrbig) and myfhr<=72 and myfhr>0:
                ibig=i
                fhrbig=myfhr
                assert(fhrbig>0)
        return (split,izeros,ibig,fhrbig)

    def _parse_atcf_time(self,data,tech='CARQ',logger=None,raise_all=True):
        """Do not call this.  It is an internal implementation
        routine.  Adds to this StormInfo object the "when" parameter
        that contains the analysis time.  If available, will also add
        the "technum" technique sort number.  The instr is a line of
        original input text for error messages, the "data" is an
        output from _split_carq, and the other parameters are inputs
        to the original constructor."""
        imin=0
        if data[3]!='':
            if tech=='BEST':
                imin=int(data[3])
            else:
                self.__dict__['technum']=int(data[3])
        iwhen=int(data[2])
        when=datetime.datetime(
            year=iwhen/1000000,month=(iwhen/10000)%100,day=(iwhen/100)%100,
            hour=iwhen%100,minute=imin,second=0,microsecond=0,tzinfo=None)
        self.__dict__['when']=when
        self.__dict__['YMDH']=when.strftime('%Y%m%d%H')

    def _parse_atcf_radii_seas(self,instr,data,logger=None,raise_all=True):
        """Do not call this.  It is an internal implementation
        routine.  Adds to this StormInfo object radii and sea height
        data from the given input.  The instr is a line of original
        input text for error messages, the "data" is an output from
        _split_carq, and the other parameters are inputs to the
        original constructor."""
        from hwrf.constants import ft2m,nmi2km,kts2mps
        d=self.__dict__
        if 'qset' in d:
            qset=self.qset
        else:
            qset=set()
            d['qset']=qset
        n=len(data)
        if n>=17:
            try:
                #print 'repr data',repr(data)
                #print 'repr data 13 4=',repr(data[13:17])
                irad=int(data[11])
                windcode=data[12]
                quadrantinfo(d,qset,irad,windcode,data[13:17],'',nmi2km)
                d['windcode%02d'%(irad,)]=windcode
            except (KeyError,ValueError,TypeError,AttributeError) as e:
                if logger is not None:
                    logger.warning('could not parse wind radii: %s line: %s'%
                                   (str(e),instr),exc_info=True)
                if raise_all: raise
        if n>=34:
            try:
                iseas=int(data[28])
                seascode=data[29]
                #print 'repr qdata 30 4=',repr(data[30:34])
                quadrantinfo(d,qset,iseas,seascode,data[30:34],'seas',nmi2km)
                d['seascode%02d'%(iseas,)]=seascode
            except (KeyError,ValueError,TypeError,AttributeError):
                if logger is not None:
                    logger.warning(
                        'could not parse wave height info: %s line: %s'%
                        (str(e),instr),exc_info=True)
                if raise_all: raise

    def _parse_atcf_meat(self,instr,data,logger=None,raise_all=True):
        """Do not call this.  It is an internal implementation
        routine.  Parses just about everything except the time, radii
        and sea height from the input ATCF data.  The instr is a line
        of original input text for error messages, the "data" is an
        output from _split_carq, and the other parameters are inputs
        to the original constructor."""
        if logger is not None and not isinstance(logger,logging.Logger):
            raise TypeError(
                'in _parse_atcf_meat, logger must be a logging.Logger, '
                'but instead it is a %s'%(type(logger).__name__))
        from hwrf.constants import ft2m,nmi2km,kts2mps
        n=len(data)
        d=self.__dict__
        basin=data[0]
        subbasin=None
        #print 'line %s'%(', '.join(data),)
        d['technique']=data[4]
        d['tau']=int(data[5])
        d['lat']=floatlatlon(data[6])
        d['lon']=floatlatlon(data[7])
        # Convenience functions to reduce code complexity.  These
        # attempt an operation, log a failure and move on.
        def fic(s,i,c):
            # unit conversion of an integer value data[i], producing a
            # float, and assigning to d[s] if the input is >=0.
            try:
                ix=int(data[i])
                if ix>=0: 
                    d[s]=float(ix*c)
                    #print 'd[%s]=%f from %d'%(s,d[s],i)
                #else:
                    #print 'd[%s] skipped due to %d (from %d)'%(s,ix,i)
            except (KeyError,ValueError,TypeError,AttributeError) as e:
                if logger is not None:
                    logger.warning('could not parse %s: %s line: %s'%
                                   (repr(data[i]),str(e),instr),exc_info=True)
                if raise_all:
                    raise InvalidATCF('%s: %s: line %s'%(s,str(e),instr),instr)
        def fa(s,i):
            # Simply convert a data value data[i] to a float and
            # assign to d[s] if the result is >=0
            try:
                fx=float(data[i])
                if fx>=0: d[s]=fx
                #print 'd[%s]=%f from %d'%(s,fx,i)
            except(KeyError,ValueError,TypeError,AttributeError) as e:
                if logger is not None:
                    logger.warning('could not parse %s: %s line: %s'%
                                   (repr(data[i]),str(e),instr),exc_info=True)
                if raise_all:
                    raise InvalidATCF('%s: %s: line %s'%(s,str(e),instr),instr)
        if n>=9 and data[8]!='': fic('wmax',8,kts2mps)
        if n>=10 and data[9]!='': fa('pmin',9)
        if n>=11 and data[10]!='': d['stormtype']=data[10]
        if n>=18 and data[17]!='': fa('poci',17)
        if n>=19 and data[18]!='': fic('roci',18,nmi2km)
        if n>=20 and data[19]!='': fic('rmw',19,nmi2km)
        if n>=21 and data[20]!='': fic('gusts',20,kts2mps)
        if n>=22 and data[21]!='': fic('eyediam',21,nmi2km)
        if n>=23 and data[22]!='': subregion=data[22]
        if n>=24 and data[23]!='' and data[23]!='L': fic('maxseas',23,ft2m)
        if n>=25 and data[24]!='': d['initials']=data[24] # retain case
        if n>=26 and data[25]!='':
            if data[25]=='X':
                d['stormdir']=-99
            else:
                fa('stormdir',25)
        if n>=27 and data[26]!='': fic('stormspeed',26,kts2mps)
        if n>=28 and data[27]!='': 
            d['stormname']=str(data[27]).upper()
        if n>=29 and data[28]!='': d['depth']=str(data[28]).upper()[0]
        self._set_basin(basin,subbasin)
        self.renumber_storm(int(data[1]),True)
        d['stormnamelc']=d['stormname'].lower()

    def _parse_message_line(self,instr):
        """Do not call this routine directly.  Call
        StormInfo("message",instr) instead.

        This subroutine parses one line of a hurricane message text
        that is assumed to be for the current century.  The format of
        a hurricane message is the same as for a tcvitals file, except
        that the century is omitted and the file is always exactly one
        line."""
        return self._parse_tcvitals_line(instr,century=
            int(datetime.datetime.utcnow().year)/100)
    def _parse_tcvitals_line(self,instr,century=None):
        """Do not call this routine directly.  Call
        StormInfo("tcvitals",instr) instead.

        This subroutine parses one line of a tcvitals file of a format
        described here:

          http://www.emc.ncep.noaa.gov/mmb/data_processing/tcvitals_description.htm

        Here is an example line with only some of the possible data:

          JTWC 31W HAIYAN    20131104 1200 061N 1483E 270 077 0989 1008 0352 23 064 0084 0074 0074 0084 M ... more stuff ...

          The resulting data is put in self._data.  Note that, at this
          time, there is one new field not present in the above
          mentioned webpage.  The "storm type parameter" is a two
          letter description of the type of the storm: LO=low,
          WV=wave, etc. (there are many possibilities).  That field is
          at the end of the line described in the above link, after
          one space.

          The "century" argument is the first two digits of the year,
          so 19 for the 1900s, 20 for the 2000s and so on.  If century
          is missing or None, and the tcvitals does not specify the
          century either, then InvalidVitals will be raised.  If both
          are available, the tcvitals century is used."""
        m=re.search('''(?xi) 
          (?P<center>\S+) \s+ (?P<stnum>\d\d)(?P<rawbasin>[A-Za-z])
          \s+ (?P<rawstormname>[A-Za-z_ -]+)
          \s+ (?P<rawcentury>\d\d)? (?P<rawYYMMDD>\d\d\d\d\d\d)
          \s+ (?P<rawHHMM>\d\d\d\d)
          \s+ (?P<strlat>-?0*\d+[NS ]) \s+ (?P<strlon>-?0*\d+[EW ])
          \s+ (?P<stormdir>-?0*\d+)
          \s+ (?P<stormspeed>-?0*\d+)
          \s+ (?P<pmin>-?0*\d+)
          \s+ (?P<poci>-?0*\d+) \s+ (?P<roci>-?0*\d+)
          \s+ (?P<wmax>-?0*\d+)
          \s+ (?P<rmw>-?0*\d+)
          \s+ (?P<NE34>-?0*\d+) \s+ (?P<SE34>-?0*\d+) \s+ (?P<SW34>-?0*\d+) \s+ (?P<NW34>-?0*\d+)
          (?: \s+ (?P<depth>\S)
           (?:
            \s+ (?P<NE50>-?0*\d+) \s+ (?P<SE50>-?0*\d+) \s+ (?P<SW50>-?0*\d+) \s+ (?P<NW50>-?0*\d+)
            (?:
              \s+ (?P<fhr>-?0*\d+)
              \s+ (?P<fstrlat>-?0*\d+[NS ])
              \s+ (?P<fstrlon>-?0*\d+[EW ])
              (?:
                \s+ (?P<NE64>-?0*\d+) \s+ (?P<SE64>-?0*\d+) \s+ (?P<SW64>-?0*\d+) \s+ (?P<NW64>-?0*\d+)
                (?: \s+ (?P<stormtype>\S\S?) )?
              )?
            )?
           )?
          )?''',instr)
        if not m:
            raise InvalidVitals('Cannot parse vitals: %s'%(repr(instr),),instr)

        # Variables that are allowed to have None values:
        noneok=set(('NE50','SE50','SW50','NW50','fhr','fstrlat','fstrlon',
                    'NE64','SE64','SW64','NW64','stormtype','rawcentury',
                    'depth'))

        # Variables copied without type conversion:
        raws=set(('rawbasin','rawstormname','rawcentury','rawYYMMDD','rawHHMM',
                  'depth'))

        # Convert lats and lons to signed floats, divided by 10.0:
        latlons=set(('strlat','fstrlat','strlon','fstrlon'))

        # Call .strip on these variables:
        stripme=set(('center','stormtype'))

        # Converted to int:
        int1=set(('stnum',))

        # These variables are converted to floats:
        float1=set(('stormdir','pmin','poci','roci','wmax','rmw','fhr'))
        float1radii=set(('NE34','SE34','SW34','NW34',
                         'NE50','SE50','SW50','NW50',
                         'NE64','SE64','SW64','NW64'))

        # Convert to float, divide by 10:
        float10=set(('stormspeed',))

        d=self.__dict__
        if 'qset' in d:
            qset=d['qset']
        else:
            qset=set()
            d['qset']=qset
        mdict=dict(m.groupdict()) # input dict

        for k,v in mdict.iteritems():
            if v is None:
                if k in noneok: continue
                raise InvalidVitals(
                    'Mandatory variable %s had None value in line: %s'%
                    (str(k),repr(instr)), instr )
            try:
                if k in raws:      d[k] = v
                elif k in stripme: d[k] = str(v).strip()
                elif k in float1:  d[k] = float(v.strip())
                elif k in float10: d[k] = float(v.strip())/10.0
                elif k in int1:    d[k] = int(v.strip())
                elif k in float1radii:
                    val=float(v.strip())
                    d[k]=float(v.strip())
                    if val>0: qset.add(k)
                elif k in latlons:
                    repl=floatlatlon(v,10.0)
                    if repl is None and not k in noneok:
                        raise InvalidVitals(
                            'Mandatory variable %s had invalid value %s '
                            'in line: %s'%\
                                (str(k),str(v),repr(instr)), instr )
                    d[k.replace('str','')]=repl
            except ValueError as e:
                raise InvalidVitals(
                    'Cannot parse vitals key %s value %s: %s from line %s'%
                    ( str(k),repr(v),str(e),repr(instr) ), instr )

        # Boolean flag have34kt: do we have at least one valid 34kt
        # wind radius:
        d['have34kt'] = ( \
            'NE34' in d and 'SE34' in d and 'SW34' in d and 'NW34' in d and \
            ( d['NE34']>0 or d['SE34']>0 or d['SW34']>0 or d['NW34']>0 ) )

        # Boolean flag havefhr: do we have a forecast location?
        d['havefcstloc'] = 'flat' in d and 'flon' in d and \
            d['flat'] is not None and d['flon'] is not None

        # Generate upper-case storm name:
        if 'rawstormname' in d:
            d['stormname']=d['rawstormname'].strip().upper()
        else:
            raise InvalidVitals(
                'No storm name detected in this line: %s'%(instr,),instr)

        # Store a datetime.datetime object in the "when" variable, and
        # the ten-digit YYYYMMDDHH date in the YMDH variable as a str:
        if 'rawYYMMDD' in d and 'rawHHMM' in d:
            if 'rawcentury' in d:
                icentury=int(d['rawcentury'])
            else:
                icentury=int(current_century)
                if(icentury<16 or icentury>20):
                    # icentury = first two digits of the year
                    raise CenturyError(
                        'Implausable tcvitals century %d.  Require '
                        '16 through 20.'%(icentury,))
            sdate='%02d%06d%02d'%(icentury,int(d['rawYYMMDD']), 
                                  int(d['rawHHMM'])/100)
            d['YMDH']=sdate
            d['when']=hwrf.numerics.to_datetime(sdate)
        else:
            raise InvalidVitals('Cannot determine date and time '
                                'from vitals: %s'%(repr(instr),),instr)

        if self.rawbasin=='L' and self.lat<0:
            self.rawbasin='Q'

        # Generate the basin information:
        if 'rawbasin' in d:
            self._set_basin(self.rawbasin)
        else:
            raise InvalidVitals('Cannot find a basin in this line: %s'
                                %(instr,),instr)

        # Generate the auxiliary storm ID information:
        self.renumber_storm(self.stnum,discardold=True)
        d['stormnamelc']=d['stormname'].lower()

        return self
    def set_stormtype(self,stormtype,discardold=False):
        """Sets the two letter storm type self.stormtype.  If
        discardold=False (the default), then the old value, if any, is
        moved to self.old_stormtype."""
        if 'stormtype' in self.__dict__ and not discardold:
            self.__dict__['old_stormtype']=self.stormtype
        if isinstance(stormtype,basestring):
            self.stormtype=str(stormtype)[0:2]
        else:
            self.stormtype=getattr(stormtype,'stormtype','XX')
            if self.stormtype is None or self.stormtype=='':
                self.stormtype='XX'
            else:
                self.stormtype=self.stormtype[0:2]
        assert(self.stormtype is not None)
    def rename_storm(self,newname,discardold=False):
        """Sets the name of the storm.  If discardold=False (the
        default) then the old storm name is moved to
        self.old_stormname."""
        if 'stormname' in self.__dict__ and not discardold:
            self.__dict__['old_stormname']=self.stormname
        self.stormname=str(newname)[0:9]
        if self.format=='tcvitals' or self.format=='message':
            self.line='%s%-9s%s' % (self.line[0:9], 
                self.stormname[0:9], self.line[18:])
        self.__dict__['stormnamelc']=self.stormname.lower()

    def renumber_storm(self,newnumber,discardold=False):
        """Changes the storm number: the 09 in 09L.  That changes
        self.stnum, stormid3, stormid3lc, stormdi4 and longstormid.
        If discardold=False (the default), then the old values are
        moved to the old_stnum, old_stormid3, etc."""
        if 'stnum' in self.__dict__ and not discardold:
            self.__dict__['old_stnum']=self.stnum
            self.__dict__['old_stormid3']=self.stormid3
            self.__dict__['old_stormid3lc']=self.stormid3lc
            self.__dict__['old_stormid4']=self.stormid4
            self.__dict__['old_longstormid']=self.longstormid
            self.has_old_stnum=True
        self.stnum=int(newnumber)
        self.stormid3='%02d%s' % (self.stnum,self.basin1)
        self.stormid4='%s%02d' % (self.pubbasin2,self.stnum)
        self.stormid3lc='%02d%s' % (self.stnum,self.basin1lc)
        if self.lat<0 and self.when.month<7:
            # South hemispheric season year starts in July.  Storms
            # before that are for the prior year.
            self.longstormid='%s%02d%04d' % (self.pubbasin2,self.stnum,
                                             self.when.year-1)
        else:
            self.longstormid='%s%02d%04d' % (self.pubbasin2,self.stnum,
                                             self.when.year)
        if self.format=='tcvitals' or self.format=='message':
            self.line='%s%02d%s' % (self.line[0:5], self.stnum, 
                                    self.line[7:])

    def swap_numbers(self):
        """Swaps the new and old stormid variables.  The stnum and
        old_stnum are swapped, the stormid3 and old_stormid3 are
        swapped, and so on."""
        def swapname(o,n):
            if o in self.__dict__ and n in self.__dict__:
                keep=self.__dict__[o]
                self.__dict__[o]=self.__dict__[n]
                self.__dict__[n]=keep
        swapname('old_stnum','stnum')
        swapname('old_stormid3','stormid3')
        swapname('old_stormid3lc','stormid3lc')
        swapname('old_stormid4','stormid4')
        swapname('old_longstormid','longstormid')
        if self.format=='tcvitals' or self.format=='message':
            self.line='%s%02d%s' % (self.line[0:5], self.stnum, 
                                    self.line[7:])

    def as_tcvitals(self):
        """Returns a tcvitals version of this data.  This is not
        cached, and will be recalculated every time it is called."""
        return self.as_tcvitals_or_message(no_century=False)

    def as_message(self):
        """Returns a message line version of this data.  This is not
        cached, and will be recalculated every time it is called."""
        return self.as_tcvitals_or_message(no_century=True)

    def as_tcvitals_or_message(self,no_century=False):
        """Returns a tcvitals or message version of this data.  This
        is not cached, and will be recalculated every time it is
        called.  If no_century=True, then only two digits of the year
        are written, and the line will be a message."""
        d=self.__dict__
        def bad(s):
            if s not in d: return True
            val=d[s]
            if val is None: return True
            return val<0
        def bad0(s):
            if s not in d: return True
            val=d[s]
            if val is None: return True
            return val<=1e-5
        def cint(i): return int(abs(round(i)))
        if no_century:
            datestring='%y%m%d %H%M'
        else:
            datestring='%Y%m%d %H%M'
        result='%-4s %02d%s %-9s %s %03d%s %04d%s %03d %03d %04d ' \
        '%04d %04d %02d %03d %04d %04d %04d %04d' % (
            str(self.center)[0:4], int(abs(self.stnum)%100), 
            str(self.basin1[0]),
            str(self.stormname)[0:9], self.when.strftime(datestring),
            min(900,cint(self.lat*10.0)), ( 'N' if(self.lat>0) else 'S' ),
            min(3600,cint(self.lon*10.0)), ( 'E' if(self.lon>0) else 'W' ),
            -99 if (bad('stormdir')) else min(360,cint(self.stormdir)),
            -99 if (bad('stormspeed')) else \
                min(999,cint(self.stormspeed*10.0)),
            -999 if(bad0('pmin')) else min(1100,cint(self.pmin)),
            -999 if(bad0('poci')) else min(1100,cint(self.poci)),
            -99 if(bad('roci')) else min(9999,cint(self.roci)),
            -9 if(bad('wmax')) else min(99,cint(self.wmax)),
            -99 if(bad('rmw')) else min(999,cint(self.rmw)),
            -999 if(bad0('NE34')) else min(9999,cint(self.NE34)),
            -999 if(bad0('SE34')) else min(9999,cint(self.SE34)),
            -999 if(bad0('SW34')) else min(9999,cint(self.SW34)),
            -999 if(bad0('NW34')) else min(9999,cint(self.NW34)))

        gotfcst='fhr' in d and 'flon' in d and \
                'flat' in d and self.fhr is not None and\
                self.flat is not None and \
                self.flon is not None and self.fhr>0
        gotst=( 'stormtype' in d )

        if 'depth' not in d and not gotst and not gotfcst: 
            return result
        result='%s %s'%(result,str(getattr(self,'depth','X'))[0])

        if 'NW50' not in d and not gotst and not gotfcst:
            return result
        result='%s %04d %04d %04d %04d'%(
            result,
            -999 if(bad0('NE50')) else min(9999,cint(self.NE50)),
            -999 if(bad0('SE50')) else min(9999,cint(self.SE50)),
            -999 if(bad0('SW50')) else min(9999,cint(self.SW50)),
            -999 if(bad0('NW50')) else min(9999,cint(self.NW50)))

        if gotfcst:
            result='%s %02d %03d%s %04d%s'%(
                result,
                -9 if(self.fhr<0) else min(99,int(self.fhr)),
                min(900,cint(self.flat*10.0)),
                ( 'N' if(self.flat>0) else 'S' ),
                min(3600,cint(self.flon*10.0)),
                ( 'E' if(self.flon>0) else 'W' ))

        if 'NW64' not in d and not gotst: return result

        if not gotfcst: result+=' -9 -99N -999W'

        result='%s %04d %04d %04d %04d'%(
            result,
            -999 if(bad0('NE64')) else min(9999,cint(self.NE64)),
            -999 if(bad0('SE64')) else min(9999,cint(self.SE64)),
            -999 if(bad0('SW64')) else min(9999,cint(self.SW64)),
            -999 if(bad0('NW64')) else min(9999,cint(self.NW64)))
            
        if not gotst: return result

        return '%s % 2s'%(result,str(self.stormtype)[0:2])

    def change_basin(self,basin,subbasin=None,discardold=False):
        if not discardold:
            self.__dict__['old_hwrfbasin2']=self.hwrfbasin2
            self.__dict__['old_pubbasin2']=self.pubbasin2
            self.__dict__['old_basin1']=self.basin1
            self.__dict__['old_basin1lc']=self.basin1lc
            self.__dict__['old_basinname']=self.basinname
        self._set_basin(basin,subbasin)
        if self.format=='tcvitals':
            self.line='%s%s%s'%(self.line[0:7],self.basin1[0],self.line[8:])
        #self.renumber_storm(self.stnum,True)

    def _set_basin(self,basin,subbasin=None,discardold=False):
        """This is a utility function that creates the one and two
        letter basins from a raw one and/or two letter basin.  If the
        input basin is invalid, InvalidBasinError is raised."""
        bb=expand_basin(basin,subbasin)
        self.__dict__['hwrfbasin2']=bb[0]
        self.__dict__['pubbasin2']=bb[1]
        self.__dict__['basin1']=bb[2].upper()
        self.__dict__['basin1lc']=bb[2].lower()
        self.__dict__['basinname']=bb[3]

def expand_basin(basin,subbasin=None):
    """Given a one-letter or two-letter tropical basin identifier, and
    possibly another one-letter tropical basin identifier (subbasin),
    attempts to determine more information about the basin.  Some
    information may be ambiguous if a two letter basin is specified.
    Returns a four-element tuple:

      1. The internal (HWRF/GFDL) two-letter basin identifier.  These
      have an unambiguous mapping to the one-letter basin.

      2. The public, standard two-letter basin identifier.  These are
      ambiguous: IO can be A or B, and SH can be S or P.

      3. The one-letter basin identifier.
      
      4. A description of the meaning of the basin."""
    b=str(basin).upper()
    s='' if(subbasin is None) else str(subbasin).upper()
    if   b=='AL' or b=='L': bb=( 'AL', 'AL', 'L', 
                    'North Atlantic (L/AL)' )
    elif b=='SL' or b=='Q': bb=( 'SL', 'SL', 'Q', 
                    'South Atlantic (Q/SL/LS)' )
    elif b=='LS'          : bb=( 'LS', 'LS', 'Q',
                    'South Atlantic (Q/SL/LS)' )
    elif b=='EP' or b=='E': bb=( 'EP', 'EP', 'E', 
                    'North East Pacific (E/EP)' )
    elif b=='CP' or b=='C': bb=( 'CP', 'CP', 'C', 
                    'North Central Pacific (C/CP)' )
    elif b=='SS' or b=='S': bb=( 'SS', 'SH', 'S', 
                    'South Pacific (S/SH)' )
    elif b=='PP' or b=='P': bb=( 'PP', 'SH', 'P', 
                    'South Indian Ocean (P/SH/PP)' )
    elif b=='AA' or b=='A': bb=( 'AA', 'IO', 'A', 
                    'Indian Ocean: Arabian Sea (A/IO/AA)' )
    elif b=='NA' or b=='A': bb=( 'NA', 'IO', 'A', 
                    'Indian Ocean: Arabian Sea (A/IO/NA)' )
    elif b=='BB' or b=='B': bb=( 'BB', 'IO', 'B', 
                    'Indian Ocean: Bay of Bengal (B/IO/BB)' )
    elif b=='WP':
        if          s=='O': bb=( 'OO', 'WP', 'O', 
                    'North West Pacific: South China Sea Basin (O/W/WP)' )
        elif        s=='T': bb=( 'TT', 'WP', 'T', 
                    'North West Pacific: East China Sea (T/W/WP)' )
        # no subbasin is same as s=='W' when basin is WP
        else:               bb=( 'WP', 'WP', 'W', 
                    'North West Pacific (W/WP)' )
    elif            b=='W': bb=( 'WP', 'WP', 'W', 
                    'North West Pacific (W/WP)' )
    elif b=='SH':
        if          s=='S': bb=( 'SS', 'SH', 'S', 
                    'South Pacific (S/SH)' )
        elif        s=='P': bb=( 'PP', 'SH', 'P', 
                    'South Indian Ocean (P/SH)' )
        elif        s=='U': bb=( 'UU', 'SH', 'U', 
                    'South Pacific: Australian Basin (U/P/S/SH)' )
        else:               bb=( 'SH', 'SH', 'S', 
                    'South Pacific or South Indian Ocean (SH)' )
    elif b=='IO':
        if          s=='A': bb=( 'AA', 'IO', 'A', 
                    'Indian Ocean: Arabian Sea (A/IO)' )
        elif        s=='B': bb=( 'BB', 'IO', 'B', 
                    'Indian Ocean: Bay of Bengal (B/IO)' )
        else:               bb=( 'IO', 'IO', 'B', 
                    'Unspecified North Indian Ocean (IO)' )
    # These three are never used but are defined in the 2007
    # tcvitals documentation:
    elif            b=='U': bb=( 'UU', 'SH', 'U', 
                    'South Pacific: Australian Basin (U/P/S/SH)' )
    elif            b=='O': bb=( 'OO', 'WP', 'O', 
                    'North West Pacific: South China Sea Basin (O/W/WP)' )
    elif            b=='T': bb=( 'TT', 'WP', 'T', 
                    'North West Pacific: East China Sea (T/W/WP)' )
    else:
        raise InvalidBasinError(basin,subbasin)
    return bb
