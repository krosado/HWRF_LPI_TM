"""This module provides two different ways to generate Fortran
namelist files from HWRFConfig sections:

    NamelistInserter - given a file, or a multi-line string, finds
      angle bracket (<vartype:stuff>) strings with values from an
      HWRFConfig section.  This is the suggested method for namelists
      with very few configurable items.

    Conf2Namelist - given one or more Config sections, find values of
      the form nlsec.nlkey=value.  The nlsec specifies the namelist
      &nlsec, the nlkey specifies the namelist variable name, and the
      value is converted to a Python value and then a Fortran namelist
      value.  The Conf2Namelist provides ways to append multiple
      Conf2Namelist objects' data to form Fortran one-dimensional
      arrays.  This is to support WRF namelists, where one specifies
      each domain's data in a 1D array.

In addition, this module provides two functions to_fortnml and
from_fortnml to convert between in-memory Python objects and strings
suitable for pasting in a Fortran namelist to achieve the same value
in Fortran."""

import collections,re,fractions,datetime,ConfigParser, StringIO,logging
import hwrf.numerics

from ConfigParser import NoOptionError,NoSectionError
from hwrf.exceptions import *
from hwrf.numerics import to_datetime, to_datetime_rel, to_fraction

__all__=['to_fortnml','from_fortnml','Conf2Namelist','NamelistInserter']

emptydict=dict() # used for efficiency in a few places that need an
                 # empty dict()

class NamelistRecursion(Exception):
    """This exception is used only for internal purposes and will
    never be raised beyond this module."""

def to_fortnml(py,exc_hint=''):
    """Converts the given python data structure to a string suitable
    for representing in a Fortran namelist.  It can handle bool, int,
    float, string, datetime, fraction and a list (or tuple) of those.
    A datetime is converted via strftime('"%Y-%m-%d_%H:%M:%S"').  The
    optional exc_hint is used when raising exceptions to give an idea
    of what file and line number, or config option name, generated the
    exception."""
    try:
        return __to_fortnml_impl(py,exc_hint=exc_hint)
    except NamelistRecursion:
        s=repr(py)
        s=s[0:37]+'...' if len(s)>40 else s
        raise NamelistValueError('%s: cannot convert nested python containers to Fortran namelist values.'%(exc_hint+s,))

def __to_fortnml_impl(py,recursed=False,exc_hint=''):
    if isinstance(py,bool):                  return 'T' if py else 'F'
    if isinstance(py,int):                   return str(py)
    if isinstance(py,float):                 return str(py)
    if isinstance(py,fractions.Fraction):    return str(float(py))
    if isinstance(py,basestring):            return '"'+re.sub('"','""',str(py))+'"'
    if isinstance(py,datetime.datetime):     return py.strftime('"%Y-%m-%d_%H:%M:%S"') # WPS format
    if isinstance(py,list) or isinstance(py,tuple):
        if recursed:
            raise NamelistRecursion()
        else:
            return ', '.join([__to_fortnml_impl(x,recursed=True,
                exc_hint=exc_hint) for x in py])
    raise NamelistValueError('%s%s Unable to convert to a fortran namelist '
                             'value'%(exc_hint,repr(py)))

fortnml_parse=re.compile("""(?ix)\s*(?:
    (?P<fraction>[0-9]+[+][0-9]+[/][0-9]+) |
    \"(?P<dqchar>(?:[^\"]+|\"\")+)\" |
    \'(?P<sqchar>(?:[^\']+|\'\')+)\' |
    (?P<real>
      (?:(?:[+-]?[0-9]+\.[0-9]*|[+-]?\.[0-9]+)(?:[eE][+-]?[0-9]+)?) |
      (?:[+-]?[0-9]+[eE][+-]?[0-9]+)
    ) |
    (?P<int>[+-]?[0-9]+) | 
    (?P<identifier>[a-zA-Z_][a-zA-Z0-9_]+|[a-eg-su-zA-EG-SU-Z_]) |
    (?P<true>t|\.true\.) |
    (?P<false>f|\.false\.) |
    (?P<comment>[!#].*$) |
    (?P<comma>\s*,\s*) |
    (?P<bad>.)
)""")
"""A regular expression for tokenizing a fortran namelist scalar value."""

def from_fortnml(py):
    """Given a string from a Fortran namelist value, returns an
    equivalent python object.  Will throw NamelistValueError if an
    unrecognized object is present, or NamelistRecursion if you send a
    nested container (such as a list of lists)."""
    out=[]
    islist=False
    for match in fortnml_parse.finditer(py):
        tok=match.lastgroup
        if tok is None or tok=='bad':
            raise NamelistValueError('%s: cannot parse namelist value'%(py,))
        elif tok=='comment':    break
        elif tok=='int':        out.append(int(match.group(tok)))
        elif tok=='real':       out.append(float(match.group(tok)))
        elif tok=='true':       out.append(True)
        elif tok=='false':      out.append(False)
        elif tok=='fraction':   out.append(to_fraction(match.group(tok)))
        elif tok=='dqchar':     out.append(re.sub('""','"',match.group(tok)))
        elif tok=='sqchar':     out.append(re.sub("''","'",match.group(tok)))
        elif tok=='comma':      islist=True
        elif tok=='identifier': out.append(match.group(tok))
    if len(out)==1:
        return out[0]
    elif len(out)==0:
        raise NamelistValueError('%s: does not specify any data',(py,))
    else:
        return out

class NamelistInserter(object):
    """
    This class parses an input file that contains a partial namelist,
    inserting missing values of the format <s:cycle> with values from
    a ConfigParser-like object.  The <s:cycle> sorts of text follow a
    specific format:

    <s:varname> -- insert a string surrounded by double quotes taken
      from the specified conf variable
 
    <f:varname> -- insert a float, taken from conf variable varname
    <r:varname> -- same as <f:varname>

    <i:varname> -- insert an integer, taken from conf variable varname

    <l:varname> -- insert a logical, taken from conf variable varname
    <b:varname> -- same as <l:varname>

    <d:varname> -- the conf variable is converted to a
      datetime.datetime using hwrf.numerics.to_datetime, and then to a
      string of the format "YYYY-MM-DD_HH:MM:SS".  If atime is
      specified to the parse subroutine, then the value is allowed to
      be a difference relative to the atime (as accepted by
      hwrf.numerics.to_datetime_rel).
 
    <u:varname> -- convert the conf variable to a string, and dump its
      value unquoted.  This is used, for example, to have the name of a
      namelist variable be generated from a conf file.

    You can also specify angle braces without a type:

    <varname>

    to ask for the variable to be converted by guessing the type that
    was intended in the conf file.  For example:

    This conf file:
      [conf]
      var=T,F,T

    With this namelist:
      &nl
        myvar=<var> / 

    will produce:

      &nl
        myvar=.true., .false., .true. /

    As for variables, one can request a subitem of a variable:
 
      varname -- get variable varname
      vit[stormname] -- get variable "vit" and then get 
                        vit.__getitem__["stormname"]

    for subscriptable types.  This is mainly intended for vitals."""
    find_ltgt=re.compile('''(?P<pre>(?:[^<]|"(?:[^"]|""")*"|'(?:[^']|'{3})*')*)<(?:(?P<typ>[^:]*):)?(?P<var>[^>\[]*)(?:\[(?P<sub>[^\]]*)\])?>(?P<rest>.*)''')
    nlfalse=re.compile('\A(?i)(?:f.*|.false.|n|no|0*[1-9][0-9]*)\Z')
    nltrue=re.compile('\A(?i)(?:t.*|.true.|y|yes|0)\Z')
    comment=re.compile('(?P<code>(?:[^!]|\"(?:[^\"]|\"\"\")*\"|\'(?:[^\']|\'\'\')*\')*)(?P<comment>!.*)?')
    def __init__(self,conf,section):
        """Creates a new NamelistInserter that will get its data from
        the specified section of the HWRFConfig conf."""
        self._conf=conf
        self._section=section
    def parse(self,line_iterable,logger=None,source='<internal>',
              raise_all=True,atime=None,ftime=None,**kwargs):
        """Generates the namelist, returning it as a string.

          line_iterable - an iterator that iterates over each line of
            the file.

          logger - optional: a logging.Logger to log messages, or None
            to disable.  Default: None
          
          source - for warning or error messages: the filename.

          raise_all=False - log messages instead of raising
            exceptions.  Raise only one exception at the end.
            Default: raise an exception as soon as the first error is
            seen.
         
          atime - Optional: the analysis time for conf.timestrinterp

          ftime - Optional: the forecast time for conf.timestrinterp

          kwargs - additional variables and their values."""
        assert(not isinstance(line_iterable,basestring))
        if atime is not None:
            atime=to_datetime(atime)
        if ftime is not None:
            ftime=to_datetime_rel(ftime,atime)
        elif atime is not None:
            ftime=atime
        out=StringIO.StringIO()
        conf=self._conf
        section=self._section
        iline=0
        def synerr(what):
            if logger is not None:
                logger.warning('%s:%d: syntax error: %s'%(source,iline,what))
        for line in line_iterable:
            line=line.rstrip()
            iline+=1
            linepart=line
            m=NamelistInserter.comment.match(linepart)
            comment=''
            if m:
                code=m.group('code')
                comment=m.group('comment')
                if not code:
                    out.write(line)
                    continue
                linepart=code
            while len(linepart)>0:
                m=NamelistInserter.find_ltgt.match(linepart)
                if m:
                    pre=m.group('pre')
                    typ=m.group('typ')
                    var=m.group('var')
                    sub=m.group('sub')
                    rest=m.group('rest')
                    if rest:
                        assert(linepart!=rest)
                        linepart=rest
                    else:
                        linepart=''
                    if pre: out.write(pre)
                    if typ is None: typ='*'
                    if not typ:    synerr('no output type specified')
                    elif not var:  synerr('no variable specified')
                    elif len(typ)!=1:
                        synerr('output type must be one of: bdfilsu, not %s'
                               %(typ,))
                    elif typ not in 'bdfilrsuBDFILRSU*':
                        synerr('invalid type %s specified: only bdfilsu are '
                               'allowed'%(typ,))
                    else:
                        try:
                            if var in kwargs:
                                val=kwargs[var]
                            elif atime is not None:
                                val=conf.timestrinterp(
                                    section,'{'+var+'}',ftime=ftime,
                                    atime=atime,**kwargs)
                            else:
                                val=conf.strinterp(
                                    section,'{'+var+'}',**kwargs)
                        except(KeyError,TypeError,ValueError,NoOptionError,
                               NoSectionError) as e:
                            if logger is not None:
                                logger.warning(
                                    '%s:%d: cannot find variable %s'
                                    %(source,iline,var))
                            if raise_all: raise
                            continue
                        if sub:
                            try:
                                newval=val[sub]
                                val=newval
                            except (TypeError,KeyError,ValueError,HWRFError) \
                                    as e:
                                if logger is not None:
                                    logger.warning('%s:%d: %s[%s]: %s'%(
                                            source,iline,var,sub,str(e)),
                                                   exc_info=True)
                                if raise_all: raise
                                continue
                        try:
                            if typ in 'rRfF':   typval=float(val)
                            elif typ in 'iI': typval=int(val)
                            elif typ in 'bBlL':
                                if isinstance(val,bool):
                                    typval=val
                                elif isinstance(val,basestring):
                                    if NamelistInserter.nlfalse.match(val):
                                        typval=False
                                    elif NamelistInserter.nltrue.match(val):
                                        typval=True
                                    else: 
                                        raise ValueError(
                                            '%s is not a valid logical'
                                            %(repr(val),))
                                else:
                                    typval=bool(val)
                            elif typ in 'dD':
                                dval=from_fortnml(val)
                                if atime is not None:
                                    typval=to_datetime_rel(dval,atime)
                                else:
                                    typval=to_datetime(dval)
                            elif typ=='*':
                                typval=from_fortnml(val)
                            else: # types u and s
                                typval=str(val)
                        except (TypeError,KeyError,ValueError) as e:
                            if logger is not None:
                                logger.warning(
                                    '%s:%d: cannot convert %s to type=%s: %s'
                                    %(source,iline,repr(val),typ,str(e)),
                                    exc_info=True)
                            if raise_all: raise
                            continue
                        if sub: fromthis='%s[%s]'%(var,sub)
                        else: fromthis=var
                        try:
                            if typ in 'bdfilrsBDFILRS*':
                                writeme=to_fortnml(
                                    typval,exc_hint=fromthis+':')
                            else: # type u
                                writeme=str(typval)
                        except (TypeError,KeyError,ValueError) as e:
                            if logger is not None:
                                logger.warning(
                                    '%s:%d: <%s:%s>=%s: error converting to '
                                    'string: %s'
                                    %(source,iline,typ,fromthis,repr(typval),
                                      str(e)),exc_info=True)
                            if raise_all: raise
                            continue
                        out.write(writeme)
                else:
                    # No more <typ:val> on this line.  Write the rest.
                    out.write(linepart)
                    linepart=''
            out.write(comment)
            out.write('\n')
        return out.getvalue()

class Conf2Namelist(object):
    """This class generates namelist information from any
    ConfigParser-like object, starting at a specified conf section.
    Variables of the form nlsection.nlkey will be put in namelist
    section nlsection, with variable name nlkey.  Variables that
    contain no "." but are valid Fortran variable names (except for
    the variable "namelist") are called "traits" and are accessible
    via trait_get, trait_set, and the other trait_* subroutines.  The
    special variable "namelist" specifies a list of conf sections to
    recurse into.  Variables in the local conf section will always
    override variables in included conf sections, and variables in
    later included conf sections will override variables in earlier
    included conf sections.
        
    For example:
        conf=RawConfigParser()
        conf.readfp(StringIO('''
          [sec1]
             happiness_quotient=0.7
             physics.mp_physics=85
             physics.cu_physics=84
             namelist=sec2,sec3
          [sec2]
             physics.cu_physics=4
             physics.bl_pbl_physics=93
          [sec3]
             physics.bl_pbl_physics=3
       ''')
       str(Conf2Namelist(conf,'sec1'))
    Will result in:
         &physics
           bl_pbl_physics=3
           cu_physics=84
           mp_physics=85
         /
    and a trait accessible via self.trait_get('happiness_quotient')
    """
    testnl='''[sec1]
happiness_quotient=0.7
physics.mp_physics=85
physics.cu_physics=84
namelist=sec2,sec3
[sec2]
physics.cu_physics=4
physics.bl_pbl_physics=93
domains.something=32
[sec3]
physics.bl_pbl_physics=3'''
    """A test namelist for debugging"""

    nlentry=re.compile('\A(?:(?P<section>[a-zA-Z_][a-zA-Z0-9_]*)\.)?(?P<var>[a-zA-Z_][a-zA-Z0-9_%]*)\Z')
    """A regular expression from re.compile.  This is used to scan
    config section option names to detect if they are namelist
    variable names (format: section.variable)."""
    
    nlfalse=re.compile('\A(?i)(?:f.*|.false.)\Z')
    """A regular expression from re.compile, to detect false Fortran logical values."""

    nltrue=re.compile('\A(?i)(?:t.*|.true.)\Z')
    """A regular expression from re.compile, to detect true Fortran logical values."""

    TRAIT='-trait-' # special section name for traits
    """A special, fake, invalid namelist name for traits."""

    def copy(self,other):
        """Returns a deep copy of self."""
        return Conf2Namelist(section_sorter=self.section_sorter,
                             var_sorters=self.var_sorters,
                             logger=self.logger,
                             nl=self.nl)

    def __init__(self,conf=None,section=None,section_sorter=None,
                 var_sorters=None,logger=None,nl=None,morevars=None):
        """Creates a Conf2Namelist.
          conf - the HWRFConfig object
          section - the section to start searching from.  
          section_sorter - the cmp-like function to use to sort the
            sections when generating the output namelist
          var_sorters - a dict-like mapping from section name to a
            cmp-like function to use to sort variable names within 
            each section.
          logger - a logging.Logger object to use to log messages
          nl - a dict of dicts (or object that acts like that)
            to use to initialize the Conf2Namelist.  Warning: this
            functionality is untested
          morevars - additional variables to use when expanding strings
            This is simply passed to conf.items.  See the HWRFConfig
            documentation for details."""
        if morevars is None: morevars=emptydict
        self.section_sorter=None
        self.var_sorters=var_sorters
        if self.section_sorter is None:
            self.section_sorter=cmp
        self.nl=collections.defaultdict(lambda: collections.defaultdict())
        if nl is not None:
            for (sec,con) in nl.iteritems():
                for key,val in con.iteritems():
                    nl[sec][key]=val
        if conf is None or section is None:
            return
        initial=str(section)
        touched=set()
        parseme=[initial]
        nlentry=Conf2Namelist.nlentry
        while len(parseme)>0:
            sec=parseme.pop()
            if sec in touched:
                continue
            touched.add(sec)
            if logger is not None:
                logger.debug('Conf2Namelist now parsing section %s'%(sec,))
            for key,value in conf.items(sec,morevars=morevars):
                m=nlentry.match(key)
                if m and m.group('section') is not None:
                    self.nl_set_if_unset(m.group('section'),m.group('var'),
                                         from_fortnml(value))
                elif key=='namelist':
                    # namelist=list,of,conf,sections
                    # Add each one to the list of sections to parse
                    for sec2 in value.split(','):
                        trim=sec2.strip()
                        if len(trim)>0 and not trim in touched:
                            parseme.append(trim)
                elif re.match('\A[a-zA-Z_][a-zA-Z_0-9%]*\Z',key):
                    self.trait_set_if_unset(m.group('var'),
                                            from_fortnml(value))
                elif logger is not None:
                    logger.debug(
                        'Conf2Namelist ignoring %s = %s in section %s'
                        %(key,value,sec))
    def nl_section(self,*args):
        """Ensures that the specified namelist section or sections
        exist.  Returns self."""
        for section in args:
            self.nl[str(section).lower()]
        return self
    def nl_set(self,section,var,data):
        """Sets a namelist section's variable."""
        if not ( isinstance(data,basestring) or 
                 isinstance(data,datetime.datetime) or
                 isinstance(data,int) or isinstance(data,float) or
                 isinstance(data,list) or
                 isinstance(data,fractions.Fraction) ):
            raise TypeError('%s: invalid type for namelist (value=%s)'
                            %(data.__class__.__name__,repr(data)))
        self.nl[str(section).lower()][str(var).lower()]=data
    def nl_del(self,section,var):
        """Removes a variable from a namelist section."""
        try:
            del self.nl[str(section).lower()][var]
        except KeyError: pass
    def nl_have(self,section,var):
        """Returns True if a variable exists in a namelist section and
        False otherwise."""
        if section not in self.nl: return False
        return var in self.nl[section]
    def nl_get(self,section,var,default=None):
        """Gets the value of a variable in a namelist section.  If the
        default is supplied and non-Null, it will be returned if the
        variable does not exist."""
        s=str(section).lower()
        v=str(var).lower()
        try:
            return self.nl[s][v]
        except KeyError:
            if default is not None:
                return default
            raise NamelistKeyError('no value given',s,v)
    def nl_set_if_unset(self,section,var,data):
        """Sets the namelist section's variable if it has no value."""
        try:
            self.nl_get(section,var)
        except KeyError:
            self.nl_set(section,var,data)
    def nl_each(self,section):
        """Iterates over variable,value tuples in the given
        section."""
        if not isinstance(section,basestring): section=str(section)
        if not section in self.nl: return
        for var,value in self.nl[section].iteritems():
            yield var,value
    def trait_each(self):
        """Iterates over variable,value tuples in the traits."""
        assert(Conf2Namelist.TRAIT in self.nl)
        assert(self.nl[Conf2Namelist.TRAIT])
        for var,value in self.nl[Conf2Namelist.TRAIT].iteritems():
            yield var,value
    def trait_set(self,var,value):
        """Sets a trait's value."""
        return self.nl_set(Conf2Namelist.TRAIT,var,value)
    def trait_del(self,var):
        """Deletes a trait."""
        return self.nl_del(Conf2Namelist.TRAIT,var)
    def trait_get(self,var,default=None):
        """Returns a trait's value.  If a default is given and
        non-Null, returns the default if the trait does not exist."""
        return self.nl_get(Conf2Namelist.TRAIT,var,default)
    def trait_have(self,var):
        """Returns True if the trait exists, and False otherwise."""
        return self.nl_have(Conf2Namelist.TRAIT,var)
    def trait_set_if_unset(self,var,value):
        """Sets the traits value if it does not have one."""
        return self.nl_set_if_unset(Conf2Namelist.TRAIT,var,value)
    def join(self,others):
        """Generates a new namelist by looping over all arguments in
        this namelist, and appending the other namelists' values with
        string sep in between.  If the other namelist does not have a
        given value, then this namelist's value, or the last namelist
        that had a value for that argument, will be appended.
        Variables or namelist sections that exist in the other
        namelists, but not this one, will be ignored."""
        if len(others)==0:
            return self.copy()
        out=Conf2Namelist(None,None)
        for secname,mydict in self.nl.iteritems():
            outsec=out.nl[secname]
            for var,value in mydict.iteritems():
                thelist=[value]
                default=value
                for other in others:
                    nextval=other.nl_get(secname,var,default)
                    default=nextval
                    if isinstance(nextval,tuple) or isinstance(nextval,list):
                        thelist.extend(nextval)
                    else:
                        thelist.append(nextval)
                outsec[var]=thelist
        return out
    def copy(self,section_subset=None,var_subset=None,other=None):
        """Returns a copy of this object, or if other is specified,
        copies this object's contents into the target Conf2Namelist.
        When copying into a target Conf2Namelist, only values that are
        not already in that namelist will be copied.  The copy has its
        own data structures, so modifying the copy will not modify the
        original.  Optionally, you can copy only a subset of this
        object:

        section_subset(secname) = a callable object that returns True
          for each section to be kept
        var_subset(secname,varname) = a callable object that returns
          True for each variable to be kept.

        Traits are identified by secname=Conf2Namelist.TRAIT"""
        if other is None:
            other=Conf2Namelist(None,None)
        for s,sd in self.nl.iteritems():
            if section_subset is None or section_subset(s):
                outdict=other.nl[s]
                for var,value in sd.iteritems():
                    if var_subset is None or var_subset(s,var):
                        try:
                            junk=outdict[var]
                        except KeyError:
                            outdict[var]=value
        return other
    def remove_traits(self):
        """Removes all traits."""
        if Conf2Namelist.TRAIT in self.nl:
            del(self.nl[Conf2Namelist.TRAIT])
        return self
    def __str__(self):
        """Generates a Fortran namelist as a string.  Equivalent to
        self.make_namelist()."""
        return self.make_namelist()
    def make_namelist(self,section_sorter=None,var_sorters=None,
                      morevars=None):
        """Returns the stringified namelist for this object, suitable
        for writing to a file for Fortran to read.  Optionally, you
        can provide a sorting for the namelist entries:

        section_sorter = a cmp-like function for sorting sections by
          name If absent, cmp is used.

        var_sorters = a dict-like mapping of section names to cmp-like
          objects for sorting variables within each section.  If
          absent, self.namelist_sorter(sectionname) is used"""
        out=''
        if section_sorter is None:
            section_sorter=self.section_sorter

        sd=None
        def getvar(var):
            if morevars is not None and var in morevars:
                return morevars[var]
            else:
                return sd[var]

        for sec in sorted(self.nl.iterkeys(),section_sorter):
            sd=self.nl[sec]
            if sec==Conf2Namelist.TRAIT:
                out+='! Traits:\n'
            else:
                out+='&%s\n'%(sec,)
            if sd:
                sorter=None
                if var_sorters is not None and sec in var_sorters:
                    sorter=var_sorters[sec]
                if sorter is None:
                    sorter=self.namelist_sorter(sec)
                if sec==Conf2Namelist.TRAIT:
                    out+="\n".join('!  %s = %s,'%(var,to_fortnml(getvar(var),
                        exc_hint='%s%%%s='%(str(sec),str(var)))) \
                            for var in sorted(sd.keys(),sorter))
                else:
                    out+="\n".join('  %s = %s,'%(var,to_fortnml(getvar(var),
                        exc_hint='%s%%%s='%(str(sec),str(var)))) \
                            for var in sorted(sd.keys(),sorter))
            if sec==Conf2Namelist.TRAIT:
                out+='\n\n'
            else:
                out+="\n/\n\n"
        return out
    def namelist_sorter(self,section):
        """Returns a cmp function that orders namelist entries for the
        specified namelist section.  See the argument "cmp" of the
        python built-in function sorted() for details."""
        if self.var_sorters is not None:
            if section in self.var_sorters:
                return self.var_sorters[section]
        return lambda x,y: cmp(x,y)
    def set_sorters(self,section_sorter,var_sorters):
        """Sets the cmp-like functions for sorting sections and
        variables in each section.  The section_sorter sorts sections.
        The var_sorters is a dict-like mapping from section name to a
        cmp-like function for sorting that section.  If any sorter is
        unspecified, cmp will be used.  See the "cmp" argument of the
        python built-in function sorted() for details."""
        self.section_sorter=cmp if section_sorter is None else section_sorter
        self.var_sorters=var_sorters
        return self
