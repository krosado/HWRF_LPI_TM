__all__=["DataCatalog","InputSource",'in_date_range']

import collections, os, ftplib, tempfile, ConfigParser, urlparse, stat, \
    re, threading, time
import produtil.run, produtil.cluster, produtil.fileop, produtil.cd, \
    produtil.workpool
import hwrf.numerics, hwrf.exceptions

from produtil.run import alias, exe, checkrun, ExitStatusException, run
from produtil.fileop import deliver_file, isnonempty, make_symlink, makedirs
from hwrf.numerics import to_datetime, to_datetime_rel, to_timedelta
from hwrf.exceptions import InputSourceBadType,PartialTransfer,\
    UnsupportedTransfer

########################################################################
def in_date_range(t,trange):
    epsilon=to_timedelta('1800') # epsilon = one half hour
    t=to_datetime(t)
    for tr in trange.split(','):
        idash=tr.find('-')
        if idash<0:
            # single date
            start=to_datetime(tr)
            if t>=to_datetime_rel(-epsilon,start) \
                    and t<=to_datetime_rel(epsilon,start):
                return True
        else:
            # date range
            start=to_datetime(tr[0:10])
            end=to_datetime(tr[idash+1:idash+11])
            if t>=to_datetime_rel(-epsilon,start) \
                    and t<=to_datetime_rel(epsilon,end):
                return True
    return False
            

########################################################################
def tempopen(f,m):
    produtil.fileop.makedirs(os.path.dirname(f))
    return tempfile.NamedTemporaryFile(prefix=os.path.basename(f),
                                  dir=os.path.dirname(f),
                                  mode=m,suffix='.tmp',delete=False)

########################################################################
class DataCatalog(object):
    """This class is a collection of functions that know how to
    provide the location of a file in either an archive or a
    filesystem.  It does not know how to actually obtain the file.
    This serves as the underlying "where is that file" implementation
    of InputSource."""
    def __init__(self,conf,section,anltime):
        self.conf=conf
        if not isinstance(section,basestring):
            raise TypeError('In DataCatalog.__init__, section must be a '
                            'string.')
        self.section=section
        self.anltime=to_datetime(anltime)
    def __repr__(self):
        return "DataCatalog(%s,%s,%s)"%(repr(self.conf),repr(self.section),
                                        repr(self.anltime))
    def rt_updated(self):
        """Returns True if this dataset is updated in real-time, False
        otherwise.  By default, this will return True if
        conf[section,"rt_updated"] is set to "yes" or False otherwise."""
        try:
            return conf.getbool(section,'rt_updated',False)
        except ( ConfigParser.Error,KeyError,TypeError,ValueError ) as e:
            return False
    def parse(self,string,atime=None,ftime=None,logger=None,dates=None,
              **kwargs):
        """Performs string interpolation using the underlying conf
        object.  This acts exactly like the expansions done in the
        hwrf.conf file: {stuff} is expanded to the contents of the
        "stuff" variable.  Expanstions are done in the section
        specified in the constructor.  In addition, various a* and f*
        variables are expanded based on the analysis time ("atime")
        and forecast time ("ftime").  See
        hwrf.HWRFConfig.timestrinterp for details."""
        if atime is None: 
            if logger is not None:
                logger.info(
                    '{%s}: has no atime.  Will use atime=self.anltime=%s.'%(
                        str(string),repr(atime)))
            atime=self.anltime
        if ftime is None: 
            if logger is not None:
                logger.info('{%s}: has no ftime.  Will use ftime=atime=%s.'%(
                        str(string),repr(atime)))
            ftime=atime
        atime=to_datetime(atime)
        ftime=to_datetime_rel(ftime,atime)
        if dates is not None and atime is not None:
            if not in_date_range(atime,dates):
                if logger is not None:
                    logger.info('{%s}: atime %s not in %s'%(
                            str(string),str(atime),str(dates)))
                return None
        if logger is not None:
            logger.info(
                'parsing {%s} with ftime=%s atime=%s in section %s'
                %(str(string),repr(ftime),repr(atime),repr(self.section)))
        return self.conf.timestrinterp(
            self.section,"{"+string+"}",ftime,atime,**kwargs)
    def locate(self,dataset,item,atime=None,ftime=None,logger=None,
               dates=None,**kwargs):
        """Locates the specified item for the specified dataset, at
        the given analysis time ("atime") and forecast time ("ftime").
        If the requested data is known to not exist, returns None.
        This should be overridden by subclasses.  The present
        implementation just does this: {dataset}/{item} expanding
        dataset and item with self.parse.  Any kwargs are passed
        along: this allows such things as ensemble ID, or switching
        between GRIB1 or GRIB2 via a keyword argument."""
        if logger is not None: 
            logger.info(
                'locate item=%s atime=%s ftime=%s in dataset=%s'
                %(repr(item),repr(atime),repr(ftime),repr(dataset)))
        ds=self.parse(dataset,atime=atime,ftime=ftime,logger=logger,
                      dates=dates,**kwargs)
        if ds is None: return None
        it=self.parse(item,atime=atime,ftime=ftime,logger=logger,**kwargs)
        result=ds+it
        if logger is not None:
            logger.info( 'result %s %s => %s'%(
                    repr(ds),repr(it),repr(result),))
        return result

########################################################################
class InputSource(object):
    """This class knows how to fetch data from remote clusters, or the
    local machine.  The data locations are specified by a DataCatalog,
    and the transfer mechanisms are specified in a config file."""
    def __init__(self,conf,section,anltime,htar=None,logger=None,hsi=None):
        self.conf=conf
        self.section=section
        self.anltime=anltime
        self._logger=logger
        self.forecast=list() # FORECAST mode DataCatalogs
        self._f_sorted=True
        self.history=list() # HISTORY mode DataCatalogs
        self._h_sorted=True
        self.locks=collections.defaultdict(threading.Lock)
        assert(htar is not None)
        assert(hsi is not None)
        self.htar=alias(htar)
        self.hsi=alias(hsi)
        self.valid=collections.defaultdict(None)

        sections=[section]
        if conf.has_option(section,'@inc'):
            sections.extend(conf[section,'@inc'].split(','))

        sources=collections.defaultdict(dict)
        for sec in sections:
            for key in conf.keys(sec):
                c=key.find('%')
                if(c>0):
                    (src,attr)=(key[0:c],key[c+1:])
                    try:
                        sources[src][attr]=conf.get(sec,key)
                    except KeyError as ke:
                        if logger is not None:
                            logger.warning("[%s] %s: key error: %s"%(
                                    sec,key,str(ke)))
                        continue
        for (src,attr) in sources.iteritems():
            if 'location' in attr and ('histprio' in attr or \
                                           'fcstprio' in attr):
                dctype=attr.get('type','DataCatalog')
                if   dctype=='DataCatalog': 
                    dc=DataCatalog(self.conf,src,self.anltime)
                else:
                    raise InputSourceBadType(
                        'Do not know how to make a DataCatalog of type "%s"'
                        %(dctype,))
                if 'dates' in attr:
                    dates=attr['dates']
                else:
                    dates='1970010100-2038011818'
                self.add(dc,location=attr['location'],
                         fcstprio=attr.get('fcstprio',None),
                         histprio=attr.get('histprio',None),
                         dates=dates)
        self._sort()
    def _rsync_ssh_exe(self,netpart,path=None,dest=None):
        """Returns a Runner object (as in produtil.run) for executing
        rsync -e ssh.  This subroutine is used to implement
        workarounds for known bugs."""
        rsync=self.conf.getexe('rsync','rsync')
        if 'jet' in netpart or produtil.cluster.name()=='jet':
            # Workaround for Jet bug: use protocol 29
            cmd=alias(exe(rsync)['-e','ssh','--protocol','29'])
        else:
            cmd=alias(exe(rsync)['-e','ssh'])
        if path and dest:
            cmd=cmd['-LvptgoD',"%s:%s"%(netpart,path),dest]
        else:
            # Don't transfer a file.  Just check access.  Use /
            cmd=cmd['-d','%s:/'%(netpart,)]
        return cmd
    def _sort(self):
        """Sorts the list of history and forecast DataCatalogs by
        decreasing priority."""
        self.forecast=sorted(self.forecast,key=lambda x: -x[0])
        self.history=sorted(self.history,key=lambda x: -x[0])
    def add(self,dc,location,fcstprio=None,histprio=None,dates=None):
        """Adds a DataCatalog to this InputSource.  Note that bad
        things will happen if you add the same source twice

        Arguments:
          dc = the DataCatelog object
          location = the URL of the data source, including the
            username if needed.
            Examples:
              local files: file:///
              scp:         sftp://Some.Username@dtn-zeus.rdhpcs.noaa.gov/
              ftp:         ftp://anonymous@ftpprd.ncep.noaa.gov/
          fcstprio = the priority for using this source in FORECAST
            (real-time) mode.  If missing or None, the source will not
            be used in FORECAST mode.

          histprio = the priority for using this source in HISTORY
            (retrospective) mode.  If missing or None,the source will
            not be used in HISTORY mode.
            
          If fcstprio and histprio are both None, the request is
          ignored."""
        if fcstprio is None and histprio is None: return
        if dates is None:
            dates='1970010100-2038011818'
        parsed=urlparse.urlparse(location)
        if fcstprio is not None:
            self.forecast.append( ( float(fcstprio), location, parsed, dc, dates ) )
            self._f_sorted=False
        if histprio is not None:
            self.history.append( ( float(histprio), location, parsed, dc, dates ) )
            self._h_sorted=False
    def open_ftp(self,netpart,logger=None,timeout=20):
        """Opens the specified ftp://user@host/... request subject to
        the specified timeout, logging to the specified logger (if
        present and non-Null)."""
        if logger is None: logger=self._logger
        if logger is not None:
            logger.info('open_ftp %s'%(netpart,))
        r=re.search('([a-zA-Z0-9_.-]+)+@(.+)',netpart)
        if r:
            (user,host)=r.groups()
            if not user or not host:
                raise InvalidLogin(
                    'FTP logins must be of the form user@host but you '
                    'gave "%s"'%(netpart))
        else:
            (user,host)=('anonymous',netpart)
        f=None
        try:
            if logger is not None: logger.info('%s@%s: log in'%(user,host))
            f=ftplib.FTP(host,user,timeout=timeout)
            f.login()
            assert(f is not None)
            retval=f
            f=None
            valid['ftp://'+netpart]=True
            return retval
        except Exception as e:
            valid['ftp://'+netpart]=False
        finally:
            if f is not None: f.close()
    def rsync_check_access(self,netpart,logger=None,timeout=20):
        try:
            cmd=self._rsync_ssh_exe(netpart)
            checkrun(cmd,logger=logger)
            return True
        except Exception as e:
            if logger is not None:
                logger.warning('%s: rsync cannot access: %s'
                               %(str(netpart),str(e)))
            return False

    def fetch_file(self,streams,dc,dsurl,urlmore,dest,logger=None,
                   timeout=20,realtime=True):
        """You should not call this directly; it is meant to be called
        by "get" and re-implemented in subclasses.  This grabs one
        file, potentially from a remote location.  The URL for the
        base directory of some dataset is in dsurl, while the specific
        file is in urlmore.  The urlmore will be appended to the file
        part of dsurl via urljoin, and the resulting file will be
        transferred."""
        if logger is None: logger=self._logger
        parsed=urlparse.urlparse(dsurl)
        joined=urlparse.urljoin(dsurl,urlmore,allow_fragments=True)
        parsed=urlparse.urlparse(joined)
        if logger is not None:
            logger.info('%s + %s = %s',repr(dsurl),repr(urlmore),repr(joined))
        scheme=parsed.scheme
        path=parsed.path
        netpart=parsed.netloc
        n="%s://%s"%(scheme,netpart)
        if scheme== 'file':
            return self._impl_fetch_file(
                parsed,joined,scheme,path,netpart,streams,dc,dsurl,urlmore,dest,
                logger,timeout,realtime)
        elif scheme=='ftp':
            with self.locks[n]:
                return self._impl_fetch_ftp(
                    parsed,joined,scheme,path,netpart,streams,dc,dsurl,urlmore,dest,
                    logger,timeout,realtime)
        elif scheme=='sftp':
            return self._impl_fetch_sftp(
                parsed,joined,scheme,path,netpart,streams,dc,dsurl,urlmore,dest,
                logger,timeout,realtime)
        else:
            raise UnsupportedTransfer(
                'Cannot transfer this url: unsupported method (not htar, '
                'ftp, file or sftp): '+joined)
        return True
    def _impl_fetch_file(self,parsed,joined,scheme,path,netpart,streams,dc,dsurl,
                         urlmore,dest,logger,timeout,realtime):
        if logger is not None:
            logger.info('%s: from local file %s'%(dest,joined))
        if ( realtime and dc.rt_updated() ) or os.path.exists(path):
            makedirs(os.path.dirname(dest),logger=logger)
            make_symlink(path,dest,force=True,logger=logger)
        else:
            return False
            #produtil.fileop.deliver_file(path,dest,keep=True,logger=logger)
        return True
    def _impl_fetch_sftp(self,parsed,joined,scheme,path,netpart,streams,dc,dsurl,
                         urlmore,dest,logger,timeout,realtime):
        tempname=None
        try:
            makedirs(os.path.dirname(dest),logger=logger)
            with tempopen(dest,'wb') as f:
                tempname=f.name
            cmd=self._rsync_ssh_exe(netpart,path,tempname)
            checkrun(cmd,logger=logger)
            os.rename(tempname,dest)
            tempname=None
        except produtil.run.ExitStatusException as e:
            if logger is not None:
                logger.warning("%s: non-zero exit status %s"%(
                        joined,repr(e.returncode)))
            return False
        finally:
            if tempname is not None: os.remove(tempname)
        return True
    def _impl_fetch_ftp(self,parsed,joined,scheme,path,netpart,streams,dc,dsurl,
                        urlmore,dest,logger,timeout,realtime):
        n="%s://%s"%(scheme,netpart)
        if n not in streams:
            streams[n]=self.open_ftp(n,logger=logger,timeout=timeout)
        stream=streams[n]
        tempname=None
        try:
            makedirs(os.path.dirname(dest),logger=logger)
            with tempopen(dest,'wb') as f:
                tempname=f.name
                if logger is not None:
                    logger.info('%s: pull %s => %s'
                                %(n,parsed.path,tempname))
                stream.retrbinary("RETR "+parsed.path,f.write)
            remote_size=stream.size(parsed.path)
            if remote_size is not None:
                local_size=os.path.getsize(tempname)
                if local_size!=remote_size:
                    if logger is not None:
                        logger.warning(
                            '%s: wrong size: %d local vs %d remote'
                            %(tempname,local_size,remote_size))
                    raise PartialTransfer(
                        'Could not transfer full file: only %d of %d '
                        'bytes transferred.'%(local_size,remote_size))
            if logger is not None:
                logger.info('%s: move from %s'%(dest,tempname))
            os.rename(tempname,dest)
            tempname=None
        finally:
            if tempname is not None: os.remove(tempname)
        return True
    def list_for(self,realtime=True):
        if realtime:
            if not self._f_sorted: self._sort()
            return self.forecast
        else:
            if not self._h_sorted: self._sort()
            return self.history

    def _impl_get_archive(self,archpath,parts,done,prio, loc, parsed, dc,
                          data,target_dc,realtime,logger,skip_existing):
        with produtil.cd.TempDir(prefix="pull.",cd=False,
                                 keep_on_error=False) as td:
            assert(isinstance(td,produtil.cd.TempDir))
            assert(self.hsi is not None)
            if self.hsi is not None:
                i=self.hsi['get','-',':',archpath+'.idx']>"/dev/null"
                err=run(i,logger=logger)
                if err!=0:
                    logger.warning("%s.idx: exit status %d dumping index "
                                   "file. Htar will probably fail."
                                   %(archpath,int(err)))
            r=self.htar['-Hnostage','-xpf',archpath]\
                [ [p for p in parts.iterkeys()] ]\
                .cd(td.dirname)
            stat=run(r,logger=logger)
            if stat!=0:
                logger.info('non-zero exit status %d from htar; will retry '
                            'in five seconds.'%stat)
                time.sleep(5)
                stat=run(r,logger=logger)
            if stat!=0:
                logger.info('non-zero exit status %d from htar; will keep '
                            'going anyway'%stat)
            if logger is not None:
                logger.info("%s: pull %d files"
                            %(archpath,len(parts)))
            for (filepart,tgti) in parts.iteritems():
                tgt=tgti[0]
                src=os.path.join(td.dirname,filepart)
                if os.path.exists(src):
                    makedirs(os.path.dirname(tgt),logger=logger)
                    deliver_file(src,tgt,keep=False,logger=logger)
                    for i in tgti[1:]:
                        done.add(i)

    def _impl_get_file(self,i,done,src,tgt,prio, loc, parsed, dc,streams,
                       archives,data,target_dc,realtime,logger,skip_existing):
        archsep=src.find('#')
        if archsep>=0:
            # This is in an archive, so we will have to stage
            # the archive first, and get the file in the
            # second pass.
            arch=src[0:archsep]
            filepart=src[archsep+1:]
            if arch in archives and filepart in archives[arch]:
                archives[arch][filepart].append(i)
            else:
                archives[arch][filepart]=[tgt,i]
        else:
            if src[0:5]=='htar:':
                logger.warning("%s: no # in path - skipping this"
                               %(src,))
                return
            try:
                if self.fetch_file(
                    streams,dc,loc,src,tgt,
                    logger=logger,realtime=realtime):
                    done.add(i)
            except (EnvironmentError,ExitStatusException) as e:
                if logger is not None:
                    logger.warning(
                        'fetching %s=>%s: %s'%(str(src),str(tgt),
                                               str(e)),exc_info=True)

    def get(self,data,target_dc,realtime=False,logger=None,
            skip_existing=True):
        """Transfers the specified set of data to the specified
        target.  The "target_dc" is a DataCatalog that specifies the
        destination filenames.  The "realtime" argument is True for
        FORECAST (real-time) mode runs, and False for HISTORY
        (retrospective) mode runs.  The "data" argument should be an
        iterable (list, tuple, etc.) where each element is a dict-like
        object that describes one file to obtain.  Each dict contains:

          dataset - string name of the dataset (gfs, gdas1, gefs,
            enkf, etc.)
          item - string name of the object (ie.: sf, sfcanl, bufr)
          atime - Optional: a datetime.datetime specifying the
            analysis time.  Default is the atime from the
            InputSource's constructor.
          ftime - Optional: a datetime.datetime specifying the
            forecast time.
          ...others... - any other keyword arguments will be sent to
            the .location functions in any of this InputSource's
            DataCatalog objects."""  
        if logger is None: logger=self._logger
        dclist=self.list_for(realtime)
        done=set()
        for ( prio, loc, parsed, dc, dates ) in dclist:
            assert(loc is not None)
            assert(prio is not None)
            assert(parsed is not None)
            assert(dc is not None)
            assert(dates is not None)
            scheme=parsed.scheme
            netpart=parsed.netloc
            if scheme=='sftp':
                if not self.rsync_check_access(netpart,logger):
                    logger.error('%s: cannot access; will skip'%(netpart,))
                    continue
            elif scheme not in ['ftp','htar','file']:
                logger.error('%s: invalid transfer mode %s; will skip'
                             %(loc,scheme,))
                continue
            streams=dict()
            archives=collections.defaultdict(dict)
            workpool=None
            try:
                with produtil.workpool.WorkPool(3,logger) as workpool:
                    i=0
                    seen=set()
                    for d in data:
                        i+=1
                        if i in done: continue # skip files we already
                                               # transferred
                        assert('dates' not in d)
                        tgt=target_dc.locate(**d)
                        if tgt is None:
                            continue
                        if tgt in seen:
                            if logger is not None:
                                logger.info('%s: already processing this'%(tgt,))
                            continue
                        if os.path.exists(tgt) and skip_existing:
                            if logger is not None: 
                                logger.info('%s: already exists'%(tgt,))
                                done.add(i)
                                continue
                        if logger is not None:
                            logger.debug("%s => %s"%(repr(d),repr(tgt)))
                        src="(unknown)"
                        if logger is not None:
                            logger.debug('search for %s in %s'%(repr(d),repr(dc)))
                        try:
                            src=dc.locate(dates=dates,**d)
                        except KeyError as k:
                            logger.debug("%s: key error %s"%(src,str(k)))
                            continue
                        if src is None: continue
                        if logger is not None:
                            logger.info("SRC %s => %s"%(repr(d),repr(src)))
                        seen.add(tgt)
                        workpool.add_work(self._impl_get_file,args=[
                                i,done,src,tgt,prio, loc, parsed, dc,streams,
                                archives,data,target_dc,realtime,logger,
                                skip_existing])
                    workpool.barrier()
                    for (archpath,parts) in archives.iteritems():
                        if len(parts)<=0: 
                            if logger is not None:
                                logger.info("%s: nothing to pull; skip"
                                            %(archpath,))
                            continue
                        workpool.add_work(self._impl_get_archive,args=[
                                archpath,parts,done,prio, loc, parsed, dc,
                                data,target_dc,realtime,logger,skip_existing])
                    workpool.barrier()
            finally:
                for (key,stream) in streams.iteritems(): 
                    try:
                        stream.close()
                    except Exception as e:
                        if logger is not None:
                            logger.warning(
                                'Exception while closing stream %s: %s'
                                %(key,str(e)),exc_info=True)
            del workpool
        i=0
        bad=False
        for d in data:
            i+=1
            if i in done:
                continue
            tgt=target_dc.locate(**d)
            if os.path.exists(tgt):
                continue
            if d.get('optional',False):
                if logger is not None:
                    logger.info('missing optional data: %s'%(repr(d),))
            else:
                if logger is not None:
                    logger.warning('MISSING INPUT: %s'%(repr(d),))
                bad=True
        return not bad

    def get_one(self,dataset,item,dest,logger=None,timeout=20,realtime=True,
                **kwargs):
        """This is a simple wrapper around fetch_file that gets only
        one file.  It will fail if the file requires pulling an
        archive."""
        if logger is None: logger=self._logger
        streams=dict()
        try:
            dclist=list_for(realtime)
            for ( prio, loc, parsed, dc ) in dclist:
                src=dc.locate(dataset=dataset,item=item,**kwargs)
                if src is None: continue
                archsep=src.find('#')
                if archsep>=0:
                    raise NotImplementedError(
                        'Source is in an archive.  De-archiving is not '
                        'supported by "get_one."  Use "get" instead.')
                elif self.fetch_file(streams,dc,loc,src,dest,logger=logger): 
                    break
        finally:
            for (key,stream) in streams.iteritems(): 
                try:
                    stream.close()
                except Exception as e:
                    if logger is not None:
                        logger.warning(
                            'Exception while closing stream %s: %s'
                            %(key,str(e)),exc_info=True)
