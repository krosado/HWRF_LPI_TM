__all__=['Listing']

import os,stat,StringIO,grp,pwd,time

class Listing(object):
    """Imitates ls -l, but with a longer mtime string.  
       print Listing("/usr/local")
    To include files whose names begin with "."  add hidden=True:
       print Listing("/usr/local",hidden=True)
    To log messages related to failures of lstat and readlink, pass 
    a logging.Logger:
       print Listing("/usr/local",hidden=True,logger=logger)"""
    def __init__(self,path=".",hidden=False,logger=None):
        self.__path=path
        self.__contents=dict()
        self.list(hidden=bool(hidden),logger=logger)
    def __iter__(self):
        """Iterates over filenames in the listed directory."""
        for name in self.contents.iterkeys():
            yield name
    def iteritems(self):
        """Iterates over name,data pairs in the listed directory.  The
        "data" will be a tuple containing the output of lstat and the
        output of readlink."""
        for (name,data) in self.contents.iteritems():
            yield name,data
    def iterkeys(self):
        """Iterates over filenames in the listed directory."""
        for name in self.contents.iterkeys():
            yield name

    def list(self,hidden=False,logger=None):
        """Updates the internal data structures with a new listing of
        the directory.  Arguments are the same as for the constructor."""
        hidden=bool(hidden)
        path=self.__path
        listing=os.listdir(path)
        contents=dict()
        for item in listing:
            if item[0]=='.' and not hidden: 
                # Skip "hidden" files.
                continue
            loc=os.path.join(path,item)
            try:
                lstat=os.lstat(loc)
            except EnvironmentError as e:
                if logger is not None:
                    logger.info(
                        "%s: cannot lstat: %s"%(loc,str(e)),exc_info=True)
                continue
            if not stat.S_ISLNK(lstat.st_mode):
                contents[item]=(lstat,None)
                continue
            try:
                linkpath=os.readlink(loc)
            except EnvironmentError as e:
                if logger is not None:
                    logger.info(
                        "%s: cannot readlink: %s"%(loc,str(e)),exc_info=True)
                contents[item]=(lstat,"(**UNKNOWN**)")
                continue
            contents[item]=(lstat,linkpath)
        self.__contents=contents
    def __str__(self):
        """Generates an ls -l style listing of the directory."""
        c=self.__contents
        sizes=[0,0,0,0,0,0]
        rows=list()
        for (name,item) in c.iteritems():
            row=self._stritem(name,item)
            for col in xrange(len(row)-1):
                sizes[col]=max(sizes[col],len(row[col]))
            rows.append(row)
        format=' %%%ds'*6+' %%s\n'
        format=format[1:]
        format=format%tuple(sizes)

        s=StringIO.StringIO()
        for row in rows:
            s.write(format%row)
        st=s.getvalue()
        s.close()
        return st
    def _stritem(self,name,item):
        """This is an internal implementation function.  Do not call
        it directly.  It generates one line of ls output as a tuple of
        strings, one string per column of information (the mtime is
        one column).  The __str__ turns the lines into one big string."""
        lstat=item[0]
        linkpath=item[1]
        if linkpath is None: linkpath='(**UNKNOWN**)'
        mode=lstat.st_mode

        s=''

        # file type character
        if   stat.S_ISDIR(mode):  s+='d'
        elif stat.S_ISCHR(mode):  s+='c'
        elif stat.S_ISBLK(mode):  s+='b'
        elif stat.S_ISFIFO(mode): s+='p'
        elif stat.S_ISLNK(mode):  s+='l'
        elif stat.S_ISSOCK(mode): s+='s'
        else:                     s+='-'

        # User permissions
        s+=('r' if mode&stat.S_IRUSR else '-')
        s+=('w' if mode&stat.S_IWUSR else '-')
        if mode&stat.S_IXUSR:
            if mode&stat.S_ISUID:
                s+='s'
            else:
                s+='x'
        elif mode&stat.S_ISUID:
            s+='S'
        else:
            s+='-'

        # Group permissions
        s+=('r' if mode&stat.S_IRGRP else '-')
        s+=('w' if mode&stat.S_IWGRP else '-')
        if mode&stat.S_IXGRP:
            if mode&stat.S_ISGID:
                s+='s'
            else:
                s+='x'
        elif mode&stat.S_ISGID:
            s+='S'
        else:
            s+='-'


        # Other permissions
        s+=('r' if mode&stat.S_IROTH else '-')
        s+=('w' if mode&stat.S_IWOTH else '-')
        if mode&stat.S_IXOTH:
            if mode&stat.S_ISVTX:
                s+='t'
            else:
                s+='x'
        elif mode&stat.S_ISVTX:
            s+='T'
        else:
            s+='-'

        nlink=str(int(lstat.st_nlink))
        username=self._username(lstat.st_uid)
        groupname=self._groupname(lstat.st_gid)
        size=str(int(lstat.st_size))
        when=time.asctime(time.localtime(lstat.st_mtime))
        if stat.S_ISLNK(lstat.st_mode):
            return (s,nlink,username,groupname,size,when,name+' -> '+linkpath)
        else:
            return (s,nlink,username,groupname,size,when,name)


    def _groupname(self,gid):
        try:
            gr=grp.getgrgid(gid)
            return str(gr.gr_name)
        except (KeyError,ValueError,EnvironmentError):
            return str(gid)

    def _username(self,uid):
        try:
            pw=pwd.getpwuid(uid)
            return str(pw.pw_name)
        except (KeyError,ValueError,EnvironmentError):
            return str(uid)
