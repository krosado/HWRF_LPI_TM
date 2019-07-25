#!/usr/bin/env python

"""
Written By Biju Thomas, GSO, University of Rhode Island on 6/08/14.
Many of the functions here are for just testing the python based
MPIPOM scripts during its development, can be replaced with much 
sophisticated functions in the produtil modules. 
Please report bugs/questions/comments to bijuthomas@mail.uri.edu.
"""

__all__ = 'ysplitter', 'trailzero', 

import os
import re
import sys
import shutil
import os.path
import datetime
import subprocess

def ysplitter(ymdh):
    y4=ymdh[0:4]
    mm=ymdh[4:6]
    dd=ymdh[6:8]
    hh=ymdh[8:10]
    if int(dd) <= 15:
        mmn1 = int(mm) - 1
    else:
        mmn1 = int(mm) + 1
    if mmn1 <= 9:
       mm1str="0"+str(mmn1)
    else:
       mm1str=str(mmn1)
    return (y4,mm, mm1str,dd,hh)
def trailzero(astr, n): 
    """Returns a string of length n or greater, that consists of
    n-len(astr) zeros followed by astr"""
    ( '0'*max(0,int(n)-len(astr)) ) + astr
def inpfile(filename,lst,logger=None):
    if logger is not None:
        logger.info('%s: creating input file'%(filename,))
    with open(filename,"w") as fid:
        for mem in lst:
            fid.write(mem)
            fid.write("\n")
def remove(file):
    """Deletes the specified file.  Ignores errors."""
    if arg is None or arg=='':
        return
    if os.path.exists(arg):
        try:
            os.unlink(arg)
        except EnvironmentError:
            pass
def rmall(*args):
    """Deletes a list of files."""
    for arg in args:
        remove(arg)
def copy(src, dest, force=True):
    try:
        if os.path.exists(dest) and force:
            os.remove(dest)
    except os.error:
        pass
    try:
        if os.path.exists(dest) and os.stat(dest).st_size==0:
            os.remove(dest)
    except os.error:
        pass
    try:
        if not os.path.exists(dest): 
            if os.path.exists(src):
                shutil.copy(src, dest)
            else:
                print("%s does NOT exists NOT copied" %(src))
        else:
            print("%s does exists NOT copied" %(dest))
    except os.error:
        pass

def move(src, dest, force=True):
    try:
        if os.path.exists(dest) and force:
            os.remove(dest)
    except os.error:
        pass
    try:
        if os.path.exists(dest) and os.stat(dest).st_size==0:
            os.remove(dest)
    except os.error:
        pass
    try:
        if not os.path.exists(dest):
            if os.path.exists(src):
                shutil.move(src, dest)
            else:
                print("%s does NOT exists NOT moved" %(src))
        else:
            print("%s does exists NOT moved" %(dest))
    except os.error:
        pass
def link(src, dest):
    try:
        if os.path.exists(dest):
            os.remove(dest)
    except os.error:
        pass
    try:
        if os.path.exists(src):
            os.symlink(src, dest)
        else:
            print("%s does NOT exists NOT linked" %(src))
    except os.error:
        pass

def runexe(nproc, rundir, exefile, stdin, stdout):
    if not os.path.exists(rundir): 
        print("%s does NOT exists " %(rundir))   
        return -999
    os.chdir(rundir)
    if not os.path.exists(exefile):
        print("%s does NOT exists " %(exefile))
        return -999
    if  nproc <= 1:
        if os.path.exists(stdin):
            fin = open(stdin, 'r')
        else:
            fin = None
        with open(rundir+"/std.out", 'w') as fout:
            try:
                retcode=subprocess.call(exefile, stdin=fin,stdout=fout)          
            except OSError:
                print("Failed: (%s)" % exefile )
                pass
        return retcode
    else:
        with open(rundir+"/std.out", 'w') as fout:
            runstr="/apps/local/bin/mpiexec -np " + str(nproc) + "  " + exefile
            print(runstr)
            try:
                retcode=subprocess.call(runstr, stdout=fout, shell=True)
            except EnvironmentError as e:
                print("Failed: (%s): %s" %(exefile, str(e)))
                pass
        fout.close()
        return retcode
def read_input(filename):
    try:
        input = {}  
        with open(filename, 'rt') as f:
            for line in f:
                line=re.split(r'[;,:\s]\s*',line)
                input[line[1]]=line[0]
        return input
    except IOError:
        print("can NOT open (%s)" %filename)
def makenwrap(f):
    def decoraten(self,DEST):
        filename = DEST+"/pom.nml"
        asgn = " = "
        with open(filename, "w+") as file:
            arg = "&pom_nml"
            file.write("%s\n" % (arg))
            for k in range(len(self.keys)):
                file.write("\t %s %s %s\n" % (self.keys[k], asgn, \
                    self.namelist[self.keys[k]]))
            file.write("%s\n" %('/'))
        return f
    return decoraten
def dateplushours(ymdh, hours):
    if len(ymdh) == 10 and isinstance(ymdh, str)   \
                       and isinstance(hours, int):
           t=datetime.datetime(int(ymdh[0:4]),int(ymdh[4:6]),int(ymdh[6:8]),
                            int(ymdh[8:10]))
           t+=datetime.timedelta(hours=hours)
           return t.strftime('%Y%m%d%H')
    else:
        print("InputErr: ymdh hours in  dateplushours", ymdh, hours)

class counter:
    value = 0
    def set(self, x):
        self.value = x
    def up(self):
        self.value=self.value+1
    def down(self):
        self.value=self.value-1
def logi2int(l):
    assert(type(l) == type(True))
    if l:
       return 1
    else:
       return 0
jn2r=os.path.join
     
def veto():
    try:
        sys.exit(1)
    except SystemExit, value:
        print("caught exit(%s)" % value)
        return False
    try:
        os._exit(2)
    except SystemExit, value:
        print("caught exit(%s)" % value)
        return False
