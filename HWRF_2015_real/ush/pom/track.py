#!/usr/bin/env python

"""
Written By Biju Thomas, GSO, University of Rhode Island on 6/11/14. 
This module has  a simple "get_vitals" function to extract vitals
from NHC vital file (syndat_tcvitals.${yyyy}). 

Please report bugs/questions/comments to bijuthomas@mail.uri.edu.
"""

__all__ = [ 'get_vitals' ]

import os.path
import re
from util import counter
from produtil.fileop import isnonempty
import logging

def get_vitals(vitalfile, centerid, stormid, yyyy, trackfile):
    try:
        vitals = []
        dates = []
        with open(vitalfile, "rt") as f: 
            for line in f:
                fields = re.split('[\s]\s*', line)
                if fields[0] == centerid.upper() and  \
                   fields[1] == stormid.upper():
                    if len(dates) == 0: 
                        dates.append(int(''.join([fields[3],fields[4]])))
                        vitals.append(line)
                    else:
                        if int(''.join([fields[3],fields[4]])) in dates:
                            n=dates.index(int( ''.join([fields[3],fields[4]])))
                            if len(line) >= len(vitals[n]):
                               dates[n] = int(''.join([fields[3],fields[4]]))
                               vitals[n] = line 
                        else:
                            dates.append(int(''.join([fields[3],fields[4]])))
                            vitals.append(line)     
            vitals = [y for (x,y) in sorted(zip(dates,vitals))]           
            #vitals.sort(key=dict(zip(vitals,dates)).get)
        c = counter()
        with open(trackfile, "wt") as f:
            for line in vitals:
                c.up()
                r1s = qck(line[69:73], line[73:78])
                r2s = qck(line[69:73], line[78:83])
                r3s = qck(line[69:73], line[83:88])
                r4s = qck(line[69:73], line[88:93])
                if len(line) <= 96:
                    f.write("%s%s%s%s%s%s%s%s \n"%(line[:19],line[21:36],line[37:42], \
                                                  line[43:73],r1s,r2s,r3s,r4s))
                else:
                    r5s = qck(line[69:73], line[95:100])
                    r6s = qck(line[69:73], line[100:105])
                    r7s = qck(line[69:73], line[105:110])
                    r8s = qck(line[69:73], line[110:115])
                    f.write("%s%s%s%s%s%s%s%s%s%s%s%s \n"%(line[:19],line[21:36],line[37:42], \
                                                  line[43:73],r1s,r2s,r3s,r4s,r5s,r6s,r7s,r8s))
            f.write("%s\n"%("000  000 00000     000000 0000 000 0000 000 000 0000 0000"))
        return c.value
    except IOError as err:
        print("%s" %err)
def qck(astr,bstr):
    if int(astr) > int(bstr) or int(bstr) == 999:
        nstr =  " -999"
    else:
        nstr =  bstr
    return nstr

def track_shorten(fin, fout, ymdh, logger=None):
    if logger is None: logger=logging.getLogger('pom')
    if not os.path.exists(fin) or not isnonempty(fin):
        logger.error("%s does not exists " %(fin))
        print("%s does not exists " %(fin))
        return 0
    with open(fin) as fid:
        with open(fout,'wt') as fo:
            c = counter()
            warn = True
            for l in fid:
                fo.write(l)
                c.up()
                if l[19:25] == ymdh[2:8] and l[26:28] == ymdh[8:10]:
                    warn = False
                    break
            fo.write("%s\n"%("000  000 00000     000000 0000 000 0000 000 000 0000 0000"))
    if (warn):
        logger.error("%s does not Found in %s"%(ymdh,fin))
        print("%s does not Found in %s"%(ymdh,fin))
        return 0
    else:
        return c.value

