#! /bin/env python

"""This program converts HHS baseline cycle list fix files (sent to
stdin) to XML entities for use with Rocoto (sent to stdout).  It was
used to generate the rocoto/storms/H214.ent file."""

import sys, logging, os, collections
import produtil.setup
import hwrf.numerics

from hwrf.numerics import to_datetime, to_timedelta

produtil.setup.setup(ologlevel=None) # never log to stdout
logger=logging.getLogger('hhs2storms')

epsilon=to_timedelta(5)
six_hours=to_timedelta(6*3600)

storms=collections.defaultdict(set)

for line in sys.stdin:
    # Input line format:
    # BB NN YYYYMMDDHH model
    # AL 13 2016091312 H217
    line=line.rstrip().upper()
    basin=line[0:2]
    number=line[4:6]
    ymdh=line[8:18]
    storm=basin+number+ymdh[0:4]
    storms[storm].add(ymdh)

sortstorms=[ x for x in storms.iterkeys() ]
sortstorms.sort()

for storm in sortstorms:
    unsorted_cycles=storms[storm]
    cycles=list(unsorted_cycles)
    cycles = [ to_datetime(x) for x in cycles ]
    cycles.sort()
    basin2=storm[0:2].lower()
    number=storm[2:4]
    if basin2 == 'al':
        basin1='L'
    elif basin2 == 'ep':
        basin1='E'
    elif basin2 == 'cp':
        basin1='C'
    elif basin2 == 'wp':
        basin1='W'
    else:
        raise Exception(
            "Unsupported basin %s: only AL, EP, CP and WP have unambiguous "
            "one letter basins."%(basin2,))
    sys.stdout.write('<!ENTITY %ssid "%s%s">\n'%(storm,number,basin1))
    sys.stdout.write('<!ENTITY %ssidlc "%s%s">\n'
                     %(storm,number,basin1.lower()))
    sys.stdout.write('<!ENTITY %scyc "'%(storm,))
    first=cycles[0]
    last=cycles[0]
    sent=cycles[0]-six_hours
    for cycle in cycles:
        if to_datetime(cycle) > to_datetime(last)+six_hours+epsilon:
            # Found a break in the cycles
            sys.stdout.write('<cycledef>%s00 %s00 06:00:00</cycledef> '
                             %(first.strftime('%Y%m%d%H'),
                               last.strftime('%Y%m%d%H')))
            first=cycle
            last=cycle
            sent=cycle
        else:
            last=cycle
    if sent+epsilon < last:
        # Need to send the last group of cycles
        sys.stdout.write('<cycledef>%s00 %s00 06:00:00</cycledef> '
                         %(first.strftime('%Y%m%d%H'),
                           last.strftime('%Y%m%d%H')))
    sys.stdout.write('">\n')
