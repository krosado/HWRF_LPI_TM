#! /bin/env python

import sys, logging, collections
import produtil.setup
import hwrf.revital, hwrf.numerics

from hwrf.numerics import to_timedelta, to_datetime
produtil.setup.setup()

renumber=True
args=sys.argv[1:]

if args[0]=='-n':
    renumber=False
    args=args[1:]

if len(args)<1:
    print>>sys.stderr, """ERROR: Format: rocoto-from-vitals.py [-n] /path/to/vitals-file1 [/path/to/vitals-file2 [...]]
 -n = disable renumbering (do this for invests)

ERROR: Script requires at least one argument."""
    sys.exit(1)

logger=logging.getLogger('rocoto_from_vitals')
revital=hwrf.revital.Revital(logger=logger)
logger.info('Read input files: '+repr(args))
revital.readfiles(args,raise_all=False)
if renumber:
    logger.info('Renumber invests.')
    revital.renumber()
else:
    logger.info('Not renumbering vitals.  Just clean them up')
    revital.clean_up_vitals()

basins=dict()
numbers=dict()
storms=collections.defaultdict(set)
epsilon=to_timedelta(5)
six_hours=to_timedelta(6*3600)

for vit in revital:
    storms[vit.longstormid].add(vit.when.strftime('%Y%m%d%H'))
    basins[vit.longstormid]=vit.basin1.upper()
    numbers[vit.longstormid]=vit.stnum

sortstorms=[ x for x in storms.iterkeys() ]
sortstorms.sort()

for storm in sortstorms:
    unsorted_cycles=storms[storm]
    cycles=list(unsorted_cycles)
    cycles = [ to_datetime(x) for x in cycles ]
    cycles.sort()
    basin1=basins[storm]
    number=numbers[storm]

    if number>=50:
        sys.stderr.write("Skipping %02d%s\n"%(number,basin1))
        continue

    sys.stdout.write('<!ENTITY %ssid "%02d%s">\n'%(storm,number,basin1))
    sys.stdout.write('<!ENTITY %ssidlc "%02d%s">\n'
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


