#!/usr/bin/env python
#PBS -o /misc/whome/Biju.Thomas/PyMPIPOM/stdout
#PBS -j oe
#PBS -N /misc/whome/Biju.Thomas/PyMPIPOM/stdout
#PBS -d .
#PBS -A hur-uri
#PBS -l walltime=00:39:59
#PBS -l procs=9
#PBS -l partition=tjet:ujet

import produtil.setup
from pom.master import main

produtil.setup.setup()

#if main("IRENE","09L","2011082300"):
#if main("AMANDA","01E","2014052612"):
if main("HAIYAN","31W","2013110500"):
    print("OCEAN SPIN UP SUCCESS")
else:
    print("OCEAN SPIN UP FAILED")


