#! /usr/bin/env python

import sys, logging
from produtil.atparse import ATParser
       
p=ATParser(logger=logging.getLogger('streamparse'))
nodash=False
logging.basicConfig()
for file in sys.argv[1:]:
    if not nodash:
        if file=='--':
            nodash=True
            continue
        elif file=='-':
            p.parse_stream(sys.stdin,'(stdin)')
            continue
    p.parse_file(file)

