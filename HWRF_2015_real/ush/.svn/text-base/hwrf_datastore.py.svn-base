#! /usr/bin/env python

import logging,sys
import produtil.datastore

from produtil.datastore import Datastore,TASK_CATEGORY,UNSTARTED,COMPLETED

def unfail(ds):
    with ds.transaction() as t:
        t.mutate("""UPDATE products SET available=? 
                      WHERE id LIKE '%s%%' AND 
                      NOT available=?"""%(TASK_CATEGORY),
                 (UNSTARTED,COMPLETED))

def unrun(ds):
    with ds.transaction() as t:
        t.mutate("UPDATE products SET available=?",(UNSTARTED,))

def unrun_one(ds,did):
    with ds.transaction() as t:
        taskid="%s::%s"%(TASK_CATEGORY,str(did))
        print>>sys.stderr,'Marking %s as unstarted.'%(taskid,)
        t.mutate("UPDATE products SET available=? WHERE id=?",
                 (UNSTARTED,taskid))

def dump(ds):
    ds.dump()

def usage(args):
    print>>sys.stderr,'''FORMAT: hwrf_datastore.py OP [ARG] file1.sqlite3 [file2.sqlite3 [...]]
where OP is one of:
  DUMP   - dump entire database to stdout
  UNFAIL - mark all failed or running tasks as unstarted
  UNRUN  - mark all tasks as unstarted
  UNRUN_ONE - see below
Only UNRUN_ONE takes an argument, and that argument is mandatory: the
task to "unrun".  The argument is the task id, which is everything in
the id after the %s:: in the output of ab hwrf_datastore.py DUMP.
'''%(TASK_CATEGORY,)
    for arg in args:
        print>>sys.stderr,arg
    sys.exit(2)

if len(sys.argv)<3: usage()

first_arg=2
opargs=[]
readonly=False
if   sys.argv[1].upper()=='UNFAIL':    op=unfail
elif sys.argv[1].upper()=='DUMP':      op=dump
elif sys.argv[1].upper()=='UNRUN':     op=unrun
elif sys.argv[1].upper()=='UNRUN_ONE':
    op=unrun_one
    opargs=[sys.argv[2]]
    first_arg=3
else:
    usage('Unrecognized datastore operation %s'%(sys.argv[1],))

if len(sys.argv)<first_arg+1:    usage()

if op is dump:   readonly=True

logging.basicConfig(stream=sys.stderr,level=logging.INFO)
for filename in sys.argv[first_arg:]:
    ds=Datastore(filename,logger=logging.getLogger(),locking=not readonly)
    with ds.transaction() as t:
        op(ds,*opargs)
