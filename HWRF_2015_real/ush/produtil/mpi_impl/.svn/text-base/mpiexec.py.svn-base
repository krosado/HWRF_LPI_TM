"""This module is part of the mpi_impl package -- see __init__.py for
details.  This implements the Hydra MPI with Intel OpenMP, but may
work for other MPI implementations that use the "mpiexec" command and
OpenMP implementations that use the KMP_NUM_THREADS or OMP_NUM_THREADS
environment variables.

HOWEVER, this module assumes the TOTAL_TASKS environment variable is
set to the maximum number of MPI ranks the program has available to
it.  That is used when the mpirunner is called with the allranks=True
option."""

import os
import produtil.fileop,produtil.prog,produtil.mpiprog

from .mpi_impl_base import MPIMixed,CMDFGen

mpiexec_path=produtil.fileop.find_exe('mpiexec',raise_missing=False)

def openmp(arg,threads):
    if threads is not None:
        return arg.env(OMP_NUM_THREADS=threads,KMP_NUM_THREADS=threads,
                       KMP_AFFINITY='scatter')
    else:
        return arg

def detect():
    return mpiexec_path is not None

def can_run_mpi():
    return True

def bigexe_prepend(arg,**kwargs): return []

def mpirunner(arg,allranks=False,**kwargs):
    assert(isinstance(arg,produtil.mpiprog.MPIRanksBase))
    (serial,parallel)=arg.check_serial()
    if serial and parallel:
        raise MPIMixed('Cannot mix serial and parallel MPI ranks in the '
                       'same MPI program.')
    if arg.nranks()==1 and allranks:
        arglist=[ a for a in arg.to_arglist(
                pre=[mpiexec_path],
                before=['-np',os.environ['TOTAL_TASKS']],
                between=[':'])]
        return produtil.prog.Runner(arglist)
    elif allranks:
        raise MPIAllRanksError(
            "When using allranks=True, you must provide an mpi program "
            "specification with only one MPI rank (to be duplicated across "
            "all ranks).")
    elif serial:
        lines=[a for a in arg.to_arglist(to_shell=True,expand=True)]
        if produtil.fileop.find_exe('mpiserial') is None:
            raise MPISerialMissing(
                'Attempting to run a serial program via mpiexec but the '
                'mpiserial program is not in your $PATH.')
        return produtil.prog.Runner(
            [mpiexec_path,'-np','%s'%(arg.nranks()),'mpiserial'],
            prerun=CMDFGen('serialcmdf',lines,**kwargs))
    else:
        arglist=[ a for a in arg.to_arglist(
                pre=[mpiexec_path],   # command is mpiexec
                before=['-np','%(n)d'], # pass env, number of procs is kw['n']
                between=[':']) ] # separate commands with ':'
        return produtil.prog.Runner(arglist)

