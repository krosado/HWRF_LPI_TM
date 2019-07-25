import os
import produtil.fileop,produtil.prog,produtil.mpiprog

from .mpi_impl_base import CMDFGen,MPIMixed

mpiexec_mpt_path=produtil.fileop.find_exe('mpiexec_mpt',raise_missing=False)

def openmp(arg,threads):
    if threads is not None:
        return arg.env(OMP_NUM_THREADS=threads)
    else:
        return arg

def detect():
    return mpiexec_mpt_path is not None

def guess_nthreads(default):
    omp=int(os.environ.get('OMP_NUM_THREADS',None))
    mkl=int(os.environ.get('MKL_NUM_THREADS',None))
    if omp is None and mkl is None:
        return default
    omp = (1 if omp is None else omp)
    mkl = (1 if mkl is None else mkl)
    return omp*mkl

def can_run_mpi():
    return True

def bigexe_prepend(arg,**kwargs): return []

def mpirunner(arg,allranks=False,**kwargs):
    assert(isinstance(arg,produtil.mpiprog.MPIRanksBase))
    (serial,parallel)=arg.check_serial()
    if serial and parallel:
        raise MPIMixed('Cannot mix serial and parallel MPI ranks in the same MPI program.')
    if arg.nranks()==1 and allranks:
        arglist=[ a for a in arg.to_arglist(
                pre=[mpiexec_mpt_path],
                before=['-n',os.environ['TOTAL_TASKS']],
                between=[':'])]
        runner=produtil.prog.Runner(arglist)
    elif allranks:
        raise MPIAllRanksError("When using allranks=True, you must provide an mpi program specification with only one MPI rank (to be duplicated across all ranks).")
    elif serial:
        lines=[a for a in arg.to_arglist(to_shell=True,expand=True)]
        if produtil.fileop.find_exe('mpiserial') is None:
            raise MPISerialMissing('Attempting to run a serial program via mpiexec_mpt but the mpiserial program is not in your $PATH.')
        runner=produtil.prog.Runner(
            [mpiexec_mpt_path,'-n','%s'%(arg.nranks()),'mpiserial'],
            prerun=CMDFGen('serialcmdf',lines,**kwargs))
    else:
        arglist=[ a for a in arg.to_arglist(
                pre=[mpiexec_mpt_path],   # command is mpiexec
                before=['-n','%(n)d'], # pass env, number of procs is kw['n']
                between=[':']) ] # separate commands with ':'
        runner=produtil.prog.Runner(arglist).env(MPI_TYPE_DEPTH=20)
    runner=runner.env(MPI_TYPE_DEPTH=20,MPI_BUFS_PER_PROC=256,MPI_BUFS_PER_HOST=1024)
    return runner
