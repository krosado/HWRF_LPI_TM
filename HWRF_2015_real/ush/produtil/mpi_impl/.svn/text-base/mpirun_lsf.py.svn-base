import os, socket, logging
import produtil.fileop,produtil.prog,produtil.mpiprog

from .mpi_impl_base import MPIMixed,CMDFGen

mpirun_lsf_path=produtil.fileop.find_exe('mpirun.lsf',raise_missing=False)

def openmp(arg,threads):
    if threads is not None:
        return arg.env(OMP_NUM_THREADS=threads)
    else:
        return arg

def detect():
    return mpirun_lsf_path is not None

def can_run_mpi():
    return True

def bigexe_prepend(arg,**kwargs): return []

def mpirunner(arg,allranks=False,logger=None,**kwargs):
    if logger is None:
        logger=logging.getLogger('mpirun_lsf')
    assert(isinstance(arg,produtil.mpiprog.MPIRanksBase))
    (serial,parallel)=arg.check_serial()
    if serial and parallel:
        raise MPIMixed(
            'Cannot mix serial and parallel MPI ranks in the same '
            'MPI program.')
    if arg.nranks()==1 and allranks:
        arglist=[ a for a in arg.to_arglist(
                pre=[mpirun_lsf_path],
                before=[],
                between=[])]
        return produtil.prog.Runner(arglist)
    elif arg.nranks()==1:
        # Hack to get LSF to run only one MPI rank.  Tested on NCEP
        # WCOSS supercomputer and approved by its IBM support staff.
        host=socket.gethostname()
        runner=produtil.prog.Runner(
            [ a for a in arg.to_arglist(
                    pre=[mpirun_lsf_path],
                    before=[], between=[]) ])
        runner=runner.env(
            LSB_PJL_TASK_GEOMETRY="{(0)}",LSB_HOSTS=host,
            LSB_MCPU_HOSTS=host+" 1", LSB_DJOB_NUMPROC='1',
            LSB_MAX_NUM_PROCESSORS='1',MP_TASK_AFFINITY='core')
        if logger is not None:
            logger.info(
                'Using a hack to work around an LSF bug and run a one core '
                'program: '+repr(runner))
        return runner
    elif allranks:
        raise MPIAllRanksError(
            "When using allranks=True, you must provide an mpi program "
            "specification with only one MPI rank (to be duplicated "
            "across all ranks).")
    elif serial:
        lines=[a for a in arg.to_arglist(to_shell=True,expand=True)]
        if produtil.fileop.find_exe('mpiserial') is None:
            raise MPISerialMissing(
                'Attempting to run a serial program via mpirun.lsf but '
                'the mpiserial program is not in your $PATH.')
        return produtil.prog.Runner([mpirun_lsf_path,'mpiserial'],
                                    prerun=CMDFGen('serialcmdf',lines,
                                                   cmd_envar='MP_CMDFILE',
                                                   model_envar='MP_PGMMODEL',
                                                   **kwargs))
    else:        
        lines=[a for a in arg.to_arglist(to_shell=True,expand=True)]
        return produtil.prog.Runner([mpirun_lsf_path],
                                    prerun=CMDFGen('mpirun_lsf_cmdf',lines,
                                                   cmd_envar='MP_CMDFILE',
                                                   model_envar='MP_PGMMODEL',
                                                   **kwargs))

