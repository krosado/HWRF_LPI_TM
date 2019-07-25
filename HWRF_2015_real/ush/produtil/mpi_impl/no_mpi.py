def openmp(arg,threads):
    pass
def mpirunner(arg,**kwargs):
    raise MPIDisabled('This job cannot run MPI programs.')
def can_run_mpi():
    return False
def bigexe_prepend(arg,**kwargs): return []

