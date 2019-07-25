program mpi_example

  ! Rules of MPI:

  ! The entire program is parallel.  Before you call MPI_Init, the
  ! program is STILL PARALLEL.  That is different than OpenMP.  Even
  ! before MPI_Init, the program is parallel.  Even after
  ! MPI_Finalize, the program is still parallel.  You just cannot run
  ! the MPI routines until you call MPI_Init, or after you call
  ! MPI_Finalize.  If you want to run something only once, you have to
  ! put it in an "if(rank==0)" (or some other rank) and then broadcast
  ! the results that you need to all MPI ranks.

  ! You NEVER USE "STOP."  With some MPI implementations, that will
  ! cause the other MPI ranks to hang until the job reaches its
  ! wallclock limit.  You must exit the program with MPI_Abort or
  ! MPI_Finalize.  It is critical for our scripts that you use
  ! MPI_Finalize if the program succeeds, and MPI_Abort with a
  ! non-zero parameter if the program fails.  That ensures that the
  ! program exit value is 0 on success and non-zero on failure.  Also,
  ! MPI_Finalize will not always exit the program: you need to exit by
  ! reaching the end of the main program block.

  ! Don't do this:
  !    import "mpi.h"   ! never do this
  ! That disables type checking.  Use this instead:
  !    use mpi

  use mpi
  integer :: ierr
  integer :: rank,size
  integer :: input, isum

38 format('I am rank ',I0,' in a world of size ',I0,'.')
20 format('Rank ',I0,' read in an integer ',I0,'.')
30 format('Rank ',I0,' has an integer ',I0,'.')
40 format('Sum of numbers from ',I0,' to ',I0,' is ',I0,'.')
50 format('Please provide a non-negative integer.',A)

  call mpi_init(ierr) ! must be the first statement in the program!!
  call mpi_comm_rank(MPI_COMM_WORLD,rank,ierr)
  call mpi_comm_size(MPI_COMM_WORLD,size,ierr)

  print 38,rank,size

  if (rank==0) then
     read(5,*,end=1000,err=2000) input
     write(0,20) rank,input
  endif
  call mpi_bcast(input,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  write(0,30) rank,input

  if(input>0) then
     write(0,'(I0,": ",A)') rank,'Sum the MPI ranks via MPI_Reduce...'
     call mpi_reduce(rank,isum,1,MPI_INTEGER,MPI_SUM,0,MPI_COMM_WORLD,ierr)
     if(rank==0) then
        write(0,40) 0,size-1,isum
     endif
  else
     if(rank==0) then
        write(0,50) ' '
     endif
     call mpi_abort(MPI_COMM_WORLD,3,ierr) ! will exit the program with
                                           ! non-zero status
  endif

  write(0,'(I0,": no error, so mpi_finalize")') rank
  call mpi_finalize(ierr) ! Terminates MPI support, does not exist program

  goto 3000 ! skip error handling code

1000 continue
1001 format('Rank ',I0,': unexpected end of file reading from stdin.')
     write(0,1001) rank
     call mpi_abort(MPI_COMM_WORLD,5,ierr) ! will exit program with
                                           ! non-zero status
2000 continue
2001 format('Rank ',I0,': error reading from stdin.')
     write(0,2001) rank
     call mpi_abort(MPI_COMM_WORLD,10,ierr) ! will exit program with
                                            ! non-zero status
3000 continue ! jump here when there is no error
end program mpi_example
