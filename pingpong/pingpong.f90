program pingpong
  use mpi
  implicit none

  integer :: nprocs, myrank, ierr
  integer :: istatus(MPI_STATUS_SIZE)
  integer, parameter :: n = 1000000
  integer :: i, datasize
  real(8) :: buf(n) = 0d0 
  real(8) :: t1, t2
  real(8) :: time, bandwidth

  call MPI_Init(ierr)

  call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)

  if(myrank == 0) write(*, '(a19,a19,a19)') 'Size (Byte),', 'Time (usec),', 'Bandwidth (MB/sec)'
  do i = 1, n, 1000
    t1 = MPI_wtime()

    if(myrank == 0) then
      call MPI_Send(buf, i, MPI_REAL8, 1, 1, MPI_COMM_WORLD, ierr)
      call MPI_Recv(buf, i, MPI_REAL8, 1, 1, MPI_COMM_WORLD, istatus, ierr)
    else
      call MPI_Recv(buf, i, MPI_REAL8, 0, 1, MPI_COMM_WORLD, istatus, ierr)
      call MPI_Send(buf, i, MPI_REAL8, 0, 1, MPI_COMM_WORLD, ierr)
    endif

    t2 = MPI_Wtime()

    if(myrank == 0) then
      datasize = i*8
      time = (t2-t1)/2*1.d6
      bandwidth = 2*datasize/(t2-t1)/1.d6
      write(*, '(i18,a,f18.2,a,f19.2)') datasize, ',', time, ',', bandwidth
    endif
  enddo

  call MPI_Finalize(ierr)

end program pingpong
