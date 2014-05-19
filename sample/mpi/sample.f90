module sample_size
  implicit none

  integer, PARAMETER :: mimax=1024, mjmax=512, mkmax=512

end module sample_size

program sample
  use sample_size
  use mpi
  implicit none

! Set Variables and Arrays
  integer :: i, j, k

  real :: x(mimax,mjmax,mkmax)
  real :: y(mimax,mjmax,mkmax)

! Variables for MPI
  integer :: myrank, nprocs, ierr
  integer :: kstart, kend, m
  integer :: leftnode, rightnode
  real(8) :: cpu0, cpu1, cpu2
  integer, dimension(MPI_STATUS_SIZE) :: istat ! for sendrecv

  call mpi_init(ierr)
  call mpi_comm_size(MPI_COMM_WORLD, nprocs, ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, myrank, ierr)

! initialize Variables for parallel
  kstart = mkmax*myrank/nprocs+1
  kend = mkmax*(myrank+1)/nprocs
  m = mimax*mjmax

  if(myrank == 0) then
     leftnode = MPI_PROC_NULL
  else
     leftnode = myrank-1
  end if

  if(myrank == nprocs-1) then
     rightnode = MPI_PROC_NULL
  else
     rightnode = myrank+1
  end if

! initialize array
  cpu0 = MPI_Wtime()

  do k=kstart, kend
     do j=1, mjmax
        do i=1, mimax
           x(i, j, k) = i + j + k
           y(i, j, k) = 0
        end do
     end do
  end do

! exchange values

  call mpi_sendrecv(x(1,1,kstart), m, MPI_REAL, leftnode, 100, &
       x(1,1,kend+1), m, MPI_REAL, rightnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  call mpi_sendrecv(x(1,1,kend), m, MPI_REAL, rightnode, 100, &
       x(1,1,kstart-1), m, MPI_REAL, leftnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

! introduction
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    cpu1 = MPI_Wtime()

! main loop

  do k=kstart, kend
     do j=2, mjmax-1
        do i=2, mimax-1
           ! substitute to y
           if(kstart == 1) then
              y(i, j, k) = x(i+1, j+1, k+1)
           else if(kend == mkmax) then
              y(i, j, k) = x(i-1, j-1, k-1)
           else
              y(i, j, k) = x(i-1, j-1, k-1) + x(i+1, j+1, k+1)
           end if

           ! output
           if(i == 2 .and. j == 2 .and. k == kstart) then
              write(*,'(A,3(i3,A),f5.1)') 'y(', i, ',', j, ',', k, ') = ',y(i, j, k)
           end if

        end do
     end do
  end do
  
  !for debug
  !cpu2 = MPI_Wtime()
  !write(*,'(A,i3,A,f8.5,A)') 'Rank', myrank, ', finished in ', cpu2 - cpu0, 'sec.'

  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  cpu2 = MPI_Wtime()

  if(myrank == 0) then
     write(*,*)
     write(*,*) 'The time was counted.'
     write(*,'(A,f8.5)') 'Initialize (s): ',cpu1-cpu0
     write(*,'(A,f8.5)') 'Caliculate (s): ',cpu2-cpu1
     write(*,'(A,f8.5)') 'Total (s): ',cpu2-cpu0
     write(*,*)
!     write(*,'(f9.6)') cpu2-cpu0 ! for descripting a graph
  end if

! finalize MPI variables
  call mpi_finalize(ierr)

end program sample
