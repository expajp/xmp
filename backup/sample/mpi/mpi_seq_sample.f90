module sample_size
  implicit none

  integer, PARAMETER :: mimax=1024, mjmax=1024, mkmax=16

end module sample_size

program sample
  use sample_size
  use mpi
  implicit none

! Set Variables
  integer :: i, j, k

! Variables for MPI
  integer :: myrank, nprocs, ierr
  integer :: kstart, kend, m
  integer :: leftnode, rightnode
  real(8) :: cpu0, cpu1, cpu2, cpu3
  integer, dimension(4) :: reqs ! for nonblocking

! Set Allocatable Arrays
  real, dimension(:, :, :), allocatable :: x
  real, dimension(:, :, :), allocatable :: y

! initialize MPI
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

! Allocate and Initialize Arrays
  allocate(x(mimax, mjmax, kstart-1:kend+1))
  allocate(y(mimax, mjmax, kstart-1:kend+1))

  x = 0.0
  y = 0.0

! --- count start --- !
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  cpu0 = MPI_Wtime()

! initialize array
  do k=kstart-1, kend+1
     do j=1, mjmax
        do i=1, mimax
           x(i, j, k) = i + j + k
        end do
     end do
  end do

! --- count split 01 --- !
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    cpu1 = MPI_Wtime()

! message transfer

  ! to leftnode
  call mpi_send_init(x(1,1,kstart), m, MPI_REAL, leftnode, 100, &
       MPI_COMM_WORLD, reqs(1), ierr)

  ! to rightnode
  call mpi_send_init(x(1,1,kend), m, MPI_REAL, rightnode, 100, &
       MPI_COMM_WORLD, reqs(2), ierr)

  ! from leftnode
  call mpi_recv_init(x(1,1,kstart-1), m, MPI_REAL, leftnode, 100, &
       MPI_COMM_WORLD, reqs(3), ierr)

  ! from rightnode
  call mpi_recv_init(x(1,1,kend+1), m, MPI_REAL, rightnode, 100, &
       MPI_COMM_WORLD, reqs(4), ierr)

  ! start communication
  call mpi_startall(4, reqs, ierr)

  ! synchronization
  call mpi_waitall(4, reqs, MPI_STATUSES_IGNORE, ierr)

! --- count split 02 --- !
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    cpu2 = MPI_Wtime()

! main loop
  do k=kstart, kend
     do j=1, mjmax
        do i=1, mimax

           ! substitute to y
           y(i, j, k) = x(i, j, k-1) + x(i, j, k+1)

           ! output for debug
           !if(mod(i*j*k,3**15) == 0) then
           !   write(*,'(A,3(i3,A),f7.1)') 'y(', i, ',', j, ',', k, ') = ',y(i, j, k)
           !end if

        end do
     end do
  end do


! --- count stop --- !
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  cpu3 = MPI_Wtime()

! to protect a compiler to remove for-loop
  write(*, *) y(mimax, mjmax, kend)

! output
if(myrank == 0) then
!  write(*,'(A,f10.7)') 'Initialize (s): ',cpu1-cpu0
!  write(*,'(A,f10.7)') 'Caliculate (s): ',cpu2-cpu1
!  write(*,'(A,f10.7)') 'Total (s): ',cpu2-cpu0
  write(*,'(i3,X,4(f9.6,X))') mkmax, cpu1-cpu0, cpu2-cpu1, cpu3-cpu2, cpu3-cpu0 ! for descripting a graph
!  write(*,'(i3,X,3(f9.6,X))') nprocs, cpu2-cpu0 ! for descripting a graph
end if

! finalize MPI
  call mpi_finalize(ierr)

end program sample
