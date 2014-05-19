module sample_size

  integer, PARAMETER :: mimax=256, mjmax=128, mkmax=128

end module sample_size

program sample

  use sample_size
  use mpi

! Set Variables and Arrays
  integer :: i, j, k

  real :: x(mimax,mjmax,mkmax+1)
  real :: y(mimax,mjmax,mkmax+1)

! initialize for MPI
  integer :: myrank, nprocs, ierr
  integer :: kstart, kend, m
  integer :: leftnode, rightnode
  integer, dimension(MPI_STATUS_SIZE) :: istat ! for sendrecv

  call mpi_init(ierr)
  call mpi_comm_size(MPI_COMM_WORLD, nprocs, ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, myrank, ierr)

! initialize Variables for parallel

  kstart = mkmax*(myrank/nprocs)+1
  kend = mkmax*((myrank+1)/nprocs)
  m = mimax*mjmax*mkmax/nprocs

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

  do k=kstart, kend
     do j=1, mjmax
        do i=1, mimax
           x(i, j, k) = i + j + k
           y(i, j, k) = 0
        end do
     end do
  end do

! exchange values

!  call mpi_sendrecv(x(:,:,kstart), m, MPI_DOUBLE_PRECISION, leftnode, 100, &
!       x(:,:,kend+1), m, MPI_DOUBLE_PRECISION, rightnode, 100, &
!       MPI_COMM_WORLD, istat, ierr)

!  call mpi_sendrecv(x(:,:,kend), m, MPI_DOUBLE_PRECISION, rightnode, 100, &
!       x(:,:,kstart-1), m, MPI_DOUBLE_PRECISION, leftnode, 100, &
!       MPI_COMM_WORLD, istat, ierr)

! introduction

     write(*,'(A,i2,A)') 'The initialization of rank ',myrank,' was over.'

  if(myrank == 0) then
     write(*,*) 'Print only such matters that &
          the product of its i, j, and k is the multiple of 3^12'
  end if

! main loop

  do k=kstart, kend
     do j=2, mjmax-1
        do i=2, mimax-1
           ! substitute to y
              y(i, j, k) = x(i-1, j-1, k) + x(i+1, j+1, k)

           ! output
           if(mod(i*j*k,3**12) == 0) then
              write(*,'(A,3(i3,A),f5.1)') 'y(', i, ',', j, ',', k, ') = ',y(i, j, k)
           end if

        end do
     end do
  end do
  
  write(*,'(A,i2,A)') 'Rank', myrank, ', finished.'

! finalize MPI variables
  call mpi_finalize(ierr)

end program sample
