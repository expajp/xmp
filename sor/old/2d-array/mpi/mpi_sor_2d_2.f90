program mpi_sor_2d
  use mpi
  implicit none

  ! mesh
  integer, parameter :: l = 100, m = 129
  integer, parameter :: mesh = (l-1)*(m-1)

  ! region
  real(8), parameter :: region_x_lower=0.0d0, region_x_upper=1.0d0
  real(8), parameter :: region_y_lower=0.0d0, region_y_upper=1.0d0

  ! border
  real(8), parameter :: border_x_lower=0.0d0, border_x_upper=1.0d0
  real(8), parameter :: border_y_lower=0.0d0 ! border_y_upper=sin(pi*x)

  ! constants
  real(8), parameter :: epsilon = 1.000E-8
  real(8), parameter :: pi = acos(-1.0d0)
  real(8), parameter :: denomi = 1.0d0/(exp(pi)-exp(-pi))
  real(8), parameter :: omega = 1.5 ! it must be from (1, 2)

  real(8) :: region_x_length, region_y_length
  real(8) :: h_x, h_y

  real(8) :: a_diag
  real(8), dimension(:, :), allocatable :: x, x_old, x_diff ! object of calc
  real(8), dimension(:, :), allocatable :: a_h_x, a_h_y ! left-hand side
  real(8), dimension(:, :), allocatable :: b ! right_hand side
  
  ! error check
  real(8) :: norm_diff, norm_x
  real(8) :: norm_diff_local, norm_x_local

  ! compare with analysis
  real(8) :: diff, diff_local
  real(8) :: row_d, column_d

  integer :: i, j ! iteration
  integer :: count

  ! variables for MPI
  integer :: myrank, nprocs, ierr
  integer, dimension(MPI_STATUS_SIZE) :: istat
  integer :: start, goal
  integer :: leftnode, rightnode


  ! initialization

  ! MPI
  call mpi_init(ierr)
  call mpi_comm_size(MPI_COMM_WORLD, nprocs, ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, myrank, ierr)

  start = (m-1)*myrank/nprocs+1
  goal = (m-1)*(myrank+1)/nprocs

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

  allocate(x(l-1, start-1:goal+1))
  allocate(x_old(l-1, start:goal))
  allocate(x_diff(l-1, start:goal))

  allocate(a_h_x(l-1, start:goal))
  allocate(a_h_y(l-1, start-1:goal))
  allocate(b(l-1, start:goal))

  ! constants
  region_x_length = region_x_upper - region_x_lower ! = 1
  region_y_length = region_y_upper - region_y_lower ! = 1
  h_x = (region_x_length)/l ! 1/l
  h_y = (region_y_length)/m ! 1/m
  
  ! matrix
  a_diag = -2.0d0*((1/h_x**2)+(1/h_y**2))
  a_h_x = 0.0d0
  a_h_y = 0.0d0

  do j = start, goal
     do i = 1, l-1
        if(j <= m-2) a_h_y(i, j) = 1.0d0/h_y**2
        if(i /= l-1) a_h_x(i, j) = 1.0d0/h_x**2
     end do
  end do

  ! message-sending
  call mpi_sendrecv(a_h_y(1, goal), l-1, MPI_REAL8, rightnode, 100, &
       a_h_y(1, start-1), l-1, MPI_REAL8, leftnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  ! right-hand side
  b = 0.0d0

  if(myrank == nprocs-1) then
     do i = 1, l-1
        b(i, m-1) = -sin(i*h_x*pi)/h_y**2
     end do
  end if

  ! main loop
  count = 0
  norm_diff = 0.0d0
  norm_x = 0.0d0
  norm_diff_local = 0.0d0
  norm_x_local = 0.0d0

  x = 0.0d0
  x_old = 0.0d0

  if(myrank == 0) write(*,*) "epsilon = ", epsilon

  do
     ! x_old = x <-バグの温床
     do j = start, goal
        do i = 1, l-1
           x_old(i, j) = x(i, j)
        end do
     end do

     ! calculate new vector
     do j = start, goal
        do i = 1, l-1
           x(i, j) = (b(i, j) &
                - a_h_x(i, j)*x(i+1, j) - a_h_x(i-1, j)*x(i-1, j) &
                - a_h_y(i, j)*x(i, j+1) - a_h_y(i, j-1)*x(i, j-1)) &
                * (omega/a_diag) + (1-omega)*x(i, j)
        end do
     end do
     
     ! message-sending
     call mpi_sendrecv(x(1, goal), l-1, MPI_REAL8, rightnode, 100, &
          x(1, start-1), l-1, MPI_REAL8, leftnode, 100, &
          MPI_COMM_WORLD, istat, ierr)

     call mpi_sendrecv(x(1, start), l-1, MPI_REAL8, leftnode, 100, &
          x(1, goal+1), l-1, MPI_REAL8, rightnode, 100, &
          MPI_COMM_WORLD, istat, ierr)


     ! calculate norm

     ! x_diff = x - x_old <-バグの温床
     do j = start, goal
        do i = 1, l-1
           x_diff(i, j) = x(i, j) - x_old(i, j)
        end do
     end do

     do j = start, goal
        do i = 1, l-1
           norm_diff_local = norm_diff_local + x_diff(i, j)**2
           norm_x_local = norm_x_local + x(i, j)**2
        end do
     end do

     ! reduce norm_diff and norm_x
     call MPI_Allreduce(norm_diff_local, norm_diff, 1, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, ierr)
     call MPI_Allreduce(norm_x_local, norm_x, 1, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, ierr)

     norm_diff = sqrt(norm_diff)
     norm_x = sqrt(norm_x)

     ! check convergence
     if(norm_diff <= epsilon*norm_x) exit
     
     ! preparation of next iter
     count = count+1

     ! shinchoku dou desuka?
     if(myrank == 0 .and. mod(count,500) == 0) then
        write(*, *) "iteration: ", count
        write(*, *) "relative error = ", norm_diff/norm_x
     end if

     ! preparation of next iteration
     norm_diff = 0.0d0
     norm_x = 0.0d0
     norm_diff_local = 0.0d0
     norm_x_local = 0.0d0
     
     call mpi_barrier(MPI_COMM_WORLD, ierr)

  end do

  call mpi_barrier(MPI_COMM_WORLD, ierr)

  ! output
  if(myrank == 0) write(*, *) "iteration: ", count

  ! compare with analysed answer here
  diff = 0.0d0
  diff_local = 0.0d0

  do j = start, goal
     do i = 1, l-1
        row_d = dble(i)
        column_d = dble(j)
        diff_local = diff_local + abs(x(i, j) - (sin(pi*h_x*row_d)*(exp(pi*h_y*column_d)-exp(-pi*h_y*column_d))*denomi))
        ! write(*, 100) i, j, x(i, j)
     end do
  end do

  call MPI_Allreduce(diff_local, diff, 1, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, ierr)

  if(myrank == 0) write(*, *) "difference from analysis solution: ", diff

  deallocate(x, x_old, x_diff)
  deallocate(a_h_x, a_h_y)
  deallocate(b)

  call mpi_finalize(ierr)


100 format(2i4, X, f10.8)

end program mpi_sor_2d

! 2015/04/02
! written by Shu OGAWARA
