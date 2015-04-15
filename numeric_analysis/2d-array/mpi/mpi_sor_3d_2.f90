program mpi_sor_3d_2_widely_allocated
  use mpi
  implicit none

  ! mesh
  integer, parameter :: l = 100, m = 100, n = 129 ! 2^n 分割をしやすくするため
  integer, parameter :: mesh = (l-1)*(m-1)*(n-1)
  integer, parameter :: sf = (l-1)*(m-1) ! sf:surface

  ! region
  real(8), parameter :: region_x_lower=0.0d0, region_x_upper=1.0d0
  real(8), parameter :: region_y_lower=0.0d0, region_y_upper=1.0d0
  real(8), parameter :: region_z_lower=0.0d0, region_z_upper=1.0d0

  ! border
  real(8), parameter :: border_x_lower=0.0d0, border_x_upper=0.0d0
  real(8), parameter :: border_y_lower=0.0d0, border_y_upper=0.0d0
  real(8), parameter :: border_z_lower=0.0d0 ! border_z_upper=sin(pi*x)sin(pi*y)

  ! constants
  real(8), parameter :: epsilon = 1.000E-08
  real(8), parameter :: pi = acos(-1.0d0)
  real(8), parameter :: omega = 1.5 ! it must be from (1, 2)

  real(8) :: region_x_length, region_y_length, region_z_length
  real(8) :: h_x, h_y, h_z

  real(8) :: a_diag
  real(8), dimension(:, :), allocatable :: x, x_old, x_diff ! object of calc
  real(8), dimension(:, :), allocatable :: a_h_x_upper, a_h_y_upper, a_h_z_upper ! left-hand side
  real(8), dimension(:, :), allocatable :: a_h_x_lower, a_h_y_lower, a_h_z_lower ! left-hand side
  real(8), dimension(:, :), allocatable :: b ! right_hand side
  integer :: coef_h_x, coef_h_y

  ! error check
  real(8) :: norm_diff_local, norm_x_local
  real(8) :: norm_diff, norm_x

  ! iteration
  integer :: i, j, count

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

  start = (n-1)*myrank/nprocs+1
  goal = (n-1)*(myrank+1)/nprocs

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

  !write(*, '(3(A,i4))') "myrank = ", myrank, ", start = ", start, ", goal = ", goal

  ! constants
  region_x_length = region_x_upper - region_x_lower ! = 1
  region_y_length = region_y_upper - region_y_lower ! = 1
  region_z_length = region_z_upper - region_z_lower ! = 1
  h_x = (region_x_length)/l ! 1/l
  h_y = (region_y_length)/m ! 1/m
  h_z = (region_z_length)/n ! 1/n
  
  ! matrix
  allocate(x(-l+2:sf+l-1, start-1:goal+1))
  allocate(x_old(sf, start:goal))
  allocate(x_diff(sf, start:goal))

  allocate(a_h_x_upper(sf, start:goal))
  allocate(a_h_y_upper(sf, start:goal))
  allocate(a_h_z_upper(sf, start:goal))
  allocate(a_h_x_lower(sf, start:goal))
  allocate(a_h_y_lower(sf, start:goal))
  allocate(a_h_z_lower(sf, start:goal))

  allocate(b(sf, start:goal))

  a_diag = -2.0d0*((1.0d0/h_x**2)+(1.0d0/h_y**2)+(1.0d0/h_z**2))
  a_h_x_upper = 0.0d0
  a_h_y_upper = 0.0d0
  a_h_z_upper = 0.0d0
  a_h_x_lower = 0.0d0
  a_h_y_lower = 0.0d0
  a_h_z_lower = 0.0d0


  do j = start, goal
     do i = 1, sf
        if(mod(i,l-1) /= 0) a_h_x_upper(i, j) = 1.0d0/h_x**2
        if(i <= (l-1)*(m-2)) a_h_y_upper(i, j) = 1.0d0/h_y**2
        if(j /= 0 .and. j /= n-1) a_h_z_upper(i, j) = 1.0d0/h_z**2
     end do
  end do

  do j = start, goal
     do i = 1, sf
        if(mod(i,l-1) /= 1) a_h_x_lower(i, j) = 1.0d0/h_x**2
        if(i > l-1) a_h_y_lower(i, j) = 1.0d0/h_y**2
        if(j /= 1) a_h_z_lower(i, j) = 1.0d0/h_z**2
     end do
  end do

  ! right-hand side
  b = 0.0d0
  coef_h_x = 0
  coef_h_y = 0

  if(myrank == nprocs-1) then
     do i = 1, sf
        coef_h_x = mod(i-1, l-1) + 1
        coef_h_y = (i-1)/(l-1) + 1
        b(i, n-1) = -sin(coef_h_x*h_x*pi)*sin(coef_h_y*h_y*pi)/h_z**2
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
     ! x_old = x
     do j = start, goal
        do i = 1, sf
           x_old(i, j) = x(i, j)
        end do
     end do


     ! you don't need sync for the first sequence
     ! calculate new vector
     do j = start, goal
        do i = 1, sf
           x(i, j) = (b(i, j) &
                - a_h_x_lower(i, j)*x(i-1, j) - a_h_x_upper(i, j)*x(i+1, j) &
                - a_h_y_lower(i, j)*x(i-l+1, j) - a_h_y_upper(i, j)*x(i+l-1, j) &
                - a_h_z_lower(i, j)*x(i, j-1) - a_h_z_upper(i, j)*x(i, j+1) ) &
                * (omega/a_diag) + (1-omega)*x(i, j)
        end do
     end do
     
     ! message transfer
     call mpi_sendrecv(x(1, start), sf, MPI_REAL8, leftnode, 100, &
          x(1, goal+1), sf, MPI_REAL8, rightnode, 100, &
          MPI_COMM_WORLD, istat, ierr)

     call mpi_sendrecv(x(1, goal), sf, MPI_REAL8, rightnode, 100, &
          x(1, start-1), sf, MPI_REAL8, leftnode, 100, &
          MPI_COMM_WORLD, istat, ierr)


     ! calculate norm
     ! x_diff = x - x_old <-もしかしてXMPが動かない原因もこれか？
     do j = start, goal
        do i = 1, sf
           x_diff(i, j) = x(i, j) - x_old(i, j)
        end do
     end do

     do j = start, goal
        do i = 1, sf
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
        write(*, *) "iteration: ", count !, "relative error = ", norm_diff/norm_x
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
  ! write(*, *) "difference from analysis solution: ", diff

  ! output
  if(myrank == 0) then
     do i = 1, l-1
        write(*, '(i3, e15.5)') i, x((l-1)*((m-1)/2)+i, 1)
     end do
  end if

  ! 仮想マシンではこれがないとエラーを吐く
  ! 逆に、piではこれがあるとここの部分で処理が止まって実行が終わらない
  ! deallocate(x, x_old, x_diff)
  ! deallocate(a_h_x_upper, a_h_y_upper, a_h_z_upper)
  ! deallocate(a_h_x_lower, a_h_y_lower, a_h_z_lower)
  ! deallocate(b)

  call mpi_finalize(ierr)

100 format(2i4, X, f10.8)

  ! write(*, *) "myrank = ", myrank, " I finished execution on this node."

end program mpi_sor_3d_2_widely_allocated

! 2015/03/31
! written by Shu OGAWARA
