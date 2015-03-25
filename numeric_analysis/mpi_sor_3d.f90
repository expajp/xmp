program mpi_rbsor_3d
  use mpi
  implicit none

  ! mesh
  integer, parameter :: l = 100, m = 100, n = 129 ! こうしないとred-black化がきつい
  integer, parameter :: mesh = (l-1)*(m-1)*(n-1)
  integer, parameter :: sf = (l-1)*(m-1) ! sf: surface

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

  real(8), dimension(:), allocatable :: x, x_old, x_diff ! object of calc
  real(8), dimension(:), allocatable :: a_h_x, a_h_y, a_h_z, a_diag ! left-hand side
  real(8), dimension(:), allocatable :: b ! right_hand side
  integer :: b_border_region ! the upper of region of b inserted border's value
  integer :: coef_h_x, coef_h_y

  ! error check
  real(8) :: norm_diff_local, norm_x_local
  real(8) :: norm_diff, norm_x

  ! iteration
  integer :: i, count

  ! variables for MPI
  integer :: myrank, nprocs, ierr
  integer, dimension(MPI_STATUS_SIZE) :: istat
  integer :: start, goal
  integer :: nstart, ngoal
  integer :: leftnode, rightnode


  ! initialization

  ! MPI
  call mpi_init(ierr)
  call mpi_comm_size(MPI_COMM_WORLD, nprocs, ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, myrank, ierr)

  nstart = (n-1)*myrank/nprocs+1
  ngoal = (n-1)*(myrank+1)/nprocs

  start = (n-1)*myrank/nprocs*sf+1
  goal = ngoal*sf

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

  write(*, '(2(A,i4))') "nstart = ", nstart, ", ngoal = ", ngoal
  write(*, '(2(A,i7))') "start = ", start, ", goal = ", goal


  ! constants
  region_x_length = region_x_upper - region_x_lower ! = 1
  region_y_length = region_y_upper - region_y_lower ! = 1
  region_z_length = region_z_upper - region_z_lower ! = 1
  h_x = (region_x_length)/l ! 1/l
  h_y = (region_y_length)/m ! 1/m
  h_z = (region_z_length)/n ! 1/n
  
  ! matrix
  allocate(x(start-sf:goal+sf))
  allocate(x_old(start:goal))
  allocate(x_diff(start:goal))

  allocate(a_h_x(start-1:goal+1))
  allocate(a_h_y(start-l+1:goal))
  allocate(a_h_z(start-sf:goal))
  allocate(a_diag(start:goal))
  allocate(b(start:goal))

  a_diag = -2.0d0*((1.0d0/h_x**2)+(1.0d0/h_y**2)+(1.0d0/h_z**2)) ! startより前は参照しないためこれでOK
  a_h_x = 0.0d0
  a_h_y = 0.0d0
  a_h_z = 0.0d0

  do i = start-sf, goal
     if(i >= start-1 .and. mod(i,l-1) /= 0) a_h_x(i) = 1.0d0/h_x**2
     if(i >= start-l+1 .and. i <= mesh-l+1 .and. mod(i, n-1) <= (l-1)*(m-2)) a_h_y(i) = 1.0d0/h_y**2
     if(i <= mesh-sf) a_h_z(i) = 1.0d0/h_z**2
  end do

  ! goal%(l-1)=0であるため、条件式は不要
  a_h_x(goal+1) = 1.0d0/h_x**2


  ! right-hand side
  b = 0.0d0
  b_border_region = mesh-sf
  coef_h_x = 0
  coef_h_y = 0

  ! sf単位での並列化なので、b_border_region+1は最後のノード以外には存在し得ない
  if(myrank == nprocs-1) then
     do i = b_border_region+1, mesh
        coef_h_x = mod(i-b_border_region-1,l-1) + 1
        coef_h_y = (i-b_border_region-1)/(l-1) + 1
        b(i) = -sin(coef_h_x*h_x*pi)*sin(coef_h_y*h_y*pi)/h_z**2
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
     x_old = x

     ! calculate new vector
     do i = start, goal, 2 ! odd number
        x(i) = (b(i) - a_h_x(i-1)*x(i-1) - a_h_x(i)*x(i+1) &
                     - a_h_y(i-l+1)*x(i-l+1) - a_h_y(i)*x(i+l-1) &
                     - a_h_z(i-sf)*x(i-sf) - a_h_z(i)*x(i+sf)) &
                * (omega/a_diag(i)) + (1-omega)*x(i)
     end do

     ! message transfer
     call mpi_sendrecv(x(start), sf, MPI_REAL8, leftnode, 100, &
          x(goal+1), sf, MPI_REAL8, rightnode, 100, &
          MPI_COMM_WORLD, istat, ierr)


     do i = start+1, goal, 2 ! even number
        x(i) = (b(i) - a_h_x(i-1)*x(i-1) - a_h_x(i)*x(i+1) &
                     - a_h_y(i-l+1)*x(i-l+1) - a_h_y(i)*x(i+l-1) &
                     - a_h_z(i-sf)*x(i-sf) - a_h_z(i)*x(i+sf)) &
                * (omega/a_diag(i)) + (1-omega)*x(i)
     end do
     
     ! message transfer
     call mpi_sendrecv(x(goal-sf+1), sf, MPI_REAL8, rightnode, 100, &
          x(start-sf), sf, MPI_REAL8, leftnode, 100, &
          MPI_COMM_WORLD, istat, ierr)


     ! calculate norm
     x_diff = x - x_old
     do i = start, goal
        norm_diff_local = norm_diff_local + x_diff(i)**2
        norm_x_local = norm_x_local + x(i)**2
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
     if(mod(count,500) == 0 .and. myrank == 0) then
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
  ! write(*, *) "difference from analysis solution: ", diff

  ! output for check
  if(myrank == 0) then
     do i = 1, l-1
        write(*, '(i3, e15.5)') i, x((l-1)*((m-1)/2+1)+i) ! i = 1-99, j = 50, k = 1
     end do
  end if

  call mpi_finalize(ierr)

100 format(2i4, X, f10.8)

end program mpi_rbsor_3d

! 2015/02/06 first version
! written by Shu OGAWARA
