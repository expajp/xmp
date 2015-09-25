program mpi_on_xmp_sorrb1_3d_1
  implicit none
  include 'mpif.h'

  ! mesh
  integer, parameter :: l = 100, m = 100, n = 129
  integer, parameter :: mesh = (l-1)*(m-1)*(n-1)
  integer, parameter :: sf = (l-1)*(m-1) ! sf:surface

  ! region
  double precision, parameter :: region_x_lower=0.0d0, region_x_upper=1.0d0
  double precision, parameter :: region_y_lower=0.0d0, region_y_upper=1.0d0
  double precision, parameter :: region_z_lower=0.0d0, region_z_upper=1.0d0

  ! border
  double precision, parameter :: border_x_lower=0.0d0, border_x_upper=0.0d0
  double precision, parameter :: border_y_lower=0.0d0, border_y_upper=0.0d0
  double precision, parameter :: border_z_lower=0.0d0 ! border_z_upper=sin(pi*x)sin(pi*y)

  ! constants
  double precision, parameter :: epsilon = 1.000E-08
  double precision, parameter :: pi = acos(-1.0d0)
  !double precision, parameter :: omega = 2.0d0/(1+sqrt(1-cos(pi/n)**2)) ! it must be from (1, 2)
  double precision, parameter :: omega = 1.8d0

  ! denominator
  double precision, parameter :: denomi = 1.0d0/sinh(sqrt(2.0d0)*pi)

  double precision :: region_x_length, region_y_length, region_z_length
  double precision :: h_x, h_y, h_z

  ! object of calc
  double precision, dimension(:), allocatable :: x, x_diff

  ! left-hand side
  double precision :: a_diag
  double precision, dimension(:), allocatable :: a_h_x_upper, a_h_y_upper, a_h_z_upper
  double precision, dimension(:), allocatable :: a_h_x_lower, a_h_y_lower, a_h_z_lower

  ! right_hand side
  double precision, dimension(:), allocatable :: b

  ! the upper of region of b inserted border's value
  integer :: b_border_region
  integer :: coef_h_x, coef_h_y, coef_h_z

  ! error check
  double precision :: norm_diff, norm_x, norm_b, norm_analysis
  double precision :: norm_diff_local, norm_x_local, norm_analysis_local
  double precision :: diff, diff_local
  double precision :: analysis_answer

  ! iteration
  integer :: i, j, count

  ! variables for time measurement
  !integer :: time0, time1, time2, t_rate, t_max ! sequential
  double precision :: time0, time1, time2, time3, time4, time5 ! parallel
  double precision :: tick, caltime, comtime, checktime, alltime


  ! variables for MPI
  integer :: myrank, nprocs, ierr
  integer, dimension(MPI_STATUS_SIZE) :: istat
  integer :: start, goal
  integer :: nstart, ngoal
  integer :: leftnode, rightnode


  ! initialization

  ! MPI
  call xmp_init_mpi()
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

  ! constants
  region_x_length = region_x_upper - region_x_lower ! = 1
  region_y_length = region_y_upper - region_y_lower ! = 1
  region_z_length = region_z_upper - region_z_lower ! = 1
  h_x = (region_x_length)/l ! 1/l
  h_y = (region_y_length)/m ! 1/m
  h_z = (region_z_length)/n ! 1/n
  
  ! matrix
  a_diag = -2.0d0*((1.0d0/h_x**2)+(1.0d0/h_y**2)+(1.0d0/h_z**2))
  a_h_x_upper = 0.0d0
  a_h_y_upper = 0.0d0
  a_h_z_upper = 0.0d0
  a_h_x_lower = 0.0d0
  a_h_y_lower = 0.0d0
  a_h_z_lower = 0.0d0

  ! matrix
  allocate(x(start-sf:goal+sf))
  allocate(x_diff(start:goal))

  allocate(a_h_x_upper(start:goal))
  allocate(a_h_y_upper(start:goal))
  allocate(a_h_z_upper(start:goal))
  allocate(a_h_x_lower(start:goal))
  allocate(a_h_y_lower(start:goal))
  allocate(a_h_z_lower(start:goal))

  allocate(b(start:goal))

  do i = start, goal

     if(mod(i,l-1) /= 0) a_h_x_upper(i) = 1.0d0/h_x**2
     if(mod(i,sf) <= (l-1)*(m-2)) a_h_y_upper(i) = 1.0d0/h_y**2
     if(i <= mesh-sf) a_h_z_upper(i) = 1.0d0/h_z**2

     if(mod(i,l-1) /= 1) a_h_x_lower(i) = 1.0d0/h_x**2
     if(mod(i,sf) > l-1) a_h_y_lower(i) = 1.0d0/h_y**2
     if(i > sf) a_h_z_lower(i) = 1.0d0/h_z**2

  end do

  ! right-hand side
  b = 0.0d0
  b_border_region = mesh-sf
  norm_b = 0.0d0
  coef_h_x = 0
  coef_h_y = 0
  coef_h_z = 0

  if(myrank == nprocs-1) then

     do i = b_border_region+1, mesh
        coef_h_x = mod(i-b_border_region-1,l-1) + 1
        coef_h_y = (i-b_border_region-1)/(l-1) + 1
        b(i) = -sin(coef_h_x*h_x*pi)*sin(coef_h_y*h_y*pi)/h_z**2
        norm_b = norm_b + b(i)**2
     end do

     norm_b = sqrt(norm_b)

  end if

  ! broadcast norm_b
  call mpi_bcast(norm_b, 1, MPI_REAL8, nprocs-1, MPI_COMM_WORLD, ierr)

  ! main loop
  count = 0
  norm_diff = 0.0d0
  norm_x = 0.0d0
  diff = 0.0d0
  norm_analysis = 0.0d0

  norm_diff_local = 0.0d0
  norm_x_local = 0.0d0
  diff_local = 0.0d0
  norm_analysis_local = 0.0d0

  do i = start-sf, goal+sf
     x(i) = 0.0d0
  end do

  caltime = 0.0d0
  comtime = 0.0d0
  checktime = 0.0d0
  alltime = 0.0d0

  tick = mpi_wtick()

  do
     call mpi_barrier(MPI_COMM_WORLD, ierr)
     time0 = mpi_wtime()

     ! calculate new vector
     do i = start, goal, 2
        x(i) = (b(i) - a_h_x_lower(i)*x(i-1) - a_h_x_upper(i)*x(i+1) &
                     - a_h_y_lower(i)*x(i-l+1) - a_h_y_upper(i)*x(i+l-1) &
                     - a_h_z_lower(i)*x(i-sf) - a_h_z_upper(i)*x(i+sf)) &
                * (omega/a_diag) + (1-omega)*x(i)
     end do

     call mpi_barrier(MPI_COMM_WORLD, ierr)
     time1 = mpi_wtime()

     ! message transfer to left
     ! start must be an odd number and computed
     call mpi_sendrecv(x(start), sf, MPI_REAL8, leftnode, 100, &
          x(goal+1), sf, MPI_REAL8, rightnode, 100, &
          MPI_COMM_WORLD, istat, ierr)

     call mpi_barrier(MPI_COMM_WORLD, ierr)
     time2 = mpi_wtime()

     ! calculate new vector
     do i = start+1, goal, 2
        x(i) = (b(i) - a_h_x_lower(i)*x(i-1) - a_h_x_upper(i)*x(i+1) &
                     - a_h_y_lower(i)*x(i-l+1) - a_h_y_upper(i)*x(i+l-1) &
                     - a_h_z_lower(i)*x(i-sf) - a_h_z_upper(i)*x(i+sf)) &
                * (omega/a_diag) + (1-omega)*x(i)
     end do

     call mpi_barrier(MPI_COMM_WORLD, ierr)
     time3 = mpi_wtime()

     ! message transfer to right
     ! goal must be an even number and computed
     call mpi_sendrecv(x(goal-sf+1), sf, MPI_REAL8, rightnode, 100, &
          x(start-sf), sf, MPI_REAL8, leftnode, 100, &
          MPI_COMM_WORLD, istat, ierr)

     call mpi_barrier(MPI_COMM_WORLD, ierr)
     time4 = mpi_wtime()
     
     ! calculate norm
     do i = start, goal
        x_diff(i) = b(i) - a_diag*x(i) &
                - a_h_x_lower(i)*x(i-1) - a_h_x_upper(i)*x(i+1) &
                - a_h_y_lower(i)*x(i-l+1) - a_h_y_upper(i)*x(i+l-1) &
                - a_h_z_lower(i)*x(i-sf) - a_h_z_upper(i)*x(i+sf)
     end do

     do i = start, goal
        norm_diff_local = norm_diff_local + x_diff(i)**2
        norm_x_local = norm_x_local + x(i)**2
     end do

     ! reduce norm_diff and norm_x
     call MPI_Allreduce(norm_diff_local, norm_diff, 1, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, ierr)
     call MPI_Allreduce(norm_x_local, norm_x, 1, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, ierr)

     norm_diff = sqrt(norm_diff)
     norm_x = sqrt(norm_x)

     ! preparation of next iter
     count = count+1

     ! stop clock
     call mpi_barrier(MPI_COMM_WORLD, ierr)
     time5 = mpi_wtime()

     ! calculate time
     alltime = alltime + (time5-time0)
     caltime = caltime + (time1-time0) + (time3-time2)
     comtime = comtime + (time2-time1) + (time4-time3)
     checktime = checktime + (time5-time4)

     ! shinchoku dou desuka?
     if(myrank == 0) write(*, '(i5, e15.5)') count, norm_diff/norm_b

     ! check convergence
     if(norm_diff <= epsilon*norm_b) exit

     ! preparation of next iteration
     norm_diff = 0.0d0
     norm_x = 0.0d0
     norm_diff_local = 0.0d0
     norm_x_local = 0.0d0
     
     call mpi_barrier(MPI_COMM_WORLD, ierr)

  end do

  ! compare with analysed answer here
  do i = start, goal
     coef_h_x = mod(i-1,l-1) + 1
     coef_h_y = mod(i-1, sf)/(l-1) + 1
     coef_h_z = (i-1)/sf + 1
     analysis_answer = sin(pi*coef_h_x*h_x)*sin(pi*coef_h_y*h_y)*sinh(sqrt(2.0d0)*pi*coef_h_z*h_z)*denomi

     norm_analysis_local = norm_analysis_local + analysis_answer**2

     diff_local = diff_local + abs(x(i)-analysis_answer)
  end do

  call MPI_Allreduce(norm_analysis_local, norm_analysis, 1, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, ierr)
  call MPI_Allreduce(diff_local, diff, 1, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, ierr)

  norm_analysis = sqrt(norm_analysis)

  ! output
  if(myrank == 0) then
     write(*, '(/,A,i6,/)') "iteration: ", count

     write(*,200) "epsilon = ", epsilon
     write(*,200) "omega = ", omega
     write(*,200) "tick = ", tick

     write(*, '(/,2(A, e15.5))') "norm_x = ", norm_x, ", norm_analysis = ", norm_analysis
     write(*, 200) "diff = ", diff

     write(*, '(/,3(A, f9.4))') "caltime = ", caltime, ", comtime = ", comtime, ", checktime = ", checktime
     write(*, '((A, f9.4),/)') "alltime = ", alltime
  end if

  ! 仮想マシンではこれがないとエラーを吐く
  ! 逆に、piではこれがあるとここの部分で処理が止まって実行が終わらない
  ! deallocate(x, x_diff)
  ! deallocate(a_h_x_upper, a_h_y_upper, a_h_z_upper)
  ! deallocate(a_h_x_lower, a_h_y_lower, a_h_z_lower)
  ! deallocate(b)

  call xmp_finalize_mpi()

100 format(2i4, X, f10.8)
200 format(A, e15.5)

end program mpi_on_xmp_sorrb1_3d_1

! 2015/09/22
! written by Shu OGAWARA
