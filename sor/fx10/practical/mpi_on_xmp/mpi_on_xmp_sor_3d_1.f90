program mpi_on_xmp_sor_3d_1
  implicit none
  include "mpif.h"

  ! mesh
  integer, parameter :: l = 100, m = 100, n = 129
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
  !real(8), parameter :: omega = 2.0d0/(1+sqrt(1-cos(pi/n)**2)) ! it must be from (1, 2)
  real(8), parameter :: omega = 1.8d0

  ! denominator
  real(8), parameter :: denomi = 1.0d0/sinh(sqrt(2.0d0)*pi)

  real(8) :: region_x_length, region_y_length, region_z_length
  real(8) :: h_x, h_y, h_z

  ! object of calc
  real(8) :: x(mesh), x_diff(mesh)

  ! left-hand side
  real(8) :: a_diag
  real(8) :: a_h_x_upper(mesh), a_h_y_upper(mesh), a_h_z_upper(mesh)
  real(8) :: a_h_x_lower(mesh), a_h_y_lower(mesh), a_h_z_lower(mesh)

  ! right_hand side
  real(8) :: b(mesh)

  ! the upper of region of b inserted border's value
  integer :: b_border_region
  integer :: coef_h_x, coef_h_y, coef_h_z

  ! error check
  real(8) :: norm_diff, norm_x, norm_b, norm_analysis
  real(8) :: diff
  real(8) :: analysis_answer

  ! iteration
  integer :: i, j, count

  ! variables for time measurement
  !integer :: time0, time1, time2, t_rate, t_max ! sequential
  real(8) :: time0, time1, time2, time3 ! parallel
  real(8) :: tick, caltime, comtime, checktime, alltime

  ! variables for XMP
  integer :: xmp_node_num, xmp_num_nodes
  real(8) :: xmp_wtick, xmp_wtime

  ! variables for MPI
  integer :: myrank, nprocs, ierr
  integer, dimension(2) :: ireq_send, ireq_recv
  integer, dimension(MPI_STATUS_SIZE) :: istat
  integer :: start, goal
  integer :: nstart, ngoal
  integer :: leftnode, rightnode


  ! initialization

  ! xmp
  !$xmp nodes p(*)
  !$xmp template t(mesh)
  !$xmp distribute t(block) onto p
  !$xmp align(k) with t(k) :: x, x_diff, b
  !$xmp align(k) with t(k) :: a_h_x_upper, a_h_y_upper, a_h_z_upper
  !$xmp align(k) with t(k) :: a_h_x_lower, a_h_y_lower, a_h_z_lower
  !$xmp shadow x(9801)

  myrank = xmp_node_num()-1
  nprocs = xmp_num_nodes()

  call xmp_init_mpi()

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

  ! initialization

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
  !$xmp loop on t(i)
  do i = 1, mesh

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

  !$xmp task on p(nprocs)

  do i = b_border_region+1, mesh
     coef_h_x = mod(i-b_border_region-1,l-1) + 1
     coef_h_y = (i-b_border_region-1)/(l-1) + 1
     b(i) = -sin(coef_h_x*h_x*pi)*sin(coef_h_y*h_y*pi)/h_z**2
     norm_b = norm_b + b(i)**2
  end do

  norm_b = sqrt(norm_b)

  !$xmp end task

  ! broadcast norm_b
  !$xmp bcast(norm_b) from p(nprocs)

  ! main loop
  count = 0
  norm_diff = 0.0d0
  norm_x = 0.0d0
  diff = 0.0d0
  norm_analysis = 0.0d0

  !$xmp loop on t(i)
  do i = 1, mesh
     x(i) = 0.0d0
  end do

  caltime = 0.0d0
  comtime = 0.0d0
  checktime = 0.0d0
  alltime = 0.0d0

  tick = xmp_wtick()

  do
     !$xmp barrier
     time0 = xmp_wtime()

     ! calculate new vector
     !$xmp loop on t(i)
     do i = 1, mesh
        x(i) = (b(i) - a_h_x_lower(i)*x(i-1) - a_h_x_upper(i)*x(i+1) &
                     - a_h_y_lower(i)*x(i-l+1) - a_h_y_upper(i)*x(i+l-1) &
                     - a_h_z_lower(i)*x(i-sf) - a_h_z_upper(i)*x(i+sf)) &
                * (omega/a_diag) + (1-omega)*x(i)
     end do

     !$xmp barrier
     time1 = xmp_wtime()

     ! message transfer
     call mpi_isend(x(start), sf, MPI_REAL8, leftnode, 100, MPI_COMM_WORLD, ireq_send(1), ierr)
     call mpi_isend(x(goal-sf+1), sf, MPI_REAL8, rightnode, 200, MPI_COMM_WORLD, ireq_send(2), ierr)

     call mpi_irecv(x(goal+1), sf, MPI_REAL8, rightnode, 100, MPI_COMM_WORLD, ireq_recv(1), ierr)
     call mpi_irecv(x(start-sf), sf, MPI_REAL8, leftnode, 200, MPI_COMM_WORLD, ireq_recv(2), ierr)

     ! 何故か動いてくれない
     !call mpi_waitall(2, ireq_recv, istat, ierr)
     !call mpi_waitall(2, ireq_send, istat, ierr)

     call mpi_wait(ireq_recv(1), istat, ierr)
     call mpi_wait(ireq_recv(2), istat, ierr)
     call mpi_wait(ireq_send(1), istat, ierr)
     call mpi_wait(ireq_send(2), istat, ierr)

     !$xmp barrier
     time2 = xmp_wtime()

     ! calculate norm
     !$xmp loop on t(i)
     do i = 1, mesh
        x_diff(i) = b(i) - a_diag*x(i) &
                - a_h_x_lower(i)*x(i-1) - a_h_x_upper(i)*x(i+1) &
                - a_h_y_lower(i)*x(i-l+1) - a_h_y_upper(i)*x(i+l-1) &
                - a_h_z_lower(i)*x(i-sf) - a_h_z_upper(i)*x(i+sf)
     end do

     !$xmp loop on t(i) reduction( + : norm_diff, norm_x )
     do i = 1, mesh
        norm_diff = norm_diff + x_diff(i)**2
        norm_x = norm_x + x(i)**2
     end do

     norm_diff = sqrt(norm_diff)
     norm_x = sqrt(norm_x)

     ! preparation of next iter
     count = count+1

     ! stop clock
     !$xmp barrier
     time3 = xmp_wtime()

     ! calculate time
     alltime = alltime + (time3-time0)
     caltime = caltime + (time1-time0)
     comtime = comtime + (time2-time1)
     checktime = checktime + (time3-time2)

     ! shinchoku dou desuka?
     !$xmp task on p(1)
     write(*, '(i5, e15.5)') count, norm_diff/norm_b
     !$xmp end task

     ! check convergence
     if(norm_diff <= epsilon*norm_b) exit

     ! preparation of next iteration
     norm_diff = 0.0d0
     norm_x = 0.0d0
     
     !$xmp barrier

  end do

  ! compare with analysed answer here
  !$xmp loop on t(i) reduction( + : norm_analysis, diff )
  do i = 1, mesh
     coef_h_x = mod(i-1,l-1) + 1
     coef_h_y = mod(i-1, sf)/(l-1) + 1
     coef_h_z = (i-1)/sf + 1
     analysis_answer = sin(pi*coef_h_x*h_x)*sin(pi*coef_h_y*h_y)*sinh(sqrt(2.0d0)*pi*coef_h_z*h_z)*denomi

     norm_analysis = norm_analysis + analysis_answer**2

     diff = diff + abs(x(i)-analysis_answer)
  end do

  norm_analysis = sqrt(norm_analysis)

  ! output
  !$xmp task on p(1)
     write(*, '(/,A,i6,/)') "iteration: ", count

     write(*,200) "epsilon = ", epsilon
     write(*,200) "omega = ", omega
     write(*,200) "tick = ", tick

     write(*, '(/,2(A, e15.5))') "norm_x = ", norm_x, ", norm_analysis = ", norm_analysis
     write(*, 200) "diff = ", diff

     write(*, '(/,3(A, f9.4))') "caltime = ", caltime, ", comtime = ", comtime, ", checktime = ", checktime
     write(*, '((A, f9.4),/)') "alltime = ", alltime
  !$xmp end task

  ! 仮想マシンではこれがないとエラーを吐く
  ! 逆に、piではこれがあるとここの部分で処理が止まって実行が終わらない
  ! deallocate(x, x_diff)
  ! deallocate(a_h_x_upper, a_h_y_upper, a_h_z_upper)
  ! deallocate(a_h_x_lower, a_h_y_lower, a_h_z_lower)
  ! deallocate(b)

100 format(2i4, X, f10.8)
200 format(A, e15.5)

  call xmp_finalize_mpi()

end program mpi_on_xmp_sor_3d_1

! 2015/10/11
! written by Shu OGAWARA
