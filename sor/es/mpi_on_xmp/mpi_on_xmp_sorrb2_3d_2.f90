program mpi_on_xmp_sorrb2_3d_2
  implicit none
  include "mpif.h"

  ! mesh
  integer, parameter :: l = 1000, m = 1000, n = 129
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
  real(8) :: pi
  !real(8), parameter :: omega = 2.0d0/(1+sqrt(1-cos(pi/n)**2)) ! it must be from (1, 2)
  real(8), parameter :: omega = 1.8d0

  ! denominator
  real(8) :: denomi

  real(8) :: region_x_length, region_y_length, region_z_length
  real(8) :: h_x, h_y, h_z

  ! object of calc
  real(8) :: x(sf, n-1), x_diff(sf, n-1)

  ! left-hand side
  real(8) :: a_diag
  real(8) :: a_h_x_upper(sf, n-1), a_h_y_upper(sf, n-1), a_h_z_upper(sf, n-1)
  real(8) :: a_h_x_lower(sf, n-1), a_h_y_lower(sf, n-1), a_h_z_lower(sf, n-1)

  ! right_hand side
  real(8) :: b(sf, n-1)

  ! the upper of region of b inserted border's value
  integer :: coef_h_x, coef_h_y, coef_h_z

  ! error check
  real(8) :: norm_diff, norm_x, norm_b
  real(8) :: norm_analysis
  real(8) :: diff
  real(8) :: analysis_answer

  ! iteration
  integer :: i, j, count

  ! variables for time measurement
  !integer :: time0, time1, time2, t_rate, t_max ! sequential
  real(8) :: time0, time1, time2, time3, time4, time5 ! parallel
  real(8) :: tick, caltime, comtime, checktime, alltime

  ! variables for XMP
  integer :: xmp_node_num, xmp_num_nodes
  real(8) :: xmp_wtick, xmp_wtime

  ! variables for MPI
  integer :: myrank, nprocs, ierr
  integer, dimension(2) :: ireq_send, ireq_recv
  integer, dimension(MPI_STATUS_SIZE) :: istat
  integer :: start, goal
  integer :: leftnode, rightnode


  ! initialization

  ! xmp
  !$xmp nodes p(*)
  !$xmp template t(n-1)
  !$xmp distribute t(block) onto p
  !$xmp align(*, k) with t(k) :: x, x_diff, b
  !$xmp align(*, k) with t(k) :: a_h_x_upper, a_h_y_upper, a_h_z_upper
  !$xmp align(*, k) with t(k) :: a_h_x_lower, a_h_y_lower, a_h_z_lower
  !$xmp shadow x(0, 1)

  call xmp_init_mpi()

  myrank = xmp_node_num()-1
  nprocs = xmp_num_nodes()

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
  pi = acos(-1.0d0)
  denomi = 1.0d0/sinh(sqrt(2.0d0)*pi)

  !$xmp loop on t(j)
  do j = 1, n-1
     do i = 1, sf

        if(mod(i,l-1) /= 0) a_h_x_upper(i, j) = 1.0d0/h_x**2
        if(i <= (l-1)*(m-2)) a_h_y_upper(i, j) = 1.0d0/h_y**2
        if(j /= n-1) a_h_z_upper(i, j) = 1.0d0/h_z**2

        if(mod(i,l-1) /= 1) a_h_x_lower(i, j) = 1.0d0/h_x**2
        if(i > l-1) a_h_y_lower(i, j) = 1.0d0/h_y**2
        if(j /= 1) a_h_z_lower(i, j) = 1.0d0/h_z**2

     end do
  end do

  ! right-hand side
  b = 0.0d0
  norm_b = 0.0d0
  coef_h_x = 0
  coef_h_y = 0
  coef_h_z = 0

  !$xmp task on p(nprocs)
  
  do i = 1, sf
     coef_h_x = mod(i-1,l-1) + 1
     coef_h_y = (i-1)/(l-1) + 1
     b(i, n-1) = -sin(coef_h_x*h_x*pi)*sin(coef_h_y*h_y*pi)/h_z**2
     norm_b = norm_b + b(i, n-1)**2
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

  x = 0.0d0

  caltime = 0.0d0
  comtime = 0.0d0
  checktime = 0.0d0
  alltime = 0.0d0

  tick = xmp_wtick()

  do
     !$xmp barrier
     time0 = xmp_wtime()

     ! calculate new vector
     !$xmp loop on t(j)
     do j = 1, n-1, 2
        do i = 1, sf, 2
           x(i, j) = (b(i, j) &
                - a_h_x_lower(i, j)*x(i-1, j) - a_h_x_upper(i, j)*x(i+1, j) &
                - a_h_y_lower(i, j)*x(i-l+1, j) - a_h_y_upper(i, j)*x(i+l-1, j) &
                - a_h_z_lower(i, j)*x(i, j-1) - a_h_z_upper(i, j)*x(i, j+1)) &
                * (omega/a_diag) + (1-omega)*x(i, j)
        end do

        do i = 2, sf, 2
           x(i, j+1) = (b(i, j+1) &
                - a_h_x_lower(i, j+1)*x(i-1, j+1) - a_h_x_upper(i, j+1)*x(i+1, j+1) &
                - a_h_y_lower(i, j+1)*x(i-l+1, j+1) - a_h_y_upper(i, j+1)*x(i+l-1, j+1) &
                - a_h_z_lower(i, j+1)*x(i, j) - a_h_z_upper(i, j+1)*x(i, j+2)) &
                * (omega/a_diag) + (1-omega)*x(i, j+1)
        end do
     end do

     !$xmp barrier
     time1 = xmp_wtime()

     ! message transfer
     call mpi_isend(x(1, start), sf, MPI_REAL8, leftnode, 100, MPI_COMM_WORLD, ireq_send(1), ierr)
     call mpi_isend(x(1, goal), sf, MPI_REAL8, rightnode, 200, MPI_COMM_WORLD, ireq_send(2), ierr)

     call mpi_irecv(x(1, goal+1), sf, MPI_REAL8, rightnode, 100, MPI_COMM_WORLD, ireq_recv(1), ierr)
     call mpi_irecv(x(1, start-1), sf, MPI_REAL8, leftnode, 200, MPI_COMM_WORLD, ireq_recv(2), ierr)

     ! 何故か動いてくれない
     !call mpi_waitall(2, ireq_recv, istat, ierr)
     !call mpi_waitall(2, ireq_send, istat, ierr)

     call mpi_wait(ireq_recv(1), istat, ierr)
     call mpi_wait(ireq_recv(2), istat, ierr)
     call mpi_wait(ireq_send(1), istat, ierr)
     call mpi_wait(ireq_send(2), istat, ierr)

     !$xmp barrier
     time2 = xmp_wtime()

     ! calculate new vector
     !$xmp loop on t(j)
     do j = 1, n-1, 2
        do i = 2, sf, 2
           x(i, j) = (b(i, j) &
                - a_h_x_lower(i, j)*x(i-1, j) - a_h_x_upper(i, j)*x(i+1, j) &
                - a_h_y_lower(i, j)*x(i-l+1, j) - a_h_y_upper(i, j)*x(i+l-1, j) &
                - a_h_z_lower(i, j)*x(i, j-1) - a_h_z_upper(i, j)*x(i, j+1)) &
                * (omega/a_diag) + (1-omega)*x(i, j)
        end do

        do i = 1, sf, 2
           x(i, j+1) = (b(i, j+1) &
                - a_h_x_lower(i, j+1)*x(i-1, j+1) - a_h_x_upper(i, j+1)*x(i+1, j+1) &
                - a_h_y_lower(i, j+1)*x(i-l+1, j+1) - a_h_y_upper(i, j+1)*x(i+l-1, j+1) &
                - a_h_z_lower(i, j+1)*x(i, j) - a_h_z_upper(i, j+1)*x(i, j+2)) &
                * (omega/a_diag) + (1-omega)*x(i, j+1)
        end do
     end do

     !$xmp barrier
     time3 = xmp_wtime()

     ! message transfer
     call mpi_isend(x(1, start), sf, MPI_REAL8, leftnode, 100, MPI_COMM_WORLD, ireq_send(1), ierr)
     call mpi_isend(x(1, goal), sf, MPI_REAL8, rightnode, 200, MPI_COMM_WORLD, ireq_send(2), ierr)

     call mpi_irecv(x(1, goal+1), sf, MPI_REAL8, rightnode, 100, MPI_COMM_WORLD, ireq_recv(1), ierr)
     call mpi_irecv(x(1, start-1), sf, MPI_REAL8, leftnode, 200, MPI_COMM_WORLD, ireq_recv(2), ierr)

     ! 何故か動いてくれない
     !call mpi_waitall(2, ireq_recv, istat, ierr)
     !call mpi_waitall(2, ireq_send, istat, ierr)

     call mpi_wait(ireq_recv(1), istat, ierr)
     call mpi_wait(ireq_recv(2), istat, ierr)
     call mpi_wait(ireq_send(1), istat, ierr)
     call mpi_wait(ireq_send(2), istat, ierr)

     !$xmp barrier
     time4 = xmp_wtime()

     
     ! calculate norm
     !$xmp loop on t(j)
     do j = 1, n-1
        do i = 1, sf
           x_diff(i, j) = b(i, j) - a_diag*x(i, j) &
                - a_h_x_lower(i, j)*x(i-1, j) - a_h_x_upper(i, j)*x(i+1, j) &
                - a_h_y_lower(i, j)*x(i-l+1, j) - a_h_y_upper(i, j)*x(i+l-1, j) &
                - a_h_z_lower(i, j)*x(i, j-1) - a_h_z_upper(i, j)*x(i, j+1)
        end do
     end do

     !$xmp loop on t(j) reduction( + : norm_diff, norm_x )
     do j = 1, n-1
        do i = 1, sf
           norm_diff = norm_diff + x_diff(i, j)**2
           norm_x = norm_x + x(i, j)**2
        end do
     end do

     ! reduce norm_diff and norm_x

     norm_diff = sqrt(norm_diff)
     norm_x = sqrt(norm_x)

     ! preparation of next iter
     count = count+1

     ! stop clock
     !$xmp barrier
     time5 = xmp_wtime()

     ! calculate time
     alltime = alltime + (time5-time0)
     caltime = caltime + (time1-time0) + (time3-time2)
     comtime = comtime + (time2-time1) + (time4-time3)
     checktime = checktime + (time5-time4)

     ! shinchoku dou desuka?
     !$xmp task on p(1)
     write(*, '(i5, e15.5)') count, norm_diff/norm_b
     !$xmp end task

     ! check convergence
     if(norm_diff <= epsilon*norm_b) exit
     if(count >= 10) exit

     ! preparation of next iteration
     norm_diff = 0.0d0
     norm_x = 0.0d0
     
     !$xmp barrier
     
  end do

  ! compare with analysed answer here

  !$xmp loop on t(j) reduction( + : norm_analysis, diff )
  do j = 1, n-1
     do i = 1, sf
        coef_h_x = mod(i-1,l-1) + 1
        coef_h_y = (i-1)/(l-1) + 1
        coef_h_z = j
        analysis_answer = sin(pi*coef_h_x*h_x)*sin(pi*coef_h_y*h_y)*sinh(sqrt(2.0d0)*pi*coef_h_z*h_z)*denomi

        norm_analysis = norm_analysis + analysis_answer**2

        diff = diff + abs(x(i, j)-analysis_answer)
     end do
  end do

  norm_analysis = sqrt(norm_analysis)

  ! output
  !$xmp task on p(1)
     write(*, '(/,A,i6,/)') "iteration: ", count

     write(*, 200) "epsilon = ", epsilon
     write(*, 200) "omega = ", omega
     write(*, 200) "tick = ", tick

     write(*, '(/,2(A, e15.5))') "norm_x = ", norm_x, ", norm_analysis = ", norm_analysis
     write(*, 200) "diff = ", diff

     write(*, '(/,3(A, f9.4))') "caltime = ", caltime, ", comtime = ", comtime, ", checktime = ", checktime
     write(*, '((A, f9.4),/)') "alltime = ", alltime
  !$xmp end task

  call xmp_finalize_mpi()

100 format(2i4, X, f10.8)
200 format(A, e15.5)

end program mpi_on_xmp_sorrb2_3d_2

! 2015/10/11
! written by Shu OGAWARA
