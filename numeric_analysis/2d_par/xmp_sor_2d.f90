program xmp_sor_2d
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
  real(8) :: x(l-1, m-1), x_old(l-1, m-1), x_diff(l-1, m-1) ! object of calc
  real(8) :: a_h_x(l-1, m-1), a_h_y(l-1, m-1) ! left-hand side
  real(8) :: b(l-1, m-1) ! right_hand side

  ! error check
  real(8) :: norm_diff, norm_x

  ! compare with analysis
  real(8) :: diff
  real(8) :: row_d, column_d

  integer :: i, j ! iteration
  integer :: count

  ! variables for MPI
  integer :: myrank, nprocs
  integer :: xmp_node_num, xmp_num_nodes


  ! initialization

  ! xmp directives
  !$xmp nodes p(4)
  !$xmp template t(m-1)
  !$xmp distribute t(block) onto p
  !$xmp align(*,k) with t(k) :: x, x_old, x_diff, a_h_x, a_h_y, b
  !$xmp shadow x(0,1)
  !$xmp shadow a_h_y(0,1)

  myrank = xmp_node_num()
  nprocs = xmp_num_nodes()

  ! constants
  region_x_length = region_x_upper - region_x_lower ! = 1
  region_y_length = region_y_upper - region_y_lower ! = 1
  h_x = (region_x_length)/l ! 1/l
  h_y = (region_y_length)/m ! 1/m
  
  ! matrix
  a_diag = -2.0d0*((1/h_x**2)+(1/h_y**2))
  a_h_x = 0.0d0
  a_h_y = 0.0d0

  !$xmp loop on t(j)
  do j = 1, m-1
     do i = 1, l-1
        if(j <= m-2) a_h_y(i, j) = 1.0d0/h_y**2
        if(i /= l-1) a_h_x(i, j) = 1.0d0/h_x**2
     end do
  end do

  ! message-sending
  !$xmp reflect(a_h_y)

  ! right-hand side
  b = 0.0d0

  !$xmp task on p(4)
  do i = 1, l-1
     b(i, m-1) = -sin(i*h_x*pi)/h_y**2
  end do
  !$xmp end task

  ! main loop
  count = 0
  norm_diff = 0.0d0
  norm_x = 0.0d0

  x = 0.0d0
  x_old = 0.0d0

  if(myrank == 1) write(*,*) "epsilon = ", epsilon

  do
     ! x_old = x <-バグの温床
     !$xmp loop on t(j)
     do j = 1, m-1
        do i = 1, l-1
           x_old(i, j) = x(i, j)
        end do
     end do

     ! calculate new vector
     !$xmp loop on t(j)
     do j = 1, m-1
        do i = 1, l-1
           x(i, j) = (b(i, j) &
                - a_h_x(i, j)*x(i+1, j) - a_h_x(i-1, j)*x(i-1, j) &
                - a_h_y(i, j)*x(i, j+1) - a_h_y(i, j-1)*x(i, j-1)) &
                * (omega/a_diag) + (1-omega)*x(i, j)
        end do
     end do
     
     ! message-sending
     !$xmp reflect(x)

     ! calculate norm

     ! x_diff = x - x_old <-バグの温床
     !$xmp loop on t(j)
     do j = 1, m-1
        do i = 1, l-1
           x_diff(i, j) = x(i, j) - x_old(i, j)
        end do
     end do

     !$xmp loop on t(j) reduction(+:norm_diff,norm_x)
     do j = 1, m-1
        do i = 1, l-1
           norm_diff = norm_diff + x_diff(i, j)**2
           norm_x = norm_x + x(i, j)**2
        end do
     end do

     norm_diff = sqrt(norm_diff)
     norm_x = sqrt(norm_x)

     ! check convergence
     if(norm_diff <= epsilon*norm_x) exit
     
     ! preparation of next iter
     count = count+1

     ! shinchoku dou desuka?
     if(myrank == 1 .and. mod(count,500) == 0) then
        write(*, *) "iteration: ", count
        write(*, *) "relative error = ", norm_diff/norm_x
     end if

     ! preparation of next iteration
     norm_diff = 0.0d0
     norm_x = 0.0d0
     
     !$xmp barrier

  end do

  !$xmp barrier

  ! output
  if(myrank == 1) write(*, *) "iteration: ", count

  ! compare with analysed answer here
  diff = 0.0d0

  !$xmp loop on t(j) reduction(+:diff)
  do j = 1, m-1
     do i = 1, l-1
        row_d = dble(i)
        column_d = dble(j)
        diff = diff + abs(x(i, j) - (sin(pi*h_x*row_d)*(exp(pi*h_y*column_d)-exp(-pi*h_y*column_d))*denomi))
     end do
  end do

  if(myrank == 1) write(*, *) "difference from analysis solution: ", diff

100 format(2i4, X, f10.8)

end program xmp_sor_2d

! 2015/04/02
! written by Shu OGAWARA
