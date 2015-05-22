program sor_3d_2_red_black
  implicit none

  ! mesh
  integer, parameter :: l = 100, m = 100, n = 129 ! 2^n 分割をしやすくするため
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
  real(8), parameter :: omega = 1.8 ! it must be from (1, 2)

  real(8) :: region_x_length, region_y_length, region_z_length
  real(8) :: h_x, h_y, h_z

  real(8) :: x(-l+2:sf+l-1, 0:n-1), x_diff(sf, n-1) ! object of calc
  real(8) :: a_h_x_upper(sf, n-1), a_h_y_upper(sf, n-1), a_h_z_upper(sf, n-1) ! left-hand side
  real(8) :: a_h_x_lower(sf, n-1), a_h_y_lower(sf, n-1), a_h_z_lower(sf, n-1), a_diag ! left-hand side
  real(8) :: b(sf, n-1) ! right_hand side
  integer :: coef_h_x, coef_h_y

  ! error check
  real(8) :: norm_diff, norm_x, norm_b

  ! iteration
  integer :: i, j, count

  ! variables for time measurement
  integer :: time0, time1, t_rate, t_max
  real(8) :: tick, alltime


  ! initialization

  ! constants
  pi = acos(-1.0d0)
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

  do i = 1, sf
     coef_h_x = mod(i-1, l-1) + 1
     coef_h_y = (i-1)/(l-1) + 1
     b(i, n-1) = -sin(coef_h_x*h_x*pi)*sin(coef_h_y*h_y*pi)/h_z**2
     norm_b = norm_b + b(i, n-1)**2
  end do

  norm_b = sqrt(norm_b)

  ! main loop
  count = 0
  norm_diff = 0.0d0
  norm_x = 0.0d0

  x = 0.0d0

  alltime = 0.0d0
  time0 = 0
  time1 = 0
  t_rate = 0
  t_max = 0

  call system_clock(time0, t_rate, t_max)
  tick = 1.0d0/t_rate

  write(*,*) "epsilon = ", epsilon
  write(*,*) "tick = ", tick

  do
     call system_clock(time0)

     ! calculate new vector
     do j = 1, n-1

        do i = 1, sf, 2

           x(i, j) = (b(i, j) &
                - a_h_x_lower(i, j)*x(i-1, j) - a_h_x_upper(i, j)*x(i+1, j) &
                - a_h_y_lower(i, j)*x(i-l+1, j) - a_h_y_upper(i, j)*x(i+l-1, j) &
                - a_h_z_lower(i, j)*x(i, j-1) - a_h_z_upper(i, j)*x(i, j+1) ) &
                * (omega/a_diag) + (1-omega)*x(i, j)

        end do

        do i = 2, sf, 2

           x(i, j) = (b(i, j) &
                - a_h_x_lower(i, j)*x(i-1, j) - a_h_x_upper(i, j)*x(i+1, j) &
                - a_h_y_lower(i, j)*x(i-l+1, j) - a_h_y_upper(i, j)*x(i+l-1, j) &
                - a_h_z_lower(i, j)*x(i, j-1) - a_h_z_upper(i, j)*x(i, j+1) ) &
                * (omega/a_diag) + (1-omega)*x(i, j)

        end do

     end do

     ! calculate norm
     do j = 1, n-1
        do i = 1, sf
           x_diff(i, j) = b(i, j) - a_diag*x(i, j) &
                - a_h_x_lower(i, j)*x(i-1, j) - a_h_x_upper(i, j)*x(i+1, j) &
                - a_h_y_lower(i, j)*x(i-l+1, j) - a_h_y_upper(i, j)*x(i+l-1, j) &
                - a_h_z_lower(i, j)*x(i, j-1) - a_h_z_upper(i, j)*x(i, j+1)
        end do
     end do

     do j = 1, n-1
        do i = 1, sf
           norm_diff = norm_diff + x_diff(i, j)**2
           norm_x = norm_x + x(i, j)**2
        end do
     end do

     norm_diff = sqrt(norm_diff)
     norm_x = sqrt(norm_x)

     ! preparation of next iter
     count = count+1

     call system_clock(time1)

     if (time1 < time0) then
        alltime = alltime + ((t_max-time0)+time1+1)/dble(t_rate)
     else
        alltime = alltime + (time1-time0)/dble(t_rate)
     end if

     ! shinchoku dou desuka?
     write(*, '(i5, e15.5)') count, norm_diff/norm_b

     ! check convergence
     if(norm_diff <= epsilon*norm_b) exit
     
     ! preparation of next iteration
     norm_diff = 0.0d0
     norm_x = 0.0d0

  end do

  ! output
  write(*, '(/,A,i6)') "iteration: ", count
  write(*, '(A, e15.5)') "norm_x = ", norm_x
  write(*, '(A, f9.4, /)') "alltime = ", alltime

100 format(2i4, X, f10.8)

end program sor_3d_2_red_black

! 2015/05/15
! written by Shu OGAWARA
