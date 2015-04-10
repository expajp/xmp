program gs_1d
  implicit none

  integer, parameter :: n = 100
  real(8), parameter :: region_lower=0.0d0, region_upper=1.0d0
  real(8), parameter :: border_lower=0.0d0, border_upper=1.0d0
  real(8), parameter :: epsilon = 1.000E-08

  real(8) :: region_length
  real(8) :: h

  real(8) :: x(n-1), x_new(n-1), x_diff(n-1) ! object of calc
  real(8) :: a(n-1, n-1) ! left_hand side
  real(8) :: b(n-1) ! right_hand side

  real(8) :: norm_diff, norm_x ! error check

  integer :: i, j ! iteration
  integer :: count

  ! initialization
  region_length = region_upper - region_lower
  h = (region_length)/n ! 1/n
  
  x = 0.0d0
  x_new = 0.0d0

  ! matrix
  a = 0.0d0
  do i = 1, n-1
     a(i, i) = -2.0d0/(h**2)

     if(i < n-1) then
        a(i+1, i) = 1.0d0/(h**2)
        a(i, i+1) = 1.0d0/(h**2)
     end if
  end do

  ! right-hand side
  b = 0.0d0
  b(1) = -(border_lower)/(h**2) ! 0
  b(n-1) = -(border_upper)/(h**2) ! -1/h^2

  ! main loop
  count = 0
  norm_diff = 0.0d0
  norm_x = 0.0d0

  write(*,*) "epsilon = ", epsilon

  do

     ! calculate new vector
     x_new = b

     do i = 1, n-1
        do j = 1, n-1
           if(i < j) x_new(i) = x_new(i) - a(i, j)*x(j)
           if(i > j) x_new(i) = x_new(i) - a(i, j)*x_new(j)
        end do
        x_new(i) = x_new(i) / a(i, i)
     end do
     
     ! calculate norm
     x_diff = x_new - x
     do i = 1, n-1
        norm_diff = norm_diff + x_diff(i)**2
        norm_x = norm_x + x(i)**2
     end do

     norm_diff = sqrt(norm_diff)
     norm_x = sqrt(norm_x)

     ! check convergence
     if(norm_diff <= epsilon*norm_x) exit
     
     ! preparation of next iter
     count = count+1

     ! shinchoku dou desuka?
     if(mod(count,1000) == 0) then
        write(*, *) "iteration: ", count
        write(*, *) "relative error = ", norm_diff/norm_x
     end if

     ! preparation of next iteration
     x = x_new
     norm_diff = 0.0d0
     norm_x = 0.0d0
     
  end do

  ! output
  write(*, *) "iteration: ", count

  write(*, 100) 0, border_lower

  do i = 10, n-1, 10
     write(*, 100) i, x(i)
  end do

  write(*, 100) n, border_upper

100 format("x(", i6, ") = ", f10.8)

end program gs_1d

! 2015/01/09
! written by Shu OGAWARA
