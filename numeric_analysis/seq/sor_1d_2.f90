program sor_1d
  implicit none

  ! convert a to 1d array

  integer, parameter :: n = 400
  real(8), parameter :: region_lower=0.0d0, region_upper=1.0d0
  real(8), parameter :: border_lower=0.0d0, border_upper=1.0d0
  real(8), parameter :: epsilon = 1.000E-08
  real(8), parameter :: omega = 1.5 ! it must be from (1, 2)

  real(8) :: region_length
  real(8) :: h

  real(8) :: x(n-1), x_old(n-1), x_diff(n-1) ! object of calc
  real(8) :: a_upper(1:n-2), a_diag(1:n-1), a_lower(2:n-1) ! left_hand side, a_*(i) means ith row's number
  real(8) :: b(n-1) ! right_hand side

  real(8) :: norm_diff, norm_x ! error check

  integer :: i, j ! iteration
  integer :: count

  ! initialization
  region_length = region_upper - region_lower
  h = (region_length)/n ! 1/n
  
  x = 0.0d0
  x_old = 0.0d0

  ! matrix
  a_upper = 0.0d0
  a_diag = 0.0d0
  a_lower = 0.0d0

  do i = 1, n-2
     a_upper(i) = 1.0d0/h**2
     a_diag(i) = -2.0d0/h**2
     a_lower(i+1) = 1.0d0/h**2
  end do

  a_diag(n-1) = -2.0d0/h**2
  

  ! right-hand side
  b = 0.0d0
  b(1) = -(border_lower)/h**2 ! 0
  b(n-1) = -(border_upper)/h**2 ! -1/h^2

  ! main loop
  count = 0
  norm_diff = 0.0d0
  norm_x = 0.0d0

  write(*,*) "epsilon = ", epsilon

  do

     ! calculate new vector
     x_old = x ! preserve for checking difference

     ! i = 1
     x(1) = (omega/a_diag(1)) * (b(1)-a_upper(1)*x(2)) + (1-omega)*x(1)

     ! ordered
     do i = 2, n-2, 2
        x(i) = (omega/a_diag(i)) * (b(i)-a_upper(i)*x(i+1)-a_lower(i)*x(i-1)) + (1-omega)*x(i)
     end do

     do i = 3, n-2, 2
        x(i) = (omega/a_diag(i)) * (b(i)-a_upper(i)*x(i+1)-a_lower(i)*x(i-1)) + (1-omega)*x(i)
     end do
     
     ! i = n-1
     x(n-1) = (omega/a_diag(n-1))*((b(n-1)-a_lower(n-1)*x(n-2))) + (1-omega)*x(n-1)
     

     ! calculate norm
     x_diff = x - x_old
     do i = 1, n-1
        norm_diff = norm_diff + x_diff(i)**2
        norm_x = norm_x + x_old(i)**2
     end do

     norm_diff = sqrt(norm_diff)
     norm_x = sqrt(norm_x)

     ! check convergence
     if(norm_diff <= epsilon*norm_x) exit
     
     ! preparation of next iter
     count = count+1

     ! shinchoku dou desuka?
     if(mod(count,5000) == 0) then
        write(*, *) "iteration: ", count
        write(*, *) "relative error = ", norm_diff/norm_x
     end if

     ! preparation of next iteration
     norm_diff = 0.0d0
     norm_x = 0.0d0
     
  end do

  ! output
  write(*, *) "iteration: ", count

  write(*, 100) 0, border_lower

  do i = n/10, n-1, n/10
     write(*, 100) i, x(i)
  end do

  write(*, 100) n, border_upper

100 format("x(", i6, ") = ", f10.8)

end program sor_1d

! 2015/01/09 ver.1.0
! 2015/01/20 ver.2.0
! written by Shu OGAWARA
