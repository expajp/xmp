program sor_1d
  use f95_lapack
  implicit none

  integer, parameter :: n = 100
  real(8), parameter :: region_lower=0.0d0, region_upper=1.0d0
  real(8), parameter :: border_lower=0.0d0, border_upper=1.0d0
  real(8), parameter :: epsilon = 1.000E-08
  real(8), parameter :: omega = 1.5 ! it must be from (1, 2)

  real(8) :: region_length
  real(8) :: h

  real(8) :: x(n-1), x_new(n-1), x_diff(n-1) ! object of calc
  real(8) :: d(n-1, n-1), l(n-1, n-1), u(n-1, n-1) ! matrix
  real(8) :: d_omega_l_inverse(n-1, n-1), coefficient(n-1, n-1) ! coefficients
  real(8) :: b(n-1) ! right_hand side
  integer :: pivot(n-1) ! for LU decomposition

  real(8) :: norm_diff, norm_x ! error check

  integer :: i, j ! iteration
  integer :: count

  ! initialization
  region_length = region_upper - region_lower
  h = (region_length)/n ! 1/n
  
  x = 0.0d0
  x_new = 0.0d0

  ! diagonal matrix
  d = 0.0d0
  do j = 1, n-1
     do i = 1, n-1
        if(i == j) then
           d(i, j) = -2.0d0/(h**2)
        end if
     end do
  end do

  ! lower triangle matrix
  l = 0.0d0
  do j = 1, n-1
     do i = 1, n-1
        if(i == j+1) l(i, j) = 1.0d0/(h**2)
     end do
  end do

  ! upper triangle matrix
  u = 0.0d0
  do j = 1, n-1
     do i = 1, n-1
        if(i == j-1) u(i, j) = 1.0d0/(h**2)
     end do
  end do

  ! matrices for calculation
  d_omega_l_inverse = 0.0d0
  coefficient = 0.0d0

  ! pivot
  pivot = 0.0d0

  ! right-hand side
  b = 0.0d0
  b(1) = -(border_lower)/(h**2) ! 0
  b(n-1) = -(border_upper)/(h**2) ! -1/h^2


  ! calculate (D+omega*L)^(-1)
  ! code from http://www.rcs.arch.t.u-tokyo.ac.jp/kusuhara/tips/linux/fortran.html
  d_omega_l_inverse = d + omega*l

  call LA_GETRF(d_omega_l_inverse, pivot)
  call LA_GETRI(d_omega_l_inverse, pivot)

  ! calculate (1-omega)*D-omega*U
  coefficient = (1-omega)*d - omega*u

  ! main loop
  count = 0
  norm_diff = 0.0d0
  norm_x = 0.0d0

  write(*,*) "epsilon = ", epsilon

  do
     ! calculate new vector 
     x_new = matmul(matmul(d_omega_l_inverse, coefficient), x) + omega*matmul(d_omega_l_inverse, b)

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

end program sor_1d

! compile-command:
! gfortran sor_1d.f90 -I ~/include -L ~/lib -l lapack95 -l lapack -l blas

! 2015/01/08
! written by Shu OGAWARA
