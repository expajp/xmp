program jacobi_2d
  implicit none

  ! mesh
  integer, parameter :: l = 30, m = 30
  integer, parameter :: mesh = (l-1)*(m-1)

  ! region
  real(8), parameter :: region_x_lower=0.0d0, region_x_upper=1.0d0
  real(8), parameter :: region_y_lower=0.0d0, region_y_upper=1.0d0

  ! border
  real(8), parameter :: border_x_lower=0.0d0, border_x_upper=1.0d0
  real(8), parameter :: border_y_lower=0.0d0 ! border_y_upper=sin(pi*x)

  ! epsilon
  real(8), parameter :: epsilon = 1.000E-08

  ! pi
  real(8), parameter :: pi = 3.141592653589793

  real(8) :: region_x_length, region_y_length
  real(8) :: h, k

  real(8) :: x(mesh), x_new(mesh), x_diff(mesh) ! object of calc
  real(8) :: a(mesh, mesh) ! left_hand side
  real(8) :: b(mesh) ! right_hand side

  real(8) :: norm_diff, norm_x ! error check

  integer :: i, j ! iteration
  integer :: count

  ! initialization
  region_x_length = region_x_upper - region_x_lower ! = 1
  region_y_length = region_y_upper - region_y_lower ! = 1
  h = (region_x_length)/l ! 1/l
  k = (region_y_length)/m ! 1/m
  
  x = 0.0d0
  x_new = 0.0d0

  ! matrix
  a = 0.0d0
  do i = 1, mesh
     a(i, i) = -2.0d0*((1/h**2)+(1/k**2))

     if(i /= mesh) a(i, i+1) = 1.0d0/h**2
     if(i /= 1)  a(i, i-1) = 1.0d0/h**2

     if(i+(l-1) <= mesh) a(i, i+l-1) = 1.0d0/k**2
     if(i-(l-1) >= 1)    a(i, i-l+1) = 1.0d0/k**2

  end do

  ! error check
  !write(*, *) "1/h^2 = ", 1.0d0/h**2
  !write(*, *) "1/k^2 = ", 1.0d0/k**2

  !write(*, *) "a = "
  !do j = 1, mesh
  !   write(*, '(16f7.2)') (a(i, j), i=1, mesh)
  !end do
  

  ! right-hand side
  b = 0.0d0

  do i = 1, mesh
     if(i+(l-1) > mesh) b(i) = -sin((i-(mesh-(l-1)))*h*pi)/k**2
  end do

  ! error check
  !write(*, *) "b = "
  !do i = 1, mesh
  !   write(*, '(16f7.2)') b(i)
  !end do

  ! main loop
  count = 0
  norm_diff = 0.0d0
  norm_x = 0.0d0

  write(*,*) "epsilon = ", epsilon

  do

     ! calculate new vector
     x_new = b
     do j = 1, mesh
        do i = 1, mesh
           if(i /= j) x_new(i) = x_new(i) - a(i, j)*x(j)
        end do
     end do
     
     do i = 1, mesh
        x_new(i) = x_new(i) / a(i, i)
     end do

     ! calculate norm
     x_diff = x_new - x
     do i = 1, mesh
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
     if(mod(count,500) == 0) then
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

  ! compare with analysed answer here

100 format("x(", i6, ") = ", f10.8)

end program jacobi_2d

! 2015/01/13
! written by Shu OGAWARA
