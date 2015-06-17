program sor_2d
  implicit none

  ! mesh
  integer, parameter :: l = 100, m = 100
  integer, parameter :: mesh = (l-1)*(m-1)

  ! region
  real(8), parameter :: region_x_lower=0.0d0, region_x_upper=1.0d0
  real(8), parameter :: region_y_lower=0.0d0, region_y_upper=1.0d0

  ! border
  real(8), parameter :: border_x_lower=0.0d0, border_x_upper=1.0d0
  real(8), parameter :: border_y_lower=0.0d0 ! border_y_upper=sin(pi*x)

  ! epsilon
  real(8), parameter :: epsilon = 1.000E-8

  ! pi
  real(8), parameter :: pi = acos(-1.0d0)

  ! denominator
  real(8), parameter :: denomi = 1.0d0/(exp(pi)-exp(-pi))

  ! omega
  real(8), parameter :: omega = 1.5 ! it must be from (1, 2)

  real(8) :: region_x_length, region_y_length
  real(8) :: h, k

  real(8) :: x(mesh), x_old(mesh), x_diff(mesh) ! object of calc
  real(8) :: a_k(mesh-l+1), a_h(mesh-1), a_diag(1:mesh) ! left-hand side
  real(8) :: b(mesh) ! right_hand side

  real(8) :: norm_diff, norm_x ! error check

  ! compare with analysis
  real(8) :: diff
  integer :: row, column
  real(8) :: row_d, column_d

  integer :: i, j ! iteration
  integer :: count

  ! initialization
  region_x_length = region_x_upper - region_x_lower ! = 1
  region_y_length = region_y_upper - region_y_lower ! = 1
  h = (region_x_length)/l ! 1/l
  k = (region_y_length)/m ! 1/m
  
  ! matrix
  a_diag = 0.0d0
  a_h = 0.0d0
  a_k = 0.0d0

  do i = 1, mesh-1
     a_diag(i) = -2.0d0*((1/h**2)+(1/k**2))
     if(i <= mesh-l+1) a_k(i) = 1.0d0/k**2
     if(mod(i,m-1) /= 0) a_h(i) = 1.0d0/h**2
  end do
  a_diag(mesh) = -2.0d0*((1/h**2)+(1/k**2))

  ! right-hand side
  b = 0.0d0
  do i = mesh-l+2, mesh
     b(i) = -sin((i-mesh+l-1)*h*pi)/k**2
  end do

  ! main loop
  count = 0
  norm_diff = 0.0d0
  norm_x = 0.0d0

  x = 0.0d0
  x_old = 0.0d0

  write(*,*) "epsilon = ", epsilon

  do
     x_old = x

     ! calculate new vector
     x(1) = (b(1)-a_k(1)*x(l)-a_h(1)*x(2)) * (omega/a_diag(1)) + (1-omega)*x(1)

     do i = 2, l-1
        x(i) = (b(i)-a_h(i)*x(i+1)-a_h(i-1)*x(i-1)-a_k(i)*x(i+l-1)) * (omega/a_diag(i)) + (1-omega)*x(i)
     end do

     do i = l, mesh-l
        x(i) = (b(i)-a_h(i)*x(i+1)-a_h(i-1)*x(i-1)-a_k(i)*x(i+l-1)-a_k(i-l+1)*x(i-l+1)) &
                * (omega/a_diag(i)) + (1-omega)*x(i)
     end do

     do i = mesh-l+1, mesh-1
        x(i) = (b(i)-a_h(i)*x(i+1)-a_h(i-1)*x(i-1)-a_k(i-l+1)*x(i-l+1)) * (omega/a_diag(i)) + (1-omega)*x(i)
     end do

     x(mesh) = (b(mesh)-a_h(mesh-1)*x(mesh-1)-a_k(mesh-l+1)*x(mesh-l+1)) * (omega/a_diag(mesh)) + (1-omega)*x(mesh)

     
     ! calculate norm
     x_diff = x - x_old
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
     norm_diff = 0.0d0
     norm_x = 0.0d0
     
  end do

  ! output
  write(*, *) "iteration: ", count

  ! compare with analysed answer here
  diff = 0.0d0

  do i = 1, mesh
     row = mod(i,m-1)
     if(row == 0) row = l-1
     column = 1 + (i-1)/(l-1)

     row_d = dble(row)
     column_d = dble(column)

     diff = diff + abs(x(i) - (sin(pi*h*row_d)*(exp(pi*k*column_d)-exp(-pi*k*column_d))*denomi))

     !write(*, 100) row, column, x(i)
  end do

  write(*, *) "difference from analysis solution: ", diff

100 format(2i4, X, f10.8)

end program sor_2d

! 2015/01/23
! written by Shu OGAWARA
