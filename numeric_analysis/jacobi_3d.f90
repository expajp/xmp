program jacobi_3d
  implicit none

  ! mesh
  integer, parameter :: l = 50, m = 50, n = 50
  integer, parameter :: mesh = (l-1)*(m-1)*(n-1)

  ! region
  real(8), parameter :: region_x_lower=0.0d0, region_x_upper=1.0d0
  real(8), parameter :: region_y_lower=0.0d0, region_y_upper=1.0d0
  real(8), parameter :: region_z_lower=0.0d0, region_z_upper=1.0d0

  ! border
  real(8), parameter :: border_x_lower=0.0d0, border_x_upper=0.0d0
  real(8), parameter :: border_y_lower=0.0d0, border_y_upper=0.0d0
  real(8), parameter :: border_z_lower=0.0d0 ! border_z_upper=sin(pi*x)sin(pi*y)

  ! epsilon
  real(8), parameter :: epsilon = 1.000E-08

  ! pi
  real(8), parameter :: pi = acos(-1.0d0)

  ! denominator
  real(8), parameter :: denomi = 1.0d0/(exp(pi)-exp(-pi))

  real(8) :: region_x_length, region_y_length, region_z_length
  real(8) :: h_x, h_y, h_z

  real(8) :: x(mesh), x_new(mesh), x_diff(mesh) ! object of calc
  real(8) :: a_h_x(mesh-1), a_h_y(mesh-l+1), a_h_z(mesh-(l-1)(m-1)), a_diag(mesh) ! left-hand side
  real(8) :: b(mesh) ! right_hand side
  integer :: b_border_region ! the upper of region of b inserted border's value
  integer :: coef_h_x, coef_h_y

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
  region_z_length = region_z_upper - region_z_lower ! = 1
  h_x = (region_x_length)/l ! 1/l
  h_y = (region_y_length)/m ! 1/m
  h_z = (region_z_length)/n ! 1/n
  
  ! matrix
  a_diag = 0.0d0
  a_h_x = 0.0d0
  a_h_y = 0.0d0
  a_h_z = 0.0d0

  do i = 1, mesh-1
     a_diag(i) = -2.0d0*((1.0d0/h_x**2)+(1.0d0/h_y**2)+(1.0d0/h_z**2))
     if(i <= mesh-(l-1)*(m-1)) a_h_z(i) = 1.0d0/h_z**2
     if(i <= mesh-l+1 .and. mod(i,(l-1)*(m-1)) /= 0) a_h_y(i) = 1.0d0/h_y**2
     if(mod(i,m-1) /= 0) a_h_x(i) = 1.0d0/h_x**2
  end do
  a_diag(mesh) = -2.0d0*((1.0d0/h_x**2)+(1.0d0/h_y**2)+(1.0d0/h_z**2))

  ! right-hand side
  b = 0.0d0
  b_border_region = mesh-(l-1)*(m-1)
  coef_h_x = 0, coef_h_y = 0
  do i = b_border_region+1, mesh
     coef_h_x = mod(i-b_border_region-1,l-1) + 1
     coef_h_y = (i-b_border_region-1)/(l-1) + 1
     b(i) = -sin(coef_h_x*h_x*pi)*sin(coef_h_y*h_y*pi)/h_z**2
  end do

  ! main loop
  count = 0
  norm_diff = 0.0d0
  norm_x = 0.0d0

  x = 0.0d0
  x_new = 0.0d0

  write(*,*) "epsilon = ", epsilon

  do
     ! calculate new vector
     x_new(1) = (b(1)-a_h_x(1)*x(2)-a_h_y(1)*x(l)-a_h_z(1)*x((l-1)*(m-1)+1))/a_diag(1)

     do i = 2, mesh-1
           x_new(i) = b(i)-a_h_x(i)*x(i+1)-a_h_x(i-1)*x(i-1)
           if(i-l+1 >= 0) x_new(i) = x_new(i)-a_h_y(i-l+1)*x(i-l+1) ! 今回はここまで
           if(i-(l-1)(m-1) >= 0) x_new(i) = x_new(i)-a_h_y(i-l+1)*x(i-l+1)
           if(i+l-1 <= mesh) x_new(i) = x_new(i)-a_h_y(i)*x(i+l-1)
           x_new(i) = x_new(i)/a_diag(i)
     end do

     x_new(mesh) = (b(mesh)-a_h(mesh-1)*x(mesh-1)-a_h_y(mesh-l+1)*x(mesh-l+1))/a_diag(mesh)

     
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
  diff = 0.0d0

  do i = 1, mesh
     row = mod(i,m-1)
     if(row == 0) row = l-1
     column = 1 + (i-1)/(l-1)

     row_d = dble(row)
     column_d = dble(column)

     diff = diff + abs(x(i) - (sin(pi*h_x*row_d)*(exp(pi*h_y*column_d)-exp(-pi*h_y*column_d))*denomi))

     !write(*, 100) row, column, x(i)
  end do

  write(*, *) "difference from analysis solution: ", diff

100 format(2i4, X, f10.8)

end program jacobi_3d

! 2015/01/23
! written by Shu OGAWARA
