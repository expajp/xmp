program sor_3d
  implicit none

  ! mesh
  integer, parameter :: l = 100, m = 100, n = 129
  integer, parameter :: mesh = (l-1)*(m-1)*(n-1)
  integer, parameter :: sf = (l-1)*(m-1)

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

  ! omega
  real(8), parameter :: omega = 1.5 ! it must be from (1, 2)

  real(8) :: region_x_length, region_y_length, region_z_length
  real(8) :: h_x, h_y, h_z

  real(8) :: x(-sf+1:mesh+sf), x_old(mesh), x_diff(mesh) ! object of calc
  real(8) :: a_h_x_upper(mesh), a_h_y_upper(mesh), a_h_z_upper(mesh) ! left-hand side
  real(8) :: a_h_x_lower(mesh), a_h_y_lower(mesh), a_h_z_lower(mesh), a_diag ! left-hand side
  real(8) :: b(mesh) ! right_hand side
  integer :: b_border_region ! the upper of region of b inserted border's value
  integer :: coef_h_x, coef_h_y

  real(8) :: norm_diff, norm_x ! error check

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
  a_diag = -2.0d0*((1.0d0/h_x**2)+(1.0d0/h_y**2)+(1.0d0/h_z**2))
  a_h_x_upper = 0.0d0
  a_h_y_upper = 0.0d0
  a_h_z_upper = 0.0d0
  a_h_x_lower = 0.0d0
  a_h_y_lower = 0.0d0
  a_h_z_lower = 0.0d0

  do i = 1, mesh
     if(mod(i,l-1) /= 0) a_h_x_upper(i) = 1.0d0/h_x**2
     if(mod(i,sf) <= (l-1)*(m-2)) a_h_y_upper(i) = 1.0d0/h_y**2
     if(i <= mesh-sf) a_h_z_upper(i) = 1.0d0/h_z**2
  end do

  do i = 1, mesh
     if(mod(i,l-1) /= 1) a_h_x_lower(i) = 1.0d0/h_x**2
     if(mod(i,sf) > l-1) a_h_y_lower(i) = 1.0d0/h_y**2
     if(i > sf) a_h_z_lower(i) = 1.0d0/h_z**2
  end do

  ! right-hand side
  b = 0.0d0
  b_border_region = mesh-sf
  coef_h_x = 0
  coef_h_y = 0
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
  x_old = 0.0d0

  write(*,*) "epsilon = ", epsilon

  do
     ! x_old = x
     do i = 1, mesh
        x_old(i) = x(i)
     end do

     ! calculate new vector
     do i = 1, mesh
        x(i) = (b(i) - a_h_x_lower(i)*x(i-1) - a_h_x_upper(i)*x(i+1) &
                     - a_h_y_lower(i)*x(i-l+1) - a_h_y_upper(i)*x(i+l-1) &
                     - a_h_z_lower(i)*x(i-sf) - a_h_z_upper(i)*x(i+sf)) &
                * (omega/a_diag) + (1-omega)*x(i)
     end do
     
     ! calculate norm
     ! x_diff = x - x_old
     do i = 1, mesh
        x_diff(i) = x(i) - x_old(i)
     end do

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
  ! write(*, *) "difference from analysis solution: ", diff

  ! output
  do i = 1, l-1
     write(*, '(i3, e15.5)') i, x((l-1)*((m-1)/2)+i)
  end do


100 format(2i4, X, f10.8)

end program sor_3d

! 2015/01/28
! written by Shu OGAWARA
