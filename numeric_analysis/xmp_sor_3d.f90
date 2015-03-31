program xmp_sor_3d_2d_array
  implicit none

  ! test comment

  ! mesh
  integer, parameter :: l = 100, m = 100, n = 127
  integer, parameter :: sf = (l-1)*(m-1) ! sf:surface

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

  ! real(8) :: x(-l+2:sf+l-1, n+1), x_old(-l+2:sf+l-1, n+1), x_diff(-l+2:sf+l-1, n+1) ! object of calc
  real(8) :: x(-l+2:sf+l-1, 0:n), x_old(-l+2:sf+l-1, 0:n), x_diff(-l+2:sf+l-1, 0:n) ! object of calc
  real(8) :: a_h_x(0:sf, 0:n), a_h_y(-l+2:sf, 0:n), a_h_z(sf, 0:n), a_diag ! left-hand side
  real(8) :: b(sf, n) ! right_hand side
  integer :: coef_h_x, coef_h_y

  real(8) :: norm_diff, norm_x ! error check

  integer :: i, j ! iteration
  integer :: count

  ! variables for xmp
  integer :: myrank
  integer :: xmp_node_num

  ! xmp directives
  !$xmp nodes p(4)
  !$xmp template t(0:n)
  !$xmp distribute t(block) onto p
  !$xmp align(*,k) with t(k) :: x, x_old, x_diff, a_h_x, a_h_y, a_h_z, b
  !$xmp shadow x(0,1)

  myrank = xmp_node_num()

  write(*,*) "Hello! My Rank is ", myrank, "."

  ! initialization
  region_x_length = region_x_upper - region_x_lower ! = 1
  region_y_length = region_y_upper - region_y_lower ! = 1
  region_z_length = region_z_upper - region_z_lower ! = 1
  h_x = (region_x_length)/l ! 1/l
  h_y = (region_y_length)/m ! 1/m
  h_z = (region_z_length)/n ! 1/n
  
  ! matrix
  a_diag = -2.0d0*((1.0d0/h_x**2)+(1.0d0/h_y**2)+(1.0d0/h_z**2))
  a_h_x = 0.0d0
  a_h_y = 0.0d0
  a_h_z = 0.0d0

  !$xmp loop on t(j)
  do j = 1, n-1
     do i = 1, sf
        if(j /= n-1) a_h_z(i, j) = 1.0d0/h_z**2
        if(i <= (l-1)*(m-2)) a_h_y(i, j) = 1.0d0/h_y**2
        if(mod(i,l-1) /= 0) a_h_x(i, j) = 1.0d0/h_x**2
     end do
  end do

  ! right-hand side
  b = 0.0d0
  coef_h_x = 0
  coef_h_y = 0

  !$xmp task on p(4)
  do i = 1, sf
     coef_h_x = mod(i-1, l-1) + 1
     coef_h_y = (i-1)/(l-1) + 1
     b(i, n-1) = -sin(coef_h_x*h_x*pi)*sin(coef_h_y*h_y*pi)/h_z**2
  end do
  !$xmp end task

  ! main loop
  count = 0
  norm_diff = 0.0d0
  norm_x = 0.0d0

  x = 0.0d0
  x_old = 0.0d0

  if(myrank == 0) write(*,*) "epsilon = ", epsilon

  !$xmp task on p(4)
  write(*,*) "x(sf, n-1) = ", x(sf, n-1)
  !$xmp end task

  do
     x_old = x

     !$xmp barrier
     !$xmp reflect(x)

     ! 原因はやっぱりここから

     ! calculate new vector
     !$xmp loop on t(j)
     do j = 1, n-1
        do i = 1, sf
           x(i, j) = (b(i, j) &
                - a_h_x(i-1, j)*x(i-1, j) - a_h_x(i, j)*x(i+1, j) &
                - a_h_y(i-l+1, j)*x(i-l+1, j) - a_h_y(i, j)*x(i+l-1, j) &
                - a_h_z(i, j-1)*x(i, j-1) - a_h_z(i, j)*x(i, j+1) ) &
                * (omega/a_diag) + (1-omega)*x(i, j)
        end do
     end do

     ! ここまで   
     
     ! calculate norm
     x_diff = x - x_old

     !$xmp loop on t(j) reduction(+:norm_diff,norm_x)
     do j = 1, n-1
        do i = 1, sf
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
     if(mod(count,500) == 0 .and. myrank == 1) then
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
  ! write(*, *) "difference from analysis solution: ", diff

  ! output
  if(myrank == 1) then
     do i = 1, l-1
        write(*, '(i3, e15.5)') i, x((l-1)*((m-1)/2+1)+i, (n-1)/2)
     end do
  end if

100 format(2i4, X, f10.8)

end program xmp_sor_3d_2d_array

! 2015/03/25
! written by Shu OGAWARA
