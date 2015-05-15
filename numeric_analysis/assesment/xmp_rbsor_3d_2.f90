program xmp_sor_3d_2_red_black
  implicit none

  ! mesh
  integer, parameter :: l = 100, m = 100, n = 129 ! 2^n 分割をしやすくするため
  integer, parameter :: sf = (l-1)*(m-1) ! sf:surface
  integer, parameter :: bcast_from = n-1 ! bcast指示文の不具合対応

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
  real(8), parameter :: pi = acos(-1.0d0)
  real(8), parameter :: omega = 1.8 ! it must be from (1, 2)

  real(8) :: region_x_length, region_y_length, region_z_length
  real(8) :: h_x, h_y, h_z

  real(8) :: x(-l+2:sf+l-1, n-1), x_diff(sf, n-1) ! object of calc
  real(8) :: a_h_x_upper(sf, n-1), a_h_y_upper(sf, n-1), a_h_z_upper(sf, n-1) ! left-hand side
  real(8) :: a_h_x_lower(sf, n-1), a_h_y_lower(sf, n-1), a_h_z_lower(sf, n-1), a_diag ! left-hand side
  real(8) :: b(sf, n-1) ! right_hand side
  integer :: coef_h_x, coef_h_y

  ! error check
  real(8) :: norm_diff, norm_x, norm_b

  ! iteration
  integer :: i, j, count

  ! variables for XMP
  !integer :: myrank, nprocs
  !integer :: xmp_node_num, xmp_num_nodes


  ! initialization

  ! xmp directives
  !$xmp nodes p(*)
  !$xmp template t(n-1)
  !$xmp distribute t(block) onto p
  !$xmp align(*,k) with t(k) :: x, x_diff, b
  !$xmp align(*,k) with t(k) :: a_h_x_upper, a_h_y_upper, a_h_z_upper
  !$xmp align(*,k) with t(k) :: a_h_x_lower, a_h_y_lower, a_h_z_lower
  !$xmp shadow x(0,1)

  !myrank = xmp_node_num()
  !nprocs = xmp_num_nodes()

  ! constants
  region_x_length = region_x_upper - region_x_lower ! = 1
  region_y_length = region_y_upper - region_y_lower ! = 1
  region_z_length = region_z_upper - region_z_lower ! = 1
  h_x = (region_x_length)/l ! 1/l
  h_y = (region_y_length)/m ! 1/m
  h_z = (region_z_length)/n ! 1/n
  
  ! matrix
  a_diag = -2.0d0*((1.0d0/h_x**2)+(1.0d0/h_y**2)+(1.0d0/h_z**2))

  ! array指示文
  !$xmp array on t
  a_h_x_upper = 0.0d0

  !$xmp array on t
  a_h_y_upper = 0.0d0

  !$xmp array on t
  a_h_z_upper = 0.0d0

  !$xmp array on t
  a_h_x_lower = 0.0d0

  !$xmp array on t
  a_h_y_lower = 0.0d0

  !$xmp array on t
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
  !$xmp array on t
  b = 0.0d0
  norm_b = 0.0d0
  coef_h_x = 0
  coef_h_y = 0

  !$xmp task on t(n-1)
  do i = 1, sf
     coef_h_x = mod(i-1, l-1) + 1
     coef_h_y = (i-1)/(l-1) + 1
     b(i, n-1) = -sin(coef_h_x*h_x*pi)*sin(coef_h_y*h_y*pi)/h_z**2
     norm_b = norm_b + b(i, n-1)**2
  end do
  norm_b = sqrt(norm_b)
  !$xmp end task

  !$xmp bcast (norm_b) from t(bcast_from) on t(1:bcast_from)

  ! main loop
  count = 0
  norm_diff = 0.0d0
  norm_x = 0.0d0

  !$xmp array on t
  x = 0.0d0

  !$xmp task on p(1)
  write(*,*) "epsilon = ", epsilon
  !$xmp end task

  ! first sync, that is, initialize
  !$xmp reflect(x)

  do
     ! calculate new vector

     !$xmp loop on t(j)
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
     
     ! message transfer
     !$xmp reflect(x)

     ! calculate norm
     !$xmp loop on t(j)
     do j = 1, n-1
        do i = 1, sf
           x_diff(i, j) = b(i, j) - a_diag*x(i, j) &
                - a_h_x_lower(i, j)*x(i-1, j) - a_h_x_upper(i, j)*x(i+1, j) &
                - a_h_y_lower(i, j)*x(i-l+1, j) - a_h_y_upper(i, j)*x(i+l-1, j) &
                - a_h_z_lower(i, j)*x(i, j-1) - a_h_z_upper(i, j)*x(i, j+1)
        end do
     end do

     !$xmp loop on t(j) reduction(+:norm_diff,norm_x)
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

     ! shinchoku dou desuka?
     !$xmp task on p(1)
     write(*, '(i5, e15.5)') count, norm_diff/norm_b
     !$xmp end task

     ! check convergence
     if(norm_diff <= epsilon*norm_b) exit
     
     ! preparation of next iteration
     norm_diff = 0.0d0
     norm_x = 0.0d0

     !$xmp barrier

  end do

  ! output
  !$xmp task on t(1)
  write(*, '(/,A,i6)') "iteration: ", count
  write(*, '(A, e15.5)') "norm_x = ", norm_x
  !$xmp end task

100 format(2i4, X, f10.8)

end program xmp_sor_3d_2_red_black

! 2015/05/14
! written by Shu OGAWARA
