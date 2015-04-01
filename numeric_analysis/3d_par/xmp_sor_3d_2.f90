program xmp_sor_3d_2
  implicit none

  ! mesh
  integer, parameter :: l = 100, m = 100, n = 129 ! 2^n 分割をしやすくするため
  integer, parameter :: mesh = (l-1)*(m-1)*(n-1)
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
  real(8), parameter :: pi = acos(-1.0d0)
  real(8), parameter :: omega = 1.5 ! it must be from (1, 2)

  real(8) :: region_x_length, region_y_length, region_z_length
  real(8) :: h_x, h_y, h_z

  real(8) :: a_diag
  real(8) :: x(sf, n-1), x_old(sf, n-1), x_diff(sf, n-1) ! object of calc
  real(8) :: a_h_x(sf, n-1), a_h_y(sf, n-1), a_h_z(sf, n-1) ! left-hand side
  real(8) :: b(sf, n-1) ! right_hand side
  integer :: coef_h_x, coef_h_y

  ! error check
  real(8) :: norm_diff, norm_x

  ! iteration
  integer :: i, j, count

  ! variables for XMP
  integer :: myrank, nprocs
  integer :: xmp_node_num, xmp_num_nodes


  ! initialization

  ! xmp directives
  !$xmp nodes p(4)
  !$xmp template t(n-1)
  !$xmp distribute t(block) onto p
  !$xmp align(*,k) with t(k) :: x, x_old, x_diff, a_h_x, a_h_y, a_h_z, b
  !$xmp shadow x(0,1)
  !$xmp shadow a_h_z(0,1)

  myrank = xmp_node_num()
  nprocs = xmp_num_nodes()

  write(*,'(A,i2,A)') "Hello! My Rank is ", myrank, "."

  ! constants
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
        if(mod(i,l-1) /= 0) a_h_x(i, j) = 1.0d0/h_x**2
        if(i <= (l-1)*(m-2)) a_h_y(i, j) = 1.0d0/h_y**2
        if(j /= 0 .and. j /= n-1) a_h_z(i, j) = 1.0d0/h_z**2
     end do
  end do

  !$xmp reflect(a_h_z)

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

  ! 初期化サボって大丈夫かな？
  x = 0.0d0
  x_old = 0.0d0

  if(myrank == 1) write(*,*) "epsilon = ", epsilon

  ! first sync, that is, initialize
  !$xmp reflect(x)

  do
     ! x_old = x
     !$xmp loop on t(j)
     do j = 1, n-1
        do i = 1, sf
           x_old(i, j) = x(i, j)
        end do
     end do

     ! calculate new vector
     ! 未定義領域の参照があるため、エラーが出たら一応ここを疑ってみること
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
     
     ! message transfer
     !$xmp reflect(x)

     ! calculate norm
     ! x_diff = x - x_old
     !$xmp loop on t(j)
     do j = 1, n-1
        do i = 1, sf
           x_diff(i, j) = x(i, j) - x_old(i, j)
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

     ! check convergence
     if(norm_diff <= epsilon*norm_x) exit
     
     ! preparation of next iter
     count = count+1

     ! shinchoku dou desuka?
     if(myrank == 1 .and. mod(count,500) == 0) then
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
  if(myrank == nprocs/2) then
     do i = 1, l-1
        write(*, '(i3, e15.5)') i, x((l-1)*((m-1)/2+1)+i, (n-1)/2)
     end do
  end if

100 format(2i4, X, f10.8)

end program xmp_sor_3d_2

! 2015/04/01
! written by Shu OGAWARA
