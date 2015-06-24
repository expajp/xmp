program sor_3d_1
  implicit none

  ! mesh
  integer, parameter :: l = 100, m = 100, n = 129
  integer, parameter :: mesh = (l-1)*(m-1)*(n-1)
  integer, parameter :: sf = (l-1)*(m-1) ! sf:surface

  ! region
  double precision, parameter :: region_x_lower=0.0d0, region_x_upper=1.0d0
  double precision, parameter :: region_y_lower=0.0d0, region_y_upper=1.0d0
  double precision, parameter :: region_z_lower=0.0d0, region_z_upper=1.0d0

  ! border
  double precision, parameter :: border_x_lower=0.0d0, border_x_upper=0.0d0
  double precision, parameter :: border_y_lower=0.0d0, border_y_upper=0.0d0
  double precision, parameter :: border_z_lower=0.0d0 ! border_z_upper=sin(pi*x)sin(pi*y)

  ! constants
  double precision, parameter :: epsilon = 1.000E-08
  double precision, parameter :: pi = acos(-1.0d0)
  !double precision, parameter :: omega = 2.0d0/(1+sqrt(1-cos(pi/n)**2)) ! it must be from (1, 2)
  double precision, parameter :: omega = 1.9d0

  ! denominator
  double precision, parameter :: denomi = 1.0d0/sinh(sqrt(2.0d0)*pi)

  double precision :: region_x_length, region_y_length, region_z_length
  double precision :: h_x, h_y, h_z

  ! object of calc
  double precision :: x(-sf+1:mesh+sf), x_diff(mesh)

  ! left-hand side
  double precision :: a_diag
  double precision :: a_h_x_upper(mesh), a_h_y_upper(mesh), a_h_z_upper(mesh)
  double precision :: a_h_x_lower(mesh), a_h_y_lower(mesh), a_h_z_lower(mesh)

  ! right_hand side
  double precision :: b(mesh)

  ! the upper of region of b inserted border's value
  integer :: b_border_region
  integer :: coef_h_x, coef_h_y, coef_h_z

  ! error check
  double precision :: norm_diff, norm_x, norm_b, norm_analysis
  double precision :: diff
  double precision :: analysis_answer

  ! iteration
  integer :: i, j, count

  ! variables for time measurement
  integer :: time0, time1, time2, t_rate, t_max ! sequential
  ! double precision :: time0, time1, time2, time3 ! parallel
  double precision :: tick, caltime, comtime, checktime, alltime


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

     if(mod(i,l-1) /= 1) a_h_x_lower(i) = 1.0d0/h_x**2
     if(mod(i,sf) > l-1) a_h_y_lower(i) = 1.0d0/h_y**2
     if(i > sf) a_h_z_lower(i) = 1.0d0/h_z**2

  end do

  ! right-hand side
  b = 0.0d0
  b_border_region = mesh-sf
  norm_b = 0.0d0
  coef_h_x = 0
  coef_h_y = 0
  coef_h_z = 0

  do i = b_border_region+1, mesh
     coef_h_x = mod(i-b_border_region-1,l-1) + 1
     coef_h_y = (i-b_border_region-1)/(l-1) + 1
     b(i) = -sin(coef_h_x*h_x*pi)*sin(coef_h_y*h_y*pi)/h_z**2
     norm_b = norm_b + b(i)**2
  end do

  norm_b = sqrt(norm_b)

  ! main loop
  count = 0
  norm_diff = 0.0d0
  norm_x = 0.0d0
  diff = 0.0d0
  norm_analysis = 0.0d0

  x = 0.0d0

  caltime = 0.0d0
  comtime = 0.0d0
  checktime = 0.0d0
  alltime = 0.0d0

  call system_clock(time0, t_rate, t_max)
  tick = 1.0d0/t_rate

  do
     call system_clock(time0)

     ! calculate new vector
     do i = 1, mesh
        x(i) = (b(i) - a_h_x_lower(i)*x(i-1) - a_h_x_upper(i)*x(i+1) &
                     - a_h_y_lower(i)*x(i-l+1) - a_h_y_upper(i)*x(i+l-1) &
                     - a_h_z_lower(i)*x(i-sf) - a_h_z_upper(i)*x(i+sf)) &
                * (omega/a_diag) + (1-omega)*x(i)
     end do

     call system_clock(time1)
     
     ! calculate norm
     ! x_diff = x - x_old
     do i = 1, mesh
        x_diff(i) = b(i) - a_diag*x(i) &
                - a_h_x_lower(i)*x(i-1) - a_h_x_upper(i)*x(i+1) &
                - a_h_y_lower(i)*x(i-l+1) - a_h_y_upper(i)*x(i+l-1) &
                - a_h_z_lower(i)*x(i-sf) - a_h_z_upper(i)*x(i+sf)
     end do

     do i = 1, mesh
        norm_diff = norm_diff + x_diff(i)**2
        norm_x = norm_x + x(i)**2
     end do

     norm_diff = sqrt(norm_diff)
     norm_x = sqrt(norm_x)

     ! preparation of next iter
     count = count+1

     ! stop clock
     call system_clock(time2)

     if (time1 < time0) then
        caltime = caltime + ((t_max-time0)+time1+1)/dble(t_rate)
     else
        caltime = caltime + (time1-time0)/dble(t_rate)
     end if

     if (time2 < time1) then
        checktime = checktime + ((t_max-time1)+time2+1)/dble(t_rate)
     else
        checktime = checktime + (time2-time1)/dble(t_rate)
     end if

     if (time2 < time0) then
        alltime = alltime + ((t_max-time0)+time2+1)/dble(t_rate)
     else
        alltime = alltime + (time2-time0)/dble(t_rate)
     end if

     ! shinchoku dou desuka?
     write(*, '(i5, e15.5)') count, norm_diff/norm_b

     ! check convergence
     if(norm_diff <= epsilon*norm_b) exit

     ! preparation of next iteration
     norm_diff = 0.0d0
     norm_x = 0.0d0
     
  end do

  ! compare with analysed answer here
  do i = 1, mesh
     coef_h_x = mod(i-1,l-1) + 1
     coef_h_y = mod(i-1, sf)/(l-1) + 1
     coef_h_z = (i-1)/sf + 1
     analysis_answer = sin(pi*coef_h_x*h_x)*sin(pi*coef_h_y*h_y)*sinh(sqrt(2.0d0)*pi*coef_h_z*h_z)*denomi

     norm_analysis = norm_analysis + analysis_answer**2

     diff = diff + abs(x(i)-analysis_answer)
  end do

  norm_analysis = sqrt(norm_analysis)

  ! output
  write(*, '(/,A,i6,/)') "iteration: ", count

  write(*,200) "epsilon = ", epsilon
  write(*,200) "omega = ", omega
  write(*,200) "tick = ", tick

  write(*, '(/,2(A, e15.5))') "norm_x = ", norm_x, ", norm_analysis = ", norm_analysis
  write(*, 200) "diff = ", diff

  write(*, '(/,3(A, f9.4))') "caltime = ", caltime, ", comtime = ", comtime, ", checktime = ", checktime
  write(*, '((A, f9.4),/)') "alltime = ", alltime

  ! 仮想マシンではこれがないとエラーを吐く
  ! 逆に、piではこれがあるとここの部分で処理が止まって実行が終わらない
  ! deallocate(x, x_diff)
  ! deallocate(a_h_x_upper, a_h_y_upper, a_h_z_upper)
  ! deallocate(a_h_x_lower, a_h_y_lower, a_h_z_lower)
  ! deallocate(b)

100 format(2i4, X, f10.8)
200 format(A, e15.5)

end program sor_3d_1

! 2015/06/17
! written by Shu OGAWARA
