program advection_equation

  integer, parameter :: n = 30
  double precision, parameter :: coef_c = 2.0d0
  double precision, parameter :: delta_t = 0.01d0
  double precision, parameter :: delta_x = 1.0d0/n

  double precision :: t = 0.0d0
  
  double precision :: u(n), nu(n)
  double precision :: diff = 0.0d0

  integer :: i

  ! initialization
  write(*, '(f7.3, f7.3, e15.5)') t, 0.0d0, exp(-100*(0.5d0)**2)
  do i = 1, n
     u(i) = exp(-100*(i*delta_x-0.5d0)**2)
     write(*, '(f7.3, f7.3, e15.5)') t, i*delta_x, u(i)
  end do
  write(*, *) ! CR+LF

  ! main loop
  do while(t < 1.0d0)

     ! calclulate values in next step
     do i = 1, n-1
        nu(i) = u(i) + (coef_c*delta_t/delta_x)*(u(i) - u(i+1))
     end do

     ! calculate a value on border
     nu(n) = u(n) + (coef_c*delta_t/delta_x)*(u(n) - u(1))

     ! cumulate difference
     do i = 1, n
        diff = diff + abs(nu(i) - exp(-100*((i*delta_x-coef_c*t)-0.5d0)**2))
     end do

     ! update
     t = t + delta_t

     write(*, '(f7.3, f7.3, e15.5)') t, 0.0d0, nu(n)

     do i = 1, n
        u(i) = nu(i)
        write(*, '(f7.3, f7.3, e15.5)') t, i*delta_x, u(i)
     end do

     write(*, *) "" ! CR+LF

  end do

  write(*, '(f7.3, f7.3, e15.5)') t, 0.0d0, u(n)

  do i = 1, n
    write(*, '(f7.3, f7.3, e15.5)') t, i*delta_x, u(i)
  end do

  ! write(*, '(A, e15.5)') "diff = ", diff

end program advection_equation
