program advection_equation

  integer, parameter :: n = 30
  double precision, parameter :: coef_c = -2.0d0
  double precision, parameter :: delta_t = 0.02d0
  double precision, parameter :: delta_x = 1.0d0/n

  double precision :: t = 0.0d0
  
  double precision :: u(n), nu(n)
  double precision :: diff = 0.0d0

  integer :: i

  ! initialization
  do i = 1, n
     u(i) = exp(-100*(i*delta_x-0.5d0)**2)
  end do

  call output

  ! main loop
  do while(t < 1.0d0)

     ! calclulate values in next step
     do i = 1, n-1
        nu(i) = u(i) + (coef_c*delta_t/delta_x)*(u(i) - u(i+1))
     end do

     ! calculate a value on border
     nu(n) = u(n) + (coef_c*delta_t/delta_x)*(u(n) - u(1))

     ! update
     t = t + delta_t

     do i = 1, n
        u(i) = nu(i)
     end do

     ! output
     call output

  end do

  ! output
  call output


contains
subroutine output

  write(*, 100) t, 0.0d0, nu(n) ! when x = 0.0d0

  do i = 1, n
     write(*, 100) t, i*delta_x, u(i)
  end do

  write(*, *) "" ! CR+LF

100 format(f7.3, f7.3, e15.5)

end subroutine output
end program advection_equation
