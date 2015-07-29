program wave_2
  implicit none

  ! set parameter
  integer, parameter :: N = 50, M = 100
  double precision, parameter :: dt = 1.0d0/M
  double precision, parameter :: dx = 1.0d0/N, dy = 1.0d0/N

  ! set time
  double precision :: t = 0.0d0

  ! set array
  double precision :: u(0:N, 0:N), old_u(0:N, 0:N), new_u(0:N, 0:N)

  ! set temporary variables
  integer :: i, j
  double precision :: x, y
  integer :: counter = 0

  ! initialization
  do j = 0, N
     do i = 0, N

        if(i*j == 0 .or. i == N .or. j == N) then
           u(i, j) = 0.0d0
        else 
           x = i*dx
           y = j*dy
           u(i, j) = exp(-100*((x-0.5d0)**2 + (y-0.5d0)**2))
        end if

        ! apply initial condition : u_t = 0
        old_u(i, j) = u(i, j)

     end do
  end do

  ! output on t = 0.0d0
  call output

  ! main loop
  do while(t < 1.0d0)

     ! calculate next condition
     do j = 1, N-1
        do i = 1, N-1
           new_u(i, j) = 2*u(i, j) - old_u(i, j) &
                 + dt**2 * ((u(i+1, j)-2*u(i, j)+u(i-1, j))*N*N + (u(i, j+1)-2*u(i, j)+u(i, j-1))*N*N)
        end do
     end do

     ! apply boundary condition on ...
     do j = 1, N-1
        new_u(0, j) = 0.0d0 ! x = 0
        new_u(N, j) = 0.0d0 ! x = 1
     end do

     do i = 1, N-1
        new_u(i, 0) = 0.0d0 ! y = 0
        new_u(i, N) = 0.0d0 ! y = 1
     end do

     ! put t forward by 1-step
     t = t + dt
     counter = counter + 1

     ! update array
     do j = 0, N
        do i = 0, N
           old_u(i, j) = u(i, j)
           u(i, j) = new_u(i, j)
        end do
     end do

     if(counter == 25 .or. counter == 50) call output

  end do

  ! output on t = 1.0d0
  call output

contains
subroutine output

  do j = 0, N
     do i = 0, N
        write(*, 100) t, i*dx, j*dy, u(i, j) ! t, x, y, u
     end do
  end do

  write(*, *) "" ! CR+LF

100 format(3(f7.3), e15.5)

end subroutine output
end program wave_2
