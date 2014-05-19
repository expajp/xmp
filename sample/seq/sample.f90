module sample_size
  implicit none

  integer, PARAMETER :: mimax=1024, mjmax=512, mkmax=512

end module sample_size

program sample
  use sample_size
  implicit none

! Set Variables and Arrays
  integer :: i, j, k

  real :: x(mimax,mjmax,mkmax)
  real :: y(mimax,mjmax,mkmax)

! Variables for XMP
  integer :: cpu0, cpu1, cpu2, t_rate, t_max, diff

! initialize array
  call system_clock(cpu0)

  do k=1, mkmax
     do j=1, mjmax
        do i=1, mimax
           x(i, j, k) = i + j + k
           y(i, j, k) = 0
        end do
     end do
  end do

    call system_clock(cpu1)

! main loop

  do k=1, mkmax
     do j=2, mjmax-1
        do i=2, mimax-1
           ! substitute to y
           if(k == 1) then
              y(i, j, k) = x(i+1, j+1, k+1)
           else if(k == mkmax) then
              y(i, j, k) = x(i-1, j-1, k-1)
           else
              y(i, j, k) = x(i-1, j-1, k-1) + x(i+1, j+1, k+1)
           end if

           ! output for debug
           !if(i == 2 .and. j == 2 .and. mod(k,mkmax/nprocs) == 1) then
           !   write(*,'(A,3(i3,A),f5.1)') 'y(', i, ',', j, ',', k, ') = ',y(i, j, k)
           !end if

        end do
     end do
  end do
  
  call system_clock(cpu2, t_rate, t_max)
  if ( cpu2 < cpu0 ) then
    diff = t_max - cpu0 + cpu2
  else
    diff = cpu2 - cpu0
  endif

!     write(*,*)
!     write(*,*) 'The time was counted.'
!     write(*,'(A,f8.5)') 'Initialize (s): ',cpu1-cpu0
!     write(*,'(A,f8.5)') 'Caliculate (s): ',cpu2-cpu1
!     write(*,'(A,f8.5)') 'Total (s): ',cpu2-cpu0
!     write(*,*)
     write(*,'(f9.6)') (cpu2-cpu0)/dble(t_rate)

end program sample
