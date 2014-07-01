module sample_size
  implicit none

  integer, PARAMETER :: mimax=1024, mjmax=1024, mkmax=512

end module sample_size

program sample
  use sample_size
  implicit none

! Set Variables
  integer :: i, j, k

! Variables for Calculation
  integer :: cpu0
  integer :: cpu1, rate1, max1, diff1
  integer :: cpu3, rate3, max3, diff3

! Set Arrays
  real :: x(mimax,mjmax,0:mkmax+1) = 0.0
  real :: y(mimax,mjmax,0:mkmax+1) = 0.0

! --- count start --- !
  call system_clock(cpu0)

! initialize array
  do k=1, mkmax
     do j=1, mjmax
        do i=1, mimax
           x(i, j, k) = i + j + k
        end do
     end do
  end do

! --- count split 01 --- !
  call system_clock(cpu1, rate1, max1)

! message transfer
! no need, of course

! --- count split 02 --- !
! cpu2 = cpu1

! main loop
  do k=1, mkmax
     do j=1, mjmax
        do i=1, mimax

           ! substitute to y
           y(i, j, k) = x(i, j, k-1) + x(i, j, k+1)

           ! output for debug
           !if(mod(i*j*k,3**15) == 0) then
           !   write(*,'(A,3(i3,A),f7.1)') 'y(', i, ',', j, ',', k, ') = ',y(i, j, k)
           !end if

        end do
     end do
  end do

! --- count stop --- !
  call system_clock(cpu3, rate3, max3)

! to protect a compiler to remove for-loop
  write(*, *) y(mimax, mjmax, mkmax)

! calculate time
  if ( cpu1 < cpu0 ) then
    diff1 = max1 - cpu0 + cpu1
  else
    diff1 = cpu1 - cpu0
  endif

  if ( cpu3 < cpu0 ) then
    diff3 = max3 - cpu0 + cpu3
  else
    diff3 = cpu3 - cpu0
  endif

! output
!  write(*,*)
!  write(*,*) 'The time was counted.'
!  write(*,'(A,f8.5)') 'Initialize (s): ',cpu1-cpu0
!  write(*,'(A,f8.5)') 'Caliculate (s): ',cpu2-cpu1
!  write(*,'(A,f8.5)') 'Total (s): ',cpu2-cpu0
  write(*,'(i3,X,4(f9.6,X))') mkmax, diff1/dble(rate1), 0.0, (diff3/dble(rate3))-(diff1/dble(rate1)), diff3/dble(rate3) ! for descripting a graph
!  write(*,*)

end program sample
