module sample_size
  implicit none

  integer, PARAMETER :: mimax=1024, mjmax=512, mkmax=512

end module sample_size

program sample
  use sample_size
  implicit none

! Set Variables
  integer :: i, j, k

! Variables for XMP
  integer :: cpu0, cpu1, cpu2, t_rate, t_max, diff

! Set Arrays
  real :: x(mimax,mjmax,0:mkmax+1)
  real :: y(mimax,mjmax,0:mkmax+1)

  x = y = 0.0

! --- count start --- !

! initialize array
  do k=1, mkmax
     do j=1, mjmax
        do i=1, mimax
           x(i, j, k) = i + j + k
        end do
     end do
  end do

! message transfer

! --- count split 01 --- !

! main loop
  do k=1, mkmax
     do j=1, mjmax
        do i=1, mimax

           ! substitute to y
           y(i, j, k) = x(i, j, k-1) + x(i, j, k+1)

           ! output for debug
           ! write(*,'(A,3(i3,A),f5.1)') 'y(', i, ',', j, ',', k, ') = ',y(i, j, k)

        end do
     end do
  end do

! --- count stop --- !

! output
  write(*,*)
  write(*,*) 'The time was counted.'
  write(*,'(A,f8.5)') 'Initialize (s): ',cpu1-cpu0
  write(*,'(A,f8.5)') 'Caliculate (s): ',cpu2-cpu1
  write(*,'(A,f8.5)') 'Total (s): ',cpu2-cpu0
  write(*,*)

end program sample
