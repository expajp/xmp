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
  integer :: myrank, xmp_node_num
  double precision :: xmp_wtime, cpu0, cpu1, cpu2

! Set Arrays
  real :: x(mimax,mjmax,0:mkmax+1)
  real :: y(mimax,mjmax,0:mkmax+1)

! XMP directives
!$xmp nodes n(1,1,*)
!$xmp template t(mimax, mjmax, 0:mkmax+1)
!$xmp distribute t(block,block,block) onto n
!$xmp align (i,j,k) with t(i,j,k) :: x, y
!$xmp shadow x(0,0,1)

  x = 0.0
  y = 0.0

  myrank = xmp_node_num()

! --- count start --- !
!$xmp barrier
  cpu0 = xmp_wtime()

! initialize array

!$xmp loop on t(*,*,k)
  do k=1, mkmax
!$xmp loop on t(*,j,*)
     do j=1, mjmax
!$xmp loop on t(i,*,*)
        do i=1, mimax
           x(i, j, k) = i + j + k
        end do
     end do
  end do


! message transfer
!$xmp reflect (x)

! --- count split 01 --- !
!$xmp barrier
  cpu1 = xmp_wtime()

! main loop

!$xmp loop on t(*,*,k)
  do k=1, mkmax
!$xmp loop on t(*,j,*)
     do j=1, mjmax
!$xmp loop on t(i,*,*)
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
!$xmp barrier
  cpu2 = xmp_wtime()

! output
if(myrank == 1) then
  write(*,'(A,f8.5)') 'Initialize (s): ',cpu1-cpu0
  write(*,'(A,f8.5)') 'Caliculate (s): ',cpu2-cpu1
  write(*,'(A,f8.5)') 'Total (s): ',cpu2-cpu0
! write(*,'(f9.6)') cpu2-cpu0 ! for descripting a graph
end if

end program sample
