module sample_size
  implicit none

  integer, PARAMETER :: mimax=128, mjmax=128, mkmax=512

end module sample_size

program sample
  use sample_size
  implicit none

! Set Variables
  integer :: i, j, k

! Variables for XMP
  integer :: myrank, xmp_node_num
  integer :: nprocs, xmp_all_num_nodes
  double precision :: xmp_wtime, cpu0, cpu1, cpu2, cpu3

! Set Arrays
  real :: x(mimax,mjmax,0:mkmax+1)
  real :: y(mimax,mjmax,0:mkmax+1)

! XMP directives
!$xmp nodes n(*)
!$xmp template t(0:mkmax+1)
!$xmp distribute t(block) onto n
!$xmp align (*,*,k) with t(k) :: x, y
!$xmp shadow x(0,0,1)

  x = 0.0
  y = 0.0

  myrank = xmp_node_num()
  nprocs = xmp_all_num_nodes()

! --- count start --- !
!$xmp barrier
  cpu0 = xmp_wtime()

! initialize array

!$xmp loop on t(k)
  do k=1, mkmax
     do j=1, mjmax
        do i=1, mimax
           x(i, j, k) = i + j + k
        end do
     end do
  end do

! --- count split 01 --- !
!$xmp barrier
  cpu1 = xmp_wtime()

! message transfer
!$xmp reflect (x)

! --- count split 02 --- !
!$xmp barrier
  cpu2 = xmp_wtime()

! main loop

!$xmp loop on t(k)
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
!$xmp barrier
  cpu3 = xmp_wtime()

! to protect a compiler to remove for-loop
  write(*, *) y(mimax, mjmax, mkmax*myrank/nprocs)

! output
if(myrank == 1) then
!  write(*,'(A,f10.7)') 'Initialize (s): ',cpu1-cpu0
!  write(*,'(A,f10.7)') 'Caliculate (s): ',cpu2-cpu1
!  write(*,'(A,f10.7)') 'Total (s): ',cpu2-cpu0
  write(*,'(i3,X,4(f9.6,X))') nprocs, cpu1-cpu0, cpu2-cpu1, cpu3-cpu2, cpu3-cpu0 ! for descripting a graph
!  write(*,'(i3,X,f9.6))') nprocs, cpu2-cpu0 ! for descripting a graph
end if

end program sample
