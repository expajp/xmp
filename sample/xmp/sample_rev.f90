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
  integer :: nprocs, xmp_all_num_nodes
  integer :: kstart, kend
  double precision :: xmp_wtime, cpu0, cpu1, cpu2

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

  kstart = mkmax*myrank/nprocs+1
  kend = mkmax*(myrank+1)/nprocs

! --- count start --- !
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

! message transfer

! --- count split 01 --- !

! main loop

!$xmp reflect (x)
!$xmp loop on t(k)
  do k=1, mkmax
     do j=1, mjmax
        do i=1, mimax

           ! substitute to y
           y(i, j, k) = x(i, j, k-1) + x(i, j, k+1)

           ! output for debug
           if(mod(i*j*k,3**15) == 0) then
              write(*,'(A,3(i3,A),f7.1)') 'y(', i, ',', j, ',', k, ') = ',y(i, j, k)
           end if

        end do
     end do
  end do

! --- count stop --- !

! output
!  write(*,*)
  write(*,*) 'The time was counted.'
!  write(*,'(A,f8.5)') 'Initialize (s): ',cpu1-cpu0
!  write(*,'(A,f8.5)') 'Caliculate (s): ',cpu2-cpu1
!  write(*,'(A,f8.5)') 'Total (s): ',cpu2-cpu0
!  write(*,*)

end program sample
