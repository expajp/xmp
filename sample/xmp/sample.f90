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
  integer :: myrank, xmp_node_num
  integer :: nprocs, xmp_all_num_nodes
  integer :: kstart, kend
  double precision :: xmp_wtime, cpu0, cpu1, cpu2

! XMP directives
  !$xmp nodes n(*)
  !$xmp template t(mkmax)
  !$xmp distribute t(block) onto n
  !$xmp align (*,*,k) with t(k) :: x, y
  !$xmp shadow x(0,0,1)

  myrank = xmp_node_num()
  nprocs = xmp_all_num_nodes()

  kstart = mkmax*myrank/nprocs+1
  kend = mkmax*(myrank+1)/nprocs

! initialize array
  cpu0 = xmp_wtime()

  !$xmp reflect (x)
  !$xmp loop on t(k)
  do k=1, mkmax
     do j=1, mjmax
        do i=1, mimax
           x(i, j, k) = i + j + k
           y(i, j, k) = 0
        end do
     end do
  end do

! introduction
    !$xmp barrier
    cpu1 = xmp_wtime()

! main loop

  !$xmp reflect (x)
  !$xmp loop on t(k)
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

           ! output
           if(i == 2 .and. j == 2 .and. mod(k,mkmax/nprocs) == 1) then
              write(*,'(A,3(i3,A),f5.1)') 'y(', i, ',', j, ',', k, ') = ',y(i, j, k)
           end if

        end do
     end do
  end do
  
  !for debug
  !cpu2 = xmp_wtime()
  !write(*,'(A,i3,A)') 'Rank', myrank, ', finished.'

  !$xmp barrier
  cpu2 = xmp_wtime()

  if(myrank == 1) then
     write(*,*)
     write(*,*) 'The time was counted.'
     write(*,'(A,f8.5)') 'Initialize (s): ',cpu1-cpu0
     write(*,'(A,f8.5)') 'Caliculate (s): ',cpu2-cpu1
     write(*,'(A,f8.5)') 'Total (s): ',cpu2-cpu0
     write(*,*)
!     write(*,'(f9.6)') cpu2-cpu0 ! for descripting a graph
  end if

  !There is no directive to finalize XMP

end program sample
