module single_size

  integer, PARAMETER :: mimax=1024,mjmax=512*512

end module single_size

program single

  use single_size

! Set Variables and Arrays
  integer :: i, j

  real :: x(mimax,mjmax)
  real :: y(mimax,mjmax)

! Variables for caliculating time
  double precision :: xmp_wtime, cpu0, cpu1, cpu2

! Variables for parallel
  integer :: myrank, xmp_node_num

! XMP directives
  !$xmp nodes n(*)
  !$xmp template t(mjmax)
  !$xmp distribute t(block) onto n
  !$xmp align (*,j) with t(j) :: x, y
  !$xmp shadow x(0,1)

! initialization
  myrank = xmp_node_num()
  cpu0 = xmp_wtime()

  !$xmp reflect (x)
  !$xmp loop on t(j)
  do j=1, mjmax
     do i=1, mimax
        x(i, j) = i + j
        y(i,j) = 0
     end do
  end do

  write(*,'(A,i3,A)') 'The initialization of rank ',myrank,' was over.'

  !$xmp barrier
  cpu1 = xmp_wtime()

! main loop

  !$xmp reflect (x)
  !$xmp loop on t(j)
  do j=1, mjmax
     do i=2, mimax-1
        ! substitute to y
        !y(i) = x(i)
        y(i,j) = x(i-1,j) + x(i,j) + x(i+1,j)
     end do
  end do
 
  cpu2 = xmp_wtime()
  write(*,'(A,i3,A,f8.5,A)') 'Rank', myrank, ', finished in ',cpu2-cpu0, 'sec.'

  !$xmp barrier
  cpu2 = xmp_wtime()

  if(myrank == 1) then
     write(*,*) 'The time was counted.'
     write(*,'(A,f8.5)') 'Initialize (s): ',cpu1-cpu0
     write(*,'(A,f8.5)') 'Caliculate (s): ',cpu2-cpu1
     write(*,'(A,f8.5)') 'Total (s): ',cpu2-cpu0

  end if

end program single
