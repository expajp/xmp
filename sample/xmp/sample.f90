module sample_size

  PARAMETER (mimax=256, mjmax=128, mkmax=128)

end module sample_size

program sample

  use sample_size

! Set Variables and Arrays
  integer i, j, k

  real x(mimax,mjmax,mkmax)
  real y(mimax,mjmax,mkmax)

! integer for parallel
  integer myrank, xmp_node_num

! XMP directives
  !$xmp nodes n(*)
  !$xmp template t(mimax*mjmax*mkmax)
  !$xmp distribute t(block) onto n
  !$xmp align x(*,*,k) with t(k)
  !$xmp align y(*,*,k) with t(k)
  !$xmp shadow x(0,0,1)

! initialization
  myrank = xmp_node_num()

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

     write(*,*) 'The initialization of rank ',myrank,' was over.'

  if(myrank == 0) then

     write(*,*) 'Print only such matters that &
          the product of its i, j, and k is the multiple of 2^18'
  end if

! main loop

  !$xmp reflect (x)
  !$xmp loop on t(k)
  do k=1, mkmax
     do j=2, mjmax-1
        do i=2, mimax-1
           ! substitute to y
              y(i, j, k) = x(i-1, j-1, k) + x(i+1, j+1, k)

           ! output
           if(mod(i*j*k,2**18) == 0) then
              write(*,'(A,3(i3,A),f5.1)') 'y(', i, ',', j, ',', k, ') = ',y(i, j, k)
           end if

        end do
     end do
  end do
  
  write(*,*) 'Rank', myrank, ', finished.'

end program sample
