module sample_size

  PARAMETER (mimax=128, mjmax=64, mkmax=64)

end module sample_size

program sample

  use sample_size

! Set Variables and Arrays
  integer i, j, k

  real x(mimax, mjmax, mkmax)
  real y(mimax, mjmax, mkmax)

! initialization
  do k=1, mkmax
     do j=1, mjmax
        do i=1, mimax
           x(i, j, k) = i + j + k
        end do
     end do
  end do

  write(*,*) 'The initialization was over.'

  write(*,*) 'Print only such matters that &
               the product of its i, j, and k is the multiple of 2^17'

              y(1,:,:) = x(1,:,:)
              y(:,1,:) = x(:,1,:)
              y(mimax,:,:) = x(mimax,:,:)
              y(:,mjmax,:) = x(:,mjmax,:)

! main loop
  do k=1, mkmax
     do j=2, mjmax-1
        do i=2, mimax-1
           ! substitute to y
              y(i, j, k) = x(i-1, j-1, k) + x(i+1, j+1, k)

           ! output
           if(mod(i*j*k,2**16) == 0) then
              write(*,'(A,3(i3,A),f5.1)') 'y(', i, ',', j, ',', k, ') = ',y(i, j, k)
           end if

        end do
     end do
  end do
  
  write(*,*) 'Finished.'

end program sample
