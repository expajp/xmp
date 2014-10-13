subroutine initu

  use multimod

  integer :: i, j, k
  real(8) :: umax

  !integer :: myrank, xmp_node_num

  !myrank = xmp_node_num()

  umax = 0.0d0

!$xmp loop on t(k) reduction(max:umax)
  do k = 1, n2
     do j = 1, m2
        do i = 1, l1
           u(i, j, k) = dble(i + j + k)
           umax = max(umax, abs(u(i, j, k)))
        end do
     end do
  end do

!$xmp barrier
  write(*,*) "myrank = ", myrank
  if(myrank == 1)  write(*,*) "umax = ", umax

end subroutine initu
