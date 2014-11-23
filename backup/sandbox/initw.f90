subroutine initw

  use multimod

  integer :: i, j, k
  real(8) :: wmax

  integer :: myrank, xmp_node_num

  myrank = xmp_node_num()

  wmax = 0.0d0

!$xmp loop on t(k) reduction(max:wmax)
  do k = 1, n1
     do j = 1, m2
        do i = 1, l2
           w(i, j, k) = dble(i - j + k)
           wmax = max(wmax, abs(w(i, j, k)))
        end do
     end do
  end do

!$xmp barrier
  if(myrank == 1) write(*,*) "wmax = ", wmax

end subroutine initw
