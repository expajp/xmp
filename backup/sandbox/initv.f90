subroutine initv

  use multimod

  integer :: i, j, k
  real(8) :: vmax

  integer :: myrank, xmp_node_num

  myrank = xmp_node_num()

  vmax = 0.0d0

!$xmp loop on t(k) reduction(max:vmax)
  do k = 1, n2
     do j = 1, m1
        do i = 1, l2
           v(i, j, k) = dble(i * j * k)
           vmax = max(vmax, abs(v(i, j, k)))
        end do
     end do
  end do

!$xmp barrier
  if(myrank == 1) write(*,*) "vmax = ", vmax

end subroutine initv
