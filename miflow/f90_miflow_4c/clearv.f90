subroutine  clearv

  use cmmod
  implicit none

  integer :: i, j, k
  integer :: ip, kp

  do k = 1, n2
     do j = 1, m2
        do i = 1, l1
           u (i,j,k)  =  0.0d0
           u1(i,j,k)  =  0.0d0
        end do
     end do
  end do

  do k = 1, n2
     do j = 1, m1
        do i = 1, l2
           v (i,j,k)  =  0.0d0
           v1(i,j,k)  =  0.0d0
        end do
     end do
  end do

  do k = 1, n1
     do j = 1, m2
        do i = 1, l2
           w (i,j,k)  =  0.0d0
           w1(i,j,k)  =  0.0d0
        end do
     end do
  end do

  do k = 1, n2
     do j = 1, m2
        do i = 1, l2
           p (i,j,k)  =  0.0d0
        end do
     end do
  end do


  do k = 1, n1
     do j = 1, m1
        do i = 1, l1
           wk1(i,j,k)  =  0.0d0
           wk2(i,j,k)  =  0.0d0
           wk3(i,j,k)  =  0.0d0
           dfs(i,j,k)  =  0.0d0
        end do
     end do
  end do

  do k = 1, n
     do ip = 1, lm
        zcoef(ip,1,k)  =  0.0d0
        zcoef(ip,2,k)  =  0.0d0
        zcoef(ip,3,k)  =  0.0d0
        zcoef(ip,4,k)  =  0.0d0
        zcoef(ip,5,k)  =  0.0d0
        zcoef(ip,6,k)  =  0.0d0
        zcoef(ip,7,k)  =  0.0d0
        zb(ip,k)       =  0.0d0
     end do
  end do

  do kp = 1, n2
     do ip = 1, lm2
        zx(ip,k)       =  0.0d0
     end do
  end do

  return

end subroutine clearv
