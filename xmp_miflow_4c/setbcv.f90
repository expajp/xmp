subroutine  setbcv

  use cmmod
  implicit none

  integer :: i, j, k

  ! ---( BC for velocity <u> )--------------------------------------------

  !$xmp loop on t(k)
  do k = 2, n1
     do j = 2, m1
        u1( 1, j, k)  =   uinit
        u1(l1, j, k)  =   u1(l, j, k)
     end do
  end do

  ! ---( BC for velocity <v> )--------------------------------------------

  !$xmp loop on t(k)
  do k = 2, n1
     do i = 2, l1
        v1( i, 1, k)  =   v1( i, 2, k)
        v1( i,m1, k)  =   v1( i, m, k)
     end do
  end do

  ! ---( BC for velocity <w> )--------------------------------------------

  if(myrank == 1) then
     do j = 2, m1
        do i = 2, l1
           w1( i, j, 1)  =   0.0d0
        end do
     end do
  end if
  
  if(myrank == nprocs) then
     do j = 2, m1
        do i = 2, l1
           w1( i, j,n1)  =   0.0d0
        end do
     end do
  end if

  return

end subroutine setbcv
