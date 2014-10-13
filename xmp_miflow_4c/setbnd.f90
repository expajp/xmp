 subroutine  setbnd

  use cmmod
  implicit none

  integer :: i, j, k

  ! ---( BC for velocity <u> )--------------------------------------------

  !$xmp loop on t(k)
  do k = 2, n1
     do j = 2, m1
        u( 1, j, k)  =   uinit
        u(l1, j, k)  =   u(l,j,k)
     end do
  end do

  !$xmp loop on t(k)
  do k = 2, n1
     do i = 2, l
        u( i, 1, k)  =   u( i, 2, k)
        u( i,m2, k)  =   u( i,m1, k)
     end do
  end do

  ! no effect to other nodes
  if(myrank == 1) then
     do j = 2, m1
        do i = 2, l
           u( i, j, 1)  =  -u( i, j, 2)
        end do
     end do
  end if

  if(myrank == nprocs) then
     do j = 2, m1
        do i = 2, l
           u( i, j,n2)  =  -u( i, j,n1)
        end do
     end do
  end if


  ! ---( BC for velocity <v> )--------------------------------------------

  !$xmp loop on t(k)
  do k = 2, n1
     do j = 2, m
        v( 1, j, k)  =  -v( 2,j,k)
        v(l2, j, k)  =   v(l1,j,k)
     end do
  end do

  !$xmp loop on t(k)
  do k = 2, n1
     do i = 2, l1
        v( i, 1, k)  =   v( i, 2, k)
        v( i,m1, k)  =   v( i, m, k)
     end do
  end do

  ! no effect to other nodes
  if(myrank == 1) then
     do j = 2, m
        do i = 2, l1
           v( i, j, 1)  =  -v( i, j, 2)
        end do
     end do
  end if

  if(myrank == nprocs) then
     do j = 2, m
        do i = 2, l1
           v( i, j,n2)  =  -v( i, j,n1)
        end do
     end do
  end if


  ! ---( BC for velocity <w> )--------------------------------------------

  !$xmp loop on t(k)
  do k = 2, n
     do j = 2, m1
        w( 1, j, k)  =  -w( 2,j,k)
        w(l2, j, k)  =   w(l1,j,k)
     end do
  end do

  !$xmp loop on t(k)
  do k = 2, n
     do i = 2, l1
        w( i, 1, k)  =   w( i, 2, k)
        w( i,m2, k)  =   w( i,m1, k)
     end do
  end do

  ! no effect to other nodes
  do j = 2, m1
     do i = 2, l1
        w( i, j, 1)  =   0.0d0
        w( i, j,n1)  =   0.0d0
     end do
  end do

  ! no effect to other nodes
  if(myrank == 1) then
     do j = 2, m1
        do i = 2, l1
           w( i, j, 1)  =  0.0d0
        end do
     end do
  end if

  if(myrank == nprocs) then
     do j = 2, m1
        do i = 2, l1
           w( i, j,n2)  =  0.0d0
        end do
     end do
  end if


  ! ----------------------------------------------------------------------

  return

end subroutine setbnd
