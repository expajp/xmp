subroutine  caluvw
  use cmmod
  implicit none

  integer :: i, j, k
  integer :: ip

  ! ---( Make right hand side of system of linear equation )--------------
  
  ! sync for [uvw]1(*, *, k+1)
  !$xmp reflect (u1)
  !$xmp reflect (v1)
  !$xmp reflect (w1)

  ! calculate

  !$xmp loop on t(k)
  do k = 1, n

     ip  =  0

     do j = 1, m
        do i = 1, l

           ip       =  ip + 1
           zb(ip,k) = odtodx*( - u1(i+1,j+1,k+1) + u1(i  ,j+1,k+1) ) &
                + odtody*( - v1(i+1,j+1,k+1) + v1(i+1,j  ,k+1) ) &
                + odtodz*( - w1(i+1,j+1,k+1) + w1(i+1,j+1,k  ) )

        end do
     end do
  end do

  ! for reference point      
  !         b(lmctr,nctr)  =  0.0d0


  ! ---( Poison solver )--------------------------------------------------

  call  lsor4c( l, lm, n, eps, maxitr, zcoef, zb, zx, omega, s1omg, myrank, nprocs )

  ! ---( put the solution into presure variable "p" )---------------------

  !$xmp loop on t(k)
  do k = 2, n1

     ip  =  0

     do j = 1, m
        do i = 1, l

           ip              =  ip + 1
           p(i+1,j+1,k)  =  zx(ip+l,k)

        end do
     end do
  end do

  ! ---( Velocity correction )--------------------------------------------

  !$xmp loop on t(k)
  do k = 2, n1
     do j = 2, m1
        do i = 2, l
           u(i,j,k) = u1(i,j,k) - dtodx*( p(i+1,j  ,k  ) - p(i,j,k) )
        end do
     end do
  end do

  !$xmp loop on t(k)
  do k = 2, n1
     do j = 2, m
        do i = 2, l1
           v(i,j,k) = v1(i,j,k) - dtody*( p(i  ,j+1,k  ) - p(i,j,k) )
        end do
     end do
  end do

  ! sync for p(*, *, k+1)
  !$xmp reflect (p)

  !$xmp loop on t(k)
  do k = 2, n
     do j = 2, m1
        do i = 2, l1
           w(i,j,k) = w1(i,j,k) - dtodz*( p(i  ,j  ,k+1) - p(i,j,k) )
        end do
     end do
  end do

  return

end subroutine caluvw
