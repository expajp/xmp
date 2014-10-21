subroutine  mkcoef

  use cmmod
  implicit none

  integer :: i, j, k
  integer :: ip

  ! --- ( for ilsolv = 5 )------------------------------------------------

  !$xmp loop on t(k)
  do  k = 1, n

     ip  =  0

     do j = 1, m
        do i = 1, l

           ip  =  ip + 1

           if( k .ne. 1 )  zcoef(ip,1,k)  =  -odz2
           if( j .ne. 1 )  zcoef(ip,2,k)  =  -ody2
           if( i .ne. 1 )  zcoef(ip,3,k)  =  -odx2

           if( i .ne. l )  zcoef(ip,5,k)  =  -odx2
           if( j .ne. m )  zcoef(ip,6,k)  =  -ody2
           if( k .ne. n )  zcoef(ip,7,k)  =  -odz2

        end do
     end do
  end do

  !$xmp loop on t(k)
  do k = 1, n
     do ip = 1, lm

        zcoef(ip,4,k)  =  - zcoef(ip,1,k) - zcoef(ip,2,k) &
             - zcoef(ip,3,k) + odx2 &
             - zcoef(ip,6,k) - zcoef(ip,7,k)

        ! for Neumann condition        
        ! &                      - zcoef(ip,3,k) + zcoef(ip,5,k)
        ! &                      - zcoef(ip,6,k) - zcoef(ip,7,k)

     end do
  end do
  
  ! for reference point      
  ! zcoef(lmctr,1,nctr)  =  0.0d0
  ! zcoef(lmctr,2,nctr)  =  0.0d0
  ! zcoef(lmctr,3,nctr)  =  0.0d0
  ! zcoef(lmctr,4,nctr)  =  1.0d0
  ! zcoef(lmctr,5,nctr)  =  0.0d0
  ! zcoef(lmctr,6,nctr)  =  0.0d0
  ! zcoef(lmctr,7,nctr)  =  0.0d0

  return

end subroutine mkcoef
