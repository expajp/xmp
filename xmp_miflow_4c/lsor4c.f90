subroutine lsor4c
  use cmmod
  implicit none

  integer :: k, ip, kp, ix, iter
  real(8) :: bmax, res, rtmp, xtmp
  real(8) :: xmp_wtime

  ! start calculation
  bmax  =  0.0d0

  !$xmp loop on t(k) reduction(max:bmax)
  do k = 1, n
     do ip = 1, lm
        bmax  =  max( bmax, abs( zb(ip,k) ) )
     end do
  end do

  !$xmp reflect(zcoef)
  !$xmp reflect(zx)
  !$xmp reflect(zb)

  res  =  0.0d0

  !$xmp loop on t(k) reduction(max:res)
  do k = 2, n+1

     do ip = 1, lm
        ix    =  ip + l
        rtmp  =  zb(ip,k-1) - zcoef(ip,1,k-1)*zx(ix  , k-1)   &
             - zcoef(ip,2,k-1)*zx(ix-l, k  )   &
             - zcoef(ip,3,k-1)*zx(ix-1, k  )   &
             - zcoef(ip,4,k-1)*zx(ix  , k  )   &
             - zcoef(ip,5,k-1)*zx(ix+1, k  )   &
             - zcoef(ip,6,k-1)*zx(ix+l, k  )   &
             - zcoef(ip,7,k-1)*zx(ix  , k+1)
        res  =  max( res, abs(rtmp) )

     end do
  end do

  if( bmax .ne. 0.0 )   res = res / bmax

  if( res .lt. eps )  then

     iter = 0
     if(myrank == 1) write(6,6000) iter, res
     return

  end if

  ! --- ( iteration phase ) --------------------------------------------
  iter  =  0

10 continue ! label

  iter  =  iter + 1

  ! data has been received in the previous line of 'res = 0.0d0'

  !$xmp loop on t(k)
  do k = 2, n+1, 2 ! start is needed to be an odd number.

     do ip = 1, lm, 2

        ix      =  ip + l
        xtmp    = ( zb(ip,k-1) - zcoef(ip,1,k-1)*zx(ix  , k-1) &
             - zcoef(ip,2,k-1)*zx(ix-l, k  ) &
             - zcoef(ip,3,k-1)*zx(ix-1, k  ) &
             - zcoef(ip,5,k-1)*zx(ix+1, k  ) &
             - zcoef(ip,6,k-1)*zx(ix+l, k  ) &
             - zcoef(ip,7,k-1)*zx(ix  , k+1) ) &
             / zcoef(ip,4,k-1)
        zx(ix,k) =  s1omg*zx(ix,k) + omega*xtmp

     end do

     do ip = 2, lm, 2

        ix      =  ip + l
        xtmp    = ( zb(ip,k-1) - zcoef(ip,1,k-1)*zx(ix  , k-1) &
             - zcoef(ip,2,k-1)*zx(ix-l, k  ) &
             - zcoef(ip,3,k-1)*zx(ix-1, k  ) &
             - zcoef(ip,5,k-1)*zx(ix+1, k  ) &
             - zcoef(ip,6,k-1)*zx(ix+l, k  ) &
             - zcoef(ip,7,k-1)*zx(ix  , k+1) ) &
             / zcoef(ip,4,k-1)
        zx(ix,k) =  s1omg*zx(ix,k) + omega*xtmp

     end do

  end do

  ! sendrecv for x(*, kp)
  !$xmp reflect(zx)

  !$xmp loop on t(k)
  do k = 3, n+1, 2 ! start is needed to be an even number

     do ip = 2, lm, 2

        ix      =  ip + l
        xtmp    = ( zb(ip,k-1) - zcoef(ip,1,k-1)*zx(ix  , k-1) &
             - zcoef(ip,2,k-1)*zx(ix-l, k  ) &
             - zcoef(ip,3,k-1)*zx(ix-1, k  ) &
             - zcoef(ip,5,k-1)*zx(ix+1, k  ) &
             - zcoef(ip,6,k-1)*zx(ix+l, k  ) &
             - zcoef(ip,7,k-1)*zx(ix  , k+1) ) &
             / zcoef(ip,4,k-1)
        zx(ix,k) =  s1omg*zx(ix,k) + omega*xtmp

     end do

     do ip = 1, lm, 2

        ix      =  ip + l
        xtmp    = ( zb(ip,k-1) - zcoef(ip,1,k-1)*zx(ix  , k-1) &
             - zcoef(ip,2,k-1)*zx(ix-l, k  ) &
             - zcoef(ip,3,k-1)*zx(ix-1, k  ) &
             - zcoef(ip,5,k-1)*zx(ix+1, k  ) &
             - zcoef(ip,6,k-1)*zx(ix+l, k  ) &
             - zcoef(ip,7,k-1)*zx(ix  , k+1) ) &
             / zcoef(ip,4,k-1)
        zx(ix,k) =  s1omg*zx(ix,k) + omega*xtmp

     end do

  end do

  ! sendrecv for zx(*, kp)
  !$xmp reflect(zx)

  res  =  0.0d0

  !$xmp loop on t(k) reduction(max:res)
  do k = 2, n+1

     ! kp  =  k + 1

     do ip = 1, lm

        ix    =  ip + l
        rtmp  =  zb(ip,k-1) - zcoef(ip,1,k-1)*zx(ix  , k-1)   &
             - zcoef(ip,2,k-1)*zx(ix-l, k  )   &
             - zcoef(ip,3,k-1)*zx(ix-1, k  )   &
             - zcoef(ip,4,k-1)*zx(ix  , k  )   &
             - zcoef(ip,5,k-1)*zx(ix+1, k  )   &
             - zcoef(ip,6,k-1)*zx(ix+l, k  )   &
             - zcoef(ip,7,k-1)*zx(ix  , k+1)
        res   =  max( res, abs(rtmp) )

     end do
  end do

  res = res / bmax

  ! ===( debug write )======================
  !        if( mod(iter,10) .eq. 0 )  then
  !           if(myrank == 0) write(6,6000)  iter, res
  !        end if


  if( (res .gt. eps) .and. (iter .le. maxitr) )  go to 10

  if(myrank == 1)  write(6,6000)  iter, res

6000 format(8x,'== SOR4C ==  ',i5,5x,e15.6)

  return

end subroutine lsor4c
