subroutine lsor4c
  use cmmod
  implicit none

  integer :: k, ip, kp, ix, iter
  real(8) :: bmax, res, rtmp, xtmp
  real(8) :: time0, time1, time2, time3, time4
  real(8) :: time5, time6, time7, time8
  real(8) :: xmp_wtime

  !$xmp barrier
  time0 = xmp_wtime()

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

     ! kp  =  k + 1
     ! write(*, *) "res: k = ", k, "myrank = ", myrank

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

  !$xmp barrier
  time2 = xmp_wtime()

  ! --- ( iteration phase ) --------------------------------------------
  iter  =  0

10 continue ! label

  !$xmp barrier
  time3 = time3 + xmp_wtime()

  iter  =  iter + 1

  ! data has been received in the previous line of 'res = 0.0d0'

  !$xmp loop on t(k)
  do k = 2, n+1, 2 ! start is needed to be an odd number.

     ! kp  =  k + 1
     ! write(*, *) "iter: k = ", k, "iter = ", iter, "myrank = ", myrank

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

  !$xmp barrier
  time4 = time4 + xmp_wtime()

  ! sendrecv for x(*, kp)
  !$xmp reflect(zx)

  !$xmp barrier
  time5 = time5 + xmp_wtime()

  !$xmp loop on t(k)
  do k = 3, n+1, 2 ! start is needed to be an even number

     ! kp  =  k + 1
     ! write(*, *) "iter: k = ", k, "iter = ", iter, "myrank = ", myrank

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

  !$xmp barrier
  time6 = time6 + xmp_wtime()

  ! sendrecv for zx(*, kp)
  !$xmp reflect(zx)

  !$xmp barrier
  time7 = time7 + xmp_wtime()

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

  !$xmp barrier
  time8 = time8 + xmp_wtime()

  res = res / bmax

  ! ===( debug write )======================
  !        if( mod(iter,10) .eq. 0 )  then
  !           if(myrank == 0) write(6,6000)  iter, res
  !        end if


  if( (res .gt. eps) .and. (iter .le. maxitr) )  go to 10

  if(myrank == 1)  write(6,6000)  iter, res

  !$xmp barrier
  time1 = xmp_wtime()

  if(myrank == 1) then
     write(*, *) "even board updating sum = ", time4-time3
     write(*, *) "odd board updating sum = ", time6-time5
     write(*, *) "res updating sum = ", time8-time7
     write(*, *) "reflecting sum = ", (time5-time4)+(time7-time6)
     write(*, *)
     write(*, *) "before iteration = ", time2-time0     
     write(*, *) "iteration = ", time1-time2
     write(*, *) "all executing time = ", time1-time0
  end if

6000 format(8x,'== SOR4C ==  ',i5,5x,e15.6)

  return

end subroutine lsor4c
