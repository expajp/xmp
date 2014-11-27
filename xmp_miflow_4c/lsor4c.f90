subroutine lsor4c
  use cmmod
  implicit none

  integer :: k, ip, kp, ix, iter
  real(8) :: bmax, res, rtmp, xtmp
  real(8) :: xmp_wtime

  ! for time split
  real(8) :: time0, time1, time2, time3, time4
  real(8) :: time5, time6, time7, time8, time9
  real(8) :: time10, time11
  real(8) :: calc, comm

  ! initialize time
  time0 = 0.0d0
  time1 = 0.0d0
  time2 = 0.0d0
  time3 = 0.0d0
  time4 = 0.0d0
  time5 = 0.0d0
  time6 = 0.0d0
  time7 = 0.0d0
  time8 = 0.0d0
  time9 = 0.0d0
  time10 = 0.0d0
  time11 = 0.0d0

  ! start calculation
  !$xmp barrier
  time0 = xmp_wtime()

  bmax  =  0.0d0

  !$xmp loop on t(k) reduction(max:bmax)
  do k = 1, n
     do ip = 1, lm
        bmax  =  max( bmax, abs( zb(ip,k) ) )
     end do
  end do

  !$xmp barrier
  time1 = xmp_wtime()

  !$xmp reflect(zcoef)
  !$xmp reflect(zx)
  !$xmp reflect(zb)

  !$xmp barrier
  time2 = xmp_wtime()

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

  !$xmp barrier
  time4 = xmp_wtime()

  if( bmax .ne. 0.0 )   res = res / bmax

  if( res .lt. eps )  then

     iter = 0
     if(myrank == 1) write(6,6000) iter, res
     return

  end if

  ! --- ( iteration phase ) --------------------------------------------
  iter  =  0

10 continue ! label

  !$xmp barrier
  time5 = time5 + xmp_wtime()

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

  !$xmp barrier
  time6 = time6 + xmp_wtime()

  ! sendrecv for x(*, kp)
  !$xmp reflect(zx)

  !$xmp barrier
  time7 = time7 + xmp_wtime()

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

  !$xmp barrier
  time8 = time8 + xmp_wtime()

  ! sendrecv for zx(*, kp)
  !$xmp reflect(zx)

  !$xmp barrier
  time9 = time9 + xmp_wtime()

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
  time11 = time11 + xmp_wtime()

  res = res / bmax

  ! ===( debug write )======================
  !        if( mod(iter,10) .eq. 0 )  then
  !           if(myrank == 0) write(6,6000)  iter, res
  !        end if


  if( (res .gt. eps) .and. (iter .le. maxitr) )  go to 10

!  if(myrank == 1)  write(6,6000)  iter, res

  if(myrank == 1) then
     calc = (time6-time5)+(time8-time7)+(time11-time9)
     comm = (time2-time1)+(time7-time6)+(time9-time8)
     write(*, *) "init: ", time4-time0
     write(*, *) "comm: ", comm
     write(*, *) "comm per 1 iteration: ", comm/iter
     write(*, *) "calc: ", calc
     write(*, *) "calc per 1 iteration: ", calc/iter
     write(*, '(i4,3(f10.6))') nprocs, time4-time0, comm, calc/iter
  end if


6000 format(8x,'== SOR4C ==  ',i5,5x,e15.6)

  return

end subroutine lsor4c
