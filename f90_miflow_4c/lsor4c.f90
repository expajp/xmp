subroutine  lsor4c( l, lm, n, eps, maxitr, coef, b, x, omega, s1omg )
  implicit none

  integer :: l, lm, n, maxitr
  real(8) :: eps, omega, s1omg
  real(8) :: coef(lm,7,n),  b(lm,n),  x(lm+2*l,n+2)
  
  integer :: k, ip, kp, ix, iter
  real(8) :: bmax, res, rtmp, xtmp

  ! for time split
  real(8) :: time0, time1, time2, time3, time4
  real(8) :: time5, time6, time7, time8, time9
  real(8) :: time10, time11
  real(8) :: calc, comm, buffer

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
  buffer = 0.0d0

  ! start calculation
  call cpu_time(time0)

  bmax  =  0.0d0

  do k = 1, n
     do ip = 1, lm
        bmax  =  max( bmax, abs( b(ip,k) ) )
     end do
  end do

  res  =  0.0d0

  do k = 1, n

     kp  =  k + 1

     do ip = 1, lm
        ix    =  ip + l
        rtmp  =  b(ip,k) - coef(ip,1,k)*x(ix  , kp-1)   &
             - coef(ip,2,k)*x(ix-l, kp  )   &
             - coef(ip,3,k)*x(ix-1, kp  )   &
             - coef(ip,4,k)*x(ix  , kp  )   &
             - coef(ip,5,k)*x(ix+1, kp  )   &
             - coef(ip,6,k)*x(ix+l, kp  )   &
             - coef(ip,7,k)*x(ix  , kp+1)
        res   =  max( res, abs(rtmp) )

     end do
  end do

  call cpu_time(time4)

  if( bmax .ne. 0.0 )   res = res / bmax

  if( res .lt. eps )  then

     iter = 0
     write(6,6000) iter, res
     return

  end if


  ! --- ( iteration phase ) --------------------------------------------
  iter  =  0

10 continue ! label

  call cpu_time(buffer)
  time5 = time5 + buffer

  iter  =  iter + 1

  do k = 1, n, 2
     kp  =  k + 1

     do ip = 1, lm, 2

        ix      =  ip + l
        xtmp    = ( b(ip,k) - coef(ip,1,k)*x(ix  , kp-1) &
             - coef(ip,2,k)*x(ix-l, kp  ) &
             - coef(ip,3,k)*x(ix-1, kp  ) &
             - coef(ip,5,k)*x(ix+1, kp  ) &
             - coef(ip,6,k)*x(ix+l, kp  ) &
             - coef(ip,7,k)*x(ix  , kp+1) ) &
             / coef(ip,4,k)
        x(ix,kp) =  s1omg*x(ix,kp) + omega*xtmp

     end do

     do ip = 2, lm, 2

        ix      =  ip + l
        xtmp    = ( b(ip,k) - coef(ip,1,k)*x(ix  , kp-1) &
             - coef(ip,2,k)*x(ix-l, kp  ) &
             - coef(ip,3,k)*x(ix-1, kp  ) &
             - coef(ip,5,k)*x(ix+1, kp  ) &
             - coef(ip,6,k)*x(ix+l, kp  ) &
             - coef(ip,7,k)*x(ix  , kp+1) ) &
             / coef(ip,4,k)
        x(ix,kp) =  s1omg*x(ix,kp) + omega*xtmp

     end do

  end do


  do k = 2, n, 2

     kp  =  k + 1

     do ip = 2, lm, 2

        ix      =  ip + l
        xtmp    = ( b(ip,k) - coef(ip,1,k)*x(ix  , kp-1) &
             - coef(ip,2,k)*x(ix-l, kp  ) &
             - coef(ip,3,k)*x(ix-1, kp  ) &
             - coef(ip,5,k)*x(ix+1, kp  ) &
             - coef(ip,6,k)*x(ix+l, kp  ) &
             - coef(ip,7,k)*x(ix  , kp+1) ) &
             / coef(ip,4,k)
        x(ix,kp) =  s1omg*x(ix,kp) + omega*xtmp

     end do

     do ip = 1, lm, 2

        ix      =  ip + l
        xtmp    = ( b(ip,k) - coef(ip,1,k)*x(ix  , kp-1) &
             - coef(ip,2,k)*x(ix-l, kp  ) &
             - coef(ip,3,k)*x(ix-1, kp  ) &
             - coef(ip,5,k)*x(ix+1, kp  ) &
             - coef(ip,6,k)*x(ix+l, kp  ) &
             - coef(ip,7,k)*x(ix  , kp+1) ) &
             / coef(ip,4,k)
        x(ix,kp) =  s1omg*x(ix,kp) + omega*xtmp

     end do

  end do

  res  =  0.0d0

  do k = 1, n

     kp  =  k + 1

     do ip = 1, lm

        ix    =  ip + l
        rtmp  =  b(ip,k) - coef(ip,1,k)*x(ix  , kp-1)   &
             - coef(ip,2,k)*x(ix-l, kp  )   &
             - coef(ip,3,k)*x(ix-1, kp  )   &
             - coef(ip,4,k)*x(ix  , kp  )   &
             - coef(ip,5,k)*x(ix+1, kp  )   &
             - coef(ip,6,k)*x(ix+l, kp  )   &
             - coef(ip,7,k)*x(ix  , kp+1)
        res   =  max( res, abs(rtmp) )

     end do
  end do

  call cpu_time(buffer)
  time11 = time11 + buffer

  res = res / bmax


  ! ===( debug write )======================
  !        if( mod(iter,10) .eq. 0 )  then
  !           write(6,6000)  iter, res
  !        endif


  if( (res .gt. eps) .and. (iter .le. maxitr) )  go to 10

  calc = time11-time5
  write(*, *) "time0: ", time0
  write(*, *) "time4: ", time4
  write(*, *) "init: ", time4-time0
  write(*, *) "comm: ", 0.0d0
  write(*, *) "comm per 1 iteration: ", 0.0d0
  write(*, *) "calc: ", calc
  write(*, *) "calc per 1 iteration: ", calc/iter
  write(*, '(i4,3(f10.6))') 1, time4-time0, 0.0d0, calc/iter


!  write(6,6000)  iter, res

6000 format(8x,'== SOR4C ==  ',i5,5x,e15.6)


  return

end subroutine lsor4c
