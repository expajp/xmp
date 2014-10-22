subroutine  lsor4c( l, lm, n, eps, maxitr, coef, b, x, omega, s1omg, myrank, nprocs )
  implicit none

  integer :: l, lm, n, maxitr
  real(8) :: eps, omega, s1omg
  real(8) :: coef(lm,7,n),  b(lm,n),  x(lm+2*l,n+2)
  
  integer :: k, ip, kp, ix, iter
  real(8) :: bmax, res, rtmp, xtmp

  ! Variables for XMP
  integer :: myrank, nprocs
  !integer :: npstart, npend
  integer :: dist(4) = (/9,8,8,9/)
  
  ! XMP directives
  !$xmp nodes n(4)
  !$xmp template t(34)
  !$xmp distribute t(gblock(dist)) onto n

  !$xmp align (*,*,k) with t(k) :: coef
  !$xmp align (*,j) with t(j) :: b, x

  !$xmp shadow coef(*,*,1)
  !$xmp shadow x(*,1)
  !$xmp shadow b(*,1)

  ! "p"
  !npstart = ((myrank-1) * n / nprocs) + 1
  !npend = myrank * n / nprocs

  ! start calculation

  bmax  =  0.0d0

  !$xmp loop on t(k) reduction(max:bmax)
  do k = 1, n
     ! write(*, *) "bmax: k = ", k, "myrank = ", myrank
     do ip = 1, lm
        bmax  =  max( bmax, abs( b(ip,k) ) )
     end do
  end do

  !$xmp reflect(coef)
  !$xmp reflect(x)
  !$xmp reflect(b)

  res  =  0.0d0

  !$xmp loop on t(k) reduction(max:res)
  do k = 2, n+1

     ! kp  =  k + 1
     ! write(*, *) "res: k = ", k, "myrank = ", myrank

     do ip = 1, lm
        ix    =  ip + l
        rtmp  =  b(ip,k-1) - coef(ip,1,k-1)*x(ix  , k-1)   &
             - coef(ip,2,k-1)*x(ix-l, k  )   &
             - coef(ip,3,k-1)*x(ix-1, k  )   &
             - coef(ip,4,k-1)*x(ix  , k  )   &
             - coef(ip,5,k-1)*x(ix+1, k  )   &
             - coef(ip,6,k-1)*x(ix+l, k  )   &
             - coef(ip,7,k-1)*x(ix  , k+1)
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

     ! kp  =  k + 1
     ! write(*, *) "iter: k = ", k, "iter = ", iter, "myrank = ", myrank

     do ip = 1, lm, 2

        ix      =  ip + l
        xtmp    = ( b(ip,k-1) - coef(ip,1,k-1)*x(ix  , k-1) &
             - coef(ip,2,k-1)*x(ix-l, k  ) &
             - coef(ip,3,k-1)*x(ix-1, k  ) &
             - coef(ip,5,k-1)*x(ix+1, k  ) &
             - coef(ip,6,k-1)*x(ix+l, k  ) &
             - coef(ip,7,k-1)*x(ix  , k+1) ) &
             / coef(ip,4,k-1)
        x(ix,k) =  s1omg*x(ix,k) + omega*xtmp

     end do

     do ip = 2, lm, 2

        ix      =  ip + l
        xtmp    = ( b(ip,k-1) - coef(ip,1,k-1)*x(ix  , k-1) &
             - coef(ip,2,k-1)*x(ix-l, k  ) &
             - coef(ip,3,k-1)*x(ix-1, k  ) &
             - coef(ip,5,k-1)*x(ix+1, k  ) &
             - coef(ip,6,k-1)*x(ix+l, k  ) &
             - coef(ip,7,k-1)*x(ix  , k+1) ) &
             / coef(ip,4,k-1)
        x(ix,k) =  s1omg*x(ix,k) + omega*xtmp

     end do

  end do

  ! sendrecv for x(*, kp)
  !$xmp reflect(x)

  !$xmp loop on t(k)
  do k = 3, n+1, 2 ! start is needed to be an even number

     ! kp  =  k + 1
     ! write(*, *) "iter: k = ", k, "iter = ", iter, "myrank = ", myrank

     do ip = 2, lm, 2

        ix      =  ip + l
        xtmp    = ( b(ip,k-1) - coef(ip,1,k-1)*x(ix  , k-1) &
             - coef(ip,2,k-1)*x(ix-l, k  ) &
             - coef(ip,3,k-1)*x(ix-1, k  ) &
             - coef(ip,5,k-1)*x(ix+1, k  ) &
             - coef(ip,6,k-1)*x(ix+l, k  ) &
             - coef(ip,7,k-1)*x(ix  , k+1) ) &
             / coef(ip,4,k-1)
        x(ix,k) =  s1omg*x(ix,k) + omega*xtmp

     end do

     do ip = 1, lm, 2

        ix      =  ip + l
        xtmp    = ( b(ip,k-1) - coef(ip,1,k-1)*x(ix  , k-1) &
             - coef(ip,2,k-1)*x(ix-l, k  ) &
             - coef(ip,3,k-1)*x(ix-1, k  ) &
             - coef(ip,5,k-1)*x(ix+1, k  ) &
             - coef(ip,6,k-1)*x(ix+l, k  ) &
             - coef(ip,7,k-1)*x(ix  , k+1) ) &
             / coef(ip,4,k-1)
        x(ix,k) =  s1omg*x(ix,k) + omega*xtmp

     end do

  end do

  ! sendrecv for x(*, kp)
  !$xmp reflect(x)

  res  =  0.0d0

  !$xmp loop on t(k) reduction(max:res)
  do k = 2, n+1

     ! kp  =  k + 1

     do ip = 1, lm

        ix    =  ip + l
        rtmp  =  b(ip,k-1) - coef(ip,1,k-1)*x(ix  , k-1)   &
             - coef(ip,2,k-1)*x(ix-l, k  )   &
             - coef(ip,3,k-1)*x(ix-1, k  )   &
             - coef(ip,4,k-1)*x(ix  , k  )   &
             - coef(ip,5,k-1)*x(ix+1, k  )   &
             - coef(ip,6,k-1)*x(ix+l, k  )   &
             - coef(ip,7,k-1)*x(ix  , k+1)
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
