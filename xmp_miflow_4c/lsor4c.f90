subroutine  lsor4c( l, lm, n, eps, maxitr, coef, b, x, omega, s1omg, myrank, nprocs )
  implicit none

  integer :: l, lm, n, maxitr
  integer :: lm2
  real(8) :: eps, omega, s1omg
  real(8) :: coef(lm,7,n),  b(lm,n),  x(lm+2*l,n+2)
  
  integer :: k, ip, kp, ix, iter
  real(8) :: bmax_local, bmax, res_local, res, rtmp, xtmp

  ! Variables for XMP
  integer :: myrank, nprocs
  integer :: dist(4) = (/8,8,8,10/)
  
  ! XMP directives
  !$xmp nodes n(4)
  !$xmp template t(34)
  !$xmp distribute t(gblock(dist)) onto n

  !$xmp align (*,*,k) with t(k) :: coef
  !$xmp align (*,j) with t(j) :: b, x

  !$xmp shadow x(*,2)

  lm2 = lm+2*l ! いる？

  ! start calculation

  bmax  =  0.0d0
  bmax_local = 0.0d0
  do k = 1, n
     do ip = 1, lm
        bmax_local  =  max( bmax_local, abs( b(ip,k) ) )
     end do
  end do

  ! reduction for bmax
  call mpi_allreduce(bmax_local, bmax, 1, MPI_REAL8, MPI_MAX, MPI_COMM_WORLD, ierr)

  ! sendrecv for x(*, kp+1)( = x(*, k+2) ) columns
  call mpi_sendrecv(x(1, nstart), lm2*2, MPI_REAL8, leftnode, 100, &
       x(1, nend+1), lm2*2, MPI_REAL8, rightnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  res  =  0.0d0
  res_local = 0.0d0
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
        res_local   =  max( res_local, abs(rtmp) )

     end do
  end do

  ! reduction for res
  call mpi_allreduce(res_local, res, 1, MPI_REAL8, MPI_MAX, MPI_COMM_WORLD, ierr)

  if( bmax .ne. 0.0 )   res = res / bmax

  if( res .lt. eps )  then

     iter = 0
     if(myrank == 0) write(6,6000) iter, res
     return

  end if


  ! --- ( iteration phase ) --------------------------------------------
  iter  =  0

10 continue ! label

  iter  =  iter + 1


  if(mod(nstart,2) /= 1) write(*, *) "an error occured."

  ! data has been received in the previous line of 'res = 0.0d0'
  do k = 1, n, 2 ! nstart is needed to be an odd number.
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

  ! sendrecv for x(*, kp)
  ! when nstart is an odd number, x(*, nstart+1) has been computed
  ! (because kp = k + 1)
  call mpi_sendrecv(x(1, nstart+1), lm2, MPI_REAL8, leftnode, 100, &
       x(1, nend+2), lm2, MPI_REAL8, rightnode, 100, &
       MPI_COMM_WORLD, istat, ierr)  

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

  ! sendrecv for x(*, kp)
  ! x(*, kp+1) has received and x(*, nend+1) has been updated in this node
  call mpi_sendrecv(x(1, nend+1), lm2, MPI_REAL8, rightnode, 100, &
       x(1, nstart), lm2, MPI_REAL8, leftnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  res  =  0.0d0
  res_local = 0.0d0
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
        res_local   =  max( res_local, abs(rtmp) )

     end do
  end do


  ! reduction for res
  call mpi_allreduce(res_local, res, 1, MPI_REAL8, MPI_MAX, MPI_COMM_WORLD, ierr);


  res = res / bmax


  ! ===( debug write )======================
  !        if( mod(iter,10) .eq. 0 )  then
  !           if(myrank == 0) write(6,6000)  iter, res
  !        end if


  if( (res .gt. eps) .and. (iter .le. maxitr) )  go to 10

  if(myrank == 0)  write(6,6000)  iter, res

6000 format(8x,'== SOR4C ==  ',i5,5x,e15.6)

  return

end subroutine lsor4c
