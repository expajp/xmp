subroutine  lsor4c( l, lm, n, eps, maxitr, coef, b, x, omega, s1omg, myrank, nprocs)
  use mpi
  implicit none

  integer :: l, lm, n, maxitr
  integer :: lm2
  real(8) :: eps, omega, s1omg
  real(8) :: coef(lm,7,n),  b(lm,n),  x(lm+2*l,n+2)
  
  integer :: k, ip, kp, ix, iter
  real(8) :: bmax_local, bmax, res_local, res, rtmp, xtmp

  ! for time split
  real(8) :: time0, time1, time2, time3, time4
  real(8) :: time5, time6, time7, time8, time9
  real(8) :: time10, time11
  real(8) :: calc, comm

  ! Variables for MPI
  integer :: ierr
  integer :: myrank, nprocs
  integer :: leftnode, rightnode
  integer, dimension(MPI_STATUS_SIZE) :: istat
  
  integer :: nstart, nend
  integer :: npstart, npend

    ! initialize Variables for parallel
  if(myrank == 0) then
     leftnode = MPI_PROC_NULL
  else
     leftnode = myrank-1
  end if

  if(myrank == nprocs-1) then
     rightnode = MPI_PROC_NULL
  else
     rightnode = myrank+1
  end if

  lm2 = lm+2*l

  ! set distribution
  if(myrank == 0) then
     nstart = 1
  else
     nstart = (myrank * n / nprocs) + 2
  end if

  if(myrank == nprocs-1) then
     nend = n
  else
     nend = (myrank+1) * n / nprocs + 1
  end if

  ! "p"
  npstart = (myrank * n / nprocs) + 1
  npend = (myrank+1) * n / nprocs

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
  call mpi_barrier(MPI_COMM_WORLD, ierr)
  time0 = MPI_Wtime()

  bmax  =  0.0d0
  bmax_local = 0.0d0
  do k = nstart, nend
     do ip = 1, lm
        bmax_local  =  max( bmax_local, abs( b(ip,k) ) )
     end do
  end do

  call mpi_barrier(MPI_COMM_WORLD, ierr)
  time1 = MPI_Wtime()

  ! reduction for bmax
  call mpi_allreduce(bmax_local, bmax, 1, MPI_REAL8, MPI_MAX, MPI_COMM_WORLD, ierr)

  ! sendrecv for x(*, kp-1) columns
  call mpi_sendrecv(x(1, nend), lm2, MPI_REAL8, rightnode, 100, &
       x(1, nstart-1), lm2, MPI_REAL8, leftnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  ! sendrecv for x(*, kp+1) columns
  call mpi_sendrecv(x(1, nstart), lm2, MPI_REAL8, leftnode, 100, &
       x(1, nend+1), lm2, MPI_REAL8, rightnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  ! sendrecv for b(*, k) columns
  call mpi_sendrecv(b(1, nend), lm, MPI_REAL8, rightnode, 100, &
       b(1, nstart-1), lm, MPI_REAL8, leftnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  ! sendrecv for coef(*, *, k) columns
  call mpi_sendrecv(coef(1, 1, nend), lm*7, MPI_REAL8, rightnode, 100, &
       coef(1, 1, nstart-1), lm*7, MPI_REAL8, leftnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  call mpi_barrier(MPI_COMM_WORLD, ierr)
  time2 = MPI_Wtime()

  res  =  0.0d0
  res_local = 0.0d0
  do k = npstart, npend

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

  !call mpi_barrier(MPI_COMM_WORLD, ierr)
  !time3 = MPI_Wtime()

  ! reduction for res
  call mpi_allreduce(res_local, res, 1, MPI_REAL8, MPI_MAX, MPI_COMM_WORLD, ierr)

  call mpi_barrier(MPI_COMM_WORLD, ierr)
  time4 = MPI_Wtime()

  if( bmax .ne. 0.0 )   res = res / bmax

  if( res .lt. eps ) then

     iter = 0
     if(myrank == 0) write(6,6000) iter, res

     return

  end if


  ! --- ( iteration phase ) --------------------------------------------
  iter  =  0

10 continue ! label

  call mpi_barrier(MPI_COMM_WORLD, ierr)
  time5 = time5 + MPI_Wtime()

  iter  =  iter + 1

  ! data has been received in the previous lines of 'res = 0.0d0'
  do k = npstart, npend, 2 ! npstart is needed to be an odd number.
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

  call mpi_barrier(MPI_COMM_WORLD, ierr)
  time6 = time6 + MPI_Wtime()

  ! sendrecv for x(*, kp+1) columns
  call mpi_sendrecv(x(1, nstart), lm2, MPI_REAL8, leftnode, 100, &
       x(1, nend+1), lm2, MPI_REAL8, rightnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  call mpi_barrier(MPI_COMM_WORLD, ierr)
  time7 = time7 + MPI_Wtime()

  do k = npstart+1, npend, 2

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

  call mpi_barrier(MPI_COMM_WORLD, ierr)
  time8 = time8 + MPI_Wtime()

  ! sendrecv for x(*, kp-1) columns
  call mpi_sendrecv(x(1, nend), lm2, MPI_REAL8, rightnode, 100, &
       x(1, nstart-1), lm2, MPI_REAL8, leftnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  call mpi_barrier(MPI_COMM_WORLD, ierr)
  time9 = time9 + MPI_Wtime()

  res  =  0.0d0
  res_local = 0.0d0
  do k = npstart, npend

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

  !call mpi_barrier(MPI_COMM_WORLD, ierr)
  !time10 = time10 + MPI_Wtime()

  ! reduction for res
  call mpi_allreduce(res_local, res, 1, MPI_REAL8, MPI_MAX, MPI_COMM_WORLD, ierr);

  call mpi_barrier(MPI_COMM_WORLD, ierr)
  time11 = time11 + MPI_Wtime()

  res = res / bmax

  ! ===( debug write )======================
  !        if( mod(iter,10) .eq. 0 )  then
  !           if(myrank == 0) write(6,6000)  iter, res
  !        end if

  if( (res .gt. eps) .and. (iter .le. maxitr) )  go to 10

!  if(myrank == 0)  write(6,6000)  iter, res

  if(myrank == 0) then
     calc = (time6-time5)+(time8-time7)+(time11-time9)! xmpと合わせるため、リダクションの通信を含む
     comm = (time7-time6)+(time9-time8)
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
