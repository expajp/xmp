subroutine  lsor4c( l, lm, n, eps, maxitr, coef, b, x, omega, s1omg, &
     myrank, nprocs, loopcount, caltime, comtime )
  use mpi
  implicit none

  integer :: l, lm, n, maxitr
  integer :: lm2
  real(8) :: eps, omega, s1omg
  real(8) :: coef(lm,7,n),  b(lm,n),  x(lm+2*l,n+2)
  
  integer :: k, ip, kp, ix, iter
  real(8) :: bmax_local, bmax, res_local, res, rtmp, xtmp

  ! Variables for MPI
  integer :: ierr
  integer :: myrank, nprocs
  integer :: leftnode, rightnode
  integer, dimension(MPI_STATUS_SIZE) :: istat
  
  integer :: nstart, nend

  integer :: loopcount
  real(8) :: caltime, comtime
  real(8) :: cpu0, cpu1, cpu2, cpu3, cpu4, cpu5
  real(8) :: cpu6, cpu7, cpu8, cpu9, cpu10, cpu11


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
  nstart = ( n / nprocs ) * myrank + 1

  if(myrank .ne. nprocs-1) then
     nend = ( n / nprocs ) * (myrank+1)
  else 
     nend = n
  end if

  ! start calculation

  !--- clock start ---!
  call mpi_barrier(MPI_COMM_WORLD, ierr)
  cpu0 = MPI_Wtime()
  !--- clock start ---!

  bmax  =  0.0d0
  bmax_local = 0.0d0
  do k = nstart, nend
     do ip = 1, lm
        bmax_local  =  max( bmax_local, abs( b(ip,k) ) )
     end do
  end do

  !--- clock split ---!
  call mpi_barrier(MPI_COMM_WORLD, ierr)
  cpu1 = MPI_Wtime()
  !--- clock split ---!

  ! reduction for bmax
  call mpi_allreduce(bmax_local, bmax, 1, MPI_REAL8, MPI_MAX, MPI_COMM_WORLD, ierr)

  ! sendrecv for x(*, kp+1)( = x(*, k+2) ) columns
  call mpi_sendrecv(x(1, nstart), lm2*2, MPI_REAL8, leftnode, 100, &
       x(1, nend+1), lm2*2, MPI_REAL8, rightnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  !--- clock split ---!
  call mpi_barrier(MPI_COMM_WORLD, ierr)
  cpu2 = MPI_Wtime()
  !--- clock split ---!

  res  =  0.0d0
  res_local = 0.0d0
  do k = nstart, nend

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

  !--- clock split ---!
  call mpi_barrier(MPI_COMM_WORLD, ierr)
  cpu3 = MPI_Wtime()
  !--- clock split ---!

  ! reduction for res
  call mpi_allreduce(res_local, res, 1, MPI_REAL8, MPI_MAX, MPI_COMM_WORLD, ierr)

  !--- clock split ---!
  call mpi_barrier(MPI_COMM_WORLD, ierr)
  cpu4 = MPI_Wtime()
  !--- clock split ---!

  caltime = caltime + (cpu1-cpu0) + (cpu3-cpu2)
  comtime = comtime + (cpu2-cpu1) + (cpu4-cpu3)

  if( bmax .ne. 0.0 )   res = res / bmax

  if( res .lt. eps )  then

     iter = 0

     if(myrank == 0) then
        call mpi_bcast(caltime, 1, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
        call mpi_bcast(comtime, 1, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
        write(6,6000) iter, res
     end if

     return

  end if


  ! --- ( iteration phase ) --------------------------------------------
  iter  =  0

10 continue ! label

  !--- clock split ---!
  call mpi_barrier(MPI_COMM_WORLD, ierr)
  cpu5 = MPI_Wtime()
  !--- clock split ---!

  iter  =  iter + 1

  ! data has been received in the previous line of 'res = 0.0d0'
  do k = nstart, nend, 2 ! nstart is needed to be an odd number.
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

  !--- clock split ---!
  call mpi_barrier(MPI_COMM_WORLD, ierr)
  cpu6 = MPI_Wtime()
  !--- clock split ---!

  ! sendrecv for x(*, kp)
  ! when nstart is an odd number, x(*, nstart+1) has been computed
  ! (because kp = k + 1)
  call mpi_sendrecv(x(1, nstart+1), lm2, MPI_REAL8, leftnode, 100, &
       x(1, nend+2), lm2, MPI_REAL8, rightnode, 100, &
       MPI_COMM_WORLD, istat, ierr)  

  !--- clock split ---!
  call mpi_barrier(MPI_COMM_WORLD, ierr)
  cpu7 = MPI_Wtime()
  !--- clock split ---!

  do k = nstart+1, nend, 2

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

  !--- clock split ---!
  call mpi_barrier(MPI_COMM_WORLD, ierr)
  cpu8 = MPI_Wtime()
  !--- clock split ---!

  ! sendrecv for x(*, kp)
  ! x(*, kp+1) has received and x(*, nend+1) has been updated in this node
  call mpi_sendrecv(x(1, nend+1), lm2, MPI_REAL8, rightnode, 100, &
       x(1, nstart), lm2, MPI_REAL8, leftnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  !--- clock split ---!
  call mpi_barrier(MPI_COMM_WORLD, ierr)
  cpu9 = MPI_Wtime()
  !--- clock split ---!

  res  =  0.0d0
  res_local = 0.0d0
  do k = nstart, nend

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

  !--- clock split ---!
  call mpi_barrier(MPI_COMM_WORLD, ierr)
  cpu10 = MPI_Wtime()
  !--- clock split ---!

  ! reduction for res
  call mpi_allreduce(res_local, res, 1, MPI_REAL8, MPI_MAX, MPI_COMM_WORLD, ierr);

  !--- clock split ---!
  call mpi_barrier(MPI_COMM_WORLD, ierr)
  cpu11 = MPI_Wtime()
  !--- clock split ---!

  res = res / bmax


  ! ===( debug write )======================
  !        if( mod(iter,10) .eq. 0 )  then
  !           if(myrank == 0) write(6,6000)  iter, res
  !        end if

  ! add time
  caltime = caltime + (cpu6-cpu5) + (cpu8-cpu7) + (cpu10-cpu9)
  comtime = comtime + (cpu7-cpu6) + (cpu9-cpu8) + (cpu11-cpu10)

  if( (res .gt. eps) .and. (iter .le. maxitr) )  go to 10

  loopcount = loopcount + iter

  call mpi_bcast(loopcount, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
  call mpi_bcast(caltime, 1, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
  call mpi_bcast(comtime, 1, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)

  if(myrank == 0)  write(6,6000)  iter, res

6000 format(8x,'== SOR4C ==  ',i5,5x,e15.6)

  return

end subroutine lsor4c
