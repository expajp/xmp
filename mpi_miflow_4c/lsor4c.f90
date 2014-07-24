subroutine  lsor4c( l, lm, n, eps, maxitr, coef, b, x, omega, s1omg, myrank, nprocs )

  use mpi
  implicit none
  
  integer :: l, lm, n, maxitr, myrank, nprocs
  real(8) :: eps, omega, s1omg
  real(8) :: coef(lm,7,n),  b(lm,n),  x(lm+2*l,n+2)

  integer :: k, ip, kp, ix, iter
  real(8) :: bmax, res, rtmp, xtmp

  ! Variables for MPI
  integer :: ierr
  integer :: leftnode, rightnode
  integer, dimension(MPI_STATUS_SIZE) :: istat
  
  integer :: nstart, nend, nchunk
  
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

  ! for n
  nstart = n * myrank / nprocs + 1
  nend = n * (myrank+1) / nprocs
  
  if(myrank == nprocs-1) then
     nchunk = n / nprocs + mod(n, nprocs)
  else
     nchunk = n / nprocs
  end if


  ! need for reduction
  bmax  =  0.0d0
  do k = nstart, nend
     do ip = 1, lm
        bmax  =  max( bmax, abs( b(ip,k) ) )
     end do
  end do
  
  res  =  0.0d0
  do k = nstart, nend
     kp  =  k + 1
     do  ip = 1, lm
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
  
  
  if( bmax .ne. 0.0 )   res = res / bmax
  
  if( res .lt. eps )  then

     iter = 0

     if(myrank == 0) write(6,6000) iter, res

     return

  end if


! --- ( iteration phase ) --------------------------------------------
  iter  =  0
    
10 continue
    
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
  
  res = res / bmax
  
  ! ===( debug write )======================
  !        if( mod(iter,10) .eq. 0 )  then
  !           write(6,6000)  iter, res
  !        endif
  
  
  if( (res .gt. eps) .and. (iter .le. maxitr) )  go to 10

  if(myrank == 0) then  

     write(6,6000)  iter, res
6000 format(8x,'== SOR4C ==  ',i5,5x,e15.6)

  end if

  return

end subroutine lsor4c
