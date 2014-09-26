subroutine  caluvw

  use cmmod
  implicit none

  integer :: i, j, k
  integer :: ip
  real(8), dimension(l2, m2, 3) :: sendbuf
  real(8), dimension(l2, m2, 3) :: recvbuf

  ! ---( Make right hand side of system of linear equation )--------------
  
  ! initialize buffers
  sendbuf = 0.0d0
  recvbuf = 0.0d0

  ! cram data to sendbuf
  if(myrank /= 0) then

     do j = 1, m2
        do i = 1, l1
           sendbuf(i, j, 1) = u1(i, j, nstart)
        end do
     end do

     do j = 1, m1
        do i = 1, l2
           sendbuf(i, j, 2) = v1(i, j, nstart)
        end do
     end do

     do j = 1, m2
        do i = 1, l2
           sendbuf(i, j, 3) = w1(i, j, nstart)
        end do
     end do

  end if

  ! sendrecv for k+1
  call mpi_sendrecv(sendbuf, l2*m2*3, MPI_REAL8, leftnode, 100, &
       recvbuf, l2*m2*3, MPI_REAL8, rightnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  ! distribute data from recvbuf
  if(myrank /= nprocs-1) then

     do j = 1, m2
        do i = 1, l1
           u1(i, j, nend+1) = recvbuf(i, j, 1)
        end do
     end do

     do j = 1, m1
        do i = 1, l2
           v1(i, j, nend+1) = recvbuf(i, j, 2)
        end do
     end do

     do j = 1, m2
        do i = 1, l2
           v1(i, j, nend+1) = recvbuf(i, j, 3)
        end do
     end do

  end if
  
  ! calculate
  do k = nstart, nend

     ip  =  0

     do j = 1, m
        do i = 1, l

           ip       =  ip + 1
           zb(ip,k) = odtodx*( - u1(i+1,j+1,k+1) + u1(i  ,j+1,k+1) ) &
                + odtody*( - v1(i+1,j+1,k+1) + v1(i+1,j  ,k+1) ) &
                + odtodz*( - w1(i+1,j+1,k+1) + w1(i+1,j+1,k  ) )

        end do
     end do
  end do

  ! for reference point      
  !         b(lmctr,nctr)  =  0.0d0


  ! ---( Poison solver )--------------------------------------------------

  call  lsor4c( l, lm, n, eps, maxitr, zcoef, zb, zx, omega, s1omg, myrank, nprocs )

  ! ---( put the solution into presure variable "p" )---------------------

  do k = nstart2, n1end

     ip  =  0

     do j = 1, m
        do i = 1, l

           ip              =  ip + 1
           p(i+1,j+1,k)  =  zx(ip+l,k)

        end do
     end do
  end do

  ! ---( Velocity correction )--------------------------------------------

  do k = nstart2, n1end
     do j = 2, m1
        do i = 2, l
           u(i,j,k) = u1(i,j,k) - dtodx*( p(i+1,j  ,k  ) - p(i,j,k) )
        end do
     end do
  end do

  do k = nstart2, n1end
     do j = 2, m
        do i = 2, l1
           v(i,j,k) = v1(i,j,k) - dtody*( p(i  ,j+1,k  ) - p(i,j,k) )
        end do
     end do
  end do

  ! sendrecv for p(*, *, k+1)
  call mpi_sendrecv(p(1, 1, nstart), l2*m2, MPI_REAL8, leftnode, 100, &
       p(1, 1, nend+1), l2*m2, MPI_REAL8, rightnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  do k = nstart2, nend
     do j = 2, m1
        do i = 2, l1
           w(i,j,k) = w1(i,j,k) - dtodz*( p(i  ,j  ,k+1) - p(i,j,k) )
        end do
     end do
  end do

  return

end subroutine caluvw
