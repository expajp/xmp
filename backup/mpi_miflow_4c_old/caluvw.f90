subroutine  caluvw

  use cmmod
  implicit none

  integer :: i, j, k
  integer :: ip

  ! ---( Make right hand side of system of linear equation )--------------
  

  ! sendrecv for k+1
  call mpi_sendrecv(u1(1, 1, nstart), l1*m2, MPI_REAL8, leftnode, 100, &
       u1(1, 1, nend+1), l1*m2, MPI_REAL8, rightnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  call mpi_sendrecv(v1(1, 1, nstart), l2*m1, MPI_REAL8, leftnode, 100, &
       v1(1, 1, nend+1), l2*m1, MPI_REAL8, rightnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  call mpi_sendrecv(w1(1, 1, nstart), l2*m2, MPI_REAL8, leftnode, 100, &
       w1(1, 1, nend+1), l2*m2, MPI_REAL8, rightnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  
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

  call lsor4c( l, lm, n, eps, maxitr, zcoef, zb, zx, omega, s1omg, myrank, nprocs)

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
