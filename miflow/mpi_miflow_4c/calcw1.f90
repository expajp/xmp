subroutine  calcw1

  use cmmod
  implicit none

  integer :: i, j, k
  real(8) :: utmp, vtmp, wtmp
  real(8), dimension(l2, m2, 3) :: sendbuf, recvbuf


  ! ===( Convection term )================================================

  ! -----( upwind )-------------------------------------------------------

  ! initialize buffers
  sendbuf = 0
  recvbuf = 0

  ! cram data to sendbuf
  if(myrank /= 0) then

     do j = 1, m2
        do i = 1, l1
           sendbuf(i, j, 1) = u(i, j, nstart)
        end do
     end do

     do j = 1, m1
        do i = 1, l2
           sendbuf(i, j, 2) = v(i, j, nstart)
        end do
     end do

     do j = 1, m2
        do i = 1, l2
           sendbuf(i, j, 3) = w(i, j, nstart)
        end do
     end do

  end if

  ! sendrecv for [uvw](*, k+1)
  call mpi_sendrecv(sendbuf, l2*m2*3, MPI_REAL8, leftnode, 100, &
       recvbuf, l2*m2*3, MPI_REAL8, rightnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  ! distribute data from recvbuf
  if(myrank /= nprocs-1) then

     do j = 1, m2
        do i = 1, l1
           u(i, j, nend+1) = recvbuf(i, j, 1)
        end do
     end do

     do j = 1, m1
        do i = 1, l2
           v(i, j, nend+1) = recvbuf(i, j, 2)
        end do
     end do

     do j = 1, m2
        do i = 1, l2
           w(i, j, nend+1) = recvbuf(i, j, 3)
        end do
     end do

  end if

  ! calculate
  do k = nstart2, nend
     do j = 2, m1
        do i = 1, l1

           utmp  =  u(i,j,k) + u(i  ,j  ,k+1)
           if( utmp .ge. 0.0d0 )  then
              wk1(i,j,k) = cdt2dx*utmp*w(i  ,j  ,k  )
           else
              wk1(i,j,k) = cdt2dx*utmp*w(i+1,j  ,k  )
           end if

        end do
     end do
  end do

  do k = nstart2, nend
     do j = 1, m1
        do i = 2, l1

           vtmp  =  v(i,j,k) + v(i  ,j  ,k+1)
           if( vtmp .ge. 0.0d0 )  then
              wk2(i,j,k) = cdt2dy*vtmp*w(i  ,j  ,k  )
           else
              wk2(i,j,k) = cdt2dy*vtmp*w(i  ,j+1,k  )
           end if

        end do
     end do
  end do

  do k = nstart, nend
     do j = 2, m1
        do i = 2, l1

           wtmp  =  w(i,j,k) + w(i  ,j  ,k+1)
           if( wtmp .ge. 0.0d0 )  then
              wk3(i,j,k) = cdt2dz*wtmp*w(i  ,j  ,k  )
           else
              wk3(i,j,k) = cdt2dz*wtmp*w(i  ,j  ,k+1)
           end if

        end do
     end do
  end do


  ! ===( Diffusion term )=================================================

  ! sendrecv for w(*, *, k-1)
  call mpi_sendrecv(w(1, 1, nend), l2*m2, MPI_REAL8, rightnode, 100, &
       w(1, 1, nstart2-1), l2*m2, MPI_REAL8, leftnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  do k = nstart2, nend
     do j = 2, m1
        do i = 2, l1
           dfs(i,j,k) =  dfxore*(  w(i+1,j  ,k  ) - 2.0d0*w(i,j,k) &
                + w(i-1,j  ,k  )                  ) &
                + dfyore*(  w(i  ,j+1,k  ) - 2.0d0*w(i,j,k) &
                + w(i  ,j-1,k  )                  ) &
                + dfzore*(  w(i  ,j  ,k+1) - 2.0d0*w(i,j,k) &
                + w(i  ,j  ,k-1)                  )
        end do
     end do
  end do


  ! ===( the first step )=================================================

  ! sendrecv for wk3(*, *, k-1)
  call mpi_sendrecv(wk3(1, 1, nend), l1*m1, MPI_REAL8, rightnode, 100, &
       wk3(1, 1, nstart2-1), l1*m1, MPI_REAL8, leftnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  do k = nstart2, nend
     do j = 2, m1
        do i = 2, l1
           w1(i,j,k)  =  w(i,j,k) + dfs(i,j,k) &
                + wk1(i,j,k) - wk1(i-1,j  ,k  ) &
                + wk2(i,j,k) - wk2(i  ,j-1,k  ) &
                + wk3(i,j,k) - wk3(i  ,j  ,k-1)
        end do
     end do
  end do

  return

end subroutine calcw1
