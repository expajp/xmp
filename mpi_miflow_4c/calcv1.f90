subroutine  calcv1
      
  use cmmod
  use mpi
  implicit none

  integer :: i, j, k
  real(8) :: utmp, vtmp, wtmp
  integer :: nstart_temp, n1start_temp

! ---( set start_temp )-------------------------------------------------
  if(nstart == 1) then
     nstart_temp = 2
  else
     nstart_temp = nstart
  end if
  
  if(n1start == 1) then
     n1start_temp = 2
  else
     n1start_temp = n1start
  end if

! ===( Convection term )================================================

! -----( upwind )-------------------------------------------------------

  do k = n1start_temp, n1end
     do j = 2, m
        do i = 1, l1
           utmp  =  u(i,j,k) + u(i  ,j+1,k  )
           if( utmp .ge. 0.0d0 )  then
              wk1(i,j,k) = cdt2dx*utmp*v(i  ,j  ,k  )
           else
              wk1(i,j,k) = cdt2dx*utmp*v(i+1,j  ,k  )
           end if
        end do
     end do
  end do
   
  do k = n1start_temp, n1end
     do j = 1, m
        do i = 2, l1
           vtmp  =  v(i,j,k) + v(i  ,j+1,k  )
           if( vtmp .ge. 0.0d0 )  then
              wk2(i,j,k) = cdt2dy*vtmp*v(i  ,j  ,k  )
           else
              wk2(i,j,k) = cdt2dy*vtmp*v(i  ,j+1,k  )
           end if
        end do
     end do
  end do
   
  call mpi_sendrecv(v(1,1,n1start), l2*m1, MPI_REAL, leftnode, 100, &
       v(1,1,n1end+1), l2*m1, MPI_REAL, rightnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  do k = n1start, n1end
     do j = 2, m
        do i = 2, l1
           wtmp  =  w(i,j,k) + w(i  ,j+1,k  )
           if( wtmp .ge. 0.0d0 )  then
              wk3(i,j,k) = cdt2dz*wtmp*v(i  ,j  ,k  )
           else
              wk3(i,j,k) = cdt2dz*wtmp*v(i  ,j  ,k+1)
           end if
        end do
     end do
  end do
         

! ===( Diffusion term )=================================================

  call mpi_sendrecv(v(1,1,n1end), l2*m1, MPI_REAL, leftnode, 100, &
       v(1,1,n1start_temp-1), l2*m1, MPI_REAL, rightnode, 100, &
       MPI_COMM_WORLD, istat, ierr)

  do k = n1start_temp, n1end
     do j = 2, m
        do i = 2, l1
           dfs(i,j,k) =  dfxore*(  v(i+1,j  ,k  ) - 2.0d0*v(i,j,k) &
                + v(i-1,j  ,k  )                  ) &
                + dfyore*(  v(i  ,j+1,k  ) - 2.0d0*v(i,j,k) &
                + v(i  ,j-1,k  )                  ) &
                + dfzore*(  v(i  ,j  ,k+1) - 2.0d0*v(i,j,k) &
                + v(i  ,j  ,k-1)                  )
        end do
     end do
  end do

! ===( the first step )=================================================

  call mpi_sendrecv(wk3(1,1,n1end), l1*m1, MPI_REAL, leftnode, 100, &
       wk3(1,1,n1start_temp-1), l1*m1, MPI_REAL, rightnode, 100, &
       MPI_COMM_WORLD, istat, ierr)
      
  do k = n1start_temp, n1end
     do j = 2, m
        do i = 2, l1
           v1(i,j,k)  =  v(i,j,k) + dfs(i,j,k) &
                + wk1(i,j,k) - wk1(i-1,j  ,k  ) &
                + wk2(i,j,k) - wk2(i  ,j-1,k  ) &
                + wk3(i,j,k) - wk3(i  ,j  ,k-1)
        end do
     end do
  end do


  return
end subroutine calcv1
            
