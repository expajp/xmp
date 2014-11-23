subroutine  calcw1

  use cmmod
  implicit none

  integer :: i, j, k
  real(8) :: utmp, vtmp, wtmp


  ! ===( Convection term )================================================

  ! -----( upwind )-------------------------------------------------------

  do k = 2, n
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

  do k = 2, n
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

  do k = 1, n
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

  do k = 2, n
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
  do k = 2, n
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
