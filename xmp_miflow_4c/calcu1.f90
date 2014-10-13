subroutine  calcu1

  use cmmod
  implicit none

  integer :: i, j, k
  real(8) :: utmp, vtmp, wtmp

  ! ===( Convection term )================================================

  ! -----( upwind )-------------------------------------------------------

  !$xmp loop on t(k)
  do k = 2, n1
     do j = 2, m1
        do i = 1, l

           utmp  =  u(i,j,k) + u(i+1,j  ,k  )
           if( utmp .ge. 0.0d0 )  then
              wk1(i,j,k) = cdt2dx*utmp*u(i  ,j  ,k  )
           else
              wk1(i,j,k) = cdt2dx*utmp*u(i+1,j  ,k  )
           end if

        end do
     end do
  end do

  !$xmp loop on t(k)
  do k = 2, n1
     do j = 1, m1
        do i = 2, l

           vtmp  =  v(i,j,k) + v(i+1,j  ,k  )
           if( vtmp .ge. 0.0d0 )  then
              wk2(i,j,k) = cdt2dy*vtmp*u(i  ,j  ,k  )
           else
              wk2(i,j,k) = cdt2dy*vtmp*u(i  ,j+1,k  )
           end if

        end do
     end do
  end do

  ! sync for u(*, *, k+1), u(*, *, k-1)
  !$xmp reflect (u)

 
  !$xmp loop on t(k)
  do k = 1, n1
     do j = 2, m1
        do i = 2, l

           wtmp  =  w(i,j,k) + w(i+1,j  ,k  )
           if( wtmp .ge. 0.0d0 )  then
              wk3(i,j,k) = cdt2dz*wtmp*u(i  ,j  ,k  )
           else
              wk3(i,j,k) = cdt2dz*wtmp*u(i  ,j  ,k+1)
           end if

        end do
     end do
  end do

  ! ===( Diffusion term )=================================================

  !$xmp loop on t(k)
  do k = 2, n1
     do j = 2, m1
        do i = 2, l
           dfs(i,j,k) =  dfxore*(  u(i+1,j  ,k  ) - 2.0d0*u(i,j,k) &
                + u(i-1,j  ,k  )                  ) &
                + dfyore*(  u(i  ,j+1,k  ) - 2.0d0*u(i,j,k) &
                + u(i  ,j-1,k  )                  ) &
                + dfzore*(  u(i  ,j  ,k+1) - 2.0d0*u(i,j,k) &
                + u(i  ,j  ,k-1)                  )
        end do
     end do
  end do


  ! ===( the first step )=================================================

  ! sync for wk3(*, *, k-1)
  !$xmp reflect (wk3)

  !$xmp loop on t(k)
  do k = 2, n1
     do j = 2, m1
        do i = 2, l
           u1(i,j,k)  =  u(i,j,k) + dfs(i,j,k) &
                + wk1(i,j,k) - wk1(i-1,j  ,k  ) &
                + wk2(i,j,k) - wk2(i  ,j-1,k  ) &
                + wk3(i,j,k) - wk3(i  ,j  ,k-1)
        end do
     end do
  end do

  return

end subroutine calcu1

