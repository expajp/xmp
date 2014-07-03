      subroutine  calcw1

      use cmmod
      

! ===( Convection term )================================================
      
! -----( upwind )-------------------------------------------------------

       do  100  k = 2, n
         do  100  j = 2, m1
           do  100  i = 1, l1
             utmp  =  u(i,j,k) + u(i  ,j  ,k+1)
             if( utmp .ge. 0.0d0 )  then
                wk1(i,j,k) = cdt2dx*utmp*w(i  ,j  ,k  )
             else
                wk1(i,j,k) = cdt2dx*utmp*w(i+1,j  ,k  )
             endif
 100   continue
   
       do  110  k = 2, n
         do  110  j = 1, m1
           do  110  i = 2, l1
             vtmp  =  v(i,j,k) + v(i  ,j  ,k+1)
             if( vtmp .ge. 0.0d0 )  then
                wk2(i,j,k) = cdt2dy*vtmp*w(i  ,j  ,k  )
             else
                wk2(i,j,k) = cdt2dy*vtmp*w(i  ,j+1,k  )
             endif
 110   continue
   
       do  120  k = 1, n
         do  120  j = 2, m1
           do  120  i = 2, l1
             wtmp  =  w(i,j,k) + w(i  ,j  ,k+1)
             if( wtmp .ge. 0.0d0 )  then
                wk3(i,j,k) = cdt2dz*wtmp*w(i  ,j  ,k  )
             else
                wk3(i,j,k) = cdt2dz*wtmp*w(i  ,j  ,k+1)
             endif
 120   continue
         

! ===( Diffusion term )=================================================
            
      do  500  k = 2, n
        do  500  j = 2, m1
          do  500  i = 2, l1
            dfs(i,j,k) =  dfxore*(  w(i+1,j  ,k  ) - 2.0d0*w(i,j,k) &
                            + w(i-1,j  ,k  )                  ) &
                  + dfyore*(  w(i  ,j+1,k  ) - 2.0d0*w(i,j,k) &
                            + w(i  ,j-1,k  )                  ) &
                  + dfzore*(  w(i  ,j  ,k+1) - 2.0d0*w(i,j,k) &
                            + w(i  ,j  ,k-1)                  )
 500  continue


! ===( the first step )=================================================
      do  600  k = 2, n
        do  600  j = 2, m1
          do  600  i = 2, l1
            w1(i,j,k)  =  w(i,j,k) + dfs(i,j,k) &
                             + wk1(i,j,k) - wk1(i-1,j  ,k  ) &
                             + wk2(i,j,k) - wk2(i  ,j-1,k  ) &
                             + wk3(i,j,k) - wk3(i  ,j  ,k-1)
 600  continue


      return
      end
