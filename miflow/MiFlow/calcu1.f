      subroutine  calcu1

      include  "./Include/cmparm"
      include  "./Include/cmvars"
      

c ===( Convection term )================================================

c -----( upwind )-------------------------------------------------------
      if( icschm .eq. 1 )  then

         do  100  k = 2, n1
           do  100  j = 2, m1
             do  100  i = 1, l
               utmp  =  u(i,j,k) + u(i+1,j  ,k  )
               if( utmp .ge. 0.0d0 )  then
                  wk1(i,j,k) = cdt2dx*utmp*u(i  ,j  ,k  )
               else
                  wk1(i,j,k) = cdt2dx*utmp*u(i+1,j  ,k  )
               endif
 100     continue
   
         do  110  k = 2, n1
           do  110  j = 1, m1
             do  110  i = 2, l
               vtmp  =  v(i,j,k) + v(i+1,j  ,k  )
               if( vtmp .ge. 0.0d0 )  then
                  wk2(i,j,k) = cdt2dy*vtmp*u(i  ,j  ,k  )
               else
                  wk2(i,j,k) = cdt2dy*vtmp*u(i  ,j+1,k  )
               endif
 110     continue
   
         do  120  k = 1, n1
           do  120  j = 2, m1
             do  120  i = 2, l
               wtmp  =  w(i,j,k) + w(i+1,j  ,k  )
               if( wtmp .ge. 0.0d0 )  then
                  wk3(i,j,k) = cdt2dz*wtmp*u(i  ,j  ,k  )
               else
                  wk3(i,j,k) = cdt2dz*wtmp*u(i  ,j  ,k+1)
               endif
 120     continue
         
      endif

c -----( central diffrence )--------------------------------------------
      if( icschm .eq. 2 )  then

         do  200  k = 2, n1
           do  200  j = 2, m1
             do  200  i = 1, l
               wk1(i,j,k) = cdt4dx*( u(i,j,k) + u(i+1,j  ,k  ) )
     &                            *( u(i,j,k) + u(i+1,j  ,k  ) )
 200     continue
   
         do  210  k = 2, n1
           do  210  j = 1, m1
             do  210  i = 2, l
               wk2(i,j,k) = cdt4dy*( v(i,j,k) + v(i+1,j  ,k  ) )
     &                            *( u(i,j,k) + u(i  ,j+1,k  ) )
 210     continue
   
         do  220  k = 1, n1
           do  220  j = 2, m1
             do  220  i = 2, l
               wk3(i,j,k) = cdt4dz*( w(i,j,k) + w(i+1,j  ,k  ) )
     &                            *( u(i,j,k) + u(i  ,j  ,k+1) )
 220     continue
         
      endif

      
c ===( Diffusion term )=================================================
            
      do  500  k = 2, n1
        do  500  j = 2, m1
          do  500  i = 2, l
            dfs(i,j,k) =  dfxore*(  u(i+1,j  ,k  ) - 2.0d0*u(i,j,k)
     &                            + u(i-1,j  ,k  )                  )
     &                  + dfyore*(  u(i  ,j+1,k  ) - 2.0d0*u(i,j,k)
     &                            + u(i  ,j-1,k  )                  )
     &                  + dfzore*(  u(i  ,j  ,k+1) - 2.0d0*u(i,j,k)
     &                            + u(i  ,j  ,k-1)                  )
 500  continue


c ===( the first step )=================================================
      do  600  k = 2, n1
        do  600  j = 2, m1
          do  600  i = 2, l
            u1(i,j,k)  =  u(i,j,k) + dfs(i,j,k)
     &                             + wk1(i,j,k) - wk1(i-1,j  ,k  )
     &                             + wk2(i,j,k) - wk2(i  ,j-1,k  )
     &                             + wk3(i,j,k) - wk3(i  ,j  ,k-1)
 600  continue


      return
      end
            
