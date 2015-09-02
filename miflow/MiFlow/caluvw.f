      subroutine  caluvw

      include  "./Include/cmparm"
      include  "./Include/cmvars"


c ---( Make right hand side of system of linear equation )--------------

      if( ilsolv .eq. 5 ) then

         do  110  k = 1, n
           ip  =  0
           do  110  j = 1, m
             do  110  i = 1, l
               ip       =  ip + 1
               zb(ip,k) = odtodx*( - u1(i+1,j+1,k+1) + u1(i  ,j+1,k+1) )
     &                  + odtody*( - v1(i+1,j+1,k+1) + v1(i+1,j  ,k+1) )
     &                  + odtodz*( - w1(i+1,j+1,k+1) + w1(i+1,j+1,k  ) )
 110     continue
         
c for reference point      
c         b(lmctr,nctr)  =  0.0d0

      else
     
         ip  =  0
         do  100  k = 1, n
           do  100  j = 1, m
             do  100  i = 1, l
               ip     =  ip + 1
               b(ip)  =  odtodx*( - u1(i+1,j+1,k+1) + u1(i  ,j+1,k+1) )
     &                 + odtody*( - v1(i+1,j+1,k+1) + v1(i+1,j  ,k+1) )
     &                 + odtodz*( - w1(i+1,j+1,k+1) + w1(i+1,j+1,k  ) )
 100     continue
         
c for reference point      
c         b(lmnctr)  =  0.0d0

      endif

      
c ---( Poison solver )--------------------------------------------------
      
      if( ilsolv .eq. 1 )
     &   call  linsor( l, lm, lmn, eps, maxitr,
     &                 coef, b, x, omega, s1omg )
      
      if( ilsolv .eq. 2 )
     &   call  lsorrb( l, lm, lmn, eps, maxitr,
     &                 coef, b, x, omega, s1omg )
      
      if( ilsolv .eq. 3 )
     &   call  linpcg( l, lm, lmn, lmn2, nls, eps, maxitr,
     &                 coef, b, x, dd, vr, vp, vq )

      if( ilsolv .eq. 4 )
     &   call  lpcghp( l, lm, lmn, lmn2, nls, eps, maxitr,
     &                 coef, b, x, dd, vr, vp, vq, lv, lpt )

      if( ilsolv .eq. 5 )
     &   call  lsor4c( l, lm, n, eps, maxitr, 
     &                 zcoef, zb, zx, omega, s1omg )


c ---( put the solution into presure variable "p" )---------------------

      if( ilsolv .eq. 5 )  then

         do  210  k = 1, n
           ip  =  0
           do  210  j = 1, m
             do  210  i = 1, l
               ip              =  ip + 1
               p(i+1,j+1,k+1)  =  zx(ip+l,k+1)
 210     continue

      else
   
         ip  =  0
         do  200  k = 1, n
           do  200  j = 1, m
             do  200  i = 1, l
               ip              =  ip + 1
               p(i+1,j+1,k+1)  =  x(ip+lm)
 200     continue

      endif

      
c ---( Velocity correction )--------------------------------------------
      
      do  500  k = 2, n1
        do  500  j = 2, m1
          do  500  i = 2, l
            u(i,j,k) = u1(i,j,k) - dtodx*( p(i+1,j  ,k  ) - p(i,j,k) )
 500  continue


      do  600  k = 2, n1
        do  600  j = 2, m
          do  600  i = 2, l1
            v(i,j,k) = v1(i,j,k) - dtody*( p(i  ,j+1,k  ) - p(i,j,k) )
 600  continue


      do  700  k = 2, n
        do  700  j = 2, m1
          do  700  i = 2, l1
            w(i,j,k) = w1(i,j,k) - dtodz*( p(i  ,j  ,k+1) - p(i,j,k) )
 700  continue



      return
      end
