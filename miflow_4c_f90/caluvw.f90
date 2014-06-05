      subroutine  caluvw

      use cmmod


! ---( Make right hand side of system of linear equation )--------------

      do  110  k = 1, n
        ip  =  0
        do  110  j = 1, m
          do  110  i = 1, l
            ip       =  ip + 1
            zb(ip,k) = odtodx*( - u1(i+1,j+1,k+1) + u1(i  ,j+1,k+1) ) &
               + odtody*( - v1(i+1,j+1,k+1) + v1(i+1,j  ,k+1) ) &
               + odtodz*( - w1(i+1,j+1,k+1) + w1(i+1,j+1,k  ) )
 110  continue
         
! for reference point      
!         b(lmctr,nctr)  =  0.0d0

      
! ---( Poison solver )--------------------------------------------------
      
      call  lsor4c( l, lm, n, eps, maxitr,  &
              zcoef, zb, zx, omega, s1omg )


! ---( put the solution into presure variable "p" )---------------------

      do  210  k = 1, n
        ip  =  0
        do  210  j = 1, m
          do  210  i = 1, l
            ip              =  ip + 1
            p(i+1,j+1,k+1)  =  zx(ip+l,k+1)
 210  continue

      
! ---( Velocity correction )--------------------------------------------
      
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
