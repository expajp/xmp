      subroutine  clearv

        use cmmod

      do  100  k = 1, n2
        do  100  j = 1, m2
          do  100  i = 1, l1
            u (i,j,k)  =  0.0d0
            u1(i,j,k)  =  0.0d0
 100  continue

      do  110  k = 1, n2
        do  110  j = 1, m1
          do  110  i = 1, l2
            v (i,j,k)  =  0.0d0
            v1(i,j,k)  =  0.0d0
 110  continue

      do  120  k = 1, n1
        do  120  j = 1, m2
          do  120  i = 1, l2
            w (i,j,k)  =  0.0d0
            w1(i,j,k)  =  0.0d0
 120  continue

      do  200  k = 1, n2
        do  200  j = 1, m2
          do  200  i = 1, l2
            p (i,j,k)  =  0.0d0
 200  continue

      
      do  300  k = 1, n1
        do  300  j = 1, m1
          do  300  i = 1, l1
            wk1(i,j,k)  =  0.0d0
            wk2(i,j,k)  =  0.0d0
            wk3(i,j,k)  =  0.0d0
            dfs(i,j,k)  =  0.0d0
 300  continue


      do  500  k = 1, n
        do  500  ip = 1, lm
          zcoef(ip,1,k)  =  0.0d0
          zcoef(ip,2,k)  =  0.0d0
          zcoef(ip,3,k)  =  0.0d0
          zcoef(ip,4,k)  =  0.0d0
          zcoef(ip,5,k)  =  0.0d0
          zcoef(ip,6,k)  =  0.0d0
          zcoef(ip,7,k)  =  0.0d0
          zb(ip,k)       =  0.0d0
 500  continue

      do  510  kp = 1, n2
        do  510  ip = 1, lm2
          zx(ip,k)       =  0.0d0
 510  continue


      return
      end
