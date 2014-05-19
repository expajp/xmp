      subroutine  lsor4c( l,      lm,     n,      eps,    maxitr,
     &                    coef,   b,      x,      omega,  s1omg  )

      implicit  real*8  (a-h,o-z)
      dimension   coef(lm,7,n),  b(lm,n),  x(lm+2*l,n+2)


      bmax  =  0.0d0
      do  100  k = 1, n
         do  100  ip = 1, lm
            bmax  =  max( bmax, abs( b(ip,k) ) )
 100  continue

      
      res  =  0.0d0
      do  110  k = 1, n
        kp  =  k + 1
        do  110  ip = 1, lm
          ix    =  ip + l
          rtmp  =  b(ip,k) - coef(ip,1,k)*x(ix  , kp-1)  
     &                     - coef(ip,2,k)*x(ix-l, kp  )  
     &                     - coef(ip,3,k)*x(ix-1, kp  )  
     &                     - coef(ip,4,k)*x(ix  , kp  )  
     &                     - coef(ip,5,k)*x(ix+1, kp  )  
     &                     - coef(ip,6,k)*x(ix+l, kp  )  
     &                     - coef(ip,7,k)*x(ix  , kp+1)
          res   =  max( res, abs(rtmp) )
 110  continue


      if( bmax .ne. 0.0 )   res = res / bmax

      if( res .lt. eps )  then
         iter = 0
         write(6,6000) iter, res
         return
      endif


c --- ( iteration phase ) --------------------------------------------
      iter  =  0
      
 10   continue
      
        iter  =  iter + 1

        do  200  k = 1, n, 2
          kp  =  k + 1

*vocl loop,novrec
          do  210  ip = 1, lm, 2
            ix      =  ip + l
            xtmp    = ( b(ip,k) - coef(ip,1,k)*x(ix  , kp-1)
     &                          - coef(ip,2,k)*x(ix-l, kp  )
     &                          - coef(ip,3,k)*x(ix-1, kp  )
     &                          - coef(ip,5,k)*x(ix+1, kp  )
     &                          - coef(ip,6,k)*x(ix+l, kp  )
     &                          - coef(ip,7,k)*x(ix  , kp+1) )
     &                / coef(ip,4,k)
            x(ix,kp) =  s1omg*x(ix,kp) + omega*xtmp
 210    continue
  
*vocl loop,novrec
          do  220  ip = 2, lm, 2
            ix      =  ip + l
            xtmp    = ( b(ip,k) - coef(ip,1,k)*x(ix  , kp-1)
     &                          - coef(ip,2,k)*x(ix-l, kp  )
     &                          - coef(ip,3,k)*x(ix-1, kp  )
     &                          - coef(ip,5,k)*x(ix+1, kp  )
     &                          - coef(ip,6,k)*x(ix+l, kp  )
     &                          - coef(ip,7,k)*x(ix  , kp+1) )
     &                / coef(ip,4,k)
            x(ix,kp) =  s1omg*x(ix,kp) + omega*xtmp

 220    continue

 200  continue

  
        do  300  k = 2, n, 2
          kp  =  k + 1

*vocl loop,novrec
          do  310  ip = 2, lm, 2
            ix      =  ip + l
            xtmp    = ( b(ip,k) - coef(ip,1,k)*x(ix  , kp-1)
     &                          - coef(ip,2,k)*x(ix-l, kp  )
     &                          - coef(ip,3,k)*x(ix-1, kp  )
     &                          - coef(ip,5,k)*x(ix+1, kp  )
     &                          - coef(ip,6,k)*x(ix+l, kp  )
     &                          - coef(ip,7,k)*x(ix  , kp+1) )
     &                / coef(ip,4,k)
            x(ix,kp) =  s1omg*x(ix,kp) + omega*xtmp
 310    continue
  
*vocl loop,novrec
          do  320  ip = 1, lm, 2
            ix      =  ip + l
            xtmp    = ( b(ip,k) - coef(ip,1,k)*x(ix  , kp-1)
     &                          - coef(ip,2,k)*x(ix-l, kp  )
     &                          - coef(ip,3,k)*x(ix-1, kp  )
     &                          - coef(ip,5,k)*x(ix+1, kp  )
     &                          - coef(ip,6,k)*x(ix+l, kp  )
     &                          - coef(ip,7,k)*x(ix  , kp+1) )
     &                / coef(ip,4,k)
            x(ix,kp) =  s1omg*x(ix,kp) + omega*xtmp
 320    continue

 300  continue
  

  
        res  =  0.0d0
        do  400  k = 1, n
          kp  =  k + 1
          do  400  ip = 1, lm
            ix    =  ip + l
            rtmp  =  b(ip,k) - coef(ip,1,k)*x(ix  , kp-1)  
     &                       - coef(ip,2,k)*x(ix-l, kp  )  
     &                       - coef(ip,3,k)*x(ix-1, kp  )  
     &                       - coef(ip,4,k)*x(ix  , kp  )  
     &                       - coef(ip,5,k)*x(ix+1, kp  )  
     &                       - coef(ip,6,k)*x(ix+l, kp  )  
     &                       - coef(ip,7,k)*x(ix  , kp+1)
            res   =  max( res, abs(rtmp) )
 400    continue
        res = res / bmax


c ===( debug write )======================
c        if( mod(iter,10) .eq. 0 )  then
c           write(6,6000)  iter, res
c        endif


        if( (res .gt. eps) .and. (iter .le. maxitr) )  go to 10

      write(6,6000)  iter, res
 6000 format(8x,'== SOR4C ==  ',i5,5x,e15.6)

      
      return
      end
