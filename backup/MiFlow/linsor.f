      subroutine  linsor( l,      lm,     lmn,    eps,    maxitr,
     &                    coef,   b,      x,      omega,  s1omg  )

      implicit  real*8  (a-h,o-z)
      dimension   coef(lmn,7),  b(lmn),  x(lmn+2*lm)


      bmax  =  0.0d0
      do  100  ip = 1, lmn
        bmax  =  max( bmax, abs(b(ip)) )
 100  continue
      
      res  =  0.0d0
      do  110  ip = 1, lmn
        ix    =  ip + lm
        rtmp  =  b(ip) - coef(ip,1)*x(ix-lm)  
     &                 - coef(ip,2)*x(ix-l )  
     &                 - coef(ip,3)*x(ix-1 )  
     &                 - coef(ip,4)*x(ix   )  
     &                 - coef(ip,5)*x(ix+1 )  
     &                 - coef(ip,6)*x(ix+l )  
     &                 - coef(ip,7)*x(ix+lm)
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

        do  200  ip = 1, lmn
          ix    =  ip + lm
          xtmp  = ( b(ip) - coef(ip,1)*x(ix-lm)
     &                    - coef(ip,2)*x(ix-l )
     &                    - coef(ip,3)*x(ix-1 )
     &                    - coef(ip,5)*x(ix+1 )
     &                    - coef(ip,6)*x(ix+l )
     &                    - coef(ip,7)*x(ix+lm) ) / coef(ip,4)
          x(ix) =  s1omg*x(ix) + omega*xtmp
 200    continue
        
  
        res  =  0.0d0
        do  300  ip = 1, lmn
          ix    =  ip + lm
          rtmp  =  b(ip) - coef(ip,1)*x(ix-lm)  
     &                   - coef(ip,2)*x(ix-l )  
     &                   - coef(ip,3)*x(ix-1 )  
     &                   - coef(ip,4)*x(ix   )  
     &                   - coef(ip,5)*x(ix+1 )  
     &                   - coef(ip,6)*x(ix+l )  
     &                   - coef(ip,7)*x(ix+lm)
          res   =  max( res, abs(rtmp) )
 300    continue
        res = res / bmax


c ===( debug write )======================
c        if( mod(iter,10) .eq. 0 )  then
c           write(6,6000)  iter, res
c        endif


        if( (res .gt. eps) .and. (iter .le. maxitr) )  go to 10

      write(6,6000)  iter, res
 6000 format(8x,'== SORNA ==  ',i5,5x,e15.6)

      
      return
      end
