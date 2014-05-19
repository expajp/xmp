      subroutine  linpcg( l, lm, lmn, np, nls, eps, maxitr,
     &                    coef, b, xs, dd, vr, vp, vq )

      implicit  real*8  (a-h,o-z)
      
      dimension   coef(lmn,7),      b(lmn)
      dimension   xs(np),  vr(np),  vp(np),  vq(np),  dd(np)


      bmax = 0.0
      do  200  i = 1, lmn
        bmax = max( bmax, abs(b(i)) )
  200 continue
      
      res  = 0.0
      do  210  i = 1, lmn
        ip    =   i + lm
        vr(ip) =   b(i) - coef(i,1)*xs(ip-lm)
     &                  - coef(i,2)*xs(ip-l )
     &                  - coef(i,3)*xs(ip-1 )
     &                  - coef(i,4)*xs(ip)   
     &                  - coef(i,5)*xs(ip+1 )
     &                  - coef(i,6)*xs(ip+l )
     &                  - coef(i,7)*xs(ip+lm)
        res    =   max( res, abs(vr(ip)) )
  210 continue
      if( bmax .ne. 0.0 )   res = res / bmax

      if( res .lt. eps )  then
         iter = 0
         write(6,6000) iter, res
         return
      endif

      
c --- ( initialization ) ---------------------------------------------
      do  400  i = 1, lmn
        ip     =  i + lm
        vp(ip) = ( vr(ip) - vp(ip-1 )*coef(i,3)
     &                    - vp(ip-l )*coef(i,2)
     &                    - vp(ip-lm)*coef(i,1) ) * dd(ip)
  400 continue
      
      do  410  i = lmn, 1, -1
        ip     =  i + lm
        vp(ip) =  vp(ip) - ( vp(ip+1 )*coef(i,5)
     &                     + vp(ip+l )*coef(i,6)
     &                     + vp(ip+lm)*coef(i,7) ) * dd(ip)
  410 continue

      sgm  =  0.0
      do  430  i = 1, lmn
        sgm  =  sgm + vr(i+lm)*vp(i+lm)
  430 continue

      
c --- ( iteration phase ) --------------------------------------------
      iter =  0
      
 1000 continue

        iter = iter + 1

        sgm1  =  0.0
        do  500  i = 1, lmn
          ip     =   i + lm
          vq(ip) =   coef(i,1)*vp(ip-lm) 
     &             + coef(i,2)*vp(ip-l )
     &             + coef(i,3)*vp(ip-1 )
     &             + coef(i,4)*vp(ip   )
     &             + coef(i,5)*vp(ip+1 )
     &             + coef(i,6)*vp(ip+l )
     &             + coef(i,7)*vp(ip+lm)
          sgm1   =   sgm1 + vp(ip)*vq(ip)
  500   continue

        alp  =  sgm/sgm1

        do  510  i = 1, lmn
          ip     = i + lm
          xs(ip) = xs(ip) + alp*vp(ip)
          vr(ip) = vr(ip) - alp*vq(ip)
  510   continue

        do  520  i = 1, lmn
          ip     =  i + lm
          vq(ip) = ( vr(ip) - vq(ip-1 )*coef(i,3)
     &                      - vq(ip-l )*coef(i,2)
     &                      - vq(ip-lm)*coef(i,1) ) * dd(ip)
  520   continue
        do  530  i = lmn, 1, -1
          ip     =  i + lm
          vq(ip) =  vq(ip) - ( vq(ip+1 )*coef(i,5)
     &                       + vq(ip+l )*coef(i,6)
     &                       + vq(ip+lm)*coef(i,7) ) * dd(ip)
  530   continue

        sgm1  =  0.0
        do  550  i = 1, lmn
          sgm1  =  sgm1 + vr(i+lm)*vq(i+lm)
  550   continue

        beta  =  sgm1/sgm
        sgm   =  sgm1

        do  560  i = 1, lmn
          vp(i+lm) = vq(i+lm) + beta*vp(i+lm)
  560   continue

c --- ( convergence condition ) --------------------------------------
        res  =  0.0
        do  600  i = 1, lmn
          ip   =   i + lm
          rtmp =   b(i) - coef(i,1)*xs(ip-lm)
     &                  - coef(i,2)*xs(ip-l )
     &                  - coef(i,3)*xs(ip-1 )
     &                  - coef(i,4)*xs(ip   )
     &                  - coef(i,5)*xs(ip+1 )
     &                  - coef(i,6)*xs(ip+l )
     &                  - coef(i,7)*xs(ip+lm)
          res  =  max( res, abs( rtmp ) )
  600   continue
        if( bmax .ne. 0.0 )   res = res / bmax

c ===( debug write )======================
c        if( mod(iter,10) .eq. 0 )  then
c           write(6,6000)  iter, res
c        endif


        if( (res .gt. eps) .and. (iter .le. maxitr) )  go to 1000

      write(6,6000)  iter, res
 6000 format(8x,'== PCG ==  ',i5,5x,e15.6)

      
      return
      end
