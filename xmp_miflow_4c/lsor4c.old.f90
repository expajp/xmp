      subroutine  lsor4c( l,      lm,     n,      eps,    maxitr, &
                    coef,   b,      x,      omega,  s1omg  )

      implicit  real*8  (a-h,o-z)
      dimension   coef(lm,7,n),  b(lm,n),  x(lm+2*l,n+2)

      ! n = 21

      ! XMP directives
      !$xmp nodes n(*)
      !$xmp template t(0:22)
      !$xmp distribute t(block) onto n
      !$xmp align coef(*,*,k) with t(k)
      !$xmp align b(*,j) with t(j)
      !$xmp align x(*,j) with t(j-1)
      !$xmp shadow x(0, 2)

      bmax  =  0.0d0

!$xmp loop on t(k) reduction(max:bmax)
      do  100  k = 1, n
         do  100  ip = 1, lm
            bmax  =  max( bmax, abs( b(ip,k) ) )
         end do
      end do
! 100  continue
      
      res  =  0.0d0

!$xmp loop on t(k) reduction(max:res)
      do  110  k = 1, n
        kp  =  k + 1
        do  110  ip = 1, lm
          ix    =  ip + l
          rtmp  =  b(ip,k) - coef(ip,1,k)*x(ix  , kp-1)   &
                     - coef(ip,2,k)*x(ix-l, kp  )   &
                     - coef(ip,3,k)*x(ix-1, kp  )   &
                     - coef(ip,4,k)*x(ix  , kp  )   &
                     - coef(ip,5,k)*x(ix+1, kp  )   &
                     - coef(ip,6,k)*x(ix+l, kp  )   &
                     - coef(ip,7,k)*x(ix  , kp+1)
          res   =  max( res, abs(rtmp) )
       end do
    end do
! 110  continue

      if( bmax .ne. 0.0 )   res = res / bmax

      if( res .lt. eps )  then
         iter = 0

         !$xmp task on n(1)
         write(6,6000) iter, res
         !$xmp end task

         return
      endif


! --- ( iteration phase ) --------------------------------------------
      iter  =  0

! for goto 10      
 10   continue
      
        iter  =  iter + 1

!$xmp reflect(x)
!$xmp loop on t(k)
        do  200  k = 1, n, 2
          kp  =  k + 1 ! kp is an even number

          do  210  ip = 1, lm, 2
            ix      =  ip + l
            xtmp    = ( b(ip,k) - coef(ip,1,k)*x(ix  , kp-1) &
                          - coef(ip,2,k)*x(ix-l, kp  ) &
                          - coef(ip,3,k)*x(ix-1, kp  ) &
                          - coef(ip,5,k)*x(ix+1, kp  ) &
                          - coef(ip,6,k)*x(ix+l, kp  ) &
                          - coef(ip,7,k)*x(ix  , kp+1) ) &
                / coef(ip,4,k)
            x(ix,kp) =  s1omg*x(ix,kp) + omega*xtmp
         end do
! 210    continue

          do  220  ip = 2, lm, 2
            ix      =  ip + l
            xtmp    = ( b(ip,k) - coef(ip,1,k)*x(ix  , kp-1) &
                          - coef(ip,2,k)*x(ix-l, kp  ) &
                          - coef(ip,3,k)*x(ix-1, kp  ) &
                          - coef(ip,5,k)*x(ix+1, kp  ) &
                          - coef(ip,6,k)*x(ix+l, kp  ) &
                          - coef(ip,7,k)*x(ix  , kp+1) ) &
                / coef(ip,4,k)
            x(ix,kp) =  s1omg*x(ix,kp) + omega*xtmp
         end do
! 220    continue

      end do
! 200  continue

!$xmp reflect(x)
!$xmp loop on t(k)
        do  300  k = 2, n, 2
          kp  =  k + 1 ! kp is an odd number

          do  310  ip = 2, lm, 2
            ix      =  ip + l
            xtmp    = ( b(ip,k) - coef(ip,1,k)*x(ix  , kp-1) &
                          - coef(ip,2,k)*x(ix-l, kp  ) &
                          - coef(ip,3,k)*x(ix-1, kp  ) &
                          - coef(ip,5,k)*x(ix+1, kp  ) &
                          - coef(ip,6,k)*x(ix+l, kp  ) &
                          - coef(ip,7,k)*x(ix  , kp+1) ) &
                / coef(ip,4,k)
            x(ix,kp) =  s1omg*x(ix,kp) + omega*xtmp
         end do
! 310    continue

          do  320  ip = 1, lm, 2
            ix      =  ip + l
            xtmp    = ( b(ip,k) - coef(ip,1,k)*x(ix  , kp-1) &
                          - coef(ip,2,k)*x(ix-l, kp  ) &
                          - coef(ip,3,k)*x(ix-1, kp  ) &
                          - coef(ip,5,k)*x(ix+1, kp  ) &
                          - coef(ip,6,k)*x(ix+l, kp  ) &
                          - coef(ip,7,k)*x(ix  , kp+1) ) &
                / coef(ip,4,k)
            x(ix,kp) =  s1omg*x(ix,kp) + omega*xtmp
         end do
! 320    continue

      end do
! 300  continue

        res  =  0.0d0

!$xmp reflect(x)
!$xmp loop on t(k) reduction(max:res)
        do  400  k = 1, n
          kp  =  k + 1
          do  400  ip = 1, lm
            ix    =  ip + l
            rtmp  =  b(ip,k) - coef(ip,1,k)*x(ix  , kp-1)   &
                       - coef(ip,2,k)*x(ix-l, kp  )   &
                       - coef(ip,3,k)*x(ix-1, kp  )   &
                       - coef(ip,4,k)*x(ix  , kp  )   &
                       - coef(ip,5,k)*x(ix+1, kp  )   &
                       - coef(ip,6,k)*x(ix+l, kp  )   &
                       - coef(ip,7,k)*x(ix  , kp+1)
            res   =  max( res, abs(rtmp) )
         end do
      end do
! 400    continue

        res = res / bmax

! ===( debug write )======================
!        if( mod(iter,10) .eq. 0 )  then
!           write(6,6000)  iter, res
!        endif


        if( (res .gt. eps) .and. (iter .le. maxitr) )  go to 10

        !$xmp task on n(1)
        write(6,6000)  iter, res
        !$xmp end task

 6000 format(8x,'== SOR4C ==  ',i5,5x,e15.6)
      
      return
      end
