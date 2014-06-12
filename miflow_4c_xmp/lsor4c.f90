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

!$xmp barrier

      bmax  =  0.0d0

!$xmp loop on t(k) reduction(max:bmax)
      do k = 1, n
         do ip = 1, lm
            bmax  =  max( bmax, abs( b(ip,k) ) )
         end do
      end do

!$xmp barrier

      res  =  0.0d0

!$xmp loop on t(k) reduction(max:res)
      do k = 1, n
        kp  =  k + 1
        do ip = 1, lm
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

!$xmp barrier

      if( bmax .ne. 0.0 )   res = res / bmax

      if( res .lt. eps )  then
         iter = 0

         !$xmp task on n(1)
         write(6,6000) iter, res
         !debug write(6, *) "return from head"
         !$xmp end task

         return
      endif

! --- ( iteration phase ) --------------------------------------------
      iter  =  0

! for goto 10      
 10   continue
      
        iter  =  iter + 1

!$xmp barrier
!$xmp reflect(x)
!$xmp loop on t(k)
        do k = 1, n, 2
          kp  =  k + 1 ! kp is an even number

          do ip = 1, lm, 2
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

          do ip = 2, lm, 2
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

      end do

!$xmp barrier
!$xmp reflect(x)
!$xmp loop on t(k)
        do k = 2, n, 2
          kp  =  k + 1 ! kp is an odd number

          do ip = 2, lm, 2
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

          do ip = 1, lm, 2
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

      end do

      !$xmp barrier
        res  =  0.0d0

!$xmp barrier
!$xmp reflect(x)
!$xmp loop on t(k) reduction(max:res)
        do k = 1, n
          kp  =  k + 1
          do ip = 1, lm
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

      !$xmp barrier
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
