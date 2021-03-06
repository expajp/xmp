module cmmod

      implicit none

      integer, parameter :: l=101, m=5, n=32 !default:n=21
      
      integer, parameter :: l1=l+1, m1=m+1, n1=n+1, l2=l+2, m2=m+2, n2=n+2
      integer, parameter :: lm=l*m, lmn=l*m*n, lm2=lm+2*l

      ! cmcnst
      real(8) :: re, dt, dx, dy, dz, ore, odt, odx, ody, odz, odx2, ody2
      real(8) :: odz2, omega, s1omg, eps, uinit, xlen, ylen, zlen

      ! cmcalc
      real(8) :: dtodx, dtody, dtodz, odtodx, odtody, odtodz
      real(8) :: cdt4dx, cdt4dy, cdt4dz, dfxore, dfyore, dfzore
      real(8) :: cdt2dx, cdt2dy, cdt2dz

      ! cmcntl
      integer ::  lpbgn, lpend, linner, maxitr, lmnctr, lmctr, nctr
      
      ! cmflow
      real(8) :: u(l1,m2,n2), v(l2,m1,n2), w(l2,m2,n1)
      real(8) :: u1(l1,m2,n2),  v1(l2,m1,n2),  w1(l2,m2,n1)
      real(8) :: p(l2,m2,n2)
      
      ! cmwork
      real(8) :: wk1(l1,m1,n1), wk2(l1,m1,n1), wk3(l1,m1,n1)
      real(8) :: dfs(l1,m1,n1)

      ! cmlslz
      real(8) :: zcoef(lm,7,n), zb(lm,n), zx(lm2,n2)

end module cmmod
