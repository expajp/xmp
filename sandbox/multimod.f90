module multimod

      implicit none

      integer, parameter :: l=101, m=5, n=21
      
      integer, parameter :: l1=l+1, m1=m+1, n1=n+1, l2=l+2, m2=m+2, n2=n+2
      integer, parameter :: lm=l*m, lmn=l*m*n, lm2=lm+2*l

      real(8) :: u(l1,m2,n2), v(l2,m1,n2), w(l2,m2,n1)

      ! XMP directives
      !$xmp nodes n(*)
      !$xmp template t(23)
      !$xmp distribute t(block) onto n
      !$xmp align (*,*,j) with t(j) :: u, v, w

end module multimod
