      subroutine  setbnd
      
      include  "./Include/cmparm"
      include  "./Include/cmvars"

      
c ---( BC for velocity <u> )--------------------------------------------
      
      do  100  k = 2, n1
        do  100 j = 2, m1
          u( 1, j, k)  =   uinit
          u(l1, j, k)  =   u(l,j,k)
 100  continue

      do  110  k = 2, n1
        do  110  i = 2, l
          u( i, 1, k)  =   u( i, 2, k)
          u( i,m2, k)  =   u( i,m1, k)
 110  continue

      do  120  j = 2, m1
        do  120  i = 2, l
          u( i, j, 1)  =  -u( i, j, 2)
          u( i, j,n2)  =  -u( i, j,n1)
 120  continue

      
c ---( BC for velocity <v> )--------------------------------------------

      do  200  k = 2, n1
        do  200  j = 2, m
          v( 1, j, k)  =  -v( 2,j,k)
          v(l2, j, k)  =   v(l1,j,k)
 200  continue

      do  210  k = 2, n1
        do  210  i = 2, l1
          v( i, 1, k)  =   v( i, 2, k)
          v( i,m1, k)  =   v( i, m, k)
 210  continue

      do  220  j = 2, m
        do  220  i = 2, l1
          v( i, j, 1)  =  -v( i, j, 2)
          v( i, j,n2)  =  -v( i, j,n1)
 220  continue

      
c ---( BC for velocity <w> )--------------------------------------------
      
      do  300  k = 2, n
        do  300  j = 2, m1
          w( 1, j, k)  =  -w( 2,j,k)
          w(l2, j, k)  =   w(l1,j,k)
 300  continue

      do  310  k = 2, n
        do  310  i = 2, l1
          w( i, 1, k)  =   w( i, 2, k)
          w( i,m2, k)  =   w( i,m1, k)
 310  continue

      do  320  j = 2, m1
        do  320  i = 2, l1
          w( i, j, 1)  =   0.0d0
          w( i, j,n1)  =   0.0d0
 320  continue

c ----------------------------------------------------------------------

      return
      end
