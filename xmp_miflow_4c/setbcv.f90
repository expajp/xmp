      subroutine  setbcv
      
      use cmmod

      
! ---( BC for velocity <u> )--------------------------------------------
      
      do  100  k = 2, n1
        do  100 j = 2, m1
          u1( 1, j, k)  =   uinit
          u1(l1, j, k)  =   u1(l, j, k)
 100  continue

! ---( BC for velocity <v> )--------------------------------------------

      do  210  k = 2, n1
        do  210  i = 2, l1
          v1( i, 1, k)  =   v1( i, 2, k)
          v1( i,m1, k)  =   v1( i, m, k)
 210  continue

! ---( BC for velocity <w> )--------------------------------------------
      
      do  320  j = 2, m1
        do  320  i = 2, l1
          w1( i, j, 1)  =   0.0d0
          w1( i, j,n1)  =   0.0d0
 320  continue


      return
      end
