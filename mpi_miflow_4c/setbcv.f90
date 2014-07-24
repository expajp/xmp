subroutine  setbcv
      
  use cmmod

! ---( set start_temp )-------------------------------------------------
  if(n1start == 1) then
     n1start_temp = 2
  else
     n1start_temp = n1start
  end if
  
      
! ---( BC for velocity <u> )--------------------------------------------
      
  do k = n1start_temp, n1end
     do j = 2, m1
          u1( 1, j, k)  =   uinit
          u1(l1, j, k)  =   u1(l, j, k)
       end do
    end do

! ---( BC for velocity <v> )--------------------------------------------

    do k = n1start_temp, n1end
       do i = 2, l1
          v1( i, 1, k)  =   v1( i, 2, k)
          v1( i,m1, k)  =   v1( i, m, k)
       end do
    end do

! ---( BC for velocity <w> )--------------------------------------------
      
    do j = 2, m1
       do i = 2, l1
          w1( i, j, 1)  =   0.0d0
          w1( i, j,n1)  =   0.0d0
       end do
    end do


    return
    
  end subroutine setbcv
