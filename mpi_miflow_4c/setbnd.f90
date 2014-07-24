subroutine  setbnd
      
        use cmmod
        
        integer :: nstart_temp, n1start_temp

! ---( set start_temp )-------------------------------------------------
        if(nstart == 1) then
           nstart_temp = 2
        else
           nstart_temp = nstart
        end if

        if(n1start == 1) then
           n1start_temp = 2
        else
           n1start_temp = n1start
        end if

      
! ---( BC for velocity <u> )--------------------------------------------
      
      do  k = n1start_temp, n1end
        do j = 2, m1
          u( 1, j, k)  =   uinit
          u(l1, j, k)  =   u(l,j,k)
       end do
    end do

      do k = n1start_temp, n1end
        do i = 2, l
          u( i, 1, k)  =   u( i, 2, k)
          u( i,m2, k)  =   u( i,m1, k)
       end do
    end do

      do  j = 2, m1
        do i = 2, l
          u( i, j, 1)  =  -u( i, j, 2)
          u( i, j,n2)  =  -u( i, j,n1)
       end do
    end do

      
! ---( BC for velocity <v> )--------------------------------------------

    do k = n1start_temp, n1end
       do j = 2, m
          v( 1, j, k)  =  -v( 2,j,k)
          v(l2, j, k)  =   v(l1,j,k)
       end do
    end do

    do k = n1start_temp, n1end
       do i = 2, l1
          v( i, 1, k)  =   v( i, 2, k)
          v( i,m1, k)  =   v( i, m, k)
       end do
    end do

    do j = 2, m
       do i = 2, l1
          v( i, j, 1)  =  -v( i, j, 2)
          v( i, j,n2)  =  -v( i, j,n1)
       end do
    end do

      
! ---( BC for velocity <w> )--------------------------------------------
      
    do k = nstart_temp, nend
       do j = 2, m1
          w( 1, j, k)  =  -w( 2,j,k)
          w(l2, j, k)  =   w(l1,j,k)
       end do
    end do

    do k = nstart_temp, nend
       do i = 2, l1
          w( i, 1, k)  =   w( i, 2, k)
          w( i,m2, k)  =   w( i,m1, k)
       end do
    end do

    do j = 2, m1
       do i = 2, l1
          w( i, j, 1)  =   0.0d0
          w( i, j,n1)  =   0.0d0
       end do
    end do

! ----------------------------------------------------------------------

    return
  end subroutine setbnd
