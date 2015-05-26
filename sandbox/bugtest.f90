program bug_test

  integer, parameter :: n = 100
  integer :: i
  integer :: a(n)

  do i = 1, n
     a(i) = i
     if(mod(i,10) == 0) a(i) = i*10
  end do

  ! output
  do i = 1, n
     write(*, '(2(A,i4))') "a(", i, ") = ", a(i)
  end do
  

end program bug_test
