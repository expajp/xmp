program xmp_bug_test
  implicit none

  integer, parameter :: n = 4
  integer :: i
  integer :: a(n)
  integer :: buffer
  integer :: myrank, xmp_node_num

  !$xmp nodes p(4)
  !$xmp template t(4)
  !$xmp distribute t(block) onto p
  !$xmp align a(i) with t(i)

  myrank = xmp_node_num()

  !$xmp loop on t(i)
  do i = 1, n
     a(i) = i
     buffer = i
  end do
  
  write(*, *) "before: myrank = ", myrank, " , buffer = ", buffer

  !$xmp bcast(buffer) from t(1) on t(1:4)

  write(*, *) " after: myrank = ", myrank, " , buffer = ", buffer

end program xmp_bug_test
