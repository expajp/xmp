module lsor4cmod
  implicit none

  ! Variables for XMP
  integer :: dist(4) = (/9,8,8,9/)
  
  ! XMP directives
  !$xmp nodes n(4)
  !$xmp template t(34)
  !$xmp distribute t(gblock(dist)) onto n

end module lsor4cmod
