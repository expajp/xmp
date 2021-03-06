subroutine  datain

  use cmmod
  implicit none

  ! read(5,*) xlen, dt
      
  xlen   =  15.0d0
  ylen   =   1.0d0
  zlen   =   1.0d0
  dx     =  xlen/real(l)
  dy     =  ylen/real(m)
  dz     =  zlen/real(n)
  
  dt     =  2.0e-02
      
  re     =  100.0d0
      
  uinit  =  1.0

  lpbgn  =  1
  lpend  =  20 ! default: 20
  linner =  50 ! default: 50
  maxitr =  299 ! default: 299

  if(myrank == 1) then
     write(6,6000)  l, m, n, xlen, ylen, zlen, dx, dy, dz, dt, re
  end if

!  write(6,7000) nprocs, myrank

6000 format(5x,20('=')/ &
          5x,'  MiFlow (Ver 1.1)'/ &
          5x,20('=')// &
          5x,'Number of lattice        : ',i5,' *',i5,' *',i5// &
          5x,'Length (x-direction)     : ',1p,e15.4/ &
          5x,'Length (y-direction)     : ',1p,e15.4/ &
          5x,'Length (z-direction)     : ',1p,e15.4// &
          5x,'Mesh size (x-direction)  : ',e15.4/ &
          5x,'          (y-direction)  : ',e15.4/ &
          5x,'          (z-direction)  : ',e15.4// &
          5x,'Time step                : ',e15.4// &
          5x,'Reynolds Number          : ',e15.4///)

7000 format( &
          5x,'number of nodes          :',i5/ &
          5x,'My rank                  :',i5//)

      
  return
  
end subroutine datain
