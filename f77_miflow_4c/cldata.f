      subroutine  cldata( l, m, n )

      
      xlen   =  15.0d0
      ylen   =   1.0d0
      zlen   =   1.0d0
      dx     =  xlen/real(l)
      dy     =  ylen/real(m)
      dz     =  zlen/real(n)
      
      
      write(6,6000)  l, m, n, xlen, ylen, zlen, dx, dy, dz
 6000 format(5x,'Number of lattice        : ',i5,' *',i5,' *',i5//
     &       5x,'Length (x-direction)     : ',1p,e15.4/
     &       5x,'Length (y-direction)     : ',1p,e15.4/
     &       5x,'Length (z-direction)     : ',1p,e15.4//
     &       5x,'Mesh size (x-direction)  : ',e15.4/
     &       5x,'          (y-direction)  : ',e15.4/
     &       5x,'          (z-direction)  : ',e15.4//)

      
      return
      end
      
   
