      subroutine  datain

      include  "./Include/cmparm"
      include  "./Include/cmvars"
      

c      read(5,*) xlen, dt
      
      xlen   =  15.0d0
      ylen   =   1.0d0
      zlen   =   1.0d0
      dx     =  xlen/real(l)
      dy     =  ylen/real(m)
      dz     =  zlen/real(n)
      
      dt     =  2.0e-02
      
      re     =  100.0d0

      uinit  =  1.0

      icschm =  1
      ilsolv =  1
      
      lpbgn  =  1
      lpend  =  20
      linner =  50
      maxitr =  199

      
      write(6,6000)  l, m, n, xlen, ylen, zlen, dx, dy, dz, dt, re
 6000 format(5x,20('=')/
     &       5x,'  MiFlow (Ver 1.1)'/
     &       5x,20('=')//
     &       5x,'Number of lattice        : ',i5,' *',i5,' *',i5//
     &       5x,'Length (x-direction)     : ',1p,e15.4/
     &       5x,'Length (y-direction)     : ',1p,e15.4/
     &       5x,'Length (z-direction)     : ',1p,e15.4//
     &       5x,'Mesh size (x-direction)  : ',e15.4/
     &       5x,'          (y-direction)  : ',e15.4/
     &       5x,'          (z-direction)  : ',e15.4//
     &       5x,'Time step                : ',e15.4//
     &       5x,'Reynolds Number          : ',e15.4///)
      
      write(6,6100)  icschm, ilsolv 
 6100 format(5x,'Difference scheme for convection term : ',i5/
     &       5x,'   1 = Upwind'/
     &       5x,'   2 = Central'//
     &       5x,'Poisson Solver                        : ',i5/
     &       5x,'   1 = SOR method with natural ordering'/
     &       5x,'   2 = SOR method with red-black ordering ',
     &          '(Vectorizable)'/
     &       5x,'   3 = ILUCG method with natural ordering'/
     &       5x,'   4 = ILUCG method with hyperplane ordering ',
     &          '(Vectorizable)'/
     &       5x,'   5 = SOR method with 4-color ordering ',
     &          '(Vectorizable)'///)

      
      return
      end
