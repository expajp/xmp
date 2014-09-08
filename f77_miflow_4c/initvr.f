      subroutine  initvr

      include  "./Include/cmparm"
      include  "./Include/cmvars"


      nd2     =  n/2 - 1
      md2     =  m/2 - 1
      lmnctr  =  l*m*nd2 + l*md2 + l
      lmctr   =  l*md2 + l
      nctr    =  nd2
      
      odt     =  1.0d0/dt
      
      odx     =  1.0d0/dx
      ody     =  1.0d0/dy
      odz     =  1.0d0/dz
      odx2    =  odx*odx
      ody2    =  ody*ody
      odz2    =  odz*odz

      ore     =  1.0d0/re

      dtodx   =  dt*odx
      dtody   =  dt*ody
      dtodz   =  dt*odz
      
      odtodx  =  odt*odx
      odtody  =  odt*ody
      odtodz  =  odt*odz
      
      cdt2dx  = -0.50d0*odx*dt
      cdt2dy  = -0.50d0*ody*dt
      cdt2dz  = -0.50d0*odz*dt

      cdt4dx  = -0.25d0*odx*dt
      cdt4dy  = -0.25d0*ody*dt
      cdt4dz  = -0.25d0*odz*dt

      dfxore  =  ore*odx2*dt
      dfyore  =  ore*ody2*dt
      dfzore  =  ore*odz2*dt

      omega   =  1.92d0
      s1omg   =  1.0d0 - omega
      eps     =  1.0e-5
      

      do  100  k = 2, n1
        do  100  j = 2, m1
          do  100  i = 2, l
            u(i,j,k)  =  uinit
 100  continue

      
      return
      end
