      subroutine  mkcoef

      include  "./Include/cmparm"
      include  "./Include/cmvars"


c --- ( for ilsolv = 5 )------------------------------------------------

      do  300  k = 1, n

        ip  =  0

        do  300  j = 1, m
          do  300  i = 1, l

            ip  =  ip + 1
            
            if( k .ne. 1 )  zcoef(ip,1,k)  =  -odz2
            if( j .ne. 1 )  zcoef(ip,2,k)  =  -ody2
            if( i .ne. 1 )  zcoef(ip,3,k)  =  -odx2

            if( i .ne. l )  zcoef(ip,5,k)  =  -odx2
            if( j .ne. m )  zcoef(ip,6,k)  =  -ody2
            if( k .ne. n )  zcoef(ip,7,k)  =  -odz2

 300  continue

      do  400  k = 1, n
        do  400  ip = 1, lm
          zcoef(ip,4,k)  =  - zcoef(ip,1,k) - zcoef(ip,2,k)
     &                      - zcoef(ip,3,k) + odx2
     &                      - zcoef(ip,6,k) - zcoef(ip,7,k)
        
c for Neumann condition        
c    &                      - zcoef(ip,3,k) + zcoef(ip,5,k)
c    &                      - zcoef(ip,6,k) - zcoef(ip,7,k)
        
 400  continue

c for reference point      
c      zcoef(lmctr,1,nctr)  =  0.0d0
c      zcoef(lmctr,2,nctr)  =  0.0d0
c      zcoef(lmctr,3,nctr)  =  0.0d0
c      zcoef(lmctr,4,nctr)  =  1.0d0
c      zcoef(lmctr,5,nctr)  =  0.0d0
c      zcoef(lmctr,6,nctr)  =  0.0d0
c      zcoef(lmctr,7,nctr)  =  0.0d0

      
      return
      end
