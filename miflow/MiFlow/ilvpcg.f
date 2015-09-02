      subroutine  ilvpcg( l, m, n, lm, lmn, nls, coef, dd, lv, lpt )

      implicit    real*8 (a-h,o-z)
      
      dimension   coef(lmn,7),  dd(lmn)
      dimension   lv(lmn),  lpt(nls)


      il     =  0
      ilp    =  2
      lpt(1) =  0
      lpt(2) =  0

      
      do  100  np = 3, l+m+n
        
         ip  =  0
         
         do  110  k = 1, n
           do  110  j = 1, m
             do  110  i = 1, l
               
               ip  =  ip  +  1
               
               if( i+j+k .eq. np )  then
                  il      =  il  +  1
                  lv(il)  =  ip
               endif
               
 110     continue
         ilp       = ilp + 1
         lpt(ilp)  = il
 100  continue
      
      lpt(ilp+1)  = -1

      
c Debug write
c      ilp   =  2
c      ibgn  =  lpt(ilp) + 1
c      iend  =  lpt(ilp+1)
c 10   continue
c        write(6,6000)  ibgn, iend
c        write(6,6000) (lv(i),i=ibgn,iend)
c        write(6,*)
c 6000   format(5x,10i5)
c        ilp   =  ilp + 1
c        ibgn  =  lpt(ilp) + 1
c        iend  =  lpt(ilp+1)
c        if( iend .gt. 0 )  go to 10

c --- ( precondition ) -----------------------------------------------
      do  200  i = 1, lmn
        ip      =  i + lm
        dwk     =  coef(i,4) - dd(ip-1 )*coef(i,3)**2
     &                       - dd(ip-l )*coef(i,2)**2
     &                       - dd(ip-lm)*coef(i,1)**2
        dd(ip)  =  1.0d0 / dwk
  200 continue


      return
      end
