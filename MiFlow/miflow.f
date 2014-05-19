c ----------------------------------------------------------------------
c     MiFlow Ver 1.1
c                                        created by Mitsuo YOKOKAWA
c ----------------------------------------------------------------------

      program  miflow

      include  "./Include/cmparm"
      include  "./Include/cmvars"

      character  filenm*32

c     caliculate time
      integer t1, t2, t_rate, t_max, diff
      call system_clock(t1)! clock start

      
      call  clearv
      call  datain
      call  initvr
      call  mkcoef
      call  ilvpcg( l, m, n, lm, lmn, nls, coef, dd, lv, lpt )

      
      do  1000  loopo = lpbgn, lpend

        do  1100  loopi = 1, linner

          call  setbnd
          call  calcu1
          call  calcv1
          call  calcw1
          call  setbcv
          call  caluvw

 1100   continue

        lh = l/2 + 2
        mh = m/2 + 2
        nh = n/2 + 2
        pcal =  0.125*re*(p(l,mh,nh)-p(l1,mh,nh))*odx
        write(6,*) 'Loop = ', loopo
        write(6,'(a,1p,2e15.5)')
     &      ' Maximum velocity at outlet  : ', pcal, u(l,mh,nh)
      
c Debug
c      write(6,'(5e15.5)') (u(l ,mh, k),k=2,n1)
c      write(6,'(5e15.5)') (p(i ,mh,nh),i=2,l1)

 1000 continue


c      filenm = '../Figure/snap'
c      write( filenm(15:18), '(i4.4)' ) loopo
c      open(10, file=filenm, form='unformatted')
c      write(10)  l, n, uinit
c      write(10) ((u(i,mh,k),i=1,l1),k=2,n1)
c      close(10)

c     caliculate time
      call system_clock(t2, t_rate, t_max)! clock stop
      if ( t2 < t1 ) then
          diff = t_max - t1 + t2
       else
          diff = t2 - t1
       endif
       
       print "(A, F10.3)", "time it took was:", diff/dble(t_rate)

      
      stop
      end
