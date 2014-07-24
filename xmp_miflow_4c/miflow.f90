! ----------------------------------------------------------------------
!     MiFlow Ver 2.0
!                                        created by Mitsuo YOKOKAWA
! ----------------------------------------------------------------------

      program  miflow

      use cmmod

!      character  filenm*32

      ! Variables for Calculate
      integer :: myrank, xmp_node_num
      double precision :: xmp_wtime, cpu0, cpu1

      myrank = xmp_node_num()

      ! --- count start --- !
      !$xmp barrier
      cpu0 = xmp_wtime()


      call  clearv
      call  datain
      call  initvr
      call  mkcoef
      
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

      !$xmp task on n(1)
        write(6,*) 'Loop = ', loopo
        write(6,'(a,1p,2e15.5)') &
             ' Maximum velocity at outlet  : ', pcal, u(l,mh,nh)
      !$xmp end task


! Debug
!      write(6,'(5e15.5)') (u(l ,mh, k),k=2,n1)
!      write(6,'(5e15.5)') (p(i ,mh,nh),i=2,l1)

 1000 continue


!      filenm = '../Figure/snap'
!      write( filenm(15:18), '(i4.4)' ) loopo
!      open(10, file=filenm, form='unformatted')
!      write(10)  l, n, uinit
!      write(10) ((u(i,mh,k),i=1,l1),k=2,n1)
!      close(10)

        ! --- count stop --- !
        !$xmp barrier
        cpu1 = xmp_wtime()

        if(myrank == 0) write(*,*) cpu1-cpu0
              
      stop
      end