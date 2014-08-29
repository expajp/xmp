! ----------------------------------------------------------------------
!     MiFlow Ver 2.0
!                                        created by Mitsuo YOKOKAWA
! ----------------------------------------------------------------------

program  miflow

  use mpi
  use cmmod

  character  filenm*32

  call initialize_mpi
  call set_distribution

  call  clearv ! finished
  call  datain ! finished
  call  initvr ! finished
  call  mkcoef ! finished


  do loopo = lpbgn, lpend
     do loopi = 1, linner

        call  setbnd ! finished
        call  calcu1 ! finished
        call  calcv1 ! finished
        call  calcw1 ! finished
        call  setbcv ! finished
        call  caluvw ! finished

     end do

        lh = l/2 + 2
        mh = m/2 + 2
        nh = n/2 + 2
        pcal =  0.125*re*(p(l,mh,nh)-p(l1,mh,nh))*odx

        if(myrank == 0) then
           write(6,*) 'Loop = ', loopo
           write(6,'(a,1p,2e15.5)') &
                ' Maximum velocity at outlet  : ', pcal, u(l,mh,nh)
        end if

        ! Debug
        !      write(6,'(5e15.5)') (u(l ,mh, k),k=2,n1)
        !      write(6,'(5e15.5)') (p(i ,mh,nh),i=2,l1)

     end do

        !      filenm = '../Figure/snap'
        !      write( filenm(15:18), '(i4.4)' ) loopo
        !      open(10, file=filenm, form='unformatted')
        !      write(10)  l, n, uinit
        !      write(10) ((u(i,mh,k),i=1,l1),k=2,n1)
        !      close(10)

     call finalize_mpi

   end program miflow
