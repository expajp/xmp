module cmmod
  
  use mpi
  implicit none

  integer, parameter :: l=101, m=5, n=21

  integer, parameter :: l1=l+1, m1=m+1, n1=n+1, l2=l+2, m2=m+2, n2=n+2
  integer, parameter :: lm=l*m, lmn=l*m*n, lm2=lm+2*l

  ! cmcnst
  real(8) :: re, dt, dx, dy, dz, ore, odt, odx, ody, odz, odx2, ody2
  real(8) :: odz2, omega, s1omg, eps, uinit, xlen, ylen, zlen

  ! cmcalc
  real(8) :: dtodx, dtody, dtodz, odtodx, odtody, odtodz
  real(8) :: cdt4dx, cdt4dy, cdt4dz, dfxore, dfyore, dfzore
  real(8) :: cdt2dx, cdt2dy, cdt2dz

  ! cmcntl
  integer ::  lpbgn, lpend, linner, maxitr, lmnctr, lmctr, nctr

  ! cmflow
  real(8) :: u(l1,m2,n2), v(l2,m1,n2), w(l2,m2,n1)
  real(8) :: u1(l1,m2,n2),  v1(l2,m1,n2),  w1(l2,m2,n1)
  real(8) :: p(l2,m2,n2)

  ! cmwork
  real(8) :: wk1(l1,m1,n1), wk2(l1,m1,n1), wk3(l1,m1,n1)
  real(8) :: dfs(l1,m1,n1)

  ! cmlslz
  real(8) :: zcoef(lm,7,n), zb(lm,n), zx(lm2,n2)

  ! Variables for MPI
  integer :: myrank, nprocs, ierr
  integer :: leftnode, rightnode
  integer, dimension(MPI_STATUS_SIZE) :: istat

  integer :: nstart, nend, nchunk
  integer :: n1start, n1end, n1chunk
  integer :: n2start, n2end, n2chunk

contains
  subroutine initialize_mpi

    ! initialize MPI
    call mpi_init(ierr)
    call mpi_comm_size(MPI_COMM_WORLD, nprocs, ierr)
    call mpi_comm_rank(MPI_COMM_WORLD, myrank, ierr)

    ! initialize Variables for parallel
    if(myrank == 0) then
       leftnode = MPI_PROC_NULL
    else
       leftnode = myrank-1
    end if

    if(myrank == nprocs-1) then
       rightnode = MPI_PROC_NULL
    else
       rightnode = myrank+1
    end if

  end subroutine initialize_mpi

  subroutine set_distribution

    ! for n
    nstart = n * myrank / nprocs + 1
    nend = n * (myrank+1) / nprocs

    if(myrank == nprocs-1) then
       nchunk = n / nprocs + mod(n, nprocs)
    else
       nchunk = n / nprocs
    end if

    ! for n1
    n1start = n1 * myrank / nprocs + 1
    n1end = n1 * (myrank+1) / nprocs

    if(myrank == nprocs-1) then
       n1chunk = n1 / nprocs + mod(n1, nprocs)
    else
       n1chunk = n1 / nprocs
    end if

    ! for n2
    n2start = n2 * myrank / nprocs + 1
    n2end = n2 * (myrank+1) / nprocs

    if(myrank == nprocs-1) then
       n2chunk = n2 / nprocs + mod(n2, nprocs)
    else
       n2chunk = n2 / nprocs
    end if

  end subroutine set_distribution

  subroutine finalize_mpi

    call mpi_finalize(ierr)

  end subroutine finalize_mpi


end module cmmod