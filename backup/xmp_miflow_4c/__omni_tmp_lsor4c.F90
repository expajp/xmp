SUBROUTINE lsor4c ( )
# 2 "lsor4c.f90"
 USE cmmod
# 6 "lsor4c.f90"
 REAL ( KIND= 8 ) :: res
 INTEGER ( KIND= 8 ) :: t2
 INTEGER ( KIND= 8 ) :: t0

 INTEGER :: XMP_loop_lb8
# 7 "lsor4c.f90"
 REAL ( KIND= 8 ) :: time4
# 7 "lsor4c.f90"
 REAL ( KIND= 8 ) :: time3
# 7 "lsor4c.f90"
 REAL ( KIND= 8 ) :: time2
# 7 "lsor4c.f90"
 REAL ( KIND= 8 ) :: time1

# 7 "lsor4c.f90"
 REAL ( KIND= 8 ) :: time0


# 8 "lsor4c.f90"
 REAL ( KIND= 8 ) :: time8
# 6 "lsor4c.f90"
 REAL ( KIND= 8 ) :: rtmp
# 8 "lsor4c.f90"
 REAL ( KIND= 8 ) :: time7
# 8 "lsor4c.f90"
 REAL ( KIND= 8 ) :: time6
# 8 "lsor4c.f90"
 REAL ( KIND= 8 ) :: time5
 INTEGER :: k3
 INTEGER :: XMP_loop_ub9
 INTEGER :: k7
# 6 "lsor4c.f90"
 REAL ( KIND= 8 ) :: xtmp

# 5 "lsor4c.f90"
 INTEGER :: ix
 LOGICAL :: xmpf_loop_test_skip_

# 5 "lsor4c.f90"
 INTEGER :: kp
 INTEGER :: XMP_loop_lb14
 INTEGER :: xmpf_local_idx_

 INTEGER ( KIND= 8 ) :: XMP_NULL = 0
# 5 "lsor4c.f90"
 INTEGER :: k
 INTEGER :: k1
# 5 "lsor4c.f90"
 INTEGER :: ip
 INTEGER :: XMP_loop_ub15
 INTEGER :: XMP_loop_step10

 INTEGER :: XMP_loop_step13
# 6 "lsor4c.f90"
 REAL ( KIND= 8 ) :: bmax
 INTEGER :: XMP_loop_lb11
 INTEGER ( KIND= 8 ) :: t4
# 9 "lsor4c.f90"
 REAL ( KIND= 8 ) :: xmp_wtime
 INTEGER ( KIND= 8 ) :: t5
 INTEGER ( KIND= 8 ) :: t6
 INTEGER :: XMP_loop_step16
# 5 "lsor4c.f90"
 INTEGER :: iter
 INTEGER :: XMP_loop_ub12

 CALL xmpf_barrier_ ( XMP_NULL )
# 12 "lsor4c.f90"
 time0 = xmp_wtime ( )
# 15 "lsor4c.f90"
 bmax = 0.0d0
 CALL xmpf_ref_templ_alloc_ ( t0 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_loop_info_ ( t0 , 0 , 0 , 0 )
 CALL xmpf_ref_init_ ( t0 )
 XMP_loop_lb8 = 1
 XMP_loop_ub9 = n
 XMP_loop_step10 = 1
 CALL xmpf_loop_sched_ ( XMP_loop_lb8 , XMP_loop_ub9 , XMP_loop_step10 , 0 , t0 )
 DO k1 = XMP_loop_lb8 , XMP_loop_ub9 , XMP_loop_step10
  DO ip = 1 , lm , 1
# 20 "lsor4c.f90"
   bmax = max ( bmax , abs ( XMP__zb ( ip , k1 + 1 ) ) )
  END DO
 END DO
 CALL xmpf_ref_templ_alloc_ ( t0 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_dim_info_ ( t0 , 0 , 2 , 1 , n , 1 )
 CALL xmpf_ref_init_ ( t0 )
 CALL xmpf_reduction_ ( bmax , 1 , 514 , 308 , t0 )
 CALL xmpf_reflect_ ( XMP_DESC_zcoef )
 CALL xmpf_reflect_ ( XMP_DESC_zx )
 CALL xmpf_reflect_ ( XMP_DESC_zb )
# 28 "lsor4c.f90"
 res = 0.0d0
 CALL xmpf_ref_templ_alloc_ ( t2 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_loop_info_ ( t2 , 0 , 0 , 0 )
 CALL xmpf_ref_init_ ( t2 )
 XMP_loop_lb11 = 2
 XMP_loop_ub12 = n + 1
 XMP_loop_step13 = 1
 CALL xmpf_loop_sched_ ( XMP_loop_lb11 , XMP_loop_ub12 , XMP_loop_step13 , 0 , t2 )
 DO k3 = XMP_loop_lb11 , XMP_loop_ub12 , XMP_loop_step13
  DO ip = 1 , lm , 1
# 37 "lsor4c.f90"
   ix = ip + l
# 38 "lsor4c.f90"
   rtmp = XMP__zb ( ip , k3 ) - XMP__zcoef ( ip , 1 , k3 ) * XMP__zx ( ix , k3 ) - XMP__zcoef ( ip , 2 , k3 ) * XMP__zx ( ix - l ,&
    k3 + 1 ) - XMP__zcoef ( ip , 3 , k3 ) * XMP__zx ( ix - 1 , k3 + 1 ) - XMP__zcoef ( ip , 4 , k3 ) * XMP__zx ( ix , k3 + 1 ) -&
    XMP__zcoef ( ip , 5 , k3 ) * XMP__zx ( ix + 1 , k3 + 1 ) - XMP__zcoef ( ip , 6 , k3 ) * XMP__zx ( ix + l , k3 + 1 ) -&
    XMP__zcoef ( ip , 7 , k3 ) * XMP__zx ( ix , k3 + 2 )
# 45 "lsor4c.f90"
   res = max ( res , abs ( rtmp ) )
  END DO
 END DO
 CALL xmpf_ref_templ_alloc_ ( t2 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_dim_info_ ( t2 , 0 , 2 , 2 , n + 1 , 1 )
 CALL xmpf_ref_init_ ( t2 )
 CALL xmpf_reduction_ ( res , 1 , 514 , 308 , t2 )
# 50 "lsor4c.f90"
 IF ( bmax /= 0.0 ) THEN
# 50 "lsor4c.f90"
  res = res / bmax
 END IF
# 52 "lsor4c.f90"
 IF ( res < eps ) THEN
# 54 "lsor4c.f90"
  iter = 0
# 55 "lsor4c.f90"
  IF ( myrank == 1 ) THEN
   WRITE ( unit = 6 , fmt = 6000 ) iter , res
  END IF
  GOTO 99999
 END IF
 CALL xmpf_barrier_ ( XMP_NULL )
# 61 "lsor4c.f90"
 time2 = xmp_wtime ( )
# 64 "lsor4c.f90"
 iter = 0
# 66 "lsor4c.f90"
00010 &
# 66 "lsor4c.f90"
 CONTINUE
 CALL xmpf_barrier_ ( XMP_NULL )
# 69 "lsor4c.f90"
 time3 = time3 + xmp_wtime ( )
# 71 "lsor4c.f90"
 iter = iter + 1
 CALL xmpf_ref_templ_alloc_ ( t4 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_loop_info_ ( t4 , 0 , 0 , 0 )
 CALL xmpf_ref_init_ ( t4 )
 DO k = 2 , n + 1 , 2
  IF ( xmpf_loop_test_skip_ ( t4 , 0 , k ) ) THEN
   CYCLE
  END IF
  DO ip = 1 , lm , 2
# 83 "lsor4c.f90"
   ix = ip + l
# 84 "lsor4c.f90"
   xtmp = ( XMP__zb ( ip , xmpf_local_idx_ ( XMP_DESC_zb , 1 , k - 1 ) ) - XMP__zcoef ( ip , 1 , xmpf_local_idx_ ( XMP_DESC_zcoef&
    , 2 , k - 1 ) ) * XMP__zx ( ix , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k - 1 ) ) - XMP__zcoef ( ip , 2 , xmpf_local_idx_ (&
    XMP_DESC_zcoef , 2 , k - 1 ) ) * XMP__zx ( ix - l , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k ) ) - XMP__zcoef ( ip , 3 ,&
    xmpf_local_idx_ ( XMP_DESC_zcoef , 2 , k - 1 ) ) * XMP__zx ( ix - 1 , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k ) ) - XMP__zcoef (&
    ip , 5 , xmpf_local_idx_ ( XMP_DESC_zcoef , 2 , k - 1 ) ) * XMP__zx ( ix + 1 , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k ) ) -&
    XMP__zcoef ( ip , 6 , xmpf_local_idx_ ( XMP_DESC_zcoef , 2 , k - 1 ) ) * XMP__zx ( ix + l , xmpf_local_idx_ ( XMP_DESC_zx , 1&
    , k ) ) - XMP__zcoef ( ip , 7 , xmpf_local_idx_ ( XMP_DESC_zcoef , 2 , k - 1 ) ) * XMP__zx ( ix , xmpf_local_idx_ (&
    XMP_DESC_zx , 1 , k + 1 ) ) ) / XMP__zcoef ( ip , 4 , xmpf_local_idx_ ( XMP_DESC_zcoef , 2 , k - 1 ) )
# 91 "lsor4c.f90"
   XMP__zx ( ix , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k ) ) = s1omg * XMP__zx ( ix , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k ) ) +&
    omega * xtmp
  END DO
  DO ip = 2 , lm , 2
# 97 "lsor4c.f90"
   ix = ip + l
# 98 "lsor4c.f90"
   xtmp = ( XMP__zb ( ip , xmpf_local_idx_ ( XMP_DESC_zb , 1 , k - 1 ) ) - XMP__zcoef ( ip , 1 , xmpf_local_idx_ ( XMP_DESC_zcoef&
    , 2 , k - 1 ) ) * XMP__zx ( ix , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k - 1 ) ) - XMP__zcoef ( ip , 2 , xmpf_local_idx_ (&
    XMP_DESC_zcoef , 2 , k - 1 ) ) * XMP__zx ( ix - l , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k ) ) - XMP__zcoef ( ip , 3 ,&
    xmpf_local_idx_ ( XMP_DESC_zcoef , 2 , k - 1 ) ) * XMP__zx ( ix - 1 , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k ) ) - XMP__zcoef (&
    ip , 5 , xmpf_local_idx_ ( XMP_DESC_zcoef , 2 , k - 1 ) ) * XMP__zx ( ix + 1 , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k ) ) -&
    XMP__zcoef ( ip , 6 , xmpf_local_idx_ ( XMP_DESC_zcoef , 2 , k - 1 ) ) * XMP__zx ( ix + l , xmpf_local_idx_ ( XMP_DESC_zx , 1&
    , k ) ) - XMP__zcoef ( ip , 7 , xmpf_local_idx_ ( XMP_DESC_zcoef , 2 , k - 1 ) ) * XMP__zx ( ix , xmpf_local_idx_ (&
    XMP_DESC_zx , 1 , k + 1 ) ) ) / XMP__zcoef ( ip , 4 , xmpf_local_idx_ ( XMP_DESC_zcoef , 2 , k - 1 ) )
# 105 "lsor4c.f90"
   XMP__zx ( ix , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k ) ) = s1omg * XMP__zx ( ix , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k ) ) +&
    omega * xtmp
  END DO
 END DO
 CALL xmpf_barrier_ ( XMP_NULL )
# 112 "lsor4c.f90"
 time4 = time4 + xmp_wtime ( )
 CALL xmpf_reflect_ ( XMP_DESC_zx )
 CALL xmpf_barrier_ ( XMP_NULL )
# 118 "lsor4c.f90"
 time5 = time5 + xmp_wtime ( )
 CALL xmpf_ref_templ_alloc_ ( t5 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_loop_info_ ( t5 , 0 , 0 , 0 )
 CALL xmpf_ref_init_ ( t5 )
 DO k = 3 , n + 1 , 2
  IF ( xmpf_loop_test_skip_ ( t5 , 0 , k ) ) THEN
   CYCLE
  END IF
  DO ip = 2 , lm , 2
# 128 "lsor4c.f90"
   ix = ip + l
# 129 "lsor4c.f90"
   xtmp = ( XMP__zb ( ip , xmpf_local_idx_ ( XMP_DESC_zb , 1 , k - 1 ) ) - XMP__zcoef ( ip , 1 , xmpf_local_idx_ ( XMP_DESC_zcoef&
    , 2 , k - 1 ) ) * XMP__zx ( ix , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k - 1 ) ) - XMP__zcoef ( ip , 2 , xmpf_local_idx_ (&
    XMP_DESC_zcoef , 2 , k - 1 ) ) * XMP__zx ( ix - l , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k ) ) - XMP__zcoef ( ip , 3 ,&
    xmpf_local_idx_ ( XMP_DESC_zcoef , 2 , k - 1 ) ) * XMP__zx ( ix - 1 , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k ) ) - XMP__zcoef (&
    ip , 5 , xmpf_local_idx_ ( XMP_DESC_zcoef , 2 , k - 1 ) ) * XMP__zx ( ix + 1 , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k ) ) -&
    XMP__zcoef ( ip , 6 , xmpf_local_idx_ ( XMP_DESC_zcoef , 2 , k - 1 ) ) * XMP__zx ( ix + l , xmpf_local_idx_ ( XMP_DESC_zx , 1&
    , k ) ) - XMP__zcoef ( ip , 7 , xmpf_local_idx_ ( XMP_DESC_zcoef , 2 , k - 1 ) ) * XMP__zx ( ix , xmpf_local_idx_ (&
    XMP_DESC_zx , 1 , k + 1 ) ) ) / XMP__zcoef ( ip , 4 , xmpf_local_idx_ ( XMP_DESC_zcoef , 2 , k - 1 ) )
# 136 "lsor4c.f90"
   XMP__zx ( ix , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k ) ) = s1omg * XMP__zx ( ix , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k ) ) +&
    omega * xtmp
  END DO
  DO ip = 1 , lm , 2
# 142 "lsor4c.f90"
   ix = ip + l
# 143 "lsor4c.f90"
   xtmp = ( XMP__zb ( ip , xmpf_local_idx_ ( XMP_DESC_zb , 1 , k - 1 ) ) - XMP__zcoef ( ip , 1 , xmpf_local_idx_ ( XMP_DESC_zcoef&
    , 2 , k - 1 ) ) * XMP__zx ( ix , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k - 1 ) ) - XMP__zcoef ( ip , 2 , xmpf_local_idx_ (&
    XMP_DESC_zcoef , 2 , k - 1 ) ) * XMP__zx ( ix - l , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k ) ) - XMP__zcoef ( ip , 3 ,&
    xmpf_local_idx_ ( XMP_DESC_zcoef , 2 , k - 1 ) ) * XMP__zx ( ix - 1 , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k ) ) - XMP__zcoef (&
    ip , 5 , xmpf_local_idx_ ( XMP_DESC_zcoef , 2 , k - 1 ) ) * XMP__zx ( ix + 1 , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k ) ) -&
    XMP__zcoef ( ip , 6 , xmpf_local_idx_ ( XMP_DESC_zcoef , 2 , k - 1 ) ) * XMP__zx ( ix + l , xmpf_local_idx_ ( XMP_DESC_zx , 1&
    , k ) ) - XMP__zcoef ( ip , 7 , xmpf_local_idx_ ( XMP_DESC_zcoef , 2 , k - 1 ) ) * XMP__zx ( ix , xmpf_local_idx_ (&
    XMP_DESC_zx , 1 , k + 1 ) ) ) / XMP__zcoef ( ip , 4 , xmpf_local_idx_ ( XMP_DESC_zcoef , 2 , k - 1 ) )
# 150 "lsor4c.f90"
   XMP__zx ( ix , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k ) ) = s1omg * XMP__zx ( ix , xmpf_local_idx_ ( XMP_DESC_zx , 1 , k ) ) +&
    omega * xtmp
  END DO
 END DO
 CALL xmpf_barrier_ ( XMP_NULL )
# 157 "lsor4c.f90"
 time6 = time6 + xmp_wtime ( )
 CALL xmpf_reflect_ ( XMP_DESC_zx )
 CALL xmpf_barrier_ ( XMP_NULL )
# 163 "lsor4c.f90"
 time7 = time7 + xmp_wtime ( )
# 165 "lsor4c.f90"
 res = 0.0d0
 CALL xmpf_ref_templ_alloc_ ( t6 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_loop_info_ ( t6 , 0 , 0 , 0 )
 CALL xmpf_ref_init_ ( t6 )
 XMP_loop_lb14 = 2
 XMP_loop_ub15 = n + 1
 XMP_loop_step16 = 1
 CALL xmpf_loop_sched_ ( XMP_loop_lb14 , XMP_loop_ub15 , XMP_loop_step16 , 0 , t6 )
 DO k7 = XMP_loop_lb14 , XMP_loop_ub15 , XMP_loop_step16
  DO ip = 1 , lm , 1
# 174 "lsor4c.f90"
   ix = ip + l
# 175 "lsor4c.f90"
   rtmp = XMP__zb ( ip , k7 ) - XMP__zcoef ( ip , 1 , k7 ) * XMP__zx ( ix , k7 ) - XMP__zcoef ( ip , 2 , k7 ) * XMP__zx ( ix - l ,&
    k7 + 1 ) - XMP__zcoef ( ip , 3 , k7 ) * XMP__zx ( ix - 1 , k7 + 1 ) - XMP__zcoef ( ip , 4 , k7 ) * XMP__zx ( ix , k7 + 1 ) -&
    XMP__zcoef ( ip , 5 , k7 ) * XMP__zx ( ix + 1 , k7 + 1 ) - XMP__zcoef ( ip , 6 , k7 ) * XMP__zx ( ix + l , k7 + 1 ) -&
    XMP__zcoef ( ip , 7 , k7 ) * XMP__zx ( ix , k7 + 2 )
# 182 "lsor4c.f90"
   res = max ( res , abs ( rtmp ) )
  END DO
 END DO
 CALL xmpf_ref_templ_alloc_ ( t6 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_dim_info_ ( t6 , 0 , 2 , 2 , n + 1 , 1 )
 CALL xmpf_ref_init_ ( t6 )
 CALL xmpf_reduction_ ( res , 1 , 514 , 308 , t6 )
 CALL xmpf_barrier_ ( XMP_NULL )
# 188 "lsor4c.f90"
 time8 = time8 + xmp_wtime ( )
# 190 "lsor4c.f90"
 res = res / bmax
# 198 "lsor4c.f90"
 IF ( res > eps .AND. iter <= maxitr ) THEN
# 198 "lsor4c.f90"
  GOTO 00010
 END IF
# 200 "lsor4c.f90"
 IF ( myrank == 1 ) THEN
  WRITE ( unit = 6 , fmt = 6000 ) iter , res
 END IF
 CALL xmpf_barrier_ ( XMP_NULL )
# 203 "lsor4c.f90"
 time1 = xmp_wtime ( )
# 205 "lsor4c.f90"
 IF ( myrank == 1 ) THEN
  WRITE ( unit = * , fmt = * )"even board updating sum = " , time4 - time3
  WRITE ( unit = * , fmt = * )"odd board updating sum = " , time6 - time5
  WRITE ( unit = * , fmt = * )"res updating sum = " , time8 - time7
  WRITE ( unit = * , fmt = * )"reflecting sum = " , time5 - time4 + ( time7 - time6 )
  WRITE ( unit = * , fmt = * )
  WRITE ( unit = * , fmt = * )"before iteration = " , time2 - time0
  WRITE ( unit = * , fmt = * )"iteration = " , time1 - time2
  WRITE ( unit = * , fmt = * )"all executing time = " , time1 - time0
 END IF
# 216 "lsor4c.f90"
06000 &
# 216 "lsor4c.f90"
 FORMAT (8x,"== SOR4C ==  ",i5,5x,e15.6)
 GOTO 99999
99999 &
 CONTINUE
END SUBROUTINE lsor4c

