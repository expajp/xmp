SUBROUTINE lsor4c ( l , lm , n , eps , maxitr , XMP__coef , XMP__b , XMP__x , omega , s1omg , myrank , nprocs )

# 4 "lsor4c.f90"
 INTEGER :: maxitr
 INTEGER :: XMP_loop_lb8
 INTEGER :: XMP_DESC_x_off_1
 INTEGER ( KIND= 8 ) :: XMP_DESC_t


 INTEGER :: XMP_DESC_x_off_0
 INTEGER ( KIND= 8 ) :: XMP_DESC_x
 INTEGER ( KIND= 8 ) :: XMP_DESC_coef
# 9 "lsor4c.f90"
 REAL ( KIND= 8 ) :: rtmp

 INTEGER ( KIND= 8 ) :: XMP_DESC_n
 INTEGER :: XMP_DESC_b_size_1
 INTEGER :: XMP_DESC_b_off_1
 INTEGER :: XMP_loop_ub9
 INTEGER :: XMP_DESC_b_off_0

 INTEGER :: XMP_DESC_b_size_0
 LOGICAL :: xmpf_loop_test_skip_
# 8 "lsor4c.f90"
 INTEGER :: kp
# 6 "lsor4c.f90"
 REAL ( KIND= 8 ) :: XMP__coef ( 0 : 1 )
 INTEGER :: XMP_loop_lb14

 INTEGER ( KIND= 8 ) :: XMP_NULL = 0
# 5 "lsor4c.f90"
 REAL ( KIND= 8 ) :: omega
# 5 "lsor4c.f90"
 REAL ( KIND= 8 ) :: eps
 INTEGER :: XMP_loop_step10
# 12 "lsor4c.f90"
 INTEGER :: nprocs

 INTEGER :: XMP_loop_step13

 INTEGER :: XMP_loop_lb11
 INTEGER ( KIND= 8 ) :: t4
 INTEGER ( KIND= 8 ) :: t5

 INTEGER ( KIND= 8 ) :: t6
 INTEGER :: XMP_loop_step16
 INTEGER :: XMP_DESC_coef_off_2
 INTEGER :: XMP_DESC_coef_off_1
# 14 "lsor4c.f90"
 INTEGER :: dist ( 1 : 4 ) = (/ 9 , 8 , 8 , 9 /)
 INTEGER :: XMP_DESC_coef_off_0

# 9 "lsor4c.f90"
 REAL ( KIND= 8 ) :: res

 INTEGER ( KIND= 8 ) :: t2

# 4 "lsor4c.f90"
 INTEGER :: lm
 INTEGER :: XMP_DESC_coef_size_1
 INTEGER ( KIND= 8 ) :: t0
 INTEGER :: XMP_DESC_coef_size_0
 INTEGER :: XMP_DESC_coef_size_2
# 6 "lsor4c.f90"
 REAL ( KIND= 8 ) :: XMP__b ( 0 : 1 )


# 5 "lsor4c.f90"
 REAL ( KIND= 8 ) :: s1omg
# 6 "lsor4c.f90"
 REAL ( KIND= 8 ) :: XMP__x ( 0 : 1 )



# 12 "lsor4c.f90"
 INTEGER :: myrank


 INTEGER :: k3
 INTEGER :: k7
# 9 "lsor4c.f90"
 REAL ( KIND= 8 ) :: xtmp

# 8 "lsor4c.f90"
 INTEGER :: ix
# 4 "lsor4c.f90"
 INTEGER :: n

# 4 "lsor4c.f90"
 INTEGER :: l
 INTEGER :: xmpf_local_idx_
# 8 "lsor4c.f90"
 INTEGER :: k
 INTEGER :: k1
# 8 "lsor4c.f90"
 INTEGER :: ip
 INTEGER :: XMP_loop_ub15

# 9 "lsor4c.f90"
 REAL ( KIND= 8 ) :: bmax

 INTEGER :: XMP_DESC_x_size_0
# 8 "lsor4c.f90"
 INTEGER :: iter
 INTEGER :: XMP_loop_ub12
 INTEGER :: XMP_DESC_x_size_1
 INTEGER ( KIND= 8 ) :: XMP_DESC_b

 CALL xmpf_nodes_alloc_ ( XMP_DESC_n , 1 )
 CALL xmpf_nodes_dim_size_ ( XMP_DESC_n , 0 , 4 )
 CALL xmpf_nodes_init_GLOBAL_ ( XMP_DESC_n )
 CALL xmpf_template_alloc_ ( XMP_DESC_t , 1 , 1 )
 CALL xmpf_template_dim_info_ ( XMP_DESC_t , 0 , 1 , 34 , 104 , dist )
 CALL xmpf_template_init_ ( XMP_DESC_t , XMP_DESC_n )
 CALL xmpf_array_alloc_ ( XMP_DESC_coef , 3 , 514 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_coef , 0 , 1 , lm , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_coef , 1 , 1 , 7 , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_coef , 2 , 1 , n , 0 , 0 )
 CALL xmpf_array_init_shadow_ ( XMP_DESC_coef , 0 , (-1) , (-1) )
 CALL xmpf_array_init_shadow_ ( XMP_DESC_coef , 1 , (-1) , (-1) )
 CALL xmpf_array_init_shadow_ ( XMP_DESC_coef , 2 , 1 , 1 )
 CALL xmpf_array_init_ ( XMP_DESC_coef )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_coef , 0 , XMP_DESC_coef_size_0 , XMP_DESC_coef_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_coef , 1 , XMP_DESC_coef_size_1 , XMP_DESC_coef_off_1 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_coef , 2 , XMP_DESC_coef_size_2 , XMP_DESC_coef_off_2 )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_coef , XMP__coef )
 CALL xmpf_array_alloc_ ( XMP_DESC_b , 2 , 514 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_b , 0 , 1 , lm , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_b , 1 , 1 , n , 0 , 0 )
 CALL xmpf_array_init_shadow_ ( XMP_DESC_b , 0 , (-1) , (-1) )
 CALL xmpf_array_init_shadow_ ( XMP_DESC_b , 1 , 1 , 1 )
 CALL xmpf_array_init_ ( XMP_DESC_b )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_b , 0 , XMP_DESC_b_size_0 , XMP_DESC_b_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_b , 1 , XMP_DESC_b_size_1 , XMP_DESC_b_off_1 )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_b , XMP__b )
 CALL xmpf_array_alloc_ ( XMP_DESC_x , 2 , 514 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_x , 0 , 1 , lm + 2 * l , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_x , 1 , 1 , n + 2 , 0 , 0 )
 CALL xmpf_array_init_shadow_ ( XMP_DESC_x , 0 , (-1) , (-1) )
 CALL xmpf_array_init_shadow_ ( XMP_DESC_x , 1 , 1 , 1 )
 CALL xmpf_array_init_ ( XMP_DESC_x )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_x , 0 , XMP_DESC_x_size_0 , XMP_DESC_x_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_x , 1 , XMP_DESC_x_size_1 , XMP_DESC_x_off_1 )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_x , XMP__x )
# 34 "lsor4c.f90"
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
# 39 "lsor4c.f90"
   bmax = max ( bmax , abs ( XMP__b ( ( k1 + 1 ) * XMP_DESC_b_size_0 + ( ip - 1 ) ) ) )
  END DO
 END DO
 CALL xmpf_ref_templ_alloc_ ( t0 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_dim_info_ ( t0 , 0 , 2 , 1 , n , 1 )
 CALL xmpf_ref_init_ ( t0 )
 CALL xmpf_reduction_ ( bmax , 1 , 514 , 308 , t0 )
 CALL xmpf_reflect_ ( XMP_DESC_coef )
 CALL xmpf_reflect_ ( XMP_DESC_x )
 CALL xmpf_reflect_ ( XMP_DESC_b )
# 47 "lsor4c.f90"
 res = 0.0d0
 CALL xmpf_ref_templ_alloc_ ( t2 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_loop_info_ ( t2 , 0 , 0 , 1 )
 CALL xmpf_ref_init_ ( t2 )
 XMP_loop_lb11 = 1
 XMP_loop_ub12 = n
 XMP_loop_step13 = 1
 CALL xmpf_loop_sched_ ( XMP_loop_lb11 , XMP_loop_ub12 , XMP_loop_step13 , 0 , t2 )
 DO k3 = XMP_loop_lb11 , XMP_loop_ub12 , XMP_loop_step13
  CALL xmpf_l2g_ ( k , k3 , 0 , t2 )
# 52 "lsor4c.f90"
  kp = k + 1
  WRITE ( unit = * , fmt = * )"kp = " , kp ,"myrank = " , myrank
  DO ip = 1 , lm , 1
# 56 "lsor4c.f90"
   ix = ip + l
# 57 "lsor4c.f90"
   rtmp = XMP__b ( ( k3 + 1 ) * XMP_DESC_b_size_0 + ( ip - 1 ) ) - XMP__coef ( ( k3 + 1 ) * XMP_DESC_coef_size_1 *&
    XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp - 1 ) * XMP_DESC_x_size_0 + ( ix - 1 ) )&
    - XMP__coef ( ( ( k3 + 1 ) * XMP_DESC_coef_size_1 + 1 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ (&
    XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - l - 1 ) ) - XMP__coef ( ( ( k3 + 1 ) * XMP_DESC_coef_size_1 + 2 ) *&
    XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 - 1 ) )&
    - XMP__coef ( ( ( k3 + 1 ) * XMP_DESC_coef_size_1 + 3 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ (&
    XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) - XMP__coef ( ( ( k3 + 1 ) * XMP_DESC_coef_size_1 + 4 ) *&
    XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix + 1 - 1 ) )&
    - XMP__coef ( ( ( k3 + 1 ) * XMP_DESC_coef_size_1 + 5 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ (&
    XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix + l - 1 ) ) - XMP__coef ( ( ( k3 + 1 ) * XMP_DESC_coef_size_1 + 6 ) *&
    XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp + 1 ) * XMP_DESC_x_size_0 + ( ix - 1 ) )
# 64 "lsor4c.f90"
   res = max ( res , abs ( rtmp ) )
  END DO
 END DO
 CALL xmpf_ref_templ_alloc_ ( t2 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_dim_info_ ( t2 , 0 , 2 , 1 , n , 1 )
 CALL xmpf_ref_init_ ( t2 )
 CALL xmpf_reduction_ ( res , 1 , 514 , 308 , t2 )
# 69 "lsor4c.f90"
 IF ( bmax /= 0.0 ) THEN
# 69 "lsor4c.f90"
  res = res / bmax
 END IF
# 71 "lsor4c.f90"
 IF ( res < eps ) THEN
# 73 "lsor4c.f90"
  iter = 0
# 74 "lsor4c.f90"
  IF ( myrank == 1 ) THEN
   WRITE ( unit = 6 , fmt = 6000 ) iter , res
  END IF
  GOTO 99999
 END IF
# 80 "lsor4c.f90"
 iter = 0
# 82 "lsor4c.f90"
00010 &
# 82 "lsor4c.f90"
 CONTINUE
# 84 "lsor4c.f90"
 iter = iter + 1
 CALL xmpf_ref_templ_alloc_ ( t4 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_loop_info_ ( t4 , 0 , 0 , 1 )
 CALL xmpf_ref_init_ ( t4 )
 DO k = 1 , n , 2
  IF ( xmpf_loop_test_skip_ ( t4 , 0 , k ) ) THEN
   CYCLE
  END IF
# 91 "lsor4c.f90"
  kp = k + 1
  DO ip = 1 , lm , 2
# 95 "lsor4c.f90"
   ix = ip + l
# 96 "lsor4c.f90"
   xtmp = ( XMP__b ( xmpf_local_idx_ ( XMP_DESC_b , 1 , k ) * XMP_DESC_b_size_0 + ( ip - 1 ) ) - XMP__coef ( xmpf_local_idx_ (&
    XMP_DESC_coef , 2 , k ) * XMP_DESC_coef_size_1 * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x ,&
    1 , kp - 1 ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) - XMP__coef ( ( xmpf_local_idx_ ( XMP_DESC_coef , 2 , k ) *&
    XMP_DESC_coef_size_1 + 1 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) *&
    XMP_DESC_x_size_0 + ( ix - l - 1 ) ) - XMP__coef ( ( xmpf_local_idx_ ( XMP_DESC_coef , 2 , k ) * XMP_DESC_coef_size_1 + 2 ) *&
    XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 - 1 ) )&
    - XMP__coef ( ( xmpf_local_idx_ ( XMP_DESC_coef , 2 , k ) * XMP_DESC_coef_size_1 + 4 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) *&
    XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix + 1 - 1 ) ) - XMP__coef ( ( xmpf_local_idx_ (&
    XMP_DESC_coef , 2 , k ) * XMP_DESC_coef_size_1 + 5 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ (&
    XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix + l - 1 ) ) - XMP__coef ( ( xmpf_local_idx_ ( XMP_DESC_coef , 2 , k ) *&
    XMP_DESC_coef_size_1 + 6 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp + 1 ) *&
    XMP_DESC_x_size_0 + ( ix - 1 ) ) ) / XMP__coef ( ( xmpf_local_idx_ ( XMP_DESC_coef , 2 , k ) * XMP_DESC_coef_size_1 + 3 ) *&
    XMP_DESC_coef_size_0 + ( ip - 1 ) )
# 103 "lsor4c.f90"
   XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) = s1omg * XMP__x ( xmpf_local_idx_ (&
    XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) + omega * xtmp
  END DO
  DO ip = 2 , lm , 2
# 109 "lsor4c.f90"
   ix = ip + l
# 110 "lsor4c.f90"
   xtmp = ( XMP__b ( xmpf_local_idx_ ( XMP_DESC_b , 1 , k ) * XMP_DESC_b_size_0 + ( ip - 1 ) ) - XMP__coef ( xmpf_local_idx_ (&
    XMP_DESC_coef , 2 , k ) * XMP_DESC_coef_size_1 * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x ,&
    1 , kp - 1 ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) - XMP__coef ( ( xmpf_local_idx_ ( XMP_DESC_coef , 2 , k ) *&
    XMP_DESC_coef_size_1 + 1 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) *&
    XMP_DESC_x_size_0 + ( ix - l - 1 ) ) - XMP__coef ( ( xmpf_local_idx_ ( XMP_DESC_coef , 2 , k ) * XMP_DESC_coef_size_1 + 2 ) *&
    XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 - 1 ) )&
    - XMP__coef ( ( xmpf_local_idx_ ( XMP_DESC_coef , 2 , k ) * XMP_DESC_coef_size_1 + 4 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) *&
    XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix + 1 - 1 ) ) - XMP__coef ( ( xmpf_local_idx_ (&
    XMP_DESC_coef , 2 , k ) * XMP_DESC_coef_size_1 + 5 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ (&
    XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix + l - 1 ) ) - XMP__coef ( ( xmpf_local_idx_ ( XMP_DESC_coef , 2 , k ) *&
    XMP_DESC_coef_size_1 + 6 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp + 1 ) *&
    XMP_DESC_x_size_0 + ( ix - 1 ) ) ) / XMP__coef ( ( xmpf_local_idx_ ( XMP_DESC_coef , 2 , k ) * XMP_DESC_coef_size_1 + 3 ) *&
    XMP_DESC_coef_size_0 + ( ip - 1 ) )
# 117 "lsor4c.f90"
   XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) = s1omg * XMP__x ( xmpf_local_idx_ (&
    XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) + omega * xtmp
  END DO
 END DO
 CALL xmpf_reflect_ ( XMP_DESC_x )
 CALL xmpf_ref_templ_alloc_ ( t5 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_loop_info_ ( t5 , 0 , 0 , 1 )
 CALL xmpf_ref_init_ ( t5 )
 DO k = 2 , n , 2
  IF ( xmpf_loop_test_skip_ ( t5 , 0 , k ) ) THEN
   CYCLE
  END IF
# 129 "lsor4c.f90"
  kp = k + 1
  DO ip = 2 , lm , 2
# 133 "lsor4c.f90"
   ix = ip + l
# 134 "lsor4c.f90"
   xtmp = ( XMP__b ( xmpf_local_idx_ ( XMP_DESC_b , 1 , k ) * XMP_DESC_b_size_0 + ( ip - 1 ) ) - XMP__coef ( xmpf_local_idx_ (&
    XMP_DESC_coef , 2 , k ) * XMP_DESC_coef_size_1 * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x ,&
    1 , kp - 1 ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) - XMP__coef ( ( xmpf_local_idx_ ( XMP_DESC_coef , 2 , k ) *&
    XMP_DESC_coef_size_1 + 1 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) *&
    XMP_DESC_x_size_0 + ( ix - l - 1 ) ) - XMP__coef ( ( xmpf_local_idx_ ( XMP_DESC_coef , 2 , k ) * XMP_DESC_coef_size_1 + 2 ) *&
    XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 - 1 ) )&
    - XMP__coef ( ( xmpf_local_idx_ ( XMP_DESC_coef , 2 , k ) * XMP_DESC_coef_size_1 + 4 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) *&
    XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix + 1 - 1 ) ) - XMP__coef ( ( xmpf_local_idx_ (&
    XMP_DESC_coef , 2 , k ) * XMP_DESC_coef_size_1 + 5 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ (&
    XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix + l - 1 ) ) - XMP__coef ( ( xmpf_local_idx_ ( XMP_DESC_coef , 2 , k ) *&
    XMP_DESC_coef_size_1 + 6 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp + 1 ) *&
    XMP_DESC_x_size_0 + ( ix - 1 ) ) ) / XMP__coef ( ( xmpf_local_idx_ ( XMP_DESC_coef , 2 , k ) * XMP_DESC_coef_size_1 + 3 ) *&
    XMP_DESC_coef_size_0 + ( ip - 1 ) )
# 141 "lsor4c.f90"
   XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) = s1omg * XMP__x ( xmpf_local_idx_ (&
    XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) + omega * xtmp
  END DO
  DO ip = 1 , lm , 2
# 147 "lsor4c.f90"
   ix = ip + l
# 148 "lsor4c.f90"
   xtmp = ( XMP__b ( xmpf_local_idx_ ( XMP_DESC_b , 1 , k ) * XMP_DESC_b_size_0 + ( ip - 1 ) ) - XMP__coef ( xmpf_local_idx_ (&
    XMP_DESC_coef , 2 , k ) * XMP_DESC_coef_size_1 * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x ,&
    1 , kp - 1 ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) - XMP__coef ( ( xmpf_local_idx_ ( XMP_DESC_coef , 2 , k ) *&
    XMP_DESC_coef_size_1 + 1 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) *&
    XMP_DESC_x_size_0 + ( ix - l - 1 ) ) - XMP__coef ( ( xmpf_local_idx_ ( XMP_DESC_coef , 2 , k ) * XMP_DESC_coef_size_1 + 2 ) *&
    XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 - 1 ) )&
    - XMP__coef ( ( xmpf_local_idx_ ( XMP_DESC_coef , 2 , k ) * XMP_DESC_coef_size_1 + 4 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) *&
    XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix + 1 - 1 ) ) - XMP__coef ( ( xmpf_local_idx_ (&
    XMP_DESC_coef , 2 , k ) * XMP_DESC_coef_size_1 + 5 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ (&
    XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix + l - 1 ) ) - XMP__coef ( ( xmpf_local_idx_ ( XMP_DESC_coef , 2 , k ) *&
    XMP_DESC_coef_size_1 + 6 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp + 1 ) *&
    XMP_DESC_x_size_0 + ( ix - 1 ) ) ) / XMP__coef ( ( xmpf_local_idx_ ( XMP_DESC_coef , 2 , k ) * XMP_DESC_coef_size_1 + 3 ) *&
    XMP_DESC_coef_size_0 + ( ip - 1 ) )
# 155 "lsor4c.f90"
   XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) = s1omg * XMP__x ( xmpf_local_idx_ (&
    XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) + omega * xtmp
  END DO
 END DO
 CALL xmpf_reflect_ ( XMP_DESC_x )
# 164 "lsor4c.f90"
 res = 0.0d0
 CALL xmpf_ref_templ_alloc_ ( t6 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_loop_info_ ( t6 , 0 , 0 , 1 )
 CALL xmpf_ref_init_ ( t6 )
 XMP_loop_lb14 = 1
 XMP_loop_ub15 = n
 XMP_loop_step16 = 1
 CALL xmpf_loop_sched_ ( XMP_loop_lb14 , XMP_loop_ub15 , XMP_loop_step16 , 0 , t6 )
 DO k7 = XMP_loop_lb14 , XMP_loop_ub15 , XMP_loop_step16
  CALL xmpf_l2g_ ( k , k7 , 0 , t6 )
# 169 "lsor4c.f90"
  kp = k + 1
  DO ip = 1 , lm , 1
# 173 "lsor4c.f90"
   ix = ip + l
# 174 "lsor4c.f90"
   rtmp = XMP__b ( ( k7 + 1 ) * XMP_DESC_b_size_0 + ( ip - 1 ) ) - XMP__coef ( ( k7 + 1 ) * XMP_DESC_coef_size_1 *&
    XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp - 1 ) * XMP_DESC_x_size_0 + ( ix - 1 ) )&
    - XMP__coef ( ( ( k7 + 1 ) * XMP_DESC_coef_size_1 + 1 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ (&
    XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - l - 1 ) ) - XMP__coef ( ( ( k7 + 1 ) * XMP_DESC_coef_size_1 + 2 ) *&
    XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 - 1 ) )&
    - XMP__coef ( ( ( k7 + 1 ) * XMP_DESC_coef_size_1 + 3 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ (&
    XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) - XMP__coef ( ( ( k7 + 1 ) * XMP_DESC_coef_size_1 + 4 ) *&
    XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix + 1 - 1 ) )&
    - XMP__coef ( ( ( k7 + 1 ) * XMP_DESC_coef_size_1 + 5 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ (&
    XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix + l - 1 ) ) - XMP__coef ( ( ( k7 + 1 ) * XMP_DESC_coef_size_1 + 6 ) *&
    XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp + 1 ) * XMP_DESC_x_size_0 + ( ix - 1 ) )
# 181 "lsor4c.f90"
   res = max ( res , abs ( rtmp ) )
  END DO
 END DO
 CALL xmpf_ref_templ_alloc_ ( t6 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_dim_info_ ( t6 , 0 , 2 , 1 , n , 1 )
 CALL xmpf_ref_init_ ( t6 )
 CALL xmpf_reduction_ ( res , 1 , 514 , 308 , t6 )
# 186 "lsor4c.f90"
 res = res / bmax
# 194 "lsor4c.f90"
 IF ( res > eps .AND. iter <= maxitr ) THEN
# 194 "lsor4c.f90"
  GOTO 00010
 END IF
# 196 "lsor4c.f90"
 IF ( myrank == 1 ) THEN
  WRITE ( unit = 6 , fmt = 6000 ) iter , res
 END IF
# 198 "lsor4c.f90"
06000 &
# 198 "lsor4c.f90"
 FORMAT (8x,"== SOR4C ==  ",i5,5x,e15.6)
 GOTO 99999
99999 &
 CONTINUE
 CALL xmpf_nodes_dealloc_ ( XMP_DESC_n )
 CALL xmpf_template_dealloc_ ( XMP_DESC_t )
 CALL xmpf_array_dealloc_ ( XMP_DESC_coef )
 CALL xmpf_array_dealloc_ ( XMP_DESC_b )
 CALL xmpf_array_dealloc_ ( XMP_DESC_x )
END SUBROUTINE lsor4c

