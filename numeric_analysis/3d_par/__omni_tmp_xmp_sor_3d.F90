SUBROUTINE xmpf_main ( )
 EXTERNAL xmpf_template_dim_info_
 INTEGER ( KIND= 8 ) :: p3
 INTEGER ( KIND= 8 ) :: p2
# 11 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , PARAMETER :: region_z_upper = 1.0d0
 INTEGER ( KIND= 8 ) :: XMP_DESC_t
 EXTERNAL xmpf_loop_sched_
 EXTERNAL xmpf_nodes_init_GLOBAL_
 INTEGER ( KIND= 8 ) :: XMP_DESC_x
# 15 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , PARAMETER :: border_y_lower = 0.0d0
 INTEGER ( KIND= 8 ) :: XMP_DESC_x_old
 INTEGER ( KIND= 8 ) :: XMP_DESC_p
 INTEGER :: XMP_DESC_b_size_1
 INTEGER :: XMP_DESC_b_off_1
 INTEGER :: XMP_loop_ub9
 INTEGER :: XMP_DESC_b_off_0
 EXTERNAL xmpf_nodes_dim_size_
 INTEGER :: XMP_DESC_b_size_0
 INTEGER ( KIND= 8 ) :: XMP_DESC_a_h_x
# 37 "xmp_sor_3d.f90"
 INTEGER :: coef_h_y
 INTEGER ( KIND= 8 ) :: XMP_DESC_a_h_y
 INTEGER ( KIND= 8 ) :: XMP_DESC_a_h_z
# 39 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) :: norm_diff
 INTEGER :: XMP_loop_lb14
 INTEGER ( KIND= 8 ) :: XMP_DESC_x_diff
 EXTERNAL xmpf_ref_set_dim_info_
 LOGICAL :: xmpf_test_task_on_
 EXTERNAL xmpf_test_task_on_
 INTEGER :: XMP_loop_step10
 INTEGER :: XMP_loop_step13
 INTEGER :: XMP_loop_lb11
 EXTERNAL xmpf_align_info_
# 37 "xmp_sor_3d.f90"
 INTEGER :: coef_h_x
 INTEGER :: XMP_loop_step16
# 22 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , PARAMETER :: pi = acos ( ( - 1.0d0 ) )
 EXTERNAL xmpf_nodes_alloc_
 EXTERNAL xmpf_array_alloc_
# 36 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__b ( : , : )
 EXTERNAL xmpf_nodes_dealloc_
# 34 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__x_diff ( : , : )
# 9 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , PARAMETER :: region_x_upper = 1.0d0
# 34 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__x ( : , : )
 INTEGER :: XMP_DESC_a_h_y_off_1
 EXTERNAL xmpf_array_init_shadow_
 INTEGER :: XMP_DESC_a_h_y_off_0
 EXTERNAL xmpf_array_dealloc_
# 45 "xmp_sor_3d.f90"
 INTEGER :: myrank
 EXTERNAL xmpf_ref_templ_alloc_
# 16 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , PARAMETER :: border_z_lower = 0.0d0
 EXTERNAL xmpf_end_task_
 EXTERNAL xmpf_template_dealloc_
# 14 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , PARAMETER :: border_x_lower = 0.0d0
 EXTERNAL xmpf_reduction_
 INTEGER :: XMP_DESC_a_h_z_off_1
 EXTERNAL xmpf_reflect_
# 5 "xmp_sor_3d.f90"
 INTEGER , PARAMETER :: n = 127
 INTEGER :: XMP_DESC_a_h_z_off_0
# 5 "xmp_sor_3d.f90"
 INTEGER , PARAMETER :: l = 100
# 5 "xmp_sor_3d.f90"
 INTEGER , PARAMETER :: m = 100
# 41 "xmp_sor_3d.f90"
 INTEGER :: j
# 41 "xmp_sor_3d.f90"
 INTEGER :: i
 EXTERNAL xmpf_ref_nodes_alloc_
# 31 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) :: h_y
# 31 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) :: h_z
 INTEGER :: XMP_DESC_x_size_0
 EXTERNAL xmpf_template_alloc_
# 31 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) :: h_x
 INTEGER :: XMP_DESC_x_size_1
# 30 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) :: region_y_length
 INTEGER :: XMP_DESC_a_h_y_size_1
# 10 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , PARAMETER :: region_y_upper = 1.0d0
 INTEGER :: XMP_DESC_x_old_off_1
 INTEGER :: XMP_DESC_x_old_size_0
# 42 "xmp_sor_3d.f90"
 INTEGER :: count
 INTEGER :: XMP_DESC_x_old_size_1
 EXTERNAL xmpf_barrier_
 INTEGER :: XMP_loop_lb8
 INTEGER :: XMP_DESC_x_off_1
# 46 "xmp_sor_3d.f90"
 INTEGER :: xmp_node_num
 INTEGER :: XMP_DESC_x_old_off_0
 INTEGER :: XMP_DESC_a_h_y_size_0
 INTEGER :: XMP_DESC_x_off_0
 EXTERNAL xmpf_array_set_local_array_
 INTEGER :: XMP_DESC_x_diff_size_0
 INTEGER :: XMP_DESC_x_diff_off_0
# 10 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , PARAMETER :: region_y_lower = 0.0d0
 INTEGER :: XMP_DESC_x_diff_size_1
# 34 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__x_old ( : , : )
 INTEGER :: XMP_DESC_a_h_x_off_0
# 6 "xmp_sor_3d.f90"
 INTEGER , PARAMETER :: sf = ( l - 1 ) * ( m - 1 )
 INTEGER :: XMP_DESC_a_h_x_off_1
 INTEGER :: XMP_DESC_x_diff_off_1
# 28 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , PARAMETER :: omega = 1.5
# 35 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__a_h_z ( : , : )
 EXTERNAL xmpf_ref_init_
# 35 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__a_h_x ( : , : )
# 35 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__a_h_y ( : , : )
 INTEGER ( KIND= 8 ) :: t4
 EXTERNAL xmpf_l2g_
 INTEGER ( KIND= 8 ) :: t6
# 25 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , PARAMETER :: denomi = 1.0d0 / ( exp ( pi ) - exp ( ( - pi ) ) )
# 30 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) :: region_z_length
# 9 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , PARAMETER :: region_x_lower = 0.0d0
 INTEGER :: XMP_DESC_a_h_z_size_1
 EXTERNAL xmpf_array_get_local_size_off_
 INTEGER :: j5
 INTEGER :: XMP_DESC_a_h_z_size_0
 INTEGER ( KIND= 8 ) :: t0
# 14 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , PARAMETER :: border_x_upper = 0.0d0
 INTEGER :: j7
 EXTERNAL xmpf_array_init_
# 30 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) :: region_x_length
 INTEGER :: j1
# 39 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) :: norm_x
 EXTERNAL xmpf_ref_set_loop_info_
# 19 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , PARAMETER :: epsilon = 1.000e-08
 INTEGER :: xmpf_local_idx_
 EXTERNAL xmpf_local_idx_
# 35 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) :: a_diag
 INTEGER :: XMP_DESC_a_h_x_size_0
 INTEGER :: XMP_DESC_a_h_x_size_1
 INTEGER :: XMP_loop_ub15
# 11 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , PARAMETER :: region_z_lower = 0.0d0
 EXTERNAL xmpf_template_init_
 INTEGER :: XMP_loop_ub12
 INTEGER ( KIND= 8 ) :: XMP_DESC_b
# 15 "xmp_sor_3d.f90"
 REAL ( KIND= 8 ) , PARAMETER :: border_y_upper = 0.0d0

 CALL xmpf_nodes_alloc_ ( XMP_DESC_p , 1 )
 CALL xmpf_nodes_dim_size_ ( XMP_DESC_p , 0 , 4 )
 CALL xmpf_nodes_init_GLOBAL_ ( XMP_DESC_p )
 CALL xmpf_template_alloc_ ( XMP_DESC_t , 1 , 1 )
 CALL xmpf_template_dim_info_ ( XMP_DESC_t , 0 , 0 , n , 101 , 0 )
 CALL xmpf_template_init_ ( XMP_DESC_t , XMP_DESC_p )
 CALL xmpf_array_alloc_ ( XMP_DESC_x , 2 , 514 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_x , 0 , ( - l ) + 2 , sf + l - 1 , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_x , 1 , 0 , n , 0 , 0 )
 CALL xmpf_array_init_shadow_ ( XMP_DESC_x , 1 , 1 , 1 )
 CALL xmpf_array_init_ ( XMP_DESC_x )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_x , 0 , XMP_DESC_x_size_0 , XMP_DESC_x_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_x , 1 , XMP_DESC_x_size_1 , XMP_DESC_x_off_1 )
 ALLOCATE ( XMP__x ( ( - l ) + 2 : XMP_DESC_x_size_0 , 0 : XMP_DESC_x_size_1 - 1 ) )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_x , XMP__x )
 CALL xmpf_array_alloc_ ( XMP_DESC_x_old , 2 , 514 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_x_old , 0 , ( - l ) + 2 , sf + l - 1 , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_x_old , 1 , 0 , n , 0 , 0 )
 CALL xmpf_array_init_ ( XMP_DESC_x_old )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_x_old , 0 , XMP_DESC_x_old_size_0 , XMP_DESC_x_old_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_x_old , 1 , XMP_DESC_x_old_size_1 , XMP_DESC_x_old_off_1 )
 ALLOCATE ( XMP__x_old ( ( - l ) + 2 : XMP_DESC_x_old_size_0 , 0 : XMP_DESC_x_old_size_1 - 1 ) )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_x_old , XMP__x_old )
 CALL xmpf_array_alloc_ ( XMP_DESC_x_diff , 2 , 514 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_x_diff , 0 , ( - l ) + 2 , sf + l - 1 , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_x_diff , 1 , 0 , n , 0 , 0 )
 CALL xmpf_array_init_ ( XMP_DESC_x_diff )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_x_diff , 0 , XMP_DESC_x_diff_size_0 , XMP_DESC_x_diff_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_x_diff , 1 , XMP_DESC_x_diff_size_1 , XMP_DESC_x_diff_off_1 )
 ALLOCATE ( XMP__x_diff ( ( - l ) + 2 : XMP_DESC_x_diff_size_0 , 0 : XMP_DESC_x_diff_size_1 - 1 ) )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_x_diff , XMP__x_diff )
 CALL xmpf_array_alloc_ ( XMP_DESC_a_h_x , 2 , 514 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_a_h_x , 0 , 0 , sf , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_a_h_x , 1 , 0 , n , 0 , 0 )
 CALL xmpf_array_init_ ( XMP_DESC_a_h_x )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_a_h_x , 0 , XMP_DESC_a_h_x_size_0 , XMP_DESC_a_h_x_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_a_h_x , 1 , XMP_DESC_a_h_x_size_1 , XMP_DESC_a_h_x_off_1 )
 ALLOCATE ( XMP__a_h_x ( 0 : XMP_DESC_a_h_x_size_0 , 0 : XMP_DESC_a_h_x_size_1 - 1 ) )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_a_h_x , XMP__a_h_x )
 CALL xmpf_array_alloc_ ( XMP_DESC_a_h_y , 2 , 514 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_a_h_y , 0 , ( - l ) + 2 , sf , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_a_h_y , 1 , 0 , n , 0 , 0 )
 CALL xmpf_array_init_ ( XMP_DESC_a_h_y )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_a_h_y , 0 , XMP_DESC_a_h_y_size_0 , XMP_DESC_a_h_y_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_a_h_y , 1 , XMP_DESC_a_h_y_size_1 , XMP_DESC_a_h_y_off_1 )
 ALLOCATE ( XMP__a_h_y ( ( - l ) + 2 : XMP_DESC_a_h_y_size_0 , 0 : XMP_DESC_a_h_y_size_1 - 1 ) )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_a_h_y , XMP__a_h_y )
 CALL xmpf_array_alloc_ ( XMP_DESC_a_h_z , 2 , 514 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_a_h_z , 0 , 1 , sf , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_a_h_z , 1 , 0 , n , 0 , 0 )
 CALL xmpf_array_init_ ( XMP_DESC_a_h_z )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_a_h_z , 0 , XMP_DESC_a_h_z_size_0 , XMP_DESC_a_h_z_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_a_h_z , 1 , XMP_DESC_a_h_z_size_1 , XMP_DESC_a_h_z_off_1 )
 ALLOCATE ( XMP__a_h_z ( 1 : XMP_DESC_a_h_z_size_0 , 0 : XMP_DESC_a_h_z_size_1 - 1 ) )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_a_h_z , XMP__a_h_z )
 CALL xmpf_array_alloc_ ( XMP_DESC_b , 2 , 514 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_b , 0 , 1 , sf , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_b , 1 , 1 , n , 0 , 0 )
 CALL xmpf_array_init_ ( XMP_DESC_b )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_b , 0 , XMP_DESC_b_size_0 , XMP_DESC_b_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_b , 1 , XMP_DESC_b_size_1 , XMP_DESC_b_off_1 )
 ALLOCATE ( XMP__b ( 1 : XMP_DESC_b_size_0 , 0 : XMP_DESC_b_size_1 - 1 ) )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_b , XMP__b )
# 55 "xmp_sor_3d.f90"
 myrank = xmp_node_num ( )
 WRITE ( unit = * , fmt = * )"Hello! My Rank is " , myrank ,"."
# 60 "xmp_sor_3d.f90"
 region_x_length = region_x_upper - region_x_lower
# 61 "xmp_sor_3d.f90"
 region_y_length = region_y_upper - region_y_lower
# 62 "xmp_sor_3d.f90"
 region_z_length = region_z_upper - region_z_lower
# 63 "xmp_sor_3d.f90"
 h_x = region_x_length / l
# 64 "xmp_sor_3d.f90"
 h_y = region_y_length / m
# 65 "xmp_sor_3d.f90"
 h_z = region_z_length / n
# 68 "xmp_sor_3d.f90"
 a_diag = ( - 2.0d0 * ( 1.0d0 / h_x ** ( 2 ) + 1.0d0 / h_y ** ( 2 ) + 1.0d0 / h_z ** ( 2 ) ) )
# 69 "xmp_sor_3d.f90"
 XMP__a_h_x = 0.0d0
# 70 "xmp_sor_3d.f90"
 XMP__a_h_y = 0.0d0
# 71 "xmp_sor_3d.f90"
 XMP__a_h_z = 0.0d0
 CALL xmpf_ref_templ_alloc_ ( t0 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_loop_info_ ( t0 , 0 , 0 , 0 )
 CALL xmpf_ref_init_ ( t0 )
 XMP_loop_lb8 = 1
 XMP_loop_ub9 = n - 1
 XMP_loop_step10 = 1
 CALL xmpf_loop_sched_ ( XMP_loop_lb8 , XMP_loop_ub9 , XMP_loop_step10 , 0 , t0 )
 DO j1 = XMP_loop_lb8 , XMP_loop_ub9 , XMP_loop_step10
  CALL xmpf_l2g_ ( j , j1 , 0 , t0 )
  DO i = 1 , sf , 1
# 76 "xmp_sor_3d.f90"
   IF ( j /= n - 1 ) THEN
# 76 "xmp_sor_3d.f90"
    XMP__a_h_z ( i , j1 ) = 1.0d0 / h_z ** ( 2 )
   END IF
# 77 "xmp_sor_3d.f90"
   IF ( i <= ( l - 1 ) * ( m - 2 ) ) THEN
# 77 "xmp_sor_3d.f90"
    XMP__a_h_y ( i , j1 ) = 1.0d0 / h_y ** ( 2 )
   END IF
# 78 "xmp_sor_3d.f90"
   IF ( mod ( i , l - 1 ) /= 0 ) THEN
# 78 "xmp_sor_3d.f90"
    XMP__a_h_x ( i , j1 ) = 1.0d0 / h_x ** ( 2 )
   END IF
  END DO
 END DO
# 83 "xmp_sor_3d.f90"
 XMP__b = 0.0d0
# 84 "xmp_sor_3d.f90"
 coef_h_x = 0
# 85 "xmp_sor_3d.f90"
 coef_h_y = 0
 CALL xmpf_ref_nodes_alloc_ ( p2 , XMP_DESC_p , 1 )
 CALL xmpf_ref_set_dim_info_ ( p2 , 0 , 1 , 4 , 0 , 0 )
 CALL xmpf_ref_init_ ( p2 )
 IF ( xmpf_test_task_on_ ( p2 ) ) THEN
  DO i = 1 , sf , 1
# 89 "xmp_sor_3d.f90"
   coef_h_x = mod ( i - 1 , l - 1 ) + 1
# 90 "xmp_sor_3d.f90"
   coef_h_y = ( i - 1 ) / ( l - 1 ) + 1
# 91 "xmp_sor_3d.f90"
   XMP__b ( i , xmpf_local_idx_ ( XMP_DESC_b , 1 , n - 1 ) ) = ( - sin ( coef_h_x * h_x * pi ) * sin ( coef_h_y * h_y * pi ) / h_z&
    ** ( 2 ) )
  END DO
  CALL xmpf_end_task_ ( )
 END IF
# 96 "xmp_sor_3d.f90"
 count = 0
# 97 "xmp_sor_3d.f90"
 norm_diff = 0.0d0
# 98 "xmp_sor_3d.f90"
 norm_x = 0.0d0
# 100 "xmp_sor_3d.f90"
 XMP__x = 0.0d0
# 101 "xmp_sor_3d.f90"
 XMP__x_old = 0.0d0
# 103 "xmp_sor_3d.f90"
 IF ( myrank == 0 ) THEN
  WRITE ( unit = * , fmt = * )"epsilon = " , epsilon
 END IF
 CALL xmpf_ref_nodes_alloc_ ( p3 , XMP_DESC_p , 1 )
 CALL xmpf_ref_set_dim_info_ ( p3 , 0 , 1 , 4 , 0 , 0 )
 CALL xmpf_ref_init_ ( p3 )
 IF ( xmpf_test_task_on_ ( p3 ) ) THEN
  WRITE ( unit = * , fmt = * )"x(sf, n-1) = " , XMP__x ( sf , xmpf_local_idx_ ( XMP_DESC_x , 1 , n - 1 ) )
  CALL xmpf_end_task_ ( )
 END IF
 DO
# 110 "xmp_sor_3d.f90"
  XMP__x_old = XMP__x
  CALL xmpf_barrier_ ( 0 )
  CALL xmpf_reflect_ ( XMP_DESC_x )
  CALL xmpf_ref_templ_alloc_ ( t4 , XMP_DESC_t , 1 )
  CALL xmpf_ref_set_loop_info_ ( t4 , 0 , 0 , 0 )
  CALL xmpf_ref_init_ ( t4 )
  XMP_loop_lb11 = 1
  XMP_loop_ub12 = n - 1
  XMP_loop_step13 = 1
  CALL xmpf_loop_sched_ ( XMP_loop_lb11 , XMP_loop_ub12 , XMP_loop_step13 , 0 , t4 )
  DO j5 = XMP_loop_lb11 , XMP_loop_ub12 , XMP_loop_step13
   DO i = 1 , sf , 1
# 121 "xmp_sor_3d.f90"
    XMP__x ( i , j5 + 1 ) = ( XMP__b ( i , j5 - XMP_DESC_b_off_1 ) - XMP__a_h_x ( i - 1 , j5 ) * XMP__x ( i - 1 , j5 + 1 ) -&
     XMP__a_h_x ( i , j5 ) * XMP__x ( i + 1 , j5 + 1 ) - XMP__a_h_y ( i - l + 1 , j5 ) * XMP__x ( i - l + 1 , j5 + 1 ) -&
     XMP__a_h_y ( i , j5 ) * XMP__x ( i + l - 1 , j5 + 1 ) - XMP__a_h_z ( i , j5 + (-1) ) * XMP__x ( i , j5 ) - XMP__a_h_z ( i ,&
     j5 ) * XMP__x ( i , j5 + 2 ) ) * ( omega / a_diag ) + ( 1 - omega ) * XMP__x ( i , j5 + 1 )
   END DO
  END DO
# 132 "xmp_sor_3d.f90"
  XMP__x_diff = XMP__x - XMP__x_old
  CALL xmpf_ref_templ_alloc_ ( t6 , XMP_DESC_t , 1 )
  CALL xmpf_ref_set_loop_info_ ( t6 , 0 , 0 , 0 )
  CALL xmpf_ref_init_ ( t6 )
  XMP_loop_lb14 = 1
  XMP_loop_ub15 = n - 1
  XMP_loop_step16 = 1
  CALL xmpf_loop_sched_ ( XMP_loop_lb14 , XMP_loop_ub15 , XMP_loop_step16 , 0 , t6 )
  DO j7 = XMP_loop_lb14 , XMP_loop_ub15 , XMP_loop_step16
   DO i = 1 , sf , 1
# 137 "xmp_sor_3d.f90"
    norm_diff = norm_diff + XMP__x_diff ( i , j7 ) ** ( 2 )
# 138 "xmp_sor_3d.f90"
    norm_x = norm_x + XMP__x ( i , j7 + 1 ) ** ( 2 )
   END DO
  END DO
  CALL xmpf_reduction_ ( norm_diff , 1 , 14 , 300 , 0 )
  CALL xmpf_reduction_ ( norm_x , 1 , 14 , 300 , 0 )
# 142 "xmp_sor_3d.f90"
  norm_diff = sqrt ( norm_diff )
# 143 "xmp_sor_3d.f90"
  norm_x = sqrt ( norm_x )
# 146 "xmp_sor_3d.f90"
  IF ( norm_diff <= epsilon * norm_x ) THEN
# 146 "xmp_sor_3d.f90"
   EXIT
  END IF
# 149 "xmp_sor_3d.f90"
  count = count + 1
# 152 "xmp_sor_3d.f90"
  IF ( mod ( count , 500 ) == 0 .AND. myrank == 1 ) THEN
   WRITE ( unit = * , fmt = * )"iteration: " , count
   WRITE ( unit = * , fmt = * )"relative error = " , norm_diff / norm_x
  END IF
# 158 "xmp_sor_3d.f90"
  norm_diff = 0.0d0
# 159 "xmp_sor_3d.f90"
  norm_x = 0.0d0
  CALL xmpf_barrier_ ( 0 )
 END DO
 CALL xmpf_barrier_ ( 0 )
# 168 "xmp_sor_3d.f90"
 IF ( myrank == 1 ) THEN
  WRITE ( unit = * , fmt = * )"iteration: " , count
 END IF
# 174 "xmp_sor_3d.f90"
 IF ( myrank == 1 ) THEN
  DO i = 1 , l - 1 , 1
   WRITE ( unit = * , fmt ="(i3, e15.5)" ) i , XMP__x ( ( l - 1 ) * ( ( m - 1 ) / 2 + 1 ) + i , xmpf_local_idx_ ( XMP_DESC_x , 1 ,&
    ( n - 1 ) / 2 ) )
  END DO
 END IF
# 180 "xmp_sor_3d.f90"
00100 &
# 180 "xmp_sor_3d.f90"
 FORMAT (2i4, x, f10.8)
99999 &
 CALL xmpf_nodes_dealloc_ ( XMP_DESC_p )
 CALL xmpf_template_dealloc_ ( XMP_DESC_t )
 CALL xmpf_array_dealloc_ ( XMP_DESC_x )
 CALL xmpf_array_dealloc_ ( XMP_DESC_x_old )
 CALL xmpf_array_dealloc_ ( XMP_DESC_x_diff )
 CALL xmpf_array_dealloc_ ( XMP_DESC_a_h_x )
 CALL xmpf_array_dealloc_ ( XMP_DESC_a_h_y )
 CALL xmpf_array_dealloc_ ( XMP_DESC_a_h_z )
 CALL xmpf_array_dealloc_ ( XMP_DESC_b )
END SUBROUTINE xmpf_main

