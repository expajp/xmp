SUBROUTINE xmpf_main ( )
 INTEGER :: XMP_loop_ub24
 EXTERNAL xmpf_template_dim_info_
 INTEGER ( KIND= 8 ) :: p2
# 12 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , PARAMETER :: region_z_upper = 1.0d0
 INTEGER :: XMP_loop_ub21
# 41 "xmp_sor_3d_2.f90"
 INTEGER :: xmp_num_nodes
 INTEGER ( KIND= 8 ) :: XMP_DESC_t
 EXTERNAL xmpf_loop_sched_
 INTEGER :: XMP_loop_lb20
 EXTERNAL xmpf_nodes_init_GLOBAL_
 INTEGER :: XMP_loop_lb23
 INTEGER ( KIND= 8 ) :: XMP_DESC_x
# 16 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , PARAMETER :: border_y_lower = 0.0d0
 INTEGER ( KIND= 8 ) :: XMP_DESC_x_old
 INTEGER ( KIND= 8 ) :: XMP_DESC_p
 INTEGER :: XMP_DESC_b_size_1
 INTEGER :: XMP_DESC_b_off_1
 INTEGER :: XMP_DESC_b_off_0
 EXTERNAL xmpf_nodes_dim_size_
 INTEGER :: XMP_DESC_b_size_0
 INTEGER ( KIND= 8 ) :: XMP_DESC_a_h_x
# 31 "xmp_sor_3d_2.f90"
 INTEGER :: coef_h_y
 INTEGER ( KIND= 8 ) :: XMP_DESC_a_h_y
 INTEGER ( KIND= 8 ) :: XMP_DESC_a_h_z
 INTEGER :: XMP_loop_lb17
# 34 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) :: norm_diff
 INTEGER :: XMP_loop_lb14
 INTEGER ( KIND= 8 ) :: XMP_DESC_x_diff
 EXTERNAL xmpf_ref_set_dim_info_
 LOGICAL :: xmpf_test_task_on_
 EXTERNAL xmpf_test_task_on_
 INTEGER :: XMP_loop_step13
 INTEGER :: XMP_loop_lb11
 EXTERNAL xmpf_align_info_
# 31 "xmp_sor_3d_2.f90"
 INTEGER :: coef_h_x
 INTEGER :: XMP_loop_step19
 INTEGER :: XMP_loop_step16
# 21 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , PARAMETER :: pi = acos ( ( - 1.0d0 ) )
 EXTERNAL xmpf_nodes_alloc_
 EXTERNAL xmpf_array_alloc_
# 30 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__b ( : , : )
 EXTERNAL xmpf_nodes_dealloc_
# 28 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__x_diff ( : , : )
# 10 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , PARAMETER :: region_x_upper = 1.0d0
# 28 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__x ( : , : )
 INTEGER :: XMP_DESC_a_h_y_off_1
 INTEGER :: XMP_loop_step25
 EXTERNAL xmpf_array_init_shadow_
 INTEGER :: XMP_DESC_a_h_y_off_0
 EXTERNAL xmpf_array_dealloc_
 INTEGER :: XMP_loop_step22
# 40 "xmp_sor_3d_2.f90"
 INTEGER :: myrank
 EXTERNAL xmpf_ref_templ_alloc_
# 17 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , PARAMETER :: border_z_lower = 0.0d0
 EXTERNAL xmpf_end_task_
 EXTERNAL xmpf_template_dealloc_
# 15 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , PARAMETER :: border_x_lower = 0.0d0
 EXTERNAL xmpf_reduction_
 INTEGER :: XMP_DESC_a_h_z_off_1
 EXTERNAL xmpf_reflect_
# 5 "xmp_sor_3d_2.f90"
 INTEGER , PARAMETER :: n = 129
 INTEGER :: XMP_DESC_a_h_z_off_0
# 5 "xmp_sor_3d_2.f90"
 INTEGER , PARAMETER :: l = 100
# 5 "xmp_sor_3d_2.f90"
 INTEGER , PARAMETER :: m = 100
# 37 "xmp_sor_3d_2.f90"
 INTEGER :: j
# 37 "xmp_sor_3d_2.f90"
 INTEGER :: i
 EXTERNAL xmpf_ref_nodes_alloc_
# 25 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) :: h_y
# 25 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) :: h_z
 INTEGER :: j10
 INTEGER :: XMP_DESC_x_size_0
 EXTERNAL xmpf_template_alloc_
# 25 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) :: h_x
 INTEGER :: XMP_DESC_x_size_1
# 24 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) :: region_y_length
# 11 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , PARAMETER :: region_y_upper = 1.0d0
 INTEGER :: XMP_DESC_a_h_y_size_1
 INTEGER :: XMP_DESC_x_old_off_1
 INTEGER :: XMP_DESC_x_old_size_0
# 37 "xmp_sor_3d_2.f90"
 INTEGER :: count
 INTEGER :: XMP_DESC_x_old_size_1
 EXTERNAL xmpf_barrier_
 INTEGER :: XMP_DESC_x_off_1
# 41 "xmp_sor_3d_2.f90"
 INTEGER :: xmp_node_num
 INTEGER :: XMP_DESC_x_old_off_0
 INTEGER :: XMP_DESC_a_h_y_size_0
 INTEGER :: XMP_DESC_x_off_0
 EXTERNAL xmpf_array_set_local_array_
# 6 "xmp_sor_3d_2.f90"
 INTEGER , PARAMETER :: mesh = ( l - 1 ) * ( m - 1 ) * ( n - 1 )
# 11 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , PARAMETER :: region_y_lower = 0.0d0
 INTEGER :: XMP_DESC_x_diff_size_0
 INTEGER :: XMP_DESC_x_diff_off_0
 INTEGER :: XMP_DESC_x_diff_size_1
# 28 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__x_old ( : , : )
 INTEGER :: XMP_DESC_a_h_x_off_0
# 7 "xmp_sor_3d_2.f90"
 INTEGER , PARAMETER :: sf = ( l - 1 ) * ( m - 1 )
 INTEGER :: XMP_DESC_a_h_x_off_1
 INTEGER :: XMP_DESC_x_diff_off_1
# 22 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , PARAMETER :: omega = 1.5
# 29 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__a_h_z ( : , : )
# 40 "xmp_sor_3d_2.f90"
 INTEGER :: nprocs
 EXTERNAL xmpf_ref_init_
# 29 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__a_h_x ( : , : )
# 29 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__a_h_y ( : , : )
 INTEGER ( KIND= 8 ) :: t5
 EXTERNAL xmpf_l2g_
 INTEGER ( KIND= 8 ) :: t7
 INTEGER ( KIND= 8 ) :: t9
# 24 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) :: region_z_length
# 10 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , PARAMETER :: region_x_lower = 0.0d0
 INTEGER :: j4
 INTEGER ( KIND= 8 ) :: t3
 INTEGER :: XMP_DESC_a_h_z_size_1
 EXTERNAL xmpf_array_get_local_size_off_
 INTEGER :: XMP_DESC_a_h_z_size_0
# 15 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , PARAMETER :: border_x_upper = 0.0d0
 INTEGER ( KIND= 8 ) :: t0
 INTEGER :: j8
 INTEGER :: j6
 EXTERNAL xmpf_array_init_
# 24 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) :: region_x_length
 INTEGER :: j1
# 34 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) :: norm_x
 EXTERNAL xmpf_ref_set_loop_info_
# 20 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , PARAMETER :: epsilon = 1.000e-08
 INTEGER :: xmpf_local_idx_
 EXTERNAL xmpf_local_idx_
# 27 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) :: a_diag
 INTEGER :: XMP_DESC_a_h_x_size_0
 INTEGER :: XMP_DESC_a_h_x_size_1
 INTEGER :: XMP_loop_ub15
 INTEGER :: XMP_loop_ub18
# 12 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , PARAMETER :: region_z_lower = 0.0d0
 EXTERNAL xmpf_template_init_
 INTEGER :: XMP_loop_ub12
 INTEGER ( KIND= 8 ) :: XMP_DESC_b
# 16 "xmp_sor_3d_2.f90"
 REAL ( KIND= 8 ) , PARAMETER :: border_y_upper = 0.0d0

 CALL xmpf_nodes_alloc_ ( XMP_DESC_p , 1 )
 CALL xmpf_nodes_dim_size_ ( XMP_DESC_p , 0 , 4 )
 CALL xmpf_nodes_init_GLOBAL_ ( XMP_DESC_p )
 CALL xmpf_template_alloc_ ( XMP_DESC_t , 1 , 1 )
 CALL xmpf_template_dim_info_ ( XMP_DESC_t , 0 , 1 , n - 1 , 101 , 0 )
 CALL xmpf_template_init_ ( XMP_DESC_t , XMP_DESC_p )
 CALL xmpf_array_alloc_ ( XMP_DESC_x , 2 , 514 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_x , 0 , 1 , sf , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_x , 1 , 1 , n - 1 , 0 , 0 )
 CALL xmpf_array_init_shadow_ ( XMP_DESC_x , 1 , 1 , 1 )
 CALL xmpf_array_init_ ( XMP_DESC_x )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_x , 0 , XMP_DESC_x_size_0 , XMP_DESC_x_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_x , 1 , XMP_DESC_x_size_1 , XMP_DESC_x_off_1 )
 ALLOCATE ( XMP__x ( 1 : XMP_DESC_x_size_0 , 0 : XMP_DESC_x_size_1 - 1 ) )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_x , XMP__x )
 CALL xmpf_array_alloc_ ( XMP_DESC_x_old , 2 , 514 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_x_old , 0 , 1 , sf , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_x_old , 1 , 1 , n - 1 , 0 , 0 )
 CALL xmpf_array_init_ ( XMP_DESC_x_old )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_x_old , 0 , XMP_DESC_x_old_size_0 , XMP_DESC_x_old_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_x_old , 1 , XMP_DESC_x_old_size_1 , XMP_DESC_x_old_off_1 )
 ALLOCATE ( XMP__x_old ( 1 : XMP_DESC_x_old_size_0 , 0 : XMP_DESC_x_old_size_1 - 1 ) )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_x_old , XMP__x_old )
 CALL xmpf_array_alloc_ ( XMP_DESC_x_diff , 2 , 514 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_x_diff , 0 , 1 , sf , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_x_diff , 1 , 1 , n - 1 , 0 , 0 )
 CALL xmpf_array_init_ ( XMP_DESC_x_diff )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_x_diff , 0 , XMP_DESC_x_diff_size_0 , XMP_DESC_x_diff_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_x_diff , 1 , XMP_DESC_x_diff_size_1 , XMP_DESC_x_diff_off_1 )
 ALLOCATE ( XMP__x_diff ( 1 : XMP_DESC_x_diff_size_0 , 0 : XMP_DESC_x_diff_size_1 - 1 ) )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_x_diff , XMP__x_diff )
 CALL xmpf_array_alloc_ ( XMP_DESC_a_h_x , 2 , 514 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_a_h_x , 0 , 1 , sf , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_a_h_x , 1 , 1 , n - 1 , 0 , 0 )
 CALL xmpf_array_init_ ( XMP_DESC_a_h_x )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_a_h_x , 0 , XMP_DESC_a_h_x_size_0 , XMP_DESC_a_h_x_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_a_h_x , 1 , XMP_DESC_a_h_x_size_1 , XMP_DESC_a_h_x_off_1 )
 ALLOCATE ( XMP__a_h_x ( 1 : XMP_DESC_a_h_x_size_0 , 0 : XMP_DESC_a_h_x_size_1 - 1 ) )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_a_h_x , XMP__a_h_x )
 CALL xmpf_array_alloc_ ( XMP_DESC_a_h_y , 2 , 514 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_a_h_y , 0 , 1 , sf , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_a_h_y , 1 , 1 , n - 1 , 0 , 0 )
 CALL xmpf_array_init_ ( XMP_DESC_a_h_y )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_a_h_y , 0 , XMP_DESC_a_h_y_size_0 , XMP_DESC_a_h_y_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_a_h_y , 1 , XMP_DESC_a_h_y_size_1 , XMP_DESC_a_h_y_off_1 )
 ALLOCATE ( XMP__a_h_y ( 1 : XMP_DESC_a_h_y_size_0 , 0 : XMP_DESC_a_h_y_size_1 - 1 ) )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_a_h_y , XMP__a_h_y )
 CALL xmpf_array_alloc_ ( XMP_DESC_a_h_z , 2 , 514 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_a_h_z , 0 , 1 , sf , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_a_h_z , 1 , 1 , n - 1 , 0 , 0 )
 CALL xmpf_array_init_shadow_ ( XMP_DESC_a_h_z , 1 , 1 , 1 )
 CALL xmpf_array_init_ ( XMP_DESC_a_h_z )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_a_h_z , 0 , XMP_DESC_a_h_z_size_0 , XMP_DESC_a_h_z_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_a_h_z , 1 , XMP_DESC_a_h_z_size_1 , XMP_DESC_a_h_z_off_1 )
 ALLOCATE ( XMP__a_h_z ( 1 : XMP_DESC_a_h_z_size_0 , 0 : XMP_DESC_a_h_z_size_1 - 1 ) )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_a_h_z , XMP__a_h_z )
 CALL xmpf_array_alloc_ ( XMP_DESC_b , 2 , 514 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_b , 0 , 1 , sf , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_b , 1 , 1 , n - 1 , 0 , 0 )
 CALL xmpf_array_init_ ( XMP_DESC_b )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_b , 0 , XMP_DESC_b_size_0 , XMP_DESC_b_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_b , 1 , XMP_DESC_b_size_1 , XMP_DESC_b_off_1 )
 ALLOCATE ( XMP__b ( 1 : XMP_DESC_b_size_0 , 0 : XMP_DESC_b_size_1 - 1 ) )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_b , XMP__b )
# 54 "xmp_sor_3d_2.f90"
 myrank = xmp_node_num ( )
# 55 "xmp_sor_3d_2.f90"
 nprocs = xmp_num_nodes ( )
 WRITE ( unit = * , fmt ="(A,i2,A)" )"Hello! My Rank is " , myrank ,"."
# 60 "xmp_sor_3d_2.f90"
 region_x_length = region_x_upper - region_x_lower
# 61 "xmp_sor_3d_2.f90"
 region_y_length = region_y_upper - region_y_lower
# 62 "xmp_sor_3d_2.f90"
 region_z_length = region_z_upper - region_z_lower
# 63 "xmp_sor_3d_2.f90"
 h_x = region_x_length / l
# 64 "xmp_sor_3d_2.f90"
 h_y = region_y_length / m
# 65 "xmp_sor_3d_2.f90"
 h_z = region_z_length / n
# 68 "xmp_sor_3d_2.f90"
 a_diag = ( - 2.0d0 * ( 1.0d0 / h_x ** ( 2 ) + 1.0d0 / h_y ** ( 2 ) + 1.0d0 / h_z ** ( 2 ) ) )
# 69 "xmp_sor_3d_2.f90"
 XMP__a_h_x = 0.0d0
# 70 "xmp_sor_3d_2.f90"
 XMP__a_h_y = 0.0d0
# 71 "xmp_sor_3d_2.f90"
 XMP__a_h_z = 0.0d0
 CALL xmpf_ref_templ_alloc_ ( t0 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_loop_info_ ( t0 , 0 , 0 , 0 )
 CALL xmpf_ref_init_ ( t0 )
 XMP_loop_lb11 = 1
 XMP_loop_ub12 = n - 1
 XMP_loop_step13 = 1
 CALL xmpf_loop_sched_ ( XMP_loop_lb11 , XMP_loop_ub12 , XMP_loop_step13 , 0 , t0 )
 DO j1 = XMP_loop_lb11 , XMP_loop_ub12 , XMP_loop_step13
  CALL xmpf_l2g_ ( j , j1 , 0 , t0 )
  DO i = 1 , sf , 1
# 76 "xmp_sor_3d_2.f90"
   IF ( mod ( i , l - 1 ) /= 0 ) THEN
# 76 "xmp_sor_3d_2.f90"
    XMP__a_h_x ( i , j1 ) = 1.0d0 / h_x ** ( 2 )
   END IF
# 77 "xmp_sor_3d_2.f90"
   IF ( i <= ( l - 1 ) * ( m - 2 ) ) THEN
# 77 "xmp_sor_3d_2.f90"
    XMP__a_h_y ( i , j1 ) = 1.0d0 / h_y ** ( 2 )
   END IF
# 78 "xmp_sor_3d_2.f90"
   IF ( j /= 0 .AND. j /= n - 1 ) THEN
# 78 "xmp_sor_3d_2.f90"
    XMP__a_h_z ( i , j1 + 1 ) = 1.0d0 / h_z ** ( 2 )
   END IF
  END DO
 END DO
 CALL xmpf_reflect_ ( XMP_DESC_a_h_z )
# 85 "xmp_sor_3d_2.f90"
 XMP__b = 0.0d0
# 86 "xmp_sor_3d_2.f90"
 coef_h_x = 0
# 87 "xmp_sor_3d_2.f90"
 coef_h_y = 0
 CALL xmpf_ref_nodes_alloc_ ( p2 , XMP_DESC_p , 1 )
 CALL xmpf_ref_set_dim_info_ ( p2 , 0 , 1 , 4 , 0 , 0 )
 CALL xmpf_ref_init_ ( p2 )
 IF ( xmpf_test_task_on_ ( p2 ) ) THEN
  DO i = 1 , sf , 1
# 91 "xmp_sor_3d_2.f90"
   coef_h_x = mod ( i - 1 , l - 1 ) + 1
# 92 "xmp_sor_3d_2.f90"
   coef_h_y = ( i - 1 ) / ( l - 1 ) + 1
# 93 "xmp_sor_3d_2.f90"
   XMP__b ( i , xmpf_local_idx_ ( XMP_DESC_b , 1 , n - 1 ) ) = ( - sin ( coef_h_x * h_x * pi ) * sin ( coef_h_y * h_y * pi ) / h_z&
    ** ( 2 ) )
  END DO
  CALL xmpf_end_task_ ( )
 END IF
# 98 "xmp_sor_3d_2.f90"
 count = 0
# 99 "xmp_sor_3d_2.f90"
 norm_diff = 0.0d0
# 100 "xmp_sor_3d_2.f90"
 norm_x = 0.0d0
# 103 "xmp_sor_3d_2.f90"
 XMP__x = 0.0d0
# 104 "xmp_sor_3d_2.f90"
 XMP__x_old = 0.0d0
# 106 "xmp_sor_3d_2.f90"
 IF ( myrank == 1 ) THEN
  WRITE ( unit = * , fmt = * )"epsilon = " , epsilon
 END IF
 CALL xmpf_reflect_ ( XMP_DESC_x )
 DO
  CALL xmpf_ref_templ_alloc_ ( t3 , XMP_DESC_t , 1 )
  CALL xmpf_ref_set_loop_info_ ( t3 , 0 , 0 , 0 )
  CALL xmpf_ref_init_ ( t3 )
  XMP_loop_lb14 = 1
  XMP_loop_ub15 = n - 1
  XMP_loop_step16 = 1
  CALL xmpf_loop_sched_ ( XMP_loop_lb14 , XMP_loop_ub15 , XMP_loop_step16 , 0 , t3 )
  DO j4 = XMP_loop_lb14 , XMP_loop_ub15 , XMP_loop_step16
   DO i = 1 , sf , 1
# 116 "xmp_sor_3d_2.f90"
    XMP__x_old ( i , j4 ) = XMP__x ( i , j4 + 1 )
   END DO
  END DO
  CALL xmpf_ref_templ_alloc_ ( t5 , XMP_DESC_t , 1 )
  CALL xmpf_ref_set_loop_info_ ( t5 , 0 , 0 , 0 )
  CALL xmpf_ref_init_ ( t5 )
  XMP_loop_lb17 = 1
  XMP_loop_ub18 = n - 1
  XMP_loop_step19 = 1
  CALL xmpf_loop_sched_ ( XMP_loop_lb17 , XMP_loop_ub18 , XMP_loop_step19 , 0 , t5 )
  DO j6 = XMP_loop_lb17 , XMP_loop_ub18 , XMP_loop_step19
   DO i = 1 , sf , 1
# 125 "xmp_sor_3d_2.f90"
    XMP__x ( i , j6 + 1 ) = ( XMP__b ( i , j6 ) - XMP__a_h_x ( i - 1 , j6 ) * XMP__x ( i - 1 , j6 + 1 ) - XMP__a_h_x ( i , j6 ) *&
     XMP__x ( i + 1 , j6 + 1 ) - XMP__a_h_y ( i - l + 1 , j6 ) * XMP__x ( i - l + 1 , j6 + 1 ) - XMP__a_h_y ( i , j6 ) * XMP__x (&
     i + l - 1 , j6 + 1 ) - XMP__a_h_z ( i , j6 ) * XMP__x ( i , j6 ) - XMP__a_h_z ( i , j6 + 1 ) * XMP__x ( i , j6 + 2 ) ) * (&
     omega / a_diag ) + ( 1 - omega ) * XMP__x ( i , j6 + 1 )
   END DO
  END DO
  CALL xmpf_reflect_ ( XMP_DESC_x )
  CALL xmpf_ref_templ_alloc_ ( t7 , XMP_DESC_t , 1 )
  CALL xmpf_ref_set_loop_info_ ( t7 , 0 , 0 , 0 )
  CALL xmpf_ref_init_ ( t7 )
  XMP_loop_lb20 = 1
  XMP_loop_ub21 = n - 1
  XMP_loop_step22 = 1
  CALL xmpf_loop_sched_ ( XMP_loop_lb20 , XMP_loop_ub21 , XMP_loop_step22 , 0 , t7 )
  DO j8 = XMP_loop_lb20 , XMP_loop_ub21 , XMP_loop_step22
   DO i = 1 , sf , 1
# 141 "xmp_sor_3d_2.f90"
    XMP__x_diff ( i , j8 ) = XMP__x ( i , j8 + 1 ) - XMP__x_old ( i , j8 )
   END DO
  END DO
  CALL xmpf_ref_templ_alloc_ ( t9 , XMP_DESC_t , 1 )
  CALL xmpf_ref_set_loop_info_ ( t9 , 0 , 0 , 0 )
  CALL xmpf_ref_init_ ( t9 )
  XMP_loop_lb23 = 1
  XMP_loop_ub24 = n - 1
  XMP_loop_step25 = 1
  CALL xmpf_loop_sched_ ( XMP_loop_lb23 , XMP_loop_ub24 , XMP_loop_step25 , 0 , t9 )
  DO j10 = XMP_loop_lb23 , XMP_loop_ub24 , XMP_loop_step25
   DO i = 1 , sf , 1
# 148 "xmp_sor_3d_2.f90"
    norm_diff = norm_diff + XMP__x_diff ( i , j10 ) ** ( 2 )
# 149 "xmp_sor_3d_2.f90"
    norm_x = norm_x + XMP__x ( i , j10 + 1 ) ** ( 2 )
   END DO
  END DO
  CALL xmpf_reduction_ ( norm_diff , 1 , 14 , 300 , 0 )
  CALL xmpf_reduction_ ( norm_x , 1 , 14 , 300 , 0 )
# 153 "xmp_sor_3d_2.f90"
  norm_diff = sqrt ( norm_diff )
# 154 "xmp_sor_3d_2.f90"
  norm_x = sqrt ( norm_x )
# 157 "xmp_sor_3d_2.f90"
  IF ( norm_diff <= epsilon * norm_x ) THEN
# 157 "xmp_sor_3d_2.f90"
   EXIT
  END IF
# 160 "xmp_sor_3d_2.f90"
  count = count + 1
# 163 "xmp_sor_3d_2.f90"
  IF ( myrank == 1 .AND. mod ( count , 500 ) == 0 ) THEN
   WRITE ( unit = * , fmt = * )"iteration: " , count
   WRITE ( unit = * , fmt = * )"relative error = " , norm_diff / norm_x
  END IF
# 169 "xmp_sor_3d_2.f90"
  norm_diff = 0.0d0
# 170 "xmp_sor_3d_2.f90"
  norm_x = 0.0d0
  CALL xmpf_barrier_ ( 0 )
 END DO
 CALL xmpf_barrier_ ( 0 )
# 179 "xmp_sor_3d_2.f90"
 IF ( myrank == 1 ) THEN
  WRITE ( unit = * , fmt = * )"iteration: " , count
 END IF
# 185 "xmp_sor_3d_2.f90"
 IF ( myrank == nprocs / 2 ) THEN
  DO i = 1 , l - 1 , 1
   WRITE ( unit = * , fmt ="(i3, e15.5)" ) i , XMP__x ( ( l - 1 ) * ( ( m - 1 ) / 2 + 1 ) + i , xmpf_local_idx_ ( XMP_DESC_x , 1 ,&
    ( n - 1 ) / 2 ) )
  END DO
 END IF
# 191 "xmp_sor_3d_2.f90"
00100 &
# 191 "xmp_sor_3d_2.f90"
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

