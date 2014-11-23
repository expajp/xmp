# 1 "sample.f90"
MODULE sample_size
# 4 "sample.f90"
 INTEGER , PARAMETER :: mkmax = 512
# 4 "sample.f90"
 INTEGER , PARAMETER :: mimax = 1024
# 4 "sample.f90"
 INTEGER , PARAMETER :: mjmax = 512
END MODULE sample_size

SUBROUTINE xmpf_main ( )
# 9 "sample.f90"
 USE sample_size


 INTEGER :: XMP_loop_lb7
 INTEGER :: XMP_DESC_x_off_2
 INTEGER :: XMP_DESC_x_off_1
 INTEGER :: XMP_loop_lb4
# 16 "sample.f90"
 INTEGER :: xmp_node_num
 INTEGER :: XMP_DESC_y_size_2
 INTEGER ( KIND= 8 ) :: XMP_DESC_t

# 18 "sample.f90"
 REAL ( KIND= kind ( 0.0D0 ) ) :: cpu1
 INTEGER :: XMP_DESC_y_size_0

# 18 "sample.f90"
 REAL ( KIND= kind ( 0.0D0 ) ) :: cpu0
 INTEGER :: XMP_DESC_y_size_1
 INTEGER ( KIND= 8 ) :: XMP_DESC_y
# 18 "sample.f90"
 REAL ( KIND= kind ( 0.0D0 ) ) :: cpu2
 INTEGER ( KIND= 8 ) :: XMP_DESC_x
 INTEGER :: XMP_DESC_x_off_0

 INTEGER ( KIND= 8 ) :: XMP_DESC_n
 INTEGER :: XMP_loop_ub8

 INTEGER :: XMP_loop_ub5
 INTEGER ( KIND= 8 ) :: XMP_NULL = 0
# 17 "sample.f90"
 INTEGER :: xmp_all_num_nodes
# 17 "sample.f90"
 INTEGER :: nprocs


# 18 "sample.f90"
 REAL ( KIND= kind ( 0.0D0 ) ) :: xmp_wtime


 INTEGER ( KIND= 8 ) :: t2


 INTEGER ( KIND= 8 ) :: t0


# 22 "sample.f90"
 REAL , ALLOCATABLE :: XMP__y ( : , : , : )
# 21 "sample.f90"
 REAL , ALLOCATABLE :: XMP__x ( : , : , : )



# 16 "sample.f90"
 INTEGER :: myrank


 INTEGER :: k3
 INTEGER :: XMP_DESC_y_off_2
 INTEGER :: XMP_DESC_y_off_0
 INTEGER :: XMP_DESC_y_off_1

 INTEGER :: XMP_loop_step9
# 13 "sample.f90"
 INTEGER :: j
# 13 "sample.f90"
 INTEGER :: k
 INTEGER :: k1
 INTEGER :: XMP_loop_step6
# 13 "sample.f90"
 INTEGER :: i

 INTEGER :: XMP_DESC_x_size_0

 INTEGER :: XMP_DESC_x_size_2
 INTEGER :: XMP_DESC_x_size_1

 CALL xmpf_nodes_alloc_ ( XMP_DESC_n , 1 )
 CALL xmpf_nodes_dim_size_ ( XMP_DESC_n , 0 , (-1) )
 CALL xmpf_nodes_init_GLOBAL_ ( XMP_DESC_n )
 CALL xmpf_template_alloc_ ( XMP_DESC_t , 1 , 1 )
 CALL xmpf_template_dim_info_ ( XMP_DESC_t , 0 , 0 , mkmax + 1 , 101 , 0 )
 CALL xmpf_template_init_ ( XMP_DESC_t , XMP_DESC_n )
 CALL xmpf_array_alloc_ ( XMP_DESC_x , 3 , 513 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_x , 0 , 1 , mimax , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_x , 1 , 1 , mjmax , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_x , 2 , 0 , mkmax + 1 , 0 , 0 )
 CALL xmpf_array_init_shadow_ ( XMP_DESC_x , 2 , 1 , 1 )
 CALL xmpf_array_init_ ( XMP_DESC_x )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_x , 0 , XMP_DESC_x_size_0 , XMP_DESC_x_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_x , 1 , XMP_DESC_x_size_1 , XMP_DESC_x_off_1 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_x , 2 , XMP_DESC_x_size_2 , XMP_DESC_x_off_2 )
 ALLOCATE ( XMP__x ( 1 : XMP_DESC_x_size_0 , 1 : XMP_DESC_x_size_1 , 0 : XMP_DESC_x_size_2 - 1 ) )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_x , XMP__x )
 CALL xmpf_array_alloc_ ( XMP_DESC_y , 3 , 513 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_y , 0 , 1 , mimax , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_y , 1 , 1 , mjmax , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_y , 2 , 0 , mkmax + 1 , 0 , 0 )
 CALL xmpf_array_init_ ( XMP_DESC_y )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_y , 0 , XMP_DESC_y_size_0 , XMP_DESC_y_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_y , 1 , XMP_DESC_y_size_1 , XMP_DESC_y_off_1 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_y , 2 , XMP_DESC_y_size_2 , XMP_DESC_y_off_2 )
 ALLOCATE ( XMP__y ( 1 : XMP_DESC_y_size_0 , 1 : XMP_DESC_y_size_1 , 0 : XMP_DESC_y_size_2 - 1 ) )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_y , XMP__y )
# 31 "sample.f90"
 XMP__x = 0.0
# 32 "sample.f90"
 XMP__y = 0.0
# 34 "sample.f90"
 myrank = xmp_node_num ( )
# 35 "sample.f90"
 nprocs = xmp_all_num_nodes ( )
 CALL xmpf_barrier_ ( XMP_NULL )
# 39 "sample.f90"
 cpu0 = xmp_wtime ( )
 CALL xmpf_ref_templ_alloc_ ( t0 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_loop_info_ ( t0 , 0 , 0 , 0 )
 CALL xmpf_ref_init_ ( t0 )
 XMP_loop_lb4 = 1
 XMP_loop_ub5 = mkmax
 XMP_loop_step6 = 1
 CALL xmpf_loop_sched_ ( XMP_loop_lb4 , XMP_loop_ub5 , XMP_loop_step6 , 0 , t0 )
 DO k1 = XMP_loop_lb4 , XMP_loop_ub5 , XMP_loop_step6
  CALL xmpf_l2g_ ( k , k1 , 0 , t0 )
  DO j = 1 , mjmax , 1
   DO i = 1 , mimax , 1
# 47 "sample.f90"
    XMP__x ( i , j , k1 + 1 ) = i + j + k
   END DO
  END DO
 END DO
 CALL xmpf_reflect_ ( XMP_DESC_x )
 CALL xmpf_barrier_ ( XMP_NULL )
# 57 "sample.f90"
 cpu1 = xmp_wtime ( )
 CALL xmpf_ref_templ_alloc_ ( t2 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_loop_info_ ( t2 , 0 , 0 , 0 )
 CALL xmpf_ref_init_ ( t2 )
 XMP_loop_lb7 = 1
 XMP_loop_ub8 = mkmax
 XMP_loop_step9 = 1
 CALL xmpf_loop_sched_ ( XMP_loop_lb7 , XMP_loop_ub8 , XMP_loop_step9 , 0 , t2 )
 DO k3 = XMP_loop_lb7 , XMP_loop_ub8 , XMP_loop_step9
  DO j = 1 , mjmax , 1
   DO i = 1 , mimax , 1
# 67 "sample.f90"
    XMP__y ( i , j , k3 ) = XMP__x ( i , j , k3 ) + XMP__x ( i , j , k3 + 2 )
   END DO
  END DO
 END DO
 CALL xmpf_barrier_ ( XMP_NULL )
# 80 "sample.f90"
 cpu2 = xmp_wtime ( )
# 83 "sample.f90"
 IF ( myrank == 1 ) THEN
  WRITE ( unit = * , fmt ="(i3,X,f9.6))" ) nprocs , cpu2 - cpu0
 END IF
99999 &
 CONTINUE
 CALL xmpf_nodes_dealloc_ ( XMP_DESC_n )
 CALL xmpf_template_dealloc_ ( XMP_DESC_t )
 CALL xmpf_array_dealloc_ ( XMP_DESC_x )
 CALL xmpf_array_dealloc_ ( XMP_DESC_y )
END SUBROUTINE xmpf_main

