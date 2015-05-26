SUBROUTINE xmpf_main ( )




 INTEGER :: XMP_loop_lb7
# 6 "xmp_bugtest.f90"
 INTEGER , ALLOCATABLE :: XMP__a ( : )

 INTEGER ( KIND= 8 ) :: XMP_REF_t2

 INTEGER ( KIND= 8 ) :: XMP_REF_t0
 INTEGER :: XMP_loop_lb4

 INTEGER :: XMP_DESC_a_size_0
 INTEGER ( KIND= 8 ) :: XMP_DESC_t







 INTEGER ( KIND= 8 ) :: XMP_DESC_p
 INTEGER :: XMP_loop_ub8

 INTEGER :: XMP_DESC_a_blkoff_0
 INTEGER :: XMP_loop_ub5
# 4 "xmp_bugtest.f90"
 INTEGER , PARAMETER :: n = 100
 INTEGER :: XMP_loop_step9
 INTEGER :: XMP_DESC_a_off_0
 INTEGER :: XMP_loop_step6
# 5 "xmp_bugtest.f90"
 INTEGER :: i
 INTEGER ( KIND= 8 ) :: XMP_DESC_a





 INTEGER :: i3
 INTEGER :: i1

 CALL xmpf_nodes_alloc_ ( XMP_DESC_p , 1 )
 CALL xmpf_nodes_dim_size_ ( XMP_DESC_p , 0 , 4 )
 CALL xmpf_nodes_init_GLOBAL_ ( XMP_DESC_p )
 CALL xmpf_template_alloc_ ( XMP_DESC_t , 1 , 1 )
 CALL xmpf_template_dim_info_ ( XMP_DESC_t , 0 , 1 , n , 101 , 0 )
 CALL xmpf_template_init_ ( XMP_DESC_t , XMP_DESC_p )
 CALL xmpf_array_alloc_ ( XMP_DESC_a , 1 , 507 , XMP_DESC_t )
 CALL xmp_f_init_allocated_ ( XMP_DESC_a )
 CALL xmpf_align_info_ ( XMP_DESC_a , 0 , 1 , n , 0 , 0 )
 CALL xmpf_array_init_ ( XMP_DESC_a )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_a , 0 , XMP_DESC_a_size_0 , XMP_DESC_a_off_0 , XMP_DESC_a_blkoff_0 )
 ALLOCATE ( XMP__a ( 0 : XMP_DESC_a_size_0 - 1 ) )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_a , XMP__a )
 CALL xmpf_ref_templ_alloc_ ( XMP_REF_t0 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_loop_info_ ( XMP_REF_t0 , 0 , 0 , 0 )
 CALL xmpf_ref_init_ ( XMP_REF_t0 )
 XMP_loop_lb4 = 1
 XMP_loop_ub5 = n
 XMP_loop_step6 = 1
 CALL xmpf_loop_sched_ ( XMP_loop_lb4 , XMP_loop_ub5 , XMP_loop_step6 , 0 , XMP_REF_t0 )
 DO i1 = XMP_loop_lb4 , XMP_loop_ub5 , 1
  CALL xmpf_l2g_ ( i , i1 , 0 , XMP_REF_t0 )
# 15 "xmp_bugtest.f90"
  XMP__a ( i1 ) = i
# 16 "xmp_bugtest.f90"
  IF ( mod ( i , 10 ) == 0 ) THEN
# 16 "xmp_bugtest.f90"
   XMP__a ( i1 ) = i * 10
  END IF
# 17 "xmp_bugtest.f90"
  IF ( i /= 100 ) THEN
# 17 "xmp_bugtest.f90"
   XMP__a ( i1 ) = XMP__a ( i1 ) + 1000
  END IF
 END DO
 CALL xmpf_ref_templ_alloc_ ( XMP_REF_t2 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_loop_info_ ( XMP_REF_t2 , 0 , 0 , 0 )
 CALL xmpf_ref_init_ ( XMP_REF_t2 )
 XMP_loop_lb7 = 1
 XMP_loop_ub8 = n
 XMP_loop_step9 = 1
 CALL xmpf_loop_sched_ ( XMP_loop_lb7 , XMP_loop_ub8 , XMP_loop_step9 , 0 , XMP_REF_t2 )
 DO i3 = XMP_loop_lb7 , XMP_loop_ub8 , 1
  CALL xmpf_l2g_ ( i , i3 , 0 , XMP_REF_t2 )
  WRITE ( unit = * , fmt ="(2(A,i4))" )"a(" , i ,") = " , XMP__a ( i3 )
 END DO
99999 &
 CONTINUE
 CALL xmpf_nodes_dealloc_ ( XMP_DESC_p )
 CALL xmpf_template_dealloc_ ( XMP_DESC_t )
 CALL xmpf_array_dealloc_ ( XMP_DESC_a )
END SUBROUTINE xmpf_main

