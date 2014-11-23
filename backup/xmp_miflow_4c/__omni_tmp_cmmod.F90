MODULE cmmod
 INTEGER :: XMP_DESC_p_off_2

 INTEGER :: XMP_DESC_p_off_1
# 19 "cmmod.f90"
 INTEGER :: maxitr
# 10 "cmmod.f90"
 REAL ( KIND= 8 ) :: odx2
# 15 "cmmod.f90"
 REAL ( KIND= 8 ) :: dfzore
 LOGICAL , PRIVATE , SAVE :: xmpf_init_flag
 INTEGER :: XMP_DESC_p_off_0
# 23 "cmmod.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__w1 ( : , : , : )
 INTEGER ( KIND= 8 ) :: XMP_DESC_u
 INTEGER ( KIND= 8 ) :: XMP_DESC_t

 INTEGER ( KIND= 8 ) :: XMP_DESC_w
 INTEGER ( KIND= 8 ) :: XMP_DESC_v
 INTEGER :: XMP_DESC_zb_size_0
 INTEGER ( KIND= 8 ) :: XMP_DESC_n
 INTEGER ( KIND= 8 ) :: XMP_DESC_p
# 11 "cmmod.f90"
 REAL ( KIND= 8 ) :: ylen

 INTEGER :: XMP_DESC_p_size_0
 INTEGER :: XMP_DESC_p_size_1
 INTEGER :: XMP_DESC_p_size_2
 INTEGER :: XMP_DESC_wk3_off_1
# 10 "cmmod.f90"
 REAL ( KIND= 8 ) :: ody2
 INTEGER :: XMP_DESC_wk3_off_0
 INTEGER :: XMP_DESC_v_size_1
 INTEGER :: XMP_DESC_zcoef_size_1
# 27 "cmmod.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__wk3 ( : , : , : )
 INTEGER :: XMP_DESC_v_size_2
 INTEGER :: XMP_DESC_wk3_off_2
 INTEGER :: XMP_DESC_zcoef_size_0
 INTEGER :: XMP_DESC_dfs_size_2
 INTEGER :: XMP_DESC_v_size_0
 INTEGER :: XMP_DESC_zcoef_size_2
# 23 "cmmod.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__v1 ( : , : , : )
# 14 "cmmod.f90"
 REAL ( KIND= 8 ) :: odtody
 INTEGER :: XMP_DESC_wk3_size_2
 INTEGER :: XMP_DESC_dfs_size_0
# 14 "cmmod.f90"
 REAL ( KIND= 8 ) :: odtodx
 INTEGER :: XMP_DESC_dfs_size_1
 INTEGER :: XMP_DESC_dfs_off_2
# 27 "cmmod.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__wk2 ( : , : , : )
 INTEGER :: XMP_DESC_wk3_size_0
 INTEGER :: XMP_DESC_dfs_off_1
# 27 "cmmod.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__wk1 ( : , : , : )
# 14 "cmmod.f90"
 REAL ( KIND= 8 ) :: odtodz
 INTEGER :: XMP_DESC_wk3_size_1
 INTEGER :: XMP_DESC_dfs_off_0

 INTEGER :: XMP_DESC_zx_off_1
 INTEGER :: XMP_DESC_zx_off_0
 INTEGER ( KIND= 8 ) :: XMP_DESC_zcoef
 INTEGER :: XMP_DESC_zcoef_off_2
# 19 "cmmod.f90"
 INTEGER :: lpend
# 10 "cmmod.f90"
 REAL ( KIND= 8 ) :: dt



# 10 "cmmod.f90"
 REAL ( KIND= 8 ) :: dx
 INTEGER :: XMP_DESC_zcoef_off_0
# 10 "cmmod.f90"
 REAL ( KIND= 8 ) :: dy
 INTEGER :: XMP_DESC_zcoef_off_1
# 10 "cmmod.f90"
 REAL ( KIND= 8 ) :: dz
# 31 "cmmod.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__zcoef ( : , : , : )
# 10 "cmmod.f90"
 REAL ( KIND= 8 ) :: ore
# 23 "cmmod.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__u1 ( : , : , : )
# 11 "cmmod.f90"
 REAL ( KIND= 8 ) :: uinit

 INTEGER :: XMP_DESC_v1_off_0
 INTEGER :: XMP_DESC_wk2_off_1
 INTEGER :: XMP_DESC_v1_off_1
 INTEGER :: XMP_DESC_wk2_off_2
# 22 "cmmod.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__w ( : , : , : )
# 11 "cmmod.f90"
 REAL ( KIND= 8 ) :: s1omg

 INTEGER :: XMP_DESC_wk2_off_0
# 22 "cmmod.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__u ( : , : , : )

# 22 "cmmod.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__v ( : , : , : )
# 19 "cmmod.f90"
 INTEGER :: lmnctr
# 34 "cmmod.f90"
 INTEGER :: myrank
 INTEGER :: XMP_DESC_w1_size_0
 INTEGER :: XMP_DESC_w1_size_2
# 24 "cmmod.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__p ( : , : , : )
 INTEGER :: XMP_DESC_w1_size_1
 INTEGER ( KIND= 8 ) :: XMP_DESC_dfs
 INTEGER ( KIND= 8 ) :: XMP_DESC_zx
 INTEGER :: XMP_DESC_v1_off_2

 INTEGER ( KIND= 8 ) :: XMP_DESC_zb
# 4 "cmmod.f90"
 INTEGER , PARAMETER :: n = 32
# 4 "cmmod.f90"
 INTEGER , PARAMETER :: l = 101
# 4 "cmmod.f90"
 INTEGER , PARAMETER :: m = 5
# 31 "cmmod.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__zb ( : , : )
# 31 "cmmod.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__zx ( : , : )

# 6 "cmmod.f90"
 INTEGER , PARAMETER :: l2 = l + 2
 INTEGER :: XMP_DESC_wk1_off_1
 INTEGER :: XMP_DESC_wk1_off_0
# 19 "cmmod.f90"
 INTEGER :: lpbgn
# 6 "cmmod.f90"
 INTEGER , PARAMETER :: l1 = l + 1
 INTEGER :: XMP_DESC_wk1_off_2
 INTEGER :: XMP_DESC_wk1_size_2
 INTEGER :: XMP_DESC_zx_size_1
 INTEGER :: XMP_DESC_zx_size_0
 INTEGER ( KIND= 8 ) :: XMP_DESC_v1
# 11 "cmmod.f90"
 REAL ( KIND= 8 ) :: xlen
# 15 "cmmod.f90"
 REAL ( KIND= 8 ) :: dfxore
# 28 "cmmod.f90"
 REAL ( KIND= 8 ) , ALLOCATABLE :: XMP__dfs ( : , : , : )

 INTEGER :: XMP_DESC_u_size_2
 INTEGER :: XMP_DESC_u1_off_2
 INTEGER :: XMP_DESC_u1_size_2
 INTEGER :: XMP_DESC_u1_off_1
 INTEGER :: XMP_DESC_u1_size_1
 INTEGER :: XMP_DESC_u1_size_0
# 6 "cmmod.f90"
 INTEGER , PARAMETER :: m1 = m + 1
# 19 "cmmod.f90"
 INTEGER :: lmctr
# 6 "cmmod.f90"
 INTEGER , PARAMETER :: m2 = m + 2
# 11 "cmmod.f90"
 REAL ( KIND= 8 ) :: zlen
 INTEGER :: XMP_DESC_u1_off_0
# 11 "cmmod.f90"
 REAL ( KIND= 8 ) :: omega
# 10 "cmmod.f90"
 REAL ( KIND= 8 ) :: odt
 INTEGER :: XMP_DESC_u_size_1
 INTEGER ( KIND= 8 ) :: XMP_DESC_u1
 INTEGER :: XMP_DESC_u_size_0
# 11 "cmmod.f90"
 REAL ( KIND= 8 ) :: eps
 INTEGER :: XMP_DESC_zb_off_1
# 34 "cmmod.f90"
 INTEGER :: nprocs
 INTEGER :: XMP_DESC_zb_off_0
# 19 "cmmod.f90"
 INTEGER :: linner
 INTEGER :: XMP_DESC_v1_size_2
 INTEGER :: XMP_DESC_v1_size_0
# 35 "cmmod.f90"
 INTEGER :: dist ( 1 : 4 ) = (/ 9 , 8 , 8 , 9 /)
 INTEGER :: XMP_DESC_v1_size_1

 INTEGER :: XMP_DESC_w_off_0
 INTEGER :: XMP_DESC_w_off_1

# 11 "cmmod.f90"
 REAL ( KIND= 8 ) :: odz2
# 10 "cmmod.f90"
 REAL ( KIND= 8 ) :: re
# 10 "cmmod.f90"
 REAL ( KIND= 8 ) :: odz
# 10 "cmmod.f90"
 REAL ( KIND= 8 ) :: ody
# 14 "cmmod.f90"
 REAL ( KIND= 8 ) :: dtodx
# 14 "cmmod.f90"
 REAL ( KIND= 8 ) :: dtody
 INTEGER :: XMP_DESC_w_size_1
 INTEGER :: XMP_DESC_wk2_size_2
# 14 "cmmod.f90"
 REAL ( KIND= 8 ) :: dtodz
 INTEGER :: XMP_DESC_u_off_2
 INTEGER :: XMP_DESC_w_size_0
 INTEGER :: XMP_DESC_wk2_size_1
# 10 "cmmod.f90"
 REAL ( KIND= 8 ) :: odx
 INTEGER :: XMP_DESC_u_off_1
 INTEGER :: XMP_DESC_wk2_size_0
# 19 "cmmod.f90"
 INTEGER :: nctr
# 15 "cmmod.f90"
 REAL ( KIND= 8 ) :: dfyore
 INTEGER :: XMP_DESC_u_off_0
 INTEGER :: XMP_DESC_w_size_2
 INTEGER :: XMP_DESC_w_off_2
 INTEGER :: XMP_DESC_v_off_2
 INTEGER :: XMP_DESC_v_off_1
 INTEGER :: XMP_DESC_v_off_0
 INTEGER ( KIND= 8 ) :: XMP_DESC_wk1
# 15 "cmmod.f90"
 REAL ( KIND= 8 ) :: cdt4dy
 INTEGER ( KIND= 8 ) :: XMP_DESC_wk2
# 15 "cmmod.f90"
 REAL ( KIND= 8 ) :: cdt4dz
 INTEGER ( KIND= 8 ) :: XMP_DESC_wk3
# 15 "cmmod.f90"
 REAL ( KIND= 8 ) :: cdt4dx
 INTEGER ( KIND= 8 ) :: XMP_DESC_w1
# 16 "cmmod.f90"
 REAL ( KIND= 8 ) :: cdt2dx
# 16 "cmmod.f90"
 REAL ( KIND= 8 ) :: cdt2dz
# 16 "cmmod.f90"
 REAL ( KIND= 8 ) :: cdt2dy

 INTEGER :: XMP_DESC_wk1_size_1
 INTEGER :: XMP_DESC_wk1_size_0
 INTEGER :: XMP_DESC_zb_size_1
 INTEGER :: XMP_DESC_w1_off_0
 INTEGER :: XMP_DESC_w1_off_2
 INTEGER :: XMP_DESC_w1_off_1
# 6 "cmmod.f90"
 INTEGER , PARAMETER :: n1 = n + 1
# 7 "cmmod.f90"
 INTEGER , PARAMETER :: lm = l * m
# 6 "cmmod.f90"
 INTEGER , PARAMETER :: n2 = n + 2
# 7 "cmmod.f90"
 INTEGER , PARAMETER :: lmn = l * m * n
# 7 "cmmod.f90"
 INTEGER , PARAMETER :: lm2 = lm + 2 * l

CONTAINS
 SUBROUTINE cmmod_xmpf_module_init_ ( )

  CALL xmpf_nodes_alloc_ ( XMP_DESC_n , 1 )
  CALL xmpf_nodes_dim_size_ ( XMP_DESC_n , 0 , 4 )
  CALL xmpf_nodes_init_GLOBAL_ ( XMP_DESC_n )
  CALL xmpf_template_alloc_ ( XMP_DESC_t , 1 , 1 )
  CALL xmpf_template_dim_info_ ( XMP_DESC_t , 0 , 1 , 34 , 104 , dist )
  CALL xmpf_template_init_ ( XMP_DESC_t , XMP_DESC_n )
  CALL xmpf_array_alloc_ ( XMP_DESC_u , 3 , 514 , XMP_DESC_t )
  CALL xmpf_align_info_ ( XMP_DESC_u , 0 , 1 , l1 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_u , 1 , 1 , m2 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_u , 2 , 1 , n2 , 0 , 0 )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_u , 0 , (-1) , (-1) )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_u , 1 , (-1) , (-1) )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_u , 2 , 1 , 1 )
  CALL xmpf_array_init_ ( XMP_DESC_u )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_u , 0 , XMP_DESC_u_size_0 , XMP_DESC_u_off_0 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_u , 1 , XMP_DESC_u_size_1 , XMP_DESC_u_off_1 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_u , 2 , XMP_DESC_u_size_2 , XMP_DESC_u_off_2 )
  IF ( ( .NOT. allocated ( XMP__u ) ) ) THEN
   ALLOCATE ( XMP__u ( 1 : XMP_DESC_u_size_0 , 1 : XMP_DESC_u_size_1 , 0 : XMP_DESC_u_size_2 - 1 ) )
  END IF
  CALL xmpf_array_set_local_array_ ( XMP_DESC_u , XMP__u )
  CALL xmpf_array_alloc_ ( XMP_DESC_v , 3 , 514 , XMP_DESC_t )
  CALL xmpf_align_info_ ( XMP_DESC_v , 0 , 1 , l2 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_v , 1 , 1 , m1 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_v , 2 , 1 , n2 , 0 , 0 )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_v , 0 , (-1) , (-1) )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_v , 1 , (-1) , (-1) )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_v , 2 , 1 , 1 )
  CALL xmpf_array_init_ ( XMP_DESC_v )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_v , 0 , XMP_DESC_v_size_0 , XMP_DESC_v_off_0 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_v , 1 , XMP_DESC_v_size_1 , XMP_DESC_v_off_1 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_v , 2 , XMP_DESC_v_size_2 , XMP_DESC_v_off_2 )
  IF ( ( .NOT. allocated ( XMP__v ) ) ) THEN
   ALLOCATE ( XMP__v ( 1 : XMP_DESC_v_size_0 , 1 : XMP_DESC_v_size_1 , 0 : XMP_DESC_v_size_2 - 1 ) )
  END IF
  CALL xmpf_array_set_local_array_ ( XMP_DESC_v , XMP__v )
  CALL xmpf_array_alloc_ ( XMP_DESC_w , 3 , 514 , XMP_DESC_t )
  CALL xmpf_align_info_ ( XMP_DESC_w , 0 , 1 , l2 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_w , 1 , 1 , m2 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_w , 2 , 1 , n1 , 0 , 0 )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_w , 0 , (-1) , (-1) )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_w , 1 , (-1) , (-1) )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_w , 2 , 1 , 1 )
  CALL xmpf_array_init_ ( XMP_DESC_w )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_w , 0 , XMP_DESC_w_size_0 , XMP_DESC_w_off_0 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_w , 1 , XMP_DESC_w_size_1 , XMP_DESC_w_off_1 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_w , 2 , XMP_DESC_w_size_2 , XMP_DESC_w_off_2 )
  IF ( ( .NOT. allocated ( XMP__w ) ) ) THEN
   ALLOCATE ( XMP__w ( 1 : XMP_DESC_w_size_0 , 1 : XMP_DESC_w_size_1 , 0 : XMP_DESC_w_size_2 - 1 ) )
  END IF
  CALL xmpf_array_set_local_array_ ( XMP_DESC_w , XMP__w )
  CALL xmpf_array_alloc_ ( XMP_DESC_u1 , 3 , 514 , XMP_DESC_t )
  CALL xmpf_align_info_ ( XMP_DESC_u1 , 0 , 1 , l1 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_u1 , 1 , 1 , m2 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_u1 , 2 , 1 , n2 , 0 , 0 )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_u1 , 0 , (-1) , (-1) )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_u1 , 1 , (-1) , (-1) )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_u1 , 2 , 1 , 1 )
  CALL xmpf_array_init_ ( XMP_DESC_u1 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_u1 , 0 , XMP_DESC_u1_size_0 , XMP_DESC_u1_off_0 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_u1 , 1 , XMP_DESC_u1_size_1 , XMP_DESC_u1_off_1 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_u1 , 2 , XMP_DESC_u1_size_2 , XMP_DESC_u1_off_2 )
  IF ( ( .NOT. allocated ( XMP__u1 ) ) ) THEN
   ALLOCATE ( XMP__u1 ( 1 : XMP_DESC_u1_size_0 , 1 : XMP_DESC_u1_size_1 , 0 : XMP_DESC_u1_size_2 - 1 ) )
  END IF
  CALL xmpf_array_set_local_array_ ( XMP_DESC_u1 , XMP__u1 )
  CALL xmpf_array_alloc_ ( XMP_DESC_v1 , 3 , 514 , XMP_DESC_t )
  CALL xmpf_align_info_ ( XMP_DESC_v1 , 0 , 1 , l2 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_v1 , 1 , 1 , m1 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_v1 , 2 , 1 , n2 , 0 , 0 )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_v1 , 0 , (-1) , (-1) )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_v1 , 1 , (-1) , (-1) )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_v1 , 2 , 1 , 1 )
  CALL xmpf_array_init_ ( XMP_DESC_v1 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_v1 , 0 , XMP_DESC_v1_size_0 , XMP_DESC_v1_off_0 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_v1 , 1 , XMP_DESC_v1_size_1 , XMP_DESC_v1_off_1 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_v1 , 2 , XMP_DESC_v1_size_2 , XMP_DESC_v1_off_2 )
  IF ( ( .NOT. allocated ( XMP__v1 ) ) ) THEN
   ALLOCATE ( XMP__v1 ( 1 : XMP_DESC_v1_size_0 , 1 : XMP_DESC_v1_size_1 , 0 : XMP_DESC_v1_size_2 - 1 ) )
  END IF
  CALL xmpf_array_set_local_array_ ( XMP_DESC_v1 , XMP__v1 )
  CALL xmpf_array_alloc_ ( XMP_DESC_w1 , 3 , 514 , XMP_DESC_t )
  CALL xmpf_align_info_ ( XMP_DESC_w1 , 0 , 1 , l2 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_w1 , 1 , 1 , m2 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_w1 , 2 , 1 , n1 , 0 , 0 )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_w1 , 0 , (-1) , (-1) )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_w1 , 1 , (-1) , (-1) )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_w1 , 2 , 1 , 1 )
  CALL xmpf_array_init_ ( XMP_DESC_w1 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_w1 , 0 , XMP_DESC_w1_size_0 , XMP_DESC_w1_off_0 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_w1 , 1 , XMP_DESC_w1_size_1 , XMP_DESC_w1_off_1 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_w1 , 2 , XMP_DESC_w1_size_2 , XMP_DESC_w1_off_2 )
  IF ( ( .NOT. allocated ( XMP__w1 ) ) ) THEN
   ALLOCATE ( XMP__w1 ( 1 : XMP_DESC_w1_size_0 , 1 : XMP_DESC_w1_size_1 , 0 : XMP_DESC_w1_size_2 - 1 ) )
  END IF
  CALL xmpf_array_set_local_array_ ( XMP_DESC_w1 , XMP__w1 )
  CALL xmpf_array_alloc_ ( XMP_DESC_p , 3 , 514 , XMP_DESC_t )
  CALL xmpf_align_info_ ( XMP_DESC_p , 0 , 1 , l2 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_p , 1 , 1 , m2 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_p , 2 , 1 , n2 , 0 , 0 )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_p , 0 , (-1) , (-1) )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_p , 1 , (-1) , (-1) )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_p , 2 , 1 , 1 )
  CALL xmpf_array_init_ ( XMP_DESC_p )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_p , 0 , XMP_DESC_p_size_0 , XMP_DESC_p_off_0 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_p , 1 , XMP_DESC_p_size_1 , XMP_DESC_p_off_1 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_p , 2 , XMP_DESC_p_size_2 , XMP_DESC_p_off_2 )
  IF ( ( .NOT. allocated ( XMP__p ) ) ) THEN
   ALLOCATE ( XMP__p ( 1 : XMP_DESC_p_size_0 , 1 : XMP_DESC_p_size_1 , 0 : XMP_DESC_p_size_2 - 1 ) )
  END IF
  CALL xmpf_array_set_local_array_ ( XMP_DESC_p , XMP__p )
  CALL xmpf_array_alloc_ ( XMP_DESC_wk1 , 3 , 514 , XMP_DESC_t )
  CALL xmpf_align_info_ ( XMP_DESC_wk1 , 0 , 1 , l1 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_wk1 , 1 , 1 , m1 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_wk1 , 2 , 1 , n1 , 0 , 0 )
  CALL xmpf_array_init_ ( XMP_DESC_wk1 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_wk1 , 0 , XMP_DESC_wk1_size_0 , XMP_DESC_wk1_off_0 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_wk1 , 1 , XMP_DESC_wk1_size_1 , XMP_DESC_wk1_off_1 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_wk1 , 2 , XMP_DESC_wk1_size_2 , XMP_DESC_wk1_off_2 )
  IF ( ( .NOT. allocated ( XMP__wk1 ) ) ) THEN
   ALLOCATE ( XMP__wk1 ( 1 : XMP_DESC_wk1_size_0 , 1 : XMP_DESC_wk1_size_1 , 0 : XMP_DESC_wk1_size_2 - 1 ) )
  END IF
  CALL xmpf_array_set_local_array_ ( XMP_DESC_wk1 , XMP__wk1 )
  CALL xmpf_array_alloc_ ( XMP_DESC_wk2 , 3 , 514 , XMP_DESC_t )
  CALL xmpf_align_info_ ( XMP_DESC_wk2 , 0 , 1 , l1 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_wk2 , 1 , 1 , m1 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_wk2 , 2 , 1 , n1 , 0 , 0 )
  CALL xmpf_array_init_ ( XMP_DESC_wk2 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_wk2 , 0 , XMP_DESC_wk2_size_0 , XMP_DESC_wk2_off_0 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_wk2 , 1 , XMP_DESC_wk2_size_1 , XMP_DESC_wk2_off_1 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_wk2 , 2 , XMP_DESC_wk2_size_2 , XMP_DESC_wk2_off_2 )
  IF ( ( .NOT. allocated ( XMP__wk2 ) ) ) THEN
   ALLOCATE ( XMP__wk2 ( 1 : XMP_DESC_wk2_size_0 , 1 : XMP_DESC_wk2_size_1 , 0 : XMP_DESC_wk2_size_2 - 1 ) )
  END IF
  CALL xmpf_array_set_local_array_ ( XMP_DESC_wk2 , XMP__wk2 )
  CALL xmpf_array_alloc_ ( XMP_DESC_wk3 , 3 , 514 , XMP_DESC_t )
  CALL xmpf_align_info_ ( XMP_DESC_wk3 , 0 , 1 , l1 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_wk3 , 1 , 1 , m1 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_wk3 , 2 , 1 , n1 , 0 , 0 )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_wk3 , 0 , (-1) , (-1) )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_wk3 , 1 , (-1) , (-1) )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_wk3 , 2 , 1 , 1 )
  CALL xmpf_array_init_ ( XMP_DESC_wk3 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_wk3 , 0 , XMP_DESC_wk3_size_0 , XMP_DESC_wk3_off_0 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_wk3 , 1 , XMP_DESC_wk3_size_1 , XMP_DESC_wk3_off_1 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_wk3 , 2 , XMP_DESC_wk3_size_2 , XMP_DESC_wk3_off_2 )
  IF ( ( .NOT. allocated ( XMP__wk3 ) ) ) THEN
   ALLOCATE ( XMP__wk3 ( 1 : XMP_DESC_wk3_size_0 , 1 : XMP_DESC_wk3_size_1 , 0 : XMP_DESC_wk3_size_2 - 1 ) )
  END IF
  CALL xmpf_array_set_local_array_ ( XMP_DESC_wk3 , XMP__wk3 )
  CALL xmpf_array_alloc_ ( XMP_DESC_dfs , 3 , 514 , XMP_DESC_t )
  CALL xmpf_align_info_ ( XMP_DESC_dfs , 0 , 1 , l1 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_dfs , 1 , 1 , m1 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_dfs , 2 , 1 , n1 , 0 , 0 )
  CALL xmpf_array_init_ ( XMP_DESC_dfs )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_dfs , 0 , XMP_DESC_dfs_size_0 , XMP_DESC_dfs_off_0 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_dfs , 1 , XMP_DESC_dfs_size_1 , XMP_DESC_dfs_off_1 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_dfs , 2 , XMP_DESC_dfs_size_2 , XMP_DESC_dfs_off_2 )
  IF ( ( .NOT. allocated ( XMP__dfs ) ) ) THEN
   ALLOCATE ( XMP__dfs ( 1 : XMP_DESC_dfs_size_0 , 1 : XMP_DESC_dfs_size_1 , 0 : XMP_DESC_dfs_size_2 - 1 ) )
  END IF
  CALL xmpf_array_set_local_array_ ( XMP_DESC_dfs , XMP__dfs )
  CALL xmpf_array_alloc_ ( XMP_DESC_zcoef , 3 , 514 , XMP_DESC_t )
  CALL xmpf_align_info_ ( XMP_DESC_zcoef , 0 , 1 , lm , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_zcoef , 1 , 1 , 7 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_zcoef , 2 , 1 , n , 0 , 0 )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_zcoef , 0 , (-1) , (-1) )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_zcoef , 1 , (-1) , (-1) )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_zcoef , 2 , 1 , 1 )
  CALL xmpf_array_init_ ( XMP_DESC_zcoef )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_zcoef , 0 , XMP_DESC_zcoef_size_0 , XMP_DESC_zcoef_off_0 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_zcoef , 1 , XMP_DESC_zcoef_size_1 , XMP_DESC_zcoef_off_1 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_zcoef , 2 , XMP_DESC_zcoef_size_2 , XMP_DESC_zcoef_off_2 )
  IF ( ( .NOT. allocated ( XMP__zcoef ) ) ) THEN
   ALLOCATE ( XMP__zcoef ( 1 : XMP_DESC_zcoef_size_0 , 1 : XMP_DESC_zcoef_size_1 , 0 : XMP_DESC_zcoef_size_2 - 1 ) )
  END IF
  CALL xmpf_array_set_local_array_ ( XMP_DESC_zcoef , XMP__zcoef )
  CALL xmpf_array_alloc_ ( XMP_DESC_zb , 2 , 514 , XMP_DESC_t )
  CALL xmpf_align_info_ ( XMP_DESC_zb , 0 , 1 , lm , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_zb , 1 , 1 , n , 0 , 0 )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_zb , 0 , (-1) , (-1) )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_zb , 1 , 1 , 1 )
  CALL xmpf_array_init_ ( XMP_DESC_zb )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_zb , 0 , XMP_DESC_zb_size_0 , XMP_DESC_zb_off_0 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_zb , 1 , XMP_DESC_zb_size_1 , XMP_DESC_zb_off_1 )
  IF ( ( .NOT. allocated ( XMP__zb ) ) ) THEN
   ALLOCATE ( XMP__zb ( 1 : XMP_DESC_zb_size_0 , 0 : XMP_DESC_zb_size_1 - 1 ) )
  END IF
  CALL xmpf_array_set_local_array_ ( XMP_DESC_zb , XMP__zb )
  CALL xmpf_array_alloc_ ( XMP_DESC_zx , 2 , 514 , XMP_DESC_t )
  CALL xmpf_align_info_ ( XMP_DESC_zx , 0 , 1 , lm2 , (-1) , 0 )
  CALL xmpf_align_info_ ( XMP_DESC_zx , 1 , 1 , n2 , 0 , 0 )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_zx , 0 , (-1) , (-1) )
  CALL xmpf_array_init_shadow_ ( XMP_DESC_zx , 1 , 1 , 1 )
  CALL xmpf_array_init_ ( XMP_DESC_zx )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_zx , 0 , XMP_DESC_zx_size_0 , XMP_DESC_zx_off_0 )
  CALL xmpf_array_get_local_size_off_ ( XMP_DESC_zx , 1 , XMP_DESC_zx_size_1 , XMP_DESC_zx_off_1 )
  IF ( ( .NOT. allocated ( XMP__zx ) ) ) THEN
   ALLOCATE ( XMP__zx ( 1 : XMP_DESC_zx_size_0 , 0 : XMP_DESC_zx_size_1 - 1 ) )
  END IF
  CALL xmpf_array_set_local_array_ ( XMP_DESC_zx , XMP__zx )
  xmpf_init_flag = .TRUE.
 END SUBROUTINE cmmod_xmpf_module_init_

 SUBROUTINE initialize_xmp ( )
# 61 "cmmod.f90"
  INTEGER :: xmp_num_nodes
# 61 "cmmod.f90"
  INTEGER :: xmp_node_num

# 64 "cmmod.f90"
  myrank = xmp_node_num ( )
# 65 "cmmod.f90"
  nprocs = xmp_num_nodes ( )
 99999 &
  CONTINUE
 END SUBROUTINE initialize_xmp

END MODULE cmmod

