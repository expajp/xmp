SUBROUTINE lsor4c ( l , lm , n , eps , maxitr , XMP__coef , XMP__b , XMP__x , omega , s1omg , myrank , nprocs )
 EXTERNAL mpi_type_dup_fn

# 367 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_combiner_subarray = 12
# 68 "mpif-mpi-io.h"
 INTEGER , PARAMETER :: mpi_mode_unique_open = 32
# 239 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_dims = 12
# 411 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_real = 13
# 397 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_comm_world = 0
# 66 "mpif-mpi-io.h"
 INTEGER , PARAMETER :: mpi_mode_create = 1
 EXTERNAL mpi_comm_dup_fn
# 433 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_cxx_float_complex = 55
# 413 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_complex16 = 20
# 92 "mpif-common.h"
 INTEGER , PARAMETER :: impi_client_color = 11
# 399 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_errors_are_fatal = 1
# 85 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_root = (-4)
# 362 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_combiner_hindexed_integer = 7
 INTEGER ( KIND= 8 ) :: XMP_DESC_t

# 68 "mpif-mpi-io.h"
 INTEGER , PARAMETER :: mpi_mode_delete_on_close = 16
# 439 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_c_bool = 73
 INTEGER ( KIND= 8 ) :: XMP_DESC_x
# 101 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_mode_nosucceed = 16
# 426 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_int = 39
# 279 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_unsupported_operation = 52
# 89 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_error = 3
# 11 "lsor4c.f90"
 REAL ( KIND= 8 ) :: rtmp
# 245 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_in_status = 18
# 521 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_lor = 7
 INTEGER ( KIND= 8 ) :: XMP_DESC_n
# 363 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_combiner_hindexed = 8
# 16 "lsor4c.f90"
 INTEGER :: rightnode
 INTEGER :: XMP_DESC_b_size_1
# 249 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_assert = 22
 INTEGER :: XMP_DESC_b_size_0
# 438 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_int64_t = 64
# 356 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_combiner_dup = 1
 INTEGER :: XMP_loop_ub7
# 263 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_keyval = 36
# 67 "mpif-mpi-io.h"
 INTEGER , PARAMETER :: mpi_mode_wronly = 4
# 67 "mpif-mpi-io.h"
 INTEGER , PARAMETER :: mpi_mode_rdonly = 2

# 89 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_tag = 2
# 283 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_lastcode = 54

# 357 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_combiner_contiguous = 2
# 408 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_logical = 6
 INTEGER :: XMP_DESC_coef_off_2
 INTEGER :: XMP_DESC_coef_off_1
 INTEGER :: XMP_DESC_coef_off_0
# 11 "lsor4c.f90"
 REAL ( KIND= 8 ) :: res
# 90 "mpif-config.h"
 INTEGER , PARAMETER :: mpi_status_size = 5
# 92 "mpif-common.h"
 INTEGER , PARAMETER :: impi_client_size = 10
# 185 "mpif-common.h"
 INTEGER :: mpi_errcodes_ignore ( 1 : 1 )
# 73 "mpif-config.h"
 CHARACTER ( LEN= 32 ) , PARAMETER :: ompi_greek_version ="rc4"
# 261 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_info = 34
# 370 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_combiner_f90_complex = 15
# 238 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_topology = 11
# 7 "lsor4c.f90"
 REAL ( KIND= 8 ) :: s1omg
# 166 "mpif-common.h"
 INTEGER :: mpi_unweighted ( 1 : 1 )
 EXTERNAL mpi_type_null_delete_fn
# 415 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_2integer = 25
# 415 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_2double_precision = 24

# 87 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_cart = 1
# 228 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_buffer = 1

# 413 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_complex = 18

# 267 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_not_same = 40
# 252 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_conversion = 25
# 11 "lsor4c.f90"
 REAL ( KIND= 8 ) :: xtmp
# 437 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_int32_t = 62
# 433 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_cxx_bool = 54
# 408 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_character = 5
# 76 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_version = 2

# 99 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_typeclass_complex = 3
# 5 "lsor4c.f90"
 INTEGER :: n
# 407 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_lb = 4
# 5 "lsor4c.f90"
 INTEGER :: l
# 520 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_land = 5
# 10 "lsor4c.f90"
 INTEGER :: k
# 96 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_distribute_cyclic = 1
# 241 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_unknown = 14
# 270 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_port = 43
# 11 "lsor4c.f90"
 REAL ( KIND= 8 ) :: bmax

# 416 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_2double_complex = 27
# 202 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_datatype_null = 0
 EXTERNAL mpi_win_null_delete_fn
# 98 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_typeclass_integer = 1
# 365 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_combiner_struct_integer = 10
# 409 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_integer2 = 9
# 426 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_unsigned = 40
# 409 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_integer1 = 8
# 409 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_integer4 = 10
# 170 "mpif-common.h"
 CHARACTER :: mpi_argv_null ( 1 : 1 )
# 74 "mpif-config.h"
 CHARACTER ( LEN= 32 ) , PARAMETER :: ompi_svn_version ="r3253"
# 434 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_cxx_double_complex = 56
# 427 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_unsigned_long = 42
 INTEGER :: XMP_DESC_x_off_1
# 203 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_request_null = 0
# 255 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_file_exists = 28
# 439 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_offset = 67
# 407 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_byte = 1
# 96 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_distribute_block = 0
# 87 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_dist_graph = 3
# 424 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_signed_char = 36
 INTEGER :: XMP_DESC_x_off_0
 INTEGER ( KIND= 8 ) :: XMP_DESC_coef
# 520 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_max = 1
# 410 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_integer8 = 11
# 364 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_combiner_indexed_block = 9
# 244 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_intern = 17
# 262 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_io = 35
# 231 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_tag = 4
# 414 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_double_complex = 22
 EXTERNAL mpi_dup_fn
# 520 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_sum = 3
# 84 "mpif.h"
 REAL ( KIND= kind ( 0.0D0 ) ) :: mpi_wtick
 EXTERNAL mpi_wtick
# 282 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_sysresource = (-2)
 INTEGER ( KIND= 8 ) :: XMP_NULL = 0
# 520 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_prod = 4
# 254 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_dup_datarep = 27
# 7 "lsor4c.f90"
 REAL ( KIND= 8 ) :: eps
# 260 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_info_value = 33

# 436 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_uint16_t = 61
# 83 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_any_source = (-1)
 INTEGER ( KIND= 8 ) :: t4
# 251 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_base = 24
# 69 "mpif-mpi-io.h"
 INTEGER , PARAMETER :: mpi_mode_append = 128

# 90 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_tag_ub = 0
# 373 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_combiner_hindexed_block = 18
# 359 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_combiner_hvector_integer = 4
# 429 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_long_double = 47
# 98 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_typeclass_real = 2
# 91 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_universe_size = 6
# 217 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_thread_single = 0
# 259 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_info_nokey = 32
# 411 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_real4 = 14
 INTEGER ( KIND= 8 ) :: t2
 INTEGER ( KIND= 8 ) :: t0
 EXTERNAL mpi_win_dup_fn
# 84 "mpif.h"
 REAL ( KIND= kind ( 0.0D0 ) ) :: pmpi_wtime
 EXTERNAL pmpi_wtime
# 411 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_real2 = 28
# 427 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_long_long_int = 43
# 89 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_source = 1

# 399 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_errors_return = 2
# 69 "mpif-mpi-io.h"
 INTEGER , PARAMETER :: mpi_mode_excl = 64
# 91 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_lastusedcode = 5
# 240 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_arg = 13
# 271 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_quota = 44
# 97 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_distribute_none = 2
# 412 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_double_precision = 17
# 243 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_other = 16
# 440 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_c_float_complex = 69
# 103 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_win_base = 7
# 442 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_count = 72
# 436 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_int16_t = 60
# 86 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_undefined = (-32766)
# 434 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_cxx_long_double_complex = 57

# 103 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_win_disp_unit = 9
# 277 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_spawn = 50
# 371 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_combiner_f90_integer = 16
# 189 "mpif-common.h"
 REAL ( KIND= kind ( 0.0D0 ) ) :: mpi_statuses_ignore
# 83 "mpif-config.h"
 INTEGER , PARAMETER :: mpi_address_kind = 8
# 368 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_combiner_darray = 13
# 84 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_proc_null = (-2)
# 268 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_no_space = 41
# 242 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_truncate = 15
# 360 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_combiner_hvector = 5
 EXTERNAL mpi_null_copy_fn
# 411 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_real16 = 16
# 64 "mpif-mpi-io.h"
 INTEGER , PARAMETER :: mpi_file_null = 0
# 521 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_bxor = 10
# 272 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_read_only = 45
# 65 "mpif-mpi-io.h"
 INTEGER , PARAMETER :: mpi_seek_cur = 602
# 250 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_bad_file = 23
# 203 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_op_null = 0
# 411 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_real8 = 15
 EXTERNAL mpi_null_delete_fn
# 428 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_unsigned_long_long = 44
# 5 "lsor4c.f90"
 INTEGER :: maxitr
# 346 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_unequal = 3
# 204 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_info_null = 0
# 18 "lsor4c.f90"
 INTEGER :: istat ( 1 : mpi_status_size )
# 71 "mpif-config.h"
 INTEGER , PARAMETER :: ompi_major_version = 1
# 102 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_lock_shared = 2
# 278 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_unsupported_datarep = 51

# 440 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_c_complex = 68
# 275 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_service = 48
# 521 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_lxor = 9
# 248 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_amode = 21
# 429 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_double = 46
# 230 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_type = 3
# 274 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_rma_sync = 47
# 522 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_minloc = 12
# 521 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_band = 6
 INTEGER :: XMP_DESC_b_off_1
 INTEGER :: XMP_DESC_b_off_0

# 439 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_aint = 66
# 71 "mpif-config.h"
 INTEGER , PARAMETER :: ompi_minor_version = 6
# 273 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_rma_conflict = 46
# 91 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_appnum = 4
# 203 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_errhandler_null = 0
# 346 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_congruent = 1
# 410 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_integer16 = 12
# 17 "lsor4c.f90"
 INTEGER :: nend
# 437 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_uint32_t = 63
# 247 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_access = 20
# 435 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_uint8_t = 59
# 218 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_thread_multiple = 3
# 69 "mpif-mpi-io.h"
 INTEGER , PARAMETER :: mpi_mode_sequential = 256
# 431 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_longdbl_int = 50
# 93 "mpif-common.h"
 INTEGER , PARAMETER :: impi_host_color = 13
# 346 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_similar = 2
# 257 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_file = 30
# 217 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_thread_funneled = 1
# 407 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_ub = 3
 INTEGER :: XMP_loop_step11
# 372 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_combiner_resized = 17
# 88 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_keyval_invalid = (-1)
# 99 "mpif-config.h"
 INTEGER , PARAMETER :: mpi_max_info_val = 255
# 96 "mpif-config.h"
 INTEGER , PARAMETER :: mpi_max_error_string = 255
 INTEGER :: XMP_loop_lb12
 INTEGER :: XMP_loop_step14
# 258 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_info_key = 31
# 264 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_locktype = 37
# 67 "mpif-mpi-io.h"
 INTEGER , PARAMETER :: mpi_mode_rdwr = 8
# 429 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_float = 45
# 65 "mpif-mpi-io.h"
 INTEGER , PARAMETER :: mpi_seek_set = 600
# 234 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_request = 7


# 5 "lsor4c.f90"
 INTEGER :: lm
# 358 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_combiner_vector = 3
# 236 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_group = 9
# 369 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_combiner_f90_real = 14
# 8 "lsor4c.f90"
 REAL ( KIND= 8 ) :: XMP__b ( 0 : 1 )
# 90 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_host = 1
# 355 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_combiner_named = 0

# 100 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_mode_nocheck = 1
# 235 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_root = 8
# 256 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_file_in_use = 29
# 233 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_rank = 6
# 8 "lsor4c.f90"
 REAL ( KIND= 8 ) :: XMP__x ( 0 : 1 )


# 397 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_comm_self = 1
# 98 "mpif-config.h"
 INTEGER , PARAMETER :: mpi_max_info_key = 35
# 15 "lsor4c.f90"
 INTEGER :: myrank
# 426 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_long = 41
# 202 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_group_null = 0
# 253 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_disp = 26
# 72 "mpif-config.h"
 INTEGER , PARAMETER :: ompi_release_version = 3
# 95 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_order_fortran = 1
# 522 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_maxloc = 11
# 415 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_2real = 23
 EXTERNAL mpi_comm_null_delete_fn
# 83 "mpif-config.h"
 INTEGER , PARAMETER :: mpi_offset_kind = 8

# 90 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_wtime_is_global = 3
# 16 "lsor4c.f90"
 INTEGER :: leftnode
# 398 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_group_empty = 1
# 361 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_combiner_indexed = 6
 INTEGER :: XMP_DESC_x_size_0
# 10 "lsor4c.f90"
 INTEGER :: iter
# 423 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_wchar = 33
 INTEGER :: XMP_DESC_x_size_1
# 95 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_order_c = 0
# 432 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_2int = 52
# 280 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_win = 53
 INTEGER :: XMP_loop_lb9
# 227 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_success = 0
# 266 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_no_mem = 39
 INTEGER :: XMP_loop_lb6

# 90 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_io = 2
# 232 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_comm = 5
# 103 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_win_size = 8
# 430 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_double_int = 49
# 430 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_float_int = 48
# 424 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_unsigned_char = 35
# 432 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_short_int = 53
 EXTERNAL mpi_win_null_copy_fn
# 102 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_lock_exclusive = 1
# 100 "mpif-config.h"
 INTEGER , PARAMETER :: mpi_max_port_name = 1023

# 97 "mpif-config.h"
 INTEGER , PARAMETER :: mpi_max_object_name = 63
# 83 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_any_tag = (-1)
# 6 "lsor4c.f90"
 INTEGER :: lm2
# 10 "lsor4c.f90"
 INTEGER :: kp
# 76 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_subversion = 2
# 8 "lsor4c.f90"
 REAL ( KIND= 8 ) :: XMP__coef ( 0 : 1 )
# 97 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_distribute_dflt_darg = (-1)
# 7 "lsor4c.f90"
 REAL ( KIND= 8 ) :: omega
# 407 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_packed = 2
# 413 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_complex8 = 19
# 101 "mpif-config.h"
 INTEGER , PARAMETER :: mpi_max_datarep_string = 127
# 178 "mpif-common.h"
 REAL ( KIND= kind ( 0.0D0 ) ) :: mpi_argvs_null
# 14 "lsor4c.f90"
 INTEGER :: ierr
# 94 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_bsend_overhead = 128
# 521 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_bor = 8
# 15 "lsor4c.f90"
 INTEGER :: nprocs
 EXTERNAL mpi_type_null_copy_fn
# 265 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_name = 38
# 425 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_short = 37
 EXTERNAL mpi_conversion_fn_null
# 83 "mpif-config.h"
 INTEGER , PARAMETER :: mpi_integer_kind = 4
# 20 "lsor4c.f90"
 INTEGER :: dist ( 1 : 4 ) = (/ 8 , 8 , 8 , 10 /)

# 84 "mpif.h"
 REAL ( KIND= kind ( 0.0D0 ) ) :: pmpi_wtick
 EXTERNAL pmpi_wtick
 INTEGER :: XMP_DESC_coef_size_1
 INTEGER :: XMP_DESC_coef_size_0
# 93 "mpif-common.h"
 INTEGER , PARAMETER :: impi_host_size = 12
# 229 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_count = 2
# 165 "mpif-common.h"
 INTEGER :: mpi_in_place
 INTEGER :: XMP_DESC_coef_size_2
# 438 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_uint64_t = 65
# 431 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_long_int = 51
# 425 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_unsigned_short = 38
# 522 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_replace = 13
# 435 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_int8_t = 58
# 87 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_graph = 2
# 441 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_c_double_complex = 70
# 420 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_logical8 = 32
# 420 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_logical4 = 31
# 100 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_mode_noput = 4
# 413 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_complex32 = 21
# 409 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_integer = 7
# 420 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_logical2 = 30
# 246 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_pending = 19
# 237 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_op = 10
# 70 "mpif-mpi-io.h"
 INTEGER , PARAMETER :: mpi_displacement_current = (-54278278)
# 441 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_c_long_double_complex = 71
# 420 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_logical1 = 29
# 276 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_size = 49
 INTEGER :: k3
# 202 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_comm_null = 2
 INTEGER :: k5
# 95 "mpif-config.h"
 INTEGER , PARAMETER :: mpi_max_processor_name = 255
# 10 "lsor4c.f90"
 INTEGER :: ix
# 269 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_err_no_such_file = 42
 INTEGER :: XMP_loop_step8
# 101 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_mode_nostore = 8
# 84 "mpif.h"
 REAL ( KIND= kind ( 0.0D0 ) ) :: mpi_wtime
 EXTERNAL mpi_wtime
 INTEGER :: xmpf_local_idx_
# 100 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_mode_noprecede = 2
# 366 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_combiner_struct = 11
 INTEGER :: k1
# 10 "lsor4c.f90"
 INTEGER :: ip
# 218 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_thread_serialized = 2
# 204 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_win_null = 0
# 65 "mpif-mpi-io.h"
 INTEGER , PARAMETER :: mpi_seek_end = 604
# 17 "lsor4c.f90"
 INTEGER :: nstart
 EXTERNAL mpi_comm_null_copy_fn

# 416 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_2complex = 26
# 423 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_char = 34
 INTEGER :: XMP_loop_ub10
# 520 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_min = 2
 INTEGER ( KIND= 8 ) :: XMP_DESC_b
# 346 "mpif-common.h"
 INTEGER , PARAMETER :: mpi_ident = 0
# 163 "mpif-common.h"
 INTEGER :: mpi_bottom
 INTEGER :: XMP_loop_ub13
# 187 "mpif-common.h"
 INTEGER :: mpi_status_ignore ( 1 : mpi_status_size )
 COMMON / mpi_fortran_bottom / mpi_bottom
 COMMON / mpi_fortran_in_place / mpi_in_place
 COMMON / mpi_fortran_unweighted / mpi_unweighted
 COMMON / mpi_fortran_argv_null / mpi_argv_null
 COMMON / mpi_fortran_argvs_null / mpi_argvs_null
 COMMON / mpi_fortran_errcodes_ignore / mpi_errcodes_ignore
 COMMON / mpi_fortran_status_ignore / mpi_status_ignore
 COMMON / mpi_fortran_statuses_ignore / mpi_statuses_ignore

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
 CALL xmpf_array_init_ ( XMP_DESC_coef )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_coef , 0 , XMP_DESC_coef_size_0 , XMP_DESC_coef_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_coef , 1 , XMP_DESC_coef_size_1 , XMP_DESC_coef_off_1 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_coef , 2 , XMP_DESC_coef_size_2 , XMP_DESC_coef_off_2 )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_coef , XMP__coef )
 CALL xmpf_array_alloc_ ( XMP_DESC_b , 2 , 514 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_b , 0 , 1 , lm , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_b , 1 , 1 , n , 0 , 0 )
 CALL xmpf_array_init_ ( XMP_DESC_b )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_b , 0 , XMP_DESC_b_size_0 , XMP_DESC_b_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_b , 1 , XMP_DESC_b_size_1 , XMP_DESC_b_off_1 )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_b , XMP__b )
 CALL xmpf_array_alloc_ ( XMP_DESC_x , 2 , 514 , XMP_DESC_t )
 CALL xmpf_align_info_ ( XMP_DESC_x , 0 , 1 , lm + 2 * l , (-1) , 0 )
 CALL xmpf_align_info_ ( XMP_DESC_x , 1 , 1 , n + 2 , 0 , 0 )
 CALL xmpf_array_init_shadow_ ( XMP_DESC_x , 0 , (-1) , (-1) )
 CALL xmpf_array_init_shadow_ ( XMP_DESC_x , 1 , 2 , 2 )
 CALL xmpf_array_init_ ( XMP_DESC_x )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_x , 0 , XMP_DESC_x_size_0 , XMP_DESC_x_off_0 )
 CALL xmpf_array_get_local_size_off_ ( XMP_DESC_x , 1 , XMP_DESC_x_size_1 , XMP_DESC_x_off_1 )
 CALL xmpf_array_set_local_array_ ( XMP_DESC_x , XMP__x )
# 33 "lsor4c.f90"
 IF ( myrank == 1 ) THEN
# 34 "lsor4c.f90"
  leftnode = mpi_proc_null
 ELSE
# 36 "lsor4c.f90"
  leftnode = myrank - 1
 END IF
# 39 "lsor4c.f90"
 IF ( myrank == nprocs ) THEN
# 40 "lsor4c.f90"
  rightnode = mpi_proc_null
 ELSE
# 42 "lsor4c.f90"
  rightnode = myrank + 1
 END IF
# 46 "lsor4c.f90"
 nstart = n / nprocs * ( myrank - 1 ) + 1
# 48 "lsor4c.f90"
 IF ( myrank /= nprocs ) THEN
# 49 "lsor4c.f90"
  nend = n / nprocs * myrank
 ELSE
# 51 "lsor4c.f90"
  nend = n
 END IF
# 54 "lsor4c.f90"
 lm2 = lm + 2 * l
# 58 "lsor4c.f90"
 bmax = 0.0d0
 CALL xmpf_ref_templ_alloc_ ( t0 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_loop_info_ ( t0 , 0 , 0 , 0 )
 CALL xmpf_ref_init_ ( t0 )
 XMP_loop_lb6 = 1
 XMP_loop_ub7 = n
 XMP_loop_step8 = 1
 CALL xmpf_loop_sched_ ( XMP_loop_lb6 , XMP_loop_ub7 , XMP_loop_step8 , 0 , t0 )
 DO k1 = XMP_loop_lb6 , XMP_loop_ub7 , XMP_loop_step8
  DO ip = 1 , lm , 1
# 63 "lsor4c.f90"
   bmax = max ( bmax , abs ( XMP__b ( k1 * XMP_DESC_b_size_0 + ( ip - 1 ) ) ) )
  END DO
 END DO
 CALL xmpf_ref_templ_alloc_ ( t0 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_dim_info_ ( t0 , 0 , 2 , 1 , n , 1 )
 CALL xmpf_ref_init_ ( t0 )
 CALL xmpf_reduction_ ( bmax , 1 , 514 , 308 , t0 )
 CALL xmpf_reflect_ ( XMP_DESC_x )
# 70 "lsor4c.f90"
 res = 0.0d0
 CALL xmpf_ref_templ_alloc_ ( t2 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_loop_info_ ( t2 , 0 , 0 , 0 )
 CALL xmpf_ref_init_ ( t2 )
 XMP_loop_lb9 = 1
 XMP_loop_ub10 = n
 XMP_loop_step11 = 1
 CALL xmpf_loop_sched_ ( XMP_loop_lb9 , XMP_loop_ub10 , XMP_loop_step11 , 0 , t2 )
 DO k3 = XMP_loop_lb9 , XMP_loop_ub10 , XMP_loop_step11
  CALL xmpf_l2g_ ( k , k3 , 0 , t2 )
# 75 "lsor4c.f90"
  kp = k + 1
  DO ip = 1 , lm , 1
# 78 "lsor4c.f90"
   ix = ip + l
# 79 "lsor4c.f90"
   rtmp = XMP__b ( k3 * XMP_DESC_b_size_0 + ( ip - 1 ) ) - XMP__coef ( k3 * XMP_DESC_coef_size_1 * XMP_DESC_coef_size_0 + ( ip - 1&
    ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp - 1 ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) - XMP__coef ( ( k3 *&
    XMP_DESC_coef_size_1 + 1 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) *&
    XMP_DESC_x_size_0 + ( ix - l - 1 ) ) - XMP__coef ( ( k3 * XMP_DESC_coef_size_1 + 2 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) *&
    XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 - 1 ) ) - XMP__coef ( ( k3 *&
    XMP_DESC_coef_size_1 + 3 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) *&
    XMP_DESC_x_size_0 + ( ix - 1 ) ) - XMP__coef ( ( k3 * XMP_DESC_coef_size_1 + 4 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) *&
    XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix + 1 - 1 ) ) - XMP__coef ( ( k3 *&
    XMP_DESC_coef_size_1 + 5 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) *&
    XMP_DESC_x_size_0 + ( ix + l - 1 ) ) - XMP__coef ( ( k3 * XMP_DESC_coef_size_1 + 6 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) *&
    XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp + 1 ) * XMP_DESC_x_size_0 + ( ix - 1 ) )
# 86 "lsor4c.f90"
   res = max ( res , abs ( rtmp ) )
  END DO
 END DO
 CALL xmpf_ref_templ_alloc_ ( t2 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_dim_info_ ( t2 , 0 , 2 , 1 , n , 1 )
 CALL xmpf_ref_init_ ( t2 )
 CALL xmpf_reduction_ ( res , 1 , 514 , 308 , t2 )
# 91 "lsor4c.f90"
 IF ( bmax /= 0.0 ) THEN
# 91 "lsor4c.f90"
  res = res / bmax
 END IF
# 93 "lsor4c.f90"
 IF ( res < eps ) THEN
# 95 "lsor4c.f90"
  iter = 0
# 96 "lsor4c.f90"
  IF ( myrank == 1 ) THEN
   WRITE ( unit = 6 , fmt = 6000 ) iter , res
  END IF
  GOTO 99999
 END IF
# 102 "lsor4c.f90"
 iter = 0
# 104 "lsor4c.f90"
00010 &
# 104 "lsor4c.f90"
 CONTINUE
# 106 "lsor4c.f90"
 iter = iter + 1
 DO k = nstart , nend , 2
# 113 "lsor4c.f90"
  kp = k + 1
  DO ip = 1 , lm , 2
# 117 "lsor4c.f90"
   ix = ip + l
# 118 "lsor4c.f90"
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
# 125 "lsor4c.f90"
   XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) = s1omg * XMP__x ( xmpf_local_idx_ (&
    XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) + omega * xtmp
  END DO
  DO ip = 2 , lm , 2
# 131 "lsor4c.f90"
   ix = ip + l
# 132 "lsor4c.f90"
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
# 139 "lsor4c.f90"
   XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) = s1omg * XMP__x ( xmpf_local_idx_ (&
    XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) + omega * xtmp
  END DO
 END DO
 CALL xmpf_set_reflect_ ( XMP_DESC_x , 0 , 2 , 2 , 0 )
 CALL xmpf_reflect_ ( XMP_DESC_x )
 DO k = nstart + 1 , nend , 2
# 153 "lsor4c.f90"
  kp = k + 1
  DO ip = 2 , lm , 2
# 157 "lsor4c.f90"
   ix = ip + l
# 158 "lsor4c.f90"
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
# 165 "lsor4c.f90"
   XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) = s1omg * XMP__x ( xmpf_local_idx_ (&
    XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) + omega * xtmp
  END DO
  DO ip = 1 , lm , 2
# 171 "lsor4c.f90"
   ix = ip + l
# 172 "lsor4c.f90"
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
# 179 "lsor4c.f90"
   XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) = s1omg * XMP__x ( xmpf_local_idx_ (&
    XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) + omega * xtmp
  END DO
 END DO
 CALL mpi_sendrecv ( XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , nend + 1 ) * XMP_DESC_x_size_0 ) , lm2 , mpi_real8 , rightnode ,&
  100 , XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , nstart ) * XMP_DESC_x_size_0 ) , lm2 , mpi_real8 , leftnode , 100 ,&
  mpi_comm_world , istat , ierr )
# 192 "lsor4c.f90"
 res = 0.0d0
 CALL xmpf_ref_templ_alloc_ ( t4 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_loop_info_ ( t4 , 0 , 0 , 0 )
 CALL xmpf_ref_init_ ( t4 )
 XMP_loop_lb12 = 1
 XMP_loop_ub13 = n
 XMP_loop_step14 = 1
 CALL xmpf_loop_sched_ ( XMP_loop_lb12 , XMP_loop_ub13 , XMP_loop_step14 , 0 , t4 )
 DO k5 = XMP_loop_lb12 , XMP_loop_ub13 , XMP_loop_step14
  CALL xmpf_l2g_ ( k , k5 , 0 , t4 )
# 197 "lsor4c.f90"
  kp = k + 1
  DO ip = 1 , lm , 1
# 201 "lsor4c.f90"
   ix = ip + l
# 202 "lsor4c.f90"
   rtmp = XMP__b ( k5 * XMP_DESC_b_size_0 + ( ip - 1 ) ) - XMP__coef ( k5 * XMP_DESC_coef_size_1 * XMP_DESC_coef_size_0 + ( ip - 1&
    ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp - 1 ) * XMP_DESC_x_size_0 + ( ix - 1 ) ) - XMP__coef ( ( k5 *&
    XMP_DESC_coef_size_1 + 1 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) *&
    XMP_DESC_x_size_0 + ( ix - l - 1 ) ) - XMP__coef ( ( k5 * XMP_DESC_coef_size_1 + 2 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) *&
    XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix - 1 - 1 ) ) - XMP__coef ( ( k5 *&
    XMP_DESC_coef_size_1 + 3 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) *&
    XMP_DESC_x_size_0 + ( ix - 1 ) ) - XMP__coef ( ( k5 * XMP_DESC_coef_size_1 + 4 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) *&
    XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) * XMP_DESC_x_size_0 + ( ix + 1 - 1 ) ) - XMP__coef ( ( k5 *&
    XMP_DESC_coef_size_1 + 5 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) * XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp ) *&
    XMP_DESC_x_size_0 + ( ix + l - 1 ) ) - XMP__coef ( ( k5 * XMP_DESC_coef_size_1 + 6 ) * XMP_DESC_coef_size_0 + ( ip - 1 ) ) *&
    XMP__x ( xmpf_local_idx_ ( XMP_DESC_x , 1 , kp + 1 ) * XMP_DESC_x_size_0 + ( ix - 1 ) )
# 209 "lsor4c.f90"
   res = max ( res , abs ( rtmp ) )
  END DO
 END DO
 CALL xmpf_ref_templ_alloc_ ( t4 , XMP_DESC_t , 1 )
 CALL xmpf_ref_set_dim_info_ ( t4 , 0 , 2 , 1 , n , 1 )
 CALL xmpf_ref_init_ ( t4 )
 CALL xmpf_reduction_ ( res , 1 , 514 , 308 , t4 )
# 215 "lsor4c.f90"
 res = res / bmax
# 224 "lsor4c.f90"
 IF ( res > eps .AND. iter <= maxitr ) THEN
# 224 "lsor4c.f90"
  GOTO 00010
 END IF
 CALL xmpf_barrier_ ( XMP_NULL )
# 228 "lsor4c.f90"
 IF ( myrank == 1 ) THEN
  WRITE ( unit = 6 , fmt = 6000 ) iter , res
 END IF
# 230 "lsor4c.f90"
06000 &
# 230 "lsor4c.f90"
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

