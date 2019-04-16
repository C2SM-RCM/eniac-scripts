
PROGRAM ftg_velocity_tendencies_test
  
  USE mtime
  USE mo_kind
  USE mo_exception,      ONLY: message, finish
  USE mo_mpi,            ONLY: start_mpi, stop_mpi, get_my_mpi_all_id, get_my_mpi_all_comm_size, work_mpi_barrier, p_recv, p_send, &
  &  p_bcast, p_comm_Work !ICON
  
  USE mo_velocity_advection, ONLY: velocity_tendencies
  
  USE m_ser_ftg, ONLY: ftg_set_serializer, ftg_set_savepoint, ftg_destroy_serializer, ftg_destroy_savepoint, &
  &  ftg_print_serializer_debuginfo, ftg_field_exists, ftg_get_bounds, ftg_read, ftg_allocate_and_read_pointer, &
  &  ftg_allocate_and_read_allocatable
  USE m_ser_ftg_cmp, ONLY: ftg_compare, ftg_cmp_message_prefix, ftg_cmp_print_when_equal, ftg_cmp_count_missing_field_as_failure
  
  USE mo_init_vgrid, ONLY: mo_init_vgrid__nflatlev => nflatlev
  USE mo_nonhydrostatic_config, ONLY: mo_nonhydrostatic_config__lextra_diffu => lextra_diffu
  USE mo_parallel_config, ONLY: mo_parallel_config__nproma => nproma
  USE mo_real_timer, ONLY: mo_real_timer__active_timers => active_timers, mo_real_timer__active_timers_top => active_timers_top, &
  &  mo_real_timer__delta_i => delta_i, mo_real_timer__rt => rt, mo_real_timer__srt => srt, mo_real_timer__timer_top => timer_top
  USE mo_run_config, ONLY: mo_run_config__ltimer => ltimer, mo_run_config__lvert_nest => lvert_nest, mo_run_config__timers_level &
  &  => timers_level
  USE mo_timer, ONLY: mo_timer__timer_intp => timer_intp, mo_timer__timer_solve_nh_veltend => timer_solve_nh_veltend
  USE mo_vertical_grid, ONLY: mo_vertical_grid__nrdmax => nrdmax
  USE mo_intp_data_strc, ONLY: t_int_state
  USE mo_model_domain, ONLY: t_patch
  USE mo_nonhydro_types, ONLY: t_nh_diag, t_nh_metrics, t_nh_prog
  
  IMPLICIT NONE
  
  CHARACTER(*), PARAMETER :: INPUT_DIR = &
  &  '/scratch/snx3000/pmarti/new_FTG/icon-eniac/experiments/ftg/ftg_velocity_tendencies_test/input'
  CHARACTER(*), PARAMETER :: OUTPUT_DIR = &
  &  '/scratch/snx3000/pmarti/new_FTG/icon-eniac/experiments/ftg/ftg_velocity_tendencies_test/output'
  
  INTEGER :: mpi_rank, mpi_size, total_failure_count
  CHARACTER(len=9) :: total_failure_count_char
  CHARACTER(len=9) :: arg1
  LOGICAL :: verbose
  
  CALL GET_COMMAND_ARGUMENT(1, arg1)
  verbose = TRIM(arg1) == '-v' .OR. TRIM(arg1) == '--verbose'
  
  CALL start_mpi('ftg_velocity_tendencies_test')
  
  mpi_rank = get_my_mpi_all_id()
  mpi_size = get_my_mpi_all_comm_size()
  
  WRITE (ftg_cmp_message_prefix,'(A,I0.2,A)') 'FTG velocity_tendencies (#', mpi_rank, ') * '
  ftg_cmp_print_when_equal = verbose
  ftg_cmp_count_missing_field_as_failure = .FALSE.
  
  CALL message('FTG', '*** Run test for velocity_tendencies ***')
  
  CALL ftg_test_velocity_tendencies(total_failure_count)
  
  IF (mpi_rank == 0) THEN
    IF (total_failure_count > 0) THEN
      WRITE (total_failure_count_char,'(I0)') total_failure_count
      CALL finish('FTG', 'velocity_tendencies: *** TEST FAILED, total number of failures: '//TRIM(total_failure_count_char) &
      &  //' ***', 2)
    ELSE
      CALL message('FTG', 'velocity_tendencies: *** TEST PASSED ***')
    END IF
  END IF
  
  CALL stop_mpi()
  
CONTAINS
  
  SUBROUTINE ftg_test_velocity_tendencies(total_failure_count)
    
    INTEGER, INTENT(out) :: total_failure_count
    
    TYPE(t_nh_prog) :: p_prog
    TYPE(t_patch) :: p_patch
    TYPE(t_int_state) :: p_int
    TYPE(t_nh_metrics) :: p_metrics
    TYPE(t_nh_diag) :: p_diag
    REAL(vp), DIMENSION(:,:,:), ALLOCATABLE :: z_w_concorr_me
    REAL(vp), DIMENSION(:,:,:), ALLOCATABLE :: z_kin_hor_e
    REAL(vp), DIMENSION(:,:,:), ALLOCATABLE :: z_vt_ie
    INTEGER :: ntnd
    INTEGER :: istep
    LOGICAL :: lvn_only
    REAL(wp) :: dtime
    
    INTEGER :: failure_count
    
    ! ----------- REPLAY -----------
    CALL ftg_velocity_tendencies_replay_input(p_prog, p_patch, p_int, p_metrics, p_diag, z_w_concorr_me, z_kin_hor_e, z_vt_ie, &
    &  ntnd, istep, lvn_only, dtime)
    
    CALL velocity_tendencies(p_prog, p_patch, p_int, p_metrics, p_diag, z_w_concorr_me, z_kin_hor_e, z_vt_ie, ntnd, istep, &
    &  lvn_only, dtime)
    
    ! ----------- COMPARE -----------
    ! One process after another to get nice, ordered printing
    ! Change this if to slow
    IF (mpi_rank > 0) THEN
      CALL p_recv(total_failure_count, mpi_rank - 1, 0)
    ELSE
      total_failure_count = 0
    END IF
    
    CALL ftg_velocity_tendencies_compare_output(p_prog, p_metrics, p_diag, z_w_concorr_me, z_kin_hor_e, z_vt_ie, failure_count)
    
    IF (failure_count == 0) THEN
      WRITE (*,'(A,I0.2,A)') 'FTG velocity_tendencies: RANK #', mpi_rank, ' OK'
    ELSE
      WRITE (*,'(A,I0.2,A,I0,A)') 'FTG velocity_tendencies: RANK #', mpi_rank, ' FAILED: ', failure_count, ' failures'
    END IF
    
    total_failure_count = total_failure_count + failure_count
    
    IF (mpi_rank < mpi_size - 1) THEN
      CALL p_send(total_failure_count, mpi_rank + 1, 0)
    END IF
    
    CALL p_bcast(total_failure_count, mpi_size - 1)
    
  END SUBROUTINE ftg_test_velocity_tendencies
  
  SUBROUTINE ftg_velocity_tendencies_init_serializer(stage, dir)
    
    CHARACTER(*), INTENT(IN) :: stage, dir
    
    CHARACTER(len=1024) :: basename
    
    WRITE (basename,'(A,A,A,I0.2)') 'ftg_velocity_tendencies_', TRIM(stage), '_', mpi_rank
    
    CALL ftg_set_serializer(TRIM(dir), TRIM(basename), 'r')
    CALL ftg_set_savepoint(stage)
    
  END SUBROUTINE ftg_velocity_tendencies_init_serializer
  
  SUBROUTINE ftg_velocity_tendencies_close_serializer()
    
    CALL ftg_destroy_savepoint()
    CALL ftg_destroy_serializer()
    
  END SUBROUTINE ftg_velocity_tendencies_close_serializer
  
  SUBROUTINE ftg_velocity_tendencies_replay_input(p_prog, p_patch, p_int, p_metrics, p_diag, z_w_concorr_me, z_kin_hor_e, z_vt_ie, &
  &  ntnd, istep, lvn_only, dtime)
    
    TYPE(t_nh_prog), INTENT(inout) :: p_prog
    TYPE(t_patch), INTENT(inout) :: p_patch
    TYPE(t_int_state), INTENT(inout) :: p_int
    TYPE(t_nh_metrics), INTENT(inout) :: p_metrics
    TYPE(t_nh_diag), INTENT(inout) :: p_diag
    REAL(vp), DIMENSION(:,:,:), ALLOCATABLE, INTENT(inout) :: z_w_concorr_me
    REAL(vp), DIMENSION(:,:,:), ALLOCATABLE, INTENT(inout) :: z_kin_hor_e
    REAL(vp), DIMENSION(:,:,:), ALLOCATABLE, INTENT(inout) :: z_vt_ie
    INTEGER, INTENT(inout) :: ntnd
    INTEGER, INTENT(inout) :: istep
    LOGICAL, INTENT(inout) :: lvn_only
    REAL(wp), INTENT(inout) :: dtime
    
    INTEGER, DIMENSION(8) :: ftg_bounds
    INTEGER :: ftg_d1, ftg_d2, ftg_d3, ftg_d4, ftg_d5, ftg_d6
    CHARACTER(len=256) :: ftg_name
    INTEGER ftg_mtime_calendar
    
    WRITE (0,'(A,I0.2,A)') 'FTG READ INPUT STARTED velocity_tendencies (Rank #', mpi_rank, ')'
    CALL ftg_velocity_tendencies_init_serializer('input', INPUT_DIR)
    
    ! MTIME CALENDAR TYPE --> Remove these lines if mtime is not used
    CALL ftg_read("ftg_mtime_calendar", ftg_mtime_calendar)
    CALL setCalendar(ftg_mtime_calendar)
    
    ! ARGUMENTS
    CALL ftg_allocate_and_read_pointer("p_prog%vn", p_prog%vn)
    CALL ftg_allocate_and_read_pointer("p_prog%w", p_prog%w)
    CALL ftg_read("p_patch%id", p_patch%id)
    CALL ftg_read("p_patch%n_childdom", p_patch%n_childdom)
    CALL ftg_read("p_patch%nblks_c", p_patch%nblks_c)
    CALL ftg_read("p_patch%nblks_e", p_patch%nblks_e)
    CALL ftg_read("p_patch%nblks_v", p_patch%nblks_v)
    CALL ftg_read("p_patch%nlev", p_patch%nlev)
    CALL ftg_read("p_patch%nlevp1", p_patch%nlevp1)
    CALL ftg_read("p_patch%nshift", p_patch%nshift)
    CALL ftg_allocate_and_read_pointer("p_patch%cells%area", p_patch%cells%area)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%edge_blk", p_patch%cells%edge_blk)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%edge_idx", p_patch%cells%edge_idx)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%end_block", p_patch%cells%end_block)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%end_index", p_patch%cells%end_index)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%neighbor_blk", p_patch%cells%neighbor_blk)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%neighbor_idx", p_patch%cells%neighbor_idx)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%start_block", p_patch%cells%start_block)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%start_index", p_patch%cells%start_index)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%area_edge", p_patch%edges%area_edge)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%cell_blk", p_patch%edges%cell_blk)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%cell_idx", p_patch%edges%cell_idx)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%end_block", p_patch%edges%end_block)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%end_index", p_patch%edges%end_index)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%f_e", p_patch%edges%f_e)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%inv_dual_edge_length", p_patch%edges%inv_dual_edge_length)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%inv_primal_edge_length", p_patch%edges%inv_primal_edge_length)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%quad_blk", p_patch%edges%quad_blk)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%quad_idx", p_patch%edges%quad_idx)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%start_block", p_patch%edges%start_block)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%start_index", p_patch%edges%start_index)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%tangent_orientation", p_patch%edges%tangent_orientation)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%vertex_blk", p_patch%edges%vertex_blk)
    CALL ftg_allocate_and_read_allocatable("p_patch%edges%vertex_idx", p_patch%edges%vertex_idx)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%cell_blk", p_patch%verts%cell_blk)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%cell_idx", p_patch%verts%cell_idx)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%edge_blk", p_patch%verts%edge_blk)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%edge_idx", p_patch%verts%edge_idx)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%end_blk", p_patch%verts%end_blk)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%end_block", p_patch%verts%end_block)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%end_index", p_patch%verts%end_index)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%start_blk", p_patch%verts%start_blk)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%start_block", p_patch%verts%start_block)
    CALL ftg_allocate_and_read_allocatable("p_patch%verts%start_index", p_patch%verts%start_index)
    CALL ftg_allocate_and_read_allocatable("p_patch%cells%decomp_info%owner_mask", p_patch%cells%decomp_info%owner_mask)
    CALL ftg_allocate_and_read_allocatable("p_int%c_lin_e", p_int%c_lin_e)
    CALL ftg_allocate_and_read_allocatable("p_int%cells_aw_verts", p_int%cells_aw_verts)
    CALL ftg_allocate_and_read_allocatable("p_int%e_bln_c_s", p_int%e_bln_c_s)
    CALL ftg_allocate_and_read_allocatable("p_int%geofac_grdiv", p_int%geofac_grdiv)
    CALL ftg_allocate_and_read_allocatable("p_int%geofac_n2s", p_int%geofac_n2s)
    CALL ftg_allocate_and_read_allocatable("p_int%geofac_rot", p_int%geofac_rot)
    CALL ftg_allocate_and_read_allocatable("p_int%rbf_vec_coeff_e", p_int%rbf_vec_coeff_e)
    CALL ftg_allocate_and_read_pointer("p_metrics%coeff1_dwdz", p_metrics%coeff1_dwdz)
    CALL ftg_allocate_and_read_pointer("p_metrics%coeff2_dwdz", p_metrics%coeff2_dwdz)
    CALL ftg_allocate_and_read_pointer("p_metrics%coeff_gradekin", p_metrics%coeff_gradekin)
    CALL ftg_allocate_and_read_pointer("p_metrics%ddqz_z_full_e", p_metrics%ddqz_z_full_e)
    CALL ftg_allocate_and_read_pointer("p_metrics%ddqz_z_half", p_metrics%ddqz_z_half)
    CALL ftg_allocate_and_read_pointer("p_metrics%ddxn_z_full", p_metrics%ddxn_z_full)
    CALL ftg_allocate_and_read_pointer("p_metrics%ddxt_z_full", p_metrics%ddxt_z_full)
    CALL ftg_allocate_and_read_pointer("p_metrics%wgtfac_c", p_metrics%wgtfac_c)
    CALL ftg_allocate_and_read_pointer("p_metrics%wgtfac_e", p_metrics%wgtfac_e)
    CALL ftg_allocate_and_read_pointer("p_metrics%wgtfacq_e", p_metrics%wgtfacq_e)
    CALL ftg_allocate_and_read_pointer("p_diag%ddt_vn_adv", p_diag%ddt_vn_adv)
    CALL ftg_allocate_and_read_pointer("p_diag%ddt_w_adv", p_diag%ddt_w_adv)
    CALL ftg_allocate_and_read_pointer("p_diag%dvn_ie_ubc", p_diag%dvn_ie_ubc)
    CALL ftg_read("p_diag%max_vcfl_dyn", p_diag%max_vcfl_dyn)
    CALL ftg_allocate_and_read_pointer("p_diag%vn_ie", p_diag%vn_ie)
    CALL ftg_allocate_and_read_pointer("p_diag%vt", p_diag%vt)
    CALL ftg_allocate_and_read_pointer("p_diag%w_concorr_c", p_diag%w_concorr_c)
    CALL ftg_allocate_and_read_allocatable("z_w_concorr_me", z_w_concorr_me)
    CALL ftg_allocate_and_read_allocatable("z_kin_hor_e", z_kin_hor_e)
    CALL ftg_allocate_and_read_allocatable("z_vt_ie", z_vt_ie)
    CALL ftg_read("ntnd", ntnd)
    CALL ftg_read("istep", istep)
    CALL ftg_read("lvn_only", lvn_only)
    CALL ftg_read("dtime", dtime)
    
    ! GLOBALS
    CALL ftg_read("mo_real_timer__active_timers", mo_real_timer__active_timers)
    CALL ftg_read("mo_real_timer__active_timers_top", mo_real_timer__active_timers_top)
    CALL ftg_read("mo_real_timer__delta_i", mo_real_timer__delta_i)
    CALL ftg_read("mo_nonhydrostatic_config__lextra_diffu", mo_nonhydrostatic_config__lextra_diffu)
    CALL ftg_read("mo_run_config__ltimer", mo_run_config__ltimer)
    CALL ftg_read("mo_run_config__lvert_nest", mo_run_config__lvert_nest)
    CALL ftg_read("mo_init_vgrid__nflatlev", mo_init_vgrid__nflatlev)
    CALL ftg_read("mo_parallel_config__nproma", mo_parallel_config__nproma)
    CALL ftg_read("mo_vertical_grid__nrdmax", mo_vertical_grid__nrdmax)
    CALL ftg_read("mo_timer__timer_intp", mo_timer__timer_intp)
    CALL ftg_read("mo_timer__timer_solve_nh_veltend", mo_timer__timer_solve_nh_veltend)
    CALL ftg_read("mo_real_timer__timer_top", mo_real_timer__timer_top)
    CALL ftg_read("mo_run_config__timers_level", mo_run_config__timers_level)
    CALL ftg_read("mo_real_timer__rt%active_under", mo_real_timer__rt%active_under)
    CALL ftg_read("mo_real_timer__rt%call_n", mo_real_timer__rt%call_n)
    CALL ftg_read("mo_real_timer__rt%last", mo_real_timer__rt%last)
    CALL ftg_read("mo_real_timer__rt%mark1", mo_real_timer__rt%mark1)
    CALL ftg_read("mo_real_timer__rt%max", mo_real_timer__rt%max)
    CALL ftg_read("mo_real_timer__rt%min", mo_real_timer__rt%min)
    CALL ftg_read("mo_real_timer__rt%stat", mo_real_timer__rt%stat)
    CALL ftg_read("mo_real_timer__rt%tot", mo_real_timer__rt%tot)
    ! *** WARNING: Type not supported by serialbox ***
    !     mo_real_timer__srt%text
    !     CHARACTER(len=80), DIMENSION(0)
    
    CALL ftg_velocity_tendencies_close_serializer()
    WRITE (0,'(A,I0.2,A)') 'FTG READ INPUT FINISHED velocity_tendencies (Rank #', mpi_rank, ')'
    
  END SUBROUTINE ftg_velocity_tendencies_replay_input
  
  SUBROUTINE ftg_velocity_tendencies_compare_output(p_prog, p_metrics, p_diag, z_w_concorr_me, z_kin_hor_e, z_vt_ie, failure_count)
    
    TYPE(t_nh_prog), INTENT(in) :: p_prog
    TYPE(t_nh_metrics), INTENT(in) :: p_metrics
    TYPE(t_nh_diag), INTENT(in) :: p_diag
    REAL(vp), DIMENSION(:,:,:), INTENT(in) :: z_w_concorr_me
    REAL(vp), DIMENSION(:,:,:), INTENT(in) :: z_kin_hor_e
    REAL(vp), DIMENSION(:,:,:), INTENT(in) :: z_vt_ie
    
    INTEGER, INTENT(out) :: failure_count
    LOGICAL :: result
    INTEGER :: ftg_d1, ftg_d2, ftg_d3, ftg_d4, ftg_d5, ftg_d6
    CHARACTER(len=256) :: ftg_name
    
    WRITE (0,'(A,I0.2,A)') 'FTG COMPARE OUTPUT DATA velocity_tendencies (Rank #', mpi_rank, ')'
    CALL ftg_velocity_tendencies_init_serializer('output', OUTPUT_DIR)
    
    failure_count = 0
    
    ! ARGUMENTS
    CALL ftg_compare("p_prog%vn", p_prog%vn, result, failure_count, LBOUND(p_prog%vn), UBOUND(p_prog%vn))
    CALL ftg_compare("p_prog%w", p_prog%w, result, failure_count, LBOUND(p_prog%w), UBOUND(p_prog%w))
    CALL ftg_compare("p_metrics%coeff1_dwdz", p_metrics%coeff1_dwdz, result, failure_count, LBOUND(p_metrics%coeff1_dwdz), UBOUND( &
    &  p_metrics%coeff1_dwdz))
    CALL ftg_compare("p_metrics%coeff2_dwdz", p_metrics%coeff2_dwdz, result, failure_count, LBOUND(p_metrics%coeff2_dwdz), UBOUND( &
    &  p_metrics%coeff2_dwdz))
    CALL ftg_compare("p_metrics%coeff_gradekin", p_metrics%coeff_gradekin, result, failure_count, LBOUND(p_metrics%coeff_gradekin) &
    &  , UBOUND(p_metrics%coeff_gradekin))
    CALL ftg_compare("p_metrics%ddqz_z_full_e", p_metrics%ddqz_z_full_e, result, failure_count, LBOUND(p_metrics%ddqz_z_full_e), &
    &  UBOUND(p_metrics%ddqz_z_full_e))
    CALL ftg_compare("p_metrics%ddqz_z_half", p_metrics%ddqz_z_half, result, failure_count, LBOUND(p_metrics%ddqz_z_half), UBOUND( &
    &  p_metrics%ddqz_z_half))
    CALL ftg_compare("p_metrics%ddxn_z_full", p_metrics%ddxn_z_full, result, failure_count, LBOUND(p_metrics%ddxn_z_full), UBOUND( &
    &  p_metrics%ddxn_z_full))
    CALL ftg_compare("p_metrics%ddxt_z_full", p_metrics%ddxt_z_full, result, failure_count, LBOUND(p_metrics%ddxt_z_full), UBOUND( &
    &  p_metrics%ddxt_z_full))
    CALL ftg_compare("p_metrics%wgtfac_c", p_metrics%wgtfac_c, result, failure_count, LBOUND(p_metrics%wgtfac_c), UBOUND( &
    &  p_metrics%wgtfac_c))
    CALL ftg_compare("p_metrics%wgtfac_e", p_metrics%wgtfac_e, result, failure_count, LBOUND(p_metrics%wgtfac_e), UBOUND( &
    &  p_metrics%wgtfac_e))
    CALL ftg_compare("p_metrics%wgtfacq_e", p_metrics%wgtfacq_e, result, failure_count, LBOUND(p_metrics%wgtfacq_e), UBOUND( &
    &  p_metrics%wgtfacq_e))
    CALL ftg_compare("p_diag%ddt_vn_adv", p_diag%ddt_vn_adv, result, failure_count, LBOUND(p_diag%ddt_vn_adv), UBOUND(p_diag% &
    &  ddt_vn_adv))
    CALL ftg_compare("p_diag%ddt_w_adv", p_diag%ddt_w_adv, result, failure_count, LBOUND(p_diag%ddt_w_adv), UBOUND(p_diag% &
    &  ddt_w_adv))
    CALL ftg_compare("p_diag%dvn_ie_ubc", p_diag%dvn_ie_ubc, result, failure_count, LBOUND(p_diag%dvn_ie_ubc), UBOUND(p_diag% &
    &  dvn_ie_ubc))
    CALL ftg_compare("p_diag%max_vcfl_dyn", p_diag%max_vcfl_dyn, result, failure_count)
    CALL ftg_compare("p_diag%vn_ie", p_diag%vn_ie, result, failure_count, LBOUND(p_diag%vn_ie), UBOUND(p_diag%vn_ie))
    CALL ftg_compare("p_diag%vt", p_diag%vt, result, failure_count, LBOUND(p_diag%vt), UBOUND(p_diag%vt))
    CALL ftg_compare("p_diag%w_concorr_c", p_diag%w_concorr_c, result, failure_count, LBOUND(p_diag%w_concorr_c), UBOUND(p_diag% &
    &  w_concorr_c))
    CALL ftg_compare("z_w_concorr_me", z_w_concorr_me, result, failure_count, LBOUND(z_w_concorr_me), UBOUND(z_w_concorr_me))
    CALL ftg_compare("z_kin_hor_e", z_kin_hor_e, result, failure_count, LBOUND(z_kin_hor_e), UBOUND(z_kin_hor_e))
    CALL ftg_compare("z_vt_ie", z_vt_ie, result, failure_count, LBOUND(z_vt_ie), UBOUND(z_vt_ie))
    
    
    ! GLOBALS
    CALL ftg_compare("mo_nonhydrostatic_config__lextra_diffu", mo_nonhydrostatic_config__lextra_diffu, result, failure_count)
    CALL ftg_compare("mo_run_config__ltimer", mo_run_config__ltimer, result, failure_count)
    CALL ftg_compare("mo_run_config__lvert_nest", mo_run_config__lvert_nest, result, failure_count)
    CALL ftg_compare("mo_init_vgrid__nflatlev", mo_init_vgrid__nflatlev, result, failure_count, LBOUND(mo_init_vgrid__nflatlev), &
    &  UBOUND(mo_init_vgrid__nflatlev))
    CALL ftg_compare("mo_parallel_config__nproma", mo_parallel_config__nproma, result, failure_count)
    CALL ftg_compare("mo_vertical_grid__nrdmax", mo_vertical_grid__nrdmax, result, failure_count, LBOUND(mo_vertical_grid__nrdmax) &
    &  , UBOUND(mo_vertical_grid__nrdmax))
    CALL ftg_compare("mo_timer__timer_intp", mo_timer__timer_intp, result, failure_count)
    CALL ftg_compare("mo_timer__timer_solve_nh_veltend", mo_timer__timer_solve_nh_veltend, result, failure_count)
    CALL ftg_compare("mo_run_config__timers_level", mo_run_config__timers_level, result, failure_count)
    
  END SUBROUTINE ftg_velocity_tendencies_compare_output
  
END PROGRAM ftg_velocity_tendencies_test

